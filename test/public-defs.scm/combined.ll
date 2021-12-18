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

@global$sym$ae4407350125 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae4407650127 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae4407950129 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae4408250131 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae4408550133 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae4408850135 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae4409150137 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae4409450139 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae4409750141 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae4410050143 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae4410350145 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae4410650147 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae4410950149 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae4411250151 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae4411550153 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae4411850155 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae4412150157 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae4412450159 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae4412750163 = private unnamed_addr constant [8 x i8] c"ignored\00", align 8
@global$sym$ae4396050182 = private unnamed_addr constant [8 x i8] c"promise\00", align 8

define ccc i32 @main() {
%mainenv49697 = call %struct.ScmObj* @const_init_null()
%mainargs49698 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv49697, %struct.ScmObj* %mainargs49698)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv49695,%struct.ScmObj* %mainargs49696) {
%stackaddr$makeclosure49699 = alloca %struct.ScmObj*, align 8
%fptrToInt49700 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40572 to i64
%ae40572 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49700)
store volatile %struct.ScmObj* %ae40572, %struct.ScmObj** %stackaddr$makeclosure49699, align 8
%ae40573 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49701 = alloca %struct.ScmObj*, align 8
%fptrToInt49702 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40574 to i64
%ae40574 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49702)
store volatile %struct.ScmObj* %ae40574, %struct.ScmObj** %stackaddr$makeclosure49701, align 8
%args49694$ae40572$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49703 = alloca %struct.ScmObj*, align 8
%args49694$ae40572$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40574, %struct.ScmObj* %args49694$ae40572$0)
store volatile %struct.ScmObj* %args49694$ae40572$1, %struct.ScmObj** %stackaddr$prim49703, align 8
%stackaddr$prim49704 = alloca %struct.ScmObj*, align 8
%args49694$ae40572$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40573, %struct.ScmObj* %args49694$ae40572$1)
store volatile %struct.ScmObj* %args49694$ae40572$2, %struct.ScmObj** %stackaddr$prim49704, align 8
%clofunc49705 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40572)
musttail call tailcc void %clofunc49705(%struct.ScmObj* %ae40572, %struct.ScmObj* %args49694$ae40572$2)
ret void
}

define tailcc void @proc_clo$ae40572(%struct.ScmObj* %env$ae40572,%struct.ScmObj* %current_45args49158) {
%stackaddr$prim49706 = alloca %struct.ScmObj*, align 8
%_95k40409 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49158)
store volatile %struct.ScmObj* %_95k40409, %struct.ScmObj** %stackaddr$prim49706, align 8
%stackaddr$prim49707 = alloca %struct.ScmObj*, align 8
%current_45args49159 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49158)
store volatile %struct.ScmObj* %current_45args49159, %struct.ScmObj** %stackaddr$prim49707, align 8
%stackaddr$prim49708 = alloca %struct.ScmObj*, align 8
%anf_45bind40276 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49159)
store volatile %struct.ScmObj* %anf_45bind40276, %struct.ScmObj** %stackaddr$prim49708, align 8
%stackaddr$makeclosure49709 = alloca %struct.ScmObj*, align 8
%fptrToInt49710 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40587 to i64
%ae40587 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49710)
store volatile %struct.ScmObj* %ae40587, %struct.ScmObj** %stackaddr$makeclosure49709, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40587, %struct.ScmObj* %anf_45bind40276, i64 0)
%ae40588 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49711 = alloca %struct.ScmObj*, align 8
%fptrToInt49712 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40589 to i64
%ae40589 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49712)
store volatile %struct.ScmObj* %ae40589, %struct.ScmObj** %stackaddr$makeclosure49711, align 8
%args49689$ae40587$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49713 = alloca %struct.ScmObj*, align 8
%args49689$ae40587$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40589, %struct.ScmObj* %args49689$ae40587$0)
store volatile %struct.ScmObj* %args49689$ae40587$1, %struct.ScmObj** %stackaddr$prim49713, align 8
%stackaddr$prim49714 = alloca %struct.ScmObj*, align 8
%args49689$ae40587$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40588, %struct.ScmObj* %args49689$ae40587$1)
store volatile %struct.ScmObj* %args49689$ae40587$2, %struct.ScmObj** %stackaddr$prim49714, align 8
%clofunc49715 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40587)
musttail call tailcc void %clofunc49715(%struct.ScmObj* %ae40587, %struct.ScmObj* %args49689$ae40587$2)
ret void
}

define tailcc void @proc_clo$ae40587(%struct.ScmObj* %env$ae40587,%struct.ScmObj* %current_45args49161) {
%stackaddr$env-ref49716 = alloca %struct.ScmObj*, align 8
%anf_45bind40276 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40587, i64 0)
store %struct.ScmObj* %anf_45bind40276, %struct.ScmObj** %stackaddr$env-ref49716
%stackaddr$prim49717 = alloca %struct.ScmObj*, align 8
%_95k40410 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49161)
store volatile %struct.ScmObj* %_95k40410, %struct.ScmObj** %stackaddr$prim49717, align 8
%stackaddr$prim49718 = alloca %struct.ScmObj*, align 8
%current_45args49162 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49161)
store volatile %struct.ScmObj* %current_45args49162, %struct.ScmObj** %stackaddr$prim49718, align 8
%stackaddr$prim49719 = alloca %struct.ScmObj*, align 8
%anf_45bind40280 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49162)
store volatile %struct.ScmObj* %anf_45bind40280, %struct.ScmObj** %stackaddr$prim49719, align 8
%stackaddr$makeclosure49720 = alloca %struct.ScmObj*, align 8
%fptrToInt49721 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40702 to i64
%ae40702 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49721)
store volatile %struct.ScmObj* %ae40702, %struct.ScmObj** %stackaddr$makeclosure49720, align 8
%args49668$anf_45bind40276$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49722 = alloca %struct.ScmObj*, align 8
%args49668$anf_45bind40276$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40280, %struct.ScmObj* %args49668$anf_45bind40276$0)
store volatile %struct.ScmObj* %args49668$anf_45bind40276$1, %struct.ScmObj** %stackaddr$prim49722, align 8
%stackaddr$prim49723 = alloca %struct.ScmObj*, align 8
%args49668$anf_45bind40276$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40702, %struct.ScmObj* %args49668$anf_45bind40276$1)
store volatile %struct.ScmObj* %args49668$anf_45bind40276$2, %struct.ScmObj** %stackaddr$prim49723, align 8
%clofunc49724 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40276)
musttail call tailcc void %clofunc49724(%struct.ScmObj* %anf_45bind40276, %struct.ScmObj* %args49668$anf_45bind40276$2)
ret void
}

define tailcc void @proc_clo$ae40702(%struct.ScmObj* %env$ae40702,%struct.ScmObj* %current_45args49164) {
%stackaddr$prim49725 = alloca %struct.ScmObj*, align 8
%_95k40411 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49164)
store volatile %struct.ScmObj* %_95k40411, %struct.ScmObj** %stackaddr$prim49725, align 8
%stackaddr$prim49726 = alloca %struct.ScmObj*, align 8
%current_45args49165 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49164)
store volatile %struct.ScmObj* %current_45args49165, %struct.ScmObj** %stackaddr$prim49726, align 8
%stackaddr$prim49727 = alloca %struct.ScmObj*, align 8
%Ycmb40120 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49165)
store volatile %struct.ScmObj* %Ycmb40120, %struct.ScmObj** %stackaddr$prim49727, align 8
%stackaddr$makeclosure49728 = alloca %struct.ScmObj*, align 8
%fptrToInt49729 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40704 to i64
%ae40704 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49729)
store volatile %struct.ScmObj* %ae40704, %struct.ScmObj** %stackaddr$makeclosure49728, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40704, %struct.ScmObj* %Ycmb40120, i64 0)
%ae40705 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49730 = alloca %struct.ScmObj*, align 8
%fptrToInt49731 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40706 to i64
%ae40706 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49731)
store volatile %struct.ScmObj* %ae40706, %struct.ScmObj** %stackaddr$makeclosure49730, align 8
%args49667$ae40704$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49732 = alloca %struct.ScmObj*, align 8
%args49667$ae40704$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40706, %struct.ScmObj* %args49667$ae40704$0)
store volatile %struct.ScmObj* %args49667$ae40704$1, %struct.ScmObj** %stackaddr$prim49732, align 8
%stackaddr$prim49733 = alloca %struct.ScmObj*, align 8
%args49667$ae40704$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40705, %struct.ScmObj* %args49667$ae40704$1)
store volatile %struct.ScmObj* %args49667$ae40704$2, %struct.ScmObj** %stackaddr$prim49733, align 8
%clofunc49734 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40704)
musttail call tailcc void %clofunc49734(%struct.ScmObj* %ae40704, %struct.ScmObj* %args49667$ae40704$2)
ret void
}

define tailcc void @proc_clo$ae40704(%struct.ScmObj* %env$ae40704,%struct.ScmObj* %current_45args49167) {
%stackaddr$env-ref49735 = alloca %struct.ScmObj*, align 8
%Ycmb40120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40704, i64 0)
store %struct.ScmObj* %Ycmb40120, %struct.ScmObj** %stackaddr$env-ref49735
%stackaddr$prim49736 = alloca %struct.ScmObj*, align 8
%_95k40412 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49167)
store volatile %struct.ScmObj* %_95k40412, %struct.ScmObj** %stackaddr$prim49736, align 8
%stackaddr$prim49737 = alloca %struct.ScmObj*, align 8
%current_45args49168 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49167)
store volatile %struct.ScmObj* %current_45args49168, %struct.ScmObj** %stackaddr$prim49737, align 8
%stackaddr$prim49738 = alloca %struct.ScmObj*, align 8
%anf_45bind40285 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49168)
store volatile %struct.ScmObj* %anf_45bind40285, %struct.ScmObj** %stackaddr$prim49738, align 8
%stackaddr$makeclosure49739 = alloca %struct.ScmObj*, align 8
%fptrToInt49740 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40782 to i64
%ae40782 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49740)
store volatile %struct.ScmObj* %ae40782, %struct.ScmObj** %stackaddr$makeclosure49739, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40782, %struct.ScmObj* %Ycmb40120, i64 0)
%args49651$Ycmb40120$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49741 = alloca %struct.ScmObj*, align 8
%args49651$Ycmb40120$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40285, %struct.ScmObj* %args49651$Ycmb40120$0)
store volatile %struct.ScmObj* %args49651$Ycmb40120$1, %struct.ScmObj** %stackaddr$prim49741, align 8
%stackaddr$prim49742 = alloca %struct.ScmObj*, align 8
%args49651$Ycmb40120$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40782, %struct.ScmObj* %args49651$Ycmb40120$1)
store volatile %struct.ScmObj* %args49651$Ycmb40120$2, %struct.ScmObj** %stackaddr$prim49742, align 8
%clofunc49743 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40120)
musttail call tailcc void %clofunc49743(%struct.ScmObj* %Ycmb40120, %struct.ScmObj* %args49651$Ycmb40120$2)
ret void
}

define tailcc void @proc_clo$ae40782(%struct.ScmObj* %env$ae40782,%struct.ScmObj* %current_45args49170) {
%stackaddr$env-ref49744 = alloca %struct.ScmObj*, align 8
%Ycmb40120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40782, i64 0)
store %struct.ScmObj* %Ycmb40120, %struct.ScmObj** %stackaddr$env-ref49744
%stackaddr$prim49745 = alloca %struct.ScmObj*, align 8
%_95k40413 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49170)
store volatile %struct.ScmObj* %_95k40413, %struct.ScmObj** %stackaddr$prim49745, align 8
%stackaddr$prim49746 = alloca %struct.ScmObj*, align 8
%current_45args49171 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49170)
store volatile %struct.ScmObj* %current_45args49171, %struct.ScmObj** %stackaddr$prim49746, align 8
%stackaddr$prim49747 = alloca %struct.ScmObj*, align 8
%_37foldr140141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49171)
store volatile %struct.ScmObj* %_37foldr140141, %struct.ScmObj** %stackaddr$prim49747, align 8
%stackaddr$makeclosure49748 = alloca %struct.ScmObj*, align 8
%fptrToInt49749 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40784 to i64
%ae40784 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49749)
store volatile %struct.ScmObj* %ae40784, %struct.ScmObj** %stackaddr$makeclosure49748, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40784, %struct.ScmObj* %_37foldr140141, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40784, %struct.ScmObj* %Ycmb40120, i64 1)
%ae40785 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49750 = alloca %struct.ScmObj*, align 8
%fptrToInt49751 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40786 to i64
%ae40786 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49751)
store volatile %struct.ScmObj* %ae40786, %struct.ScmObj** %stackaddr$makeclosure49750, align 8
%args49650$ae40784$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49752 = alloca %struct.ScmObj*, align 8
%args49650$ae40784$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40786, %struct.ScmObj* %args49650$ae40784$0)
store volatile %struct.ScmObj* %args49650$ae40784$1, %struct.ScmObj** %stackaddr$prim49752, align 8
%stackaddr$prim49753 = alloca %struct.ScmObj*, align 8
%args49650$ae40784$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40785, %struct.ScmObj* %args49650$ae40784$1)
store volatile %struct.ScmObj* %args49650$ae40784$2, %struct.ScmObj** %stackaddr$prim49753, align 8
%clofunc49754 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40784)
musttail call tailcc void %clofunc49754(%struct.ScmObj* %ae40784, %struct.ScmObj* %args49650$ae40784$2)
ret void
}

define tailcc void @proc_clo$ae40784(%struct.ScmObj* %env$ae40784,%struct.ScmObj* %current_45args49173) {
%stackaddr$env-ref49755 = alloca %struct.ScmObj*, align 8
%_37foldr140141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40784, i64 0)
store %struct.ScmObj* %_37foldr140141, %struct.ScmObj** %stackaddr$env-ref49755
%stackaddr$env-ref49756 = alloca %struct.ScmObj*, align 8
%Ycmb40120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40784, i64 1)
store %struct.ScmObj* %Ycmb40120, %struct.ScmObj** %stackaddr$env-ref49756
%stackaddr$prim49757 = alloca %struct.ScmObj*, align 8
%_95k40414 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49173)
store volatile %struct.ScmObj* %_95k40414, %struct.ScmObj** %stackaddr$prim49757, align 8
%stackaddr$prim49758 = alloca %struct.ScmObj*, align 8
%current_45args49174 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49173)
store volatile %struct.ScmObj* %current_45args49174, %struct.ScmObj** %stackaddr$prim49758, align 8
%stackaddr$prim49759 = alloca %struct.ScmObj*, align 8
%anf_45bind40291 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49174)
store volatile %struct.ScmObj* %anf_45bind40291, %struct.ScmObj** %stackaddr$prim49759, align 8
%stackaddr$makeclosure49760 = alloca %struct.ScmObj*, align 8
%fptrToInt49761 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40879 to i64
%ae40879 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49761)
store volatile %struct.ScmObj* %ae40879, %struct.ScmObj** %stackaddr$makeclosure49760, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40879, %struct.ScmObj* %_37foldr140141, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40879, %struct.ScmObj* %Ycmb40120, i64 1)
%args49631$Ycmb40120$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49762 = alloca %struct.ScmObj*, align 8
%args49631$Ycmb40120$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40291, %struct.ScmObj* %args49631$Ycmb40120$0)
store volatile %struct.ScmObj* %args49631$Ycmb40120$1, %struct.ScmObj** %stackaddr$prim49762, align 8
%stackaddr$prim49763 = alloca %struct.ScmObj*, align 8
%args49631$Ycmb40120$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40879, %struct.ScmObj* %args49631$Ycmb40120$1)
store volatile %struct.ScmObj* %args49631$Ycmb40120$2, %struct.ScmObj** %stackaddr$prim49763, align 8
%clofunc49764 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40120)
musttail call tailcc void %clofunc49764(%struct.ScmObj* %Ycmb40120, %struct.ScmObj* %args49631$Ycmb40120$2)
ret void
}

define tailcc void @proc_clo$ae40879(%struct.ScmObj* %env$ae40879,%struct.ScmObj* %current_45args49176) {
%stackaddr$env-ref49765 = alloca %struct.ScmObj*, align 8
%_37foldr140141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40879, i64 0)
store %struct.ScmObj* %_37foldr140141, %struct.ScmObj** %stackaddr$env-ref49765
%stackaddr$env-ref49766 = alloca %struct.ScmObj*, align 8
%Ycmb40120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40879, i64 1)
store %struct.ScmObj* %Ycmb40120, %struct.ScmObj** %stackaddr$env-ref49766
%stackaddr$prim49767 = alloca %struct.ScmObj*, align 8
%_95k40415 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49176)
store volatile %struct.ScmObj* %_95k40415, %struct.ScmObj** %stackaddr$prim49767, align 8
%stackaddr$prim49768 = alloca %struct.ScmObj*, align 8
%current_45args49177 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49176)
store volatile %struct.ScmObj* %current_45args49177, %struct.ScmObj** %stackaddr$prim49768, align 8
%stackaddr$prim49769 = alloca %struct.ScmObj*, align 8
%_37map140137 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49177)
store volatile %struct.ScmObj* %_37map140137, %struct.ScmObj** %stackaddr$prim49769, align 8
%stackaddr$makeclosure49770 = alloca %struct.ScmObj*, align 8
%fptrToInt49771 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40881 to i64
%ae40881 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49771)
store volatile %struct.ScmObj* %ae40881, %struct.ScmObj** %stackaddr$makeclosure49770, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40881, %struct.ScmObj* %_37foldr140141, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40881, %struct.ScmObj* %_37map140137, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40881, %struct.ScmObj* %Ycmb40120, i64 2)
%ae40882 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49772 = alloca %struct.ScmObj*, align 8
%fptrToInt49773 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40883 to i64
%ae40883 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49773)
store volatile %struct.ScmObj* %ae40883, %struct.ScmObj** %stackaddr$makeclosure49772, align 8
%args49630$ae40881$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49774 = alloca %struct.ScmObj*, align 8
%args49630$ae40881$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40883, %struct.ScmObj* %args49630$ae40881$0)
store volatile %struct.ScmObj* %args49630$ae40881$1, %struct.ScmObj** %stackaddr$prim49774, align 8
%stackaddr$prim49775 = alloca %struct.ScmObj*, align 8
%args49630$ae40881$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40882, %struct.ScmObj* %args49630$ae40881$1)
store volatile %struct.ScmObj* %args49630$ae40881$2, %struct.ScmObj** %stackaddr$prim49775, align 8
%clofunc49776 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40881)
musttail call tailcc void %clofunc49776(%struct.ScmObj* %ae40881, %struct.ScmObj* %args49630$ae40881$2)
ret void
}

define tailcc void @proc_clo$ae40881(%struct.ScmObj* %env$ae40881,%struct.ScmObj* %current_45args49179) {
%stackaddr$env-ref49777 = alloca %struct.ScmObj*, align 8
%_37foldr140141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40881, i64 0)
store %struct.ScmObj* %_37foldr140141, %struct.ScmObj** %stackaddr$env-ref49777
%stackaddr$env-ref49778 = alloca %struct.ScmObj*, align 8
%_37map140137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40881, i64 1)
store %struct.ScmObj* %_37map140137, %struct.ScmObj** %stackaddr$env-ref49778
%stackaddr$env-ref49779 = alloca %struct.ScmObj*, align 8
%Ycmb40120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40881, i64 2)
store %struct.ScmObj* %Ycmb40120, %struct.ScmObj** %stackaddr$env-ref49779
%stackaddr$prim49780 = alloca %struct.ScmObj*, align 8
%_95k40416 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49179)
store volatile %struct.ScmObj* %_95k40416, %struct.ScmObj** %stackaddr$prim49780, align 8
%stackaddr$prim49781 = alloca %struct.ScmObj*, align 8
%current_45args49180 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49179)
store volatile %struct.ScmObj* %current_45args49180, %struct.ScmObj** %stackaddr$prim49781, align 8
%stackaddr$prim49782 = alloca %struct.ScmObj*, align 8
%anf_45bind40298 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49180)
store volatile %struct.ScmObj* %anf_45bind40298, %struct.ScmObj** %stackaddr$prim49782, align 8
%stackaddr$makeclosure49783 = alloca %struct.ScmObj*, align 8
%fptrToInt49784 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41029 to i64
%ae41029 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49784)
store volatile %struct.ScmObj* %ae41029, %struct.ScmObj** %stackaddr$makeclosure49783, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41029, %struct.ScmObj* %_37foldr140141, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41029, %struct.ScmObj* %_37map140137, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41029, %struct.ScmObj* %Ycmb40120, i64 2)
%args49614$Ycmb40120$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49785 = alloca %struct.ScmObj*, align 8
%args49614$Ycmb40120$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40298, %struct.ScmObj* %args49614$Ycmb40120$0)
store volatile %struct.ScmObj* %args49614$Ycmb40120$1, %struct.ScmObj** %stackaddr$prim49785, align 8
%stackaddr$prim49786 = alloca %struct.ScmObj*, align 8
%args49614$Ycmb40120$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41029, %struct.ScmObj* %args49614$Ycmb40120$1)
store volatile %struct.ScmObj* %args49614$Ycmb40120$2, %struct.ScmObj** %stackaddr$prim49786, align 8
%clofunc49787 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40120)
musttail call tailcc void %clofunc49787(%struct.ScmObj* %Ycmb40120, %struct.ScmObj* %args49614$Ycmb40120$2)
ret void
}

define tailcc void @proc_clo$ae41029(%struct.ScmObj* %env$ae41029,%struct.ScmObj* %current_45args49182) {
%stackaddr$env-ref49788 = alloca %struct.ScmObj*, align 8
%_37foldr140141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41029, i64 0)
store %struct.ScmObj* %_37foldr140141, %struct.ScmObj** %stackaddr$env-ref49788
%stackaddr$env-ref49789 = alloca %struct.ScmObj*, align 8
%_37map140137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41029, i64 1)
store %struct.ScmObj* %_37map140137, %struct.ScmObj** %stackaddr$env-ref49789
%stackaddr$env-ref49790 = alloca %struct.ScmObj*, align 8
%Ycmb40120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41029, i64 2)
store %struct.ScmObj* %Ycmb40120, %struct.ScmObj** %stackaddr$env-ref49790
%stackaddr$prim49791 = alloca %struct.ScmObj*, align 8
%_95k40417 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49182)
store volatile %struct.ScmObj* %_95k40417, %struct.ScmObj** %stackaddr$prim49791, align 8
%stackaddr$prim49792 = alloca %struct.ScmObj*, align 8
%current_45args49183 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49182)
store volatile %struct.ScmObj* %current_45args49183, %struct.ScmObj** %stackaddr$prim49792, align 8
%stackaddr$prim49793 = alloca %struct.ScmObj*, align 8
%_37take40133 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49183)
store volatile %struct.ScmObj* %_37take40133, %struct.ScmObj** %stackaddr$prim49793, align 8
%stackaddr$makeclosure49794 = alloca %struct.ScmObj*, align 8
%fptrToInt49795 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41031 to i64
%ae41031 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt49795)
store volatile %struct.ScmObj* %ae41031, %struct.ScmObj** %stackaddr$makeclosure49794, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41031, %struct.ScmObj* %_37take40133, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41031, %struct.ScmObj* %_37foldr140141, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41031, %struct.ScmObj* %_37map140137, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41031, %struct.ScmObj* %Ycmb40120, i64 3)
%ae41032 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49796 = alloca %struct.ScmObj*, align 8
%fptrToInt49797 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41033 to i64
%ae41033 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49797)
store volatile %struct.ScmObj* %ae41033, %struct.ScmObj** %stackaddr$makeclosure49796, align 8
%args49613$ae41031$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49798 = alloca %struct.ScmObj*, align 8
%args49613$ae41031$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41033, %struct.ScmObj* %args49613$ae41031$0)
store volatile %struct.ScmObj* %args49613$ae41031$1, %struct.ScmObj** %stackaddr$prim49798, align 8
%stackaddr$prim49799 = alloca %struct.ScmObj*, align 8
%args49613$ae41031$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41032, %struct.ScmObj* %args49613$ae41031$1)
store volatile %struct.ScmObj* %args49613$ae41031$2, %struct.ScmObj** %stackaddr$prim49799, align 8
%clofunc49800 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41031)
musttail call tailcc void %clofunc49800(%struct.ScmObj* %ae41031, %struct.ScmObj* %args49613$ae41031$2)
ret void
}

define tailcc void @proc_clo$ae41031(%struct.ScmObj* %env$ae41031,%struct.ScmObj* %current_45args49185) {
%stackaddr$env-ref49801 = alloca %struct.ScmObj*, align 8
%_37take40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41031, i64 0)
store %struct.ScmObj* %_37take40133, %struct.ScmObj** %stackaddr$env-ref49801
%stackaddr$env-ref49802 = alloca %struct.ScmObj*, align 8
%_37foldr140141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41031, i64 1)
store %struct.ScmObj* %_37foldr140141, %struct.ScmObj** %stackaddr$env-ref49802
%stackaddr$env-ref49803 = alloca %struct.ScmObj*, align 8
%_37map140137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41031, i64 2)
store %struct.ScmObj* %_37map140137, %struct.ScmObj** %stackaddr$env-ref49803
%stackaddr$env-ref49804 = alloca %struct.ScmObj*, align 8
%Ycmb40120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41031, i64 3)
store %struct.ScmObj* %Ycmb40120, %struct.ScmObj** %stackaddr$env-ref49804
%stackaddr$prim49805 = alloca %struct.ScmObj*, align 8
%_95k40418 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49185)
store volatile %struct.ScmObj* %_95k40418, %struct.ScmObj** %stackaddr$prim49805, align 8
%stackaddr$prim49806 = alloca %struct.ScmObj*, align 8
%current_45args49186 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49185)
store volatile %struct.ScmObj* %current_45args49186, %struct.ScmObj** %stackaddr$prim49806, align 8
%stackaddr$prim49807 = alloca %struct.ScmObj*, align 8
%anf_45bind40302 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49186)
store volatile %struct.ScmObj* %anf_45bind40302, %struct.ScmObj** %stackaddr$prim49807, align 8
%stackaddr$makeclosure49808 = alloca %struct.ScmObj*, align 8
%fptrToInt49809 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41112 to i64
%ae41112 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt49809)
store volatile %struct.ScmObj* %ae41112, %struct.ScmObj** %stackaddr$makeclosure49808, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41112, %struct.ScmObj* %_37take40133, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41112, %struct.ScmObj* %_37foldr140141, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41112, %struct.ScmObj* %_37map140137, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41112, %struct.ScmObj* %Ycmb40120, i64 3)
%args49599$Ycmb40120$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49810 = alloca %struct.ScmObj*, align 8
%args49599$Ycmb40120$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40302, %struct.ScmObj* %args49599$Ycmb40120$0)
store volatile %struct.ScmObj* %args49599$Ycmb40120$1, %struct.ScmObj** %stackaddr$prim49810, align 8
%stackaddr$prim49811 = alloca %struct.ScmObj*, align 8
%args49599$Ycmb40120$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41112, %struct.ScmObj* %args49599$Ycmb40120$1)
store volatile %struct.ScmObj* %args49599$Ycmb40120$2, %struct.ScmObj** %stackaddr$prim49811, align 8
%clofunc49812 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40120)
musttail call tailcc void %clofunc49812(%struct.ScmObj* %Ycmb40120, %struct.ScmObj* %args49599$Ycmb40120$2)
ret void
}

define tailcc void @proc_clo$ae41112(%struct.ScmObj* %env$ae41112,%struct.ScmObj* %current_45args49188) {
%stackaddr$env-ref49813 = alloca %struct.ScmObj*, align 8
%_37take40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41112, i64 0)
store %struct.ScmObj* %_37take40133, %struct.ScmObj** %stackaddr$env-ref49813
%stackaddr$env-ref49814 = alloca %struct.ScmObj*, align 8
%_37foldr140141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41112, i64 1)
store %struct.ScmObj* %_37foldr140141, %struct.ScmObj** %stackaddr$env-ref49814
%stackaddr$env-ref49815 = alloca %struct.ScmObj*, align 8
%_37map140137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41112, i64 2)
store %struct.ScmObj* %_37map140137, %struct.ScmObj** %stackaddr$env-ref49815
%stackaddr$env-ref49816 = alloca %struct.ScmObj*, align 8
%Ycmb40120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41112, i64 3)
store %struct.ScmObj* %Ycmb40120, %struct.ScmObj** %stackaddr$env-ref49816
%stackaddr$prim49817 = alloca %struct.ScmObj*, align 8
%_95k40419 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49188)
store volatile %struct.ScmObj* %_95k40419, %struct.ScmObj** %stackaddr$prim49817, align 8
%stackaddr$prim49818 = alloca %struct.ScmObj*, align 8
%current_45args49189 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49188)
store volatile %struct.ScmObj* %current_45args49189, %struct.ScmObj** %stackaddr$prim49818, align 8
%stackaddr$prim49819 = alloca %struct.ScmObj*, align 8
%_37length40130 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49189)
store volatile %struct.ScmObj* %_37length40130, %struct.ScmObj** %stackaddr$prim49819, align 8
%stackaddr$makeclosure49820 = alloca %struct.ScmObj*, align 8
%fptrToInt49821 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41114 to i64
%ae41114 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt49821)
store volatile %struct.ScmObj* %ae41114, %struct.ScmObj** %stackaddr$makeclosure49820, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41114, %struct.ScmObj* %_37take40133, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41114, %struct.ScmObj* %_37length40130, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41114, %struct.ScmObj* %_37foldr140141, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41114, %struct.ScmObj* %_37map140137, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41114, %struct.ScmObj* %Ycmb40120, i64 4)
%ae41115 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49822 = alloca %struct.ScmObj*, align 8
%fptrToInt49823 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41116 to i64
%ae41116 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49823)
store volatile %struct.ScmObj* %ae41116, %struct.ScmObj** %stackaddr$makeclosure49822, align 8
%args49598$ae41114$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49824 = alloca %struct.ScmObj*, align 8
%args49598$ae41114$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41116, %struct.ScmObj* %args49598$ae41114$0)
store volatile %struct.ScmObj* %args49598$ae41114$1, %struct.ScmObj** %stackaddr$prim49824, align 8
%stackaddr$prim49825 = alloca %struct.ScmObj*, align 8
%args49598$ae41114$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41115, %struct.ScmObj* %args49598$ae41114$1)
store volatile %struct.ScmObj* %args49598$ae41114$2, %struct.ScmObj** %stackaddr$prim49825, align 8
%clofunc49826 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41114)
musttail call tailcc void %clofunc49826(%struct.ScmObj* %ae41114, %struct.ScmObj* %args49598$ae41114$2)
ret void
}

define tailcc void @proc_clo$ae41114(%struct.ScmObj* %env$ae41114,%struct.ScmObj* %current_45args49191) {
%stackaddr$env-ref49827 = alloca %struct.ScmObj*, align 8
%_37take40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41114, i64 0)
store %struct.ScmObj* %_37take40133, %struct.ScmObj** %stackaddr$env-ref49827
%stackaddr$env-ref49828 = alloca %struct.ScmObj*, align 8
%_37length40130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41114, i64 1)
store %struct.ScmObj* %_37length40130, %struct.ScmObj** %stackaddr$env-ref49828
%stackaddr$env-ref49829 = alloca %struct.ScmObj*, align 8
%_37foldr140141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41114, i64 2)
store %struct.ScmObj* %_37foldr140141, %struct.ScmObj** %stackaddr$env-ref49829
%stackaddr$env-ref49830 = alloca %struct.ScmObj*, align 8
%_37map140137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41114, i64 3)
store %struct.ScmObj* %_37map140137, %struct.ScmObj** %stackaddr$env-ref49830
%stackaddr$env-ref49831 = alloca %struct.ScmObj*, align 8
%Ycmb40120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41114, i64 4)
store %struct.ScmObj* %Ycmb40120, %struct.ScmObj** %stackaddr$env-ref49831
%stackaddr$prim49832 = alloca %struct.ScmObj*, align 8
%_95k40420 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49191)
store volatile %struct.ScmObj* %_95k40420, %struct.ScmObj** %stackaddr$prim49832, align 8
%stackaddr$prim49833 = alloca %struct.ScmObj*, align 8
%current_45args49192 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49191)
store volatile %struct.ScmObj* %current_45args49192, %struct.ScmObj** %stackaddr$prim49833, align 8
%stackaddr$prim49834 = alloca %struct.ScmObj*, align 8
%anf_45bind40307 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49192)
store volatile %struct.ScmObj* %anf_45bind40307, %struct.ScmObj** %stackaddr$prim49834, align 8
%stackaddr$makeclosure49835 = alloca %struct.ScmObj*, align 8
%fptrToInt49836 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41191 to i64
%ae41191 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt49836)
store volatile %struct.ScmObj* %ae41191, %struct.ScmObj** %stackaddr$makeclosure49835, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41191, %struct.ScmObj* %_37take40133, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41191, %struct.ScmObj* %_37length40130, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41191, %struct.ScmObj* %_37foldr140141, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41191, %struct.ScmObj* %_37map140137, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41191, %struct.ScmObj* %Ycmb40120, i64 4)
%args49582$Ycmb40120$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49837 = alloca %struct.ScmObj*, align 8
%args49582$Ycmb40120$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40307, %struct.ScmObj* %args49582$Ycmb40120$0)
store volatile %struct.ScmObj* %args49582$Ycmb40120$1, %struct.ScmObj** %stackaddr$prim49837, align 8
%stackaddr$prim49838 = alloca %struct.ScmObj*, align 8
%args49582$Ycmb40120$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41191, %struct.ScmObj* %args49582$Ycmb40120$1)
store volatile %struct.ScmObj* %args49582$Ycmb40120$2, %struct.ScmObj** %stackaddr$prim49838, align 8
%clofunc49839 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40120)
musttail call tailcc void %clofunc49839(%struct.ScmObj* %Ycmb40120, %struct.ScmObj* %args49582$Ycmb40120$2)
ret void
}

define tailcc void @proc_clo$ae41191(%struct.ScmObj* %env$ae41191,%struct.ScmObj* %current_45args49194) {
%stackaddr$env-ref49840 = alloca %struct.ScmObj*, align 8
%_37take40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41191, i64 0)
store %struct.ScmObj* %_37take40133, %struct.ScmObj** %stackaddr$env-ref49840
%stackaddr$env-ref49841 = alloca %struct.ScmObj*, align 8
%_37length40130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41191, i64 1)
store %struct.ScmObj* %_37length40130, %struct.ScmObj** %stackaddr$env-ref49841
%stackaddr$env-ref49842 = alloca %struct.ScmObj*, align 8
%_37foldr140141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41191, i64 2)
store %struct.ScmObj* %_37foldr140141, %struct.ScmObj** %stackaddr$env-ref49842
%stackaddr$env-ref49843 = alloca %struct.ScmObj*, align 8
%_37map140137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41191, i64 3)
store %struct.ScmObj* %_37map140137, %struct.ScmObj** %stackaddr$env-ref49843
%stackaddr$env-ref49844 = alloca %struct.ScmObj*, align 8
%Ycmb40120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41191, i64 4)
store %struct.ScmObj* %Ycmb40120, %struct.ScmObj** %stackaddr$env-ref49844
%stackaddr$prim49845 = alloca %struct.ScmObj*, align 8
%_95k40421 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49194)
store volatile %struct.ScmObj* %_95k40421, %struct.ScmObj** %stackaddr$prim49845, align 8
%stackaddr$prim49846 = alloca %struct.ScmObj*, align 8
%current_45args49195 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49194)
store volatile %struct.ScmObj* %current_45args49195, %struct.ScmObj** %stackaddr$prim49846, align 8
%stackaddr$prim49847 = alloca %struct.ScmObj*, align 8
%_37foldl140125 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49195)
store volatile %struct.ScmObj* %_37foldl140125, %struct.ScmObj** %stackaddr$prim49847, align 8
%stackaddr$makeclosure49848 = alloca %struct.ScmObj*, align 8
%fptrToInt49849 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41193 to i64
%ae41193 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt49849)
store volatile %struct.ScmObj* %ae41193, %struct.ScmObj** %stackaddr$makeclosure49848, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41193, %struct.ScmObj* %_37foldr140141, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41193, %struct.ScmObj* %_37foldl140125, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41193, %struct.ScmObj* %_37take40133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41193, %struct.ScmObj* %_37length40130, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41193, %struct.ScmObj* %_37map140137, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41193, %struct.ScmObj* %Ycmb40120, i64 5)
%ae41194 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49850 = alloca %struct.ScmObj*, align 8
%fptrToInt49851 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41195 to i64
%ae41195 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49851)
store volatile %struct.ScmObj* %ae41195, %struct.ScmObj** %stackaddr$makeclosure49850, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41195, %struct.ScmObj* %_37foldl140125, i64 0)
%args49581$ae41193$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49852 = alloca %struct.ScmObj*, align 8
%args49581$ae41193$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41195, %struct.ScmObj* %args49581$ae41193$0)
store volatile %struct.ScmObj* %args49581$ae41193$1, %struct.ScmObj** %stackaddr$prim49852, align 8
%stackaddr$prim49853 = alloca %struct.ScmObj*, align 8
%args49581$ae41193$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41194, %struct.ScmObj* %args49581$ae41193$1)
store volatile %struct.ScmObj* %args49581$ae41193$2, %struct.ScmObj** %stackaddr$prim49853, align 8
%clofunc49854 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41193)
musttail call tailcc void %clofunc49854(%struct.ScmObj* %ae41193, %struct.ScmObj* %args49581$ae41193$2)
ret void
}

define tailcc void @proc_clo$ae41193(%struct.ScmObj* %env$ae41193,%struct.ScmObj* %current_45args49197) {
%stackaddr$env-ref49855 = alloca %struct.ScmObj*, align 8
%_37foldr140141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41193, i64 0)
store %struct.ScmObj* %_37foldr140141, %struct.ScmObj** %stackaddr$env-ref49855
%stackaddr$env-ref49856 = alloca %struct.ScmObj*, align 8
%_37foldl140125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41193, i64 1)
store %struct.ScmObj* %_37foldl140125, %struct.ScmObj** %stackaddr$env-ref49856
%stackaddr$env-ref49857 = alloca %struct.ScmObj*, align 8
%_37take40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41193, i64 2)
store %struct.ScmObj* %_37take40133, %struct.ScmObj** %stackaddr$env-ref49857
%stackaddr$env-ref49858 = alloca %struct.ScmObj*, align 8
%_37length40130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41193, i64 3)
store %struct.ScmObj* %_37length40130, %struct.ScmObj** %stackaddr$env-ref49858
%stackaddr$env-ref49859 = alloca %struct.ScmObj*, align 8
%_37map140137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41193, i64 4)
store %struct.ScmObj* %_37map140137, %struct.ScmObj** %stackaddr$env-ref49859
%stackaddr$env-ref49860 = alloca %struct.ScmObj*, align 8
%Ycmb40120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41193, i64 5)
store %struct.ScmObj* %Ycmb40120, %struct.ScmObj** %stackaddr$env-ref49860
%stackaddr$prim49861 = alloca %struct.ScmObj*, align 8
%_95k40422 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49197)
store volatile %struct.ScmObj* %_95k40422, %struct.ScmObj** %stackaddr$prim49861, align 8
%stackaddr$prim49862 = alloca %struct.ScmObj*, align 8
%current_45args49198 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49197)
store volatile %struct.ScmObj* %current_45args49198, %struct.ScmObj** %stackaddr$prim49862, align 8
%stackaddr$prim49863 = alloca %struct.ScmObj*, align 8
%_37last40163 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49198)
store volatile %struct.ScmObj* %_37last40163, %struct.ScmObj** %stackaddr$prim49863, align 8
%stackaddr$makeclosure49864 = alloca %struct.ScmObj*, align 8
%fptrToInt49865 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41247 to i64
%ae41247 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt49865)
store volatile %struct.ScmObj* %ae41247, %struct.ScmObj** %stackaddr$makeclosure49864, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41247, %struct.ScmObj* %_37foldr140141, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41247, %struct.ScmObj* %_37foldl140125, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41247, %struct.ScmObj* %_37last40163, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41247, %struct.ScmObj* %_37map140137, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41247, %struct.ScmObj* %Ycmb40120, i64 4)
%ae41248 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49866 = alloca %struct.ScmObj*, align 8
%fptrToInt49867 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41249 to i64
%ae41249 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49867)
store volatile %struct.ScmObj* %ae41249, %struct.ScmObj** %stackaddr$makeclosure49866, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41249, %struct.ScmObj* %_37take40133, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41249, %struct.ScmObj* %_37length40130, i64 1)
%args49567$ae41247$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49868 = alloca %struct.ScmObj*, align 8
%args49567$ae41247$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41249, %struct.ScmObj* %args49567$ae41247$0)
store volatile %struct.ScmObj* %args49567$ae41247$1, %struct.ScmObj** %stackaddr$prim49868, align 8
%stackaddr$prim49869 = alloca %struct.ScmObj*, align 8
%args49567$ae41247$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41248, %struct.ScmObj* %args49567$ae41247$1)
store volatile %struct.ScmObj* %args49567$ae41247$2, %struct.ScmObj** %stackaddr$prim49869, align 8
%clofunc49870 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41247)
musttail call tailcc void %clofunc49870(%struct.ScmObj* %ae41247, %struct.ScmObj* %args49567$ae41247$2)
ret void
}

define tailcc void @proc_clo$ae41247(%struct.ScmObj* %env$ae41247,%struct.ScmObj* %current_45args49200) {
%stackaddr$env-ref49871 = alloca %struct.ScmObj*, align 8
%_37foldr140141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41247, i64 0)
store %struct.ScmObj* %_37foldr140141, %struct.ScmObj** %stackaddr$env-ref49871
%stackaddr$env-ref49872 = alloca %struct.ScmObj*, align 8
%_37foldl140125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41247, i64 1)
store %struct.ScmObj* %_37foldl140125, %struct.ScmObj** %stackaddr$env-ref49872
%stackaddr$env-ref49873 = alloca %struct.ScmObj*, align 8
%_37last40163 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41247, i64 2)
store %struct.ScmObj* %_37last40163, %struct.ScmObj** %stackaddr$env-ref49873
%stackaddr$env-ref49874 = alloca %struct.ScmObj*, align 8
%_37map140137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41247, i64 3)
store %struct.ScmObj* %_37map140137, %struct.ScmObj** %stackaddr$env-ref49874
%stackaddr$env-ref49875 = alloca %struct.ScmObj*, align 8
%Ycmb40120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41247, i64 4)
store %struct.ScmObj* %Ycmb40120, %struct.ScmObj** %stackaddr$env-ref49875
%stackaddr$prim49876 = alloca %struct.ScmObj*, align 8
%_95k40423 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49200)
store volatile %struct.ScmObj* %_95k40423, %struct.ScmObj** %stackaddr$prim49876, align 8
%stackaddr$prim49877 = alloca %struct.ScmObj*, align 8
%current_45args49201 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49200)
store volatile %struct.ScmObj* %current_45args49201, %struct.ScmObj** %stackaddr$prim49877, align 8
%stackaddr$prim49878 = alloca %struct.ScmObj*, align 8
%_37drop_45right40160 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49201)
store volatile %struct.ScmObj* %_37drop_45right40160, %struct.ScmObj** %stackaddr$prim49878, align 8
%stackaddr$makeclosure49879 = alloca %struct.ScmObj*, align 8
%fptrToInt49880 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41277 to i64
%ae41277 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt49880)
store volatile %struct.ScmObj* %ae41277, %struct.ScmObj** %stackaddr$makeclosure49879, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41277, %struct.ScmObj* %_37foldr140141, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41277, %struct.ScmObj* %_37foldl140125, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41277, %struct.ScmObj* %_37last40163, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41277, %struct.ScmObj* %_37drop_45right40160, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41277, %struct.ScmObj* %Ycmb40120, i64 4)
%ae41278 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49881 = alloca %struct.ScmObj*, align 8
%fptrToInt49882 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41279 to i64
%ae41279 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49882)
store volatile %struct.ScmObj* %ae41279, %struct.ScmObj** %stackaddr$makeclosure49881, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41279, %struct.ScmObj* %_37foldr140141, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41279, %struct.ScmObj* %_37map140137, i64 1)
%args49557$ae41277$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49883 = alloca %struct.ScmObj*, align 8
%args49557$ae41277$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41279, %struct.ScmObj* %args49557$ae41277$0)
store volatile %struct.ScmObj* %args49557$ae41277$1, %struct.ScmObj** %stackaddr$prim49883, align 8
%stackaddr$prim49884 = alloca %struct.ScmObj*, align 8
%args49557$ae41277$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41278, %struct.ScmObj* %args49557$ae41277$1)
store volatile %struct.ScmObj* %args49557$ae41277$2, %struct.ScmObj** %stackaddr$prim49884, align 8
%clofunc49885 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41277)
musttail call tailcc void %clofunc49885(%struct.ScmObj* %ae41277, %struct.ScmObj* %args49557$ae41277$2)
ret void
}

define tailcc void @proc_clo$ae41277(%struct.ScmObj* %env$ae41277,%struct.ScmObj* %current_45args49203) {
%stackaddr$env-ref49886 = alloca %struct.ScmObj*, align 8
%_37foldr140141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41277, i64 0)
store %struct.ScmObj* %_37foldr140141, %struct.ScmObj** %stackaddr$env-ref49886
%stackaddr$env-ref49887 = alloca %struct.ScmObj*, align 8
%_37foldl140125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41277, i64 1)
store %struct.ScmObj* %_37foldl140125, %struct.ScmObj** %stackaddr$env-ref49887
%stackaddr$env-ref49888 = alloca %struct.ScmObj*, align 8
%_37last40163 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41277, i64 2)
store %struct.ScmObj* %_37last40163, %struct.ScmObj** %stackaddr$env-ref49888
%stackaddr$env-ref49889 = alloca %struct.ScmObj*, align 8
%_37drop_45right40160 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41277, i64 3)
store %struct.ScmObj* %_37drop_45right40160, %struct.ScmObj** %stackaddr$env-ref49889
%stackaddr$env-ref49890 = alloca %struct.ScmObj*, align 8
%Ycmb40120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41277, i64 4)
store %struct.ScmObj* %Ycmb40120, %struct.ScmObj** %stackaddr$env-ref49890
%stackaddr$prim49891 = alloca %struct.ScmObj*, align 8
%_95k40424 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49203)
store volatile %struct.ScmObj* %_95k40424, %struct.ScmObj** %stackaddr$prim49891, align 8
%stackaddr$prim49892 = alloca %struct.ScmObj*, align 8
%current_45args49204 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49203)
store volatile %struct.ScmObj* %current_45args49204, %struct.ScmObj** %stackaddr$prim49892, align 8
%stackaddr$prim49893 = alloca %struct.ScmObj*, align 8
%anf_45bind40323 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49204)
store volatile %struct.ScmObj* %anf_45bind40323, %struct.ScmObj** %stackaddr$prim49893, align 8
%stackaddr$makeclosure49894 = alloca %struct.ScmObj*, align 8
%fptrToInt49895 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41661 to i64
%ae41661 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt49895)
store volatile %struct.ScmObj* %ae41661, %struct.ScmObj** %stackaddr$makeclosure49894, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41661, %struct.ScmObj* %_37foldr140141, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41661, %struct.ScmObj* %_37foldl140125, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41661, %struct.ScmObj* %_37last40163, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41661, %struct.ScmObj* %_37drop_45right40160, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41661, %struct.ScmObj* %Ycmb40120, i64 4)
%args49497$Ycmb40120$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49896 = alloca %struct.ScmObj*, align 8
%args49497$Ycmb40120$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40323, %struct.ScmObj* %args49497$Ycmb40120$0)
store volatile %struct.ScmObj* %args49497$Ycmb40120$1, %struct.ScmObj** %stackaddr$prim49896, align 8
%stackaddr$prim49897 = alloca %struct.ScmObj*, align 8
%args49497$Ycmb40120$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41661, %struct.ScmObj* %args49497$Ycmb40120$1)
store volatile %struct.ScmObj* %args49497$Ycmb40120$2, %struct.ScmObj** %stackaddr$prim49897, align 8
%clofunc49898 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40120)
musttail call tailcc void %clofunc49898(%struct.ScmObj* %Ycmb40120, %struct.ScmObj* %args49497$Ycmb40120$2)
ret void
}

define tailcc void @proc_clo$ae41661(%struct.ScmObj* %env$ae41661,%struct.ScmObj* %current_45args49206) {
%stackaddr$env-ref49899 = alloca %struct.ScmObj*, align 8
%_37foldr140141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41661, i64 0)
store %struct.ScmObj* %_37foldr140141, %struct.ScmObj** %stackaddr$env-ref49899
%stackaddr$env-ref49900 = alloca %struct.ScmObj*, align 8
%_37foldl140125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41661, i64 1)
store %struct.ScmObj* %_37foldl140125, %struct.ScmObj** %stackaddr$env-ref49900
%stackaddr$env-ref49901 = alloca %struct.ScmObj*, align 8
%_37last40163 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41661, i64 2)
store %struct.ScmObj* %_37last40163, %struct.ScmObj** %stackaddr$env-ref49901
%stackaddr$env-ref49902 = alloca %struct.ScmObj*, align 8
%_37drop_45right40160 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41661, i64 3)
store %struct.ScmObj* %_37drop_45right40160, %struct.ScmObj** %stackaddr$env-ref49902
%stackaddr$env-ref49903 = alloca %struct.ScmObj*, align 8
%Ycmb40120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41661, i64 4)
store %struct.ScmObj* %Ycmb40120, %struct.ScmObj** %stackaddr$env-ref49903
%stackaddr$prim49904 = alloca %struct.ScmObj*, align 8
%_95k40425 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49206)
store volatile %struct.ScmObj* %_95k40425, %struct.ScmObj** %stackaddr$prim49904, align 8
%stackaddr$prim49905 = alloca %struct.ScmObj*, align 8
%current_45args49207 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49206)
store volatile %struct.ScmObj* %current_45args49207, %struct.ScmObj** %stackaddr$prim49905, align 8
%stackaddr$prim49906 = alloca %struct.ScmObj*, align 8
%_37foldr40146 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49207)
store volatile %struct.ScmObj* %_37foldr40146, %struct.ScmObj** %stackaddr$prim49906, align 8
%stackaddr$makeclosure49907 = alloca %struct.ScmObj*, align 8
%fptrToInt49908 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41663 to i64
%ae41663 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt49908)
store volatile %struct.ScmObj* %ae41663, %struct.ScmObj** %stackaddr$makeclosure49907, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41663, %struct.ScmObj* %_37foldr140141, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41663, %struct.ScmObj* %_37foldl140125, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41663, %struct.ScmObj* %_37last40163, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41663, %struct.ScmObj* %_37foldr40146, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41663, %struct.ScmObj* %_37drop_45right40160, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41663, %struct.ScmObj* %Ycmb40120, i64 5)
%ae41664 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49909 = alloca %struct.ScmObj*, align 8
%fptrToInt49910 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41665 to i64
%ae41665 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49910)
store volatile %struct.ScmObj* %ae41665, %struct.ScmObj** %stackaddr$makeclosure49909, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41665, %struct.ScmObj* %_37foldr140141, i64 0)
%args49496$ae41663$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49911 = alloca %struct.ScmObj*, align 8
%args49496$ae41663$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41665, %struct.ScmObj* %args49496$ae41663$0)
store volatile %struct.ScmObj* %args49496$ae41663$1, %struct.ScmObj** %stackaddr$prim49911, align 8
%stackaddr$prim49912 = alloca %struct.ScmObj*, align 8
%args49496$ae41663$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41664, %struct.ScmObj* %args49496$ae41663$1)
store volatile %struct.ScmObj* %args49496$ae41663$2, %struct.ScmObj** %stackaddr$prim49912, align 8
%clofunc49913 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41663)
musttail call tailcc void %clofunc49913(%struct.ScmObj* %ae41663, %struct.ScmObj* %args49496$ae41663$2)
ret void
}

define tailcc void @proc_clo$ae41663(%struct.ScmObj* %env$ae41663,%struct.ScmObj* %current_45args49209) {
%stackaddr$env-ref49914 = alloca %struct.ScmObj*, align 8
%_37foldr140141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41663, i64 0)
store %struct.ScmObj* %_37foldr140141, %struct.ScmObj** %stackaddr$env-ref49914
%stackaddr$env-ref49915 = alloca %struct.ScmObj*, align 8
%_37foldl140125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41663, i64 1)
store %struct.ScmObj* %_37foldl140125, %struct.ScmObj** %stackaddr$env-ref49915
%stackaddr$env-ref49916 = alloca %struct.ScmObj*, align 8
%_37last40163 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41663, i64 2)
store %struct.ScmObj* %_37last40163, %struct.ScmObj** %stackaddr$env-ref49916
%stackaddr$env-ref49917 = alloca %struct.ScmObj*, align 8
%_37foldr40146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41663, i64 3)
store %struct.ScmObj* %_37foldr40146, %struct.ScmObj** %stackaddr$env-ref49917
%stackaddr$env-ref49918 = alloca %struct.ScmObj*, align 8
%_37drop_45right40160 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41663, i64 4)
store %struct.ScmObj* %_37drop_45right40160, %struct.ScmObj** %stackaddr$env-ref49918
%stackaddr$env-ref49919 = alloca %struct.ScmObj*, align 8
%Ycmb40120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41663, i64 5)
store %struct.ScmObj* %Ycmb40120, %struct.ScmObj** %stackaddr$env-ref49919
%stackaddr$prim49920 = alloca %struct.ScmObj*, align 8
%_95k40426 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49209)
store volatile %struct.ScmObj* %_95k40426, %struct.ScmObj** %stackaddr$prim49920, align 8
%stackaddr$prim49921 = alloca %struct.ScmObj*, align 8
%current_45args49210 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49209)
store volatile %struct.ScmObj* %current_45args49210, %struct.ScmObj** %stackaddr$prim49921, align 8
%stackaddr$prim49922 = alloca %struct.ScmObj*, align 8
%_37map140172 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49210)
store volatile %struct.ScmObj* %_37map140172, %struct.ScmObj** %stackaddr$prim49922, align 8
%stackaddr$makeclosure49923 = alloca %struct.ScmObj*, align 8
%fptrToInt49924 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41740 to i64
%ae41740 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt49924)
store volatile %struct.ScmObj* %ae41740, %struct.ScmObj** %stackaddr$makeclosure49923, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41740, %struct.ScmObj* %_37foldr140141, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41740, %struct.ScmObj* %_37foldl140125, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41740, %struct.ScmObj* %_37foldr40146, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41740, %struct.ScmObj* %_37map140172, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41740, %struct.ScmObj* %Ycmb40120, i64 4)
%ae41741 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49925 = alloca %struct.ScmObj*, align 8
%fptrToInt49926 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41742 to i64
%ae41742 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49926)
store volatile %struct.ScmObj* %ae41742, %struct.ScmObj** %stackaddr$makeclosure49925, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41742, %struct.ScmObj* %_37last40163, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41742, %struct.ScmObj* %_37foldr40146, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41742, %struct.ScmObj* %_37drop_45right40160, i64 2)
%args49477$ae41740$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49927 = alloca %struct.ScmObj*, align 8
%args49477$ae41740$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41742, %struct.ScmObj* %args49477$ae41740$0)
store volatile %struct.ScmObj* %args49477$ae41740$1, %struct.ScmObj** %stackaddr$prim49927, align 8
%stackaddr$prim49928 = alloca %struct.ScmObj*, align 8
%args49477$ae41740$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41741, %struct.ScmObj* %args49477$ae41740$1)
store volatile %struct.ScmObj* %args49477$ae41740$2, %struct.ScmObj** %stackaddr$prim49928, align 8
%clofunc49929 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41740)
musttail call tailcc void %clofunc49929(%struct.ScmObj* %ae41740, %struct.ScmObj* %args49477$ae41740$2)
ret void
}

define tailcc void @proc_clo$ae41740(%struct.ScmObj* %env$ae41740,%struct.ScmObj* %current_45args49212) {
%stackaddr$env-ref49930 = alloca %struct.ScmObj*, align 8
%_37foldr140141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41740, i64 0)
store %struct.ScmObj* %_37foldr140141, %struct.ScmObj** %stackaddr$env-ref49930
%stackaddr$env-ref49931 = alloca %struct.ScmObj*, align 8
%_37foldl140125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41740, i64 1)
store %struct.ScmObj* %_37foldl140125, %struct.ScmObj** %stackaddr$env-ref49931
%stackaddr$env-ref49932 = alloca %struct.ScmObj*, align 8
%_37foldr40146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41740, i64 2)
store %struct.ScmObj* %_37foldr40146, %struct.ScmObj** %stackaddr$env-ref49932
%stackaddr$env-ref49933 = alloca %struct.ScmObj*, align 8
%_37map140172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41740, i64 3)
store %struct.ScmObj* %_37map140172, %struct.ScmObj** %stackaddr$env-ref49933
%stackaddr$env-ref49934 = alloca %struct.ScmObj*, align 8
%Ycmb40120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41740, i64 4)
store %struct.ScmObj* %Ycmb40120, %struct.ScmObj** %stackaddr$env-ref49934
%stackaddr$prim49935 = alloca %struct.ScmObj*, align 8
%_95k40427 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49212)
store volatile %struct.ScmObj* %_95k40427, %struct.ScmObj** %stackaddr$prim49935, align 8
%stackaddr$prim49936 = alloca %struct.ScmObj*, align 8
%current_45args49213 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49212)
store volatile %struct.ScmObj* %current_45args49213, %struct.ScmObj** %stackaddr$prim49936, align 8
%stackaddr$prim49937 = alloca %struct.ScmObj*, align 8
%_37map40167 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49213)
store volatile %struct.ScmObj* %_37map40167, %struct.ScmObj** %stackaddr$prim49937, align 8
%stackaddr$makeclosure49938 = alloca %struct.ScmObj*, align 8
%fptrToInt49939 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41886 to i64
%ae41886 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49939)
store volatile %struct.ScmObj* %ae41886, %struct.ScmObj** %stackaddr$makeclosure49938, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41886, %struct.ScmObj* %_37foldl140125, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41886, %struct.ScmObj* %Ycmb40120, i64 1)
%ae41887 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49940 = alloca %struct.ScmObj*, align 8
%fptrToInt49941 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41888 to i64
%ae41888 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49941)
store volatile %struct.ScmObj* %ae41888, %struct.ScmObj** %stackaddr$makeclosure49940, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41888, %struct.ScmObj* %_37foldr40146, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41888, %struct.ScmObj* %_37foldr140141, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41888, %struct.ScmObj* %_37map140172, i64 2)
%args49460$ae41886$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49942 = alloca %struct.ScmObj*, align 8
%args49460$ae41886$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41888, %struct.ScmObj* %args49460$ae41886$0)
store volatile %struct.ScmObj* %args49460$ae41886$1, %struct.ScmObj** %stackaddr$prim49942, align 8
%stackaddr$prim49943 = alloca %struct.ScmObj*, align 8
%args49460$ae41886$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41887, %struct.ScmObj* %args49460$ae41886$1)
store volatile %struct.ScmObj* %args49460$ae41886$2, %struct.ScmObj** %stackaddr$prim49943, align 8
%clofunc49944 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41886)
musttail call tailcc void %clofunc49944(%struct.ScmObj* %ae41886, %struct.ScmObj* %args49460$ae41886$2)
ret void
}

define tailcc void @proc_clo$ae41886(%struct.ScmObj* %env$ae41886,%struct.ScmObj* %current_45args49215) {
%stackaddr$env-ref49945 = alloca %struct.ScmObj*, align 8
%_37foldl140125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41886, i64 0)
store %struct.ScmObj* %_37foldl140125, %struct.ScmObj** %stackaddr$env-ref49945
%stackaddr$env-ref49946 = alloca %struct.ScmObj*, align 8
%Ycmb40120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41886, i64 1)
store %struct.ScmObj* %Ycmb40120, %struct.ScmObj** %stackaddr$env-ref49946
%stackaddr$prim49947 = alloca %struct.ScmObj*, align 8
%_95k40428 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49215)
store volatile %struct.ScmObj* %_95k40428, %struct.ScmObj** %stackaddr$prim49947, align 8
%stackaddr$prim49948 = alloca %struct.ScmObj*, align 8
%current_45args49216 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49215)
store volatile %struct.ScmObj* %current_45args49216, %struct.ScmObj** %stackaddr$prim49948, align 8
%stackaddr$prim49949 = alloca %struct.ScmObj*, align 8
%anf_45bind40343 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49216)
store volatile %struct.ScmObj* %anf_45bind40343, %struct.ScmObj** %stackaddr$prim49949, align 8
%stackaddr$makeclosure49950 = alloca %struct.ScmObj*, align 8
%fptrToInt49951 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42278 to i64
%ae42278 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49951)
store volatile %struct.ScmObj* %ae42278, %struct.ScmObj** %stackaddr$makeclosure49950, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42278, %struct.ScmObj* %_37foldl140125, i64 0)
%args49400$Ycmb40120$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49952 = alloca %struct.ScmObj*, align 8
%args49400$Ycmb40120$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40343, %struct.ScmObj* %args49400$Ycmb40120$0)
store volatile %struct.ScmObj* %args49400$Ycmb40120$1, %struct.ScmObj** %stackaddr$prim49952, align 8
%stackaddr$prim49953 = alloca %struct.ScmObj*, align 8
%args49400$Ycmb40120$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42278, %struct.ScmObj* %args49400$Ycmb40120$1)
store volatile %struct.ScmObj* %args49400$Ycmb40120$2, %struct.ScmObj** %stackaddr$prim49953, align 8
%clofunc49954 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40120)
musttail call tailcc void %clofunc49954(%struct.ScmObj* %Ycmb40120, %struct.ScmObj* %args49400$Ycmb40120$2)
ret void
}

define tailcc void @proc_clo$ae42278(%struct.ScmObj* %env$ae42278,%struct.ScmObj* %current_45args49218) {
%stackaddr$env-ref49955 = alloca %struct.ScmObj*, align 8
%_37foldl140125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42278, i64 0)
store %struct.ScmObj* %_37foldl140125, %struct.ScmObj** %stackaddr$env-ref49955
%stackaddr$prim49956 = alloca %struct.ScmObj*, align 8
%_95k40429 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49218)
store volatile %struct.ScmObj* %_95k40429, %struct.ScmObj** %stackaddr$prim49956, align 8
%stackaddr$prim49957 = alloca %struct.ScmObj*, align 8
%current_45args49219 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49218)
store volatile %struct.ScmObj* %current_45args49219, %struct.ScmObj** %stackaddr$prim49957, align 8
%stackaddr$prim49958 = alloca %struct.ScmObj*, align 8
%_37foldl40223 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49219)
store volatile %struct.ScmObj* %_37foldl40223, %struct.ScmObj** %stackaddr$prim49958, align 8
%stackaddr$makeclosure49959 = alloca %struct.ScmObj*, align 8
%fptrToInt49960 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42280 to i64
%ae42280 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49960)
store volatile %struct.ScmObj* %ae42280, %struct.ScmObj** %stackaddr$makeclosure49959, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42280, %struct.ScmObj* %_37foldl140125, i64 0)
%ae42281 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49961 = alloca %struct.ScmObj*, align 8
%fptrToInt49962 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42282 to i64
%ae42282 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49962)
store volatile %struct.ScmObj* %ae42282, %struct.ScmObj** %stackaddr$makeclosure49961, align 8
%args49399$ae42280$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49963 = alloca %struct.ScmObj*, align 8
%args49399$ae42280$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42282, %struct.ScmObj* %args49399$ae42280$0)
store volatile %struct.ScmObj* %args49399$ae42280$1, %struct.ScmObj** %stackaddr$prim49963, align 8
%stackaddr$prim49964 = alloca %struct.ScmObj*, align 8
%args49399$ae42280$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42281, %struct.ScmObj* %args49399$ae42280$1)
store volatile %struct.ScmObj* %args49399$ae42280$2, %struct.ScmObj** %stackaddr$prim49964, align 8
%clofunc49965 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42280)
musttail call tailcc void %clofunc49965(%struct.ScmObj* %ae42280, %struct.ScmObj* %args49399$ae42280$2)
ret void
}

define tailcc void @proc_clo$ae42280(%struct.ScmObj* %env$ae42280,%struct.ScmObj* %current_45args49221) {
%stackaddr$env-ref49966 = alloca %struct.ScmObj*, align 8
%_37foldl140125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42280, i64 0)
store %struct.ScmObj* %_37foldl140125, %struct.ScmObj** %stackaddr$env-ref49966
%stackaddr$prim49967 = alloca %struct.ScmObj*, align 8
%_95k40430 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49221)
store volatile %struct.ScmObj* %_95k40430, %struct.ScmObj** %stackaddr$prim49967, align 8
%stackaddr$prim49968 = alloca %struct.ScmObj*, align 8
%current_45args49222 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49221)
store volatile %struct.ScmObj* %current_45args49222, %struct.ScmObj** %stackaddr$prim49968, align 8
%stackaddr$prim49969 = alloca %struct.ScmObj*, align 8
%_37_6240220 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49222)
store volatile %struct.ScmObj* %_37_6240220, %struct.ScmObj** %stackaddr$prim49969, align 8
%stackaddr$makeclosure49970 = alloca %struct.ScmObj*, align 8
%fptrToInt49971 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42304 to i64
%ae42304 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49971)
store volatile %struct.ScmObj* %ae42304, %struct.ScmObj** %stackaddr$makeclosure49970, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42304, %struct.ScmObj* %_37foldl140125, i64 0)
%ae42305 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49972 = alloca %struct.ScmObj*, align 8
%fptrToInt49973 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42306 to i64
%ae42306 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49973)
store volatile %struct.ScmObj* %ae42306, %struct.ScmObj** %stackaddr$makeclosure49972, align 8
%args49393$ae42304$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49974 = alloca %struct.ScmObj*, align 8
%args49393$ae42304$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42306, %struct.ScmObj* %args49393$ae42304$0)
store volatile %struct.ScmObj* %args49393$ae42304$1, %struct.ScmObj** %stackaddr$prim49974, align 8
%stackaddr$prim49975 = alloca %struct.ScmObj*, align 8
%args49393$ae42304$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42305, %struct.ScmObj* %args49393$ae42304$1)
store volatile %struct.ScmObj* %args49393$ae42304$2, %struct.ScmObj** %stackaddr$prim49975, align 8
%clofunc49976 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42304)
musttail call tailcc void %clofunc49976(%struct.ScmObj* %ae42304, %struct.ScmObj* %args49393$ae42304$2)
ret void
}

define tailcc void @proc_clo$ae42304(%struct.ScmObj* %env$ae42304,%struct.ScmObj* %current_45args49224) {
%stackaddr$env-ref49977 = alloca %struct.ScmObj*, align 8
%_37foldl140125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42304, i64 0)
store %struct.ScmObj* %_37foldl140125, %struct.ScmObj** %stackaddr$env-ref49977
%stackaddr$prim49978 = alloca %struct.ScmObj*, align 8
%_95k40431 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49224)
store volatile %struct.ScmObj* %_95k40431, %struct.ScmObj** %stackaddr$prim49978, align 8
%stackaddr$prim49979 = alloca %struct.ScmObj*, align 8
%current_45args49225 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49224)
store volatile %struct.ScmObj* %current_45args49225, %struct.ScmObj** %stackaddr$prim49979, align 8
%stackaddr$prim49980 = alloca %struct.ScmObj*, align 8
%_37_62_6140217 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49225)
store volatile %struct.ScmObj* %_37_62_6140217, %struct.ScmObj** %stackaddr$prim49980, align 8
%ae42328 = call %struct.ScmObj* @const_init_int(i64 1)
%ae42329 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49981 = alloca %struct.ScmObj*, align 8
%_37append40213 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42328, %struct.ScmObj* %ae42329)
store volatile %struct.ScmObj* %_37append40213, %struct.ScmObj** %stackaddr$prim49981, align 8
%stackaddr$makeclosure49982 = alloca %struct.ScmObj*, align 8
%fptrToInt49983 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42330 to i64
%ae42330 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49983)
store volatile %struct.ScmObj* %ae42330, %struct.ScmObj** %stackaddr$makeclosure49982, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42330, %struct.ScmObj* %_37append40213, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42330, %struct.ScmObj* %_37foldl140125, i64 1)
%ae42331 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49984 = alloca %struct.ScmObj*, align 8
%fptrToInt49985 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42332 to i64
%ae42332 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49985)
store volatile %struct.ScmObj* %ae42332, %struct.ScmObj** %stackaddr$makeclosure49984, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42332, %struct.ScmObj* %_37append40213, i64 0)
%args49387$ae42330$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49986 = alloca %struct.ScmObj*, align 8
%args49387$ae42330$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42332, %struct.ScmObj* %args49387$ae42330$0)
store volatile %struct.ScmObj* %args49387$ae42330$1, %struct.ScmObj** %stackaddr$prim49986, align 8
%stackaddr$prim49987 = alloca %struct.ScmObj*, align 8
%args49387$ae42330$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42331, %struct.ScmObj* %args49387$ae42330$1)
store volatile %struct.ScmObj* %args49387$ae42330$2, %struct.ScmObj** %stackaddr$prim49987, align 8
%clofunc49988 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42330)
musttail call tailcc void %clofunc49988(%struct.ScmObj* %ae42330, %struct.ScmObj* %args49387$ae42330$2)
ret void
}

define tailcc void @proc_clo$ae42330(%struct.ScmObj* %env$ae42330,%struct.ScmObj* %current_45args49227) {
%stackaddr$env-ref49989 = alloca %struct.ScmObj*, align 8
%_37append40213 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42330, i64 0)
store %struct.ScmObj* %_37append40213, %struct.ScmObj** %stackaddr$env-ref49989
%stackaddr$env-ref49990 = alloca %struct.ScmObj*, align 8
%_37foldl140125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42330, i64 1)
store %struct.ScmObj* %_37foldl140125, %struct.ScmObj** %stackaddr$env-ref49990
%stackaddr$prim49991 = alloca %struct.ScmObj*, align 8
%_95k40432 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49227)
store volatile %struct.ScmObj* %_95k40432, %struct.ScmObj** %stackaddr$prim49991, align 8
%stackaddr$prim49992 = alloca %struct.ScmObj*, align 8
%current_45args49228 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49227)
store volatile %struct.ScmObj* %current_45args49228, %struct.ScmObj** %stackaddr$prim49992, align 8
%stackaddr$prim49993 = alloca %struct.ScmObj*, align 8
%anf_45bind40351 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49228)
store volatile %struct.ScmObj* %anf_45bind40351, %struct.ScmObj** %stackaddr$prim49993, align 8
%ae42398 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49994 = alloca %struct.ScmObj*, align 8
%_95040214 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append40213, %struct.ScmObj* %ae42398, %struct.ScmObj* %anf_45bind40351)
store volatile %struct.ScmObj* %_95040214, %struct.ScmObj** %stackaddr$prim49994, align 8
%ae42401 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49995 = alloca %struct.ScmObj*, align 8
%_37append40212 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append40213, %struct.ScmObj* %ae42401)
store volatile %struct.ScmObj* %_37append40212, %struct.ScmObj** %stackaddr$prim49995, align 8
%stackaddr$makeclosure49996 = alloca %struct.ScmObj*, align 8
%fptrToInt49997 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42402 to i64
%ae42402 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49997)
store volatile %struct.ScmObj* %ae42402, %struct.ScmObj** %stackaddr$makeclosure49996, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42402, %struct.ScmObj* %_37foldl140125, i64 0)
%ae42403 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49998 = alloca %struct.ScmObj*, align 8
%fptrToInt49999 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42404 to i64
%ae42404 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49999)
store volatile %struct.ScmObj* %ae42404, %struct.ScmObj** %stackaddr$makeclosure49998, align 8
%args49376$ae42402$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50000 = alloca %struct.ScmObj*, align 8
%args49376$ae42402$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42404, %struct.ScmObj* %args49376$ae42402$0)
store volatile %struct.ScmObj* %args49376$ae42402$1, %struct.ScmObj** %stackaddr$prim50000, align 8
%stackaddr$prim50001 = alloca %struct.ScmObj*, align 8
%args49376$ae42402$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42403, %struct.ScmObj* %args49376$ae42402$1)
store volatile %struct.ScmObj* %args49376$ae42402$2, %struct.ScmObj** %stackaddr$prim50001, align 8
%clofunc50002 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42402)
musttail call tailcc void %clofunc50002(%struct.ScmObj* %ae42402, %struct.ScmObj* %args49376$ae42402$2)
ret void
}

define tailcc void @proc_clo$ae42402(%struct.ScmObj* %env$ae42402,%struct.ScmObj* %current_45args49230) {
%stackaddr$env-ref50003 = alloca %struct.ScmObj*, align 8
%_37foldl140125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42402, i64 0)
store %struct.ScmObj* %_37foldl140125, %struct.ScmObj** %stackaddr$env-ref50003
%stackaddr$prim50004 = alloca %struct.ScmObj*, align 8
%_95k40433 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49230)
store volatile %struct.ScmObj* %_95k40433, %struct.ScmObj** %stackaddr$prim50004, align 8
%stackaddr$prim50005 = alloca %struct.ScmObj*, align 8
%current_45args49231 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49230)
store volatile %struct.ScmObj* %current_45args49231, %struct.ScmObj** %stackaddr$prim50005, align 8
%stackaddr$prim50006 = alloca %struct.ScmObj*, align 8
%_37list_6340205 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49231)
store volatile %struct.ScmObj* %_37list_6340205, %struct.ScmObj** %stackaddr$prim50006, align 8
%stackaddr$makeclosure50007 = alloca %struct.ScmObj*, align 8
%fptrToInt50008 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42818 to i64
%ae42818 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50008)
store volatile %struct.ScmObj* %ae42818, %struct.ScmObj** %stackaddr$makeclosure50007, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42818, %struct.ScmObj* %_37foldl140125, i64 0)
%ae42819 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50009 = alloca %struct.ScmObj*, align 8
%fptrToInt50010 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42820 to i64
%ae42820 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50010)
store volatile %struct.ScmObj* %ae42820, %struct.ScmObj** %stackaddr$makeclosure50009, align 8
%args49351$ae42818$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50011 = alloca %struct.ScmObj*, align 8
%args49351$ae42818$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42820, %struct.ScmObj* %args49351$ae42818$0)
store volatile %struct.ScmObj* %args49351$ae42818$1, %struct.ScmObj** %stackaddr$prim50011, align 8
%stackaddr$prim50012 = alloca %struct.ScmObj*, align 8
%args49351$ae42818$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42819, %struct.ScmObj* %args49351$ae42818$1)
store volatile %struct.ScmObj* %args49351$ae42818$2, %struct.ScmObj** %stackaddr$prim50012, align 8
%clofunc50013 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42818)
musttail call tailcc void %clofunc50013(%struct.ScmObj* %ae42818, %struct.ScmObj* %args49351$ae42818$2)
ret void
}

define tailcc void @proc_clo$ae42818(%struct.ScmObj* %env$ae42818,%struct.ScmObj* %current_45args49233) {
%stackaddr$env-ref50014 = alloca %struct.ScmObj*, align 8
%_37foldl140125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42818, i64 0)
store %struct.ScmObj* %_37foldl140125, %struct.ScmObj** %stackaddr$env-ref50014
%stackaddr$prim50015 = alloca %struct.ScmObj*, align 8
%_95k40434 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49233)
store volatile %struct.ScmObj* %_95k40434, %struct.ScmObj** %stackaddr$prim50015, align 8
%stackaddr$prim50016 = alloca %struct.ScmObj*, align 8
%current_45args49234 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49233)
store volatile %struct.ScmObj* %current_45args49234, %struct.ScmObj** %stackaddr$prim50016, align 8
%stackaddr$prim50017 = alloca %struct.ScmObj*, align 8
%_37drop40196 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49234)
store volatile %struct.ScmObj* %_37drop40196, %struct.ScmObj** %stackaddr$prim50017, align 8
%stackaddr$makeclosure50018 = alloca %struct.ScmObj*, align 8
%fptrToInt50019 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43354 to i64
%ae43354 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50019)
store volatile %struct.ScmObj* %ae43354, %struct.ScmObj** %stackaddr$makeclosure50018, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43354, %struct.ScmObj* %_37foldl140125, i64 0)
%ae43355 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50020 = alloca %struct.ScmObj*, align 8
%fptrToInt50021 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43356 to i64
%ae43356 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50021)
store volatile %struct.ScmObj* %ae43356, %struct.ScmObj** %stackaddr$makeclosure50020, align 8
%args49327$ae43354$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50022 = alloca %struct.ScmObj*, align 8
%args49327$ae43354$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43356, %struct.ScmObj* %args49327$ae43354$0)
store volatile %struct.ScmObj* %args49327$ae43354$1, %struct.ScmObj** %stackaddr$prim50022, align 8
%stackaddr$prim50023 = alloca %struct.ScmObj*, align 8
%args49327$ae43354$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43355, %struct.ScmObj* %args49327$ae43354$1)
store volatile %struct.ScmObj* %args49327$ae43354$2, %struct.ScmObj** %stackaddr$prim50023, align 8
%clofunc50024 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43354)
musttail call tailcc void %clofunc50024(%struct.ScmObj* %ae43354, %struct.ScmObj* %args49327$ae43354$2)
ret void
}

define tailcc void @proc_clo$ae43354(%struct.ScmObj* %env$ae43354,%struct.ScmObj* %current_45args49236) {
%stackaddr$env-ref50025 = alloca %struct.ScmObj*, align 8
%_37foldl140125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43354, i64 0)
store %struct.ScmObj* %_37foldl140125, %struct.ScmObj** %stackaddr$env-ref50025
%stackaddr$prim50026 = alloca %struct.ScmObj*, align 8
%_95k40435 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49236)
store volatile %struct.ScmObj* %_95k40435, %struct.ScmObj** %stackaddr$prim50026, align 8
%stackaddr$prim50027 = alloca %struct.ScmObj*, align 8
%current_45args49237 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49236)
store volatile %struct.ScmObj* %current_45args49237, %struct.ScmObj** %stackaddr$prim50027, align 8
%stackaddr$prim50028 = alloca %struct.ScmObj*, align 8
%_37memv40189 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49237)
store volatile %struct.ScmObj* %_37memv40189, %struct.ScmObj** %stackaddr$prim50028, align 8
%stackaddr$makeclosure50029 = alloca %struct.ScmObj*, align 8
%fptrToInt50030 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43758 to i64
%ae43758 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50030)
store volatile %struct.ScmObj* %ae43758, %struct.ScmObj** %stackaddr$makeclosure50029, align 8
%ae43759 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50031 = alloca %struct.ScmObj*, align 8
%fptrToInt50032 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43760 to i64
%ae43760 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50032)
store volatile %struct.ScmObj* %ae43760, %struct.ScmObj** %stackaddr$makeclosure50031, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43760, %struct.ScmObj* %_37foldl140125, i64 0)
%args49301$ae43758$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50033 = alloca %struct.ScmObj*, align 8
%args49301$ae43758$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43760, %struct.ScmObj* %args49301$ae43758$0)
store volatile %struct.ScmObj* %args49301$ae43758$1, %struct.ScmObj** %stackaddr$prim50033, align 8
%stackaddr$prim50034 = alloca %struct.ScmObj*, align 8
%args49301$ae43758$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43759, %struct.ScmObj* %args49301$ae43758$1)
store volatile %struct.ScmObj* %args49301$ae43758$2, %struct.ScmObj** %stackaddr$prim50034, align 8
%clofunc50035 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43758)
musttail call tailcc void %clofunc50035(%struct.ScmObj* %ae43758, %struct.ScmObj* %args49301$ae43758$2)
ret void
}

define tailcc void @proc_clo$ae43758(%struct.ScmObj* %env$ae43758,%struct.ScmObj* %current_45args49239) {
%stackaddr$prim50036 = alloca %struct.ScmObj*, align 8
%_95k40436 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49239)
store volatile %struct.ScmObj* %_95k40436, %struct.ScmObj** %stackaddr$prim50036, align 8
%stackaddr$prim50037 = alloca %struct.ScmObj*, align 8
%current_45args49240 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49239)
store volatile %struct.ScmObj* %current_45args49240, %struct.ScmObj** %stackaddr$prim50037, align 8
%stackaddr$prim50038 = alloca %struct.ScmObj*, align 8
%_37_4740185 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49240)
store volatile %struct.ScmObj* %_37_4740185, %struct.ScmObj** %stackaddr$prim50038, align 8
%stackaddr$makeclosure50039 = alloca %struct.ScmObj*, align 8
%fptrToInt50040 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43856 to i64
%ae43856 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50040)
store volatile %struct.ScmObj* %ae43856, %struct.ScmObj** %stackaddr$makeclosure50039, align 8
%ae43857 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50041 = alloca %struct.ScmObj*, align 8
%fptrToInt50042 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43858 to i64
%ae43858 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50042)
store volatile %struct.ScmObj* %ae43858, %struct.ScmObj** %stackaddr$makeclosure50041, align 8
%args49288$ae43856$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50043 = alloca %struct.ScmObj*, align 8
%args49288$ae43856$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43858, %struct.ScmObj* %args49288$ae43856$0)
store volatile %struct.ScmObj* %args49288$ae43856$1, %struct.ScmObj** %stackaddr$prim50043, align 8
%stackaddr$prim50044 = alloca %struct.ScmObj*, align 8
%args49288$ae43856$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43857, %struct.ScmObj* %args49288$ae43856$1)
store volatile %struct.ScmObj* %args49288$ae43856$2, %struct.ScmObj** %stackaddr$prim50044, align 8
%clofunc50045 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43856)
musttail call tailcc void %clofunc50045(%struct.ScmObj* %ae43856, %struct.ScmObj* %args49288$ae43856$2)
ret void
}

define tailcc void @proc_clo$ae43856(%struct.ScmObj* %env$ae43856,%struct.ScmObj* %current_45args49242) {
%stackaddr$prim50046 = alloca %struct.ScmObj*, align 8
%_95k40437 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49242)
store volatile %struct.ScmObj* %_95k40437, %struct.ScmObj** %stackaddr$prim50046, align 8
%stackaddr$prim50047 = alloca %struct.ScmObj*, align 8
%current_45args49243 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49242)
store volatile %struct.ScmObj* %current_45args49243, %struct.ScmObj** %stackaddr$prim50047, align 8
%stackaddr$prim50048 = alloca %struct.ScmObj*, align 8
%_37first40183 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49243)
store volatile %struct.ScmObj* %_37first40183, %struct.ScmObj** %stackaddr$prim50048, align 8
%stackaddr$makeclosure50049 = alloca %struct.ScmObj*, align 8
%fptrToInt50050 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43876 to i64
%ae43876 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50050)
store volatile %struct.ScmObj* %ae43876, %struct.ScmObj** %stackaddr$makeclosure50049, align 8
%ae43877 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50051 = alloca %struct.ScmObj*, align 8
%fptrToInt50052 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43878 to i64
%ae43878 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50052)
store volatile %struct.ScmObj* %ae43878, %struct.ScmObj** %stackaddr$makeclosure50051, align 8
%args49283$ae43876$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50053 = alloca %struct.ScmObj*, align 8
%args49283$ae43876$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43878, %struct.ScmObj* %args49283$ae43876$0)
store volatile %struct.ScmObj* %args49283$ae43876$1, %struct.ScmObj** %stackaddr$prim50053, align 8
%stackaddr$prim50054 = alloca %struct.ScmObj*, align 8
%args49283$ae43876$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43877, %struct.ScmObj* %args49283$ae43876$1)
store volatile %struct.ScmObj* %args49283$ae43876$2, %struct.ScmObj** %stackaddr$prim50054, align 8
%clofunc50055 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43876)
musttail call tailcc void %clofunc50055(%struct.ScmObj* %ae43876, %struct.ScmObj* %args49283$ae43876$2)
ret void
}

define tailcc void @proc_clo$ae43876(%struct.ScmObj* %env$ae43876,%struct.ScmObj* %current_45args49245) {
%stackaddr$prim50056 = alloca %struct.ScmObj*, align 8
%_95k40438 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49245)
store volatile %struct.ScmObj* %_95k40438, %struct.ScmObj** %stackaddr$prim50056, align 8
%stackaddr$prim50057 = alloca %struct.ScmObj*, align 8
%current_45args49246 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49245)
store volatile %struct.ScmObj* %current_45args49246, %struct.ScmObj** %stackaddr$prim50057, align 8
%stackaddr$prim50058 = alloca %struct.ScmObj*, align 8
%_37second40181 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49246)
store volatile %struct.ScmObj* %_37second40181, %struct.ScmObj** %stackaddr$prim50058, align 8
%stackaddr$makeclosure50059 = alloca %struct.ScmObj*, align 8
%fptrToInt50060 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43898 to i64
%ae43898 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50060)
store volatile %struct.ScmObj* %ae43898, %struct.ScmObj** %stackaddr$makeclosure50059, align 8
%ae43899 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50061 = alloca %struct.ScmObj*, align 8
%fptrToInt50062 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43900 to i64
%ae43900 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50062)
store volatile %struct.ScmObj* %ae43900, %struct.ScmObj** %stackaddr$makeclosure50061, align 8
%args49278$ae43898$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50063 = alloca %struct.ScmObj*, align 8
%args49278$ae43898$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43900, %struct.ScmObj* %args49278$ae43898$0)
store volatile %struct.ScmObj* %args49278$ae43898$1, %struct.ScmObj** %stackaddr$prim50063, align 8
%stackaddr$prim50064 = alloca %struct.ScmObj*, align 8
%args49278$ae43898$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43899, %struct.ScmObj* %args49278$ae43898$1)
store volatile %struct.ScmObj* %args49278$ae43898$2, %struct.ScmObj** %stackaddr$prim50064, align 8
%clofunc50065 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43898)
musttail call tailcc void %clofunc50065(%struct.ScmObj* %ae43898, %struct.ScmObj* %args49278$ae43898$2)
ret void
}

define tailcc void @proc_clo$ae43898(%struct.ScmObj* %env$ae43898,%struct.ScmObj* %current_45args49248) {
%stackaddr$prim50066 = alloca %struct.ScmObj*, align 8
%_95k40439 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49248)
store volatile %struct.ScmObj* %_95k40439, %struct.ScmObj** %stackaddr$prim50066, align 8
%stackaddr$prim50067 = alloca %struct.ScmObj*, align 8
%current_45args49249 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49248)
store volatile %struct.ScmObj* %current_45args49249, %struct.ScmObj** %stackaddr$prim50067, align 8
%stackaddr$prim50068 = alloca %struct.ScmObj*, align 8
%_37third40179 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49249)
store volatile %struct.ScmObj* %_37third40179, %struct.ScmObj** %stackaddr$prim50068, align 8
%stackaddr$makeclosure50069 = alloca %struct.ScmObj*, align 8
%fptrToInt50070 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43922 to i64
%ae43922 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50070)
store volatile %struct.ScmObj* %ae43922, %struct.ScmObj** %stackaddr$makeclosure50069, align 8
%ae43923 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50071 = alloca %struct.ScmObj*, align 8
%fptrToInt50072 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43924 to i64
%ae43924 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50072)
store volatile %struct.ScmObj* %ae43924, %struct.ScmObj** %stackaddr$makeclosure50071, align 8
%args49273$ae43922$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50073 = alloca %struct.ScmObj*, align 8
%args49273$ae43922$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43924, %struct.ScmObj* %args49273$ae43922$0)
store volatile %struct.ScmObj* %args49273$ae43922$1, %struct.ScmObj** %stackaddr$prim50073, align 8
%stackaddr$prim50074 = alloca %struct.ScmObj*, align 8
%args49273$ae43922$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43923, %struct.ScmObj* %args49273$ae43922$1)
store volatile %struct.ScmObj* %args49273$ae43922$2, %struct.ScmObj** %stackaddr$prim50074, align 8
%clofunc50075 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43922)
musttail call tailcc void %clofunc50075(%struct.ScmObj* %ae43922, %struct.ScmObj* %args49273$ae43922$2)
ret void
}

define tailcc void @proc_clo$ae43922(%struct.ScmObj* %env$ae43922,%struct.ScmObj* %current_45args49251) {
%stackaddr$prim50076 = alloca %struct.ScmObj*, align 8
%_95k40440 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49251)
store volatile %struct.ScmObj* %_95k40440, %struct.ScmObj** %stackaddr$prim50076, align 8
%stackaddr$prim50077 = alloca %struct.ScmObj*, align 8
%current_45args49252 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49251)
store volatile %struct.ScmObj* %current_45args49252, %struct.ScmObj** %stackaddr$prim50077, align 8
%stackaddr$prim50078 = alloca %struct.ScmObj*, align 8
%_37fourth40177 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49252)
store volatile %struct.ScmObj* %_37fourth40177, %struct.ScmObj** %stackaddr$prim50078, align 8
%stackaddr$makeclosure50079 = alloca %struct.ScmObj*, align 8
%fptrToInt50080 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43948 to i64
%ae43948 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50080)
store volatile %struct.ScmObj* %ae43948, %struct.ScmObj** %stackaddr$makeclosure50079, align 8
%ae43949 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50081 = alloca %struct.ScmObj*, align 8
%fptrToInt50082 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43950 to i64
%ae43950 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50082)
store volatile %struct.ScmObj* %ae43950, %struct.ScmObj** %stackaddr$makeclosure50081, align 8
%args49268$ae43948$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50083 = alloca %struct.ScmObj*, align 8
%args49268$ae43948$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43950, %struct.ScmObj* %args49268$ae43948$0)
store volatile %struct.ScmObj* %args49268$ae43948$1, %struct.ScmObj** %stackaddr$prim50083, align 8
%stackaddr$prim50084 = alloca %struct.ScmObj*, align 8
%args49268$ae43948$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43949, %struct.ScmObj* %args49268$ae43948$1)
store volatile %struct.ScmObj* %args49268$ae43948$2, %struct.ScmObj** %stackaddr$prim50084, align 8
%clofunc50085 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43948)
musttail call tailcc void %clofunc50085(%struct.ScmObj* %ae43948, %struct.ScmObj* %args49268$ae43948$2)
ret void
}

define tailcc void @proc_clo$ae43948(%struct.ScmObj* %env$ae43948,%struct.ScmObj* %current_45args49254) {
%stackaddr$prim50086 = alloca %struct.ScmObj*, align 8
%_95k40441 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49254)
store volatile %struct.ScmObj* %_95k40441, %struct.ScmObj** %stackaddr$prim50086, align 8
%stackaddr$prim50087 = alloca %struct.ScmObj*, align 8
%current_45args49255 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49254)
store volatile %struct.ScmObj* %current_45args49255, %struct.ScmObj** %stackaddr$prim50087, align 8
%stackaddr$prim50088 = alloca %struct.ScmObj*, align 8
%promise_6340238 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49255)
store volatile %struct.ScmObj* %promise_6340238, %struct.ScmObj** %stackaddr$prim50088, align 8
%stackaddr$prim50089 = alloca %struct.ScmObj*, align 8
%anf_45bind40391 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind40391, %struct.ScmObj** %stackaddr$prim50089, align 8
%ae44035 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim50090 = alloca %struct.ScmObj*, align 8
%g4140257 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae44035, %struct.ScmObj* %anf_45bind40391)
store volatile %struct.ScmObj* %g4140257, %struct.ScmObj** %stackaddr$prim50090, align 8
%stackaddr$prim50091 = alloca %struct.ScmObj*, align 8
%anf_45bind40392 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind40392, %struct.ScmObj** %stackaddr$prim50091, align 8
%ae44037 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim50092 = alloca %struct.ScmObj*, align 8
%g4240256 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae44037, %struct.ScmObj* %anf_45bind40392)
store volatile %struct.ScmObj* %g4240256, %struct.ScmObj** %stackaddr$prim50092, align 8
%stackaddr$prim50093 = alloca %struct.ScmObj*, align 8
%anf_45bind40393 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind40393, %struct.ScmObj** %stackaddr$prim50093, align 8
%ae44039 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim50094 = alloca %struct.ScmObj*, align 8
%g4340255 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae44039, %struct.ScmObj* %anf_45bind40393)
store volatile %struct.ScmObj* %g4340255, %struct.ScmObj** %stackaddr$prim50094, align 8
%stackaddr$prim50095 = alloca %struct.ScmObj*, align 8
%anf_45bind40394 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind40394, %struct.ScmObj** %stackaddr$prim50095, align 8
%ae44041 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim50096 = alloca %struct.ScmObj*, align 8
%g4440254 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae44041, %struct.ScmObj* %anf_45bind40394)
store volatile %struct.ScmObj* %g4440254, %struct.ScmObj** %stackaddr$prim50096, align 8
%stackaddr$prim50097 = alloca %struct.ScmObj*, align 8
%anf_45bind40395 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind40395, %struct.ScmObj** %stackaddr$prim50097, align 8
%ae44043 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim50098 = alloca %struct.ScmObj*, align 8
%g4540253 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae44043, %struct.ScmObj* %anf_45bind40395)
store volatile %struct.ScmObj* %g4540253, %struct.ScmObj** %stackaddr$prim50098, align 8
%stackaddr$prim50099 = alloca %struct.ScmObj*, align 8
%anf_45bind40396 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind40396, %struct.ScmObj** %stackaddr$prim50099, align 8
%ae44045 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim50100 = alloca %struct.ScmObj*, align 8
%g4640252 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae44045, %struct.ScmObj* %anf_45bind40396)
store volatile %struct.ScmObj* %g4640252, %struct.ScmObj** %stackaddr$prim50100, align 8
%stackaddr$prim50101 = alloca %struct.ScmObj*, align 8
%anf_45bind40397 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind40397, %struct.ScmObj** %stackaddr$prim50101, align 8
%ae44047 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim50102 = alloca %struct.ScmObj*, align 8
%g4740251 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae44047, %struct.ScmObj* %anf_45bind40397)
store volatile %struct.ScmObj* %g4740251, %struct.ScmObj** %stackaddr$prim50102, align 8
%stackaddr$prim50103 = alloca %struct.ScmObj*, align 8
%anf_45bind40398 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind40398, %struct.ScmObj** %stackaddr$prim50103, align 8
%ae44049 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim50104 = alloca %struct.ScmObj*, align 8
%g4840250 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae44049, %struct.ScmObj* %anf_45bind40398)
store volatile %struct.ScmObj* %g4840250, %struct.ScmObj** %stackaddr$prim50104, align 8
%stackaddr$prim50105 = alloca %struct.ScmObj*, align 8
%anf_45bind40399 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind40399, %struct.ScmObj** %stackaddr$prim50105, align 8
%ae44051 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim50106 = alloca %struct.ScmObj*, align 8
%g4940249 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae44051, %struct.ScmObj* %anf_45bind40399)
store volatile %struct.ScmObj* %g4940249, %struct.ScmObj** %stackaddr$prim50106, align 8
%stackaddr$prim50107 = alloca %struct.ScmObj*, align 8
%anf_45bind40400 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind40400, %struct.ScmObj** %stackaddr$prim50107, align 8
%ae44053 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim50108 = alloca %struct.ScmObj*, align 8
%g5040248 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae44053, %struct.ScmObj* %anf_45bind40400)
store volatile %struct.ScmObj* %g5040248, %struct.ScmObj** %stackaddr$prim50108, align 8
%stackaddr$prim50109 = alloca %struct.ScmObj*, align 8
%anf_45bind40401 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind40401, %struct.ScmObj** %stackaddr$prim50109, align 8
%ae44055 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim50110 = alloca %struct.ScmObj*, align 8
%g5140247 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae44055, %struct.ScmObj* %anf_45bind40401)
store volatile %struct.ScmObj* %g5140247, %struct.ScmObj** %stackaddr$prim50110, align 8
%stackaddr$prim50111 = alloca %struct.ScmObj*, align 8
%anf_45bind40402 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind40402, %struct.ScmObj** %stackaddr$prim50111, align 8
%ae44057 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim50112 = alloca %struct.ScmObj*, align 8
%g5240246 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae44057, %struct.ScmObj* %anf_45bind40402)
store volatile %struct.ScmObj* %g5240246, %struct.ScmObj** %stackaddr$prim50112, align 8
%stackaddr$prim50113 = alloca %struct.ScmObj*, align 8
%anf_45bind40403 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind40403, %struct.ScmObj** %stackaddr$prim50113, align 8
%ae44059 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim50114 = alloca %struct.ScmObj*, align 8
%g5340245 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae44059, %struct.ScmObj* %anf_45bind40403)
store volatile %struct.ScmObj* %g5340245, %struct.ScmObj** %stackaddr$prim50114, align 8
%stackaddr$prim50115 = alloca %struct.ScmObj*, align 8
%anf_45bind40404 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind40404, %struct.ScmObj** %stackaddr$prim50115, align 8
%ae44061 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim50116 = alloca %struct.ScmObj*, align 8
%g5440244 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae44061, %struct.ScmObj* %anf_45bind40404)
store volatile %struct.ScmObj* %g5440244, %struct.ScmObj** %stackaddr$prim50116, align 8
%stackaddr$prim50117 = alloca %struct.ScmObj*, align 8
%anf_45bind40405 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind40405, %struct.ScmObj** %stackaddr$prim50117, align 8
%ae44063 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim50118 = alloca %struct.ScmObj*, align 8
%g5540243 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae44063, %struct.ScmObj* %anf_45bind40405)
store volatile %struct.ScmObj* %g5540243, %struct.ScmObj** %stackaddr$prim50118, align 8
%stackaddr$prim50119 = alloca %struct.ScmObj*, align 8
%anf_45bind40406 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind40406, %struct.ScmObj** %stackaddr$prim50119, align 8
%ae44065 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim50120 = alloca %struct.ScmObj*, align 8
%g5640242 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae44065, %struct.ScmObj* %anf_45bind40406)
store volatile %struct.ScmObj* %g5640242, %struct.ScmObj** %stackaddr$prim50120, align 8
%stackaddr$prim50121 = alloca %struct.ScmObj*, align 8
%anf_45bind40407 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind40407, %struct.ScmObj** %stackaddr$prim50121, align 8
%ae44067 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim50122 = alloca %struct.ScmObj*, align 8
%g5740241 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae44067, %struct.ScmObj* %anf_45bind40407)
store volatile %struct.ScmObj* %g5740241, %struct.ScmObj** %stackaddr$prim50122, align 8
%stackaddr$prim50123 = alloca %struct.ScmObj*, align 8
%anf_45bind40408 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind40408, %struct.ScmObj** %stackaddr$prim50123, align 8
%ae44069 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim50124 = alloca %struct.ScmObj*, align 8
%g5840240 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae44069, %struct.ScmObj* %anf_45bind40408)
store volatile %struct.ScmObj* %g5840240, %struct.ScmObj** %stackaddr$prim50124, align 8
%ae44072 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44073 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae4407350125, i32 0, i32 0))
%stackaddr$prim50126 = alloca %struct.ScmObj*, align 8
%t4011940275 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g4140257, %struct.ScmObj* %ae44072, %struct.ScmObj* %ae44073)
store volatile %struct.ScmObj* %t4011940275, %struct.ScmObj** %stackaddr$prim50126, align 8
%ae44075 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44076 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae4407650127, i32 0, i32 0))
%stackaddr$prim50128 = alloca %struct.ScmObj*, align 8
%t4011840274 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g4240256, %struct.ScmObj* %ae44075, %struct.ScmObj* %ae44076)
store volatile %struct.ScmObj* %t4011840274, %struct.ScmObj** %stackaddr$prim50128, align 8
%ae44078 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44079 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae4407950129, i32 0, i32 0))
%stackaddr$prim50130 = alloca %struct.ScmObj*, align 8
%t4011740273 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g4340255, %struct.ScmObj* %ae44078, %struct.ScmObj* %ae44079)
store volatile %struct.ScmObj* %t4011740273, %struct.ScmObj** %stackaddr$prim50130, align 8
%ae44081 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44082 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae4408250131, i32 0, i32 0))
%stackaddr$prim50132 = alloca %struct.ScmObj*, align 8
%t4011640272 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g4440254, %struct.ScmObj* %ae44081, %struct.ScmObj* %ae44082)
store volatile %struct.ScmObj* %t4011640272, %struct.ScmObj** %stackaddr$prim50132, align 8
%ae44084 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44085 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae4408550133, i32 0, i32 0))
%stackaddr$prim50134 = alloca %struct.ScmObj*, align 8
%t4011540271 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g4540253, %struct.ScmObj* %ae44084, %struct.ScmObj* %ae44085)
store volatile %struct.ScmObj* %t4011540271, %struct.ScmObj** %stackaddr$prim50134, align 8
%ae44087 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44088 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae4408850135, i32 0, i32 0))
%stackaddr$prim50136 = alloca %struct.ScmObj*, align 8
%t4011440270 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g4640252, %struct.ScmObj* %ae44087, %struct.ScmObj* %ae44088)
store volatile %struct.ScmObj* %t4011440270, %struct.ScmObj** %stackaddr$prim50136, align 8
%ae44090 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44091 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae4409150137, i32 0, i32 0))
%stackaddr$prim50138 = alloca %struct.ScmObj*, align 8
%t4011340269 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g4740251, %struct.ScmObj* %ae44090, %struct.ScmObj* %ae44091)
store volatile %struct.ScmObj* %t4011340269, %struct.ScmObj** %stackaddr$prim50138, align 8
%ae44093 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44094 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae4409450139, i32 0, i32 0))
%stackaddr$prim50140 = alloca %struct.ScmObj*, align 8
%t4011240268 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g4840250, %struct.ScmObj* %ae44093, %struct.ScmObj* %ae44094)
store volatile %struct.ScmObj* %t4011240268, %struct.ScmObj** %stackaddr$prim50140, align 8
%ae44096 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44097 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae4409750141, i32 0, i32 0))
%stackaddr$prim50142 = alloca %struct.ScmObj*, align 8
%t4011140267 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g4940249, %struct.ScmObj* %ae44096, %struct.ScmObj* %ae44097)
store volatile %struct.ScmObj* %t4011140267, %struct.ScmObj** %stackaddr$prim50142, align 8
%ae44099 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44100 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae4410050143, i32 0, i32 0))
%stackaddr$prim50144 = alloca %struct.ScmObj*, align 8
%t4011040266 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g5040248, %struct.ScmObj* %ae44099, %struct.ScmObj* %ae44100)
store volatile %struct.ScmObj* %t4011040266, %struct.ScmObj** %stackaddr$prim50144, align 8
%ae44102 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44103 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae4410350145, i32 0, i32 0))
%stackaddr$prim50146 = alloca %struct.ScmObj*, align 8
%t4010940265 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g5140247, %struct.ScmObj* %ae44102, %struct.ScmObj* %ae44103)
store volatile %struct.ScmObj* %t4010940265, %struct.ScmObj** %stackaddr$prim50146, align 8
%ae44105 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44106 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae4410650147, i32 0, i32 0))
%stackaddr$prim50148 = alloca %struct.ScmObj*, align 8
%t4010840264 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g5240246, %struct.ScmObj* %ae44105, %struct.ScmObj* %ae44106)
store volatile %struct.ScmObj* %t4010840264, %struct.ScmObj** %stackaddr$prim50148, align 8
%ae44108 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44109 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae4410950149, i32 0, i32 0))
%stackaddr$prim50150 = alloca %struct.ScmObj*, align 8
%t4010740263 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g5340245, %struct.ScmObj* %ae44108, %struct.ScmObj* %ae44109)
store volatile %struct.ScmObj* %t4010740263, %struct.ScmObj** %stackaddr$prim50150, align 8
%ae44111 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44112 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae4411250151, i32 0, i32 0))
%stackaddr$prim50152 = alloca %struct.ScmObj*, align 8
%t4010640262 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g5440244, %struct.ScmObj* %ae44111, %struct.ScmObj* %ae44112)
store volatile %struct.ScmObj* %t4010640262, %struct.ScmObj** %stackaddr$prim50152, align 8
%ae44114 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44115 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae4411550153, i32 0, i32 0))
%stackaddr$prim50154 = alloca %struct.ScmObj*, align 8
%t4010540261 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g5540243, %struct.ScmObj* %ae44114, %struct.ScmObj* %ae44115)
store volatile %struct.ScmObj* %t4010540261, %struct.ScmObj** %stackaddr$prim50154, align 8
%ae44117 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44118 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae4411850155, i32 0, i32 0))
%stackaddr$prim50156 = alloca %struct.ScmObj*, align 8
%t4010440260 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g5640242, %struct.ScmObj* %ae44117, %struct.ScmObj* %ae44118)
store volatile %struct.ScmObj* %t4010440260, %struct.ScmObj** %stackaddr$prim50156, align 8
%ae44120 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44121 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae4412150157, i32 0, i32 0))
%stackaddr$prim50158 = alloca %struct.ScmObj*, align 8
%t4010340259 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g5740241, %struct.ScmObj* %ae44120, %struct.ScmObj* %ae44121)
store volatile %struct.ScmObj* %t4010340259, %struct.ScmObj** %stackaddr$prim50158, align 8
%ae44123 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44124 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae4412450159, i32 0, i32 0))
%stackaddr$prim50160 = alloca %struct.ScmObj*, align 8
%t4010240258 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g5840240, %struct.ScmObj* %ae44123, %struct.ScmObj* %ae44124)
store volatile %struct.ScmObj* %t4010240258, %struct.ScmObj** %stackaddr$prim50160, align 8
%stackaddr$makeclosure50161 = alloca %struct.ScmObj*, align 8
%fptrToInt50162 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44125 to i64
%ae44125 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50162)
store volatile %struct.ScmObj* %ae44125, %struct.ScmObj** %stackaddr$makeclosure50161, align 8
%ae44126 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44127 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @global$sym$ae4412750163, i32 0, i32 0))
%args49261$ae44125$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50164 = alloca %struct.ScmObj*, align 8
%args49261$ae44125$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44127, %struct.ScmObj* %args49261$ae44125$0)
store volatile %struct.ScmObj* %args49261$ae44125$1, %struct.ScmObj** %stackaddr$prim50164, align 8
%stackaddr$prim50165 = alloca %struct.ScmObj*, align 8
%args49261$ae44125$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44126, %struct.ScmObj* %args49261$ae44125$1)
store volatile %struct.ScmObj* %args49261$ae44125$2, %struct.ScmObj** %stackaddr$prim50165, align 8
%clofunc50166 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44125)
musttail call tailcc void %clofunc50166(%struct.ScmObj* %ae44125, %struct.ScmObj* %args49261$ae44125$2)
ret void
}

define tailcc void @proc_clo$ae44125(%struct.ScmObj* %env$ae44125,%struct.ScmObj* %current_45args49257) {
%stackaddr$prim50167 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49257)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim50167, align 8
%stackaddr$prim50168 = alloca %struct.ScmObj*, align 8
%current_45args49258 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49257)
store volatile %struct.ScmObj* %current_45args49258, %struct.ScmObj** %stackaddr$prim50168, align 8
%stackaddr$prim50169 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49258)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim50169, align 8
%stackaddr$prim50170 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim50170, align 8
%args49260$k$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50171 = alloca %struct.ScmObj*, align 8
%args49260$k$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %args49260$k$0)
store volatile %struct.ScmObj* %args49260$k$1, %struct.ScmObj** %stackaddr$prim50171, align 8
%clofunc50172 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc50172(%struct.ScmObj* %k, %struct.ScmObj* %args49260$k$1)
ret void
}

define tailcc void @proc_clo$ae43950(%struct.ScmObj* %env$ae43950,%struct.ScmObj* %current_45args49262) {
%stackaddr$prim50173 = alloca %struct.ScmObj*, align 8
%k40442 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49262)
store volatile %struct.ScmObj* %k40442, %struct.ScmObj** %stackaddr$prim50173, align 8
%stackaddr$prim50174 = alloca %struct.ScmObj*, align 8
%current_45args49263 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49262)
store volatile %struct.ScmObj* %current_45args49263, %struct.ScmObj** %stackaddr$prim50174, align 8
%stackaddr$prim50175 = alloca %struct.ScmObj*, align 8
%thunk40239 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49263)
store volatile %struct.ScmObj* %thunk40239, %struct.ScmObj** %stackaddr$prim50175, align 8
%stackaddr$prim50176 = alloca %struct.ScmObj*, align 8
%anf_45bind40387 = call %struct.ScmObj* @prim_vector_63(%struct.ScmObj* %thunk40239)
store volatile %struct.ScmObj* %anf_45bind40387, %struct.ScmObj** %stackaddr$prim50176, align 8
%truthy$cmp50177 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40387)
%cmp$cmp50177 = icmp eq i64 %truthy$cmp50177, 1
br i1 %cmp$cmp50177, label %truebranch$cmp50177, label %falsebranch$cmp50177
truebranch$cmp50177:
%stackaddr$prim50178 = alloca %struct.ScmObj*, align 8
%anf_45bind40388 = call %struct.ScmObj* @prim_vector_45length(%struct.ScmObj* %thunk40239)
store volatile %struct.ScmObj* %anf_45bind40388, %struct.ScmObj** %stackaddr$prim50178, align 8
%ae43955 = call %struct.ScmObj* @const_init_int(i64 3)
%stackaddr$prim50179 = alloca %struct.ScmObj*, align 8
%anf_45bind40389 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind40388, %struct.ScmObj* %ae43955)
store volatile %struct.ScmObj* %anf_45bind40389, %struct.ScmObj** %stackaddr$prim50179, align 8
%truthy$cmp50180 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40389)
%cmp$cmp50180 = icmp eq i64 %truthy$cmp50180, 1
br i1 %cmp$cmp50180, label %truebranch$cmp50180, label %falsebranch$cmp50180
truebranch$cmp50180:
%ae43958 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50181 = alloca %struct.ScmObj*, align 8
%anf_45bind40390 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk40239, %struct.ScmObj* %ae43958)
store volatile %struct.ScmObj* %anf_45bind40390, %struct.ScmObj** %stackaddr$prim50181, align 8
%ae43960 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @global$sym$ae4396050182, i32 0, i32 0))
%stackaddr$prim50183 = alloca %struct.ScmObj*, align 8
%cpsprim40443 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind40390, %struct.ScmObj* %ae43960)
store volatile %struct.ScmObj* %cpsprim40443, %struct.ScmObj** %stackaddr$prim50183, align 8
%ae43962 = call %struct.ScmObj* @const_init_int(i64 0)
%args49265$k40442$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50184 = alloca %struct.ScmObj*, align 8
%args49265$k40442$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40443, %struct.ScmObj* %args49265$k40442$0)
store volatile %struct.ScmObj* %args49265$k40442$1, %struct.ScmObj** %stackaddr$prim50184, align 8
%stackaddr$prim50185 = alloca %struct.ScmObj*, align 8
%args49265$k40442$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43962, %struct.ScmObj* %args49265$k40442$1)
store volatile %struct.ScmObj* %args49265$k40442$2, %struct.ScmObj** %stackaddr$prim50185, align 8
%clofunc50186 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40442)
musttail call tailcc void %clofunc50186(%struct.ScmObj* %k40442, %struct.ScmObj* %args49265$k40442$2)
ret void
falsebranch$cmp50180:
%ae43980 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43981 = call %struct.ScmObj* @const_init_false()
%args49266$k40442$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50187 = alloca %struct.ScmObj*, align 8
%args49266$k40442$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43981, %struct.ScmObj* %args49266$k40442$0)
store volatile %struct.ScmObj* %args49266$k40442$1, %struct.ScmObj** %stackaddr$prim50187, align 8
%stackaddr$prim50188 = alloca %struct.ScmObj*, align 8
%args49266$k40442$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43980, %struct.ScmObj* %args49266$k40442$1)
store volatile %struct.ScmObj* %args49266$k40442$2, %struct.ScmObj** %stackaddr$prim50188, align 8
%clofunc50189 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40442)
musttail call tailcc void %clofunc50189(%struct.ScmObj* %k40442, %struct.ScmObj* %args49266$k40442$2)
ret void
falsebranch$cmp50177:
%ae44002 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44003 = call %struct.ScmObj* @const_init_false()
%args49267$k40442$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50190 = alloca %struct.ScmObj*, align 8
%args49267$k40442$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44003, %struct.ScmObj* %args49267$k40442$0)
store volatile %struct.ScmObj* %args49267$k40442$1, %struct.ScmObj** %stackaddr$prim50190, align 8
%stackaddr$prim50191 = alloca %struct.ScmObj*, align 8
%args49267$k40442$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44002, %struct.ScmObj* %args49267$k40442$1)
store volatile %struct.ScmObj* %args49267$k40442$2, %struct.ScmObj** %stackaddr$prim50191, align 8
%clofunc50192 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40442)
musttail call tailcc void %clofunc50192(%struct.ScmObj* %k40442, %struct.ScmObj* %args49267$k40442$2)
ret void
}

define tailcc void @proc_clo$ae43924(%struct.ScmObj* %env$ae43924,%struct.ScmObj* %current_45args49269) {
%stackaddr$prim50193 = alloca %struct.ScmObj*, align 8
%k40444 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49269)
store volatile %struct.ScmObj* %k40444, %struct.ScmObj** %stackaddr$prim50193, align 8
%stackaddr$prim50194 = alloca %struct.ScmObj*, align 8
%current_45args49270 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49269)
store volatile %struct.ScmObj* %current_45args49270, %struct.ScmObj** %stackaddr$prim50194, align 8
%stackaddr$prim50195 = alloca %struct.ScmObj*, align 8
%x40178 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49270)
store volatile %struct.ScmObj* %x40178, %struct.ScmObj** %stackaddr$prim50195, align 8
%stackaddr$prim50196 = alloca %struct.ScmObj*, align 8
%anf_45bind40384 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40178)
store volatile %struct.ScmObj* %anf_45bind40384, %struct.ScmObj** %stackaddr$prim50196, align 8
%stackaddr$prim50197 = alloca %struct.ScmObj*, align 8
%anf_45bind40385 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40384)
store volatile %struct.ScmObj* %anf_45bind40385, %struct.ScmObj** %stackaddr$prim50197, align 8
%stackaddr$prim50198 = alloca %struct.ScmObj*, align 8
%anf_45bind40386 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40385)
store volatile %struct.ScmObj* %anf_45bind40386, %struct.ScmObj** %stackaddr$prim50198, align 8
%stackaddr$prim50199 = alloca %struct.ScmObj*, align 8
%cpsprim40445 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40386)
store volatile %struct.ScmObj* %cpsprim40445, %struct.ScmObj** %stackaddr$prim50199, align 8
%ae43930 = call %struct.ScmObj* @const_init_int(i64 0)
%args49272$k40444$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50200 = alloca %struct.ScmObj*, align 8
%args49272$k40444$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40445, %struct.ScmObj* %args49272$k40444$0)
store volatile %struct.ScmObj* %args49272$k40444$1, %struct.ScmObj** %stackaddr$prim50200, align 8
%stackaddr$prim50201 = alloca %struct.ScmObj*, align 8
%args49272$k40444$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43930, %struct.ScmObj* %args49272$k40444$1)
store volatile %struct.ScmObj* %args49272$k40444$2, %struct.ScmObj** %stackaddr$prim50201, align 8
%clofunc50202 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40444)
musttail call tailcc void %clofunc50202(%struct.ScmObj* %k40444, %struct.ScmObj* %args49272$k40444$2)
ret void
}

define tailcc void @proc_clo$ae43900(%struct.ScmObj* %env$ae43900,%struct.ScmObj* %current_45args49274) {
%stackaddr$prim50203 = alloca %struct.ScmObj*, align 8
%k40446 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49274)
store volatile %struct.ScmObj* %k40446, %struct.ScmObj** %stackaddr$prim50203, align 8
%stackaddr$prim50204 = alloca %struct.ScmObj*, align 8
%current_45args49275 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49274)
store volatile %struct.ScmObj* %current_45args49275, %struct.ScmObj** %stackaddr$prim50204, align 8
%stackaddr$prim50205 = alloca %struct.ScmObj*, align 8
%x40180 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49275)
store volatile %struct.ScmObj* %x40180, %struct.ScmObj** %stackaddr$prim50205, align 8
%stackaddr$prim50206 = alloca %struct.ScmObj*, align 8
%anf_45bind40382 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40180)
store volatile %struct.ScmObj* %anf_45bind40382, %struct.ScmObj** %stackaddr$prim50206, align 8
%stackaddr$prim50207 = alloca %struct.ScmObj*, align 8
%anf_45bind40383 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40382)
store volatile %struct.ScmObj* %anf_45bind40383, %struct.ScmObj** %stackaddr$prim50207, align 8
%stackaddr$prim50208 = alloca %struct.ScmObj*, align 8
%cpsprim40447 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40383)
store volatile %struct.ScmObj* %cpsprim40447, %struct.ScmObj** %stackaddr$prim50208, align 8
%ae43905 = call %struct.ScmObj* @const_init_int(i64 0)
%args49277$k40446$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50209 = alloca %struct.ScmObj*, align 8
%args49277$k40446$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40447, %struct.ScmObj* %args49277$k40446$0)
store volatile %struct.ScmObj* %args49277$k40446$1, %struct.ScmObj** %stackaddr$prim50209, align 8
%stackaddr$prim50210 = alloca %struct.ScmObj*, align 8
%args49277$k40446$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43905, %struct.ScmObj* %args49277$k40446$1)
store volatile %struct.ScmObj* %args49277$k40446$2, %struct.ScmObj** %stackaddr$prim50210, align 8
%clofunc50211 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40446)
musttail call tailcc void %clofunc50211(%struct.ScmObj* %k40446, %struct.ScmObj* %args49277$k40446$2)
ret void
}

define tailcc void @proc_clo$ae43878(%struct.ScmObj* %env$ae43878,%struct.ScmObj* %current_45args49279) {
%stackaddr$prim50212 = alloca %struct.ScmObj*, align 8
%k40448 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49279)
store volatile %struct.ScmObj* %k40448, %struct.ScmObj** %stackaddr$prim50212, align 8
%stackaddr$prim50213 = alloca %struct.ScmObj*, align 8
%current_45args49280 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49279)
store volatile %struct.ScmObj* %current_45args49280, %struct.ScmObj** %stackaddr$prim50213, align 8
%stackaddr$prim50214 = alloca %struct.ScmObj*, align 8
%x40182 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49280)
store volatile %struct.ScmObj* %x40182, %struct.ScmObj** %stackaddr$prim50214, align 8
%stackaddr$prim50215 = alloca %struct.ScmObj*, align 8
%anf_45bind40381 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40182)
store volatile %struct.ScmObj* %anf_45bind40381, %struct.ScmObj** %stackaddr$prim50215, align 8
%stackaddr$prim50216 = alloca %struct.ScmObj*, align 8
%cpsprim40449 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40381)
store volatile %struct.ScmObj* %cpsprim40449, %struct.ScmObj** %stackaddr$prim50216, align 8
%ae43882 = call %struct.ScmObj* @const_init_int(i64 0)
%args49282$k40448$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50217 = alloca %struct.ScmObj*, align 8
%args49282$k40448$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40449, %struct.ScmObj* %args49282$k40448$0)
store volatile %struct.ScmObj* %args49282$k40448$1, %struct.ScmObj** %stackaddr$prim50217, align 8
%stackaddr$prim50218 = alloca %struct.ScmObj*, align 8
%args49282$k40448$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43882, %struct.ScmObj* %args49282$k40448$1)
store volatile %struct.ScmObj* %args49282$k40448$2, %struct.ScmObj** %stackaddr$prim50218, align 8
%clofunc50219 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40448)
musttail call tailcc void %clofunc50219(%struct.ScmObj* %k40448, %struct.ScmObj* %args49282$k40448$2)
ret void
}

define tailcc void @proc_clo$ae43858(%struct.ScmObj* %env$ae43858,%struct.ScmObj* %current_45args49284) {
%stackaddr$prim50220 = alloca %struct.ScmObj*, align 8
%k40450 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49284)
store volatile %struct.ScmObj* %k40450, %struct.ScmObj** %stackaddr$prim50220, align 8
%stackaddr$prim50221 = alloca %struct.ScmObj*, align 8
%current_45args49285 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49284)
store volatile %struct.ScmObj* %current_45args49285, %struct.ScmObj** %stackaddr$prim50221, align 8
%stackaddr$prim50222 = alloca %struct.ScmObj*, align 8
%x40184 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49285)
store volatile %struct.ScmObj* %x40184, %struct.ScmObj** %stackaddr$prim50222, align 8
%stackaddr$prim50223 = alloca %struct.ScmObj*, align 8
%cpsprim40451 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40184)
store volatile %struct.ScmObj* %cpsprim40451, %struct.ScmObj** %stackaddr$prim50223, align 8
%ae43861 = call %struct.ScmObj* @const_init_int(i64 0)
%args49287$k40450$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50224 = alloca %struct.ScmObj*, align 8
%args49287$k40450$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40451, %struct.ScmObj* %args49287$k40450$0)
store volatile %struct.ScmObj* %args49287$k40450$1, %struct.ScmObj** %stackaddr$prim50224, align 8
%stackaddr$prim50225 = alloca %struct.ScmObj*, align 8
%args49287$k40450$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43861, %struct.ScmObj* %args49287$k40450$1)
store volatile %struct.ScmObj* %args49287$k40450$2, %struct.ScmObj** %stackaddr$prim50225, align 8
%clofunc50226 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40450)
musttail call tailcc void %clofunc50226(%struct.ScmObj* %k40450, %struct.ScmObj* %args49287$k40450$2)
ret void
}

define tailcc void @proc_clo$ae43760(%struct.ScmObj* %env$ae43760,%struct.ScmObj* %args4018640452) {
%stackaddr$env-ref50227 = alloca %struct.ScmObj*, align 8
%_37foldl140125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43760, i64 0)
store %struct.ScmObj* %_37foldl140125, %struct.ScmObj** %stackaddr$env-ref50227
%stackaddr$prim50228 = alloca %struct.ScmObj*, align 8
%k40453 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4018640452)
store volatile %struct.ScmObj* %k40453, %struct.ScmObj** %stackaddr$prim50228, align 8
%stackaddr$prim50229 = alloca %struct.ScmObj*, align 8
%args40186 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4018640452)
store volatile %struct.ScmObj* %args40186, %struct.ScmObj** %stackaddr$prim50229, align 8
%stackaddr$prim50230 = alloca %struct.ScmObj*, align 8
%anf_45bind40375 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args40186)
store volatile %struct.ScmObj* %anf_45bind40375, %struct.ScmObj** %stackaddr$prim50230, align 8
%truthy$cmp50231 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40375)
%cmp$cmp50231 = icmp eq i64 %truthy$cmp50231, 1
br i1 %cmp$cmp50231, label %truebranch$cmp50231, label %falsebranch$cmp50231
truebranch$cmp50231:
%ae43766 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43767 = call %struct.ScmObj* @const_init_int(i64 1)
%args49289$k40453$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50232 = alloca %struct.ScmObj*, align 8
%args49289$k40453$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43767, %struct.ScmObj* %args49289$k40453$0)
store volatile %struct.ScmObj* %args49289$k40453$1, %struct.ScmObj** %stackaddr$prim50232, align 8
%stackaddr$prim50233 = alloca %struct.ScmObj*, align 8
%args49289$k40453$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43766, %struct.ScmObj* %args49289$k40453$1)
store volatile %struct.ScmObj* %args49289$k40453$2, %struct.ScmObj** %stackaddr$prim50233, align 8
%clofunc50234 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40453)
musttail call tailcc void %clofunc50234(%struct.ScmObj* %k40453, %struct.ScmObj* %args49289$k40453$2)
ret void
falsebranch$cmp50231:
%stackaddr$prim50235 = alloca %struct.ScmObj*, align 8
%anf_45bind40376 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40186)
store volatile %struct.ScmObj* %anf_45bind40376, %struct.ScmObj** %stackaddr$prim50235, align 8
%stackaddr$prim50236 = alloca %struct.ScmObj*, align 8
%anf_45bind40377 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40376)
store volatile %struct.ScmObj* %anf_45bind40377, %struct.ScmObj** %stackaddr$prim50236, align 8
%truthy$cmp50237 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40377)
%cmp$cmp50237 = icmp eq i64 %truthy$cmp50237, 1
br i1 %cmp$cmp50237, label %truebranch$cmp50237, label %falsebranch$cmp50237
truebranch$cmp50237:
%stackaddr$prim50238 = alloca %struct.ScmObj*, align 8
%cpsprim40454 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40186)
store volatile %struct.ScmObj* %cpsprim40454, %struct.ScmObj** %stackaddr$prim50238, align 8
%ae43779 = call %struct.ScmObj* @const_init_int(i64 0)
%args49290$k40453$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50239 = alloca %struct.ScmObj*, align 8
%args49290$k40453$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40454, %struct.ScmObj* %args49290$k40453$0)
store volatile %struct.ScmObj* %args49290$k40453$1, %struct.ScmObj** %stackaddr$prim50239, align 8
%stackaddr$prim50240 = alloca %struct.ScmObj*, align 8
%args49290$k40453$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43779, %struct.ScmObj* %args49290$k40453$1)
store volatile %struct.ScmObj* %args49290$k40453$2, %struct.ScmObj** %stackaddr$prim50240, align 8
%clofunc50241 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40453)
musttail call tailcc void %clofunc50241(%struct.ScmObj* %k40453, %struct.ScmObj* %args49290$k40453$2)
ret void
falsebranch$cmp50237:
%stackaddr$makeclosure50242 = alloca %struct.ScmObj*, align 8
%fptrToInt50243 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43784 to i64
%ae43784 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50243)
store volatile %struct.ScmObj* %ae43784, %struct.ScmObj** %stackaddr$makeclosure50242, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43784, %struct.ScmObj* %k40453, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43784, %struct.ScmObj* %_37foldl140125, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43784, %struct.ScmObj* %args40186, i64 2)
%ae43785 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50244 = alloca %struct.ScmObj*, align 8
%fptrToInt50245 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43786 to i64
%ae43786 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50245)
store volatile %struct.ScmObj* %ae43786, %struct.ScmObj** %stackaddr$makeclosure50244, align 8
%args49300$ae43784$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50246 = alloca %struct.ScmObj*, align 8
%args49300$ae43784$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43786, %struct.ScmObj* %args49300$ae43784$0)
store volatile %struct.ScmObj* %args49300$ae43784$1, %struct.ScmObj** %stackaddr$prim50246, align 8
%stackaddr$prim50247 = alloca %struct.ScmObj*, align 8
%args49300$ae43784$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43785, %struct.ScmObj* %args49300$ae43784$1)
store volatile %struct.ScmObj* %args49300$ae43784$2, %struct.ScmObj** %stackaddr$prim50247, align 8
%clofunc50248 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43784)
musttail call tailcc void %clofunc50248(%struct.ScmObj* %ae43784, %struct.ScmObj* %args49300$ae43784$2)
ret void
}

define tailcc void @proc_clo$ae43784(%struct.ScmObj* %env$ae43784,%struct.ScmObj* %current_45args49291) {
%stackaddr$env-ref50249 = alloca %struct.ScmObj*, align 8
%k40453 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43784, i64 0)
store %struct.ScmObj* %k40453, %struct.ScmObj** %stackaddr$env-ref50249
%stackaddr$env-ref50250 = alloca %struct.ScmObj*, align 8
%_37foldl140125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43784, i64 1)
store %struct.ScmObj* %_37foldl140125, %struct.ScmObj** %stackaddr$env-ref50250
%stackaddr$env-ref50251 = alloca %struct.ScmObj*, align 8
%args40186 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43784, i64 2)
store %struct.ScmObj* %args40186, %struct.ScmObj** %stackaddr$env-ref50251
%stackaddr$prim50252 = alloca %struct.ScmObj*, align 8
%_95k40455 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49291)
store volatile %struct.ScmObj* %_95k40455, %struct.ScmObj** %stackaddr$prim50252, align 8
%stackaddr$prim50253 = alloca %struct.ScmObj*, align 8
%current_45args49292 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49291)
store volatile %struct.ScmObj* %current_45args49292, %struct.ScmObj** %stackaddr$prim50253, align 8
%stackaddr$prim50254 = alloca %struct.ScmObj*, align 8
%anf_45bind40378 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49292)
store volatile %struct.ScmObj* %anf_45bind40378, %struct.ScmObj** %stackaddr$prim50254, align 8
%stackaddr$prim50255 = alloca %struct.ScmObj*, align 8
%anf_45bind40379 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40186)
store volatile %struct.ScmObj* %anf_45bind40379, %struct.ScmObj** %stackaddr$prim50255, align 8
%stackaddr$prim50256 = alloca %struct.ScmObj*, align 8
%anf_45bind40380 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40186)
store volatile %struct.ScmObj* %anf_45bind40380, %struct.ScmObj** %stackaddr$prim50256, align 8
%args49294$_37foldl140125$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50257 = alloca %struct.ScmObj*, align 8
%args49294$_37foldl140125$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40380, %struct.ScmObj* %args49294$_37foldl140125$0)
store volatile %struct.ScmObj* %args49294$_37foldl140125$1, %struct.ScmObj** %stackaddr$prim50257, align 8
%stackaddr$prim50258 = alloca %struct.ScmObj*, align 8
%args49294$_37foldl140125$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40379, %struct.ScmObj* %args49294$_37foldl140125$1)
store volatile %struct.ScmObj* %args49294$_37foldl140125$2, %struct.ScmObj** %stackaddr$prim50258, align 8
%stackaddr$prim50259 = alloca %struct.ScmObj*, align 8
%args49294$_37foldl140125$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40378, %struct.ScmObj* %args49294$_37foldl140125$2)
store volatile %struct.ScmObj* %args49294$_37foldl140125$3, %struct.ScmObj** %stackaddr$prim50259, align 8
%stackaddr$prim50260 = alloca %struct.ScmObj*, align 8
%args49294$_37foldl140125$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40453, %struct.ScmObj* %args49294$_37foldl140125$3)
store volatile %struct.ScmObj* %args49294$_37foldl140125$4, %struct.ScmObj** %stackaddr$prim50260, align 8
%clofunc50261 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140125)
musttail call tailcc void %clofunc50261(%struct.ScmObj* %_37foldl140125, %struct.ScmObj* %args49294$_37foldl140125$4)
ret void
}

define tailcc void @proc_clo$ae43786(%struct.ScmObj* %env$ae43786,%struct.ScmObj* %current_45args49295) {
%stackaddr$prim50262 = alloca %struct.ScmObj*, align 8
%k40456 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49295)
store volatile %struct.ScmObj* %k40456, %struct.ScmObj** %stackaddr$prim50262, align 8
%stackaddr$prim50263 = alloca %struct.ScmObj*, align 8
%current_45args49296 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49295)
store volatile %struct.ScmObj* %current_45args49296, %struct.ScmObj** %stackaddr$prim50263, align 8
%stackaddr$prim50264 = alloca %struct.ScmObj*, align 8
%n40188 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49296)
store volatile %struct.ScmObj* %n40188, %struct.ScmObj** %stackaddr$prim50264, align 8
%stackaddr$prim50265 = alloca %struct.ScmObj*, align 8
%current_45args49297 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49296)
store volatile %struct.ScmObj* %current_45args49297, %struct.ScmObj** %stackaddr$prim50265, align 8
%stackaddr$prim50266 = alloca %struct.ScmObj*, align 8
%v40187 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49297)
store volatile %struct.ScmObj* %v40187, %struct.ScmObj** %stackaddr$prim50266, align 8
%stackaddr$prim50267 = alloca %struct.ScmObj*, align 8
%cpsprim40457 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v40187, %struct.ScmObj* %n40188)
store volatile %struct.ScmObj* %cpsprim40457, %struct.ScmObj** %stackaddr$prim50267, align 8
%ae43790 = call %struct.ScmObj* @const_init_int(i64 0)
%args49299$k40456$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50268 = alloca %struct.ScmObj*, align 8
%args49299$k40456$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40457, %struct.ScmObj* %args49299$k40456$0)
store volatile %struct.ScmObj* %args49299$k40456$1, %struct.ScmObj** %stackaddr$prim50268, align 8
%stackaddr$prim50269 = alloca %struct.ScmObj*, align 8
%args49299$k40456$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43790, %struct.ScmObj* %args49299$k40456$1)
store volatile %struct.ScmObj* %args49299$k40456$2, %struct.ScmObj** %stackaddr$prim50269, align 8
%clofunc50270 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40456)
musttail call tailcc void %clofunc50270(%struct.ScmObj* %k40456, %struct.ScmObj* %args49299$k40456$2)
ret void
}

define tailcc void @proc_clo$ae43356(%struct.ScmObj* %env$ae43356,%struct.ScmObj* %current_45args49302) {
%stackaddr$prim50271 = alloca %struct.ScmObj*, align 8
%k40458 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49302)
store volatile %struct.ScmObj* %k40458, %struct.ScmObj** %stackaddr$prim50271, align 8
%stackaddr$prim50272 = alloca %struct.ScmObj*, align 8
%current_45args49303 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49302)
store volatile %struct.ScmObj* %current_45args49303, %struct.ScmObj** %stackaddr$prim50272, align 8
%stackaddr$prim50273 = alloca %struct.ScmObj*, align 8
%v40191 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49303)
store volatile %struct.ScmObj* %v40191, %struct.ScmObj** %stackaddr$prim50273, align 8
%stackaddr$prim50274 = alloca %struct.ScmObj*, align 8
%current_45args49304 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49303)
store volatile %struct.ScmObj* %current_45args49304, %struct.ScmObj** %stackaddr$prim50274, align 8
%stackaddr$prim50275 = alloca %struct.ScmObj*, align 8
%lst40190 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49304)
store volatile %struct.ScmObj* %lst40190, %struct.ScmObj** %stackaddr$prim50275, align 8
%ae43357 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim50276 = alloca %struct.ScmObj*, align 8
%lst40192 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae43357, %struct.ScmObj* %lst40190)
store volatile %struct.ScmObj* %lst40192, %struct.ScmObj** %stackaddr$prim50276, align 8
%stackaddr$makeclosure50277 = alloca %struct.ScmObj*, align 8
%fptrToInt50278 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43359 to i64
%ae43359 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50278)
store volatile %struct.ScmObj* %ae43359, %struct.ScmObj** %stackaddr$makeclosure50277, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43359, %struct.ScmObj* %lst40192, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43359, %struct.ScmObj* %v40191, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43359, %struct.ScmObj* %k40458, i64 2)
%ae43360 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50279 = alloca %struct.ScmObj*, align 8
%fptrToInt50280 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43361 to i64
%ae43361 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50280)
store volatile %struct.ScmObj* %ae43361, %struct.ScmObj** %stackaddr$makeclosure50279, align 8
%args49326$ae43359$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50281 = alloca %struct.ScmObj*, align 8
%args49326$ae43359$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43361, %struct.ScmObj* %args49326$ae43359$0)
store volatile %struct.ScmObj* %args49326$ae43359$1, %struct.ScmObj** %stackaddr$prim50281, align 8
%stackaddr$prim50282 = alloca %struct.ScmObj*, align 8
%args49326$ae43359$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43360, %struct.ScmObj* %args49326$ae43359$1)
store volatile %struct.ScmObj* %args49326$ae43359$2, %struct.ScmObj** %stackaddr$prim50282, align 8
%clofunc50283 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43359)
musttail call tailcc void %clofunc50283(%struct.ScmObj* %ae43359, %struct.ScmObj* %args49326$ae43359$2)
ret void
}

define tailcc void @proc_clo$ae43359(%struct.ScmObj* %env$ae43359,%struct.ScmObj* %current_45args49306) {
%stackaddr$env-ref50284 = alloca %struct.ScmObj*, align 8
%lst40192 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43359, i64 0)
store %struct.ScmObj* %lst40192, %struct.ScmObj** %stackaddr$env-ref50284
%stackaddr$env-ref50285 = alloca %struct.ScmObj*, align 8
%v40191 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43359, i64 1)
store %struct.ScmObj* %v40191, %struct.ScmObj** %stackaddr$env-ref50285
%stackaddr$env-ref50286 = alloca %struct.ScmObj*, align 8
%k40458 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43359, i64 2)
store %struct.ScmObj* %k40458, %struct.ScmObj** %stackaddr$env-ref50286
%stackaddr$prim50287 = alloca %struct.ScmObj*, align 8
%_95k40459 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49306)
store volatile %struct.ScmObj* %_95k40459, %struct.ScmObj** %stackaddr$prim50287, align 8
%stackaddr$prim50288 = alloca %struct.ScmObj*, align 8
%current_45args49307 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49306)
store volatile %struct.ScmObj* %current_45args49307, %struct.ScmObj** %stackaddr$prim50288, align 8
%stackaddr$prim50289 = alloca %struct.ScmObj*, align 8
%anf_45bind40367 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49307)
store volatile %struct.ScmObj* %anf_45bind40367, %struct.ScmObj** %stackaddr$prim50289, align 8
%stackaddr$makeclosure50290 = alloca %struct.ScmObj*, align 8
%fptrToInt50291 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43375 to i64
%ae43375 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50291)
store volatile %struct.ScmObj* %ae43375, %struct.ScmObj** %stackaddr$makeclosure50290, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43375, %struct.ScmObj* %lst40192, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43375, %struct.ScmObj* %v40191, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43375, %struct.ScmObj* %k40458, i64 2)
%stackaddr$makeclosure50292 = alloca %struct.ScmObj*, align 8
%fptrToInt50293 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43376 to i64
%ae43376 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50293)
store volatile %struct.ScmObj* %ae43376, %struct.ScmObj** %stackaddr$makeclosure50292, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43376, %struct.ScmObj* %lst40192, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43376, %struct.ScmObj* %v40191, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43376, %struct.ScmObj* %k40458, i64 2)
%args49321$anf_45bind40367$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50294 = alloca %struct.ScmObj*, align 8
%args49321$anf_45bind40367$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43376, %struct.ScmObj* %args49321$anf_45bind40367$0)
store volatile %struct.ScmObj* %args49321$anf_45bind40367$1, %struct.ScmObj** %stackaddr$prim50294, align 8
%stackaddr$prim50295 = alloca %struct.ScmObj*, align 8
%args49321$anf_45bind40367$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43375, %struct.ScmObj* %args49321$anf_45bind40367$1)
store volatile %struct.ScmObj* %args49321$anf_45bind40367$2, %struct.ScmObj** %stackaddr$prim50295, align 8
%clofunc50296 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40367)
musttail call tailcc void %clofunc50296(%struct.ScmObj* %anf_45bind40367, %struct.ScmObj* %args49321$anf_45bind40367$2)
ret void
}

define tailcc void @proc_clo$ae43375(%struct.ScmObj* %env$ae43375,%struct.ScmObj* %current_45args49309) {
%stackaddr$env-ref50297 = alloca %struct.ScmObj*, align 8
%lst40192 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43375, i64 0)
store %struct.ScmObj* %lst40192, %struct.ScmObj** %stackaddr$env-ref50297
%stackaddr$env-ref50298 = alloca %struct.ScmObj*, align 8
%v40191 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43375, i64 1)
store %struct.ScmObj* %v40191, %struct.ScmObj** %stackaddr$env-ref50298
%stackaddr$env-ref50299 = alloca %struct.ScmObj*, align 8
%k40458 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43375, i64 2)
store %struct.ScmObj* %k40458, %struct.ScmObj** %stackaddr$env-ref50299
%stackaddr$prim50300 = alloca %struct.ScmObj*, align 8
%_95k40460 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49309)
store volatile %struct.ScmObj* %_95k40460, %struct.ScmObj** %stackaddr$prim50300, align 8
%stackaddr$prim50301 = alloca %struct.ScmObj*, align 8
%current_45args49310 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49309)
store volatile %struct.ScmObj* %current_45args49310, %struct.ScmObj** %stackaddr$prim50301, align 8
%stackaddr$prim50302 = alloca %struct.ScmObj*, align 8
%cc40193 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49310)
store volatile %struct.ScmObj* %cc40193, %struct.ScmObj** %stackaddr$prim50302, align 8
%ae43484 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50303 = alloca %struct.ScmObj*, align 8
%anf_45bind40368 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40192, %struct.ScmObj* %ae43484)
store volatile %struct.ScmObj* %anf_45bind40368, %struct.ScmObj** %stackaddr$prim50303, align 8
%stackaddr$prim50304 = alloca %struct.ScmObj*, align 8
%anf_45bind40369 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40368)
store volatile %struct.ScmObj* %anf_45bind40369, %struct.ScmObj** %stackaddr$prim50304, align 8
%truthy$cmp50305 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40369)
%cmp$cmp50305 = icmp eq i64 %truthy$cmp50305, 1
br i1 %cmp$cmp50305, label %truebranch$cmp50305, label %falsebranch$cmp50305
truebranch$cmp50305:
%ae43488 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43489 = call %struct.ScmObj* @const_init_false()
%args49312$k40458$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50306 = alloca %struct.ScmObj*, align 8
%args49312$k40458$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43489, %struct.ScmObj* %args49312$k40458$0)
store volatile %struct.ScmObj* %args49312$k40458$1, %struct.ScmObj** %stackaddr$prim50306, align 8
%stackaddr$prim50307 = alloca %struct.ScmObj*, align 8
%args49312$k40458$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43488, %struct.ScmObj* %args49312$k40458$1)
store volatile %struct.ScmObj* %args49312$k40458$2, %struct.ScmObj** %stackaddr$prim50307, align 8
%clofunc50308 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40458)
musttail call tailcc void %clofunc50308(%struct.ScmObj* %k40458, %struct.ScmObj* %args49312$k40458$2)
ret void
falsebranch$cmp50305:
%ae43497 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50309 = alloca %struct.ScmObj*, align 8
%anf_45bind40370 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40192, %struct.ScmObj* %ae43497)
store volatile %struct.ScmObj* %anf_45bind40370, %struct.ScmObj** %stackaddr$prim50309, align 8
%stackaddr$prim50310 = alloca %struct.ScmObj*, align 8
%anf_45bind40371 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40370)
store volatile %struct.ScmObj* %anf_45bind40371, %struct.ScmObj** %stackaddr$prim50310, align 8
%stackaddr$prim50311 = alloca %struct.ScmObj*, align 8
%anf_45bind40372 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind40371, %struct.ScmObj* %v40191)
store volatile %struct.ScmObj* %anf_45bind40372, %struct.ScmObj** %stackaddr$prim50311, align 8
%truthy$cmp50312 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40372)
%cmp$cmp50312 = icmp eq i64 %truthy$cmp50312, 1
br i1 %cmp$cmp50312, label %truebranch$cmp50312, label %falsebranch$cmp50312
truebranch$cmp50312:
%ae43503 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50313 = alloca %struct.ScmObj*, align 8
%cpsprim40461 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40192, %struct.ScmObj* %ae43503)
store volatile %struct.ScmObj* %cpsprim40461, %struct.ScmObj** %stackaddr$prim50313, align 8
%ae43505 = call %struct.ScmObj* @const_init_int(i64 0)
%args49313$k40458$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50314 = alloca %struct.ScmObj*, align 8
%args49313$k40458$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40461, %struct.ScmObj* %args49313$k40458$0)
store volatile %struct.ScmObj* %args49313$k40458$1, %struct.ScmObj** %stackaddr$prim50314, align 8
%stackaddr$prim50315 = alloca %struct.ScmObj*, align 8
%args49313$k40458$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43505, %struct.ScmObj* %args49313$k40458$1)
store volatile %struct.ScmObj* %args49313$k40458$2, %struct.ScmObj** %stackaddr$prim50315, align 8
%clofunc50316 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40458)
musttail call tailcc void %clofunc50316(%struct.ScmObj* %k40458, %struct.ScmObj* %args49313$k40458$2)
ret void
falsebranch$cmp50312:
%ae43516 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50317 = alloca %struct.ScmObj*, align 8
%anf_45bind40373 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40192, %struct.ScmObj* %ae43516)
store volatile %struct.ScmObj* %anf_45bind40373, %struct.ScmObj** %stackaddr$prim50317, align 8
%stackaddr$prim50318 = alloca %struct.ScmObj*, align 8
%anf_45bind40374 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40373)
store volatile %struct.ScmObj* %anf_45bind40374, %struct.ScmObj** %stackaddr$prim50318, align 8
%ae43519 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50319 = alloca %struct.ScmObj*, align 8
%_95040195 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40192, %struct.ScmObj* %ae43519, %struct.ScmObj* %anf_45bind40374)
store volatile %struct.ScmObj* %_95040195, %struct.ScmObj** %stackaddr$prim50319, align 8
%args49314$cc40193$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50320 = alloca %struct.ScmObj*, align 8
%args49314$cc40193$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40193, %struct.ScmObj* %args49314$cc40193$0)
store volatile %struct.ScmObj* %args49314$cc40193$1, %struct.ScmObj** %stackaddr$prim50320, align 8
%stackaddr$prim50321 = alloca %struct.ScmObj*, align 8
%args49314$cc40193$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40458, %struct.ScmObj* %args49314$cc40193$1)
store volatile %struct.ScmObj* %args49314$cc40193$2, %struct.ScmObj** %stackaddr$prim50321, align 8
%clofunc50322 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40193)
musttail call tailcc void %clofunc50322(%struct.ScmObj* %cc40193, %struct.ScmObj* %args49314$cc40193$2)
ret void
}

define tailcc void @proc_clo$ae43376(%struct.ScmObj* %env$ae43376,%struct.ScmObj* %current_45args49315) {
%stackaddr$env-ref50323 = alloca %struct.ScmObj*, align 8
%lst40192 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43376, i64 0)
store %struct.ScmObj* %lst40192, %struct.ScmObj** %stackaddr$env-ref50323
%stackaddr$env-ref50324 = alloca %struct.ScmObj*, align 8
%v40191 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43376, i64 1)
store %struct.ScmObj* %v40191, %struct.ScmObj** %stackaddr$env-ref50324
%stackaddr$env-ref50325 = alloca %struct.ScmObj*, align 8
%k40458 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43376, i64 2)
store %struct.ScmObj* %k40458, %struct.ScmObj** %stackaddr$env-ref50325
%stackaddr$prim50326 = alloca %struct.ScmObj*, align 8
%_95k40460 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49315)
store volatile %struct.ScmObj* %_95k40460, %struct.ScmObj** %stackaddr$prim50326, align 8
%stackaddr$prim50327 = alloca %struct.ScmObj*, align 8
%current_45args49316 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49315)
store volatile %struct.ScmObj* %current_45args49316, %struct.ScmObj** %stackaddr$prim50327, align 8
%stackaddr$prim50328 = alloca %struct.ScmObj*, align 8
%cc40193 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49316)
store volatile %struct.ScmObj* %cc40193, %struct.ScmObj** %stackaddr$prim50328, align 8
%ae43378 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50329 = alloca %struct.ScmObj*, align 8
%anf_45bind40368 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40192, %struct.ScmObj* %ae43378)
store volatile %struct.ScmObj* %anf_45bind40368, %struct.ScmObj** %stackaddr$prim50329, align 8
%stackaddr$prim50330 = alloca %struct.ScmObj*, align 8
%anf_45bind40369 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40368)
store volatile %struct.ScmObj* %anf_45bind40369, %struct.ScmObj** %stackaddr$prim50330, align 8
%truthy$cmp50331 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40369)
%cmp$cmp50331 = icmp eq i64 %truthy$cmp50331, 1
br i1 %cmp$cmp50331, label %truebranch$cmp50331, label %falsebranch$cmp50331
truebranch$cmp50331:
%ae43382 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43383 = call %struct.ScmObj* @const_init_false()
%args49318$k40458$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50332 = alloca %struct.ScmObj*, align 8
%args49318$k40458$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43383, %struct.ScmObj* %args49318$k40458$0)
store volatile %struct.ScmObj* %args49318$k40458$1, %struct.ScmObj** %stackaddr$prim50332, align 8
%stackaddr$prim50333 = alloca %struct.ScmObj*, align 8
%args49318$k40458$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43382, %struct.ScmObj* %args49318$k40458$1)
store volatile %struct.ScmObj* %args49318$k40458$2, %struct.ScmObj** %stackaddr$prim50333, align 8
%clofunc50334 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40458)
musttail call tailcc void %clofunc50334(%struct.ScmObj* %k40458, %struct.ScmObj* %args49318$k40458$2)
ret void
falsebranch$cmp50331:
%ae43391 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50335 = alloca %struct.ScmObj*, align 8
%anf_45bind40370 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40192, %struct.ScmObj* %ae43391)
store volatile %struct.ScmObj* %anf_45bind40370, %struct.ScmObj** %stackaddr$prim50335, align 8
%stackaddr$prim50336 = alloca %struct.ScmObj*, align 8
%anf_45bind40371 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40370)
store volatile %struct.ScmObj* %anf_45bind40371, %struct.ScmObj** %stackaddr$prim50336, align 8
%stackaddr$prim50337 = alloca %struct.ScmObj*, align 8
%anf_45bind40372 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind40371, %struct.ScmObj* %v40191)
store volatile %struct.ScmObj* %anf_45bind40372, %struct.ScmObj** %stackaddr$prim50337, align 8
%truthy$cmp50338 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40372)
%cmp$cmp50338 = icmp eq i64 %truthy$cmp50338, 1
br i1 %cmp$cmp50338, label %truebranch$cmp50338, label %falsebranch$cmp50338
truebranch$cmp50338:
%ae43397 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50339 = alloca %struct.ScmObj*, align 8
%cpsprim40461 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40192, %struct.ScmObj* %ae43397)
store volatile %struct.ScmObj* %cpsprim40461, %struct.ScmObj** %stackaddr$prim50339, align 8
%ae43399 = call %struct.ScmObj* @const_init_int(i64 0)
%args49319$k40458$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50340 = alloca %struct.ScmObj*, align 8
%args49319$k40458$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40461, %struct.ScmObj* %args49319$k40458$0)
store volatile %struct.ScmObj* %args49319$k40458$1, %struct.ScmObj** %stackaddr$prim50340, align 8
%stackaddr$prim50341 = alloca %struct.ScmObj*, align 8
%args49319$k40458$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43399, %struct.ScmObj* %args49319$k40458$1)
store volatile %struct.ScmObj* %args49319$k40458$2, %struct.ScmObj** %stackaddr$prim50341, align 8
%clofunc50342 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40458)
musttail call tailcc void %clofunc50342(%struct.ScmObj* %k40458, %struct.ScmObj* %args49319$k40458$2)
ret void
falsebranch$cmp50338:
%ae43410 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50343 = alloca %struct.ScmObj*, align 8
%anf_45bind40373 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40192, %struct.ScmObj* %ae43410)
store volatile %struct.ScmObj* %anf_45bind40373, %struct.ScmObj** %stackaddr$prim50343, align 8
%stackaddr$prim50344 = alloca %struct.ScmObj*, align 8
%anf_45bind40374 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40373)
store volatile %struct.ScmObj* %anf_45bind40374, %struct.ScmObj** %stackaddr$prim50344, align 8
%ae43413 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50345 = alloca %struct.ScmObj*, align 8
%_95040195 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40192, %struct.ScmObj* %ae43413, %struct.ScmObj* %anf_45bind40374)
store volatile %struct.ScmObj* %_95040195, %struct.ScmObj** %stackaddr$prim50345, align 8
%args49320$cc40193$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50346 = alloca %struct.ScmObj*, align 8
%args49320$cc40193$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40193, %struct.ScmObj* %args49320$cc40193$0)
store volatile %struct.ScmObj* %args49320$cc40193$1, %struct.ScmObj** %stackaddr$prim50346, align 8
%stackaddr$prim50347 = alloca %struct.ScmObj*, align 8
%args49320$cc40193$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40458, %struct.ScmObj* %args49320$cc40193$1)
store volatile %struct.ScmObj* %args49320$cc40193$2, %struct.ScmObj** %stackaddr$prim50347, align 8
%clofunc50348 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40193)
musttail call tailcc void %clofunc50348(%struct.ScmObj* %cc40193, %struct.ScmObj* %args49320$cc40193$2)
ret void
}

define tailcc void @proc_clo$ae43361(%struct.ScmObj* %env$ae43361,%struct.ScmObj* %current_45args49322) {
%stackaddr$prim50349 = alloca %struct.ScmObj*, align 8
%k40462 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49322)
store volatile %struct.ScmObj* %k40462, %struct.ScmObj** %stackaddr$prim50349, align 8
%stackaddr$prim50350 = alloca %struct.ScmObj*, align 8
%current_45args49323 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49322)
store volatile %struct.ScmObj* %current_45args49323, %struct.ScmObj** %stackaddr$prim50350, align 8
%stackaddr$prim50351 = alloca %struct.ScmObj*, align 8
%u40194 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49323)
store volatile %struct.ScmObj* %u40194, %struct.ScmObj** %stackaddr$prim50351, align 8
%args49325$u40194$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50352 = alloca %struct.ScmObj*, align 8
%args49325$u40194$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u40194, %struct.ScmObj* %args49325$u40194$0)
store volatile %struct.ScmObj* %args49325$u40194$1, %struct.ScmObj** %stackaddr$prim50352, align 8
%stackaddr$prim50353 = alloca %struct.ScmObj*, align 8
%args49325$u40194$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40462, %struct.ScmObj* %args49325$u40194$1)
store volatile %struct.ScmObj* %args49325$u40194$2, %struct.ScmObj** %stackaddr$prim50353, align 8
%clofunc50354 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u40194)
musttail call tailcc void %clofunc50354(%struct.ScmObj* %u40194, %struct.ScmObj* %args49325$u40194$2)
ret void
}

define tailcc void @proc_clo$ae42820(%struct.ScmObj* %env$ae42820,%struct.ScmObj* %current_45args49328) {
%stackaddr$prim50355 = alloca %struct.ScmObj*, align 8
%k40463 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49328)
store volatile %struct.ScmObj* %k40463, %struct.ScmObj** %stackaddr$prim50355, align 8
%stackaddr$prim50356 = alloca %struct.ScmObj*, align 8
%current_45args49329 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49328)
store volatile %struct.ScmObj* %current_45args49329, %struct.ScmObj** %stackaddr$prim50356, align 8
%stackaddr$prim50357 = alloca %struct.ScmObj*, align 8
%lst40198 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49329)
store volatile %struct.ScmObj* %lst40198, %struct.ScmObj** %stackaddr$prim50357, align 8
%stackaddr$prim50358 = alloca %struct.ScmObj*, align 8
%current_45args49330 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49329)
store volatile %struct.ScmObj* %current_45args49330, %struct.ScmObj** %stackaddr$prim50358, align 8
%stackaddr$prim50359 = alloca %struct.ScmObj*, align 8
%n40197 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49330)
store volatile %struct.ScmObj* %n40197, %struct.ScmObj** %stackaddr$prim50359, align 8
%ae42821 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim50360 = alloca %struct.ScmObj*, align 8
%n40200 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42821, %struct.ScmObj* %n40197)
store volatile %struct.ScmObj* %n40200, %struct.ScmObj** %stackaddr$prim50360, align 8
%ae42823 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim50361 = alloca %struct.ScmObj*, align 8
%lst40199 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42823, %struct.ScmObj* %lst40198)
store volatile %struct.ScmObj* %lst40199, %struct.ScmObj** %stackaddr$prim50361, align 8
%stackaddr$makeclosure50362 = alloca %struct.ScmObj*, align 8
%fptrToInt50363 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42825 to i64
%ae42825 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50363)
store volatile %struct.ScmObj* %ae42825, %struct.ScmObj** %stackaddr$makeclosure50362, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42825, %struct.ScmObj* %lst40199, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42825, %struct.ScmObj* %k40463, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42825, %struct.ScmObj* %n40200, i64 2)
%ae42826 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50364 = alloca %struct.ScmObj*, align 8
%fptrToInt50365 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42827 to i64
%ae42827 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50365)
store volatile %struct.ScmObj* %ae42827, %struct.ScmObj** %stackaddr$makeclosure50364, align 8
%args49350$ae42825$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50366 = alloca %struct.ScmObj*, align 8
%args49350$ae42825$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42827, %struct.ScmObj* %args49350$ae42825$0)
store volatile %struct.ScmObj* %args49350$ae42825$1, %struct.ScmObj** %stackaddr$prim50366, align 8
%stackaddr$prim50367 = alloca %struct.ScmObj*, align 8
%args49350$ae42825$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42826, %struct.ScmObj* %args49350$ae42825$1)
store volatile %struct.ScmObj* %args49350$ae42825$2, %struct.ScmObj** %stackaddr$prim50367, align 8
%clofunc50368 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42825)
musttail call tailcc void %clofunc50368(%struct.ScmObj* %ae42825, %struct.ScmObj* %args49350$ae42825$2)
ret void
}

define tailcc void @proc_clo$ae42825(%struct.ScmObj* %env$ae42825,%struct.ScmObj* %current_45args49332) {
%stackaddr$env-ref50369 = alloca %struct.ScmObj*, align 8
%lst40199 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42825, i64 0)
store %struct.ScmObj* %lst40199, %struct.ScmObj** %stackaddr$env-ref50369
%stackaddr$env-ref50370 = alloca %struct.ScmObj*, align 8
%k40463 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42825, i64 1)
store %struct.ScmObj* %k40463, %struct.ScmObj** %stackaddr$env-ref50370
%stackaddr$env-ref50371 = alloca %struct.ScmObj*, align 8
%n40200 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42825, i64 2)
store %struct.ScmObj* %n40200, %struct.ScmObj** %stackaddr$env-ref50371
%stackaddr$prim50372 = alloca %struct.ScmObj*, align 8
%_95k40464 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49332)
store volatile %struct.ScmObj* %_95k40464, %struct.ScmObj** %stackaddr$prim50372, align 8
%stackaddr$prim50373 = alloca %struct.ScmObj*, align 8
%current_45args49333 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49332)
store volatile %struct.ScmObj* %current_45args49333, %struct.ScmObj** %stackaddr$prim50373, align 8
%stackaddr$prim50374 = alloca %struct.ScmObj*, align 8
%anf_45bind40360 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49333)
store volatile %struct.ScmObj* %anf_45bind40360, %struct.ScmObj** %stackaddr$prim50374, align 8
%stackaddr$makeclosure50375 = alloca %struct.ScmObj*, align 8
%fptrToInt50376 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42841 to i64
%ae42841 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50376)
store volatile %struct.ScmObj* %ae42841, %struct.ScmObj** %stackaddr$makeclosure50375, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42841, %struct.ScmObj* %lst40199, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42841, %struct.ScmObj* %k40463, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42841, %struct.ScmObj* %n40200, i64 2)
%stackaddr$makeclosure50377 = alloca %struct.ScmObj*, align 8
%fptrToInt50378 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42842 to i64
%ae42842 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50378)
store volatile %struct.ScmObj* %ae42842, %struct.ScmObj** %stackaddr$makeclosure50377, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42842, %struct.ScmObj* %lst40199, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42842, %struct.ScmObj* %k40463, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42842, %struct.ScmObj* %n40200, i64 2)
%args49345$anf_45bind40360$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50379 = alloca %struct.ScmObj*, align 8
%args49345$anf_45bind40360$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42842, %struct.ScmObj* %args49345$anf_45bind40360$0)
store volatile %struct.ScmObj* %args49345$anf_45bind40360$1, %struct.ScmObj** %stackaddr$prim50379, align 8
%stackaddr$prim50380 = alloca %struct.ScmObj*, align 8
%args49345$anf_45bind40360$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42841, %struct.ScmObj* %args49345$anf_45bind40360$1)
store volatile %struct.ScmObj* %args49345$anf_45bind40360$2, %struct.ScmObj** %stackaddr$prim50380, align 8
%clofunc50381 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40360)
musttail call tailcc void %clofunc50381(%struct.ScmObj* %anf_45bind40360, %struct.ScmObj* %args49345$anf_45bind40360$2)
ret void
}

define tailcc void @proc_clo$ae42841(%struct.ScmObj* %env$ae42841,%struct.ScmObj* %current_45args49335) {
%stackaddr$env-ref50382 = alloca %struct.ScmObj*, align 8
%lst40199 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42841, i64 0)
store %struct.ScmObj* %lst40199, %struct.ScmObj** %stackaddr$env-ref50382
%stackaddr$env-ref50383 = alloca %struct.ScmObj*, align 8
%k40463 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42841, i64 1)
store %struct.ScmObj* %k40463, %struct.ScmObj** %stackaddr$env-ref50383
%stackaddr$env-ref50384 = alloca %struct.ScmObj*, align 8
%n40200 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42841, i64 2)
store %struct.ScmObj* %n40200, %struct.ScmObj** %stackaddr$env-ref50384
%stackaddr$prim50385 = alloca %struct.ScmObj*, align 8
%_95k40465 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49335)
store volatile %struct.ScmObj* %_95k40465, %struct.ScmObj** %stackaddr$prim50385, align 8
%stackaddr$prim50386 = alloca %struct.ScmObj*, align 8
%current_45args49336 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49335)
store volatile %struct.ScmObj* %current_45args49336, %struct.ScmObj** %stackaddr$prim50386, align 8
%stackaddr$prim50387 = alloca %struct.ScmObj*, align 8
%cc40201 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49336)
store volatile %struct.ScmObj* %cc40201, %struct.ScmObj** %stackaddr$prim50387, align 8
%ae42984 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50388 = alloca %struct.ScmObj*, align 8
%anf_45bind40361 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40200, %struct.ScmObj* %ae42984)
store volatile %struct.ScmObj* %anf_45bind40361, %struct.ScmObj** %stackaddr$prim50388, align 8
%ae42985 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50389 = alloca %struct.ScmObj*, align 8
%anf_45bind40362 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae42985, %struct.ScmObj* %anf_45bind40361)
store volatile %struct.ScmObj* %anf_45bind40362, %struct.ScmObj** %stackaddr$prim50389, align 8
%truthy$cmp50390 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40362)
%cmp$cmp50390 = icmp eq i64 %truthy$cmp50390, 1
br i1 %cmp$cmp50390, label %truebranch$cmp50390, label %falsebranch$cmp50390
truebranch$cmp50390:
%ae42989 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50391 = alloca %struct.ScmObj*, align 8
%cpsprim40466 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40199, %struct.ScmObj* %ae42989)
store volatile %struct.ScmObj* %cpsprim40466, %struct.ScmObj** %stackaddr$prim50391, align 8
%ae42991 = call %struct.ScmObj* @const_init_int(i64 0)
%args49338$k40463$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50392 = alloca %struct.ScmObj*, align 8
%args49338$k40463$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40466, %struct.ScmObj* %args49338$k40463$0)
store volatile %struct.ScmObj* %args49338$k40463$1, %struct.ScmObj** %stackaddr$prim50392, align 8
%stackaddr$prim50393 = alloca %struct.ScmObj*, align 8
%args49338$k40463$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42991, %struct.ScmObj* %args49338$k40463$1)
store volatile %struct.ScmObj* %args49338$k40463$2, %struct.ScmObj** %stackaddr$prim50393, align 8
%clofunc50394 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40463)
musttail call tailcc void %clofunc50394(%struct.ScmObj* %k40463, %struct.ScmObj* %args49338$k40463$2)
ret void
falsebranch$cmp50390:
%ae43002 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50395 = alloca %struct.ScmObj*, align 8
%anf_45bind40363 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40199, %struct.ScmObj* %ae43002)
store volatile %struct.ScmObj* %anf_45bind40363, %struct.ScmObj** %stackaddr$prim50395, align 8
%stackaddr$prim50396 = alloca %struct.ScmObj*, align 8
%anf_45bind40364 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40363)
store volatile %struct.ScmObj* %anf_45bind40364, %struct.ScmObj** %stackaddr$prim50396, align 8
%ae43005 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50397 = alloca %struct.ScmObj*, align 8
%_95040204 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40199, %struct.ScmObj* %ae43005, %struct.ScmObj* %anf_45bind40364)
store volatile %struct.ScmObj* %_95040204, %struct.ScmObj** %stackaddr$prim50397, align 8
%ae43008 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50398 = alloca %struct.ScmObj*, align 8
%anf_45bind40365 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40200, %struct.ScmObj* %ae43008)
store volatile %struct.ScmObj* %anf_45bind40365, %struct.ScmObj** %stackaddr$prim50398, align 8
%ae43010 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim50399 = alloca %struct.ScmObj*, align 8
%anf_45bind40366 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind40365, %struct.ScmObj* %ae43010)
store volatile %struct.ScmObj* %anf_45bind40366, %struct.ScmObj** %stackaddr$prim50399, align 8
%ae43012 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50400 = alloca %struct.ScmObj*, align 8
%_95140203 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n40200, %struct.ScmObj* %ae43012, %struct.ScmObj* %anf_45bind40366)
store volatile %struct.ScmObj* %_95140203, %struct.ScmObj** %stackaddr$prim50400, align 8
%args49339$cc40201$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50401 = alloca %struct.ScmObj*, align 8
%args49339$cc40201$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40201, %struct.ScmObj* %args49339$cc40201$0)
store volatile %struct.ScmObj* %args49339$cc40201$1, %struct.ScmObj** %stackaddr$prim50401, align 8
%stackaddr$prim50402 = alloca %struct.ScmObj*, align 8
%args49339$cc40201$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40463, %struct.ScmObj* %args49339$cc40201$1)
store volatile %struct.ScmObj* %args49339$cc40201$2, %struct.ScmObj** %stackaddr$prim50402, align 8
%clofunc50403 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40201)
musttail call tailcc void %clofunc50403(%struct.ScmObj* %cc40201, %struct.ScmObj* %args49339$cc40201$2)
ret void
}

define tailcc void @proc_clo$ae42842(%struct.ScmObj* %env$ae42842,%struct.ScmObj* %current_45args49340) {
%stackaddr$env-ref50404 = alloca %struct.ScmObj*, align 8
%lst40199 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42842, i64 0)
store %struct.ScmObj* %lst40199, %struct.ScmObj** %stackaddr$env-ref50404
%stackaddr$env-ref50405 = alloca %struct.ScmObj*, align 8
%k40463 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42842, i64 1)
store %struct.ScmObj* %k40463, %struct.ScmObj** %stackaddr$env-ref50405
%stackaddr$env-ref50406 = alloca %struct.ScmObj*, align 8
%n40200 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42842, i64 2)
store %struct.ScmObj* %n40200, %struct.ScmObj** %stackaddr$env-ref50406
%stackaddr$prim50407 = alloca %struct.ScmObj*, align 8
%_95k40465 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49340)
store volatile %struct.ScmObj* %_95k40465, %struct.ScmObj** %stackaddr$prim50407, align 8
%stackaddr$prim50408 = alloca %struct.ScmObj*, align 8
%current_45args49341 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49340)
store volatile %struct.ScmObj* %current_45args49341, %struct.ScmObj** %stackaddr$prim50408, align 8
%stackaddr$prim50409 = alloca %struct.ScmObj*, align 8
%cc40201 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49341)
store volatile %struct.ScmObj* %cc40201, %struct.ScmObj** %stackaddr$prim50409, align 8
%ae42844 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50410 = alloca %struct.ScmObj*, align 8
%anf_45bind40361 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40200, %struct.ScmObj* %ae42844)
store volatile %struct.ScmObj* %anf_45bind40361, %struct.ScmObj** %stackaddr$prim50410, align 8
%ae42845 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50411 = alloca %struct.ScmObj*, align 8
%anf_45bind40362 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae42845, %struct.ScmObj* %anf_45bind40361)
store volatile %struct.ScmObj* %anf_45bind40362, %struct.ScmObj** %stackaddr$prim50411, align 8
%truthy$cmp50412 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40362)
%cmp$cmp50412 = icmp eq i64 %truthy$cmp50412, 1
br i1 %cmp$cmp50412, label %truebranch$cmp50412, label %falsebranch$cmp50412
truebranch$cmp50412:
%ae42849 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50413 = alloca %struct.ScmObj*, align 8
%cpsprim40466 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40199, %struct.ScmObj* %ae42849)
store volatile %struct.ScmObj* %cpsprim40466, %struct.ScmObj** %stackaddr$prim50413, align 8
%ae42851 = call %struct.ScmObj* @const_init_int(i64 0)
%args49343$k40463$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50414 = alloca %struct.ScmObj*, align 8
%args49343$k40463$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40466, %struct.ScmObj* %args49343$k40463$0)
store volatile %struct.ScmObj* %args49343$k40463$1, %struct.ScmObj** %stackaddr$prim50414, align 8
%stackaddr$prim50415 = alloca %struct.ScmObj*, align 8
%args49343$k40463$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42851, %struct.ScmObj* %args49343$k40463$1)
store volatile %struct.ScmObj* %args49343$k40463$2, %struct.ScmObj** %stackaddr$prim50415, align 8
%clofunc50416 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40463)
musttail call tailcc void %clofunc50416(%struct.ScmObj* %k40463, %struct.ScmObj* %args49343$k40463$2)
ret void
falsebranch$cmp50412:
%ae42862 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50417 = alloca %struct.ScmObj*, align 8
%anf_45bind40363 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40199, %struct.ScmObj* %ae42862)
store volatile %struct.ScmObj* %anf_45bind40363, %struct.ScmObj** %stackaddr$prim50417, align 8
%stackaddr$prim50418 = alloca %struct.ScmObj*, align 8
%anf_45bind40364 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40363)
store volatile %struct.ScmObj* %anf_45bind40364, %struct.ScmObj** %stackaddr$prim50418, align 8
%ae42865 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50419 = alloca %struct.ScmObj*, align 8
%_95040204 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40199, %struct.ScmObj* %ae42865, %struct.ScmObj* %anf_45bind40364)
store volatile %struct.ScmObj* %_95040204, %struct.ScmObj** %stackaddr$prim50419, align 8
%ae42868 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50420 = alloca %struct.ScmObj*, align 8
%anf_45bind40365 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40200, %struct.ScmObj* %ae42868)
store volatile %struct.ScmObj* %anf_45bind40365, %struct.ScmObj** %stackaddr$prim50420, align 8
%ae42870 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim50421 = alloca %struct.ScmObj*, align 8
%anf_45bind40366 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind40365, %struct.ScmObj* %ae42870)
store volatile %struct.ScmObj* %anf_45bind40366, %struct.ScmObj** %stackaddr$prim50421, align 8
%ae42872 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50422 = alloca %struct.ScmObj*, align 8
%_95140203 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n40200, %struct.ScmObj* %ae42872, %struct.ScmObj* %anf_45bind40366)
store volatile %struct.ScmObj* %_95140203, %struct.ScmObj** %stackaddr$prim50422, align 8
%args49344$cc40201$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50423 = alloca %struct.ScmObj*, align 8
%args49344$cc40201$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40201, %struct.ScmObj* %args49344$cc40201$0)
store volatile %struct.ScmObj* %args49344$cc40201$1, %struct.ScmObj** %stackaddr$prim50423, align 8
%stackaddr$prim50424 = alloca %struct.ScmObj*, align 8
%args49344$cc40201$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40463, %struct.ScmObj* %args49344$cc40201$1)
store volatile %struct.ScmObj* %args49344$cc40201$2, %struct.ScmObj** %stackaddr$prim50424, align 8
%clofunc50425 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40201)
musttail call tailcc void %clofunc50425(%struct.ScmObj* %cc40201, %struct.ScmObj* %args49344$cc40201$2)
ret void
}

define tailcc void @proc_clo$ae42827(%struct.ScmObj* %env$ae42827,%struct.ScmObj* %current_45args49346) {
%stackaddr$prim50426 = alloca %struct.ScmObj*, align 8
%k40467 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49346)
store volatile %struct.ScmObj* %k40467, %struct.ScmObj** %stackaddr$prim50426, align 8
%stackaddr$prim50427 = alloca %struct.ScmObj*, align 8
%current_45args49347 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49346)
store volatile %struct.ScmObj* %current_45args49347, %struct.ScmObj** %stackaddr$prim50427, align 8
%stackaddr$prim50428 = alloca %struct.ScmObj*, align 8
%u40202 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49347)
store volatile %struct.ScmObj* %u40202, %struct.ScmObj** %stackaddr$prim50428, align 8
%args49349$u40202$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50429 = alloca %struct.ScmObj*, align 8
%args49349$u40202$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u40202, %struct.ScmObj* %args49349$u40202$0)
store volatile %struct.ScmObj* %args49349$u40202$1, %struct.ScmObj** %stackaddr$prim50429, align 8
%stackaddr$prim50430 = alloca %struct.ScmObj*, align 8
%args49349$u40202$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40467, %struct.ScmObj* %args49349$u40202$1)
store volatile %struct.ScmObj* %args49349$u40202$2, %struct.ScmObj** %stackaddr$prim50430, align 8
%clofunc50431 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u40202)
musttail call tailcc void %clofunc50431(%struct.ScmObj* %u40202, %struct.ScmObj* %args49349$u40202$2)
ret void
}

define tailcc void @proc_clo$ae42404(%struct.ScmObj* %env$ae42404,%struct.ScmObj* %current_45args49352) {
%stackaddr$prim50432 = alloca %struct.ScmObj*, align 8
%k40468 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49352)
store volatile %struct.ScmObj* %k40468, %struct.ScmObj** %stackaddr$prim50432, align 8
%stackaddr$prim50433 = alloca %struct.ScmObj*, align 8
%current_45args49353 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49352)
store volatile %struct.ScmObj* %current_45args49353, %struct.ScmObj** %stackaddr$prim50433, align 8
%stackaddr$prim50434 = alloca %struct.ScmObj*, align 8
%a40206 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49353)
store volatile %struct.ScmObj* %a40206, %struct.ScmObj** %stackaddr$prim50434, align 8
%ae42405 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim50435 = alloca %struct.ScmObj*, align 8
%a40207 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42405, %struct.ScmObj* %a40206)
store volatile %struct.ScmObj* %a40207, %struct.ScmObj** %stackaddr$prim50435, align 8
%stackaddr$makeclosure50436 = alloca %struct.ScmObj*, align 8
%fptrToInt50437 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42407 to i64
%ae42407 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt50437)
store volatile %struct.ScmObj* %ae42407, %struct.ScmObj** %stackaddr$makeclosure50436, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42407, %struct.ScmObj* %k40468, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42407, %struct.ScmObj* %a40207, i64 1)
%ae42408 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50438 = alloca %struct.ScmObj*, align 8
%fptrToInt50439 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42409 to i64
%ae42409 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50439)
store volatile %struct.ScmObj* %ae42409, %struct.ScmObj** %stackaddr$makeclosure50438, align 8
%args49375$ae42407$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50440 = alloca %struct.ScmObj*, align 8
%args49375$ae42407$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42409, %struct.ScmObj* %args49375$ae42407$0)
store volatile %struct.ScmObj* %args49375$ae42407$1, %struct.ScmObj** %stackaddr$prim50440, align 8
%stackaddr$prim50441 = alloca %struct.ScmObj*, align 8
%args49375$ae42407$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42408, %struct.ScmObj* %args49375$ae42407$1)
store volatile %struct.ScmObj* %args49375$ae42407$2, %struct.ScmObj** %stackaddr$prim50441, align 8
%clofunc50442 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42407)
musttail call tailcc void %clofunc50442(%struct.ScmObj* %ae42407, %struct.ScmObj* %args49375$ae42407$2)
ret void
}

define tailcc void @proc_clo$ae42407(%struct.ScmObj* %env$ae42407,%struct.ScmObj* %current_45args49355) {
%stackaddr$env-ref50443 = alloca %struct.ScmObj*, align 8
%k40468 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42407, i64 0)
store %struct.ScmObj* %k40468, %struct.ScmObj** %stackaddr$env-ref50443
%stackaddr$env-ref50444 = alloca %struct.ScmObj*, align 8
%a40207 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42407, i64 1)
store %struct.ScmObj* %a40207, %struct.ScmObj** %stackaddr$env-ref50444
%stackaddr$prim50445 = alloca %struct.ScmObj*, align 8
%_95k40469 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49355)
store volatile %struct.ScmObj* %_95k40469, %struct.ScmObj** %stackaddr$prim50445, align 8
%stackaddr$prim50446 = alloca %struct.ScmObj*, align 8
%current_45args49356 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49355)
store volatile %struct.ScmObj* %current_45args49356, %struct.ScmObj** %stackaddr$prim50446, align 8
%stackaddr$prim50447 = alloca %struct.ScmObj*, align 8
%anf_45bind40352 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49356)
store volatile %struct.ScmObj* %anf_45bind40352, %struct.ScmObj** %stackaddr$prim50447, align 8
%stackaddr$makeclosure50448 = alloca %struct.ScmObj*, align 8
%fptrToInt50449 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42426 to i64
%ae42426 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt50449)
store volatile %struct.ScmObj* %ae42426, %struct.ScmObj** %stackaddr$makeclosure50448, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42426, %struct.ScmObj* %k40468, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42426, %struct.ScmObj* %a40207, i64 1)
%stackaddr$makeclosure50450 = alloca %struct.ScmObj*, align 8
%fptrToInt50451 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42427 to i64
%ae42427 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt50451)
store volatile %struct.ScmObj* %ae42427, %struct.ScmObj** %stackaddr$makeclosure50450, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42427, %struct.ScmObj* %k40468, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42427, %struct.ScmObj* %a40207, i64 1)
%args49370$anf_45bind40352$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50452 = alloca %struct.ScmObj*, align 8
%args49370$anf_45bind40352$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42427, %struct.ScmObj* %args49370$anf_45bind40352$0)
store volatile %struct.ScmObj* %args49370$anf_45bind40352$1, %struct.ScmObj** %stackaddr$prim50452, align 8
%stackaddr$prim50453 = alloca %struct.ScmObj*, align 8
%args49370$anf_45bind40352$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42426, %struct.ScmObj* %args49370$anf_45bind40352$1)
store volatile %struct.ScmObj* %args49370$anf_45bind40352$2, %struct.ScmObj** %stackaddr$prim50453, align 8
%clofunc50454 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40352)
musttail call tailcc void %clofunc50454(%struct.ScmObj* %anf_45bind40352, %struct.ScmObj* %args49370$anf_45bind40352$2)
ret void
}

define tailcc void @proc_clo$ae42426(%struct.ScmObj* %env$ae42426,%struct.ScmObj* %current_45args49358) {
%stackaddr$env-ref50455 = alloca %struct.ScmObj*, align 8
%k40468 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42426, i64 0)
store %struct.ScmObj* %k40468, %struct.ScmObj** %stackaddr$env-ref50455
%stackaddr$env-ref50456 = alloca %struct.ScmObj*, align 8
%a40207 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42426, i64 1)
store %struct.ScmObj* %a40207, %struct.ScmObj** %stackaddr$env-ref50456
%stackaddr$prim50457 = alloca %struct.ScmObj*, align 8
%_95k40470 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49358)
store volatile %struct.ScmObj* %_95k40470, %struct.ScmObj** %stackaddr$prim50457, align 8
%stackaddr$prim50458 = alloca %struct.ScmObj*, align 8
%current_45args49359 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49358)
store volatile %struct.ScmObj* %current_45args49359, %struct.ScmObj** %stackaddr$prim50458, align 8
%stackaddr$prim50459 = alloca %struct.ScmObj*, align 8
%cc40208 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49359)
store volatile %struct.ScmObj* %cc40208, %struct.ScmObj** %stackaddr$prim50459, align 8
%ae42542 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50460 = alloca %struct.ScmObj*, align 8
%anf_45bind40353 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40207, %struct.ScmObj* %ae42542)
store volatile %struct.ScmObj* %anf_45bind40353, %struct.ScmObj** %stackaddr$prim50460, align 8
%stackaddr$prim50461 = alloca %struct.ScmObj*, align 8
%anf_45bind40354 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40353)
store volatile %struct.ScmObj* %anf_45bind40354, %struct.ScmObj** %stackaddr$prim50461, align 8
%truthy$cmp50462 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40354)
%cmp$cmp50462 = icmp eq i64 %truthy$cmp50462, 1
br i1 %cmp$cmp50462, label %truebranch$cmp50462, label %falsebranch$cmp50462
truebranch$cmp50462:
%ae42546 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42547 = call %struct.ScmObj* @const_init_true()
%args49361$k40468$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50463 = alloca %struct.ScmObj*, align 8
%args49361$k40468$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42547, %struct.ScmObj* %args49361$k40468$0)
store volatile %struct.ScmObj* %args49361$k40468$1, %struct.ScmObj** %stackaddr$prim50463, align 8
%stackaddr$prim50464 = alloca %struct.ScmObj*, align 8
%args49361$k40468$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42546, %struct.ScmObj* %args49361$k40468$1)
store volatile %struct.ScmObj* %args49361$k40468$2, %struct.ScmObj** %stackaddr$prim50464, align 8
%clofunc50465 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40468)
musttail call tailcc void %clofunc50465(%struct.ScmObj* %k40468, %struct.ScmObj* %args49361$k40468$2)
ret void
falsebranch$cmp50462:
%ae42555 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50466 = alloca %struct.ScmObj*, align 8
%anf_45bind40355 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40207, %struct.ScmObj* %ae42555)
store volatile %struct.ScmObj* %anf_45bind40355, %struct.ScmObj** %stackaddr$prim50466, align 8
%stackaddr$prim50467 = alloca %struct.ScmObj*, align 8
%anf_45bind40356 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind40355)
store volatile %struct.ScmObj* %anf_45bind40356, %struct.ScmObj** %stackaddr$prim50467, align 8
%truthy$cmp50468 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40356)
%cmp$cmp50468 = icmp eq i64 %truthy$cmp50468, 1
br i1 %cmp$cmp50468, label %truebranch$cmp50468, label %falsebranch$cmp50468
truebranch$cmp50468:
%ae42559 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50469 = alloca %struct.ScmObj*, align 8
%anf_45bind40357 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40207, %struct.ScmObj* %ae42559)
store volatile %struct.ScmObj* %anf_45bind40357, %struct.ScmObj** %stackaddr$prim50469, align 8
%stackaddr$prim50470 = alloca %struct.ScmObj*, align 8
%b40210 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40357)
store volatile %struct.ScmObj* %b40210, %struct.ScmObj** %stackaddr$prim50470, align 8
%ae42562 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50471 = alloca %struct.ScmObj*, align 8
%anf_45bind40358 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40207, %struct.ScmObj* %ae42562)
store volatile %struct.ScmObj* %anf_45bind40358, %struct.ScmObj** %stackaddr$prim50471, align 8
%stackaddr$prim50472 = alloca %struct.ScmObj*, align 8
%anf_45bind40359 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40358)
store volatile %struct.ScmObj* %anf_45bind40359, %struct.ScmObj** %stackaddr$prim50472, align 8
%ae42565 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50473 = alloca %struct.ScmObj*, align 8
%_95040211 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a40207, %struct.ScmObj* %ae42565, %struct.ScmObj* %anf_45bind40359)
store volatile %struct.ScmObj* %_95040211, %struct.ScmObj** %stackaddr$prim50473, align 8
%args49362$cc40208$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50474 = alloca %struct.ScmObj*, align 8
%args49362$cc40208$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40208, %struct.ScmObj* %args49362$cc40208$0)
store volatile %struct.ScmObj* %args49362$cc40208$1, %struct.ScmObj** %stackaddr$prim50474, align 8
%stackaddr$prim50475 = alloca %struct.ScmObj*, align 8
%args49362$cc40208$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40468, %struct.ScmObj* %args49362$cc40208$1)
store volatile %struct.ScmObj* %args49362$cc40208$2, %struct.ScmObj** %stackaddr$prim50475, align 8
%clofunc50476 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40208)
musttail call tailcc void %clofunc50476(%struct.ScmObj* %cc40208, %struct.ScmObj* %args49362$cc40208$2)
ret void
falsebranch$cmp50468:
%ae42598 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42599 = call %struct.ScmObj* @const_init_false()
%args49363$k40468$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50477 = alloca %struct.ScmObj*, align 8
%args49363$k40468$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42599, %struct.ScmObj* %args49363$k40468$0)
store volatile %struct.ScmObj* %args49363$k40468$1, %struct.ScmObj** %stackaddr$prim50477, align 8
%stackaddr$prim50478 = alloca %struct.ScmObj*, align 8
%args49363$k40468$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42598, %struct.ScmObj* %args49363$k40468$1)
store volatile %struct.ScmObj* %args49363$k40468$2, %struct.ScmObj** %stackaddr$prim50478, align 8
%clofunc50479 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40468)
musttail call tailcc void %clofunc50479(%struct.ScmObj* %k40468, %struct.ScmObj* %args49363$k40468$2)
ret void
}

define tailcc void @proc_clo$ae42427(%struct.ScmObj* %env$ae42427,%struct.ScmObj* %current_45args49364) {
%stackaddr$env-ref50480 = alloca %struct.ScmObj*, align 8
%k40468 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42427, i64 0)
store %struct.ScmObj* %k40468, %struct.ScmObj** %stackaddr$env-ref50480
%stackaddr$env-ref50481 = alloca %struct.ScmObj*, align 8
%a40207 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42427, i64 1)
store %struct.ScmObj* %a40207, %struct.ScmObj** %stackaddr$env-ref50481
%stackaddr$prim50482 = alloca %struct.ScmObj*, align 8
%_95k40470 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49364)
store volatile %struct.ScmObj* %_95k40470, %struct.ScmObj** %stackaddr$prim50482, align 8
%stackaddr$prim50483 = alloca %struct.ScmObj*, align 8
%current_45args49365 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49364)
store volatile %struct.ScmObj* %current_45args49365, %struct.ScmObj** %stackaddr$prim50483, align 8
%stackaddr$prim50484 = alloca %struct.ScmObj*, align 8
%cc40208 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49365)
store volatile %struct.ScmObj* %cc40208, %struct.ScmObj** %stackaddr$prim50484, align 8
%ae42429 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50485 = alloca %struct.ScmObj*, align 8
%anf_45bind40353 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40207, %struct.ScmObj* %ae42429)
store volatile %struct.ScmObj* %anf_45bind40353, %struct.ScmObj** %stackaddr$prim50485, align 8
%stackaddr$prim50486 = alloca %struct.ScmObj*, align 8
%anf_45bind40354 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40353)
store volatile %struct.ScmObj* %anf_45bind40354, %struct.ScmObj** %stackaddr$prim50486, align 8
%truthy$cmp50487 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40354)
%cmp$cmp50487 = icmp eq i64 %truthy$cmp50487, 1
br i1 %cmp$cmp50487, label %truebranch$cmp50487, label %falsebranch$cmp50487
truebranch$cmp50487:
%ae42433 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42434 = call %struct.ScmObj* @const_init_true()
%args49367$k40468$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50488 = alloca %struct.ScmObj*, align 8
%args49367$k40468$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42434, %struct.ScmObj* %args49367$k40468$0)
store volatile %struct.ScmObj* %args49367$k40468$1, %struct.ScmObj** %stackaddr$prim50488, align 8
%stackaddr$prim50489 = alloca %struct.ScmObj*, align 8
%args49367$k40468$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42433, %struct.ScmObj* %args49367$k40468$1)
store volatile %struct.ScmObj* %args49367$k40468$2, %struct.ScmObj** %stackaddr$prim50489, align 8
%clofunc50490 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40468)
musttail call tailcc void %clofunc50490(%struct.ScmObj* %k40468, %struct.ScmObj* %args49367$k40468$2)
ret void
falsebranch$cmp50487:
%ae42442 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50491 = alloca %struct.ScmObj*, align 8
%anf_45bind40355 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40207, %struct.ScmObj* %ae42442)
store volatile %struct.ScmObj* %anf_45bind40355, %struct.ScmObj** %stackaddr$prim50491, align 8
%stackaddr$prim50492 = alloca %struct.ScmObj*, align 8
%anf_45bind40356 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind40355)
store volatile %struct.ScmObj* %anf_45bind40356, %struct.ScmObj** %stackaddr$prim50492, align 8
%truthy$cmp50493 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40356)
%cmp$cmp50493 = icmp eq i64 %truthy$cmp50493, 1
br i1 %cmp$cmp50493, label %truebranch$cmp50493, label %falsebranch$cmp50493
truebranch$cmp50493:
%ae42446 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50494 = alloca %struct.ScmObj*, align 8
%anf_45bind40357 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40207, %struct.ScmObj* %ae42446)
store volatile %struct.ScmObj* %anf_45bind40357, %struct.ScmObj** %stackaddr$prim50494, align 8
%stackaddr$prim50495 = alloca %struct.ScmObj*, align 8
%b40210 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40357)
store volatile %struct.ScmObj* %b40210, %struct.ScmObj** %stackaddr$prim50495, align 8
%ae42449 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50496 = alloca %struct.ScmObj*, align 8
%anf_45bind40358 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40207, %struct.ScmObj* %ae42449)
store volatile %struct.ScmObj* %anf_45bind40358, %struct.ScmObj** %stackaddr$prim50496, align 8
%stackaddr$prim50497 = alloca %struct.ScmObj*, align 8
%anf_45bind40359 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40358)
store volatile %struct.ScmObj* %anf_45bind40359, %struct.ScmObj** %stackaddr$prim50497, align 8
%ae42452 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50498 = alloca %struct.ScmObj*, align 8
%_95040211 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a40207, %struct.ScmObj* %ae42452, %struct.ScmObj* %anf_45bind40359)
store volatile %struct.ScmObj* %_95040211, %struct.ScmObj** %stackaddr$prim50498, align 8
%args49368$cc40208$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50499 = alloca %struct.ScmObj*, align 8
%args49368$cc40208$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40208, %struct.ScmObj* %args49368$cc40208$0)
store volatile %struct.ScmObj* %args49368$cc40208$1, %struct.ScmObj** %stackaddr$prim50499, align 8
%stackaddr$prim50500 = alloca %struct.ScmObj*, align 8
%args49368$cc40208$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40468, %struct.ScmObj* %args49368$cc40208$1)
store volatile %struct.ScmObj* %args49368$cc40208$2, %struct.ScmObj** %stackaddr$prim50500, align 8
%clofunc50501 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40208)
musttail call tailcc void %clofunc50501(%struct.ScmObj* %cc40208, %struct.ScmObj* %args49368$cc40208$2)
ret void
falsebranch$cmp50493:
%ae42485 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42486 = call %struct.ScmObj* @const_init_false()
%args49369$k40468$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50502 = alloca %struct.ScmObj*, align 8
%args49369$k40468$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42486, %struct.ScmObj* %args49369$k40468$0)
store volatile %struct.ScmObj* %args49369$k40468$1, %struct.ScmObj** %stackaddr$prim50502, align 8
%stackaddr$prim50503 = alloca %struct.ScmObj*, align 8
%args49369$k40468$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42485, %struct.ScmObj* %args49369$k40468$1)
store volatile %struct.ScmObj* %args49369$k40468$2, %struct.ScmObj** %stackaddr$prim50503, align 8
%clofunc50504 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40468)
musttail call tailcc void %clofunc50504(%struct.ScmObj* %k40468, %struct.ScmObj* %args49369$k40468$2)
ret void
}

define tailcc void @proc_clo$ae42409(%struct.ScmObj* %env$ae42409,%struct.ScmObj* %current_45args49371) {
%stackaddr$prim50505 = alloca %struct.ScmObj*, align 8
%k40471 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49371)
store volatile %struct.ScmObj* %k40471, %struct.ScmObj** %stackaddr$prim50505, align 8
%stackaddr$prim50506 = alloca %struct.ScmObj*, align 8
%current_45args49372 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49371)
store volatile %struct.ScmObj* %current_45args49372, %struct.ScmObj** %stackaddr$prim50506, align 8
%stackaddr$prim50507 = alloca %struct.ScmObj*, align 8
%k40209 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49372)
store volatile %struct.ScmObj* %k40209, %struct.ScmObj** %stackaddr$prim50507, align 8
%ae42411 = call %struct.ScmObj* @const_init_int(i64 0)
%args49374$k40471$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50508 = alloca %struct.ScmObj*, align 8
%args49374$k40471$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40209, %struct.ScmObj* %args49374$k40471$0)
store volatile %struct.ScmObj* %args49374$k40471$1, %struct.ScmObj** %stackaddr$prim50508, align 8
%stackaddr$prim50509 = alloca %struct.ScmObj*, align 8
%args49374$k40471$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42411, %struct.ScmObj* %args49374$k40471$1)
store volatile %struct.ScmObj* %args49374$k40471$2, %struct.ScmObj** %stackaddr$prim50509, align 8
%clofunc50510 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40471)
musttail call tailcc void %clofunc50510(%struct.ScmObj* %k40471, %struct.ScmObj* %args49374$k40471$2)
ret void
}

define tailcc void @proc_clo$ae42332(%struct.ScmObj* %env$ae42332,%struct.ScmObj* %current_45args49377) {
%stackaddr$env-ref50511 = alloca %struct.ScmObj*, align 8
%_37append40213 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42332, i64 0)
store %struct.ScmObj* %_37append40213, %struct.ScmObj** %stackaddr$env-ref50511
%stackaddr$prim50512 = alloca %struct.ScmObj*, align 8
%k40472 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49377)
store volatile %struct.ScmObj* %k40472, %struct.ScmObj** %stackaddr$prim50512, align 8
%stackaddr$prim50513 = alloca %struct.ScmObj*, align 8
%current_45args49378 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49377)
store volatile %struct.ScmObj* %current_45args49378, %struct.ScmObj** %stackaddr$prim50513, align 8
%stackaddr$prim50514 = alloca %struct.ScmObj*, align 8
%ls040216 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49378)
store volatile %struct.ScmObj* %ls040216, %struct.ScmObj** %stackaddr$prim50514, align 8
%stackaddr$prim50515 = alloca %struct.ScmObj*, align 8
%current_45args49379 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49378)
store volatile %struct.ScmObj* %current_45args49379, %struct.ScmObj** %stackaddr$prim50515, align 8
%stackaddr$prim50516 = alloca %struct.ScmObj*, align 8
%ls140215 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49379)
store volatile %struct.ScmObj* %ls140215, %struct.ScmObj** %stackaddr$prim50516, align 8
%stackaddr$prim50517 = alloca %struct.ScmObj*, align 8
%anf_45bind40346 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls040216)
store volatile %struct.ScmObj* %anf_45bind40346, %struct.ScmObj** %stackaddr$prim50517, align 8
%truthy$cmp50518 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40346)
%cmp$cmp50518 = icmp eq i64 %truthy$cmp50518, 1
br i1 %cmp$cmp50518, label %truebranch$cmp50518, label %falsebranch$cmp50518
truebranch$cmp50518:
%ae42336 = call %struct.ScmObj* @const_init_int(i64 0)
%args49381$k40472$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50519 = alloca %struct.ScmObj*, align 8
%args49381$k40472$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls140215, %struct.ScmObj* %args49381$k40472$0)
store volatile %struct.ScmObj* %args49381$k40472$1, %struct.ScmObj** %stackaddr$prim50519, align 8
%stackaddr$prim50520 = alloca %struct.ScmObj*, align 8
%args49381$k40472$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42336, %struct.ScmObj* %args49381$k40472$1)
store volatile %struct.ScmObj* %args49381$k40472$2, %struct.ScmObj** %stackaddr$prim50520, align 8
%clofunc50521 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40472)
musttail call tailcc void %clofunc50521(%struct.ScmObj* %k40472, %struct.ScmObj* %args49381$k40472$2)
ret void
falsebranch$cmp50518:
%stackaddr$prim50522 = alloca %struct.ScmObj*, align 8
%anf_45bind40347 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls040216)
store volatile %struct.ScmObj* %anf_45bind40347, %struct.ScmObj** %stackaddr$prim50522, align 8
%ae42343 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50523 = alloca %struct.ScmObj*, align 8
%anf_45bind40348 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append40213, %struct.ScmObj* %ae42343)
store volatile %struct.ScmObj* %anf_45bind40348, %struct.ScmObj** %stackaddr$prim50523, align 8
%stackaddr$prim50524 = alloca %struct.ScmObj*, align 8
%anf_45bind40349 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls040216)
store volatile %struct.ScmObj* %anf_45bind40349, %struct.ScmObj** %stackaddr$prim50524, align 8
%stackaddr$makeclosure50525 = alloca %struct.ScmObj*, align 8
%fptrToInt50526 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42346 to i64
%ae42346 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt50526)
store volatile %struct.ScmObj* %ae42346, %struct.ScmObj** %stackaddr$makeclosure50525, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42346, %struct.ScmObj* %anf_45bind40347, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42346, %struct.ScmObj* %k40472, i64 1)
%args49386$anf_45bind40348$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50527 = alloca %struct.ScmObj*, align 8
%args49386$anf_45bind40348$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls140215, %struct.ScmObj* %args49386$anf_45bind40348$0)
store volatile %struct.ScmObj* %args49386$anf_45bind40348$1, %struct.ScmObj** %stackaddr$prim50527, align 8
%stackaddr$prim50528 = alloca %struct.ScmObj*, align 8
%args49386$anf_45bind40348$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40349, %struct.ScmObj* %args49386$anf_45bind40348$1)
store volatile %struct.ScmObj* %args49386$anf_45bind40348$2, %struct.ScmObj** %stackaddr$prim50528, align 8
%stackaddr$prim50529 = alloca %struct.ScmObj*, align 8
%args49386$anf_45bind40348$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42346, %struct.ScmObj* %args49386$anf_45bind40348$2)
store volatile %struct.ScmObj* %args49386$anf_45bind40348$3, %struct.ScmObj** %stackaddr$prim50529, align 8
%clofunc50530 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40348)
musttail call tailcc void %clofunc50530(%struct.ScmObj* %anf_45bind40348, %struct.ScmObj* %args49386$anf_45bind40348$3)
ret void
}

define tailcc void @proc_clo$ae42346(%struct.ScmObj* %env$ae42346,%struct.ScmObj* %current_45args49382) {
%stackaddr$env-ref50531 = alloca %struct.ScmObj*, align 8
%anf_45bind40347 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42346, i64 0)
store %struct.ScmObj* %anf_45bind40347, %struct.ScmObj** %stackaddr$env-ref50531
%stackaddr$env-ref50532 = alloca %struct.ScmObj*, align 8
%k40472 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42346, i64 1)
store %struct.ScmObj* %k40472, %struct.ScmObj** %stackaddr$env-ref50532
%stackaddr$prim50533 = alloca %struct.ScmObj*, align 8
%_95k40473 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49382)
store volatile %struct.ScmObj* %_95k40473, %struct.ScmObj** %stackaddr$prim50533, align 8
%stackaddr$prim50534 = alloca %struct.ScmObj*, align 8
%current_45args49383 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49382)
store volatile %struct.ScmObj* %current_45args49383, %struct.ScmObj** %stackaddr$prim50534, align 8
%stackaddr$prim50535 = alloca %struct.ScmObj*, align 8
%anf_45bind40350 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49383)
store volatile %struct.ScmObj* %anf_45bind40350, %struct.ScmObj** %stackaddr$prim50535, align 8
%stackaddr$prim50536 = alloca %struct.ScmObj*, align 8
%cpsprim40474 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40347, %struct.ScmObj* %anf_45bind40350)
store volatile %struct.ScmObj* %cpsprim40474, %struct.ScmObj** %stackaddr$prim50536, align 8
%ae42352 = call %struct.ScmObj* @const_init_int(i64 0)
%args49385$k40472$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50537 = alloca %struct.ScmObj*, align 8
%args49385$k40472$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40474, %struct.ScmObj* %args49385$k40472$0)
store volatile %struct.ScmObj* %args49385$k40472$1, %struct.ScmObj** %stackaddr$prim50537, align 8
%stackaddr$prim50538 = alloca %struct.ScmObj*, align 8
%args49385$k40472$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42352, %struct.ScmObj* %args49385$k40472$1)
store volatile %struct.ScmObj* %args49385$k40472$2, %struct.ScmObj** %stackaddr$prim50538, align 8
%clofunc50539 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40472)
musttail call tailcc void %clofunc50539(%struct.ScmObj* %k40472, %struct.ScmObj* %args49385$k40472$2)
ret void
}

define tailcc void @proc_clo$ae42306(%struct.ScmObj* %env$ae42306,%struct.ScmObj* %current_45args49388) {
%stackaddr$prim50540 = alloca %struct.ScmObj*, align 8
%k40475 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49388)
store volatile %struct.ScmObj* %k40475, %struct.ScmObj** %stackaddr$prim50540, align 8
%stackaddr$prim50541 = alloca %struct.ScmObj*, align 8
%current_45args49389 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49388)
store volatile %struct.ScmObj* %current_45args49389, %struct.ScmObj** %stackaddr$prim50541, align 8
%stackaddr$prim50542 = alloca %struct.ScmObj*, align 8
%a40219 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49389)
store volatile %struct.ScmObj* %a40219, %struct.ScmObj** %stackaddr$prim50542, align 8
%stackaddr$prim50543 = alloca %struct.ScmObj*, align 8
%current_45args49390 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49389)
store volatile %struct.ScmObj* %current_45args49390, %struct.ScmObj** %stackaddr$prim50543, align 8
%stackaddr$prim50544 = alloca %struct.ScmObj*, align 8
%b40218 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49390)
store volatile %struct.ScmObj* %b40218, %struct.ScmObj** %stackaddr$prim50544, align 8
%stackaddr$prim50545 = alloca %struct.ScmObj*, align 8
%anf_45bind40345 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a40219, %struct.ScmObj* %b40218)
store volatile %struct.ScmObj* %anf_45bind40345, %struct.ScmObj** %stackaddr$prim50545, align 8
%stackaddr$prim50546 = alloca %struct.ScmObj*, align 8
%cpsprim40476 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind40345)
store volatile %struct.ScmObj* %cpsprim40476, %struct.ScmObj** %stackaddr$prim50546, align 8
%ae42311 = call %struct.ScmObj* @const_init_int(i64 0)
%args49392$k40475$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50547 = alloca %struct.ScmObj*, align 8
%args49392$k40475$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40476, %struct.ScmObj* %args49392$k40475$0)
store volatile %struct.ScmObj* %args49392$k40475$1, %struct.ScmObj** %stackaddr$prim50547, align 8
%stackaddr$prim50548 = alloca %struct.ScmObj*, align 8
%args49392$k40475$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42311, %struct.ScmObj* %args49392$k40475$1)
store volatile %struct.ScmObj* %args49392$k40475$2, %struct.ScmObj** %stackaddr$prim50548, align 8
%clofunc50549 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40475)
musttail call tailcc void %clofunc50549(%struct.ScmObj* %k40475, %struct.ScmObj* %args49392$k40475$2)
ret void
}

define tailcc void @proc_clo$ae42282(%struct.ScmObj* %env$ae42282,%struct.ScmObj* %current_45args49394) {
%stackaddr$prim50550 = alloca %struct.ScmObj*, align 8
%k40477 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49394)
store volatile %struct.ScmObj* %k40477, %struct.ScmObj** %stackaddr$prim50550, align 8
%stackaddr$prim50551 = alloca %struct.ScmObj*, align 8
%current_45args49395 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49394)
store volatile %struct.ScmObj* %current_45args49395, %struct.ScmObj** %stackaddr$prim50551, align 8
%stackaddr$prim50552 = alloca %struct.ScmObj*, align 8
%a40222 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49395)
store volatile %struct.ScmObj* %a40222, %struct.ScmObj** %stackaddr$prim50552, align 8
%stackaddr$prim50553 = alloca %struct.ScmObj*, align 8
%current_45args49396 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49395)
store volatile %struct.ScmObj* %current_45args49396, %struct.ScmObj** %stackaddr$prim50553, align 8
%stackaddr$prim50554 = alloca %struct.ScmObj*, align 8
%b40221 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49396)
store volatile %struct.ScmObj* %b40221, %struct.ScmObj** %stackaddr$prim50554, align 8
%stackaddr$prim50555 = alloca %struct.ScmObj*, align 8
%anf_45bind40344 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a40222, %struct.ScmObj* %b40221)
store volatile %struct.ScmObj* %anf_45bind40344, %struct.ScmObj** %stackaddr$prim50555, align 8
%stackaddr$prim50556 = alloca %struct.ScmObj*, align 8
%cpsprim40478 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind40344)
store volatile %struct.ScmObj* %cpsprim40478, %struct.ScmObj** %stackaddr$prim50556, align 8
%ae42287 = call %struct.ScmObj* @const_init_int(i64 0)
%args49398$k40477$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50557 = alloca %struct.ScmObj*, align 8
%args49398$k40477$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40478, %struct.ScmObj* %args49398$k40477$0)
store volatile %struct.ScmObj* %args49398$k40477$1, %struct.ScmObj** %stackaddr$prim50557, align 8
%stackaddr$prim50558 = alloca %struct.ScmObj*, align 8
%args49398$k40477$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42287, %struct.ScmObj* %args49398$k40477$1)
store volatile %struct.ScmObj* %args49398$k40477$2, %struct.ScmObj** %stackaddr$prim50558, align 8
%clofunc50559 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40477)
musttail call tailcc void %clofunc50559(%struct.ScmObj* %k40477, %struct.ScmObj* %args49398$k40477$2)
ret void
}

define tailcc void @proc_clo$ae41888(%struct.ScmObj* %env$ae41888,%struct.ScmObj* %current_45args49401) {
%stackaddr$env-ref50560 = alloca %struct.ScmObj*, align 8
%_37foldr40146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41888, i64 0)
store %struct.ScmObj* %_37foldr40146, %struct.ScmObj** %stackaddr$env-ref50560
%stackaddr$env-ref50561 = alloca %struct.ScmObj*, align 8
%_37foldr140141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41888, i64 1)
store %struct.ScmObj* %_37foldr140141, %struct.ScmObj** %stackaddr$env-ref50561
%stackaddr$env-ref50562 = alloca %struct.ScmObj*, align 8
%_37map140172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41888, i64 2)
store %struct.ScmObj* %_37map140172, %struct.ScmObj** %stackaddr$env-ref50562
%stackaddr$prim50563 = alloca %struct.ScmObj*, align 8
%k40479 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49401)
store volatile %struct.ScmObj* %k40479, %struct.ScmObj** %stackaddr$prim50563, align 8
%stackaddr$prim50564 = alloca %struct.ScmObj*, align 8
%current_45args49402 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49401)
store volatile %struct.ScmObj* %current_45args49402, %struct.ScmObj** %stackaddr$prim50564, align 8
%stackaddr$prim50565 = alloca %struct.ScmObj*, align 8
%_37foldl40224 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49402)
store volatile %struct.ScmObj* %_37foldl40224, %struct.ScmObj** %stackaddr$prim50565, align 8
%ae41890 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50566 = alloca %struct.ScmObj*, align 8
%fptrToInt50567 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41891 to i64
%ae41891 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt50567)
store volatile %struct.ScmObj* %ae41891, %struct.ScmObj** %stackaddr$makeclosure50566, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41891, %struct.ScmObj* %_37foldr40146, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41891, %struct.ScmObj* %_37foldl40224, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41891, %struct.ScmObj* %_37foldr140141, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41891, %struct.ScmObj* %_37map140172, i64 3)
%args49459$k40479$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50568 = alloca %struct.ScmObj*, align 8
%args49459$k40479$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41891, %struct.ScmObj* %args49459$k40479$0)
store volatile %struct.ScmObj* %args49459$k40479$1, %struct.ScmObj** %stackaddr$prim50568, align 8
%stackaddr$prim50569 = alloca %struct.ScmObj*, align 8
%args49459$k40479$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41890, %struct.ScmObj* %args49459$k40479$1)
store volatile %struct.ScmObj* %args49459$k40479$2, %struct.ScmObj** %stackaddr$prim50569, align 8
%clofunc50570 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40479)
musttail call tailcc void %clofunc50570(%struct.ScmObj* %k40479, %struct.ScmObj* %args49459$k40479$2)
ret void
}

define tailcc void @proc_clo$ae41891(%struct.ScmObj* %env$ae41891,%struct.ScmObj* %args4022540480) {
%stackaddr$env-ref50571 = alloca %struct.ScmObj*, align 8
%_37foldr40146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41891, i64 0)
store %struct.ScmObj* %_37foldr40146, %struct.ScmObj** %stackaddr$env-ref50571
%stackaddr$env-ref50572 = alloca %struct.ScmObj*, align 8
%_37foldl40224 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41891, i64 1)
store %struct.ScmObj* %_37foldl40224, %struct.ScmObj** %stackaddr$env-ref50572
%stackaddr$env-ref50573 = alloca %struct.ScmObj*, align 8
%_37foldr140141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41891, i64 2)
store %struct.ScmObj* %_37foldr140141, %struct.ScmObj** %stackaddr$env-ref50573
%stackaddr$env-ref50574 = alloca %struct.ScmObj*, align 8
%_37map140172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41891, i64 3)
store %struct.ScmObj* %_37map140172, %struct.ScmObj** %stackaddr$env-ref50574
%stackaddr$prim50575 = alloca %struct.ScmObj*, align 8
%k40481 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4022540480)
store volatile %struct.ScmObj* %k40481, %struct.ScmObj** %stackaddr$prim50575, align 8
%stackaddr$prim50576 = alloca %struct.ScmObj*, align 8
%args40225 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4022540480)
store volatile %struct.ScmObj* %args40225, %struct.ScmObj** %stackaddr$prim50576, align 8
%stackaddr$prim50577 = alloca %struct.ScmObj*, align 8
%f40228 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40225)
store volatile %struct.ScmObj* %f40228, %struct.ScmObj** %stackaddr$prim50577, align 8
%stackaddr$prim50578 = alloca %struct.ScmObj*, align 8
%anf_45bind40332 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40225)
store volatile %struct.ScmObj* %anf_45bind40332, %struct.ScmObj** %stackaddr$prim50578, align 8
%stackaddr$prim50579 = alloca %struct.ScmObj*, align 8
%acc40227 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40332)
store volatile %struct.ScmObj* %acc40227, %struct.ScmObj** %stackaddr$prim50579, align 8
%stackaddr$prim50580 = alloca %struct.ScmObj*, align 8
%anf_45bind40333 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40225)
store volatile %struct.ScmObj* %anf_45bind40333, %struct.ScmObj** %stackaddr$prim50580, align 8
%stackaddr$prim50581 = alloca %struct.ScmObj*, align 8
%lsts40226 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40333)
store volatile %struct.ScmObj* %lsts40226, %struct.ScmObj** %stackaddr$prim50581, align 8
%stackaddr$makeclosure50582 = alloca %struct.ScmObj*, align 8
%fptrToInt50583 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41899 to i64
%ae41899 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt50583)
store volatile %struct.ScmObj* %ae41899, %struct.ScmObj** %stackaddr$makeclosure50582, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41899, %struct.ScmObj* %lsts40226, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41899, %struct.ScmObj* %_37foldr40146, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41899, %struct.ScmObj* %f40228, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41899, %struct.ScmObj* %acc40227, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41899, %struct.ScmObj* %k40481, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41899, %struct.ScmObj* %_37foldl40224, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41899, %struct.ScmObj* %_37foldr140141, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae41899, %struct.ScmObj* %_37map140172, i64 7)
%ae41900 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50584 = alloca %struct.ScmObj*, align 8
%fptrToInt50585 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41901 to i64
%ae41901 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50585)
store volatile %struct.ScmObj* %ae41901, %struct.ScmObj** %stackaddr$makeclosure50584, align 8
%args49458$ae41899$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50586 = alloca %struct.ScmObj*, align 8
%args49458$ae41899$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41901, %struct.ScmObj* %args49458$ae41899$0)
store volatile %struct.ScmObj* %args49458$ae41899$1, %struct.ScmObj** %stackaddr$prim50586, align 8
%stackaddr$prim50587 = alloca %struct.ScmObj*, align 8
%args49458$ae41899$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41900, %struct.ScmObj* %args49458$ae41899$1)
store volatile %struct.ScmObj* %args49458$ae41899$2, %struct.ScmObj** %stackaddr$prim50587, align 8
%clofunc50588 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41899)
musttail call tailcc void %clofunc50588(%struct.ScmObj* %ae41899, %struct.ScmObj* %args49458$ae41899$2)
ret void
}

define tailcc void @proc_clo$ae41899(%struct.ScmObj* %env$ae41899,%struct.ScmObj* %current_45args49404) {
%stackaddr$env-ref50589 = alloca %struct.ScmObj*, align 8
%lsts40226 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41899, i64 0)
store %struct.ScmObj* %lsts40226, %struct.ScmObj** %stackaddr$env-ref50589
%stackaddr$env-ref50590 = alloca %struct.ScmObj*, align 8
%_37foldr40146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41899, i64 1)
store %struct.ScmObj* %_37foldr40146, %struct.ScmObj** %stackaddr$env-ref50590
%stackaddr$env-ref50591 = alloca %struct.ScmObj*, align 8
%f40228 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41899, i64 2)
store %struct.ScmObj* %f40228, %struct.ScmObj** %stackaddr$env-ref50591
%stackaddr$env-ref50592 = alloca %struct.ScmObj*, align 8
%acc40227 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41899, i64 3)
store %struct.ScmObj* %acc40227, %struct.ScmObj** %stackaddr$env-ref50592
%stackaddr$env-ref50593 = alloca %struct.ScmObj*, align 8
%k40481 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41899, i64 4)
store %struct.ScmObj* %k40481, %struct.ScmObj** %stackaddr$env-ref50593
%stackaddr$env-ref50594 = alloca %struct.ScmObj*, align 8
%_37foldl40224 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41899, i64 5)
store %struct.ScmObj* %_37foldl40224, %struct.ScmObj** %stackaddr$env-ref50594
%stackaddr$env-ref50595 = alloca %struct.ScmObj*, align 8
%_37foldr140141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41899, i64 6)
store %struct.ScmObj* %_37foldr140141, %struct.ScmObj** %stackaddr$env-ref50595
%stackaddr$env-ref50596 = alloca %struct.ScmObj*, align 8
%_37map140172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41899, i64 7)
store %struct.ScmObj* %_37map140172, %struct.ScmObj** %stackaddr$env-ref50596
%stackaddr$prim50597 = alloca %struct.ScmObj*, align 8
%_95k40482 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49404)
store volatile %struct.ScmObj* %_95k40482, %struct.ScmObj** %stackaddr$prim50597, align 8
%stackaddr$prim50598 = alloca %struct.ScmObj*, align 8
%current_45args49405 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49404)
store volatile %struct.ScmObj* %current_45args49405, %struct.ScmObj** %stackaddr$prim50598, align 8
%stackaddr$prim50599 = alloca %struct.ScmObj*, align 8
%anf_45bind40334 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49405)
store volatile %struct.ScmObj* %anf_45bind40334, %struct.ScmObj** %stackaddr$prim50599, align 8
%stackaddr$makeclosure50600 = alloca %struct.ScmObj*, align 8
%fptrToInt50601 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41931 to i64
%ae41931 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt50601)
store volatile %struct.ScmObj* %ae41931, %struct.ScmObj** %stackaddr$makeclosure50600, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41931, %struct.ScmObj* %lsts40226, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41931, %struct.ScmObj* %_37foldr40146, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41931, %struct.ScmObj* %f40228, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41931, %struct.ScmObj* %acc40227, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41931, %struct.ScmObj* %k40481, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41931, %struct.ScmObj* %_37foldl40224, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41931, %struct.ScmObj* %_37map140172, i64 6)
%ae41933 = call %struct.ScmObj* @const_init_false()
%args49451$_37foldr140141$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50602 = alloca %struct.ScmObj*, align 8
%args49451$_37foldr140141$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40226, %struct.ScmObj* %args49451$_37foldr140141$0)
store volatile %struct.ScmObj* %args49451$_37foldr140141$1, %struct.ScmObj** %stackaddr$prim50602, align 8
%stackaddr$prim50603 = alloca %struct.ScmObj*, align 8
%args49451$_37foldr140141$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41933, %struct.ScmObj* %args49451$_37foldr140141$1)
store volatile %struct.ScmObj* %args49451$_37foldr140141$2, %struct.ScmObj** %stackaddr$prim50603, align 8
%stackaddr$prim50604 = alloca %struct.ScmObj*, align 8
%args49451$_37foldr140141$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40334, %struct.ScmObj* %args49451$_37foldr140141$2)
store volatile %struct.ScmObj* %args49451$_37foldr140141$3, %struct.ScmObj** %stackaddr$prim50604, align 8
%stackaddr$prim50605 = alloca %struct.ScmObj*, align 8
%args49451$_37foldr140141$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41931, %struct.ScmObj* %args49451$_37foldr140141$3)
store volatile %struct.ScmObj* %args49451$_37foldr140141$4, %struct.ScmObj** %stackaddr$prim50605, align 8
%clofunc50606 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140141)
musttail call tailcc void %clofunc50606(%struct.ScmObj* %_37foldr140141, %struct.ScmObj* %args49451$_37foldr140141$4)
ret void
}

define tailcc void @proc_clo$ae41931(%struct.ScmObj* %env$ae41931,%struct.ScmObj* %current_45args49407) {
%stackaddr$env-ref50607 = alloca %struct.ScmObj*, align 8
%lsts40226 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41931, i64 0)
store %struct.ScmObj* %lsts40226, %struct.ScmObj** %stackaddr$env-ref50607
%stackaddr$env-ref50608 = alloca %struct.ScmObj*, align 8
%_37foldr40146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41931, i64 1)
store %struct.ScmObj* %_37foldr40146, %struct.ScmObj** %stackaddr$env-ref50608
%stackaddr$env-ref50609 = alloca %struct.ScmObj*, align 8
%f40228 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41931, i64 2)
store %struct.ScmObj* %f40228, %struct.ScmObj** %stackaddr$env-ref50609
%stackaddr$env-ref50610 = alloca %struct.ScmObj*, align 8
%acc40227 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41931, i64 3)
store %struct.ScmObj* %acc40227, %struct.ScmObj** %stackaddr$env-ref50610
%stackaddr$env-ref50611 = alloca %struct.ScmObj*, align 8
%k40481 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41931, i64 4)
store %struct.ScmObj* %k40481, %struct.ScmObj** %stackaddr$env-ref50611
%stackaddr$env-ref50612 = alloca %struct.ScmObj*, align 8
%_37foldl40224 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41931, i64 5)
store %struct.ScmObj* %_37foldl40224, %struct.ScmObj** %stackaddr$env-ref50612
%stackaddr$env-ref50613 = alloca %struct.ScmObj*, align 8
%_37map140172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41931, i64 6)
store %struct.ScmObj* %_37map140172, %struct.ScmObj** %stackaddr$env-ref50613
%stackaddr$prim50614 = alloca %struct.ScmObj*, align 8
%_95k40483 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49407)
store volatile %struct.ScmObj* %_95k40483, %struct.ScmObj** %stackaddr$prim50614, align 8
%stackaddr$prim50615 = alloca %struct.ScmObj*, align 8
%current_45args49408 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49407)
store volatile %struct.ScmObj* %current_45args49408, %struct.ScmObj** %stackaddr$prim50615, align 8
%stackaddr$prim50616 = alloca %struct.ScmObj*, align 8
%anf_45bind40335 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49408)
store volatile %struct.ScmObj* %anf_45bind40335, %struct.ScmObj** %stackaddr$prim50616, align 8
%truthy$cmp50617 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40335)
%cmp$cmp50617 = icmp eq i64 %truthy$cmp50617, 1
br i1 %cmp$cmp50617, label %truebranch$cmp50617, label %falsebranch$cmp50617
truebranch$cmp50617:
%ae41942 = call %struct.ScmObj* @const_init_int(i64 0)
%args49410$k40481$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50618 = alloca %struct.ScmObj*, align 8
%args49410$k40481$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40227, %struct.ScmObj* %args49410$k40481$0)
store volatile %struct.ScmObj* %args49410$k40481$1, %struct.ScmObj** %stackaddr$prim50618, align 8
%stackaddr$prim50619 = alloca %struct.ScmObj*, align 8
%args49410$k40481$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41942, %struct.ScmObj* %args49410$k40481$1)
store volatile %struct.ScmObj* %args49410$k40481$2, %struct.ScmObj** %stackaddr$prim50619, align 8
%clofunc50620 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40481)
musttail call tailcc void %clofunc50620(%struct.ScmObj* %k40481, %struct.ScmObj* %args49410$k40481$2)
ret void
falsebranch$cmp50617:
%stackaddr$makeclosure50621 = alloca %struct.ScmObj*, align 8
%fptrToInt50622 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41947 to i64
%ae41947 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt50622)
store volatile %struct.ScmObj* %ae41947, %struct.ScmObj** %stackaddr$makeclosure50621, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41947, %struct.ScmObj* %lsts40226, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41947, %struct.ScmObj* %_37foldr40146, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41947, %struct.ScmObj* %f40228, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41947, %struct.ScmObj* %acc40227, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41947, %struct.ScmObj* %k40481, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41947, %struct.ScmObj* %_37foldl40224, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41947, %struct.ScmObj* %_37map140172, i64 6)
%ae41948 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50623 = alloca %struct.ScmObj*, align 8
%fptrToInt50624 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41949 to i64
%ae41949 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50624)
store volatile %struct.ScmObj* %ae41949, %struct.ScmObj** %stackaddr$makeclosure50623, align 8
%args49450$ae41947$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50625 = alloca %struct.ScmObj*, align 8
%args49450$ae41947$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41949, %struct.ScmObj* %args49450$ae41947$0)
store volatile %struct.ScmObj* %args49450$ae41947$1, %struct.ScmObj** %stackaddr$prim50625, align 8
%stackaddr$prim50626 = alloca %struct.ScmObj*, align 8
%args49450$ae41947$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41948, %struct.ScmObj* %args49450$ae41947$1)
store volatile %struct.ScmObj* %args49450$ae41947$2, %struct.ScmObj** %stackaddr$prim50626, align 8
%clofunc50627 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41947)
musttail call tailcc void %clofunc50627(%struct.ScmObj* %ae41947, %struct.ScmObj* %args49450$ae41947$2)
ret void
}

define tailcc void @proc_clo$ae41947(%struct.ScmObj* %env$ae41947,%struct.ScmObj* %current_45args49411) {
%stackaddr$env-ref50628 = alloca %struct.ScmObj*, align 8
%lsts40226 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41947, i64 0)
store %struct.ScmObj* %lsts40226, %struct.ScmObj** %stackaddr$env-ref50628
%stackaddr$env-ref50629 = alloca %struct.ScmObj*, align 8
%_37foldr40146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41947, i64 1)
store %struct.ScmObj* %_37foldr40146, %struct.ScmObj** %stackaddr$env-ref50629
%stackaddr$env-ref50630 = alloca %struct.ScmObj*, align 8
%f40228 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41947, i64 2)
store %struct.ScmObj* %f40228, %struct.ScmObj** %stackaddr$env-ref50630
%stackaddr$env-ref50631 = alloca %struct.ScmObj*, align 8
%acc40227 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41947, i64 3)
store %struct.ScmObj* %acc40227, %struct.ScmObj** %stackaddr$env-ref50631
%stackaddr$env-ref50632 = alloca %struct.ScmObj*, align 8
%k40481 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41947, i64 4)
store %struct.ScmObj* %k40481, %struct.ScmObj** %stackaddr$env-ref50632
%stackaddr$env-ref50633 = alloca %struct.ScmObj*, align 8
%_37foldl40224 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41947, i64 5)
store %struct.ScmObj* %_37foldl40224, %struct.ScmObj** %stackaddr$env-ref50633
%stackaddr$env-ref50634 = alloca %struct.ScmObj*, align 8
%_37map140172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41947, i64 6)
store %struct.ScmObj* %_37map140172, %struct.ScmObj** %stackaddr$env-ref50634
%stackaddr$prim50635 = alloca %struct.ScmObj*, align 8
%_95k40484 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49411)
store volatile %struct.ScmObj* %_95k40484, %struct.ScmObj** %stackaddr$prim50635, align 8
%stackaddr$prim50636 = alloca %struct.ScmObj*, align 8
%current_45args49412 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49411)
store volatile %struct.ScmObj* %current_45args49412, %struct.ScmObj** %stackaddr$prim50636, align 8
%stackaddr$prim50637 = alloca %struct.ScmObj*, align 8
%anf_45bind40336 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49412)
store volatile %struct.ScmObj* %anf_45bind40336, %struct.ScmObj** %stackaddr$prim50637, align 8
%stackaddr$makeclosure50638 = alloca %struct.ScmObj*, align 8
%fptrToInt50639 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41968 to i64
%ae41968 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt50639)
store volatile %struct.ScmObj* %ae41968, %struct.ScmObj** %stackaddr$makeclosure50638, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41968, %struct.ScmObj* %lsts40226, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41968, %struct.ScmObj* %_37foldr40146, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41968, %struct.ScmObj* %f40228, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41968, %struct.ScmObj* %acc40227, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41968, %struct.ScmObj* %k40481, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41968, %struct.ScmObj* %_37foldl40224, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41968, %struct.ScmObj* %_37map140172, i64 6)
%args49445$_37map140172$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50640 = alloca %struct.ScmObj*, align 8
%args49445$_37map140172$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40226, %struct.ScmObj* %args49445$_37map140172$0)
store volatile %struct.ScmObj* %args49445$_37map140172$1, %struct.ScmObj** %stackaddr$prim50640, align 8
%stackaddr$prim50641 = alloca %struct.ScmObj*, align 8
%args49445$_37map140172$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40336, %struct.ScmObj* %args49445$_37map140172$1)
store volatile %struct.ScmObj* %args49445$_37map140172$2, %struct.ScmObj** %stackaddr$prim50641, align 8
%stackaddr$prim50642 = alloca %struct.ScmObj*, align 8
%args49445$_37map140172$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41968, %struct.ScmObj* %args49445$_37map140172$2)
store volatile %struct.ScmObj* %args49445$_37map140172$3, %struct.ScmObj** %stackaddr$prim50642, align 8
%clofunc50643 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140172)
musttail call tailcc void %clofunc50643(%struct.ScmObj* %_37map140172, %struct.ScmObj* %args49445$_37map140172$3)
ret void
}

define tailcc void @proc_clo$ae41968(%struct.ScmObj* %env$ae41968,%struct.ScmObj* %current_45args49414) {
%stackaddr$env-ref50644 = alloca %struct.ScmObj*, align 8
%lsts40226 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41968, i64 0)
store %struct.ScmObj* %lsts40226, %struct.ScmObj** %stackaddr$env-ref50644
%stackaddr$env-ref50645 = alloca %struct.ScmObj*, align 8
%_37foldr40146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41968, i64 1)
store %struct.ScmObj* %_37foldr40146, %struct.ScmObj** %stackaddr$env-ref50645
%stackaddr$env-ref50646 = alloca %struct.ScmObj*, align 8
%f40228 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41968, i64 2)
store %struct.ScmObj* %f40228, %struct.ScmObj** %stackaddr$env-ref50646
%stackaddr$env-ref50647 = alloca %struct.ScmObj*, align 8
%acc40227 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41968, i64 3)
store %struct.ScmObj* %acc40227, %struct.ScmObj** %stackaddr$env-ref50647
%stackaddr$env-ref50648 = alloca %struct.ScmObj*, align 8
%k40481 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41968, i64 4)
store %struct.ScmObj* %k40481, %struct.ScmObj** %stackaddr$env-ref50648
%stackaddr$env-ref50649 = alloca %struct.ScmObj*, align 8
%_37foldl40224 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41968, i64 5)
store %struct.ScmObj* %_37foldl40224, %struct.ScmObj** %stackaddr$env-ref50649
%stackaddr$env-ref50650 = alloca %struct.ScmObj*, align 8
%_37map140172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41968, i64 6)
store %struct.ScmObj* %_37map140172, %struct.ScmObj** %stackaddr$env-ref50650
%stackaddr$prim50651 = alloca %struct.ScmObj*, align 8
%_95k40485 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49414)
store volatile %struct.ScmObj* %_95k40485, %struct.ScmObj** %stackaddr$prim50651, align 8
%stackaddr$prim50652 = alloca %struct.ScmObj*, align 8
%current_45args49415 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49414)
store volatile %struct.ScmObj* %current_45args49415, %struct.ScmObj** %stackaddr$prim50652, align 8
%stackaddr$prim50653 = alloca %struct.ScmObj*, align 8
%lsts_4340233 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49415)
store volatile %struct.ScmObj* %lsts_4340233, %struct.ScmObj** %stackaddr$prim50653, align 8
%stackaddr$makeclosure50654 = alloca %struct.ScmObj*, align 8
%fptrToInt50655 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41971 to i64
%ae41971 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt50655)
store volatile %struct.ScmObj* %ae41971, %struct.ScmObj** %stackaddr$makeclosure50654, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41971, %struct.ScmObj* %lsts40226, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41971, %struct.ScmObj* %_37foldr40146, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41971, %struct.ScmObj* %f40228, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41971, %struct.ScmObj* %acc40227, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41971, %struct.ScmObj* %k40481, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41971, %struct.ScmObj* %_37foldl40224, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41971, %struct.ScmObj* %_37map140172, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae41971, %struct.ScmObj* %lsts_4340233, i64 7)
%ae41972 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50656 = alloca %struct.ScmObj*, align 8
%fptrToInt50657 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41973 to i64
%ae41973 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50657)
store volatile %struct.ScmObj* %ae41973, %struct.ScmObj** %stackaddr$makeclosure50656, align 8
%args49444$ae41971$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50658 = alloca %struct.ScmObj*, align 8
%args49444$ae41971$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41973, %struct.ScmObj* %args49444$ae41971$0)
store volatile %struct.ScmObj* %args49444$ae41971$1, %struct.ScmObj** %stackaddr$prim50658, align 8
%stackaddr$prim50659 = alloca %struct.ScmObj*, align 8
%args49444$ae41971$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41972, %struct.ScmObj* %args49444$ae41971$1)
store volatile %struct.ScmObj* %args49444$ae41971$2, %struct.ScmObj** %stackaddr$prim50659, align 8
%clofunc50660 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41971)
musttail call tailcc void %clofunc50660(%struct.ScmObj* %ae41971, %struct.ScmObj* %args49444$ae41971$2)
ret void
}

define tailcc void @proc_clo$ae41971(%struct.ScmObj* %env$ae41971,%struct.ScmObj* %current_45args49417) {
%stackaddr$env-ref50661 = alloca %struct.ScmObj*, align 8
%lsts40226 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41971, i64 0)
store %struct.ScmObj* %lsts40226, %struct.ScmObj** %stackaddr$env-ref50661
%stackaddr$env-ref50662 = alloca %struct.ScmObj*, align 8
%_37foldr40146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41971, i64 1)
store %struct.ScmObj* %_37foldr40146, %struct.ScmObj** %stackaddr$env-ref50662
%stackaddr$env-ref50663 = alloca %struct.ScmObj*, align 8
%f40228 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41971, i64 2)
store %struct.ScmObj* %f40228, %struct.ScmObj** %stackaddr$env-ref50663
%stackaddr$env-ref50664 = alloca %struct.ScmObj*, align 8
%acc40227 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41971, i64 3)
store %struct.ScmObj* %acc40227, %struct.ScmObj** %stackaddr$env-ref50664
%stackaddr$env-ref50665 = alloca %struct.ScmObj*, align 8
%k40481 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41971, i64 4)
store %struct.ScmObj* %k40481, %struct.ScmObj** %stackaddr$env-ref50665
%stackaddr$env-ref50666 = alloca %struct.ScmObj*, align 8
%_37foldl40224 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41971, i64 5)
store %struct.ScmObj* %_37foldl40224, %struct.ScmObj** %stackaddr$env-ref50666
%stackaddr$env-ref50667 = alloca %struct.ScmObj*, align 8
%_37map140172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41971, i64 6)
store %struct.ScmObj* %_37map140172, %struct.ScmObj** %stackaddr$env-ref50667
%stackaddr$env-ref50668 = alloca %struct.ScmObj*, align 8
%lsts_4340233 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41971, i64 7)
store %struct.ScmObj* %lsts_4340233, %struct.ScmObj** %stackaddr$env-ref50668
%stackaddr$prim50669 = alloca %struct.ScmObj*, align 8
%_95k40486 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49417)
store volatile %struct.ScmObj* %_95k40486, %struct.ScmObj** %stackaddr$prim50669, align 8
%stackaddr$prim50670 = alloca %struct.ScmObj*, align 8
%current_45args49418 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49417)
store volatile %struct.ScmObj* %current_45args49418, %struct.ScmObj** %stackaddr$prim50670, align 8
%stackaddr$prim50671 = alloca %struct.ScmObj*, align 8
%anf_45bind40337 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49418)
store volatile %struct.ScmObj* %anf_45bind40337, %struct.ScmObj** %stackaddr$prim50671, align 8
%stackaddr$makeclosure50672 = alloca %struct.ScmObj*, align 8
%fptrToInt50673 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41992 to i64
%ae41992 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt50673)
store volatile %struct.ScmObj* %ae41992, %struct.ScmObj** %stackaddr$makeclosure50672, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41992, %struct.ScmObj* %f40228, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41992, %struct.ScmObj* %acc40227, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41992, %struct.ScmObj* %_37foldr40146, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41992, %struct.ScmObj* %k40481, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41992, %struct.ScmObj* %_37foldl40224, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41992, %struct.ScmObj* %lsts_4340233, i64 5)
%args49439$_37map140172$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50674 = alloca %struct.ScmObj*, align 8
%args49439$_37map140172$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40226, %struct.ScmObj* %args49439$_37map140172$0)
store volatile %struct.ScmObj* %args49439$_37map140172$1, %struct.ScmObj** %stackaddr$prim50674, align 8
%stackaddr$prim50675 = alloca %struct.ScmObj*, align 8
%args49439$_37map140172$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40337, %struct.ScmObj* %args49439$_37map140172$1)
store volatile %struct.ScmObj* %args49439$_37map140172$2, %struct.ScmObj** %stackaddr$prim50675, align 8
%stackaddr$prim50676 = alloca %struct.ScmObj*, align 8
%args49439$_37map140172$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41992, %struct.ScmObj* %args49439$_37map140172$2)
store volatile %struct.ScmObj* %args49439$_37map140172$3, %struct.ScmObj** %stackaddr$prim50676, align 8
%clofunc50677 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140172)
musttail call tailcc void %clofunc50677(%struct.ScmObj* %_37map140172, %struct.ScmObj* %args49439$_37map140172$3)
ret void
}

define tailcc void @proc_clo$ae41992(%struct.ScmObj* %env$ae41992,%struct.ScmObj* %current_45args49420) {
%stackaddr$env-ref50678 = alloca %struct.ScmObj*, align 8
%f40228 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41992, i64 0)
store %struct.ScmObj* %f40228, %struct.ScmObj** %stackaddr$env-ref50678
%stackaddr$env-ref50679 = alloca %struct.ScmObj*, align 8
%acc40227 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41992, i64 1)
store %struct.ScmObj* %acc40227, %struct.ScmObj** %stackaddr$env-ref50679
%stackaddr$env-ref50680 = alloca %struct.ScmObj*, align 8
%_37foldr40146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41992, i64 2)
store %struct.ScmObj* %_37foldr40146, %struct.ScmObj** %stackaddr$env-ref50680
%stackaddr$env-ref50681 = alloca %struct.ScmObj*, align 8
%k40481 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41992, i64 3)
store %struct.ScmObj* %k40481, %struct.ScmObj** %stackaddr$env-ref50681
%stackaddr$env-ref50682 = alloca %struct.ScmObj*, align 8
%_37foldl40224 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41992, i64 4)
store %struct.ScmObj* %_37foldl40224, %struct.ScmObj** %stackaddr$env-ref50682
%stackaddr$env-ref50683 = alloca %struct.ScmObj*, align 8
%lsts_4340233 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41992, i64 5)
store %struct.ScmObj* %lsts_4340233, %struct.ScmObj** %stackaddr$env-ref50683
%stackaddr$prim50684 = alloca %struct.ScmObj*, align 8
%_95k40487 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49420)
store volatile %struct.ScmObj* %_95k40487, %struct.ScmObj** %stackaddr$prim50684, align 8
%stackaddr$prim50685 = alloca %struct.ScmObj*, align 8
%current_45args49421 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49420)
store volatile %struct.ScmObj* %current_45args49421, %struct.ScmObj** %stackaddr$prim50685, align 8
%stackaddr$prim50686 = alloca %struct.ScmObj*, align 8
%vs40231 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49421)
store volatile %struct.ScmObj* %vs40231, %struct.ScmObj** %stackaddr$prim50686, align 8
%stackaddr$makeclosure50687 = alloca %struct.ScmObj*, align 8
%fptrToInt50688 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41995 to i64
%ae41995 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt50688)
store volatile %struct.ScmObj* %ae41995, %struct.ScmObj** %stackaddr$makeclosure50687, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41995, %struct.ScmObj* %vs40231, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41995, %struct.ScmObj* %f40228, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41995, %struct.ScmObj* %acc40227, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41995, %struct.ScmObj* %_37foldr40146, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41995, %struct.ScmObj* %k40481, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41995, %struct.ScmObj* %_37foldl40224, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41995, %struct.ScmObj* %lsts_4340233, i64 6)
%ae41996 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50689 = alloca %struct.ScmObj*, align 8
%fptrToInt50690 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41997 to i64
%ae41997 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50690)
store volatile %struct.ScmObj* %ae41997, %struct.ScmObj** %stackaddr$makeclosure50689, align 8
%args49438$ae41995$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50691 = alloca %struct.ScmObj*, align 8
%args49438$ae41995$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41997, %struct.ScmObj* %args49438$ae41995$0)
store volatile %struct.ScmObj* %args49438$ae41995$1, %struct.ScmObj** %stackaddr$prim50691, align 8
%stackaddr$prim50692 = alloca %struct.ScmObj*, align 8
%args49438$ae41995$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41996, %struct.ScmObj* %args49438$ae41995$1)
store volatile %struct.ScmObj* %args49438$ae41995$2, %struct.ScmObj** %stackaddr$prim50692, align 8
%clofunc50693 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41995)
musttail call tailcc void %clofunc50693(%struct.ScmObj* %ae41995, %struct.ScmObj* %args49438$ae41995$2)
ret void
}

define tailcc void @proc_clo$ae41995(%struct.ScmObj* %env$ae41995,%struct.ScmObj* %current_45args49423) {
%stackaddr$env-ref50694 = alloca %struct.ScmObj*, align 8
%vs40231 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41995, i64 0)
store %struct.ScmObj* %vs40231, %struct.ScmObj** %stackaddr$env-ref50694
%stackaddr$env-ref50695 = alloca %struct.ScmObj*, align 8
%f40228 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41995, i64 1)
store %struct.ScmObj* %f40228, %struct.ScmObj** %stackaddr$env-ref50695
%stackaddr$env-ref50696 = alloca %struct.ScmObj*, align 8
%acc40227 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41995, i64 2)
store %struct.ScmObj* %acc40227, %struct.ScmObj** %stackaddr$env-ref50696
%stackaddr$env-ref50697 = alloca %struct.ScmObj*, align 8
%_37foldr40146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41995, i64 3)
store %struct.ScmObj* %_37foldr40146, %struct.ScmObj** %stackaddr$env-ref50697
%stackaddr$env-ref50698 = alloca %struct.ScmObj*, align 8
%k40481 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41995, i64 4)
store %struct.ScmObj* %k40481, %struct.ScmObj** %stackaddr$env-ref50698
%stackaddr$env-ref50699 = alloca %struct.ScmObj*, align 8
%_37foldl40224 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41995, i64 5)
store %struct.ScmObj* %_37foldl40224, %struct.ScmObj** %stackaddr$env-ref50699
%stackaddr$env-ref50700 = alloca %struct.ScmObj*, align 8
%lsts_4340233 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41995, i64 6)
store %struct.ScmObj* %lsts_4340233, %struct.ScmObj** %stackaddr$env-ref50700
%stackaddr$prim50701 = alloca %struct.ScmObj*, align 8
%_95k40488 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49423)
store volatile %struct.ScmObj* %_95k40488, %struct.ScmObj** %stackaddr$prim50701, align 8
%stackaddr$prim50702 = alloca %struct.ScmObj*, align 8
%current_45args49424 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49423)
store volatile %struct.ScmObj* %current_45args49424, %struct.ScmObj** %stackaddr$prim50702, align 8
%stackaddr$prim50703 = alloca %struct.ScmObj*, align 8
%anf_45bind40338 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49424)
store volatile %struct.ScmObj* %anf_45bind40338, %struct.ScmObj** %stackaddr$prim50703, align 8
%ae42018 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50704 = alloca %struct.ScmObj*, align 8
%anf_45bind40339 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40227, %struct.ScmObj* %ae42018)
store volatile %struct.ScmObj* %anf_45bind40339, %struct.ScmObj** %stackaddr$prim50704, align 8
%stackaddr$makeclosure50705 = alloca %struct.ScmObj*, align 8
%fptrToInt50706 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42020 to i64
%ae42020 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt50706)
store volatile %struct.ScmObj* %ae42020, %struct.ScmObj** %stackaddr$makeclosure50705, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42020, %struct.ScmObj* %f40228, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42020, %struct.ScmObj* %k40481, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42020, %struct.ScmObj* %_37foldl40224, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae42020, %struct.ScmObj* %lsts_4340233, i64 3)
%args49432$_37foldr40146$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50707 = alloca %struct.ScmObj*, align 8
%args49432$_37foldr40146$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs40231, %struct.ScmObj* %args49432$_37foldr40146$0)
store volatile %struct.ScmObj* %args49432$_37foldr40146$1, %struct.ScmObj** %stackaddr$prim50707, align 8
%stackaddr$prim50708 = alloca %struct.ScmObj*, align 8
%args49432$_37foldr40146$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40339, %struct.ScmObj* %args49432$_37foldr40146$1)
store volatile %struct.ScmObj* %args49432$_37foldr40146$2, %struct.ScmObj** %stackaddr$prim50708, align 8
%stackaddr$prim50709 = alloca %struct.ScmObj*, align 8
%args49432$_37foldr40146$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40338, %struct.ScmObj* %args49432$_37foldr40146$2)
store volatile %struct.ScmObj* %args49432$_37foldr40146$3, %struct.ScmObj** %stackaddr$prim50709, align 8
%stackaddr$prim50710 = alloca %struct.ScmObj*, align 8
%args49432$_37foldr40146$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42020, %struct.ScmObj* %args49432$_37foldr40146$3)
store volatile %struct.ScmObj* %args49432$_37foldr40146$4, %struct.ScmObj** %stackaddr$prim50710, align 8
%clofunc50711 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr40146)
musttail call tailcc void %clofunc50711(%struct.ScmObj* %_37foldr40146, %struct.ScmObj* %args49432$_37foldr40146$4)
ret void
}

define tailcc void @proc_clo$ae42020(%struct.ScmObj* %env$ae42020,%struct.ScmObj* %current_45args49426) {
%stackaddr$env-ref50712 = alloca %struct.ScmObj*, align 8
%f40228 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42020, i64 0)
store %struct.ScmObj* %f40228, %struct.ScmObj** %stackaddr$env-ref50712
%stackaddr$env-ref50713 = alloca %struct.ScmObj*, align 8
%k40481 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42020, i64 1)
store %struct.ScmObj* %k40481, %struct.ScmObj** %stackaddr$env-ref50713
%stackaddr$env-ref50714 = alloca %struct.ScmObj*, align 8
%_37foldl40224 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42020, i64 2)
store %struct.ScmObj* %_37foldl40224, %struct.ScmObj** %stackaddr$env-ref50714
%stackaddr$env-ref50715 = alloca %struct.ScmObj*, align 8
%lsts_4340233 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42020, i64 3)
store %struct.ScmObj* %lsts_4340233, %struct.ScmObj** %stackaddr$env-ref50715
%stackaddr$prim50716 = alloca %struct.ScmObj*, align 8
%_95k40489 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49426)
store volatile %struct.ScmObj* %_95k40489, %struct.ScmObj** %stackaddr$prim50716, align 8
%stackaddr$prim50717 = alloca %struct.ScmObj*, align 8
%current_45args49427 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49426)
store volatile %struct.ScmObj* %current_45args49427, %struct.ScmObj** %stackaddr$prim50717, align 8
%stackaddr$prim50718 = alloca %struct.ScmObj*, align 8
%anf_45bind40340 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49427)
store volatile %struct.ScmObj* %anf_45bind40340, %struct.ScmObj** %stackaddr$prim50718, align 8
%stackaddr$makeclosure50719 = alloca %struct.ScmObj*, align 8
%fptrToInt50720 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42024 to i64
%ae42024 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt50720)
store volatile %struct.ScmObj* %ae42024, %struct.ScmObj** %stackaddr$makeclosure50719, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42024, %struct.ScmObj* %f40228, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42024, %struct.ScmObj* %k40481, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42024, %struct.ScmObj* %_37foldl40224, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae42024, %struct.ScmObj* %lsts_4340233, i64 3)
%stackaddr$prim50721 = alloca %struct.ScmObj*, align 8
%cpsargs40492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42024, %struct.ScmObj* %anf_45bind40340)
store volatile %struct.ScmObj* %cpsargs40492, %struct.ScmObj** %stackaddr$prim50721, align 8
%clofunc50722 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40228)
musttail call tailcc void %clofunc50722(%struct.ScmObj* %f40228, %struct.ScmObj* %cpsargs40492)
ret void
}

define tailcc void @proc_clo$ae42024(%struct.ScmObj* %env$ae42024,%struct.ScmObj* %current_45args49429) {
%stackaddr$env-ref50723 = alloca %struct.ScmObj*, align 8
%f40228 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42024, i64 0)
store %struct.ScmObj* %f40228, %struct.ScmObj** %stackaddr$env-ref50723
%stackaddr$env-ref50724 = alloca %struct.ScmObj*, align 8
%k40481 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42024, i64 1)
store %struct.ScmObj* %k40481, %struct.ScmObj** %stackaddr$env-ref50724
%stackaddr$env-ref50725 = alloca %struct.ScmObj*, align 8
%_37foldl40224 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42024, i64 2)
store %struct.ScmObj* %_37foldl40224, %struct.ScmObj** %stackaddr$env-ref50725
%stackaddr$env-ref50726 = alloca %struct.ScmObj*, align 8
%lsts_4340233 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42024, i64 3)
store %struct.ScmObj* %lsts_4340233, %struct.ScmObj** %stackaddr$env-ref50726
%stackaddr$prim50727 = alloca %struct.ScmObj*, align 8
%_95k40490 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49429)
store volatile %struct.ScmObj* %_95k40490, %struct.ScmObj** %stackaddr$prim50727, align 8
%stackaddr$prim50728 = alloca %struct.ScmObj*, align 8
%current_45args49430 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49429)
store volatile %struct.ScmObj* %current_45args49430, %struct.ScmObj** %stackaddr$prim50728, align 8
%stackaddr$prim50729 = alloca %struct.ScmObj*, align 8
%acc_4340235 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49430)
store volatile %struct.ScmObj* %acc_4340235, %struct.ScmObj** %stackaddr$prim50729, align 8
%stackaddr$prim50730 = alloca %struct.ScmObj*, align 8
%anf_45bind40341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4340235, %struct.ScmObj* %lsts_4340233)
store volatile %struct.ScmObj* %anf_45bind40341, %struct.ScmObj** %stackaddr$prim50730, align 8
%stackaddr$prim50731 = alloca %struct.ScmObj*, align 8
%anf_45bind40342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40228, %struct.ScmObj* %anf_45bind40341)
store volatile %struct.ScmObj* %anf_45bind40342, %struct.ScmObj** %stackaddr$prim50731, align 8
%stackaddr$prim50732 = alloca %struct.ScmObj*, align 8
%cpsargs40491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40481, %struct.ScmObj* %anf_45bind40342)
store volatile %struct.ScmObj* %cpsargs40491, %struct.ScmObj** %stackaddr$prim50732, align 8
%clofunc50733 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl40224)
musttail call tailcc void %clofunc50733(%struct.ScmObj* %_37foldl40224, %struct.ScmObj* %cpsargs40491)
ret void
}

define tailcc void @proc_clo$ae41997(%struct.ScmObj* %env$ae41997,%struct.ScmObj* %current_45args49433) {
%stackaddr$prim50734 = alloca %struct.ScmObj*, align 8
%k40493 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49433)
store volatile %struct.ScmObj* %k40493, %struct.ScmObj** %stackaddr$prim50734, align 8
%stackaddr$prim50735 = alloca %struct.ScmObj*, align 8
%current_45args49434 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49433)
store volatile %struct.ScmObj* %current_45args49434, %struct.ScmObj** %stackaddr$prim50735, align 8
%stackaddr$prim50736 = alloca %struct.ScmObj*, align 8
%a40237 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49434)
store volatile %struct.ScmObj* %a40237, %struct.ScmObj** %stackaddr$prim50736, align 8
%stackaddr$prim50737 = alloca %struct.ScmObj*, align 8
%current_45args49435 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49434)
store volatile %struct.ScmObj* %current_45args49435, %struct.ScmObj** %stackaddr$prim50737, align 8
%stackaddr$prim50738 = alloca %struct.ScmObj*, align 8
%b40236 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49435)
store volatile %struct.ScmObj* %b40236, %struct.ScmObj** %stackaddr$prim50738, align 8
%stackaddr$prim50739 = alloca %struct.ScmObj*, align 8
%cpsprim40494 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a40237, %struct.ScmObj* %b40236)
store volatile %struct.ScmObj* %cpsprim40494, %struct.ScmObj** %stackaddr$prim50739, align 8
%ae42001 = call %struct.ScmObj* @const_init_int(i64 0)
%args49437$k40493$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50740 = alloca %struct.ScmObj*, align 8
%args49437$k40493$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40494, %struct.ScmObj* %args49437$k40493$0)
store volatile %struct.ScmObj* %args49437$k40493$1, %struct.ScmObj** %stackaddr$prim50740, align 8
%stackaddr$prim50741 = alloca %struct.ScmObj*, align 8
%args49437$k40493$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42001, %struct.ScmObj* %args49437$k40493$1)
store volatile %struct.ScmObj* %args49437$k40493$2, %struct.ScmObj** %stackaddr$prim50741, align 8
%clofunc50742 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40493)
musttail call tailcc void %clofunc50742(%struct.ScmObj* %k40493, %struct.ScmObj* %args49437$k40493$2)
ret void
}

define tailcc void @proc_clo$ae41973(%struct.ScmObj* %env$ae41973,%struct.ScmObj* %current_45args49440) {
%stackaddr$prim50743 = alloca %struct.ScmObj*, align 8
%k40495 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49440)
store volatile %struct.ScmObj* %k40495, %struct.ScmObj** %stackaddr$prim50743, align 8
%stackaddr$prim50744 = alloca %struct.ScmObj*, align 8
%current_45args49441 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49440)
store volatile %struct.ScmObj* %current_45args49441, %struct.ScmObj** %stackaddr$prim50744, align 8
%stackaddr$prim50745 = alloca %struct.ScmObj*, align 8
%x40232 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49441)
store volatile %struct.ScmObj* %x40232, %struct.ScmObj** %stackaddr$prim50745, align 8
%stackaddr$prim50746 = alloca %struct.ScmObj*, align 8
%cpsprim40496 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40232)
store volatile %struct.ScmObj* %cpsprim40496, %struct.ScmObj** %stackaddr$prim50746, align 8
%ae41976 = call %struct.ScmObj* @const_init_int(i64 0)
%args49443$k40495$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50747 = alloca %struct.ScmObj*, align 8
%args49443$k40495$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40496, %struct.ScmObj* %args49443$k40495$0)
store volatile %struct.ScmObj* %args49443$k40495$1, %struct.ScmObj** %stackaddr$prim50747, align 8
%stackaddr$prim50748 = alloca %struct.ScmObj*, align 8
%args49443$k40495$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41976, %struct.ScmObj* %args49443$k40495$1)
store volatile %struct.ScmObj* %args49443$k40495$2, %struct.ScmObj** %stackaddr$prim50748, align 8
%clofunc50749 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40495)
musttail call tailcc void %clofunc50749(%struct.ScmObj* %k40495, %struct.ScmObj* %args49443$k40495$2)
ret void
}

define tailcc void @proc_clo$ae41949(%struct.ScmObj* %env$ae41949,%struct.ScmObj* %current_45args49446) {
%stackaddr$prim50750 = alloca %struct.ScmObj*, align 8
%k40497 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49446)
store volatile %struct.ScmObj* %k40497, %struct.ScmObj** %stackaddr$prim50750, align 8
%stackaddr$prim50751 = alloca %struct.ScmObj*, align 8
%current_45args49447 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49446)
store volatile %struct.ScmObj* %current_45args49447, %struct.ScmObj** %stackaddr$prim50751, align 8
%stackaddr$prim50752 = alloca %struct.ScmObj*, align 8
%x40234 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49447)
store volatile %struct.ScmObj* %x40234, %struct.ScmObj** %stackaddr$prim50752, align 8
%stackaddr$prim50753 = alloca %struct.ScmObj*, align 8
%cpsprim40498 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40234)
store volatile %struct.ScmObj* %cpsprim40498, %struct.ScmObj** %stackaddr$prim50753, align 8
%ae41952 = call %struct.ScmObj* @const_init_int(i64 0)
%args49449$k40497$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50754 = alloca %struct.ScmObj*, align 8
%args49449$k40497$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40498, %struct.ScmObj* %args49449$k40497$0)
store volatile %struct.ScmObj* %args49449$k40497$1, %struct.ScmObj** %stackaddr$prim50754, align 8
%stackaddr$prim50755 = alloca %struct.ScmObj*, align 8
%args49449$k40497$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41952, %struct.ScmObj* %args49449$k40497$1)
store volatile %struct.ScmObj* %args49449$k40497$2, %struct.ScmObj** %stackaddr$prim50755, align 8
%clofunc50756 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40497)
musttail call tailcc void %clofunc50756(%struct.ScmObj* %k40497, %struct.ScmObj* %args49449$k40497$2)
ret void
}

define tailcc void @proc_clo$ae41901(%struct.ScmObj* %env$ae41901,%struct.ScmObj* %current_45args49452) {
%stackaddr$prim50757 = alloca %struct.ScmObj*, align 8
%k40499 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49452)
store volatile %struct.ScmObj* %k40499, %struct.ScmObj** %stackaddr$prim50757, align 8
%stackaddr$prim50758 = alloca %struct.ScmObj*, align 8
%current_45args49453 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49452)
store volatile %struct.ScmObj* %current_45args49453, %struct.ScmObj** %stackaddr$prim50758, align 8
%stackaddr$prim50759 = alloca %struct.ScmObj*, align 8
%lst40230 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49453)
store volatile %struct.ScmObj* %lst40230, %struct.ScmObj** %stackaddr$prim50759, align 8
%stackaddr$prim50760 = alloca %struct.ScmObj*, align 8
%current_45args49454 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49453)
store volatile %struct.ScmObj* %current_45args49454, %struct.ScmObj** %stackaddr$prim50760, align 8
%stackaddr$prim50761 = alloca %struct.ScmObj*, align 8
%b40229 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49454)
store volatile %struct.ScmObj* %b40229, %struct.ScmObj** %stackaddr$prim50761, align 8
%truthy$cmp50762 = call i64 @is_truthy_value(%struct.ScmObj* %b40229)
%cmp$cmp50762 = icmp eq i64 %truthy$cmp50762, 1
br i1 %cmp$cmp50762, label %truebranch$cmp50762, label %falsebranch$cmp50762
truebranch$cmp50762:
%ae41904 = call %struct.ScmObj* @const_init_int(i64 0)
%args49456$k40499$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50763 = alloca %struct.ScmObj*, align 8
%args49456$k40499$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b40229, %struct.ScmObj* %args49456$k40499$0)
store volatile %struct.ScmObj* %args49456$k40499$1, %struct.ScmObj** %stackaddr$prim50763, align 8
%stackaddr$prim50764 = alloca %struct.ScmObj*, align 8
%args49456$k40499$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41904, %struct.ScmObj* %args49456$k40499$1)
store volatile %struct.ScmObj* %args49456$k40499$2, %struct.ScmObj** %stackaddr$prim50764, align 8
%clofunc50765 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40499)
musttail call tailcc void %clofunc50765(%struct.ScmObj* %k40499, %struct.ScmObj* %args49456$k40499$2)
ret void
falsebranch$cmp50762:
%stackaddr$prim50766 = alloca %struct.ScmObj*, align 8
%cpsprim40500 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40230)
store volatile %struct.ScmObj* %cpsprim40500, %struct.ScmObj** %stackaddr$prim50766, align 8
%ae41911 = call %struct.ScmObj* @const_init_int(i64 0)
%args49457$k40499$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50767 = alloca %struct.ScmObj*, align 8
%args49457$k40499$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40500, %struct.ScmObj* %args49457$k40499$0)
store volatile %struct.ScmObj* %args49457$k40499$1, %struct.ScmObj** %stackaddr$prim50767, align 8
%stackaddr$prim50768 = alloca %struct.ScmObj*, align 8
%args49457$k40499$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41911, %struct.ScmObj* %args49457$k40499$1)
store volatile %struct.ScmObj* %args49457$k40499$2, %struct.ScmObj** %stackaddr$prim50768, align 8
%clofunc50769 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40499)
musttail call tailcc void %clofunc50769(%struct.ScmObj* %k40499, %struct.ScmObj* %args49457$k40499$2)
ret void
}

define tailcc void @proc_clo$ae41742(%struct.ScmObj* %env$ae41742,%struct.ScmObj* %args4016840501) {
%stackaddr$env-ref50770 = alloca %struct.ScmObj*, align 8
%_37last40163 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41742, i64 0)
store %struct.ScmObj* %_37last40163, %struct.ScmObj** %stackaddr$env-ref50770
%stackaddr$env-ref50771 = alloca %struct.ScmObj*, align 8
%_37foldr40146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41742, i64 1)
store %struct.ScmObj* %_37foldr40146, %struct.ScmObj** %stackaddr$env-ref50771
%stackaddr$env-ref50772 = alloca %struct.ScmObj*, align 8
%_37drop_45right40160 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41742, i64 2)
store %struct.ScmObj* %_37drop_45right40160, %struct.ScmObj** %stackaddr$env-ref50772
%stackaddr$prim50773 = alloca %struct.ScmObj*, align 8
%k40502 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4016840501)
store volatile %struct.ScmObj* %k40502, %struct.ScmObj** %stackaddr$prim50773, align 8
%stackaddr$prim50774 = alloca %struct.ScmObj*, align 8
%args40168 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4016840501)
store volatile %struct.ScmObj* %args40168, %struct.ScmObj** %stackaddr$prim50774, align 8
%stackaddr$prim50775 = alloca %struct.ScmObj*, align 8
%f40170 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40168)
store volatile %struct.ScmObj* %f40170, %struct.ScmObj** %stackaddr$prim50775, align 8
%stackaddr$prim50776 = alloca %struct.ScmObj*, align 8
%lsts40169 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40168)
store volatile %struct.ScmObj* %lsts40169, %struct.ScmObj** %stackaddr$prim50776, align 8
%stackaddr$makeclosure50777 = alloca %struct.ScmObj*, align 8
%fptrToInt50778 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41747 to i64
%ae41747 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50778)
store volatile %struct.ScmObj* %ae41747, %struct.ScmObj** %stackaddr$makeclosure50777, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41747, %struct.ScmObj* %k40502, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41747, %struct.ScmObj* %_37foldr40146, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41747, %struct.ScmObj* %lsts40169, i64 2)
%ae41748 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50779 = alloca %struct.ScmObj*, align 8
%fptrToInt50780 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41749 to i64
%ae41749 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50780)
store volatile %struct.ScmObj* %ae41749, %struct.ScmObj** %stackaddr$makeclosure50779, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41749, %struct.ScmObj* %_37last40163, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41749, %struct.ScmObj* %_37drop_45right40160, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41749, %struct.ScmObj* %f40170, i64 2)
%args49476$ae41747$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50781 = alloca %struct.ScmObj*, align 8
%args49476$ae41747$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41749, %struct.ScmObj* %args49476$ae41747$0)
store volatile %struct.ScmObj* %args49476$ae41747$1, %struct.ScmObj** %stackaddr$prim50781, align 8
%stackaddr$prim50782 = alloca %struct.ScmObj*, align 8
%args49476$ae41747$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41748, %struct.ScmObj* %args49476$ae41747$1)
store volatile %struct.ScmObj* %args49476$ae41747$2, %struct.ScmObj** %stackaddr$prim50782, align 8
%clofunc50783 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41747)
musttail call tailcc void %clofunc50783(%struct.ScmObj* %ae41747, %struct.ScmObj* %args49476$ae41747$2)
ret void
}

define tailcc void @proc_clo$ae41747(%struct.ScmObj* %env$ae41747,%struct.ScmObj* %current_45args49461) {
%stackaddr$env-ref50784 = alloca %struct.ScmObj*, align 8
%k40502 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41747, i64 0)
store %struct.ScmObj* %k40502, %struct.ScmObj** %stackaddr$env-ref50784
%stackaddr$env-ref50785 = alloca %struct.ScmObj*, align 8
%_37foldr40146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41747, i64 1)
store %struct.ScmObj* %_37foldr40146, %struct.ScmObj** %stackaddr$env-ref50785
%stackaddr$env-ref50786 = alloca %struct.ScmObj*, align 8
%lsts40169 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41747, i64 2)
store %struct.ScmObj* %lsts40169, %struct.ScmObj** %stackaddr$env-ref50786
%stackaddr$prim50787 = alloca %struct.ScmObj*, align 8
%_95k40503 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49461)
store volatile %struct.ScmObj* %_95k40503, %struct.ScmObj** %stackaddr$prim50787, align 8
%stackaddr$prim50788 = alloca %struct.ScmObj*, align 8
%current_45args49462 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49461)
store volatile %struct.ScmObj* %current_45args49462, %struct.ScmObj** %stackaddr$prim50788, align 8
%stackaddr$prim50789 = alloca %struct.ScmObj*, align 8
%anf_45bind40329 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49462)
store volatile %struct.ScmObj* %anf_45bind40329, %struct.ScmObj** %stackaddr$prim50789, align 8
%ae41810 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50790 = alloca %struct.ScmObj*, align 8
%anf_45bind40330 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41810, %struct.ScmObj* %lsts40169)
store volatile %struct.ScmObj* %anf_45bind40330, %struct.ScmObj** %stackaddr$prim50790, align 8
%stackaddr$prim50791 = alloca %struct.ScmObj*, align 8
%anf_45bind40331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40329, %struct.ScmObj* %anf_45bind40330)
store volatile %struct.ScmObj* %anf_45bind40331, %struct.ScmObj** %stackaddr$prim50791, align 8
%stackaddr$prim50792 = alloca %struct.ScmObj*, align 8
%cpsargs40504 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40502, %struct.ScmObj* %anf_45bind40331)
store volatile %struct.ScmObj* %cpsargs40504, %struct.ScmObj** %stackaddr$prim50792, align 8
%clofunc50793 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr40146)
musttail call tailcc void %clofunc50793(%struct.ScmObj* %_37foldr40146, %struct.ScmObj* %cpsargs40504)
ret void
}

define tailcc void @proc_clo$ae41749(%struct.ScmObj* %env$ae41749,%struct.ScmObj* %fargs4017140505) {
%stackaddr$env-ref50794 = alloca %struct.ScmObj*, align 8
%_37last40163 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41749, i64 0)
store %struct.ScmObj* %_37last40163, %struct.ScmObj** %stackaddr$env-ref50794
%stackaddr$env-ref50795 = alloca %struct.ScmObj*, align 8
%_37drop_45right40160 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41749, i64 1)
store %struct.ScmObj* %_37drop_45right40160, %struct.ScmObj** %stackaddr$env-ref50795
%stackaddr$env-ref50796 = alloca %struct.ScmObj*, align 8
%f40170 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41749, i64 2)
store %struct.ScmObj* %f40170, %struct.ScmObj** %stackaddr$env-ref50796
%stackaddr$prim50797 = alloca %struct.ScmObj*, align 8
%k40506 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs4017140505)
store volatile %struct.ScmObj* %k40506, %struct.ScmObj** %stackaddr$prim50797, align 8
%stackaddr$prim50798 = alloca %struct.ScmObj*, align 8
%fargs40171 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs4017140505)
store volatile %struct.ScmObj* %fargs40171, %struct.ScmObj** %stackaddr$prim50798, align 8
%stackaddr$makeclosure50799 = alloca %struct.ScmObj*, align 8
%fptrToInt50800 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41753 to i64
%ae41753 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt50800)
store volatile %struct.ScmObj* %ae41753, %struct.ScmObj** %stackaddr$makeclosure50799, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41753, %struct.ScmObj* %k40506, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41753, %struct.ScmObj* %f40170, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41753, %struct.ScmObj* %_37last40163, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41753, %struct.ScmObj* %fargs40171, i64 3)
%ae41755 = call %struct.ScmObj* @const_init_int(i64 1)
%args49475$_37drop_45right40160$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50801 = alloca %struct.ScmObj*, align 8
%args49475$_37drop_45right40160$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41755, %struct.ScmObj* %args49475$_37drop_45right40160$0)
store volatile %struct.ScmObj* %args49475$_37drop_45right40160$1, %struct.ScmObj** %stackaddr$prim50801, align 8
%stackaddr$prim50802 = alloca %struct.ScmObj*, align 8
%args49475$_37drop_45right40160$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs40171, %struct.ScmObj* %args49475$_37drop_45right40160$1)
store volatile %struct.ScmObj* %args49475$_37drop_45right40160$2, %struct.ScmObj** %stackaddr$prim50802, align 8
%stackaddr$prim50803 = alloca %struct.ScmObj*, align 8
%args49475$_37drop_45right40160$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41753, %struct.ScmObj* %args49475$_37drop_45right40160$2)
store volatile %struct.ScmObj* %args49475$_37drop_45right40160$3, %struct.ScmObj** %stackaddr$prim50803, align 8
%clofunc50804 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right40160)
musttail call tailcc void %clofunc50804(%struct.ScmObj* %_37drop_45right40160, %struct.ScmObj* %args49475$_37drop_45right40160$3)
ret void
}

define tailcc void @proc_clo$ae41753(%struct.ScmObj* %env$ae41753,%struct.ScmObj* %current_45args49464) {
%stackaddr$env-ref50805 = alloca %struct.ScmObj*, align 8
%k40506 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41753, i64 0)
store %struct.ScmObj* %k40506, %struct.ScmObj** %stackaddr$env-ref50805
%stackaddr$env-ref50806 = alloca %struct.ScmObj*, align 8
%f40170 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41753, i64 1)
store %struct.ScmObj* %f40170, %struct.ScmObj** %stackaddr$env-ref50806
%stackaddr$env-ref50807 = alloca %struct.ScmObj*, align 8
%_37last40163 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41753, i64 2)
store %struct.ScmObj* %_37last40163, %struct.ScmObj** %stackaddr$env-ref50807
%stackaddr$env-ref50808 = alloca %struct.ScmObj*, align 8
%fargs40171 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41753, i64 3)
store %struct.ScmObj* %fargs40171, %struct.ScmObj** %stackaddr$env-ref50808
%stackaddr$prim50809 = alloca %struct.ScmObj*, align 8
%_95k40507 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49464)
store volatile %struct.ScmObj* %_95k40507, %struct.ScmObj** %stackaddr$prim50809, align 8
%stackaddr$prim50810 = alloca %struct.ScmObj*, align 8
%current_45args49465 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49464)
store volatile %struct.ScmObj* %current_45args49465, %struct.ScmObj** %stackaddr$prim50810, align 8
%stackaddr$prim50811 = alloca %struct.ScmObj*, align 8
%anf_45bind40326 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49465)
store volatile %struct.ScmObj* %anf_45bind40326, %struct.ScmObj** %stackaddr$prim50811, align 8
%stackaddr$makeclosure50812 = alloca %struct.ScmObj*, align 8
%fptrToInt50813 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41760 to i64
%ae41760 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50813)
store volatile %struct.ScmObj* %ae41760, %struct.ScmObj** %stackaddr$makeclosure50812, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41760, %struct.ScmObj* %_37last40163, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41760, %struct.ScmObj* %fargs40171, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41760, %struct.ScmObj* %k40506, i64 2)
%stackaddr$prim50814 = alloca %struct.ScmObj*, align 8
%cpsargs40511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41760, %struct.ScmObj* %anf_45bind40326)
store volatile %struct.ScmObj* %cpsargs40511, %struct.ScmObj** %stackaddr$prim50814, align 8
%clofunc50815 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40170)
musttail call tailcc void %clofunc50815(%struct.ScmObj* %f40170, %struct.ScmObj* %cpsargs40511)
ret void
}

define tailcc void @proc_clo$ae41760(%struct.ScmObj* %env$ae41760,%struct.ScmObj* %current_45args49467) {
%stackaddr$env-ref50816 = alloca %struct.ScmObj*, align 8
%_37last40163 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41760, i64 0)
store %struct.ScmObj* %_37last40163, %struct.ScmObj** %stackaddr$env-ref50816
%stackaddr$env-ref50817 = alloca %struct.ScmObj*, align 8
%fargs40171 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41760, i64 1)
store %struct.ScmObj* %fargs40171, %struct.ScmObj** %stackaddr$env-ref50817
%stackaddr$env-ref50818 = alloca %struct.ScmObj*, align 8
%k40506 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41760, i64 2)
store %struct.ScmObj* %k40506, %struct.ScmObj** %stackaddr$env-ref50818
%stackaddr$prim50819 = alloca %struct.ScmObj*, align 8
%_95k40508 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49467)
store volatile %struct.ScmObj* %_95k40508, %struct.ScmObj** %stackaddr$prim50819, align 8
%stackaddr$prim50820 = alloca %struct.ScmObj*, align 8
%current_45args49468 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49467)
store volatile %struct.ScmObj* %current_45args49468, %struct.ScmObj** %stackaddr$prim50820, align 8
%stackaddr$prim50821 = alloca %struct.ScmObj*, align 8
%anf_45bind40327 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49468)
store volatile %struct.ScmObj* %anf_45bind40327, %struct.ScmObj** %stackaddr$prim50821, align 8
%stackaddr$makeclosure50822 = alloca %struct.ScmObj*, align 8
%fptrToInt50823 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41765 to i64
%ae41765 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt50823)
store volatile %struct.ScmObj* %ae41765, %struct.ScmObj** %stackaddr$makeclosure50822, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41765, %struct.ScmObj* %anf_45bind40327, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41765, %struct.ScmObj* %k40506, i64 1)
%args49474$_37last40163$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50824 = alloca %struct.ScmObj*, align 8
%args49474$_37last40163$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs40171, %struct.ScmObj* %args49474$_37last40163$0)
store volatile %struct.ScmObj* %args49474$_37last40163$1, %struct.ScmObj** %stackaddr$prim50824, align 8
%stackaddr$prim50825 = alloca %struct.ScmObj*, align 8
%args49474$_37last40163$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41765, %struct.ScmObj* %args49474$_37last40163$1)
store volatile %struct.ScmObj* %args49474$_37last40163$2, %struct.ScmObj** %stackaddr$prim50825, align 8
%clofunc50826 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last40163)
musttail call tailcc void %clofunc50826(%struct.ScmObj* %_37last40163, %struct.ScmObj* %args49474$_37last40163$2)
ret void
}

define tailcc void @proc_clo$ae41765(%struct.ScmObj* %env$ae41765,%struct.ScmObj* %current_45args49470) {
%stackaddr$env-ref50827 = alloca %struct.ScmObj*, align 8
%anf_45bind40327 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41765, i64 0)
store %struct.ScmObj* %anf_45bind40327, %struct.ScmObj** %stackaddr$env-ref50827
%stackaddr$env-ref50828 = alloca %struct.ScmObj*, align 8
%k40506 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41765, i64 1)
store %struct.ScmObj* %k40506, %struct.ScmObj** %stackaddr$env-ref50828
%stackaddr$prim50829 = alloca %struct.ScmObj*, align 8
%_95k40509 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49470)
store volatile %struct.ScmObj* %_95k40509, %struct.ScmObj** %stackaddr$prim50829, align 8
%stackaddr$prim50830 = alloca %struct.ScmObj*, align 8
%current_45args49471 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49470)
store volatile %struct.ScmObj* %current_45args49471, %struct.ScmObj** %stackaddr$prim50830, align 8
%stackaddr$prim50831 = alloca %struct.ScmObj*, align 8
%anf_45bind40328 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49471)
store volatile %struct.ScmObj* %anf_45bind40328, %struct.ScmObj** %stackaddr$prim50831, align 8
%stackaddr$prim50832 = alloca %struct.ScmObj*, align 8
%cpsprim40510 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40327, %struct.ScmObj* %anf_45bind40328)
store volatile %struct.ScmObj* %cpsprim40510, %struct.ScmObj** %stackaddr$prim50832, align 8
%ae41770 = call %struct.ScmObj* @const_init_int(i64 0)
%args49473$k40506$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50833 = alloca %struct.ScmObj*, align 8
%args49473$k40506$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40510, %struct.ScmObj* %args49473$k40506$0)
store volatile %struct.ScmObj* %args49473$k40506$1, %struct.ScmObj** %stackaddr$prim50833, align 8
%stackaddr$prim50834 = alloca %struct.ScmObj*, align 8
%args49473$k40506$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41770, %struct.ScmObj* %args49473$k40506$1)
store volatile %struct.ScmObj* %args49473$k40506$2, %struct.ScmObj** %stackaddr$prim50834, align 8
%clofunc50835 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40506)
musttail call tailcc void %clofunc50835(%struct.ScmObj* %k40506, %struct.ScmObj* %args49473$k40506$2)
ret void
}

define tailcc void @proc_clo$ae41665(%struct.ScmObj* %env$ae41665,%struct.ScmObj* %current_45args49478) {
%stackaddr$env-ref50836 = alloca %struct.ScmObj*, align 8
%_37foldr140141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41665, i64 0)
store %struct.ScmObj* %_37foldr140141, %struct.ScmObj** %stackaddr$env-ref50836
%stackaddr$prim50837 = alloca %struct.ScmObj*, align 8
%k40512 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49478)
store volatile %struct.ScmObj* %k40512, %struct.ScmObj** %stackaddr$prim50837, align 8
%stackaddr$prim50838 = alloca %struct.ScmObj*, align 8
%current_45args49479 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49478)
store volatile %struct.ScmObj* %current_45args49479, %struct.ScmObj** %stackaddr$prim50838, align 8
%stackaddr$prim50839 = alloca %struct.ScmObj*, align 8
%f40174 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49479)
store volatile %struct.ScmObj* %f40174, %struct.ScmObj** %stackaddr$prim50839, align 8
%stackaddr$prim50840 = alloca %struct.ScmObj*, align 8
%current_45args49480 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49479)
store volatile %struct.ScmObj* %current_45args49480, %struct.ScmObj** %stackaddr$prim50840, align 8
%stackaddr$prim50841 = alloca %struct.ScmObj*, align 8
%lst40173 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49480)
store volatile %struct.ScmObj* %lst40173, %struct.ScmObj** %stackaddr$prim50841, align 8
%stackaddr$makeclosure50842 = alloca %struct.ScmObj*, align 8
%fptrToInt50843 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41666 to i64
%ae41666 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50843)
store volatile %struct.ScmObj* %ae41666, %struct.ScmObj** %stackaddr$makeclosure50842, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41666, %struct.ScmObj* %lst40173, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41666, %struct.ScmObj* %_37foldr140141, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41666, %struct.ScmObj* %k40512, i64 2)
%ae41667 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50844 = alloca %struct.ScmObj*, align 8
%fptrToInt50845 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41668 to i64
%ae41668 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50845)
store volatile %struct.ScmObj* %ae41668, %struct.ScmObj** %stackaddr$makeclosure50844, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41668, %struct.ScmObj* %f40174, i64 0)
%args49495$ae41666$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50846 = alloca %struct.ScmObj*, align 8
%args49495$ae41666$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41668, %struct.ScmObj* %args49495$ae41666$0)
store volatile %struct.ScmObj* %args49495$ae41666$1, %struct.ScmObj** %stackaddr$prim50846, align 8
%stackaddr$prim50847 = alloca %struct.ScmObj*, align 8
%args49495$ae41666$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41667, %struct.ScmObj* %args49495$ae41666$1)
store volatile %struct.ScmObj* %args49495$ae41666$2, %struct.ScmObj** %stackaddr$prim50847, align 8
%clofunc50848 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41666)
musttail call tailcc void %clofunc50848(%struct.ScmObj* %ae41666, %struct.ScmObj* %args49495$ae41666$2)
ret void
}

define tailcc void @proc_clo$ae41666(%struct.ScmObj* %env$ae41666,%struct.ScmObj* %current_45args49482) {
%stackaddr$env-ref50849 = alloca %struct.ScmObj*, align 8
%lst40173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41666, i64 0)
store %struct.ScmObj* %lst40173, %struct.ScmObj** %stackaddr$env-ref50849
%stackaddr$env-ref50850 = alloca %struct.ScmObj*, align 8
%_37foldr140141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41666, i64 1)
store %struct.ScmObj* %_37foldr140141, %struct.ScmObj** %stackaddr$env-ref50850
%stackaddr$env-ref50851 = alloca %struct.ScmObj*, align 8
%k40512 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41666, i64 2)
store %struct.ScmObj* %k40512, %struct.ScmObj** %stackaddr$env-ref50851
%stackaddr$prim50852 = alloca %struct.ScmObj*, align 8
%_95k40513 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49482)
store volatile %struct.ScmObj* %_95k40513, %struct.ScmObj** %stackaddr$prim50852, align 8
%stackaddr$prim50853 = alloca %struct.ScmObj*, align 8
%current_45args49483 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49482)
store volatile %struct.ScmObj* %current_45args49483, %struct.ScmObj** %stackaddr$prim50853, align 8
%stackaddr$prim50854 = alloca %struct.ScmObj*, align 8
%anf_45bind40325 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49483)
store volatile %struct.ScmObj* %anf_45bind40325, %struct.ScmObj** %stackaddr$prim50854, align 8
%ae41700 = call %struct.ScmObj* @const_init_null()
%args49485$_37foldr140141$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50855 = alloca %struct.ScmObj*, align 8
%args49485$_37foldr140141$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40173, %struct.ScmObj* %args49485$_37foldr140141$0)
store volatile %struct.ScmObj* %args49485$_37foldr140141$1, %struct.ScmObj** %stackaddr$prim50855, align 8
%stackaddr$prim50856 = alloca %struct.ScmObj*, align 8
%args49485$_37foldr140141$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41700, %struct.ScmObj* %args49485$_37foldr140141$1)
store volatile %struct.ScmObj* %args49485$_37foldr140141$2, %struct.ScmObj** %stackaddr$prim50856, align 8
%stackaddr$prim50857 = alloca %struct.ScmObj*, align 8
%args49485$_37foldr140141$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40325, %struct.ScmObj* %args49485$_37foldr140141$2)
store volatile %struct.ScmObj* %args49485$_37foldr140141$3, %struct.ScmObj** %stackaddr$prim50857, align 8
%stackaddr$prim50858 = alloca %struct.ScmObj*, align 8
%args49485$_37foldr140141$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40512, %struct.ScmObj* %args49485$_37foldr140141$3)
store volatile %struct.ScmObj* %args49485$_37foldr140141$4, %struct.ScmObj** %stackaddr$prim50858, align 8
%clofunc50859 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140141)
musttail call tailcc void %clofunc50859(%struct.ScmObj* %_37foldr140141, %struct.ScmObj* %args49485$_37foldr140141$4)
ret void
}

define tailcc void @proc_clo$ae41668(%struct.ScmObj* %env$ae41668,%struct.ScmObj* %current_45args49486) {
%stackaddr$env-ref50860 = alloca %struct.ScmObj*, align 8
%f40174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41668, i64 0)
store %struct.ScmObj* %f40174, %struct.ScmObj** %stackaddr$env-ref50860
%stackaddr$prim50861 = alloca %struct.ScmObj*, align 8
%k40514 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49486)
store volatile %struct.ScmObj* %k40514, %struct.ScmObj** %stackaddr$prim50861, align 8
%stackaddr$prim50862 = alloca %struct.ScmObj*, align 8
%current_45args49487 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49486)
store volatile %struct.ScmObj* %current_45args49487, %struct.ScmObj** %stackaddr$prim50862, align 8
%stackaddr$prim50863 = alloca %struct.ScmObj*, align 8
%v40176 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49487)
store volatile %struct.ScmObj* %v40176, %struct.ScmObj** %stackaddr$prim50863, align 8
%stackaddr$prim50864 = alloca %struct.ScmObj*, align 8
%current_45args49488 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49487)
store volatile %struct.ScmObj* %current_45args49488, %struct.ScmObj** %stackaddr$prim50864, align 8
%stackaddr$prim50865 = alloca %struct.ScmObj*, align 8
%r40175 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49488)
store volatile %struct.ScmObj* %r40175, %struct.ScmObj** %stackaddr$prim50865, align 8
%stackaddr$makeclosure50866 = alloca %struct.ScmObj*, align 8
%fptrToInt50867 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41670 to i64
%ae41670 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt50867)
store volatile %struct.ScmObj* %ae41670, %struct.ScmObj** %stackaddr$makeclosure50866, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41670, %struct.ScmObj* %k40514, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41670, %struct.ScmObj* %r40175, i64 1)
%args49494$f40174$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50868 = alloca %struct.ScmObj*, align 8
%args49494$f40174$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v40176, %struct.ScmObj* %args49494$f40174$0)
store volatile %struct.ScmObj* %args49494$f40174$1, %struct.ScmObj** %stackaddr$prim50868, align 8
%stackaddr$prim50869 = alloca %struct.ScmObj*, align 8
%args49494$f40174$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41670, %struct.ScmObj* %args49494$f40174$1)
store volatile %struct.ScmObj* %args49494$f40174$2, %struct.ScmObj** %stackaddr$prim50869, align 8
%clofunc50870 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40174)
musttail call tailcc void %clofunc50870(%struct.ScmObj* %f40174, %struct.ScmObj* %args49494$f40174$2)
ret void
}

define tailcc void @proc_clo$ae41670(%struct.ScmObj* %env$ae41670,%struct.ScmObj* %current_45args49490) {
%stackaddr$env-ref50871 = alloca %struct.ScmObj*, align 8
%k40514 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41670, i64 0)
store %struct.ScmObj* %k40514, %struct.ScmObj** %stackaddr$env-ref50871
%stackaddr$env-ref50872 = alloca %struct.ScmObj*, align 8
%r40175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41670, i64 1)
store %struct.ScmObj* %r40175, %struct.ScmObj** %stackaddr$env-ref50872
%stackaddr$prim50873 = alloca %struct.ScmObj*, align 8
%_95k40515 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49490)
store volatile %struct.ScmObj* %_95k40515, %struct.ScmObj** %stackaddr$prim50873, align 8
%stackaddr$prim50874 = alloca %struct.ScmObj*, align 8
%current_45args49491 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49490)
store volatile %struct.ScmObj* %current_45args49491, %struct.ScmObj** %stackaddr$prim50874, align 8
%stackaddr$prim50875 = alloca %struct.ScmObj*, align 8
%anf_45bind40324 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49491)
store volatile %struct.ScmObj* %anf_45bind40324, %struct.ScmObj** %stackaddr$prim50875, align 8
%stackaddr$prim50876 = alloca %struct.ScmObj*, align 8
%cpsprim40516 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40324, %struct.ScmObj* %r40175)
store volatile %struct.ScmObj* %cpsprim40516, %struct.ScmObj** %stackaddr$prim50876, align 8
%ae41675 = call %struct.ScmObj* @const_init_int(i64 0)
%args49493$k40514$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50877 = alloca %struct.ScmObj*, align 8
%args49493$k40514$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40516, %struct.ScmObj* %args49493$k40514$0)
store volatile %struct.ScmObj* %args49493$k40514$1, %struct.ScmObj** %stackaddr$prim50877, align 8
%stackaddr$prim50878 = alloca %struct.ScmObj*, align 8
%args49493$k40514$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41675, %struct.ScmObj* %args49493$k40514$1)
store volatile %struct.ScmObj* %args49493$k40514$2, %struct.ScmObj** %stackaddr$prim50878, align 8
%clofunc50879 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40514)
musttail call tailcc void %clofunc50879(%struct.ScmObj* %k40514, %struct.ScmObj* %args49493$k40514$2)
ret void
}

define tailcc void @proc_clo$ae41279(%struct.ScmObj* %env$ae41279,%struct.ScmObj* %current_45args49498) {
%stackaddr$env-ref50880 = alloca %struct.ScmObj*, align 8
%_37foldr140141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41279, i64 0)
store %struct.ScmObj* %_37foldr140141, %struct.ScmObj** %stackaddr$env-ref50880
%stackaddr$env-ref50881 = alloca %struct.ScmObj*, align 8
%_37map140137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41279, i64 1)
store %struct.ScmObj* %_37map140137, %struct.ScmObj** %stackaddr$env-ref50881
%stackaddr$prim50882 = alloca %struct.ScmObj*, align 8
%k40517 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49498)
store volatile %struct.ScmObj* %k40517, %struct.ScmObj** %stackaddr$prim50882, align 8
%stackaddr$prim50883 = alloca %struct.ScmObj*, align 8
%current_45args49499 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49498)
store volatile %struct.ScmObj* %current_45args49499, %struct.ScmObj** %stackaddr$prim50883, align 8
%stackaddr$prim50884 = alloca %struct.ScmObj*, align 8
%_37foldr40147 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49499)
store volatile %struct.ScmObj* %_37foldr40147, %struct.ScmObj** %stackaddr$prim50884, align 8
%ae41281 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50885 = alloca %struct.ScmObj*, align 8
%fptrToInt50886 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41282 to i64
%ae41282 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50886)
store volatile %struct.ScmObj* %ae41282, %struct.ScmObj** %stackaddr$makeclosure50885, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41282, %struct.ScmObj* %_37foldr40147, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41282, %struct.ScmObj* %_37foldr140141, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41282, %struct.ScmObj* %_37map140137, i64 2)
%args49556$k40517$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50887 = alloca %struct.ScmObj*, align 8
%args49556$k40517$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41282, %struct.ScmObj* %args49556$k40517$0)
store volatile %struct.ScmObj* %args49556$k40517$1, %struct.ScmObj** %stackaddr$prim50887, align 8
%stackaddr$prim50888 = alloca %struct.ScmObj*, align 8
%args49556$k40517$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41281, %struct.ScmObj* %args49556$k40517$1)
store volatile %struct.ScmObj* %args49556$k40517$2, %struct.ScmObj** %stackaddr$prim50888, align 8
%clofunc50889 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40517)
musttail call tailcc void %clofunc50889(%struct.ScmObj* %k40517, %struct.ScmObj* %args49556$k40517$2)
ret void
}

define tailcc void @proc_clo$ae41282(%struct.ScmObj* %env$ae41282,%struct.ScmObj* %args4014840518) {
%stackaddr$env-ref50890 = alloca %struct.ScmObj*, align 8
%_37foldr40147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41282, i64 0)
store %struct.ScmObj* %_37foldr40147, %struct.ScmObj** %stackaddr$env-ref50890
%stackaddr$env-ref50891 = alloca %struct.ScmObj*, align 8
%_37foldr140141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41282, i64 1)
store %struct.ScmObj* %_37foldr140141, %struct.ScmObj** %stackaddr$env-ref50891
%stackaddr$env-ref50892 = alloca %struct.ScmObj*, align 8
%_37map140137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41282, i64 2)
store %struct.ScmObj* %_37map140137, %struct.ScmObj** %stackaddr$env-ref50892
%stackaddr$prim50893 = alloca %struct.ScmObj*, align 8
%k40519 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4014840518)
store volatile %struct.ScmObj* %k40519, %struct.ScmObj** %stackaddr$prim50893, align 8
%stackaddr$prim50894 = alloca %struct.ScmObj*, align 8
%args40148 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4014840518)
store volatile %struct.ScmObj* %args40148, %struct.ScmObj** %stackaddr$prim50894, align 8
%stackaddr$prim50895 = alloca %struct.ScmObj*, align 8
%f40151 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40148)
store volatile %struct.ScmObj* %f40151, %struct.ScmObj** %stackaddr$prim50895, align 8
%stackaddr$prim50896 = alloca %struct.ScmObj*, align 8
%anf_45bind40311 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40148)
store volatile %struct.ScmObj* %anf_45bind40311, %struct.ScmObj** %stackaddr$prim50896, align 8
%stackaddr$prim50897 = alloca %struct.ScmObj*, align 8
%acc40150 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40311)
store volatile %struct.ScmObj* %acc40150, %struct.ScmObj** %stackaddr$prim50897, align 8
%stackaddr$prim50898 = alloca %struct.ScmObj*, align 8
%anf_45bind40312 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40148)
store volatile %struct.ScmObj* %anf_45bind40312, %struct.ScmObj** %stackaddr$prim50898, align 8
%stackaddr$prim50899 = alloca %struct.ScmObj*, align 8
%lsts40149 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40312)
store volatile %struct.ScmObj* %lsts40149, %struct.ScmObj** %stackaddr$prim50899, align 8
%stackaddr$makeclosure50900 = alloca %struct.ScmObj*, align 8
%fptrToInt50901 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41290 to i64
%ae41290 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt50901)
store volatile %struct.ScmObj* %ae41290, %struct.ScmObj** %stackaddr$makeclosure50900, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41290, %struct.ScmObj* %k40519, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41290, %struct.ScmObj* %f40151, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41290, %struct.ScmObj* %acc40150, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41290, %struct.ScmObj* %lsts40149, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41290, %struct.ScmObj* %_37foldr40147, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41290, %struct.ScmObj* %_37foldr140141, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41290, %struct.ScmObj* %_37map140137, i64 6)
%ae41291 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50902 = alloca %struct.ScmObj*, align 8
%fptrToInt50903 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41292 to i64
%ae41292 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50903)
store volatile %struct.ScmObj* %ae41292, %struct.ScmObj** %stackaddr$makeclosure50902, align 8
%args49555$ae41290$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50904 = alloca %struct.ScmObj*, align 8
%args49555$ae41290$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41292, %struct.ScmObj* %args49555$ae41290$0)
store volatile %struct.ScmObj* %args49555$ae41290$1, %struct.ScmObj** %stackaddr$prim50904, align 8
%stackaddr$prim50905 = alloca %struct.ScmObj*, align 8
%args49555$ae41290$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41291, %struct.ScmObj* %args49555$ae41290$1)
store volatile %struct.ScmObj* %args49555$ae41290$2, %struct.ScmObj** %stackaddr$prim50905, align 8
%clofunc50906 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41290)
musttail call tailcc void %clofunc50906(%struct.ScmObj* %ae41290, %struct.ScmObj* %args49555$ae41290$2)
ret void
}

define tailcc void @proc_clo$ae41290(%struct.ScmObj* %env$ae41290,%struct.ScmObj* %current_45args49501) {
%stackaddr$env-ref50907 = alloca %struct.ScmObj*, align 8
%k40519 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41290, i64 0)
store %struct.ScmObj* %k40519, %struct.ScmObj** %stackaddr$env-ref50907
%stackaddr$env-ref50908 = alloca %struct.ScmObj*, align 8
%f40151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41290, i64 1)
store %struct.ScmObj* %f40151, %struct.ScmObj** %stackaddr$env-ref50908
%stackaddr$env-ref50909 = alloca %struct.ScmObj*, align 8
%acc40150 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41290, i64 2)
store %struct.ScmObj* %acc40150, %struct.ScmObj** %stackaddr$env-ref50909
%stackaddr$env-ref50910 = alloca %struct.ScmObj*, align 8
%lsts40149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41290, i64 3)
store %struct.ScmObj* %lsts40149, %struct.ScmObj** %stackaddr$env-ref50910
%stackaddr$env-ref50911 = alloca %struct.ScmObj*, align 8
%_37foldr40147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41290, i64 4)
store %struct.ScmObj* %_37foldr40147, %struct.ScmObj** %stackaddr$env-ref50911
%stackaddr$env-ref50912 = alloca %struct.ScmObj*, align 8
%_37foldr140141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41290, i64 5)
store %struct.ScmObj* %_37foldr140141, %struct.ScmObj** %stackaddr$env-ref50912
%stackaddr$env-ref50913 = alloca %struct.ScmObj*, align 8
%_37map140137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41290, i64 6)
store %struct.ScmObj* %_37map140137, %struct.ScmObj** %stackaddr$env-ref50913
%stackaddr$prim50914 = alloca %struct.ScmObj*, align 8
%_95k40520 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49501)
store volatile %struct.ScmObj* %_95k40520, %struct.ScmObj** %stackaddr$prim50914, align 8
%stackaddr$prim50915 = alloca %struct.ScmObj*, align 8
%current_45args49502 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49501)
store volatile %struct.ScmObj* %current_45args49502, %struct.ScmObj** %stackaddr$prim50915, align 8
%stackaddr$prim50916 = alloca %struct.ScmObj*, align 8
%anf_45bind40313 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49502)
store volatile %struct.ScmObj* %anf_45bind40313, %struct.ScmObj** %stackaddr$prim50916, align 8
%stackaddr$makeclosure50917 = alloca %struct.ScmObj*, align 8
%fptrToInt50918 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41322 to i64
%ae41322 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt50918)
store volatile %struct.ScmObj* %ae41322, %struct.ScmObj** %stackaddr$makeclosure50917, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41322, %struct.ScmObj* %k40519, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41322, %struct.ScmObj* %f40151, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41322, %struct.ScmObj* %acc40150, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41322, %struct.ScmObj* %lsts40149, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41322, %struct.ScmObj* %_37foldr40147, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41322, %struct.ScmObj* %_37foldr140141, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41322, %struct.ScmObj* %_37map140137, i64 6)
%ae41324 = call %struct.ScmObj* @const_init_false()
%args49548$_37foldr140141$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50919 = alloca %struct.ScmObj*, align 8
%args49548$_37foldr140141$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40149, %struct.ScmObj* %args49548$_37foldr140141$0)
store volatile %struct.ScmObj* %args49548$_37foldr140141$1, %struct.ScmObj** %stackaddr$prim50919, align 8
%stackaddr$prim50920 = alloca %struct.ScmObj*, align 8
%args49548$_37foldr140141$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41324, %struct.ScmObj* %args49548$_37foldr140141$1)
store volatile %struct.ScmObj* %args49548$_37foldr140141$2, %struct.ScmObj** %stackaddr$prim50920, align 8
%stackaddr$prim50921 = alloca %struct.ScmObj*, align 8
%args49548$_37foldr140141$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40313, %struct.ScmObj* %args49548$_37foldr140141$2)
store volatile %struct.ScmObj* %args49548$_37foldr140141$3, %struct.ScmObj** %stackaddr$prim50921, align 8
%stackaddr$prim50922 = alloca %struct.ScmObj*, align 8
%args49548$_37foldr140141$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41322, %struct.ScmObj* %args49548$_37foldr140141$3)
store volatile %struct.ScmObj* %args49548$_37foldr140141$4, %struct.ScmObj** %stackaddr$prim50922, align 8
%clofunc50923 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140141)
musttail call tailcc void %clofunc50923(%struct.ScmObj* %_37foldr140141, %struct.ScmObj* %args49548$_37foldr140141$4)
ret void
}

define tailcc void @proc_clo$ae41322(%struct.ScmObj* %env$ae41322,%struct.ScmObj* %current_45args49504) {
%stackaddr$env-ref50924 = alloca %struct.ScmObj*, align 8
%k40519 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41322, i64 0)
store %struct.ScmObj* %k40519, %struct.ScmObj** %stackaddr$env-ref50924
%stackaddr$env-ref50925 = alloca %struct.ScmObj*, align 8
%f40151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41322, i64 1)
store %struct.ScmObj* %f40151, %struct.ScmObj** %stackaddr$env-ref50925
%stackaddr$env-ref50926 = alloca %struct.ScmObj*, align 8
%acc40150 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41322, i64 2)
store %struct.ScmObj* %acc40150, %struct.ScmObj** %stackaddr$env-ref50926
%stackaddr$env-ref50927 = alloca %struct.ScmObj*, align 8
%lsts40149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41322, i64 3)
store %struct.ScmObj* %lsts40149, %struct.ScmObj** %stackaddr$env-ref50927
%stackaddr$env-ref50928 = alloca %struct.ScmObj*, align 8
%_37foldr40147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41322, i64 4)
store %struct.ScmObj* %_37foldr40147, %struct.ScmObj** %stackaddr$env-ref50928
%stackaddr$env-ref50929 = alloca %struct.ScmObj*, align 8
%_37foldr140141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41322, i64 5)
store %struct.ScmObj* %_37foldr140141, %struct.ScmObj** %stackaddr$env-ref50929
%stackaddr$env-ref50930 = alloca %struct.ScmObj*, align 8
%_37map140137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41322, i64 6)
store %struct.ScmObj* %_37map140137, %struct.ScmObj** %stackaddr$env-ref50930
%stackaddr$prim50931 = alloca %struct.ScmObj*, align 8
%_95k40521 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49504)
store volatile %struct.ScmObj* %_95k40521, %struct.ScmObj** %stackaddr$prim50931, align 8
%stackaddr$prim50932 = alloca %struct.ScmObj*, align 8
%current_45args49505 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49504)
store volatile %struct.ScmObj* %current_45args49505, %struct.ScmObj** %stackaddr$prim50932, align 8
%stackaddr$prim50933 = alloca %struct.ScmObj*, align 8
%anf_45bind40314 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49505)
store volatile %struct.ScmObj* %anf_45bind40314, %struct.ScmObj** %stackaddr$prim50933, align 8
%truthy$cmp50934 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40314)
%cmp$cmp50934 = icmp eq i64 %truthy$cmp50934, 1
br i1 %cmp$cmp50934, label %truebranch$cmp50934, label %falsebranch$cmp50934
truebranch$cmp50934:
%ae41333 = call %struct.ScmObj* @const_init_int(i64 0)
%args49507$k40519$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50935 = alloca %struct.ScmObj*, align 8
%args49507$k40519$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40150, %struct.ScmObj* %args49507$k40519$0)
store volatile %struct.ScmObj* %args49507$k40519$1, %struct.ScmObj** %stackaddr$prim50935, align 8
%stackaddr$prim50936 = alloca %struct.ScmObj*, align 8
%args49507$k40519$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41333, %struct.ScmObj* %args49507$k40519$1)
store volatile %struct.ScmObj* %args49507$k40519$2, %struct.ScmObj** %stackaddr$prim50936, align 8
%clofunc50937 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40519)
musttail call tailcc void %clofunc50937(%struct.ScmObj* %k40519, %struct.ScmObj* %args49507$k40519$2)
ret void
falsebranch$cmp50934:
%stackaddr$makeclosure50938 = alloca %struct.ScmObj*, align 8
%fptrToInt50939 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41338 to i64
%ae41338 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt50939)
store volatile %struct.ScmObj* %ae41338, %struct.ScmObj** %stackaddr$makeclosure50938, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41338, %struct.ScmObj* %k40519, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41338, %struct.ScmObj* %f40151, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41338, %struct.ScmObj* %acc40150, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41338, %struct.ScmObj* %lsts40149, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41338, %struct.ScmObj* %_37foldr40147, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41338, %struct.ScmObj* %_37foldr140141, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41338, %struct.ScmObj* %_37map140137, i64 6)
%ae41339 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50940 = alloca %struct.ScmObj*, align 8
%fptrToInt50941 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41340 to i64
%ae41340 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50941)
store volatile %struct.ScmObj* %ae41340, %struct.ScmObj** %stackaddr$makeclosure50940, align 8
%args49547$ae41338$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50942 = alloca %struct.ScmObj*, align 8
%args49547$ae41338$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41340, %struct.ScmObj* %args49547$ae41338$0)
store volatile %struct.ScmObj* %args49547$ae41338$1, %struct.ScmObj** %stackaddr$prim50942, align 8
%stackaddr$prim50943 = alloca %struct.ScmObj*, align 8
%args49547$ae41338$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41339, %struct.ScmObj* %args49547$ae41338$1)
store volatile %struct.ScmObj* %args49547$ae41338$2, %struct.ScmObj** %stackaddr$prim50943, align 8
%clofunc50944 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41338)
musttail call tailcc void %clofunc50944(%struct.ScmObj* %ae41338, %struct.ScmObj* %args49547$ae41338$2)
ret void
}

define tailcc void @proc_clo$ae41338(%struct.ScmObj* %env$ae41338,%struct.ScmObj* %current_45args49508) {
%stackaddr$env-ref50945 = alloca %struct.ScmObj*, align 8
%k40519 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41338, i64 0)
store %struct.ScmObj* %k40519, %struct.ScmObj** %stackaddr$env-ref50945
%stackaddr$env-ref50946 = alloca %struct.ScmObj*, align 8
%f40151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41338, i64 1)
store %struct.ScmObj* %f40151, %struct.ScmObj** %stackaddr$env-ref50946
%stackaddr$env-ref50947 = alloca %struct.ScmObj*, align 8
%acc40150 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41338, i64 2)
store %struct.ScmObj* %acc40150, %struct.ScmObj** %stackaddr$env-ref50947
%stackaddr$env-ref50948 = alloca %struct.ScmObj*, align 8
%lsts40149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41338, i64 3)
store %struct.ScmObj* %lsts40149, %struct.ScmObj** %stackaddr$env-ref50948
%stackaddr$env-ref50949 = alloca %struct.ScmObj*, align 8
%_37foldr40147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41338, i64 4)
store %struct.ScmObj* %_37foldr40147, %struct.ScmObj** %stackaddr$env-ref50949
%stackaddr$env-ref50950 = alloca %struct.ScmObj*, align 8
%_37foldr140141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41338, i64 5)
store %struct.ScmObj* %_37foldr140141, %struct.ScmObj** %stackaddr$env-ref50950
%stackaddr$env-ref50951 = alloca %struct.ScmObj*, align 8
%_37map140137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41338, i64 6)
store %struct.ScmObj* %_37map140137, %struct.ScmObj** %stackaddr$env-ref50951
%stackaddr$prim50952 = alloca %struct.ScmObj*, align 8
%_95k40522 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49508)
store volatile %struct.ScmObj* %_95k40522, %struct.ScmObj** %stackaddr$prim50952, align 8
%stackaddr$prim50953 = alloca %struct.ScmObj*, align 8
%current_45args49509 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49508)
store volatile %struct.ScmObj* %current_45args49509, %struct.ScmObj** %stackaddr$prim50953, align 8
%stackaddr$prim50954 = alloca %struct.ScmObj*, align 8
%anf_45bind40315 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49509)
store volatile %struct.ScmObj* %anf_45bind40315, %struct.ScmObj** %stackaddr$prim50954, align 8
%stackaddr$makeclosure50955 = alloca %struct.ScmObj*, align 8
%fptrToInt50956 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41359 to i64
%ae41359 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt50956)
store volatile %struct.ScmObj* %ae41359, %struct.ScmObj** %stackaddr$makeclosure50955, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41359, %struct.ScmObj* %k40519, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41359, %struct.ScmObj* %f40151, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41359, %struct.ScmObj* %acc40150, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41359, %struct.ScmObj* %lsts40149, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41359, %struct.ScmObj* %_37foldr40147, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41359, %struct.ScmObj* %_37foldr140141, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41359, %struct.ScmObj* %_37map140137, i64 6)
%args49542$_37map140137$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50957 = alloca %struct.ScmObj*, align 8
%args49542$_37map140137$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40149, %struct.ScmObj* %args49542$_37map140137$0)
store volatile %struct.ScmObj* %args49542$_37map140137$1, %struct.ScmObj** %stackaddr$prim50957, align 8
%stackaddr$prim50958 = alloca %struct.ScmObj*, align 8
%args49542$_37map140137$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40315, %struct.ScmObj* %args49542$_37map140137$1)
store volatile %struct.ScmObj* %args49542$_37map140137$2, %struct.ScmObj** %stackaddr$prim50958, align 8
%stackaddr$prim50959 = alloca %struct.ScmObj*, align 8
%args49542$_37map140137$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41359, %struct.ScmObj* %args49542$_37map140137$2)
store volatile %struct.ScmObj* %args49542$_37map140137$3, %struct.ScmObj** %stackaddr$prim50959, align 8
%clofunc50960 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140137)
musttail call tailcc void %clofunc50960(%struct.ScmObj* %_37map140137, %struct.ScmObj* %args49542$_37map140137$3)
ret void
}

define tailcc void @proc_clo$ae41359(%struct.ScmObj* %env$ae41359,%struct.ScmObj* %current_45args49511) {
%stackaddr$env-ref50961 = alloca %struct.ScmObj*, align 8
%k40519 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41359, i64 0)
store %struct.ScmObj* %k40519, %struct.ScmObj** %stackaddr$env-ref50961
%stackaddr$env-ref50962 = alloca %struct.ScmObj*, align 8
%f40151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41359, i64 1)
store %struct.ScmObj* %f40151, %struct.ScmObj** %stackaddr$env-ref50962
%stackaddr$env-ref50963 = alloca %struct.ScmObj*, align 8
%acc40150 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41359, i64 2)
store %struct.ScmObj* %acc40150, %struct.ScmObj** %stackaddr$env-ref50963
%stackaddr$env-ref50964 = alloca %struct.ScmObj*, align 8
%lsts40149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41359, i64 3)
store %struct.ScmObj* %lsts40149, %struct.ScmObj** %stackaddr$env-ref50964
%stackaddr$env-ref50965 = alloca %struct.ScmObj*, align 8
%_37foldr40147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41359, i64 4)
store %struct.ScmObj* %_37foldr40147, %struct.ScmObj** %stackaddr$env-ref50965
%stackaddr$env-ref50966 = alloca %struct.ScmObj*, align 8
%_37foldr140141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41359, i64 5)
store %struct.ScmObj* %_37foldr140141, %struct.ScmObj** %stackaddr$env-ref50966
%stackaddr$env-ref50967 = alloca %struct.ScmObj*, align 8
%_37map140137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41359, i64 6)
store %struct.ScmObj* %_37map140137, %struct.ScmObj** %stackaddr$env-ref50967
%stackaddr$prim50968 = alloca %struct.ScmObj*, align 8
%_95k40523 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49511)
store volatile %struct.ScmObj* %_95k40523, %struct.ScmObj** %stackaddr$prim50968, align 8
%stackaddr$prim50969 = alloca %struct.ScmObj*, align 8
%current_45args49512 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49511)
store volatile %struct.ScmObj* %current_45args49512, %struct.ScmObj** %stackaddr$prim50969, align 8
%stackaddr$prim50970 = alloca %struct.ScmObj*, align 8
%lsts_4340156 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49512)
store volatile %struct.ScmObj* %lsts_4340156, %struct.ScmObj** %stackaddr$prim50970, align 8
%stackaddr$makeclosure50971 = alloca %struct.ScmObj*, align 8
%fptrToInt50972 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41362 to i64
%ae41362 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt50972)
store volatile %struct.ScmObj* %ae41362, %struct.ScmObj** %stackaddr$makeclosure50971, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41362, %struct.ScmObj* %k40519, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41362, %struct.ScmObj* %f40151, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41362, %struct.ScmObj* %acc40150, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41362, %struct.ScmObj* %lsts40149, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41362, %struct.ScmObj* %_37foldr40147, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41362, %struct.ScmObj* %_37foldr140141, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41362, %struct.ScmObj* %lsts_4340156, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae41362, %struct.ScmObj* %_37map140137, i64 7)
%ae41363 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50973 = alloca %struct.ScmObj*, align 8
%fptrToInt50974 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41364 to i64
%ae41364 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50974)
store volatile %struct.ScmObj* %ae41364, %struct.ScmObj** %stackaddr$makeclosure50973, align 8
%args49541$ae41362$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50975 = alloca %struct.ScmObj*, align 8
%args49541$ae41362$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41364, %struct.ScmObj* %args49541$ae41362$0)
store volatile %struct.ScmObj* %args49541$ae41362$1, %struct.ScmObj** %stackaddr$prim50975, align 8
%stackaddr$prim50976 = alloca %struct.ScmObj*, align 8
%args49541$ae41362$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41363, %struct.ScmObj* %args49541$ae41362$1)
store volatile %struct.ScmObj* %args49541$ae41362$2, %struct.ScmObj** %stackaddr$prim50976, align 8
%clofunc50977 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41362)
musttail call tailcc void %clofunc50977(%struct.ScmObj* %ae41362, %struct.ScmObj* %args49541$ae41362$2)
ret void
}

define tailcc void @proc_clo$ae41362(%struct.ScmObj* %env$ae41362,%struct.ScmObj* %current_45args49514) {
%stackaddr$env-ref50978 = alloca %struct.ScmObj*, align 8
%k40519 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41362, i64 0)
store %struct.ScmObj* %k40519, %struct.ScmObj** %stackaddr$env-ref50978
%stackaddr$env-ref50979 = alloca %struct.ScmObj*, align 8
%f40151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41362, i64 1)
store %struct.ScmObj* %f40151, %struct.ScmObj** %stackaddr$env-ref50979
%stackaddr$env-ref50980 = alloca %struct.ScmObj*, align 8
%acc40150 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41362, i64 2)
store %struct.ScmObj* %acc40150, %struct.ScmObj** %stackaddr$env-ref50980
%stackaddr$env-ref50981 = alloca %struct.ScmObj*, align 8
%lsts40149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41362, i64 3)
store %struct.ScmObj* %lsts40149, %struct.ScmObj** %stackaddr$env-ref50981
%stackaddr$env-ref50982 = alloca %struct.ScmObj*, align 8
%_37foldr40147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41362, i64 4)
store %struct.ScmObj* %_37foldr40147, %struct.ScmObj** %stackaddr$env-ref50982
%stackaddr$env-ref50983 = alloca %struct.ScmObj*, align 8
%_37foldr140141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41362, i64 5)
store %struct.ScmObj* %_37foldr140141, %struct.ScmObj** %stackaddr$env-ref50983
%stackaddr$env-ref50984 = alloca %struct.ScmObj*, align 8
%lsts_4340156 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41362, i64 6)
store %struct.ScmObj* %lsts_4340156, %struct.ScmObj** %stackaddr$env-ref50984
%stackaddr$env-ref50985 = alloca %struct.ScmObj*, align 8
%_37map140137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41362, i64 7)
store %struct.ScmObj* %_37map140137, %struct.ScmObj** %stackaddr$env-ref50985
%stackaddr$prim50986 = alloca %struct.ScmObj*, align 8
%_95k40524 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49514)
store volatile %struct.ScmObj* %_95k40524, %struct.ScmObj** %stackaddr$prim50986, align 8
%stackaddr$prim50987 = alloca %struct.ScmObj*, align 8
%current_45args49515 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49514)
store volatile %struct.ScmObj* %current_45args49515, %struct.ScmObj** %stackaddr$prim50987, align 8
%stackaddr$prim50988 = alloca %struct.ScmObj*, align 8
%anf_45bind40316 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49515)
store volatile %struct.ScmObj* %anf_45bind40316, %struct.ScmObj** %stackaddr$prim50988, align 8
%stackaddr$makeclosure50989 = alloca %struct.ScmObj*, align 8
%fptrToInt50990 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41383 to i64
%ae41383 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt50990)
store volatile %struct.ScmObj* %ae41383, %struct.ScmObj** %stackaddr$makeclosure50989, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41383, %struct.ScmObj* %k40519, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41383, %struct.ScmObj* %f40151, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41383, %struct.ScmObj* %acc40150, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41383, %struct.ScmObj* %_37foldr40147, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41383, %struct.ScmObj* %_37foldr140141, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41383, %struct.ScmObj* %lsts_4340156, i64 5)
%args49536$_37map140137$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50991 = alloca %struct.ScmObj*, align 8
%args49536$_37map140137$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40149, %struct.ScmObj* %args49536$_37map140137$0)
store volatile %struct.ScmObj* %args49536$_37map140137$1, %struct.ScmObj** %stackaddr$prim50991, align 8
%stackaddr$prim50992 = alloca %struct.ScmObj*, align 8
%args49536$_37map140137$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40316, %struct.ScmObj* %args49536$_37map140137$1)
store volatile %struct.ScmObj* %args49536$_37map140137$2, %struct.ScmObj** %stackaddr$prim50992, align 8
%stackaddr$prim50993 = alloca %struct.ScmObj*, align 8
%args49536$_37map140137$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41383, %struct.ScmObj* %args49536$_37map140137$2)
store volatile %struct.ScmObj* %args49536$_37map140137$3, %struct.ScmObj** %stackaddr$prim50993, align 8
%clofunc50994 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140137)
musttail call tailcc void %clofunc50994(%struct.ScmObj* %_37map140137, %struct.ScmObj* %args49536$_37map140137$3)
ret void
}

define tailcc void @proc_clo$ae41383(%struct.ScmObj* %env$ae41383,%struct.ScmObj* %current_45args49517) {
%stackaddr$env-ref50995 = alloca %struct.ScmObj*, align 8
%k40519 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41383, i64 0)
store %struct.ScmObj* %k40519, %struct.ScmObj** %stackaddr$env-ref50995
%stackaddr$env-ref50996 = alloca %struct.ScmObj*, align 8
%f40151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41383, i64 1)
store %struct.ScmObj* %f40151, %struct.ScmObj** %stackaddr$env-ref50996
%stackaddr$env-ref50997 = alloca %struct.ScmObj*, align 8
%acc40150 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41383, i64 2)
store %struct.ScmObj* %acc40150, %struct.ScmObj** %stackaddr$env-ref50997
%stackaddr$env-ref50998 = alloca %struct.ScmObj*, align 8
%_37foldr40147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41383, i64 3)
store %struct.ScmObj* %_37foldr40147, %struct.ScmObj** %stackaddr$env-ref50998
%stackaddr$env-ref50999 = alloca %struct.ScmObj*, align 8
%_37foldr140141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41383, i64 4)
store %struct.ScmObj* %_37foldr140141, %struct.ScmObj** %stackaddr$env-ref50999
%stackaddr$env-ref51000 = alloca %struct.ScmObj*, align 8
%lsts_4340156 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41383, i64 5)
store %struct.ScmObj* %lsts_4340156, %struct.ScmObj** %stackaddr$env-ref51000
%stackaddr$prim51001 = alloca %struct.ScmObj*, align 8
%_95k40525 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49517)
store volatile %struct.ScmObj* %_95k40525, %struct.ScmObj** %stackaddr$prim51001, align 8
%stackaddr$prim51002 = alloca %struct.ScmObj*, align 8
%current_45args49518 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49517)
store volatile %struct.ScmObj* %current_45args49518, %struct.ScmObj** %stackaddr$prim51002, align 8
%stackaddr$prim51003 = alloca %struct.ScmObj*, align 8
%vs40154 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49518)
store volatile %struct.ScmObj* %vs40154, %struct.ScmObj** %stackaddr$prim51003, align 8
%stackaddr$makeclosure51004 = alloca %struct.ScmObj*, align 8
%fptrToInt51005 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41386 to i64
%ae41386 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt51005)
store volatile %struct.ScmObj* %ae41386, %struct.ScmObj** %stackaddr$makeclosure51004, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41386, %struct.ScmObj* %k40519, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41386, %struct.ScmObj* %f40151, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41386, %struct.ScmObj* %acc40150, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41386, %struct.ScmObj* %_37foldr40147, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41386, %struct.ScmObj* %_37foldr140141, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41386, %struct.ScmObj* %lsts_4340156, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41386, %struct.ScmObj* %vs40154, i64 6)
%ae41387 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure51006 = alloca %struct.ScmObj*, align 8
%fptrToInt51007 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41388 to i64
%ae41388 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt51007)
store volatile %struct.ScmObj* %ae41388, %struct.ScmObj** %stackaddr$makeclosure51006, align 8
%args49535$ae41386$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51008 = alloca %struct.ScmObj*, align 8
%args49535$ae41386$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41388, %struct.ScmObj* %args49535$ae41386$0)
store volatile %struct.ScmObj* %args49535$ae41386$1, %struct.ScmObj** %stackaddr$prim51008, align 8
%stackaddr$prim51009 = alloca %struct.ScmObj*, align 8
%args49535$ae41386$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41387, %struct.ScmObj* %args49535$ae41386$1)
store volatile %struct.ScmObj* %args49535$ae41386$2, %struct.ScmObj** %stackaddr$prim51009, align 8
%clofunc51010 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41386)
musttail call tailcc void %clofunc51010(%struct.ScmObj* %ae41386, %struct.ScmObj* %args49535$ae41386$2)
ret void
}

define tailcc void @proc_clo$ae41386(%struct.ScmObj* %env$ae41386,%struct.ScmObj* %current_45args49520) {
%stackaddr$env-ref51011 = alloca %struct.ScmObj*, align 8
%k40519 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41386, i64 0)
store %struct.ScmObj* %k40519, %struct.ScmObj** %stackaddr$env-ref51011
%stackaddr$env-ref51012 = alloca %struct.ScmObj*, align 8
%f40151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41386, i64 1)
store %struct.ScmObj* %f40151, %struct.ScmObj** %stackaddr$env-ref51012
%stackaddr$env-ref51013 = alloca %struct.ScmObj*, align 8
%acc40150 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41386, i64 2)
store %struct.ScmObj* %acc40150, %struct.ScmObj** %stackaddr$env-ref51013
%stackaddr$env-ref51014 = alloca %struct.ScmObj*, align 8
%_37foldr40147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41386, i64 3)
store %struct.ScmObj* %_37foldr40147, %struct.ScmObj** %stackaddr$env-ref51014
%stackaddr$env-ref51015 = alloca %struct.ScmObj*, align 8
%_37foldr140141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41386, i64 4)
store %struct.ScmObj* %_37foldr140141, %struct.ScmObj** %stackaddr$env-ref51015
%stackaddr$env-ref51016 = alloca %struct.ScmObj*, align 8
%lsts_4340156 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41386, i64 5)
store %struct.ScmObj* %lsts_4340156, %struct.ScmObj** %stackaddr$env-ref51016
%stackaddr$env-ref51017 = alloca %struct.ScmObj*, align 8
%vs40154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41386, i64 6)
store %struct.ScmObj* %vs40154, %struct.ScmObj** %stackaddr$env-ref51017
%stackaddr$prim51018 = alloca %struct.ScmObj*, align 8
%_95k40526 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49520)
store volatile %struct.ScmObj* %_95k40526, %struct.ScmObj** %stackaddr$prim51018, align 8
%stackaddr$prim51019 = alloca %struct.ScmObj*, align 8
%current_45args49521 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49520)
store volatile %struct.ScmObj* %current_45args49521, %struct.ScmObj** %stackaddr$prim51019, align 8
%stackaddr$prim51020 = alloca %struct.ScmObj*, align 8
%anf_45bind40317 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49521)
store volatile %struct.ScmObj* %anf_45bind40317, %struct.ScmObj** %stackaddr$prim51020, align 8
%stackaddr$prim51021 = alloca %struct.ScmObj*, align 8
%anf_45bind40318 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40150, %struct.ScmObj* %lsts_4340156)
store volatile %struct.ScmObj* %anf_45bind40318, %struct.ScmObj** %stackaddr$prim51021, align 8
%stackaddr$prim51022 = alloca %struct.ScmObj*, align 8
%anf_45bind40319 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40151, %struct.ScmObj* %anf_45bind40318)
store volatile %struct.ScmObj* %anf_45bind40319, %struct.ScmObj** %stackaddr$prim51022, align 8
%stackaddr$makeclosure51023 = alloca %struct.ScmObj*, align 8
%fptrToInt51024 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41412 to i64
%ae41412 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt51024)
store volatile %struct.ScmObj* %ae41412, %struct.ScmObj** %stackaddr$makeclosure51023, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41412, %struct.ScmObj* %k40519, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41412, %struct.ScmObj* %f40151, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41412, %struct.ScmObj* %_37foldr140141, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41412, %struct.ScmObj* %anf_45bind40317, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41412, %struct.ScmObj* %vs40154, i64 4)
%stackaddr$prim51025 = alloca %struct.ScmObj*, align 8
%cpsargs40530 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41412, %struct.ScmObj* %anf_45bind40319)
store volatile %struct.ScmObj* %cpsargs40530, %struct.ScmObj** %stackaddr$prim51025, align 8
%clofunc51026 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr40147)
musttail call tailcc void %clofunc51026(%struct.ScmObj* %_37foldr40147, %struct.ScmObj* %cpsargs40530)
ret void
}

define tailcc void @proc_clo$ae41412(%struct.ScmObj* %env$ae41412,%struct.ScmObj* %current_45args49523) {
%stackaddr$env-ref51027 = alloca %struct.ScmObj*, align 8
%k40519 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41412, i64 0)
store %struct.ScmObj* %k40519, %struct.ScmObj** %stackaddr$env-ref51027
%stackaddr$env-ref51028 = alloca %struct.ScmObj*, align 8
%f40151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41412, i64 1)
store %struct.ScmObj* %f40151, %struct.ScmObj** %stackaddr$env-ref51028
%stackaddr$env-ref51029 = alloca %struct.ScmObj*, align 8
%_37foldr140141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41412, i64 2)
store %struct.ScmObj* %_37foldr140141, %struct.ScmObj** %stackaddr$env-ref51029
%stackaddr$env-ref51030 = alloca %struct.ScmObj*, align 8
%anf_45bind40317 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41412, i64 3)
store %struct.ScmObj* %anf_45bind40317, %struct.ScmObj** %stackaddr$env-ref51030
%stackaddr$env-ref51031 = alloca %struct.ScmObj*, align 8
%vs40154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41412, i64 4)
store %struct.ScmObj* %vs40154, %struct.ScmObj** %stackaddr$env-ref51031
%stackaddr$prim51032 = alloca %struct.ScmObj*, align 8
%_95k40527 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49523)
store volatile %struct.ScmObj* %_95k40527, %struct.ScmObj** %stackaddr$prim51032, align 8
%stackaddr$prim51033 = alloca %struct.ScmObj*, align 8
%current_45args49524 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49523)
store volatile %struct.ScmObj* %current_45args49524, %struct.ScmObj** %stackaddr$prim51033, align 8
%stackaddr$prim51034 = alloca %struct.ScmObj*, align 8
%anf_45bind40320 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49524)
store volatile %struct.ScmObj* %anf_45bind40320, %struct.ScmObj** %stackaddr$prim51034, align 8
%ae41417 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51035 = alloca %struct.ScmObj*, align 8
%anf_45bind40321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40320, %struct.ScmObj* %ae41417)
store volatile %struct.ScmObj* %anf_45bind40321, %struct.ScmObj** %stackaddr$prim51035, align 8
%stackaddr$makeclosure51036 = alloca %struct.ScmObj*, align 8
%fptrToInt51037 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41419 to i64
%ae41419 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt51037)
store volatile %struct.ScmObj* %ae41419, %struct.ScmObj** %stackaddr$makeclosure51036, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41419, %struct.ScmObj* %k40519, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41419, %struct.ScmObj* %f40151, i64 1)
%args49529$_37foldr140141$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51038 = alloca %struct.ScmObj*, align 8
%args49529$_37foldr140141$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs40154, %struct.ScmObj* %args49529$_37foldr140141$0)
store volatile %struct.ScmObj* %args49529$_37foldr140141$1, %struct.ScmObj** %stackaddr$prim51038, align 8
%stackaddr$prim51039 = alloca %struct.ScmObj*, align 8
%args49529$_37foldr140141$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40321, %struct.ScmObj* %args49529$_37foldr140141$1)
store volatile %struct.ScmObj* %args49529$_37foldr140141$2, %struct.ScmObj** %stackaddr$prim51039, align 8
%stackaddr$prim51040 = alloca %struct.ScmObj*, align 8
%args49529$_37foldr140141$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40317, %struct.ScmObj* %args49529$_37foldr140141$2)
store volatile %struct.ScmObj* %args49529$_37foldr140141$3, %struct.ScmObj** %stackaddr$prim51040, align 8
%stackaddr$prim51041 = alloca %struct.ScmObj*, align 8
%args49529$_37foldr140141$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41419, %struct.ScmObj* %args49529$_37foldr140141$3)
store volatile %struct.ScmObj* %args49529$_37foldr140141$4, %struct.ScmObj** %stackaddr$prim51041, align 8
%clofunc51042 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140141)
musttail call tailcc void %clofunc51042(%struct.ScmObj* %_37foldr140141, %struct.ScmObj* %args49529$_37foldr140141$4)
ret void
}

define tailcc void @proc_clo$ae41419(%struct.ScmObj* %env$ae41419,%struct.ScmObj* %current_45args49526) {
%stackaddr$env-ref51043 = alloca %struct.ScmObj*, align 8
%k40519 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41419, i64 0)
store %struct.ScmObj* %k40519, %struct.ScmObj** %stackaddr$env-ref51043
%stackaddr$env-ref51044 = alloca %struct.ScmObj*, align 8
%f40151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41419, i64 1)
store %struct.ScmObj* %f40151, %struct.ScmObj** %stackaddr$env-ref51044
%stackaddr$prim51045 = alloca %struct.ScmObj*, align 8
%_95k40528 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49526)
store volatile %struct.ScmObj* %_95k40528, %struct.ScmObj** %stackaddr$prim51045, align 8
%stackaddr$prim51046 = alloca %struct.ScmObj*, align 8
%current_45args49527 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49526)
store volatile %struct.ScmObj* %current_45args49527, %struct.ScmObj** %stackaddr$prim51046, align 8
%stackaddr$prim51047 = alloca %struct.ScmObj*, align 8
%anf_45bind40322 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49527)
store volatile %struct.ScmObj* %anf_45bind40322, %struct.ScmObj** %stackaddr$prim51047, align 8
%stackaddr$prim51048 = alloca %struct.ScmObj*, align 8
%cpsargs40529 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40519, %struct.ScmObj* %anf_45bind40322)
store volatile %struct.ScmObj* %cpsargs40529, %struct.ScmObj** %stackaddr$prim51048, align 8
%clofunc51049 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40151)
musttail call tailcc void %clofunc51049(%struct.ScmObj* %f40151, %struct.ScmObj* %cpsargs40529)
ret void
}

define tailcc void @proc_clo$ae41388(%struct.ScmObj* %env$ae41388,%struct.ScmObj* %current_45args49530) {
%stackaddr$prim51050 = alloca %struct.ScmObj*, align 8
%k40531 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49530)
store volatile %struct.ScmObj* %k40531, %struct.ScmObj** %stackaddr$prim51050, align 8
%stackaddr$prim51051 = alloca %struct.ScmObj*, align 8
%current_45args49531 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49530)
store volatile %struct.ScmObj* %current_45args49531, %struct.ScmObj** %stackaddr$prim51051, align 8
%stackaddr$prim51052 = alloca %struct.ScmObj*, align 8
%a40159 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49531)
store volatile %struct.ScmObj* %a40159, %struct.ScmObj** %stackaddr$prim51052, align 8
%stackaddr$prim51053 = alloca %struct.ScmObj*, align 8
%current_45args49532 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49531)
store volatile %struct.ScmObj* %current_45args49532, %struct.ScmObj** %stackaddr$prim51053, align 8
%stackaddr$prim51054 = alloca %struct.ScmObj*, align 8
%b40158 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49532)
store volatile %struct.ScmObj* %b40158, %struct.ScmObj** %stackaddr$prim51054, align 8
%stackaddr$prim51055 = alloca %struct.ScmObj*, align 8
%cpsprim40532 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a40159, %struct.ScmObj* %b40158)
store volatile %struct.ScmObj* %cpsprim40532, %struct.ScmObj** %stackaddr$prim51055, align 8
%ae41392 = call %struct.ScmObj* @const_init_int(i64 0)
%args49534$k40531$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51056 = alloca %struct.ScmObj*, align 8
%args49534$k40531$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40532, %struct.ScmObj* %args49534$k40531$0)
store volatile %struct.ScmObj* %args49534$k40531$1, %struct.ScmObj** %stackaddr$prim51056, align 8
%stackaddr$prim51057 = alloca %struct.ScmObj*, align 8
%args49534$k40531$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41392, %struct.ScmObj* %args49534$k40531$1)
store volatile %struct.ScmObj* %args49534$k40531$2, %struct.ScmObj** %stackaddr$prim51057, align 8
%clofunc51058 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40531)
musttail call tailcc void %clofunc51058(%struct.ScmObj* %k40531, %struct.ScmObj* %args49534$k40531$2)
ret void
}

define tailcc void @proc_clo$ae41364(%struct.ScmObj* %env$ae41364,%struct.ScmObj* %current_45args49537) {
%stackaddr$prim51059 = alloca %struct.ScmObj*, align 8
%k40533 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49537)
store volatile %struct.ScmObj* %k40533, %struct.ScmObj** %stackaddr$prim51059, align 8
%stackaddr$prim51060 = alloca %struct.ScmObj*, align 8
%current_45args49538 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49537)
store volatile %struct.ScmObj* %current_45args49538, %struct.ScmObj** %stackaddr$prim51060, align 8
%stackaddr$prim51061 = alloca %struct.ScmObj*, align 8
%x40155 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49538)
store volatile %struct.ScmObj* %x40155, %struct.ScmObj** %stackaddr$prim51061, align 8
%stackaddr$prim51062 = alloca %struct.ScmObj*, align 8
%cpsprim40534 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40155)
store volatile %struct.ScmObj* %cpsprim40534, %struct.ScmObj** %stackaddr$prim51062, align 8
%ae41367 = call %struct.ScmObj* @const_init_int(i64 0)
%args49540$k40533$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51063 = alloca %struct.ScmObj*, align 8
%args49540$k40533$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40534, %struct.ScmObj* %args49540$k40533$0)
store volatile %struct.ScmObj* %args49540$k40533$1, %struct.ScmObj** %stackaddr$prim51063, align 8
%stackaddr$prim51064 = alloca %struct.ScmObj*, align 8
%args49540$k40533$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41367, %struct.ScmObj* %args49540$k40533$1)
store volatile %struct.ScmObj* %args49540$k40533$2, %struct.ScmObj** %stackaddr$prim51064, align 8
%clofunc51065 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40533)
musttail call tailcc void %clofunc51065(%struct.ScmObj* %k40533, %struct.ScmObj* %args49540$k40533$2)
ret void
}

define tailcc void @proc_clo$ae41340(%struct.ScmObj* %env$ae41340,%struct.ScmObj* %current_45args49543) {
%stackaddr$prim51066 = alloca %struct.ScmObj*, align 8
%k40535 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49543)
store volatile %struct.ScmObj* %k40535, %struct.ScmObj** %stackaddr$prim51066, align 8
%stackaddr$prim51067 = alloca %struct.ScmObj*, align 8
%current_45args49544 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49543)
store volatile %struct.ScmObj* %current_45args49544, %struct.ScmObj** %stackaddr$prim51067, align 8
%stackaddr$prim51068 = alloca %struct.ScmObj*, align 8
%x40157 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49544)
store volatile %struct.ScmObj* %x40157, %struct.ScmObj** %stackaddr$prim51068, align 8
%stackaddr$prim51069 = alloca %struct.ScmObj*, align 8
%cpsprim40536 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40157)
store volatile %struct.ScmObj* %cpsprim40536, %struct.ScmObj** %stackaddr$prim51069, align 8
%ae41343 = call %struct.ScmObj* @const_init_int(i64 0)
%args49546$k40535$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51070 = alloca %struct.ScmObj*, align 8
%args49546$k40535$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40536, %struct.ScmObj* %args49546$k40535$0)
store volatile %struct.ScmObj* %args49546$k40535$1, %struct.ScmObj** %stackaddr$prim51070, align 8
%stackaddr$prim51071 = alloca %struct.ScmObj*, align 8
%args49546$k40535$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41343, %struct.ScmObj* %args49546$k40535$1)
store volatile %struct.ScmObj* %args49546$k40535$2, %struct.ScmObj** %stackaddr$prim51071, align 8
%clofunc51072 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40535)
musttail call tailcc void %clofunc51072(%struct.ScmObj* %k40535, %struct.ScmObj* %args49546$k40535$2)
ret void
}

define tailcc void @proc_clo$ae41292(%struct.ScmObj* %env$ae41292,%struct.ScmObj* %current_45args49549) {
%stackaddr$prim51073 = alloca %struct.ScmObj*, align 8
%k40537 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49549)
store volatile %struct.ScmObj* %k40537, %struct.ScmObj** %stackaddr$prim51073, align 8
%stackaddr$prim51074 = alloca %struct.ScmObj*, align 8
%current_45args49550 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49549)
store volatile %struct.ScmObj* %current_45args49550, %struct.ScmObj** %stackaddr$prim51074, align 8
%stackaddr$prim51075 = alloca %struct.ScmObj*, align 8
%lst40153 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49550)
store volatile %struct.ScmObj* %lst40153, %struct.ScmObj** %stackaddr$prim51075, align 8
%stackaddr$prim51076 = alloca %struct.ScmObj*, align 8
%current_45args49551 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49550)
store volatile %struct.ScmObj* %current_45args49551, %struct.ScmObj** %stackaddr$prim51076, align 8
%stackaddr$prim51077 = alloca %struct.ScmObj*, align 8
%b40152 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49551)
store volatile %struct.ScmObj* %b40152, %struct.ScmObj** %stackaddr$prim51077, align 8
%truthy$cmp51078 = call i64 @is_truthy_value(%struct.ScmObj* %b40152)
%cmp$cmp51078 = icmp eq i64 %truthy$cmp51078, 1
br i1 %cmp$cmp51078, label %truebranch$cmp51078, label %falsebranch$cmp51078
truebranch$cmp51078:
%ae41295 = call %struct.ScmObj* @const_init_int(i64 0)
%args49553$k40537$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51079 = alloca %struct.ScmObj*, align 8
%args49553$k40537$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b40152, %struct.ScmObj* %args49553$k40537$0)
store volatile %struct.ScmObj* %args49553$k40537$1, %struct.ScmObj** %stackaddr$prim51079, align 8
%stackaddr$prim51080 = alloca %struct.ScmObj*, align 8
%args49553$k40537$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41295, %struct.ScmObj* %args49553$k40537$1)
store volatile %struct.ScmObj* %args49553$k40537$2, %struct.ScmObj** %stackaddr$prim51080, align 8
%clofunc51081 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40537)
musttail call tailcc void %clofunc51081(%struct.ScmObj* %k40537, %struct.ScmObj* %args49553$k40537$2)
ret void
falsebranch$cmp51078:
%stackaddr$prim51082 = alloca %struct.ScmObj*, align 8
%cpsprim40538 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40153)
store volatile %struct.ScmObj* %cpsprim40538, %struct.ScmObj** %stackaddr$prim51082, align 8
%ae41302 = call %struct.ScmObj* @const_init_int(i64 0)
%args49554$k40537$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51083 = alloca %struct.ScmObj*, align 8
%args49554$k40537$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40538, %struct.ScmObj* %args49554$k40537$0)
store volatile %struct.ScmObj* %args49554$k40537$1, %struct.ScmObj** %stackaddr$prim51083, align 8
%stackaddr$prim51084 = alloca %struct.ScmObj*, align 8
%args49554$k40537$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41302, %struct.ScmObj* %args49554$k40537$1)
store volatile %struct.ScmObj* %args49554$k40537$2, %struct.ScmObj** %stackaddr$prim51084, align 8
%clofunc51085 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40537)
musttail call tailcc void %clofunc51085(%struct.ScmObj* %k40537, %struct.ScmObj* %args49554$k40537$2)
ret void
}

define tailcc void @proc_clo$ae41249(%struct.ScmObj* %env$ae41249,%struct.ScmObj* %current_45args49558) {
%stackaddr$env-ref51086 = alloca %struct.ScmObj*, align 8
%_37take40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41249, i64 0)
store %struct.ScmObj* %_37take40133, %struct.ScmObj** %stackaddr$env-ref51086
%stackaddr$env-ref51087 = alloca %struct.ScmObj*, align 8
%_37length40130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41249, i64 1)
store %struct.ScmObj* %_37length40130, %struct.ScmObj** %stackaddr$env-ref51087
%stackaddr$prim51088 = alloca %struct.ScmObj*, align 8
%k40539 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49558)
store volatile %struct.ScmObj* %k40539, %struct.ScmObj** %stackaddr$prim51088, align 8
%stackaddr$prim51089 = alloca %struct.ScmObj*, align 8
%current_45args49559 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49558)
store volatile %struct.ScmObj* %current_45args49559, %struct.ScmObj** %stackaddr$prim51089, align 8
%stackaddr$prim51090 = alloca %struct.ScmObj*, align 8
%lst40162 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49559)
store volatile %struct.ScmObj* %lst40162, %struct.ScmObj** %stackaddr$prim51090, align 8
%stackaddr$prim51091 = alloca %struct.ScmObj*, align 8
%current_45args49560 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49559)
store volatile %struct.ScmObj* %current_45args49560, %struct.ScmObj** %stackaddr$prim51091, align 8
%stackaddr$prim51092 = alloca %struct.ScmObj*, align 8
%n40161 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49560)
store volatile %struct.ScmObj* %n40161, %struct.ScmObj** %stackaddr$prim51092, align 8
%stackaddr$makeclosure51093 = alloca %struct.ScmObj*, align 8
%fptrToInt51094 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41251 to i64
%ae41251 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt51094)
store volatile %struct.ScmObj* %ae41251, %struct.ScmObj** %stackaddr$makeclosure51093, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41251, %struct.ScmObj* %_37take40133, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41251, %struct.ScmObj* %lst40162, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41251, %struct.ScmObj* %n40161, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41251, %struct.ScmObj* %k40539, i64 3)
%args49566$_37length40130$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51095 = alloca %struct.ScmObj*, align 8
%args49566$_37length40130$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40162, %struct.ScmObj* %args49566$_37length40130$0)
store volatile %struct.ScmObj* %args49566$_37length40130$1, %struct.ScmObj** %stackaddr$prim51095, align 8
%stackaddr$prim51096 = alloca %struct.ScmObj*, align 8
%args49566$_37length40130$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41251, %struct.ScmObj* %args49566$_37length40130$1)
store volatile %struct.ScmObj* %args49566$_37length40130$2, %struct.ScmObj** %stackaddr$prim51096, align 8
%clofunc51097 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length40130)
musttail call tailcc void %clofunc51097(%struct.ScmObj* %_37length40130, %struct.ScmObj* %args49566$_37length40130$2)
ret void
}

define tailcc void @proc_clo$ae41251(%struct.ScmObj* %env$ae41251,%struct.ScmObj* %current_45args49562) {
%stackaddr$env-ref51098 = alloca %struct.ScmObj*, align 8
%_37take40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41251, i64 0)
store %struct.ScmObj* %_37take40133, %struct.ScmObj** %stackaddr$env-ref51098
%stackaddr$env-ref51099 = alloca %struct.ScmObj*, align 8
%lst40162 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41251, i64 1)
store %struct.ScmObj* %lst40162, %struct.ScmObj** %stackaddr$env-ref51099
%stackaddr$env-ref51100 = alloca %struct.ScmObj*, align 8
%n40161 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41251, i64 2)
store %struct.ScmObj* %n40161, %struct.ScmObj** %stackaddr$env-ref51100
%stackaddr$env-ref51101 = alloca %struct.ScmObj*, align 8
%k40539 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41251, i64 3)
store %struct.ScmObj* %k40539, %struct.ScmObj** %stackaddr$env-ref51101
%stackaddr$prim51102 = alloca %struct.ScmObj*, align 8
%_95k40540 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49562)
store volatile %struct.ScmObj* %_95k40540, %struct.ScmObj** %stackaddr$prim51102, align 8
%stackaddr$prim51103 = alloca %struct.ScmObj*, align 8
%current_45args49563 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49562)
store volatile %struct.ScmObj* %current_45args49563, %struct.ScmObj** %stackaddr$prim51103, align 8
%stackaddr$prim51104 = alloca %struct.ScmObj*, align 8
%anf_45bind40309 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49563)
store volatile %struct.ScmObj* %anf_45bind40309, %struct.ScmObj** %stackaddr$prim51104, align 8
%stackaddr$prim51105 = alloca %struct.ScmObj*, align 8
%anf_45bind40310 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind40309, %struct.ScmObj* %n40161)
store volatile %struct.ScmObj* %anf_45bind40310, %struct.ScmObj** %stackaddr$prim51105, align 8
%args49565$_37take40133$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51106 = alloca %struct.ScmObj*, align 8
%args49565$_37take40133$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40310, %struct.ScmObj* %args49565$_37take40133$0)
store volatile %struct.ScmObj* %args49565$_37take40133$1, %struct.ScmObj** %stackaddr$prim51106, align 8
%stackaddr$prim51107 = alloca %struct.ScmObj*, align 8
%args49565$_37take40133$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40162, %struct.ScmObj* %args49565$_37take40133$1)
store volatile %struct.ScmObj* %args49565$_37take40133$2, %struct.ScmObj** %stackaddr$prim51107, align 8
%stackaddr$prim51108 = alloca %struct.ScmObj*, align 8
%args49565$_37take40133$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40539, %struct.ScmObj* %args49565$_37take40133$2)
store volatile %struct.ScmObj* %args49565$_37take40133$3, %struct.ScmObj** %stackaddr$prim51108, align 8
%clofunc51109 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take40133)
musttail call tailcc void %clofunc51109(%struct.ScmObj* %_37take40133, %struct.ScmObj* %args49565$_37take40133$3)
ret void
}

define tailcc void @proc_clo$ae41195(%struct.ScmObj* %env$ae41195,%struct.ScmObj* %current_45args49568) {
%stackaddr$env-ref51110 = alloca %struct.ScmObj*, align 8
%_37foldl140125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41195, i64 0)
store %struct.ScmObj* %_37foldl140125, %struct.ScmObj** %stackaddr$env-ref51110
%stackaddr$prim51111 = alloca %struct.ScmObj*, align 8
%k40541 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49568)
store volatile %struct.ScmObj* %k40541, %struct.ScmObj** %stackaddr$prim51111, align 8
%stackaddr$prim51112 = alloca %struct.ScmObj*, align 8
%current_45args49569 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49568)
store volatile %struct.ScmObj* %current_45args49569, %struct.ScmObj** %stackaddr$prim51112, align 8
%stackaddr$prim51113 = alloca %struct.ScmObj*, align 8
%lst40164 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49569)
store volatile %struct.ScmObj* %lst40164, %struct.ScmObj** %stackaddr$prim51113, align 8
%stackaddr$makeclosure51114 = alloca %struct.ScmObj*, align 8
%fptrToInt51115 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41196 to i64
%ae41196 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt51115)
store volatile %struct.ScmObj* %ae41196, %struct.ScmObj** %stackaddr$makeclosure51114, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41196, %struct.ScmObj* %k40541, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41196, %struct.ScmObj* %_37foldl140125, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41196, %struct.ScmObj* %lst40164, i64 2)
%ae41197 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure51116 = alloca %struct.ScmObj*, align 8
%fptrToInt51117 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41198 to i64
%ae41198 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt51117)
store volatile %struct.ScmObj* %ae41198, %struct.ScmObj** %stackaddr$makeclosure51116, align 8
%args49580$ae41196$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51118 = alloca %struct.ScmObj*, align 8
%args49580$ae41196$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41198, %struct.ScmObj* %args49580$ae41196$0)
store volatile %struct.ScmObj* %args49580$ae41196$1, %struct.ScmObj** %stackaddr$prim51118, align 8
%stackaddr$prim51119 = alloca %struct.ScmObj*, align 8
%args49580$ae41196$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41197, %struct.ScmObj* %args49580$ae41196$1)
store volatile %struct.ScmObj* %args49580$ae41196$2, %struct.ScmObj** %stackaddr$prim51119, align 8
%clofunc51120 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41196)
musttail call tailcc void %clofunc51120(%struct.ScmObj* %ae41196, %struct.ScmObj* %args49580$ae41196$2)
ret void
}

define tailcc void @proc_clo$ae41196(%struct.ScmObj* %env$ae41196,%struct.ScmObj* %current_45args49571) {
%stackaddr$env-ref51121 = alloca %struct.ScmObj*, align 8
%k40541 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41196, i64 0)
store %struct.ScmObj* %k40541, %struct.ScmObj** %stackaddr$env-ref51121
%stackaddr$env-ref51122 = alloca %struct.ScmObj*, align 8
%_37foldl140125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41196, i64 1)
store %struct.ScmObj* %_37foldl140125, %struct.ScmObj** %stackaddr$env-ref51122
%stackaddr$env-ref51123 = alloca %struct.ScmObj*, align 8
%lst40164 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41196, i64 2)
store %struct.ScmObj* %lst40164, %struct.ScmObj** %stackaddr$env-ref51123
%stackaddr$prim51124 = alloca %struct.ScmObj*, align 8
%_95k40542 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49571)
store volatile %struct.ScmObj* %_95k40542, %struct.ScmObj** %stackaddr$prim51124, align 8
%stackaddr$prim51125 = alloca %struct.ScmObj*, align 8
%current_45args49572 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49571)
store volatile %struct.ScmObj* %current_45args49572, %struct.ScmObj** %stackaddr$prim51125, align 8
%stackaddr$prim51126 = alloca %struct.ScmObj*, align 8
%anf_45bind40308 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49572)
store volatile %struct.ScmObj* %anf_45bind40308, %struct.ScmObj** %stackaddr$prim51126, align 8
%ae41217 = call %struct.ScmObj* @const_init_null()
%args49574$_37foldl140125$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51127 = alloca %struct.ScmObj*, align 8
%args49574$_37foldl140125$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40164, %struct.ScmObj* %args49574$_37foldl140125$0)
store volatile %struct.ScmObj* %args49574$_37foldl140125$1, %struct.ScmObj** %stackaddr$prim51127, align 8
%stackaddr$prim51128 = alloca %struct.ScmObj*, align 8
%args49574$_37foldl140125$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41217, %struct.ScmObj* %args49574$_37foldl140125$1)
store volatile %struct.ScmObj* %args49574$_37foldl140125$2, %struct.ScmObj** %stackaddr$prim51128, align 8
%stackaddr$prim51129 = alloca %struct.ScmObj*, align 8
%args49574$_37foldl140125$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40308, %struct.ScmObj* %args49574$_37foldl140125$2)
store volatile %struct.ScmObj* %args49574$_37foldl140125$3, %struct.ScmObj** %stackaddr$prim51129, align 8
%stackaddr$prim51130 = alloca %struct.ScmObj*, align 8
%args49574$_37foldl140125$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40541, %struct.ScmObj* %args49574$_37foldl140125$3)
store volatile %struct.ScmObj* %args49574$_37foldl140125$4, %struct.ScmObj** %stackaddr$prim51130, align 8
%clofunc51131 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140125)
musttail call tailcc void %clofunc51131(%struct.ScmObj* %_37foldl140125, %struct.ScmObj* %args49574$_37foldl140125$4)
ret void
}

define tailcc void @proc_clo$ae41198(%struct.ScmObj* %env$ae41198,%struct.ScmObj* %current_45args49575) {
%stackaddr$prim51132 = alloca %struct.ScmObj*, align 8
%k40543 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49575)
store volatile %struct.ScmObj* %k40543, %struct.ScmObj** %stackaddr$prim51132, align 8
%stackaddr$prim51133 = alloca %struct.ScmObj*, align 8
%current_45args49576 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49575)
store volatile %struct.ScmObj* %current_45args49576, %struct.ScmObj** %stackaddr$prim51133, align 8
%stackaddr$prim51134 = alloca %struct.ScmObj*, align 8
%x40166 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49576)
store volatile %struct.ScmObj* %x40166, %struct.ScmObj** %stackaddr$prim51134, align 8
%stackaddr$prim51135 = alloca %struct.ScmObj*, align 8
%current_45args49577 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49576)
store volatile %struct.ScmObj* %current_45args49577, %struct.ScmObj** %stackaddr$prim51135, align 8
%stackaddr$prim51136 = alloca %struct.ScmObj*, align 8
%y40165 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49577)
store volatile %struct.ScmObj* %y40165, %struct.ScmObj** %stackaddr$prim51136, align 8
%ae41200 = call %struct.ScmObj* @const_init_int(i64 0)
%args49579$k40543$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51137 = alloca %struct.ScmObj*, align 8
%args49579$k40543$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x40166, %struct.ScmObj* %args49579$k40543$0)
store volatile %struct.ScmObj* %args49579$k40543$1, %struct.ScmObj** %stackaddr$prim51137, align 8
%stackaddr$prim51138 = alloca %struct.ScmObj*, align 8
%args49579$k40543$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41200, %struct.ScmObj* %args49579$k40543$1)
store volatile %struct.ScmObj* %args49579$k40543$2, %struct.ScmObj** %stackaddr$prim51138, align 8
%clofunc51139 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40543)
musttail call tailcc void %clofunc51139(%struct.ScmObj* %k40543, %struct.ScmObj* %args49579$k40543$2)
ret void
}

define tailcc void @proc_clo$ae41116(%struct.ScmObj* %env$ae41116,%struct.ScmObj* %current_45args49583) {
%stackaddr$prim51140 = alloca %struct.ScmObj*, align 8
%k40544 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49583)
store volatile %struct.ScmObj* %k40544, %struct.ScmObj** %stackaddr$prim51140, align 8
%stackaddr$prim51141 = alloca %struct.ScmObj*, align 8
%current_45args49584 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49583)
store volatile %struct.ScmObj* %current_45args49584, %struct.ScmObj** %stackaddr$prim51141, align 8
%stackaddr$prim51142 = alloca %struct.ScmObj*, align 8
%_37foldl140126 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49584)
store volatile %struct.ScmObj* %_37foldl140126, %struct.ScmObj** %stackaddr$prim51142, align 8
%ae41118 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure51143 = alloca %struct.ScmObj*, align 8
%fptrToInt51144 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41119 to i64
%ae41119 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt51144)
store volatile %struct.ScmObj* %ae41119, %struct.ScmObj** %stackaddr$makeclosure51143, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41119, %struct.ScmObj* %_37foldl140126, i64 0)
%args49597$k40544$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51145 = alloca %struct.ScmObj*, align 8
%args49597$k40544$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41119, %struct.ScmObj* %args49597$k40544$0)
store volatile %struct.ScmObj* %args49597$k40544$1, %struct.ScmObj** %stackaddr$prim51145, align 8
%stackaddr$prim51146 = alloca %struct.ScmObj*, align 8
%args49597$k40544$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41118, %struct.ScmObj* %args49597$k40544$1)
store volatile %struct.ScmObj* %args49597$k40544$2, %struct.ScmObj** %stackaddr$prim51146, align 8
%clofunc51147 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40544)
musttail call tailcc void %clofunc51147(%struct.ScmObj* %k40544, %struct.ScmObj* %args49597$k40544$2)
ret void
}

define tailcc void @proc_clo$ae41119(%struct.ScmObj* %env$ae41119,%struct.ScmObj* %current_45args49586) {
%stackaddr$env-ref51148 = alloca %struct.ScmObj*, align 8
%_37foldl140126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41119, i64 0)
store %struct.ScmObj* %_37foldl140126, %struct.ScmObj** %stackaddr$env-ref51148
%stackaddr$prim51149 = alloca %struct.ScmObj*, align 8
%k40545 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49586)
store volatile %struct.ScmObj* %k40545, %struct.ScmObj** %stackaddr$prim51149, align 8
%stackaddr$prim51150 = alloca %struct.ScmObj*, align 8
%current_45args49587 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49586)
store volatile %struct.ScmObj* %current_45args49587, %struct.ScmObj** %stackaddr$prim51150, align 8
%stackaddr$prim51151 = alloca %struct.ScmObj*, align 8
%f40129 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49587)
store volatile %struct.ScmObj* %f40129, %struct.ScmObj** %stackaddr$prim51151, align 8
%stackaddr$prim51152 = alloca %struct.ScmObj*, align 8
%current_45args49588 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49587)
store volatile %struct.ScmObj* %current_45args49588, %struct.ScmObj** %stackaddr$prim51152, align 8
%stackaddr$prim51153 = alloca %struct.ScmObj*, align 8
%acc40128 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49588)
store volatile %struct.ScmObj* %acc40128, %struct.ScmObj** %stackaddr$prim51153, align 8
%stackaddr$prim51154 = alloca %struct.ScmObj*, align 8
%current_45args49589 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49588)
store volatile %struct.ScmObj* %current_45args49589, %struct.ScmObj** %stackaddr$prim51154, align 8
%stackaddr$prim51155 = alloca %struct.ScmObj*, align 8
%lst40127 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49589)
store volatile %struct.ScmObj* %lst40127, %struct.ScmObj** %stackaddr$prim51155, align 8
%stackaddr$prim51156 = alloca %struct.ScmObj*, align 8
%anf_45bind40303 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40127)
store volatile %struct.ScmObj* %anf_45bind40303, %struct.ScmObj** %stackaddr$prim51156, align 8
%truthy$cmp51157 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40303)
%cmp$cmp51157 = icmp eq i64 %truthy$cmp51157, 1
br i1 %cmp$cmp51157, label %truebranch$cmp51157, label %falsebranch$cmp51157
truebranch$cmp51157:
%ae41123 = call %struct.ScmObj* @const_init_int(i64 0)
%args49591$k40545$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51158 = alloca %struct.ScmObj*, align 8
%args49591$k40545$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40128, %struct.ScmObj* %args49591$k40545$0)
store volatile %struct.ScmObj* %args49591$k40545$1, %struct.ScmObj** %stackaddr$prim51158, align 8
%stackaddr$prim51159 = alloca %struct.ScmObj*, align 8
%args49591$k40545$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41123, %struct.ScmObj* %args49591$k40545$1)
store volatile %struct.ScmObj* %args49591$k40545$2, %struct.ScmObj** %stackaddr$prim51159, align 8
%clofunc51160 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40545)
musttail call tailcc void %clofunc51160(%struct.ScmObj* %k40545, %struct.ScmObj* %args49591$k40545$2)
ret void
falsebranch$cmp51157:
%stackaddr$prim51161 = alloca %struct.ScmObj*, align 8
%anf_45bind40304 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40127)
store volatile %struct.ScmObj* %anf_45bind40304, %struct.ScmObj** %stackaddr$prim51161, align 8
%stackaddr$makeclosure51162 = alloca %struct.ScmObj*, align 8
%fptrToInt51163 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41130 to i64
%ae41130 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt51163)
store volatile %struct.ScmObj* %ae41130, %struct.ScmObj** %stackaddr$makeclosure51162, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41130, %struct.ScmObj* %k40545, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41130, %struct.ScmObj* %f40129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41130, %struct.ScmObj* %lst40127, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41130, %struct.ScmObj* %_37foldl140126, i64 3)
%args49596$f40129$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51164 = alloca %struct.ScmObj*, align 8
%args49596$f40129$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40128, %struct.ScmObj* %args49596$f40129$0)
store volatile %struct.ScmObj* %args49596$f40129$1, %struct.ScmObj** %stackaddr$prim51164, align 8
%stackaddr$prim51165 = alloca %struct.ScmObj*, align 8
%args49596$f40129$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40304, %struct.ScmObj* %args49596$f40129$1)
store volatile %struct.ScmObj* %args49596$f40129$2, %struct.ScmObj** %stackaddr$prim51165, align 8
%stackaddr$prim51166 = alloca %struct.ScmObj*, align 8
%args49596$f40129$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41130, %struct.ScmObj* %args49596$f40129$2)
store volatile %struct.ScmObj* %args49596$f40129$3, %struct.ScmObj** %stackaddr$prim51166, align 8
%clofunc51167 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40129)
musttail call tailcc void %clofunc51167(%struct.ScmObj* %f40129, %struct.ScmObj* %args49596$f40129$3)
ret void
}

define tailcc void @proc_clo$ae41130(%struct.ScmObj* %env$ae41130,%struct.ScmObj* %current_45args49592) {
%stackaddr$env-ref51168 = alloca %struct.ScmObj*, align 8
%k40545 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41130, i64 0)
store %struct.ScmObj* %k40545, %struct.ScmObj** %stackaddr$env-ref51168
%stackaddr$env-ref51169 = alloca %struct.ScmObj*, align 8
%f40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41130, i64 1)
store %struct.ScmObj* %f40129, %struct.ScmObj** %stackaddr$env-ref51169
%stackaddr$env-ref51170 = alloca %struct.ScmObj*, align 8
%lst40127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41130, i64 2)
store %struct.ScmObj* %lst40127, %struct.ScmObj** %stackaddr$env-ref51170
%stackaddr$env-ref51171 = alloca %struct.ScmObj*, align 8
%_37foldl140126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41130, i64 3)
store %struct.ScmObj* %_37foldl140126, %struct.ScmObj** %stackaddr$env-ref51171
%stackaddr$prim51172 = alloca %struct.ScmObj*, align 8
%_95k40546 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49592)
store volatile %struct.ScmObj* %_95k40546, %struct.ScmObj** %stackaddr$prim51172, align 8
%stackaddr$prim51173 = alloca %struct.ScmObj*, align 8
%current_45args49593 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49592)
store volatile %struct.ScmObj* %current_45args49593, %struct.ScmObj** %stackaddr$prim51173, align 8
%stackaddr$prim51174 = alloca %struct.ScmObj*, align 8
%anf_45bind40305 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49593)
store volatile %struct.ScmObj* %anf_45bind40305, %struct.ScmObj** %stackaddr$prim51174, align 8
%stackaddr$prim51175 = alloca %struct.ScmObj*, align 8
%anf_45bind40306 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40127)
store volatile %struct.ScmObj* %anf_45bind40306, %struct.ScmObj** %stackaddr$prim51175, align 8
%args49595$_37foldl140126$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51176 = alloca %struct.ScmObj*, align 8
%args49595$_37foldl140126$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40306, %struct.ScmObj* %args49595$_37foldl140126$0)
store volatile %struct.ScmObj* %args49595$_37foldl140126$1, %struct.ScmObj** %stackaddr$prim51176, align 8
%stackaddr$prim51177 = alloca %struct.ScmObj*, align 8
%args49595$_37foldl140126$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40305, %struct.ScmObj* %args49595$_37foldl140126$1)
store volatile %struct.ScmObj* %args49595$_37foldl140126$2, %struct.ScmObj** %stackaddr$prim51177, align 8
%stackaddr$prim51178 = alloca %struct.ScmObj*, align 8
%args49595$_37foldl140126$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40129, %struct.ScmObj* %args49595$_37foldl140126$2)
store volatile %struct.ScmObj* %args49595$_37foldl140126$3, %struct.ScmObj** %stackaddr$prim51178, align 8
%stackaddr$prim51179 = alloca %struct.ScmObj*, align 8
%args49595$_37foldl140126$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40545, %struct.ScmObj* %args49595$_37foldl140126$3)
store volatile %struct.ScmObj* %args49595$_37foldl140126$4, %struct.ScmObj** %stackaddr$prim51179, align 8
%clofunc51180 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140126)
musttail call tailcc void %clofunc51180(%struct.ScmObj* %_37foldl140126, %struct.ScmObj* %args49595$_37foldl140126$4)
ret void
}

define tailcc void @proc_clo$ae41033(%struct.ScmObj* %env$ae41033,%struct.ScmObj* %current_45args49600) {
%stackaddr$prim51181 = alloca %struct.ScmObj*, align 8
%k40547 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49600)
store volatile %struct.ScmObj* %k40547, %struct.ScmObj** %stackaddr$prim51181, align 8
%stackaddr$prim51182 = alloca %struct.ScmObj*, align 8
%current_45args49601 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49600)
store volatile %struct.ScmObj* %current_45args49601, %struct.ScmObj** %stackaddr$prim51182, align 8
%stackaddr$prim51183 = alloca %struct.ScmObj*, align 8
%_37length40131 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49601)
store volatile %struct.ScmObj* %_37length40131, %struct.ScmObj** %stackaddr$prim51183, align 8
%ae41035 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure51184 = alloca %struct.ScmObj*, align 8
%fptrToInt51185 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41036 to i64
%ae41036 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt51185)
store volatile %struct.ScmObj* %ae41036, %struct.ScmObj** %stackaddr$makeclosure51184, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41036, %struct.ScmObj* %_37length40131, i64 0)
%args49612$k40547$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51186 = alloca %struct.ScmObj*, align 8
%args49612$k40547$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41036, %struct.ScmObj* %args49612$k40547$0)
store volatile %struct.ScmObj* %args49612$k40547$1, %struct.ScmObj** %stackaddr$prim51186, align 8
%stackaddr$prim51187 = alloca %struct.ScmObj*, align 8
%args49612$k40547$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41035, %struct.ScmObj* %args49612$k40547$1)
store volatile %struct.ScmObj* %args49612$k40547$2, %struct.ScmObj** %stackaddr$prim51187, align 8
%clofunc51188 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40547)
musttail call tailcc void %clofunc51188(%struct.ScmObj* %k40547, %struct.ScmObj* %args49612$k40547$2)
ret void
}

define tailcc void @proc_clo$ae41036(%struct.ScmObj* %env$ae41036,%struct.ScmObj* %current_45args49603) {
%stackaddr$env-ref51189 = alloca %struct.ScmObj*, align 8
%_37length40131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41036, i64 0)
store %struct.ScmObj* %_37length40131, %struct.ScmObj** %stackaddr$env-ref51189
%stackaddr$prim51190 = alloca %struct.ScmObj*, align 8
%k40548 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49603)
store volatile %struct.ScmObj* %k40548, %struct.ScmObj** %stackaddr$prim51190, align 8
%stackaddr$prim51191 = alloca %struct.ScmObj*, align 8
%current_45args49604 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49603)
store volatile %struct.ScmObj* %current_45args49604, %struct.ScmObj** %stackaddr$prim51191, align 8
%stackaddr$prim51192 = alloca %struct.ScmObj*, align 8
%lst40132 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49604)
store volatile %struct.ScmObj* %lst40132, %struct.ScmObj** %stackaddr$prim51192, align 8
%stackaddr$prim51193 = alloca %struct.ScmObj*, align 8
%anf_45bind40299 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40132)
store volatile %struct.ScmObj* %anf_45bind40299, %struct.ScmObj** %stackaddr$prim51193, align 8
%truthy$cmp51194 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40299)
%cmp$cmp51194 = icmp eq i64 %truthy$cmp51194, 1
br i1 %cmp$cmp51194, label %truebranch$cmp51194, label %falsebranch$cmp51194
truebranch$cmp51194:
%ae41040 = call %struct.ScmObj* @const_init_int(i64 0)
%ae41041 = call %struct.ScmObj* @const_init_int(i64 0)
%args49606$k40548$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51195 = alloca %struct.ScmObj*, align 8
%args49606$k40548$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41041, %struct.ScmObj* %args49606$k40548$0)
store volatile %struct.ScmObj* %args49606$k40548$1, %struct.ScmObj** %stackaddr$prim51195, align 8
%stackaddr$prim51196 = alloca %struct.ScmObj*, align 8
%args49606$k40548$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41040, %struct.ScmObj* %args49606$k40548$1)
store volatile %struct.ScmObj* %args49606$k40548$2, %struct.ScmObj** %stackaddr$prim51196, align 8
%clofunc51197 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40548)
musttail call tailcc void %clofunc51197(%struct.ScmObj* %k40548, %struct.ScmObj* %args49606$k40548$2)
ret void
falsebranch$cmp51194:
%stackaddr$prim51198 = alloca %struct.ScmObj*, align 8
%anf_45bind40300 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40132)
store volatile %struct.ScmObj* %anf_45bind40300, %struct.ScmObj** %stackaddr$prim51198, align 8
%stackaddr$makeclosure51199 = alloca %struct.ScmObj*, align 8
%fptrToInt51200 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41050 to i64
%ae41050 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt51200)
store volatile %struct.ScmObj* %ae41050, %struct.ScmObj** %stackaddr$makeclosure51199, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41050, %struct.ScmObj* %k40548, i64 0)
%args49611$_37length40131$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51201 = alloca %struct.ScmObj*, align 8
%args49611$_37length40131$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40300, %struct.ScmObj* %args49611$_37length40131$0)
store volatile %struct.ScmObj* %args49611$_37length40131$1, %struct.ScmObj** %stackaddr$prim51201, align 8
%stackaddr$prim51202 = alloca %struct.ScmObj*, align 8
%args49611$_37length40131$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41050, %struct.ScmObj* %args49611$_37length40131$1)
store volatile %struct.ScmObj* %args49611$_37length40131$2, %struct.ScmObj** %stackaddr$prim51202, align 8
%clofunc51203 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length40131)
musttail call tailcc void %clofunc51203(%struct.ScmObj* %_37length40131, %struct.ScmObj* %args49611$_37length40131$2)
ret void
}

define tailcc void @proc_clo$ae41050(%struct.ScmObj* %env$ae41050,%struct.ScmObj* %current_45args49607) {
%stackaddr$env-ref51204 = alloca %struct.ScmObj*, align 8
%k40548 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41050, i64 0)
store %struct.ScmObj* %k40548, %struct.ScmObj** %stackaddr$env-ref51204
%stackaddr$prim51205 = alloca %struct.ScmObj*, align 8
%_95k40549 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49607)
store volatile %struct.ScmObj* %_95k40549, %struct.ScmObj** %stackaddr$prim51205, align 8
%stackaddr$prim51206 = alloca %struct.ScmObj*, align 8
%current_45args49608 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49607)
store volatile %struct.ScmObj* %current_45args49608, %struct.ScmObj** %stackaddr$prim51206, align 8
%stackaddr$prim51207 = alloca %struct.ScmObj*, align 8
%anf_45bind40301 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49608)
store volatile %struct.ScmObj* %anf_45bind40301, %struct.ScmObj** %stackaddr$prim51207, align 8
%ae41052 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim51208 = alloca %struct.ScmObj*, align 8
%cpsprim40550 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae41052, %struct.ScmObj* %anf_45bind40301)
store volatile %struct.ScmObj* %cpsprim40550, %struct.ScmObj** %stackaddr$prim51208, align 8
%ae41055 = call %struct.ScmObj* @const_init_int(i64 0)
%args49610$k40548$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51209 = alloca %struct.ScmObj*, align 8
%args49610$k40548$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40550, %struct.ScmObj* %args49610$k40548$0)
store volatile %struct.ScmObj* %args49610$k40548$1, %struct.ScmObj** %stackaddr$prim51209, align 8
%stackaddr$prim51210 = alloca %struct.ScmObj*, align 8
%args49610$k40548$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41055, %struct.ScmObj* %args49610$k40548$1)
store volatile %struct.ScmObj* %args49610$k40548$2, %struct.ScmObj** %stackaddr$prim51210, align 8
%clofunc51211 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40548)
musttail call tailcc void %clofunc51211(%struct.ScmObj* %k40548, %struct.ScmObj* %args49610$k40548$2)
ret void
}

define tailcc void @proc_clo$ae40883(%struct.ScmObj* %env$ae40883,%struct.ScmObj* %current_45args49615) {
%stackaddr$prim51212 = alloca %struct.ScmObj*, align 8
%k40551 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49615)
store volatile %struct.ScmObj* %k40551, %struct.ScmObj** %stackaddr$prim51212, align 8
%stackaddr$prim51213 = alloca %struct.ScmObj*, align 8
%current_45args49616 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49615)
store volatile %struct.ScmObj* %current_45args49616, %struct.ScmObj** %stackaddr$prim51213, align 8
%stackaddr$prim51214 = alloca %struct.ScmObj*, align 8
%_37take40134 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49616)
store volatile %struct.ScmObj* %_37take40134, %struct.ScmObj** %stackaddr$prim51214, align 8
%ae40885 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure51215 = alloca %struct.ScmObj*, align 8
%fptrToInt51216 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40886 to i64
%ae40886 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt51216)
store volatile %struct.ScmObj* %ae40886, %struct.ScmObj** %stackaddr$makeclosure51215, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40886, %struct.ScmObj* %_37take40134, i64 0)
%args49629$k40551$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51217 = alloca %struct.ScmObj*, align 8
%args49629$k40551$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40886, %struct.ScmObj* %args49629$k40551$0)
store volatile %struct.ScmObj* %args49629$k40551$1, %struct.ScmObj** %stackaddr$prim51217, align 8
%stackaddr$prim51218 = alloca %struct.ScmObj*, align 8
%args49629$k40551$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40885, %struct.ScmObj* %args49629$k40551$1)
store volatile %struct.ScmObj* %args49629$k40551$2, %struct.ScmObj** %stackaddr$prim51218, align 8
%clofunc51219 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40551)
musttail call tailcc void %clofunc51219(%struct.ScmObj* %k40551, %struct.ScmObj* %args49629$k40551$2)
ret void
}

define tailcc void @proc_clo$ae40886(%struct.ScmObj* %env$ae40886,%struct.ScmObj* %current_45args49618) {
%stackaddr$env-ref51220 = alloca %struct.ScmObj*, align 8
%_37take40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40886, i64 0)
store %struct.ScmObj* %_37take40134, %struct.ScmObj** %stackaddr$env-ref51220
%stackaddr$prim51221 = alloca %struct.ScmObj*, align 8
%k40552 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49618)
store volatile %struct.ScmObj* %k40552, %struct.ScmObj** %stackaddr$prim51221, align 8
%stackaddr$prim51222 = alloca %struct.ScmObj*, align 8
%current_45args49619 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49618)
store volatile %struct.ScmObj* %current_45args49619, %struct.ScmObj** %stackaddr$prim51222, align 8
%stackaddr$prim51223 = alloca %struct.ScmObj*, align 8
%lst40136 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49619)
store volatile %struct.ScmObj* %lst40136, %struct.ScmObj** %stackaddr$prim51223, align 8
%stackaddr$prim51224 = alloca %struct.ScmObj*, align 8
%current_45args49620 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49619)
store volatile %struct.ScmObj* %current_45args49620, %struct.ScmObj** %stackaddr$prim51224, align 8
%stackaddr$prim51225 = alloca %struct.ScmObj*, align 8
%n40135 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49620)
store volatile %struct.ScmObj* %n40135, %struct.ScmObj** %stackaddr$prim51225, align 8
%ae40888 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim51226 = alloca %struct.ScmObj*, align 8
%anf_45bind40292 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n40135, %struct.ScmObj* %ae40888)
store volatile %struct.ScmObj* %anf_45bind40292, %struct.ScmObj** %stackaddr$prim51226, align 8
%truthy$cmp51227 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40292)
%cmp$cmp51227 = icmp eq i64 %truthy$cmp51227, 1
br i1 %cmp$cmp51227, label %truebranch$cmp51227, label %falsebranch$cmp51227
truebranch$cmp51227:
%ae40891 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40892 = call %struct.ScmObj* @const_init_null()
%args49622$k40552$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51228 = alloca %struct.ScmObj*, align 8
%args49622$k40552$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40892, %struct.ScmObj* %args49622$k40552$0)
store volatile %struct.ScmObj* %args49622$k40552$1, %struct.ScmObj** %stackaddr$prim51228, align 8
%stackaddr$prim51229 = alloca %struct.ScmObj*, align 8
%args49622$k40552$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40891, %struct.ScmObj* %args49622$k40552$1)
store volatile %struct.ScmObj* %args49622$k40552$2, %struct.ScmObj** %stackaddr$prim51229, align 8
%clofunc51230 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40552)
musttail call tailcc void %clofunc51230(%struct.ScmObj* %k40552, %struct.ScmObj* %args49622$k40552$2)
ret void
falsebranch$cmp51227:
%stackaddr$prim51231 = alloca %struct.ScmObj*, align 8
%anf_45bind40293 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40136)
store volatile %struct.ScmObj* %anf_45bind40293, %struct.ScmObj** %stackaddr$prim51231, align 8
%truthy$cmp51232 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40293)
%cmp$cmp51232 = icmp eq i64 %truthy$cmp51232, 1
br i1 %cmp$cmp51232, label %truebranch$cmp51232, label %falsebranch$cmp51232
truebranch$cmp51232:
%ae40902 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40903 = call %struct.ScmObj* @const_init_null()
%args49623$k40552$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51233 = alloca %struct.ScmObj*, align 8
%args49623$k40552$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40903, %struct.ScmObj* %args49623$k40552$0)
store volatile %struct.ScmObj* %args49623$k40552$1, %struct.ScmObj** %stackaddr$prim51233, align 8
%stackaddr$prim51234 = alloca %struct.ScmObj*, align 8
%args49623$k40552$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40902, %struct.ScmObj* %args49623$k40552$1)
store volatile %struct.ScmObj* %args49623$k40552$2, %struct.ScmObj** %stackaddr$prim51234, align 8
%clofunc51235 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40552)
musttail call tailcc void %clofunc51235(%struct.ScmObj* %k40552, %struct.ScmObj* %args49623$k40552$2)
ret void
falsebranch$cmp51232:
%stackaddr$prim51236 = alloca %struct.ScmObj*, align 8
%anf_45bind40294 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40136)
store volatile %struct.ScmObj* %anf_45bind40294, %struct.ScmObj** %stackaddr$prim51236, align 8
%stackaddr$prim51237 = alloca %struct.ScmObj*, align 8
%anf_45bind40295 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40136)
store volatile %struct.ScmObj* %anf_45bind40295, %struct.ScmObj** %stackaddr$prim51237, align 8
%ae40913 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim51238 = alloca %struct.ScmObj*, align 8
%anf_45bind40296 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n40135, %struct.ScmObj* %ae40913)
store volatile %struct.ScmObj* %anf_45bind40296, %struct.ScmObj** %stackaddr$prim51238, align 8
%stackaddr$makeclosure51239 = alloca %struct.ScmObj*, align 8
%fptrToInt51240 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40915 to i64
%ae40915 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt51240)
store volatile %struct.ScmObj* %ae40915, %struct.ScmObj** %stackaddr$makeclosure51239, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40915, %struct.ScmObj* %anf_45bind40294, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40915, %struct.ScmObj* %k40552, i64 1)
%args49628$_37take40134$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51241 = alloca %struct.ScmObj*, align 8
%args49628$_37take40134$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40296, %struct.ScmObj* %args49628$_37take40134$0)
store volatile %struct.ScmObj* %args49628$_37take40134$1, %struct.ScmObj** %stackaddr$prim51241, align 8
%stackaddr$prim51242 = alloca %struct.ScmObj*, align 8
%args49628$_37take40134$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40295, %struct.ScmObj* %args49628$_37take40134$1)
store volatile %struct.ScmObj* %args49628$_37take40134$2, %struct.ScmObj** %stackaddr$prim51242, align 8
%stackaddr$prim51243 = alloca %struct.ScmObj*, align 8
%args49628$_37take40134$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40915, %struct.ScmObj* %args49628$_37take40134$2)
store volatile %struct.ScmObj* %args49628$_37take40134$3, %struct.ScmObj** %stackaddr$prim51243, align 8
%clofunc51244 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take40134)
musttail call tailcc void %clofunc51244(%struct.ScmObj* %_37take40134, %struct.ScmObj* %args49628$_37take40134$3)
ret void
}

define tailcc void @proc_clo$ae40915(%struct.ScmObj* %env$ae40915,%struct.ScmObj* %current_45args49624) {
%stackaddr$env-ref51245 = alloca %struct.ScmObj*, align 8
%anf_45bind40294 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40915, i64 0)
store %struct.ScmObj* %anf_45bind40294, %struct.ScmObj** %stackaddr$env-ref51245
%stackaddr$env-ref51246 = alloca %struct.ScmObj*, align 8
%k40552 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40915, i64 1)
store %struct.ScmObj* %k40552, %struct.ScmObj** %stackaddr$env-ref51246
%stackaddr$prim51247 = alloca %struct.ScmObj*, align 8
%_95k40553 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49624)
store volatile %struct.ScmObj* %_95k40553, %struct.ScmObj** %stackaddr$prim51247, align 8
%stackaddr$prim51248 = alloca %struct.ScmObj*, align 8
%current_45args49625 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49624)
store volatile %struct.ScmObj* %current_45args49625, %struct.ScmObj** %stackaddr$prim51248, align 8
%stackaddr$prim51249 = alloca %struct.ScmObj*, align 8
%anf_45bind40297 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49625)
store volatile %struct.ScmObj* %anf_45bind40297, %struct.ScmObj** %stackaddr$prim51249, align 8
%stackaddr$prim51250 = alloca %struct.ScmObj*, align 8
%cpsprim40554 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40294, %struct.ScmObj* %anf_45bind40297)
store volatile %struct.ScmObj* %cpsprim40554, %struct.ScmObj** %stackaddr$prim51250, align 8
%ae40921 = call %struct.ScmObj* @const_init_int(i64 0)
%args49627$k40552$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51251 = alloca %struct.ScmObj*, align 8
%args49627$k40552$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40554, %struct.ScmObj* %args49627$k40552$0)
store volatile %struct.ScmObj* %args49627$k40552$1, %struct.ScmObj** %stackaddr$prim51251, align 8
%stackaddr$prim51252 = alloca %struct.ScmObj*, align 8
%args49627$k40552$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40921, %struct.ScmObj* %args49627$k40552$1)
store volatile %struct.ScmObj* %args49627$k40552$2, %struct.ScmObj** %stackaddr$prim51252, align 8
%clofunc51253 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40552)
musttail call tailcc void %clofunc51253(%struct.ScmObj* %k40552, %struct.ScmObj* %args49627$k40552$2)
ret void
}

define tailcc void @proc_clo$ae40786(%struct.ScmObj* %env$ae40786,%struct.ScmObj* %current_45args49632) {
%stackaddr$prim51254 = alloca %struct.ScmObj*, align 8
%k40555 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49632)
store volatile %struct.ScmObj* %k40555, %struct.ScmObj** %stackaddr$prim51254, align 8
%stackaddr$prim51255 = alloca %struct.ScmObj*, align 8
%current_45args49633 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49632)
store volatile %struct.ScmObj* %current_45args49633, %struct.ScmObj** %stackaddr$prim51255, align 8
%stackaddr$prim51256 = alloca %struct.ScmObj*, align 8
%_37map40138 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49633)
store volatile %struct.ScmObj* %_37map40138, %struct.ScmObj** %stackaddr$prim51256, align 8
%ae40788 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure51257 = alloca %struct.ScmObj*, align 8
%fptrToInt51258 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40789 to i64
%ae40789 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt51258)
store volatile %struct.ScmObj* %ae40789, %struct.ScmObj** %stackaddr$makeclosure51257, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40789, %struct.ScmObj* %_37map40138, i64 0)
%args49649$k40555$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51259 = alloca %struct.ScmObj*, align 8
%args49649$k40555$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40789, %struct.ScmObj* %args49649$k40555$0)
store volatile %struct.ScmObj* %args49649$k40555$1, %struct.ScmObj** %stackaddr$prim51259, align 8
%stackaddr$prim51260 = alloca %struct.ScmObj*, align 8
%args49649$k40555$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40788, %struct.ScmObj* %args49649$k40555$1)
store volatile %struct.ScmObj* %args49649$k40555$2, %struct.ScmObj** %stackaddr$prim51260, align 8
%clofunc51261 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40555)
musttail call tailcc void %clofunc51261(%struct.ScmObj* %k40555, %struct.ScmObj* %args49649$k40555$2)
ret void
}

define tailcc void @proc_clo$ae40789(%struct.ScmObj* %env$ae40789,%struct.ScmObj* %current_45args49635) {
%stackaddr$env-ref51262 = alloca %struct.ScmObj*, align 8
%_37map40138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40789, i64 0)
store %struct.ScmObj* %_37map40138, %struct.ScmObj** %stackaddr$env-ref51262
%stackaddr$prim51263 = alloca %struct.ScmObj*, align 8
%k40556 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49635)
store volatile %struct.ScmObj* %k40556, %struct.ScmObj** %stackaddr$prim51263, align 8
%stackaddr$prim51264 = alloca %struct.ScmObj*, align 8
%current_45args49636 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49635)
store volatile %struct.ScmObj* %current_45args49636, %struct.ScmObj** %stackaddr$prim51264, align 8
%stackaddr$prim51265 = alloca %struct.ScmObj*, align 8
%f40140 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49636)
store volatile %struct.ScmObj* %f40140, %struct.ScmObj** %stackaddr$prim51265, align 8
%stackaddr$prim51266 = alloca %struct.ScmObj*, align 8
%current_45args49637 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49636)
store volatile %struct.ScmObj* %current_45args49637, %struct.ScmObj** %stackaddr$prim51266, align 8
%stackaddr$prim51267 = alloca %struct.ScmObj*, align 8
%lst40139 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49637)
store volatile %struct.ScmObj* %lst40139, %struct.ScmObj** %stackaddr$prim51267, align 8
%stackaddr$prim51268 = alloca %struct.ScmObj*, align 8
%anf_45bind40286 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40139)
store volatile %struct.ScmObj* %anf_45bind40286, %struct.ScmObj** %stackaddr$prim51268, align 8
%truthy$cmp51269 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40286)
%cmp$cmp51269 = icmp eq i64 %truthy$cmp51269, 1
br i1 %cmp$cmp51269, label %truebranch$cmp51269, label %falsebranch$cmp51269
truebranch$cmp51269:
%ae40793 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40794 = call %struct.ScmObj* @const_init_null()
%args49639$k40556$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51270 = alloca %struct.ScmObj*, align 8
%args49639$k40556$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40794, %struct.ScmObj* %args49639$k40556$0)
store volatile %struct.ScmObj* %args49639$k40556$1, %struct.ScmObj** %stackaddr$prim51270, align 8
%stackaddr$prim51271 = alloca %struct.ScmObj*, align 8
%args49639$k40556$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40793, %struct.ScmObj* %args49639$k40556$1)
store volatile %struct.ScmObj* %args49639$k40556$2, %struct.ScmObj** %stackaddr$prim51271, align 8
%clofunc51272 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40556)
musttail call tailcc void %clofunc51272(%struct.ScmObj* %k40556, %struct.ScmObj* %args49639$k40556$2)
ret void
falsebranch$cmp51269:
%stackaddr$prim51273 = alloca %struct.ScmObj*, align 8
%anf_45bind40287 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40139)
store volatile %struct.ScmObj* %anf_45bind40287, %struct.ScmObj** %stackaddr$prim51273, align 8
%stackaddr$makeclosure51274 = alloca %struct.ScmObj*, align 8
%fptrToInt51275 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40803 to i64
%ae40803 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt51275)
store volatile %struct.ScmObj* %ae40803, %struct.ScmObj** %stackaddr$makeclosure51274, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40803, %struct.ScmObj* %k40556, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40803, %struct.ScmObj* %f40140, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40803, %struct.ScmObj* %lst40139, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae40803, %struct.ScmObj* %_37map40138, i64 3)
%args49648$f40140$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51276 = alloca %struct.ScmObj*, align 8
%args49648$f40140$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40287, %struct.ScmObj* %args49648$f40140$0)
store volatile %struct.ScmObj* %args49648$f40140$1, %struct.ScmObj** %stackaddr$prim51276, align 8
%stackaddr$prim51277 = alloca %struct.ScmObj*, align 8
%args49648$f40140$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40803, %struct.ScmObj* %args49648$f40140$1)
store volatile %struct.ScmObj* %args49648$f40140$2, %struct.ScmObj** %stackaddr$prim51277, align 8
%clofunc51278 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40140)
musttail call tailcc void %clofunc51278(%struct.ScmObj* %f40140, %struct.ScmObj* %args49648$f40140$2)
ret void
}

define tailcc void @proc_clo$ae40803(%struct.ScmObj* %env$ae40803,%struct.ScmObj* %current_45args49640) {
%stackaddr$env-ref51279 = alloca %struct.ScmObj*, align 8
%k40556 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40803, i64 0)
store %struct.ScmObj* %k40556, %struct.ScmObj** %stackaddr$env-ref51279
%stackaddr$env-ref51280 = alloca %struct.ScmObj*, align 8
%f40140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40803, i64 1)
store %struct.ScmObj* %f40140, %struct.ScmObj** %stackaddr$env-ref51280
%stackaddr$env-ref51281 = alloca %struct.ScmObj*, align 8
%lst40139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40803, i64 2)
store %struct.ScmObj* %lst40139, %struct.ScmObj** %stackaddr$env-ref51281
%stackaddr$env-ref51282 = alloca %struct.ScmObj*, align 8
%_37map40138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40803, i64 3)
store %struct.ScmObj* %_37map40138, %struct.ScmObj** %stackaddr$env-ref51282
%stackaddr$prim51283 = alloca %struct.ScmObj*, align 8
%_95k40557 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49640)
store volatile %struct.ScmObj* %_95k40557, %struct.ScmObj** %stackaddr$prim51283, align 8
%stackaddr$prim51284 = alloca %struct.ScmObj*, align 8
%current_45args49641 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49640)
store volatile %struct.ScmObj* %current_45args49641, %struct.ScmObj** %stackaddr$prim51284, align 8
%stackaddr$prim51285 = alloca %struct.ScmObj*, align 8
%anf_45bind40288 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49641)
store volatile %struct.ScmObj* %anf_45bind40288, %struct.ScmObj** %stackaddr$prim51285, align 8
%stackaddr$prim51286 = alloca %struct.ScmObj*, align 8
%anf_45bind40289 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40139)
store volatile %struct.ScmObj* %anf_45bind40289, %struct.ScmObj** %stackaddr$prim51286, align 8
%stackaddr$makeclosure51287 = alloca %struct.ScmObj*, align 8
%fptrToInt51288 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40807 to i64
%ae40807 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt51288)
store volatile %struct.ScmObj* %ae40807, %struct.ScmObj** %stackaddr$makeclosure51287, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40807, %struct.ScmObj* %anf_45bind40288, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40807, %struct.ScmObj* %k40556, i64 1)
%args49647$_37map40138$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51289 = alloca %struct.ScmObj*, align 8
%args49647$_37map40138$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40289, %struct.ScmObj* %args49647$_37map40138$0)
store volatile %struct.ScmObj* %args49647$_37map40138$1, %struct.ScmObj** %stackaddr$prim51289, align 8
%stackaddr$prim51290 = alloca %struct.ScmObj*, align 8
%args49647$_37map40138$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40140, %struct.ScmObj* %args49647$_37map40138$1)
store volatile %struct.ScmObj* %args49647$_37map40138$2, %struct.ScmObj** %stackaddr$prim51290, align 8
%stackaddr$prim51291 = alloca %struct.ScmObj*, align 8
%args49647$_37map40138$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40807, %struct.ScmObj* %args49647$_37map40138$2)
store volatile %struct.ScmObj* %args49647$_37map40138$3, %struct.ScmObj** %stackaddr$prim51291, align 8
%clofunc51292 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map40138)
musttail call tailcc void %clofunc51292(%struct.ScmObj* %_37map40138, %struct.ScmObj* %args49647$_37map40138$3)
ret void
}

define tailcc void @proc_clo$ae40807(%struct.ScmObj* %env$ae40807,%struct.ScmObj* %current_45args49643) {
%stackaddr$env-ref51293 = alloca %struct.ScmObj*, align 8
%anf_45bind40288 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40807, i64 0)
store %struct.ScmObj* %anf_45bind40288, %struct.ScmObj** %stackaddr$env-ref51293
%stackaddr$env-ref51294 = alloca %struct.ScmObj*, align 8
%k40556 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40807, i64 1)
store %struct.ScmObj* %k40556, %struct.ScmObj** %stackaddr$env-ref51294
%stackaddr$prim51295 = alloca %struct.ScmObj*, align 8
%_95k40558 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49643)
store volatile %struct.ScmObj* %_95k40558, %struct.ScmObj** %stackaddr$prim51295, align 8
%stackaddr$prim51296 = alloca %struct.ScmObj*, align 8
%current_45args49644 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49643)
store volatile %struct.ScmObj* %current_45args49644, %struct.ScmObj** %stackaddr$prim51296, align 8
%stackaddr$prim51297 = alloca %struct.ScmObj*, align 8
%anf_45bind40290 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49644)
store volatile %struct.ScmObj* %anf_45bind40290, %struct.ScmObj** %stackaddr$prim51297, align 8
%stackaddr$prim51298 = alloca %struct.ScmObj*, align 8
%cpsprim40559 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40288, %struct.ScmObj* %anf_45bind40290)
store volatile %struct.ScmObj* %cpsprim40559, %struct.ScmObj** %stackaddr$prim51298, align 8
%ae40813 = call %struct.ScmObj* @const_init_int(i64 0)
%args49646$k40556$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51299 = alloca %struct.ScmObj*, align 8
%args49646$k40556$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40559, %struct.ScmObj* %args49646$k40556$0)
store volatile %struct.ScmObj* %args49646$k40556$1, %struct.ScmObj** %stackaddr$prim51299, align 8
%stackaddr$prim51300 = alloca %struct.ScmObj*, align 8
%args49646$k40556$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40813, %struct.ScmObj* %args49646$k40556$1)
store volatile %struct.ScmObj* %args49646$k40556$2, %struct.ScmObj** %stackaddr$prim51300, align 8
%clofunc51301 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40556)
musttail call tailcc void %clofunc51301(%struct.ScmObj* %k40556, %struct.ScmObj* %args49646$k40556$2)
ret void
}

define tailcc void @proc_clo$ae40706(%struct.ScmObj* %env$ae40706,%struct.ScmObj* %current_45args49652) {
%stackaddr$prim51302 = alloca %struct.ScmObj*, align 8
%k40560 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49652)
store volatile %struct.ScmObj* %k40560, %struct.ScmObj** %stackaddr$prim51302, align 8
%stackaddr$prim51303 = alloca %struct.ScmObj*, align 8
%current_45args49653 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49652)
store volatile %struct.ScmObj* %current_45args49653, %struct.ScmObj** %stackaddr$prim51303, align 8
%stackaddr$prim51304 = alloca %struct.ScmObj*, align 8
%_37foldr140142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49653)
store volatile %struct.ScmObj* %_37foldr140142, %struct.ScmObj** %stackaddr$prim51304, align 8
%ae40708 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure51305 = alloca %struct.ScmObj*, align 8
%fptrToInt51306 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40709 to i64
%ae40709 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt51306)
store volatile %struct.ScmObj* %ae40709, %struct.ScmObj** %stackaddr$makeclosure51305, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40709, %struct.ScmObj* %_37foldr140142, i64 0)
%args49666$k40560$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51307 = alloca %struct.ScmObj*, align 8
%args49666$k40560$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40709, %struct.ScmObj* %args49666$k40560$0)
store volatile %struct.ScmObj* %args49666$k40560$1, %struct.ScmObj** %stackaddr$prim51307, align 8
%stackaddr$prim51308 = alloca %struct.ScmObj*, align 8
%args49666$k40560$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40708, %struct.ScmObj* %args49666$k40560$1)
store volatile %struct.ScmObj* %args49666$k40560$2, %struct.ScmObj** %stackaddr$prim51308, align 8
%clofunc51309 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40560)
musttail call tailcc void %clofunc51309(%struct.ScmObj* %k40560, %struct.ScmObj* %args49666$k40560$2)
ret void
}

define tailcc void @proc_clo$ae40709(%struct.ScmObj* %env$ae40709,%struct.ScmObj* %current_45args49655) {
%stackaddr$env-ref51310 = alloca %struct.ScmObj*, align 8
%_37foldr140142 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40709, i64 0)
store %struct.ScmObj* %_37foldr140142, %struct.ScmObj** %stackaddr$env-ref51310
%stackaddr$prim51311 = alloca %struct.ScmObj*, align 8
%k40561 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49655)
store volatile %struct.ScmObj* %k40561, %struct.ScmObj** %stackaddr$prim51311, align 8
%stackaddr$prim51312 = alloca %struct.ScmObj*, align 8
%current_45args49656 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49655)
store volatile %struct.ScmObj* %current_45args49656, %struct.ScmObj** %stackaddr$prim51312, align 8
%stackaddr$prim51313 = alloca %struct.ScmObj*, align 8
%f40145 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49656)
store volatile %struct.ScmObj* %f40145, %struct.ScmObj** %stackaddr$prim51313, align 8
%stackaddr$prim51314 = alloca %struct.ScmObj*, align 8
%current_45args49657 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49656)
store volatile %struct.ScmObj* %current_45args49657, %struct.ScmObj** %stackaddr$prim51314, align 8
%stackaddr$prim51315 = alloca %struct.ScmObj*, align 8
%acc40144 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49657)
store volatile %struct.ScmObj* %acc40144, %struct.ScmObj** %stackaddr$prim51315, align 8
%stackaddr$prim51316 = alloca %struct.ScmObj*, align 8
%current_45args49658 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49657)
store volatile %struct.ScmObj* %current_45args49658, %struct.ScmObj** %stackaddr$prim51316, align 8
%stackaddr$prim51317 = alloca %struct.ScmObj*, align 8
%lst40143 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49658)
store volatile %struct.ScmObj* %lst40143, %struct.ScmObj** %stackaddr$prim51317, align 8
%stackaddr$prim51318 = alloca %struct.ScmObj*, align 8
%anf_45bind40281 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40143)
store volatile %struct.ScmObj* %anf_45bind40281, %struct.ScmObj** %stackaddr$prim51318, align 8
%truthy$cmp51319 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40281)
%cmp$cmp51319 = icmp eq i64 %truthy$cmp51319, 1
br i1 %cmp$cmp51319, label %truebranch$cmp51319, label %falsebranch$cmp51319
truebranch$cmp51319:
%ae40713 = call %struct.ScmObj* @const_init_int(i64 0)
%args49660$k40561$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51320 = alloca %struct.ScmObj*, align 8
%args49660$k40561$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40144, %struct.ScmObj* %args49660$k40561$0)
store volatile %struct.ScmObj* %args49660$k40561$1, %struct.ScmObj** %stackaddr$prim51320, align 8
%stackaddr$prim51321 = alloca %struct.ScmObj*, align 8
%args49660$k40561$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40713, %struct.ScmObj* %args49660$k40561$1)
store volatile %struct.ScmObj* %args49660$k40561$2, %struct.ScmObj** %stackaddr$prim51321, align 8
%clofunc51322 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40561)
musttail call tailcc void %clofunc51322(%struct.ScmObj* %k40561, %struct.ScmObj* %args49660$k40561$2)
ret void
falsebranch$cmp51319:
%stackaddr$prim51323 = alloca %struct.ScmObj*, align 8
%anf_45bind40282 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40143)
store volatile %struct.ScmObj* %anf_45bind40282, %struct.ScmObj** %stackaddr$prim51323, align 8
%stackaddr$prim51324 = alloca %struct.ScmObj*, align 8
%anf_45bind40283 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40143)
store volatile %struct.ScmObj* %anf_45bind40283, %struct.ScmObj** %stackaddr$prim51324, align 8
%stackaddr$makeclosure51325 = alloca %struct.ScmObj*, align 8
%fptrToInt51326 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40721 to i64
%ae40721 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt51326)
store volatile %struct.ScmObj* %ae40721, %struct.ScmObj** %stackaddr$makeclosure51325, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40721, %struct.ScmObj* %k40561, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40721, %struct.ScmObj* %f40145, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40721, %struct.ScmObj* %anf_45bind40282, i64 2)
%args49665$_37foldr140142$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51327 = alloca %struct.ScmObj*, align 8
%args49665$_37foldr140142$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40283, %struct.ScmObj* %args49665$_37foldr140142$0)
store volatile %struct.ScmObj* %args49665$_37foldr140142$1, %struct.ScmObj** %stackaddr$prim51327, align 8
%stackaddr$prim51328 = alloca %struct.ScmObj*, align 8
%args49665$_37foldr140142$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40144, %struct.ScmObj* %args49665$_37foldr140142$1)
store volatile %struct.ScmObj* %args49665$_37foldr140142$2, %struct.ScmObj** %stackaddr$prim51328, align 8
%stackaddr$prim51329 = alloca %struct.ScmObj*, align 8
%args49665$_37foldr140142$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40145, %struct.ScmObj* %args49665$_37foldr140142$2)
store volatile %struct.ScmObj* %args49665$_37foldr140142$3, %struct.ScmObj** %stackaddr$prim51329, align 8
%stackaddr$prim51330 = alloca %struct.ScmObj*, align 8
%args49665$_37foldr140142$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40721, %struct.ScmObj* %args49665$_37foldr140142$3)
store volatile %struct.ScmObj* %args49665$_37foldr140142$4, %struct.ScmObj** %stackaddr$prim51330, align 8
%clofunc51331 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140142)
musttail call tailcc void %clofunc51331(%struct.ScmObj* %_37foldr140142, %struct.ScmObj* %args49665$_37foldr140142$4)
ret void
}

define tailcc void @proc_clo$ae40721(%struct.ScmObj* %env$ae40721,%struct.ScmObj* %current_45args49661) {
%stackaddr$env-ref51332 = alloca %struct.ScmObj*, align 8
%k40561 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40721, i64 0)
store %struct.ScmObj* %k40561, %struct.ScmObj** %stackaddr$env-ref51332
%stackaddr$env-ref51333 = alloca %struct.ScmObj*, align 8
%f40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40721, i64 1)
store %struct.ScmObj* %f40145, %struct.ScmObj** %stackaddr$env-ref51333
%stackaddr$env-ref51334 = alloca %struct.ScmObj*, align 8
%anf_45bind40282 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40721, i64 2)
store %struct.ScmObj* %anf_45bind40282, %struct.ScmObj** %stackaddr$env-ref51334
%stackaddr$prim51335 = alloca %struct.ScmObj*, align 8
%_95k40562 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49661)
store volatile %struct.ScmObj* %_95k40562, %struct.ScmObj** %stackaddr$prim51335, align 8
%stackaddr$prim51336 = alloca %struct.ScmObj*, align 8
%current_45args49662 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49661)
store volatile %struct.ScmObj* %current_45args49662, %struct.ScmObj** %stackaddr$prim51336, align 8
%stackaddr$prim51337 = alloca %struct.ScmObj*, align 8
%anf_45bind40284 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49662)
store volatile %struct.ScmObj* %anf_45bind40284, %struct.ScmObj** %stackaddr$prim51337, align 8
%args49664$f40145$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51338 = alloca %struct.ScmObj*, align 8
%args49664$f40145$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40284, %struct.ScmObj* %args49664$f40145$0)
store volatile %struct.ScmObj* %args49664$f40145$1, %struct.ScmObj** %stackaddr$prim51338, align 8
%stackaddr$prim51339 = alloca %struct.ScmObj*, align 8
%args49664$f40145$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40282, %struct.ScmObj* %args49664$f40145$1)
store volatile %struct.ScmObj* %args49664$f40145$2, %struct.ScmObj** %stackaddr$prim51339, align 8
%stackaddr$prim51340 = alloca %struct.ScmObj*, align 8
%args49664$f40145$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40561, %struct.ScmObj* %args49664$f40145$2)
store volatile %struct.ScmObj* %args49664$f40145$3, %struct.ScmObj** %stackaddr$prim51340, align 8
%clofunc51341 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40145)
musttail call tailcc void %clofunc51341(%struct.ScmObj* %f40145, %struct.ScmObj* %args49664$f40145$3)
ret void
}

define tailcc void @proc_clo$ae40589(%struct.ScmObj* %env$ae40589,%struct.ScmObj* %current_45args49669) {
%stackaddr$prim51342 = alloca %struct.ScmObj*, align 8
%k40563 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49669)
store volatile %struct.ScmObj* %k40563, %struct.ScmObj** %stackaddr$prim51342, align 8
%stackaddr$prim51343 = alloca %struct.ScmObj*, align 8
%current_45args49670 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49669)
store volatile %struct.ScmObj* %current_45args49670, %struct.ScmObj** %stackaddr$prim51343, align 8
%stackaddr$prim51344 = alloca %struct.ScmObj*, align 8
%y40122 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49670)
store volatile %struct.ScmObj* %y40122, %struct.ScmObj** %stackaddr$prim51344, align 8
%ae40591 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure51345 = alloca %struct.ScmObj*, align 8
%fptrToInt51346 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40592 to i64
%ae40592 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt51346)
store volatile %struct.ScmObj* %ae40592, %struct.ScmObj** %stackaddr$makeclosure51345, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40592, %struct.ScmObj* %y40122, i64 0)
%args49688$k40563$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51347 = alloca %struct.ScmObj*, align 8
%args49688$k40563$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40592, %struct.ScmObj* %args49688$k40563$0)
store volatile %struct.ScmObj* %args49688$k40563$1, %struct.ScmObj** %stackaddr$prim51347, align 8
%stackaddr$prim51348 = alloca %struct.ScmObj*, align 8
%args49688$k40563$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40591, %struct.ScmObj* %args49688$k40563$1)
store volatile %struct.ScmObj* %args49688$k40563$2, %struct.ScmObj** %stackaddr$prim51348, align 8
%clofunc51349 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40563)
musttail call tailcc void %clofunc51349(%struct.ScmObj* %k40563, %struct.ScmObj* %args49688$k40563$2)
ret void
}

define tailcc void @proc_clo$ae40592(%struct.ScmObj* %env$ae40592,%struct.ScmObj* %current_45args49672) {
%stackaddr$env-ref51350 = alloca %struct.ScmObj*, align 8
%y40122 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40592, i64 0)
store %struct.ScmObj* %y40122, %struct.ScmObj** %stackaddr$env-ref51350
%stackaddr$prim51351 = alloca %struct.ScmObj*, align 8
%k40564 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49672)
store volatile %struct.ScmObj* %k40564, %struct.ScmObj** %stackaddr$prim51351, align 8
%stackaddr$prim51352 = alloca %struct.ScmObj*, align 8
%current_45args49673 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49672)
store volatile %struct.ScmObj* %current_45args49673, %struct.ScmObj** %stackaddr$prim51352, align 8
%stackaddr$prim51353 = alloca %struct.ScmObj*, align 8
%f40123 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49673)
store volatile %struct.ScmObj* %f40123, %struct.ScmObj** %stackaddr$prim51353, align 8
%stackaddr$makeclosure51354 = alloca %struct.ScmObj*, align 8
%fptrToInt51355 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40593 to i64
%ae40593 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt51355)
store volatile %struct.ScmObj* %ae40593, %struct.ScmObj** %stackaddr$makeclosure51354, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40593, %struct.ScmObj* %k40564, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40593, %struct.ScmObj* %f40123, i64 1)
%ae40594 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure51356 = alloca %struct.ScmObj*, align 8
%fptrToInt51357 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40595 to i64
%ae40595 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt51357)
store volatile %struct.ScmObj* %ae40595, %struct.ScmObj** %stackaddr$makeclosure51356, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40595, %struct.ScmObj* %f40123, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40595, %struct.ScmObj* %y40122, i64 1)
%args49687$ae40593$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51358 = alloca %struct.ScmObj*, align 8
%args49687$ae40593$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40595, %struct.ScmObj* %args49687$ae40593$0)
store volatile %struct.ScmObj* %args49687$ae40593$1, %struct.ScmObj** %stackaddr$prim51358, align 8
%stackaddr$prim51359 = alloca %struct.ScmObj*, align 8
%args49687$ae40593$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40594, %struct.ScmObj* %args49687$ae40593$1)
store volatile %struct.ScmObj* %args49687$ae40593$2, %struct.ScmObj** %stackaddr$prim51359, align 8
%clofunc51360 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40593)
musttail call tailcc void %clofunc51360(%struct.ScmObj* %ae40593, %struct.ScmObj* %args49687$ae40593$2)
ret void
}

define tailcc void @proc_clo$ae40593(%struct.ScmObj* %env$ae40593,%struct.ScmObj* %current_45args49675) {
%stackaddr$env-ref51361 = alloca %struct.ScmObj*, align 8
%k40564 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40593, i64 0)
store %struct.ScmObj* %k40564, %struct.ScmObj** %stackaddr$env-ref51361
%stackaddr$env-ref51362 = alloca %struct.ScmObj*, align 8
%f40123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40593, i64 1)
store %struct.ScmObj* %f40123, %struct.ScmObj** %stackaddr$env-ref51362
%stackaddr$prim51363 = alloca %struct.ScmObj*, align 8
%_95k40565 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49675)
store volatile %struct.ScmObj* %_95k40565, %struct.ScmObj** %stackaddr$prim51363, align 8
%stackaddr$prim51364 = alloca %struct.ScmObj*, align 8
%current_45args49676 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49675)
store volatile %struct.ScmObj* %current_45args49676, %struct.ScmObj** %stackaddr$prim51364, align 8
%stackaddr$prim51365 = alloca %struct.ScmObj*, align 8
%anf_45bind40279 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49676)
store volatile %struct.ScmObj* %anf_45bind40279, %struct.ScmObj** %stackaddr$prim51365, align 8
%args49678$f40123$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51366 = alloca %struct.ScmObj*, align 8
%args49678$f40123$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40279, %struct.ScmObj* %args49678$f40123$0)
store volatile %struct.ScmObj* %args49678$f40123$1, %struct.ScmObj** %stackaddr$prim51366, align 8
%stackaddr$prim51367 = alloca %struct.ScmObj*, align 8
%args49678$f40123$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40564, %struct.ScmObj* %args49678$f40123$1)
store volatile %struct.ScmObj* %args49678$f40123$2, %struct.ScmObj** %stackaddr$prim51367, align 8
%clofunc51368 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40123)
musttail call tailcc void %clofunc51368(%struct.ScmObj* %f40123, %struct.ScmObj* %args49678$f40123$2)
ret void
}

define tailcc void @proc_clo$ae40595(%struct.ScmObj* %env$ae40595,%struct.ScmObj* %args4012440566) {
%stackaddr$env-ref51369 = alloca %struct.ScmObj*, align 8
%f40123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40595, i64 0)
store %struct.ScmObj* %f40123, %struct.ScmObj** %stackaddr$env-ref51369
%stackaddr$env-ref51370 = alloca %struct.ScmObj*, align 8
%y40122 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40595, i64 1)
store %struct.ScmObj* %y40122, %struct.ScmObj** %stackaddr$env-ref51370
%stackaddr$prim51371 = alloca %struct.ScmObj*, align 8
%k40567 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4012440566)
store volatile %struct.ScmObj* %k40567, %struct.ScmObj** %stackaddr$prim51371, align 8
%stackaddr$prim51372 = alloca %struct.ScmObj*, align 8
%args40124 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4012440566)
store volatile %struct.ScmObj* %args40124, %struct.ScmObj** %stackaddr$prim51372, align 8
%stackaddr$makeclosure51373 = alloca %struct.ScmObj*, align 8
%fptrToInt51374 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40599 to i64
%ae40599 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt51374)
store volatile %struct.ScmObj* %ae40599, %struct.ScmObj** %stackaddr$makeclosure51373, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40599, %struct.ScmObj* %k40567, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40599, %struct.ScmObj* %args40124, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40599, %struct.ScmObj* %f40123, i64 2)
%args49686$y40122$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51375 = alloca %struct.ScmObj*, align 8
%args49686$y40122$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y40122, %struct.ScmObj* %args49686$y40122$0)
store volatile %struct.ScmObj* %args49686$y40122$1, %struct.ScmObj** %stackaddr$prim51375, align 8
%stackaddr$prim51376 = alloca %struct.ScmObj*, align 8
%args49686$y40122$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40599, %struct.ScmObj* %args49686$y40122$1)
store volatile %struct.ScmObj* %args49686$y40122$2, %struct.ScmObj** %stackaddr$prim51376, align 8
%clofunc51377 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y40122)
musttail call tailcc void %clofunc51377(%struct.ScmObj* %y40122, %struct.ScmObj* %args49686$y40122$2)
ret void
}

define tailcc void @proc_clo$ae40599(%struct.ScmObj* %env$ae40599,%struct.ScmObj* %current_45args49679) {
%stackaddr$env-ref51378 = alloca %struct.ScmObj*, align 8
%k40567 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40599, i64 0)
store %struct.ScmObj* %k40567, %struct.ScmObj** %stackaddr$env-ref51378
%stackaddr$env-ref51379 = alloca %struct.ScmObj*, align 8
%args40124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40599, i64 1)
store %struct.ScmObj* %args40124, %struct.ScmObj** %stackaddr$env-ref51379
%stackaddr$env-ref51380 = alloca %struct.ScmObj*, align 8
%f40123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40599, i64 2)
store %struct.ScmObj* %f40123, %struct.ScmObj** %stackaddr$env-ref51380
%stackaddr$prim51381 = alloca %struct.ScmObj*, align 8
%_95k40568 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49679)
store volatile %struct.ScmObj* %_95k40568, %struct.ScmObj** %stackaddr$prim51381, align 8
%stackaddr$prim51382 = alloca %struct.ScmObj*, align 8
%current_45args49680 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49679)
store volatile %struct.ScmObj* %current_45args49680, %struct.ScmObj** %stackaddr$prim51382, align 8
%stackaddr$prim51383 = alloca %struct.ScmObj*, align 8
%anf_45bind40277 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49680)
store volatile %struct.ScmObj* %anf_45bind40277, %struct.ScmObj** %stackaddr$prim51383, align 8
%stackaddr$makeclosure51384 = alloca %struct.ScmObj*, align 8
%fptrToInt51385 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40602 to i64
%ae40602 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt51385)
store volatile %struct.ScmObj* %ae40602, %struct.ScmObj** %stackaddr$makeclosure51384, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40602, %struct.ScmObj* %k40567, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40602, %struct.ScmObj* %args40124, i64 1)
%args49685$anf_45bind40277$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51386 = alloca %struct.ScmObj*, align 8
%args49685$anf_45bind40277$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40123, %struct.ScmObj* %args49685$anf_45bind40277$0)
store volatile %struct.ScmObj* %args49685$anf_45bind40277$1, %struct.ScmObj** %stackaddr$prim51386, align 8
%stackaddr$prim51387 = alloca %struct.ScmObj*, align 8
%args49685$anf_45bind40277$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40602, %struct.ScmObj* %args49685$anf_45bind40277$1)
store volatile %struct.ScmObj* %args49685$anf_45bind40277$2, %struct.ScmObj** %stackaddr$prim51387, align 8
%clofunc51388 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40277)
musttail call tailcc void %clofunc51388(%struct.ScmObj* %anf_45bind40277, %struct.ScmObj* %args49685$anf_45bind40277$2)
ret void
}

define tailcc void @proc_clo$ae40602(%struct.ScmObj* %env$ae40602,%struct.ScmObj* %current_45args49682) {
%stackaddr$env-ref51389 = alloca %struct.ScmObj*, align 8
%k40567 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40602, i64 0)
store %struct.ScmObj* %k40567, %struct.ScmObj** %stackaddr$env-ref51389
%stackaddr$env-ref51390 = alloca %struct.ScmObj*, align 8
%args40124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40602, i64 1)
store %struct.ScmObj* %args40124, %struct.ScmObj** %stackaddr$env-ref51390
%stackaddr$prim51391 = alloca %struct.ScmObj*, align 8
%_95k40569 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49682)
store volatile %struct.ScmObj* %_95k40569, %struct.ScmObj** %stackaddr$prim51391, align 8
%stackaddr$prim51392 = alloca %struct.ScmObj*, align 8
%current_45args49683 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49682)
store volatile %struct.ScmObj* %current_45args49683, %struct.ScmObj** %stackaddr$prim51392, align 8
%stackaddr$prim51393 = alloca %struct.ScmObj*, align 8
%anf_45bind40278 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49683)
store volatile %struct.ScmObj* %anf_45bind40278, %struct.ScmObj** %stackaddr$prim51393, align 8
%stackaddr$prim51394 = alloca %struct.ScmObj*, align 8
%cpsargs40570 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40567, %struct.ScmObj* %args40124)
store volatile %struct.ScmObj* %cpsargs40570, %struct.ScmObj** %stackaddr$prim51394, align 8
%clofunc51395 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40278)
musttail call tailcc void %clofunc51395(%struct.ScmObj* %anf_45bind40278, %struct.ScmObj* %cpsargs40570)
ret void
}

define tailcc void @proc_clo$ae40574(%struct.ScmObj* %env$ae40574,%struct.ScmObj* %current_45args49690) {
%stackaddr$prim51396 = alloca %struct.ScmObj*, align 8
%k40571 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49690)
store volatile %struct.ScmObj* %k40571, %struct.ScmObj** %stackaddr$prim51396, align 8
%stackaddr$prim51397 = alloca %struct.ScmObj*, align 8
%current_45args49691 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49690)
store volatile %struct.ScmObj* %current_45args49691, %struct.ScmObj** %stackaddr$prim51397, align 8
%stackaddr$prim51398 = alloca %struct.ScmObj*, align 8
%yu40121 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49691)
store volatile %struct.ScmObj* %yu40121, %struct.ScmObj** %stackaddr$prim51398, align 8
%args49693$yu40121$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51399 = alloca %struct.ScmObj*, align 8
%args49693$yu40121$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu40121, %struct.ScmObj* %args49693$yu40121$0)
store volatile %struct.ScmObj* %args49693$yu40121$1, %struct.ScmObj** %stackaddr$prim51399, align 8
%stackaddr$prim51400 = alloca %struct.ScmObj*, align 8
%args49693$yu40121$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40571, %struct.ScmObj* %args49693$yu40121$1)
store volatile %struct.ScmObj* %args49693$yu40121$2, %struct.ScmObj** %stackaddr$prim51400, align 8
%clofunc51401 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu40121)
musttail call tailcc void %clofunc51401(%struct.ScmObj* %yu40121, %struct.ScmObj* %args49693$yu40121$2)
ret void
}