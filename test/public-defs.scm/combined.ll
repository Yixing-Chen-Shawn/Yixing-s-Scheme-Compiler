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

@global$sym$ae5094356865 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae5094656867 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae5094956869 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae5095256871 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae5095556873 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae5095856875 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae5096156877 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae5096456879 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae5096756881 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae5097056883 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae5097356885 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae5097656887 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae5097956889 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae5098256891 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae5098556893 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae5098856895 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae5099156897 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae5099456899 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae5099756903 = private unnamed_addr constant [8 x i8] c"ignored\00", align 8

define ccc i32 @main() {
%mainenv56447 = call %struct.ScmObj* @const_init_null()
%mainargs56448 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv56447, %struct.ScmObj* %mainargs56448)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv56445,%struct.ScmObj* %mainargs56446) {
%stackaddr$makeclosure56449 = alloca %struct.ScmObj*, align 8
%fptrToInt56450 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47529 to i64
%ae47529 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56450)
store volatile %struct.ScmObj* %ae47529, %struct.ScmObj** %stackaddr$makeclosure56449, align 8
%ae47530 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56451 = alloca %struct.ScmObj*, align 8
%fptrToInt56452 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47531 to i64
%ae47531 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56452)
store volatile %struct.ScmObj* %ae47531, %struct.ScmObj** %stackaddr$makeclosure56451, align 8
%argslist56444$ae475290 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56453 = alloca %struct.ScmObj*, align 8
%argslist56444$ae475291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47531, %struct.ScmObj* %argslist56444$ae475290)
store volatile %struct.ScmObj* %argslist56444$ae475291, %struct.ScmObj** %stackaddr$prim56453, align 8
%stackaddr$prim56454 = alloca %struct.ScmObj*, align 8
%argslist56444$ae475292 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47530, %struct.ScmObj* %argslist56444$ae475291)
store volatile %struct.ScmObj* %argslist56444$ae475292, %struct.ScmObj** %stackaddr$prim56454, align 8
%clofunc56455 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47529)
musttail call tailcc void %clofunc56455(%struct.ScmObj* %ae47529, %struct.ScmObj* %argslist56444$ae475292)
ret void
}

define tailcc void @proc_clo$ae47529(%struct.ScmObj* %env$ae47529,%struct.ScmObj* %current_45args55918) {
%stackaddr$prim56456 = alloca %struct.ScmObj*, align 8
%_95k47369 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55918)
store volatile %struct.ScmObj* %_95k47369, %struct.ScmObj** %stackaddr$prim56456, align 8
%stackaddr$prim56457 = alloca %struct.ScmObj*, align 8
%current_45args55919 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55918)
store volatile %struct.ScmObj* %current_45args55919, %struct.ScmObj** %stackaddr$prim56457, align 8
%stackaddr$prim56458 = alloca %struct.ScmObj*, align 8
%anf_45bind47240 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55919)
store volatile %struct.ScmObj* %anf_45bind47240, %struct.ScmObj** %stackaddr$prim56458, align 8
%stackaddr$makeclosure56459 = alloca %struct.ScmObj*, align 8
%fptrToInt56460 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47544 to i64
%ae47544 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56460)
store volatile %struct.ScmObj* %ae47544, %struct.ScmObj** %stackaddr$makeclosure56459, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47544, %struct.ScmObj* %anf_45bind47240, i64 0)
%ae47545 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56461 = alloca %struct.ScmObj*, align 8
%fptrToInt56462 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47546 to i64
%ae47546 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56462)
store volatile %struct.ScmObj* %ae47546, %struct.ScmObj** %stackaddr$makeclosure56461, align 8
%argslist56439$ae475440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56463 = alloca %struct.ScmObj*, align 8
%argslist56439$ae475441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47546, %struct.ScmObj* %argslist56439$ae475440)
store volatile %struct.ScmObj* %argslist56439$ae475441, %struct.ScmObj** %stackaddr$prim56463, align 8
%stackaddr$prim56464 = alloca %struct.ScmObj*, align 8
%argslist56439$ae475442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47545, %struct.ScmObj* %argslist56439$ae475441)
store volatile %struct.ScmObj* %argslist56439$ae475442, %struct.ScmObj** %stackaddr$prim56464, align 8
%clofunc56465 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47544)
musttail call tailcc void %clofunc56465(%struct.ScmObj* %ae47544, %struct.ScmObj* %argslist56439$ae475442)
ret void
}

define tailcc void @proc_clo$ae47544(%struct.ScmObj* %env$ae47544,%struct.ScmObj* %current_45args55921) {
%stackaddr$env-ref56466 = alloca %struct.ScmObj*, align 8
%anf_45bind47240 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47544, i64 0)
store %struct.ScmObj* %anf_45bind47240, %struct.ScmObj** %stackaddr$env-ref56466
%stackaddr$prim56467 = alloca %struct.ScmObj*, align 8
%_95k47370 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55921)
store volatile %struct.ScmObj* %_95k47370, %struct.ScmObj** %stackaddr$prim56467, align 8
%stackaddr$prim56468 = alloca %struct.ScmObj*, align 8
%current_45args55922 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55921)
store volatile %struct.ScmObj* %current_45args55922, %struct.ScmObj** %stackaddr$prim56468, align 8
%stackaddr$prim56469 = alloca %struct.ScmObj*, align 8
%anf_45bind47244 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55922)
store volatile %struct.ScmObj* %anf_45bind47244, %struct.ScmObj** %stackaddr$prim56469, align 8
%stackaddr$makeclosure56470 = alloca %struct.ScmObj*, align 8
%fptrToInt56471 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47659 to i64
%ae47659 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56471)
store volatile %struct.ScmObj* %ae47659, %struct.ScmObj** %stackaddr$makeclosure56470, align 8
%argslist56418$anf_45bind472400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56472 = alloca %struct.ScmObj*, align 8
%argslist56418$anf_45bind472401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47244, %struct.ScmObj* %argslist56418$anf_45bind472400)
store volatile %struct.ScmObj* %argslist56418$anf_45bind472401, %struct.ScmObj** %stackaddr$prim56472, align 8
%stackaddr$prim56473 = alloca %struct.ScmObj*, align 8
%argslist56418$anf_45bind472402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47659, %struct.ScmObj* %argslist56418$anf_45bind472401)
store volatile %struct.ScmObj* %argslist56418$anf_45bind472402, %struct.ScmObj** %stackaddr$prim56473, align 8
%clofunc56474 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47240)
musttail call tailcc void %clofunc56474(%struct.ScmObj* %anf_45bind47240, %struct.ScmObj* %argslist56418$anf_45bind472402)
ret void
}

define tailcc void @proc_clo$ae47659(%struct.ScmObj* %env$ae47659,%struct.ScmObj* %current_45args55924) {
%stackaddr$prim56475 = alloca %struct.ScmObj*, align 8
%_95k47371 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55924)
store volatile %struct.ScmObj* %_95k47371, %struct.ScmObj** %stackaddr$prim56475, align 8
%stackaddr$prim56476 = alloca %struct.ScmObj*, align 8
%current_45args55925 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55924)
store volatile %struct.ScmObj* %current_45args55925, %struct.ScmObj** %stackaddr$prim56476, align 8
%stackaddr$prim56477 = alloca %struct.ScmObj*, align 8
%Ycmb47086 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55925)
store volatile %struct.ScmObj* %Ycmb47086, %struct.ScmObj** %stackaddr$prim56477, align 8
%stackaddr$makeclosure56478 = alloca %struct.ScmObj*, align 8
%fptrToInt56479 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47661 to i64
%ae47661 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56479)
store volatile %struct.ScmObj* %ae47661, %struct.ScmObj** %stackaddr$makeclosure56478, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47661, %struct.ScmObj* %Ycmb47086, i64 0)
%ae47662 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56480 = alloca %struct.ScmObj*, align 8
%fptrToInt56481 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47663 to i64
%ae47663 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56481)
store volatile %struct.ScmObj* %ae47663, %struct.ScmObj** %stackaddr$makeclosure56480, align 8
%argslist56417$ae476610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56482 = alloca %struct.ScmObj*, align 8
%argslist56417$ae476611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47663, %struct.ScmObj* %argslist56417$ae476610)
store volatile %struct.ScmObj* %argslist56417$ae476611, %struct.ScmObj** %stackaddr$prim56482, align 8
%stackaddr$prim56483 = alloca %struct.ScmObj*, align 8
%argslist56417$ae476612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47662, %struct.ScmObj* %argslist56417$ae476611)
store volatile %struct.ScmObj* %argslist56417$ae476612, %struct.ScmObj** %stackaddr$prim56483, align 8
%clofunc56484 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47661)
musttail call tailcc void %clofunc56484(%struct.ScmObj* %ae47661, %struct.ScmObj* %argslist56417$ae476612)
ret void
}

define tailcc void @proc_clo$ae47661(%struct.ScmObj* %env$ae47661,%struct.ScmObj* %current_45args55927) {
%stackaddr$env-ref56485 = alloca %struct.ScmObj*, align 8
%Ycmb47086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47661, i64 0)
store %struct.ScmObj* %Ycmb47086, %struct.ScmObj** %stackaddr$env-ref56485
%stackaddr$prim56486 = alloca %struct.ScmObj*, align 8
%_95k47372 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55927)
store volatile %struct.ScmObj* %_95k47372, %struct.ScmObj** %stackaddr$prim56486, align 8
%stackaddr$prim56487 = alloca %struct.ScmObj*, align 8
%current_45args55928 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55927)
store volatile %struct.ScmObj* %current_45args55928, %struct.ScmObj** %stackaddr$prim56487, align 8
%stackaddr$prim56488 = alloca %struct.ScmObj*, align 8
%anf_45bind47249 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55928)
store volatile %struct.ScmObj* %anf_45bind47249, %struct.ScmObj** %stackaddr$prim56488, align 8
%stackaddr$makeclosure56489 = alloca %struct.ScmObj*, align 8
%fptrToInt56490 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47739 to i64
%ae47739 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56490)
store volatile %struct.ScmObj* %ae47739, %struct.ScmObj** %stackaddr$makeclosure56489, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47739, %struct.ScmObj* %Ycmb47086, i64 0)
%argslist56401$Ycmb470860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56491 = alloca %struct.ScmObj*, align 8
%argslist56401$Ycmb470861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47249, %struct.ScmObj* %argslist56401$Ycmb470860)
store volatile %struct.ScmObj* %argslist56401$Ycmb470861, %struct.ScmObj** %stackaddr$prim56491, align 8
%stackaddr$prim56492 = alloca %struct.ScmObj*, align 8
%argslist56401$Ycmb470862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47739, %struct.ScmObj* %argslist56401$Ycmb470861)
store volatile %struct.ScmObj* %argslist56401$Ycmb470862, %struct.ScmObj** %stackaddr$prim56492, align 8
%clofunc56493 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47086)
musttail call tailcc void %clofunc56493(%struct.ScmObj* %Ycmb47086, %struct.ScmObj* %argslist56401$Ycmb470862)
ret void
}

define tailcc void @proc_clo$ae47739(%struct.ScmObj* %env$ae47739,%struct.ScmObj* %current_45args55930) {
%stackaddr$env-ref56494 = alloca %struct.ScmObj*, align 8
%Ycmb47086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47739, i64 0)
store %struct.ScmObj* %Ycmb47086, %struct.ScmObj** %stackaddr$env-ref56494
%stackaddr$prim56495 = alloca %struct.ScmObj*, align 8
%_95k47373 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55930)
store volatile %struct.ScmObj* %_95k47373, %struct.ScmObj** %stackaddr$prim56495, align 8
%stackaddr$prim56496 = alloca %struct.ScmObj*, align 8
%current_45args55931 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55930)
store volatile %struct.ScmObj* %current_45args55931, %struct.ScmObj** %stackaddr$prim56496, align 8
%stackaddr$prim56497 = alloca %struct.ScmObj*, align 8
%_37foldr147107 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55931)
store volatile %struct.ScmObj* %_37foldr147107, %struct.ScmObj** %stackaddr$prim56497, align 8
%stackaddr$makeclosure56498 = alloca %struct.ScmObj*, align 8
%fptrToInt56499 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47741 to i64
%ae47741 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56499)
store volatile %struct.ScmObj* %ae47741, %struct.ScmObj** %stackaddr$makeclosure56498, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47741, %struct.ScmObj* %_37foldr147107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47741, %struct.ScmObj* %Ycmb47086, i64 1)
%ae47742 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56500 = alloca %struct.ScmObj*, align 8
%fptrToInt56501 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47743 to i64
%ae47743 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56501)
store volatile %struct.ScmObj* %ae47743, %struct.ScmObj** %stackaddr$makeclosure56500, align 8
%argslist56400$ae477410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56502 = alloca %struct.ScmObj*, align 8
%argslist56400$ae477411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47743, %struct.ScmObj* %argslist56400$ae477410)
store volatile %struct.ScmObj* %argslist56400$ae477411, %struct.ScmObj** %stackaddr$prim56502, align 8
%stackaddr$prim56503 = alloca %struct.ScmObj*, align 8
%argslist56400$ae477412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47742, %struct.ScmObj* %argslist56400$ae477411)
store volatile %struct.ScmObj* %argslist56400$ae477412, %struct.ScmObj** %stackaddr$prim56503, align 8
%clofunc56504 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47741)
musttail call tailcc void %clofunc56504(%struct.ScmObj* %ae47741, %struct.ScmObj* %argslist56400$ae477412)
ret void
}

define tailcc void @proc_clo$ae47741(%struct.ScmObj* %env$ae47741,%struct.ScmObj* %current_45args55933) {
%stackaddr$env-ref56505 = alloca %struct.ScmObj*, align 8
%_37foldr147107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47741, i64 0)
store %struct.ScmObj* %_37foldr147107, %struct.ScmObj** %stackaddr$env-ref56505
%stackaddr$env-ref56506 = alloca %struct.ScmObj*, align 8
%Ycmb47086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47741, i64 1)
store %struct.ScmObj* %Ycmb47086, %struct.ScmObj** %stackaddr$env-ref56506
%stackaddr$prim56507 = alloca %struct.ScmObj*, align 8
%_95k47374 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55933)
store volatile %struct.ScmObj* %_95k47374, %struct.ScmObj** %stackaddr$prim56507, align 8
%stackaddr$prim56508 = alloca %struct.ScmObj*, align 8
%current_45args55934 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55933)
store volatile %struct.ScmObj* %current_45args55934, %struct.ScmObj** %stackaddr$prim56508, align 8
%stackaddr$prim56509 = alloca %struct.ScmObj*, align 8
%anf_45bind47255 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55934)
store volatile %struct.ScmObj* %anf_45bind47255, %struct.ScmObj** %stackaddr$prim56509, align 8
%stackaddr$makeclosure56510 = alloca %struct.ScmObj*, align 8
%fptrToInt56511 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47836 to i64
%ae47836 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56511)
store volatile %struct.ScmObj* %ae47836, %struct.ScmObj** %stackaddr$makeclosure56510, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47836, %struct.ScmObj* %_37foldr147107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47836, %struct.ScmObj* %Ycmb47086, i64 1)
%argslist56381$Ycmb470860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56512 = alloca %struct.ScmObj*, align 8
%argslist56381$Ycmb470861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47255, %struct.ScmObj* %argslist56381$Ycmb470860)
store volatile %struct.ScmObj* %argslist56381$Ycmb470861, %struct.ScmObj** %stackaddr$prim56512, align 8
%stackaddr$prim56513 = alloca %struct.ScmObj*, align 8
%argslist56381$Ycmb470862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47836, %struct.ScmObj* %argslist56381$Ycmb470861)
store volatile %struct.ScmObj* %argslist56381$Ycmb470862, %struct.ScmObj** %stackaddr$prim56513, align 8
%clofunc56514 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47086)
musttail call tailcc void %clofunc56514(%struct.ScmObj* %Ycmb47086, %struct.ScmObj* %argslist56381$Ycmb470862)
ret void
}

define tailcc void @proc_clo$ae47836(%struct.ScmObj* %env$ae47836,%struct.ScmObj* %current_45args55936) {
%stackaddr$env-ref56515 = alloca %struct.ScmObj*, align 8
%_37foldr147107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47836, i64 0)
store %struct.ScmObj* %_37foldr147107, %struct.ScmObj** %stackaddr$env-ref56515
%stackaddr$env-ref56516 = alloca %struct.ScmObj*, align 8
%Ycmb47086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47836, i64 1)
store %struct.ScmObj* %Ycmb47086, %struct.ScmObj** %stackaddr$env-ref56516
%stackaddr$prim56517 = alloca %struct.ScmObj*, align 8
%_95k47375 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55936)
store volatile %struct.ScmObj* %_95k47375, %struct.ScmObj** %stackaddr$prim56517, align 8
%stackaddr$prim56518 = alloca %struct.ScmObj*, align 8
%current_45args55937 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55936)
store volatile %struct.ScmObj* %current_45args55937, %struct.ScmObj** %stackaddr$prim56518, align 8
%stackaddr$prim56519 = alloca %struct.ScmObj*, align 8
%_37map147103 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55937)
store volatile %struct.ScmObj* %_37map147103, %struct.ScmObj** %stackaddr$prim56519, align 8
%stackaddr$makeclosure56520 = alloca %struct.ScmObj*, align 8
%fptrToInt56521 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47838 to i64
%ae47838 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56521)
store volatile %struct.ScmObj* %ae47838, %struct.ScmObj** %stackaddr$makeclosure56520, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47838, %struct.ScmObj* %_37foldr147107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47838, %struct.ScmObj* %_37map147103, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47838, %struct.ScmObj* %Ycmb47086, i64 2)
%ae47839 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56522 = alloca %struct.ScmObj*, align 8
%fptrToInt56523 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47840 to i64
%ae47840 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56523)
store volatile %struct.ScmObj* %ae47840, %struct.ScmObj** %stackaddr$makeclosure56522, align 8
%argslist56380$ae478380 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56524 = alloca %struct.ScmObj*, align 8
%argslist56380$ae478381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47840, %struct.ScmObj* %argslist56380$ae478380)
store volatile %struct.ScmObj* %argslist56380$ae478381, %struct.ScmObj** %stackaddr$prim56524, align 8
%stackaddr$prim56525 = alloca %struct.ScmObj*, align 8
%argslist56380$ae478382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47839, %struct.ScmObj* %argslist56380$ae478381)
store volatile %struct.ScmObj* %argslist56380$ae478382, %struct.ScmObj** %stackaddr$prim56525, align 8
%clofunc56526 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47838)
musttail call tailcc void %clofunc56526(%struct.ScmObj* %ae47838, %struct.ScmObj* %argslist56380$ae478382)
ret void
}

define tailcc void @proc_clo$ae47838(%struct.ScmObj* %env$ae47838,%struct.ScmObj* %current_45args55939) {
%stackaddr$env-ref56527 = alloca %struct.ScmObj*, align 8
%_37foldr147107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47838, i64 0)
store %struct.ScmObj* %_37foldr147107, %struct.ScmObj** %stackaddr$env-ref56527
%stackaddr$env-ref56528 = alloca %struct.ScmObj*, align 8
%_37map147103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47838, i64 1)
store %struct.ScmObj* %_37map147103, %struct.ScmObj** %stackaddr$env-ref56528
%stackaddr$env-ref56529 = alloca %struct.ScmObj*, align 8
%Ycmb47086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47838, i64 2)
store %struct.ScmObj* %Ycmb47086, %struct.ScmObj** %stackaddr$env-ref56529
%stackaddr$prim56530 = alloca %struct.ScmObj*, align 8
%_95k47376 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55939)
store volatile %struct.ScmObj* %_95k47376, %struct.ScmObj** %stackaddr$prim56530, align 8
%stackaddr$prim56531 = alloca %struct.ScmObj*, align 8
%current_45args55940 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55939)
store volatile %struct.ScmObj* %current_45args55940, %struct.ScmObj** %stackaddr$prim56531, align 8
%stackaddr$prim56532 = alloca %struct.ScmObj*, align 8
%anf_45bind47262 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55940)
store volatile %struct.ScmObj* %anf_45bind47262, %struct.ScmObj** %stackaddr$prim56532, align 8
%stackaddr$makeclosure56533 = alloca %struct.ScmObj*, align 8
%fptrToInt56534 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47986 to i64
%ae47986 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56534)
store volatile %struct.ScmObj* %ae47986, %struct.ScmObj** %stackaddr$makeclosure56533, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47986, %struct.ScmObj* %_37foldr147107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47986, %struct.ScmObj* %_37map147103, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47986, %struct.ScmObj* %Ycmb47086, i64 2)
%argslist56364$Ycmb470860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56535 = alloca %struct.ScmObj*, align 8
%argslist56364$Ycmb470861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47262, %struct.ScmObj* %argslist56364$Ycmb470860)
store volatile %struct.ScmObj* %argslist56364$Ycmb470861, %struct.ScmObj** %stackaddr$prim56535, align 8
%stackaddr$prim56536 = alloca %struct.ScmObj*, align 8
%argslist56364$Ycmb470862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47986, %struct.ScmObj* %argslist56364$Ycmb470861)
store volatile %struct.ScmObj* %argslist56364$Ycmb470862, %struct.ScmObj** %stackaddr$prim56536, align 8
%clofunc56537 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47086)
musttail call tailcc void %clofunc56537(%struct.ScmObj* %Ycmb47086, %struct.ScmObj* %argslist56364$Ycmb470862)
ret void
}

define tailcc void @proc_clo$ae47986(%struct.ScmObj* %env$ae47986,%struct.ScmObj* %current_45args55942) {
%stackaddr$env-ref56538 = alloca %struct.ScmObj*, align 8
%_37foldr147107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47986, i64 0)
store %struct.ScmObj* %_37foldr147107, %struct.ScmObj** %stackaddr$env-ref56538
%stackaddr$env-ref56539 = alloca %struct.ScmObj*, align 8
%_37map147103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47986, i64 1)
store %struct.ScmObj* %_37map147103, %struct.ScmObj** %stackaddr$env-ref56539
%stackaddr$env-ref56540 = alloca %struct.ScmObj*, align 8
%Ycmb47086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47986, i64 2)
store %struct.ScmObj* %Ycmb47086, %struct.ScmObj** %stackaddr$env-ref56540
%stackaddr$prim56541 = alloca %struct.ScmObj*, align 8
%_95k47377 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55942)
store volatile %struct.ScmObj* %_95k47377, %struct.ScmObj** %stackaddr$prim56541, align 8
%stackaddr$prim56542 = alloca %struct.ScmObj*, align 8
%current_45args55943 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55942)
store volatile %struct.ScmObj* %current_45args55943, %struct.ScmObj** %stackaddr$prim56542, align 8
%stackaddr$prim56543 = alloca %struct.ScmObj*, align 8
%_37take47099 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55943)
store volatile %struct.ScmObj* %_37take47099, %struct.ScmObj** %stackaddr$prim56543, align 8
%stackaddr$makeclosure56544 = alloca %struct.ScmObj*, align 8
%fptrToInt56545 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47988 to i64
%ae47988 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56545)
store volatile %struct.ScmObj* %ae47988, %struct.ScmObj** %stackaddr$makeclosure56544, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47988, %struct.ScmObj* %_37foldr147107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47988, %struct.ScmObj* %_37map147103, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47988, %struct.ScmObj* %Ycmb47086, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae47988, %struct.ScmObj* %_37take47099, i64 3)
%ae47989 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56546 = alloca %struct.ScmObj*, align 8
%fptrToInt56547 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47990 to i64
%ae47990 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56547)
store volatile %struct.ScmObj* %ae47990, %struct.ScmObj** %stackaddr$makeclosure56546, align 8
%argslist56363$ae479880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56548 = alloca %struct.ScmObj*, align 8
%argslist56363$ae479881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47990, %struct.ScmObj* %argslist56363$ae479880)
store volatile %struct.ScmObj* %argslist56363$ae479881, %struct.ScmObj** %stackaddr$prim56548, align 8
%stackaddr$prim56549 = alloca %struct.ScmObj*, align 8
%argslist56363$ae479882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47989, %struct.ScmObj* %argslist56363$ae479881)
store volatile %struct.ScmObj* %argslist56363$ae479882, %struct.ScmObj** %stackaddr$prim56549, align 8
%clofunc56550 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47988)
musttail call tailcc void %clofunc56550(%struct.ScmObj* %ae47988, %struct.ScmObj* %argslist56363$ae479882)
ret void
}

define tailcc void @proc_clo$ae47988(%struct.ScmObj* %env$ae47988,%struct.ScmObj* %current_45args55945) {
%stackaddr$env-ref56551 = alloca %struct.ScmObj*, align 8
%_37foldr147107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47988, i64 0)
store %struct.ScmObj* %_37foldr147107, %struct.ScmObj** %stackaddr$env-ref56551
%stackaddr$env-ref56552 = alloca %struct.ScmObj*, align 8
%_37map147103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47988, i64 1)
store %struct.ScmObj* %_37map147103, %struct.ScmObj** %stackaddr$env-ref56552
%stackaddr$env-ref56553 = alloca %struct.ScmObj*, align 8
%Ycmb47086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47988, i64 2)
store %struct.ScmObj* %Ycmb47086, %struct.ScmObj** %stackaddr$env-ref56553
%stackaddr$env-ref56554 = alloca %struct.ScmObj*, align 8
%_37take47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47988, i64 3)
store %struct.ScmObj* %_37take47099, %struct.ScmObj** %stackaddr$env-ref56554
%stackaddr$prim56555 = alloca %struct.ScmObj*, align 8
%_95k47378 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55945)
store volatile %struct.ScmObj* %_95k47378, %struct.ScmObj** %stackaddr$prim56555, align 8
%stackaddr$prim56556 = alloca %struct.ScmObj*, align 8
%current_45args55946 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55945)
store volatile %struct.ScmObj* %current_45args55946, %struct.ScmObj** %stackaddr$prim56556, align 8
%stackaddr$prim56557 = alloca %struct.ScmObj*, align 8
%anf_45bind47266 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55946)
store volatile %struct.ScmObj* %anf_45bind47266, %struct.ScmObj** %stackaddr$prim56557, align 8
%stackaddr$makeclosure56558 = alloca %struct.ScmObj*, align 8
%fptrToInt56559 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48069 to i64
%ae48069 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56559)
store volatile %struct.ScmObj* %ae48069, %struct.ScmObj** %stackaddr$makeclosure56558, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48069, %struct.ScmObj* %_37foldr147107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48069, %struct.ScmObj* %_37map147103, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48069, %struct.ScmObj* %Ycmb47086, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48069, %struct.ScmObj* %_37take47099, i64 3)
%argslist56349$Ycmb470860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56560 = alloca %struct.ScmObj*, align 8
%argslist56349$Ycmb470861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47266, %struct.ScmObj* %argslist56349$Ycmb470860)
store volatile %struct.ScmObj* %argslist56349$Ycmb470861, %struct.ScmObj** %stackaddr$prim56560, align 8
%stackaddr$prim56561 = alloca %struct.ScmObj*, align 8
%argslist56349$Ycmb470862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48069, %struct.ScmObj* %argslist56349$Ycmb470861)
store volatile %struct.ScmObj* %argslist56349$Ycmb470862, %struct.ScmObj** %stackaddr$prim56561, align 8
%clofunc56562 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47086)
musttail call tailcc void %clofunc56562(%struct.ScmObj* %Ycmb47086, %struct.ScmObj* %argslist56349$Ycmb470862)
ret void
}

define tailcc void @proc_clo$ae48069(%struct.ScmObj* %env$ae48069,%struct.ScmObj* %current_45args55948) {
%stackaddr$env-ref56563 = alloca %struct.ScmObj*, align 8
%_37foldr147107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48069, i64 0)
store %struct.ScmObj* %_37foldr147107, %struct.ScmObj** %stackaddr$env-ref56563
%stackaddr$env-ref56564 = alloca %struct.ScmObj*, align 8
%_37map147103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48069, i64 1)
store %struct.ScmObj* %_37map147103, %struct.ScmObj** %stackaddr$env-ref56564
%stackaddr$env-ref56565 = alloca %struct.ScmObj*, align 8
%Ycmb47086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48069, i64 2)
store %struct.ScmObj* %Ycmb47086, %struct.ScmObj** %stackaddr$env-ref56565
%stackaddr$env-ref56566 = alloca %struct.ScmObj*, align 8
%_37take47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48069, i64 3)
store %struct.ScmObj* %_37take47099, %struct.ScmObj** %stackaddr$env-ref56566
%stackaddr$prim56567 = alloca %struct.ScmObj*, align 8
%_95k47379 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55948)
store volatile %struct.ScmObj* %_95k47379, %struct.ScmObj** %stackaddr$prim56567, align 8
%stackaddr$prim56568 = alloca %struct.ScmObj*, align 8
%current_45args55949 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55948)
store volatile %struct.ScmObj* %current_45args55949, %struct.ScmObj** %stackaddr$prim56568, align 8
%stackaddr$prim56569 = alloca %struct.ScmObj*, align 8
%_37length47096 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55949)
store volatile %struct.ScmObj* %_37length47096, %struct.ScmObj** %stackaddr$prim56569, align 8
%stackaddr$makeclosure56570 = alloca %struct.ScmObj*, align 8
%fptrToInt56571 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48071 to i64
%ae48071 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt56571)
store volatile %struct.ScmObj* %ae48071, %struct.ScmObj** %stackaddr$makeclosure56570, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48071, %struct.ScmObj* %_37foldr147107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48071, %struct.ScmObj* %_37map147103, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48071, %struct.ScmObj* %Ycmb47086, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48071, %struct.ScmObj* %_37take47099, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48071, %struct.ScmObj* %_37length47096, i64 4)
%ae48072 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56572 = alloca %struct.ScmObj*, align 8
%fptrToInt56573 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48073 to i64
%ae48073 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56573)
store volatile %struct.ScmObj* %ae48073, %struct.ScmObj** %stackaddr$makeclosure56572, align 8
%argslist56348$ae480710 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56574 = alloca %struct.ScmObj*, align 8
%argslist56348$ae480711 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48073, %struct.ScmObj* %argslist56348$ae480710)
store volatile %struct.ScmObj* %argslist56348$ae480711, %struct.ScmObj** %stackaddr$prim56574, align 8
%stackaddr$prim56575 = alloca %struct.ScmObj*, align 8
%argslist56348$ae480712 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48072, %struct.ScmObj* %argslist56348$ae480711)
store volatile %struct.ScmObj* %argslist56348$ae480712, %struct.ScmObj** %stackaddr$prim56575, align 8
%clofunc56576 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48071)
musttail call tailcc void %clofunc56576(%struct.ScmObj* %ae48071, %struct.ScmObj* %argslist56348$ae480712)
ret void
}

define tailcc void @proc_clo$ae48071(%struct.ScmObj* %env$ae48071,%struct.ScmObj* %current_45args55951) {
%stackaddr$env-ref56577 = alloca %struct.ScmObj*, align 8
%_37foldr147107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48071, i64 0)
store %struct.ScmObj* %_37foldr147107, %struct.ScmObj** %stackaddr$env-ref56577
%stackaddr$env-ref56578 = alloca %struct.ScmObj*, align 8
%_37map147103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48071, i64 1)
store %struct.ScmObj* %_37map147103, %struct.ScmObj** %stackaddr$env-ref56578
%stackaddr$env-ref56579 = alloca %struct.ScmObj*, align 8
%Ycmb47086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48071, i64 2)
store %struct.ScmObj* %Ycmb47086, %struct.ScmObj** %stackaddr$env-ref56579
%stackaddr$env-ref56580 = alloca %struct.ScmObj*, align 8
%_37take47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48071, i64 3)
store %struct.ScmObj* %_37take47099, %struct.ScmObj** %stackaddr$env-ref56580
%stackaddr$env-ref56581 = alloca %struct.ScmObj*, align 8
%_37length47096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48071, i64 4)
store %struct.ScmObj* %_37length47096, %struct.ScmObj** %stackaddr$env-ref56581
%stackaddr$prim56582 = alloca %struct.ScmObj*, align 8
%_95k47380 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55951)
store volatile %struct.ScmObj* %_95k47380, %struct.ScmObj** %stackaddr$prim56582, align 8
%stackaddr$prim56583 = alloca %struct.ScmObj*, align 8
%current_45args55952 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55951)
store volatile %struct.ScmObj* %current_45args55952, %struct.ScmObj** %stackaddr$prim56583, align 8
%stackaddr$prim56584 = alloca %struct.ScmObj*, align 8
%anf_45bind47271 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55952)
store volatile %struct.ScmObj* %anf_45bind47271, %struct.ScmObj** %stackaddr$prim56584, align 8
%stackaddr$makeclosure56585 = alloca %struct.ScmObj*, align 8
%fptrToInt56586 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48148 to i64
%ae48148 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt56586)
store volatile %struct.ScmObj* %ae48148, %struct.ScmObj** %stackaddr$makeclosure56585, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48148, %struct.ScmObj* %_37foldr147107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48148, %struct.ScmObj* %_37map147103, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48148, %struct.ScmObj* %Ycmb47086, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48148, %struct.ScmObj* %_37take47099, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48148, %struct.ScmObj* %_37length47096, i64 4)
%argslist56332$Ycmb470860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56587 = alloca %struct.ScmObj*, align 8
%argslist56332$Ycmb470861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47271, %struct.ScmObj* %argslist56332$Ycmb470860)
store volatile %struct.ScmObj* %argslist56332$Ycmb470861, %struct.ScmObj** %stackaddr$prim56587, align 8
%stackaddr$prim56588 = alloca %struct.ScmObj*, align 8
%argslist56332$Ycmb470862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48148, %struct.ScmObj* %argslist56332$Ycmb470861)
store volatile %struct.ScmObj* %argslist56332$Ycmb470862, %struct.ScmObj** %stackaddr$prim56588, align 8
%clofunc56589 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47086)
musttail call tailcc void %clofunc56589(%struct.ScmObj* %Ycmb47086, %struct.ScmObj* %argslist56332$Ycmb470862)
ret void
}

define tailcc void @proc_clo$ae48148(%struct.ScmObj* %env$ae48148,%struct.ScmObj* %current_45args55954) {
%stackaddr$env-ref56590 = alloca %struct.ScmObj*, align 8
%_37foldr147107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48148, i64 0)
store %struct.ScmObj* %_37foldr147107, %struct.ScmObj** %stackaddr$env-ref56590
%stackaddr$env-ref56591 = alloca %struct.ScmObj*, align 8
%_37map147103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48148, i64 1)
store %struct.ScmObj* %_37map147103, %struct.ScmObj** %stackaddr$env-ref56591
%stackaddr$env-ref56592 = alloca %struct.ScmObj*, align 8
%Ycmb47086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48148, i64 2)
store %struct.ScmObj* %Ycmb47086, %struct.ScmObj** %stackaddr$env-ref56592
%stackaddr$env-ref56593 = alloca %struct.ScmObj*, align 8
%_37take47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48148, i64 3)
store %struct.ScmObj* %_37take47099, %struct.ScmObj** %stackaddr$env-ref56593
%stackaddr$env-ref56594 = alloca %struct.ScmObj*, align 8
%_37length47096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48148, i64 4)
store %struct.ScmObj* %_37length47096, %struct.ScmObj** %stackaddr$env-ref56594
%stackaddr$prim56595 = alloca %struct.ScmObj*, align 8
%_95k47381 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55954)
store volatile %struct.ScmObj* %_95k47381, %struct.ScmObj** %stackaddr$prim56595, align 8
%stackaddr$prim56596 = alloca %struct.ScmObj*, align 8
%current_45args55955 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55954)
store volatile %struct.ScmObj* %current_45args55955, %struct.ScmObj** %stackaddr$prim56596, align 8
%stackaddr$prim56597 = alloca %struct.ScmObj*, align 8
%_37foldl147091 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55955)
store volatile %struct.ScmObj* %_37foldl147091, %struct.ScmObj** %stackaddr$prim56597, align 8
%stackaddr$makeclosure56598 = alloca %struct.ScmObj*, align 8
%fptrToInt56599 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48150 to i64
%ae48150 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt56599)
store volatile %struct.ScmObj* %ae48150, %struct.ScmObj** %stackaddr$makeclosure56598, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48150, %struct.ScmObj* %_37foldr147107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48150, %struct.ScmObj* %_37foldl147091, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48150, %struct.ScmObj* %_37map147103, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48150, %struct.ScmObj* %Ycmb47086, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48150, %struct.ScmObj* %_37take47099, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48150, %struct.ScmObj* %_37length47096, i64 5)
%ae48151 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56600 = alloca %struct.ScmObj*, align 8
%fptrToInt56601 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48152 to i64
%ae48152 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56601)
store volatile %struct.ScmObj* %ae48152, %struct.ScmObj** %stackaddr$makeclosure56600, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48152, %struct.ScmObj* %_37foldl147091, i64 0)
%argslist56331$ae481500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56602 = alloca %struct.ScmObj*, align 8
%argslist56331$ae481501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48152, %struct.ScmObj* %argslist56331$ae481500)
store volatile %struct.ScmObj* %argslist56331$ae481501, %struct.ScmObj** %stackaddr$prim56602, align 8
%stackaddr$prim56603 = alloca %struct.ScmObj*, align 8
%argslist56331$ae481502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48151, %struct.ScmObj* %argslist56331$ae481501)
store volatile %struct.ScmObj* %argslist56331$ae481502, %struct.ScmObj** %stackaddr$prim56603, align 8
%clofunc56604 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48150)
musttail call tailcc void %clofunc56604(%struct.ScmObj* %ae48150, %struct.ScmObj* %argslist56331$ae481502)
ret void
}

define tailcc void @proc_clo$ae48150(%struct.ScmObj* %env$ae48150,%struct.ScmObj* %current_45args55957) {
%stackaddr$env-ref56605 = alloca %struct.ScmObj*, align 8
%_37foldr147107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48150, i64 0)
store %struct.ScmObj* %_37foldr147107, %struct.ScmObj** %stackaddr$env-ref56605
%stackaddr$env-ref56606 = alloca %struct.ScmObj*, align 8
%_37foldl147091 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48150, i64 1)
store %struct.ScmObj* %_37foldl147091, %struct.ScmObj** %stackaddr$env-ref56606
%stackaddr$env-ref56607 = alloca %struct.ScmObj*, align 8
%_37map147103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48150, i64 2)
store %struct.ScmObj* %_37map147103, %struct.ScmObj** %stackaddr$env-ref56607
%stackaddr$env-ref56608 = alloca %struct.ScmObj*, align 8
%Ycmb47086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48150, i64 3)
store %struct.ScmObj* %Ycmb47086, %struct.ScmObj** %stackaddr$env-ref56608
%stackaddr$env-ref56609 = alloca %struct.ScmObj*, align 8
%_37take47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48150, i64 4)
store %struct.ScmObj* %_37take47099, %struct.ScmObj** %stackaddr$env-ref56609
%stackaddr$env-ref56610 = alloca %struct.ScmObj*, align 8
%_37length47096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48150, i64 5)
store %struct.ScmObj* %_37length47096, %struct.ScmObj** %stackaddr$env-ref56610
%stackaddr$prim56611 = alloca %struct.ScmObj*, align 8
%_95k47382 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55957)
store volatile %struct.ScmObj* %_95k47382, %struct.ScmObj** %stackaddr$prim56611, align 8
%stackaddr$prim56612 = alloca %struct.ScmObj*, align 8
%current_45args55958 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55957)
store volatile %struct.ScmObj* %current_45args55958, %struct.ScmObj** %stackaddr$prim56612, align 8
%stackaddr$prim56613 = alloca %struct.ScmObj*, align 8
%_37last47129 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55958)
store volatile %struct.ScmObj* %_37last47129, %struct.ScmObj** %stackaddr$prim56613, align 8
%stackaddr$makeclosure56614 = alloca %struct.ScmObj*, align 8
%fptrToInt56615 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48204 to i64
%ae48204 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt56615)
store volatile %struct.ScmObj* %ae48204, %struct.ScmObj** %stackaddr$makeclosure56614, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48204, %struct.ScmObj* %_37foldr147107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48204, %struct.ScmObj* %_37foldl147091, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48204, %struct.ScmObj* %_37map147103, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48204, %struct.ScmObj* %Ycmb47086, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48204, %struct.ScmObj* %_37last47129, i64 4)
%ae48205 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56616 = alloca %struct.ScmObj*, align 8
%fptrToInt56617 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48206 to i64
%ae48206 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56617)
store volatile %struct.ScmObj* %ae48206, %struct.ScmObj** %stackaddr$makeclosure56616, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48206, %struct.ScmObj* %_37take47099, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48206, %struct.ScmObj* %_37length47096, i64 1)
%argslist56317$ae482040 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56618 = alloca %struct.ScmObj*, align 8
%argslist56317$ae482041 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48206, %struct.ScmObj* %argslist56317$ae482040)
store volatile %struct.ScmObj* %argslist56317$ae482041, %struct.ScmObj** %stackaddr$prim56618, align 8
%stackaddr$prim56619 = alloca %struct.ScmObj*, align 8
%argslist56317$ae482042 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48205, %struct.ScmObj* %argslist56317$ae482041)
store volatile %struct.ScmObj* %argslist56317$ae482042, %struct.ScmObj** %stackaddr$prim56619, align 8
%clofunc56620 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48204)
musttail call tailcc void %clofunc56620(%struct.ScmObj* %ae48204, %struct.ScmObj* %argslist56317$ae482042)
ret void
}

define tailcc void @proc_clo$ae48204(%struct.ScmObj* %env$ae48204,%struct.ScmObj* %current_45args55960) {
%stackaddr$env-ref56621 = alloca %struct.ScmObj*, align 8
%_37foldr147107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48204, i64 0)
store %struct.ScmObj* %_37foldr147107, %struct.ScmObj** %stackaddr$env-ref56621
%stackaddr$env-ref56622 = alloca %struct.ScmObj*, align 8
%_37foldl147091 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48204, i64 1)
store %struct.ScmObj* %_37foldl147091, %struct.ScmObj** %stackaddr$env-ref56622
%stackaddr$env-ref56623 = alloca %struct.ScmObj*, align 8
%_37map147103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48204, i64 2)
store %struct.ScmObj* %_37map147103, %struct.ScmObj** %stackaddr$env-ref56623
%stackaddr$env-ref56624 = alloca %struct.ScmObj*, align 8
%Ycmb47086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48204, i64 3)
store %struct.ScmObj* %Ycmb47086, %struct.ScmObj** %stackaddr$env-ref56624
%stackaddr$env-ref56625 = alloca %struct.ScmObj*, align 8
%_37last47129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48204, i64 4)
store %struct.ScmObj* %_37last47129, %struct.ScmObj** %stackaddr$env-ref56625
%stackaddr$prim56626 = alloca %struct.ScmObj*, align 8
%_95k47383 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55960)
store volatile %struct.ScmObj* %_95k47383, %struct.ScmObj** %stackaddr$prim56626, align 8
%stackaddr$prim56627 = alloca %struct.ScmObj*, align 8
%current_45args55961 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55960)
store volatile %struct.ScmObj* %current_45args55961, %struct.ScmObj** %stackaddr$prim56627, align 8
%stackaddr$prim56628 = alloca %struct.ScmObj*, align 8
%_37drop_45right47126 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55961)
store volatile %struct.ScmObj* %_37drop_45right47126, %struct.ScmObj** %stackaddr$prim56628, align 8
%stackaddr$makeclosure56629 = alloca %struct.ScmObj*, align 8
%fptrToInt56630 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48234 to i64
%ae48234 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt56630)
store volatile %struct.ScmObj* %ae48234, %struct.ScmObj** %stackaddr$makeclosure56629, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48234, %struct.ScmObj* %_37foldr147107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48234, %struct.ScmObj* %_37foldl147091, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48234, %struct.ScmObj* %_37drop_45right47126, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48234, %struct.ScmObj* %Ycmb47086, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48234, %struct.ScmObj* %_37last47129, i64 4)
%ae48235 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56631 = alloca %struct.ScmObj*, align 8
%fptrToInt56632 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48236 to i64
%ae48236 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56632)
store volatile %struct.ScmObj* %ae48236, %struct.ScmObj** %stackaddr$makeclosure56631, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48236, %struct.ScmObj* %_37foldr147107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48236, %struct.ScmObj* %_37map147103, i64 1)
%argslist56307$ae482340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56633 = alloca %struct.ScmObj*, align 8
%argslist56307$ae482341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48236, %struct.ScmObj* %argslist56307$ae482340)
store volatile %struct.ScmObj* %argslist56307$ae482341, %struct.ScmObj** %stackaddr$prim56633, align 8
%stackaddr$prim56634 = alloca %struct.ScmObj*, align 8
%argslist56307$ae482342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48235, %struct.ScmObj* %argslist56307$ae482341)
store volatile %struct.ScmObj* %argslist56307$ae482342, %struct.ScmObj** %stackaddr$prim56634, align 8
%clofunc56635 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48234)
musttail call tailcc void %clofunc56635(%struct.ScmObj* %ae48234, %struct.ScmObj* %argslist56307$ae482342)
ret void
}

define tailcc void @proc_clo$ae48234(%struct.ScmObj* %env$ae48234,%struct.ScmObj* %current_45args55963) {
%stackaddr$env-ref56636 = alloca %struct.ScmObj*, align 8
%_37foldr147107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48234, i64 0)
store %struct.ScmObj* %_37foldr147107, %struct.ScmObj** %stackaddr$env-ref56636
%stackaddr$env-ref56637 = alloca %struct.ScmObj*, align 8
%_37foldl147091 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48234, i64 1)
store %struct.ScmObj* %_37foldl147091, %struct.ScmObj** %stackaddr$env-ref56637
%stackaddr$env-ref56638 = alloca %struct.ScmObj*, align 8
%_37drop_45right47126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48234, i64 2)
store %struct.ScmObj* %_37drop_45right47126, %struct.ScmObj** %stackaddr$env-ref56638
%stackaddr$env-ref56639 = alloca %struct.ScmObj*, align 8
%Ycmb47086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48234, i64 3)
store %struct.ScmObj* %Ycmb47086, %struct.ScmObj** %stackaddr$env-ref56639
%stackaddr$env-ref56640 = alloca %struct.ScmObj*, align 8
%_37last47129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48234, i64 4)
store %struct.ScmObj* %_37last47129, %struct.ScmObj** %stackaddr$env-ref56640
%stackaddr$prim56641 = alloca %struct.ScmObj*, align 8
%_95k47384 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55963)
store volatile %struct.ScmObj* %_95k47384, %struct.ScmObj** %stackaddr$prim56641, align 8
%stackaddr$prim56642 = alloca %struct.ScmObj*, align 8
%current_45args55964 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55963)
store volatile %struct.ScmObj* %current_45args55964, %struct.ScmObj** %stackaddr$prim56642, align 8
%stackaddr$prim56643 = alloca %struct.ScmObj*, align 8
%anf_45bind47287 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55964)
store volatile %struct.ScmObj* %anf_45bind47287, %struct.ScmObj** %stackaddr$prim56643, align 8
%stackaddr$makeclosure56644 = alloca %struct.ScmObj*, align 8
%fptrToInt56645 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48618 to i64
%ae48618 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt56645)
store volatile %struct.ScmObj* %ae48618, %struct.ScmObj** %stackaddr$makeclosure56644, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48618, %struct.ScmObj* %_37foldr147107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48618, %struct.ScmObj* %_37foldl147091, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48618, %struct.ScmObj* %_37drop_45right47126, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48618, %struct.ScmObj* %Ycmb47086, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48618, %struct.ScmObj* %_37last47129, i64 4)
%argslist56247$Ycmb470860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56646 = alloca %struct.ScmObj*, align 8
%argslist56247$Ycmb470861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47287, %struct.ScmObj* %argslist56247$Ycmb470860)
store volatile %struct.ScmObj* %argslist56247$Ycmb470861, %struct.ScmObj** %stackaddr$prim56646, align 8
%stackaddr$prim56647 = alloca %struct.ScmObj*, align 8
%argslist56247$Ycmb470862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48618, %struct.ScmObj* %argslist56247$Ycmb470861)
store volatile %struct.ScmObj* %argslist56247$Ycmb470862, %struct.ScmObj** %stackaddr$prim56647, align 8
%clofunc56648 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47086)
musttail call tailcc void %clofunc56648(%struct.ScmObj* %Ycmb47086, %struct.ScmObj* %argslist56247$Ycmb470862)
ret void
}

define tailcc void @proc_clo$ae48618(%struct.ScmObj* %env$ae48618,%struct.ScmObj* %current_45args55966) {
%stackaddr$env-ref56649 = alloca %struct.ScmObj*, align 8
%_37foldr147107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48618, i64 0)
store %struct.ScmObj* %_37foldr147107, %struct.ScmObj** %stackaddr$env-ref56649
%stackaddr$env-ref56650 = alloca %struct.ScmObj*, align 8
%_37foldl147091 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48618, i64 1)
store %struct.ScmObj* %_37foldl147091, %struct.ScmObj** %stackaddr$env-ref56650
%stackaddr$env-ref56651 = alloca %struct.ScmObj*, align 8
%_37drop_45right47126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48618, i64 2)
store %struct.ScmObj* %_37drop_45right47126, %struct.ScmObj** %stackaddr$env-ref56651
%stackaddr$env-ref56652 = alloca %struct.ScmObj*, align 8
%Ycmb47086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48618, i64 3)
store %struct.ScmObj* %Ycmb47086, %struct.ScmObj** %stackaddr$env-ref56652
%stackaddr$env-ref56653 = alloca %struct.ScmObj*, align 8
%_37last47129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48618, i64 4)
store %struct.ScmObj* %_37last47129, %struct.ScmObj** %stackaddr$env-ref56653
%stackaddr$prim56654 = alloca %struct.ScmObj*, align 8
%_95k47385 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55966)
store volatile %struct.ScmObj* %_95k47385, %struct.ScmObj** %stackaddr$prim56654, align 8
%stackaddr$prim56655 = alloca %struct.ScmObj*, align 8
%current_45args55967 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55966)
store volatile %struct.ScmObj* %current_45args55967, %struct.ScmObj** %stackaddr$prim56655, align 8
%stackaddr$prim56656 = alloca %struct.ScmObj*, align 8
%_37foldr47112 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55967)
store volatile %struct.ScmObj* %_37foldr47112, %struct.ScmObj** %stackaddr$prim56656, align 8
%stackaddr$makeclosure56657 = alloca %struct.ScmObj*, align 8
%fptrToInt56658 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48620 to i64
%ae48620 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt56658)
store volatile %struct.ScmObj* %ae48620, %struct.ScmObj** %stackaddr$makeclosure56657, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48620, %struct.ScmObj* %_37foldr147107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48620, %struct.ScmObj* %_37foldl147091, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48620, %struct.ScmObj* %_37drop_45right47126, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48620, %struct.ScmObj* %Ycmb47086, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48620, %struct.ScmObj* %_37last47129, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48620, %struct.ScmObj* %_37foldr47112, i64 5)
%ae48621 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56659 = alloca %struct.ScmObj*, align 8
%fptrToInt56660 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48622 to i64
%ae48622 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56660)
store volatile %struct.ScmObj* %ae48622, %struct.ScmObj** %stackaddr$makeclosure56659, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48622, %struct.ScmObj* %_37foldr147107, i64 0)
%argslist56246$ae486200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56661 = alloca %struct.ScmObj*, align 8
%argslist56246$ae486201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48622, %struct.ScmObj* %argslist56246$ae486200)
store volatile %struct.ScmObj* %argslist56246$ae486201, %struct.ScmObj** %stackaddr$prim56661, align 8
%stackaddr$prim56662 = alloca %struct.ScmObj*, align 8
%argslist56246$ae486202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48621, %struct.ScmObj* %argslist56246$ae486201)
store volatile %struct.ScmObj* %argslist56246$ae486202, %struct.ScmObj** %stackaddr$prim56662, align 8
%clofunc56663 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48620)
musttail call tailcc void %clofunc56663(%struct.ScmObj* %ae48620, %struct.ScmObj* %argslist56246$ae486202)
ret void
}

define tailcc void @proc_clo$ae48620(%struct.ScmObj* %env$ae48620,%struct.ScmObj* %current_45args55969) {
%stackaddr$env-ref56664 = alloca %struct.ScmObj*, align 8
%_37foldr147107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48620, i64 0)
store %struct.ScmObj* %_37foldr147107, %struct.ScmObj** %stackaddr$env-ref56664
%stackaddr$env-ref56665 = alloca %struct.ScmObj*, align 8
%_37foldl147091 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48620, i64 1)
store %struct.ScmObj* %_37foldl147091, %struct.ScmObj** %stackaddr$env-ref56665
%stackaddr$env-ref56666 = alloca %struct.ScmObj*, align 8
%_37drop_45right47126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48620, i64 2)
store %struct.ScmObj* %_37drop_45right47126, %struct.ScmObj** %stackaddr$env-ref56666
%stackaddr$env-ref56667 = alloca %struct.ScmObj*, align 8
%Ycmb47086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48620, i64 3)
store %struct.ScmObj* %Ycmb47086, %struct.ScmObj** %stackaddr$env-ref56667
%stackaddr$env-ref56668 = alloca %struct.ScmObj*, align 8
%_37last47129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48620, i64 4)
store %struct.ScmObj* %_37last47129, %struct.ScmObj** %stackaddr$env-ref56668
%stackaddr$env-ref56669 = alloca %struct.ScmObj*, align 8
%_37foldr47112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48620, i64 5)
store %struct.ScmObj* %_37foldr47112, %struct.ScmObj** %stackaddr$env-ref56669
%stackaddr$prim56670 = alloca %struct.ScmObj*, align 8
%_95k47386 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55969)
store volatile %struct.ScmObj* %_95k47386, %struct.ScmObj** %stackaddr$prim56670, align 8
%stackaddr$prim56671 = alloca %struct.ScmObj*, align 8
%current_45args55970 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55969)
store volatile %struct.ScmObj* %current_45args55970, %struct.ScmObj** %stackaddr$prim56671, align 8
%stackaddr$prim56672 = alloca %struct.ScmObj*, align 8
%_37map147138 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55970)
store volatile %struct.ScmObj* %_37map147138, %struct.ScmObj** %stackaddr$prim56672, align 8
%stackaddr$makeclosure56673 = alloca %struct.ScmObj*, align 8
%fptrToInt56674 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48697 to i64
%ae48697 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt56674)
store volatile %struct.ScmObj* %ae48697, %struct.ScmObj** %stackaddr$makeclosure56673, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48697, %struct.ScmObj* %_37foldr147107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48697, %struct.ScmObj* %_37foldl147091, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48697, %struct.ScmObj* %_37map147138, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48697, %struct.ScmObj* %Ycmb47086, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48697, %struct.ScmObj* %_37foldr47112, i64 4)
%ae48698 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56675 = alloca %struct.ScmObj*, align 8
%fptrToInt56676 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48699 to i64
%ae48699 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56676)
store volatile %struct.ScmObj* %ae48699, %struct.ScmObj** %stackaddr$makeclosure56675, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48699, %struct.ScmObj* %_37drop_45right47126, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48699, %struct.ScmObj* %_37last47129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48699, %struct.ScmObj* %_37foldr47112, i64 2)
%argslist56227$ae486970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56677 = alloca %struct.ScmObj*, align 8
%argslist56227$ae486971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48699, %struct.ScmObj* %argslist56227$ae486970)
store volatile %struct.ScmObj* %argslist56227$ae486971, %struct.ScmObj** %stackaddr$prim56677, align 8
%stackaddr$prim56678 = alloca %struct.ScmObj*, align 8
%argslist56227$ae486972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48698, %struct.ScmObj* %argslist56227$ae486971)
store volatile %struct.ScmObj* %argslist56227$ae486972, %struct.ScmObj** %stackaddr$prim56678, align 8
%clofunc56679 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48697)
musttail call tailcc void %clofunc56679(%struct.ScmObj* %ae48697, %struct.ScmObj* %argslist56227$ae486972)
ret void
}

define tailcc void @proc_clo$ae48697(%struct.ScmObj* %env$ae48697,%struct.ScmObj* %current_45args55972) {
%stackaddr$env-ref56680 = alloca %struct.ScmObj*, align 8
%_37foldr147107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48697, i64 0)
store %struct.ScmObj* %_37foldr147107, %struct.ScmObj** %stackaddr$env-ref56680
%stackaddr$env-ref56681 = alloca %struct.ScmObj*, align 8
%_37foldl147091 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48697, i64 1)
store %struct.ScmObj* %_37foldl147091, %struct.ScmObj** %stackaddr$env-ref56681
%stackaddr$env-ref56682 = alloca %struct.ScmObj*, align 8
%_37map147138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48697, i64 2)
store %struct.ScmObj* %_37map147138, %struct.ScmObj** %stackaddr$env-ref56682
%stackaddr$env-ref56683 = alloca %struct.ScmObj*, align 8
%Ycmb47086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48697, i64 3)
store %struct.ScmObj* %Ycmb47086, %struct.ScmObj** %stackaddr$env-ref56683
%stackaddr$env-ref56684 = alloca %struct.ScmObj*, align 8
%_37foldr47112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48697, i64 4)
store %struct.ScmObj* %_37foldr47112, %struct.ScmObj** %stackaddr$env-ref56684
%stackaddr$prim56685 = alloca %struct.ScmObj*, align 8
%_95k47387 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55972)
store volatile %struct.ScmObj* %_95k47387, %struct.ScmObj** %stackaddr$prim56685, align 8
%stackaddr$prim56686 = alloca %struct.ScmObj*, align 8
%current_45args55973 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55972)
store volatile %struct.ScmObj* %current_45args55973, %struct.ScmObj** %stackaddr$prim56686, align 8
%stackaddr$prim56687 = alloca %struct.ScmObj*, align 8
%_37map47133 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55973)
store volatile %struct.ScmObj* %_37map47133, %struct.ScmObj** %stackaddr$prim56687, align 8
%stackaddr$makeclosure56688 = alloca %struct.ScmObj*, align 8
%fptrToInt56689 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48843 to i64
%ae48843 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56689)
store volatile %struct.ScmObj* %ae48843, %struct.ScmObj** %stackaddr$makeclosure56688, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48843, %struct.ScmObj* %_37foldl147091, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48843, %struct.ScmObj* %Ycmb47086, i64 1)
%ae48844 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56690 = alloca %struct.ScmObj*, align 8
%fptrToInt56691 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48845 to i64
%ae48845 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56691)
store volatile %struct.ScmObj* %ae48845, %struct.ScmObj** %stackaddr$makeclosure56690, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48845, %struct.ScmObj* %_37foldr147107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48845, %struct.ScmObj* %_37map147138, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48845, %struct.ScmObj* %_37foldr47112, i64 2)
%argslist56210$ae488430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56692 = alloca %struct.ScmObj*, align 8
%argslist56210$ae488431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48845, %struct.ScmObj* %argslist56210$ae488430)
store volatile %struct.ScmObj* %argslist56210$ae488431, %struct.ScmObj** %stackaddr$prim56692, align 8
%stackaddr$prim56693 = alloca %struct.ScmObj*, align 8
%argslist56210$ae488432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48844, %struct.ScmObj* %argslist56210$ae488431)
store volatile %struct.ScmObj* %argslist56210$ae488432, %struct.ScmObj** %stackaddr$prim56693, align 8
%clofunc56694 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48843)
musttail call tailcc void %clofunc56694(%struct.ScmObj* %ae48843, %struct.ScmObj* %argslist56210$ae488432)
ret void
}

define tailcc void @proc_clo$ae48843(%struct.ScmObj* %env$ae48843,%struct.ScmObj* %current_45args55975) {
%stackaddr$env-ref56695 = alloca %struct.ScmObj*, align 8
%_37foldl147091 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48843, i64 0)
store %struct.ScmObj* %_37foldl147091, %struct.ScmObj** %stackaddr$env-ref56695
%stackaddr$env-ref56696 = alloca %struct.ScmObj*, align 8
%Ycmb47086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48843, i64 1)
store %struct.ScmObj* %Ycmb47086, %struct.ScmObj** %stackaddr$env-ref56696
%stackaddr$prim56697 = alloca %struct.ScmObj*, align 8
%_95k47388 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55975)
store volatile %struct.ScmObj* %_95k47388, %struct.ScmObj** %stackaddr$prim56697, align 8
%stackaddr$prim56698 = alloca %struct.ScmObj*, align 8
%current_45args55976 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55975)
store volatile %struct.ScmObj* %current_45args55976, %struct.ScmObj** %stackaddr$prim56698, align 8
%stackaddr$prim56699 = alloca %struct.ScmObj*, align 8
%anf_45bind47307 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55976)
store volatile %struct.ScmObj* %anf_45bind47307, %struct.ScmObj** %stackaddr$prim56699, align 8
%stackaddr$makeclosure56700 = alloca %struct.ScmObj*, align 8
%fptrToInt56701 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49235 to i64
%ae49235 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56701)
store volatile %struct.ScmObj* %ae49235, %struct.ScmObj** %stackaddr$makeclosure56700, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49235, %struct.ScmObj* %_37foldl147091, i64 0)
%argslist56150$Ycmb470860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56702 = alloca %struct.ScmObj*, align 8
%argslist56150$Ycmb470861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47307, %struct.ScmObj* %argslist56150$Ycmb470860)
store volatile %struct.ScmObj* %argslist56150$Ycmb470861, %struct.ScmObj** %stackaddr$prim56702, align 8
%stackaddr$prim56703 = alloca %struct.ScmObj*, align 8
%argslist56150$Ycmb470862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49235, %struct.ScmObj* %argslist56150$Ycmb470861)
store volatile %struct.ScmObj* %argslist56150$Ycmb470862, %struct.ScmObj** %stackaddr$prim56703, align 8
%clofunc56704 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47086)
musttail call tailcc void %clofunc56704(%struct.ScmObj* %Ycmb47086, %struct.ScmObj* %argslist56150$Ycmb470862)
ret void
}

define tailcc void @proc_clo$ae49235(%struct.ScmObj* %env$ae49235,%struct.ScmObj* %current_45args55978) {
%stackaddr$env-ref56705 = alloca %struct.ScmObj*, align 8
%_37foldl147091 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49235, i64 0)
store %struct.ScmObj* %_37foldl147091, %struct.ScmObj** %stackaddr$env-ref56705
%stackaddr$prim56706 = alloca %struct.ScmObj*, align 8
%_95k47389 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55978)
store volatile %struct.ScmObj* %_95k47389, %struct.ScmObj** %stackaddr$prim56706, align 8
%stackaddr$prim56707 = alloca %struct.ScmObj*, align 8
%current_45args55979 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55978)
store volatile %struct.ScmObj* %current_45args55979, %struct.ScmObj** %stackaddr$prim56707, align 8
%stackaddr$prim56708 = alloca %struct.ScmObj*, align 8
%_37foldl47189 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55979)
store volatile %struct.ScmObj* %_37foldl47189, %struct.ScmObj** %stackaddr$prim56708, align 8
%stackaddr$makeclosure56709 = alloca %struct.ScmObj*, align 8
%fptrToInt56710 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49237 to i64
%ae49237 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56710)
store volatile %struct.ScmObj* %ae49237, %struct.ScmObj** %stackaddr$makeclosure56709, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49237, %struct.ScmObj* %_37foldl147091, i64 0)
%ae49238 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56711 = alloca %struct.ScmObj*, align 8
%fptrToInt56712 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49239 to i64
%ae49239 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56712)
store volatile %struct.ScmObj* %ae49239, %struct.ScmObj** %stackaddr$makeclosure56711, align 8
%argslist56149$ae492370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56713 = alloca %struct.ScmObj*, align 8
%argslist56149$ae492371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49239, %struct.ScmObj* %argslist56149$ae492370)
store volatile %struct.ScmObj* %argslist56149$ae492371, %struct.ScmObj** %stackaddr$prim56713, align 8
%stackaddr$prim56714 = alloca %struct.ScmObj*, align 8
%argslist56149$ae492372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49238, %struct.ScmObj* %argslist56149$ae492371)
store volatile %struct.ScmObj* %argslist56149$ae492372, %struct.ScmObj** %stackaddr$prim56714, align 8
%clofunc56715 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49237)
musttail call tailcc void %clofunc56715(%struct.ScmObj* %ae49237, %struct.ScmObj* %argslist56149$ae492372)
ret void
}

define tailcc void @proc_clo$ae49237(%struct.ScmObj* %env$ae49237,%struct.ScmObj* %current_45args55981) {
%stackaddr$env-ref56716 = alloca %struct.ScmObj*, align 8
%_37foldl147091 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49237, i64 0)
store %struct.ScmObj* %_37foldl147091, %struct.ScmObj** %stackaddr$env-ref56716
%stackaddr$prim56717 = alloca %struct.ScmObj*, align 8
%_95k47390 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55981)
store volatile %struct.ScmObj* %_95k47390, %struct.ScmObj** %stackaddr$prim56717, align 8
%stackaddr$prim56718 = alloca %struct.ScmObj*, align 8
%current_45args55982 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55981)
store volatile %struct.ScmObj* %current_45args55982, %struct.ScmObj** %stackaddr$prim56718, align 8
%stackaddr$prim56719 = alloca %struct.ScmObj*, align 8
%_37_6247186 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55982)
store volatile %struct.ScmObj* %_37_6247186, %struct.ScmObj** %stackaddr$prim56719, align 8
%stackaddr$makeclosure56720 = alloca %struct.ScmObj*, align 8
%fptrToInt56721 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49261 to i64
%ae49261 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56721)
store volatile %struct.ScmObj* %ae49261, %struct.ScmObj** %stackaddr$makeclosure56720, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49261, %struct.ScmObj* %_37foldl147091, i64 0)
%ae49262 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56722 = alloca %struct.ScmObj*, align 8
%fptrToInt56723 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49263 to i64
%ae49263 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56723)
store volatile %struct.ScmObj* %ae49263, %struct.ScmObj** %stackaddr$makeclosure56722, align 8
%argslist56143$ae492610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56724 = alloca %struct.ScmObj*, align 8
%argslist56143$ae492611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49263, %struct.ScmObj* %argslist56143$ae492610)
store volatile %struct.ScmObj* %argslist56143$ae492611, %struct.ScmObj** %stackaddr$prim56724, align 8
%stackaddr$prim56725 = alloca %struct.ScmObj*, align 8
%argslist56143$ae492612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49262, %struct.ScmObj* %argslist56143$ae492611)
store volatile %struct.ScmObj* %argslist56143$ae492612, %struct.ScmObj** %stackaddr$prim56725, align 8
%clofunc56726 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49261)
musttail call tailcc void %clofunc56726(%struct.ScmObj* %ae49261, %struct.ScmObj* %argslist56143$ae492612)
ret void
}

define tailcc void @proc_clo$ae49261(%struct.ScmObj* %env$ae49261,%struct.ScmObj* %current_45args55984) {
%stackaddr$env-ref56727 = alloca %struct.ScmObj*, align 8
%_37foldl147091 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49261, i64 0)
store %struct.ScmObj* %_37foldl147091, %struct.ScmObj** %stackaddr$env-ref56727
%stackaddr$prim56728 = alloca %struct.ScmObj*, align 8
%_95k47391 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55984)
store volatile %struct.ScmObj* %_95k47391, %struct.ScmObj** %stackaddr$prim56728, align 8
%stackaddr$prim56729 = alloca %struct.ScmObj*, align 8
%current_45args55985 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55984)
store volatile %struct.ScmObj* %current_45args55985, %struct.ScmObj** %stackaddr$prim56729, align 8
%stackaddr$prim56730 = alloca %struct.ScmObj*, align 8
%_37_62_6147183 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55985)
store volatile %struct.ScmObj* %_37_62_6147183, %struct.ScmObj** %stackaddr$prim56730, align 8
%ae49285 = call %struct.ScmObj* @const_init_int(i64 1)
%ae49286 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56731 = alloca %struct.ScmObj*, align 8
%_37append47179 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49285, %struct.ScmObj* %ae49286)
store volatile %struct.ScmObj* %_37append47179, %struct.ScmObj** %stackaddr$prim56731, align 8
%stackaddr$makeclosure56732 = alloca %struct.ScmObj*, align 8
%fptrToInt56733 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49287 to i64
%ae49287 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56733)
store volatile %struct.ScmObj* %ae49287, %struct.ScmObj** %stackaddr$makeclosure56732, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49287, %struct.ScmObj* %_37foldl147091, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49287, %struct.ScmObj* %_37append47179, i64 1)
%ae49288 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56734 = alloca %struct.ScmObj*, align 8
%fptrToInt56735 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49289 to i64
%ae49289 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56735)
store volatile %struct.ScmObj* %ae49289, %struct.ScmObj** %stackaddr$makeclosure56734, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49289, %struct.ScmObj* %_37append47179, i64 0)
%argslist56137$ae492870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56736 = alloca %struct.ScmObj*, align 8
%argslist56137$ae492871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49289, %struct.ScmObj* %argslist56137$ae492870)
store volatile %struct.ScmObj* %argslist56137$ae492871, %struct.ScmObj** %stackaddr$prim56736, align 8
%stackaddr$prim56737 = alloca %struct.ScmObj*, align 8
%argslist56137$ae492872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49288, %struct.ScmObj* %argslist56137$ae492871)
store volatile %struct.ScmObj* %argslist56137$ae492872, %struct.ScmObj** %stackaddr$prim56737, align 8
%clofunc56738 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49287)
musttail call tailcc void %clofunc56738(%struct.ScmObj* %ae49287, %struct.ScmObj* %argslist56137$ae492872)
ret void
}

define tailcc void @proc_clo$ae49287(%struct.ScmObj* %env$ae49287,%struct.ScmObj* %current_45args55987) {
%stackaddr$env-ref56739 = alloca %struct.ScmObj*, align 8
%_37foldl147091 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49287, i64 0)
store %struct.ScmObj* %_37foldl147091, %struct.ScmObj** %stackaddr$env-ref56739
%stackaddr$env-ref56740 = alloca %struct.ScmObj*, align 8
%_37append47179 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49287, i64 1)
store %struct.ScmObj* %_37append47179, %struct.ScmObj** %stackaddr$env-ref56740
%stackaddr$prim56741 = alloca %struct.ScmObj*, align 8
%_95k47392 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55987)
store volatile %struct.ScmObj* %_95k47392, %struct.ScmObj** %stackaddr$prim56741, align 8
%stackaddr$prim56742 = alloca %struct.ScmObj*, align 8
%current_45args55988 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55987)
store volatile %struct.ScmObj* %current_45args55988, %struct.ScmObj** %stackaddr$prim56742, align 8
%stackaddr$prim56743 = alloca %struct.ScmObj*, align 8
%anf_45bind47315 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55988)
store volatile %struct.ScmObj* %anf_45bind47315, %struct.ScmObj** %stackaddr$prim56743, align 8
%ae49355 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56744 = alloca %struct.ScmObj*, align 8
%_95047180 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append47179, %struct.ScmObj* %ae49355, %struct.ScmObj* %anf_45bind47315)
store volatile %struct.ScmObj* %_95047180, %struct.ScmObj** %stackaddr$prim56744, align 8
%ae49358 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56745 = alloca %struct.ScmObj*, align 8
%_37append47178 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append47179, %struct.ScmObj* %ae49358)
store volatile %struct.ScmObj* %_37append47178, %struct.ScmObj** %stackaddr$prim56745, align 8
%stackaddr$makeclosure56746 = alloca %struct.ScmObj*, align 8
%fptrToInt56747 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49359 to i64
%ae49359 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56747)
store volatile %struct.ScmObj* %ae49359, %struct.ScmObj** %stackaddr$makeclosure56746, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49359, %struct.ScmObj* %_37foldl147091, i64 0)
%ae49360 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56748 = alloca %struct.ScmObj*, align 8
%fptrToInt56749 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49361 to i64
%ae49361 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56749)
store volatile %struct.ScmObj* %ae49361, %struct.ScmObj** %stackaddr$makeclosure56748, align 8
%argslist56126$ae493590 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56750 = alloca %struct.ScmObj*, align 8
%argslist56126$ae493591 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49361, %struct.ScmObj* %argslist56126$ae493590)
store volatile %struct.ScmObj* %argslist56126$ae493591, %struct.ScmObj** %stackaddr$prim56750, align 8
%stackaddr$prim56751 = alloca %struct.ScmObj*, align 8
%argslist56126$ae493592 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49360, %struct.ScmObj* %argslist56126$ae493591)
store volatile %struct.ScmObj* %argslist56126$ae493592, %struct.ScmObj** %stackaddr$prim56751, align 8
%clofunc56752 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49359)
musttail call tailcc void %clofunc56752(%struct.ScmObj* %ae49359, %struct.ScmObj* %argslist56126$ae493592)
ret void
}

define tailcc void @proc_clo$ae49359(%struct.ScmObj* %env$ae49359,%struct.ScmObj* %current_45args55990) {
%stackaddr$env-ref56753 = alloca %struct.ScmObj*, align 8
%_37foldl147091 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49359, i64 0)
store %struct.ScmObj* %_37foldl147091, %struct.ScmObj** %stackaddr$env-ref56753
%stackaddr$prim56754 = alloca %struct.ScmObj*, align 8
%_95k47393 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55990)
store volatile %struct.ScmObj* %_95k47393, %struct.ScmObj** %stackaddr$prim56754, align 8
%stackaddr$prim56755 = alloca %struct.ScmObj*, align 8
%current_45args55991 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55990)
store volatile %struct.ScmObj* %current_45args55991, %struct.ScmObj** %stackaddr$prim56755, align 8
%stackaddr$prim56756 = alloca %struct.ScmObj*, align 8
%_37list_6347171 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55991)
store volatile %struct.ScmObj* %_37list_6347171, %struct.ScmObj** %stackaddr$prim56756, align 8
%stackaddr$makeclosure56757 = alloca %struct.ScmObj*, align 8
%fptrToInt56758 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49775 to i64
%ae49775 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56758)
store volatile %struct.ScmObj* %ae49775, %struct.ScmObj** %stackaddr$makeclosure56757, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49775, %struct.ScmObj* %_37foldl147091, i64 0)
%ae49776 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56759 = alloca %struct.ScmObj*, align 8
%fptrToInt56760 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49777 to i64
%ae49777 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56760)
store volatile %struct.ScmObj* %ae49777, %struct.ScmObj** %stackaddr$makeclosure56759, align 8
%argslist56101$ae497750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56761 = alloca %struct.ScmObj*, align 8
%argslist56101$ae497751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49777, %struct.ScmObj* %argslist56101$ae497750)
store volatile %struct.ScmObj* %argslist56101$ae497751, %struct.ScmObj** %stackaddr$prim56761, align 8
%stackaddr$prim56762 = alloca %struct.ScmObj*, align 8
%argslist56101$ae497752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49776, %struct.ScmObj* %argslist56101$ae497751)
store volatile %struct.ScmObj* %argslist56101$ae497752, %struct.ScmObj** %stackaddr$prim56762, align 8
%clofunc56763 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49775)
musttail call tailcc void %clofunc56763(%struct.ScmObj* %ae49775, %struct.ScmObj* %argslist56101$ae497752)
ret void
}

define tailcc void @proc_clo$ae49775(%struct.ScmObj* %env$ae49775,%struct.ScmObj* %current_45args55993) {
%stackaddr$env-ref56764 = alloca %struct.ScmObj*, align 8
%_37foldl147091 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49775, i64 0)
store %struct.ScmObj* %_37foldl147091, %struct.ScmObj** %stackaddr$env-ref56764
%stackaddr$prim56765 = alloca %struct.ScmObj*, align 8
%_95k47394 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55993)
store volatile %struct.ScmObj* %_95k47394, %struct.ScmObj** %stackaddr$prim56765, align 8
%stackaddr$prim56766 = alloca %struct.ScmObj*, align 8
%current_45args55994 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55993)
store volatile %struct.ScmObj* %current_45args55994, %struct.ScmObj** %stackaddr$prim56766, align 8
%stackaddr$prim56767 = alloca %struct.ScmObj*, align 8
%_37drop47162 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55994)
store volatile %struct.ScmObj* %_37drop47162, %struct.ScmObj** %stackaddr$prim56767, align 8
%stackaddr$makeclosure56768 = alloca %struct.ScmObj*, align 8
%fptrToInt56769 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50311 to i64
%ae50311 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56769)
store volatile %struct.ScmObj* %ae50311, %struct.ScmObj** %stackaddr$makeclosure56768, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50311, %struct.ScmObj* %_37foldl147091, i64 0)
%ae50312 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56770 = alloca %struct.ScmObj*, align 8
%fptrToInt56771 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50313 to i64
%ae50313 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56771)
store volatile %struct.ScmObj* %ae50313, %struct.ScmObj** %stackaddr$makeclosure56770, align 8
%argslist56077$ae503110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56772 = alloca %struct.ScmObj*, align 8
%argslist56077$ae503111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50313, %struct.ScmObj* %argslist56077$ae503110)
store volatile %struct.ScmObj* %argslist56077$ae503111, %struct.ScmObj** %stackaddr$prim56772, align 8
%stackaddr$prim56773 = alloca %struct.ScmObj*, align 8
%argslist56077$ae503112 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50312, %struct.ScmObj* %argslist56077$ae503111)
store volatile %struct.ScmObj* %argslist56077$ae503112, %struct.ScmObj** %stackaddr$prim56773, align 8
%clofunc56774 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50311)
musttail call tailcc void %clofunc56774(%struct.ScmObj* %ae50311, %struct.ScmObj* %argslist56077$ae503112)
ret void
}

define tailcc void @proc_clo$ae50311(%struct.ScmObj* %env$ae50311,%struct.ScmObj* %current_45args55996) {
%stackaddr$env-ref56775 = alloca %struct.ScmObj*, align 8
%_37foldl147091 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50311, i64 0)
store %struct.ScmObj* %_37foldl147091, %struct.ScmObj** %stackaddr$env-ref56775
%stackaddr$prim56776 = alloca %struct.ScmObj*, align 8
%_95k47395 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55996)
store volatile %struct.ScmObj* %_95k47395, %struct.ScmObj** %stackaddr$prim56776, align 8
%stackaddr$prim56777 = alloca %struct.ScmObj*, align 8
%current_45args55997 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55996)
store volatile %struct.ScmObj* %current_45args55997, %struct.ScmObj** %stackaddr$prim56777, align 8
%stackaddr$prim56778 = alloca %struct.ScmObj*, align 8
%_37memv47155 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55997)
store volatile %struct.ScmObj* %_37memv47155, %struct.ScmObj** %stackaddr$prim56778, align 8
%stackaddr$makeclosure56779 = alloca %struct.ScmObj*, align 8
%fptrToInt56780 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50715 to i64
%ae50715 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56780)
store volatile %struct.ScmObj* %ae50715, %struct.ScmObj** %stackaddr$makeclosure56779, align 8
%ae50716 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56781 = alloca %struct.ScmObj*, align 8
%fptrToInt56782 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50717 to i64
%ae50717 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56782)
store volatile %struct.ScmObj* %ae50717, %struct.ScmObj** %stackaddr$makeclosure56781, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50717, %struct.ScmObj* %_37foldl147091, i64 0)
%argslist56051$ae507150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56783 = alloca %struct.ScmObj*, align 8
%argslist56051$ae507151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50717, %struct.ScmObj* %argslist56051$ae507150)
store volatile %struct.ScmObj* %argslist56051$ae507151, %struct.ScmObj** %stackaddr$prim56783, align 8
%stackaddr$prim56784 = alloca %struct.ScmObj*, align 8
%argslist56051$ae507152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50716, %struct.ScmObj* %argslist56051$ae507151)
store volatile %struct.ScmObj* %argslist56051$ae507152, %struct.ScmObj** %stackaddr$prim56784, align 8
%clofunc56785 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50715)
musttail call tailcc void %clofunc56785(%struct.ScmObj* %ae50715, %struct.ScmObj* %argslist56051$ae507152)
ret void
}

define tailcc void @proc_clo$ae50715(%struct.ScmObj* %env$ae50715,%struct.ScmObj* %current_45args55999) {
%stackaddr$prim56786 = alloca %struct.ScmObj*, align 8
%_95k47396 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55999)
store volatile %struct.ScmObj* %_95k47396, %struct.ScmObj** %stackaddr$prim56786, align 8
%stackaddr$prim56787 = alloca %struct.ScmObj*, align 8
%current_45args56000 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55999)
store volatile %struct.ScmObj* %current_45args56000, %struct.ScmObj** %stackaddr$prim56787, align 8
%stackaddr$prim56788 = alloca %struct.ScmObj*, align 8
%_37_4747151 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56000)
store volatile %struct.ScmObj* %_37_4747151, %struct.ScmObj** %stackaddr$prim56788, align 8
%stackaddr$makeclosure56789 = alloca %struct.ScmObj*, align 8
%fptrToInt56790 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50813 to i64
%ae50813 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56790)
store volatile %struct.ScmObj* %ae50813, %struct.ScmObj** %stackaddr$makeclosure56789, align 8
%ae50814 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56791 = alloca %struct.ScmObj*, align 8
%fptrToInt56792 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50815 to i64
%ae50815 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56792)
store volatile %struct.ScmObj* %ae50815, %struct.ScmObj** %stackaddr$makeclosure56791, align 8
%argslist56038$ae508130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56793 = alloca %struct.ScmObj*, align 8
%argslist56038$ae508131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50815, %struct.ScmObj* %argslist56038$ae508130)
store volatile %struct.ScmObj* %argslist56038$ae508131, %struct.ScmObj** %stackaddr$prim56793, align 8
%stackaddr$prim56794 = alloca %struct.ScmObj*, align 8
%argslist56038$ae508132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50814, %struct.ScmObj* %argslist56038$ae508131)
store volatile %struct.ScmObj* %argslist56038$ae508132, %struct.ScmObj** %stackaddr$prim56794, align 8
%clofunc56795 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50813)
musttail call tailcc void %clofunc56795(%struct.ScmObj* %ae50813, %struct.ScmObj* %argslist56038$ae508132)
ret void
}

define tailcc void @proc_clo$ae50813(%struct.ScmObj* %env$ae50813,%struct.ScmObj* %current_45args56002) {
%stackaddr$prim56796 = alloca %struct.ScmObj*, align 8
%_95k47397 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56002)
store volatile %struct.ScmObj* %_95k47397, %struct.ScmObj** %stackaddr$prim56796, align 8
%stackaddr$prim56797 = alloca %struct.ScmObj*, align 8
%current_45args56003 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56002)
store volatile %struct.ScmObj* %current_45args56003, %struct.ScmObj** %stackaddr$prim56797, align 8
%stackaddr$prim56798 = alloca %struct.ScmObj*, align 8
%_37first47149 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56003)
store volatile %struct.ScmObj* %_37first47149, %struct.ScmObj** %stackaddr$prim56798, align 8
%stackaddr$makeclosure56799 = alloca %struct.ScmObj*, align 8
%fptrToInt56800 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50833 to i64
%ae50833 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56800)
store volatile %struct.ScmObj* %ae50833, %struct.ScmObj** %stackaddr$makeclosure56799, align 8
%ae50834 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56801 = alloca %struct.ScmObj*, align 8
%fptrToInt56802 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50835 to i64
%ae50835 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56802)
store volatile %struct.ScmObj* %ae50835, %struct.ScmObj** %stackaddr$makeclosure56801, align 8
%argslist56033$ae508330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56803 = alloca %struct.ScmObj*, align 8
%argslist56033$ae508331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50835, %struct.ScmObj* %argslist56033$ae508330)
store volatile %struct.ScmObj* %argslist56033$ae508331, %struct.ScmObj** %stackaddr$prim56803, align 8
%stackaddr$prim56804 = alloca %struct.ScmObj*, align 8
%argslist56033$ae508332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50834, %struct.ScmObj* %argslist56033$ae508331)
store volatile %struct.ScmObj* %argslist56033$ae508332, %struct.ScmObj** %stackaddr$prim56804, align 8
%clofunc56805 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50833)
musttail call tailcc void %clofunc56805(%struct.ScmObj* %ae50833, %struct.ScmObj* %argslist56033$ae508332)
ret void
}

define tailcc void @proc_clo$ae50833(%struct.ScmObj* %env$ae50833,%struct.ScmObj* %current_45args56005) {
%stackaddr$prim56806 = alloca %struct.ScmObj*, align 8
%_95k47398 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56005)
store volatile %struct.ScmObj* %_95k47398, %struct.ScmObj** %stackaddr$prim56806, align 8
%stackaddr$prim56807 = alloca %struct.ScmObj*, align 8
%current_45args56006 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56005)
store volatile %struct.ScmObj* %current_45args56006, %struct.ScmObj** %stackaddr$prim56807, align 8
%stackaddr$prim56808 = alloca %struct.ScmObj*, align 8
%_37second47147 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56006)
store volatile %struct.ScmObj* %_37second47147, %struct.ScmObj** %stackaddr$prim56808, align 8
%stackaddr$makeclosure56809 = alloca %struct.ScmObj*, align 8
%fptrToInt56810 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50855 to i64
%ae50855 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56810)
store volatile %struct.ScmObj* %ae50855, %struct.ScmObj** %stackaddr$makeclosure56809, align 8
%ae50856 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56811 = alloca %struct.ScmObj*, align 8
%fptrToInt56812 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50857 to i64
%ae50857 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56812)
store volatile %struct.ScmObj* %ae50857, %struct.ScmObj** %stackaddr$makeclosure56811, align 8
%argslist56028$ae508550 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56813 = alloca %struct.ScmObj*, align 8
%argslist56028$ae508551 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50857, %struct.ScmObj* %argslist56028$ae508550)
store volatile %struct.ScmObj* %argslist56028$ae508551, %struct.ScmObj** %stackaddr$prim56813, align 8
%stackaddr$prim56814 = alloca %struct.ScmObj*, align 8
%argslist56028$ae508552 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50856, %struct.ScmObj* %argslist56028$ae508551)
store volatile %struct.ScmObj* %argslist56028$ae508552, %struct.ScmObj** %stackaddr$prim56814, align 8
%clofunc56815 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50855)
musttail call tailcc void %clofunc56815(%struct.ScmObj* %ae50855, %struct.ScmObj* %argslist56028$ae508552)
ret void
}

define tailcc void @proc_clo$ae50855(%struct.ScmObj* %env$ae50855,%struct.ScmObj* %current_45args56008) {
%stackaddr$prim56816 = alloca %struct.ScmObj*, align 8
%_95k47399 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56008)
store volatile %struct.ScmObj* %_95k47399, %struct.ScmObj** %stackaddr$prim56816, align 8
%stackaddr$prim56817 = alloca %struct.ScmObj*, align 8
%current_45args56009 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56008)
store volatile %struct.ScmObj* %current_45args56009, %struct.ScmObj** %stackaddr$prim56817, align 8
%stackaddr$prim56818 = alloca %struct.ScmObj*, align 8
%_37third47145 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56009)
store volatile %struct.ScmObj* %_37third47145, %struct.ScmObj** %stackaddr$prim56818, align 8
%stackaddr$makeclosure56819 = alloca %struct.ScmObj*, align 8
%fptrToInt56820 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50879 to i64
%ae50879 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56820)
store volatile %struct.ScmObj* %ae50879, %struct.ScmObj** %stackaddr$makeclosure56819, align 8
%ae50880 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56821 = alloca %struct.ScmObj*, align 8
%fptrToInt56822 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50881 to i64
%ae50881 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56822)
store volatile %struct.ScmObj* %ae50881, %struct.ScmObj** %stackaddr$makeclosure56821, align 8
%argslist56023$ae508790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56823 = alloca %struct.ScmObj*, align 8
%argslist56023$ae508791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50881, %struct.ScmObj* %argslist56023$ae508790)
store volatile %struct.ScmObj* %argslist56023$ae508791, %struct.ScmObj** %stackaddr$prim56823, align 8
%stackaddr$prim56824 = alloca %struct.ScmObj*, align 8
%argslist56023$ae508792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50880, %struct.ScmObj* %argslist56023$ae508791)
store volatile %struct.ScmObj* %argslist56023$ae508792, %struct.ScmObj** %stackaddr$prim56824, align 8
%clofunc56825 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50879)
musttail call tailcc void %clofunc56825(%struct.ScmObj* %ae50879, %struct.ScmObj* %argslist56023$ae508792)
ret void
}

define tailcc void @proc_clo$ae50879(%struct.ScmObj* %env$ae50879,%struct.ScmObj* %current_45args56011) {
%stackaddr$prim56826 = alloca %struct.ScmObj*, align 8
%_95k47400 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56011)
store volatile %struct.ScmObj* %_95k47400, %struct.ScmObj** %stackaddr$prim56826, align 8
%stackaddr$prim56827 = alloca %struct.ScmObj*, align 8
%current_45args56012 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56011)
store volatile %struct.ScmObj* %current_45args56012, %struct.ScmObj** %stackaddr$prim56827, align 8
%stackaddr$prim56828 = alloca %struct.ScmObj*, align 8
%_37fourth47143 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56012)
store volatile %struct.ScmObj* %_37fourth47143, %struct.ScmObj** %stackaddr$prim56828, align 8
%stackaddr$prim56829 = alloca %struct.ScmObj*, align 8
%anf_45bind47351 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind47351, %struct.ScmObj** %stackaddr$prim56829, align 8
%ae50905 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56830 = alloca %struct.ScmObj*, align 8
%g4147221 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50905, %struct.ScmObj* %anf_45bind47351)
store volatile %struct.ScmObj* %g4147221, %struct.ScmObj** %stackaddr$prim56830, align 8
%stackaddr$prim56831 = alloca %struct.ScmObj*, align 8
%anf_45bind47352 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind47352, %struct.ScmObj** %stackaddr$prim56831, align 8
%ae50907 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56832 = alloca %struct.ScmObj*, align 8
%g4247220 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50907, %struct.ScmObj* %anf_45bind47352)
store volatile %struct.ScmObj* %g4247220, %struct.ScmObj** %stackaddr$prim56832, align 8
%stackaddr$prim56833 = alloca %struct.ScmObj*, align 8
%anf_45bind47353 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind47353, %struct.ScmObj** %stackaddr$prim56833, align 8
%ae50909 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56834 = alloca %struct.ScmObj*, align 8
%g4347219 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50909, %struct.ScmObj* %anf_45bind47353)
store volatile %struct.ScmObj* %g4347219, %struct.ScmObj** %stackaddr$prim56834, align 8
%stackaddr$prim56835 = alloca %struct.ScmObj*, align 8
%anf_45bind47354 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind47354, %struct.ScmObj** %stackaddr$prim56835, align 8
%ae50911 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56836 = alloca %struct.ScmObj*, align 8
%g4447218 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50911, %struct.ScmObj* %anf_45bind47354)
store volatile %struct.ScmObj* %g4447218, %struct.ScmObj** %stackaddr$prim56836, align 8
%stackaddr$prim56837 = alloca %struct.ScmObj*, align 8
%anf_45bind47355 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind47355, %struct.ScmObj** %stackaddr$prim56837, align 8
%ae50913 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56838 = alloca %struct.ScmObj*, align 8
%g4547217 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50913, %struct.ScmObj* %anf_45bind47355)
store volatile %struct.ScmObj* %g4547217, %struct.ScmObj** %stackaddr$prim56838, align 8
%stackaddr$prim56839 = alloca %struct.ScmObj*, align 8
%anf_45bind47356 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind47356, %struct.ScmObj** %stackaddr$prim56839, align 8
%ae50915 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56840 = alloca %struct.ScmObj*, align 8
%g4647216 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50915, %struct.ScmObj* %anf_45bind47356)
store volatile %struct.ScmObj* %g4647216, %struct.ScmObj** %stackaddr$prim56840, align 8
%stackaddr$prim56841 = alloca %struct.ScmObj*, align 8
%anf_45bind47357 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind47357, %struct.ScmObj** %stackaddr$prim56841, align 8
%ae50917 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56842 = alloca %struct.ScmObj*, align 8
%g4747215 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50917, %struct.ScmObj* %anf_45bind47357)
store volatile %struct.ScmObj* %g4747215, %struct.ScmObj** %stackaddr$prim56842, align 8
%stackaddr$prim56843 = alloca %struct.ScmObj*, align 8
%anf_45bind47358 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind47358, %struct.ScmObj** %stackaddr$prim56843, align 8
%ae50919 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56844 = alloca %struct.ScmObj*, align 8
%g4847214 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50919, %struct.ScmObj* %anf_45bind47358)
store volatile %struct.ScmObj* %g4847214, %struct.ScmObj** %stackaddr$prim56844, align 8
%stackaddr$prim56845 = alloca %struct.ScmObj*, align 8
%anf_45bind47359 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind47359, %struct.ScmObj** %stackaddr$prim56845, align 8
%ae50921 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56846 = alloca %struct.ScmObj*, align 8
%g4947213 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50921, %struct.ScmObj* %anf_45bind47359)
store volatile %struct.ScmObj* %g4947213, %struct.ScmObj** %stackaddr$prim56846, align 8
%stackaddr$prim56847 = alloca %struct.ScmObj*, align 8
%anf_45bind47360 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind47360, %struct.ScmObj** %stackaddr$prim56847, align 8
%ae50923 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56848 = alloca %struct.ScmObj*, align 8
%g5047212 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50923, %struct.ScmObj* %anf_45bind47360)
store volatile %struct.ScmObj* %g5047212, %struct.ScmObj** %stackaddr$prim56848, align 8
%stackaddr$prim56849 = alloca %struct.ScmObj*, align 8
%anf_45bind47361 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind47361, %struct.ScmObj** %stackaddr$prim56849, align 8
%ae50925 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56850 = alloca %struct.ScmObj*, align 8
%g5147211 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50925, %struct.ScmObj* %anf_45bind47361)
store volatile %struct.ScmObj* %g5147211, %struct.ScmObj** %stackaddr$prim56850, align 8
%stackaddr$prim56851 = alloca %struct.ScmObj*, align 8
%anf_45bind47362 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind47362, %struct.ScmObj** %stackaddr$prim56851, align 8
%ae50927 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56852 = alloca %struct.ScmObj*, align 8
%g5247210 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50927, %struct.ScmObj* %anf_45bind47362)
store volatile %struct.ScmObj* %g5247210, %struct.ScmObj** %stackaddr$prim56852, align 8
%stackaddr$prim56853 = alloca %struct.ScmObj*, align 8
%anf_45bind47363 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind47363, %struct.ScmObj** %stackaddr$prim56853, align 8
%ae50929 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56854 = alloca %struct.ScmObj*, align 8
%g5347209 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50929, %struct.ScmObj* %anf_45bind47363)
store volatile %struct.ScmObj* %g5347209, %struct.ScmObj** %stackaddr$prim56854, align 8
%stackaddr$prim56855 = alloca %struct.ScmObj*, align 8
%anf_45bind47364 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind47364, %struct.ScmObj** %stackaddr$prim56855, align 8
%ae50931 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56856 = alloca %struct.ScmObj*, align 8
%g5447208 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50931, %struct.ScmObj* %anf_45bind47364)
store volatile %struct.ScmObj* %g5447208, %struct.ScmObj** %stackaddr$prim56856, align 8
%stackaddr$prim56857 = alloca %struct.ScmObj*, align 8
%anf_45bind47365 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind47365, %struct.ScmObj** %stackaddr$prim56857, align 8
%ae50933 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56858 = alloca %struct.ScmObj*, align 8
%g5547207 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50933, %struct.ScmObj* %anf_45bind47365)
store volatile %struct.ScmObj* %g5547207, %struct.ScmObj** %stackaddr$prim56858, align 8
%stackaddr$prim56859 = alloca %struct.ScmObj*, align 8
%anf_45bind47366 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind47366, %struct.ScmObj** %stackaddr$prim56859, align 8
%ae50935 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56860 = alloca %struct.ScmObj*, align 8
%g5647206 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50935, %struct.ScmObj* %anf_45bind47366)
store volatile %struct.ScmObj* %g5647206, %struct.ScmObj** %stackaddr$prim56860, align 8
%stackaddr$prim56861 = alloca %struct.ScmObj*, align 8
%anf_45bind47367 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind47367, %struct.ScmObj** %stackaddr$prim56861, align 8
%ae50937 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56862 = alloca %struct.ScmObj*, align 8
%g5747205 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50937, %struct.ScmObj* %anf_45bind47367)
store volatile %struct.ScmObj* %g5747205, %struct.ScmObj** %stackaddr$prim56862, align 8
%stackaddr$prim56863 = alloca %struct.ScmObj*, align 8
%anf_45bind47368 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind47368, %struct.ScmObj** %stackaddr$prim56863, align 8
%ae50939 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56864 = alloca %struct.ScmObj*, align 8
%g5847204 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50939, %struct.ScmObj* %anf_45bind47368)
store volatile %struct.ScmObj* %g5847204, %struct.ScmObj** %stackaddr$prim56864, align 8
%ae50942 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50943 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae5094356865, i32 0, i32 0))
%stackaddr$prim56866 = alloca %struct.ScmObj*, align 8
%t4708547239 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g4147221, %struct.ScmObj* %ae50942, %struct.ScmObj* %ae50943)
store volatile %struct.ScmObj* %t4708547239, %struct.ScmObj** %stackaddr$prim56866, align 8
%ae50945 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50946 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae5094656867, i32 0, i32 0))
%stackaddr$prim56868 = alloca %struct.ScmObj*, align 8
%t4708447238 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g4247220, %struct.ScmObj* %ae50945, %struct.ScmObj* %ae50946)
store volatile %struct.ScmObj* %t4708447238, %struct.ScmObj** %stackaddr$prim56868, align 8
%ae50948 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50949 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae5094956869, i32 0, i32 0))
%stackaddr$prim56870 = alloca %struct.ScmObj*, align 8
%t4708347237 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g4347219, %struct.ScmObj* %ae50948, %struct.ScmObj* %ae50949)
store volatile %struct.ScmObj* %t4708347237, %struct.ScmObj** %stackaddr$prim56870, align 8
%ae50951 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50952 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae5095256871, i32 0, i32 0))
%stackaddr$prim56872 = alloca %struct.ScmObj*, align 8
%t4708247236 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g4447218, %struct.ScmObj* %ae50951, %struct.ScmObj* %ae50952)
store volatile %struct.ScmObj* %t4708247236, %struct.ScmObj** %stackaddr$prim56872, align 8
%ae50954 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50955 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae5095556873, i32 0, i32 0))
%stackaddr$prim56874 = alloca %struct.ScmObj*, align 8
%t4708147235 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g4547217, %struct.ScmObj* %ae50954, %struct.ScmObj* %ae50955)
store volatile %struct.ScmObj* %t4708147235, %struct.ScmObj** %stackaddr$prim56874, align 8
%ae50957 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50958 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae5095856875, i32 0, i32 0))
%stackaddr$prim56876 = alloca %struct.ScmObj*, align 8
%t4708047234 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g4647216, %struct.ScmObj* %ae50957, %struct.ScmObj* %ae50958)
store volatile %struct.ScmObj* %t4708047234, %struct.ScmObj** %stackaddr$prim56876, align 8
%ae50960 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50961 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae5096156877, i32 0, i32 0))
%stackaddr$prim56878 = alloca %struct.ScmObj*, align 8
%t4707947233 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g4747215, %struct.ScmObj* %ae50960, %struct.ScmObj* %ae50961)
store volatile %struct.ScmObj* %t4707947233, %struct.ScmObj** %stackaddr$prim56878, align 8
%ae50963 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50964 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae5096456879, i32 0, i32 0))
%stackaddr$prim56880 = alloca %struct.ScmObj*, align 8
%t4707847232 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g4847214, %struct.ScmObj* %ae50963, %struct.ScmObj* %ae50964)
store volatile %struct.ScmObj* %t4707847232, %struct.ScmObj** %stackaddr$prim56880, align 8
%ae50966 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50967 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae5096756881, i32 0, i32 0))
%stackaddr$prim56882 = alloca %struct.ScmObj*, align 8
%t4707747231 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g4947213, %struct.ScmObj* %ae50966, %struct.ScmObj* %ae50967)
store volatile %struct.ScmObj* %t4707747231, %struct.ScmObj** %stackaddr$prim56882, align 8
%ae50969 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50970 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae5097056883, i32 0, i32 0))
%stackaddr$prim56884 = alloca %struct.ScmObj*, align 8
%t4707647230 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g5047212, %struct.ScmObj* %ae50969, %struct.ScmObj* %ae50970)
store volatile %struct.ScmObj* %t4707647230, %struct.ScmObj** %stackaddr$prim56884, align 8
%ae50972 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50973 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae5097356885, i32 0, i32 0))
%stackaddr$prim56886 = alloca %struct.ScmObj*, align 8
%t4707547229 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g5147211, %struct.ScmObj* %ae50972, %struct.ScmObj* %ae50973)
store volatile %struct.ScmObj* %t4707547229, %struct.ScmObj** %stackaddr$prim56886, align 8
%ae50975 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50976 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae5097656887, i32 0, i32 0))
%stackaddr$prim56888 = alloca %struct.ScmObj*, align 8
%t4707447228 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g5247210, %struct.ScmObj* %ae50975, %struct.ScmObj* %ae50976)
store volatile %struct.ScmObj* %t4707447228, %struct.ScmObj** %stackaddr$prim56888, align 8
%ae50978 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50979 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae5097956889, i32 0, i32 0))
%stackaddr$prim56890 = alloca %struct.ScmObj*, align 8
%t4707347227 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g5347209, %struct.ScmObj* %ae50978, %struct.ScmObj* %ae50979)
store volatile %struct.ScmObj* %t4707347227, %struct.ScmObj** %stackaddr$prim56890, align 8
%ae50981 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50982 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae5098256891, i32 0, i32 0))
%stackaddr$prim56892 = alloca %struct.ScmObj*, align 8
%t4707247226 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g5447208, %struct.ScmObj* %ae50981, %struct.ScmObj* %ae50982)
store volatile %struct.ScmObj* %t4707247226, %struct.ScmObj** %stackaddr$prim56892, align 8
%ae50984 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50985 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae5098556893, i32 0, i32 0))
%stackaddr$prim56894 = alloca %struct.ScmObj*, align 8
%t4707147225 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g5547207, %struct.ScmObj* %ae50984, %struct.ScmObj* %ae50985)
store volatile %struct.ScmObj* %t4707147225, %struct.ScmObj** %stackaddr$prim56894, align 8
%ae50987 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50988 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae5098856895, i32 0, i32 0))
%stackaddr$prim56896 = alloca %struct.ScmObj*, align 8
%t4707047224 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g5647206, %struct.ScmObj* %ae50987, %struct.ScmObj* %ae50988)
store volatile %struct.ScmObj* %t4707047224, %struct.ScmObj** %stackaddr$prim56896, align 8
%ae50990 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50991 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae5099156897, i32 0, i32 0))
%stackaddr$prim56898 = alloca %struct.ScmObj*, align 8
%t4706947223 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g5747205, %struct.ScmObj* %ae50990, %struct.ScmObj* %ae50991)
store volatile %struct.ScmObj* %t4706947223, %struct.ScmObj** %stackaddr$prim56898, align 8
%ae50993 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50994 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae5099456899, i32 0, i32 0))
%stackaddr$prim56900 = alloca %struct.ScmObj*, align 8
%t4706847222 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g5847204, %struct.ScmObj* %ae50993, %struct.ScmObj* %ae50994)
store volatile %struct.ScmObj* %t4706847222, %struct.ScmObj** %stackaddr$prim56900, align 8
%stackaddr$makeclosure56901 = alloca %struct.ScmObj*, align 8
%fptrToInt56902 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50995 to i64
%ae50995 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56902)
store volatile %struct.ScmObj* %ae50995, %struct.ScmObj** %stackaddr$makeclosure56901, align 8
%ae50996 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50997 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @global$sym$ae5099756903, i32 0, i32 0))
%argslist56018$ae509950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56904 = alloca %struct.ScmObj*, align 8
%argslist56018$ae509951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50997, %struct.ScmObj* %argslist56018$ae509950)
store volatile %struct.ScmObj* %argslist56018$ae509951, %struct.ScmObj** %stackaddr$prim56904, align 8
%stackaddr$prim56905 = alloca %struct.ScmObj*, align 8
%argslist56018$ae509952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50996, %struct.ScmObj* %argslist56018$ae509951)
store volatile %struct.ScmObj* %argslist56018$ae509952, %struct.ScmObj** %stackaddr$prim56905, align 8
%clofunc56906 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50995)
musttail call tailcc void %clofunc56906(%struct.ScmObj* %ae50995, %struct.ScmObj* %argslist56018$ae509952)
ret void
}

define tailcc void @proc_clo$ae50995(%struct.ScmObj* %env$ae50995,%struct.ScmObj* %current_45args56014) {
%stackaddr$prim56907 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56014)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim56907, align 8
%stackaddr$prim56908 = alloca %struct.ScmObj*, align 8
%current_45args56015 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56014)
store volatile %struct.ScmObj* %current_45args56015, %struct.ScmObj** %stackaddr$prim56908, align 8
%stackaddr$prim56909 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56015)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim56909, align 8
%stackaddr$prim56910 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim56910, align 8
%argslist56017$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56911 = alloca %struct.ScmObj*, align 8
%argslist56017$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist56017$k0)
store volatile %struct.ScmObj* %argslist56017$k1, %struct.ScmObj** %stackaddr$prim56911, align 8
%clofunc56912 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc56912(%struct.ScmObj* %k, %struct.ScmObj* %argslist56017$k1)
ret void
}

define tailcc void @proc_clo$ae50881(%struct.ScmObj* %env$ae50881,%struct.ScmObj* %current_45args56019) {
%stackaddr$prim56913 = alloca %struct.ScmObj*, align 8
%k47401 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56019)
store volatile %struct.ScmObj* %k47401, %struct.ScmObj** %stackaddr$prim56913, align 8
%stackaddr$prim56914 = alloca %struct.ScmObj*, align 8
%current_45args56020 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56019)
store volatile %struct.ScmObj* %current_45args56020, %struct.ScmObj** %stackaddr$prim56914, align 8
%stackaddr$prim56915 = alloca %struct.ScmObj*, align 8
%x47144 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56020)
store volatile %struct.ScmObj* %x47144, %struct.ScmObj** %stackaddr$prim56915, align 8
%stackaddr$prim56916 = alloca %struct.ScmObj*, align 8
%anf_45bind47348 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47144)
store volatile %struct.ScmObj* %anf_45bind47348, %struct.ScmObj** %stackaddr$prim56916, align 8
%stackaddr$prim56917 = alloca %struct.ScmObj*, align 8
%anf_45bind47349 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47348)
store volatile %struct.ScmObj* %anf_45bind47349, %struct.ScmObj** %stackaddr$prim56917, align 8
%stackaddr$prim56918 = alloca %struct.ScmObj*, align 8
%anf_45bind47350 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47349)
store volatile %struct.ScmObj* %anf_45bind47350, %struct.ScmObj** %stackaddr$prim56918, align 8
%stackaddr$prim56919 = alloca %struct.ScmObj*, align 8
%cpsprim47402 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47350)
store volatile %struct.ScmObj* %cpsprim47402, %struct.ScmObj** %stackaddr$prim56919, align 8
%ae50887 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56022$k474010 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56920 = alloca %struct.ScmObj*, align 8
%argslist56022$k474011 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47402, %struct.ScmObj* %argslist56022$k474010)
store volatile %struct.ScmObj* %argslist56022$k474011, %struct.ScmObj** %stackaddr$prim56920, align 8
%stackaddr$prim56921 = alloca %struct.ScmObj*, align 8
%argslist56022$k474012 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50887, %struct.ScmObj* %argslist56022$k474011)
store volatile %struct.ScmObj* %argslist56022$k474012, %struct.ScmObj** %stackaddr$prim56921, align 8
%clofunc56922 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47401)
musttail call tailcc void %clofunc56922(%struct.ScmObj* %k47401, %struct.ScmObj* %argslist56022$k474012)
ret void
}

define tailcc void @proc_clo$ae50857(%struct.ScmObj* %env$ae50857,%struct.ScmObj* %current_45args56024) {
%stackaddr$prim56923 = alloca %struct.ScmObj*, align 8
%k47403 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56024)
store volatile %struct.ScmObj* %k47403, %struct.ScmObj** %stackaddr$prim56923, align 8
%stackaddr$prim56924 = alloca %struct.ScmObj*, align 8
%current_45args56025 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56024)
store volatile %struct.ScmObj* %current_45args56025, %struct.ScmObj** %stackaddr$prim56924, align 8
%stackaddr$prim56925 = alloca %struct.ScmObj*, align 8
%x47146 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56025)
store volatile %struct.ScmObj* %x47146, %struct.ScmObj** %stackaddr$prim56925, align 8
%stackaddr$prim56926 = alloca %struct.ScmObj*, align 8
%anf_45bind47346 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47146)
store volatile %struct.ScmObj* %anf_45bind47346, %struct.ScmObj** %stackaddr$prim56926, align 8
%stackaddr$prim56927 = alloca %struct.ScmObj*, align 8
%anf_45bind47347 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47346)
store volatile %struct.ScmObj* %anf_45bind47347, %struct.ScmObj** %stackaddr$prim56927, align 8
%stackaddr$prim56928 = alloca %struct.ScmObj*, align 8
%cpsprim47404 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47347)
store volatile %struct.ScmObj* %cpsprim47404, %struct.ScmObj** %stackaddr$prim56928, align 8
%ae50862 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56027$k474030 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56929 = alloca %struct.ScmObj*, align 8
%argslist56027$k474031 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47404, %struct.ScmObj* %argslist56027$k474030)
store volatile %struct.ScmObj* %argslist56027$k474031, %struct.ScmObj** %stackaddr$prim56929, align 8
%stackaddr$prim56930 = alloca %struct.ScmObj*, align 8
%argslist56027$k474032 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50862, %struct.ScmObj* %argslist56027$k474031)
store volatile %struct.ScmObj* %argslist56027$k474032, %struct.ScmObj** %stackaddr$prim56930, align 8
%clofunc56931 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47403)
musttail call tailcc void %clofunc56931(%struct.ScmObj* %k47403, %struct.ScmObj* %argslist56027$k474032)
ret void
}

define tailcc void @proc_clo$ae50835(%struct.ScmObj* %env$ae50835,%struct.ScmObj* %current_45args56029) {
%stackaddr$prim56932 = alloca %struct.ScmObj*, align 8
%k47405 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56029)
store volatile %struct.ScmObj* %k47405, %struct.ScmObj** %stackaddr$prim56932, align 8
%stackaddr$prim56933 = alloca %struct.ScmObj*, align 8
%current_45args56030 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56029)
store volatile %struct.ScmObj* %current_45args56030, %struct.ScmObj** %stackaddr$prim56933, align 8
%stackaddr$prim56934 = alloca %struct.ScmObj*, align 8
%x47148 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56030)
store volatile %struct.ScmObj* %x47148, %struct.ScmObj** %stackaddr$prim56934, align 8
%stackaddr$prim56935 = alloca %struct.ScmObj*, align 8
%anf_45bind47345 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47148)
store volatile %struct.ScmObj* %anf_45bind47345, %struct.ScmObj** %stackaddr$prim56935, align 8
%stackaddr$prim56936 = alloca %struct.ScmObj*, align 8
%cpsprim47406 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47345)
store volatile %struct.ScmObj* %cpsprim47406, %struct.ScmObj** %stackaddr$prim56936, align 8
%ae50839 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56032$k474050 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56937 = alloca %struct.ScmObj*, align 8
%argslist56032$k474051 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47406, %struct.ScmObj* %argslist56032$k474050)
store volatile %struct.ScmObj* %argslist56032$k474051, %struct.ScmObj** %stackaddr$prim56937, align 8
%stackaddr$prim56938 = alloca %struct.ScmObj*, align 8
%argslist56032$k474052 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50839, %struct.ScmObj* %argslist56032$k474051)
store volatile %struct.ScmObj* %argslist56032$k474052, %struct.ScmObj** %stackaddr$prim56938, align 8
%clofunc56939 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47405)
musttail call tailcc void %clofunc56939(%struct.ScmObj* %k47405, %struct.ScmObj* %argslist56032$k474052)
ret void
}

define tailcc void @proc_clo$ae50815(%struct.ScmObj* %env$ae50815,%struct.ScmObj* %current_45args56034) {
%stackaddr$prim56940 = alloca %struct.ScmObj*, align 8
%k47407 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56034)
store volatile %struct.ScmObj* %k47407, %struct.ScmObj** %stackaddr$prim56940, align 8
%stackaddr$prim56941 = alloca %struct.ScmObj*, align 8
%current_45args56035 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56034)
store volatile %struct.ScmObj* %current_45args56035, %struct.ScmObj** %stackaddr$prim56941, align 8
%stackaddr$prim56942 = alloca %struct.ScmObj*, align 8
%x47150 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56035)
store volatile %struct.ScmObj* %x47150, %struct.ScmObj** %stackaddr$prim56942, align 8
%stackaddr$prim56943 = alloca %struct.ScmObj*, align 8
%cpsprim47408 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47150)
store volatile %struct.ScmObj* %cpsprim47408, %struct.ScmObj** %stackaddr$prim56943, align 8
%ae50818 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56037$k474070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56944 = alloca %struct.ScmObj*, align 8
%argslist56037$k474071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47408, %struct.ScmObj* %argslist56037$k474070)
store volatile %struct.ScmObj* %argslist56037$k474071, %struct.ScmObj** %stackaddr$prim56944, align 8
%stackaddr$prim56945 = alloca %struct.ScmObj*, align 8
%argslist56037$k474072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50818, %struct.ScmObj* %argslist56037$k474071)
store volatile %struct.ScmObj* %argslist56037$k474072, %struct.ScmObj** %stackaddr$prim56945, align 8
%clofunc56946 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47407)
musttail call tailcc void %clofunc56946(%struct.ScmObj* %k47407, %struct.ScmObj* %argslist56037$k474072)
ret void
}

define tailcc void @proc_clo$ae50717(%struct.ScmObj* %env$ae50717,%struct.ScmObj* %args4715247409) {
%stackaddr$env-ref56947 = alloca %struct.ScmObj*, align 8
%_37foldl147091 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50717, i64 0)
store %struct.ScmObj* %_37foldl147091, %struct.ScmObj** %stackaddr$env-ref56947
%stackaddr$prim56948 = alloca %struct.ScmObj*, align 8
%k47410 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4715247409)
store volatile %struct.ScmObj* %k47410, %struct.ScmObj** %stackaddr$prim56948, align 8
%stackaddr$prim56949 = alloca %struct.ScmObj*, align 8
%args47152 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4715247409)
store volatile %struct.ScmObj* %args47152, %struct.ScmObj** %stackaddr$prim56949, align 8
%stackaddr$prim56950 = alloca %struct.ScmObj*, align 8
%anf_45bind47339 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args47152)
store volatile %struct.ScmObj* %anf_45bind47339, %struct.ScmObj** %stackaddr$prim56950, align 8
%truthy$cmp56951 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47339)
%cmp$cmp56951 = icmp eq i64 %truthy$cmp56951, 1
br i1 %cmp$cmp56951, label %truebranch$cmp56951, label %falsebranch$cmp56951
truebranch$cmp56951:
%ae50723 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50724 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist56039$k474100 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56952 = alloca %struct.ScmObj*, align 8
%argslist56039$k474101 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50724, %struct.ScmObj* %argslist56039$k474100)
store volatile %struct.ScmObj* %argslist56039$k474101, %struct.ScmObj** %stackaddr$prim56952, align 8
%stackaddr$prim56953 = alloca %struct.ScmObj*, align 8
%argslist56039$k474102 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50723, %struct.ScmObj* %argslist56039$k474101)
store volatile %struct.ScmObj* %argslist56039$k474102, %struct.ScmObj** %stackaddr$prim56953, align 8
%clofunc56954 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47410)
musttail call tailcc void %clofunc56954(%struct.ScmObj* %k47410, %struct.ScmObj* %argslist56039$k474102)
ret void
falsebranch$cmp56951:
%stackaddr$prim56955 = alloca %struct.ScmObj*, align 8
%anf_45bind47340 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47152)
store volatile %struct.ScmObj* %anf_45bind47340, %struct.ScmObj** %stackaddr$prim56955, align 8
%stackaddr$prim56956 = alloca %struct.ScmObj*, align 8
%anf_45bind47341 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47340)
store volatile %struct.ScmObj* %anf_45bind47341, %struct.ScmObj** %stackaddr$prim56956, align 8
%truthy$cmp56957 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47341)
%cmp$cmp56957 = icmp eq i64 %truthy$cmp56957, 1
br i1 %cmp$cmp56957, label %truebranch$cmp56957, label %falsebranch$cmp56957
truebranch$cmp56957:
%stackaddr$prim56958 = alloca %struct.ScmObj*, align 8
%cpsprim47411 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47152)
store volatile %struct.ScmObj* %cpsprim47411, %struct.ScmObj** %stackaddr$prim56958, align 8
%ae50736 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56040$k474100 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56959 = alloca %struct.ScmObj*, align 8
%argslist56040$k474101 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47411, %struct.ScmObj* %argslist56040$k474100)
store volatile %struct.ScmObj* %argslist56040$k474101, %struct.ScmObj** %stackaddr$prim56959, align 8
%stackaddr$prim56960 = alloca %struct.ScmObj*, align 8
%argslist56040$k474102 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50736, %struct.ScmObj* %argslist56040$k474101)
store volatile %struct.ScmObj* %argslist56040$k474102, %struct.ScmObj** %stackaddr$prim56960, align 8
%clofunc56961 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47410)
musttail call tailcc void %clofunc56961(%struct.ScmObj* %k47410, %struct.ScmObj* %argslist56040$k474102)
ret void
falsebranch$cmp56957:
%stackaddr$makeclosure56962 = alloca %struct.ScmObj*, align 8
%fptrToInt56963 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50741 to i64
%ae50741 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56963)
store volatile %struct.ScmObj* %ae50741, %struct.ScmObj** %stackaddr$makeclosure56962, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50741, %struct.ScmObj* %_37foldl147091, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50741, %struct.ScmObj* %k47410, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50741, %struct.ScmObj* %args47152, i64 2)
%ae50742 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56964 = alloca %struct.ScmObj*, align 8
%fptrToInt56965 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50743 to i64
%ae50743 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56965)
store volatile %struct.ScmObj* %ae50743, %struct.ScmObj** %stackaddr$makeclosure56964, align 8
%argslist56050$ae507410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56966 = alloca %struct.ScmObj*, align 8
%argslist56050$ae507411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50743, %struct.ScmObj* %argslist56050$ae507410)
store volatile %struct.ScmObj* %argslist56050$ae507411, %struct.ScmObj** %stackaddr$prim56966, align 8
%stackaddr$prim56967 = alloca %struct.ScmObj*, align 8
%argslist56050$ae507412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50742, %struct.ScmObj* %argslist56050$ae507411)
store volatile %struct.ScmObj* %argslist56050$ae507412, %struct.ScmObj** %stackaddr$prim56967, align 8
%clofunc56968 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50741)
musttail call tailcc void %clofunc56968(%struct.ScmObj* %ae50741, %struct.ScmObj* %argslist56050$ae507412)
ret void
}

define tailcc void @proc_clo$ae50741(%struct.ScmObj* %env$ae50741,%struct.ScmObj* %current_45args56041) {
%stackaddr$env-ref56969 = alloca %struct.ScmObj*, align 8
%_37foldl147091 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50741, i64 0)
store %struct.ScmObj* %_37foldl147091, %struct.ScmObj** %stackaddr$env-ref56969
%stackaddr$env-ref56970 = alloca %struct.ScmObj*, align 8
%k47410 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50741, i64 1)
store %struct.ScmObj* %k47410, %struct.ScmObj** %stackaddr$env-ref56970
%stackaddr$env-ref56971 = alloca %struct.ScmObj*, align 8
%args47152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50741, i64 2)
store %struct.ScmObj* %args47152, %struct.ScmObj** %stackaddr$env-ref56971
%stackaddr$prim56972 = alloca %struct.ScmObj*, align 8
%_95k47412 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56041)
store volatile %struct.ScmObj* %_95k47412, %struct.ScmObj** %stackaddr$prim56972, align 8
%stackaddr$prim56973 = alloca %struct.ScmObj*, align 8
%current_45args56042 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56041)
store volatile %struct.ScmObj* %current_45args56042, %struct.ScmObj** %stackaddr$prim56973, align 8
%stackaddr$prim56974 = alloca %struct.ScmObj*, align 8
%anf_45bind47342 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56042)
store volatile %struct.ScmObj* %anf_45bind47342, %struct.ScmObj** %stackaddr$prim56974, align 8
%stackaddr$prim56975 = alloca %struct.ScmObj*, align 8
%anf_45bind47343 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47152)
store volatile %struct.ScmObj* %anf_45bind47343, %struct.ScmObj** %stackaddr$prim56975, align 8
%stackaddr$prim56976 = alloca %struct.ScmObj*, align 8
%anf_45bind47344 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47152)
store volatile %struct.ScmObj* %anf_45bind47344, %struct.ScmObj** %stackaddr$prim56976, align 8
%argslist56044$_37foldl1470910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56977 = alloca %struct.ScmObj*, align 8
%argslist56044$_37foldl1470911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47344, %struct.ScmObj* %argslist56044$_37foldl1470910)
store volatile %struct.ScmObj* %argslist56044$_37foldl1470911, %struct.ScmObj** %stackaddr$prim56977, align 8
%stackaddr$prim56978 = alloca %struct.ScmObj*, align 8
%argslist56044$_37foldl1470912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47343, %struct.ScmObj* %argslist56044$_37foldl1470911)
store volatile %struct.ScmObj* %argslist56044$_37foldl1470912, %struct.ScmObj** %stackaddr$prim56978, align 8
%stackaddr$prim56979 = alloca %struct.ScmObj*, align 8
%argslist56044$_37foldl1470913 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47342, %struct.ScmObj* %argslist56044$_37foldl1470912)
store volatile %struct.ScmObj* %argslist56044$_37foldl1470913, %struct.ScmObj** %stackaddr$prim56979, align 8
%stackaddr$prim56980 = alloca %struct.ScmObj*, align 8
%argslist56044$_37foldl1470914 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47410, %struct.ScmObj* %argslist56044$_37foldl1470913)
store volatile %struct.ScmObj* %argslist56044$_37foldl1470914, %struct.ScmObj** %stackaddr$prim56980, align 8
%clofunc56981 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147091)
musttail call tailcc void %clofunc56981(%struct.ScmObj* %_37foldl147091, %struct.ScmObj* %argslist56044$_37foldl1470914)
ret void
}

define tailcc void @proc_clo$ae50743(%struct.ScmObj* %env$ae50743,%struct.ScmObj* %current_45args56045) {
%stackaddr$prim56982 = alloca %struct.ScmObj*, align 8
%k47413 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56045)
store volatile %struct.ScmObj* %k47413, %struct.ScmObj** %stackaddr$prim56982, align 8
%stackaddr$prim56983 = alloca %struct.ScmObj*, align 8
%current_45args56046 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56045)
store volatile %struct.ScmObj* %current_45args56046, %struct.ScmObj** %stackaddr$prim56983, align 8
%stackaddr$prim56984 = alloca %struct.ScmObj*, align 8
%n47154 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56046)
store volatile %struct.ScmObj* %n47154, %struct.ScmObj** %stackaddr$prim56984, align 8
%stackaddr$prim56985 = alloca %struct.ScmObj*, align 8
%current_45args56047 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56046)
store volatile %struct.ScmObj* %current_45args56047, %struct.ScmObj** %stackaddr$prim56985, align 8
%stackaddr$prim56986 = alloca %struct.ScmObj*, align 8
%v47153 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56047)
store volatile %struct.ScmObj* %v47153, %struct.ScmObj** %stackaddr$prim56986, align 8
%stackaddr$prim56987 = alloca %struct.ScmObj*, align 8
%cpsprim47414 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v47153, %struct.ScmObj* %n47154)
store volatile %struct.ScmObj* %cpsprim47414, %struct.ScmObj** %stackaddr$prim56987, align 8
%ae50747 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56049$k474130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56988 = alloca %struct.ScmObj*, align 8
%argslist56049$k474131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47414, %struct.ScmObj* %argslist56049$k474130)
store volatile %struct.ScmObj* %argslist56049$k474131, %struct.ScmObj** %stackaddr$prim56988, align 8
%stackaddr$prim56989 = alloca %struct.ScmObj*, align 8
%argslist56049$k474132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50747, %struct.ScmObj* %argslist56049$k474131)
store volatile %struct.ScmObj* %argslist56049$k474132, %struct.ScmObj** %stackaddr$prim56989, align 8
%clofunc56990 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47413)
musttail call tailcc void %clofunc56990(%struct.ScmObj* %k47413, %struct.ScmObj* %argslist56049$k474132)
ret void
}

define tailcc void @proc_clo$ae50313(%struct.ScmObj* %env$ae50313,%struct.ScmObj* %current_45args56052) {
%stackaddr$prim56991 = alloca %struct.ScmObj*, align 8
%k47415 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56052)
store volatile %struct.ScmObj* %k47415, %struct.ScmObj** %stackaddr$prim56991, align 8
%stackaddr$prim56992 = alloca %struct.ScmObj*, align 8
%current_45args56053 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56052)
store volatile %struct.ScmObj* %current_45args56053, %struct.ScmObj** %stackaddr$prim56992, align 8
%stackaddr$prim56993 = alloca %struct.ScmObj*, align 8
%v47157 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56053)
store volatile %struct.ScmObj* %v47157, %struct.ScmObj** %stackaddr$prim56993, align 8
%stackaddr$prim56994 = alloca %struct.ScmObj*, align 8
%current_45args56054 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56053)
store volatile %struct.ScmObj* %current_45args56054, %struct.ScmObj** %stackaddr$prim56994, align 8
%stackaddr$prim56995 = alloca %struct.ScmObj*, align 8
%lst47156 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56054)
store volatile %struct.ScmObj* %lst47156, %struct.ScmObj** %stackaddr$prim56995, align 8
%ae50314 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56996 = alloca %struct.ScmObj*, align 8
%lst47158 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50314, %struct.ScmObj* %lst47156)
store volatile %struct.ScmObj* %lst47158, %struct.ScmObj** %stackaddr$prim56996, align 8
%stackaddr$makeclosure56997 = alloca %struct.ScmObj*, align 8
%fptrToInt56998 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50316 to i64
%ae50316 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56998)
store volatile %struct.ScmObj* %ae50316, %struct.ScmObj** %stackaddr$makeclosure56997, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50316, %struct.ScmObj* %lst47158, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50316, %struct.ScmObj* %v47157, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50316, %struct.ScmObj* %k47415, i64 2)
%ae50317 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56999 = alloca %struct.ScmObj*, align 8
%fptrToInt57000 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50318 to i64
%ae50318 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57000)
store volatile %struct.ScmObj* %ae50318, %struct.ScmObj** %stackaddr$makeclosure56999, align 8
%argslist56076$ae503160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57001 = alloca %struct.ScmObj*, align 8
%argslist56076$ae503161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50318, %struct.ScmObj* %argslist56076$ae503160)
store volatile %struct.ScmObj* %argslist56076$ae503161, %struct.ScmObj** %stackaddr$prim57001, align 8
%stackaddr$prim57002 = alloca %struct.ScmObj*, align 8
%argslist56076$ae503162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50317, %struct.ScmObj* %argslist56076$ae503161)
store volatile %struct.ScmObj* %argslist56076$ae503162, %struct.ScmObj** %stackaddr$prim57002, align 8
%clofunc57003 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50316)
musttail call tailcc void %clofunc57003(%struct.ScmObj* %ae50316, %struct.ScmObj* %argslist56076$ae503162)
ret void
}

define tailcc void @proc_clo$ae50316(%struct.ScmObj* %env$ae50316,%struct.ScmObj* %current_45args56056) {
%stackaddr$env-ref57004 = alloca %struct.ScmObj*, align 8
%lst47158 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50316, i64 0)
store %struct.ScmObj* %lst47158, %struct.ScmObj** %stackaddr$env-ref57004
%stackaddr$env-ref57005 = alloca %struct.ScmObj*, align 8
%v47157 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50316, i64 1)
store %struct.ScmObj* %v47157, %struct.ScmObj** %stackaddr$env-ref57005
%stackaddr$env-ref57006 = alloca %struct.ScmObj*, align 8
%k47415 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50316, i64 2)
store %struct.ScmObj* %k47415, %struct.ScmObj** %stackaddr$env-ref57006
%stackaddr$prim57007 = alloca %struct.ScmObj*, align 8
%_95k47416 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56056)
store volatile %struct.ScmObj* %_95k47416, %struct.ScmObj** %stackaddr$prim57007, align 8
%stackaddr$prim57008 = alloca %struct.ScmObj*, align 8
%current_45args56057 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56056)
store volatile %struct.ScmObj* %current_45args56057, %struct.ScmObj** %stackaddr$prim57008, align 8
%stackaddr$prim57009 = alloca %struct.ScmObj*, align 8
%anf_45bind47331 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56057)
store volatile %struct.ScmObj* %anf_45bind47331, %struct.ScmObj** %stackaddr$prim57009, align 8
%stackaddr$makeclosure57010 = alloca %struct.ScmObj*, align 8
%fptrToInt57011 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50332 to i64
%ae50332 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57011)
store volatile %struct.ScmObj* %ae50332, %struct.ScmObj** %stackaddr$makeclosure57010, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50332, %struct.ScmObj* %lst47158, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50332, %struct.ScmObj* %v47157, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50332, %struct.ScmObj* %k47415, i64 2)
%stackaddr$makeclosure57012 = alloca %struct.ScmObj*, align 8
%fptrToInt57013 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50333 to i64
%ae50333 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57013)
store volatile %struct.ScmObj* %ae50333, %struct.ScmObj** %stackaddr$makeclosure57012, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50333, %struct.ScmObj* %lst47158, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50333, %struct.ScmObj* %v47157, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50333, %struct.ScmObj* %k47415, i64 2)
%argslist56071$anf_45bind473310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57014 = alloca %struct.ScmObj*, align 8
%argslist56071$anf_45bind473311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50333, %struct.ScmObj* %argslist56071$anf_45bind473310)
store volatile %struct.ScmObj* %argslist56071$anf_45bind473311, %struct.ScmObj** %stackaddr$prim57014, align 8
%stackaddr$prim57015 = alloca %struct.ScmObj*, align 8
%argslist56071$anf_45bind473312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50332, %struct.ScmObj* %argslist56071$anf_45bind473311)
store volatile %struct.ScmObj* %argslist56071$anf_45bind473312, %struct.ScmObj** %stackaddr$prim57015, align 8
%clofunc57016 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47331)
musttail call tailcc void %clofunc57016(%struct.ScmObj* %anf_45bind47331, %struct.ScmObj* %argslist56071$anf_45bind473312)
ret void
}

define tailcc void @proc_clo$ae50332(%struct.ScmObj* %env$ae50332,%struct.ScmObj* %current_45args56059) {
%stackaddr$env-ref57017 = alloca %struct.ScmObj*, align 8
%lst47158 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50332, i64 0)
store %struct.ScmObj* %lst47158, %struct.ScmObj** %stackaddr$env-ref57017
%stackaddr$env-ref57018 = alloca %struct.ScmObj*, align 8
%v47157 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50332, i64 1)
store %struct.ScmObj* %v47157, %struct.ScmObj** %stackaddr$env-ref57018
%stackaddr$env-ref57019 = alloca %struct.ScmObj*, align 8
%k47415 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50332, i64 2)
store %struct.ScmObj* %k47415, %struct.ScmObj** %stackaddr$env-ref57019
%stackaddr$prim57020 = alloca %struct.ScmObj*, align 8
%_95k47417 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56059)
store volatile %struct.ScmObj* %_95k47417, %struct.ScmObj** %stackaddr$prim57020, align 8
%stackaddr$prim57021 = alloca %struct.ScmObj*, align 8
%current_45args56060 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56059)
store volatile %struct.ScmObj* %current_45args56060, %struct.ScmObj** %stackaddr$prim57021, align 8
%stackaddr$prim57022 = alloca %struct.ScmObj*, align 8
%cc47159 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56060)
store volatile %struct.ScmObj* %cc47159, %struct.ScmObj** %stackaddr$prim57022, align 8
%ae50441 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57023 = alloca %struct.ScmObj*, align 8
%anf_45bind47332 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47158, %struct.ScmObj* %ae50441)
store volatile %struct.ScmObj* %anf_45bind47332, %struct.ScmObj** %stackaddr$prim57023, align 8
%stackaddr$prim57024 = alloca %struct.ScmObj*, align 8
%anf_45bind47333 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47332)
store volatile %struct.ScmObj* %anf_45bind47333, %struct.ScmObj** %stackaddr$prim57024, align 8
%truthy$cmp57025 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47333)
%cmp$cmp57025 = icmp eq i64 %truthy$cmp57025, 1
br i1 %cmp$cmp57025, label %truebranch$cmp57025, label %falsebranch$cmp57025
truebranch$cmp57025:
%ae50445 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50446 = call %struct.ScmObj* @const_init_false()
%argslist56062$k474150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57026 = alloca %struct.ScmObj*, align 8
%argslist56062$k474151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50446, %struct.ScmObj* %argslist56062$k474150)
store volatile %struct.ScmObj* %argslist56062$k474151, %struct.ScmObj** %stackaddr$prim57026, align 8
%stackaddr$prim57027 = alloca %struct.ScmObj*, align 8
%argslist56062$k474152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50445, %struct.ScmObj* %argslist56062$k474151)
store volatile %struct.ScmObj* %argslist56062$k474152, %struct.ScmObj** %stackaddr$prim57027, align 8
%clofunc57028 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47415)
musttail call tailcc void %clofunc57028(%struct.ScmObj* %k47415, %struct.ScmObj* %argslist56062$k474152)
ret void
falsebranch$cmp57025:
%ae50454 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57029 = alloca %struct.ScmObj*, align 8
%anf_45bind47334 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47158, %struct.ScmObj* %ae50454)
store volatile %struct.ScmObj* %anf_45bind47334, %struct.ScmObj** %stackaddr$prim57029, align 8
%stackaddr$prim57030 = alloca %struct.ScmObj*, align 8
%anf_45bind47335 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47334)
store volatile %struct.ScmObj* %anf_45bind47335, %struct.ScmObj** %stackaddr$prim57030, align 8
%stackaddr$prim57031 = alloca %struct.ScmObj*, align 8
%anf_45bind47336 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind47335, %struct.ScmObj* %v47157)
store volatile %struct.ScmObj* %anf_45bind47336, %struct.ScmObj** %stackaddr$prim57031, align 8
%truthy$cmp57032 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47336)
%cmp$cmp57032 = icmp eq i64 %truthy$cmp57032, 1
br i1 %cmp$cmp57032, label %truebranch$cmp57032, label %falsebranch$cmp57032
truebranch$cmp57032:
%ae50460 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57033 = alloca %struct.ScmObj*, align 8
%cpsprim47418 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47158, %struct.ScmObj* %ae50460)
store volatile %struct.ScmObj* %cpsprim47418, %struct.ScmObj** %stackaddr$prim57033, align 8
%ae50462 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56063$k474150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57034 = alloca %struct.ScmObj*, align 8
%argslist56063$k474151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47418, %struct.ScmObj* %argslist56063$k474150)
store volatile %struct.ScmObj* %argslist56063$k474151, %struct.ScmObj** %stackaddr$prim57034, align 8
%stackaddr$prim57035 = alloca %struct.ScmObj*, align 8
%argslist56063$k474152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50462, %struct.ScmObj* %argslist56063$k474151)
store volatile %struct.ScmObj* %argslist56063$k474152, %struct.ScmObj** %stackaddr$prim57035, align 8
%clofunc57036 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47415)
musttail call tailcc void %clofunc57036(%struct.ScmObj* %k47415, %struct.ScmObj* %argslist56063$k474152)
ret void
falsebranch$cmp57032:
%ae50473 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57037 = alloca %struct.ScmObj*, align 8
%anf_45bind47337 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47158, %struct.ScmObj* %ae50473)
store volatile %struct.ScmObj* %anf_45bind47337, %struct.ScmObj** %stackaddr$prim57037, align 8
%stackaddr$prim57038 = alloca %struct.ScmObj*, align 8
%anf_45bind47338 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47337)
store volatile %struct.ScmObj* %anf_45bind47338, %struct.ScmObj** %stackaddr$prim57038, align 8
%ae50476 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57039 = alloca %struct.ScmObj*, align 8
%_95047161 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47158, %struct.ScmObj* %ae50476, %struct.ScmObj* %anf_45bind47338)
store volatile %struct.ScmObj* %_95047161, %struct.ScmObj** %stackaddr$prim57039, align 8
%argslist56064$cc471590 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57040 = alloca %struct.ScmObj*, align 8
%argslist56064$cc471591 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47159, %struct.ScmObj* %argslist56064$cc471590)
store volatile %struct.ScmObj* %argslist56064$cc471591, %struct.ScmObj** %stackaddr$prim57040, align 8
%stackaddr$prim57041 = alloca %struct.ScmObj*, align 8
%argslist56064$cc471592 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47415, %struct.ScmObj* %argslist56064$cc471591)
store volatile %struct.ScmObj* %argslist56064$cc471592, %struct.ScmObj** %stackaddr$prim57041, align 8
%clofunc57042 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47159)
musttail call tailcc void %clofunc57042(%struct.ScmObj* %cc47159, %struct.ScmObj* %argslist56064$cc471592)
ret void
}

define tailcc void @proc_clo$ae50333(%struct.ScmObj* %env$ae50333,%struct.ScmObj* %current_45args56065) {
%stackaddr$env-ref57043 = alloca %struct.ScmObj*, align 8
%lst47158 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50333, i64 0)
store %struct.ScmObj* %lst47158, %struct.ScmObj** %stackaddr$env-ref57043
%stackaddr$env-ref57044 = alloca %struct.ScmObj*, align 8
%v47157 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50333, i64 1)
store %struct.ScmObj* %v47157, %struct.ScmObj** %stackaddr$env-ref57044
%stackaddr$env-ref57045 = alloca %struct.ScmObj*, align 8
%k47415 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50333, i64 2)
store %struct.ScmObj* %k47415, %struct.ScmObj** %stackaddr$env-ref57045
%stackaddr$prim57046 = alloca %struct.ScmObj*, align 8
%_95k47417 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56065)
store volatile %struct.ScmObj* %_95k47417, %struct.ScmObj** %stackaddr$prim57046, align 8
%stackaddr$prim57047 = alloca %struct.ScmObj*, align 8
%current_45args56066 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56065)
store volatile %struct.ScmObj* %current_45args56066, %struct.ScmObj** %stackaddr$prim57047, align 8
%stackaddr$prim57048 = alloca %struct.ScmObj*, align 8
%cc47159 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56066)
store volatile %struct.ScmObj* %cc47159, %struct.ScmObj** %stackaddr$prim57048, align 8
%ae50335 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57049 = alloca %struct.ScmObj*, align 8
%anf_45bind47332 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47158, %struct.ScmObj* %ae50335)
store volatile %struct.ScmObj* %anf_45bind47332, %struct.ScmObj** %stackaddr$prim57049, align 8
%stackaddr$prim57050 = alloca %struct.ScmObj*, align 8
%anf_45bind47333 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47332)
store volatile %struct.ScmObj* %anf_45bind47333, %struct.ScmObj** %stackaddr$prim57050, align 8
%truthy$cmp57051 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47333)
%cmp$cmp57051 = icmp eq i64 %truthy$cmp57051, 1
br i1 %cmp$cmp57051, label %truebranch$cmp57051, label %falsebranch$cmp57051
truebranch$cmp57051:
%ae50339 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50340 = call %struct.ScmObj* @const_init_false()
%argslist56068$k474150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57052 = alloca %struct.ScmObj*, align 8
%argslist56068$k474151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50340, %struct.ScmObj* %argslist56068$k474150)
store volatile %struct.ScmObj* %argslist56068$k474151, %struct.ScmObj** %stackaddr$prim57052, align 8
%stackaddr$prim57053 = alloca %struct.ScmObj*, align 8
%argslist56068$k474152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50339, %struct.ScmObj* %argslist56068$k474151)
store volatile %struct.ScmObj* %argslist56068$k474152, %struct.ScmObj** %stackaddr$prim57053, align 8
%clofunc57054 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47415)
musttail call tailcc void %clofunc57054(%struct.ScmObj* %k47415, %struct.ScmObj* %argslist56068$k474152)
ret void
falsebranch$cmp57051:
%ae50348 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57055 = alloca %struct.ScmObj*, align 8
%anf_45bind47334 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47158, %struct.ScmObj* %ae50348)
store volatile %struct.ScmObj* %anf_45bind47334, %struct.ScmObj** %stackaddr$prim57055, align 8
%stackaddr$prim57056 = alloca %struct.ScmObj*, align 8
%anf_45bind47335 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47334)
store volatile %struct.ScmObj* %anf_45bind47335, %struct.ScmObj** %stackaddr$prim57056, align 8
%stackaddr$prim57057 = alloca %struct.ScmObj*, align 8
%anf_45bind47336 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind47335, %struct.ScmObj* %v47157)
store volatile %struct.ScmObj* %anf_45bind47336, %struct.ScmObj** %stackaddr$prim57057, align 8
%truthy$cmp57058 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47336)
%cmp$cmp57058 = icmp eq i64 %truthy$cmp57058, 1
br i1 %cmp$cmp57058, label %truebranch$cmp57058, label %falsebranch$cmp57058
truebranch$cmp57058:
%ae50354 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57059 = alloca %struct.ScmObj*, align 8
%cpsprim47418 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47158, %struct.ScmObj* %ae50354)
store volatile %struct.ScmObj* %cpsprim47418, %struct.ScmObj** %stackaddr$prim57059, align 8
%ae50356 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56069$k474150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57060 = alloca %struct.ScmObj*, align 8
%argslist56069$k474151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47418, %struct.ScmObj* %argslist56069$k474150)
store volatile %struct.ScmObj* %argslist56069$k474151, %struct.ScmObj** %stackaddr$prim57060, align 8
%stackaddr$prim57061 = alloca %struct.ScmObj*, align 8
%argslist56069$k474152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50356, %struct.ScmObj* %argslist56069$k474151)
store volatile %struct.ScmObj* %argslist56069$k474152, %struct.ScmObj** %stackaddr$prim57061, align 8
%clofunc57062 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47415)
musttail call tailcc void %clofunc57062(%struct.ScmObj* %k47415, %struct.ScmObj* %argslist56069$k474152)
ret void
falsebranch$cmp57058:
%ae50367 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57063 = alloca %struct.ScmObj*, align 8
%anf_45bind47337 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47158, %struct.ScmObj* %ae50367)
store volatile %struct.ScmObj* %anf_45bind47337, %struct.ScmObj** %stackaddr$prim57063, align 8
%stackaddr$prim57064 = alloca %struct.ScmObj*, align 8
%anf_45bind47338 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47337)
store volatile %struct.ScmObj* %anf_45bind47338, %struct.ScmObj** %stackaddr$prim57064, align 8
%ae50370 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57065 = alloca %struct.ScmObj*, align 8
%_95047161 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47158, %struct.ScmObj* %ae50370, %struct.ScmObj* %anf_45bind47338)
store volatile %struct.ScmObj* %_95047161, %struct.ScmObj** %stackaddr$prim57065, align 8
%argslist56070$cc471590 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57066 = alloca %struct.ScmObj*, align 8
%argslist56070$cc471591 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47159, %struct.ScmObj* %argslist56070$cc471590)
store volatile %struct.ScmObj* %argslist56070$cc471591, %struct.ScmObj** %stackaddr$prim57066, align 8
%stackaddr$prim57067 = alloca %struct.ScmObj*, align 8
%argslist56070$cc471592 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47415, %struct.ScmObj* %argslist56070$cc471591)
store volatile %struct.ScmObj* %argslist56070$cc471592, %struct.ScmObj** %stackaddr$prim57067, align 8
%clofunc57068 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47159)
musttail call tailcc void %clofunc57068(%struct.ScmObj* %cc47159, %struct.ScmObj* %argslist56070$cc471592)
ret void
}

define tailcc void @proc_clo$ae50318(%struct.ScmObj* %env$ae50318,%struct.ScmObj* %current_45args56072) {
%stackaddr$prim57069 = alloca %struct.ScmObj*, align 8
%k47419 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56072)
store volatile %struct.ScmObj* %k47419, %struct.ScmObj** %stackaddr$prim57069, align 8
%stackaddr$prim57070 = alloca %struct.ScmObj*, align 8
%current_45args56073 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56072)
store volatile %struct.ScmObj* %current_45args56073, %struct.ScmObj** %stackaddr$prim57070, align 8
%stackaddr$prim57071 = alloca %struct.ScmObj*, align 8
%u47160 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56073)
store volatile %struct.ScmObj* %u47160, %struct.ScmObj** %stackaddr$prim57071, align 8
%argslist56075$u471600 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57072 = alloca %struct.ScmObj*, align 8
%argslist56075$u471601 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u47160, %struct.ScmObj* %argslist56075$u471600)
store volatile %struct.ScmObj* %argslist56075$u471601, %struct.ScmObj** %stackaddr$prim57072, align 8
%stackaddr$prim57073 = alloca %struct.ScmObj*, align 8
%argslist56075$u471602 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47419, %struct.ScmObj* %argslist56075$u471601)
store volatile %struct.ScmObj* %argslist56075$u471602, %struct.ScmObj** %stackaddr$prim57073, align 8
%clofunc57074 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u47160)
musttail call tailcc void %clofunc57074(%struct.ScmObj* %u47160, %struct.ScmObj* %argslist56075$u471602)
ret void
}

define tailcc void @proc_clo$ae49777(%struct.ScmObj* %env$ae49777,%struct.ScmObj* %current_45args56078) {
%stackaddr$prim57075 = alloca %struct.ScmObj*, align 8
%k47420 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56078)
store volatile %struct.ScmObj* %k47420, %struct.ScmObj** %stackaddr$prim57075, align 8
%stackaddr$prim57076 = alloca %struct.ScmObj*, align 8
%current_45args56079 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56078)
store volatile %struct.ScmObj* %current_45args56079, %struct.ScmObj** %stackaddr$prim57076, align 8
%stackaddr$prim57077 = alloca %struct.ScmObj*, align 8
%lst47164 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56079)
store volatile %struct.ScmObj* %lst47164, %struct.ScmObj** %stackaddr$prim57077, align 8
%stackaddr$prim57078 = alloca %struct.ScmObj*, align 8
%current_45args56080 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56079)
store volatile %struct.ScmObj* %current_45args56080, %struct.ScmObj** %stackaddr$prim57078, align 8
%stackaddr$prim57079 = alloca %struct.ScmObj*, align 8
%n47163 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56080)
store volatile %struct.ScmObj* %n47163, %struct.ScmObj** %stackaddr$prim57079, align 8
%ae49778 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57080 = alloca %struct.ScmObj*, align 8
%n47166 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49778, %struct.ScmObj* %n47163)
store volatile %struct.ScmObj* %n47166, %struct.ScmObj** %stackaddr$prim57080, align 8
%ae49780 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57081 = alloca %struct.ScmObj*, align 8
%lst47165 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49780, %struct.ScmObj* %lst47164)
store volatile %struct.ScmObj* %lst47165, %struct.ScmObj** %stackaddr$prim57081, align 8
%stackaddr$makeclosure57082 = alloca %struct.ScmObj*, align 8
%fptrToInt57083 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49782 to i64
%ae49782 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57083)
store volatile %struct.ScmObj* %ae49782, %struct.ScmObj** %stackaddr$makeclosure57082, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49782, %struct.ScmObj* %n47166, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49782, %struct.ScmObj* %lst47165, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49782, %struct.ScmObj* %k47420, i64 2)
%ae49783 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57084 = alloca %struct.ScmObj*, align 8
%fptrToInt57085 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49784 to i64
%ae49784 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57085)
store volatile %struct.ScmObj* %ae49784, %struct.ScmObj** %stackaddr$makeclosure57084, align 8
%argslist56100$ae497820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57086 = alloca %struct.ScmObj*, align 8
%argslist56100$ae497821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49784, %struct.ScmObj* %argslist56100$ae497820)
store volatile %struct.ScmObj* %argslist56100$ae497821, %struct.ScmObj** %stackaddr$prim57086, align 8
%stackaddr$prim57087 = alloca %struct.ScmObj*, align 8
%argslist56100$ae497822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49783, %struct.ScmObj* %argslist56100$ae497821)
store volatile %struct.ScmObj* %argslist56100$ae497822, %struct.ScmObj** %stackaddr$prim57087, align 8
%clofunc57088 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49782)
musttail call tailcc void %clofunc57088(%struct.ScmObj* %ae49782, %struct.ScmObj* %argslist56100$ae497822)
ret void
}

define tailcc void @proc_clo$ae49782(%struct.ScmObj* %env$ae49782,%struct.ScmObj* %current_45args56082) {
%stackaddr$env-ref57089 = alloca %struct.ScmObj*, align 8
%n47166 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49782, i64 0)
store %struct.ScmObj* %n47166, %struct.ScmObj** %stackaddr$env-ref57089
%stackaddr$env-ref57090 = alloca %struct.ScmObj*, align 8
%lst47165 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49782, i64 1)
store %struct.ScmObj* %lst47165, %struct.ScmObj** %stackaddr$env-ref57090
%stackaddr$env-ref57091 = alloca %struct.ScmObj*, align 8
%k47420 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49782, i64 2)
store %struct.ScmObj* %k47420, %struct.ScmObj** %stackaddr$env-ref57091
%stackaddr$prim57092 = alloca %struct.ScmObj*, align 8
%_95k47421 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56082)
store volatile %struct.ScmObj* %_95k47421, %struct.ScmObj** %stackaddr$prim57092, align 8
%stackaddr$prim57093 = alloca %struct.ScmObj*, align 8
%current_45args56083 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56082)
store volatile %struct.ScmObj* %current_45args56083, %struct.ScmObj** %stackaddr$prim57093, align 8
%stackaddr$prim57094 = alloca %struct.ScmObj*, align 8
%anf_45bind47324 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56083)
store volatile %struct.ScmObj* %anf_45bind47324, %struct.ScmObj** %stackaddr$prim57094, align 8
%stackaddr$makeclosure57095 = alloca %struct.ScmObj*, align 8
%fptrToInt57096 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49798 to i64
%ae49798 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57096)
store volatile %struct.ScmObj* %ae49798, %struct.ScmObj** %stackaddr$makeclosure57095, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49798, %struct.ScmObj* %n47166, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49798, %struct.ScmObj* %lst47165, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49798, %struct.ScmObj* %k47420, i64 2)
%stackaddr$makeclosure57097 = alloca %struct.ScmObj*, align 8
%fptrToInt57098 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49799 to i64
%ae49799 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57098)
store volatile %struct.ScmObj* %ae49799, %struct.ScmObj** %stackaddr$makeclosure57097, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49799, %struct.ScmObj* %n47166, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49799, %struct.ScmObj* %lst47165, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49799, %struct.ScmObj* %k47420, i64 2)
%argslist56095$anf_45bind473240 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57099 = alloca %struct.ScmObj*, align 8
%argslist56095$anf_45bind473241 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49799, %struct.ScmObj* %argslist56095$anf_45bind473240)
store volatile %struct.ScmObj* %argslist56095$anf_45bind473241, %struct.ScmObj** %stackaddr$prim57099, align 8
%stackaddr$prim57100 = alloca %struct.ScmObj*, align 8
%argslist56095$anf_45bind473242 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49798, %struct.ScmObj* %argslist56095$anf_45bind473241)
store volatile %struct.ScmObj* %argslist56095$anf_45bind473242, %struct.ScmObj** %stackaddr$prim57100, align 8
%clofunc57101 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47324)
musttail call tailcc void %clofunc57101(%struct.ScmObj* %anf_45bind47324, %struct.ScmObj* %argslist56095$anf_45bind473242)
ret void
}

define tailcc void @proc_clo$ae49798(%struct.ScmObj* %env$ae49798,%struct.ScmObj* %current_45args56085) {
%stackaddr$env-ref57102 = alloca %struct.ScmObj*, align 8
%n47166 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49798, i64 0)
store %struct.ScmObj* %n47166, %struct.ScmObj** %stackaddr$env-ref57102
%stackaddr$env-ref57103 = alloca %struct.ScmObj*, align 8
%lst47165 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49798, i64 1)
store %struct.ScmObj* %lst47165, %struct.ScmObj** %stackaddr$env-ref57103
%stackaddr$env-ref57104 = alloca %struct.ScmObj*, align 8
%k47420 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49798, i64 2)
store %struct.ScmObj* %k47420, %struct.ScmObj** %stackaddr$env-ref57104
%stackaddr$prim57105 = alloca %struct.ScmObj*, align 8
%_95k47422 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56085)
store volatile %struct.ScmObj* %_95k47422, %struct.ScmObj** %stackaddr$prim57105, align 8
%stackaddr$prim57106 = alloca %struct.ScmObj*, align 8
%current_45args56086 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56085)
store volatile %struct.ScmObj* %current_45args56086, %struct.ScmObj** %stackaddr$prim57106, align 8
%stackaddr$prim57107 = alloca %struct.ScmObj*, align 8
%cc47167 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56086)
store volatile %struct.ScmObj* %cc47167, %struct.ScmObj** %stackaddr$prim57107, align 8
%ae49941 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57108 = alloca %struct.ScmObj*, align 8
%anf_45bind47325 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47166, %struct.ScmObj* %ae49941)
store volatile %struct.ScmObj* %anf_45bind47325, %struct.ScmObj** %stackaddr$prim57108, align 8
%ae49942 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57109 = alloca %struct.ScmObj*, align 8
%anf_45bind47326 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae49942, %struct.ScmObj* %anf_45bind47325)
store volatile %struct.ScmObj* %anf_45bind47326, %struct.ScmObj** %stackaddr$prim57109, align 8
%truthy$cmp57110 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47326)
%cmp$cmp57110 = icmp eq i64 %truthy$cmp57110, 1
br i1 %cmp$cmp57110, label %truebranch$cmp57110, label %falsebranch$cmp57110
truebranch$cmp57110:
%ae49946 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57111 = alloca %struct.ScmObj*, align 8
%cpsprim47423 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47165, %struct.ScmObj* %ae49946)
store volatile %struct.ScmObj* %cpsprim47423, %struct.ScmObj** %stackaddr$prim57111, align 8
%ae49948 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56088$k474200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57112 = alloca %struct.ScmObj*, align 8
%argslist56088$k474201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47423, %struct.ScmObj* %argslist56088$k474200)
store volatile %struct.ScmObj* %argslist56088$k474201, %struct.ScmObj** %stackaddr$prim57112, align 8
%stackaddr$prim57113 = alloca %struct.ScmObj*, align 8
%argslist56088$k474202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49948, %struct.ScmObj* %argslist56088$k474201)
store volatile %struct.ScmObj* %argslist56088$k474202, %struct.ScmObj** %stackaddr$prim57113, align 8
%clofunc57114 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47420)
musttail call tailcc void %clofunc57114(%struct.ScmObj* %k47420, %struct.ScmObj* %argslist56088$k474202)
ret void
falsebranch$cmp57110:
%ae49959 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57115 = alloca %struct.ScmObj*, align 8
%anf_45bind47327 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47165, %struct.ScmObj* %ae49959)
store volatile %struct.ScmObj* %anf_45bind47327, %struct.ScmObj** %stackaddr$prim57115, align 8
%stackaddr$prim57116 = alloca %struct.ScmObj*, align 8
%anf_45bind47328 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47327)
store volatile %struct.ScmObj* %anf_45bind47328, %struct.ScmObj** %stackaddr$prim57116, align 8
%ae49962 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57117 = alloca %struct.ScmObj*, align 8
%_95047170 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47165, %struct.ScmObj* %ae49962, %struct.ScmObj* %anf_45bind47328)
store volatile %struct.ScmObj* %_95047170, %struct.ScmObj** %stackaddr$prim57117, align 8
%ae49965 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57118 = alloca %struct.ScmObj*, align 8
%anf_45bind47329 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47166, %struct.ScmObj* %ae49965)
store volatile %struct.ScmObj* %anf_45bind47329, %struct.ScmObj** %stackaddr$prim57118, align 8
%ae49967 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57119 = alloca %struct.ScmObj*, align 8
%anf_45bind47330 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind47329, %struct.ScmObj* %ae49967)
store volatile %struct.ScmObj* %anf_45bind47330, %struct.ScmObj** %stackaddr$prim57119, align 8
%ae49969 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57120 = alloca %struct.ScmObj*, align 8
%_95147169 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n47166, %struct.ScmObj* %ae49969, %struct.ScmObj* %anf_45bind47330)
store volatile %struct.ScmObj* %_95147169, %struct.ScmObj** %stackaddr$prim57120, align 8
%argslist56089$cc471670 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57121 = alloca %struct.ScmObj*, align 8
%argslist56089$cc471671 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47167, %struct.ScmObj* %argslist56089$cc471670)
store volatile %struct.ScmObj* %argslist56089$cc471671, %struct.ScmObj** %stackaddr$prim57121, align 8
%stackaddr$prim57122 = alloca %struct.ScmObj*, align 8
%argslist56089$cc471672 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47420, %struct.ScmObj* %argslist56089$cc471671)
store volatile %struct.ScmObj* %argslist56089$cc471672, %struct.ScmObj** %stackaddr$prim57122, align 8
%clofunc57123 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47167)
musttail call tailcc void %clofunc57123(%struct.ScmObj* %cc47167, %struct.ScmObj* %argslist56089$cc471672)
ret void
}

define tailcc void @proc_clo$ae49799(%struct.ScmObj* %env$ae49799,%struct.ScmObj* %current_45args56090) {
%stackaddr$env-ref57124 = alloca %struct.ScmObj*, align 8
%n47166 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49799, i64 0)
store %struct.ScmObj* %n47166, %struct.ScmObj** %stackaddr$env-ref57124
%stackaddr$env-ref57125 = alloca %struct.ScmObj*, align 8
%lst47165 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49799, i64 1)
store %struct.ScmObj* %lst47165, %struct.ScmObj** %stackaddr$env-ref57125
%stackaddr$env-ref57126 = alloca %struct.ScmObj*, align 8
%k47420 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49799, i64 2)
store %struct.ScmObj* %k47420, %struct.ScmObj** %stackaddr$env-ref57126
%stackaddr$prim57127 = alloca %struct.ScmObj*, align 8
%_95k47422 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56090)
store volatile %struct.ScmObj* %_95k47422, %struct.ScmObj** %stackaddr$prim57127, align 8
%stackaddr$prim57128 = alloca %struct.ScmObj*, align 8
%current_45args56091 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56090)
store volatile %struct.ScmObj* %current_45args56091, %struct.ScmObj** %stackaddr$prim57128, align 8
%stackaddr$prim57129 = alloca %struct.ScmObj*, align 8
%cc47167 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56091)
store volatile %struct.ScmObj* %cc47167, %struct.ScmObj** %stackaddr$prim57129, align 8
%ae49801 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57130 = alloca %struct.ScmObj*, align 8
%anf_45bind47325 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47166, %struct.ScmObj* %ae49801)
store volatile %struct.ScmObj* %anf_45bind47325, %struct.ScmObj** %stackaddr$prim57130, align 8
%ae49802 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57131 = alloca %struct.ScmObj*, align 8
%anf_45bind47326 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae49802, %struct.ScmObj* %anf_45bind47325)
store volatile %struct.ScmObj* %anf_45bind47326, %struct.ScmObj** %stackaddr$prim57131, align 8
%truthy$cmp57132 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47326)
%cmp$cmp57132 = icmp eq i64 %truthy$cmp57132, 1
br i1 %cmp$cmp57132, label %truebranch$cmp57132, label %falsebranch$cmp57132
truebranch$cmp57132:
%ae49806 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57133 = alloca %struct.ScmObj*, align 8
%cpsprim47423 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47165, %struct.ScmObj* %ae49806)
store volatile %struct.ScmObj* %cpsprim47423, %struct.ScmObj** %stackaddr$prim57133, align 8
%ae49808 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56093$k474200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57134 = alloca %struct.ScmObj*, align 8
%argslist56093$k474201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47423, %struct.ScmObj* %argslist56093$k474200)
store volatile %struct.ScmObj* %argslist56093$k474201, %struct.ScmObj** %stackaddr$prim57134, align 8
%stackaddr$prim57135 = alloca %struct.ScmObj*, align 8
%argslist56093$k474202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49808, %struct.ScmObj* %argslist56093$k474201)
store volatile %struct.ScmObj* %argslist56093$k474202, %struct.ScmObj** %stackaddr$prim57135, align 8
%clofunc57136 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47420)
musttail call tailcc void %clofunc57136(%struct.ScmObj* %k47420, %struct.ScmObj* %argslist56093$k474202)
ret void
falsebranch$cmp57132:
%ae49819 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57137 = alloca %struct.ScmObj*, align 8
%anf_45bind47327 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47165, %struct.ScmObj* %ae49819)
store volatile %struct.ScmObj* %anf_45bind47327, %struct.ScmObj** %stackaddr$prim57137, align 8
%stackaddr$prim57138 = alloca %struct.ScmObj*, align 8
%anf_45bind47328 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47327)
store volatile %struct.ScmObj* %anf_45bind47328, %struct.ScmObj** %stackaddr$prim57138, align 8
%ae49822 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57139 = alloca %struct.ScmObj*, align 8
%_95047170 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47165, %struct.ScmObj* %ae49822, %struct.ScmObj* %anf_45bind47328)
store volatile %struct.ScmObj* %_95047170, %struct.ScmObj** %stackaddr$prim57139, align 8
%ae49825 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57140 = alloca %struct.ScmObj*, align 8
%anf_45bind47329 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47166, %struct.ScmObj* %ae49825)
store volatile %struct.ScmObj* %anf_45bind47329, %struct.ScmObj** %stackaddr$prim57140, align 8
%ae49827 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57141 = alloca %struct.ScmObj*, align 8
%anf_45bind47330 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind47329, %struct.ScmObj* %ae49827)
store volatile %struct.ScmObj* %anf_45bind47330, %struct.ScmObj** %stackaddr$prim57141, align 8
%ae49829 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57142 = alloca %struct.ScmObj*, align 8
%_95147169 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n47166, %struct.ScmObj* %ae49829, %struct.ScmObj* %anf_45bind47330)
store volatile %struct.ScmObj* %_95147169, %struct.ScmObj** %stackaddr$prim57142, align 8
%argslist56094$cc471670 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57143 = alloca %struct.ScmObj*, align 8
%argslist56094$cc471671 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47167, %struct.ScmObj* %argslist56094$cc471670)
store volatile %struct.ScmObj* %argslist56094$cc471671, %struct.ScmObj** %stackaddr$prim57143, align 8
%stackaddr$prim57144 = alloca %struct.ScmObj*, align 8
%argslist56094$cc471672 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47420, %struct.ScmObj* %argslist56094$cc471671)
store volatile %struct.ScmObj* %argslist56094$cc471672, %struct.ScmObj** %stackaddr$prim57144, align 8
%clofunc57145 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47167)
musttail call tailcc void %clofunc57145(%struct.ScmObj* %cc47167, %struct.ScmObj* %argslist56094$cc471672)
ret void
}

define tailcc void @proc_clo$ae49784(%struct.ScmObj* %env$ae49784,%struct.ScmObj* %current_45args56096) {
%stackaddr$prim57146 = alloca %struct.ScmObj*, align 8
%k47424 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56096)
store volatile %struct.ScmObj* %k47424, %struct.ScmObj** %stackaddr$prim57146, align 8
%stackaddr$prim57147 = alloca %struct.ScmObj*, align 8
%current_45args56097 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56096)
store volatile %struct.ScmObj* %current_45args56097, %struct.ScmObj** %stackaddr$prim57147, align 8
%stackaddr$prim57148 = alloca %struct.ScmObj*, align 8
%u47168 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56097)
store volatile %struct.ScmObj* %u47168, %struct.ScmObj** %stackaddr$prim57148, align 8
%argslist56099$u471680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57149 = alloca %struct.ScmObj*, align 8
%argslist56099$u471681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u47168, %struct.ScmObj* %argslist56099$u471680)
store volatile %struct.ScmObj* %argslist56099$u471681, %struct.ScmObj** %stackaddr$prim57149, align 8
%stackaddr$prim57150 = alloca %struct.ScmObj*, align 8
%argslist56099$u471682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47424, %struct.ScmObj* %argslist56099$u471681)
store volatile %struct.ScmObj* %argslist56099$u471682, %struct.ScmObj** %stackaddr$prim57150, align 8
%clofunc57151 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u47168)
musttail call tailcc void %clofunc57151(%struct.ScmObj* %u47168, %struct.ScmObj* %argslist56099$u471682)
ret void
}

define tailcc void @proc_clo$ae49361(%struct.ScmObj* %env$ae49361,%struct.ScmObj* %current_45args56102) {
%stackaddr$prim57152 = alloca %struct.ScmObj*, align 8
%k47425 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56102)
store volatile %struct.ScmObj* %k47425, %struct.ScmObj** %stackaddr$prim57152, align 8
%stackaddr$prim57153 = alloca %struct.ScmObj*, align 8
%current_45args56103 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56102)
store volatile %struct.ScmObj* %current_45args56103, %struct.ScmObj** %stackaddr$prim57153, align 8
%stackaddr$prim57154 = alloca %struct.ScmObj*, align 8
%a47172 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56103)
store volatile %struct.ScmObj* %a47172, %struct.ScmObj** %stackaddr$prim57154, align 8
%ae49362 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57155 = alloca %struct.ScmObj*, align 8
%a47173 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49362, %struct.ScmObj* %a47172)
store volatile %struct.ScmObj* %a47173, %struct.ScmObj** %stackaddr$prim57155, align 8
%stackaddr$makeclosure57156 = alloca %struct.ScmObj*, align 8
%fptrToInt57157 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49364 to i64
%ae49364 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57157)
store volatile %struct.ScmObj* %ae49364, %struct.ScmObj** %stackaddr$makeclosure57156, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49364, %struct.ScmObj* %a47173, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49364, %struct.ScmObj* %k47425, i64 1)
%ae49365 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57158 = alloca %struct.ScmObj*, align 8
%fptrToInt57159 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49366 to i64
%ae49366 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57159)
store volatile %struct.ScmObj* %ae49366, %struct.ScmObj** %stackaddr$makeclosure57158, align 8
%argslist56125$ae493640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57160 = alloca %struct.ScmObj*, align 8
%argslist56125$ae493641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49366, %struct.ScmObj* %argslist56125$ae493640)
store volatile %struct.ScmObj* %argslist56125$ae493641, %struct.ScmObj** %stackaddr$prim57160, align 8
%stackaddr$prim57161 = alloca %struct.ScmObj*, align 8
%argslist56125$ae493642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49365, %struct.ScmObj* %argslist56125$ae493641)
store volatile %struct.ScmObj* %argslist56125$ae493642, %struct.ScmObj** %stackaddr$prim57161, align 8
%clofunc57162 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49364)
musttail call tailcc void %clofunc57162(%struct.ScmObj* %ae49364, %struct.ScmObj* %argslist56125$ae493642)
ret void
}

define tailcc void @proc_clo$ae49364(%struct.ScmObj* %env$ae49364,%struct.ScmObj* %current_45args56105) {
%stackaddr$env-ref57163 = alloca %struct.ScmObj*, align 8
%a47173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49364, i64 0)
store %struct.ScmObj* %a47173, %struct.ScmObj** %stackaddr$env-ref57163
%stackaddr$env-ref57164 = alloca %struct.ScmObj*, align 8
%k47425 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49364, i64 1)
store %struct.ScmObj* %k47425, %struct.ScmObj** %stackaddr$env-ref57164
%stackaddr$prim57165 = alloca %struct.ScmObj*, align 8
%_95k47426 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56105)
store volatile %struct.ScmObj* %_95k47426, %struct.ScmObj** %stackaddr$prim57165, align 8
%stackaddr$prim57166 = alloca %struct.ScmObj*, align 8
%current_45args56106 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56105)
store volatile %struct.ScmObj* %current_45args56106, %struct.ScmObj** %stackaddr$prim57166, align 8
%stackaddr$prim57167 = alloca %struct.ScmObj*, align 8
%anf_45bind47316 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56106)
store volatile %struct.ScmObj* %anf_45bind47316, %struct.ScmObj** %stackaddr$prim57167, align 8
%stackaddr$makeclosure57168 = alloca %struct.ScmObj*, align 8
%fptrToInt57169 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49383 to i64
%ae49383 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57169)
store volatile %struct.ScmObj* %ae49383, %struct.ScmObj** %stackaddr$makeclosure57168, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49383, %struct.ScmObj* %a47173, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49383, %struct.ScmObj* %k47425, i64 1)
%stackaddr$makeclosure57170 = alloca %struct.ScmObj*, align 8
%fptrToInt57171 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49384 to i64
%ae49384 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57171)
store volatile %struct.ScmObj* %ae49384, %struct.ScmObj** %stackaddr$makeclosure57170, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49384, %struct.ScmObj* %a47173, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49384, %struct.ScmObj* %k47425, i64 1)
%argslist56120$anf_45bind473160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57172 = alloca %struct.ScmObj*, align 8
%argslist56120$anf_45bind473161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49384, %struct.ScmObj* %argslist56120$anf_45bind473160)
store volatile %struct.ScmObj* %argslist56120$anf_45bind473161, %struct.ScmObj** %stackaddr$prim57172, align 8
%stackaddr$prim57173 = alloca %struct.ScmObj*, align 8
%argslist56120$anf_45bind473162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49383, %struct.ScmObj* %argslist56120$anf_45bind473161)
store volatile %struct.ScmObj* %argslist56120$anf_45bind473162, %struct.ScmObj** %stackaddr$prim57173, align 8
%clofunc57174 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47316)
musttail call tailcc void %clofunc57174(%struct.ScmObj* %anf_45bind47316, %struct.ScmObj* %argslist56120$anf_45bind473162)
ret void
}

define tailcc void @proc_clo$ae49383(%struct.ScmObj* %env$ae49383,%struct.ScmObj* %current_45args56108) {
%stackaddr$env-ref57175 = alloca %struct.ScmObj*, align 8
%a47173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49383, i64 0)
store %struct.ScmObj* %a47173, %struct.ScmObj** %stackaddr$env-ref57175
%stackaddr$env-ref57176 = alloca %struct.ScmObj*, align 8
%k47425 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49383, i64 1)
store %struct.ScmObj* %k47425, %struct.ScmObj** %stackaddr$env-ref57176
%stackaddr$prim57177 = alloca %struct.ScmObj*, align 8
%_95k47427 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56108)
store volatile %struct.ScmObj* %_95k47427, %struct.ScmObj** %stackaddr$prim57177, align 8
%stackaddr$prim57178 = alloca %struct.ScmObj*, align 8
%current_45args56109 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56108)
store volatile %struct.ScmObj* %current_45args56109, %struct.ScmObj** %stackaddr$prim57178, align 8
%stackaddr$prim57179 = alloca %struct.ScmObj*, align 8
%cc47174 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56109)
store volatile %struct.ScmObj* %cc47174, %struct.ScmObj** %stackaddr$prim57179, align 8
%ae49499 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57180 = alloca %struct.ScmObj*, align 8
%anf_45bind47317 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47173, %struct.ScmObj* %ae49499)
store volatile %struct.ScmObj* %anf_45bind47317, %struct.ScmObj** %stackaddr$prim57180, align 8
%stackaddr$prim57181 = alloca %struct.ScmObj*, align 8
%anf_45bind47318 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47317)
store volatile %struct.ScmObj* %anf_45bind47318, %struct.ScmObj** %stackaddr$prim57181, align 8
%truthy$cmp57182 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47318)
%cmp$cmp57182 = icmp eq i64 %truthy$cmp57182, 1
br i1 %cmp$cmp57182, label %truebranch$cmp57182, label %falsebranch$cmp57182
truebranch$cmp57182:
%ae49503 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49504 = call %struct.ScmObj* @const_init_true()
%argslist56111$k474250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57183 = alloca %struct.ScmObj*, align 8
%argslist56111$k474251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49504, %struct.ScmObj* %argslist56111$k474250)
store volatile %struct.ScmObj* %argslist56111$k474251, %struct.ScmObj** %stackaddr$prim57183, align 8
%stackaddr$prim57184 = alloca %struct.ScmObj*, align 8
%argslist56111$k474252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49503, %struct.ScmObj* %argslist56111$k474251)
store volatile %struct.ScmObj* %argslist56111$k474252, %struct.ScmObj** %stackaddr$prim57184, align 8
%clofunc57185 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47425)
musttail call tailcc void %clofunc57185(%struct.ScmObj* %k47425, %struct.ScmObj* %argslist56111$k474252)
ret void
falsebranch$cmp57182:
%ae49512 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57186 = alloca %struct.ScmObj*, align 8
%anf_45bind47319 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47173, %struct.ScmObj* %ae49512)
store volatile %struct.ScmObj* %anf_45bind47319, %struct.ScmObj** %stackaddr$prim57186, align 8
%stackaddr$prim57187 = alloca %struct.ScmObj*, align 8
%anf_45bind47320 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind47319)
store volatile %struct.ScmObj* %anf_45bind47320, %struct.ScmObj** %stackaddr$prim57187, align 8
%truthy$cmp57188 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47320)
%cmp$cmp57188 = icmp eq i64 %truthy$cmp57188, 1
br i1 %cmp$cmp57188, label %truebranch$cmp57188, label %falsebranch$cmp57188
truebranch$cmp57188:
%ae49516 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57189 = alloca %struct.ScmObj*, align 8
%anf_45bind47321 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47173, %struct.ScmObj* %ae49516)
store volatile %struct.ScmObj* %anf_45bind47321, %struct.ScmObj** %stackaddr$prim57189, align 8
%stackaddr$prim57190 = alloca %struct.ScmObj*, align 8
%b47176 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47321)
store volatile %struct.ScmObj* %b47176, %struct.ScmObj** %stackaddr$prim57190, align 8
%ae49519 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57191 = alloca %struct.ScmObj*, align 8
%anf_45bind47322 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47173, %struct.ScmObj* %ae49519)
store volatile %struct.ScmObj* %anf_45bind47322, %struct.ScmObj** %stackaddr$prim57191, align 8
%stackaddr$prim57192 = alloca %struct.ScmObj*, align 8
%anf_45bind47323 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47322)
store volatile %struct.ScmObj* %anf_45bind47323, %struct.ScmObj** %stackaddr$prim57192, align 8
%ae49522 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57193 = alloca %struct.ScmObj*, align 8
%_95047177 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a47173, %struct.ScmObj* %ae49522, %struct.ScmObj* %anf_45bind47323)
store volatile %struct.ScmObj* %_95047177, %struct.ScmObj** %stackaddr$prim57193, align 8
%argslist56112$cc471740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57194 = alloca %struct.ScmObj*, align 8
%argslist56112$cc471741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47174, %struct.ScmObj* %argslist56112$cc471740)
store volatile %struct.ScmObj* %argslist56112$cc471741, %struct.ScmObj** %stackaddr$prim57194, align 8
%stackaddr$prim57195 = alloca %struct.ScmObj*, align 8
%argslist56112$cc471742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47425, %struct.ScmObj* %argslist56112$cc471741)
store volatile %struct.ScmObj* %argslist56112$cc471742, %struct.ScmObj** %stackaddr$prim57195, align 8
%clofunc57196 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47174)
musttail call tailcc void %clofunc57196(%struct.ScmObj* %cc47174, %struct.ScmObj* %argslist56112$cc471742)
ret void
falsebranch$cmp57188:
%ae49555 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49556 = call %struct.ScmObj* @const_init_false()
%argslist56113$k474250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57197 = alloca %struct.ScmObj*, align 8
%argslist56113$k474251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49556, %struct.ScmObj* %argslist56113$k474250)
store volatile %struct.ScmObj* %argslist56113$k474251, %struct.ScmObj** %stackaddr$prim57197, align 8
%stackaddr$prim57198 = alloca %struct.ScmObj*, align 8
%argslist56113$k474252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49555, %struct.ScmObj* %argslist56113$k474251)
store volatile %struct.ScmObj* %argslist56113$k474252, %struct.ScmObj** %stackaddr$prim57198, align 8
%clofunc57199 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47425)
musttail call tailcc void %clofunc57199(%struct.ScmObj* %k47425, %struct.ScmObj* %argslist56113$k474252)
ret void
}

define tailcc void @proc_clo$ae49384(%struct.ScmObj* %env$ae49384,%struct.ScmObj* %current_45args56114) {
%stackaddr$env-ref57200 = alloca %struct.ScmObj*, align 8
%a47173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49384, i64 0)
store %struct.ScmObj* %a47173, %struct.ScmObj** %stackaddr$env-ref57200
%stackaddr$env-ref57201 = alloca %struct.ScmObj*, align 8
%k47425 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49384, i64 1)
store %struct.ScmObj* %k47425, %struct.ScmObj** %stackaddr$env-ref57201
%stackaddr$prim57202 = alloca %struct.ScmObj*, align 8
%_95k47427 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56114)
store volatile %struct.ScmObj* %_95k47427, %struct.ScmObj** %stackaddr$prim57202, align 8
%stackaddr$prim57203 = alloca %struct.ScmObj*, align 8
%current_45args56115 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56114)
store volatile %struct.ScmObj* %current_45args56115, %struct.ScmObj** %stackaddr$prim57203, align 8
%stackaddr$prim57204 = alloca %struct.ScmObj*, align 8
%cc47174 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56115)
store volatile %struct.ScmObj* %cc47174, %struct.ScmObj** %stackaddr$prim57204, align 8
%ae49386 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57205 = alloca %struct.ScmObj*, align 8
%anf_45bind47317 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47173, %struct.ScmObj* %ae49386)
store volatile %struct.ScmObj* %anf_45bind47317, %struct.ScmObj** %stackaddr$prim57205, align 8
%stackaddr$prim57206 = alloca %struct.ScmObj*, align 8
%anf_45bind47318 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47317)
store volatile %struct.ScmObj* %anf_45bind47318, %struct.ScmObj** %stackaddr$prim57206, align 8
%truthy$cmp57207 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47318)
%cmp$cmp57207 = icmp eq i64 %truthy$cmp57207, 1
br i1 %cmp$cmp57207, label %truebranch$cmp57207, label %falsebranch$cmp57207
truebranch$cmp57207:
%ae49390 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49391 = call %struct.ScmObj* @const_init_true()
%argslist56117$k474250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57208 = alloca %struct.ScmObj*, align 8
%argslist56117$k474251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49391, %struct.ScmObj* %argslist56117$k474250)
store volatile %struct.ScmObj* %argslist56117$k474251, %struct.ScmObj** %stackaddr$prim57208, align 8
%stackaddr$prim57209 = alloca %struct.ScmObj*, align 8
%argslist56117$k474252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49390, %struct.ScmObj* %argslist56117$k474251)
store volatile %struct.ScmObj* %argslist56117$k474252, %struct.ScmObj** %stackaddr$prim57209, align 8
%clofunc57210 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47425)
musttail call tailcc void %clofunc57210(%struct.ScmObj* %k47425, %struct.ScmObj* %argslist56117$k474252)
ret void
falsebranch$cmp57207:
%ae49399 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57211 = alloca %struct.ScmObj*, align 8
%anf_45bind47319 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47173, %struct.ScmObj* %ae49399)
store volatile %struct.ScmObj* %anf_45bind47319, %struct.ScmObj** %stackaddr$prim57211, align 8
%stackaddr$prim57212 = alloca %struct.ScmObj*, align 8
%anf_45bind47320 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind47319)
store volatile %struct.ScmObj* %anf_45bind47320, %struct.ScmObj** %stackaddr$prim57212, align 8
%truthy$cmp57213 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47320)
%cmp$cmp57213 = icmp eq i64 %truthy$cmp57213, 1
br i1 %cmp$cmp57213, label %truebranch$cmp57213, label %falsebranch$cmp57213
truebranch$cmp57213:
%ae49403 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57214 = alloca %struct.ScmObj*, align 8
%anf_45bind47321 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47173, %struct.ScmObj* %ae49403)
store volatile %struct.ScmObj* %anf_45bind47321, %struct.ScmObj** %stackaddr$prim57214, align 8
%stackaddr$prim57215 = alloca %struct.ScmObj*, align 8
%b47176 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47321)
store volatile %struct.ScmObj* %b47176, %struct.ScmObj** %stackaddr$prim57215, align 8
%ae49406 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57216 = alloca %struct.ScmObj*, align 8
%anf_45bind47322 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47173, %struct.ScmObj* %ae49406)
store volatile %struct.ScmObj* %anf_45bind47322, %struct.ScmObj** %stackaddr$prim57216, align 8
%stackaddr$prim57217 = alloca %struct.ScmObj*, align 8
%anf_45bind47323 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47322)
store volatile %struct.ScmObj* %anf_45bind47323, %struct.ScmObj** %stackaddr$prim57217, align 8
%ae49409 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57218 = alloca %struct.ScmObj*, align 8
%_95047177 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a47173, %struct.ScmObj* %ae49409, %struct.ScmObj* %anf_45bind47323)
store volatile %struct.ScmObj* %_95047177, %struct.ScmObj** %stackaddr$prim57218, align 8
%argslist56118$cc471740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57219 = alloca %struct.ScmObj*, align 8
%argslist56118$cc471741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47174, %struct.ScmObj* %argslist56118$cc471740)
store volatile %struct.ScmObj* %argslist56118$cc471741, %struct.ScmObj** %stackaddr$prim57219, align 8
%stackaddr$prim57220 = alloca %struct.ScmObj*, align 8
%argslist56118$cc471742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47425, %struct.ScmObj* %argslist56118$cc471741)
store volatile %struct.ScmObj* %argslist56118$cc471742, %struct.ScmObj** %stackaddr$prim57220, align 8
%clofunc57221 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47174)
musttail call tailcc void %clofunc57221(%struct.ScmObj* %cc47174, %struct.ScmObj* %argslist56118$cc471742)
ret void
falsebranch$cmp57213:
%ae49442 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49443 = call %struct.ScmObj* @const_init_false()
%argslist56119$k474250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57222 = alloca %struct.ScmObj*, align 8
%argslist56119$k474251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49443, %struct.ScmObj* %argslist56119$k474250)
store volatile %struct.ScmObj* %argslist56119$k474251, %struct.ScmObj** %stackaddr$prim57222, align 8
%stackaddr$prim57223 = alloca %struct.ScmObj*, align 8
%argslist56119$k474252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49442, %struct.ScmObj* %argslist56119$k474251)
store volatile %struct.ScmObj* %argslist56119$k474252, %struct.ScmObj** %stackaddr$prim57223, align 8
%clofunc57224 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47425)
musttail call tailcc void %clofunc57224(%struct.ScmObj* %k47425, %struct.ScmObj* %argslist56119$k474252)
ret void
}

define tailcc void @proc_clo$ae49366(%struct.ScmObj* %env$ae49366,%struct.ScmObj* %current_45args56121) {
%stackaddr$prim57225 = alloca %struct.ScmObj*, align 8
%k47428 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56121)
store volatile %struct.ScmObj* %k47428, %struct.ScmObj** %stackaddr$prim57225, align 8
%stackaddr$prim57226 = alloca %struct.ScmObj*, align 8
%current_45args56122 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56121)
store volatile %struct.ScmObj* %current_45args56122, %struct.ScmObj** %stackaddr$prim57226, align 8
%stackaddr$prim57227 = alloca %struct.ScmObj*, align 8
%k47175 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56122)
store volatile %struct.ScmObj* %k47175, %struct.ScmObj** %stackaddr$prim57227, align 8
%ae49368 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56124$k474280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57228 = alloca %struct.ScmObj*, align 8
%argslist56124$k474281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47175, %struct.ScmObj* %argslist56124$k474280)
store volatile %struct.ScmObj* %argslist56124$k474281, %struct.ScmObj** %stackaddr$prim57228, align 8
%stackaddr$prim57229 = alloca %struct.ScmObj*, align 8
%argslist56124$k474282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49368, %struct.ScmObj* %argslist56124$k474281)
store volatile %struct.ScmObj* %argslist56124$k474282, %struct.ScmObj** %stackaddr$prim57229, align 8
%clofunc57230 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47428)
musttail call tailcc void %clofunc57230(%struct.ScmObj* %k47428, %struct.ScmObj* %argslist56124$k474282)
ret void
}

define tailcc void @proc_clo$ae49289(%struct.ScmObj* %env$ae49289,%struct.ScmObj* %current_45args56127) {
%stackaddr$env-ref57231 = alloca %struct.ScmObj*, align 8
%_37append47179 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49289, i64 0)
store %struct.ScmObj* %_37append47179, %struct.ScmObj** %stackaddr$env-ref57231
%stackaddr$prim57232 = alloca %struct.ScmObj*, align 8
%k47429 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56127)
store volatile %struct.ScmObj* %k47429, %struct.ScmObj** %stackaddr$prim57232, align 8
%stackaddr$prim57233 = alloca %struct.ScmObj*, align 8
%current_45args56128 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56127)
store volatile %struct.ScmObj* %current_45args56128, %struct.ScmObj** %stackaddr$prim57233, align 8
%stackaddr$prim57234 = alloca %struct.ScmObj*, align 8
%ls047182 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56128)
store volatile %struct.ScmObj* %ls047182, %struct.ScmObj** %stackaddr$prim57234, align 8
%stackaddr$prim57235 = alloca %struct.ScmObj*, align 8
%current_45args56129 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56128)
store volatile %struct.ScmObj* %current_45args56129, %struct.ScmObj** %stackaddr$prim57235, align 8
%stackaddr$prim57236 = alloca %struct.ScmObj*, align 8
%ls147181 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56129)
store volatile %struct.ScmObj* %ls147181, %struct.ScmObj** %stackaddr$prim57236, align 8
%stackaddr$prim57237 = alloca %struct.ScmObj*, align 8
%anf_45bind47310 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls047182)
store volatile %struct.ScmObj* %anf_45bind47310, %struct.ScmObj** %stackaddr$prim57237, align 8
%truthy$cmp57238 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47310)
%cmp$cmp57238 = icmp eq i64 %truthy$cmp57238, 1
br i1 %cmp$cmp57238, label %truebranch$cmp57238, label %falsebranch$cmp57238
truebranch$cmp57238:
%ae49293 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56131$k474290 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57239 = alloca %struct.ScmObj*, align 8
%argslist56131$k474291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls147181, %struct.ScmObj* %argslist56131$k474290)
store volatile %struct.ScmObj* %argslist56131$k474291, %struct.ScmObj** %stackaddr$prim57239, align 8
%stackaddr$prim57240 = alloca %struct.ScmObj*, align 8
%argslist56131$k474292 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49293, %struct.ScmObj* %argslist56131$k474291)
store volatile %struct.ScmObj* %argslist56131$k474292, %struct.ScmObj** %stackaddr$prim57240, align 8
%clofunc57241 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47429)
musttail call tailcc void %clofunc57241(%struct.ScmObj* %k47429, %struct.ScmObj* %argslist56131$k474292)
ret void
falsebranch$cmp57238:
%stackaddr$prim57242 = alloca %struct.ScmObj*, align 8
%anf_45bind47311 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls047182)
store volatile %struct.ScmObj* %anf_45bind47311, %struct.ScmObj** %stackaddr$prim57242, align 8
%ae49300 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57243 = alloca %struct.ScmObj*, align 8
%anf_45bind47312 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append47179, %struct.ScmObj* %ae49300)
store volatile %struct.ScmObj* %anf_45bind47312, %struct.ScmObj** %stackaddr$prim57243, align 8
%stackaddr$prim57244 = alloca %struct.ScmObj*, align 8
%anf_45bind47313 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls047182)
store volatile %struct.ScmObj* %anf_45bind47313, %struct.ScmObj** %stackaddr$prim57244, align 8
%stackaddr$makeclosure57245 = alloca %struct.ScmObj*, align 8
%fptrToInt57246 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49303 to i64
%ae49303 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57246)
store volatile %struct.ScmObj* %ae49303, %struct.ScmObj** %stackaddr$makeclosure57245, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49303, %struct.ScmObj* %k47429, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49303, %struct.ScmObj* %anf_45bind47311, i64 1)
%argslist56136$anf_45bind473120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57247 = alloca %struct.ScmObj*, align 8
%argslist56136$anf_45bind473121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls147181, %struct.ScmObj* %argslist56136$anf_45bind473120)
store volatile %struct.ScmObj* %argslist56136$anf_45bind473121, %struct.ScmObj** %stackaddr$prim57247, align 8
%stackaddr$prim57248 = alloca %struct.ScmObj*, align 8
%argslist56136$anf_45bind473122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47313, %struct.ScmObj* %argslist56136$anf_45bind473121)
store volatile %struct.ScmObj* %argslist56136$anf_45bind473122, %struct.ScmObj** %stackaddr$prim57248, align 8
%stackaddr$prim57249 = alloca %struct.ScmObj*, align 8
%argslist56136$anf_45bind473123 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49303, %struct.ScmObj* %argslist56136$anf_45bind473122)
store volatile %struct.ScmObj* %argslist56136$anf_45bind473123, %struct.ScmObj** %stackaddr$prim57249, align 8
%clofunc57250 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47312)
musttail call tailcc void %clofunc57250(%struct.ScmObj* %anf_45bind47312, %struct.ScmObj* %argslist56136$anf_45bind473123)
ret void
}

define tailcc void @proc_clo$ae49303(%struct.ScmObj* %env$ae49303,%struct.ScmObj* %current_45args56132) {
%stackaddr$env-ref57251 = alloca %struct.ScmObj*, align 8
%k47429 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49303, i64 0)
store %struct.ScmObj* %k47429, %struct.ScmObj** %stackaddr$env-ref57251
%stackaddr$env-ref57252 = alloca %struct.ScmObj*, align 8
%anf_45bind47311 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49303, i64 1)
store %struct.ScmObj* %anf_45bind47311, %struct.ScmObj** %stackaddr$env-ref57252
%stackaddr$prim57253 = alloca %struct.ScmObj*, align 8
%_95k47430 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56132)
store volatile %struct.ScmObj* %_95k47430, %struct.ScmObj** %stackaddr$prim57253, align 8
%stackaddr$prim57254 = alloca %struct.ScmObj*, align 8
%current_45args56133 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56132)
store volatile %struct.ScmObj* %current_45args56133, %struct.ScmObj** %stackaddr$prim57254, align 8
%stackaddr$prim57255 = alloca %struct.ScmObj*, align 8
%anf_45bind47314 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56133)
store volatile %struct.ScmObj* %anf_45bind47314, %struct.ScmObj** %stackaddr$prim57255, align 8
%stackaddr$prim57256 = alloca %struct.ScmObj*, align 8
%cpsprim47431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47311, %struct.ScmObj* %anf_45bind47314)
store volatile %struct.ScmObj* %cpsprim47431, %struct.ScmObj** %stackaddr$prim57256, align 8
%ae49309 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56135$k474290 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57257 = alloca %struct.ScmObj*, align 8
%argslist56135$k474291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47431, %struct.ScmObj* %argslist56135$k474290)
store volatile %struct.ScmObj* %argslist56135$k474291, %struct.ScmObj** %stackaddr$prim57257, align 8
%stackaddr$prim57258 = alloca %struct.ScmObj*, align 8
%argslist56135$k474292 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49309, %struct.ScmObj* %argslist56135$k474291)
store volatile %struct.ScmObj* %argslist56135$k474292, %struct.ScmObj** %stackaddr$prim57258, align 8
%clofunc57259 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47429)
musttail call tailcc void %clofunc57259(%struct.ScmObj* %k47429, %struct.ScmObj* %argslist56135$k474292)
ret void
}

define tailcc void @proc_clo$ae49263(%struct.ScmObj* %env$ae49263,%struct.ScmObj* %current_45args56138) {
%stackaddr$prim57260 = alloca %struct.ScmObj*, align 8
%k47432 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56138)
store volatile %struct.ScmObj* %k47432, %struct.ScmObj** %stackaddr$prim57260, align 8
%stackaddr$prim57261 = alloca %struct.ScmObj*, align 8
%current_45args56139 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56138)
store volatile %struct.ScmObj* %current_45args56139, %struct.ScmObj** %stackaddr$prim57261, align 8
%stackaddr$prim57262 = alloca %struct.ScmObj*, align 8
%a47185 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56139)
store volatile %struct.ScmObj* %a47185, %struct.ScmObj** %stackaddr$prim57262, align 8
%stackaddr$prim57263 = alloca %struct.ScmObj*, align 8
%current_45args56140 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56139)
store volatile %struct.ScmObj* %current_45args56140, %struct.ScmObj** %stackaddr$prim57263, align 8
%stackaddr$prim57264 = alloca %struct.ScmObj*, align 8
%b47184 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56140)
store volatile %struct.ScmObj* %b47184, %struct.ScmObj** %stackaddr$prim57264, align 8
%stackaddr$prim57265 = alloca %struct.ScmObj*, align 8
%anf_45bind47309 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a47185, %struct.ScmObj* %b47184)
store volatile %struct.ScmObj* %anf_45bind47309, %struct.ScmObj** %stackaddr$prim57265, align 8
%stackaddr$prim57266 = alloca %struct.ScmObj*, align 8
%cpsprim47433 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind47309)
store volatile %struct.ScmObj* %cpsprim47433, %struct.ScmObj** %stackaddr$prim57266, align 8
%ae49268 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56142$k474320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57267 = alloca %struct.ScmObj*, align 8
%argslist56142$k474321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47433, %struct.ScmObj* %argslist56142$k474320)
store volatile %struct.ScmObj* %argslist56142$k474321, %struct.ScmObj** %stackaddr$prim57267, align 8
%stackaddr$prim57268 = alloca %struct.ScmObj*, align 8
%argslist56142$k474322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49268, %struct.ScmObj* %argslist56142$k474321)
store volatile %struct.ScmObj* %argslist56142$k474322, %struct.ScmObj** %stackaddr$prim57268, align 8
%clofunc57269 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47432)
musttail call tailcc void %clofunc57269(%struct.ScmObj* %k47432, %struct.ScmObj* %argslist56142$k474322)
ret void
}

define tailcc void @proc_clo$ae49239(%struct.ScmObj* %env$ae49239,%struct.ScmObj* %current_45args56144) {
%stackaddr$prim57270 = alloca %struct.ScmObj*, align 8
%k47434 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56144)
store volatile %struct.ScmObj* %k47434, %struct.ScmObj** %stackaddr$prim57270, align 8
%stackaddr$prim57271 = alloca %struct.ScmObj*, align 8
%current_45args56145 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56144)
store volatile %struct.ScmObj* %current_45args56145, %struct.ScmObj** %stackaddr$prim57271, align 8
%stackaddr$prim57272 = alloca %struct.ScmObj*, align 8
%a47188 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56145)
store volatile %struct.ScmObj* %a47188, %struct.ScmObj** %stackaddr$prim57272, align 8
%stackaddr$prim57273 = alloca %struct.ScmObj*, align 8
%current_45args56146 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56145)
store volatile %struct.ScmObj* %current_45args56146, %struct.ScmObj** %stackaddr$prim57273, align 8
%stackaddr$prim57274 = alloca %struct.ScmObj*, align 8
%b47187 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56146)
store volatile %struct.ScmObj* %b47187, %struct.ScmObj** %stackaddr$prim57274, align 8
%stackaddr$prim57275 = alloca %struct.ScmObj*, align 8
%anf_45bind47308 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a47188, %struct.ScmObj* %b47187)
store volatile %struct.ScmObj* %anf_45bind47308, %struct.ScmObj** %stackaddr$prim57275, align 8
%stackaddr$prim57276 = alloca %struct.ScmObj*, align 8
%cpsprim47435 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind47308)
store volatile %struct.ScmObj* %cpsprim47435, %struct.ScmObj** %stackaddr$prim57276, align 8
%ae49244 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56148$k474340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57277 = alloca %struct.ScmObj*, align 8
%argslist56148$k474341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47435, %struct.ScmObj* %argslist56148$k474340)
store volatile %struct.ScmObj* %argslist56148$k474341, %struct.ScmObj** %stackaddr$prim57277, align 8
%stackaddr$prim57278 = alloca %struct.ScmObj*, align 8
%argslist56148$k474342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49244, %struct.ScmObj* %argslist56148$k474341)
store volatile %struct.ScmObj* %argslist56148$k474342, %struct.ScmObj** %stackaddr$prim57278, align 8
%clofunc57279 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47434)
musttail call tailcc void %clofunc57279(%struct.ScmObj* %k47434, %struct.ScmObj* %argslist56148$k474342)
ret void
}

define tailcc void @proc_clo$ae48845(%struct.ScmObj* %env$ae48845,%struct.ScmObj* %current_45args56151) {
%stackaddr$env-ref57280 = alloca %struct.ScmObj*, align 8
%_37foldr147107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48845, i64 0)
store %struct.ScmObj* %_37foldr147107, %struct.ScmObj** %stackaddr$env-ref57280
%stackaddr$env-ref57281 = alloca %struct.ScmObj*, align 8
%_37map147138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48845, i64 1)
store %struct.ScmObj* %_37map147138, %struct.ScmObj** %stackaddr$env-ref57281
%stackaddr$env-ref57282 = alloca %struct.ScmObj*, align 8
%_37foldr47112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48845, i64 2)
store %struct.ScmObj* %_37foldr47112, %struct.ScmObj** %stackaddr$env-ref57282
%stackaddr$prim57283 = alloca %struct.ScmObj*, align 8
%k47436 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56151)
store volatile %struct.ScmObj* %k47436, %struct.ScmObj** %stackaddr$prim57283, align 8
%stackaddr$prim57284 = alloca %struct.ScmObj*, align 8
%current_45args56152 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56151)
store volatile %struct.ScmObj* %current_45args56152, %struct.ScmObj** %stackaddr$prim57284, align 8
%stackaddr$prim57285 = alloca %struct.ScmObj*, align 8
%_37foldl47190 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56152)
store volatile %struct.ScmObj* %_37foldl47190, %struct.ScmObj** %stackaddr$prim57285, align 8
%ae48847 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57286 = alloca %struct.ScmObj*, align 8
%fptrToInt57287 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48848 to i64
%ae48848 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57287)
store volatile %struct.ScmObj* %ae48848, %struct.ScmObj** %stackaddr$makeclosure57286, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48848, %struct.ScmObj* %_37foldl47190, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48848, %struct.ScmObj* %_37foldr147107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48848, %struct.ScmObj* %_37map147138, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48848, %struct.ScmObj* %_37foldr47112, i64 3)
%argslist56209$k474360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57288 = alloca %struct.ScmObj*, align 8
%argslist56209$k474361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48848, %struct.ScmObj* %argslist56209$k474360)
store volatile %struct.ScmObj* %argslist56209$k474361, %struct.ScmObj** %stackaddr$prim57288, align 8
%stackaddr$prim57289 = alloca %struct.ScmObj*, align 8
%argslist56209$k474362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48847, %struct.ScmObj* %argslist56209$k474361)
store volatile %struct.ScmObj* %argslist56209$k474362, %struct.ScmObj** %stackaddr$prim57289, align 8
%clofunc57290 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47436)
musttail call tailcc void %clofunc57290(%struct.ScmObj* %k47436, %struct.ScmObj* %argslist56209$k474362)
ret void
}

define tailcc void @proc_clo$ae48848(%struct.ScmObj* %env$ae48848,%struct.ScmObj* %args4719147437) {
%stackaddr$env-ref57291 = alloca %struct.ScmObj*, align 8
%_37foldl47190 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48848, i64 0)
store %struct.ScmObj* %_37foldl47190, %struct.ScmObj** %stackaddr$env-ref57291
%stackaddr$env-ref57292 = alloca %struct.ScmObj*, align 8
%_37foldr147107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48848, i64 1)
store %struct.ScmObj* %_37foldr147107, %struct.ScmObj** %stackaddr$env-ref57292
%stackaddr$env-ref57293 = alloca %struct.ScmObj*, align 8
%_37map147138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48848, i64 2)
store %struct.ScmObj* %_37map147138, %struct.ScmObj** %stackaddr$env-ref57293
%stackaddr$env-ref57294 = alloca %struct.ScmObj*, align 8
%_37foldr47112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48848, i64 3)
store %struct.ScmObj* %_37foldr47112, %struct.ScmObj** %stackaddr$env-ref57294
%stackaddr$prim57295 = alloca %struct.ScmObj*, align 8
%k47438 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4719147437)
store volatile %struct.ScmObj* %k47438, %struct.ScmObj** %stackaddr$prim57295, align 8
%stackaddr$prim57296 = alloca %struct.ScmObj*, align 8
%args47191 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4719147437)
store volatile %struct.ScmObj* %args47191, %struct.ScmObj** %stackaddr$prim57296, align 8
%stackaddr$prim57297 = alloca %struct.ScmObj*, align 8
%f47194 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47191)
store volatile %struct.ScmObj* %f47194, %struct.ScmObj** %stackaddr$prim57297, align 8
%stackaddr$prim57298 = alloca %struct.ScmObj*, align 8
%anf_45bind47296 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47191)
store volatile %struct.ScmObj* %anf_45bind47296, %struct.ScmObj** %stackaddr$prim57298, align 8
%stackaddr$prim57299 = alloca %struct.ScmObj*, align 8
%acc47193 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47296)
store volatile %struct.ScmObj* %acc47193, %struct.ScmObj** %stackaddr$prim57299, align 8
%stackaddr$prim57300 = alloca %struct.ScmObj*, align 8
%anf_45bind47297 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47191)
store volatile %struct.ScmObj* %anf_45bind47297, %struct.ScmObj** %stackaddr$prim57300, align 8
%stackaddr$prim57301 = alloca %struct.ScmObj*, align 8
%lsts47192 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47297)
store volatile %struct.ScmObj* %lsts47192, %struct.ScmObj** %stackaddr$prim57301, align 8
%stackaddr$makeclosure57302 = alloca %struct.ScmObj*, align 8
%fptrToInt57303 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48856 to i64
%ae48856 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt57303)
store volatile %struct.ScmObj* %ae48856, %struct.ScmObj** %stackaddr$makeclosure57302, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48856, %struct.ScmObj* %lsts47192, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48856, %struct.ScmObj* %_37foldr47112, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48856, %struct.ScmObj* %_37foldl47190, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48856, %struct.ScmObj* %_37foldr147107, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48856, %struct.ScmObj* %_37map147138, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48856, %struct.ScmObj* %k47438, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48856, %struct.ScmObj* %f47194, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae48856, %struct.ScmObj* %acc47193, i64 7)
%ae48857 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57304 = alloca %struct.ScmObj*, align 8
%fptrToInt57305 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48858 to i64
%ae48858 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57305)
store volatile %struct.ScmObj* %ae48858, %struct.ScmObj** %stackaddr$makeclosure57304, align 8
%argslist56208$ae488560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57306 = alloca %struct.ScmObj*, align 8
%argslist56208$ae488561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48858, %struct.ScmObj* %argslist56208$ae488560)
store volatile %struct.ScmObj* %argslist56208$ae488561, %struct.ScmObj** %stackaddr$prim57306, align 8
%stackaddr$prim57307 = alloca %struct.ScmObj*, align 8
%argslist56208$ae488562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48857, %struct.ScmObj* %argslist56208$ae488561)
store volatile %struct.ScmObj* %argslist56208$ae488562, %struct.ScmObj** %stackaddr$prim57307, align 8
%clofunc57308 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48856)
musttail call tailcc void %clofunc57308(%struct.ScmObj* %ae48856, %struct.ScmObj* %argslist56208$ae488562)
ret void
}

define tailcc void @proc_clo$ae48856(%struct.ScmObj* %env$ae48856,%struct.ScmObj* %current_45args56154) {
%stackaddr$env-ref57309 = alloca %struct.ScmObj*, align 8
%lsts47192 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48856, i64 0)
store %struct.ScmObj* %lsts47192, %struct.ScmObj** %stackaddr$env-ref57309
%stackaddr$env-ref57310 = alloca %struct.ScmObj*, align 8
%_37foldr47112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48856, i64 1)
store %struct.ScmObj* %_37foldr47112, %struct.ScmObj** %stackaddr$env-ref57310
%stackaddr$env-ref57311 = alloca %struct.ScmObj*, align 8
%_37foldl47190 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48856, i64 2)
store %struct.ScmObj* %_37foldl47190, %struct.ScmObj** %stackaddr$env-ref57311
%stackaddr$env-ref57312 = alloca %struct.ScmObj*, align 8
%_37foldr147107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48856, i64 3)
store %struct.ScmObj* %_37foldr147107, %struct.ScmObj** %stackaddr$env-ref57312
%stackaddr$env-ref57313 = alloca %struct.ScmObj*, align 8
%_37map147138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48856, i64 4)
store %struct.ScmObj* %_37map147138, %struct.ScmObj** %stackaddr$env-ref57313
%stackaddr$env-ref57314 = alloca %struct.ScmObj*, align 8
%k47438 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48856, i64 5)
store %struct.ScmObj* %k47438, %struct.ScmObj** %stackaddr$env-ref57314
%stackaddr$env-ref57315 = alloca %struct.ScmObj*, align 8
%f47194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48856, i64 6)
store %struct.ScmObj* %f47194, %struct.ScmObj** %stackaddr$env-ref57315
%stackaddr$env-ref57316 = alloca %struct.ScmObj*, align 8
%acc47193 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48856, i64 7)
store %struct.ScmObj* %acc47193, %struct.ScmObj** %stackaddr$env-ref57316
%stackaddr$prim57317 = alloca %struct.ScmObj*, align 8
%_95k47439 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56154)
store volatile %struct.ScmObj* %_95k47439, %struct.ScmObj** %stackaddr$prim57317, align 8
%stackaddr$prim57318 = alloca %struct.ScmObj*, align 8
%current_45args56155 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56154)
store volatile %struct.ScmObj* %current_45args56155, %struct.ScmObj** %stackaddr$prim57318, align 8
%stackaddr$prim57319 = alloca %struct.ScmObj*, align 8
%anf_45bind47298 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56155)
store volatile %struct.ScmObj* %anf_45bind47298, %struct.ScmObj** %stackaddr$prim57319, align 8
%stackaddr$makeclosure57320 = alloca %struct.ScmObj*, align 8
%fptrToInt57321 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48888 to i64
%ae48888 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt57321)
store volatile %struct.ScmObj* %ae48888, %struct.ScmObj** %stackaddr$makeclosure57320, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48888, %struct.ScmObj* %lsts47192, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48888, %struct.ScmObj* %_37foldr47112, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48888, %struct.ScmObj* %_37foldl47190, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48888, %struct.ScmObj* %_37map147138, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48888, %struct.ScmObj* %k47438, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48888, %struct.ScmObj* %f47194, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48888, %struct.ScmObj* %acc47193, i64 6)
%ae48890 = call %struct.ScmObj* @const_init_false()
%argslist56201$_37foldr1471070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57322 = alloca %struct.ScmObj*, align 8
%argslist56201$_37foldr1471071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47192, %struct.ScmObj* %argslist56201$_37foldr1471070)
store volatile %struct.ScmObj* %argslist56201$_37foldr1471071, %struct.ScmObj** %stackaddr$prim57322, align 8
%stackaddr$prim57323 = alloca %struct.ScmObj*, align 8
%argslist56201$_37foldr1471072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48890, %struct.ScmObj* %argslist56201$_37foldr1471071)
store volatile %struct.ScmObj* %argslist56201$_37foldr1471072, %struct.ScmObj** %stackaddr$prim57323, align 8
%stackaddr$prim57324 = alloca %struct.ScmObj*, align 8
%argslist56201$_37foldr1471073 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47298, %struct.ScmObj* %argslist56201$_37foldr1471072)
store volatile %struct.ScmObj* %argslist56201$_37foldr1471073, %struct.ScmObj** %stackaddr$prim57324, align 8
%stackaddr$prim57325 = alloca %struct.ScmObj*, align 8
%argslist56201$_37foldr1471074 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48888, %struct.ScmObj* %argslist56201$_37foldr1471073)
store volatile %struct.ScmObj* %argslist56201$_37foldr1471074, %struct.ScmObj** %stackaddr$prim57325, align 8
%clofunc57326 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147107)
musttail call tailcc void %clofunc57326(%struct.ScmObj* %_37foldr147107, %struct.ScmObj* %argslist56201$_37foldr1471074)
ret void
}

define tailcc void @proc_clo$ae48888(%struct.ScmObj* %env$ae48888,%struct.ScmObj* %current_45args56157) {
%stackaddr$env-ref57327 = alloca %struct.ScmObj*, align 8
%lsts47192 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48888, i64 0)
store %struct.ScmObj* %lsts47192, %struct.ScmObj** %stackaddr$env-ref57327
%stackaddr$env-ref57328 = alloca %struct.ScmObj*, align 8
%_37foldr47112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48888, i64 1)
store %struct.ScmObj* %_37foldr47112, %struct.ScmObj** %stackaddr$env-ref57328
%stackaddr$env-ref57329 = alloca %struct.ScmObj*, align 8
%_37foldl47190 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48888, i64 2)
store %struct.ScmObj* %_37foldl47190, %struct.ScmObj** %stackaddr$env-ref57329
%stackaddr$env-ref57330 = alloca %struct.ScmObj*, align 8
%_37map147138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48888, i64 3)
store %struct.ScmObj* %_37map147138, %struct.ScmObj** %stackaddr$env-ref57330
%stackaddr$env-ref57331 = alloca %struct.ScmObj*, align 8
%k47438 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48888, i64 4)
store %struct.ScmObj* %k47438, %struct.ScmObj** %stackaddr$env-ref57331
%stackaddr$env-ref57332 = alloca %struct.ScmObj*, align 8
%f47194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48888, i64 5)
store %struct.ScmObj* %f47194, %struct.ScmObj** %stackaddr$env-ref57332
%stackaddr$env-ref57333 = alloca %struct.ScmObj*, align 8
%acc47193 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48888, i64 6)
store %struct.ScmObj* %acc47193, %struct.ScmObj** %stackaddr$env-ref57333
%stackaddr$prim57334 = alloca %struct.ScmObj*, align 8
%_95k47440 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56157)
store volatile %struct.ScmObj* %_95k47440, %struct.ScmObj** %stackaddr$prim57334, align 8
%stackaddr$prim57335 = alloca %struct.ScmObj*, align 8
%current_45args56158 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56157)
store volatile %struct.ScmObj* %current_45args56158, %struct.ScmObj** %stackaddr$prim57335, align 8
%stackaddr$prim57336 = alloca %struct.ScmObj*, align 8
%anf_45bind47299 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56158)
store volatile %struct.ScmObj* %anf_45bind47299, %struct.ScmObj** %stackaddr$prim57336, align 8
%truthy$cmp57337 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47299)
%cmp$cmp57337 = icmp eq i64 %truthy$cmp57337, 1
br i1 %cmp$cmp57337, label %truebranch$cmp57337, label %falsebranch$cmp57337
truebranch$cmp57337:
%ae48899 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56160$k474380 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57338 = alloca %struct.ScmObj*, align 8
%argslist56160$k474381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47193, %struct.ScmObj* %argslist56160$k474380)
store volatile %struct.ScmObj* %argslist56160$k474381, %struct.ScmObj** %stackaddr$prim57338, align 8
%stackaddr$prim57339 = alloca %struct.ScmObj*, align 8
%argslist56160$k474382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48899, %struct.ScmObj* %argslist56160$k474381)
store volatile %struct.ScmObj* %argslist56160$k474382, %struct.ScmObj** %stackaddr$prim57339, align 8
%clofunc57340 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47438)
musttail call tailcc void %clofunc57340(%struct.ScmObj* %k47438, %struct.ScmObj* %argslist56160$k474382)
ret void
falsebranch$cmp57337:
%stackaddr$makeclosure57341 = alloca %struct.ScmObj*, align 8
%fptrToInt57342 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48904 to i64
%ae48904 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt57342)
store volatile %struct.ScmObj* %ae48904, %struct.ScmObj** %stackaddr$makeclosure57341, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48904, %struct.ScmObj* %lsts47192, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48904, %struct.ScmObj* %_37foldr47112, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48904, %struct.ScmObj* %_37foldl47190, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48904, %struct.ScmObj* %_37map147138, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48904, %struct.ScmObj* %k47438, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48904, %struct.ScmObj* %f47194, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48904, %struct.ScmObj* %acc47193, i64 6)
%ae48905 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57343 = alloca %struct.ScmObj*, align 8
%fptrToInt57344 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48906 to i64
%ae48906 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57344)
store volatile %struct.ScmObj* %ae48906, %struct.ScmObj** %stackaddr$makeclosure57343, align 8
%argslist56200$ae489040 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57345 = alloca %struct.ScmObj*, align 8
%argslist56200$ae489041 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48906, %struct.ScmObj* %argslist56200$ae489040)
store volatile %struct.ScmObj* %argslist56200$ae489041, %struct.ScmObj** %stackaddr$prim57345, align 8
%stackaddr$prim57346 = alloca %struct.ScmObj*, align 8
%argslist56200$ae489042 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48905, %struct.ScmObj* %argslist56200$ae489041)
store volatile %struct.ScmObj* %argslist56200$ae489042, %struct.ScmObj** %stackaddr$prim57346, align 8
%clofunc57347 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48904)
musttail call tailcc void %clofunc57347(%struct.ScmObj* %ae48904, %struct.ScmObj* %argslist56200$ae489042)
ret void
}

define tailcc void @proc_clo$ae48904(%struct.ScmObj* %env$ae48904,%struct.ScmObj* %current_45args56161) {
%stackaddr$env-ref57348 = alloca %struct.ScmObj*, align 8
%lsts47192 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48904, i64 0)
store %struct.ScmObj* %lsts47192, %struct.ScmObj** %stackaddr$env-ref57348
%stackaddr$env-ref57349 = alloca %struct.ScmObj*, align 8
%_37foldr47112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48904, i64 1)
store %struct.ScmObj* %_37foldr47112, %struct.ScmObj** %stackaddr$env-ref57349
%stackaddr$env-ref57350 = alloca %struct.ScmObj*, align 8
%_37foldl47190 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48904, i64 2)
store %struct.ScmObj* %_37foldl47190, %struct.ScmObj** %stackaddr$env-ref57350
%stackaddr$env-ref57351 = alloca %struct.ScmObj*, align 8
%_37map147138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48904, i64 3)
store %struct.ScmObj* %_37map147138, %struct.ScmObj** %stackaddr$env-ref57351
%stackaddr$env-ref57352 = alloca %struct.ScmObj*, align 8
%k47438 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48904, i64 4)
store %struct.ScmObj* %k47438, %struct.ScmObj** %stackaddr$env-ref57352
%stackaddr$env-ref57353 = alloca %struct.ScmObj*, align 8
%f47194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48904, i64 5)
store %struct.ScmObj* %f47194, %struct.ScmObj** %stackaddr$env-ref57353
%stackaddr$env-ref57354 = alloca %struct.ScmObj*, align 8
%acc47193 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48904, i64 6)
store %struct.ScmObj* %acc47193, %struct.ScmObj** %stackaddr$env-ref57354
%stackaddr$prim57355 = alloca %struct.ScmObj*, align 8
%_95k47441 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56161)
store volatile %struct.ScmObj* %_95k47441, %struct.ScmObj** %stackaddr$prim57355, align 8
%stackaddr$prim57356 = alloca %struct.ScmObj*, align 8
%current_45args56162 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56161)
store volatile %struct.ScmObj* %current_45args56162, %struct.ScmObj** %stackaddr$prim57356, align 8
%stackaddr$prim57357 = alloca %struct.ScmObj*, align 8
%anf_45bind47300 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56162)
store volatile %struct.ScmObj* %anf_45bind47300, %struct.ScmObj** %stackaddr$prim57357, align 8
%stackaddr$makeclosure57358 = alloca %struct.ScmObj*, align 8
%fptrToInt57359 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48925 to i64
%ae48925 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt57359)
store volatile %struct.ScmObj* %ae48925, %struct.ScmObj** %stackaddr$makeclosure57358, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48925, %struct.ScmObj* %lsts47192, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48925, %struct.ScmObj* %_37foldr47112, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48925, %struct.ScmObj* %_37foldl47190, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48925, %struct.ScmObj* %_37map147138, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48925, %struct.ScmObj* %k47438, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48925, %struct.ScmObj* %f47194, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48925, %struct.ScmObj* %acc47193, i64 6)
%argslist56195$_37map1471380 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57360 = alloca %struct.ScmObj*, align 8
%argslist56195$_37map1471381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47192, %struct.ScmObj* %argslist56195$_37map1471380)
store volatile %struct.ScmObj* %argslist56195$_37map1471381, %struct.ScmObj** %stackaddr$prim57360, align 8
%stackaddr$prim57361 = alloca %struct.ScmObj*, align 8
%argslist56195$_37map1471382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47300, %struct.ScmObj* %argslist56195$_37map1471381)
store volatile %struct.ScmObj* %argslist56195$_37map1471382, %struct.ScmObj** %stackaddr$prim57361, align 8
%stackaddr$prim57362 = alloca %struct.ScmObj*, align 8
%argslist56195$_37map1471383 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48925, %struct.ScmObj* %argslist56195$_37map1471382)
store volatile %struct.ScmObj* %argslist56195$_37map1471383, %struct.ScmObj** %stackaddr$prim57362, align 8
%clofunc57363 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147138)
musttail call tailcc void %clofunc57363(%struct.ScmObj* %_37map147138, %struct.ScmObj* %argslist56195$_37map1471383)
ret void
}

define tailcc void @proc_clo$ae48925(%struct.ScmObj* %env$ae48925,%struct.ScmObj* %current_45args56164) {
%stackaddr$env-ref57364 = alloca %struct.ScmObj*, align 8
%lsts47192 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48925, i64 0)
store %struct.ScmObj* %lsts47192, %struct.ScmObj** %stackaddr$env-ref57364
%stackaddr$env-ref57365 = alloca %struct.ScmObj*, align 8
%_37foldr47112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48925, i64 1)
store %struct.ScmObj* %_37foldr47112, %struct.ScmObj** %stackaddr$env-ref57365
%stackaddr$env-ref57366 = alloca %struct.ScmObj*, align 8
%_37foldl47190 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48925, i64 2)
store %struct.ScmObj* %_37foldl47190, %struct.ScmObj** %stackaddr$env-ref57366
%stackaddr$env-ref57367 = alloca %struct.ScmObj*, align 8
%_37map147138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48925, i64 3)
store %struct.ScmObj* %_37map147138, %struct.ScmObj** %stackaddr$env-ref57367
%stackaddr$env-ref57368 = alloca %struct.ScmObj*, align 8
%k47438 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48925, i64 4)
store %struct.ScmObj* %k47438, %struct.ScmObj** %stackaddr$env-ref57368
%stackaddr$env-ref57369 = alloca %struct.ScmObj*, align 8
%f47194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48925, i64 5)
store %struct.ScmObj* %f47194, %struct.ScmObj** %stackaddr$env-ref57369
%stackaddr$env-ref57370 = alloca %struct.ScmObj*, align 8
%acc47193 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48925, i64 6)
store %struct.ScmObj* %acc47193, %struct.ScmObj** %stackaddr$env-ref57370
%stackaddr$prim57371 = alloca %struct.ScmObj*, align 8
%_95k47442 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56164)
store volatile %struct.ScmObj* %_95k47442, %struct.ScmObj** %stackaddr$prim57371, align 8
%stackaddr$prim57372 = alloca %struct.ScmObj*, align 8
%current_45args56165 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56164)
store volatile %struct.ScmObj* %current_45args56165, %struct.ScmObj** %stackaddr$prim57372, align 8
%stackaddr$prim57373 = alloca %struct.ScmObj*, align 8
%lsts_4347199 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56165)
store volatile %struct.ScmObj* %lsts_4347199, %struct.ScmObj** %stackaddr$prim57373, align 8
%stackaddr$makeclosure57374 = alloca %struct.ScmObj*, align 8
%fptrToInt57375 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48928 to i64
%ae48928 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt57375)
store volatile %struct.ScmObj* %ae48928, %struct.ScmObj** %stackaddr$makeclosure57374, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48928, %struct.ScmObj* %lsts47192, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48928, %struct.ScmObj* %_37foldr47112, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48928, %struct.ScmObj* %_37foldl47190, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48928, %struct.ScmObj* %_37map147138, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48928, %struct.ScmObj* %lsts_4347199, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48928, %struct.ScmObj* %k47438, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48928, %struct.ScmObj* %f47194, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae48928, %struct.ScmObj* %acc47193, i64 7)
%ae48929 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57376 = alloca %struct.ScmObj*, align 8
%fptrToInt57377 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48930 to i64
%ae48930 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57377)
store volatile %struct.ScmObj* %ae48930, %struct.ScmObj** %stackaddr$makeclosure57376, align 8
%argslist56194$ae489280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57378 = alloca %struct.ScmObj*, align 8
%argslist56194$ae489281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48930, %struct.ScmObj* %argslist56194$ae489280)
store volatile %struct.ScmObj* %argslist56194$ae489281, %struct.ScmObj** %stackaddr$prim57378, align 8
%stackaddr$prim57379 = alloca %struct.ScmObj*, align 8
%argslist56194$ae489282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48929, %struct.ScmObj* %argslist56194$ae489281)
store volatile %struct.ScmObj* %argslist56194$ae489282, %struct.ScmObj** %stackaddr$prim57379, align 8
%clofunc57380 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48928)
musttail call tailcc void %clofunc57380(%struct.ScmObj* %ae48928, %struct.ScmObj* %argslist56194$ae489282)
ret void
}

define tailcc void @proc_clo$ae48928(%struct.ScmObj* %env$ae48928,%struct.ScmObj* %current_45args56167) {
%stackaddr$env-ref57381 = alloca %struct.ScmObj*, align 8
%lsts47192 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48928, i64 0)
store %struct.ScmObj* %lsts47192, %struct.ScmObj** %stackaddr$env-ref57381
%stackaddr$env-ref57382 = alloca %struct.ScmObj*, align 8
%_37foldr47112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48928, i64 1)
store %struct.ScmObj* %_37foldr47112, %struct.ScmObj** %stackaddr$env-ref57382
%stackaddr$env-ref57383 = alloca %struct.ScmObj*, align 8
%_37foldl47190 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48928, i64 2)
store %struct.ScmObj* %_37foldl47190, %struct.ScmObj** %stackaddr$env-ref57383
%stackaddr$env-ref57384 = alloca %struct.ScmObj*, align 8
%_37map147138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48928, i64 3)
store %struct.ScmObj* %_37map147138, %struct.ScmObj** %stackaddr$env-ref57384
%stackaddr$env-ref57385 = alloca %struct.ScmObj*, align 8
%lsts_4347199 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48928, i64 4)
store %struct.ScmObj* %lsts_4347199, %struct.ScmObj** %stackaddr$env-ref57385
%stackaddr$env-ref57386 = alloca %struct.ScmObj*, align 8
%k47438 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48928, i64 5)
store %struct.ScmObj* %k47438, %struct.ScmObj** %stackaddr$env-ref57386
%stackaddr$env-ref57387 = alloca %struct.ScmObj*, align 8
%f47194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48928, i64 6)
store %struct.ScmObj* %f47194, %struct.ScmObj** %stackaddr$env-ref57387
%stackaddr$env-ref57388 = alloca %struct.ScmObj*, align 8
%acc47193 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48928, i64 7)
store %struct.ScmObj* %acc47193, %struct.ScmObj** %stackaddr$env-ref57388
%stackaddr$prim57389 = alloca %struct.ScmObj*, align 8
%_95k47443 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56167)
store volatile %struct.ScmObj* %_95k47443, %struct.ScmObj** %stackaddr$prim57389, align 8
%stackaddr$prim57390 = alloca %struct.ScmObj*, align 8
%current_45args56168 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56167)
store volatile %struct.ScmObj* %current_45args56168, %struct.ScmObj** %stackaddr$prim57390, align 8
%stackaddr$prim57391 = alloca %struct.ScmObj*, align 8
%anf_45bind47301 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56168)
store volatile %struct.ScmObj* %anf_45bind47301, %struct.ScmObj** %stackaddr$prim57391, align 8
%stackaddr$makeclosure57392 = alloca %struct.ScmObj*, align 8
%fptrToInt57393 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48949 to i64
%ae48949 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt57393)
store volatile %struct.ScmObj* %ae48949, %struct.ScmObj** %stackaddr$makeclosure57392, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48949, %struct.ScmObj* %_37foldl47190, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48949, %struct.ScmObj* %lsts_4347199, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48949, %struct.ScmObj* %k47438, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48949, %struct.ScmObj* %f47194, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48949, %struct.ScmObj* %acc47193, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48949, %struct.ScmObj* %_37foldr47112, i64 5)
%argslist56189$_37map1471380 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57394 = alloca %struct.ScmObj*, align 8
%argslist56189$_37map1471381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47192, %struct.ScmObj* %argslist56189$_37map1471380)
store volatile %struct.ScmObj* %argslist56189$_37map1471381, %struct.ScmObj** %stackaddr$prim57394, align 8
%stackaddr$prim57395 = alloca %struct.ScmObj*, align 8
%argslist56189$_37map1471382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47301, %struct.ScmObj* %argslist56189$_37map1471381)
store volatile %struct.ScmObj* %argslist56189$_37map1471382, %struct.ScmObj** %stackaddr$prim57395, align 8
%stackaddr$prim57396 = alloca %struct.ScmObj*, align 8
%argslist56189$_37map1471383 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48949, %struct.ScmObj* %argslist56189$_37map1471382)
store volatile %struct.ScmObj* %argslist56189$_37map1471383, %struct.ScmObj** %stackaddr$prim57396, align 8
%clofunc57397 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147138)
musttail call tailcc void %clofunc57397(%struct.ScmObj* %_37map147138, %struct.ScmObj* %argslist56189$_37map1471383)
ret void
}

define tailcc void @proc_clo$ae48949(%struct.ScmObj* %env$ae48949,%struct.ScmObj* %current_45args56170) {
%stackaddr$env-ref57398 = alloca %struct.ScmObj*, align 8
%_37foldl47190 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48949, i64 0)
store %struct.ScmObj* %_37foldl47190, %struct.ScmObj** %stackaddr$env-ref57398
%stackaddr$env-ref57399 = alloca %struct.ScmObj*, align 8
%lsts_4347199 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48949, i64 1)
store %struct.ScmObj* %lsts_4347199, %struct.ScmObj** %stackaddr$env-ref57399
%stackaddr$env-ref57400 = alloca %struct.ScmObj*, align 8
%k47438 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48949, i64 2)
store %struct.ScmObj* %k47438, %struct.ScmObj** %stackaddr$env-ref57400
%stackaddr$env-ref57401 = alloca %struct.ScmObj*, align 8
%f47194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48949, i64 3)
store %struct.ScmObj* %f47194, %struct.ScmObj** %stackaddr$env-ref57401
%stackaddr$env-ref57402 = alloca %struct.ScmObj*, align 8
%acc47193 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48949, i64 4)
store %struct.ScmObj* %acc47193, %struct.ScmObj** %stackaddr$env-ref57402
%stackaddr$env-ref57403 = alloca %struct.ScmObj*, align 8
%_37foldr47112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48949, i64 5)
store %struct.ScmObj* %_37foldr47112, %struct.ScmObj** %stackaddr$env-ref57403
%stackaddr$prim57404 = alloca %struct.ScmObj*, align 8
%_95k47444 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56170)
store volatile %struct.ScmObj* %_95k47444, %struct.ScmObj** %stackaddr$prim57404, align 8
%stackaddr$prim57405 = alloca %struct.ScmObj*, align 8
%current_45args56171 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56170)
store volatile %struct.ScmObj* %current_45args56171, %struct.ScmObj** %stackaddr$prim57405, align 8
%stackaddr$prim57406 = alloca %struct.ScmObj*, align 8
%vs47197 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56171)
store volatile %struct.ScmObj* %vs47197, %struct.ScmObj** %stackaddr$prim57406, align 8
%stackaddr$makeclosure57407 = alloca %struct.ScmObj*, align 8
%fptrToInt57408 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48952 to i64
%ae48952 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt57408)
store volatile %struct.ScmObj* %ae48952, %struct.ScmObj** %stackaddr$makeclosure57407, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48952, %struct.ScmObj* %_37foldl47190, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48952, %struct.ScmObj* %lsts_4347199, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48952, %struct.ScmObj* %k47438, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48952, %struct.ScmObj* %vs47197, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48952, %struct.ScmObj* %f47194, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48952, %struct.ScmObj* %acc47193, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48952, %struct.ScmObj* %_37foldr47112, i64 6)
%ae48953 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57409 = alloca %struct.ScmObj*, align 8
%fptrToInt57410 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48954 to i64
%ae48954 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57410)
store volatile %struct.ScmObj* %ae48954, %struct.ScmObj** %stackaddr$makeclosure57409, align 8
%argslist56188$ae489520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57411 = alloca %struct.ScmObj*, align 8
%argslist56188$ae489521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48954, %struct.ScmObj* %argslist56188$ae489520)
store volatile %struct.ScmObj* %argslist56188$ae489521, %struct.ScmObj** %stackaddr$prim57411, align 8
%stackaddr$prim57412 = alloca %struct.ScmObj*, align 8
%argslist56188$ae489522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48953, %struct.ScmObj* %argslist56188$ae489521)
store volatile %struct.ScmObj* %argslist56188$ae489522, %struct.ScmObj** %stackaddr$prim57412, align 8
%clofunc57413 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48952)
musttail call tailcc void %clofunc57413(%struct.ScmObj* %ae48952, %struct.ScmObj* %argslist56188$ae489522)
ret void
}

define tailcc void @proc_clo$ae48952(%struct.ScmObj* %env$ae48952,%struct.ScmObj* %current_45args56173) {
%stackaddr$env-ref57414 = alloca %struct.ScmObj*, align 8
%_37foldl47190 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48952, i64 0)
store %struct.ScmObj* %_37foldl47190, %struct.ScmObj** %stackaddr$env-ref57414
%stackaddr$env-ref57415 = alloca %struct.ScmObj*, align 8
%lsts_4347199 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48952, i64 1)
store %struct.ScmObj* %lsts_4347199, %struct.ScmObj** %stackaddr$env-ref57415
%stackaddr$env-ref57416 = alloca %struct.ScmObj*, align 8
%k47438 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48952, i64 2)
store %struct.ScmObj* %k47438, %struct.ScmObj** %stackaddr$env-ref57416
%stackaddr$env-ref57417 = alloca %struct.ScmObj*, align 8
%vs47197 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48952, i64 3)
store %struct.ScmObj* %vs47197, %struct.ScmObj** %stackaddr$env-ref57417
%stackaddr$env-ref57418 = alloca %struct.ScmObj*, align 8
%f47194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48952, i64 4)
store %struct.ScmObj* %f47194, %struct.ScmObj** %stackaddr$env-ref57418
%stackaddr$env-ref57419 = alloca %struct.ScmObj*, align 8
%acc47193 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48952, i64 5)
store %struct.ScmObj* %acc47193, %struct.ScmObj** %stackaddr$env-ref57419
%stackaddr$env-ref57420 = alloca %struct.ScmObj*, align 8
%_37foldr47112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48952, i64 6)
store %struct.ScmObj* %_37foldr47112, %struct.ScmObj** %stackaddr$env-ref57420
%stackaddr$prim57421 = alloca %struct.ScmObj*, align 8
%_95k47445 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56173)
store volatile %struct.ScmObj* %_95k47445, %struct.ScmObj** %stackaddr$prim57421, align 8
%stackaddr$prim57422 = alloca %struct.ScmObj*, align 8
%current_45args56174 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56173)
store volatile %struct.ScmObj* %current_45args56174, %struct.ScmObj** %stackaddr$prim57422, align 8
%stackaddr$prim57423 = alloca %struct.ScmObj*, align 8
%anf_45bind47302 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56174)
store volatile %struct.ScmObj* %anf_45bind47302, %struct.ScmObj** %stackaddr$prim57423, align 8
%ae48975 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57424 = alloca %struct.ScmObj*, align 8
%anf_45bind47303 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47193, %struct.ScmObj* %ae48975)
store volatile %struct.ScmObj* %anf_45bind47303, %struct.ScmObj** %stackaddr$prim57424, align 8
%stackaddr$makeclosure57425 = alloca %struct.ScmObj*, align 8
%fptrToInt57426 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48977 to i64
%ae48977 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57426)
store volatile %struct.ScmObj* %ae48977, %struct.ScmObj** %stackaddr$makeclosure57425, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48977, %struct.ScmObj* %_37foldl47190, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48977, %struct.ScmObj* %lsts_4347199, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48977, %struct.ScmObj* %k47438, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48977, %struct.ScmObj* %f47194, i64 3)
%argslist56182$_37foldr471120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57427 = alloca %struct.ScmObj*, align 8
%argslist56182$_37foldr471121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs47197, %struct.ScmObj* %argslist56182$_37foldr471120)
store volatile %struct.ScmObj* %argslist56182$_37foldr471121, %struct.ScmObj** %stackaddr$prim57427, align 8
%stackaddr$prim57428 = alloca %struct.ScmObj*, align 8
%argslist56182$_37foldr471122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47303, %struct.ScmObj* %argslist56182$_37foldr471121)
store volatile %struct.ScmObj* %argslist56182$_37foldr471122, %struct.ScmObj** %stackaddr$prim57428, align 8
%stackaddr$prim57429 = alloca %struct.ScmObj*, align 8
%argslist56182$_37foldr471123 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47302, %struct.ScmObj* %argslist56182$_37foldr471122)
store volatile %struct.ScmObj* %argslist56182$_37foldr471123, %struct.ScmObj** %stackaddr$prim57429, align 8
%stackaddr$prim57430 = alloca %struct.ScmObj*, align 8
%argslist56182$_37foldr471124 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48977, %struct.ScmObj* %argslist56182$_37foldr471123)
store volatile %struct.ScmObj* %argslist56182$_37foldr471124, %struct.ScmObj** %stackaddr$prim57430, align 8
%clofunc57431 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr47112)
musttail call tailcc void %clofunc57431(%struct.ScmObj* %_37foldr47112, %struct.ScmObj* %argslist56182$_37foldr471124)
ret void
}

define tailcc void @proc_clo$ae48977(%struct.ScmObj* %env$ae48977,%struct.ScmObj* %current_45args56176) {
%stackaddr$env-ref57432 = alloca %struct.ScmObj*, align 8
%_37foldl47190 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48977, i64 0)
store %struct.ScmObj* %_37foldl47190, %struct.ScmObj** %stackaddr$env-ref57432
%stackaddr$env-ref57433 = alloca %struct.ScmObj*, align 8
%lsts_4347199 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48977, i64 1)
store %struct.ScmObj* %lsts_4347199, %struct.ScmObj** %stackaddr$env-ref57433
%stackaddr$env-ref57434 = alloca %struct.ScmObj*, align 8
%k47438 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48977, i64 2)
store %struct.ScmObj* %k47438, %struct.ScmObj** %stackaddr$env-ref57434
%stackaddr$env-ref57435 = alloca %struct.ScmObj*, align 8
%f47194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48977, i64 3)
store %struct.ScmObj* %f47194, %struct.ScmObj** %stackaddr$env-ref57435
%stackaddr$prim57436 = alloca %struct.ScmObj*, align 8
%_95k47446 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56176)
store volatile %struct.ScmObj* %_95k47446, %struct.ScmObj** %stackaddr$prim57436, align 8
%stackaddr$prim57437 = alloca %struct.ScmObj*, align 8
%current_45args56177 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56176)
store volatile %struct.ScmObj* %current_45args56177, %struct.ScmObj** %stackaddr$prim57437, align 8
%stackaddr$prim57438 = alloca %struct.ScmObj*, align 8
%anf_45bind47304 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56177)
store volatile %struct.ScmObj* %anf_45bind47304, %struct.ScmObj** %stackaddr$prim57438, align 8
%stackaddr$makeclosure57439 = alloca %struct.ScmObj*, align 8
%fptrToInt57440 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48981 to i64
%ae48981 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57440)
store volatile %struct.ScmObj* %ae48981, %struct.ScmObj** %stackaddr$makeclosure57439, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48981, %struct.ScmObj* %_37foldl47190, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48981, %struct.ScmObj* %lsts_4347199, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48981, %struct.ScmObj* %k47438, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48981, %struct.ScmObj* %f47194, i64 3)
%stackaddr$prim57441 = alloca %struct.ScmObj*, align 8
%cpsargs47449 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48981, %struct.ScmObj* %anf_45bind47304)
store volatile %struct.ScmObj* %cpsargs47449, %struct.ScmObj** %stackaddr$prim57441, align 8
%clofunc57442 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47194)
musttail call tailcc void %clofunc57442(%struct.ScmObj* %f47194, %struct.ScmObj* %cpsargs47449)
ret void
}

define tailcc void @proc_clo$ae48981(%struct.ScmObj* %env$ae48981,%struct.ScmObj* %current_45args56179) {
%stackaddr$env-ref57443 = alloca %struct.ScmObj*, align 8
%_37foldl47190 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48981, i64 0)
store %struct.ScmObj* %_37foldl47190, %struct.ScmObj** %stackaddr$env-ref57443
%stackaddr$env-ref57444 = alloca %struct.ScmObj*, align 8
%lsts_4347199 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48981, i64 1)
store %struct.ScmObj* %lsts_4347199, %struct.ScmObj** %stackaddr$env-ref57444
%stackaddr$env-ref57445 = alloca %struct.ScmObj*, align 8
%k47438 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48981, i64 2)
store %struct.ScmObj* %k47438, %struct.ScmObj** %stackaddr$env-ref57445
%stackaddr$env-ref57446 = alloca %struct.ScmObj*, align 8
%f47194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48981, i64 3)
store %struct.ScmObj* %f47194, %struct.ScmObj** %stackaddr$env-ref57446
%stackaddr$prim57447 = alloca %struct.ScmObj*, align 8
%_95k47447 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56179)
store volatile %struct.ScmObj* %_95k47447, %struct.ScmObj** %stackaddr$prim57447, align 8
%stackaddr$prim57448 = alloca %struct.ScmObj*, align 8
%current_45args56180 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56179)
store volatile %struct.ScmObj* %current_45args56180, %struct.ScmObj** %stackaddr$prim57448, align 8
%stackaddr$prim57449 = alloca %struct.ScmObj*, align 8
%acc_4347201 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56180)
store volatile %struct.ScmObj* %acc_4347201, %struct.ScmObj** %stackaddr$prim57449, align 8
%stackaddr$prim57450 = alloca %struct.ScmObj*, align 8
%anf_45bind47305 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4347201, %struct.ScmObj* %lsts_4347199)
store volatile %struct.ScmObj* %anf_45bind47305, %struct.ScmObj** %stackaddr$prim57450, align 8
%stackaddr$prim57451 = alloca %struct.ScmObj*, align 8
%anf_45bind47306 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47194, %struct.ScmObj* %anf_45bind47305)
store volatile %struct.ScmObj* %anf_45bind47306, %struct.ScmObj** %stackaddr$prim57451, align 8
%stackaddr$prim57452 = alloca %struct.ScmObj*, align 8
%cpsargs47448 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47438, %struct.ScmObj* %anf_45bind47306)
store volatile %struct.ScmObj* %cpsargs47448, %struct.ScmObj** %stackaddr$prim57452, align 8
%clofunc57453 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl47190)
musttail call tailcc void %clofunc57453(%struct.ScmObj* %_37foldl47190, %struct.ScmObj* %cpsargs47448)
ret void
}

define tailcc void @proc_clo$ae48954(%struct.ScmObj* %env$ae48954,%struct.ScmObj* %current_45args56183) {
%stackaddr$prim57454 = alloca %struct.ScmObj*, align 8
%k47450 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56183)
store volatile %struct.ScmObj* %k47450, %struct.ScmObj** %stackaddr$prim57454, align 8
%stackaddr$prim57455 = alloca %struct.ScmObj*, align 8
%current_45args56184 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56183)
store volatile %struct.ScmObj* %current_45args56184, %struct.ScmObj** %stackaddr$prim57455, align 8
%stackaddr$prim57456 = alloca %struct.ScmObj*, align 8
%a47203 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56184)
store volatile %struct.ScmObj* %a47203, %struct.ScmObj** %stackaddr$prim57456, align 8
%stackaddr$prim57457 = alloca %struct.ScmObj*, align 8
%current_45args56185 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56184)
store volatile %struct.ScmObj* %current_45args56185, %struct.ScmObj** %stackaddr$prim57457, align 8
%stackaddr$prim57458 = alloca %struct.ScmObj*, align 8
%b47202 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56185)
store volatile %struct.ScmObj* %b47202, %struct.ScmObj** %stackaddr$prim57458, align 8
%stackaddr$prim57459 = alloca %struct.ScmObj*, align 8
%cpsprim47451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a47203, %struct.ScmObj* %b47202)
store volatile %struct.ScmObj* %cpsprim47451, %struct.ScmObj** %stackaddr$prim57459, align 8
%ae48958 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56187$k474500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57460 = alloca %struct.ScmObj*, align 8
%argslist56187$k474501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47451, %struct.ScmObj* %argslist56187$k474500)
store volatile %struct.ScmObj* %argslist56187$k474501, %struct.ScmObj** %stackaddr$prim57460, align 8
%stackaddr$prim57461 = alloca %struct.ScmObj*, align 8
%argslist56187$k474502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48958, %struct.ScmObj* %argslist56187$k474501)
store volatile %struct.ScmObj* %argslist56187$k474502, %struct.ScmObj** %stackaddr$prim57461, align 8
%clofunc57462 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47450)
musttail call tailcc void %clofunc57462(%struct.ScmObj* %k47450, %struct.ScmObj* %argslist56187$k474502)
ret void
}

define tailcc void @proc_clo$ae48930(%struct.ScmObj* %env$ae48930,%struct.ScmObj* %current_45args56190) {
%stackaddr$prim57463 = alloca %struct.ScmObj*, align 8
%k47452 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56190)
store volatile %struct.ScmObj* %k47452, %struct.ScmObj** %stackaddr$prim57463, align 8
%stackaddr$prim57464 = alloca %struct.ScmObj*, align 8
%current_45args56191 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56190)
store volatile %struct.ScmObj* %current_45args56191, %struct.ScmObj** %stackaddr$prim57464, align 8
%stackaddr$prim57465 = alloca %struct.ScmObj*, align 8
%x47198 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56191)
store volatile %struct.ScmObj* %x47198, %struct.ScmObj** %stackaddr$prim57465, align 8
%stackaddr$prim57466 = alloca %struct.ScmObj*, align 8
%cpsprim47453 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47198)
store volatile %struct.ScmObj* %cpsprim47453, %struct.ScmObj** %stackaddr$prim57466, align 8
%ae48933 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56193$k474520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57467 = alloca %struct.ScmObj*, align 8
%argslist56193$k474521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47453, %struct.ScmObj* %argslist56193$k474520)
store volatile %struct.ScmObj* %argslist56193$k474521, %struct.ScmObj** %stackaddr$prim57467, align 8
%stackaddr$prim57468 = alloca %struct.ScmObj*, align 8
%argslist56193$k474522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48933, %struct.ScmObj* %argslist56193$k474521)
store volatile %struct.ScmObj* %argslist56193$k474522, %struct.ScmObj** %stackaddr$prim57468, align 8
%clofunc57469 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47452)
musttail call tailcc void %clofunc57469(%struct.ScmObj* %k47452, %struct.ScmObj* %argslist56193$k474522)
ret void
}

define tailcc void @proc_clo$ae48906(%struct.ScmObj* %env$ae48906,%struct.ScmObj* %current_45args56196) {
%stackaddr$prim57470 = alloca %struct.ScmObj*, align 8
%k47454 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56196)
store volatile %struct.ScmObj* %k47454, %struct.ScmObj** %stackaddr$prim57470, align 8
%stackaddr$prim57471 = alloca %struct.ScmObj*, align 8
%current_45args56197 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56196)
store volatile %struct.ScmObj* %current_45args56197, %struct.ScmObj** %stackaddr$prim57471, align 8
%stackaddr$prim57472 = alloca %struct.ScmObj*, align 8
%x47200 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56197)
store volatile %struct.ScmObj* %x47200, %struct.ScmObj** %stackaddr$prim57472, align 8
%stackaddr$prim57473 = alloca %struct.ScmObj*, align 8
%cpsprim47455 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47200)
store volatile %struct.ScmObj* %cpsprim47455, %struct.ScmObj** %stackaddr$prim57473, align 8
%ae48909 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56199$k474540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57474 = alloca %struct.ScmObj*, align 8
%argslist56199$k474541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47455, %struct.ScmObj* %argslist56199$k474540)
store volatile %struct.ScmObj* %argslist56199$k474541, %struct.ScmObj** %stackaddr$prim57474, align 8
%stackaddr$prim57475 = alloca %struct.ScmObj*, align 8
%argslist56199$k474542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48909, %struct.ScmObj* %argslist56199$k474541)
store volatile %struct.ScmObj* %argslist56199$k474542, %struct.ScmObj** %stackaddr$prim57475, align 8
%clofunc57476 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47454)
musttail call tailcc void %clofunc57476(%struct.ScmObj* %k47454, %struct.ScmObj* %argslist56199$k474542)
ret void
}

define tailcc void @proc_clo$ae48858(%struct.ScmObj* %env$ae48858,%struct.ScmObj* %current_45args56202) {
%stackaddr$prim57477 = alloca %struct.ScmObj*, align 8
%k47456 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56202)
store volatile %struct.ScmObj* %k47456, %struct.ScmObj** %stackaddr$prim57477, align 8
%stackaddr$prim57478 = alloca %struct.ScmObj*, align 8
%current_45args56203 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56202)
store volatile %struct.ScmObj* %current_45args56203, %struct.ScmObj** %stackaddr$prim57478, align 8
%stackaddr$prim57479 = alloca %struct.ScmObj*, align 8
%lst47196 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56203)
store volatile %struct.ScmObj* %lst47196, %struct.ScmObj** %stackaddr$prim57479, align 8
%stackaddr$prim57480 = alloca %struct.ScmObj*, align 8
%current_45args56204 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56203)
store volatile %struct.ScmObj* %current_45args56204, %struct.ScmObj** %stackaddr$prim57480, align 8
%stackaddr$prim57481 = alloca %struct.ScmObj*, align 8
%b47195 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56204)
store volatile %struct.ScmObj* %b47195, %struct.ScmObj** %stackaddr$prim57481, align 8
%truthy$cmp57482 = call i64 @is_truthy_value(%struct.ScmObj* %b47195)
%cmp$cmp57482 = icmp eq i64 %truthy$cmp57482, 1
br i1 %cmp$cmp57482, label %truebranch$cmp57482, label %falsebranch$cmp57482
truebranch$cmp57482:
%ae48861 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56206$k474560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57483 = alloca %struct.ScmObj*, align 8
%argslist56206$k474561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b47195, %struct.ScmObj* %argslist56206$k474560)
store volatile %struct.ScmObj* %argslist56206$k474561, %struct.ScmObj** %stackaddr$prim57483, align 8
%stackaddr$prim57484 = alloca %struct.ScmObj*, align 8
%argslist56206$k474562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48861, %struct.ScmObj* %argslist56206$k474561)
store volatile %struct.ScmObj* %argslist56206$k474562, %struct.ScmObj** %stackaddr$prim57484, align 8
%clofunc57485 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47456)
musttail call tailcc void %clofunc57485(%struct.ScmObj* %k47456, %struct.ScmObj* %argslist56206$k474562)
ret void
falsebranch$cmp57482:
%stackaddr$prim57486 = alloca %struct.ScmObj*, align 8
%cpsprim47457 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47196)
store volatile %struct.ScmObj* %cpsprim47457, %struct.ScmObj** %stackaddr$prim57486, align 8
%ae48868 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56207$k474560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57487 = alloca %struct.ScmObj*, align 8
%argslist56207$k474561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47457, %struct.ScmObj* %argslist56207$k474560)
store volatile %struct.ScmObj* %argslist56207$k474561, %struct.ScmObj** %stackaddr$prim57487, align 8
%stackaddr$prim57488 = alloca %struct.ScmObj*, align 8
%argslist56207$k474562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48868, %struct.ScmObj* %argslist56207$k474561)
store volatile %struct.ScmObj* %argslist56207$k474562, %struct.ScmObj** %stackaddr$prim57488, align 8
%clofunc57489 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47456)
musttail call tailcc void %clofunc57489(%struct.ScmObj* %k47456, %struct.ScmObj* %argslist56207$k474562)
ret void
}

define tailcc void @proc_clo$ae48699(%struct.ScmObj* %env$ae48699,%struct.ScmObj* %args4713447458) {
%stackaddr$env-ref57490 = alloca %struct.ScmObj*, align 8
%_37drop_45right47126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48699, i64 0)
store %struct.ScmObj* %_37drop_45right47126, %struct.ScmObj** %stackaddr$env-ref57490
%stackaddr$env-ref57491 = alloca %struct.ScmObj*, align 8
%_37last47129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48699, i64 1)
store %struct.ScmObj* %_37last47129, %struct.ScmObj** %stackaddr$env-ref57491
%stackaddr$env-ref57492 = alloca %struct.ScmObj*, align 8
%_37foldr47112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48699, i64 2)
store %struct.ScmObj* %_37foldr47112, %struct.ScmObj** %stackaddr$env-ref57492
%stackaddr$prim57493 = alloca %struct.ScmObj*, align 8
%k47459 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4713447458)
store volatile %struct.ScmObj* %k47459, %struct.ScmObj** %stackaddr$prim57493, align 8
%stackaddr$prim57494 = alloca %struct.ScmObj*, align 8
%args47134 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4713447458)
store volatile %struct.ScmObj* %args47134, %struct.ScmObj** %stackaddr$prim57494, align 8
%stackaddr$prim57495 = alloca %struct.ScmObj*, align 8
%f47136 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47134)
store volatile %struct.ScmObj* %f47136, %struct.ScmObj** %stackaddr$prim57495, align 8
%stackaddr$prim57496 = alloca %struct.ScmObj*, align 8
%lsts47135 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47134)
store volatile %struct.ScmObj* %lsts47135, %struct.ScmObj** %stackaddr$prim57496, align 8
%stackaddr$makeclosure57497 = alloca %struct.ScmObj*, align 8
%fptrToInt57498 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48704 to i64
%ae48704 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57498)
store volatile %struct.ScmObj* %ae48704, %struct.ScmObj** %stackaddr$makeclosure57497, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48704, %struct.ScmObj* %k47459, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48704, %struct.ScmObj* %lsts47135, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48704, %struct.ScmObj* %_37foldr47112, i64 2)
%ae48705 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57499 = alloca %struct.ScmObj*, align 8
%fptrToInt57500 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48706 to i64
%ae48706 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57500)
store volatile %struct.ScmObj* %ae48706, %struct.ScmObj** %stackaddr$makeclosure57499, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48706, %struct.ScmObj* %_37drop_45right47126, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48706, %struct.ScmObj* %f47136, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48706, %struct.ScmObj* %_37last47129, i64 2)
%argslist56226$ae487040 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57501 = alloca %struct.ScmObj*, align 8
%argslist56226$ae487041 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48706, %struct.ScmObj* %argslist56226$ae487040)
store volatile %struct.ScmObj* %argslist56226$ae487041, %struct.ScmObj** %stackaddr$prim57501, align 8
%stackaddr$prim57502 = alloca %struct.ScmObj*, align 8
%argslist56226$ae487042 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48705, %struct.ScmObj* %argslist56226$ae487041)
store volatile %struct.ScmObj* %argslist56226$ae487042, %struct.ScmObj** %stackaddr$prim57502, align 8
%clofunc57503 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48704)
musttail call tailcc void %clofunc57503(%struct.ScmObj* %ae48704, %struct.ScmObj* %argslist56226$ae487042)
ret void
}

define tailcc void @proc_clo$ae48704(%struct.ScmObj* %env$ae48704,%struct.ScmObj* %current_45args56211) {
%stackaddr$env-ref57504 = alloca %struct.ScmObj*, align 8
%k47459 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48704, i64 0)
store %struct.ScmObj* %k47459, %struct.ScmObj** %stackaddr$env-ref57504
%stackaddr$env-ref57505 = alloca %struct.ScmObj*, align 8
%lsts47135 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48704, i64 1)
store %struct.ScmObj* %lsts47135, %struct.ScmObj** %stackaddr$env-ref57505
%stackaddr$env-ref57506 = alloca %struct.ScmObj*, align 8
%_37foldr47112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48704, i64 2)
store %struct.ScmObj* %_37foldr47112, %struct.ScmObj** %stackaddr$env-ref57506
%stackaddr$prim57507 = alloca %struct.ScmObj*, align 8
%_95k47460 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56211)
store volatile %struct.ScmObj* %_95k47460, %struct.ScmObj** %stackaddr$prim57507, align 8
%stackaddr$prim57508 = alloca %struct.ScmObj*, align 8
%current_45args56212 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56211)
store volatile %struct.ScmObj* %current_45args56212, %struct.ScmObj** %stackaddr$prim57508, align 8
%stackaddr$prim57509 = alloca %struct.ScmObj*, align 8
%anf_45bind47293 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56212)
store volatile %struct.ScmObj* %anf_45bind47293, %struct.ScmObj** %stackaddr$prim57509, align 8
%ae48767 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57510 = alloca %struct.ScmObj*, align 8
%anf_45bind47294 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48767, %struct.ScmObj* %lsts47135)
store volatile %struct.ScmObj* %anf_45bind47294, %struct.ScmObj** %stackaddr$prim57510, align 8
%stackaddr$prim57511 = alloca %struct.ScmObj*, align 8
%anf_45bind47295 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47293, %struct.ScmObj* %anf_45bind47294)
store volatile %struct.ScmObj* %anf_45bind47295, %struct.ScmObj** %stackaddr$prim57511, align 8
%stackaddr$prim57512 = alloca %struct.ScmObj*, align 8
%cpsargs47461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47459, %struct.ScmObj* %anf_45bind47295)
store volatile %struct.ScmObj* %cpsargs47461, %struct.ScmObj** %stackaddr$prim57512, align 8
%clofunc57513 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr47112)
musttail call tailcc void %clofunc57513(%struct.ScmObj* %_37foldr47112, %struct.ScmObj* %cpsargs47461)
ret void
}

define tailcc void @proc_clo$ae48706(%struct.ScmObj* %env$ae48706,%struct.ScmObj* %fargs4713747462) {
%stackaddr$env-ref57514 = alloca %struct.ScmObj*, align 8
%_37drop_45right47126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48706, i64 0)
store %struct.ScmObj* %_37drop_45right47126, %struct.ScmObj** %stackaddr$env-ref57514
%stackaddr$env-ref57515 = alloca %struct.ScmObj*, align 8
%f47136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48706, i64 1)
store %struct.ScmObj* %f47136, %struct.ScmObj** %stackaddr$env-ref57515
%stackaddr$env-ref57516 = alloca %struct.ScmObj*, align 8
%_37last47129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48706, i64 2)
store %struct.ScmObj* %_37last47129, %struct.ScmObj** %stackaddr$env-ref57516
%stackaddr$prim57517 = alloca %struct.ScmObj*, align 8
%k47463 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs4713747462)
store volatile %struct.ScmObj* %k47463, %struct.ScmObj** %stackaddr$prim57517, align 8
%stackaddr$prim57518 = alloca %struct.ScmObj*, align 8
%fargs47137 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs4713747462)
store volatile %struct.ScmObj* %fargs47137, %struct.ScmObj** %stackaddr$prim57518, align 8
%stackaddr$makeclosure57519 = alloca %struct.ScmObj*, align 8
%fptrToInt57520 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48710 to i64
%ae48710 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57520)
store volatile %struct.ScmObj* %ae48710, %struct.ScmObj** %stackaddr$makeclosure57519, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48710, %struct.ScmObj* %fargs47137, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48710, %struct.ScmObj* %f47136, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48710, %struct.ScmObj* %_37last47129, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48710, %struct.ScmObj* %k47463, i64 3)
%ae48712 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist56225$_37drop_45right471260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57521 = alloca %struct.ScmObj*, align 8
%argslist56225$_37drop_45right471261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48712, %struct.ScmObj* %argslist56225$_37drop_45right471260)
store volatile %struct.ScmObj* %argslist56225$_37drop_45right471261, %struct.ScmObj** %stackaddr$prim57521, align 8
%stackaddr$prim57522 = alloca %struct.ScmObj*, align 8
%argslist56225$_37drop_45right471262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs47137, %struct.ScmObj* %argslist56225$_37drop_45right471261)
store volatile %struct.ScmObj* %argslist56225$_37drop_45right471262, %struct.ScmObj** %stackaddr$prim57522, align 8
%stackaddr$prim57523 = alloca %struct.ScmObj*, align 8
%argslist56225$_37drop_45right471263 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48710, %struct.ScmObj* %argslist56225$_37drop_45right471262)
store volatile %struct.ScmObj* %argslist56225$_37drop_45right471263, %struct.ScmObj** %stackaddr$prim57523, align 8
%clofunc57524 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right47126)
musttail call tailcc void %clofunc57524(%struct.ScmObj* %_37drop_45right47126, %struct.ScmObj* %argslist56225$_37drop_45right471263)
ret void
}

define tailcc void @proc_clo$ae48710(%struct.ScmObj* %env$ae48710,%struct.ScmObj* %current_45args56214) {
%stackaddr$env-ref57525 = alloca %struct.ScmObj*, align 8
%fargs47137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48710, i64 0)
store %struct.ScmObj* %fargs47137, %struct.ScmObj** %stackaddr$env-ref57525
%stackaddr$env-ref57526 = alloca %struct.ScmObj*, align 8
%f47136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48710, i64 1)
store %struct.ScmObj* %f47136, %struct.ScmObj** %stackaddr$env-ref57526
%stackaddr$env-ref57527 = alloca %struct.ScmObj*, align 8
%_37last47129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48710, i64 2)
store %struct.ScmObj* %_37last47129, %struct.ScmObj** %stackaddr$env-ref57527
%stackaddr$env-ref57528 = alloca %struct.ScmObj*, align 8
%k47463 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48710, i64 3)
store %struct.ScmObj* %k47463, %struct.ScmObj** %stackaddr$env-ref57528
%stackaddr$prim57529 = alloca %struct.ScmObj*, align 8
%_95k47464 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56214)
store volatile %struct.ScmObj* %_95k47464, %struct.ScmObj** %stackaddr$prim57529, align 8
%stackaddr$prim57530 = alloca %struct.ScmObj*, align 8
%current_45args56215 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56214)
store volatile %struct.ScmObj* %current_45args56215, %struct.ScmObj** %stackaddr$prim57530, align 8
%stackaddr$prim57531 = alloca %struct.ScmObj*, align 8
%anf_45bind47290 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56215)
store volatile %struct.ScmObj* %anf_45bind47290, %struct.ScmObj** %stackaddr$prim57531, align 8
%stackaddr$makeclosure57532 = alloca %struct.ScmObj*, align 8
%fptrToInt57533 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48717 to i64
%ae48717 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57533)
store volatile %struct.ScmObj* %ae48717, %struct.ScmObj** %stackaddr$makeclosure57532, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48717, %struct.ScmObj* %fargs47137, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48717, %struct.ScmObj* %_37last47129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48717, %struct.ScmObj* %k47463, i64 2)
%stackaddr$prim57534 = alloca %struct.ScmObj*, align 8
%cpsargs47468 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48717, %struct.ScmObj* %anf_45bind47290)
store volatile %struct.ScmObj* %cpsargs47468, %struct.ScmObj** %stackaddr$prim57534, align 8
%clofunc57535 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47136)
musttail call tailcc void %clofunc57535(%struct.ScmObj* %f47136, %struct.ScmObj* %cpsargs47468)
ret void
}

define tailcc void @proc_clo$ae48717(%struct.ScmObj* %env$ae48717,%struct.ScmObj* %current_45args56217) {
%stackaddr$env-ref57536 = alloca %struct.ScmObj*, align 8
%fargs47137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48717, i64 0)
store %struct.ScmObj* %fargs47137, %struct.ScmObj** %stackaddr$env-ref57536
%stackaddr$env-ref57537 = alloca %struct.ScmObj*, align 8
%_37last47129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48717, i64 1)
store %struct.ScmObj* %_37last47129, %struct.ScmObj** %stackaddr$env-ref57537
%stackaddr$env-ref57538 = alloca %struct.ScmObj*, align 8
%k47463 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48717, i64 2)
store %struct.ScmObj* %k47463, %struct.ScmObj** %stackaddr$env-ref57538
%stackaddr$prim57539 = alloca %struct.ScmObj*, align 8
%_95k47465 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56217)
store volatile %struct.ScmObj* %_95k47465, %struct.ScmObj** %stackaddr$prim57539, align 8
%stackaddr$prim57540 = alloca %struct.ScmObj*, align 8
%current_45args56218 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56217)
store volatile %struct.ScmObj* %current_45args56218, %struct.ScmObj** %stackaddr$prim57540, align 8
%stackaddr$prim57541 = alloca %struct.ScmObj*, align 8
%anf_45bind47291 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56218)
store volatile %struct.ScmObj* %anf_45bind47291, %struct.ScmObj** %stackaddr$prim57541, align 8
%stackaddr$makeclosure57542 = alloca %struct.ScmObj*, align 8
%fptrToInt57543 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48722 to i64
%ae48722 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57543)
store volatile %struct.ScmObj* %ae48722, %struct.ScmObj** %stackaddr$makeclosure57542, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48722, %struct.ScmObj* %anf_45bind47291, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48722, %struct.ScmObj* %k47463, i64 1)
%argslist56224$_37last471290 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57544 = alloca %struct.ScmObj*, align 8
%argslist56224$_37last471291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs47137, %struct.ScmObj* %argslist56224$_37last471290)
store volatile %struct.ScmObj* %argslist56224$_37last471291, %struct.ScmObj** %stackaddr$prim57544, align 8
%stackaddr$prim57545 = alloca %struct.ScmObj*, align 8
%argslist56224$_37last471292 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48722, %struct.ScmObj* %argslist56224$_37last471291)
store volatile %struct.ScmObj* %argslist56224$_37last471292, %struct.ScmObj** %stackaddr$prim57545, align 8
%clofunc57546 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last47129)
musttail call tailcc void %clofunc57546(%struct.ScmObj* %_37last47129, %struct.ScmObj* %argslist56224$_37last471292)
ret void
}

define tailcc void @proc_clo$ae48722(%struct.ScmObj* %env$ae48722,%struct.ScmObj* %current_45args56220) {
%stackaddr$env-ref57547 = alloca %struct.ScmObj*, align 8
%anf_45bind47291 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48722, i64 0)
store %struct.ScmObj* %anf_45bind47291, %struct.ScmObj** %stackaddr$env-ref57547
%stackaddr$env-ref57548 = alloca %struct.ScmObj*, align 8
%k47463 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48722, i64 1)
store %struct.ScmObj* %k47463, %struct.ScmObj** %stackaddr$env-ref57548
%stackaddr$prim57549 = alloca %struct.ScmObj*, align 8
%_95k47466 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56220)
store volatile %struct.ScmObj* %_95k47466, %struct.ScmObj** %stackaddr$prim57549, align 8
%stackaddr$prim57550 = alloca %struct.ScmObj*, align 8
%current_45args56221 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56220)
store volatile %struct.ScmObj* %current_45args56221, %struct.ScmObj** %stackaddr$prim57550, align 8
%stackaddr$prim57551 = alloca %struct.ScmObj*, align 8
%anf_45bind47292 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56221)
store volatile %struct.ScmObj* %anf_45bind47292, %struct.ScmObj** %stackaddr$prim57551, align 8
%stackaddr$prim57552 = alloca %struct.ScmObj*, align 8
%cpsprim47467 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47291, %struct.ScmObj* %anf_45bind47292)
store volatile %struct.ScmObj* %cpsprim47467, %struct.ScmObj** %stackaddr$prim57552, align 8
%ae48727 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56223$k474630 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57553 = alloca %struct.ScmObj*, align 8
%argslist56223$k474631 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47467, %struct.ScmObj* %argslist56223$k474630)
store volatile %struct.ScmObj* %argslist56223$k474631, %struct.ScmObj** %stackaddr$prim57553, align 8
%stackaddr$prim57554 = alloca %struct.ScmObj*, align 8
%argslist56223$k474632 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48727, %struct.ScmObj* %argslist56223$k474631)
store volatile %struct.ScmObj* %argslist56223$k474632, %struct.ScmObj** %stackaddr$prim57554, align 8
%clofunc57555 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47463)
musttail call tailcc void %clofunc57555(%struct.ScmObj* %k47463, %struct.ScmObj* %argslist56223$k474632)
ret void
}

define tailcc void @proc_clo$ae48622(%struct.ScmObj* %env$ae48622,%struct.ScmObj* %current_45args56228) {
%stackaddr$env-ref57556 = alloca %struct.ScmObj*, align 8
%_37foldr147107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48622, i64 0)
store %struct.ScmObj* %_37foldr147107, %struct.ScmObj** %stackaddr$env-ref57556
%stackaddr$prim57557 = alloca %struct.ScmObj*, align 8
%k47469 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56228)
store volatile %struct.ScmObj* %k47469, %struct.ScmObj** %stackaddr$prim57557, align 8
%stackaddr$prim57558 = alloca %struct.ScmObj*, align 8
%current_45args56229 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56228)
store volatile %struct.ScmObj* %current_45args56229, %struct.ScmObj** %stackaddr$prim57558, align 8
%stackaddr$prim57559 = alloca %struct.ScmObj*, align 8
%f47140 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56229)
store volatile %struct.ScmObj* %f47140, %struct.ScmObj** %stackaddr$prim57559, align 8
%stackaddr$prim57560 = alloca %struct.ScmObj*, align 8
%current_45args56230 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56229)
store volatile %struct.ScmObj* %current_45args56230, %struct.ScmObj** %stackaddr$prim57560, align 8
%stackaddr$prim57561 = alloca %struct.ScmObj*, align 8
%lst47139 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56230)
store volatile %struct.ScmObj* %lst47139, %struct.ScmObj** %stackaddr$prim57561, align 8
%stackaddr$makeclosure57562 = alloca %struct.ScmObj*, align 8
%fptrToInt57563 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48623 to i64
%ae48623 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57563)
store volatile %struct.ScmObj* %ae48623, %struct.ScmObj** %stackaddr$makeclosure57562, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48623, %struct.ScmObj* %lst47139, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48623, %struct.ScmObj* %_37foldr147107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48623, %struct.ScmObj* %k47469, i64 2)
%ae48624 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57564 = alloca %struct.ScmObj*, align 8
%fptrToInt57565 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48625 to i64
%ae48625 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57565)
store volatile %struct.ScmObj* %ae48625, %struct.ScmObj** %stackaddr$makeclosure57564, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48625, %struct.ScmObj* %f47140, i64 0)
%argslist56245$ae486230 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57566 = alloca %struct.ScmObj*, align 8
%argslist56245$ae486231 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48625, %struct.ScmObj* %argslist56245$ae486230)
store volatile %struct.ScmObj* %argslist56245$ae486231, %struct.ScmObj** %stackaddr$prim57566, align 8
%stackaddr$prim57567 = alloca %struct.ScmObj*, align 8
%argslist56245$ae486232 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48624, %struct.ScmObj* %argslist56245$ae486231)
store volatile %struct.ScmObj* %argslist56245$ae486232, %struct.ScmObj** %stackaddr$prim57567, align 8
%clofunc57568 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48623)
musttail call tailcc void %clofunc57568(%struct.ScmObj* %ae48623, %struct.ScmObj* %argslist56245$ae486232)
ret void
}

define tailcc void @proc_clo$ae48623(%struct.ScmObj* %env$ae48623,%struct.ScmObj* %current_45args56232) {
%stackaddr$env-ref57569 = alloca %struct.ScmObj*, align 8
%lst47139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48623, i64 0)
store %struct.ScmObj* %lst47139, %struct.ScmObj** %stackaddr$env-ref57569
%stackaddr$env-ref57570 = alloca %struct.ScmObj*, align 8
%_37foldr147107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48623, i64 1)
store %struct.ScmObj* %_37foldr147107, %struct.ScmObj** %stackaddr$env-ref57570
%stackaddr$env-ref57571 = alloca %struct.ScmObj*, align 8
%k47469 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48623, i64 2)
store %struct.ScmObj* %k47469, %struct.ScmObj** %stackaddr$env-ref57571
%stackaddr$prim57572 = alloca %struct.ScmObj*, align 8
%_95k47470 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56232)
store volatile %struct.ScmObj* %_95k47470, %struct.ScmObj** %stackaddr$prim57572, align 8
%stackaddr$prim57573 = alloca %struct.ScmObj*, align 8
%current_45args56233 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56232)
store volatile %struct.ScmObj* %current_45args56233, %struct.ScmObj** %stackaddr$prim57573, align 8
%stackaddr$prim57574 = alloca %struct.ScmObj*, align 8
%anf_45bind47289 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56233)
store volatile %struct.ScmObj* %anf_45bind47289, %struct.ScmObj** %stackaddr$prim57574, align 8
%ae48657 = call %struct.ScmObj* @const_init_null()
%argslist56235$_37foldr1471070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57575 = alloca %struct.ScmObj*, align 8
%argslist56235$_37foldr1471071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47139, %struct.ScmObj* %argslist56235$_37foldr1471070)
store volatile %struct.ScmObj* %argslist56235$_37foldr1471071, %struct.ScmObj** %stackaddr$prim57575, align 8
%stackaddr$prim57576 = alloca %struct.ScmObj*, align 8
%argslist56235$_37foldr1471072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48657, %struct.ScmObj* %argslist56235$_37foldr1471071)
store volatile %struct.ScmObj* %argslist56235$_37foldr1471072, %struct.ScmObj** %stackaddr$prim57576, align 8
%stackaddr$prim57577 = alloca %struct.ScmObj*, align 8
%argslist56235$_37foldr1471073 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47289, %struct.ScmObj* %argslist56235$_37foldr1471072)
store volatile %struct.ScmObj* %argslist56235$_37foldr1471073, %struct.ScmObj** %stackaddr$prim57577, align 8
%stackaddr$prim57578 = alloca %struct.ScmObj*, align 8
%argslist56235$_37foldr1471074 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47469, %struct.ScmObj* %argslist56235$_37foldr1471073)
store volatile %struct.ScmObj* %argslist56235$_37foldr1471074, %struct.ScmObj** %stackaddr$prim57578, align 8
%clofunc57579 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147107)
musttail call tailcc void %clofunc57579(%struct.ScmObj* %_37foldr147107, %struct.ScmObj* %argslist56235$_37foldr1471074)
ret void
}

define tailcc void @proc_clo$ae48625(%struct.ScmObj* %env$ae48625,%struct.ScmObj* %current_45args56236) {
%stackaddr$env-ref57580 = alloca %struct.ScmObj*, align 8
%f47140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48625, i64 0)
store %struct.ScmObj* %f47140, %struct.ScmObj** %stackaddr$env-ref57580
%stackaddr$prim57581 = alloca %struct.ScmObj*, align 8
%k47471 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56236)
store volatile %struct.ScmObj* %k47471, %struct.ScmObj** %stackaddr$prim57581, align 8
%stackaddr$prim57582 = alloca %struct.ScmObj*, align 8
%current_45args56237 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56236)
store volatile %struct.ScmObj* %current_45args56237, %struct.ScmObj** %stackaddr$prim57582, align 8
%stackaddr$prim57583 = alloca %struct.ScmObj*, align 8
%v47142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56237)
store volatile %struct.ScmObj* %v47142, %struct.ScmObj** %stackaddr$prim57583, align 8
%stackaddr$prim57584 = alloca %struct.ScmObj*, align 8
%current_45args56238 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56237)
store volatile %struct.ScmObj* %current_45args56238, %struct.ScmObj** %stackaddr$prim57584, align 8
%stackaddr$prim57585 = alloca %struct.ScmObj*, align 8
%r47141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56238)
store volatile %struct.ScmObj* %r47141, %struct.ScmObj** %stackaddr$prim57585, align 8
%stackaddr$makeclosure57586 = alloca %struct.ScmObj*, align 8
%fptrToInt57587 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48627 to i64
%ae48627 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57587)
store volatile %struct.ScmObj* %ae48627, %struct.ScmObj** %stackaddr$makeclosure57586, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48627, %struct.ScmObj* %r47141, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48627, %struct.ScmObj* %k47471, i64 1)
%argslist56244$f471400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57588 = alloca %struct.ScmObj*, align 8
%argslist56244$f471401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v47142, %struct.ScmObj* %argslist56244$f471400)
store volatile %struct.ScmObj* %argslist56244$f471401, %struct.ScmObj** %stackaddr$prim57588, align 8
%stackaddr$prim57589 = alloca %struct.ScmObj*, align 8
%argslist56244$f471402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48627, %struct.ScmObj* %argslist56244$f471401)
store volatile %struct.ScmObj* %argslist56244$f471402, %struct.ScmObj** %stackaddr$prim57589, align 8
%clofunc57590 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47140)
musttail call tailcc void %clofunc57590(%struct.ScmObj* %f47140, %struct.ScmObj* %argslist56244$f471402)
ret void
}

define tailcc void @proc_clo$ae48627(%struct.ScmObj* %env$ae48627,%struct.ScmObj* %current_45args56240) {
%stackaddr$env-ref57591 = alloca %struct.ScmObj*, align 8
%r47141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48627, i64 0)
store %struct.ScmObj* %r47141, %struct.ScmObj** %stackaddr$env-ref57591
%stackaddr$env-ref57592 = alloca %struct.ScmObj*, align 8
%k47471 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48627, i64 1)
store %struct.ScmObj* %k47471, %struct.ScmObj** %stackaddr$env-ref57592
%stackaddr$prim57593 = alloca %struct.ScmObj*, align 8
%_95k47472 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56240)
store volatile %struct.ScmObj* %_95k47472, %struct.ScmObj** %stackaddr$prim57593, align 8
%stackaddr$prim57594 = alloca %struct.ScmObj*, align 8
%current_45args56241 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56240)
store volatile %struct.ScmObj* %current_45args56241, %struct.ScmObj** %stackaddr$prim57594, align 8
%stackaddr$prim57595 = alloca %struct.ScmObj*, align 8
%anf_45bind47288 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56241)
store volatile %struct.ScmObj* %anf_45bind47288, %struct.ScmObj** %stackaddr$prim57595, align 8
%stackaddr$prim57596 = alloca %struct.ScmObj*, align 8
%cpsprim47473 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47288, %struct.ScmObj* %r47141)
store volatile %struct.ScmObj* %cpsprim47473, %struct.ScmObj** %stackaddr$prim57596, align 8
%ae48632 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56243$k474710 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57597 = alloca %struct.ScmObj*, align 8
%argslist56243$k474711 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47473, %struct.ScmObj* %argslist56243$k474710)
store volatile %struct.ScmObj* %argslist56243$k474711, %struct.ScmObj** %stackaddr$prim57597, align 8
%stackaddr$prim57598 = alloca %struct.ScmObj*, align 8
%argslist56243$k474712 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48632, %struct.ScmObj* %argslist56243$k474711)
store volatile %struct.ScmObj* %argslist56243$k474712, %struct.ScmObj** %stackaddr$prim57598, align 8
%clofunc57599 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47471)
musttail call tailcc void %clofunc57599(%struct.ScmObj* %k47471, %struct.ScmObj* %argslist56243$k474712)
ret void
}

define tailcc void @proc_clo$ae48236(%struct.ScmObj* %env$ae48236,%struct.ScmObj* %current_45args56248) {
%stackaddr$env-ref57600 = alloca %struct.ScmObj*, align 8
%_37foldr147107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48236, i64 0)
store %struct.ScmObj* %_37foldr147107, %struct.ScmObj** %stackaddr$env-ref57600
%stackaddr$env-ref57601 = alloca %struct.ScmObj*, align 8
%_37map147103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48236, i64 1)
store %struct.ScmObj* %_37map147103, %struct.ScmObj** %stackaddr$env-ref57601
%stackaddr$prim57602 = alloca %struct.ScmObj*, align 8
%k47474 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56248)
store volatile %struct.ScmObj* %k47474, %struct.ScmObj** %stackaddr$prim57602, align 8
%stackaddr$prim57603 = alloca %struct.ScmObj*, align 8
%current_45args56249 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56248)
store volatile %struct.ScmObj* %current_45args56249, %struct.ScmObj** %stackaddr$prim57603, align 8
%stackaddr$prim57604 = alloca %struct.ScmObj*, align 8
%_37foldr47113 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56249)
store volatile %struct.ScmObj* %_37foldr47113, %struct.ScmObj** %stackaddr$prim57604, align 8
%ae48238 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57605 = alloca %struct.ScmObj*, align 8
%fptrToInt57606 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48239 to i64
%ae48239 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57606)
store volatile %struct.ScmObj* %ae48239, %struct.ScmObj** %stackaddr$makeclosure57605, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48239, %struct.ScmObj* %_37foldr147107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48239, %struct.ScmObj* %_37map147103, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48239, %struct.ScmObj* %_37foldr47113, i64 2)
%argslist56306$k474740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57607 = alloca %struct.ScmObj*, align 8
%argslist56306$k474741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48239, %struct.ScmObj* %argslist56306$k474740)
store volatile %struct.ScmObj* %argslist56306$k474741, %struct.ScmObj** %stackaddr$prim57607, align 8
%stackaddr$prim57608 = alloca %struct.ScmObj*, align 8
%argslist56306$k474742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48238, %struct.ScmObj* %argslist56306$k474741)
store volatile %struct.ScmObj* %argslist56306$k474742, %struct.ScmObj** %stackaddr$prim57608, align 8
%clofunc57609 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47474)
musttail call tailcc void %clofunc57609(%struct.ScmObj* %k47474, %struct.ScmObj* %argslist56306$k474742)
ret void
}

define tailcc void @proc_clo$ae48239(%struct.ScmObj* %env$ae48239,%struct.ScmObj* %args4711447475) {
%stackaddr$env-ref57610 = alloca %struct.ScmObj*, align 8
%_37foldr147107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48239, i64 0)
store %struct.ScmObj* %_37foldr147107, %struct.ScmObj** %stackaddr$env-ref57610
%stackaddr$env-ref57611 = alloca %struct.ScmObj*, align 8
%_37map147103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48239, i64 1)
store %struct.ScmObj* %_37map147103, %struct.ScmObj** %stackaddr$env-ref57611
%stackaddr$env-ref57612 = alloca %struct.ScmObj*, align 8
%_37foldr47113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48239, i64 2)
store %struct.ScmObj* %_37foldr47113, %struct.ScmObj** %stackaddr$env-ref57612
%stackaddr$prim57613 = alloca %struct.ScmObj*, align 8
%k47476 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4711447475)
store volatile %struct.ScmObj* %k47476, %struct.ScmObj** %stackaddr$prim57613, align 8
%stackaddr$prim57614 = alloca %struct.ScmObj*, align 8
%args47114 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4711447475)
store volatile %struct.ScmObj* %args47114, %struct.ScmObj** %stackaddr$prim57614, align 8
%stackaddr$prim57615 = alloca %struct.ScmObj*, align 8
%f47117 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47114)
store volatile %struct.ScmObj* %f47117, %struct.ScmObj** %stackaddr$prim57615, align 8
%stackaddr$prim57616 = alloca %struct.ScmObj*, align 8
%anf_45bind47275 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47114)
store volatile %struct.ScmObj* %anf_45bind47275, %struct.ScmObj** %stackaddr$prim57616, align 8
%stackaddr$prim57617 = alloca %struct.ScmObj*, align 8
%acc47116 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47275)
store volatile %struct.ScmObj* %acc47116, %struct.ScmObj** %stackaddr$prim57617, align 8
%stackaddr$prim57618 = alloca %struct.ScmObj*, align 8
%anf_45bind47276 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47114)
store volatile %struct.ScmObj* %anf_45bind47276, %struct.ScmObj** %stackaddr$prim57618, align 8
%stackaddr$prim57619 = alloca %struct.ScmObj*, align 8
%lsts47115 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47276)
store volatile %struct.ScmObj* %lsts47115, %struct.ScmObj** %stackaddr$prim57619, align 8
%stackaddr$makeclosure57620 = alloca %struct.ScmObj*, align 8
%fptrToInt57621 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48247 to i64
%ae48247 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt57621)
store volatile %struct.ScmObj* %ae48247, %struct.ScmObj** %stackaddr$makeclosure57620, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48247, %struct.ScmObj* %k47476, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48247, %struct.ScmObj* %_37foldr147107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48247, %struct.ScmObj* %_37map147103, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48247, %struct.ScmObj* %f47117, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48247, %struct.ScmObj* %acc47116, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48247, %struct.ScmObj* %lsts47115, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48247, %struct.ScmObj* %_37foldr47113, i64 6)
%ae48248 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57622 = alloca %struct.ScmObj*, align 8
%fptrToInt57623 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48249 to i64
%ae48249 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57623)
store volatile %struct.ScmObj* %ae48249, %struct.ScmObj** %stackaddr$makeclosure57622, align 8
%argslist56305$ae482470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57624 = alloca %struct.ScmObj*, align 8
%argslist56305$ae482471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48249, %struct.ScmObj* %argslist56305$ae482470)
store volatile %struct.ScmObj* %argslist56305$ae482471, %struct.ScmObj** %stackaddr$prim57624, align 8
%stackaddr$prim57625 = alloca %struct.ScmObj*, align 8
%argslist56305$ae482472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48248, %struct.ScmObj* %argslist56305$ae482471)
store volatile %struct.ScmObj* %argslist56305$ae482472, %struct.ScmObj** %stackaddr$prim57625, align 8
%clofunc57626 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48247)
musttail call tailcc void %clofunc57626(%struct.ScmObj* %ae48247, %struct.ScmObj* %argslist56305$ae482472)
ret void
}

define tailcc void @proc_clo$ae48247(%struct.ScmObj* %env$ae48247,%struct.ScmObj* %current_45args56251) {
%stackaddr$env-ref57627 = alloca %struct.ScmObj*, align 8
%k47476 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48247, i64 0)
store %struct.ScmObj* %k47476, %struct.ScmObj** %stackaddr$env-ref57627
%stackaddr$env-ref57628 = alloca %struct.ScmObj*, align 8
%_37foldr147107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48247, i64 1)
store %struct.ScmObj* %_37foldr147107, %struct.ScmObj** %stackaddr$env-ref57628
%stackaddr$env-ref57629 = alloca %struct.ScmObj*, align 8
%_37map147103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48247, i64 2)
store %struct.ScmObj* %_37map147103, %struct.ScmObj** %stackaddr$env-ref57629
%stackaddr$env-ref57630 = alloca %struct.ScmObj*, align 8
%f47117 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48247, i64 3)
store %struct.ScmObj* %f47117, %struct.ScmObj** %stackaddr$env-ref57630
%stackaddr$env-ref57631 = alloca %struct.ScmObj*, align 8
%acc47116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48247, i64 4)
store %struct.ScmObj* %acc47116, %struct.ScmObj** %stackaddr$env-ref57631
%stackaddr$env-ref57632 = alloca %struct.ScmObj*, align 8
%lsts47115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48247, i64 5)
store %struct.ScmObj* %lsts47115, %struct.ScmObj** %stackaddr$env-ref57632
%stackaddr$env-ref57633 = alloca %struct.ScmObj*, align 8
%_37foldr47113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48247, i64 6)
store %struct.ScmObj* %_37foldr47113, %struct.ScmObj** %stackaddr$env-ref57633
%stackaddr$prim57634 = alloca %struct.ScmObj*, align 8
%_95k47477 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56251)
store volatile %struct.ScmObj* %_95k47477, %struct.ScmObj** %stackaddr$prim57634, align 8
%stackaddr$prim57635 = alloca %struct.ScmObj*, align 8
%current_45args56252 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56251)
store volatile %struct.ScmObj* %current_45args56252, %struct.ScmObj** %stackaddr$prim57635, align 8
%stackaddr$prim57636 = alloca %struct.ScmObj*, align 8
%anf_45bind47277 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56252)
store volatile %struct.ScmObj* %anf_45bind47277, %struct.ScmObj** %stackaddr$prim57636, align 8
%stackaddr$makeclosure57637 = alloca %struct.ScmObj*, align 8
%fptrToInt57638 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48279 to i64
%ae48279 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt57638)
store volatile %struct.ScmObj* %ae48279, %struct.ScmObj** %stackaddr$makeclosure57637, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48279, %struct.ScmObj* %k47476, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48279, %struct.ScmObj* %_37foldr147107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48279, %struct.ScmObj* %_37map147103, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48279, %struct.ScmObj* %f47117, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48279, %struct.ScmObj* %acc47116, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48279, %struct.ScmObj* %lsts47115, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48279, %struct.ScmObj* %_37foldr47113, i64 6)
%ae48281 = call %struct.ScmObj* @const_init_false()
%argslist56298$_37foldr1471070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57639 = alloca %struct.ScmObj*, align 8
%argslist56298$_37foldr1471071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47115, %struct.ScmObj* %argslist56298$_37foldr1471070)
store volatile %struct.ScmObj* %argslist56298$_37foldr1471071, %struct.ScmObj** %stackaddr$prim57639, align 8
%stackaddr$prim57640 = alloca %struct.ScmObj*, align 8
%argslist56298$_37foldr1471072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48281, %struct.ScmObj* %argslist56298$_37foldr1471071)
store volatile %struct.ScmObj* %argslist56298$_37foldr1471072, %struct.ScmObj** %stackaddr$prim57640, align 8
%stackaddr$prim57641 = alloca %struct.ScmObj*, align 8
%argslist56298$_37foldr1471073 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47277, %struct.ScmObj* %argslist56298$_37foldr1471072)
store volatile %struct.ScmObj* %argslist56298$_37foldr1471073, %struct.ScmObj** %stackaddr$prim57641, align 8
%stackaddr$prim57642 = alloca %struct.ScmObj*, align 8
%argslist56298$_37foldr1471074 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48279, %struct.ScmObj* %argslist56298$_37foldr1471073)
store volatile %struct.ScmObj* %argslist56298$_37foldr1471074, %struct.ScmObj** %stackaddr$prim57642, align 8
%clofunc57643 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147107)
musttail call tailcc void %clofunc57643(%struct.ScmObj* %_37foldr147107, %struct.ScmObj* %argslist56298$_37foldr1471074)
ret void
}

define tailcc void @proc_clo$ae48279(%struct.ScmObj* %env$ae48279,%struct.ScmObj* %current_45args56254) {
%stackaddr$env-ref57644 = alloca %struct.ScmObj*, align 8
%k47476 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48279, i64 0)
store %struct.ScmObj* %k47476, %struct.ScmObj** %stackaddr$env-ref57644
%stackaddr$env-ref57645 = alloca %struct.ScmObj*, align 8
%_37foldr147107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48279, i64 1)
store %struct.ScmObj* %_37foldr147107, %struct.ScmObj** %stackaddr$env-ref57645
%stackaddr$env-ref57646 = alloca %struct.ScmObj*, align 8
%_37map147103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48279, i64 2)
store %struct.ScmObj* %_37map147103, %struct.ScmObj** %stackaddr$env-ref57646
%stackaddr$env-ref57647 = alloca %struct.ScmObj*, align 8
%f47117 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48279, i64 3)
store %struct.ScmObj* %f47117, %struct.ScmObj** %stackaddr$env-ref57647
%stackaddr$env-ref57648 = alloca %struct.ScmObj*, align 8
%acc47116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48279, i64 4)
store %struct.ScmObj* %acc47116, %struct.ScmObj** %stackaddr$env-ref57648
%stackaddr$env-ref57649 = alloca %struct.ScmObj*, align 8
%lsts47115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48279, i64 5)
store %struct.ScmObj* %lsts47115, %struct.ScmObj** %stackaddr$env-ref57649
%stackaddr$env-ref57650 = alloca %struct.ScmObj*, align 8
%_37foldr47113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48279, i64 6)
store %struct.ScmObj* %_37foldr47113, %struct.ScmObj** %stackaddr$env-ref57650
%stackaddr$prim57651 = alloca %struct.ScmObj*, align 8
%_95k47478 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56254)
store volatile %struct.ScmObj* %_95k47478, %struct.ScmObj** %stackaddr$prim57651, align 8
%stackaddr$prim57652 = alloca %struct.ScmObj*, align 8
%current_45args56255 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56254)
store volatile %struct.ScmObj* %current_45args56255, %struct.ScmObj** %stackaddr$prim57652, align 8
%stackaddr$prim57653 = alloca %struct.ScmObj*, align 8
%anf_45bind47278 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56255)
store volatile %struct.ScmObj* %anf_45bind47278, %struct.ScmObj** %stackaddr$prim57653, align 8
%truthy$cmp57654 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47278)
%cmp$cmp57654 = icmp eq i64 %truthy$cmp57654, 1
br i1 %cmp$cmp57654, label %truebranch$cmp57654, label %falsebranch$cmp57654
truebranch$cmp57654:
%ae48290 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56257$k474760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57655 = alloca %struct.ScmObj*, align 8
%argslist56257$k474761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47116, %struct.ScmObj* %argslist56257$k474760)
store volatile %struct.ScmObj* %argslist56257$k474761, %struct.ScmObj** %stackaddr$prim57655, align 8
%stackaddr$prim57656 = alloca %struct.ScmObj*, align 8
%argslist56257$k474762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48290, %struct.ScmObj* %argslist56257$k474761)
store volatile %struct.ScmObj* %argslist56257$k474762, %struct.ScmObj** %stackaddr$prim57656, align 8
%clofunc57657 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47476)
musttail call tailcc void %clofunc57657(%struct.ScmObj* %k47476, %struct.ScmObj* %argslist56257$k474762)
ret void
falsebranch$cmp57654:
%stackaddr$makeclosure57658 = alloca %struct.ScmObj*, align 8
%fptrToInt57659 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48295 to i64
%ae48295 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt57659)
store volatile %struct.ScmObj* %ae48295, %struct.ScmObj** %stackaddr$makeclosure57658, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48295, %struct.ScmObj* %k47476, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48295, %struct.ScmObj* %_37foldr147107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48295, %struct.ScmObj* %_37map147103, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48295, %struct.ScmObj* %f47117, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48295, %struct.ScmObj* %acc47116, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48295, %struct.ScmObj* %lsts47115, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48295, %struct.ScmObj* %_37foldr47113, i64 6)
%ae48296 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57660 = alloca %struct.ScmObj*, align 8
%fptrToInt57661 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48297 to i64
%ae48297 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57661)
store volatile %struct.ScmObj* %ae48297, %struct.ScmObj** %stackaddr$makeclosure57660, align 8
%argslist56297$ae482950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57662 = alloca %struct.ScmObj*, align 8
%argslist56297$ae482951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48297, %struct.ScmObj* %argslist56297$ae482950)
store volatile %struct.ScmObj* %argslist56297$ae482951, %struct.ScmObj** %stackaddr$prim57662, align 8
%stackaddr$prim57663 = alloca %struct.ScmObj*, align 8
%argslist56297$ae482952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48296, %struct.ScmObj* %argslist56297$ae482951)
store volatile %struct.ScmObj* %argslist56297$ae482952, %struct.ScmObj** %stackaddr$prim57663, align 8
%clofunc57664 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48295)
musttail call tailcc void %clofunc57664(%struct.ScmObj* %ae48295, %struct.ScmObj* %argslist56297$ae482952)
ret void
}

define tailcc void @proc_clo$ae48295(%struct.ScmObj* %env$ae48295,%struct.ScmObj* %current_45args56258) {
%stackaddr$env-ref57665 = alloca %struct.ScmObj*, align 8
%k47476 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48295, i64 0)
store %struct.ScmObj* %k47476, %struct.ScmObj** %stackaddr$env-ref57665
%stackaddr$env-ref57666 = alloca %struct.ScmObj*, align 8
%_37foldr147107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48295, i64 1)
store %struct.ScmObj* %_37foldr147107, %struct.ScmObj** %stackaddr$env-ref57666
%stackaddr$env-ref57667 = alloca %struct.ScmObj*, align 8
%_37map147103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48295, i64 2)
store %struct.ScmObj* %_37map147103, %struct.ScmObj** %stackaddr$env-ref57667
%stackaddr$env-ref57668 = alloca %struct.ScmObj*, align 8
%f47117 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48295, i64 3)
store %struct.ScmObj* %f47117, %struct.ScmObj** %stackaddr$env-ref57668
%stackaddr$env-ref57669 = alloca %struct.ScmObj*, align 8
%acc47116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48295, i64 4)
store %struct.ScmObj* %acc47116, %struct.ScmObj** %stackaddr$env-ref57669
%stackaddr$env-ref57670 = alloca %struct.ScmObj*, align 8
%lsts47115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48295, i64 5)
store %struct.ScmObj* %lsts47115, %struct.ScmObj** %stackaddr$env-ref57670
%stackaddr$env-ref57671 = alloca %struct.ScmObj*, align 8
%_37foldr47113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48295, i64 6)
store %struct.ScmObj* %_37foldr47113, %struct.ScmObj** %stackaddr$env-ref57671
%stackaddr$prim57672 = alloca %struct.ScmObj*, align 8
%_95k47479 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56258)
store volatile %struct.ScmObj* %_95k47479, %struct.ScmObj** %stackaddr$prim57672, align 8
%stackaddr$prim57673 = alloca %struct.ScmObj*, align 8
%current_45args56259 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56258)
store volatile %struct.ScmObj* %current_45args56259, %struct.ScmObj** %stackaddr$prim57673, align 8
%stackaddr$prim57674 = alloca %struct.ScmObj*, align 8
%anf_45bind47279 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56259)
store volatile %struct.ScmObj* %anf_45bind47279, %struct.ScmObj** %stackaddr$prim57674, align 8
%stackaddr$makeclosure57675 = alloca %struct.ScmObj*, align 8
%fptrToInt57676 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48316 to i64
%ae48316 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt57676)
store volatile %struct.ScmObj* %ae48316, %struct.ScmObj** %stackaddr$makeclosure57675, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48316, %struct.ScmObj* %k47476, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48316, %struct.ScmObj* %_37foldr147107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48316, %struct.ScmObj* %_37map147103, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48316, %struct.ScmObj* %f47117, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48316, %struct.ScmObj* %acc47116, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48316, %struct.ScmObj* %lsts47115, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48316, %struct.ScmObj* %_37foldr47113, i64 6)
%argslist56292$_37map1471030 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57677 = alloca %struct.ScmObj*, align 8
%argslist56292$_37map1471031 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47115, %struct.ScmObj* %argslist56292$_37map1471030)
store volatile %struct.ScmObj* %argslist56292$_37map1471031, %struct.ScmObj** %stackaddr$prim57677, align 8
%stackaddr$prim57678 = alloca %struct.ScmObj*, align 8
%argslist56292$_37map1471032 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47279, %struct.ScmObj* %argslist56292$_37map1471031)
store volatile %struct.ScmObj* %argslist56292$_37map1471032, %struct.ScmObj** %stackaddr$prim57678, align 8
%stackaddr$prim57679 = alloca %struct.ScmObj*, align 8
%argslist56292$_37map1471033 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48316, %struct.ScmObj* %argslist56292$_37map1471032)
store volatile %struct.ScmObj* %argslist56292$_37map1471033, %struct.ScmObj** %stackaddr$prim57679, align 8
%clofunc57680 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147103)
musttail call tailcc void %clofunc57680(%struct.ScmObj* %_37map147103, %struct.ScmObj* %argslist56292$_37map1471033)
ret void
}

define tailcc void @proc_clo$ae48316(%struct.ScmObj* %env$ae48316,%struct.ScmObj* %current_45args56261) {
%stackaddr$env-ref57681 = alloca %struct.ScmObj*, align 8
%k47476 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48316, i64 0)
store %struct.ScmObj* %k47476, %struct.ScmObj** %stackaddr$env-ref57681
%stackaddr$env-ref57682 = alloca %struct.ScmObj*, align 8
%_37foldr147107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48316, i64 1)
store %struct.ScmObj* %_37foldr147107, %struct.ScmObj** %stackaddr$env-ref57682
%stackaddr$env-ref57683 = alloca %struct.ScmObj*, align 8
%_37map147103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48316, i64 2)
store %struct.ScmObj* %_37map147103, %struct.ScmObj** %stackaddr$env-ref57683
%stackaddr$env-ref57684 = alloca %struct.ScmObj*, align 8
%f47117 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48316, i64 3)
store %struct.ScmObj* %f47117, %struct.ScmObj** %stackaddr$env-ref57684
%stackaddr$env-ref57685 = alloca %struct.ScmObj*, align 8
%acc47116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48316, i64 4)
store %struct.ScmObj* %acc47116, %struct.ScmObj** %stackaddr$env-ref57685
%stackaddr$env-ref57686 = alloca %struct.ScmObj*, align 8
%lsts47115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48316, i64 5)
store %struct.ScmObj* %lsts47115, %struct.ScmObj** %stackaddr$env-ref57686
%stackaddr$env-ref57687 = alloca %struct.ScmObj*, align 8
%_37foldr47113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48316, i64 6)
store %struct.ScmObj* %_37foldr47113, %struct.ScmObj** %stackaddr$env-ref57687
%stackaddr$prim57688 = alloca %struct.ScmObj*, align 8
%_95k47480 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56261)
store volatile %struct.ScmObj* %_95k47480, %struct.ScmObj** %stackaddr$prim57688, align 8
%stackaddr$prim57689 = alloca %struct.ScmObj*, align 8
%current_45args56262 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56261)
store volatile %struct.ScmObj* %current_45args56262, %struct.ScmObj** %stackaddr$prim57689, align 8
%stackaddr$prim57690 = alloca %struct.ScmObj*, align 8
%lsts_4347122 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56262)
store volatile %struct.ScmObj* %lsts_4347122, %struct.ScmObj** %stackaddr$prim57690, align 8
%stackaddr$makeclosure57691 = alloca %struct.ScmObj*, align 8
%fptrToInt57692 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48319 to i64
%ae48319 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt57692)
store volatile %struct.ScmObj* %ae48319, %struct.ScmObj** %stackaddr$makeclosure57691, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48319, %struct.ScmObj* %k47476, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48319, %struct.ScmObj* %_37foldr147107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48319, %struct.ScmObj* %lsts_4347122, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48319, %struct.ScmObj* %_37map147103, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48319, %struct.ScmObj* %f47117, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48319, %struct.ScmObj* %acc47116, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48319, %struct.ScmObj* %lsts47115, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae48319, %struct.ScmObj* %_37foldr47113, i64 7)
%ae48320 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57693 = alloca %struct.ScmObj*, align 8
%fptrToInt57694 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48321 to i64
%ae48321 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57694)
store volatile %struct.ScmObj* %ae48321, %struct.ScmObj** %stackaddr$makeclosure57693, align 8
%argslist56291$ae483190 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57695 = alloca %struct.ScmObj*, align 8
%argslist56291$ae483191 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48321, %struct.ScmObj* %argslist56291$ae483190)
store volatile %struct.ScmObj* %argslist56291$ae483191, %struct.ScmObj** %stackaddr$prim57695, align 8
%stackaddr$prim57696 = alloca %struct.ScmObj*, align 8
%argslist56291$ae483192 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48320, %struct.ScmObj* %argslist56291$ae483191)
store volatile %struct.ScmObj* %argslist56291$ae483192, %struct.ScmObj** %stackaddr$prim57696, align 8
%clofunc57697 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48319)
musttail call tailcc void %clofunc57697(%struct.ScmObj* %ae48319, %struct.ScmObj* %argslist56291$ae483192)
ret void
}

define tailcc void @proc_clo$ae48319(%struct.ScmObj* %env$ae48319,%struct.ScmObj* %current_45args56264) {
%stackaddr$env-ref57698 = alloca %struct.ScmObj*, align 8
%k47476 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48319, i64 0)
store %struct.ScmObj* %k47476, %struct.ScmObj** %stackaddr$env-ref57698
%stackaddr$env-ref57699 = alloca %struct.ScmObj*, align 8
%_37foldr147107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48319, i64 1)
store %struct.ScmObj* %_37foldr147107, %struct.ScmObj** %stackaddr$env-ref57699
%stackaddr$env-ref57700 = alloca %struct.ScmObj*, align 8
%lsts_4347122 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48319, i64 2)
store %struct.ScmObj* %lsts_4347122, %struct.ScmObj** %stackaddr$env-ref57700
%stackaddr$env-ref57701 = alloca %struct.ScmObj*, align 8
%_37map147103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48319, i64 3)
store %struct.ScmObj* %_37map147103, %struct.ScmObj** %stackaddr$env-ref57701
%stackaddr$env-ref57702 = alloca %struct.ScmObj*, align 8
%f47117 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48319, i64 4)
store %struct.ScmObj* %f47117, %struct.ScmObj** %stackaddr$env-ref57702
%stackaddr$env-ref57703 = alloca %struct.ScmObj*, align 8
%acc47116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48319, i64 5)
store %struct.ScmObj* %acc47116, %struct.ScmObj** %stackaddr$env-ref57703
%stackaddr$env-ref57704 = alloca %struct.ScmObj*, align 8
%lsts47115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48319, i64 6)
store %struct.ScmObj* %lsts47115, %struct.ScmObj** %stackaddr$env-ref57704
%stackaddr$env-ref57705 = alloca %struct.ScmObj*, align 8
%_37foldr47113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48319, i64 7)
store %struct.ScmObj* %_37foldr47113, %struct.ScmObj** %stackaddr$env-ref57705
%stackaddr$prim57706 = alloca %struct.ScmObj*, align 8
%_95k47481 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56264)
store volatile %struct.ScmObj* %_95k47481, %struct.ScmObj** %stackaddr$prim57706, align 8
%stackaddr$prim57707 = alloca %struct.ScmObj*, align 8
%current_45args56265 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56264)
store volatile %struct.ScmObj* %current_45args56265, %struct.ScmObj** %stackaddr$prim57707, align 8
%stackaddr$prim57708 = alloca %struct.ScmObj*, align 8
%anf_45bind47280 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56265)
store volatile %struct.ScmObj* %anf_45bind47280, %struct.ScmObj** %stackaddr$prim57708, align 8
%stackaddr$makeclosure57709 = alloca %struct.ScmObj*, align 8
%fptrToInt57710 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48340 to i64
%ae48340 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt57710)
store volatile %struct.ScmObj* %ae48340, %struct.ScmObj** %stackaddr$makeclosure57709, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48340, %struct.ScmObj* %k47476, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48340, %struct.ScmObj* %_37foldr147107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48340, %struct.ScmObj* %lsts_4347122, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48340, %struct.ScmObj* %f47117, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48340, %struct.ScmObj* %acc47116, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48340, %struct.ScmObj* %_37foldr47113, i64 5)
%argslist56286$_37map1471030 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57711 = alloca %struct.ScmObj*, align 8
%argslist56286$_37map1471031 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47115, %struct.ScmObj* %argslist56286$_37map1471030)
store volatile %struct.ScmObj* %argslist56286$_37map1471031, %struct.ScmObj** %stackaddr$prim57711, align 8
%stackaddr$prim57712 = alloca %struct.ScmObj*, align 8
%argslist56286$_37map1471032 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47280, %struct.ScmObj* %argslist56286$_37map1471031)
store volatile %struct.ScmObj* %argslist56286$_37map1471032, %struct.ScmObj** %stackaddr$prim57712, align 8
%stackaddr$prim57713 = alloca %struct.ScmObj*, align 8
%argslist56286$_37map1471033 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48340, %struct.ScmObj* %argslist56286$_37map1471032)
store volatile %struct.ScmObj* %argslist56286$_37map1471033, %struct.ScmObj** %stackaddr$prim57713, align 8
%clofunc57714 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147103)
musttail call tailcc void %clofunc57714(%struct.ScmObj* %_37map147103, %struct.ScmObj* %argslist56286$_37map1471033)
ret void
}

define tailcc void @proc_clo$ae48340(%struct.ScmObj* %env$ae48340,%struct.ScmObj* %current_45args56267) {
%stackaddr$env-ref57715 = alloca %struct.ScmObj*, align 8
%k47476 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48340, i64 0)
store %struct.ScmObj* %k47476, %struct.ScmObj** %stackaddr$env-ref57715
%stackaddr$env-ref57716 = alloca %struct.ScmObj*, align 8
%_37foldr147107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48340, i64 1)
store %struct.ScmObj* %_37foldr147107, %struct.ScmObj** %stackaddr$env-ref57716
%stackaddr$env-ref57717 = alloca %struct.ScmObj*, align 8
%lsts_4347122 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48340, i64 2)
store %struct.ScmObj* %lsts_4347122, %struct.ScmObj** %stackaddr$env-ref57717
%stackaddr$env-ref57718 = alloca %struct.ScmObj*, align 8
%f47117 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48340, i64 3)
store %struct.ScmObj* %f47117, %struct.ScmObj** %stackaddr$env-ref57718
%stackaddr$env-ref57719 = alloca %struct.ScmObj*, align 8
%acc47116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48340, i64 4)
store %struct.ScmObj* %acc47116, %struct.ScmObj** %stackaddr$env-ref57719
%stackaddr$env-ref57720 = alloca %struct.ScmObj*, align 8
%_37foldr47113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48340, i64 5)
store %struct.ScmObj* %_37foldr47113, %struct.ScmObj** %stackaddr$env-ref57720
%stackaddr$prim57721 = alloca %struct.ScmObj*, align 8
%_95k47482 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56267)
store volatile %struct.ScmObj* %_95k47482, %struct.ScmObj** %stackaddr$prim57721, align 8
%stackaddr$prim57722 = alloca %struct.ScmObj*, align 8
%current_45args56268 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56267)
store volatile %struct.ScmObj* %current_45args56268, %struct.ScmObj** %stackaddr$prim57722, align 8
%stackaddr$prim57723 = alloca %struct.ScmObj*, align 8
%vs47120 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56268)
store volatile %struct.ScmObj* %vs47120, %struct.ScmObj** %stackaddr$prim57723, align 8
%stackaddr$makeclosure57724 = alloca %struct.ScmObj*, align 8
%fptrToInt57725 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48343 to i64
%ae48343 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt57725)
store volatile %struct.ScmObj* %ae48343, %struct.ScmObj** %stackaddr$makeclosure57724, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48343, %struct.ScmObj* %k47476, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48343, %struct.ScmObj* %_37foldr147107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48343, %struct.ScmObj* %lsts_4347122, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48343, %struct.ScmObj* %vs47120, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48343, %struct.ScmObj* %f47117, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48343, %struct.ScmObj* %acc47116, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48343, %struct.ScmObj* %_37foldr47113, i64 6)
%ae48344 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57726 = alloca %struct.ScmObj*, align 8
%fptrToInt57727 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48345 to i64
%ae48345 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57727)
store volatile %struct.ScmObj* %ae48345, %struct.ScmObj** %stackaddr$makeclosure57726, align 8
%argslist56285$ae483430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57728 = alloca %struct.ScmObj*, align 8
%argslist56285$ae483431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48345, %struct.ScmObj* %argslist56285$ae483430)
store volatile %struct.ScmObj* %argslist56285$ae483431, %struct.ScmObj** %stackaddr$prim57728, align 8
%stackaddr$prim57729 = alloca %struct.ScmObj*, align 8
%argslist56285$ae483432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48344, %struct.ScmObj* %argslist56285$ae483431)
store volatile %struct.ScmObj* %argslist56285$ae483432, %struct.ScmObj** %stackaddr$prim57729, align 8
%clofunc57730 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48343)
musttail call tailcc void %clofunc57730(%struct.ScmObj* %ae48343, %struct.ScmObj* %argslist56285$ae483432)
ret void
}

define tailcc void @proc_clo$ae48343(%struct.ScmObj* %env$ae48343,%struct.ScmObj* %current_45args56270) {
%stackaddr$env-ref57731 = alloca %struct.ScmObj*, align 8
%k47476 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48343, i64 0)
store %struct.ScmObj* %k47476, %struct.ScmObj** %stackaddr$env-ref57731
%stackaddr$env-ref57732 = alloca %struct.ScmObj*, align 8
%_37foldr147107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48343, i64 1)
store %struct.ScmObj* %_37foldr147107, %struct.ScmObj** %stackaddr$env-ref57732
%stackaddr$env-ref57733 = alloca %struct.ScmObj*, align 8
%lsts_4347122 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48343, i64 2)
store %struct.ScmObj* %lsts_4347122, %struct.ScmObj** %stackaddr$env-ref57733
%stackaddr$env-ref57734 = alloca %struct.ScmObj*, align 8
%vs47120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48343, i64 3)
store %struct.ScmObj* %vs47120, %struct.ScmObj** %stackaddr$env-ref57734
%stackaddr$env-ref57735 = alloca %struct.ScmObj*, align 8
%f47117 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48343, i64 4)
store %struct.ScmObj* %f47117, %struct.ScmObj** %stackaddr$env-ref57735
%stackaddr$env-ref57736 = alloca %struct.ScmObj*, align 8
%acc47116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48343, i64 5)
store %struct.ScmObj* %acc47116, %struct.ScmObj** %stackaddr$env-ref57736
%stackaddr$env-ref57737 = alloca %struct.ScmObj*, align 8
%_37foldr47113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48343, i64 6)
store %struct.ScmObj* %_37foldr47113, %struct.ScmObj** %stackaddr$env-ref57737
%stackaddr$prim57738 = alloca %struct.ScmObj*, align 8
%_95k47483 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56270)
store volatile %struct.ScmObj* %_95k47483, %struct.ScmObj** %stackaddr$prim57738, align 8
%stackaddr$prim57739 = alloca %struct.ScmObj*, align 8
%current_45args56271 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56270)
store volatile %struct.ScmObj* %current_45args56271, %struct.ScmObj** %stackaddr$prim57739, align 8
%stackaddr$prim57740 = alloca %struct.ScmObj*, align 8
%anf_45bind47281 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56271)
store volatile %struct.ScmObj* %anf_45bind47281, %struct.ScmObj** %stackaddr$prim57740, align 8
%stackaddr$prim57741 = alloca %struct.ScmObj*, align 8
%anf_45bind47282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47116, %struct.ScmObj* %lsts_4347122)
store volatile %struct.ScmObj* %anf_45bind47282, %struct.ScmObj** %stackaddr$prim57741, align 8
%stackaddr$prim57742 = alloca %struct.ScmObj*, align 8
%anf_45bind47283 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47117, %struct.ScmObj* %anf_45bind47282)
store volatile %struct.ScmObj* %anf_45bind47283, %struct.ScmObj** %stackaddr$prim57742, align 8
%stackaddr$makeclosure57743 = alloca %struct.ScmObj*, align 8
%fptrToInt57744 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48369 to i64
%ae48369 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt57744)
store volatile %struct.ScmObj* %ae48369, %struct.ScmObj** %stackaddr$makeclosure57743, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48369, %struct.ScmObj* %k47476, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48369, %struct.ScmObj* %_37foldr147107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48369, %struct.ScmObj* %anf_45bind47281, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48369, %struct.ScmObj* %vs47120, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48369, %struct.ScmObj* %f47117, i64 4)
%stackaddr$prim57745 = alloca %struct.ScmObj*, align 8
%cpsargs47487 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48369, %struct.ScmObj* %anf_45bind47283)
store volatile %struct.ScmObj* %cpsargs47487, %struct.ScmObj** %stackaddr$prim57745, align 8
%clofunc57746 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr47113)
musttail call tailcc void %clofunc57746(%struct.ScmObj* %_37foldr47113, %struct.ScmObj* %cpsargs47487)
ret void
}

define tailcc void @proc_clo$ae48369(%struct.ScmObj* %env$ae48369,%struct.ScmObj* %current_45args56273) {
%stackaddr$env-ref57747 = alloca %struct.ScmObj*, align 8
%k47476 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48369, i64 0)
store %struct.ScmObj* %k47476, %struct.ScmObj** %stackaddr$env-ref57747
%stackaddr$env-ref57748 = alloca %struct.ScmObj*, align 8
%_37foldr147107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48369, i64 1)
store %struct.ScmObj* %_37foldr147107, %struct.ScmObj** %stackaddr$env-ref57748
%stackaddr$env-ref57749 = alloca %struct.ScmObj*, align 8
%anf_45bind47281 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48369, i64 2)
store %struct.ScmObj* %anf_45bind47281, %struct.ScmObj** %stackaddr$env-ref57749
%stackaddr$env-ref57750 = alloca %struct.ScmObj*, align 8
%vs47120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48369, i64 3)
store %struct.ScmObj* %vs47120, %struct.ScmObj** %stackaddr$env-ref57750
%stackaddr$env-ref57751 = alloca %struct.ScmObj*, align 8
%f47117 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48369, i64 4)
store %struct.ScmObj* %f47117, %struct.ScmObj** %stackaddr$env-ref57751
%stackaddr$prim57752 = alloca %struct.ScmObj*, align 8
%_95k47484 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56273)
store volatile %struct.ScmObj* %_95k47484, %struct.ScmObj** %stackaddr$prim57752, align 8
%stackaddr$prim57753 = alloca %struct.ScmObj*, align 8
%current_45args56274 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56273)
store volatile %struct.ScmObj* %current_45args56274, %struct.ScmObj** %stackaddr$prim57753, align 8
%stackaddr$prim57754 = alloca %struct.ScmObj*, align 8
%anf_45bind47284 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56274)
store volatile %struct.ScmObj* %anf_45bind47284, %struct.ScmObj** %stackaddr$prim57754, align 8
%ae48374 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57755 = alloca %struct.ScmObj*, align 8
%anf_45bind47285 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47284, %struct.ScmObj* %ae48374)
store volatile %struct.ScmObj* %anf_45bind47285, %struct.ScmObj** %stackaddr$prim57755, align 8
%stackaddr$makeclosure57756 = alloca %struct.ScmObj*, align 8
%fptrToInt57757 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48376 to i64
%ae48376 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57757)
store volatile %struct.ScmObj* %ae48376, %struct.ScmObj** %stackaddr$makeclosure57756, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48376, %struct.ScmObj* %k47476, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48376, %struct.ScmObj* %f47117, i64 1)
%argslist56279$_37foldr1471070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57758 = alloca %struct.ScmObj*, align 8
%argslist56279$_37foldr1471071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs47120, %struct.ScmObj* %argslist56279$_37foldr1471070)
store volatile %struct.ScmObj* %argslist56279$_37foldr1471071, %struct.ScmObj** %stackaddr$prim57758, align 8
%stackaddr$prim57759 = alloca %struct.ScmObj*, align 8
%argslist56279$_37foldr1471072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47285, %struct.ScmObj* %argslist56279$_37foldr1471071)
store volatile %struct.ScmObj* %argslist56279$_37foldr1471072, %struct.ScmObj** %stackaddr$prim57759, align 8
%stackaddr$prim57760 = alloca %struct.ScmObj*, align 8
%argslist56279$_37foldr1471073 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47281, %struct.ScmObj* %argslist56279$_37foldr1471072)
store volatile %struct.ScmObj* %argslist56279$_37foldr1471073, %struct.ScmObj** %stackaddr$prim57760, align 8
%stackaddr$prim57761 = alloca %struct.ScmObj*, align 8
%argslist56279$_37foldr1471074 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48376, %struct.ScmObj* %argslist56279$_37foldr1471073)
store volatile %struct.ScmObj* %argslist56279$_37foldr1471074, %struct.ScmObj** %stackaddr$prim57761, align 8
%clofunc57762 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147107)
musttail call tailcc void %clofunc57762(%struct.ScmObj* %_37foldr147107, %struct.ScmObj* %argslist56279$_37foldr1471074)
ret void
}

define tailcc void @proc_clo$ae48376(%struct.ScmObj* %env$ae48376,%struct.ScmObj* %current_45args56276) {
%stackaddr$env-ref57763 = alloca %struct.ScmObj*, align 8
%k47476 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48376, i64 0)
store %struct.ScmObj* %k47476, %struct.ScmObj** %stackaddr$env-ref57763
%stackaddr$env-ref57764 = alloca %struct.ScmObj*, align 8
%f47117 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48376, i64 1)
store %struct.ScmObj* %f47117, %struct.ScmObj** %stackaddr$env-ref57764
%stackaddr$prim57765 = alloca %struct.ScmObj*, align 8
%_95k47485 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56276)
store volatile %struct.ScmObj* %_95k47485, %struct.ScmObj** %stackaddr$prim57765, align 8
%stackaddr$prim57766 = alloca %struct.ScmObj*, align 8
%current_45args56277 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56276)
store volatile %struct.ScmObj* %current_45args56277, %struct.ScmObj** %stackaddr$prim57766, align 8
%stackaddr$prim57767 = alloca %struct.ScmObj*, align 8
%anf_45bind47286 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56277)
store volatile %struct.ScmObj* %anf_45bind47286, %struct.ScmObj** %stackaddr$prim57767, align 8
%stackaddr$prim57768 = alloca %struct.ScmObj*, align 8
%cpsargs47486 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47476, %struct.ScmObj* %anf_45bind47286)
store volatile %struct.ScmObj* %cpsargs47486, %struct.ScmObj** %stackaddr$prim57768, align 8
%clofunc57769 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47117)
musttail call tailcc void %clofunc57769(%struct.ScmObj* %f47117, %struct.ScmObj* %cpsargs47486)
ret void
}

define tailcc void @proc_clo$ae48345(%struct.ScmObj* %env$ae48345,%struct.ScmObj* %current_45args56280) {
%stackaddr$prim57770 = alloca %struct.ScmObj*, align 8
%k47488 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56280)
store volatile %struct.ScmObj* %k47488, %struct.ScmObj** %stackaddr$prim57770, align 8
%stackaddr$prim57771 = alloca %struct.ScmObj*, align 8
%current_45args56281 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56280)
store volatile %struct.ScmObj* %current_45args56281, %struct.ScmObj** %stackaddr$prim57771, align 8
%stackaddr$prim57772 = alloca %struct.ScmObj*, align 8
%a47125 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56281)
store volatile %struct.ScmObj* %a47125, %struct.ScmObj** %stackaddr$prim57772, align 8
%stackaddr$prim57773 = alloca %struct.ScmObj*, align 8
%current_45args56282 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56281)
store volatile %struct.ScmObj* %current_45args56282, %struct.ScmObj** %stackaddr$prim57773, align 8
%stackaddr$prim57774 = alloca %struct.ScmObj*, align 8
%b47124 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56282)
store volatile %struct.ScmObj* %b47124, %struct.ScmObj** %stackaddr$prim57774, align 8
%stackaddr$prim57775 = alloca %struct.ScmObj*, align 8
%cpsprim47489 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a47125, %struct.ScmObj* %b47124)
store volatile %struct.ScmObj* %cpsprim47489, %struct.ScmObj** %stackaddr$prim57775, align 8
%ae48349 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56284$k474880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57776 = alloca %struct.ScmObj*, align 8
%argslist56284$k474881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47489, %struct.ScmObj* %argslist56284$k474880)
store volatile %struct.ScmObj* %argslist56284$k474881, %struct.ScmObj** %stackaddr$prim57776, align 8
%stackaddr$prim57777 = alloca %struct.ScmObj*, align 8
%argslist56284$k474882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48349, %struct.ScmObj* %argslist56284$k474881)
store volatile %struct.ScmObj* %argslist56284$k474882, %struct.ScmObj** %stackaddr$prim57777, align 8
%clofunc57778 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47488)
musttail call tailcc void %clofunc57778(%struct.ScmObj* %k47488, %struct.ScmObj* %argslist56284$k474882)
ret void
}

define tailcc void @proc_clo$ae48321(%struct.ScmObj* %env$ae48321,%struct.ScmObj* %current_45args56287) {
%stackaddr$prim57779 = alloca %struct.ScmObj*, align 8
%k47490 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56287)
store volatile %struct.ScmObj* %k47490, %struct.ScmObj** %stackaddr$prim57779, align 8
%stackaddr$prim57780 = alloca %struct.ScmObj*, align 8
%current_45args56288 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56287)
store volatile %struct.ScmObj* %current_45args56288, %struct.ScmObj** %stackaddr$prim57780, align 8
%stackaddr$prim57781 = alloca %struct.ScmObj*, align 8
%x47121 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56288)
store volatile %struct.ScmObj* %x47121, %struct.ScmObj** %stackaddr$prim57781, align 8
%stackaddr$prim57782 = alloca %struct.ScmObj*, align 8
%cpsprim47491 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47121)
store volatile %struct.ScmObj* %cpsprim47491, %struct.ScmObj** %stackaddr$prim57782, align 8
%ae48324 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56290$k474900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57783 = alloca %struct.ScmObj*, align 8
%argslist56290$k474901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47491, %struct.ScmObj* %argslist56290$k474900)
store volatile %struct.ScmObj* %argslist56290$k474901, %struct.ScmObj** %stackaddr$prim57783, align 8
%stackaddr$prim57784 = alloca %struct.ScmObj*, align 8
%argslist56290$k474902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48324, %struct.ScmObj* %argslist56290$k474901)
store volatile %struct.ScmObj* %argslist56290$k474902, %struct.ScmObj** %stackaddr$prim57784, align 8
%clofunc57785 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47490)
musttail call tailcc void %clofunc57785(%struct.ScmObj* %k47490, %struct.ScmObj* %argslist56290$k474902)
ret void
}

define tailcc void @proc_clo$ae48297(%struct.ScmObj* %env$ae48297,%struct.ScmObj* %current_45args56293) {
%stackaddr$prim57786 = alloca %struct.ScmObj*, align 8
%k47492 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56293)
store volatile %struct.ScmObj* %k47492, %struct.ScmObj** %stackaddr$prim57786, align 8
%stackaddr$prim57787 = alloca %struct.ScmObj*, align 8
%current_45args56294 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56293)
store volatile %struct.ScmObj* %current_45args56294, %struct.ScmObj** %stackaddr$prim57787, align 8
%stackaddr$prim57788 = alloca %struct.ScmObj*, align 8
%x47123 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56294)
store volatile %struct.ScmObj* %x47123, %struct.ScmObj** %stackaddr$prim57788, align 8
%stackaddr$prim57789 = alloca %struct.ScmObj*, align 8
%cpsprim47493 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47123)
store volatile %struct.ScmObj* %cpsprim47493, %struct.ScmObj** %stackaddr$prim57789, align 8
%ae48300 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56296$k474920 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57790 = alloca %struct.ScmObj*, align 8
%argslist56296$k474921 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47493, %struct.ScmObj* %argslist56296$k474920)
store volatile %struct.ScmObj* %argslist56296$k474921, %struct.ScmObj** %stackaddr$prim57790, align 8
%stackaddr$prim57791 = alloca %struct.ScmObj*, align 8
%argslist56296$k474922 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48300, %struct.ScmObj* %argslist56296$k474921)
store volatile %struct.ScmObj* %argslist56296$k474922, %struct.ScmObj** %stackaddr$prim57791, align 8
%clofunc57792 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47492)
musttail call tailcc void %clofunc57792(%struct.ScmObj* %k47492, %struct.ScmObj* %argslist56296$k474922)
ret void
}

define tailcc void @proc_clo$ae48249(%struct.ScmObj* %env$ae48249,%struct.ScmObj* %current_45args56299) {
%stackaddr$prim57793 = alloca %struct.ScmObj*, align 8
%k47494 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56299)
store volatile %struct.ScmObj* %k47494, %struct.ScmObj** %stackaddr$prim57793, align 8
%stackaddr$prim57794 = alloca %struct.ScmObj*, align 8
%current_45args56300 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56299)
store volatile %struct.ScmObj* %current_45args56300, %struct.ScmObj** %stackaddr$prim57794, align 8
%stackaddr$prim57795 = alloca %struct.ScmObj*, align 8
%lst47119 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56300)
store volatile %struct.ScmObj* %lst47119, %struct.ScmObj** %stackaddr$prim57795, align 8
%stackaddr$prim57796 = alloca %struct.ScmObj*, align 8
%current_45args56301 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56300)
store volatile %struct.ScmObj* %current_45args56301, %struct.ScmObj** %stackaddr$prim57796, align 8
%stackaddr$prim57797 = alloca %struct.ScmObj*, align 8
%b47118 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56301)
store volatile %struct.ScmObj* %b47118, %struct.ScmObj** %stackaddr$prim57797, align 8
%truthy$cmp57798 = call i64 @is_truthy_value(%struct.ScmObj* %b47118)
%cmp$cmp57798 = icmp eq i64 %truthy$cmp57798, 1
br i1 %cmp$cmp57798, label %truebranch$cmp57798, label %falsebranch$cmp57798
truebranch$cmp57798:
%ae48252 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56303$k474940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57799 = alloca %struct.ScmObj*, align 8
%argslist56303$k474941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b47118, %struct.ScmObj* %argslist56303$k474940)
store volatile %struct.ScmObj* %argslist56303$k474941, %struct.ScmObj** %stackaddr$prim57799, align 8
%stackaddr$prim57800 = alloca %struct.ScmObj*, align 8
%argslist56303$k474942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48252, %struct.ScmObj* %argslist56303$k474941)
store volatile %struct.ScmObj* %argslist56303$k474942, %struct.ScmObj** %stackaddr$prim57800, align 8
%clofunc57801 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47494)
musttail call tailcc void %clofunc57801(%struct.ScmObj* %k47494, %struct.ScmObj* %argslist56303$k474942)
ret void
falsebranch$cmp57798:
%stackaddr$prim57802 = alloca %struct.ScmObj*, align 8
%cpsprim47495 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47119)
store volatile %struct.ScmObj* %cpsprim47495, %struct.ScmObj** %stackaddr$prim57802, align 8
%ae48259 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56304$k474940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57803 = alloca %struct.ScmObj*, align 8
%argslist56304$k474941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47495, %struct.ScmObj* %argslist56304$k474940)
store volatile %struct.ScmObj* %argslist56304$k474941, %struct.ScmObj** %stackaddr$prim57803, align 8
%stackaddr$prim57804 = alloca %struct.ScmObj*, align 8
%argslist56304$k474942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48259, %struct.ScmObj* %argslist56304$k474941)
store volatile %struct.ScmObj* %argslist56304$k474942, %struct.ScmObj** %stackaddr$prim57804, align 8
%clofunc57805 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47494)
musttail call tailcc void %clofunc57805(%struct.ScmObj* %k47494, %struct.ScmObj* %argslist56304$k474942)
ret void
}

define tailcc void @proc_clo$ae48206(%struct.ScmObj* %env$ae48206,%struct.ScmObj* %current_45args56308) {
%stackaddr$env-ref57806 = alloca %struct.ScmObj*, align 8
%_37take47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48206, i64 0)
store %struct.ScmObj* %_37take47099, %struct.ScmObj** %stackaddr$env-ref57806
%stackaddr$env-ref57807 = alloca %struct.ScmObj*, align 8
%_37length47096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48206, i64 1)
store %struct.ScmObj* %_37length47096, %struct.ScmObj** %stackaddr$env-ref57807
%stackaddr$prim57808 = alloca %struct.ScmObj*, align 8
%k47496 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56308)
store volatile %struct.ScmObj* %k47496, %struct.ScmObj** %stackaddr$prim57808, align 8
%stackaddr$prim57809 = alloca %struct.ScmObj*, align 8
%current_45args56309 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56308)
store volatile %struct.ScmObj* %current_45args56309, %struct.ScmObj** %stackaddr$prim57809, align 8
%stackaddr$prim57810 = alloca %struct.ScmObj*, align 8
%lst47128 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56309)
store volatile %struct.ScmObj* %lst47128, %struct.ScmObj** %stackaddr$prim57810, align 8
%stackaddr$prim57811 = alloca %struct.ScmObj*, align 8
%current_45args56310 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56309)
store volatile %struct.ScmObj* %current_45args56310, %struct.ScmObj** %stackaddr$prim57811, align 8
%stackaddr$prim57812 = alloca %struct.ScmObj*, align 8
%n47127 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56310)
store volatile %struct.ScmObj* %n47127, %struct.ScmObj** %stackaddr$prim57812, align 8
%stackaddr$makeclosure57813 = alloca %struct.ScmObj*, align 8
%fptrToInt57814 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48208 to i64
%ae48208 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57814)
store volatile %struct.ScmObj* %ae48208, %struct.ScmObj** %stackaddr$makeclosure57813, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48208, %struct.ScmObj* %lst47128, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48208, %struct.ScmObj* %k47496, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48208, %struct.ScmObj* %_37take47099, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48208, %struct.ScmObj* %n47127, i64 3)
%argslist56316$_37length470960 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57815 = alloca %struct.ScmObj*, align 8
%argslist56316$_37length470961 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47128, %struct.ScmObj* %argslist56316$_37length470960)
store volatile %struct.ScmObj* %argslist56316$_37length470961, %struct.ScmObj** %stackaddr$prim57815, align 8
%stackaddr$prim57816 = alloca %struct.ScmObj*, align 8
%argslist56316$_37length470962 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48208, %struct.ScmObj* %argslist56316$_37length470961)
store volatile %struct.ScmObj* %argslist56316$_37length470962, %struct.ScmObj** %stackaddr$prim57816, align 8
%clofunc57817 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length47096)
musttail call tailcc void %clofunc57817(%struct.ScmObj* %_37length47096, %struct.ScmObj* %argslist56316$_37length470962)
ret void
}

define tailcc void @proc_clo$ae48208(%struct.ScmObj* %env$ae48208,%struct.ScmObj* %current_45args56312) {
%stackaddr$env-ref57818 = alloca %struct.ScmObj*, align 8
%lst47128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48208, i64 0)
store %struct.ScmObj* %lst47128, %struct.ScmObj** %stackaddr$env-ref57818
%stackaddr$env-ref57819 = alloca %struct.ScmObj*, align 8
%k47496 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48208, i64 1)
store %struct.ScmObj* %k47496, %struct.ScmObj** %stackaddr$env-ref57819
%stackaddr$env-ref57820 = alloca %struct.ScmObj*, align 8
%_37take47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48208, i64 2)
store %struct.ScmObj* %_37take47099, %struct.ScmObj** %stackaddr$env-ref57820
%stackaddr$env-ref57821 = alloca %struct.ScmObj*, align 8
%n47127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48208, i64 3)
store %struct.ScmObj* %n47127, %struct.ScmObj** %stackaddr$env-ref57821
%stackaddr$prim57822 = alloca %struct.ScmObj*, align 8
%_95k47497 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56312)
store volatile %struct.ScmObj* %_95k47497, %struct.ScmObj** %stackaddr$prim57822, align 8
%stackaddr$prim57823 = alloca %struct.ScmObj*, align 8
%current_45args56313 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56312)
store volatile %struct.ScmObj* %current_45args56313, %struct.ScmObj** %stackaddr$prim57823, align 8
%stackaddr$prim57824 = alloca %struct.ScmObj*, align 8
%anf_45bind47273 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56313)
store volatile %struct.ScmObj* %anf_45bind47273, %struct.ScmObj** %stackaddr$prim57824, align 8
%stackaddr$prim57825 = alloca %struct.ScmObj*, align 8
%anf_45bind47274 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind47273, %struct.ScmObj* %n47127)
store volatile %struct.ScmObj* %anf_45bind47274, %struct.ScmObj** %stackaddr$prim57825, align 8
%argslist56315$_37take470990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57826 = alloca %struct.ScmObj*, align 8
%argslist56315$_37take470991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47274, %struct.ScmObj* %argslist56315$_37take470990)
store volatile %struct.ScmObj* %argslist56315$_37take470991, %struct.ScmObj** %stackaddr$prim57826, align 8
%stackaddr$prim57827 = alloca %struct.ScmObj*, align 8
%argslist56315$_37take470992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47128, %struct.ScmObj* %argslist56315$_37take470991)
store volatile %struct.ScmObj* %argslist56315$_37take470992, %struct.ScmObj** %stackaddr$prim57827, align 8
%stackaddr$prim57828 = alloca %struct.ScmObj*, align 8
%argslist56315$_37take470993 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47496, %struct.ScmObj* %argslist56315$_37take470992)
store volatile %struct.ScmObj* %argslist56315$_37take470993, %struct.ScmObj** %stackaddr$prim57828, align 8
%clofunc57829 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take47099)
musttail call tailcc void %clofunc57829(%struct.ScmObj* %_37take47099, %struct.ScmObj* %argslist56315$_37take470993)
ret void
}

define tailcc void @proc_clo$ae48152(%struct.ScmObj* %env$ae48152,%struct.ScmObj* %current_45args56318) {
%stackaddr$env-ref57830 = alloca %struct.ScmObj*, align 8
%_37foldl147091 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48152, i64 0)
store %struct.ScmObj* %_37foldl147091, %struct.ScmObj** %stackaddr$env-ref57830
%stackaddr$prim57831 = alloca %struct.ScmObj*, align 8
%k47498 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56318)
store volatile %struct.ScmObj* %k47498, %struct.ScmObj** %stackaddr$prim57831, align 8
%stackaddr$prim57832 = alloca %struct.ScmObj*, align 8
%current_45args56319 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56318)
store volatile %struct.ScmObj* %current_45args56319, %struct.ScmObj** %stackaddr$prim57832, align 8
%stackaddr$prim57833 = alloca %struct.ScmObj*, align 8
%lst47130 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56319)
store volatile %struct.ScmObj* %lst47130, %struct.ScmObj** %stackaddr$prim57833, align 8
%stackaddr$makeclosure57834 = alloca %struct.ScmObj*, align 8
%fptrToInt57835 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48153 to i64
%ae48153 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57835)
store volatile %struct.ScmObj* %ae48153, %struct.ScmObj** %stackaddr$makeclosure57834, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48153, %struct.ScmObj* %lst47130, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48153, %struct.ScmObj* %k47498, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48153, %struct.ScmObj* %_37foldl147091, i64 2)
%ae48154 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57836 = alloca %struct.ScmObj*, align 8
%fptrToInt57837 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48155 to i64
%ae48155 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57837)
store volatile %struct.ScmObj* %ae48155, %struct.ScmObj** %stackaddr$makeclosure57836, align 8
%argslist56330$ae481530 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57838 = alloca %struct.ScmObj*, align 8
%argslist56330$ae481531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48155, %struct.ScmObj* %argslist56330$ae481530)
store volatile %struct.ScmObj* %argslist56330$ae481531, %struct.ScmObj** %stackaddr$prim57838, align 8
%stackaddr$prim57839 = alloca %struct.ScmObj*, align 8
%argslist56330$ae481532 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48154, %struct.ScmObj* %argslist56330$ae481531)
store volatile %struct.ScmObj* %argslist56330$ae481532, %struct.ScmObj** %stackaddr$prim57839, align 8
%clofunc57840 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48153)
musttail call tailcc void %clofunc57840(%struct.ScmObj* %ae48153, %struct.ScmObj* %argslist56330$ae481532)
ret void
}

define tailcc void @proc_clo$ae48153(%struct.ScmObj* %env$ae48153,%struct.ScmObj* %current_45args56321) {
%stackaddr$env-ref57841 = alloca %struct.ScmObj*, align 8
%lst47130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48153, i64 0)
store %struct.ScmObj* %lst47130, %struct.ScmObj** %stackaddr$env-ref57841
%stackaddr$env-ref57842 = alloca %struct.ScmObj*, align 8
%k47498 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48153, i64 1)
store %struct.ScmObj* %k47498, %struct.ScmObj** %stackaddr$env-ref57842
%stackaddr$env-ref57843 = alloca %struct.ScmObj*, align 8
%_37foldl147091 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48153, i64 2)
store %struct.ScmObj* %_37foldl147091, %struct.ScmObj** %stackaddr$env-ref57843
%stackaddr$prim57844 = alloca %struct.ScmObj*, align 8
%_95k47499 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56321)
store volatile %struct.ScmObj* %_95k47499, %struct.ScmObj** %stackaddr$prim57844, align 8
%stackaddr$prim57845 = alloca %struct.ScmObj*, align 8
%current_45args56322 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56321)
store volatile %struct.ScmObj* %current_45args56322, %struct.ScmObj** %stackaddr$prim57845, align 8
%stackaddr$prim57846 = alloca %struct.ScmObj*, align 8
%anf_45bind47272 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56322)
store volatile %struct.ScmObj* %anf_45bind47272, %struct.ScmObj** %stackaddr$prim57846, align 8
%ae48174 = call %struct.ScmObj* @const_init_null()
%argslist56324$_37foldl1470910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57847 = alloca %struct.ScmObj*, align 8
%argslist56324$_37foldl1470911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47130, %struct.ScmObj* %argslist56324$_37foldl1470910)
store volatile %struct.ScmObj* %argslist56324$_37foldl1470911, %struct.ScmObj** %stackaddr$prim57847, align 8
%stackaddr$prim57848 = alloca %struct.ScmObj*, align 8
%argslist56324$_37foldl1470912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48174, %struct.ScmObj* %argslist56324$_37foldl1470911)
store volatile %struct.ScmObj* %argslist56324$_37foldl1470912, %struct.ScmObj** %stackaddr$prim57848, align 8
%stackaddr$prim57849 = alloca %struct.ScmObj*, align 8
%argslist56324$_37foldl1470913 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47272, %struct.ScmObj* %argslist56324$_37foldl1470912)
store volatile %struct.ScmObj* %argslist56324$_37foldl1470913, %struct.ScmObj** %stackaddr$prim57849, align 8
%stackaddr$prim57850 = alloca %struct.ScmObj*, align 8
%argslist56324$_37foldl1470914 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47498, %struct.ScmObj* %argslist56324$_37foldl1470913)
store volatile %struct.ScmObj* %argslist56324$_37foldl1470914, %struct.ScmObj** %stackaddr$prim57850, align 8
%clofunc57851 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147091)
musttail call tailcc void %clofunc57851(%struct.ScmObj* %_37foldl147091, %struct.ScmObj* %argslist56324$_37foldl1470914)
ret void
}

define tailcc void @proc_clo$ae48155(%struct.ScmObj* %env$ae48155,%struct.ScmObj* %current_45args56325) {
%stackaddr$prim57852 = alloca %struct.ScmObj*, align 8
%k47500 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56325)
store volatile %struct.ScmObj* %k47500, %struct.ScmObj** %stackaddr$prim57852, align 8
%stackaddr$prim57853 = alloca %struct.ScmObj*, align 8
%current_45args56326 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56325)
store volatile %struct.ScmObj* %current_45args56326, %struct.ScmObj** %stackaddr$prim57853, align 8
%stackaddr$prim57854 = alloca %struct.ScmObj*, align 8
%x47132 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56326)
store volatile %struct.ScmObj* %x47132, %struct.ScmObj** %stackaddr$prim57854, align 8
%stackaddr$prim57855 = alloca %struct.ScmObj*, align 8
%current_45args56327 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56326)
store volatile %struct.ScmObj* %current_45args56327, %struct.ScmObj** %stackaddr$prim57855, align 8
%stackaddr$prim57856 = alloca %struct.ScmObj*, align 8
%y47131 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56327)
store volatile %struct.ScmObj* %y47131, %struct.ScmObj** %stackaddr$prim57856, align 8
%ae48157 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56329$k475000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57857 = alloca %struct.ScmObj*, align 8
%argslist56329$k475001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x47132, %struct.ScmObj* %argslist56329$k475000)
store volatile %struct.ScmObj* %argslist56329$k475001, %struct.ScmObj** %stackaddr$prim57857, align 8
%stackaddr$prim57858 = alloca %struct.ScmObj*, align 8
%argslist56329$k475002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48157, %struct.ScmObj* %argslist56329$k475001)
store volatile %struct.ScmObj* %argslist56329$k475002, %struct.ScmObj** %stackaddr$prim57858, align 8
%clofunc57859 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47500)
musttail call tailcc void %clofunc57859(%struct.ScmObj* %k47500, %struct.ScmObj* %argslist56329$k475002)
ret void
}

define tailcc void @proc_clo$ae48073(%struct.ScmObj* %env$ae48073,%struct.ScmObj* %current_45args56333) {
%stackaddr$prim57860 = alloca %struct.ScmObj*, align 8
%k47501 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56333)
store volatile %struct.ScmObj* %k47501, %struct.ScmObj** %stackaddr$prim57860, align 8
%stackaddr$prim57861 = alloca %struct.ScmObj*, align 8
%current_45args56334 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56333)
store volatile %struct.ScmObj* %current_45args56334, %struct.ScmObj** %stackaddr$prim57861, align 8
%stackaddr$prim57862 = alloca %struct.ScmObj*, align 8
%_37foldl147092 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56334)
store volatile %struct.ScmObj* %_37foldl147092, %struct.ScmObj** %stackaddr$prim57862, align 8
%ae48075 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57863 = alloca %struct.ScmObj*, align 8
%fptrToInt57864 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48076 to i64
%ae48076 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57864)
store volatile %struct.ScmObj* %ae48076, %struct.ScmObj** %stackaddr$makeclosure57863, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48076, %struct.ScmObj* %_37foldl147092, i64 0)
%argslist56347$k475010 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57865 = alloca %struct.ScmObj*, align 8
%argslist56347$k475011 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48076, %struct.ScmObj* %argslist56347$k475010)
store volatile %struct.ScmObj* %argslist56347$k475011, %struct.ScmObj** %stackaddr$prim57865, align 8
%stackaddr$prim57866 = alloca %struct.ScmObj*, align 8
%argslist56347$k475012 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48075, %struct.ScmObj* %argslist56347$k475011)
store volatile %struct.ScmObj* %argslist56347$k475012, %struct.ScmObj** %stackaddr$prim57866, align 8
%clofunc57867 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47501)
musttail call tailcc void %clofunc57867(%struct.ScmObj* %k47501, %struct.ScmObj* %argslist56347$k475012)
ret void
}

define tailcc void @proc_clo$ae48076(%struct.ScmObj* %env$ae48076,%struct.ScmObj* %current_45args56336) {
%stackaddr$env-ref57868 = alloca %struct.ScmObj*, align 8
%_37foldl147092 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48076, i64 0)
store %struct.ScmObj* %_37foldl147092, %struct.ScmObj** %stackaddr$env-ref57868
%stackaddr$prim57869 = alloca %struct.ScmObj*, align 8
%k47502 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56336)
store volatile %struct.ScmObj* %k47502, %struct.ScmObj** %stackaddr$prim57869, align 8
%stackaddr$prim57870 = alloca %struct.ScmObj*, align 8
%current_45args56337 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56336)
store volatile %struct.ScmObj* %current_45args56337, %struct.ScmObj** %stackaddr$prim57870, align 8
%stackaddr$prim57871 = alloca %struct.ScmObj*, align 8
%f47095 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56337)
store volatile %struct.ScmObj* %f47095, %struct.ScmObj** %stackaddr$prim57871, align 8
%stackaddr$prim57872 = alloca %struct.ScmObj*, align 8
%current_45args56338 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56337)
store volatile %struct.ScmObj* %current_45args56338, %struct.ScmObj** %stackaddr$prim57872, align 8
%stackaddr$prim57873 = alloca %struct.ScmObj*, align 8
%acc47094 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56338)
store volatile %struct.ScmObj* %acc47094, %struct.ScmObj** %stackaddr$prim57873, align 8
%stackaddr$prim57874 = alloca %struct.ScmObj*, align 8
%current_45args56339 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56338)
store volatile %struct.ScmObj* %current_45args56339, %struct.ScmObj** %stackaddr$prim57874, align 8
%stackaddr$prim57875 = alloca %struct.ScmObj*, align 8
%lst47093 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56339)
store volatile %struct.ScmObj* %lst47093, %struct.ScmObj** %stackaddr$prim57875, align 8
%stackaddr$prim57876 = alloca %struct.ScmObj*, align 8
%anf_45bind47267 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47093)
store volatile %struct.ScmObj* %anf_45bind47267, %struct.ScmObj** %stackaddr$prim57876, align 8
%truthy$cmp57877 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47267)
%cmp$cmp57877 = icmp eq i64 %truthy$cmp57877, 1
br i1 %cmp$cmp57877, label %truebranch$cmp57877, label %falsebranch$cmp57877
truebranch$cmp57877:
%ae48080 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56341$k475020 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57878 = alloca %struct.ScmObj*, align 8
%argslist56341$k475021 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47094, %struct.ScmObj* %argslist56341$k475020)
store volatile %struct.ScmObj* %argslist56341$k475021, %struct.ScmObj** %stackaddr$prim57878, align 8
%stackaddr$prim57879 = alloca %struct.ScmObj*, align 8
%argslist56341$k475022 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48080, %struct.ScmObj* %argslist56341$k475021)
store volatile %struct.ScmObj* %argslist56341$k475022, %struct.ScmObj** %stackaddr$prim57879, align 8
%clofunc57880 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47502)
musttail call tailcc void %clofunc57880(%struct.ScmObj* %k47502, %struct.ScmObj* %argslist56341$k475022)
ret void
falsebranch$cmp57877:
%stackaddr$prim57881 = alloca %struct.ScmObj*, align 8
%anf_45bind47268 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47093)
store volatile %struct.ScmObj* %anf_45bind47268, %struct.ScmObj** %stackaddr$prim57881, align 8
%stackaddr$makeclosure57882 = alloca %struct.ScmObj*, align 8
%fptrToInt57883 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48087 to i64
%ae48087 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57883)
store volatile %struct.ScmObj* %ae48087, %struct.ScmObj** %stackaddr$makeclosure57882, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48087, %struct.ScmObj* %lst47093, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48087, %struct.ScmObj* %_37foldl147092, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48087, %struct.ScmObj* %k47502, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48087, %struct.ScmObj* %f47095, i64 3)
%argslist56346$f470950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57884 = alloca %struct.ScmObj*, align 8
%argslist56346$f470951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47094, %struct.ScmObj* %argslist56346$f470950)
store volatile %struct.ScmObj* %argslist56346$f470951, %struct.ScmObj** %stackaddr$prim57884, align 8
%stackaddr$prim57885 = alloca %struct.ScmObj*, align 8
%argslist56346$f470952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47268, %struct.ScmObj* %argslist56346$f470951)
store volatile %struct.ScmObj* %argslist56346$f470952, %struct.ScmObj** %stackaddr$prim57885, align 8
%stackaddr$prim57886 = alloca %struct.ScmObj*, align 8
%argslist56346$f470953 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48087, %struct.ScmObj* %argslist56346$f470952)
store volatile %struct.ScmObj* %argslist56346$f470953, %struct.ScmObj** %stackaddr$prim57886, align 8
%clofunc57887 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47095)
musttail call tailcc void %clofunc57887(%struct.ScmObj* %f47095, %struct.ScmObj* %argslist56346$f470953)
ret void
}

define tailcc void @proc_clo$ae48087(%struct.ScmObj* %env$ae48087,%struct.ScmObj* %current_45args56342) {
%stackaddr$env-ref57888 = alloca %struct.ScmObj*, align 8
%lst47093 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48087, i64 0)
store %struct.ScmObj* %lst47093, %struct.ScmObj** %stackaddr$env-ref57888
%stackaddr$env-ref57889 = alloca %struct.ScmObj*, align 8
%_37foldl147092 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48087, i64 1)
store %struct.ScmObj* %_37foldl147092, %struct.ScmObj** %stackaddr$env-ref57889
%stackaddr$env-ref57890 = alloca %struct.ScmObj*, align 8
%k47502 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48087, i64 2)
store %struct.ScmObj* %k47502, %struct.ScmObj** %stackaddr$env-ref57890
%stackaddr$env-ref57891 = alloca %struct.ScmObj*, align 8
%f47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48087, i64 3)
store %struct.ScmObj* %f47095, %struct.ScmObj** %stackaddr$env-ref57891
%stackaddr$prim57892 = alloca %struct.ScmObj*, align 8
%_95k47503 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56342)
store volatile %struct.ScmObj* %_95k47503, %struct.ScmObj** %stackaddr$prim57892, align 8
%stackaddr$prim57893 = alloca %struct.ScmObj*, align 8
%current_45args56343 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56342)
store volatile %struct.ScmObj* %current_45args56343, %struct.ScmObj** %stackaddr$prim57893, align 8
%stackaddr$prim57894 = alloca %struct.ScmObj*, align 8
%anf_45bind47269 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56343)
store volatile %struct.ScmObj* %anf_45bind47269, %struct.ScmObj** %stackaddr$prim57894, align 8
%stackaddr$prim57895 = alloca %struct.ScmObj*, align 8
%anf_45bind47270 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47093)
store volatile %struct.ScmObj* %anf_45bind47270, %struct.ScmObj** %stackaddr$prim57895, align 8
%argslist56345$_37foldl1470920 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57896 = alloca %struct.ScmObj*, align 8
%argslist56345$_37foldl1470921 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47270, %struct.ScmObj* %argslist56345$_37foldl1470920)
store volatile %struct.ScmObj* %argslist56345$_37foldl1470921, %struct.ScmObj** %stackaddr$prim57896, align 8
%stackaddr$prim57897 = alloca %struct.ScmObj*, align 8
%argslist56345$_37foldl1470922 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47269, %struct.ScmObj* %argslist56345$_37foldl1470921)
store volatile %struct.ScmObj* %argslist56345$_37foldl1470922, %struct.ScmObj** %stackaddr$prim57897, align 8
%stackaddr$prim57898 = alloca %struct.ScmObj*, align 8
%argslist56345$_37foldl1470923 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47095, %struct.ScmObj* %argslist56345$_37foldl1470922)
store volatile %struct.ScmObj* %argslist56345$_37foldl1470923, %struct.ScmObj** %stackaddr$prim57898, align 8
%stackaddr$prim57899 = alloca %struct.ScmObj*, align 8
%argslist56345$_37foldl1470924 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47502, %struct.ScmObj* %argslist56345$_37foldl1470923)
store volatile %struct.ScmObj* %argslist56345$_37foldl1470924, %struct.ScmObj** %stackaddr$prim57899, align 8
%clofunc57900 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147092)
musttail call tailcc void %clofunc57900(%struct.ScmObj* %_37foldl147092, %struct.ScmObj* %argslist56345$_37foldl1470924)
ret void
}

define tailcc void @proc_clo$ae47990(%struct.ScmObj* %env$ae47990,%struct.ScmObj* %current_45args56350) {
%stackaddr$prim57901 = alloca %struct.ScmObj*, align 8
%k47504 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56350)
store volatile %struct.ScmObj* %k47504, %struct.ScmObj** %stackaddr$prim57901, align 8
%stackaddr$prim57902 = alloca %struct.ScmObj*, align 8
%current_45args56351 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56350)
store volatile %struct.ScmObj* %current_45args56351, %struct.ScmObj** %stackaddr$prim57902, align 8
%stackaddr$prim57903 = alloca %struct.ScmObj*, align 8
%_37length47097 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56351)
store volatile %struct.ScmObj* %_37length47097, %struct.ScmObj** %stackaddr$prim57903, align 8
%ae47992 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57904 = alloca %struct.ScmObj*, align 8
%fptrToInt57905 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47993 to i64
%ae47993 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57905)
store volatile %struct.ScmObj* %ae47993, %struct.ScmObj** %stackaddr$makeclosure57904, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47993, %struct.ScmObj* %_37length47097, i64 0)
%argslist56362$k475040 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57906 = alloca %struct.ScmObj*, align 8
%argslist56362$k475041 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47993, %struct.ScmObj* %argslist56362$k475040)
store volatile %struct.ScmObj* %argslist56362$k475041, %struct.ScmObj** %stackaddr$prim57906, align 8
%stackaddr$prim57907 = alloca %struct.ScmObj*, align 8
%argslist56362$k475042 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47992, %struct.ScmObj* %argslist56362$k475041)
store volatile %struct.ScmObj* %argslist56362$k475042, %struct.ScmObj** %stackaddr$prim57907, align 8
%clofunc57908 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47504)
musttail call tailcc void %clofunc57908(%struct.ScmObj* %k47504, %struct.ScmObj* %argslist56362$k475042)
ret void
}

define tailcc void @proc_clo$ae47993(%struct.ScmObj* %env$ae47993,%struct.ScmObj* %current_45args56353) {
%stackaddr$env-ref57909 = alloca %struct.ScmObj*, align 8
%_37length47097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47993, i64 0)
store %struct.ScmObj* %_37length47097, %struct.ScmObj** %stackaddr$env-ref57909
%stackaddr$prim57910 = alloca %struct.ScmObj*, align 8
%k47505 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56353)
store volatile %struct.ScmObj* %k47505, %struct.ScmObj** %stackaddr$prim57910, align 8
%stackaddr$prim57911 = alloca %struct.ScmObj*, align 8
%current_45args56354 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56353)
store volatile %struct.ScmObj* %current_45args56354, %struct.ScmObj** %stackaddr$prim57911, align 8
%stackaddr$prim57912 = alloca %struct.ScmObj*, align 8
%lst47098 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56354)
store volatile %struct.ScmObj* %lst47098, %struct.ScmObj** %stackaddr$prim57912, align 8
%stackaddr$prim57913 = alloca %struct.ScmObj*, align 8
%anf_45bind47263 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47098)
store volatile %struct.ScmObj* %anf_45bind47263, %struct.ScmObj** %stackaddr$prim57913, align 8
%truthy$cmp57914 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47263)
%cmp$cmp57914 = icmp eq i64 %truthy$cmp57914, 1
br i1 %cmp$cmp57914, label %truebranch$cmp57914, label %falsebranch$cmp57914
truebranch$cmp57914:
%ae47997 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47998 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56356$k475050 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57915 = alloca %struct.ScmObj*, align 8
%argslist56356$k475051 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47998, %struct.ScmObj* %argslist56356$k475050)
store volatile %struct.ScmObj* %argslist56356$k475051, %struct.ScmObj** %stackaddr$prim57915, align 8
%stackaddr$prim57916 = alloca %struct.ScmObj*, align 8
%argslist56356$k475052 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47997, %struct.ScmObj* %argslist56356$k475051)
store volatile %struct.ScmObj* %argslist56356$k475052, %struct.ScmObj** %stackaddr$prim57916, align 8
%clofunc57917 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47505)
musttail call tailcc void %clofunc57917(%struct.ScmObj* %k47505, %struct.ScmObj* %argslist56356$k475052)
ret void
falsebranch$cmp57914:
%stackaddr$prim57918 = alloca %struct.ScmObj*, align 8
%anf_45bind47264 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47098)
store volatile %struct.ScmObj* %anf_45bind47264, %struct.ScmObj** %stackaddr$prim57918, align 8
%stackaddr$makeclosure57919 = alloca %struct.ScmObj*, align 8
%fptrToInt57920 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48007 to i64
%ae48007 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57920)
store volatile %struct.ScmObj* %ae48007, %struct.ScmObj** %stackaddr$makeclosure57919, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48007, %struct.ScmObj* %k47505, i64 0)
%argslist56361$_37length470970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57921 = alloca %struct.ScmObj*, align 8
%argslist56361$_37length470971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47264, %struct.ScmObj* %argslist56361$_37length470970)
store volatile %struct.ScmObj* %argslist56361$_37length470971, %struct.ScmObj** %stackaddr$prim57921, align 8
%stackaddr$prim57922 = alloca %struct.ScmObj*, align 8
%argslist56361$_37length470972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48007, %struct.ScmObj* %argslist56361$_37length470971)
store volatile %struct.ScmObj* %argslist56361$_37length470972, %struct.ScmObj** %stackaddr$prim57922, align 8
%clofunc57923 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length47097)
musttail call tailcc void %clofunc57923(%struct.ScmObj* %_37length47097, %struct.ScmObj* %argslist56361$_37length470972)
ret void
}

define tailcc void @proc_clo$ae48007(%struct.ScmObj* %env$ae48007,%struct.ScmObj* %current_45args56357) {
%stackaddr$env-ref57924 = alloca %struct.ScmObj*, align 8
%k47505 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48007, i64 0)
store %struct.ScmObj* %k47505, %struct.ScmObj** %stackaddr$env-ref57924
%stackaddr$prim57925 = alloca %struct.ScmObj*, align 8
%_95k47506 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56357)
store volatile %struct.ScmObj* %_95k47506, %struct.ScmObj** %stackaddr$prim57925, align 8
%stackaddr$prim57926 = alloca %struct.ScmObj*, align 8
%current_45args56358 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56357)
store volatile %struct.ScmObj* %current_45args56358, %struct.ScmObj** %stackaddr$prim57926, align 8
%stackaddr$prim57927 = alloca %struct.ScmObj*, align 8
%anf_45bind47265 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56358)
store volatile %struct.ScmObj* %anf_45bind47265, %struct.ScmObj** %stackaddr$prim57927, align 8
%ae48009 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57928 = alloca %struct.ScmObj*, align 8
%cpsprim47507 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae48009, %struct.ScmObj* %anf_45bind47265)
store volatile %struct.ScmObj* %cpsprim47507, %struct.ScmObj** %stackaddr$prim57928, align 8
%ae48012 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56360$k475050 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57929 = alloca %struct.ScmObj*, align 8
%argslist56360$k475051 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47507, %struct.ScmObj* %argslist56360$k475050)
store volatile %struct.ScmObj* %argslist56360$k475051, %struct.ScmObj** %stackaddr$prim57929, align 8
%stackaddr$prim57930 = alloca %struct.ScmObj*, align 8
%argslist56360$k475052 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48012, %struct.ScmObj* %argslist56360$k475051)
store volatile %struct.ScmObj* %argslist56360$k475052, %struct.ScmObj** %stackaddr$prim57930, align 8
%clofunc57931 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47505)
musttail call tailcc void %clofunc57931(%struct.ScmObj* %k47505, %struct.ScmObj* %argslist56360$k475052)
ret void
}

define tailcc void @proc_clo$ae47840(%struct.ScmObj* %env$ae47840,%struct.ScmObj* %current_45args56365) {
%stackaddr$prim57932 = alloca %struct.ScmObj*, align 8
%k47508 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56365)
store volatile %struct.ScmObj* %k47508, %struct.ScmObj** %stackaddr$prim57932, align 8
%stackaddr$prim57933 = alloca %struct.ScmObj*, align 8
%current_45args56366 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56365)
store volatile %struct.ScmObj* %current_45args56366, %struct.ScmObj** %stackaddr$prim57933, align 8
%stackaddr$prim57934 = alloca %struct.ScmObj*, align 8
%_37take47100 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56366)
store volatile %struct.ScmObj* %_37take47100, %struct.ScmObj** %stackaddr$prim57934, align 8
%ae47842 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57935 = alloca %struct.ScmObj*, align 8
%fptrToInt57936 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47843 to i64
%ae47843 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57936)
store volatile %struct.ScmObj* %ae47843, %struct.ScmObj** %stackaddr$makeclosure57935, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47843, %struct.ScmObj* %_37take47100, i64 0)
%argslist56379$k475080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57937 = alloca %struct.ScmObj*, align 8
%argslist56379$k475081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47843, %struct.ScmObj* %argslist56379$k475080)
store volatile %struct.ScmObj* %argslist56379$k475081, %struct.ScmObj** %stackaddr$prim57937, align 8
%stackaddr$prim57938 = alloca %struct.ScmObj*, align 8
%argslist56379$k475082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47842, %struct.ScmObj* %argslist56379$k475081)
store volatile %struct.ScmObj* %argslist56379$k475082, %struct.ScmObj** %stackaddr$prim57938, align 8
%clofunc57939 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47508)
musttail call tailcc void %clofunc57939(%struct.ScmObj* %k47508, %struct.ScmObj* %argslist56379$k475082)
ret void
}

define tailcc void @proc_clo$ae47843(%struct.ScmObj* %env$ae47843,%struct.ScmObj* %current_45args56368) {
%stackaddr$env-ref57940 = alloca %struct.ScmObj*, align 8
%_37take47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47843, i64 0)
store %struct.ScmObj* %_37take47100, %struct.ScmObj** %stackaddr$env-ref57940
%stackaddr$prim57941 = alloca %struct.ScmObj*, align 8
%k47509 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56368)
store volatile %struct.ScmObj* %k47509, %struct.ScmObj** %stackaddr$prim57941, align 8
%stackaddr$prim57942 = alloca %struct.ScmObj*, align 8
%current_45args56369 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56368)
store volatile %struct.ScmObj* %current_45args56369, %struct.ScmObj** %stackaddr$prim57942, align 8
%stackaddr$prim57943 = alloca %struct.ScmObj*, align 8
%lst47102 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56369)
store volatile %struct.ScmObj* %lst47102, %struct.ScmObj** %stackaddr$prim57943, align 8
%stackaddr$prim57944 = alloca %struct.ScmObj*, align 8
%current_45args56370 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56369)
store volatile %struct.ScmObj* %current_45args56370, %struct.ScmObj** %stackaddr$prim57944, align 8
%stackaddr$prim57945 = alloca %struct.ScmObj*, align 8
%n47101 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56370)
store volatile %struct.ScmObj* %n47101, %struct.ScmObj** %stackaddr$prim57945, align 8
%ae47845 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57946 = alloca %struct.ScmObj*, align 8
%anf_45bind47256 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n47101, %struct.ScmObj* %ae47845)
store volatile %struct.ScmObj* %anf_45bind47256, %struct.ScmObj** %stackaddr$prim57946, align 8
%truthy$cmp57947 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47256)
%cmp$cmp57947 = icmp eq i64 %truthy$cmp57947, 1
br i1 %cmp$cmp57947, label %truebranch$cmp57947, label %falsebranch$cmp57947
truebranch$cmp57947:
%ae47848 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47849 = call %struct.ScmObj* @const_init_null()
%argslist56372$k475090 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57948 = alloca %struct.ScmObj*, align 8
%argslist56372$k475091 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47849, %struct.ScmObj* %argslist56372$k475090)
store volatile %struct.ScmObj* %argslist56372$k475091, %struct.ScmObj** %stackaddr$prim57948, align 8
%stackaddr$prim57949 = alloca %struct.ScmObj*, align 8
%argslist56372$k475092 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47848, %struct.ScmObj* %argslist56372$k475091)
store volatile %struct.ScmObj* %argslist56372$k475092, %struct.ScmObj** %stackaddr$prim57949, align 8
%clofunc57950 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47509)
musttail call tailcc void %clofunc57950(%struct.ScmObj* %k47509, %struct.ScmObj* %argslist56372$k475092)
ret void
falsebranch$cmp57947:
%stackaddr$prim57951 = alloca %struct.ScmObj*, align 8
%anf_45bind47257 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47102)
store volatile %struct.ScmObj* %anf_45bind47257, %struct.ScmObj** %stackaddr$prim57951, align 8
%truthy$cmp57952 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47257)
%cmp$cmp57952 = icmp eq i64 %truthy$cmp57952, 1
br i1 %cmp$cmp57952, label %truebranch$cmp57952, label %falsebranch$cmp57952
truebranch$cmp57952:
%ae47859 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47860 = call %struct.ScmObj* @const_init_null()
%argslist56373$k475090 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57953 = alloca %struct.ScmObj*, align 8
%argslist56373$k475091 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47860, %struct.ScmObj* %argslist56373$k475090)
store volatile %struct.ScmObj* %argslist56373$k475091, %struct.ScmObj** %stackaddr$prim57953, align 8
%stackaddr$prim57954 = alloca %struct.ScmObj*, align 8
%argslist56373$k475092 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47859, %struct.ScmObj* %argslist56373$k475091)
store volatile %struct.ScmObj* %argslist56373$k475092, %struct.ScmObj** %stackaddr$prim57954, align 8
%clofunc57955 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47509)
musttail call tailcc void %clofunc57955(%struct.ScmObj* %k47509, %struct.ScmObj* %argslist56373$k475092)
ret void
falsebranch$cmp57952:
%stackaddr$prim57956 = alloca %struct.ScmObj*, align 8
%anf_45bind47258 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47102)
store volatile %struct.ScmObj* %anf_45bind47258, %struct.ScmObj** %stackaddr$prim57956, align 8
%stackaddr$prim57957 = alloca %struct.ScmObj*, align 8
%anf_45bind47259 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47102)
store volatile %struct.ScmObj* %anf_45bind47259, %struct.ScmObj** %stackaddr$prim57957, align 8
%ae47870 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57958 = alloca %struct.ScmObj*, align 8
%anf_45bind47260 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n47101, %struct.ScmObj* %ae47870)
store volatile %struct.ScmObj* %anf_45bind47260, %struct.ScmObj** %stackaddr$prim57958, align 8
%stackaddr$makeclosure57959 = alloca %struct.ScmObj*, align 8
%fptrToInt57960 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47872 to i64
%ae47872 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57960)
store volatile %struct.ScmObj* %ae47872, %struct.ScmObj** %stackaddr$makeclosure57959, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47872, %struct.ScmObj* %k47509, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47872, %struct.ScmObj* %anf_45bind47258, i64 1)
%argslist56378$_37take471000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57961 = alloca %struct.ScmObj*, align 8
%argslist56378$_37take471001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47260, %struct.ScmObj* %argslist56378$_37take471000)
store volatile %struct.ScmObj* %argslist56378$_37take471001, %struct.ScmObj** %stackaddr$prim57961, align 8
%stackaddr$prim57962 = alloca %struct.ScmObj*, align 8
%argslist56378$_37take471002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47259, %struct.ScmObj* %argslist56378$_37take471001)
store volatile %struct.ScmObj* %argslist56378$_37take471002, %struct.ScmObj** %stackaddr$prim57962, align 8
%stackaddr$prim57963 = alloca %struct.ScmObj*, align 8
%argslist56378$_37take471003 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47872, %struct.ScmObj* %argslist56378$_37take471002)
store volatile %struct.ScmObj* %argslist56378$_37take471003, %struct.ScmObj** %stackaddr$prim57963, align 8
%clofunc57964 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take47100)
musttail call tailcc void %clofunc57964(%struct.ScmObj* %_37take47100, %struct.ScmObj* %argslist56378$_37take471003)
ret void
}

define tailcc void @proc_clo$ae47872(%struct.ScmObj* %env$ae47872,%struct.ScmObj* %current_45args56374) {
%stackaddr$env-ref57965 = alloca %struct.ScmObj*, align 8
%k47509 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47872, i64 0)
store %struct.ScmObj* %k47509, %struct.ScmObj** %stackaddr$env-ref57965
%stackaddr$env-ref57966 = alloca %struct.ScmObj*, align 8
%anf_45bind47258 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47872, i64 1)
store %struct.ScmObj* %anf_45bind47258, %struct.ScmObj** %stackaddr$env-ref57966
%stackaddr$prim57967 = alloca %struct.ScmObj*, align 8
%_95k47510 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56374)
store volatile %struct.ScmObj* %_95k47510, %struct.ScmObj** %stackaddr$prim57967, align 8
%stackaddr$prim57968 = alloca %struct.ScmObj*, align 8
%current_45args56375 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56374)
store volatile %struct.ScmObj* %current_45args56375, %struct.ScmObj** %stackaddr$prim57968, align 8
%stackaddr$prim57969 = alloca %struct.ScmObj*, align 8
%anf_45bind47261 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56375)
store volatile %struct.ScmObj* %anf_45bind47261, %struct.ScmObj** %stackaddr$prim57969, align 8
%stackaddr$prim57970 = alloca %struct.ScmObj*, align 8
%cpsprim47511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47258, %struct.ScmObj* %anf_45bind47261)
store volatile %struct.ScmObj* %cpsprim47511, %struct.ScmObj** %stackaddr$prim57970, align 8
%ae47878 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56377$k475090 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57971 = alloca %struct.ScmObj*, align 8
%argslist56377$k475091 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47511, %struct.ScmObj* %argslist56377$k475090)
store volatile %struct.ScmObj* %argslist56377$k475091, %struct.ScmObj** %stackaddr$prim57971, align 8
%stackaddr$prim57972 = alloca %struct.ScmObj*, align 8
%argslist56377$k475092 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47878, %struct.ScmObj* %argslist56377$k475091)
store volatile %struct.ScmObj* %argslist56377$k475092, %struct.ScmObj** %stackaddr$prim57972, align 8
%clofunc57973 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47509)
musttail call tailcc void %clofunc57973(%struct.ScmObj* %k47509, %struct.ScmObj* %argslist56377$k475092)
ret void
}

define tailcc void @proc_clo$ae47743(%struct.ScmObj* %env$ae47743,%struct.ScmObj* %current_45args56382) {
%stackaddr$prim57974 = alloca %struct.ScmObj*, align 8
%k47512 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56382)
store volatile %struct.ScmObj* %k47512, %struct.ScmObj** %stackaddr$prim57974, align 8
%stackaddr$prim57975 = alloca %struct.ScmObj*, align 8
%current_45args56383 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56382)
store volatile %struct.ScmObj* %current_45args56383, %struct.ScmObj** %stackaddr$prim57975, align 8
%stackaddr$prim57976 = alloca %struct.ScmObj*, align 8
%_37map47104 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56383)
store volatile %struct.ScmObj* %_37map47104, %struct.ScmObj** %stackaddr$prim57976, align 8
%ae47745 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57977 = alloca %struct.ScmObj*, align 8
%fptrToInt57978 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47746 to i64
%ae47746 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57978)
store volatile %struct.ScmObj* %ae47746, %struct.ScmObj** %stackaddr$makeclosure57977, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47746, %struct.ScmObj* %_37map47104, i64 0)
%argslist56399$k475120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57979 = alloca %struct.ScmObj*, align 8
%argslist56399$k475121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47746, %struct.ScmObj* %argslist56399$k475120)
store volatile %struct.ScmObj* %argslist56399$k475121, %struct.ScmObj** %stackaddr$prim57979, align 8
%stackaddr$prim57980 = alloca %struct.ScmObj*, align 8
%argslist56399$k475122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47745, %struct.ScmObj* %argslist56399$k475121)
store volatile %struct.ScmObj* %argslist56399$k475122, %struct.ScmObj** %stackaddr$prim57980, align 8
%clofunc57981 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47512)
musttail call tailcc void %clofunc57981(%struct.ScmObj* %k47512, %struct.ScmObj* %argslist56399$k475122)
ret void
}

define tailcc void @proc_clo$ae47746(%struct.ScmObj* %env$ae47746,%struct.ScmObj* %current_45args56385) {
%stackaddr$env-ref57982 = alloca %struct.ScmObj*, align 8
%_37map47104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47746, i64 0)
store %struct.ScmObj* %_37map47104, %struct.ScmObj** %stackaddr$env-ref57982
%stackaddr$prim57983 = alloca %struct.ScmObj*, align 8
%k47513 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56385)
store volatile %struct.ScmObj* %k47513, %struct.ScmObj** %stackaddr$prim57983, align 8
%stackaddr$prim57984 = alloca %struct.ScmObj*, align 8
%current_45args56386 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56385)
store volatile %struct.ScmObj* %current_45args56386, %struct.ScmObj** %stackaddr$prim57984, align 8
%stackaddr$prim57985 = alloca %struct.ScmObj*, align 8
%f47106 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56386)
store volatile %struct.ScmObj* %f47106, %struct.ScmObj** %stackaddr$prim57985, align 8
%stackaddr$prim57986 = alloca %struct.ScmObj*, align 8
%current_45args56387 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56386)
store volatile %struct.ScmObj* %current_45args56387, %struct.ScmObj** %stackaddr$prim57986, align 8
%stackaddr$prim57987 = alloca %struct.ScmObj*, align 8
%lst47105 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56387)
store volatile %struct.ScmObj* %lst47105, %struct.ScmObj** %stackaddr$prim57987, align 8
%stackaddr$prim57988 = alloca %struct.ScmObj*, align 8
%anf_45bind47250 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47105)
store volatile %struct.ScmObj* %anf_45bind47250, %struct.ScmObj** %stackaddr$prim57988, align 8
%truthy$cmp57989 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47250)
%cmp$cmp57989 = icmp eq i64 %truthy$cmp57989, 1
br i1 %cmp$cmp57989, label %truebranch$cmp57989, label %falsebranch$cmp57989
truebranch$cmp57989:
%ae47750 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47751 = call %struct.ScmObj* @const_init_null()
%argslist56389$k475130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57990 = alloca %struct.ScmObj*, align 8
%argslist56389$k475131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47751, %struct.ScmObj* %argslist56389$k475130)
store volatile %struct.ScmObj* %argslist56389$k475131, %struct.ScmObj** %stackaddr$prim57990, align 8
%stackaddr$prim57991 = alloca %struct.ScmObj*, align 8
%argslist56389$k475132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47750, %struct.ScmObj* %argslist56389$k475131)
store volatile %struct.ScmObj* %argslist56389$k475132, %struct.ScmObj** %stackaddr$prim57991, align 8
%clofunc57992 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47513)
musttail call tailcc void %clofunc57992(%struct.ScmObj* %k47513, %struct.ScmObj* %argslist56389$k475132)
ret void
falsebranch$cmp57989:
%stackaddr$prim57993 = alloca %struct.ScmObj*, align 8
%anf_45bind47251 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47105)
store volatile %struct.ScmObj* %anf_45bind47251, %struct.ScmObj** %stackaddr$prim57993, align 8
%stackaddr$makeclosure57994 = alloca %struct.ScmObj*, align 8
%fptrToInt57995 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47760 to i64
%ae47760 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57995)
store volatile %struct.ScmObj* %ae47760, %struct.ScmObj** %stackaddr$makeclosure57994, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47760, %struct.ScmObj* %f47106, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47760, %struct.ScmObj* %lst47105, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47760, %struct.ScmObj* %_37map47104, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae47760, %struct.ScmObj* %k47513, i64 3)
%argslist56398$f471060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57996 = alloca %struct.ScmObj*, align 8
%argslist56398$f471061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47251, %struct.ScmObj* %argslist56398$f471060)
store volatile %struct.ScmObj* %argslist56398$f471061, %struct.ScmObj** %stackaddr$prim57996, align 8
%stackaddr$prim57997 = alloca %struct.ScmObj*, align 8
%argslist56398$f471062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47760, %struct.ScmObj* %argslist56398$f471061)
store volatile %struct.ScmObj* %argslist56398$f471062, %struct.ScmObj** %stackaddr$prim57997, align 8
%clofunc57998 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47106)
musttail call tailcc void %clofunc57998(%struct.ScmObj* %f47106, %struct.ScmObj* %argslist56398$f471062)
ret void
}

define tailcc void @proc_clo$ae47760(%struct.ScmObj* %env$ae47760,%struct.ScmObj* %current_45args56390) {
%stackaddr$env-ref57999 = alloca %struct.ScmObj*, align 8
%f47106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47760, i64 0)
store %struct.ScmObj* %f47106, %struct.ScmObj** %stackaddr$env-ref57999
%stackaddr$env-ref58000 = alloca %struct.ScmObj*, align 8
%lst47105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47760, i64 1)
store %struct.ScmObj* %lst47105, %struct.ScmObj** %stackaddr$env-ref58000
%stackaddr$env-ref58001 = alloca %struct.ScmObj*, align 8
%_37map47104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47760, i64 2)
store %struct.ScmObj* %_37map47104, %struct.ScmObj** %stackaddr$env-ref58001
%stackaddr$env-ref58002 = alloca %struct.ScmObj*, align 8
%k47513 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47760, i64 3)
store %struct.ScmObj* %k47513, %struct.ScmObj** %stackaddr$env-ref58002
%stackaddr$prim58003 = alloca %struct.ScmObj*, align 8
%_95k47514 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56390)
store volatile %struct.ScmObj* %_95k47514, %struct.ScmObj** %stackaddr$prim58003, align 8
%stackaddr$prim58004 = alloca %struct.ScmObj*, align 8
%current_45args56391 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56390)
store volatile %struct.ScmObj* %current_45args56391, %struct.ScmObj** %stackaddr$prim58004, align 8
%stackaddr$prim58005 = alloca %struct.ScmObj*, align 8
%anf_45bind47252 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56391)
store volatile %struct.ScmObj* %anf_45bind47252, %struct.ScmObj** %stackaddr$prim58005, align 8
%stackaddr$prim58006 = alloca %struct.ScmObj*, align 8
%anf_45bind47253 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47105)
store volatile %struct.ScmObj* %anf_45bind47253, %struct.ScmObj** %stackaddr$prim58006, align 8
%stackaddr$makeclosure58007 = alloca %struct.ScmObj*, align 8
%fptrToInt58008 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47764 to i64
%ae47764 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58008)
store volatile %struct.ScmObj* %ae47764, %struct.ScmObj** %stackaddr$makeclosure58007, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47764, %struct.ScmObj* %anf_45bind47252, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47764, %struct.ScmObj* %k47513, i64 1)
%argslist56397$_37map471040 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58009 = alloca %struct.ScmObj*, align 8
%argslist56397$_37map471041 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47253, %struct.ScmObj* %argslist56397$_37map471040)
store volatile %struct.ScmObj* %argslist56397$_37map471041, %struct.ScmObj** %stackaddr$prim58009, align 8
%stackaddr$prim58010 = alloca %struct.ScmObj*, align 8
%argslist56397$_37map471042 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47106, %struct.ScmObj* %argslist56397$_37map471041)
store volatile %struct.ScmObj* %argslist56397$_37map471042, %struct.ScmObj** %stackaddr$prim58010, align 8
%stackaddr$prim58011 = alloca %struct.ScmObj*, align 8
%argslist56397$_37map471043 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47764, %struct.ScmObj* %argslist56397$_37map471042)
store volatile %struct.ScmObj* %argslist56397$_37map471043, %struct.ScmObj** %stackaddr$prim58011, align 8
%clofunc58012 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map47104)
musttail call tailcc void %clofunc58012(%struct.ScmObj* %_37map47104, %struct.ScmObj* %argslist56397$_37map471043)
ret void
}

define tailcc void @proc_clo$ae47764(%struct.ScmObj* %env$ae47764,%struct.ScmObj* %current_45args56393) {
%stackaddr$env-ref58013 = alloca %struct.ScmObj*, align 8
%anf_45bind47252 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47764, i64 0)
store %struct.ScmObj* %anf_45bind47252, %struct.ScmObj** %stackaddr$env-ref58013
%stackaddr$env-ref58014 = alloca %struct.ScmObj*, align 8
%k47513 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47764, i64 1)
store %struct.ScmObj* %k47513, %struct.ScmObj** %stackaddr$env-ref58014
%stackaddr$prim58015 = alloca %struct.ScmObj*, align 8
%_95k47515 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56393)
store volatile %struct.ScmObj* %_95k47515, %struct.ScmObj** %stackaddr$prim58015, align 8
%stackaddr$prim58016 = alloca %struct.ScmObj*, align 8
%current_45args56394 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56393)
store volatile %struct.ScmObj* %current_45args56394, %struct.ScmObj** %stackaddr$prim58016, align 8
%stackaddr$prim58017 = alloca %struct.ScmObj*, align 8
%anf_45bind47254 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56394)
store volatile %struct.ScmObj* %anf_45bind47254, %struct.ScmObj** %stackaddr$prim58017, align 8
%stackaddr$prim58018 = alloca %struct.ScmObj*, align 8
%cpsprim47516 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47252, %struct.ScmObj* %anf_45bind47254)
store volatile %struct.ScmObj* %cpsprim47516, %struct.ScmObj** %stackaddr$prim58018, align 8
%ae47770 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56396$k475130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58019 = alloca %struct.ScmObj*, align 8
%argslist56396$k475131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47516, %struct.ScmObj* %argslist56396$k475130)
store volatile %struct.ScmObj* %argslist56396$k475131, %struct.ScmObj** %stackaddr$prim58019, align 8
%stackaddr$prim58020 = alloca %struct.ScmObj*, align 8
%argslist56396$k475132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47770, %struct.ScmObj* %argslist56396$k475131)
store volatile %struct.ScmObj* %argslist56396$k475132, %struct.ScmObj** %stackaddr$prim58020, align 8
%clofunc58021 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47513)
musttail call tailcc void %clofunc58021(%struct.ScmObj* %k47513, %struct.ScmObj* %argslist56396$k475132)
ret void
}

define tailcc void @proc_clo$ae47663(%struct.ScmObj* %env$ae47663,%struct.ScmObj* %current_45args56402) {
%stackaddr$prim58022 = alloca %struct.ScmObj*, align 8
%k47517 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56402)
store volatile %struct.ScmObj* %k47517, %struct.ScmObj** %stackaddr$prim58022, align 8
%stackaddr$prim58023 = alloca %struct.ScmObj*, align 8
%current_45args56403 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56402)
store volatile %struct.ScmObj* %current_45args56403, %struct.ScmObj** %stackaddr$prim58023, align 8
%stackaddr$prim58024 = alloca %struct.ScmObj*, align 8
%_37foldr147108 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56403)
store volatile %struct.ScmObj* %_37foldr147108, %struct.ScmObj** %stackaddr$prim58024, align 8
%ae47665 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58025 = alloca %struct.ScmObj*, align 8
%fptrToInt58026 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47666 to i64
%ae47666 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58026)
store volatile %struct.ScmObj* %ae47666, %struct.ScmObj** %stackaddr$makeclosure58025, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47666, %struct.ScmObj* %_37foldr147108, i64 0)
%argslist56416$k475170 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58027 = alloca %struct.ScmObj*, align 8
%argslist56416$k475171 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47666, %struct.ScmObj* %argslist56416$k475170)
store volatile %struct.ScmObj* %argslist56416$k475171, %struct.ScmObj** %stackaddr$prim58027, align 8
%stackaddr$prim58028 = alloca %struct.ScmObj*, align 8
%argslist56416$k475172 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47665, %struct.ScmObj* %argslist56416$k475171)
store volatile %struct.ScmObj* %argslist56416$k475172, %struct.ScmObj** %stackaddr$prim58028, align 8
%clofunc58029 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47517)
musttail call tailcc void %clofunc58029(%struct.ScmObj* %k47517, %struct.ScmObj* %argslist56416$k475172)
ret void
}

define tailcc void @proc_clo$ae47666(%struct.ScmObj* %env$ae47666,%struct.ScmObj* %current_45args56405) {
%stackaddr$env-ref58030 = alloca %struct.ScmObj*, align 8
%_37foldr147108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47666, i64 0)
store %struct.ScmObj* %_37foldr147108, %struct.ScmObj** %stackaddr$env-ref58030
%stackaddr$prim58031 = alloca %struct.ScmObj*, align 8
%k47518 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56405)
store volatile %struct.ScmObj* %k47518, %struct.ScmObj** %stackaddr$prim58031, align 8
%stackaddr$prim58032 = alloca %struct.ScmObj*, align 8
%current_45args56406 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56405)
store volatile %struct.ScmObj* %current_45args56406, %struct.ScmObj** %stackaddr$prim58032, align 8
%stackaddr$prim58033 = alloca %struct.ScmObj*, align 8
%f47111 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56406)
store volatile %struct.ScmObj* %f47111, %struct.ScmObj** %stackaddr$prim58033, align 8
%stackaddr$prim58034 = alloca %struct.ScmObj*, align 8
%current_45args56407 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56406)
store volatile %struct.ScmObj* %current_45args56407, %struct.ScmObj** %stackaddr$prim58034, align 8
%stackaddr$prim58035 = alloca %struct.ScmObj*, align 8
%acc47110 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56407)
store volatile %struct.ScmObj* %acc47110, %struct.ScmObj** %stackaddr$prim58035, align 8
%stackaddr$prim58036 = alloca %struct.ScmObj*, align 8
%current_45args56408 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56407)
store volatile %struct.ScmObj* %current_45args56408, %struct.ScmObj** %stackaddr$prim58036, align 8
%stackaddr$prim58037 = alloca %struct.ScmObj*, align 8
%lst47109 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56408)
store volatile %struct.ScmObj* %lst47109, %struct.ScmObj** %stackaddr$prim58037, align 8
%stackaddr$prim58038 = alloca %struct.ScmObj*, align 8
%anf_45bind47245 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47109)
store volatile %struct.ScmObj* %anf_45bind47245, %struct.ScmObj** %stackaddr$prim58038, align 8
%truthy$cmp58039 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47245)
%cmp$cmp58039 = icmp eq i64 %truthy$cmp58039, 1
br i1 %cmp$cmp58039, label %truebranch$cmp58039, label %falsebranch$cmp58039
truebranch$cmp58039:
%ae47670 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56410$k475180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58040 = alloca %struct.ScmObj*, align 8
%argslist56410$k475181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47110, %struct.ScmObj* %argslist56410$k475180)
store volatile %struct.ScmObj* %argslist56410$k475181, %struct.ScmObj** %stackaddr$prim58040, align 8
%stackaddr$prim58041 = alloca %struct.ScmObj*, align 8
%argslist56410$k475182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47670, %struct.ScmObj* %argslist56410$k475181)
store volatile %struct.ScmObj* %argslist56410$k475182, %struct.ScmObj** %stackaddr$prim58041, align 8
%clofunc58042 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47518)
musttail call tailcc void %clofunc58042(%struct.ScmObj* %k47518, %struct.ScmObj* %argslist56410$k475182)
ret void
falsebranch$cmp58039:
%stackaddr$prim58043 = alloca %struct.ScmObj*, align 8
%anf_45bind47246 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47109)
store volatile %struct.ScmObj* %anf_45bind47246, %struct.ScmObj** %stackaddr$prim58043, align 8
%stackaddr$prim58044 = alloca %struct.ScmObj*, align 8
%anf_45bind47247 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47109)
store volatile %struct.ScmObj* %anf_45bind47247, %struct.ScmObj** %stackaddr$prim58044, align 8
%stackaddr$makeclosure58045 = alloca %struct.ScmObj*, align 8
%fptrToInt58046 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47678 to i64
%ae47678 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58046)
store volatile %struct.ScmObj* %ae47678, %struct.ScmObj** %stackaddr$makeclosure58045, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47678, %struct.ScmObj* %k47518, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47678, %struct.ScmObj* %anf_45bind47246, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47678, %struct.ScmObj* %f47111, i64 2)
%argslist56415$_37foldr1471080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58047 = alloca %struct.ScmObj*, align 8
%argslist56415$_37foldr1471081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47247, %struct.ScmObj* %argslist56415$_37foldr1471080)
store volatile %struct.ScmObj* %argslist56415$_37foldr1471081, %struct.ScmObj** %stackaddr$prim58047, align 8
%stackaddr$prim58048 = alloca %struct.ScmObj*, align 8
%argslist56415$_37foldr1471082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47110, %struct.ScmObj* %argslist56415$_37foldr1471081)
store volatile %struct.ScmObj* %argslist56415$_37foldr1471082, %struct.ScmObj** %stackaddr$prim58048, align 8
%stackaddr$prim58049 = alloca %struct.ScmObj*, align 8
%argslist56415$_37foldr1471083 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47111, %struct.ScmObj* %argslist56415$_37foldr1471082)
store volatile %struct.ScmObj* %argslist56415$_37foldr1471083, %struct.ScmObj** %stackaddr$prim58049, align 8
%stackaddr$prim58050 = alloca %struct.ScmObj*, align 8
%argslist56415$_37foldr1471084 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47678, %struct.ScmObj* %argslist56415$_37foldr1471083)
store volatile %struct.ScmObj* %argslist56415$_37foldr1471084, %struct.ScmObj** %stackaddr$prim58050, align 8
%clofunc58051 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147108)
musttail call tailcc void %clofunc58051(%struct.ScmObj* %_37foldr147108, %struct.ScmObj* %argslist56415$_37foldr1471084)
ret void
}

define tailcc void @proc_clo$ae47678(%struct.ScmObj* %env$ae47678,%struct.ScmObj* %current_45args56411) {
%stackaddr$env-ref58052 = alloca %struct.ScmObj*, align 8
%k47518 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47678, i64 0)
store %struct.ScmObj* %k47518, %struct.ScmObj** %stackaddr$env-ref58052
%stackaddr$env-ref58053 = alloca %struct.ScmObj*, align 8
%anf_45bind47246 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47678, i64 1)
store %struct.ScmObj* %anf_45bind47246, %struct.ScmObj** %stackaddr$env-ref58053
%stackaddr$env-ref58054 = alloca %struct.ScmObj*, align 8
%f47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47678, i64 2)
store %struct.ScmObj* %f47111, %struct.ScmObj** %stackaddr$env-ref58054
%stackaddr$prim58055 = alloca %struct.ScmObj*, align 8
%_95k47519 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56411)
store volatile %struct.ScmObj* %_95k47519, %struct.ScmObj** %stackaddr$prim58055, align 8
%stackaddr$prim58056 = alloca %struct.ScmObj*, align 8
%current_45args56412 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56411)
store volatile %struct.ScmObj* %current_45args56412, %struct.ScmObj** %stackaddr$prim58056, align 8
%stackaddr$prim58057 = alloca %struct.ScmObj*, align 8
%anf_45bind47248 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56412)
store volatile %struct.ScmObj* %anf_45bind47248, %struct.ScmObj** %stackaddr$prim58057, align 8
%argslist56414$f471110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58058 = alloca %struct.ScmObj*, align 8
%argslist56414$f471111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47248, %struct.ScmObj* %argslist56414$f471110)
store volatile %struct.ScmObj* %argslist56414$f471111, %struct.ScmObj** %stackaddr$prim58058, align 8
%stackaddr$prim58059 = alloca %struct.ScmObj*, align 8
%argslist56414$f471112 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47246, %struct.ScmObj* %argslist56414$f471111)
store volatile %struct.ScmObj* %argslist56414$f471112, %struct.ScmObj** %stackaddr$prim58059, align 8
%stackaddr$prim58060 = alloca %struct.ScmObj*, align 8
%argslist56414$f471113 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47518, %struct.ScmObj* %argslist56414$f471112)
store volatile %struct.ScmObj* %argslist56414$f471113, %struct.ScmObj** %stackaddr$prim58060, align 8
%clofunc58061 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47111)
musttail call tailcc void %clofunc58061(%struct.ScmObj* %f47111, %struct.ScmObj* %argslist56414$f471113)
ret void
}

define tailcc void @proc_clo$ae47546(%struct.ScmObj* %env$ae47546,%struct.ScmObj* %current_45args56419) {
%stackaddr$prim58062 = alloca %struct.ScmObj*, align 8
%k47520 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56419)
store volatile %struct.ScmObj* %k47520, %struct.ScmObj** %stackaddr$prim58062, align 8
%stackaddr$prim58063 = alloca %struct.ScmObj*, align 8
%current_45args56420 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56419)
store volatile %struct.ScmObj* %current_45args56420, %struct.ScmObj** %stackaddr$prim58063, align 8
%stackaddr$prim58064 = alloca %struct.ScmObj*, align 8
%y47088 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56420)
store volatile %struct.ScmObj* %y47088, %struct.ScmObj** %stackaddr$prim58064, align 8
%ae47548 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58065 = alloca %struct.ScmObj*, align 8
%fptrToInt58066 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47549 to i64
%ae47549 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58066)
store volatile %struct.ScmObj* %ae47549, %struct.ScmObj** %stackaddr$makeclosure58065, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47549, %struct.ScmObj* %y47088, i64 0)
%argslist56438$k475200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58067 = alloca %struct.ScmObj*, align 8
%argslist56438$k475201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47549, %struct.ScmObj* %argslist56438$k475200)
store volatile %struct.ScmObj* %argslist56438$k475201, %struct.ScmObj** %stackaddr$prim58067, align 8
%stackaddr$prim58068 = alloca %struct.ScmObj*, align 8
%argslist56438$k475202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47548, %struct.ScmObj* %argslist56438$k475201)
store volatile %struct.ScmObj* %argslist56438$k475202, %struct.ScmObj** %stackaddr$prim58068, align 8
%clofunc58069 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47520)
musttail call tailcc void %clofunc58069(%struct.ScmObj* %k47520, %struct.ScmObj* %argslist56438$k475202)
ret void
}

define tailcc void @proc_clo$ae47549(%struct.ScmObj* %env$ae47549,%struct.ScmObj* %current_45args56422) {
%stackaddr$env-ref58070 = alloca %struct.ScmObj*, align 8
%y47088 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47549, i64 0)
store %struct.ScmObj* %y47088, %struct.ScmObj** %stackaddr$env-ref58070
%stackaddr$prim58071 = alloca %struct.ScmObj*, align 8
%k47521 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56422)
store volatile %struct.ScmObj* %k47521, %struct.ScmObj** %stackaddr$prim58071, align 8
%stackaddr$prim58072 = alloca %struct.ScmObj*, align 8
%current_45args56423 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56422)
store volatile %struct.ScmObj* %current_45args56423, %struct.ScmObj** %stackaddr$prim58072, align 8
%stackaddr$prim58073 = alloca %struct.ScmObj*, align 8
%f47089 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56423)
store volatile %struct.ScmObj* %f47089, %struct.ScmObj** %stackaddr$prim58073, align 8
%stackaddr$makeclosure58074 = alloca %struct.ScmObj*, align 8
%fptrToInt58075 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47550 to i64
%ae47550 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58075)
store volatile %struct.ScmObj* %ae47550, %struct.ScmObj** %stackaddr$makeclosure58074, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47550, %struct.ScmObj* %f47089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47550, %struct.ScmObj* %k47521, i64 1)
%ae47551 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58076 = alloca %struct.ScmObj*, align 8
%fptrToInt58077 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47552 to i64
%ae47552 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58077)
store volatile %struct.ScmObj* %ae47552, %struct.ScmObj** %stackaddr$makeclosure58076, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47552, %struct.ScmObj* %f47089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47552, %struct.ScmObj* %y47088, i64 1)
%argslist56437$ae475500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58078 = alloca %struct.ScmObj*, align 8
%argslist56437$ae475501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47552, %struct.ScmObj* %argslist56437$ae475500)
store volatile %struct.ScmObj* %argslist56437$ae475501, %struct.ScmObj** %stackaddr$prim58078, align 8
%stackaddr$prim58079 = alloca %struct.ScmObj*, align 8
%argslist56437$ae475502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47551, %struct.ScmObj* %argslist56437$ae475501)
store volatile %struct.ScmObj* %argslist56437$ae475502, %struct.ScmObj** %stackaddr$prim58079, align 8
%clofunc58080 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47550)
musttail call tailcc void %clofunc58080(%struct.ScmObj* %ae47550, %struct.ScmObj* %argslist56437$ae475502)
ret void
}

define tailcc void @proc_clo$ae47550(%struct.ScmObj* %env$ae47550,%struct.ScmObj* %current_45args56425) {
%stackaddr$env-ref58081 = alloca %struct.ScmObj*, align 8
%f47089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47550, i64 0)
store %struct.ScmObj* %f47089, %struct.ScmObj** %stackaddr$env-ref58081
%stackaddr$env-ref58082 = alloca %struct.ScmObj*, align 8
%k47521 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47550, i64 1)
store %struct.ScmObj* %k47521, %struct.ScmObj** %stackaddr$env-ref58082
%stackaddr$prim58083 = alloca %struct.ScmObj*, align 8
%_95k47522 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56425)
store volatile %struct.ScmObj* %_95k47522, %struct.ScmObj** %stackaddr$prim58083, align 8
%stackaddr$prim58084 = alloca %struct.ScmObj*, align 8
%current_45args56426 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56425)
store volatile %struct.ScmObj* %current_45args56426, %struct.ScmObj** %stackaddr$prim58084, align 8
%stackaddr$prim58085 = alloca %struct.ScmObj*, align 8
%anf_45bind47243 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56426)
store volatile %struct.ScmObj* %anf_45bind47243, %struct.ScmObj** %stackaddr$prim58085, align 8
%argslist56428$f470890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58086 = alloca %struct.ScmObj*, align 8
%argslist56428$f470891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47243, %struct.ScmObj* %argslist56428$f470890)
store volatile %struct.ScmObj* %argslist56428$f470891, %struct.ScmObj** %stackaddr$prim58086, align 8
%stackaddr$prim58087 = alloca %struct.ScmObj*, align 8
%argslist56428$f470892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47521, %struct.ScmObj* %argslist56428$f470891)
store volatile %struct.ScmObj* %argslist56428$f470892, %struct.ScmObj** %stackaddr$prim58087, align 8
%clofunc58088 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47089)
musttail call tailcc void %clofunc58088(%struct.ScmObj* %f47089, %struct.ScmObj* %argslist56428$f470892)
ret void
}

define tailcc void @proc_clo$ae47552(%struct.ScmObj* %env$ae47552,%struct.ScmObj* %args4709047523) {
%stackaddr$env-ref58089 = alloca %struct.ScmObj*, align 8
%f47089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47552, i64 0)
store %struct.ScmObj* %f47089, %struct.ScmObj** %stackaddr$env-ref58089
%stackaddr$env-ref58090 = alloca %struct.ScmObj*, align 8
%y47088 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47552, i64 1)
store %struct.ScmObj* %y47088, %struct.ScmObj** %stackaddr$env-ref58090
%stackaddr$prim58091 = alloca %struct.ScmObj*, align 8
%k47524 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4709047523)
store volatile %struct.ScmObj* %k47524, %struct.ScmObj** %stackaddr$prim58091, align 8
%stackaddr$prim58092 = alloca %struct.ScmObj*, align 8
%args47090 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4709047523)
store volatile %struct.ScmObj* %args47090, %struct.ScmObj** %stackaddr$prim58092, align 8
%stackaddr$makeclosure58093 = alloca %struct.ScmObj*, align 8
%fptrToInt58094 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47556 to i64
%ae47556 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58094)
store volatile %struct.ScmObj* %ae47556, %struct.ScmObj** %stackaddr$makeclosure58093, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47556, %struct.ScmObj* %k47524, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47556, %struct.ScmObj* %args47090, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47556, %struct.ScmObj* %f47089, i64 2)
%argslist56436$y470880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58095 = alloca %struct.ScmObj*, align 8
%argslist56436$y470881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y47088, %struct.ScmObj* %argslist56436$y470880)
store volatile %struct.ScmObj* %argslist56436$y470881, %struct.ScmObj** %stackaddr$prim58095, align 8
%stackaddr$prim58096 = alloca %struct.ScmObj*, align 8
%argslist56436$y470882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47556, %struct.ScmObj* %argslist56436$y470881)
store volatile %struct.ScmObj* %argslist56436$y470882, %struct.ScmObj** %stackaddr$prim58096, align 8
%clofunc58097 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y47088)
musttail call tailcc void %clofunc58097(%struct.ScmObj* %y47088, %struct.ScmObj* %argslist56436$y470882)
ret void
}

define tailcc void @proc_clo$ae47556(%struct.ScmObj* %env$ae47556,%struct.ScmObj* %current_45args56429) {
%stackaddr$env-ref58098 = alloca %struct.ScmObj*, align 8
%k47524 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47556, i64 0)
store %struct.ScmObj* %k47524, %struct.ScmObj** %stackaddr$env-ref58098
%stackaddr$env-ref58099 = alloca %struct.ScmObj*, align 8
%args47090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47556, i64 1)
store %struct.ScmObj* %args47090, %struct.ScmObj** %stackaddr$env-ref58099
%stackaddr$env-ref58100 = alloca %struct.ScmObj*, align 8
%f47089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47556, i64 2)
store %struct.ScmObj* %f47089, %struct.ScmObj** %stackaddr$env-ref58100
%stackaddr$prim58101 = alloca %struct.ScmObj*, align 8
%_95k47525 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56429)
store volatile %struct.ScmObj* %_95k47525, %struct.ScmObj** %stackaddr$prim58101, align 8
%stackaddr$prim58102 = alloca %struct.ScmObj*, align 8
%current_45args56430 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56429)
store volatile %struct.ScmObj* %current_45args56430, %struct.ScmObj** %stackaddr$prim58102, align 8
%stackaddr$prim58103 = alloca %struct.ScmObj*, align 8
%anf_45bind47241 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56430)
store volatile %struct.ScmObj* %anf_45bind47241, %struct.ScmObj** %stackaddr$prim58103, align 8
%stackaddr$makeclosure58104 = alloca %struct.ScmObj*, align 8
%fptrToInt58105 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47559 to i64
%ae47559 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58105)
store volatile %struct.ScmObj* %ae47559, %struct.ScmObj** %stackaddr$makeclosure58104, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47559, %struct.ScmObj* %k47524, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47559, %struct.ScmObj* %args47090, i64 1)
%argslist56435$anf_45bind472410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58106 = alloca %struct.ScmObj*, align 8
%argslist56435$anf_45bind472411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47089, %struct.ScmObj* %argslist56435$anf_45bind472410)
store volatile %struct.ScmObj* %argslist56435$anf_45bind472411, %struct.ScmObj** %stackaddr$prim58106, align 8
%stackaddr$prim58107 = alloca %struct.ScmObj*, align 8
%argslist56435$anf_45bind472412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47559, %struct.ScmObj* %argslist56435$anf_45bind472411)
store volatile %struct.ScmObj* %argslist56435$anf_45bind472412, %struct.ScmObj** %stackaddr$prim58107, align 8
%clofunc58108 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47241)
musttail call tailcc void %clofunc58108(%struct.ScmObj* %anf_45bind47241, %struct.ScmObj* %argslist56435$anf_45bind472412)
ret void
}

define tailcc void @proc_clo$ae47559(%struct.ScmObj* %env$ae47559,%struct.ScmObj* %current_45args56432) {
%stackaddr$env-ref58109 = alloca %struct.ScmObj*, align 8
%k47524 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47559, i64 0)
store %struct.ScmObj* %k47524, %struct.ScmObj** %stackaddr$env-ref58109
%stackaddr$env-ref58110 = alloca %struct.ScmObj*, align 8
%args47090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47559, i64 1)
store %struct.ScmObj* %args47090, %struct.ScmObj** %stackaddr$env-ref58110
%stackaddr$prim58111 = alloca %struct.ScmObj*, align 8
%_95k47526 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56432)
store volatile %struct.ScmObj* %_95k47526, %struct.ScmObj** %stackaddr$prim58111, align 8
%stackaddr$prim58112 = alloca %struct.ScmObj*, align 8
%current_45args56433 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56432)
store volatile %struct.ScmObj* %current_45args56433, %struct.ScmObj** %stackaddr$prim58112, align 8
%stackaddr$prim58113 = alloca %struct.ScmObj*, align 8
%anf_45bind47242 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56433)
store volatile %struct.ScmObj* %anf_45bind47242, %struct.ScmObj** %stackaddr$prim58113, align 8
%stackaddr$prim58114 = alloca %struct.ScmObj*, align 8
%cpsargs47527 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47524, %struct.ScmObj* %args47090)
store volatile %struct.ScmObj* %cpsargs47527, %struct.ScmObj** %stackaddr$prim58114, align 8
%clofunc58115 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47242)
musttail call tailcc void %clofunc58115(%struct.ScmObj* %anf_45bind47242, %struct.ScmObj* %cpsargs47527)
ret void
}

define tailcc void @proc_clo$ae47531(%struct.ScmObj* %env$ae47531,%struct.ScmObj* %current_45args56440) {
%stackaddr$prim58116 = alloca %struct.ScmObj*, align 8
%k47528 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56440)
store volatile %struct.ScmObj* %k47528, %struct.ScmObj** %stackaddr$prim58116, align 8
%stackaddr$prim58117 = alloca %struct.ScmObj*, align 8
%current_45args56441 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56440)
store volatile %struct.ScmObj* %current_45args56441, %struct.ScmObj** %stackaddr$prim58117, align 8
%stackaddr$prim58118 = alloca %struct.ScmObj*, align 8
%yu47087 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56441)
store volatile %struct.ScmObj* %yu47087, %struct.ScmObj** %stackaddr$prim58118, align 8
%argslist56443$yu470870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58119 = alloca %struct.ScmObj*, align 8
%argslist56443$yu470871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu47087, %struct.ScmObj* %argslist56443$yu470870)
store volatile %struct.ScmObj* %argslist56443$yu470871, %struct.ScmObj** %stackaddr$prim58119, align 8
%stackaddr$prim58120 = alloca %struct.ScmObj*, align 8
%argslist56443$yu470872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47528, %struct.ScmObj* %argslist56443$yu470871)
store volatile %struct.ScmObj* %argslist56443$yu470872, %struct.ScmObj** %stackaddr$prim58120, align 8
%clofunc58121 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu47087)
musttail call tailcc void %clofunc58121(%struct.ScmObj* %yu47087, %struct.ScmObj* %argslist56443$yu470872)
ret void
}