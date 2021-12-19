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

@global$sym$ae5190057822 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae5190357824 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae5190657826 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae5190957828 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae5191257830 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae5191557832 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae5191857834 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae5192157836 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae5192457838 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae5192757840 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae5193057842 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae5193357844 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae5193657846 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae5193957848 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae5194257850 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae5194557852 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae5194857854 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae5195157856 = private unnamed_addr constant [2 x i8] c"a\00", align 8
@global$sym$ae5195457860 = private unnamed_addr constant [8 x i8] c"ignored\00", align 8

define ccc i32 @main() {
%mainenv57404 = call %struct.ScmObj* @const_init_null()
%mainargs57405 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv57404, %struct.ScmObj* %mainargs57405)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv57402,%struct.ScmObj* %mainargs57403) {
%stackaddr$makeclosure57406 = alloca %struct.ScmObj*, align 8
%fptrToInt57407 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48486 to i64
%ae48486 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57407)
store volatile %struct.ScmObj* %ae48486, %struct.ScmObj** %stackaddr$makeclosure57406, align 8
%ae48487 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57408 = alloca %struct.ScmObj*, align 8
%fptrToInt57409 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48488 to i64
%ae48488 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57409)
store volatile %struct.ScmObj* %ae48488, %struct.ScmObj** %stackaddr$makeclosure57408, align 8
%argslist57401$ae484860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57410 = alloca %struct.ScmObj*, align 8
%argslist57401$ae484861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48488, %struct.ScmObj* %argslist57401$ae484860)
store volatile %struct.ScmObj* %argslist57401$ae484861, %struct.ScmObj** %stackaddr$prim57410, align 8
%stackaddr$prim57411 = alloca %struct.ScmObj*, align 8
%argslist57401$ae484862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48487, %struct.ScmObj* %argslist57401$ae484861)
store volatile %struct.ScmObj* %argslist57401$ae484862, %struct.ScmObj** %stackaddr$prim57411, align 8
%clofunc57412 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48486)
musttail call tailcc void %clofunc57412(%struct.ScmObj* %ae48486, %struct.ScmObj* %argslist57401$ae484862)
ret void
}

define tailcc void @proc_clo$ae48486(%struct.ScmObj* %env$ae48486,%struct.ScmObj* %current_45args56875) {
%stackaddr$prim57413 = alloca %struct.ScmObj*, align 8
%_95k48326 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56875)
store volatile %struct.ScmObj* %_95k48326, %struct.ScmObj** %stackaddr$prim57413, align 8
%stackaddr$prim57414 = alloca %struct.ScmObj*, align 8
%current_45args56876 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56875)
store volatile %struct.ScmObj* %current_45args56876, %struct.ScmObj** %stackaddr$prim57414, align 8
%stackaddr$prim57415 = alloca %struct.ScmObj*, align 8
%anf_45bind48197 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56876)
store volatile %struct.ScmObj* %anf_45bind48197, %struct.ScmObj** %stackaddr$prim57415, align 8
%stackaddr$makeclosure57416 = alloca %struct.ScmObj*, align 8
%fptrToInt57417 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48501 to i64
%ae48501 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57417)
store volatile %struct.ScmObj* %ae48501, %struct.ScmObj** %stackaddr$makeclosure57416, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48501, %struct.ScmObj* %anf_45bind48197, i64 0)
%ae48502 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57418 = alloca %struct.ScmObj*, align 8
%fptrToInt57419 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48503 to i64
%ae48503 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57419)
store volatile %struct.ScmObj* %ae48503, %struct.ScmObj** %stackaddr$makeclosure57418, align 8
%argslist57396$ae485010 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57420 = alloca %struct.ScmObj*, align 8
%argslist57396$ae485011 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48503, %struct.ScmObj* %argslist57396$ae485010)
store volatile %struct.ScmObj* %argslist57396$ae485011, %struct.ScmObj** %stackaddr$prim57420, align 8
%stackaddr$prim57421 = alloca %struct.ScmObj*, align 8
%argslist57396$ae485012 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48502, %struct.ScmObj* %argslist57396$ae485011)
store volatile %struct.ScmObj* %argslist57396$ae485012, %struct.ScmObj** %stackaddr$prim57421, align 8
%clofunc57422 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48501)
musttail call tailcc void %clofunc57422(%struct.ScmObj* %ae48501, %struct.ScmObj* %argslist57396$ae485012)
ret void
}

define tailcc void @proc_clo$ae48501(%struct.ScmObj* %env$ae48501,%struct.ScmObj* %current_45args56878) {
%stackaddr$env-ref57423 = alloca %struct.ScmObj*, align 8
%anf_45bind48197 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48501, i64 0)
store %struct.ScmObj* %anf_45bind48197, %struct.ScmObj** %stackaddr$env-ref57423
%stackaddr$prim57424 = alloca %struct.ScmObj*, align 8
%_95k48327 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56878)
store volatile %struct.ScmObj* %_95k48327, %struct.ScmObj** %stackaddr$prim57424, align 8
%stackaddr$prim57425 = alloca %struct.ScmObj*, align 8
%current_45args56879 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56878)
store volatile %struct.ScmObj* %current_45args56879, %struct.ScmObj** %stackaddr$prim57425, align 8
%stackaddr$prim57426 = alloca %struct.ScmObj*, align 8
%anf_45bind48201 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56879)
store volatile %struct.ScmObj* %anf_45bind48201, %struct.ScmObj** %stackaddr$prim57426, align 8
%stackaddr$makeclosure57427 = alloca %struct.ScmObj*, align 8
%fptrToInt57428 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48616 to i64
%ae48616 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57428)
store volatile %struct.ScmObj* %ae48616, %struct.ScmObj** %stackaddr$makeclosure57427, align 8
%argslist57375$anf_45bind481970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57429 = alloca %struct.ScmObj*, align 8
%argslist57375$anf_45bind481971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48201, %struct.ScmObj* %argslist57375$anf_45bind481970)
store volatile %struct.ScmObj* %argslist57375$anf_45bind481971, %struct.ScmObj** %stackaddr$prim57429, align 8
%stackaddr$prim57430 = alloca %struct.ScmObj*, align 8
%argslist57375$anf_45bind481972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48616, %struct.ScmObj* %argslist57375$anf_45bind481971)
store volatile %struct.ScmObj* %argslist57375$anf_45bind481972, %struct.ScmObj** %stackaddr$prim57430, align 8
%clofunc57431 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48197)
musttail call tailcc void %clofunc57431(%struct.ScmObj* %anf_45bind48197, %struct.ScmObj* %argslist57375$anf_45bind481972)
ret void
}

define tailcc void @proc_clo$ae48616(%struct.ScmObj* %env$ae48616,%struct.ScmObj* %current_45args56881) {
%stackaddr$prim57432 = alloca %struct.ScmObj*, align 8
%_95k48328 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56881)
store volatile %struct.ScmObj* %_95k48328, %struct.ScmObj** %stackaddr$prim57432, align 8
%stackaddr$prim57433 = alloca %struct.ScmObj*, align 8
%current_45args56882 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56881)
store volatile %struct.ScmObj* %current_45args56882, %struct.ScmObj** %stackaddr$prim57433, align 8
%stackaddr$prim57434 = alloca %struct.ScmObj*, align 8
%Ycmb48043 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56882)
store volatile %struct.ScmObj* %Ycmb48043, %struct.ScmObj** %stackaddr$prim57434, align 8
%stackaddr$makeclosure57435 = alloca %struct.ScmObj*, align 8
%fptrToInt57436 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48618 to i64
%ae48618 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57436)
store volatile %struct.ScmObj* %ae48618, %struct.ScmObj** %stackaddr$makeclosure57435, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48618, %struct.ScmObj* %Ycmb48043, i64 0)
%ae48619 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57437 = alloca %struct.ScmObj*, align 8
%fptrToInt57438 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48620 to i64
%ae48620 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57438)
store volatile %struct.ScmObj* %ae48620, %struct.ScmObj** %stackaddr$makeclosure57437, align 8
%argslist57374$ae486180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57439 = alloca %struct.ScmObj*, align 8
%argslist57374$ae486181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48620, %struct.ScmObj* %argslist57374$ae486180)
store volatile %struct.ScmObj* %argslist57374$ae486181, %struct.ScmObj** %stackaddr$prim57439, align 8
%stackaddr$prim57440 = alloca %struct.ScmObj*, align 8
%argslist57374$ae486182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48619, %struct.ScmObj* %argslist57374$ae486181)
store volatile %struct.ScmObj* %argslist57374$ae486182, %struct.ScmObj** %stackaddr$prim57440, align 8
%clofunc57441 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48618)
musttail call tailcc void %clofunc57441(%struct.ScmObj* %ae48618, %struct.ScmObj* %argslist57374$ae486182)
ret void
}

define tailcc void @proc_clo$ae48618(%struct.ScmObj* %env$ae48618,%struct.ScmObj* %current_45args56884) {
%stackaddr$env-ref57442 = alloca %struct.ScmObj*, align 8
%Ycmb48043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48618, i64 0)
store %struct.ScmObj* %Ycmb48043, %struct.ScmObj** %stackaddr$env-ref57442
%stackaddr$prim57443 = alloca %struct.ScmObj*, align 8
%_95k48329 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56884)
store volatile %struct.ScmObj* %_95k48329, %struct.ScmObj** %stackaddr$prim57443, align 8
%stackaddr$prim57444 = alloca %struct.ScmObj*, align 8
%current_45args56885 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56884)
store volatile %struct.ScmObj* %current_45args56885, %struct.ScmObj** %stackaddr$prim57444, align 8
%stackaddr$prim57445 = alloca %struct.ScmObj*, align 8
%anf_45bind48206 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56885)
store volatile %struct.ScmObj* %anf_45bind48206, %struct.ScmObj** %stackaddr$prim57445, align 8
%stackaddr$makeclosure57446 = alloca %struct.ScmObj*, align 8
%fptrToInt57447 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48696 to i64
%ae48696 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57447)
store volatile %struct.ScmObj* %ae48696, %struct.ScmObj** %stackaddr$makeclosure57446, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48696, %struct.ScmObj* %Ycmb48043, i64 0)
%argslist57358$Ycmb480430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57448 = alloca %struct.ScmObj*, align 8
%argslist57358$Ycmb480431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48206, %struct.ScmObj* %argslist57358$Ycmb480430)
store volatile %struct.ScmObj* %argslist57358$Ycmb480431, %struct.ScmObj** %stackaddr$prim57448, align 8
%stackaddr$prim57449 = alloca %struct.ScmObj*, align 8
%argslist57358$Ycmb480432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48696, %struct.ScmObj* %argslist57358$Ycmb480431)
store volatile %struct.ScmObj* %argslist57358$Ycmb480432, %struct.ScmObj** %stackaddr$prim57449, align 8
%clofunc57450 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48043)
musttail call tailcc void %clofunc57450(%struct.ScmObj* %Ycmb48043, %struct.ScmObj* %argslist57358$Ycmb480432)
ret void
}

define tailcc void @proc_clo$ae48696(%struct.ScmObj* %env$ae48696,%struct.ScmObj* %current_45args56887) {
%stackaddr$env-ref57451 = alloca %struct.ScmObj*, align 8
%Ycmb48043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48696, i64 0)
store %struct.ScmObj* %Ycmb48043, %struct.ScmObj** %stackaddr$env-ref57451
%stackaddr$prim57452 = alloca %struct.ScmObj*, align 8
%_95k48330 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56887)
store volatile %struct.ScmObj* %_95k48330, %struct.ScmObj** %stackaddr$prim57452, align 8
%stackaddr$prim57453 = alloca %struct.ScmObj*, align 8
%current_45args56888 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56887)
store volatile %struct.ScmObj* %current_45args56888, %struct.ScmObj** %stackaddr$prim57453, align 8
%stackaddr$prim57454 = alloca %struct.ScmObj*, align 8
%_37foldr148064 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56888)
store volatile %struct.ScmObj* %_37foldr148064, %struct.ScmObj** %stackaddr$prim57454, align 8
%stackaddr$makeclosure57455 = alloca %struct.ScmObj*, align 8
%fptrToInt57456 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48698 to i64
%ae48698 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57456)
store volatile %struct.ScmObj* %ae48698, %struct.ScmObj** %stackaddr$makeclosure57455, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48698, %struct.ScmObj* %_37foldr148064, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48698, %struct.ScmObj* %Ycmb48043, i64 1)
%ae48699 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57457 = alloca %struct.ScmObj*, align 8
%fptrToInt57458 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48700 to i64
%ae48700 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57458)
store volatile %struct.ScmObj* %ae48700, %struct.ScmObj** %stackaddr$makeclosure57457, align 8
%argslist57357$ae486980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57459 = alloca %struct.ScmObj*, align 8
%argslist57357$ae486981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48700, %struct.ScmObj* %argslist57357$ae486980)
store volatile %struct.ScmObj* %argslist57357$ae486981, %struct.ScmObj** %stackaddr$prim57459, align 8
%stackaddr$prim57460 = alloca %struct.ScmObj*, align 8
%argslist57357$ae486982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48699, %struct.ScmObj* %argslist57357$ae486981)
store volatile %struct.ScmObj* %argslist57357$ae486982, %struct.ScmObj** %stackaddr$prim57460, align 8
%clofunc57461 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48698)
musttail call tailcc void %clofunc57461(%struct.ScmObj* %ae48698, %struct.ScmObj* %argslist57357$ae486982)
ret void
}

define tailcc void @proc_clo$ae48698(%struct.ScmObj* %env$ae48698,%struct.ScmObj* %current_45args56890) {
%stackaddr$env-ref57462 = alloca %struct.ScmObj*, align 8
%_37foldr148064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48698, i64 0)
store %struct.ScmObj* %_37foldr148064, %struct.ScmObj** %stackaddr$env-ref57462
%stackaddr$env-ref57463 = alloca %struct.ScmObj*, align 8
%Ycmb48043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48698, i64 1)
store %struct.ScmObj* %Ycmb48043, %struct.ScmObj** %stackaddr$env-ref57463
%stackaddr$prim57464 = alloca %struct.ScmObj*, align 8
%_95k48331 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56890)
store volatile %struct.ScmObj* %_95k48331, %struct.ScmObj** %stackaddr$prim57464, align 8
%stackaddr$prim57465 = alloca %struct.ScmObj*, align 8
%current_45args56891 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56890)
store volatile %struct.ScmObj* %current_45args56891, %struct.ScmObj** %stackaddr$prim57465, align 8
%stackaddr$prim57466 = alloca %struct.ScmObj*, align 8
%anf_45bind48212 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56891)
store volatile %struct.ScmObj* %anf_45bind48212, %struct.ScmObj** %stackaddr$prim57466, align 8
%stackaddr$makeclosure57467 = alloca %struct.ScmObj*, align 8
%fptrToInt57468 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48793 to i64
%ae48793 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57468)
store volatile %struct.ScmObj* %ae48793, %struct.ScmObj** %stackaddr$makeclosure57467, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48793, %struct.ScmObj* %_37foldr148064, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48793, %struct.ScmObj* %Ycmb48043, i64 1)
%argslist57338$Ycmb480430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57469 = alloca %struct.ScmObj*, align 8
%argslist57338$Ycmb480431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48212, %struct.ScmObj* %argslist57338$Ycmb480430)
store volatile %struct.ScmObj* %argslist57338$Ycmb480431, %struct.ScmObj** %stackaddr$prim57469, align 8
%stackaddr$prim57470 = alloca %struct.ScmObj*, align 8
%argslist57338$Ycmb480432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48793, %struct.ScmObj* %argslist57338$Ycmb480431)
store volatile %struct.ScmObj* %argslist57338$Ycmb480432, %struct.ScmObj** %stackaddr$prim57470, align 8
%clofunc57471 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48043)
musttail call tailcc void %clofunc57471(%struct.ScmObj* %Ycmb48043, %struct.ScmObj* %argslist57338$Ycmb480432)
ret void
}

define tailcc void @proc_clo$ae48793(%struct.ScmObj* %env$ae48793,%struct.ScmObj* %current_45args56893) {
%stackaddr$env-ref57472 = alloca %struct.ScmObj*, align 8
%_37foldr148064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48793, i64 0)
store %struct.ScmObj* %_37foldr148064, %struct.ScmObj** %stackaddr$env-ref57472
%stackaddr$env-ref57473 = alloca %struct.ScmObj*, align 8
%Ycmb48043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48793, i64 1)
store %struct.ScmObj* %Ycmb48043, %struct.ScmObj** %stackaddr$env-ref57473
%stackaddr$prim57474 = alloca %struct.ScmObj*, align 8
%_95k48332 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56893)
store volatile %struct.ScmObj* %_95k48332, %struct.ScmObj** %stackaddr$prim57474, align 8
%stackaddr$prim57475 = alloca %struct.ScmObj*, align 8
%current_45args56894 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56893)
store volatile %struct.ScmObj* %current_45args56894, %struct.ScmObj** %stackaddr$prim57475, align 8
%stackaddr$prim57476 = alloca %struct.ScmObj*, align 8
%_37map148060 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56894)
store volatile %struct.ScmObj* %_37map148060, %struct.ScmObj** %stackaddr$prim57476, align 8
%stackaddr$makeclosure57477 = alloca %struct.ScmObj*, align 8
%fptrToInt57478 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48795 to i64
%ae48795 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57478)
store volatile %struct.ScmObj* %ae48795, %struct.ScmObj** %stackaddr$makeclosure57477, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48795, %struct.ScmObj* %_37foldr148064, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48795, %struct.ScmObj* %_37map148060, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48795, %struct.ScmObj* %Ycmb48043, i64 2)
%ae48796 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57479 = alloca %struct.ScmObj*, align 8
%fptrToInt57480 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48797 to i64
%ae48797 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57480)
store volatile %struct.ScmObj* %ae48797, %struct.ScmObj** %stackaddr$makeclosure57479, align 8
%argslist57337$ae487950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57481 = alloca %struct.ScmObj*, align 8
%argslist57337$ae487951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48797, %struct.ScmObj* %argslist57337$ae487950)
store volatile %struct.ScmObj* %argslist57337$ae487951, %struct.ScmObj** %stackaddr$prim57481, align 8
%stackaddr$prim57482 = alloca %struct.ScmObj*, align 8
%argslist57337$ae487952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48796, %struct.ScmObj* %argslist57337$ae487951)
store volatile %struct.ScmObj* %argslist57337$ae487952, %struct.ScmObj** %stackaddr$prim57482, align 8
%clofunc57483 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48795)
musttail call tailcc void %clofunc57483(%struct.ScmObj* %ae48795, %struct.ScmObj* %argslist57337$ae487952)
ret void
}

define tailcc void @proc_clo$ae48795(%struct.ScmObj* %env$ae48795,%struct.ScmObj* %current_45args56896) {
%stackaddr$env-ref57484 = alloca %struct.ScmObj*, align 8
%_37foldr148064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48795, i64 0)
store %struct.ScmObj* %_37foldr148064, %struct.ScmObj** %stackaddr$env-ref57484
%stackaddr$env-ref57485 = alloca %struct.ScmObj*, align 8
%_37map148060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48795, i64 1)
store %struct.ScmObj* %_37map148060, %struct.ScmObj** %stackaddr$env-ref57485
%stackaddr$env-ref57486 = alloca %struct.ScmObj*, align 8
%Ycmb48043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48795, i64 2)
store %struct.ScmObj* %Ycmb48043, %struct.ScmObj** %stackaddr$env-ref57486
%stackaddr$prim57487 = alloca %struct.ScmObj*, align 8
%_95k48333 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56896)
store volatile %struct.ScmObj* %_95k48333, %struct.ScmObj** %stackaddr$prim57487, align 8
%stackaddr$prim57488 = alloca %struct.ScmObj*, align 8
%current_45args56897 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56896)
store volatile %struct.ScmObj* %current_45args56897, %struct.ScmObj** %stackaddr$prim57488, align 8
%stackaddr$prim57489 = alloca %struct.ScmObj*, align 8
%anf_45bind48219 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56897)
store volatile %struct.ScmObj* %anf_45bind48219, %struct.ScmObj** %stackaddr$prim57489, align 8
%stackaddr$makeclosure57490 = alloca %struct.ScmObj*, align 8
%fptrToInt57491 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48943 to i64
%ae48943 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57491)
store volatile %struct.ScmObj* %ae48943, %struct.ScmObj** %stackaddr$makeclosure57490, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48943, %struct.ScmObj* %_37foldr148064, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48943, %struct.ScmObj* %_37map148060, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48943, %struct.ScmObj* %Ycmb48043, i64 2)
%argslist57321$Ycmb480430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57492 = alloca %struct.ScmObj*, align 8
%argslist57321$Ycmb480431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48219, %struct.ScmObj* %argslist57321$Ycmb480430)
store volatile %struct.ScmObj* %argslist57321$Ycmb480431, %struct.ScmObj** %stackaddr$prim57492, align 8
%stackaddr$prim57493 = alloca %struct.ScmObj*, align 8
%argslist57321$Ycmb480432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48943, %struct.ScmObj* %argslist57321$Ycmb480431)
store volatile %struct.ScmObj* %argslist57321$Ycmb480432, %struct.ScmObj** %stackaddr$prim57493, align 8
%clofunc57494 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48043)
musttail call tailcc void %clofunc57494(%struct.ScmObj* %Ycmb48043, %struct.ScmObj* %argslist57321$Ycmb480432)
ret void
}

define tailcc void @proc_clo$ae48943(%struct.ScmObj* %env$ae48943,%struct.ScmObj* %current_45args56899) {
%stackaddr$env-ref57495 = alloca %struct.ScmObj*, align 8
%_37foldr148064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48943, i64 0)
store %struct.ScmObj* %_37foldr148064, %struct.ScmObj** %stackaddr$env-ref57495
%stackaddr$env-ref57496 = alloca %struct.ScmObj*, align 8
%_37map148060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48943, i64 1)
store %struct.ScmObj* %_37map148060, %struct.ScmObj** %stackaddr$env-ref57496
%stackaddr$env-ref57497 = alloca %struct.ScmObj*, align 8
%Ycmb48043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48943, i64 2)
store %struct.ScmObj* %Ycmb48043, %struct.ScmObj** %stackaddr$env-ref57497
%stackaddr$prim57498 = alloca %struct.ScmObj*, align 8
%_95k48334 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56899)
store volatile %struct.ScmObj* %_95k48334, %struct.ScmObj** %stackaddr$prim57498, align 8
%stackaddr$prim57499 = alloca %struct.ScmObj*, align 8
%current_45args56900 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56899)
store volatile %struct.ScmObj* %current_45args56900, %struct.ScmObj** %stackaddr$prim57499, align 8
%stackaddr$prim57500 = alloca %struct.ScmObj*, align 8
%_37take48056 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56900)
store volatile %struct.ScmObj* %_37take48056, %struct.ScmObj** %stackaddr$prim57500, align 8
%stackaddr$makeclosure57501 = alloca %struct.ScmObj*, align 8
%fptrToInt57502 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48945 to i64
%ae48945 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57502)
store volatile %struct.ScmObj* %ae48945, %struct.ScmObj** %stackaddr$makeclosure57501, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48945, %struct.ScmObj* %_37take48056, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48945, %struct.ScmObj* %_37foldr148064, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48945, %struct.ScmObj* %_37map148060, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48945, %struct.ScmObj* %Ycmb48043, i64 3)
%ae48946 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57503 = alloca %struct.ScmObj*, align 8
%fptrToInt57504 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48947 to i64
%ae48947 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57504)
store volatile %struct.ScmObj* %ae48947, %struct.ScmObj** %stackaddr$makeclosure57503, align 8
%argslist57320$ae489450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57505 = alloca %struct.ScmObj*, align 8
%argslist57320$ae489451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48947, %struct.ScmObj* %argslist57320$ae489450)
store volatile %struct.ScmObj* %argslist57320$ae489451, %struct.ScmObj** %stackaddr$prim57505, align 8
%stackaddr$prim57506 = alloca %struct.ScmObj*, align 8
%argslist57320$ae489452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48946, %struct.ScmObj* %argslist57320$ae489451)
store volatile %struct.ScmObj* %argslist57320$ae489452, %struct.ScmObj** %stackaddr$prim57506, align 8
%clofunc57507 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48945)
musttail call tailcc void %clofunc57507(%struct.ScmObj* %ae48945, %struct.ScmObj* %argslist57320$ae489452)
ret void
}

define tailcc void @proc_clo$ae48945(%struct.ScmObj* %env$ae48945,%struct.ScmObj* %current_45args56902) {
%stackaddr$env-ref57508 = alloca %struct.ScmObj*, align 8
%_37take48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48945, i64 0)
store %struct.ScmObj* %_37take48056, %struct.ScmObj** %stackaddr$env-ref57508
%stackaddr$env-ref57509 = alloca %struct.ScmObj*, align 8
%_37foldr148064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48945, i64 1)
store %struct.ScmObj* %_37foldr148064, %struct.ScmObj** %stackaddr$env-ref57509
%stackaddr$env-ref57510 = alloca %struct.ScmObj*, align 8
%_37map148060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48945, i64 2)
store %struct.ScmObj* %_37map148060, %struct.ScmObj** %stackaddr$env-ref57510
%stackaddr$env-ref57511 = alloca %struct.ScmObj*, align 8
%Ycmb48043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48945, i64 3)
store %struct.ScmObj* %Ycmb48043, %struct.ScmObj** %stackaddr$env-ref57511
%stackaddr$prim57512 = alloca %struct.ScmObj*, align 8
%_95k48335 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56902)
store volatile %struct.ScmObj* %_95k48335, %struct.ScmObj** %stackaddr$prim57512, align 8
%stackaddr$prim57513 = alloca %struct.ScmObj*, align 8
%current_45args56903 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56902)
store volatile %struct.ScmObj* %current_45args56903, %struct.ScmObj** %stackaddr$prim57513, align 8
%stackaddr$prim57514 = alloca %struct.ScmObj*, align 8
%anf_45bind48223 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56903)
store volatile %struct.ScmObj* %anf_45bind48223, %struct.ScmObj** %stackaddr$prim57514, align 8
%stackaddr$makeclosure57515 = alloca %struct.ScmObj*, align 8
%fptrToInt57516 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49026 to i64
%ae49026 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57516)
store volatile %struct.ScmObj* %ae49026, %struct.ScmObj** %stackaddr$makeclosure57515, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49026, %struct.ScmObj* %_37take48056, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49026, %struct.ScmObj* %_37foldr148064, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49026, %struct.ScmObj* %_37map148060, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49026, %struct.ScmObj* %Ycmb48043, i64 3)
%argslist57306$Ycmb480430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57517 = alloca %struct.ScmObj*, align 8
%argslist57306$Ycmb480431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48223, %struct.ScmObj* %argslist57306$Ycmb480430)
store volatile %struct.ScmObj* %argslist57306$Ycmb480431, %struct.ScmObj** %stackaddr$prim57517, align 8
%stackaddr$prim57518 = alloca %struct.ScmObj*, align 8
%argslist57306$Ycmb480432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49026, %struct.ScmObj* %argslist57306$Ycmb480431)
store volatile %struct.ScmObj* %argslist57306$Ycmb480432, %struct.ScmObj** %stackaddr$prim57518, align 8
%clofunc57519 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48043)
musttail call tailcc void %clofunc57519(%struct.ScmObj* %Ycmb48043, %struct.ScmObj* %argslist57306$Ycmb480432)
ret void
}

define tailcc void @proc_clo$ae49026(%struct.ScmObj* %env$ae49026,%struct.ScmObj* %current_45args56905) {
%stackaddr$env-ref57520 = alloca %struct.ScmObj*, align 8
%_37take48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49026, i64 0)
store %struct.ScmObj* %_37take48056, %struct.ScmObj** %stackaddr$env-ref57520
%stackaddr$env-ref57521 = alloca %struct.ScmObj*, align 8
%_37foldr148064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49026, i64 1)
store %struct.ScmObj* %_37foldr148064, %struct.ScmObj** %stackaddr$env-ref57521
%stackaddr$env-ref57522 = alloca %struct.ScmObj*, align 8
%_37map148060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49026, i64 2)
store %struct.ScmObj* %_37map148060, %struct.ScmObj** %stackaddr$env-ref57522
%stackaddr$env-ref57523 = alloca %struct.ScmObj*, align 8
%Ycmb48043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49026, i64 3)
store %struct.ScmObj* %Ycmb48043, %struct.ScmObj** %stackaddr$env-ref57523
%stackaddr$prim57524 = alloca %struct.ScmObj*, align 8
%_95k48336 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56905)
store volatile %struct.ScmObj* %_95k48336, %struct.ScmObj** %stackaddr$prim57524, align 8
%stackaddr$prim57525 = alloca %struct.ScmObj*, align 8
%current_45args56906 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56905)
store volatile %struct.ScmObj* %current_45args56906, %struct.ScmObj** %stackaddr$prim57525, align 8
%stackaddr$prim57526 = alloca %struct.ScmObj*, align 8
%_37length48053 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56906)
store volatile %struct.ScmObj* %_37length48053, %struct.ScmObj** %stackaddr$prim57526, align 8
%stackaddr$makeclosure57527 = alloca %struct.ScmObj*, align 8
%fptrToInt57528 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49028 to i64
%ae49028 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt57528)
store volatile %struct.ScmObj* %ae49028, %struct.ScmObj** %stackaddr$makeclosure57527, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49028, %struct.ScmObj* %_37take48056, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49028, %struct.ScmObj* %_37length48053, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49028, %struct.ScmObj* %_37foldr148064, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49028, %struct.ScmObj* %_37map148060, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49028, %struct.ScmObj* %Ycmb48043, i64 4)
%ae49029 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57529 = alloca %struct.ScmObj*, align 8
%fptrToInt57530 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49030 to i64
%ae49030 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57530)
store volatile %struct.ScmObj* %ae49030, %struct.ScmObj** %stackaddr$makeclosure57529, align 8
%argslist57305$ae490280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57531 = alloca %struct.ScmObj*, align 8
%argslist57305$ae490281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49030, %struct.ScmObj* %argslist57305$ae490280)
store volatile %struct.ScmObj* %argslist57305$ae490281, %struct.ScmObj** %stackaddr$prim57531, align 8
%stackaddr$prim57532 = alloca %struct.ScmObj*, align 8
%argslist57305$ae490282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49029, %struct.ScmObj* %argslist57305$ae490281)
store volatile %struct.ScmObj* %argslist57305$ae490282, %struct.ScmObj** %stackaddr$prim57532, align 8
%clofunc57533 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49028)
musttail call tailcc void %clofunc57533(%struct.ScmObj* %ae49028, %struct.ScmObj* %argslist57305$ae490282)
ret void
}

define tailcc void @proc_clo$ae49028(%struct.ScmObj* %env$ae49028,%struct.ScmObj* %current_45args56908) {
%stackaddr$env-ref57534 = alloca %struct.ScmObj*, align 8
%_37take48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49028, i64 0)
store %struct.ScmObj* %_37take48056, %struct.ScmObj** %stackaddr$env-ref57534
%stackaddr$env-ref57535 = alloca %struct.ScmObj*, align 8
%_37length48053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49028, i64 1)
store %struct.ScmObj* %_37length48053, %struct.ScmObj** %stackaddr$env-ref57535
%stackaddr$env-ref57536 = alloca %struct.ScmObj*, align 8
%_37foldr148064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49028, i64 2)
store %struct.ScmObj* %_37foldr148064, %struct.ScmObj** %stackaddr$env-ref57536
%stackaddr$env-ref57537 = alloca %struct.ScmObj*, align 8
%_37map148060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49028, i64 3)
store %struct.ScmObj* %_37map148060, %struct.ScmObj** %stackaddr$env-ref57537
%stackaddr$env-ref57538 = alloca %struct.ScmObj*, align 8
%Ycmb48043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49028, i64 4)
store %struct.ScmObj* %Ycmb48043, %struct.ScmObj** %stackaddr$env-ref57538
%stackaddr$prim57539 = alloca %struct.ScmObj*, align 8
%_95k48337 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56908)
store volatile %struct.ScmObj* %_95k48337, %struct.ScmObj** %stackaddr$prim57539, align 8
%stackaddr$prim57540 = alloca %struct.ScmObj*, align 8
%current_45args56909 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56908)
store volatile %struct.ScmObj* %current_45args56909, %struct.ScmObj** %stackaddr$prim57540, align 8
%stackaddr$prim57541 = alloca %struct.ScmObj*, align 8
%anf_45bind48228 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56909)
store volatile %struct.ScmObj* %anf_45bind48228, %struct.ScmObj** %stackaddr$prim57541, align 8
%stackaddr$makeclosure57542 = alloca %struct.ScmObj*, align 8
%fptrToInt57543 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49105 to i64
%ae49105 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt57543)
store volatile %struct.ScmObj* %ae49105, %struct.ScmObj** %stackaddr$makeclosure57542, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49105, %struct.ScmObj* %_37take48056, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49105, %struct.ScmObj* %_37length48053, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49105, %struct.ScmObj* %_37foldr148064, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49105, %struct.ScmObj* %_37map148060, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49105, %struct.ScmObj* %Ycmb48043, i64 4)
%argslist57289$Ycmb480430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57544 = alloca %struct.ScmObj*, align 8
%argslist57289$Ycmb480431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48228, %struct.ScmObj* %argslist57289$Ycmb480430)
store volatile %struct.ScmObj* %argslist57289$Ycmb480431, %struct.ScmObj** %stackaddr$prim57544, align 8
%stackaddr$prim57545 = alloca %struct.ScmObj*, align 8
%argslist57289$Ycmb480432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49105, %struct.ScmObj* %argslist57289$Ycmb480431)
store volatile %struct.ScmObj* %argslist57289$Ycmb480432, %struct.ScmObj** %stackaddr$prim57545, align 8
%clofunc57546 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48043)
musttail call tailcc void %clofunc57546(%struct.ScmObj* %Ycmb48043, %struct.ScmObj* %argslist57289$Ycmb480432)
ret void
}

define tailcc void @proc_clo$ae49105(%struct.ScmObj* %env$ae49105,%struct.ScmObj* %current_45args56911) {
%stackaddr$env-ref57547 = alloca %struct.ScmObj*, align 8
%_37take48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49105, i64 0)
store %struct.ScmObj* %_37take48056, %struct.ScmObj** %stackaddr$env-ref57547
%stackaddr$env-ref57548 = alloca %struct.ScmObj*, align 8
%_37length48053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49105, i64 1)
store %struct.ScmObj* %_37length48053, %struct.ScmObj** %stackaddr$env-ref57548
%stackaddr$env-ref57549 = alloca %struct.ScmObj*, align 8
%_37foldr148064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49105, i64 2)
store %struct.ScmObj* %_37foldr148064, %struct.ScmObj** %stackaddr$env-ref57549
%stackaddr$env-ref57550 = alloca %struct.ScmObj*, align 8
%_37map148060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49105, i64 3)
store %struct.ScmObj* %_37map148060, %struct.ScmObj** %stackaddr$env-ref57550
%stackaddr$env-ref57551 = alloca %struct.ScmObj*, align 8
%Ycmb48043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49105, i64 4)
store %struct.ScmObj* %Ycmb48043, %struct.ScmObj** %stackaddr$env-ref57551
%stackaddr$prim57552 = alloca %struct.ScmObj*, align 8
%_95k48338 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56911)
store volatile %struct.ScmObj* %_95k48338, %struct.ScmObj** %stackaddr$prim57552, align 8
%stackaddr$prim57553 = alloca %struct.ScmObj*, align 8
%current_45args56912 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56911)
store volatile %struct.ScmObj* %current_45args56912, %struct.ScmObj** %stackaddr$prim57553, align 8
%stackaddr$prim57554 = alloca %struct.ScmObj*, align 8
%_37foldl148048 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56912)
store volatile %struct.ScmObj* %_37foldl148048, %struct.ScmObj** %stackaddr$prim57554, align 8
%stackaddr$makeclosure57555 = alloca %struct.ScmObj*, align 8
%fptrToInt57556 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49107 to i64
%ae49107 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt57556)
store volatile %struct.ScmObj* %ae49107, %struct.ScmObj** %stackaddr$makeclosure57555, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49107, %struct.ScmObj* %_37foldr148064, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49107, %struct.ScmObj* %_37foldl148048, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49107, %struct.ScmObj* %_37take48056, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49107, %struct.ScmObj* %_37length48053, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49107, %struct.ScmObj* %_37map148060, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49107, %struct.ScmObj* %Ycmb48043, i64 5)
%ae49108 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57557 = alloca %struct.ScmObj*, align 8
%fptrToInt57558 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49109 to i64
%ae49109 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57558)
store volatile %struct.ScmObj* %ae49109, %struct.ScmObj** %stackaddr$makeclosure57557, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49109, %struct.ScmObj* %_37foldl148048, i64 0)
%argslist57288$ae491070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57559 = alloca %struct.ScmObj*, align 8
%argslist57288$ae491071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49109, %struct.ScmObj* %argslist57288$ae491070)
store volatile %struct.ScmObj* %argslist57288$ae491071, %struct.ScmObj** %stackaddr$prim57559, align 8
%stackaddr$prim57560 = alloca %struct.ScmObj*, align 8
%argslist57288$ae491072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49108, %struct.ScmObj* %argslist57288$ae491071)
store volatile %struct.ScmObj* %argslist57288$ae491072, %struct.ScmObj** %stackaddr$prim57560, align 8
%clofunc57561 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49107)
musttail call tailcc void %clofunc57561(%struct.ScmObj* %ae49107, %struct.ScmObj* %argslist57288$ae491072)
ret void
}

define tailcc void @proc_clo$ae49107(%struct.ScmObj* %env$ae49107,%struct.ScmObj* %current_45args56914) {
%stackaddr$env-ref57562 = alloca %struct.ScmObj*, align 8
%_37foldr148064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49107, i64 0)
store %struct.ScmObj* %_37foldr148064, %struct.ScmObj** %stackaddr$env-ref57562
%stackaddr$env-ref57563 = alloca %struct.ScmObj*, align 8
%_37foldl148048 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49107, i64 1)
store %struct.ScmObj* %_37foldl148048, %struct.ScmObj** %stackaddr$env-ref57563
%stackaddr$env-ref57564 = alloca %struct.ScmObj*, align 8
%_37take48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49107, i64 2)
store %struct.ScmObj* %_37take48056, %struct.ScmObj** %stackaddr$env-ref57564
%stackaddr$env-ref57565 = alloca %struct.ScmObj*, align 8
%_37length48053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49107, i64 3)
store %struct.ScmObj* %_37length48053, %struct.ScmObj** %stackaddr$env-ref57565
%stackaddr$env-ref57566 = alloca %struct.ScmObj*, align 8
%_37map148060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49107, i64 4)
store %struct.ScmObj* %_37map148060, %struct.ScmObj** %stackaddr$env-ref57566
%stackaddr$env-ref57567 = alloca %struct.ScmObj*, align 8
%Ycmb48043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49107, i64 5)
store %struct.ScmObj* %Ycmb48043, %struct.ScmObj** %stackaddr$env-ref57567
%stackaddr$prim57568 = alloca %struct.ScmObj*, align 8
%_95k48339 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56914)
store volatile %struct.ScmObj* %_95k48339, %struct.ScmObj** %stackaddr$prim57568, align 8
%stackaddr$prim57569 = alloca %struct.ScmObj*, align 8
%current_45args56915 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56914)
store volatile %struct.ScmObj* %current_45args56915, %struct.ScmObj** %stackaddr$prim57569, align 8
%stackaddr$prim57570 = alloca %struct.ScmObj*, align 8
%_37last48086 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56915)
store volatile %struct.ScmObj* %_37last48086, %struct.ScmObj** %stackaddr$prim57570, align 8
%stackaddr$makeclosure57571 = alloca %struct.ScmObj*, align 8
%fptrToInt57572 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49161 to i64
%ae49161 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt57572)
store volatile %struct.ScmObj* %ae49161, %struct.ScmObj** %stackaddr$makeclosure57571, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49161, %struct.ScmObj* %_37foldr148064, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49161, %struct.ScmObj* %_37foldl148048, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49161, %struct.ScmObj* %_37last48086, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49161, %struct.ScmObj* %_37map148060, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49161, %struct.ScmObj* %Ycmb48043, i64 4)
%ae49162 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57573 = alloca %struct.ScmObj*, align 8
%fptrToInt57574 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49163 to i64
%ae49163 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57574)
store volatile %struct.ScmObj* %ae49163, %struct.ScmObj** %stackaddr$makeclosure57573, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49163, %struct.ScmObj* %_37take48056, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49163, %struct.ScmObj* %_37length48053, i64 1)
%argslist57274$ae491610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57575 = alloca %struct.ScmObj*, align 8
%argslist57274$ae491611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49163, %struct.ScmObj* %argslist57274$ae491610)
store volatile %struct.ScmObj* %argslist57274$ae491611, %struct.ScmObj** %stackaddr$prim57575, align 8
%stackaddr$prim57576 = alloca %struct.ScmObj*, align 8
%argslist57274$ae491612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49162, %struct.ScmObj* %argslist57274$ae491611)
store volatile %struct.ScmObj* %argslist57274$ae491612, %struct.ScmObj** %stackaddr$prim57576, align 8
%clofunc57577 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49161)
musttail call tailcc void %clofunc57577(%struct.ScmObj* %ae49161, %struct.ScmObj* %argslist57274$ae491612)
ret void
}

define tailcc void @proc_clo$ae49161(%struct.ScmObj* %env$ae49161,%struct.ScmObj* %current_45args56917) {
%stackaddr$env-ref57578 = alloca %struct.ScmObj*, align 8
%_37foldr148064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49161, i64 0)
store %struct.ScmObj* %_37foldr148064, %struct.ScmObj** %stackaddr$env-ref57578
%stackaddr$env-ref57579 = alloca %struct.ScmObj*, align 8
%_37foldl148048 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49161, i64 1)
store %struct.ScmObj* %_37foldl148048, %struct.ScmObj** %stackaddr$env-ref57579
%stackaddr$env-ref57580 = alloca %struct.ScmObj*, align 8
%_37last48086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49161, i64 2)
store %struct.ScmObj* %_37last48086, %struct.ScmObj** %stackaddr$env-ref57580
%stackaddr$env-ref57581 = alloca %struct.ScmObj*, align 8
%_37map148060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49161, i64 3)
store %struct.ScmObj* %_37map148060, %struct.ScmObj** %stackaddr$env-ref57581
%stackaddr$env-ref57582 = alloca %struct.ScmObj*, align 8
%Ycmb48043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49161, i64 4)
store %struct.ScmObj* %Ycmb48043, %struct.ScmObj** %stackaddr$env-ref57582
%stackaddr$prim57583 = alloca %struct.ScmObj*, align 8
%_95k48340 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56917)
store volatile %struct.ScmObj* %_95k48340, %struct.ScmObj** %stackaddr$prim57583, align 8
%stackaddr$prim57584 = alloca %struct.ScmObj*, align 8
%current_45args56918 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56917)
store volatile %struct.ScmObj* %current_45args56918, %struct.ScmObj** %stackaddr$prim57584, align 8
%stackaddr$prim57585 = alloca %struct.ScmObj*, align 8
%_37drop_45right48083 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56918)
store volatile %struct.ScmObj* %_37drop_45right48083, %struct.ScmObj** %stackaddr$prim57585, align 8
%stackaddr$makeclosure57586 = alloca %struct.ScmObj*, align 8
%fptrToInt57587 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49191 to i64
%ae49191 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt57587)
store volatile %struct.ScmObj* %ae49191, %struct.ScmObj** %stackaddr$makeclosure57586, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49191, %struct.ScmObj* %_37foldr148064, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49191, %struct.ScmObj* %_37foldl148048, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49191, %struct.ScmObj* %_37last48086, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49191, %struct.ScmObj* %_37drop_45right48083, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49191, %struct.ScmObj* %Ycmb48043, i64 4)
%ae49192 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57588 = alloca %struct.ScmObj*, align 8
%fptrToInt57589 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49193 to i64
%ae49193 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57589)
store volatile %struct.ScmObj* %ae49193, %struct.ScmObj** %stackaddr$makeclosure57588, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49193, %struct.ScmObj* %_37foldr148064, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49193, %struct.ScmObj* %_37map148060, i64 1)
%argslist57264$ae491910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57590 = alloca %struct.ScmObj*, align 8
%argslist57264$ae491911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49193, %struct.ScmObj* %argslist57264$ae491910)
store volatile %struct.ScmObj* %argslist57264$ae491911, %struct.ScmObj** %stackaddr$prim57590, align 8
%stackaddr$prim57591 = alloca %struct.ScmObj*, align 8
%argslist57264$ae491912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49192, %struct.ScmObj* %argslist57264$ae491911)
store volatile %struct.ScmObj* %argslist57264$ae491912, %struct.ScmObj** %stackaddr$prim57591, align 8
%clofunc57592 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49191)
musttail call tailcc void %clofunc57592(%struct.ScmObj* %ae49191, %struct.ScmObj* %argslist57264$ae491912)
ret void
}

define tailcc void @proc_clo$ae49191(%struct.ScmObj* %env$ae49191,%struct.ScmObj* %current_45args56920) {
%stackaddr$env-ref57593 = alloca %struct.ScmObj*, align 8
%_37foldr148064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49191, i64 0)
store %struct.ScmObj* %_37foldr148064, %struct.ScmObj** %stackaddr$env-ref57593
%stackaddr$env-ref57594 = alloca %struct.ScmObj*, align 8
%_37foldl148048 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49191, i64 1)
store %struct.ScmObj* %_37foldl148048, %struct.ScmObj** %stackaddr$env-ref57594
%stackaddr$env-ref57595 = alloca %struct.ScmObj*, align 8
%_37last48086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49191, i64 2)
store %struct.ScmObj* %_37last48086, %struct.ScmObj** %stackaddr$env-ref57595
%stackaddr$env-ref57596 = alloca %struct.ScmObj*, align 8
%_37drop_45right48083 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49191, i64 3)
store %struct.ScmObj* %_37drop_45right48083, %struct.ScmObj** %stackaddr$env-ref57596
%stackaddr$env-ref57597 = alloca %struct.ScmObj*, align 8
%Ycmb48043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49191, i64 4)
store %struct.ScmObj* %Ycmb48043, %struct.ScmObj** %stackaddr$env-ref57597
%stackaddr$prim57598 = alloca %struct.ScmObj*, align 8
%_95k48341 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56920)
store volatile %struct.ScmObj* %_95k48341, %struct.ScmObj** %stackaddr$prim57598, align 8
%stackaddr$prim57599 = alloca %struct.ScmObj*, align 8
%current_45args56921 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56920)
store volatile %struct.ScmObj* %current_45args56921, %struct.ScmObj** %stackaddr$prim57599, align 8
%stackaddr$prim57600 = alloca %struct.ScmObj*, align 8
%anf_45bind48244 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56921)
store volatile %struct.ScmObj* %anf_45bind48244, %struct.ScmObj** %stackaddr$prim57600, align 8
%stackaddr$makeclosure57601 = alloca %struct.ScmObj*, align 8
%fptrToInt57602 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49575 to i64
%ae49575 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt57602)
store volatile %struct.ScmObj* %ae49575, %struct.ScmObj** %stackaddr$makeclosure57601, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49575, %struct.ScmObj* %_37foldr148064, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49575, %struct.ScmObj* %_37foldl148048, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49575, %struct.ScmObj* %_37last48086, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49575, %struct.ScmObj* %_37drop_45right48083, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49575, %struct.ScmObj* %Ycmb48043, i64 4)
%argslist57204$Ycmb480430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57603 = alloca %struct.ScmObj*, align 8
%argslist57204$Ycmb480431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48244, %struct.ScmObj* %argslist57204$Ycmb480430)
store volatile %struct.ScmObj* %argslist57204$Ycmb480431, %struct.ScmObj** %stackaddr$prim57603, align 8
%stackaddr$prim57604 = alloca %struct.ScmObj*, align 8
%argslist57204$Ycmb480432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49575, %struct.ScmObj* %argslist57204$Ycmb480431)
store volatile %struct.ScmObj* %argslist57204$Ycmb480432, %struct.ScmObj** %stackaddr$prim57604, align 8
%clofunc57605 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48043)
musttail call tailcc void %clofunc57605(%struct.ScmObj* %Ycmb48043, %struct.ScmObj* %argslist57204$Ycmb480432)
ret void
}

define tailcc void @proc_clo$ae49575(%struct.ScmObj* %env$ae49575,%struct.ScmObj* %current_45args56923) {
%stackaddr$env-ref57606 = alloca %struct.ScmObj*, align 8
%_37foldr148064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49575, i64 0)
store %struct.ScmObj* %_37foldr148064, %struct.ScmObj** %stackaddr$env-ref57606
%stackaddr$env-ref57607 = alloca %struct.ScmObj*, align 8
%_37foldl148048 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49575, i64 1)
store %struct.ScmObj* %_37foldl148048, %struct.ScmObj** %stackaddr$env-ref57607
%stackaddr$env-ref57608 = alloca %struct.ScmObj*, align 8
%_37last48086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49575, i64 2)
store %struct.ScmObj* %_37last48086, %struct.ScmObj** %stackaddr$env-ref57608
%stackaddr$env-ref57609 = alloca %struct.ScmObj*, align 8
%_37drop_45right48083 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49575, i64 3)
store %struct.ScmObj* %_37drop_45right48083, %struct.ScmObj** %stackaddr$env-ref57609
%stackaddr$env-ref57610 = alloca %struct.ScmObj*, align 8
%Ycmb48043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49575, i64 4)
store %struct.ScmObj* %Ycmb48043, %struct.ScmObj** %stackaddr$env-ref57610
%stackaddr$prim57611 = alloca %struct.ScmObj*, align 8
%_95k48342 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56923)
store volatile %struct.ScmObj* %_95k48342, %struct.ScmObj** %stackaddr$prim57611, align 8
%stackaddr$prim57612 = alloca %struct.ScmObj*, align 8
%current_45args56924 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56923)
store volatile %struct.ScmObj* %current_45args56924, %struct.ScmObj** %stackaddr$prim57612, align 8
%stackaddr$prim57613 = alloca %struct.ScmObj*, align 8
%_37foldr48069 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56924)
store volatile %struct.ScmObj* %_37foldr48069, %struct.ScmObj** %stackaddr$prim57613, align 8
%stackaddr$makeclosure57614 = alloca %struct.ScmObj*, align 8
%fptrToInt57615 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49577 to i64
%ae49577 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt57615)
store volatile %struct.ScmObj* %ae49577, %struct.ScmObj** %stackaddr$makeclosure57614, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49577, %struct.ScmObj* %_37foldr148064, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49577, %struct.ScmObj* %_37foldl148048, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49577, %struct.ScmObj* %_37last48086, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49577, %struct.ScmObj* %_37foldr48069, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49577, %struct.ScmObj* %_37drop_45right48083, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49577, %struct.ScmObj* %Ycmb48043, i64 5)
%ae49578 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57616 = alloca %struct.ScmObj*, align 8
%fptrToInt57617 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49579 to i64
%ae49579 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57617)
store volatile %struct.ScmObj* %ae49579, %struct.ScmObj** %stackaddr$makeclosure57616, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49579, %struct.ScmObj* %_37foldr148064, i64 0)
%argslist57203$ae495770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57618 = alloca %struct.ScmObj*, align 8
%argslist57203$ae495771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49579, %struct.ScmObj* %argslist57203$ae495770)
store volatile %struct.ScmObj* %argslist57203$ae495771, %struct.ScmObj** %stackaddr$prim57618, align 8
%stackaddr$prim57619 = alloca %struct.ScmObj*, align 8
%argslist57203$ae495772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49578, %struct.ScmObj* %argslist57203$ae495771)
store volatile %struct.ScmObj* %argslist57203$ae495772, %struct.ScmObj** %stackaddr$prim57619, align 8
%clofunc57620 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49577)
musttail call tailcc void %clofunc57620(%struct.ScmObj* %ae49577, %struct.ScmObj* %argslist57203$ae495772)
ret void
}

define tailcc void @proc_clo$ae49577(%struct.ScmObj* %env$ae49577,%struct.ScmObj* %current_45args56926) {
%stackaddr$env-ref57621 = alloca %struct.ScmObj*, align 8
%_37foldr148064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49577, i64 0)
store %struct.ScmObj* %_37foldr148064, %struct.ScmObj** %stackaddr$env-ref57621
%stackaddr$env-ref57622 = alloca %struct.ScmObj*, align 8
%_37foldl148048 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49577, i64 1)
store %struct.ScmObj* %_37foldl148048, %struct.ScmObj** %stackaddr$env-ref57622
%stackaddr$env-ref57623 = alloca %struct.ScmObj*, align 8
%_37last48086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49577, i64 2)
store %struct.ScmObj* %_37last48086, %struct.ScmObj** %stackaddr$env-ref57623
%stackaddr$env-ref57624 = alloca %struct.ScmObj*, align 8
%_37foldr48069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49577, i64 3)
store %struct.ScmObj* %_37foldr48069, %struct.ScmObj** %stackaddr$env-ref57624
%stackaddr$env-ref57625 = alloca %struct.ScmObj*, align 8
%_37drop_45right48083 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49577, i64 4)
store %struct.ScmObj* %_37drop_45right48083, %struct.ScmObj** %stackaddr$env-ref57625
%stackaddr$env-ref57626 = alloca %struct.ScmObj*, align 8
%Ycmb48043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49577, i64 5)
store %struct.ScmObj* %Ycmb48043, %struct.ScmObj** %stackaddr$env-ref57626
%stackaddr$prim57627 = alloca %struct.ScmObj*, align 8
%_95k48343 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56926)
store volatile %struct.ScmObj* %_95k48343, %struct.ScmObj** %stackaddr$prim57627, align 8
%stackaddr$prim57628 = alloca %struct.ScmObj*, align 8
%current_45args56927 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56926)
store volatile %struct.ScmObj* %current_45args56927, %struct.ScmObj** %stackaddr$prim57628, align 8
%stackaddr$prim57629 = alloca %struct.ScmObj*, align 8
%_37map148095 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56927)
store volatile %struct.ScmObj* %_37map148095, %struct.ScmObj** %stackaddr$prim57629, align 8
%stackaddr$makeclosure57630 = alloca %struct.ScmObj*, align 8
%fptrToInt57631 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49654 to i64
%ae49654 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt57631)
store volatile %struct.ScmObj* %ae49654, %struct.ScmObj** %stackaddr$makeclosure57630, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49654, %struct.ScmObj* %_37foldr148064, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49654, %struct.ScmObj* %_37foldl148048, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49654, %struct.ScmObj* %_37foldr48069, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49654, %struct.ScmObj* %_37map148095, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49654, %struct.ScmObj* %Ycmb48043, i64 4)
%ae49655 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57632 = alloca %struct.ScmObj*, align 8
%fptrToInt57633 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49656 to i64
%ae49656 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57633)
store volatile %struct.ScmObj* %ae49656, %struct.ScmObj** %stackaddr$makeclosure57632, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49656, %struct.ScmObj* %_37last48086, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49656, %struct.ScmObj* %_37foldr48069, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49656, %struct.ScmObj* %_37drop_45right48083, i64 2)
%argslist57184$ae496540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57634 = alloca %struct.ScmObj*, align 8
%argslist57184$ae496541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49656, %struct.ScmObj* %argslist57184$ae496540)
store volatile %struct.ScmObj* %argslist57184$ae496541, %struct.ScmObj** %stackaddr$prim57634, align 8
%stackaddr$prim57635 = alloca %struct.ScmObj*, align 8
%argslist57184$ae496542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49655, %struct.ScmObj* %argslist57184$ae496541)
store volatile %struct.ScmObj* %argslist57184$ae496542, %struct.ScmObj** %stackaddr$prim57635, align 8
%clofunc57636 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49654)
musttail call tailcc void %clofunc57636(%struct.ScmObj* %ae49654, %struct.ScmObj* %argslist57184$ae496542)
ret void
}

define tailcc void @proc_clo$ae49654(%struct.ScmObj* %env$ae49654,%struct.ScmObj* %current_45args56929) {
%stackaddr$env-ref57637 = alloca %struct.ScmObj*, align 8
%_37foldr148064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49654, i64 0)
store %struct.ScmObj* %_37foldr148064, %struct.ScmObj** %stackaddr$env-ref57637
%stackaddr$env-ref57638 = alloca %struct.ScmObj*, align 8
%_37foldl148048 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49654, i64 1)
store %struct.ScmObj* %_37foldl148048, %struct.ScmObj** %stackaddr$env-ref57638
%stackaddr$env-ref57639 = alloca %struct.ScmObj*, align 8
%_37foldr48069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49654, i64 2)
store %struct.ScmObj* %_37foldr48069, %struct.ScmObj** %stackaddr$env-ref57639
%stackaddr$env-ref57640 = alloca %struct.ScmObj*, align 8
%_37map148095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49654, i64 3)
store %struct.ScmObj* %_37map148095, %struct.ScmObj** %stackaddr$env-ref57640
%stackaddr$env-ref57641 = alloca %struct.ScmObj*, align 8
%Ycmb48043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49654, i64 4)
store %struct.ScmObj* %Ycmb48043, %struct.ScmObj** %stackaddr$env-ref57641
%stackaddr$prim57642 = alloca %struct.ScmObj*, align 8
%_95k48344 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56929)
store volatile %struct.ScmObj* %_95k48344, %struct.ScmObj** %stackaddr$prim57642, align 8
%stackaddr$prim57643 = alloca %struct.ScmObj*, align 8
%current_45args56930 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56929)
store volatile %struct.ScmObj* %current_45args56930, %struct.ScmObj** %stackaddr$prim57643, align 8
%stackaddr$prim57644 = alloca %struct.ScmObj*, align 8
%_37map48090 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56930)
store volatile %struct.ScmObj* %_37map48090, %struct.ScmObj** %stackaddr$prim57644, align 8
%stackaddr$makeclosure57645 = alloca %struct.ScmObj*, align 8
%fptrToInt57646 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49800 to i64
%ae49800 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57646)
store volatile %struct.ScmObj* %ae49800, %struct.ScmObj** %stackaddr$makeclosure57645, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49800, %struct.ScmObj* %_37foldl148048, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49800, %struct.ScmObj* %Ycmb48043, i64 1)
%ae49801 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57647 = alloca %struct.ScmObj*, align 8
%fptrToInt57648 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49802 to i64
%ae49802 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57648)
store volatile %struct.ScmObj* %ae49802, %struct.ScmObj** %stackaddr$makeclosure57647, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49802, %struct.ScmObj* %_37foldr48069, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49802, %struct.ScmObj* %_37foldr148064, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49802, %struct.ScmObj* %_37map148095, i64 2)
%argslist57167$ae498000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57649 = alloca %struct.ScmObj*, align 8
%argslist57167$ae498001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49802, %struct.ScmObj* %argslist57167$ae498000)
store volatile %struct.ScmObj* %argslist57167$ae498001, %struct.ScmObj** %stackaddr$prim57649, align 8
%stackaddr$prim57650 = alloca %struct.ScmObj*, align 8
%argslist57167$ae498002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49801, %struct.ScmObj* %argslist57167$ae498001)
store volatile %struct.ScmObj* %argslist57167$ae498002, %struct.ScmObj** %stackaddr$prim57650, align 8
%clofunc57651 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49800)
musttail call tailcc void %clofunc57651(%struct.ScmObj* %ae49800, %struct.ScmObj* %argslist57167$ae498002)
ret void
}

define tailcc void @proc_clo$ae49800(%struct.ScmObj* %env$ae49800,%struct.ScmObj* %current_45args56932) {
%stackaddr$env-ref57652 = alloca %struct.ScmObj*, align 8
%_37foldl148048 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49800, i64 0)
store %struct.ScmObj* %_37foldl148048, %struct.ScmObj** %stackaddr$env-ref57652
%stackaddr$env-ref57653 = alloca %struct.ScmObj*, align 8
%Ycmb48043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49800, i64 1)
store %struct.ScmObj* %Ycmb48043, %struct.ScmObj** %stackaddr$env-ref57653
%stackaddr$prim57654 = alloca %struct.ScmObj*, align 8
%_95k48345 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56932)
store volatile %struct.ScmObj* %_95k48345, %struct.ScmObj** %stackaddr$prim57654, align 8
%stackaddr$prim57655 = alloca %struct.ScmObj*, align 8
%current_45args56933 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56932)
store volatile %struct.ScmObj* %current_45args56933, %struct.ScmObj** %stackaddr$prim57655, align 8
%stackaddr$prim57656 = alloca %struct.ScmObj*, align 8
%anf_45bind48264 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56933)
store volatile %struct.ScmObj* %anf_45bind48264, %struct.ScmObj** %stackaddr$prim57656, align 8
%stackaddr$makeclosure57657 = alloca %struct.ScmObj*, align 8
%fptrToInt57658 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50192 to i64
%ae50192 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57658)
store volatile %struct.ScmObj* %ae50192, %struct.ScmObj** %stackaddr$makeclosure57657, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50192, %struct.ScmObj* %_37foldl148048, i64 0)
%argslist57107$Ycmb480430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57659 = alloca %struct.ScmObj*, align 8
%argslist57107$Ycmb480431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48264, %struct.ScmObj* %argslist57107$Ycmb480430)
store volatile %struct.ScmObj* %argslist57107$Ycmb480431, %struct.ScmObj** %stackaddr$prim57659, align 8
%stackaddr$prim57660 = alloca %struct.ScmObj*, align 8
%argslist57107$Ycmb480432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50192, %struct.ScmObj* %argslist57107$Ycmb480431)
store volatile %struct.ScmObj* %argslist57107$Ycmb480432, %struct.ScmObj** %stackaddr$prim57660, align 8
%clofunc57661 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48043)
musttail call tailcc void %clofunc57661(%struct.ScmObj* %Ycmb48043, %struct.ScmObj* %argslist57107$Ycmb480432)
ret void
}

define tailcc void @proc_clo$ae50192(%struct.ScmObj* %env$ae50192,%struct.ScmObj* %current_45args56935) {
%stackaddr$env-ref57662 = alloca %struct.ScmObj*, align 8
%_37foldl148048 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50192, i64 0)
store %struct.ScmObj* %_37foldl148048, %struct.ScmObj** %stackaddr$env-ref57662
%stackaddr$prim57663 = alloca %struct.ScmObj*, align 8
%_95k48346 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56935)
store volatile %struct.ScmObj* %_95k48346, %struct.ScmObj** %stackaddr$prim57663, align 8
%stackaddr$prim57664 = alloca %struct.ScmObj*, align 8
%current_45args56936 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56935)
store volatile %struct.ScmObj* %current_45args56936, %struct.ScmObj** %stackaddr$prim57664, align 8
%stackaddr$prim57665 = alloca %struct.ScmObj*, align 8
%_37foldl48146 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56936)
store volatile %struct.ScmObj* %_37foldl48146, %struct.ScmObj** %stackaddr$prim57665, align 8
%stackaddr$makeclosure57666 = alloca %struct.ScmObj*, align 8
%fptrToInt57667 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50194 to i64
%ae50194 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57667)
store volatile %struct.ScmObj* %ae50194, %struct.ScmObj** %stackaddr$makeclosure57666, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50194, %struct.ScmObj* %_37foldl148048, i64 0)
%ae50195 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57668 = alloca %struct.ScmObj*, align 8
%fptrToInt57669 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50196 to i64
%ae50196 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57669)
store volatile %struct.ScmObj* %ae50196, %struct.ScmObj** %stackaddr$makeclosure57668, align 8
%argslist57106$ae501940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57670 = alloca %struct.ScmObj*, align 8
%argslist57106$ae501941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50196, %struct.ScmObj* %argslist57106$ae501940)
store volatile %struct.ScmObj* %argslist57106$ae501941, %struct.ScmObj** %stackaddr$prim57670, align 8
%stackaddr$prim57671 = alloca %struct.ScmObj*, align 8
%argslist57106$ae501942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50195, %struct.ScmObj* %argslist57106$ae501941)
store volatile %struct.ScmObj* %argslist57106$ae501942, %struct.ScmObj** %stackaddr$prim57671, align 8
%clofunc57672 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50194)
musttail call tailcc void %clofunc57672(%struct.ScmObj* %ae50194, %struct.ScmObj* %argslist57106$ae501942)
ret void
}

define tailcc void @proc_clo$ae50194(%struct.ScmObj* %env$ae50194,%struct.ScmObj* %current_45args56938) {
%stackaddr$env-ref57673 = alloca %struct.ScmObj*, align 8
%_37foldl148048 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50194, i64 0)
store %struct.ScmObj* %_37foldl148048, %struct.ScmObj** %stackaddr$env-ref57673
%stackaddr$prim57674 = alloca %struct.ScmObj*, align 8
%_95k48347 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56938)
store volatile %struct.ScmObj* %_95k48347, %struct.ScmObj** %stackaddr$prim57674, align 8
%stackaddr$prim57675 = alloca %struct.ScmObj*, align 8
%current_45args56939 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56938)
store volatile %struct.ScmObj* %current_45args56939, %struct.ScmObj** %stackaddr$prim57675, align 8
%stackaddr$prim57676 = alloca %struct.ScmObj*, align 8
%_37_6248143 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56939)
store volatile %struct.ScmObj* %_37_6248143, %struct.ScmObj** %stackaddr$prim57676, align 8
%stackaddr$makeclosure57677 = alloca %struct.ScmObj*, align 8
%fptrToInt57678 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50218 to i64
%ae50218 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57678)
store volatile %struct.ScmObj* %ae50218, %struct.ScmObj** %stackaddr$makeclosure57677, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50218, %struct.ScmObj* %_37foldl148048, i64 0)
%ae50219 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57679 = alloca %struct.ScmObj*, align 8
%fptrToInt57680 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50220 to i64
%ae50220 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57680)
store volatile %struct.ScmObj* %ae50220, %struct.ScmObj** %stackaddr$makeclosure57679, align 8
%argslist57100$ae502180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57681 = alloca %struct.ScmObj*, align 8
%argslist57100$ae502181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50220, %struct.ScmObj* %argslist57100$ae502180)
store volatile %struct.ScmObj* %argslist57100$ae502181, %struct.ScmObj** %stackaddr$prim57681, align 8
%stackaddr$prim57682 = alloca %struct.ScmObj*, align 8
%argslist57100$ae502182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50219, %struct.ScmObj* %argslist57100$ae502181)
store volatile %struct.ScmObj* %argslist57100$ae502182, %struct.ScmObj** %stackaddr$prim57682, align 8
%clofunc57683 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50218)
musttail call tailcc void %clofunc57683(%struct.ScmObj* %ae50218, %struct.ScmObj* %argslist57100$ae502182)
ret void
}

define tailcc void @proc_clo$ae50218(%struct.ScmObj* %env$ae50218,%struct.ScmObj* %current_45args56941) {
%stackaddr$env-ref57684 = alloca %struct.ScmObj*, align 8
%_37foldl148048 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50218, i64 0)
store %struct.ScmObj* %_37foldl148048, %struct.ScmObj** %stackaddr$env-ref57684
%stackaddr$prim57685 = alloca %struct.ScmObj*, align 8
%_95k48348 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56941)
store volatile %struct.ScmObj* %_95k48348, %struct.ScmObj** %stackaddr$prim57685, align 8
%stackaddr$prim57686 = alloca %struct.ScmObj*, align 8
%current_45args56942 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56941)
store volatile %struct.ScmObj* %current_45args56942, %struct.ScmObj** %stackaddr$prim57686, align 8
%stackaddr$prim57687 = alloca %struct.ScmObj*, align 8
%_37_62_6148140 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56942)
store volatile %struct.ScmObj* %_37_62_6148140, %struct.ScmObj** %stackaddr$prim57687, align 8
%ae50242 = call %struct.ScmObj* @const_init_int(i64 1)
%ae50243 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57688 = alloca %struct.ScmObj*, align 8
%_37append48136 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50242, %struct.ScmObj* %ae50243)
store volatile %struct.ScmObj* %_37append48136, %struct.ScmObj** %stackaddr$prim57688, align 8
%stackaddr$makeclosure57689 = alloca %struct.ScmObj*, align 8
%fptrToInt57690 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50244 to i64
%ae50244 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57690)
store volatile %struct.ScmObj* %ae50244, %struct.ScmObj** %stackaddr$makeclosure57689, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50244, %struct.ScmObj* %_37append48136, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50244, %struct.ScmObj* %_37foldl148048, i64 1)
%ae50245 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57691 = alloca %struct.ScmObj*, align 8
%fptrToInt57692 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50246 to i64
%ae50246 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57692)
store volatile %struct.ScmObj* %ae50246, %struct.ScmObj** %stackaddr$makeclosure57691, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50246, %struct.ScmObj* %_37append48136, i64 0)
%argslist57094$ae502440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57693 = alloca %struct.ScmObj*, align 8
%argslist57094$ae502441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50246, %struct.ScmObj* %argslist57094$ae502440)
store volatile %struct.ScmObj* %argslist57094$ae502441, %struct.ScmObj** %stackaddr$prim57693, align 8
%stackaddr$prim57694 = alloca %struct.ScmObj*, align 8
%argslist57094$ae502442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50245, %struct.ScmObj* %argslist57094$ae502441)
store volatile %struct.ScmObj* %argslist57094$ae502442, %struct.ScmObj** %stackaddr$prim57694, align 8
%clofunc57695 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50244)
musttail call tailcc void %clofunc57695(%struct.ScmObj* %ae50244, %struct.ScmObj* %argslist57094$ae502442)
ret void
}

define tailcc void @proc_clo$ae50244(%struct.ScmObj* %env$ae50244,%struct.ScmObj* %current_45args56944) {
%stackaddr$env-ref57696 = alloca %struct.ScmObj*, align 8
%_37append48136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50244, i64 0)
store %struct.ScmObj* %_37append48136, %struct.ScmObj** %stackaddr$env-ref57696
%stackaddr$env-ref57697 = alloca %struct.ScmObj*, align 8
%_37foldl148048 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50244, i64 1)
store %struct.ScmObj* %_37foldl148048, %struct.ScmObj** %stackaddr$env-ref57697
%stackaddr$prim57698 = alloca %struct.ScmObj*, align 8
%_95k48349 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56944)
store volatile %struct.ScmObj* %_95k48349, %struct.ScmObj** %stackaddr$prim57698, align 8
%stackaddr$prim57699 = alloca %struct.ScmObj*, align 8
%current_45args56945 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56944)
store volatile %struct.ScmObj* %current_45args56945, %struct.ScmObj** %stackaddr$prim57699, align 8
%stackaddr$prim57700 = alloca %struct.ScmObj*, align 8
%anf_45bind48272 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56945)
store volatile %struct.ScmObj* %anf_45bind48272, %struct.ScmObj** %stackaddr$prim57700, align 8
%ae50312 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57701 = alloca %struct.ScmObj*, align 8
%_95048137 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append48136, %struct.ScmObj* %ae50312, %struct.ScmObj* %anf_45bind48272)
store volatile %struct.ScmObj* %_95048137, %struct.ScmObj** %stackaddr$prim57701, align 8
%ae50315 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57702 = alloca %struct.ScmObj*, align 8
%_37append48135 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append48136, %struct.ScmObj* %ae50315)
store volatile %struct.ScmObj* %_37append48135, %struct.ScmObj** %stackaddr$prim57702, align 8
%stackaddr$makeclosure57703 = alloca %struct.ScmObj*, align 8
%fptrToInt57704 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50316 to i64
%ae50316 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57704)
store volatile %struct.ScmObj* %ae50316, %struct.ScmObj** %stackaddr$makeclosure57703, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50316, %struct.ScmObj* %_37foldl148048, i64 0)
%ae50317 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57705 = alloca %struct.ScmObj*, align 8
%fptrToInt57706 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50318 to i64
%ae50318 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57706)
store volatile %struct.ScmObj* %ae50318, %struct.ScmObj** %stackaddr$makeclosure57705, align 8
%argslist57083$ae503160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57707 = alloca %struct.ScmObj*, align 8
%argslist57083$ae503161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50318, %struct.ScmObj* %argslist57083$ae503160)
store volatile %struct.ScmObj* %argslist57083$ae503161, %struct.ScmObj** %stackaddr$prim57707, align 8
%stackaddr$prim57708 = alloca %struct.ScmObj*, align 8
%argslist57083$ae503162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50317, %struct.ScmObj* %argslist57083$ae503161)
store volatile %struct.ScmObj* %argslist57083$ae503162, %struct.ScmObj** %stackaddr$prim57708, align 8
%clofunc57709 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50316)
musttail call tailcc void %clofunc57709(%struct.ScmObj* %ae50316, %struct.ScmObj* %argslist57083$ae503162)
ret void
}

define tailcc void @proc_clo$ae50316(%struct.ScmObj* %env$ae50316,%struct.ScmObj* %current_45args56947) {
%stackaddr$env-ref57710 = alloca %struct.ScmObj*, align 8
%_37foldl148048 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50316, i64 0)
store %struct.ScmObj* %_37foldl148048, %struct.ScmObj** %stackaddr$env-ref57710
%stackaddr$prim57711 = alloca %struct.ScmObj*, align 8
%_95k48350 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56947)
store volatile %struct.ScmObj* %_95k48350, %struct.ScmObj** %stackaddr$prim57711, align 8
%stackaddr$prim57712 = alloca %struct.ScmObj*, align 8
%current_45args56948 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56947)
store volatile %struct.ScmObj* %current_45args56948, %struct.ScmObj** %stackaddr$prim57712, align 8
%stackaddr$prim57713 = alloca %struct.ScmObj*, align 8
%_37list_6348128 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56948)
store volatile %struct.ScmObj* %_37list_6348128, %struct.ScmObj** %stackaddr$prim57713, align 8
%stackaddr$makeclosure57714 = alloca %struct.ScmObj*, align 8
%fptrToInt57715 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50732 to i64
%ae50732 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57715)
store volatile %struct.ScmObj* %ae50732, %struct.ScmObj** %stackaddr$makeclosure57714, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50732, %struct.ScmObj* %_37foldl148048, i64 0)
%ae50733 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57716 = alloca %struct.ScmObj*, align 8
%fptrToInt57717 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50734 to i64
%ae50734 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57717)
store volatile %struct.ScmObj* %ae50734, %struct.ScmObj** %stackaddr$makeclosure57716, align 8
%argslist57058$ae507320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57718 = alloca %struct.ScmObj*, align 8
%argslist57058$ae507321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50734, %struct.ScmObj* %argslist57058$ae507320)
store volatile %struct.ScmObj* %argslist57058$ae507321, %struct.ScmObj** %stackaddr$prim57718, align 8
%stackaddr$prim57719 = alloca %struct.ScmObj*, align 8
%argslist57058$ae507322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50733, %struct.ScmObj* %argslist57058$ae507321)
store volatile %struct.ScmObj* %argslist57058$ae507322, %struct.ScmObj** %stackaddr$prim57719, align 8
%clofunc57720 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50732)
musttail call tailcc void %clofunc57720(%struct.ScmObj* %ae50732, %struct.ScmObj* %argslist57058$ae507322)
ret void
}

define tailcc void @proc_clo$ae50732(%struct.ScmObj* %env$ae50732,%struct.ScmObj* %current_45args56950) {
%stackaddr$env-ref57721 = alloca %struct.ScmObj*, align 8
%_37foldl148048 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50732, i64 0)
store %struct.ScmObj* %_37foldl148048, %struct.ScmObj** %stackaddr$env-ref57721
%stackaddr$prim57722 = alloca %struct.ScmObj*, align 8
%_95k48351 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56950)
store volatile %struct.ScmObj* %_95k48351, %struct.ScmObj** %stackaddr$prim57722, align 8
%stackaddr$prim57723 = alloca %struct.ScmObj*, align 8
%current_45args56951 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56950)
store volatile %struct.ScmObj* %current_45args56951, %struct.ScmObj** %stackaddr$prim57723, align 8
%stackaddr$prim57724 = alloca %struct.ScmObj*, align 8
%_37drop48119 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56951)
store volatile %struct.ScmObj* %_37drop48119, %struct.ScmObj** %stackaddr$prim57724, align 8
%stackaddr$makeclosure57725 = alloca %struct.ScmObj*, align 8
%fptrToInt57726 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51268 to i64
%ae51268 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57726)
store volatile %struct.ScmObj* %ae51268, %struct.ScmObj** %stackaddr$makeclosure57725, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51268, %struct.ScmObj* %_37foldl148048, i64 0)
%ae51269 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57727 = alloca %struct.ScmObj*, align 8
%fptrToInt57728 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51270 to i64
%ae51270 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57728)
store volatile %struct.ScmObj* %ae51270, %struct.ScmObj** %stackaddr$makeclosure57727, align 8
%argslist57034$ae512680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57729 = alloca %struct.ScmObj*, align 8
%argslist57034$ae512681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51270, %struct.ScmObj* %argslist57034$ae512680)
store volatile %struct.ScmObj* %argslist57034$ae512681, %struct.ScmObj** %stackaddr$prim57729, align 8
%stackaddr$prim57730 = alloca %struct.ScmObj*, align 8
%argslist57034$ae512682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51269, %struct.ScmObj* %argslist57034$ae512681)
store volatile %struct.ScmObj* %argslist57034$ae512682, %struct.ScmObj** %stackaddr$prim57730, align 8
%clofunc57731 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51268)
musttail call tailcc void %clofunc57731(%struct.ScmObj* %ae51268, %struct.ScmObj* %argslist57034$ae512682)
ret void
}

define tailcc void @proc_clo$ae51268(%struct.ScmObj* %env$ae51268,%struct.ScmObj* %current_45args56953) {
%stackaddr$env-ref57732 = alloca %struct.ScmObj*, align 8
%_37foldl148048 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51268, i64 0)
store %struct.ScmObj* %_37foldl148048, %struct.ScmObj** %stackaddr$env-ref57732
%stackaddr$prim57733 = alloca %struct.ScmObj*, align 8
%_95k48352 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56953)
store volatile %struct.ScmObj* %_95k48352, %struct.ScmObj** %stackaddr$prim57733, align 8
%stackaddr$prim57734 = alloca %struct.ScmObj*, align 8
%current_45args56954 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56953)
store volatile %struct.ScmObj* %current_45args56954, %struct.ScmObj** %stackaddr$prim57734, align 8
%stackaddr$prim57735 = alloca %struct.ScmObj*, align 8
%_37memv48112 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56954)
store volatile %struct.ScmObj* %_37memv48112, %struct.ScmObj** %stackaddr$prim57735, align 8
%stackaddr$makeclosure57736 = alloca %struct.ScmObj*, align 8
%fptrToInt57737 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51672 to i64
%ae51672 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57737)
store volatile %struct.ScmObj* %ae51672, %struct.ScmObj** %stackaddr$makeclosure57736, align 8
%ae51673 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57738 = alloca %struct.ScmObj*, align 8
%fptrToInt57739 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51674 to i64
%ae51674 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57739)
store volatile %struct.ScmObj* %ae51674, %struct.ScmObj** %stackaddr$makeclosure57738, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51674, %struct.ScmObj* %_37foldl148048, i64 0)
%argslist57008$ae516720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57740 = alloca %struct.ScmObj*, align 8
%argslist57008$ae516721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51674, %struct.ScmObj* %argslist57008$ae516720)
store volatile %struct.ScmObj* %argslist57008$ae516721, %struct.ScmObj** %stackaddr$prim57740, align 8
%stackaddr$prim57741 = alloca %struct.ScmObj*, align 8
%argslist57008$ae516722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51673, %struct.ScmObj* %argslist57008$ae516721)
store volatile %struct.ScmObj* %argslist57008$ae516722, %struct.ScmObj** %stackaddr$prim57741, align 8
%clofunc57742 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51672)
musttail call tailcc void %clofunc57742(%struct.ScmObj* %ae51672, %struct.ScmObj* %argslist57008$ae516722)
ret void
}

define tailcc void @proc_clo$ae51672(%struct.ScmObj* %env$ae51672,%struct.ScmObj* %current_45args56956) {
%stackaddr$prim57743 = alloca %struct.ScmObj*, align 8
%_95k48353 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56956)
store volatile %struct.ScmObj* %_95k48353, %struct.ScmObj** %stackaddr$prim57743, align 8
%stackaddr$prim57744 = alloca %struct.ScmObj*, align 8
%current_45args56957 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56956)
store volatile %struct.ScmObj* %current_45args56957, %struct.ScmObj** %stackaddr$prim57744, align 8
%stackaddr$prim57745 = alloca %struct.ScmObj*, align 8
%_37_4748108 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56957)
store volatile %struct.ScmObj* %_37_4748108, %struct.ScmObj** %stackaddr$prim57745, align 8
%stackaddr$makeclosure57746 = alloca %struct.ScmObj*, align 8
%fptrToInt57747 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51770 to i64
%ae51770 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57747)
store volatile %struct.ScmObj* %ae51770, %struct.ScmObj** %stackaddr$makeclosure57746, align 8
%ae51771 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57748 = alloca %struct.ScmObj*, align 8
%fptrToInt57749 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51772 to i64
%ae51772 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57749)
store volatile %struct.ScmObj* %ae51772, %struct.ScmObj** %stackaddr$makeclosure57748, align 8
%argslist56995$ae517700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57750 = alloca %struct.ScmObj*, align 8
%argslist56995$ae517701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51772, %struct.ScmObj* %argslist56995$ae517700)
store volatile %struct.ScmObj* %argslist56995$ae517701, %struct.ScmObj** %stackaddr$prim57750, align 8
%stackaddr$prim57751 = alloca %struct.ScmObj*, align 8
%argslist56995$ae517702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51771, %struct.ScmObj* %argslist56995$ae517701)
store volatile %struct.ScmObj* %argslist56995$ae517702, %struct.ScmObj** %stackaddr$prim57751, align 8
%clofunc57752 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51770)
musttail call tailcc void %clofunc57752(%struct.ScmObj* %ae51770, %struct.ScmObj* %argslist56995$ae517702)
ret void
}

define tailcc void @proc_clo$ae51770(%struct.ScmObj* %env$ae51770,%struct.ScmObj* %current_45args56959) {
%stackaddr$prim57753 = alloca %struct.ScmObj*, align 8
%_95k48354 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56959)
store volatile %struct.ScmObj* %_95k48354, %struct.ScmObj** %stackaddr$prim57753, align 8
%stackaddr$prim57754 = alloca %struct.ScmObj*, align 8
%current_45args56960 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56959)
store volatile %struct.ScmObj* %current_45args56960, %struct.ScmObj** %stackaddr$prim57754, align 8
%stackaddr$prim57755 = alloca %struct.ScmObj*, align 8
%_37first48106 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56960)
store volatile %struct.ScmObj* %_37first48106, %struct.ScmObj** %stackaddr$prim57755, align 8
%stackaddr$makeclosure57756 = alloca %struct.ScmObj*, align 8
%fptrToInt57757 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51790 to i64
%ae51790 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57757)
store volatile %struct.ScmObj* %ae51790, %struct.ScmObj** %stackaddr$makeclosure57756, align 8
%ae51791 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57758 = alloca %struct.ScmObj*, align 8
%fptrToInt57759 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51792 to i64
%ae51792 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57759)
store volatile %struct.ScmObj* %ae51792, %struct.ScmObj** %stackaddr$makeclosure57758, align 8
%argslist56990$ae517900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57760 = alloca %struct.ScmObj*, align 8
%argslist56990$ae517901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51792, %struct.ScmObj* %argslist56990$ae517900)
store volatile %struct.ScmObj* %argslist56990$ae517901, %struct.ScmObj** %stackaddr$prim57760, align 8
%stackaddr$prim57761 = alloca %struct.ScmObj*, align 8
%argslist56990$ae517902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51791, %struct.ScmObj* %argslist56990$ae517901)
store volatile %struct.ScmObj* %argslist56990$ae517902, %struct.ScmObj** %stackaddr$prim57761, align 8
%clofunc57762 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51790)
musttail call tailcc void %clofunc57762(%struct.ScmObj* %ae51790, %struct.ScmObj* %argslist56990$ae517902)
ret void
}

define tailcc void @proc_clo$ae51790(%struct.ScmObj* %env$ae51790,%struct.ScmObj* %current_45args56962) {
%stackaddr$prim57763 = alloca %struct.ScmObj*, align 8
%_95k48355 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56962)
store volatile %struct.ScmObj* %_95k48355, %struct.ScmObj** %stackaddr$prim57763, align 8
%stackaddr$prim57764 = alloca %struct.ScmObj*, align 8
%current_45args56963 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56962)
store volatile %struct.ScmObj* %current_45args56963, %struct.ScmObj** %stackaddr$prim57764, align 8
%stackaddr$prim57765 = alloca %struct.ScmObj*, align 8
%_37second48104 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56963)
store volatile %struct.ScmObj* %_37second48104, %struct.ScmObj** %stackaddr$prim57765, align 8
%stackaddr$makeclosure57766 = alloca %struct.ScmObj*, align 8
%fptrToInt57767 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51812 to i64
%ae51812 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57767)
store volatile %struct.ScmObj* %ae51812, %struct.ScmObj** %stackaddr$makeclosure57766, align 8
%ae51813 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57768 = alloca %struct.ScmObj*, align 8
%fptrToInt57769 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51814 to i64
%ae51814 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57769)
store volatile %struct.ScmObj* %ae51814, %struct.ScmObj** %stackaddr$makeclosure57768, align 8
%argslist56985$ae518120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57770 = alloca %struct.ScmObj*, align 8
%argslist56985$ae518121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51814, %struct.ScmObj* %argslist56985$ae518120)
store volatile %struct.ScmObj* %argslist56985$ae518121, %struct.ScmObj** %stackaddr$prim57770, align 8
%stackaddr$prim57771 = alloca %struct.ScmObj*, align 8
%argslist56985$ae518122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51813, %struct.ScmObj* %argslist56985$ae518121)
store volatile %struct.ScmObj* %argslist56985$ae518122, %struct.ScmObj** %stackaddr$prim57771, align 8
%clofunc57772 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51812)
musttail call tailcc void %clofunc57772(%struct.ScmObj* %ae51812, %struct.ScmObj* %argslist56985$ae518122)
ret void
}

define tailcc void @proc_clo$ae51812(%struct.ScmObj* %env$ae51812,%struct.ScmObj* %current_45args56965) {
%stackaddr$prim57773 = alloca %struct.ScmObj*, align 8
%_95k48356 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56965)
store volatile %struct.ScmObj* %_95k48356, %struct.ScmObj** %stackaddr$prim57773, align 8
%stackaddr$prim57774 = alloca %struct.ScmObj*, align 8
%current_45args56966 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56965)
store volatile %struct.ScmObj* %current_45args56966, %struct.ScmObj** %stackaddr$prim57774, align 8
%stackaddr$prim57775 = alloca %struct.ScmObj*, align 8
%_37third48102 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56966)
store volatile %struct.ScmObj* %_37third48102, %struct.ScmObj** %stackaddr$prim57775, align 8
%stackaddr$makeclosure57776 = alloca %struct.ScmObj*, align 8
%fptrToInt57777 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51836 to i64
%ae51836 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57777)
store volatile %struct.ScmObj* %ae51836, %struct.ScmObj** %stackaddr$makeclosure57776, align 8
%ae51837 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57778 = alloca %struct.ScmObj*, align 8
%fptrToInt57779 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51838 to i64
%ae51838 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57779)
store volatile %struct.ScmObj* %ae51838, %struct.ScmObj** %stackaddr$makeclosure57778, align 8
%argslist56980$ae518360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57780 = alloca %struct.ScmObj*, align 8
%argslist56980$ae518361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51838, %struct.ScmObj* %argslist56980$ae518360)
store volatile %struct.ScmObj* %argslist56980$ae518361, %struct.ScmObj** %stackaddr$prim57780, align 8
%stackaddr$prim57781 = alloca %struct.ScmObj*, align 8
%argslist56980$ae518362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51837, %struct.ScmObj* %argslist56980$ae518361)
store volatile %struct.ScmObj* %argslist56980$ae518362, %struct.ScmObj** %stackaddr$prim57781, align 8
%clofunc57782 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51836)
musttail call tailcc void %clofunc57782(%struct.ScmObj* %ae51836, %struct.ScmObj* %argslist56980$ae518362)
ret void
}

define tailcc void @proc_clo$ae51836(%struct.ScmObj* %env$ae51836,%struct.ScmObj* %current_45args56968) {
%stackaddr$prim57783 = alloca %struct.ScmObj*, align 8
%_95k48357 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56968)
store volatile %struct.ScmObj* %_95k48357, %struct.ScmObj** %stackaddr$prim57783, align 8
%stackaddr$prim57784 = alloca %struct.ScmObj*, align 8
%current_45args56969 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56968)
store volatile %struct.ScmObj* %current_45args56969, %struct.ScmObj** %stackaddr$prim57784, align 8
%stackaddr$prim57785 = alloca %struct.ScmObj*, align 8
%_37fourth48100 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56969)
store volatile %struct.ScmObj* %_37fourth48100, %struct.ScmObj** %stackaddr$prim57785, align 8
%stackaddr$prim57786 = alloca %struct.ScmObj*, align 8
%anf_45bind48308 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind48308, %struct.ScmObj** %stackaddr$prim57786, align 8
%ae51862 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57787 = alloca %struct.ScmObj*, align 8
%g4148178 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51862, %struct.ScmObj* %anf_45bind48308)
store volatile %struct.ScmObj* %g4148178, %struct.ScmObj** %stackaddr$prim57787, align 8
%stackaddr$prim57788 = alloca %struct.ScmObj*, align 8
%anf_45bind48309 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind48309, %struct.ScmObj** %stackaddr$prim57788, align 8
%ae51864 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57789 = alloca %struct.ScmObj*, align 8
%g4248177 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51864, %struct.ScmObj* %anf_45bind48309)
store volatile %struct.ScmObj* %g4248177, %struct.ScmObj** %stackaddr$prim57789, align 8
%stackaddr$prim57790 = alloca %struct.ScmObj*, align 8
%anf_45bind48310 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind48310, %struct.ScmObj** %stackaddr$prim57790, align 8
%ae51866 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57791 = alloca %struct.ScmObj*, align 8
%g4348176 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51866, %struct.ScmObj* %anf_45bind48310)
store volatile %struct.ScmObj* %g4348176, %struct.ScmObj** %stackaddr$prim57791, align 8
%stackaddr$prim57792 = alloca %struct.ScmObj*, align 8
%anf_45bind48311 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind48311, %struct.ScmObj** %stackaddr$prim57792, align 8
%ae51868 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57793 = alloca %struct.ScmObj*, align 8
%g4448175 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51868, %struct.ScmObj* %anf_45bind48311)
store volatile %struct.ScmObj* %g4448175, %struct.ScmObj** %stackaddr$prim57793, align 8
%stackaddr$prim57794 = alloca %struct.ScmObj*, align 8
%anf_45bind48312 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind48312, %struct.ScmObj** %stackaddr$prim57794, align 8
%ae51870 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57795 = alloca %struct.ScmObj*, align 8
%g4548174 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51870, %struct.ScmObj* %anf_45bind48312)
store volatile %struct.ScmObj* %g4548174, %struct.ScmObj** %stackaddr$prim57795, align 8
%stackaddr$prim57796 = alloca %struct.ScmObj*, align 8
%anf_45bind48313 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind48313, %struct.ScmObj** %stackaddr$prim57796, align 8
%ae51872 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57797 = alloca %struct.ScmObj*, align 8
%g4648173 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51872, %struct.ScmObj* %anf_45bind48313)
store volatile %struct.ScmObj* %g4648173, %struct.ScmObj** %stackaddr$prim57797, align 8
%stackaddr$prim57798 = alloca %struct.ScmObj*, align 8
%anf_45bind48314 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind48314, %struct.ScmObj** %stackaddr$prim57798, align 8
%ae51874 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57799 = alloca %struct.ScmObj*, align 8
%g4748172 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51874, %struct.ScmObj* %anf_45bind48314)
store volatile %struct.ScmObj* %g4748172, %struct.ScmObj** %stackaddr$prim57799, align 8
%stackaddr$prim57800 = alloca %struct.ScmObj*, align 8
%anf_45bind48315 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind48315, %struct.ScmObj** %stackaddr$prim57800, align 8
%ae51876 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57801 = alloca %struct.ScmObj*, align 8
%g4848171 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51876, %struct.ScmObj* %anf_45bind48315)
store volatile %struct.ScmObj* %g4848171, %struct.ScmObj** %stackaddr$prim57801, align 8
%stackaddr$prim57802 = alloca %struct.ScmObj*, align 8
%anf_45bind48316 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind48316, %struct.ScmObj** %stackaddr$prim57802, align 8
%ae51878 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57803 = alloca %struct.ScmObj*, align 8
%g4948170 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51878, %struct.ScmObj* %anf_45bind48316)
store volatile %struct.ScmObj* %g4948170, %struct.ScmObj** %stackaddr$prim57803, align 8
%stackaddr$prim57804 = alloca %struct.ScmObj*, align 8
%anf_45bind48317 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind48317, %struct.ScmObj** %stackaddr$prim57804, align 8
%ae51880 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57805 = alloca %struct.ScmObj*, align 8
%g5048169 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51880, %struct.ScmObj* %anf_45bind48317)
store volatile %struct.ScmObj* %g5048169, %struct.ScmObj** %stackaddr$prim57805, align 8
%stackaddr$prim57806 = alloca %struct.ScmObj*, align 8
%anf_45bind48318 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind48318, %struct.ScmObj** %stackaddr$prim57806, align 8
%ae51882 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57807 = alloca %struct.ScmObj*, align 8
%g5148168 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51882, %struct.ScmObj* %anf_45bind48318)
store volatile %struct.ScmObj* %g5148168, %struct.ScmObj** %stackaddr$prim57807, align 8
%stackaddr$prim57808 = alloca %struct.ScmObj*, align 8
%anf_45bind48319 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind48319, %struct.ScmObj** %stackaddr$prim57808, align 8
%ae51884 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57809 = alloca %struct.ScmObj*, align 8
%g5248167 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51884, %struct.ScmObj* %anf_45bind48319)
store volatile %struct.ScmObj* %g5248167, %struct.ScmObj** %stackaddr$prim57809, align 8
%stackaddr$prim57810 = alloca %struct.ScmObj*, align 8
%anf_45bind48320 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind48320, %struct.ScmObj** %stackaddr$prim57810, align 8
%ae51886 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57811 = alloca %struct.ScmObj*, align 8
%g5348166 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51886, %struct.ScmObj* %anf_45bind48320)
store volatile %struct.ScmObj* %g5348166, %struct.ScmObj** %stackaddr$prim57811, align 8
%stackaddr$prim57812 = alloca %struct.ScmObj*, align 8
%anf_45bind48321 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind48321, %struct.ScmObj** %stackaddr$prim57812, align 8
%ae51888 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57813 = alloca %struct.ScmObj*, align 8
%g5448165 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51888, %struct.ScmObj* %anf_45bind48321)
store volatile %struct.ScmObj* %g5448165, %struct.ScmObj** %stackaddr$prim57813, align 8
%stackaddr$prim57814 = alloca %struct.ScmObj*, align 8
%anf_45bind48322 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind48322, %struct.ScmObj** %stackaddr$prim57814, align 8
%ae51890 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57815 = alloca %struct.ScmObj*, align 8
%g5548164 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51890, %struct.ScmObj* %anf_45bind48322)
store volatile %struct.ScmObj* %g5548164, %struct.ScmObj** %stackaddr$prim57815, align 8
%stackaddr$prim57816 = alloca %struct.ScmObj*, align 8
%anf_45bind48323 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind48323, %struct.ScmObj** %stackaddr$prim57816, align 8
%ae51892 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57817 = alloca %struct.ScmObj*, align 8
%g5648163 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51892, %struct.ScmObj* %anf_45bind48323)
store volatile %struct.ScmObj* %g5648163, %struct.ScmObj** %stackaddr$prim57817, align 8
%stackaddr$prim57818 = alloca %struct.ScmObj*, align 8
%anf_45bind48324 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind48324, %struct.ScmObj** %stackaddr$prim57818, align 8
%ae51894 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57819 = alloca %struct.ScmObj*, align 8
%g5748162 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51894, %struct.ScmObj* %anf_45bind48324)
store volatile %struct.ScmObj* %g5748162, %struct.ScmObj** %stackaddr$prim57819, align 8
%stackaddr$prim57820 = alloca %struct.ScmObj*, align 8
%anf_45bind48325 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind48325, %struct.ScmObj** %stackaddr$prim57820, align 8
%ae51896 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57821 = alloca %struct.ScmObj*, align 8
%g5848161 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51896, %struct.ScmObj* %anf_45bind48325)
store volatile %struct.ScmObj* %g5848161, %struct.ScmObj** %stackaddr$prim57821, align 8
%ae51899 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51900 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae5190057822, i32 0, i32 0))
%stackaddr$prim57823 = alloca %struct.ScmObj*, align 8
%t4804248196 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g4148178, %struct.ScmObj* %ae51899, %struct.ScmObj* %ae51900)
store volatile %struct.ScmObj* %t4804248196, %struct.ScmObj** %stackaddr$prim57823, align 8
%ae51902 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51903 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae5190357824, i32 0, i32 0))
%stackaddr$prim57825 = alloca %struct.ScmObj*, align 8
%t4804148195 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g4248177, %struct.ScmObj* %ae51902, %struct.ScmObj* %ae51903)
store volatile %struct.ScmObj* %t4804148195, %struct.ScmObj** %stackaddr$prim57825, align 8
%ae51905 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51906 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae5190657826, i32 0, i32 0))
%stackaddr$prim57827 = alloca %struct.ScmObj*, align 8
%t4804048194 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g4348176, %struct.ScmObj* %ae51905, %struct.ScmObj* %ae51906)
store volatile %struct.ScmObj* %t4804048194, %struct.ScmObj** %stackaddr$prim57827, align 8
%ae51908 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51909 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae5190957828, i32 0, i32 0))
%stackaddr$prim57829 = alloca %struct.ScmObj*, align 8
%t4803948193 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g4448175, %struct.ScmObj* %ae51908, %struct.ScmObj* %ae51909)
store volatile %struct.ScmObj* %t4803948193, %struct.ScmObj** %stackaddr$prim57829, align 8
%ae51911 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51912 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae5191257830, i32 0, i32 0))
%stackaddr$prim57831 = alloca %struct.ScmObj*, align 8
%t4803848192 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g4548174, %struct.ScmObj* %ae51911, %struct.ScmObj* %ae51912)
store volatile %struct.ScmObj* %t4803848192, %struct.ScmObj** %stackaddr$prim57831, align 8
%ae51914 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51915 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae5191557832, i32 0, i32 0))
%stackaddr$prim57833 = alloca %struct.ScmObj*, align 8
%t4803748191 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g4648173, %struct.ScmObj* %ae51914, %struct.ScmObj* %ae51915)
store volatile %struct.ScmObj* %t4803748191, %struct.ScmObj** %stackaddr$prim57833, align 8
%ae51917 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51918 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae5191857834, i32 0, i32 0))
%stackaddr$prim57835 = alloca %struct.ScmObj*, align 8
%t4803648190 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g4748172, %struct.ScmObj* %ae51917, %struct.ScmObj* %ae51918)
store volatile %struct.ScmObj* %t4803648190, %struct.ScmObj** %stackaddr$prim57835, align 8
%ae51920 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51921 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae5192157836, i32 0, i32 0))
%stackaddr$prim57837 = alloca %struct.ScmObj*, align 8
%t4803548189 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g4848171, %struct.ScmObj* %ae51920, %struct.ScmObj* %ae51921)
store volatile %struct.ScmObj* %t4803548189, %struct.ScmObj** %stackaddr$prim57837, align 8
%ae51923 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51924 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae5192457838, i32 0, i32 0))
%stackaddr$prim57839 = alloca %struct.ScmObj*, align 8
%t4803448188 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g4948170, %struct.ScmObj* %ae51923, %struct.ScmObj* %ae51924)
store volatile %struct.ScmObj* %t4803448188, %struct.ScmObj** %stackaddr$prim57839, align 8
%ae51926 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51927 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae5192757840, i32 0, i32 0))
%stackaddr$prim57841 = alloca %struct.ScmObj*, align 8
%t4803348187 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g5048169, %struct.ScmObj* %ae51926, %struct.ScmObj* %ae51927)
store volatile %struct.ScmObj* %t4803348187, %struct.ScmObj** %stackaddr$prim57841, align 8
%ae51929 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51930 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae5193057842, i32 0, i32 0))
%stackaddr$prim57843 = alloca %struct.ScmObj*, align 8
%t4803248186 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g5148168, %struct.ScmObj* %ae51929, %struct.ScmObj* %ae51930)
store volatile %struct.ScmObj* %t4803248186, %struct.ScmObj** %stackaddr$prim57843, align 8
%ae51932 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51933 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae5193357844, i32 0, i32 0))
%stackaddr$prim57845 = alloca %struct.ScmObj*, align 8
%t4803148185 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g5248167, %struct.ScmObj* %ae51932, %struct.ScmObj* %ae51933)
store volatile %struct.ScmObj* %t4803148185, %struct.ScmObj** %stackaddr$prim57845, align 8
%ae51935 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51936 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae5193657846, i32 0, i32 0))
%stackaddr$prim57847 = alloca %struct.ScmObj*, align 8
%t4803048184 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g5348166, %struct.ScmObj* %ae51935, %struct.ScmObj* %ae51936)
store volatile %struct.ScmObj* %t4803048184, %struct.ScmObj** %stackaddr$prim57847, align 8
%ae51938 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51939 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae5193957848, i32 0, i32 0))
%stackaddr$prim57849 = alloca %struct.ScmObj*, align 8
%t4802948183 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g5448165, %struct.ScmObj* %ae51938, %struct.ScmObj* %ae51939)
store volatile %struct.ScmObj* %t4802948183, %struct.ScmObj** %stackaddr$prim57849, align 8
%ae51941 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51942 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae5194257850, i32 0, i32 0))
%stackaddr$prim57851 = alloca %struct.ScmObj*, align 8
%t4802848182 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g5548164, %struct.ScmObj* %ae51941, %struct.ScmObj* %ae51942)
store volatile %struct.ScmObj* %t4802848182, %struct.ScmObj** %stackaddr$prim57851, align 8
%ae51944 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51945 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae5194557852, i32 0, i32 0))
%stackaddr$prim57853 = alloca %struct.ScmObj*, align 8
%t4802748181 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g5648163, %struct.ScmObj* %ae51944, %struct.ScmObj* %ae51945)
store volatile %struct.ScmObj* %t4802748181, %struct.ScmObj** %stackaddr$prim57853, align 8
%ae51947 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51948 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae5194857854, i32 0, i32 0))
%stackaddr$prim57855 = alloca %struct.ScmObj*, align 8
%t4802648180 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g5748162, %struct.ScmObj* %ae51947, %struct.ScmObj* %ae51948)
store volatile %struct.ScmObj* %t4802648180, %struct.ScmObj** %stackaddr$prim57855, align 8
%ae51950 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51951 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @global$sym$ae5195157856, i32 0, i32 0))
%stackaddr$prim57857 = alloca %struct.ScmObj*, align 8
%t4802548179 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %g5848161, %struct.ScmObj* %ae51950, %struct.ScmObj* %ae51951)
store volatile %struct.ScmObj* %t4802548179, %struct.ScmObj** %stackaddr$prim57857, align 8
%stackaddr$makeclosure57858 = alloca %struct.ScmObj*, align 8
%fptrToInt57859 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51952 to i64
%ae51952 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57859)
store volatile %struct.ScmObj* %ae51952, %struct.ScmObj** %stackaddr$makeclosure57858, align 8
%ae51953 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51954 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @global$sym$ae5195457860, i32 0, i32 0))
%argslist56975$ae519520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57861 = alloca %struct.ScmObj*, align 8
%argslist56975$ae519521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51954, %struct.ScmObj* %argslist56975$ae519520)
store volatile %struct.ScmObj* %argslist56975$ae519521, %struct.ScmObj** %stackaddr$prim57861, align 8
%stackaddr$prim57862 = alloca %struct.ScmObj*, align 8
%argslist56975$ae519522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51953, %struct.ScmObj* %argslist56975$ae519521)
store volatile %struct.ScmObj* %argslist56975$ae519522, %struct.ScmObj** %stackaddr$prim57862, align 8
%clofunc57863 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51952)
musttail call tailcc void %clofunc57863(%struct.ScmObj* %ae51952, %struct.ScmObj* %argslist56975$ae519522)
ret void
}

define tailcc void @proc_clo$ae51952(%struct.ScmObj* %env$ae51952,%struct.ScmObj* %current_45args56971) {
%stackaddr$prim57864 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56971)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim57864, align 8
%stackaddr$prim57865 = alloca %struct.ScmObj*, align 8
%current_45args56972 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56971)
store volatile %struct.ScmObj* %current_45args56972, %struct.ScmObj** %stackaddr$prim57865, align 8
%stackaddr$prim57866 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56972)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim57866, align 8
%stackaddr$prim57867 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim57867, align 8
%argslist56974$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57868 = alloca %struct.ScmObj*, align 8
%argslist56974$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist56974$k0)
store volatile %struct.ScmObj* %argslist56974$k1, %struct.ScmObj** %stackaddr$prim57868, align 8
%clofunc57869 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc57869(%struct.ScmObj* %k, %struct.ScmObj* %argslist56974$k1)
ret void
}

define tailcc void @proc_clo$ae51838(%struct.ScmObj* %env$ae51838,%struct.ScmObj* %current_45args56976) {
%stackaddr$prim57870 = alloca %struct.ScmObj*, align 8
%k48358 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56976)
store volatile %struct.ScmObj* %k48358, %struct.ScmObj** %stackaddr$prim57870, align 8
%stackaddr$prim57871 = alloca %struct.ScmObj*, align 8
%current_45args56977 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56976)
store volatile %struct.ScmObj* %current_45args56977, %struct.ScmObj** %stackaddr$prim57871, align 8
%stackaddr$prim57872 = alloca %struct.ScmObj*, align 8
%x48101 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56977)
store volatile %struct.ScmObj* %x48101, %struct.ScmObj** %stackaddr$prim57872, align 8
%stackaddr$prim57873 = alloca %struct.ScmObj*, align 8
%anf_45bind48305 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48101)
store volatile %struct.ScmObj* %anf_45bind48305, %struct.ScmObj** %stackaddr$prim57873, align 8
%stackaddr$prim57874 = alloca %struct.ScmObj*, align 8
%anf_45bind48306 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48305)
store volatile %struct.ScmObj* %anf_45bind48306, %struct.ScmObj** %stackaddr$prim57874, align 8
%stackaddr$prim57875 = alloca %struct.ScmObj*, align 8
%anf_45bind48307 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48306)
store volatile %struct.ScmObj* %anf_45bind48307, %struct.ScmObj** %stackaddr$prim57875, align 8
%stackaddr$prim57876 = alloca %struct.ScmObj*, align 8
%cpsprim48359 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48307)
store volatile %struct.ScmObj* %cpsprim48359, %struct.ScmObj** %stackaddr$prim57876, align 8
%ae51844 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56979$k483580 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57877 = alloca %struct.ScmObj*, align 8
%argslist56979$k483581 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48359, %struct.ScmObj* %argslist56979$k483580)
store volatile %struct.ScmObj* %argslist56979$k483581, %struct.ScmObj** %stackaddr$prim57877, align 8
%stackaddr$prim57878 = alloca %struct.ScmObj*, align 8
%argslist56979$k483582 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51844, %struct.ScmObj* %argslist56979$k483581)
store volatile %struct.ScmObj* %argslist56979$k483582, %struct.ScmObj** %stackaddr$prim57878, align 8
%clofunc57879 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48358)
musttail call tailcc void %clofunc57879(%struct.ScmObj* %k48358, %struct.ScmObj* %argslist56979$k483582)
ret void
}

define tailcc void @proc_clo$ae51814(%struct.ScmObj* %env$ae51814,%struct.ScmObj* %current_45args56981) {
%stackaddr$prim57880 = alloca %struct.ScmObj*, align 8
%k48360 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56981)
store volatile %struct.ScmObj* %k48360, %struct.ScmObj** %stackaddr$prim57880, align 8
%stackaddr$prim57881 = alloca %struct.ScmObj*, align 8
%current_45args56982 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56981)
store volatile %struct.ScmObj* %current_45args56982, %struct.ScmObj** %stackaddr$prim57881, align 8
%stackaddr$prim57882 = alloca %struct.ScmObj*, align 8
%x48103 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56982)
store volatile %struct.ScmObj* %x48103, %struct.ScmObj** %stackaddr$prim57882, align 8
%stackaddr$prim57883 = alloca %struct.ScmObj*, align 8
%anf_45bind48303 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48103)
store volatile %struct.ScmObj* %anf_45bind48303, %struct.ScmObj** %stackaddr$prim57883, align 8
%stackaddr$prim57884 = alloca %struct.ScmObj*, align 8
%anf_45bind48304 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48303)
store volatile %struct.ScmObj* %anf_45bind48304, %struct.ScmObj** %stackaddr$prim57884, align 8
%stackaddr$prim57885 = alloca %struct.ScmObj*, align 8
%cpsprim48361 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48304)
store volatile %struct.ScmObj* %cpsprim48361, %struct.ScmObj** %stackaddr$prim57885, align 8
%ae51819 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56984$k483600 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57886 = alloca %struct.ScmObj*, align 8
%argslist56984$k483601 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48361, %struct.ScmObj* %argslist56984$k483600)
store volatile %struct.ScmObj* %argslist56984$k483601, %struct.ScmObj** %stackaddr$prim57886, align 8
%stackaddr$prim57887 = alloca %struct.ScmObj*, align 8
%argslist56984$k483602 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51819, %struct.ScmObj* %argslist56984$k483601)
store volatile %struct.ScmObj* %argslist56984$k483602, %struct.ScmObj** %stackaddr$prim57887, align 8
%clofunc57888 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48360)
musttail call tailcc void %clofunc57888(%struct.ScmObj* %k48360, %struct.ScmObj* %argslist56984$k483602)
ret void
}

define tailcc void @proc_clo$ae51792(%struct.ScmObj* %env$ae51792,%struct.ScmObj* %current_45args56986) {
%stackaddr$prim57889 = alloca %struct.ScmObj*, align 8
%k48362 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56986)
store volatile %struct.ScmObj* %k48362, %struct.ScmObj** %stackaddr$prim57889, align 8
%stackaddr$prim57890 = alloca %struct.ScmObj*, align 8
%current_45args56987 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56986)
store volatile %struct.ScmObj* %current_45args56987, %struct.ScmObj** %stackaddr$prim57890, align 8
%stackaddr$prim57891 = alloca %struct.ScmObj*, align 8
%x48105 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56987)
store volatile %struct.ScmObj* %x48105, %struct.ScmObj** %stackaddr$prim57891, align 8
%stackaddr$prim57892 = alloca %struct.ScmObj*, align 8
%anf_45bind48302 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48105)
store volatile %struct.ScmObj* %anf_45bind48302, %struct.ScmObj** %stackaddr$prim57892, align 8
%stackaddr$prim57893 = alloca %struct.ScmObj*, align 8
%cpsprim48363 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48302)
store volatile %struct.ScmObj* %cpsprim48363, %struct.ScmObj** %stackaddr$prim57893, align 8
%ae51796 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56989$k483620 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57894 = alloca %struct.ScmObj*, align 8
%argslist56989$k483621 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48363, %struct.ScmObj* %argslist56989$k483620)
store volatile %struct.ScmObj* %argslist56989$k483621, %struct.ScmObj** %stackaddr$prim57894, align 8
%stackaddr$prim57895 = alloca %struct.ScmObj*, align 8
%argslist56989$k483622 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51796, %struct.ScmObj* %argslist56989$k483621)
store volatile %struct.ScmObj* %argslist56989$k483622, %struct.ScmObj** %stackaddr$prim57895, align 8
%clofunc57896 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48362)
musttail call tailcc void %clofunc57896(%struct.ScmObj* %k48362, %struct.ScmObj* %argslist56989$k483622)
ret void
}

define tailcc void @proc_clo$ae51772(%struct.ScmObj* %env$ae51772,%struct.ScmObj* %current_45args56991) {
%stackaddr$prim57897 = alloca %struct.ScmObj*, align 8
%k48364 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56991)
store volatile %struct.ScmObj* %k48364, %struct.ScmObj** %stackaddr$prim57897, align 8
%stackaddr$prim57898 = alloca %struct.ScmObj*, align 8
%current_45args56992 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56991)
store volatile %struct.ScmObj* %current_45args56992, %struct.ScmObj** %stackaddr$prim57898, align 8
%stackaddr$prim57899 = alloca %struct.ScmObj*, align 8
%x48107 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56992)
store volatile %struct.ScmObj* %x48107, %struct.ScmObj** %stackaddr$prim57899, align 8
%stackaddr$prim57900 = alloca %struct.ScmObj*, align 8
%cpsprim48365 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48107)
store volatile %struct.ScmObj* %cpsprim48365, %struct.ScmObj** %stackaddr$prim57900, align 8
%ae51775 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56994$k483640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57901 = alloca %struct.ScmObj*, align 8
%argslist56994$k483641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48365, %struct.ScmObj* %argslist56994$k483640)
store volatile %struct.ScmObj* %argslist56994$k483641, %struct.ScmObj** %stackaddr$prim57901, align 8
%stackaddr$prim57902 = alloca %struct.ScmObj*, align 8
%argslist56994$k483642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51775, %struct.ScmObj* %argslist56994$k483641)
store volatile %struct.ScmObj* %argslist56994$k483642, %struct.ScmObj** %stackaddr$prim57902, align 8
%clofunc57903 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48364)
musttail call tailcc void %clofunc57903(%struct.ScmObj* %k48364, %struct.ScmObj* %argslist56994$k483642)
ret void
}

define tailcc void @proc_clo$ae51674(%struct.ScmObj* %env$ae51674,%struct.ScmObj* %args4810948366) {
%stackaddr$env-ref57904 = alloca %struct.ScmObj*, align 8
%_37foldl148048 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51674, i64 0)
store %struct.ScmObj* %_37foldl148048, %struct.ScmObj** %stackaddr$env-ref57904
%stackaddr$prim57905 = alloca %struct.ScmObj*, align 8
%k48367 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4810948366)
store volatile %struct.ScmObj* %k48367, %struct.ScmObj** %stackaddr$prim57905, align 8
%stackaddr$prim57906 = alloca %struct.ScmObj*, align 8
%args48109 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4810948366)
store volatile %struct.ScmObj* %args48109, %struct.ScmObj** %stackaddr$prim57906, align 8
%stackaddr$prim57907 = alloca %struct.ScmObj*, align 8
%anf_45bind48296 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args48109)
store volatile %struct.ScmObj* %anf_45bind48296, %struct.ScmObj** %stackaddr$prim57907, align 8
%truthy$cmp57908 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48296)
%cmp$cmp57908 = icmp eq i64 %truthy$cmp57908, 1
br i1 %cmp$cmp57908, label %truebranch$cmp57908, label %falsebranch$cmp57908
truebranch$cmp57908:
%ae51680 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51681 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist56996$k483670 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57909 = alloca %struct.ScmObj*, align 8
%argslist56996$k483671 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51681, %struct.ScmObj* %argslist56996$k483670)
store volatile %struct.ScmObj* %argslist56996$k483671, %struct.ScmObj** %stackaddr$prim57909, align 8
%stackaddr$prim57910 = alloca %struct.ScmObj*, align 8
%argslist56996$k483672 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51680, %struct.ScmObj* %argslist56996$k483671)
store volatile %struct.ScmObj* %argslist56996$k483672, %struct.ScmObj** %stackaddr$prim57910, align 8
%clofunc57911 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48367)
musttail call tailcc void %clofunc57911(%struct.ScmObj* %k48367, %struct.ScmObj* %argslist56996$k483672)
ret void
falsebranch$cmp57908:
%stackaddr$prim57912 = alloca %struct.ScmObj*, align 8
%anf_45bind48297 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48109)
store volatile %struct.ScmObj* %anf_45bind48297, %struct.ScmObj** %stackaddr$prim57912, align 8
%stackaddr$prim57913 = alloca %struct.ScmObj*, align 8
%anf_45bind48298 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48297)
store volatile %struct.ScmObj* %anf_45bind48298, %struct.ScmObj** %stackaddr$prim57913, align 8
%truthy$cmp57914 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48298)
%cmp$cmp57914 = icmp eq i64 %truthy$cmp57914, 1
br i1 %cmp$cmp57914, label %truebranch$cmp57914, label %falsebranch$cmp57914
truebranch$cmp57914:
%stackaddr$prim57915 = alloca %struct.ScmObj*, align 8
%cpsprim48368 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48109)
store volatile %struct.ScmObj* %cpsprim48368, %struct.ScmObj** %stackaddr$prim57915, align 8
%ae51693 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56997$k483670 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57916 = alloca %struct.ScmObj*, align 8
%argslist56997$k483671 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48368, %struct.ScmObj* %argslist56997$k483670)
store volatile %struct.ScmObj* %argslist56997$k483671, %struct.ScmObj** %stackaddr$prim57916, align 8
%stackaddr$prim57917 = alloca %struct.ScmObj*, align 8
%argslist56997$k483672 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51693, %struct.ScmObj* %argslist56997$k483671)
store volatile %struct.ScmObj* %argslist56997$k483672, %struct.ScmObj** %stackaddr$prim57917, align 8
%clofunc57918 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48367)
musttail call tailcc void %clofunc57918(%struct.ScmObj* %k48367, %struct.ScmObj* %argslist56997$k483672)
ret void
falsebranch$cmp57914:
%stackaddr$makeclosure57919 = alloca %struct.ScmObj*, align 8
%fptrToInt57920 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51698 to i64
%ae51698 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57920)
store volatile %struct.ScmObj* %ae51698, %struct.ScmObj** %stackaddr$makeclosure57919, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51698, %struct.ScmObj* %_37foldl148048, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51698, %struct.ScmObj* %k48367, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51698, %struct.ScmObj* %args48109, i64 2)
%ae51699 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57921 = alloca %struct.ScmObj*, align 8
%fptrToInt57922 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51700 to i64
%ae51700 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57922)
store volatile %struct.ScmObj* %ae51700, %struct.ScmObj** %stackaddr$makeclosure57921, align 8
%argslist57007$ae516980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57923 = alloca %struct.ScmObj*, align 8
%argslist57007$ae516981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51700, %struct.ScmObj* %argslist57007$ae516980)
store volatile %struct.ScmObj* %argslist57007$ae516981, %struct.ScmObj** %stackaddr$prim57923, align 8
%stackaddr$prim57924 = alloca %struct.ScmObj*, align 8
%argslist57007$ae516982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51699, %struct.ScmObj* %argslist57007$ae516981)
store volatile %struct.ScmObj* %argslist57007$ae516982, %struct.ScmObj** %stackaddr$prim57924, align 8
%clofunc57925 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51698)
musttail call tailcc void %clofunc57925(%struct.ScmObj* %ae51698, %struct.ScmObj* %argslist57007$ae516982)
ret void
}

define tailcc void @proc_clo$ae51698(%struct.ScmObj* %env$ae51698,%struct.ScmObj* %current_45args56998) {
%stackaddr$env-ref57926 = alloca %struct.ScmObj*, align 8
%_37foldl148048 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51698, i64 0)
store %struct.ScmObj* %_37foldl148048, %struct.ScmObj** %stackaddr$env-ref57926
%stackaddr$env-ref57927 = alloca %struct.ScmObj*, align 8
%k48367 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51698, i64 1)
store %struct.ScmObj* %k48367, %struct.ScmObj** %stackaddr$env-ref57927
%stackaddr$env-ref57928 = alloca %struct.ScmObj*, align 8
%args48109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51698, i64 2)
store %struct.ScmObj* %args48109, %struct.ScmObj** %stackaddr$env-ref57928
%stackaddr$prim57929 = alloca %struct.ScmObj*, align 8
%_95k48369 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56998)
store volatile %struct.ScmObj* %_95k48369, %struct.ScmObj** %stackaddr$prim57929, align 8
%stackaddr$prim57930 = alloca %struct.ScmObj*, align 8
%current_45args56999 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56998)
store volatile %struct.ScmObj* %current_45args56999, %struct.ScmObj** %stackaddr$prim57930, align 8
%stackaddr$prim57931 = alloca %struct.ScmObj*, align 8
%anf_45bind48299 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56999)
store volatile %struct.ScmObj* %anf_45bind48299, %struct.ScmObj** %stackaddr$prim57931, align 8
%stackaddr$prim57932 = alloca %struct.ScmObj*, align 8
%anf_45bind48300 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48109)
store volatile %struct.ScmObj* %anf_45bind48300, %struct.ScmObj** %stackaddr$prim57932, align 8
%stackaddr$prim57933 = alloca %struct.ScmObj*, align 8
%anf_45bind48301 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48109)
store volatile %struct.ScmObj* %anf_45bind48301, %struct.ScmObj** %stackaddr$prim57933, align 8
%argslist57001$_37foldl1480480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57934 = alloca %struct.ScmObj*, align 8
%argslist57001$_37foldl1480481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48301, %struct.ScmObj* %argslist57001$_37foldl1480480)
store volatile %struct.ScmObj* %argslist57001$_37foldl1480481, %struct.ScmObj** %stackaddr$prim57934, align 8
%stackaddr$prim57935 = alloca %struct.ScmObj*, align 8
%argslist57001$_37foldl1480482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48300, %struct.ScmObj* %argslist57001$_37foldl1480481)
store volatile %struct.ScmObj* %argslist57001$_37foldl1480482, %struct.ScmObj** %stackaddr$prim57935, align 8
%stackaddr$prim57936 = alloca %struct.ScmObj*, align 8
%argslist57001$_37foldl1480483 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48299, %struct.ScmObj* %argslist57001$_37foldl1480482)
store volatile %struct.ScmObj* %argslist57001$_37foldl1480483, %struct.ScmObj** %stackaddr$prim57936, align 8
%stackaddr$prim57937 = alloca %struct.ScmObj*, align 8
%argslist57001$_37foldl1480484 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48367, %struct.ScmObj* %argslist57001$_37foldl1480483)
store volatile %struct.ScmObj* %argslist57001$_37foldl1480484, %struct.ScmObj** %stackaddr$prim57937, align 8
%clofunc57938 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148048)
musttail call tailcc void %clofunc57938(%struct.ScmObj* %_37foldl148048, %struct.ScmObj* %argslist57001$_37foldl1480484)
ret void
}

define tailcc void @proc_clo$ae51700(%struct.ScmObj* %env$ae51700,%struct.ScmObj* %current_45args57002) {
%stackaddr$prim57939 = alloca %struct.ScmObj*, align 8
%k48370 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57002)
store volatile %struct.ScmObj* %k48370, %struct.ScmObj** %stackaddr$prim57939, align 8
%stackaddr$prim57940 = alloca %struct.ScmObj*, align 8
%current_45args57003 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57002)
store volatile %struct.ScmObj* %current_45args57003, %struct.ScmObj** %stackaddr$prim57940, align 8
%stackaddr$prim57941 = alloca %struct.ScmObj*, align 8
%n48111 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57003)
store volatile %struct.ScmObj* %n48111, %struct.ScmObj** %stackaddr$prim57941, align 8
%stackaddr$prim57942 = alloca %struct.ScmObj*, align 8
%current_45args57004 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57003)
store volatile %struct.ScmObj* %current_45args57004, %struct.ScmObj** %stackaddr$prim57942, align 8
%stackaddr$prim57943 = alloca %struct.ScmObj*, align 8
%v48110 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57004)
store volatile %struct.ScmObj* %v48110, %struct.ScmObj** %stackaddr$prim57943, align 8
%stackaddr$prim57944 = alloca %struct.ScmObj*, align 8
%cpsprim48371 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v48110, %struct.ScmObj* %n48111)
store volatile %struct.ScmObj* %cpsprim48371, %struct.ScmObj** %stackaddr$prim57944, align 8
%ae51704 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57006$k483700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57945 = alloca %struct.ScmObj*, align 8
%argslist57006$k483701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48371, %struct.ScmObj* %argslist57006$k483700)
store volatile %struct.ScmObj* %argslist57006$k483701, %struct.ScmObj** %stackaddr$prim57945, align 8
%stackaddr$prim57946 = alloca %struct.ScmObj*, align 8
%argslist57006$k483702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51704, %struct.ScmObj* %argslist57006$k483701)
store volatile %struct.ScmObj* %argslist57006$k483702, %struct.ScmObj** %stackaddr$prim57946, align 8
%clofunc57947 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48370)
musttail call tailcc void %clofunc57947(%struct.ScmObj* %k48370, %struct.ScmObj* %argslist57006$k483702)
ret void
}

define tailcc void @proc_clo$ae51270(%struct.ScmObj* %env$ae51270,%struct.ScmObj* %current_45args57009) {
%stackaddr$prim57948 = alloca %struct.ScmObj*, align 8
%k48372 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57009)
store volatile %struct.ScmObj* %k48372, %struct.ScmObj** %stackaddr$prim57948, align 8
%stackaddr$prim57949 = alloca %struct.ScmObj*, align 8
%current_45args57010 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57009)
store volatile %struct.ScmObj* %current_45args57010, %struct.ScmObj** %stackaddr$prim57949, align 8
%stackaddr$prim57950 = alloca %struct.ScmObj*, align 8
%v48114 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57010)
store volatile %struct.ScmObj* %v48114, %struct.ScmObj** %stackaddr$prim57950, align 8
%stackaddr$prim57951 = alloca %struct.ScmObj*, align 8
%current_45args57011 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57010)
store volatile %struct.ScmObj* %current_45args57011, %struct.ScmObj** %stackaddr$prim57951, align 8
%stackaddr$prim57952 = alloca %struct.ScmObj*, align 8
%lst48113 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57011)
store volatile %struct.ScmObj* %lst48113, %struct.ScmObj** %stackaddr$prim57952, align 8
%ae51271 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57953 = alloca %struct.ScmObj*, align 8
%lst48115 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51271, %struct.ScmObj* %lst48113)
store volatile %struct.ScmObj* %lst48115, %struct.ScmObj** %stackaddr$prim57953, align 8
%stackaddr$makeclosure57954 = alloca %struct.ScmObj*, align 8
%fptrToInt57955 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51273 to i64
%ae51273 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57955)
store volatile %struct.ScmObj* %ae51273, %struct.ScmObj** %stackaddr$makeclosure57954, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51273, %struct.ScmObj* %k48372, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51273, %struct.ScmObj* %lst48115, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51273, %struct.ScmObj* %v48114, i64 2)
%ae51274 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57956 = alloca %struct.ScmObj*, align 8
%fptrToInt57957 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51275 to i64
%ae51275 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57957)
store volatile %struct.ScmObj* %ae51275, %struct.ScmObj** %stackaddr$makeclosure57956, align 8
%argslist57033$ae512730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57958 = alloca %struct.ScmObj*, align 8
%argslist57033$ae512731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51275, %struct.ScmObj* %argslist57033$ae512730)
store volatile %struct.ScmObj* %argslist57033$ae512731, %struct.ScmObj** %stackaddr$prim57958, align 8
%stackaddr$prim57959 = alloca %struct.ScmObj*, align 8
%argslist57033$ae512732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51274, %struct.ScmObj* %argslist57033$ae512731)
store volatile %struct.ScmObj* %argslist57033$ae512732, %struct.ScmObj** %stackaddr$prim57959, align 8
%clofunc57960 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51273)
musttail call tailcc void %clofunc57960(%struct.ScmObj* %ae51273, %struct.ScmObj* %argslist57033$ae512732)
ret void
}

define tailcc void @proc_clo$ae51273(%struct.ScmObj* %env$ae51273,%struct.ScmObj* %current_45args57013) {
%stackaddr$env-ref57961 = alloca %struct.ScmObj*, align 8
%k48372 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51273, i64 0)
store %struct.ScmObj* %k48372, %struct.ScmObj** %stackaddr$env-ref57961
%stackaddr$env-ref57962 = alloca %struct.ScmObj*, align 8
%lst48115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51273, i64 1)
store %struct.ScmObj* %lst48115, %struct.ScmObj** %stackaddr$env-ref57962
%stackaddr$env-ref57963 = alloca %struct.ScmObj*, align 8
%v48114 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51273, i64 2)
store %struct.ScmObj* %v48114, %struct.ScmObj** %stackaddr$env-ref57963
%stackaddr$prim57964 = alloca %struct.ScmObj*, align 8
%_95k48373 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57013)
store volatile %struct.ScmObj* %_95k48373, %struct.ScmObj** %stackaddr$prim57964, align 8
%stackaddr$prim57965 = alloca %struct.ScmObj*, align 8
%current_45args57014 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57013)
store volatile %struct.ScmObj* %current_45args57014, %struct.ScmObj** %stackaddr$prim57965, align 8
%stackaddr$prim57966 = alloca %struct.ScmObj*, align 8
%anf_45bind48288 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57014)
store volatile %struct.ScmObj* %anf_45bind48288, %struct.ScmObj** %stackaddr$prim57966, align 8
%stackaddr$makeclosure57967 = alloca %struct.ScmObj*, align 8
%fptrToInt57968 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51289 to i64
%ae51289 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57968)
store volatile %struct.ScmObj* %ae51289, %struct.ScmObj** %stackaddr$makeclosure57967, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51289, %struct.ScmObj* %k48372, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51289, %struct.ScmObj* %lst48115, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51289, %struct.ScmObj* %v48114, i64 2)
%stackaddr$makeclosure57969 = alloca %struct.ScmObj*, align 8
%fptrToInt57970 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51290 to i64
%ae51290 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57970)
store volatile %struct.ScmObj* %ae51290, %struct.ScmObj** %stackaddr$makeclosure57969, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51290, %struct.ScmObj* %k48372, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51290, %struct.ScmObj* %lst48115, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51290, %struct.ScmObj* %v48114, i64 2)
%argslist57028$anf_45bind482880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57971 = alloca %struct.ScmObj*, align 8
%argslist57028$anf_45bind482881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51290, %struct.ScmObj* %argslist57028$anf_45bind482880)
store volatile %struct.ScmObj* %argslist57028$anf_45bind482881, %struct.ScmObj** %stackaddr$prim57971, align 8
%stackaddr$prim57972 = alloca %struct.ScmObj*, align 8
%argslist57028$anf_45bind482882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51289, %struct.ScmObj* %argslist57028$anf_45bind482881)
store volatile %struct.ScmObj* %argslist57028$anf_45bind482882, %struct.ScmObj** %stackaddr$prim57972, align 8
%clofunc57973 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48288)
musttail call tailcc void %clofunc57973(%struct.ScmObj* %anf_45bind48288, %struct.ScmObj* %argslist57028$anf_45bind482882)
ret void
}

define tailcc void @proc_clo$ae51289(%struct.ScmObj* %env$ae51289,%struct.ScmObj* %current_45args57016) {
%stackaddr$env-ref57974 = alloca %struct.ScmObj*, align 8
%k48372 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51289, i64 0)
store %struct.ScmObj* %k48372, %struct.ScmObj** %stackaddr$env-ref57974
%stackaddr$env-ref57975 = alloca %struct.ScmObj*, align 8
%lst48115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51289, i64 1)
store %struct.ScmObj* %lst48115, %struct.ScmObj** %stackaddr$env-ref57975
%stackaddr$env-ref57976 = alloca %struct.ScmObj*, align 8
%v48114 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51289, i64 2)
store %struct.ScmObj* %v48114, %struct.ScmObj** %stackaddr$env-ref57976
%stackaddr$prim57977 = alloca %struct.ScmObj*, align 8
%_95k48374 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57016)
store volatile %struct.ScmObj* %_95k48374, %struct.ScmObj** %stackaddr$prim57977, align 8
%stackaddr$prim57978 = alloca %struct.ScmObj*, align 8
%current_45args57017 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57016)
store volatile %struct.ScmObj* %current_45args57017, %struct.ScmObj** %stackaddr$prim57978, align 8
%stackaddr$prim57979 = alloca %struct.ScmObj*, align 8
%cc48116 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57017)
store volatile %struct.ScmObj* %cc48116, %struct.ScmObj** %stackaddr$prim57979, align 8
%ae51398 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57980 = alloca %struct.ScmObj*, align 8
%anf_45bind48289 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48115, %struct.ScmObj* %ae51398)
store volatile %struct.ScmObj* %anf_45bind48289, %struct.ScmObj** %stackaddr$prim57980, align 8
%stackaddr$prim57981 = alloca %struct.ScmObj*, align 8
%anf_45bind48290 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48289)
store volatile %struct.ScmObj* %anf_45bind48290, %struct.ScmObj** %stackaddr$prim57981, align 8
%truthy$cmp57982 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48290)
%cmp$cmp57982 = icmp eq i64 %truthy$cmp57982, 1
br i1 %cmp$cmp57982, label %truebranch$cmp57982, label %falsebranch$cmp57982
truebranch$cmp57982:
%ae51402 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51403 = call %struct.ScmObj* @const_init_false()
%argslist57019$k483720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57983 = alloca %struct.ScmObj*, align 8
%argslist57019$k483721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51403, %struct.ScmObj* %argslist57019$k483720)
store volatile %struct.ScmObj* %argslist57019$k483721, %struct.ScmObj** %stackaddr$prim57983, align 8
%stackaddr$prim57984 = alloca %struct.ScmObj*, align 8
%argslist57019$k483722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51402, %struct.ScmObj* %argslist57019$k483721)
store volatile %struct.ScmObj* %argslist57019$k483722, %struct.ScmObj** %stackaddr$prim57984, align 8
%clofunc57985 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48372)
musttail call tailcc void %clofunc57985(%struct.ScmObj* %k48372, %struct.ScmObj* %argslist57019$k483722)
ret void
falsebranch$cmp57982:
%ae51411 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57986 = alloca %struct.ScmObj*, align 8
%anf_45bind48291 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48115, %struct.ScmObj* %ae51411)
store volatile %struct.ScmObj* %anf_45bind48291, %struct.ScmObj** %stackaddr$prim57986, align 8
%stackaddr$prim57987 = alloca %struct.ScmObj*, align 8
%anf_45bind48292 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48291)
store volatile %struct.ScmObj* %anf_45bind48292, %struct.ScmObj** %stackaddr$prim57987, align 8
%stackaddr$prim57988 = alloca %struct.ScmObj*, align 8
%anf_45bind48293 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind48292, %struct.ScmObj* %v48114)
store volatile %struct.ScmObj* %anf_45bind48293, %struct.ScmObj** %stackaddr$prim57988, align 8
%truthy$cmp57989 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48293)
%cmp$cmp57989 = icmp eq i64 %truthy$cmp57989, 1
br i1 %cmp$cmp57989, label %truebranch$cmp57989, label %falsebranch$cmp57989
truebranch$cmp57989:
%ae51417 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57990 = alloca %struct.ScmObj*, align 8
%cpsprim48375 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48115, %struct.ScmObj* %ae51417)
store volatile %struct.ScmObj* %cpsprim48375, %struct.ScmObj** %stackaddr$prim57990, align 8
%ae51419 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57020$k483720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57991 = alloca %struct.ScmObj*, align 8
%argslist57020$k483721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48375, %struct.ScmObj* %argslist57020$k483720)
store volatile %struct.ScmObj* %argslist57020$k483721, %struct.ScmObj** %stackaddr$prim57991, align 8
%stackaddr$prim57992 = alloca %struct.ScmObj*, align 8
%argslist57020$k483722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51419, %struct.ScmObj* %argslist57020$k483721)
store volatile %struct.ScmObj* %argslist57020$k483722, %struct.ScmObj** %stackaddr$prim57992, align 8
%clofunc57993 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48372)
musttail call tailcc void %clofunc57993(%struct.ScmObj* %k48372, %struct.ScmObj* %argslist57020$k483722)
ret void
falsebranch$cmp57989:
%ae51430 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57994 = alloca %struct.ScmObj*, align 8
%anf_45bind48294 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48115, %struct.ScmObj* %ae51430)
store volatile %struct.ScmObj* %anf_45bind48294, %struct.ScmObj** %stackaddr$prim57994, align 8
%stackaddr$prim57995 = alloca %struct.ScmObj*, align 8
%anf_45bind48295 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48294)
store volatile %struct.ScmObj* %anf_45bind48295, %struct.ScmObj** %stackaddr$prim57995, align 8
%ae51433 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57996 = alloca %struct.ScmObj*, align 8
%_95048118 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48115, %struct.ScmObj* %ae51433, %struct.ScmObj* %anf_45bind48295)
store volatile %struct.ScmObj* %_95048118, %struct.ScmObj** %stackaddr$prim57996, align 8
%argslist57021$cc481160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57997 = alloca %struct.ScmObj*, align 8
%argslist57021$cc481161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48116, %struct.ScmObj* %argslist57021$cc481160)
store volatile %struct.ScmObj* %argslist57021$cc481161, %struct.ScmObj** %stackaddr$prim57997, align 8
%stackaddr$prim57998 = alloca %struct.ScmObj*, align 8
%argslist57021$cc481162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48372, %struct.ScmObj* %argslist57021$cc481161)
store volatile %struct.ScmObj* %argslist57021$cc481162, %struct.ScmObj** %stackaddr$prim57998, align 8
%clofunc57999 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48116)
musttail call tailcc void %clofunc57999(%struct.ScmObj* %cc48116, %struct.ScmObj* %argslist57021$cc481162)
ret void
}

define tailcc void @proc_clo$ae51290(%struct.ScmObj* %env$ae51290,%struct.ScmObj* %current_45args57022) {
%stackaddr$env-ref58000 = alloca %struct.ScmObj*, align 8
%k48372 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51290, i64 0)
store %struct.ScmObj* %k48372, %struct.ScmObj** %stackaddr$env-ref58000
%stackaddr$env-ref58001 = alloca %struct.ScmObj*, align 8
%lst48115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51290, i64 1)
store %struct.ScmObj* %lst48115, %struct.ScmObj** %stackaddr$env-ref58001
%stackaddr$env-ref58002 = alloca %struct.ScmObj*, align 8
%v48114 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51290, i64 2)
store %struct.ScmObj* %v48114, %struct.ScmObj** %stackaddr$env-ref58002
%stackaddr$prim58003 = alloca %struct.ScmObj*, align 8
%_95k48374 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57022)
store volatile %struct.ScmObj* %_95k48374, %struct.ScmObj** %stackaddr$prim58003, align 8
%stackaddr$prim58004 = alloca %struct.ScmObj*, align 8
%current_45args57023 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57022)
store volatile %struct.ScmObj* %current_45args57023, %struct.ScmObj** %stackaddr$prim58004, align 8
%stackaddr$prim58005 = alloca %struct.ScmObj*, align 8
%cc48116 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57023)
store volatile %struct.ScmObj* %cc48116, %struct.ScmObj** %stackaddr$prim58005, align 8
%ae51292 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58006 = alloca %struct.ScmObj*, align 8
%anf_45bind48289 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48115, %struct.ScmObj* %ae51292)
store volatile %struct.ScmObj* %anf_45bind48289, %struct.ScmObj** %stackaddr$prim58006, align 8
%stackaddr$prim58007 = alloca %struct.ScmObj*, align 8
%anf_45bind48290 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48289)
store volatile %struct.ScmObj* %anf_45bind48290, %struct.ScmObj** %stackaddr$prim58007, align 8
%truthy$cmp58008 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48290)
%cmp$cmp58008 = icmp eq i64 %truthy$cmp58008, 1
br i1 %cmp$cmp58008, label %truebranch$cmp58008, label %falsebranch$cmp58008
truebranch$cmp58008:
%ae51296 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51297 = call %struct.ScmObj* @const_init_false()
%argslist57025$k483720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58009 = alloca %struct.ScmObj*, align 8
%argslist57025$k483721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51297, %struct.ScmObj* %argslist57025$k483720)
store volatile %struct.ScmObj* %argslist57025$k483721, %struct.ScmObj** %stackaddr$prim58009, align 8
%stackaddr$prim58010 = alloca %struct.ScmObj*, align 8
%argslist57025$k483722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51296, %struct.ScmObj* %argslist57025$k483721)
store volatile %struct.ScmObj* %argslist57025$k483722, %struct.ScmObj** %stackaddr$prim58010, align 8
%clofunc58011 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48372)
musttail call tailcc void %clofunc58011(%struct.ScmObj* %k48372, %struct.ScmObj* %argslist57025$k483722)
ret void
falsebranch$cmp58008:
%ae51305 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58012 = alloca %struct.ScmObj*, align 8
%anf_45bind48291 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48115, %struct.ScmObj* %ae51305)
store volatile %struct.ScmObj* %anf_45bind48291, %struct.ScmObj** %stackaddr$prim58012, align 8
%stackaddr$prim58013 = alloca %struct.ScmObj*, align 8
%anf_45bind48292 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48291)
store volatile %struct.ScmObj* %anf_45bind48292, %struct.ScmObj** %stackaddr$prim58013, align 8
%stackaddr$prim58014 = alloca %struct.ScmObj*, align 8
%anf_45bind48293 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind48292, %struct.ScmObj* %v48114)
store volatile %struct.ScmObj* %anf_45bind48293, %struct.ScmObj** %stackaddr$prim58014, align 8
%truthy$cmp58015 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48293)
%cmp$cmp58015 = icmp eq i64 %truthy$cmp58015, 1
br i1 %cmp$cmp58015, label %truebranch$cmp58015, label %falsebranch$cmp58015
truebranch$cmp58015:
%ae51311 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58016 = alloca %struct.ScmObj*, align 8
%cpsprim48375 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48115, %struct.ScmObj* %ae51311)
store volatile %struct.ScmObj* %cpsprim48375, %struct.ScmObj** %stackaddr$prim58016, align 8
%ae51313 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57026$k483720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58017 = alloca %struct.ScmObj*, align 8
%argslist57026$k483721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48375, %struct.ScmObj* %argslist57026$k483720)
store volatile %struct.ScmObj* %argslist57026$k483721, %struct.ScmObj** %stackaddr$prim58017, align 8
%stackaddr$prim58018 = alloca %struct.ScmObj*, align 8
%argslist57026$k483722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51313, %struct.ScmObj* %argslist57026$k483721)
store volatile %struct.ScmObj* %argslist57026$k483722, %struct.ScmObj** %stackaddr$prim58018, align 8
%clofunc58019 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48372)
musttail call tailcc void %clofunc58019(%struct.ScmObj* %k48372, %struct.ScmObj* %argslist57026$k483722)
ret void
falsebranch$cmp58015:
%ae51324 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58020 = alloca %struct.ScmObj*, align 8
%anf_45bind48294 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48115, %struct.ScmObj* %ae51324)
store volatile %struct.ScmObj* %anf_45bind48294, %struct.ScmObj** %stackaddr$prim58020, align 8
%stackaddr$prim58021 = alloca %struct.ScmObj*, align 8
%anf_45bind48295 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48294)
store volatile %struct.ScmObj* %anf_45bind48295, %struct.ScmObj** %stackaddr$prim58021, align 8
%ae51327 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58022 = alloca %struct.ScmObj*, align 8
%_95048118 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48115, %struct.ScmObj* %ae51327, %struct.ScmObj* %anf_45bind48295)
store volatile %struct.ScmObj* %_95048118, %struct.ScmObj** %stackaddr$prim58022, align 8
%argslist57027$cc481160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58023 = alloca %struct.ScmObj*, align 8
%argslist57027$cc481161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48116, %struct.ScmObj* %argslist57027$cc481160)
store volatile %struct.ScmObj* %argslist57027$cc481161, %struct.ScmObj** %stackaddr$prim58023, align 8
%stackaddr$prim58024 = alloca %struct.ScmObj*, align 8
%argslist57027$cc481162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48372, %struct.ScmObj* %argslist57027$cc481161)
store volatile %struct.ScmObj* %argslist57027$cc481162, %struct.ScmObj** %stackaddr$prim58024, align 8
%clofunc58025 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48116)
musttail call tailcc void %clofunc58025(%struct.ScmObj* %cc48116, %struct.ScmObj* %argslist57027$cc481162)
ret void
}

define tailcc void @proc_clo$ae51275(%struct.ScmObj* %env$ae51275,%struct.ScmObj* %current_45args57029) {
%stackaddr$prim58026 = alloca %struct.ScmObj*, align 8
%k48376 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57029)
store volatile %struct.ScmObj* %k48376, %struct.ScmObj** %stackaddr$prim58026, align 8
%stackaddr$prim58027 = alloca %struct.ScmObj*, align 8
%current_45args57030 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57029)
store volatile %struct.ScmObj* %current_45args57030, %struct.ScmObj** %stackaddr$prim58027, align 8
%stackaddr$prim58028 = alloca %struct.ScmObj*, align 8
%u48117 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57030)
store volatile %struct.ScmObj* %u48117, %struct.ScmObj** %stackaddr$prim58028, align 8
%argslist57032$u481170 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58029 = alloca %struct.ScmObj*, align 8
%argslist57032$u481171 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u48117, %struct.ScmObj* %argslist57032$u481170)
store volatile %struct.ScmObj* %argslist57032$u481171, %struct.ScmObj** %stackaddr$prim58029, align 8
%stackaddr$prim58030 = alloca %struct.ScmObj*, align 8
%argslist57032$u481172 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48376, %struct.ScmObj* %argslist57032$u481171)
store volatile %struct.ScmObj* %argslist57032$u481172, %struct.ScmObj** %stackaddr$prim58030, align 8
%clofunc58031 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u48117)
musttail call tailcc void %clofunc58031(%struct.ScmObj* %u48117, %struct.ScmObj* %argslist57032$u481172)
ret void
}

define tailcc void @proc_clo$ae50734(%struct.ScmObj* %env$ae50734,%struct.ScmObj* %current_45args57035) {
%stackaddr$prim58032 = alloca %struct.ScmObj*, align 8
%k48377 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57035)
store volatile %struct.ScmObj* %k48377, %struct.ScmObj** %stackaddr$prim58032, align 8
%stackaddr$prim58033 = alloca %struct.ScmObj*, align 8
%current_45args57036 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57035)
store volatile %struct.ScmObj* %current_45args57036, %struct.ScmObj** %stackaddr$prim58033, align 8
%stackaddr$prim58034 = alloca %struct.ScmObj*, align 8
%lst48121 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57036)
store volatile %struct.ScmObj* %lst48121, %struct.ScmObj** %stackaddr$prim58034, align 8
%stackaddr$prim58035 = alloca %struct.ScmObj*, align 8
%current_45args57037 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57036)
store volatile %struct.ScmObj* %current_45args57037, %struct.ScmObj** %stackaddr$prim58035, align 8
%stackaddr$prim58036 = alloca %struct.ScmObj*, align 8
%n48120 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57037)
store volatile %struct.ScmObj* %n48120, %struct.ScmObj** %stackaddr$prim58036, align 8
%ae50735 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim58037 = alloca %struct.ScmObj*, align 8
%n48123 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50735, %struct.ScmObj* %n48120)
store volatile %struct.ScmObj* %n48123, %struct.ScmObj** %stackaddr$prim58037, align 8
%ae50737 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim58038 = alloca %struct.ScmObj*, align 8
%lst48122 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50737, %struct.ScmObj* %lst48121)
store volatile %struct.ScmObj* %lst48122, %struct.ScmObj** %stackaddr$prim58038, align 8
%stackaddr$makeclosure58039 = alloca %struct.ScmObj*, align 8
%fptrToInt58040 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50739 to i64
%ae50739 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58040)
store volatile %struct.ScmObj* %ae50739, %struct.ScmObj** %stackaddr$makeclosure58039, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50739, %struct.ScmObj* %k48377, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50739, %struct.ScmObj* %n48123, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50739, %struct.ScmObj* %lst48122, i64 2)
%ae50740 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58041 = alloca %struct.ScmObj*, align 8
%fptrToInt58042 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50741 to i64
%ae50741 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58042)
store volatile %struct.ScmObj* %ae50741, %struct.ScmObj** %stackaddr$makeclosure58041, align 8
%argslist57057$ae507390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58043 = alloca %struct.ScmObj*, align 8
%argslist57057$ae507391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50741, %struct.ScmObj* %argslist57057$ae507390)
store volatile %struct.ScmObj* %argslist57057$ae507391, %struct.ScmObj** %stackaddr$prim58043, align 8
%stackaddr$prim58044 = alloca %struct.ScmObj*, align 8
%argslist57057$ae507392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50740, %struct.ScmObj* %argslist57057$ae507391)
store volatile %struct.ScmObj* %argslist57057$ae507392, %struct.ScmObj** %stackaddr$prim58044, align 8
%clofunc58045 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50739)
musttail call tailcc void %clofunc58045(%struct.ScmObj* %ae50739, %struct.ScmObj* %argslist57057$ae507392)
ret void
}

define tailcc void @proc_clo$ae50739(%struct.ScmObj* %env$ae50739,%struct.ScmObj* %current_45args57039) {
%stackaddr$env-ref58046 = alloca %struct.ScmObj*, align 8
%k48377 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50739, i64 0)
store %struct.ScmObj* %k48377, %struct.ScmObj** %stackaddr$env-ref58046
%stackaddr$env-ref58047 = alloca %struct.ScmObj*, align 8
%n48123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50739, i64 1)
store %struct.ScmObj* %n48123, %struct.ScmObj** %stackaddr$env-ref58047
%stackaddr$env-ref58048 = alloca %struct.ScmObj*, align 8
%lst48122 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50739, i64 2)
store %struct.ScmObj* %lst48122, %struct.ScmObj** %stackaddr$env-ref58048
%stackaddr$prim58049 = alloca %struct.ScmObj*, align 8
%_95k48378 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57039)
store volatile %struct.ScmObj* %_95k48378, %struct.ScmObj** %stackaddr$prim58049, align 8
%stackaddr$prim58050 = alloca %struct.ScmObj*, align 8
%current_45args57040 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57039)
store volatile %struct.ScmObj* %current_45args57040, %struct.ScmObj** %stackaddr$prim58050, align 8
%stackaddr$prim58051 = alloca %struct.ScmObj*, align 8
%anf_45bind48281 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57040)
store volatile %struct.ScmObj* %anf_45bind48281, %struct.ScmObj** %stackaddr$prim58051, align 8
%stackaddr$makeclosure58052 = alloca %struct.ScmObj*, align 8
%fptrToInt58053 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50755 to i64
%ae50755 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58053)
store volatile %struct.ScmObj* %ae50755, %struct.ScmObj** %stackaddr$makeclosure58052, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50755, %struct.ScmObj* %k48377, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50755, %struct.ScmObj* %n48123, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50755, %struct.ScmObj* %lst48122, i64 2)
%stackaddr$makeclosure58054 = alloca %struct.ScmObj*, align 8
%fptrToInt58055 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50756 to i64
%ae50756 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58055)
store volatile %struct.ScmObj* %ae50756, %struct.ScmObj** %stackaddr$makeclosure58054, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50756, %struct.ScmObj* %k48377, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50756, %struct.ScmObj* %n48123, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50756, %struct.ScmObj* %lst48122, i64 2)
%argslist57052$anf_45bind482810 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58056 = alloca %struct.ScmObj*, align 8
%argslist57052$anf_45bind482811 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50756, %struct.ScmObj* %argslist57052$anf_45bind482810)
store volatile %struct.ScmObj* %argslist57052$anf_45bind482811, %struct.ScmObj** %stackaddr$prim58056, align 8
%stackaddr$prim58057 = alloca %struct.ScmObj*, align 8
%argslist57052$anf_45bind482812 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50755, %struct.ScmObj* %argslist57052$anf_45bind482811)
store volatile %struct.ScmObj* %argslist57052$anf_45bind482812, %struct.ScmObj** %stackaddr$prim58057, align 8
%clofunc58058 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48281)
musttail call tailcc void %clofunc58058(%struct.ScmObj* %anf_45bind48281, %struct.ScmObj* %argslist57052$anf_45bind482812)
ret void
}

define tailcc void @proc_clo$ae50755(%struct.ScmObj* %env$ae50755,%struct.ScmObj* %current_45args57042) {
%stackaddr$env-ref58059 = alloca %struct.ScmObj*, align 8
%k48377 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50755, i64 0)
store %struct.ScmObj* %k48377, %struct.ScmObj** %stackaddr$env-ref58059
%stackaddr$env-ref58060 = alloca %struct.ScmObj*, align 8
%n48123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50755, i64 1)
store %struct.ScmObj* %n48123, %struct.ScmObj** %stackaddr$env-ref58060
%stackaddr$env-ref58061 = alloca %struct.ScmObj*, align 8
%lst48122 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50755, i64 2)
store %struct.ScmObj* %lst48122, %struct.ScmObj** %stackaddr$env-ref58061
%stackaddr$prim58062 = alloca %struct.ScmObj*, align 8
%_95k48379 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57042)
store volatile %struct.ScmObj* %_95k48379, %struct.ScmObj** %stackaddr$prim58062, align 8
%stackaddr$prim58063 = alloca %struct.ScmObj*, align 8
%current_45args57043 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57042)
store volatile %struct.ScmObj* %current_45args57043, %struct.ScmObj** %stackaddr$prim58063, align 8
%stackaddr$prim58064 = alloca %struct.ScmObj*, align 8
%cc48124 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57043)
store volatile %struct.ScmObj* %cc48124, %struct.ScmObj** %stackaddr$prim58064, align 8
%ae50898 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58065 = alloca %struct.ScmObj*, align 8
%anf_45bind48282 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48123, %struct.ScmObj* %ae50898)
store volatile %struct.ScmObj* %anf_45bind48282, %struct.ScmObj** %stackaddr$prim58065, align 8
%ae50899 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58066 = alloca %struct.ScmObj*, align 8
%anf_45bind48283 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae50899, %struct.ScmObj* %anf_45bind48282)
store volatile %struct.ScmObj* %anf_45bind48283, %struct.ScmObj** %stackaddr$prim58066, align 8
%truthy$cmp58067 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48283)
%cmp$cmp58067 = icmp eq i64 %truthy$cmp58067, 1
br i1 %cmp$cmp58067, label %truebranch$cmp58067, label %falsebranch$cmp58067
truebranch$cmp58067:
%ae50903 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58068 = alloca %struct.ScmObj*, align 8
%cpsprim48380 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48122, %struct.ScmObj* %ae50903)
store volatile %struct.ScmObj* %cpsprim48380, %struct.ScmObj** %stackaddr$prim58068, align 8
%ae50905 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57045$k483770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58069 = alloca %struct.ScmObj*, align 8
%argslist57045$k483771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48380, %struct.ScmObj* %argslist57045$k483770)
store volatile %struct.ScmObj* %argslist57045$k483771, %struct.ScmObj** %stackaddr$prim58069, align 8
%stackaddr$prim58070 = alloca %struct.ScmObj*, align 8
%argslist57045$k483772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50905, %struct.ScmObj* %argslist57045$k483771)
store volatile %struct.ScmObj* %argslist57045$k483772, %struct.ScmObj** %stackaddr$prim58070, align 8
%clofunc58071 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48377)
musttail call tailcc void %clofunc58071(%struct.ScmObj* %k48377, %struct.ScmObj* %argslist57045$k483772)
ret void
falsebranch$cmp58067:
%ae50916 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58072 = alloca %struct.ScmObj*, align 8
%anf_45bind48284 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48122, %struct.ScmObj* %ae50916)
store volatile %struct.ScmObj* %anf_45bind48284, %struct.ScmObj** %stackaddr$prim58072, align 8
%stackaddr$prim58073 = alloca %struct.ScmObj*, align 8
%anf_45bind48285 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48284)
store volatile %struct.ScmObj* %anf_45bind48285, %struct.ScmObj** %stackaddr$prim58073, align 8
%ae50919 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58074 = alloca %struct.ScmObj*, align 8
%_95048127 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48122, %struct.ScmObj* %ae50919, %struct.ScmObj* %anf_45bind48285)
store volatile %struct.ScmObj* %_95048127, %struct.ScmObj** %stackaddr$prim58074, align 8
%ae50922 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58075 = alloca %struct.ScmObj*, align 8
%anf_45bind48286 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48123, %struct.ScmObj* %ae50922)
store volatile %struct.ScmObj* %anf_45bind48286, %struct.ScmObj** %stackaddr$prim58075, align 8
%ae50924 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim58076 = alloca %struct.ScmObj*, align 8
%anf_45bind48287 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind48286, %struct.ScmObj* %ae50924)
store volatile %struct.ScmObj* %anf_45bind48287, %struct.ScmObj** %stackaddr$prim58076, align 8
%ae50926 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58077 = alloca %struct.ScmObj*, align 8
%_95148126 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n48123, %struct.ScmObj* %ae50926, %struct.ScmObj* %anf_45bind48287)
store volatile %struct.ScmObj* %_95148126, %struct.ScmObj** %stackaddr$prim58077, align 8
%argslist57046$cc481240 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58078 = alloca %struct.ScmObj*, align 8
%argslist57046$cc481241 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48124, %struct.ScmObj* %argslist57046$cc481240)
store volatile %struct.ScmObj* %argslist57046$cc481241, %struct.ScmObj** %stackaddr$prim58078, align 8
%stackaddr$prim58079 = alloca %struct.ScmObj*, align 8
%argslist57046$cc481242 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48377, %struct.ScmObj* %argslist57046$cc481241)
store volatile %struct.ScmObj* %argslist57046$cc481242, %struct.ScmObj** %stackaddr$prim58079, align 8
%clofunc58080 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48124)
musttail call tailcc void %clofunc58080(%struct.ScmObj* %cc48124, %struct.ScmObj* %argslist57046$cc481242)
ret void
}

define tailcc void @proc_clo$ae50756(%struct.ScmObj* %env$ae50756,%struct.ScmObj* %current_45args57047) {
%stackaddr$env-ref58081 = alloca %struct.ScmObj*, align 8
%k48377 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50756, i64 0)
store %struct.ScmObj* %k48377, %struct.ScmObj** %stackaddr$env-ref58081
%stackaddr$env-ref58082 = alloca %struct.ScmObj*, align 8
%n48123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50756, i64 1)
store %struct.ScmObj* %n48123, %struct.ScmObj** %stackaddr$env-ref58082
%stackaddr$env-ref58083 = alloca %struct.ScmObj*, align 8
%lst48122 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50756, i64 2)
store %struct.ScmObj* %lst48122, %struct.ScmObj** %stackaddr$env-ref58083
%stackaddr$prim58084 = alloca %struct.ScmObj*, align 8
%_95k48379 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57047)
store volatile %struct.ScmObj* %_95k48379, %struct.ScmObj** %stackaddr$prim58084, align 8
%stackaddr$prim58085 = alloca %struct.ScmObj*, align 8
%current_45args57048 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57047)
store volatile %struct.ScmObj* %current_45args57048, %struct.ScmObj** %stackaddr$prim58085, align 8
%stackaddr$prim58086 = alloca %struct.ScmObj*, align 8
%cc48124 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57048)
store volatile %struct.ScmObj* %cc48124, %struct.ScmObj** %stackaddr$prim58086, align 8
%ae50758 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58087 = alloca %struct.ScmObj*, align 8
%anf_45bind48282 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48123, %struct.ScmObj* %ae50758)
store volatile %struct.ScmObj* %anf_45bind48282, %struct.ScmObj** %stackaddr$prim58087, align 8
%ae50759 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58088 = alloca %struct.ScmObj*, align 8
%anf_45bind48283 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae50759, %struct.ScmObj* %anf_45bind48282)
store volatile %struct.ScmObj* %anf_45bind48283, %struct.ScmObj** %stackaddr$prim58088, align 8
%truthy$cmp58089 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48283)
%cmp$cmp58089 = icmp eq i64 %truthy$cmp58089, 1
br i1 %cmp$cmp58089, label %truebranch$cmp58089, label %falsebranch$cmp58089
truebranch$cmp58089:
%ae50763 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58090 = alloca %struct.ScmObj*, align 8
%cpsprim48380 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48122, %struct.ScmObj* %ae50763)
store volatile %struct.ScmObj* %cpsprim48380, %struct.ScmObj** %stackaddr$prim58090, align 8
%ae50765 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57050$k483770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58091 = alloca %struct.ScmObj*, align 8
%argslist57050$k483771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48380, %struct.ScmObj* %argslist57050$k483770)
store volatile %struct.ScmObj* %argslist57050$k483771, %struct.ScmObj** %stackaddr$prim58091, align 8
%stackaddr$prim58092 = alloca %struct.ScmObj*, align 8
%argslist57050$k483772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50765, %struct.ScmObj* %argslist57050$k483771)
store volatile %struct.ScmObj* %argslist57050$k483772, %struct.ScmObj** %stackaddr$prim58092, align 8
%clofunc58093 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48377)
musttail call tailcc void %clofunc58093(%struct.ScmObj* %k48377, %struct.ScmObj* %argslist57050$k483772)
ret void
falsebranch$cmp58089:
%ae50776 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58094 = alloca %struct.ScmObj*, align 8
%anf_45bind48284 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48122, %struct.ScmObj* %ae50776)
store volatile %struct.ScmObj* %anf_45bind48284, %struct.ScmObj** %stackaddr$prim58094, align 8
%stackaddr$prim58095 = alloca %struct.ScmObj*, align 8
%anf_45bind48285 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48284)
store volatile %struct.ScmObj* %anf_45bind48285, %struct.ScmObj** %stackaddr$prim58095, align 8
%ae50779 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58096 = alloca %struct.ScmObj*, align 8
%_95048127 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48122, %struct.ScmObj* %ae50779, %struct.ScmObj* %anf_45bind48285)
store volatile %struct.ScmObj* %_95048127, %struct.ScmObj** %stackaddr$prim58096, align 8
%ae50782 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58097 = alloca %struct.ScmObj*, align 8
%anf_45bind48286 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48123, %struct.ScmObj* %ae50782)
store volatile %struct.ScmObj* %anf_45bind48286, %struct.ScmObj** %stackaddr$prim58097, align 8
%ae50784 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim58098 = alloca %struct.ScmObj*, align 8
%anf_45bind48287 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind48286, %struct.ScmObj* %ae50784)
store volatile %struct.ScmObj* %anf_45bind48287, %struct.ScmObj** %stackaddr$prim58098, align 8
%ae50786 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58099 = alloca %struct.ScmObj*, align 8
%_95148126 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n48123, %struct.ScmObj* %ae50786, %struct.ScmObj* %anf_45bind48287)
store volatile %struct.ScmObj* %_95148126, %struct.ScmObj** %stackaddr$prim58099, align 8
%argslist57051$cc481240 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58100 = alloca %struct.ScmObj*, align 8
%argslist57051$cc481241 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48124, %struct.ScmObj* %argslist57051$cc481240)
store volatile %struct.ScmObj* %argslist57051$cc481241, %struct.ScmObj** %stackaddr$prim58100, align 8
%stackaddr$prim58101 = alloca %struct.ScmObj*, align 8
%argslist57051$cc481242 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48377, %struct.ScmObj* %argslist57051$cc481241)
store volatile %struct.ScmObj* %argslist57051$cc481242, %struct.ScmObj** %stackaddr$prim58101, align 8
%clofunc58102 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48124)
musttail call tailcc void %clofunc58102(%struct.ScmObj* %cc48124, %struct.ScmObj* %argslist57051$cc481242)
ret void
}

define tailcc void @proc_clo$ae50741(%struct.ScmObj* %env$ae50741,%struct.ScmObj* %current_45args57053) {
%stackaddr$prim58103 = alloca %struct.ScmObj*, align 8
%k48381 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57053)
store volatile %struct.ScmObj* %k48381, %struct.ScmObj** %stackaddr$prim58103, align 8
%stackaddr$prim58104 = alloca %struct.ScmObj*, align 8
%current_45args57054 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57053)
store volatile %struct.ScmObj* %current_45args57054, %struct.ScmObj** %stackaddr$prim58104, align 8
%stackaddr$prim58105 = alloca %struct.ScmObj*, align 8
%u48125 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57054)
store volatile %struct.ScmObj* %u48125, %struct.ScmObj** %stackaddr$prim58105, align 8
%argslist57056$u481250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58106 = alloca %struct.ScmObj*, align 8
%argslist57056$u481251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u48125, %struct.ScmObj* %argslist57056$u481250)
store volatile %struct.ScmObj* %argslist57056$u481251, %struct.ScmObj** %stackaddr$prim58106, align 8
%stackaddr$prim58107 = alloca %struct.ScmObj*, align 8
%argslist57056$u481252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48381, %struct.ScmObj* %argslist57056$u481251)
store volatile %struct.ScmObj* %argslist57056$u481252, %struct.ScmObj** %stackaddr$prim58107, align 8
%clofunc58108 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u48125)
musttail call tailcc void %clofunc58108(%struct.ScmObj* %u48125, %struct.ScmObj* %argslist57056$u481252)
ret void
}

define tailcc void @proc_clo$ae50318(%struct.ScmObj* %env$ae50318,%struct.ScmObj* %current_45args57059) {
%stackaddr$prim58109 = alloca %struct.ScmObj*, align 8
%k48382 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57059)
store volatile %struct.ScmObj* %k48382, %struct.ScmObj** %stackaddr$prim58109, align 8
%stackaddr$prim58110 = alloca %struct.ScmObj*, align 8
%current_45args57060 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57059)
store volatile %struct.ScmObj* %current_45args57060, %struct.ScmObj** %stackaddr$prim58110, align 8
%stackaddr$prim58111 = alloca %struct.ScmObj*, align 8
%a48129 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57060)
store volatile %struct.ScmObj* %a48129, %struct.ScmObj** %stackaddr$prim58111, align 8
%ae50319 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim58112 = alloca %struct.ScmObj*, align 8
%a48130 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50319, %struct.ScmObj* %a48129)
store volatile %struct.ScmObj* %a48130, %struct.ScmObj** %stackaddr$prim58112, align 8
%stackaddr$makeclosure58113 = alloca %struct.ScmObj*, align 8
%fptrToInt58114 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50321 to i64
%ae50321 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58114)
store volatile %struct.ScmObj* %ae50321, %struct.ScmObj** %stackaddr$makeclosure58113, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50321, %struct.ScmObj* %a48130, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50321, %struct.ScmObj* %k48382, i64 1)
%ae50322 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58115 = alloca %struct.ScmObj*, align 8
%fptrToInt58116 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50323 to i64
%ae50323 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58116)
store volatile %struct.ScmObj* %ae50323, %struct.ScmObj** %stackaddr$makeclosure58115, align 8
%argslist57082$ae503210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58117 = alloca %struct.ScmObj*, align 8
%argslist57082$ae503211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50323, %struct.ScmObj* %argslist57082$ae503210)
store volatile %struct.ScmObj* %argslist57082$ae503211, %struct.ScmObj** %stackaddr$prim58117, align 8
%stackaddr$prim58118 = alloca %struct.ScmObj*, align 8
%argslist57082$ae503212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50322, %struct.ScmObj* %argslist57082$ae503211)
store volatile %struct.ScmObj* %argslist57082$ae503212, %struct.ScmObj** %stackaddr$prim58118, align 8
%clofunc58119 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50321)
musttail call tailcc void %clofunc58119(%struct.ScmObj* %ae50321, %struct.ScmObj* %argslist57082$ae503212)
ret void
}

define tailcc void @proc_clo$ae50321(%struct.ScmObj* %env$ae50321,%struct.ScmObj* %current_45args57062) {
%stackaddr$env-ref58120 = alloca %struct.ScmObj*, align 8
%a48130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50321, i64 0)
store %struct.ScmObj* %a48130, %struct.ScmObj** %stackaddr$env-ref58120
%stackaddr$env-ref58121 = alloca %struct.ScmObj*, align 8
%k48382 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50321, i64 1)
store %struct.ScmObj* %k48382, %struct.ScmObj** %stackaddr$env-ref58121
%stackaddr$prim58122 = alloca %struct.ScmObj*, align 8
%_95k48383 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57062)
store volatile %struct.ScmObj* %_95k48383, %struct.ScmObj** %stackaddr$prim58122, align 8
%stackaddr$prim58123 = alloca %struct.ScmObj*, align 8
%current_45args57063 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57062)
store volatile %struct.ScmObj* %current_45args57063, %struct.ScmObj** %stackaddr$prim58123, align 8
%stackaddr$prim58124 = alloca %struct.ScmObj*, align 8
%anf_45bind48273 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57063)
store volatile %struct.ScmObj* %anf_45bind48273, %struct.ScmObj** %stackaddr$prim58124, align 8
%stackaddr$makeclosure58125 = alloca %struct.ScmObj*, align 8
%fptrToInt58126 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50340 to i64
%ae50340 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58126)
store volatile %struct.ScmObj* %ae50340, %struct.ScmObj** %stackaddr$makeclosure58125, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50340, %struct.ScmObj* %a48130, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50340, %struct.ScmObj* %k48382, i64 1)
%stackaddr$makeclosure58127 = alloca %struct.ScmObj*, align 8
%fptrToInt58128 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50341 to i64
%ae50341 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58128)
store volatile %struct.ScmObj* %ae50341, %struct.ScmObj** %stackaddr$makeclosure58127, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50341, %struct.ScmObj* %a48130, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50341, %struct.ScmObj* %k48382, i64 1)
%argslist57077$anf_45bind482730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58129 = alloca %struct.ScmObj*, align 8
%argslist57077$anf_45bind482731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50341, %struct.ScmObj* %argslist57077$anf_45bind482730)
store volatile %struct.ScmObj* %argslist57077$anf_45bind482731, %struct.ScmObj** %stackaddr$prim58129, align 8
%stackaddr$prim58130 = alloca %struct.ScmObj*, align 8
%argslist57077$anf_45bind482732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50340, %struct.ScmObj* %argslist57077$anf_45bind482731)
store volatile %struct.ScmObj* %argslist57077$anf_45bind482732, %struct.ScmObj** %stackaddr$prim58130, align 8
%clofunc58131 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48273)
musttail call tailcc void %clofunc58131(%struct.ScmObj* %anf_45bind48273, %struct.ScmObj* %argslist57077$anf_45bind482732)
ret void
}

define tailcc void @proc_clo$ae50340(%struct.ScmObj* %env$ae50340,%struct.ScmObj* %current_45args57065) {
%stackaddr$env-ref58132 = alloca %struct.ScmObj*, align 8
%a48130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50340, i64 0)
store %struct.ScmObj* %a48130, %struct.ScmObj** %stackaddr$env-ref58132
%stackaddr$env-ref58133 = alloca %struct.ScmObj*, align 8
%k48382 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50340, i64 1)
store %struct.ScmObj* %k48382, %struct.ScmObj** %stackaddr$env-ref58133
%stackaddr$prim58134 = alloca %struct.ScmObj*, align 8
%_95k48384 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57065)
store volatile %struct.ScmObj* %_95k48384, %struct.ScmObj** %stackaddr$prim58134, align 8
%stackaddr$prim58135 = alloca %struct.ScmObj*, align 8
%current_45args57066 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57065)
store volatile %struct.ScmObj* %current_45args57066, %struct.ScmObj** %stackaddr$prim58135, align 8
%stackaddr$prim58136 = alloca %struct.ScmObj*, align 8
%cc48131 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57066)
store volatile %struct.ScmObj* %cc48131, %struct.ScmObj** %stackaddr$prim58136, align 8
%ae50456 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58137 = alloca %struct.ScmObj*, align 8
%anf_45bind48274 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48130, %struct.ScmObj* %ae50456)
store volatile %struct.ScmObj* %anf_45bind48274, %struct.ScmObj** %stackaddr$prim58137, align 8
%stackaddr$prim58138 = alloca %struct.ScmObj*, align 8
%anf_45bind48275 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48274)
store volatile %struct.ScmObj* %anf_45bind48275, %struct.ScmObj** %stackaddr$prim58138, align 8
%truthy$cmp58139 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48275)
%cmp$cmp58139 = icmp eq i64 %truthy$cmp58139, 1
br i1 %cmp$cmp58139, label %truebranch$cmp58139, label %falsebranch$cmp58139
truebranch$cmp58139:
%ae50460 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50461 = call %struct.ScmObj* @const_init_true()
%argslist57068$k483820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58140 = alloca %struct.ScmObj*, align 8
%argslist57068$k483821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50461, %struct.ScmObj* %argslist57068$k483820)
store volatile %struct.ScmObj* %argslist57068$k483821, %struct.ScmObj** %stackaddr$prim58140, align 8
%stackaddr$prim58141 = alloca %struct.ScmObj*, align 8
%argslist57068$k483822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50460, %struct.ScmObj* %argslist57068$k483821)
store volatile %struct.ScmObj* %argslist57068$k483822, %struct.ScmObj** %stackaddr$prim58141, align 8
%clofunc58142 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48382)
musttail call tailcc void %clofunc58142(%struct.ScmObj* %k48382, %struct.ScmObj* %argslist57068$k483822)
ret void
falsebranch$cmp58139:
%ae50469 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58143 = alloca %struct.ScmObj*, align 8
%anf_45bind48276 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48130, %struct.ScmObj* %ae50469)
store volatile %struct.ScmObj* %anf_45bind48276, %struct.ScmObj** %stackaddr$prim58143, align 8
%stackaddr$prim58144 = alloca %struct.ScmObj*, align 8
%anf_45bind48277 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind48276)
store volatile %struct.ScmObj* %anf_45bind48277, %struct.ScmObj** %stackaddr$prim58144, align 8
%truthy$cmp58145 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48277)
%cmp$cmp58145 = icmp eq i64 %truthy$cmp58145, 1
br i1 %cmp$cmp58145, label %truebranch$cmp58145, label %falsebranch$cmp58145
truebranch$cmp58145:
%ae50473 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58146 = alloca %struct.ScmObj*, align 8
%anf_45bind48278 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48130, %struct.ScmObj* %ae50473)
store volatile %struct.ScmObj* %anf_45bind48278, %struct.ScmObj** %stackaddr$prim58146, align 8
%stackaddr$prim58147 = alloca %struct.ScmObj*, align 8
%b48133 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48278)
store volatile %struct.ScmObj* %b48133, %struct.ScmObj** %stackaddr$prim58147, align 8
%ae50476 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58148 = alloca %struct.ScmObj*, align 8
%anf_45bind48279 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48130, %struct.ScmObj* %ae50476)
store volatile %struct.ScmObj* %anf_45bind48279, %struct.ScmObj** %stackaddr$prim58148, align 8
%stackaddr$prim58149 = alloca %struct.ScmObj*, align 8
%anf_45bind48280 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48279)
store volatile %struct.ScmObj* %anf_45bind48280, %struct.ScmObj** %stackaddr$prim58149, align 8
%ae50479 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58150 = alloca %struct.ScmObj*, align 8
%_95048134 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a48130, %struct.ScmObj* %ae50479, %struct.ScmObj* %anf_45bind48280)
store volatile %struct.ScmObj* %_95048134, %struct.ScmObj** %stackaddr$prim58150, align 8
%argslist57069$cc481310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58151 = alloca %struct.ScmObj*, align 8
%argslist57069$cc481311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48131, %struct.ScmObj* %argslist57069$cc481310)
store volatile %struct.ScmObj* %argslist57069$cc481311, %struct.ScmObj** %stackaddr$prim58151, align 8
%stackaddr$prim58152 = alloca %struct.ScmObj*, align 8
%argslist57069$cc481312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48382, %struct.ScmObj* %argslist57069$cc481311)
store volatile %struct.ScmObj* %argslist57069$cc481312, %struct.ScmObj** %stackaddr$prim58152, align 8
%clofunc58153 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48131)
musttail call tailcc void %clofunc58153(%struct.ScmObj* %cc48131, %struct.ScmObj* %argslist57069$cc481312)
ret void
falsebranch$cmp58145:
%ae50512 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50513 = call %struct.ScmObj* @const_init_false()
%argslist57070$k483820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58154 = alloca %struct.ScmObj*, align 8
%argslist57070$k483821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50513, %struct.ScmObj* %argslist57070$k483820)
store volatile %struct.ScmObj* %argslist57070$k483821, %struct.ScmObj** %stackaddr$prim58154, align 8
%stackaddr$prim58155 = alloca %struct.ScmObj*, align 8
%argslist57070$k483822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50512, %struct.ScmObj* %argslist57070$k483821)
store volatile %struct.ScmObj* %argslist57070$k483822, %struct.ScmObj** %stackaddr$prim58155, align 8
%clofunc58156 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48382)
musttail call tailcc void %clofunc58156(%struct.ScmObj* %k48382, %struct.ScmObj* %argslist57070$k483822)
ret void
}

define tailcc void @proc_clo$ae50341(%struct.ScmObj* %env$ae50341,%struct.ScmObj* %current_45args57071) {
%stackaddr$env-ref58157 = alloca %struct.ScmObj*, align 8
%a48130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50341, i64 0)
store %struct.ScmObj* %a48130, %struct.ScmObj** %stackaddr$env-ref58157
%stackaddr$env-ref58158 = alloca %struct.ScmObj*, align 8
%k48382 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50341, i64 1)
store %struct.ScmObj* %k48382, %struct.ScmObj** %stackaddr$env-ref58158
%stackaddr$prim58159 = alloca %struct.ScmObj*, align 8
%_95k48384 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57071)
store volatile %struct.ScmObj* %_95k48384, %struct.ScmObj** %stackaddr$prim58159, align 8
%stackaddr$prim58160 = alloca %struct.ScmObj*, align 8
%current_45args57072 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57071)
store volatile %struct.ScmObj* %current_45args57072, %struct.ScmObj** %stackaddr$prim58160, align 8
%stackaddr$prim58161 = alloca %struct.ScmObj*, align 8
%cc48131 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57072)
store volatile %struct.ScmObj* %cc48131, %struct.ScmObj** %stackaddr$prim58161, align 8
%ae50343 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58162 = alloca %struct.ScmObj*, align 8
%anf_45bind48274 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48130, %struct.ScmObj* %ae50343)
store volatile %struct.ScmObj* %anf_45bind48274, %struct.ScmObj** %stackaddr$prim58162, align 8
%stackaddr$prim58163 = alloca %struct.ScmObj*, align 8
%anf_45bind48275 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48274)
store volatile %struct.ScmObj* %anf_45bind48275, %struct.ScmObj** %stackaddr$prim58163, align 8
%truthy$cmp58164 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48275)
%cmp$cmp58164 = icmp eq i64 %truthy$cmp58164, 1
br i1 %cmp$cmp58164, label %truebranch$cmp58164, label %falsebranch$cmp58164
truebranch$cmp58164:
%ae50347 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50348 = call %struct.ScmObj* @const_init_true()
%argslist57074$k483820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58165 = alloca %struct.ScmObj*, align 8
%argslist57074$k483821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50348, %struct.ScmObj* %argslist57074$k483820)
store volatile %struct.ScmObj* %argslist57074$k483821, %struct.ScmObj** %stackaddr$prim58165, align 8
%stackaddr$prim58166 = alloca %struct.ScmObj*, align 8
%argslist57074$k483822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50347, %struct.ScmObj* %argslist57074$k483821)
store volatile %struct.ScmObj* %argslist57074$k483822, %struct.ScmObj** %stackaddr$prim58166, align 8
%clofunc58167 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48382)
musttail call tailcc void %clofunc58167(%struct.ScmObj* %k48382, %struct.ScmObj* %argslist57074$k483822)
ret void
falsebranch$cmp58164:
%ae50356 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58168 = alloca %struct.ScmObj*, align 8
%anf_45bind48276 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48130, %struct.ScmObj* %ae50356)
store volatile %struct.ScmObj* %anf_45bind48276, %struct.ScmObj** %stackaddr$prim58168, align 8
%stackaddr$prim58169 = alloca %struct.ScmObj*, align 8
%anf_45bind48277 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind48276)
store volatile %struct.ScmObj* %anf_45bind48277, %struct.ScmObj** %stackaddr$prim58169, align 8
%truthy$cmp58170 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48277)
%cmp$cmp58170 = icmp eq i64 %truthy$cmp58170, 1
br i1 %cmp$cmp58170, label %truebranch$cmp58170, label %falsebranch$cmp58170
truebranch$cmp58170:
%ae50360 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58171 = alloca %struct.ScmObj*, align 8
%anf_45bind48278 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48130, %struct.ScmObj* %ae50360)
store volatile %struct.ScmObj* %anf_45bind48278, %struct.ScmObj** %stackaddr$prim58171, align 8
%stackaddr$prim58172 = alloca %struct.ScmObj*, align 8
%b48133 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48278)
store volatile %struct.ScmObj* %b48133, %struct.ScmObj** %stackaddr$prim58172, align 8
%ae50363 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58173 = alloca %struct.ScmObj*, align 8
%anf_45bind48279 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48130, %struct.ScmObj* %ae50363)
store volatile %struct.ScmObj* %anf_45bind48279, %struct.ScmObj** %stackaddr$prim58173, align 8
%stackaddr$prim58174 = alloca %struct.ScmObj*, align 8
%anf_45bind48280 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48279)
store volatile %struct.ScmObj* %anf_45bind48280, %struct.ScmObj** %stackaddr$prim58174, align 8
%ae50366 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58175 = alloca %struct.ScmObj*, align 8
%_95048134 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a48130, %struct.ScmObj* %ae50366, %struct.ScmObj* %anf_45bind48280)
store volatile %struct.ScmObj* %_95048134, %struct.ScmObj** %stackaddr$prim58175, align 8
%argslist57075$cc481310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58176 = alloca %struct.ScmObj*, align 8
%argslist57075$cc481311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48131, %struct.ScmObj* %argslist57075$cc481310)
store volatile %struct.ScmObj* %argslist57075$cc481311, %struct.ScmObj** %stackaddr$prim58176, align 8
%stackaddr$prim58177 = alloca %struct.ScmObj*, align 8
%argslist57075$cc481312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48382, %struct.ScmObj* %argslist57075$cc481311)
store volatile %struct.ScmObj* %argslist57075$cc481312, %struct.ScmObj** %stackaddr$prim58177, align 8
%clofunc58178 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48131)
musttail call tailcc void %clofunc58178(%struct.ScmObj* %cc48131, %struct.ScmObj* %argslist57075$cc481312)
ret void
falsebranch$cmp58170:
%ae50399 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50400 = call %struct.ScmObj* @const_init_false()
%argslist57076$k483820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58179 = alloca %struct.ScmObj*, align 8
%argslist57076$k483821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50400, %struct.ScmObj* %argslist57076$k483820)
store volatile %struct.ScmObj* %argslist57076$k483821, %struct.ScmObj** %stackaddr$prim58179, align 8
%stackaddr$prim58180 = alloca %struct.ScmObj*, align 8
%argslist57076$k483822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50399, %struct.ScmObj* %argslist57076$k483821)
store volatile %struct.ScmObj* %argslist57076$k483822, %struct.ScmObj** %stackaddr$prim58180, align 8
%clofunc58181 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48382)
musttail call tailcc void %clofunc58181(%struct.ScmObj* %k48382, %struct.ScmObj* %argslist57076$k483822)
ret void
}

define tailcc void @proc_clo$ae50323(%struct.ScmObj* %env$ae50323,%struct.ScmObj* %current_45args57078) {
%stackaddr$prim58182 = alloca %struct.ScmObj*, align 8
%k48385 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57078)
store volatile %struct.ScmObj* %k48385, %struct.ScmObj** %stackaddr$prim58182, align 8
%stackaddr$prim58183 = alloca %struct.ScmObj*, align 8
%current_45args57079 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57078)
store volatile %struct.ScmObj* %current_45args57079, %struct.ScmObj** %stackaddr$prim58183, align 8
%stackaddr$prim58184 = alloca %struct.ScmObj*, align 8
%k48132 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57079)
store volatile %struct.ScmObj* %k48132, %struct.ScmObj** %stackaddr$prim58184, align 8
%ae50325 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57081$k483850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58185 = alloca %struct.ScmObj*, align 8
%argslist57081$k483851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48132, %struct.ScmObj* %argslist57081$k483850)
store volatile %struct.ScmObj* %argslist57081$k483851, %struct.ScmObj** %stackaddr$prim58185, align 8
%stackaddr$prim58186 = alloca %struct.ScmObj*, align 8
%argslist57081$k483852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50325, %struct.ScmObj* %argslist57081$k483851)
store volatile %struct.ScmObj* %argslist57081$k483852, %struct.ScmObj** %stackaddr$prim58186, align 8
%clofunc58187 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48385)
musttail call tailcc void %clofunc58187(%struct.ScmObj* %k48385, %struct.ScmObj* %argslist57081$k483852)
ret void
}

define tailcc void @proc_clo$ae50246(%struct.ScmObj* %env$ae50246,%struct.ScmObj* %current_45args57084) {
%stackaddr$env-ref58188 = alloca %struct.ScmObj*, align 8
%_37append48136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50246, i64 0)
store %struct.ScmObj* %_37append48136, %struct.ScmObj** %stackaddr$env-ref58188
%stackaddr$prim58189 = alloca %struct.ScmObj*, align 8
%k48386 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57084)
store volatile %struct.ScmObj* %k48386, %struct.ScmObj** %stackaddr$prim58189, align 8
%stackaddr$prim58190 = alloca %struct.ScmObj*, align 8
%current_45args57085 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57084)
store volatile %struct.ScmObj* %current_45args57085, %struct.ScmObj** %stackaddr$prim58190, align 8
%stackaddr$prim58191 = alloca %struct.ScmObj*, align 8
%ls048139 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57085)
store volatile %struct.ScmObj* %ls048139, %struct.ScmObj** %stackaddr$prim58191, align 8
%stackaddr$prim58192 = alloca %struct.ScmObj*, align 8
%current_45args57086 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57085)
store volatile %struct.ScmObj* %current_45args57086, %struct.ScmObj** %stackaddr$prim58192, align 8
%stackaddr$prim58193 = alloca %struct.ScmObj*, align 8
%ls148138 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57086)
store volatile %struct.ScmObj* %ls148138, %struct.ScmObj** %stackaddr$prim58193, align 8
%stackaddr$prim58194 = alloca %struct.ScmObj*, align 8
%anf_45bind48267 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls048139)
store volatile %struct.ScmObj* %anf_45bind48267, %struct.ScmObj** %stackaddr$prim58194, align 8
%truthy$cmp58195 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48267)
%cmp$cmp58195 = icmp eq i64 %truthy$cmp58195, 1
br i1 %cmp$cmp58195, label %truebranch$cmp58195, label %falsebranch$cmp58195
truebranch$cmp58195:
%ae50250 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57088$k483860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58196 = alloca %struct.ScmObj*, align 8
%argslist57088$k483861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls148138, %struct.ScmObj* %argslist57088$k483860)
store volatile %struct.ScmObj* %argslist57088$k483861, %struct.ScmObj** %stackaddr$prim58196, align 8
%stackaddr$prim58197 = alloca %struct.ScmObj*, align 8
%argslist57088$k483862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50250, %struct.ScmObj* %argslist57088$k483861)
store volatile %struct.ScmObj* %argslist57088$k483862, %struct.ScmObj** %stackaddr$prim58197, align 8
%clofunc58198 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48386)
musttail call tailcc void %clofunc58198(%struct.ScmObj* %k48386, %struct.ScmObj* %argslist57088$k483862)
ret void
falsebranch$cmp58195:
%stackaddr$prim58199 = alloca %struct.ScmObj*, align 8
%anf_45bind48268 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls048139)
store volatile %struct.ScmObj* %anf_45bind48268, %struct.ScmObj** %stackaddr$prim58199, align 8
%ae50257 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58200 = alloca %struct.ScmObj*, align 8
%anf_45bind48269 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append48136, %struct.ScmObj* %ae50257)
store volatile %struct.ScmObj* %anf_45bind48269, %struct.ScmObj** %stackaddr$prim58200, align 8
%stackaddr$prim58201 = alloca %struct.ScmObj*, align 8
%anf_45bind48270 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls048139)
store volatile %struct.ScmObj* %anf_45bind48270, %struct.ScmObj** %stackaddr$prim58201, align 8
%stackaddr$makeclosure58202 = alloca %struct.ScmObj*, align 8
%fptrToInt58203 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50260 to i64
%ae50260 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58203)
store volatile %struct.ScmObj* %ae50260, %struct.ScmObj** %stackaddr$makeclosure58202, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50260, %struct.ScmObj* %k48386, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50260, %struct.ScmObj* %anf_45bind48268, i64 1)
%argslist57093$anf_45bind482690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58204 = alloca %struct.ScmObj*, align 8
%argslist57093$anf_45bind482691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls148138, %struct.ScmObj* %argslist57093$anf_45bind482690)
store volatile %struct.ScmObj* %argslist57093$anf_45bind482691, %struct.ScmObj** %stackaddr$prim58204, align 8
%stackaddr$prim58205 = alloca %struct.ScmObj*, align 8
%argslist57093$anf_45bind482692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48270, %struct.ScmObj* %argslist57093$anf_45bind482691)
store volatile %struct.ScmObj* %argslist57093$anf_45bind482692, %struct.ScmObj** %stackaddr$prim58205, align 8
%stackaddr$prim58206 = alloca %struct.ScmObj*, align 8
%argslist57093$anf_45bind482693 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50260, %struct.ScmObj* %argslist57093$anf_45bind482692)
store volatile %struct.ScmObj* %argslist57093$anf_45bind482693, %struct.ScmObj** %stackaddr$prim58206, align 8
%clofunc58207 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48269)
musttail call tailcc void %clofunc58207(%struct.ScmObj* %anf_45bind48269, %struct.ScmObj* %argslist57093$anf_45bind482693)
ret void
}

define tailcc void @proc_clo$ae50260(%struct.ScmObj* %env$ae50260,%struct.ScmObj* %current_45args57089) {
%stackaddr$env-ref58208 = alloca %struct.ScmObj*, align 8
%k48386 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50260, i64 0)
store %struct.ScmObj* %k48386, %struct.ScmObj** %stackaddr$env-ref58208
%stackaddr$env-ref58209 = alloca %struct.ScmObj*, align 8
%anf_45bind48268 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50260, i64 1)
store %struct.ScmObj* %anf_45bind48268, %struct.ScmObj** %stackaddr$env-ref58209
%stackaddr$prim58210 = alloca %struct.ScmObj*, align 8
%_95k48387 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57089)
store volatile %struct.ScmObj* %_95k48387, %struct.ScmObj** %stackaddr$prim58210, align 8
%stackaddr$prim58211 = alloca %struct.ScmObj*, align 8
%current_45args57090 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57089)
store volatile %struct.ScmObj* %current_45args57090, %struct.ScmObj** %stackaddr$prim58211, align 8
%stackaddr$prim58212 = alloca %struct.ScmObj*, align 8
%anf_45bind48271 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57090)
store volatile %struct.ScmObj* %anf_45bind48271, %struct.ScmObj** %stackaddr$prim58212, align 8
%stackaddr$prim58213 = alloca %struct.ScmObj*, align 8
%cpsprim48388 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48268, %struct.ScmObj* %anf_45bind48271)
store volatile %struct.ScmObj* %cpsprim48388, %struct.ScmObj** %stackaddr$prim58213, align 8
%ae50266 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57092$k483860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58214 = alloca %struct.ScmObj*, align 8
%argslist57092$k483861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48388, %struct.ScmObj* %argslist57092$k483860)
store volatile %struct.ScmObj* %argslist57092$k483861, %struct.ScmObj** %stackaddr$prim58214, align 8
%stackaddr$prim58215 = alloca %struct.ScmObj*, align 8
%argslist57092$k483862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50266, %struct.ScmObj* %argslist57092$k483861)
store volatile %struct.ScmObj* %argslist57092$k483862, %struct.ScmObj** %stackaddr$prim58215, align 8
%clofunc58216 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48386)
musttail call tailcc void %clofunc58216(%struct.ScmObj* %k48386, %struct.ScmObj* %argslist57092$k483862)
ret void
}

define tailcc void @proc_clo$ae50220(%struct.ScmObj* %env$ae50220,%struct.ScmObj* %current_45args57095) {
%stackaddr$prim58217 = alloca %struct.ScmObj*, align 8
%k48389 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57095)
store volatile %struct.ScmObj* %k48389, %struct.ScmObj** %stackaddr$prim58217, align 8
%stackaddr$prim58218 = alloca %struct.ScmObj*, align 8
%current_45args57096 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57095)
store volatile %struct.ScmObj* %current_45args57096, %struct.ScmObj** %stackaddr$prim58218, align 8
%stackaddr$prim58219 = alloca %struct.ScmObj*, align 8
%a48142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57096)
store volatile %struct.ScmObj* %a48142, %struct.ScmObj** %stackaddr$prim58219, align 8
%stackaddr$prim58220 = alloca %struct.ScmObj*, align 8
%current_45args57097 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57096)
store volatile %struct.ScmObj* %current_45args57097, %struct.ScmObj** %stackaddr$prim58220, align 8
%stackaddr$prim58221 = alloca %struct.ScmObj*, align 8
%b48141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57097)
store volatile %struct.ScmObj* %b48141, %struct.ScmObj** %stackaddr$prim58221, align 8
%stackaddr$prim58222 = alloca %struct.ScmObj*, align 8
%anf_45bind48266 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a48142, %struct.ScmObj* %b48141)
store volatile %struct.ScmObj* %anf_45bind48266, %struct.ScmObj** %stackaddr$prim58222, align 8
%stackaddr$prim58223 = alloca %struct.ScmObj*, align 8
%cpsprim48390 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind48266)
store volatile %struct.ScmObj* %cpsprim48390, %struct.ScmObj** %stackaddr$prim58223, align 8
%ae50225 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57099$k483890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58224 = alloca %struct.ScmObj*, align 8
%argslist57099$k483891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48390, %struct.ScmObj* %argslist57099$k483890)
store volatile %struct.ScmObj* %argslist57099$k483891, %struct.ScmObj** %stackaddr$prim58224, align 8
%stackaddr$prim58225 = alloca %struct.ScmObj*, align 8
%argslist57099$k483892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50225, %struct.ScmObj* %argslist57099$k483891)
store volatile %struct.ScmObj* %argslist57099$k483892, %struct.ScmObj** %stackaddr$prim58225, align 8
%clofunc58226 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48389)
musttail call tailcc void %clofunc58226(%struct.ScmObj* %k48389, %struct.ScmObj* %argslist57099$k483892)
ret void
}

define tailcc void @proc_clo$ae50196(%struct.ScmObj* %env$ae50196,%struct.ScmObj* %current_45args57101) {
%stackaddr$prim58227 = alloca %struct.ScmObj*, align 8
%k48391 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57101)
store volatile %struct.ScmObj* %k48391, %struct.ScmObj** %stackaddr$prim58227, align 8
%stackaddr$prim58228 = alloca %struct.ScmObj*, align 8
%current_45args57102 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57101)
store volatile %struct.ScmObj* %current_45args57102, %struct.ScmObj** %stackaddr$prim58228, align 8
%stackaddr$prim58229 = alloca %struct.ScmObj*, align 8
%a48145 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57102)
store volatile %struct.ScmObj* %a48145, %struct.ScmObj** %stackaddr$prim58229, align 8
%stackaddr$prim58230 = alloca %struct.ScmObj*, align 8
%current_45args57103 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57102)
store volatile %struct.ScmObj* %current_45args57103, %struct.ScmObj** %stackaddr$prim58230, align 8
%stackaddr$prim58231 = alloca %struct.ScmObj*, align 8
%b48144 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57103)
store volatile %struct.ScmObj* %b48144, %struct.ScmObj** %stackaddr$prim58231, align 8
%stackaddr$prim58232 = alloca %struct.ScmObj*, align 8
%anf_45bind48265 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a48145, %struct.ScmObj* %b48144)
store volatile %struct.ScmObj* %anf_45bind48265, %struct.ScmObj** %stackaddr$prim58232, align 8
%stackaddr$prim58233 = alloca %struct.ScmObj*, align 8
%cpsprim48392 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind48265)
store volatile %struct.ScmObj* %cpsprim48392, %struct.ScmObj** %stackaddr$prim58233, align 8
%ae50201 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57105$k483910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58234 = alloca %struct.ScmObj*, align 8
%argslist57105$k483911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48392, %struct.ScmObj* %argslist57105$k483910)
store volatile %struct.ScmObj* %argslist57105$k483911, %struct.ScmObj** %stackaddr$prim58234, align 8
%stackaddr$prim58235 = alloca %struct.ScmObj*, align 8
%argslist57105$k483912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50201, %struct.ScmObj* %argslist57105$k483911)
store volatile %struct.ScmObj* %argslist57105$k483912, %struct.ScmObj** %stackaddr$prim58235, align 8
%clofunc58236 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48391)
musttail call tailcc void %clofunc58236(%struct.ScmObj* %k48391, %struct.ScmObj* %argslist57105$k483912)
ret void
}

define tailcc void @proc_clo$ae49802(%struct.ScmObj* %env$ae49802,%struct.ScmObj* %current_45args57108) {
%stackaddr$env-ref58237 = alloca %struct.ScmObj*, align 8
%_37foldr48069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49802, i64 0)
store %struct.ScmObj* %_37foldr48069, %struct.ScmObj** %stackaddr$env-ref58237
%stackaddr$env-ref58238 = alloca %struct.ScmObj*, align 8
%_37foldr148064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49802, i64 1)
store %struct.ScmObj* %_37foldr148064, %struct.ScmObj** %stackaddr$env-ref58238
%stackaddr$env-ref58239 = alloca %struct.ScmObj*, align 8
%_37map148095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49802, i64 2)
store %struct.ScmObj* %_37map148095, %struct.ScmObj** %stackaddr$env-ref58239
%stackaddr$prim58240 = alloca %struct.ScmObj*, align 8
%k48393 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57108)
store volatile %struct.ScmObj* %k48393, %struct.ScmObj** %stackaddr$prim58240, align 8
%stackaddr$prim58241 = alloca %struct.ScmObj*, align 8
%current_45args57109 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57108)
store volatile %struct.ScmObj* %current_45args57109, %struct.ScmObj** %stackaddr$prim58241, align 8
%stackaddr$prim58242 = alloca %struct.ScmObj*, align 8
%_37foldl48147 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57109)
store volatile %struct.ScmObj* %_37foldl48147, %struct.ScmObj** %stackaddr$prim58242, align 8
%ae49804 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58243 = alloca %struct.ScmObj*, align 8
%fptrToInt58244 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49805 to i64
%ae49805 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt58244)
store volatile %struct.ScmObj* %ae49805, %struct.ScmObj** %stackaddr$makeclosure58243, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49805, %struct.ScmObj* %_37foldr48069, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49805, %struct.ScmObj* %_37foldl48147, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49805, %struct.ScmObj* %_37foldr148064, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49805, %struct.ScmObj* %_37map148095, i64 3)
%argslist57166$k483930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58245 = alloca %struct.ScmObj*, align 8
%argslist57166$k483931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49805, %struct.ScmObj* %argslist57166$k483930)
store volatile %struct.ScmObj* %argslist57166$k483931, %struct.ScmObj** %stackaddr$prim58245, align 8
%stackaddr$prim58246 = alloca %struct.ScmObj*, align 8
%argslist57166$k483932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49804, %struct.ScmObj* %argslist57166$k483931)
store volatile %struct.ScmObj* %argslist57166$k483932, %struct.ScmObj** %stackaddr$prim58246, align 8
%clofunc58247 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48393)
musttail call tailcc void %clofunc58247(%struct.ScmObj* %k48393, %struct.ScmObj* %argslist57166$k483932)
ret void
}

define tailcc void @proc_clo$ae49805(%struct.ScmObj* %env$ae49805,%struct.ScmObj* %args4814848394) {
%stackaddr$env-ref58248 = alloca %struct.ScmObj*, align 8
%_37foldr48069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49805, i64 0)
store %struct.ScmObj* %_37foldr48069, %struct.ScmObj** %stackaddr$env-ref58248
%stackaddr$env-ref58249 = alloca %struct.ScmObj*, align 8
%_37foldl48147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49805, i64 1)
store %struct.ScmObj* %_37foldl48147, %struct.ScmObj** %stackaddr$env-ref58249
%stackaddr$env-ref58250 = alloca %struct.ScmObj*, align 8
%_37foldr148064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49805, i64 2)
store %struct.ScmObj* %_37foldr148064, %struct.ScmObj** %stackaddr$env-ref58250
%stackaddr$env-ref58251 = alloca %struct.ScmObj*, align 8
%_37map148095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49805, i64 3)
store %struct.ScmObj* %_37map148095, %struct.ScmObj** %stackaddr$env-ref58251
%stackaddr$prim58252 = alloca %struct.ScmObj*, align 8
%k48395 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4814848394)
store volatile %struct.ScmObj* %k48395, %struct.ScmObj** %stackaddr$prim58252, align 8
%stackaddr$prim58253 = alloca %struct.ScmObj*, align 8
%args48148 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4814848394)
store volatile %struct.ScmObj* %args48148, %struct.ScmObj** %stackaddr$prim58253, align 8
%stackaddr$prim58254 = alloca %struct.ScmObj*, align 8
%f48151 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48148)
store volatile %struct.ScmObj* %f48151, %struct.ScmObj** %stackaddr$prim58254, align 8
%stackaddr$prim58255 = alloca %struct.ScmObj*, align 8
%anf_45bind48253 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48148)
store volatile %struct.ScmObj* %anf_45bind48253, %struct.ScmObj** %stackaddr$prim58255, align 8
%stackaddr$prim58256 = alloca %struct.ScmObj*, align 8
%acc48150 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48253)
store volatile %struct.ScmObj* %acc48150, %struct.ScmObj** %stackaddr$prim58256, align 8
%stackaddr$prim58257 = alloca %struct.ScmObj*, align 8
%anf_45bind48254 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48148)
store volatile %struct.ScmObj* %anf_45bind48254, %struct.ScmObj** %stackaddr$prim58257, align 8
%stackaddr$prim58258 = alloca %struct.ScmObj*, align 8
%lsts48149 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48254)
store volatile %struct.ScmObj* %lsts48149, %struct.ScmObj** %stackaddr$prim58258, align 8
%stackaddr$makeclosure58259 = alloca %struct.ScmObj*, align 8
%fptrToInt58260 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49813 to i64
%ae49813 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt58260)
store volatile %struct.ScmObj* %ae49813, %struct.ScmObj** %stackaddr$makeclosure58259, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49813, %struct.ScmObj* %lsts48149, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49813, %struct.ScmObj* %_37foldr48069, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49813, %struct.ScmObj* %f48151, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49813, %struct.ScmObj* %acc48150, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49813, %struct.ScmObj* %_37foldl48147, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49813, %struct.ScmObj* %_37foldr148064, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49813, %struct.ScmObj* %_37map148095, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae49813, %struct.ScmObj* %k48395, i64 7)
%ae49814 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58261 = alloca %struct.ScmObj*, align 8
%fptrToInt58262 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49815 to i64
%ae49815 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58262)
store volatile %struct.ScmObj* %ae49815, %struct.ScmObj** %stackaddr$makeclosure58261, align 8
%argslist57165$ae498130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58263 = alloca %struct.ScmObj*, align 8
%argslist57165$ae498131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49815, %struct.ScmObj* %argslist57165$ae498130)
store volatile %struct.ScmObj* %argslist57165$ae498131, %struct.ScmObj** %stackaddr$prim58263, align 8
%stackaddr$prim58264 = alloca %struct.ScmObj*, align 8
%argslist57165$ae498132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49814, %struct.ScmObj* %argslist57165$ae498131)
store volatile %struct.ScmObj* %argslist57165$ae498132, %struct.ScmObj** %stackaddr$prim58264, align 8
%clofunc58265 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49813)
musttail call tailcc void %clofunc58265(%struct.ScmObj* %ae49813, %struct.ScmObj* %argslist57165$ae498132)
ret void
}

define tailcc void @proc_clo$ae49813(%struct.ScmObj* %env$ae49813,%struct.ScmObj* %current_45args57111) {
%stackaddr$env-ref58266 = alloca %struct.ScmObj*, align 8
%lsts48149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49813, i64 0)
store %struct.ScmObj* %lsts48149, %struct.ScmObj** %stackaddr$env-ref58266
%stackaddr$env-ref58267 = alloca %struct.ScmObj*, align 8
%_37foldr48069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49813, i64 1)
store %struct.ScmObj* %_37foldr48069, %struct.ScmObj** %stackaddr$env-ref58267
%stackaddr$env-ref58268 = alloca %struct.ScmObj*, align 8
%f48151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49813, i64 2)
store %struct.ScmObj* %f48151, %struct.ScmObj** %stackaddr$env-ref58268
%stackaddr$env-ref58269 = alloca %struct.ScmObj*, align 8
%acc48150 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49813, i64 3)
store %struct.ScmObj* %acc48150, %struct.ScmObj** %stackaddr$env-ref58269
%stackaddr$env-ref58270 = alloca %struct.ScmObj*, align 8
%_37foldl48147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49813, i64 4)
store %struct.ScmObj* %_37foldl48147, %struct.ScmObj** %stackaddr$env-ref58270
%stackaddr$env-ref58271 = alloca %struct.ScmObj*, align 8
%_37foldr148064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49813, i64 5)
store %struct.ScmObj* %_37foldr148064, %struct.ScmObj** %stackaddr$env-ref58271
%stackaddr$env-ref58272 = alloca %struct.ScmObj*, align 8
%_37map148095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49813, i64 6)
store %struct.ScmObj* %_37map148095, %struct.ScmObj** %stackaddr$env-ref58272
%stackaddr$env-ref58273 = alloca %struct.ScmObj*, align 8
%k48395 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49813, i64 7)
store %struct.ScmObj* %k48395, %struct.ScmObj** %stackaddr$env-ref58273
%stackaddr$prim58274 = alloca %struct.ScmObj*, align 8
%_95k48396 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57111)
store volatile %struct.ScmObj* %_95k48396, %struct.ScmObj** %stackaddr$prim58274, align 8
%stackaddr$prim58275 = alloca %struct.ScmObj*, align 8
%current_45args57112 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57111)
store volatile %struct.ScmObj* %current_45args57112, %struct.ScmObj** %stackaddr$prim58275, align 8
%stackaddr$prim58276 = alloca %struct.ScmObj*, align 8
%anf_45bind48255 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57112)
store volatile %struct.ScmObj* %anf_45bind48255, %struct.ScmObj** %stackaddr$prim58276, align 8
%stackaddr$makeclosure58277 = alloca %struct.ScmObj*, align 8
%fptrToInt58278 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49845 to i64
%ae49845 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt58278)
store volatile %struct.ScmObj* %ae49845, %struct.ScmObj** %stackaddr$makeclosure58277, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49845, %struct.ScmObj* %lsts48149, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49845, %struct.ScmObj* %_37foldr48069, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49845, %struct.ScmObj* %f48151, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49845, %struct.ScmObj* %acc48150, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49845, %struct.ScmObj* %_37foldl48147, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49845, %struct.ScmObj* %_37map148095, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49845, %struct.ScmObj* %k48395, i64 6)
%ae49847 = call %struct.ScmObj* @const_init_false()
%argslist57158$_37foldr1480640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58279 = alloca %struct.ScmObj*, align 8
%argslist57158$_37foldr1480641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48149, %struct.ScmObj* %argslist57158$_37foldr1480640)
store volatile %struct.ScmObj* %argslist57158$_37foldr1480641, %struct.ScmObj** %stackaddr$prim58279, align 8
%stackaddr$prim58280 = alloca %struct.ScmObj*, align 8
%argslist57158$_37foldr1480642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49847, %struct.ScmObj* %argslist57158$_37foldr1480641)
store volatile %struct.ScmObj* %argslist57158$_37foldr1480642, %struct.ScmObj** %stackaddr$prim58280, align 8
%stackaddr$prim58281 = alloca %struct.ScmObj*, align 8
%argslist57158$_37foldr1480643 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48255, %struct.ScmObj* %argslist57158$_37foldr1480642)
store volatile %struct.ScmObj* %argslist57158$_37foldr1480643, %struct.ScmObj** %stackaddr$prim58281, align 8
%stackaddr$prim58282 = alloca %struct.ScmObj*, align 8
%argslist57158$_37foldr1480644 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49845, %struct.ScmObj* %argslist57158$_37foldr1480643)
store volatile %struct.ScmObj* %argslist57158$_37foldr1480644, %struct.ScmObj** %stackaddr$prim58282, align 8
%clofunc58283 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148064)
musttail call tailcc void %clofunc58283(%struct.ScmObj* %_37foldr148064, %struct.ScmObj* %argslist57158$_37foldr1480644)
ret void
}

define tailcc void @proc_clo$ae49845(%struct.ScmObj* %env$ae49845,%struct.ScmObj* %current_45args57114) {
%stackaddr$env-ref58284 = alloca %struct.ScmObj*, align 8
%lsts48149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49845, i64 0)
store %struct.ScmObj* %lsts48149, %struct.ScmObj** %stackaddr$env-ref58284
%stackaddr$env-ref58285 = alloca %struct.ScmObj*, align 8
%_37foldr48069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49845, i64 1)
store %struct.ScmObj* %_37foldr48069, %struct.ScmObj** %stackaddr$env-ref58285
%stackaddr$env-ref58286 = alloca %struct.ScmObj*, align 8
%f48151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49845, i64 2)
store %struct.ScmObj* %f48151, %struct.ScmObj** %stackaddr$env-ref58286
%stackaddr$env-ref58287 = alloca %struct.ScmObj*, align 8
%acc48150 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49845, i64 3)
store %struct.ScmObj* %acc48150, %struct.ScmObj** %stackaddr$env-ref58287
%stackaddr$env-ref58288 = alloca %struct.ScmObj*, align 8
%_37foldl48147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49845, i64 4)
store %struct.ScmObj* %_37foldl48147, %struct.ScmObj** %stackaddr$env-ref58288
%stackaddr$env-ref58289 = alloca %struct.ScmObj*, align 8
%_37map148095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49845, i64 5)
store %struct.ScmObj* %_37map148095, %struct.ScmObj** %stackaddr$env-ref58289
%stackaddr$env-ref58290 = alloca %struct.ScmObj*, align 8
%k48395 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49845, i64 6)
store %struct.ScmObj* %k48395, %struct.ScmObj** %stackaddr$env-ref58290
%stackaddr$prim58291 = alloca %struct.ScmObj*, align 8
%_95k48397 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57114)
store volatile %struct.ScmObj* %_95k48397, %struct.ScmObj** %stackaddr$prim58291, align 8
%stackaddr$prim58292 = alloca %struct.ScmObj*, align 8
%current_45args57115 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57114)
store volatile %struct.ScmObj* %current_45args57115, %struct.ScmObj** %stackaddr$prim58292, align 8
%stackaddr$prim58293 = alloca %struct.ScmObj*, align 8
%anf_45bind48256 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57115)
store volatile %struct.ScmObj* %anf_45bind48256, %struct.ScmObj** %stackaddr$prim58293, align 8
%truthy$cmp58294 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48256)
%cmp$cmp58294 = icmp eq i64 %truthy$cmp58294, 1
br i1 %cmp$cmp58294, label %truebranch$cmp58294, label %falsebranch$cmp58294
truebranch$cmp58294:
%ae49856 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57117$k483950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58295 = alloca %struct.ScmObj*, align 8
%argslist57117$k483951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48150, %struct.ScmObj* %argslist57117$k483950)
store volatile %struct.ScmObj* %argslist57117$k483951, %struct.ScmObj** %stackaddr$prim58295, align 8
%stackaddr$prim58296 = alloca %struct.ScmObj*, align 8
%argslist57117$k483952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49856, %struct.ScmObj* %argslist57117$k483951)
store volatile %struct.ScmObj* %argslist57117$k483952, %struct.ScmObj** %stackaddr$prim58296, align 8
%clofunc58297 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48395)
musttail call tailcc void %clofunc58297(%struct.ScmObj* %k48395, %struct.ScmObj* %argslist57117$k483952)
ret void
falsebranch$cmp58294:
%stackaddr$makeclosure58298 = alloca %struct.ScmObj*, align 8
%fptrToInt58299 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49861 to i64
%ae49861 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt58299)
store volatile %struct.ScmObj* %ae49861, %struct.ScmObj** %stackaddr$makeclosure58298, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49861, %struct.ScmObj* %lsts48149, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49861, %struct.ScmObj* %_37foldr48069, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49861, %struct.ScmObj* %f48151, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49861, %struct.ScmObj* %acc48150, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49861, %struct.ScmObj* %_37foldl48147, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49861, %struct.ScmObj* %_37map148095, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49861, %struct.ScmObj* %k48395, i64 6)
%ae49862 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58300 = alloca %struct.ScmObj*, align 8
%fptrToInt58301 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49863 to i64
%ae49863 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58301)
store volatile %struct.ScmObj* %ae49863, %struct.ScmObj** %stackaddr$makeclosure58300, align 8
%argslist57157$ae498610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58302 = alloca %struct.ScmObj*, align 8
%argslist57157$ae498611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49863, %struct.ScmObj* %argslist57157$ae498610)
store volatile %struct.ScmObj* %argslist57157$ae498611, %struct.ScmObj** %stackaddr$prim58302, align 8
%stackaddr$prim58303 = alloca %struct.ScmObj*, align 8
%argslist57157$ae498612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49862, %struct.ScmObj* %argslist57157$ae498611)
store volatile %struct.ScmObj* %argslist57157$ae498612, %struct.ScmObj** %stackaddr$prim58303, align 8
%clofunc58304 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49861)
musttail call tailcc void %clofunc58304(%struct.ScmObj* %ae49861, %struct.ScmObj* %argslist57157$ae498612)
ret void
}

define tailcc void @proc_clo$ae49861(%struct.ScmObj* %env$ae49861,%struct.ScmObj* %current_45args57118) {
%stackaddr$env-ref58305 = alloca %struct.ScmObj*, align 8
%lsts48149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49861, i64 0)
store %struct.ScmObj* %lsts48149, %struct.ScmObj** %stackaddr$env-ref58305
%stackaddr$env-ref58306 = alloca %struct.ScmObj*, align 8
%_37foldr48069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49861, i64 1)
store %struct.ScmObj* %_37foldr48069, %struct.ScmObj** %stackaddr$env-ref58306
%stackaddr$env-ref58307 = alloca %struct.ScmObj*, align 8
%f48151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49861, i64 2)
store %struct.ScmObj* %f48151, %struct.ScmObj** %stackaddr$env-ref58307
%stackaddr$env-ref58308 = alloca %struct.ScmObj*, align 8
%acc48150 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49861, i64 3)
store %struct.ScmObj* %acc48150, %struct.ScmObj** %stackaddr$env-ref58308
%stackaddr$env-ref58309 = alloca %struct.ScmObj*, align 8
%_37foldl48147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49861, i64 4)
store %struct.ScmObj* %_37foldl48147, %struct.ScmObj** %stackaddr$env-ref58309
%stackaddr$env-ref58310 = alloca %struct.ScmObj*, align 8
%_37map148095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49861, i64 5)
store %struct.ScmObj* %_37map148095, %struct.ScmObj** %stackaddr$env-ref58310
%stackaddr$env-ref58311 = alloca %struct.ScmObj*, align 8
%k48395 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49861, i64 6)
store %struct.ScmObj* %k48395, %struct.ScmObj** %stackaddr$env-ref58311
%stackaddr$prim58312 = alloca %struct.ScmObj*, align 8
%_95k48398 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57118)
store volatile %struct.ScmObj* %_95k48398, %struct.ScmObj** %stackaddr$prim58312, align 8
%stackaddr$prim58313 = alloca %struct.ScmObj*, align 8
%current_45args57119 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57118)
store volatile %struct.ScmObj* %current_45args57119, %struct.ScmObj** %stackaddr$prim58313, align 8
%stackaddr$prim58314 = alloca %struct.ScmObj*, align 8
%anf_45bind48257 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57119)
store volatile %struct.ScmObj* %anf_45bind48257, %struct.ScmObj** %stackaddr$prim58314, align 8
%stackaddr$makeclosure58315 = alloca %struct.ScmObj*, align 8
%fptrToInt58316 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49882 to i64
%ae49882 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt58316)
store volatile %struct.ScmObj* %ae49882, %struct.ScmObj** %stackaddr$makeclosure58315, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49882, %struct.ScmObj* %lsts48149, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49882, %struct.ScmObj* %_37foldr48069, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49882, %struct.ScmObj* %f48151, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49882, %struct.ScmObj* %acc48150, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49882, %struct.ScmObj* %_37foldl48147, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49882, %struct.ScmObj* %_37map148095, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49882, %struct.ScmObj* %k48395, i64 6)
%argslist57152$_37map1480950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58317 = alloca %struct.ScmObj*, align 8
%argslist57152$_37map1480951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48149, %struct.ScmObj* %argslist57152$_37map1480950)
store volatile %struct.ScmObj* %argslist57152$_37map1480951, %struct.ScmObj** %stackaddr$prim58317, align 8
%stackaddr$prim58318 = alloca %struct.ScmObj*, align 8
%argslist57152$_37map1480952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48257, %struct.ScmObj* %argslist57152$_37map1480951)
store volatile %struct.ScmObj* %argslist57152$_37map1480952, %struct.ScmObj** %stackaddr$prim58318, align 8
%stackaddr$prim58319 = alloca %struct.ScmObj*, align 8
%argslist57152$_37map1480953 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49882, %struct.ScmObj* %argslist57152$_37map1480952)
store volatile %struct.ScmObj* %argslist57152$_37map1480953, %struct.ScmObj** %stackaddr$prim58319, align 8
%clofunc58320 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148095)
musttail call tailcc void %clofunc58320(%struct.ScmObj* %_37map148095, %struct.ScmObj* %argslist57152$_37map1480953)
ret void
}

define tailcc void @proc_clo$ae49882(%struct.ScmObj* %env$ae49882,%struct.ScmObj* %current_45args57121) {
%stackaddr$env-ref58321 = alloca %struct.ScmObj*, align 8
%lsts48149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49882, i64 0)
store %struct.ScmObj* %lsts48149, %struct.ScmObj** %stackaddr$env-ref58321
%stackaddr$env-ref58322 = alloca %struct.ScmObj*, align 8
%_37foldr48069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49882, i64 1)
store %struct.ScmObj* %_37foldr48069, %struct.ScmObj** %stackaddr$env-ref58322
%stackaddr$env-ref58323 = alloca %struct.ScmObj*, align 8
%f48151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49882, i64 2)
store %struct.ScmObj* %f48151, %struct.ScmObj** %stackaddr$env-ref58323
%stackaddr$env-ref58324 = alloca %struct.ScmObj*, align 8
%acc48150 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49882, i64 3)
store %struct.ScmObj* %acc48150, %struct.ScmObj** %stackaddr$env-ref58324
%stackaddr$env-ref58325 = alloca %struct.ScmObj*, align 8
%_37foldl48147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49882, i64 4)
store %struct.ScmObj* %_37foldl48147, %struct.ScmObj** %stackaddr$env-ref58325
%stackaddr$env-ref58326 = alloca %struct.ScmObj*, align 8
%_37map148095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49882, i64 5)
store %struct.ScmObj* %_37map148095, %struct.ScmObj** %stackaddr$env-ref58326
%stackaddr$env-ref58327 = alloca %struct.ScmObj*, align 8
%k48395 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49882, i64 6)
store %struct.ScmObj* %k48395, %struct.ScmObj** %stackaddr$env-ref58327
%stackaddr$prim58328 = alloca %struct.ScmObj*, align 8
%_95k48399 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57121)
store volatile %struct.ScmObj* %_95k48399, %struct.ScmObj** %stackaddr$prim58328, align 8
%stackaddr$prim58329 = alloca %struct.ScmObj*, align 8
%current_45args57122 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57121)
store volatile %struct.ScmObj* %current_45args57122, %struct.ScmObj** %stackaddr$prim58329, align 8
%stackaddr$prim58330 = alloca %struct.ScmObj*, align 8
%lsts_4348156 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57122)
store volatile %struct.ScmObj* %lsts_4348156, %struct.ScmObj** %stackaddr$prim58330, align 8
%stackaddr$makeclosure58331 = alloca %struct.ScmObj*, align 8
%fptrToInt58332 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49885 to i64
%ae49885 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt58332)
store volatile %struct.ScmObj* %ae49885, %struct.ScmObj** %stackaddr$makeclosure58331, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49885, %struct.ScmObj* %lsts48149, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49885, %struct.ScmObj* %_37foldr48069, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49885, %struct.ScmObj* %f48151, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49885, %struct.ScmObj* %acc48150, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49885, %struct.ScmObj* %_37foldl48147, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49885, %struct.ScmObj* %_37map148095, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49885, %struct.ScmObj* %lsts_4348156, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae49885, %struct.ScmObj* %k48395, i64 7)
%ae49886 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58333 = alloca %struct.ScmObj*, align 8
%fptrToInt58334 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49887 to i64
%ae49887 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58334)
store volatile %struct.ScmObj* %ae49887, %struct.ScmObj** %stackaddr$makeclosure58333, align 8
%argslist57151$ae498850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58335 = alloca %struct.ScmObj*, align 8
%argslist57151$ae498851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49887, %struct.ScmObj* %argslist57151$ae498850)
store volatile %struct.ScmObj* %argslist57151$ae498851, %struct.ScmObj** %stackaddr$prim58335, align 8
%stackaddr$prim58336 = alloca %struct.ScmObj*, align 8
%argslist57151$ae498852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49886, %struct.ScmObj* %argslist57151$ae498851)
store volatile %struct.ScmObj* %argslist57151$ae498852, %struct.ScmObj** %stackaddr$prim58336, align 8
%clofunc58337 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49885)
musttail call tailcc void %clofunc58337(%struct.ScmObj* %ae49885, %struct.ScmObj* %argslist57151$ae498852)
ret void
}

define tailcc void @proc_clo$ae49885(%struct.ScmObj* %env$ae49885,%struct.ScmObj* %current_45args57124) {
%stackaddr$env-ref58338 = alloca %struct.ScmObj*, align 8
%lsts48149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49885, i64 0)
store %struct.ScmObj* %lsts48149, %struct.ScmObj** %stackaddr$env-ref58338
%stackaddr$env-ref58339 = alloca %struct.ScmObj*, align 8
%_37foldr48069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49885, i64 1)
store %struct.ScmObj* %_37foldr48069, %struct.ScmObj** %stackaddr$env-ref58339
%stackaddr$env-ref58340 = alloca %struct.ScmObj*, align 8
%f48151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49885, i64 2)
store %struct.ScmObj* %f48151, %struct.ScmObj** %stackaddr$env-ref58340
%stackaddr$env-ref58341 = alloca %struct.ScmObj*, align 8
%acc48150 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49885, i64 3)
store %struct.ScmObj* %acc48150, %struct.ScmObj** %stackaddr$env-ref58341
%stackaddr$env-ref58342 = alloca %struct.ScmObj*, align 8
%_37foldl48147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49885, i64 4)
store %struct.ScmObj* %_37foldl48147, %struct.ScmObj** %stackaddr$env-ref58342
%stackaddr$env-ref58343 = alloca %struct.ScmObj*, align 8
%_37map148095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49885, i64 5)
store %struct.ScmObj* %_37map148095, %struct.ScmObj** %stackaddr$env-ref58343
%stackaddr$env-ref58344 = alloca %struct.ScmObj*, align 8
%lsts_4348156 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49885, i64 6)
store %struct.ScmObj* %lsts_4348156, %struct.ScmObj** %stackaddr$env-ref58344
%stackaddr$env-ref58345 = alloca %struct.ScmObj*, align 8
%k48395 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49885, i64 7)
store %struct.ScmObj* %k48395, %struct.ScmObj** %stackaddr$env-ref58345
%stackaddr$prim58346 = alloca %struct.ScmObj*, align 8
%_95k48400 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57124)
store volatile %struct.ScmObj* %_95k48400, %struct.ScmObj** %stackaddr$prim58346, align 8
%stackaddr$prim58347 = alloca %struct.ScmObj*, align 8
%current_45args57125 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57124)
store volatile %struct.ScmObj* %current_45args57125, %struct.ScmObj** %stackaddr$prim58347, align 8
%stackaddr$prim58348 = alloca %struct.ScmObj*, align 8
%anf_45bind48258 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57125)
store volatile %struct.ScmObj* %anf_45bind48258, %struct.ScmObj** %stackaddr$prim58348, align 8
%stackaddr$makeclosure58349 = alloca %struct.ScmObj*, align 8
%fptrToInt58350 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49906 to i64
%ae49906 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt58350)
store volatile %struct.ScmObj* %ae49906, %struct.ScmObj** %stackaddr$makeclosure58349, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49906, %struct.ScmObj* %f48151, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49906, %struct.ScmObj* %acc48150, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49906, %struct.ScmObj* %_37foldr48069, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49906, %struct.ScmObj* %_37foldl48147, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49906, %struct.ScmObj* %lsts_4348156, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49906, %struct.ScmObj* %k48395, i64 5)
%argslist57146$_37map1480950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58351 = alloca %struct.ScmObj*, align 8
%argslist57146$_37map1480951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48149, %struct.ScmObj* %argslist57146$_37map1480950)
store volatile %struct.ScmObj* %argslist57146$_37map1480951, %struct.ScmObj** %stackaddr$prim58351, align 8
%stackaddr$prim58352 = alloca %struct.ScmObj*, align 8
%argslist57146$_37map1480952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48258, %struct.ScmObj* %argslist57146$_37map1480951)
store volatile %struct.ScmObj* %argslist57146$_37map1480952, %struct.ScmObj** %stackaddr$prim58352, align 8
%stackaddr$prim58353 = alloca %struct.ScmObj*, align 8
%argslist57146$_37map1480953 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49906, %struct.ScmObj* %argslist57146$_37map1480952)
store volatile %struct.ScmObj* %argslist57146$_37map1480953, %struct.ScmObj** %stackaddr$prim58353, align 8
%clofunc58354 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148095)
musttail call tailcc void %clofunc58354(%struct.ScmObj* %_37map148095, %struct.ScmObj* %argslist57146$_37map1480953)
ret void
}

define tailcc void @proc_clo$ae49906(%struct.ScmObj* %env$ae49906,%struct.ScmObj* %current_45args57127) {
%stackaddr$env-ref58355 = alloca %struct.ScmObj*, align 8
%f48151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49906, i64 0)
store %struct.ScmObj* %f48151, %struct.ScmObj** %stackaddr$env-ref58355
%stackaddr$env-ref58356 = alloca %struct.ScmObj*, align 8
%acc48150 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49906, i64 1)
store %struct.ScmObj* %acc48150, %struct.ScmObj** %stackaddr$env-ref58356
%stackaddr$env-ref58357 = alloca %struct.ScmObj*, align 8
%_37foldr48069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49906, i64 2)
store %struct.ScmObj* %_37foldr48069, %struct.ScmObj** %stackaddr$env-ref58357
%stackaddr$env-ref58358 = alloca %struct.ScmObj*, align 8
%_37foldl48147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49906, i64 3)
store %struct.ScmObj* %_37foldl48147, %struct.ScmObj** %stackaddr$env-ref58358
%stackaddr$env-ref58359 = alloca %struct.ScmObj*, align 8
%lsts_4348156 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49906, i64 4)
store %struct.ScmObj* %lsts_4348156, %struct.ScmObj** %stackaddr$env-ref58359
%stackaddr$env-ref58360 = alloca %struct.ScmObj*, align 8
%k48395 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49906, i64 5)
store %struct.ScmObj* %k48395, %struct.ScmObj** %stackaddr$env-ref58360
%stackaddr$prim58361 = alloca %struct.ScmObj*, align 8
%_95k48401 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57127)
store volatile %struct.ScmObj* %_95k48401, %struct.ScmObj** %stackaddr$prim58361, align 8
%stackaddr$prim58362 = alloca %struct.ScmObj*, align 8
%current_45args57128 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57127)
store volatile %struct.ScmObj* %current_45args57128, %struct.ScmObj** %stackaddr$prim58362, align 8
%stackaddr$prim58363 = alloca %struct.ScmObj*, align 8
%vs48154 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57128)
store volatile %struct.ScmObj* %vs48154, %struct.ScmObj** %stackaddr$prim58363, align 8
%stackaddr$makeclosure58364 = alloca %struct.ScmObj*, align 8
%fptrToInt58365 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49909 to i64
%ae49909 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt58365)
store volatile %struct.ScmObj* %ae49909, %struct.ScmObj** %stackaddr$makeclosure58364, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49909, %struct.ScmObj* %f48151, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49909, %struct.ScmObj* %acc48150, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49909, %struct.ScmObj* %_37foldr48069, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49909, %struct.ScmObj* %_37foldl48147, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49909, %struct.ScmObj* %lsts_4348156, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49909, %struct.ScmObj* %k48395, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49909, %struct.ScmObj* %vs48154, i64 6)
%ae49910 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58366 = alloca %struct.ScmObj*, align 8
%fptrToInt58367 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49911 to i64
%ae49911 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58367)
store volatile %struct.ScmObj* %ae49911, %struct.ScmObj** %stackaddr$makeclosure58366, align 8
%argslist57145$ae499090 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58368 = alloca %struct.ScmObj*, align 8
%argslist57145$ae499091 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49911, %struct.ScmObj* %argslist57145$ae499090)
store volatile %struct.ScmObj* %argslist57145$ae499091, %struct.ScmObj** %stackaddr$prim58368, align 8
%stackaddr$prim58369 = alloca %struct.ScmObj*, align 8
%argslist57145$ae499092 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49910, %struct.ScmObj* %argslist57145$ae499091)
store volatile %struct.ScmObj* %argslist57145$ae499092, %struct.ScmObj** %stackaddr$prim58369, align 8
%clofunc58370 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49909)
musttail call tailcc void %clofunc58370(%struct.ScmObj* %ae49909, %struct.ScmObj* %argslist57145$ae499092)
ret void
}

define tailcc void @proc_clo$ae49909(%struct.ScmObj* %env$ae49909,%struct.ScmObj* %current_45args57130) {
%stackaddr$env-ref58371 = alloca %struct.ScmObj*, align 8
%f48151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49909, i64 0)
store %struct.ScmObj* %f48151, %struct.ScmObj** %stackaddr$env-ref58371
%stackaddr$env-ref58372 = alloca %struct.ScmObj*, align 8
%acc48150 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49909, i64 1)
store %struct.ScmObj* %acc48150, %struct.ScmObj** %stackaddr$env-ref58372
%stackaddr$env-ref58373 = alloca %struct.ScmObj*, align 8
%_37foldr48069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49909, i64 2)
store %struct.ScmObj* %_37foldr48069, %struct.ScmObj** %stackaddr$env-ref58373
%stackaddr$env-ref58374 = alloca %struct.ScmObj*, align 8
%_37foldl48147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49909, i64 3)
store %struct.ScmObj* %_37foldl48147, %struct.ScmObj** %stackaddr$env-ref58374
%stackaddr$env-ref58375 = alloca %struct.ScmObj*, align 8
%lsts_4348156 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49909, i64 4)
store %struct.ScmObj* %lsts_4348156, %struct.ScmObj** %stackaddr$env-ref58375
%stackaddr$env-ref58376 = alloca %struct.ScmObj*, align 8
%k48395 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49909, i64 5)
store %struct.ScmObj* %k48395, %struct.ScmObj** %stackaddr$env-ref58376
%stackaddr$env-ref58377 = alloca %struct.ScmObj*, align 8
%vs48154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49909, i64 6)
store %struct.ScmObj* %vs48154, %struct.ScmObj** %stackaddr$env-ref58377
%stackaddr$prim58378 = alloca %struct.ScmObj*, align 8
%_95k48402 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57130)
store volatile %struct.ScmObj* %_95k48402, %struct.ScmObj** %stackaddr$prim58378, align 8
%stackaddr$prim58379 = alloca %struct.ScmObj*, align 8
%current_45args57131 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57130)
store volatile %struct.ScmObj* %current_45args57131, %struct.ScmObj** %stackaddr$prim58379, align 8
%stackaddr$prim58380 = alloca %struct.ScmObj*, align 8
%anf_45bind48259 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57131)
store volatile %struct.ScmObj* %anf_45bind48259, %struct.ScmObj** %stackaddr$prim58380, align 8
%ae49932 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58381 = alloca %struct.ScmObj*, align 8
%anf_45bind48260 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48150, %struct.ScmObj* %ae49932)
store volatile %struct.ScmObj* %anf_45bind48260, %struct.ScmObj** %stackaddr$prim58381, align 8
%stackaddr$makeclosure58382 = alloca %struct.ScmObj*, align 8
%fptrToInt58383 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49934 to i64
%ae49934 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt58383)
store volatile %struct.ScmObj* %ae49934, %struct.ScmObj** %stackaddr$makeclosure58382, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49934, %struct.ScmObj* %f48151, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49934, %struct.ScmObj* %_37foldl48147, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49934, %struct.ScmObj* %lsts_4348156, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49934, %struct.ScmObj* %k48395, i64 3)
%argslist57139$_37foldr480690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58384 = alloca %struct.ScmObj*, align 8
%argslist57139$_37foldr480691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs48154, %struct.ScmObj* %argslist57139$_37foldr480690)
store volatile %struct.ScmObj* %argslist57139$_37foldr480691, %struct.ScmObj** %stackaddr$prim58384, align 8
%stackaddr$prim58385 = alloca %struct.ScmObj*, align 8
%argslist57139$_37foldr480692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48260, %struct.ScmObj* %argslist57139$_37foldr480691)
store volatile %struct.ScmObj* %argslist57139$_37foldr480692, %struct.ScmObj** %stackaddr$prim58385, align 8
%stackaddr$prim58386 = alloca %struct.ScmObj*, align 8
%argslist57139$_37foldr480693 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48259, %struct.ScmObj* %argslist57139$_37foldr480692)
store volatile %struct.ScmObj* %argslist57139$_37foldr480693, %struct.ScmObj** %stackaddr$prim58386, align 8
%stackaddr$prim58387 = alloca %struct.ScmObj*, align 8
%argslist57139$_37foldr480694 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49934, %struct.ScmObj* %argslist57139$_37foldr480693)
store volatile %struct.ScmObj* %argslist57139$_37foldr480694, %struct.ScmObj** %stackaddr$prim58387, align 8
%clofunc58388 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr48069)
musttail call tailcc void %clofunc58388(%struct.ScmObj* %_37foldr48069, %struct.ScmObj* %argslist57139$_37foldr480694)
ret void
}

define tailcc void @proc_clo$ae49934(%struct.ScmObj* %env$ae49934,%struct.ScmObj* %current_45args57133) {
%stackaddr$env-ref58389 = alloca %struct.ScmObj*, align 8
%f48151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49934, i64 0)
store %struct.ScmObj* %f48151, %struct.ScmObj** %stackaddr$env-ref58389
%stackaddr$env-ref58390 = alloca %struct.ScmObj*, align 8
%_37foldl48147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49934, i64 1)
store %struct.ScmObj* %_37foldl48147, %struct.ScmObj** %stackaddr$env-ref58390
%stackaddr$env-ref58391 = alloca %struct.ScmObj*, align 8
%lsts_4348156 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49934, i64 2)
store %struct.ScmObj* %lsts_4348156, %struct.ScmObj** %stackaddr$env-ref58391
%stackaddr$env-ref58392 = alloca %struct.ScmObj*, align 8
%k48395 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49934, i64 3)
store %struct.ScmObj* %k48395, %struct.ScmObj** %stackaddr$env-ref58392
%stackaddr$prim58393 = alloca %struct.ScmObj*, align 8
%_95k48403 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57133)
store volatile %struct.ScmObj* %_95k48403, %struct.ScmObj** %stackaddr$prim58393, align 8
%stackaddr$prim58394 = alloca %struct.ScmObj*, align 8
%current_45args57134 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57133)
store volatile %struct.ScmObj* %current_45args57134, %struct.ScmObj** %stackaddr$prim58394, align 8
%stackaddr$prim58395 = alloca %struct.ScmObj*, align 8
%anf_45bind48261 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57134)
store volatile %struct.ScmObj* %anf_45bind48261, %struct.ScmObj** %stackaddr$prim58395, align 8
%stackaddr$makeclosure58396 = alloca %struct.ScmObj*, align 8
%fptrToInt58397 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49938 to i64
%ae49938 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt58397)
store volatile %struct.ScmObj* %ae49938, %struct.ScmObj** %stackaddr$makeclosure58396, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49938, %struct.ScmObj* %f48151, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49938, %struct.ScmObj* %_37foldl48147, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49938, %struct.ScmObj* %lsts_4348156, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49938, %struct.ScmObj* %k48395, i64 3)
%stackaddr$prim58398 = alloca %struct.ScmObj*, align 8
%cpsargs48406 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49938, %struct.ScmObj* %anf_45bind48261)
store volatile %struct.ScmObj* %cpsargs48406, %struct.ScmObj** %stackaddr$prim58398, align 8
%clofunc58399 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48151)
musttail call tailcc void %clofunc58399(%struct.ScmObj* %f48151, %struct.ScmObj* %cpsargs48406)
ret void
}

define tailcc void @proc_clo$ae49938(%struct.ScmObj* %env$ae49938,%struct.ScmObj* %current_45args57136) {
%stackaddr$env-ref58400 = alloca %struct.ScmObj*, align 8
%f48151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49938, i64 0)
store %struct.ScmObj* %f48151, %struct.ScmObj** %stackaddr$env-ref58400
%stackaddr$env-ref58401 = alloca %struct.ScmObj*, align 8
%_37foldl48147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49938, i64 1)
store %struct.ScmObj* %_37foldl48147, %struct.ScmObj** %stackaddr$env-ref58401
%stackaddr$env-ref58402 = alloca %struct.ScmObj*, align 8
%lsts_4348156 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49938, i64 2)
store %struct.ScmObj* %lsts_4348156, %struct.ScmObj** %stackaddr$env-ref58402
%stackaddr$env-ref58403 = alloca %struct.ScmObj*, align 8
%k48395 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49938, i64 3)
store %struct.ScmObj* %k48395, %struct.ScmObj** %stackaddr$env-ref58403
%stackaddr$prim58404 = alloca %struct.ScmObj*, align 8
%_95k48404 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57136)
store volatile %struct.ScmObj* %_95k48404, %struct.ScmObj** %stackaddr$prim58404, align 8
%stackaddr$prim58405 = alloca %struct.ScmObj*, align 8
%current_45args57137 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57136)
store volatile %struct.ScmObj* %current_45args57137, %struct.ScmObj** %stackaddr$prim58405, align 8
%stackaddr$prim58406 = alloca %struct.ScmObj*, align 8
%acc_4348158 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57137)
store volatile %struct.ScmObj* %acc_4348158, %struct.ScmObj** %stackaddr$prim58406, align 8
%stackaddr$prim58407 = alloca %struct.ScmObj*, align 8
%anf_45bind48262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4348158, %struct.ScmObj* %lsts_4348156)
store volatile %struct.ScmObj* %anf_45bind48262, %struct.ScmObj** %stackaddr$prim58407, align 8
%stackaddr$prim58408 = alloca %struct.ScmObj*, align 8
%anf_45bind48263 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48151, %struct.ScmObj* %anf_45bind48262)
store volatile %struct.ScmObj* %anf_45bind48263, %struct.ScmObj** %stackaddr$prim58408, align 8
%stackaddr$prim58409 = alloca %struct.ScmObj*, align 8
%cpsargs48405 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48395, %struct.ScmObj* %anf_45bind48263)
store volatile %struct.ScmObj* %cpsargs48405, %struct.ScmObj** %stackaddr$prim58409, align 8
%clofunc58410 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl48147)
musttail call tailcc void %clofunc58410(%struct.ScmObj* %_37foldl48147, %struct.ScmObj* %cpsargs48405)
ret void
}

define tailcc void @proc_clo$ae49911(%struct.ScmObj* %env$ae49911,%struct.ScmObj* %current_45args57140) {
%stackaddr$prim58411 = alloca %struct.ScmObj*, align 8
%k48407 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57140)
store volatile %struct.ScmObj* %k48407, %struct.ScmObj** %stackaddr$prim58411, align 8
%stackaddr$prim58412 = alloca %struct.ScmObj*, align 8
%current_45args57141 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57140)
store volatile %struct.ScmObj* %current_45args57141, %struct.ScmObj** %stackaddr$prim58412, align 8
%stackaddr$prim58413 = alloca %struct.ScmObj*, align 8
%a48160 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57141)
store volatile %struct.ScmObj* %a48160, %struct.ScmObj** %stackaddr$prim58413, align 8
%stackaddr$prim58414 = alloca %struct.ScmObj*, align 8
%current_45args57142 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57141)
store volatile %struct.ScmObj* %current_45args57142, %struct.ScmObj** %stackaddr$prim58414, align 8
%stackaddr$prim58415 = alloca %struct.ScmObj*, align 8
%b48159 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57142)
store volatile %struct.ScmObj* %b48159, %struct.ScmObj** %stackaddr$prim58415, align 8
%stackaddr$prim58416 = alloca %struct.ScmObj*, align 8
%cpsprim48408 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a48160, %struct.ScmObj* %b48159)
store volatile %struct.ScmObj* %cpsprim48408, %struct.ScmObj** %stackaddr$prim58416, align 8
%ae49915 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57144$k484070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58417 = alloca %struct.ScmObj*, align 8
%argslist57144$k484071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48408, %struct.ScmObj* %argslist57144$k484070)
store volatile %struct.ScmObj* %argslist57144$k484071, %struct.ScmObj** %stackaddr$prim58417, align 8
%stackaddr$prim58418 = alloca %struct.ScmObj*, align 8
%argslist57144$k484072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49915, %struct.ScmObj* %argslist57144$k484071)
store volatile %struct.ScmObj* %argslist57144$k484072, %struct.ScmObj** %stackaddr$prim58418, align 8
%clofunc58419 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48407)
musttail call tailcc void %clofunc58419(%struct.ScmObj* %k48407, %struct.ScmObj* %argslist57144$k484072)
ret void
}

define tailcc void @proc_clo$ae49887(%struct.ScmObj* %env$ae49887,%struct.ScmObj* %current_45args57147) {
%stackaddr$prim58420 = alloca %struct.ScmObj*, align 8
%k48409 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57147)
store volatile %struct.ScmObj* %k48409, %struct.ScmObj** %stackaddr$prim58420, align 8
%stackaddr$prim58421 = alloca %struct.ScmObj*, align 8
%current_45args57148 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57147)
store volatile %struct.ScmObj* %current_45args57148, %struct.ScmObj** %stackaddr$prim58421, align 8
%stackaddr$prim58422 = alloca %struct.ScmObj*, align 8
%x48155 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57148)
store volatile %struct.ScmObj* %x48155, %struct.ScmObj** %stackaddr$prim58422, align 8
%stackaddr$prim58423 = alloca %struct.ScmObj*, align 8
%cpsprim48410 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48155)
store volatile %struct.ScmObj* %cpsprim48410, %struct.ScmObj** %stackaddr$prim58423, align 8
%ae49890 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57150$k484090 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58424 = alloca %struct.ScmObj*, align 8
%argslist57150$k484091 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48410, %struct.ScmObj* %argslist57150$k484090)
store volatile %struct.ScmObj* %argslist57150$k484091, %struct.ScmObj** %stackaddr$prim58424, align 8
%stackaddr$prim58425 = alloca %struct.ScmObj*, align 8
%argslist57150$k484092 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49890, %struct.ScmObj* %argslist57150$k484091)
store volatile %struct.ScmObj* %argslist57150$k484092, %struct.ScmObj** %stackaddr$prim58425, align 8
%clofunc58426 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48409)
musttail call tailcc void %clofunc58426(%struct.ScmObj* %k48409, %struct.ScmObj* %argslist57150$k484092)
ret void
}

define tailcc void @proc_clo$ae49863(%struct.ScmObj* %env$ae49863,%struct.ScmObj* %current_45args57153) {
%stackaddr$prim58427 = alloca %struct.ScmObj*, align 8
%k48411 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57153)
store volatile %struct.ScmObj* %k48411, %struct.ScmObj** %stackaddr$prim58427, align 8
%stackaddr$prim58428 = alloca %struct.ScmObj*, align 8
%current_45args57154 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57153)
store volatile %struct.ScmObj* %current_45args57154, %struct.ScmObj** %stackaddr$prim58428, align 8
%stackaddr$prim58429 = alloca %struct.ScmObj*, align 8
%x48157 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57154)
store volatile %struct.ScmObj* %x48157, %struct.ScmObj** %stackaddr$prim58429, align 8
%stackaddr$prim58430 = alloca %struct.ScmObj*, align 8
%cpsprim48412 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48157)
store volatile %struct.ScmObj* %cpsprim48412, %struct.ScmObj** %stackaddr$prim58430, align 8
%ae49866 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57156$k484110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58431 = alloca %struct.ScmObj*, align 8
%argslist57156$k484111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48412, %struct.ScmObj* %argslist57156$k484110)
store volatile %struct.ScmObj* %argslist57156$k484111, %struct.ScmObj** %stackaddr$prim58431, align 8
%stackaddr$prim58432 = alloca %struct.ScmObj*, align 8
%argslist57156$k484112 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49866, %struct.ScmObj* %argslist57156$k484111)
store volatile %struct.ScmObj* %argslist57156$k484112, %struct.ScmObj** %stackaddr$prim58432, align 8
%clofunc58433 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48411)
musttail call tailcc void %clofunc58433(%struct.ScmObj* %k48411, %struct.ScmObj* %argslist57156$k484112)
ret void
}

define tailcc void @proc_clo$ae49815(%struct.ScmObj* %env$ae49815,%struct.ScmObj* %current_45args57159) {
%stackaddr$prim58434 = alloca %struct.ScmObj*, align 8
%k48413 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57159)
store volatile %struct.ScmObj* %k48413, %struct.ScmObj** %stackaddr$prim58434, align 8
%stackaddr$prim58435 = alloca %struct.ScmObj*, align 8
%current_45args57160 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57159)
store volatile %struct.ScmObj* %current_45args57160, %struct.ScmObj** %stackaddr$prim58435, align 8
%stackaddr$prim58436 = alloca %struct.ScmObj*, align 8
%lst48153 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57160)
store volatile %struct.ScmObj* %lst48153, %struct.ScmObj** %stackaddr$prim58436, align 8
%stackaddr$prim58437 = alloca %struct.ScmObj*, align 8
%current_45args57161 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57160)
store volatile %struct.ScmObj* %current_45args57161, %struct.ScmObj** %stackaddr$prim58437, align 8
%stackaddr$prim58438 = alloca %struct.ScmObj*, align 8
%b48152 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57161)
store volatile %struct.ScmObj* %b48152, %struct.ScmObj** %stackaddr$prim58438, align 8
%truthy$cmp58439 = call i64 @is_truthy_value(%struct.ScmObj* %b48152)
%cmp$cmp58439 = icmp eq i64 %truthy$cmp58439, 1
br i1 %cmp$cmp58439, label %truebranch$cmp58439, label %falsebranch$cmp58439
truebranch$cmp58439:
%ae49818 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57163$k484130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58440 = alloca %struct.ScmObj*, align 8
%argslist57163$k484131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b48152, %struct.ScmObj* %argslist57163$k484130)
store volatile %struct.ScmObj* %argslist57163$k484131, %struct.ScmObj** %stackaddr$prim58440, align 8
%stackaddr$prim58441 = alloca %struct.ScmObj*, align 8
%argslist57163$k484132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49818, %struct.ScmObj* %argslist57163$k484131)
store volatile %struct.ScmObj* %argslist57163$k484132, %struct.ScmObj** %stackaddr$prim58441, align 8
%clofunc58442 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48413)
musttail call tailcc void %clofunc58442(%struct.ScmObj* %k48413, %struct.ScmObj* %argslist57163$k484132)
ret void
falsebranch$cmp58439:
%stackaddr$prim58443 = alloca %struct.ScmObj*, align 8
%cpsprim48414 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48153)
store volatile %struct.ScmObj* %cpsprim48414, %struct.ScmObj** %stackaddr$prim58443, align 8
%ae49825 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57164$k484130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58444 = alloca %struct.ScmObj*, align 8
%argslist57164$k484131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48414, %struct.ScmObj* %argslist57164$k484130)
store volatile %struct.ScmObj* %argslist57164$k484131, %struct.ScmObj** %stackaddr$prim58444, align 8
%stackaddr$prim58445 = alloca %struct.ScmObj*, align 8
%argslist57164$k484132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49825, %struct.ScmObj* %argslist57164$k484131)
store volatile %struct.ScmObj* %argslist57164$k484132, %struct.ScmObj** %stackaddr$prim58445, align 8
%clofunc58446 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48413)
musttail call tailcc void %clofunc58446(%struct.ScmObj* %k48413, %struct.ScmObj* %argslist57164$k484132)
ret void
}

define tailcc void @proc_clo$ae49656(%struct.ScmObj* %env$ae49656,%struct.ScmObj* %args4809148415) {
%stackaddr$env-ref58447 = alloca %struct.ScmObj*, align 8
%_37last48086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49656, i64 0)
store %struct.ScmObj* %_37last48086, %struct.ScmObj** %stackaddr$env-ref58447
%stackaddr$env-ref58448 = alloca %struct.ScmObj*, align 8
%_37foldr48069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49656, i64 1)
store %struct.ScmObj* %_37foldr48069, %struct.ScmObj** %stackaddr$env-ref58448
%stackaddr$env-ref58449 = alloca %struct.ScmObj*, align 8
%_37drop_45right48083 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49656, i64 2)
store %struct.ScmObj* %_37drop_45right48083, %struct.ScmObj** %stackaddr$env-ref58449
%stackaddr$prim58450 = alloca %struct.ScmObj*, align 8
%k48416 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4809148415)
store volatile %struct.ScmObj* %k48416, %struct.ScmObj** %stackaddr$prim58450, align 8
%stackaddr$prim58451 = alloca %struct.ScmObj*, align 8
%args48091 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4809148415)
store volatile %struct.ScmObj* %args48091, %struct.ScmObj** %stackaddr$prim58451, align 8
%stackaddr$prim58452 = alloca %struct.ScmObj*, align 8
%f48093 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48091)
store volatile %struct.ScmObj* %f48093, %struct.ScmObj** %stackaddr$prim58452, align 8
%stackaddr$prim58453 = alloca %struct.ScmObj*, align 8
%lsts48092 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48091)
store volatile %struct.ScmObj* %lsts48092, %struct.ScmObj** %stackaddr$prim58453, align 8
%stackaddr$makeclosure58454 = alloca %struct.ScmObj*, align 8
%fptrToInt58455 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49661 to i64
%ae49661 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58455)
store volatile %struct.ScmObj* %ae49661, %struct.ScmObj** %stackaddr$makeclosure58454, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49661, %struct.ScmObj* %_37foldr48069, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49661, %struct.ScmObj* %k48416, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49661, %struct.ScmObj* %lsts48092, i64 2)
%ae49662 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58456 = alloca %struct.ScmObj*, align 8
%fptrToInt58457 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49663 to i64
%ae49663 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58457)
store volatile %struct.ScmObj* %ae49663, %struct.ScmObj** %stackaddr$makeclosure58456, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49663, %struct.ScmObj* %_37last48086, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49663, %struct.ScmObj* %_37drop_45right48083, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49663, %struct.ScmObj* %f48093, i64 2)
%argslist57183$ae496610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58458 = alloca %struct.ScmObj*, align 8
%argslist57183$ae496611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49663, %struct.ScmObj* %argslist57183$ae496610)
store volatile %struct.ScmObj* %argslist57183$ae496611, %struct.ScmObj** %stackaddr$prim58458, align 8
%stackaddr$prim58459 = alloca %struct.ScmObj*, align 8
%argslist57183$ae496612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49662, %struct.ScmObj* %argslist57183$ae496611)
store volatile %struct.ScmObj* %argslist57183$ae496612, %struct.ScmObj** %stackaddr$prim58459, align 8
%clofunc58460 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49661)
musttail call tailcc void %clofunc58460(%struct.ScmObj* %ae49661, %struct.ScmObj* %argslist57183$ae496612)
ret void
}

define tailcc void @proc_clo$ae49661(%struct.ScmObj* %env$ae49661,%struct.ScmObj* %current_45args57168) {
%stackaddr$env-ref58461 = alloca %struct.ScmObj*, align 8
%_37foldr48069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49661, i64 0)
store %struct.ScmObj* %_37foldr48069, %struct.ScmObj** %stackaddr$env-ref58461
%stackaddr$env-ref58462 = alloca %struct.ScmObj*, align 8
%k48416 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49661, i64 1)
store %struct.ScmObj* %k48416, %struct.ScmObj** %stackaddr$env-ref58462
%stackaddr$env-ref58463 = alloca %struct.ScmObj*, align 8
%lsts48092 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49661, i64 2)
store %struct.ScmObj* %lsts48092, %struct.ScmObj** %stackaddr$env-ref58463
%stackaddr$prim58464 = alloca %struct.ScmObj*, align 8
%_95k48417 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57168)
store volatile %struct.ScmObj* %_95k48417, %struct.ScmObj** %stackaddr$prim58464, align 8
%stackaddr$prim58465 = alloca %struct.ScmObj*, align 8
%current_45args57169 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57168)
store volatile %struct.ScmObj* %current_45args57169, %struct.ScmObj** %stackaddr$prim58465, align 8
%stackaddr$prim58466 = alloca %struct.ScmObj*, align 8
%anf_45bind48250 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57169)
store volatile %struct.ScmObj* %anf_45bind48250, %struct.ScmObj** %stackaddr$prim58466, align 8
%ae49724 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58467 = alloca %struct.ScmObj*, align 8
%anf_45bind48251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49724, %struct.ScmObj* %lsts48092)
store volatile %struct.ScmObj* %anf_45bind48251, %struct.ScmObj** %stackaddr$prim58467, align 8
%stackaddr$prim58468 = alloca %struct.ScmObj*, align 8
%anf_45bind48252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48250, %struct.ScmObj* %anf_45bind48251)
store volatile %struct.ScmObj* %anf_45bind48252, %struct.ScmObj** %stackaddr$prim58468, align 8
%stackaddr$prim58469 = alloca %struct.ScmObj*, align 8
%cpsargs48418 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48416, %struct.ScmObj* %anf_45bind48252)
store volatile %struct.ScmObj* %cpsargs48418, %struct.ScmObj** %stackaddr$prim58469, align 8
%clofunc58470 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr48069)
musttail call tailcc void %clofunc58470(%struct.ScmObj* %_37foldr48069, %struct.ScmObj* %cpsargs48418)
ret void
}

define tailcc void @proc_clo$ae49663(%struct.ScmObj* %env$ae49663,%struct.ScmObj* %fargs4809448419) {
%stackaddr$env-ref58471 = alloca %struct.ScmObj*, align 8
%_37last48086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49663, i64 0)
store %struct.ScmObj* %_37last48086, %struct.ScmObj** %stackaddr$env-ref58471
%stackaddr$env-ref58472 = alloca %struct.ScmObj*, align 8
%_37drop_45right48083 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49663, i64 1)
store %struct.ScmObj* %_37drop_45right48083, %struct.ScmObj** %stackaddr$env-ref58472
%stackaddr$env-ref58473 = alloca %struct.ScmObj*, align 8
%f48093 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49663, i64 2)
store %struct.ScmObj* %f48093, %struct.ScmObj** %stackaddr$env-ref58473
%stackaddr$prim58474 = alloca %struct.ScmObj*, align 8
%k48420 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs4809448419)
store volatile %struct.ScmObj* %k48420, %struct.ScmObj** %stackaddr$prim58474, align 8
%stackaddr$prim58475 = alloca %struct.ScmObj*, align 8
%fargs48094 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs4809448419)
store volatile %struct.ScmObj* %fargs48094, %struct.ScmObj** %stackaddr$prim58475, align 8
%stackaddr$makeclosure58476 = alloca %struct.ScmObj*, align 8
%fptrToInt58477 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49667 to i64
%ae49667 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt58477)
store volatile %struct.ScmObj* %ae49667, %struct.ScmObj** %stackaddr$makeclosure58476, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49667, %struct.ScmObj* %_37last48086, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49667, %struct.ScmObj* %k48420, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49667, %struct.ScmObj* %fargs48094, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49667, %struct.ScmObj* %f48093, i64 3)
%ae49669 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist57182$_37drop_45right480830 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58478 = alloca %struct.ScmObj*, align 8
%argslist57182$_37drop_45right480831 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49669, %struct.ScmObj* %argslist57182$_37drop_45right480830)
store volatile %struct.ScmObj* %argslist57182$_37drop_45right480831, %struct.ScmObj** %stackaddr$prim58478, align 8
%stackaddr$prim58479 = alloca %struct.ScmObj*, align 8
%argslist57182$_37drop_45right480832 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs48094, %struct.ScmObj* %argslist57182$_37drop_45right480831)
store volatile %struct.ScmObj* %argslist57182$_37drop_45right480832, %struct.ScmObj** %stackaddr$prim58479, align 8
%stackaddr$prim58480 = alloca %struct.ScmObj*, align 8
%argslist57182$_37drop_45right480833 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49667, %struct.ScmObj* %argslist57182$_37drop_45right480832)
store volatile %struct.ScmObj* %argslist57182$_37drop_45right480833, %struct.ScmObj** %stackaddr$prim58480, align 8
%clofunc58481 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right48083)
musttail call tailcc void %clofunc58481(%struct.ScmObj* %_37drop_45right48083, %struct.ScmObj* %argslist57182$_37drop_45right480833)
ret void
}

define tailcc void @proc_clo$ae49667(%struct.ScmObj* %env$ae49667,%struct.ScmObj* %current_45args57171) {
%stackaddr$env-ref58482 = alloca %struct.ScmObj*, align 8
%_37last48086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49667, i64 0)
store %struct.ScmObj* %_37last48086, %struct.ScmObj** %stackaddr$env-ref58482
%stackaddr$env-ref58483 = alloca %struct.ScmObj*, align 8
%k48420 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49667, i64 1)
store %struct.ScmObj* %k48420, %struct.ScmObj** %stackaddr$env-ref58483
%stackaddr$env-ref58484 = alloca %struct.ScmObj*, align 8
%fargs48094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49667, i64 2)
store %struct.ScmObj* %fargs48094, %struct.ScmObj** %stackaddr$env-ref58484
%stackaddr$env-ref58485 = alloca %struct.ScmObj*, align 8
%f48093 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49667, i64 3)
store %struct.ScmObj* %f48093, %struct.ScmObj** %stackaddr$env-ref58485
%stackaddr$prim58486 = alloca %struct.ScmObj*, align 8
%_95k48421 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57171)
store volatile %struct.ScmObj* %_95k48421, %struct.ScmObj** %stackaddr$prim58486, align 8
%stackaddr$prim58487 = alloca %struct.ScmObj*, align 8
%current_45args57172 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57171)
store volatile %struct.ScmObj* %current_45args57172, %struct.ScmObj** %stackaddr$prim58487, align 8
%stackaddr$prim58488 = alloca %struct.ScmObj*, align 8
%anf_45bind48247 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57172)
store volatile %struct.ScmObj* %anf_45bind48247, %struct.ScmObj** %stackaddr$prim58488, align 8
%stackaddr$makeclosure58489 = alloca %struct.ScmObj*, align 8
%fptrToInt58490 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49674 to i64
%ae49674 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58490)
store volatile %struct.ScmObj* %ae49674, %struct.ScmObj** %stackaddr$makeclosure58489, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49674, %struct.ScmObj* %_37last48086, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49674, %struct.ScmObj* %k48420, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49674, %struct.ScmObj* %fargs48094, i64 2)
%stackaddr$prim58491 = alloca %struct.ScmObj*, align 8
%cpsargs48425 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49674, %struct.ScmObj* %anf_45bind48247)
store volatile %struct.ScmObj* %cpsargs48425, %struct.ScmObj** %stackaddr$prim58491, align 8
%clofunc58492 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48093)
musttail call tailcc void %clofunc58492(%struct.ScmObj* %f48093, %struct.ScmObj* %cpsargs48425)
ret void
}

define tailcc void @proc_clo$ae49674(%struct.ScmObj* %env$ae49674,%struct.ScmObj* %current_45args57174) {
%stackaddr$env-ref58493 = alloca %struct.ScmObj*, align 8
%_37last48086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49674, i64 0)
store %struct.ScmObj* %_37last48086, %struct.ScmObj** %stackaddr$env-ref58493
%stackaddr$env-ref58494 = alloca %struct.ScmObj*, align 8
%k48420 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49674, i64 1)
store %struct.ScmObj* %k48420, %struct.ScmObj** %stackaddr$env-ref58494
%stackaddr$env-ref58495 = alloca %struct.ScmObj*, align 8
%fargs48094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49674, i64 2)
store %struct.ScmObj* %fargs48094, %struct.ScmObj** %stackaddr$env-ref58495
%stackaddr$prim58496 = alloca %struct.ScmObj*, align 8
%_95k48422 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57174)
store volatile %struct.ScmObj* %_95k48422, %struct.ScmObj** %stackaddr$prim58496, align 8
%stackaddr$prim58497 = alloca %struct.ScmObj*, align 8
%current_45args57175 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57174)
store volatile %struct.ScmObj* %current_45args57175, %struct.ScmObj** %stackaddr$prim58497, align 8
%stackaddr$prim58498 = alloca %struct.ScmObj*, align 8
%anf_45bind48248 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57175)
store volatile %struct.ScmObj* %anf_45bind48248, %struct.ScmObj** %stackaddr$prim58498, align 8
%stackaddr$makeclosure58499 = alloca %struct.ScmObj*, align 8
%fptrToInt58500 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49679 to i64
%ae49679 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58500)
store volatile %struct.ScmObj* %ae49679, %struct.ScmObj** %stackaddr$makeclosure58499, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49679, %struct.ScmObj* %anf_45bind48248, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49679, %struct.ScmObj* %k48420, i64 1)
%argslist57181$_37last480860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58501 = alloca %struct.ScmObj*, align 8
%argslist57181$_37last480861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs48094, %struct.ScmObj* %argslist57181$_37last480860)
store volatile %struct.ScmObj* %argslist57181$_37last480861, %struct.ScmObj** %stackaddr$prim58501, align 8
%stackaddr$prim58502 = alloca %struct.ScmObj*, align 8
%argslist57181$_37last480862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49679, %struct.ScmObj* %argslist57181$_37last480861)
store volatile %struct.ScmObj* %argslist57181$_37last480862, %struct.ScmObj** %stackaddr$prim58502, align 8
%clofunc58503 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last48086)
musttail call tailcc void %clofunc58503(%struct.ScmObj* %_37last48086, %struct.ScmObj* %argslist57181$_37last480862)
ret void
}

define tailcc void @proc_clo$ae49679(%struct.ScmObj* %env$ae49679,%struct.ScmObj* %current_45args57177) {
%stackaddr$env-ref58504 = alloca %struct.ScmObj*, align 8
%anf_45bind48248 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49679, i64 0)
store %struct.ScmObj* %anf_45bind48248, %struct.ScmObj** %stackaddr$env-ref58504
%stackaddr$env-ref58505 = alloca %struct.ScmObj*, align 8
%k48420 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49679, i64 1)
store %struct.ScmObj* %k48420, %struct.ScmObj** %stackaddr$env-ref58505
%stackaddr$prim58506 = alloca %struct.ScmObj*, align 8
%_95k48423 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57177)
store volatile %struct.ScmObj* %_95k48423, %struct.ScmObj** %stackaddr$prim58506, align 8
%stackaddr$prim58507 = alloca %struct.ScmObj*, align 8
%current_45args57178 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57177)
store volatile %struct.ScmObj* %current_45args57178, %struct.ScmObj** %stackaddr$prim58507, align 8
%stackaddr$prim58508 = alloca %struct.ScmObj*, align 8
%anf_45bind48249 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57178)
store volatile %struct.ScmObj* %anf_45bind48249, %struct.ScmObj** %stackaddr$prim58508, align 8
%stackaddr$prim58509 = alloca %struct.ScmObj*, align 8
%cpsprim48424 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48248, %struct.ScmObj* %anf_45bind48249)
store volatile %struct.ScmObj* %cpsprim48424, %struct.ScmObj** %stackaddr$prim58509, align 8
%ae49684 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57180$k484200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58510 = alloca %struct.ScmObj*, align 8
%argslist57180$k484201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48424, %struct.ScmObj* %argslist57180$k484200)
store volatile %struct.ScmObj* %argslist57180$k484201, %struct.ScmObj** %stackaddr$prim58510, align 8
%stackaddr$prim58511 = alloca %struct.ScmObj*, align 8
%argslist57180$k484202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49684, %struct.ScmObj* %argslist57180$k484201)
store volatile %struct.ScmObj* %argslist57180$k484202, %struct.ScmObj** %stackaddr$prim58511, align 8
%clofunc58512 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48420)
musttail call tailcc void %clofunc58512(%struct.ScmObj* %k48420, %struct.ScmObj* %argslist57180$k484202)
ret void
}

define tailcc void @proc_clo$ae49579(%struct.ScmObj* %env$ae49579,%struct.ScmObj* %current_45args57185) {
%stackaddr$env-ref58513 = alloca %struct.ScmObj*, align 8
%_37foldr148064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49579, i64 0)
store %struct.ScmObj* %_37foldr148064, %struct.ScmObj** %stackaddr$env-ref58513
%stackaddr$prim58514 = alloca %struct.ScmObj*, align 8
%k48426 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57185)
store volatile %struct.ScmObj* %k48426, %struct.ScmObj** %stackaddr$prim58514, align 8
%stackaddr$prim58515 = alloca %struct.ScmObj*, align 8
%current_45args57186 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57185)
store volatile %struct.ScmObj* %current_45args57186, %struct.ScmObj** %stackaddr$prim58515, align 8
%stackaddr$prim58516 = alloca %struct.ScmObj*, align 8
%f48097 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57186)
store volatile %struct.ScmObj* %f48097, %struct.ScmObj** %stackaddr$prim58516, align 8
%stackaddr$prim58517 = alloca %struct.ScmObj*, align 8
%current_45args57187 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57186)
store volatile %struct.ScmObj* %current_45args57187, %struct.ScmObj** %stackaddr$prim58517, align 8
%stackaddr$prim58518 = alloca %struct.ScmObj*, align 8
%lst48096 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57187)
store volatile %struct.ScmObj* %lst48096, %struct.ScmObj** %stackaddr$prim58518, align 8
%stackaddr$makeclosure58519 = alloca %struct.ScmObj*, align 8
%fptrToInt58520 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49580 to i64
%ae49580 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58520)
store volatile %struct.ScmObj* %ae49580, %struct.ScmObj** %stackaddr$makeclosure58519, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49580, %struct.ScmObj* %lst48096, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49580, %struct.ScmObj* %_37foldr148064, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49580, %struct.ScmObj* %k48426, i64 2)
%ae49581 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58521 = alloca %struct.ScmObj*, align 8
%fptrToInt58522 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49582 to i64
%ae49582 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58522)
store volatile %struct.ScmObj* %ae49582, %struct.ScmObj** %stackaddr$makeclosure58521, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49582, %struct.ScmObj* %f48097, i64 0)
%argslist57202$ae495800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58523 = alloca %struct.ScmObj*, align 8
%argslist57202$ae495801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49582, %struct.ScmObj* %argslist57202$ae495800)
store volatile %struct.ScmObj* %argslist57202$ae495801, %struct.ScmObj** %stackaddr$prim58523, align 8
%stackaddr$prim58524 = alloca %struct.ScmObj*, align 8
%argslist57202$ae495802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49581, %struct.ScmObj* %argslist57202$ae495801)
store volatile %struct.ScmObj* %argslist57202$ae495802, %struct.ScmObj** %stackaddr$prim58524, align 8
%clofunc58525 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49580)
musttail call tailcc void %clofunc58525(%struct.ScmObj* %ae49580, %struct.ScmObj* %argslist57202$ae495802)
ret void
}

define tailcc void @proc_clo$ae49580(%struct.ScmObj* %env$ae49580,%struct.ScmObj* %current_45args57189) {
%stackaddr$env-ref58526 = alloca %struct.ScmObj*, align 8
%lst48096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49580, i64 0)
store %struct.ScmObj* %lst48096, %struct.ScmObj** %stackaddr$env-ref58526
%stackaddr$env-ref58527 = alloca %struct.ScmObj*, align 8
%_37foldr148064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49580, i64 1)
store %struct.ScmObj* %_37foldr148064, %struct.ScmObj** %stackaddr$env-ref58527
%stackaddr$env-ref58528 = alloca %struct.ScmObj*, align 8
%k48426 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49580, i64 2)
store %struct.ScmObj* %k48426, %struct.ScmObj** %stackaddr$env-ref58528
%stackaddr$prim58529 = alloca %struct.ScmObj*, align 8
%_95k48427 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57189)
store volatile %struct.ScmObj* %_95k48427, %struct.ScmObj** %stackaddr$prim58529, align 8
%stackaddr$prim58530 = alloca %struct.ScmObj*, align 8
%current_45args57190 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57189)
store volatile %struct.ScmObj* %current_45args57190, %struct.ScmObj** %stackaddr$prim58530, align 8
%stackaddr$prim58531 = alloca %struct.ScmObj*, align 8
%anf_45bind48246 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57190)
store volatile %struct.ScmObj* %anf_45bind48246, %struct.ScmObj** %stackaddr$prim58531, align 8
%ae49614 = call %struct.ScmObj* @const_init_null()
%argslist57192$_37foldr1480640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58532 = alloca %struct.ScmObj*, align 8
%argslist57192$_37foldr1480641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48096, %struct.ScmObj* %argslist57192$_37foldr1480640)
store volatile %struct.ScmObj* %argslist57192$_37foldr1480641, %struct.ScmObj** %stackaddr$prim58532, align 8
%stackaddr$prim58533 = alloca %struct.ScmObj*, align 8
%argslist57192$_37foldr1480642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49614, %struct.ScmObj* %argslist57192$_37foldr1480641)
store volatile %struct.ScmObj* %argslist57192$_37foldr1480642, %struct.ScmObj** %stackaddr$prim58533, align 8
%stackaddr$prim58534 = alloca %struct.ScmObj*, align 8
%argslist57192$_37foldr1480643 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48246, %struct.ScmObj* %argslist57192$_37foldr1480642)
store volatile %struct.ScmObj* %argslist57192$_37foldr1480643, %struct.ScmObj** %stackaddr$prim58534, align 8
%stackaddr$prim58535 = alloca %struct.ScmObj*, align 8
%argslist57192$_37foldr1480644 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48426, %struct.ScmObj* %argslist57192$_37foldr1480643)
store volatile %struct.ScmObj* %argslist57192$_37foldr1480644, %struct.ScmObj** %stackaddr$prim58535, align 8
%clofunc58536 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148064)
musttail call tailcc void %clofunc58536(%struct.ScmObj* %_37foldr148064, %struct.ScmObj* %argslist57192$_37foldr1480644)
ret void
}

define tailcc void @proc_clo$ae49582(%struct.ScmObj* %env$ae49582,%struct.ScmObj* %current_45args57193) {
%stackaddr$env-ref58537 = alloca %struct.ScmObj*, align 8
%f48097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49582, i64 0)
store %struct.ScmObj* %f48097, %struct.ScmObj** %stackaddr$env-ref58537
%stackaddr$prim58538 = alloca %struct.ScmObj*, align 8
%k48428 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57193)
store volatile %struct.ScmObj* %k48428, %struct.ScmObj** %stackaddr$prim58538, align 8
%stackaddr$prim58539 = alloca %struct.ScmObj*, align 8
%current_45args57194 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57193)
store volatile %struct.ScmObj* %current_45args57194, %struct.ScmObj** %stackaddr$prim58539, align 8
%stackaddr$prim58540 = alloca %struct.ScmObj*, align 8
%v48099 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57194)
store volatile %struct.ScmObj* %v48099, %struct.ScmObj** %stackaddr$prim58540, align 8
%stackaddr$prim58541 = alloca %struct.ScmObj*, align 8
%current_45args57195 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57194)
store volatile %struct.ScmObj* %current_45args57195, %struct.ScmObj** %stackaddr$prim58541, align 8
%stackaddr$prim58542 = alloca %struct.ScmObj*, align 8
%r48098 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57195)
store volatile %struct.ScmObj* %r48098, %struct.ScmObj** %stackaddr$prim58542, align 8
%stackaddr$makeclosure58543 = alloca %struct.ScmObj*, align 8
%fptrToInt58544 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49584 to i64
%ae49584 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58544)
store volatile %struct.ScmObj* %ae49584, %struct.ScmObj** %stackaddr$makeclosure58543, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49584, %struct.ScmObj* %r48098, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49584, %struct.ScmObj* %k48428, i64 1)
%argslist57201$f480970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58545 = alloca %struct.ScmObj*, align 8
%argslist57201$f480971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v48099, %struct.ScmObj* %argslist57201$f480970)
store volatile %struct.ScmObj* %argslist57201$f480971, %struct.ScmObj** %stackaddr$prim58545, align 8
%stackaddr$prim58546 = alloca %struct.ScmObj*, align 8
%argslist57201$f480972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49584, %struct.ScmObj* %argslist57201$f480971)
store volatile %struct.ScmObj* %argslist57201$f480972, %struct.ScmObj** %stackaddr$prim58546, align 8
%clofunc58547 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48097)
musttail call tailcc void %clofunc58547(%struct.ScmObj* %f48097, %struct.ScmObj* %argslist57201$f480972)
ret void
}

define tailcc void @proc_clo$ae49584(%struct.ScmObj* %env$ae49584,%struct.ScmObj* %current_45args57197) {
%stackaddr$env-ref58548 = alloca %struct.ScmObj*, align 8
%r48098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49584, i64 0)
store %struct.ScmObj* %r48098, %struct.ScmObj** %stackaddr$env-ref58548
%stackaddr$env-ref58549 = alloca %struct.ScmObj*, align 8
%k48428 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49584, i64 1)
store %struct.ScmObj* %k48428, %struct.ScmObj** %stackaddr$env-ref58549
%stackaddr$prim58550 = alloca %struct.ScmObj*, align 8
%_95k48429 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57197)
store volatile %struct.ScmObj* %_95k48429, %struct.ScmObj** %stackaddr$prim58550, align 8
%stackaddr$prim58551 = alloca %struct.ScmObj*, align 8
%current_45args57198 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57197)
store volatile %struct.ScmObj* %current_45args57198, %struct.ScmObj** %stackaddr$prim58551, align 8
%stackaddr$prim58552 = alloca %struct.ScmObj*, align 8
%anf_45bind48245 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57198)
store volatile %struct.ScmObj* %anf_45bind48245, %struct.ScmObj** %stackaddr$prim58552, align 8
%stackaddr$prim58553 = alloca %struct.ScmObj*, align 8
%cpsprim48430 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48245, %struct.ScmObj* %r48098)
store volatile %struct.ScmObj* %cpsprim48430, %struct.ScmObj** %stackaddr$prim58553, align 8
%ae49589 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57200$k484280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58554 = alloca %struct.ScmObj*, align 8
%argslist57200$k484281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48430, %struct.ScmObj* %argslist57200$k484280)
store volatile %struct.ScmObj* %argslist57200$k484281, %struct.ScmObj** %stackaddr$prim58554, align 8
%stackaddr$prim58555 = alloca %struct.ScmObj*, align 8
%argslist57200$k484282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49589, %struct.ScmObj* %argslist57200$k484281)
store volatile %struct.ScmObj* %argslist57200$k484282, %struct.ScmObj** %stackaddr$prim58555, align 8
%clofunc58556 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48428)
musttail call tailcc void %clofunc58556(%struct.ScmObj* %k48428, %struct.ScmObj* %argslist57200$k484282)
ret void
}

define tailcc void @proc_clo$ae49193(%struct.ScmObj* %env$ae49193,%struct.ScmObj* %current_45args57205) {
%stackaddr$env-ref58557 = alloca %struct.ScmObj*, align 8
%_37foldr148064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49193, i64 0)
store %struct.ScmObj* %_37foldr148064, %struct.ScmObj** %stackaddr$env-ref58557
%stackaddr$env-ref58558 = alloca %struct.ScmObj*, align 8
%_37map148060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49193, i64 1)
store %struct.ScmObj* %_37map148060, %struct.ScmObj** %stackaddr$env-ref58558
%stackaddr$prim58559 = alloca %struct.ScmObj*, align 8
%k48431 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57205)
store volatile %struct.ScmObj* %k48431, %struct.ScmObj** %stackaddr$prim58559, align 8
%stackaddr$prim58560 = alloca %struct.ScmObj*, align 8
%current_45args57206 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57205)
store volatile %struct.ScmObj* %current_45args57206, %struct.ScmObj** %stackaddr$prim58560, align 8
%stackaddr$prim58561 = alloca %struct.ScmObj*, align 8
%_37foldr48070 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57206)
store volatile %struct.ScmObj* %_37foldr48070, %struct.ScmObj** %stackaddr$prim58561, align 8
%ae49195 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58562 = alloca %struct.ScmObj*, align 8
%fptrToInt58563 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49196 to i64
%ae49196 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58563)
store volatile %struct.ScmObj* %ae49196, %struct.ScmObj** %stackaddr$makeclosure58562, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49196, %struct.ScmObj* %_37foldr48070, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49196, %struct.ScmObj* %_37foldr148064, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49196, %struct.ScmObj* %_37map148060, i64 2)
%argslist57263$k484310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58564 = alloca %struct.ScmObj*, align 8
%argslist57263$k484311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49196, %struct.ScmObj* %argslist57263$k484310)
store volatile %struct.ScmObj* %argslist57263$k484311, %struct.ScmObj** %stackaddr$prim58564, align 8
%stackaddr$prim58565 = alloca %struct.ScmObj*, align 8
%argslist57263$k484312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49195, %struct.ScmObj* %argslist57263$k484311)
store volatile %struct.ScmObj* %argslist57263$k484312, %struct.ScmObj** %stackaddr$prim58565, align 8
%clofunc58566 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48431)
musttail call tailcc void %clofunc58566(%struct.ScmObj* %k48431, %struct.ScmObj* %argslist57263$k484312)
ret void
}

define tailcc void @proc_clo$ae49196(%struct.ScmObj* %env$ae49196,%struct.ScmObj* %args4807148432) {
%stackaddr$env-ref58567 = alloca %struct.ScmObj*, align 8
%_37foldr48070 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49196, i64 0)
store %struct.ScmObj* %_37foldr48070, %struct.ScmObj** %stackaddr$env-ref58567
%stackaddr$env-ref58568 = alloca %struct.ScmObj*, align 8
%_37foldr148064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49196, i64 1)
store %struct.ScmObj* %_37foldr148064, %struct.ScmObj** %stackaddr$env-ref58568
%stackaddr$env-ref58569 = alloca %struct.ScmObj*, align 8
%_37map148060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49196, i64 2)
store %struct.ScmObj* %_37map148060, %struct.ScmObj** %stackaddr$env-ref58569
%stackaddr$prim58570 = alloca %struct.ScmObj*, align 8
%k48433 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4807148432)
store volatile %struct.ScmObj* %k48433, %struct.ScmObj** %stackaddr$prim58570, align 8
%stackaddr$prim58571 = alloca %struct.ScmObj*, align 8
%args48071 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4807148432)
store volatile %struct.ScmObj* %args48071, %struct.ScmObj** %stackaddr$prim58571, align 8
%stackaddr$prim58572 = alloca %struct.ScmObj*, align 8
%f48074 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48071)
store volatile %struct.ScmObj* %f48074, %struct.ScmObj** %stackaddr$prim58572, align 8
%stackaddr$prim58573 = alloca %struct.ScmObj*, align 8
%anf_45bind48232 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48071)
store volatile %struct.ScmObj* %anf_45bind48232, %struct.ScmObj** %stackaddr$prim58573, align 8
%stackaddr$prim58574 = alloca %struct.ScmObj*, align 8
%acc48073 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48232)
store volatile %struct.ScmObj* %acc48073, %struct.ScmObj** %stackaddr$prim58574, align 8
%stackaddr$prim58575 = alloca %struct.ScmObj*, align 8
%anf_45bind48233 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48071)
store volatile %struct.ScmObj* %anf_45bind48233, %struct.ScmObj** %stackaddr$prim58575, align 8
%stackaddr$prim58576 = alloca %struct.ScmObj*, align 8
%lsts48072 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48233)
store volatile %struct.ScmObj* %lsts48072, %struct.ScmObj** %stackaddr$prim58576, align 8
%stackaddr$makeclosure58577 = alloca %struct.ScmObj*, align 8
%fptrToInt58578 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49204 to i64
%ae49204 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt58578)
store volatile %struct.ScmObj* %ae49204, %struct.ScmObj** %stackaddr$makeclosure58577, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49204, %struct.ScmObj* %acc48073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49204, %struct.ScmObj* %lsts48072, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49204, %struct.ScmObj* %_37foldr48070, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49204, %struct.ScmObj* %k48433, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49204, %struct.ScmObj* %_37foldr148064, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49204, %struct.ScmObj* %_37map148060, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49204, %struct.ScmObj* %f48074, i64 6)
%ae49205 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58579 = alloca %struct.ScmObj*, align 8
%fptrToInt58580 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49206 to i64
%ae49206 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58580)
store volatile %struct.ScmObj* %ae49206, %struct.ScmObj** %stackaddr$makeclosure58579, align 8
%argslist57262$ae492040 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58581 = alloca %struct.ScmObj*, align 8
%argslist57262$ae492041 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49206, %struct.ScmObj* %argslist57262$ae492040)
store volatile %struct.ScmObj* %argslist57262$ae492041, %struct.ScmObj** %stackaddr$prim58581, align 8
%stackaddr$prim58582 = alloca %struct.ScmObj*, align 8
%argslist57262$ae492042 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49205, %struct.ScmObj* %argslist57262$ae492041)
store volatile %struct.ScmObj* %argslist57262$ae492042, %struct.ScmObj** %stackaddr$prim58582, align 8
%clofunc58583 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49204)
musttail call tailcc void %clofunc58583(%struct.ScmObj* %ae49204, %struct.ScmObj* %argslist57262$ae492042)
ret void
}

define tailcc void @proc_clo$ae49204(%struct.ScmObj* %env$ae49204,%struct.ScmObj* %current_45args57208) {
%stackaddr$env-ref58584 = alloca %struct.ScmObj*, align 8
%acc48073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49204, i64 0)
store %struct.ScmObj* %acc48073, %struct.ScmObj** %stackaddr$env-ref58584
%stackaddr$env-ref58585 = alloca %struct.ScmObj*, align 8
%lsts48072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49204, i64 1)
store %struct.ScmObj* %lsts48072, %struct.ScmObj** %stackaddr$env-ref58585
%stackaddr$env-ref58586 = alloca %struct.ScmObj*, align 8
%_37foldr48070 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49204, i64 2)
store %struct.ScmObj* %_37foldr48070, %struct.ScmObj** %stackaddr$env-ref58586
%stackaddr$env-ref58587 = alloca %struct.ScmObj*, align 8
%k48433 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49204, i64 3)
store %struct.ScmObj* %k48433, %struct.ScmObj** %stackaddr$env-ref58587
%stackaddr$env-ref58588 = alloca %struct.ScmObj*, align 8
%_37foldr148064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49204, i64 4)
store %struct.ScmObj* %_37foldr148064, %struct.ScmObj** %stackaddr$env-ref58588
%stackaddr$env-ref58589 = alloca %struct.ScmObj*, align 8
%_37map148060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49204, i64 5)
store %struct.ScmObj* %_37map148060, %struct.ScmObj** %stackaddr$env-ref58589
%stackaddr$env-ref58590 = alloca %struct.ScmObj*, align 8
%f48074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49204, i64 6)
store %struct.ScmObj* %f48074, %struct.ScmObj** %stackaddr$env-ref58590
%stackaddr$prim58591 = alloca %struct.ScmObj*, align 8
%_95k48434 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57208)
store volatile %struct.ScmObj* %_95k48434, %struct.ScmObj** %stackaddr$prim58591, align 8
%stackaddr$prim58592 = alloca %struct.ScmObj*, align 8
%current_45args57209 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57208)
store volatile %struct.ScmObj* %current_45args57209, %struct.ScmObj** %stackaddr$prim58592, align 8
%stackaddr$prim58593 = alloca %struct.ScmObj*, align 8
%anf_45bind48234 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57209)
store volatile %struct.ScmObj* %anf_45bind48234, %struct.ScmObj** %stackaddr$prim58593, align 8
%stackaddr$makeclosure58594 = alloca %struct.ScmObj*, align 8
%fptrToInt58595 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49236 to i64
%ae49236 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt58595)
store volatile %struct.ScmObj* %ae49236, %struct.ScmObj** %stackaddr$makeclosure58594, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49236, %struct.ScmObj* %acc48073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49236, %struct.ScmObj* %lsts48072, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49236, %struct.ScmObj* %_37foldr48070, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49236, %struct.ScmObj* %k48433, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49236, %struct.ScmObj* %_37foldr148064, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49236, %struct.ScmObj* %_37map148060, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49236, %struct.ScmObj* %f48074, i64 6)
%ae49238 = call %struct.ScmObj* @const_init_false()
%argslist57255$_37foldr1480640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58596 = alloca %struct.ScmObj*, align 8
%argslist57255$_37foldr1480641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48072, %struct.ScmObj* %argslist57255$_37foldr1480640)
store volatile %struct.ScmObj* %argslist57255$_37foldr1480641, %struct.ScmObj** %stackaddr$prim58596, align 8
%stackaddr$prim58597 = alloca %struct.ScmObj*, align 8
%argslist57255$_37foldr1480642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49238, %struct.ScmObj* %argslist57255$_37foldr1480641)
store volatile %struct.ScmObj* %argslist57255$_37foldr1480642, %struct.ScmObj** %stackaddr$prim58597, align 8
%stackaddr$prim58598 = alloca %struct.ScmObj*, align 8
%argslist57255$_37foldr1480643 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48234, %struct.ScmObj* %argslist57255$_37foldr1480642)
store volatile %struct.ScmObj* %argslist57255$_37foldr1480643, %struct.ScmObj** %stackaddr$prim58598, align 8
%stackaddr$prim58599 = alloca %struct.ScmObj*, align 8
%argslist57255$_37foldr1480644 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49236, %struct.ScmObj* %argslist57255$_37foldr1480643)
store volatile %struct.ScmObj* %argslist57255$_37foldr1480644, %struct.ScmObj** %stackaddr$prim58599, align 8
%clofunc58600 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148064)
musttail call tailcc void %clofunc58600(%struct.ScmObj* %_37foldr148064, %struct.ScmObj* %argslist57255$_37foldr1480644)
ret void
}

define tailcc void @proc_clo$ae49236(%struct.ScmObj* %env$ae49236,%struct.ScmObj* %current_45args57211) {
%stackaddr$env-ref58601 = alloca %struct.ScmObj*, align 8
%acc48073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49236, i64 0)
store %struct.ScmObj* %acc48073, %struct.ScmObj** %stackaddr$env-ref58601
%stackaddr$env-ref58602 = alloca %struct.ScmObj*, align 8
%lsts48072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49236, i64 1)
store %struct.ScmObj* %lsts48072, %struct.ScmObj** %stackaddr$env-ref58602
%stackaddr$env-ref58603 = alloca %struct.ScmObj*, align 8
%_37foldr48070 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49236, i64 2)
store %struct.ScmObj* %_37foldr48070, %struct.ScmObj** %stackaddr$env-ref58603
%stackaddr$env-ref58604 = alloca %struct.ScmObj*, align 8
%k48433 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49236, i64 3)
store %struct.ScmObj* %k48433, %struct.ScmObj** %stackaddr$env-ref58604
%stackaddr$env-ref58605 = alloca %struct.ScmObj*, align 8
%_37foldr148064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49236, i64 4)
store %struct.ScmObj* %_37foldr148064, %struct.ScmObj** %stackaddr$env-ref58605
%stackaddr$env-ref58606 = alloca %struct.ScmObj*, align 8
%_37map148060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49236, i64 5)
store %struct.ScmObj* %_37map148060, %struct.ScmObj** %stackaddr$env-ref58606
%stackaddr$env-ref58607 = alloca %struct.ScmObj*, align 8
%f48074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49236, i64 6)
store %struct.ScmObj* %f48074, %struct.ScmObj** %stackaddr$env-ref58607
%stackaddr$prim58608 = alloca %struct.ScmObj*, align 8
%_95k48435 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57211)
store volatile %struct.ScmObj* %_95k48435, %struct.ScmObj** %stackaddr$prim58608, align 8
%stackaddr$prim58609 = alloca %struct.ScmObj*, align 8
%current_45args57212 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57211)
store volatile %struct.ScmObj* %current_45args57212, %struct.ScmObj** %stackaddr$prim58609, align 8
%stackaddr$prim58610 = alloca %struct.ScmObj*, align 8
%anf_45bind48235 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57212)
store volatile %struct.ScmObj* %anf_45bind48235, %struct.ScmObj** %stackaddr$prim58610, align 8
%truthy$cmp58611 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48235)
%cmp$cmp58611 = icmp eq i64 %truthy$cmp58611, 1
br i1 %cmp$cmp58611, label %truebranch$cmp58611, label %falsebranch$cmp58611
truebranch$cmp58611:
%ae49247 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57214$k484330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58612 = alloca %struct.ScmObj*, align 8
%argslist57214$k484331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48073, %struct.ScmObj* %argslist57214$k484330)
store volatile %struct.ScmObj* %argslist57214$k484331, %struct.ScmObj** %stackaddr$prim58612, align 8
%stackaddr$prim58613 = alloca %struct.ScmObj*, align 8
%argslist57214$k484332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49247, %struct.ScmObj* %argslist57214$k484331)
store volatile %struct.ScmObj* %argslist57214$k484332, %struct.ScmObj** %stackaddr$prim58613, align 8
%clofunc58614 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48433)
musttail call tailcc void %clofunc58614(%struct.ScmObj* %k48433, %struct.ScmObj* %argslist57214$k484332)
ret void
falsebranch$cmp58611:
%stackaddr$makeclosure58615 = alloca %struct.ScmObj*, align 8
%fptrToInt58616 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49252 to i64
%ae49252 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt58616)
store volatile %struct.ScmObj* %ae49252, %struct.ScmObj** %stackaddr$makeclosure58615, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49252, %struct.ScmObj* %acc48073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49252, %struct.ScmObj* %lsts48072, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49252, %struct.ScmObj* %_37foldr48070, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49252, %struct.ScmObj* %k48433, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49252, %struct.ScmObj* %_37foldr148064, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49252, %struct.ScmObj* %_37map148060, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49252, %struct.ScmObj* %f48074, i64 6)
%ae49253 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58617 = alloca %struct.ScmObj*, align 8
%fptrToInt58618 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49254 to i64
%ae49254 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58618)
store volatile %struct.ScmObj* %ae49254, %struct.ScmObj** %stackaddr$makeclosure58617, align 8
%argslist57254$ae492520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58619 = alloca %struct.ScmObj*, align 8
%argslist57254$ae492521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49254, %struct.ScmObj* %argslist57254$ae492520)
store volatile %struct.ScmObj* %argslist57254$ae492521, %struct.ScmObj** %stackaddr$prim58619, align 8
%stackaddr$prim58620 = alloca %struct.ScmObj*, align 8
%argslist57254$ae492522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49253, %struct.ScmObj* %argslist57254$ae492521)
store volatile %struct.ScmObj* %argslist57254$ae492522, %struct.ScmObj** %stackaddr$prim58620, align 8
%clofunc58621 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49252)
musttail call tailcc void %clofunc58621(%struct.ScmObj* %ae49252, %struct.ScmObj* %argslist57254$ae492522)
ret void
}

define tailcc void @proc_clo$ae49252(%struct.ScmObj* %env$ae49252,%struct.ScmObj* %current_45args57215) {
%stackaddr$env-ref58622 = alloca %struct.ScmObj*, align 8
%acc48073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49252, i64 0)
store %struct.ScmObj* %acc48073, %struct.ScmObj** %stackaddr$env-ref58622
%stackaddr$env-ref58623 = alloca %struct.ScmObj*, align 8
%lsts48072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49252, i64 1)
store %struct.ScmObj* %lsts48072, %struct.ScmObj** %stackaddr$env-ref58623
%stackaddr$env-ref58624 = alloca %struct.ScmObj*, align 8
%_37foldr48070 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49252, i64 2)
store %struct.ScmObj* %_37foldr48070, %struct.ScmObj** %stackaddr$env-ref58624
%stackaddr$env-ref58625 = alloca %struct.ScmObj*, align 8
%k48433 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49252, i64 3)
store %struct.ScmObj* %k48433, %struct.ScmObj** %stackaddr$env-ref58625
%stackaddr$env-ref58626 = alloca %struct.ScmObj*, align 8
%_37foldr148064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49252, i64 4)
store %struct.ScmObj* %_37foldr148064, %struct.ScmObj** %stackaddr$env-ref58626
%stackaddr$env-ref58627 = alloca %struct.ScmObj*, align 8
%_37map148060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49252, i64 5)
store %struct.ScmObj* %_37map148060, %struct.ScmObj** %stackaddr$env-ref58627
%stackaddr$env-ref58628 = alloca %struct.ScmObj*, align 8
%f48074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49252, i64 6)
store %struct.ScmObj* %f48074, %struct.ScmObj** %stackaddr$env-ref58628
%stackaddr$prim58629 = alloca %struct.ScmObj*, align 8
%_95k48436 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57215)
store volatile %struct.ScmObj* %_95k48436, %struct.ScmObj** %stackaddr$prim58629, align 8
%stackaddr$prim58630 = alloca %struct.ScmObj*, align 8
%current_45args57216 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57215)
store volatile %struct.ScmObj* %current_45args57216, %struct.ScmObj** %stackaddr$prim58630, align 8
%stackaddr$prim58631 = alloca %struct.ScmObj*, align 8
%anf_45bind48236 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57216)
store volatile %struct.ScmObj* %anf_45bind48236, %struct.ScmObj** %stackaddr$prim58631, align 8
%stackaddr$makeclosure58632 = alloca %struct.ScmObj*, align 8
%fptrToInt58633 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49273 to i64
%ae49273 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt58633)
store volatile %struct.ScmObj* %ae49273, %struct.ScmObj** %stackaddr$makeclosure58632, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49273, %struct.ScmObj* %acc48073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49273, %struct.ScmObj* %lsts48072, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49273, %struct.ScmObj* %_37foldr48070, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49273, %struct.ScmObj* %k48433, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49273, %struct.ScmObj* %_37foldr148064, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49273, %struct.ScmObj* %_37map148060, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49273, %struct.ScmObj* %f48074, i64 6)
%argslist57249$_37map1480600 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58634 = alloca %struct.ScmObj*, align 8
%argslist57249$_37map1480601 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48072, %struct.ScmObj* %argslist57249$_37map1480600)
store volatile %struct.ScmObj* %argslist57249$_37map1480601, %struct.ScmObj** %stackaddr$prim58634, align 8
%stackaddr$prim58635 = alloca %struct.ScmObj*, align 8
%argslist57249$_37map1480602 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48236, %struct.ScmObj* %argslist57249$_37map1480601)
store volatile %struct.ScmObj* %argslist57249$_37map1480602, %struct.ScmObj** %stackaddr$prim58635, align 8
%stackaddr$prim58636 = alloca %struct.ScmObj*, align 8
%argslist57249$_37map1480603 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49273, %struct.ScmObj* %argslist57249$_37map1480602)
store volatile %struct.ScmObj* %argslist57249$_37map1480603, %struct.ScmObj** %stackaddr$prim58636, align 8
%clofunc58637 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148060)
musttail call tailcc void %clofunc58637(%struct.ScmObj* %_37map148060, %struct.ScmObj* %argslist57249$_37map1480603)
ret void
}

define tailcc void @proc_clo$ae49273(%struct.ScmObj* %env$ae49273,%struct.ScmObj* %current_45args57218) {
%stackaddr$env-ref58638 = alloca %struct.ScmObj*, align 8
%acc48073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49273, i64 0)
store %struct.ScmObj* %acc48073, %struct.ScmObj** %stackaddr$env-ref58638
%stackaddr$env-ref58639 = alloca %struct.ScmObj*, align 8
%lsts48072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49273, i64 1)
store %struct.ScmObj* %lsts48072, %struct.ScmObj** %stackaddr$env-ref58639
%stackaddr$env-ref58640 = alloca %struct.ScmObj*, align 8
%_37foldr48070 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49273, i64 2)
store %struct.ScmObj* %_37foldr48070, %struct.ScmObj** %stackaddr$env-ref58640
%stackaddr$env-ref58641 = alloca %struct.ScmObj*, align 8
%k48433 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49273, i64 3)
store %struct.ScmObj* %k48433, %struct.ScmObj** %stackaddr$env-ref58641
%stackaddr$env-ref58642 = alloca %struct.ScmObj*, align 8
%_37foldr148064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49273, i64 4)
store %struct.ScmObj* %_37foldr148064, %struct.ScmObj** %stackaddr$env-ref58642
%stackaddr$env-ref58643 = alloca %struct.ScmObj*, align 8
%_37map148060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49273, i64 5)
store %struct.ScmObj* %_37map148060, %struct.ScmObj** %stackaddr$env-ref58643
%stackaddr$env-ref58644 = alloca %struct.ScmObj*, align 8
%f48074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49273, i64 6)
store %struct.ScmObj* %f48074, %struct.ScmObj** %stackaddr$env-ref58644
%stackaddr$prim58645 = alloca %struct.ScmObj*, align 8
%_95k48437 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57218)
store volatile %struct.ScmObj* %_95k48437, %struct.ScmObj** %stackaddr$prim58645, align 8
%stackaddr$prim58646 = alloca %struct.ScmObj*, align 8
%current_45args57219 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57218)
store volatile %struct.ScmObj* %current_45args57219, %struct.ScmObj** %stackaddr$prim58646, align 8
%stackaddr$prim58647 = alloca %struct.ScmObj*, align 8
%lsts_4348079 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57219)
store volatile %struct.ScmObj* %lsts_4348079, %struct.ScmObj** %stackaddr$prim58647, align 8
%stackaddr$makeclosure58648 = alloca %struct.ScmObj*, align 8
%fptrToInt58649 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49276 to i64
%ae49276 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt58649)
store volatile %struct.ScmObj* %ae49276, %struct.ScmObj** %stackaddr$makeclosure58648, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49276, %struct.ScmObj* %acc48073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49276, %struct.ScmObj* %lsts48072, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49276, %struct.ScmObj* %_37foldr48070, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49276, %struct.ScmObj* %k48433, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49276, %struct.ScmObj* %_37foldr148064, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49276, %struct.ScmObj* %lsts_4348079, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49276, %struct.ScmObj* %_37map148060, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae49276, %struct.ScmObj* %f48074, i64 7)
%ae49277 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58650 = alloca %struct.ScmObj*, align 8
%fptrToInt58651 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49278 to i64
%ae49278 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58651)
store volatile %struct.ScmObj* %ae49278, %struct.ScmObj** %stackaddr$makeclosure58650, align 8
%argslist57248$ae492760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58652 = alloca %struct.ScmObj*, align 8
%argslist57248$ae492761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49278, %struct.ScmObj* %argslist57248$ae492760)
store volatile %struct.ScmObj* %argslist57248$ae492761, %struct.ScmObj** %stackaddr$prim58652, align 8
%stackaddr$prim58653 = alloca %struct.ScmObj*, align 8
%argslist57248$ae492762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49277, %struct.ScmObj* %argslist57248$ae492761)
store volatile %struct.ScmObj* %argslist57248$ae492762, %struct.ScmObj** %stackaddr$prim58653, align 8
%clofunc58654 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49276)
musttail call tailcc void %clofunc58654(%struct.ScmObj* %ae49276, %struct.ScmObj* %argslist57248$ae492762)
ret void
}

define tailcc void @proc_clo$ae49276(%struct.ScmObj* %env$ae49276,%struct.ScmObj* %current_45args57221) {
%stackaddr$env-ref58655 = alloca %struct.ScmObj*, align 8
%acc48073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49276, i64 0)
store %struct.ScmObj* %acc48073, %struct.ScmObj** %stackaddr$env-ref58655
%stackaddr$env-ref58656 = alloca %struct.ScmObj*, align 8
%lsts48072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49276, i64 1)
store %struct.ScmObj* %lsts48072, %struct.ScmObj** %stackaddr$env-ref58656
%stackaddr$env-ref58657 = alloca %struct.ScmObj*, align 8
%_37foldr48070 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49276, i64 2)
store %struct.ScmObj* %_37foldr48070, %struct.ScmObj** %stackaddr$env-ref58657
%stackaddr$env-ref58658 = alloca %struct.ScmObj*, align 8
%k48433 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49276, i64 3)
store %struct.ScmObj* %k48433, %struct.ScmObj** %stackaddr$env-ref58658
%stackaddr$env-ref58659 = alloca %struct.ScmObj*, align 8
%_37foldr148064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49276, i64 4)
store %struct.ScmObj* %_37foldr148064, %struct.ScmObj** %stackaddr$env-ref58659
%stackaddr$env-ref58660 = alloca %struct.ScmObj*, align 8
%lsts_4348079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49276, i64 5)
store %struct.ScmObj* %lsts_4348079, %struct.ScmObj** %stackaddr$env-ref58660
%stackaddr$env-ref58661 = alloca %struct.ScmObj*, align 8
%_37map148060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49276, i64 6)
store %struct.ScmObj* %_37map148060, %struct.ScmObj** %stackaddr$env-ref58661
%stackaddr$env-ref58662 = alloca %struct.ScmObj*, align 8
%f48074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49276, i64 7)
store %struct.ScmObj* %f48074, %struct.ScmObj** %stackaddr$env-ref58662
%stackaddr$prim58663 = alloca %struct.ScmObj*, align 8
%_95k48438 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57221)
store volatile %struct.ScmObj* %_95k48438, %struct.ScmObj** %stackaddr$prim58663, align 8
%stackaddr$prim58664 = alloca %struct.ScmObj*, align 8
%current_45args57222 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57221)
store volatile %struct.ScmObj* %current_45args57222, %struct.ScmObj** %stackaddr$prim58664, align 8
%stackaddr$prim58665 = alloca %struct.ScmObj*, align 8
%anf_45bind48237 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57222)
store volatile %struct.ScmObj* %anf_45bind48237, %struct.ScmObj** %stackaddr$prim58665, align 8
%stackaddr$makeclosure58666 = alloca %struct.ScmObj*, align 8
%fptrToInt58667 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49297 to i64
%ae49297 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt58667)
store volatile %struct.ScmObj* %ae49297, %struct.ScmObj** %stackaddr$makeclosure58666, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49297, %struct.ScmObj* %acc48073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49297, %struct.ScmObj* %_37foldr48070, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49297, %struct.ScmObj* %k48433, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49297, %struct.ScmObj* %_37foldr148064, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49297, %struct.ScmObj* %lsts_4348079, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49297, %struct.ScmObj* %f48074, i64 5)
%argslist57243$_37map1480600 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58668 = alloca %struct.ScmObj*, align 8
%argslist57243$_37map1480601 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48072, %struct.ScmObj* %argslist57243$_37map1480600)
store volatile %struct.ScmObj* %argslist57243$_37map1480601, %struct.ScmObj** %stackaddr$prim58668, align 8
%stackaddr$prim58669 = alloca %struct.ScmObj*, align 8
%argslist57243$_37map1480602 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48237, %struct.ScmObj* %argslist57243$_37map1480601)
store volatile %struct.ScmObj* %argslist57243$_37map1480602, %struct.ScmObj** %stackaddr$prim58669, align 8
%stackaddr$prim58670 = alloca %struct.ScmObj*, align 8
%argslist57243$_37map1480603 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49297, %struct.ScmObj* %argslist57243$_37map1480602)
store volatile %struct.ScmObj* %argslist57243$_37map1480603, %struct.ScmObj** %stackaddr$prim58670, align 8
%clofunc58671 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148060)
musttail call tailcc void %clofunc58671(%struct.ScmObj* %_37map148060, %struct.ScmObj* %argslist57243$_37map1480603)
ret void
}

define tailcc void @proc_clo$ae49297(%struct.ScmObj* %env$ae49297,%struct.ScmObj* %current_45args57224) {
%stackaddr$env-ref58672 = alloca %struct.ScmObj*, align 8
%acc48073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49297, i64 0)
store %struct.ScmObj* %acc48073, %struct.ScmObj** %stackaddr$env-ref58672
%stackaddr$env-ref58673 = alloca %struct.ScmObj*, align 8
%_37foldr48070 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49297, i64 1)
store %struct.ScmObj* %_37foldr48070, %struct.ScmObj** %stackaddr$env-ref58673
%stackaddr$env-ref58674 = alloca %struct.ScmObj*, align 8
%k48433 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49297, i64 2)
store %struct.ScmObj* %k48433, %struct.ScmObj** %stackaddr$env-ref58674
%stackaddr$env-ref58675 = alloca %struct.ScmObj*, align 8
%_37foldr148064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49297, i64 3)
store %struct.ScmObj* %_37foldr148064, %struct.ScmObj** %stackaddr$env-ref58675
%stackaddr$env-ref58676 = alloca %struct.ScmObj*, align 8
%lsts_4348079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49297, i64 4)
store %struct.ScmObj* %lsts_4348079, %struct.ScmObj** %stackaddr$env-ref58676
%stackaddr$env-ref58677 = alloca %struct.ScmObj*, align 8
%f48074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49297, i64 5)
store %struct.ScmObj* %f48074, %struct.ScmObj** %stackaddr$env-ref58677
%stackaddr$prim58678 = alloca %struct.ScmObj*, align 8
%_95k48439 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57224)
store volatile %struct.ScmObj* %_95k48439, %struct.ScmObj** %stackaddr$prim58678, align 8
%stackaddr$prim58679 = alloca %struct.ScmObj*, align 8
%current_45args57225 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57224)
store volatile %struct.ScmObj* %current_45args57225, %struct.ScmObj** %stackaddr$prim58679, align 8
%stackaddr$prim58680 = alloca %struct.ScmObj*, align 8
%vs48077 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57225)
store volatile %struct.ScmObj* %vs48077, %struct.ScmObj** %stackaddr$prim58680, align 8
%stackaddr$makeclosure58681 = alloca %struct.ScmObj*, align 8
%fptrToInt58682 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49300 to i64
%ae49300 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt58682)
store volatile %struct.ScmObj* %ae49300, %struct.ScmObj** %stackaddr$makeclosure58681, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49300, %struct.ScmObj* %acc48073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49300, %struct.ScmObj* %_37foldr48070, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49300, %struct.ScmObj* %k48433, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49300, %struct.ScmObj* %_37foldr148064, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49300, %struct.ScmObj* %lsts_4348079, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49300, %struct.ScmObj* %vs48077, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49300, %struct.ScmObj* %f48074, i64 6)
%ae49301 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58683 = alloca %struct.ScmObj*, align 8
%fptrToInt58684 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49302 to i64
%ae49302 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58684)
store volatile %struct.ScmObj* %ae49302, %struct.ScmObj** %stackaddr$makeclosure58683, align 8
%argslist57242$ae493000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58685 = alloca %struct.ScmObj*, align 8
%argslist57242$ae493001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49302, %struct.ScmObj* %argslist57242$ae493000)
store volatile %struct.ScmObj* %argslist57242$ae493001, %struct.ScmObj** %stackaddr$prim58685, align 8
%stackaddr$prim58686 = alloca %struct.ScmObj*, align 8
%argslist57242$ae493002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49301, %struct.ScmObj* %argslist57242$ae493001)
store volatile %struct.ScmObj* %argslist57242$ae493002, %struct.ScmObj** %stackaddr$prim58686, align 8
%clofunc58687 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49300)
musttail call tailcc void %clofunc58687(%struct.ScmObj* %ae49300, %struct.ScmObj* %argslist57242$ae493002)
ret void
}

define tailcc void @proc_clo$ae49300(%struct.ScmObj* %env$ae49300,%struct.ScmObj* %current_45args57227) {
%stackaddr$env-ref58688 = alloca %struct.ScmObj*, align 8
%acc48073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49300, i64 0)
store %struct.ScmObj* %acc48073, %struct.ScmObj** %stackaddr$env-ref58688
%stackaddr$env-ref58689 = alloca %struct.ScmObj*, align 8
%_37foldr48070 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49300, i64 1)
store %struct.ScmObj* %_37foldr48070, %struct.ScmObj** %stackaddr$env-ref58689
%stackaddr$env-ref58690 = alloca %struct.ScmObj*, align 8
%k48433 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49300, i64 2)
store %struct.ScmObj* %k48433, %struct.ScmObj** %stackaddr$env-ref58690
%stackaddr$env-ref58691 = alloca %struct.ScmObj*, align 8
%_37foldr148064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49300, i64 3)
store %struct.ScmObj* %_37foldr148064, %struct.ScmObj** %stackaddr$env-ref58691
%stackaddr$env-ref58692 = alloca %struct.ScmObj*, align 8
%lsts_4348079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49300, i64 4)
store %struct.ScmObj* %lsts_4348079, %struct.ScmObj** %stackaddr$env-ref58692
%stackaddr$env-ref58693 = alloca %struct.ScmObj*, align 8
%vs48077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49300, i64 5)
store %struct.ScmObj* %vs48077, %struct.ScmObj** %stackaddr$env-ref58693
%stackaddr$env-ref58694 = alloca %struct.ScmObj*, align 8
%f48074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49300, i64 6)
store %struct.ScmObj* %f48074, %struct.ScmObj** %stackaddr$env-ref58694
%stackaddr$prim58695 = alloca %struct.ScmObj*, align 8
%_95k48440 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57227)
store volatile %struct.ScmObj* %_95k48440, %struct.ScmObj** %stackaddr$prim58695, align 8
%stackaddr$prim58696 = alloca %struct.ScmObj*, align 8
%current_45args57228 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57227)
store volatile %struct.ScmObj* %current_45args57228, %struct.ScmObj** %stackaddr$prim58696, align 8
%stackaddr$prim58697 = alloca %struct.ScmObj*, align 8
%anf_45bind48238 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57228)
store volatile %struct.ScmObj* %anf_45bind48238, %struct.ScmObj** %stackaddr$prim58697, align 8
%stackaddr$prim58698 = alloca %struct.ScmObj*, align 8
%anf_45bind48239 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48073, %struct.ScmObj* %lsts_4348079)
store volatile %struct.ScmObj* %anf_45bind48239, %struct.ScmObj** %stackaddr$prim58698, align 8
%stackaddr$prim58699 = alloca %struct.ScmObj*, align 8
%anf_45bind48240 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48074, %struct.ScmObj* %anf_45bind48239)
store volatile %struct.ScmObj* %anf_45bind48240, %struct.ScmObj** %stackaddr$prim58699, align 8
%stackaddr$makeclosure58700 = alloca %struct.ScmObj*, align 8
%fptrToInt58701 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49326 to i64
%ae49326 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt58701)
store volatile %struct.ScmObj* %ae49326, %struct.ScmObj** %stackaddr$makeclosure58700, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49326, %struct.ScmObj* %k48433, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49326, %struct.ScmObj* %_37foldr148064, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49326, %struct.ScmObj* %anf_45bind48238, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49326, %struct.ScmObj* %vs48077, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49326, %struct.ScmObj* %f48074, i64 4)
%stackaddr$prim58702 = alloca %struct.ScmObj*, align 8
%cpsargs48444 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49326, %struct.ScmObj* %anf_45bind48240)
store volatile %struct.ScmObj* %cpsargs48444, %struct.ScmObj** %stackaddr$prim58702, align 8
%clofunc58703 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr48070)
musttail call tailcc void %clofunc58703(%struct.ScmObj* %_37foldr48070, %struct.ScmObj* %cpsargs48444)
ret void
}

define tailcc void @proc_clo$ae49326(%struct.ScmObj* %env$ae49326,%struct.ScmObj* %current_45args57230) {
%stackaddr$env-ref58704 = alloca %struct.ScmObj*, align 8
%k48433 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49326, i64 0)
store %struct.ScmObj* %k48433, %struct.ScmObj** %stackaddr$env-ref58704
%stackaddr$env-ref58705 = alloca %struct.ScmObj*, align 8
%_37foldr148064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49326, i64 1)
store %struct.ScmObj* %_37foldr148064, %struct.ScmObj** %stackaddr$env-ref58705
%stackaddr$env-ref58706 = alloca %struct.ScmObj*, align 8
%anf_45bind48238 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49326, i64 2)
store %struct.ScmObj* %anf_45bind48238, %struct.ScmObj** %stackaddr$env-ref58706
%stackaddr$env-ref58707 = alloca %struct.ScmObj*, align 8
%vs48077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49326, i64 3)
store %struct.ScmObj* %vs48077, %struct.ScmObj** %stackaddr$env-ref58707
%stackaddr$env-ref58708 = alloca %struct.ScmObj*, align 8
%f48074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49326, i64 4)
store %struct.ScmObj* %f48074, %struct.ScmObj** %stackaddr$env-ref58708
%stackaddr$prim58709 = alloca %struct.ScmObj*, align 8
%_95k48441 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57230)
store volatile %struct.ScmObj* %_95k48441, %struct.ScmObj** %stackaddr$prim58709, align 8
%stackaddr$prim58710 = alloca %struct.ScmObj*, align 8
%current_45args57231 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57230)
store volatile %struct.ScmObj* %current_45args57231, %struct.ScmObj** %stackaddr$prim58710, align 8
%stackaddr$prim58711 = alloca %struct.ScmObj*, align 8
%anf_45bind48241 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57231)
store volatile %struct.ScmObj* %anf_45bind48241, %struct.ScmObj** %stackaddr$prim58711, align 8
%ae49331 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58712 = alloca %struct.ScmObj*, align 8
%anf_45bind48242 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48241, %struct.ScmObj* %ae49331)
store volatile %struct.ScmObj* %anf_45bind48242, %struct.ScmObj** %stackaddr$prim58712, align 8
%stackaddr$makeclosure58713 = alloca %struct.ScmObj*, align 8
%fptrToInt58714 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49333 to i64
%ae49333 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58714)
store volatile %struct.ScmObj* %ae49333, %struct.ScmObj** %stackaddr$makeclosure58713, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49333, %struct.ScmObj* %k48433, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49333, %struct.ScmObj* %f48074, i64 1)
%argslist57236$_37foldr1480640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58715 = alloca %struct.ScmObj*, align 8
%argslist57236$_37foldr1480641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs48077, %struct.ScmObj* %argslist57236$_37foldr1480640)
store volatile %struct.ScmObj* %argslist57236$_37foldr1480641, %struct.ScmObj** %stackaddr$prim58715, align 8
%stackaddr$prim58716 = alloca %struct.ScmObj*, align 8
%argslist57236$_37foldr1480642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48242, %struct.ScmObj* %argslist57236$_37foldr1480641)
store volatile %struct.ScmObj* %argslist57236$_37foldr1480642, %struct.ScmObj** %stackaddr$prim58716, align 8
%stackaddr$prim58717 = alloca %struct.ScmObj*, align 8
%argslist57236$_37foldr1480643 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48238, %struct.ScmObj* %argslist57236$_37foldr1480642)
store volatile %struct.ScmObj* %argslist57236$_37foldr1480643, %struct.ScmObj** %stackaddr$prim58717, align 8
%stackaddr$prim58718 = alloca %struct.ScmObj*, align 8
%argslist57236$_37foldr1480644 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49333, %struct.ScmObj* %argslist57236$_37foldr1480643)
store volatile %struct.ScmObj* %argslist57236$_37foldr1480644, %struct.ScmObj** %stackaddr$prim58718, align 8
%clofunc58719 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148064)
musttail call tailcc void %clofunc58719(%struct.ScmObj* %_37foldr148064, %struct.ScmObj* %argslist57236$_37foldr1480644)
ret void
}

define tailcc void @proc_clo$ae49333(%struct.ScmObj* %env$ae49333,%struct.ScmObj* %current_45args57233) {
%stackaddr$env-ref58720 = alloca %struct.ScmObj*, align 8
%k48433 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49333, i64 0)
store %struct.ScmObj* %k48433, %struct.ScmObj** %stackaddr$env-ref58720
%stackaddr$env-ref58721 = alloca %struct.ScmObj*, align 8
%f48074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49333, i64 1)
store %struct.ScmObj* %f48074, %struct.ScmObj** %stackaddr$env-ref58721
%stackaddr$prim58722 = alloca %struct.ScmObj*, align 8
%_95k48442 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57233)
store volatile %struct.ScmObj* %_95k48442, %struct.ScmObj** %stackaddr$prim58722, align 8
%stackaddr$prim58723 = alloca %struct.ScmObj*, align 8
%current_45args57234 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57233)
store volatile %struct.ScmObj* %current_45args57234, %struct.ScmObj** %stackaddr$prim58723, align 8
%stackaddr$prim58724 = alloca %struct.ScmObj*, align 8
%anf_45bind48243 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57234)
store volatile %struct.ScmObj* %anf_45bind48243, %struct.ScmObj** %stackaddr$prim58724, align 8
%stackaddr$prim58725 = alloca %struct.ScmObj*, align 8
%cpsargs48443 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48433, %struct.ScmObj* %anf_45bind48243)
store volatile %struct.ScmObj* %cpsargs48443, %struct.ScmObj** %stackaddr$prim58725, align 8
%clofunc58726 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48074)
musttail call tailcc void %clofunc58726(%struct.ScmObj* %f48074, %struct.ScmObj* %cpsargs48443)
ret void
}

define tailcc void @proc_clo$ae49302(%struct.ScmObj* %env$ae49302,%struct.ScmObj* %current_45args57237) {
%stackaddr$prim58727 = alloca %struct.ScmObj*, align 8
%k48445 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57237)
store volatile %struct.ScmObj* %k48445, %struct.ScmObj** %stackaddr$prim58727, align 8
%stackaddr$prim58728 = alloca %struct.ScmObj*, align 8
%current_45args57238 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57237)
store volatile %struct.ScmObj* %current_45args57238, %struct.ScmObj** %stackaddr$prim58728, align 8
%stackaddr$prim58729 = alloca %struct.ScmObj*, align 8
%a48082 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57238)
store volatile %struct.ScmObj* %a48082, %struct.ScmObj** %stackaddr$prim58729, align 8
%stackaddr$prim58730 = alloca %struct.ScmObj*, align 8
%current_45args57239 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57238)
store volatile %struct.ScmObj* %current_45args57239, %struct.ScmObj** %stackaddr$prim58730, align 8
%stackaddr$prim58731 = alloca %struct.ScmObj*, align 8
%b48081 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57239)
store volatile %struct.ScmObj* %b48081, %struct.ScmObj** %stackaddr$prim58731, align 8
%stackaddr$prim58732 = alloca %struct.ScmObj*, align 8
%cpsprim48446 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a48082, %struct.ScmObj* %b48081)
store volatile %struct.ScmObj* %cpsprim48446, %struct.ScmObj** %stackaddr$prim58732, align 8
%ae49306 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57241$k484450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58733 = alloca %struct.ScmObj*, align 8
%argslist57241$k484451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48446, %struct.ScmObj* %argslist57241$k484450)
store volatile %struct.ScmObj* %argslist57241$k484451, %struct.ScmObj** %stackaddr$prim58733, align 8
%stackaddr$prim58734 = alloca %struct.ScmObj*, align 8
%argslist57241$k484452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49306, %struct.ScmObj* %argslist57241$k484451)
store volatile %struct.ScmObj* %argslist57241$k484452, %struct.ScmObj** %stackaddr$prim58734, align 8
%clofunc58735 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48445)
musttail call tailcc void %clofunc58735(%struct.ScmObj* %k48445, %struct.ScmObj* %argslist57241$k484452)
ret void
}

define tailcc void @proc_clo$ae49278(%struct.ScmObj* %env$ae49278,%struct.ScmObj* %current_45args57244) {
%stackaddr$prim58736 = alloca %struct.ScmObj*, align 8
%k48447 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57244)
store volatile %struct.ScmObj* %k48447, %struct.ScmObj** %stackaddr$prim58736, align 8
%stackaddr$prim58737 = alloca %struct.ScmObj*, align 8
%current_45args57245 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57244)
store volatile %struct.ScmObj* %current_45args57245, %struct.ScmObj** %stackaddr$prim58737, align 8
%stackaddr$prim58738 = alloca %struct.ScmObj*, align 8
%x48078 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57245)
store volatile %struct.ScmObj* %x48078, %struct.ScmObj** %stackaddr$prim58738, align 8
%stackaddr$prim58739 = alloca %struct.ScmObj*, align 8
%cpsprim48448 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48078)
store volatile %struct.ScmObj* %cpsprim48448, %struct.ScmObj** %stackaddr$prim58739, align 8
%ae49281 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57247$k484470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58740 = alloca %struct.ScmObj*, align 8
%argslist57247$k484471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48448, %struct.ScmObj* %argslist57247$k484470)
store volatile %struct.ScmObj* %argslist57247$k484471, %struct.ScmObj** %stackaddr$prim58740, align 8
%stackaddr$prim58741 = alloca %struct.ScmObj*, align 8
%argslist57247$k484472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49281, %struct.ScmObj* %argslist57247$k484471)
store volatile %struct.ScmObj* %argslist57247$k484472, %struct.ScmObj** %stackaddr$prim58741, align 8
%clofunc58742 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48447)
musttail call tailcc void %clofunc58742(%struct.ScmObj* %k48447, %struct.ScmObj* %argslist57247$k484472)
ret void
}

define tailcc void @proc_clo$ae49254(%struct.ScmObj* %env$ae49254,%struct.ScmObj* %current_45args57250) {
%stackaddr$prim58743 = alloca %struct.ScmObj*, align 8
%k48449 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57250)
store volatile %struct.ScmObj* %k48449, %struct.ScmObj** %stackaddr$prim58743, align 8
%stackaddr$prim58744 = alloca %struct.ScmObj*, align 8
%current_45args57251 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57250)
store volatile %struct.ScmObj* %current_45args57251, %struct.ScmObj** %stackaddr$prim58744, align 8
%stackaddr$prim58745 = alloca %struct.ScmObj*, align 8
%x48080 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57251)
store volatile %struct.ScmObj* %x48080, %struct.ScmObj** %stackaddr$prim58745, align 8
%stackaddr$prim58746 = alloca %struct.ScmObj*, align 8
%cpsprim48450 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48080)
store volatile %struct.ScmObj* %cpsprim48450, %struct.ScmObj** %stackaddr$prim58746, align 8
%ae49257 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57253$k484490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58747 = alloca %struct.ScmObj*, align 8
%argslist57253$k484491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48450, %struct.ScmObj* %argslist57253$k484490)
store volatile %struct.ScmObj* %argslist57253$k484491, %struct.ScmObj** %stackaddr$prim58747, align 8
%stackaddr$prim58748 = alloca %struct.ScmObj*, align 8
%argslist57253$k484492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49257, %struct.ScmObj* %argslist57253$k484491)
store volatile %struct.ScmObj* %argslist57253$k484492, %struct.ScmObj** %stackaddr$prim58748, align 8
%clofunc58749 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48449)
musttail call tailcc void %clofunc58749(%struct.ScmObj* %k48449, %struct.ScmObj* %argslist57253$k484492)
ret void
}

define tailcc void @proc_clo$ae49206(%struct.ScmObj* %env$ae49206,%struct.ScmObj* %current_45args57256) {
%stackaddr$prim58750 = alloca %struct.ScmObj*, align 8
%k48451 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57256)
store volatile %struct.ScmObj* %k48451, %struct.ScmObj** %stackaddr$prim58750, align 8
%stackaddr$prim58751 = alloca %struct.ScmObj*, align 8
%current_45args57257 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57256)
store volatile %struct.ScmObj* %current_45args57257, %struct.ScmObj** %stackaddr$prim58751, align 8
%stackaddr$prim58752 = alloca %struct.ScmObj*, align 8
%lst48076 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57257)
store volatile %struct.ScmObj* %lst48076, %struct.ScmObj** %stackaddr$prim58752, align 8
%stackaddr$prim58753 = alloca %struct.ScmObj*, align 8
%current_45args57258 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57257)
store volatile %struct.ScmObj* %current_45args57258, %struct.ScmObj** %stackaddr$prim58753, align 8
%stackaddr$prim58754 = alloca %struct.ScmObj*, align 8
%b48075 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57258)
store volatile %struct.ScmObj* %b48075, %struct.ScmObj** %stackaddr$prim58754, align 8
%truthy$cmp58755 = call i64 @is_truthy_value(%struct.ScmObj* %b48075)
%cmp$cmp58755 = icmp eq i64 %truthy$cmp58755, 1
br i1 %cmp$cmp58755, label %truebranch$cmp58755, label %falsebranch$cmp58755
truebranch$cmp58755:
%ae49209 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57260$k484510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58756 = alloca %struct.ScmObj*, align 8
%argslist57260$k484511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b48075, %struct.ScmObj* %argslist57260$k484510)
store volatile %struct.ScmObj* %argslist57260$k484511, %struct.ScmObj** %stackaddr$prim58756, align 8
%stackaddr$prim58757 = alloca %struct.ScmObj*, align 8
%argslist57260$k484512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49209, %struct.ScmObj* %argslist57260$k484511)
store volatile %struct.ScmObj* %argslist57260$k484512, %struct.ScmObj** %stackaddr$prim58757, align 8
%clofunc58758 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48451)
musttail call tailcc void %clofunc58758(%struct.ScmObj* %k48451, %struct.ScmObj* %argslist57260$k484512)
ret void
falsebranch$cmp58755:
%stackaddr$prim58759 = alloca %struct.ScmObj*, align 8
%cpsprim48452 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48076)
store volatile %struct.ScmObj* %cpsprim48452, %struct.ScmObj** %stackaddr$prim58759, align 8
%ae49216 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57261$k484510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58760 = alloca %struct.ScmObj*, align 8
%argslist57261$k484511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48452, %struct.ScmObj* %argslist57261$k484510)
store volatile %struct.ScmObj* %argslist57261$k484511, %struct.ScmObj** %stackaddr$prim58760, align 8
%stackaddr$prim58761 = alloca %struct.ScmObj*, align 8
%argslist57261$k484512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49216, %struct.ScmObj* %argslist57261$k484511)
store volatile %struct.ScmObj* %argslist57261$k484512, %struct.ScmObj** %stackaddr$prim58761, align 8
%clofunc58762 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48451)
musttail call tailcc void %clofunc58762(%struct.ScmObj* %k48451, %struct.ScmObj* %argslist57261$k484512)
ret void
}

define tailcc void @proc_clo$ae49163(%struct.ScmObj* %env$ae49163,%struct.ScmObj* %current_45args57265) {
%stackaddr$env-ref58763 = alloca %struct.ScmObj*, align 8
%_37take48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49163, i64 0)
store %struct.ScmObj* %_37take48056, %struct.ScmObj** %stackaddr$env-ref58763
%stackaddr$env-ref58764 = alloca %struct.ScmObj*, align 8
%_37length48053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49163, i64 1)
store %struct.ScmObj* %_37length48053, %struct.ScmObj** %stackaddr$env-ref58764
%stackaddr$prim58765 = alloca %struct.ScmObj*, align 8
%k48453 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57265)
store volatile %struct.ScmObj* %k48453, %struct.ScmObj** %stackaddr$prim58765, align 8
%stackaddr$prim58766 = alloca %struct.ScmObj*, align 8
%current_45args57266 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57265)
store volatile %struct.ScmObj* %current_45args57266, %struct.ScmObj** %stackaddr$prim58766, align 8
%stackaddr$prim58767 = alloca %struct.ScmObj*, align 8
%lst48085 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57266)
store volatile %struct.ScmObj* %lst48085, %struct.ScmObj** %stackaddr$prim58767, align 8
%stackaddr$prim58768 = alloca %struct.ScmObj*, align 8
%current_45args57267 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57266)
store volatile %struct.ScmObj* %current_45args57267, %struct.ScmObj** %stackaddr$prim58768, align 8
%stackaddr$prim58769 = alloca %struct.ScmObj*, align 8
%n48084 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57267)
store volatile %struct.ScmObj* %n48084, %struct.ScmObj** %stackaddr$prim58769, align 8
%stackaddr$makeclosure58770 = alloca %struct.ScmObj*, align 8
%fptrToInt58771 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49165 to i64
%ae49165 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt58771)
store volatile %struct.ScmObj* %ae49165, %struct.ScmObj** %stackaddr$makeclosure58770, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49165, %struct.ScmObj* %lst48085, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49165, %struct.ScmObj* %k48453, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49165, %struct.ScmObj* %_37take48056, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49165, %struct.ScmObj* %n48084, i64 3)
%argslist57273$_37length480530 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58772 = alloca %struct.ScmObj*, align 8
%argslist57273$_37length480531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48085, %struct.ScmObj* %argslist57273$_37length480530)
store volatile %struct.ScmObj* %argslist57273$_37length480531, %struct.ScmObj** %stackaddr$prim58772, align 8
%stackaddr$prim58773 = alloca %struct.ScmObj*, align 8
%argslist57273$_37length480532 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49165, %struct.ScmObj* %argslist57273$_37length480531)
store volatile %struct.ScmObj* %argslist57273$_37length480532, %struct.ScmObj** %stackaddr$prim58773, align 8
%clofunc58774 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length48053)
musttail call tailcc void %clofunc58774(%struct.ScmObj* %_37length48053, %struct.ScmObj* %argslist57273$_37length480532)
ret void
}

define tailcc void @proc_clo$ae49165(%struct.ScmObj* %env$ae49165,%struct.ScmObj* %current_45args57269) {
%stackaddr$env-ref58775 = alloca %struct.ScmObj*, align 8
%lst48085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49165, i64 0)
store %struct.ScmObj* %lst48085, %struct.ScmObj** %stackaddr$env-ref58775
%stackaddr$env-ref58776 = alloca %struct.ScmObj*, align 8
%k48453 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49165, i64 1)
store %struct.ScmObj* %k48453, %struct.ScmObj** %stackaddr$env-ref58776
%stackaddr$env-ref58777 = alloca %struct.ScmObj*, align 8
%_37take48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49165, i64 2)
store %struct.ScmObj* %_37take48056, %struct.ScmObj** %stackaddr$env-ref58777
%stackaddr$env-ref58778 = alloca %struct.ScmObj*, align 8
%n48084 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49165, i64 3)
store %struct.ScmObj* %n48084, %struct.ScmObj** %stackaddr$env-ref58778
%stackaddr$prim58779 = alloca %struct.ScmObj*, align 8
%_95k48454 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57269)
store volatile %struct.ScmObj* %_95k48454, %struct.ScmObj** %stackaddr$prim58779, align 8
%stackaddr$prim58780 = alloca %struct.ScmObj*, align 8
%current_45args57270 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57269)
store volatile %struct.ScmObj* %current_45args57270, %struct.ScmObj** %stackaddr$prim58780, align 8
%stackaddr$prim58781 = alloca %struct.ScmObj*, align 8
%anf_45bind48230 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57270)
store volatile %struct.ScmObj* %anf_45bind48230, %struct.ScmObj** %stackaddr$prim58781, align 8
%stackaddr$prim58782 = alloca %struct.ScmObj*, align 8
%anf_45bind48231 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind48230, %struct.ScmObj* %n48084)
store volatile %struct.ScmObj* %anf_45bind48231, %struct.ScmObj** %stackaddr$prim58782, align 8
%argslist57272$_37take480560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58783 = alloca %struct.ScmObj*, align 8
%argslist57272$_37take480561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48231, %struct.ScmObj* %argslist57272$_37take480560)
store volatile %struct.ScmObj* %argslist57272$_37take480561, %struct.ScmObj** %stackaddr$prim58783, align 8
%stackaddr$prim58784 = alloca %struct.ScmObj*, align 8
%argslist57272$_37take480562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48085, %struct.ScmObj* %argslist57272$_37take480561)
store volatile %struct.ScmObj* %argslist57272$_37take480562, %struct.ScmObj** %stackaddr$prim58784, align 8
%stackaddr$prim58785 = alloca %struct.ScmObj*, align 8
%argslist57272$_37take480563 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48453, %struct.ScmObj* %argslist57272$_37take480562)
store volatile %struct.ScmObj* %argslist57272$_37take480563, %struct.ScmObj** %stackaddr$prim58785, align 8
%clofunc58786 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take48056)
musttail call tailcc void %clofunc58786(%struct.ScmObj* %_37take48056, %struct.ScmObj* %argslist57272$_37take480563)
ret void
}

define tailcc void @proc_clo$ae49109(%struct.ScmObj* %env$ae49109,%struct.ScmObj* %current_45args57275) {
%stackaddr$env-ref58787 = alloca %struct.ScmObj*, align 8
%_37foldl148048 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49109, i64 0)
store %struct.ScmObj* %_37foldl148048, %struct.ScmObj** %stackaddr$env-ref58787
%stackaddr$prim58788 = alloca %struct.ScmObj*, align 8
%k48455 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57275)
store volatile %struct.ScmObj* %k48455, %struct.ScmObj** %stackaddr$prim58788, align 8
%stackaddr$prim58789 = alloca %struct.ScmObj*, align 8
%current_45args57276 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57275)
store volatile %struct.ScmObj* %current_45args57276, %struct.ScmObj** %stackaddr$prim58789, align 8
%stackaddr$prim58790 = alloca %struct.ScmObj*, align 8
%lst48087 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57276)
store volatile %struct.ScmObj* %lst48087, %struct.ScmObj** %stackaddr$prim58790, align 8
%stackaddr$makeclosure58791 = alloca %struct.ScmObj*, align 8
%fptrToInt58792 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49110 to i64
%ae49110 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58792)
store volatile %struct.ScmObj* %ae49110, %struct.ScmObj** %stackaddr$makeclosure58791, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49110, %struct.ScmObj* %lst48087, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49110, %struct.ScmObj* %k48455, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49110, %struct.ScmObj* %_37foldl148048, i64 2)
%ae49111 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58793 = alloca %struct.ScmObj*, align 8
%fptrToInt58794 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49112 to i64
%ae49112 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58794)
store volatile %struct.ScmObj* %ae49112, %struct.ScmObj** %stackaddr$makeclosure58793, align 8
%argslist57287$ae491100 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58795 = alloca %struct.ScmObj*, align 8
%argslist57287$ae491101 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49112, %struct.ScmObj* %argslist57287$ae491100)
store volatile %struct.ScmObj* %argslist57287$ae491101, %struct.ScmObj** %stackaddr$prim58795, align 8
%stackaddr$prim58796 = alloca %struct.ScmObj*, align 8
%argslist57287$ae491102 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49111, %struct.ScmObj* %argslist57287$ae491101)
store volatile %struct.ScmObj* %argslist57287$ae491102, %struct.ScmObj** %stackaddr$prim58796, align 8
%clofunc58797 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49110)
musttail call tailcc void %clofunc58797(%struct.ScmObj* %ae49110, %struct.ScmObj* %argslist57287$ae491102)
ret void
}

define tailcc void @proc_clo$ae49110(%struct.ScmObj* %env$ae49110,%struct.ScmObj* %current_45args57278) {
%stackaddr$env-ref58798 = alloca %struct.ScmObj*, align 8
%lst48087 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49110, i64 0)
store %struct.ScmObj* %lst48087, %struct.ScmObj** %stackaddr$env-ref58798
%stackaddr$env-ref58799 = alloca %struct.ScmObj*, align 8
%k48455 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49110, i64 1)
store %struct.ScmObj* %k48455, %struct.ScmObj** %stackaddr$env-ref58799
%stackaddr$env-ref58800 = alloca %struct.ScmObj*, align 8
%_37foldl148048 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49110, i64 2)
store %struct.ScmObj* %_37foldl148048, %struct.ScmObj** %stackaddr$env-ref58800
%stackaddr$prim58801 = alloca %struct.ScmObj*, align 8
%_95k48456 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57278)
store volatile %struct.ScmObj* %_95k48456, %struct.ScmObj** %stackaddr$prim58801, align 8
%stackaddr$prim58802 = alloca %struct.ScmObj*, align 8
%current_45args57279 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57278)
store volatile %struct.ScmObj* %current_45args57279, %struct.ScmObj** %stackaddr$prim58802, align 8
%stackaddr$prim58803 = alloca %struct.ScmObj*, align 8
%anf_45bind48229 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57279)
store volatile %struct.ScmObj* %anf_45bind48229, %struct.ScmObj** %stackaddr$prim58803, align 8
%ae49131 = call %struct.ScmObj* @const_init_null()
%argslist57281$_37foldl1480480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58804 = alloca %struct.ScmObj*, align 8
%argslist57281$_37foldl1480481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48087, %struct.ScmObj* %argslist57281$_37foldl1480480)
store volatile %struct.ScmObj* %argslist57281$_37foldl1480481, %struct.ScmObj** %stackaddr$prim58804, align 8
%stackaddr$prim58805 = alloca %struct.ScmObj*, align 8
%argslist57281$_37foldl1480482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49131, %struct.ScmObj* %argslist57281$_37foldl1480481)
store volatile %struct.ScmObj* %argslist57281$_37foldl1480482, %struct.ScmObj** %stackaddr$prim58805, align 8
%stackaddr$prim58806 = alloca %struct.ScmObj*, align 8
%argslist57281$_37foldl1480483 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48229, %struct.ScmObj* %argslist57281$_37foldl1480482)
store volatile %struct.ScmObj* %argslist57281$_37foldl1480483, %struct.ScmObj** %stackaddr$prim58806, align 8
%stackaddr$prim58807 = alloca %struct.ScmObj*, align 8
%argslist57281$_37foldl1480484 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48455, %struct.ScmObj* %argslist57281$_37foldl1480483)
store volatile %struct.ScmObj* %argslist57281$_37foldl1480484, %struct.ScmObj** %stackaddr$prim58807, align 8
%clofunc58808 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148048)
musttail call tailcc void %clofunc58808(%struct.ScmObj* %_37foldl148048, %struct.ScmObj* %argslist57281$_37foldl1480484)
ret void
}

define tailcc void @proc_clo$ae49112(%struct.ScmObj* %env$ae49112,%struct.ScmObj* %current_45args57282) {
%stackaddr$prim58809 = alloca %struct.ScmObj*, align 8
%k48457 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57282)
store volatile %struct.ScmObj* %k48457, %struct.ScmObj** %stackaddr$prim58809, align 8
%stackaddr$prim58810 = alloca %struct.ScmObj*, align 8
%current_45args57283 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57282)
store volatile %struct.ScmObj* %current_45args57283, %struct.ScmObj** %stackaddr$prim58810, align 8
%stackaddr$prim58811 = alloca %struct.ScmObj*, align 8
%x48089 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57283)
store volatile %struct.ScmObj* %x48089, %struct.ScmObj** %stackaddr$prim58811, align 8
%stackaddr$prim58812 = alloca %struct.ScmObj*, align 8
%current_45args57284 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57283)
store volatile %struct.ScmObj* %current_45args57284, %struct.ScmObj** %stackaddr$prim58812, align 8
%stackaddr$prim58813 = alloca %struct.ScmObj*, align 8
%y48088 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57284)
store volatile %struct.ScmObj* %y48088, %struct.ScmObj** %stackaddr$prim58813, align 8
%ae49114 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57286$k484570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58814 = alloca %struct.ScmObj*, align 8
%argslist57286$k484571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x48089, %struct.ScmObj* %argslist57286$k484570)
store volatile %struct.ScmObj* %argslist57286$k484571, %struct.ScmObj** %stackaddr$prim58814, align 8
%stackaddr$prim58815 = alloca %struct.ScmObj*, align 8
%argslist57286$k484572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49114, %struct.ScmObj* %argslist57286$k484571)
store volatile %struct.ScmObj* %argslist57286$k484572, %struct.ScmObj** %stackaddr$prim58815, align 8
%clofunc58816 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48457)
musttail call tailcc void %clofunc58816(%struct.ScmObj* %k48457, %struct.ScmObj* %argslist57286$k484572)
ret void
}

define tailcc void @proc_clo$ae49030(%struct.ScmObj* %env$ae49030,%struct.ScmObj* %current_45args57290) {
%stackaddr$prim58817 = alloca %struct.ScmObj*, align 8
%k48458 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57290)
store volatile %struct.ScmObj* %k48458, %struct.ScmObj** %stackaddr$prim58817, align 8
%stackaddr$prim58818 = alloca %struct.ScmObj*, align 8
%current_45args57291 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57290)
store volatile %struct.ScmObj* %current_45args57291, %struct.ScmObj** %stackaddr$prim58818, align 8
%stackaddr$prim58819 = alloca %struct.ScmObj*, align 8
%_37foldl148049 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57291)
store volatile %struct.ScmObj* %_37foldl148049, %struct.ScmObj** %stackaddr$prim58819, align 8
%ae49032 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58820 = alloca %struct.ScmObj*, align 8
%fptrToInt58821 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49033 to i64
%ae49033 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58821)
store volatile %struct.ScmObj* %ae49033, %struct.ScmObj** %stackaddr$makeclosure58820, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49033, %struct.ScmObj* %_37foldl148049, i64 0)
%argslist57304$k484580 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58822 = alloca %struct.ScmObj*, align 8
%argslist57304$k484581 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49033, %struct.ScmObj* %argslist57304$k484580)
store volatile %struct.ScmObj* %argslist57304$k484581, %struct.ScmObj** %stackaddr$prim58822, align 8
%stackaddr$prim58823 = alloca %struct.ScmObj*, align 8
%argslist57304$k484582 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49032, %struct.ScmObj* %argslist57304$k484581)
store volatile %struct.ScmObj* %argslist57304$k484582, %struct.ScmObj** %stackaddr$prim58823, align 8
%clofunc58824 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48458)
musttail call tailcc void %clofunc58824(%struct.ScmObj* %k48458, %struct.ScmObj* %argslist57304$k484582)
ret void
}

define tailcc void @proc_clo$ae49033(%struct.ScmObj* %env$ae49033,%struct.ScmObj* %current_45args57293) {
%stackaddr$env-ref58825 = alloca %struct.ScmObj*, align 8
%_37foldl148049 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49033, i64 0)
store %struct.ScmObj* %_37foldl148049, %struct.ScmObj** %stackaddr$env-ref58825
%stackaddr$prim58826 = alloca %struct.ScmObj*, align 8
%k48459 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57293)
store volatile %struct.ScmObj* %k48459, %struct.ScmObj** %stackaddr$prim58826, align 8
%stackaddr$prim58827 = alloca %struct.ScmObj*, align 8
%current_45args57294 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57293)
store volatile %struct.ScmObj* %current_45args57294, %struct.ScmObj** %stackaddr$prim58827, align 8
%stackaddr$prim58828 = alloca %struct.ScmObj*, align 8
%f48052 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57294)
store volatile %struct.ScmObj* %f48052, %struct.ScmObj** %stackaddr$prim58828, align 8
%stackaddr$prim58829 = alloca %struct.ScmObj*, align 8
%current_45args57295 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57294)
store volatile %struct.ScmObj* %current_45args57295, %struct.ScmObj** %stackaddr$prim58829, align 8
%stackaddr$prim58830 = alloca %struct.ScmObj*, align 8
%acc48051 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57295)
store volatile %struct.ScmObj* %acc48051, %struct.ScmObj** %stackaddr$prim58830, align 8
%stackaddr$prim58831 = alloca %struct.ScmObj*, align 8
%current_45args57296 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57295)
store volatile %struct.ScmObj* %current_45args57296, %struct.ScmObj** %stackaddr$prim58831, align 8
%stackaddr$prim58832 = alloca %struct.ScmObj*, align 8
%lst48050 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57296)
store volatile %struct.ScmObj* %lst48050, %struct.ScmObj** %stackaddr$prim58832, align 8
%stackaddr$prim58833 = alloca %struct.ScmObj*, align 8
%anf_45bind48224 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48050)
store volatile %struct.ScmObj* %anf_45bind48224, %struct.ScmObj** %stackaddr$prim58833, align 8
%truthy$cmp58834 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48224)
%cmp$cmp58834 = icmp eq i64 %truthy$cmp58834, 1
br i1 %cmp$cmp58834, label %truebranch$cmp58834, label %falsebranch$cmp58834
truebranch$cmp58834:
%ae49037 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57298$k484590 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58835 = alloca %struct.ScmObj*, align 8
%argslist57298$k484591 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48051, %struct.ScmObj* %argslist57298$k484590)
store volatile %struct.ScmObj* %argslist57298$k484591, %struct.ScmObj** %stackaddr$prim58835, align 8
%stackaddr$prim58836 = alloca %struct.ScmObj*, align 8
%argslist57298$k484592 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49037, %struct.ScmObj* %argslist57298$k484591)
store volatile %struct.ScmObj* %argslist57298$k484592, %struct.ScmObj** %stackaddr$prim58836, align 8
%clofunc58837 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48459)
musttail call tailcc void %clofunc58837(%struct.ScmObj* %k48459, %struct.ScmObj* %argslist57298$k484592)
ret void
falsebranch$cmp58834:
%stackaddr$prim58838 = alloca %struct.ScmObj*, align 8
%anf_45bind48225 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48050)
store volatile %struct.ScmObj* %anf_45bind48225, %struct.ScmObj** %stackaddr$prim58838, align 8
%stackaddr$makeclosure58839 = alloca %struct.ScmObj*, align 8
%fptrToInt58840 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49044 to i64
%ae49044 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt58840)
store volatile %struct.ScmObj* %ae49044, %struct.ScmObj** %stackaddr$makeclosure58839, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49044, %struct.ScmObj* %f48052, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49044, %struct.ScmObj* %lst48050, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49044, %struct.ScmObj* %_37foldl148049, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49044, %struct.ScmObj* %k48459, i64 3)
%argslist57303$f480520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58841 = alloca %struct.ScmObj*, align 8
%argslist57303$f480521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48051, %struct.ScmObj* %argslist57303$f480520)
store volatile %struct.ScmObj* %argslist57303$f480521, %struct.ScmObj** %stackaddr$prim58841, align 8
%stackaddr$prim58842 = alloca %struct.ScmObj*, align 8
%argslist57303$f480522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48225, %struct.ScmObj* %argslist57303$f480521)
store volatile %struct.ScmObj* %argslist57303$f480522, %struct.ScmObj** %stackaddr$prim58842, align 8
%stackaddr$prim58843 = alloca %struct.ScmObj*, align 8
%argslist57303$f480523 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49044, %struct.ScmObj* %argslist57303$f480522)
store volatile %struct.ScmObj* %argslist57303$f480523, %struct.ScmObj** %stackaddr$prim58843, align 8
%clofunc58844 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48052)
musttail call tailcc void %clofunc58844(%struct.ScmObj* %f48052, %struct.ScmObj* %argslist57303$f480523)
ret void
}

define tailcc void @proc_clo$ae49044(%struct.ScmObj* %env$ae49044,%struct.ScmObj* %current_45args57299) {
%stackaddr$env-ref58845 = alloca %struct.ScmObj*, align 8
%f48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49044, i64 0)
store %struct.ScmObj* %f48052, %struct.ScmObj** %stackaddr$env-ref58845
%stackaddr$env-ref58846 = alloca %struct.ScmObj*, align 8
%lst48050 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49044, i64 1)
store %struct.ScmObj* %lst48050, %struct.ScmObj** %stackaddr$env-ref58846
%stackaddr$env-ref58847 = alloca %struct.ScmObj*, align 8
%_37foldl148049 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49044, i64 2)
store %struct.ScmObj* %_37foldl148049, %struct.ScmObj** %stackaddr$env-ref58847
%stackaddr$env-ref58848 = alloca %struct.ScmObj*, align 8
%k48459 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49044, i64 3)
store %struct.ScmObj* %k48459, %struct.ScmObj** %stackaddr$env-ref58848
%stackaddr$prim58849 = alloca %struct.ScmObj*, align 8
%_95k48460 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57299)
store volatile %struct.ScmObj* %_95k48460, %struct.ScmObj** %stackaddr$prim58849, align 8
%stackaddr$prim58850 = alloca %struct.ScmObj*, align 8
%current_45args57300 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57299)
store volatile %struct.ScmObj* %current_45args57300, %struct.ScmObj** %stackaddr$prim58850, align 8
%stackaddr$prim58851 = alloca %struct.ScmObj*, align 8
%anf_45bind48226 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57300)
store volatile %struct.ScmObj* %anf_45bind48226, %struct.ScmObj** %stackaddr$prim58851, align 8
%stackaddr$prim58852 = alloca %struct.ScmObj*, align 8
%anf_45bind48227 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48050)
store volatile %struct.ScmObj* %anf_45bind48227, %struct.ScmObj** %stackaddr$prim58852, align 8
%argslist57302$_37foldl1480490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58853 = alloca %struct.ScmObj*, align 8
%argslist57302$_37foldl1480491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48227, %struct.ScmObj* %argslist57302$_37foldl1480490)
store volatile %struct.ScmObj* %argslist57302$_37foldl1480491, %struct.ScmObj** %stackaddr$prim58853, align 8
%stackaddr$prim58854 = alloca %struct.ScmObj*, align 8
%argslist57302$_37foldl1480492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48226, %struct.ScmObj* %argslist57302$_37foldl1480491)
store volatile %struct.ScmObj* %argslist57302$_37foldl1480492, %struct.ScmObj** %stackaddr$prim58854, align 8
%stackaddr$prim58855 = alloca %struct.ScmObj*, align 8
%argslist57302$_37foldl1480493 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48052, %struct.ScmObj* %argslist57302$_37foldl1480492)
store volatile %struct.ScmObj* %argslist57302$_37foldl1480493, %struct.ScmObj** %stackaddr$prim58855, align 8
%stackaddr$prim58856 = alloca %struct.ScmObj*, align 8
%argslist57302$_37foldl1480494 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48459, %struct.ScmObj* %argslist57302$_37foldl1480493)
store volatile %struct.ScmObj* %argslist57302$_37foldl1480494, %struct.ScmObj** %stackaddr$prim58856, align 8
%clofunc58857 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148049)
musttail call tailcc void %clofunc58857(%struct.ScmObj* %_37foldl148049, %struct.ScmObj* %argslist57302$_37foldl1480494)
ret void
}

define tailcc void @proc_clo$ae48947(%struct.ScmObj* %env$ae48947,%struct.ScmObj* %current_45args57307) {
%stackaddr$prim58858 = alloca %struct.ScmObj*, align 8
%k48461 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57307)
store volatile %struct.ScmObj* %k48461, %struct.ScmObj** %stackaddr$prim58858, align 8
%stackaddr$prim58859 = alloca %struct.ScmObj*, align 8
%current_45args57308 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57307)
store volatile %struct.ScmObj* %current_45args57308, %struct.ScmObj** %stackaddr$prim58859, align 8
%stackaddr$prim58860 = alloca %struct.ScmObj*, align 8
%_37length48054 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57308)
store volatile %struct.ScmObj* %_37length48054, %struct.ScmObj** %stackaddr$prim58860, align 8
%ae48949 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58861 = alloca %struct.ScmObj*, align 8
%fptrToInt58862 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48950 to i64
%ae48950 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58862)
store volatile %struct.ScmObj* %ae48950, %struct.ScmObj** %stackaddr$makeclosure58861, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48950, %struct.ScmObj* %_37length48054, i64 0)
%argslist57319$k484610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58863 = alloca %struct.ScmObj*, align 8
%argslist57319$k484611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48950, %struct.ScmObj* %argslist57319$k484610)
store volatile %struct.ScmObj* %argslist57319$k484611, %struct.ScmObj** %stackaddr$prim58863, align 8
%stackaddr$prim58864 = alloca %struct.ScmObj*, align 8
%argslist57319$k484612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48949, %struct.ScmObj* %argslist57319$k484611)
store volatile %struct.ScmObj* %argslist57319$k484612, %struct.ScmObj** %stackaddr$prim58864, align 8
%clofunc58865 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48461)
musttail call tailcc void %clofunc58865(%struct.ScmObj* %k48461, %struct.ScmObj* %argslist57319$k484612)
ret void
}

define tailcc void @proc_clo$ae48950(%struct.ScmObj* %env$ae48950,%struct.ScmObj* %current_45args57310) {
%stackaddr$env-ref58866 = alloca %struct.ScmObj*, align 8
%_37length48054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48950, i64 0)
store %struct.ScmObj* %_37length48054, %struct.ScmObj** %stackaddr$env-ref58866
%stackaddr$prim58867 = alloca %struct.ScmObj*, align 8
%k48462 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57310)
store volatile %struct.ScmObj* %k48462, %struct.ScmObj** %stackaddr$prim58867, align 8
%stackaddr$prim58868 = alloca %struct.ScmObj*, align 8
%current_45args57311 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57310)
store volatile %struct.ScmObj* %current_45args57311, %struct.ScmObj** %stackaddr$prim58868, align 8
%stackaddr$prim58869 = alloca %struct.ScmObj*, align 8
%lst48055 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57311)
store volatile %struct.ScmObj* %lst48055, %struct.ScmObj** %stackaddr$prim58869, align 8
%stackaddr$prim58870 = alloca %struct.ScmObj*, align 8
%anf_45bind48220 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48055)
store volatile %struct.ScmObj* %anf_45bind48220, %struct.ScmObj** %stackaddr$prim58870, align 8
%truthy$cmp58871 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48220)
%cmp$cmp58871 = icmp eq i64 %truthy$cmp58871, 1
br i1 %cmp$cmp58871, label %truebranch$cmp58871, label %falsebranch$cmp58871
truebranch$cmp58871:
%ae48954 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48955 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57313$k484620 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58872 = alloca %struct.ScmObj*, align 8
%argslist57313$k484621 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48955, %struct.ScmObj* %argslist57313$k484620)
store volatile %struct.ScmObj* %argslist57313$k484621, %struct.ScmObj** %stackaddr$prim58872, align 8
%stackaddr$prim58873 = alloca %struct.ScmObj*, align 8
%argslist57313$k484622 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48954, %struct.ScmObj* %argslist57313$k484621)
store volatile %struct.ScmObj* %argslist57313$k484622, %struct.ScmObj** %stackaddr$prim58873, align 8
%clofunc58874 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48462)
musttail call tailcc void %clofunc58874(%struct.ScmObj* %k48462, %struct.ScmObj* %argslist57313$k484622)
ret void
falsebranch$cmp58871:
%stackaddr$prim58875 = alloca %struct.ScmObj*, align 8
%anf_45bind48221 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48055)
store volatile %struct.ScmObj* %anf_45bind48221, %struct.ScmObj** %stackaddr$prim58875, align 8
%stackaddr$makeclosure58876 = alloca %struct.ScmObj*, align 8
%fptrToInt58877 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48964 to i64
%ae48964 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58877)
store volatile %struct.ScmObj* %ae48964, %struct.ScmObj** %stackaddr$makeclosure58876, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48964, %struct.ScmObj* %k48462, i64 0)
%argslist57318$_37length480540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58878 = alloca %struct.ScmObj*, align 8
%argslist57318$_37length480541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48221, %struct.ScmObj* %argslist57318$_37length480540)
store volatile %struct.ScmObj* %argslist57318$_37length480541, %struct.ScmObj** %stackaddr$prim58878, align 8
%stackaddr$prim58879 = alloca %struct.ScmObj*, align 8
%argslist57318$_37length480542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48964, %struct.ScmObj* %argslist57318$_37length480541)
store volatile %struct.ScmObj* %argslist57318$_37length480542, %struct.ScmObj** %stackaddr$prim58879, align 8
%clofunc58880 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length48054)
musttail call tailcc void %clofunc58880(%struct.ScmObj* %_37length48054, %struct.ScmObj* %argslist57318$_37length480542)
ret void
}

define tailcc void @proc_clo$ae48964(%struct.ScmObj* %env$ae48964,%struct.ScmObj* %current_45args57314) {
%stackaddr$env-ref58881 = alloca %struct.ScmObj*, align 8
%k48462 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48964, i64 0)
store %struct.ScmObj* %k48462, %struct.ScmObj** %stackaddr$env-ref58881
%stackaddr$prim58882 = alloca %struct.ScmObj*, align 8
%_95k48463 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57314)
store volatile %struct.ScmObj* %_95k48463, %struct.ScmObj** %stackaddr$prim58882, align 8
%stackaddr$prim58883 = alloca %struct.ScmObj*, align 8
%current_45args57315 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57314)
store volatile %struct.ScmObj* %current_45args57315, %struct.ScmObj** %stackaddr$prim58883, align 8
%stackaddr$prim58884 = alloca %struct.ScmObj*, align 8
%anf_45bind48222 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57315)
store volatile %struct.ScmObj* %anf_45bind48222, %struct.ScmObj** %stackaddr$prim58884, align 8
%ae48966 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim58885 = alloca %struct.ScmObj*, align 8
%cpsprim48464 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae48966, %struct.ScmObj* %anf_45bind48222)
store volatile %struct.ScmObj* %cpsprim48464, %struct.ScmObj** %stackaddr$prim58885, align 8
%ae48969 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57317$k484620 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58886 = alloca %struct.ScmObj*, align 8
%argslist57317$k484621 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48464, %struct.ScmObj* %argslist57317$k484620)
store volatile %struct.ScmObj* %argslist57317$k484621, %struct.ScmObj** %stackaddr$prim58886, align 8
%stackaddr$prim58887 = alloca %struct.ScmObj*, align 8
%argslist57317$k484622 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48969, %struct.ScmObj* %argslist57317$k484621)
store volatile %struct.ScmObj* %argslist57317$k484622, %struct.ScmObj** %stackaddr$prim58887, align 8
%clofunc58888 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48462)
musttail call tailcc void %clofunc58888(%struct.ScmObj* %k48462, %struct.ScmObj* %argslist57317$k484622)
ret void
}

define tailcc void @proc_clo$ae48797(%struct.ScmObj* %env$ae48797,%struct.ScmObj* %current_45args57322) {
%stackaddr$prim58889 = alloca %struct.ScmObj*, align 8
%k48465 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57322)
store volatile %struct.ScmObj* %k48465, %struct.ScmObj** %stackaddr$prim58889, align 8
%stackaddr$prim58890 = alloca %struct.ScmObj*, align 8
%current_45args57323 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57322)
store volatile %struct.ScmObj* %current_45args57323, %struct.ScmObj** %stackaddr$prim58890, align 8
%stackaddr$prim58891 = alloca %struct.ScmObj*, align 8
%_37take48057 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57323)
store volatile %struct.ScmObj* %_37take48057, %struct.ScmObj** %stackaddr$prim58891, align 8
%ae48799 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58892 = alloca %struct.ScmObj*, align 8
%fptrToInt58893 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48800 to i64
%ae48800 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58893)
store volatile %struct.ScmObj* %ae48800, %struct.ScmObj** %stackaddr$makeclosure58892, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48800, %struct.ScmObj* %_37take48057, i64 0)
%argslist57336$k484650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58894 = alloca %struct.ScmObj*, align 8
%argslist57336$k484651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48800, %struct.ScmObj* %argslist57336$k484650)
store volatile %struct.ScmObj* %argslist57336$k484651, %struct.ScmObj** %stackaddr$prim58894, align 8
%stackaddr$prim58895 = alloca %struct.ScmObj*, align 8
%argslist57336$k484652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48799, %struct.ScmObj* %argslist57336$k484651)
store volatile %struct.ScmObj* %argslist57336$k484652, %struct.ScmObj** %stackaddr$prim58895, align 8
%clofunc58896 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48465)
musttail call tailcc void %clofunc58896(%struct.ScmObj* %k48465, %struct.ScmObj* %argslist57336$k484652)
ret void
}

define tailcc void @proc_clo$ae48800(%struct.ScmObj* %env$ae48800,%struct.ScmObj* %current_45args57325) {
%stackaddr$env-ref58897 = alloca %struct.ScmObj*, align 8
%_37take48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48800, i64 0)
store %struct.ScmObj* %_37take48057, %struct.ScmObj** %stackaddr$env-ref58897
%stackaddr$prim58898 = alloca %struct.ScmObj*, align 8
%k48466 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57325)
store volatile %struct.ScmObj* %k48466, %struct.ScmObj** %stackaddr$prim58898, align 8
%stackaddr$prim58899 = alloca %struct.ScmObj*, align 8
%current_45args57326 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57325)
store volatile %struct.ScmObj* %current_45args57326, %struct.ScmObj** %stackaddr$prim58899, align 8
%stackaddr$prim58900 = alloca %struct.ScmObj*, align 8
%lst48059 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57326)
store volatile %struct.ScmObj* %lst48059, %struct.ScmObj** %stackaddr$prim58900, align 8
%stackaddr$prim58901 = alloca %struct.ScmObj*, align 8
%current_45args57327 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57326)
store volatile %struct.ScmObj* %current_45args57327, %struct.ScmObj** %stackaddr$prim58901, align 8
%stackaddr$prim58902 = alloca %struct.ScmObj*, align 8
%n48058 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57327)
store volatile %struct.ScmObj* %n48058, %struct.ScmObj** %stackaddr$prim58902, align 8
%ae48802 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58903 = alloca %struct.ScmObj*, align 8
%anf_45bind48213 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n48058, %struct.ScmObj* %ae48802)
store volatile %struct.ScmObj* %anf_45bind48213, %struct.ScmObj** %stackaddr$prim58903, align 8
%truthy$cmp58904 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48213)
%cmp$cmp58904 = icmp eq i64 %truthy$cmp58904, 1
br i1 %cmp$cmp58904, label %truebranch$cmp58904, label %falsebranch$cmp58904
truebranch$cmp58904:
%ae48805 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48806 = call %struct.ScmObj* @const_init_null()
%argslist57329$k484660 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58905 = alloca %struct.ScmObj*, align 8
%argslist57329$k484661 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48806, %struct.ScmObj* %argslist57329$k484660)
store volatile %struct.ScmObj* %argslist57329$k484661, %struct.ScmObj** %stackaddr$prim58905, align 8
%stackaddr$prim58906 = alloca %struct.ScmObj*, align 8
%argslist57329$k484662 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48805, %struct.ScmObj* %argslist57329$k484661)
store volatile %struct.ScmObj* %argslist57329$k484662, %struct.ScmObj** %stackaddr$prim58906, align 8
%clofunc58907 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48466)
musttail call tailcc void %clofunc58907(%struct.ScmObj* %k48466, %struct.ScmObj* %argslist57329$k484662)
ret void
falsebranch$cmp58904:
%stackaddr$prim58908 = alloca %struct.ScmObj*, align 8
%anf_45bind48214 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48059)
store volatile %struct.ScmObj* %anf_45bind48214, %struct.ScmObj** %stackaddr$prim58908, align 8
%truthy$cmp58909 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48214)
%cmp$cmp58909 = icmp eq i64 %truthy$cmp58909, 1
br i1 %cmp$cmp58909, label %truebranch$cmp58909, label %falsebranch$cmp58909
truebranch$cmp58909:
%ae48816 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48817 = call %struct.ScmObj* @const_init_null()
%argslist57330$k484660 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58910 = alloca %struct.ScmObj*, align 8
%argslist57330$k484661 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48817, %struct.ScmObj* %argslist57330$k484660)
store volatile %struct.ScmObj* %argslist57330$k484661, %struct.ScmObj** %stackaddr$prim58910, align 8
%stackaddr$prim58911 = alloca %struct.ScmObj*, align 8
%argslist57330$k484662 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48816, %struct.ScmObj* %argslist57330$k484661)
store volatile %struct.ScmObj* %argslist57330$k484662, %struct.ScmObj** %stackaddr$prim58911, align 8
%clofunc58912 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48466)
musttail call tailcc void %clofunc58912(%struct.ScmObj* %k48466, %struct.ScmObj* %argslist57330$k484662)
ret void
falsebranch$cmp58909:
%stackaddr$prim58913 = alloca %struct.ScmObj*, align 8
%anf_45bind48215 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48059)
store volatile %struct.ScmObj* %anf_45bind48215, %struct.ScmObj** %stackaddr$prim58913, align 8
%stackaddr$prim58914 = alloca %struct.ScmObj*, align 8
%anf_45bind48216 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48059)
store volatile %struct.ScmObj* %anf_45bind48216, %struct.ScmObj** %stackaddr$prim58914, align 8
%ae48827 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim58915 = alloca %struct.ScmObj*, align 8
%anf_45bind48217 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n48058, %struct.ScmObj* %ae48827)
store volatile %struct.ScmObj* %anf_45bind48217, %struct.ScmObj** %stackaddr$prim58915, align 8
%stackaddr$makeclosure58916 = alloca %struct.ScmObj*, align 8
%fptrToInt58917 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48829 to i64
%ae48829 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58917)
store volatile %struct.ScmObj* %ae48829, %struct.ScmObj** %stackaddr$makeclosure58916, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48829, %struct.ScmObj* %anf_45bind48215, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48829, %struct.ScmObj* %k48466, i64 1)
%argslist57335$_37take480570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58918 = alloca %struct.ScmObj*, align 8
%argslist57335$_37take480571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48217, %struct.ScmObj* %argslist57335$_37take480570)
store volatile %struct.ScmObj* %argslist57335$_37take480571, %struct.ScmObj** %stackaddr$prim58918, align 8
%stackaddr$prim58919 = alloca %struct.ScmObj*, align 8
%argslist57335$_37take480572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48216, %struct.ScmObj* %argslist57335$_37take480571)
store volatile %struct.ScmObj* %argslist57335$_37take480572, %struct.ScmObj** %stackaddr$prim58919, align 8
%stackaddr$prim58920 = alloca %struct.ScmObj*, align 8
%argslist57335$_37take480573 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48829, %struct.ScmObj* %argslist57335$_37take480572)
store volatile %struct.ScmObj* %argslist57335$_37take480573, %struct.ScmObj** %stackaddr$prim58920, align 8
%clofunc58921 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take48057)
musttail call tailcc void %clofunc58921(%struct.ScmObj* %_37take48057, %struct.ScmObj* %argslist57335$_37take480573)
ret void
}

define tailcc void @proc_clo$ae48829(%struct.ScmObj* %env$ae48829,%struct.ScmObj* %current_45args57331) {
%stackaddr$env-ref58922 = alloca %struct.ScmObj*, align 8
%anf_45bind48215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48829, i64 0)
store %struct.ScmObj* %anf_45bind48215, %struct.ScmObj** %stackaddr$env-ref58922
%stackaddr$env-ref58923 = alloca %struct.ScmObj*, align 8
%k48466 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48829, i64 1)
store %struct.ScmObj* %k48466, %struct.ScmObj** %stackaddr$env-ref58923
%stackaddr$prim58924 = alloca %struct.ScmObj*, align 8
%_95k48467 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57331)
store volatile %struct.ScmObj* %_95k48467, %struct.ScmObj** %stackaddr$prim58924, align 8
%stackaddr$prim58925 = alloca %struct.ScmObj*, align 8
%current_45args57332 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57331)
store volatile %struct.ScmObj* %current_45args57332, %struct.ScmObj** %stackaddr$prim58925, align 8
%stackaddr$prim58926 = alloca %struct.ScmObj*, align 8
%anf_45bind48218 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57332)
store volatile %struct.ScmObj* %anf_45bind48218, %struct.ScmObj** %stackaddr$prim58926, align 8
%stackaddr$prim58927 = alloca %struct.ScmObj*, align 8
%cpsprim48468 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48215, %struct.ScmObj* %anf_45bind48218)
store volatile %struct.ScmObj* %cpsprim48468, %struct.ScmObj** %stackaddr$prim58927, align 8
%ae48835 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57334$k484660 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58928 = alloca %struct.ScmObj*, align 8
%argslist57334$k484661 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48468, %struct.ScmObj* %argslist57334$k484660)
store volatile %struct.ScmObj* %argslist57334$k484661, %struct.ScmObj** %stackaddr$prim58928, align 8
%stackaddr$prim58929 = alloca %struct.ScmObj*, align 8
%argslist57334$k484662 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48835, %struct.ScmObj* %argslist57334$k484661)
store volatile %struct.ScmObj* %argslist57334$k484662, %struct.ScmObj** %stackaddr$prim58929, align 8
%clofunc58930 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48466)
musttail call tailcc void %clofunc58930(%struct.ScmObj* %k48466, %struct.ScmObj* %argslist57334$k484662)
ret void
}

define tailcc void @proc_clo$ae48700(%struct.ScmObj* %env$ae48700,%struct.ScmObj* %current_45args57339) {
%stackaddr$prim58931 = alloca %struct.ScmObj*, align 8
%k48469 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57339)
store volatile %struct.ScmObj* %k48469, %struct.ScmObj** %stackaddr$prim58931, align 8
%stackaddr$prim58932 = alloca %struct.ScmObj*, align 8
%current_45args57340 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57339)
store volatile %struct.ScmObj* %current_45args57340, %struct.ScmObj** %stackaddr$prim58932, align 8
%stackaddr$prim58933 = alloca %struct.ScmObj*, align 8
%_37map48061 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57340)
store volatile %struct.ScmObj* %_37map48061, %struct.ScmObj** %stackaddr$prim58933, align 8
%ae48702 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58934 = alloca %struct.ScmObj*, align 8
%fptrToInt58935 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48703 to i64
%ae48703 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58935)
store volatile %struct.ScmObj* %ae48703, %struct.ScmObj** %stackaddr$makeclosure58934, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48703, %struct.ScmObj* %_37map48061, i64 0)
%argslist57356$k484690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58936 = alloca %struct.ScmObj*, align 8
%argslist57356$k484691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48703, %struct.ScmObj* %argslist57356$k484690)
store volatile %struct.ScmObj* %argslist57356$k484691, %struct.ScmObj** %stackaddr$prim58936, align 8
%stackaddr$prim58937 = alloca %struct.ScmObj*, align 8
%argslist57356$k484692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48702, %struct.ScmObj* %argslist57356$k484691)
store volatile %struct.ScmObj* %argslist57356$k484692, %struct.ScmObj** %stackaddr$prim58937, align 8
%clofunc58938 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48469)
musttail call tailcc void %clofunc58938(%struct.ScmObj* %k48469, %struct.ScmObj* %argslist57356$k484692)
ret void
}

define tailcc void @proc_clo$ae48703(%struct.ScmObj* %env$ae48703,%struct.ScmObj* %current_45args57342) {
%stackaddr$env-ref58939 = alloca %struct.ScmObj*, align 8
%_37map48061 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48703, i64 0)
store %struct.ScmObj* %_37map48061, %struct.ScmObj** %stackaddr$env-ref58939
%stackaddr$prim58940 = alloca %struct.ScmObj*, align 8
%k48470 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57342)
store volatile %struct.ScmObj* %k48470, %struct.ScmObj** %stackaddr$prim58940, align 8
%stackaddr$prim58941 = alloca %struct.ScmObj*, align 8
%current_45args57343 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57342)
store volatile %struct.ScmObj* %current_45args57343, %struct.ScmObj** %stackaddr$prim58941, align 8
%stackaddr$prim58942 = alloca %struct.ScmObj*, align 8
%f48063 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57343)
store volatile %struct.ScmObj* %f48063, %struct.ScmObj** %stackaddr$prim58942, align 8
%stackaddr$prim58943 = alloca %struct.ScmObj*, align 8
%current_45args57344 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57343)
store volatile %struct.ScmObj* %current_45args57344, %struct.ScmObj** %stackaddr$prim58943, align 8
%stackaddr$prim58944 = alloca %struct.ScmObj*, align 8
%lst48062 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57344)
store volatile %struct.ScmObj* %lst48062, %struct.ScmObj** %stackaddr$prim58944, align 8
%stackaddr$prim58945 = alloca %struct.ScmObj*, align 8
%anf_45bind48207 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48062)
store volatile %struct.ScmObj* %anf_45bind48207, %struct.ScmObj** %stackaddr$prim58945, align 8
%truthy$cmp58946 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48207)
%cmp$cmp58946 = icmp eq i64 %truthy$cmp58946, 1
br i1 %cmp$cmp58946, label %truebranch$cmp58946, label %falsebranch$cmp58946
truebranch$cmp58946:
%ae48707 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48708 = call %struct.ScmObj* @const_init_null()
%argslist57346$k484700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58947 = alloca %struct.ScmObj*, align 8
%argslist57346$k484701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48708, %struct.ScmObj* %argslist57346$k484700)
store volatile %struct.ScmObj* %argslist57346$k484701, %struct.ScmObj** %stackaddr$prim58947, align 8
%stackaddr$prim58948 = alloca %struct.ScmObj*, align 8
%argslist57346$k484702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48707, %struct.ScmObj* %argslist57346$k484701)
store volatile %struct.ScmObj* %argslist57346$k484702, %struct.ScmObj** %stackaddr$prim58948, align 8
%clofunc58949 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48470)
musttail call tailcc void %clofunc58949(%struct.ScmObj* %k48470, %struct.ScmObj* %argslist57346$k484702)
ret void
falsebranch$cmp58946:
%stackaddr$prim58950 = alloca %struct.ScmObj*, align 8
%anf_45bind48208 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48062)
store volatile %struct.ScmObj* %anf_45bind48208, %struct.ScmObj** %stackaddr$prim58950, align 8
%stackaddr$makeclosure58951 = alloca %struct.ScmObj*, align 8
%fptrToInt58952 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48717 to i64
%ae48717 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt58952)
store volatile %struct.ScmObj* %ae48717, %struct.ScmObj** %stackaddr$makeclosure58951, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48717, %struct.ScmObj* %k48470, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48717, %struct.ScmObj* %f48063, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48717, %struct.ScmObj* %lst48062, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48717, %struct.ScmObj* %_37map48061, i64 3)
%argslist57355$f480630 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58953 = alloca %struct.ScmObj*, align 8
%argslist57355$f480631 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48208, %struct.ScmObj* %argslist57355$f480630)
store volatile %struct.ScmObj* %argslist57355$f480631, %struct.ScmObj** %stackaddr$prim58953, align 8
%stackaddr$prim58954 = alloca %struct.ScmObj*, align 8
%argslist57355$f480632 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48717, %struct.ScmObj* %argslist57355$f480631)
store volatile %struct.ScmObj* %argslist57355$f480632, %struct.ScmObj** %stackaddr$prim58954, align 8
%clofunc58955 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48063)
musttail call tailcc void %clofunc58955(%struct.ScmObj* %f48063, %struct.ScmObj* %argslist57355$f480632)
ret void
}

define tailcc void @proc_clo$ae48717(%struct.ScmObj* %env$ae48717,%struct.ScmObj* %current_45args57347) {
%stackaddr$env-ref58956 = alloca %struct.ScmObj*, align 8
%k48470 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48717, i64 0)
store %struct.ScmObj* %k48470, %struct.ScmObj** %stackaddr$env-ref58956
%stackaddr$env-ref58957 = alloca %struct.ScmObj*, align 8
%f48063 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48717, i64 1)
store %struct.ScmObj* %f48063, %struct.ScmObj** %stackaddr$env-ref58957
%stackaddr$env-ref58958 = alloca %struct.ScmObj*, align 8
%lst48062 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48717, i64 2)
store %struct.ScmObj* %lst48062, %struct.ScmObj** %stackaddr$env-ref58958
%stackaddr$env-ref58959 = alloca %struct.ScmObj*, align 8
%_37map48061 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48717, i64 3)
store %struct.ScmObj* %_37map48061, %struct.ScmObj** %stackaddr$env-ref58959
%stackaddr$prim58960 = alloca %struct.ScmObj*, align 8
%_95k48471 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57347)
store volatile %struct.ScmObj* %_95k48471, %struct.ScmObj** %stackaddr$prim58960, align 8
%stackaddr$prim58961 = alloca %struct.ScmObj*, align 8
%current_45args57348 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57347)
store volatile %struct.ScmObj* %current_45args57348, %struct.ScmObj** %stackaddr$prim58961, align 8
%stackaddr$prim58962 = alloca %struct.ScmObj*, align 8
%anf_45bind48209 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57348)
store volatile %struct.ScmObj* %anf_45bind48209, %struct.ScmObj** %stackaddr$prim58962, align 8
%stackaddr$prim58963 = alloca %struct.ScmObj*, align 8
%anf_45bind48210 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48062)
store volatile %struct.ScmObj* %anf_45bind48210, %struct.ScmObj** %stackaddr$prim58963, align 8
%stackaddr$makeclosure58964 = alloca %struct.ScmObj*, align 8
%fptrToInt58965 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48721 to i64
%ae48721 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58965)
store volatile %struct.ScmObj* %ae48721, %struct.ScmObj** %stackaddr$makeclosure58964, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48721, %struct.ScmObj* %k48470, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48721, %struct.ScmObj* %anf_45bind48209, i64 1)
%argslist57354$_37map480610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58966 = alloca %struct.ScmObj*, align 8
%argslist57354$_37map480611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48210, %struct.ScmObj* %argslist57354$_37map480610)
store volatile %struct.ScmObj* %argslist57354$_37map480611, %struct.ScmObj** %stackaddr$prim58966, align 8
%stackaddr$prim58967 = alloca %struct.ScmObj*, align 8
%argslist57354$_37map480612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48063, %struct.ScmObj* %argslist57354$_37map480611)
store volatile %struct.ScmObj* %argslist57354$_37map480612, %struct.ScmObj** %stackaddr$prim58967, align 8
%stackaddr$prim58968 = alloca %struct.ScmObj*, align 8
%argslist57354$_37map480613 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48721, %struct.ScmObj* %argslist57354$_37map480612)
store volatile %struct.ScmObj* %argslist57354$_37map480613, %struct.ScmObj** %stackaddr$prim58968, align 8
%clofunc58969 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map48061)
musttail call tailcc void %clofunc58969(%struct.ScmObj* %_37map48061, %struct.ScmObj* %argslist57354$_37map480613)
ret void
}

define tailcc void @proc_clo$ae48721(%struct.ScmObj* %env$ae48721,%struct.ScmObj* %current_45args57350) {
%stackaddr$env-ref58970 = alloca %struct.ScmObj*, align 8
%k48470 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48721, i64 0)
store %struct.ScmObj* %k48470, %struct.ScmObj** %stackaddr$env-ref58970
%stackaddr$env-ref58971 = alloca %struct.ScmObj*, align 8
%anf_45bind48209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48721, i64 1)
store %struct.ScmObj* %anf_45bind48209, %struct.ScmObj** %stackaddr$env-ref58971
%stackaddr$prim58972 = alloca %struct.ScmObj*, align 8
%_95k48472 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57350)
store volatile %struct.ScmObj* %_95k48472, %struct.ScmObj** %stackaddr$prim58972, align 8
%stackaddr$prim58973 = alloca %struct.ScmObj*, align 8
%current_45args57351 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57350)
store volatile %struct.ScmObj* %current_45args57351, %struct.ScmObj** %stackaddr$prim58973, align 8
%stackaddr$prim58974 = alloca %struct.ScmObj*, align 8
%anf_45bind48211 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57351)
store volatile %struct.ScmObj* %anf_45bind48211, %struct.ScmObj** %stackaddr$prim58974, align 8
%stackaddr$prim58975 = alloca %struct.ScmObj*, align 8
%cpsprim48473 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48209, %struct.ScmObj* %anf_45bind48211)
store volatile %struct.ScmObj* %cpsprim48473, %struct.ScmObj** %stackaddr$prim58975, align 8
%ae48727 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57353$k484700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58976 = alloca %struct.ScmObj*, align 8
%argslist57353$k484701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48473, %struct.ScmObj* %argslist57353$k484700)
store volatile %struct.ScmObj* %argslist57353$k484701, %struct.ScmObj** %stackaddr$prim58976, align 8
%stackaddr$prim58977 = alloca %struct.ScmObj*, align 8
%argslist57353$k484702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48727, %struct.ScmObj* %argslist57353$k484701)
store volatile %struct.ScmObj* %argslist57353$k484702, %struct.ScmObj** %stackaddr$prim58977, align 8
%clofunc58978 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48470)
musttail call tailcc void %clofunc58978(%struct.ScmObj* %k48470, %struct.ScmObj* %argslist57353$k484702)
ret void
}

define tailcc void @proc_clo$ae48620(%struct.ScmObj* %env$ae48620,%struct.ScmObj* %current_45args57359) {
%stackaddr$prim58979 = alloca %struct.ScmObj*, align 8
%k48474 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57359)
store volatile %struct.ScmObj* %k48474, %struct.ScmObj** %stackaddr$prim58979, align 8
%stackaddr$prim58980 = alloca %struct.ScmObj*, align 8
%current_45args57360 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57359)
store volatile %struct.ScmObj* %current_45args57360, %struct.ScmObj** %stackaddr$prim58980, align 8
%stackaddr$prim58981 = alloca %struct.ScmObj*, align 8
%_37foldr148065 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57360)
store volatile %struct.ScmObj* %_37foldr148065, %struct.ScmObj** %stackaddr$prim58981, align 8
%ae48622 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58982 = alloca %struct.ScmObj*, align 8
%fptrToInt58983 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48623 to i64
%ae48623 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58983)
store volatile %struct.ScmObj* %ae48623, %struct.ScmObj** %stackaddr$makeclosure58982, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48623, %struct.ScmObj* %_37foldr148065, i64 0)
%argslist57373$k484740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58984 = alloca %struct.ScmObj*, align 8
%argslist57373$k484741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48623, %struct.ScmObj* %argslist57373$k484740)
store volatile %struct.ScmObj* %argslist57373$k484741, %struct.ScmObj** %stackaddr$prim58984, align 8
%stackaddr$prim58985 = alloca %struct.ScmObj*, align 8
%argslist57373$k484742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48622, %struct.ScmObj* %argslist57373$k484741)
store volatile %struct.ScmObj* %argslist57373$k484742, %struct.ScmObj** %stackaddr$prim58985, align 8
%clofunc58986 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48474)
musttail call tailcc void %clofunc58986(%struct.ScmObj* %k48474, %struct.ScmObj* %argslist57373$k484742)
ret void
}

define tailcc void @proc_clo$ae48623(%struct.ScmObj* %env$ae48623,%struct.ScmObj* %current_45args57362) {
%stackaddr$env-ref58987 = alloca %struct.ScmObj*, align 8
%_37foldr148065 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48623, i64 0)
store %struct.ScmObj* %_37foldr148065, %struct.ScmObj** %stackaddr$env-ref58987
%stackaddr$prim58988 = alloca %struct.ScmObj*, align 8
%k48475 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57362)
store volatile %struct.ScmObj* %k48475, %struct.ScmObj** %stackaddr$prim58988, align 8
%stackaddr$prim58989 = alloca %struct.ScmObj*, align 8
%current_45args57363 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57362)
store volatile %struct.ScmObj* %current_45args57363, %struct.ScmObj** %stackaddr$prim58989, align 8
%stackaddr$prim58990 = alloca %struct.ScmObj*, align 8
%f48068 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57363)
store volatile %struct.ScmObj* %f48068, %struct.ScmObj** %stackaddr$prim58990, align 8
%stackaddr$prim58991 = alloca %struct.ScmObj*, align 8
%current_45args57364 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57363)
store volatile %struct.ScmObj* %current_45args57364, %struct.ScmObj** %stackaddr$prim58991, align 8
%stackaddr$prim58992 = alloca %struct.ScmObj*, align 8
%acc48067 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57364)
store volatile %struct.ScmObj* %acc48067, %struct.ScmObj** %stackaddr$prim58992, align 8
%stackaddr$prim58993 = alloca %struct.ScmObj*, align 8
%current_45args57365 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57364)
store volatile %struct.ScmObj* %current_45args57365, %struct.ScmObj** %stackaddr$prim58993, align 8
%stackaddr$prim58994 = alloca %struct.ScmObj*, align 8
%lst48066 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57365)
store volatile %struct.ScmObj* %lst48066, %struct.ScmObj** %stackaddr$prim58994, align 8
%stackaddr$prim58995 = alloca %struct.ScmObj*, align 8
%anf_45bind48202 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48066)
store volatile %struct.ScmObj* %anf_45bind48202, %struct.ScmObj** %stackaddr$prim58995, align 8
%truthy$cmp58996 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48202)
%cmp$cmp58996 = icmp eq i64 %truthy$cmp58996, 1
br i1 %cmp$cmp58996, label %truebranch$cmp58996, label %falsebranch$cmp58996
truebranch$cmp58996:
%ae48627 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57367$k484750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58997 = alloca %struct.ScmObj*, align 8
%argslist57367$k484751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48067, %struct.ScmObj* %argslist57367$k484750)
store volatile %struct.ScmObj* %argslist57367$k484751, %struct.ScmObj** %stackaddr$prim58997, align 8
%stackaddr$prim58998 = alloca %struct.ScmObj*, align 8
%argslist57367$k484752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48627, %struct.ScmObj* %argslist57367$k484751)
store volatile %struct.ScmObj* %argslist57367$k484752, %struct.ScmObj** %stackaddr$prim58998, align 8
%clofunc58999 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48475)
musttail call tailcc void %clofunc58999(%struct.ScmObj* %k48475, %struct.ScmObj* %argslist57367$k484752)
ret void
falsebranch$cmp58996:
%stackaddr$prim59000 = alloca %struct.ScmObj*, align 8
%anf_45bind48203 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48066)
store volatile %struct.ScmObj* %anf_45bind48203, %struct.ScmObj** %stackaddr$prim59000, align 8
%stackaddr$prim59001 = alloca %struct.ScmObj*, align 8
%anf_45bind48204 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48066)
store volatile %struct.ScmObj* %anf_45bind48204, %struct.ScmObj** %stackaddr$prim59001, align 8
%stackaddr$makeclosure59002 = alloca %struct.ScmObj*, align 8
%fptrToInt59003 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48635 to i64
%ae48635 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt59003)
store volatile %struct.ScmObj* %ae48635, %struct.ScmObj** %stackaddr$makeclosure59002, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48635, %struct.ScmObj* %k48475, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48635, %struct.ScmObj* %anf_45bind48203, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48635, %struct.ScmObj* %f48068, i64 2)
%argslist57372$_37foldr1480650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59004 = alloca %struct.ScmObj*, align 8
%argslist57372$_37foldr1480651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48204, %struct.ScmObj* %argslist57372$_37foldr1480650)
store volatile %struct.ScmObj* %argslist57372$_37foldr1480651, %struct.ScmObj** %stackaddr$prim59004, align 8
%stackaddr$prim59005 = alloca %struct.ScmObj*, align 8
%argslist57372$_37foldr1480652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48067, %struct.ScmObj* %argslist57372$_37foldr1480651)
store volatile %struct.ScmObj* %argslist57372$_37foldr1480652, %struct.ScmObj** %stackaddr$prim59005, align 8
%stackaddr$prim59006 = alloca %struct.ScmObj*, align 8
%argslist57372$_37foldr1480653 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48068, %struct.ScmObj* %argslist57372$_37foldr1480652)
store volatile %struct.ScmObj* %argslist57372$_37foldr1480653, %struct.ScmObj** %stackaddr$prim59006, align 8
%stackaddr$prim59007 = alloca %struct.ScmObj*, align 8
%argslist57372$_37foldr1480654 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48635, %struct.ScmObj* %argslist57372$_37foldr1480653)
store volatile %struct.ScmObj* %argslist57372$_37foldr1480654, %struct.ScmObj** %stackaddr$prim59007, align 8
%clofunc59008 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148065)
musttail call tailcc void %clofunc59008(%struct.ScmObj* %_37foldr148065, %struct.ScmObj* %argslist57372$_37foldr1480654)
ret void
}

define tailcc void @proc_clo$ae48635(%struct.ScmObj* %env$ae48635,%struct.ScmObj* %current_45args57368) {
%stackaddr$env-ref59009 = alloca %struct.ScmObj*, align 8
%k48475 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48635, i64 0)
store %struct.ScmObj* %k48475, %struct.ScmObj** %stackaddr$env-ref59009
%stackaddr$env-ref59010 = alloca %struct.ScmObj*, align 8
%anf_45bind48203 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48635, i64 1)
store %struct.ScmObj* %anf_45bind48203, %struct.ScmObj** %stackaddr$env-ref59010
%stackaddr$env-ref59011 = alloca %struct.ScmObj*, align 8
%f48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48635, i64 2)
store %struct.ScmObj* %f48068, %struct.ScmObj** %stackaddr$env-ref59011
%stackaddr$prim59012 = alloca %struct.ScmObj*, align 8
%_95k48476 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57368)
store volatile %struct.ScmObj* %_95k48476, %struct.ScmObj** %stackaddr$prim59012, align 8
%stackaddr$prim59013 = alloca %struct.ScmObj*, align 8
%current_45args57369 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57368)
store volatile %struct.ScmObj* %current_45args57369, %struct.ScmObj** %stackaddr$prim59013, align 8
%stackaddr$prim59014 = alloca %struct.ScmObj*, align 8
%anf_45bind48205 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57369)
store volatile %struct.ScmObj* %anf_45bind48205, %struct.ScmObj** %stackaddr$prim59014, align 8
%argslist57371$f480680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59015 = alloca %struct.ScmObj*, align 8
%argslist57371$f480681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48205, %struct.ScmObj* %argslist57371$f480680)
store volatile %struct.ScmObj* %argslist57371$f480681, %struct.ScmObj** %stackaddr$prim59015, align 8
%stackaddr$prim59016 = alloca %struct.ScmObj*, align 8
%argslist57371$f480682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48203, %struct.ScmObj* %argslist57371$f480681)
store volatile %struct.ScmObj* %argslist57371$f480682, %struct.ScmObj** %stackaddr$prim59016, align 8
%stackaddr$prim59017 = alloca %struct.ScmObj*, align 8
%argslist57371$f480683 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48475, %struct.ScmObj* %argslist57371$f480682)
store volatile %struct.ScmObj* %argslist57371$f480683, %struct.ScmObj** %stackaddr$prim59017, align 8
%clofunc59018 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48068)
musttail call tailcc void %clofunc59018(%struct.ScmObj* %f48068, %struct.ScmObj* %argslist57371$f480683)
ret void
}

define tailcc void @proc_clo$ae48503(%struct.ScmObj* %env$ae48503,%struct.ScmObj* %current_45args57376) {
%stackaddr$prim59019 = alloca %struct.ScmObj*, align 8
%k48477 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57376)
store volatile %struct.ScmObj* %k48477, %struct.ScmObj** %stackaddr$prim59019, align 8
%stackaddr$prim59020 = alloca %struct.ScmObj*, align 8
%current_45args57377 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57376)
store volatile %struct.ScmObj* %current_45args57377, %struct.ScmObj** %stackaddr$prim59020, align 8
%stackaddr$prim59021 = alloca %struct.ScmObj*, align 8
%y48045 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57377)
store volatile %struct.ScmObj* %y48045, %struct.ScmObj** %stackaddr$prim59021, align 8
%ae48505 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59022 = alloca %struct.ScmObj*, align 8
%fptrToInt59023 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48506 to i64
%ae48506 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt59023)
store volatile %struct.ScmObj* %ae48506, %struct.ScmObj** %stackaddr$makeclosure59022, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48506, %struct.ScmObj* %y48045, i64 0)
%argslist57395$k484770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59024 = alloca %struct.ScmObj*, align 8
%argslist57395$k484771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48506, %struct.ScmObj* %argslist57395$k484770)
store volatile %struct.ScmObj* %argslist57395$k484771, %struct.ScmObj** %stackaddr$prim59024, align 8
%stackaddr$prim59025 = alloca %struct.ScmObj*, align 8
%argslist57395$k484772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48505, %struct.ScmObj* %argslist57395$k484771)
store volatile %struct.ScmObj* %argslist57395$k484772, %struct.ScmObj** %stackaddr$prim59025, align 8
%clofunc59026 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48477)
musttail call tailcc void %clofunc59026(%struct.ScmObj* %k48477, %struct.ScmObj* %argslist57395$k484772)
ret void
}

define tailcc void @proc_clo$ae48506(%struct.ScmObj* %env$ae48506,%struct.ScmObj* %current_45args57379) {
%stackaddr$env-ref59027 = alloca %struct.ScmObj*, align 8
%y48045 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48506, i64 0)
store %struct.ScmObj* %y48045, %struct.ScmObj** %stackaddr$env-ref59027
%stackaddr$prim59028 = alloca %struct.ScmObj*, align 8
%k48478 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57379)
store volatile %struct.ScmObj* %k48478, %struct.ScmObj** %stackaddr$prim59028, align 8
%stackaddr$prim59029 = alloca %struct.ScmObj*, align 8
%current_45args57380 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57379)
store volatile %struct.ScmObj* %current_45args57380, %struct.ScmObj** %stackaddr$prim59029, align 8
%stackaddr$prim59030 = alloca %struct.ScmObj*, align 8
%f48046 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57380)
store volatile %struct.ScmObj* %f48046, %struct.ScmObj** %stackaddr$prim59030, align 8
%stackaddr$makeclosure59031 = alloca %struct.ScmObj*, align 8
%fptrToInt59032 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48507 to i64
%ae48507 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt59032)
store volatile %struct.ScmObj* %ae48507, %struct.ScmObj** %stackaddr$makeclosure59031, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48507, %struct.ScmObj* %f48046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48507, %struct.ScmObj* %k48478, i64 1)
%ae48508 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59033 = alloca %struct.ScmObj*, align 8
%fptrToInt59034 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48509 to i64
%ae48509 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt59034)
store volatile %struct.ScmObj* %ae48509, %struct.ScmObj** %stackaddr$makeclosure59033, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48509, %struct.ScmObj* %f48046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48509, %struct.ScmObj* %y48045, i64 1)
%argslist57394$ae485070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59035 = alloca %struct.ScmObj*, align 8
%argslist57394$ae485071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48509, %struct.ScmObj* %argslist57394$ae485070)
store volatile %struct.ScmObj* %argslist57394$ae485071, %struct.ScmObj** %stackaddr$prim59035, align 8
%stackaddr$prim59036 = alloca %struct.ScmObj*, align 8
%argslist57394$ae485072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48508, %struct.ScmObj* %argslist57394$ae485071)
store volatile %struct.ScmObj* %argslist57394$ae485072, %struct.ScmObj** %stackaddr$prim59036, align 8
%clofunc59037 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48507)
musttail call tailcc void %clofunc59037(%struct.ScmObj* %ae48507, %struct.ScmObj* %argslist57394$ae485072)
ret void
}

define tailcc void @proc_clo$ae48507(%struct.ScmObj* %env$ae48507,%struct.ScmObj* %current_45args57382) {
%stackaddr$env-ref59038 = alloca %struct.ScmObj*, align 8
%f48046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48507, i64 0)
store %struct.ScmObj* %f48046, %struct.ScmObj** %stackaddr$env-ref59038
%stackaddr$env-ref59039 = alloca %struct.ScmObj*, align 8
%k48478 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48507, i64 1)
store %struct.ScmObj* %k48478, %struct.ScmObj** %stackaddr$env-ref59039
%stackaddr$prim59040 = alloca %struct.ScmObj*, align 8
%_95k48479 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57382)
store volatile %struct.ScmObj* %_95k48479, %struct.ScmObj** %stackaddr$prim59040, align 8
%stackaddr$prim59041 = alloca %struct.ScmObj*, align 8
%current_45args57383 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57382)
store volatile %struct.ScmObj* %current_45args57383, %struct.ScmObj** %stackaddr$prim59041, align 8
%stackaddr$prim59042 = alloca %struct.ScmObj*, align 8
%anf_45bind48200 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57383)
store volatile %struct.ScmObj* %anf_45bind48200, %struct.ScmObj** %stackaddr$prim59042, align 8
%argslist57385$f480460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59043 = alloca %struct.ScmObj*, align 8
%argslist57385$f480461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48200, %struct.ScmObj* %argslist57385$f480460)
store volatile %struct.ScmObj* %argslist57385$f480461, %struct.ScmObj** %stackaddr$prim59043, align 8
%stackaddr$prim59044 = alloca %struct.ScmObj*, align 8
%argslist57385$f480462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48478, %struct.ScmObj* %argslist57385$f480461)
store volatile %struct.ScmObj* %argslist57385$f480462, %struct.ScmObj** %stackaddr$prim59044, align 8
%clofunc59045 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48046)
musttail call tailcc void %clofunc59045(%struct.ScmObj* %f48046, %struct.ScmObj* %argslist57385$f480462)
ret void
}

define tailcc void @proc_clo$ae48509(%struct.ScmObj* %env$ae48509,%struct.ScmObj* %args4804748480) {
%stackaddr$env-ref59046 = alloca %struct.ScmObj*, align 8
%f48046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48509, i64 0)
store %struct.ScmObj* %f48046, %struct.ScmObj** %stackaddr$env-ref59046
%stackaddr$env-ref59047 = alloca %struct.ScmObj*, align 8
%y48045 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48509, i64 1)
store %struct.ScmObj* %y48045, %struct.ScmObj** %stackaddr$env-ref59047
%stackaddr$prim59048 = alloca %struct.ScmObj*, align 8
%k48481 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4804748480)
store volatile %struct.ScmObj* %k48481, %struct.ScmObj** %stackaddr$prim59048, align 8
%stackaddr$prim59049 = alloca %struct.ScmObj*, align 8
%args48047 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4804748480)
store volatile %struct.ScmObj* %args48047, %struct.ScmObj** %stackaddr$prim59049, align 8
%stackaddr$makeclosure59050 = alloca %struct.ScmObj*, align 8
%fptrToInt59051 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48513 to i64
%ae48513 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt59051)
store volatile %struct.ScmObj* %ae48513, %struct.ScmObj** %stackaddr$makeclosure59050, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48513, %struct.ScmObj* %k48481, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48513, %struct.ScmObj* %args48047, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48513, %struct.ScmObj* %f48046, i64 2)
%argslist57393$y480450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59052 = alloca %struct.ScmObj*, align 8
%argslist57393$y480451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y48045, %struct.ScmObj* %argslist57393$y480450)
store volatile %struct.ScmObj* %argslist57393$y480451, %struct.ScmObj** %stackaddr$prim59052, align 8
%stackaddr$prim59053 = alloca %struct.ScmObj*, align 8
%argslist57393$y480452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48513, %struct.ScmObj* %argslist57393$y480451)
store volatile %struct.ScmObj* %argslist57393$y480452, %struct.ScmObj** %stackaddr$prim59053, align 8
%clofunc59054 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y48045)
musttail call tailcc void %clofunc59054(%struct.ScmObj* %y48045, %struct.ScmObj* %argslist57393$y480452)
ret void
}

define tailcc void @proc_clo$ae48513(%struct.ScmObj* %env$ae48513,%struct.ScmObj* %current_45args57386) {
%stackaddr$env-ref59055 = alloca %struct.ScmObj*, align 8
%k48481 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48513, i64 0)
store %struct.ScmObj* %k48481, %struct.ScmObj** %stackaddr$env-ref59055
%stackaddr$env-ref59056 = alloca %struct.ScmObj*, align 8
%args48047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48513, i64 1)
store %struct.ScmObj* %args48047, %struct.ScmObj** %stackaddr$env-ref59056
%stackaddr$env-ref59057 = alloca %struct.ScmObj*, align 8
%f48046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48513, i64 2)
store %struct.ScmObj* %f48046, %struct.ScmObj** %stackaddr$env-ref59057
%stackaddr$prim59058 = alloca %struct.ScmObj*, align 8
%_95k48482 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57386)
store volatile %struct.ScmObj* %_95k48482, %struct.ScmObj** %stackaddr$prim59058, align 8
%stackaddr$prim59059 = alloca %struct.ScmObj*, align 8
%current_45args57387 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57386)
store volatile %struct.ScmObj* %current_45args57387, %struct.ScmObj** %stackaddr$prim59059, align 8
%stackaddr$prim59060 = alloca %struct.ScmObj*, align 8
%anf_45bind48198 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57387)
store volatile %struct.ScmObj* %anf_45bind48198, %struct.ScmObj** %stackaddr$prim59060, align 8
%stackaddr$makeclosure59061 = alloca %struct.ScmObj*, align 8
%fptrToInt59062 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48516 to i64
%ae48516 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt59062)
store volatile %struct.ScmObj* %ae48516, %struct.ScmObj** %stackaddr$makeclosure59061, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48516, %struct.ScmObj* %k48481, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48516, %struct.ScmObj* %args48047, i64 1)
%argslist57392$anf_45bind481980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59063 = alloca %struct.ScmObj*, align 8
%argslist57392$anf_45bind481981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48046, %struct.ScmObj* %argslist57392$anf_45bind481980)
store volatile %struct.ScmObj* %argslist57392$anf_45bind481981, %struct.ScmObj** %stackaddr$prim59063, align 8
%stackaddr$prim59064 = alloca %struct.ScmObj*, align 8
%argslist57392$anf_45bind481982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48516, %struct.ScmObj* %argslist57392$anf_45bind481981)
store volatile %struct.ScmObj* %argslist57392$anf_45bind481982, %struct.ScmObj** %stackaddr$prim59064, align 8
%clofunc59065 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48198)
musttail call tailcc void %clofunc59065(%struct.ScmObj* %anf_45bind48198, %struct.ScmObj* %argslist57392$anf_45bind481982)
ret void
}

define tailcc void @proc_clo$ae48516(%struct.ScmObj* %env$ae48516,%struct.ScmObj* %current_45args57389) {
%stackaddr$env-ref59066 = alloca %struct.ScmObj*, align 8
%k48481 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48516, i64 0)
store %struct.ScmObj* %k48481, %struct.ScmObj** %stackaddr$env-ref59066
%stackaddr$env-ref59067 = alloca %struct.ScmObj*, align 8
%args48047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48516, i64 1)
store %struct.ScmObj* %args48047, %struct.ScmObj** %stackaddr$env-ref59067
%stackaddr$prim59068 = alloca %struct.ScmObj*, align 8
%_95k48483 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57389)
store volatile %struct.ScmObj* %_95k48483, %struct.ScmObj** %stackaddr$prim59068, align 8
%stackaddr$prim59069 = alloca %struct.ScmObj*, align 8
%current_45args57390 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57389)
store volatile %struct.ScmObj* %current_45args57390, %struct.ScmObj** %stackaddr$prim59069, align 8
%stackaddr$prim59070 = alloca %struct.ScmObj*, align 8
%anf_45bind48199 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57390)
store volatile %struct.ScmObj* %anf_45bind48199, %struct.ScmObj** %stackaddr$prim59070, align 8
%stackaddr$prim59071 = alloca %struct.ScmObj*, align 8
%cpsargs48484 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48481, %struct.ScmObj* %args48047)
store volatile %struct.ScmObj* %cpsargs48484, %struct.ScmObj** %stackaddr$prim59071, align 8
%clofunc59072 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48199)
musttail call tailcc void %clofunc59072(%struct.ScmObj* %anf_45bind48199, %struct.ScmObj* %cpsargs48484)
ret void
}

define tailcc void @proc_clo$ae48488(%struct.ScmObj* %env$ae48488,%struct.ScmObj* %current_45args57397) {
%stackaddr$prim59073 = alloca %struct.ScmObj*, align 8
%k48485 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57397)
store volatile %struct.ScmObj* %k48485, %struct.ScmObj** %stackaddr$prim59073, align 8
%stackaddr$prim59074 = alloca %struct.ScmObj*, align 8
%current_45args57398 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57397)
store volatile %struct.ScmObj* %current_45args57398, %struct.ScmObj** %stackaddr$prim59074, align 8
%stackaddr$prim59075 = alloca %struct.ScmObj*, align 8
%yu48044 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57398)
store volatile %struct.ScmObj* %yu48044, %struct.ScmObj** %stackaddr$prim59075, align 8
%argslist57400$yu480440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59076 = alloca %struct.ScmObj*, align 8
%argslist57400$yu480441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu48044, %struct.ScmObj* %argslist57400$yu480440)
store volatile %struct.ScmObj* %argslist57400$yu480441, %struct.ScmObj** %stackaddr$prim59076, align 8
%stackaddr$prim59077 = alloca %struct.ScmObj*, align 8
%argslist57400$yu480442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48485, %struct.ScmObj* %argslist57400$yu480441)
store volatile %struct.ScmObj* %argslist57400$yu480442, %struct.ScmObj** %stackaddr$prim59077, align 8
%clofunc59078 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu48044)
musttail call tailcc void %clofunc59078(%struct.ScmObj* %yu48044, %struct.ScmObj* %argslist57400$yu480442)
ret void
}