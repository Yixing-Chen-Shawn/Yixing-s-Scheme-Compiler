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
%mainenv60972 = call %struct.ScmObj* @const_init_null()
%mainargs60973 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv60972, %struct.ScmObj* %mainargs60973)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv60970,%struct.ScmObj* %mainargs60971) {
%stackaddr$makeclosure60974 = alloca %struct.ScmObj*, align 8
%fptrToInt60975 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48570 to i64
%ae48570 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt60975)
store volatile %struct.ScmObj* %ae48570, %struct.ScmObj** %stackaddr$makeclosure60974, align 8
%ae48571 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure60976 = alloca %struct.ScmObj*, align 8
%fptrToInt60977 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48572 to i64
%ae48572 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt60977)
store volatile %struct.ScmObj* %ae48572, %struct.ScmObj** %stackaddr$makeclosure60976, align 8
%argslist60969$ae485700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60978 = alloca %struct.ScmObj*, align 8
%argslist60969$ae485701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48572, %struct.ScmObj* %argslist60969$ae485700)
store volatile %struct.ScmObj* %argslist60969$ae485701, %struct.ScmObj** %stackaddr$prim60978, align 8
%stackaddr$prim60979 = alloca %struct.ScmObj*, align 8
%argslist60969$ae485702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48571, %struct.ScmObj* %argslist60969$ae485701)
store volatile %struct.ScmObj* %argslist60969$ae485702, %struct.ScmObj** %stackaddr$prim60979, align 8
%clofunc60980 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48570)
musttail call tailcc void %clofunc60980(%struct.ScmObj* %ae48570, %struct.ScmObj* %argslist60969$ae485702)
ret void
}

define tailcc void @proc_clo$ae48570(%struct.ScmObj* %env$ae48570,%struct.ScmObj* %current_45args60228) {
%stackaddr$prim60981 = alloca %struct.ScmObj*, align 8
%_95k48356 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60228)
store volatile %struct.ScmObj* %_95k48356, %struct.ScmObj** %stackaddr$prim60981, align 8
%stackaddr$prim60982 = alloca %struct.ScmObj*, align 8
%current_45args60229 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60228)
store volatile %struct.ScmObj* %current_45args60229, %struct.ScmObj** %stackaddr$prim60982, align 8
%stackaddr$prim60983 = alloca %struct.ScmObj*, align 8
%anf_45bind48181 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60229)
store volatile %struct.ScmObj* %anf_45bind48181, %struct.ScmObj** %stackaddr$prim60983, align 8
%stackaddr$makeclosure60984 = alloca %struct.ScmObj*, align 8
%fptrToInt60985 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48585 to i64
%ae48585 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt60985)
store volatile %struct.ScmObj* %ae48585, %struct.ScmObj** %stackaddr$makeclosure60984, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48585, %struct.ScmObj* %anf_45bind48181, i64 0)
%ae48586 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure60986 = alloca %struct.ScmObj*, align 8
%fptrToInt60987 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48587 to i64
%ae48587 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt60987)
store volatile %struct.ScmObj* %ae48587, %struct.ScmObj** %stackaddr$makeclosure60986, align 8
%argslist60964$ae485850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60988 = alloca %struct.ScmObj*, align 8
%argslist60964$ae485851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48587, %struct.ScmObj* %argslist60964$ae485850)
store volatile %struct.ScmObj* %argslist60964$ae485851, %struct.ScmObj** %stackaddr$prim60988, align 8
%stackaddr$prim60989 = alloca %struct.ScmObj*, align 8
%argslist60964$ae485852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48586, %struct.ScmObj* %argslist60964$ae485851)
store volatile %struct.ScmObj* %argslist60964$ae485852, %struct.ScmObj** %stackaddr$prim60989, align 8
%clofunc60990 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48585)
musttail call tailcc void %clofunc60990(%struct.ScmObj* %ae48585, %struct.ScmObj* %argslist60964$ae485852)
ret void
}

define tailcc void @proc_clo$ae48585(%struct.ScmObj* %env$ae48585,%struct.ScmObj* %current_45args60231) {
%stackaddr$env-ref60991 = alloca %struct.ScmObj*, align 8
%anf_45bind48181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48585, i64 0)
store %struct.ScmObj* %anf_45bind48181, %struct.ScmObj** %stackaddr$env-ref60991
%stackaddr$prim60992 = alloca %struct.ScmObj*, align 8
%_95k48357 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60231)
store volatile %struct.ScmObj* %_95k48357, %struct.ScmObj** %stackaddr$prim60992, align 8
%stackaddr$prim60993 = alloca %struct.ScmObj*, align 8
%current_45args60232 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60231)
store volatile %struct.ScmObj* %current_45args60232, %struct.ScmObj** %stackaddr$prim60993, align 8
%stackaddr$prim60994 = alloca %struct.ScmObj*, align 8
%anf_45bind48185 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60232)
store volatile %struct.ScmObj* %anf_45bind48185, %struct.ScmObj** %stackaddr$prim60994, align 8
%stackaddr$makeclosure60995 = alloca %struct.ScmObj*, align 8
%fptrToInt60996 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48700 to i64
%ae48700 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt60996)
store volatile %struct.ScmObj* %ae48700, %struct.ScmObj** %stackaddr$makeclosure60995, align 8
%argslist60943$anf_45bind481810 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60997 = alloca %struct.ScmObj*, align 8
%argslist60943$anf_45bind481811 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48185, %struct.ScmObj* %argslist60943$anf_45bind481810)
store volatile %struct.ScmObj* %argslist60943$anf_45bind481811, %struct.ScmObj** %stackaddr$prim60997, align 8
%stackaddr$prim60998 = alloca %struct.ScmObj*, align 8
%argslist60943$anf_45bind481812 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48700, %struct.ScmObj* %argslist60943$anf_45bind481811)
store volatile %struct.ScmObj* %argslist60943$anf_45bind481812, %struct.ScmObj** %stackaddr$prim60998, align 8
%clofunc60999 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48181)
musttail call tailcc void %clofunc60999(%struct.ScmObj* %anf_45bind48181, %struct.ScmObj* %argslist60943$anf_45bind481812)
ret void
}

define tailcc void @proc_clo$ae48700(%struct.ScmObj* %env$ae48700,%struct.ScmObj* %current_45args60234) {
%stackaddr$prim61000 = alloca %struct.ScmObj*, align 8
%_95k48358 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60234)
store volatile %struct.ScmObj* %_95k48358, %struct.ScmObj** %stackaddr$prim61000, align 8
%stackaddr$prim61001 = alloca %struct.ScmObj*, align 8
%current_45args60235 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60234)
store volatile %struct.ScmObj* %current_45args60235, %struct.ScmObj** %stackaddr$prim61001, align 8
%stackaddr$prim61002 = alloca %struct.ScmObj*, align 8
%Ycmb48033 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60235)
store volatile %struct.ScmObj* %Ycmb48033, %struct.ScmObj** %stackaddr$prim61002, align 8
%stackaddr$makeclosure61003 = alloca %struct.ScmObj*, align 8
%fptrToInt61004 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48702 to i64
%ae48702 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt61004)
store volatile %struct.ScmObj* %ae48702, %struct.ScmObj** %stackaddr$makeclosure61003, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48702, %struct.ScmObj* %Ycmb48033, i64 0)
%ae48703 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61005 = alloca %struct.ScmObj*, align 8
%fptrToInt61006 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48704 to i64
%ae48704 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt61006)
store volatile %struct.ScmObj* %ae48704, %struct.ScmObj** %stackaddr$makeclosure61005, align 8
%argslist60942$ae487020 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61007 = alloca %struct.ScmObj*, align 8
%argslist60942$ae487021 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48704, %struct.ScmObj* %argslist60942$ae487020)
store volatile %struct.ScmObj* %argslist60942$ae487021, %struct.ScmObj** %stackaddr$prim61007, align 8
%stackaddr$prim61008 = alloca %struct.ScmObj*, align 8
%argslist60942$ae487022 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48703, %struct.ScmObj* %argslist60942$ae487021)
store volatile %struct.ScmObj* %argslist60942$ae487022, %struct.ScmObj** %stackaddr$prim61008, align 8
%clofunc61009 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48702)
musttail call tailcc void %clofunc61009(%struct.ScmObj* %ae48702, %struct.ScmObj* %argslist60942$ae487022)
ret void
}

define tailcc void @proc_clo$ae48702(%struct.ScmObj* %env$ae48702,%struct.ScmObj* %current_45args60237) {
%stackaddr$env-ref61010 = alloca %struct.ScmObj*, align 8
%Ycmb48033 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48702, i64 0)
store %struct.ScmObj* %Ycmb48033, %struct.ScmObj** %stackaddr$env-ref61010
%stackaddr$prim61011 = alloca %struct.ScmObj*, align 8
%_95k48359 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60237)
store volatile %struct.ScmObj* %_95k48359, %struct.ScmObj** %stackaddr$prim61011, align 8
%stackaddr$prim61012 = alloca %struct.ScmObj*, align 8
%current_45args60238 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60237)
store volatile %struct.ScmObj* %current_45args60238, %struct.ScmObj** %stackaddr$prim61012, align 8
%stackaddr$prim61013 = alloca %struct.ScmObj*, align 8
%anf_45bind48190 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60238)
store volatile %struct.ScmObj* %anf_45bind48190, %struct.ScmObj** %stackaddr$prim61013, align 8
%stackaddr$makeclosure61014 = alloca %struct.ScmObj*, align 8
%fptrToInt61015 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48780 to i64
%ae48780 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt61015)
store volatile %struct.ScmObj* %ae48780, %struct.ScmObj** %stackaddr$makeclosure61014, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48780, %struct.ScmObj* %Ycmb48033, i64 0)
%argslist60926$Ycmb480330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61016 = alloca %struct.ScmObj*, align 8
%argslist60926$Ycmb480331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48190, %struct.ScmObj* %argslist60926$Ycmb480330)
store volatile %struct.ScmObj* %argslist60926$Ycmb480331, %struct.ScmObj** %stackaddr$prim61016, align 8
%stackaddr$prim61017 = alloca %struct.ScmObj*, align 8
%argslist60926$Ycmb480332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48780, %struct.ScmObj* %argslist60926$Ycmb480331)
store volatile %struct.ScmObj* %argslist60926$Ycmb480332, %struct.ScmObj** %stackaddr$prim61017, align 8
%clofunc61018 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48033)
musttail call tailcc void %clofunc61018(%struct.ScmObj* %Ycmb48033, %struct.ScmObj* %argslist60926$Ycmb480332)
ret void
}

define tailcc void @proc_clo$ae48780(%struct.ScmObj* %env$ae48780,%struct.ScmObj* %current_45args60240) {
%stackaddr$env-ref61019 = alloca %struct.ScmObj*, align 8
%Ycmb48033 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48780, i64 0)
store %struct.ScmObj* %Ycmb48033, %struct.ScmObj** %stackaddr$env-ref61019
%stackaddr$prim61020 = alloca %struct.ScmObj*, align 8
%_95k48360 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60240)
store volatile %struct.ScmObj* %_95k48360, %struct.ScmObj** %stackaddr$prim61020, align 8
%stackaddr$prim61021 = alloca %struct.ScmObj*, align 8
%current_45args60241 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60240)
store volatile %struct.ScmObj* %current_45args60241, %struct.ScmObj** %stackaddr$prim61021, align 8
%stackaddr$prim61022 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60241)
store volatile %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$prim61022, align 8
%stackaddr$makeclosure61023 = alloca %struct.ScmObj*, align 8
%fptrToInt61024 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48782 to i64
%ae48782 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt61024)
store volatile %struct.ScmObj* %ae48782, %struct.ScmObj** %stackaddr$makeclosure61023, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48782, %struct.ScmObj* %_37foldr148054, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48782, %struct.ScmObj* %Ycmb48033, i64 1)
%ae48783 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61025 = alloca %struct.ScmObj*, align 8
%fptrToInt61026 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48784 to i64
%ae48784 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt61026)
store volatile %struct.ScmObj* %ae48784, %struct.ScmObj** %stackaddr$makeclosure61025, align 8
%argslist60925$ae487820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61027 = alloca %struct.ScmObj*, align 8
%argslist60925$ae487821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48784, %struct.ScmObj* %argslist60925$ae487820)
store volatile %struct.ScmObj* %argslist60925$ae487821, %struct.ScmObj** %stackaddr$prim61027, align 8
%stackaddr$prim61028 = alloca %struct.ScmObj*, align 8
%argslist60925$ae487822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48783, %struct.ScmObj* %argslist60925$ae487821)
store volatile %struct.ScmObj* %argslist60925$ae487822, %struct.ScmObj** %stackaddr$prim61028, align 8
%clofunc61029 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48782)
musttail call tailcc void %clofunc61029(%struct.ScmObj* %ae48782, %struct.ScmObj* %argslist60925$ae487822)
ret void
}

define tailcc void @proc_clo$ae48782(%struct.ScmObj* %env$ae48782,%struct.ScmObj* %current_45args60243) {
%stackaddr$env-ref61030 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48782, i64 0)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref61030
%stackaddr$env-ref61031 = alloca %struct.ScmObj*, align 8
%Ycmb48033 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48782, i64 1)
store %struct.ScmObj* %Ycmb48033, %struct.ScmObj** %stackaddr$env-ref61031
%stackaddr$prim61032 = alloca %struct.ScmObj*, align 8
%_95k48361 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60243)
store volatile %struct.ScmObj* %_95k48361, %struct.ScmObj** %stackaddr$prim61032, align 8
%stackaddr$prim61033 = alloca %struct.ScmObj*, align 8
%current_45args60244 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60243)
store volatile %struct.ScmObj* %current_45args60244, %struct.ScmObj** %stackaddr$prim61033, align 8
%stackaddr$prim61034 = alloca %struct.ScmObj*, align 8
%anf_45bind48196 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60244)
store volatile %struct.ScmObj* %anf_45bind48196, %struct.ScmObj** %stackaddr$prim61034, align 8
%stackaddr$makeclosure61035 = alloca %struct.ScmObj*, align 8
%fptrToInt61036 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48877 to i64
%ae48877 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt61036)
store volatile %struct.ScmObj* %ae48877, %struct.ScmObj** %stackaddr$makeclosure61035, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48877, %struct.ScmObj* %_37foldr148054, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48877, %struct.ScmObj* %Ycmb48033, i64 1)
%argslist60906$Ycmb480330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61037 = alloca %struct.ScmObj*, align 8
%argslist60906$Ycmb480331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48196, %struct.ScmObj* %argslist60906$Ycmb480330)
store volatile %struct.ScmObj* %argslist60906$Ycmb480331, %struct.ScmObj** %stackaddr$prim61037, align 8
%stackaddr$prim61038 = alloca %struct.ScmObj*, align 8
%argslist60906$Ycmb480332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48877, %struct.ScmObj* %argslist60906$Ycmb480331)
store volatile %struct.ScmObj* %argslist60906$Ycmb480332, %struct.ScmObj** %stackaddr$prim61038, align 8
%clofunc61039 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48033)
musttail call tailcc void %clofunc61039(%struct.ScmObj* %Ycmb48033, %struct.ScmObj* %argslist60906$Ycmb480332)
ret void
}

define tailcc void @proc_clo$ae48877(%struct.ScmObj* %env$ae48877,%struct.ScmObj* %current_45args60246) {
%stackaddr$env-ref61040 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48877, i64 0)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref61040
%stackaddr$env-ref61041 = alloca %struct.ScmObj*, align 8
%Ycmb48033 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48877, i64 1)
store %struct.ScmObj* %Ycmb48033, %struct.ScmObj** %stackaddr$env-ref61041
%stackaddr$prim61042 = alloca %struct.ScmObj*, align 8
%_95k48362 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60246)
store volatile %struct.ScmObj* %_95k48362, %struct.ScmObj** %stackaddr$prim61042, align 8
%stackaddr$prim61043 = alloca %struct.ScmObj*, align 8
%current_45args60247 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60246)
store volatile %struct.ScmObj* %current_45args60247, %struct.ScmObj** %stackaddr$prim61043, align 8
%stackaddr$prim61044 = alloca %struct.ScmObj*, align 8
%_37map148050 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60247)
store volatile %struct.ScmObj* %_37map148050, %struct.ScmObj** %stackaddr$prim61044, align 8
%stackaddr$makeclosure61045 = alloca %struct.ScmObj*, align 8
%fptrToInt61046 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48879 to i64
%ae48879 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt61046)
store volatile %struct.ScmObj* %ae48879, %struct.ScmObj** %stackaddr$makeclosure61045, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48879, %struct.ScmObj* %_37foldr148054, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48879, %struct.ScmObj* %_37map148050, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48879, %struct.ScmObj* %Ycmb48033, i64 2)
%ae48880 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61047 = alloca %struct.ScmObj*, align 8
%fptrToInt61048 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48881 to i64
%ae48881 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt61048)
store volatile %struct.ScmObj* %ae48881, %struct.ScmObj** %stackaddr$makeclosure61047, align 8
%argslist60905$ae488790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61049 = alloca %struct.ScmObj*, align 8
%argslist60905$ae488791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48881, %struct.ScmObj* %argslist60905$ae488790)
store volatile %struct.ScmObj* %argslist60905$ae488791, %struct.ScmObj** %stackaddr$prim61049, align 8
%stackaddr$prim61050 = alloca %struct.ScmObj*, align 8
%argslist60905$ae488792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48880, %struct.ScmObj* %argslist60905$ae488791)
store volatile %struct.ScmObj* %argslist60905$ae488792, %struct.ScmObj** %stackaddr$prim61050, align 8
%clofunc61051 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48879)
musttail call tailcc void %clofunc61051(%struct.ScmObj* %ae48879, %struct.ScmObj* %argslist60905$ae488792)
ret void
}

define tailcc void @proc_clo$ae48879(%struct.ScmObj* %env$ae48879,%struct.ScmObj* %current_45args60249) {
%stackaddr$env-ref61052 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48879, i64 0)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref61052
%stackaddr$env-ref61053 = alloca %struct.ScmObj*, align 8
%_37map148050 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48879, i64 1)
store %struct.ScmObj* %_37map148050, %struct.ScmObj** %stackaddr$env-ref61053
%stackaddr$env-ref61054 = alloca %struct.ScmObj*, align 8
%Ycmb48033 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48879, i64 2)
store %struct.ScmObj* %Ycmb48033, %struct.ScmObj** %stackaddr$env-ref61054
%stackaddr$prim61055 = alloca %struct.ScmObj*, align 8
%_95k48363 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60249)
store volatile %struct.ScmObj* %_95k48363, %struct.ScmObj** %stackaddr$prim61055, align 8
%stackaddr$prim61056 = alloca %struct.ScmObj*, align 8
%current_45args60250 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60249)
store volatile %struct.ScmObj* %current_45args60250, %struct.ScmObj** %stackaddr$prim61056, align 8
%stackaddr$prim61057 = alloca %struct.ScmObj*, align 8
%anf_45bind48203 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60250)
store volatile %struct.ScmObj* %anf_45bind48203, %struct.ScmObj** %stackaddr$prim61057, align 8
%stackaddr$makeclosure61058 = alloca %struct.ScmObj*, align 8
%fptrToInt61059 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49027 to i64
%ae49027 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt61059)
store volatile %struct.ScmObj* %ae49027, %struct.ScmObj** %stackaddr$makeclosure61058, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49027, %struct.ScmObj* %_37foldr148054, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49027, %struct.ScmObj* %_37map148050, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49027, %struct.ScmObj* %Ycmb48033, i64 2)
%argslist60889$Ycmb480330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61060 = alloca %struct.ScmObj*, align 8
%argslist60889$Ycmb480331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48203, %struct.ScmObj* %argslist60889$Ycmb480330)
store volatile %struct.ScmObj* %argslist60889$Ycmb480331, %struct.ScmObj** %stackaddr$prim61060, align 8
%stackaddr$prim61061 = alloca %struct.ScmObj*, align 8
%argslist60889$Ycmb480332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49027, %struct.ScmObj* %argslist60889$Ycmb480331)
store volatile %struct.ScmObj* %argslist60889$Ycmb480332, %struct.ScmObj** %stackaddr$prim61061, align 8
%clofunc61062 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48033)
musttail call tailcc void %clofunc61062(%struct.ScmObj* %Ycmb48033, %struct.ScmObj* %argslist60889$Ycmb480332)
ret void
}

define tailcc void @proc_clo$ae49027(%struct.ScmObj* %env$ae49027,%struct.ScmObj* %current_45args60252) {
%stackaddr$env-ref61063 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49027, i64 0)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref61063
%stackaddr$env-ref61064 = alloca %struct.ScmObj*, align 8
%_37map148050 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49027, i64 1)
store %struct.ScmObj* %_37map148050, %struct.ScmObj** %stackaddr$env-ref61064
%stackaddr$env-ref61065 = alloca %struct.ScmObj*, align 8
%Ycmb48033 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49027, i64 2)
store %struct.ScmObj* %Ycmb48033, %struct.ScmObj** %stackaddr$env-ref61065
%stackaddr$prim61066 = alloca %struct.ScmObj*, align 8
%_95k48364 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60252)
store volatile %struct.ScmObj* %_95k48364, %struct.ScmObj** %stackaddr$prim61066, align 8
%stackaddr$prim61067 = alloca %struct.ScmObj*, align 8
%current_45args60253 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60252)
store volatile %struct.ScmObj* %current_45args60253, %struct.ScmObj** %stackaddr$prim61067, align 8
%stackaddr$prim61068 = alloca %struct.ScmObj*, align 8
%_37take48046 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60253)
store volatile %struct.ScmObj* %_37take48046, %struct.ScmObj** %stackaddr$prim61068, align 8
%stackaddr$makeclosure61069 = alloca %struct.ScmObj*, align 8
%fptrToInt61070 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49029 to i64
%ae49029 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt61070)
store volatile %struct.ScmObj* %ae49029, %struct.ScmObj** %stackaddr$makeclosure61069, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49029, %struct.ScmObj* %_37foldr148054, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49029, %struct.ScmObj* %_37map148050, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49029, %struct.ScmObj* %Ycmb48033, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49029, %struct.ScmObj* %_37take48046, i64 3)
%ae49030 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61071 = alloca %struct.ScmObj*, align 8
%fptrToInt61072 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49031 to i64
%ae49031 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt61072)
store volatile %struct.ScmObj* %ae49031, %struct.ScmObj** %stackaddr$makeclosure61071, align 8
%argslist60888$ae490290 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61073 = alloca %struct.ScmObj*, align 8
%argslist60888$ae490291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49031, %struct.ScmObj* %argslist60888$ae490290)
store volatile %struct.ScmObj* %argslist60888$ae490291, %struct.ScmObj** %stackaddr$prim61073, align 8
%stackaddr$prim61074 = alloca %struct.ScmObj*, align 8
%argslist60888$ae490292 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49030, %struct.ScmObj* %argslist60888$ae490291)
store volatile %struct.ScmObj* %argslist60888$ae490292, %struct.ScmObj** %stackaddr$prim61074, align 8
%clofunc61075 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49029)
musttail call tailcc void %clofunc61075(%struct.ScmObj* %ae49029, %struct.ScmObj* %argslist60888$ae490292)
ret void
}

define tailcc void @proc_clo$ae49029(%struct.ScmObj* %env$ae49029,%struct.ScmObj* %current_45args60255) {
%stackaddr$env-ref61076 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49029, i64 0)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref61076
%stackaddr$env-ref61077 = alloca %struct.ScmObj*, align 8
%_37map148050 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49029, i64 1)
store %struct.ScmObj* %_37map148050, %struct.ScmObj** %stackaddr$env-ref61077
%stackaddr$env-ref61078 = alloca %struct.ScmObj*, align 8
%Ycmb48033 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49029, i64 2)
store %struct.ScmObj* %Ycmb48033, %struct.ScmObj** %stackaddr$env-ref61078
%stackaddr$env-ref61079 = alloca %struct.ScmObj*, align 8
%_37take48046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49029, i64 3)
store %struct.ScmObj* %_37take48046, %struct.ScmObj** %stackaddr$env-ref61079
%stackaddr$prim61080 = alloca %struct.ScmObj*, align 8
%_95k48365 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60255)
store volatile %struct.ScmObj* %_95k48365, %struct.ScmObj** %stackaddr$prim61080, align 8
%stackaddr$prim61081 = alloca %struct.ScmObj*, align 8
%current_45args60256 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60255)
store volatile %struct.ScmObj* %current_45args60256, %struct.ScmObj** %stackaddr$prim61081, align 8
%stackaddr$prim61082 = alloca %struct.ScmObj*, align 8
%anf_45bind48207 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60256)
store volatile %struct.ScmObj* %anf_45bind48207, %struct.ScmObj** %stackaddr$prim61082, align 8
%stackaddr$makeclosure61083 = alloca %struct.ScmObj*, align 8
%fptrToInt61084 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49110 to i64
%ae49110 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt61084)
store volatile %struct.ScmObj* %ae49110, %struct.ScmObj** %stackaddr$makeclosure61083, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49110, %struct.ScmObj* %_37foldr148054, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49110, %struct.ScmObj* %_37map148050, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49110, %struct.ScmObj* %Ycmb48033, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49110, %struct.ScmObj* %_37take48046, i64 3)
%argslist60874$Ycmb480330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61085 = alloca %struct.ScmObj*, align 8
%argslist60874$Ycmb480331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48207, %struct.ScmObj* %argslist60874$Ycmb480330)
store volatile %struct.ScmObj* %argslist60874$Ycmb480331, %struct.ScmObj** %stackaddr$prim61085, align 8
%stackaddr$prim61086 = alloca %struct.ScmObj*, align 8
%argslist60874$Ycmb480332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49110, %struct.ScmObj* %argslist60874$Ycmb480331)
store volatile %struct.ScmObj* %argslist60874$Ycmb480332, %struct.ScmObj** %stackaddr$prim61086, align 8
%clofunc61087 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48033)
musttail call tailcc void %clofunc61087(%struct.ScmObj* %Ycmb48033, %struct.ScmObj* %argslist60874$Ycmb480332)
ret void
}

define tailcc void @proc_clo$ae49110(%struct.ScmObj* %env$ae49110,%struct.ScmObj* %current_45args60258) {
%stackaddr$env-ref61088 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49110, i64 0)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref61088
%stackaddr$env-ref61089 = alloca %struct.ScmObj*, align 8
%_37map148050 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49110, i64 1)
store %struct.ScmObj* %_37map148050, %struct.ScmObj** %stackaddr$env-ref61089
%stackaddr$env-ref61090 = alloca %struct.ScmObj*, align 8
%Ycmb48033 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49110, i64 2)
store %struct.ScmObj* %Ycmb48033, %struct.ScmObj** %stackaddr$env-ref61090
%stackaddr$env-ref61091 = alloca %struct.ScmObj*, align 8
%_37take48046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49110, i64 3)
store %struct.ScmObj* %_37take48046, %struct.ScmObj** %stackaddr$env-ref61091
%stackaddr$prim61092 = alloca %struct.ScmObj*, align 8
%_95k48366 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60258)
store volatile %struct.ScmObj* %_95k48366, %struct.ScmObj** %stackaddr$prim61092, align 8
%stackaddr$prim61093 = alloca %struct.ScmObj*, align 8
%current_45args60259 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60258)
store volatile %struct.ScmObj* %current_45args60259, %struct.ScmObj** %stackaddr$prim61093, align 8
%stackaddr$prim61094 = alloca %struct.ScmObj*, align 8
%_37length48043 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60259)
store volatile %struct.ScmObj* %_37length48043, %struct.ScmObj** %stackaddr$prim61094, align 8
%stackaddr$makeclosure61095 = alloca %struct.ScmObj*, align 8
%fptrToInt61096 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49112 to i64
%ae49112 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt61096)
store volatile %struct.ScmObj* %ae49112, %struct.ScmObj** %stackaddr$makeclosure61095, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49112, %struct.ScmObj* %_37foldr148054, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49112, %struct.ScmObj* %_37map148050, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49112, %struct.ScmObj* %Ycmb48033, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49112, %struct.ScmObj* %_37take48046, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49112, %struct.ScmObj* %_37length48043, i64 4)
%ae49113 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61097 = alloca %struct.ScmObj*, align 8
%fptrToInt61098 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49114 to i64
%ae49114 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt61098)
store volatile %struct.ScmObj* %ae49114, %struct.ScmObj** %stackaddr$makeclosure61097, align 8
%argslist60873$ae491120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61099 = alloca %struct.ScmObj*, align 8
%argslist60873$ae491121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49114, %struct.ScmObj* %argslist60873$ae491120)
store volatile %struct.ScmObj* %argslist60873$ae491121, %struct.ScmObj** %stackaddr$prim61099, align 8
%stackaddr$prim61100 = alloca %struct.ScmObj*, align 8
%argslist60873$ae491122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49113, %struct.ScmObj* %argslist60873$ae491121)
store volatile %struct.ScmObj* %argslist60873$ae491122, %struct.ScmObj** %stackaddr$prim61100, align 8
%clofunc61101 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49112)
musttail call tailcc void %clofunc61101(%struct.ScmObj* %ae49112, %struct.ScmObj* %argslist60873$ae491122)
ret void
}

define tailcc void @proc_clo$ae49112(%struct.ScmObj* %env$ae49112,%struct.ScmObj* %current_45args60261) {
%stackaddr$env-ref61102 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49112, i64 0)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref61102
%stackaddr$env-ref61103 = alloca %struct.ScmObj*, align 8
%_37map148050 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49112, i64 1)
store %struct.ScmObj* %_37map148050, %struct.ScmObj** %stackaddr$env-ref61103
%stackaddr$env-ref61104 = alloca %struct.ScmObj*, align 8
%Ycmb48033 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49112, i64 2)
store %struct.ScmObj* %Ycmb48033, %struct.ScmObj** %stackaddr$env-ref61104
%stackaddr$env-ref61105 = alloca %struct.ScmObj*, align 8
%_37take48046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49112, i64 3)
store %struct.ScmObj* %_37take48046, %struct.ScmObj** %stackaddr$env-ref61105
%stackaddr$env-ref61106 = alloca %struct.ScmObj*, align 8
%_37length48043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49112, i64 4)
store %struct.ScmObj* %_37length48043, %struct.ScmObj** %stackaddr$env-ref61106
%stackaddr$prim61107 = alloca %struct.ScmObj*, align 8
%_95k48367 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60261)
store volatile %struct.ScmObj* %_95k48367, %struct.ScmObj** %stackaddr$prim61107, align 8
%stackaddr$prim61108 = alloca %struct.ScmObj*, align 8
%current_45args60262 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60261)
store volatile %struct.ScmObj* %current_45args60262, %struct.ScmObj** %stackaddr$prim61108, align 8
%stackaddr$prim61109 = alloca %struct.ScmObj*, align 8
%anf_45bind48212 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60262)
store volatile %struct.ScmObj* %anf_45bind48212, %struct.ScmObj** %stackaddr$prim61109, align 8
%stackaddr$makeclosure61110 = alloca %struct.ScmObj*, align 8
%fptrToInt61111 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49189 to i64
%ae49189 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt61111)
store volatile %struct.ScmObj* %ae49189, %struct.ScmObj** %stackaddr$makeclosure61110, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49189, %struct.ScmObj* %_37foldr148054, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49189, %struct.ScmObj* %_37map148050, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49189, %struct.ScmObj* %Ycmb48033, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49189, %struct.ScmObj* %_37take48046, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49189, %struct.ScmObj* %_37length48043, i64 4)
%argslist60857$Ycmb480330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61112 = alloca %struct.ScmObj*, align 8
%argslist60857$Ycmb480331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48212, %struct.ScmObj* %argslist60857$Ycmb480330)
store volatile %struct.ScmObj* %argslist60857$Ycmb480331, %struct.ScmObj** %stackaddr$prim61112, align 8
%stackaddr$prim61113 = alloca %struct.ScmObj*, align 8
%argslist60857$Ycmb480332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49189, %struct.ScmObj* %argslist60857$Ycmb480331)
store volatile %struct.ScmObj* %argslist60857$Ycmb480332, %struct.ScmObj** %stackaddr$prim61113, align 8
%clofunc61114 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48033)
musttail call tailcc void %clofunc61114(%struct.ScmObj* %Ycmb48033, %struct.ScmObj* %argslist60857$Ycmb480332)
ret void
}

define tailcc void @proc_clo$ae49189(%struct.ScmObj* %env$ae49189,%struct.ScmObj* %current_45args60264) {
%stackaddr$env-ref61115 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49189, i64 0)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref61115
%stackaddr$env-ref61116 = alloca %struct.ScmObj*, align 8
%_37map148050 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49189, i64 1)
store %struct.ScmObj* %_37map148050, %struct.ScmObj** %stackaddr$env-ref61116
%stackaddr$env-ref61117 = alloca %struct.ScmObj*, align 8
%Ycmb48033 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49189, i64 2)
store %struct.ScmObj* %Ycmb48033, %struct.ScmObj** %stackaddr$env-ref61117
%stackaddr$env-ref61118 = alloca %struct.ScmObj*, align 8
%_37take48046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49189, i64 3)
store %struct.ScmObj* %_37take48046, %struct.ScmObj** %stackaddr$env-ref61118
%stackaddr$env-ref61119 = alloca %struct.ScmObj*, align 8
%_37length48043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49189, i64 4)
store %struct.ScmObj* %_37length48043, %struct.ScmObj** %stackaddr$env-ref61119
%stackaddr$prim61120 = alloca %struct.ScmObj*, align 8
%_95k48368 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60264)
store volatile %struct.ScmObj* %_95k48368, %struct.ScmObj** %stackaddr$prim61120, align 8
%stackaddr$prim61121 = alloca %struct.ScmObj*, align 8
%current_45args60265 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60264)
store volatile %struct.ScmObj* %current_45args60265, %struct.ScmObj** %stackaddr$prim61121, align 8
%stackaddr$prim61122 = alloca %struct.ScmObj*, align 8
%_37foldl148038 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60265)
store volatile %struct.ScmObj* %_37foldl148038, %struct.ScmObj** %stackaddr$prim61122, align 8
%stackaddr$makeclosure61123 = alloca %struct.ScmObj*, align 8
%fptrToInt61124 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49191 to i64
%ae49191 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt61124)
store volatile %struct.ScmObj* %ae49191, %struct.ScmObj** %stackaddr$makeclosure61123, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49191, %struct.ScmObj* %_37foldr148054, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49191, %struct.ScmObj* %_37foldl148038, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49191, %struct.ScmObj* %_37map148050, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49191, %struct.ScmObj* %Ycmb48033, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49191, %struct.ScmObj* %_37take48046, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49191, %struct.ScmObj* %_37length48043, i64 5)
%ae49192 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61125 = alloca %struct.ScmObj*, align 8
%fptrToInt61126 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49193 to i64
%ae49193 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt61126)
store volatile %struct.ScmObj* %ae49193, %struct.ScmObj** %stackaddr$makeclosure61125, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49193, %struct.ScmObj* %_37foldl148038, i64 0)
%argslist60856$ae491910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61127 = alloca %struct.ScmObj*, align 8
%argslist60856$ae491911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49193, %struct.ScmObj* %argslist60856$ae491910)
store volatile %struct.ScmObj* %argslist60856$ae491911, %struct.ScmObj** %stackaddr$prim61127, align 8
%stackaddr$prim61128 = alloca %struct.ScmObj*, align 8
%argslist60856$ae491912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49192, %struct.ScmObj* %argslist60856$ae491911)
store volatile %struct.ScmObj* %argslist60856$ae491912, %struct.ScmObj** %stackaddr$prim61128, align 8
%clofunc61129 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49191)
musttail call tailcc void %clofunc61129(%struct.ScmObj* %ae49191, %struct.ScmObj* %argslist60856$ae491912)
ret void
}

define tailcc void @proc_clo$ae49191(%struct.ScmObj* %env$ae49191,%struct.ScmObj* %current_45args60267) {
%stackaddr$env-ref61130 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49191, i64 0)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref61130
%stackaddr$env-ref61131 = alloca %struct.ScmObj*, align 8
%_37foldl148038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49191, i64 1)
store %struct.ScmObj* %_37foldl148038, %struct.ScmObj** %stackaddr$env-ref61131
%stackaddr$env-ref61132 = alloca %struct.ScmObj*, align 8
%_37map148050 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49191, i64 2)
store %struct.ScmObj* %_37map148050, %struct.ScmObj** %stackaddr$env-ref61132
%stackaddr$env-ref61133 = alloca %struct.ScmObj*, align 8
%Ycmb48033 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49191, i64 3)
store %struct.ScmObj* %Ycmb48033, %struct.ScmObj** %stackaddr$env-ref61133
%stackaddr$env-ref61134 = alloca %struct.ScmObj*, align 8
%_37take48046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49191, i64 4)
store %struct.ScmObj* %_37take48046, %struct.ScmObj** %stackaddr$env-ref61134
%stackaddr$env-ref61135 = alloca %struct.ScmObj*, align 8
%_37length48043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49191, i64 5)
store %struct.ScmObj* %_37length48043, %struct.ScmObj** %stackaddr$env-ref61135
%stackaddr$prim61136 = alloca %struct.ScmObj*, align 8
%_95k48369 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60267)
store volatile %struct.ScmObj* %_95k48369, %struct.ScmObj** %stackaddr$prim61136, align 8
%stackaddr$prim61137 = alloca %struct.ScmObj*, align 8
%current_45args60268 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60267)
store volatile %struct.ScmObj* %current_45args60268, %struct.ScmObj** %stackaddr$prim61137, align 8
%stackaddr$prim61138 = alloca %struct.ScmObj*, align 8
%_37last48076 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60268)
store volatile %struct.ScmObj* %_37last48076, %struct.ScmObj** %stackaddr$prim61138, align 8
%stackaddr$makeclosure61139 = alloca %struct.ScmObj*, align 8
%fptrToInt61140 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49245 to i64
%ae49245 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt61140)
store volatile %struct.ScmObj* %ae49245, %struct.ScmObj** %stackaddr$makeclosure61139, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49245, %struct.ScmObj* %_37foldr148054, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49245, %struct.ScmObj* %_37foldl148038, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49245, %struct.ScmObj* %_37map148050, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49245, %struct.ScmObj* %Ycmb48033, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49245, %struct.ScmObj* %_37last48076, i64 4)
%ae49246 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61141 = alloca %struct.ScmObj*, align 8
%fptrToInt61142 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49247 to i64
%ae49247 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt61142)
store volatile %struct.ScmObj* %ae49247, %struct.ScmObj** %stackaddr$makeclosure61141, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49247, %struct.ScmObj* %_37take48046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49247, %struct.ScmObj* %_37length48043, i64 1)
%argslist60842$ae492450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61143 = alloca %struct.ScmObj*, align 8
%argslist60842$ae492451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49247, %struct.ScmObj* %argslist60842$ae492450)
store volatile %struct.ScmObj* %argslist60842$ae492451, %struct.ScmObj** %stackaddr$prim61143, align 8
%stackaddr$prim61144 = alloca %struct.ScmObj*, align 8
%argslist60842$ae492452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49246, %struct.ScmObj* %argslist60842$ae492451)
store volatile %struct.ScmObj* %argslist60842$ae492452, %struct.ScmObj** %stackaddr$prim61144, align 8
%clofunc61145 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49245)
musttail call tailcc void %clofunc61145(%struct.ScmObj* %ae49245, %struct.ScmObj* %argslist60842$ae492452)
ret void
}

define tailcc void @proc_clo$ae49245(%struct.ScmObj* %env$ae49245,%struct.ScmObj* %current_45args60270) {
%stackaddr$env-ref61146 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49245, i64 0)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref61146
%stackaddr$env-ref61147 = alloca %struct.ScmObj*, align 8
%_37foldl148038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49245, i64 1)
store %struct.ScmObj* %_37foldl148038, %struct.ScmObj** %stackaddr$env-ref61147
%stackaddr$env-ref61148 = alloca %struct.ScmObj*, align 8
%_37map148050 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49245, i64 2)
store %struct.ScmObj* %_37map148050, %struct.ScmObj** %stackaddr$env-ref61148
%stackaddr$env-ref61149 = alloca %struct.ScmObj*, align 8
%Ycmb48033 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49245, i64 3)
store %struct.ScmObj* %Ycmb48033, %struct.ScmObj** %stackaddr$env-ref61149
%stackaddr$env-ref61150 = alloca %struct.ScmObj*, align 8
%_37last48076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49245, i64 4)
store %struct.ScmObj* %_37last48076, %struct.ScmObj** %stackaddr$env-ref61150
%stackaddr$prim61151 = alloca %struct.ScmObj*, align 8
%_95k48370 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60270)
store volatile %struct.ScmObj* %_95k48370, %struct.ScmObj** %stackaddr$prim61151, align 8
%stackaddr$prim61152 = alloca %struct.ScmObj*, align 8
%current_45args60271 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60270)
store volatile %struct.ScmObj* %current_45args60271, %struct.ScmObj** %stackaddr$prim61152, align 8
%stackaddr$prim61153 = alloca %struct.ScmObj*, align 8
%_37drop_45right48073 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60271)
store volatile %struct.ScmObj* %_37drop_45right48073, %struct.ScmObj** %stackaddr$prim61153, align 8
%stackaddr$makeclosure61154 = alloca %struct.ScmObj*, align 8
%fptrToInt61155 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49275 to i64
%ae49275 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt61155)
store volatile %struct.ScmObj* %ae49275, %struct.ScmObj** %stackaddr$makeclosure61154, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49275, %struct.ScmObj* %_37foldr148054, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49275, %struct.ScmObj* %_37foldl148038, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49275, %struct.ScmObj* %_37drop_45right48073, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49275, %struct.ScmObj* %Ycmb48033, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49275, %struct.ScmObj* %_37last48076, i64 4)
%ae49276 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61156 = alloca %struct.ScmObj*, align 8
%fptrToInt61157 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49277 to i64
%ae49277 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt61157)
store volatile %struct.ScmObj* %ae49277, %struct.ScmObj** %stackaddr$makeclosure61156, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49277, %struct.ScmObj* %_37foldr148054, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49277, %struct.ScmObj* %_37map148050, i64 1)
%argslist60832$ae492750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61158 = alloca %struct.ScmObj*, align 8
%argslist60832$ae492751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49277, %struct.ScmObj* %argslist60832$ae492750)
store volatile %struct.ScmObj* %argslist60832$ae492751, %struct.ScmObj** %stackaddr$prim61158, align 8
%stackaddr$prim61159 = alloca %struct.ScmObj*, align 8
%argslist60832$ae492752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49276, %struct.ScmObj* %argslist60832$ae492751)
store volatile %struct.ScmObj* %argslist60832$ae492752, %struct.ScmObj** %stackaddr$prim61159, align 8
%clofunc61160 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49275)
musttail call tailcc void %clofunc61160(%struct.ScmObj* %ae49275, %struct.ScmObj* %argslist60832$ae492752)
ret void
}

define tailcc void @proc_clo$ae49275(%struct.ScmObj* %env$ae49275,%struct.ScmObj* %current_45args60273) {
%stackaddr$env-ref61161 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49275, i64 0)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref61161
%stackaddr$env-ref61162 = alloca %struct.ScmObj*, align 8
%_37foldl148038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49275, i64 1)
store %struct.ScmObj* %_37foldl148038, %struct.ScmObj** %stackaddr$env-ref61162
%stackaddr$env-ref61163 = alloca %struct.ScmObj*, align 8
%_37drop_45right48073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49275, i64 2)
store %struct.ScmObj* %_37drop_45right48073, %struct.ScmObj** %stackaddr$env-ref61163
%stackaddr$env-ref61164 = alloca %struct.ScmObj*, align 8
%Ycmb48033 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49275, i64 3)
store %struct.ScmObj* %Ycmb48033, %struct.ScmObj** %stackaddr$env-ref61164
%stackaddr$env-ref61165 = alloca %struct.ScmObj*, align 8
%_37last48076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49275, i64 4)
store %struct.ScmObj* %_37last48076, %struct.ScmObj** %stackaddr$env-ref61165
%stackaddr$prim61166 = alloca %struct.ScmObj*, align 8
%_95k48371 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60273)
store volatile %struct.ScmObj* %_95k48371, %struct.ScmObj** %stackaddr$prim61166, align 8
%stackaddr$prim61167 = alloca %struct.ScmObj*, align 8
%current_45args60274 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60273)
store volatile %struct.ScmObj* %current_45args60274, %struct.ScmObj** %stackaddr$prim61167, align 8
%stackaddr$prim61168 = alloca %struct.ScmObj*, align 8
%anf_45bind48228 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60274)
store volatile %struct.ScmObj* %anf_45bind48228, %struct.ScmObj** %stackaddr$prim61168, align 8
%stackaddr$makeclosure61169 = alloca %struct.ScmObj*, align 8
%fptrToInt61170 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49659 to i64
%ae49659 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt61170)
store volatile %struct.ScmObj* %ae49659, %struct.ScmObj** %stackaddr$makeclosure61169, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49659, %struct.ScmObj* %_37foldr148054, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49659, %struct.ScmObj* %_37foldl148038, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49659, %struct.ScmObj* %_37drop_45right48073, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49659, %struct.ScmObj* %Ycmb48033, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49659, %struct.ScmObj* %_37last48076, i64 4)
%argslist60772$Ycmb480330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61171 = alloca %struct.ScmObj*, align 8
%argslist60772$Ycmb480331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48228, %struct.ScmObj* %argslist60772$Ycmb480330)
store volatile %struct.ScmObj* %argslist60772$Ycmb480331, %struct.ScmObj** %stackaddr$prim61171, align 8
%stackaddr$prim61172 = alloca %struct.ScmObj*, align 8
%argslist60772$Ycmb480332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49659, %struct.ScmObj* %argslist60772$Ycmb480331)
store volatile %struct.ScmObj* %argslist60772$Ycmb480332, %struct.ScmObj** %stackaddr$prim61172, align 8
%clofunc61173 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48033)
musttail call tailcc void %clofunc61173(%struct.ScmObj* %Ycmb48033, %struct.ScmObj* %argslist60772$Ycmb480332)
ret void
}

define tailcc void @proc_clo$ae49659(%struct.ScmObj* %env$ae49659,%struct.ScmObj* %current_45args60276) {
%stackaddr$env-ref61174 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49659, i64 0)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref61174
%stackaddr$env-ref61175 = alloca %struct.ScmObj*, align 8
%_37foldl148038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49659, i64 1)
store %struct.ScmObj* %_37foldl148038, %struct.ScmObj** %stackaddr$env-ref61175
%stackaddr$env-ref61176 = alloca %struct.ScmObj*, align 8
%_37drop_45right48073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49659, i64 2)
store %struct.ScmObj* %_37drop_45right48073, %struct.ScmObj** %stackaddr$env-ref61176
%stackaddr$env-ref61177 = alloca %struct.ScmObj*, align 8
%Ycmb48033 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49659, i64 3)
store %struct.ScmObj* %Ycmb48033, %struct.ScmObj** %stackaddr$env-ref61177
%stackaddr$env-ref61178 = alloca %struct.ScmObj*, align 8
%_37last48076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49659, i64 4)
store %struct.ScmObj* %_37last48076, %struct.ScmObj** %stackaddr$env-ref61178
%stackaddr$prim61179 = alloca %struct.ScmObj*, align 8
%_95k48372 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60276)
store volatile %struct.ScmObj* %_95k48372, %struct.ScmObj** %stackaddr$prim61179, align 8
%stackaddr$prim61180 = alloca %struct.ScmObj*, align 8
%current_45args60277 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60276)
store volatile %struct.ScmObj* %current_45args60277, %struct.ScmObj** %stackaddr$prim61180, align 8
%stackaddr$prim61181 = alloca %struct.ScmObj*, align 8
%_37foldr48059 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60277)
store volatile %struct.ScmObj* %_37foldr48059, %struct.ScmObj** %stackaddr$prim61181, align 8
%stackaddr$makeclosure61182 = alloca %struct.ScmObj*, align 8
%fptrToInt61183 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49661 to i64
%ae49661 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt61183)
store volatile %struct.ScmObj* %ae49661, %struct.ScmObj** %stackaddr$makeclosure61182, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49661, %struct.ScmObj* %_37foldr148054, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49661, %struct.ScmObj* %_37foldl148038, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49661, %struct.ScmObj* %_37drop_45right48073, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49661, %struct.ScmObj* %Ycmb48033, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49661, %struct.ScmObj* %_37last48076, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49661, %struct.ScmObj* %_37foldr48059, i64 5)
%ae49662 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61184 = alloca %struct.ScmObj*, align 8
%fptrToInt61185 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49663 to i64
%ae49663 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt61185)
store volatile %struct.ScmObj* %ae49663, %struct.ScmObj** %stackaddr$makeclosure61184, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49663, %struct.ScmObj* %_37foldr148054, i64 0)
%argslist60771$ae496610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61186 = alloca %struct.ScmObj*, align 8
%argslist60771$ae496611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49663, %struct.ScmObj* %argslist60771$ae496610)
store volatile %struct.ScmObj* %argslist60771$ae496611, %struct.ScmObj** %stackaddr$prim61186, align 8
%stackaddr$prim61187 = alloca %struct.ScmObj*, align 8
%argslist60771$ae496612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49662, %struct.ScmObj* %argslist60771$ae496611)
store volatile %struct.ScmObj* %argslist60771$ae496612, %struct.ScmObj** %stackaddr$prim61187, align 8
%clofunc61188 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49661)
musttail call tailcc void %clofunc61188(%struct.ScmObj* %ae49661, %struct.ScmObj* %argslist60771$ae496612)
ret void
}

define tailcc void @proc_clo$ae49661(%struct.ScmObj* %env$ae49661,%struct.ScmObj* %current_45args60279) {
%stackaddr$env-ref61189 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49661, i64 0)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref61189
%stackaddr$env-ref61190 = alloca %struct.ScmObj*, align 8
%_37foldl148038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49661, i64 1)
store %struct.ScmObj* %_37foldl148038, %struct.ScmObj** %stackaddr$env-ref61190
%stackaddr$env-ref61191 = alloca %struct.ScmObj*, align 8
%_37drop_45right48073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49661, i64 2)
store %struct.ScmObj* %_37drop_45right48073, %struct.ScmObj** %stackaddr$env-ref61191
%stackaddr$env-ref61192 = alloca %struct.ScmObj*, align 8
%Ycmb48033 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49661, i64 3)
store %struct.ScmObj* %Ycmb48033, %struct.ScmObj** %stackaddr$env-ref61192
%stackaddr$env-ref61193 = alloca %struct.ScmObj*, align 8
%_37last48076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49661, i64 4)
store %struct.ScmObj* %_37last48076, %struct.ScmObj** %stackaddr$env-ref61193
%stackaddr$env-ref61194 = alloca %struct.ScmObj*, align 8
%_37foldr48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49661, i64 5)
store %struct.ScmObj* %_37foldr48059, %struct.ScmObj** %stackaddr$env-ref61194
%stackaddr$prim61195 = alloca %struct.ScmObj*, align 8
%_95k48373 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60279)
store volatile %struct.ScmObj* %_95k48373, %struct.ScmObj** %stackaddr$prim61195, align 8
%stackaddr$prim61196 = alloca %struct.ScmObj*, align 8
%current_45args60280 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60279)
store volatile %struct.ScmObj* %current_45args60280, %struct.ScmObj** %stackaddr$prim61196, align 8
%stackaddr$prim61197 = alloca %struct.ScmObj*, align 8
%_37map148085 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60280)
store volatile %struct.ScmObj* %_37map148085, %struct.ScmObj** %stackaddr$prim61197, align 8
%stackaddr$makeclosure61198 = alloca %struct.ScmObj*, align 8
%fptrToInt61199 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49738 to i64
%ae49738 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt61199)
store volatile %struct.ScmObj* %ae49738, %struct.ScmObj** %stackaddr$makeclosure61198, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49738, %struct.ScmObj* %_37foldr148054, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49738, %struct.ScmObj* %_37foldl148038, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49738, %struct.ScmObj* %_37map148085, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49738, %struct.ScmObj* %Ycmb48033, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49738, %struct.ScmObj* %_37foldr48059, i64 4)
%ae49739 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61200 = alloca %struct.ScmObj*, align 8
%fptrToInt61201 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49740 to i64
%ae49740 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt61201)
store volatile %struct.ScmObj* %ae49740, %struct.ScmObj** %stackaddr$makeclosure61200, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49740, %struct.ScmObj* %_37drop_45right48073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49740, %struct.ScmObj* %_37last48076, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49740, %struct.ScmObj* %_37foldr48059, i64 2)
%argslist60752$ae497380 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61202 = alloca %struct.ScmObj*, align 8
%argslist60752$ae497381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49740, %struct.ScmObj* %argslist60752$ae497380)
store volatile %struct.ScmObj* %argslist60752$ae497381, %struct.ScmObj** %stackaddr$prim61202, align 8
%stackaddr$prim61203 = alloca %struct.ScmObj*, align 8
%argslist60752$ae497382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49739, %struct.ScmObj* %argslist60752$ae497381)
store volatile %struct.ScmObj* %argslist60752$ae497382, %struct.ScmObj** %stackaddr$prim61203, align 8
%clofunc61204 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49738)
musttail call tailcc void %clofunc61204(%struct.ScmObj* %ae49738, %struct.ScmObj* %argslist60752$ae497382)
ret void
}

define tailcc void @proc_clo$ae49738(%struct.ScmObj* %env$ae49738,%struct.ScmObj* %current_45args60282) {
%stackaddr$env-ref61205 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49738, i64 0)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref61205
%stackaddr$env-ref61206 = alloca %struct.ScmObj*, align 8
%_37foldl148038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49738, i64 1)
store %struct.ScmObj* %_37foldl148038, %struct.ScmObj** %stackaddr$env-ref61206
%stackaddr$env-ref61207 = alloca %struct.ScmObj*, align 8
%_37map148085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49738, i64 2)
store %struct.ScmObj* %_37map148085, %struct.ScmObj** %stackaddr$env-ref61207
%stackaddr$env-ref61208 = alloca %struct.ScmObj*, align 8
%Ycmb48033 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49738, i64 3)
store %struct.ScmObj* %Ycmb48033, %struct.ScmObj** %stackaddr$env-ref61208
%stackaddr$env-ref61209 = alloca %struct.ScmObj*, align 8
%_37foldr48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49738, i64 4)
store %struct.ScmObj* %_37foldr48059, %struct.ScmObj** %stackaddr$env-ref61209
%stackaddr$prim61210 = alloca %struct.ScmObj*, align 8
%_95k48374 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60282)
store volatile %struct.ScmObj* %_95k48374, %struct.ScmObj** %stackaddr$prim61210, align 8
%stackaddr$prim61211 = alloca %struct.ScmObj*, align 8
%current_45args60283 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60282)
store volatile %struct.ScmObj* %current_45args60283, %struct.ScmObj** %stackaddr$prim61211, align 8
%stackaddr$prim61212 = alloca %struct.ScmObj*, align 8
%_37map48080 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60283)
store volatile %struct.ScmObj* %_37map48080, %struct.ScmObj** %stackaddr$prim61212, align 8
%stackaddr$makeclosure61213 = alloca %struct.ScmObj*, align 8
%fptrToInt61214 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49884 to i64
%ae49884 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt61214)
store volatile %struct.ScmObj* %ae49884, %struct.ScmObj** %stackaddr$makeclosure61213, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49884, %struct.ScmObj* %_37foldl148038, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49884, %struct.ScmObj* %Ycmb48033, i64 1)
%ae49885 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61215 = alloca %struct.ScmObj*, align 8
%fptrToInt61216 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49886 to i64
%ae49886 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt61216)
store volatile %struct.ScmObj* %ae49886, %struct.ScmObj** %stackaddr$makeclosure61215, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49886, %struct.ScmObj* %_37foldr148054, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49886, %struct.ScmObj* %_37map148085, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49886, %struct.ScmObj* %_37foldr48059, i64 2)
%argslist60735$ae498840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61217 = alloca %struct.ScmObj*, align 8
%argslist60735$ae498841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49886, %struct.ScmObj* %argslist60735$ae498840)
store volatile %struct.ScmObj* %argslist60735$ae498841, %struct.ScmObj** %stackaddr$prim61217, align 8
%stackaddr$prim61218 = alloca %struct.ScmObj*, align 8
%argslist60735$ae498842 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49885, %struct.ScmObj* %argslist60735$ae498841)
store volatile %struct.ScmObj* %argslist60735$ae498842, %struct.ScmObj** %stackaddr$prim61218, align 8
%clofunc61219 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49884)
musttail call tailcc void %clofunc61219(%struct.ScmObj* %ae49884, %struct.ScmObj* %argslist60735$ae498842)
ret void
}

define tailcc void @proc_clo$ae49884(%struct.ScmObj* %env$ae49884,%struct.ScmObj* %current_45args60285) {
%stackaddr$env-ref61220 = alloca %struct.ScmObj*, align 8
%_37foldl148038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49884, i64 0)
store %struct.ScmObj* %_37foldl148038, %struct.ScmObj** %stackaddr$env-ref61220
%stackaddr$env-ref61221 = alloca %struct.ScmObj*, align 8
%Ycmb48033 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49884, i64 1)
store %struct.ScmObj* %Ycmb48033, %struct.ScmObj** %stackaddr$env-ref61221
%stackaddr$prim61222 = alloca %struct.ScmObj*, align 8
%_95k48375 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60285)
store volatile %struct.ScmObj* %_95k48375, %struct.ScmObj** %stackaddr$prim61222, align 8
%stackaddr$prim61223 = alloca %struct.ScmObj*, align 8
%current_45args60286 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60285)
store volatile %struct.ScmObj* %current_45args60286, %struct.ScmObj** %stackaddr$prim61223, align 8
%stackaddr$prim61224 = alloca %struct.ScmObj*, align 8
%anf_45bind48248 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60286)
store volatile %struct.ScmObj* %anf_45bind48248, %struct.ScmObj** %stackaddr$prim61224, align 8
%stackaddr$makeclosure61225 = alloca %struct.ScmObj*, align 8
%fptrToInt61226 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50276 to i64
%ae50276 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt61226)
store volatile %struct.ScmObj* %ae50276, %struct.ScmObj** %stackaddr$makeclosure61225, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50276, %struct.ScmObj* %_37foldl148038, i64 0)
%argslist60675$Ycmb480330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61227 = alloca %struct.ScmObj*, align 8
%argslist60675$Ycmb480331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48248, %struct.ScmObj* %argslist60675$Ycmb480330)
store volatile %struct.ScmObj* %argslist60675$Ycmb480331, %struct.ScmObj** %stackaddr$prim61227, align 8
%stackaddr$prim61228 = alloca %struct.ScmObj*, align 8
%argslist60675$Ycmb480332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50276, %struct.ScmObj* %argslist60675$Ycmb480331)
store volatile %struct.ScmObj* %argslist60675$Ycmb480332, %struct.ScmObj** %stackaddr$prim61228, align 8
%clofunc61229 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48033)
musttail call tailcc void %clofunc61229(%struct.ScmObj* %Ycmb48033, %struct.ScmObj* %argslist60675$Ycmb480332)
ret void
}

define tailcc void @proc_clo$ae50276(%struct.ScmObj* %env$ae50276,%struct.ScmObj* %current_45args60288) {
%stackaddr$env-ref61230 = alloca %struct.ScmObj*, align 8
%_37foldl148038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50276, i64 0)
store %struct.ScmObj* %_37foldl148038, %struct.ScmObj** %stackaddr$env-ref61230
%stackaddr$prim61231 = alloca %struct.ScmObj*, align 8
%_95k48376 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60288)
store volatile %struct.ScmObj* %_95k48376, %struct.ScmObj** %stackaddr$prim61231, align 8
%stackaddr$prim61232 = alloca %struct.ScmObj*, align 8
%current_45args60289 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60288)
store volatile %struct.ScmObj* %current_45args60289, %struct.ScmObj** %stackaddr$prim61232, align 8
%stackaddr$prim61233 = alloca %struct.ScmObj*, align 8
%_37foldl48136 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60289)
store volatile %struct.ScmObj* %_37foldl48136, %struct.ScmObj** %stackaddr$prim61233, align 8
%stackaddr$makeclosure61234 = alloca %struct.ScmObj*, align 8
%fptrToInt61235 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50278 to i64
%ae50278 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt61235)
store volatile %struct.ScmObj* %ae50278, %struct.ScmObj** %stackaddr$makeclosure61234, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50278, %struct.ScmObj* %_37foldl148038, i64 0)
%ae50279 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61236 = alloca %struct.ScmObj*, align 8
%fptrToInt61237 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50280 to i64
%ae50280 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt61237)
store volatile %struct.ScmObj* %ae50280, %struct.ScmObj** %stackaddr$makeclosure61236, align 8
%argslist60674$ae502780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61238 = alloca %struct.ScmObj*, align 8
%argslist60674$ae502781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50280, %struct.ScmObj* %argslist60674$ae502780)
store volatile %struct.ScmObj* %argslist60674$ae502781, %struct.ScmObj** %stackaddr$prim61238, align 8
%stackaddr$prim61239 = alloca %struct.ScmObj*, align 8
%argslist60674$ae502782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50279, %struct.ScmObj* %argslist60674$ae502781)
store volatile %struct.ScmObj* %argslist60674$ae502782, %struct.ScmObj** %stackaddr$prim61239, align 8
%clofunc61240 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50278)
musttail call tailcc void %clofunc61240(%struct.ScmObj* %ae50278, %struct.ScmObj* %argslist60674$ae502782)
ret void
}

define tailcc void @proc_clo$ae50278(%struct.ScmObj* %env$ae50278,%struct.ScmObj* %current_45args60291) {
%stackaddr$env-ref61241 = alloca %struct.ScmObj*, align 8
%_37foldl148038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50278, i64 0)
store %struct.ScmObj* %_37foldl148038, %struct.ScmObj** %stackaddr$env-ref61241
%stackaddr$prim61242 = alloca %struct.ScmObj*, align 8
%_95k48377 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60291)
store volatile %struct.ScmObj* %_95k48377, %struct.ScmObj** %stackaddr$prim61242, align 8
%stackaddr$prim61243 = alloca %struct.ScmObj*, align 8
%current_45args60292 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60291)
store volatile %struct.ScmObj* %current_45args60292, %struct.ScmObj** %stackaddr$prim61243, align 8
%stackaddr$prim61244 = alloca %struct.ScmObj*, align 8
%_37_6248133 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60292)
store volatile %struct.ScmObj* %_37_6248133, %struct.ScmObj** %stackaddr$prim61244, align 8
%stackaddr$makeclosure61245 = alloca %struct.ScmObj*, align 8
%fptrToInt61246 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50302 to i64
%ae50302 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt61246)
store volatile %struct.ScmObj* %ae50302, %struct.ScmObj** %stackaddr$makeclosure61245, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50302, %struct.ScmObj* %_37foldl148038, i64 0)
%ae50303 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61247 = alloca %struct.ScmObj*, align 8
%fptrToInt61248 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50304 to i64
%ae50304 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt61248)
store volatile %struct.ScmObj* %ae50304, %struct.ScmObj** %stackaddr$makeclosure61247, align 8
%argslist60668$ae503020 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61249 = alloca %struct.ScmObj*, align 8
%argslist60668$ae503021 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50304, %struct.ScmObj* %argslist60668$ae503020)
store volatile %struct.ScmObj* %argslist60668$ae503021, %struct.ScmObj** %stackaddr$prim61249, align 8
%stackaddr$prim61250 = alloca %struct.ScmObj*, align 8
%argslist60668$ae503022 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50303, %struct.ScmObj* %argslist60668$ae503021)
store volatile %struct.ScmObj* %argslist60668$ae503022, %struct.ScmObj** %stackaddr$prim61250, align 8
%clofunc61251 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50302)
musttail call tailcc void %clofunc61251(%struct.ScmObj* %ae50302, %struct.ScmObj* %argslist60668$ae503022)
ret void
}

define tailcc void @proc_clo$ae50302(%struct.ScmObj* %env$ae50302,%struct.ScmObj* %current_45args60294) {
%stackaddr$env-ref61252 = alloca %struct.ScmObj*, align 8
%_37foldl148038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50302, i64 0)
store %struct.ScmObj* %_37foldl148038, %struct.ScmObj** %stackaddr$env-ref61252
%stackaddr$prim61253 = alloca %struct.ScmObj*, align 8
%_95k48378 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60294)
store volatile %struct.ScmObj* %_95k48378, %struct.ScmObj** %stackaddr$prim61253, align 8
%stackaddr$prim61254 = alloca %struct.ScmObj*, align 8
%current_45args60295 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60294)
store volatile %struct.ScmObj* %current_45args60295, %struct.ScmObj** %stackaddr$prim61254, align 8
%stackaddr$prim61255 = alloca %struct.ScmObj*, align 8
%_37_62_6148130 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60295)
store volatile %struct.ScmObj* %_37_62_6148130, %struct.ScmObj** %stackaddr$prim61255, align 8
%ae50326 = call %struct.ScmObj* @const_init_int(i64 1)
%ae50327 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61256 = alloca %struct.ScmObj*, align 8
%_37append48126 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50326, %struct.ScmObj* %ae50327)
store volatile %struct.ScmObj* %_37append48126, %struct.ScmObj** %stackaddr$prim61256, align 8
%stackaddr$makeclosure61257 = alloca %struct.ScmObj*, align 8
%fptrToInt61258 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50328 to i64
%ae50328 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt61258)
store volatile %struct.ScmObj* %ae50328, %struct.ScmObj** %stackaddr$makeclosure61257, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50328, %struct.ScmObj* %_37foldl148038, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50328, %struct.ScmObj* %_37append48126, i64 1)
%ae50329 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61259 = alloca %struct.ScmObj*, align 8
%fptrToInt61260 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50330 to i64
%ae50330 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt61260)
store volatile %struct.ScmObj* %ae50330, %struct.ScmObj** %stackaddr$makeclosure61259, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50330, %struct.ScmObj* %_37append48126, i64 0)
%argslist60662$ae503280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61261 = alloca %struct.ScmObj*, align 8
%argslist60662$ae503281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50330, %struct.ScmObj* %argslist60662$ae503280)
store volatile %struct.ScmObj* %argslist60662$ae503281, %struct.ScmObj** %stackaddr$prim61261, align 8
%stackaddr$prim61262 = alloca %struct.ScmObj*, align 8
%argslist60662$ae503282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50329, %struct.ScmObj* %argslist60662$ae503281)
store volatile %struct.ScmObj* %argslist60662$ae503282, %struct.ScmObj** %stackaddr$prim61262, align 8
%clofunc61263 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50328)
musttail call tailcc void %clofunc61263(%struct.ScmObj* %ae50328, %struct.ScmObj* %argslist60662$ae503282)
ret void
}

define tailcc void @proc_clo$ae50328(%struct.ScmObj* %env$ae50328,%struct.ScmObj* %current_45args60297) {
%stackaddr$env-ref61264 = alloca %struct.ScmObj*, align 8
%_37foldl148038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50328, i64 0)
store %struct.ScmObj* %_37foldl148038, %struct.ScmObj** %stackaddr$env-ref61264
%stackaddr$env-ref61265 = alloca %struct.ScmObj*, align 8
%_37append48126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50328, i64 1)
store %struct.ScmObj* %_37append48126, %struct.ScmObj** %stackaddr$env-ref61265
%stackaddr$prim61266 = alloca %struct.ScmObj*, align 8
%_95k48379 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60297)
store volatile %struct.ScmObj* %_95k48379, %struct.ScmObj** %stackaddr$prim61266, align 8
%stackaddr$prim61267 = alloca %struct.ScmObj*, align 8
%current_45args60298 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60297)
store volatile %struct.ScmObj* %current_45args60298, %struct.ScmObj** %stackaddr$prim61267, align 8
%stackaddr$prim61268 = alloca %struct.ScmObj*, align 8
%anf_45bind48256 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60298)
store volatile %struct.ScmObj* %anf_45bind48256, %struct.ScmObj** %stackaddr$prim61268, align 8
%ae50396 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61269 = alloca %struct.ScmObj*, align 8
%_95048127 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append48126, %struct.ScmObj* %ae50396, %struct.ScmObj* %anf_45bind48256)
store volatile %struct.ScmObj* %_95048127, %struct.ScmObj** %stackaddr$prim61269, align 8
%ae50399 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61270 = alloca %struct.ScmObj*, align 8
%_37append48125 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append48126, %struct.ScmObj* %ae50399)
store volatile %struct.ScmObj* %_37append48125, %struct.ScmObj** %stackaddr$prim61270, align 8
%stackaddr$makeclosure61271 = alloca %struct.ScmObj*, align 8
%fptrToInt61272 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50400 to i64
%ae50400 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt61272)
store volatile %struct.ScmObj* %ae50400, %struct.ScmObj** %stackaddr$makeclosure61271, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50400, %struct.ScmObj* %_37foldl148038, i64 0)
%ae50401 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61273 = alloca %struct.ScmObj*, align 8
%fptrToInt61274 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50402 to i64
%ae50402 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt61274)
store volatile %struct.ScmObj* %ae50402, %struct.ScmObj** %stackaddr$makeclosure61273, align 8
%argslist60651$ae504000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61275 = alloca %struct.ScmObj*, align 8
%argslist60651$ae504001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50402, %struct.ScmObj* %argslist60651$ae504000)
store volatile %struct.ScmObj* %argslist60651$ae504001, %struct.ScmObj** %stackaddr$prim61275, align 8
%stackaddr$prim61276 = alloca %struct.ScmObj*, align 8
%argslist60651$ae504002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50401, %struct.ScmObj* %argslist60651$ae504001)
store volatile %struct.ScmObj* %argslist60651$ae504002, %struct.ScmObj** %stackaddr$prim61276, align 8
%clofunc61277 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50400)
musttail call tailcc void %clofunc61277(%struct.ScmObj* %ae50400, %struct.ScmObj* %argslist60651$ae504002)
ret void
}

define tailcc void @proc_clo$ae50400(%struct.ScmObj* %env$ae50400,%struct.ScmObj* %current_45args60300) {
%stackaddr$env-ref61278 = alloca %struct.ScmObj*, align 8
%_37foldl148038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50400, i64 0)
store %struct.ScmObj* %_37foldl148038, %struct.ScmObj** %stackaddr$env-ref61278
%stackaddr$prim61279 = alloca %struct.ScmObj*, align 8
%_95k48380 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60300)
store volatile %struct.ScmObj* %_95k48380, %struct.ScmObj** %stackaddr$prim61279, align 8
%stackaddr$prim61280 = alloca %struct.ScmObj*, align 8
%current_45args60301 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60300)
store volatile %struct.ScmObj* %current_45args60301, %struct.ScmObj** %stackaddr$prim61280, align 8
%stackaddr$prim61281 = alloca %struct.ScmObj*, align 8
%_37list_6348118 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60301)
store volatile %struct.ScmObj* %_37list_6348118, %struct.ScmObj** %stackaddr$prim61281, align 8
%stackaddr$makeclosure61282 = alloca %struct.ScmObj*, align 8
%fptrToInt61283 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50816 to i64
%ae50816 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt61283)
store volatile %struct.ScmObj* %ae50816, %struct.ScmObj** %stackaddr$makeclosure61282, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50816, %struct.ScmObj* %_37foldl148038, i64 0)
%ae50817 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61284 = alloca %struct.ScmObj*, align 8
%fptrToInt61285 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50818 to i64
%ae50818 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt61285)
store volatile %struct.ScmObj* %ae50818, %struct.ScmObj** %stackaddr$makeclosure61284, align 8
%argslist60626$ae508160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61286 = alloca %struct.ScmObj*, align 8
%argslist60626$ae508161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50818, %struct.ScmObj* %argslist60626$ae508160)
store volatile %struct.ScmObj* %argslist60626$ae508161, %struct.ScmObj** %stackaddr$prim61286, align 8
%stackaddr$prim61287 = alloca %struct.ScmObj*, align 8
%argslist60626$ae508162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50817, %struct.ScmObj* %argslist60626$ae508161)
store volatile %struct.ScmObj* %argslist60626$ae508162, %struct.ScmObj** %stackaddr$prim61287, align 8
%clofunc61288 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50816)
musttail call tailcc void %clofunc61288(%struct.ScmObj* %ae50816, %struct.ScmObj* %argslist60626$ae508162)
ret void
}

define tailcc void @proc_clo$ae50816(%struct.ScmObj* %env$ae50816,%struct.ScmObj* %current_45args60303) {
%stackaddr$env-ref61289 = alloca %struct.ScmObj*, align 8
%_37foldl148038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50816, i64 0)
store %struct.ScmObj* %_37foldl148038, %struct.ScmObj** %stackaddr$env-ref61289
%stackaddr$prim61290 = alloca %struct.ScmObj*, align 8
%_95k48381 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60303)
store volatile %struct.ScmObj* %_95k48381, %struct.ScmObj** %stackaddr$prim61290, align 8
%stackaddr$prim61291 = alloca %struct.ScmObj*, align 8
%current_45args60304 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60303)
store volatile %struct.ScmObj* %current_45args60304, %struct.ScmObj** %stackaddr$prim61291, align 8
%stackaddr$prim61292 = alloca %struct.ScmObj*, align 8
%_37drop48109 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60304)
store volatile %struct.ScmObj* %_37drop48109, %struct.ScmObj** %stackaddr$prim61292, align 8
%stackaddr$makeclosure61293 = alloca %struct.ScmObj*, align 8
%fptrToInt61294 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51352 to i64
%ae51352 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt61294)
store volatile %struct.ScmObj* %ae51352, %struct.ScmObj** %stackaddr$makeclosure61293, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51352, %struct.ScmObj* %_37foldl148038, i64 0)
%ae51353 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61295 = alloca %struct.ScmObj*, align 8
%fptrToInt61296 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51354 to i64
%ae51354 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt61296)
store volatile %struct.ScmObj* %ae51354, %struct.ScmObj** %stackaddr$makeclosure61295, align 8
%argslist60602$ae513520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61297 = alloca %struct.ScmObj*, align 8
%argslist60602$ae513521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51354, %struct.ScmObj* %argslist60602$ae513520)
store volatile %struct.ScmObj* %argslist60602$ae513521, %struct.ScmObj** %stackaddr$prim61297, align 8
%stackaddr$prim61298 = alloca %struct.ScmObj*, align 8
%argslist60602$ae513522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51353, %struct.ScmObj* %argslist60602$ae513521)
store volatile %struct.ScmObj* %argslist60602$ae513522, %struct.ScmObj** %stackaddr$prim61298, align 8
%clofunc61299 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51352)
musttail call tailcc void %clofunc61299(%struct.ScmObj* %ae51352, %struct.ScmObj* %argslist60602$ae513522)
ret void
}

define tailcc void @proc_clo$ae51352(%struct.ScmObj* %env$ae51352,%struct.ScmObj* %current_45args60306) {
%stackaddr$env-ref61300 = alloca %struct.ScmObj*, align 8
%_37foldl148038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51352, i64 0)
store %struct.ScmObj* %_37foldl148038, %struct.ScmObj** %stackaddr$env-ref61300
%stackaddr$prim61301 = alloca %struct.ScmObj*, align 8
%_95k48382 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60306)
store volatile %struct.ScmObj* %_95k48382, %struct.ScmObj** %stackaddr$prim61301, align 8
%stackaddr$prim61302 = alloca %struct.ScmObj*, align 8
%current_45args60307 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60306)
store volatile %struct.ScmObj* %current_45args60307, %struct.ScmObj** %stackaddr$prim61302, align 8
%stackaddr$prim61303 = alloca %struct.ScmObj*, align 8
%_37memv48102 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60307)
store volatile %struct.ScmObj* %_37memv48102, %struct.ScmObj** %stackaddr$prim61303, align 8
%stackaddr$makeclosure61304 = alloca %struct.ScmObj*, align 8
%fptrToInt61305 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51756 to i64
%ae51756 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt61305)
store volatile %struct.ScmObj* %ae51756, %struct.ScmObj** %stackaddr$makeclosure61304, align 8
%ae51757 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61306 = alloca %struct.ScmObj*, align 8
%fptrToInt61307 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51758 to i64
%ae51758 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt61307)
store volatile %struct.ScmObj* %ae51758, %struct.ScmObj** %stackaddr$makeclosure61306, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51758, %struct.ScmObj* %_37foldl148038, i64 0)
%argslist60576$ae517560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61308 = alloca %struct.ScmObj*, align 8
%argslist60576$ae517561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51758, %struct.ScmObj* %argslist60576$ae517560)
store volatile %struct.ScmObj* %argslist60576$ae517561, %struct.ScmObj** %stackaddr$prim61308, align 8
%stackaddr$prim61309 = alloca %struct.ScmObj*, align 8
%argslist60576$ae517562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51757, %struct.ScmObj* %argslist60576$ae517561)
store volatile %struct.ScmObj* %argslist60576$ae517562, %struct.ScmObj** %stackaddr$prim61309, align 8
%clofunc61310 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51756)
musttail call tailcc void %clofunc61310(%struct.ScmObj* %ae51756, %struct.ScmObj* %argslist60576$ae517562)
ret void
}

define tailcc void @proc_clo$ae51756(%struct.ScmObj* %env$ae51756,%struct.ScmObj* %current_45args60309) {
%stackaddr$prim61311 = alloca %struct.ScmObj*, align 8
%_95k48383 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60309)
store volatile %struct.ScmObj* %_95k48383, %struct.ScmObj** %stackaddr$prim61311, align 8
%stackaddr$prim61312 = alloca %struct.ScmObj*, align 8
%current_45args60310 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60309)
store volatile %struct.ScmObj* %current_45args60310, %struct.ScmObj** %stackaddr$prim61312, align 8
%stackaddr$prim61313 = alloca %struct.ScmObj*, align 8
%_37_4748098 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60310)
store volatile %struct.ScmObj* %_37_4748098, %struct.ScmObj** %stackaddr$prim61313, align 8
%stackaddr$makeclosure61314 = alloca %struct.ScmObj*, align 8
%fptrToInt61315 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51854 to i64
%ae51854 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt61315)
store volatile %struct.ScmObj* %ae51854, %struct.ScmObj** %stackaddr$makeclosure61314, align 8
%ae51855 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61316 = alloca %struct.ScmObj*, align 8
%fptrToInt61317 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51856 to i64
%ae51856 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt61317)
store volatile %struct.ScmObj* %ae51856, %struct.ScmObj** %stackaddr$makeclosure61316, align 8
%argslist60563$ae518540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61318 = alloca %struct.ScmObj*, align 8
%argslist60563$ae518541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51856, %struct.ScmObj* %argslist60563$ae518540)
store volatile %struct.ScmObj* %argslist60563$ae518541, %struct.ScmObj** %stackaddr$prim61318, align 8
%stackaddr$prim61319 = alloca %struct.ScmObj*, align 8
%argslist60563$ae518542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51855, %struct.ScmObj* %argslist60563$ae518541)
store volatile %struct.ScmObj* %argslist60563$ae518542, %struct.ScmObj** %stackaddr$prim61319, align 8
%clofunc61320 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51854)
musttail call tailcc void %clofunc61320(%struct.ScmObj* %ae51854, %struct.ScmObj* %argslist60563$ae518542)
ret void
}

define tailcc void @proc_clo$ae51854(%struct.ScmObj* %env$ae51854,%struct.ScmObj* %current_45args60312) {
%stackaddr$prim61321 = alloca %struct.ScmObj*, align 8
%_95k48384 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60312)
store volatile %struct.ScmObj* %_95k48384, %struct.ScmObj** %stackaddr$prim61321, align 8
%stackaddr$prim61322 = alloca %struct.ScmObj*, align 8
%current_45args60313 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60312)
store volatile %struct.ScmObj* %current_45args60313, %struct.ScmObj** %stackaddr$prim61322, align 8
%stackaddr$prim61323 = alloca %struct.ScmObj*, align 8
%_37first48096 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60313)
store volatile %struct.ScmObj* %_37first48096, %struct.ScmObj** %stackaddr$prim61323, align 8
%stackaddr$makeclosure61324 = alloca %struct.ScmObj*, align 8
%fptrToInt61325 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51874 to i64
%ae51874 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt61325)
store volatile %struct.ScmObj* %ae51874, %struct.ScmObj** %stackaddr$makeclosure61324, align 8
%ae51875 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61326 = alloca %struct.ScmObj*, align 8
%fptrToInt61327 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51876 to i64
%ae51876 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt61327)
store volatile %struct.ScmObj* %ae51876, %struct.ScmObj** %stackaddr$makeclosure61326, align 8
%argslist60558$ae518740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61328 = alloca %struct.ScmObj*, align 8
%argslist60558$ae518741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51876, %struct.ScmObj* %argslist60558$ae518740)
store volatile %struct.ScmObj* %argslist60558$ae518741, %struct.ScmObj** %stackaddr$prim61328, align 8
%stackaddr$prim61329 = alloca %struct.ScmObj*, align 8
%argslist60558$ae518742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51875, %struct.ScmObj* %argslist60558$ae518741)
store volatile %struct.ScmObj* %argslist60558$ae518742, %struct.ScmObj** %stackaddr$prim61329, align 8
%clofunc61330 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51874)
musttail call tailcc void %clofunc61330(%struct.ScmObj* %ae51874, %struct.ScmObj* %argslist60558$ae518742)
ret void
}

define tailcc void @proc_clo$ae51874(%struct.ScmObj* %env$ae51874,%struct.ScmObj* %current_45args60315) {
%stackaddr$prim61331 = alloca %struct.ScmObj*, align 8
%_95k48385 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60315)
store volatile %struct.ScmObj* %_95k48385, %struct.ScmObj** %stackaddr$prim61331, align 8
%stackaddr$prim61332 = alloca %struct.ScmObj*, align 8
%current_45args60316 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60315)
store volatile %struct.ScmObj* %current_45args60316, %struct.ScmObj** %stackaddr$prim61332, align 8
%stackaddr$prim61333 = alloca %struct.ScmObj*, align 8
%_37second48094 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60316)
store volatile %struct.ScmObj* %_37second48094, %struct.ScmObj** %stackaddr$prim61333, align 8
%stackaddr$makeclosure61334 = alloca %struct.ScmObj*, align 8
%fptrToInt61335 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51896 to i64
%ae51896 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt61335)
store volatile %struct.ScmObj* %ae51896, %struct.ScmObj** %stackaddr$makeclosure61334, align 8
%ae51897 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61336 = alloca %struct.ScmObj*, align 8
%fptrToInt61337 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51898 to i64
%ae51898 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt61337)
store volatile %struct.ScmObj* %ae51898, %struct.ScmObj** %stackaddr$makeclosure61336, align 8
%argslist60553$ae518960 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61338 = alloca %struct.ScmObj*, align 8
%argslist60553$ae518961 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51898, %struct.ScmObj* %argslist60553$ae518960)
store volatile %struct.ScmObj* %argslist60553$ae518961, %struct.ScmObj** %stackaddr$prim61338, align 8
%stackaddr$prim61339 = alloca %struct.ScmObj*, align 8
%argslist60553$ae518962 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51897, %struct.ScmObj* %argslist60553$ae518961)
store volatile %struct.ScmObj* %argslist60553$ae518962, %struct.ScmObj** %stackaddr$prim61339, align 8
%clofunc61340 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51896)
musttail call tailcc void %clofunc61340(%struct.ScmObj* %ae51896, %struct.ScmObj* %argslist60553$ae518962)
ret void
}

define tailcc void @proc_clo$ae51896(%struct.ScmObj* %env$ae51896,%struct.ScmObj* %current_45args60318) {
%stackaddr$prim61341 = alloca %struct.ScmObj*, align 8
%_95k48386 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60318)
store volatile %struct.ScmObj* %_95k48386, %struct.ScmObj** %stackaddr$prim61341, align 8
%stackaddr$prim61342 = alloca %struct.ScmObj*, align 8
%current_45args60319 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60318)
store volatile %struct.ScmObj* %current_45args60319, %struct.ScmObj** %stackaddr$prim61342, align 8
%stackaddr$prim61343 = alloca %struct.ScmObj*, align 8
%_37third48092 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60319)
store volatile %struct.ScmObj* %_37third48092, %struct.ScmObj** %stackaddr$prim61343, align 8
%stackaddr$makeclosure61344 = alloca %struct.ScmObj*, align 8
%fptrToInt61345 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51920 to i64
%ae51920 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt61345)
store volatile %struct.ScmObj* %ae51920, %struct.ScmObj** %stackaddr$makeclosure61344, align 8
%ae51921 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61346 = alloca %struct.ScmObj*, align 8
%fptrToInt61347 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51922 to i64
%ae51922 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt61347)
store volatile %struct.ScmObj* %ae51922, %struct.ScmObj** %stackaddr$makeclosure61346, align 8
%argslist60548$ae519200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61348 = alloca %struct.ScmObj*, align 8
%argslist60548$ae519201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51922, %struct.ScmObj* %argslist60548$ae519200)
store volatile %struct.ScmObj* %argslist60548$ae519201, %struct.ScmObj** %stackaddr$prim61348, align 8
%stackaddr$prim61349 = alloca %struct.ScmObj*, align 8
%argslist60548$ae519202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51921, %struct.ScmObj* %argslist60548$ae519201)
store volatile %struct.ScmObj* %argslist60548$ae519202, %struct.ScmObj** %stackaddr$prim61349, align 8
%clofunc61350 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51920)
musttail call tailcc void %clofunc61350(%struct.ScmObj* %ae51920, %struct.ScmObj* %argslist60548$ae519202)
ret void
}

define tailcc void @proc_clo$ae51920(%struct.ScmObj* %env$ae51920,%struct.ScmObj* %current_45args60321) {
%stackaddr$prim61351 = alloca %struct.ScmObj*, align 8
%_95k48387 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60321)
store volatile %struct.ScmObj* %_95k48387, %struct.ScmObj** %stackaddr$prim61351, align 8
%stackaddr$prim61352 = alloca %struct.ScmObj*, align 8
%current_45args60322 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60321)
store volatile %struct.ScmObj* %current_45args60322, %struct.ScmObj** %stackaddr$prim61352, align 8
%stackaddr$prim61353 = alloca %struct.ScmObj*, align 8
%_37fourth48090 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60322)
store volatile %struct.ScmObj* %_37fourth48090, %struct.ScmObj** %stackaddr$prim61353, align 8
%stackaddr$prim61354 = alloca %struct.ScmObj*, align 8
%anf_45bind48292 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind48292, %struct.ScmObj** %stackaddr$prim61354, align 8
%ae51946 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim61355 = alloca %struct.ScmObj*, align 8
%nat_45_62peano48157 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51946, %struct.ScmObj* %anf_45bind48292)
store volatile %struct.ScmObj* %nat_45_62peano48157, %struct.ScmObj** %stackaddr$prim61355, align 8
%stackaddr$prim61356 = alloca %struct.ScmObj*, align 8
%anf_45bind48293 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind48293, %struct.ScmObj** %stackaddr$prim61356, align 8
%ae51948 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim61357 = alloca %struct.ScmObj*, align 8
%peano_45_62nat48156 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51948, %struct.ScmObj* %anf_45bind48293)
store volatile %struct.ScmObj* %peano_45_62nat48156, %struct.ScmObj** %stackaddr$prim61357, align 8
%stackaddr$prim61358 = alloca %struct.ScmObj*, align 8
%anf_45bind48294 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind48294, %struct.ScmObj** %stackaddr$prim61358, align 8
%ae51950 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim61359 = alloca %struct.ScmObj*, align 8
%succ48155 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51950, %struct.ScmObj* %anf_45bind48294)
store volatile %struct.ScmObj* %succ48155, %struct.ScmObj** %stackaddr$prim61359, align 8
%stackaddr$prim61360 = alloca %struct.ScmObj*, align 8
%anf_45bind48295 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind48295, %struct.ScmObj** %stackaddr$prim61360, align 8
%ae51952 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim61361 = alloca %struct.ScmObj*, align 8
%pred48154 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51952, %struct.ScmObj* %anf_45bind48295)
store volatile %struct.ScmObj* %pred48154, %struct.ScmObj** %stackaddr$prim61361, align 8
%stackaddr$prim61362 = alloca %struct.ScmObj*, align 8
%anf_45bind48296 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind48296, %struct.ScmObj** %stackaddr$prim61362, align 8
%ae51954 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim61363 = alloca %struct.ScmObj*, align 8
%z_6348153 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51954, %struct.ScmObj* %anf_45bind48296)
store volatile %struct.ScmObj* %z_6348153, %struct.ScmObj** %stackaddr$prim61363, align 8
%stackaddr$prim61364 = alloca %struct.ScmObj*, align 8
%anf_45bind48297 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind48297, %struct.ScmObj** %stackaddr$prim61364, align 8
%ae51956 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim61365 = alloca %struct.ScmObj*, align 8
%addc48152 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51956, %struct.ScmObj* %anf_45bind48297)
store volatile %struct.ScmObj* %addc48152, %struct.ScmObj** %stackaddr$prim61365, align 8
%stackaddr$prim61366 = alloca %struct.ScmObj*, align 8
%anf_45bind48298 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind48298, %struct.ScmObj** %stackaddr$prim61366, align 8
%ae51958 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim61367 = alloca %struct.ScmObj*, align 8
%fibc48151 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51958, %struct.ScmObj* %anf_45bind48298)
store volatile %struct.ScmObj* %fibc48151, %struct.ScmObj** %stackaddr$prim61367, align 8
%stackaddr$makeclosure61368 = alloca %struct.ScmObj*, align 8
%fptrToInt61369 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51960 to i64
%ae51960 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt61369)
store volatile %struct.ScmObj* %ae51960, %struct.ScmObj** %stackaddr$makeclosure61368, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51960, %struct.ScmObj* %z_6348153, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51960, %struct.ScmObj* %addc48152, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51960, %struct.ScmObj* %fibc48151, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51960, %struct.ScmObj* %nat_45_62peano48157, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51960, %struct.ScmObj* %peano_45_62nat48156, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51960, %struct.ScmObj* %succ48155, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae51960, %struct.ScmObj* %pred48154, i64 6)
%ae51961 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61370 = alloca %struct.ScmObj*, align 8
%fptrToInt61371 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51962 to i64
%ae51962 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt61371)
store volatile %struct.ScmObj* %ae51962, %struct.ScmObj** %stackaddr$makeclosure61370, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51962, %struct.ScmObj* %nat_45_62peano48157, i64 0)
%argslist60543$ae519600 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61372 = alloca %struct.ScmObj*, align 8
%argslist60543$ae519601 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51962, %struct.ScmObj* %argslist60543$ae519600)
store volatile %struct.ScmObj* %argslist60543$ae519601, %struct.ScmObj** %stackaddr$prim61372, align 8
%stackaddr$prim61373 = alloca %struct.ScmObj*, align 8
%argslist60543$ae519602 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51961, %struct.ScmObj* %argslist60543$ae519601)
store volatile %struct.ScmObj* %argslist60543$ae519602, %struct.ScmObj** %stackaddr$prim61373, align 8
%clofunc61374 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51960)
musttail call tailcc void %clofunc61374(%struct.ScmObj* %ae51960, %struct.ScmObj* %argslist60543$ae519602)
ret void
}

define tailcc void @proc_clo$ae51960(%struct.ScmObj* %env$ae51960,%struct.ScmObj* %current_45args60324) {
%stackaddr$env-ref61375 = alloca %struct.ScmObj*, align 8
%z_6348153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51960, i64 0)
store %struct.ScmObj* %z_6348153, %struct.ScmObj** %stackaddr$env-ref61375
%stackaddr$env-ref61376 = alloca %struct.ScmObj*, align 8
%addc48152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51960, i64 1)
store %struct.ScmObj* %addc48152, %struct.ScmObj** %stackaddr$env-ref61376
%stackaddr$env-ref61377 = alloca %struct.ScmObj*, align 8
%fibc48151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51960, i64 2)
store %struct.ScmObj* %fibc48151, %struct.ScmObj** %stackaddr$env-ref61377
%stackaddr$env-ref61378 = alloca %struct.ScmObj*, align 8
%nat_45_62peano48157 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51960, i64 3)
store %struct.ScmObj* %nat_45_62peano48157, %struct.ScmObj** %stackaddr$env-ref61378
%stackaddr$env-ref61379 = alloca %struct.ScmObj*, align 8
%peano_45_62nat48156 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51960, i64 4)
store %struct.ScmObj* %peano_45_62nat48156, %struct.ScmObj** %stackaddr$env-ref61379
%stackaddr$env-ref61380 = alloca %struct.ScmObj*, align 8
%succ48155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51960, i64 5)
store %struct.ScmObj* %succ48155, %struct.ScmObj** %stackaddr$env-ref61380
%stackaddr$env-ref61381 = alloca %struct.ScmObj*, align 8
%pred48154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51960, i64 6)
store %struct.ScmObj* %pred48154, %struct.ScmObj** %stackaddr$env-ref61381
%stackaddr$prim61382 = alloca %struct.ScmObj*, align 8
%_95k48388 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60324)
store volatile %struct.ScmObj* %_95k48388, %struct.ScmObj** %stackaddr$prim61382, align 8
%stackaddr$prim61383 = alloca %struct.ScmObj*, align 8
%current_45args60325 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60324)
store volatile %struct.ScmObj* %current_45args60325, %struct.ScmObj** %stackaddr$prim61383, align 8
%stackaddr$prim61384 = alloca %struct.ScmObj*, align 8
%anf_45bind48306 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60325)
store volatile %struct.ScmObj* %anf_45bind48306, %struct.ScmObj** %stackaddr$prim61384, align 8
%ae52169 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61385 = alloca %struct.ScmObj*, align 8
%t4803248176 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %nat_45_62peano48157, %struct.ScmObj* %ae52169, %struct.ScmObj* %anf_45bind48306)
store volatile %struct.ScmObj* %t4803248176, %struct.ScmObj** %stackaddr$prim61385, align 8
%stackaddr$makeclosure61386 = alloca %struct.ScmObj*, align 8
%fptrToInt61387 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52171 to i64
%ae52171 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt61387)
store volatile %struct.ScmObj* %ae52171, %struct.ScmObj** %stackaddr$makeclosure61386, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52171, %struct.ScmObj* %z_6348153, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52171, %struct.ScmObj* %addc48152, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52171, %struct.ScmObj* %fibc48151, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52171, %struct.ScmObj* %nat_45_62peano48157, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52171, %struct.ScmObj* %peano_45_62nat48156, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae52171, %struct.ScmObj* %succ48155, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae52171, %struct.ScmObj* %pred48154, i64 6)
%ae52172 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61388 = alloca %struct.ScmObj*, align 8
%fptrToInt61389 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52173 to i64
%ae52173 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt61389)
store volatile %struct.ScmObj* %ae52173, %struct.ScmObj** %stackaddr$makeclosure61388, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52173, %struct.ScmObj* %z_6348153, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52173, %struct.ScmObj* %peano_45_62nat48156, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52173, %struct.ScmObj* %pred48154, i64 2)
%argslist60519$ae521710 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61390 = alloca %struct.ScmObj*, align 8
%argslist60519$ae521711 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52173, %struct.ScmObj* %argslist60519$ae521710)
store volatile %struct.ScmObj* %argslist60519$ae521711, %struct.ScmObj** %stackaddr$prim61390, align 8
%stackaddr$prim61391 = alloca %struct.ScmObj*, align 8
%argslist60519$ae521712 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52172, %struct.ScmObj* %argslist60519$ae521711)
store volatile %struct.ScmObj* %argslist60519$ae521712, %struct.ScmObj** %stackaddr$prim61391, align 8
%clofunc61392 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52171)
musttail call tailcc void %clofunc61392(%struct.ScmObj* %ae52171, %struct.ScmObj* %argslist60519$ae521712)
ret void
}

define tailcc void @proc_clo$ae52171(%struct.ScmObj* %env$ae52171,%struct.ScmObj* %current_45args60327) {
%stackaddr$env-ref61393 = alloca %struct.ScmObj*, align 8
%z_6348153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52171, i64 0)
store %struct.ScmObj* %z_6348153, %struct.ScmObj** %stackaddr$env-ref61393
%stackaddr$env-ref61394 = alloca %struct.ScmObj*, align 8
%addc48152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52171, i64 1)
store %struct.ScmObj* %addc48152, %struct.ScmObj** %stackaddr$env-ref61394
%stackaddr$env-ref61395 = alloca %struct.ScmObj*, align 8
%fibc48151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52171, i64 2)
store %struct.ScmObj* %fibc48151, %struct.ScmObj** %stackaddr$env-ref61395
%stackaddr$env-ref61396 = alloca %struct.ScmObj*, align 8
%nat_45_62peano48157 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52171, i64 3)
store %struct.ScmObj* %nat_45_62peano48157, %struct.ScmObj** %stackaddr$env-ref61396
%stackaddr$env-ref61397 = alloca %struct.ScmObj*, align 8
%peano_45_62nat48156 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52171, i64 4)
store %struct.ScmObj* %peano_45_62nat48156, %struct.ScmObj** %stackaddr$env-ref61397
%stackaddr$env-ref61398 = alloca %struct.ScmObj*, align 8
%succ48155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52171, i64 5)
store %struct.ScmObj* %succ48155, %struct.ScmObj** %stackaddr$env-ref61398
%stackaddr$env-ref61399 = alloca %struct.ScmObj*, align 8
%pred48154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52171, i64 6)
store %struct.ScmObj* %pred48154, %struct.ScmObj** %stackaddr$env-ref61399
%stackaddr$prim61400 = alloca %struct.ScmObj*, align 8
%_95k48389 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60327)
store volatile %struct.ScmObj* %_95k48389, %struct.ScmObj** %stackaddr$prim61400, align 8
%stackaddr$prim61401 = alloca %struct.ScmObj*, align 8
%current_45args60328 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60327)
store volatile %struct.ScmObj* %current_45args60328, %struct.ScmObj** %stackaddr$prim61401, align 8
%stackaddr$prim61402 = alloca %struct.ScmObj*, align 8
%anf_45bind48313 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60328)
store volatile %struct.ScmObj* %anf_45bind48313, %struct.ScmObj** %stackaddr$prim61402, align 8
%ae52304 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61403 = alloca %struct.ScmObj*, align 8
%t4803148174 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %peano_45_62nat48156, %struct.ScmObj* %ae52304, %struct.ScmObj* %anf_45bind48313)
store volatile %struct.ScmObj* %t4803148174, %struct.ScmObj** %stackaddr$prim61403, align 8
%stackaddr$makeclosure61404 = alloca %struct.ScmObj*, align 8
%fptrToInt61405 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52306 to i64
%ae52306 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt61405)
store volatile %struct.ScmObj* %ae52306, %struct.ScmObj** %stackaddr$makeclosure61404, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52306, %struct.ScmObj* %z_6348153, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52306, %struct.ScmObj* %addc48152, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52306, %struct.ScmObj* %fibc48151, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52306, %struct.ScmObj* %nat_45_62peano48157, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52306, %struct.ScmObj* %peano_45_62nat48156, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae52306, %struct.ScmObj* %succ48155, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae52306, %struct.ScmObj* %pred48154, i64 6)
%ae52307 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61406 = alloca %struct.ScmObj*, align 8
%fptrToInt61407 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52308 to i64
%ae52308 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt61407)
store volatile %struct.ScmObj* %ae52308, %struct.ScmObj** %stackaddr$makeclosure61406, align 8
%argslist60501$ae523060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61408 = alloca %struct.ScmObj*, align 8
%argslist60501$ae523061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52308, %struct.ScmObj* %argslist60501$ae523060)
store volatile %struct.ScmObj* %argslist60501$ae523061, %struct.ScmObj** %stackaddr$prim61408, align 8
%stackaddr$prim61409 = alloca %struct.ScmObj*, align 8
%argslist60501$ae523062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52307, %struct.ScmObj* %argslist60501$ae523061)
store volatile %struct.ScmObj* %argslist60501$ae523062, %struct.ScmObj** %stackaddr$prim61409, align 8
%clofunc61410 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52306)
musttail call tailcc void %clofunc61410(%struct.ScmObj* %ae52306, %struct.ScmObj* %argslist60501$ae523062)
ret void
}

define tailcc void @proc_clo$ae52306(%struct.ScmObj* %env$ae52306,%struct.ScmObj* %current_45args60330) {
%stackaddr$env-ref61411 = alloca %struct.ScmObj*, align 8
%z_6348153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52306, i64 0)
store %struct.ScmObj* %z_6348153, %struct.ScmObj** %stackaddr$env-ref61411
%stackaddr$env-ref61412 = alloca %struct.ScmObj*, align 8
%addc48152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52306, i64 1)
store %struct.ScmObj* %addc48152, %struct.ScmObj** %stackaddr$env-ref61412
%stackaddr$env-ref61413 = alloca %struct.ScmObj*, align 8
%fibc48151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52306, i64 2)
store %struct.ScmObj* %fibc48151, %struct.ScmObj** %stackaddr$env-ref61413
%stackaddr$env-ref61414 = alloca %struct.ScmObj*, align 8
%nat_45_62peano48157 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52306, i64 3)
store %struct.ScmObj* %nat_45_62peano48157, %struct.ScmObj** %stackaddr$env-ref61414
%stackaddr$env-ref61415 = alloca %struct.ScmObj*, align 8
%peano_45_62nat48156 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52306, i64 4)
store %struct.ScmObj* %peano_45_62nat48156, %struct.ScmObj** %stackaddr$env-ref61415
%stackaddr$env-ref61416 = alloca %struct.ScmObj*, align 8
%succ48155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52306, i64 5)
store %struct.ScmObj* %succ48155, %struct.ScmObj** %stackaddr$env-ref61416
%stackaddr$env-ref61417 = alloca %struct.ScmObj*, align 8
%pred48154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52306, i64 6)
store %struct.ScmObj* %pred48154, %struct.ScmObj** %stackaddr$env-ref61417
%stackaddr$prim61418 = alloca %struct.ScmObj*, align 8
%_95k48390 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60330)
store volatile %struct.ScmObj* %_95k48390, %struct.ScmObj** %stackaddr$prim61418, align 8
%stackaddr$prim61419 = alloca %struct.ScmObj*, align 8
%current_45args60331 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60330)
store volatile %struct.ScmObj* %current_45args60331, %struct.ScmObj** %stackaddr$prim61419, align 8
%stackaddr$prim61420 = alloca %struct.ScmObj*, align 8
%anf_45bind48316 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60331)
store volatile %struct.ScmObj* %anf_45bind48316, %struct.ScmObj** %stackaddr$prim61420, align 8
%ae52373 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61421 = alloca %struct.ScmObj*, align 8
%t4803048171 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %succ48155, %struct.ScmObj* %ae52373, %struct.ScmObj* %anf_45bind48316)
store volatile %struct.ScmObj* %t4803048171, %struct.ScmObj** %stackaddr$prim61421, align 8
%stackaddr$makeclosure61422 = alloca %struct.ScmObj*, align 8
%fptrToInt61423 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52375 to i64
%ae52375 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt61423)
store volatile %struct.ScmObj* %ae52375, %struct.ScmObj** %stackaddr$makeclosure61422, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52375, %struct.ScmObj* %z_6348153, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52375, %struct.ScmObj* %addc48152, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52375, %struct.ScmObj* %fibc48151, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52375, %struct.ScmObj* %nat_45_62peano48157, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52375, %struct.ScmObj* %peano_45_62nat48156, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae52375, %struct.ScmObj* %succ48155, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae52375, %struct.ScmObj* %pred48154, i64 6)
%ae52376 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61424 = alloca %struct.ScmObj*, align 8
%fptrToInt61425 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52377 to i64
%ae52377 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt61425)
store volatile %struct.ScmObj* %ae52377, %struct.ScmObj** %stackaddr$makeclosure61424, align 8
%argslist60487$ae523750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61426 = alloca %struct.ScmObj*, align 8
%argslist60487$ae523751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52377, %struct.ScmObj* %argslist60487$ae523750)
store volatile %struct.ScmObj* %argslist60487$ae523751, %struct.ScmObj** %stackaddr$prim61426, align 8
%stackaddr$prim61427 = alloca %struct.ScmObj*, align 8
%argslist60487$ae523752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52376, %struct.ScmObj* %argslist60487$ae523751)
store volatile %struct.ScmObj* %argslist60487$ae523752, %struct.ScmObj** %stackaddr$prim61427, align 8
%clofunc61428 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52375)
musttail call tailcc void %clofunc61428(%struct.ScmObj* %ae52375, %struct.ScmObj* %argslist60487$ae523752)
ret void
}

define tailcc void @proc_clo$ae52375(%struct.ScmObj* %env$ae52375,%struct.ScmObj* %current_45args60333) {
%stackaddr$env-ref61429 = alloca %struct.ScmObj*, align 8
%z_6348153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52375, i64 0)
store %struct.ScmObj* %z_6348153, %struct.ScmObj** %stackaddr$env-ref61429
%stackaddr$env-ref61430 = alloca %struct.ScmObj*, align 8
%addc48152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52375, i64 1)
store %struct.ScmObj* %addc48152, %struct.ScmObj** %stackaddr$env-ref61430
%stackaddr$env-ref61431 = alloca %struct.ScmObj*, align 8
%fibc48151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52375, i64 2)
store %struct.ScmObj* %fibc48151, %struct.ScmObj** %stackaddr$env-ref61431
%stackaddr$env-ref61432 = alloca %struct.ScmObj*, align 8
%nat_45_62peano48157 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52375, i64 3)
store %struct.ScmObj* %nat_45_62peano48157, %struct.ScmObj** %stackaddr$env-ref61432
%stackaddr$env-ref61433 = alloca %struct.ScmObj*, align 8
%peano_45_62nat48156 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52375, i64 4)
store %struct.ScmObj* %peano_45_62nat48156, %struct.ScmObj** %stackaddr$env-ref61433
%stackaddr$env-ref61434 = alloca %struct.ScmObj*, align 8
%succ48155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52375, i64 5)
store %struct.ScmObj* %succ48155, %struct.ScmObj** %stackaddr$env-ref61434
%stackaddr$env-ref61435 = alloca %struct.ScmObj*, align 8
%pred48154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52375, i64 6)
store %struct.ScmObj* %pred48154, %struct.ScmObj** %stackaddr$env-ref61435
%stackaddr$prim61436 = alloca %struct.ScmObj*, align 8
%_95k48391 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60333)
store volatile %struct.ScmObj* %_95k48391, %struct.ScmObj** %stackaddr$prim61436, align 8
%stackaddr$prim61437 = alloca %struct.ScmObj*, align 8
%current_45args60334 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60333)
store volatile %struct.ScmObj* %current_45args60334, %struct.ScmObj** %stackaddr$prim61437, align 8
%stackaddr$prim61438 = alloca %struct.ScmObj*, align 8
%anf_45bind48317 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60334)
store volatile %struct.ScmObj* %anf_45bind48317, %struct.ScmObj** %stackaddr$prim61438, align 8
%ae52396 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61439 = alloca %struct.ScmObj*, align 8
%t4802848169 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %pred48154, %struct.ScmObj* %ae52396, %struct.ScmObj* %anf_45bind48317)
store volatile %struct.ScmObj* %t4802848169, %struct.ScmObj** %stackaddr$prim61439, align 8
%stackaddr$makeclosure61440 = alloca %struct.ScmObj*, align 8
%fptrToInt61441 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52398 to i64
%ae52398 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt61441)
store volatile %struct.ScmObj* %ae52398, %struct.ScmObj** %stackaddr$makeclosure61440, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52398, %struct.ScmObj* %z_6348153, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52398, %struct.ScmObj* %addc48152, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52398, %struct.ScmObj* %fibc48151, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52398, %struct.ScmObj* %nat_45_62peano48157, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52398, %struct.ScmObj* %peano_45_62nat48156, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae52398, %struct.ScmObj* %succ48155, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae52398, %struct.ScmObj* %pred48154, i64 6)
%ae52399 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61442 = alloca %struct.ScmObj*, align 8
%fptrToInt61443 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52400 to i64
%ae52400 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt61443)
store volatile %struct.ScmObj* %ae52400, %struct.ScmObj** %stackaddr$makeclosure61442, align 8
%argslist60482$ae523980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61444 = alloca %struct.ScmObj*, align 8
%argslist60482$ae523981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52400, %struct.ScmObj* %argslist60482$ae523980)
store volatile %struct.ScmObj* %argslist60482$ae523981, %struct.ScmObj** %stackaddr$prim61444, align 8
%stackaddr$prim61445 = alloca %struct.ScmObj*, align 8
%argslist60482$ae523982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52399, %struct.ScmObj* %argslist60482$ae523981)
store volatile %struct.ScmObj* %argslist60482$ae523982, %struct.ScmObj** %stackaddr$prim61445, align 8
%clofunc61446 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52398)
musttail call tailcc void %clofunc61446(%struct.ScmObj* %ae52398, %struct.ScmObj* %argslist60482$ae523982)
ret void
}

define tailcc void @proc_clo$ae52398(%struct.ScmObj* %env$ae52398,%struct.ScmObj* %current_45args60336) {
%stackaddr$env-ref61447 = alloca %struct.ScmObj*, align 8
%z_6348153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52398, i64 0)
store %struct.ScmObj* %z_6348153, %struct.ScmObj** %stackaddr$env-ref61447
%stackaddr$env-ref61448 = alloca %struct.ScmObj*, align 8
%addc48152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52398, i64 1)
store %struct.ScmObj* %addc48152, %struct.ScmObj** %stackaddr$env-ref61448
%stackaddr$env-ref61449 = alloca %struct.ScmObj*, align 8
%fibc48151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52398, i64 2)
store %struct.ScmObj* %fibc48151, %struct.ScmObj** %stackaddr$env-ref61449
%stackaddr$env-ref61450 = alloca %struct.ScmObj*, align 8
%nat_45_62peano48157 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52398, i64 3)
store %struct.ScmObj* %nat_45_62peano48157, %struct.ScmObj** %stackaddr$env-ref61450
%stackaddr$env-ref61451 = alloca %struct.ScmObj*, align 8
%peano_45_62nat48156 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52398, i64 4)
store %struct.ScmObj* %peano_45_62nat48156, %struct.ScmObj** %stackaddr$env-ref61451
%stackaddr$env-ref61452 = alloca %struct.ScmObj*, align 8
%succ48155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52398, i64 5)
store %struct.ScmObj* %succ48155, %struct.ScmObj** %stackaddr$env-ref61452
%stackaddr$env-ref61453 = alloca %struct.ScmObj*, align 8
%pred48154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52398, i64 6)
store %struct.ScmObj* %pred48154, %struct.ScmObj** %stackaddr$env-ref61453
%stackaddr$prim61454 = alloca %struct.ScmObj*, align 8
%_95k48392 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60336)
store volatile %struct.ScmObj* %_95k48392, %struct.ScmObj** %stackaddr$prim61454, align 8
%stackaddr$prim61455 = alloca %struct.ScmObj*, align 8
%current_45args60337 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60336)
store volatile %struct.ScmObj* %current_45args60337, %struct.ScmObj** %stackaddr$prim61455, align 8
%stackaddr$prim61456 = alloca %struct.ScmObj*, align 8
%anf_45bind48318 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60337)
store volatile %struct.ScmObj* %anf_45bind48318, %struct.ScmObj** %stackaddr$prim61456, align 8
%ae52419 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61457 = alloca %struct.ScmObj*, align 8
%t4802748167 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %z_6348153, %struct.ScmObj* %ae52419, %struct.ScmObj* %anf_45bind48318)
store volatile %struct.ScmObj* %t4802748167, %struct.ScmObj** %stackaddr$prim61457, align 8
%stackaddr$makeclosure61458 = alloca %struct.ScmObj*, align 8
%fptrToInt61459 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52421 to i64
%ae52421 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt61459)
store volatile %struct.ScmObj* %ae52421, %struct.ScmObj** %stackaddr$makeclosure61458, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52421, %struct.ScmObj* %z_6348153, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52421, %struct.ScmObj* %addc48152, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52421, %struct.ScmObj* %fibc48151, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52421, %struct.ScmObj* %nat_45_62peano48157, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52421, %struct.ScmObj* %peano_45_62nat48156, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae52421, %struct.ScmObj* %pred48154, i64 5)
%ae52422 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61460 = alloca %struct.ScmObj*, align 8
%fptrToInt61461 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52423 to i64
%ae52423 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt61461)
store volatile %struct.ScmObj* %ae52423, %struct.ScmObj** %stackaddr$makeclosure61460, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52423, %struct.ScmObj* %z_6348153, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52423, %struct.ScmObj* %addc48152, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52423, %struct.ScmObj* %succ48155, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52423, %struct.ScmObj* %pred48154, i64 3)
%argslist60477$ae524210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61462 = alloca %struct.ScmObj*, align 8
%argslist60477$ae524211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52423, %struct.ScmObj* %argslist60477$ae524210)
store volatile %struct.ScmObj* %argslist60477$ae524211, %struct.ScmObj** %stackaddr$prim61462, align 8
%stackaddr$prim61463 = alloca %struct.ScmObj*, align 8
%argslist60477$ae524212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52422, %struct.ScmObj* %argslist60477$ae524211)
store volatile %struct.ScmObj* %argslist60477$ae524212, %struct.ScmObj** %stackaddr$prim61463, align 8
%clofunc61464 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52421)
musttail call tailcc void %clofunc61464(%struct.ScmObj* %ae52421, %struct.ScmObj* %argslist60477$ae524212)
ret void
}

define tailcc void @proc_clo$ae52421(%struct.ScmObj* %env$ae52421,%struct.ScmObj* %current_45args60339) {
%stackaddr$env-ref61465 = alloca %struct.ScmObj*, align 8
%z_6348153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52421, i64 0)
store %struct.ScmObj* %z_6348153, %struct.ScmObj** %stackaddr$env-ref61465
%stackaddr$env-ref61466 = alloca %struct.ScmObj*, align 8
%addc48152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52421, i64 1)
store %struct.ScmObj* %addc48152, %struct.ScmObj** %stackaddr$env-ref61466
%stackaddr$env-ref61467 = alloca %struct.ScmObj*, align 8
%fibc48151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52421, i64 2)
store %struct.ScmObj* %fibc48151, %struct.ScmObj** %stackaddr$env-ref61467
%stackaddr$env-ref61468 = alloca %struct.ScmObj*, align 8
%nat_45_62peano48157 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52421, i64 3)
store %struct.ScmObj* %nat_45_62peano48157, %struct.ScmObj** %stackaddr$env-ref61468
%stackaddr$env-ref61469 = alloca %struct.ScmObj*, align 8
%peano_45_62nat48156 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52421, i64 4)
store %struct.ScmObj* %peano_45_62nat48156, %struct.ScmObj** %stackaddr$env-ref61469
%stackaddr$env-ref61470 = alloca %struct.ScmObj*, align 8
%pred48154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52421, i64 5)
store %struct.ScmObj* %pred48154, %struct.ScmObj** %stackaddr$env-ref61470
%stackaddr$prim61471 = alloca %struct.ScmObj*, align 8
%_95k48393 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60339)
store volatile %struct.ScmObj* %_95k48393, %struct.ScmObj** %stackaddr$prim61471, align 8
%stackaddr$prim61472 = alloca %struct.ScmObj*, align 8
%current_45args60340 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60339)
store volatile %struct.ScmObj* %current_45args60340, %struct.ScmObj** %stackaddr$prim61472, align 8
%stackaddr$prim61473 = alloca %struct.ScmObj*, align 8
%anf_45bind48326 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60340)
store volatile %struct.ScmObj* %anf_45bind48326, %struct.ScmObj** %stackaddr$prim61473, align 8
%ae52560 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61474 = alloca %struct.ScmObj*, align 8
%t4802648163 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %addc48152, %struct.ScmObj* %ae52560, %struct.ScmObj* %anf_45bind48326)
store volatile %struct.ScmObj* %t4802648163, %struct.ScmObj** %stackaddr$prim61474, align 8
%stackaddr$makeclosure61475 = alloca %struct.ScmObj*, align 8
%fptrToInt61476 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52562 to i64
%ae52562 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt61476)
store volatile %struct.ScmObj* %ae52562, %struct.ScmObj** %stackaddr$makeclosure61475, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52562, %struct.ScmObj* %fibc48151, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52562, %struct.ScmObj* %nat_45_62peano48157, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52562, %struct.ScmObj* %peano_45_62nat48156, i64 2)
%ae52563 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61477 = alloca %struct.ScmObj*, align 8
%fptrToInt61478 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52564 to i64
%ae52564 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt61478)
store volatile %struct.ScmObj* %ae52564, %struct.ScmObj** %stackaddr$makeclosure61477, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52564, %struct.ScmObj* %z_6348153, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52564, %struct.ScmObj* %addc48152, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52564, %struct.ScmObj* %fibc48151, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52564, %struct.ScmObj* %nat_45_62peano48157, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52564, %struct.ScmObj* %pred48154, i64 4)
%argslist60457$ae525620 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61479 = alloca %struct.ScmObj*, align 8
%argslist60457$ae525621 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52564, %struct.ScmObj* %argslist60457$ae525620)
store volatile %struct.ScmObj* %argslist60457$ae525621, %struct.ScmObj** %stackaddr$prim61479, align 8
%stackaddr$prim61480 = alloca %struct.ScmObj*, align 8
%argslist60457$ae525622 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52563, %struct.ScmObj* %argslist60457$ae525621)
store volatile %struct.ScmObj* %argslist60457$ae525622, %struct.ScmObj** %stackaddr$prim61480, align 8
%clofunc61481 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52562)
musttail call tailcc void %clofunc61481(%struct.ScmObj* %ae52562, %struct.ScmObj* %argslist60457$ae525622)
ret void
}

define tailcc void @proc_clo$ae52562(%struct.ScmObj* %env$ae52562,%struct.ScmObj* %current_45args60342) {
%stackaddr$env-ref61482 = alloca %struct.ScmObj*, align 8
%fibc48151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52562, i64 0)
store %struct.ScmObj* %fibc48151, %struct.ScmObj** %stackaddr$env-ref61482
%stackaddr$env-ref61483 = alloca %struct.ScmObj*, align 8
%nat_45_62peano48157 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52562, i64 1)
store %struct.ScmObj* %nat_45_62peano48157, %struct.ScmObj** %stackaddr$env-ref61483
%stackaddr$env-ref61484 = alloca %struct.ScmObj*, align 8
%peano_45_62nat48156 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52562, i64 2)
store %struct.ScmObj* %peano_45_62nat48156, %struct.ScmObj** %stackaddr$env-ref61484
%stackaddr$prim61485 = alloca %struct.ScmObj*, align 8
%_95k48394 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60342)
store volatile %struct.ScmObj* %_95k48394, %struct.ScmObj** %stackaddr$prim61485, align 8
%stackaddr$prim61486 = alloca %struct.ScmObj*, align 8
%current_45args60343 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60342)
store volatile %struct.ScmObj* %current_45args60343, %struct.ScmObj** %stackaddr$prim61486, align 8
%stackaddr$prim61487 = alloca %struct.ScmObj*, align 8
%anf_45bind48350 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60343)
store volatile %struct.ScmObj* %anf_45bind48350, %struct.ScmObj** %stackaddr$prim61487, align 8
%ae53534 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61488 = alloca %struct.ScmObj*, align 8
%t4802548158 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %fibc48151, %struct.ScmObj* %ae53534, %struct.ScmObj* %anf_45bind48350)
store volatile %struct.ScmObj* %t4802548158, %struct.ScmObj** %stackaddr$prim61488, align 8
%ae53537 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61489 = alloca %struct.ScmObj*, align 8
%anf_45bind48351 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %fibc48151, %struct.ScmObj* %ae53537)
store volatile %struct.ScmObj* %anf_45bind48351, %struct.ScmObj** %stackaddr$prim61489, align 8
%ae53539 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61490 = alloca %struct.ScmObj*, align 8
%anf_45bind48352 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %nat_45_62peano48157, %struct.ScmObj* %ae53539)
store volatile %struct.ScmObj* %anf_45bind48352, %struct.ScmObj** %stackaddr$prim61490, align 8
%stackaddr$makeclosure61491 = alloca %struct.ScmObj*, align 8
%fptrToInt61492 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53541 to i64
%ae53541 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt61492)
store volatile %struct.ScmObj* %ae53541, %struct.ScmObj** %stackaddr$makeclosure61491, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae53541, %struct.ScmObj* %anf_45bind48351, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae53541, %struct.ScmObj* %peano_45_62nat48156, i64 1)
%ae53542 = call %struct.ScmObj* @const_init_int(i64 13)
%argslist60361$anf_45bind483520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61493 = alloca %struct.ScmObj*, align 8
%argslist60361$anf_45bind483521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53542, %struct.ScmObj* %argslist60361$anf_45bind483520)
store volatile %struct.ScmObj* %argslist60361$anf_45bind483521, %struct.ScmObj** %stackaddr$prim61493, align 8
%stackaddr$prim61494 = alloca %struct.ScmObj*, align 8
%argslist60361$anf_45bind483522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53541, %struct.ScmObj* %argslist60361$anf_45bind483521)
store volatile %struct.ScmObj* %argslist60361$anf_45bind483522, %struct.ScmObj** %stackaddr$prim61494, align 8
%clofunc61495 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48352)
musttail call tailcc void %clofunc61495(%struct.ScmObj* %anf_45bind48352, %struct.ScmObj* %argslist60361$anf_45bind483522)
ret void
}

define tailcc void @proc_clo$ae53541(%struct.ScmObj* %env$ae53541,%struct.ScmObj* %current_45args60345) {
%stackaddr$env-ref61496 = alloca %struct.ScmObj*, align 8
%anf_45bind48351 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae53541, i64 0)
store %struct.ScmObj* %anf_45bind48351, %struct.ScmObj** %stackaddr$env-ref61496
%stackaddr$env-ref61497 = alloca %struct.ScmObj*, align 8
%peano_45_62nat48156 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae53541, i64 1)
store %struct.ScmObj* %peano_45_62nat48156, %struct.ScmObj** %stackaddr$env-ref61497
%stackaddr$prim61498 = alloca %struct.ScmObj*, align 8
%_95k48395 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60345)
store volatile %struct.ScmObj* %_95k48395, %struct.ScmObj** %stackaddr$prim61498, align 8
%stackaddr$prim61499 = alloca %struct.ScmObj*, align 8
%current_45args60346 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60345)
store volatile %struct.ScmObj* %current_45args60346, %struct.ScmObj** %stackaddr$prim61499, align 8
%stackaddr$prim61500 = alloca %struct.ScmObj*, align 8
%anf_45bind48353 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60346)
store volatile %struct.ScmObj* %anf_45bind48353, %struct.ScmObj** %stackaddr$prim61500, align 8
%stackaddr$makeclosure61501 = alloca %struct.ScmObj*, align 8
%fptrToInt61502 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53546 to i64
%ae53546 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt61502)
store volatile %struct.ScmObj* %ae53546, %struct.ScmObj** %stackaddr$makeclosure61501, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae53546, %struct.ScmObj* %anf_45bind48353, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae53546, %struct.ScmObj* %anf_45bind48351, i64 1)
%ae53547 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61503 = alloca %struct.ScmObj*, align 8
%fptrToInt61504 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53548 to i64
%ae53548 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt61504)
store volatile %struct.ScmObj* %ae53548, %struct.ScmObj** %stackaddr$makeclosure61503, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae53548, %struct.ScmObj* %peano_45_62nat48156, i64 0)
%argslist60360$ae535460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61505 = alloca %struct.ScmObj*, align 8
%argslist60360$ae535461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53548, %struct.ScmObj* %argslist60360$ae535460)
store volatile %struct.ScmObj* %argslist60360$ae535461, %struct.ScmObj** %stackaddr$prim61505, align 8
%stackaddr$prim61506 = alloca %struct.ScmObj*, align 8
%argslist60360$ae535462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53547, %struct.ScmObj* %argslist60360$ae535461)
store volatile %struct.ScmObj* %argslist60360$ae535462, %struct.ScmObj** %stackaddr$prim61506, align 8
%clofunc61507 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae53546)
musttail call tailcc void %clofunc61507(%struct.ScmObj* %ae53546, %struct.ScmObj* %argslist60360$ae535462)
ret void
}

define tailcc void @proc_clo$ae53546(%struct.ScmObj* %env$ae53546,%struct.ScmObj* %current_45args60348) {
%stackaddr$env-ref61508 = alloca %struct.ScmObj*, align 8
%anf_45bind48353 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae53546, i64 0)
store %struct.ScmObj* %anf_45bind48353, %struct.ScmObj** %stackaddr$env-ref61508
%stackaddr$env-ref61509 = alloca %struct.ScmObj*, align 8
%anf_45bind48351 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae53546, i64 1)
store %struct.ScmObj* %anf_45bind48351, %struct.ScmObj** %stackaddr$env-ref61509
%stackaddr$prim61510 = alloca %struct.ScmObj*, align 8
%_95k48396 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60348)
store volatile %struct.ScmObj* %_95k48396, %struct.ScmObj** %stackaddr$prim61510, align 8
%stackaddr$prim61511 = alloca %struct.ScmObj*, align 8
%current_45args60349 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60348)
store volatile %struct.ScmObj* %current_45args60349, %struct.ScmObj** %stackaddr$prim61511, align 8
%stackaddr$prim61512 = alloca %struct.ScmObj*, align 8
%anf_45bind48355 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60349)
store volatile %struct.ScmObj* %anf_45bind48355, %struct.ScmObj** %stackaddr$prim61512, align 8
%stackaddr$makeclosure61513 = alloca %struct.ScmObj*, align 8
%fptrToInt61514 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53571 to i64
%ae53571 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt61514)
store volatile %struct.ScmObj* %ae53571, %struct.ScmObj** %stackaddr$makeclosure61513, align 8
%argslist60355$anf_45bind483510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61515 = alloca %struct.ScmObj*, align 8
%argslist60355$anf_45bind483511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48355, %struct.ScmObj* %argslist60355$anf_45bind483510)
store volatile %struct.ScmObj* %argslist60355$anf_45bind483511, %struct.ScmObj** %stackaddr$prim61515, align 8
%stackaddr$prim61516 = alloca %struct.ScmObj*, align 8
%argslist60355$anf_45bind483512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48353, %struct.ScmObj* %argslist60355$anf_45bind483511)
store volatile %struct.ScmObj* %argslist60355$anf_45bind483512, %struct.ScmObj** %stackaddr$prim61516, align 8
%stackaddr$prim61517 = alloca %struct.ScmObj*, align 8
%argslist60355$anf_45bind483513 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53571, %struct.ScmObj* %argslist60355$anf_45bind483512)
store volatile %struct.ScmObj* %argslist60355$anf_45bind483513, %struct.ScmObj** %stackaddr$prim61517, align 8
%clofunc61518 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48351)
musttail call tailcc void %clofunc61518(%struct.ScmObj* %anf_45bind48351, %struct.ScmObj* %argslist60355$anf_45bind483513)
ret void
}

define tailcc void @proc_clo$ae53571(%struct.ScmObj* %env$ae53571,%struct.ScmObj* %current_45args60351) {
%stackaddr$prim61519 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60351)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim61519, align 8
%stackaddr$prim61520 = alloca %struct.ScmObj*, align 8
%current_45args60352 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60351)
store volatile %struct.ScmObj* %current_45args60352, %struct.ScmObj** %stackaddr$prim61520, align 8
%stackaddr$prim61521 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60352)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim61521, align 8
%stackaddr$prim61522 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim61522, align 8
%argslist60354$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61523 = alloca %struct.ScmObj*, align 8
%argslist60354$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist60354$k0)
store volatile %struct.ScmObj* %argslist60354$k1, %struct.ScmObj** %stackaddr$prim61523, align 8
%clofunc61524 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc61524(%struct.ScmObj* %k, %struct.ScmObj* %argslist60354$k1)
ret void
}

define tailcc void @proc_clo$ae53548(%struct.ScmObj* %env$ae53548,%struct.ScmObj* %current_45args60356) {
%stackaddr$env-ref61525 = alloca %struct.ScmObj*, align 8
%peano_45_62nat48156 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae53548, i64 0)
store %struct.ScmObj* %peano_45_62nat48156, %struct.ScmObj** %stackaddr$env-ref61525
%stackaddr$prim61526 = alloca %struct.ScmObj*, align 8
%k48397 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60356)
store volatile %struct.ScmObj* %k48397, %struct.ScmObj** %stackaddr$prim61526, align 8
%stackaddr$prim61527 = alloca %struct.ScmObj*, align 8
%current_45args60357 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60356)
store volatile %struct.ScmObj* %current_45args60357, %struct.ScmObj** %stackaddr$prim61527, align 8
%stackaddr$prim61528 = alloca %struct.ScmObj*, align 8
%x48180 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60357)
store volatile %struct.ScmObj* %x48180, %struct.ScmObj** %stackaddr$prim61528, align 8
%ae53550 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61529 = alloca %struct.ScmObj*, align 8
%anf_45bind48354 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %peano_45_62nat48156, %struct.ScmObj* %ae53550)
store volatile %struct.ScmObj* %anf_45bind48354, %struct.ScmObj** %stackaddr$prim61529, align 8
%argslist60359$anf_45bind483540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61530 = alloca %struct.ScmObj*, align 8
%argslist60359$anf_45bind483541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x48180, %struct.ScmObj* %argslist60359$anf_45bind483540)
store volatile %struct.ScmObj* %argslist60359$anf_45bind483541, %struct.ScmObj** %stackaddr$prim61530, align 8
%stackaddr$prim61531 = alloca %struct.ScmObj*, align 8
%argslist60359$anf_45bind483542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48397, %struct.ScmObj* %argslist60359$anf_45bind483541)
store volatile %struct.ScmObj* %argslist60359$anf_45bind483542, %struct.ScmObj** %stackaddr$prim61531, align 8
%clofunc61532 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48354)
musttail call tailcc void %clofunc61532(%struct.ScmObj* %anf_45bind48354, %struct.ScmObj* %argslist60359$anf_45bind483542)
ret void
}

define tailcc void @proc_clo$ae52564(%struct.ScmObj* %env$ae52564,%struct.ScmObj* %current_45args60362) {
%stackaddr$env-ref61533 = alloca %struct.ScmObj*, align 8
%z_6348153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52564, i64 0)
store %struct.ScmObj* %z_6348153, %struct.ScmObj** %stackaddr$env-ref61533
%stackaddr$env-ref61534 = alloca %struct.ScmObj*, align 8
%addc48152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52564, i64 1)
store %struct.ScmObj* %addc48152, %struct.ScmObj** %stackaddr$env-ref61534
%stackaddr$env-ref61535 = alloca %struct.ScmObj*, align 8
%fibc48151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52564, i64 2)
store %struct.ScmObj* %fibc48151, %struct.ScmObj** %stackaddr$env-ref61535
%stackaddr$env-ref61536 = alloca %struct.ScmObj*, align 8
%nat_45_62peano48157 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52564, i64 3)
store %struct.ScmObj* %nat_45_62peano48157, %struct.ScmObj** %stackaddr$env-ref61536
%stackaddr$env-ref61537 = alloca %struct.ScmObj*, align 8
%pred48154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52564, i64 4)
store %struct.ScmObj* %pred48154, %struct.ScmObj** %stackaddr$env-ref61537
%stackaddr$prim61538 = alloca %struct.ScmObj*, align 8
%k48398 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60362)
store volatile %struct.ScmObj* %k48398, %struct.ScmObj** %stackaddr$prim61538, align 8
%stackaddr$prim61539 = alloca %struct.ScmObj*, align 8
%current_45args60363 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60362)
store volatile %struct.ScmObj* %current_45args60363, %struct.ScmObj** %stackaddr$prim61539, align 8
%stackaddr$prim61540 = alloca %struct.ScmObj*, align 8
%x48160 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60363)
store volatile %struct.ScmObj* %x48160, %struct.ScmObj** %stackaddr$prim61540, align 8
%stackaddr$prim61541 = alloca %struct.ScmObj*, align 8
%current_45args60364 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60363)
store volatile %struct.ScmObj* %current_45args60364, %struct.ScmObj** %stackaddr$prim61541, align 8
%stackaddr$prim61542 = alloca %struct.ScmObj*, align 8
%c48159 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60364)
store volatile %struct.ScmObj* %c48159, %struct.ScmObj** %stackaddr$prim61542, align 8
%ae52566 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61543 = alloca %struct.ScmObj*, align 8
%anf_45bind48327 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %z_6348153, %struct.ScmObj* %ae52566)
store volatile %struct.ScmObj* %anf_45bind48327, %struct.ScmObj** %stackaddr$prim61543, align 8
%stackaddr$makeclosure61544 = alloca %struct.ScmObj*, align 8
%fptrToInt61545 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52568 to i64
%ae52568 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt61545)
store volatile %struct.ScmObj* %ae52568, %struct.ScmObj** %stackaddr$makeclosure61544, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52568, %struct.ScmObj* %z_6348153, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52568, %struct.ScmObj* %addc48152, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52568, %struct.ScmObj* %fibc48151, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52568, %struct.ScmObj* %x48160, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52568, %struct.ScmObj* %c48159, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae52568, %struct.ScmObj* %k48398, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae52568, %struct.ScmObj* %nat_45_62peano48157, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae52568, %struct.ScmObj* %pred48154, i64 7)
%argslist60456$anf_45bind483270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61546 = alloca %struct.ScmObj*, align 8
%argslist60456$anf_45bind483271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x48160, %struct.ScmObj* %argslist60456$anf_45bind483270)
store volatile %struct.ScmObj* %argslist60456$anf_45bind483271, %struct.ScmObj** %stackaddr$prim61546, align 8
%stackaddr$prim61547 = alloca %struct.ScmObj*, align 8
%argslist60456$anf_45bind483272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52568, %struct.ScmObj* %argslist60456$anf_45bind483271)
store volatile %struct.ScmObj* %argslist60456$anf_45bind483272, %struct.ScmObj** %stackaddr$prim61547, align 8
%clofunc61548 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48327)
musttail call tailcc void %clofunc61548(%struct.ScmObj* %anf_45bind48327, %struct.ScmObj* %argslist60456$anf_45bind483272)
ret void
}

define tailcc void @proc_clo$ae52568(%struct.ScmObj* %env$ae52568,%struct.ScmObj* %current_45args60366) {
%stackaddr$env-ref61549 = alloca %struct.ScmObj*, align 8
%z_6348153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52568, i64 0)
store %struct.ScmObj* %z_6348153, %struct.ScmObj** %stackaddr$env-ref61549
%stackaddr$env-ref61550 = alloca %struct.ScmObj*, align 8
%addc48152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52568, i64 1)
store %struct.ScmObj* %addc48152, %struct.ScmObj** %stackaddr$env-ref61550
%stackaddr$env-ref61551 = alloca %struct.ScmObj*, align 8
%fibc48151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52568, i64 2)
store %struct.ScmObj* %fibc48151, %struct.ScmObj** %stackaddr$env-ref61551
%stackaddr$env-ref61552 = alloca %struct.ScmObj*, align 8
%x48160 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52568, i64 3)
store %struct.ScmObj* %x48160, %struct.ScmObj** %stackaddr$env-ref61552
%stackaddr$env-ref61553 = alloca %struct.ScmObj*, align 8
%c48159 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52568, i64 4)
store %struct.ScmObj* %c48159, %struct.ScmObj** %stackaddr$env-ref61553
%stackaddr$env-ref61554 = alloca %struct.ScmObj*, align 8
%k48398 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52568, i64 5)
store %struct.ScmObj* %k48398, %struct.ScmObj** %stackaddr$env-ref61554
%stackaddr$env-ref61555 = alloca %struct.ScmObj*, align 8
%nat_45_62peano48157 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52568, i64 6)
store %struct.ScmObj* %nat_45_62peano48157, %struct.ScmObj** %stackaddr$env-ref61555
%stackaddr$env-ref61556 = alloca %struct.ScmObj*, align 8
%pred48154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52568, i64 7)
store %struct.ScmObj* %pred48154, %struct.ScmObj** %stackaddr$env-ref61556
%stackaddr$prim61557 = alloca %struct.ScmObj*, align 8
%_95k48399 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60366)
store volatile %struct.ScmObj* %_95k48399, %struct.ScmObj** %stackaddr$prim61557, align 8
%stackaddr$prim61558 = alloca %struct.ScmObj*, align 8
%current_45args60367 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60366)
store volatile %struct.ScmObj* %current_45args60367, %struct.ScmObj** %stackaddr$prim61558, align 8
%stackaddr$prim61559 = alloca %struct.ScmObj*, align 8
%anf_45bind48328 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60367)
store volatile %struct.ScmObj* %anf_45bind48328, %struct.ScmObj** %stackaddr$prim61559, align 8
%truthy$cmp61560 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48328)
%cmp$cmp61560 = icmp eq i64 %truthy$cmp61560, 1
br i1 %cmp$cmp61560, label %truebranch$cmp61560, label %falsebranch$cmp61560
truebranch$cmp61560:
%ae52572 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61561 = alloca %struct.ScmObj*, align 8
%anf_45bind48329 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %nat_45_62peano48157, %struct.ScmObj* %ae52572)
store volatile %struct.ScmObj* %anf_45bind48329, %struct.ScmObj** %stackaddr$prim61561, align 8
%stackaddr$makeclosure61562 = alloca %struct.ScmObj*, align 8
%fptrToInt61563 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52574 to i64
%ae52574 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt61563)
store volatile %struct.ScmObj* %ae52574, %struct.ScmObj** %stackaddr$makeclosure61562, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52574, %struct.ScmObj* %c48159, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52574, %struct.ScmObj* %k48398, i64 1)
%ae52575 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist60373$anf_45bind483290 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61564 = alloca %struct.ScmObj*, align 8
%argslist60373$anf_45bind483291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52575, %struct.ScmObj* %argslist60373$anf_45bind483290)
store volatile %struct.ScmObj* %argslist60373$anf_45bind483291, %struct.ScmObj** %stackaddr$prim61564, align 8
%stackaddr$prim61565 = alloca %struct.ScmObj*, align 8
%argslist60373$anf_45bind483292 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52574, %struct.ScmObj* %argslist60373$anf_45bind483291)
store volatile %struct.ScmObj* %argslist60373$anf_45bind483292, %struct.ScmObj** %stackaddr$prim61565, align 8
%clofunc61566 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48329)
musttail call tailcc void %clofunc61566(%struct.ScmObj* %anf_45bind48329, %struct.ScmObj* %argslist60373$anf_45bind483292)
ret void
falsebranch$cmp61560:
%ae52594 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61567 = alloca %struct.ScmObj*, align 8
%anf_45bind48331 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %z_6348153, %struct.ScmObj* %ae52594)
store volatile %struct.ScmObj* %anf_45bind48331, %struct.ScmObj** %stackaddr$prim61567, align 8
%ae52596 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61568 = alloca %struct.ScmObj*, align 8
%anf_45bind48332 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %pred48154, %struct.ScmObj* %ae52596)
store volatile %struct.ScmObj* %anf_45bind48332, %struct.ScmObj** %stackaddr$prim61568, align 8
%stackaddr$makeclosure61569 = alloca %struct.ScmObj*, align 8
%fptrToInt61570 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52598 to i64
%ae52598 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt61570)
store volatile %struct.ScmObj* %ae52598, %struct.ScmObj** %stackaddr$makeclosure61569, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52598, %struct.ScmObj* %addc48152, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52598, %struct.ScmObj* %fibc48151, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52598, %struct.ScmObj* %x48160, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52598, %struct.ScmObj* %c48159, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52598, %struct.ScmObj* %k48398, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae52598, %struct.ScmObj* %nat_45_62peano48157, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae52598, %struct.ScmObj* %anf_45bind48331, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae52598, %struct.ScmObj* %pred48154, i64 7)
%argslist60455$anf_45bind483320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61571 = alloca %struct.ScmObj*, align 8
%argslist60455$anf_45bind483321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x48160, %struct.ScmObj* %argslist60455$anf_45bind483320)
store volatile %struct.ScmObj* %argslist60455$anf_45bind483321, %struct.ScmObj** %stackaddr$prim61571, align 8
%stackaddr$prim61572 = alloca %struct.ScmObj*, align 8
%argslist60455$anf_45bind483322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52598, %struct.ScmObj* %argslist60455$anf_45bind483321)
store volatile %struct.ScmObj* %argslist60455$anf_45bind483322, %struct.ScmObj** %stackaddr$prim61572, align 8
%clofunc61573 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48332)
musttail call tailcc void %clofunc61573(%struct.ScmObj* %anf_45bind48332, %struct.ScmObj* %argslist60455$anf_45bind483322)
ret void
}

define tailcc void @proc_clo$ae52574(%struct.ScmObj* %env$ae52574,%struct.ScmObj* %current_45args60369) {
%stackaddr$env-ref61574 = alloca %struct.ScmObj*, align 8
%c48159 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52574, i64 0)
store %struct.ScmObj* %c48159, %struct.ScmObj** %stackaddr$env-ref61574
%stackaddr$env-ref61575 = alloca %struct.ScmObj*, align 8
%k48398 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52574, i64 1)
store %struct.ScmObj* %k48398, %struct.ScmObj** %stackaddr$env-ref61575
%stackaddr$prim61576 = alloca %struct.ScmObj*, align 8
%_95k48400 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60369)
store volatile %struct.ScmObj* %_95k48400, %struct.ScmObj** %stackaddr$prim61576, align 8
%stackaddr$prim61577 = alloca %struct.ScmObj*, align 8
%current_45args60370 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60369)
store volatile %struct.ScmObj* %current_45args60370, %struct.ScmObj** %stackaddr$prim61577, align 8
%stackaddr$prim61578 = alloca %struct.ScmObj*, align 8
%anf_45bind48330 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60370)
store volatile %struct.ScmObj* %anf_45bind48330, %struct.ScmObj** %stackaddr$prim61578, align 8
%argslist60372$c481590 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61579 = alloca %struct.ScmObj*, align 8
%argslist60372$c481591 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48330, %struct.ScmObj* %argslist60372$c481590)
store volatile %struct.ScmObj* %argslist60372$c481591, %struct.ScmObj** %stackaddr$prim61579, align 8
%stackaddr$prim61580 = alloca %struct.ScmObj*, align 8
%argslist60372$c481592 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48398, %struct.ScmObj* %argslist60372$c481591)
store volatile %struct.ScmObj* %argslist60372$c481592, %struct.ScmObj** %stackaddr$prim61580, align 8
%clofunc61581 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %c48159)
musttail call tailcc void %clofunc61581(%struct.ScmObj* %c48159, %struct.ScmObj* %argslist60372$c481592)
ret void
}

define tailcc void @proc_clo$ae52598(%struct.ScmObj* %env$ae52598,%struct.ScmObj* %current_45args60374) {
%stackaddr$env-ref61582 = alloca %struct.ScmObj*, align 8
%addc48152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52598, i64 0)
store %struct.ScmObj* %addc48152, %struct.ScmObj** %stackaddr$env-ref61582
%stackaddr$env-ref61583 = alloca %struct.ScmObj*, align 8
%fibc48151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52598, i64 1)
store %struct.ScmObj* %fibc48151, %struct.ScmObj** %stackaddr$env-ref61583
%stackaddr$env-ref61584 = alloca %struct.ScmObj*, align 8
%x48160 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52598, i64 2)
store %struct.ScmObj* %x48160, %struct.ScmObj** %stackaddr$env-ref61584
%stackaddr$env-ref61585 = alloca %struct.ScmObj*, align 8
%c48159 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52598, i64 3)
store %struct.ScmObj* %c48159, %struct.ScmObj** %stackaddr$env-ref61585
%stackaddr$env-ref61586 = alloca %struct.ScmObj*, align 8
%k48398 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52598, i64 4)
store %struct.ScmObj* %k48398, %struct.ScmObj** %stackaddr$env-ref61586
%stackaddr$env-ref61587 = alloca %struct.ScmObj*, align 8
%nat_45_62peano48157 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52598, i64 5)
store %struct.ScmObj* %nat_45_62peano48157, %struct.ScmObj** %stackaddr$env-ref61587
%stackaddr$env-ref61588 = alloca %struct.ScmObj*, align 8
%anf_45bind48331 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52598, i64 6)
store %struct.ScmObj* %anf_45bind48331, %struct.ScmObj** %stackaddr$env-ref61588
%stackaddr$env-ref61589 = alloca %struct.ScmObj*, align 8
%pred48154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52598, i64 7)
store %struct.ScmObj* %pred48154, %struct.ScmObj** %stackaddr$env-ref61589
%stackaddr$prim61590 = alloca %struct.ScmObj*, align 8
%_95k48401 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60374)
store volatile %struct.ScmObj* %_95k48401, %struct.ScmObj** %stackaddr$prim61590, align 8
%stackaddr$prim61591 = alloca %struct.ScmObj*, align 8
%current_45args60375 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60374)
store volatile %struct.ScmObj* %current_45args60375, %struct.ScmObj** %stackaddr$prim61591, align 8
%stackaddr$prim61592 = alloca %struct.ScmObj*, align 8
%anf_45bind48333 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60375)
store volatile %struct.ScmObj* %anf_45bind48333, %struct.ScmObj** %stackaddr$prim61592, align 8
%stackaddr$makeclosure61593 = alloca %struct.ScmObj*, align 8
%fptrToInt61594 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52601 to i64
%ae52601 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt61594)
store volatile %struct.ScmObj* %ae52601, %struct.ScmObj** %stackaddr$makeclosure61593, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52601, %struct.ScmObj* %addc48152, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52601, %struct.ScmObj* %fibc48151, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52601, %struct.ScmObj* %x48160, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52601, %struct.ScmObj* %c48159, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52601, %struct.ScmObj* %k48398, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae52601, %struct.ScmObj* %nat_45_62peano48157, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae52601, %struct.ScmObj* %pred48154, i64 6)
%argslist60454$anf_45bind483310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61595 = alloca %struct.ScmObj*, align 8
%argslist60454$anf_45bind483311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48333, %struct.ScmObj* %argslist60454$anf_45bind483310)
store volatile %struct.ScmObj* %argslist60454$anf_45bind483311, %struct.ScmObj** %stackaddr$prim61595, align 8
%stackaddr$prim61596 = alloca %struct.ScmObj*, align 8
%argslist60454$anf_45bind483312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52601, %struct.ScmObj* %argslist60454$anf_45bind483311)
store volatile %struct.ScmObj* %argslist60454$anf_45bind483312, %struct.ScmObj** %stackaddr$prim61596, align 8
%clofunc61597 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48331)
musttail call tailcc void %clofunc61597(%struct.ScmObj* %anf_45bind48331, %struct.ScmObj* %argslist60454$anf_45bind483312)
ret void
}

define tailcc void @proc_clo$ae52601(%struct.ScmObj* %env$ae52601,%struct.ScmObj* %current_45args60377) {
%stackaddr$env-ref61598 = alloca %struct.ScmObj*, align 8
%addc48152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52601, i64 0)
store %struct.ScmObj* %addc48152, %struct.ScmObj** %stackaddr$env-ref61598
%stackaddr$env-ref61599 = alloca %struct.ScmObj*, align 8
%fibc48151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52601, i64 1)
store %struct.ScmObj* %fibc48151, %struct.ScmObj** %stackaddr$env-ref61599
%stackaddr$env-ref61600 = alloca %struct.ScmObj*, align 8
%x48160 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52601, i64 2)
store %struct.ScmObj* %x48160, %struct.ScmObj** %stackaddr$env-ref61600
%stackaddr$env-ref61601 = alloca %struct.ScmObj*, align 8
%c48159 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52601, i64 3)
store %struct.ScmObj* %c48159, %struct.ScmObj** %stackaddr$env-ref61601
%stackaddr$env-ref61602 = alloca %struct.ScmObj*, align 8
%k48398 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52601, i64 4)
store %struct.ScmObj* %k48398, %struct.ScmObj** %stackaddr$env-ref61602
%stackaddr$env-ref61603 = alloca %struct.ScmObj*, align 8
%nat_45_62peano48157 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52601, i64 5)
store %struct.ScmObj* %nat_45_62peano48157, %struct.ScmObj** %stackaddr$env-ref61603
%stackaddr$env-ref61604 = alloca %struct.ScmObj*, align 8
%pred48154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52601, i64 6)
store %struct.ScmObj* %pred48154, %struct.ScmObj** %stackaddr$env-ref61604
%stackaddr$prim61605 = alloca %struct.ScmObj*, align 8
%_95k48402 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60377)
store volatile %struct.ScmObj* %_95k48402, %struct.ScmObj** %stackaddr$prim61605, align 8
%stackaddr$prim61606 = alloca %struct.ScmObj*, align 8
%current_45args60378 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60377)
store volatile %struct.ScmObj* %current_45args60378, %struct.ScmObj** %stackaddr$prim61606, align 8
%stackaddr$prim61607 = alloca %struct.ScmObj*, align 8
%anf_45bind48334 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60378)
store volatile %struct.ScmObj* %anf_45bind48334, %struct.ScmObj** %stackaddr$prim61607, align 8
%truthy$cmp61608 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48334)
%cmp$cmp61608 = icmp eq i64 %truthy$cmp61608, 1
br i1 %cmp$cmp61608, label %truebranch$cmp61608, label %falsebranch$cmp61608
truebranch$cmp61608:
%ae52605 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61609 = alloca %struct.ScmObj*, align 8
%anf_45bind48335 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %nat_45_62peano48157, %struct.ScmObj* %ae52605)
store volatile %struct.ScmObj* %anf_45bind48335, %struct.ScmObj** %stackaddr$prim61609, align 8
%stackaddr$makeclosure61610 = alloca %struct.ScmObj*, align 8
%fptrToInt61611 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52607 to i64
%ae52607 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt61611)
store volatile %struct.ScmObj* %ae52607, %struct.ScmObj** %stackaddr$makeclosure61610, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52607, %struct.ScmObj* %c48159, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52607, %struct.ScmObj* %k48398, i64 1)
%ae52608 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist60384$anf_45bind483350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61612 = alloca %struct.ScmObj*, align 8
%argslist60384$anf_45bind483351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52608, %struct.ScmObj* %argslist60384$anf_45bind483350)
store volatile %struct.ScmObj* %argslist60384$anf_45bind483351, %struct.ScmObj** %stackaddr$prim61612, align 8
%stackaddr$prim61613 = alloca %struct.ScmObj*, align 8
%argslist60384$anf_45bind483352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52607, %struct.ScmObj* %argslist60384$anf_45bind483351)
store volatile %struct.ScmObj* %argslist60384$anf_45bind483352, %struct.ScmObj** %stackaddr$prim61613, align 8
%clofunc61614 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48335)
musttail call tailcc void %clofunc61614(%struct.ScmObj* %anf_45bind48335, %struct.ScmObj* %argslist60384$anf_45bind483352)
ret void
falsebranch$cmp61608:
%ae52627 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61615 = alloca %struct.ScmObj*, align 8
%anf_45bind48337 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %addc48152, %struct.ScmObj* %ae52627)
store volatile %struct.ScmObj* %anf_45bind48337, %struct.ScmObj** %stackaddr$prim61615, align 8
%stackaddr$makeclosure61616 = alloca %struct.ScmObj*, align 8
%fptrToInt61617 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52628 to i64
%ae52628 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt61617)
store volatile %struct.ScmObj* %ae52628, %struct.ScmObj** %stackaddr$makeclosure61616, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52628, %struct.ScmObj* %fibc48151, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52628, %struct.ScmObj* %anf_45bind48337, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52628, %struct.ScmObj* %x48160, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52628, %struct.ScmObj* %c48159, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52628, %struct.ScmObj* %k48398, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae52628, %struct.ScmObj* %pred48154, i64 5)
%ae52629 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61618 = alloca %struct.ScmObj*, align 8
%fptrToInt61619 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52630 to i64
%ae52630 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt61619)
store volatile %struct.ScmObj* %ae52630, %struct.ScmObj** %stackaddr$makeclosure61618, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52630, %struct.ScmObj* %fibc48151, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52630, %struct.ScmObj* %x48160, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52630, %struct.ScmObj* %pred48154, i64 2)
%argslist60453$ae526280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61620 = alloca %struct.ScmObj*, align 8
%argslist60453$ae526281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52630, %struct.ScmObj* %argslist60453$ae526280)
store volatile %struct.ScmObj* %argslist60453$ae526281, %struct.ScmObj** %stackaddr$prim61620, align 8
%stackaddr$prim61621 = alloca %struct.ScmObj*, align 8
%argslist60453$ae526282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52629, %struct.ScmObj* %argslist60453$ae526281)
store volatile %struct.ScmObj* %argslist60453$ae526282, %struct.ScmObj** %stackaddr$prim61621, align 8
%clofunc61622 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52628)
musttail call tailcc void %clofunc61622(%struct.ScmObj* %ae52628, %struct.ScmObj* %argslist60453$ae526282)
ret void
}

define tailcc void @proc_clo$ae52607(%struct.ScmObj* %env$ae52607,%struct.ScmObj* %current_45args60380) {
%stackaddr$env-ref61623 = alloca %struct.ScmObj*, align 8
%c48159 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52607, i64 0)
store %struct.ScmObj* %c48159, %struct.ScmObj** %stackaddr$env-ref61623
%stackaddr$env-ref61624 = alloca %struct.ScmObj*, align 8
%k48398 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52607, i64 1)
store %struct.ScmObj* %k48398, %struct.ScmObj** %stackaddr$env-ref61624
%stackaddr$prim61625 = alloca %struct.ScmObj*, align 8
%_95k48403 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60380)
store volatile %struct.ScmObj* %_95k48403, %struct.ScmObj** %stackaddr$prim61625, align 8
%stackaddr$prim61626 = alloca %struct.ScmObj*, align 8
%current_45args60381 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60380)
store volatile %struct.ScmObj* %current_45args60381, %struct.ScmObj** %stackaddr$prim61626, align 8
%stackaddr$prim61627 = alloca %struct.ScmObj*, align 8
%anf_45bind48336 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60381)
store volatile %struct.ScmObj* %anf_45bind48336, %struct.ScmObj** %stackaddr$prim61627, align 8
%argslist60383$c481590 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61628 = alloca %struct.ScmObj*, align 8
%argslist60383$c481591 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48336, %struct.ScmObj* %argslist60383$c481590)
store volatile %struct.ScmObj* %argslist60383$c481591, %struct.ScmObj** %stackaddr$prim61628, align 8
%stackaddr$prim61629 = alloca %struct.ScmObj*, align 8
%argslist60383$c481592 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48398, %struct.ScmObj* %argslist60383$c481591)
store volatile %struct.ScmObj* %argslist60383$c481592, %struct.ScmObj** %stackaddr$prim61629, align 8
%clofunc61630 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %c48159)
musttail call tailcc void %clofunc61630(%struct.ScmObj* %c48159, %struct.ScmObj* %argslist60383$c481592)
ret void
}

define tailcc void @proc_clo$ae52628(%struct.ScmObj* %env$ae52628,%struct.ScmObj* %current_45args60385) {
%stackaddr$env-ref61631 = alloca %struct.ScmObj*, align 8
%fibc48151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52628, i64 0)
store %struct.ScmObj* %fibc48151, %struct.ScmObj** %stackaddr$env-ref61631
%stackaddr$env-ref61632 = alloca %struct.ScmObj*, align 8
%anf_45bind48337 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52628, i64 1)
store %struct.ScmObj* %anf_45bind48337, %struct.ScmObj** %stackaddr$env-ref61632
%stackaddr$env-ref61633 = alloca %struct.ScmObj*, align 8
%x48160 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52628, i64 2)
store %struct.ScmObj* %x48160, %struct.ScmObj** %stackaddr$env-ref61633
%stackaddr$env-ref61634 = alloca %struct.ScmObj*, align 8
%c48159 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52628, i64 3)
store %struct.ScmObj* %c48159, %struct.ScmObj** %stackaddr$env-ref61634
%stackaddr$env-ref61635 = alloca %struct.ScmObj*, align 8
%k48398 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52628, i64 4)
store %struct.ScmObj* %k48398, %struct.ScmObj** %stackaddr$env-ref61635
%stackaddr$env-ref61636 = alloca %struct.ScmObj*, align 8
%pred48154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52628, i64 5)
store %struct.ScmObj* %pred48154, %struct.ScmObj** %stackaddr$env-ref61636
%stackaddr$prim61637 = alloca %struct.ScmObj*, align 8
%_95k48404 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60385)
store volatile %struct.ScmObj* %_95k48404, %struct.ScmObj** %stackaddr$prim61637, align 8
%stackaddr$prim61638 = alloca %struct.ScmObj*, align 8
%current_45args60386 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60385)
store volatile %struct.ScmObj* %current_45args60386, %struct.ScmObj** %stackaddr$prim61638, align 8
%stackaddr$prim61639 = alloca %struct.ScmObj*, align 8
%anf_45bind48341 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60386)
store volatile %struct.ScmObj* %anf_45bind48341, %struct.ScmObj** %stackaddr$prim61639, align 8
%stackaddr$makeclosure61640 = alloca %struct.ScmObj*, align 8
%fptrToInt61641 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52683 to i64
%ae52683 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt61641)
store volatile %struct.ScmObj* %ae52683, %struct.ScmObj** %stackaddr$makeclosure61640, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52683, %struct.ScmObj* %fibc48151, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52683, %struct.ScmObj* %anf_45bind48337, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52683, %struct.ScmObj* %x48160, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52683, %struct.ScmObj* %c48159, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52683, %struct.ScmObj* %k48398, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae52683, %struct.ScmObj* %pred48154, i64 5)
%stackaddr$makeclosure61642 = alloca %struct.ScmObj*, align 8
%fptrToInt61643 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52684 to i64
%ae52684 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt61643)
store volatile %struct.ScmObj* %ae52684, %struct.ScmObj** %stackaddr$makeclosure61642, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52684, %struct.ScmObj* %fibc48151, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52684, %struct.ScmObj* %anf_45bind48337, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52684, %struct.ScmObj* %x48160, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52684, %struct.ScmObj* %c48159, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52684, %struct.ScmObj* %k48398, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae52684, %struct.ScmObj* %pred48154, i64 5)
%argslist60444$anf_45bind483410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61644 = alloca %struct.ScmObj*, align 8
%argslist60444$anf_45bind483411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52684, %struct.ScmObj* %argslist60444$anf_45bind483410)
store volatile %struct.ScmObj* %argslist60444$anf_45bind483411, %struct.ScmObj** %stackaddr$prim61644, align 8
%stackaddr$prim61645 = alloca %struct.ScmObj*, align 8
%argslist60444$anf_45bind483412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52683, %struct.ScmObj* %argslist60444$anf_45bind483411)
store volatile %struct.ScmObj* %argslist60444$anf_45bind483412, %struct.ScmObj** %stackaddr$prim61645, align 8
%clofunc61646 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48341)
musttail call tailcc void %clofunc61646(%struct.ScmObj* %anf_45bind48341, %struct.ScmObj* %argslist60444$anf_45bind483412)
ret void
}

define tailcc void @proc_clo$ae52683(%struct.ScmObj* %env$ae52683,%struct.ScmObj* %current_45args60388) {
%stackaddr$env-ref61647 = alloca %struct.ScmObj*, align 8
%fibc48151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52683, i64 0)
store %struct.ScmObj* %fibc48151, %struct.ScmObj** %stackaddr$env-ref61647
%stackaddr$env-ref61648 = alloca %struct.ScmObj*, align 8
%anf_45bind48337 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52683, i64 1)
store %struct.ScmObj* %anf_45bind48337, %struct.ScmObj** %stackaddr$env-ref61648
%stackaddr$env-ref61649 = alloca %struct.ScmObj*, align 8
%x48160 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52683, i64 2)
store %struct.ScmObj* %x48160, %struct.ScmObj** %stackaddr$env-ref61649
%stackaddr$env-ref61650 = alloca %struct.ScmObj*, align 8
%c48159 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52683, i64 3)
store %struct.ScmObj* %c48159, %struct.ScmObj** %stackaddr$env-ref61650
%stackaddr$env-ref61651 = alloca %struct.ScmObj*, align 8
%k48398 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52683, i64 4)
store %struct.ScmObj* %k48398, %struct.ScmObj** %stackaddr$env-ref61651
%stackaddr$env-ref61652 = alloca %struct.ScmObj*, align 8
%pred48154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52683, i64 5)
store %struct.ScmObj* %pred48154, %struct.ScmObj** %stackaddr$env-ref61652
%stackaddr$prim61653 = alloca %struct.ScmObj*, align 8
%_95k48405 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60388)
store volatile %struct.ScmObj* %_95k48405, %struct.ScmObj** %stackaddr$prim61653, align 8
%stackaddr$prim61654 = alloca %struct.ScmObj*, align 8
%current_45args60389 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60388)
store volatile %struct.ScmObj* %current_45args60389, %struct.ScmObj** %stackaddr$prim61654, align 8
%stackaddr$prim61655 = alloca %struct.ScmObj*, align 8
%anf_45bind48342 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60389)
store volatile %struct.ScmObj* %anf_45bind48342, %struct.ScmObj** %stackaddr$prim61655, align 8
%stackaddr$makeclosure61656 = alloca %struct.ScmObj*, align 8
%fptrToInt61657 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52820 to i64
%ae52820 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt61657)
store volatile %struct.ScmObj* %ae52820, %struct.ScmObj** %stackaddr$makeclosure61656, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52820, %struct.ScmObj* %anf_45bind48342, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52820, %struct.ScmObj* %anf_45bind48337, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52820, %struct.ScmObj* %c48159, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52820, %struct.ScmObj* %k48398, i64 3)
%ae52821 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61658 = alloca %struct.ScmObj*, align 8
%fptrToInt61659 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52822 to i64
%ae52822 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt61659)
store volatile %struct.ScmObj* %ae52822, %struct.ScmObj** %stackaddr$makeclosure61658, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52822, %struct.ScmObj* %fibc48151, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52822, %struct.ScmObj* %x48160, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52822, %struct.ScmObj* %pred48154, i64 2)
%argslist60415$ae528200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61660 = alloca %struct.ScmObj*, align 8
%argslist60415$ae528201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52822, %struct.ScmObj* %argslist60415$ae528200)
store volatile %struct.ScmObj* %argslist60415$ae528201, %struct.ScmObj** %stackaddr$prim61660, align 8
%stackaddr$prim61661 = alloca %struct.ScmObj*, align 8
%argslist60415$ae528202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52821, %struct.ScmObj* %argslist60415$ae528201)
store volatile %struct.ScmObj* %argslist60415$ae528202, %struct.ScmObj** %stackaddr$prim61661, align 8
%clofunc61662 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52820)
musttail call tailcc void %clofunc61662(%struct.ScmObj* %ae52820, %struct.ScmObj* %argslist60415$ae528202)
ret void
}

define tailcc void @proc_clo$ae52820(%struct.ScmObj* %env$ae52820,%struct.ScmObj* %current_45args60391) {
%stackaddr$env-ref61663 = alloca %struct.ScmObj*, align 8
%anf_45bind48342 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52820, i64 0)
store %struct.ScmObj* %anf_45bind48342, %struct.ScmObj** %stackaddr$env-ref61663
%stackaddr$env-ref61664 = alloca %struct.ScmObj*, align 8
%anf_45bind48337 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52820, i64 1)
store %struct.ScmObj* %anf_45bind48337, %struct.ScmObj** %stackaddr$env-ref61664
%stackaddr$env-ref61665 = alloca %struct.ScmObj*, align 8
%c48159 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52820, i64 2)
store %struct.ScmObj* %c48159, %struct.ScmObj** %stackaddr$env-ref61665
%stackaddr$env-ref61666 = alloca %struct.ScmObj*, align 8
%k48398 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52820, i64 3)
store %struct.ScmObj* %k48398, %struct.ScmObj** %stackaddr$env-ref61666
%stackaddr$prim61667 = alloca %struct.ScmObj*, align 8
%_95k48406 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60391)
store volatile %struct.ScmObj* %_95k48406, %struct.ScmObj** %stackaddr$prim61667, align 8
%stackaddr$prim61668 = alloca %struct.ScmObj*, align 8
%current_45args60392 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60391)
store volatile %struct.ScmObj* %current_45args60392, %struct.ScmObj** %stackaddr$prim61668, align 8
%stackaddr$prim61669 = alloca %struct.ScmObj*, align 8
%anf_45bind48348 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60392)
store volatile %struct.ScmObj* %anf_45bind48348, %struct.ScmObj** %stackaddr$prim61669, align 8
%stackaddr$makeclosure61670 = alloca %struct.ScmObj*, align 8
%fptrToInt61671 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52910 to i64
%ae52910 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt61671)
store volatile %struct.ScmObj* %ae52910, %struct.ScmObj** %stackaddr$makeclosure61670, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52910, %struct.ScmObj* %anf_45bind48342, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52910, %struct.ScmObj* %anf_45bind48337, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52910, %struct.ScmObj* %c48159, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52910, %struct.ScmObj* %k48398, i64 3)
%stackaddr$makeclosure61672 = alloca %struct.ScmObj*, align 8
%fptrToInt61673 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52911 to i64
%ae52911 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt61673)
store volatile %struct.ScmObj* %ae52911, %struct.ScmObj** %stackaddr$makeclosure61672, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52911, %struct.ScmObj* %anf_45bind48342, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52911, %struct.ScmObj* %anf_45bind48337, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52911, %struct.ScmObj* %c48159, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52911, %struct.ScmObj* %k48398, i64 3)
%argslist60402$anf_45bind483480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61674 = alloca %struct.ScmObj*, align 8
%argslist60402$anf_45bind483481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52911, %struct.ScmObj* %argslist60402$anf_45bind483480)
store volatile %struct.ScmObj* %argslist60402$anf_45bind483481, %struct.ScmObj** %stackaddr$prim61674, align 8
%stackaddr$prim61675 = alloca %struct.ScmObj*, align 8
%argslist60402$anf_45bind483482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52910, %struct.ScmObj* %argslist60402$anf_45bind483481)
store volatile %struct.ScmObj* %argslist60402$anf_45bind483482, %struct.ScmObj** %stackaddr$prim61675, align 8
%clofunc61676 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48348)
musttail call tailcc void %clofunc61676(%struct.ScmObj* %anf_45bind48348, %struct.ScmObj* %argslist60402$anf_45bind483482)
ret void
}

define tailcc void @proc_clo$ae52910(%struct.ScmObj* %env$ae52910,%struct.ScmObj* %current_45args60394) {
%stackaddr$env-ref61677 = alloca %struct.ScmObj*, align 8
%anf_45bind48342 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52910, i64 0)
store %struct.ScmObj* %anf_45bind48342, %struct.ScmObj** %stackaddr$env-ref61677
%stackaddr$env-ref61678 = alloca %struct.ScmObj*, align 8
%anf_45bind48337 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52910, i64 1)
store %struct.ScmObj* %anf_45bind48337, %struct.ScmObj** %stackaddr$env-ref61678
%stackaddr$env-ref61679 = alloca %struct.ScmObj*, align 8
%c48159 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52910, i64 2)
store %struct.ScmObj* %c48159, %struct.ScmObj** %stackaddr$env-ref61679
%stackaddr$env-ref61680 = alloca %struct.ScmObj*, align 8
%k48398 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52910, i64 3)
store %struct.ScmObj* %k48398, %struct.ScmObj** %stackaddr$env-ref61680
%stackaddr$prim61681 = alloca %struct.ScmObj*, align 8
%_95k48407 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60394)
store volatile %struct.ScmObj* %_95k48407, %struct.ScmObj** %stackaddr$prim61681, align 8
%stackaddr$prim61682 = alloca %struct.ScmObj*, align 8
%current_45args60395 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60394)
store volatile %struct.ScmObj* %current_45args60395, %struct.ScmObj** %stackaddr$prim61682, align 8
%stackaddr$prim61683 = alloca %struct.ScmObj*, align 8
%anf_45bind48349 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60395)
store volatile %struct.ScmObj* %anf_45bind48349, %struct.ScmObj** %stackaddr$prim61683, align 8
%argslist60397$anf_45bind483370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61684 = alloca %struct.ScmObj*, align 8
%argslist60397$anf_45bind483371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %c48159, %struct.ScmObj* %argslist60397$anf_45bind483370)
store volatile %struct.ScmObj* %argslist60397$anf_45bind483371, %struct.ScmObj** %stackaddr$prim61684, align 8
%stackaddr$prim61685 = alloca %struct.ScmObj*, align 8
%argslist60397$anf_45bind483372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48349, %struct.ScmObj* %argslist60397$anf_45bind483371)
store volatile %struct.ScmObj* %argslist60397$anf_45bind483372, %struct.ScmObj** %stackaddr$prim61685, align 8
%stackaddr$prim61686 = alloca %struct.ScmObj*, align 8
%argslist60397$anf_45bind483373 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48342, %struct.ScmObj* %argslist60397$anf_45bind483372)
store volatile %struct.ScmObj* %argslist60397$anf_45bind483373, %struct.ScmObj** %stackaddr$prim61686, align 8
%stackaddr$prim61687 = alloca %struct.ScmObj*, align 8
%argslist60397$anf_45bind483374 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48398, %struct.ScmObj* %argslist60397$anf_45bind483373)
store volatile %struct.ScmObj* %argslist60397$anf_45bind483374, %struct.ScmObj** %stackaddr$prim61687, align 8
%clofunc61688 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48337)
musttail call tailcc void %clofunc61688(%struct.ScmObj* %anf_45bind48337, %struct.ScmObj* %argslist60397$anf_45bind483374)
ret void
}

define tailcc void @proc_clo$ae52911(%struct.ScmObj* %env$ae52911,%struct.ScmObj* %current_45args60398) {
%stackaddr$env-ref61689 = alloca %struct.ScmObj*, align 8
%anf_45bind48342 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52911, i64 0)
store %struct.ScmObj* %anf_45bind48342, %struct.ScmObj** %stackaddr$env-ref61689
%stackaddr$env-ref61690 = alloca %struct.ScmObj*, align 8
%anf_45bind48337 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52911, i64 1)
store %struct.ScmObj* %anf_45bind48337, %struct.ScmObj** %stackaddr$env-ref61690
%stackaddr$env-ref61691 = alloca %struct.ScmObj*, align 8
%c48159 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52911, i64 2)
store %struct.ScmObj* %c48159, %struct.ScmObj** %stackaddr$env-ref61691
%stackaddr$env-ref61692 = alloca %struct.ScmObj*, align 8
%k48398 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52911, i64 3)
store %struct.ScmObj* %k48398, %struct.ScmObj** %stackaddr$env-ref61692
%stackaddr$prim61693 = alloca %struct.ScmObj*, align 8
%_95k48407 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60398)
store volatile %struct.ScmObj* %_95k48407, %struct.ScmObj** %stackaddr$prim61693, align 8
%stackaddr$prim61694 = alloca %struct.ScmObj*, align 8
%current_45args60399 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60398)
store volatile %struct.ScmObj* %current_45args60399, %struct.ScmObj** %stackaddr$prim61694, align 8
%stackaddr$prim61695 = alloca %struct.ScmObj*, align 8
%anf_45bind48349 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60399)
store volatile %struct.ScmObj* %anf_45bind48349, %struct.ScmObj** %stackaddr$prim61695, align 8
%argslist60401$anf_45bind483370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61696 = alloca %struct.ScmObj*, align 8
%argslist60401$anf_45bind483371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %c48159, %struct.ScmObj* %argslist60401$anf_45bind483370)
store volatile %struct.ScmObj* %argslist60401$anf_45bind483371, %struct.ScmObj** %stackaddr$prim61696, align 8
%stackaddr$prim61697 = alloca %struct.ScmObj*, align 8
%argslist60401$anf_45bind483372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48349, %struct.ScmObj* %argslist60401$anf_45bind483371)
store volatile %struct.ScmObj* %argslist60401$anf_45bind483372, %struct.ScmObj** %stackaddr$prim61697, align 8
%stackaddr$prim61698 = alloca %struct.ScmObj*, align 8
%argslist60401$anf_45bind483373 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48342, %struct.ScmObj* %argslist60401$anf_45bind483372)
store volatile %struct.ScmObj* %argslist60401$anf_45bind483373, %struct.ScmObj** %stackaddr$prim61698, align 8
%stackaddr$prim61699 = alloca %struct.ScmObj*, align 8
%argslist60401$anf_45bind483374 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48398, %struct.ScmObj* %argslist60401$anf_45bind483373)
store volatile %struct.ScmObj* %argslist60401$anf_45bind483374, %struct.ScmObj** %stackaddr$prim61699, align 8
%clofunc61700 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48337)
musttail call tailcc void %clofunc61700(%struct.ScmObj* %anf_45bind48337, %struct.ScmObj* %argslist60401$anf_45bind483374)
ret void
}

define tailcc void @proc_clo$ae52822(%struct.ScmObj* %env$ae52822,%struct.ScmObj* %current_45args60403) {
%stackaddr$env-ref61701 = alloca %struct.ScmObj*, align 8
%fibc48151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52822, i64 0)
store %struct.ScmObj* %fibc48151, %struct.ScmObj** %stackaddr$env-ref61701
%stackaddr$env-ref61702 = alloca %struct.ScmObj*, align 8
%x48160 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52822, i64 1)
store %struct.ScmObj* %x48160, %struct.ScmObj** %stackaddr$env-ref61702
%stackaddr$env-ref61703 = alloca %struct.ScmObj*, align 8
%pred48154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52822, i64 2)
store %struct.ScmObj* %pred48154, %struct.ScmObj** %stackaddr$env-ref61703
%stackaddr$prim61704 = alloca %struct.ScmObj*, align 8
%k48408 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60403)
store volatile %struct.ScmObj* %k48408, %struct.ScmObj** %stackaddr$prim61704, align 8
%stackaddr$prim61705 = alloca %struct.ScmObj*, align 8
%current_45args60404 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60403)
store volatile %struct.ScmObj* %current_45args60404, %struct.ScmObj** %stackaddr$prim61705, align 8
%stackaddr$prim61706 = alloca %struct.ScmObj*, align 8
%c48162 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60404)
store volatile %struct.ScmObj* %c48162, %struct.ScmObj** %stackaddr$prim61706, align 8
%ae52824 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61707 = alloca %struct.ScmObj*, align 8
%anf_45bind48343 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %fibc48151, %struct.ScmObj* %ae52824)
store volatile %struct.ScmObj* %anf_45bind48343, %struct.ScmObj** %stackaddr$prim61707, align 8
%ae52826 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61708 = alloca %struct.ScmObj*, align 8
%anf_45bind48344 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %pred48154, %struct.ScmObj* %ae52826)
store volatile %struct.ScmObj* %anf_45bind48344, %struct.ScmObj** %stackaddr$prim61708, align 8
%ae52828 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61709 = alloca %struct.ScmObj*, align 8
%anf_45bind48345 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %pred48154, %struct.ScmObj* %ae52828)
store volatile %struct.ScmObj* %anf_45bind48345, %struct.ScmObj** %stackaddr$prim61709, align 8
%stackaddr$makeclosure61710 = alloca %struct.ScmObj*, align 8
%fptrToInt61711 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52830 to i64
%ae52830 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt61711)
store volatile %struct.ScmObj* %ae52830, %struct.ScmObj** %stackaddr$makeclosure61710, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52830, %struct.ScmObj* %k48408, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52830, %struct.ScmObj* %anf_45bind48344, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52830, %struct.ScmObj* %anf_45bind48343, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52830, %struct.ScmObj* %c48162, i64 3)
%argslist60414$anf_45bind483450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61712 = alloca %struct.ScmObj*, align 8
%argslist60414$anf_45bind483451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x48160, %struct.ScmObj* %argslist60414$anf_45bind483450)
store volatile %struct.ScmObj* %argslist60414$anf_45bind483451, %struct.ScmObj** %stackaddr$prim61712, align 8
%stackaddr$prim61713 = alloca %struct.ScmObj*, align 8
%argslist60414$anf_45bind483452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52830, %struct.ScmObj* %argslist60414$anf_45bind483451)
store volatile %struct.ScmObj* %argslist60414$anf_45bind483452, %struct.ScmObj** %stackaddr$prim61713, align 8
%clofunc61714 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48345)
musttail call tailcc void %clofunc61714(%struct.ScmObj* %anf_45bind48345, %struct.ScmObj* %argslist60414$anf_45bind483452)
ret void
}

define tailcc void @proc_clo$ae52830(%struct.ScmObj* %env$ae52830,%struct.ScmObj* %current_45args60406) {
%stackaddr$env-ref61715 = alloca %struct.ScmObj*, align 8
%k48408 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52830, i64 0)
store %struct.ScmObj* %k48408, %struct.ScmObj** %stackaddr$env-ref61715
%stackaddr$env-ref61716 = alloca %struct.ScmObj*, align 8
%anf_45bind48344 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52830, i64 1)
store %struct.ScmObj* %anf_45bind48344, %struct.ScmObj** %stackaddr$env-ref61716
%stackaddr$env-ref61717 = alloca %struct.ScmObj*, align 8
%anf_45bind48343 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52830, i64 2)
store %struct.ScmObj* %anf_45bind48343, %struct.ScmObj** %stackaddr$env-ref61717
%stackaddr$env-ref61718 = alloca %struct.ScmObj*, align 8
%c48162 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52830, i64 3)
store %struct.ScmObj* %c48162, %struct.ScmObj** %stackaddr$env-ref61718
%stackaddr$prim61719 = alloca %struct.ScmObj*, align 8
%_95k48409 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60406)
store volatile %struct.ScmObj* %_95k48409, %struct.ScmObj** %stackaddr$prim61719, align 8
%stackaddr$prim61720 = alloca %struct.ScmObj*, align 8
%current_45args60407 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60406)
store volatile %struct.ScmObj* %current_45args60407, %struct.ScmObj** %stackaddr$prim61720, align 8
%stackaddr$prim61721 = alloca %struct.ScmObj*, align 8
%anf_45bind48346 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60407)
store volatile %struct.ScmObj* %anf_45bind48346, %struct.ScmObj** %stackaddr$prim61721, align 8
%stackaddr$makeclosure61722 = alloca %struct.ScmObj*, align 8
%fptrToInt61723 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52833 to i64
%ae52833 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt61723)
store volatile %struct.ScmObj* %ae52833, %struct.ScmObj** %stackaddr$makeclosure61722, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52833, %struct.ScmObj* %k48408, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52833, %struct.ScmObj* %anf_45bind48343, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52833, %struct.ScmObj* %c48162, i64 2)
%argslist60413$anf_45bind483440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61724 = alloca %struct.ScmObj*, align 8
%argslist60413$anf_45bind483441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48346, %struct.ScmObj* %argslist60413$anf_45bind483440)
store volatile %struct.ScmObj* %argslist60413$anf_45bind483441, %struct.ScmObj** %stackaddr$prim61724, align 8
%stackaddr$prim61725 = alloca %struct.ScmObj*, align 8
%argslist60413$anf_45bind483442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52833, %struct.ScmObj* %argslist60413$anf_45bind483441)
store volatile %struct.ScmObj* %argslist60413$anf_45bind483442, %struct.ScmObj** %stackaddr$prim61725, align 8
%clofunc61726 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48344)
musttail call tailcc void %clofunc61726(%struct.ScmObj* %anf_45bind48344, %struct.ScmObj* %argslist60413$anf_45bind483442)
ret void
}

define tailcc void @proc_clo$ae52833(%struct.ScmObj* %env$ae52833,%struct.ScmObj* %current_45args60409) {
%stackaddr$env-ref61727 = alloca %struct.ScmObj*, align 8
%k48408 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52833, i64 0)
store %struct.ScmObj* %k48408, %struct.ScmObj** %stackaddr$env-ref61727
%stackaddr$env-ref61728 = alloca %struct.ScmObj*, align 8
%anf_45bind48343 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52833, i64 1)
store %struct.ScmObj* %anf_45bind48343, %struct.ScmObj** %stackaddr$env-ref61728
%stackaddr$env-ref61729 = alloca %struct.ScmObj*, align 8
%c48162 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52833, i64 2)
store %struct.ScmObj* %c48162, %struct.ScmObj** %stackaddr$env-ref61729
%stackaddr$prim61730 = alloca %struct.ScmObj*, align 8
%_95k48410 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60409)
store volatile %struct.ScmObj* %_95k48410, %struct.ScmObj** %stackaddr$prim61730, align 8
%stackaddr$prim61731 = alloca %struct.ScmObj*, align 8
%current_45args60410 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60409)
store volatile %struct.ScmObj* %current_45args60410, %struct.ScmObj** %stackaddr$prim61731, align 8
%stackaddr$prim61732 = alloca %struct.ScmObj*, align 8
%anf_45bind48347 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60410)
store volatile %struct.ScmObj* %anf_45bind48347, %struct.ScmObj** %stackaddr$prim61732, align 8
%argslist60412$anf_45bind483430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61733 = alloca %struct.ScmObj*, align 8
%argslist60412$anf_45bind483431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %c48162, %struct.ScmObj* %argslist60412$anf_45bind483430)
store volatile %struct.ScmObj* %argslist60412$anf_45bind483431, %struct.ScmObj** %stackaddr$prim61733, align 8
%stackaddr$prim61734 = alloca %struct.ScmObj*, align 8
%argslist60412$anf_45bind483432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48347, %struct.ScmObj* %argslist60412$anf_45bind483431)
store volatile %struct.ScmObj* %argslist60412$anf_45bind483432, %struct.ScmObj** %stackaddr$prim61734, align 8
%stackaddr$prim61735 = alloca %struct.ScmObj*, align 8
%argslist60412$anf_45bind483433 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48408, %struct.ScmObj* %argslist60412$anf_45bind483432)
store volatile %struct.ScmObj* %argslist60412$anf_45bind483433, %struct.ScmObj** %stackaddr$prim61735, align 8
%clofunc61736 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48343)
musttail call tailcc void %clofunc61736(%struct.ScmObj* %anf_45bind48343, %struct.ScmObj* %argslist60412$anf_45bind483433)
ret void
}

define tailcc void @proc_clo$ae52684(%struct.ScmObj* %env$ae52684,%struct.ScmObj* %current_45args60416) {
%stackaddr$env-ref61737 = alloca %struct.ScmObj*, align 8
%fibc48151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52684, i64 0)
store %struct.ScmObj* %fibc48151, %struct.ScmObj** %stackaddr$env-ref61737
%stackaddr$env-ref61738 = alloca %struct.ScmObj*, align 8
%anf_45bind48337 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52684, i64 1)
store %struct.ScmObj* %anf_45bind48337, %struct.ScmObj** %stackaddr$env-ref61738
%stackaddr$env-ref61739 = alloca %struct.ScmObj*, align 8
%x48160 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52684, i64 2)
store %struct.ScmObj* %x48160, %struct.ScmObj** %stackaddr$env-ref61739
%stackaddr$env-ref61740 = alloca %struct.ScmObj*, align 8
%c48159 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52684, i64 3)
store %struct.ScmObj* %c48159, %struct.ScmObj** %stackaddr$env-ref61740
%stackaddr$env-ref61741 = alloca %struct.ScmObj*, align 8
%k48398 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52684, i64 4)
store %struct.ScmObj* %k48398, %struct.ScmObj** %stackaddr$env-ref61741
%stackaddr$env-ref61742 = alloca %struct.ScmObj*, align 8
%pred48154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52684, i64 5)
store %struct.ScmObj* %pred48154, %struct.ScmObj** %stackaddr$env-ref61742
%stackaddr$prim61743 = alloca %struct.ScmObj*, align 8
%_95k48405 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60416)
store volatile %struct.ScmObj* %_95k48405, %struct.ScmObj** %stackaddr$prim61743, align 8
%stackaddr$prim61744 = alloca %struct.ScmObj*, align 8
%current_45args60417 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60416)
store volatile %struct.ScmObj* %current_45args60417, %struct.ScmObj** %stackaddr$prim61744, align 8
%stackaddr$prim61745 = alloca %struct.ScmObj*, align 8
%anf_45bind48342 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60417)
store volatile %struct.ScmObj* %anf_45bind48342, %struct.ScmObj** %stackaddr$prim61745, align 8
%stackaddr$makeclosure61746 = alloca %struct.ScmObj*, align 8
%fptrToInt61747 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52685 to i64
%ae52685 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt61747)
store volatile %struct.ScmObj* %ae52685, %struct.ScmObj** %stackaddr$makeclosure61746, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52685, %struct.ScmObj* %anf_45bind48342, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52685, %struct.ScmObj* %anf_45bind48337, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52685, %struct.ScmObj* %c48159, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52685, %struct.ScmObj* %k48398, i64 3)
%ae52686 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61748 = alloca %struct.ScmObj*, align 8
%fptrToInt61749 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52687 to i64
%ae52687 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt61749)
store volatile %struct.ScmObj* %ae52687, %struct.ScmObj** %stackaddr$makeclosure61748, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52687, %struct.ScmObj* %fibc48151, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52687, %struct.ScmObj* %x48160, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52687, %struct.ScmObj* %pred48154, i64 2)
%argslist60443$ae526850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61750 = alloca %struct.ScmObj*, align 8
%argslist60443$ae526851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52687, %struct.ScmObj* %argslist60443$ae526850)
store volatile %struct.ScmObj* %argslist60443$ae526851, %struct.ScmObj** %stackaddr$prim61750, align 8
%stackaddr$prim61751 = alloca %struct.ScmObj*, align 8
%argslist60443$ae526852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52686, %struct.ScmObj* %argslist60443$ae526851)
store volatile %struct.ScmObj* %argslist60443$ae526852, %struct.ScmObj** %stackaddr$prim61751, align 8
%clofunc61752 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52685)
musttail call tailcc void %clofunc61752(%struct.ScmObj* %ae52685, %struct.ScmObj* %argslist60443$ae526852)
ret void
}

define tailcc void @proc_clo$ae52685(%struct.ScmObj* %env$ae52685,%struct.ScmObj* %current_45args60419) {
%stackaddr$env-ref61753 = alloca %struct.ScmObj*, align 8
%anf_45bind48342 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52685, i64 0)
store %struct.ScmObj* %anf_45bind48342, %struct.ScmObj** %stackaddr$env-ref61753
%stackaddr$env-ref61754 = alloca %struct.ScmObj*, align 8
%anf_45bind48337 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52685, i64 1)
store %struct.ScmObj* %anf_45bind48337, %struct.ScmObj** %stackaddr$env-ref61754
%stackaddr$env-ref61755 = alloca %struct.ScmObj*, align 8
%c48159 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52685, i64 2)
store %struct.ScmObj* %c48159, %struct.ScmObj** %stackaddr$env-ref61755
%stackaddr$env-ref61756 = alloca %struct.ScmObj*, align 8
%k48398 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52685, i64 3)
store %struct.ScmObj* %k48398, %struct.ScmObj** %stackaddr$env-ref61756
%stackaddr$prim61757 = alloca %struct.ScmObj*, align 8
%_95k48406 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60419)
store volatile %struct.ScmObj* %_95k48406, %struct.ScmObj** %stackaddr$prim61757, align 8
%stackaddr$prim61758 = alloca %struct.ScmObj*, align 8
%current_45args60420 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60419)
store volatile %struct.ScmObj* %current_45args60420, %struct.ScmObj** %stackaddr$prim61758, align 8
%stackaddr$prim61759 = alloca %struct.ScmObj*, align 8
%anf_45bind48348 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60420)
store volatile %struct.ScmObj* %anf_45bind48348, %struct.ScmObj** %stackaddr$prim61759, align 8
%stackaddr$makeclosure61760 = alloca %struct.ScmObj*, align 8
%fptrToInt61761 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52775 to i64
%ae52775 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt61761)
store volatile %struct.ScmObj* %ae52775, %struct.ScmObj** %stackaddr$makeclosure61760, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52775, %struct.ScmObj* %anf_45bind48342, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52775, %struct.ScmObj* %anf_45bind48337, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52775, %struct.ScmObj* %c48159, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52775, %struct.ScmObj* %k48398, i64 3)
%stackaddr$makeclosure61762 = alloca %struct.ScmObj*, align 8
%fptrToInt61763 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52776 to i64
%ae52776 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt61763)
store volatile %struct.ScmObj* %ae52776, %struct.ScmObj** %stackaddr$makeclosure61762, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52776, %struct.ScmObj* %anf_45bind48342, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52776, %struct.ScmObj* %anf_45bind48337, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52776, %struct.ScmObj* %c48159, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52776, %struct.ScmObj* %k48398, i64 3)
%argslist60430$anf_45bind483480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61764 = alloca %struct.ScmObj*, align 8
%argslist60430$anf_45bind483481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52776, %struct.ScmObj* %argslist60430$anf_45bind483480)
store volatile %struct.ScmObj* %argslist60430$anf_45bind483481, %struct.ScmObj** %stackaddr$prim61764, align 8
%stackaddr$prim61765 = alloca %struct.ScmObj*, align 8
%argslist60430$anf_45bind483482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52775, %struct.ScmObj* %argslist60430$anf_45bind483481)
store volatile %struct.ScmObj* %argslist60430$anf_45bind483482, %struct.ScmObj** %stackaddr$prim61765, align 8
%clofunc61766 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48348)
musttail call tailcc void %clofunc61766(%struct.ScmObj* %anf_45bind48348, %struct.ScmObj* %argslist60430$anf_45bind483482)
ret void
}

define tailcc void @proc_clo$ae52775(%struct.ScmObj* %env$ae52775,%struct.ScmObj* %current_45args60422) {
%stackaddr$env-ref61767 = alloca %struct.ScmObj*, align 8
%anf_45bind48342 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52775, i64 0)
store %struct.ScmObj* %anf_45bind48342, %struct.ScmObj** %stackaddr$env-ref61767
%stackaddr$env-ref61768 = alloca %struct.ScmObj*, align 8
%anf_45bind48337 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52775, i64 1)
store %struct.ScmObj* %anf_45bind48337, %struct.ScmObj** %stackaddr$env-ref61768
%stackaddr$env-ref61769 = alloca %struct.ScmObj*, align 8
%c48159 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52775, i64 2)
store %struct.ScmObj* %c48159, %struct.ScmObj** %stackaddr$env-ref61769
%stackaddr$env-ref61770 = alloca %struct.ScmObj*, align 8
%k48398 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52775, i64 3)
store %struct.ScmObj* %k48398, %struct.ScmObj** %stackaddr$env-ref61770
%stackaddr$prim61771 = alloca %struct.ScmObj*, align 8
%_95k48407 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60422)
store volatile %struct.ScmObj* %_95k48407, %struct.ScmObj** %stackaddr$prim61771, align 8
%stackaddr$prim61772 = alloca %struct.ScmObj*, align 8
%current_45args60423 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60422)
store volatile %struct.ScmObj* %current_45args60423, %struct.ScmObj** %stackaddr$prim61772, align 8
%stackaddr$prim61773 = alloca %struct.ScmObj*, align 8
%anf_45bind48349 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60423)
store volatile %struct.ScmObj* %anf_45bind48349, %struct.ScmObj** %stackaddr$prim61773, align 8
%argslist60425$anf_45bind483370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61774 = alloca %struct.ScmObj*, align 8
%argslist60425$anf_45bind483371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %c48159, %struct.ScmObj* %argslist60425$anf_45bind483370)
store volatile %struct.ScmObj* %argslist60425$anf_45bind483371, %struct.ScmObj** %stackaddr$prim61774, align 8
%stackaddr$prim61775 = alloca %struct.ScmObj*, align 8
%argslist60425$anf_45bind483372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48349, %struct.ScmObj* %argslist60425$anf_45bind483371)
store volatile %struct.ScmObj* %argslist60425$anf_45bind483372, %struct.ScmObj** %stackaddr$prim61775, align 8
%stackaddr$prim61776 = alloca %struct.ScmObj*, align 8
%argslist60425$anf_45bind483373 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48342, %struct.ScmObj* %argslist60425$anf_45bind483372)
store volatile %struct.ScmObj* %argslist60425$anf_45bind483373, %struct.ScmObj** %stackaddr$prim61776, align 8
%stackaddr$prim61777 = alloca %struct.ScmObj*, align 8
%argslist60425$anf_45bind483374 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48398, %struct.ScmObj* %argslist60425$anf_45bind483373)
store volatile %struct.ScmObj* %argslist60425$anf_45bind483374, %struct.ScmObj** %stackaddr$prim61777, align 8
%clofunc61778 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48337)
musttail call tailcc void %clofunc61778(%struct.ScmObj* %anf_45bind48337, %struct.ScmObj* %argslist60425$anf_45bind483374)
ret void
}

define tailcc void @proc_clo$ae52776(%struct.ScmObj* %env$ae52776,%struct.ScmObj* %current_45args60426) {
%stackaddr$env-ref61779 = alloca %struct.ScmObj*, align 8
%anf_45bind48342 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52776, i64 0)
store %struct.ScmObj* %anf_45bind48342, %struct.ScmObj** %stackaddr$env-ref61779
%stackaddr$env-ref61780 = alloca %struct.ScmObj*, align 8
%anf_45bind48337 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52776, i64 1)
store %struct.ScmObj* %anf_45bind48337, %struct.ScmObj** %stackaddr$env-ref61780
%stackaddr$env-ref61781 = alloca %struct.ScmObj*, align 8
%c48159 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52776, i64 2)
store %struct.ScmObj* %c48159, %struct.ScmObj** %stackaddr$env-ref61781
%stackaddr$env-ref61782 = alloca %struct.ScmObj*, align 8
%k48398 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52776, i64 3)
store %struct.ScmObj* %k48398, %struct.ScmObj** %stackaddr$env-ref61782
%stackaddr$prim61783 = alloca %struct.ScmObj*, align 8
%_95k48407 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60426)
store volatile %struct.ScmObj* %_95k48407, %struct.ScmObj** %stackaddr$prim61783, align 8
%stackaddr$prim61784 = alloca %struct.ScmObj*, align 8
%current_45args60427 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60426)
store volatile %struct.ScmObj* %current_45args60427, %struct.ScmObj** %stackaddr$prim61784, align 8
%stackaddr$prim61785 = alloca %struct.ScmObj*, align 8
%anf_45bind48349 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60427)
store volatile %struct.ScmObj* %anf_45bind48349, %struct.ScmObj** %stackaddr$prim61785, align 8
%argslist60429$anf_45bind483370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61786 = alloca %struct.ScmObj*, align 8
%argslist60429$anf_45bind483371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %c48159, %struct.ScmObj* %argslist60429$anf_45bind483370)
store volatile %struct.ScmObj* %argslist60429$anf_45bind483371, %struct.ScmObj** %stackaddr$prim61786, align 8
%stackaddr$prim61787 = alloca %struct.ScmObj*, align 8
%argslist60429$anf_45bind483372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48349, %struct.ScmObj* %argslist60429$anf_45bind483371)
store volatile %struct.ScmObj* %argslist60429$anf_45bind483372, %struct.ScmObj** %stackaddr$prim61787, align 8
%stackaddr$prim61788 = alloca %struct.ScmObj*, align 8
%argslist60429$anf_45bind483373 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48342, %struct.ScmObj* %argslist60429$anf_45bind483372)
store volatile %struct.ScmObj* %argslist60429$anf_45bind483373, %struct.ScmObj** %stackaddr$prim61788, align 8
%stackaddr$prim61789 = alloca %struct.ScmObj*, align 8
%argslist60429$anf_45bind483374 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48398, %struct.ScmObj* %argslist60429$anf_45bind483373)
store volatile %struct.ScmObj* %argslist60429$anf_45bind483374, %struct.ScmObj** %stackaddr$prim61789, align 8
%clofunc61790 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48337)
musttail call tailcc void %clofunc61790(%struct.ScmObj* %anf_45bind48337, %struct.ScmObj* %argslist60429$anf_45bind483374)
ret void
}

define tailcc void @proc_clo$ae52687(%struct.ScmObj* %env$ae52687,%struct.ScmObj* %current_45args60431) {
%stackaddr$env-ref61791 = alloca %struct.ScmObj*, align 8
%fibc48151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52687, i64 0)
store %struct.ScmObj* %fibc48151, %struct.ScmObj** %stackaddr$env-ref61791
%stackaddr$env-ref61792 = alloca %struct.ScmObj*, align 8
%x48160 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52687, i64 1)
store %struct.ScmObj* %x48160, %struct.ScmObj** %stackaddr$env-ref61792
%stackaddr$env-ref61793 = alloca %struct.ScmObj*, align 8
%pred48154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52687, i64 2)
store %struct.ScmObj* %pred48154, %struct.ScmObj** %stackaddr$env-ref61793
%stackaddr$prim61794 = alloca %struct.ScmObj*, align 8
%k48408 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60431)
store volatile %struct.ScmObj* %k48408, %struct.ScmObj** %stackaddr$prim61794, align 8
%stackaddr$prim61795 = alloca %struct.ScmObj*, align 8
%current_45args60432 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60431)
store volatile %struct.ScmObj* %current_45args60432, %struct.ScmObj** %stackaddr$prim61795, align 8
%stackaddr$prim61796 = alloca %struct.ScmObj*, align 8
%c48162 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60432)
store volatile %struct.ScmObj* %c48162, %struct.ScmObj** %stackaddr$prim61796, align 8
%ae52689 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61797 = alloca %struct.ScmObj*, align 8
%anf_45bind48343 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %fibc48151, %struct.ScmObj* %ae52689)
store volatile %struct.ScmObj* %anf_45bind48343, %struct.ScmObj** %stackaddr$prim61797, align 8
%ae52691 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61798 = alloca %struct.ScmObj*, align 8
%anf_45bind48344 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %pred48154, %struct.ScmObj* %ae52691)
store volatile %struct.ScmObj* %anf_45bind48344, %struct.ScmObj** %stackaddr$prim61798, align 8
%ae52693 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61799 = alloca %struct.ScmObj*, align 8
%anf_45bind48345 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %pred48154, %struct.ScmObj* %ae52693)
store volatile %struct.ScmObj* %anf_45bind48345, %struct.ScmObj** %stackaddr$prim61799, align 8
%stackaddr$makeclosure61800 = alloca %struct.ScmObj*, align 8
%fptrToInt61801 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52695 to i64
%ae52695 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt61801)
store volatile %struct.ScmObj* %ae52695, %struct.ScmObj** %stackaddr$makeclosure61800, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52695, %struct.ScmObj* %k48408, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52695, %struct.ScmObj* %anf_45bind48344, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52695, %struct.ScmObj* %anf_45bind48343, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52695, %struct.ScmObj* %c48162, i64 3)
%argslist60442$anf_45bind483450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61802 = alloca %struct.ScmObj*, align 8
%argslist60442$anf_45bind483451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x48160, %struct.ScmObj* %argslist60442$anf_45bind483450)
store volatile %struct.ScmObj* %argslist60442$anf_45bind483451, %struct.ScmObj** %stackaddr$prim61802, align 8
%stackaddr$prim61803 = alloca %struct.ScmObj*, align 8
%argslist60442$anf_45bind483452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52695, %struct.ScmObj* %argslist60442$anf_45bind483451)
store volatile %struct.ScmObj* %argslist60442$anf_45bind483452, %struct.ScmObj** %stackaddr$prim61803, align 8
%clofunc61804 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48345)
musttail call tailcc void %clofunc61804(%struct.ScmObj* %anf_45bind48345, %struct.ScmObj* %argslist60442$anf_45bind483452)
ret void
}

define tailcc void @proc_clo$ae52695(%struct.ScmObj* %env$ae52695,%struct.ScmObj* %current_45args60434) {
%stackaddr$env-ref61805 = alloca %struct.ScmObj*, align 8
%k48408 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52695, i64 0)
store %struct.ScmObj* %k48408, %struct.ScmObj** %stackaddr$env-ref61805
%stackaddr$env-ref61806 = alloca %struct.ScmObj*, align 8
%anf_45bind48344 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52695, i64 1)
store %struct.ScmObj* %anf_45bind48344, %struct.ScmObj** %stackaddr$env-ref61806
%stackaddr$env-ref61807 = alloca %struct.ScmObj*, align 8
%anf_45bind48343 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52695, i64 2)
store %struct.ScmObj* %anf_45bind48343, %struct.ScmObj** %stackaddr$env-ref61807
%stackaddr$env-ref61808 = alloca %struct.ScmObj*, align 8
%c48162 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52695, i64 3)
store %struct.ScmObj* %c48162, %struct.ScmObj** %stackaddr$env-ref61808
%stackaddr$prim61809 = alloca %struct.ScmObj*, align 8
%_95k48409 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60434)
store volatile %struct.ScmObj* %_95k48409, %struct.ScmObj** %stackaddr$prim61809, align 8
%stackaddr$prim61810 = alloca %struct.ScmObj*, align 8
%current_45args60435 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60434)
store volatile %struct.ScmObj* %current_45args60435, %struct.ScmObj** %stackaddr$prim61810, align 8
%stackaddr$prim61811 = alloca %struct.ScmObj*, align 8
%anf_45bind48346 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60435)
store volatile %struct.ScmObj* %anf_45bind48346, %struct.ScmObj** %stackaddr$prim61811, align 8
%stackaddr$makeclosure61812 = alloca %struct.ScmObj*, align 8
%fptrToInt61813 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52698 to i64
%ae52698 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt61813)
store volatile %struct.ScmObj* %ae52698, %struct.ScmObj** %stackaddr$makeclosure61812, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52698, %struct.ScmObj* %k48408, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52698, %struct.ScmObj* %anf_45bind48343, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52698, %struct.ScmObj* %c48162, i64 2)
%argslist60441$anf_45bind483440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61814 = alloca %struct.ScmObj*, align 8
%argslist60441$anf_45bind483441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48346, %struct.ScmObj* %argslist60441$anf_45bind483440)
store volatile %struct.ScmObj* %argslist60441$anf_45bind483441, %struct.ScmObj** %stackaddr$prim61814, align 8
%stackaddr$prim61815 = alloca %struct.ScmObj*, align 8
%argslist60441$anf_45bind483442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52698, %struct.ScmObj* %argslist60441$anf_45bind483441)
store volatile %struct.ScmObj* %argslist60441$anf_45bind483442, %struct.ScmObj** %stackaddr$prim61815, align 8
%clofunc61816 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48344)
musttail call tailcc void %clofunc61816(%struct.ScmObj* %anf_45bind48344, %struct.ScmObj* %argslist60441$anf_45bind483442)
ret void
}

define tailcc void @proc_clo$ae52698(%struct.ScmObj* %env$ae52698,%struct.ScmObj* %current_45args60437) {
%stackaddr$env-ref61817 = alloca %struct.ScmObj*, align 8
%k48408 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52698, i64 0)
store %struct.ScmObj* %k48408, %struct.ScmObj** %stackaddr$env-ref61817
%stackaddr$env-ref61818 = alloca %struct.ScmObj*, align 8
%anf_45bind48343 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52698, i64 1)
store %struct.ScmObj* %anf_45bind48343, %struct.ScmObj** %stackaddr$env-ref61818
%stackaddr$env-ref61819 = alloca %struct.ScmObj*, align 8
%c48162 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52698, i64 2)
store %struct.ScmObj* %c48162, %struct.ScmObj** %stackaddr$env-ref61819
%stackaddr$prim61820 = alloca %struct.ScmObj*, align 8
%_95k48410 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60437)
store volatile %struct.ScmObj* %_95k48410, %struct.ScmObj** %stackaddr$prim61820, align 8
%stackaddr$prim61821 = alloca %struct.ScmObj*, align 8
%current_45args60438 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60437)
store volatile %struct.ScmObj* %current_45args60438, %struct.ScmObj** %stackaddr$prim61821, align 8
%stackaddr$prim61822 = alloca %struct.ScmObj*, align 8
%anf_45bind48347 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60438)
store volatile %struct.ScmObj* %anf_45bind48347, %struct.ScmObj** %stackaddr$prim61822, align 8
%argslist60440$anf_45bind483430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61823 = alloca %struct.ScmObj*, align 8
%argslist60440$anf_45bind483431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %c48162, %struct.ScmObj* %argslist60440$anf_45bind483430)
store volatile %struct.ScmObj* %argslist60440$anf_45bind483431, %struct.ScmObj** %stackaddr$prim61823, align 8
%stackaddr$prim61824 = alloca %struct.ScmObj*, align 8
%argslist60440$anf_45bind483432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48347, %struct.ScmObj* %argslist60440$anf_45bind483431)
store volatile %struct.ScmObj* %argslist60440$anf_45bind483432, %struct.ScmObj** %stackaddr$prim61824, align 8
%stackaddr$prim61825 = alloca %struct.ScmObj*, align 8
%argslist60440$anf_45bind483433 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48408, %struct.ScmObj* %argslist60440$anf_45bind483432)
store volatile %struct.ScmObj* %argslist60440$anf_45bind483433, %struct.ScmObj** %stackaddr$prim61825, align 8
%clofunc61826 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48343)
musttail call tailcc void %clofunc61826(%struct.ScmObj* %anf_45bind48343, %struct.ScmObj* %argslist60440$anf_45bind483433)
ret void
}

define tailcc void @proc_clo$ae52630(%struct.ScmObj* %env$ae52630,%struct.ScmObj* %current_45args60445) {
%stackaddr$env-ref61827 = alloca %struct.ScmObj*, align 8
%fibc48151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52630, i64 0)
store %struct.ScmObj* %fibc48151, %struct.ScmObj** %stackaddr$env-ref61827
%stackaddr$env-ref61828 = alloca %struct.ScmObj*, align 8
%x48160 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52630, i64 1)
store %struct.ScmObj* %x48160, %struct.ScmObj** %stackaddr$env-ref61828
%stackaddr$env-ref61829 = alloca %struct.ScmObj*, align 8
%pred48154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52630, i64 2)
store %struct.ScmObj* %pred48154, %struct.ScmObj** %stackaddr$env-ref61829
%stackaddr$prim61830 = alloca %struct.ScmObj*, align 8
%k48411 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60445)
store volatile %struct.ScmObj* %k48411, %struct.ScmObj** %stackaddr$prim61830, align 8
%stackaddr$prim61831 = alloca %struct.ScmObj*, align 8
%current_45args60446 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60445)
store volatile %struct.ScmObj* %current_45args60446, %struct.ScmObj** %stackaddr$prim61831, align 8
%stackaddr$prim61832 = alloca %struct.ScmObj*, align 8
%c48161 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60446)
store volatile %struct.ScmObj* %c48161, %struct.ScmObj** %stackaddr$prim61832, align 8
%ae52632 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61833 = alloca %struct.ScmObj*, align 8
%anf_45bind48338 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %fibc48151, %struct.ScmObj* %ae52632)
store volatile %struct.ScmObj* %anf_45bind48338, %struct.ScmObj** %stackaddr$prim61833, align 8
%ae52634 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61834 = alloca %struct.ScmObj*, align 8
%anf_45bind48339 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %pred48154, %struct.ScmObj* %ae52634)
store volatile %struct.ScmObj* %anf_45bind48339, %struct.ScmObj** %stackaddr$prim61834, align 8
%stackaddr$makeclosure61835 = alloca %struct.ScmObj*, align 8
%fptrToInt61836 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52636 to i64
%ae52636 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt61836)
store volatile %struct.ScmObj* %ae52636, %struct.ScmObj** %stackaddr$makeclosure61835, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52636, %struct.ScmObj* %anf_45bind48338, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52636, %struct.ScmObj* %c48161, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52636, %struct.ScmObj* %k48411, i64 2)
%argslist60452$anf_45bind483390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61837 = alloca %struct.ScmObj*, align 8
%argslist60452$anf_45bind483391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x48160, %struct.ScmObj* %argslist60452$anf_45bind483390)
store volatile %struct.ScmObj* %argslist60452$anf_45bind483391, %struct.ScmObj** %stackaddr$prim61837, align 8
%stackaddr$prim61838 = alloca %struct.ScmObj*, align 8
%argslist60452$anf_45bind483392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52636, %struct.ScmObj* %argslist60452$anf_45bind483391)
store volatile %struct.ScmObj* %argslist60452$anf_45bind483392, %struct.ScmObj** %stackaddr$prim61838, align 8
%clofunc61839 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48339)
musttail call tailcc void %clofunc61839(%struct.ScmObj* %anf_45bind48339, %struct.ScmObj* %argslist60452$anf_45bind483392)
ret void
}

define tailcc void @proc_clo$ae52636(%struct.ScmObj* %env$ae52636,%struct.ScmObj* %current_45args60448) {
%stackaddr$env-ref61840 = alloca %struct.ScmObj*, align 8
%anf_45bind48338 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52636, i64 0)
store %struct.ScmObj* %anf_45bind48338, %struct.ScmObj** %stackaddr$env-ref61840
%stackaddr$env-ref61841 = alloca %struct.ScmObj*, align 8
%c48161 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52636, i64 1)
store %struct.ScmObj* %c48161, %struct.ScmObj** %stackaddr$env-ref61841
%stackaddr$env-ref61842 = alloca %struct.ScmObj*, align 8
%k48411 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52636, i64 2)
store %struct.ScmObj* %k48411, %struct.ScmObj** %stackaddr$env-ref61842
%stackaddr$prim61843 = alloca %struct.ScmObj*, align 8
%_95k48412 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60448)
store volatile %struct.ScmObj* %_95k48412, %struct.ScmObj** %stackaddr$prim61843, align 8
%stackaddr$prim61844 = alloca %struct.ScmObj*, align 8
%current_45args60449 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60448)
store volatile %struct.ScmObj* %current_45args60449, %struct.ScmObj** %stackaddr$prim61844, align 8
%stackaddr$prim61845 = alloca %struct.ScmObj*, align 8
%anf_45bind48340 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60449)
store volatile %struct.ScmObj* %anf_45bind48340, %struct.ScmObj** %stackaddr$prim61845, align 8
%argslist60451$anf_45bind483380 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61846 = alloca %struct.ScmObj*, align 8
%argslist60451$anf_45bind483381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %c48161, %struct.ScmObj* %argslist60451$anf_45bind483380)
store volatile %struct.ScmObj* %argslist60451$anf_45bind483381, %struct.ScmObj** %stackaddr$prim61846, align 8
%stackaddr$prim61847 = alloca %struct.ScmObj*, align 8
%argslist60451$anf_45bind483382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48340, %struct.ScmObj* %argslist60451$anf_45bind483381)
store volatile %struct.ScmObj* %argslist60451$anf_45bind483382, %struct.ScmObj** %stackaddr$prim61847, align 8
%stackaddr$prim61848 = alloca %struct.ScmObj*, align 8
%argslist60451$anf_45bind483383 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48411, %struct.ScmObj* %argslist60451$anf_45bind483382)
store volatile %struct.ScmObj* %argslist60451$anf_45bind483383, %struct.ScmObj** %stackaddr$prim61848, align 8
%clofunc61849 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48338)
musttail call tailcc void %clofunc61849(%struct.ScmObj* %anf_45bind48338, %struct.ScmObj* %argslist60451$anf_45bind483383)
ret void
}

define tailcc void @proc_clo$ae52423(%struct.ScmObj* %env$ae52423,%struct.ScmObj* %current_45args60458) {
%stackaddr$env-ref61850 = alloca %struct.ScmObj*, align 8
%z_6348153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52423, i64 0)
store %struct.ScmObj* %z_6348153, %struct.ScmObj** %stackaddr$env-ref61850
%stackaddr$env-ref61851 = alloca %struct.ScmObj*, align 8
%addc48152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52423, i64 1)
store %struct.ScmObj* %addc48152, %struct.ScmObj** %stackaddr$env-ref61851
%stackaddr$env-ref61852 = alloca %struct.ScmObj*, align 8
%succ48155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52423, i64 2)
store %struct.ScmObj* %succ48155, %struct.ScmObj** %stackaddr$env-ref61852
%stackaddr$env-ref61853 = alloca %struct.ScmObj*, align 8
%pred48154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52423, i64 3)
store %struct.ScmObj* %pred48154, %struct.ScmObj** %stackaddr$env-ref61853
%stackaddr$prim61854 = alloca %struct.ScmObj*, align 8
%k48413 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60458)
store volatile %struct.ScmObj* %k48413, %struct.ScmObj** %stackaddr$prim61854, align 8
%stackaddr$prim61855 = alloca %struct.ScmObj*, align 8
%current_45args60459 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60458)
store volatile %struct.ScmObj* %current_45args60459, %struct.ScmObj** %stackaddr$prim61855, align 8
%stackaddr$prim61856 = alloca %struct.ScmObj*, align 8
%x48166 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60459)
store volatile %struct.ScmObj* %x48166, %struct.ScmObj** %stackaddr$prim61856, align 8
%stackaddr$prim61857 = alloca %struct.ScmObj*, align 8
%current_45args60460 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60459)
store volatile %struct.ScmObj* %current_45args60460, %struct.ScmObj** %stackaddr$prim61857, align 8
%stackaddr$prim61858 = alloca %struct.ScmObj*, align 8
%y48165 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60460)
store volatile %struct.ScmObj* %y48165, %struct.ScmObj** %stackaddr$prim61858, align 8
%stackaddr$prim61859 = alloca %struct.ScmObj*, align 8
%current_45args60461 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60460)
store volatile %struct.ScmObj* %current_45args60461, %struct.ScmObj** %stackaddr$prim61859, align 8
%stackaddr$prim61860 = alloca %struct.ScmObj*, align 8
%k48164 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60461)
store volatile %struct.ScmObj* %k48164, %struct.ScmObj** %stackaddr$prim61860, align 8
%ae52425 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61861 = alloca %struct.ScmObj*, align 8
%anf_45bind48319 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %z_6348153, %struct.ScmObj* %ae52425)
store volatile %struct.ScmObj* %anf_45bind48319, %struct.ScmObj** %stackaddr$prim61861, align 8
%stackaddr$makeclosure61862 = alloca %struct.ScmObj*, align 8
%fptrToInt61863 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52427 to i64
%ae52427 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt61863)
store volatile %struct.ScmObj* %ae52427, %struct.ScmObj** %stackaddr$makeclosure61862, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52427, %struct.ScmObj* %addc48152, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52427, %struct.ScmObj* %x48166, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52427, %struct.ScmObj* %y48165, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52427, %struct.ScmObj* %k48164, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52427, %struct.ScmObj* %k48413, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae52427, %struct.ScmObj* %succ48155, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae52427, %struct.ScmObj* %pred48154, i64 6)
%argslist60476$anf_45bind483190 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61864 = alloca %struct.ScmObj*, align 8
%argslist60476$anf_45bind483191 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y48165, %struct.ScmObj* %argslist60476$anf_45bind483190)
store volatile %struct.ScmObj* %argslist60476$anf_45bind483191, %struct.ScmObj** %stackaddr$prim61864, align 8
%stackaddr$prim61865 = alloca %struct.ScmObj*, align 8
%argslist60476$anf_45bind483192 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52427, %struct.ScmObj* %argslist60476$anf_45bind483191)
store volatile %struct.ScmObj* %argslist60476$anf_45bind483192, %struct.ScmObj** %stackaddr$prim61865, align 8
%clofunc61866 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48319)
musttail call tailcc void %clofunc61866(%struct.ScmObj* %anf_45bind48319, %struct.ScmObj* %argslist60476$anf_45bind483192)
ret void
}

define tailcc void @proc_clo$ae52427(%struct.ScmObj* %env$ae52427,%struct.ScmObj* %current_45args60463) {
%stackaddr$env-ref61867 = alloca %struct.ScmObj*, align 8
%addc48152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52427, i64 0)
store %struct.ScmObj* %addc48152, %struct.ScmObj** %stackaddr$env-ref61867
%stackaddr$env-ref61868 = alloca %struct.ScmObj*, align 8
%x48166 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52427, i64 1)
store %struct.ScmObj* %x48166, %struct.ScmObj** %stackaddr$env-ref61868
%stackaddr$env-ref61869 = alloca %struct.ScmObj*, align 8
%y48165 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52427, i64 2)
store %struct.ScmObj* %y48165, %struct.ScmObj** %stackaddr$env-ref61869
%stackaddr$env-ref61870 = alloca %struct.ScmObj*, align 8
%k48164 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52427, i64 3)
store %struct.ScmObj* %k48164, %struct.ScmObj** %stackaddr$env-ref61870
%stackaddr$env-ref61871 = alloca %struct.ScmObj*, align 8
%k48413 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52427, i64 4)
store %struct.ScmObj* %k48413, %struct.ScmObj** %stackaddr$env-ref61871
%stackaddr$env-ref61872 = alloca %struct.ScmObj*, align 8
%succ48155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52427, i64 5)
store %struct.ScmObj* %succ48155, %struct.ScmObj** %stackaddr$env-ref61872
%stackaddr$env-ref61873 = alloca %struct.ScmObj*, align 8
%pred48154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52427, i64 6)
store %struct.ScmObj* %pred48154, %struct.ScmObj** %stackaddr$env-ref61873
%stackaddr$prim61874 = alloca %struct.ScmObj*, align 8
%_95k48414 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60463)
store volatile %struct.ScmObj* %_95k48414, %struct.ScmObj** %stackaddr$prim61874, align 8
%stackaddr$prim61875 = alloca %struct.ScmObj*, align 8
%current_45args60464 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60463)
store volatile %struct.ScmObj* %current_45args60464, %struct.ScmObj** %stackaddr$prim61875, align 8
%stackaddr$prim61876 = alloca %struct.ScmObj*, align 8
%anf_45bind48320 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60464)
store volatile %struct.ScmObj* %anf_45bind48320, %struct.ScmObj** %stackaddr$prim61876, align 8
%truthy$cmp61877 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48320)
%cmp$cmp61877 = icmp eq i64 %truthy$cmp61877, 1
br i1 %cmp$cmp61877, label %truebranch$cmp61877, label %falsebranch$cmp61877
truebranch$cmp61877:
%argslist60466$k481640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61878 = alloca %struct.ScmObj*, align 8
%argslist60466$k481641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x48166, %struct.ScmObj* %argslist60466$k481640)
store volatile %struct.ScmObj* %argslist60466$k481641, %struct.ScmObj** %stackaddr$prim61878, align 8
%stackaddr$prim61879 = alloca %struct.ScmObj*, align 8
%argslist60466$k481642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48413, %struct.ScmObj* %argslist60466$k481641)
store volatile %struct.ScmObj* %argslist60466$k481642, %struct.ScmObj** %stackaddr$prim61879, align 8
%clofunc61880 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48164)
musttail call tailcc void %clofunc61880(%struct.ScmObj* %k48164, %struct.ScmObj* %argslist60466$k481642)
ret void
falsebranch$cmp61877:
%ae52434 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61881 = alloca %struct.ScmObj*, align 8
%anf_45bind48321 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %addc48152, %struct.ScmObj* %ae52434)
store volatile %struct.ScmObj* %anf_45bind48321, %struct.ScmObj** %stackaddr$prim61881, align 8
%ae52436 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61882 = alloca %struct.ScmObj*, align 8
%anf_45bind48322 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %succ48155, %struct.ScmObj* %ae52436)
store volatile %struct.ScmObj* %anf_45bind48322, %struct.ScmObj** %stackaddr$prim61882, align 8
%stackaddr$makeclosure61883 = alloca %struct.ScmObj*, align 8
%fptrToInt61884 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52438 to i64
%ae52438 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt61884)
store volatile %struct.ScmObj* %ae52438, %struct.ScmObj** %stackaddr$makeclosure61883, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52438, %struct.ScmObj* %y48165, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52438, %struct.ScmObj* %k48164, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52438, %struct.ScmObj* %anf_45bind48321, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52438, %struct.ScmObj* %k48413, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52438, %struct.ScmObj* %pred48154, i64 4)
%argslist60475$anf_45bind483220 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61885 = alloca %struct.ScmObj*, align 8
%argslist60475$anf_45bind483221 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x48166, %struct.ScmObj* %argslist60475$anf_45bind483220)
store volatile %struct.ScmObj* %argslist60475$anf_45bind483221, %struct.ScmObj** %stackaddr$prim61885, align 8
%stackaddr$prim61886 = alloca %struct.ScmObj*, align 8
%argslist60475$anf_45bind483222 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52438, %struct.ScmObj* %argslist60475$anf_45bind483221)
store volatile %struct.ScmObj* %argslist60475$anf_45bind483222, %struct.ScmObj** %stackaddr$prim61886, align 8
%clofunc61887 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48322)
musttail call tailcc void %clofunc61887(%struct.ScmObj* %anf_45bind48322, %struct.ScmObj* %argslist60475$anf_45bind483222)
ret void
}

define tailcc void @proc_clo$ae52438(%struct.ScmObj* %env$ae52438,%struct.ScmObj* %current_45args60467) {
%stackaddr$env-ref61888 = alloca %struct.ScmObj*, align 8
%y48165 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52438, i64 0)
store %struct.ScmObj* %y48165, %struct.ScmObj** %stackaddr$env-ref61888
%stackaddr$env-ref61889 = alloca %struct.ScmObj*, align 8
%k48164 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52438, i64 1)
store %struct.ScmObj* %k48164, %struct.ScmObj** %stackaddr$env-ref61889
%stackaddr$env-ref61890 = alloca %struct.ScmObj*, align 8
%anf_45bind48321 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52438, i64 2)
store %struct.ScmObj* %anf_45bind48321, %struct.ScmObj** %stackaddr$env-ref61890
%stackaddr$env-ref61891 = alloca %struct.ScmObj*, align 8
%k48413 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52438, i64 3)
store %struct.ScmObj* %k48413, %struct.ScmObj** %stackaddr$env-ref61891
%stackaddr$env-ref61892 = alloca %struct.ScmObj*, align 8
%pred48154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52438, i64 4)
store %struct.ScmObj* %pred48154, %struct.ScmObj** %stackaddr$env-ref61892
%stackaddr$prim61893 = alloca %struct.ScmObj*, align 8
%_95k48415 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60467)
store volatile %struct.ScmObj* %_95k48415, %struct.ScmObj** %stackaddr$prim61893, align 8
%stackaddr$prim61894 = alloca %struct.ScmObj*, align 8
%current_45args60468 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60467)
store volatile %struct.ScmObj* %current_45args60468, %struct.ScmObj** %stackaddr$prim61894, align 8
%stackaddr$prim61895 = alloca %struct.ScmObj*, align 8
%anf_45bind48323 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60468)
store volatile %struct.ScmObj* %anf_45bind48323, %struct.ScmObj** %stackaddr$prim61895, align 8
%ae52441 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61896 = alloca %struct.ScmObj*, align 8
%anf_45bind48324 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %pred48154, %struct.ScmObj* %ae52441)
store volatile %struct.ScmObj* %anf_45bind48324, %struct.ScmObj** %stackaddr$prim61896, align 8
%stackaddr$makeclosure61897 = alloca %struct.ScmObj*, align 8
%fptrToInt61898 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52443 to i64
%ae52443 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt61898)
store volatile %struct.ScmObj* %ae52443, %struct.ScmObj** %stackaddr$makeclosure61897, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52443, %struct.ScmObj* %k48164, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52443, %struct.ScmObj* %anf_45bind48323, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52443, %struct.ScmObj* %anf_45bind48321, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52443, %struct.ScmObj* %k48413, i64 3)
%argslist60474$anf_45bind483240 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61899 = alloca %struct.ScmObj*, align 8
%argslist60474$anf_45bind483241 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y48165, %struct.ScmObj* %argslist60474$anf_45bind483240)
store volatile %struct.ScmObj* %argslist60474$anf_45bind483241, %struct.ScmObj** %stackaddr$prim61899, align 8
%stackaddr$prim61900 = alloca %struct.ScmObj*, align 8
%argslist60474$anf_45bind483242 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52443, %struct.ScmObj* %argslist60474$anf_45bind483241)
store volatile %struct.ScmObj* %argslist60474$anf_45bind483242, %struct.ScmObj** %stackaddr$prim61900, align 8
%clofunc61901 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48324)
musttail call tailcc void %clofunc61901(%struct.ScmObj* %anf_45bind48324, %struct.ScmObj* %argslist60474$anf_45bind483242)
ret void
}

define tailcc void @proc_clo$ae52443(%struct.ScmObj* %env$ae52443,%struct.ScmObj* %current_45args60470) {
%stackaddr$env-ref61902 = alloca %struct.ScmObj*, align 8
%k48164 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52443, i64 0)
store %struct.ScmObj* %k48164, %struct.ScmObj** %stackaddr$env-ref61902
%stackaddr$env-ref61903 = alloca %struct.ScmObj*, align 8
%anf_45bind48323 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52443, i64 1)
store %struct.ScmObj* %anf_45bind48323, %struct.ScmObj** %stackaddr$env-ref61903
%stackaddr$env-ref61904 = alloca %struct.ScmObj*, align 8
%anf_45bind48321 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52443, i64 2)
store %struct.ScmObj* %anf_45bind48321, %struct.ScmObj** %stackaddr$env-ref61904
%stackaddr$env-ref61905 = alloca %struct.ScmObj*, align 8
%k48413 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52443, i64 3)
store %struct.ScmObj* %k48413, %struct.ScmObj** %stackaddr$env-ref61905
%stackaddr$prim61906 = alloca %struct.ScmObj*, align 8
%_95k48416 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60470)
store volatile %struct.ScmObj* %_95k48416, %struct.ScmObj** %stackaddr$prim61906, align 8
%stackaddr$prim61907 = alloca %struct.ScmObj*, align 8
%current_45args60471 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60470)
store volatile %struct.ScmObj* %current_45args60471, %struct.ScmObj** %stackaddr$prim61907, align 8
%stackaddr$prim61908 = alloca %struct.ScmObj*, align 8
%anf_45bind48325 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60471)
store volatile %struct.ScmObj* %anf_45bind48325, %struct.ScmObj** %stackaddr$prim61908, align 8
%argslist60473$anf_45bind483210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61909 = alloca %struct.ScmObj*, align 8
%argslist60473$anf_45bind483211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48164, %struct.ScmObj* %argslist60473$anf_45bind483210)
store volatile %struct.ScmObj* %argslist60473$anf_45bind483211, %struct.ScmObj** %stackaddr$prim61909, align 8
%stackaddr$prim61910 = alloca %struct.ScmObj*, align 8
%argslist60473$anf_45bind483212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48325, %struct.ScmObj* %argslist60473$anf_45bind483211)
store volatile %struct.ScmObj* %argslist60473$anf_45bind483212, %struct.ScmObj** %stackaddr$prim61910, align 8
%stackaddr$prim61911 = alloca %struct.ScmObj*, align 8
%argslist60473$anf_45bind483213 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48323, %struct.ScmObj* %argslist60473$anf_45bind483212)
store volatile %struct.ScmObj* %argslist60473$anf_45bind483213, %struct.ScmObj** %stackaddr$prim61911, align 8
%stackaddr$prim61912 = alloca %struct.ScmObj*, align 8
%argslist60473$anf_45bind483214 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48413, %struct.ScmObj* %argslist60473$anf_45bind483213)
store volatile %struct.ScmObj* %argslist60473$anf_45bind483214, %struct.ScmObj** %stackaddr$prim61912, align 8
%clofunc61913 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48321)
musttail call tailcc void %clofunc61913(%struct.ScmObj* %anf_45bind48321, %struct.ScmObj* %argslist60473$anf_45bind483214)
ret void
}

define tailcc void @proc_clo$ae52400(%struct.ScmObj* %env$ae52400,%struct.ScmObj* %current_45args60478) {
%stackaddr$prim61914 = alloca %struct.ScmObj*, align 8
%k48417 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60478)
store volatile %struct.ScmObj* %k48417, %struct.ScmObj** %stackaddr$prim61914, align 8
%stackaddr$prim61915 = alloca %struct.ScmObj*, align 8
%current_45args60479 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60478)
store volatile %struct.ScmObj* %current_45args60479, %struct.ScmObj** %stackaddr$prim61915, align 8
%stackaddr$prim61916 = alloca %struct.ScmObj*, align 8
%n48168 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60479)
store volatile %struct.ScmObj* %n48168, %struct.ScmObj** %stackaddr$prim61916, align 8
%stackaddr$prim61917 = alloca %struct.ScmObj*, align 8
%cpsprim48418 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %n48168)
store volatile %struct.ScmObj* %cpsprim48418, %struct.ScmObj** %stackaddr$prim61917, align 8
%ae52403 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist60481$k484170 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61918 = alloca %struct.ScmObj*, align 8
%argslist60481$k484171 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48418, %struct.ScmObj* %argslist60481$k484170)
store volatile %struct.ScmObj* %argslist60481$k484171, %struct.ScmObj** %stackaddr$prim61918, align 8
%stackaddr$prim61919 = alloca %struct.ScmObj*, align 8
%argslist60481$k484172 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52403, %struct.ScmObj* %argslist60481$k484171)
store volatile %struct.ScmObj* %argslist60481$k484172, %struct.ScmObj** %stackaddr$prim61919, align 8
%clofunc61920 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48417)
musttail call tailcc void %clofunc61920(%struct.ScmObj* %k48417, %struct.ScmObj* %argslist60481$k484172)
ret void
}

define tailcc void @proc_clo$ae52377(%struct.ScmObj* %env$ae52377,%struct.ScmObj* %current_45args60483) {
%stackaddr$prim61921 = alloca %struct.ScmObj*, align 8
%k48419 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60483)
store volatile %struct.ScmObj* %k48419, %struct.ScmObj** %stackaddr$prim61921, align 8
%stackaddr$prim61922 = alloca %struct.ScmObj*, align 8
%current_45args60484 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60483)
store volatile %struct.ScmObj* %current_45args60484, %struct.ScmObj** %stackaddr$prim61922, align 8
%stackaddr$prim61923 = alloca %struct.ScmObj*, align 8
%x4802948170 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60484)
store volatile %struct.ScmObj* %x4802948170, %struct.ScmObj** %stackaddr$prim61923, align 8
%stackaddr$prim61924 = alloca %struct.ScmObj*, align 8
%cpsprim48420 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x4802948170)
store volatile %struct.ScmObj* %cpsprim48420, %struct.ScmObj** %stackaddr$prim61924, align 8
%ae52380 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist60486$k484190 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61925 = alloca %struct.ScmObj*, align 8
%argslist60486$k484191 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48420, %struct.ScmObj* %argslist60486$k484190)
store volatile %struct.ScmObj* %argslist60486$k484191, %struct.ScmObj** %stackaddr$prim61925, align 8
%stackaddr$prim61926 = alloca %struct.ScmObj*, align 8
%argslist60486$k484192 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52380, %struct.ScmObj* %argslist60486$k484191)
store volatile %struct.ScmObj* %argslist60486$k484192, %struct.ScmObj** %stackaddr$prim61926, align 8
%clofunc61927 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48419)
musttail call tailcc void %clofunc61927(%struct.ScmObj* %k48419, %struct.ScmObj* %argslist60486$k484192)
ret void
}

define tailcc void @proc_clo$ae52308(%struct.ScmObj* %env$ae52308,%struct.ScmObj* %current_45args60488) {
%stackaddr$prim61928 = alloca %struct.ScmObj*, align 8
%k48421 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60488)
store volatile %struct.ScmObj* %k48421, %struct.ScmObj** %stackaddr$prim61928, align 8
%stackaddr$prim61929 = alloca %struct.ScmObj*, align 8
%current_45args60489 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60488)
store volatile %struct.ScmObj* %current_45args60489, %struct.ScmObj** %stackaddr$prim61929, align 8
%stackaddr$prim61930 = alloca %struct.ScmObj*, align 8
%n48172 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60489)
store volatile %struct.ScmObj* %n48172, %struct.ScmObj** %stackaddr$prim61930, align 8
%stackaddr$makeclosure61931 = alloca %struct.ScmObj*, align 8
%fptrToInt61932 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52309 to i64
%ae52309 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt61932)
store volatile %struct.ScmObj* %ae52309, %struct.ScmObj** %stackaddr$makeclosure61931, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52309, %struct.ScmObj* %k48421, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52309, %struct.ScmObj* %n48172, i64 1)
%ae52310 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61933 = alloca %struct.ScmObj*, align 8
%fptrToInt61934 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52311 to i64
%ae52311 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt61934)
store volatile %struct.ScmObj* %ae52311, %struct.ScmObj** %stackaddr$makeclosure61933, align 8
%argslist60500$ae523090 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61935 = alloca %struct.ScmObj*, align 8
%argslist60500$ae523091 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52311, %struct.ScmObj* %argslist60500$ae523090)
store volatile %struct.ScmObj* %argslist60500$ae523091, %struct.ScmObj** %stackaddr$prim61935, align 8
%stackaddr$prim61936 = alloca %struct.ScmObj*, align 8
%argslist60500$ae523092 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52310, %struct.ScmObj* %argslist60500$ae523091)
store volatile %struct.ScmObj* %argslist60500$ae523092, %struct.ScmObj** %stackaddr$prim61936, align 8
%clofunc61937 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52309)
musttail call tailcc void %clofunc61937(%struct.ScmObj* %ae52309, %struct.ScmObj* %argslist60500$ae523092)
ret void
}

define tailcc void @proc_clo$ae52309(%struct.ScmObj* %env$ae52309,%struct.ScmObj* %current_45args60491) {
%stackaddr$env-ref61938 = alloca %struct.ScmObj*, align 8
%k48421 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52309, i64 0)
store %struct.ScmObj* %k48421, %struct.ScmObj** %stackaddr$env-ref61938
%stackaddr$env-ref61939 = alloca %struct.ScmObj*, align 8
%n48172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52309, i64 1)
store %struct.ScmObj* %n48172, %struct.ScmObj** %stackaddr$env-ref61939
%stackaddr$prim61940 = alloca %struct.ScmObj*, align 8
%_95k48422 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60491)
store volatile %struct.ScmObj* %_95k48422, %struct.ScmObj** %stackaddr$prim61940, align 8
%stackaddr$prim61941 = alloca %struct.ScmObj*, align 8
%current_45args60492 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60491)
store volatile %struct.ScmObj* %current_45args60492, %struct.ScmObj** %stackaddr$prim61941, align 8
%stackaddr$prim61942 = alloca %struct.ScmObj*, align 8
%anf_45bind48314 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60492)
store volatile %struct.ScmObj* %anf_45bind48314, %struct.ScmObj** %stackaddr$prim61942, align 8
%stackaddr$makeclosure61943 = alloca %struct.ScmObj*, align 8
%fptrToInt61944 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52332 to i64
%ae52332 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt61944)
store volatile %struct.ScmObj* %ae52332, %struct.ScmObj** %stackaddr$makeclosure61943, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52332, %struct.ScmObj* %k48421, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52332, %struct.ScmObj* %n48172, i64 1)
%argslist60498$anf_45bind483140 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61945 = alloca %struct.ScmObj*, align 8
%argslist60498$anf_45bind483141 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52332, %struct.ScmObj* %argslist60498$anf_45bind483140)
store volatile %struct.ScmObj* %argslist60498$anf_45bind483141, %struct.ScmObj** %stackaddr$prim61945, align 8
%clofunc61946 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48314)
musttail call tailcc void %clofunc61946(%struct.ScmObj* %anf_45bind48314, %struct.ScmObj* %argslist60498$anf_45bind483141)
ret void
}

define tailcc void @proc_clo$ae52332(%struct.ScmObj* %env$ae52332,%struct.ScmObj* %current_45args60494) {
%stackaddr$env-ref61947 = alloca %struct.ScmObj*, align 8
%k48421 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52332, i64 0)
store %struct.ScmObj* %k48421, %struct.ScmObj** %stackaddr$env-ref61947
%stackaddr$env-ref61948 = alloca %struct.ScmObj*, align 8
%n48172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52332, i64 1)
store %struct.ScmObj* %n48172, %struct.ScmObj** %stackaddr$env-ref61948
%stackaddr$prim61949 = alloca %struct.ScmObj*, align 8
%_95k48423 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60494)
store volatile %struct.ScmObj* %_95k48423, %struct.ScmObj** %stackaddr$prim61949, align 8
%stackaddr$prim61950 = alloca %struct.ScmObj*, align 8
%current_45args60495 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60494)
store volatile %struct.ScmObj* %current_45args60495, %struct.ScmObj** %stackaddr$prim61950, align 8
%stackaddr$prim61951 = alloca %struct.ScmObj*, align 8
%anf_45bind48315 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60495)
store volatile %struct.ScmObj* %anf_45bind48315, %struct.ScmObj** %stackaddr$prim61951, align 8
%stackaddr$prim61952 = alloca %struct.ScmObj*, align 8
%cpsprim48424 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %n48172, %struct.ScmObj* %anf_45bind48315)
store volatile %struct.ScmObj* %cpsprim48424, %struct.ScmObj** %stackaddr$prim61952, align 8
%ae52336 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist60497$k484210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61953 = alloca %struct.ScmObj*, align 8
%argslist60497$k484211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48424, %struct.ScmObj* %argslist60497$k484210)
store volatile %struct.ScmObj* %argslist60497$k484211, %struct.ScmObj** %stackaddr$prim61953, align 8
%stackaddr$prim61954 = alloca %struct.ScmObj*, align 8
%argslist60497$k484212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52336, %struct.ScmObj* %argslist60497$k484211)
store volatile %struct.ScmObj* %argslist60497$k484212, %struct.ScmObj** %stackaddr$prim61954, align 8
%clofunc61955 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48421)
musttail call tailcc void %clofunc61955(%struct.ScmObj* %k48421, %struct.ScmObj* %argslist60497$k484212)
ret void
}

define tailcc void @proc_clo$ae52311(%struct.ScmObj* %env$ae52311,%struct.ScmObj* %lst4817348425) {
%stackaddr$prim61956 = alloca %struct.ScmObj*, align 8
%k48426 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4817348425)
store volatile %struct.ScmObj* %k48426, %struct.ScmObj** %stackaddr$prim61956, align 8
%stackaddr$prim61957 = alloca %struct.ScmObj*, align 8
%lst48173 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4817348425)
store volatile %struct.ScmObj* %lst48173, %struct.ScmObj** %stackaddr$prim61957, align 8
%ae52315 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist60499$k484260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61958 = alloca %struct.ScmObj*, align 8
%argslist60499$k484261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48173, %struct.ScmObj* %argslist60499$k484260)
store volatile %struct.ScmObj* %argslist60499$k484261, %struct.ScmObj** %stackaddr$prim61958, align 8
%stackaddr$prim61959 = alloca %struct.ScmObj*, align 8
%argslist60499$k484262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52315, %struct.ScmObj* %argslist60499$k484261)
store volatile %struct.ScmObj* %argslist60499$k484262, %struct.ScmObj** %stackaddr$prim61959, align 8
%clofunc61960 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48426)
musttail call tailcc void %clofunc61960(%struct.ScmObj* %k48426, %struct.ScmObj* %argslist60499$k484262)
ret void
}

define tailcc void @proc_clo$ae52173(%struct.ScmObj* %env$ae52173,%struct.ScmObj* %current_45args60502) {
%stackaddr$env-ref61961 = alloca %struct.ScmObj*, align 8
%z_6348153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52173, i64 0)
store %struct.ScmObj* %z_6348153, %struct.ScmObj** %stackaddr$env-ref61961
%stackaddr$env-ref61962 = alloca %struct.ScmObj*, align 8
%peano_45_62nat48156 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52173, i64 1)
store %struct.ScmObj* %peano_45_62nat48156, %struct.ScmObj** %stackaddr$env-ref61962
%stackaddr$env-ref61963 = alloca %struct.ScmObj*, align 8
%pred48154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52173, i64 2)
store %struct.ScmObj* %pred48154, %struct.ScmObj** %stackaddr$env-ref61963
%stackaddr$prim61964 = alloca %struct.ScmObj*, align 8
%k48427 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60502)
store volatile %struct.ScmObj* %k48427, %struct.ScmObj** %stackaddr$prim61964, align 8
%stackaddr$prim61965 = alloca %struct.ScmObj*, align 8
%current_45args60503 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60502)
store volatile %struct.ScmObj* %current_45args60503, %struct.ScmObj** %stackaddr$prim61965, align 8
%stackaddr$prim61966 = alloca %struct.ScmObj*, align 8
%n48175 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60503)
store volatile %struct.ScmObj* %n48175, %struct.ScmObj** %stackaddr$prim61966, align 8
%ae52175 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61967 = alloca %struct.ScmObj*, align 8
%anf_45bind48307 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %z_6348153, %struct.ScmObj* %ae52175)
store volatile %struct.ScmObj* %anf_45bind48307, %struct.ScmObj** %stackaddr$prim61967, align 8
%stackaddr$makeclosure61968 = alloca %struct.ScmObj*, align 8
%fptrToInt61969 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52177 to i64
%ae52177 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt61969)
store volatile %struct.ScmObj* %ae52177, %struct.ScmObj** %stackaddr$makeclosure61968, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52177, %struct.ScmObj* %n48175, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52177, %struct.ScmObj* %peano_45_62nat48156, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52177, %struct.ScmObj* %k48427, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52177, %struct.ScmObj* %pred48154, i64 3)
%argslist60518$anf_45bind483070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61970 = alloca %struct.ScmObj*, align 8
%argslist60518$anf_45bind483071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %n48175, %struct.ScmObj* %argslist60518$anf_45bind483070)
store volatile %struct.ScmObj* %argslist60518$anf_45bind483071, %struct.ScmObj** %stackaddr$prim61970, align 8
%stackaddr$prim61971 = alloca %struct.ScmObj*, align 8
%argslist60518$anf_45bind483072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52177, %struct.ScmObj* %argslist60518$anf_45bind483071)
store volatile %struct.ScmObj* %argslist60518$anf_45bind483072, %struct.ScmObj** %stackaddr$prim61971, align 8
%clofunc61972 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48307)
musttail call tailcc void %clofunc61972(%struct.ScmObj* %anf_45bind48307, %struct.ScmObj* %argslist60518$anf_45bind483072)
ret void
}

define tailcc void @proc_clo$ae52177(%struct.ScmObj* %env$ae52177,%struct.ScmObj* %current_45args60505) {
%stackaddr$env-ref61973 = alloca %struct.ScmObj*, align 8
%n48175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52177, i64 0)
store %struct.ScmObj* %n48175, %struct.ScmObj** %stackaddr$env-ref61973
%stackaddr$env-ref61974 = alloca %struct.ScmObj*, align 8
%peano_45_62nat48156 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52177, i64 1)
store %struct.ScmObj* %peano_45_62nat48156, %struct.ScmObj** %stackaddr$env-ref61974
%stackaddr$env-ref61975 = alloca %struct.ScmObj*, align 8
%k48427 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52177, i64 2)
store %struct.ScmObj* %k48427, %struct.ScmObj** %stackaddr$env-ref61975
%stackaddr$env-ref61976 = alloca %struct.ScmObj*, align 8
%pred48154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52177, i64 3)
store %struct.ScmObj* %pred48154, %struct.ScmObj** %stackaddr$env-ref61976
%stackaddr$prim61977 = alloca %struct.ScmObj*, align 8
%_95k48428 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60505)
store volatile %struct.ScmObj* %_95k48428, %struct.ScmObj** %stackaddr$prim61977, align 8
%stackaddr$prim61978 = alloca %struct.ScmObj*, align 8
%current_45args60506 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60505)
store volatile %struct.ScmObj* %current_45args60506, %struct.ScmObj** %stackaddr$prim61978, align 8
%stackaddr$prim61979 = alloca %struct.ScmObj*, align 8
%anf_45bind48308 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60506)
store volatile %struct.ScmObj* %anf_45bind48308, %struct.ScmObj** %stackaddr$prim61979, align 8
%truthy$cmp61980 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48308)
%cmp$cmp61980 = icmp eq i64 %truthy$cmp61980, 1
br i1 %cmp$cmp61980, label %truebranch$cmp61980, label %falsebranch$cmp61980
truebranch$cmp61980:
%ae52181 = call %struct.ScmObj* @const_init_int(i64 0)
%ae52182 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist60508$k484270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61981 = alloca %struct.ScmObj*, align 8
%argslist60508$k484271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52182, %struct.ScmObj* %argslist60508$k484270)
store volatile %struct.ScmObj* %argslist60508$k484271, %struct.ScmObj** %stackaddr$prim61981, align 8
%stackaddr$prim61982 = alloca %struct.ScmObj*, align 8
%argslist60508$k484272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52181, %struct.ScmObj* %argslist60508$k484271)
store volatile %struct.ScmObj* %argslist60508$k484272, %struct.ScmObj** %stackaddr$prim61982, align 8
%clofunc61983 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48427)
musttail call tailcc void %clofunc61983(%struct.ScmObj* %k48427, %struct.ScmObj* %argslist60508$k484272)
ret void
falsebranch$cmp61980:
%ae52190 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61984 = alloca %struct.ScmObj*, align 8
%anf_45bind48309 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %peano_45_62nat48156, %struct.ScmObj* %ae52190)
store volatile %struct.ScmObj* %anf_45bind48309, %struct.ScmObj** %stackaddr$prim61984, align 8
%ae52192 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61985 = alloca %struct.ScmObj*, align 8
%anf_45bind48310 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %pred48154, %struct.ScmObj* %ae52192)
store volatile %struct.ScmObj* %anf_45bind48310, %struct.ScmObj** %stackaddr$prim61985, align 8
%stackaddr$makeclosure61986 = alloca %struct.ScmObj*, align 8
%fptrToInt61987 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52194 to i64
%ae52194 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt61987)
store volatile %struct.ScmObj* %ae52194, %struct.ScmObj** %stackaddr$makeclosure61986, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52194, %struct.ScmObj* %anf_45bind48309, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52194, %struct.ScmObj* %k48427, i64 1)
%argslist60517$anf_45bind483100 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61988 = alloca %struct.ScmObj*, align 8
%argslist60517$anf_45bind483101 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %n48175, %struct.ScmObj* %argslist60517$anf_45bind483100)
store volatile %struct.ScmObj* %argslist60517$anf_45bind483101, %struct.ScmObj** %stackaddr$prim61988, align 8
%stackaddr$prim61989 = alloca %struct.ScmObj*, align 8
%argslist60517$anf_45bind483102 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52194, %struct.ScmObj* %argslist60517$anf_45bind483101)
store volatile %struct.ScmObj* %argslist60517$anf_45bind483102, %struct.ScmObj** %stackaddr$prim61989, align 8
%clofunc61990 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48310)
musttail call tailcc void %clofunc61990(%struct.ScmObj* %anf_45bind48310, %struct.ScmObj* %argslist60517$anf_45bind483102)
ret void
}

define tailcc void @proc_clo$ae52194(%struct.ScmObj* %env$ae52194,%struct.ScmObj* %current_45args60509) {
%stackaddr$env-ref61991 = alloca %struct.ScmObj*, align 8
%anf_45bind48309 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52194, i64 0)
store %struct.ScmObj* %anf_45bind48309, %struct.ScmObj** %stackaddr$env-ref61991
%stackaddr$env-ref61992 = alloca %struct.ScmObj*, align 8
%k48427 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52194, i64 1)
store %struct.ScmObj* %k48427, %struct.ScmObj** %stackaddr$env-ref61992
%stackaddr$prim61993 = alloca %struct.ScmObj*, align 8
%_95k48429 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60509)
store volatile %struct.ScmObj* %_95k48429, %struct.ScmObj** %stackaddr$prim61993, align 8
%stackaddr$prim61994 = alloca %struct.ScmObj*, align 8
%current_45args60510 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60509)
store volatile %struct.ScmObj* %current_45args60510, %struct.ScmObj** %stackaddr$prim61994, align 8
%stackaddr$prim61995 = alloca %struct.ScmObj*, align 8
%anf_45bind48311 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60510)
store volatile %struct.ScmObj* %anf_45bind48311, %struct.ScmObj** %stackaddr$prim61995, align 8
%stackaddr$makeclosure61996 = alloca %struct.ScmObj*, align 8
%fptrToInt61997 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52197 to i64
%ae52197 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt61997)
store volatile %struct.ScmObj* %ae52197, %struct.ScmObj** %stackaddr$makeclosure61996, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52197, %struct.ScmObj* %k48427, i64 0)
%argslist60516$anf_45bind483090 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61998 = alloca %struct.ScmObj*, align 8
%argslist60516$anf_45bind483091 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48311, %struct.ScmObj* %argslist60516$anf_45bind483090)
store volatile %struct.ScmObj* %argslist60516$anf_45bind483091, %struct.ScmObj** %stackaddr$prim61998, align 8
%stackaddr$prim61999 = alloca %struct.ScmObj*, align 8
%argslist60516$anf_45bind483092 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52197, %struct.ScmObj* %argslist60516$anf_45bind483091)
store volatile %struct.ScmObj* %argslist60516$anf_45bind483092, %struct.ScmObj** %stackaddr$prim61999, align 8
%clofunc62000 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48309)
musttail call tailcc void %clofunc62000(%struct.ScmObj* %anf_45bind48309, %struct.ScmObj* %argslist60516$anf_45bind483092)
ret void
}

define tailcc void @proc_clo$ae52197(%struct.ScmObj* %env$ae52197,%struct.ScmObj* %current_45args60512) {
%stackaddr$env-ref62001 = alloca %struct.ScmObj*, align 8
%k48427 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52197, i64 0)
store %struct.ScmObj* %k48427, %struct.ScmObj** %stackaddr$env-ref62001
%stackaddr$prim62002 = alloca %struct.ScmObj*, align 8
%_95k48430 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60512)
store volatile %struct.ScmObj* %_95k48430, %struct.ScmObj** %stackaddr$prim62002, align 8
%stackaddr$prim62003 = alloca %struct.ScmObj*, align 8
%current_45args60513 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60512)
store volatile %struct.ScmObj* %current_45args60513, %struct.ScmObj** %stackaddr$prim62003, align 8
%stackaddr$prim62004 = alloca %struct.ScmObj*, align 8
%anf_45bind48312 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60513)
store volatile %struct.ScmObj* %anf_45bind48312, %struct.ScmObj** %stackaddr$prim62004, align 8
%ae52199 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim62005 = alloca %struct.ScmObj*, align 8
%cpsprim48431 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae52199, %struct.ScmObj* %anf_45bind48312)
store volatile %struct.ScmObj* %cpsprim48431, %struct.ScmObj** %stackaddr$prim62005, align 8
%ae52202 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist60515$k484270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62006 = alloca %struct.ScmObj*, align 8
%argslist60515$k484271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48431, %struct.ScmObj* %argslist60515$k484270)
store volatile %struct.ScmObj* %argslist60515$k484271, %struct.ScmObj** %stackaddr$prim62006, align 8
%stackaddr$prim62007 = alloca %struct.ScmObj*, align 8
%argslist60515$k484272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52202, %struct.ScmObj* %argslist60515$k484271)
store volatile %struct.ScmObj* %argslist60515$k484272, %struct.ScmObj** %stackaddr$prim62007, align 8
%clofunc62008 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48427)
musttail call tailcc void %clofunc62008(%struct.ScmObj* %k48427, %struct.ScmObj* %argslist60515$k484272)
ret void
}

define tailcc void @proc_clo$ae51962(%struct.ScmObj* %env$ae51962,%struct.ScmObj* %current_45args60520) {
%stackaddr$env-ref62009 = alloca %struct.ScmObj*, align 8
%nat_45_62peano48157 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51962, i64 0)
store %struct.ScmObj* %nat_45_62peano48157, %struct.ScmObj** %stackaddr$env-ref62009
%stackaddr$prim62010 = alloca %struct.ScmObj*, align 8
%k48432 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60520)
store volatile %struct.ScmObj* %k48432, %struct.ScmObj** %stackaddr$prim62010, align 8
%stackaddr$prim62011 = alloca %struct.ScmObj*, align 8
%current_45args60521 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60520)
store volatile %struct.ScmObj* %current_45args60521, %struct.ScmObj** %stackaddr$prim62011, align 8
%stackaddr$prim62012 = alloca %struct.ScmObj*, align 8
%n48177 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60521)
store volatile %struct.ScmObj* %n48177, %struct.ScmObj** %stackaddr$prim62012, align 8
%ae51963 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim62013 = alloca %struct.ScmObj*, align 8
%anf_45bind48299 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae51963, %struct.ScmObj* %n48177)
store volatile %struct.ScmObj* %anf_45bind48299, %struct.ScmObj** %stackaddr$prim62013, align 8
%truthy$cmp62014 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48299)
%cmp$cmp62014 = icmp eq i64 %truthy$cmp62014, 1
br i1 %cmp$cmp62014, label %truebranch$cmp62014, label %falsebranch$cmp62014
truebranch$cmp62014:
%stackaddr$makeclosure62015 = alloca %struct.ScmObj*, align 8
%fptrToInt62016 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51966 to i64
%ae51966 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt62016)
store volatile %struct.ScmObj* %ae51966, %struct.ScmObj** %stackaddr$makeclosure62015, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51966, %struct.ScmObj* %k48432, i64 0)
%ae51967 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure62017 = alloca %struct.ScmObj*, align 8
%fptrToInt62018 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51968 to i64
%ae51968 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt62018)
store volatile %struct.ScmObj* %ae51968, %struct.ScmObj** %stackaddr$makeclosure62017, align 8
%argslist60528$ae519660 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62019 = alloca %struct.ScmObj*, align 8
%argslist60528$ae519661 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51968, %struct.ScmObj* %argslist60528$ae519660)
store volatile %struct.ScmObj* %argslist60528$ae519661, %struct.ScmObj** %stackaddr$prim62019, align 8
%stackaddr$prim62020 = alloca %struct.ScmObj*, align 8
%argslist60528$ae519662 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51967, %struct.ScmObj* %argslist60528$ae519661)
store volatile %struct.ScmObj* %argslist60528$ae519662, %struct.ScmObj** %stackaddr$prim62020, align 8
%clofunc62021 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51966)
musttail call tailcc void %clofunc62021(%struct.ScmObj* %ae51966, %struct.ScmObj* %argslist60528$ae519662)
ret void
falsebranch$cmp62014:
%ae51999 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim62022 = alloca %struct.ScmObj*, align 8
%anf_45bind48301 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %nat_45_62peano48157, %struct.ScmObj* %ae51999)
store volatile %struct.ScmObj* %anf_45bind48301, %struct.ScmObj** %stackaddr$prim62022, align 8
%ae52001 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim62023 = alloca %struct.ScmObj*, align 8
%anf_45bind48302 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n48177, %struct.ScmObj* %ae52001)
store volatile %struct.ScmObj* %anf_45bind48302, %struct.ScmObj** %stackaddr$prim62023, align 8
%stackaddr$makeclosure62024 = alloca %struct.ScmObj*, align 8
%fptrToInt62025 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52003 to i64
%ae52003 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt62025)
store volatile %struct.ScmObj* %ae52003, %struct.ScmObj** %stackaddr$makeclosure62024, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52003, %struct.ScmObj* %k48432, i64 0)
%argslist60542$anf_45bind483010 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62026 = alloca %struct.ScmObj*, align 8
%argslist60542$anf_45bind483011 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48302, %struct.ScmObj* %argslist60542$anf_45bind483010)
store volatile %struct.ScmObj* %argslist60542$anf_45bind483011, %struct.ScmObj** %stackaddr$prim62026, align 8
%stackaddr$prim62027 = alloca %struct.ScmObj*, align 8
%argslist60542$anf_45bind483012 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52003, %struct.ScmObj* %argslist60542$anf_45bind483011)
store volatile %struct.ScmObj* %argslist60542$anf_45bind483012, %struct.ScmObj** %stackaddr$prim62027, align 8
%clofunc62028 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48301)
musttail call tailcc void %clofunc62028(%struct.ScmObj* %anf_45bind48301, %struct.ScmObj* %argslist60542$anf_45bind483012)
ret void
}

define tailcc void @proc_clo$ae51966(%struct.ScmObj* %env$ae51966,%struct.ScmObj* %current_45args60523) {
%stackaddr$env-ref62029 = alloca %struct.ScmObj*, align 8
%k48432 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51966, i64 0)
store %struct.ScmObj* %k48432, %struct.ScmObj** %stackaddr$env-ref62029
%stackaddr$prim62030 = alloca %struct.ScmObj*, align 8
%_95k48433 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60523)
store volatile %struct.ScmObj* %_95k48433, %struct.ScmObj** %stackaddr$prim62030, align 8
%stackaddr$prim62031 = alloca %struct.ScmObj*, align 8
%current_45args60524 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60523)
store volatile %struct.ScmObj* %current_45args60524, %struct.ScmObj** %stackaddr$prim62031, align 8
%stackaddr$prim62032 = alloca %struct.ScmObj*, align 8
%anf_45bind48300 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60524)
store volatile %struct.ScmObj* %anf_45bind48300, %struct.ScmObj** %stackaddr$prim62032, align 8
%argslist60526$anf_45bind483000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62033 = alloca %struct.ScmObj*, align 8
%argslist60526$anf_45bind483001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48432, %struct.ScmObj* %argslist60526$anf_45bind483000)
store volatile %struct.ScmObj* %argslist60526$anf_45bind483001, %struct.ScmObj** %stackaddr$prim62033, align 8
%clofunc62034 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48300)
musttail call tailcc void %clofunc62034(%struct.ScmObj* %anf_45bind48300, %struct.ScmObj* %argslist60526$anf_45bind483001)
ret void
}

define tailcc void @proc_clo$ae51968(%struct.ScmObj* %env$ae51968,%struct.ScmObj* %lst4817848434) {
%stackaddr$prim62035 = alloca %struct.ScmObj*, align 8
%k48435 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4817848434)
store volatile %struct.ScmObj* %k48435, %struct.ScmObj** %stackaddr$prim62035, align 8
%stackaddr$prim62036 = alloca %struct.ScmObj*, align 8
%lst48178 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4817848434)
store volatile %struct.ScmObj* %lst48178, %struct.ScmObj** %stackaddr$prim62036, align 8
%ae51972 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist60527$k484350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62037 = alloca %struct.ScmObj*, align 8
%argslist60527$k484351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48178, %struct.ScmObj* %argslist60527$k484350)
store volatile %struct.ScmObj* %argslist60527$k484351, %struct.ScmObj** %stackaddr$prim62037, align 8
%stackaddr$prim62038 = alloca %struct.ScmObj*, align 8
%argslist60527$k484352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51972, %struct.ScmObj* %argslist60527$k484351)
store volatile %struct.ScmObj* %argslist60527$k484352, %struct.ScmObj** %stackaddr$prim62038, align 8
%clofunc62039 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48435)
musttail call tailcc void %clofunc62039(%struct.ScmObj* %k48435, %struct.ScmObj* %argslist60527$k484352)
ret void
}

define tailcc void @proc_clo$ae52003(%struct.ScmObj* %env$ae52003,%struct.ScmObj* %current_45args60529) {
%stackaddr$env-ref62040 = alloca %struct.ScmObj*, align 8
%k48432 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52003, i64 0)
store %struct.ScmObj* %k48432, %struct.ScmObj** %stackaddr$env-ref62040
%stackaddr$prim62041 = alloca %struct.ScmObj*, align 8
%_95k48436 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60529)
store volatile %struct.ScmObj* %_95k48436, %struct.ScmObj** %stackaddr$prim62041, align 8
%stackaddr$prim62042 = alloca %struct.ScmObj*, align 8
%current_45args60530 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60529)
store volatile %struct.ScmObj* %current_45args60530, %struct.ScmObj** %stackaddr$prim62042, align 8
%stackaddr$prim62043 = alloca %struct.ScmObj*, align 8
%anf_45bind48303 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60530)
store volatile %struct.ScmObj* %anf_45bind48303, %struct.ScmObj** %stackaddr$prim62043, align 8
%stackaddr$makeclosure62044 = alloca %struct.ScmObj*, align 8
%fptrToInt62045 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52005 to i64
%ae52005 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt62045)
store volatile %struct.ScmObj* %ae52005, %struct.ScmObj** %stackaddr$makeclosure62044, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52005, %struct.ScmObj* %k48432, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52005, %struct.ScmObj* %anf_45bind48303, i64 1)
%ae52006 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure62046 = alloca %struct.ScmObj*, align 8
%fptrToInt62047 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52007 to i64
%ae52007 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt62047)
store volatile %struct.ScmObj* %ae52007, %struct.ScmObj** %stackaddr$makeclosure62046, align 8
%argslist60541$ae520050 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62048 = alloca %struct.ScmObj*, align 8
%argslist60541$ae520051 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52007, %struct.ScmObj* %argslist60541$ae520050)
store volatile %struct.ScmObj* %argslist60541$ae520051, %struct.ScmObj** %stackaddr$prim62048, align 8
%stackaddr$prim62049 = alloca %struct.ScmObj*, align 8
%argslist60541$ae520052 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52006, %struct.ScmObj* %argslist60541$ae520051)
store volatile %struct.ScmObj* %argslist60541$ae520052, %struct.ScmObj** %stackaddr$prim62049, align 8
%clofunc62050 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52005)
musttail call tailcc void %clofunc62050(%struct.ScmObj* %ae52005, %struct.ScmObj* %argslist60541$ae520052)
ret void
}

define tailcc void @proc_clo$ae52005(%struct.ScmObj* %env$ae52005,%struct.ScmObj* %current_45args60532) {
%stackaddr$env-ref62051 = alloca %struct.ScmObj*, align 8
%k48432 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52005, i64 0)
store %struct.ScmObj* %k48432, %struct.ScmObj** %stackaddr$env-ref62051
%stackaddr$env-ref62052 = alloca %struct.ScmObj*, align 8
%anf_45bind48303 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52005, i64 1)
store %struct.ScmObj* %anf_45bind48303, %struct.ScmObj** %stackaddr$env-ref62052
%stackaddr$prim62053 = alloca %struct.ScmObj*, align 8
%_95k48437 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60532)
store volatile %struct.ScmObj* %_95k48437, %struct.ScmObj** %stackaddr$prim62053, align 8
%stackaddr$prim62054 = alloca %struct.ScmObj*, align 8
%current_45args60533 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60532)
store volatile %struct.ScmObj* %current_45args60533, %struct.ScmObj** %stackaddr$prim62054, align 8
%stackaddr$prim62055 = alloca %struct.ScmObj*, align 8
%anf_45bind48304 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60533)
store volatile %struct.ScmObj* %anf_45bind48304, %struct.ScmObj** %stackaddr$prim62055, align 8
%stackaddr$makeclosure62056 = alloca %struct.ScmObj*, align 8
%fptrToInt62057 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52028 to i64
%ae52028 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt62057)
store volatile %struct.ScmObj* %ae52028, %struct.ScmObj** %stackaddr$makeclosure62056, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52028, %struct.ScmObj* %k48432, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52028, %struct.ScmObj* %anf_45bind48303, i64 1)
%argslist60539$anf_45bind483040 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62058 = alloca %struct.ScmObj*, align 8
%argslist60539$anf_45bind483041 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52028, %struct.ScmObj* %argslist60539$anf_45bind483040)
store volatile %struct.ScmObj* %argslist60539$anf_45bind483041, %struct.ScmObj** %stackaddr$prim62058, align 8
%clofunc62059 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48304)
musttail call tailcc void %clofunc62059(%struct.ScmObj* %anf_45bind48304, %struct.ScmObj* %argslist60539$anf_45bind483041)
ret void
}

define tailcc void @proc_clo$ae52028(%struct.ScmObj* %env$ae52028,%struct.ScmObj* %current_45args60535) {
%stackaddr$env-ref62060 = alloca %struct.ScmObj*, align 8
%k48432 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52028, i64 0)
store %struct.ScmObj* %k48432, %struct.ScmObj** %stackaddr$env-ref62060
%stackaddr$env-ref62061 = alloca %struct.ScmObj*, align 8
%anf_45bind48303 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52028, i64 1)
store %struct.ScmObj* %anf_45bind48303, %struct.ScmObj** %stackaddr$env-ref62061
%stackaddr$prim62062 = alloca %struct.ScmObj*, align 8
%_95k48438 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60535)
store volatile %struct.ScmObj* %_95k48438, %struct.ScmObj** %stackaddr$prim62062, align 8
%stackaddr$prim62063 = alloca %struct.ScmObj*, align 8
%current_45args60536 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60535)
store volatile %struct.ScmObj* %current_45args60536, %struct.ScmObj** %stackaddr$prim62063, align 8
%stackaddr$prim62064 = alloca %struct.ScmObj*, align 8
%anf_45bind48305 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60536)
store volatile %struct.ScmObj* %anf_45bind48305, %struct.ScmObj** %stackaddr$prim62064, align 8
%stackaddr$prim62065 = alloca %struct.ScmObj*, align 8
%cpsprim48439 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48303, %struct.ScmObj* %anf_45bind48305)
store volatile %struct.ScmObj* %cpsprim48439, %struct.ScmObj** %stackaddr$prim62065, align 8
%ae52032 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist60538$k484320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62066 = alloca %struct.ScmObj*, align 8
%argslist60538$k484321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48439, %struct.ScmObj* %argslist60538$k484320)
store volatile %struct.ScmObj* %argslist60538$k484321, %struct.ScmObj** %stackaddr$prim62066, align 8
%stackaddr$prim62067 = alloca %struct.ScmObj*, align 8
%argslist60538$k484322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52032, %struct.ScmObj* %argslist60538$k484321)
store volatile %struct.ScmObj* %argslist60538$k484322, %struct.ScmObj** %stackaddr$prim62067, align 8
%clofunc62068 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48432)
musttail call tailcc void %clofunc62068(%struct.ScmObj* %k48432, %struct.ScmObj* %argslist60538$k484322)
ret void
}

define tailcc void @proc_clo$ae52007(%struct.ScmObj* %env$ae52007,%struct.ScmObj* %lst4817948440) {
%stackaddr$prim62069 = alloca %struct.ScmObj*, align 8
%k48441 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4817948440)
store volatile %struct.ScmObj* %k48441, %struct.ScmObj** %stackaddr$prim62069, align 8
%stackaddr$prim62070 = alloca %struct.ScmObj*, align 8
%lst48179 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4817948440)
store volatile %struct.ScmObj* %lst48179, %struct.ScmObj** %stackaddr$prim62070, align 8
%ae52011 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist60540$k484410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62071 = alloca %struct.ScmObj*, align 8
%argslist60540$k484411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48179, %struct.ScmObj* %argslist60540$k484410)
store volatile %struct.ScmObj* %argslist60540$k484411, %struct.ScmObj** %stackaddr$prim62071, align 8
%stackaddr$prim62072 = alloca %struct.ScmObj*, align 8
%argslist60540$k484412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52011, %struct.ScmObj* %argslist60540$k484411)
store volatile %struct.ScmObj* %argslist60540$k484412, %struct.ScmObj** %stackaddr$prim62072, align 8
%clofunc62073 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48441)
musttail call tailcc void %clofunc62073(%struct.ScmObj* %k48441, %struct.ScmObj* %argslist60540$k484412)
ret void
}

define tailcc void @proc_clo$ae51922(%struct.ScmObj* %env$ae51922,%struct.ScmObj* %current_45args60544) {
%stackaddr$prim62074 = alloca %struct.ScmObj*, align 8
%k48442 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60544)
store volatile %struct.ScmObj* %k48442, %struct.ScmObj** %stackaddr$prim62074, align 8
%stackaddr$prim62075 = alloca %struct.ScmObj*, align 8
%current_45args60545 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60544)
store volatile %struct.ScmObj* %current_45args60545, %struct.ScmObj** %stackaddr$prim62075, align 8
%stackaddr$prim62076 = alloca %struct.ScmObj*, align 8
%x48091 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60545)
store volatile %struct.ScmObj* %x48091, %struct.ScmObj** %stackaddr$prim62076, align 8
%stackaddr$prim62077 = alloca %struct.ScmObj*, align 8
%anf_45bind48289 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48091)
store volatile %struct.ScmObj* %anf_45bind48289, %struct.ScmObj** %stackaddr$prim62077, align 8
%stackaddr$prim62078 = alloca %struct.ScmObj*, align 8
%anf_45bind48290 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48289)
store volatile %struct.ScmObj* %anf_45bind48290, %struct.ScmObj** %stackaddr$prim62078, align 8
%stackaddr$prim62079 = alloca %struct.ScmObj*, align 8
%anf_45bind48291 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48290)
store volatile %struct.ScmObj* %anf_45bind48291, %struct.ScmObj** %stackaddr$prim62079, align 8
%stackaddr$prim62080 = alloca %struct.ScmObj*, align 8
%cpsprim48443 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48291)
store volatile %struct.ScmObj* %cpsprim48443, %struct.ScmObj** %stackaddr$prim62080, align 8
%ae51928 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist60547$k484420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62081 = alloca %struct.ScmObj*, align 8
%argslist60547$k484421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48443, %struct.ScmObj* %argslist60547$k484420)
store volatile %struct.ScmObj* %argslist60547$k484421, %struct.ScmObj** %stackaddr$prim62081, align 8
%stackaddr$prim62082 = alloca %struct.ScmObj*, align 8
%argslist60547$k484422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51928, %struct.ScmObj* %argslist60547$k484421)
store volatile %struct.ScmObj* %argslist60547$k484422, %struct.ScmObj** %stackaddr$prim62082, align 8
%clofunc62083 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48442)
musttail call tailcc void %clofunc62083(%struct.ScmObj* %k48442, %struct.ScmObj* %argslist60547$k484422)
ret void
}

define tailcc void @proc_clo$ae51898(%struct.ScmObj* %env$ae51898,%struct.ScmObj* %current_45args60549) {
%stackaddr$prim62084 = alloca %struct.ScmObj*, align 8
%k48444 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60549)
store volatile %struct.ScmObj* %k48444, %struct.ScmObj** %stackaddr$prim62084, align 8
%stackaddr$prim62085 = alloca %struct.ScmObj*, align 8
%current_45args60550 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60549)
store volatile %struct.ScmObj* %current_45args60550, %struct.ScmObj** %stackaddr$prim62085, align 8
%stackaddr$prim62086 = alloca %struct.ScmObj*, align 8
%x48093 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60550)
store volatile %struct.ScmObj* %x48093, %struct.ScmObj** %stackaddr$prim62086, align 8
%stackaddr$prim62087 = alloca %struct.ScmObj*, align 8
%anf_45bind48287 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48093)
store volatile %struct.ScmObj* %anf_45bind48287, %struct.ScmObj** %stackaddr$prim62087, align 8
%stackaddr$prim62088 = alloca %struct.ScmObj*, align 8
%anf_45bind48288 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48287)
store volatile %struct.ScmObj* %anf_45bind48288, %struct.ScmObj** %stackaddr$prim62088, align 8
%stackaddr$prim62089 = alloca %struct.ScmObj*, align 8
%cpsprim48445 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48288)
store volatile %struct.ScmObj* %cpsprim48445, %struct.ScmObj** %stackaddr$prim62089, align 8
%ae51903 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist60552$k484440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62090 = alloca %struct.ScmObj*, align 8
%argslist60552$k484441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48445, %struct.ScmObj* %argslist60552$k484440)
store volatile %struct.ScmObj* %argslist60552$k484441, %struct.ScmObj** %stackaddr$prim62090, align 8
%stackaddr$prim62091 = alloca %struct.ScmObj*, align 8
%argslist60552$k484442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51903, %struct.ScmObj* %argslist60552$k484441)
store volatile %struct.ScmObj* %argslist60552$k484442, %struct.ScmObj** %stackaddr$prim62091, align 8
%clofunc62092 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48444)
musttail call tailcc void %clofunc62092(%struct.ScmObj* %k48444, %struct.ScmObj* %argslist60552$k484442)
ret void
}

define tailcc void @proc_clo$ae51876(%struct.ScmObj* %env$ae51876,%struct.ScmObj* %current_45args60554) {
%stackaddr$prim62093 = alloca %struct.ScmObj*, align 8
%k48446 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60554)
store volatile %struct.ScmObj* %k48446, %struct.ScmObj** %stackaddr$prim62093, align 8
%stackaddr$prim62094 = alloca %struct.ScmObj*, align 8
%current_45args60555 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60554)
store volatile %struct.ScmObj* %current_45args60555, %struct.ScmObj** %stackaddr$prim62094, align 8
%stackaddr$prim62095 = alloca %struct.ScmObj*, align 8
%x48095 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60555)
store volatile %struct.ScmObj* %x48095, %struct.ScmObj** %stackaddr$prim62095, align 8
%stackaddr$prim62096 = alloca %struct.ScmObj*, align 8
%anf_45bind48286 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48095)
store volatile %struct.ScmObj* %anf_45bind48286, %struct.ScmObj** %stackaddr$prim62096, align 8
%stackaddr$prim62097 = alloca %struct.ScmObj*, align 8
%cpsprim48447 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48286)
store volatile %struct.ScmObj* %cpsprim48447, %struct.ScmObj** %stackaddr$prim62097, align 8
%ae51880 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist60557$k484460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62098 = alloca %struct.ScmObj*, align 8
%argslist60557$k484461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48447, %struct.ScmObj* %argslist60557$k484460)
store volatile %struct.ScmObj* %argslist60557$k484461, %struct.ScmObj** %stackaddr$prim62098, align 8
%stackaddr$prim62099 = alloca %struct.ScmObj*, align 8
%argslist60557$k484462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51880, %struct.ScmObj* %argslist60557$k484461)
store volatile %struct.ScmObj* %argslist60557$k484462, %struct.ScmObj** %stackaddr$prim62099, align 8
%clofunc62100 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48446)
musttail call tailcc void %clofunc62100(%struct.ScmObj* %k48446, %struct.ScmObj* %argslist60557$k484462)
ret void
}

define tailcc void @proc_clo$ae51856(%struct.ScmObj* %env$ae51856,%struct.ScmObj* %current_45args60559) {
%stackaddr$prim62101 = alloca %struct.ScmObj*, align 8
%k48448 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60559)
store volatile %struct.ScmObj* %k48448, %struct.ScmObj** %stackaddr$prim62101, align 8
%stackaddr$prim62102 = alloca %struct.ScmObj*, align 8
%current_45args60560 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60559)
store volatile %struct.ScmObj* %current_45args60560, %struct.ScmObj** %stackaddr$prim62102, align 8
%stackaddr$prim62103 = alloca %struct.ScmObj*, align 8
%x48097 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60560)
store volatile %struct.ScmObj* %x48097, %struct.ScmObj** %stackaddr$prim62103, align 8
%stackaddr$prim62104 = alloca %struct.ScmObj*, align 8
%cpsprim48449 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48097)
store volatile %struct.ScmObj* %cpsprim48449, %struct.ScmObj** %stackaddr$prim62104, align 8
%ae51859 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist60562$k484480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62105 = alloca %struct.ScmObj*, align 8
%argslist60562$k484481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48449, %struct.ScmObj* %argslist60562$k484480)
store volatile %struct.ScmObj* %argslist60562$k484481, %struct.ScmObj** %stackaddr$prim62105, align 8
%stackaddr$prim62106 = alloca %struct.ScmObj*, align 8
%argslist60562$k484482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51859, %struct.ScmObj* %argslist60562$k484481)
store volatile %struct.ScmObj* %argslist60562$k484482, %struct.ScmObj** %stackaddr$prim62106, align 8
%clofunc62107 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48448)
musttail call tailcc void %clofunc62107(%struct.ScmObj* %k48448, %struct.ScmObj* %argslist60562$k484482)
ret void
}

define tailcc void @proc_clo$ae51758(%struct.ScmObj* %env$ae51758,%struct.ScmObj* %args4809948450) {
%stackaddr$env-ref62108 = alloca %struct.ScmObj*, align 8
%_37foldl148038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51758, i64 0)
store %struct.ScmObj* %_37foldl148038, %struct.ScmObj** %stackaddr$env-ref62108
%stackaddr$prim62109 = alloca %struct.ScmObj*, align 8
%k48451 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4809948450)
store volatile %struct.ScmObj* %k48451, %struct.ScmObj** %stackaddr$prim62109, align 8
%stackaddr$prim62110 = alloca %struct.ScmObj*, align 8
%args48099 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4809948450)
store volatile %struct.ScmObj* %args48099, %struct.ScmObj** %stackaddr$prim62110, align 8
%stackaddr$prim62111 = alloca %struct.ScmObj*, align 8
%anf_45bind48280 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args48099)
store volatile %struct.ScmObj* %anf_45bind48280, %struct.ScmObj** %stackaddr$prim62111, align 8
%truthy$cmp62112 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48280)
%cmp$cmp62112 = icmp eq i64 %truthy$cmp62112, 1
br i1 %cmp$cmp62112, label %truebranch$cmp62112, label %falsebranch$cmp62112
truebranch$cmp62112:
%ae51764 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51765 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist60564$k484510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62113 = alloca %struct.ScmObj*, align 8
%argslist60564$k484511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51765, %struct.ScmObj* %argslist60564$k484510)
store volatile %struct.ScmObj* %argslist60564$k484511, %struct.ScmObj** %stackaddr$prim62113, align 8
%stackaddr$prim62114 = alloca %struct.ScmObj*, align 8
%argslist60564$k484512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51764, %struct.ScmObj* %argslist60564$k484511)
store volatile %struct.ScmObj* %argslist60564$k484512, %struct.ScmObj** %stackaddr$prim62114, align 8
%clofunc62115 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48451)
musttail call tailcc void %clofunc62115(%struct.ScmObj* %k48451, %struct.ScmObj* %argslist60564$k484512)
ret void
falsebranch$cmp62112:
%stackaddr$prim62116 = alloca %struct.ScmObj*, align 8
%anf_45bind48281 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48099)
store volatile %struct.ScmObj* %anf_45bind48281, %struct.ScmObj** %stackaddr$prim62116, align 8
%stackaddr$prim62117 = alloca %struct.ScmObj*, align 8
%anf_45bind48282 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48281)
store volatile %struct.ScmObj* %anf_45bind48282, %struct.ScmObj** %stackaddr$prim62117, align 8
%truthy$cmp62118 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48282)
%cmp$cmp62118 = icmp eq i64 %truthy$cmp62118, 1
br i1 %cmp$cmp62118, label %truebranch$cmp62118, label %falsebranch$cmp62118
truebranch$cmp62118:
%stackaddr$prim62119 = alloca %struct.ScmObj*, align 8
%cpsprim48452 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48099)
store volatile %struct.ScmObj* %cpsprim48452, %struct.ScmObj** %stackaddr$prim62119, align 8
%ae51777 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist60565$k484510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62120 = alloca %struct.ScmObj*, align 8
%argslist60565$k484511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48452, %struct.ScmObj* %argslist60565$k484510)
store volatile %struct.ScmObj* %argslist60565$k484511, %struct.ScmObj** %stackaddr$prim62120, align 8
%stackaddr$prim62121 = alloca %struct.ScmObj*, align 8
%argslist60565$k484512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51777, %struct.ScmObj* %argslist60565$k484511)
store volatile %struct.ScmObj* %argslist60565$k484512, %struct.ScmObj** %stackaddr$prim62121, align 8
%clofunc62122 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48451)
musttail call tailcc void %clofunc62122(%struct.ScmObj* %k48451, %struct.ScmObj* %argslist60565$k484512)
ret void
falsebranch$cmp62118:
%stackaddr$makeclosure62123 = alloca %struct.ScmObj*, align 8
%fptrToInt62124 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51782 to i64
%ae51782 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt62124)
store volatile %struct.ScmObj* %ae51782, %struct.ScmObj** %stackaddr$makeclosure62123, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51782, %struct.ScmObj* %args48099, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51782, %struct.ScmObj* %k48451, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51782, %struct.ScmObj* %_37foldl148038, i64 2)
%ae51783 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure62125 = alloca %struct.ScmObj*, align 8
%fptrToInt62126 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51784 to i64
%ae51784 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt62126)
store volatile %struct.ScmObj* %ae51784, %struct.ScmObj** %stackaddr$makeclosure62125, align 8
%argslist60575$ae517820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62127 = alloca %struct.ScmObj*, align 8
%argslist60575$ae517821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51784, %struct.ScmObj* %argslist60575$ae517820)
store volatile %struct.ScmObj* %argslist60575$ae517821, %struct.ScmObj** %stackaddr$prim62127, align 8
%stackaddr$prim62128 = alloca %struct.ScmObj*, align 8
%argslist60575$ae517822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51783, %struct.ScmObj* %argslist60575$ae517821)
store volatile %struct.ScmObj* %argslist60575$ae517822, %struct.ScmObj** %stackaddr$prim62128, align 8
%clofunc62129 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51782)
musttail call tailcc void %clofunc62129(%struct.ScmObj* %ae51782, %struct.ScmObj* %argslist60575$ae517822)
ret void
}

define tailcc void @proc_clo$ae51782(%struct.ScmObj* %env$ae51782,%struct.ScmObj* %current_45args60566) {
%stackaddr$env-ref62130 = alloca %struct.ScmObj*, align 8
%args48099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51782, i64 0)
store %struct.ScmObj* %args48099, %struct.ScmObj** %stackaddr$env-ref62130
%stackaddr$env-ref62131 = alloca %struct.ScmObj*, align 8
%k48451 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51782, i64 1)
store %struct.ScmObj* %k48451, %struct.ScmObj** %stackaddr$env-ref62131
%stackaddr$env-ref62132 = alloca %struct.ScmObj*, align 8
%_37foldl148038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51782, i64 2)
store %struct.ScmObj* %_37foldl148038, %struct.ScmObj** %stackaddr$env-ref62132
%stackaddr$prim62133 = alloca %struct.ScmObj*, align 8
%_95k48453 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60566)
store volatile %struct.ScmObj* %_95k48453, %struct.ScmObj** %stackaddr$prim62133, align 8
%stackaddr$prim62134 = alloca %struct.ScmObj*, align 8
%current_45args60567 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60566)
store volatile %struct.ScmObj* %current_45args60567, %struct.ScmObj** %stackaddr$prim62134, align 8
%stackaddr$prim62135 = alloca %struct.ScmObj*, align 8
%anf_45bind48283 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60567)
store volatile %struct.ScmObj* %anf_45bind48283, %struct.ScmObj** %stackaddr$prim62135, align 8
%stackaddr$prim62136 = alloca %struct.ScmObj*, align 8
%anf_45bind48284 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48099)
store volatile %struct.ScmObj* %anf_45bind48284, %struct.ScmObj** %stackaddr$prim62136, align 8
%stackaddr$prim62137 = alloca %struct.ScmObj*, align 8
%anf_45bind48285 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48099)
store volatile %struct.ScmObj* %anf_45bind48285, %struct.ScmObj** %stackaddr$prim62137, align 8
%argslist60569$_37foldl1480380 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62138 = alloca %struct.ScmObj*, align 8
%argslist60569$_37foldl1480381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48285, %struct.ScmObj* %argslist60569$_37foldl1480380)
store volatile %struct.ScmObj* %argslist60569$_37foldl1480381, %struct.ScmObj** %stackaddr$prim62138, align 8
%stackaddr$prim62139 = alloca %struct.ScmObj*, align 8
%argslist60569$_37foldl1480382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48284, %struct.ScmObj* %argslist60569$_37foldl1480381)
store volatile %struct.ScmObj* %argslist60569$_37foldl1480382, %struct.ScmObj** %stackaddr$prim62139, align 8
%stackaddr$prim62140 = alloca %struct.ScmObj*, align 8
%argslist60569$_37foldl1480383 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48283, %struct.ScmObj* %argslist60569$_37foldl1480382)
store volatile %struct.ScmObj* %argslist60569$_37foldl1480383, %struct.ScmObj** %stackaddr$prim62140, align 8
%stackaddr$prim62141 = alloca %struct.ScmObj*, align 8
%argslist60569$_37foldl1480384 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48451, %struct.ScmObj* %argslist60569$_37foldl1480383)
store volatile %struct.ScmObj* %argslist60569$_37foldl1480384, %struct.ScmObj** %stackaddr$prim62141, align 8
%clofunc62142 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148038)
musttail call tailcc void %clofunc62142(%struct.ScmObj* %_37foldl148038, %struct.ScmObj* %argslist60569$_37foldl1480384)
ret void
}

define tailcc void @proc_clo$ae51784(%struct.ScmObj* %env$ae51784,%struct.ScmObj* %current_45args60570) {
%stackaddr$prim62143 = alloca %struct.ScmObj*, align 8
%k48454 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60570)
store volatile %struct.ScmObj* %k48454, %struct.ScmObj** %stackaddr$prim62143, align 8
%stackaddr$prim62144 = alloca %struct.ScmObj*, align 8
%current_45args60571 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60570)
store volatile %struct.ScmObj* %current_45args60571, %struct.ScmObj** %stackaddr$prim62144, align 8
%stackaddr$prim62145 = alloca %struct.ScmObj*, align 8
%n48101 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60571)
store volatile %struct.ScmObj* %n48101, %struct.ScmObj** %stackaddr$prim62145, align 8
%stackaddr$prim62146 = alloca %struct.ScmObj*, align 8
%current_45args60572 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60571)
store volatile %struct.ScmObj* %current_45args60572, %struct.ScmObj** %stackaddr$prim62146, align 8
%stackaddr$prim62147 = alloca %struct.ScmObj*, align 8
%v48100 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60572)
store volatile %struct.ScmObj* %v48100, %struct.ScmObj** %stackaddr$prim62147, align 8
%stackaddr$prim62148 = alloca %struct.ScmObj*, align 8
%cpsprim48455 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v48100, %struct.ScmObj* %n48101)
store volatile %struct.ScmObj* %cpsprim48455, %struct.ScmObj** %stackaddr$prim62148, align 8
%ae51788 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist60574$k484540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62149 = alloca %struct.ScmObj*, align 8
%argslist60574$k484541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48455, %struct.ScmObj* %argslist60574$k484540)
store volatile %struct.ScmObj* %argslist60574$k484541, %struct.ScmObj** %stackaddr$prim62149, align 8
%stackaddr$prim62150 = alloca %struct.ScmObj*, align 8
%argslist60574$k484542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51788, %struct.ScmObj* %argslist60574$k484541)
store volatile %struct.ScmObj* %argslist60574$k484542, %struct.ScmObj** %stackaddr$prim62150, align 8
%clofunc62151 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48454)
musttail call tailcc void %clofunc62151(%struct.ScmObj* %k48454, %struct.ScmObj* %argslist60574$k484542)
ret void
}

define tailcc void @proc_clo$ae51354(%struct.ScmObj* %env$ae51354,%struct.ScmObj* %current_45args60577) {
%stackaddr$prim62152 = alloca %struct.ScmObj*, align 8
%k48456 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60577)
store volatile %struct.ScmObj* %k48456, %struct.ScmObj** %stackaddr$prim62152, align 8
%stackaddr$prim62153 = alloca %struct.ScmObj*, align 8
%current_45args60578 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60577)
store volatile %struct.ScmObj* %current_45args60578, %struct.ScmObj** %stackaddr$prim62153, align 8
%stackaddr$prim62154 = alloca %struct.ScmObj*, align 8
%v48104 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60578)
store volatile %struct.ScmObj* %v48104, %struct.ScmObj** %stackaddr$prim62154, align 8
%stackaddr$prim62155 = alloca %struct.ScmObj*, align 8
%current_45args60579 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60578)
store volatile %struct.ScmObj* %current_45args60579, %struct.ScmObj** %stackaddr$prim62155, align 8
%stackaddr$prim62156 = alloca %struct.ScmObj*, align 8
%lst48103 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60579)
store volatile %struct.ScmObj* %lst48103, %struct.ScmObj** %stackaddr$prim62156, align 8
%ae51355 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim62157 = alloca %struct.ScmObj*, align 8
%lst48105 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51355, %struct.ScmObj* %lst48103)
store volatile %struct.ScmObj* %lst48105, %struct.ScmObj** %stackaddr$prim62157, align 8
%stackaddr$makeclosure62158 = alloca %struct.ScmObj*, align 8
%fptrToInt62159 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51357 to i64
%ae51357 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt62159)
store volatile %struct.ScmObj* %ae51357, %struct.ScmObj** %stackaddr$makeclosure62158, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51357, %struct.ScmObj* %v48104, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51357, %struct.ScmObj* %k48456, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51357, %struct.ScmObj* %lst48105, i64 2)
%ae51358 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure62160 = alloca %struct.ScmObj*, align 8
%fptrToInt62161 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51359 to i64
%ae51359 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt62161)
store volatile %struct.ScmObj* %ae51359, %struct.ScmObj** %stackaddr$makeclosure62160, align 8
%argslist60601$ae513570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62162 = alloca %struct.ScmObj*, align 8
%argslist60601$ae513571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51359, %struct.ScmObj* %argslist60601$ae513570)
store volatile %struct.ScmObj* %argslist60601$ae513571, %struct.ScmObj** %stackaddr$prim62162, align 8
%stackaddr$prim62163 = alloca %struct.ScmObj*, align 8
%argslist60601$ae513572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51358, %struct.ScmObj* %argslist60601$ae513571)
store volatile %struct.ScmObj* %argslist60601$ae513572, %struct.ScmObj** %stackaddr$prim62163, align 8
%clofunc62164 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51357)
musttail call tailcc void %clofunc62164(%struct.ScmObj* %ae51357, %struct.ScmObj* %argslist60601$ae513572)
ret void
}

define tailcc void @proc_clo$ae51357(%struct.ScmObj* %env$ae51357,%struct.ScmObj* %current_45args60581) {
%stackaddr$env-ref62165 = alloca %struct.ScmObj*, align 8
%v48104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51357, i64 0)
store %struct.ScmObj* %v48104, %struct.ScmObj** %stackaddr$env-ref62165
%stackaddr$env-ref62166 = alloca %struct.ScmObj*, align 8
%k48456 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51357, i64 1)
store %struct.ScmObj* %k48456, %struct.ScmObj** %stackaddr$env-ref62166
%stackaddr$env-ref62167 = alloca %struct.ScmObj*, align 8
%lst48105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51357, i64 2)
store %struct.ScmObj* %lst48105, %struct.ScmObj** %stackaddr$env-ref62167
%stackaddr$prim62168 = alloca %struct.ScmObj*, align 8
%_95k48457 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60581)
store volatile %struct.ScmObj* %_95k48457, %struct.ScmObj** %stackaddr$prim62168, align 8
%stackaddr$prim62169 = alloca %struct.ScmObj*, align 8
%current_45args60582 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60581)
store volatile %struct.ScmObj* %current_45args60582, %struct.ScmObj** %stackaddr$prim62169, align 8
%stackaddr$prim62170 = alloca %struct.ScmObj*, align 8
%anf_45bind48272 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60582)
store volatile %struct.ScmObj* %anf_45bind48272, %struct.ScmObj** %stackaddr$prim62170, align 8
%stackaddr$makeclosure62171 = alloca %struct.ScmObj*, align 8
%fptrToInt62172 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51373 to i64
%ae51373 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt62172)
store volatile %struct.ScmObj* %ae51373, %struct.ScmObj** %stackaddr$makeclosure62171, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51373, %struct.ScmObj* %v48104, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51373, %struct.ScmObj* %k48456, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51373, %struct.ScmObj* %lst48105, i64 2)
%stackaddr$makeclosure62173 = alloca %struct.ScmObj*, align 8
%fptrToInt62174 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51374 to i64
%ae51374 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt62174)
store volatile %struct.ScmObj* %ae51374, %struct.ScmObj** %stackaddr$makeclosure62173, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51374, %struct.ScmObj* %v48104, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51374, %struct.ScmObj* %k48456, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51374, %struct.ScmObj* %lst48105, i64 2)
%argslist60596$anf_45bind482720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62175 = alloca %struct.ScmObj*, align 8
%argslist60596$anf_45bind482721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51374, %struct.ScmObj* %argslist60596$anf_45bind482720)
store volatile %struct.ScmObj* %argslist60596$anf_45bind482721, %struct.ScmObj** %stackaddr$prim62175, align 8
%stackaddr$prim62176 = alloca %struct.ScmObj*, align 8
%argslist60596$anf_45bind482722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51373, %struct.ScmObj* %argslist60596$anf_45bind482721)
store volatile %struct.ScmObj* %argslist60596$anf_45bind482722, %struct.ScmObj** %stackaddr$prim62176, align 8
%clofunc62177 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48272)
musttail call tailcc void %clofunc62177(%struct.ScmObj* %anf_45bind48272, %struct.ScmObj* %argslist60596$anf_45bind482722)
ret void
}

define tailcc void @proc_clo$ae51373(%struct.ScmObj* %env$ae51373,%struct.ScmObj* %current_45args60584) {
%stackaddr$env-ref62178 = alloca %struct.ScmObj*, align 8
%v48104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51373, i64 0)
store %struct.ScmObj* %v48104, %struct.ScmObj** %stackaddr$env-ref62178
%stackaddr$env-ref62179 = alloca %struct.ScmObj*, align 8
%k48456 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51373, i64 1)
store %struct.ScmObj* %k48456, %struct.ScmObj** %stackaddr$env-ref62179
%stackaddr$env-ref62180 = alloca %struct.ScmObj*, align 8
%lst48105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51373, i64 2)
store %struct.ScmObj* %lst48105, %struct.ScmObj** %stackaddr$env-ref62180
%stackaddr$prim62181 = alloca %struct.ScmObj*, align 8
%_95k48458 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60584)
store volatile %struct.ScmObj* %_95k48458, %struct.ScmObj** %stackaddr$prim62181, align 8
%stackaddr$prim62182 = alloca %struct.ScmObj*, align 8
%current_45args60585 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60584)
store volatile %struct.ScmObj* %current_45args60585, %struct.ScmObj** %stackaddr$prim62182, align 8
%stackaddr$prim62183 = alloca %struct.ScmObj*, align 8
%cc48106 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60585)
store volatile %struct.ScmObj* %cc48106, %struct.ScmObj** %stackaddr$prim62183, align 8
%ae51482 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim62184 = alloca %struct.ScmObj*, align 8
%anf_45bind48273 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48105, %struct.ScmObj* %ae51482)
store volatile %struct.ScmObj* %anf_45bind48273, %struct.ScmObj** %stackaddr$prim62184, align 8
%stackaddr$prim62185 = alloca %struct.ScmObj*, align 8
%anf_45bind48274 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48273)
store volatile %struct.ScmObj* %anf_45bind48274, %struct.ScmObj** %stackaddr$prim62185, align 8
%truthy$cmp62186 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48274)
%cmp$cmp62186 = icmp eq i64 %truthy$cmp62186, 1
br i1 %cmp$cmp62186, label %truebranch$cmp62186, label %falsebranch$cmp62186
truebranch$cmp62186:
%ae51486 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51487 = call %struct.ScmObj* @const_init_false()
%argslist60587$k484560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62187 = alloca %struct.ScmObj*, align 8
%argslist60587$k484561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51487, %struct.ScmObj* %argslist60587$k484560)
store volatile %struct.ScmObj* %argslist60587$k484561, %struct.ScmObj** %stackaddr$prim62187, align 8
%stackaddr$prim62188 = alloca %struct.ScmObj*, align 8
%argslist60587$k484562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51486, %struct.ScmObj* %argslist60587$k484561)
store volatile %struct.ScmObj* %argslist60587$k484562, %struct.ScmObj** %stackaddr$prim62188, align 8
%clofunc62189 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48456)
musttail call tailcc void %clofunc62189(%struct.ScmObj* %k48456, %struct.ScmObj* %argslist60587$k484562)
ret void
falsebranch$cmp62186:
%ae51495 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim62190 = alloca %struct.ScmObj*, align 8
%anf_45bind48275 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48105, %struct.ScmObj* %ae51495)
store volatile %struct.ScmObj* %anf_45bind48275, %struct.ScmObj** %stackaddr$prim62190, align 8
%stackaddr$prim62191 = alloca %struct.ScmObj*, align 8
%anf_45bind48276 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48275)
store volatile %struct.ScmObj* %anf_45bind48276, %struct.ScmObj** %stackaddr$prim62191, align 8
%stackaddr$prim62192 = alloca %struct.ScmObj*, align 8
%anf_45bind48277 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind48276, %struct.ScmObj* %v48104)
store volatile %struct.ScmObj* %anf_45bind48277, %struct.ScmObj** %stackaddr$prim62192, align 8
%truthy$cmp62193 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48277)
%cmp$cmp62193 = icmp eq i64 %truthy$cmp62193, 1
br i1 %cmp$cmp62193, label %truebranch$cmp62193, label %falsebranch$cmp62193
truebranch$cmp62193:
%ae51501 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim62194 = alloca %struct.ScmObj*, align 8
%cpsprim48459 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48105, %struct.ScmObj* %ae51501)
store volatile %struct.ScmObj* %cpsprim48459, %struct.ScmObj** %stackaddr$prim62194, align 8
%ae51503 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist60588$k484560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62195 = alloca %struct.ScmObj*, align 8
%argslist60588$k484561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48459, %struct.ScmObj* %argslist60588$k484560)
store volatile %struct.ScmObj* %argslist60588$k484561, %struct.ScmObj** %stackaddr$prim62195, align 8
%stackaddr$prim62196 = alloca %struct.ScmObj*, align 8
%argslist60588$k484562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51503, %struct.ScmObj* %argslist60588$k484561)
store volatile %struct.ScmObj* %argslist60588$k484562, %struct.ScmObj** %stackaddr$prim62196, align 8
%clofunc62197 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48456)
musttail call tailcc void %clofunc62197(%struct.ScmObj* %k48456, %struct.ScmObj* %argslist60588$k484562)
ret void
falsebranch$cmp62193:
%ae51514 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim62198 = alloca %struct.ScmObj*, align 8
%anf_45bind48278 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48105, %struct.ScmObj* %ae51514)
store volatile %struct.ScmObj* %anf_45bind48278, %struct.ScmObj** %stackaddr$prim62198, align 8
%stackaddr$prim62199 = alloca %struct.ScmObj*, align 8
%anf_45bind48279 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48278)
store volatile %struct.ScmObj* %anf_45bind48279, %struct.ScmObj** %stackaddr$prim62199, align 8
%ae51517 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim62200 = alloca %struct.ScmObj*, align 8
%_95048108 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48105, %struct.ScmObj* %ae51517, %struct.ScmObj* %anf_45bind48279)
store volatile %struct.ScmObj* %_95048108, %struct.ScmObj** %stackaddr$prim62200, align 8
%argslist60589$cc481060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62201 = alloca %struct.ScmObj*, align 8
%argslist60589$cc481061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48106, %struct.ScmObj* %argslist60589$cc481060)
store volatile %struct.ScmObj* %argslist60589$cc481061, %struct.ScmObj** %stackaddr$prim62201, align 8
%stackaddr$prim62202 = alloca %struct.ScmObj*, align 8
%argslist60589$cc481062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48456, %struct.ScmObj* %argslist60589$cc481061)
store volatile %struct.ScmObj* %argslist60589$cc481062, %struct.ScmObj** %stackaddr$prim62202, align 8
%clofunc62203 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48106)
musttail call tailcc void %clofunc62203(%struct.ScmObj* %cc48106, %struct.ScmObj* %argslist60589$cc481062)
ret void
}

define tailcc void @proc_clo$ae51374(%struct.ScmObj* %env$ae51374,%struct.ScmObj* %current_45args60590) {
%stackaddr$env-ref62204 = alloca %struct.ScmObj*, align 8
%v48104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51374, i64 0)
store %struct.ScmObj* %v48104, %struct.ScmObj** %stackaddr$env-ref62204
%stackaddr$env-ref62205 = alloca %struct.ScmObj*, align 8
%k48456 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51374, i64 1)
store %struct.ScmObj* %k48456, %struct.ScmObj** %stackaddr$env-ref62205
%stackaddr$env-ref62206 = alloca %struct.ScmObj*, align 8
%lst48105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51374, i64 2)
store %struct.ScmObj* %lst48105, %struct.ScmObj** %stackaddr$env-ref62206
%stackaddr$prim62207 = alloca %struct.ScmObj*, align 8
%_95k48458 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60590)
store volatile %struct.ScmObj* %_95k48458, %struct.ScmObj** %stackaddr$prim62207, align 8
%stackaddr$prim62208 = alloca %struct.ScmObj*, align 8
%current_45args60591 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60590)
store volatile %struct.ScmObj* %current_45args60591, %struct.ScmObj** %stackaddr$prim62208, align 8
%stackaddr$prim62209 = alloca %struct.ScmObj*, align 8
%cc48106 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60591)
store volatile %struct.ScmObj* %cc48106, %struct.ScmObj** %stackaddr$prim62209, align 8
%ae51376 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim62210 = alloca %struct.ScmObj*, align 8
%anf_45bind48273 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48105, %struct.ScmObj* %ae51376)
store volatile %struct.ScmObj* %anf_45bind48273, %struct.ScmObj** %stackaddr$prim62210, align 8
%stackaddr$prim62211 = alloca %struct.ScmObj*, align 8
%anf_45bind48274 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48273)
store volatile %struct.ScmObj* %anf_45bind48274, %struct.ScmObj** %stackaddr$prim62211, align 8
%truthy$cmp62212 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48274)
%cmp$cmp62212 = icmp eq i64 %truthy$cmp62212, 1
br i1 %cmp$cmp62212, label %truebranch$cmp62212, label %falsebranch$cmp62212
truebranch$cmp62212:
%ae51380 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51381 = call %struct.ScmObj* @const_init_false()
%argslist60593$k484560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62213 = alloca %struct.ScmObj*, align 8
%argslist60593$k484561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51381, %struct.ScmObj* %argslist60593$k484560)
store volatile %struct.ScmObj* %argslist60593$k484561, %struct.ScmObj** %stackaddr$prim62213, align 8
%stackaddr$prim62214 = alloca %struct.ScmObj*, align 8
%argslist60593$k484562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51380, %struct.ScmObj* %argslist60593$k484561)
store volatile %struct.ScmObj* %argslist60593$k484562, %struct.ScmObj** %stackaddr$prim62214, align 8
%clofunc62215 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48456)
musttail call tailcc void %clofunc62215(%struct.ScmObj* %k48456, %struct.ScmObj* %argslist60593$k484562)
ret void
falsebranch$cmp62212:
%ae51389 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim62216 = alloca %struct.ScmObj*, align 8
%anf_45bind48275 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48105, %struct.ScmObj* %ae51389)
store volatile %struct.ScmObj* %anf_45bind48275, %struct.ScmObj** %stackaddr$prim62216, align 8
%stackaddr$prim62217 = alloca %struct.ScmObj*, align 8
%anf_45bind48276 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48275)
store volatile %struct.ScmObj* %anf_45bind48276, %struct.ScmObj** %stackaddr$prim62217, align 8
%stackaddr$prim62218 = alloca %struct.ScmObj*, align 8
%anf_45bind48277 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind48276, %struct.ScmObj* %v48104)
store volatile %struct.ScmObj* %anf_45bind48277, %struct.ScmObj** %stackaddr$prim62218, align 8
%truthy$cmp62219 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48277)
%cmp$cmp62219 = icmp eq i64 %truthy$cmp62219, 1
br i1 %cmp$cmp62219, label %truebranch$cmp62219, label %falsebranch$cmp62219
truebranch$cmp62219:
%ae51395 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim62220 = alloca %struct.ScmObj*, align 8
%cpsprim48459 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48105, %struct.ScmObj* %ae51395)
store volatile %struct.ScmObj* %cpsprim48459, %struct.ScmObj** %stackaddr$prim62220, align 8
%ae51397 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist60594$k484560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62221 = alloca %struct.ScmObj*, align 8
%argslist60594$k484561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48459, %struct.ScmObj* %argslist60594$k484560)
store volatile %struct.ScmObj* %argslist60594$k484561, %struct.ScmObj** %stackaddr$prim62221, align 8
%stackaddr$prim62222 = alloca %struct.ScmObj*, align 8
%argslist60594$k484562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51397, %struct.ScmObj* %argslist60594$k484561)
store volatile %struct.ScmObj* %argslist60594$k484562, %struct.ScmObj** %stackaddr$prim62222, align 8
%clofunc62223 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48456)
musttail call tailcc void %clofunc62223(%struct.ScmObj* %k48456, %struct.ScmObj* %argslist60594$k484562)
ret void
falsebranch$cmp62219:
%ae51408 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim62224 = alloca %struct.ScmObj*, align 8
%anf_45bind48278 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48105, %struct.ScmObj* %ae51408)
store volatile %struct.ScmObj* %anf_45bind48278, %struct.ScmObj** %stackaddr$prim62224, align 8
%stackaddr$prim62225 = alloca %struct.ScmObj*, align 8
%anf_45bind48279 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48278)
store volatile %struct.ScmObj* %anf_45bind48279, %struct.ScmObj** %stackaddr$prim62225, align 8
%ae51411 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim62226 = alloca %struct.ScmObj*, align 8
%_95048108 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48105, %struct.ScmObj* %ae51411, %struct.ScmObj* %anf_45bind48279)
store volatile %struct.ScmObj* %_95048108, %struct.ScmObj** %stackaddr$prim62226, align 8
%argslist60595$cc481060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62227 = alloca %struct.ScmObj*, align 8
%argslist60595$cc481061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48106, %struct.ScmObj* %argslist60595$cc481060)
store volatile %struct.ScmObj* %argslist60595$cc481061, %struct.ScmObj** %stackaddr$prim62227, align 8
%stackaddr$prim62228 = alloca %struct.ScmObj*, align 8
%argslist60595$cc481062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48456, %struct.ScmObj* %argslist60595$cc481061)
store volatile %struct.ScmObj* %argslist60595$cc481062, %struct.ScmObj** %stackaddr$prim62228, align 8
%clofunc62229 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48106)
musttail call tailcc void %clofunc62229(%struct.ScmObj* %cc48106, %struct.ScmObj* %argslist60595$cc481062)
ret void
}

define tailcc void @proc_clo$ae51359(%struct.ScmObj* %env$ae51359,%struct.ScmObj* %current_45args60597) {
%stackaddr$prim62230 = alloca %struct.ScmObj*, align 8
%k48460 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60597)
store volatile %struct.ScmObj* %k48460, %struct.ScmObj** %stackaddr$prim62230, align 8
%stackaddr$prim62231 = alloca %struct.ScmObj*, align 8
%current_45args60598 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60597)
store volatile %struct.ScmObj* %current_45args60598, %struct.ScmObj** %stackaddr$prim62231, align 8
%stackaddr$prim62232 = alloca %struct.ScmObj*, align 8
%u48107 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60598)
store volatile %struct.ScmObj* %u48107, %struct.ScmObj** %stackaddr$prim62232, align 8
%argslist60600$u481070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62233 = alloca %struct.ScmObj*, align 8
%argslist60600$u481071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u48107, %struct.ScmObj* %argslist60600$u481070)
store volatile %struct.ScmObj* %argslist60600$u481071, %struct.ScmObj** %stackaddr$prim62233, align 8
%stackaddr$prim62234 = alloca %struct.ScmObj*, align 8
%argslist60600$u481072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48460, %struct.ScmObj* %argslist60600$u481071)
store volatile %struct.ScmObj* %argslist60600$u481072, %struct.ScmObj** %stackaddr$prim62234, align 8
%clofunc62235 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u48107)
musttail call tailcc void %clofunc62235(%struct.ScmObj* %u48107, %struct.ScmObj* %argslist60600$u481072)
ret void
}

define tailcc void @proc_clo$ae50818(%struct.ScmObj* %env$ae50818,%struct.ScmObj* %current_45args60603) {
%stackaddr$prim62236 = alloca %struct.ScmObj*, align 8
%k48461 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60603)
store volatile %struct.ScmObj* %k48461, %struct.ScmObj** %stackaddr$prim62236, align 8
%stackaddr$prim62237 = alloca %struct.ScmObj*, align 8
%current_45args60604 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60603)
store volatile %struct.ScmObj* %current_45args60604, %struct.ScmObj** %stackaddr$prim62237, align 8
%stackaddr$prim62238 = alloca %struct.ScmObj*, align 8
%lst48111 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60604)
store volatile %struct.ScmObj* %lst48111, %struct.ScmObj** %stackaddr$prim62238, align 8
%stackaddr$prim62239 = alloca %struct.ScmObj*, align 8
%current_45args60605 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60604)
store volatile %struct.ScmObj* %current_45args60605, %struct.ScmObj** %stackaddr$prim62239, align 8
%stackaddr$prim62240 = alloca %struct.ScmObj*, align 8
%n48110 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60605)
store volatile %struct.ScmObj* %n48110, %struct.ScmObj** %stackaddr$prim62240, align 8
%ae50819 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim62241 = alloca %struct.ScmObj*, align 8
%n48113 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50819, %struct.ScmObj* %n48110)
store volatile %struct.ScmObj* %n48113, %struct.ScmObj** %stackaddr$prim62241, align 8
%ae50821 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim62242 = alloca %struct.ScmObj*, align 8
%lst48112 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50821, %struct.ScmObj* %lst48111)
store volatile %struct.ScmObj* %lst48112, %struct.ScmObj** %stackaddr$prim62242, align 8
%stackaddr$makeclosure62243 = alloca %struct.ScmObj*, align 8
%fptrToInt62244 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50823 to i64
%ae50823 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt62244)
store volatile %struct.ScmObj* %ae50823, %struct.ScmObj** %stackaddr$makeclosure62243, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50823, %struct.ScmObj* %n48113, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50823, %struct.ScmObj* %lst48112, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50823, %struct.ScmObj* %k48461, i64 2)
%ae50824 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure62245 = alloca %struct.ScmObj*, align 8
%fptrToInt62246 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50825 to i64
%ae50825 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt62246)
store volatile %struct.ScmObj* %ae50825, %struct.ScmObj** %stackaddr$makeclosure62245, align 8
%argslist60625$ae508230 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62247 = alloca %struct.ScmObj*, align 8
%argslist60625$ae508231 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50825, %struct.ScmObj* %argslist60625$ae508230)
store volatile %struct.ScmObj* %argslist60625$ae508231, %struct.ScmObj** %stackaddr$prim62247, align 8
%stackaddr$prim62248 = alloca %struct.ScmObj*, align 8
%argslist60625$ae508232 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50824, %struct.ScmObj* %argslist60625$ae508231)
store volatile %struct.ScmObj* %argslist60625$ae508232, %struct.ScmObj** %stackaddr$prim62248, align 8
%clofunc62249 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50823)
musttail call tailcc void %clofunc62249(%struct.ScmObj* %ae50823, %struct.ScmObj* %argslist60625$ae508232)
ret void
}

define tailcc void @proc_clo$ae50823(%struct.ScmObj* %env$ae50823,%struct.ScmObj* %current_45args60607) {
%stackaddr$env-ref62250 = alloca %struct.ScmObj*, align 8
%n48113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50823, i64 0)
store %struct.ScmObj* %n48113, %struct.ScmObj** %stackaddr$env-ref62250
%stackaddr$env-ref62251 = alloca %struct.ScmObj*, align 8
%lst48112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50823, i64 1)
store %struct.ScmObj* %lst48112, %struct.ScmObj** %stackaddr$env-ref62251
%stackaddr$env-ref62252 = alloca %struct.ScmObj*, align 8
%k48461 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50823, i64 2)
store %struct.ScmObj* %k48461, %struct.ScmObj** %stackaddr$env-ref62252
%stackaddr$prim62253 = alloca %struct.ScmObj*, align 8
%_95k48462 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60607)
store volatile %struct.ScmObj* %_95k48462, %struct.ScmObj** %stackaddr$prim62253, align 8
%stackaddr$prim62254 = alloca %struct.ScmObj*, align 8
%current_45args60608 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60607)
store volatile %struct.ScmObj* %current_45args60608, %struct.ScmObj** %stackaddr$prim62254, align 8
%stackaddr$prim62255 = alloca %struct.ScmObj*, align 8
%anf_45bind48265 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60608)
store volatile %struct.ScmObj* %anf_45bind48265, %struct.ScmObj** %stackaddr$prim62255, align 8
%stackaddr$makeclosure62256 = alloca %struct.ScmObj*, align 8
%fptrToInt62257 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50839 to i64
%ae50839 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt62257)
store volatile %struct.ScmObj* %ae50839, %struct.ScmObj** %stackaddr$makeclosure62256, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50839, %struct.ScmObj* %n48113, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50839, %struct.ScmObj* %lst48112, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50839, %struct.ScmObj* %k48461, i64 2)
%stackaddr$makeclosure62258 = alloca %struct.ScmObj*, align 8
%fptrToInt62259 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50840 to i64
%ae50840 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt62259)
store volatile %struct.ScmObj* %ae50840, %struct.ScmObj** %stackaddr$makeclosure62258, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50840, %struct.ScmObj* %n48113, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50840, %struct.ScmObj* %lst48112, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50840, %struct.ScmObj* %k48461, i64 2)
%argslist60620$anf_45bind482650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62260 = alloca %struct.ScmObj*, align 8
%argslist60620$anf_45bind482651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50840, %struct.ScmObj* %argslist60620$anf_45bind482650)
store volatile %struct.ScmObj* %argslist60620$anf_45bind482651, %struct.ScmObj** %stackaddr$prim62260, align 8
%stackaddr$prim62261 = alloca %struct.ScmObj*, align 8
%argslist60620$anf_45bind482652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50839, %struct.ScmObj* %argslist60620$anf_45bind482651)
store volatile %struct.ScmObj* %argslist60620$anf_45bind482652, %struct.ScmObj** %stackaddr$prim62261, align 8
%clofunc62262 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48265)
musttail call tailcc void %clofunc62262(%struct.ScmObj* %anf_45bind48265, %struct.ScmObj* %argslist60620$anf_45bind482652)
ret void
}

define tailcc void @proc_clo$ae50839(%struct.ScmObj* %env$ae50839,%struct.ScmObj* %current_45args60610) {
%stackaddr$env-ref62263 = alloca %struct.ScmObj*, align 8
%n48113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50839, i64 0)
store %struct.ScmObj* %n48113, %struct.ScmObj** %stackaddr$env-ref62263
%stackaddr$env-ref62264 = alloca %struct.ScmObj*, align 8
%lst48112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50839, i64 1)
store %struct.ScmObj* %lst48112, %struct.ScmObj** %stackaddr$env-ref62264
%stackaddr$env-ref62265 = alloca %struct.ScmObj*, align 8
%k48461 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50839, i64 2)
store %struct.ScmObj* %k48461, %struct.ScmObj** %stackaddr$env-ref62265
%stackaddr$prim62266 = alloca %struct.ScmObj*, align 8
%_95k48463 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60610)
store volatile %struct.ScmObj* %_95k48463, %struct.ScmObj** %stackaddr$prim62266, align 8
%stackaddr$prim62267 = alloca %struct.ScmObj*, align 8
%current_45args60611 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60610)
store volatile %struct.ScmObj* %current_45args60611, %struct.ScmObj** %stackaddr$prim62267, align 8
%stackaddr$prim62268 = alloca %struct.ScmObj*, align 8
%cc48114 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60611)
store volatile %struct.ScmObj* %cc48114, %struct.ScmObj** %stackaddr$prim62268, align 8
%ae50982 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim62269 = alloca %struct.ScmObj*, align 8
%anf_45bind48266 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48113, %struct.ScmObj* %ae50982)
store volatile %struct.ScmObj* %anf_45bind48266, %struct.ScmObj** %stackaddr$prim62269, align 8
%ae50983 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim62270 = alloca %struct.ScmObj*, align 8
%anf_45bind48267 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae50983, %struct.ScmObj* %anf_45bind48266)
store volatile %struct.ScmObj* %anf_45bind48267, %struct.ScmObj** %stackaddr$prim62270, align 8
%truthy$cmp62271 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48267)
%cmp$cmp62271 = icmp eq i64 %truthy$cmp62271, 1
br i1 %cmp$cmp62271, label %truebranch$cmp62271, label %falsebranch$cmp62271
truebranch$cmp62271:
%ae50987 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim62272 = alloca %struct.ScmObj*, align 8
%cpsprim48464 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48112, %struct.ScmObj* %ae50987)
store volatile %struct.ScmObj* %cpsprim48464, %struct.ScmObj** %stackaddr$prim62272, align 8
%ae50989 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist60613$k484610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62273 = alloca %struct.ScmObj*, align 8
%argslist60613$k484611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48464, %struct.ScmObj* %argslist60613$k484610)
store volatile %struct.ScmObj* %argslist60613$k484611, %struct.ScmObj** %stackaddr$prim62273, align 8
%stackaddr$prim62274 = alloca %struct.ScmObj*, align 8
%argslist60613$k484612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50989, %struct.ScmObj* %argslist60613$k484611)
store volatile %struct.ScmObj* %argslist60613$k484612, %struct.ScmObj** %stackaddr$prim62274, align 8
%clofunc62275 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48461)
musttail call tailcc void %clofunc62275(%struct.ScmObj* %k48461, %struct.ScmObj* %argslist60613$k484612)
ret void
falsebranch$cmp62271:
%ae51000 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim62276 = alloca %struct.ScmObj*, align 8
%anf_45bind48268 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48112, %struct.ScmObj* %ae51000)
store volatile %struct.ScmObj* %anf_45bind48268, %struct.ScmObj** %stackaddr$prim62276, align 8
%stackaddr$prim62277 = alloca %struct.ScmObj*, align 8
%anf_45bind48269 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48268)
store volatile %struct.ScmObj* %anf_45bind48269, %struct.ScmObj** %stackaddr$prim62277, align 8
%ae51003 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim62278 = alloca %struct.ScmObj*, align 8
%_95048117 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48112, %struct.ScmObj* %ae51003, %struct.ScmObj* %anf_45bind48269)
store volatile %struct.ScmObj* %_95048117, %struct.ScmObj** %stackaddr$prim62278, align 8
%ae51006 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim62279 = alloca %struct.ScmObj*, align 8
%anf_45bind48270 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48113, %struct.ScmObj* %ae51006)
store volatile %struct.ScmObj* %anf_45bind48270, %struct.ScmObj** %stackaddr$prim62279, align 8
%ae51008 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim62280 = alloca %struct.ScmObj*, align 8
%anf_45bind48271 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind48270, %struct.ScmObj* %ae51008)
store volatile %struct.ScmObj* %anf_45bind48271, %struct.ScmObj** %stackaddr$prim62280, align 8
%ae51010 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim62281 = alloca %struct.ScmObj*, align 8
%_95148116 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n48113, %struct.ScmObj* %ae51010, %struct.ScmObj* %anf_45bind48271)
store volatile %struct.ScmObj* %_95148116, %struct.ScmObj** %stackaddr$prim62281, align 8
%argslist60614$cc481140 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62282 = alloca %struct.ScmObj*, align 8
%argslist60614$cc481141 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48114, %struct.ScmObj* %argslist60614$cc481140)
store volatile %struct.ScmObj* %argslist60614$cc481141, %struct.ScmObj** %stackaddr$prim62282, align 8
%stackaddr$prim62283 = alloca %struct.ScmObj*, align 8
%argslist60614$cc481142 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48461, %struct.ScmObj* %argslist60614$cc481141)
store volatile %struct.ScmObj* %argslist60614$cc481142, %struct.ScmObj** %stackaddr$prim62283, align 8
%clofunc62284 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48114)
musttail call tailcc void %clofunc62284(%struct.ScmObj* %cc48114, %struct.ScmObj* %argslist60614$cc481142)
ret void
}

define tailcc void @proc_clo$ae50840(%struct.ScmObj* %env$ae50840,%struct.ScmObj* %current_45args60615) {
%stackaddr$env-ref62285 = alloca %struct.ScmObj*, align 8
%n48113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50840, i64 0)
store %struct.ScmObj* %n48113, %struct.ScmObj** %stackaddr$env-ref62285
%stackaddr$env-ref62286 = alloca %struct.ScmObj*, align 8
%lst48112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50840, i64 1)
store %struct.ScmObj* %lst48112, %struct.ScmObj** %stackaddr$env-ref62286
%stackaddr$env-ref62287 = alloca %struct.ScmObj*, align 8
%k48461 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50840, i64 2)
store %struct.ScmObj* %k48461, %struct.ScmObj** %stackaddr$env-ref62287
%stackaddr$prim62288 = alloca %struct.ScmObj*, align 8
%_95k48463 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60615)
store volatile %struct.ScmObj* %_95k48463, %struct.ScmObj** %stackaddr$prim62288, align 8
%stackaddr$prim62289 = alloca %struct.ScmObj*, align 8
%current_45args60616 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60615)
store volatile %struct.ScmObj* %current_45args60616, %struct.ScmObj** %stackaddr$prim62289, align 8
%stackaddr$prim62290 = alloca %struct.ScmObj*, align 8
%cc48114 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60616)
store volatile %struct.ScmObj* %cc48114, %struct.ScmObj** %stackaddr$prim62290, align 8
%ae50842 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim62291 = alloca %struct.ScmObj*, align 8
%anf_45bind48266 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48113, %struct.ScmObj* %ae50842)
store volatile %struct.ScmObj* %anf_45bind48266, %struct.ScmObj** %stackaddr$prim62291, align 8
%ae50843 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim62292 = alloca %struct.ScmObj*, align 8
%anf_45bind48267 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae50843, %struct.ScmObj* %anf_45bind48266)
store volatile %struct.ScmObj* %anf_45bind48267, %struct.ScmObj** %stackaddr$prim62292, align 8
%truthy$cmp62293 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48267)
%cmp$cmp62293 = icmp eq i64 %truthy$cmp62293, 1
br i1 %cmp$cmp62293, label %truebranch$cmp62293, label %falsebranch$cmp62293
truebranch$cmp62293:
%ae50847 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim62294 = alloca %struct.ScmObj*, align 8
%cpsprim48464 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48112, %struct.ScmObj* %ae50847)
store volatile %struct.ScmObj* %cpsprim48464, %struct.ScmObj** %stackaddr$prim62294, align 8
%ae50849 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist60618$k484610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62295 = alloca %struct.ScmObj*, align 8
%argslist60618$k484611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48464, %struct.ScmObj* %argslist60618$k484610)
store volatile %struct.ScmObj* %argslist60618$k484611, %struct.ScmObj** %stackaddr$prim62295, align 8
%stackaddr$prim62296 = alloca %struct.ScmObj*, align 8
%argslist60618$k484612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50849, %struct.ScmObj* %argslist60618$k484611)
store volatile %struct.ScmObj* %argslist60618$k484612, %struct.ScmObj** %stackaddr$prim62296, align 8
%clofunc62297 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48461)
musttail call tailcc void %clofunc62297(%struct.ScmObj* %k48461, %struct.ScmObj* %argslist60618$k484612)
ret void
falsebranch$cmp62293:
%ae50860 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim62298 = alloca %struct.ScmObj*, align 8
%anf_45bind48268 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48112, %struct.ScmObj* %ae50860)
store volatile %struct.ScmObj* %anf_45bind48268, %struct.ScmObj** %stackaddr$prim62298, align 8
%stackaddr$prim62299 = alloca %struct.ScmObj*, align 8
%anf_45bind48269 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48268)
store volatile %struct.ScmObj* %anf_45bind48269, %struct.ScmObj** %stackaddr$prim62299, align 8
%ae50863 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim62300 = alloca %struct.ScmObj*, align 8
%_95048117 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48112, %struct.ScmObj* %ae50863, %struct.ScmObj* %anf_45bind48269)
store volatile %struct.ScmObj* %_95048117, %struct.ScmObj** %stackaddr$prim62300, align 8
%ae50866 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim62301 = alloca %struct.ScmObj*, align 8
%anf_45bind48270 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48113, %struct.ScmObj* %ae50866)
store volatile %struct.ScmObj* %anf_45bind48270, %struct.ScmObj** %stackaddr$prim62301, align 8
%ae50868 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim62302 = alloca %struct.ScmObj*, align 8
%anf_45bind48271 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind48270, %struct.ScmObj* %ae50868)
store volatile %struct.ScmObj* %anf_45bind48271, %struct.ScmObj** %stackaddr$prim62302, align 8
%ae50870 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim62303 = alloca %struct.ScmObj*, align 8
%_95148116 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n48113, %struct.ScmObj* %ae50870, %struct.ScmObj* %anf_45bind48271)
store volatile %struct.ScmObj* %_95148116, %struct.ScmObj** %stackaddr$prim62303, align 8
%argslist60619$cc481140 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62304 = alloca %struct.ScmObj*, align 8
%argslist60619$cc481141 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48114, %struct.ScmObj* %argslist60619$cc481140)
store volatile %struct.ScmObj* %argslist60619$cc481141, %struct.ScmObj** %stackaddr$prim62304, align 8
%stackaddr$prim62305 = alloca %struct.ScmObj*, align 8
%argslist60619$cc481142 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48461, %struct.ScmObj* %argslist60619$cc481141)
store volatile %struct.ScmObj* %argslist60619$cc481142, %struct.ScmObj** %stackaddr$prim62305, align 8
%clofunc62306 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48114)
musttail call tailcc void %clofunc62306(%struct.ScmObj* %cc48114, %struct.ScmObj* %argslist60619$cc481142)
ret void
}

define tailcc void @proc_clo$ae50825(%struct.ScmObj* %env$ae50825,%struct.ScmObj* %current_45args60621) {
%stackaddr$prim62307 = alloca %struct.ScmObj*, align 8
%k48465 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60621)
store volatile %struct.ScmObj* %k48465, %struct.ScmObj** %stackaddr$prim62307, align 8
%stackaddr$prim62308 = alloca %struct.ScmObj*, align 8
%current_45args60622 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60621)
store volatile %struct.ScmObj* %current_45args60622, %struct.ScmObj** %stackaddr$prim62308, align 8
%stackaddr$prim62309 = alloca %struct.ScmObj*, align 8
%u48115 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60622)
store volatile %struct.ScmObj* %u48115, %struct.ScmObj** %stackaddr$prim62309, align 8
%argslist60624$u481150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62310 = alloca %struct.ScmObj*, align 8
%argslist60624$u481151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u48115, %struct.ScmObj* %argslist60624$u481150)
store volatile %struct.ScmObj* %argslist60624$u481151, %struct.ScmObj** %stackaddr$prim62310, align 8
%stackaddr$prim62311 = alloca %struct.ScmObj*, align 8
%argslist60624$u481152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48465, %struct.ScmObj* %argslist60624$u481151)
store volatile %struct.ScmObj* %argslist60624$u481152, %struct.ScmObj** %stackaddr$prim62311, align 8
%clofunc62312 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u48115)
musttail call tailcc void %clofunc62312(%struct.ScmObj* %u48115, %struct.ScmObj* %argslist60624$u481152)
ret void
}

define tailcc void @proc_clo$ae50402(%struct.ScmObj* %env$ae50402,%struct.ScmObj* %current_45args60627) {
%stackaddr$prim62313 = alloca %struct.ScmObj*, align 8
%k48466 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60627)
store volatile %struct.ScmObj* %k48466, %struct.ScmObj** %stackaddr$prim62313, align 8
%stackaddr$prim62314 = alloca %struct.ScmObj*, align 8
%current_45args60628 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60627)
store volatile %struct.ScmObj* %current_45args60628, %struct.ScmObj** %stackaddr$prim62314, align 8
%stackaddr$prim62315 = alloca %struct.ScmObj*, align 8
%a48119 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60628)
store volatile %struct.ScmObj* %a48119, %struct.ScmObj** %stackaddr$prim62315, align 8
%ae50403 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim62316 = alloca %struct.ScmObj*, align 8
%a48120 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50403, %struct.ScmObj* %a48119)
store volatile %struct.ScmObj* %a48120, %struct.ScmObj** %stackaddr$prim62316, align 8
%stackaddr$makeclosure62317 = alloca %struct.ScmObj*, align 8
%fptrToInt62318 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50405 to i64
%ae50405 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt62318)
store volatile %struct.ScmObj* %ae50405, %struct.ScmObj** %stackaddr$makeclosure62317, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50405, %struct.ScmObj* %a48120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50405, %struct.ScmObj* %k48466, i64 1)
%ae50406 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure62319 = alloca %struct.ScmObj*, align 8
%fptrToInt62320 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50407 to i64
%ae50407 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt62320)
store volatile %struct.ScmObj* %ae50407, %struct.ScmObj** %stackaddr$makeclosure62319, align 8
%argslist60650$ae504050 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62321 = alloca %struct.ScmObj*, align 8
%argslist60650$ae504051 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50407, %struct.ScmObj* %argslist60650$ae504050)
store volatile %struct.ScmObj* %argslist60650$ae504051, %struct.ScmObj** %stackaddr$prim62321, align 8
%stackaddr$prim62322 = alloca %struct.ScmObj*, align 8
%argslist60650$ae504052 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50406, %struct.ScmObj* %argslist60650$ae504051)
store volatile %struct.ScmObj* %argslist60650$ae504052, %struct.ScmObj** %stackaddr$prim62322, align 8
%clofunc62323 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50405)
musttail call tailcc void %clofunc62323(%struct.ScmObj* %ae50405, %struct.ScmObj* %argslist60650$ae504052)
ret void
}

define tailcc void @proc_clo$ae50405(%struct.ScmObj* %env$ae50405,%struct.ScmObj* %current_45args60630) {
%stackaddr$env-ref62324 = alloca %struct.ScmObj*, align 8
%a48120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50405, i64 0)
store %struct.ScmObj* %a48120, %struct.ScmObj** %stackaddr$env-ref62324
%stackaddr$env-ref62325 = alloca %struct.ScmObj*, align 8
%k48466 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50405, i64 1)
store %struct.ScmObj* %k48466, %struct.ScmObj** %stackaddr$env-ref62325
%stackaddr$prim62326 = alloca %struct.ScmObj*, align 8
%_95k48467 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60630)
store volatile %struct.ScmObj* %_95k48467, %struct.ScmObj** %stackaddr$prim62326, align 8
%stackaddr$prim62327 = alloca %struct.ScmObj*, align 8
%current_45args60631 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60630)
store volatile %struct.ScmObj* %current_45args60631, %struct.ScmObj** %stackaddr$prim62327, align 8
%stackaddr$prim62328 = alloca %struct.ScmObj*, align 8
%anf_45bind48257 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60631)
store volatile %struct.ScmObj* %anf_45bind48257, %struct.ScmObj** %stackaddr$prim62328, align 8
%stackaddr$makeclosure62329 = alloca %struct.ScmObj*, align 8
%fptrToInt62330 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50424 to i64
%ae50424 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt62330)
store volatile %struct.ScmObj* %ae50424, %struct.ScmObj** %stackaddr$makeclosure62329, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50424, %struct.ScmObj* %a48120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50424, %struct.ScmObj* %k48466, i64 1)
%stackaddr$makeclosure62331 = alloca %struct.ScmObj*, align 8
%fptrToInt62332 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50425 to i64
%ae50425 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt62332)
store volatile %struct.ScmObj* %ae50425, %struct.ScmObj** %stackaddr$makeclosure62331, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50425, %struct.ScmObj* %a48120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50425, %struct.ScmObj* %k48466, i64 1)
%argslist60645$anf_45bind482570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62333 = alloca %struct.ScmObj*, align 8
%argslist60645$anf_45bind482571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50425, %struct.ScmObj* %argslist60645$anf_45bind482570)
store volatile %struct.ScmObj* %argslist60645$anf_45bind482571, %struct.ScmObj** %stackaddr$prim62333, align 8
%stackaddr$prim62334 = alloca %struct.ScmObj*, align 8
%argslist60645$anf_45bind482572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50424, %struct.ScmObj* %argslist60645$anf_45bind482571)
store volatile %struct.ScmObj* %argslist60645$anf_45bind482572, %struct.ScmObj** %stackaddr$prim62334, align 8
%clofunc62335 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48257)
musttail call tailcc void %clofunc62335(%struct.ScmObj* %anf_45bind48257, %struct.ScmObj* %argslist60645$anf_45bind482572)
ret void
}

define tailcc void @proc_clo$ae50424(%struct.ScmObj* %env$ae50424,%struct.ScmObj* %current_45args60633) {
%stackaddr$env-ref62336 = alloca %struct.ScmObj*, align 8
%a48120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50424, i64 0)
store %struct.ScmObj* %a48120, %struct.ScmObj** %stackaddr$env-ref62336
%stackaddr$env-ref62337 = alloca %struct.ScmObj*, align 8
%k48466 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50424, i64 1)
store %struct.ScmObj* %k48466, %struct.ScmObj** %stackaddr$env-ref62337
%stackaddr$prim62338 = alloca %struct.ScmObj*, align 8
%_95k48468 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60633)
store volatile %struct.ScmObj* %_95k48468, %struct.ScmObj** %stackaddr$prim62338, align 8
%stackaddr$prim62339 = alloca %struct.ScmObj*, align 8
%current_45args60634 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60633)
store volatile %struct.ScmObj* %current_45args60634, %struct.ScmObj** %stackaddr$prim62339, align 8
%stackaddr$prim62340 = alloca %struct.ScmObj*, align 8
%cc48121 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60634)
store volatile %struct.ScmObj* %cc48121, %struct.ScmObj** %stackaddr$prim62340, align 8
%ae50540 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim62341 = alloca %struct.ScmObj*, align 8
%anf_45bind48258 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48120, %struct.ScmObj* %ae50540)
store volatile %struct.ScmObj* %anf_45bind48258, %struct.ScmObj** %stackaddr$prim62341, align 8
%stackaddr$prim62342 = alloca %struct.ScmObj*, align 8
%anf_45bind48259 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48258)
store volatile %struct.ScmObj* %anf_45bind48259, %struct.ScmObj** %stackaddr$prim62342, align 8
%truthy$cmp62343 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48259)
%cmp$cmp62343 = icmp eq i64 %truthy$cmp62343, 1
br i1 %cmp$cmp62343, label %truebranch$cmp62343, label %falsebranch$cmp62343
truebranch$cmp62343:
%ae50544 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50545 = call %struct.ScmObj* @const_init_true()
%argslist60636$k484660 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62344 = alloca %struct.ScmObj*, align 8
%argslist60636$k484661 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50545, %struct.ScmObj* %argslist60636$k484660)
store volatile %struct.ScmObj* %argslist60636$k484661, %struct.ScmObj** %stackaddr$prim62344, align 8
%stackaddr$prim62345 = alloca %struct.ScmObj*, align 8
%argslist60636$k484662 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50544, %struct.ScmObj* %argslist60636$k484661)
store volatile %struct.ScmObj* %argslist60636$k484662, %struct.ScmObj** %stackaddr$prim62345, align 8
%clofunc62346 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48466)
musttail call tailcc void %clofunc62346(%struct.ScmObj* %k48466, %struct.ScmObj* %argslist60636$k484662)
ret void
falsebranch$cmp62343:
%ae50553 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim62347 = alloca %struct.ScmObj*, align 8
%anf_45bind48260 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48120, %struct.ScmObj* %ae50553)
store volatile %struct.ScmObj* %anf_45bind48260, %struct.ScmObj** %stackaddr$prim62347, align 8
%stackaddr$prim62348 = alloca %struct.ScmObj*, align 8
%anf_45bind48261 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind48260)
store volatile %struct.ScmObj* %anf_45bind48261, %struct.ScmObj** %stackaddr$prim62348, align 8
%truthy$cmp62349 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48261)
%cmp$cmp62349 = icmp eq i64 %truthy$cmp62349, 1
br i1 %cmp$cmp62349, label %truebranch$cmp62349, label %falsebranch$cmp62349
truebranch$cmp62349:
%ae50557 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim62350 = alloca %struct.ScmObj*, align 8
%anf_45bind48262 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48120, %struct.ScmObj* %ae50557)
store volatile %struct.ScmObj* %anf_45bind48262, %struct.ScmObj** %stackaddr$prim62350, align 8
%stackaddr$prim62351 = alloca %struct.ScmObj*, align 8
%b48123 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48262)
store volatile %struct.ScmObj* %b48123, %struct.ScmObj** %stackaddr$prim62351, align 8
%ae50560 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim62352 = alloca %struct.ScmObj*, align 8
%anf_45bind48263 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48120, %struct.ScmObj* %ae50560)
store volatile %struct.ScmObj* %anf_45bind48263, %struct.ScmObj** %stackaddr$prim62352, align 8
%stackaddr$prim62353 = alloca %struct.ScmObj*, align 8
%anf_45bind48264 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48263)
store volatile %struct.ScmObj* %anf_45bind48264, %struct.ScmObj** %stackaddr$prim62353, align 8
%ae50563 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim62354 = alloca %struct.ScmObj*, align 8
%_95048124 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a48120, %struct.ScmObj* %ae50563, %struct.ScmObj* %anf_45bind48264)
store volatile %struct.ScmObj* %_95048124, %struct.ScmObj** %stackaddr$prim62354, align 8
%argslist60637$cc481210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62355 = alloca %struct.ScmObj*, align 8
%argslist60637$cc481211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48121, %struct.ScmObj* %argslist60637$cc481210)
store volatile %struct.ScmObj* %argslist60637$cc481211, %struct.ScmObj** %stackaddr$prim62355, align 8
%stackaddr$prim62356 = alloca %struct.ScmObj*, align 8
%argslist60637$cc481212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48466, %struct.ScmObj* %argslist60637$cc481211)
store volatile %struct.ScmObj* %argslist60637$cc481212, %struct.ScmObj** %stackaddr$prim62356, align 8
%clofunc62357 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48121)
musttail call tailcc void %clofunc62357(%struct.ScmObj* %cc48121, %struct.ScmObj* %argslist60637$cc481212)
ret void
falsebranch$cmp62349:
%ae50596 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50597 = call %struct.ScmObj* @const_init_false()
%argslist60638$k484660 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62358 = alloca %struct.ScmObj*, align 8
%argslist60638$k484661 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50597, %struct.ScmObj* %argslist60638$k484660)
store volatile %struct.ScmObj* %argslist60638$k484661, %struct.ScmObj** %stackaddr$prim62358, align 8
%stackaddr$prim62359 = alloca %struct.ScmObj*, align 8
%argslist60638$k484662 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50596, %struct.ScmObj* %argslist60638$k484661)
store volatile %struct.ScmObj* %argslist60638$k484662, %struct.ScmObj** %stackaddr$prim62359, align 8
%clofunc62360 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48466)
musttail call tailcc void %clofunc62360(%struct.ScmObj* %k48466, %struct.ScmObj* %argslist60638$k484662)
ret void
}

define tailcc void @proc_clo$ae50425(%struct.ScmObj* %env$ae50425,%struct.ScmObj* %current_45args60639) {
%stackaddr$env-ref62361 = alloca %struct.ScmObj*, align 8
%a48120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50425, i64 0)
store %struct.ScmObj* %a48120, %struct.ScmObj** %stackaddr$env-ref62361
%stackaddr$env-ref62362 = alloca %struct.ScmObj*, align 8
%k48466 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50425, i64 1)
store %struct.ScmObj* %k48466, %struct.ScmObj** %stackaddr$env-ref62362
%stackaddr$prim62363 = alloca %struct.ScmObj*, align 8
%_95k48468 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60639)
store volatile %struct.ScmObj* %_95k48468, %struct.ScmObj** %stackaddr$prim62363, align 8
%stackaddr$prim62364 = alloca %struct.ScmObj*, align 8
%current_45args60640 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60639)
store volatile %struct.ScmObj* %current_45args60640, %struct.ScmObj** %stackaddr$prim62364, align 8
%stackaddr$prim62365 = alloca %struct.ScmObj*, align 8
%cc48121 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60640)
store volatile %struct.ScmObj* %cc48121, %struct.ScmObj** %stackaddr$prim62365, align 8
%ae50427 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim62366 = alloca %struct.ScmObj*, align 8
%anf_45bind48258 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48120, %struct.ScmObj* %ae50427)
store volatile %struct.ScmObj* %anf_45bind48258, %struct.ScmObj** %stackaddr$prim62366, align 8
%stackaddr$prim62367 = alloca %struct.ScmObj*, align 8
%anf_45bind48259 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48258)
store volatile %struct.ScmObj* %anf_45bind48259, %struct.ScmObj** %stackaddr$prim62367, align 8
%truthy$cmp62368 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48259)
%cmp$cmp62368 = icmp eq i64 %truthy$cmp62368, 1
br i1 %cmp$cmp62368, label %truebranch$cmp62368, label %falsebranch$cmp62368
truebranch$cmp62368:
%ae50431 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50432 = call %struct.ScmObj* @const_init_true()
%argslist60642$k484660 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62369 = alloca %struct.ScmObj*, align 8
%argslist60642$k484661 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50432, %struct.ScmObj* %argslist60642$k484660)
store volatile %struct.ScmObj* %argslist60642$k484661, %struct.ScmObj** %stackaddr$prim62369, align 8
%stackaddr$prim62370 = alloca %struct.ScmObj*, align 8
%argslist60642$k484662 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50431, %struct.ScmObj* %argslist60642$k484661)
store volatile %struct.ScmObj* %argslist60642$k484662, %struct.ScmObj** %stackaddr$prim62370, align 8
%clofunc62371 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48466)
musttail call tailcc void %clofunc62371(%struct.ScmObj* %k48466, %struct.ScmObj* %argslist60642$k484662)
ret void
falsebranch$cmp62368:
%ae50440 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim62372 = alloca %struct.ScmObj*, align 8
%anf_45bind48260 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48120, %struct.ScmObj* %ae50440)
store volatile %struct.ScmObj* %anf_45bind48260, %struct.ScmObj** %stackaddr$prim62372, align 8
%stackaddr$prim62373 = alloca %struct.ScmObj*, align 8
%anf_45bind48261 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind48260)
store volatile %struct.ScmObj* %anf_45bind48261, %struct.ScmObj** %stackaddr$prim62373, align 8
%truthy$cmp62374 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48261)
%cmp$cmp62374 = icmp eq i64 %truthy$cmp62374, 1
br i1 %cmp$cmp62374, label %truebranch$cmp62374, label %falsebranch$cmp62374
truebranch$cmp62374:
%ae50444 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim62375 = alloca %struct.ScmObj*, align 8
%anf_45bind48262 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48120, %struct.ScmObj* %ae50444)
store volatile %struct.ScmObj* %anf_45bind48262, %struct.ScmObj** %stackaddr$prim62375, align 8
%stackaddr$prim62376 = alloca %struct.ScmObj*, align 8
%b48123 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48262)
store volatile %struct.ScmObj* %b48123, %struct.ScmObj** %stackaddr$prim62376, align 8
%ae50447 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim62377 = alloca %struct.ScmObj*, align 8
%anf_45bind48263 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48120, %struct.ScmObj* %ae50447)
store volatile %struct.ScmObj* %anf_45bind48263, %struct.ScmObj** %stackaddr$prim62377, align 8
%stackaddr$prim62378 = alloca %struct.ScmObj*, align 8
%anf_45bind48264 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48263)
store volatile %struct.ScmObj* %anf_45bind48264, %struct.ScmObj** %stackaddr$prim62378, align 8
%ae50450 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim62379 = alloca %struct.ScmObj*, align 8
%_95048124 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a48120, %struct.ScmObj* %ae50450, %struct.ScmObj* %anf_45bind48264)
store volatile %struct.ScmObj* %_95048124, %struct.ScmObj** %stackaddr$prim62379, align 8
%argslist60643$cc481210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62380 = alloca %struct.ScmObj*, align 8
%argslist60643$cc481211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48121, %struct.ScmObj* %argslist60643$cc481210)
store volatile %struct.ScmObj* %argslist60643$cc481211, %struct.ScmObj** %stackaddr$prim62380, align 8
%stackaddr$prim62381 = alloca %struct.ScmObj*, align 8
%argslist60643$cc481212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48466, %struct.ScmObj* %argslist60643$cc481211)
store volatile %struct.ScmObj* %argslist60643$cc481212, %struct.ScmObj** %stackaddr$prim62381, align 8
%clofunc62382 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48121)
musttail call tailcc void %clofunc62382(%struct.ScmObj* %cc48121, %struct.ScmObj* %argslist60643$cc481212)
ret void
falsebranch$cmp62374:
%ae50483 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50484 = call %struct.ScmObj* @const_init_false()
%argslist60644$k484660 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62383 = alloca %struct.ScmObj*, align 8
%argslist60644$k484661 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50484, %struct.ScmObj* %argslist60644$k484660)
store volatile %struct.ScmObj* %argslist60644$k484661, %struct.ScmObj** %stackaddr$prim62383, align 8
%stackaddr$prim62384 = alloca %struct.ScmObj*, align 8
%argslist60644$k484662 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50483, %struct.ScmObj* %argslist60644$k484661)
store volatile %struct.ScmObj* %argslist60644$k484662, %struct.ScmObj** %stackaddr$prim62384, align 8
%clofunc62385 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48466)
musttail call tailcc void %clofunc62385(%struct.ScmObj* %k48466, %struct.ScmObj* %argslist60644$k484662)
ret void
}

define tailcc void @proc_clo$ae50407(%struct.ScmObj* %env$ae50407,%struct.ScmObj* %current_45args60646) {
%stackaddr$prim62386 = alloca %struct.ScmObj*, align 8
%k48469 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60646)
store volatile %struct.ScmObj* %k48469, %struct.ScmObj** %stackaddr$prim62386, align 8
%stackaddr$prim62387 = alloca %struct.ScmObj*, align 8
%current_45args60647 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60646)
store volatile %struct.ScmObj* %current_45args60647, %struct.ScmObj** %stackaddr$prim62387, align 8
%stackaddr$prim62388 = alloca %struct.ScmObj*, align 8
%k48122 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60647)
store volatile %struct.ScmObj* %k48122, %struct.ScmObj** %stackaddr$prim62388, align 8
%ae50409 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist60649$k484690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62389 = alloca %struct.ScmObj*, align 8
%argslist60649$k484691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48122, %struct.ScmObj* %argslist60649$k484690)
store volatile %struct.ScmObj* %argslist60649$k484691, %struct.ScmObj** %stackaddr$prim62389, align 8
%stackaddr$prim62390 = alloca %struct.ScmObj*, align 8
%argslist60649$k484692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50409, %struct.ScmObj* %argslist60649$k484691)
store volatile %struct.ScmObj* %argslist60649$k484692, %struct.ScmObj** %stackaddr$prim62390, align 8
%clofunc62391 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48469)
musttail call tailcc void %clofunc62391(%struct.ScmObj* %k48469, %struct.ScmObj* %argslist60649$k484692)
ret void
}

define tailcc void @proc_clo$ae50330(%struct.ScmObj* %env$ae50330,%struct.ScmObj* %current_45args60652) {
%stackaddr$env-ref62392 = alloca %struct.ScmObj*, align 8
%_37append48126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50330, i64 0)
store %struct.ScmObj* %_37append48126, %struct.ScmObj** %stackaddr$env-ref62392
%stackaddr$prim62393 = alloca %struct.ScmObj*, align 8
%k48470 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60652)
store volatile %struct.ScmObj* %k48470, %struct.ScmObj** %stackaddr$prim62393, align 8
%stackaddr$prim62394 = alloca %struct.ScmObj*, align 8
%current_45args60653 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60652)
store volatile %struct.ScmObj* %current_45args60653, %struct.ScmObj** %stackaddr$prim62394, align 8
%stackaddr$prim62395 = alloca %struct.ScmObj*, align 8
%ls048129 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60653)
store volatile %struct.ScmObj* %ls048129, %struct.ScmObj** %stackaddr$prim62395, align 8
%stackaddr$prim62396 = alloca %struct.ScmObj*, align 8
%current_45args60654 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60653)
store volatile %struct.ScmObj* %current_45args60654, %struct.ScmObj** %stackaddr$prim62396, align 8
%stackaddr$prim62397 = alloca %struct.ScmObj*, align 8
%ls148128 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60654)
store volatile %struct.ScmObj* %ls148128, %struct.ScmObj** %stackaddr$prim62397, align 8
%stackaddr$prim62398 = alloca %struct.ScmObj*, align 8
%anf_45bind48251 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls048129)
store volatile %struct.ScmObj* %anf_45bind48251, %struct.ScmObj** %stackaddr$prim62398, align 8
%truthy$cmp62399 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48251)
%cmp$cmp62399 = icmp eq i64 %truthy$cmp62399, 1
br i1 %cmp$cmp62399, label %truebranch$cmp62399, label %falsebranch$cmp62399
truebranch$cmp62399:
%ae50334 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist60656$k484700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62400 = alloca %struct.ScmObj*, align 8
%argslist60656$k484701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls148128, %struct.ScmObj* %argslist60656$k484700)
store volatile %struct.ScmObj* %argslist60656$k484701, %struct.ScmObj** %stackaddr$prim62400, align 8
%stackaddr$prim62401 = alloca %struct.ScmObj*, align 8
%argslist60656$k484702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50334, %struct.ScmObj* %argslist60656$k484701)
store volatile %struct.ScmObj* %argslist60656$k484702, %struct.ScmObj** %stackaddr$prim62401, align 8
%clofunc62402 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48470)
musttail call tailcc void %clofunc62402(%struct.ScmObj* %k48470, %struct.ScmObj* %argslist60656$k484702)
ret void
falsebranch$cmp62399:
%stackaddr$prim62403 = alloca %struct.ScmObj*, align 8
%anf_45bind48252 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls048129)
store volatile %struct.ScmObj* %anf_45bind48252, %struct.ScmObj** %stackaddr$prim62403, align 8
%ae50341 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim62404 = alloca %struct.ScmObj*, align 8
%anf_45bind48253 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append48126, %struct.ScmObj* %ae50341)
store volatile %struct.ScmObj* %anf_45bind48253, %struct.ScmObj** %stackaddr$prim62404, align 8
%stackaddr$prim62405 = alloca %struct.ScmObj*, align 8
%anf_45bind48254 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls048129)
store volatile %struct.ScmObj* %anf_45bind48254, %struct.ScmObj** %stackaddr$prim62405, align 8
%stackaddr$makeclosure62406 = alloca %struct.ScmObj*, align 8
%fptrToInt62407 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50344 to i64
%ae50344 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt62407)
store volatile %struct.ScmObj* %ae50344, %struct.ScmObj** %stackaddr$makeclosure62406, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50344, %struct.ScmObj* %k48470, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50344, %struct.ScmObj* %anf_45bind48252, i64 1)
%argslist60661$anf_45bind482530 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62408 = alloca %struct.ScmObj*, align 8
%argslist60661$anf_45bind482531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls148128, %struct.ScmObj* %argslist60661$anf_45bind482530)
store volatile %struct.ScmObj* %argslist60661$anf_45bind482531, %struct.ScmObj** %stackaddr$prim62408, align 8
%stackaddr$prim62409 = alloca %struct.ScmObj*, align 8
%argslist60661$anf_45bind482532 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48254, %struct.ScmObj* %argslist60661$anf_45bind482531)
store volatile %struct.ScmObj* %argslist60661$anf_45bind482532, %struct.ScmObj** %stackaddr$prim62409, align 8
%stackaddr$prim62410 = alloca %struct.ScmObj*, align 8
%argslist60661$anf_45bind482533 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50344, %struct.ScmObj* %argslist60661$anf_45bind482532)
store volatile %struct.ScmObj* %argslist60661$anf_45bind482533, %struct.ScmObj** %stackaddr$prim62410, align 8
%clofunc62411 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48253)
musttail call tailcc void %clofunc62411(%struct.ScmObj* %anf_45bind48253, %struct.ScmObj* %argslist60661$anf_45bind482533)
ret void
}

define tailcc void @proc_clo$ae50344(%struct.ScmObj* %env$ae50344,%struct.ScmObj* %current_45args60657) {
%stackaddr$env-ref62412 = alloca %struct.ScmObj*, align 8
%k48470 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50344, i64 0)
store %struct.ScmObj* %k48470, %struct.ScmObj** %stackaddr$env-ref62412
%stackaddr$env-ref62413 = alloca %struct.ScmObj*, align 8
%anf_45bind48252 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50344, i64 1)
store %struct.ScmObj* %anf_45bind48252, %struct.ScmObj** %stackaddr$env-ref62413
%stackaddr$prim62414 = alloca %struct.ScmObj*, align 8
%_95k48471 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60657)
store volatile %struct.ScmObj* %_95k48471, %struct.ScmObj** %stackaddr$prim62414, align 8
%stackaddr$prim62415 = alloca %struct.ScmObj*, align 8
%current_45args60658 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60657)
store volatile %struct.ScmObj* %current_45args60658, %struct.ScmObj** %stackaddr$prim62415, align 8
%stackaddr$prim62416 = alloca %struct.ScmObj*, align 8
%anf_45bind48255 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60658)
store volatile %struct.ScmObj* %anf_45bind48255, %struct.ScmObj** %stackaddr$prim62416, align 8
%stackaddr$prim62417 = alloca %struct.ScmObj*, align 8
%cpsprim48472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48252, %struct.ScmObj* %anf_45bind48255)
store volatile %struct.ScmObj* %cpsprim48472, %struct.ScmObj** %stackaddr$prim62417, align 8
%ae50350 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist60660$k484700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62418 = alloca %struct.ScmObj*, align 8
%argslist60660$k484701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48472, %struct.ScmObj* %argslist60660$k484700)
store volatile %struct.ScmObj* %argslist60660$k484701, %struct.ScmObj** %stackaddr$prim62418, align 8
%stackaddr$prim62419 = alloca %struct.ScmObj*, align 8
%argslist60660$k484702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50350, %struct.ScmObj* %argslist60660$k484701)
store volatile %struct.ScmObj* %argslist60660$k484702, %struct.ScmObj** %stackaddr$prim62419, align 8
%clofunc62420 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48470)
musttail call tailcc void %clofunc62420(%struct.ScmObj* %k48470, %struct.ScmObj* %argslist60660$k484702)
ret void
}

define tailcc void @proc_clo$ae50304(%struct.ScmObj* %env$ae50304,%struct.ScmObj* %current_45args60663) {
%stackaddr$prim62421 = alloca %struct.ScmObj*, align 8
%k48473 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60663)
store volatile %struct.ScmObj* %k48473, %struct.ScmObj** %stackaddr$prim62421, align 8
%stackaddr$prim62422 = alloca %struct.ScmObj*, align 8
%current_45args60664 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60663)
store volatile %struct.ScmObj* %current_45args60664, %struct.ScmObj** %stackaddr$prim62422, align 8
%stackaddr$prim62423 = alloca %struct.ScmObj*, align 8
%a48132 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60664)
store volatile %struct.ScmObj* %a48132, %struct.ScmObj** %stackaddr$prim62423, align 8
%stackaddr$prim62424 = alloca %struct.ScmObj*, align 8
%current_45args60665 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60664)
store volatile %struct.ScmObj* %current_45args60665, %struct.ScmObj** %stackaddr$prim62424, align 8
%stackaddr$prim62425 = alloca %struct.ScmObj*, align 8
%b48131 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60665)
store volatile %struct.ScmObj* %b48131, %struct.ScmObj** %stackaddr$prim62425, align 8
%stackaddr$prim62426 = alloca %struct.ScmObj*, align 8
%anf_45bind48250 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a48132, %struct.ScmObj* %b48131)
store volatile %struct.ScmObj* %anf_45bind48250, %struct.ScmObj** %stackaddr$prim62426, align 8
%stackaddr$prim62427 = alloca %struct.ScmObj*, align 8
%cpsprim48474 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind48250)
store volatile %struct.ScmObj* %cpsprim48474, %struct.ScmObj** %stackaddr$prim62427, align 8
%ae50309 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist60667$k484730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62428 = alloca %struct.ScmObj*, align 8
%argslist60667$k484731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48474, %struct.ScmObj* %argslist60667$k484730)
store volatile %struct.ScmObj* %argslist60667$k484731, %struct.ScmObj** %stackaddr$prim62428, align 8
%stackaddr$prim62429 = alloca %struct.ScmObj*, align 8
%argslist60667$k484732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50309, %struct.ScmObj* %argslist60667$k484731)
store volatile %struct.ScmObj* %argslist60667$k484732, %struct.ScmObj** %stackaddr$prim62429, align 8
%clofunc62430 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48473)
musttail call tailcc void %clofunc62430(%struct.ScmObj* %k48473, %struct.ScmObj* %argslist60667$k484732)
ret void
}

define tailcc void @proc_clo$ae50280(%struct.ScmObj* %env$ae50280,%struct.ScmObj* %current_45args60669) {
%stackaddr$prim62431 = alloca %struct.ScmObj*, align 8
%k48475 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60669)
store volatile %struct.ScmObj* %k48475, %struct.ScmObj** %stackaddr$prim62431, align 8
%stackaddr$prim62432 = alloca %struct.ScmObj*, align 8
%current_45args60670 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60669)
store volatile %struct.ScmObj* %current_45args60670, %struct.ScmObj** %stackaddr$prim62432, align 8
%stackaddr$prim62433 = alloca %struct.ScmObj*, align 8
%a48135 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60670)
store volatile %struct.ScmObj* %a48135, %struct.ScmObj** %stackaddr$prim62433, align 8
%stackaddr$prim62434 = alloca %struct.ScmObj*, align 8
%current_45args60671 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60670)
store volatile %struct.ScmObj* %current_45args60671, %struct.ScmObj** %stackaddr$prim62434, align 8
%stackaddr$prim62435 = alloca %struct.ScmObj*, align 8
%b48134 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60671)
store volatile %struct.ScmObj* %b48134, %struct.ScmObj** %stackaddr$prim62435, align 8
%stackaddr$prim62436 = alloca %struct.ScmObj*, align 8
%anf_45bind48249 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a48135, %struct.ScmObj* %b48134)
store volatile %struct.ScmObj* %anf_45bind48249, %struct.ScmObj** %stackaddr$prim62436, align 8
%stackaddr$prim62437 = alloca %struct.ScmObj*, align 8
%cpsprim48476 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind48249)
store volatile %struct.ScmObj* %cpsprim48476, %struct.ScmObj** %stackaddr$prim62437, align 8
%ae50285 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist60673$k484750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62438 = alloca %struct.ScmObj*, align 8
%argslist60673$k484751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48476, %struct.ScmObj* %argslist60673$k484750)
store volatile %struct.ScmObj* %argslist60673$k484751, %struct.ScmObj** %stackaddr$prim62438, align 8
%stackaddr$prim62439 = alloca %struct.ScmObj*, align 8
%argslist60673$k484752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50285, %struct.ScmObj* %argslist60673$k484751)
store volatile %struct.ScmObj* %argslist60673$k484752, %struct.ScmObj** %stackaddr$prim62439, align 8
%clofunc62440 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48475)
musttail call tailcc void %clofunc62440(%struct.ScmObj* %k48475, %struct.ScmObj* %argslist60673$k484752)
ret void
}

define tailcc void @proc_clo$ae49886(%struct.ScmObj* %env$ae49886,%struct.ScmObj* %current_45args60676) {
%stackaddr$env-ref62441 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49886, i64 0)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref62441
%stackaddr$env-ref62442 = alloca %struct.ScmObj*, align 8
%_37map148085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49886, i64 1)
store %struct.ScmObj* %_37map148085, %struct.ScmObj** %stackaddr$env-ref62442
%stackaddr$env-ref62443 = alloca %struct.ScmObj*, align 8
%_37foldr48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49886, i64 2)
store %struct.ScmObj* %_37foldr48059, %struct.ScmObj** %stackaddr$env-ref62443
%stackaddr$prim62444 = alloca %struct.ScmObj*, align 8
%k48477 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60676)
store volatile %struct.ScmObj* %k48477, %struct.ScmObj** %stackaddr$prim62444, align 8
%stackaddr$prim62445 = alloca %struct.ScmObj*, align 8
%current_45args60677 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60676)
store volatile %struct.ScmObj* %current_45args60677, %struct.ScmObj** %stackaddr$prim62445, align 8
%stackaddr$prim62446 = alloca %struct.ScmObj*, align 8
%_37foldl48137 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60677)
store volatile %struct.ScmObj* %_37foldl48137, %struct.ScmObj** %stackaddr$prim62446, align 8
%ae49888 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure62447 = alloca %struct.ScmObj*, align 8
%fptrToInt62448 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49889 to i64
%ae49889 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt62448)
store volatile %struct.ScmObj* %ae49889, %struct.ScmObj** %stackaddr$makeclosure62447, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49889, %struct.ScmObj* %_37foldl48137, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49889, %struct.ScmObj* %_37foldr148054, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49889, %struct.ScmObj* %_37map148085, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49889, %struct.ScmObj* %_37foldr48059, i64 3)
%argslist60734$k484770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62449 = alloca %struct.ScmObj*, align 8
%argslist60734$k484771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49889, %struct.ScmObj* %argslist60734$k484770)
store volatile %struct.ScmObj* %argslist60734$k484771, %struct.ScmObj** %stackaddr$prim62449, align 8
%stackaddr$prim62450 = alloca %struct.ScmObj*, align 8
%argslist60734$k484772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49888, %struct.ScmObj* %argslist60734$k484771)
store volatile %struct.ScmObj* %argslist60734$k484772, %struct.ScmObj** %stackaddr$prim62450, align 8
%clofunc62451 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48477)
musttail call tailcc void %clofunc62451(%struct.ScmObj* %k48477, %struct.ScmObj* %argslist60734$k484772)
ret void
}

define tailcc void @proc_clo$ae49889(%struct.ScmObj* %env$ae49889,%struct.ScmObj* %args4813848478) {
%stackaddr$env-ref62452 = alloca %struct.ScmObj*, align 8
%_37foldl48137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49889, i64 0)
store %struct.ScmObj* %_37foldl48137, %struct.ScmObj** %stackaddr$env-ref62452
%stackaddr$env-ref62453 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49889, i64 1)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref62453
%stackaddr$env-ref62454 = alloca %struct.ScmObj*, align 8
%_37map148085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49889, i64 2)
store %struct.ScmObj* %_37map148085, %struct.ScmObj** %stackaddr$env-ref62454
%stackaddr$env-ref62455 = alloca %struct.ScmObj*, align 8
%_37foldr48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49889, i64 3)
store %struct.ScmObj* %_37foldr48059, %struct.ScmObj** %stackaddr$env-ref62455
%stackaddr$prim62456 = alloca %struct.ScmObj*, align 8
%k48479 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4813848478)
store volatile %struct.ScmObj* %k48479, %struct.ScmObj** %stackaddr$prim62456, align 8
%stackaddr$prim62457 = alloca %struct.ScmObj*, align 8
%args48138 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4813848478)
store volatile %struct.ScmObj* %args48138, %struct.ScmObj** %stackaddr$prim62457, align 8
%stackaddr$prim62458 = alloca %struct.ScmObj*, align 8
%f48141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48138)
store volatile %struct.ScmObj* %f48141, %struct.ScmObj** %stackaddr$prim62458, align 8
%stackaddr$prim62459 = alloca %struct.ScmObj*, align 8
%anf_45bind48237 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48138)
store volatile %struct.ScmObj* %anf_45bind48237, %struct.ScmObj** %stackaddr$prim62459, align 8
%stackaddr$prim62460 = alloca %struct.ScmObj*, align 8
%acc48140 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48237)
store volatile %struct.ScmObj* %acc48140, %struct.ScmObj** %stackaddr$prim62460, align 8
%stackaddr$prim62461 = alloca %struct.ScmObj*, align 8
%anf_45bind48238 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48138)
store volatile %struct.ScmObj* %anf_45bind48238, %struct.ScmObj** %stackaddr$prim62461, align 8
%stackaddr$prim62462 = alloca %struct.ScmObj*, align 8
%lsts48139 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48238)
store volatile %struct.ScmObj* %lsts48139, %struct.ScmObj** %stackaddr$prim62462, align 8
%stackaddr$makeclosure62463 = alloca %struct.ScmObj*, align 8
%fptrToInt62464 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49897 to i64
%ae49897 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt62464)
store volatile %struct.ScmObj* %ae49897, %struct.ScmObj** %stackaddr$makeclosure62463, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49897, %struct.ScmObj* %lsts48139, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49897, %struct.ScmObj* %_37foldr48059, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49897, %struct.ScmObj* %_37foldl48137, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49897, %struct.ScmObj* %_37foldr148054, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49897, %struct.ScmObj* %_37map148085, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49897, %struct.ScmObj* %k48479, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49897, %struct.ScmObj* %f48141, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae49897, %struct.ScmObj* %acc48140, i64 7)
%ae49898 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure62465 = alloca %struct.ScmObj*, align 8
%fptrToInt62466 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49899 to i64
%ae49899 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt62466)
store volatile %struct.ScmObj* %ae49899, %struct.ScmObj** %stackaddr$makeclosure62465, align 8
%argslist60733$ae498970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62467 = alloca %struct.ScmObj*, align 8
%argslist60733$ae498971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49899, %struct.ScmObj* %argslist60733$ae498970)
store volatile %struct.ScmObj* %argslist60733$ae498971, %struct.ScmObj** %stackaddr$prim62467, align 8
%stackaddr$prim62468 = alloca %struct.ScmObj*, align 8
%argslist60733$ae498972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49898, %struct.ScmObj* %argslist60733$ae498971)
store volatile %struct.ScmObj* %argslist60733$ae498972, %struct.ScmObj** %stackaddr$prim62468, align 8
%clofunc62469 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49897)
musttail call tailcc void %clofunc62469(%struct.ScmObj* %ae49897, %struct.ScmObj* %argslist60733$ae498972)
ret void
}

define tailcc void @proc_clo$ae49897(%struct.ScmObj* %env$ae49897,%struct.ScmObj* %current_45args60679) {
%stackaddr$env-ref62470 = alloca %struct.ScmObj*, align 8
%lsts48139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49897, i64 0)
store %struct.ScmObj* %lsts48139, %struct.ScmObj** %stackaddr$env-ref62470
%stackaddr$env-ref62471 = alloca %struct.ScmObj*, align 8
%_37foldr48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49897, i64 1)
store %struct.ScmObj* %_37foldr48059, %struct.ScmObj** %stackaddr$env-ref62471
%stackaddr$env-ref62472 = alloca %struct.ScmObj*, align 8
%_37foldl48137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49897, i64 2)
store %struct.ScmObj* %_37foldl48137, %struct.ScmObj** %stackaddr$env-ref62472
%stackaddr$env-ref62473 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49897, i64 3)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref62473
%stackaddr$env-ref62474 = alloca %struct.ScmObj*, align 8
%_37map148085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49897, i64 4)
store %struct.ScmObj* %_37map148085, %struct.ScmObj** %stackaddr$env-ref62474
%stackaddr$env-ref62475 = alloca %struct.ScmObj*, align 8
%k48479 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49897, i64 5)
store %struct.ScmObj* %k48479, %struct.ScmObj** %stackaddr$env-ref62475
%stackaddr$env-ref62476 = alloca %struct.ScmObj*, align 8
%f48141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49897, i64 6)
store %struct.ScmObj* %f48141, %struct.ScmObj** %stackaddr$env-ref62476
%stackaddr$env-ref62477 = alloca %struct.ScmObj*, align 8
%acc48140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49897, i64 7)
store %struct.ScmObj* %acc48140, %struct.ScmObj** %stackaddr$env-ref62477
%stackaddr$prim62478 = alloca %struct.ScmObj*, align 8
%_95k48480 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60679)
store volatile %struct.ScmObj* %_95k48480, %struct.ScmObj** %stackaddr$prim62478, align 8
%stackaddr$prim62479 = alloca %struct.ScmObj*, align 8
%current_45args60680 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60679)
store volatile %struct.ScmObj* %current_45args60680, %struct.ScmObj** %stackaddr$prim62479, align 8
%stackaddr$prim62480 = alloca %struct.ScmObj*, align 8
%anf_45bind48239 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60680)
store volatile %struct.ScmObj* %anf_45bind48239, %struct.ScmObj** %stackaddr$prim62480, align 8
%stackaddr$makeclosure62481 = alloca %struct.ScmObj*, align 8
%fptrToInt62482 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49929 to i64
%ae49929 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt62482)
store volatile %struct.ScmObj* %ae49929, %struct.ScmObj** %stackaddr$makeclosure62481, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49929, %struct.ScmObj* %lsts48139, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49929, %struct.ScmObj* %_37foldr48059, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49929, %struct.ScmObj* %_37foldl48137, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49929, %struct.ScmObj* %_37map148085, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49929, %struct.ScmObj* %k48479, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49929, %struct.ScmObj* %f48141, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49929, %struct.ScmObj* %acc48140, i64 6)
%ae49931 = call %struct.ScmObj* @const_init_false()
%argslist60726$_37foldr1480540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62483 = alloca %struct.ScmObj*, align 8
%argslist60726$_37foldr1480541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48139, %struct.ScmObj* %argslist60726$_37foldr1480540)
store volatile %struct.ScmObj* %argslist60726$_37foldr1480541, %struct.ScmObj** %stackaddr$prim62483, align 8
%stackaddr$prim62484 = alloca %struct.ScmObj*, align 8
%argslist60726$_37foldr1480542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49931, %struct.ScmObj* %argslist60726$_37foldr1480541)
store volatile %struct.ScmObj* %argslist60726$_37foldr1480542, %struct.ScmObj** %stackaddr$prim62484, align 8
%stackaddr$prim62485 = alloca %struct.ScmObj*, align 8
%argslist60726$_37foldr1480543 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48239, %struct.ScmObj* %argslist60726$_37foldr1480542)
store volatile %struct.ScmObj* %argslist60726$_37foldr1480543, %struct.ScmObj** %stackaddr$prim62485, align 8
%stackaddr$prim62486 = alloca %struct.ScmObj*, align 8
%argslist60726$_37foldr1480544 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49929, %struct.ScmObj* %argslist60726$_37foldr1480543)
store volatile %struct.ScmObj* %argslist60726$_37foldr1480544, %struct.ScmObj** %stackaddr$prim62486, align 8
%clofunc62487 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148054)
musttail call tailcc void %clofunc62487(%struct.ScmObj* %_37foldr148054, %struct.ScmObj* %argslist60726$_37foldr1480544)
ret void
}

define tailcc void @proc_clo$ae49929(%struct.ScmObj* %env$ae49929,%struct.ScmObj* %current_45args60682) {
%stackaddr$env-ref62488 = alloca %struct.ScmObj*, align 8
%lsts48139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49929, i64 0)
store %struct.ScmObj* %lsts48139, %struct.ScmObj** %stackaddr$env-ref62488
%stackaddr$env-ref62489 = alloca %struct.ScmObj*, align 8
%_37foldr48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49929, i64 1)
store %struct.ScmObj* %_37foldr48059, %struct.ScmObj** %stackaddr$env-ref62489
%stackaddr$env-ref62490 = alloca %struct.ScmObj*, align 8
%_37foldl48137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49929, i64 2)
store %struct.ScmObj* %_37foldl48137, %struct.ScmObj** %stackaddr$env-ref62490
%stackaddr$env-ref62491 = alloca %struct.ScmObj*, align 8
%_37map148085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49929, i64 3)
store %struct.ScmObj* %_37map148085, %struct.ScmObj** %stackaddr$env-ref62491
%stackaddr$env-ref62492 = alloca %struct.ScmObj*, align 8
%k48479 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49929, i64 4)
store %struct.ScmObj* %k48479, %struct.ScmObj** %stackaddr$env-ref62492
%stackaddr$env-ref62493 = alloca %struct.ScmObj*, align 8
%f48141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49929, i64 5)
store %struct.ScmObj* %f48141, %struct.ScmObj** %stackaddr$env-ref62493
%stackaddr$env-ref62494 = alloca %struct.ScmObj*, align 8
%acc48140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49929, i64 6)
store %struct.ScmObj* %acc48140, %struct.ScmObj** %stackaddr$env-ref62494
%stackaddr$prim62495 = alloca %struct.ScmObj*, align 8
%_95k48481 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60682)
store volatile %struct.ScmObj* %_95k48481, %struct.ScmObj** %stackaddr$prim62495, align 8
%stackaddr$prim62496 = alloca %struct.ScmObj*, align 8
%current_45args60683 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60682)
store volatile %struct.ScmObj* %current_45args60683, %struct.ScmObj** %stackaddr$prim62496, align 8
%stackaddr$prim62497 = alloca %struct.ScmObj*, align 8
%anf_45bind48240 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60683)
store volatile %struct.ScmObj* %anf_45bind48240, %struct.ScmObj** %stackaddr$prim62497, align 8
%truthy$cmp62498 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48240)
%cmp$cmp62498 = icmp eq i64 %truthy$cmp62498, 1
br i1 %cmp$cmp62498, label %truebranch$cmp62498, label %falsebranch$cmp62498
truebranch$cmp62498:
%ae49940 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist60685$k484790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62499 = alloca %struct.ScmObj*, align 8
%argslist60685$k484791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48140, %struct.ScmObj* %argslist60685$k484790)
store volatile %struct.ScmObj* %argslist60685$k484791, %struct.ScmObj** %stackaddr$prim62499, align 8
%stackaddr$prim62500 = alloca %struct.ScmObj*, align 8
%argslist60685$k484792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49940, %struct.ScmObj* %argslist60685$k484791)
store volatile %struct.ScmObj* %argslist60685$k484792, %struct.ScmObj** %stackaddr$prim62500, align 8
%clofunc62501 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48479)
musttail call tailcc void %clofunc62501(%struct.ScmObj* %k48479, %struct.ScmObj* %argslist60685$k484792)
ret void
falsebranch$cmp62498:
%stackaddr$makeclosure62502 = alloca %struct.ScmObj*, align 8
%fptrToInt62503 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49945 to i64
%ae49945 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt62503)
store volatile %struct.ScmObj* %ae49945, %struct.ScmObj** %stackaddr$makeclosure62502, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49945, %struct.ScmObj* %lsts48139, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49945, %struct.ScmObj* %_37foldr48059, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49945, %struct.ScmObj* %_37foldl48137, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49945, %struct.ScmObj* %_37map148085, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49945, %struct.ScmObj* %k48479, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49945, %struct.ScmObj* %f48141, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49945, %struct.ScmObj* %acc48140, i64 6)
%ae49946 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure62504 = alloca %struct.ScmObj*, align 8
%fptrToInt62505 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49947 to i64
%ae49947 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt62505)
store volatile %struct.ScmObj* %ae49947, %struct.ScmObj** %stackaddr$makeclosure62504, align 8
%argslist60725$ae499450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62506 = alloca %struct.ScmObj*, align 8
%argslist60725$ae499451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49947, %struct.ScmObj* %argslist60725$ae499450)
store volatile %struct.ScmObj* %argslist60725$ae499451, %struct.ScmObj** %stackaddr$prim62506, align 8
%stackaddr$prim62507 = alloca %struct.ScmObj*, align 8
%argslist60725$ae499452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49946, %struct.ScmObj* %argslist60725$ae499451)
store volatile %struct.ScmObj* %argslist60725$ae499452, %struct.ScmObj** %stackaddr$prim62507, align 8
%clofunc62508 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49945)
musttail call tailcc void %clofunc62508(%struct.ScmObj* %ae49945, %struct.ScmObj* %argslist60725$ae499452)
ret void
}

define tailcc void @proc_clo$ae49945(%struct.ScmObj* %env$ae49945,%struct.ScmObj* %current_45args60686) {
%stackaddr$env-ref62509 = alloca %struct.ScmObj*, align 8
%lsts48139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49945, i64 0)
store %struct.ScmObj* %lsts48139, %struct.ScmObj** %stackaddr$env-ref62509
%stackaddr$env-ref62510 = alloca %struct.ScmObj*, align 8
%_37foldr48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49945, i64 1)
store %struct.ScmObj* %_37foldr48059, %struct.ScmObj** %stackaddr$env-ref62510
%stackaddr$env-ref62511 = alloca %struct.ScmObj*, align 8
%_37foldl48137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49945, i64 2)
store %struct.ScmObj* %_37foldl48137, %struct.ScmObj** %stackaddr$env-ref62511
%stackaddr$env-ref62512 = alloca %struct.ScmObj*, align 8
%_37map148085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49945, i64 3)
store %struct.ScmObj* %_37map148085, %struct.ScmObj** %stackaddr$env-ref62512
%stackaddr$env-ref62513 = alloca %struct.ScmObj*, align 8
%k48479 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49945, i64 4)
store %struct.ScmObj* %k48479, %struct.ScmObj** %stackaddr$env-ref62513
%stackaddr$env-ref62514 = alloca %struct.ScmObj*, align 8
%f48141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49945, i64 5)
store %struct.ScmObj* %f48141, %struct.ScmObj** %stackaddr$env-ref62514
%stackaddr$env-ref62515 = alloca %struct.ScmObj*, align 8
%acc48140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49945, i64 6)
store %struct.ScmObj* %acc48140, %struct.ScmObj** %stackaddr$env-ref62515
%stackaddr$prim62516 = alloca %struct.ScmObj*, align 8
%_95k48482 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60686)
store volatile %struct.ScmObj* %_95k48482, %struct.ScmObj** %stackaddr$prim62516, align 8
%stackaddr$prim62517 = alloca %struct.ScmObj*, align 8
%current_45args60687 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60686)
store volatile %struct.ScmObj* %current_45args60687, %struct.ScmObj** %stackaddr$prim62517, align 8
%stackaddr$prim62518 = alloca %struct.ScmObj*, align 8
%anf_45bind48241 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60687)
store volatile %struct.ScmObj* %anf_45bind48241, %struct.ScmObj** %stackaddr$prim62518, align 8
%stackaddr$makeclosure62519 = alloca %struct.ScmObj*, align 8
%fptrToInt62520 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49966 to i64
%ae49966 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt62520)
store volatile %struct.ScmObj* %ae49966, %struct.ScmObj** %stackaddr$makeclosure62519, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49966, %struct.ScmObj* %lsts48139, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49966, %struct.ScmObj* %_37foldr48059, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49966, %struct.ScmObj* %_37foldl48137, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49966, %struct.ScmObj* %_37map148085, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49966, %struct.ScmObj* %k48479, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49966, %struct.ScmObj* %f48141, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49966, %struct.ScmObj* %acc48140, i64 6)
%argslist60720$_37map1480850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62521 = alloca %struct.ScmObj*, align 8
%argslist60720$_37map1480851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48139, %struct.ScmObj* %argslist60720$_37map1480850)
store volatile %struct.ScmObj* %argslist60720$_37map1480851, %struct.ScmObj** %stackaddr$prim62521, align 8
%stackaddr$prim62522 = alloca %struct.ScmObj*, align 8
%argslist60720$_37map1480852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48241, %struct.ScmObj* %argslist60720$_37map1480851)
store volatile %struct.ScmObj* %argslist60720$_37map1480852, %struct.ScmObj** %stackaddr$prim62522, align 8
%stackaddr$prim62523 = alloca %struct.ScmObj*, align 8
%argslist60720$_37map1480853 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49966, %struct.ScmObj* %argslist60720$_37map1480852)
store volatile %struct.ScmObj* %argslist60720$_37map1480853, %struct.ScmObj** %stackaddr$prim62523, align 8
%clofunc62524 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148085)
musttail call tailcc void %clofunc62524(%struct.ScmObj* %_37map148085, %struct.ScmObj* %argslist60720$_37map1480853)
ret void
}

define tailcc void @proc_clo$ae49966(%struct.ScmObj* %env$ae49966,%struct.ScmObj* %current_45args60689) {
%stackaddr$env-ref62525 = alloca %struct.ScmObj*, align 8
%lsts48139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49966, i64 0)
store %struct.ScmObj* %lsts48139, %struct.ScmObj** %stackaddr$env-ref62525
%stackaddr$env-ref62526 = alloca %struct.ScmObj*, align 8
%_37foldr48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49966, i64 1)
store %struct.ScmObj* %_37foldr48059, %struct.ScmObj** %stackaddr$env-ref62526
%stackaddr$env-ref62527 = alloca %struct.ScmObj*, align 8
%_37foldl48137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49966, i64 2)
store %struct.ScmObj* %_37foldl48137, %struct.ScmObj** %stackaddr$env-ref62527
%stackaddr$env-ref62528 = alloca %struct.ScmObj*, align 8
%_37map148085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49966, i64 3)
store %struct.ScmObj* %_37map148085, %struct.ScmObj** %stackaddr$env-ref62528
%stackaddr$env-ref62529 = alloca %struct.ScmObj*, align 8
%k48479 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49966, i64 4)
store %struct.ScmObj* %k48479, %struct.ScmObj** %stackaddr$env-ref62529
%stackaddr$env-ref62530 = alloca %struct.ScmObj*, align 8
%f48141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49966, i64 5)
store %struct.ScmObj* %f48141, %struct.ScmObj** %stackaddr$env-ref62530
%stackaddr$env-ref62531 = alloca %struct.ScmObj*, align 8
%acc48140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49966, i64 6)
store %struct.ScmObj* %acc48140, %struct.ScmObj** %stackaddr$env-ref62531
%stackaddr$prim62532 = alloca %struct.ScmObj*, align 8
%_95k48483 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60689)
store volatile %struct.ScmObj* %_95k48483, %struct.ScmObj** %stackaddr$prim62532, align 8
%stackaddr$prim62533 = alloca %struct.ScmObj*, align 8
%current_45args60690 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60689)
store volatile %struct.ScmObj* %current_45args60690, %struct.ScmObj** %stackaddr$prim62533, align 8
%stackaddr$prim62534 = alloca %struct.ScmObj*, align 8
%lsts_4348146 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60690)
store volatile %struct.ScmObj* %lsts_4348146, %struct.ScmObj** %stackaddr$prim62534, align 8
%stackaddr$makeclosure62535 = alloca %struct.ScmObj*, align 8
%fptrToInt62536 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49969 to i64
%ae49969 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt62536)
store volatile %struct.ScmObj* %ae49969, %struct.ScmObj** %stackaddr$makeclosure62535, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49969, %struct.ScmObj* %lsts48139, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49969, %struct.ScmObj* %_37foldr48059, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49969, %struct.ScmObj* %_37foldl48137, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49969, %struct.ScmObj* %_37map148085, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49969, %struct.ScmObj* %lsts_4348146, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49969, %struct.ScmObj* %k48479, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49969, %struct.ScmObj* %f48141, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae49969, %struct.ScmObj* %acc48140, i64 7)
%ae49970 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure62537 = alloca %struct.ScmObj*, align 8
%fptrToInt62538 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49971 to i64
%ae49971 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt62538)
store volatile %struct.ScmObj* %ae49971, %struct.ScmObj** %stackaddr$makeclosure62537, align 8
%argslist60719$ae499690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62539 = alloca %struct.ScmObj*, align 8
%argslist60719$ae499691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49971, %struct.ScmObj* %argslist60719$ae499690)
store volatile %struct.ScmObj* %argslist60719$ae499691, %struct.ScmObj** %stackaddr$prim62539, align 8
%stackaddr$prim62540 = alloca %struct.ScmObj*, align 8
%argslist60719$ae499692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49970, %struct.ScmObj* %argslist60719$ae499691)
store volatile %struct.ScmObj* %argslist60719$ae499692, %struct.ScmObj** %stackaddr$prim62540, align 8
%clofunc62541 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49969)
musttail call tailcc void %clofunc62541(%struct.ScmObj* %ae49969, %struct.ScmObj* %argslist60719$ae499692)
ret void
}

define tailcc void @proc_clo$ae49969(%struct.ScmObj* %env$ae49969,%struct.ScmObj* %current_45args60692) {
%stackaddr$env-ref62542 = alloca %struct.ScmObj*, align 8
%lsts48139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49969, i64 0)
store %struct.ScmObj* %lsts48139, %struct.ScmObj** %stackaddr$env-ref62542
%stackaddr$env-ref62543 = alloca %struct.ScmObj*, align 8
%_37foldr48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49969, i64 1)
store %struct.ScmObj* %_37foldr48059, %struct.ScmObj** %stackaddr$env-ref62543
%stackaddr$env-ref62544 = alloca %struct.ScmObj*, align 8
%_37foldl48137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49969, i64 2)
store %struct.ScmObj* %_37foldl48137, %struct.ScmObj** %stackaddr$env-ref62544
%stackaddr$env-ref62545 = alloca %struct.ScmObj*, align 8
%_37map148085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49969, i64 3)
store %struct.ScmObj* %_37map148085, %struct.ScmObj** %stackaddr$env-ref62545
%stackaddr$env-ref62546 = alloca %struct.ScmObj*, align 8
%lsts_4348146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49969, i64 4)
store %struct.ScmObj* %lsts_4348146, %struct.ScmObj** %stackaddr$env-ref62546
%stackaddr$env-ref62547 = alloca %struct.ScmObj*, align 8
%k48479 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49969, i64 5)
store %struct.ScmObj* %k48479, %struct.ScmObj** %stackaddr$env-ref62547
%stackaddr$env-ref62548 = alloca %struct.ScmObj*, align 8
%f48141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49969, i64 6)
store %struct.ScmObj* %f48141, %struct.ScmObj** %stackaddr$env-ref62548
%stackaddr$env-ref62549 = alloca %struct.ScmObj*, align 8
%acc48140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49969, i64 7)
store %struct.ScmObj* %acc48140, %struct.ScmObj** %stackaddr$env-ref62549
%stackaddr$prim62550 = alloca %struct.ScmObj*, align 8
%_95k48484 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60692)
store volatile %struct.ScmObj* %_95k48484, %struct.ScmObj** %stackaddr$prim62550, align 8
%stackaddr$prim62551 = alloca %struct.ScmObj*, align 8
%current_45args60693 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60692)
store volatile %struct.ScmObj* %current_45args60693, %struct.ScmObj** %stackaddr$prim62551, align 8
%stackaddr$prim62552 = alloca %struct.ScmObj*, align 8
%anf_45bind48242 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60693)
store volatile %struct.ScmObj* %anf_45bind48242, %struct.ScmObj** %stackaddr$prim62552, align 8
%stackaddr$makeclosure62553 = alloca %struct.ScmObj*, align 8
%fptrToInt62554 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49990 to i64
%ae49990 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt62554)
store volatile %struct.ScmObj* %ae49990, %struct.ScmObj** %stackaddr$makeclosure62553, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49990, %struct.ScmObj* %_37foldl48137, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49990, %struct.ScmObj* %lsts_4348146, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49990, %struct.ScmObj* %k48479, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49990, %struct.ScmObj* %f48141, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49990, %struct.ScmObj* %acc48140, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49990, %struct.ScmObj* %_37foldr48059, i64 5)
%argslist60714$_37map1480850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62555 = alloca %struct.ScmObj*, align 8
%argslist60714$_37map1480851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48139, %struct.ScmObj* %argslist60714$_37map1480850)
store volatile %struct.ScmObj* %argslist60714$_37map1480851, %struct.ScmObj** %stackaddr$prim62555, align 8
%stackaddr$prim62556 = alloca %struct.ScmObj*, align 8
%argslist60714$_37map1480852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48242, %struct.ScmObj* %argslist60714$_37map1480851)
store volatile %struct.ScmObj* %argslist60714$_37map1480852, %struct.ScmObj** %stackaddr$prim62556, align 8
%stackaddr$prim62557 = alloca %struct.ScmObj*, align 8
%argslist60714$_37map1480853 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49990, %struct.ScmObj* %argslist60714$_37map1480852)
store volatile %struct.ScmObj* %argslist60714$_37map1480853, %struct.ScmObj** %stackaddr$prim62557, align 8
%clofunc62558 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148085)
musttail call tailcc void %clofunc62558(%struct.ScmObj* %_37map148085, %struct.ScmObj* %argslist60714$_37map1480853)
ret void
}

define tailcc void @proc_clo$ae49990(%struct.ScmObj* %env$ae49990,%struct.ScmObj* %current_45args60695) {
%stackaddr$env-ref62559 = alloca %struct.ScmObj*, align 8
%_37foldl48137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49990, i64 0)
store %struct.ScmObj* %_37foldl48137, %struct.ScmObj** %stackaddr$env-ref62559
%stackaddr$env-ref62560 = alloca %struct.ScmObj*, align 8
%lsts_4348146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49990, i64 1)
store %struct.ScmObj* %lsts_4348146, %struct.ScmObj** %stackaddr$env-ref62560
%stackaddr$env-ref62561 = alloca %struct.ScmObj*, align 8
%k48479 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49990, i64 2)
store %struct.ScmObj* %k48479, %struct.ScmObj** %stackaddr$env-ref62561
%stackaddr$env-ref62562 = alloca %struct.ScmObj*, align 8
%f48141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49990, i64 3)
store %struct.ScmObj* %f48141, %struct.ScmObj** %stackaddr$env-ref62562
%stackaddr$env-ref62563 = alloca %struct.ScmObj*, align 8
%acc48140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49990, i64 4)
store %struct.ScmObj* %acc48140, %struct.ScmObj** %stackaddr$env-ref62563
%stackaddr$env-ref62564 = alloca %struct.ScmObj*, align 8
%_37foldr48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49990, i64 5)
store %struct.ScmObj* %_37foldr48059, %struct.ScmObj** %stackaddr$env-ref62564
%stackaddr$prim62565 = alloca %struct.ScmObj*, align 8
%_95k48485 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60695)
store volatile %struct.ScmObj* %_95k48485, %struct.ScmObj** %stackaddr$prim62565, align 8
%stackaddr$prim62566 = alloca %struct.ScmObj*, align 8
%current_45args60696 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60695)
store volatile %struct.ScmObj* %current_45args60696, %struct.ScmObj** %stackaddr$prim62566, align 8
%stackaddr$prim62567 = alloca %struct.ScmObj*, align 8
%vs48144 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60696)
store volatile %struct.ScmObj* %vs48144, %struct.ScmObj** %stackaddr$prim62567, align 8
%stackaddr$makeclosure62568 = alloca %struct.ScmObj*, align 8
%fptrToInt62569 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49993 to i64
%ae49993 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt62569)
store volatile %struct.ScmObj* %ae49993, %struct.ScmObj** %stackaddr$makeclosure62568, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49993, %struct.ScmObj* %_37foldl48137, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49993, %struct.ScmObj* %lsts_4348146, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49993, %struct.ScmObj* %vs48144, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49993, %struct.ScmObj* %k48479, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49993, %struct.ScmObj* %f48141, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49993, %struct.ScmObj* %acc48140, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49993, %struct.ScmObj* %_37foldr48059, i64 6)
%ae49994 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure62570 = alloca %struct.ScmObj*, align 8
%fptrToInt62571 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49995 to i64
%ae49995 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt62571)
store volatile %struct.ScmObj* %ae49995, %struct.ScmObj** %stackaddr$makeclosure62570, align 8
%argslist60713$ae499930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62572 = alloca %struct.ScmObj*, align 8
%argslist60713$ae499931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49995, %struct.ScmObj* %argslist60713$ae499930)
store volatile %struct.ScmObj* %argslist60713$ae499931, %struct.ScmObj** %stackaddr$prim62572, align 8
%stackaddr$prim62573 = alloca %struct.ScmObj*, align 8
%argslist60713$ae499932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49994, %struct.ScmObj* %argslist60713$ae499931)
store volatile %struct.ScmObj* %argslist60713$ae499932, %struct.ScmObj** %stackaddr$prim62573, align 8
%clofunc62574 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49993)
musttail call tailcc void %clofunc62574(%struct.ScmObj* %ae49993, %struct.ScmObj* %argslist60713$ae499932)
ret void
}

define tailcc void @proc_clo$ae49993(%struct.ScmObj* %env$ae49993,%struct.ScmObj* %current_45args60698) {
%stackaddr$env-ref62575 = alloca %struct.ScmObj*, align 8
%_37foldl48137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49993, i64 0)
store %struct.ScmObj* %_37foldl48137, %struct.ScmObj** %stackaddr$env-ref62575
%stackaddr$env-ref62576 = alloca %struct.ScmObj*, align 8
%lsts_4348146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49993, i64 1)
store %struct.ScmObj* %lsts_4348146, %struct.ScmObj** %stackaddr$env-ref62576
%stackaddr$env-ref62577 = alloca %struct.ScmObj*, align 8
%vs48144 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49993, i64 2)
store %struct.ScmObj* %vs48144, %struct.ScmObj** %stackaddr$env-ref62577
%stackaddr$env-ref62578 = alloca %struct.ScmObj*, align 8
%k48479 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49993, i64 3)
store %struct.ScmObj* %k48479, %struct.ScmObj** %stackaddr$env-ref62578
%stackaddr$env-ref62579 = alloca %struct.ScmObj*, align 8
%f48141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49993, i64 4)
store %struct.ScmObj* %f48141, %struct.ScmObj** %stackaddr$env-ref62579
%stackaddr$env-ref62580 = alloca %struct.ScmObj*, align 8
%acc48140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49993, i64 5)
store %struct.ScmObj* %acc48140, %struct.ScmObj** %stackaddr$env-ref62580
%stackaddr$env-ref62581 = alloca %struct.ScmObj*, align 8
%_37foldr48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49993, i64 6)
store %struct.ScmObj* %_37foldr48059, %struct.ScmObj** %stackaddr$env-ref62581
%stackaddr$prim62582 = alloca %struct.ScmObj*, align 8
%_95k48486 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60698)
store volatile %struct.ScmObj* %_95k48486, %struct.ScmObj** %stackaddr$prim62582, align 8
%stackaddr$prim62583 = alloca %struct.ScmObj*, align 8
%current_45args60699 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60698)
store volatile %struct.ScmObj* %current_45args60699, %struct.ScmObj** %stackaddr$prim62583, align 8
%stackaddr$prim62584 = alloca %struct.ScmObj*, align 8
%anf_45bind48243 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60699)
store volatile %struct.ScmObj* %anf_45bind48243, %struct.ScmObj** %stackaddr$prim62584, align 8
%ae50016 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62585 = alloca %struct.ScmObj*, align 8
%anf_45bind48244 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48140, %struct.ScmObj* %ae50016)
store volatile %struct.ScmObj* %anf_45bind48244, %struct.ScmObj** %stackaddr$prim62585, align 8
%stackaddr$makeclosure62586 = alloca %struct.ScmObj*, align 8
%fptrToInt62587 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50018 to i64
%ae50018 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt62587)
store volatile %struct.ScmObj* %ae50018, %struct.ScmObj** %stackaddr$makeclosure62586, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50018, %struct.ScmObj* %_37foldl48137, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50018, %struct.ScmObj* %lsts_4348146, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50018, %struct.ScmObj* %k48479, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae50018, %struct.ScmObj* %f48141, i64 3)
%argslist60707$_37foldr480590 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62588 = alloca %struct.ScmObj*, align 8
%argslist60707$_37foldr480591 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs48144, %struct.ScmObj* %argslist60707$_37foldr480590)
store volatile %struct.ScmObj* %argslist60707$_37foldr480591, %struct.ScmObj** %stackaddr$prim62588, align 8
%stackaddr$prim62589 = alloca %struct.ScmObj*, align 8
%argslist60707$_37foldr480592 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48244, %struct.ScmObj* %argslist60707$_37foldr480591)
store volatile %struct.ScmObj* %argslist60707$_37foldr480592, %struct.ScmObj** %stackaddr$prim62589, align 8
%stackaddr$prim62590 = alloca %struct.ScmObj*, align 8
%argslist60707$_37foldr480593 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48243, %struct.ScmObj* %argslist60707$_37foldr480592)
store volatile %struct.ScmObj* %argslist60707$_37foldr480593, %struct.ScmObj** %stackaddr$prim62590, align 8
%stackaddr$prim62591 = alloca %struct.ScmObj*, align 8
%argslist60707$_37foldr480594 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50018, %struct.ScmObj* %argslist60707$_37foldr480593)
store volatile %struct.ScmObj* %argslist60707$_37foldr480594, %struct.ScmObj** %stackaddr$prim62591, align 8
%clofunc62592 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr48059)
musttail call tailcc void %clofunc62592(%struct.ScmObj* %_37foldr48059, %struct.ScmObj* %argslist60707$_37foldr480594)
ret void
}

define tailcc void @proc_clo$ae50018(%struct.ScmObj* %env$ae50018,%struct.ScmObj* %current_45args60701) {
%stackaddr$env-ref62593 = alloca %struct.ScmObj*, align 8
%_37foldl48137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50018, i64 0)
store %struct.ScmObj* %_37foldl48137, %struct.ScmObj** %stackaddr$env-ref62593
%stackaddr$env-ref62594 = alloca %struct.ScmObj*, align 8
%lsts_4348146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50018, i64 1)
store %struct.ScmObj* %lsts_4348146, %struct.ScmObj** %stackaddr$env-ref62594
%stackaddr$env-ref62595 = alloca %struct.ScmObj*, align 8
%k48479 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50018, i64 2)
store %struct.ScmObj* %k48479, %struct.ScmObj** %stackaddr$env-ref62595
%stackaddr$env-ref62596 = alloca %struct.ScmObj*, align 8
%f48141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50018, i64 3)
store %struct.ScmObj* %f48141, %struct.ScmObj** %stackaddr$env-ref62596
%stackaddr$prim62597 = alloca %struct.ScmObj*, align 8
%_95k48487 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60701)
store volatile %struct.ScmObj* %_95k48487, %struct.ScmObj** %stackaddr$prim62597, align 8
%stackaddr$prim62598 = alloca %struct.ScmObj*, align 8
%current_45args60702 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60701)
store volatile %struct.ScmObj* %current_45args60702, %struct.ScmObj** %stackaddr$prim62598, align 8
%stackaddr$prim62599 = alloca %struct.ScmObj*, align 8
%anf_45bind48245 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60702)
store volatile %struct.ScmObj* %anf_45bind48245, %struct.ScmObj** %stackaddr$prim62599, align 8
%stackaddr$makeclosure62600 = alloca %struct.ScmObj*, align 8
%fptrToInt62601 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50022 to i64
%ae50022 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt62601)
store volatile %struct.ScmObj* %ae50022, %struct.ScmObj** %stackaddr$makeclosure62600, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50022, %struct.ScmObj* %_37foldl48137, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50022, %struct.ScmObj* %lsts_4348146, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50022, %struct.ScmObj* %k48479, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae50022, %struct.ScmObj* %f48141, i64 3)
%stackaddr$prim62602 = alloca %struct.ScmObj*, align 8
%cpsargs48490 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50022, %struct.ScmObj* %anf_45bind48245)
store volatile %struct.ScmObj* %cpsargs48490, %struct.ScmObj** %stackaddr$prim62602, align 8
%clofunc62603 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48141)
musttail call tailcc void %clofunc62603(%struct.ScmObj* %f48141, %struct.ScmObj* %cpsargs48490)
ret void
}

define tailcc void @proc_clo$ae50022(%struct.ScmObj* %env$ae50022,%struct.ScmObj* %current_45args60704) {
%stackaddr$env-ref62604 = alloca %struct.ScmObj*, align 8
%_37foldl48137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50022, i64 0)
store %struct.ScmObj* %_37foldl48137, %struct.ScmObj** %stackaddr$env-ref62604
%stackaddr$env-ref62605 = alloca %struct.ScmObj*, align 8
%lsts_4348146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50022, i64 1)
store %struct.ScmObj* %lsts_4348146, %struct.ScmObj** %stackaddr$env-ref62605
%stackaddr$env-ref62606 = alloca %struct.ScmObj*, align 8
%k48479 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50022, i64 2)
store %struct.ScmObj* %k48479, %struct.ScmObj** %stackaddr$env-ref62606
%stackaddr$env-ref62607 = alloca %struct.ScmObj*, align 8
%f48141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50022, i64 3)
store %struct.ScmObj* %f48141, %struct.ScmObj** %stackaddr$env-ref62607
%stackaddr$prim62608 = alloca %struct.ScmObj*, align 8
%_95k48488 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60704)
store volatile %struct.ScmObj* %_95k48488, %struct.ScmObj** %stackaddr$prim62608, align 8
%stackaddr$prim62609 = alloca %struct.ScmObj*, align 8
%current_45args60705 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60704)
store volatile %struct.ScmObj* %current_45args60705, %struct.ScmObj** %stackaddr$prim62609, align 8
%stackaddr$prim62610 = alloca %struct.ScmObj*, align 8
%acc_4348148 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60705)
store volatile %struct.ScmObj* %acc_4348148, %struct.ScmObj** %stackaddr$prim62610, align 8
%stackaddr$prim62611 = alloca %struct.ScmObj*, align 8
%anf_45bind48246 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4348148, %struct.ScmObj* %lsts_4348146)
store volatile %struct.ScmObj* %anf_45bind48246, %struct.ScmObj** %stackaddr$prim62611, align 8
%stackaddr$prim62612 = alloca %struct.ScmObj*, align 8
%anf_45bind48247 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48141, %struct.ScmObj* %anf_45bind48246)
store volatile %struct.ScmObj* %anf_45bind48247, %struct.ScmObj** %stackaddr$prim62612, align 8
%stackaddr$prim62613 = alloca %struct.ScmObj*, align 8
%cpsargs48489 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48479, %struct.ScmObj* %anf_45bind48247)
store volatile %struct.ScmObj* %cpsargs48489, %struct.ScmObj** %stackaddr$prim62613, align 8
%clofunc62614 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl48137)
musttail call tailcc void %clofunc62614(%struct.ScmObj* %_37foldl48137, %struct.ScmObj* %cpsargs48489)
ret void
}

define tailcc void @proc_clo$ae49995(%struct.ScmObj* %env$ae49995,%struct.ScmObj* %current_45args60708) {
%stackaddr$prim62615 = alloca %struct.ScmObj*, align 8
%k48491 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60708)
store volatile %struct.ScmObj* %k48491, %struct.ScmObj** %stackaddr$prim62615, align 8
%stackaddr$prim62616 = alloca %struct.ScmObj*, align 8
%current_45args60709 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60708)
store volatile %struct.ScmObj* %current_45args60709, %struct.ScmObj** %stackaddr$prim62616, align 8
%stackaddr$prim62617 = alloca %struct.ScmObj*, align 8
%a48150 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60709)
store volatile %struct.ScmObj* %a48150, %struct.ScmObj** %stackaddr$prim62617, align 8
%stackaddr$prim62618 = alloca %struct.ScmObj*, align 8
%current_45args60710 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60709)
store volatile %struct.ScmObj* %current_45args60710, %struct.ScmObj** %stackaddr$prim62618, align 8
%stackaddr$prim62619 = alloca %struct.ScmObj*, align 8
%b48149 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60710)
store volatile %struct.ScmObj* %b48149, %struct.ScmObj** %stackaddr$prim62619, align 8
%stackaddr$prim62620 = alloca %struct.ScmObj*, align 8
%cpsprim48492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a48150, %struct.ScmObj* %b48149)
store volatile %struct.ScmObj* %cpsprim48492, %struct.ScmObj** %stackaddr$prim62620, align 8
%ae49999 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist60712$k484910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62621 = alloca %struct.ScmObj*, align 8
%argslist60712$k484911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48492, %struct.ScmObj* %argslist60712$k484910)
store volatile %struct.ScmObj* %argslist60712$k484911, %struct.ScmObj** %stackaddr$prim62621, align 8
%stackaddr$prim62622 = alloca %struct.ScmObj*, align 8
%argslist60712$k484912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49999, %struct.ScmObj* %argslist60712$k484911)
store volatile %struct.ScmObj* %argslist60712$k484912, %struct.ScmObj** %stackaddr$prim62622, align 8
%clofunc62623 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48491)
musttail call tailcc void %clofunc62623(%struct.ScmObj* %k48491, %struct.ScmObj* %argslist60712$k484912)
ret void
}

define tailcc void @proc_clo$ae49971(%struct.ScmObj* %env$ae49971,%struct.ScmObj* %current_45args60715) {
%stackaddr$prim62624 = alloca %struct.ScmObj*, align 8
%k48493 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60715)
store volatile %struct.ScmObj* %k48493, %struct.ScmObj** %stackaddr$prim62624, align 8
%stackaddr$prim62625 = alloca %struct.ScmObj*, align 8
%current_45args60716 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60715)
store volatile %struct.ScmObj* %current_45args60716, %struct.ScmObj** %stackaddr$prim62625, align 8
%stackaddr$prim62626 = alloca %struct.ScmObj*, align 8
%x48145 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60716)
store volatile %struct.ScmObj* %x48145, %struct.ScmObj** %stackaddr$prim62626, align 8
%stackaddr$prim62627 = alloca %struct.ScmObj*, align 8
%cpsprim48494 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48145)
store volatile %struct.ScmObj* %cpsprim48494, %struct.ScmObj** %stackaddr$prim62627, align 8
%ae49974 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist60718$k484930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62628 = alloca %struct.ScmObj*, align 8
%argslist60718$k484931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48494, %struct.ScmObj* %argslist60718$k484930)
store volatile %struct.ScmObj* %argslist60718$k484931, %struct.ScmObj** %stackaddr$prim62628, align 8
%stackaddr$prim62629 = alloca %struct.ScmObj*, align 8
%argslist60718$k484932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49974, %struct.ScmObj* %argslist60718$k484931)
store volatile %struct.ScmObj* %argslist60718$k484932, %struct.ScmObj** %stackaddr$prim62629, align 8
%clofunc62630 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48493)
musttail call tailcc void %clofunc62630(%struct.ScmObj* %k48493, %struct.ScmObj* %argslist60718$k484932)
ret void
}

define tailcc void @proc_clo$ae49947(%struct.ScmObj* %env$ae49947,%struct.ScmObj* %current_45args60721) {
%stackaddr$prim62631 = alloca %struct.ScmObj*, align 8
%k48495 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60721)
store volatile %struct.ScmObj* %k48495, %struct.ScmObj** %stackaddr$prim62631, align 8
%stackaddr$prim62632 = alloca %struct.ScmObj*, align 8
%current_45args60722 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60721)
store volatile %struct.ScmObj* %current_45args60722, %struct.ScmObj** %stackaddr$prim62632, align 8
%stackaddr$prim62633 = alloca %struct.ScmObj*, align 8
%x48147 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60722)
store volatile %struct.ScmObj* %x48147, %struct.ScmObj** %stackaddr$prim62633, align 8
%stackaddr$prim62634 = alloca %struct.ScmObj*, align 8
%cpsprim48496 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48147)
store volatile %struct.ScmObj* %cpsprim48496, %struct.ScmObj** %stackaddr$prim62634, align 8
%ae49950 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist60724$k484950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62635 = alloca %struct.ScmObj*, align 8
%argslist60724$k484951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48496, %struct.ScmObj* %argslist60724$k484950)
store volatile %struct.ScmObj* %argslist60724$k484951, %struct.ScmObj** %stackaddr$prim62635, align 8
%stackaddr$prim62636 = alloca %struct.ScmObj*, align 8
%argslist60724$k484952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49950, %struct.ScmObj* %argslist60724$k484951)
store volatile %struct.ScmObj* %argslist60724$k484952, %struct.ScmObj** %stackaddr$prim62636, align 8
%clofunc62637 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48495)
musttail call tailcc void %clofunc62637(%struct.ScmObj* %k48495, %struct.ScmObj* %argslist60724$k484952)
ret void
}

define tailcc void @proc_clo$ae49899(%struct.ScmObj* %env$ae49899,%struct.ScmObj* %current_45args60727) {
%stackaddr$prim62638 = alloca %struct.ScmObj*, align 8
%k48497 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60727)
store volatile %struct.ScmObj* %k48497, %struct.ScmObj** %stackaddr$prim62638, align 8
%stackaddr$prim62639 = alloca %struct.ScmObj*, align 8
%current_45args60728 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60727)
store volatile %struct.ScmObj* %current_45args60728, %struct.ScmObj** %stackaddr$prim62639, align 8
%stackaddr$prim62640 = alloca %struct.ScmObj*, align 8
%lst48143 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60728)
store volatile %struct.ScmObj* %lst48143, %struct.ScmObj** %stackaddr$prim62640, align 8
%stackaddr$prim62641 = alloca %struct.ScmObj*, align 8
%current_45args60729 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60728)
store volatile %struct.ScmObj* %current_45args60729, %struct.ScmObj** %stackaddr$prim62641, align 8
%stackaddr$prim62642 = alloca %struct.ScmObj*, align 8
%b48142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60729)
store volatile %struct.ScmObj* %b48142, %struct.ScmObj** %stackaddr$prim62642, align 8
%truthy$cmp62643 = call i64 @is_truthy_value(%struct.ScmObj* %b48142)
%cmp$cmp62643 = icmp eq i64 %truthy$cmp62643, 1
br i1 %cmp$cmp62643, label %truebranch$cmp62643, label %falsebranch$cmp62643
truebranch$cmp62643:
%ae49902 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist60731$k484970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62644 = alloca %struct.ScmObj*, align 8
%argslist60731$k484971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b48142, %struct.ScmObj* %argslist60731$k484970)
store volatile %struct.ScmObj* %argslist60731$k484971, %struct.ScmObj** %stackaddr$prim62644, align 8
%stackaddr$prim62645 = alloca %struct.ScmObj*, align 8
%argslist60731$k484972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49902, %struct.ScmObj* %argslist60731$k484971)
store volatile %struct.ScmObj* %argslist60731$k484972, %struct.ScmObj** %stackaddr$prim62645, align 8
%clofunc62646 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48497)
musttail call tailcc void %clofunc62646(%struct.ScmObj* %k48497, %struct.ScmObj* %argslist60731$k484972)
ret void
falsebranch$cmp62643:
%stackaddr$prim62647 = alloca %struct.ScmObj*, align 8
%cpsprim48498 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48143)
store volatile %struct.ScmObj* %cpsprim48498, %struct.ScmObj** %stackaddr$prim62647, align 8
%ae49909 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist60732$k484970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62648 = alloca %struct.ScmObj*, align 8
%argslist60732$k484971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48498, %struct.ScmObj* %argslist60732$k484970)
store volatile %struct.ScmObj* %argslist60732$k484971, %struct.ScmObj** %stackaddr$prim62648, align 8
%stackaddr$prim62649 = alloca %struct.ScmObj*, align 8
%argslist60732$k484972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49909, %struct.ScmObj* %argslist60732$k484971)
store volatile %struct.ScmObj* %argslist60732$k484972, %struct.ScmObj** %stackaddr$prim62649, align 8
%clofunc62650 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48497)
musttail call tailcc void %clofunc62650(%struct.ScmObj* %k48497, %struct.ScmObj* %argslist60732$k484972)
ret void
}

define tailcc void @proc_clo$ae49740(%struct.ScmObj* %env$ae49740,%struct.ScmObj* %args4808148499) {
%stackaddr$env-ref62651 = alloca %struct.ScmObj*, align 8
%_37drop_45right48073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49740, i64 0)
store %struct.ScmObj* %_37drop_45right48073, %struct.ScmObj** %stackaddr$env-ref62651
%stackaddr$env-ref62652 = alloca %struct.ScmObj*, align 8
%_37last48076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49740, i64 1)
store %struct.ScmObj* %_37last48076, %struct.ScmObj** %stackaddr$env-ref62652
%stackaddr$env-ref62653 = alloca %struct.ScmObj*, align 8
%_37foldr48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49740, i64 2)
store %struct.ScmObj* %_37foldr48059, %struct.ScmObj** %stackaddr$env-ref62653
%stackaddr$prim62654 = alloca %struct.ScmObj*, align 8
%k48500 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4808148499)
store volatile %struct.ScmObj* %k48500, %struct.ScmObj** %stackaddr$prim62654, align 8
%stackaddr$prim62655 = alloca %struct.ScmObj*, align 8
%args48081 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4808148499)
store volatile %struct.ScmObj* %args48081, %struct.ScmObj** %stackaddr$prim62655, align 8
%stackaddr$prim62656 = alloca %struct.ScmObj*, align 8
%f48083 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48081)
store volatile %struct.ScmObj* %f48083, %struct.ScmObj** %stackaddr$prim62656, align 8
%stackaddr$prim62657 = alloca %struct.ScmObj*, align 8
%lsts48082 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48081)
store volatile %struct.ScmObj* %lsts48082, %struct.ScmObj** %stackaddr$prim62657, align 8
%stackaddr$makeclosure62658 = alloca %struct.ScmObj*, align 8
%fptrToInt62659 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49745 to i64
%ae49745 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt62659)
store volatile %struct.ScmObj* %ae49745, %struct.ScmObj** %stackaddr$makeclosure62658, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49745, %struct.ScmObj* %k48500, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49745, %struct.ScmObj* %lsts48082, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49745, %struct.ScmObj* %_37foldr48059, i64 2)
%ae49746 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure62660 = alloca %struct.ScmObj*, align 8
%fptrToInt62661 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49747 to i64
%ae49747 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt62661)
store volatile %struct.ScmObj* %ae49747, %struct.ScmObj** %stackaddr$makeclosure62660, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49747, %struct.ScmObj* %_37drop_45right48073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49747, %struct.ScmObj* %f48083, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49747, %struct.ScmObj* %_37last48076, i64 2)
%argslist60751$ae497450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62662 = alloca %struct.ScmObj*, align 8
%argslist60751$ae497451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49747, %struct.ScmObj* %argslist60751$ae497450)
store volatile %struct.ScmObj* %argslist60751$ae497451, %struct.ScmObj** %stackaddr$prim62662, align 8
%stackaddr$prim62663 = alloca %struct.ScmObj*, align 8
%argslist60751$ae497452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49746, %struct.ScmObj* %argslist60751$ae497451)
store volatile %struct.ScmObj* %argslist60751$ae497452, %struct.ScmObj** %stackaddr$prim62663, align 8
%clofunc62664 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49745)
musttail call tailcc void %clofunc62664(%struct.ScmObj* %ae49745, %struct.ScmObj* %argslist60751$ae497452)
ret void
}

define tailcc void @proc_clo$ae49745(%struct.ScmObj* %env$ae49745,%struct.ScmObj* %current_45args60736) {
%stackaddr$env-ref62665 = alloca %struct.ScmObj*, align 8
%k48500 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49745, i64 0)
store %struct.ScmObj* %k48500, %struct.ScmObj** %stackaddr$env-ref62665
%stackaddr$env-ref62666 = alloca %struct.ScmObj*, align 8
%lsts48082 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49745, i64 1)
store %struct.ScmObj* %lsts48082, %struct.ScmObj** %stackaddr$env-ref62666
%stackaddr$env-ref62667 = alloca %struct.ScmObj*, align 8
%_37foldr48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49745, i64 2)
store %struct.ScmObj* %_37foldr48059, %struct.ScmObj** %stackaddr$env-ref62667
%stackaddr$prim62668 = alloca %struct.ScmObj*, align 8
%_95k48501 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60736)
store volatile %struct.ScmObj* %_95k48501, %struct.ScmObj** %stackaddr$prim62668, align 8
%stackaddr$prim62669 = alloca %struct.ScmObj*, align 8
%current_45args60737 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60736)
store volatile %struct.ScmObj* %current_45args60737, %struct.ScmObj** %stackaddr$prim62669, align 8
%stackaddr$prim62670 = alloca %struct.ScmObj*, align 8
%anf_45bind48234 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60737)
store volatile %struct.ScmObj* %anf_45bind48234, %struct.ScmObj** %stackaddr$prim62670, align 8
%ae49808 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62671 = alloca %struct.ScmObj*, align 8
%anf_45bind48235 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49808, %struct.ScmObj* %lsts48082)
store volatile %struct.ScmObj* %anf_45bind48235, %struct.ScmObj** %stackaddr$prim62671, align 8
%stackaddr$prim62672 = alloca %struct.ScmObj*, align 8
%anf_45bind48236 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48234, %struct.ScmObj* %anf_45bind48235)
store volatile %struct.ScmObj* %anf_45bind48236, %struct.ScmObj** %stackaddr$prim62672, align 8
%stackaddr$prim62673 = alloca %struct.ScmObj*, align 8
%cpsargs48502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48500, %struct.ScmObj* %anf_45bind48236)
store volatile %struct.ScmObj* %cpsargs48502, %struct.ScmObj** %stackaddr$prim62673, align 8
%clofunc62674 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr48059)
musttail call tailcc void %clofunc62674(%struct.ScmObj* %_37foldr48059, %struct.ScmObj* %cpsargs48502)
ret void
}

define tailcc void @proc_clo$ae49747(%struct.ScmObj* %env$ae49747,%struct.ScmObj* %fargs4808448503) {
%stackaddr$env-ref62675 = alloca %struct.ScmObj*, align 8
%_37drop_45right48073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49747, i64 0)
store %struct.ScmObj* %_37drop_45right48073, %struct.ScmObj** %stackaddr$env-ref62675
%stackaddr$env-ref62676 = alloca %struct.ScmObj*, align 8
%f48083 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49747, i64 1)
store %struct.ScmObj* %f48083, %struct.ScmObj** %stackaddr$env-ref62676
%stackaddr$env-ref62677 = alloca %struct.ScmObj*, align 8
%_37last48076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49747, i64 2)
store %struct.ScmObj* %_37last48076, %struct.ScmObj** %stackaddr$env-ref62677
%stackaddr$prim62678 = alloca %struct.ScmObj*, align 8
%k48504 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs4808448503)
store volatile %struct.ScmObj* %k48504, %struct.ScmObj** %stackaddr$prim62678, align 8
%stackaddr$prim62679 = alloca %struct.ScmObj*, align 8
%fargs48084 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs4808448503)
store volatile %struct.ScmObj* %fargs48084, %struct.ScmObj** %stackaddr$prim62679, align 8
%stackaddr$makeclosure62680 = alloca %struct.ScmObj*, align 8
%fptrToInt62681 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49751 to i64
%ae49751 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt62681)
store volatile %struct.ScmObj* %ae49751, %struct.ScmObj** %stackaddr$makeclosure62680, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49751, %struct.ScmObj* %k48504, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49751, %struct.ScmObj* %fargs48084, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49751, %struct.ScmObj* %f48083, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49751, %struct.ScmObj* %_37last48076, i64 3)
%ae49753 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist60750$_37drop_45right480730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62682 = alloca %struct.ScmObj*, align 8
%argslist60750$_37drop_45right480731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49753, %struct.ScmObj* %argslist60750$_37drop_45right480730)
store volatile %struct.ScmObj* %argslist60750$_37drop_45right480731, %struct.ScmObj** %stackaddr$prim62682, align 8
%stackaddr$prim62683 = alloca %struct.ScmObj*, align 8
%argslist60750$_37drop_45right480732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs48084, %struct.ScmObj* %argslist60750$_37drop_45right480731)
store volatile %struct.ScmObj* %argslist60750$_37drop_45right480732, %struct.ScmObj** %stackaddr$prim62683, align 8
%stackaddr$prim62684 = alloca %struct.ScmObj*, align 8
%argslist60750$_37drop_45right480733 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49751, %struct.ScmObj* %argslist60750$_37drop_45right480732)
store volatile %struct.ScmObj* %argslist60750$_37drop_45right480733, %struct.ScmObj** %stackaddr$prim62684, align 8
%clofunc62685 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right48073)
musttail call tailcc void %clofunc62685(%struct.ScmObj* %_37drop_45right48073, %struct.ScmObj* %argslist60750$_37drop_45right480733)
ret void
}

define tailcc void @proc_clo$ae49751(%struct.ScmObj* %env$ae49751,%struct.ScmObj* %current_45args60739) {
%stackaddr$env-ref62686 = alloca %struct.ScmObj*, align 8
%k48504 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49751, i64 0)
store %struct.ScmObj* %k48504, %struct.ScmObj** %stackaddr$env-ref62686
%stackaddr$env-ref62687 = alloca %struct.ScmObj*, align 8
%fargs48084 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49751, i64 1)
store %struct.ScmObj* %fargs48084, %struct.ScmObj** %stackaddr$env-ref62687
%stackaddr$env-ref62688 = alloca %struct.ScmObj*, align 8
%f48083 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49751, i64 2)
store %struct.ScmObj* %f48083, %struct.ScmObj** %stackaddr$env-ref62688
%stackaddr$env-ref62689 = alloca %struct.ScmObj*, align 8
%_37last48076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49751, i64 3)
store %struct.ScmObj* %_37last48076, %struct.ScmObj** %stackaddr$env-ref62689
%stackaddr$prim62690 = alloca %struct.ScmObj*, align 8
%_95k48505 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60739)
store volatile %struct.ScmObj* %_95k48505, %struct.ScmObj** %stackaddr$prim62690, align 8
%stackaddr$prim62691 = alloca %struct.ScmObj*, align 8
%current_45args60740 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60739)
store volatile %struct.ScmObj* %current_45args60740, %struct.ScmObj** %stackaddr$prim62691, align 8
%stackaddr$prim62692 = alloca %struct.ScmObj*, align 8
%anf_45bind48231 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60740)
store volatile %struct.ScmObj* %anf_45bind48231, %struct.ScmObj** %stackaddr$prim62692, align 8
%stackaddr$makeclosure62693 = alloca %struct.ScmObj*, align 8
%fptrToInt62694 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49758 to i64
%ae49758 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt62694)
store volatile %struct.ScmObj* %ae49758, %struct.ScmObj** %stackaddr$makeclosure62693, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49758, %struct.ScmObj* %k48504, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49758, %struct.ScmObj* %fargs48084, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49758, %struct.ScmObj* %_37last48076, i64 2)
%stackaddr$prim62695 = alloca %struct.ScmObj*, align 8
%cpsargs48509 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49758, %struct.ScmObj* %anf_45bind48231)
store volatile %struct.ScmObj* %cpsargs48509, %struct.ScmObj** %stackaddr$prim62695, align 8
%clofunc62696 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48083)
musttail call tailcc void %clofunc62696(%struct.ScmObj* %f48083, %struct.ScmObj* %cpsargs48509)
ret void
}

define tailcc void @proc_clo$ae49758(%struct.ScmObj* %env$ae49758,%struct.ScmObj* %current_45args60742) {
%stackaddr$env-ref62697 = alloca %struct.ScmObj*, align 8
%k48504 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49758, i64 0)
store %struct.ScmObj* %k48504, %struct.ScmObj** %stackaddr$env-ref62697
%stackaddr$env-ref62698 = alloca %struct.ScmObj*, align 8
%fargs48084 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49758, i64 1)
store %struct.ScmObj* %fargs48084, %struct.ScmObj** %stackaddr$env-ref62698
%stackaddr$env-ref62699 = alloca %struct.ScmObj*, align 8
%_37last48076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49758, i64 2)
store %struct.ScmObj* %_37last48076, %struct.ScmObj** %stackaddr$env-ref62699
%stackaddr$prim62700 = alloca %struct.ScmObj*, align 8
%_95k48506 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60742)
store volatile %struct.ScmObj* %_95k48506, %struct.ScmObj** %stackaddr$prim62700, align 8
%stackaddr$prim62701 = alloca %struct.ScmObj*, align 8
%current_45args60743 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60742)
store volatile %struct.ScmObj* %current_45args60743, %struct.ScmObj** %stackaddr$prim62701, align 8
%stackaddr$prim62702 = alloca %struct.ScmObj*, align 8
%anf_45bind48232 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60743)
store volatile %struct.ScmObj* %anf_45bind48232, %struct.ScmObj** %stackaddr$prim62702, align 8
%stackaddr$makeclosure62703 = alloca %struct.ScmObj*, align 8
%fptrToInt62704 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49763 to i64
%ae49763 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt62704)
store volatile %struct.ScmObj* %ae49763, %struct.ScmObj** %stackaddr$makeclosure62703, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49763, %struct.ScmObj* %k48504, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49763, %struct.ScmObj* %anf_45bind48232, i64 1)
%argslist60749$_37last480760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62705 = alloca %struct.ScmObj*, align 8
%argslist60749$_37last480761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs48084, %struct.ScmObj* %argslist60749$_37last480760)
store volatile %struct.ScmObj* %argslist60749$_37last480761, %struct.ScmObj** %stackaddr$prim62705, align 8
%stackaddr$prim62706 = alloca %struct.ScmObj*, align 8
%argslist60749$_37last480762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49763, %struct.ScmObj* %argslist60749$_37last480761)
store volatile %struct.ScmObj* %argslist60749$_37last480762, %struct.ScmObj** %stackaddr$prim62706, align 8
%clofunc62707 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last48076)
musttail call tailcc void %clofunc62707(%struct.ScmObj* %_37last48076, %struct.ScmObj* %argslist60749$_37last480762)
ret void
}

define tailcc void @proc_clo$ae49763(%struct.ScmObj* %env$ae49763,%struct.ScmObj* %current_45args60745) {
%stackaddr$env-ref62708 = alloca %struct.ScmObj*, align 8
%k48504 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49763, i64 0)
store %struct.ScmObj* %k48504, %struct.ScmObj** %stackaddr$env-ref62708
%stackaddr$env-ref62709 = alloca %struct.ScmObj*, align 8
%anf_45bind48232 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49763, i64 1)
store %struct.ScmObj* %anf_45bind48232, %struct.ScmObj** %stackaddr$env-ref62709
%stackaddr$prim62710 = alloca %struct.ScmObj*, align 8
%_95k48507 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60745)
store volatile %struct.ScmObj* %_95k48507, %struct.ScmObj** %stackaddr$prim62710, align 8
%stackaddr$prim62711 = alloca %struct.ScmObj*, align 8
%current_45args60746 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60745)
store volatile %struct.ScmObj* %current_45args60746, %struct.ScmObj** %stackaddr$prim62711, align 8
%stackaddr$prim62712 = alloca %struct.ScmObj*, align 8
%anf_45bind48233 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60746)
store volatile %struct.ScmObj* %anf_45bind48233, %struct.ScmObj** %stackaddr$prim62712, align 8
%stackaddr$prim62713 = alloca %struct.ScmObj*, align 8
%cpsprim48508 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48232, %struct.ScmObj* %anf_45bind48233)
store volatile %struct.ScmObj* %cpsprim48508, %struct.ScmObj** %stackaddr$prim62713, align 8
%ae49768 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist60748$k485040 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62714 = alloca %struct.ScmObj*, align 8
%argslist60748$k485041 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48508, %struct.ScmObj* %argslist60748$k485040)
store volatile %struct.ScmObj* %argslist60748$k485041, %struct.ScmObj** %stackaddr$prim62714, align 8
%stackaddr$prim62715 = alloca %struct.ScmObj*, align 8
%argslist60748$k485042 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49768, %struct.ScmObj* %argslist60748$k485041)
store volatile %struct.ScmObj* %argslist60748$k485042, %struct.ScmObj** %stackaddr$prim62715, align 8
%clofunc62716 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48504)
musttail call tailcc void %clofunc62716(%struct.ScmObj* %k48504, %struct.ScmObj* %argslist60748$k485042)
ret void
}

define tailcc void @proc_clo$ae49663(%struct.ScmObj* %env$ae49663,%struct.ScmObj* %current_45args60753) {
%stackaddr$env-ref62717 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49663, i64 0)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref62717
%stackaddr$prim62718 = alloca %struct.ScmObj*, align 8
%k48510 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60753)
store volatile %struct.ScmObj* %k48510, %struct.ScmObj** %stackaddr$prim62718, align 8
%stackaddr$prim62719 = alloca %struct.ScmObj*, align 8
%current_45args60754 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60753)
store volatile %struct.ScmObj* %current_45args60754, %struct.ScmObj** %stackaddr$prim62719, align 8
%stackaddr$prim62720 = alloca %struct.ScmObj*, align 8
%f48087 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60754)
store volatile %struct.ScmObj* %f48087, %struct.ScmObj** %stackaddr$prim62720, align 8
%stackaddr$prim62721 = alloca %struct.ScmObj*, align 8
%current_45args60755 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60754)
store volatile %struct.ScmObj* %current_45args60755, %struct.ScmObj** %stackaddr$prim62721, align 8
%stackaddr$prim62722 = alloca %struct.ScmObj*, align 8
%lst48086 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60755)
store volatile %struct.ScmObj* %lst48086, %struct.ScmObj** %stackaddr$prim62722, align 8
%stackaddr$makeclosure62723 = alloca %struct.ScmObj*, align 8
%fptrToInt62724 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49664 to i64
%ae49664 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt62724)
store volatile %struct.ScmObj* %ae49664, %struct.ScmObj** %stackaddr$makeclosure62723, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49664, %struct.ScmObj* %lst48086, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49664, %struct.ScmObj* %_37foldr148054, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49664, %struct.ScmObj* %k48510, i64 2)
%ae49665 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure62725 = alloca %struct.ScmObj*, align 8
%fptrToInt62726 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49666 to i64
%ae49666 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt62726)
store volatile %struct.ScmObj* %ae49666, %struct.ScmObj** %stackaddr$makeclosure62725, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49666, %struct.ScmObj* %f48087, i64 0)
%argslist60770$ae496640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62727 = alloca %struct.ScmObj*, align 8
%argslist60770$ae496641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49666, %struct.ScmObj* %argslist60770$ae496640)
store volatile %struct.ScmObj* %argslist60770$ae496641, %struct.ScmObj** %stackaddr$prim62727, align 8
%stackaddr$prim62728 = alloca %struct.ScmObj*, align 8
%argslist60770$ae496642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49665, %struct.ScmObj* %argslist60770$ae496641)
store volatile %struct.ScmObj* %argslist60770$ae496642, %struct.ScmObj** %stackaddr$prim62728, align 8
%clofunc62729 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49664)
musttail call tailcc void %clofunc62729(%struct.ScmObj* %ae49664, %struct.ScmObj* %argslist60770$ae496642)
ret void
}

define tailcc void @proc_clo$ae49664(%struct.ScmObj* %env$ae49664,%struct.ScmObj* %current_45args60757) {
%stackaddr$env-ref62730 = alloca %struct.ScmObj*, align 8
%lst48086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49664, i64 0)
store %struct.ScmObj* %lst48086, %struct.ScmObj** %stackaddr$env-ref62730
%stackaddr$env-ref62731 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49664, i64 1)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref62731
%stackaddr$env-ref62732 = alloca %struct.ScmObj*, align 8
%k48510 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49664, i64 2)
store %struct.ScmObj* %k48510, %struct.ScmObj** %stackaddr$env-ref62732
%stackaddr$prim62733 = alloca %struct.ScmObj*, align 8
%_95k48511 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60757)
store volatile %struct.ScmObj* %_95k48511, %struct.ScmObj** %stackaddr$prim62733, align 8
%stackaddr$prim62734 = alloca %struct.ScmObj*, align 8
%current_45args60758 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60757)
store volatile %struct.ScmObj* %current_45args60758, %struct.ScmObj** %stackaddr$prim62734, align 8
%stackaddr$prim62735 = alloca %struct.ScmObj*, align 8
%anf_45bind48230 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60758)
store volatile %struct.ScmObj* %anf_45bind48230, %struct.ScmObj** %stackaddr$prim62735, align 8
%ae49698 = call %struct.ScmObj* @const_init_null()
%argslist60760$_37foldr1480540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62736 = alloca %struct.ScmObj*, align 8
%argslist60760$_37foldr1480541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48086, %struct.ScmObj* %argslist60760$_37foldr1480540)
store volatile %struct.ScmObj* %argslist60760$_37foldr1480541, %struct.ScmObj** %stackaddr$prim62736, align 8
%stackaddr$prim62737 = alloca %struct.ScmObj*, align 8
%argslist60760$_37foldr1480542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49698, %struct.ScmObj* %argslist60760$_37foldr1480541)
store volatile %struct.ScmObj* %argslist60760$_37foldr1480542, %struct.ScmObj** %stackaddr$prim62737, align 8
%stackaddr$prim62738 = alloca %struct.ScmObj*, align 8
%argslist60760$_37foldr1480543 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48230, %struct.ScmObj* %argslist60760$_37foldr1480542)
store volatile %struct.ScmObj* %argslist60760$_37foldr1480543, %struct.ScmObj** %stackaddr$prim62738, align 8
%stackaddr$prim62739 = alloca %struct.ScmObj*, align 8
%argslist60760$_37foldr1480544 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48510, %struct.ScmObj* %argslist60760$_37foldr1480543)
store volatile %struct.ScmObj* %argslist60760$_37foldr1480544, %struct.ScmObj** %stackaddr$prim62739, align 8
%clofunc62740 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148054)
musttail call tailcc void %clofunc62740(%struct.ScmObj* %_37foldr148054, %struct.ScmObj* %argslist60760$_37foldr1480544)
ret void
}

define tailcc void @proc_clo$ae49666(%struct.ScmObj* %env$ae49666,%struct.ScmObj* %current_45args60761) {
%stackaddr$env-ref62741 = alloca %struct.ScmObj*, align 8
%f48087 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49666, i64 0)
store %struct.ScmObj* %f48087, %struct.ScmObj** %stackaddr$env-ref62741
%stackaddr$prim62742 = alloca %struct.ScmObj*, align 8
%k48512 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60761)
store volatile %struct.ScmObj* %k48512, %struct.ScmObj** %stackaddr$prim62742, align 8
%stackaddr$prim62743 = alloca %struct.ScmObj*, align 8
%current_45args60762 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60761)
store volatile %struct.ScmObj* %current_45args60762, %struct.ScmObj** %stackaddr$prim62743, align 8
%stackaddr$prim62744 = alloca %struct.ScmObj*, align 8
%v48089 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60762)
store volatile %struct.ScmObj* %v48089, %struct.ScmObj** %stackaddr$prim62744, align 8
%stackaddr$prim62745 = alloca %struct.ScmObj*, align 8
%current_45args60763 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60762)
store volatile %struct.ScmObj* %current_45args60763, %struct.ScmObj** %stackaddr$prim62745, align 8
%stackaddr$prim62746 = alloca %struct.ScmObj*, align 8
%r48088 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60763)
store volatile %struct.ScmObj* %r48088, %struct.ScmObj** %stackaddr$prim62746, align 8
%stackaddr$makeclosure62747 = alloca %struct.ScmObj*, align 8
%fptrToInt62748 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49668 to i64
%ae49668 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt62748)
store volatile %struct.ScmObj* %ae49668, %struct.ScmObj** %stackaddr$makeclosure62747, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49668, %struct.ScmObj* %r48088, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49668, %struct.ScmObj* %k48512, i64 1)
%argslist60769$f480870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62749 = alloca %struct.ScmObj*, align 8
%argslist60769$f480871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v48089, %struct.ScmObj* %argslist60769$f480870)
store volatile %struct.ScmObj* %argslist60769$f480871, %struct.ScmObj** %stackaddr$prim62749, align 8
%stackaddr$prim62750 = alloca %struct.ScmObj*, align 8
%argslist60769$f480872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49668, %struct.ScmObj* %argslist60769$f480871)
store volatile %struct.ScmObj* %argslist60769$f480872, %struct.ScmObj** %stackaddr$prim62750, align 8
%clofunc62751 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48087)
musttail call tailcc void %clofunc62751(%struct.ScmObj* %f48087, %struct.ScmObj* %argslist60769$f480872)
ret void
}

define tailcc void @proc_clo$ae49668(%struct.ScmObj* %env$ae49668,%struct.ScmObj* %current_45args60765) {
%stackaddr$env-ref62752 = alloca %struct.ScmObj*, align 8
%r48088 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49668, i64 0)
store %struct.ScmObj* %r48088, %struct.ScmObj** %stackaddr$env-ref62752
%stackaddr$env-ref62753 = alloca %struct.ScmObj*, align 8
%k48512 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49668, i64 1)
store %struct.ScmObj* %k48512, %struct.ScmObj** %stackaddr$env-ref62753
%stackaddr$prim62754 = alloca %struct.ScmObj*, align 8
%_95k48513 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60765)
store volatile %struct.ScmObj* %_95k48513, %struct.ScmObj** %stackaddr$prim62754, align 8
%stackaddr$prim62755 = alloca %struct.ScmObj*, align 8
%current_45args60766 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60765)
store volatile %struct.ScmObj* %current_45args60766, %struct.ScmObj** %stackaddr$prim62755, align 8
%stackaddr$prim62756 = alloca %struct.ScmObj*, align 8
%anf_45bind48229 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60766)
store volatile %struct.ScmObj* %anf_45bind48229, %struct.ScmObj** %stackaddr$prim62756, align 8
%stackaddr$prim62757 = alloca %struct.ScmObj*, align 8
%cpsprim48514 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48229, %struct.ScmObj* %r48088)
store volatile %struct.ScmObj* %cpsprim48514, %struct.ScmObj** %stackaddr$prim62757, align 8
%ae49673 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist60768$k485120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62758 = alloca %struct.ScmObj*, align 8
%argslist60768$k485121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48514, %struct.ScmObj* %argslist60768$k485120)
store volatile %struct.ScmObj* %argslist60768$k485121, %struct.ScmObj** %stackaddr$prim62758, align 8
%stackaddr$prim62759 = alloca %struct.ScmObj*, align 8
%argslist60768$k485122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49673, %struct.ScmObj* %argslist60768$k485121)
store volatile %struct.ScmObj* %argslist60768$k485122, %struct.ScmObj** %stackaddr$prim62759, align 8
%clofunc62760 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48512)
musttail call tailcc void %clofunc62760(%struct.ScmObj* %k48512, %struct.ScmObj* %argslist60768$k485122)
ret void
}

define tailcc void @proc_clo$ae49277(%struct.ScmObj* %env$ae49277,%struct.ScmObj* %current_45args60773) {
%stackaddr$env-ref62761 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49277, i64 0)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref62761
%stackaddr$env-ref62762 = alloca %struct.ScmObj*, align 8
%_37map148050 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49277, i64 1)
store %struct.ScmObj* %_37map148050, %struct.ScmObj** %stackaddr$env-ref62762
%stackaddr$prim62763 = alloca %struct.ScmObj*, align 8
%k48515 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60773)
store volatile %struct.ScmObj* %k48515, %struct.ScmObj** %stackaddr$prim62763, align 8
%stackaddr$prim62764 = alloca %struct.ScmObj*, align 8
%current_45args60774 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60773)
store volatile %struct.ScmObj* %current_45args60774, %struct.ScmObj** %stackaddr$prim62764, align 8
%stackaddr$prim62765 = alloca %struct.ScmObj*, align 8
%_37foldr48060 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60774)
store volatile %struct.ScmObj* %_37foldr48060, %struct.ScmObj** %stackaddr$prim62765, align 8
%ae49279 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure62766 = alloca %struct.ScmObj*, align 8
%fptrToInt62767 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49280 to i64
%ae49280 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt62767)
store volatile %struct.ScmObj* %ae49280, %struct.ScmObj** %stackaddr$makeclosure62766, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49280, %struct.ScmObj* %_37foldr148054, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49280, %struct.ScmObj* %_37map148050, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49280, %struct.ScmObj* %_37foldr48060, i64 2)
%argslist60831$k485150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62768 = alloca %struct.ScmObj*, align 8
%argslist60831$k485151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49280, %struct.ScmObj* %argslist60831$k485150)
store volatile %struct.ScmObj* %argslist60831$k485151, %struct.ScmObj** %stackaddr$prim62768, align 8
%stackaddr$prim62769 = alloca %struct.ScmObj*, align 8
%argslist60831$k485152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49279, %struct.ScmObj* %argslist60831$k485151)
store volatile %struct.ScmObj* %argslist60831$k485152, %struct.ScmObj** %stackaddr$prim62769, align 8
%clofunc62770 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48515)
musttail call tailcc void %clofunc62770(%struct.ScmObj* %k48515, %struct.ScmObj* %argslist60831$k485152)
ret void
}

define tailcc void @proc_clo$ae49280(%struct.ScmObj* %env$ae49280,%struct.ScmObj* %args4806148516) {
%stackaddr$env-ref62771 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49280, i64 0)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref62771
%stackaddr$env-ref62772 = alloca %struct.ScmObj*, align 8
%_37map148050 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49280, i64 1)
store %struct.ScmObj* %_37map148050, %struct.ScmObj** %stackaddr$env-ref62772
%stackaddr$env-ref62773 = alloca %struct.ScmObj*, align 8
%_37foldr48060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49280, i64 2)
store %struct.ScmObj* %_37foldr48060, %struct.ScmObj** %stackaddr$env-ref62773
%stackaddr$prim62774 = alloca %struct.ScmObj*, align 8
%k48517 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4806148516)
store volatile %struct.ScmObj* %k48517, %struct.ScmObj** %stackaddr$prim62774, align 8
%stackaddr$prim62775 = alloca %struct.ScmObj*, align 8
%args48061 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4806148516)
store volatile %struct.ScmObj* %args48061, %struct.ScmObj** %stackaddr$prim62775, align 8
%stackaddr$prim62776 = alloca %struct.ScmObj*, align 8
%f48064 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48061)
store volatile %struct.ScmObj* %f48064, %struct.ScmObj** %stackaddr$prim62776, align 8
%stackaddr$prim62777 = alloca %struct.ScmObj*, align 8
%anf_45bind48216 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48061)
store volatile %struct.ScmObj* %anf_45bind48216, %struct.ScmObj** %stackaddr$prim62777, align 8
%stackaddr$prim62778 = alloca %struct.ScmObj*, align 8
%acc48063 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48216)
store volatile %struct.ScmObj* %acc48063, %struct.ScmObj** %stackaddr$prim62778, align 8
%stackaddr$prim62779 = alloca %struct.ScmObj*, align 8
%anf_45bind48217 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48061)
store volatile %struct.ScmObj* %anf_45bind48217, %struct.ScmObj** %stackaddr$prim62779, align 8
%stackaddr$prim62780 = alloca %struct.ScmObj*, align 8
%lsts48062 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48217)
store volatile %struct.ScmObj* %lsts48062, %struct.ScmObj** %stackaddr$prim62780, align 8
%stackaddr$makeclosure62781 = alloca %struct.ScmObj*, align 8
%fptrToInt62782 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49288 to i64
%ae49288 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt62782)
store volatile %struct.ScmObj* %ae49288, %struct.ScmObj** %stackaddr$makeclosure62781, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49288, %struct.ScmObj* %_37foldr148054, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49288, %struct.ScmObj* %k48517, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49288, %struct.ScmObj* %_37map148050, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49288, %struct.ScmObj* %f48064, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49288, %struct.ScmObj* %acc48063, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49288, %struct.ScmObj* %lsts48062, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49288, %struct.ScmObj* %_37foldr48060, i64 6)
%ae49289 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure62783 = alloca %struct.ScmObj*, align 8
%fptrToInt62784 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49290 to i64
%ae49290 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt62784)
store volatile %struct.ScmObj* %ae49290, %struct.ScmObj** %stackaddr$makeclosure62783, align 8
%argslist60830$ae492880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62785 = alloca %struct.ScmObj*, align 8
%argslist60830$ae492881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49290, %struct.ScmObj* %argslist60830$ae492880)
store volatile %struct.ScmObj* %argslist60830$ae492881, %struct.ScmObj** %stackaddr$prim62785, align 8
%stackaddr$prim62786 = alloca %struct.ScmObj*, align 8
%argslist60830$ae492882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49289, %struct.ScmObj* %argslist60830$ae492881)
store volatile %struct.ScmObj* %argslist60830$ae492882, %struct.ScmObj** %stackaddr$prim62786, align 8
%clofunc62787 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49288)
musttail call tailcc void %clofunc62787(%struct.ScmObj* %ae49288, %struct.ScmObj* %argslist60830$ae492882)
ret void
}

define tailcc void @proc_clo$ae49288(%struct.ScmObj* %env$ae49288,%struct.ScmObj* %current_45args60776) {
%stackaddr$env-ref62788 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49288, i64 0)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref62788
%stackaddr$env-ref62789 = alloca %struct.ScmObj*, align 8
%k48517 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49288, i64 1)
store %struct.ScmObj* %k48517, %struct.ScmObj** %stackaddr$env-ref62789
%stackaddr$env-ref62790 = alloca %struct.ScmObj*, align 8
%_37map148050 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49288, i64 2)
store %struct.ScmObj* %_37map148050, %struct.ScmObj** %stackaddr$env-ref62790
%stackaddr$env-ref62791 = alloca %struct.ScmObj*, align 8
%f48064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49288, i64 3)
store %struct.ScmObj* %f48064, %struct.ScmObj** %stackaddr$env-ref62791
%stackaddr$env-ref62792 = alloca %struct.ScmObj*, align 8
%acc48063 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49288, i64 4)
store %struct.ScmObj* %acc48063, %struct.ScmObj** %stackaddr$env-ref62792
%stackaddr$env-ref62793 = alloca %struct.ScmObj*, align 8
%lsts48062 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49288, i64 5)
store %struct.ScmObj* %lsts48062, %struct.ScmObj** %stackaddr$env-ref62793
%stackaddr$env-ref62794 = alloca %struct.ScmObj*, align 8
%_37foldr48060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49288, i64 6)
store %struct.ScmObj* %_37foldr48060, %struct.ScmObj** %stackaddr$env-ref62794
%stackaddr$prim62795 = alloca %struct.ScmObj*, align 8
%_95k48518 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60776)
store volatile %struct.ScmObj* %_95k48518, %struct.ScmObj** %stackaddr$prim62795, align 8
%stackaddr$prim62796 = alloca %struct.ScmObj*, align 8
%current_45args60777 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60776)
store volatile %struct.ScmObj* %current_45args60777, %struct.ScmObj** %stackaddr$prim62796, align 8
%stackaddr$prim62797 = alloca %struct.ScmObj*, align 8
%anf_45bind48218 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60777)
store volatile %struct.ScmObj* %anf_45bind48218, %struct.ScmObj** %stackaddr$prim62797, align 8
%stackaddr$makeclosure62798 = alloca %struct.ScmObj*, align 8
%fptrToInt62799 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49320 to i64
%ae49320 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt62799)
store volatile %struct.ScmObj* %ae49320, %struct.ScmObj** %stackaddr$makeclosure62798, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49320, %struct.ScmObj* %_37foldr148054, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49320, %struct.ScmObj* %k48517, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49320, %struct.ScmObj* %_37map148050, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49320, %struct.ScmObj* %f48064, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49320, %struct.ScmObj* %acc48063, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49320, %struct.ScmObj* %lsts48062, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49320, %struct.ScmObj* %_37foldr48060, i64 6)
%ae49322 = call %struct.ScmObj* @const_init_false()
%argslist60823$_37foldr1480540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62800 = alloca %struct.ScmObj*, align 8
%argslist60823$_37foldr1480541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48062, %struct.ScmObj* %argslist60823$_37foldr1480540)
store volatile %struct.ScmObj* %argslist60823$_37foldr1480541, %struct.ScmObj** %stackaddr$prim62800, align 8
%stackaddr$prim62801 = alloca %struct.ScmObj*, align 8
%argslist60823$_37foldr1480542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49322, %struct.ScmObj* %argslist60823$_37foldr1480541)
store volatile %struct.ScmObj* %argslist60823$_37foldr1480542, %struct.ScmObj** %stackaddr$prim62801, align 8
%stackaddr$prim62802 = alloca %struct.ScmObj*, align 8
%argslist60823$_37foldr1480543 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48218, %struct.ScmObj* %argslist60823$_37foldr1480542)
store volatile %struct.ScmObj* %argslist60823$_37foldr1480543, %struct.ScmObj** %stackaddr$prim62802, align 8
%stackaddr$prim62803 = alloca %struct.ScmObj*, align 8
%argslist60823$_37foldr1480544 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49320, %struct.ScmObj* %argslist60823$_37foldr1480543)
store volatile %struct.ScmObj* %argslist60823$_37foldr1480544, %struct.ScmObj** %stackaddr$prim62803, align 8
%clofunc62804 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148054)
musttail call tailcc void %clofunc62804(%struct.ScmObj* %_37foldr148054, %struct.ScmObj* %argslist60823$_37foldr1480544)
ret void
}

define tailcc void @proc_clo$ae49320(%struct.ScmObj* %env$ae49320,%struct.ScmObj* %current_45args60779) {
%stackaddr$env-ref62805 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49320, i64 0)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref62805
%stackaddr$env-ref62806 = alloca %struct.ScmObj*, align 8
%k48517 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49320, i64 1)
store %struct.ScmObj* %k48517, %struct.ScmObj** %stackaddr$env-ref62806
%stackaddr$env-ref62807 = alloca %struct.ScmObj*, align 8
%_37map148050 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49320, i64 2)
store %struct.ScmObj* %_37map148050, %struct.ScmObj** %stackaddr$env-ref62807
%stackaddr$env-ref62808 = alloca %struct.ScmObj*, align 8
%f48064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49320, i64 3)
store %struct.ScmObj* %f48064, %struct.ScmObj** %stackaddr$env-ref62808
%stackaddr$env-ref62809 = alloca %struct.ScmObj*, align 8
%acc48063 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49320, i64 4)
store %struct.ScmObj* %acc48063, %struct.ScmObj** %stackaddr$env-ref62809
%stackaddr$env-ref62810 = alloca %struct.ScmObj*, align 8
%lsts48062 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49320, i64 5)
store %struct.ScmObj* %lsts48062, %struct.ScmObj** %stackaddr$env-ref62810
%stackaddr$env-ref62811 = alloca %struct.ScmObj*, align 8
%_37foldr48060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49320, i64 6)
store %struct.ScmObj* %_37foldr48060, %struct.ScmObj** %stackaddr$env-ref62811
%stackaddr$prim62812 = alloca %struct.ScmObj*, align 8
%_95k48519 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60779)
store volatile %struct.ScmObj* %_95k48519, %struct.ScmObj** %stackaddr$prim62812, align 8
%stackaddr$prim62813 = alloca %struct.ScmObj*, align 8
%current_45args60780 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60779)
store volatile %struct.ScmObj* %current_45args60780, %struct.ScmObj** %stackaddr$prim62813, align 8
%stackaddr$prim62814 = alloca %struct.ScmObj*, align 8
%anf_45bind48219 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60780)
store volatile %struct.ScmObj* %anf_45bind48219, %struct.ScmObj** %stackaddr$prim62814, align 8
%truthy$cmp62815 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48219)
%cmp$cmp62815 = icmp eq i64 %truthy$cmp62815, 1
br i1 %cmp$cmp62815, label %truebranch$cmp62815, label %falsebranch$cmp62815
truebranch$cmp62815:
%ae49331 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist60782$k485170 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62816 = alloca %struct.ScmObj*, align 8
%argslist60782$k485171 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48063, %struct.ScmObj* %argslist60782$k485170)
store volatile %struct.ScmObj* %argslist60782$k485171, %struct.ScmObj** %stackaddr$prim62816, align 8
%stackaddr$prim62817 = alloca %struct.ScmObj*, align 8
%argslist60782$k485172 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49331, %struct.ScmObj* %argslist60782$k485171)
store volatile %struct.ScmObj* %argslist60782$k485172, %struct.ScmObj** %stackaddr$prim62817, align 8
%clofunc62818 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48517)
musttail call tailcc void %clofunc62818(%struct.ScmObj* %k48517, %struct.ScmObj* %argslist60782$k485172)
ret void
falsebranch$cmp62815:
%stackaddr$makeclosure62819 = alloca %struct.ScmObj*, align 8
%fptrToInt62820 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49336 to i64
%ae49336 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt62820)
store volatile %struct.ScmObj* %ae49336, %struct.ScmObj** %stackaddr$makeclosure62819, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49336, %struct.ScmObj* %_37foldr148054, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49336, %struct.ScmObj* %k48517, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49336, %struct.ScmObj* %_37map148050, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49336, %struct.ScmObj* %f48064, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49336, %struct.ScmObj* %acc48063, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49336, %struct.ScmObj* %lsts48062, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49336, %struct.ScmObj* %_37foldr48060, i64 6)
%ae49337 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure62821 = alloca %struct.ScmObj*, align 8
%fptrToInt62822 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49338 to i64
%ae49338 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt62822)
store volatile %struct.ScmObj* %ae49338, %struct.ScmObj** %stackaddr$makeclosure62821, align 8
%argslist60822$ae493360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62823 = alloca %struct.ScmObj*, align 8
%argslist60822$ae493361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49338, %struct.ScmObj* %argslist60822$ae493360)
store volatile %struct.ScmObj* %argslist60822$ae493361, %struct.ScmObj** %stackaddr$prim62823, align 8
%stackaddr$prim62824 = alloca %struct.ScmObj*, align 8
%argslist60822$ae493362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49337, %struct.ScmObj* %argslist60822$ae493361)
store volatile %struct.ScmObj* %argslist60822$ae493362, %struct.ScmObj** %stackaddr$prim62824, align 8
%clofunc62825 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49336)
musttail call tailcc void %clofunc62825(%struct.ScmObj* %ae49336, %struct.ScmObj* %argslist60822$ae493362)
ret void
}

define tailcc void @proc_clo$ae49336(%struct.ScmObj* %env$ae49336,%struct.ScmObj* %current_45args60783) {
%stackaddr$env-ref62826 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49336, i64 0)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref62826
%stackaddr$env-ref62827 = alloca %struct.ScmObj*, align 8
%k48517 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49336, i64 1)
store %struct.ScmObj* %k48517, %struct.ScmObj** %stackaddr$env-ref62827
%stackaddr$env-ref62828 = alloca %struct.ScmObj*, align 8
%_37map148050 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49336, i64 2)
store %struct.ScmObj* %_37map148050, %struct.ScmObj** %stackaddr$env-ref62828
%stackaddr$env-ref62829 = alloca %struct.ScmObj*, align 8
%f48064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49336, i64 3)
store %struct.ScmObj* %f48064, %struct.ScmObj** %stackaddr$env-ref62829
%stackaddr$env-ref62830 = alloca %struct.ScmObj*, align 8
%acc48063 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49336, i64 4)
store %struct.ScmObj* %acc48063, %struct.ScmObj** %stackaddr$env-ref62830
%stackaddr$env-ref62831 = alloca %struct.ScmObj*, align 8
%lsts48062 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49336, i64 5)
store %struct.ScmObj* %lsts48062, %struct.ScmObj** %stackaddr$env-ref62831
%stackaddr$env-ref62832 = alloca %struct.ScmObj*, align 8
%_37foldr48060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49336, i64 6)
store %struct.ScmObj* %_37foldr48060, %struct.ScmObj** %stackaddr$env-ref62832
%stackaddr$prim62833 = alloca %struct.ScmObj*, align 8
%_95k48520 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60783)
store volatile %struct.ScmObj* %_95k48520, %struct.ScmObj** %stackaddr$prim62833, align 8
%stackaddr$prim62834 = alloca %struct.ScmObj*, align 8
%current_45args60784 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60783)
store volatile %struct.ScmObj* %current_45args60784, %struct.ScmObj** %stackaddr$prim62834, align 8
%stackaddr$prim62835 = alloca %struct.ScmObj*, align 8
%anf_45bind48220 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60784)
store volatile %struct.ScmObj* %anf_45bind48220, %struct.ScmObj** %stackaddr$prim62835, align 8
%stackaddr$makeclosure62836 = alloca %struct.ScmObj*, align 8
%fptrToInt62837 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49357 to i64
%ae49357 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt62837)
store volatile %struct.ScmObj* %ae49357, %struct.ScmObj** %stackaddr$makeclosure62836, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49357, %struct.ScmObj* %_37foldr148054, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49357, %struct.ScmObj* %k48517, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49357, %struct.ScmObj* %_37map148050, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49357, %struct.ScmObj* %f48064, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49357, %struct.ScmObj* %acc48063, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49357, %struct.ScmObj* %lsts48062, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49357, %struct.ScmObj* %_37foldr48060, i64 6)
%argslist60817$_37map1480500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62838 = alloca %struct.ScmObj*, align 8
%argslist60817$_37map1480501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48062, %struct.ScmObj* %argslist60817$_37map1480500)
store volatile %struct.ScmObj* %argslist60817$_37map1480501, %struct.ScmObj** %stackaddr$prim62838, align 8
%stackaddr$prim62839 = alloca %struct.ScmObj*, align 8
%argslist60817$_37map1480502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48220, %struct.ScmObj* %argslist60817$_37map1480501)
store volatile %struct.ScmObj* %argslist60817$_37map1480502, %struct.ScmObj** %stackaddr$prim62839, align 8
%stackaddr$prim62840 = alloca %struct.ScmObj*, align 8
%argslist60817$_37map1480503 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49357, %struct.ScmObj* %argslist60817$_37map1480502)
store volatile %struct.ScmObj* %argslist60817$_37map1480503, %struct.ScmObj** %stackaddr$prim62840, align 8
%clofunc62841 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148050)
musttail call tailcc void %clofunc62841(%struct.ScmObj* %_37map148050, %struct.ScmObj* %argslist60817$_37map1480503)
ret void
}

define tailcc void @proc_clo$ae49357(%struct.ScmObj* %env$ae49357,%struct.ScmObj* %current_45args60786) {
%stackaddr$env-ref62842 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49357, i64 0)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref62842
%stackaddr$env-ref62843 = alloca %struct.ScmObj*, align 8
%k48517 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49357, i64 1)
store %struct.ScmObj* %k48517, %struct.ScmObj** %stackaddr$env-ref62843
%stackaddr$env-ref62844 = alloca %struct.ScmObj*, align 8
%_37map148050 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49357, i64 2)
store %struct.ScmObj* %_37map148050, %struct.ScmObj** %stackaddr$env-ref62844
%stackaddr$env-ref62845 = alloca %struct.ScmObj*, align 8
%f48064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49357, i64 3)
store %struct.ScmObj* %f48064, %struct.ScmObj** %stackaddr$env-ref62845
%stackaddr$env-ref62846 = alloca %struct.ScmObj*, align 8
%acc48063 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49357, i64 4)
store %struct.ScmObj* %acc48063, %struct.ScmObj** %stackaddr$env-ref62846
%stackaddr$env-ref62847 = alloca %struct.ScmObj*, align 8
%lsts48062 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49357, i64 5)
store %struct.ScmObj* %lsts48062, %struct.ScmObj** %stackaddr$env-ref62847
%stackaddr$env-ref62848 = alloca %struct.ScmObj*, align 8
%_37foldr48060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49357, i64 6)
store %struct.ScmObj* %_37foldr48060, %struct.ScmObj** %stackaddr$env-ref62848
%stackaddr$prim62849 = alloca %struct.ScmObj*, align 8
%_95k48521 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60786)
store volatile %struct.ScmObj* %_95k48521, %struct.ScmObj** %stackaddr$prim62849, align 8
%stackaddr$prim62850 = alloca %struct.ScmObj*, align 8
%current_45args60787 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60786)
store volatile %struct.ScmObj* %current_45args60787, %struct.ScmObj** %stackaddr$prim62850, align 8
%stackaddr$prim62851 = alloca %struct.ScmObj*, align 8
%lsts_4348069 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60787)
store volatile %struct.ScmObj* %lsts_4348069, %struct.ScmObj** %stackaddr$prim62851, align 8
%stackaddr$makeclosure62852 = alloca %struct.ScmObj*, align 8
%fptrToInt62853 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49360 to i64
%ae49360 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt62853)
store volatile %struct.ScmObj* %ae49360, %struct.ScmObj** %stackaddr$makeclosure62852, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49360, %struct.ScmObj* %lsts_4348069, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49360, %struct.ScmObj* %k48517, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49360, %struct.ScmObj* %_37foldr148054, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49360, %struct.ScmObj* %_37map148050, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49360, %struct.ScmObj* %f48064, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49360, %struct.ScmObj* %acc48063, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49360, %struct.ScmObj* %lsts48062, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae49360, %struct.ScmObj* %_37foldr48060, i64 7)
%ae49361 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure62854 = alloca %struct.ScmObj*, align 8
%fptrToInt62855 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49362 to i64
%ae49362 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt62855)
store volatile %struct.ScmObj* %ae49362, %struct.ScmObj** %stackaddr$makeclosure62854, align 8
%argslist60816$ae493600 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62856 = alloca %struct.ScmObj*, align 8
%argslist60816$ae493601 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49362, %struct.ScmObj* %argslist60816$ae493600)
store volatile %struct.ScmObj* %argslist60816$ae493601, %struct.ScmObj** %stackaddr$prim62856, align 8
%stackaddr$prim62857 = alloca %struct.ScmObj*, align 8
%argslist60816$ae493602 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49361, %struct.ScmObj* %argslist60816$ae493601)
store volatile %struct.ScmObj* %argslist60816$ae493602, %struct.ScmObj** %stackaddr$prim62857, align 8
%clofunc62858 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49360)
musttail call tailcc void %clofunc62858(%struct.ScmObj* %ae49360, %struct.ScmObj* %argslist60816$ae493602)
ret void
}

define tailcc void @proc_clo$ae49360(%struct.ScmObj* %env$ae49360,%struct.ScmObj* %current_45args60789) {
%stackaddr$env-ref62859 = alloca %struct.ScmObj*, align 8
%lsts_4348069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49360, i64 0)
store %struct.ScmObj* %lsts_4348069, %struct.ScmObj** %stackaddr$env-ref62859
%stackaddr$env-ref62860 = alloca %struct.ScmObj*, align 8
%k48517 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49360, i64 1)
store %struct.ScmObj* %k48517, %struct.ScmObj** %stackaddr$env-ref62860
%stackaddr$env-ref62861 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49360, i64 2)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref62861
%stackaddr$env-ref62862 = alloca %struct.ScmObj*, align 8
%_37map148050 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49360, i64 3)
store %struct.ScmObj* %_37map148050, %struct.ScmObj** %stackaddr$env-ref62862
%stackaddr$env-ref62863 = alloca %struct.ScmObj*, align 8
%f48064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49360, i64 4)
store %struct.ScmObj* %f48064, %struct.ScmObj** %stackaddr$env-ref62863
%stackaddr$env-ref62864 = alloca %struct.ScmObj*, align 8
%acc48063 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49360, i64 5)
store %struct.ScmObj* %acc48063, %struct.ScmObj** %stackaddr$env-ref62864
%stackaddr$env-ref62865 = alloca %struct.ScmObj*, align 8
%lsts48062 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49360, i64 6)
store %struct.ScmObj* %lsts48062, %struct.ScmObj** %stackaddr$env-ref62865
%stackaddr$env-ref62866 = alloca %struct.ScmObj*, align 8
%_37foldr48060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49360, i64 7)
store %struct.ScmObj* %_37foldr48060, %struct.ScmObj** %stackaddr$env-ref62866
%stackaddr$prim62867 = alloca %struct.ScmObj*, align 8
%_95k48522 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60789)
store volatile %struct.ScmObj* %_95k48522, %struct.ScmObj** %stackaddr$prim62867, align 8
%stackaddr$prim62868 = alloca %struct.ScmObj*, align 8
%current_45args60790 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60789)
store volatile %struct.ScmObj* %current_45args60790, %struct.ScmObj** %stackaddr$prim62868, align 8
%stackaddr$prim62869 = alloca %struct.ScmObj*, align 8
%anf_45bind48221 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60790)
store volatile %struct.ScmObj* %anf_45bind48221, %struct.ScmObj** %stackaddr$prim62869, align 8
%stackaddr$makeclosure62870 = alloca %struct.ScmObj*, align 8
%fptrToInt62871 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49381 to i64
%ae49381 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt62871)
store volatile %struct.ScmObj* %ae49381, %struct.ScmObj** %stackaddr$makeclosure62870, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49381, %struct.ScmObj* %lsts_4348069, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49381, %struct.ScmObj* %k48517, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49381, %struct.ScmObj* %_37foldr148054, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49381, %struct.ScmObj* %f48064, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49381, %struct.ScmObj* %acc48063, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49381, %struct.ScmObj* %_37foldr48060, i64 5)
%argslist60811$_37map1480500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62872 = alloca %struct.ScmObj*, align 8
%argslist60811$_37map1480501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48062, %struct.ScmObj* %argslist60811$_37map1480500)
store volatile %struct.ScmObj* %argslist60811$_37map1480501, %struct.ScmObj** %stackaddr$prim62872, align 8
%stackaddr$prim62873 = alloca %struct.ScmObj*, align 8
%argslist60811$_37map1480502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48221, %struct.ScmObj* %argslist60811$_37map1480501)
store volatile %struct.ScmObj* %argslist60811$_37map1480502, %struct.ScmObj** %stackaddr$prim62873, align 8
%stackaddr$prim62874 = alloca %struct.ScmObj*, align 8
%argslist60811$_37map1480503 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49381, %struct.ScmObj* %argslist60811$_37map1480502)
store volatile %struct.ScmObj* %argslist60811$_37map1480503, %struct.ScmObj** %stackaddr$prim62874, align 8
%clofunc62875 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148050)
musttail call tailcc void %clofunc62875(%struct.ScmObj* %_37map148050, %struct.ScmObj* %argslist60811$_37map1480503)
ret void
}

define tailcc void @proc_clo$ae49381(%struct.ScmObj* %env$ae49381,%struct.ScmObj* %current_45args60792) {
%stackaddr$env-ref62876 = alloca %struct.ScmObj*, align 8
%lsts_4348069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49381, i64 0)
store %struct.ScmObj* %lsts_4348069, %struct.ScmObj** %stackaddr$env-ref62876
%stackaddr$env-ref62877 = alloca %struct.ScmObj*, align 8
%k48517 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49381, i64 1)
store %struct.ScmObj* %k48517, %struct.ScmObj** %stackaddr$env-ref62877
%stackaddr$env-ref62878 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49381, i64 2)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref62878
%stackaddr$env-ref62879 = alloca %struct.ScmObj*, align 8
%f48064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49381, i64 3)
store %struct.ScmObj* %f48064, %struct.ScmObj** %stackaddr$env-ref62879
%stackaddr$env-ref62880 = alloca %struct.ScmObj*, align 8
%acc48063 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49381, i64 4)
store %struct.ScmObj* %acc48063, %struct.ScmObj** %stackaddr$env-ref62880
%stackaddr$env-ref62881 = alloca %struct.ScmObj*, align 8
%_37foldr48060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49381, i64 5)
store %struct.ScmObj* %_37foldr48060, %struct.ScmObj** %stackaddr$env-ref62881
%stackaddr$prim62882 = alloca %struct.ScmObj*, align 8
%_95k48523 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60792)
store volatile %struct.ScmObj* %_95k48523, %struct.ScmObj** %stackaddr$prim62882, align 8
%stackaddr$prim62883 = alloca %struct.ScmObj*, align 8
%current_45args60793 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60792)
store volatile %struct.ScmObj* %current_45args60793, %struct.ScmObj** %stackaddr$prim62883, align 8
%stackaddr$prim62884 = alloca %struct.ScmObj*, align 8
%vs48067 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60793)
store volatile %struct.ScmObj* %vs48067, %struct.ScmObj** %stackaddr$prim62884, align 8
%stackaddr$makeclosure62885 = alloca %struct.ScmObj*, align 8
%fptrToInt62886 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49384 to i64
%ae49384 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt62886)
store volatile %struct.ScmObj* %ae49384, %struct.ScmObj** %stackaddr$makeclosure62885, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49384, %struct.ScmObj* %lsts_4348069, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49384, %struct.ScmObj* %k48517, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49384, %struct.ScmObj* %_37foldr148054, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49384, %struct.ScmObj* %vs48067, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49384, %struct.ScmObj* %f48064, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49384, %struct.ScmObj* %acc48063, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49384, %struct.ScmObj* %_37foldr48060, i64 6)
%ae49385 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure62887 = alloca %struct.ScmObj*, align 8
%fptrToInt62888 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49386 to i64
%ae49386 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt62888)
store volatile %struct.ScmObj* %ae49386, %struct.ScmObj** %stackaddr$makeclosure62887, align 8
%argslist60810$ae493840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62889 = alloca %struct.ScmObj*, align 8
%argslist60810$ae493841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49386, %struct.ScmObj* %argslist60810$ae493840)
store volatile %struct.ScmObj* %argslist60810$ae493841, %struct.ScmObj** %stackaddr$prim62889, align 8
%stackaddr$prim62890 = alloca %struct.ScmObj*, align 8
%argslist60810$ae493842 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49385, %struct.ScmObj* %argslist60810$ae493841)
store volatile %struct.ScmObj* %argslist60810$ae493842, %struct.ScmObj** %stackaddr$prim62890, align 8
%clofunc62891 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49384)
musttail call tailcc void %clofunc62891(%struct.ScmObj* %ae49384, %struct.ScmObj* %argslist60810$ae493842)
ret void
}

define tailcc void @proc_clo$ae49384(%struct.ScmObj* %env$ae49384,%struct.ScmObj* %current_45args60795) {
%stackaddr$env-ref62892 = alloca %struct.ScmObj*, align 8
%lsts_4348069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49384, i64 0)
store %struct.ScmObj* %lsts_4348069, %struct.ScmObj** %stackaddr$env-ref62892
%stackaddr$env-ref62893 = alloca %struct.ScmObj*, align 8
%k48517 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49384, i64 1)
store %struct.ScmObj* %k48517, %struct.ScmObj** %stackaddr$env-ref62893
%stackaddr$env-ref62894 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49384, i64 2)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref62894
%stackaddr$env-ref62895 = alloca %struct.ScmObj*, align 8
%vs48067 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49384, i64 3)
store %struct.ScmObj* %vs48067, %struct.ScmObj** %stackaddr$env-ref62895
%stackaddr$env-ref62896 = alloca %struct.ScmObj*, align 8
%f48064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49384, i64 4)
store %struct.ScmObj* %f48064, %struct.ScmObj** %stackaddr$env-ref62896
%stackaddr$env-ref62897 = alloca %struct.ScmObj*, align 8
%acc48063 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49384, i64 5)
store %struct.ScmObj* %acc48063, %struct.ScmObj** %stackaddr$env-ref62897
%stackaddr$env-ref62898 = alloca %struct.ScmObj*, align 8
%_37foldr48060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49384, i64 6)
store %struct.ScmObj* %_37foldr48060, %struct.ScmObj** %stackaddr$env-ref62898
%stackaddr$prim62899 = alloca %struct.ScmObj*, align 8
%_95k48524 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60795)
store volatile %struct.ScmObj* %_95k48524, %struct.ScmObj** %stackaddr$prim62899, align 8
%stackaddr$prim62900 = alloca %struct.ScmObj*, align 8
%current_45args60796 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60795)
store volatile %struct.ScmObj* %current_45args60796, %struct.ScmObj** %stackaddr$prim62900, align 8
%stackaddr$prim62901 = alloca %struct.ScmObj*, align 8
%anf_45bind48222 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60796)
store volatile %struct.ScmObj* %anf_45bind48222, %struct.ScmObj** %stackaddr$prim62901, align 8
%stackaddr$prim62902 = alloca %struct.ScmObj*, align 8
%anf_45bind48223 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48063, %struct.ScmObj* %lsts_4348069)
store volatile %struct.ScmObj* %anf_45bind48223, %struct.ScmObj** %stackaddr$prim62902, align 8
%stackaddr$prim62903 = alloca %struct.ScmObj*, align 8
%anf_45bind48224 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48064, %struct.ScmObj* %anf_45bind48223)
store volatile %struct.ScmObj* %anf_45bind48224, %struct.ScmObj** %stackaddr$prim62903, align 8
%stackaddr$makeclosure62904 = alloca %struct.ScmObj*, align 8
%fptrToInt62905 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49410 to i64
%ae49410 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt62905)
store volatile %struct.ScmObj* %ae49410, %struct.ScmObj** %stackaddr$makeclosure62904, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49410, %struct.ScmObj* %_37foldr148054, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49410, %struct.ScmObj* %k48517, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49410, %struct.ScmObj* %vs48067, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49410, %struct.ScmObj* %f48064, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49410, %struct.ScmObj* %anf_45bind48222, i64 4)
%stackaddr$prim62906 = alloca %struct.ScmObj*, align 8
%cpsargs48528 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49410, %struct.ScmObj* %anf_45bind48224)
store volatile %struct.ScmObj* %cpsargs48528, %struct.ScmObj** %stackaddr$prim62906, align 8
%clofunc62907 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr48060)
musttail call tailcc void %clofunc62907(%struct.ScmObj* %_37foldr48060, %struct.ScmObj* %cpsargs48528)
ret void
}

define tailcc void @proc_clo$ae49410(%struct.ScmObj* %env$ae49410,%struct.ScmObj* %current_45args60798) {
%stackaddr$env-ref62908 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49410, i64 0)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref62908
%stackaddr$env-ref62909 = alloca %struct.ScmObj*, align 8
%k48517 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49410, i64 1)
store %struct.ScmObj* %k48517, %struct.ScmObj** %stackaddr$env-ref62909
%stackaddr$env-ref62910 = alloca %struct.ScmObj*, align 8
%vs48067 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49410, i64 2)
store %struct.ScmObj* %vs48067, %struct.ScmObj** %stackaddr$env-ref62910
%stackaddr$env-ref62911 = alloca %struct.ScmObj*, align 8
%f48064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49410, i64 3)
store %struct.ScmObj* %f48064, %struct.ScmObj** %stackaddr$env-ref62911
%stackaddr$env-ref62912 = alloca %struct.ScmObj*, align 8
%anf_45bind48222 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49410, i64 4)
store %struct.ScmObj* %anf_45bind48222, %struct.ScmObj** %stackaddr$env-ref62912
%stackaddr$prim62913 = alloca %struct.ScmObj*, align 8
%_95k48525 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60798)
store volatile %struct.ScmObj* %_95k48525, %struct.ScmObj** %stackaddr$prim62913, align 8
%stackaddr$prim62914 = alloca %struct.ScmObj*, align 8
%current_45args60799 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60798)
store volatile %struct.ScmObj* %current_45args60799, %struct.ScmObj** %stackaddr$prim62914, align 8
%stackaddr$prim62915 = alloca %struct.ScmObj*, align 8
%anf_45bind48225 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60799)
store volatile %struct.ScmObj* %anf_45bind48225, %struct.ScmObj** %stackaddr$prim62915, align 8
%ae49415 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62916 = alloca %struct.ScmObj*, align 8
%anf_45bind48226 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48225, %struct.ScmObj* %ae49415)
store volatile %struct.ScmObj* %anf_45bind48226, %struct.ScmObj** %stackaddr$prim62916, align 8
%stackaddr$makeclosure62917 = alloca %struct.ScmObj*, align 8
%fptrToInt62918 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49417 to i64
%ae49417 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt62918)
store volatile %struct.ScmObj* %ae49417, %struct.ScmObj** %stackaddr$makeclosure62917, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49417, %struct.ScmObj* %k48517, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49417, %struct.ScmObj* %f48064, i64 1)
%argslist60804$_37foldr1480540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62919 = alloca %struct.ScmObj*, align 8
%argslist60804$_37foldr1480541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs48067, %struct.ScmObj* %argslist60804$_37foldr1480540)
store volatile %struct.ScmObj* %argslist60804$_37foldr1480541, %struct.ScmObj** %stackaddr$prim62919, align 8
%stackaddr$prim62920 = alloca %struct.ScmObj*, align 8
%argslist60804$_37foldr1480542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48226, %struct.ScmObj* %argslist60804$_37foldr1480541)
store volatile %struct.ScmObj* %argslist60804$_37foldr1480542, %struct.ScmObj** %stackaddr$prim62920, align 8
%stackaddr$prim62921 = alloca %struct.ScmObj*, align 8
%argslist60804$_37foldr1480543 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48222, %struct.ScmObj* %argslist60804$_37foldr1480542)
store volatile %struct.ScmObj* %argslist60804$_37foldr1480543, %struct.ScmObj** %stackaddr$prim62921, align 8
%stackaddr$prim62922 = alloca %struct.ScmObj*, align 8
%argslist60804$_37foldr1480544 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49417, %struct.ScmObj* %argslist60804$_37foldr1480543)
store volatile %struct.ScmObj* %argslist60804$_37foldr1480544, %struct.ScmObj** %stackaddr$prim62922, align 8
%clofunc62923 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148054)
musttail call tailcc void %clofunc62923(%struct.ScmObj* %_37foldr148054, %struct.ScmObj* %argslist60804$_37foldr1480544)
ret void
}

define tailcc void @proc_clo$ae49417(%struct.ScmObj* %env$ae49417,%struct.ScmObj* %current_45args60801) {
%stackaddr$env-ref62924 = alloca %struct.ScmObj*, align 8
%k48517 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49417, i64 0)
store %struct.ScmObj* %k48517, %struct.ScmObj** %stackaddr$env-ref62924
%stackaddr$env-ref62925 = alloca %struct.ScmObj*, align 8
%f48064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49417, i64 1)
store %struct.ScmObj* %f48064, %struct.ScmObj** %stackaddr$env-ref62925
%stackaddr$prim62926 = alloca %struct.ScmObj*, align 8
%_95k48526 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60801)
store volatile %struct.ScmObj* %_95k48526, %struct.ScmObj** %stackaddr$prim62926, align 8
%stackaddr$prim62927 = alloca %struct.ScmObj*, align 8
%current_45args60802 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60801)
store volatile %struct.ScmObj* %current_45args60802, %struct.ScmObj** %stackaddr$prim62927, align 8
%stackaddr$prim62928 = alloca %struct.ScmObj*, align 8
%anf_45bind48227 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60802)
store volatile %struct.ScmObj* %anf_45bind48227, %struct.ScmObj** %stackaddr$prim62928, align 8
%stackaddr$prim62929 = alloca %struct.ScmObj*, align 8
%cpsargs48527 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48517, %struct.ScmObj* %anf_45bind48227)
store volatile %struct.ScmObj* %cpsargs48527, %struct.ScmObj** %stackaddr$prim62929, align 8
%clofunc62930 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48064)
musttail call tailcc void %clofunc62930(%struct.ScmObj* %f48064, %struct.ScmObj* %cpsargs48527)
ret void
}

define tailcc void @proc_clo$ae49386(%struct.ScmObj* %env$ae49386,%struct.ScmObj* %current_45args60805) {
%stackaddr$prim62931 = alloca %struct.ScmObj*, align 8
%k48529 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60805)
store volatile %struct.ScmObj* %k48529, %struct.ScmObj** %stackaddr$prim62931, align 8
%stackaddr$prim62932 = alloca %struct.ScmObj*, align 8
%current_45args60806 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60805)
store volatile %struct.ScmObj* %current_45args60806, %struct.ScmObj** %stackaddr$prim62932, align 8
%stackaddr$prim62933 = alloca %struct.ScmObj*, align 8
%a48072 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60806)
store volatile %struct.ScmObj* %a48072, %struct.ScmObj** %stackaddr$prim62933, align 8
%stackaddr$prim62934 = alloca %struct.ScmObj*, align 8
%current_45args60807 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60806)
store volatile %struct.ScmObj* %current_45args60807, %struct.ScmObj** %stackaddr$prim62934, align 8
%stackaddr$prim62935 = alloca %struct.ScmObj*, align 8
%b48071 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60807)
store volatile %struct.ScmObj* %b48071, %struct.ScmObj** %stackaddr$prim62935, align 8
%stackaddr$prim62936 = alloca %struct.ScmObj*, align 8
%cpsprim48530 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a48072, %struct.ScmObj* %b48071)
store volatile %struct.ScmObj* %cpsprim48530, %struct.ScmObj** %stackaddr$prim62936, align 8
%ae49390 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist60809$k485290 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62937 = alloca %struct.ScmObj*, align 8
%argslist60809$k485291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48530, %struct.ScmObj* %argslist60809$k485290)
store volatile %struct.ScmObj* %argslist60809$k485291, %struct.ScmObj** %stackaddr$prim62937, align 8
%stackaddr$prim62938 = alloca %struct.ScmObj*, align 8
%argslist60809$k485292 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49390, %struct.ScmObj* %argslist60809$k485291)
store volatile %struct.ScmObj* %argslist60809$k485292, %struct.ScmObj** %stackaddr$prim62938, align 8
%clofunc62939 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48529)
musttail call tailcc void %clofunc62939(%struct.ScmObj* %k48529, %struct.ScmObj* %argslist60809$k485292)
ret void
}

define tailcc void @proc_clo$ae49362(%struct.ScmObj* %env$ae49362,%struct.ScmObj* %current_45args60812) {
%stackaddr$prim62940 = alloca %struct.ScmObj*, align 8
%k48531 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60812)
store volatile %struct.ScmObj* %k48531, %struct.ScmObj** %stackaddr$prim62940, align 8
%stackaddr$prim62941 = alloca %struct.ScmObj*, align 8
%current_45args60813 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60812)
store volatile %struct.ScmObj* %current_45args60813, %struct.ScmObj** %stackaddr$prim62941, align 8
%stackaddr$prim62942 = alloca %struct.ScmObj*, align 8
%x48068 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60813)
store volatile %struct.ScmObj* %x48068, %struct.ScmObj** %stackaddr$prim62942, align 8
%stackaddr$prim62943 = alloca %struct.ScmObj*, align 8
%cpsprim48532 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48068)
store volatile %struct.ScmObj* %cpsprim48532, %struct.ScmObj** %stackaddr$prim62943, align 8
%ae49365 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist60815$k485310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62944 = alloca %struct.ScmObj*, align 8
%argslist60815$k485311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48532, %struct.ScmObj* %argslist60815$k485310)
store volatile %struct.ScmObj* %argslist60815$k485311, %struct.ScmObj** %stackaddr$prim62944, align 8
%stackaddr$prim62945 = alloca %struct.ScmObj*, align 8
%argslist60815$k485312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49365, %struct.ScmObj* %argslist60815$k485311)
store volatile %struct.ScmObj* %argslist60815$k485312, %struct.ScmObj** %stackaddr$prim62945, align 8
%clofunc62946 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48531)
musttail call tailcc void %clofunc62946(%struct.ScmObj* %k48531, %struct.ScmObj* %argslist60815$k485312)
ret void
}

define tailcc void @proc_clo$ae49338(%struct.ScmObj* %env$ae49338,%struct.ScmObj* %current_45args60818) {
%stackaddr$prim62947 = alloca %struct.ScmObj*, align 8
%k48533 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60818)
store volatile %struct.ScmObj* %k48533, %struct.ScmObj** %stackaddr$prim62947, align 8
%stackaddr$prim62948 = alloca %struct.ScmObj*, align 8
%current_45args60819 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60818)
store volatile %struct.ScmObj* %current_45args60819, %struct.ScmObj** %stackaddr$prim62948, align 8
%stackaddr$prim62949 = alloca %struct.ScmObj*, align 8
%x48070 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60819)
store volatile %struct.ScmObj* %x48070, %struct.ScmObj** %stackaddr$prim62949, align 8
%stackaddr$prim62950 = alloca %struct.ScmObj*, align 8
%cpsprim48534 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48070)
store volatile %struct.ScmObj* %cpsprim48534, %struct.ScmObj** %stackaddr$prim62950, align 8
%ae49341 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist60821$k485330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62951 = alloca %struct.ScmObj*, align 8
%argslist60821$k485331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48534, %struct.ScmObj* %argslist60821$k485330)
store volatile %struct.ScmObj* %argslist60821$k485331, %struct.ScmObj** %stackaddr$prim62951, align 8
%stackaddr$prim62952 = alloca %struct.ScmObj*, align 8
%argslist60821$k485332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49341, %struct.ScmObj* %argslist60821$k485331)
store volatile %struct.ScmObj* %argslist60821$k485332, %struct.ScmObj** %stackaddr$prim62952, align 8
%clofunc62953 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48533)
musttail call tailcc void %clofunc62953(%struct.ScmObj* %k48533, %struct.ScmObj* %argslist60821$k485332)
ret void
}

define tailcc void @proc_clo$ae49290(%struct.ScmObj* %env$ae49290,%struct.ScmObj* %current_45args60824) {
%stackaddr$prim62954 = alloca %struct.ScmObj*, align 8
%k48535 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60824)
store volatile %struct.ScmObj* %k48535, %struct.ScmObj** %stackaddr$prim62954, align 8
%stackaddr$prim62955 = alloca %struct.ScmObj*, align 8
%current_45args60825 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60824)
store volatile %struct.ScmObj* %current_45args60825, %struct.ScmObj** %stackaddr$prim62955, align 8
%stackaddr$prim62956 = alloca %struct.ScmObj*, align 8
%lst48066 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60825)
store volatile %struct.ScmObj* %lst48066, %struct.ScmObj** %stackaddr$prim62956, align 8
%stackaddr$prim62957 = alloca %struct.ScmObj*, align 8
%current_45args60826 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60825)
store volatile %struct.ScmObj* %current_45args60826, %struct.ScmObj** %stackaddr$prim62957, align 8
%stackaddr$prim62958 = alloca %struct.ScmObj*, align 8
%b48065 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60826)
store volatile %struct.ScmObj* %b48065, %struct.ScmObj** %stackaddr$prim62958, align 8
%truthy$cmp62959 = call i64 @is_truthy_value(%struct.ScmObj* %b48065)
%cmp$cmp62959 = icmp eq i64 %truthy$cmp62959, 1
br i1 %cmp$cmp62959, label %truebranch$cmp62959, label %falsebranch$cmp62959
truebranch$cmp62959:
%ae49293 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist60828$k485350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62960 = alloca %struct.ScmObj*, align 8
%argslist60828$k485351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b48065, %struct.ScmObj* %argslist60828$k485350)
store volatile %struct.ScmObj* %argslist60828$k485351, %struct.ScmObj** %stackaddr$prim62960, align 8
%stackaddr$prim62961 = alloca %struct.ScmObj*, align 8
%argslist60828$k485352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49293, %struct.ScmObj* %argslist60828$k485351)
store volatile %struct.ScmObj* %argslist60828$k485352, %struct.ScmObj** %stackaddr$prim62961, align 8
%clofunc62962 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48535)
musttail call tailcc void %clofunc62962(%struct.ScmObj* %k48535, %struct.ScmObj* %argslist60828$k485352)
ret void
falsebranch$cmp62959:
%stackaddr$prim62963 = alloca %struct.ScmObj*, align 8
%cpsprim48536 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48066)
store volatile %struct.ScmObj* %cpsprim48536, %struct.ScmObj** %stackaddr$prim62963, align 8
%ae49300 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist60829$k485350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62964 = alloca %struct.ScmObj*, align 8
%argslist60829$k485351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48536, %struct.ScmObj* %argslist60829$k485350)
store volatile %struct.ScmObj* %argslist60829$k485351, %struct.ScmObj** %stackaddr$prim62964, align 8
%stackaddr$prim62965 = alloca %struct.ScmObj*, align 8
%argslist60829$k485352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49300, %struct.ScmObj* %argslist60829$k485351)
store volatile %struct.ScmObj* %argslist60829$k485352, %struct.ScmObj** %stackaddr$prim62965, align 8
%clofunc62966 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48535)
musttail call tailcc void %clofunc62966(%struct.ScmObj* %k48535, %struct.ScmObj* %argslist60829$k485352)
ret void
}

define tailcc void @proc_clo$ae49247(%struct.ScmObj* %env$ae49247,%struct.ScmObj* %current_45args60833) {
%stackaddr$env-ref62967 = alloca %struct.ScmObj*, align 8
%_37take48046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49247, i64 0)
store %struct.ScmObj* %_37take48046, %struct.ScmObj** %stackaddr$env-ref62967
%stackaddr$env-ref62968 = alloca %struct.ScmObj*, align 8
%_37length48043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49247, i64 1)
store %struct.ScmObj* %_37length48043, %struct.ScmObj** %stackaddr$env-ref62968
%stackaddr$prim62969 = alloca %struct.ScmObj*, align 8
%k48537 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60833)
store volatile %struct.ScmObj* %k48537, %struct.ScmObj** %stackaddr$prim62969, align 8
%stackaddr$prim62970 = alloca %struct.ScmObj*, align 8
%current_45args60834 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60833)
store volatile %struct.ScmObj* %current_45args60834, %struct.ScmObj** %stackaddr$prim62970, align 8
%stackaddr$prim62971 = alloca %struct.ScmObj*, align 8
%lst48075 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60834)
store volatile %struct.ScmObj* %lst48075, %struct.ScmObj** %stackaddr$prim62971, align 8
%stackaddr$prim62972 = alloca %struct.ScmObj*, align 8
%current_45args60835 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60834)
store volatile %struct.ScmObj* %current_45args60835, %struct.ScmObj** %stackaddr$prim62972, align 8
%stackaddr$prim62973 = alloca %struct.ScmObj*, align 8
%n48074 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60835)
store volatile %struct.ScmObj* %n48074, %struct.ScmObj** %stackaddr$prim62973, align 8
%stackaddr$makeclosure62974 = alloca %struct.ScmObj*, align 8
%fptrToInt62975 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49249 to i64
%ae49249 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt62975)
store volatile %struct.ScmObj* %ae49249, %struct.ScmObj** %stackaddr$makeclosure62974, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49249, %struct.ScmObj* %k48537, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49249, %struct.ScmObj* %_37take48046, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49249, %struct.ScmObj* %lst48075, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49249, %struct.ScmObj* %n48074, i64 3)
%argslist60841$_37length480430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62976 = alloca %struct.ScmObj*, align 8
%argslist60841$_37length480431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48075, %struct.ScmObj* %argslist60841$_37length480430)
store volatile %struct.ScmObj* %argslist60841$_37length480431, %struct.ScmObj** %stackaddr$prim62976, align 8
%stackaddr$prim62977 = alloca %struct.ScmObj*, align 8
%argslist60841$_37length480432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49249, %struct.ScmObj* %argslist60841$_37length480431)
store volatile %struct.ScmObj* %argslist60841$_37length480432, %struct.ScmObj** %stackaddr$prim62977, align 8
%clofunc62978 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length48043)
musttail call tailcc void %clofunc62978(%struct.ScmObj* %_37length48043, %struct.ScmObj* %argslist60841$_37length480432)
ret void
}

define tailcc void @proc_clo$ae49249(%struct.ScmObj* %env$ae49249,%struct.ScmObj* %current_45args60837) {
%stackaddr$env-ref62979 = alloca %struct.ScmObj*, align 8
%k48537 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49249, i64 0)
store %struct.ScmObj* %k48537, %struct.ScmObj** %stackaddr$env-ref62979
%stackaddr$env-ref62980 = alloca %struct.ScmObj*, align 8
%_37take48046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49249, i64 1)
store %struct.ScmObj* %_37take48046, %struct.ScmObj** %stackaddr$env-ref62980
%stackaddr$env-ref62981 = alloca %struct.ScmObj*, align 8
%lst48075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49249, i64 2)
store %struct.ScmObj* %lst48075, %struct.ScmObj** %stackaddr$env-ref62981
%stackaddr$env-ref62982 = alloca %struct.ScmObj*, align 8
%n48074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49249, i64 3)
store %struct.ScmObj* %n48074, %struct.ScmObj** %stackaddr$env-ref62982
%stackaddr$prim62983 = alloca %struct.ScmObj*, align 8
%_95k48538 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60837)
store volatile %struct.ScmObj* %_95k48538, %struct.ScmObj** %stackaddr$prim62983, align 8
%stackaddr$prim62984 = alloca %struct.ScmObj*, align 8
%current_45args60838 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60837)
store volatile %struct.ScmObj* %current_45args60838, %struct.ScmObj** %stackaddr$prim62984, align 8
%stackaddr$prim62985 = alloca %struct.ScmObj*, align 8
%anf_45bind48214 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60838)
store volatile %struct.ScmObj* %anf_45bind48214, %struct.ScmObj** %stackaddr$prim62985, align 8
%stackaddr$prim62986 = alloca %struct.ScmObj*, align 8
%anf_45bind48215 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind48214, %struct.ScmObj* %n48074)
store volatile %struct.ScmObj* %anf_45bind48215, %struct.ScmObj** %stackaddr$prim62986, align 8
%argslist60840$_37take480460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62987 = alloca %struct.ScmObj*, align 8
%argslist60840$_37take480461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48215, %struct.ScmObj* %argslist60840$_37take480460)
store volatile %struct.ScmObj* %argslist60840$_37take480461, %struct.ScmObj** %stackaddr$prim62987, align 8
%stackaddr$prim62988 = alloca %struct.ScmObj*, align 8
%argslist60840$_37take480462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48075, %struct.ScmObj* %argslist60840$_37take480461)
store volatile %struct.ScmObj* %argslist60840$_37take480462, %struct.ScmObj** %stackaddr$prim62988, align 8
%stackaddr$prim62989 = alloca %struct.ScmObj*, align 8
%argslist60840$_37take480463 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48537, %struct.ScmObj* %argslist60840$_37take480462)
store volatile %struct.ScmObj* %argslist60840$_37take480463, %struct.ScmObj** %stackaddr$prim62989, align 8
%clofunc62990 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take48046)
musttail call tailcc void %clofunc62990(%struct.ScmObj* %_37take48046, %struct.ScmObj* %argslist60840$_37take480463)
ret void
}

define tailcc void @proc_clo$ae49193(%struct.ScmObj* %env$ae49193,%struct.ScmObj* %current_45args60843) {
%stackaddr$env-ref62991 = alloca %struct.ScmObj*, align 8
%_37foldl148038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49193, i64 0)
store %struct.ScmObj* %_37foldl148038, %struct.ScmObj** %stackaddr$env-ref62991
%stackaddr$prim62992 = alloca %struct.ScmObj*, align 8
%k48539 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60843)
store volatile %struct.ScmObj* %k48539, %struct.ScmObj** %stackaddr$prim62992, align 8
%stackaddr$prim62993 = alloca %struct.ScmObj*, align 8
%current_45args60844 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60843)
store volatile %struct.ScmObj* %current_45args60844, %struct.ScmObj** %stackaddr$prim62993, align 8
%stackaddr$prim62994 = alloca %struct.ScmObj*, align 8
%lst48077 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60844)
store volatile %struct.ScmObj* %lst48077, %struct.ScmObj** %stackaddr$prim62994, align 8
%stackaddr$makeclosure62995 = alloca %struct.ScmObj*, align 8
%fptrToInt62996 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49194 to i64
%ae49194 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt62996)
store volatile %struct.ScmObj* %ae49194, %struct.ScmObj** %stackaddr$makeclosure62995, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49194, %struct.ScmObj* %_37foldl148038, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49194, %struct.ScmObj* %lst48077, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49194, %struct.ScmObj* %k48539, i64 2)
%ae49195 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure62997 = alloca %struct.ScmObj*, align 8
%fptrToInt62998 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49196 to i64
%ae49196 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt62998)
store volatile %struct.ScmObj* %ae49196, %struct.ScmObj** %stackaddr$makeclosure62997, align 8
%argslist60855$ae491940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62999 = alloca %struct.ScmObj*, align 8
%argslist60855$ae491941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49196, %struct.ScmObj* %argslist60855$ae491940)
store volatile %struct.ScmObj* %argslist60855$ae491941, %struct.ScmObj** %stackaddr$prim62999, align 8
%stackaddr$prim63000 = alloca %struct.ScmObj*, align 8
%argslist60855$ae491942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49195, %struct.ScmObj* %argslist60855$ae491941)
store volatile %struct.ScmObj* %argslist60855$ae491942, %struct.ScmObj** %stackaddr$prim63000, align 8
%clofunc63001 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49194)
musttail call tailcc void %clofunc63001(%struct.ScmObj* %ae49194, %struct.ScmObj* %argslist60855$ae491942)
ret void
}

define tailcc void @proc_clo$ae49194(%struct.ScmObj* %env$ae49194,%struct.ScmObj* %current_45args60846) {
%stackaddr$env-ref63002 = alloca %struct.ScmObj*, align 8
%_37foldl148038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49194, i64 0)
store %struct.ScmObj* %_37foldl148038, %struct.ScmObj** %stackaddr$env-ref63002
%stackaddr$env-ref63003 = alloca %struct.ScmObj*, align 8
%lst48077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49194, i64 1)
store %struct.ScmObj* %lst48077, %struct.ScmObj** %stackaddr$env-ref63003
%stackaddr$env-ref63004 = alloca %struct.ScmObj*, align 8
%k48539 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49194, i64 2)
store %struct.ScmObj* %k48539, %struct.ScmObj** %stackaddr$env-ref63004
%stackaddr$prim63005 = alloca %struct.ScmObj*, align 8
%_95k48540 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60846)
store volatile %struct.ScmObj* %_95k48540, %struct.ScmObj** %stackaddr$prim63005, align 8
%stackaddr$prim63006 = alloca %struct.ScmObj*, align 8
%current_45args60847 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60846)
store volatile %struct.ScmObj* %current_45args60847, %struct.ScmObj** %stackaddr$prim63006, align 8
%stackaddr$prim63007 = alloca %struct.ScmObj*, align 8
%anf_45bind48213 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60847)
store volatile %struct.ScmObj* %anf_45bind48213, %struct.ScmObj** %stackaddr$prim63007, align 8
%ae49215 = call %struct.ScmObj* @const_init_null()
%argslist60849$_37foldl1480380 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim63008 = alloca %struct.ScmObj*, align 8
%argslist60849$_37foldl1480381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48077, %struct.ScmObj* %argslist60849$_37foldl1480380)
store volatile %struct.ScmObj* %argslist60849$_37foldl1480381, %struct.ScmObj** %stackaddr$prim63008, align 8
%stackaddr$prim63009 = alloca %struct.ScmObj*, align 8
%argslist60849$_37foldl1480382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49215, %struct.ScmObj* %argslist60849$_37foldl1480381)
store volatile %struct.ScmObj* %argslist60849$_37foldl1480382, %struct.ScmObj** %stackaddr$prim63009, align 8
%stackaddr$prim63010 = alloca %struct.ScmObj*, align 8
%argslist60849$_37foldl1480383 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48213, %struct.ScmObj* %argslist60849$_37foldl1480382)
store volatile %struct.ScmObj* %argslist60849$_37foldl1480383, %struct.ScmObj** %stackaddr$prim63010, align 8
%stackaddr$prim63011 = alloca %struct.ScmObj*, align 8
%argslist60849$_37foldl1480384 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48539, %struct.ScmObj* %argslist60849$_37foldl1480383)
store volatile %struct.ScmObj* %argslist60849$_37foldl1480384, %struct.ScmObj** %stackaddr$prim63011, align 8
%clofunc63012 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148038)
musttail call tailcc void %clofunc63012(%struct.ScmObj* %_37foldl148038, %struct.ScmObj* %argslist60849$_37foldl1480384)
ret void
}

define tailcc void @proc_clo$ae49196(%struct.ScmObj* %env$ae49196,%struct.ScmObj* %current_45args60850) {
%stackaddr$prim63013 = alloca %struct.ScmObj*, align 8
%k48541 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60850)
store volatile %struct.ScmObj* %k48541, %struct.ScmObj** %stackaddr$prim63013, align 8
%stackaddr$prim63014 = alloca %struct.ScmObj*, align 8
%current_45args60851 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60850)
store volatile %struct.ScmObj* %current_45args60851, %struct.ScmObj** %stackaddr$prim63014, align 8
%stackaddr$prim63015 = alloca %struct.ScmObj*, align 8
%x48079 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60851)
store volatile %struct.ScmObj* %x48079, %struct.ScmObj** %stackaddr$prim63015, align 8
%stackaddr$prim63016 = alloca %struct.ScmObj*, align 8
%current_45args60852 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60851)
store volatile %struct.ScmObj* %current_45args60852, %struct.ScmObj** %stackaddr$prim63016, align 8
%stackaddr$prim63017 = alloca %struct.ScmObj*, align 8
%y48078 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60852)
store volatile %struct.ScmObj* %y48078, %struct.ScmObj** %stackaddr$prim63017, align 8
%ae49198 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist60854$k485410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim63018 = alloca %struct.ScmObj*, align 8
%argslist60854$k485411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x48079, %struct.ScmObj* %argslist60854$k485410)
store volatile %struct.ScmObj* %argslist60854$k485411, %struct.ScmObj** %stackaddr$prim63018, align 8
%stackaddr$prim63019 = alloca %struct.ScmObj*, align 8
%argslist60854$k485412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49198, %struct.ScmObj* %argslist60854$k485411)
store volatile %struct.ScmObj* %argslist60854$k485412, %struct.ScmObj** %stackaddr$prim63019, align 8
%clofunc63020 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48541)
musttail call tailcc void %clofunc63020(%struct.ScmObj* %k48541, %struct.ScmObj* %argslist60854$k485412)
ret void
}

define tailcc void @proc_clo$ae49114(%struct.ScmObj* %env$ae49114,%struct.ScmObj* %current_45args60858) {
%stackaddr$prim63021 = alloca %struct.ScmObj*, align 8
%k48542 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60858)
store volatile %struct.ScmObj* %k48542, %struct.ScmObj** %stackaddr$prim63021, align 8
%stackaddr$prim63022 = alloca %struct.ScmObj*, align 8
%current_45args60859 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60858)
store volatile %struct.ScmObj* %current_45args60859, %struct.ScmObj** %stackaddr$prim63022, align 8
%stackaddr$prim63023 = alloca %struct.ScmObj*, align 8
%_37foldl148039 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60859)
store volatile %struct.ScmObj* %_37foldl148039, %struct.ScmObj** %stackaddr$prim63023, align 8
%ae49116 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure63024 = alloca %struct.ScmObj*, align 8
%fptrToInt63025 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49117 to i64
%ae49117 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt63025)
store volatile %struct.ScmObj* %ae49117, %struct.ScmObj** %stackaddr$makeclosure63024, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49117, %struct.ScmObj* %_37foldl148039, i64 0)
%argslist60872$k485420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim63026 = alloca %struct.ScmObj*, align 8
%argslist60872$k485421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49117, %struct.ScmObj* %argslist60872$k485420)
store volatile %struct.ScmObj* %argslist60872$k485421, %struct.ScmObj** %stackaddr$prim63026, align 8
%stackaddr$prim63027 = alloca %struct.ScmObj*, align 8
%argslist60872$k485422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49116, %struct.ScmObj* %argslist60872$k485421)
store volatile %struct.ScmObj* %argslist60872$k485422, %struct.ScmObj** %stackaddr$prim63027, align 8
%clofunc63028 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48542)
musttail call tailcc void %clofunc63028(%struct.ScmObj* %k48542, %struct.ScmObj* %argslist60872$k485422)
ret void
}

define tailcc void @proc_clo$ae49117(%struct.ScmObj* %env$ae49117,%struct.ScmObj* %current_45args60861) {
%stackaddr$env-ref63029 = alloca %struct.ScmObj*, align 8
%_37foldl148039 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49117, i64 0)
store %struct.ScmObj* %_37foldl148039, %struct.ScmObj** %stackaddr$env-ref63029
%stackaddr$prim63030 = alloca %struct.ScmObj*, align 8
%k48543 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60861)
store volatile %struct.ScmObj* %k48543, %struct.ScmObj** %stackaddr$prim63030, align 8
%stackaddr$prim63031 = alloca %struct.ScmObj*, align 8
%current_45args60862 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60861)
store volatile %struct.ScmObj* %current_45args60862, %struct.ScmObj** %stackaddr$prim63031, align 8
%stackaddr$prim63032 = alloca %struct.ScmObj*, align 8
%f48042 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60862)
store volatile %struct.ScmObj* %f48042, %struct.ScmObj** %stackaddr$prim63032, align 8
%stackaddr$prim63033 = alloca %struct.ScmObj*, align 8
%current_45args60863 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60862)
store volatile %struct.ScmObj* %current_45args60863, %struct.ScmObj** %stackaddr$prim63033, align 8
%stackaddr$prim63034 = alloca %struct.ScmObj*, align 8
%acc48041 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60863)
store volatile %struct.ScmObj* %acc48041, %struct.ScmObj** %stackaddr$prim63034, align 8
%stackaddr$prim63035 = alloca %struct.ScmObj*, align 8
%current_45args60864 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60863)
store volatile %struct.ScmObj* %current_45args60864, %struct.ScmObj** %stackaddr$prim63035, align 8
%stackaddr$prim63036 = alloca %struct.ScmObj*, align 8
%lst48040 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60864)
store volatile %struct.ScmObj* %lst48040, %struct.ScmObj** %stackaddr$prim63036, align 8
%stackaddr$prim63037 = alloca %struct.ScmObj*, align 8
%anf_45bind48208 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48040)
store volatile %struct.ScmObj* %anf_45bind48208, %struct.ScmObj** %stackaddr$prim63037, align 8
%truthy$cmp63038 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48208)
%cmp$cmp63038 = icmp eq i64 %truthy$cmp63038, 1
br i1 %cmp$cmp63038, label %truebranch$cmp63038, label %falsebranch$cmp63038
truebranch$cmp63038:
%ae49121 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist60866$k485430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim63039 = alloca %struct.ScmObj*, align 8
%argslist60866$k485431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48041, %struct.ScmObj* %argslist60866$k485430)
store volatile %struct.ScmObj* %argslist60866$k485431, %struct.ScmObj** %stackaddr$prim63039, align 8
%stackaddr$prim63040 = alloca %struct.ScmObj*, align 8
%argslist60866$k485432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49121, %struct.ScmObj* %argslist60866$k485431)
store volatile %struct.ScmObj* %argslist60866$k485432, %struct.ScmObj** %stackaddr$prim63040, align 8
%clofunc63041 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48543)
musttail call tailcc void %clofunc63041(%struct.ScmObj* %k48543, %struct.ScmObj* %argslist60866$k485432)
ret void
falsebranch$cmp63038:
%stackaddr$prim63042 = alloca %struct.ScmObj*, align 8
%anf_45bind48209 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48040)
store volatile %struct.ScmObj* %anf_45bind48209, %struct.ScmObj** %stackaddr$prim63042, align 8
%stackaddr$makeclosure63043 = alloca %struct.ScmObj*, align 8
%fptrToInt63044 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49128 to i64
%ae49128 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt63044)
store volatile %struct.ScmObj* %ae49128, %struct.ScmObj** %stackaddr$makeclosure63043, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49128, %struct.ScmObj* %lst48040, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49128, %struct.ScmObj* %_37foldl148039, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49128, %struct.ScmObj* %k48543, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49128, %struct.ScmObj* %f48042, i64 3)
%argslist60871$f480420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim63045 = alloca %struct.ScmObj*, align 8
%argslist60871$f480421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48041, %struct.ScmObj* %argslist60871$f480420)
store volatile %struct.ScmObj* %argslist60871$f480421, %struct.ScmObj** %stackaddr$prim63045, align 8
%stackaddr$prim63046 = alloca %struct.ScmObj*, align 8
%argslist60871$f480422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48209, %struct.ScmObj* %argslist60871$f480421)
store volatile %struct.ScmObj* %argslist60871$f480422, %struct.ScmObj** %stackaddr$prim63046, align 8
%stackaddr$prim63047 = alloca %struct.ScmObj*, align 8
%argslist60871$f480423 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49128, %struct.ScmObj* %argslist60871$f480422)
store volatile %struct.ScmObj* %argslist60871$f480423, %struct.ScmObj** %stackaddr$prim63047, align 8
%clofunc63048 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48042)
musttail call tailcc void %clofunc63048(%struct.ScmObj* %f48042, %struct.ScmObj* %argslist60871$f480423)
ret void
}

define tailcc void @proc_clo$ae49128(%struct.ScmObj* %env$ae49128,%struct.ScmObj* %current_45args60867) {
%stackaddr$env-ref63049 = alloca %struct.ScmObj*, align 8
%lst48040 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49128, i64 0)
store %struct.ScmObj* %lst48040, %struct.ScmObj** %stackaddr$env-ref63049
%stackaddr$env-ref63050 = alloca %struct.ScmObj*, align 8
%_37foldl148039 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49128, i64 1)
store %struct.ScmObj* %_37foldl148039, %struct.ScmObj** %stackaddr$env-ref63050
%stackaddr$env-ref63051 = alloca %struct.ScmObj*, align 8
%k48543 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49128, i64 2)
store %struct.ScmObj* %k48543, %struct.ScmObj** %stackaddr$env-ref63051
%stackaddr$env-ref63052 = alloca %struct.ScmObj*, align 8
%f48042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49128, i64 3)
store %struct.ScmObj* %f48042, %struct.ScmObj** %stackaddr$env-ref63052
%stackaddr$prim63053 = alloca %struct.ScmObj*, align 8
%_95k48544 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60867)
store volatile %struct.ScmObj* %_95k48544, %struct.ScmObj** %stackaddr$prim63053, align 8
%stackaddr$prim63054 = alloca %struct.ScmObj*, align 8
%current_45args60868 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60867)
store volatile %struct.ScmObj* %current_45args60868, %struct.ScmObj** %stackaddr$prim63054, align 8
%stackaddr$prim63055 = alloca %struct.ScmObj*, align 8
%anf_45bind48210 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60868)
store volatile %struct.ScmObj* %anf_45bind48210, %struct.ScmObj** %stackaddr$prim63055, align 8
%stackaddr$prim63056 = alloca %struct.ScmObj*, align 8
%anf_45bind48211 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48040)
store volatile %struct.ScmObj* %anf_45bind48211, %struct.ScmObj** %stackaddr$prim63056, align 8
%argslist60870$_37foldl1480390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim63057 = alloca %struct.ScmObj*, align 8
%argslist60870$_37foldl1480391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48211, %struct.ScmObj* %argslist60870$_37foldl1480390)
store volatile %struct.ScmObj* %argslist60870$_37foldl1480391, %struct.ScmObj** %stackaddr$prim63057, align 8
%stackaddr$prim63058 = alloca %struct.ScmObj*, align 8
%argslist60870$_37foldl1480392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48210, %struct.ScmObj* %argslist60870$_37foldl1480391)
store volatile %struct.ScmObj* %argslist60870$_37foldl1480392, %struct.ScmObj** %stackaddr$prim63058, align 8
%stackaddr$prim63059 = alloca %struct.ScmObj*, align 8
%argslist60870$_37foldl1480393 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48042, %struct.ScmObj* %argslist60870$_37foldl1480392)
store volatile %struct.ScmObj* %argslist60870$_37foldl1480393, %struct.ScmObj** %stackaddr$prim63059, align 8
%stackaddr$prim63060 = alloca %struct.ScmObj*, align 8
%argslist60870$_37foldl1480394 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48543, %struct.ScmObj* %argslist60870$_37foldl1480393)
store volatile %struct.ScmObj* %argslist60870$_37foldl1480394, %struct.ScmObj** %stackaddr$prim63060, align 8
%clofunc63061 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148039)
musttail call tailcc void %clofunc63061(%struct.ScmObj* %_37foldl148039, %struct.ScmObj* %argslist60870$_37foldl1480394)
ret void
}

define tailcc void @proc_clo$ae49031(%struct.ScmObj* %env$ae49031,%struct.ScmObj* %current_45args60875) {
%stackaddr$prim63062 = alloca %struct.ScmObj*, align 8
%k48545 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60875)
store volatile %struct.ScmObj* %k48545, %struct.ScmObj** %stackaddr$prim63062, align 8
%stackaddr$prim63063 = alloca %struct.ScmObj*, align 8
%current_45args60876 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60875)
store volatile %struct.ScmObj* %current_45args60876, %struct.ScmObj** %stackaddr$prim63063, align 8
%stackaddr$prim63064 = alloca %struct.ScmObj*, align 8
%_37length48044 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60876)
store volatile %struct.ScmObj* %_37length48044, %struct.ScmObj** %stackaddr$prim63064, align 8
%ae49033 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure63065 = alloca %struct.ScmObj*, align 8
%fptrToInt63066 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49034 to i64
%ae49034 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt63066)
store volatile %struct.ScmObj* %ae49034, %struct.ScmObj** %stackaddr$makeclosure63065, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49034, %struct.ScmObj* %_37length48044, i64 0)
%argslist60887$k485450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim63067 = alloca %struct.ScmObj*, align 8
%argslist60887$k485451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49034, %struct.ScmObj* %argslist60887$k485450)
store volatile %struct.ScmObj* %argslist60887$k485451, %struct.ScmObj** %stackaddr$prim63067, align 8
%stackaddr$prim63068 = alloca %struct.ScmObj*, align 8
%argslist60887$k485452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49033, %struct.ScmObj* %argslist60887$k485451)
store volatile %struct.ScmObj* %argslist60887$k485452, %struct.ScmObj** %stackaddr$prim63068, align 8
%clofunc63069 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48545)
musttail call tailcc void %clofunc63069(%struct.ScmObj* %k48545, %struct.ScmObj* %argslist60887$k485452)
ret void
}

define tailcc void @proc_clo$ae49034(%struct.ScmObj* %env$ae49034,%struct.ScmObj* %current_45args60878) {
%stackaddr$env-ref63070 = alloca %struct.ScmObj*, align 8
%_37length48044 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49034, i64 0)
store %struct.ScmObj* %_37length48044, %struct.ScmObj** %stackaddr$env-ref63070
%stackaddr$prim63071 = alloca %struct.ScmObj*, align 8
%k48546 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60878)
store volatile %struct.ScmObj* %k48546, %struct.ScmObj** %stackaddr$prim63071, align 8
%stackaddr$prim63072 = alloca %struct.ScmObj*, align 8
%current_45args60879 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60878)
store volatile %struct.ScmObj* %current_45args60879, %struct.ScmObj** %stackaddr$prim63072, align 8
%stackaddr$prim63073 = alloca %struct.ScmObj*, align 8
%lst48045 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60879)
store volatile %struct.ScmObj* %lst48045, %struct.ScmObj** %stackaddr$prim63073, align 8
%stackaddr$prim63074 = alloca %struct.ScmObj*, align 8
%anf_45bind48204 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48045)
store volatile %struct.ScmObj* %anf_45bind48204, %struct.ScmObj** %stackaddr$prim63074, align 8
%truthy$cmp63075 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48204)
%cmp$cmp63075 = icmp eq i64 %truthy$cmp63075, 1
br i1 %cmp$cmp63075, label %truebranch$cmp63075, label %falsebranch$cmp63075
truebranch$cmp63075:
%ae49038 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49039 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist60881$k485460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim63076 = alloca %struct.ScmObj*, align 8
%argslist60881$k485461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49039, %struct.ScmObj* %argslist60881$k485460)
store volatile %struct.ScmObj* %argslist60881$k485461, %struct.ScmObj** %stackaddr$prim63076, align 8
%stackaddr$prim63077 = alloca %struct.ScmObj*, align 8
%argslist60881$k485462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49038, %struct.ScmObj* %argslist60881$k485461)
store volatile %struct.ScmObj* %argslist60881$k485462, %struct.ScmObj** %stackaddr$prim63077, align 8
%clofunc63078 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48546)
musttail call tailcc void %clofunc63078(%struct.ScmObj* %k48546, %struct.ScmObj* %argslist60881$k485462)
ret void
falsebranch$cmp63075:
%stackaddr$prim63079 = alloca %struct.ScmObj*, align 8
%anf_45bind48205 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48045)
store volatile %struct.ScmObj* %anf_45bind48205, %struct.ScmObj** %stackaddr$prim63079, align 8
%stackaddr$makeclosure63080 = alloca %struct.ScmObj*, align 8
%fptrToInt63081 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49048 to i64
%ae49048 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt63081)
store volatile %struct.ScmObj* %ae49048, %struct.ScmObj** %stackaddr$makeclosure63080, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49048, %struct.ScmObj* %k48546, i64 0)
%argslist60886$_37length480440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim63082 = alloca %struct.ScmObj*, align 8
%argslist60886$_37length480441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48205, %struct.ScmObj* %argslist60886$_37length480440)
store volatile %struct.ScmObj* %argslist60886$_37length480441, %struct.ScmObj** %stackaddr$prim63082, align 8
%stackaddr$prim63083 = alloca %struct.ScmObj*, align 8
%argslist60886$_37length480442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49048, %struct.ScmObj* %argslist60886$_37length480441)
store volatile %struct.ScmObj* %argslist60886$_37length480442, %struct.ScmObj** %stackaddr$prim63083, align 8
%clofunc63084 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length48044)
musttail call tailcc void %clofunc63084(%struct.ScmObj* %_37length48044, %struct.ScmObj* %argslist60886$_37length480442)
ret void
}

define tailcc void @proc_clo$ae49048(%struct.ScmObj* %env$ae49048,%struct.ScmObj* %current_45args60882) {
%stackaddr$env-ref63085 = alloca %struct.ScmObj*, align 8
%k48546 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49048, i64 0)
store %struct.ScmObj* %k48546, %struct.ScmObj** %stackaddr$env-ref63085
%stackaddr$prim63086 = alloca %struct.ScmObj*, align 8
%_95k48547 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60882)
store volatile %struct.ScmObj* %_95k48547, %struct.ScmObj** %stackaddr$prim63086, align 8
%stackaddr$prim63087 = alloca %struct.ScmObj*, align 8
%current_45args60883 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60882)
store volatile %struct.ScmObj* %current_45args60883, %struct.ScmObj** %stackaddr$prim63087, align 8
%stackaddr$prim63088 = alloca %struct.ScmObj*, align 8
%anf_45bind48206 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60883)
store volatile %struct.ScmObj* %anf_45bind48206, %struct.ScmObj** %stackaddr$prim63088, align 8
%ae49050 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim63089 = alloca %struct.ScmObj*, align 8
%cpsprim48548 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae49050, %struct.ScmObj* %anf_45bind48206)
store volatile %struct.ScmObj* %cpsprim48548, %struct.ScmObj** %stackaddr$prim63089, align 8
%ae49053 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist60885$k485460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim63090 = alloca %struct.ScmObj*, align 8
%argslist60885$k485461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48548, %struct.ScmObj* %argslist60885$k485460)
store volatile %struct.ScmObj* %argslist60885$k485461, %struct.ScmObj** %stackaddr$prim63090, align 8
%stackaddr$prim63091 = alloca %struct.ScmObj*, align 8
%argslist60885$k485462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49053, %struct.ScmObj* %argslist60885$k485461)
store volatile %struct.ScmObj* %argslist60885$k485462, %struct.ScmObj** %stackaddr$prim63091, align 8
%clofunc63092 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48546)
musttail call tailcc void %clofunc63092(%struct.ScmObj* %k48546, %struct.ScmObj* %argslist60885$k485462)
ret void
}

define tailcc void @proc_clo$ae48881(%struct.ScmObj* %env$ae48881,%struct.ScmObj* %current_45args60890) {
%stackaddr$prim63093 = alloca %struct.ScmObj*, align 8
%k48549 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60890)
store volatile %struct.ScmObj* %k48549, %struct.ScmObj** %stackaddr$prim63093, align 8
%stackaddr$prim63094 = alloca %struct.ScmObj*, align 8
%current_45args60891 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60890)
store volatile %struct.ScmObj* %current_45args60891, %struct.ScmObj** %stackaddr$prim63094, align 8
%stackaddr$prim63095 = alloca %struct.ScmObj*, align 8
%_37take48047 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60891)
store volatile %struct.ScmObj* %_37take48047, %struct.ScmObj** %stackaddr$prim63095, align 8
%ae48883 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure63096 = alloca %struct.ScmObj*, align 8
%fptrToInt63097 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48884 to i64
%ae48884 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt63097)
store volatile %struct.ScmObj* %ae48884, %struct.ScmObj** %stackaddr$makeclosure63096, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48884, %struct.ScmObj* %_37take48047, i64 0)
%argslist60904$k485490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim63098 = alloca %struct.ScmObj*, align 8
%argslist60904$k485491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48884, %struct.ScmObj* %argslist60904$k485490)
store volatile %struct.ScmObj* %argslist60904$k485491, %struct.ScmObj** %stackaddr$prim63098, align 8
%stackaddr$prim63099 = alloca %struct.ScmObj*, align 8
%argslist60904$k485492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48883, %struct.ScmObj* %argslist60904$k485491)
store volatile %struct.ScmObj* %argslist60904$k485492, %struct.ScmObj** %stackaddr$prim63099, align 8
%clofunc63100 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48549)
musttail call tailcc void %clofunc63100(%struct.ScmObj* %k48549, %struct.ScmObj* %argslist60904$k485492)
ret void
}

define tailcc void @proc_clo$ae48884(%struct.ScmObj* %env$ae48884,%struct.ScmObj* %current_45args60893) {
%stackaddr$env-ref63101 = alloca %struct.ScmObj*, align 8
%_37take48047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48884, i64 0)
store %struct.ScmObj* %_37take48047, %struct.ScmObj** %stackaddr$env-ref63101
%stackaddr$prim63102 = alloca %struct.ScmObj*, align 8
%k48550 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60893)
store volatile %struct.ScmObj* %k48550, %struct.ScmObj** %stackaddr$prim63102, align 8
%stackaddr$prim63103 = alloca %struct.ScmObj*, align 8
%current_45args60894 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60893)
store volatile %struct.ScmObj* %current_45args60894, %struct.ScmObj** %stackaddr$prim63103, align 8
%stackaddr$prim63104 = alloca %struct.ScmObj*, align 8
%lst48049 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60894)
store volatile %struct.ScmObj* %lst48049, %struct.ScmObj** %stackaddr$prim63104, align 8
%stackaddr$prim63105 = alloca %struct.ScmObj*, align 8
%current_45args60895 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60894)
store volatile %struct.ScmObj* %current_45args60895, %struct.ScmObj** %stackaddr$prim63105, align 8
%stackaddr$prim63106 = alloca %struct.ScmObj*, align 8
%n48048 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60895)
store volatile %struct.ScmObj* %n48048, %struct.ScmObj** %stackaddr$prim63106, align 8
%ae48886 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim63107 = alloca %struct.ScmObj*, align 8
%anf_45bind48197 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n48048, %struct.ScmObj* %ae48886)
store volatile %struct.ScmObj* %anf_45bind48197, %struct.ScmObj** %stackaddr$prim63107, align 8
%truthy$cmp63108 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48197)
%cmp$cmp63108 = icmp eq i64 %truthy$cmp63108, 1
br i1 %cmp$cmp63108, label %truebranch$cmp63108, label %falsebranch$cmp63108
truebranch$cmp63108:
%ae48889 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48890 = call %struct.ScmObj* @const_init_null()
%argslist60897$k485500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim63109 = alloca %struct.ScmObj*, align 8
%argslist60897$k485501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48890, %struct.ScmObj* %argslist60897$k485500)
store volatile %struct.ScmObj* %argslist60897$k485501, %struct.ScmObj** %stackaddr$prim63109, align 8
%stackaddr$prim63110 = alloca %struct.ScmObj*, align 8
%argslist60897$k485502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48889, %struct.ScmObj* %argslist60897$k485501)
store volatile %struct.ScmObj* %argslist60897$k485502, %struct.ScmObj** %stackaddr$prim63110, align 8
%clofunc63111 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48550)
musttail call tailcc void %clofunc63111(%struct.ScmObj* %k48550, %struct.ScmObj* %argslist60897$k485502)
ret void
falsebranch$cmp63108:
%stackaddr$prim63112 = alloca %struct.ScmObj*, align 8
%anf_45bind48198 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48049)
store volatile %struct.ScmObj* %anf_45bind48198, %struct.ScmObj** %stackaddr$prim63112, align 8
%truthy$cmp63113 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48198)
%cmp$cmp63113 = icmp eq i64 %truthy$cmp63113, 1
br i1 %cmp$cmp63113, label %truebranch$cmp63113, label %falsebranch$cmp63113
truebranch$cmp63113:
%ae48900 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48901 = call %struct.ScmObj* @const_init_null()
%argslist60898$k485500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim63114 = alloca %struct.ScmObj*, align 8
%argslist60898$k485501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48901, %struct.ScmObj* %argslist60898$k485500)
store volatile %struct.ScmObj* %argslist60898$k485501, %struct.ScmObj** %stackaddr$prim63114, align 8
%stackaddr$prim63115 = alloca %struct.ScmObj*, align 8
%argslist60898$k485502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48900, %struct.ScmObj* %argslist60898$k485501)
store volatile %struct.ScmObj* %argslist60898$k485502, %struct.ScmObj** %stackaddr$prim63115, align 8
%clofunc63116 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48550)
musttail call tailcc void %clofunc63116(%struct.ScmObj* %k48550, %struct.ScmObj* %argslist60898$k485502)
ret void
falsebranch$cmp63113:
%stackaddr$prim63117 = alloca %struct.ScmObj*, align 8
%anf_45bind48199 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48049)
store volatile %struct.ScmObj* %anf_45bind48199, %struct.ScmObj** %stackaddr$prim63117, align 8
%stackaddr$prim63118 = alloca %struct.ScmObj*, align 8
%anf_45bind48200 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48049)
store volatile %struct.ScmObj* %anf_45bind48200, %struct.ScmObj** %stackaddr$prim63118, align 8
%ae48911 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim63119 = alloca %struct.ScmObj*, align 8
%anf_45bind48201 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n48048, %struct.ScmObj* %ae48911)
store volatile %struct.ScmObj* %anf_45bind48201, %struct.ScmObj** %stackaddr$prim63119, align 8
%stackaddr$makeclosure63120 = alloca %struct.ScmObj*, align 8
%fptrToInt63121 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48913 to i64
%ae48913 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt63121)
store volatile %struct.ScmObj* %ae48913, %struct.ScmObj** %stackaddr$makeclosure63120, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48913, %struct.ScmObj* %anf_45bind48199, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48913, %struct.ScmObj* %k48550, i64 1)
%argslist60903$_37take480470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim63122 = alloca %struct.ScmObj*, align 8
%argslist60903$_37take480471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48201, %struct.ScmObj* %argslist60903$_37take480470)
store volatile %struct.ScmObj* %argslist60903$_37take480471, %struct.ScmObj** %stackaddr$prim63122, align 8
%stackaddr$prim63123 = alloca %struct.ScmObj*, align 8
%argslist60903$_37take480472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48200, %struct.ScmObj* %argslist60903$_37take480471)
store volatile %struct.ScmObj* %argslist60903$_37take480472, %struct.ScmObj** %stackaddr$prim63123, align 8
%stackaddr$prim63124 = alloca %struct.ScmObj*, align 8
%argslist60903$_37take480473 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48913, %struct.ScmObj* %argslist60903$_37take480472)
store volatile %struct.ScmObj* %argslist60903$_37take480473, %struct.ScmObj** %stackaddr$prim63124, align 8
%clofunc63125 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take48047)
musttail call tailcc void %clofunc63125(%struct.ScmObj* %_37take48047, %struct.ScmObj* %argslist60903$_37take480473)
ret void
}

define tailcc void @proc_clo$ae48913(%struct.ScmObj* %env$ae48913,%struct.ScmObj* %current_45args60899) {
%stackaddr$env-ref63126 = alloca %struct.ScmObj*, align 8
%anf_45bind48199 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48913, i64 0)
store %struct.ScmObj* %anf_45bind48199, %struct.ScmObj** %stackaddr$env-ref63126
%stackaddr$env-ref63127 = alloca %struct.ScmObj*, align 8
%k48550 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48913, i64 1)
store %struct.ScmObj* %k48550, %struct.ScmObj** %stackaddr$env-ref63127
%stackaddr$prim63128 = alloca %struct.ScmObj*, align 8
%_95k48551 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60899)
store volatile %struct.ScmObj* %_95k48551, %struct.ScmObj** %stackaddr$prim63128, align 8
%stackaddr$prim63129 = alloca %struct.ScmObj*, align 8
%current_45args60900 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60899)
store volatile %struct.ScmObj* %current_45args60900, %struct.ScmObj** %stackaddr$prim63129, align 8
%stackaddr$prim63130 = alloca %struct.ScmObj*, align 8
%anf_45bind48202 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60900)
store volatile %struct.ScmObj* %anf_45bind48202, %struct.ScmObj** %stackaddr$prim63130, align 8
%stackaddr$prim63131 = alloca %struct.ScmObj*, align 8
%cpsprim48552 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48199, %struct.ScmObj* %anf_45bind48202)
store volatile %struct.ScmObj* %cpsprim48552, %struct.ScmObj** %stackaddr$prim63131, align 8
%ae48919 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist60902$k485500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim63132 = alloca %struct.ScmObj*, align 8
%argslist60902$k485501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48552, %struct.ScmObj* %argslist60902$k485500)
store volatile %struct.ScmObj* %argslist60902$k485501, %struct.ScmObj** %stackaddr$prim63132, align 8
%stackaddr$prim63133 = alloca %struct.ScmObj*, align 8
%argslist60902$k485502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48919, %struct.ScmObj* %argslist60902$k485501)
store volatile %struct.ScmObj* %argslist60902$k485502, %struct.ScmObj** %stackaddr$prim63133, align 8
%clofunc63134 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48550)
musttail call tailcc void %clofunc63134(%struct.ScmObj* %k48550, %struct.ScmObj* %argslist60902$k485502)
ret void
}

define tailcc void @proc_clo$ae48784(%struct.ScmObj* %env$ae48784,%struct.ScmObj* %current_45args60907) {
%stackaddr$prim63135 = alloca %struct.ScmObj*, align 8
%k48553 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60907)
store volatile %struct.ScmObj* %k48553, %struct.ScmObj** %stackaddr$prim63135, align 8
%stackaddr$prim63136 = alloca %struct.ScmObj*, align 8
%current_45args60908 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60907)
store volatile %struct.ScmObj* %current_45args60908, %struct.ScmObj** %stackaddr$prim63136, align 8
%stackaddr$prim63137 = alloca %struct.ScmObj*, align 8
%_37map48051 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60908)
store volatile %struct.ScmObj* %_37map48051, %struct.ScmObj** %stackaddr$prim63137, align 8
%ae48786 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure63138 = alloca %struct.ScmObj*, align 8
%fptrToInt63139 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48787 to i64
%ae48787 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt63139)
store volatile %struct.ScmObj* %ae48787, %struct.ScmObj** %stackaddr$makeclosure63138, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48787, %struct.ScmObj* %_37map48051, i64 0)
%argslist60924$k485530 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim63140 = alloca %struct.ScmObj*, align 8
%argslist60924$k485531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48787, %struct.ScmObj* %argslist60924$k485530)
store volatile %struct.ScmObj* %argslist60924$k485531, %struct.ScmObj** %stackaddr$prim63140, align 8
%stackaddr$prim63141 = alloca %struct.ScmObj*, align 8
%argslist60924$k485532 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48786, %struct.ScmObj* %argslist60924$k485531)
store volatile %struct.ScmObj* %argslist60924$k485532, %struct.ScmObj** %stackaddr$prim63141, align 8
%clofunc63142 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48553)
musttail call tailcc void %clofunc63142(%struct.ScmObj* %k48553, %struct.ScmObj* %argslist60924$k485532)
ret void
}

define tailcc void @proc_clo$ae48787(%struct.ScmObj* %env$ae48787,%struct.ScmObj* %current_45args60910) {
%stackaddr$env-ref63143 = alloca %struct.ScmObj*, align 8
%_37map48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48787, i64 0)
store %struct.ScmObj* %_37map48051, %struct.ScmObj** %stackaddr$env-ref63143
%stackaddr$prim63144 = alloca %struct.ScmObj*, align 8
%k48554 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60910)
store volatile %struct.ScmObj* %k48554, %struct.ScmObj** %stackaddr$prim63144, align 8
%stackaddr$prim63145 = alloca %struct.ScmObj*, align 8
%current_45args60911 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60910)
store volatile %struct.ScmObj* %current_45args60911, %struct.ScmObj** %stackaddr$prim63145, align 8
%stackaddr$prim63146 = alloca %struct.ScmObj*, align 8
%f48053 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60911)
store volatile %struct.ScmObj* %f48053, %struct.ScmObj** %stackaddr$prim63146, align 8
%stackaddr$prim63147 = alloca %struct.ScmObj*, align 8
%current_45args60912 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60911)
store volatile %struct.ScmObj* %current_45args60912, %struct.ScmObj** %stackaddr$prim63147, align 8
%stackaddr$prim63148 = alloca %struct.ScmObj*, align 8
%lst48052 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60912)
store volatile %struct.ScmObj* %lst48052, %struct.ScmObj** %stackaddr$prim63148, align 8
%stackaddr$prim63149 = alloca %struct.ScmObj*, align 8
%anf_45bind48191 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48052)
store volatile %struct.ScmObj* %anf_45bind48191, %struct.ScmObj** %stackaddr$prim63149, align 8
%truthy$cmp63150 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48191)
%cmp$cmp63150 = icmp eq i64 %truthy$cmp63150, 1
br i1 %cmp$cmp63150, label %truebranch$cmp63150, label %falsebranch$cmp63150
truebranch$cmp63150:
%ae48791 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48792 = call %struct.ScmObj* @const_init_null()
%argslist60914$k485540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim63151 = alloca %struct.ScmObj*, align 8
%argslist60914$k485541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48792, %struct.ScmObj* %argslist60914$k485540)
store volatile %struct.ScmObj* %argslist60914$k485541, %struct.ScmObj** %stackaddr$prim63151, align 8
%stackaddr$prim63152 = alloca %struct.ScmObj*, align 8
%argslist60914$k485542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48791, %struct.ScmObj* %argslist60914$k485541)
store volatile %struct.ScmObj* %argslist60914$k485542, %struct.ScmObj** %stackaddr$prim63152, align 8
%clofunc63153 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48554)
musttail call tailcc void %clofunc63153(%struct.ScmObj* %k48554, %struct.ScmObj* %argslist60914$k485542)
ret void
falsebranch$cmp63150:
%stackaddr$prim63154 = alloca %struct.ScmObj*, align 8
%anf_45bind48192 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48052)
store volatile %struct.ScmObj* %anf_45bind48192, %struct.ScmObj** %stackaddr$prim63154, align 8
%stackaddr$makeclosure63155 = alloca %struct.ScmObj*, align 8
%fptrToInt63156 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48801 to i64
%ae48801 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt63156)
store volatile %struct.ScmObj* %ae48801, %struct.ScmObj** %stackaddr$makeclosure63155, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48801, %struct.ScmObj* %f48053, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48801, %struct.ScmObj* %lst48052, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48801, %struct.ScmObj* %_37map48051, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48801, %struct.ScmObj* %k48554, i64 3)
%argslist60923$f480530 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim63157 = alloca %struct.ScmObj*, align 8
%argslist60923$f480531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48192, %struct.ScmObj* %argslist60923$f480530)
store volatile %struct.ScmObj* %argslist60923$f480531, %struct.ScmObj** %stackaddr$prim63157, align 8
%stackaddr$prim63158 = alloca %struct.ScmObj*, align 8
%argslist60923$f480532 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48801, %struct.ScmObj* %argslist60923$f480531)
store volatile %struct.ScmObj* %argslist60923$f480532, %struct.ScmObj** %stackaddr$prim63158, align 8
%clofunc63159 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48053)
musttail call tailcc void %clofunc63159(%struct.ScmObj* %f48053, %struct.ScmObj* %argslist60923$f480532)
ret void
}

define tailcc void @proc_clo$ae48801(%struct.ScmObj* %env$ae48801,%struct.ScmObj* %current_45args60915) {
%stackaddr$env-ref63160 = alloca %struct.ScmObj*, align 8
%f48053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48801, i64 0)
store %struct.ScmObj* %f48053, %struct.ScmObj** %stackaddr$env-ref63160
%stackaddr$env-ref63161 = alloca %struct.ScmObj*, align 8
%lst48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48801, i64 1)
store %struct.ScmObj* %lst48052, %struct.ScmObj** %stackaddr$env-ref63161
%stackaddr$env-ref63162 = alloca %struct.ScmObj*, align 8
%_37map48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48801, i64 2)
store %struct.ScmObj* %_37map48051, %struct.ScmObj** %stackaddr$env-ref63162
%stackaddr$env-ref63163 = alloca %struct.ScmObj*, align 8
%k48554 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48801, i64 3)
store %struct.ScmObj* %k48554, %struct.ScmObj** %stackaddr$env-ref63163
%stackaddr$prim63164 = alloca %struct.ScmObj*, align 8
%_95k48555 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60915)
store volatile %struct.ScmObj* %_95k48555, %struct.ScmObj** %stackaddr$prim63164, align 8
%stackaddr$prim63165 = alloca %struct.ScmObj*, align 8
%current_45args60916 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60915)
store volatile %struct.ScmObj* %current_45args60916, %struct.ScmObj** %stackaddr$prim63165, align 8
%stackaddr$prim63166 = alloca %struct.ScmObj*, align 8
%anf_45bind48193 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60916)
store volatile %struct.ScmObj* %anf_45bind48193, %struct.ScmObj** %stackaddr$prim63166, align 8
%stackaddr$prim63167 = alloca %struct.ScmObj*, align 8
%anf_45bind48194 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48052)
store volatile %struct.ScmObj* %anf_45bind48194, %struct.ScmObj** %stackaddr$prim63167, align 8
%stackaddr$makeclosure63168 = alloca %struct.ScmObj*, align 8
%fptrToInt63169 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48805 to i64
%ae48805 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt63169)
store volatile %struct.ScmObj* %ae48805, %struct.ScmObj** %stackaddr$makeclosure63168, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48805, %struct.ScmObj* %anf_45bind48193, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48805, %struct.ScmObj* %k48554, i64 1)
%argslist60922$_37map480510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim63170 = alloca %struct.ScmObj*, align 8
%argslist60922$_37map480511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48194, %struct.ScmObj* %argslist60922$_37map480510)
store volatile %struct.ScmObj* %argslist60922$_37map480511, %struct.ScmObj** %stackaddr$prim63170, align 8
%stackaddr$prim63171 = alloca %struct.ScmObj*, align 8
%argslist60922$_37map480512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48053, %struct.ScmObj* %argslist60922$_37map480511)
store volatile %struct.ScmObj* %argslist60922$_37map480512, %struct.ScmObj** %stackaddr$prim63171, align 8
%stackaddr$prim63172 = alloca %struct.ScmObj*, align 8
%argslist60922$_37map480513 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48805, %struct.ScmObj* %argslist60922$_37map480512)
store volatile %struct.ScmObj* %argslist60922$_37map480513, %struct.ScmObj** %stackaddr$prim63172, align 8
%clofunc63173 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map48051)
musttail call tailcc void %clofunc63173(%struct.ScmObj* %_37map48051, %struct.ScmObj* %argslist60922$_37map480513)
ret void
}

define tailcc void @proc_clo$ae48805(%struct.ScmObj* %env$ae48805,%struct.ScmObj* %current_45args60918) {
%stackaddr$env-ref63174 = alloca %struct.ScmObj*, align 8
%anf_45bind48193 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48805, i64 0)
store %struct.ScmObj* %anf_45bind48193, %struct.ScmObj** %stackaddr$env-ref63174
%stackaddr$env-ref63175 = alloca %struct.ScmObj*, align 8
%k48554 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48805, i64 1)
store %struct.ScmObj* %k48554, %struct.ScmObj** %stackaddr$env-ref63175
%stackaddr$prim63176 = alloca %struct.ScmObj*, align 8
%_95k48556 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60918)
store volatile %struct.ScmObj* %_95k48556, %struct.ScmObj** %stackaddr$prim63176, align 8
%stackaddr$prim63177 = alloca %struct.ScmObj*, align 8
%current_45args60919 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60918)
store volatile %struct.ScmObj* %current_45args60919, %struct.ScmObj** %stackaddr$prim63177, align 8
%stackaddr$prim63178 = alloca %struct.ScmObj*, align 8
%anf_45bind48195 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60919)
store volatile %struct.ScmObj* %anf_45bind48195, %struct.ScmObj** %stackaddr$prim63178, align 8
%stackaddr$prim63179 = alloca %struct.ScmObj*, align 8
%cpsprim48557 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48193, %struct.ScmObj* %anf_45bind48195)
store volatile %struct.ScmObj* %cpsprim48557, %struct.ScmObj** %stackaddr$prim63179, align 8
%ae48811 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist60921$k485540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim63180 = alloca %struct.ScmObj*, align 8
%argslist60921$k485541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48557, %struct.ScmObj* %argslist60921$k485540)
store volatile %struct.ScmObj* %argslist60921$k485541, %struct.ScmObj** %stackaddr$prim63180, align 8
%stackaddr$prim63181 = alloca %struct.ScmObj*, align 8
%argslist60921$k485542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48811, %struct.ScmObj* %argslist60921$k485541)
store volatile %struct.ScmObj* %argslist60921$k485542, %struct.ScmObj** %stackaddr$prim63181, align 8
%clofunc63182 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48554)
musttail call tailcc void %clofunc63182(%struct.ScmObj* %k48554, %struct.ScmObj* %argslist60921$k485542)
ret void
}

define tailcc void @proc_clo$ae48704(%struct.ScmObj* %env$ae48704,%struct.ScmObj* %current_45args60927) {
%stackaddr$prim63183 = alloca %struct.ScmObj*, align 8
%k48558 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60927)
store volatile %struct.ScmObj* %k48558, %struct.ScmObj** %stackaddr$prim63183, align 8
%stackaddr$prim63184 = alloca %struct.ScmObj*, align 8
%current_45args60928 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60927)
store volatile %struct.ScmObj* %current_45args60928, %struct.ScmObj** %stackaddr$prim63184, align 8
%stackaddr$prim63185 = alloca %struct.ScmObj*, align 8
%_37foldr148055 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60928)
store volatile %struct.ScmObj* %_37foldr148055, %struct.ScmObj** %stackaddr$prim63185, align 8
%ae48706 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure63186 = alloca %struct.ScmObj*, align 8
%fptrToInt63187 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48707 to i64
%ae48707 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt63187)
store volatile %struct.ScmObj* %ae48707, %struct.ScmObj** %stackaddr$makeclosure63186, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48707, %struct.ScmObj* %_37foldr148055, i64 0)
%argslist60941$k485580 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim63188 = alloca %struct.ScmObj*, align 8
%argslist60941$k485581 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48707, %struct.ScmObj* %argslist60941$k485580)
store volatile %struct.ScmObj* %argslist60941$k485581, %struct.ScmObj** %stackaddr$prim63188, align 8
%stackaddr$prim63189 = alloca %struct.ScmObj*, align 8
%argslist60941$k485582 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48706, %struct.ScmObj* %argslist60941$k485581)
store volatile %struct.ScmObj* %argslist60941$k485582, %struct.ScmObj** %stackaddr$prim63189, align 8
%clofunc63190 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48558)
musttail call tailcc void %clofunc63190(%struct.ScmObj* %k48558, %struct.ScmObj* %argslist60941$k485582)
ret void
}

define tailcc void @proc_clo$ae48707(%struct.ScmObj* %env$ae48707,%struct.ScmObj* %current_45args60930) {
%stackaddr$env-ref63191 = alloca %struct.ScmObj*, align 8
%_37foldr148055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48707, i64 0)
store %struct.ScmObj* %_37foldr148055, %struct.ScmObj** %stackaddr$env-ref63191
%stackaddr$prim63192 = alloca %struct.ScmObj*, align 8
%k48559 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60930)
store volatile %struct.ScmObj* %k48559, %struct.ScmObj** %stackaddr$prim63192, align 8
%stackaddr$prim63193 = alloca %struct.ScmObj*, align 8
%current_45args60931 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60930)
store volatile %struct.ScmObj* %current_45args60931, %struct.ScmObj** %stackaddr$prim63193, align 8
%stackaddr$prim63194 = alloca %struct.ScmObj*, align 8
%f48058 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60931)
store volatile %struct.ScmObj* %f48058, %struct.ScmObj** %stackaddr$prim63194, align 8
%stackaddr$prim63195 = alloca %struct.ScmObj*, align 8
%current_45args60932 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60931)
store volatile %struct.ScmObj* %current_45args60932, %struct.ScmObj** %stackaddr$prim63195, align 8
%stackaddr$prim63196 = alloca %struct.ScmObj*, align 8
%acc48057 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60932)
store volatile %struct.ScmObj* %acc48057, %struct.ScmObj** %stackaddr$prim63196, align 8
%stackaddr$prim63197 = alloca %struct.ScmObj*, align 8
%current_45args60933 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60932)
store volatile %struct.ScmObj* %current_45args60933, %struct.ScmObj** %stackaddr$prim63197, align 8
%stackaddr$prim63198 = alloca %struct.ScmObj*, align 8
%lst48056 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60933)
store volatile %struct.ScmObj* %lst48056, %struct.ScmObj** %stackaddr$prim63198, align 8
%stackaddr$prim63199 = alloca %struct.ScmObj*, align 8
%anf_45bind48186 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48056)
store volatile %struct.ScmObj* %anf_45bind48186, %struct.ScmObj** %stackaddr$prim63199, align 8
%truthy$cmp63200 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48186)
%cmp$cmp63200 = icmp eq i64 %truthy$cmp63200, 1
br i1 %cmp$cmp63200, label %truebranch$cmp63200, label %falsebranch$cmp63200
truebranch$cmp63200:
%ae48711 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist60935$k485590 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim63201 = alloca %struct.ScmObj*, align 8
%argslist60935$k485591 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48057, %struct.ScmObj* %argslist60935$k485590)
store volatile %struct.ScmObj* %argslist60935$k485591, %struct.ScmObj** %stackaddr$prim63201, align 8
%stackaddr$prim63202 = alloca %struct.ScmObj*, align 8
%argslist60935$k485592 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48711, %struct.ScmObj* %argslist60935$k485591)
store volatile %struct.ScmObj* %argslist60935$k485592, %struct.ScmObj** %stackaddr$prim63202, align 8
%clofunc63203 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48559)
musttail call tailcc void %clofunc63203(%struct.ScmObj* %k48559, %struct.ScmObj* %argslist60935$k485592)
ret void
falsebranch$cmp63200:
%stackaddr$prim63204 = alloca %struct.ScmObj*, align 8
%anf_45bind48187 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48056)
store volatile %struct.ScmObj* %anf_45bind48187, %struct.ScmObj** %stackaddr$prim63204, align 8
%stackaddr$prim63205 = alloca %struct.ScmObj*, align 8
%anf_45bind48188 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48056)
store volatile %struct.ScmObj* %anf_45bind48188, %struct.ScmObj** %stackaddr$prim63205, align 8
%stackaddr$makeclosure63206 = alloca %struct.ScmObj*, align 8
%fptrToInt63207 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48719 to i64
%ae48719 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt63207)
store volatile %struct.ScmObj* %ae48719, %struct.ScmObj** %stackaddr$makeclosure63206, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48719, %struct.ScmObj* %k48559, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48719, %struct.ScmObj* %anf_45bind48187, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48719, %struct.ScmObj* %f48058, i64 2)
%argslist60940$_37foldr1480550 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim63208 = alloca %struct.ScmObj*, align 8
%argslist60940$_37foldr1480551 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48188, %struct.ScmObj* %argslist60940$_37foldr1480550)
store volatile %struct.ScmObj* %argslist60940$_37foldr1480551, %struct.ScmObj** %stackaddr$prim63208, align 8
%stackaddr$prim63209 = alloca %struct.ScmObj*, align 8
%argslist60940$_37foldr1480552 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48057, %struct.ScmObj* %argslist60940$_37foldr1480551)
store volatile %struct.ScmObj* %argslist60940$_37foldr1480552, %struct.ScmObj** %stackaddr$prim63209, align 8
%stackaddr$prim63210 = alloca %struct.ScmObj*, align 8
%argslist60940$_37foldr1480553 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48058, %struct.ScmObj* %argslist60940$_37foldr1480552)
store volatile %struct.ScmObj* %argslist60940$_37foldr1480553, %struct.ScmObj** %stackaddr$prim63210, align 8
%stackaddr$prim63211 = alloca %struct.ScmObj*, align 8
%argslist60940$_37foldr1480554 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48719, %struct.ScmObj* %argslist60940$_37foldr1480553)
store volatile %struct.ScmObj* %argslist60940$_37foldr1480554, %struct.ScmObj** %stackaddr$prim63211, align 8
%clofunc63212 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148055)
musttail call tailcc void %clofunc63212(%struct.ScmObj* %_37foldr148055, %struct.ScmObj* %argslist60940$_37foldr1480554)
ret void
}

define tailcc void @proc_clo$ae48719(%struct.ScmObj* %env$ae48719,%struct.ScmObj* %current_45args60936) {
%stackaddr$env-ref63213 = alloca %struct.ScmObj*, align 8
%k48559 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48719, i64 0)
store %struct.ScmObj* %k48559, %struct.ScmObj** %stackaddr$env-ref63213
%stackaddr$env-ref63214 = alloca %struct.ScmObj*, align 8
%anf_45bind48187 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48719, i64 1)
store %struct.ScmObj* %anf_45bind48187, %struct.ScmObj** %stackaddr$env-ref63214
%stackaddr$env-ref63215 = alloca %struct.ScmObj*, align 8
%f48058 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48719, i64 2)
store %struct.ScmObj* %f48058, %struct.ScmObj** %stackaddr$env-ref63215
%stackaddr$prim63216 = alloca %struct.ScmObj*, align 8
%_95k48560 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60936)
store volatile %struct.ScmObj* %_95k48560, %struct.ScmObj** %stackaddr$prim63216, align 8
%stackaddr$prim63217 = alloca %struct.ScmObj*, align 8
%current_45args60937 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60936)
store volatile %struct.ScmObj* %current_45args60937, %struct.ScmObj** %stackaddr$prim63217, align 8
%stackaddr$prim63218 = alloca %struct.ScmObj*, align 8
%anf_45bind48189 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60937)
store volatile %struct.ScmObj* %anf_45bind48189, %struct.ScmObj** %stackaddr$prim63218, align 8
%argslist60939$f480580 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim63219 = alloca %struct.ScmObj*, align 8
%argslist60939$f480581 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48189, %struct.ScmObj* %argslist60939$f480580)
store volatile %struct.ScmObj* %argslist60939$f480581, %struct.ScmObj** %stackaddr$prim63219, align 8
%stackaddr$prim63220 = alloca %struct.ScmObj*, align 8
%argslist60939$f480582 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48187, %struct.ScmObj* %argslist60939$f480581)
store volatile %struct.ScmObj* %argslist60939$f480582, %struct.ScmObj** %stackaddr$prim63220, align 8
%stackaddr$prim63221 = alloca %struct.ScmObj*, align 8
%argslist60939$f480583 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48559, %struct.ScmObj* %argslist60939$f480582)
store volatile %struct.ScmObj* %argslist60939$f480583, %struct.ScmObj** %stackaddr$prim63221, align 8
%clofunc63222 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48058)
musttail call tailcc void %clofunc63222(%struct.ScmObj* %f48058, %struct.ScmObj* %argslist60939$f480583)
ret void
}

define tailcc void @proc_clo$ae48587(%struct.ScmObj* %env$ae48587,%struct.ScmObj* %current_45args60944) {
%stackaddr$prim63223 = alloca %struct.ScmObj*, align 8
%k48561 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60944)
store volatile %struct.ScmObj* %k48561, %struct.ScmObj** %stackaddr$prim63223, align 8
%stackaddr$prim63224 = alloca %struct.ScmObj*, align 8
%current_45args60945 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60944)
store volatile %struct.ScmObj* %current_45args60945, %struct.ScmObj** %stackaddr$prim63224, align 8
%stackaddr$prim63225 = alloca %struct.ScmObj*, align 8
%y48035 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60945)
store volatile %struct.ScmObj* %y48035, %struct.ScmObj** %stackaddr$prim63225, align 8
%ae48589 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure63226 = alloca %struct.ScmObj*, align 8
%fptrToInt63227 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48590 to i64
%ae48590 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt63227)
store volatile %struct.ScmObj* %ae48590, %struct.ScmObj** %stackaddr$makeclosure63226, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48590, %struct.ScmObj* %y48035, i64 0)
%argslist60963$k485610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim63228 = alloca %struct.ScmObj*, align 8
%argslist60963$k485611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48590, %struct.ScmObj* %argslist60963$k485610)
store volatile %struct.ScmObj* %argslist60963$k485611, %struct.ScmObj** %stackaddr$prim63228, align 8
%stackaddr$prim63229 = alloca %struct.ScmObj*, align 8
%argslist60963$k485612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48589, %struct.ScmObj* %argslist60963$k485611)
store volatile %struct.ScmObj* %argslist60963$k485612, %struct.ScmObj** %stackaddr$prim63229, align 8
%clofunc63230 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48561)
musttail call tailcc void %clofunc63230(%struct.ScmObj* %k48561, %struct.ScmObj* %argslist60963$k485612)
ret void
}

define tailcc void @proc_clo$ae48590(%struct.ScmObj* %env$ae48590,%struct.ScmObj* %current_45args60947) {
%stackaddr$env-ref63231 = alloca %struct.ScmObj*, align 8
%y48035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48590, i64 0)
store %struct.ScmObj* %y48035, %struct.ScmObj** %stackaddr$env-ref63231
%stackaddr$prim63232 = alloca %struct.ScmObj*, align 8
%k48562 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60947)
store volatile %struct.ScmObj* %k48562, %struct.ScmObj** %stackaddr$prim63232, align 8
%stackaddr$prim63233 = alloca %struct.ScmObj*, align 8
%current_45args60948 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60947)
store volatile %struct.ScmObj* %current_45args60948, %struct.ScmObj** %stackaddr$prim63233, align 8
%stackaddr$prim63234 = alloca %struct.ScmObj*, align 8
%f48036 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60948)
store volatile %struct.ScmObj* %f48036, %struct.ScmObj** %stackaddr$prim63234, align 8
%stackaddr$makeclosure63235 = alloca %struct.ScmObj*, align 8
%fptrToInt63236 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48591 to i64
%ae48591 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt63236)
store volatile %struct.ScmObj* %ae48591, %struct.ScmObj** %stackaddr$makeclosure63235, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48591, %struct.ScmObj* %f48036, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48591, %struct.ScmObj* %k48562, i64 1)
%ae48592 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure63237 = alloca %struct.ScmObj*, align 8
%fptrToInt63238 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48593 to i64
%ae48593 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt63238)
store volatile %struct.ScmObj* %ae48593, %struct.ScmObj** %stackaddr$makeclosure63237, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48593, %struct.ScmObj* %f48036, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48593, %struct.ScmObj* %y48035, i64 1)
%argslist60962$ae485910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim63239 = alloca %struct.ScmObj*, align 8
%argslist60962$ae485911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48593, %struct.ScmObj* %argslist60962$ae485910)
store volatile %struct.ScmObj* %argslist60962$ae485911, %struct.ScmObj** %stackaddr$prim63239, align 8
%stackaddr$prim63240 = alloca %struct.ScmObj*, align 8
%argslist60962$ae485912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48592, %struct.ScmObj* %argslist60962$ae485911)
store volatile %struct.ScmObj* %argslist60962$ae485912, %struct.ScmObj** %stackaddr$prim63240, align 8
%clofunc63241 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48591)
musttail call tailcc void %clofunc63241(%struct.ScmObj* %ae48591, %struct.ScmObj* %argslist60962$ae485912)
ret void
}

define tailcc void @proc_clo$ae48591(%struct.ScmObj* %env$ae48591,%struct.ScmObj* %current_45args60950) {
%stackaddr$env-ref63242 = alloca %struct.ScmObj*, align 8
%f48036 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48591, i64 0)
store %struct.ScmObj* %f48036, %struct.ScmObj** %stackaddr$env-ref63242
%stackaddr$env-ref63243 = alloca %struct.ScmObj*, align 8
%k48562 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48591, i64 1)
store %struct.ScmObj* %k48562, %struct.ScmObj** %stackaddr$env-ref63243
%stackaddr$prim63244 = alloca %struct.ScmObj*, align 8
%_95k48563 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60950)
store volatile %struct.ScmObj* %_95k48563, %struct.ScmObj** %stackaddr$prim63244, align 8
%stackaddr$prim63245 = alloca %struct.ScmObj*, align 8
%current_45args60951 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60950)
store volatile %struct.ScmObj* %current_45args60951, %struct.ScmObj** %stackaddr$prim63245, align 8
%stackaddr$prim63246 = alloca %struct.ScmObj*, align 8
%anf_45bind48184 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60951)
store volatile %struct.ScmObj* %anf_45bind48184, %struct.ScmObj** %stackaddr$prim63246, align 8
%argslist60953$f480360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim63247 = alloca %struct.ScmObj*, align 8
%argslist60953$f480361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48184, %struct.ScmObj* %argslist60953$f480360)
store volatile %struct.ScmObj* %argslist60953$f480361, %struct.ScmObj** %stackaddr$prim63247, align 8
%stackaddr$prim63248 = alloca %struct.ScmObj*, align 8
%argslist60953$f480362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48562, %struct.ScmObj* %argslist60953$f480361)
store volatile %struct.ScmObj* %argslist60953$f480362, %struct.ScmObj** %stackaddr$prim63248, align 8
%clofunc63249 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48036)
musttail call tailcc void %clofunc63249(%struct.ScmObj* %f48036, %struct.ScmObj* %argslist60953$f480362)
ret void
}

define tailcc void @proc_clo$ae48593(%struct.ScmObj* %env$ae48593,%struct.ScmObj* %args4803748564) {
%stackaddr$env-ref63250 = alloca %struct.ScmObj*, align 8
%f48036 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48593, i64 0)
store %struct.ScmObj* %f48036, %struct.ScmObj** %stackaddr$env-ref63250
%stackaddr$env-ref63251 = alloca %struct.ScmObj*, align 8
%y48035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48593, i64 1)
store %struct.ScmObj* %y48035, %struct.ScmObj** %stackaddr$env-ref63251
%stackaddr$prim63252 = alloca %struct.ScmObj*, align 8
%k48565 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4803748564)
store volatile %struct.ScmObj* %k48565, %struct.ScmObj** %stackaddr$prim63252, align 8
%stackaddr$prim63253 = alloca %struct.ScmObj*, align 8
%args48037 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4803748564)
store volatile %struct.ScmObj* %args48037, %struct.ScmObj** %stackaddr$prim63253, align 8
%stackaddr$makeclosure63254 = alloca %struct.ScmObj*, align 8
%fptrToInt63255 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48597 to i64
%ae48597 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt63255)
store volatile %struct.ScmObj* %ae48597, %struct.ScmObj** %stackaddr$makeclosure63254, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48597, %struct.ScmObj* %k48565, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48597, %struct.ScmObj* %args48037, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48597, %struct.ScmObj* %f48036, i64 2)
%argslist60961$y480350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim63256 = alloca %struct.ScmObj*, align 8
%argslist60961$y480351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y48035, %struct.ScmObj* %argslist60961$y480350)
store volatile %struct.ScmObj* %argslist60961$y480351, %struct.ScmObj** %stackaddr$prim63256, align 8
%stackaddr$prim63257 = alloca %struct.ScmObj*, align 8
%argslist60961$y480352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48597, %struct.ScmObj* %argslist60961$y480351)
store volatile %struct.ScmObj* %argslist60961$y480352, %struct.ScmObj** %stackaddr$prim63257, align 8
%clofunc63258 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y48035)
musttail call tailcc void %clofunc63258(%struct.ScmObj* %y48035, %struct.ScmObj* %argslist60961$y480352)
ret void
}

define tailcc void @proc_clo$ae48597(%struct.ScmObj* %env$ae48597,%struct.ScmObj* %current_45args60954) {
%stackaddr$env-ref63259 = alloca %struct.ScmObj*, align 8
%k48565 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48597, i64 0)
store %struct.ScmObj* %k48565, %struct.ScmObj** %stackaddr$env-ref63259
%stackaddr$env-ref63260 = alloca %struct.ScmObj*, align 8
%args48037 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48597, i64 1)
store %struct.ScmObj* %args48037, %struct.ScmObj** %stackaddr$env-ref63260
%stackaddr$env-ref63261 = alloca %struct.ScmObj*, align 8
%f48036 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48597, i64 2)
store %struct.ScmObj* %f48036, %struct.ScmObj** %stackaddr$env-ref63261
%stackaddr$prim63262 = alloca %struct.ScmObj*, align 8
%_95k48566 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60954)
store volatile %struct.ScmObj* %_95k48566, %struct.ScmObj** %stackaddr$prim63262, align 8
%stackaddr$prim63263 = alloca %struct.ScmObj*, align 8
%current_45args60955 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60954)
store volatile %struct.ScmObj* %current_45args60955, %struct.ScmObj** %stackaddr$prim63263, align 8
%stackaddr$prim63264 = alloca %struct.ScmObj*, align 8
%anf_45bind48182 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60955)
store volatile %struct.ScmObj* %anf_45bind48182, %struct.ScmObj** %stackaddr$prim63264, align 8
%stackaddr$makeclosure63265 = alloca %struct.ScmObj*, align 8
%fptrToInt63266 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48600 to i64
%ae48600 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt63266)
store volatile %struct.ScmObj* %ae48600, %struct.ScmObj** %stackaddr$makeclosure63265, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48600, %struct.ScmObj* %k48565, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48600, %struct.ScmObj* %args48037, i64 1)
%argslist60960$anf_45bind481820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim63267 = alloca %struct.ScmObj*, align 8
%argslist60960$anf_45bind481821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48036, %struct.ScmObj* %argslist60960$anf_45bind481820)
store volatile %struct.ScmObj* %argslist60960$anf_45bind481821, %struct.ScmObj** %stackaddr$prim63267, align 8
%stackaddr$prim63268 = alloca %struct.ScmObj*, align 8
%argslist60960$anf_45bind481822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48600, %struct.ScmObj* %argslist60960$anf_45bind481821)
store volatile %struct.ScmObj* %argslist60960$anf_45bind481822, %struct.ScmObj** %stackaddr$prim63268, align 8
%clofunc63269 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48182)
musttail call tailcc void %clofunc63269(%struct.ScmObj* %anf_45bind48182, %struct.ScmObj* %argslist60960$anf_45bind481822)
ret void
}

define tailcc void @proc_clo$ae48600(%struct.ScmObj* %env$ae48600,%struct.ScmObj* %current_45args60957) {
%stackaddr$env-ref63270 = alloca %struct.ScmObj*, align 8
%k48565 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48600, i64 0)
store %struct.ScmObj* %k48565, %struct.ScmObj** %stackaddr$env-ref63270
%stackaddr$env-ref63271 = alloca %struct.ScmObj*, align 8
%args48037 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48600, i64 1)
store %struct.ScmObj* %args48037, %struct.ScmObj** %stackaddr$env-ref63271
%stackaddr$prim63272 = alloca %struct.ScmObj*, align 8
%_95k48567 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60957)
store volatile %struct.ScmObj* %_95k48567, %struct.ScmObj** %stackaddr$prim63272, align 8
%stackaddr$prim63273 = alloca %struct.ScmObj*, align 8
%current_45args60958 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60957)
store volatile %struct.ScmObj* %current_45args60958, %struct.ScmObj** %stackaddr$prim63273, align 8
%stackaddr$prim63274 = alloca %struct.ScmObj*, align 8
%anf_45bind48183 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60958)
store volatile %struct.ScmObj* %anf_45bind48183, %struct.ScmObj** %stackaddr$prim63274, align 8
%stackaddr$prim63275 = alloca %struct.ScmObj*, align 8
%cpsargs48568 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48565, %struct.ScmObj* %args48037)
store volatile %struct.ScmObj* %cpsargs48568, %struct.ScmObj** %stackaddr$prim63275, align 8
%clofunc63276 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48183)
musttail call tailcc void %clofunc63276(%struct.ScmObj* %anf_45bind48183, %struct.ScmObj* %cpsargs48568)
ret void
}

define tailcc void @proc_clo$ae48572(%struct.ScmObj* %env$ae48572,%struct.ScmObj* %current_45args60965) {
%stackaddr$prim63277 = alloca %struct.ScmObj*, align 8
%k48569 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60965)
store volatile %struct.ScmObj* %k48569, %struct.ScmObj** %stackaddr$prim63277, align 8
%stackaddr$prim63278 = alloca %struct.ScmObj*, align 8
%current_45args60966 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60965)
store volatile %struct.ScmObj* %current_45args60966, %struct.ScmObj** %stackaddr$prim63278, align 8
%stackaddr$prim63279 = alloca %struct.ScmObj*, align 8
%yu48034 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60966)
store volatile %struct.ScmObj* %yu48034, %struct.ScmObj** %stackaddr$prim63279, align 8
%argslist60968$yu480340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim63280 = alloca %struct.ScmObj*, align 8
%argslist60968$yu480341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu48034, %struct.ScmObj* %argslist60968$yu480340)
store volatile %struct.ScmObj* %argslist60968$yu480341, %struct.ScmObj** %stackaddr$prim63280, align 8
%stackaddr$prim63281 = alloca %struct.ScmObj*, align 8
%argslist60968$yu480342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48569, %struct.ScmObj* %argslist60968$yu480341)
store volatile %struct.ScmObj* %argslist60968$yu480342, %struct.ScmObj** %stackaddr$prim63281, align 8
%clofunc63282 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu48034)
musttail call tailcc void %clofunc63282(%struct.ScmObj* %yu48034, %struct.ScmObj* %argslist60968$yu480342)
ret void
}