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
%mainenv60015 = call %struct.ScmObj* @const_init_null()
%mainargs60016 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv60015, %struct.ScmObj* %mainargs60016)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv60013,%struct.ScmObj* %mainargs60014) {
%stackaddr$makeclosure60017 = alloca %struct.ScmObj*, align 8
%fptrToInt60018 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47613 to i64
%ae47613 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt60018)
store volatile %struct.ScmObj* %ae47613, %struct.ScmObj** %stackaddr$makeclosure60017, align 8
%ae47614 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure60019 = alloca %struct.ScmObj*, align 8
%fptrToInt60020 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47615 to i64
%ae47615 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt60020)
store volatile %struct.ScmObj* %ae47615, %struct.ScmObj** %stackaddr$makeclosure60019, align 8
%argslist60012$ae476130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60021 = alloca %struct.ScmObj*, align 8
%argslist60012$ae476131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47615, %struct.ScmObj* %argslist60012$ae476130)
store volatile %struct.ScmObj* %argslist60012$ae476131, %struct.ScmObj** %stackaddr$prim60021, align 8
%stackaddr$prim60022 = alloca %struct.ScmObj*, align 8
%argslist60012$ae476132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47614, %struct.ScmObj* %argslist60012$ae476131)
store volatile %struct.ScmObj* %argslist60012$ae476132, %struct.ScmObj** %stackaddr$prim60022, align 8
%clofunc60023 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47613)
musttail call tailcc void %clofunc60023(%struct.ScmObj* %ae47613, %struct.ScmObj* %argslist60012$ae476132)
ret void
}

define tailcc void @proc_clo$ae47613(%struct.ScmObj* %env$ae47613,%struct.ScmObj* %current_45args59271) {
%stackaddr$prim60024 = alloca %struct.ScmObj*, align 8
%_95k47399 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59271)
store volatile %struct.ScmObj* %_95k47399, %struct.ScmObj** %stackaddr$prim60024, align 8
%stackaddr$prim60025 = alloca %struct.ScmObj*, align 8
%current_45args59272 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59271)
store volatile %struct.ScmObj* %current_45args59272, %struct.ScmObj** %stackaddr$prim60025, align 8
%stackaddr$prim60026 = alloca %struct.ScmObj*, align 8
%anf_45bind47224 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59272)
store volatile %struct.ScmObj* %anf_45bind47224, %struct.ScmObj** %stackaddr$prim60026, align 8
%stackaddr$makeclosure60027 = alloca %struct.ScmObj*, align 8
%fptrToInt60028 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47628 to i64
%ae47628 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt60028)
store volatile %struct.ScmObj* %ae47628, %struct.ScmObj** %stackaddr$makeclosure60027, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47628, %struct.ScmObj* %anf_45bind47224, i64 0)
%ae47629 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure60029 = alloca %struct.ScmObj*, align 8
%fptrToInt60030 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47630 to i64
%ae47630 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt60030)
store volatile %struct.ScmObj* %ae47630, %struct.ScmObj** %stackaddr$makeclosure60029, align 8
%argslist60007$ae476280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60031 = alloca %struct.ScmObj*, align 8
%argslist60007$ae476281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47630, %struct.ScmObj* %argslist60007$ae476280)
store volatile %struct.ScmObj* %argslist60007$ae476281, %struct.ScmObj** %stackaddr$prim60031, align 8
%stackaddr$prim60032 = alloca %struct.ScmObj*, align 8
%argslist60007$ae476282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47629, %struct.ScmObj* %argslist60007$ae476281)
store volatile %struct.ScmObj* %argslist60007$ae476282, %struct.ScmObj** %stackaddr$prim60032, align 8
%clofunc60033 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47628)
musttail call tailcc void %clofunc60033(%struct.ScmObj* %ae47628, %struct.ScmObj* %argslist60007$ae476282)
ret void
}

define tailcc void @proc_clo$ae47628(%struct.ScmObj* %env$ae47628,%struct.ScmObj* %current_45args59274) {
%stackaddr$env-ref60034 = alloca %struct.ScmObj*, align 8
%anf_45bind47224 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47628, i64 0)
store %struct.ScmObj* %anf_45bind47224, %struct.ScmObj** %stackaddr$env-ref60034
%stackaddr$prim60035 = alloca %struct.ScmObj*, align 8
%_95k47400 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59274)
store volatile %struct.ScmObj* %_95k47400, %struct.ScmObj** %stackaddr$prim60035, align 8
%stackaddr$prim60036 = alloca %struct.ScmObj*, align 8
%current_45args59275 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59274)
store volatile %struct.ScmObj* %current_45args59275, %struct.ScmObj** %stackaddr$prim60036, align 8
%stackaddr$prim60037 = alloca %struct.ScmObj*, align 8
%anf_45bind47228 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59275)
store volatile %struct.ScmObj* %anf_45bind47228, %struct.ScmObj** %stackaddr$prim60037, align 8
%stackaddr$makeclosure60038 = alloca %struct.ScmObj*, align 8
%fptrToInt60039 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47743 to i64
%ae47743 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt60039)
store volatile %struct.ScmObj* %ae47743, %struct.ScmObj** %stackaddr$makeclosure60038, align 8
%argslist59986$anf_45bind472240 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60040 = alloca %struct.ScmObj*, align 8
%argslist59986$anf_45bind472241 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47228, %struct.ScmObj* %argslist59986$anf_45bind472240)
store volatile %struct.ScmObj* %argslist59986$anf_45bind472241, %struct.ScmObj** %stackaddr$prim60040, align 8
%stackaddr$prim60041 = alloca %struct.ScmObj*, align 8
%argslist59986$anf_45bind472242 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47743, %struct.ScmObj* %argslist59986$anf_45bind472241)
store volatile %struct.ScmObj* %argslist59986$anf_45bind472242, %struct.ScmObj** %stackaddr$prim60041, align 8
%clofunc60042 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47224)
musttail call tailcc void %clofunc60042(%struct.ScmObj* %anf_45bind47224, %struct.ScmObj* %argslist59986$anf_45bind472242)
ret void
}

define tailcc void @proc_clo$ae47743(%struct.ScmObj* %env$ae47743,%struct.ScmObj* %current_45args59277) {
%stackaddr$prim60043 = alloca %struct.ScmObj*, align 8
%_95k47401 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59277)
store volatile %struct.ScmObj* %_95k47401, %struct.ScmObj** %stackaddr$prim60043, align 8
%stackaddr$prim60044 = alloca %struct.ScmObj*, align 8
%current_45args59278 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59277)
store volatile %struct.ScmObj* %current_45args59278, %struct.ScmObj** %stackaddr$prim60044, align 8
%stackaddr$prim60045 = alloca %struct.ScmObj*, align 8
%Ycmb47076 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59278)
store volatile %struct.ScmObj* %Ycmb47076, %struct.ScmObj** %stackaddr$prim60045, align 8
%stackaddr$makeclosure60046 = alloca %struct.ScmObj*, align 8
%fptrToInt60047 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47745 to i64
%ae47745 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt60047)
store volatile %struct.ScmObj* %ae47745, %struct.ScmObj** %stackaddr$makeclosure60046, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47745, %struct.ScmObj* %Ycmb47076, i64 0)
%ae47746 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure60048 = alloca %struct.ScmObj*, align 8
%fptrToInt60049 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47747 to i64
%ae47747 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt60049)
store volatile %struct.ScmObj* %ae47747, %struct.ScmObj** %stackaddr$makeclosure60048, align 8
%argslist59985$ae477450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60050 = alloca %struct.ScmObj*, align 8
%argslist59985$ae477451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47747, %struct.ScmObj* %argslist59985$ae477450)
store volatile %struct.ScmObj* %argslist59985$ae477451, %struct.ScmObj** %stackaddr$prim60050, align 8
%stackaddr$prim60051 = alloca %struct.ScmObj*, align 8
%argslist59985$ae477452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47746, %struct.ScmObj* %argslist59985$ae477451)
store volatile %struct.ScmObj* %argslist59985$ae477452, %struct.ScmObj** %stackaddr$prim60051, align 8
%clofunc60052 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47745)
musttail call tailcc void %clofunc60052(%struct.ScmObj* %ae47745, %struct.ScmObj* %argslist59985$ae477452)
ret void
}

define tailcc void @proc_clo$ae47745(%struct.ScmObj* %env$ae47745,%struct.ScmObj* %current_45args59280) {
%stackaddr$env-ref60053 = alloca %struct.ScmObj*, align 8
%Ycmb47076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47745, i64 0)
store %struct.ScmObj* %Ycmb47076, %struct.ScmObj** %stackaddr$env-ref60053
%stackaddr$prim60054 = alloca %struct.ScmObj*, align 8
%_95k47402 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59280)
store volatile %struct.ScmObj* %_95k47402, %struct.ScmObj** %stackaddr$prim60054, align 8
%stackaddr$prim60055 = alloca %struct.ScmObj*, align 8
%current_45args59281 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59280)
store volatile %struct.ScmObj* %current_45args59281, %struct.ScmObj** %stackaddr$prim60055, align 8
%stackaddr$prim60056 = alloca %struct.ScmObj*, align 8
%anf_45bind47233 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59281)
store volatile %struct.ScmObj* %anf_45bind47233, %struct.ScmObj** %stackaddr$prim60056, align 8
%stackaddr$makeclosure60057 = alloca %struct.ScmObj*, align 8
%fptrToInt60058 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47823 to i64
%ae47823 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt60058)
store volatile %struct.ScmObj* %ae47823, %struct.ScmObj** %stackaddr$makeclosure60057, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47823, %struct.ScmObj* %Ycmb47076, i64 0)
%argslist59969$Ycmb470760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60059 = alloca %struct.ScmObj*, align 8
%argslist59969$Ycmb470761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47233, %struct.ScmObj* %argslist59969$Ycmb470760)
store volatile %struct.ScmObj* %argslist59969$Ycmb470761, %struct.ScmObj** %stackaddr$prim60059, align 8
%stackaddr$prim60060 = alloca %struct.ScmObj*, align 8
%argslist59969$Ycmb470762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47823, %struct.ScmObj* %argslist59969$Ycmb470761)
store volatile %struct.ScmObj* %argslist59969$Ycmb470762, %struct.ScmObj** %stackaddr$prim60060, align 8
%clofunc60061 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47076)
musttail call tailcc void %clofunc60061(%struct.ScmObj* %Ycmb47076, %struct.ScmObj* %argslist59969$Ycmb470762)
ret void
}

define tailcc void @proc_clo$ae47823(%struct.ScmObj* %env$ae47823,%struct.ScmObj* %current_45args59283) {
%stackaddr$env-ref60062 = alloca %struct.ScmObj*, align 8
%Ycmb47076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47823, i64 0)
store %struct.ScmObj* %Ycmb47076, %struct.ScmObj** %stackaddr$env-ref60062
%stackaddr$prim60063 = alloca %struct.ScmObj*, align 8
%_95k47403 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59283)
store volatile %struct.ScmObj* %_95k47403, %struct.ScmObj** %stackaddr$prim60063, align 8
%stackaddr$prim60064 = alloca %struct.ScmObj*, align 8
%current_45args59284 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59283)
store volatile %struct.ScmObj* %current_45args59284, %struct.ScmObj** %stackaddr$prim60064, align 8
%stackaddr$prim60065 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59284)
store volatile %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$prim60065, align 8
%stackaddr$makeclosure60066 = alloca %struct.ScmObj*, align 8
%fptrToInt60067 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47825 to i64
%ae47825 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt60067)
store volatile %struct.ScmObj* %ae47825, %struct.ScmObj** %stackaddr$makeclosure60066, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47825, %struct.ScmObj* %Ycmb47076, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47825, %struct.ScmObj* %_37foldr147097, i64 1)
%ae47826 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure60068 = alloca %struct.ScmObj*, align 8
%fptrToInt60069 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47827 to i64
%ae47827 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt60069)
store volatile %struct.ScmObj* %ae47827, %struct.ScmObj** %stackaddr$makeclosure60068, align 8
%argslist59968$ae478250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60070 = alloca %struct.ScmObj*, align 8
%argslist59968$ae478251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47827, %struct.ScmObj* %argslist59968$ae478250)
store volatile %struct.ScmObj* %argslist59968$ae478251, %struct.ScmObj** %stackaddr$prim60070, align 8
%stackaddr$prim60071 = alloca %struct.ScmObj*, align 8
%argslist59968$ae478252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47826, %struct.ScmObj* %argslist59968$ae478251)
store volatile %struct.ScmObj* %argslist59968$ae478252, %struct.ScmObj** %stackaddr$prim60071, align 8
%clofunc60072 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47825)
musttail call tailcc void %clofunc60072(%struct.ScmObj* %ae47825, %struct.ScmObj* %argslist59968$ae478252)
ret void
}

define tailcc void @proc_clo$ae47825(%struct.ScmObj* %env$ae47825,%struct.ScmObj* %current_45args59286) {
%stackaddr$env-ref60073 = alloca %struct.ScmObj*, align 8
%Ycmb47076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47825, i64 0)
store %struct.ScmObj* %Ycmb47076, %struct.ScmObj** %stackaddr$env-ref60073
%stackaddr$env-ref60074 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47825, i64 1)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref60074
%stackaddr$prim60075 = alloca %struct.ScmObj*, align 8
%_95k47404 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59286)
store volatile %struct.ScmObj* %_95k47404, %struct.ScmObj** %stackaddr$prim60075, align 8
%stackaddr$prim60076 = alloca %struct.ScmObj*, align 8
%current_45args59287 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59286)
store volatile %struct.ScmObj* %current_45args59287, %struct.ScmObj** %stackaddr$prim60076, align 8
%stackaddr$prim60077 = alloca %struct.ScmObj*, align 8
%anf_45bind47239 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59287)
store volatile %struct.ScmObj* %anf_45bind47239, %struct.ScmObj** %stackaddr$prim60077, align 8
%stackaddr$makeclosure60078 = alloca %struct.ScmObj*, align 8
%fptrToInt60079 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47920 to i64
%ae47920 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt60079)
store volatile %struct.ScmObj* %ae47920, %struct.ScmObj** %stackaddr$makeclosure60078, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47920, %struct.ScmObj* %Ycmb47076, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47920, %struct.ScmObj* %_37foldr147097, i64 1)
%argslist59949$Ycmb470760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60080 = alloca %struct.ScmObj*, align 8
%argslist59949$Ycmb470761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47239, %struct.ScmObj* %argslist59949$Ycmb470760)
store volatile %struct.ScmObj* %argslist59949$Ycmb470761, %struct.ScmObj** %stackaddr$prim60080, align 8
%stackaddr$prim60081 = alloca %struct.ScmObj*, align 8
%argslist59949$Ycmb470762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47920, %struct.ScmObj* %argslist59949$Ycmb470761)
store volatile %struct.ScmObj* %argslist59949$Ycmb470762, %struct.ScmObj** %stackaddr$prim60081, align 8
%clofunc60082 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47076)
musttail call tailcc void %clofunc60082(%struct.ScmObj* %Ycmb47076, %struct.ScmObj* %argslist59949$Ycmb470762)
ret void
}

define tailcc void @proc_clo$ae47920(%struct.ScmObj* %env$ae47920,%struct.ScmObj* %current_45args59289) {
%stackaddr$env-ref60083 = alloca %struct.ScmObj*, align 8
%Ycmb47076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47920, i64 0)
store %struct.ScmObj* %Ycmb47076, %struct.ScmObj** %stackaddr$env-ref60083
%stackaddr$env-ref60084 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47920, i64 1)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref60084
%stackaddr$prim60085 = alloca %struct.ScmObj*, align 8
%_95k47405 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59289)
store volatile %struct.ScmObj* %_95k47405, %struct.ScmObj** %stackaddr$prim60085, align 8
%stackaddr$prim60086 = alloca %struct.ScmObj*, align 8
%current_45args59290 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59289)
store volatile %struct.ScmObj* %current_45args59290, %struct.ScmObj** %stackaddr$prim60086, align 8
%stackaddr$prim60087 = alloca %struct.ScmObj*, align 8
%_37map147093 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59290)
store volatile %struct.ScmObj* %_37map147093, %struct.ScmObj** %stackaddr$prim60087, align 8
%stackaddr$makeclosure60088 = alloca %struct.ScmObj*, align 8
%fptrToInt60089 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47922 to i64
%ae47922 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt60089)
store volatile %struct.ScmObj* %ae47922, %struct.ScmObj** %stackaddr$makeclosure60088, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47922, %struct.ScmObj* %_37map147093, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47922, %struct.ScmObj* %Ycmb47076, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47922, %struct.ScmObj* %_37foldr147097, i64 2)
%ae47923 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure60090 = alloca %struct.ScmObj*, align 8
%fptrToInt60091 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47924 to i64
%ae47924 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt60091)
store volatile %struct.ScmObj* %ae47924, %struct.ScmObj** %stackaddr$makeclosure60090, align 8
%argslist59948$ae479220 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60092 = alloca %struct.ScmObj*, align 8
%argslist59948$ae479221 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47924, %struct.ScmObj* %argslist59948$ae479220)
store volatile %struct.ScmObj* %argslist59948$ae479221, %struct.ScmObj** %stackaddr$prim60092, align 8
%stackaddr$prim60093 = alloca %struct.ScmObj*, align 8
%argslist59948$ae479222 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47923, %struct.ScmObj* %argslist59948$ae479221)
store volatile %struct.ScmObj* %argslist59948$ae479222, %struct.ScmObj** %stackaddr$prim60093, align 8
%clofunc60094 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47922)
musttail call tailcc void %clofunc60094(%struct.ScmObj* %ae47922, %struct.ScmObj* %argslist59948$ae479222)
ret void
}

define tailcc void @proc_clo$ae47922(%struct.ScmObj* %env$ae47922,%struct.ScmObj* %current_45args59292) {
%stackaddr$env-ref60095 = alloca %struct.ScmObj*, align 8
%_37map147093 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47922, i64 0)
store %struct.ScmObj* %_37map147093, %struct.ScmObj** %stackaddr$env-ref60095
%stackaddr$env-ref60096 = alloca %struct.ScmObj*, align 8
%Ycmb47076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47922, i64 1)
store %struct.ScmObj* %Ycmb47076, %struct.ScmObj** %stackaddr$env-ref60096
%stackaddr$env-ref60097 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47922, i64 2)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref60097
%stackaddr$prim60098 = alloca %struct.ScmObj*, align 8
%_95k47406 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59292)
store volatile %struct.ScmObj* %_95k47406, %struct.ScmObj** %stackaddr$prim60098, align 8
%stackaddr$prim60099 = alloca %struct.ScmObj*, align 8
%current_45args59293 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59292)
store volatile %struct.ScmObj* %current_45args59293, %struct.ScmObj** %stackaddr$prim60099, align 8
%stackaddr$prim60100 = alloca %struct.ScmObj*, align 8
%anf_45bind47246 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59293)
store volatile %struct.ScmObj* %anf_45bind47246, %struct.ScmObj** %stackaddr$prim60100, align 8
%stackaddr$makeclosure60101 = alloca %struct.ScmObj*, align 8
%fptrToInt60102 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48070 to i64
%ae48070 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt60102)
store volatile %struct.ScmObj* %ae48070, %struct.ScmObj** %stackaddr$makeclosure60101, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48070, %struct.ScmObj* %_37map147093, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48070, %struct.ScmObj* %Ycmb47076, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48070, %struct.ScmObj* %_37foldr147097, i64 2)
%argslist59932$Ycmb470760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60103 = alloca %struct.ScmObj*, align 8
%argslist59932$Ycmb470761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47246, %struct.ScmObj* %argslist59932$Ycmb470760)
store volatile %struct.ScmObj* %argslist59932$Ycmb470761, %struct.ScmObj** %stackaddr$prim60103, align 8
%stackaddr$prim60104 = alloca %struct.ScmObj*, align 8
%argslist59932$Ycmb470762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48070, %struct.ScmObj* %argslist59932$Ycmb470761)
store volatile %struct.ScmObj* %argslist59932$Ycmb470762, %struct.ScmObj** %stackaddr$prim60104, align 8
%clofunc60105 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47076)
musttail call tailcc void %clofunc60105(%struct.ScmObj* %Ycmb47076, %struct.ScmObj* %argslist59932$Ycmb470762)
ret void
}

define tailcc void @proc_clo$ae48070(%struct.ScmObj* %env$ae48070,%struct.ScmObj* %current_45args59295) {
%stackaddr$env-ref60106 = alloca %struct.ScmObj*, align 8
%_37map147093 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48070, i64 0)
store %struct.ScmObj* %_37map147093, %struct.ScmObj** %stackaddr$env-ref60106
%stackaddr$env-ref60107 = alloca %struct.ScmObj*, align 8
%Ycmb47076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48070, i64 1)
store %struct.ScmObj* %Ycmb47076, %struct.ScmObj** %stackaddr$env-ref60107
%stackaddr$env-ref60108 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48070, i64 2)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref60108
%stackaddr$prim60109 = alloca %struct.ScmObj*, align 8
%_95k47407 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59295)
store volatile %struct.ScmObj* %_95k47407, %struct.ScmObj** %stackaddr$prim60109, align 8
%stackaddr$prim60110 = alloca %struct.ScmObj*, align 8
%current_45args59296 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59295)
store volatile %struct.ScmObj* %current_45args59296, %struct.ScmObj** %stackaddr$prim60110, align 8
%stackaddr$prim60111 = alloca %struct.ScmObj*, align 8
%_37take47089 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59296)
store volatile %struct.ScmObj* %_37take47089, %struct.ScmObj** %stackaddr$prim60111, align 8
%stackaddr$makeclosure60112 = alloca %struct.ScmObj*, align 8
%fptrToInt60113 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48072 to i64
%ae48072 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt60113)
store volatile %struct.ScmObj* %ae48072, %struct.ScmObj** %stackaddr$makeclosure60112, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48072, %struct.ScmObj* %_37map147093, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48072, %struct.ScmObj* %Ycmb47076, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48072, %struct.ScmObj* %_37take47089, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48072, %struct.ScmObj* %_37foldr147097, i64 3)
%ae48073 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure60114 = alloca %struct.ScmObj*, align 8
%fptrToInt60115 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48074 to i64
%ae48074 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt60115)
store volatile %struct.ScmObj* %ae48074, %struct.ScmObj** %stackaddr$makeclosure60114, align 8
%argslist59931$ae480720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60116 = alloca %struct.ScmObj*, align 8
%argslist59931$ae480721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48074, %struct.ScmObj* %argslist59931$ae480720)
store volatile %struct.ScmObj* %argslist59931$ae480721, %struct.ScmObj** %stackaddr$prim60116, align 8
%stackaddr$prim60117 = alloca %struct.ScmObj*, align 8
%argslist59931$ae480722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48073, %struct.ScmObj* %argslist59931$ae480721)
store volatile %struct.ScmObj* %argslist59931$ae480722, %struct.ScmObj** %stackaddr$prim60117, align 8
%clofunc60118 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48072)
musttail call tailcc void %clofunc60118(%struct.ScmObj* %ae48072, %struct.ScmObj* %argslist59931$ae480722)
ret void
}

define tailcc void @proc_clo$ae48072(%struct.ScmObj* %env$ae48072,%struct.ScmObj* %current_45args59298) {
%stackaddr$env-ref60119 = alloca %struct.ScmObj*, align 8
%_37map147093 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48072, i64 0)
store %struct.ScmObj* %_37map147093, %struct.ScmObj** %stackaddr$env-ref60119
%stackaddr$env-ref60120 = alloca %struct.ScmObj*, align 8
%Ycmb47076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48072, i64 1)
store %struct.ScmObj* %Ycmb47076, %struct.ScmObj** %stackaddr$env-ref60120
%stackaddr$env-ref60121 = alloca %struct.ScmObj*, align 8
%_37take47089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48072, i64 2)
store %struct.ScmObj* %_37take47089, %struct.ScmObj** %stackaddr$env-ref60121
%stackaddr$env-ref60122 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48072, i64 3)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref60122
%stackaddr$prim60123 = alloca %struct.ScmObj*, align 8
%_95k47408 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59298)
store volatile %struct.ScmObj* %_95k47408, %struct.ScmObj** %stackaddr$prim60123, align 8
%stackaddr$prim60124 = alloca %struct.ScmObj*, align 8
%current_45args59299 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59298)
store volatile %struct.ScmObj* %current_45args59299, %struct.ScmObj** %stackaddr$prim60124, align 8
%stackaddr$prim60125 = alloca %struct.ScmObj*, align 8
%anf_45bind47250 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59299)
store volatile %struct.ScmObj* %anf_45bind47250, %struct.ScmObj** %stackaddr$prim60125, align 8
%stackaddr$makeclosure60126 = alloca %struct.ScmObj*, align 8
%fptrToInt60127 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48153 to i64
%ae48153 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt60127)
store volatile %struct.ScmObj* %ae48153, %struct.ScmObj** %stackaddr$makeclosure60126, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48153, %struct.ScmObj* %_37map147093, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48153, %struct.ScmObj* %Ycmb47076, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48153, %struct.ScmObj* %_37take47089, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48153, %struct.ScmObj* %_37foldr147097, i64 3)
%argslist59917$Ycmb470760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60128 = alloca %struct.ScmObj*, align 8
%argslist59917$Ycmb470761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47250, %struct.ScmObj* %argslist59917$Ycmb470760)
store volatile %struct.ScmObj* %argslist59917$Ycmb470761, %struct.ScmObj** %stackaddr$prim60128, align 8
%stackaddr$prim60129 = alloca %struct.ScmObj*, align 8
%argslist59917$Ycmb470762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48153, %struct.ScmObj* %argslist59917$Ycmb470761)
store volatile %struct.ScmObj* %argslist59917$Ycmb470762, %struct.ScmObj** %stackaddr$prim60129, align 8
%clofunc60130 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47076)
musttail call tailcc void %clofunc60130(%struct.ScmObj* %Ycmb47076, %struct.ScmObj* %argslist59917$Ycmb470762)
ret void
}

define tailcc void @proc_clo$ae48153(%struct.ScmObj* %env$ae48153,%struct.ScmObj* %current_45args59301) {
%stackaddr$env-ref60131 = alloca %struct.ScmObj*, align 8
%_37map147093 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48153, i64 0)
store %struct.ScmObj* %_37map147093, %struct.ScmObj** %stackaddr$env-ref60131
%stackaddr$env-ref60132 = alloca %struct.ScmObj*, align 8
%Ycmb47076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48153, i64 1)
store %struct.ScmObj* %Ycmb47076, %struct.ScmObj** %stackaddr$env-ref60132
%stackaddr$env-ref60133 = alloca %struct.ScmObj*, align 8
%_37take47089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48153, i64 2)
store %struct.ScmObj* %_37take47089, %struct.ScmObj** %stackaddr$env-ref60133
%stackaddr$env-ref60134 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48153, i64 3)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref60134
%stackaddr$prim60135 = alloca %struct.ScmObj*, align 8
%_95k47409 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59301)
store volatile %struct.ScmObj* %_95k47409, %struct.ScmObj** %stackaddr$prim60135, align 8
%stackaddr$prim60136 = alloca %struct.ScmObj*, align 8
%current_45args59302 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59301)
store volatile %struct.ScmObj* %current_45args59302, %struct.ScmObj** %stackaddr$prim60136, align 8
%stackaddr$prim60137 = alloca %struct.ScmObj*, align 8
%_37length47086 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59302)
store volatile %struct.ScmObj* %_37length47086, %struct.ScmObj** %stackaddr$prim60137, align 8
%stackaddr$makeclosure60138 = alloca %struct.ScmObj*, align 8
%fptrToInt60139 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48155 to i64
%ae48155 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt60139)
store volatile %struct.ScmObj* %ae48155, %struct.ScmObj** %stackaddr$makeclosure60138, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48155, %struct.ScmObj* %_37map147093, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48155, %struct.ScmObj* %Ycmb47076, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48155, %struct.ScmObj* %_37take47089, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48155, %struct.ScmObj* %_37length47086, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48155, %struct.ScmObj* %_37foldr147097, i64 4)
%ae48156 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure60140 = alloca %struct.ScmObj*, align 8
%fptrToInt60141 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48157 to i64
%ae48157 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt60141)
store volatile %struct.ScmObj* %ae48157, %struct.ScmObj** %stackaddr$makeclosure60140, align 8
%argslist59916$ae481550 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60142 = alloca %struct.ScmObj*, align 8
%argslist59916$ae481551 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48157, %struct.ScmObj* %argslist59916$ae481550)
store volatile %struct.ScmObj* %argslist59916$ae481551, %struct.ScmObj** %stackaddr$prim60142, align 8
%stackaddr$prim60143 = alloca %struct.ScmObj*, align 8
%argslist59916$ae481552 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48156, %struct.ScmObj* %argslist59916$ae481551)
store volatile %struct.ScmObj* %argslist59916$ae481552, %struct.ScmObj** %stackaddr$prim60143, align 8
%clofunc60144 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48155)
musttail call tailcc void %clofunc60144(%struct.ScmObj* %ae48155, %struct.ScmObj* %argslist59916$ae481552)
ret void
}

define tailcc void @proc_clo$ae48155(%struct.ScmObj* %env$ae48155,%struct.ScmObj* %current_45args59304) {
%stackaddr$env-ref60145 = alloca %struct.ScmObj*, align 8
%_37map147093 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48155, i64 0)
store %struct.ScmObj* %_37map147093, %struct.ScmObj** %stackaddr$env-ref60145
%stackaddr$env-ref60146 = alloca %struct.ScmObj*, align 8
%Ycmb47076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48155, i64 1)
store %struct.ScmObj* %Ycmb47076, %struct.ScmObj** %stackaddr$env-ref60146
%stackaddr$env-ref60147 = alloca %struct.ScmObj*, align 8
%_37take47089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48155, i64 2)
store %struct.ScmObj* %_37take47089, %struct.ScmObj** %stackaddr$env-ref60147
%stackaddr$env-ref60148 = alloca %struct.ScmObj*, align 8
%_37length47086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48155, i64 3)
store %struct.ScmObj* %_37length47086, %struct.ScmObj** %stackaddr$env-ref60148
%stackaddr$env-ref60149 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48155, i64 4)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref60149
%stackaddr$prim60150 = alloca %struct.ScmObj*, align 8
%_95k47410 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59304)
store volatile %struct.ScmObj* %_95k47410, %struct.ScmObj** %stackaddr$prim60150, align 8
%stackaddr$prim60151 = alloca %struct.ScmObj*, align 8
%current_45args59305 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59304)
store volatile %struct.ScmObj* %current_45args59305, %struct.ScmObj** %stackaddr$prim60151, align 8
%stackaddr$prim60152 = alloca %struct.ScmObj*, align 8
%anf_45bind47255 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59305)
store volatile %struct.ScmObj* %anf_45bind47255, %struct.ScmObj** %stackaddr$prim60152, align 8
%stackaddr$makeclosure60153 = alloca %struct.ScmObj*, align 8
%fptrToInt60154 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48232 to i64
%ae48232 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt60154)
store volatile %struct.ScmObj* %ae48232, %struct.ScmObj** %stackaddr$makeclosure60153, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48232, %struct.ScmObj* %_37map147093, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48232, %struct.ScmObj* %Ycmb47076, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48232, %struct.ScmObj* %_37take47089, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48232, %struct.ScmObj* %_37length47086, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48232, %struct.ScmObj* %_37foldr147097, i64 4)
%argslist59900$Ycmb470760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60155 = alloca %struct.ScmObj*, align 8
%argslist59900$Ycmb470761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47255, %struct.ScmObj* %argslist59900$Ycmb470760)
store volatile %struct.ScmObj* %argslist59900$Ycmb470761, %struct.ScmObj** %stackaddr$prim60155, align 8
%stackaddr$prim60156 = alloca %struct.ScmObj*, align 8
%argslist59900$Ycmb470762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48232, %struct.ScmObj* %argslist59900$Ycmb470761)
store volatile %struct.ScmObj* %argslist59900$Ycmb470762, %struct.ScmObj** %stackaddr$prim60156, align 8
%clofunc60157 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47076)
musttail call tailcc void %clofunc60157(%struct.ScmObj* %Ycmb47076, %struct.ScmObj* %argslist59900$Ycmb470762)
ret void
}

define tailcc void @proc_clo$ae48232(%struct.ScmObj* %env$ae48232,%struct.ScmObj* %current_45args59307) {
%stackaddr$env-ref60158 = alloca %struct.ScmObj*, align 8
%_37map147093 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48232, i64 0)
store %struct.ScmObj* %_37map147093, %struct.ScmObj** %stackaddr$env-ref60158
%stackaddr$env-ref60159 = alloca %struct.ScmObj*, align 8
%Ycmb47076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48232, i64 1)
store %struct.ScmObj* %Ycmb47076, %struct.ScmObj** %stackaddr$env-ref60159
%stackaddr$env-ref60160 = alloca %struct.ScmObj*, align 8
%_37take47089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48232, i64 2)
store %struct.ScmObj* %_37take47089, %struct.ScmObj** %stackaddr$env-ref60160
%stackaddr$env-ref60161 = alloca %struct.ScmObj*, align 8
%_37length47086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48232, i64 3)
store %struct.ScmObj* %_37length47086, %struct.ScmObj** %stackaddr$env-ref60161
%stackaddr$env-ref60162 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48232, i64 4)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref60162
%stackaddr$prim60163 = alloca %struct.ScmObj*, align 8
%_95k47411 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59307)
store volatile %struct.ScmObj* %_95k47411, %struct.ScmObj** %stackaddr$prim60163, align 8
%stackaddr$prim60164 = alloca %struct.ScmObj*, align 8
%current_45args59308 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59307)
store volatile %struct.ScmObj* %current_45args59308, %struct.ScmObj** %stackaddr$prim60164, align 8
%stackaddr$prim60165 = alloca %struct.ScmObj*, align 8
%_37foldl147081 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59308)
store volatile %struct.ScmObj* %_37foldl147081, %struct.ScmObj** %stackaddr$prim60165, align 8
%stackaddr$makeclosure60166 = alloca %struct.ScmObj*, align 8
%fptrToInt60167 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48234 to i64
%ae48234 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt60167)
store volatile %struct.ScmObj* %ae48234, %struct.ScmObj** %stackaddr$makeclosure60166, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48234, %struct.ScmObj* %_37foldr147097, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48234, %struct.ScmObj* %_37foldl147081, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48234, %struct.ScmObj* %_37map147093, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48234, %struct.ScmObj* %Ycmb47076, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48234, %struct.ScmObj* %_37take47089, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48234, %struct.ScmObj* %_37length47086, i64 5)
%ae48235 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure60168 = alloca %struct.ScmObj*, align 8
%fptrToInt60169 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48236 to i64
%ae48236 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt60169)
store volatile %struct.ScmObj* %ae48236, %struct.ScmObj** %stackaddr$makeclosure60168, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48236, %struct.ScmObj* %_37foldl147081, i64 0)
%argslist59899$ae482340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60170 = alloca %struct.ScmObj*, align 8
%argslist59899$ae482341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48236, %struct.ScmObj* %argslist59899$ae482340)
store volatile %struct.ScmObj* %argslist59899$ae482341, %struct.ScmObj** %stackaddr$prim60170, align 8
%stackaddr$prim60171 = alloca %struct.ScmObj*, align 8
%argslist59899$ae482342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48235, %struct.ScmObj* %argslist59899$ae482341)
store volatile %struct.ScmObj* %argslist59899$ae482342, %struct.ScmObj** %stackaddr$prim60171, align 8
%clofunc60172 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48234)
musttail call tailcc void %clofunc60172(%struct.ScmObj* %ae48234, %struct.ScmObj* %argslist59899$ae482342)
ret void
}

define tailcc void @proc_clo$ae48234(%struct.ScmObj* %env$ae48234,%struct.ScmObj* %current_45args59310) {
%stackaddr$env-ref60173 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48234, i64 0)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref60173
%stackaddr$env-ref60174 = alloca %struct.ScmObj*, align 8
%_37foldl147081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48234, i64 1)
store %struct.ScmObj* %_37foldl147081, %struct.ScmObj** %stackaddr$env-ref60174
%stackaddr$env-ref60175 = alloca %struct.ScmObj*, align 8
%_37map147093 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48234, i64 2)
store %struct.ScmObj* %_37map147093, %struct.ScmObj** %stackaddr$env-ref60175
%stackaddr$env-ref60176 = alloca %struct.ScmObj*, align 8
%Ycmb47076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48234, i64 3)
store %struct.ScmObj* %Ycmb47076, %struct.ScmObj** %stackaddr$env-ref60176
%stackaddr$env-ref60177 = alloca %struct.ScmObj*, align 8
%_37take47089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48234, i64 4)
store %struct.ScmObj* %_37take47089, %struct.ScmObj** %stackaddr$env-ref60177
%stackaddr$env-ref60178 = alloca %struct.ScmObj*, align 8
%_37length47086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48234, i64 5)
store %struct.ScmObj* %_37length47086, %struct.ScmObj** %stackaddr$env-ref60178
%stackaddr$prim60179 = alloca %struct.ScmObj*, align 8
%_95k47412 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59310)
store volatile %struct.ScmObj* %_95k47412, %struct.ScmObj** %stackaddr$prim60179, align 8
%stackaddr$prim60180 = alloca %struct.ScmObj*, align 8
%current_45args59311 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59310)
store volatile %struct.ScmObj* %current_45args59311, %struct.ScmObj** %stackaddr$prim60180, align 8
%stackaddr$prim60181 = alloca %struct.ScmObj*, align 8
%_37last47119 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59311)
store volatile %struct.ScmObj* %_37last47119, %struct.ScmObj** %stackaddr$prim60181, align 8
%stackaddr$makeclosure60182 = alloca %struct.ScmObj*, align 8
%fptrToInt60183 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48288 to i64
%ae48288 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt60183)
store volatile %struct.ScmObj* %ae48288, %struct.ScmObj** %stackaddr$makeclosure60182, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48288, %struct.ScmObj* %_37foldr147097, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48288, %struct.ScmObj* %_37foldl147081, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48288, %struct.ScmObj* %_37map147093, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48288, %struct.ScmObj* %Ycmb47076, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48288, %struct.ScmObj* %_37last47119, i64 4)
%ae48289 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure60184 = alloca %struct.ScmObj*, align 8
%fptrToInt60185 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48290 to i64
%ae48290 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt60185)
store volatile %struct.ScmObj* %ae48290, %struct.ScmObj** %stackaddr$makeclosure60184, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48290, %struct.ScmObj* %_37take47089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48290, %struct.ScmObj* %_37length47086, i64 1)
%argslist59885$ae482880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60186 = alloca %struct.ScmObj*, align 8
%argslist59885$ae482881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48290, %struct.ScmObj* %argslist59885$ae482880)
store volatile %struct.ScmObj* %argslist59885$ae482881, %struct.ScmObj** %stackaddr$prim60186, align 8
%stackaddr$prim60187 = alloca %struct.ScmObj*, align 8
%argslist59885$ae482882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48289, %struct.ScmObj* %argslist59885$ae482881)
store volatile %struct.ScmObj* %argslist59885$ae482882, %struct.ScmObj** %stackaddr$prim60187, align 8
%clofunc60188 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48288)
musttail call tailcc void %clofunc60188(%struct.ScmObj* %ae48288, %struct.ScmObj* %argslist59885$ae482882)
ret void
}

define tailcc void @proc_clo$ae48288(%struct.ScmObj* %env$ae48288,%struct.ScmObj* %current_45args59313) {
%stackaddr$env-ref60189 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48288, i64 0)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref60189
%stackaddr$env-ref60190 = alloca %struct.ScmObj*, align 8
%_37foldl147081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48288, i64 1)
store %struct.ScmObj* %_37foldl147081, %struct.ScmObj** %stackaddr$env-ref60190
%stackaddr$env-ref60191 = alloca %struct.ScmObj*, align 8
%_37map147093 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48288, i64 2)
store %struct.ScmObj* %_37map147093, %struct.ScmObj** %stackaddr$env-ref60191
%stackaddr$env-ref60192 = alloca %struct.ScmObj*, align 8
%Ycmb47076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48288, i64 3)
store %struct.ScmObj* %Ycmb47076, %struct.ScmObj** %stackaddr$env-ref60192
%stackaddr$env-ref60193 = alloca %struct.ScmObj*, align 8
%_37last47119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48288, i64 4)
store %struct.ScmObj* %_37last47119, %struct.ScmObj** %stackaddr$env-ref60193
%stackaddr$prim60194 = alloca %struct.ScmObj*, align 8
%_95k47413 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59313)
store volatile %struct.ScmObj* %_95k47413, %struct.ScmObj** %stackaddr$prim60194, align 8
%stackaddr$prim60195 = alloca %struct.ScmObj*, align 8
%current_45args59314 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59313)
store volatile %struct.ScmObj* %current_45args59314, %struct.ScmObj** %stackaddr$prim60195, align 8
%stackaddr$prim60196 = alloca %struct.ScmObj*, align 8
%_37drop_45right47116 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59314)
store volatile %struct.ScmObj* %_37drop_45right47116, %struct.ScmObj** %stackaddr$prim60196, align 8
%stackaddr$makeclosure60197 = alloca %struct.ScmObj*, align 8
%fptrToInt60198 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48318 to i64
%ae48318 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt60198)
store volatile %struct.ScmObj* %ae48318, %struct.ScmObj** %stackaddr$makeclosure60197, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48318, %struct.ScmObj* %_37foldr147097, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48318, %struct.ScmObj* %_37foldl147081, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48318, %struct.ScmObj* %Ycmb47076, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48318, %struct.ScmObj* %_37last47119, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48318, %struct.ScmObj* %_37drop_45right47116, i64 4)
%ae48319 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure60199 = alloca %struct.ScmObj*, align 8
%fptrToInt60200 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48320 to i64
%ae48320 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt60200)
store volatile %struct.ScmObj* %ae48320, %struct.ScmObj** %stackaddr$makeclosure60199, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48320, %struct.ScmObj* %_37map147093, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48320, %struct.ScmObj* %_37foldr147097, i64 1)
%argslist59875$ae483180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60201 = alloca %struct.ScmObj*, align 8
%argslist59875$ae483181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48320, %struct.ScmObj* %argslist59875$ae483180)
store volatile %struct.ScmObj* %argslist59875$ae483181, %struct.ScmObj** %stackaddr$prim60201, align 8
%stackaddr$prim60202 = alloca %struct.ScmObj*, align 8
%argslist59875$ae483182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48319, %struct.ScmObj* %argslist59875$ae483181)
store volatile %struct.ScmObj* %argslist59875$ae483182, %struct.ScmObj** %stackaddr$prim60202, align 8
%clofunc60203 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48318)
musttail call tailcc void %clofunc60203(%struct.ScmObj* %ae48318, %struct.ScmObj* %argslist59875$ae483182)
ret void
}

define tailcc void @proc_clo$ae48318(%struct.ScmObj* %env$ae48318,%struct.ScmObj* %current_45args59316) {
%stackaddr$env-ref60204 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48318, i64 0)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref60204
%stackaddr$env-ref60205 = alloca %struct.ScmObj*, align 8
%_37foldl147081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48318, i64 1)
store %struct.ScmObj* %_37foldl147081, %struct.ScmObj** %stackaddr$env-ref60205
%stackaddr$env-ref60206 = alloca %struct.ScmObj*, align 8
%Ycmb47076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48318, i64 2)
store %struct.ScmObj* %Ycmb47076, %struct.ScmObj** %stackaddr$env-ref60206
%stackaddr$env-ref60207 = alloca %struct.ScmObj*, align 8
%_37last47119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48318, i64 3)
store %struct.ScmObj* %_37last47119, %struct.ScmObj** %stackaddr$env-ref60207
%stackaddr$env-ref60208 = alloca %struct.ScmObj*, align 8
%_37drop_45right47116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48318, i64 4)
store %struct.ScmObj* %_37drop_45right47116, %struct.ScmObj** %stackaddr$env-ref60208
%stackaddr$prim60209 = alloca %struct.ScmObj*, align 8
%_95k47414 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59316)
store volatile %struct.ScmObj* %_95k47414, %struct.ScmObj** %stackaddr$prim60209, align 8
%stackaddr$prim60210 = alloca %struct.ScmObj*, align 8
%current_45args59317 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59316)
store volatile %struct.ScmObj* %current_45args59317, %struct.ScmObj** %stackaddr$prim60210, align 8
%stackaddr$prim60211 = alloca %struct.ScmObj*, align 8
%anf_45bind47271 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59317)
store volatile %struct.ScmObj* %anf_45bind47271, %struct.ScmObj** %stackaddr$prim60211, align 8
%stackaddr$makeclosure60212 = alloca %struct.ScmObj*, align 8
%fptrToInt60213 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48702 to i64
%ae48702 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt60213)
store volatile %struct.ScmObj* %ae48702, %struct.ScmObj** %stackaddr$makeclosure60212, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48702, %struct.ScmObj* %_37foldr147097, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48702, %struct.ScmObj* %_37foldl147081, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48702, %struct.ScmObj* %Ycmb47076, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48702, %struct.ScmObj* %_37last47119, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48702, %struct.ScmObj* %_37drop_45right47116, i64 4)
%argslist59815$Ycmb470760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60214 = alloca %struct.ScmObj*, align 8
%argslist59815$Ycmb470761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47271, %struct.ScmObj* %argslist59815$Ycmb470760)
store volatile %struct.ScmObj* %argslist59815$Ycmb470761, %struct.ScmObj** %stackaddr$prim60214, align 8
%stackaddr$prim60215 = alloca %struct.ScmObj*, align 8
%argslist59815$Ycmb470762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48702, %struct.ScmObj* %argslist59815$Ycmb470761)
store volatile %struct.ScmObj* %argslist59815$Ycmb470762, %struct.ScmObj** %stackaddr$prim60215, align 8
%clofunc60216 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47076)
musttail call tailcc void %clofunc60216(%struct.ScmObj* %Ycmb47076, %struct.ScmObj* %argslist59815$Ycmb470762)
ret void
}

define tailcc void @proc_clo$ae48702(%struct.ScmObj* %env$ae48702,%struct.ScmObj* %current_45args59319) {
%stackaddr$env-ref60217 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48702, i64 0)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref60217
%stackaddr$env-ref60218 = alloca %struct.ScmObj*, align 8
%_37foldl147081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48702, i64 1)
store %struct.ScmObj* %_37foldl147081, %struct.ScmObj** %stackaddr$env-ref60218
%stackaddr$env-ref60219 = alloca %struct.ScmObj*, align 8
%Ycmb47076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48702, i64 2)
store %struct.ScmObj* %Ycmb47076, %struct.ScmObj** %stackaddr$env-ref60219
%stackaddr$env-ref60220 = alloca %struct.ScmObj*, align 8
%_37last47119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48702, i64 3)
store %struct.ScmObj* %_37last47119, %struct.ScmObj** %stackaddr$env-ref60220
%stackaddr$env-ref60221 = alloca %struct.ScmObj*, align 8
%_37drop_45right47116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48702, i64 4)
store %struct.ScmObj* %_37drop_45right47116, %struct.ScmObj** %stackaddr$env-ref60221
%stackaddr$prim60222 = alloca %struct.ScmObj*, align 8
%_95k47415 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59319)
store volatile %struct.ScmObj* %_95k47415, %struct.ScmObj** %stackaddr$prim60222, align 8
%stackaddr$prim60223 = alloca %struct.ScmObj*, align 8
%current_45args59320 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59319)
store volatile %struct.ScmObj* %current_45args59320, %struct.ScmObj** %stackaddr$prim60223, align 8
%stackaddr$prim60224 = alloca %struct.ScmObj*, align 8
%_37foldr47102 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59320)
store volatile %struct.ScmObj* %_37foldr47102, %struct.ScmObj** %stackaddr$prim60224, align 8
%stackaddr$makeclosure60225 = alloca %struct.ScmObj*, align 8
%fptrToInt60226 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48704 to i64
%ae48704 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt60226)
store volatile %struct.ScmObj* %ae48704, %struct.ScmObj** %stackaddr$makeclosure60225, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48704, %struct.ScmObj* %_37foldr147097, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48704, %struct.ScmObj* %_37foldl147081, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48704, %struct.ScmObj* %Ycmb47076, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48704, %struct.ScmObj* %_37last47119, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48704, %struct.ScmObj* %_37foldr47102, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48704, %struct.ScmObj* %_37drop_45right47116, i64 5)
%ae48705 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure60227 = alloca %struct.ScmObj*, align 8
%fptrToInt60228 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48706 to i64
%ae48706 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt60228)
store volatile %struct.ScmObj* %ae48706, %struct.ScmObj** %stackaddr$makeclosure60227, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48706, %struct.ScmObj* %_37foldr147097, i64 0)
%argslist59814$ae487040 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60229 = alloca %struct.ScmObj*, align 8
%argslist59814$ae487041 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48706, %struct.ScmObj* %argslist59814$ae487040)
store volatile %struct.ScmObj* %argslist59814$ae487041, %struct.ScmObj** %stackaddr$prim60229, align 8
%stackaddr$prim60230 = alloca %struct.ScmObj*, align 8
%argslist59814$ae487042 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48705, %struct.ScmObj* %argslist59814$ae487041)
store volatile %struct.ScmObj* %argslist59814$ae487042, %struct.ScmObj** %stackaddr$prim60230, align 8
%clofunc60231 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48704)
musttail call tailcc void %clofunc60231(%struct.ScmObj* %ae48704, %struct.ScmObj* %argslist59814$ae487042)
ret void
}

define tailcc void @proc_clo$ae48704(%struct.ScmObj* %env$ae48704,%struct.ScmObj* %current_45args59322) {
%stackaddr$env-ref60232 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48704, i64 0)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref60232
%stackaddr$env-ref60233 = alloca %struct.ScmObj*, align 8
%_37foldl147081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48704, i64 1)
store %struct.ScmObj* %_37foldl147081, %struct.ScmObj** %stackaddr$env-ref60233
%stackaddr$env-ref60234 = alloca %struct.ScmObj*, align 8
%Ycmb47076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48704, i64 2)
store %struct.ScmObj* %Ycmb47076, %struct.ScmObj** %stackaddr$env-ref60234
%stackaddr$env-ref60235 = alloca %struct.ScmObj*, align 8
%_37last47119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48704, i64 3)
store %struct.ScmObj* %_37last47119, %struct.ScmObj** %stackaddr$env-ref60235
%stackaddr$env-ref60236 = alloca %struct.ScmObj*, align 8
%_37foldr47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48704, i64 4)
store %struct.ScmObj* %_37foldr47102, %struct.ScmObj** %stackaddr$env-ref60236
%stackaddr$env-ref60237 = alloca %struct.ScmObj*, align 8
%_37drop_45right47116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48704, i64 5)
store %struct.ScmObj* %_37drop_45right47116, %struct.ScmObj** %stackaddr$env-ref60237
%stackaddr$prim60238 = alloca %struct.ScmObj*, align 8
%_95k47416 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59322)
store volatile %struct.ScmObj* %_95k47416, %struct.ScmObj** %stackaddr$prim60238, align 8
%stackaddr$prim60239 = alloca %struct.ScmObj*, align 8
%current_45args59323 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59322)
store volatile %struct.ScmObj* %current_45args59323, %struct.ScmObj** %stackaddr$prim60239, align 8
%stackaddr$prim60240 = alloca %struct.ScmObj*, align 8
%_37map147128 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59323)
store volatile %struct.ScmObj* %_37map147128, %struct.ScmObj** %stackaddr$prim60240, align 8
%stackaddr$makeclosure60241 = alloca %struct.ScmObj*, align 8
%fptrToInt60242 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48781 to i64
%ae48781 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt60242)
store volatile %struct.ScmObj* %ae48781, %struct.ScmObj** %stackaddr$makeclosure60241, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48781, %struct.ScmObj* %_37foldr147097, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48781, %struct.ScmObj* %_37foldl147081, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48781, %struct.ScmObj* %Ycmb47076, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48781, %struct.ScmObj* %_37foldr47102, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48781, %struct.ScmObj* %_37map147128, i64 4)
%ae48782 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure60243 = alloca %struct.ScmObj*, align 8
%fptrToInt60244 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48783 to i64
%ae48783 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt60244)
store volatile %struct.ScmObj* %ae48783, %struct.ScmObj** %stackaddr$makeclosure60243, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48783, %struct.ScmObj* %_37last47119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48783, %struct.ScmObj* %_37foldr47102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48783, %struct.ScmObj* %_37drop_45right47116, i64 2)
%argslist59795$ae487810 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60245 = alloca %struct.ScmObj*, align 8
%argslist59795$ae487811 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48783, %struct.ScmObj* %argslist59795$ae487810)
store volatile %struct.ScmObj* %argslist59795$ae487811, %struct.ScmObj** %stackaddr$prim60245, align 8
%stackaddr$prim60246 = alloca %struct.ScmObj*, align 8
%argslist59795$ae487812 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48782, %struct.ScmObj* %argslist59795$ae487811)
store volatile %struct.ScmObj* %argslist59795$ae487812, %struct.ScmObj** %stackaddr$prim60246, align 8
%clofunc60247 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48781)
musttail call tailcc void %clofunc60247(%struct.ScmObj* %ae48781, %struct.ScmObj* %argslist59795$ae487812)
ret void
}

define tailcc void @proc_clo$ae48781(%struct.ScmObj* %env$ae48781,%struct.ScmObj* %current_45args59325) {
%stackaddr$env-ref60248 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48781, i64 0)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref60248
%stackaddr$env-ref60249 = alloca %struct.ScmObj*, align 8
%_37foldl147081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48781, i64 1)
store %struct.ScmObj* %_37foldl147081, %struct.ScmObj** %stackaddr$env-ref60249
%stackaddr$env-ref60250 = alloca %struct.ScmObj*, align 8
%Ycmb47076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48781, i64 2)
store %struct.ScmObj* %Ycmb47076, %struct.ScmObj** %stackaddr$env-ref60250
%stackaddr$env-ref60251 = alloca %struct.ScmObj*, align 8
%_37foldr47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48781, i64 3)
store %struct.ScmObj* %_37foldr47102, %struct.ScmObj** %stackaddr$env-ref60251
%stackaddr$env-ref60252 = alloca %struct.ScmObj*, align 8
%_37map147128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48781, i64 4)
store %struct.ScmObj* %_37map147128, %struct.ScmObj** %stackaddr$env-ref60252
%stackaddr$prim60253 = alloca %struct.ScmObj*, align 8
%_95k47417 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59325)
store volatile %struct.ScmObj* %_95k47417, %struct.ScmObj** %stackaddr$prim60253, align 8
%stackaddr$prim60254 = alloca %struct.ScmObj*, align 8
%current_45args59326 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59325)
store volatile %struct.ScmObj* %current_45args59326, %struct.ScmObj** %stackaddr$prim60254, align 8
%stackaddr$prim60255 = alloca %struct.ScmObj*, align 8
%_37map47123 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59326)
store volatile %struct.ScmObj* %_37map47123, %struct.ScmObj** %stackaddr$prim60255, align 8
%stackaddr$makeclosure60256 = alloca %struct.ScmObj*, align 8
%fptrToInt60257 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48927 to i64
%ae48927 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt60257)
store volatile %struct.ScmObj* %ae48927, %struct.ScmObj** %stackaddr$makeclosure60256, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48927, %struct.ScmObj* %Ycmb47076, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48927, %struct.ScmObj* %_37foldl147081, i64 1)
%ae48928 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure60258 = alloca %struct.ScmObj*, align 8
%fptrToInt60259 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48929 to i64
%ae48929 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt60259)
store volatile %struct.ScmObj* %ae48929, %struct.ScmObj** %stackaddr$makeclosure60258, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48929, %struct.ScmObj* %_37foldr47102, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48929, %struct.ScmObj* %_37foldr147097, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48929, %struct.ScmObj* %_37map147128, i64 2)
%argslist59778$ae489270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60260 = alloca %struct.ScmObj*, align 8
%argslist59778$ae489271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48929, %struct.ScmObj* %argslist59778$ae489270)
store volatile %struct.ScmObj* %argslist59778$ae489271, %struct.ScmObj** %stackaddr$prim60260, align 8
%stackaddr$prim60261 = alloca %struct.ScmObj*, align 8
%argslist59778$ae489272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48928, %struct.ScmObj* %argslist59778$ae489271)
store volatile %struct.ScmObj* %argslist59778$ae489272, %struct.ScmObj** %stackaddr$prim60261, align 8
%clofunc60262 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48927)
musttail call tailcc void %clofunc60262(%struct.ScmObj* %ae48927, %struct.ScmObj* %argslist59778$ae489272)
ret void
}

define tailcc void @proc_clo$ae48927(%struct.ScmObj* %env$ae48927,%struct.ScmObj* %current_45args59328) {
%stackaddr$env-ref60263 = alloca %struct.ScmObj*, align 8
%Ycmb47076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48927, i64 0)
store %struct.ScmObj* %Ycmb47076, %struct.ScmObj** %stackaddr$env-ref60263
%stackaddr$env-ref60264 = alloca %struct.ScmObj*, align 8
%_37foldl147081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48927, i64 1)
store %struct.ScmObj* %_37foldl147081, %struct.ScmObj** %stackaddr$env-ref60264
%stackaddr$prim60265 = alloca %struct.ScmObj*, align 8
%_95k47418 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59328)
store volatile %struct.ScmObj* %_95k47418, %struct.ScmObj** %stackaddr$prim60265, align 8
%stackaddr$prim60266 = alloca %struct.ScmObj*, align 8
%current_45args59329 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59328)
store volatile %struct.ScmObj* %current_45args59329, %struct.ScmObj** %stackaddr$prim60266, align 8
%stackaddr$prim60267 = alloca %struct.ScmObj*, align 8
%anf_45bind47291 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59329)
store volatile %struct.ScmObj* %anf_45bind47291, %struct.ScmObj** %stackaddr$prim60267, align 8
%stackaddr$makeclosure60268 = alloca %struct.ScmObj*, align 8
%fptrToInt60269 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49319 to i64
%ae49319 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt60269)
store volatile %struct.ScmObj* %ae49319, %struct.ScmObj** %stackaddr$makeclosure60268, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49319, %struct.ScmObj* %_37foldl147081, i64 0)
%argslist59718$Ycmb470760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60270 = alloca %struct.ScmObj*, align 8
%argslist59718$Ycmb470761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47291, %struct.ScmObj* %argslist59718$Ycmb470760)
store volatile %struct.ScmObj* %argslist59718$Ycmb470761, %struct.ScmObj** %stackaddr$prim60270, align 8
%stackaddr$prim60271 = alloca %struct.ScmObj*, align 8
%argslist59718$Ycmb470762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49319, %struct.ScmObj* %argslist59718$Ycmb470761)
store volatile %struct.ScmObj* %argslist59718$Ycmb470762, %struct.ScmObj** %stackaddr$prim60271, align 8
%clofunc60272 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47076)
musttail call tailcc void %clofunc60272(%struct.ScmObj* %Ycmb47076, %struct.ScmObj* %argslist59718$Ycmb470762)
ret void
}

define tailcc void @proc_clo$ae49319(%struct.ScmObj* %env$ae49319,%struct.ScmObj* %current_45args59331) {
%stackaddr$env-ref60273 = alloca %struct.ScmObj*, align 8
%_37foldl147081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49319, i64 0)
store %struct.ScmObj* %_37foldl147081, %struct.ScmObj** %stackaddr$env-ref60273
%stackaddr$prim60274 = alloca %struct.ScmObj*, align 8
%_95k47419 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59331)
store volatile %struct.ScmObj* %_95k47419, %struct.ScmObj** %stackaddr$prim60274, align 8
%stackaddr$prim60275 = alloca %struct.ScmObj*, align 8
%current_45args59332 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59331)
store volatile %struct.ScmObj* %current_45args59332, %struct.ScmObj** %stackaddr$prim60275, align 8
%stackaddr$prim60276 = alloca %struct.ScmObj*, align 8
%_37foldl47179 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59332)
store volatile %struct.ScmObj* %_37foldl47179, %struct.ScmObj** %stackaddr$prim60276, align 8
%stackaddr$makeclosure60277 = alloca %struct.ScmObj*, align 8
%fptrToInt60278 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49321 to i64
%ae49321 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt60278)
store volatile %struct.ScmObj* %ae49321, %struct.ScmObj** %stackaddr$makeclosure60277, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49321, %struct.ScmObj* %_37foldl147081, i64 0)
%ae49322 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure60279 = alloca %struct.ScmObj*, align 8
%fptrToInt60280 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49323 to i64
%ae49323 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt60280)
store volatile %struct.ScmObj* %ae49323, %struct.ScmObj** %stackaddr$makeclosure60279, align 8
%argslist59717$ae493210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60281 = alloca %struct.ScmObj*, align 8
%argslist59717$ae493211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49323, %struct.ScmObj* %argslist59717$ae493210)
store volatile %struct.ScmObj* %argslist59717$ae493211, %struct.ScmObj** %stackaddr$prim60281, align 8
%stackaddr$prim60282 = alloca %struct.ScmObj*, align 8
%argslist59717$ae493212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49322, %struct.ScmObj* %argslist59717$ae493211)
store volatile %struct.ScmObj* %argslist59717$ae493212, %struct.ScmObj** %stackaddr$prim60282, align 8
%clofunc60283 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49321)
musttail call tailcc void %clofunc60283(%struct.ScmObj* %ae49321, %struct.ScmObj* %argslist59717$ae493212)
ret void
}

define tailcc void @proc_clo$ae49321(%struct.ScmObj* %env$ae49321,%struct.ScmObj* %current_45args59334) {
%stackaddr$env-ref60284 = alloca %struct.ScmObj*, align 8
%_37foldl147081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49321, i64 0)
store %struct.ScmObj* %_37foldl147081, %struct.ScmObj** %stackaddr$env-ref60284
%stackaddr$prim60285 = alloca %struct.ScmObj*, align 8
%_95k47420 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59334)
store volatile %struct.ScmObj* %_95k47420, %struct.ScmObj** %stackaddr$prim60285, align 8
%stackaddr$prim60286 = alloca %struct.ScmObj*, align 8
%current_45args59335 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59334)
store volatile %struct.ScmObj* %current_45args59335, %struct.ScmObj** %stackaddr$prim60286, align 8
%stackaddr$prim60287 = alloca %struct.ScmObj*, align 8
%_37_6247176 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59335)
store volatile %struct.ScmObj* %_37_6247176, %struct.ScmObj** %stackaddr$prim60287, align 8
%stackaddr$makeclosure60288 = alloca %struct.ScmObj*, align 8
%fptrToInt60289 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49345 to i64
%ae49345 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt60289)
store volatile %struct.ScmObj* %ae49345, %struct.ScmObj** %stackaddr$makeclosure60288, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49345, %struct.ScmObj* %_37foldl147081, i64 0)
%ae49346 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure60290 = alloca %struct.ScmObj*, align 8
%fptrToInt60291 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49347 to i64
%ae49347 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt60291)
store volatile %struct.ScmObj* %ae49347, %struct.ScmObj** %stackaddr$makeclosure60290, align 8
%argslist59711$ae493450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60292 = alloca %struct.ScmObj*, align 8
%argslist59711$ae493451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49347, %struct.ScmObj* %argslist59711$ae493450)
store volatile %struct.ScmObj* %argslist59711$ae493451, %struct.ScmObj** %stackaddr$prim60292, align 8
%stackaddr$prim60293 = alloca %struct.ScmObj*, align 8
%argslist59711$ae493452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49346, %struct.ScmObj* %argslist59711$ae493451)
store volatile %struct.ScmObj* %argslist59711$ae493452, %struct.ScmObj** %stackaddr$prim60293, align 8
%clofunc60294 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49345)
musttail call tailcc void %clofunc60294(%struct.ScmObj* %ae49345, %struct.ScmObj* %argslist59711$ae493452)
ret void
}

define tailcc void @proc_clo$ae49345(%struct.ScmObj* %env$ae49345,%struct.ScmObj* %current_45args59337) {
%stackaddr$env-ref60295 = alloca %struct.ScmObj*, align 8
%_37foldl147081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49345, i64 0)
store %struct.ScmObj* %_37foldl147081, %struct.ScmObj** %stackaddr$env-ref60295
%stackaddr$prim60296 = alloca %struct.ScmObj*, align 8
%_95k47421 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59337)
store volatile %struct.ScmObj* %_95k47421, %struct.ScmObj** %stackaddr$prim60296, align 8
%stackaddr$prim60297 = alloca %struct.ScmObj*, align 8
%current_45args59338 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59337)
store volatile %struct.ScmObj* %current_45args59338, %struct.ScmObj** %stackaddr$prim60297, align 8
%stackaddr$prim60298 = alloca %struct.ScmObj*, align 8
%_37_62_6147173 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59338)
store volatile %struct.ScmObj* %_37_62_6147173, %struct.ScmObj** %stackaddr$prim60298, align 8
%ae49369 = call %struct.ScmObj* @const_init_int(i64 1)
%ae49370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60299 = alloca %struct.ScmObj*, align 8
%_37append47169 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49369, %struct.ScmObj* %ae49370)
store volatile %struct.ScmObj* %_37append47169, %struct.ScmObj** %stackaddr$prim60299, align 8
%stackaddr$makeclosure60300 = alloca %struct.ScmObj*, align 8
%fptrToInt60301 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49371 to i64
%ae49371 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt60301)
store volatile %struct.ScmObj* %ae49371, %struct.ScmObj** %stackaddr$makeclosure60300, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49371, %struct.ScmObj* %_37append47169, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49371, %struct.ScmObj* %_37foldl147081, i64 1)
%ae49372 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure60302 = alloca %struct.ScmObj*, align 8
%fptrToInt60303 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49373 to i64
%ae49373 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt60303)
store volatile %struct.ScmObj* %ae49373, %struct.ScmObj** %stackaddr$makeclosure60302, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49373, %struct.ScmObj* %_37append47169, i64 0)
%argslist59705$ae493710 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60304 = alloca %struct.ScmObj*, align 8
%argslist59705$ae493711 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49373, %struct.ScmObj* %argslist59705$ae493710)
store volatile %struct.ScmObj* %argslist59705$ae493711, %struct.ScmObj** %stackaddr$prim60304, align 8
%stackaddr$prim60305 = alloca %struct.ScmObj*, align 8
%argslist59705$ae493712 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49372, %struct.ScmObj* %argslist59705$ae493711)
store volatile %struct.ScmObj* %argslist59705$ae493712, %struct.ScmObj** %stackaddr$prim60305, align 8
%clofunc60306 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49371)
musttail call tailcc void %clofunc60306(%struct.ScmObj* %ae49371, %struct.ScmObj* %argslist59705$ae493712)
ret void
}

define tailcc void @proc_clo$ae49371(%struct.ScmObj* %env$ae49371,%struct.ScmObj* %current_45args59340) {
%stackaddr$env-ref60307 = alloca %struct.ScmObj*, align 8
%_37append47169 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49371, i64 0)
store %struct.ScmObj* %_37append47169, %struct.ScmObj** %stackaddr$env-ref60307
%stackaddr$env-ref60308 = alloca %struct.ScmObj*, align 8
%_37foldl147081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49371, i64 1)
store %struct.ScmObj* %_37foldl147081, %struct.ScmObj** %stackaddr$env-ref60308
%stackaddr$prim60309 = alloca %struct.ScmObj*, align 8
%_95k47422 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59340)
store volatile %struct.ScmObj* %_95k47422, %struct.ScmObj** %stackaddr$prim60309, align 8
%stackaddr$prim60310 = alloca %struct.ScmObj*, align 8
%current_45args59341 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59340)
store volatile %struct.ScmObj* %current_45args59341, %struct.ScmObj** %stackaddr$prim60310, align 8
%stackaddr$prim60311 = alloca %struct.ScmObj*, align 8
%anf_45bind47299 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59341)
store volatile %struct.ScmObj* %anf_45bind47299, %struct.ScmObj** %stackaddr$prim60311, align 8
%ae49439 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim60312 = alloca %struct.ScmObj*, align 8
%_95047170 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append47169, %struct.ScmObj* %ae49439, %struct.ScmObj* %anf_45bind47299)
store volatile %struct.ScmObj* %_95047170, %struct.ScmObj** %stackaddr$prim60312, align 8
%ae49442 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim60313 = alloca %struct.ScmObj*, align 8
%_37append47168 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append47169, %struct.ScmObj* %ae49442)
store volatile %struct.ScmObj* %_37append47168, %struct.ScmObj** %stackaddr$prim60313, align 8
%stackaddr$makeclosure60314 = alloca %struct.ScmObj*, align 8
%fptrToInt60315 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49443 to i64
%ae49443 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt60315)
store volatile %struct.ScmObj* %ae49443, %struct.ScmObj** %stackaddr$makeclosure60314, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49443, %struct.ScmObj* %_37foldl147081, i64 0)
%ae49444 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure60316 = alloca %struct.ScmObj*, align 8
%fptrToInt60317 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49445 to i64
%ae49445 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt60317)
store volatile %struct.ScmObj* %ae49445, %struct.ScmObj** %stackaddr$makeclosure60316, align 8
%argslist59694$ae494430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60318 = alloca %struct.ScmObj*, align 8
%argslist59694$ae494431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49445, %struct.ScmObj* %argslist59694$ae494430)
store volatile %struct.ScmObj* %argslist59694$ae494431, %struct.ScmObj** %stackaddr$prim60318, align 8
%stackaddr$prim60319 = alloca %struct.ScmObj*, align 8
%argslist59694$ae494432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49444, %struct.ScmObj* %argslist59694$ae494431)
store volatile %struct.ScmObj* %argslist59694$ae494432, %struct.ScmObj** %stackaddr$prim60319, align 8
%clofunc60320 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49443)
musttail call tailcc void %clofunc60320(%struct.ScmObj* %ae49443, %struct.ScmObj* %argslist59694$ae494432)
ret void
}

define tailcc void @proc_clo$ae49443(%struct.ScmObj* %env$ae49443,%struct.ScmObj* %current_45args59343) {
%stackaddr$env-ref60321 = alloca %struct.ScmObj*, align 8
%_37foldl147081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49443, i64 0)
store %struct.ScmObj* %_37foldl147081, %struct.ScmObj** %stackaddr$env-ref60321
%stackaddr$prim60322 = alloca %struct.ScmObj*, align 8
%_95k47423 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59343)
store volatile %struct.ScmObj* %_95k47423, %struct.ScmObj** %stackaddr$prim60322, align 8
%stackaddr$prim60323 = alloca %struct.ScmObj*, align 8
%current_45args59344 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59343)
store volatile %struct.ScmObj* %current_45args59344, %struct.ScmObj** %stackaddr$prim60323, align 8
%stackaddr$prim60324 = alloca %struct.ScmObj*, align 8
%_37list_6347161 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59344)
store volatile %struct.ScmObj* %_37list_6347161, %struct.ScmObj** %stackaddr$prim60324, align 8
%stackaddr$makeclosure60325 = alloca %struct.ScmObj*, align 8
%fptrToInt60326 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49859 to i64
%ae49859 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt60326)
store volatile %struct.ScmObj* %ae49859, %struct.ScmObj** %stackaddr$makeclosure60325, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49859, %struct.ScmObj* %_37foldl147081, i64 0)
%ae49860 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure60327 = alloca %struct.ScmObj*, align 8
%fptrToInt60328 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49861 to i64
%ae49861 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt60328)
store volatile %struct.ScmObj* %ae49861, %struct.ScmObj** %stackaddr$makeclosure60327, align 8
%argslist59669$ae498590 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60329 = alloca %struct.ScmObj*, align 8
%argslist59669$ae498591 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49861, %struct.ScmObj* %argslist59669$ae498590)
store volatile %struct.ScmObj* %argslist59669$ae498591, %struct.ScmObj** %stackaddr$prim60329, align 8
%stackaddr$prim60330 = alloca %struct.ScmObj*, align 8
%argslist59669$ae498592 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49860, %struct.ScmObj* %argslist59669$ae498591)
store volatile %struct.ScmObj* %argslist59669$ae498592, %struct.ScmObj** %stackaddr$prim60330, align 8
%clofunc60331 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49859)
musttail call tailcc void %clofunc60331(%struct.ScmObj* %ae49859, %struct.ScmObj* %argslist59669$ae498592)
ret void
}

define tailcc void @proc_clo$ae49859(%struct.ScmObj* %env$ae49859,%struct.ScmObj* %current_45args59346) {
%stackaddr$env-ref60332 = alloca %struct.ScmObj*, align 8
%_37foldl147081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49859, i64 0)
store %struct.ScmObj* %_37foldl147081, %struct.ScmObj** %stackaddr$env-ref60332
%stackaddr$prim60333 = alloca %struct.ScmObj*, align 8
%_95k47424 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59346)
store volatile %struct.ScmObj* %_95k47424, %struct.ScmObj** %stackaddr$prim60333, align 8
%stackaddr$prim60334 = alloca %struct.ScmObj*, align 8
%current_45args59347 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59346)
store volatile %struct.ScmObj* %current_45args59347, %struct.ScmObj** %stackaddr$prim60334, align 8
%stackaddr$prim60335 = alloca %struct.ScmObj*, align 8
%_37drop47152 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59347)
store volatile %struct.ScmObj* %_37drop47152, %struct.ScmObj** %stackaddr$prim60335, align 8
%stackaddr$makeclosure60336 = alloca %struct.ScmObj*, align 8
%fptrToInt60337 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50395 to i64
%ae50395 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt60337)
store volatile %struct.ScmObj* %ae50395, %struct.ScmObj** %stackaddr$makeclosure60336, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50395, %struct.ScmObj* %_37foldl147081, i64 0)
%ae50396 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure60338 = alloca %struct.ScmObj*, align 8
%fptrToInt60339 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50397 to i64
%ae50397 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt60339)
store volatile %struct.ScmObj* %ae50397, %struct.ScmObj** %stackaddr$makeclosure60338, align 8
%argslist59645$ae503950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60340 = alloca %struct.ScmObj*, align 8
%argslist59645$ae503951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50397, %struct.ScmObj* %argslist59645$ae503950)
store volatile %struct.ScmObj* %argslist59645$ae503951, %struct.ScmObj** %stackaddr$prim60340, align 8
%stackaddr$prim60341 = alloca %struct.ScmObj*, align 8
%argslist59645$ae503952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50396, %struct.ScmObj* %argslist59645$ae503951)
store volatile %struct.ScmObj* %argslist59645$ae503952, %struct.ScmObj** %stackaddr$prim60341, align 8
%clofunc60342 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50395)
musttail call tailcc void %clofunc60342(%struct.ScmObj* %ae50395, %struct.ScmObj* %argslist59645$ae503952)
ret void
}

define tailcc void @proc_clo$ae50395(%struct.ScmObj* %env$ae50395,%struct.ScmObj* %current_45args59349) {
%stackaddr$env-ref60343 = alloca %struct.ScmObj*, align 8
%_37foldl147081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50395, i64 0)
store %struct.ScmObj* %_37foldl147081, %struct.ScmObj** %stackaddr$env-ref60343
%stackaddr$prim60344 = alloca %struct.ScmObj*, align 8
%_95k47425 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59349)
store volatile %struct.ScmObj* %_95k47425, %struct.ScmObj** %stackaddr$prim60344, align 8
%stackaddr$prim60345 = alloca %struct.ScmObj*, align 8
%current_45args59350 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59349)
store volatile %struct.ScmObj* %current_45args59350, %struct.ScmObj** %stackaddr$prim60345, align 8
%stackaddr$prim60346 = alloca %struct.ScmObj*, align 8
%_37memv47145 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59350)
store volatile %struct.ScmObj* %_37memv47145, %struct.ScmObj** %stackaddr$prim60346, align 8
%stackaddr$makeclosure60347 = alloca %struct.ScmObj*, align 8
%fptrToInt60348 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50799 to i64
%ae50799 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt60348)
store volatile %struct.ScmObj* %ae50799, %struct.ScmObj** %stackaddr$makeclosure60347, align 8
%ae50800 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure60349 = alloca %struct.ScmObj*, align 8
%fptrToInt60350 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50801 to i64
%ae50801 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt60350)
store volatile %struct.ScmObj* %ae50801, %struct.ScmObj** %stackaddr$makeclosure60349, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50801, %struct.ScmObj* %_37foldl147081, i64 0)
%argslist59619$ae507990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60351 = alloca %struct.ScmObj*, align 8
%argslist59619$ae507991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50801, %struct.ScmObj* %argslist59619$ae507990)
store volatile %struct.ScmObj* %argslist59619$ae507991, %struct.ScmObj** %stackaddr$prim60351, align 8
%stackaddr$prim60352 = alloca %struct.ScmObj*, align 8
%argslist59619$ae507992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50800, %struct.ScmObj* %argslist59619$ae507991)
store volatile %struct.ScmObj* %argslist59619$ae507992, %struct.ScmObj** %stackaddr$prim60352, align 8
%clofunc60353 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50799)
musttail call tailcc void %clofunc60353(%struct.ScmObj* %ae50799, %struct.ScmObj* %argslist59619$ae507992)
ret void
}

define tailcc void @proc_clo$ae50799(%struct.ScmObj* %env$ae50799,%struct.ScmObj* %current_45args59352) {
%stackaddr$prim60354 = alloca %struct.ScmObj*, align 8
%_95k47426 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59352)
store volatile %struct.ScmObj* %_95k47426, %struct.ScmObj** %stackaddr$prim60354, align 8
%stackaddr$prim60355 = alloca %struct.ScmObj*, align 8
%current_45args59353 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59352)
store volatile %struct.ScmObj* %current_45args59353, %struct.ScmObj** %stackaddr$prim60355, align 8
%stackaddr$prim60356 = alloca %struct.ScmObj*, align 8
%_37_4747141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59353)
store volatile %struct.ScmObj* %_37_4747141, %struct.ScmObj** %stackaddr$prim60356, align 8
%stackaddr$makeclosure60357 = alloca %struct.ScmObj*, align 8
%fptrToInt60358 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50897 to i64
%ae50897 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt60358)
store volatile %struct.ScmObj* %ae50897, %struct.ScmObj** %stackaddr$makeclosure60357, align 8
%ae50898 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure60359 = alloca %struct.ScmObj*, align 8
%fptrToInt60360 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50899 to i64
%ae50899 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt60360)
store volatile %struct.ScmObj* %ae50899, %struct.ScmObj** %stackaddr$makeclosure60359, align 8
%argslist59606$ae508970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60361 = alloca %struct.ScmObj*, align 8
%argslist59606$ae508971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50899, %struct.ScmObj* %argslist59606$ae508970)
store volatile %struct.ScmObj* %argslist59606$ae508971, %struct.ScmObj** %stackaddr$prim60361, align 8
%stackaddr$prim60362 = alloca %struct.ScmObj*, align 8
%argslist59606$ae508972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50898, %struct.ScmObj* %argslist59606$ae508971)
store volatile %struct.ScmObj* %argslist59606$ae508972, %struct.ScmObj** %stackaddr$prim60362, align 8
%clofunc60363 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50897)
musttail call tailcc void %clofunc60363(%struct.ScmObj* %ae50897, %struct.ScmObj* %argslist59606$ae508972)
ret void
}

define tailcc void @proc_clo$ae50897(%struct.ScmObj* %env$ae50897,%struct.ScmObj* %current_45args59355) {
%stackaddr$prim60364 = alloca %struct.ScmObj*, align 8
%_95k47427 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59355)
store volatile %struct.ScmObj* %_95k47427, %struct.ScmObj** %stackaddr$prim60364, align 8
%stackaddr$prim60365 = alloca %struct.ScmObj*, align 8
%current_45args59356 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59355)
store volatile %struct.ScmObj* %current_45args59356, %struct.ScmObj** %stackaddr$prim60365, align 8
%stackaddr$prim60366 = alloca %struct.ScmObj*, align 8
%_37first47139 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59356)
store volatile %struct.ScmObj* %_37first47139, %struct.ScmObj** %stackaddr$prim60366, align 8
%stackaddr$makeclosure60367 = alloca %struct.ScmObj*, align 8
%fptrToInt60368 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50917 to i64
%ae50917 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt60368)
store volatile %struct.ScmObj* %ae50917, %struct.ScmObj** %stackaddr$makeclosure60367, align 8
%ae50918 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure60369 = alloca %struct.ScmObj*, align 8
%fptrToInt60370 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50919 to i64
%ae50919 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt60370)
store volatile %struct.ScmObj* %ae50919, %struct.ScmObj** %stackaddr$makeclosure60369, align 8
%argslist59601$ae509170 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60371 = alloca %struct.ScmObj*, align 8
%argslist59601$ae509171 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50919, %struct.ScmObj* %argslist59601$ae509170)
store volatile %struct.ScmObj* %argslist59601$ae509171, %struct.ScmObj** %stackaddr$prim60371, align 8
%stackaddr$prim60372 = alloca %struct.ScmObj*, align 8
%argslist59601$ae509172 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50918, %struct.ScmObj* %argslist59601$ae509171)
store volatile %struct.ScmObj* %argslist59601$ae509172, %struct.ScmObj** %stackaddr$prim60372, align 8
%clofunc60373 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50917)
musttail call tailcc void %clofunc60373(%struct.ScmObj* %ae50917, %struct.ScmObj* %argslist59601$ae509172)
ret void
}

define tailcc void @proc_clo$ae50917(%struct.ScmObj* %env$ae50917,%struct.ScmObj* %current_45args59358) {
%stackaddr$prim60374 = alloca %struct.ScmObj*, align 8
%_95k47428 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59358)
store volatile %struct.ScmObj* %_95k47428, %struct.ScmObj** %stackaddr$prim60374, align 8
%stackaddr$prim60375 = alloca %struct.ScmObj*, align 8
%current_45args59359 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59358)
store volatile %struct.ScmObj* %current_45args59359, %struct.ScmObj** %stackaddr$prim60375, align 8
%stackaddr$prim60376 = alloca %struct.ScmObj*, align 8
%_37second47137 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59359)
store volatile %struct.ScmObj* %_37second47137, %struct.ScmObj** %stackaddr$prim60376, align 8
%stackaddr$makeclosure60377 = alloca %struct.ScmObj*, align 8
%fptrToInt60378 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50939 to i64
%ae50939 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt60378)
store volatile %struct.ScmObj* %ae50939, %struct.ScmObj** %stackaddr$makeclosure60377, align 8
%ae50940 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure60379 = alloca %struct.ScmObj*, align 8
%fptrToInt60380 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50941 to i64
%ae50941 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt60380)
store volatile %struct.ScmObj* %ae50941, %struct.ScmObj** %stackaddr$makeclosure60379, align 8
%argslist59596$ae509390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60381 = alloca %struct.ScmObj*, align 8
%argslist59596$ae509391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50941, %struct.ScmObj* %argslist59596$ae509390)
store volatile %struct.ScmObj* %argslist59596$ae509391, %struct.ScmObj** %stackaddr$prim60381, align 8
%stackaddr$prim60382 = alloca %struct.ScmObj*, align 8
%argslist59596$ae509392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50940, %struct.ScmObj* %argslist59596$ae509391)
store volatile %struct.ScmObj* %argslist59596$ae509392, %struct.ScmObj** %stackaddr$prim60382, align 8
%clofunc60383 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50939)
musttail call tailcc void %clofunc60383(%struct.ScmObj* %ae50939, %struct.ScmObj* %argslist59596$ae509392)
ret void
}

define tailcc void @proc_clo$ae50939(%struct.ScmObj* %env$ae50939,%struct.ScmObj* %current_45args59361) {
%stackaddr$prim60384 = alloca %struct.ScmObj*, align 8
%_95k47429 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59361)
store volatile %struct.ScmObj* %_95k47429, %struct.ScmObj** %stackaddr$prim60384, align 8
%stackaddr$prim60385 = alloca %struct.ScmObj*, align 8
%current_45args59362 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59361)
store volatile %struct.ScmObj* %current_45args59362, %struct.ScmObj** %stackaddr$prim60385, align 8
%stackaddr$prim60386 = alloca %struct.ScmObj*, align 8
%_37third47135 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59362)
store volatile %struct.ScmObj* %_37third47135, %struct.ScmObj** %stackaddr$prim60386, align 8
%stackaddr$makeclosure60387 = alloca %struct.ScmObj*, align 8
%fptrToInt60388 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50963 to i64
%ae50963 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt60388)
store volatile %struct.ScmObj* %ae50963, %struct.ScmObj** %stackaddr$makeclosure60387, align 8
%ae50964 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure60389 = alloca %struct.ScmObj*, align 8
%fptrToInt60390 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50965 to i64
%ae50965 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt60390)
store volatile %struct.ScmObj* %ae50965, %struct.ScmObj** %stackaddr$makeclosure60389, align 8
%argslist59591$ae509630 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60391 = alloca %struct.ScmObj*, align 8
%argslist59591$ae509631 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50965, %struct.ScmObj* %argslist59591$ae509630)
store volatile %struct.ScmObj* %argslist59591$ae509631, %struct.ScmObj** %stackaddr$prim60391, align 8
%stackaddr$prim60392 = alloca %struct.ScmObj*, align 8
%argslist59591$ae509632 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50964, %struct.ScmObj* %argslist59591$ae509631)
store volatile %struct.ScmObj* %argslist59591$ae509632, %struct.ScmObj** %stackaddr$prim60392, align 8
%clofunc60393 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50963)
musttail call tailcc void %clofunc60393(%struct.ScmObj* %ae50963, %struct.ScmObj* %argslist59591$ae509632)
ret void
}

define tailcc void @proc_clo$ae50963(%struct.ScmObj* %env$ae50963,%struct.ScmObj* %current_45args59364) {
%stackaddr$prim60394 = alloca %struct.ScmObj*, align 8
%_95k47430 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59364)
store volatile %struct.ScmObj* %_95k47430, %struct.ScmObj** %stackaddr$prim60394, align 8
%stackaddr$prim60395 = alloca %struct.ScmObj*, align 8
%current_45args59365 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59364)
store volatile %struct.ScmObj* %current_45args59365, %struct.ScmObj** %stackaddr$prim60395, align 8
%stackaddr$prim60396 = alloca %struct.ScmObj*, align 8
%_37fourth47133 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59365)
store volatile %struct.ScmObj* %_37fourth47133, %struct.ScmObj** %stackaddr$prim60396, align 8
%stackaddr$prim60397 = alloca %struct.ScmObj*, align 8
%anf_45bind47335 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind47335, %struct.ScmObj** %stackaddr$prim60397, align 8
%ae50989 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim60398 = alloca %struct.ScmObj*, align 8
%nat_45_62peano47200 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50989, %struct.ScmObj* %anf_45bind47335)
store volatile %struct.ScmObj* %nat_45_62peano47200, %struct.ScmObj** %stackaddr$prim60398, align 8
%stackaddr$prim60399 = alloca %struct.ScmObj*, align 8
%anf_45bind47336 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind47336, %struct.ScmObj** %stackaddr$prim60399, align 8
%ae50991 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim60400 = alloca %struct.ScmObj*, align 8
%peano_45_62nat47199 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50991, %struct.ScmObj* %anf_45bind47336)
store volatile %struct.ScmObj* %peano_45_62nat47199, %struct.ScmObj** %stackaddr$prim60400, align 8
%stackaddr$prim60401 = alloca %struct.ScmObj*, align 8
%anf_45bind47337 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind47337, %struct.ScmObj** %stackaddr$prim60401, align 8
%ae50993 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim60402 = alloca %struct.ScmObj*, align 8
%succ47198 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50993, %struct.ScmObj* %anf_45bind47337)
store volatile %struct.ScmObj* %succ47198, %struct.ScmObj** %stackaddr$prim60402, align 8
%stackaddr$prim60403 = alloca %struct.ScmObj*, align 8
%anf_45bind47338 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind47338, %struct.ScmObj** %stackaddr$prim60403, align 8
%ae50995 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim60404 = alloca %struct.ScmObj*, align 8
%pred47197 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50995, %struct.ScmObj* %anf_45bind47338)
store volatile %struct.ScmObj* %pred47197, %struct.ScmObj** %stackaddr$prim60404, align 8
%stackaddr$prim60405 = alloca %struct.ScmObj*, align 8
%anf_45bind47339 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind47339, %struct.ScmObj** %stackaddr$prim60405, align 8
%ae50997 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim60406 = alloca %struct.ScmObj*, align 8
%z_6347196 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50997, %struct.ScmObj* %anf_45bind47339)
store volatile %struct.ScmObj* %z_6347196, %struct.ScmObj** %stackaddr$prim60406, align 8
%stackaddr$prim60407 = alloca %struct.ScmObj*, align 8
%anf_45bind47340 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind47340, %struct.ScmObj** %stackaddr$prim60407, align 8
%ae50999 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim60408 = alloca %struct.ScmObj*, align 8
%addc47195 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50999, %struct.ScmObj* %anf_45bind47340)
store volatile %struct.ScmObj* %addc47195, %struct.ScmObj** %stackaddr$prim60408, align 8
%stackaddr$prim60409 = alloca %struct.ScmObj*, align 8
%anf_45bind47341 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind47341, %struct.ScmObj** %stackaddr$prim60409, align 8
%ae51001 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim60410 = alloca %struct.ScmObj*, align 8
%fibc47194 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51001, %struct.ScmObj* %anf_45bind47341)
store volatile %struct.ScmObj* %fibc47194, %struct.ScmObj** %stackaddr$prim60410, align 8
%stackaddr$makeclosure60411 = alloca %struct.ScmObj*, align 8
%fptrToInt60412 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51003 to i64
%ae51003 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt60412)
store volatile %struct.ScmObj* %ae51003, %struct.ScmObj** %stackaddr$makeclosure60411, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51003, %struct.ScmObj* %nat_45_62peano47200, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51003, %struct.ScmObj* %peano_45_62nat47199, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51003, %struct.ScmObj* %succ47198, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51003, %struct.ScmObj* %pred47197, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51003, %struct.ScmObj* %z_6347196, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51003, %struct.ScmObj* %addc47195, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae51003, %struct.ScmObj* %fibc47194, i64 6)
%ae51004 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure60413 = alloca %struct.ScmObj*, align 8
%fptrToInt60414 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51005 to i64
%ae51005 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt60414)
store volatile %struct.ScmObj* %ae51005, %struct.ScmObj** %stackaddr$makeclosure60413, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51005, %struct.ScmObj* %nat_45_62peano47200, i64 0)
%argslist59586$ae510030 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60415 = alloca %struct.ScmObj*, align 8
%argslist59586$ae510031 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51005, %struct.ScmObj* %argslist59586$ae510030)
store volatile %struct.ScmObj* %argslist59586$ae510031, %struct.ScmObj** %stackaddr$prim60415, align 8
%stackaddr$prim60416 = alloca %struct.ScmObj*, align 8
%argslist59586$ae510032 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51004, %struct.ScmObj* %argslist59586$ae510031)
store volatile %struct.ScmObj* %argslist59586$ae510032, %struct.ScmObj** %stackaddr$prim60416, align 8
%clofunc60417 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51003)
musttail call tailcc void %clofunc60417(%struct.ScmObj* %ae51003, %struct.ScmObj* %argslist59586$ae510032)
ret void
}

define tailcc void @proc_clo$ae51003(%struct.ScmObj* %env$ae51003,%struct.ScmObj* %current_45args59367) {
%stackaddr$env-ref60418 = alloca %struct.ScmObj*, align 8
%nat_45_62peano47200 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51003, i64 0)
store %struct.ScmObj* %nat_45_62peano47200, %struct.ScmObj** %stackaddr$env-ref60418
%stackaddr$env-ref60419 = alloca %struct.ScmObj*, align 8
%peano_45_62nat47199 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51003, i64 1)
store %struct.ScmObj* %peano_45_62nat47199, %struct.ScmObj** %stackaddr$env-ref60419
%stackaddr$env-ref60420 = alloca %struct.ScmObj*, align 8
%succ47198 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51003, i64 2)
store %struct.ScmObj* %succ47198, %struct.ScmObj** %stackaddr$env-ref60420
%stackaddr$env-ref60421 = alloca %struct.ScmObj*, align 8
%pred47197 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51003, i64 3)
store %struct.ScmObj* %pred47197, %struct.ScmObj** %stackaddr$env-ref60421
%stackaddr$env-ref60422 = alloca %struct.ScmObj*, align 8
%z_6347196 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51003, i64 4)
store %struct.ScmObj* %z_6347196, %struct.ScmObj** %stackaddr$env-ref60422
%stackaddr$env-ref60423 = alloca %struct.ScmObj*, align 8
%addc47195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51003, i64 5)
store %struct.ScmObj* %addc47195, %struct.ScmObj** %stackaddr$env-ref60423
%stackaddr$env-ref60424 = alloca %struct.ScmObj*, align 8
%fibc47194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51003, i64 6)
store %struct.ScmObj* %fibc47194, %struct.ScmObj** %stackaddr$env-ref60424
%stackaddr$prim60425 = alloca %struct.ScmObj*, align 8
%_95k47431 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59367)
store volatile %struct.ScmObj* %_95k47431, %struct.ScmObj** %stackaddr$prim60425, align 8
%stackaddr$prim60426 = alloca %struct.ScmObj*, align 8
%current_45args59368 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59367)
store volatile %struct.ScmObj* %current_45args59368, %struct.ScmObj** %stackaddr$prim60426, align 8
%stackaddr$prim60427 = alloca %struct.ScmObj*, align 8
%anf_45bind47349 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59368)
store volatile %struct.ScmObj* %anf_45bind47349, %struct.ScmObj** %stackaddr$prim60427, align 8
%ae51212 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim60428 = alloca %struct.ScmObj*, align 8
%t4707547219 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %nat_45_62peano47200, %struct.ScmObj* %ae51212, %struct.ScmObj* %anf_45bind47349)
store volatile %struct.ScmObj* %t4707547219, %struct.ScmObj** %stackaddr$prim60428, align 8
%stackaddr$makeclosure60429 = alloca %struct.ScmObj*, align 8
%fptrToInt60430 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51214 to i64
%ae51214 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt60430)
store volatile %struct.ScmObj* %ae51214, %struct.ScmObj** %stackaddr$makeclosure60429, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51214, %struct.ScmObj* %nat_45_62peano47200, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51214, %struct.ScmObj* %peano_45_62nat47199, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51214, %struct.ScmObj* %succ47198, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51214, %struct.ScmObj* %pred47197, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51214, %struct.ScmObj* %z_6347196, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51214, %struct.ScmObj* %addc47195, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae51214, %struct.ScmObj* %fibc47194, i64 6)
%ae51215 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure60431 = alloca %struct.ScmObj*, align 8
%fptrToInt60432 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51216 to i64
%ae51216 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt60432)
store volatile %struct.ScmObj* %ae51216, %struct.ScmObj** %stackaddr$makeclosure60431, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51216, %struct.ScmObj* %peano_45_62nat47199, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51216, %struct.ScmObj* %pred47197, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51216, %struct.ScmObj* %z_6347196, i64 2)
%argslist59562$ae512140 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60433 = alloca %struct.ScmObj*, align 8
%argslist59562$ae512141 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51216, %struct.ScmObj* %argslist59562$ae512140)
store volatile %struct.ScmObj* %argslist59562$ae512141, %struct.ScmObj** %stackaddr$prim60433, align 8
%stackaddr$prim60434 = alloca %struct.ScmObj*, align 8
%argslist59562$ae512142 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51215, %struct.ScmObj* %argslist59562$ae512141)
store volatile %struct.ScmObj* %argslist59562$ae512142, %struct.ScmObj** %stackaddr$prim60434, align 8
%clofunc60435 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51214)
musttail call tailcc void %clofunc60435(%struct.ScmObj* %ae51214, %struct.ScmObj* %argslist59562$ae512142)
ret void
}

define tailcc void @proc_clo$ae51214(%struct.ScmObj* %env$ae51214,%struct.ScmObj* %current_45args59370) {
%stackaddr$env-ref60436 = alloca %struct.ScmObj*, align 8
%nat_45_62peano47200 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51214, i64 0)
store %struct.ScmObj* %nat_45_62peano47200, %struct.ScmObj** %stackaddr$env-ref60436
%stackaddr$env-ref60437 = alloca %struct.ScmObj*, align 8
%peano_45_62nat47199 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51214, i64 1)
store %struct.ScmObj* %peano_45_62nat47199, %struct.ScmObj** %stackaddr$env-ref60437
%stackaddr$env-ref60438 = alloca %struct.ScmObj*, align 8
%succ47198 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51214, i64 2)
store %struct.ScmObj* %succ47198, %struct.ScmObj** %stackaddr$env-ref60438
%stackaddr$env-ref60439 = alloca %struct.ScmObj*, align 8
%pred47197 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51214, i64 3)
store %struct.ScmObj* %pred47197, %struct.ScmObj** %stackaddr$env-ref60439
%stackaddr$env-ref60440 = alloca %struct.ScmObj*, align 8
%z_6347196 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51214, i64 4)
store %struct.ScmObj* %z_6347196, %struct.ScmObj** %stackaddr$env-ref60440
%stackaddr$env-ref60441 = alloca %struct.ScmObj*, align 8
%addc47195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51214, i64 5)
store %struct.ScmObj* %addc47195, %struct.ScmObj** %stackaddr$env-ref60441
%stackaddr$env-ref60442 = alloca %struct.ScmObj*, align 8
%fibc47194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51214, i64 6)
store %struct.ScmObj* %fibc47194, %struct.ScmObj** %stackaddr$env-ref60442
%stackaddr$prim60443 = alloca %struct.ScmObj*, align 8
%_95k47432 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59370)
store volatile %struct.ScmObj* %_95k47432, %struct.ScmObj** %stackaddr$prim60443, align 8
%stackaddr$prim60444 = alloca %struct.ScmObj*, align 8
%current_45args59371 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59370)
store volatile %struct.ScmObj* %current_45args59371, %struct.ScmObj** %stackaddr$prim60444, align 8
%stackaddr$prim60445 = alloca %struct.ScmObj*, align 8
%anf_45bind47356 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59371)
store volatile %struct.ScmObj* %anf_45bind47356, %struct.ScmObj** %stackaddr$prim60445, align 8
%ae51347 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim60446 = alloca %struct.ScmObj*, align 8
%t4707447217 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %peano_45_62nat47199, %struct.ScmObj* %ae51347, %struct.ScmObj* %anf_45bind47356)
store volatile %struct.ScmObj* %t4707447217, %struct.ScmObj** %stackaddr$prim60446, align 8
%stackaddr$makeclosure60447 = alloca %struct.ScmObj*, align 8
%fptrToInt60448 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51349 to i64
%ae51349 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt60448)
store volatile %struct.ScmObj* %ae51349, %struct.ScmObj** %stackaddr$makeclosure60447, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51349, %struct.ScmObj* %nat_45_62peano47200, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51349, %struct.ScmObj* %peano_45_62nat47199, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51349, %struct.ScmObj* %succ47198, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51349, %struct.ScmObj* %pred47197, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51349, %struct.ScmObj* %z_6347196, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51349, %struct.ScmObj* %addc47195, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae51349, %struct.ScmObj* %fibc47194, i64 6)
%ae51350 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure60449 = alloca %struct.ScmObj*, align 8
%fptrToInt60450 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51351 to i64
%ae51351 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt60450)
store volatile %struct.ScmObj* %ae51351, %struct.ScmObj** %stackaddr$makeclosure60449, align 8
%argslist59544$ae513490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60451 = alloca %struct.ScmObj*, align 8
%argslist59544$ae513491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51351, %struct.ScmObj* %argslist59544$ae513490)
store volatile %struct.ScmObj* %argslist59544$ae513491, %struct.ScmObj** %stackaddr$prim60451, align 8
%stackaddr$prim60452 = alloca %struct.ScmObj*, align 8
%argslist59544$ae513492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51350, %struct.ScmObj* %argslist59544$ae513491)
store volatile %struct.ScmObj* %argslist59544$ae513492, %struct.ScmObj** %stackaddr$prim60452, align 8
%clofunc60453 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51349)
musttail call tailcc void %clofunc60453(%struct.ScmObj* %ae51349, %struct.ScmObj* %argslist59544$ae513492)
ret void
}

define tailcc void @proc_clo$ae51349(%struct.ScmObj* %env$ae51349,%struct.ScmObj* %current_45args59373) {
%stackaddr$env-ref60454 = alloca %struct.ScmObj*, align 8
%nat_45_62peano47200 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51349, i64 0)
store %struct.ScmObj* %nat_45_62peano47200, %struct.ScmObj** %stackaddr$env-ref60454
%stackaddr$env-ref60455 = alloca %struct.ScmObj*, align 8
%peano_45_62nat47199 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51349, i64 1)
store %struct.ScmObj* %peano_45_62nat47199, %struct.ScmObj** %stackaddr$env-ref60455
%stackaddr$env-ref60456 = alloca %struct.ScmObj*, align 8
%succ47198 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51349, i64 2)
store %struct.ScmObj* %succ47198, %struct.ScmObj** %stackaddr$env-ref60456
%stackaddr$env-ref60457 = alloca %struct.ScmObj*, align 8
%pred47197 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51349, i64 3)
store %struct.ScmObj* %pred47197, %struct.ScmObj** %stackaddr$env-ref60457
%stackaddr$env-ref60458 = alloca %struct.ScmObj*, align 8
%z_6347196 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51349, i64 4)
store %struct.ScmObj* %z_6347196, %struct.ScmObj** %stackaddr$env-ref60458
%stackaddr$env-ref60459 = alloca %struct.ScmObj*, align 8
%addc47195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51349, i64 5)
store %struct.ScmObj* %addc47195, %struct.ScmObj** %stackaddr$env-ref60459
%stackaddr$env-ref60460 = alloca %struct.ScmObj*, align 8
%fibc47194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51349, i64 6)
store %struct.ScmObj* %fibc47194, %struct.ScmObj** %stackaddr$env-ref60460
%stackaddr$prim60461 = alloca %struct.ScmObj*, align 8
%_95k47433 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59373)
store volatile %struct.ScmObj* %_95k47433, %struct.ScmObj** %stackaddr$prim60461, align 8
%stackaddr$prim60462 = alloca %struct.ScmObj*, align 8
%current_45args59374 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59373)
store volatile %struct.ScmObj* %current_45args59374, %struct.ScmObj** %stackaddr$prim60462, align 8
%stackaddr$prim60463 = alloca %struct.ScmObj*, align 8
%anf_45bind47359 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59374)
store volatile %struct.ScmObj* %anf_45bind47359, %struct.ScmObj** %stackaddr$prim60463, align 8
%ae51416 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim60464 = alloca %struct.ScmObj*, align 8
%t4707347214 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %succ47198, %struct.ScmObj* %ae51416, %struct.ScmObj* %anf_45bind47359)
store volatile %struct.ScmObj* %t4707347214, %struct.ScmObj** %stackaddr$prim60464, align 8
%stackaddr$makeclosure60465 = alloca %struct.ScmObj*, align 8
%fptrToInt60466 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51418 to i64
%ae51418 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt60466)
store volatile %struct.ScmObj* %ae51418, %struct.ScmObj** %stackaddr$makeclosure60465, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51418, %struct.ScmObj* %nat_45_62peano47200, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51418, %struct.ScmObj* %peano_45_62nat47199, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51418, %struct.ScmObj* %succ47198, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51418, %struct.ScmObj* %pred47197, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51418, %struct.ScmObj* %z_6347196, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51418, %struct.ScmObj* %addc47195, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae51418, %struct.ScmObj* %fibc47194, i64 6)
%ae51419 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure60467 = alloca %struct.ScmObj*, align 8
%fptrToInt60468 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51420 to i64
%ae51420 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt60468)
store volatile %struct.ScmObj* %ae51420, %struct.ScmObj** %stackaddr$makeclosure60467, align 8
%argslist59530$ae514180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60469 = alloca %struct.ScmObj*, align 8
%argslist59530$ae514181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51420, %struct.ScmObj* %argslist59530$ae514180)
store volatile %struct.ScmObj* %argslist59530$ae514181, %struct.ScmObj** %stackaddr$prim60469, align 8
%stackaddr$prim60470 = alloca %struct.ScmObj*, align 8
%argslist59530$ae514182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51419, %struct.ScmObj* %argslist59530$ae514181)
store volatile %struct.ScmObj* %argslist59530$ae514182, %struct.ScmObj** %stackaddr$prim60470, align 8
%clofunc60471 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51418)
musttail call tailcc void %clofunc60471(%struct.ScmObj* %ae51418, %struct.ScmObj* %argslist59530$ae514182)
ret void
}

define tailcc void @proc_clo$ae51418(%struct.ScmObj* %env$ae51418,%struct.ScmObj* %current_45args59376) {
%stackaddr$env-ref60472 = alloca %struct.ScmObj*, align 8
%nat_45_62peano47200 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51418, i64 0)
store %struct.ScmObj* %nat_45_62peano47200, %struct.ScmObj** %stackaddr$env-ref60472
%stackaddr$env-ref60473 = alloca %struct.ScmObj*, align 8
%peano_45_62nat47199 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51418, i64 1)
store %struct.ScmObj* %peano_45_62nat47199, %struct.ScmObj** %stackaddr$env-ref60473
%stackaddr$env-ref60474 = alloca %struct.ScmObj*, align 8
%succ47198 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51418, i64 2)
store %struct.ScmObj* %succ47198, %struct.ScmObj** %stackaddr$env-ref60474
%stackaddr$env-ref60475 = alloca %struct.ScmObj*, align 8
%pred47197 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51418, i64 3)
store %struct.ScmObj* %pred47197, %struct.ScmObj** %stackaddr$env-ref60475
%stackaddr$env-ref60476 = alloca %struct.ScmObj*, align 8
%z_6347196 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51418, i64 4)
store %struct.ScmObj* %z_6347196, %struct.ScmObj** %stackaddr$env-ref60476
%stackaddr$env-ref60477 = alloca %struct.ScmObj*, align 8
%addc47195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51418, i64 5)
store %struct.ScmObj* %addc47195, %struct.ScmObj** %stackaddr$env-ref60477
%stackaddr$env-ref60478 = alloca %struct.ScmObj*, align 8
%fibc47194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51418, i64 6)
store %struct.ScmObj* %fibc47194, %struct.ScmObj** %stackaddr$env-ref60478
%stackaddr$prim60479 = alloca %struct.ScmObj*, align 8
%_95k47434 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59376)
store volatile %struct.ScmObj* %_95k47434, %struct.ScmObj** %stackaddr$prim60479, align 8
%stackaddr$prim60480 = alloca %struct.ScmObj*, align 8
%current_45args59377 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59376)
store volatile %struct.ScmObj* %current_45args59377, %struct.ScmObj** %stackaddr$prim60480, align 8
%stackaddr$prim60481 = alloca %struct.ScmObj*, align 8
%anf_45bind47360 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59377)
store volatile %struct.ScmObj* %anf_45bind47360, %struct.ScmObj** %stackaddr$prim60481, align 8
%ae51439 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim60482 = alloca %struct.ScmObj*, align 8
%t4707147212 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %pred47197, %struct.ScmObj* %ae51439, %struct.ScmObj* %anf_45bind47360)
store volatile %struct.ScmObj* %t4707147212, %struct.ScmObj** %stackaddr$prim60482, align 8
%stackaddr$makeclosure60483 = alloca %struct.ScmObj*, align 8
%fptrToInt60484 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51441 to i64
%ae51441 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt60484)
store volatile %struct.ScmObj* %ae51441, %struct.ScmObj** %stackaddr$makeclosure60483, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51441, %struct.ScmObj* %nat_45_62peano47200, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51441, %struct.ScmObj* %peano_45_62nat47199, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51441, %struct.ScmObj* %succ47198, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51441, %struct.ScmObj* %pred47197, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51441, %struct.ScmObj* %z_6347196, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51441, %struct.ScmObj* %addc47195, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae51441, %struct.ScmObj* %fibc47194, i64 6)
%ae51442 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure60485 = alloca %struct.ScmObj*, align 8
%fptrToInt60486 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51443 to i64
%ae51443 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt60486)
store volatile %struct.ScmObj* %ae51443, %struct.ScmObj** %stackaddr$makeclosure60485, align 8
%argslist59525$ae514410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60487 = alloca %struct.ScmObj*, align 8
%argslist59525$ae514411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51443, %struct.ScmObj* %argslist59525$ae514410)
store volatile %struct.ScmObj* %argslist59525$ae514411, %struct.ScmObj** %stackaddr$prim60487, align 8
%stackaddr$prim60488 = alloca %struct.ScmObj*, align 8
%argslist59525$ae514412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51442, %struct.ScmObj* %argslist59525$ae514411)
store volatile %struct.ScmObj* %argslist59525$ae514412, %struct.ScmObj** %stackaddr$prim60488, align 8
%clofunc60489 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51441)
musttail call tailcc void %clofunc60489(%struct.ScmObj* %ae51441, %struct.ScmObj* %argslist59525$ae514412)
ret void
}

define tailcc void @proc_clo$ae51441(%struct.ScmObj* %env$ae51441,%struct.ScmObj* %current_45args59379) {
%stackaddr$env-ref60490 = alloca %struct.ScmObj*, align 8
%nat_45_62peano47200 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51441, i64 0)
store %struct.ScmObj* %nat_45_62peano47200, %struct.ScmObj** %stackaddr$env-ref60490
%stackaddr$env-ref60491 = alloca %struct.ScmObj*, align 8
%peano_45_62nat47199 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51441, i64 1)
store %struct.ScmObj* %peano_45_62nat47199, %struct.ScmObj** %stackaddr$env-ref60491
%stackaddr$env-ref60492 = alloca %struct.ScmObj*, align 8
%succ47198 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51441, i64 2)
store %struct.ScmObj* %succ47198, %struct.ScmObj** %stackaddr$env-ref60492
%stackaddr$env-ref60493 = alloca %struct.ScmObj*, align 8
%pred47197 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51441, i64 3)
store %struct.ScmObj* %pred47197, %struct.ScmObj** %stackaddr$env-ref60493
%stackaddr$env-ref60494 = alloca %struct.ScmObj*, align 8
%z_6347196 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51441, i64 4)
store %struct.ScmObj* %z_6347196, %struct.ScmObj** %stackaddr$env-ref60494
%stackaddr$env-ref60495 = alloca %struct.ScmObj*, align 8
%addc47195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51441, i64 5)
store %struct.ScmObj* %addc47195, %struct.ScmObj** %stackaddr$env-ref60495
%stackaddr$env-ref60496 = alloca %struct.ScmObj*, align 8
%fibc47194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51441, i64 6)
store %struct.ScmObj* %fibc47194, %struct.ScmObj** %stackaddr$env-ref60496
%stackaddr$prim60497 = alloca %struct.ScmObj*, align 8
%_95k47435 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59379)
store volatile %struct.ScmObj* %_95k47435, %struct.ScmObj** %stackaddr$prim60497, align 8
%stackaddr$prim60498 = alloca %struct.ScmObj*, align 8
%current_45args59380 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59379)
store volatile %struct.ScmObj* %current_45args59380, %struct.ScmObj** %stackaddr$prim60498, align 8
%stackaddr$prim60499 = alloca %struct.ScmObj*, align 8
%anf_45bind47361 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59380)
store volatile %struct.ScmObj* %anf_45bind47361, %struct.ScmObj** %stackaddr$prim60499, align 8
%ae51462 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim60500 = alloca %struct.ScmObj*, align 8
%t4707047210 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %z_6347196, %struct.ScmObj* %ae51462, %struct.ScmObj* %anf_45bind47361)
store volatile %struct.ScmObj* %t4707047210, %struct.ScmObj** %stackaddr$prim60500, align 8
%stackaddr$makeclosure60501 = alloca %struct.ScmObj*, align 8
%fptrToInt60502 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51464 to i64
%ae51464 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt60502)
store volatile %struct.ScmObj* %ae51464, %struct.ScmObj** %stackaddr$makeclosure60501, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51464, %struct.ScmObj* %nat_45_62peano47200, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51464, %struct.ScmObj* %peano_45_62nat47199, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51464, %struct.ScmObj* %pred47197, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51464, %struct.ScmObj* %z_6347196, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51464, %struct.ScmObj* %addc47195, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51464, %struct.ScmObj* %fibc47194, i64 5)
%ae51465 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure60503 = alloca %struct.ScmObj*, align 8
%fptrToInt60504 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51466 to i64
%ae51466 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt60504)
store volatile %struct.ScmObj* %ae51466, %struct.ScmObj** %stackaddr$makeclosure60503, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51466, %struct.ScmObj* %succ47198, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51466, %struct.ScmObj* %pred47197, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51466, %struct.ScmObj* %z_6347196, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51466, %struct.ScmObj* %addc47195, i64 3)
%argslist59520$ae514640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60505 = alloca %struct.ScmObj*, align 8
%argslist59520$ae514641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51466, %struct.ScmObj* %argslist59520$ae514640)
store volatile %struct.ScmObj* %argslist59520$ae514641, %struct.ScmObj** %stackaddr$prim60505, align 8
%stackaddr$prim60506 = alloca %struct.ScmObj*, align 8
%argslist59520$ae514642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51465, %struct.ScmObj* %argslist59520$ae514641)
store volatile %struct.ScmObj* %argslist59520$ae514642, %struct.ScmObj** %stackaddr$prim60506, align 8
%clofunc60507 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51464)
musttail call tailcc void %clofunc60507(%struct.ScmObj* %ae51464, %struct.ScmObj* %argslist59520$ae514642)
ret void
}

define tailcc void @proc_clo$ae51464(%struct.ScmObj* %env$ae51464,%struct.ScmObj* %current_45args59382) {
%stackaddr$env-ref60508 = alloca %struct.ScmObj*, align 8
%nat_45_62peano47200 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51464, i64 0)
store %struct.ScmObj* %nat_45_62peano47200, %struct.ScmObj** %stackaddr$env-ref60508
%stackaddr$env-ref60509 = alloca %struct.ScmObj*, align 8
%peano_45_62nat47199 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51464, i64 1)
store %struct.ScmObj* %peano_45_62nat47199, %struct.ScmObj** %stackaddr$env-ref60509
%stackaddr$env-ref60510 = alloca %struct.ScmObj*, align 8
%pred47197 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51464, i64 2)
store %struct.ScmObj* %pred47197, %struct.ScmObj** %stackaddr$env-ref60510
%stackaddr$env-ref60511 = alloca %struct.ScmObj*, align 8
%z_6347196 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51464, i64 3)
store %struct.ScmObj* %z_6347196, %struct.ScmObj** %stackaddr$env-ref60511
%stackaddr$env-ref60512 = alloca %struct.ScmObj*, align 8
%addc47195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51464, i64 4)
store %struct.ScmObj* %addc47195, %struct.ScmObj** %stackaddr$env-ref60512
%stackaddr$env-ref60513 = alloca %struct.ScmObj*, align 8
%fibc47194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51464, i64 5)
store %struct.ScmObj* %fibc47194, %struct.ScmObj** %stackaddr$env-ref60513
%stackaddr$prim60514 = alloca %struct.ScmObj*, align 8
%_95k47436 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59382)
store volatile %struct.ScmObj* %_95k47436, %struct.ScmObj** %stackaddr$prim60514, align 8
%stackaddr$prim60515 = alloca %struct.ScmObj*, align 8
%current_45args59383 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59382)
store volatile %struct.ScmObj* %current_45args59383, %struct.ScmObj** %stackaddr$prim60515, align 8
%stackaddr$prim60516 = alloca %struct.ScmObj*, align 8
%anf_45bind47369 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59383)
store volatile %struct.ScmObj* %anf_45bind47369, %struct.ScmObj** %stackaddr$prim60516, align 8
%ae51603 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim60517 = alloca %struct.ScmObj*, align 8
%t4706947206 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %addc47195, %struct.ScmObj* %ae51603, %struct.ScmObj* %anf_45bind47369)
store volatile %struct.ScmObj* %t4706947206, %struct.ScmObj** %stackaddr$prim60517, align 8
%stackaddr$makeclosure60518 = alloca %struct.ScmObj*, align 8
%fptrToInt60519 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51605 to i64
%ae51605 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt60519)
store volatile %struct.ScmObj* %ae51605, %struct.ScmObj** %stackaddr$makeclosure60518, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51605, %struct.ScmObj* %nat_45_62peano47200, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51605, %struct.ScmObj* %peano_45_62nat47199, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51605, %struct.ScmObj* %fibc47194, i64 2)
%ae51606 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure60520 = alloca %struct.ScmObj*, align 8
%fptrToInt60521 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51607 to i64
%ae51607 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt60521)
store volatile %struct.ScmObj* %ae51607, %struct.ScmObj** %stackaddr$makeclosure60520, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51607, %struct.ScmObj* %nat_45_62peano47200, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51607, %struct.ScmObj* %pred47197, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51607, %struct.ScmObj* %z_6347196, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51607, %struct.ScmObj* %addc47195, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51607, %struct.ScmObj* %fibc47194, i64 4)
%argslist59500$ae516050 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60522 = alloca %struct.ScmObj*, align 8
%argslist59500$ae516051 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51607, %struct.ScmObj* %argslist59500$ae516050)
store volatile %struct.ScmObj* %argslist59500$ae516051, %struct.ScmObj** %stackaddr$prim60522, align 8
%stackaddr$prim60523 = alloca %struct.ScmObj*, align 8
%argslist59500$ae516052 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51606, %struct.ScmObj* %argslist59500$ae516051)
store volatile %struct.ScmObj* %argslist59500$ae516052, %struct.ScmObj** %stackaddr$prim60523, align 8
%clofunc60524 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51605)
musttail call tailcc void %clofunc60524(%struct.ScmObj* %ae51605, %struct.ScmObj* %argslist59500$ae516052)
ret void
}

define tailcc void @proc_clo$ae51605(%struct.ScmObj* %env$ae51605,%struct.ScmObj* %current_45args59385) {
%stackaddr$env-ref60525 = alloca %struct.ScmObj*, align 8
%nat_45_62peano47200 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51605, i64 0)
store %struct.ScmObj* %nat_45_62peano47200, %struct.ScmObj** %stackaddr$env-ref60525
%stackaddr$env-ref60526 = alloca %struct.ScmObj*, align 8
%peano_45_62nat47199 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51605, i64 1)
store %struct.ScmObj* %peano_45_62nat47199, %struct.ScmObj** %stackaddr$env-ref60526
%stackaddr$env-ref60527 = alloca %struct.ScmObj*, align 8
%fibc47194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51605, i64 2)
store %struct.ScmObj* %fibc47194, %struct.ScmObj** %stackaddr$env-ref60527
%stackaddr$prim60528 = alloca %struct.ScmObj*, align 8
%_95k47437 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59385)
store volatile %struct.ScmObj* %_95k47437, %struct.ScmObj** %stackaddr$prim60528, align 8
%stackaddr$prim60529 = alloca %struct.ScmObj*, align 8
%current_45args59386 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59385)
store volatile %struct.ScmObj* %current_45args59386, %struct.ScmObj** %stackaddr$prim60529, align 8
%stackaddr$prim60530 = alloca %struct.ScmObj*, align 8
%anf_45bind47393 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59386)
store volatile %struct.ScmObj* %anf_45bind47393, %struct.ScmObj** %stackaddr$prim60530, align 8
%ae52577 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim60531 = alloca %struct.ScmObj*, align 8
%t4706847201 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %fibc47194, %struct.ScmObj* %ae52577, %struct.ScmObj* %anf_45bind47393)
store volatile %struct.ScmObj* %t4706847201, %struct.ScmObj** %stackaddr$prim60531, align 8
%ae52580 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim60532 = alloca %struct.ScmObj*, align 8
%anf_45bind47394 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %fibc47194, %struct.ScmObj* %ae52580)
store volatile %struct.ScmObj* %anf_45bind47394, %struct.ScmObj** %stackaddr$prim60532, align 8
%ae52582 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim60533 = alloca %struct.ScmObj*, align 8
%anf_45bind47395 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %nat_45_62peano47200, %struct.ScmObj* %ae52582)
store volatile %struct.ScmObj* %anf_45bind47395, %struct.ScmObj** %stackaddr$prim60533, align 8
%stackaddr$makeclosure60534 = alloca %struct.ScmObj*, align 8
%fptrToInt60535 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52584 to i64
%ae52584 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt60535)
store volatile %struct.ScmObj* %ae52584, %struct.ScmObj** %stackaddr$makeclosure60534, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52584, %struct.ScmObj* %anf_45bind47394, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52584, %struct.ScmObj* %peano_45_62nat47199, i64 1)
%ae52585 = call %struct.ScmObj* @const_init_int(i64 13)
%argslist59404$anf_45bind473950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60536 = alloca %struct.ScmObj*, align 8
%argslist59404$anf_45bind473951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52585, %struct.ScmObj* %argslist59404$anf_45bind473950)
store volatile %struct.ScmObj* %argslist59404$anf_45bind473951, %struct.ScmObj** %stackaddr$prim60536, align 8
%stackaddr$prim60537 = alloca %struct.ScmObj*, align 8
%argslist59404$anf_45bind473952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52584, %struct.ScmObj* %argslist59404$anf_45bind473951)
store volatile %struct.ScmObj* %argslist59404$anf_45bind473952, %struct.ScmObj** %stackaddr$prim60537, align 8
%clofunc60538 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47395)
musttail call tailcc void %clofunc60538(%struct.ScmObj* %anf_45bind47395, %struct.ScmObj* %argslist59404$anf_45bind473952)
ret void
}

define tailcc void @proc_clo$ae52584(%struct.ScmObj* %env$ae52584,%struct.ScmObj* %current_45args59388) {
%stackaddr$env-ref60539 = alloca %struct.ScmObj*, align 8
%anf_45bind47394 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52584, i64 0)
store %struct.ScmObj* %anf_45bind47394, %struct.ScmObj** %stackaddr$env-ref60539
%stackaddr$env-ref60540 = alloca %struct.ScmObj*, align 8
%peano_45_62nat47199 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52584, i64 1)
store %struct.ScmObj* %peano_45_62nat47199, %struct.ScmObj** %stackaddr$env-ref60540
%stackaddr$prim60541 = alloca %struct.ScmObj*, align 8
%_95k47438 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59388)
store volatile %struct.ScmObj* %_95k47438, %struct.ScmObj** %stackaddr$prim60541, align 8
%stackaddr$prim60542 = alloca %struct.ScmObj*, align 8
%current_45args59389 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59388)
store volatile %struct.ScmObj* %current_45args59389, %struct.ScmObj** %stackaddr$prim60542, align 8
%stackaddr$prim60543 = alloca %struct.ScmObj*, align 8
%anf_45bind47396 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59389)
store volatile %struct.ScmObj* %anf_45bind47396, %struct.ScmObj** %stackaddr$prim60543, align 8
%stackaddr$makeclosure60544 = alloca %struct.ScmObj*, align 8
%fptrToInt60545 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52589 to i64
%ae52589 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt60545)
store volatile %struct.ScmObj* %ae52589, %struct.ScmObj** %stackaddr$makeclosure60544, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52589, %struct.ScmObj* %anf_45bind47396, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52589, %struct.ScmObj* %anf_45bind47394, i64 1)
%ae52590 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure60546 = alloca %struct.ScmObj*, align 8
%fptrToInt60547 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52591 to i64
%ae52591 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt60547)
store volatile %struct.ScmObj* %ae52591, %struct.ScmObj** %stackaddr$makeclosure60546, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52591, %struct.ScmObj* %peano_45_62nat47199, i64 0)
%argslist59403$ae525890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60548 = alloca %struct.ScmObj*, align 8
%argslist59403$ae525891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52591, %struct.ScmObj* %argslist59403$ae525890)
store volatile %struct.ScmObj* %argslist59403$ae525891, %struct.ScmObj** %stackaddr$prim60548, align 8
%stackaddr$prim60549 = alloca %struct.ScmObj*, align 8
%argslist59403$ae525892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52590, %struct.ScmObj* %argslist59403$ae525891)
store volatile %struct.ScmObj* %argslist59403$ae525892, %struct.ScmObj** %stackaddr$prim60549, align 8
%clofunc60550 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52589)
musttail call tailcc void %clofunc60550(%struct.ScmObj* %ae52589, %struct.ScmObj* %argslist59403$ae525892)
ret void
}

define tailcc void @proc_clo$ae52589(%struct.ScmObj* %env$ae52589,%struct.ScmObj* %current_45args59391) {
%stackaddr$env-ref60551 = alloca %struct.ScmObj*, align 8
%anf_45bind47396 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52589, i64 0)
store %struct.ScmObj* %anf_45bind47396, %struct.ScmObj** %stackaddr$env-ref60551
%stackaddr$env-ref60552 = alloca %struct.ScmObj*, align 8
%anf_45bind47394 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52589, i64 1)
store %struct.ScmObj* %anf_45bind47394, %struct.ScmObj** %stackaddr$env-ref60552
%stackaddr$prim60553 = alloca %struct.ScmObj*, align 8
%_95k47439 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59391)
store volatile %struct.ScmObj* %_95k47439, %struct.ScmObj** %stackaddr$prim60553, align 8
%stackaddr$prim60554 = alloca %struct.ScmObj*, align 8
%current_45args59392 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59391)
store volatile %struct.ScmObj* %current_45args59392, %struct.ScmObj** %stackaddr$prim60554, align 8
%stackaddr$prim60555 = alloca %struct.ScmObj*, align 8
%anf_45bind47398 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59392)
store volatile %struct.ScmObj* %anf_45bind47398, %struct.ScmObj** %stackaddr$prim60555, align 8
%stackaddr$makeclosure60556 = alloca %struct.ScmObj*, align 8
%fptrToInt60557 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52614 to i64
%ae52614 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt60557)
store volatile %struct.ScmObj* %ae52614, %struct.ScmObj** %stackaddr$makeclosure60556, align 8
%argslist59398$anf_45bind473940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60558 = alloca %struct.ScmObj*, align 8
%argslist59398$anf_45bind473941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47398, %struct.ScmObj* %argslist59398$anf_45bind473940)
store volatile %struct.ScmObj* %argslist59398$anf_45bind473941, %struct.ScmObj** %stackaddr$prim60558, align 8
%stackaddr$prim60559 = alloca %struct.ScmObj*, align 8
%argslist59398$anf_45bind473942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47396, %struct.ScmObj* %argslist59398$anf_45bind473941)
store volatile %struct.ScmObj* %argslist59398$anf_45bind473942, %struct.ScmObj** %stackaddr$prim60559, align 8
%stackaddr$prim60560 = alloca %struct.ScmObj*, align 8
%argslist59398$anf_45bind473943 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52614, %struct.ScmObj* %argslist59398$anf_45bind473942)
store volatile %struct.ScmObj* %argslist59398$anf_45bind473943, %struct.ScmObj** %stackaddr$prim60560, align 8
%clofunc60561 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47394)
musttail call tailcc void %clofunc60561(%struct.ScmObj* %anf_45bind47394, %struct.ScmObj* %argslist59398$anf_45bind473943)
ret void
}

define tailcc void @proc_clo$ae52614(%struct.ScmObj* %env$ae52614,%struct.ScmObj* %current_45args59394) {
%stackaddr$prim60562 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59394)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim60562, align 8
%stackaddr$prim60563 = alloca %struct.ScmObj*, align 8
%current_45args59395 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59394)
store volatile %struct.ScmObj* %current_45args59395, %struct.ScmObj** %stackaddr$prim60563, align 8
%stackaddr$prim60564 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59395)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim60564, align 8
%stackaddr$prim60565 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim60565, align 8
%argslist59397$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60566 = alloca %struct.ScmObj*, align 8
%argslist59397$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist59397$k0)
store volatile %struct.ScmObj* %argslist59397$k1, %struct.ScmObj** %stackaddr$prim60566, align 8
%clofunc60567 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc60567(%struct.ScmObj* %k, %struct.ScmObj* %argslist59397$k1)
ret void
}

define tailcc void @proc_clo$ae52591(%struct.ScmObj* %env$ae52591,%struct.ScmObj* %current_45args59399) {
%stackaddr$env-ref60568 = alloca %struct.ScmObj*, align 8
%peano_45_62nat47199 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52591, i64 0)
store %struct.ScmObj* %peano_45_62nat47199, %struct.ScmObj** %stackaddr$env-ref60568
%stackaddr$prim60569 = alloca %struct.ScmObj*, align 8
%k47440 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59399)
store volatile %struct.ScmObj* %k47440, %struct.ScmObj** %stackaddr$prim60569, align 8
%stackaddr$prim60570 = alloca %struct.ScmObj*, align 8
%current_45args59400 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59399)
store volatile %struct.ScmObj* %current_45args59400, %struct.ScmObj** %stackaddr$prim60570, align 8
%stackaddr$prim60571 = alloca %struct.ScmObj*, align 8
%x47223 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59400)
store volatile %struct.ScmObj* %x47223, %struct.ScmObj** %stackaddr$prim60571, align 8
%ae52593 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim60572 = alloca %struct.ScmObj*, align 8
%anf_45bind47397 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %peano_45_62nat47199, %struct.ScmObj* %ae52593)
store volatile %struct.ScmObj* %anf_45bind47397, %struct.ScmObj** %stackaddr$prim60572, align 8
%argslist59402$anf_45bind473970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60573 = alloca %struct.ScmObj*, align 8
%argslist59402$anf_45bind473971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x47223, %struct.ScmObj* %argslist59402$anf_45bind473970)
store volatile %struct.ScmObj* %argslist59402$anf_45bind473971, %struct.ScmObj** %stackaddr$prim60573, align 8
%stackaddr$prim60574 = alloca %struct.ScmObj*, align 8
%argslist59402$anf_45bind473972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47440, %struct.ScmObj* %argslist59402$anf_45bind473971)
store volatile %struct.ScmObj* %argslist59402$anf_45bind473972, %struct.ScmObj** %stackaddr$prim60574, align 8
%clofunc60575 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47397)
musttail call tailcc void %clofunc60575(%struct.ScmObj* %anf_45bind47397, %struct.ScmObj* %argslist59402$anf_45bind473972)
ret void
}

define tailcc void @proc_clo$ae51607(%struct.ScmObj* %env$ae51607,%struct.ScmObj* %current_45args59405) {
%stackaddr$env-ref60576 = alloca %struct.ScmObj*, align 8
%nat_45_62peano47200 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51607, i64 0)
store %struct.ScmObj* %nat_45_62peano47200, %struct.ScmObj** %stackaddr$env-ref60576
%stackaddr$env-ref60577 = alloca %struct.ScmObj*, align 8
%pred47197 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51607, i64 1)
store %struct.ScmObj* %pred47197, %struct.ScmObj** %stackaddr$env-ref60577
%stackaddr$env-ref60578 = alloca %struct.ScmObj*, align 8
%z_6347196 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51607, i64 2)
store %struct.ScmObj* %z_6347196, %struct.ScmObj** %stackaddr$env-ref60578
%stackaddr$env-ref60579 = alloca %struct.ScmObj*, align 8
%addc47195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51607, i64 3)
store %struct.ScmObj* %addc47195, %struct.ScmObj** %stackaddr$env-ref60579
%stackaddr$env-ref60580 = alloca %struct.ScmObj*, align 8
%fibc47194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51607, i64 4)
store %struct.ScmObj* %fibc47194, %struct.ScmObj** %stackaddr$env-ref60580
%stackaddr$prim60581 = alloca %struct.ScmObj*, align 8
%k47441 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59405)
store volatile %struct.ScmObj* %k47441, %struct.ScmObj** %stackaddr$prim60581, align 8
%stackaddr$prim60582 = alloca %struct.ScmObj*, align 8
%current_45args59406 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59405)
store volatile %struct.ScmObj* %current_45args59406, %struct.ScmObj** %stackaddr$prim60582, align 8
%stackaddr$prim60583 = alloca %struct.ScmObj*, align 8
%x47203 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59406)
store volatile %struct.ScmObj* %x47203, %struct.ScmObj** %stackaddr$prim60583, align 8
%stackaddr$prim60584 = alloca %struct.ScmObj*, align 8
%current_45args59407 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59406)
store volatile %struct.ScmObj* %current_45args59407, %struct.ScmObj** %stackaddr$prim60584, align 8
%stackaddr$prim60585 = alloca %struct.ScmObj*, align 8
%c47202 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59407)
store volatile %struct.ScmObj* %c47202, %struct.ScmObj** %stackaddr$prim60585, align 8
%ae51609 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim60586 = alloca %struct.ScmObj*, align 8
%anf_45bind47370 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %z_6347196, %struct.ScmObj* %ae51609)
store volatile %struct.ScmObj* %anf_45bind47370, %struct.ScmObj** %stackaddr$prim60586, align 8
%stackaddr$makeclosure60587 = alloca %struct.ScmObj*, align 8
%fptrToInt60588 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51611 to i64
%ae51611 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt60588)
store volatile %struct.ScmObj* %ae51611, %struct.ScmObj** %stackaddr$makeclosure60587, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51611, %struct.ScmObj* %x47203, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51611, %struct.ScmObj* %c47202, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51611, %struct.ScmObj* %k47441, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51611, %struct.ScmObj* %nat_45_62peano47200, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51611, %struct.ScmObj* %pred47197, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51611, %struct.ScmObj* %z_6347196, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae51611, %struct.ScmObj* %addc47195, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae51611, %struct.ScmObj* %fibc47194, i64 7)
%argslist59499$anf_45bind473700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60589 = alloca %struct.ScmObj*, align 8
%argslist59499$anf_45bind473701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x47203, %struct.ScmObj* %argslist59499$anf_45bind473700)
store volatile %struct.ScmObj* %argslist59499$anf_45bind473701, %struct.ScmObj** %stackaddr$prim60589, align 8
%stackaddr$prim60590 = alloca %struct.ScmObj*, align 8
%argslist59499$anf_45bind473702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51611, %struct.ScmObj* %argslist59499$anf_45bind473701)
store volatile %struct.ScmObj* %argslist59499$anf_45bind473702, %struct.ScmObj** %stackaddr$prim60590, align 8
%clofunc60591 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47370)
musttail call tailcc void %clofunc60591(%struct.ScmObj* %anf_45bind47370, %struct.ScmObj* %argslist59499$anf_45bind473702)
ret void
}

define tailcc void @proc_clo$ae51611(%struct.ScmObj* %env$ae51611,%struct.ScmObj* %current_45args59409) {
%stackaddr$env-ref60592 = alloca %struct.ScmObj*, align 8
%x47203 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51611, i64 0)
store %struct.ScmObj* %x47203, %struct.ScmObj** %stackaddr$env-ref60592
%stackaddr$env-ref60593 = alloca %struct.ScmObj*, align 8
%c47202 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51611, i64 1)
store %struct.ScmObj* %c47202, %struct.ScmObj** %stackaddr$env-ref60593
%stackaddr$env-ref60594 = alloca %struct.ScmObj*, align 8
%k47441 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51611, i64 2)
store %struct.ScmObj* %k47441, %struct.ScmObj** %stackaddr$env-ref60594
%stackaddr$env-ref60595 = alloca %struct.ScmObj*, align 8
%nat_45_62peano47200 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51611, i64 3)
store %struct.ScmObj* %nat_45_62peano47200, %struct.ScmObj** %stackaddr$env-ref60595
%stackaddr$env-ref60596 = alloca %struct.ScmObj*, align 8
%pred47197 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51611, i64 4)
store %struct.ScmObj* %pred47197, %struct.ScmObj** %stackaddr$env-ref60596
%stackaddr$env-ref60597 = alloca %struct.ScmObj*, align 8
%z_6347196 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51611, i64 5)
store %struct.ScmObj* %z_6347196, %struct.ScmObj** %stackaddr$env-ref60597
%stackaddr$env-ref60598 = alloca %struct.ScmObj*, align 8
%addc47195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51611, i64 6)
store %struct.ScmObj* %addc47195, %struct.ScmObj** %stackaddr$env-ref60598
%stackaddr$env-ref60599 = alloca %struct.ScmObj*, align 8
%fibc47194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51611, i64 7)
store %struct.ScmObj* %fibc47194, %struct.ScmObj** %stackaddr$env-ref60599
%stackaddr$prim60600 = alloca %struct.ScmObj*, align 8
%_95k47442 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59409)
store volatile %struct.ScmObj* %_95k47442, %struct.ScmObj** %stackaddr$prim60600, align 8
%stackaddr$prim60601 = alloca %struct.ScmObj*, align 8
%current_45args59410 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59409)
store volatile %struct.ScmObj* %current_45args59410, %struct.ScmObj** %stackaddr$prim60601, align 8
%stackaddr$prim60602 = alloca %struct.ScmObj*, align 8
%anf_45bind47371 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59410)
store volatile %struct.ScmObj* %anf_45bind47371, %struct.ScmObj** %stackaddr$prim60602, align 8
%truthy$cmp60603 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47371)
%cmp$cmp60603 = icmp eq i64 %truthy$cmp60603, 1
br i1 %cmp$cmp60603, label %truebranch$cmp60603, label %falsebranch$cmp60603
truebranch$cmp60603:
%ae51615 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim60604 = alloca %struct.ScmObj*, align 8
%anf_45bind47372 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %nat_45_62peano47200, %struct.ScmObj* %ae51615)
store volatile %struct.ScmObj* %anf_45bind47372, %struct.ScmObj** %stackaddr$prim60604, align 8
%stackaddr$makeclosure60605 = alloca %struct.ScmObj*, align 8
%fptrToInt60606 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51617 to i64
%ae51617 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt60606)
store volatile %struct.ScmObj* %ae51617, %struct.ScmObj** %stackaddr$makeclosure60605, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51617, %struct.ScmObj* %c47202, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51617, %struct.ScmObj* %k47441, i64 1)
%ae51618 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist59416$anf_45bind473720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60607 = alloca %struct.ScmObj*, align 8
%argslist59416$anf_45bind473721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51618, %struct.ScmObj* %argslist59416$anf_45bind473720)
store volatile %struct.ScmObj* %argslist59416$anf_45bind473721, %struct.ScmObj** %stackaddr$prim60607, align 8
%stackaddr$prim60608 = alloca %struct.ScmObj*, align 8
%argslist59416$anf_45bind473722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51617, %struct.ScmObj* %argslist59416$anf_45bind473721)
store volatile %struct.ScmObj* %argslist59416$anf_45bind473722, %struct.ScmObj** %stackaddr$prim60608, align 8
%clofunc60609 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47372)
musttail call tailcc void %clofunc60609(%struct.ScmObj* %anf_45bind47372, %struct.ScmObj* %argslist59416$anf_45bind473722)
ret void
falsebranch$cmp60603:
%ae51637 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim60610 = alloca %struct.ScmObj*, align 8
%anf_45bind47374 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %z_6347196, %struct.ScmObj* %ae51637)
store volatile %struct.ScmObj* %anf_45bind47374, %struct.ScmObj** %stackaddr$prim60610, align 8
%ae51639 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim60611 = alloca %struct.ScmObj*, align 8
%anf_45bind47375 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %pred47197, %struct.ScmObj* %ae51639)
store volatile %struct.ScmObj* %anf_45bind47375, %struct.ScmObj** %stackaddr$prim60611, align 8
%stackaddr$makeclosure60612 = alloca %struct.ScmObj*, align 8
%fptrToInt60613 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51641 to i64
%ae51641 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt60613)
store volatile %struct.ScmObj* %ae51641, %struct.ScmObj** %stackaddr$makeclosure60612, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51641, %struct.ScmObj* %x47203, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51641, %struct.ScmObj* %c47202, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51641, %struct.ScmObj* %k47441, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51641, %struct.ScmObj* %nat_45_62peano47200, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51641, %struct.ScmObj* %anf_45bind47374, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51641, %struct.ScmObj* %pred47197, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae51641, %struct.ScmObj* %addc47195, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae51641, %struct.ScmObj* %fibc47194, i64 7)
%argslist59498$anf_45bind473750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60614 = alloca %struct.ScmObj*, align 8
%argslist59498$anf_45bind473751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x47203, %struct.ScmObj* %argslist59498$anf_45bind473750)
store volatile %struct.ScmObj* %argslist59498$anf_45bind473751, %struct.ScmObj** %stackaddr$prim60614, align 8
%stackaddr$prim60615 = alloca %struct.ScmObj*, align 8
%argslist59498$anf_45bind473752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51641, %struct.ScmObj* %argslist59498$anf_45bind473751)
store volatile %struct.ScmObj* %argslist59498$anf_45bind473752, %struct.ScmObj** %stackaddr$prim60615, align 8
%clofunc60616 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47375)
musttail call tailcc void %clofunc60616(%struct.ScmObj* %anf_45bind47375, %struct.ScmObj* %argslist59498$anf_45bind473752)
ret void
}

define tailcc void @proc_clo$ae51617(%struct.ScmObj* %env$ae51617,%struct.ScmObj* %current_45args59412) {
%stackaddr$env-ref60617 = alloca %struct.ScmObj*, align 8
%c47202 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51617, i64 0)
store %struct.ScmObj* %c47202, %struct.ScmObj** %stackaddr$env-ref60617
%stackaddr$env-ref60618 = alloca %struct.ScmObj*, align 8
%k47441 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51617, i64 1)
store %struct.ScmObj* %k47441, %struct.ScmObj** %stackaddr$env-ref60618
%stackaddr$prim60619 = alloca %struct.ScmObj*, align 8
%_95k47443 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59412)
store volatile %struct.ScmObj* %_95k47443, %struct.ScmObj** %stackaddr$prim60619, align 8
%stackaddr$prim60620 = alloca %struct.ScmObj*, align 8
%current_45args59413 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59412)
store volatile %struct.ScmObj* %current_45args59413, %struct.ScmObj** %stackaddr$prim60620, align 8
%stackaddr$prim60621 = alloca %struct.ScmObj*, align 8
%anf_45bind47373 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59413)
store volatile %struct.ScmObj* %anf_45bind47373, %struct.ScmObj** %stackaddr$prim60621, align 8
%argslist59415$c472020 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60622 = alloca %struct.ScmObj*, align 8
%argslist59415$c472021 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47373, %struct.ScmObj* %argslist59415$c472020)
store volatile %struct.ScmObj* %argslist59415$c472021, %struct.ScmObj** %stackaddr$prim60622, align 8
%stackaddr$prim60623 = alloca %struct.ScmObj*, align 8
%argslist59415$c472022 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47441, %struct.ScmObj* %argslist59415$c472021)
store volatile %struct.ScmObj* %argslist59415$c472022, %struct.ScmObj** %stackaddr$prim60623, align 8
%clofunc60624 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %c47202)
musttail call tailcc void %clofunc60624(%struct.ScmObj* %c47202, %struct.ScmObj* %argslist59415$c472022)
ret void
}

define tailcc void @proc_clo$ae51641(%struct.ScmObj* %env$ae51641,%struct.ScmObj* %current_45args59417) {
%stackaddr$env-ref60625 = alloca %struct.ScmObj*, align 8
%x47203 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51641, i64 0)
store %struct.ScmObj* %x47203, %struct.ScmObj** %stackaddr$env-ref60625
%stackaddr$env-ref60626 = alloca %struct.ScmObj*, align 8
%c47202 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51641, i64 1)
store %struct.ScmObj* %c47202, %struct.ScmObj** %stackaddr$env-ref60626
%stackaddr$env-ref60627 = alloca %struct.ScmObj*, align 8
%k47441 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51641, i64 2)
store %struct.ScmObj* %k47441, %struct.ScmObj** %stackaddr$env-ref60627
%stackaddr$env-ref60628 = alloca %struct.ScmObj*, align 8
%nat_45_62peano47200 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51641, i64 3)
store %struct.ScmObj* %nat_45_62peano47200, %struct.ScmObj** %stackaddr$env-ref60628
%stackaddr$env-ref60629 = alloca %struct.ScmObj*, align 8
%anf_45bind47374 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51641, i64 4)
store %struct.ScmObj* %anf_45bind47374, %struct.ScmObj** %stackaddr$env-ref60629
%stackaddr$env-ref60630 = alloca %struct.ScmObj*, align 8
%pred47197 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51641, i64 5)
store %struct.ScmObj* %pred47197, %struct.ScmObj** %stackaddr$env-ref60630
%stackaddr$env-ref60631 = alloca %struct.ScmObj*, align 8
%addc47195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51641, i64 6)
store %struct.ScmObj* %addc47195, %struct.ScmObj** %stackaddr$env-ref60631
%stackaddr$env-ref60632 = alloca %struct.ScmObj*, align 8
%fibc47194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51641, i64 7)
store %struct.ScmObj* %fibc47194, %struct.ScmObj** %stackaddr$env-ref60632
%stackaddr$prim60633 = alloca %struct.ScmObj*, align 8
%_95k47444 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59417)
store volatile %struct.ScmObj* %_95k47444, %struct.ScmObj** %stackaddr$prim60633, align 8
%stackaddr$prim60634 = alloca %struct.ScmObj*, align 8
%current_45args59418 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59417)
store volatile %struct.ScmObj* %current_45args59418, %struct.ScmObj** %stackaddr$prim60634, align 8
%stackaddr$prim60635 = alloca %struct.ScmObj*, align 8
%anf_45bind47376 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59418)
store volatile %struct.ScmObj* %anf_45bind47376, %struct.ScmObj** %stackaddr$prim60635, align 8
%stackaddr$makeclosure60636 = alloca %struct.ScmObj*, align 8
%fptrToInt60637 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51644 to i64
%ae51644 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt60637)
store volatile %struct.ScmObj* %ae51644, %struct.ScmObj** %stackaddr$makeclosure60636, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51644, %struct.ScmObj* %x47203, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51644, %struct.ScmObj* %c47202, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51644, %struct.ScmObj* %k47441, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51644, %struct.ScmObj* %nat_45_62peano47200, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51644, %struct.ScmObj* %pred47197, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51644, %struct.ScmObj* %addc47195, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae51644, %struct.ScmObj* %fibc47194, i64 6)
%argslist59497$anf_45bind473740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60638 = alloca %struct.ScmObj*, align 8
%argslist59497$anf_45bind473741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47376, %struct.ScmObj* %argslist59497$anf_45bind473740)
store volatile %struct.ScmObj* %argslist59497$anf_45bind473741, %struct.ScmObj** %stackaddr$prim60638, align 8
%stackaddr$prim60639 = alloca %struct.ScmObj*, align 8
%argslist59497$anf_45bind473742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51644, %struct.ScmObj* %argslist59497$anf_45bind473741)
store volatile %struct.ScmObj* %argslist59497$anf_45bind473742, %struct.ScmObj** %stackaddr$prim60639, align 8
%clofunc60640 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47374)
musttail call tailcc void %clofunc60640(%struct.ScmObj* %anf_45bind47374, %struct.ScmObj* %argslist59497$anf_45bind473742)
ret void
}

define tailcc void @proc_clo$ae51644(%struct.ScmObj* %env$ae51644,%struct.ScmObj* %current_45args59420) {
%stackaddr$env-ref60641 = alloca %struct.ScmObj*, align 8
%x47203 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51644, i64 0)
store %struct.ScmObj* %x47203, %struct.ScmObj** %stackaddr$env-ref60641
%stackaddr$env-ref60642 = alloca %struct.ScmObj*, align 8
%c47202 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51644, i64 1)
store %struct.ScmObj* %c47202, %struct.ScmObj** %stackaddr$env-ref60642
%stackaddr$env-ref60643 = alloca %struct.ScmObj*, align 8
%k47441 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51644, i64 2)
store %struct.ScmObj* %k47441, %struct.ScmObj** %stackaddr$env-ref60643
%stackaddr$env-ref60644 = alloca %struct.ScmObj*, align 8
%nat_45_62peano47200 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51644, i64 3)
store %struct.ScmObj* %nat_45_62peano47200, %struct.ScmObj** %stackaddr$env-ref60644
%stackaddr$env-ref60645 = alloca %struct.ScmObj*, align 8
%pred47197 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51644, i64 4)
store %struct.ScmObj* %pred47197, %struct.ScmObj** %stackaddr$env-ref60645
%stackaddr$env-ref60646 = alloca %struct.ScmObj*, align 8
%addc47195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51644, i64 5)
store %struct.ScmObj* %addc47195, %struct.ScmObj** %stackaddr$env-ref60646
%stackaddr$env-ref60647 = alloca %struct.ScmObj*, align 8
%fibc47194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51644, i64 6)
store %struct.ScmObj* %fibc47194, %struct.ScmObj** %stackaddr$env-ref60647
%stackaddr$prim60648 = alloca %struct.ScmObj*, align 8
%_95k47445 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59420)
store volatile %struct.ScmObj* %_95k47445, %struct.ScmObj** %stackaddr$prim60648, align 8
%stackaddr$prim60649 = alloca %struct.ScmObj*, align 8
%current_45args59421 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59420)
store volatile %struct.ScmObj* %current_45args59421, %struct.ScmObj** %stackaddr$prim60649, align 8
%stackaddr$prim60650 = alloca %struct.ScmObj*, align 8
%anf_45bind47377 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59421)
store volatile %struct.ScmObj* %anf_45bind47377, %struct.ScmObj** %stackaddr$prim60650, align 8
%truthy$cmp60651 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47377)
%cmp$cmp60651 = icmp eq i64 %truthy$cmp60651, 1
br i1 %cmp$cmp60651, label %truebranch$cmp60651, label %falsebranch$cmp60651
truebranch$cmp60651:
%ae51648 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim60652 = alloca %struct.ScmObj*, align 8
%anf_45bind47378 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %nat_45_62peano47200, %struct.ScmObj* %ae51648)
store volatile %struct.ScmObj* %anf_45bind47378, %struct.ScmObj** %stackaddr$prim60652, align 8
%stackaddr$makeclosure60653 = alloca %struct.ScmObj*, align 8
%fptrToInt60654 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51650 to i64
%ae51650 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt60654)
store volatile %struct.ScmObj* %ae51650, %struct.ScmObj** %stackaddr$makeclosure60653, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51650, %struct.ScmObj* %c47202, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51650, %struct.ScmObj* %k47441, i64 1)
%ae51651 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist59427$anf_45bind473780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60655 = alloca %struct.ScmObj*, align 8
%argslist59427$anf_45bind473781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51651, %struct.ScmObj* %argslist59427$anf_45bind473780)
store volatile %struct.ScmObj* %argslist59427$anf_45bind473781, %struct.ScmObj** %stackaddr$prim60655, align 8
%stackaddr$prim60656 = alloca %struct.ScmObj*, align 8
%argslist59427$anf_45bind473782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51650, %struct.ScmObj* %argslist59427$anf_45bind473781)
store volatile %struct.ScmObj* %argslist59427$anf_45bind473782, %struct.ScmObj** %stackaddr$prim60656, align 8
%clofunc60657 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47378)
musttail call tailcc void %clofunc60657(%struct.ScmObj* %anf_45bind47378, %struct.ScmObj* %argslist59427$anf_45bind473782)
ret void
falsebranch$cmp60651:
%ae51670 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim60658 = alloca %struct.ScmObj*, align 8
%anf_45bind47380 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %addc47195, %struct.ScmObj* %ae51670)
store volatile %struct.ScmObj* %anf_45bind47380, %struct.ScmObj** %stackaddr$prim60658, align 8
%stackaddr$makeclosure60659 = alloca %struct.ScmObj*, align 8
%fptrToInt60660 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51671 to i64
%ae51671 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt60660)
store volatile %struct.ScmObj* %ae51671, %struct.ScmObj** %stackaddr$makeclosure60659, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51671, %struct.ScmObj* %anf_45bind47380, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51671, %struct.ScmObj* %x47203, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51671, %struct.ScmObj* %c47202, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51671, %struct.ScmObj* %k47441, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51671, %struct.ScmObj* %pred47197, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51671, %struct.ScmObj* %fibc47194, i64 5)
%ae51672 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure60661 = alloca %struct.ScmObj*, align 8
%fptrToInt60662 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51673 to i64
%ae51673 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt60662)
store volatile %struct.ScmObj* %ae51673, %struct.ScmObj** %stackaddr$makeclosure60661, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51673, %struct.ScmObj* %x47203, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51673, %struct.ScmObj* %pred47197, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51673, %struct.ScmObj* %fibc47194, i64 2)
%argslist59496$ae516710 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60663 = alloca %struct.ScmObj*, align 8
%argslist59496$ae516711 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51673, %struct.ScmObj* %argslist59496$ae516710)
store volatile %struct.ScmObj* %argslist59496$ae516711, %struct.ScmObj** %stackaddr$prim60663, align 8
%stackaddr$prim60664 = alloca %struct.ScmObj*, align 8
%argslist59496$ae516712 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51672, %struct.ScmObj* %argslist59496$ae516711)
store volatile %struct.ScmObj* %argslist59496$ae516712, %struct.ScmObj** %stackaddr$prim60664, align 8
%clofunc60665 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51671)
musttail call tailcc void %clofunc60665(%struct.ScmObj* %ae51671, %struct.ScmObj* %argslist59496$ae516712)
ret void
}

define tailcc void @proc_clo$ae51650(%struct.ScmObj* %env$ae51650,%struct.ScmObj* %current_45args59423) {
%stackaddr$env-ref60666 = alloca %struct.ScmObj*, align 8
%c47202 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51650, i64 0)
store %struct.ScmObj* %c47202, %struct.ScmObj** %stackaddr$env-ref60666
%stackaddr$env-ref60667 = alloca %struct.ScmObj*, align 8
%k47441 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51650, i64 1)
store %struct.ScmObj* %k47441, %struct.ScmObj** %stackaddr$env-ref60667
%stackaddr$prim60668 = alloca %struct.ScmObj*, align 8
%_95k47446 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59423)
store volatile %struct.ScmObj* %_95k47446, %struct.ScmObj** %stackaddr$prim60668, align 8
%stackaddr$prim60669 = alloca %struct.ScmObj*, align 8
%current_45args59424 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59423)
store volatile %struct.ScmObj* %current_45args59424, %struct.ScmObj** %stackaddr$prim60669, align 8
%stackaddr$prim60670 = alloca %struct.ScmObj*, align 8
%anf_45bind47379 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59424)
store volatile %struct.ScmObj* %anf_45bind47379, %struct.ScmObj** %stackaddr$prim60670, align 8
%argslist59426$c472020 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60671 = alloca %struct.ScmObj*, align 8
%argslist59426$c472021 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47379, %struct.ScmObj* %argslist59426$c472020)
store volatile %struct.ScmObj* %argslist59426$c472021, %struct.ScmObj** %stackaddr$prim60671, align 8
%stackaddr$prim60672 = alloca %struct.ScmObj*, align 8
%argslist59426$c472022 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47441, %struct.ScmObj* %argslist59426$c472021)
store volatile %struct.ScmObj* %argslist59426$c472022, %struct.ScmObj** %stackaddr$prim60672, align 8
%clofunc60673 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %c47202)
musttail call tailcc void %clofunc60673(%struct.ScmObj* %c47202, %struct.ScmObj* %argslist59426$c472022)
ret void
}

define tailcc void @proc_clo$ae51671(%struct.ScmObj* %env$ae51671,%struct.ScmObj* %current_45args59428) {
%stackaddr$env-ref60674 = alloca %struct.ScmObj*, align 8
%anf_45bind47380 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51671, i64 0)
store %struct.ScmObj* %anf_45bind47380, %struct.ScmObj** %stackaddr$env-ref60674
%stackaddr$env-ref60675 = alloca %struct.ScmObj*, align 8
%x47203 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51671, i64 1)
store %struct.ScmObj* %x47203, %struct.ScmObj** %stackaddr$env-ref60675
%stackaddr$env-ref60676 = alloca %struct.ScmObj*, align 8
%c47202 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51671, i64 2)
store %struct.ScmObj* %c47202, %struct.ScmObj** %stackaddr$env-ref60676
%stackaddr$env-ref60677 = alloca %struct.ScmObj*, align 8
%k47441 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51671, i64 3)
store %struct.ScmObj* %k47441, %struct.ScmObj** %stackaddr$env-ref60677
%stackaddr$env-ref60678 = alloca %struct.ScmObj*, align 8
%pred47197 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51671, i64 4)
store %struct.ScmObj* %pred47197, %struct.ScmObj** %stackaddr$env-ref60678
%stackaddr$env-ref60679 = alloca %struct.ScmObj*, align 8
%fibc47194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51671, i64 5)
store %struct.ScmObj* %fibc47194, %struct.ScmObj** %stackaddr$env-ref60679
%stackaddr$prim60680 = alloca %struct.ScmObj*, align 8
%_95k47447 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59428)
store volatile %struct.ScmObj* %_95k47447, %struct.ScmObj** %stackaddr$prim60680, align 8
%stackaddr$prim60681 = alloca %struct.ScmObj*, align 8
%current_45args59429 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59428)
store volatile %struct.ScmObj* %current_45args59429, %struct.ScmObj** %stackaddr$prim60681, align 8
%stackaddr$prim60682 = alloca %struct.ScmObj*, align 8
%anf_45bind47384 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59429)
store volatile %struct.ScmObj* %anf_45bind47384, %struct.ScmObj** %stackaddr$prim60682, align 8
%stackaddr$makeclosure60683 = alloca %struct.ScmObj*, align 8
%fptrToInt60684 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51726 to i64
%ae51726 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt60684)
store volatile %struct.ScmObj* %ae51726, %struct.ScmObj** %stackaddr$makeclosure60683, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51726, %struct.ScmObj* %anf_45bind47380, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51726, %struct.ScmObj* %x47203, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51726, %struct.ScmObj* %c47202, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51726, %struct.ScmObj* %k47441, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51726, %struct.ScmObj* %pred47197, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51726, %struct.ScmObj* %fibc47194, i64 5)
%stackaddr$makeclosure60685 = alloca %struct.ScmObj*, align 8
%fptrToInt60686 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51727 to i64
%ae51727 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt60686)
store volatile %struct.ScmObj* %ae51727, %struct.ScmObj** %stackaddr$makeclosure60685, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51727, %struct.ScmObj* %anf_45bind47380, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51727, %struct.ScmObj* %x47203, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51727, %struct.ScmObj* %c47202, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51727, %struct.ScmObj* %k47441, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51727, %struct.ScmObj* %pred47197, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51727, %struct.ScmObj* %fibc47194, i64 5)
%argslist59487$anf_45bind473840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60687 = alloca %struct.ScmObj*, align 8
%argslist59487$anf_45bind473841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51727, %struct.ScmObj* %argslist59487$anf_45bind473840)
store volatile %struct.ScmObj* %argslist59487$anf_45bind473841, %struct.ScmObj** %stackaddr$prim60687, align 8
%stackaddr$prim60688 = alloca %struct.ScmObj*, align 8
%argslist59487$anf_45bind473842 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51726, %struct.ScmObj* %argslist59487$anf_45bind473841)
store volatile %struct.ScmObj* %argslist59487$anf_45bind473842, %struct.ScmObj** %stackaddr$prim60688, align 8
%clofunc60689 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47384)
musttail call tailcc void %clofunc60689(%struct.ScmObj* %anf_45bind47384, %struct.ScmObj* %argslist59487$anf_45bind473842)
ret void
}

define tailcc void @proc_clo$ae51726(%struct.ScmObj* %env$ae51726,%struct.ScmObj* %current_45args59431) {
%stackaddr$env-ref60690 = alloca %struct.ScmObj*, align 8
%anf_45bind47380 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51726, i64 0)
store %struct.ScmObj* %anf_45bind47380, %struct.ScmObj** %stackaddr$env-ref60690
%stackaddr$env-ref60691 = alloca %struct.ScmObj*, align 8
%x47203 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51726, i64 1)
store %struct.ScmObj* %x47203, %struct.ScmObj** %stackaddr$env-ref60691
%stackaddr$env-ref60692 = alloca %struct.ScmObj*, align 8
%c47202 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51726, i64 2)
store %struct.ScmObj* %c47202, %struct.ScmObj** %stackaddr$env-ref60692
%stackaddr$env-ref60693 = alloca %struct.ScmObj*, align 8
%k47441 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51726, i64 3)
store %struct.ScmObj* %k47441, %struct.ScmObj** %stackaddr$env-ref60693
%stackaddr$env-ref60694 = alloca %struct.ScmObj*, align 8
%pred47197 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51726, i64 4)
store %struct.ScmObj* %pred47197, %struct.ScmObj** %stackaddr$env-ref60694
%stackaddr$env-ref60695 = alloca %struct.ScmObj*, align 8
%fibc47194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51726, i64 5)
store %struct.ScmObj* %fibc47194, %struct.ScmObj** %stackaddr$env-ref60695
%stackaddr$prim60696 = alloca %struct.ScmObj*, align 8
%_95k47448 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59431)
store volatile %struct.ScmObj* %_95k47448, %struct.ScmObj** %stackaddr$prim60696, align 8
%stackaddr$prim60697 = alloca %struct.ScmObj*, align 8
%current_45args59432 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59431)
store volatile %struct.ScmObj* %current_45args59432, %struct.ScmObj** %stackaddr$prim60697, align 8
%stackaddr$prim60698 = alloca %struct.ScmObj*, align 8
%anf_45bind47385 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59432)
store volatile %struct.ScmObj* %anf_45bind47385, %struct.ScmObj** %stackaddr$prim60698, align 8
%stackaddr$makeclosure60699 = alloca %struct.ScmObj*, align 8
%fptrToInt60700 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51863 to i64
%ae51863 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt60700)
store volatile %struct.ScmObj* %ae51863, %struct.ScmObj** %stackaddr$makeclosure60699, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51863, %struct.ScmObj* %anf_45bind47380, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51863, %struct.ScmObj* %c47202, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51863, %struct.ScmObj* %k47441, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51863, %struct.ScmObj* %anf_45bind47385, i64 3)
%ae51864 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure60701 = alloca %struct.ScmObj*, align 8
%fptrToInt60702 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51865 to i64
%ae51865 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt60702)
store volatile %struct.ScmObj* %ae51865, %struct.ScmObj** %stackaddr$makeclosure60701, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51865, %struct.ScmObj* %x47203, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51865, %struct.ScmObj* %pred47197, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51865, %struct.ScmObj* %fibc47194, i64 2)
%argslist59458$ae518630 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60703 = alloca %struct.ScmObj*, align 8
%argslist59458$ae518631 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51865, %struct.ScmObj* %argslist59458$ae518630)
store volatile %struct.ScmObj* %argslist59458$ae518631, %struct.ScmObj** %stackaddr$prim60703, align 8
%stackaddr$prim60704 = alloca %struct.ScmObj*, align 8
%argslist59458$ae518632 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51864, %struct.ScmObj* %argslist59458$ae518631)
store volatile %struct.ScmObj* %argslist59458$ae518632, %struct.ScmObj** %stackaddr$prim60704, align 8
%clofunc60705 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51863)
musttail call tailcc void %clofunc60705(%struct.ScmObj* %ae51863, %struct.ScmObj* %argslist59458$ae518632)
ret void
}

define tailcc void @proc_clo$ae51863(%struct.ScmObj* %env$ae51863,%struct.ScmObj* %current_45args59434) {
%stackaddr$env-ref60706 = alloca %struct.ScmObj*, align 8
%anf_45bind47380 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51863, i64 0)
store %struct.ScmObj* %anf_45bind47380, %struct.ScmObj** %stackaddr$env-ref60706
%stackaddr$env-ref60707 = alloca %struct.ScmObj*, align 8
%c47202 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51863, i64 1)
store %struct.ScmObj* %c47202, %struct.ScmObj** %stackaddr$env-ref60707
%stackaddr$env-ref60708 = alloca %struct.ScmObj*, align 8
%k47441 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51863, i64 2)
store %struct.ScmObj* %k47441, %struct.ScmObj** %stackaddr$env-ref60708
%stackaddr$env-ref60709 = alloca %struct.ScmObj*, align 8
%anf_45bind47385 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51863, i64 3)
store %struct.ScmObj* %anf_45bind47385, %struct.ScmObj** %stackaddr$env-ref60709
%stackaddr$prim60710 = alloca %struct.ScmObj*, align 8
%_95k47449 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59434)
store volatile %struct.ScmObj* %_95k47449, %struct.ScmObj** %stackaddr$prim60710, align 8
%stackaddr$prim60711 = alloca %struct.ScmObj*, align 8
%current_45args59435 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59434)
store volatile %struct.ScmObj* %current_45args59435, %struct.ScmObj** %stackaddr$prim60711, align 8
%stackaddr$prim60712 = alloca %struct.ScmObj*, align 8
%anf_45bind47391 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59435)
store volatile %struct.ScmObj* %anf_45bind47391, %struct.ScmObj** %stackaddr$prim60712, align 8
%stackaddr$makeclosure60713 = alloca %struct.ScmObj*, align 8
%fptrToInt60714 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51953 to i64
%ae51953 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt60714)
store volatile %struct.ScmObj* %ae51953, %struct.ScmObj** %stackaddr$makeclosure60713, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51953, %struct.ScmObj* %anf_45bind47380, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51953, %struct.ScmObj* %c47202, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51953, %struct.ScmObj* %k47441, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51953, %struct.ScmObj* %anf_45bind47385, i64 3)
%stackaddr$makeclosure60715 = alloca %struct.ScmObj*, align 8
%fptrToInt60716 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51954 to i64
%ae51954 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt60716)
store volatile %struct.ScmObj* %ae51954, %struct.ScmObj** %stackaddr$makeclosure60715, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51954, %struct.ScmObj* %anf_45bind47380, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51954, %struct.ScmObj* %c47202, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51954, %struct.ScmObj* %k47441, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51954, %struct.ScmObj* %anf_45bind47385, i64 3)
%argslist59445$anf_45bind473910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60717 = alloca %struct.ScmObj*, align 8
%argslist59445$anf_45bind473911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51954, %struct.ScmObj* %argslist59445$anf_45bind473910)
store volatile %struct.ScmObj* %argslist59445$anf_45bind473911, %struct.ScmObj** %stackaddr$prim60717, align 8
%stackaddr$prim60718 = alloca %struct.ScmObj*, align 8
%argslist59445$anf_45bind473912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51953, %struct.ScmObj* %argslist59445$anf_45bind473911)
store volatile %struct.ScmObj* %argslist59445$anf_45bind473912, %struct.ScmObj** %stackaddr$prim60718, align 8
%clofunc60719 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47391)
musttail call tailcc void %clofunc60719(%struct.ScmObj* %anf_45bind47391, %struct.ScmObj* %argslist59445$anf_45bind473912)
ret void
}

define tailcc void @proc_clo$ae51953(%struct.ScmObj* %env$ae51953,%struct.ScmObj* %current_45args59437) {
%stackaddr$env-ref60720 = alloca %struct.ScmObj*, align 8
%anf_45bind47380 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51953, i64 0)
store %struct.ScmObj* %anf_45bind47380, %struct.ScmObj** %stackaddr$env-ref60720
%stackaddr$env-ref60721 = alloca %struct.ScmObj*, align 8
%c47202 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51953, i64 1)
store %struct.ScmObj* %c47202, %struct.ScmObj** %stackaddr$env-ref60721
%stackaddr$env-ref60722 = alloca %struct.ScmObj*, align 8
%k47441 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51953, i64 2)
store %struct.ScmObj* %k47441, %struct.ScmObj** %stackaddr$env-ref60722
%stackaddr$env-ref60723 = alloca %struct.ScmObj*, align 8
%anf_45bind47385 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51953, i64 3)
store %struct.ScmObj* %anf_45bind47385, %struct.ScmObj** %stackaddr$env-ref60723
%stackaddr$prim60724 = alloca %struct.ScmObj*, align 8
%_95k47450 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59437)
store volatile %struct.ScmObj* %_95k47450, %struct.ScmObj** %stackaddr$prim60724, align 8
%stackaddr$prim60725 = alloca %struct.ScmObj*, align 8
%current_45args59438 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59437)
store volatile %struct.ScmObj* %current_45args59438, %struct.ScmObj** %stackaddr$prim60725, align 8
%stackaddr$prim60726 = alloca %struct.ScmObj*, align 8
%anf_45bind47392 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59438)
store volatile %struct.ScmObj* %anf_45bind47392, %struct.ScmObj** %stackaddr$prim60726, align 8
%argslist59440$anf_45bind473800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60727 = alloca %struct.ScmObj*, align 8
%argslist59440$anf_45bind473801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %c47202, %struct.ScmObj* %argslist59440$anf_45bind473800)
store volatile %struct.ScmObj* %argslist59440$anf_45bind473801, %struct.ScmObj** %stackaddr$prim60727, align 8
%stackaddr$prim60728 = alloca %struct.ScmObj*, align 8
%argslist59440$anf_45bind473802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47392, %struct.ScmObj* %argslist59440$anf_45bind473801)
store volatile %struct.ScmObj* %argslist59440$anf_45bind473802, %struct.ScmObj** %stackaddr$prim60728, align 8
%stackaddr$prim60729 = alloca %struct.ScmObj*, align 8
%argslist59440$anf_45bind473803 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47385, %struct.ScmObj* %argslist59440$anf_45bind473802)
store volatile %struct.ScmObj* %argslist59440$anf_45bind473803, %struct.ScmObj** %stackaddr$prim60729, align 8
%stackaddr$prim60730 = alloca %struct.ScmObj*, align 8
%argslist59440$anf_45bind473804 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47441, %struct.ScmObj* %argslist59440$anf_45bind473803)
store volatile %struct.ScmObj* %argslist59440$anf_45bind473804, %struct.ScmObj** %stackaddr$prim60730, align 8
%clofunc60731 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47380)
musttail call tailcc void %clofunc60731(%struct.ScmObj* %anf_45bind47380, %struct.ScmObj* %argslist59440$anf_45bind473804)
ret void
}

define tailcc void @proc_clo$ae51954(%struct.ScmObj* %env$ae51954,%struct.ScmObj* %current_45args59441) {
%stackaddr$env-ref60732 = alloca %struct.ScmObj*, align 8
%anf_45bind47380 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51954, i64 0)
store %struct.ScmObj* %anf_45bind47380, %struct.ScmObj** %stackaddr$env-ref60732
%stackaddr$env-ref60733 = alloca %struct.ScmObj*, align 8
%c47202 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51954, i64 1)
store %struct.ScmObj* %c47202, %struct.ScmObj** %stackaddr$env-ref60733
%stackaddr$env-ref60734 = alloca %struct.ScmObj*, align 8
%k47441 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51954, i64 2)
store %struct.ScmObj* %k47441, %struct.ScmObj** %stackaddr$env-ref60734
%stackaddr$env-ref60735 = alloca %struct.ScmObj*, align 8
%anf_45bind47385 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51954, i64 3)
store %struct.ScmObj* %anf_45bind47385, %struct.ScmObj** %stackaddr$env-ref60735
%stackaddr$prim60736 = alloca %struct.ScmObj*, align 8
%_95k47450 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59441)
store volatile %struct.ScmObj* %_95k47450, %struct.ScmObj** %stackaddr$prim60736, align 8
%stackaddr$prim60737 = alloca %struct.ScmObj*, align 8
%current_45args59442 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59441)
store volatile %struct.ScmObj* %current_45args59442, %struct.ScmObj** %stackaddr$prim60737, align 8
%stackaddr$prim60738 = alloca %struct.ScmObj*, align 8
%anf_45bind47392 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59442)
store volatile %struct.ScmObj* %anf_45bind47392, %struct.ScmObj** %stackaddr$prim60738, align 8
%argslist59444$anf_45bind473800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60739 = alloca %struct.ScmObj*, align 8
%argslist59444$anf_45bind473801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %c47202, %struct.ScmObj* %argslist59444$anf_45bind473800)
store volatile %struct.ScmObj* %argslist59444$anf_45bind473801, %struct.ScmObj** %stackaddr$prim60739, align 8
%stackaddr$prim60740 = alloca %struct.ScmObj*, align 8
%argslist59444$anf_45bind473802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47392, %struct.ScmObj* %argslist59444$anf_45bind473801)
store volatile %struct.ScmObj* %argslist59444$anf_45bind473802, %struct.ScmObj** %stackaddr$prim60740, align 8
%stackaddr$prim60741 = alloca %struct.ScmObj*, align 8
%argslist59444$anf_45bind473803 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47385, %struct.ScmObj* %argslist59444$anf_45bind473802)
store volatile %struct.ScmObj* %argslist59444$anf_45bind473803, %struct.ScmObj** %stackaddr$prim60741, align 8
%stackaddr$prim60742 = alloca %struct.ScmObj*, align 8
%argslist59444$anf_45bind473804 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47441, %struct.ScmObj* %argslist59444$anf_45bind473803)
store volatile %struct.ScmObj* %argslist59444$anf_45bind473804, %struct.ScmObj** %stackaddr$prim60742, align 8
%clofunc60743 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47380)
musttail call tailcc void %clofunc60743(%struct.ScmObj* %anf_45bind47380, %struct.ScmObj* %argslist59444$anf_45bind473804)
ret void
}

define tailcc void @proc_clo$ae51865(%struct.ScmObj* %env$ae51865,%struct.ScmObj* %current_45args59446) {
%stackaddr$env-ref60744 = alloca %struct.ScmObj*, align 8
%x47203 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51865, i64 0)
store %struct.ScmObj* %x47203, %struct.ScmObj** %stackaddr$env-ref60744
%stackaddr$env-ref60745 = alloca %struct.ScmObj*, align 8
%pred47197 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51865, i64 1)
store %struct.ScmObj* %pred47197, %struct.ScmObj** %stackaddr$env-ref60745
%stackaddr$env-ref60746 = alloca %struct.ScmObj*, align 8
%fibc47194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51865, i64 2)
store %struct.ScmObj* %fibc47194, %struct.ScmObj** %stackaddr$env-ref60746
%stackaddr$prim60747 = alloca %struct.ScmObj*, align 8
%k47451 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59446)
store volatile %struct.ScmObj* %k47451, %struct.ScmObj** %stackaddr$prim60747, align 8
%stackaddr$prim60748 = alloca %struct.ScmObj*, align 8
%current_45args59447 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59446)
store volatile %struct.ScmObj* %current_45args59447, %struct.ScmObj** %stackaddr$prim60748, align 8
%stackaddr$prim60749 = alloca %struct.ScmObj*, align 8
%c47205 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59447)
store volatile %struct.ScmObj* %c47205, %struct.ScmObj** %stackaddr$prim60749, align 8
%ae51867 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim60750 = alloca %struct.ScmObj*, align 8
%anf_45bind47386 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %fibc47194, %struct.ScmObj* %ae51867)
store volatile %struct.ScmObj* %anf_45bind47386, %struct.ScmObj** %stackaddr$prim60750, align 8
%ae51869 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim60751 = alloca %struct.ScmObj*, align 8
%anf_45bind47387 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %pred47197, %struct.ScmObj* %ae51869)
store volatile %struct.ScmObj* %anf_45bind47387, %struct.ScmObj** %stackaddr$prim60751, align 8
%ae51871 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim60752 = alloca %struct.ScmObj*, align 8
%anf_45bind47388 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %pred47197, %struct.ScmObj* %ae51871)
store volatile %struct.ScmObj* %anf_45bind47388, %struct.ScmObj** %stackaddr$prim60752, align 8
%stackaddr$makeclosure60753 = alloca %struct.ScmObj*, align 8
%fptrToInt60754 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51873 to i64
%ae51873 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt60754)
store volatile %struct.ScmObj* %ae51873, %struct.ScmObj** %stackaddr$makeclosure60753, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51873, %struct.ScmObj* %k47451, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51873, %struct.ScmObj* %anf_45bind47387, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51873, %struct.ScmObj* %c47205, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51873, %struct.ScmObj* %anf_45bind47386, i64 3)
%argslist59457$anf_45bind473880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60755 = alloca %struct.ScmObj*, align 8
%argslist59457$anf_45bind473881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x47203, %struct.ScmObj* %argslist59457$anf_45bind473880)
store volatile %struct.ScmObj* %argslist59457$anf_45bind473881, %struct.ScmObj** %stackaddr$prim60755, align 8
%stackaddr$prim60756 = alloca %struct.ScmObj*, align 8
%argslist59457$anf_45bind473882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51873, %struct.ScmObj* %argslist59457$anf_45bind473881)
store volatile %struct.ScmObj* %argslist59457$anf_45bind473882, %struct.ScmObj** %stackaddr$prim60756, align 8
%clofunc60757 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47388)
musttail call tailcc void %clofunc60757(%struct.ScmObj* %anf_45bind47388, %struct.ScmObj* %argslist59457$anf_45bind473882)
ret void
}

define tailcc void @proc_clo$ae51873(%struct.ScmObj* %env$ae51873,%struct.ScmObj* %current_45args59449) {
%stackaddr$env-ref60758 = alloca %struct.ScmObj*, align 8
%k47451 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51873, i64 0)
store %struct.ScmObj* %k47451, %struct.ScmObj** %stackaddr$env-ref60758
%stackaddr$env-ref60759 = alloca %struct.ScmObj*, align 8
%anf_45bind47387 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51873, i64 1)
store %struct.ScmObj* %anf_45bind47387, %struct.ScmObj** %stackaddr$env-ref60759
%stackaddr$env-ref60760 = alloca %struct.ScmObj*, align 8
%c47205 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51873, i64 2)
store %struct.ScmObj* %c47205, %struct.ScmObj** %stackaddr$env-ref60760
%stackaddr$env-ref60761 = alloca %struct.ScmObj*, align 8
%anf_45bind47386 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51873, i64 3)
store %struct.ScmObj* %anf_45bind47386, %struct.ScmObj** %stackaddr$env-ref60761
%stackaddr$prim60762 = alloca %struct.ScmObj*, align 8
%_95k47452 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59449)
store volatile %struct.ScmObj* %_95k47452, %struct.ScmObj** %stackaddr$prim60762, align 8
%stackaddr$prim60763 = alloca %struct.ScmObj*, align 8
%current_45args59450 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59449)
store volatile %struct.ScmObj* %current_45args59450, %struct.ScmObj** %stackaddr$prim60763, align 8
%stackaddr$prim60764 = alloca %struct.ScmObj*, align 8
%anf_45bind47389 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59450)
store volatile %struct.ScmObj* %anf_45bind47389, %struct.ScmObj** %stackaddr$prim60764, align 8
%stackaddr$makeclosure60765 = alloca %struct.ScmObj*, align 8
%fptrToInt60766 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51876 to i64
%ae51876 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt60766)
store volatile %struct.ScmObj* %ae51876, %struct.ScmObj** %stackaddr$makeclosure60765, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51876, %struct.ScmObj* %c47205, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51876, %struct.ScmObj* %k47451, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51876, %struct.ScmObj* %anf_45bind47386, i64 2)
%argslist59456$anf_45bind473870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60767 = alloca %struct.ScmObj*, align 8
%argslist59456$anf_45bind473871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47389, %struct.ScmObj* %argslist59456$anf_45bind473870)
store volatile %struct.ScmObj* %argslist59456$anf_45bind473871, %struct.ScmObj** %stackaddr$prim60767, align 8
%stackaddr$prim60768 = alloca %struct.ScmObj*, align 8
%argslist59456$anf_45bind473872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51876, %struct.ScmObj* %argslist59456$anf_45bind473871)
store volatile %struct.ScmObj* %argslist59456$anf_45bind473872, %struct.ScmObj** %stackaddr$prim60768, align 8
%clofunc60769 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47387)
musttail call tailcc void %clofunc60769(%struct.ScmObj* %anf_45bind47387, %struct.ScmObj* %argslist59456$anf_45bind473872)
ret void
}

define tailcc void @proc_clo$ae51876(%struct.ScmObj* %env$ae51876,%struct.ScmObj* %current_45args59452) {
%stackaddr$env-ref60770 = alloca %struct.ScmObj*, align 8
%c47205 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51876, i64 0)
store %struct.ScmObj* %c47205, %struct.ScmObj** %stackaddr$env-ref60770
%stackaddr$env-ref60771 = alloca %struct.ScmObj*, align 8
%k47451 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51876, i64 1)
store %struct.ScmObj* %k47451, %struct.ScmObj** %stackaddr$env-ref60771
%stackaddr$env-ref60772 = alloca %struct.ScmObj*, align 8
%anf_45bind47386 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51876, i64 2)
store %struct.ScmObj* %anf_45bind47386, %struct.ScmObj** %stackaddr$env-ref60772
%stackaddr$prim60773 = alloca %struct.ScmObj*, align 8
%_95k47453 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59452)
store volatile %struct.ScmObj* %_95k47453, %struct.ScmObj** %stackaddr$prim60773, align 8
%stackaddr$prim60774 = alloca %struct.ScmObj*, align 8
%current_45args59453 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59452)
store volatile %struct.ScmObj* %current_45args59453, %struct.ScmObj** %stackaddr$prim60774, align 8
%stackaddr$prim60775 = alloca %struct.ScmObj*, align 8
%anf_45bind47390 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59453)
store volatile %struct.ScmObj* %anf_45bind47390, %struct.ScmObj** %stackaddr$prim60775, align 8
%argslist59455$anf_45bind473860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60776 = alloca %struct.ScmObj*, align 8
%argslist59455$anf_45bind473861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %c47205, %struct.ScmObj* %argslist59455$anf_45bind473860)
store volatile %struct.ScmObj* %argslist59455$anf_45bind473861, %struct.ScmObj** %stackaddr$prim60776, align 8
%stackaddr$prim60777 = alloca %struct.ScmObj*, align 8
%argslist59455$anf_45bind473862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47390, %struct.ScmObj* %argslist59455$anf_45bind473861)
store volatile %struct.ScmObj* %argslist59455$anf_45bind473862, %struct.ScmObj** %stackaddr$prim60777, align 8
%stackaddr$prim60778 = alloca %struct.ScmObj*, align 8
%argslist59455$anf_45bind473863 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47451, %struct.ScmObj* %argslist59455$anf_45bind473862)
store volatile %struct.ScmObj* %argslist59455$anf_45bind473863, %struct.ScmObj** %stackaddr$prim60778, align 8
%clofunc60779 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47386)
musttail call tailcc void %clofunc60779(%struct.ScmObj* %anf_45bind47386, %struct.ScmObj* %argslist59455$anf_45bind473863)
ret void
}

define tailcc void @proc_clo$ae51727(%struct.ScmObj* %env$ae51727,%struct.ScmObj* %current_45args59459) {
%stackaddr$env-ref60780 = alloca %struct.ScmObj*, align 8
%anf_45bind47380 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51727, i64 0)
store %struct.ScmObj* %anf_45bind47380, %struct.ScmObj** %stackaddr$env-ref60780
%stackaddr$env-ref60781 = alloca %struct.ScmObj*, align 8
%x47203 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51727, i64 1)
store %struct.ScmObj* %x47203, %struct.ScmObj** %stackaddr$env-ref60781
%stackaddr$env-ref60782 = alloca %struct.ScmObj*, align 8
%c47202 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51727, i64 2)
store %struct.ScmObj* %c47202, %struct.ScmObj** %stackaddr$env-ref60782
%stackaddr$env-ref60783 = alloca %struct.ScmObj*, align 8
%k47441 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51727, i64 3)
store %struct.ScmObj* %k47441, %struct.ScmObj** %stackaddr$env-ref60783
%stackaddr$env-ref60784 = alloca %struct.ScmObj*, align 8
%pred47197 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51727, i64 4)
store %struct.ScmObj* %pred47197, %struct.ScmObj** %stackaddr$env-ref60784
%stackaddr$env-ref60785 = alloca %struct.ScmObj*, align 8
%fibc47194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51727, i64 5)
store %struct.ScmObj* %fibc47194, %struct.ScmObj** %stackaddr$env-ref60785
%stackaddr$prim60786 = alloca %struct.ScmObj*, align 8
%_95k47448 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59459)
store volatile %struct.ScmObj* %_95k47448, %struct.ScmObj** %stackaddr$prim60786, align 8
%stackaddr$prim60787 = alloca %struct.ScmObj*, align 8
%current_45args59460 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59459)
store volatile %struct.ScmObj* %current_45args59460, %struct.ScmObj** %stackaddr$prim60787, align 8
%stackaddr$prim60788 = alloca %struct.ScmObj*, align 8
%anf_45bind47385 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59460)
store volatile %struct.ScmObj* %anf_45bind47385, %struct.ScmObj** %stackaddr$prim60788, align 8
%stackaddr$makeclosure60789 = alloca %struct.ScmObj*, align 8
%fptrToInt60790 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51728 to i64
%ae51728 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt60790)
store volatile %struct.ScmObj* %ae51728, %struct.ScmObj** %stackaddr$makeclosure60789, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51728, %struct.ScmObj* %anf_45bind47380, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51728, %struct.ScmObj* %c47202, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51728, %struct.ScmObj* %k47441, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51728, %struct.ScmObj* %anf_45bind47385, i64 3)
%ae51729 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure60791 = alloca %struct.ScmObj*, align 8
%fptrToInt60792 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51730 to i64
%ae51730 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt60792)
store volatile %struct.ScmObj* %ae51730, %struct.ScmObj** %stackaddr$makeclosure60791, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51730, %struct.ScmObj* %x47203, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51730, %struct.ScmObj* %pred47197, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51730, %struct.ScmObj* %fibc47194, i64 2)
%argslist59486$ae517280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60793 = alloca %struct.ScmObj*, align 8
%argslist59486$ae517281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51730, %struct.ScmObj* %argslist59486$ae517280)
store volatile %struct.ScmObj* %argslist59486$ae517281, %struct.ScmObj** %stackaddr$prim60793, align 8
%stackaddr$prim60794 = alloca %struct.ScmObj*, align 8
%argslist59486$ae517282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51729, %struct.ScmObj* %argslist59486$ae517281)
store volatile %struct.ScmObj* %argslist59486$ae517282, %struct.ScmObj** %stackaddr$prim60794, align 8
%clofunc60795 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51728)
musttail call tailcc void %clofunc60795(%struct.ScmObj* %ae51728, %struct.ScmObj* %argslist59486$ae517282)
ret void
}

define tailcc void @proc_clo$ae51728(%struct.ScmObj* %env$ae51728,%struct.ScmObj* %current_45args59462) {
%stackaddr$env-ref60796 = alloca %struct.ScmObj*, align 8
%anf_45bind47380 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51728, i64 0)
store %struct.ScmObj* %anf_45bind47380, %struct.ScmObj** %stackaddr$env-ref60796
%stackaddr$env-ref60797 = alloca %struct.ScmObj*, align 8
%c47202 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51728, i64 1)
store %struct.ScmObj* %c47202, %struct.ScmObj** %stackaddr$env-ref60797
%stackaddr$env-ref60798 = alloca %struct.ScmObj*, align 8
%k47441 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51728, i64 2)
store %struct.ScmObj* %k47441, %struct.ScmObj** %stackaddr$env-ref60798
%stackaddr$env-ref60799 = alloca %struct.ScmObj*, align 8
%anf_45bind47385 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51728, i64 3)
store %struct.ScmObj* %anf_45bind47385, %struct.ScmObj** %stackaddr$env-ref60799
%stackaddr$prim60800 = alloca %struct.ScmObj*, align 8
%_95k47449 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59462)
store volatile %struct.ScmObj* %_95k47449, %struct.ScmObj** %stackaddr$prim60800, align 8
%stackaddr$prim60801 = alloca %struct.ScmObj*, align 8
%current_45args59463 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59462)
store volatile %struct.ScmObj* %current_45args59463, %struct.ScmObj** %stackaddr$prim60801, align 8
%stackaddr$prim60802 = alloca %struct.ScmObj*, align 8
%anf_45bind47391 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59463)
store volatile %struct.ScmObj* %anf_45bind47391, %struct.ScmObj** %stackaddr$prim60802, align 8
%stackaddr$makeclosure60803 = alloca %struct.ScmObj*, align 8
%fptrToInt60804 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51818 to i64
%ae51818 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt60804)
store volatile %struct.ScmObj* %ae51818, %struct.ScmObj** %stackaddr$makeclosure60803, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51818, %struct.ScmObj* %anf_45bind47380, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51818, %struct.ScmObj* %c47202, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51818, %struct.ScmObj* %k47441, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51818, %struct.ScmObj* %anf_45bind47385, i64 3)
%stackaddr$makeclosure60805 = alloca %struct.ScmObj*, align 8
%fptrToInt60806 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51819 to i64
%ae51819 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt60806)
store volatile %struct.ScmObj* %ae51819, %struct.ScmObj** %stackaddr$makeclosure60805, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51819, %struct.ScmObj* %anf_45bind47380, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51819, %struct.ScmObj* %c47202, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51819, %struct.ScmObj* %k47441, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51819, %struct.ScmObj* %anf_45bind47385, i64 3)
%argslist59473$anf_45bind473910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60807 = alloca %struct.ScmObj*, align 8
%argslist59473$anf_45bind473911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51819, %struct.ScmObj* %argslist59473$anf_45bind473910)
store volatile %struct.ScmObj* %argslist59473$anf_45bind473911, %struct.ScmObj** %stackaddr$prim60807, align 8
%stackaddr$prim60808 = alloca %struct.ScmObj*, align 8
%argslist59473$anf_45bind473912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51818, %struct.ScmObj* %argslist59473$anf_45bind473911)
store volatile %struct.ScmObj* %argslist59473$anf_45bind473912, %struct.ScmObj** %stackaddr$prim60808, align 8
%clofunc60809 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47391)
musttail call tailcc void %clofunc60809(%struct.ScmObj* %anf_45bind47391, %struct.ScmObj* %argslist59473$anf_45bind473912)
ret void
}

define tailcc void @proc_clo$ae51818(%struct.ScmObj* %env$ae51818,%struct.ScmObj* %current_45args59465) {
%stackaddr$env-ref60810 = alloca %struct.ScmObj*, align 8
%anf_45bind47380 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51818, i64 0)
store %struct.ScmObj* %anf_45bind47380, %struct.ScmObj** %stackaddr$env-ref60810
%stackaddr$env-ref60811 = alloca %struct.ScmObj*, align 8
%c47202 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51818, i64 1)
store %struct.ScmObj* %c47202, %struct.ScmObj** %stackaddr$env-ref60811
%stackaddr$env-ref60812 = alloca %struct.ScmObj*, align 8
%k47441 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51818, i64 2)
store %struct.ScmObj* %k47441, %struct.ScmObj** %stackaddr$env-ref60812
%stackaddr$env-ref60813 = alloca %struct.ScmObj*, align 8
%anf_45bind47385 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51818, i64 3)
store %struct.ScmObj* %anf_45bind47385, %struct.ScmObj** %stackaddr$env-ref60813
%stackaddr$prim60814 = alloca %struct.ScmObj*, align 8
%_95k47450 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59465)
store volatile %struct.ScmObj* %_95k47450, %struct.ScmObj** %stackaddr$prim60814, align 8
%stackaddr$prim60815 = alloca %struct.ScmObj*, align 8
%current_45args59466 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59465)
store volatile %struct.ScmObj* %current_45args59466, %struct.ScmObj** %stackaddr$prim60815, align 8
%stackaddr$prim60816 = alloca %struct.ScmObj*, align 8
%anf_45bind47392 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59466)
store volatile %struct.ScmObj* %anf_45bind47392, %struct.ScmObj** %stackaddr$prim60816, align 8
%argslist59468$anf_45bind473800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60817 = alloca %struct.ScmObj*, align 8
%argslist59468$anf_45bind473801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %c47202, %struct.ScmObj* %argslist59468$anf_45bind473800)
store volatile %struct.ScmObj* %argslist59468$anf_45bind473801, %struct.ScmObj** %stackaddr$prim60817, align 8
%stackaddr$prim60818 = alloca %struct.ScmObj*, align 8
%argslist59468$anf_45bind473802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47392, %struct.ScmObj* %argslist59468$anf_45bind473801)
store volatile %struct.ScmObj* %argslist59468$anf_45bind473802, %struct.ScmObj** %stackaddr$prim60818, align 8
%stackaddr$prim60819 = alloca %struct.ScmObj*, align 8
%argslist59468$anf_45bind473803 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47385, %struct.ScmObj* %argslist59468$anf_45bind473802)
store volatile %struct.ScmObj* %argslist59468$anf_45bind473803, %struct.ScmObj** %stackaddr$prim60819, align 8
%stackaddr$prim60820 = alloca %struct.ScmObj*, align 8
%argslist59468$anf_45bind473804 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47441, %struct.ScmObj* %argslist59468$anf_45bind473803)
store volatile %struct.ScmObj* %argslist59468$anf_45bind473804, %struct.ScmObj** %stackaddr$prim60820, align 8
%clofunc60821 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47380)
musttail call tailcc void %clofunc60821(%struct.ScmObj* %anf_45bind47380, %struct.ScmObj* %argslist59468$anf_45bind473804)
ret void
}

define tailcc void @proc_clo$ae51819(%struct.ScmObj* %env$ae51819,%struct.ScmObj* %current_45args59469) {
%stackaddr$env-ref60822 = alloca %struct.ScmObj*, align 8
%anf_45bind47380 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51819, i64 0)
store %struct.ScmObj* %anf_45bind47380, %struct.ScmObj** %stackaddr$env-ref60822
%stackaddr$env-ref60823 = alloca %struct.ScmObj*, align 8
%c47202 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51819, i64 1)
store %struct.ScmObj* %c47202, %struct.ScmObj** %stackaddr$env-ref60823
%stackaddr$env-ref60824 = alloca %struct.ScmObj*, align 8
%k47441 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51819, i64 2)
store %struct.ScmObj* %k47441, %struct.ScmObj** %stackaddr$env-ref60824
%stackaddr$env-ref60825 = alloca %struct.ScmObj*, align 8
%anf_45bind47385 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51819, i64 3)
store %struct.ScmObj* %anf_45bind47385, %struct.ScmObj** %stackaddr$env-ref60825
%stackaddr$prim60826 = alloca %struct.ScmObj*, align 8
%_95k47450 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59469)
store volatile %struct.ScmObj* %_95k47450, %struct.ScmObj** %stackaddr$prim60826, align 8
%stackaddr$prim60827 = alloca %struct.ScmObj*, align 8
%current_45args59470 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59469)
store volatile %struct.ScmObj* %current_45args59470, %struct.ScmObj** %stackaddr$prim60827, align 8
%stackaddr$prim60828 = alloca %struct.ScmObj*, align 8
%anf_45bind47392 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59470)
store volatile %struct.ScmObj* %anf_45bind47392, %struct.ScmObj** %stackaddr$prim60828, align 8
%argslist59472$anf_45bind473800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60829 = alloca %struct.ScmObj*, align 8
%argslist59472$anf_45bind473801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %c47202, %struct.ScmObj* %argslist59472$anf_45bind473800)
store volatile %struct.ScmObj* %argslist59472$anf_45bind473801, %struct.ScmObj** %stackaddr$prim60829, align 8
%stackaddr$prim60830 = alloca %struct.ScmObj*, align 8
%argslist59472$anf_45bind473802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47392, %struct.ScmObj* %argslist59472$anf_45bind473801)
store volatile %struct.ScmObj* %argslist59472$anf_45bind473802, %struct.ScmObj** %stackaddr$prim60830, align 8
%stackaddr$prim60831 = alloca %struct.ScmObj*, align 8
%argslist59472$anf_45bind473803 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47385, %struct.ScmObj* %argslist59472$anf_45bind473802)
store volatile %struct.ScmObj* %argslist59472$anf_45bind473803, %struct.ScmObj** %stackaddr$prim60831, align 8
%stackaddr$prim60832 = alloca %struct.ScmObj*, align 8
%argslist59472$anf_45bind473804 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47441, %struct.ScmObj* %argslist59472$anf_45bind473803)
store volatile %struct.ScmObj* %argslist59472$anf_45bind473804, %struct.ScmObj** %stackaddr$prim60832, align 8
%clofunc60833 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47380)
musttail call tailcc void %clofunc60833(%struct.ScmObj* %anf_45bind47380, %struct.ScmObj* %argslist59472$anf_45bind473804)
ret void
}

define tailcc void @proc_clo$ae51730(%struct.ScmObj* %env$ae51730,%struct.ScmObj* %current_45args59474) {
%stackaddr$env-ref60834 = alloca %struct.ScmObj*, align 8
%x47203 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51730, i64 0)
store %struct.ScmObj* %x47203, %struct.ScmObj** %stackaddr$env-ref60834
%stackaddr$env-ref60835 = alloca %struct.ScmObj*, align 8
%pred47197 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51730, i64 1)
store %struct.ScmObj* %pred47197, %struct.ScmObj** %stackaddr$env-ref60835
%stackaddr$env-ref60836 = alloca %struct.ScmObj*, align 8
%fibc47194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51730, i64 2)
store %struct.ScmObj* %fibc47194, %struct.ScmObj** %stackaddr$env-ref60836
%stackaddr$prim60837 = alloca %struct.ScmObj*, align 8
%k47451 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59474)
store volatile %struct.ScmObj* %k47451, %struct.ScmObj** %stackaddr$prim60837, align 8
%stackaddr$prim60838 = alloca %struct.ScmObj*, align 8
%current_45args59475 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59474)
store volatile %struct.ScmObj* %current_45args59475, %struct.ScmObj** %stackaddr$prim60838, align 8
%stackaddr$prim60839 = alloca %struct.ScmObj*, align 8
%c47205 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59475)
store volatile %struct.ScmObj* %c47205, %struct.ScmObj** %stackaddr$prim60839, align 8
%ae51732 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim60840 = alloca %struct.ScmObj*, align 8
%anf_45bind47386 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %fibc47194, %struct.ScmObj* %ae51732)
store volatile %struct.ScmObj* %anf_45bind47386, %struct.ScmObj** %stackaddr$prim60840, align 8
%ae51734 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim60841 = alloca %struct.ScmObj*, align 8
%anf_45bind47387 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %pred47197, %struct.ScmObj* %ae51734)
store volatile %struct.ScmObj* %anf_45bind47387, %struct.ScmObj** %stackaddr$prim60841, align 8
%ae51736 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim60842 = alloca %struct.ScmObj*, align 8
%anf_45bind47388 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %pred47197, %struct.ScmObj* %ae51736)
store volatile %struct.ScmObj* %anf_45bind47388, %struct.ScmObj** %stackaddr$prim60842, align 8
%stackaddr$makeclosure60843 = alloca %struct.ScmObj*, align 8
%fptrToInt60844 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51738 to i64
%ae51738 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt60844)
store volatile %struct.ScmObj* %ae51738, %struct.ScmObj** %stackaddr$makeclosure60843, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51738, %struct.ScmObj* %k47451, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51738, %struct.ScmObj* %anf_45bind47387, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51738, %struct.ScmObj* %c47205, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51738, %struct.ScmObj* %anf_45bind47386, i64 3)
%argslist59485$anf_45bind473880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60845 = alloca %struct.ScmObj*, align 8
%argslist59485$anf_45bind473881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x47203, %struct.ScmObj* %argslist59485$anf_45bind473880)
store volatile %struct.ScmObj* %argslist59485$anf_45bind473881, %struct.ScmObj** %stackaddr$prim60845, align 8
%stackaddr$prim60846 = alloca %struct.ScmObj*, align 8
%argslist59485$anf_45bind473882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51738, %struct.ScmObj* %argslist59485$anf_45bind473881)
store volatile %struct.ScmObj* %argslist59485$anf_45bind473882, %struct.ScmObj** %stackaddr$prim60846, align 8
%clofunc60847 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47388)
musttail call tailcc void %clofunc60847(%struct.ScmObj* %anf_45bind47388, %struct.ScmObj* %argslist59485$anf_45bind473882)
ret void
}

define tailcc void @proc_clo$ae51738(%struct.ScmObj* %env$ae51738,%struct.ScmObj* %current_45args59477) {
%stackaddr$env-ref60848 = alloca %struct.ScmObj*, align 8
%k47451 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51738, i64 0)
store %struct.ScmObj* %k47451, %struct.ScmObj** %stackaddr$env-ref60848
%stackaddr$env-ref60849 = alloca %struct.ScmObj*, align 8
%anf_45bind47387 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51738, i64 1)
store %struct.ScmObj* %anf_45bind47387, %struct.ScmObj** %stackaddr$env-ref60849
%stackaddr$env-ref60850 = alloca %struct.ScmObj*, align 8
%c47205 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51738, i64 2)
store %struct.ScmObj* %c47205, %struct.ScmObj** %stackaddr$env-ref60850
%stackaddr$env-ref60851 = alloca %struct.ScmObj*, align 8
%anf_45bind47386 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51738, i64 3)
store %struct.ScmObj* %anf_45bind47386, %struct.ScmObj** %stackaddr$env-ref60851
%stackaddr$prim60852 = alloca %struct.ScmObj*, align 8
%_95k47452 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59477)
store volatile %struct.ScmObj* %_95k47452, %struct.ScmObj** %stackaddr$prim60852, align 8
%stackaddr$prim60853 = alloca %struct.ScmObj*, align 8
%current_45args59478 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59477)
store volatile %struct.ScmObj* %current_45args59478, %struct.ScmObj** %stackaddr$prim60853, align 8
%stackaddr$prim60854 = alloca %struct.ScmObj*, align 8
%anf_45bind47389 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59478)
store volatile %struct.ScmObj* %anf_45bind47389, %struct.ScmObj** %stackaddr$prim60854, align 8
%stackaddr$makeclosure60855 = alloca %struct.ScmObj*, align 8
%fptrToInt60856 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51741 to i64
%ae51741 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt60856)
store volatile %struct.ScmObj* %ae51741, %struct.ScmObj** %stackaddr$makeclosure60855, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51741, %struct.ScmObj* %c47205, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51741, %struct.ScmObj* %k47451, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51741, %struct.ScmObj* %anf_45bind47386, i64 2)
%argslist59484$anf_45bind473870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60857 = alloca %struct.ScmObj*, align 8
%argslist59484$anf_45bind473871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47389, %struct.ScmObj* %argslist59484$anf_45bind473870)
store volatile %struct.ScmObj* %argslist59484$anf_45bind473871, %struct.ScmObj** %stackaddr$prim60857, align 8
%stackaddr$prim60858 = alloca %struct.ScmObj*, align 8
%argslist59484$anf_45bind473872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51741, %struct.ScmObj* %argslist59484$anf_45bind473871)
store volatile %struct.ScmObj* %argslist59484$anf_45bind473872, %struct.ScmObj** %stackaddr$prim60858, align 8
%clofunc60859 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47387)
musttail call tailcc void %clofunc60859(%struct.ScmObj* %anf_45bind47387, %struct.ScmObj* %argslist59484$anf_45bind473872)
ret void
}

define tailcc void @proc_clo$ae51741(%struct.ScmObj* %env$ae51741,%struct.ScmObj* %current_45args59480) {
%stackaddr$env-ref60860 = alloca %struct.ScmObj*, align 8
%c47205 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51741, i64 0)
store %struct.ScmObj* %c47205, %struct.ScmObj** %stackaddr$env-ref60860
%stackaddr$env-ref60861 = alloca %struct.ScmObj*, align 8
%k47451 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51741, i64 1)
store %struct.ScmObj* %k47451, %struct.ScmObj** %stackaddr$env-ref60861
%stackaddr$env-ref60862 = alloca %struct.ScmObj*, align 8
%anf_45bind47386 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51741, i64 2)
store %struct.ScmObj* %anf_45bind47386, %struct.ScmObj** %stackaddr$env-ref60862
%stackaddr$prim60863 = alloca %struct.ScmObj*, align 8
%_95k47453 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59480)
store volatile %struct.ScmObj* %_95k47453, %struct.ScmObj** %stackaddr$prim60863, align 8
%stackaddr$prim60864 = alloca %struct.ScmObj*, align 8
%current_45args59481 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59480)
store volatile %struct.ScmObj* %current_45args59481, %struct.ScmObj** %stackaddr$prim60864, align 8
%stackaddr$prim60865 = alloca %struct.ScmObj*, align 8
%anf_45bind47390 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59481)
store volatile %struct.ScmObj* %anf_45bind47390, %struct.ScmObj** %stackaddr$prim60865, align 8
%argslist59483$anf_45bind473860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60866 = alloca %struct.ScmObj*, align 8
%argslist59483$anf_45bind473861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %c47205, %struct.ScmObj* %argslist59483$anf_45bind473860)
store volatile %struct.ScmObj* %argslist59483$anf_45bind473861, %struct.ScmObj** %stackaddr$prim60866, align 8
%stackaddr$prim60867 = alloca %struct.ScmObj*, align 8
%argslist59483$anf_45bind473862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47390, %struct.ScmObj* %argslist59483$anf_45bind473861)
store volatile %struct.ScmObj* %argslist59483$anf_45bind473862, %struct.ScmObj** %stackaddr$prim60867, align 8
%stackaddr$prim60868 = alloca %struct.ScmObj*, align 8
%argslist59483$anf_45bind473863 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47451, %struct.ScmObj* %argslist59483$anf_45bind473862)
store volatile %struct.ScmObj* %argslist59483$anf_45bind473863, %struct.ScmObj** %stackaddr$prim60868, align 8
%clofunc60869 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47386)
musttail call tailcc void %clofunc60869(%struct.ScmObj* %anf_45bind47386, %struct.ScmObj* %argslist59483$anf_45bind473863)
ret void
}

define tailcc void @proc_clo$ae51673(%struct.ScmObj* %env$ae51673,%struct.ScmObj* %current_45args59488) {
%stackaddr$env-ref60870 = alloca %struct.ScmObj*, align 8
%x47203 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51673, i64 0)
store %struct.ScmObj* %x47203, %struct.ScmObj** %stackaddr$env-ref60870
%stackaddr$env-ref60871 = alloca %struct.ScmObj*, align 8
%pred47197 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51673, i64 1)
store %struct.ScmObj* %pred47197, %struct.ScmObj** %stackaddr$env-ref60871
%stackaddr$env-ref60872 = alloca %struct.ScmObj*, align 8
%fibc47194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51673, i64 2)
store %struct.ScmObj* %fibc47194, %struct.ScmObj** %stackaddr$env-ref60872
%stackaddr$prim60873 = alloca %struct.ScmObj*, align 8
%k47454 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59488)
store volatile %struct.ScmObj* %k47454, %struct.ScmObj** %stackaddr$prim60873, align 8
%stackaddr$prim60874 = alloca %struct.ScmObj*, align 8
%current_45args59489 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59488)
store volatile %struct.ScmObj* %current_45args59489, %struct.ScmObj** %stackaddr$prim60874, align 8
%stackaddr$prim60875 = alloca %struct.ScmObj*, align 8
%c47204 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59489)
store volatile %struct.ScmObj* %c47204, %struct.ScmObj** %stackaddr$prim60875, align 8
%ae51675 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim60876 = alloca %struct.ScmObj*, align 8
%anf_45bind47381 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %fibc47194, %struct.ScmObj* %ae51675)
store volatile %struct.ScmObj* %anf_45bind47381, %struct.ScmObj** %stackaddr$prim60876, align 8
%ae51677 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim60877 = alloca %struct.ScmObj*, align 8
%anf_45bind47382 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %pred47197, %struct.ScmObj* %ae51677)
store volatile %struct.ScmObj* %anf_45bind47382, %struct.ScmObj** %stackaddr$prim60877, align 8
%stackaddr$makeclosure60878 = alloca %struct.ScmObj*, align 8
%fptrToInt60879 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51679 to i64
%ae51679 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt60879)
store volatile %struct.ScmObj* %ae51679, %struct.ScmObj** %stackaddr$makeclosure60878, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51679, %struct.ScmObj* %anf_45bind47381, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51679, %struct.ScmObj* %c47204, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51679, %struct.ScmObj* %k47454, i64 2)
%argslist59495$anf_45bind473820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60880 = alloca %struct.ScmObj*, align 8
%argslist59495$anf_45bind473821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x47203, %struct.ScmObj* %argslist59495$anf_45bind473820)
store volatile %struct.ScmObj* %argslist59495$anf_45bind473821, %struct.ScmObj** %stackaddr$prim60880, align 8
%stackaddr$prim60881 = alloca %struct.ScmObj*, align 8
%argslist59495$anf_45bind473822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51679, %struct.ScmObj* %argslist59495$anf_45bind473821)
store volatile %struct.ScmObj* %argslist59495$anf_45bind473822, %struct.ScmObj** %stackaddr$prim60881, align 8
%clofunc60882 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47382)
musttail call tailcc void %clofunc60882(%struct.ScmObj* %anf_45bind47382, %struct.ScmObj* %argslist59495$anf_45bind473822)
ret void
}

define tailcc void @proc_clo$ae51679(%struct.ScmObj* %env$ae51679,%struct.ScmObj* %current_45args59491) {
%stackaddr$env-ref60883 = alloca %struct.ScmObj*, align 8
%anf_45bind47381 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51679, i64 0)
store %struct.ScmObj* %anf_45bind47381, %struct.ScmObj** %stackaddr$env-ref60883
%stackaddr$env-ref60884 = alloca %struct.ScmObj*, align 8
%c47204 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51679, i64 1)
store %struct.ScmObj* %c47204, %struct.ScmObj** %stackaddr$env-ref60884
%stackaddr$env-ref60885 = alloca %struct.ScmObj*, align 8
%k47454 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51679, i64 2)
store %struct.ScmObj* %k47454, %struct.ScmObj** %stackaddr$env-ref60885
%stackaddr$prim60886 = alloca %struct.ScmObj*, align 8
%_95k47455 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59491)
store volatile %struct.ScmObj* %_95k47455, %struct.ScmObj** %stackaddr$prim60886, align 8
%stackaddr$prim60887 = alloca %struct.ScmObj*, align 8
%current_45args59492 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59491)
store volatile %struct.ScmObj* %current_45args59492, %struct.ScmObj** %stackaddr$prim60887, align 8
%stackaddr$prim60888 = alloca %struct.ScmObj*, align 8
%anf_45bind47383 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59492)
store volatile %struct.ScmObj* %anf_45bind47383, %struct.ScmObj** %stackaddr$prim60888, align 8
%argslist59494$anf_45bind473810 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60889 = alloca %struct.ScmObj*, align 8
%argslist59494$anf_45bind473811 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %c47204, %struct.ScmObj* %argslist59494$anf_45bind473810)
store volatile %struct.ScmObj* %argslist59494$anf_45bind473811, %struct.ScmObj** %stackaddr$prim60889, align 8
%stackaddr$prim60890 = alloca %struct.ScmObj*, align 8
%argslist59494$anf_45bind473812 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47383, %struct.ScmObj* %argslist59494$anf_45bind473811)
store volatile %struct.ScmObj* %argslist59494$anf_45bind473812, %struct.ScmObj** %stackaddr$prim60890, align 8
%stackaddr$prim60891 = alloca %struct.ScmObj*, align 8
%argslist59494$anf_45bind473813 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47454, %struct.ScmObj* %argslist59494$anf_45bind473812)
store volatile %struct.ScmObj* %argslist59494$anf_45bind473813, %struct.ScmObj** %stackaddr$prim60891, align 8
%clofunc60892 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47381)
musttail call tailcc void %clofunc60892(%struct.ScmObj* %anf_45bind47381, %struct.ScmObj* %argslist59494$anf_45bind473813)
ret void
}

define tailcc void @proc_clo$ae51466(%struct.ScmObj* %env$ae51466,%struct.ScmObj* %current_45args59501) {
%stackaddr$env-ref60893 = alloca %struct.ScmObj*, align 8
%succ47198 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51466, i64 0)
store %struct.ScmObj* %succ47198, %struct.ScmObj** %stackaddr$env-ref60893
%stackaddr$env-ref60894 = alloca %struct.ScmObj*, align 8
%pred47197 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51466, i64 1)
store %struct.ScmObj* %pred47197, %struct.ScmObj** %stackaddr$env-ref60894
%stackaddr$env-ref60895 = alloca %struct.ScmObj*, align 8
%z_6347196 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51466, i64 2)
store %struct.ScmObj* %z_6347196, %struct.ScmObj** %stackaddr$env-ref60895
%stackaddr$env-ref60896 = alloca %struct.ScmObj*, align 8
%addc47195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51466, i64 3)
store %struct.ScmObj* %addc47195, %struct.ScmObj** %stackaddr$env-ref60896
%stackaddr$prim60897 = alloca %struct.ScmObj*, align 8
%k47456 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59501)
store volatile %struct.ScmObj* %k47456, %struct.ScmObj** %stackaddr$prim60897, align 8
%stackaddr$prim60898 = alloca %struct.ScmObj*, align 8
%current_45args59502 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59501)
store volatile %struct.ScmObj* %current_45args59502, %struct.ScmObj** %stackaddr$prim60898, align 8
%stackaddr$prim60899 = alloca %struct.ScmObj*, align 8
%x47209 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59502)
store volatile %struct.ScmObj* %x47209, %struct.ScmObj** %stackaddr$prim60899, align 8
%stackaddr$prim60900 = alloca %struct.ScmObj*, align 8
%current_45args59503 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59502)
store volatile %struct.ScmObj* %current_45args59503, %struct.ScmObj** %stackaddr$prim60900, align 8
%stackaddr$prim60901 = alloca %struct.ScmObj*, align 8
%y47208 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59503)
store volatile %struct.ScmObj* %y47208, %struct.ScmObj** %stackaddr$prim60901, align 8
%stackaddr$prim60902 = alloca %struct.ScmObj*, align 8
%current_45args59504 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59503)
store volatile %struct.ScmObj* %current_45args59504, %struct.ScmObj** %stackaddr$prim60902, align 8
%stackaddr$prim60903 = alloca %struct.ScmObj*, align 8
%k47207 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59504)
store volatile %struct.ScmObj* %k47207, %struct.ScmObj** %stackaddr$prim60903, align 8
%ae51468 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim60904 = alloca %struct.ScmObj*, align 8
%anf_45bind47362 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %z_6347196, %struct.ScmObj* %ae51468)
store volatile %struct.ScmObj* %anf_45bind47362, %struct.ScmObj** %stackaddr$prim60904, align 8
%stackaddr$makeclosure60905 = alloca %struct.ScmObj*, align 8
%fptrToInt60906 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51470 to i64
%ae51470 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt60906)
store volatile %struct.ScmObj* %ae51470, %struct.ScmObj** %stackaddr$makeclosure60905, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51470, %struct.ScmObj* %k47456, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51470, %struct.ScmObj* %succ47198, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51470, %struct.ScmObj* %pred47197, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51470, %struct.ScmObj* %addc47195, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51470, %struct.ScmObj* %x47209, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51470, %struct.ScmObj* %y47208, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae51470, %struct.ScmObj* %k47207, i64 6)
%argslist59519$anf_45bind473620 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60907 = alloca %struct.ScmObj*, align 8
%argslist59519$anf_45bind473621 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y47208, %struct.ScmObj* %argslist59519$anf_45bind473620)
store volatile %struct.ScmObj* %argslist59519$anf_45bind473621, %struct.ScmObj** %stackaddr$prim60907, align 8
%stackaddr$prim60908 = alloca %struct.ScmObj*, align 8
%argslist59519$anf_45bind473622 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51470, %struct.ScmObj* %argslist59519$anf_45bind473621)
store volatile %struct.ScmObj* %argslist59519$anf_45bind473622, %struct.ScmObj** %stackaddr$prim60908, align 8
%clofunc60909 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47362)
musttail call tailcc void %clofunc60909(%struct.ScmObj* %anf_45bind47362, %struct.ScmObj* %argslist59519$anf_45bind473622)
ret void
}

define tailcc void @proc_clo$ae51470(%struct.ScmObj* %env$ae51470,%struct.ScmObj* %current_45args59506) {
%stackaddr$env-ref60910 = alloca %struct.ScmObj*, align 8
%k47456 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51470, i64 0)
store %struct.ScmObj* %k47456, %struct.ScmObj** %stackaddr$env-ref60910
%stackaddr$env-ref60911 = alloca %struct.ScmObj*, align 8
%succ47198 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51470, i64 1)
store %struct.ScmObj* %succ47198, %struct.ScmObj** %stackaddr$env-ref60911
%stackaddr$env-ref60912 = alloca %struct.ScmObj*, align 8
%pred47197 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51470, i64 2)
store %struct.ScmObj* %pred47197, %struct.ScmObj** %stackaddr$env-ref60912
%stackaddr$env-ref60913 = alloca %struct.ScmObj*, align 8
%addc47195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51470, i64 3)
store %struct.ScmObj* %addc47195, %struct.ScmObj** %stackaddr$env-ref60913
%stackaddr$env-ref60914 = alloca %struct.ScmObj*, align 8
%x47209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51470, i64 4)
store %struct.ScmObj* %x47209, %struct.ScmObj** %stackaddr$env-ref60914
%stackaddr$env-ref60915 = alloca %struct.ScmObj*, align 8
%y47208 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51470, i64 5)
store %struct.ScmObj* %y47208, %struct.ScmObj** %stackaddr$env-ref60915
%stackaddr$env-ref60916 = alloca %struct.ScmObj*, align 8
%k47207 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51470, i64 6)
store %struct.ScmObj* %k47207, %struct.ScmObj** %stackaddr$env-ref60916
%stackaddr$prim60917 = alloca %struct.ScmObj*, align 8
%_95k47457 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59506)
store volatile %struct.ScmObj* %_95k47457, %struct.ScmObj** %stackaddr$prim60917, align 8
%stackaddr$prim60918 = alloca %struct.ScmObj*, align 8
%current_45args59507 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59506)
store volatile %struct.ScmObj* %current_45args59507, %struct.ScmObj** %stackaddr$prim60918, align 8
%stackaddr$prim60919 = alloca %struct.ScmObj*, align 8
%anf_45bind47363 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59507)
store volatile %struct.ScmObj* %anf_45bind47363, %struct.ScmObj** %stackaddr$prim60919, align 8
%truthy$cmp60920 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47363)
%cmp$cmp60920 = icmp eq i64 %truthy$cmp60920, 1
br i1 %cmp$cmp60920, label %truebranch$cmp60920, label %falsebranch$cmp60920
truebranch$cmp60920:
%argslist59509$k472070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60921 = alloca %struct.ScmObj*, align 8
%argslist59509$k472071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x47209, %struct.ScmObj* %argslist59509$k472070)
store volatile %struct.ScmObj* %argslist59509$k472071, %struct.ScmObj** %stackaddr$prim60921, align 8
%stackaddr$prim60922 = alloca %struct.ScmObj*, align 8
%argslist59509$k472072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47456, %struct.ScmObj* %argslist59509$k472071)
store volatile %struct.ScmObj* %argslist59509$k472072, %struct.ScmObj** %stackaddr$prim60922, align 8
%clofunc60923 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47207)
musttail call tailcc void %clofunc60923(%struct.ScmObj* %k47207, %struct.ScmObj* %argslist59509$k472072)
ret void
falsebranch$cmp60920:
%ae51477 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim60924 = alloca %struct.ScmObj*, align 8
%anf_45bind47364 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %addc47195, %struct.ScmObj* %ae51477)
store volatile %struct.ScmObj* %anf_45bind47364, %struct.ScmObj** %stackaddr$prim60924, align 8
%ae51479 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim60925 = alloca %struct.ScmObj*, align 8
%anf_45bind47365 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %succ47198, %struct.ScmObj* %ae51479)
store volatile %struct.ScmObj* %anf_45bind47365, %struct.ScmObj** %stackaddr$prim60925, align 8
%stackaddr$makeclosure60926 = alloca %struct.ScmObj*, align 8
%fptrToInt60927 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51481 to i64
%ae51481 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt60927)
store volatile %struct.ScmObj* %ae51481, %struct.ScmObj** %stackaddr$makeclosure60926, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51481, %struct.ScmObj* %anf_45bind47364, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51481, %struct.ScmObj* %k47456, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51481, %struct.ScmObj* %pred47197, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51481, %struct.ScmObj* %y47208, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51481, %struct.ScmObj* %k47207, i64 4)
%argslist59518$anf_45bind473650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60928 = alloca %struct.ScmObj*, align 8
%argslist59518$anf_45bind473651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x47209, %struct.ScmObj* %argslist59518$anf_45bind473650)
store volatile %struct.ScmObj* %argslist59518$anf_45bind473651, %struct.ScmObj** %stackaddr$prim60928, align 8
%stackaddr$prim60929 = alloca %struct.ScmObj*, align 8
%argslist59518$anf_45bind473652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51481, %struct.ScmObj* %argslist59518$anf_45bind473651)
store volatile %struct.ScmObj* %argslist59518$anf_45bind473652, %struct.ScmObj** %stackaddr$prim60929, align 8
%clofunc60930 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47365)
musttail call tailcc void %clofunc60930(%struct.ScmObj* %anf_45bind47365, %struct.ScmObj* %argslist59518$anf_45bind473652)
ret void
}

define tailcc void @proc_clo$ae51481(%struct.ScmObj* %env$ae51481,%struct.ScmObj* %current_45args59510) {
%stackaddr$env-ref60931 = alloca %struct.ScmObj*, align 8
%anf_45bind47364 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51481, i64 0)
store %struct.ScmObj* %anf_45bind47364, %struct.ScmObj** %stackaddr$env-ref60931
%stackaddr$env-ref60932 = alloca %struct.ScmObj*, align 8
%k47456 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51481, i64 1)
store %struct.ScmObj* %k47456, %struct.ScmObj** %stackaddr$env-ref60932
%stackaddr$env-ref60933 = alloca %struct.ScmObj*, align 8
%pred47197 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51481, i64 2)
store %struct.ScmObj* %pred47197, %struct.ScmObj** %stackaddr$env-ref60933
%stackaddr$env-ref60934 = alloca %struct.ScmObj*, align 8
%y47208 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51481, i64 3)
store %struct.ScmObj* %y47208, %struct.ScmObj** %stackaddr$env-ref60934
%stackaddr$env-ref60935 = alloca %struct.ScmObj*, align 8
%k47207 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51481, i64 4)
store %struct.ScmObj* %k47207, %struct.ScmObj** %stackaddr$env-ref60935
%stackaddr$prim60936 = alloca %struct.ScmObj*, align 8
%_95k47458 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59510)
store volatile %struct.ScmObj* %_95k47458, %struct.ScmObj** %stackaddr$prim60936, align 8
%stackaddr$prim60937 = alloca %struct.ScmObj*, align 8
%current_45args59511 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59510)
store volatile %struct.ScmObj* %current_45args59511, %struct.ScmObj** %stackaddr$prim60937, align 8
%stackaddr$prim60938 = alloca %struct.ScmObj*, align 8
%anf_45bind47366 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59511)
store volatile %struct.ScmObj* %anf_45bind47366, %struct.ScmObj** %stackaddr$prim60938, align 8
%ae51484 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim60939 = alloca %struct.ScmObj*, align 8
%anf_45bind47367 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %pred47197, %struct.ScmObj* %ae51484)
store volatile %struct.ScmObj* %anf_45bind47367, %struct.ScmObj** %stackaddr$prim60939, align 8
%stackaddr$makeclosure60940 = alloca %struct.ScmObj*, align 8
%fptrToInt60941 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51486 to i64
%ae51486 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt60941)
store volatile %struct.ScmObj* %ae51486, %struct.ScmObj** %stackaddr$makeclosure60940, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51486, %struct.ScmObj* %anf_45bind47366, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51486, %struct.ScmObj* %anf_45bind47364, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51486, %struct.ScmObj* %k47456, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51486, %struct.ScmObj* %k47207, i64 3)
%argslist59517$anf_45bind473670 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60942 = alloca %struct.ScmObj*, align 8
%argslist59517$anf_45bind473671 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y47208, %struct.ScmObj* %argslist59517$anf_45bind473670)
store volatile %struct.ScmObj* %argslist59517$anf_45bind473671, %struct.ScmObj** %stackaddr$prim60942, align 8
%stackaddr$prim60943 = alloca %struct.ScmObj*, align 8
%argslist59517$anf_45bind473672 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51486, %struct.ScmObj* %argslist59517$anf_45bind473671)
store volatile %struct.ScmObj* %argslist59517$anf_45bind473672, %struct.ScmObj** %stackaddr$prim60943, align 8
%clofunc60944 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47367)
musttail call tailcc void %clofunc60944(%struct.ScmObj* %anf_45bind47367, %struct.ScmObj* %argslist59517$anf_45bind473672)
ret void
}

define tailcc void @proc_clo$ae51486(%struct.ScmObj* %env$ae51486,%struct.ScmObj* %current_45args59513) {
%stackaddr$env-ref60945 = alloca %struct.ScmObj*, align 8
%anf_45bind47366 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51486, i64 0)
store %struct.ScmObj* %anf_45bind47366, %struct.ScmObj** %stackaddr$env-ref60945
%stackaddr$env-ref60946 = alloca %struct.ScmObj*, align 8
%anf_45bind47364 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51486, i64 1)
store %struct.ScmObj* %anf_45bind47364, %struct.ScmObj** %stackaddr$env-ref60946
%stackaddr$env-ref60947 = alloca %struct.ScmObj*, align 8
%k47456 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51486, i64 2)
store %struct.ScmObj* %k47456, %struct.ScmObj** %stackaddr$env-ref60947
%stackaddr$env-ref60948 = alloca %struct.ScmObj*, align 8
%k47207 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51486, i64 3)
store %struct.ScmObj* %k47207, %struct.ScmObj** %stackaddr$env-ref60948
%stackaddr$prim60949 = alloca %struct.ScmObj*, align 8
%_95k47459 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59513)
store volatile %struct.ScmObj* %_95k47459, %struct.ScmObj** %stackaddr$prim60949, align 8
%stackaddr$prim60950 = alloca %struct.ScmObj*, align 8
%current_45args59514 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59513)
store volatile %struct.ScmObj* %current_45args59514, %struct.ScmObj** %stackaddr$prim60950, align 8
%stackaddr$prim60951 = alloca %struct.ScmObj*, align 8
%anf_45bind47368 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59514)
store volatile %struct.ScmObj* %anf_45bind47368, %struct.ScmObj** %stackaddr$prim60951, align 8
%argslist59516$anf_45bind473640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60952 = alloca %struct.ScmObj*, align 8
%argslist59516$anf_45bind473641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47207, %struct.ScmObj* %argslist59516$anf_45bind473640)
store volatile %struct.ScmObj* %argslist59516$anf_45bind473641, %struct.ScmObj** %stackaddr$prim60952, align 8
%stackaddr$prim60953 = alloca %struct.ScmObj*, align 8
%argslist59516$anf_45bind473642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47368, %struct.ScmObj* %argslist59516$anf_45bind473641)
store volatile %struct.ScmObj* %argslist59516$anf_45bind473642, %struct.ScmObj** %stackaddr$prim60953, align 8
%stackaddr$prim60954 = alloca %struct.ScmObj*, align 8
%argslist59516$anf_45bind473643 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47366, %struct.ScmObj* %argslist59516$anf_45bind473642)
store volatile %struct.ScmObj* %argslist59516$anf_45bind473643, %struct.ScmObj** %stackaddr$prim60954, align 8
%stackaddr$prim60955 = alloca %struct.ScmObj*, align 8
%argslist59516$anf_45bind473644 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47456, %struct.ScmObj* %argslist59516$anf_45bind473643)
store volatile %struct.ScmObj* %argslist59516$anf_45bind473644, %struct.ScmObj** %stackaddr$prim60955, align 8
%clofunc60956 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47364)
musttail call tailcc void %clofunc60956(%struct.ScmObj* %anf_45bind47364, %struct.ScmObj* %argslist59516$anf_45bind473644)
ret void
}

define tailcc void @proc_clo$ae51443(%struct.ScmObj* %env$ae51443,%struct.ScmObj* %current_45args59521) {
%stackaddr$prim60957 = alloca %struct.ScmObj*, align 8
%k47460 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59521)
store volatile %struct.ScmObj* %k47460, %struct.ScmObj** %stackaddr$prim60957, align 8
%stackaddr$prim60958 = alloca %struct.ScmObj*, align 8
%current_45args59522 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59521)
store volatile %struct.ScmObj* %current_45args59522, %struct.ScmObj** %stackaddr$prim60958, align 8
%stackaddr$prim60959 = alloca %struct.ScmObj*, align 8
%n47211 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59522)
store volatile %struct.ScmObj* %n47211, %struct.ScmObj** %stackaddr$prim60959, align 8
%stackaddr$prim60960 = alloca %struct.ScmObj*, align 8
%cpsprim47461 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %n47211)
store volatile %struct.ScmObj* %cpsprim47461, %struct.ScmObj** %stackaddr$prim60960, align 8
%ae51446 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist59524$k474600 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60961 = alloca %struct.ScmObj*, align 8
%argslist59524$k474601 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47461, %struct.ScmObj* %argslist59524$k474600)
store volatile %struct.ScmObj* %argslist59524$k474601, %struct.ScmObj** %stackaddr$prim60961, align 8
%stackaddr$prim60962 = alloca %struct.ScmObj*, align 8
%argslist59524$k474602 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51446, %struct.ScmObj* %argslist59524$k474601)
store volatile %struct.ScmObj* %argslist59524$k474602, %struct.ScmObj** %stackaddr$prim60962, align 8
%clofunc60963 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47460)
musttail call tailcc void %clofunc60963(%struct.ScmObj* %k47460, %struct.ScmObj* %argslist59524$k474602)
ret void
}

define tailcc void @proc_clo$ae51420(%struct.ScmObj* %env$ae51420,%struct.ScmObj* %current_45args59526) {
%stackaddr$prim60964 = alloca %struct.ScmObj*, align 8
%k47462 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59526)
store volatile %struct.ScmObj* %k47462, %struct.ScmObj** %stackaddr$prim60964, align 8
%stackaddr$prim60965 = alloca %struct.ScmObj*, align 8
%current_45args59527 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59526)
store volatile %struct.ScmObj* %current_45args59527, %struct.ScmObj** %stackaddr$prim60965, align 8
%stackaddr$prim60966 = alloca %struct.ScmObj*, align 8
%x4707247213 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59527)
store volatile %struct.ScmObj* %x4707247213, %struct.ScmObj** %stackaddr$prim60966, align 8
%stackaddr$prim60967 = alloca %struct.ScmObj*, align 8
%cpsprim47463 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x4707247213)
store volatile %struct.ScmObj* %cpsprim47463, %struct.ScmObj** %stackaddr$prim60967, align 8
%ae51423 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist59529$k474620 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60968 = alloca %struct.ScmObj*, align 8
%argslist59529$k474621 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47463, %struct.ScmObj* %argslist59529$k474620)
store volatile %struct.ScmObj* %argslist59529$k474621, %struct.ScmObj** %stackaddr$prim60968, align 8
%stackaddr$prim60969 = alloca %struct.ScmObj*, align 8
%argslist59529$k474622 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51423, %struct.ScmObj* %argslist59529$k474621)
store volatile %struct.ScmObj* %argslist59529$k474622, %struct.ScmObj** %stackaddr$prim60969, align 8
%clofunc60970 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47462)
musttail call tailcc void %clofunc60970(%struct.ScmObj* %k47462, %struct.ScmObj* %argslist59529$k474622)
ret void
}

define tailcc void @proc_clo$ae51351(%struct.ScmObj* %env$ae51351,%struct.ScmObj* %current_45args59531) {
%stackaddr$prim60971 = alloca %struct.ScmObj*, align 8
%k47464 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59531)
store volatile %struct.ScmObj* %k47464, %struct.ScmObj** %stackaddr$prim60971, align 8
%stackaddr$prim60972 = alloca %struct.ScmObj*, align 8
%current_45args59532 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59531)
store volatile %struct.ScmObj* %current_45args59532, %struct.ScmObj** %stackaddr$prim60972, align 8
%stackaddr$prim60973 = alloca %struct.ScmObj*, align 8
%n47215 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59532)
store volatile %struct.ScmObj* %n47215, %struct.ScmObj** %stackaddr$prim60973, align 8
%stackaddr$makeclosure60974 = alloca %struct.ScmObj*, align 8
%fptrToInt60975 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51352 to i64
%ae51352 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt60975)
store volatile %struct.ScmObj* %ae51352, %struct.ScmObj** %stackaddr$makeclosure60974, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51352, %struct.ScmObj* %n47215, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51352, %struct.ScmObj* %k47464, i64 1)
%ae51353 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure60976 = alloca %struct.ScmObj*, align 8
%fptrToInt60977 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51354 to i64
%ae51354 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt60977)
store volatile %struct.ScmObj* %ae51354, %struct.ScmObj** %stackaddr$makeclosure60976, align 8
%argslist59543$ae513520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60978 = alloca %struct.ScmObj*, align 8
%argslist59543$ae513521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51354, %struct.ScmObj* %argslist59543$ae513520)
store volatile %struct.ScmObj* %argslist59543$ae513521, %struct.ScmObj** %stackaddr$prim60978, align 8
%stackaddr$prim60979 = alloca %struct.ScmObj*, align 8
%argslist59543$ae513522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51353, %struct.ScmObj* %argslist59543$ae513521)
store volatile %struct.ScmObj* %argslist59543$ae513522, %struct.ScmObj** %stackaddr$prim60979, align 8
%clofunc60980 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51352)
musttail call tailcc void %clofunc60980(%struct.ScmObj* %ae51352, %struct.ScmObj* %argslist59543$ae513522)
ret void
}

define tailcc void @proc_clo$ae51352(%struct.ScmObj* %env$ae51352,%struct.ScmObj* %current_45args59534) {
%stackaddr$env-ref60981 = alloca %struct.ScmObj*, align 8
%n47215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51352, i64 0)
store %struct.ScmObj* %n47215, %struct.ScmObj** %stackaddr$env-ref60981
%stackaddr$env-ref60982 = alloca %struct.ScmObj*, align 8
%k47464 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51352, i64 1)
store %struct.ScmObj* %k47464, %struct.ScmObj** %stackaddr$env-ref60982
%stackaddr$prim60983 = alloca %struct.ScmObj*, align 8
%_95k47465 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59534)
store volatile %struct.ScmObj* %_95k47465, %struct.ScmObj** %stackaddr$prim60983, align 8
%stackaddr$prim60984 = alloca %struct.ScmObj*, align 8
%current_45args59535 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59534)
store volatile %struct.ScmObj* %current_45args59535, %struct.ScmObj** %stackaddr$prim60984, align 8
%stackaddr$prim60985 = alloca %struct.ScmObj*, align 8
%anf_45bind47357 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59535)
store volatile %struct.ScmObj* %anf_45bind47357, %struct.ScmObj** %stackaddr$prim60985, align 8
%stackaddr$makeclosure60986 = alloca %struct.ScmObj*, align 8
%fptrToInt60987 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51375 to i64
%ae51375 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt60987)
store volatile %struct.ScmObj* %ae51375, %struct.ScmObj** %stackaddr$makeclosure60986, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51375, %struct.ScmObj* %n47215, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51375, %struct.ScmObj* %k47464, i64 1)
%argslist59541$anf_45bind473570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60988 = alloca %struct.ScmObj*, align 8
%argslist59541$anf_45bind473571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51375, %struct.ScmObj* %argslist59541$anf_45bind473570)
store volatile %struct.ScmObj* %argslist59541$anf_45bind473571, %struct.ScmObj** %stackaddr$prim60988, align 8
%clofunc60989 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47357)
musttail call tailcc void %clofunc60989(%struct.ScmObj* %anf_45bind47357, %struct.ScmObj* %argslist59541$anf_45bind473571)
ret void
}

define tailcc void @proc_clo$ae51375(%struct.ScmObj* %env$ae51375,%struct.ScmObj* %current_45args59537) {
%stackaddr$env-ref60990 = alloca %struct.ScmObj*, align 8
%n47215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51375, i64 0)
store %struct.ScmObj* %n47215, %struct.ScmObj** %stackaddr$env-ref60990
%stackaddr$env-ref60991 = alloca %struct.ScmObj*, align 8
%k47464 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51375, i64 1)
store %struct.ScmObj* %k47464, %struct.ScmObj** %stackaddr$env-ref60991
%stackaddr$prim60992 = alloca %struct.ScmObj*, align 8
%_95k47466 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59537)
store volatile %struct.ScmObj* %_95k47466, %struct.ScmObj** %stackaddr$prim60992, align 8
%stackaddr$prim60993 = alloca %struct.ScmObj*, align 8
%current_45args59538 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59537)
store volatile %struct.ScmObj* %current_45args59538, %struct.ScmObj** %stackaddr$prim60993, align 8
%stackaddr$prim60994 = alloca %struct.ScmObj*, align 8
%anf_45bind47358 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59538)
store volatile %struct.ScmObj* %anf_45bind47358, %struct.ScmObj** %stackaddr$prim60994, align 8
%stackaddr$prim60995 = alloca %struct.ScmObj*, align 8
%cpsprim47467 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %n47215, %struct.ScmObj* %anf_45bind47358)
store volatile %struct.ScmObj* %cpsprim47467, %struct.ScmObj** %stackaddr$prim60995, align 8
%ae51379 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist59540$k474640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60996 = alloca %struct.ScmObj*, align 8
%argslist59540$k474641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47467, %struct.ScmObj* %argslist59540$k474640)
store volatile %struct.ScmObj* %argslist59540$k474641, %struct.ScmObj** %stackaddr$prim60996, align 8
%stackaddr$prim60997 = alloca %struct.ScmObj*, align 8
%argslist59540$k474642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51379, %struct.ScmObj* %argslist59540$k474641)
store volatile %struct.ScmObj* %argslist59540$k474642, %struct.ScmObj** %stackaddr$prim60997, align 8
%clofunc60998 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47464)
musttail call tailcc void %clofunc60998(%struct.ScmObj* %k47464, %struct.ScmObj* %argslist59540$k474642)
ret void
}

define tailcc void @proc_clo$ae51354(%struct.ScmObj* %env$ae51354,%struct.ScmObj* %lst4721647468) {
%stackaddr$prim60999 = alloca %struct.ScmObj*, align 8
%k47469 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4721647468)
store volatile %struct.ScmObj* %k47469, %struct.ScmObj** %stackaddr$prim60999, align 8
%stackaddr$prim61000 = alloca %struct.ScmObj*, align 8
%lst47216 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4721647468)
store volatile %struct.ScmObj* %lst47216, %struct.ScmObj** %stackaddr$prim61000, align 8
%ae51358 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist59542$k474690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61001 = alloca %struct.ScmObj*, align 8
%argslist59542$k474691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47216, %struct.ScmObj* %argslist59542$k474690)
store volatile %struct.ScmObj* %argslist59542$k474691, %struct.ScmObj** %stackaddr$prim61001, align 8
%stackaddr$prim61002 = alloca %struct.ScmObj*, align 8
%argslist59542$k474692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51358, %struct.ScmObj* %argslist59542$k474691)
store volatile %struct.ScmObj* %argslist59542$k474692, %struct.ScmObj** %stackaddr$prim61002, align 8
%clofunc61003 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47469)
musttail call tailcc void %clofunc61003(%struct.ScmObj* %k47469, %struct.ScmObj* %argslist59542$k474692)
ret void
}

define tailcc void @proc_clo$ae51216(%struct.ScmObj* %env$ae51216,%struct.ScmObj* %current_45args59545) {
%stackaddr$env-ref61004 = alloca %struct.ScmObj*, align 8
%peano_45_62nat47199 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51216, i64 0)
store %struct.ScmObj* %peano_45_62nat47199, %struct.ScmObj** %stackaddr$env-ref61004
%stackaddr$env-ref61005 = alloca %struct.ScmObj*, align 8
%pred47197 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51216, i64 1)
store %struct.ScmObj* %pred47197, %struct.ScmObj** %stackaddr$env-ref61005
%stackaddr$env-ref61006 = alloca %struct.ScmObj*, align 8
%z_6347196 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51216, i64 2)
store %struct.ScmObj* %z_6347196, %struct.ScmObj** %stackaddr$env-ref61006
%stackaddr$prim61007 = alloca %struct.ScmObj*, align 8
%k47470 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59545)
store volatile %struct.ScmObj* %k47470, %struct.ScmObj** %stackaddr$prim61007, align 8
%stackaddr$prim61008 = alloca %struct.ScmObj*, align 8
%current_45args59546 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59545)
store volatile %struct.ScmObj* %current_45args59546, %struct.ScmObj** %stackaddr$prim61008, align 8
%stackaddr$prim61009 = alloca %struct.ScmObj*, align 8
%n47218 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59546)
store volatile %struct.ScmObj* %n47218, %struct.ScmObj** %stackaddr$prim61009, align 8
%ae51218 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61010 = alloca %struct.ScmObj*, align 8
%anf_45bind47350 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %z_6347196, %struct.ScmObj* %ae51218)
store volatile %struct.ScmObj* %anf_45bind47350, %struct.ScmObj** %stackaddr$prim61010, align 8
%stackaddr$makeclosure61011 = alloca %struct.ScmObj*, align 8
%fptrToInt61012 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51220 to i64
%ae51220 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt61012)
store volatile %struct.ScmObj* %ae51220, %struct.ScmObj** %stackaddr$makeclosure61011, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51220, %struct.ScmObj* %n47218, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51220, %struct.ScmObj* %peano_45_62nat47199, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51220, %struct.ScmObj* %k47470, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51220, %struct.ScmObj* %pred47197, i64 3)
%argslist59561$anf_45bind473500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61013 = alloca %struct.ScmObj*, align 8
%argslist59561$anf_45bind473501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %n47218, %struct.ScmObj* %argslist59561$anf_45bind473500)
store volatile %struct.ScmObj* %argslist59561$anf_45bind473501, %struct.ScmObj** %stackaddr$prim61013, align 8
%stackaddr$prim61014 = alloca %struct.ScmObj*, align 8
%argslist59561$anf_45bind473502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51220, %struct.ScmObj* %argslist59561$anf_45bind473501)
store volatile %struct.ScmObj* %argslist59561$anf_45bind473502, %struct.ScmObj** %stackaddr$prim61014, align 8
%clofunc61015 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47350)
musttail call tailcc void %clofunc61015(%struct.ScmObj* %anf_45bind47350, %struct.ScmObj* %argslist59561$anf_45bind473502)
ret void
}

define tailcc void @proc_clo$ae51220(%struct.ScmObj* %env$ae51220,%struct.ScmObj* %current_45args59548) {
%stackaddr$env-ref61016 = alloca %struct.ScmObj*, align 8
%n47218 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51220, i64 0)
store %struct.ScmObj* %n47218, %struct.ScmObj** %stackaddr$env-ref61016
%stackaddr$env-ref61017 = alloca %struct.ScmObj*, align 8
%peano_45_62nat47199 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51220, i64 1)
store %struct.ScmObj* %peano_45_62nat47199, %struct.ScmObj** %stackaddr$env-ref61017
%stackaddr$env-ref61018 = alloca %struct.ScmObj*, align 8
%k47470 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51220, i64 2)
store %struct.ScmObj* %k47470, %struct.ScmObj** %stackaddr$env-ref61018
%stackaddr$env-ref61019 = alloca %struct.ScmObj*, align 8
%pred47197 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51220, i64 3)
store %struct.ScmObj* %pred47197, %struct.ScmObj** %stackaddr$env-ref61019
%stackaddr$prim61020 = alloca %struct.ScmObj*, align 8
%_95k47471 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59548)
store volatile %struct.ScmObj* %_95k47471, %struct.ScmObj** %stackaddr$prim61020, align 8
%stackaddr$prim61021 = alloca %struct.ScmObj*, align 8
%current_45args59549 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59548)
store volatile %struct.ScmObj* %current_45args59549, %struct.ScmObj** %stackaddr$prim61021, align 8
%stackaddr$prim61022 = alloca %struct.ScmObj*, align 8
%anf_45bind47351 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59549)
store volatile %struct.ScmObj* %anf_45bind47351, %struct.ScmObj** %stackaddr$prim61022, align 8
%truthy$cmp61023 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47351)
%cmp$cmp61023 = icmp eq i64 %truthy$cmp61023, 1
br i1 %cmp$cmp61023, label %truebranch$cmp61023, label %falsebranch$cmp61023
truebranch$cmp61023:
%ae51224 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51225 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist59551$k474700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61024 = alloca %struct.ScmObj*, align 8
%argslist59551$k474701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51225, %struct.ScmObj* %argslist59551$k474700)
store volatile %struct.ScmObj* %argslist59551$k474701, %struct.ScmObj** %stackaddr$prim61024, align 8
%stackaddr$prim61025 = alloca %struct.ScmObj*, align 8
%argslist59551$k474702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51224, %struct.ScmObj* %argslist59551$k474701)
store volatile %struct.ScmObj* %argslist59551$k474702, %struct.ScmObj** %stackaddr$prim61025, align 8
%clofunc61026 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47470)
musttail call tailcc void %clofunc61026(%struct.ScmObj* %k47470, %struct.ScmObj* %argslist59551$k474702)
ret void
falsebranch$cmp61023:
%ae51233 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61027 = alloca %struct.ScmObj*, align 8
%anf_45bind47352 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %peano_45_62nat47199, %struct.ScmObj* %ae51233)
store volatile %struct.ScmObj* %anf_45bind47352, %struct.ScmObj** %stackaddr$prim61027, align 8
%ae51235 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61028 = alloca %struct.ScmObj*, align 8
%anf_45bind47353 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %pred47197, %struct.ScmObj* %ae51235)
store volatile %struct.ScmObj* %anf_45bind47353, %struct.ScmObj** %stackaddr$prim61028, align 8
%stackaddr$makeclosure61029 = alloca %struct.ScmObj*, align 8
%fptrToInt61030 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51237 to i64
%ae51237 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt61030)
store volatile %struct.ScmObj* %ae51237, %struct.ScmObj** %stackaddr$makeclosure61029, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51237, %struct.ScmObj* %k47470, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51237, %struct.ScmObj* %anf_45bind47352, i64 1)
%argslist59560$anf_45bind473530 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61031 = alloca %struct.ScmObj*, align 8
%argslist59560$anf_45bind473531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %n47218, %struct.ScmObj* %argslist59560$anf_45bind473530)
store volatile %struct.ScmObj* %argslist59560$anf_45bind473531, %struct.ScmObj** %stackaddr$prim61031, align 8
%stackaddr$prim61032 = alloca %struct.ScmObj*, align 8
%argslist59560$anf_45bind473532 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51237, %struct.ScmObj* %argslist59560$anf_45bind473531)
store volatile %struct.ScmObj* %argslist59560$anf_45bind473532, %struct.ScmObj** %stackaddr$prim61032, align 8
%clofunc61033 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47353)
musttail call tailcc void %clofunc61033(%struct.ScmObj* %anf_45bind47353, %struct.ScmObj* %argslist59560$anf_45bind473532)
ret void
}

define tailcc void @proc_clo$ae51237(%struct.ScmObj* %env$ae51237,%struct.ScmObj* %current_45args59552) {
%stackaddr$env-ref61034 = alloca %struct.ScmObj*, align 8
%k47470 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51237, i64 0)
store %struct.ScmObj* %k47470, %struct.ScmObj** %stackaddr$env-ref61034
%stackaddr$env-ref61035 = alloca %struct.ScmObj*, align 8
%anf_45bind47352 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51237, i64 1)
store %struct.ScmObj* %anf_45bind47352, %struct.ScmObj** %stackaddr$env-ref61035
%stackaddr$prim61036 = alloca %struct.ScmObj*, align 8
%_95k47472 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59552)
store volatile %struct.ScmObj* %_95k47472, %struct.ScmObj** %stackaddr$prim61036, align 8
%stackaddr$prim61037 = alloca %struct.ScmObj*, align 8
%current_45args59553 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59552)
store volatile %struct.ScmObj* %current_45args59553, %struct.ScmObj** %stackaddr$prim61037, align 8
%stackaddr$prim61038 = alloca %struct.ScmObj*, align 8
%anf_45bind47354 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59553)
store volatile %struct.ScmObj* %anf_45bind47354, %struct.ScmObj** %stackaddr$prim61038, align 8
%stackaddr$makeclosure61039 = alloca %struct.ScmObj*, align 8
%fptrToInt61040 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51240 to i64
%ae51240 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt61040)
store volatile %struct.ScmObj* %ae51240, %struct.ScmObj** %stackaddr$makeclosure61039, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51240, %struct.ScmObj* %k47470, i64 0)
%argslist59559$anf_45bind473520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61041 = alloca %struct.ScmObj*, align 8
%argslist59559$anf_45bind473521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47354, %struct.ScmObj* %argslist59559$anf_45bind473520)
store volatile %struct.ScmObj* %argslist59559$anf_45bind473521, %struct.ScmObj** %stackaddr$prim61041, align 8
%stackaddr$prim61042 = alloca %struct.ScmObj*, align 8
%argslist59559$anf_45bind473522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51240, %struct.ScmObj* %argslist59559$anf_45bind473521)
store volatile %struct.ScmObj* %argslist59559$anf_45bind473522, %struct.ScmObj** %stackaddr$prim61042, align 8
%clofunc61043 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47352)
musttail call tailcc void %clofunc61043(%struct.ScmObj* %anf_45bind47352, %struct.ScmObj* %argslist59559$anf_45bind473522)
ret void
}

define tailcc void @proc_clo$ae51240(%struct.ScmObj* %env$ae51240,%struct.ScmObj* %current_45args59555) {
%stackaddr$env-ref61044 = alloca %struct.ScmObj*, align 8
%k47470 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51240, i64 0)
store %struct.ScmObj* %k47470, %struct.ScmObj** %stackaddr$env-ref61044
%stackaddr$prim61045 = alloca %struct.ScmObj*, align 8
%_95k47473 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59555)
store volatile %struct.ScmObj* %_95k47473, %struct.ScmObj** %stackaddr$prim61045, align 8
%stackaddr$prim61046 = alloca %struct.ScmObj*, align 8
%current_45args59556 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59555)
store volatile %struct.ScmObj* %current_45args59556, %struct.ScmObj** %stackaddr$prim61046, align 8
%stackaddr$prim61047 = alloca %struct.ScmObj*, align 8
%anf_45bind47355 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59556)
store volatile %struct.ScmObj* %anf_45bind47355, %struct.ScmObj** %stackaddr$prim61047, align 8
%ae51242 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim61048 = alloca %struct.ScmObj*, align 8
%cpsprim47474 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae51242, %struct.ScmObj* %anf_45bind47355)
store volatile %struct.ScmObj* %cpsprim47474, %struct.ScmObj** %stackaddr$prim61048, align 8
%ae51245 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist59558$k474700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61049 = alloca %struct.ScmObj*, align 8
%argslist59558$k474701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47474, %struct.ScmObj* %argslist59558$k474700)
store volatile %struct.ScmObj* %argslist59558$k474701, %struct.ScmObj** %stackaddr$prim61049, align 8
%stackaddr$prim61050 = alloca %struct.ScmObj*, align 8
%argslist59558$k474702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51245, %struct.ScmObj* %argslist59558$k474701)
store volatile %struct.ScmObj* %argslist59558$k474702, %struct.ScmObj** %stackaddr$prim61050, align 8
%clofunc61051 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47470)
musttail call tailcc void %clofunc61051(%struct.ScmObj* %k47470, %struct.ScmObj* %argslist59558$k474702)
ret void
}

define tailcc void @proc_clo$ae51005(%struct.ScmObj* %env$ae51005,%struct.ScmObj* %current_45args59563) {
%stackaddr$env-ref61052 = alloca %struct.ScmObj*, align 8
%nat_45_62peano47200 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51005, i64 0)
store %struct.ScmObj* %nat_45_62peano47200, %struct.ScmObj** %stackaddr$env-ref61052
%stackaddr$prim61053 = alloca %struct.ScmObj*, align 8
%k47475 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59563)
store volatile %struct.ScmObj* %k47475, %struct.ScmObj** %stackaddr$prim61053, align 8
%stackaddr$prim61054 = alloca %struct.ScmObj*, align 8
%current_45args59564 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59563)
store volatile %struct.ScmObj* %current_45args59564, %struct.ScmObj** %stackaddr$prim61054, align 8
%stackaddr$prim61055 = alloca %struct.ScmObj*, align 8
%n47220 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59564)
store volatile %struct.ScmObj* %n47220, %struct.ScmObj** %stackaddr$prim61055, align 8
%ae51006 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61056 = alloca %struct.ScmObj*, align 8
%anf_45bind47342 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae51006, %struct.ScmObj* %n47220)
store volatile %struct.ScmObj* %anf_45bind47342, %struct.ScmObj** %stackaddr$prim61056, align 8
%truthy$cmp61057 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47342)
%cmp$cmp61057 = icmp eq i64 %truthy$cmp61057, 1
br i1 %cmp$cmp61057, label %truebranch$cmp61057, label %falsebranch$cmp61057
truebranch$cmp61057:
%stackaddr$makeclosure61058 = alloca %struct.ScmObj*, align 8
%fptrToInt61059 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51009 to i64
%ae51009 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt61059)
store volatile %struct.ScmObj* %ae51009, %struct.ScmObj** %stackaddr$makeclosure61058, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51009, %struct.ScmObj* %k47475, i64 0)
%ae51010 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61060 = alloca %struct.ScmObj*, align 8
%fptrToInt61061 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51011 to i64
%ae51011 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt61061)
store volatile %struct.ScmObj* %ae51011, %struct.ScmObj** %stackaddr$makeclosure61060, align 8
%argslist59571$ae510090 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61062 = alloca %struct.ScmObj*, align 8
%argslist59571$ae510091 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51011, %struct.ScmObj* %argslist59571$ae510090)
store volatile %struct.ScmObj* %argslist59571$ae510091, %struct.ScmObj** %stackaddr$prim61062, align 8
%stackaddr$prim61063 = alloca %struct.ScmObj*, align 8
%argslist59571$ae510092 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51010, %struct.ScmObj* %argslist59571$ae510091)
store volatile %struct.ScmObj* %argslist59571$ae510092, %struct.ScmObj** %stackaddr$prim61063, align 8
%clofunc61064 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51009)
musttail call tailcc void %clofunc61064(%struct.ScmObj* %ae51009, %struct.ScmObj* %argslist59571$ae510092)
ret void
falsebranch$cmp61057:
%ae51042 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61065 = alloca %struct.ScmObj*, align 8
%anf_45bind47344 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %nat_45_62peano47200, %struct.ScmObj* %ae51042)
store volatile %struct.ScmObj* %anf_45bind47344, %struct.ScmObj** %stackaddr$prim61065, align 8
%ae51044 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim61066 = alloca %struct.ScmObj*, align 8
%anf_45bind47345 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n47220, %struct.ScmObj* %ae51044)
store volatile %struct.ScmObj* %anf_45bind47345, %struct.ScmObj** %stackaddr$prim61066, align 8
%stackaddr$makeclosure61067 = alloca %struct.ScmObj*, align 8
%fptrToInt61068 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51046 to i64
%ae51046 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt61068)
store volatile %struct.ScmObj* %ae51046, %struct.ScmObj** %stackaddr$makeclosure61067, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51046, %struct.ScmObj* %k47475, i64 0)
%argslist59585$anf_45bind473440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61069 = alloca %struct.ScmObj*, align 8
%argslist59585$anf_45bind473441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47345, %struct.ScmObj* %argslist59585$anf_45bind473440)
store volatile %struct.ScmObj* %argslist59585$anf_45bind473441, %struct.ScmObj** %stackaddr$prim61069, align 8
%stackaddr$prim61070 = alloca %struct.ScmObj*, align 8
%argslist59585$anf_45bind473442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51046, %struct.ScmObj* %argslist59585$anf_45bind473441)
store volatile %struct.ScmObj* %argslist59585$anf_45bind473442, %struct.ScmObj** %stackaddr$prim61070, align 8
%clofunc61071 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47344)
musttail call tailcc void %clofunc61071(%struct.ScmObj* %anf_45bind47344, %struct.ScmObj* %argslist59585$anf_45bind473442)
ret void
}

define tailcc void @proc_clo$ae51009(%struct.ScmObj* %env$ae51009,%struct.ScmObj* %current_45args59566) {
%stackaddr$env-ref61072 = alloca %struct.ScmObj*, align 8
%k47475 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51009, i64 0)
store %struct.ScmObj* %k47475, %struct.ScmObj** %stackaddr$env-ref61072
%stackaddr$prim61073 = alloca %struct.ScmObj*, align 8
%_95k47476 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59566)
store volatile %struct.ScmObj* %_95k47476, %struct.ScmObj** %stackaddr$prim61073, align 8
%stackaddr$prim61074 = alloca %struct.ScmObj*, align 8
%current_45args59567 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59566)
store volatile %struct.ScmObj* %current_45args59567, %struct.ScmObj** %stackaddr$prim61074, align 8
%stackaddr$prim61075 = alloca %struct.ScmObj*, align 8
%anf_45bind47343 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59567)
store volatile %struct.ScmObj* %anf_45bind47343, %struct.ScmObj** %stackaddr$prim61075, align 8
%argslist59569$anf_45bind473430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61076 = alloca %struct.ScmObj*, align 8
%argslist59569$anf_45bind473431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47475, %struct.ScmObj* %argslist59569$anf_45bind473430)
store volatile %struct.ScmObj* %argslist59569$anf_45bind473431, %struct.ScmObj** %stackaddr$prim61076, align 8
%clofunc61077 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47343)
musttail call tailcc void %clofunc61077(%struct.ScmObj* %anf_45bind47343, %struct.ScmObj* %argslist59569$anf_45bind473431)
ret void
}

define tailcc void @proc_clo$ae51011(%struct.ScmObj* %env$ae51011,%struct.ScmObj* %lst4722147477) {
%stackaddr$prim61078 = alloca %struct.ScmObj*, align 8
%k47478 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4722147477)
store volatile %struct.ScmObj* %k47478, %struct.ScmObj** %stackaddr$prim61078, align 8
%stackaddr$prim61079 = alloca %struct.ScmObj*, align 8
%lst47221 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4722147477)
store volatile %struct.ScmObj* %lst47221, %struct.ScmObj** %stackaddr$prim61079, align 8
%ae51015 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist59570$k474780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61080 = alloca %struct.ScmObj*, align 8
%argslist59570$k474781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47221, %struct.ScmObj* %argslist59570$k474780)
store volatile %struct.ScmObj* %argslist59570$k474781, %struct.ScmObj** %stackaddr$prim61080, align 8
%stackaddr$prim61081 = alloca %struct.ScmObj*, align 8
%argslist59570$k474782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51015, %struct.ScmObj* %argslist59570$k474781)
store volatile %struct.ScmObj* %argslist59570$k474782, %struct.ScmObj** %stackaddr$prim61081, align 8
%clofunc61082 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47478)
musttail call tailcc void %clofunc61082(%struct.ScmObj* %k47478, %struct.ScmObj* %argslist59570$k474782)
ret void
}

define tailcc void @proc_clo$ae51046(%struct.ScmObj* %env$ae51046,%struct.ScmObj* %current_45args59572) {
%stackaddr$env-ref61083 = alloca %struct.ScmObj*, align 8
%k47475 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51046, i64 0)
store %struct.ScmObj* %k47475, %struct.ScmObj** %stackaddr$env-ref61083
%stackaddr$prim61084 = alloca %struct.ScmObj*, align 8
%_95k47479 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59572)
store volatile %struct.ScmObj* %_95k47479, %struct.ScmObj** %stackaddr$prim61084, align 8
%stackaddr$prim61085 = alloca %struct.ScmObj*, align 8
%current_45args59573 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59572)
store volatile %struct.ScmObj* %current_45args59573, %struct.ScmObj** %stackaddr$prim61085, align 8
%stackaddr$prim61086 = alloca %struct.ScmObj*, align 8
%anf_45bind47346 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59573)
store volatile %struct.ScmObj* %anf_45bind47346, %struct.ScmObj** %stackaddr$prim61086, align 8
%stackaddr$makeclosure61087 = alloca %struct.ScmObj*, align 8
%fptrToInt61088 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51048 to i64
%ae51048 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt61088)
store volatile %struct.ScmObj* %ae51048, %struct.ScmObj** %stackaddr$makeclosure61087, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51048, %struct.ScmObj* %k47475, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51048, %struct.ScmObj* %anf_45bind47346, i64 1)
%ae51049 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61089 = alloca %struct.ScmObj*, align 8
%fptrToInt61090 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51050 to i64
%ae51050 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt61090)
store volatile %struct.ScmObj* %ae51050, %struct.ScmObj** %stackaddr$makeclosure61089, align 8
%argslist59584$ae510480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61091 = alloca %struct.ScmObj*, align 8
%argslist59584$ae510481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51050, %struct.ScmObj* %argslist59584$ae510480)
store volatile %struct.ScmObj* %argslist59584$ae510481, %struct.ScmObj** %stackaddr$prim61091, align 8
%stackaddr$prim61092 = alloca %struct.ScmObj*, align 8
%argslist59584$ae510482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51049, %struct.ScmObj* %argslist59584$ae510481)
store volatile %struct.ScmObj* %argslist59584$ae510482, %struct.ScmObj** %stackaddr$prim61092, align 8
%clofunc61093 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51048)
musttail call tailcc void %clofunc61093(%struct.ScmObj* %ae51048, %struct.ScmObj* %argslist59584$ae510482)
ret void
}

define tailcc void @proc_clo$ae51048(%struct.ScmObj* %env$ae51048,%struct.ScmObj* %current_45args59575) {
%stackaddr$env-ref61094 = alloca %struct.ScmObj*, align 8
%k47475 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51048, i64 0)
store %struct.ScmObj* %k47475, %struct.ScmObj** %stackaddr$env-ref61094
%stackaddr$env-ref61095 = alloca %struct.ScmObj*, align 8
%anf_45bind47346 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51048, i64 1)
store %struct.ScmObj* %anf_45bind47346, %struct.ScmObj** %stackaddr$env-ref61095
%stackaddr$prim61096 = alloca %struct.ScmObj*, align 8
%_95k47480 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59575)
store volatile %struct.ScmObj* %_95k47480, %struct.ScmObj** %stackaddr$prim61096, align 8
%stackaddr$prim61097 = alloca %struct.ScmObj*, align 8
%current_45args59576 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59575)
store volatile %struct.ScmObj* %current_45args59576, %struct.ScmObj** %stackaddr$prim61097, align 8
%stackaddr$prim61098 = alloca %struct.ScmObj*, align 8
%anf_45bind47347 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59576)
store volatile %struct.ScmObj* %anf_45bind47347, %struct.ScmObj** %stackaddr$prim61098, align 8
%stackaddr$makeclosure61099 = alloca %struct.ScmObj*, align 8
%fptrToInt61100 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51071 to i64
%ae51071 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt61100)
store volatile %struct.ScmObj* %ae51071, %struct.ScmObj** %stackaddr$makeclosure61099, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51071, %struct.ScmObj* %k47475, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51071, %struct.ScmObj* %anf_45bind47346, i64 1)
%argslist59582$anf_45bind473470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61101 = alloca %struct.ScmObj*, align 8
%argslist59582$anf_45bind473471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51071, %struct.ScmObj* %argslist59582$anf_45bind473470)
store volatile %struct.ScmObj* %argslist59582$anf_45bind473471, %struct.ScmObj** %stackaddr$prim61101, align 8
%clofunc61102 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47347)
musttail call tailcc void %clofunc61102(%struct.ScmObj* %anf_45bind47347, %struct.ScmObj* %argslist59582$anf_45bind473471)
ret void
}

define tailcc void @proc_clo$ae51071(%struct.ScmObj* %env$ae51071,%struct.ScmObj* %current_45args59578) {
%stackaddr$env-ref61103 = alloca %struct.ScmObj*, align 8
%k47475 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51071, i64 0)
store %struct.ScmObj* %k47475, %struct.ScmObj** %stackaddr$env-ref61103
%stackaddr$env-ref61104 = alloca %struct.ScmObj*, align 8
%anf_45bind47346 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51071, i64 1)
store %struct.ScmObj* %anf_45bind47346, %struct.ScmObj** %stackaddr$env-ref61104
%stackaddr$prim61105 = alloca %struct.ScmObj*, align 8
%_95k47481 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59578)
store volatile %struct.ScmObj* %_95k47481, %struct.ScmObj** %stackaddr$prim61105, align 8
%stackaddr$prim61106 = alloca %struct.ScmObj*, align 8
%current_45args59579 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59578)
store volatile %struct.ScmObj* %current_45args59579, %struct.ScmObj** %stackaddr$prim61106, align 8
%stackaddr$prim61107 = alloca %struct.ScmObj*, align 8
%anf_45bind47348 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59579)
store volatile %struct.ScmObj* %anf_45bind47348, %struct.ScmObj** %stackaddr$prim61107, align 8
%stackaddr$prim61108 = alloca %struct.ScmObj*, align 8
%cpsprim47482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47346, %struct.ScmObj* %anf_45bind47348)
store volatile %struct.ScmObj* %cpsprim47482, %struct.ScmObj** %stackaddr$prim61108, align 8
%ae51075 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist59581$k474750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61109 = alloca %struct.ScmObj*, align 8
%argslist59581$k474751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47482, %struct.ScmObj* %argslist59581$k474750)
store volatile %struct.ScmObj* %argslist59581$k474751, %struct.ScmObj** %stackaddr$prim61109, align 8
%stackaddr$prim61110 = alloca %struct.ScmObj*, align 8
%argslist59581$k474752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51075, %struct.ScmObj* %argslist59581$k474751)
store volatile %struct.ScmObj* %argslist59581$k474752, %struct.ScmObj** %stackaddr$prim61110, align 8
%clofunc61111 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47475)
musttail call tailcc void %clofunc61111(%struct.ScmObj* %k47475, %struct.ScmObj* %argslist59581$k474752)
ret void
}

define tailcc void @proc_clo$ae51050(%struct.ScmObj* %env$ae51050,%struct.ScmObj* %lst4722247483) {
%stackaddr$prim61112 = alloca %struct.ScmObj*, align 8
%k47484 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4722247483)
store volatile %struct.ScmObj* %k47484, %struct.ScmObj** %stackaddr$prim61112, align 8
%stackaddr$prim61113 = alloca %struct.ScmObj*, align 8
%lst47222 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4722247483)
store volatile %struct.ScmObj* %lst47222, %struct.ScmObj** %stackaddr$prim61113, align 8
%ae51054 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist59583$k474840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61114 = alloca %struct.ScmObj*, align 8
%argslist59583$k474841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47222, %struct.ScmObj* %argslist59583$k474840)
store volatile %struct.ScmObj* %argslist59583$k474841, %struct.ScmObj** %stackaddr$prim61114, align 8
%stackaddr$prim61115 = alloca %struct.ScmObj*, align 8
%argslist59583$k474842 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51054, %struct.ScmObj* %argslist59583$k474841)
store volatile %struct.ScmObj* %argslist59583$k474842, %struct.ScmObj** %stackaddr$prim61115, align 8
%clofunc61116 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47484)
musttail call tailcc void %clofunc61116(%struct.ScmObj* %k47484, %struct.ScmObj* %argslist59583$k474842)
ret void
}

define tailcc void @proc_clo$ae50965(%struct.ScmObj* %env$ae50965,%struct.ScmObj* %current_45args59587) {
%stackaddr$prim61117 = alloca %struct.ScmObj*, align 8
%k47485 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59587)
store volatile %struct.ScmObj* %k47485, %struct.ScmObj** %stackaddr$prim61117, align 8
%stackaddr$prim61118 = alloca %struct.ScmObj*, align 8
%current_45args59588 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59587)
store volatile %struct.ScmObj* %current_45args59588, %struct.ScmObj** %stackaddr$prim61118, align 8
%stackaddr$prim61119 = alloca %struct.ScmObj*, align 8
%x47134 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59588)
store volatile %struct.ScmObj* %x47134, %struct.ScmObj** %stackaddr$prim61119, align 8
%stackaddr$prim61120 = alloca %struct.ScmObj*, align 8
%anf_45bind47332 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47134)
store volatile %struct.ScmObj* %anf_45bind47332, %struct.ScmObj** %stackaddr$prim61120, align 8
%stackaddr$prim61121 = alloca %struct.ScmObj*, align 8
%anf_45bind47333 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47332)
store volatile %struct.ScmObj* %anf_45bind47333, %struct.ScmObj** %stackaddr$prim61121, align 8
%stackaddr$prim61122 = alloca %struct.ScmObj*, align 8
%anf_45bind47334 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47333)
store volatile %struct.ScmObj* %anf_45bind47334, %struct.ScmObj** %stackaddr$prim61122, align 8
%stackaddr$prim61123 = alloca %struct.ScmObj*, align 8
%cpsprim47486 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47334)
store volatile %struct.ScmObj* %cpsprim47486, %struct.ScmObj** %stackaddr$prim61123, align 8
%ae50971 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist59590$k474850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61124 = alloca %struct.ScmObj*, align 8
%argslist59590$k474851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47486, %struct.ScmObj* %argslist59590$k474850)
store volatile %struct.ScmObj* %argslist59590$k474851, %struct.ScmObj** %stackaddr$prim61124, align 8
%stackaddr$prim61125 = alloca %struct.ScmObj*, align 8
%argslist59590$k474852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50971, %struct.ScmObj* %argslist59590$k474851)
store volatile %struct.ScmObj* %argslist59590$k474852, %struct.ScmObj** %stackaddr$prim61125, align 8
%clofunc61126 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47485)
musttail call tailcc void %clofunc61126(%struct.ScmObj* %k47485, %struct.ScmObj* %argslist59590$k474852)
ret void
}

define tailcc void @proc_clo$ae50941(%struct.ScmObj* %env$ae50941,%struct.ScmObj* %current_45args59592) {
%stackaddr$prim61127 = alloca %struct.ScmObj*, align 8
%k47487 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59592)
store volatile %struct.ScmObj* %k47487, %struct.ScmObj** %stackaddr$prim61127, align 8
%stackaddr$prim61128 = alloca %struct.ScmObj*, align 8
%current_45args59593 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59592)
store volatile %struct.ScmObj* %current_45args59593, %struct.ScmObj** %stackaddr$prim61128, align 8
%stackaddr$prim61129 = alloca %struct.ScmObj*, align 8
%x47136 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59593)
store volatile %struct.ScmObj* %x47136, %struct.ScmObj** %stackaddr$prim61129, align 8
%stackaddr$prim61130 = alloca %struct.ScmObj*, align 8
%anf_45bind47330 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47136)
store volatile %struct.ScmObj* %anf_45bind47330, %struct.ScmObj** %stackaddr$prim61130, align 8
%stackaddr$prim61131 = alloca %struct.ScmObj*, align 8
%anf_45bind47331 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47330)
store volatile %struct.ScmObj* %anf_45bind47331, %struct.ScmObj** %stackaddr$prim61131, align 8
%stackaddr$prim61132 = alloca %struct.ScmObj*, align 8
%cpsprim47488 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47331)
store volatile %struct.ScmObj* %cpsprim47488, %struct.ScmObj** %stackaddr$prim61132, align 8
%ae50946 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist59595$k474870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61133 = alloca %struct.ScmObj*, align 8
%argslist59595$k474871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47488, %struct.ScmObj* %argslist59595$k474870)
store volatile %struct.ScmObj* %argslist59595$k474871, %struct.ScmObj** %stackaddr$prim61133, align 8
%stackaddr$prim61134 = alloca %struct.ScmObj*, align 8
%argslist59595$k474872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50946, %struct.ScmObj* %argslist59595$k474871)
store volatile %struct.ScmObj* %argslist59595$k474872, %struct.ScmObj** %stackaddr$prim61134, align 8
%clofunc61135 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47487)
musttail call tailcc void %clofunc61135(%struct.ScmObj* %k47487, %struct.ScmObj* %argslist59595$k474872)
ret void
}

define tailcc void @proc_clo$ae50919(%struct.ScmObj* %env$ae50919,%struct.ScmObj* %current_45args59597) {
%stackaddr$prim61136 = alloca %struct.ScmObj*, align 8
%k47489 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59597)
store volatile %struct.ScmObj* %k47489, %struct.ScmObj** %stackaddr$prim61136, align 8
%stackaddr$prim61137 = alloca %struct.ScmObj*, align 8
%current_45args59598 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59597)
store volatile %struct.ScmObj* %current_45args59598, %struct.ScmObj** %stackaddr$prim61137, align 8
%stackaddr$prim61138 = alloca %struct.ScmObj*, align 8
%x47138 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59598)
store volatile %struct.ScmObj* %x47138, %struct.ScmObj** %stackaddr$prim61138, align 8
%stackaddr$prim61139 = alloca %struct.ScmObj*, align 8
%anf_45bind47329 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47138)
store volatile %struct.ScmObj* %anf_45bind47329, %struct.ScmObj** %stackaddr$prim61139, align 8
%stackaddr$prim61140 = alloca %struct.ScmObj*, align 8
%cpsprim47490 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47329)
store volatile %struct.ScmObj* %cpsprim47490, %struct.ScmObj** %stackaddr$prim61140, align 8
%ae50923 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist59600$k474890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61141 = alloca %struct.ScmObj*, align 8
%argslist59600$k474891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47490, %struct.ScmObj* %argslist59600$k474890)
store volatile %struct.ScmObj* %argslist59600$k474891, %struct.ScmObj** %stackaddr$prim61141, align 8
%stackaddr$prim61142 = alloca %struct.ScmObj*, align 8
%argslist59600$k474892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50923, %struct.ScmObj* %argslist59600$k474891)
store volatile %struct.ScmObj* %argslist59600$k474892, %struct.ScmObj** %stackaddr$prim61142, align 8
%clofunc61143 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47489)
musttail call tailcc void %clofunc61143(%struct.ScmObj* %k47489, %struct.ScmObj* %argslist59600$k474892)
ret void
}

define tailcc void @proc_clo$ae50899(%struct.ScmObj* %env$ae50899,%struct.ScmObj* %current_45args59602) {
%stackaddr$prim61144 = alloca %struct.ScmObj*, align 8
%k47491 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59602)
store volatile %struct.ScmObj* %k47491, %struct.ScmObj** %stackaddr$prim61144, align 8
%stackaddr$prim61145 = alloca %struct.ScmObj*, align 8
%current_45args59603 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59602)
store volatile %struct.ScmObj* %current_45args59603, %struct.ScmObj** %stackaddr$prim61145, align 8
%stackaddr$prim61146 = alloca %struct.ScmObj*, align 8
%x47140 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59603)
store volatile %struct.ScmObj* %x47140, %struct.ScmObj** %stackaddr$prim61146, align 8
%stackaddr$prim61147 = alloca %struct.ScmObj*, align 8
%cpsprim47492 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47140)
store volatile %struct.ScmObj* %cpsprim47492, %struct.ScmObj** %stackaddr$prim61147, align 8
%ae50902 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist59605$k474910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61148 = alloca %struct.ScmObj*, align 8
%argslist59605$k474911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47492, %struct.ScmObj* %argslist59605$k474910)
store volatile %struct.ScmObj* %argslist59605$k474911, %struct.ScmObj** %stackaddr$prim61148, align 8
%stackaddr$prim61149 = alloca %struct.ScmObj*, align 8
%argslist59605$k474912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50902, %struct.ScmObj* %argslist59605$k474911)
store volatile %struct.ScmObj* %argslist59605$k474912, %struct.ScmObj** %stackaddr$prim61149, align 8
%clofunc61150 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47491)
musttail call tailcc void %clofunc61150(%struct.ScmObj* %k47491, %struct.ScmObj* %argslist59605$k474912)
ret void
}

define tailcc void @proc_clo$ae50801(%struct.ScmObj* %env$ae50801,%struct.ScmObj* %args4714247493) {
%stackaddr$env-ref61151 = alloca %struct.ScmObj*, align 8
%_37foldl147081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50801, i64 0)
store %struct.ScmObj* %_37foldl147081, %struct.ScmObj** %stackaddr$env-ref61151
%stackaddr$prim61152 = alloca %struct.ScmObj*, align 8
%k47494 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4714247493)
store volatile %struct.ScmObj* %k47494, %struct.ScmObj** %stackaddr$prim61152, align 8
%stackaddr$prim61153 = alloca %struct.ScmObj*, align 8
%args47142 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4714247493)
store volatile %struct.ScmObj* %args47142, %struct.ScmObj** %stackaddr$prim61153, align 8
%stackaddr$prim61154 = alloca %struct.ScmObj*, align 8
%anf_45bind47323 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args47142)
store volatile %struct.ScmObj* %anf_45bind47323, %struct.ScmObj** %stackaddr$prim61154, align 8
%truthy$cmp61155 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47323)
%cmp$cmp61155 = icmp eq i64 %truthy$cmp61155, 1
br i1 %cmp$cmp61155, label %truebranch$cmp61155, label %falsebranch$cmp61155
truebranch$cmp61155:
%ae50807 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50808 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist59607$k474940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61156 = alloca %struct.ScmObj*, align 8
%argslist59607$k474941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50808, %struct.ScmObj* %argslist59607$k474940)
store volatile %struct.ScmObj* %argslist59607$k474941, %struct.ScmObj** %stackaddr$prim61156, align 8
%stackaddr$prim61157 = alloca %struct.ScmObj*, align 8
%argslist59607$k474942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50807, %struct.ScmObj* %argslist59607$k474941)
store volatile %struct.ScmObj* %argslist59607$k474942, %struct.ScmObj** %stackaddr$prim61157, align 8
%clofunc61158 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47494)
musttail call tailcc void %clofunc61158(%struct.ScmObj* %k47494, %struct.ScmObj* %argslist59607$k474942)
ret void
falsebranch$cmp61155:
%stackaddr$prim61159 = alloca %struct.ScmObj*, align 8
%anf_45bind47324 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47142)
store volatile %struct.ScmObj* %anf_45bind47324, %struct.ScmObj** %stackaddr$prim61159, align 8
%stackaddr$prim61160 = alloca %struct.ScmObj*, align 8
%anf_45bind47325 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47324)
store volatile %struct.ScmObj* %anf_45bind47325, %struct.ScmObj** %stackaddr$prim61160, align 8
%truthy$cmp61161 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47325)
%cmp$cmp61161 = icmp eq i64 %truthy$cmp61161, 1
br i1 %cmp$cmp61161, label %truebranch$cmp61161, label %falsebranch$cmp61161
truebranch$cmp61161:
%stackaddr$prim61162 = alloca %struct.ScmObj*, align 8
%cpsprim47495 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47142)
store volatile %struct.ScmObj* %cpsprim47495, %struct.ScmObj** %stackaddr$prim61162, align 8
%ae50820 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist59608$k474940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61163 = alloca %struct.ScmObj*, align 8
%argslist59608$k474941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47495, %struct.ScmObj* %argslist59608$k474940)
store volatile %struct.ScmObj* %argslist59608$k474941, %struct.ScmObj** %stackaddr$prim61163, align 8
%stackaddr$prim61164 = alloca %struct.ScmObj*, align 8
%argslist59608$k474942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50820, %struct.ScmObj* %argslist59608$k474941)
store volatile %struct.ScmObj* %argslist59608$k474942, %struct.ScmObj** %stackaddr$prim61164, align 8
%clofunc61165 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47494)
musttail call tailcc void %clofunc61165(%struct.ScmObj* %k47494, %struct.ScmObj* %argslist59608$k474942)
ret void
falsebranch$cmp61161:
%stackaddr$makeclosure61166 = alloca %struct.ScmObj*, align 8
%fptrToInt61167 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50825 to i64
%ae50825 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt61167)
store volatile %struct.ScmObj* %ae50825, %struct.ScmObj** %stackaddr$makeclosure61166, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50825, %struct.ScmObj* %k47494, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50825, %struct.ScmObj* %args47142, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50825, %struct.ScmObj* %_37foldl147081, i64 2)
%ae50826 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61168 = alloca %struct.ScmObj*, align 8
%fptrToInt61169 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50827 to i64
%ae50827 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt61169)
store volatile %struct.ScmObj* %ae50827, %struct.ScmObj** %stackaddr$makeclosure61168, align 8
%argslist59618$ae508250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61170 = alloca %struct.ScmObj*, align 8
%argslist59618$ae508251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50827, %struct.ScmObj* %argslist59618$ae508250)
store volatile %struct.ScmObj* %argslist59618$ae508251, %struct.ScmObj** %stackaddr$prim61170, align 8
%stackaddr$prim61171 = alloca %struct.ScmObj*, align 8
%argslist59618$ae508252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50826, %struct.ScmObj* %argslist59618$ae508251)
store volatile %struct.ScmObj* %argslist59618$ae508252, %struct.ScmObj** %stackaddr$prim61171, align 8
%clofunc61172 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50825)
musttail call tailcc void %clofunc61172(%struct.ScmObj* %ae50825, %struct.ScmObj* %argslist59618$ae508252)
ret void
}

define tailcc void @proc_clo$ae50825(%struct.ScmObj* %env$ae50825,%struct.ScmObj* %current_45args59609) {
%stackaddr$env-ref61173 = alloca %struct.ScmObj*, align 8
%k47494 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50825, i64 0)
store %struct.ScmObj* %k47494, %struct.ScmObj** %stackaddr$env-ref61173
%stackaddr$env-ref61174 = alloca %struct.ScmObj*, align 8
%args47142 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50825, i64 1)
store %struct.ScmObj* %args47142, %struct.ScmObj** %stackaddr$env-ref61174
%stackaddr$env-ref61175 = alloca %struct.ScmObj*, align 8
%_37foldl147081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50825, i64 2)
store %struct.ScmObj* %_37foldl147081, %struct.ScmObj** %stackaddr$env-ref61175
%stackaddr$prim61176 = alloca %struct.ScmObj*, align 8
%_95k47496 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59609)
store volatile %struct.ScmObj* %_95k47496, %struct.ScmObj** %stackaddr$prim61176, align 8
%stackaddr$prim61177 = alloca %struct.ScmObj*, align 8
%current_45args59610 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59609)
store volatile %struct.ScmObj* %current_45args59610, %struct.ScmObj** %stackaddr$prim61177, align 8
%stackaddr$prim61178 = alloca %struct.ScmObj*, align 8
%anf_45bind47326 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59610)
store volatile %struct.ScmObj* %anf_45bind47326, %struct.ScmObj** %stackaddr$prim61178, align 8
%stackaddr$prim61179 = alloca %struct.ScmObj*, align 8
%anf_45bind47327 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47142)
store volatile %struct.ScmObj* %anf_45bind47327, %struct.ScmObj** %stackaddr$prim61179, align 8
%stackaddr$prim61180 = alloca %struct.ScmObj*, align 8
%anf_45bind47328 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47142)
store volatile %struct.ScmObj* %anf_45bind47328, %struct.ScmObj** %stackaddr$prim61180, align 8
%argslist59612$_37foldl1470810 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61181 = alloca %struct.ScmObj*, align 8
%argslist59612$_37foldl1470811 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47328, %struct.ScmObj* %argslist59612$_37foldl1470810)
store volatile %struct.ScmObj* %argslist59612$_37foldl1470811, %struct.ScmObj** %stackaddr$prim61181, align 8
%stackaddr$prim61182 = alloca %struct.ScmObj*, align 8
%argslist59612$_37foldl1470812 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47327, %struct.ScmObj* %argslist59612$_37foldl1470811)
store volatile %struct.ScmObj* %argslist59612$_37foldl1470812, %struct.ScmObj** %stackaddr$prim61182, align 8
%stackaddr$prim61183 = alloca %struct.ScmObj*, align 8
%argslist59612$_37foldl1470813 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47326, %struct.ScmObj* %argslist59612$_37foldl1470812)
store volatile %struct.ScmObj* %argslist59612$_37foldl1470813, %struct.ScmObj** %stackaddr$prim61183, align 8
%stackaddr$prim61184 = alloca %struct.ScmObj*, align 8
%argslist59612$_37foldl1470814 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47494, %struct.ScmObj* %argslist59612$_37foldl1470813)
store volatile %struct.ScmObj* %argslist59612$_37foldl1470814, %struct.ScmObj** %stackaddr$prim61184, align 8
%clofunc61185 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147081)
musttail call tailcc void %clofunc61185(%struct.ScmObj* %_37foldl147081, %struct.ScmObj* %argslist59612$_37foldl1470814)
ret void
}

define tailcc void @proc_clo$ae50827(%struct.ScmObj* %env$ae50827,%struct.ScmObj* %current_45args59613) {
%stackaddr$prim61186 = alloca %struct.ScmObj*, align 8
%k47497 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59613)
store volatile %struct.ScmObj* %k47497, %struct.ScmObj** %stackaddr$prim61186, align 8
%stackaddr$prim61187 = alloca %struct.ScmObj*, align 8
%current_45args59614 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59613)
store volatile %struct.ScmObj* %current_45args59614, %struct.ScmObj** %stackaddr$prim61187, align 8
%stackaddr$prim61188 = alloca %struct.ScmObj*, align 8
%n47144 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59614)
store volatile %struct.ScmObj* %n47144, %struct.ScmObj** %stackaddr$prim61188, align 8
%stackaddr$prim61189 = alloca %struct.ScmObj*, align 8
%current_45args59615 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59614)
store volatile %struct.ScmObj* %current_45args59615, %struct.ScmObj** %stackaddr$prim61189, align 8
%stackaddr$prim61190 = alloca %struct.ScmObj*, align 8
%v47143 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59615)
store volatile %struct.ScmObj* %v47143, %struct.ScmObj** %stackaddr$prim61190, align 8
%stackaddr$prim61191 = alloca %struct.ScmObj*, align 8
%cpsprim47498 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v47143, %struct.ScmObj* %n47144)
store volatile %struct.ScmObj* %cpsprim47498, %struct.ScmObj** %stackaddr$prim61191, align 8
%ae50831 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist59617$k474970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61192 = alloca %struct.ScmObj*, align 8
%argslist59617$k474971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47498, %struct.ScmObj* %argslist59617$k474970)
store volatile %struct.ScmObj* %argslist59617$k474971, %struct.ScmObj** %stackaddr$prim61192, align 8
%stackaddr$prim61193 = alloca %struct.ScmObj*, align 8
%argslist59617$k474972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50831, %struct.ScmObj* %argslist59617$k474971)
store volatile %struct.ScmObj* %argslist59617$k474972, %struct.ScmObj** %stackaddr$prim61193, align 8
%clofunc61194 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47497)
musttail call tailcc void %clofunc61194(%struct.ScmObj* %k47497, %struct.ScmObj* %argslist59617$k474972)
ret void
}

define tailcc void @proc_clo$ae50397(%struct.ScmObj* %env$ae50397,%struct.ScmObj* %current_45args59620) {
%stackaddr$prim61195 = alloca %struct.ScmObj*, align 8
%k47499 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59620)
store volatile %struct.ScmObj* %k47499, %struct.ScmObj** %stackaddr$prim61195, align 8
%stackaddr$prim61196 = alloca %struct.ScmObj*, align 8
%current_45args59621 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59620)
store volatile %struct.ScmObj* %current_45args59621, %struct.ScmObj** %stackaddr$prim61196, align 8
%stackaddr$prim61197 = alloca %struct.ScmObj*, align 8
%v47147 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59621)
store volatile %struct.ScmObj* %v47147, %struct.ScmObj** %stackaddr$prim61197, align 8
%stackaddr$prim61198 = alloca %struct.ScmObj*, align 8
%current_45args59622 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59621)
store volatile %struct.ScmObj* %current_45args59622, %struct.ScmObj** %stackaddr$prim61198, align 8
%stackaddr$prim61199 = alloca %struct.ScmObj*, align 8
%lst47146 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59622)
store volatile %struct.ScmObj* %lst47146, %struct.ScmObj** %stackaddr$prim61199, align 8
%ae50398 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim61200 = alloca %struct.ScmObj*, align 8
%lst47148 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50398, %struct.ScmObj* %lst47146)
store volatile %struct.ScmObj* %lst47148, %struct.ScmObj** %stackaddr$prim61200, align 8
%stackaddr$makeclosure61201 = alloca %struct.ScmObj*, align 8
%fptrToInt61202 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50400 to i64
%ae50400 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt61202)
store volatile %struct.ScmObj* %ae50400, %struct.ScmObj** %stackaddr$makeclosure61201, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50400, %struct.ScmObj* %v47147, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50400, %struct.ScmObj* %k47499, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50400, %struct.ScmObj* %lst47148, i64 2)
%ae50401 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61203 = alloca %struct.ScmObj*, align 8
%fptrToInt61204 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50402 to i64
%ae50402 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt61204)
store volatile %struct.ScmObj* %ae50402, %struct.ScmObj** %stackaddr$makeclosure61203, align 8
%argslist59644$ae504000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61205 = alloca %struct.ScmObj*, align 8
%argslist59644$ae504001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50402, %struct.ScmObj* %argslist59644$ae504000)
store volatile %struct.ScmObj* %argslist59644$ae504001, %struct.ScmObj** %stackaddr$prim61205, align 8
%stackaddr$prim61206 = alloca %struct.ScmObj*, align 8
%argslist59644$ae504002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50401, %struct.ScmObj* %argslist59644$ae504001)
store volatile %struct.ScmObj* %argslist59644$ae504002, %struct.ScmObj** %stackaddr$prim61206, align 8
%clofunc61207 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50400)
musttail call tailcc void %clofunc61207(%struct.ScmObj* %ae50400, %struct.ScmObj* %argslist59644$ae504002)
ret void
}

define tailcc void @proc_clo$ae50400(%struct.ScmObj* %env$ae50400,%struct.ScmObj* %current_45args59624) {
%stackaddr$env-ref61208 = alloca %struct.ScmObj*, align 8
%v47147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50400, i64 0)
store %struct.ScmObj* %v47147, %struct.ScmObj** %stackaddr$env-ref61208
%stackaddr$env-ref61209 = alloca %struct.ScmObj*, align 8
%k47499 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50400, i64 1)
store %struct.ScmObj* %k47499, %struct.ScmObj** %stackaddr$env-ref61209
%stackaddr$env-ref61210 = alloca %struct.ScmObj*, align 8
%lst47148 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50400, i64 2)
store %struct.ScmObj* %lst47148, %struct.ScmObj** %stackaddr$env-ref61210
%stackaddr$prim61211 = alloca %struct.ScmObj*, align 8
%_95k47500 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59624)
store volatile %struct.ScmObj* %_95k47500, %struct.ScmObj** %stackaddr$prim61211, align 8
%stackaddr$prim61212 = alloca %struct.ScmObj*, align 8
%current_45args59625 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59624)
store volatile %struct.ScmObj* %current_45args59625, %struct.ScmObj** %stackaddr$prim61212, align 8
%stackaddr$prim61213 = alloca %struct.ScmObj*, align 8
%anf_45bind47315 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59625)
store volatile %struct.ScmObj* %anf_45bind47315, %struct.ScmObj** %stackaddr$prim61213, align 8
%stackaddr$makeclosure61214 = alloca %struct.ScmObj*, align 8
%fptrToInt61215 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50416 to i64
%ae50416 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt61215)
store volatile %struct.ScmObj* %ae50416, %struct.ScmObj** %stackaddr$makeclosure61214, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50416, %struct.ScmObj* %v47147, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50416, %struct.ScmObj* %k47499, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50416, %struct.ScmObj* %lst47148, i64 2)
%stackaddr$makeclosure61216 = alloca %struct.ScmObj*, align 8
%fptrToInt61217 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50417 to i64
%ae50417 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt61217)
store volatile %struct.ScmObj* %ae50417, %struct.ScmObj** %stackaddr$makeclosure61216, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50417, %struct.ScmObj* %v47147, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50417, %struct.ScmObj* %k47499, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50417, %struct.ScmObj* %lst47148, i64 2)
%argslist59639$anf_45bind473150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61218 = alloca %struct.ScmObj*, align 8
%argslist59639$anf_45bind473151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50417, %struct.ScmObj* %argslist59639$anf_45bind473150)
store volatile %struct.ScmObj* %argslist59639$anf_45bind473151, %struct.ScmObj** %stackaddr$prim61218, align 8
%stackaddr$prim61219 = alloca %struct.ScmObj*, align 8
%argslist59639$anf_45bind473152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50416, %struct.ScmObj* %argslist59639$anf_45bind473151)
store volatile %struct.ScmObj* %argslist59639$anf_45bind473152, %struct.ScmObj** %stackaddr$prim61219, align 8
%clofunc61220 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47315)
musttail call tailcc void %clofunc61220(%struct.ScmObj* %anf_45bind47315, %struct.ScmObj* %argslist59639$anf_45bind473152)
ret void
}

define tailcc void @proc_clo$ae50416(%struct.ScmObj* %env$ae50416,%struct.ScmObj* %current_45args59627) {
%stackaddr$env-ref61221 = alloca %struct.ScmObj*, align 8
%v47147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50416, i64 0)
store %struct.ScmObj* %v47147, %struct.ScmObj** %stackaddr$env-ref61221
%stackaddr$env-ref61222 = alloca %struct.ScmObj*, align 8
%k47499 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50416, i64 1)
store %struct.ScmObj* %k47499, %struct.ScmObj** %stackaddr$env-ref61222
%stackaddr$env-ref61223 = alloca %struct.ScmObj*, align 8
%lst47148 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50416, i64 2)
store %struct.ScmObj* %lst47148, %struct.ScmObj** %stackaddr$env-ref61223
%stackaddr$prim61224 = alloca %struct.ScmObj*, align 8
%_95k47501 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59627)
store volatile %struct.ScmObj* %_95k47501, %struct.ScmObj** %stackaddr$prim61224, align 8
%stackaddr$prim61225 = alloca %struct.ScmObj*, align 8
%current_45args59628 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59627)
store volatile %struct.ScmObj* %current_45args59628, %struct.ScmObj** %stackaddr$prim61225, align 8
%stackaddr$prim61226 = alloca %struct.ScmObj*, align 8
%cc47149 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59628)
store volatile %struct.ScmObj* %cc47149, %struct.ScmObj** %stackaddr$prim61226, align 8
%ae50525 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61227 = alloca %struct.ScmObj*, align 8
%anf_45bind47316 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47148, %struct.ScmObj* %ae50525)
store volatile %struct.ScmObj* %anf_45bind47316, %struct.ScmObj** %stackaddr$prim61227, align 8
%stackaddr$prim61228 = alloca %struct.ScmObj*, align 8
%anf_45bind47317 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47316)
store volatile %struct.ScmObj* %anf_45bind47317, %struct.ScmObj** %stackaddr$prim61228, align 8
%truthy$cmp61229 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47317)
%cmp$cmp61229 = icmp eq i64 %truthy$cmp61229, 1
br i1 %cmp$cmp61229, label %truebranch$cmp61229, label %falsebranch$cmp61229
truebranch$cmp61229:
%ae50529 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50530 = call %struct.ScmObj* @const_init_false()
%argslist59630$k474990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61230 = alloca %struct.ScmObj*, align 8
%argslist59630$k474991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50530, %struct.ScmObj* %argslist59630$k474990)
store volatile %struct.ScmObj* %argslist59630$k474991, %struct.ScmObj** %stackaddr$prim61230, align 8
%stackaddr$prim61231 = alloca %struct.ScmObj*, align 8
%argslist59630$k474992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50529, %struct.ScmObj* %argslist59630$k474991)
store volatile %struct.ScmObj* %argslist59630$k474992, %struct.ScmObj** %stackaddr$prim61231, align 8
%clofunc61232 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47499)
musttail call tailcc void %clofunc61232(%struct.ScmObj* %k47499, %struct.ScmObj* %argslist59630$k474992)
ret void
falsebranch$cmp61229:
%ae50538 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61233 = alloca %struct.ScmObj*, align 8
%anf_45bind47318 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47148, %struct.ScmObj* %ae50538)
store volatile %struct.ScmObj* %anf_45bind47318, %struct.ScmObj** %stackaddr$prim61233, align 8
%stackaddr$prim61234 = alloca %struct.ScmObj*, align 8
%anf_45bind47319 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47318)
store volatile %struct.ScmObj* %anf_45bind47319, %struct.ScmObj** %stackaddr$prim61234, align 8
%stackaddr$prim61235 = alloca %struct.ScmObj*, align 8
%anf_45bind47320 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind47319, %struct.ScmObj* %v47147)
store volatile %struct.ScmObj* %anf_45bind47320, %struct.ScmObj** %stackaddr$prim61235, align 8
%truthy$cmp61236 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47320)
%cmp$cmp61236 = icmp eq i64 %truthy$cmp61236, 1
br i1 %cmp$cmp61236, label %truebranch$cmp61236, label %falsebranch$cmp61236
truebranch$cmp61236:
%ae50544 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61237 = alloca %struct.ScmObj*, align 8
%cpsprim47502 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47148, %struct.ScmObj* %ae50544)
store volatile %struct.ScmObj* %cpsprim47502, %struct.ScmObj** %stackaddr$prim61237, align 8
%ae50546 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist59631$k474990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61238 = alloca %struct.ScmObj*, align 8
%argslist59631$k474991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47502, %struct.ScmObj* %argslist59631$k474990)
store volatile %struct.ScmObj* %argslist59631$k474991, %struct.ScmObj** %stackaddr$prim61238, align 8
%stackaddr$prim61239 = alloca %struct.ScmObj*, align 8
%argslist59631$k474992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50546, %struct.ScmObj* %argslist59631$k474991)
store volatile %struct.ScmObj* %argslist59631$k474992, %struct.ScmObj** %stackaddr$prim61239, align 8
%clofunc61240 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47499)
musttail call tailcc void %clofunc61240(%struct.ScmObj* %k47499, %struct.ScmObj* %argslist59631$k474992)
ret void
falsebranch$cmp61236:
%ae50557 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61241 = alloca %struct.ScmObj*, align 8
%anf_45bind47321 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47148, %struct.ScmObj* %ae50557)
store volatile %struct.ScmObj* %anf_45bind47321, %struct.ScmObj** %stackaddr$prim61241, align 8
%stackaddr$prim61242 = alloca %struct.ScmObj*, align 8
%anf_45bind47322 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47321)
store volatile %struct.ScmObj* %anf_45bind47322, %struct.ScmObj** %stackaddr$prim61242, align 8
%ae50560 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61243 = alloca %struct.ScmObj*, align 8
%_95047151 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47148, %struct.ScmObj* %ae50560, %struct.ScmObj* %anf_45bind47322)
store volatile %struct.ScmObj* %_95047151, %struct.ScmObj** %stackaddr$prim61243, align 8
%argslist59632$cc471490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61244 = alloca %struct.ScmObj*, align 8
%argslist59632$cc471491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47149, %struct.ScmObj* %argslist59632$cc471490)
store volatile %struct.ScmObj* %argslist59632$cc471491, %struct.ScmObj** %stackaddr$prim61244, align 8
%stackaddr$prim61245 = alloca %struct.ScmObj*, align 8
%argslist59632$cc471492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47499, %struct.ScmObj* %argslist59632$cc471491)
store volatile %struct.ScmObj* %argslist59632$cc471492, %struct.ScmObj** %stackaddr$prim61245, align 8
%clofunc61246 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47149)
musttail call tailcc void %clofunc61246(%struct.ScmObj* %cc47149, %struct.ScmObj* %argslist59632$cc471492)
ret void
}

define tailcc void @proc_clo$ae50417(%struct.ScmObj* %env$ae50417,%struct.ScmObj* %current_45args59633) {
%stackaddr$env-ref61247 = alloca %struct.ScmObj*, align 8
%v47147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50417, i64 0)
store %struct.ScmObj* %v47147, %struct.ScmObj** %stackaddr$env-ref61247
%stackaddr$env-ref61248 = alloca %struct.ScmObj*, align 8
%k47499 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50417, i64 1)
store %struct.ScmObj* %k47499, %struct.ScmObj** %stackaddr$env-ref61248
%stackaddr$env-ref61249 = alloca %struct.ScmObj*, align 8
%lst47148 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50417, i64 2)
store %struct.ScmObj* %lst47148, %struct.ScmObj** %stackaddr$env-ref61249
%stackaddr$prim61250 = alloca %struct.ScmObj*, align 8
%_95k47501 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59633)
store volatile %struct.ScmObj* %_95k47501, %struct.ScmObj** %stackaddr$prim61250, align 8
%stackaddr$prim61251 = alloca %struct.ScmObj*, align 8
%current_45args59634 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59633)
store volatile %struct.ScmObj* %current_45args59634, %struct.ScmObj** %stackaddr$prim61251, align 8
%stackaddr$prim61252 = alloca %struct.ScmObj*, align 8
%cc47149 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59634)
store volatile %struct.ScmObj* %cc47149, %struct.ScmObj** %stackaddr$prim61252, align 8
%ae50419 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61253 = alloca %struct.ScmObj*, align 8
%anf_45bind47316 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47148, %struct.ScmObj* %ae50419)
store volatile %struct.ScmObj* %anf_45bind47316, %struct.ScmObj** %stackaddr$prim61253, align 8
%stackaddr$prim61254 = alloca %struct.ScmObj*, align 8
%anf_45bind47317 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47316)
store volatile %struct.ScmObj* %anf_45bind47317, %struct.ScmObj** %stackaddr$prim61254, align 8
%truthy$cmp61255 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47317)
%cmp$cmp61255 = icmp eq i64 %truthy$cmp61255, 1
br i1 %cmp$cmp61255, label %truebranch$cmp61255, label %falsebranch$cmp61255
truebranch$cmp61255:
%ae50423 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50424 = call %struct.ScmObj* @const_init_false()
%argslist59636$k474990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61256 = alloca %struct.ScmObj*, align 8
%argslist59636$k474991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50424, %struct.ScmObj* %argslist59636$k474990)
store volatile %struct.ScmObj* %argslist59636$k474991, %struct.ScmObj** %stackaddr$prim61256, align 8
%stackaddr$prim61257 = alloca %struct.ScmObj*, align 8
%argslist59636$k474992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50423, %struct.ScmObj* %argslist59636$k474991)
store volatile %struct.ScmObj* %argslist59636$k474992, %struct.ScmObj** %stackaddr$prim61257, align 8
%clofunc61258 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47499)
musttail call tailcc void %clofunc61258(%struct.ScmObj* %k47499, %struct.ScmObj* %argslist59636$k474992)
ret void
falsebranch$cmp61255:
%ae50432 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61259 = alloca %struct.ScmObj*, align 8
%anf_45bind47318 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47148, %struct.ScmObj* %ae50432)
store volatile %struct.ScmObj* %anf_45bind47318, %struct.ScmObj** %stackaddr$prim61259, align 8
%stackaddr$prim61260 = alloca %struct.ScmObj*, align 8
%anf_45bind47319 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47318)
store volatile %struct.ScmObj* %anf_45bind47319, %struct.ScmObj** %stackaddr$prim61260, align 8
%stackaddr$prim61261 = alloca %struct.ScmObj*, align 8
%anf_45bind47320 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind47319, %struct.ScmObj* %v47147)
store volatile %struct.ScmObj* %anf_45bind47320, %struct.ScmObj** %stackaddr$prim61261, align 8
%truthy$cmp61262 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47320)
%cmp$cmp61262 = icmp eq i64 %truthy$cmp61262, 1
br i1 %cmp$cmp61262, label %truebranch$cmp61262, label %falsebranch$cmp61262
truebranch$cmp61262:
%ae50438 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61263 = alloca %struct.ScmObj*, align 8
%cpsprim47502 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47148, %struct.ScmObj* %ae50438)
store volatile %struct.ScmObj* %cpsprim47502, %struct.ScmObj** %stackaddr$prim61263, align 8
%ae50440 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist59637$k474990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61264 = alloca %struct.ScmObj*, align 8
%argslist59637$k474991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47502, %struct.ScmObj* %argslist59637$k474990)
store volatile %struct.ScmObj* %argslist59637$k474991, %struct.ScmObj** %stackaddr$prim61264, align 8
%stackaddr$prim61265 = alloca %struct.ScmObj*, align 8
%argslist59637$k474992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50440, %struct.ScmObj* %argslist59637$k474991)
store volatile %struct.ScmObj* %argslist59637$k474992, %struct.ScmObj** %stackaddr$prim61265, align 8
%clofunc61266 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47499)
musttail call tailcc void %clofunc61266(%struct.ScmObj* %k47499, %struct.ScmObj* %argslist59637$k474992)
ret void
falsebranch$cmp61262:
%ae50451 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61267 = alloca %struct.ScmObj*, align 8
%anf_45bind47321 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47148, %struct.ScmObj* %ae50451)
store volatile %struct.ScmObj* %anf_45bind47321, %struct.ScmObj** %stackaddr$prim61267, align 8
%stackaddr$prim61268 = alloca %struct.ScmObj*, align 8
%anf_45bind47322 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47321)
store volatile %struct.ScmObj* %anf_45bind47322, %struct.ScmObj** %stackaddr$prim61268, align 8
%ae50454 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61269 = alloca %struct.ScmObj*, align 8
%_95047151 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47148, %struct.ScmObj* %ae50454, %struct.ScmObj* %anf_45bind47322)
store volatile %struct.ScmObj* %_95047151, %struct.ScmObj** %stackaddr$prim61269, align 8
%argslist59638$cc471490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61270 = alloca %struct.ScmObj*, align 8
%argslist59638$cc471491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47149, %struct.ScmObj* %argslist59638$cc471490)
store volatile %struct.ScmObj* %argslist59638$cc471491, %struct.ScmObj** %stackaddr$prim61270, align 8
%stackaddr$prim61271 = alloca %struct.ScmObj*, align 8
%argslist59638$cc471492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47499, %struct.ScmObj* %argslist59638$cc471491)
store volatile %struct.ScmObj* %argslist59638$cc471492, %struct.ScmObj** %stackaddr$prim61271, align 8
%clofunc61272 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47149)
musttail call tailcc void %clofunc61272(%struct.ScmObj* %cc47149, %struct.ScmObj* %argslist59638$cc471492)
ret void
}

define tailcc void @proc_clo$ae50402(%struct.ScmObj* %env$ae50402,%struct.ScmObj* %current_45args59640) {
%stackaddr$prim61273 = alloca %struct.ScmObj*, align 8
%k47503 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59640)
store volatile %struct.ScmObj* %k47503, %struct.ScmObj** %stackaddr$prim61273, align 8
%stackaddr$prim61274 = alloca %struct.ScmObj*, align 8
%current_45args59641 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59640)
store volatile %struct.ScmObj* %current_45args59641, %struct.ScmObj** %stackaddr$prim61274, align 8
%stackaddr$prim61275 = alloca %struct.ScmObj*, align 8
%u47150 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59641)
store volatile %struct.ScmObj* %u47150, %struct.ScmObj** %stackaddr$prim61275, align 8
%argslist59643$u471500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61276 = alloca %struct.ScmObj*, align 8
%argslist59643$u471501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u47150, %struct.ScmObj* %argslist59643$u471500)
store volatile %struct.ScmObj* %argslist59643$u471501, %struct.ScmObj** %stackaddr$prim61276, align 8
%stackaddr$prim61277 = alloca %struct.ScmObj*, align 8
%argslist59643$u471502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47503, %struct.ScmObj* %argslist59643$u471501)
store volatile %struct.ScmObj* %argslist59643$u471502, %struct.ScmObj** %stackaddr$prim61277, align 8
%clofunc61278 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u47150)
musttail call tailcc void %clofunc61278(%struct.ScmObj* %u47150, %struct.ScmObj* %argslist59643$u471502)
ret void
}

define tailcc void @proc_clo$ae49861(%struct.ScmObj* %env$ae49861,%struct.ScmObj* %current_45args59646) {
%stackaddr$prim61279 = alloca %struct.ScmObj*, align 8
%k47504 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59646)
store volatile %struct.ScmObj* %k47504, %struct.ScmObj** %stackaddr$prim61279, align 8
%stackaddr$prim61280 = alloca %struct.ScmObj*, align 8
%current_45args59647 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59646)
store volatile %struct.ScmObj* %current_45args59647, %struct.ScmObj** %stackaddr$prim61280, align 8
%stackaddr$prim61281 = alloca %struct.ScmObj*, align 8
%lst47154 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59647)
store volatile %struct.ScmObj* %lst47154, %struct.ScmObj** %stackaddr$prim61281, align 8
%stackaddr$prim61282 = alloca %struct.ScmObj*, align 8
%current_45args59648 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59647)
store volatile %struct.ScmObj* %current_45args59648, %struct.ScmObj** %stackaddr$prim61282, align 8
%stackaddr$prim61283 = alloca %struct.ScmObj*, align 8
%n47153 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59648)
store volatile %struct.ScmObj* %n47153, %struct.ScmObj** %stackaddr$prim61283, align 8
%ae49862 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim61284 = alloca %struct.ScmObj*, align 8
%n47156 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49862, %struct.ScmObj* %n47153)
store volatile %struct.ScmObj* %n47156, %struct.ScmObj** %stackaddr$prim61284, align 8
%ae49864 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim61285 = alloca %struct.ScmObj*, align 8
%lst47155 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49864, %struct.ScmObj* %lst47154)
store volatile %struct.ScmObj* %lst47155, %struct.ScmObj** %stackaddr$prim61285, align 8
%stackaddr$makeclosure61286 = alloca %struct.ScmObj*, align 8
%fptrToInt61287 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49866 to i64
%ae49866 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt61287)
store volatile %struct.ScmObj* %ae49866, %struct.ScmObj** %stackaddr$makeclosure61286, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49866, %struct.ScmObj* %n47156, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49866, %struct.ScmObj* %lst47155, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49866, %struct.ScmObj* %k47504, i64 2)
%ae49867 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61288 = alloca %struct.ScmObj*, align 8
%fptrToInt61289 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49868 to i64
%ae49868 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt61289)
store volatile %struct.ScmObj* %ae49868, %struct.ScmObj** %stackaddr$makeclosure61288, align 8
%argslist59668$ae498660 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61290 = alloca %struct.ScmObj*, align 8
%argslist59668$ae498661 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49868, %struct.ScmObj* %argslist59668$ae498660)
store volatile %struct.ScmObj* %argslist59668$ae498661, %struct.ScmObj** %stackaddr$prim61290, align 8
%stackaddr$prim61291 = alloca %struct.ScmObj*, align 8
%argslist59668$ae498662 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49867, %struct.ScmObj* %argslist59668$ae498661)
store volatile %struct.ScmObj* %argslist59668$ae498662, %struct.ScmObj** %stackaddr$prim61291, align 8
%clofunc61292 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49866)
musttail call tailcc void %clofunc61292(%struct.ScmObj* %ae49866, %struct.ScmObj* %argslist59668$ae498662)
ret void
}

define tailcc void @proc_clo$ae49866(%struct.ScmObj* %env$ae49866,%struct.ScmObj* %current_45args59650) {
%stackaddr$env-ref61293 = alloca %struct.ScmObj*, align 8
%n47156 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49866, i64 0)
store %struct.ScmObj* %n47156, %struct.ScmObj** %stackaddr$env-ref61293
%stackaddr$env-ref61294 = alloca %struct.ScmObj*, align 8
%lst47155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49866, i64 1)
store %struct.ScmObj* %lst47155, %struct.ScmObj** %stackaddr$env-ref61294
%stackaddr$env-ref61295 = alloca %struct.ScmObj*, align 8
%k47504 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49866, i64 2)
store %struct.ScmObj* %k47504, %struct.ScmObj** %stackaddr$env-ref61295
%stackaddr$prim61296 = alloca %struct.ScmObj*, align 8
%_95k47505 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59650)
store volatile %struct.ScmObj* %_95k47505, %struct.ScmObj** %stackaddr$prim61296, align 8
%stackaddr$prim61297 = alloca %struct.ScmObj*, align 8
%current_45args59651 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59650)
store volatile %struct.ScmObj* %current_45args59651, %struct.ScmObj** %stackaddr$prim61297, align 8
%stackaddr$prim61298 = alloca %struct.ScmObj*, align 8
%anf_45bind47308 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59651)
store volatile %struct.ScmObj* %anf_45bind47308, %struct.ScmObj** %stackaddr$prim61298, align 8
%stackaddr$makeclosure61299 = alloca %struct.ScmObj*, align 8
%fptrToInt61300 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49882 to i64
%ae49882 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt61300)
store volatile %struct.ScmObj* %ae49882, %struct.ScmObj** %stackaddr$makeclosure61299, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49882, %struct.ScmObj* %n47156, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49882, %struct.ScmObj* %lst47155, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49882, %struct.ScmObj* %k47504, i64 2)
%stackaddr$makeclosure61301 = alloca %struct.ScmObj*, align 8
%fptrToInt61302 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49883 to i64
%ae49883 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt61302)
store volatile %struct.ScmObj* %ae49883, %struct.ScmObj** %stackaddr$makeclosure61301, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49883, %struct.ScmObj* %n47156, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49883, %struct.ScmObj* %lst47155, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49883, %struct.ScmObj* %k47504, i64 2)
%argslist59663$anf_45bind473080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61303 = alloca %struct.ScmObj*, align 8
%argslist59663$anf_45bind473081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49883, %struct.ScmObj* %argslist59663$anf_45bind473080)
store volatile %struct.ScmObj* %argslist59663$anf_45bind473081, %struct.ScmObj** %stackaddr$prim61303, align 8
%stackaddr$prim61304 = alloca %struct.ScmObj*, align 8
%argslist59663$anf_45bind473082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49882, %struct.ScmObj* %argslist59663$anf_45bind473081)
store volatile %struct.ScmObj* %argslist59663$anf_45bind473082, %struct.ScmObj** %stackaddr$prim61304, align 8
%clofunc61305 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47308)
musttail call tailcc void %clofunc61305(%struct.ScmObj* %anf_45bind47308, %struct.ScmObj* %argslist59663$anf_45bind473082)
ret void
}

define tailcc void @proc_clo$ae49882(%struct.ScmObj* %env$ae49882,%struct.ScmObj* %current_45args59653) {
%stackaddr$env-ref61306 = alloca %struct.ScmObj*, align 8
%n47156 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49882, i64 0)
store %struct.ScmObj* %n47156, %struct.ScmObj** %stackaddr$env-ref61306
%stackaddr$env-ref61307 = alloca %struct.ScmObj*, align 8
%lst47155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49882, i64 1)
store %struct.ScmObj* %lst47155, %struct.ScmObj** %stackaddr$env-ref61307
%stackaddr$env-ref61308 = alloca %struct.ScmObj*, align 8
%k47504 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49882, i64 2)
store %struct.ScmObj* %k47504, %struct.ScmObj** %stackaddr$env-ref61308
%stackaddr$prim61309 = alloca %struct.ScmObj*, align 8
%_95k47506 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59653)
store volatile %struct.ScmObj* %_95k47506, %struct.ScmObj** %stackaddr$prim61309, align 8
%stackaddr$prim61310 = alloca %struct.ScmObj*, align 8
%current_45args59654 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59653)
store volatile %struct.ScmObj* %current_45args59654, %struct.ScmObj** %stackaddr$prim61310, align 8
%stackaddr$prim61311 = alloca %struct.ScmObj*, align 8
%cc47157 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59654)
store volatile %struct.ScmObj* %cc47157, %struct.ScmObj** %stackaddr$prim61311, align 8
%ae50025 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61312 = alloca %struct.ScmObj*, align 8
%anf_45bind47309 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47156, %struct.ScmObj* %ae50025)
store volatile %struct.ScmObj* %anf_45bind47309, %struct.ScmObj** %stackaddr$prim61312, align 8
%ae50026 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61313 = alloca %struct.ScmObj*, align 8
%anf_45bind47310 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae50026, %struct.ScmObj* %anf_45bind47309)
store volatile %struct.ScmObj* %anf_45bind47310, %struct.ScmObj** %stackaddr$prim61313, align 8
%truthy$cmp61314 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47310)
%cmp$cmp61314 = icmp eq i64 %truthy$cmp61314, 1
br i1 %cmp$cmp61314, label %truebranch$cmp61314, label %falsebranch$cmp61314
truebranch$cmp61314:
%ae50030 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61315 = alloca %struct.ScmObj*, align 8
%cpsprim47507 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47155, %struct.ScmObj* %ae50030)
store volatile %struct.ScmObj* %cpsprim47507, %struct.ScmObj** %stackaddr$prim61315, align 8
%ae50032 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist59656$k475040 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61316 = alloca %struct.ScmObj*, align 8
%argslist59656$k475041 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47507, %struct.ScmObj* %argslist59656$k475040)
store volatile %struct.ScmObj* %argslist59656$k475041, %struct.ScmObj** %stackaddr$prim61316, align 8
%stackaddr$prim61317 = alloca %struct.ScmObj*, align 8
%argslist59656$k475042 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50032, %struct.ScmObj* %argslist59656$k475041)
store volatile %struct.ScmObj* %argslist59656$k475042, %struct.ScmObj** %stackaddr$prim61317, align 8
%clofunc61318 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47504)
musttail call tailcc void %clofunc61318(%struct.ScmObj* %k47504, %struct.ScmObj* %argslist59656$k475042)
ret void
falsebranch$cmp61314:
%ae50043 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61319 = alloca %struct.ScmObj*, align 8
%anf_45bind47311 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47155, %struct.ScmObj* %ae50043)
store volatile %struct.ScmObj* %anf_45bind47311, %struct.ScmObj** %stackaddr$prim61319, align 8
%stackaddr$prim61320 = alloca %struct.ScmObj*, align 8
%anf_45bind47312 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47311)
store volatile %struct.ScmObj* %anf_45bind47312, %struct.ScmObj** %stackaddr$prim61320, align 8
%ae50046 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61321 = alloca %struct.ScmObj*, align 8
%_95047160 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47155, %struct.ScmObj* %ae50046, %struct.ScmObj* %anf_45bind47312)
store volatile %struct.ScmObj* %_95047160, %struct.ScmObj** %stackaddr$prim61321, align 8
%ae50049 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61322 = alloca %struct.ScmObj*, align 8
%anf_45bind47313 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47156, %struct.ScmObj* %ae50049)
store volatile %struct.ScmObj* %anf_45bind47313, %struct.ScmObj** %stackaddr$prim61322, align 8
%ae50051 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim61323 = alloca %struct.ScmObj*, align 8
%anf_45bind47314 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind47313, %struct.ScmObj* %ae50051)
store volatile %struct.ScmObj* %anf_45bind47314, %struct.ScmObj** %stackaddr$prim61323, align 8
%ae50053 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61324 = alloca %struct.ScmObj*, align 8
%_95147159 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n47156, %struct.ScmObj* %ae50053, %struct.ScmObj* %anf_45bind47314)
store volatile %struct.ScmObj* %_95147159, %struct.ScmObj** %stackaddr$prim61324, align 8
%argslist59657$cc471570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61325 = alloca %struct.ScmObj*, align 8
%argslist59657$cc471571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47157, %struct.ScmObj* %argslist59657$cc471570)
store volatile %struct.ScmObj* %argslist59657$cc471571, %struct.ScmObj** %stackaddr$prim61325, align 8
%stackaddr$prim61326 = alloca %struct.ScmObj*, align 8
%argslist59657$cc471572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47504, %struct.ScmObj* %argslist59657$cc471571)
store volatile %struct.ScmObj* %argslist59657$cc471572, %struct.ScmObj** %stackaddr$prim61326, align 8
%clofunc61327 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47157)
musttail call tailcc void %clofunc61327(%struct.ScmObj* %cc47157, %struct.ScmObj* %argslist59657$cc471572)
ret void
}

define tailcc void @proc_clo$ae49883(%struct.ScmObj* %env$ae49883,%struct.ScmObj* %current_45args59658) {
%stackaddr$env-ref61328 = alloca %struct.ScmObj*, align 8
%n47156 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49883, i64 0)
store %struct.ScmObj* %n47156, %struct.ScmObj** %stackaddr$env-ref61328
%stackaddr$env-ref61329 = alloca %struct.ScmObj*, align 8
%lst47155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49883, i64 1)
store %struct.ScmObj* %lst47155, %struct.ScmObj** %stackaddr$env-ref61329
%stackaddr$env-ref61330 = alloca %struct.ScmObj*, align 8
%k47504 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49883, i64 2)
store %struct.ScmObj* %k47504, %struct.ScmObj** %stackaddr$env-ref61330
%stackaddr$prim61331 = alloca %struct.ScmObj*, align 8
%_95k47506 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59658)
store volatile %struct.ScmObj* %_95k47506, %struct.ScmObj** %stackaddr$prim61331, align 8
%stackaddr$prim61332 = alloca %struct.ScmObj*, align 8
%current_45args59659 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59658)
store volatile %struct.ScmObj* %current_45args59659, %struct.ScmObj** %stackaddr$prim61332, align 8
%stackaddr$prim61333 = alloca %struct.ScmObj*, align 8
%cc47157 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59659)
store volatile %struct.ScmObj* %cc47157, %struct.ScmObj** %stackaddr$prim61333, align 8
%ae49885 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61334 = alloca %struct.ScmObj*, align 8
%anf_45bind47309 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47156, %struct.ScmObj* %ae49885)
store volatile %struct.ScmObj* %anf_45bind47309, %struct.ScmObj** %stackaddr$prim61334, align 8
%ae49886 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61335 = alloca %struct.ScmObj*, align 8
%anf_45bind47310 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae49886, %struct.ScmObj* %anf_45bind47309)
store volatile %struct.ScmObj* %anf_45bind47310, %struct.ScmObj** %stackaddr$prim61335, align 8
%truthy$cmp61336 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47310)
%cmp$cmp61336 = icmp eq i64 %truthy$cmp61336, 1
br i1 %cmp$cmp61336, label %truebranch$cmp61336, label %falsebranch$cmp61336
truebranch$cmp61336:
%ae49890 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61337 = alloca %struct.ScmObj*, align 8
%cpsprim47507 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47155, %struct.ScmObj* %ae49890)
store volatile %struct.ScmObj* %cpsprim47507, %struct.ScmObj** %stackaddr$prim61337, align 8
%ae49892 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist59661$k475040 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61338 = alloca %struct.ScmObj*, align 8
%argslist59661$k475041 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47507, %struct.ScmObj* %argslist59661$k475040)
store volatile %struct.ScmObj* %argslist59661$k475041, %struct.ScmObj** %stackaddr$prim61338, align 8
%stackaddr$prim61339 = alloca %struct.ScmObj*, align 8
%argslist59661$k475042 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49892, %struct.ScmObj* %argslist59661$k475041)
store volatile %struct.ScmObj* %argslist59661$k475042, %struct.ScmObj** %stackaddr$prim61339, align 8
%clofunc61340 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47504)
musttail call tailcc void %clofunc61340(%struct.ScmObj* %k47504, %struct.ScmObj* %argslist59661$k475042)
ret void
falsebranch$cmp61336:
%ae49903 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61341 = alloca %struct.ScmObj*, align 8
%anf_45bind47311 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47155, %struct.ScmObj* %ae49903)
store volatile %struct.ScmObj* %anf_45bind47311, %struct.ScmObj** %stackaddr$prim61341, align 8
%stackaddr$prim61342 = alloca %struct.ScmObj*, align 8
%anf_45bind47312 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47311)
store volatile %struct.ScmObj* %anf_45bind47312, %struct.ScmObj** %stackaddr$prim61342, align 8
%ae49906 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61343 = alloca %struct.ScmObj*, align 8
%_95047160 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47155, %struct.ScmObj* %ae49906, %struct.ScmObj* %anf_45bind47312)
store volatile %struct.ScmObj* %_95047160, %struct.ScmObj** %stackaddr$prim61343, align 8
%ae49909 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61344 = alloca %struct.ScmObj*, align 8
%anf_45bind47313 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47156, %struct.ScmObj* %ae49909)
store volatile %struct.ScmObj* %anf_45bind47313, %struct.ScmObj** %stackaddr$prim61344, align 8
%ae49911 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim61345 = alloca %struct.ScmObj*, align 8
%anf_45bind47314 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind47313, %struct.ScmObj* %ae49911)
store volatile %struct.ScmObj* %anf_45bind47314, %struct.ScmObj** %stackaddr$prim61345, align 8
%ae49913 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61346 = alloca %struct.ScmObj*, align 8
%_95147159 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n47156, %struct.ScmObj* %ae49913, %struct.ScmObj* %anf_45bind47314)
store volatile %struct.ScmObj* %_95147159, %struct.ScmObj** %stackaddr$prim61346, align 8
%argslist59662$cc471570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61347 = alloca %struct.ScmObj*, align 8
%argslist59662$cc471571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47157, %struct.ScmObj* %argslist59662$cc471570)
store volatile %struct.ScmObj* %argslist59662$cc471571, %struct.ScmObj** %stackaddr$prim61347, align 8
%stackaddr$prim61348 = alloca %struct.ScmObj*, align 8
%argslist59662$cc471572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47504, %struct.ScmObj* %argslist59662$cc471571)
store volatile %struct.ScmObj* %argslist59662$cc471572, %struct.ScmObj** %stackaddr$prim61348, align 8
%clofunc61349 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47157)
musttail call tailcc void %clofunc61349(%struct.ScmObj* %cc47157, %struct.ScmObj* %argslist59662$cc471572)
ret void
}

define tailcc void @proc_clo$ae49868(%struct.ScmObj* %env$ae49868,%struct.ScmObj* %current_45args59664) {
%stackaddr$prim61350 = alloca %struct.ScmObj*, align 8
%k47508 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59664)
store volatile %struct.ScmObj* %k47508, %struct.ScmObj** %stackaddr$prim61350, align 8
%stackaddr$prim61351 = alloca %struct.ScmObj*, align 8
%current_45args59665 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59664)
store volatile %struct.ScmObj* %current_45args59665, %struct.ScmObj** %stackaddr$prim61351, align 8
%stackaddr$prim61352 = alloca %struct.ScmObj*, align 8
%u47158 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59665)
store volatile %struct.ScmObj* %u47158, %struct.ScmObj** %stackaddr$prim61352, align 8
%argslist59667$u471580 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61353 = alloca %struct.ScmObj*, align 8
%argslist59667$u471581 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u47158, %struct.ScmObj* %argslist59667$u471580)
store volatile %struct.ScmObj* %argslist59667$u471581, %struct.ScmObj** %stackaddr$prim61353, align 8
%stackaddr$prim61354 = alloca %struct.ScmObj*, align 8
%argslist59667$u471582 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47508, %struct.ScmObj* %argslist59667$u471581)
store volatile %struct.ScmObj* %argslist59667$u471582, %struct.ScmObj** %stackaddr$prim61354, align 8
%clofunc61355 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u47158)
musttail call tailcc void %clofunc61355(%struct.ScmObj* %u47158, %struct.ScmObj* %argslist59667$u471582)
ret void
}

define tailcc void @proc_clo$ae49445(%struct.ScmObj* %env$ae49445,%struct.ScmObj* %current_45args59670) {
%stackaddr$prim61356 = alloca %struct.ScmObj*, align 8
%k47509 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59670)
store volatile %struct.ScmObj* %k47509, %struct.ScmObj** %stackaddr$prim61356, align 8
%stackaddr$prim61357 = alloca %struct.ScmObj*, align 8
%current_45args59671 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59670)
store volatile %struct.ScmObj* %current_45args59671, %struct.ScmObj** %stackaddr$prim61357, align 8
%stackaddr$prim61358 = alloca %struct.ScmObj*, align 8
%a47162 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59671)
store volatile %struct.ScmObj* %a47162, %struct.ScmObj** %stackaddr$prim61358, align 8
%ae49446 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim61359 = alloca %struct.ScmObj*, align 8
%a47163 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49446, %struct.ScmObj* %a47162)
store volatile %struct.ScmObj* %a47163, %struct.ScmObj** %stackaddr$prim61359, align 8
%stackaddr$makeclosure61360 = alloca %struct.ScmObj*, align 8
%fptrToInt61361 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49448 to i64
%ae49448 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt61361)
store volatile %struct.ScmObj* %ae49448, %struct.ScmObj** %stackaddr$makeclosure61360, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49448, %struct.ScmObj* %k47509, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49448, %struct.ScmObj* %a47163, i64 1)
%ae49449 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61362 = alloca %struct.ScmObj*, align 8
%fptrToInt61363 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49450 to i64
%ae49450 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt61363)
store volatile %struct.ScmObj* %ae49450, %struct.ScmObj** %stackaddr$makeclosure61362, align 8
%argslist59693$ae494480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61364 = alloca %struct.ScmObj*, align 8
%argslist59693$ae494481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49450, %struct.ScmObj* %argslist59693$ae494480)
store volatile %struct.ScmObj* %argslist59693$ae494481, %struct.ScmObj** %stackaddr$prim61364, align 8
%stackaddr$prim61365 = alloca %struct.ScmObj*, align 8
%argslist59693$ae494482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49449, %struct.ScmObj* %argslist59693$ae494481)
store volatile %struct.ScmObj* %argslist59693$ae494482, %struct.ScmObj** %stackaddr$prim61365, align 8
%clofunc61366 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49448)
musttail call tailcc void %clofunc61366(%struct.ScmObj* %ae49448, %struct.ScmObj* %argslist59693$ae494482)
ret void
}

define tailcc void @proc_clo$ae49448(%struct.ScmObj* %env$ae49448,%struct.ScmObj* %current_45args59673) {
%stackaddr$env-ref61367 = alloca %struct.ScmObj*, align 8
%k47509 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49448, i64 0)
store %struct.ScmObj* %k47509, %struct.ScmObj** %stackaddr$env-ref61367
%stackaddr$env-ref61368 = alloca %struct.ScmObj*, align 8
%a47163 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49448, i64 1)
store %struct.ScmObj* %a47163, %struct.ScmObj** %stackaddr$env-ref61368
%stackaddr$prim61369 = alloca %struct.ScmObj*, align 8
%_95k47510 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59673)
store volatile %struct.ScmObj* %_95k47510, %struct.ScmObj** %stackaddr$prim61369, align 8
%stackaddr$prim61370 = alloca %struct.ScmObj*, align 8
%current_45args59674 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59673)
store volatile %struct.ScmObj* %current_45args59674, %struct.ScmObj** %stackaddr$prim61370, align 8
%stackaddr$prim61371 = alloca %struct.ScmObj*, align 8
%anf_45bind47300 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59674)
store volatile %struct.ScmObj* %anf_45bind47300, %struct.ScmObj** %stackaddr$prim61371, align 8
%stackaddr$makeclosure61372 = alloca %struct.ScmObj*, align 8
%fptrToInt61373 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49467 to i64
%ae49467 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt61373)
store volatile %struct.ScmObj* %ae49467, %struct.ScmObj** %stackaddr$makeclosure61372, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49467, %struct.ScmObj* %k47509, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49467, %struct.ScmObj* %a47163, i64 1)
%stackaddr$makeclosure61374 = alloca %struct.ScmObj*, align 8
%fptrToInt61375 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49468 to i64
%ae49468 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt61375)
store volatile %struct.ScmObj* %ae49468, %struct.ScmObj** %stackaddr$makeclosure61374, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49468, %struct.ScmObj* %k47509, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49468, %struct.ScmObj* %a47163, i64 1)
%argslist59688$anf_45bind473000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61376 = alloca %struct.ScmObj*, align 8
%argslist59688$anf_45bind473001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49468, %struct.ScmObj* %argslist59688$anf_45bind473000)
store volatile %struct.ScmObj* %argslist59688$anf_45bind473001, %struct.ScmObj** %stackaddr$prim61376, align 8
%stackaddr$prim61377 = alloca %struct.ScmObj*, align 8
%argslist59688$anf_45bind473002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49467, %struct.ScmObj* %argslist59688$anf_45bind473001)
store volatile %struct.ScmObj* %argslist59688$anf_45bind473002, %struct.ScmObj** %stackaddr$prim61377, align 8
%clofunc61378 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47300)
musttail call tailcc void %clofunc61378(%struct.ScmObj* %anf_45bind47300, %struct.ScmObj* %argslist59688$anf_45bind473002)
ret void
}

define tailcc void @proc_clo$ae49467(%struct.ScmObj* %env$ae49467,%struct.ScmObj* %current_45args59676) {
%stackaddr$env-ref61379 = alloca %struct.ScmObj*, align 8
%k47509 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49467, i64 0)
store %struct.ScmObj* %k47509, %struct.ScmObj** %stackaddr$env-ref61379
%stackaddr$env-ref61380 = alloca %struct.ScmObj*, align 8
%a47163 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49467, i64 1)
store %struct.ScmObj* %a47163, %struct.ScmObj** %stackaddr$env-ref61380
%stackaddr$prim61381 = alloca %struct.ScmObj*, align 8
%_95k47511 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59676)
store volatile %struct.ScmObj* %_95k47511, %struct.ScmObj** %stackaddr$prim61381, align 8
%stackaddr$prim61382 = alloca %struct.ScmObj*, align 8
%current_45args59677 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59676)
store volatile %struct.ScmObj* %current_45args59677, %struct.ScmObj** %stackaddr$prim61382, align 8
%stackaddr$prim61383 = alloca %struct.ScmObj*, align 8
%cc47164 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59677)
store volatile %struct.ScmObj* %cc47164, %struct.ScmObj** %stackaddr$prim61383, align 8
%ae49583 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61384 = alloca %struct.ScmObj*, align 8
%anf_45bind47301 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47163, %struct.ScmObj* %ae49583)
store volatile %struct.ScmObj* %anf_45bind47301, %struct.ScmObj** %stackaddr$prim61384, align 8
%stackaddr$prim61385 = alloca %struct.ScmObj*, align 8
%anf_45bind47302 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47301)
store volatile %struct.ScmObj* %anf_45bind47302, %struct.ScmObj** %stackaddr$prim61385, align 8
%truthy$cmp61386 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47302)
%cmp$cmp61386 = icmp eq i64 %truthy$cmp61386, 1
br i1 %cmp$cmp61386, label %truebranch$cmp61386, label %falsebranch$cmp61386
truebranch$cmp61386:
%ae49587 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49588 = call %struct.ScmObj* @const_init_true()
%argslist59679$k475090 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61387 = alloca %struct.ScmObj*, align 8
%argslist59679$k475091 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49588, %struct.ScmObj* %argslist59679$k475090)
store volatile %struct.ScmObj* %argslist59679$k475091, %struct.ScmObj** %stackaddr$prim61387, align 8
%stackaddr$prim61388 = alloca %struct.ScmObj*, align 8
%argslist59679$k475092 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49587, %struct.ScmObj* %argslist59679$k475091)
store volatile %struct.ScmObj* %argslist59679$k475092, %struct.ScmObj** %stackaddr$prim61388, align 8
%clofunc61389 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47509)
musttail call tailcc void %clofunc61389(%struct.ScmObj* %k47509, %struct.ScmObj* %argslist59679$k475092)
ret void
falsebranch$cmp61386:
%ae49596 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61390 = alloca %struct.ScmObj*, align 8
%anf_45bind47303 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47163, %struct.ScmObj* %ae49596)
store volatile %struct.ScmObj* %anf_45bind47303, %struct.ScmObj** %stackaddr$prim61390, align 8
%stackaddr$prim61391 = alloca %struct.ScmObj*, align 8
%anf_45bind47304 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind47303)
store volatile %struct.ScmObj* %anf_45bind47304, %struct.ScmObj** %stackaddr$prim61391, align 8
%truthy$cmp61392 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47304)
%cmp$cmp61392 = icmp eq i64 %truthy$cmp61392, 1
br i1 %cmp$cmp61392, label %truebranch$cmp61392, label %falsebranch$cmp61392
truebranch$cmp61392:
%ae49600 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61393 = alloca %struct.ScmObj*, align 8
%anf_45bind47305 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47163, %struct.ScmObj* %ae49600)
store volatile %struct.ScmObj* %anf_45bind47305, %struct.ScmObj** %stackaddr$prim61393, align 8
%stackaddr$prim61394 = alloca %struct.ScmObj*, align 8
%b47166 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47305)
store volatile %struct.ScmObj* %b47166, %struct.ScmObj** %stackaddr$prim61394, align 8
%ae49603 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61395 = alloca %struct.ScmObj*, align 8
%anf_45bind47306 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47163, %struct.ScmObj* %ae49603)
store volatile %struct.ScmObj* %anf_45bind47306, %struct.ScmObj** %stackaddr$prim61395, align 8
%stackaddr$prim61396 = alloca %struct.ScmObj*, align 8
%anf_45bind47307 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47306)
store volatile %struct.ScmObj* %anf_45bind47307, %struct.ScmObj** %stackaddr$prim61396, align 8
%ae49606 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61397 = alloca %struct.ScmObj*, align 8
%_95047167 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a47163, %struct.ScmObj* %ae49606, %struct.ScmObj* %anf_45bind47307)
store volatile %struct.ScmObj* %_95047167, %struct.ScmObj** %stackaddr$prim61397, align 8
%argslist59680$cc471640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61398 = alloca %struct.ScmObj*, align 8
%argslist59680$cc471641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47164, %struct.ScmObj* %argslist59680$cc471640)
store volatile %struct.ScmObj* %argslist59680$cc471641, %struct.ScmObj** %stackaddr$prim61398, align 8
%stackaddr$prim61399 = alloca %struct.ScmObj*, align 8
%argslist59680$cc471642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47509, %struct.ScmObj* %argslist59680$cc471641)
store volatile %struct.ScmObj* %argslist59680$cc471642, %struct.ScmObj** %stackaddr$prim61399, align 8
%clofunc61400 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47164)
musttail call tailcc void %clofunc61400(%struct.ScmObj* %cc47164, %struct.ScmObj* %argslist59680$cc471642)
ret void
falsebranch$cmp61392:
%ae49639 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49640 = call %struct.ScmObj* @const_init_false()
%argslist59681$k475090 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61401 = alloca %struct.ScmObj*, align 8
%argslist59681$k475091 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49640, %struct.ScmObj* %argslist59681$k475090)
store volatile %struct.ScmObj* %argslist59681$k475091, %struct.ScmObj** %stackaddr$prim61401, align 8
%stackaddr$prim61402 = alloca %struct.ScmObj*, align 8
%argslist59681$k475092 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49639, %struct.ScmObj* %argslist59681$k475091)
store volatile %struct.ScmObj* %argslist59681$k475092, %struct.ScmObj** %stackaddr$prim61402, align 8
%clofunc61403 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47509)
musttail call tailcc void %clofunc61403(%struct.ScmObj* %k47509, %struct.ScmObj* %argslist59681$k475092)
ret void
}

define tailcc void @proc_clo$ae49468(%struct.ScmObj* %env$ae49468,%struct.ScmObj* %current_45args59682) {
%stackaddr$env-ref61404 = alloca %struct.ScmObj*, align 8
%k47509 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49468, i64 0)
store %struct.ScmObj* %k47509, %struct.ScmObj** %stackaddr$env-ref61404
%stackaddr$env-ref61405 = alloca %struct.ScmObj*, align 8
%a47163 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49468, i64 1)
store %struct.ScmObj* %a47163, %struct.ScmObj** %stackaddr$env-ref61405
%stackaddr$prim61406 = alloca %struct.ScmObj*, align 8
%_95k47511 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59682)
store volatile %struct.ScmObj* %_95k47511, %struct.ScmObj** %stackaddr$prim61406, align 8
%stackaddr$prim61407 = alloca %struct.ScmObj*, align 8
%current_45args59683 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59682)
store volatile %struct.ScmObj* %current_45args59683, %struct.ScmObj** %stackaddr$prim61407, align 8
%stackaddr$prim61408 = alloca %struct.ScmObj*, align 8
%cc47164 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59683)
store volatile %struct.ScmObj* %cc47164, %struct.ScmObj** %stackaddr$prim61408, align 8
%ae49470 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61409 = alloca %struct.ScmObj*, align 8
%anf_45bind47301 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47163, %struct.ScmObj* %ae49470)
store volatile %struct.ScmObj* %anf_45bind47301, %struct.ScmObj** %stackaddr$prim61409, align 8
%stackaddr$prim61410 = alloca %struct.ScmObj*, align 8
%anf_45bind47302 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47301)
store volatile %struct.ScmObj* %anf_45bind47302, %struct.ScmObj** %stackaddr$prim61410, align 8
%truthy$cmp61411 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47302)
%cmp$cmp61411 = icmp eq i64 %truthy$cmp61411, 1
br i1 %cmp$cmp61411, label %truebranch$cmp61411, label %falsebranch$cmp61411
truebranch$cmp61411:
%ae49474 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49475 = call %struct.ScmObj* @const_init_true()
%argslist59685$k475090 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61412 = alloca %struct.ScmObj*, align 8
%argslist59685$k475091 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49475, %struct.ScmObj* %argslist59685$k475090)
store volatile %struct.ScmObj* %argslist59685$k475091, %struct.ScmObj** %stackaddr$prim61412, align 8
%stackaddr$prim61413 = alloca %struct.ScmObj*, align 8
%argslist59685$k475092 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49474, %struct.ScmObj* %argslist59685$k475091)
store volatile %struct.ScmObj* %argslist59685$k475092, %struct.ScmObj** %stackaddr$prim61413, align 8
%clofunc61414 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47509)
musttail call tailcc void %clofunc61414(%struct.ScmObj* %k47509, %struct.ScmObj* %argslist59685$k475092)
ret void
falsebranch$cmp61411:
%ae49483 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61415 = alloca %struct.ScmObj*, align 8
%anf_45bind47303 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47163, %struct.ScmObj* %ae49483)
store volatile %struct.ScmObj* %anf_45bind47303, %struct.ScmObj** %stackaddr$prim61415, align 8
%stackaddr$prim61416 = alloca %struct.ScmObj*, align 8
%anf_45bind47304 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind47303)
store volatile %struct.ScmObj* %anf_45bind47304, %struct.ScmObj** %stackaddr$prim61416, align 8
%truthy$cmp61417 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47304)
%cmp$cmp61417 = icmp eq i64 %truthy$cmp61417, 1
br i1 %cmp$cmp61417, label %truebranch$cmp61417, label %falsebranch$cmp61417
truebranch$cmp61417:
%ae49487 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61418 = alloca %struct.ScmObj*, align 8
%anf_45bind47305 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47163, %struct.ScmObj* %ae49487)
store volatile %struct.ScmObj* %anf_45bind47305, %struct.ScmObj** %stackaddr$prim61418, align 8
%stackaddr$prim61419 = alloca %struct.ScmObj*, align 8
%b47166 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47305)
store volatile %struct.ScmObj* %b47166, %struct.ScmObj** %stackaddr$prim61419, align 8
%ae49490 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61420 = alloca %struct.ScmObj*, align 8
%anf_45bind47306 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47163, %struct.ScmObj* %ae49490)
store volatile %struct.ScmObj* %anf_45bind47306, %struct.ScmObj** %stackaddr$prim61420, align 8
%stackaddr$prim61421 = alloca %struct.ScmObj*, align 8
%anf_45bind47307 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47306)
store volatile %struct.ScmObj* %anf_45bind47307, %struct.ScmObj** %stackaddr$prim61421, align 8
%ae49493 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61422 = alloca %struct.ScmObj*, align 8
%_95047167 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a47163, %struct.ScmObj* %ae49493, %struct.ScmObj* %anf_45bind47307)
store volatile %struct.ScmObj* %_95047167, %struct.ScmObj** %stackaddr$prim61422, align 8
%argslist59686$cc471640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61423 = alloca %struct.ScmObj*, align 8
%argslist59686$cc471641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47164, %struct.ScmObj* %argslist59686$cc471640)
store volatile %struct.ScmObj* %argslist59686$cc471641, %struct.ScmObj** %stackaddr$prim61423, align 8
%stackaddr$prim61424 = alloca %struct.ScmObj*, align 8
%argslist59686$cc471642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47509, %struct.ScmObj* %argslist59686$cc471641)
store volatile %struct.ScmObj* %argslist59686$cc471642, %struct.ScmObj** %stackaddr$prim61424, align 8
%clofunc61425 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47164)
musttail call tailcc void %clofunc61425(%struct.ScmObj* %cc47164, %struct.ScmObj* %argslist59686$cc471642)
ret void
falsebranch$cmp61417:
%ae49526 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49527 = call %struct.ScmObj* @const_init_false()
%argslist59687$k475090 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61426 = alloca %struct.ScmObj*, align 8
%argslist59687$k475091 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49527, %struct.ScmObj* %argslist59687$k475090)
store volatile %struct.ScmObj* %argslist59687$k475091, %struct.ScmObj** %stackaddr$prim61426, align 8
%stackaddr$prim61427 = alloca %struct.ScmObj*, align 8
%argslist59687$k475092 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49526, %struct.ScmObj* %argslist59687$k475091)
store volatile %struct.ScmObj* %argslist59687$k475092, %struct.ScmObj** %stackaddr$prim61427, align 8
%clofunc61428 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47509)
musttail call tailcc void %clofunc61428(%struct.ScmObj* %k47509, %struct.ScmObj* %argslist59687$k475092)
ret void
}

define tailcc void @proc_clo$ae49450(%struct.ScmObj* %env$ae49450,%struct.ScmObj* %current_45args59689) {
%stackaddr$prim61429 = alloca %struct.ScmObj*, align 8
%k47512 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59689)
store volatile %struct.ScmObj* %k47512, %struct.ScmObj** %stackaddr$prim61429, align 8
%stackaddr$prim61430 = alloca %struct.ScmObj*, align 8
%current_45args59690 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59689)
store volatile %struct.ScmObj* %current_45args59690, %struct.ScmObj** %stackaddr$prim61430, align 8
%stackaddr$prim61431 = alloca %struct.ScmObj*, align 8
%k47165 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59690)
store volatile %struct.ScmObj* %k47165, %struct.ScmObj** %stackaddr$prim61431, align 8
%ae49452 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist59692$k475120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61432 = alloca %struct.ScmObj*, align 8
%argslist59692$k475121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47165, %struct.ScmObj* %argslist59692$k475120)
store volatile %struct.ScmObj* %argslist59692$k475121, %struct.ScmObj** %stackaddr$prim61432, align 8
%stackaddr$prim61433 = alloca %struct.ScmObj*, align 8
%argslist59692$k475122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49452, %struct.ScmObj* %argslist59692$k475121)
store volatile %struct.ScmObj* %argslist59692$k475122, %struct.ScmObj** %stackaddr$prim61433, align 8
%clofunc61434 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47512)
musttail call tailcc void %clofunc61434(%struct.ScmObj* %k47512, %struct.ScmObj* %argslist59692$k475122)
ret void
}

define tailcc void @proc_clo$ae49373(%struct.ScmObj* %env$ae49373,%struct.ScmObj* %current_45args59695) {
%stackaddr$env-ref61435 = alloca %struct.ScmObj*, align 8
%_37append47169 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49373, i64 0)
store %struct.ScmObj* %_37append47169, %struct.ScmObj** %stackaddr$env-ref61435
%stackaddr$prim61436 = alloca %struct.ScmObj*, align 8
%k47513 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59695)
store volatile %struct.ScmObj* %k47513, %struct.ScmObj** %stackaddr$prim61436, align 8
%stackaddr$prim61437 = alloca %struct.ScmObj*, align 8
%current_45args59696 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59695)
store volatile %struct.ScmObj* %current_45args59696, %struct.ScmObj** %stackaddr$prim61437, align 8
%stackaddr$prim61438 = alloca %struct.ScmObj*, align 8
%ls047172 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59696)
store volatile %struct.ScmObj* %ls047172, %struct.ScmObj** %stackaddr$prim61438, align 8
%stackaddr$prim61439 = alloca %struct.ScmObj*, align 8
%current_45args59697 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59696)
store volatile %struct.ScmObj* %current_45args59697, %struct.ScmObj** %stackaddr$prim61439, align 8
%stackaddr$prim61440 = alloca %struct.ScmObj*, align 8
%ls147171 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59697)
store volatile %struct.ScmObj* %ls147171, %struct.ScmObj** %stackaddr$prim61440, align 8
%stackaddr$prim61441 = alloca %struct.ScmObj*, align 8
%anf_45bind47294 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls047172)
store volatile %struct.ScmObj* %anf_45bind47294, %struct.ScmObj** %stackaddr$prim61441, align 8
%truthy$cmp61442 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47294)
%cmp$cmp61442 = icmp eq i64 %truthy$cmp61442, 1
br i1 %cmp$cmp61442, label %truebranch$cmp61442, label %falsebranch$cmp61442
truebranch$cmp61442:
%ae49377 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist59699$k475130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61443 = alloca %struct.ScmObj*, align 8
%argslist59699$k475131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls147171, %struct.ScmObj* %argslist59699$k475130)
store volatile %struct.ScmObj* %argslist59699$k475131, %struct.ScmObj** %stackaddr$prim61443, align 8
%stackaddr$prim61444 = alloca %struct.ScmObj*, align 8
%argslist59699$k475132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49377, %struct.ScmObj* %argslist59699$k475131)
store volatile %struct.ScmObj* %argslist59699$k475132, %struct.ScmObj** %stackaddr$prim61444, align 8
%clofunc61445 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47513)
musttail call tailcc void %clofunc61445(%struct.ScmObj* %k47513, %struct.ScmObj* %argslist59699$k475132)
ret void
falsebranch$cmp61442:
%stackaddr$prim61446 = alloca %struct.ScmObj*, align 8
%anf_45bind47295 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls047172)
store volatile %struct.ScmObj* %anf_45bind47295, %struct.ScmObj** %stackaddr$prim61446, align 8
%ae49384 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim61447 = alloca %struct.ScmObj*, align 8
%anf_45bind47296 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append47169, %struct.ScmObj* %ae49384)
store volatile %struct.ScmObj* %anf_45bind47296, %struct.ScmObj** %stackaddr$prim61447, align 8
%stackaddr$prim61448 = alloca %struct.ScmObj*, align 8
%anf_45bind47297 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls047172)
store volatile %struct.ScmObj* %anf_45bind47297, %struct.ScmObj** %stackaddr$prim61448, align 8
%stackaddr$makeclosure61449 = alloca %struct.ScmObj*, align 8
%fptrToInt61450 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49387 to i64
%ae49387 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt61450)
store volatile %struct.ScmObj* %ae49387, %struct.ScmObj** %stackaddr$makeclosure61449, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49387, %struct.ScmObj* %anf_45bind47295, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49387, %struct.ScmObj* %k47513, i64 1)
%argslist59704$anf_45bind472960 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61451 = alloca %struct.ScmObj*, align 8
%argslist59704$anf_45bind472961 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls147171, %struct.ScmObj* %argslist59704$anf_45bind472960)
store volatile %struct.ScmObj* %argslist59704$anf_45bind472961, %struct.ScmObj** %stackaddr$prim61451, align 8
%stackaddr$prim61452 = alloca %struct.ScmObj*, align 8
%argslist59704$anf_45bind472962 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47297, %struct.ScmObj* %argslist59704$anf_45bind472961)
store volatile %struct.ScmObj* %argslist59704$anf_45bind472962, %struct.ScmObj** %stackaddr$prim61452, align 8
%stackaddr$prim61453 = alloca %struct.ScmObj*, align 8
%argslist59704$anf_45bind472963 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49387, %struct.ScmObj* %argslist59704$anf_45bind472962)
store volatile %struct.ScmObj* %argslist59704$anf_45bind472963, %struct.ScmObj** %stackaddr$prim61453, align 8
%clofunc61454 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47296)
musttail call tailcc void %clofunc61454(%struct.ScmObj* %anf_45bind47296, %struct.ScmObj* %argslist59704$anf_45bind472963)
ret void
}

define tailcc void @proc_clo$ae49387(%struct.ScmObj* %env$ae49387,%struct.ScmObj* %current_45args59700) {
%stackaddr$env-ref61455 = alloca %struct.ScmObj*, align 8
%anf_45bind47295 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49387, i64 0)
store %struct.ScmObj* %anf_45bind47295, %struct.ScmObj** %stackaddr$env-ref61455
%stackaddr$env-ref61456 = alloca %struct.ScmObj*, align 8
%k47513 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49387, i64 1)
store %struct.ScmObj* %k47513, %struct.ScmObj** %stackaddr$env-ref61456
%stackaddr$prim61457 = alloca %struct.ScmObj*, align 8
%_95k47514 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59700)
store volatile %struct.ScmObj* %_95k47514, %struct.ScmObj** %stackaddr$prim61457, align 8
%stackaddr$prim61458 = alloca %struct.ScmObj*, align 8
%current_45args59701 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59700)
store volatile %struct.ScmObj* %current_45args59701, %struct.ScmObj** %stackaddr$prim61458, align 8
%stackaddr$prim61459 = alloca %struct.ScmObj*, align 8
%anf_45bind47298 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59701)
store volatile %struct.ScmObj* %anf_45bind47298, %struct.ScmObj** %stackaddr$prim61459, align 8
%stackaddr$prim61460 = alloca %struct.ScmObj*, align 8
%cpsprim47515 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47295, %struct.ScmObj* %anf_45bind47298)
store volatile %struct.ScmObj* %cpsprim47515, %struct.ScmObj** %stackaddr$prim61460, align 8
%ae49393 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist59703$k475130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61461 = alloca %struct.ScmObj*, align 8
%argslist59703$k475131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47515, %struct.ScmObj* %argslist59703$k475130)
store volatile %struct.ScmObj* %argslist59703$k475131, %struct.ScmObj** %stackaddr$prim61461, align 8
%stackaddr$prim61462 = alloca %struct.ScmObj*, align 8
%argslist59703$k475132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49393, %struct.ScmObj* %argslist59703$k475131)
store volatile %struct.ScmObj* %argslist59703$k475132, %struct.ScmObj** %stackaddr$prim61462, align 8
%clofunc61463 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47513)
musttail call tailcc void %clofunc61463(%struct.ScmObj* %k47513, %struct.ScmObj* %argslist59703$k475132)
ret void
}

define tailcc void @proc_clo$ae49347(%struct.ScmObj* %env$ae49347,%struct.ScmObj* %current_45args59706) {
%stackaddr$prim61464 = alloca %struct.ScmObj*, align 8
%k47516 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59706)
store volatile %struct.ScmObj* %k47516, %struct.ScmObj** %stackaddr$prim61464, align 8
%stackaddr$prim61465 = alloca %struct.ScmObj*, align 8
%current_45args59707 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59706)
store volatile %struct.ScmObj* %current_45args59707, %struct.ScmObj** %stackaddr$prim61465, align 8
%stackaddr$prim61466 = alloca %struct.ScmObj*, align 8
%a47175 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59707)
store volatile %struct.ScmObj* %a47175, %struct.ScmObj** %stackaddr$prim61466, align 8
%stackaddr$prim61467 = alloca %struct.ScmObj*, align 8
%current_45args59708 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59707)
store volatile %struct.ScmObj* %current_45args59708, %struct.ScmObj** %stackaddr$prim61467, align 8
%stackaddr$prim61468 = alloca %struct.ScmObj*, align 8
%b47174 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59708)
store volatile %struct.ScmObj* %b47174, %struct.ScmObj** %stackaddr$prim61468, align 8
%stackaddr$prim61469 = alloca %struct.ScmObj*, align 8
%anf_45bind47293 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a47175, %struct.ScmObj* %b47174)
store volatile %struct.ScmObj* %anf_45bind47293, %struct.ScmObj** %stackaddr$prim61469, align 8
%stackaddr$prim61470 = alloca %struct.ScmObj*, align 8
%cpsprim47517 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind47293)
store volatile %struct.ScmObj* %cpsprim47517, %struct.ScmObj** %stackaddr$prim61470, align 8
%ae49352 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist59710$k475160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61471 = alloca %struct.ScmObj*, align 8
%argslist59710$k475161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47517, %struct.ScmObj* %argslist59710$k475160)
store volatile %struct.ScmObj* %argslist59710$k475161, %struct.ScmObj** %stackaddr$prim61471, align 8
%stackaddr$prim61472 = alloca %struct.ScmObj*, align 8
%argslist59710$k475162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49352, %struct.ScmObj* %argslist59710$k475161)
store volatile %struct.ScmObj* %argslist59710$k475162, %struct.ScmObj** %stackaddr$prim61472, align 8
%clofunc61473 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47516)
musttail call tailcc void %clofunc61473(%struct.ScmObj* %k47516, %struct.ScmObj* %argslist59710$k475162)
ret void
}

define tailcc void @proc_clo$ae49323(%struct.ScmObj* %env$ae49323,%struct.ScmObj* %current_45args59712) {
%stackaddr$prim61474 = alloca %struct.ScmObj*, align 8
%k47518 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59712)
store volatile %struct.ScmObj* %k47518, %struct.ScmObj** %stackaddr$prim61474, align 8
%stackaddr$prim61475 = alloca %struct.ScmObj*, align 8
%current_45args59713 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59712)
store volatile %struct.ScmObj* %current_45args59713, %struct.ScmObj** %stackaddr$prim61475, align 8
%stackaddr$prim61476 = alloca %struct.ScmObj*, align 8
%a47178 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59713)
store volatile %struct.ScmObj* %a47178, %struct.ScmObj** %stackaddr$prim61476, align 8
%stackaddr$prim61477 = alloca %struct.ScmObj*, align 8
%current_45args59714 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59713)
store volatile %struct.ScmObj* %current_45args59714, %struct.ScmObj** %stackaddr$prim61477, align 8
%stackaddr$prim61478 = alloca %struct.ScmObj*, align 8
%b47177 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59714)
store volatile %struct.ScmObj* %b47177, %struct.ScmObj** %stackaddr$prim61478, align 8
%stackaddr$prim61479 = alloca %struct.ScmObj*, align 8
%anf_45bind47292 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a47178, %struct.ScmObj* %b47177)
store volatile %struct.ScmObj* %anf_45bind47292, %struct.ScmObj** %stackaddr$prim61479, align 8
%stackaddr$prim61480 = alloca %struct.ScmObj*, align 8
%cpsprim47519 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind47292)
store volatile %struct.ScmObj* %cpsprim47519, %struct.ScmObj** %stackaddr$prim61480, align 8
%ae49328 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist59716$k475180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61481 = alloca %struct.ScmObj*, align 8
%argslist59716$k475181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47519, %struct.ScmObj* %argslist59716$k475180)
store volatile %struct.ScmObj* %argslist59716$k475181, %struct.ScmObj** %stackaddr$prim61481, align 8
%stackaddr$prim61482 = alloca %struct.ScmObj*, align 8
%argslist59716$k475182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49328, %struct.ScmObj* %argslist59716$k475181)
store volatile %struct.ScmObj* %argslist59716$k475182, %struct.ScmObj** %stackaddr$prim61482, align 8
%clofunc61483 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47518)
musttail call tailcc void %clofunc61483(%struct.ScmObj* %k47518, %struct.ScmObj* %argslist59716$k475182)
ret void
}

define tailcc void @proc_clo$ae48929(%struct.ScmObj* %env$ae48929,%struct.ScmObj* %current_45args59719) {
%stackaddr$env-ref61484 = alloca %struct.ScmObj*, align 8
%_37foldr47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48929, i64 0)
store %struct.ScmObj* %_37foldr47102, %struct.ScmObj** %stackaddr$env-ref61484
%stackaddr$env-ref61485 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48929, i64 1)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref61485
%stackaddr$env-ref61486 = alloca %struct.ScmObj*, align 8
%_37map147128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48929, i64 2)
store %struct.ScmObj* %_37map147128, %struct.ScmObj** %stackaddr$env-ref61486
%stackaddr$prim61487 = alloca %struct.ScmObj*, align 8
%k47520 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59719)
store volatile %struct.ScmObj* %k47520, %struct.ScmObj** %stackaddr$prim61487, align 8
%stackaddr$prim61488 = alloca %struct.ScmObj*, align 8
%current_45args59720 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59719)
store volatile %struct.ScmObj* %current_45args59720, %struct.ScmObj** %stackaddr$prim61488, align 8
%stackaddr$prim61489 = alloca %struct.ScmObj*, align 8
%_37foldl47180 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59720)
store volatile %struct.ScmObj* %_37foldl47180, %struct.ScmObj** %stackaddr$prim61489, align 8
%ae48931 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61490 = alloca %struct.ScmObj*, align 8
%fptrToInt61491 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48932 to i64
%ae48932 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt61491)
store volatile %struct.ScmObj* %ae48932, %struct.ScmObj** %stackaddr$makeclosure61490, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48932, %struct.ScmObj* %_37foldr47102, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48932, %struct.ScmObj* %_37foldl47180, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48932, %struct.ScmObj* %_37foldr147097, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48932, %struct.ScmObj* %_37map147128, i64 3)
%argslist59777$k475200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61492 = alloca %struct.ScmObj*, align 8
%argslist59777$k475201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48932, %struct.ScmObj* %argslist59777$k475200)
store volatile %struct.ScmObj* %argslist59777$k475201, %struct.ScmObj** %stackaddr$prim61492, align 8
%stackaddr$prim61493 = alloca %struct.ScmObj*, align 8
%argslist59777$k475202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48931, %struct.ScmObj* %argslist59777$k475201)
store volatile %struct.ScmObj* %argslist59777$k475202, %struct.ScmObj** %stackaddr$prim61493, align 8
%clofunc61494 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47520)
musttail call tailcc void %clofunc61494(%struct.ScmObj* %k47520, %struct.ScmObj* %argslist59777$k475202)
ret void
}

define tailcc void @proc_clo$ae48932(%struct.ScmObj* %env$ae48932,%struct.ScmObj* %args4718147521) {
%stackaddr$env-ref61495 = alloca %struct.ScmObj*, align 8
%_37foldr47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48932, i64 0)
store %struct.ScmObj* %_37foldr47102, %struct.ScmObj** %stackaddr$env-ref61495
%stackaddr$env-ref61496 = alloca %struct.ScmObj*, align 8
%_37foldl47180 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48932, i64 1)
store %struct.ScmObj* %_37foldl47180, %struct.ScmObj** %stackaddr$env-ref61496
%stackaddr$env-ref61497 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48932, i64 2)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref61497
%stackaddr$env-ref61498 = alloca %struct.ScmObj*, align 8
%_37map147128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48932, i64 3)
store %struct.ScmObj* %_37map147128, %struct.ScmObj** %stackaddr$env-ref61498
%stackaddr$prim61499 = alloca %struct.ScmObj*, align 8
%k47522 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4718147521)
store volatile %struct.ScmObj* %k47522, %struct.ScmObj** %stackaddr$prim61499, align 8
%stackaddr$prim61500 = alloca %struct.ScmObj*, align 8
%args47181 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4718147521)
store volatile %struct.ScmObj* %args47181, %struct.ScmObj** %stackaddr$prim61500, align 8
%stackaddr$prim61501 = alloca %struct.ScmObj*, align 8
%f47184 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47181)
store volatile %struct.ScmObj* %f47184, %struct.ScmObj** %stackaddr$prim61501, align 8
%stackaddr$prim61502 = alloca %struct.ScmObj*, align 8
%anf_45bind47280 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47181)
store volatile %struct.ScmObj* %anf_45bind47280, %struct.ScmObj** %stackaddr$prim61502, align 8
%stackaddr$prim61503 = alloca %struct.ScmObj*, align 8
%acc47183 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47280)
store volatile %struct.ScmObj* %acc47183, %struct.ScmObj** %stackaddr$prim61503, align 8
%stackaddr$prim61504 = alloca %struct.ScmObj*, align 8
%anf_45bind47281 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47181)
store volatile %struct.ScmObj* %anf_45bind47281, %struct.ScmObj** %stackaddr$prim61504, align 8
%stackaddr$prim61505 = alloca %struct.ScmObj*, align 8
%lsts47182 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47281)
store volatile %struct.ScmObj* %lsts47182, %struct.ScmObj** %stackaddr$prim61505, align 8
%stackaddr$makeclosure61506 = alloca %struct.ScmObj*, align 8
%fptrToInt61507 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48940 to i64
%ae48940 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt61507)
store volatile %struct.ScmObj* %ae48940, %struct.ScmObj** %stackaddr$makeclosure61506, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48940, %struct.ScmObj* %lsts47182, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48940, %struct.ScmObj* %_37foldr47102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48940, %struct.ScmObj* %k47522, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48940, %struct.ScmObj* %f47184, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48940, %struct.ScmObj* %acc47183, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48940, %struct.ScmObj* %_37foldl47180, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48940, %struct.ScmObj* %_37foldr147097, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae48940, %struct.ScmObj* %_37map147128, i64 7)
%ae48941 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61508 = alloca %struct.ScmObj*, align 8
%fptrToInt61509 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48942 to i64
%ae48942 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt61509)
store volatile %struct.ScmObj* %ae48942, %struct.ScmObj** %stackaddr$makeclosure61508, align 8
%argslist59776$ae489400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61510 = alloca %struct.ScmObj*, align 8
%argslist59776$ae489401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48942, %struct.ScmObj* %argslist59776$ae489400)
store volatile %struct.ScmObj* %argslist59776$ae489401, %struct.ScmObj** %stackaddr$prim61510, align 8
%stackaddr$prim61511 = alloca %struct.ScmObj*, align 8
%argslist59776$ae489402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48941, %struct.ScmObj* %argslist59776$ae489401)
store volatile %struct.ScmObj* %argslist59776$ae489402, %struct.ScmObj** %stackaddr$prim61511, align 8
%clofunc61512 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48940)
musttail call tailcc void %clofunc61512(%struct.ScmObj* %ae48940, %struct.ScmObj* %argslist59776$ae489402)
ret void
}

define tailcc void @proc_clo$ae48940(%struct.ScmObj* %env$ae48940,%struct.ScmObj* %current_45args59722) {
%stackaddr$env-ref61513 = alloca %struct.ScmObj*, align 8
%lsts47182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48940, i64 0)
store %struct.ScmObj* %lsts47182, %struct.ScmObj** %stackaddr$env-ref61513
%stackaddr$env-ref61514 = alloca %struct.ScmObj*, align 8
%_37foldr47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48940, i64 1)
store %struct.ScmObj* %_37foldr47102, %struct.ScmObj** %stackaddr$env-ref61514
%stackaddr$env-ref61515 = alloca %struct.ScmObj*, align 8
%k47522 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48940, i64 2)
store %struct.ScmObj* %k47522, %struct.ScmObj** %stackaddr$env-ref61515
%stackaddr$env-ref61516 = alloca %struct.ScmObj*, align 8
%f47184 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48940, i64 3)
store %struct.ScmObj* %f47184, %struct.ScmObj** %stackaddr$env-ref61516
%stackaddr$env-ref61517 = alloca %struct.ScmObj*, align 8
%acc47183 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48940, i64 4)
store %struct.ScmObj* %acc47183, %struct.ScmObj** %stackaddr$env-ref61517
%stackaddr$env-ref61518 = alloca %struct.ScmObj*, align 8
%_37foldl47180 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48940, i64 5)
store %struct.ScmObj* %_37foldl47180, %struct.ScmObj** %stackaddr$env-ref61518
%stackaddr$env-ref61519 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48940, i64 6)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref61519
%stackaddr$env-ref61520 = alloca %struct.ScmObj*, align 8
%_37map147128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48940, i64 7)
store %struct.ScmObj* %_37map147128, %struct.ScmObj** %stackaddr$env-ref61520
%stackaddr$prim61521 = alloca %struct.ScmObj*, align 8
%_95k47523 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59722)
store volatile %struct.ScmObj* %_95k47523, %struct.ScmObj** %stackaddr$prim61521, align 8
%stackaddr$prim61522 = alloca %struct.ScmObj*, align 8
%current_45args59723 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59722)
store volatile %struct.ScmObj* %current_45args59723, %struct.ScmObj** %stackaddr$prim61522, align 8
%stackaddr$prim61523 = alloca %struct.ScmObj*, align 8
%anf_45bind47282 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59723)
store volatile %struct.ScmObj* %anf_45bind47282, %struct.ScmObj** %stackaddr$prim61523, align 8
%stackaddr$makeclosure61524 = alloca %struct.ScmObj*, align 8
%fptrToInt61525 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48972 to i64
%ae48972 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt61525)
store volatile %struct.ScmObj* %ae48972, %struct.ScmObj** %stackaddr$makeclosure61524, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48972, %struct.ScmObj* %lsts47182, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48972, %struct.ScmObj* %_37foldr47102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48972, %struct.ScmObj* %k47522, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48972, %struct.ScmObj* %f47184, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48972, %struct.ScmObj* %acc47183, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48972, %struct.ScmObj* %_37foldl47180, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48972, %struct.ScmObj* %_37map147128, i64 6)
%ae48974 = call %struct.ScmObj* @const_init_false()
%argslist59769$_37foldr1470970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61526 = alloca %struct.ScmObj*, align 8
%argslist59769$_37foldr1470971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47182, %struct.ScmObj* %argslist59769$_37foldr1470970)
store volatile %struct.ScmObj* %argslist59769$_37foldr1470971, %struct.ScmObj** %stackaddr$prim61526, align 8
%stackaddr$prim61527 = alloca %struct.ScmObj*, align 8
%argslist59769$_37foldr1470972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48974, %struct.ScmObj* %argslist59769$_37foldr1470971)
store volatile %struct.ScmObj* %argslist59769$_37foldr1470972, %struct.ScmObj** %stackaddr$prim61527, align 8
%stackaddr$prim61528 = alloca %struct.ScmObj*, align 8
%argslist59769$_37foldr1470973 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47282, %struct.ScmObj* %argslist59769$_37foldr1470972)
store volatile %struct.ScmObj* %argslist59769$_37foldr1470973, %struct.ScmObj** %stackaddr$prim61528, align 8
%stackaddr$prim61529 = alloca %struct.ScmObj*, align 8
%argslist59769$_37foldr1470974 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48972, %struct.ScmObj* %argslist59769$_37foldr1470973)
store volatile %struct.ScmObj* %argslist59769$_37foldr1470974, %struct.ScmObj** %stackaddr$prim61529, align 8
%clofunc61530 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147097)
musttail call tailcc void %clofunc61530(%struct.ScmObj* %_37foldr147097, %struct.ScmObj* %argslist59769$_37foldr1470974)
ret void
}

define tailcc void @proc_clo$ae48972(%struct.ScmObj* %env$ae48972,%struct.ScmObj* %current_45args59725) {
%stackaddr$env-ref61531 = alloca %struct.ScmObj*, align 8
%lsts47182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48972, i64 0)
store %struct.ScmObj* %lsts47182, %struct.ScmObj** %stackaddr$env-ref61531
%stackaddr$env-ref61532 = alloca %struct.ScmObj*, align 8
%_37foldr47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48972, i64 1)
store %struct.ScmObj* %_37foldr47102, %struct.ScmObj** %stackaddr$env-ref61532
%stackaddr$env-ref61533 = alloca %struct.ScmObj*, align 8
%k47522 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48972, i64 2)
store %struct.ScmObj* %k47522, %struct.ScmObj** %stackaddr$env-ref61533
%stackaddr$env-ref61534 = alloca %struct.ScmObj*, align 8
%f47184 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48972, i64 3)
store %struct.ScmObj* %f47184, %struct.ScmObj** %stackaddr$env-ref61534
%stackaddr$env-ref61535 = alloca %struct.ScmObj*, align 8
%acc47183 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48972, i64 4)
store %struct.ScmObj* %acc47183, %struct.ScmObj** %stackaddr$env-ref61535
%stackaddr$env-ref61536 = alloca %struct.ScmObj*, align 8
%_37foldl47180 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48972, i64 5)
store %struct.ScmObj* %_37foldl47180, %struct.ScmObj** %stackaddr$env-ref61536
%stackaddr$env-ref61537 = alloca %struct.ScmObj*, align 8
%_37map147128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48972, i64 6)
store %struct.ScmObj* %_37map147128, %struct.ScmObj** %stackaddr$env-ref61537
%stackaddr$prim61538 = alloca %struct.ScmObj*, align 8
%_95k47524 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59725)
store volatile %struct.ScmObj* %_95k47524, %struct.ScmObj** %stackaddr$prim61538, align 8
%stackaddr$prim61539 = alloca %struct.ScmObj*, align 8
%current_45args59726 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59725)
store volatile %struct.ScmObj* %current_45args59726, %struct.ScmObj** %stackaddr$prim61539, align 8
%stackaddr$prim61540 = alloca %struct.ScmObj*, align 8
%anf_45bind47283 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59726)
store volatile %struct.ScmObj* %anf_45bind47283, %struct.ScmObj** %stackaddr$prim61540, align 8
%truthy$cmp61541 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47283)
%cmp$cmp61541 = icmp eq i64 %truthy$cmp61541, 1
br i1 %cmp$cmp61541, label %truebranch$cmp61541, label %falsebranch$cmp61541
truebranch$cmp61541:
%ae48983 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist59728$k475220 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61542 = alloca %struct.ScmObj*, align 8
%argslist59728$k475221 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47183, %struct.ScmObj* %argslist59728$k475220)
store volatile %struct.ScmObj* %argslist59728$k475221, %struct.ScmObj** %stackaddr$prim61542, align 8
%stackaddr$prim61543 = alloca %struct.ScmObj*, align 8
%argslist59728$k475222 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48983, %struct.ScmObj* %argslist59728$k475221)
store volatile %struct.ScmObj* %argslist59728$k475222, %struct.ScmObj** %stackaddr$prim61543, align 8
%clofunc61544 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47522)
musttail call tailcc void %clofunc61544(%struct.ScmObj* %k47522, %struct.ScmObj* %argslist59728$k475222)
ret void
falsebranch$cmp61541:
%stackaddr$makeclosure61545 = alloca %struct.ScmObj*, align 8
%fptrToInt61546 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48988 to i64
%ae48988 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt61546)
store volatile %struct.ScmObj* %ae48988, %struct.ScmObj** %stackaddr$makeclosure61545, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48988, %struct.ScmObj* %lsts47182, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48988, %struct.ScmObj* %_37foldr47102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48988, %struct.ScmObj* %k47522, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48988, %struct.ScmObj* %f47184, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48988, %struct.ScmObj* %acc47183, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48988, %struct.ScmObj* %_37foldl47180, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48988, %struct.ScmObj* %_37map147128, i64 6)
%ae48989 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61547 = alloca %struct.ScmObj*, align 8
%fptrToInt61548 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48990 to i64
%ae48990 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt61548)
store volatile %struct.ScmObj* %ae48990, %struct.ScmObj** %stackaddr$makeclosure61547, align 8
%argslist59768$ae489880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61549 = alloca %struct.ScmObj*, align 8
%argslist59768$ae489881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48990, %struct.ScmObj* %argslist59768$ae489880)
store volatile %struct.ScmObj* %argslist59768$ae489881, %struct.ScmObj** %stackaddr$prim61549, align 8
%stackaddr$prim61550 = alloca %struct.ScmObj*, align 8
%argslist59768$ae489882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48989, %struct.ScmObj* %argslist59768$ae489881)
store volatile %struct.ScmObj* %argslist59768$ae489882, %struct.ScmObj** %stackaddr$prim61550, align 8
%clofunc61551 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48988)
musttail call tailcc void %clofunc61551(%struct.ScmObj* %ae48988, %struct.ScmObj* %argslist59768$ae489882)
ret void
}

define tailcc void @proc_clo$ae48988(%struct.ScmObj* %env$ae48988,%struct.ScmObj* %current_45args59729) {
%stackaddr$env-ref61552 = alloca %struct.ScmObj*, align 8
%lsts47182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48988, i64 0)
store %struct.ScmObj* %lsts47182, %struct.ScmObj** %stackaddr$env-ref61552
%stackaddr$env-ref61553 = alloca %struct.ScmObj*, align 8
%_37foldr47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48988, i64 1)
store %struct.ScmObj* %_37foldr47102, %struct.ScmObj** %stackaddr$env-ref61553
%stackaddr$env-ref61554 = alloca %struct.ScmObj*, align 8
%k47522 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48988, i64 2)
store %struct.ScmObj* %k47522, %struct.ScmObj** %stackaddr$env-ref61554
%stackaddr$env-ref61555 = alloca %struct.ScmObj*, align 8
%f47184 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48988, i64 3)
store %struct.ScmObj* %f47184, %struct.ScmObj** %stackaddr$env-ref61555
%stackaddr$env-ref61556 = alloca %struct.ScmObj*, align 8
%acc47183 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48988, i64 4)
store %struct.ScmObj* %acc47183, %struct.ScmObj** %stackaddr$env-ref61556
%stackaddr$env-ref61557 = alloca %struct.ScmObj*, align 8
%_37foldl47180 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48988, i64 5)
store %struct.ScmObj* %_37foldl47180, %struct.ScmObj** %stackaddr$env-ref61557
%stackaddr$env-ref61558 = alloca %struct.ScmObj*, align 8
%_37map147128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48988, i64 6)
store %struct.ScmObj* %_37map147128, %struct.ScmObj** %stackaddr$env-ref61558
%stackaddr$prim61559 = alloca %struct.ScmObj*, align 8
%_95k47525 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59729)
store volatile %struct.ScmObj* %_95k47525, %struct.ScmObj** %stackaddr$prim61559, align 8
%stackaddr$prim61560 = alloca %struct.ScmObj*, align 8
%current_45args59730 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59729)
store volatile %struct.ScmObj* %current_45args59730, %struct.ScmObj** %stackaddr$prim61560, align 8
%stackaddr$prim61561 = alloca %struct.ScmObj*, align 8
%anf_45bind47284 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59730)
store volatile %struct.ScmObj* %anf_45bind47284, %struct.ScmObj** %stackaddr$prim61561, align 8
%stackaddr$makeclosure61562 = alloca %struct.ScmObj*, align 8
%fptrToInt61563 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49009 to i64
%ae49009 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt61563)
store volatile %struct.ScmObj* %ae49009, %struct.ScmObj** %stackaddr$makeclosure61562, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49009, %struct.ScmObj* %lsts47182, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49009, %struct.ScmObj* %_37foldr47102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49009, %struct.ScmObj* %k47522, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49009, %struct.ScmObj* %f47184, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49009, %struct.ScmObj* %acc47183, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49009, %struct.ScmObj* %_37foldl47180, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49009, %struct.ScmObj* %_37map147128, i64 6)
%argslist59763$_37map1471280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61564 = alloca %struct.ScmObj*, align 8
%argslist59763$_37map1471281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47182, %struct.ScmObj* %argslist59763$_37map1471280)
store volatile %struct.ScmObj* %argslist59763$_37map1471281, %struct.ScmObj** %stackaddr$prim61564, align 8
%stackaddr$prim61565 = alloca %struct.ScmObj*, align 8
%argslist59763$_37map1471282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47284, %struct.ScmObj* %argslist59763$_37map1471281)
store volatile %struct.ScmObj* %argslist59763$_37map1471282, %struct.ScmObj** %stackaddr$prim61565, align 8
%stackaddr$prim61566 = alloca %struct.ScmObj*, align 8
%argslist59763$_37map1471283 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49009, %struct.ScmObj* %argslist59763$_37map1471282)
store volatile %struct.ScmObj* %argslist59763$_37map1471283, %struct.ScmObj** %stackaddr$prim61566, align 8
%clofunc61567 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147128)
musttail call tailcc void %clofunc61567(%struct.ScmObj* %_37map147128, %struct.ScmObj* %argslist59763$_37map1471283)
ret void
}

define tailcc void @proc_clo$ae49009(%struct.ScmObj* %env$ae49009,%struct.ScmObj* %current_45args59732) {
%stackaddr$env-ref61568 = alloca %struct.ScmObj*, align 8
%lsts47182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49009, i64 0)
store %struct.ScmObj* %lsts47182, %struct.ScmObj** %stackaddr$env-ref61568
%stackaddr$env-ref61569 = alloca %struct.ScmObj*, align 8
%_37foldr47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49009, i64 1)
store %struct.ScmObj* %_37foldr47102, %struct.ScmObj** %stackaddr$env-ref61569
%stackaddr$env-ref61570 = alloca %struct.ScmObj*, align 8
%k47522 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49009, i64 2)
store %struct.ScmObj* %k47522, %struct.ScmObj** %stackaddr$env-ref61570
%stackaddr$env-ref61571 = alloca %struct.ScmObj*, align 8
%f47184 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49009, i64 3)
store %struct.ScmObj* %f47184, %struct.ScmObj** %stackaddr$env-ref61571
%stackaddr$env-ref61572 = alloca %struct.ScmObj*, align 8
%acc47183 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49009, i64 4)
store %struct.ScmObj* %acc47183, %struct.ScmObj** %stackaddr$env-ref61572
%stackaddr$env-ref61573 = alloca %struct.ScmObj*, align 8
%_37foldl47180 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49009, i64 5)
store %struct.ScmObj* %_37foldl47180, %struct.ScmObj** %stackaddr$env-ref61573
%stackaddr$env-ref61574 = alloca %struct.ScmObj*, align 8
%_37map147128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49009, i64 6)
store %struct.ScmObj* %_37map147128, %struct.ScmObj** %stackaddr$env-ref61574
%stackaddr$prim61575 = alloca %struct.ScmObj*, align 8
%_95k47526 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59732)
store volatile %struct.ScmObj* %_95k47526, %struct.ScmObj** %stackaddr$prim61575, align 8
%stackaddr$prim61576 = alloca %struct.ScmObj*, align 8
%current_45args59733 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59732)
store volatile %struct.ScmObj* %current_45args59733, %struct.ScmObj** %stackaddr$prim61576, align 8
%stackaddr$prim61577 = alloca %struct.ScmObj*, align 8
%lsts_4347189 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59733)
store volatile %struct.ScmObj* %lsts_4347189, %struct.ScmObj** %stackaddr$prim61577, align 8
%stackaddr$makeclosure61578 = alloca %struct.ScmObj*, align 8
%fptrToInt61579 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49012 to i64
%ae49012 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt61579)
store volatile %struct.ScmObj* %ae49012, %struct.ScmObj** %stackaddr$makeclosure61578, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49012, %struct.ScmObj* %lsts47182, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49012, %struct.ScmObj* %_37foldr47102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49012, %struct.ScmObj* %lsts_4347189, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49012, %struct.ScmObj* %k47522, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49012, %struct.ScmObj* %f47184, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49012, %struct.ScmObj* %acc47183, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49012, %struct.ScmObj* %_37foldl47180, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae49012, %struct.ScmObj* %_37map147128, i64 7)
%ae49013 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61580 = alloca %struct.ScmObj*, align 8
%fptrToInt61581 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49014 to i64
%ae49014 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt61581)
store volatile %struct.ScmObj* %ae49014, %struct.ScmObj** %stackaddr$makeclosure61580, align 8
%argslist59762$ae490120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61582 = alloca %struct.ScmObj*, align 8
%argslist59762$ae490121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49014, %struct.ScmObj* %argslist59762$ae490120)
store volatile %struct.ScmObj* %argslist59762$ae490121, %struct.ScmObj** %stackaddr$prim61582, align 8
%stackaddr$prim61583 = alloca %struct.ScmObj*, align 8
%argslist59762$ae490122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49013, %struct.ScmObj* %argslist59762$ae490121)
store volatile %struct.ScmObj* %argslist59762$ae490122, %struct.ScmObj** %stackaddr$prim61583, align 8
%clofunc61584 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49012)
musttail call tailcc void %clofunc61584(%struct.ScmObj* %ae49012, %struct.ScmObj* %argslist59762$ae490122)
ret void
}

define tailcc void @proc_clo$ae49012(%struct.ScmObj* %env$ae49012,%struct.ScmObj* %current_45args59735) {
%stackaddr$env-ref61585 = alloca %struct.ScmObj*, align 8
%lsts47182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49012, i64 0)
store %struct.ScmObj* %lsts47182, %struct.ScmObj** %stackaddr$env-ref61585
%stackaddr$env-ref61586 = alloca %struct.ScmObj*, align 8
%_37foldr47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49012, i64 1)
store %struct.ScmObj* %_37foldr47102, %struct.ScmObj** %stackaddr$env-ref61586
%stackaddr$env-ref61587 = alloca %struct.ScmObj*, align 8
%lsts_4347189 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49012, i64 2)
store %struct.ScmObj* %lsts_4347189, %struct.ScmObj** %stackaddr$env-ref61587
%stackaddr$env-ref61588 = alloca %struct.ScmObj*, align 8
%k47522 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49012, i64 3)
store %struct.ScmObj* %k47522, %struct.ScmObj** %stackaddr$env-ref61588
%stackaddr$env-ref61589 = alloca %struct.ScmObj*, align 8
%f47184 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49012, i64 4)
store %struct.ScmObj* %f47184, %struct.ScmObj** %stackaddr$env-ref61589
%stackaddr$env-ref61590 = alloca %struct.ScmObj*, align 8
%acc47183 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49012, i64 5)
store %struct.ScmObj* %acc47183, %struct.ScmObj** %stackaddr$env-ref61590
%stackaddr$env-ref61591 = alloca %struct.ScmObj*, align 8
%_37foldl47180 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49012, i64 6)
store %struct.ScmObj* %_37foldl47180, %struct.ScmObj** %stackaddr$env-ref61591
%stackaddr$env-ref61592 = alloca %struct.ScmObj*, align 8
%_37map147128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49012, i64 7)
store %struct.ScmObj* %_37map147128, %struct.ScmObj** %stackaddr$env-ref61592
%stackaddr$prim61593 = alloca %struct.ScmObj*, align 8
%_95k47527 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59735)
store volatile %struct.ScmObj* %_95k47527, %struct.ScmObj** %stackaddr$prim61593, align 8
%stackaddr$prim61594 = alloca %struct.ScmObj*, align 8
%current_45args59736 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59735)
store volatile %struct.ScmObj* %current_45args59736, %struct.ScmObj** %stackaddr$prim61594, align 8
%stackaddr$prim61595 = alloca %struct.ScmObj*, align 8
%anf_45bind47285 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59736)
store volatile %struct.ScmObj* %anf_45bind47285, %struct.ScmObj** %stackaddr$prim61595, align 8
%stackaddr$makeclosure61596 = alloca %struct.ScmObj*, align 8
%fptrToInt61597 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49033 to i64
%ae49033 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt61597)
store volatile %struct.ScmObj* %ae49033, %struct.ScmObj** %stackaddr$makeclosure61596, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49033, %struct.ScmObj* %lsts_4347189, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49033, %struct.ScmObj* %k47522, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49033, %struct.ScmObj* %f47184, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49033, %struct.ScmObj* %acc47183, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49033, %struct.ScmObj* %_37foldr47102, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49033, %struct.ScmObj* %_37foldl47180, i64 5)
%argslist59757$_37map1471280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61598 = alloca %struct.ScmObj*, align 8
%argslist59757$_37map1471281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47182, %struct.ScmObj* %argslist59757$_37map1471280)
store volatile %struct.ScmObj* %argslist59757$_37map1471281, %struct.ScmObj** %stackaddr$prim61598, align 8
%stackaddr$prim61599 = alloca %struct.ScmObj*, align 8
%argslist59757$_37map1471282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47285, %struct.ScmObj* %argslist59757$_37map1471281)
store volatile %struct.ScmObj* %argslist59757$_37map1471282, %struct.ScmObj** %stackaddr$prim61599, align 8
%stackaddr$prim61600 = alloca %struct.ScmObj*, align 8
%argslist59757$_37map1471283 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49033, %struct.ScmObj* %argslist59757$_37map1471282)
store volatile %struct.ScmObj* %argslist59757$_37map1471283, %struct.ScmObj** %stackaddr$prim61600, align 8
%clofunc61601 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147128)
musttail call tailcc void %clofunc61601(%struct.ScmObj* %_37map147128, %struct.ScmObj* %argslist59757$_37map1471283)
ret void
}

define tailcc void @proc_clo$ae49033(%struct.ScmObj* %env$ae49033,%struct.ScmObj* %current_45args59738) {
%stackaddr$env-ref61602 = alloca %struct.ScmObj*, align 8
%lsts_4347189 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49033, i64 0)
store %struct.ScmObj* %lsts_4347189, %struct.ScmObj** %stackaddr$env-ref61602
%stackaddr$env-ref61603 = alloca %struct.ScmObj*, align 8
%k47522 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49033, i64 1)
store %struct.ScmObj* %k47522, %struct.ScmObj** %stackaddr$env-ref61603
%stackaddr$env-ref61604 = alloca %struct.ScmObj*, align 8
%f47184 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49033, i64 2)
store %struct.ScmObj* %f47184, %struct.ScmObj** %stackaddr$env-ref61604
%stackaddr$env-ref61605 = alloca %struct.ScmObj*, align 8
%acc47183 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49033, i64 3)
store %struct.ScmObj* %acc47183, %struct.ScmObj** %stackaddr$env-ref61605
%stackaddr$env-ref61606 = alloca %struct.ScmObj*, align 8
%_37foldr47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49033, i64 4)
store %struct.ScmObj* %_37foldr47102, %struct.ScmObj** %stackaddr$env-ref61606
%stackaddr$env-ref61607 = alloca %struct.ScmObj*, align 8
%_37foldl47180 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49033, i64 5)
store %struct.ScmObj* %_37foldl47180, %struct.ScmObj** %stackaddr$env-ref61607
%stackaddr$prim61608 = alloca %struct.ScmObj*, align 8
%_95k47528 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59738)
store volatile %struct.ScmObj* %_95k47528, %struct.ScmObj** %stackaddr$prim61608, align 8
%stackaddr$prim61609 = alloca %struct.ScmObj*, align 8
%current_45args59739 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59738)
store volatile %struct.ScmObj* %current_45args59739, %struct.ScmObj** %stackaddr$prim61609, align 8
%stackaddr$prim61610 = alloca %struct.ScmObj*, align 8
%vs47187 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59739)
store volatile %struct.ScmObj* %vs47187, %struct.ScmObj** %stackaddr$prim61610, align 8
%stackaddr$makeclosure61611 = alloca %struct.ScmObj*, align 8
%fptrToInt61612 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49036 to i64
%ae49036 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt61612)
store volatile %struct.ScmObj* %ae49036, %struct.ScmObj** %stackaddr$makeclosure61611, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49036, %struct.ScmObj* %lsts_4347189, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49036, %struct.ScmObj* %vs47187, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49036, %struct.ScmObj* %k47522, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49036, %struct.ScmObj* %f47184, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49036, %struct.ScmObj* %acc47183, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49036, %struct.ScmObj* %_37foldr47102, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49036, %struct.ScmObj* %_37foldl47180, i64 6)
%ae49037 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61613 = alloca %struct.ScmObj*, align 8
%fptrToInt61614 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49038 to i64
%ae49038 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt61614)
store volatile %struct.ScmObj* %ae49038, %struct.ScmObj** %stackaddr$makeclosure61613, align 8
%argslist59756$ae490360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61615 = alloca %struct.ScmObj*, align 8
%argslist59756$ae490361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49038, %struct.ScmObj* %argslist59756$ae490360)
store volatile %struct.ScmObj* %argslist59756$ae490361, %struct.ScmObj** %stackaddr$prim61615, align 8
%stackaddr$prim61616 = alloca %struct.ScmObj*, align 8
%argslist59756$ae490362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49037, %struct.ScmObj* %argslist59756$ae490361)
store volatile %struct.ScmObj* %argslist59756$ae490362, %struct.ScmObj** %stackaddr$prim61616, align 8
%clofunc61617 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49036)
musttail call tailcc void %clofunc61617(%struct.ScmObj* %ae49036, %struct.ScmObj* %argslist59756$ae490362)
ret void
}

define tailcc void @proc_clo$ae49036(%struct.ScmObj* %env$ae49036,%struct.ScmObj* %current_45args59741) {
%stackaddr$env-ref61618 = alloca %struct.ScmObj*, align 8
%lsts_4347189 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49036, i64 0)
store %struct.ScmObj* %lsts_4347189, %struct.ScmObj** %stackaddr$env-ref61618
%stackaddr$env-ref61619 = alloca %struct.ScmObj*, align 8
%vs47187 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49036, i64 1)
store %struct.ScmObj* %vs47187, %struct.ScmObj** %stackaddr$env-ref61619
%stackaddr$env-ref61620 = alloca %struct.ScmObj*, align 8
%k47522 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49036, i64 2)
store %struct.ScmObj* %k47522, %struct.ScmObj** %stackaddr$env-ref61620
%stackaddr$env-ref61621 = alloca %struct.ScmObj*, align 8
%f47184 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49036, i64 3)
store %struct.ScmObj* %f47184, %struct.ScmObj** %stackaddr$env-ref61621
%stackaddr$env-ref61622 = alloca %struct.ScmObj*, align 8
%acc47183 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49036, i64 4)
store %struct.ScmObj* %acc47183, %struct.ScmObj** %stackaddr$env-ref61622
%stackaddr$env-ref61623 = alloca %struct.ScmObj*, align 8
%_37foldr47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49036, i64 5)
store %struct.ScmObj* %_37foldr47102, %struct.ScmObj** %stackaddr$env-ref61623
%stackaddr$env-ref61624 = alloca %struct.ScmObj*, align 8
%_37foldl47180 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49036, i64 6)
store %struct.ScmObj* %_37foldl47180, %struct.ScmObj** %stackaddr$env-ref61624
%stackaddr$prim61625 = alloca %struct.ScmObj*, align 8
%_95k47529 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59741)
store volatile %struct.ScmObj* %_95k47529, %struct.ScmObj** %stackaddr$prim61625, align 8
%stackaddr$prim61626 = alloca %struct.ScmObj*, align 8
%current_45args59742 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59741)
store volatile %struct.ScmObj* %current_45args59742, %struct.ScmObj** %stackaddr$prim61626, align 8
%stackaddr$prim61627 = alloca %struct.ScmObj*, align 8
%anf_45bind47286 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59742)
store volatile %struct.ScmObj* %anf_45bind47286, %struct.ScmObj** %stackaddr$prim61627, align 8
%ae49059 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61628 = alloca %struct.ScmObj*, align 8
%anf_45bind47287 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47183, %struct.ScmObj* %ae49059)
store volatile %struct.ScmObj* %anf_45bind47287, %struct.ScmObj** %stackaddr$prim61628, align 8
%stackaddr$makeclosure61629 = alloca %struct.ScmObj*, align 8
%fptrToInt61630 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49061 to i64
%ae49061 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt61630)
store volatile %struct.ScmObj* %ae49061, %struct.ScmObj** %stackaddr$makeclosure61629, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49061, %struct.ScmObj* %lsts_4347189, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49061, %struct.ScmObj* %k47522, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49061, %struct.ScmObj* %f47184, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49061, %struct.ScmObj* %_37foldl47180, i64 3)
%argslist59750$_37foldr471020 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61631 = alloca %struct.ScmObj*, align 8
%argslist59750$_37foldr471021 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs47187, %struct.ScmObj* %argslist59750$_37foldr471020)
store volatile %struct.ScmObj* %argslist59750$_37foldr471021, %struct.ScmObj** %stackaddr$prim61631, align 8
%stackaddr$prim61632 = alloca %struct.ScmObj*, align 8
%argslist59750$_37foldr471022 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47287, %struct.ScmObj* %argslist59750$_37foldr471021)
store volatile %struct.ScmObj* %argslist59750$_37foldr471022, %struct.ScmObj** %stackaddr$prim61632, align 8
%stackaddr$prim61633 = alloca %struct.ScmObj*, align 8
%argslist59750$_37foldr471023 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47286, %struct.ScmObj* %argslist59750$_37foldr471022)
store volatile %struct.ScmObj* %argslist59750$_37foldr471023, %struct.ScmObj** %stackaddr$prim61633, align 8
%stackaddr$prim61634 = alloca %struct.ScmObj*, align 8
%argslist59750$_37foldr471024 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49061, %struct.ScmObj* %argslist59750$_37foldr471023)
store volatile %struct.ScmObj* %argslist59750$_37foldr471024, %struct.ScmObj** %stackaddr$prim61634, align 8
%clofunc61635 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr47102)
musttail call tailcc void %clofunc61635(%struct.ScmObj* %_37foldr47102, %struct.ScmObj* %argslist59750$_37foldr471024)
ret void
}

define tailcc void @proc_clo$ae49061(%struct.ScmObj* %env$ae49061,%struct.ScmObj* %current_45args59744) {
%stackaddr$env-ref61636 = alloca %struct.ScmObj*, align 8
%lsts_4347189 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49061, i64 0)
store %struct.ScmObj* %lsts_4347189, %struct.ScmObj** %stackaddr$env-ref61636
%stackaddr$env-ref61637 = alloca %struct.ScmObj*, align 8
%k47522 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49061, i64 1)
store %struct.ScmObj* %k47522, %struct.ScmObj** %stackaddr$env-ref61637
%stackaddr$env-ref61638 = alloca %struct.ScmObj*, align 8
%f47184 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49061, i64 2)
store %struct.ScmObj* %f47184, %struct.ScmObj** %stackaddr$env-ref61638
%stackaddr$env-ref61639 = alloca %struct.ScmObj*, align 8
%_37foldl47180 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49061, i64 3)
store %struct.ScmObj* %_37foldl47180, %struct.ScmObj** %stackaddr$env-ref61639
%stackaddr$prim61640 = alloca %struct.ScmObj*, align 8
%_95k47530 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59744)
store volatile %struct.ScmObj* %_95k47530, %struct.ScmObj** %stackaddr$prim61640, align 8
%stackaddr$prim61641 = alloca %struct.ScmObj*, align 8
%current_45args59745 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59744)
store volatile %struct.ScmObj* %current_45args59745, %struct.ScmObj** %stackaddr$prim61641, align 8
%stackaddr$prim61642 = alloca %struct.ScmObj*, align 8
%anf_45bind47288 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59745)
store volatile %struct.ScmObj* %anf_45bind47288, %struct.ScmObj** %stackaddr$prim61642, align 8
%stackaddr$makeclosure61643 = alloca %struct.ScmObj*, align 8
%fptrToInt61644 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49065 to i64
%ae49065 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt61644)
store volatile %struct.ScmObj* %ae49065, %struct.ScmObj** %stackaddr$makeclosure61643, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49065, %struct.ScmObj* %lsts_4347189, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49065, %struct.ScmObj* %k47522, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49065, %struct.ScmObj* %f47184, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49065, %struct.ScmObj* %_37foldl47180, i64 3)
%stackaddr$prim61645 = alloca %struct.ScmObj*, align 8
%cpsargs47533 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49065, %struct.ScmObj* %anf_45bind47288)
store volatile %struct.ScmObj* %cpsargs47533, %struct.ScmObj** %stackaddr$prim61645, align 8
%clofunc61646 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47184)
musttail call tailcc void %clofunc61646(%struct.ScmObj* %f47184, %struct.ScmObj* %cpsargs47533)
ret void
}

define tailcc void @proc_clo$ae49065(%struct.ScmObj* %env$ae49065,%struct.ScmObj* %current_45args59747) {
%stackaddr$env-ref61647 = alloca %struct.ScmObj*, align 8
%lsts_4347189 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49065, i64 0)
store %struct.ScmObj* %lsts_4347189, %struct.ScmObj** %stackaddr$env-ref61647
%stackaddr$env-ref61648 = alloca %struct.ScmObj*, align 8
%k47522 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49065, i64 1)
store %struct.ScmObj* %k47522, %struct.ScmObj** %stackaddr$env-ref61648
%stackaddr$env-ref61649 = alloca %struct.ScmObj*, align 8
%f47184 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49065, i64 2)
store %struct.ScmObj* %f47184, %struct.ScmObj** %stackaddr$env-ref61649
%stackaddr$env-ref61650 = alloca %struct.ScmObj*, align 8
%_37foldl47180 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49065, i64 3)
store %struct.ScmObj* %_37foldl47180, %struct.ScmObj** %stackaddr$env-ref61650
%stackaddr$prim61651 = alloca %struct.ScmObj*, align 8
%_95k47531 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59747)
store volatile %struct.ScmObj* %_95k47531, %struct.ScmObj** %stackaddr$prim61651, align 8
%stackaddr$prim61652 = alloca %struct.ScmObj*, align 8
%current_45args59748 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59747)
store volatile %struct.ScmObj* %current_45args59748, %struct.ScmObj** %stackaddr$prim61652, align 8
%stackaddr$prim61653 = alloca %struct.ScmObj*, align 8
%acc_4347191 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59748)
store volatile %struct.ScmObj* %acc_4347191, %struct.ScmObj** %stackaddr$prim61653, align 8
%stackaddr$prim61654 = alloca %struct.ScmObj*, align 8
%anf_45bind47289 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4347191, %struct.ScmObj* %lsts_4347189)
store volatile %struct.ScmObj* %anf_45bind47289, %struct.ScmObj** %stackaddr$prim61654, align 8
%stackaddr$prim61655 = alloca %struct.ScmObj*, align 8
%anf_45bind47290 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47184, %struct.ScmObj* %anf_45bind47289)
store volatile %struct.ScmObj* %anf_45bind47290, %struct.ScmObj** %stackaddr$prim61655, align 8
%stackaddr$prim61656 = alloca %struct.ScmObj*, align 8
%cpsargs47532 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47522, %struct.ScmObj* %anf_45bind47290)
store volatile %struct.ScmObj* %cpsargs47532, %struct.ScmObj** %stackaddr$prim61656, align 8
%clofunc61657 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl47180)
musttail call tailcc void %clofunc61657(%struct.ScmObj* %_37foldl47180, %struct.ScmObj* %cpsargs47532)
ret void
}

define tailcc void @proc_clo$ae49038(%struct.ScmObj* %env$ae49038,%struct.ScmObj* %current_45args59751) {
%stackaddr$prim61658 = alloca %struct.ScmObj*, align 8
%k47534 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59751)
store volatile %struct.ScmObj* %k47534, %struct.ScmObj** %stackaddr$prim61658, align 8
%stackaddr$prim61659 = alloca %struct.ScmObj*, align 8
%current_45args59752 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59751)
store volatile %struct.ScmObj* %current_45args59752, %struct.ScmObj** %stackaddr$prim61659, align 8
%stackaddr$prim61660 = alloca %struct.ScmObj*, align 8
%a47193 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59752)
store volatile %struct.ScmObj* %a47193, %struct.ScmObj** %stackaddr$prim61660, align 8
%stackaddr$prim61661 = alloca %struct.ScmObj*, align 8
%current_45args59753 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59752)
store volatile %struct.ScmObj* %current_45args59753, %struct.ScmObj** %stackaddr$prim61661, align 8
%stackaddr$prim61662 = alloca %struct.ScmObj*, align 8
%b47192 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59753)
store volatile %struct.ScmObj* %b47192, %struct.ScmObj** %stackaddr$prim61662, align 8
%stackaddr$prim61663 = alloca %struct.ScmObj*, align 8
%cpsprim47535 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a47193, %struct.ScmObj* %b47192)
store volatile %struct.ScmObj* %cpsprim47535, %struct.ScmObj** %stackaddr$prim61663, align 8
%ae49042 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist59755$k475340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61664 = alloca %struct.ScmObj*, align 8
%argslist59755$k475341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47535, %struct.ScmObj* %argslist59755$k475340)
store volatile %struct.ScmObj* %argslist59755$k475341, %struct.ScmObj** %stackaddr$prim61664, align 8
%stackaddr$prim61665 = alloca %struct.ScmObj*, align 8
%argslist59755$k475342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49042, %struct.ScmObj* %argslist59755$k475341)
store volatile %struct.ScmObj* %argslist59755$k475342, %struct.ScmObj** %stackaddr$prim61665, align 8
%clofunc61666 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47534)
musttail call tailcc void %clofunc61666(%struct.ScmObj* %k47534, %struct.ScmObj* %argslist59755$k475342)
ret void
}

define tailcc void @proc_clo$ae49014(%struct.ScmObj* %env$ae49014,%struct.ScmObj* %current_45args59758) {
%stackaddr$prim61667 = alloca %struct.ScmObj*, align 8
%k47536 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59758)
store volatile %struct.ScmObj* %k47536, %struct.ScmObj** %stackaddr$prim61667, align 8
%stackaddr$prim61668 = alloca %struct.ScmObj*, align 8
%current_45args59759 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59758)
store volatile %struct.ScmObj* %current_45args59759, %struct.ScmObj** %stackaddr$prim61668, align 8
%stackaddr$prim61669 = alloca %struct.ScmObj*, align 8
%x47188 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59759)
store volatile %struct.ScmObj* %x47188, %struct.ScmObj** %stackaddr$prim61669, align 8
%stackaddr$prim61670 = alloca %struct.ScmObj*, align 8
%cpsprim47537 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47188)
store volatile %struct.ScmObj* %cpsprim47537, %struct.ScmObj** %stackaddr$prim61670, align 8
%ae49017 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist59761$k475360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61671 = alloca %struct.ScmObj*, align 8
%argslist59761$k475361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47537, %struct.ScmObj* %argslist59761$k475360)
store volatile %struct.ScmObj* %argslist59761$k475361, %struct.ScmObj** %stackaddr$prim61671, align 8
%stackaddr$prim61672 = alloca %struct.ScmObj*, align 8
%argslist59761$k475362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49017, %struct.ScmObj* %argslist59761$k475361)
store volatile %struct.ScmObj* %argslist59761$k475362, %struct.ScmObj** %stackaddr$prim61672, align 8
%clofunc61673 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47536)
musttail call tailcc void %clofunc61673(%struct.ScmObj* %k47536, %struct.ScmObj* %argslist59761$k475362)
ret void
}

define tailcc void @proc_clo$ae48990(%struct.ScmObj* %env$ae48990,%struct.ScmObj* %current_45args59764) {
%stackaddr$prim61674 = alloca %struct.ScmObj*, align 8
%k47538 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59764)
store volatile %struct.ScmObj* %k47538, %struct.ScmObj** %stackaddr$prim61674, align 8
%stackaddr$prim61675 = alloca %struct.ScmObj*, align 8
%current_45args59765 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59764)
store volatile %struct.ScmObj* %current_45args59765, %struct.ScmObj** %stackaddr$prim61675, align 8
%stackaddr$prim61676 = alloca %struct.ScmObj*, align 8
%x47190 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59765)
store volatile %struct.ScmObj* %x47190, %struct.ScmObj** %stackaddr$prim61676, align 8
%stackaddr$prim61677 = alloca %struct.ScmObj*, align 8
%cpsprim47539 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47190)
store volatile %struct.ScmObj* %cpsprim47539, %struct.ScmObj** %stackaddr$prim61677, align 8
%ae48993 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist59767$k475380 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61678 = alloca %struct.ScmObj*, align 8
%argslist59767$k475381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47539, %struct.ScmObj* %argslist59767$k475380)
store volatile %struct.ScmObj* %argslist59767$k475381, %struct.ScmObj** %stackaddr$prim61678, align 8
%stackaddr$prim61679 = alloca %struct.ScmObj*, align 8
%argslist59767$k475382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48993, %struct.ScmObj* %argslist59767$k475381)
store volatile %struct.ScmObj* %argslist59767$k475382, %struct.ScmObj** %stackaddr$prim61679, align 8
%clofunc61680 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47538)
musttail call tailcc void %clofunc61680(%struct.ScmObj* %k47538, %struct.ScmObj* %argslist59767$k475382)
ret void
}

define tailcc void @proc_clo$ae48942(%struct.ScmObj* %env$ae48942,%struct.ScmObj* %current_45args59770) {
%stackaddr$prim61681 = alloca %struct.ScmObj*, align 8
%k47540 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59770)
store volatile %struct.ScmObj* %k47540, %struct.ScmObj** %stackaddr$prim61681, align 8
%stackaddr$prim61682 = alloca %struct.ScmObj*, align 8
%current_45args59771 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59770)
store volatile %struct.ScmObj* %current_45args59771, %struct.ScmObj** %stackaddr$prim61682, align 8
%stackaddr$prim61683 = alloca %struct.ScmObj*, align 8
%lst47186 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59771)
store volatile %struct.ScmObj* %lst47186, %struct.ScmObj** %stackaddr$prim61683, align 8
%stackaddr$prim61684 = alloca %struct.ScmObj*, align 8
%current_45args59772 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59771)
store volatile %struct.ScmObj* %current_45args59772, %struct.ScmObj** %stackaddr$prim61684, align 8
%stackaddr$prim61685 = alloca %struct.ScmObj*, align 8
%b47185 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59772)
store volatile %struct.ScmObj* %b47185, %struct.ScmObj** %stackaddr$prim61685, align 8
%truthy$cmp61686 = call i64 @is_truthy_value(%struct.ScmObj* %b47185)
%cmp$cmp61686 = icmp eq i64 %truthy$cmp61686, 1
br i1 %cmp$cmp61686, label %truebranch$cmp61686, label %falsebranch$cmp61686
truebranch$cmp61686:
%ae48945 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist59774$k475400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61687 = alloca %struct.ScmObj*, align 8
%argslist59774$k475401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b47185, %struct.ScmObj* %argslist59774$k475400)
store volatile %struct.ScmObj* %argslist59774$k475401, %struct.ScmObj** %stackaddr$prim61687, align 8
%stackaddr$prim61688 = alloca %struct.ScmObj*, align 8
%argslist59774$k475402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48945, %struct.ScmObj* %argslist59774$k475401)
store volatile %struct.ScmObj* %argslist59774$k475402, %struct.ScmObj** %stackaddr$prim61688, align 8
%clofunc61689 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47540)
musttail call tailcc void %clofunc61689(%struct.ScmObj* %k47540, %struct.ScmObj* %argslist59774$k475402)
ret void
falsebranch$cmp61686:
%stackaddr$prim61690 = alloca %struct.ScmObj*, align 8
%cpsprim47541 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47186)
store volatile %struct.ScmObj* %cpsprim47541, %struct.ScmObj** %stackaddr$prim61690, align 8
%ae48952 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist59775$k475400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61691 = alloca %struct.ScmObj*, align 8
%argslist59775$k475401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47541, %struct.ScmObj* %argslist59775$k475400)
store volatile %struct.ScmObj* %argslist59775$k475401, %struct.ScmObj** %stackaddr$prim61691, align 8
%stackaddr$prim61692 = alloca %struct.ScmObj*, align 8
%argslist59775$k475402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48952, %struct.ScmObj* %argslist59775$k475401)
store volatile %struct.ScmObj* %argslist59775$k475402, %struct.ScmObj** %stackaddr$prim61692, align 8
%clofunc61693 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47540)
musttail call tailcc void %clofunc61693(%struct.ScmObj* %k47540, %struct.ScmObj* %argslist59775$k475402)
ret void
}

define tailcc void @proc_clo$ae48783(%struct.ScmObj* %env$ae48783,%struct.ScmObj* %args4712447542) {
%stackaddr$env-ref61694 = alloca %struct.ScmObj*, align 8
%_37last47119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48783, i64 0)
store %struct.ScmObj* %_37last47119, %struct.ScmObj** %stackaddr$env-ref61694
%stackaddr$env-ref61695 = alloca %struct.ScmObj*, align 8
%_37foldr47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48783, i64 1)
store %struct.ScmObj* %_37foldr47102, %struct.ScmObj** %stackaddr$env-ref61695
%stackaddr$env-ref61696 = alloca %struct.ScmObj*, align 8
%_37drop_45right47116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48783, i64 2)
store %struct.ScmObj* %_37drop_45right47116, %struct.ScmObj** %stackaddr$env-ref61696
%stackaddr$prim61697 = alloca %struct.ScmObj*, align 8
%k47543 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4712447542)
store volatile %struct.ScmObj* %k47543, %struct.ScmObj** %stackaddr$prim61697, align 8
%stackaddr$prim61698 = alloca %struct.ScmObj*, align 8
%args47124 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4712447542)
store volatile %struct.ScmObj* %args47124, %struct.ScmObj** %stackaddr$prim61698, align 8
%stackaddr$prim61699 = alloca %struct.ScmObj*, align 8
%f47126 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47124)
store volatile %struct.ScmObj* %f47126, %struct.ScmObj** %stackaddr$prim61699, align 8
%stackaddr$prim61700 = alloca %struct.ScmObj*, align 8
%lsts47125 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47124)
store volatile %struct.ScmObj* %lsts47125, %struct.ScmObj** %stackaddr$prim61700, align 8
%stackaddr$makeclosure61701 = alloca %struct.ScmObj*, align 8
%fptrToInt61702 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48788 to i64
%ae48788 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt61702)
store volatile %struct.ScmObj* %ae48788, %struct.ScmObj** %stackaddr$makeclosure61701, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48788, %struct.ScmObj* %lsts47125, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48788, %struct.ScmObj* %_37foldr47102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48788, %struct.ScmObj* %k47543, i64 2)
%ae48789 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61703 = alloca %struct.ScmObj*, align 8
%fptrToInt61704 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48790 to i64
%ae48790 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt61704)
store volatile %struct.ScmObj* %ae48790, %struct.ScmObj** %stackaddr$makeclosure61703, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48790, %struct.ScmObj* %f47126, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48790, %struct.ScmObj* %_37last47119, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48790, %struct.ScmObj* %_37drop_45right47116, i64 2)
%argslist59794$ae487880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61705 = alloca %struct.ScmObj*, align 8
%argslist59794$ae487881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48790, %struct.ScmObj* %argslist59794$ae487880)
store volatile %struct.ScmObj* %argslist59794$ae487881, %struct.ScmObj** %stackaddr$prim61705, align 8
%stackaddr$prim61706 = alloca %struct.ScmObj*, align 8
%argslist59794$ae487882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48789, %struct.ScmObj* %argslist59794$ae487881)
store volatile %struct.ScmObj* %argslist59794$ae487882, %struct.ScmObj** %stackaddr$prim61706, align 8
%clofunc61707 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48788)
musttail call tailcc void %clofunc61707(%struct.ScmObj* %ae48788, %struct.ScmObj* %argslist59794$ae487882)
ret void
}

define tailcc void @proc_clo$ae48788(%struct.ScmObj* %env$ae48788,%struct.ScmObj* %current_45args59779) {
%stackaddr$env-ref61708 = alloca %struct.ScmObj*, align 8
%lsts47125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48788, i64 0)
store %struct.ScmObj* %lsts47125, %struct.ScmObj** %stackaddr$env-ref61708
%stackaddr$env-ref61709 = alloca %struct.ScmObj*, align 8
%_37foldr47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48788, i64 1)
store %struct.ScmObj* %_37foldr47102, %struct.ScmObj** %stackaddr$env-ref61709
%stackaddr$env-ref61710 = alloca %struct.ScmObj*, align 8
%k47543 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48788, i64 2)
store %struct.ScmObj* %k47543, %struct.ScmObj** %stackaddr$env-ref61710
%stackaddr$prim61711 = alloca %struct.ScmObj*, align 8
%_95k47544 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59779)
store volatile %struct.ScmObj* %_95k47544, %struct.ScmObj** %stackaddr$prim61711, align 8
%stackaddr$prim61712 = alloca %struct.ScmObj*, align 8
%current_45args59780 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59779)
store volatile %struct.ScmObj* %current_45args59780, %struct.ScmObj** %stackaddr$prim61712, align 8
%stackaddr$prim61713 = alloca %struct.ScmObj*, align 8
%anf_45bind47277 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59780)
store volatile %struct.ScmObj* %anf_45bind47277, %struct.ScmObj** %stackaddr$prim61713, align 8
%ae48851 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61714 = alloca %struct.ScmObj*, align 8
%anf_45bind47278 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48851, %struct.ScmObj* %lsts47125)
store volatile %struct.ScmObj* %anf_45bind47278, %struct.ScmObj** %stackaddr$prim61714, align 8
%stackaddr$prim61715 = alloca %struct.ScmObj*, align 8
%anf_45bind47279 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47277, %struct.ScmObj* %anf_45bind47278)
store volatile %struct.ScmObj* %anf_45bind47279, %struct.ScmObj** %stackaddr$prim61715, align 8
%stackaddr$prim61716 = alloca %struct.ScmObj*, align 8
%cpsargs47545 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47543, %struct.ScmObj* %anf_45bind47279)
store volatile %struct.ScmObj* %cpsargs47545, %struct.ScmObj** %stackaddr$prim61716, align 8
%clofunc61717 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr47102)
musttail call tailcc void %clofunc61717(%struct.ScmObj* %_37foldr47102, %struct.ScmObj* %cpsargs47545)
ret void
}

define tailcc void @proc_clo$ae48790(%struct.ScmObj* %env$ae48790,%struct.ScmObj* %fargs4712747546) {
%stackaddr$env-ref61718 = alloca %struct.ScmObj*, align 8
%f47126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48790, i64 0)
store %struct.ScmObj* %f47126, %struct.ScmObj** %stackaddr$env-ref61718
%stackaddr$env-ref61719 = alloca %struct.ScmObj*, align 8
%_37last47119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48790, i64 1)
store %struct.ScmObj* %_37last47119, %struct.ScmObj** %stackaddr$env-ref61719
%stackaddr$env-ref61720 = alloca %struct.ScmObj*, align 8
%_37drop_45right47116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48790, i64 2)
store %struct.ScmObj* %_37drop_45right47116, %struct.ScmObj** %stackaddr$env-ref61720
%stackaddr$prim61721 = alloca %struct.ScmObj*, align 8
%k47547 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs4712747546)
store volatile %struct.ScmObj* %k47547, %struct.ScmObj** %stackaddr$prim61721, align 8
%stackaddr$prim61722 = alloca %struct.ScmObj*, align 8
%fargs47127 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs4712747546)
store volatile %struct.ScmObj* %fargs47127, %struct.ScmObj** %stackaddr$prim61722, align 8
%stackaddr$makeclosure61723 = alloca %struct.ScmObj*, align 8
%fptrToInt61724 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48794 to i64
%ae48794 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt61724)
store volatile %struct.ScmObj* %ae48794, %struct.ScmObj** %stackaddr$makeclosure61723, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48794, %struct.ScmObj* %f47126, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48794, %struct.ScmObj* %_37last47119, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48794, %struct.ScmObj* %k47547, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48794, %struct.ScmObj* %fargs47127, i64 3)
%ae48796 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist59793$_37drop_45right471160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61725 = alloca %struct.ScmObj*, align 8
%argslist59793$_37drop_45right471161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48796, %struct.ScmObj* %argslist59793$_37drop_45right471160)
store volatile %struct.ScmObj* %argslist59793$_37drop_45right471161, %struct.ScmObj** %stackaddr$prim61725, align 8
%stackaddr$prim61726 = alloca %struct.ScmObj*, align 8
%argslist59793$_37drop_45right471162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs47127, %struct.ScmObj* %argslist59793$_37drop_45right471161)
store volatile %struct.ScmObj* %argslist59793$_37drop_45right471162, %struct.ScmObj** %stackaddr$prim61726, align 8
%stackaddr$prim61727 = alloca %struct.ScmObj*, align 8
%argslist59793$_37drop_45right471163 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48794, %struct.ScmObj* %argslist59793$_37drop_45right471162)
store volatile %struct.ScmObj* %argslist59793$_37drop_45right471163, %struct.ScmObj** %stackaddr$prim61727, align 8
%clofunc61728 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right47116)
musttail call tailcc void %clofunc61728(%struct.ScmObj* %_37drop_45right47116, %struct.ScmObj* %argslist59793$_37drop_45right471163)
ret void
}

define tailcc void @proc_clo$ae48794(%struct.ScmObj* %env$ae48794,%struct.ScmObj* %current_45args59782) {
%stackaddr$env-ref61729 = alloca %struct.ScmObj*, align 8
%f47126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48794, i64 0)
store %struct.ScmObj* %f47126, %struct.ScmObj** %stackaddr$env-ref61729
%stackaddr$env-ref61730 = alloca %struct.ScmObj*, align 8
%_37last47119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48794, i64 1)
store %struct.ScmObj* %_37last47119, %struct.ScmObj** %stackaddr$env-ref61730
%stackaddr$env-ref61731 = alloca %struct.ScmObj*, align 8
%k47547 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48794, i64 2)
store %struct.ScmObj* %k47547, %struct.ScmObj** %stackaddr$env-ref61731
%stackaddr$env-ref61732 = alloca %struct.ScmObj*, align 8
%fargs47127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48794, i64 3)
store %struct.ScmObj* %fargs47127, %struct.ScmObj** %stackaddr$env-ref61732
%stackaddr$prim61733 = alloca %struct.ScmObj*, align 8
%_95k47548 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59782)
store volatile %struct.ScmObj* %_95k47548, %struct.ScmObj** %stackaddr$prim61733, align 8
%stackaddr$prim61734 = alloca %struct.ScmObj*, align 8
%current_45args59783 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59782)
store volatile %struct.ScmObj* %current_45args59783, %struct.ScmObj** %stackaddr$prim61734, align 8
%stackaddr$prim61735 = alloca %struct.ScmObj*, align 8
%anf_45bind47274 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59783)
store volatile %struct.ScmObj* %anf_45bind47274, %struct.ScmObj** %stackaddr$prim61735, align 8
%stackaddr$makeclosure61736 = alloca %struct.ScmObj*, align 8
%fptrToInt61737 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48801 to i64
%ae48801 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt61737)
store volatile %struct.ScmObj* %ae48801, %struct.ScmObj** %stackaddr$makeclosure61736, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48801, %struct.ScmObj* %_37last47119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48801, %struct.ScmObj* %k47547, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48801, %struct.ScmObj* %fargs47127, i64 2)
%stackaddr$prim61738 = alloca %struct.ScmObj*, align 8
%cpsargs47552 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48801, %struct.ScmObj* %anf_45bind47274)
store volatile %struct.ScmObj* %cpsargs47552, %struct.ScmObj** %stackaddr$prim61738, align 8
%clofunc61739 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47126)
musttail call tailcc void %clofunc61739(%struct.ScmObj* %f47126, %struct.ScmObj* %cpsargs47552)
ret void
}

define tailcc void @proc_clo$ae48801(%struct.ScmObj* %env$ae48801,%struct.ScmObj* %current_45args59785) {
%stackaddr$env-ref61740 = alloca %struct.ScmObj*, align 8
%_37last47119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48801, i64 0)
store %struct.ScmObj* %_37last47119, %struct.ScmObj** %stackaddr$env-ref61740
%stackaddr$env-ref61741 = alloca %struct.ScmObj*, align 8
%k47547 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48801, i64 1)
store %struct.ScmObj* %k47547, %struct.ScmObj** %stackaddr$env-ref61741
%stackaddr$env-ref61742 = alloca %struct.ScmObj*, align 8
%fargs47127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48801, i64 2)
store %struct.ScmObj* %fargs47127, %struct.ScmObj** %stackaddr$env-ref61742
%stackaddr$prim61743 = alloca %struct.ScmObj*, align 8
%_95k47549 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59785)
store volatile %struct.ScmObj* %_95k47549, %struct.ScmObj** %stackaddr$prim61743, align 8
%stackaddr$prim61744 = alloca %struct.ScmObj*, align 8
%current_45args59786 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59785)
store volatile %struct.ScmObj* %current_45args59786, %struct.ScmObj** %stackaddr$prim61744, align 8
%stackaddr$prim61745 = alloca %struct.ScmObj*, align 8
%anf_45bind47275 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59786)
store volatile %struct.ScmObj* %anf_45bind47275, %struct.ScmObj** %stackaddr$prim61745, align 8
%stackaddr$makeclosure61746 = alloca %struct.ScmObj*, align 8
%fptrToInt61747 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48806 to i64
%ae48806 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt61747)
store volatile %struct.ScmObj* %ae48806, %struct.ScmObj** %stackaddr$makeclosure61746, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48806, %struct.ScmObj* %k47547, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48806, %struct.ScmObj* %anf_45bind47275, i64 1)
%argslist59792$_37last471190 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61748 = alloca %struct.ScmObj*, align 8
%argslist59792$_37last471191 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs47127, %struct.ScmObj* %argslist59792$_37last471190)
store volatile %struct.ScmObj* %argslist59792$_37last471191, %struct.ScmObj** %stackaddr$prim61748, align 8
%stackaddr$prim61749 = alloca %struct.ScmObj*, align 8
%argslist59792$_37last471192 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48806, %struct.ScmObj* %argslist59792$_37last471191)
store volatile %struct.ScmObj* %argslist59792$_37last471192, %struct.ScmObj** %stackaddr$prim61749, align 8
%clofunc61750 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last47119)
musttail call tailcc void %clofunc61750(%struct.ScmObj* %_37last47119, %struct.ScmObj* %argslist59792$_37last471192)
ret void
}

define tailcc void @proc_clo$ae48806(%struct.ScmObj* %env$ae48806,%struct.ScmObj* %current_45args59788) {
%stackaddr$env-ref61751 = alloca %struct.ScmObj*, align 8
%k47547 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48806, i64 0)
store %struct.ScmObj* %k47547, %struct.ScmObj** %stackaddr$env-ref61751
%stackaddr$env-ref61752 = alloca %struct.ScmObj*, align 8
%anf_45bind47275 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48806, i64 1)
store %struct.ScmObj* %anf_45bind47275, %struct.ScmObj** %stackaddr$env-ref61752
%stackaddr$prim61753 = alloca %struct.ScmObj*, align 8
%_95k47550 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59788)
store volatile %struct.ScmObj* %_95k47550, %struct.ScmObj** %stackaddr$prim61753, align 8
%stackaddr$prim61754 = alloca %struct.ScmObj*, align 8
%current_45args59789 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59788)
store volatile %struct.ScmObj* %current_45args59789, %struct.ScmObj** %stackaddr$prim61754, align 8
%stackaddr$prim61755 = alloca %struct.ScmObj*, align 8
%anf_45bind47276 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59789)
store volatile %struct.ScmObj* %anf_45bind47276, %struct.ScmObj** %stackaddr$prim61755, align 8
%stackaddr$prim61756 = alloca %struct.ScmObj*, align 8
%cpsprim47551 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47275, %struct.ScmObj* %anf_45bind47276)
store volatile %struct.ScmObj* %cpsprim47551, %struct.ScmObj** %stackaddr$prim61756, align 8
%ae48811 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist59791$k475470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61757 = alloca %struct.ScmObj*, align 8
%argslist59791$k475471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47551, %struct.ScmObj* %argslist59791$k475470)
store volatile %struct.ScmObj* %argslist59791$k475471, %struct.ScmObj** %stackaddr$prim61757, align 8
%stackaddr$prim61758 = alloca %struct.ScmObj*, align 8
%argslist59791$k475472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48811, %struct.ScmObj* %argslist59791$k475471)
store volatile %struct.ScmObj* %argslist59791$k475472, %struct.ScmObj** %stackaddr$prim61758, align 8
%clofunc61759 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47547)
musttail call tailcc void %clofunc61759(%struct.ScmObj* %k47547, %struct.ScmObj* %argslist59791$k475472)
ret void
}

define tailcc void @proc_clo$ae48706(%struct.ScmObj* %env$ae48706,%struct.ScmObj* %current_45args59796) {
%stackaddr$env-ref61760 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48706, i64 0)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref61760
%stackaddr$prim61761 = alloca %struct.ScmObj*, align 8
%k47553 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59796)
store volatile %struct.ScmObj* %k47553, %struct.ScmObj** %stackaddr$prim61761, align 8
%stackaddr$prim61762 = alloca %struct.ScmObj*, align 8
%current_45args59797 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59796)
store volatile %struct.ScmObj* %current_45args59797, %struct.ScmObj** %stackaddr$prim61762, align 8
%stackaddr$prim61763 = alloca %struct.ScmObj*, align 8
%f47130 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59797)
store volatile %struct.ScmObj* %f47130, %struct.ScmObj** %stackaddr$prim61763, align 8
%stackaddr$prim61764 = alloca %struct.ScmObj*, align 8
%current_45args59798 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59797)
store volatile %struct.ScmObj* %current_45args59798, %struct.ScmObj** %stackaddr$prim61764, align 8
%stackaddr$prim61765 = alloca %struct.ScmObj*, align 8
%lst47129 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59798)
store volatile %struct.ScmObj* %lst47129, %struct.ScmObj** %stackaddr$prim61765, align 8
%stackaddr$makeclosure61766 = alloca %struct.ScmObj*, align 8
%fptrToInt61767 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48707 to i64
%ae48707 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt61767)
store volatile %struct.ScmObj* %ae48707, %struct.ScmObj** %stackaddr$makeclosure61766, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48707, %struct.ScmObj* %lst47129, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48707, %struct.ScmObj* %_37foldr147097, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48707, %struct.ScmObj* %k47553, i64 2)
%ae48708 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61768 = alloca %struct.ScmObj*, align 8
%fptrToInt61769 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48709 to i64
%ae48709 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt61769)
store volatile %struct.ScmObj* %ae48709, %struct.ScmObj** %stackaddr$makeclosure61768, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48709, %struct.ScmObj* %f47130, i64 0)
%argslist59813$ae487070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61770 = alloca %struct.ScmObj*, align 8
%argslist59813$ae487071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48709, %struct.ScmObj* %argslist59813$ae487070)
store volatile %struct.ScmObj* %argslist59813$ae487071, %struct.ScmObj** %stackaddr$prim61770, align 8
%stackaddr$prim61771 = alloca %struct.ScmObj*, align 8
%argslist59813$ae487072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48708, %struct.ScmObj* %argslist59813$ae487071)
store volatile %struct.ScmObj* %argslist59813$ae487072, %struct.ScmObj** %stackaddr$prim61771, align 8
%clofunc61772 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48707)
musttail call tailcc void %clofunc61772(%struct.ScmObj* %ae48707, %struct.ScmObj* %argslist59813$ae487072)
ret void
}

define tailcc void @proc_clo$ae48707(%struct.ScmObj* %env$ae48707,%struct.ScmObj* %current_45args59800) {
%stackaddr$env-ref61773 = alloca %struct.ScmObj*, align 8
%lst47129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48707, i64 0)
store %struct.ScmObj* %lst47129, %struct.ScmObj** %stackaddr$env-ref61773
%stackaddr$env-ref61774 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48707, i64 1)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref61774
%stackaddr$env-ref61775 = alloca %struct.ScmObj*, align 8
%k47553 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48707, i64 2)
store %struct.ScmObj* %k47553, %struct.ScmObj** %stackaddr$env-ref61775
%stackaddr$prim61776 = alloca %struct.ScmObj*, align 8
%_95k47554 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59800)
store volatile %struct.ScmObj* %_95k47554, %struct.ScmObj** %stackaddr$prim61776, align 8
%stackaddr$prim61777 = alloca %struct.ScmObj*, align 8
%current_45args59801 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59800)
store volatile %struct.ScmObj* %current_45args59801, %struct.ScmObj** %stackaddr$prim61777, align 8
%stackaddr$prim61778 = alloca %struct.ScmObj*, align 8
%anf_45bind47273 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59801)
store volatile %struct.ScmObj* %anf_45bind47273, %struct.ScmObj** %stackaddr$prim61778, align 8
%ae48741 = call %struct.ScmObj* @const_init_null()
%argslist59803$_37foldr1470970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61779 = alloca %struct.ScmObj*, align 8
%argslist59803$_37foldr1470971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47129, %struct.ScmObj* %argslist59803$_37foldr1470970)
store volatile %struct.ScmObj* %argslist59803$_37foldr1470971, %struct.ScmObj** %stackaddr$prim61779, align 8
%stackaddr$prim61780 = alloca %struct.ScmObj*, align 8
%argslist59803$_37foldr1470972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48741, %struct.ScmObj* %argslist59803$_37foldr1470971)
store volatile %struct.ScmObj* %argslist59803$_37foldr1470972, %struct.ScmObj** %stackaddr$prim61780, align 8
%stackaddr$prim61781 = alloca %struct.ScmObj*, align 8
%argslist59803$_37foldr1470973 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47273, %struct.ScmObj* %argslist59803$_37foldr1470972)
store volatile %struct.ScmObj* %argslist59803$_37foldr1470973, %struct.ScmObj** %stackaddr$prim61781, align 8
%stackaddr$prim61782 = alloca %struct.ScmObj*, align 8
%argslist59803$_37foldr1470974 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47553, %struct.ScmObj* %argslist59803$_37foldr1470973)
store volatile %struct.ScmObj* %argslist59803$_37foldr1470974, %struct.ScmObj** %stackaddr$prim61782, align 8
%clofunc61783 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147097)
musttail call tailcc void %clofunc61783(%struct.ScmObj* %_37foldr147097, %struct.ScmObj* %argslist59803$_37foldr1470974)
ret void
}

define tailcc void @proc_clo$ae48709(%struct.ScmObj* %env$ae48709,%struct.ScmObj* %current_45args59804) {
%stackaddr$env-ref61784 = alloca %struct.ScmObj*, align 8
%f47130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48709, i64 0)
store %struct.ScmObj* %f47130, %struct.ScmObj** %stackaddr$env-ref61784
%stackaddr$prim61785 = alloca %struct.ScmObj*, align 8
%k47555 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59804)
store volatile %struct.ScmObj* %k47555, %struct.ScmObj** %stackaddr$prim61785, align 8
%stackaddr$prim61786 = alloca %struct.ScmObj*, align 8
%current_45args59805 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59804)
store volatile %struct.ScmObj* %current_45args59805, %struct.ScmObj** %stackaddr$prim61786, align 8
%stackaddr$prim61787 = alloca %struct.ScmObj*, align 8
%v47132 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59805)
store volatile %struct.ScmObj* %v47132, %struct.ScmObj** %stackaddr$prim61787, align 8
%stackaddr$prim61788 = alloca %struct.ScmObj*, align 8
%current_45args59806 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59805)
store volatile %struct.ScmObj* %current_45args59806, %struct.ScmObj** %stackaddr$prim61788, align 8
%stackaddr$prim61789 = alloca %struct.ScmObj*, align 8
%r47131 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59806)
store volatile %struct.ScmObj* %r47131, %struct.ScmObj** %stackaddr$prim61789, align 8
%stackaddr$makeclosure61790 = alloca %struct.ScmObj*, align 8
%fptrToInt61791 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48711 to i64
%ae48711 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt61791)
store volatile %struct.ScmObj* %ae48711, %struct.ScmObj** %stackaddr$makeclosure61790, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48711, %struct.ScmObj* %k47555, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48711, %struct.ScmObj* %r47131, i64 1)
%argslist59812$f471300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61792 = alloca %struct.ScmObj*, align 8
%argslist59812$f471301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v47132, %struct.ScmObj* %argslist59812$f471300)
store volatile %struct.ScmObj* %argslist59812$f471301, %struct.ScmObj** %stackaddr$prim61792, align 8
%stackaddr$prim61793 = alloca %struct.ScmObj*, align 8
%argslist59812$f471302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48711, %struct.ScmObj* %argslist59812$f471301)
store volatile %struct.ScmObj* %argslist59812$f471302, %struct.ScmObj** %stackaddr$prim61793, align 8
%clofunc61794 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47130)
musttail call tailcc void %clofunc61794(%struct.ScmObj* %f47130, %struct.ScmObj* %argslist59812$f471302)
ret void
}

define tailcc void @proc_clo$ae48711(%struct.ScmObj* %env$ae48711,%struct.ScmObj* %current_45args59808) {
%stackaddr$env-ref61795 = alloca %struct.ScmObj*, align 8
%k47555 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48711, i64 0)
store %struct.ScmObj* %k47555, %struct.ScmObj** %stackaddr$env-ref61795
%stackaddr$env-ref61796 = alloca %struct.ScmObj*, align 8
%r47131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48711, i64 1)
store %struct.ScmObj* %r47131, %struct.ScmObj** %stackaddr$env-ref61796
%stackaddr$prim61797 = alloca %struct.ScmObj*, align 8
%_95k47556 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59808)
store volatile %struct.ScmObj* %_95k47556, %struct.ScmObj** %stackaddr$prim61797, align 8
%stackaddr$prim61798 = alloca %struct.ScmObj*, align 8
%current_45args59809 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59808)
store volatile %struct.ScmObj* %current_45args59809, %struct.ScmObj** %stackaddr$prim61798, align 8
%stackaddr$prim61799 = alloca %struct.ScmObj*, align 8
%anf_45bind47272 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59809)
store volatile %struct.ScmObj* %anf_45bind47272, %struct.ScmObj** %stackaddr$prim61799, align 8
%stackaddr$prim61800 = alloca %struct.ScmObj*, align 8
%cpsprim47557 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47272, %struct.ScmObj* %r47131)
store volatile %struct.ScmObj* %cpsprim47557, %struct.ScmObj** %stackaddr$prim61800, align 8
%ae48716 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist59811$k475550 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61801 = alloca %struct.ScmObj*, align 8
%argslist59811$k475551 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47557, %struct.ScmObj* %argslist59811$k475550)
store volatile %struct.ScmObj* %argslist59811$k475551, %struct.ScmObj** %stackaddr$prim61801, align 8
%stackaddr$prim61802 = alloca %struct.ScmObj*, align 8
%argslist59811$k475552 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48716, %struct.ScmObj* %argslist59811$k475551)
store volatile %struct.ScmObj* %argslist59811$k475552, %struct.ScmObj** %stackaddr$prim61802, align 8
%clofunc61803 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47555)
musttail call tailcc void %clofunc61803(%struct.ScmObj* %k47555, %struct.ScmObj* %argslist59811$k475552)
ret void
}

define tailcc void @proc_clo$ae48320(%struct.ScmObj* %env$ae48320,%struct.ScmObj* %current_45args59816) {
%stackaddr$env-ref61804 = alloca %struct.ScmObj*, align 8
%_37map147093 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48320, i64 0)
store %struct.ScmObj* %_37map147093, %struct.ScmObj** %stackaddr$env-ref61804
%stackaddr$env-ref61805 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48320, i64 1)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref61805
%stackaddr$prim61806 = alloca %struct.ScmObj*, align 8
%k47558 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59816)
store volatile %struct.ScmObj* %k47558, %struct.ScmObj** %stackaddr$prim61806, align 8
%stackaddr$prim61807 = alloca %struct.ScmObj*, align 8
%current_45args59817 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59816)
store volatile %struct.ScmObj* %current_45args59817, %struct.ScmObj** %stackaddr$prim61807, align 8
%stackaddr$prim61808 = alloca %struct.ScmObj*, align 8
%_37foldr47103 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59817)
store volatile %struct.ScmObj* %_37foldr47103, %struct.ScmObj** %stackaddr$prim61808, align 8
%ae48322 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61809 = alloca %struct.ScmObj*, align 8
%fptrToInt61810 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48323 to i64
%ae48323 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt61810)
store volatile %struct.ScmObj* %ae48323, %struct.ScmObj** %stackaddr$makeclosure61809, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48323, %struct.ScmObj* %_37map147093, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48323, %struct.ScmObj* %_37foldr47103, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48323, %struct.ScmObj* %_37foldr147097, i64 2)
%argslist59874$k475580 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61811 = alloca %struct.ScmObj*, align 8
%argslist59874$k475581 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48323, %struct.ScmObj* %argslist59874$k475580)
store volatile %struct.ScmObj* %argslist59874$k475581, %struct.ScmObj** %stackaddr$prim61811, align 8
%stackaddr$prim61812 = alloca %struct.ScmObj*, align 8
%argslist59874$k475582 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48322, %struct.ScmObj* %argslist59874$k475581)
store volatile %struct.ScmObj* %argslist59874$k475582, %struct.ScmObj** %stackaddr$prim61812, align 8
%clofunc61813 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47558)
musttail call tailcc void %clofunc61813(%struct.ScmObj* %k47558, %struct.ScmObj* %argslist59874$k475582)
ret void
}

define tailcc void @proc_clo$ae48323(%struct.ScmObj* %env$ae48323,%struct.ScmObj* %args4710447559) {
%stackaddr$env-ref61814 = alloca %struct.ScmObj*, align 8
%_37map147093 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48323, i64 0)
store %struct.ScmObj* %_37map147093, %struct.ScmObj** %stackaddr$env-ref61814
%stackaddr$env-ref61815 = alloca %struct.ScmObj*, align 8
%_37foldr47103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48323, i64 1)
store %struct.ScmObj* %_37foldr47103, %struct.ScmObj** %stackaddr$env-ref61815
%stackaddr$env-ref61816 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48323, i64 2)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref61816
%stackaddr$prim61817 = alloca %struct.ScmObj*, align 8
%k47560 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4710447559)
store volatile %struct.ScmObj* %k47560, %struct.ScmObj** %stackaddr$prim61817, align 8
%stackaddr$prim61818 = alloca %struct.ScmObj*, align 8
%args47104 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4710447559)
store volatile %struct.ScmObj* %args47104, %struct.ScmObj** %stackaddr$prim61818, align 8
%stackaddr$prim61819 = alloca %struct.ScmObj*, align 8
%f47107 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47104)
store volatile %struct.ScmObj* %f47107, %struct.ScmObj** %stackaddr$prim61819, align 8
%stackaddr$prim61820 = alloca %struct.ScmObj*, align 8
%anf_45bind47259 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47104)
store volatile %struct.ScmObj* %anf_45bind47259, %struct.ScmObj** %stackaddr$prim61820, align 8
%stackaddr$prim61821 = alloca %struct.ScmObj*, align 8
%acc47106 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47259)
store volatile %struct.ScmObj* %acc47106, %struct.ScmObj** %stackaddr$prim61821, align 8
%stackaddr$prim61822 = alloca %struct.ScmObj*, align 8
%anf_45bind47260 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47104)
store volatile %struct.ScmObj* %anf_45bind47260, %struct.ScmObj** %stackaddr$prim61822, align 8
%stackaddr$prim61823 = alloca %struct.ScmObj*, align 8
%lsts47105 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47260)
store volatile %struct.ScmObj* %lsts47105, %struct.ScmObj** %stackaddr$prim61823, align 8
%stackaddr$makeclosure61824 = alloca %struct.ScmObj*, align 8
%fptrToInt61825 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48331 to i64
%ae48331 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt61825)
store volatile %struct.ScmObj* %ae48331, %struct.ScmObj** %stackaddr$makeclosure61824, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48331, %struct.ScmObj* %_37map147093, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48331, %struct.ScmObj* %f47107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48331, %struct.ScmObj* %acc47106, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48331, %struct.ScmObj* %lsts47105, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48331, %struct.ScmObj* %_37foldr47103, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48331, %struct.ScmObj* %_37foldr147097, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48331, %struct.ScmObj* %k47560, i64 6)
%ae48332 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61826 = alloca %struct.ScmObj*, align 8
%fptrToInt61827 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48333 to i64
%ae48333 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt61827)
store volatile %struct.ScmObj* %ae48333, %struct.ScmObj** %stackaddr$makeclosure61826, align 8
%argslist59873$ae483310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61828 = alloca %struct.ScmObj*, align 8
%argslist59873$ae483311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48333, %struct.ScmObj* %argslist59873$ae483310)
store volatile %struct.ScmObj* %argslist59873$ae483311, %struct.ScmObj** %stackaddr$prim61828, align 8
%stackaddr$prim61829 = alloca %struct.ScmObj*, align 8
%argslist59873$ae483312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48332, %struct.ScmObj* %argslist59873$ae483311)
store volatile %struct.ScmObj* %argslist59873$ae483312, %struct.ScmObj** %stackaddr$prim61829, align 8
%clofunc61830 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48331)
musttail call tailcc void %clofunc61830(%struct.ScmObj* %ae48331, %struct.ScmObj* %argslist59873$ae483312)
ret void
}

define tailcc void @proc_clo$ae48331(%struct.ScmObj* %env$ae48331,%struct.ScmObj* %current_45args59819) {
%stackaddr$env-ref61831 = alloca %struct.ScmObj*, align 8
%_37map147093 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48331, i64 0)
store %struct.ScmObj* %_37map147093, %struct.ScmObj** %stackaddr$env-ref61831
%stackaddr$env-ref61832 = alloca %struct.ScmObj*, align 8
%f47107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48331, i64 1)
store %struct.ScmObj* %f47107, %struct.ScmObj** %stackaddr$env-ref61832
%stackaddr$env-ref61833 = alloca %struct.ScmObj*, align 8
%acc47106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48331, i64 2)
store %struct.ScmObj* %acc47106, %struct.ScmObj** %stackaddr$env-ref61833
%stackaddr$env-ref61834 = alloca %struct.ScmObj*, align 8
%lsts47105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48331, i64 3)
store %struct.ScmObj* %lsts47105, %struct.ScmObj** %stackaddr$env-ref61834
%stackaddr$env-ref61835 = alloca %struct.ScmObj*, align 8
%_37foldr47103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48331, i64 4)
store %struct.ScmObj* %_37foldr47103, %struct.ScmObj** %stackaddr$env-ref61835
%stackaddr$env-ref61836 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48331, i64 5)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref61836
%stackaddr$env-ref61837 = alloca %struct.ScmObj*, align 8
%k47560 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48331, i64 6)
store %struct.ScmObj* %k47560, %struct.ScmObj** %stackaddr$env-ref61837
%stackaddr$prim61838 = alloca %struct.ScmObj*, align 8
%_95k47561 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59819)
store volatile %struct.ScmObj* %_95k47561, %struct.ScmObj** %stackaddr$prim61838, align 8
%stackaddr$prim61839 = alloca %struct.ScmObj*, align 8
%current_45args59820 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59819)
store volatile %struct.ScmObj* %current_45args59820, %struct.ScmObj** %stackaddr$prim61839, align 8
%stackaddr$prim61840 = alloca %struct.ScmObj*, align 8
%anf_45bind47261 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59820)
store volatile %struct.ScmObj* %anf_45bind47261, %struct.ScmObj** %stackaddr$prim61840, align 8
%stackaddr$makeclosure61841 = alloca %struct.ScmObj*, align 8
%fptrToInt61842 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48363 to i64
%ae48363 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt61842)
store volatile %struct.ScmObj* %ae48363, %struct.ScmObj** %stackaddr$makeclosure61841, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48363, %struct.ScmObj* %_37map147093, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48363, %struct.ScmObj* %f47107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48363, %struct.ScmObj* %acc47106, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48363, %struct.ScmObj* %lsts47105, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48363, %struct.ScmObj* %_37foldr47103, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48363, %struct.ScmObj* %_37foldr147097, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48363, %struct.ScmObj* %k47560, i64 6)
%ae48365 = call %struct.ScmObj* @const_init_false()
%argslist59866$_37foldr1470970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61843 = alloca %struct.ScmObj*, align 8
%argslist59866$_37foldr1470971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47105, %struct.ScmObj* %argslist59866$_37foldr1470970)
store volatile %struct.ScmObj* %argslist59866$_37foldr1470971, %struct.ScmObj** %stackaddr$prim61843, align 8
%stackaddr$prim61844 = alloca %struct.ScmObj*, align 8
%argslist59866$_37foldr1470972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48365, %struct.ScmObj* %argslist59866$_37foldr1470971)
store volatile %struct.ScmObj* %argslist59866$_37foldr1470972, %struct.ScmObj** %stackaddr$prim61844, align 8
%stackaddr$prim61845 = alloca %struct.ScmObj*, align 8
%argslist59866$_37foldr1470973 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47261, %struct.ScmObj* %argslist59866$_37foldr1470972)
store volatile %struct.ScmObj* %argslist59866$_37foldr1470973, %struct.ScmObj** %stackaddr$prim61845, align 8
%stackaddr$prim61846 = alloca %struct.ScmObj*, align 8
%argslist59866$_37foldr1470974 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48363, %struct.ScmObj* %argslist59866$_37foldr1470973)
store volatile %struct.ScmObj* %argslist59866$_37foldr1470974, %struct.ScmObj** %stackaddr$prim61846, align 8
%clofunc61847 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147097)
musttail call tailcc void %clofunc61847(%struct.ScmObj* %_37foldr147097, %struct.ScmObj* %argslist59866$_37foldr1470974)
ret void
}

define tailcc void @proc_clo$ae48363(%struct.ScmObj* %env$ae48363,%struct.ScmObj* %current_45args59822) {
%stackaddr$env-ref61848 = alloca %struct.ScmObj*, align 8
%_37map147093 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48363, i64 0)
store %struct.ScmObj* %_37map147093, %struct.ScmObj** %stackaddr$env-ref61848
%stackaddr$env-ref61849 = alloca %struct.ScmObj*, align 8
%f47107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48363, i64 1)
store %struct.ScmObj* %f47107, %struct.ScmObj** %stackaddr$env-ref61849
%stackaddr$env-ref61850 = alloca %struct.ScmObj*, align 8
%acc47106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48363, i64 2)
store %struct.ScmObj* %acc47106, %struct.ScmObj** %stackaddr$env-ref61850
%stackaddr$env-ref61851 = alloca %struct.ScmObj*, align 8
%lsts47105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48363, i64 3)
store %struct.ScmObj* %lsts47105, %struct.ScmObj** %stackaddr$env-ref61851
%stackaddr$env-ref61852 = alloca %struct.ScmObj*, align 8
%_37foldr47103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48363, i64 4)
store %struct.ScmObj* %_37foldr47103, %struct.ScmObj** %stackaddr$env-ref61852
%stackaddr$env-ref61853 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48363, i64 5)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref61853
%stackaddr$env-ref61854 = alloca %struct.ScmObj*, align 8
%k47560 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48363, i64 6)
store %struct.ScmObj* %k47560, %struct.ScmObj** %stackaddr$env-ref61854
%stackaddr$prim61855 = alloca %struct.ScmObj*, align 8
%_95k47562 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59822)
store volatile %struct.ScmObj* %_95k47562, %struct.ScmObj** %stackaddr$prim61855, align 8
%stackaddr$prim61856 = alloca %struct.ScmObj*, align 8
%current_45args59823 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59822)
store volatile %struct.ScmObj* %current_45args59823, %struct.ScmObj** %stackaddr$prim61856, align 8
%stackaddr$prim61857 = alloca %struct.ScmObj*, align 8
%anf_45bind47262 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59823)
store volatile %struct.ScmObj* %anf_45bind47262, %struct.ScmObj** %stackaddr$prim61857, align 8
%truthy$cmp61858 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47262)
%cmp$cmp61858 = icmp eq i64 %truthy$cmp61858, 1
br i1 %cmp$cmp61858, label %truebranch$cmp61858, label %falsebranch$cmp61858
truebranch$cmp61858:
%ae48374 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist59825$k475600 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61859 = alloca %struct.ScmObj*, align 8
%argslist59825$k475601 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47106, %struct.ScmObj* %argslist59825$k475600)
store volatile %struct.ScmObj* %argslist59825$k475601, %struct.ScmObj** %stackaddr$prim61859, align 8
%stackaddr$prim61860 = alloca %struct.ScmObj*, align 8
%argslist59825$k475602 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48374, %struct.ScmObj* %argslist59825$k475601)
store volatile %struct.ScmObj* %argslist59825$k475602, %struct.ScmObj** %stackaddr$prim61860, align 8
%clofunc61861 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47560)
musttail call tailcc void %clofunc61861(%struct.ScmObj* %k47560, %struct.ScmObj* %argslist59825$k475602)
ret void
falsebranch$cmp61858:
%stackaddr$makeclosure61862 = alloca %struct.ScmObj*, align 8
%fptrToInt61863 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48379 to i64
%ae48379 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt61863)
store volatile %struct.ScmObj* %ae48379, %struct.ScmObj** %stackaddr$makeclosure61862, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48379, %struct.ScmObj* %_37map147093, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48379, %struct.ScmObj* %f47107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48379, %struct.ScmObj* %acc47106, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48379, %struct.ScmObj* %lsts47105, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48379, %struct.ScmObj* %_37foldr47103, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48379, %struct.ScmObj* %_37foldr147097, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48379, %struct.ScmObj* %k47560, i64 6)
%ae48380 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61864 = alloca %struct.ScmObj*, align 8
%fptrToInt61865 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48381 to i64
%ae48381 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt61865)
store volatile %struct.ScmObj* %ae48381, %struct.ScmObj** %stackaddr$makeclosure61864, align 8
%argslist59865$ae483790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61866 = alloca %struct.ScmObj*, align 8
%argslist59865$ae483791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48381, %struct.ScmObj* %argslist59865$ae483790)
store volatile %struct.ScmObj* %argslist59865$ae483791, %struct.ScmObj** %stackaddr$prim61866, align 8
%stackaddr$prim61867 = alloca %struct.ScmObj*, align 8
%argslist59865$ae483792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48380, %struct.ScmObj* %argslist59865$ae483791)
store volatile %struct.ScmObj* %argslist59865$ae483792, %struct.ScmObj** %stackaddr$prim61867, align 8
%clofunc61868 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48379)
musttail call tailcc void %clofunc61868(%struct.ScmObj* %ae48379, %struct.ScmObj* %argslist59865$ae483792)
ret void
}

define tailcc void @proc_clo$ae48379(%struct.ScmObj* %env$ae48379,%struct.ScmObj* %current_45args59826) {
%stackaddr$env-ref61869 = alloca %struct.ScmObj*, align 8
%_37map147093 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48379, i64 0)
store %struct.ScmObj* %_37map147093, %struct.ScmObj** %stackaddr$env-ref61869
%stackaddr$env-ref61870 = alloca %struct.ScmObj*, align 8
%f47107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48379, i64 1)
store %struct.ScmObj* %f47107, %struct.ScmObj** %stackaddr$env-ref61870
%stackaddr$env-ref61871 = alloca %struct.ScmObj*, align 8
%acc47106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48379, i64 2)
store %struct.ScmObj* %acc47106, %struct.ScmObj** %stackaddr$env-ref61871
%stackaddr$env-ref61872 = alloca %struct.ScmObj*, align 8
%lsts47105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48379, i64 3)
store %struct.ScmObj* %lsts47105, %struct.ScmObj** %stackaddr$env-ref61872
%stackaddr$env-ref61873 = alloca %struct.ScmObj*, align 8
%_37foldr47103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48379, i64 4)
store %struct.ScmObj* %_37foldr47103, %struct.ScmObj** %stackaddr$env-ref61873
%stackaddr$env-ref61874 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48379, i64 5)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref61874
%stackaddr$env-ref61875 = alloca %struct.ScmObj*, align 8
%k47560 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48379, i64 6)
store %struct.ScmObj* %k47560, %struct.ScmObj** %stackaddr$env-ref61875
%stackaddr$prim61876 = alloca %struct.ScmObj*, align 8
%_95k47563 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59826)
store volatile %struct.ScmObj* %_95k47563, %struct.ScmObj** %stackaddr$prim61876, align 8
%stackaddr$prim61877 = alloca %struct.ScmObj*, align 8
%current_45args59827 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59826)
store volatile %struct.ScmObj* %current_45args59827, %struct.ScmObj** %stackaddr$prim61877, align 8
%stackaddr$prim61878 = alloca %struct.ScmObj*, align 8
%anf_45bind47263 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59827)
store volatile %struct.ScmObj* %anf_45bind47263, %struct.ScmObj** %stackaddr$prim61878, align 8
%stackaddr$makeclosure61879 = alloca %struct.ScmObj*, align 8
%fptrToInt61880 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48400 to i64
%ae48400 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt61880)
store volatile %struct.ScmObj* %ae48400, %struct.ScmObj** %stackaddr$makeclosure61879, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48400, %struct.ScmObj* %_37map147093, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48400, %struct.ScmObj* %f47107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48400, %struct.ScmObj* %acc47106, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48400, %struct.ScmObj* %lsts47105, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48400, %struct.ScmObj* %_37foldr47103, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48400, %struct.ScmObj* %_37foldr147097, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48400, %struct.ScmObj* %k47560, i64 6)
%argslist59860$_37map1470930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61881 = alloca %struct.ScmObj*, align 8
%argslist59860$_37map1470931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47105, %struct.ScmObj* %argslist59860$_37map1470930)
store volatile %struct.ScmObj* %argslist59860$_37map1470931, %struct.ScmObj** %stackaddr$prim61881, align 8
%stackaddr$prim61882 = alloca %struct.ScmObj*, align 8
%argslist59860$_37map1470932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47263, %struct.ScmObj* %argslist59860$_37map1470931)
store volatile %struct.ScmObj* %argslist59860$_37map1470932, %struct.ScmObj** %stackaddr$prim61882, align 8
%stackaddr$prim61883 = alloca %struct.ScmObj*, align 8
%argslist59860$_37map1470933 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48400, %struct.ScmObj* %argslist59860$_37map1470932)
store volatile %struct.ScmObj* %argslist59860$_37map1470933, %struct.ScmObj** %stackaddr$prim61883, align 8
%clofunc61884 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147093)
musttail call tailcc void %clofunc61884(%struct.ScmObj* %_37map147093, %struct.ScmObj* %argslist59860$_37map1470933)
ret void
}

define tailcc void @proc_clo$ae48400(%struct.ScmObj* %env$ae48400,%struct.ScmObj* %current_45args59829) {
%stackaddr$env-ref61885 = alloca %struct.ScmObj*, align 8
%_37map147093 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48400, i64 0)
store %struct.ScmObj* %_37map147093, %struct.ScmObj** %stackaddr$env-ref61885
%stackaddr$env-ref61886 = alloca %struct.ScmObj*, align 8
%f47107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48400, i64 1)
store %struct.ScmObj* %f47107, %struct.ScmObj** %stackaddr$env-ref61886
%stackaddr$env-ref61887 = alloca %struct.ScmObj*, align 8
%acc47106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48400, i64 2)
store %struct.ScmObj* %acc47106, %struct.ScmObj** %stackaddr$env-ref61887
%stackaddr$env-ref61888 = alloca %struct.ScmObj*, align 8
%lsts47105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48400, i64 3)
store %struct.ScmObj* %lsts47105, %struct.ScmObj** %stackaddr$env-ref61888
%stackaddr$env-ref61889 = alloca %struct.ScmObj*, align 8
%_37foldr47103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48400, i64 4)
store %struct.ScmObj* %_37foldr47103, %struct.ScmObj** %stackaddr$env-ref61889
%stackaddr$env-ref61890 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48400, i64 5)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref61890
%stackaddr$env-ref61891 = alloca %struct.ScmObj*, align 8
%k47560 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48400, i64 6)
store %struct.ScmObj* %k47560, %struct.ScmObj** %stackaddr$env-ref61891
%stackaddr$prim61892 = alloca %struct.ScmObj*, align 8
%_95k47564 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59829)
store volatile %struct.ScmObj* %_95k47564, %struct.ScmObj** %stackaddr$prim61892, align 8
%stackaddr$prim61893 = alloca %struct.ScmObj*, align 8
%current_45args59830 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59829)
store volatile %struct.ScmObj* %current_45args59830, %struct.ScmObj** %stackaddr$prim61893, align 8
%stackaddr$prim61894 = alloca %struct.ScmObj*, align 8
%lsts_4347112 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59830)
store volatile %struct.ScmObj* %lsts_4347112, %struct.ScmObj** %stackaddr$prim61894, align 8
%stackaddr$makeclosure61895 = alloca %struct.ScmObj*, align 8
%fptrToInt61896 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48403 to i64
%ae48403 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt61896)
store volatile %struct.ScmObj* %ae48403, %struct.ScmObj** %stackaddr$makeclosure61895, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48403, %struct.ScmObj* %lsts_4347112, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48403, %struct.ScmObj* %k47560, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48403, %struct.ScmObj* %_37map147093, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48403, %struct.ScmObj* %f47107, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48403, %struct.ScmObj* %acc47106, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48403, %struct.ScmObj* %lsts47105, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48403, %struct.ScmObj* %_37foldr47103, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae48403, %struct.ScmObj* %_37foldr147097, i64 7)
%ae48404 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61897 = alloca %struct.ScmObj*, align 8
%fptrToInt61898 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48405 to i64
%ae48405 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt61898)
store volatile %struct.ScmObj* %ae48405, %struct.ScmObj** %stackaddr$makeclosure61897, align 8
%argslist59859$ae484030 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61899 = alloca %struct.ScmObj*, align 8
%argslist59859$ae484031 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48405, %struct.ScmObj* %argslist59859$ae484030)
store volatile %struct.ScmObj* %argslist59859$ae484031, %struct.ScmObj** %stackaddr$prim61899, align 8
%stackaddr$prim61900 = alloca %struct.ScmObj*, align 8
%argslist59859$ae484032 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48404, %struct.ScmObj* %argslist59859$ae484031)
store volatile %struct.ScmObj* %argslist59859$ae484032, %struct.ScmObj** %stackaddr$prim61900, align 8
%clofunc61901 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48403)
musttail call tailcc void %clofunc61901(%struct.ScmObj* %ae48403, %struct.ScmObj* %argslist59859$ae484032)
ret void
}

define tailcc void @proc_clo$ae48403(%struct.ScmObj* %env$ae48403,%struct.ScmObj* %current_45args59832) {
%stackaddr$env-ref61902 = alloca %struct.ScmObj*, align 8
%lsts_4347112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48403, i64 0)
store %struct.ScmObj* %lsts_4347112, %struct.ScmObj** %stackaddr$env-ref61902
%stackaddr$env-ref61903 = alloca %struct.ScmObj*, align 8
%k47560 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48403, i64 1)
store %struct.ScmObj* %k47560, %struct.ScmObj** %stackaddr$env-ref61903
%stackaddr$env-ref61904 = alloca %struct.ScmObj*, align 8
%_37map147093 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48403, i64 2)
store %struct.ScmObj* %_37map147093, %struct.ScmObj** %stackaddr$env-ref61904
%stackaddr$env-ref61905 = alloca %struct.ScmObj*, align 8
%f47107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48403, i64 3)
store %struct.ScmObj* %f47107, %struct.ScmObj** %stackaddr$env-ref61905
%stackaddr$env-ref61906 = alloca %struct.ScmObj*, align 8
%acc47106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48403, i64 4)
store %struct.ScmObj* %acc47106, %struct.ScmObj** %stackaddr$env-ref61906
%stackaddr$env-ref61907 = alloca %struct.ScmObj*, align 8
%lsts47105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48403, i64 5)
store %struct.ScmObj* %lsts47105, %struct.ScmObj** %stackaddr$env-ref61907
%stackaddr$env-ref61908 = alloca %struct.ScmObj*, align 8
%_37foldr47103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48403, i64 6)
store %struct.ScmObj* %_37foldr47103, %struct.ScmObj** %stackaddr$env-ref61908
%stackaddr$env-ref61909 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48403, i64 7)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref61909
%stackaddr$prim61910 = alloca %struct.ScmObj*, align 8
%_95k47565 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59832)
store volatile %struct.ScmObj* %_95k47565, %struct.ScmObj** %stackaddr$prim61910, align 8
%stackaddr$prim61911 = alloca %struct.ScmObj*, align 8
%current_45args59833 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59832)
store volatile %struct.ScmObj* %current_45args59833, %struct.ScmObj** %stackaddr$prim61911, align 8
%stackaddr$prim61912 = alloca %struct.ScmObj*, align 8
%anf_45bind47264 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59833)
store volatile %struct.ScmObj* %anf_45bind47264, %struct.ScmObj** %stackaddr$prim61912, align 8
%stackaddr$makeclosure61913 = alloca %struct.ScmObj*, align 8
%fptrToInt61914 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48424 to i64
%ae48424 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt61914)
store volatile %struct.ScmObj* %ae48424, %struct.ScmObj** %stackaddr$makeclosure61913, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48424, %struct.ScmObj* %lsts_4347112, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48424, %struct.ScmObj* %k47560, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48424, %struct.ScmObj* %f47107, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48424, %struct.ScmObj* %acc47106, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48424, %struct.ScmObj* %_37foldr47103, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48424, %struct.ScmObj* %_37foldr147097, i64 5)
%argslist59854$_37map1470930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61915 = alloca %struct.ScmObj*, align 8
%argslist59854$_37map1470931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47105, %struct.ScmObj* %argslist59854$_37map1470930)
store volatile %struct.ScmObj* %argslist59854$_37map1470931, %struct.ScmObj** %stackaddr$prim61915, align 8
%stackaddr$prim61916 = alloca %struct.ScmObj*, align 8
%argslist59854$_37map1470932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47264, %struct.ScmObj* %argslist59854$_37map1470931)
store volatile %struct.ScmObj* %argslist59854$_37map1470932, %struct.ScmObj** %stackaddr$prim61916, align 8
%stackaddr$prim61917 = alloca %struct.ScmObj*, align 8
%argslist59854$_37map1470933 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48424, %struct.ScmObj* %argslist59854$_37map1470932)
store volatile %struct.ScmObj* %argslist59854$_37map1470933, %struct.ScmObj** %stackaddr$prim61917, align 8
%clofunc61918 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147093)
musttail call tailcc void %clofunc61918(%struct.ScmObj* %_37map147093, %struct.ScmObj* %argslist59854$_37map1470933)
ret void
}

define tailcc void @proc_clo$ae48424(%struct.ScmObj* %env$ae48424,%struct.ScmObj* %current_45args59835) {
%stackaddr$env-ref61919 = alloca %struct.ScmObj*, align 8
%lsts_4347112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48424, i64 0)
store %struct.ScmObj* %lsts_4347112, %struct.ScmObj** %stackaddr$env-ref61919
%stackaddr$env-ref61920 = alloca %struct.ScmObj*, align 8
%k47560 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48424, i64 1)
store %struct.ScmObj* %k47560, %struct.ScmObj** %stackaddr$env-ref61920
%stackaddr$env-ref61921 = alloca %struct.ScmObj*, align 8
%f47107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48424, i64 2)
store %struct.ScmObj* %f47107, %struct.ScmObj** %stackaddr$env-ref61921
%stackaddr$env-ref61922 = alloca %struct.ScmObj*, align 8
%acc47106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48424, i64 3)
store %struct.ScmObj* %acc47106, %struct.ScmObj** %stackaddr$env-ref61922
%stackaddr$env-ref61923 = alloca %struct.ScmObj*, align 8
%_37foldr47103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48424, i64 4)
store %struct.ScmObj* %_37foldr47103, %struct.ScmObj** %stackaddr$env-ref61923
%stackaddr$env-ref61924 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48424, i64 5)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref61924
%stackaddr$prim61925 = alloca %struct.ScmObj*, align 8
%_95k47566 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59835)
store volatile %struct.ScmObj* %_95k47566, %struct.ScmObj** %stackaddr$prim61925, align 8
%stackaddr$prim61926 = alloca %struct.ScmObj*, align 8
%current_45args59836 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59835)
store volatile %struct.ScmObj* %current_45args59836, %struct.ScmObj** %stackaddr$prim61926, align 8
%stackaddr$prim61927 = alloca %struct.ScmObj*, align 8
%vs47110 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59836)
store volatile %struct.ScmObj* %vs47110, %struct.ScmObj** %stackaddr$prim61927, align 8
%stackaddr$makeclosure61928 = alloca %struct.ScmObj*, align 8
%fptrToInt61929 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48427 to i64
%ae48427 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt61929)
store volatile %struct.ScmObj* %ae48427, %struct.ScmObj** %stackaddr$makeclosure61928, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48427, %struct.ScmObj* %lsts_4347112, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48427, %struct.ScmObj* %k47560, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48427, %struct.ScmObj* %vs47110, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48427, %struct.ScmObj* %f47107, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48427, %struct.ScmObj* %acc47106, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48427, %struct.ScmObj* %_37foldr47103, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48427, %struct.ScmObj* %_37foldr147097, i64 6)
%ae48428 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure61930 = alloca %struct.ScmObj*, align 8
%fptrToInt61931 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48429 to i64
%ae48429 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt61931)
store volatile %struct.ScmObj* %ae48429, %struct.ScmObj** %stackaddr$makeclosure61930, align 8
%argslist59853$ae484270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61932 = alloca %struct.ScmObj*, align 8
%argslist59853$ae484271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48429, %struct.ScmObj* %argslist59853$ae484270)
store volatile %struct.ScmObj* %argslist59853$ae484271, %struct.ScmObj** %stackaddr$prim61932, align 8
%stackaddr$prim61933 = alloca %struct.ScmObj*, align 8
%argslist59853$ae484272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48428, %struct.ScmObj* %argslist59853$ae484271)
store volatile %struct.ScmObj* %argslist59853$ae484272, %struct.ScmObj** %stackaddr$prim61933, align 8
%clofunc61934 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48427)
musttail call tailcc void %clofunc61934(%struct.ScmObj* %ae48427, %struct.ScmObj* %argslist59853$ae484272)
ret void
}

define tailcc void @proc_clo$ae48427(%struct.ScmObj* %env$ae48427,%struct.ScmObj* %current_45args59838) {
%stackaddr$env-ref61935 = alloca %struct.ScmObj*, align 8
%lsts_4347112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48427, i64 0)
store %struct.ScmObj* %lsts_4347112, %struct.ScmObj** %stackaddr$env-ref61935
%stackaddr$env-ref61936 = alloca %struct.ScmObj*, align 8
%k47560 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48427, i64 1)
store %struct.ScmObj* %k47560, %struct.ScmObj** %stackaddr$env-ref61936
%stackaddr$env-ref61937 = alloca %struct.ScmObj*, align 8
%vs47110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48427, i64 2)
store %struct.ScmObj* %vs47110, %struct.ScmObj** %stackaddr$env-ref61937
%stackaddr$env-ref61938 = alloca %struct.ScmObj*, align 8
%f47107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48427, i64 3)
store %struct.ScmObj* %f47107, %struct.ScmObj** %stackaddr$env-ref61938
%stackaddr$env-ref61939 = alloca %struct.ScmObj*, align 8
%acc47106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48427, i64 4)
store %struct.ScmObj* %acc47106, %struct.ScmObj** %stackaddr$env-ref61939
%stackaddr$env-ref61940 = alloca %struct.ScmObj*, align 8
%_37foldr47103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48427, i64 5)
store %struct.ScmObj* %_37foldr47103, %struct.ScmObj** %stackaddr$env-ref61940
%stackaddr$env-ref61941 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48427, i64 6)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref61941
%stackaddr$prim61942 = alloca %struct.ScmObj*, align 8
%_95k47567 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59838)
store volatile %struct.ScmObj* %_95k47567, %struct.ScmObj** %stackaddr$prim61942, align 8
%stackaddr$prim61943 = alloca %struct.ScmObj*, align 8
%current_45args59839 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59838)
store volatile %struct.ScmObj* %current_45args59839, %struct.ScmObj** %stackaddr$prim61943, align 8
%stackaddr$prim61944 = alloca %struct.ScmObj*, align 8
%anf_45bind47265 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59839)
store volatile %struct.ScmObj* %anf_45bind47265, %struct.ScmObj** %stackaddr$prim61944, align 8
%stackaddr$prim61945 = alloca %struct.ScmObj*, align 8
%anf_45bind47266 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47106, %struct.ScmObj* %lsts_4347112)
store volatile %struct.ScmObj* %anf_45bind47266, %struct.ScmObj** %stackaddr$prim61945, align 8
%stackaddr$prim61946 = alloca %struct.ScmObj*, align 8
%anf_45bind47267 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47107, %struct.ScmObj* %anf_45bind47266)
store volatile %struct.ScmObj* %anf_45bind47267, %struct.ScmObj** %stackaddr$prim61946, align 8
%stackaddr$makeclosure61947 = alloca %struct.ScmObj*, align 8
%fptrToInt61948 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48453 to i64
%ae48453 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt61948)
store volatile %struct.ScmObj* %ae48453, %struct.ScmObj** %stackaddr$makeclosure61947, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48453, %struct.ScmObj* %vs47110, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48453, %struct.ScmObj* %f47107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48453, %struct.ScmObj* %anf_45bind47265, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48453, %struct.ScmObj* %_37foldr147097, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48453, %struct.ScmObj* %k47560, i64 4)
%stackaddr$prim61949 = alloca %struct.ScmObj*, align 8
%cpsargs47571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48453, %struct.ScmObj* %anf_45bind47267)
store volatile %struct.ScmObj* %cpsargs47571, %struct.ScmObj** %stackaddr$prim61949, align 8
%clofunc61950 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr47103)
musttail call tailcc void %clofunc61950(%struct.ScmObj* %_37foldr47103, %struct.ScmObj* %cpsargs47571)
ret void
}

define tailcc void @proc_clo$ae48453(%struct.ScmObj* %env$ae48453,%struct.ScmObj* %current_45args59841) {
%stackaddr$env-ref61951 = alloca %struct.ScmObj*, align 8
%vs47110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48453, i64 0)
store %struct.ScmObj* %vs47110, %struct.ScmObj** %stackaddr$env-ref61951
%stackaddr$env-ref61952 = alloca %struct.ScmObj*, align 8
%f47107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48453, i64 1)
store %struct.ScmObj* %f47107, %struct.ScmObj** %stackaddr$env-ref61952
%stackaddr$env-ref61953 = alloca %struct.ScmObj*, align 8
%anf_45bind47265 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48453, i64 2)
store %struct.ScmObj* %anf_45bind47265, %struct.ScmObj** %stackaddr$env-ref61953
%stackaddr$env-ref61954 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48453, i64 3)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref61954
%stackaddr$env-ref61955 = alloca %struct.ScmObj*, align 8
%k47560 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48453, i64 4)
store %struct.ScmObj* %k47560, %struct.ScmObj** %stackaddr$env-ref61955
%stackaddr$prim61956 = alloca %struct.ScmObj*, align 8
%_95k47568 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59841)
store volatile %struct.ScmObj* %_95k47568, %struct.ScmObj** %stackaddr$prim61956, align 8
%stackaddr$prim61957 = alloca %struct.ScmObj*, align 8
%current_45args59842 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59841)
store volatile %struct.ScmObj* %current_45args59842, %struct.ScmObj** %stackaddr$prim61957, align 8
%stackaddr$prim61958 = alloca %struct.ScmObj*, align 8
%anf_45bind47268 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59842)
store volatile %struct.ScmObj* %anf_45bind47268, %struct.ScmObj** %stackaddr$prim61958, align 8
%ae48458 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61959 = alloca %struct.ScmObj*, align 8
%anf_45bind47269 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47268, %struct.ScmObj* %ae48458)
store volatile %struct.ScmObj* %anf_45bind47269, %struct.ScmObj** %stackaddr$prim61959, align 8
%stackaddr$makeclosure61960 = alloca %struct.ScmObj*, align 8
%fptrToInt61961 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48460 to i64
%ae48460 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt61961)
store volatile %struct.ScmObj* %ae48460, %struct.ScmObj** %stackaddr$makeclosure61960, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48460, %struct.ScmObj* %f47107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48460, %struct.ScmObj* %k47560, i64 1)
%argslist59847$_37foldr1470970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61962 = alloca %struct.ScmObj*, align 8
%argslist59847$_37foldr1470971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs47110, %struct.ScmObj* %argslist59847$_37foldr1470970)
store volatile %struct.ScmObj* %argslist59847$_37foldr1470971, %struct.ScmObj** %stackaddr$prim61962, align 8
%stackaddr$prim61963 = alloca %struct.ScmObj*, align 8
%argslist59847$_37foldr1470972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47269, %struct.ScmObj* %argslist59847$_37foldr1470971)
store volatile %struct.ScmObj* %argslist59847$_37foldr1470972, %struct.ScmObj** %stackaddr$prim61963, align 8
%stackaddr$prim61964 = alloca %struct.ScmObj*, align 8
%argslist59847$_37foldr1470973 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47265, %struct.ScmObj* %argslist59847$_37foldr1470972)
store volatile %struct.ScmObj* %argslist59847$_37foldr1470973, %struct.ScmObj** %stackaddr$prim61964, align 8
%stackaddr$prim61965 = alloca %struct.ScmObj*, align 8
%argslist59847$_37foldr1470974 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48460, %struct.ScmObj* %argslist59847$_37foldr1470973)
store volatile %struct.ScmObj* %argslist59847$_37foldr1470974, %struct.ScmObj** %stackaddr$prim61965, align 8
%clofunc61966 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147097)
musttail call tailcc void %clofunc61966(%struct.ScmObj* %_37foldr147097, %struct.ScmObj* %argslist59847$_37foldr1470974)
ret void
}

define tailcc void @proc_clo$ae48460(%struct.ScmObj* %env$ae48460,%struct.ScmObj* %current_45args59844) {
%stackaddr$env-ref61967 = alloca %struct.ScmObj*, align 8
%f47107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48460, i64 0)
store %struct.ScmObj* %f47107, %struct.ScmObj** %stackaddr$env-ref61967
%stackaddr$env-ref61968 = alloca %struct.ScmObj*, align 8
%k47560 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48460, i64 1)
store %struct.ScmObj* %k47560, %struct.ScmObj** %stackaddr$env-ref61968
%stackaddr$prim61969 = alloca %struct.ScmObj*, align 8
%_95k47569 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59844)
store volatile %struct.ScmObj* %_95k47569, %struct.ScmObj** %stackaddr$prim61969, align 8
%stackaddr$prim61970 = alloca %struct.ScmObj*, align 8
%current_45args59845 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59844)
store volatile %struct.ScmObj* %current_45args59845, %struct.ScmObj** %stackaddr$prim61970, align 8
%stackaddr$prim61971 = alloca %struct.ScmObj*, align 8
%anf_45bind47270 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59845)
store volatile %struct.ScmObj* %anf_45bind47270, %struct.ScmObj** %stackaddr$prim61971, align 8
%stackaddr$prim61972 = alloca %struct.ScmObj*, align 8
%cpsargs47570 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47560, %struct.ScmObj* %anf_45bind47270)
store volatile %struct.ScmObj* %cpsargs47570, %struct.ScmObj** %stackaddr$prim61972, align 8
%clofunc61973 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47107)
musttail call tailcc void %clofunc61973(%struct.ScmObj* %f47107, %struct.ScmObj* %cpsargs47570)
ret void
}

define tailcc void @proc_clo$ae48429(%struct.ScmObj* %env$ae48429,%struct.ScmObj* %current_45args59848) {
%stackaddr$prim61974 = alloca %struct.ScmObj*, align 8
%k47572 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59848)
store volatile %struct.ScmObj* %k47572, %struct.ScmObj** %stackaddr$prim61974, align 8
%stackaddr$prim61975 = alloca %struct.ScmObj*, align 8
%current_45args59849 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59848)
store volatile %struct.ScmObj* %current_45args59849, %struct.ScmObj** %stackaddr$prim61975, align 8
%stackaddr$prim61976 = alloca %struct.ScmObj*, align 8
%a47115 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59849)
store volatile %struct.ScmObj* %a47115, %struct.ScmObj** %stackaddr$prim61976, align 8
%stackaddr$prim61977 = alloca %struct.ScmObj*, align 8
%current_45args59850 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59849)
store volatile %struct.ScmObj* %current_45args59850, %struct.ScmObj** %stackaddr$prim61977, align 8
%stackaddr$prim61978 = alloca %struct.ScmObj*, align 8
%b47114 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59850)
store volatile %struct.ScmObj* %b47114, %struct.ScmObj** %stackaddr$prim61978, align 8
%stackaddr$prim61979 = alloca %struct.ScmObj*, align 8
%cpsprim47573 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a47115, %struct.ScmObj* %b47114)
store volatile %struct.ScmObj* %cpsprim47573, %struct.ScmObj** %stackaddr$prim61979, align 8
%ae48433 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist59852$k475720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61980 = alloca %struct.ScmObj*, align 8
%argslist59852$k475721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47573, %struct.ScmObj* %argslist59852$k475720)
store volatile %struct.ScmObj* %argslist59852$k475721, %struct.ScmObj** %stackaddr$prim61980, align 8
%stackaddr$prim61981 = alloca %struct.ScmObj*, align 8
%argslist59852$k475722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48433, %struct.ScmObj* %argslist59852$k475721)
store volatile %struct.ScmObj* %argslist59852$k475722, %struct.ScmObj** %stackaddr$prim61981, align 8
%clofunc61982 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47572)
musttail call tailcc void %clofunc61982(%struct.ScmObj* %k47572, %struct.ScmObj* %argslist59852$k475722)
ret void
}

define tailcc void @proc_clo$ae48405(%struct.ScmObj* %env$ae48405,%struct.ScmObj* %current_45args59855) {
%stackaddr$prim61983 = alloca %struct.ScmObj*, align 8
%k47574 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59855)
store volatile %struct.ScmObj* %k47574, %struct.ScmObj** %stackaddr$prim61983, align 8
%stackaddr$prim61984 = alloca %struct.ScmObj*, align 8
%current_45args59856 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59855)
store volatile %struct.ScmObj* %current_45args59856, %struct.ScmObj** %stackaddr$prim61984, align 8
%stackaddr$prim61985 = alloca %struct.ScmObj*, align 8
%x47111 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59856)
store volatile %struct.ScmObj* %x47111, %struct.ScmObj** %stackaddr$prim61985, align 8
%stackaddr$prim61986 = alloca %struct.ScmObj*, align 8
%cpsprim47575 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47111)
store volatile %struct.ScmObj* %cpsprim47575, %struct.ScmObj** %stackaddr$prim61986, align 8
%ae48408 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist59858$k475740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61987 = alloca %struct.ScmObj*, align 8
%argslist59858$k475741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47575, %struct.ScmObj* %argslist59858$k475740)
store volatile %struct.ScmObj* %argslist59858$k475741, %struct.ScmObj** %stackaddr$prim61987, align 8
%stackaddr$prim61988 = alloca %struct.ScmObj*, align 8
%argslist59858$k475742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48408, %struct.ScmObj* %argslist59858$k475741)
store volatile %struct.ScmObj* %argslist59858$k475742, %struct.ScmObj** %stackaddr$prim61988, align 8
%clofunc61989 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47574)
musttail call tailcc void %clofunc61989(%struct.ScmObj* %k47574, %struct.ScmObj* %argslist59858$k475742)
ret void
}

define tailcc void @proc_clo$ae48381(%struct.ScmObj* %env$ae48381,%struct.ScmObj* %current_45args59861) {
%stackaddr$prim61990 = alloca %struct.ScmObj*, align 8
%k47576 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59861)
store volatile %struct.ScmObj* %k47576, %struct.ScmObj** %stackaddr$prim61990, align 8
%stackaddr$prim61991 = alloca %struct.ScmObj*, align 8
%current_45args59862 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59861)
store volatile %struct.ScmObj* %current_45args59862, %struct.ScmObj** %stackaddr$prim61991, align 8
%stackaddr$prim61992 = alloca %struct.ScmObj*, align 8
%x47113 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59862)
store volatile %struct.ScmObj* %x47113, %struct.ScmObj** %stackaddr$prim61992, align 8
%stackaddr$prim61993 = alloca %struct.ScmObj*, align 8
%cpsprim47577 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47113)
store volatile %struct.ScmObj* %cpsprim47577, %struct.ScmObj** %stackaddr$prim61993, align 8
%ae48384 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist59864$k475760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim61994 = alloca %struct.ScmObj*, align 8
%argslist59864$k475761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47577, %struct.ScmObj* %argslist59864$k475760)
store volatile %struct.ScmObj* %argslist59864$k475761, %struct.ScmObj** %stackaddr$prim61994, align 8
%stackaddr$prim61995 = alloca %struct.ScmObj*, align 8
%argslist59864$k475762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48384, %struct.ScmObj* %argslist59864$k475761)
store volatile %struct.ScmObj* %argslist59864$k475762, %struct.ScmObj** %stackaddr$prim61995, align 8
%clofunc61996 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47576)
musttail call tailcc void %clofunc61996(%struct.ScmObj* %k47576, %struct.ScmObj* %argslist59864$k475762)
ret void
}

define tailcc void @proc_clo$ae48333(%struct.ScmObj* %env$ae48333,%struct.ScmObj* %current_45args59867) {
%stackaddr$prim61997 = alloca %struct.ScmObj*, align 8
%k47578 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59867)
store volatile %struct.ScmObj* %k47578, %struct.ScmObj** %stackaddr$prim61997, align 8
%stackaddr$prim61998 = alloca %struct.ScmObj*, align 8
%current_45args59868 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59867)
store volatile %struct.ScmObj* %current_45args59868, %struct.ScmObj** %stackaddr$prim61998, align 8
%stackaddr$prim61999 = alloca %struct.ScmObj*, align 8
%lst47109 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59868)
store volatile %struct.ScmObj* %lst47109, %struct.ScmObj** %stackaddr$prim61999, align 8
%stackaddr$prim62000 = alloca %struct.ScmObj*, align 8
%current_45args59869 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59868)
store volatile %struct.ScmObj* %current_45args59869, %struct.ScmObj** %stackaddr$prim62000, align 8
%stackaddr$prim62001 = alloca %struct.ScmObj*, align 8
%b47108 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59869)
store volatile %struct.ScmObj* %b47108, %struct.ScmObj** %stackaddr$prim62001, align 8
%truthy$cmp62002 = call i64 @is_truthy_value(%struct.ScmObj* %b47108)
%cmp$cmp62002 = icmp eq i64 %truthy$cmp62002, 1
br i1 %cmp$cmp62002, label %truebranch$cmp62002, label %falsebranch$cmp62002
truebranch$cmp62002:
%ae48336 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist59871$k475780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62003 = alloca %struct.ScmObj*, align 8
%argslist59871$k475781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b47108, %struct.ScmObj* %argslist59871$k475780)
store volatile %struct.ScmObj* %argslist59871$k475781, %struct.ScmObj** %stackaddr$prim62003, align 8
%stackaddr$prim62004 = alloca %struct.ScmObj*, align 8
%argslist59871$k475782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48336, %struct.ScmObj* %argslist59871$k475781)
store volatile %struct.ScmObj* %argslist59871$k475782, %struct.ScmObj** %stackaddr$prim62004, align 8
%clofunc62005 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47578)
musttail call tailcc void %clofunc62005(%struct.ScmObj* %k47578, %struct.ScmObj* %argslist59871$k475782)
ret void
falsebranch$cmp62002:
%stackaddr$prim62006 = alloca %struct.ScmObj*, align 8
%cpsprim47579 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47109)
store volatile %struct.ScmObj* %cpsprim47579, %struct.ScmObj** %stackaddr$prim62006, align 8
%ae48343 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist59872$k475780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62007 = alloca %struct.ScmObj*, align 8
%argslist59872$k475781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47579, %struct.ScmObj* %argslist59872$k475780)
store volatile %struct.ScmObj* %argslist59872$k475781, %struct.ScmObj** %stackaddr$prim62007, align 8
%stackaddr$prim62008 = alloca %struct.ScmObj*, align 8
%argslist59872$k475782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48343, %struct.ScmObj* %argslist59872$k475781)
store volatile %struct.ScmObj* %argslist59872$k475782, %struct.ScmObj** %stackaddr$prim62008, align 8
%clofunc62009 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47578)
musttail call tailcc void %clofunc62009(%struct.ScmObj* %k47578, %struct.ScmObj* %argslist59872$k475782)
ret void
}

define tailcc void @proc_clo$ae48290(%struct.ScmObj* %env$ae48290,%struct.ScmObj* %current_45args59876) {
%stackaddr$env-ref62010 = alloca %struct.ScmObj*, align 8
%_37take47089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48290, i64 0)
store %struct.ScmObj* %_37take47089, %struct.ScmObj** %stackaddr$env-ref62010
%stackaddr$env-ref62011 = alloca %struct.ScmObj*, align 8
%_37length47086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48290, i64 1)
store %struct.ScmObj* %_37length47086, %struct.ScmObj** %stackaddr$env-ref62011
%stackaddr$prim62012 = alloca %struct.ScmObj*, align 8
%k47580 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59876)
store volatile %struct.ScmObj* %k47580, %struct.ScmObj** %stackaddr$prim62012, align 8
%stackaddr$prim62013 = alloca %struct.ScmObj*, align 8
%current_45args59877 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59876)
store volatile %struct.ScmObj* %current_45args59877, %struct.ScmObj** %stackaddr$prim62013, align 8
%stackaddr$prim62014 = alloca %struct.ScmObj*, align 8
%lst47118 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59877)
store volatile %struct.ScmObj* %lst47118, %struct.ScmObj** %stackaddr$prim62014, align 8
%stackaddr$prim62015 = alloca %struct.ScmObj*, align 8
%current_45args59878 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59877)
store volatile %struct.ScmObj* %current_45args59878, %struct.ScmObj** %stackaddr$prim62015, align 8
%stackaddr$prim62016 = alloca %struct.ScmObj*, align 8
%n47117 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59878)
store volatile %struct.ScmObj* %n47117, %struct.ScmObj** %stackaddr$prim62016, align 8
%stackaddr$makeclosure62017 = alloca %struct.ScmObj*, align 8
%fptrToInt62018 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48292 to i64
%ae48292 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt62018)
store volatile %struct.ScmObj* %ae48292, %struct.ScmObj** %stackaddr$makeclosure62017, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48292, %struct.ScmObj* %_37take47089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48292, %struct.ScmObj* %lst47118, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48292, %struct.ScmObj* %n47117, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48292, %struct.ScmObj* %k47580, i64 3)
%argslist59884$_37length470860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62019 = alloca %struct.ScmObj*, align 8
%argslist59884$_37length470861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47118, %struct.ScmObj* %argslist59884$_37length470860)
store volatile %struct.ScmObj* %argslist59884$_37length470861, %struct.ScmObj** %stackaddr$prim62019, align 8
%stackaddr$prim62020 = alloca %struct.ScmObj*, align 8
%argslist59884$_37length470862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48292, %struct.ScmObj* %argslist59884$_37length470861)
store volatile %struct.ScmObj* %argslist59884$_37length470862, %struct.ScmObj** %stackaddr$prim62020, align 8
%clofunc62021 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length47086)
musttail call tailcc void %clofunc62021(%struct.ScmObj* %_37length47086, %struct.ScmObj* %argslist59884$_37length470862)
ret void
}

define tailcc void @proc_clo$ae48292(%struct.ScmObj* %env$ae48292,%struct.ScmObj* %current_45args59880) {
%stackaddr$env-ref62022 = alloca %struct.ScmObj*, align 8
%_37take47089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48292, i64 0)
store %struct.ScmObj* %_37take47089, %struct.ScmObj** %stackaddr$env-ref62022
%stackaddr$env-ref62023 = alloca %struct.ScmObj*, align 8
%lst47118 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48292, i64 1)
store %struct.ScmObj* %lst47118, %struct.ScmObj** %stackaddr$env-ref62023
%stackaddr$env-ref62024 = alloca %struct.ScmObj*, align 8
%n47117 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48292, i64 2)
store %struct.ScmObj* %n47117, %struct.ScmObj** %stackaddr$env-ref62024
%stackaddr$env-ref62025 = alloca %struct.ScmObj*, align 8
%k47580 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48292, i64 3)
store %struct.ScmObj* %k47580, %struct.ScmObj** %stackaddr$env-ref62025
%stackaddr$prim62026 = alloca %struct.ScmObj*, align 8
%_95k47581 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59880)
store volatile %struct.ScmObj* %_95k47581, %struct.ScmObj** %stackaddr$prim62026, align 8
%stackaddr$prim62027 = alloca %struct.ScmObj*, align 8
%current_45args59881 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59880)
store volatile %struct.ScmObj* %current_45args59881, %struct.ScmObj** %stackaddr$prim62027, align 8
%stackaddr$prim62028 = alloca %struct.ScmObj*, align 8
%anf_45bind47257 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59881)
store volatile %struct.ScmObj* %anf_45bind47257, %struct.ScmObj** %stackaddr$prim62028, align 8
%stackaddr$prim62029 = alloca %struct.ScmObj*, align 8
%anf_45bind47258 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind47257, %struct.ScmObj* %n47117)
store volatile %struct.ScmObj* %anf_45bind47258, %struct.ScmObj** %stackaddr$prim62029, align 8
%argslist59883$_37take470890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62030 = alloca %struct.ScmObj*, align 8
%argslist59883$_37take470891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47258, %struct.ScmObj* %argslist59883$_37take470890)
store volatile %struct.ScmObj* %argslist59883$_37take470891, %struct.ScmObj** %stackaddr$prim62030, align 8
%stackaddr$prim62031 = alloca %struct.ScmObj*, align 8
%argslist59883$_37take470892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47118, %struct.ScmObj* %argslist59883$_37take470891)
store volatile %struct.ScmObj* %argslist59883$_37take470892, %struct.ScmObj** %stackaddr$prim62031, align 8
%stackaddr$prim62032 = alloca %struct.ScmObj*, align 8
%argslist59883$_37take470893 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47580, %struct.ScmObj* %argslist59883$_37take470892)
store volatile %struct.ScmObj* %argslist59883$_37take470893, %struct.ScmObj** %stackaddr$prim62032, align 8
%clofunc62033 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take47089)
musttail call tailcc void %clofunc62033(%struct.ScmObj* %_37take47089, %struct.ScmObj* %argslist59883$_37take470893)
ret void
}

define tailcc void @proc_clo$ae48236(%struct.ScmObj* %env$ae48236,%struct.ScmObj* %current_45args59886) {
%stackaddr$env-ref62034 = alloca %struct.ScmObj*, align 8
%_37foldl147081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48236, i64 0)
store %struct.ScmObj* %_37foldl147081, %struct.ScmObj** %stackaddr$env-ref62034
%stackaddr$prim62035 = alloca %struct.ScmObj*, align 8
%k47582 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59886)
store volatile %struct.ScmObj* %k47582, %struct.ScmObj** %stackaddr$prim62035, align 8
%stackaddr$prim62036 = alloca %struct.ScmObj*, align 8
%current_45args59887 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59886)
store volatile %struct.ScmObj* %current_45args59887, %struct.ScmObj** %stackaddr$prim62036, align 8
%stackaddr$prim62037 = alloca %struct.ScmObj*, align 8
%lst47120 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59887)
store volatile %struct.ScmObj* %lst47120, %struct.ScmObj** %stackaddr$prim62037, align 8
%stackaddr$makeclosure62038 = alloca %struct.ScmObj*, align 8
%fptrToInt62039 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48237 to i64
%ae48237 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt62039)
store volatile %struct.ScmObj* %ae48237, %struct.ScmObj** %stackaddr$makeclosure62038, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48237, %struct.ScmObj* %lst47120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48237, %struct.ScmObj* %k47582, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48237, %struct.ScmObj* %_37foldl147081, i64 2)
%ae48238 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure62040 = alloca %struct.ScmObj*, align 8
%fptrToInt62041 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48239 to i64
%ae48239 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt62041)
store volatile %struct.ScmObj* %ae48239, %struct.ScmObj** %stackaddr$makeclosure62040, align 8
%argslist59898$ae482370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62042 = alloca %struct.ScmObj*, align 8
%argslist59898$ae482371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48239, %struct.ScmObj* %argslist59898$ae482370)
store volatile %struct.ScmObj* %argslist59898$ae482371, %struct.ScmObj** %stackaddr$prim62042, align 8
%stackaddr$prim62043 = alloca %struct.ScmObj*, align 8
%argslist59898$ae482372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48238, %struct.ScmObj* %argslist59898$ae482371)
store volatile %struct.ScmObj* %argslist59898$ae482372, %struct.ScmObj** %stackaddr$prim62043, align 8
%clofunc62044 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48237)
musttail call tailcc void %clofunc62044(%struct.ScmObj* %ae48237, %struct.ScmObj* %argslist59898$ae482372)
ret void
}

define tailcc void @proc_clo$ae48237(%struct.ScmObj* %env$ae48237,%struct.ScmObj* %current_45args59889) {
%stackaddr$env-ref62045 = alloca %struct.ScmObj*, align 8
%lst47120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48237, i64 0)
store %struct.ScmObj* %lst47120, %struct.ScmObj** %stackaddr$env-ref62045
%stackaddr$env-ref62046 = alloca %struct.ScmObj*, align 8
%k47582 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48237, i64 1)
store %struct.ScmObj* %k47582, %struct.ScmObj** %stackaddr$env-ref62046
%stackaddr$env-ref62047 = alloca %struct.ScmObj*, align 8
%_37foldl147081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48237, i64 2)
store %struct.ScmObj* %_37foldl147081, %struct.ScmObj** %stackaddr$env-ref62047
%stackaddr$prim62048 = alloca %struct.ScmObj*, align 8
%_95k47583 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59889)
store volatile %struct.ScmObj* %_95k47583, %struct.ScmObj** %stackaddr$prim62048, align 8
%stackaddr$prim62049 = alloca %struct.ScmObj*, align 8
%current_45args59890 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59889)
store volatile %struct.ScmObj* %current_45args59890, %struct.ScmObj** %stackaddr$prim62049, align 8
%stackaddr$prim62050 = alloca %struct.ScmObj*, align 8
%anf_45bind47256 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59890)
store volatile %struct.ScmObj* %anf_45bind47256, %struct.ScmObj** %stackaddr$prim62050, align 8
%ae48258 = call %struct.ScmObj* @const_init_null()
%argslist59892$_37foldl1470810 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62051 = alloca %struct.ScmObj*, align 8
%argslist59892$_37foldl1470811 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47120, %struct.ScmObj* %argslist59892$_37foldl1470810)
store volatile %struct.ScmObj* %argslist59892$_37foldl1470811, %struct.ScmObj** %stackaddr$prim62051, align 8
%stackaddr$prim62052 = alloca %struct.ScmObj*, align 8
%argslist59892$_37foldl1470812 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48258, %struct.ScmObj* %argslist59892$_37foldl1470811)
store volatile %struct.ScmObj* %argslist59892$_37foldl1470812, %struct.ScmObj** %stackaddr$prim62052, align 8
%stackaddr$prim62053 = alloca %struct.ScmObj*, align 8
%argslist59892$_37foldl1470813 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47256, %struct.ScmObj* %argslist59892$_37foldl1470812)
store volatile %struct.ScmObj* %argslist59892$_37foldl1470813, %struct.ScmObj** %stackaddr$prim62053, align 8
%stackaddr$prim62054 = alloca %struct.ScmObj*, align 8
%argslist59892$_37foldl1470814 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47582, %struct.ScmObj* %argslist59892$_37foldl1470813)
store volatile %struct.ScmObj* %argslist59892$_37foldl1470814, %struct.ScmObj** %stackaddr$prim62054, align 8
%clofunc62055 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147081)
musttail call tailcc void %clofunc62055(%struct.ScmObj* %_37foldl147081, %struct.ScmObj* %argslist59892$_37foldl1470814)
ret void
}

define tailcc void @proc_clo$ae48239(%struct.ScmObj* %env$ae48239,%struct.ScmObj* %current_45args59893) {
%stackaddr$prim62056 = alloca %struct.ScmObj*, align 8
%k47584 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59893)
store volatile %struct.ScmObj* %k47584, %struct.ScmObj** %stackaddr$prim62056, align 8
%stackaddr$prim62057 = alloca %struct.ScmObj*, align 8
%current_45args59894 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59893)
store volatile %struct.ScmObj* %current_45args59894, %struct.ScmObj** %stackaddr$prim62057, align 8
%stackaddr$prim62058 = alloca %struct.ScmObj*, align 8
%x47122 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59894)
store volatile %struct.ScmObj* %x47122, %struct.ScmObj** %stackaddr$prim62058, align 8
%stackaddr$prim62059 = alloca %struct.ScmObj*, align 8
%current_45args59895 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59894)
store volatile %struct.ScmObj* %current_45args59895, %struct.ScmObj** %stackaddr$prim62059, align 8
%stackaddr$prim62060 = alloca %struct.ScmObj*, align 8
%y47121 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59895)
store volatile %struct.ScmObj* %y47121, %struct.ScmObj** %stackaddr$prim62060, align 8
%ae48241 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist59897$k475840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62061 = alloca %struct.ScmObj*, align 8
%argslist59897$k475841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x47122, %struct.ScmObj* %argslist59897$k475840)
store volatile %struct.ScmObj* %argslist59897$k475841, %struct.ScmObj** %stackaddr$prim62061, align 8
%stackaddr$prim62062 = alloca %struct.ScmObj*, align 8
%argslist59897$k475842 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48241, %struct.ScmObj* %argslist59897$k475841)
store volatile %struct.ScmObj* %argslist59897$k475842, %struct.ScmObj** %stackaddr$prim62062, align 8
%clofunc62063 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47584)
musttail call tailcc void %clofunc62063(%struct.ScmObj* %k47584, %struct.ScmObj* %argslist59897$k475842)
ret void
}

define tailcc void @proc_clo$ae48157(%struct.ScmObj* %env$ae48157,%struct.ScmObj* %current_45args59901) {
%stackaddr$prim62064 = alloca %struct.ScmObj*, align 8
%k47585 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59901)
store volatile %struct.ScmObj* %k47585, %struct.ScmObj** %stackaddr$prim62064, align 8
%stackaddr$prim62065 = alloca %struct.ScmObj*, align 8
%current_45args59902 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59901)
store volatile %struct.ScmObj* %current_45args59902, %struct.ScmObj** %stackaddr$prim62065, align 8
%stackaddr$prim62066 = alloca %struct.ScmObj*, align 8
%_37foldl147082 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59902)
store volatile %struct.ScmObj* %_37foldl147082, %struct.ScmObj** %stackaddr$prim62066, align 8
%ae48159 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure62067 = alloca %struct.ScmObj*, align 8
%fptrToInt62068 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48160 to i64
%ae48160 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt62068)
store volatile %struct.ScmObj* %ae48160, %struct.ScmObj** %stackaddr$makeclosure62067, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48160, %struct.ScmObj* %_37foldl147082, i64 0)
%argslist59915$k475850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62069 = alloca %struct.ScmObj*, align 8
%argslist59915$k475851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48160, %struct.ScmObj* %argslist59915$k475850)
store volatile %struct.ScmObj* %argslist59915$k475851, %struct.ScmObj** %stackaddr$prim62069, align 8
%stackaddr$prim62070 = alloca %struct.ScmObj*, align 8
%argslist59915$k475852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48159, %struct.ScmObj* %argslist59915$k475851)
store volatile %struct.ScmObj* %argslist59915$k475852, %struct.ScmObj** %stackaddr$prim62070, align 8
%clofunc62071 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47585)
musttail call tailcc void %clofunc62071(%struct.ScmObj* %k47585, %struct.ScmObj* %argslist59915$k475852)
ret void
}

define tailcc void @proc_clo$ae48160(%struct.ScmObj* %env$ae48160,%struct.ScmObj* %current_45args59904) {
%stackaddr$env-ref62072 = alloca %struct.ScmObj*, align 8
%_37foldl147082 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48160, i64 0)
store %struct.ScmObj* %_37foldl147082, %struct.ScmObj** %stackaddr$env-ref62072
%stackaddr$prim62073 = alloca %struct.ScmObj*, align 8
%k47586 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59904)
store volatile %struct.ScmObj* %k47586, %struct.ScmObj** %stackaddr$prim62073, align 8
%stackaddr$prim62074 = alloca %struct.ScmObj*, align 8
%current_45args59905 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59904)
store volatile %struct.ScmObj* %current_45args59905, %struct.ScmObj** %stackaddr$prim62074, align 8
%stackaddr$prim62075 = alloca %struct.ScmObj*, align 8
%f47085 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59905)
store volatile %struct.ScmObj* %f47085, %struct.ScmObj** %stackaddr$prim62075, align 8
%stackaddr$prim62076 = alloca %struct.ScmObj*, align 8
%current_45args59906 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59905)
store volatile %struct.ScmObj* %current_45args59906, %struct.ScmObj** %stackaddr$prim62076, align 8
%stackaddr$prim62077 = alloca %struct.ScmObj*, align 8
%acc47084 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59906)
store volatile %struct.ScmObj* %acc47084, %struct.ScmObj** %stackaddr$prim62077, align 8
%stackaddr$prim62078 = alloca %struct.ScmObj*, align 8
%current_45args59907 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59906)
store volatile %struct.ScmObj* %current_45args59907, %struct.ScmObj** %stackaddr$prim62078, align 8
%stackaddr$prim62079 = alloca %struct.ScmObj*, align 8
%lst47083 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59907)
store volatile %struct.ScmObj* %lst47083, %struct.ScmObj** %stackaddr$prim62079, align 8
%stackaddr$prim62080 = alloca %struct.ScmObj*, align 8
%anf_45bind47251 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47083)
store volatile %struct.ScmObj* %anf_45bind47251, %struct.ScmObj** %stackaddr$prim62080, align 8
%truthy$cmp62081 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47251)
%cmp$cmp62081 = icmp eq i64 %truthy$cmp62081, 1
br i1 %cmp$cmp62081, label %truebranch$cmp62081, label %falsebranch$cmp62081
truebranch$cmp62081:
%ae48164 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist59909$k475860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62082 = alloca %struct.ScmObj*, align 8
%argslist59909$k475861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47084, %struct.ScmObj* %argslist59909$k475860)
store volatile %struct.ScmObj* %argslist59909$k475861, %struct.ScmObj** %stackaddr$prim62082, align 8
%stackaddr$prim62083 = alloca %struct.ScmObj*, align 8
%argslist59909$k475862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48164, %struct.ScmObj* %argslist59909$k475861)
store volatile %struct.ScmObj* %argslist59909$k475862, %struct.ScmObj** %stackaddr$prim62083, align 8
%clofunc62084 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47586)
musttail call tailcc void %clofunc62084(%struct.ScmObj* %k47586, %struct.ScmObj* %argslist59909$k475862)
ret void
falsebranch$cmp62081:
%stackaddr$prim62085 = alloca %struct.ScmObj*, align 8
%anf_45bind47252 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47083)
store volatile %struct.ScmObj* %anf_45bind47252, %struct.ScmObj** %stackaddr$prim62085, align 8
%stackaddr$makeclosure62086 = alloca %struct.ScmObj*, align 8
%fptrToInt62087 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48171 to i64
%ae48171 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt62087)
store volatile %struct.ScmObj* %ae48171, %struct.ScmObj** %stackaddr$makeclosure62086, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48171, %struct.ScmObj* %k47586, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48171, %struct.ScmObj* %f47085, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48171, %struct.ScmObj* %lst47083, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48171, %struct.ScmObj* %_37foldl147082, i64 3)
%argslist59914$f470850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62088 = alloca %struct.ScmObj*, align 8
%argslist59914$f470851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47084, %struct.ScmObj* %argslist59914$f470850)
store volatile %struct.ScmObj* %argslist59914$f470851, %struct.ScmObj** %stackaddr$prim62088, align 8
%stackaddr$prim62089 = alloca %struct.ScmObj*, align 8
%argslist59914$f470852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47252, %struct.ScmObj* %argslist59914$f470851)
store volatile %struct.ScmObj* %argslist59914$f470852, %struct.ScmObj** %stackaddr$prim62089, align 8
%stackaddr$prim62090 = alloca %struct.ScmObj*, align 8
%argslist59914$f470853 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48171, %struct.ScmObj* %argslist59914$f470852)
store volatile %struct.ScmObj* %argslist59914$f470853, %struct.ScmObj** %stackaddr$prim62090, align 8
%clofunc62091 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47085)
musttail call tailcc void %clofunc62091(%struct.ScmObj* %f47085, %struct.ScmObj* %argslist59914$f470853)
ret void
}

define tailcc void @proc_clo$ae48171(%struct.ScmObj* %env$ae48171,%struct.ScmObj* %current_45args59910) {
%stackaddr$env-ref62092 = alloca %struct.ScmObj*, align 8
%k47586 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48171, i64 0)
store %struct.ScmObj* %k47586, %struct.ScmObj** %stackaddr$env-ref62092
%stackaddr$env-ref62093 = alloca %struct.ScmObj*, align 8
%f47085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48171, i64 1)
store %struct.ScmObj* %f47085, %struct.ScmObj** %stackaddr$env-ref62093
%stackaddr$env-ref62094 = alloca %struct.ScmObj*, align 8
%lst47083 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48171, i64 2)
store %struct.ScmObj* %lst47083, %struct.ScmObj** %stackaddr$env-ref62094
%stackaddr$env-ref62095 = alloca %struct.ScmObj*, align 8
%_37foldl147082 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48171, i64 3)
store %struct.ScmObj* %_37foldl147082, %struct.ScmObj** %stackaddr$env-ref62095
%stackaddr$prim62096 = alloca %struct.ScmObj*, align 8
%_95k47587 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59910)
store volatile %struct.ScmObj* %_95k47587, %struct.ScmObj** %stackaddr$prim62096, align 8
%stackaddr$prim62097 = alloca %struct.ScmObj*, align 8
%current_45args59911 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59910)
store volatile %struct.ScmObj* %current_45args59911, %struct.ScmObj** %stackaddr$prim62097, align 8
%stackaddr$prim62098 = alloca %struct.ScmObj*, align 8
%anf_45bind47253 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59911)
store volatile %struct.ScmObj* %anf_45bind47253, %struct.ScmObj** %stackaddr$prim62098, align 8
%stackaddr$prim62099 = alloca %struct.ScmObj*, align 8
%anf_45bind47254 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47083)
store volatile %struct.ScmObj* %anf_45bind47254, %struct.ScmObj** %stackaddr$prim62099, align 8
%argslist59913$_37foldl1470820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62100 = alloca %struct.ScmObj*, align 8
%argslist59913$_37foldl1470821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47254, %struct.ScmObj* %argslist59913$_37foldl1470820)
store volatile %struct.ScmObj* %argslist59913$_37foldl1470821, %struct.ScmObj** %stackaddr$prim62100, align 8
%stackaddr$prim62101 = alloca %struct.ScmObj*, align 8
%argslist59913$_37foldl1470822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47253, %struct.ScmObj* %argslist59913$_37foldl1470821)
store volatile %struct.ScmObj* %argslist59913$_37foldl1470822, %struct.ScmObj** %stackaddr$prim62101, align 8
%stackaddr$prim62102 = alloca %struct.ScmObj*, align 8
%argslist59913$_37foldl1470823 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47085, %struct.ScmObj* %argslist59913$_37foldl1470822)
store volatile %struct.ScmObj* %argslist59913$_37foldl1470823, %struct.ScmObj** %stackaddr$prim62102, align 8
%stackaddr$prim62103 = alloca %struct.ScmObj*, align 8
%argslist59913$_37foldl1470824 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47586, %struct.ScmObj* %argslist59913$_37foldl1470823)
store volatile %struct.ScmObj* %argslist59913$_37foldl1470824, %struct.ScmObj** %stackaddr$prim62103, align 8
%clofunc62104 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147082)
musttail call tailcc void %clofunc62104(%struct.ScmObj* %_37foldl147082, %struct.ScmObj* %argslist59913$_37foldl1470824)
ret void
}

define tailcc void @proc_clo$ae48074(%struct.ScmObj* %env$ae48074,%struct.ScmObj* %current_45args59918) {
%stackaddr$prim62105 = alloca %struct.ScmObj*, align 8
%k47588 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59918)
store volatile %struct.ScmObj* %k47588, %struct.ScmObj** %stackaddr$prim62105, align 8
%stackaddr$prim62106 = alloca %struct.ScmObj*, align 8
%current_45args59919 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59918)
store volatile %struct.ScmObj* %current_45args59919, %struct.ScmObj** %stackaddr$prim62106, align 8
%stackaddr$prim62107 = alloca %struct.ScmObj*, align 8
%_37length47087 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59919)
store volatile %struct.ScmObj* %_37length47087, %struct.ScmObj** %stackaddr$prim62107, align 8
%ae48076 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure62108 = alloca %struct.ScmObj*, align 8
%fptrToInt62109 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48077 to i64
%ae48077 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt62109)
store volatile %struct.ScmObj* %ae48077, %struct.ScmObj** %stackaddr$makeclosure62108, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48077, %struct.ScmObj* %_37length47087, i64 0)
%argslist59930$k475880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62110 = alloca %struct.ScmObj*, align 8
%argslist59930$k475881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48077, %struct.ScmObj* %argslist59930$k475880)
store volatile %struct.ScmObj* %argslist59930$k475881, %struct.ScmObj** %stackaddr$prim62110, align 8
%stackaddr$prim62111 = alloca %struct.ScmObj*, align 8
%argslist59930$k475882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48076, %struct.ScmObj* %argslist59930$k475881)
store volatile %struct.ScmObj* %argslist59930$k475882, %struct.ScmObj** %stackaddr$prim62111, align 8
%clofunc62112 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47588)
musttail call tailcc void %clofunc62112(%struct.ScmObj* %k47588, %struct.ScmObj* %argslist59930$k475882)
ret void
}

define tailcc void @proc_clo$ae48077(%struct.ScmObj* %env$ae48077,%struct.ScmObj* %current_45args59921) {
%stackaddr$env-ref62113 = alloca %struct.ScmObj*, align 8
%_37length47087 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48077, i64 0)
store %struct.ScmObj* %_37length47087, %struct.ScmObj** %stackaddr$env-ref62113
%stackaddr$prim62114 = alloca %struct.ScmObj*, align 8
%k47589 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59921)
store volatile %struct.ScmObj* %k47589, %struct.ScmObj** %stackaddr$prim62114, align 8
%stackaddr$prim62115 = alloca %struct.ScmObj*, align 8
%current_45args59922 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59921)
store volatile %struct.ScmObj* %current_45args59922, %struct.ScmObj** %stackaddr$prim62115, align 8
%stackaddr$prim62116 = alloca %struct.ScmObj*, align 8
%lst47088 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59922)
store volatile %struct.ScmObj* %lst47088, %struct.ScmObj** %stackaddr$prim62116, align 8
%stackaddr$prim62117 = alloca %struct.ScmObj*, align 8
%anf_45bind47247 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47088)
store volatile %struct.ScmObj* %anf_45bind47247, %struct.ScmObj** %stackaddr$prim62117, align 8
%truthy$cmp62118 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47247)
%cmp$cmp62118 = icmp eq i64 %truthy$cmp62118, 1
br i1 %cmp$cmp62118, label %truebranch$cmp62118, label %falsebranch$cmp62118
truebranch$cmp62118:
%ae48081 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48082 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist59924$k475890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62119 = alloca %struct.ScmObj*, align 8
%argslist59924$k475891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48082, %struct.ScmObj* %argslist59924$k475890)
store volatile %struct.ScmObj* %argslist59924$k475891, %struct.ScmObj** %stackaddr$prim62119, align 8
%stackaddr$prim62120 = alloca %struct.ScmObj*, align 8
%argslist59924$k475892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48081, %struct.ScmObj* %argslist59924$k475891)
store volatile %struct.ScmObj* %argslist59924$k475892, %struct.ScmObj** %stackaddr$prim62120, align 8
%clofunc62121 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47589)
musttail call tailcc void %clofunc62121(%struct.ScmObj* %k47589, %struct.ScmObj* %argslist59924$k475892)
ret void
falsebranch$cmp62118:
%stackaddr$prim62122 = alloca %struct.ScmObj*, align 8
%anf_45bind47248 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47088)
store volatile %struct.ScmObj* %anf_45bind47248, %struct.ScmObj** %stackaddr$prim62122, align 8
%stackaddr$makeclosure62123 = alloca %struct.ScmObj*, align 8
%fptrToInt62124 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48091 to i64
%ae48091 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt62124)
store volatile %struct.ScmObj* %ae48091, %struct.ScmObj** %stackaddr$makeclosure62123, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48091, %struct.ScmObj* %k47589, i64 0)
%argslist59929$_37length470870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62125 = alloca %struct.ScmObj*, align 8
%argslist59929$_37length470871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47248, %struct.ScmObj* %argslist59929$_37length470870)
store volatile %struct.ScmObj* %argslist59929$_37length470871, %struct.ScmObj** %stackaddr$prim62125, align 8
%stackaddr$prim62126 = alloca %struct.ScmObj*, align 8
%argslist59929$_37length470872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48091, %struct.ScmObj* %argslist59929$_37length470871)
store volatile %struct.ScmObj* %argslist59929$_37length470872, %struct.ScmObj** %stackaddr$prim62126, align 8
%clofunc62127 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length47087)
musttail call tailcc void %clofunc62127(%struct.ScmObj* %_37length47087, %struct.ScmObj* %argslist59929$_37length470872)
ret void
}

define tailcc void @proc_clo$ae48091(%struct.ScmObj* %env$ae48091,%struct.ScmObj* %current_45args59925) {
%stackaddr$env-ref62128 = alloca %struct.ScmObj*, align 8
%k47589 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48091, i64 0)
store %struct.ScmObj* %k47589, %struct.ScmObj** %stackaddr$env-ref62128
%stackaddr$prim62129 = alloca %struct.ScmObj*, align 8
%_95k47590 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59925)
store volatile %struct.ScmObj* %_95k47590, %struct.ScmObj** %stackaddr$prim62129, align 8
%stackaddr$prim62130 = alloca %struct.ScmObj*, align 8
%current_45args59926 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59925)
store volatile %struct.ScmObj* %current_45args59926, %struct.ScmObj** %stackaddr$prim62130, align 8
%stackaddr$prim62131 = alloca %struct.ScmObj*, align 8
%anf_45bind47249 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59926)
store volatile %struct.ScmObj* %anf_45bind47249, %struct.ScmObj** %stackaddr$prim62131, align 8
%ae48093 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim62132 = alloca %struct.ScmObj*, align 8
%cpsprim47591 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae48093, %struct.ScmObj* %anf_45bind47249)
store volatile %struct.ScmObj* %cpsprim47591, %struct.ScmObj** %stackaddr$prim62132, align 8
%ae48096 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist59928$k475890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62133 = alloca %struct.ScmObj*, align 8
%argslist59928$k475891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47591, %struct.ScmObj* %argslist59928$k475890)
store volatile %struct.ScmObj* %argslist59928$k475891, %struct.ScmObj** %stackaddr$prim62133, align 8
%stackaddr$prim62134 = alloca %struct.ScmObj*, align 8
%argslist59928$k475892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48096, %struct.ScmObj* %argslist59928$k475891)
store volatile %struct.ScmObj* %argslist59928$k475892, %struct.ScmObj** %stackaddr$prim62134, align 8
%clofunc62135 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47589)
musttail call tailcc void %clofunc62135(%struct.ScmObj* %k47589, %struct.ScmObj* %argslist59928$k475892)
ret void
}

define tailcc void @proc_clo$ae47924(%struct.ScmObj* %env$ae47924,%struct.ScmObj* %current_45args59933) {
%stackaddr$prim62136 = alloca %struct.ScmObj*, align 8
%k47592 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59933)
store volatile %struct.ScmObj* %k47592, %struct.ScmObj** %stackaddr$prim62136, align 8
%stackaddr$prim62137 = alloca %struct.ScmObj*, align 8
%current_45args59934 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59933)
store volatile %struct.ScmObj* %current_45args59934, %struct.ScmObj** %stackaddr$prim62137, align 8
%stackaddr$prim62138 = alloca %struct.ScmObj*, align 8
%_37take47090 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59934)
store volatile %struct.ScmObj* %_37take47090, %struct.ScmObj** %stackaddr$prim62138, align 8
%ae47926 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure62139 = alloca %struct.ScmObj*, align 8
%fptrToInt62140 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47927 to i64
%ae47927 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt62140)
store volatile %struct.ScmObj* %ae47927, %struct.ScmObj** %stackaddr$makeclosure62139, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47927, %struct.ScmObj* %_37take47090, i64 0)
%argslist59947$k475920 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62141 = alloca %struct.ScmObj*, align 8
%argslist59947$k475921 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47927, %struct.ScmObj* %argslist59947$k475920)
store volatile %struct.ScmObj* %argslist59947$k475921, %struct.ScmObj** %stackaddr$prim62141, align 8
%stackaddr$prim62142 = alloca %struct.ScmObj*, align 8
%argslist59947$k475922 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47926, %struct.ScmObj* %argslist59947$k475921)
store volatile %struct.ScmObj* %argslist59947$k475922, %struct.ScmObj** %stackaddr$prim62142, align 8
%clofunc62143 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47592)
musttail call tailcc void %clofunc62143(%struct.ScmObj* %k47592, %struct.ScmObj* %argslist59947$k475922)
ret void
}

define tailcc void @proc_clo$ae47927(%struct.ScmObj* %env$ae47927,%struct.ScmObj* %current_45args59936) {
%stackaddr$env-ref62144 = alloca %struct.ScmObj*, align 8
%_37take47090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47927, i64 0)
store %struct.ScmObj* %_37take47090, %struct.ScmObj** %stackaddr$env-ref62144
%stackaddr$prim62145 = alloca %struct.ScmObj*, align 8
%k47593 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59936)
store volatile %struct.ScmObj* %k47593, %struct.ScmObj** %stackaddr$prim62145, align 8
%stackaddr$prim62146 = alloca %struct.ScmObj*, align 8
%current_45args59937 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59936)
store volatile %struct.ScmObj* %current_45args59937, %struct.ScmObj** %stackaddr$prim62146, align 8
%stackaddr$prim62147 = alloca %struct.ScmObj*, align 8
%lst47092 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59937)
store volatile %struct.ScmObj* %lst47092, %struct.ScmObj** %stackaddr$prim62147, align 8
%stackaddr$prim62148 = alloca %struct.ScmObj*, align 8
%current_45args59938 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59937)
store volatile %struct.ScmObj* %current_45args59938, %struct.ScmObj** %stackaddr$prim62148, align 8
%stackaddr$prim62149 = alloca %struct.ScmObj*, align 8
%n47091 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59938)
store volatile %struct.ScmObj* %n47091, %struct.ScmObj** %stackaddr$prim62149, align 8
%ae47929 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim62150 = alloca %struct.ScmObj*, align 8
%anf_45bind47240 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n47091, %struct.ScmObj* %ae47929)
store volatile %struct.ScmObj* %anf_45bind47240, %struct.ScmObj** %stackaddr$prim62150, align 8
%truthy$cmp62151 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47240)
%cmp$cmp62151 = icmp eq i64 %truthy$cmp62151, 1
br i1 %cmp$cmp62151, label %truebranch$cmp62151, label %falsebranch$cmp62151
truebranch$cmp62151:
%ae47932 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47933 = call %struct.ScmObj* @const_init_null()
%argslist59940$k475930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62152 = alloca %struct.ScmObj*, align 8
%argslist59940$k475931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47933, %struct.ScmObj* %argslist59940$k475930)
store volatile %struct.ScmObj* %argslist59940$k475931, %struct.ScmObj** %stackaddr$prim62152, align 8
%stackaddr$prim62153 = alloca %struct.ScmObj*, align 8
%argslist59940$k475932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47932, %struct.ScmObj* %argslist59940$k475931)
store volatile %struct.ScmObj* %argslist59940$k475932, %struct.ScmObj** %stackaddr$prim62153, align 8
%clofunc62154 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47593)
musttail call tailcc void %clofunc62154(%struct.ScmObj* %k47593, %struct.ScmObj* %argslist59940$k475932)
ret void
falsebranch$cmp62151:
%stackaddr$prim62155 = alloca %struct.ScmObj*, align 8
%anf_45bind47241 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47092)
store volatile %struct.ScmObj* %anf_45bind47241, %struct.ScmObj** %stackaddr$prim62155, align 8
%truthy$cmp62156 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47241)
%cmp$cmp62156 = icmp eq i64 %truthy$cmp62156, 1
br i1 %cmp$cmp62156, label %truebranch$cmp62156, label %falsebranch$cmp62156
truebranch$cmp62156:
%ae47943 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47944 = call %struct.ScmObj* @const_init_null()
%argslist59941$k475930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62157 = alloca %struct.ScmObj*, align 8
%argslist59941$k475931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47944, %struct.ScmObj* %argslist59941$k475930)
store volatile %struct.ScmObj* %argslist59941$k475931, %struct.ScmObj** %stackaddr$prim62157, align 8
%stackaddr$prim62158 = alloca %struct.ScmObj*, align 8
%argslist59941$k475932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47943, %struct.ScmObj* %argslist59941$k475931)
store volatile %struct.ScmObj* %argslist59941$k475932, %struct.ScmObj** %stackaddr$prim62158, align 8
%clofunc62159 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47593)
musttail call tailcc void %clofunc62159(%struct.ScmObj* %k47593, %struct.ScmObj* %argslist59941$k475932)
ret void
falsebranch$cmp62156:
%stackaddr$prim62160 = alloca %struct.ScmObj*, align 8
%anf_45bind47242 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47092)
store volatile %struct.ScmObj* %anf_45bind47242, %struct.ScmObj** %stackaddr$prim62160, align 8
%stackaddr$prim62161 = alloca %struct.ScmObj*, align 8
%anf_45bind47243 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47092)
store volatile %struct.ScmObj* %anf_45bind47243, %struct.ScmObj** %stackaddr$prim62161, align 8
%ae47954 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim62162 = alloca %struct.ScmObj*, align 8
%anf_45bind47244 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n47091, %struct.ScmObj* %ae47954)
store volatile %struct.ScmObj* %anf_45bind47244, %struct.ScmObj** %stackaddr$prim62162, align 8
%stackaddr$makeclosure62163 = alloca %struct.ScmObj*, align 8
%fptrToInt62164 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47956 to i64
%ae47956 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt62164)
store volatile %struct.ScmObj* %ae47956, %struct.ScmObj** %stackaddr$makeclosure62163, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47956, %struct.ScmObj* %anf_45bind47242, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47956, %struct.ScmObj* %k47593, i64 1)
%argslist59946$_37take470900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62165 = alloca %struct.ScmObj*, align 8
%argslist59946$_37take470901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47244, %struct.ScmObj* %argslist59946$_37take470900)
store volatile %struct.ScmObj* %argslist59946$_37take470901, %struct.ScmObj** %stackaddr$prim62165, align 8
%stackaddr$prim62166 = alloca %struct.ScmObj*, align 8
%argslist59946$_37take470902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47243, %struct.ScmObj* %argslist59946$_37take470901)
store volatile %struct.ScmObj* %argslist59946$_37take470902, %struct.ScmObj** %stackaddr$prim62166, align 8
%stackaddr$prim62167 = alloca %struct.ScmObj*, align 8
%argslist59946$_37take470903 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47956, %struct.ScmObj* %argslist59946$_37take470902)
store volatile %struct.ScmObj* %argslist59946$_37take470903, %struct.ScmObj** %stackaddr$prim62167, align 8
%clofunc62168 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take47090)
musttail call tailcc void %clofunc62168(%struct.ScmObj* %_37take47090, %struct.ScmObj* %argslist59946$_37take470903)
ret void
}

define tailcc void @proc_clo$ae47956(%struct.ScmObj* %env$ae47956,%struct.ScmObj* %current_45args59942) {
%stackaddr$env-ref62169 = alloca %struct.ScmObj*, align 8
%anf_45bind47242 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47956, i64 0)
store %struct.ScmObj* %anf_45bind47242, %struct.ScmObj** %stackaddr$env-ref62169
%stackaddr$env-ref62170 = alloca %struct.ScmObj*, align 8
%k47593 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47956, i64 1)
store %struct.ScmObj* %k47593, %struct.ScmObj** %stackaddr$env-ref62170
%stackaddr$prim62171 = alloca %struct.ScmObj*, align 8
%_95k47594 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59942)
store volatile %struct.ScmObj* %_95k47594, %struct.ScmObj** %stackaddr$prim62171, align 8
%stackaddr$prim62172 = alloca %struct.ScmObj*, align 8
%current_45args59943 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59942)
store volatile %struct.ScmObj* %current_45args59943, %struct.ScmObj** %stackaddr$prim62172, align 8
%stackaddr$prim62173 = alloca %struct.ScmObj*, align 8
%anf_45bind47245 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59943)
store volatile %struct.ScmObj* %anf_45bind47245, %struct.ScmObj** %stackaddr$prim62173, align 8
%stackaddr$prim62174 = alloca %struct.ScmObj*, align 8
%cpsprim47595 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47242, %struct.ScmObj* %anf_45bind47245)
store volatile %struct.ScmObj* %cpsprim47595, %struct.ScmObj** %stackaddr$prim62174, align 8
%ae47962 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist59945$k475930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62175 = alloca %struct.ScmObj*, align 8
%argslist59945$k475931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47595, %struct.ScmObj* %argslist59945$k475930)
store volatile %struct.ScmObj* %argslist59945$k475931, %struct.ScmObj** %stackaddr$prim62175, align 8
%stackaddr$prim62176 = alloca %struct.ScmObj*, align 8
%argslist59945$k475932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47962, %struct.ScmObj* %argslist59945$k475931)
store volatile %struct.ScmObj* %argslist59945$k475932, %struct.ScmObj** %stackaddr$prim62176, align 8
%clofunc62177 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47593)
musttail call tailcc void %clofunc62177(%struct.ScmObj* %k47593, %struct.ScmObj* %argslist59945$k475932)
ret void
}

define tailcc void @proc_clo$ae47827(%struct.ScmObj* %env$ae47827,%struct.ScmObj* %current_45args59950) {
%stackaddr$prim62178 = alloca %struct.ScmObj*, align 8
%k47596 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59950)
store volatile %struct.ScmObj* %k47596, %struct.ScmObj** %stackaddr$prim62178, align 8
%stackaddr$prim62179 = alloca %struct.ScmObj*, align 8
%current_45args59951 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59950)
store volatile %struct.ScmObj* %current_45args59951, %struct.ScmObj** %stackaddr$prim62179, align 8
%stackaddr$prim62180 = alloca %struct.ScmObj*, align 8
%_37map47094 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59951)
store volatile %struct.ScmObj* %_37map47094, %struct.ScmObj** %stackaddr$prim62180, align 8
%ae47829 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure62181 = alloca %struct.ScmObj*, align 8
%fptrToInt62182 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47830 to i64
%ae47830 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt62182)
store volatile %struct.ScmObj* %ae47830, %struct.ScmObj** %stackaddr$makeclosure62181, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47830, %struct.ScmObj* %_37map47094, i64 0)
%argslist59967$k475960 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62183 = alloca %struct.ScmObj*, align 8
%argslist59967$k475961 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47830, %struct.ScmObj* %argslist59967$k475960)
store volatile %struct.ScmObj* %argslist59967$k475961, %struct.ScmObj** %stackaddr$prim62183, align 8
%stackaddr$prim62184 = alloca %struct.ScmObj*, align 8
%argslist59967$k475962 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47829, %struct.ScmObj* %argslist59967$k475961)
store volatile %struct.ScmObj* %argslist59967$k475962, %struct.ScmObj** %stackaddr$prim62184, align 8
%clofunc62185 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47596)
musttail call tailcc void %clofunc62185(%struct.ScmObj* %k47596, %struct.ScmObj* %argslist59967$k475962)
ret void
}

define tailcc void @proc_clo$ae47830(%struct.ScmObj* %env$ae47830,%struct.ScmObj* %current_45args59953) {
%stackaddr$env-ref62186 = alloca %struct.ScmObj*, align 8
%_37map47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47830, i64 0)
store %struct.ScmObj* %_37map47094, %struct.ScmObj** %stackaddr$env-ref62186
%stackaddr$prim62187 = alloca %struct.ScmObj*, align 8
%k47597 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59953)
store volatile %struct.ScmObj* %k47597, %struct.ScmObj** %stackaddr$prim62187, align 8
%stackaddr$prim62188 = alloca %struct.ScmObj*, align 8
%current_45args59954 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59953)
store volatile %struct.ScmObj* %current_45args59954, %struct.ScmObj** %stackaddr$prim62188, align 8
%stackaddr$prim62189 = alloca %struct.ScmObj*, align 8
%f47096 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59954)
store volatile %struct.ScmObj* %f47096, %struct.ScmObj** %stackaddr$prim62189, align 8
%stackaddr$prim62190 = alloca %struct.ScmObj*, align 8
%current_45args59955 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59954)
store volatile %struct.ScmObj* %current_45args59955, %struct.ScmObj** %stackaddr$prim62190, align 8
%stackaddr$prim62191 = alloca %struct.ScmObj*, align 8
%lst47095 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59955)
store volatile %struct.ScmObj* %lst47095, %struct.ScmObj** %stackaddr$prim62191, align 8
%stackaddr$prim62192 = alloca %struct.ScmObj*, align 8
%anf_45bind47234 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47095)
store volatile %struct.ScmObj* %anf_45bind47234, %struct.ScmObj** %stackaddr$prim62192, align 8
%truthy$cmp62193 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47234)
%cmp$cmp62193 = icmp eq i64 %truthy$cmp62193, 1
br i1 %cmp$cmp62193, label %truebranch$cmp62193, label %falsebranch$cmp62193
truebranch$cmp62193:
%ae47834 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47835 = call %struct.ScmObj* @const_init_null()
%argslist59957$k475970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62194 = alloca %struct.ScmObj*, align 8
%argslist59957$k475971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47835, %struct.ScmObj* %argslist59957$k475970)
store volatile %struct.ScmObj* %argslist59957$k475971, %struct.ScmObj** %stackaddr$prim62194, align 8
%stackaddr$prim62195 = alloca %struct.ScmObj*, align 8
%argslist59957$k475972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47834, %struct.ScmObj* %argslist59957$k475971)
store volatile %struct.ScmObj* %argslist59957$k475972, %struct.ScmObj** %stackaddr$prim62195, align 8
%clofunc62196 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47597)
musttail call tailcc void %clofunc62196(%struct.ScmObj* %k47597, %struct.ScmObj* %argslist59957$k475972)
ret void
falsebranch$cmp62193:
%stackaddr$prim62197 = alloca %struct.ScmObj*, align 8
%anf_45bind47235 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47095)
store volatile %struct.ScmObj* %anf_45bind47235, %struct.ScmObj** %stackaddr$prim62197, align 8
%stackaddr$makeclosure62198 = alloca %struct.ScmObj*, align 8
%fptrToInt62199 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47844 to i64
%ae47844 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt62199)
store volatile %struct.ScmObj* %ae47844, %struct.ScmObj** %stackaddr$makeclosure62198, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47844, %struct.ScmObj* %_37map47094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47844, %struct.ScmObj* %k47597, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47844, %struct.ScmObj* %f47096, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae47844, %struct.ScmObj* %lst47095, i64 3)
%argslist59966$f470960 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62200 = alloca %struct.ScmObj*, align 8
%argslist59966$f470961 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47235, %struct.ScmObj* %argslist59966$f470960)
store volatile %struct.ScmObj* %argslist59966$f470961, %struct.ScmObj** %stackaddr$prim62200, align 8
%stackaddr$prim62201 = alloca %struct.ScmObj*, align 8
%argslist59966$f470962 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47844, %struct.ScmObj* %argslist59966$f470961)
store volatile %struct.ScmObj* %argslist59966$f470962, %struct.ScmObj** %stackaddr$prim62201, align 8
%clofunc62202 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47096)
musttail call tailcc void %clofunc62202(%struct.ScmObj* %f47096, %struct.ScmObj* %argslist59966$f470962)
ret void
}

define tailcc void @proc_clo$ae47844(%struct.ScmObj* %env$ae47844,%struct.ScmObj* %current_45args59958) {
%stackaddr$env-ref62203 = alloca %struct.ScmObj*, align 8
%_37map47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47844, i64 0)
store %struct.ScmObj* %_37map47094, %struct.ScmObj** %stackaddr$env-ref62203
%stackaddr$env-ref62204 = alloca %struct.ScmObj*, align 8
%k47597 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47844, i64 1)
store %struct.ScmObj* %k47597, %struct.ScmObj** %stackaddr$env-ref62204
%stackaddr$env-ref62205 = alloca %struct.ScmObj*, align 8
%f47096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47844, i64 2)
store %struct.ScmObj* %f47096, %struct.ScmObj** %stackaddr$env-ref62205
%stackaddr$env-ref62206 = alloca %struct.ScmObj*, align 8
%lst47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47844, i64 3)
store %struct.ScmObj* %lst47095, %struct.ScmObj** %stackaddr$env-ref62206
%stackaddr$prim62207 = alloca %struct.ScmObj*, align 8
%_95k47598 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59958)
store volatile %struct.ScmObj* %_95k47598, %struct.ScmObj** %stackaddr$prim62207, align 8
%stackaddr$prim62208 = alloca %struct.ScmObj*, align 8
%current_45args59959 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59958)
store volatile %struct.ScmObj* %current_45args59959, %struct.ScmObj** %stackaddr$prim62208, align 8
%stackaddr$prim62209 = alloca %struct.ScmObj*, align 8
%anf_45bind47236 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59959)
store volatile %struct.ScmObj* %anf_45bind47236, %struct.ScmObj** %stackaddr$prim62209, align 8
%stackaddr$prim62210 = alloca %struct.ScmObj*, align 8
%anf_45bind47237 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47095)
store volatile %struct.ScmObj* %anf_45bind47237, %struct.ScmObj** %stackaddr$prim62210, align 8
%stackaddr$makeclosure62211 = alloca %struct.ScmObj*, align 8
%fptrToInt62212 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47848 to i64
%ae47848 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt62212)
store volatile %struct.ScmObj* %ae47848, %struct.ScmObj** %stackaddr$makeclosure62211, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47848, %struct.ScmObj* %anf_45bind47236, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47848, %struct.ScmObj* %k47597, i64 1)
%argslist59965$_37map470940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62213 = alloca %struct.ScmObj*, align 8
%argslist59965$_37map470941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47237, %struct.ScmObj* %argslist59965$_37map470940)
store volatile %struct.ScmObj* %argslist59965$_37map470941, %struct.ScmObj** %stackaddr$prim62213, align 8
%stackaddr$prim62214 = alloca %struct.ScmObj*, align 8
%argslist59965$_37map470942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47096, %struct.ScmObj* %argslist59965$_37map470941)
store volatile %struct.ScmObj* %argslist59965$_37map470942, %struct.ScmObj** %stackaddr$prim62214, align 8
%stackaddr$prim62215 = alloca %struct.ScmObj*, align 8
%argslist59965$_37map470943 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47848, %struct.ScmObj* %argslist59965$_37map470942)
store volatile %struct.ScmObj* %argslist59965$_37map470943, %struct.ScmObj** %stackaddr$prim62215, align 8
%clofunc62216 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map47094)
musttail call tailcc void %clofunc62216(%struct.ScmObj* %_37map47094, %struct.ScmObj* %argslist59965$_37map470943)
ret void
}

define tailcc void @proc_clo$ae47848(%struct.ScmObj* %env$ae47848,%struct.ScmObj* %current_45args59961) {
%stackaddr$env-ref62217 = alloca %struct.ScmObj*, align 8
%anf_45bind47236 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47848, i64 0)
store %struct.ScmObj* %anf_45bind47236, %struct.ScmObj** %stackaddr$env-ref62217
%stackaddr$env-ref62218 = alloca %struct.ScmObj*, align 8
%k47597 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47848, i64 1)
store %struct.ScmObj* %k47597, %struct.ScmObj** %stackaddr$env-ref62218
%stackaddr$prim62219 = alloca %struct.ScmObj*, align 8
%_95k47599 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59961)
store volatile %struct.ScmObj* %_95k47599, %struct.ScmObj** %stackaddr$prim62219, align 8
%stackaddr$prim62220 = alloca %struct.ScmObj*, align 8
%current_45args59962 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59961)
store volatile %struct.ScmObj* %current_45args59962, %struct.ScmObj** %stackaddr$prim62220, align 8
%stackaddr$prim62221 = alloca %struct.ScmObj*, align 8
%anf_45bind47238 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59962)
store volatile %struct.ScmObj* %anf_45bind47238, %struct.ScmObj** %stackaddr$prim62221, align 8
%stackaddr$prim62222 = alloca %struct.ScmObj*, align 8
%cpsprim47600 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47236, %struct.ScmObj* %anf_45bind47238)
store volatile %struct.ScmObj* %cpsprim47600, %struct.ScmObj** %stackaddr$prim62222, align 8
%ae47854 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist59964$k475970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62223 = alloca %struct.ScmObj*, align 8
%argslist59964$k475971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47600, %struct.ScmObj* %argslist59964$k475970)
store volatile %struct.ScmObj* %argslist59964$k475971, %struct.ScmObj** %stackaddr$prim62223, align 8
%stackaddr$prim62224 = alloca %struct.ScmObj*, align 8
%argslist59964$k475972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47854, %struct.ScmObj* %argslist59964$k475971)
store volatile %struct.ScmObj* %argslist59964$k475972, %struct.ScmObj** %stackaddr$prim62224, align 8
%clofunc62225 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47597)
musttail call tailcc void %clofunc62225(%struct.ScmObj* %k47597, %struct.ScmObj* %argslist59964$k475972)
ret void
}

define tailcc void @proc_clo$ae47747(%struct.ScmObj* %env$ae47747,%struct.ScmObj* %current_45args59970) {
%stackaddr$prim62226 = alloca %struct.ScmObj*, align 8
%k47601 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59970)
store volatile %struct.ScmObj* %k47601, %struct.ScmObj** %stackaddr$prim62226, align 8
%stackaddr$prim62227 = alloca %struct.ScmObj*, align 8
%current_45args59971 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59970)
store volatile %struct.ScmObj* %current_45args59971, %struct.ScmObj** %stackaddr$prim62227, align 8
%stackaddr$prim62228 = alloca %struct.ScmObj*, align 8
%_37foldr147098 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59971)
store volatile %struct.ScmObj* %_37foldr147098, %struct.ScmObj** %stackaddr$prim62228, align 8
%ae47749 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure62229 = alloca %struct.ScmObj*, align 8
%fptrToInt62230 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47750 to i64
%ae47750 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt62230)
store volatile %struct.ScmObj* %ae47750, %struct.ScmObj** %stackaddr$makeclosure62229, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47750, %struct.ScmObj* %_37foldr147098, i64 0)
%argslist59984$k476010 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62231 = alloca %struct.ScmObj*, align 8
%argslist59984$k476011 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47750, %struct.ScmObj* %argslist59984$k476010)
store volatile %struct.ScmObj* %argslist59984$k476011, %struct.ScmObj** %stackaddr$prim62231, align 8
%stackaddr$prim62232 = alloca %struct.ScmObj*, align 8
%argslist59984$k476012 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47749, %struct.ScmObj* %argslist59984$k476011)
store volatile %struct.ScmObj* %argslist59984$k476012, %struct.ScmObj** %stackaddr$prim62232, align 8
%clofunc62233 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47601)
musttail call tailcc void %clofunc62233(%struct.ScmObj* %k47601, %struct.ScmObj* %argslist59984$k476012)
ret void
}

define tailcc void @proc_clo$ae47750(%struct.ScmObj* %env$ae47750,%struct.ScmObj* %current_45args59973) {
%stackaddr$env-ref62234 = alloca %struct.ScmObj*, align 8
%_37foldr147098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47750, i64 0)
store %struct.ScmObj* %_37foldr147098, %struct.ScmObj** %stackaddr$env-ref62234
%stackaddr$prim62235 = alloca %struct.ScmObj*, align 8
%k47602 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59973)
store volatile %struct.ScmObj* %k47602, %struct.ScmObj** %stackaddr$prim62235, align 8
%stackaddr$prim62236 = alloca %struct.ScmObj*, align 8
%current_45args59974 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59973)
store volatile %struct.ScmObj* %current_45args59974, %struct.ScmObj** %stackaddr$prim62236, align 8
%stackaddr$prim62237 = alloca %struct.ScmObj*, align 8
%f47101 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59974)
store volatile %struct.ScmObj* %f47101, %struct.ScmObj** %stackaddr$prim62237, align 8
%stackaddr$prim62238 = alloca %struct.ScmObj*, align 8
%current_45args59975 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59974)
store volatile %struct.ScmObj* %current_45args59975, %struct.ScmObj** %stackaddr$prim62238, align 8
%stackaddr$prim62239 = alloca %struct.ScmObj*, align 8
%acc47100 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59975)
store volatile %struct.ScmObj* %acc47100, %struct.ScmObj** %stackaddr$prim62239, align 8
%stackaddr$prim62240 = alloca %struct.ScmObj*, align 8
%current_45args59976 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59975)
store volatile %struct.ScmObj* %current_45args59976, %struct.ScmObj** %stackaddr$prim62240, align 8
%stackaddr$prim62241 = alloca %struct.ScmObj*, align 8
%lst47099 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59976)
store volatile %struct.ScmObj* %lst47099, %struct.ScmObj** %stackaddr$prim62241, align 8
%stackaddr$prim62242 = alloca %struct.ScmObj*, align 8
%anf_45bind47229 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47099)
store volatile %struct.ScmObj* %anf_45bind47229, %struct.ScmObj** %stackaddr$prim62242, align 8
%truthy$cmp62243 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47229)
%cmp$cmp62243 = icmp eq i64 %truthy$cmp62243, 1
br i1 %cmp$cmp62243, label %truebranch$cmp62243, label %falsebranch$cmp62243
truebranch$cmp62243:
%ae47754 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist59978$k476020 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62244 = alloca %struct.ScmObj*, align 8
%argslist59978$k476021 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47100, %struct.ScmObj* %argslist59978$k476020)
store volatile %struct.ScmObj* %argslist59978$k476021, %struct.ScmObj** %stackaddr$prim62244, align 8
%stackaddr$prim62245 = alloca %struct.ScmObj*, align 8
%argslist59978$k476022 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47754, %struct.ScmObj* %argslist59978$k476021)
store volatile %struct.ScmObj* %argslist59978$k476022, %struct.ScmObj** %stackaddr$prim62245, align 8
%clofunc62246 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47602)
musttail call tailcc void %clofunc62246(%struct.ScmObj* %k47602, %struct.ScmObj* %argslist59978$k476022)
ret void
falsebranch$cmp62243:
%stackaddr$prim62247 = alloca %struct.ScmObj*, align 8
%anf_45bind47230 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47099)
store volatile %struct.ScmObj* %anf_45bind47230, %struct.ScmObj** %stackaddr$prim62247, align 8
%stackaddr$prim62248 = alloca %struct.ScmObj*, align 8
%anf_45bind47231 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47099)
store volatile %struct.ScmObj* %anf_45bind47231, %struct.ScmObj** %stackaddr$prim62248, align 8
%stackaddr$makeclosure62249 = alloca %struct.ScmObj*, align 8
%fptrToInt62250 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47762 to i64
%ae47762 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt62250)
store volatile %struct.ScmObj* %ae47762, %struct.ScmObj** %stackaddr$makeclosure62249, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47762, %struct.ScmObj* %k47602, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47762, %struct.ScmObj* %anf_45bind47230, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47762, %struct.ScmObj* %f47101, i64 2)
%argslist59983$_37foldr1470980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62251 = alloca %struct.ScmObj*, align 8
%argslist59983$_37foldr1470981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47231, %struct.ScmObj* %argslist59983$_37foldr1470980)
store volatile %struct.ScmObj* %argslist59983$_37foldr1470981, %struct.ScmObj** %stackaddr$prim62251, align 8
%stackaddr$prim62252 = alloca %struct.ScmObj*, align 8
%argslist59983$_37foldr1470982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47100, %struct.ScmObj* %argslist59983$_37foldr1470981)
store volatile %struct.ScmObj* %argslist59983$_37foldr1470982, %struct.ScmObj** %stackaddr$prim62252, align 8
%stackaddr$prim62253 = alloca %struct.ScmObj*, align 8
%argslist59983$_37foldr1470983 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47101, %struct.ScmObj* %argslist59983$_37foldr1470982)
store volatile %struct.ScmObj* %argslist59983$_37foldr1470983, %struct.ScmObj** %stackaddr$prim62253, align 8
%stackaddr$prim62254 = alloca %struct.ScmObj*, align 8
%argslist59983$_37foldr1470984 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47762, %struct.ScmObj* %argslist59983$_37foldr1470983)
store volatile %struct.ScmObj* %argslist59983$_37foldr1470984, %struct.ScmObj** %stackaddr$prim62254, align 8
%clofunc62255 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147098)
musttail call tailcc void %clofunc62255(%struct.ScmObj* %_37foldr147098, %struct.ScmObj* %argslist59983$_37foldr1470984)
ret void
}

define tailcc void @proc_clo$ae47762(%struct.ScmObj* %env$ae47762,%struct.ScmObj* %current_45args59979) {
%stackaddr$env-ref62256 = alloca %struct.ScmObj*, align 8
%k47602 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47762, i64 0)
store %struct.ScmObj* %k47602, %struct.ScmObj** %stackaddr$env-ref62256
%stackaddr$env-ref62257 = alloca %struct.ScmObj*, align 8
%anf_45bind47230 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47762, i64 1)
store %struct.ScmObj* %anf_45bind47230, %struct.ScmObj** %stackaddr$env-ref62257
%stackaddr$env-ref62258 = alloca %struct.ScmObj*, align 8
%f47101 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47762, i64 2)
store %struct.ScmObj* %f47101, %struct.ScmObj** %stackaddr$env-ref62258
%stackaddr$prim62259 = alloca %struct.ScmObj*, align 8
%_95k47603 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59979)
store volatile %struct.ScmObj* %_95k47603, %struct.ScmObj** %stackaddr$prim62259, align 8
%stackaddr$prim62260 = alloca %struct.ScmObj*, align 8
%current_45args59980 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59979)
store volatile %struct.ScmObj* %current_45args59980, %struct.ScmObj** %stackaddr$prim62260, align 8
%stackaddr$prim62261 = alloca %struct.ScmObj*, align 8
%anf_45bind47232 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59980)
store volatile %struct.ScmObj* %anf_45bind47232, %struct.ScmObj** %stackaddr$prim62261, align 8
%argslist59982$f471010 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62262 = alloca %struct.ScmObj*, align 8
%argslist59982$f471011 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47232, %struct.ScmObj* %argslist59982$f471010)
store volatile %struct.ScmObj* %argslist59982$f471011, %struct.ScmObj** %stackaddr$prim62262, align 8
%stackaddr$prim62263 = alloca %struct.ScmObj*, align 8
%argslist59982$f471012 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47230, %struct.ScmObj* %argslist59982$f471011)
store volatile %struct.ScmObj* %argslist59982$f471012, %struct.ScmObj** %stackaddr$prim62263, align 8
%stackaddr$prim62264 = alloca %struct.ScmObj*, align 8
%argslist59982$f471013 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47602, %struct.ScmObj* %argslist59982$f471012)
store volatile %struct.ScmObj* %argslist59982$f471013, %struct.ScmObj** %stackaddr$prim62264, align 8
%clofunc62265 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47101)
musttail call tailcc void %clofunc62265(%struct.ScmObj* %f47101, %struct.ScmObj* %argslist59982$f471013)
ret void
}

define tailcc void @proc_clo$ae47630(%struct.ScmObj* %env$ae47630,%struct.ScmObj* %current_45args59987) {
%stackaddr$prim62266 = alloca %struct.ScmObj*, align 8
%k47604 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59987)
store volatile %struct.ScmObj* %k47604, %struct.ScmObj** %stackaddr$prim62266, align 8
%stackaddr$prim62267 = alloca %struct.ScmObj*, align 8
%current_45args59988 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59987)
store volatile %struct.ScmObj* %current_45args59988, %struct.ScmObj** %stackaddr$prim62267, align 8
%stackaddr$prim62268 = alloca %struct.ScmObj*, align 8
%y47078 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59988)
store volatile %struct.ScmObj* %y47078, %struct.ScmObj** %stackaddr$prim62268, align 8
%ae47632 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure62269 = alloca %struct.ScmObj*, align 8
%fptrToInt62270 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47633 to i64
%ae47633 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt62270)
store volatile %struct.ScmObj* %ae47633, %struct.ScmObj** %stackaddr$makeclosure62269, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47633, %struct.ScmObj* %y47078, i64 0)
%argslist60006$k476040 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62271 = alloca %struct.ScmObj*, align 8
%argslist60006$k476041 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47633, %struct.ScmObj* %argslist60006$k476040)
store volatile %struct.ScmObj* %argslist60006$k476041, %struct.ScmObj** %stackaddr$prim62271, align 8
%stackaddr$prim62272 = alloca %struct.ScmObj*, align 8
%argslist60006$k476042 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47632, %struct.ScmObj* %argslist60006$k476041)
store volatile %struct.ScmObj* %argslist60006$k476042, %struct.ScmObj** %stackaddr$prim62272, align 8
%clofunc62273 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47604)
musttail call tailcc void %clofunc62273(%struct.ScmObj* %k47604, %struct.ScmObj* %argslist60006$k476042)
ret void
}

define tailcc void @proc_clo$ae47633(%struct.ScmObj* %env$ae47633,%struct.ScmObj* %current_45args59990) {
%stackaddr$env-ref62274 = alloca %struct.ScmObj*, align 8
%y47078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47633, i64 0)
store %struct.ScmObj* %y47078, %struct.ScmObj** %stackaddr$env-ref62274
%stackaddr$prim62275 = alloca %struct.ScmObj*, align 8
%k47605 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59990)
store volatile %struct.ScmObj* %k47605, %struct.ScmObj** %stackaddr$prim62275, align 8
%stackaddr$prim62276 = alloca %struct.ScmObj*, align 8
%current_45args59991 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59990)
store volatile %struct.ScmObj* %current_45args59991, %struct.ScmObj** %stackaddr$prim62276, align 8
%stackaddr$prim62277 = alloca %struct.ScmObj*, align 8
%f47079 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59991)
store volatile %struct.ScmObj* %f47079, %struct.ScmObj** %stackaddr$prim62277, align 8
%stackaddr$makeclosure62278 = alloca %struct.ScmObj*, align 8
%fptrToInt62279 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47634 to i64
%ae47634 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt62279)
store volatile %struct.ScmObj* %ae47634, %struct.ScmObj** %stackaddr$makeclosure62278, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47634, %struct.ScmObj* %k47605, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47634, %struct.ScmObj* %f47079, i64 1)
%ae47635 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure62280 = alloca %struct.ScmObj*, align 8
%fptrToInt62281 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47636 to i64
%ae47636 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt62281)
store volatile %struct.ScmObj* %ae47636, %struct.ScmObj** %stackaddr$makeclosure62280, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47636, %struct.ScmObj* %y47078, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47636, %struct.ScmObj* %f47079, i64 1)
%argslist60005$ae476340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62282 = alloca %struct.ScmObj*, align 8
%argslist60005$ae476341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47636, %struct.ScmObj* %argslist60005$ae476340)
store volatile %struct.ScmObj* %argslist60005$ae476341, %struct.ScmObj** %stackaddr$prim62282, align 8
%stackaddr$prim62283 = alloca %struct.ScmObj*, align 8
%argslist60005$ae476342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47635, %struct.ScmObj* %argslist60005$ae476341)
store volatile %struct.ScmObj* %argslist60005$ae476342, %struct.ScmObj** %stackaddr$prim62283, align 8
%clofunc62284 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47634)
musttail call tailcc void %clofunc62284(%struct.ScmObj* %ae47634, %struct.ScmObj* %argslist60005$ae476342)
ret void
}

define tailcc void @proc_clo$ae47634(%struct.ScmObj* %env$ae47634,%struct.ScmObj* %current_45args59993) {
%stackaddr$env-ref62285 = alloca %struct.ScmObj*, align 8
%k47605 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47634, i64 0)
store %struct.ScmObj* %k47605, %struct.ScmObj** %stackaddr$env-ref62285
%stackaddr$env-ref62286 = alloca %struct.ScmObj*, align 8
%f47079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47634, i64 1)
store %struct.ScmObj* %f47079, %struct.ScmObj** %stackaddr$env-ref62286
%stackaddr$prim62287 = alloca %struct.ScmObj*, align 8
%_95k47606 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59993)
store volatile %struct.ScmObj* %_95k47606, %struct.ScmObj** %stackaddr$prim62287, align 8
%stackaddr$prim62288 = alloca %struct.ScmObj*, align 8
%current_45args59994 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59993)
store volatile %struct.ScmObj* %current_45args59994, %struct.ScmObj** %stackaddr$prim62288, align 8
%stackaddr$prim62289 = alloca %struct.ScmObj*, align 8
%anf_45bind47227 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59994)
store volatile %struct.ScmObj* %anf_45bind47227, %struct.ScmObj** %stackaddr$prim62289, align 8
%argslist59996$f470790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62290 = alloca %struct.ScmObj*, align 8
%argslist59996$f470791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47227, %struct.ScmObj* %argslist59996$f470790)
store volatile %struct.ScmObj* %argslist59996$f470791, %struct.ScmObj** %stackaddr$prim62290, align 8
%stackaddr$prim62291 = alloca %struct.ScmObj*, align 8
%argslist59996$f470792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47605, %struct.ScmObj* %argslist59996$f470791)
store volatile %struct.ScmObj* %argslist59996$f470792, %struct.ScmObj** %stackaddr$prim62291, align 8
%clofunc62292 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47079)
musttail call tailcc void %clofunc62292(%struct.ScmObj* %f47079, %struct.ScmObj* %argslist59996$f470792)
ret void
}

define tailcc void @proc_clo$ae47636(%struct.ScmObj* %env$ae47636,%struct.ScmObj* %args4708047607) {
%stackaddr$env-ref62293 = alloca %struct.ScmObj*, align 8
%y47078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47636, i64 0)
store %struct.ScmObj* %y47078, %struct.ScmObj** %stackaddr$env-ref62293
%stackaddr$env-ref62294 = alloca %struct.ScmObj*, align 8
%f47079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47636, i64 1)
store %struct.ScmObj* %f47079, %struct.ScmObj** %stackaddr$env-ref62294
%stackaddr$prim62295 = alloca %struct.ScmObj*, align 8
%k47608 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4708047607)
store volatile %struct.ScmObj* %k47608, %struct.ScmObj** %stackaddr$prim62295, align 8
%stackaddr$prim62296 = alloca %struct.ScmObj*, align 8
%args47080 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4708047607)
store volatile %struct.ScmObj* %args47080, %struct.ScmObj** %stackaddr$prim62296, align 8
%stackaddr$makeclosure62297 = alloca %struct.ScmObj*, align 8
%fptrToInt62298 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47640 to i64
%ae47640 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt62298)
store volatile %struct.ScmObj* %ae47640, %struct.ScmObj** %stackaddr$makeclosure62297, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47640, %struct.ScmObj* %k47608, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47640, %struct.ScmObj* %args47080, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47640, %struct.ScmObj* %f47079, i64 2)
%argslist60004$y470780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62299 = alloca %struct.ScmObj*, align 8
%argslist60004$y470781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y47078, %struct.ScmObj* %argslist60004$y470780)
store volatile %struct.ScmObj* %argslist60004$y470781, %struct.ScmObj** %stackaddr$prim62299, align 8
%stackaddr$prim62300 = alloca %struct.ScmObj*, align 8
%argslist60004$y470782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47640, %struct.ScmObj* %argslist60004$y470781)
store volatile %struct.ScmObj* %argslist60004$y470782, %struct.ScmObj** %stackaddr$prim62300, align 8
%clofunc62301 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y47078)
musttail call tailcc void %clofunc62301(%struct.ScmObj* %y47078, %struct.ScmObj* %argslist60004$y470782)
ret void
}

define tailcc void @proc_clo$ae47640(%struct.ScmObj* %env$ae47640,%struct.ScmObj* %current_45args59997) {
%stackaddr$env-ref62302 = alloca %struct.ScmObj*, align 8
%k47608 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47640, i64 0)
store %struct.ScmObj* %k47608, %struct.ScmObj** %stackaddr$env-ref62302
%stackaddr$env-ref62303 = alloca %struct.ScmObj*, align 8
%args47080 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47640, i64 1)
store %struct.ScmObj* %args47080, %struct.ScmObj** %stackaddr$env-ref62303
%stackaddr$env-ref62304 = alloca %struct.ScmObj*, align 8
%f47079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47640, i64 2)
store %struct.ScmObj* %f47079, %struct.ScmObj** %stackaddr$env-ref62304
%stackaddr$prim62305 = alloca %struct.ScmObj*, align 8
%_95k47609 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59997)
store volatile %struct.ScmObj* %_95k47609, %struct.ScmObj** %stackaddr$prim62305, align 8
%stackaddr$prim62306 = alloca %struct.ScmObj*, align 8
%current_45args59998 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args59997)
store volatile %struct.ScmObj* %current_45args59998, %struct.ScmObj** %stackaddr$prim62306, align 8
%stackaddr$prim62307 = alloca %struct.ScmObj*, align 8
%anf_45bind47225 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args59998)
store volatile %struct.ScmObj* %anf_45bind47225, %struct.ScmObj** %stackaddr$prim62307, align 8
%stackaddr$makeclosure62308 = alloca %struct.ScmObj*, align 8
%fptrToInt62309 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47643 to i64
%ae47643 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt62309)
store volatile %struct.ScmObj* %ae47643, %struct.ScmObj** %stackaddr$makeclosure62308, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47643, %struct.ScmObj* %k47608, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47643, %struct.ScmObj* %args47080, i64 1)
%argslist60003$anf_45bind472250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62310 = alloca %struct.ScmObj*, align 8
%argslist60003$anf_45bind472251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47079, %struct.ScmObj* %argslist60003$anf_45bind472250)
store volatile %struct.ScmObj* %argslist60003$anf_45bind472251, %struct.ScmObj** %stackaddr$prim62310, align 8
%stackaddr$prim62311 = alloca %struct.ScmObj*, align 8
%argslist60003$anf_45bind472252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47643, %struct.ScmObj* %argslist60003$anf_45bind472251)
store volatile %struct.ScmObj* %argslist60003$anf_45bind472252, %struct.ScmObj** %stackaddr$prim62311, align 8
%clofunc62312 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47225)
musttail call tailcc void %clofunc62312(%struct.ScmObj* %anf_45bind47225, %struct.ScmObj* %argslist60003$anf_45bind472252)
ret void
}

define tailcc void @proc_clo$ae47643(%struct.ScmObj* %env$ae47643,%struct.ScmObj* %current_45args60000) {
%stackaddr$env-ref62313 = alloca %struct.ScmObj*, align 8
%k47608 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47643, i64 0)
store %struct.ScmObj* %k47608, %struct.ScmObj** %stackaddr$env-ref62313
%stackaddr$env-ref62314 = alloca %struct.ScmObj*, align 8
%args47080 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47643, i64 1)
store %struct.ScmObj* %args47080, %struct.ScmObj** %stackaddr$env-ref62314
%stackaddr$prim62315 = alloca %struct.ScmObj*, align 8
%_95k47610 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60000)
store volatile %struct.ScmObj* %_95k47610, %struct.ScmObj** %stackaddr$prim62315, align 8
%stackaddr$prim62316 = alloca %struct.ScmObj*, align 8
%current_45args60001 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60000)
store volatile %struct.ScmObj* %current_45args60001, %struct.ScmObj** %stackaddr$prim62316, align 8
%stackaddr$prim62317 = alloca %struct.ScmObj*, align 8
%anf_45bind47226 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60001)
store volatile %struct.ScmObj* %anf_45bind47226, %struct.ScmObj** %stackaddr$prim62317, align 8
%stackaddr$prim62318 = alloca %struct.ScmObj*, align 8
%cpsargs47611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47608, %struct.ScmObj* %args47080)
store volatile %struct.ScmObj* %cpsargs47611, %struct.ScmObj** %stackaddr$prim62318, align 8
%clofunc62319 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47226)
musttail call tailcc void %clofunc62319(%struct.ScmObj* %anf_45bind47226, %struct.ScmObj* %cpsargs47611)
ret void
}

define tailcc void @proc_clo$ae47615(%struct.ScmObj* %env$ae47615,%struct.ScmObj* %current_45args60008) {
%stackaddr$prim62320 = alloca %struct.ScmObj*, align 8
%k47612 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60008)
store volatile %struct.ScmObj* %k47612, %struct.ScmObj** %stackaddr$prim62320, align 8
%stackaddr$prim62321 = alloca %struct.ScmObj*, align 8
%current_45args60009 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args60008)
store volatile %struct.ScmObj* %current_45args60009, %struct.ScmObj** %stackaddr$prim62321, align 8
%stackaddr$prim62322 = alloca %struct.ScmObj*, align 8
%yu47077 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args60009)
store volatile %struct.ScmObj* %yu47077, %struct.ScmObj** %stackaddr$prim62322, align 8
%argslist60011$yu470770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim62323 = alloca %struct.ScmObj*, align 8
%argslist60011$yu470771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu47077, %struct.ScmObj* %argslist60011$yu470770)
store volatile %struct.ScmObj* %argslist60011$yu470771, %struct.ScmObj** %stackaddr$prim62323, align 8
%stackaddr$prim62324 = alloca %struct.ScmObj*, align 8
%argslist60011$yu470772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47612, %struct.ScmObj* %argslist60011$yu470771)
store volatile %struct.ScmObj* %argslist60011$yu470772, %struct.ScmObj** %stackaddr$prim62324, align 8
%clofunc62325 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu47077)
musttail call tailcc void %clofunc62325(%struct.ScmObj* %yu47077, %struct.ScmObj* %argslist60011$yu470772)
ret void
}