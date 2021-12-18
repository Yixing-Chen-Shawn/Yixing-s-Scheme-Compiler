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

@global$sym$ae4404454386 = private unnamed_addr constant [8 x i8] c"promise\00", align 8

define ccc i32 @main() {
%mainenv53265 = call %struct.ScmObj* @const_init_null()
%mainargs53266 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv53265, %struct.ScmObj* %mainargs53266)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv53263,%struct.ScmObj* %mainargs53264) {
%stackaddr$makeclosure53267 = alloca %struct.ScmObj*, align 8
%fptrToInt53268 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40656 to i64
%ae40656 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53268)
store volatile %struct.ScmObj* %ae40656, %struct.ScmObj** %stackaddr$makeclosure53267, align 8
%ae40657 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53269 = alloca %struct.ScmObj*, align 8
%fptrToInt53270 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40658 to i64
%ae40658 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53270)
store volatile %struct.ScmObj* %ae40658, %struct.ScmObj** %stackaddr$makeclosure53269, align 8
%args53262$ae40656$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53271 = alloca %struct.ScmObj*, align 8
%args53262$ae40656$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40658, %struct.ScmObj* %args53262$ae40656$0)
store volatile %struct.ScmObj* %args53262$ae40656$1, %struct.ScmObj** %stackaddr$prim53271, align 8
%stackaddr$prim53272 = alloca %struct.ScmObj*, align 8
%args53262$ae40656$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40657, %struct.ScmObj* %args53262$ae40656$1)
store volatile %struct.ScmObj* %args53262$ae40656$2, %struct.ScmObj** %stackaddr$prim53272, align 8
%clofunc53273 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40656)
musttail call tailcc void %clofunc53273(%struct.ScmObj* %ae40656, %struct.ScmObj* %args53262$ae40656$2)
ret void
}

define tailcc void @proc_clo$ae40656(%struct.ScmObj* %env$ae40656,%struct.ScmObj* %current_45args52511) {
%stackaddr$prim53274 = alloca %struct.ScmObj*, align 8
%_95k40439 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52511)
store volatile %struct.ScmObj* %_95k40439, %struct.ScmObj** %stackaddr$prim53274, align 8
%stackaddr$prim53275 = alloca %struct.ScmObj*, align 8
%current_45args52512 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52511)
store volatile %struct.ScmObj* %current_45args52512, %struct.ScmObj** %stackaddr$prim53275, align 8
%stackaddr$prim53276 = alloca %struct.ScmObj*, align 8
%anf_45bind40260 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52512)
store volatile %struct.ScmObj* %anf_45bind40260, %struct.ScmObj** %stackaddr$prim53276, align 8
%stackaddr$makeclosure53277 = alloca %struct.ScmObj*, align 8
%fptrToInt53278 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40671 to i64
%ae40671 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53278)
store volatile %struct.ScmObj* %ae40671, %struct.ScmObj** %stackaddr$makeclosure53277, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40671, %struct.ScmObj* %anf_45bind40260, i64 0)
%ae40672 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53279 = alloca %struct.ScmObj*, align 8
%fptrToInt53280 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40673 to i64
%ae40673 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53280)
store volatile %struct.ScmObj* %ae40673, %struct.ScmObj** %stackaddr$makeclosure53279, align 8
%args53257$ae40671$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53281 = alloca %struct.ScmObj*, align 8
%args53257$ae40671$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40673, %struct.ScmObj* %args53257$ae40671$0)
store volatile %struct.ScmObj* %args53257$ae40671$1, %struct.ScmObj** %stackaddr$prim53281, align 8
%stackaddr$prim53282 = alloca %struct.ScmObj*, align 8
%args53257$ae40671$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40672, %struct.ScmObj* %args53257$ae40671$1)
store volatile %struct.ScmObj* %args53257$ae40671$2, %struct.ScmObj** %stackaddr$prim53282, align 8
%clofunc53283 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40671)
musttail call tailcc void %clofunc53283(%struct.ScmObj* %ae40671, %struct.ScmObj* %args53257$ae40671$2)
ret void
}

define tailcc void @proc_clo$ae40671(%struct.ScmObj* %env$ae40671,%struct.ScmObj* %current_45args52514) {
%stackaddr$env-ref53284 = alloca %struct.ScmObj*, align 8
%anf_45bind40260 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40671, i64 0)
store %struct.ScmObj* %anf_45bind40260, %struct.ScmObj** %stackaddr$env-ref53284
%stackaddr$prim53285 = alloca %struct.ScmObj*, align 8
%_95k40440 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52514)
store volatile %struct.ScmObj* %_95k40440, %struct.ScmObj** %stackaddr$prim53285, align 8
%stackaddr$prim53286 = alloca %struct.ScmObj*, align 8
%current_45args52515 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52514)
store volatile %struct.ScmObj* %current_45args52515, %struct.ScmObj** %stackaddr$prim53286, align 8
%stackaddr$prim53287 = alloca %struct.ScmObj*, align 8
%anf_45bind40264 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52515)
store volatile %struct.ScmObj* %anf_45bind40264, %struct.ScmObj** %stackaddr$prim53287, align 8
%stackaddr$makeclosure53288 = alloca %struct.ScmObj*, align 8
%fptrToInt53289 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40786 to i64
%ae40786 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53289)
store volatile %struct.ScmObj* %ae40786, %struct.ScmObj** %stackaddr$makeclosure53288, align 8
%args53236$anf_45bind40260$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53290 = alloca %struct.ScmObj*, align 8
%args53236$anf_45bind40260$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40264, %struct.ScmObj* %args53236$anf_45bind40260$0)
store volatile %struct.ScmObj* %args53236$anf_45bind40260$1, %struct.ScmObj** %stackaddr$prim53290, align 8
%stackaddr$prim53291 = alloca %struct.ScmObj*, align 8
%args53236$anf_45bind40260$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40786, %struct.ScmObj* %args53236$anf_45bind40260$1)
store volatile %struct.ScmObj* %args53236$anf_45bind40260$2, %struct.ScmObj** %stackaddr$prim53291, align 8
%clofunc53292 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40260)
musttail call tailcc void %clofunc53292(%struct.ScmObj* %anf_45bind40260, %struct.ScmObj* %args53236$anf_45bind40260$2)
ret void
}

define tailcc void @proc_clo$ae40786(%struct.ScmObj* %env$ae40786,%struct.ScmObj* %current_45args52517) {
%stackaddr$prim53293 = alloca %struct.ScmObj*, align 8
%_95k40441 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52517)
store volatile %struct.ScmObj* %_95k40441, %struct.ScmObj** %stackaddr$prim53293, align 8
%stackaddr$prim53294 = alloca %struct.ScmObj*, align 8
%current_45args52518 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52517)
store volatile %struct.ScmObj* %current_45args52518, %struct.ScmObj** %stackaddr$prim53294, align 8
%stackaddr$prim53295 = alloca %struct.ScmObj*, align 8
%Ycmb40110 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52518)
store volatile %struct.ScmObj* %Ycmb40110, %struct.ScmObj** %stackaddr$prim53295, align 8
%stackaddr$makeclosure53296 = alloca %struct.ScmObj*, align 8
%fptrToInt53297 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40788 to i64
%ae40788 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53297)
store volatile %struct.ScmObj* %ae40788, %struct.ScmObj** %stackaddr$makeclosure53296, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40788, %struct.ScmObj* %Ycmb40110, i64 0)
%ae40789 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53298 = alloca %struct.ScmObj*, align 8
%fptrToInt53299 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40790 to i64
%ae40790 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53299)
store volatile %struct.ScmObj* %ae40790, %struct.ScmObj** %stackaddr$makeclosure53298, align 8
%args53235$ae40788$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53300 = alloca %struct.ScmObj*, align 8
%args53235$ae40788$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40790, %struct.ScmObj* %args53235$ae40788$0)
store volatile %struct.ScmObj* %args53235$ae40788$1, %struct.ScmObj** %stackaddr$prim53300, align 8
%stackaddr$prim53301 = alloca %struct.ScmObj*, align 8
%args53235$ae40788$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40789, %struct.ScmObj* %args53235$ae40788$1)
store volatile %struct.ScmObj* %args53235$ae40788$2, %struct.ScmObj** %stackaddr$prim53301, align 8
%clofunc53302 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40788)
musttail call tailcc void %clofunc53302(%struct.ScmObj* %ae40788, %struct.ScmObj* %args53235$ae40788$2)
ret void
}

define tailcc void @proc_clo$ae40788(%struct.ScmObj* %env$ae40788,%struct.ScmObj* %current_45args52520) {
%stackaddr$env-ref53303 = alloca %struct.ScmObj*, align 8
%Ycmb40110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40788, i64 0)
store %struct.ScmObj* %Ycmb40110, %struct.ScmObj** %stackaddr$env-ref53303
%stackaddr$prim53304 = alloca %struct.ScmObj*, align 8
%_95k40442 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52520)
store volatile %struct.ScmObj* %_95k40442, %struct.ScmObj** %stackaddr$prim53304, align 8
%stackaddr$prim53305 = alloca %struct.ScmObj*, align 8
%current_45args52521 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52520)
store volatile %struct.ScmObj* %current_45args52521, %struct.ScmObj** %stackaddr$prim53305, align 8
%stackaddr$prim53306 = alloca %struct.ScmObj*, align 8
%anf_45bind40269 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52521)
store volatile %struct.ScmObj* %anf_45bind40269, %struct.ScmObj** %stackaddr$prim53306, align 8
%stackaddr$makeclosure53307 = alloca %struct.ScmObj*, align 8
%fptrToInt53308 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40866 to i64
%ae40866 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53308)
store volatile %struct.ScmObj* %ae40866, %struct.ScmObj** %stackaddr$makeclosure53307, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40866, %struct.ScmObj* %Ycmb40110, i64 0)
%args53219$Ycmb40110$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53309 = alloca %struct.ScmObj*, align 8
%args53219$Ycmb40110$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40269, %struct.ScmObj* %args53219$Ycmb40110$0)
store volatile %struct.ScmObj* %args53219$Ycmb40110$1, %struct.ScmObj** %stackaddr$prim53309, align 8
%stackaddr$prim53310 = alloca %struct.ScmObj*, align 8
%args53219$Ycmb40110$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40866, %struct.ScmObj* %args53219$Ycmb40110$1)
store volatile %struct.ScmObj* %args53219$Ycmb40110$2, %struct.ScmObj** %stackaddr$prim53310, align 8
%clofunc53311 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40110)
musttail call tailcc void %clofunc53311(%struct.ScmObj* %Ycmb40110, %struct.ScmObj* %args53219$Ycmb40110$2)
ret void
}

define tailcc void @proc_clo$ae40866(%struct.ScmObj* %env$ae40866,%struct.ScmObj* %current_45args52523) {
%stackaddr$env-ref53312 = alloca %struct.ScmObj*, align 8
%Ycmb40110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40866, i64 0)
store %struct.ScmObj* %Ycmb40110, %struct.ScmObj** %stackaddr$env-ref53312
%stackaddr$prim53313 = alloca %struct.ScmObj*, align 8
%_95k40443 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52523)
store volatile %struct.ScmObj* %_95k40443, %struct.ScmObj** %stackaddr$prim53313, align 8
%stackaddr$prim53314 = alloca %struct.ScmObj*, align 8
%current_45args52524 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52523)
store volatile %struct.ScmObj* %current_45args52524, %struct.ScmObj** %stackaddr$prim53314, align 8
%stackaddr$prim53315 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52524)
store volatile %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$prim53315, align 8
%stackaddr$makeclosure53316 = alloca %struct.ScmObj*, align 8
%fptrToInt53317 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40868 to i64
%ae40868 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt53317)
store volatile %struct.ScmObj* %ae40868, %struct.ScmObj** %stackaddr$makeclosure53316, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40868, %struct.ScmObj* %_37foldr140131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40868, %struct.ScmObj* %Ycmb40110, i64 1)
%ae40869 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53318 = alloca %struct.ScmObj*, align 8
%fptrToInt53319 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40870 to i64
%ae40870 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53319)
store volatile %struct.ScmObj* %ae40870, %struct.ScmObj** %stackaddr$makeclosure53318, align 8
%args53218$ae40868$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53320 = alloca %struct.ScmObj*, align 8
%args53218$ae40868$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40870, %struct.ScmObj* %args53218$ae40868$0)
store volatile %struct.ScmObj* %args53218$ae40868$1, %struct.ScmObj** %stackaddr$prim53320, align 8
%stackaddr$prim53321 = alloca %struct.ScmObj*, align 8
%args53218$ae40868$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40869, %struct.ScmObj* %args53218$ae40868$1)
store volatile %struct.ScmObj* %args53218$ae40868$2, %struct.ScmObj** %stackaddr$prim53321, align 8
%clofunc53322 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40868)
musttail call tailcc void %clofunc53322(%struct.ScmObj* %ae40868, %struct.ScmObj* %args53218$ae40868$2)
ret void
}

define tailcc void @proc_clo$ae40868(%struct.ScmObj* %env$ae40868,%struct.ScmObj* %current_45args52526) {
%stackaddr$env-ref53323 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40868, i64 0)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref53323
%stackaddr$env-ref53324 = alloca %struct.ScmObj*, align 8
%Ycmb40110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40868, i64 1)
store %struct.ScmObj* %Ycmb40110, %struct.ScmObj** %stackaddr$env-ref53324
%stackaddr$prim53325 = alloca %struct.ScmObj*, align 8
%_95k40444 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52526)
store volatile %struct.ScmObj* %_95k40444, %struct.ScmObj** %stackaddr$prim53325, align 8
%stackaddr$prim53326 = alloca %struct.ScmObj*, align 8
%current_45args52527 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52526)
store volatile %struct.ScmObj* %current_45args52527, %struct.ScmObj** %stackaddr$prim53326, align 8
%stackaddr$prim53327 = alloca %struct.ScmObj*, align 8
%anf_45bind40275 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52527)
store volatile %struct.ScmObj* %anf_45bind40275, %struct.ScmObj** %stackaddr$prim53327, align 8
%stackaddr$makeclosure53328 = alloca %struct.ScmObj*, align 8
%fptrToInt53329 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40963 to i64
%ae40963 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt53329)
store volatile %struct.ScmObj* %ae40963, %struct.ScmObj** %stackaddr$makeclosure53328, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40963, %struct.ScmObj* %_37foldr140131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40963, %struct.ScmObj* %Ycmb40110, i64 1)
%args53199$Ycmb40110$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53330 = alloca %struct.ScmObj*, align 8
%args53199$Ycmb40110$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40275, %struct.ScmObj* %args53199$Ycmb40110$0)
store volatile %struct.ScmObj* %args53199$Ycmb40110$1, %struct.ScmObj** %stackaddr$prim53330, align 8
%stackaddr$prim53331 = alloca %struct.ScmObj*, align 8
%args53199$Ycmb40110$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40963, %struct.ScmObj* %args53199$Ycmb40110$1)
store volatile %struct.ScmObj* %args53199$Ycmb40110$2, %struct.ScmObj** %stackaddr$prim53331, align 8
%clofunc53332 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40110)
musttail call tailcc void %clofunc53332(%struct.ScmObj* %Ycmb40110, %struct.ScmObj* %args53199$Ycmb40110$2)
ret void
}

define tailcc void @proc_clo$ae40963(%struct.ScmObj* %env$ae40963,%struct.ScmObj* %current_45args52529) {
%stackaddr$env-ref53333 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40963, i64 0)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref53333
%stackaddr$env-ref53334 = alloca %struct.ScmObj*, align 8
%Ycmb40110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40963, i64 1)
store %struct.ScmObj* %Ycmb40110, %struct.ScmObj** %stackaddr$env-ref53334
%stackaddr$prim53335 = alloca %struct.ScmObj*, align 8
%_95k40445 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52529)
store volatile %struct.ScmObj* %_95k40445, %struct.ScmObj** %stackaddr$prim53335, align 8
%stackaddr$prim53336 = alloca %struct.ScmObj*, align 8
%current_45args52530 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52529)
store volatile %struct.ScmObj* %current_45args52530, %struct.ScmObj** %stackaddr$prim53336, align 8
%stackaddr$prim53337 = alloca %struct.ScmObj*, align 8
%_37map140127 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52530)
store volatile %struct.ScmObj* %_37map140127, %struct.ScmObj** %stackaddr$prim53337, align 8
%stackaddr$makeclosure53338 = alloca %struct.ScmObj*, align 8
%fptrToInt53339 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40965 to i64
%ae40965 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt53339)
store volatile %struct.ScmObj* %ae40965, %struct.ScmObj** %stackaddr$makeclosure53338, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40965, %struct.ScmObj* %_37foldr140131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40965, %struct.ScmObj* %_37map140127, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40965, %struct.ScmObj* %Ycmb40110, i64 2)
%ae40966 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53340 = alloca %struct.ScmObj*, align 8
%fptrToInt53341 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40967 to i64
%ae40967 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53341)
store volatile %struct.ScmObj* %ae40967, %struct.ScmObj** %stackaddr$makeclosure53340, align 8
%args53198$ae40965$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53342 = alloca %struct.ScmObj*, align 8
%args53198$ae40965$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40967, %struct.ScmObj* %args53198$ae40965$0)
store volatile %struct.ScmObj* %args53198$ae40965$1, %struct.ScmObj** %stackaddr$prim53342, align 8
%stackaddr$prim53343 = alloca %struct.ScmObj*, align 8
%args53198$ae40965$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40966, %struct.ScmObj* %args53198$ae40965$1)
store volatile %struct.ScmObj* %args53198$ae40965$2, %struct.ScmObj** %stackaddr$prim53343, align 8
%clofunc53344 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40965)
musttail call tailcc void %clofunc53344(%struct.ScmObj* %ae40965, %struct.ScmObj* %args53198$ae40965$2)
ret void
}

define tailcc void @proc_clo$ae40965(%struct.ScmObj* %env$ae40965,%struct.ScmObj* %current_45args52532) {
%stackaddr$env-ref53345 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40965, i64 0)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref53345
%stackaddr$env-ref53346 = alloca %struct.ScmObj*, align 8
%_37map140127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40965, i64 1)
store %struct.ScmObj* %_37map140127, %struct.ScmObj** %stackaddr$env-ref53346
%stackaddr$env-ref53347 = alloca %struct.ScmObj*, align 8
%Ycmb40110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40965, i64 2)
store %struct.ScmObj* %Ycmb40110, %struct.ScmObj** %stackaddr$env-ref53347
%stackaddr$prim53348 = alloca %struct.ScmObj*, align 8
%_95k40446 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52532)
store volatile %struct.ScmObj* %_95k40446, %struct.ScmObj** %stackaddr$prim53348, align 8
%stackaddr$prim53349 = alloca %struct.ScmObj*, align 8
%current_45args52533 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52532)
store volatile %struct.ScmObj* %current_45args52533, %struct.ScmObj** %stackaddr$prim53349, align 8
%stackaddr$prim53350 = alloca %struct.ScmObj*, align 8
%anf_45bind40282 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52533)
store volatile %struct.ScmObj* %anf_45bind40282, %struct.ScmObj** %stackaddr$prim53350, align 8
%stackaddr$makeclosure53351 = alloca %struct.ScmObj*, align 8
%fptrToInt53352 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41113 to i64
%ae41113 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt53352)
store volatile %struct.ScmObj* %ae41113, %struct.ScmObj** %stackaddr$makeclosure53351, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41113, %struct.ScmObj* %_37foldr140131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41113, %struct.ScmObj* %_37map140127, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41113, %struct.ScmObj* %Ycmb40110, i64 2)
%args53182$Ycmb40110$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53353 = alloca %struct.ScmObj*, align 8
%args53182$Ycmb40110$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40282, %struct.ScmObj* %args53182$Ycmb40110$0)
store volatile %struct.ScmObj* %args53182$Ycmb40110$1, %struct.ScmObj** %stackaddr$prim53353, align 8
%stackaddr$prim53354 = alloca %struct.ScmObj*, align 8
%args53182$Ycmb40110$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41113, %struct.ScmObj* %args53182$Ycmb40110$1)
store volatile %struct.ScmObj* %args53182$Ycmb40110$2, %struct.ScmObj** %stackaddr$prim53354, align 8
%clofunc53355 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40110)
musttail call tailcc void %clofunc53355(%struct.ScmObj* %Ycmb40110, %struct.ScmObj* %args53182$Ycmb40110$2)
ret void
}

define tailcc void @proc_clo$ae41113(%struct.ScmObj* %env$ae41113,%struct.ScmObj* %current_45args52535) {
%stackaddr$env-ref53356 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41113, i64 0)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref53356
%stackaddr$env-ref53357 = alloca %struct.ScmObj*, align 8
%_37map140127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41113, i64 1)
store %struct.ScmObj* %_37map140127, %struct.ScmObj** %stackaddr$env-ref53357
%stackaddr$env-ref53358 = alloca %struct.ScmObj*, align 8
%Ycmb40110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41113, i64 2)
store %struct.ScmObj* %Ycmb40110, %struct.ScmObj** %stackaddr$env-ref53358
%stackaddr$prim53359 = alloca %struct.ScmObj*, align 8
%_95k40447 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52535)
store volatile %struct.ScmObj* %_95k40447, %struct.ScmObj** %stackaddr$prim53359, align 8
%stackaddr$prim53360 = alloca %struct.ScmObj*, align 8
%current_45args52536 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52535)
store volatile %struct.ScmObj* %current_45args52536, %struct.ScmObj** %stackaddr$prim53360, align 8
%stackaddr$prim53361 = alloca %struct.ScmObj*, align 8
%_37take40123 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52536)
store volatile %struct.ScmObj* %_37take40123, %struct.ScmObj** %stackaddr$prim53361, align 8
%stackaddr$makeclosure53362 = alloca %struct.ScmObj*, align 8
%fptrToInt53363 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41115 to i64
%ae41115 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt53363)
store volatile %struct.ScmObj* %ae41115, %struct.ScmObj** %stackaddr$makeclosure53362, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41115, %struct.ScmObj* %_37foldr140131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41115, %struct.ScmObj* %_37map140127, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41115, %struct.ScmObj* %Ycmb40110, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41115, %struct.ScmObj* %_37take40123, i64 3)
%ae41116 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53364 = alloca %struct.ScmObj*, align 8
%fptrToInt53365 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41117 to i64
%ae41117 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53365)
store volatile %struct.ScmObj* %ae41117, %struct.ScmObj** %stackaddr$makeclosure53364, align 8
%args53181$ae41115$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53366 = alloca %struct.ScmObj*, align 8
%args53181$ae41115$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41117, %struct.ScmObj* %args53181$ae41115$0)
store volatile %struct.ScmObj* %args53181$ae41115$1, %struct.ScmObj** %stackaddr$prim53366, align 8
%stackaddr$prim53367 = alloca %struct.ScmObj*, align 8
%args53181$ae41115$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41116, %struct.ScmObj* %args53181$ae41115$1)
store volatile %struct.ScmObj* %args53181$ae41115$2, %struct.ScmObj** %stackaddr$prim53367, align 8
%clofunc53368 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41115)
musttail call tailcc void %clofunc53368(%struct.ScmObj* %ae41115, %struct.ScmObj* %args53181$ae41115$2)
ret void
}

define tailcc void @proc_clo$ae41115(%struct.ScmObj* %env$ae41115,%struct.ScmObj* %current_45args52538) {
%stackaddr$env-ref53369 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41115, i64 0)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref53369
%stackaddr$env-ref53370 = alloca %struct.ScmObj*, align 8
%_37map140127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41115, i64 1)
store %struct.ScmObj* %_37map140127, %struct.ScmObj** %stackaddr$env-ref53370
%stackaddr$env-ref53371 = alloca %struct.ScmObj*, align 8
%Ycmb40110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41115, i64 2)
store %struct.ScmObj* %Ycmb40110, %struct.ScmObj** %stackaddr$env-ref53371
%stackaddr$env-ref53372 = alloca %struct.ScmObj*, align 8
%_37take40123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41115, i64 3)
store %struct.ScmObj* %_37take40123, %struct.ScmObj** %stackaddr$env-ref53372
%stackaddr$prim53373 = alloca %struct.ScmObj*, align 8
%_95k40448 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52538)
store volatile %struct.ScmObj* %_95k40448, %struct.ScmObj** %stackaddr$prim53373, align 8
%stackaddr$prim53374 = alloca %struct.ScmObj*, align 8
%current_45args52539 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52538)
store volatile %struct.ScmObj* %current_45args52539, %struct.ScmObj** %stackaddr$prim53374, align 8
%stackaddr$prim53375 = alloca %struct.ScmObj*, align 8
%anf_45bind40286 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52539)
store volatile %struct.ScmObj* %anf_45bind40286, %struct.ScmObj** %stackaddr$prim53375, align 8
%stackaddr$makeclosure53376 = alloca %struct.ScmObj*, align 8
%fptrToInt53377 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41196 to i64
%ae41196 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt53377)
store volatile %struct.ScmObj* %ae41196, %struct.ScmObj** %stackaddr$makeclosure53376, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41196, %struct.ScmObj* %_37foldr140131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41196, %struct.ScmObj* %_37map140127, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41196, %struct.ScmObj* %Ycmb40110, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41196, %struct.ScmObj* %_37take40123, i64 3)
%args53167$Ycmb40110$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53378 = alloca %struct.ScmObj*, align 8
%args53167$Ycmb40110$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40286, %struct.ScmObj* %args53167$Ycmb40110$0)
store volatile %struct.ScmObj* %args53167$Ycmb40110$1, %struct.ScmObj** %stackaddr$prim53378, align 8
%stackaddr$prim53379 = alloca %struct.ScmObj*, align 8
%args53167$Ycmb40110$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41196, %struct.ScmObj* %args53167$Ycmb40110$1)
store volatile %struct.ScmObj* %args53167$Ycmb40110$2, %struct.ScmObj** %stackaddr$prim53379, align 8
%clofunc53380 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40110)
musttail call tailcc void %clofunc53380(%struct.ScmObj* %Ycmb40110, %struct.ScmObj* %args53167$Ycmb40110$2)
ret void
}

define tailcc void @proc_clo$ae41196(%struct.ScmObj* %env$ae41196,%struct.ScmObj* %current_45args52541) {
%stackaddr$env-ref53381 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41196, i64 0)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref53381
%stackaddr$env-ref53382 = alloca %struct.ScmObj*, align 8
%_37map140127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41196, i64 1)
store %struct.ScmObj* %_37map140127, %struct.ScmObj** %stackaddr$env-ref53382
%stackaddr$env-ref53383 = alloca %struct.ScmObj*, align 8
%Ycmb40110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41196, i64 2)
store %struct.ScmObj* %Ycmb40110, %struct.ScmObj** %stackaddr$env-ref53383
%stackaddr$env-ref53384 = alloca %struct.ScmObj*, align 8
%_37take40123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41196, i64 3)
store %struct.ScmObj* %_37take40123, %struct.ScmObj** %stackaddr$env-ref53384
%stackaddr$prim53385 = alloca %struct.ScmObj*, align 8
%_95k40449 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52541)
store volatile %struct.ScmObj* %_95k40449, %struct.ScmObj** %stackaddr$prim53385, align 8
%stackaddr$prim53386 = alloca %struct.ScmObj*, align 8
%current_45args52542 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52541)
store volatile %struct.ScmObj* %current_45args52542, %struct.ScmObj** %stackaddr$prim53386, align 8
%stackaddr$prim53387 = alloca %struct.ScmObj*, align 8
%_37length40120 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52542)
store volatile %struct.ScmObj* %_37length40120, %struct.ScmObj** %stackaddr$prim53387, align 8
%stackaddr$makeclosure53388 = alloca %struct.ScmObj*, align 8
%fptrToInt53389 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41198 to i64
%ae41198 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt53389)
store volatile %struct.ScmObj* %ae41198, %struct.ScmObj** %stackaddr$makeclosure53388, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41198, %struct.ScmObj* %_37length40120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41198, %struct.ScmObj* %_37foldr140131, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41198, %struct.ScmObj* %_37map140127, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41198, %struct.ScmObj* %Ycmb40110, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41198, %struct.ScmObj* %_37take40123, i64 4)
%ae41199 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53390 = alloca %struct.ScmObj*, align 8
%fptrToInt53391 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41200 to i64
%ae41200 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53391)
store volatile %struct.ScmObj* %ae41200, %struct.ScmObj** %stackaddr$makeclosure53390, align 8
%args53166$ae41198$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53392 = alloca %struct.ScmObj*, align 8
%args53166$ae41198$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41200, %struct.ScmObj* %args53166$ae41198$0)
store volatile %struct.ScmObj* %args53166$ae41198$1, %struct.ScmObj** %stackaddr$prim53392, align 8
%stackaddr$prim53393 = alloca %struct.ScmObj*, align 8
%args53166$ae41198$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41199, %struct.ScmObj* %args53166$ae41198$1)
store volatile %struct.ScmObj* %args53166$ae41198$2, %struct.ScmObj** %stackaddr$prim53393, align 8
%clofunc53394 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41198)
musttail call tailcc void %clofunc53394(%struct.ScmObj* %ae41198, %struct.ScmObj* %args53166$ae41198$2)
ret void
}

define tailcc void @proc_clo$ae41198(%struct.ScmObj* %env$ae41198,%struct.ScmObj* %current_45args52544) {
%stackaddr$env-ref53395 = alloca %struct.ScmObj*, align 8
%_37length40120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41198, i64 0)
store %struct.ScmObj* %_37length40120, %struct.ScmObj** %stackaddr$env-ref53395
%stackaddr$env-ref53396 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41198, i64 1)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref53396
%stackaddr$env-ref53397 = alloca %struct.ScmObj*, align 8
%_37map140127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41198, i64 2)
store %struct.ScmObj* %_37map140127, %struct.ScmObj** %stackaddr$env-ref53397
%stackaddr$env-ref53398 = alloca %struct.ScmObj*, align 8
%Ycmb40110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41198, i64 3)
store %struct.ScmObj* %Ycmb40110, %struct.ScmObj** %stackaddr$env-ref53398
%stackaddr$env-ref53399 = alloca %struct.ScmObj*, align 8
%_37take40123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41198, i64 4)
store %struct.ScmObj* %_37take40123, %struct.ScmObj** %stackaddr$env-ref53399
%stackaddr$prim53400 = alloca %struct.ScmObj*, align 8
%_95k40450 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52544)
store volatile %struct.ScmObj* %_95k40450, %struct.ScmObj** %stackaddr$prim53400, align 8
%stackaddr$prim53401 = alloca %struct.ScmObj*, align 8
%current_45args52545 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52544)
store volatile %struct.ScmObj* %current_45args52545, %struct.ScmObj** %stackaddr$prim53401, align 8
%stackaddr$prim53402 = alloca %struct.ScmObj*, align 8
%anf_45bind40291 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52545)
store volatile %struct.ScmObj* %anf_45bind40291, %struct.ScmObj** %stackaddr$prim53402, align 8
%stackaddr$makeclosure53403 = alloca %struct.ScmObj*, align 8
%fptrToInt53404 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41275 to i64
%ae41275 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt53404)
store volatile %struct.ScmObj* %ae41275, %struct.ScmObj** %stackaddr$makeclosure53403, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41275, %struct.ScmObj* %_37length40120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41275, %struct.ScmObj* %_37foldr140131, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41275, %struct.ScmObj* %_37map140127, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41275, %struct.ScmObj* %Ycmb40110, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41275, %struct.ScmObj* %_37take40123, i64 4)
%args53150$Ycmb40110$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53405 = alloca %struct.ScmObj*, align 8
%args53150$Ycmb40110$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40291, %struct.ScmObj* %args53150$Ycmb40110$0)
store volatile %struct.ScmObj* %args53150$Ycmb40110$1, %struct.ScmObj** %stackaddr$prim53405, align 8
%stackaddr$prim53406 = alloca %struct.ScmObj*, align 8
%args53150$Ycmb40110$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41275, %struct.ScmObj* %args53150$Ycmb40110$1)
store volatile %struct.ScmObj* %args53150$Ycmb40110$2, %struct.ScmObj** %stackaddr$prim53406, align 8
%clofunc53407 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40110)
musttail call tailcc void %clofunc53407(%struct.ScmObj* %Ycmb40110, %struct.ScmObj* %args53150$Ycmb40110$2)
ret void
}

define tailcc void @proc_clo$ae41275(%struct.ScmObj* %env$ae41275,%struct.ScmObj* %current_45args52547) {
%stackaddr$env-ref53408 = alloca %struct.ScmObj*, align 8
%_37length40120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41275, i64 0)
store %struct.ScmObj* %_37length40120, %struct.ScmObj** %stackaddr$env-ref53408
%stackaddr$env-ref53409 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41275, i64 1)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref53409
%stackaddr$env-ref53410 = alloca %struct.ScmObj*, align 8
%_37map140127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41275, i64 2)
store %struct.ScmObj* %_37map140127, %struct.ScmObj** %stackaddr$env-ref53410
%stackaddr$env-ref53411 = alloca %struct.ScmObj*, align 8
%Ycmb40110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41275, i64 3)
store %struct.ScmObj* %Ycmb40110, %struct.ScmObj** %stackaddr$env-ref53411
%stackaddr$env-ref53412 = alloca %struct.ScmObj*, align 8
%_37take40123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41275, i64 4)
store %struct.ScmObj* %_37take40123, %struct.ScmObj** %stackaddr$env-ref53412
%stackaddr$prim53413 = alloca %struct.ScmObj*, align 8
%_95k40451 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52547)
store volatile %struct.ScmObj* %_95k40451, %struct.ScmObj** %stackaddr$prim53413, align 8
%stackaddr$prim53414 = alloca %struct.ScmObj*, align 8
%current_45args52548 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52547)
store volatile %struct.ScmObj* %current_45args52548, %struct.ScmObj** %stackaddr$prim53414, align 8
%stackaddr$prim53415 = alloca %struct.ScmObj*, align 8
%_37foldl140115 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52548)
store volatile %struct.ScmObj* %_37foldl140115, %struct.ScmObj** %stackaddr$prim53415, align 8
%stackaddr$makeclosure53416 = alloca %struct.ScmObj*, align 8
%fptrToInt53417 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41277 to i64
%ae41277 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt53417)
store volatile %struct.ScmObj* %ae41277, %struct.ScmObj** %stackaddr$makeclosure53416, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41277, %struct.ScmObj* %_37foldr140131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41277, %struct.ScmObj* %_37foldl140115, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41277, %struct.ScmObj* %_37length40120, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41277, %struct.ScmObj* %_37map140127, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41277, %struct.ScmObj* %Ycmb40110, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41277, %struct.ScmObj* %_37take40123, i64 5)
%ae41278 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53418 = alloca %struct.ScmObj*, align 8
%fptrToInt53419 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41279 to i64
%ae41279 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53419)
store volatile %struct.ScmObj* %ae41279, %struct.ScmObj** %stackaddr$makeclosure53418, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41279, %struct.ScmObj* %_37foldl140115, i64 0)
%args53149$ae41277$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53420 = alloca %struct.ScmObj*, align 8
%args53149$ae41277$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41279, %struct.ScmObj* %args53149$ae41277$0)
store volatile %struct.ScmObj* %args53149$ae41277$1, %struct.ScmObj** %stackaddr$prim53420, align 8
%stackaddr$prim53421 = alloca %struct.ScmObj*, align 8
%args53149$ae41277$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41278, %struct.ScmObj* %args53149$ae41277$1)
store volatile %struct.ScmObj* %args53149$ae41277$2, %struct.ScmObj** %stackaddr$prim53421, align 8
%clofunc53422 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41277)
musttail call tailcc void %clofunc53422(%struct.ScmObj* %ae41277, %struct.ScmObj* %args53149$ae41277$2)
ret void
}

define tailcc void @proc_clo$ae41277(%struct.ScmObj* %env$ae41277,%struct.ScmObj* %current_45args52550) {
%stackaddr$env-ref53423 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41277, i64 0)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref53423
%stackaddr$env-ref53424 = alloca %struct.ScmObj*, align 8
%_37foldl140115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41277, i64 1)
store %struct.ScmObj* %_37foldl140115, %struct.ScmObj** %stackaddr$env-ref53424
%stackaddr$env-ref53425 = alloca %struct.ScmObj*, align 8
%_37length40120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41277, i64 2)
store %struct.ScmObj* %_37length40120, %struct.ScmObj** %stackaddr$env-ref53425
%stackaddr$env-ref53426 = alloca %struct.ScmObj*, align 8
%_37map140127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41277, i64 3)
store %struct.ScmObj* %_37map140127, %struct.ScmObj** %stackaddr$env-ref53426
%stackaddr$env-ref53427 = alloca %struct.ScmObj*, align 8
%Ycmb40110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41277, i64 4)
store %struct.ScmObj* %Ycmb40110, %struct.ScmObj** %stackaddr$env-ref53427
%stackaddr$env-ref53428 = alloca %struct.ScmObj*, align 8
%_37take40123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41277, i64 5)
store %struct.ScmObj* %_37take40123, %struct.ScmObj** %stackaddr$env-ref53428
%stackaddr$prim53429 = alloca %struct.ScmObj*, align 8
%_95k40452 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52550)
store volatile %struct.ScmObj* %_95k40452, %struct.ScmObj** %stackaddr$prim53429, align 8
%stackaddr$prim53430 = alloca %struct.ScmObj*, align 8
%current_45args52551 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52550)
store volatile %struct.ScmObj* %current_45args52551, %struct.ScmObj** %stackaddr$prim53430, align 8
%stackaddr$prim53431 = alloca %struct.ScmObj*, align 8
%_37last40153 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52551)
store volatile %struct.ScmObj* %_37last40153, %struct.ScmObj** %stackaddr$prim53431, align 8
%stackaddr$makeclosure53432 = alloca %struct.ScmObj*, align 8
%fptrToInt53433 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41331 to i64
%ae41331 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt53433)
store volatile %struct.ScmObj* %ae41331, %struct.ScmObj** %stackaddr$makeclosure53432, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41331, %struct.ScmObj* %_37foldr140131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41331, %struct.ScmObj* %_37foldl140115, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41331, %struct.ScmObj* %_37last40153, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41331, %struct.ScmObj* %_37map140127, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41331, %struct.ScmObj* %Ycmb40110, i64 4)
%ae41332 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53434 = alloca %struct.ScmObj*, align 8
%fptrToInt53435 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41333 to i64
%ae41333 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt53435)
store volatile %struct.ScmObj* %ae41333, %struct.ScmObj** %stackaddr$makeclosure53434, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41333, %struct.ScmObj* %_37length40120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41333, %struct.ScmObj* %_37take40123, i64 1)
%args53135$ae41331$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53436 = alloca %struct.ScmObj*, align 8
%args53135$ae41331$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41333, %struct.ScmObj* %args53135$ae41331$0)
store volatile %struct.ScmObj* %args53135$ae41331$1, %struct.ScmObj** %stackaddr$prim53436, align 8
%stackaddr$prim53437 = alloca %struct.ScmObj*, align 8
%args53135$ae41331$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41332, %struct.ScmObj* %args53135$ae41331$1)
store volatile %struct.ScmObj* %args53135$ae41331$2, %struct.ScmObj** %stackaddr$prim53437, align 8
%clofunc53438 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41331)
musttail call tailcc void %clofunc53438(%struct.ScmObj* %ae41331, %struct.ScmObj* %args53135$ae41331$2)
ret void
}

define tailcc void @proc_clo$ae41331(%struct.ScmObj* %env$ae41331,%struct.ScmObj* %current_45args52553) {
%stackaddr$env-ref53439 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41331, i64 0)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref53439
%stackaddr$env-ref53440 = alloca %struct.ScmObj*, align 8
%_37foldl140115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41331, i64 1)
store %struct.ScmObj* %_37foldl140115, %struct.ScmObj** %stackaddr$env-ref53440
%stackaddr$env-ref53441 = alloca %struct.ScmObj*, align 8
%_37last40153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41331, i64 2)
store %struct.ScmObj* %_37last40153, %struct.ScmObj** %stackaddr$env-ref53441
%stackaddr$env-ref53442 = alloca %struct.ScmObj*, align 8
%_37map140127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41331, i64 3)
store %struct.ScmObj* %_37map140127, %struct.ScmObj** %stackaddr$env-ref53442
%stackaddr$env-ref53443 = alloca %struct.ScmObj*, align 8
%Ycmb40110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41331, i64 4)
store %struct.ScmObj* %Ycmb40110, %struct.ScmObj** %stackaddr$env-ref53443
%stackaddr$prim53444 = alloca %struct.ScmObj*, align 8
%_95k40453 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52553)
store volatile %struct.ScmObj* %_95k40453, %struct.ScmObj** %stackaddr$prim53444, align 8
%stackaddr$prim53445 = alloca %struct.ScmObj*, align 8
%current_45args52554 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52553)
store volatile %struct.ScmObj* %current_45args52554, %struct.ScmObj** %stackaddr$prim53445, align 8
%stackaddr$prim53446 = alloca %struct.ScmObj*, align 8
%_37drop_45right40150 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52554)
store volatile %struct.ScmObj* %_37drop_45right40150, %struct.ScmObj** %stackaddr$prim53446, align 8
%stackaddr$makeclosure53447 = alloca %struct.ScmObj*, align 8
%fptrToInt53448 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41361 to i64
%ae41361 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt53448)
store volatile %struct.ScmObj* %ae41361, %struct.ScmObj** %stackaddr$makeclosure53447, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41361, %struct.ScmObj* %_37foldr140131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41361, %struct.ScmObj* %_37foldl140115, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41361, %struct.ScmObj* %_37last40153, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41361, %struct.ScmObj* %_37drop_45right40150, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41361, %struct.ScmObj* %Ycmb40110, i64 4)
%ae41362 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53449 = alloca %struct.ScmObj*, align 8
%fptrToInt53450 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41363 to i64
%ae41363 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt53450)
store volatile %struct.ScmObj* %ae41363, %struct.ScmObj** %stackaddr$makeclosure53449, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41363, %struct.ScmObj* %_37foldr140131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41363, %struct.ScmObj* %_37map140127, i64 1)
%args53125$ae41361$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53451 = alloca %struct.ScmObj*, align 8
%args53125$ae41361$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41363, %struct.ScmObj* %args53125$ae41361$0)
store volatile %struct.ScmObj* %args53125$ae41361$1, %struct.ScmObj** %stackaddr$prim53451, align 8
%stackaddr$prim53452 = alloca %struct.ScmObj*, align 8
%args53125$ae41361$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41362, %struct.ScmObj* %args53125$ae41361$1)
store volatile %struct.ScmObj* %args53125$ae41361$2, %struct.ScmObj** %stackaddr$prim53452, align 8
%clofunc53453 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41361)
musttail call tailcc void %clofunc53453(%struct.ScmObj* %ae41361, %struct.ScmObj* %args53125$ae41361$2)
ret void
}

define tailcc void @proc_clo$ae41361(%struct.ScmObj* %env$ae41361,%struct.ScmObj* %current_45args52556) {
%stackaddr$env-ref53454 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41361, i64 0)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref53454
%stackaddr$env-ref53455 = alloca %struct.ScmObj*, align 8
%_37foldl140115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41361, i64 1)
store %struct.ScmObj* %_37foldl140115, %struct.ScmObj** %stackaddr$env-ref53455
%stackaddr$env-ref53456 = alloca %struct.ScmObj*, align 8
%_37last40153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41361, i64 2)
store %struct.ScmObj* %_37last40153, %struct.ScmObj** %stackaddr$env-ref53456
%stackaddr$env-ref53457 = alloca %struct.ScmObj*, align 8
%_37drop_45right40150 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41361, i64 3)
store %struct.ScmObj* %_37drop_45right40150, %struct.ScmObj** %stackaddr$env-ref53457
%stackaddr$env-ref53458 = alloca %struct.ScmObj*, align 8
%Ycmb40110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41361, i64 4)
store %struct.ScmObj* %Ycmb40110, %struct.ScmObj** %stackaddr$env-ref53458
%stackaddr$prim53459 = alloca %struct.ScmObj*, align 8
%_95k40454 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52556)
store volatile %struct.ScmObj* %_95k40454, %struct.ScmObj** %stackaddr$prim53459, align 8
%stackaddr$prim53460 = alloca %struct.ScmObj*, align 8
%current_45args52557 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52556)
store volatile %struct.ScmObj* %current_45args52557, %struct.ScmObj** %stackaddr$prim53460, align 8
%stackaddr$prim53461 = alloca %struct.ScmObj*, align 8
%anf_45bind40307 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52557)
store volatile %struct.ScmObj* %anf_45bind40307, %struct.ScmObj** %stackaddr$prim53461, align 8
%stackaddr$makeclosure53462 = alloca %struct.ScmObj*, align 8
%fptrToInt53463 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41745 to i64
%ae41745 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt53463)
store volatile %struct.ScmObj* %ae41745, %struct.ScmObj** %stackaddr$makeclosure53462, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41745, %struct.ScmObj* %_37foldr140131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41745, %struct.ScmObj* %_37foldl140115, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41745, %struct.ScmObj* %_37last40153, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41745, %struct.ScmObj* %_37drop_45right40150, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41745, %struct.ScmObj* %Ycmb40110, i64 4)
%args53065$Ycmb40110$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53464 = alloca %struct.ScmObj*, align 8
%args53065$Ycmb40110$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40307, %struct.ScmObj* %args53065$Ycmb40110$0)
store volatile %struct.ScmObj* %args53065$Ycmb40110$1, %struct.ScmObj** %stackaddr$prim53464, align 8
%stackaddr$prim53465 = alloca %struct.ScmObj*, align 8
%args53065$Ycmb40110$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41745, %struct.ScmObj* %args53065$Ycmb40110$1)
store volatile %struct.ScmObj* %args53065$Ycmb40110$2, %struct.ScmObj** %stackaddr$prim53465, align 8
%clofunc53466 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40110)
musttail call tailcc void %clofunc53466(%struct.ScmObj* %Ycmb40110, %struct.ScmObj* %args53065$Ycmb40110$2)
ret void
}

define tailcc void @proc_clo$ae41745(%struct.ScmObj* %env$ae41745,%struct.ScmObj* %current_45args52559) {
%stackaddr$env-ref53467 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41745, i64 0)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref53467
%stackaddr$env-ref53468 = alloca %struct.ScmObj*, align 8
%_37foldl140115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41745, i64 1)
store %struct.ScmObj* %_37foldl140115, %struct.ScmObj** %stackaddr$env-ref53468
%stackaddr$env-ref53469 = alloca %struct.ScmObj*, align 8
%_37last40153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41745, i64 2)
store %struct.ScmObj* %_37last40153, %struct.ScmObj** %stackaddr$env-ref53469
%stackaddr$env-ref53470 = alloca %struct.ScmObj*, align 8
%_37drop_45right40150 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41745, i64 3)
store %struct.ScmObj* %_37drop_45right40150, %struct.ScmObj** %stackaddr$env-ref53470
%stackaddr$env-ref53471 = alloca %struct.ScmObj*, align 8
%Ycmb40110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41745, i64 4)
store %struct.ScmObj* %Ycmb40110, %struct.ScmObj** %stackaddr$env-ref53471
%stackaddr$prim53472 = alloca %struct.ScmObj*, align 8
%_95k40455 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52559)
store volatile %struct.ScmObj* %_95k40455, %struct.ScmObj** %stackaddr$prim53472, align 8
%stackaddr$prim53473 = alloca %struct.ScmObj*, align 8
%current_45args52560 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52559)
store volatile %struct.ScmObj* %current_45args52560, %struct.ScmObj** %stackaddr$prim53473, align 8
%stackaddr$prim53474 = alloca %struct.ScmObj*, align 8
%_37foldr40136 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52560)
store volatile %struct.ScmObj* %_37foldr40136, %struct.ScmObj** %stackaddr$prim53474, align 8
%stackaddr$makeclosure53475 = alloca %struct.ScmObj*, align 8
%fptrToInt53476 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41747 to i64
%ae41747 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt53476)
store volatile %struct.ScmObj* %ae41747, %struct.ScmObj** %stackaddr$makeclosure53475, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41747, %struct.ScmObj* %_37foldr140131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41747, %struct.ScmObj* %_37foldl140115, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41747, %struct.ScmObj* %_37last40153, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41747, %struct.ScmObj* %_37foldr40136, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41747, %struct.ScmObj* %_37drop_45right40150, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41747, %struct.ScmObj* %Ycmb40110, i64 5)
%ae41748 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53477 = alloca %struct.ScmObj*, align 8
%fptrToInt53478 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41749 to i64
%ae41749 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53478)
store volatile %struct.ScmObj* %ae41749, %struct.ScmObj** %stackaddr$makeclosure53477, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41749, %struct.ScmObj* %_37foldr140131, i64 0)
%args53064$ae41747$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53479 = alloca %struct.ScmObj*, align 8
%args53064$ae41747$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41749, %struct.ScmObj* %args53064$ae41747$0)
store volatile %struct.ScmObj* %args53064$ae41747$1, %struct.ScmObj** %stackaddr$prim53479, align 8
%stackaddr$prim53480 = alloca %struct.ScmObj*, align 8
%args53064$ae41747$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41748, %struct.ScmObj* %args53064$ae41747$1)
store volatile %struct.ScmObj* %args53064$ae41747$2, %struct.ScmObj** %stackaddr$prim53480, align 8
%clofunc53481 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41747)
musttail call tailcc void %clofunc53481(%struct.ScmObj* %ae41747, %struct.ScmObj* %args53064$ae41747$2)
ret void
}

define tailcc void @proc_clo$ae41747(%struct.ScmObj* %env$ae41747,%struct.ScmObj* %current_45args52562) {
%stackaddr$env-ref53482 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41747, i64 0)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref53482
%stackaddr$env-ref53483 = alloca %struct.ScmObj*, align 8
%_37foldl140115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41747, i64 1)
store %struct.ScmObj* %_37foldl140115, %struct.ScmObj** %stackaddr$env-ref53483
%stackaddr$env-ref53484 = alloca %struct.ScmObj*, align 8
%_37last40153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41747, i64 2)
store %struct.ScmObj* %_37last40153, %struct.ScmObj** %stackaddr$env-ref53484
%stackaddr$env-ref53485 = alloca %struct.ScmObj*, align 8
%_37foldr40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41747, i64 3)
store %struct.ScmObj* %_37foldr40136, %struct.ScmObj** %stackaddr$env-ref53485
%stackaddr$env-ref53486 = alloca %struct.ScmObj*, align 8
%_37drop_45right40150 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41747, i64 4)
store %struct.ScmObj* %_37drop_45right40150, %struct.ScmObj** %stackaddr$env-ref53486
%stackaddr$env-ref53487 = alloca %struct.ScmObj*, align 8
%Ycmb40110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41747, i64 5)
store %struct.ScmObj* %Ycmb40110, %struct.ScmObj** %stackaddr$env-ref53487
%stackaddr$prim53488 = alloca %struct.ScmObj*, align 8
%_95k40456 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52562)
store volatile %struct.ScmObj* %_95k40456, %struct.ScmObj** %stackaddr$prim53488, align 8
%stackaddr$prim53489 = alloca %struct.ScmObj*, align 8
%current_45args52563 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52562)
store volatile %struct.ScmObj* %current_45args52563, %struct.ScmObj** %stackaddr$prim53489, align 8
%stackaddr$prim53490 = alloca %struct.ScmObj*, align 8
%_37map140162 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52563)
store volatile %struct.ScmObj* %_37map140162, %struct.ScmObj** %stackaddr$prim53490, align 8
%stackaddr$makeclosure53491 = alloca %struct.ScmObj*, align 8
%fptrToInt53492 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41824 to i64
%ae41824 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt53492)
store volatile %struct.ScmObj* %ae41824, %struct.ScmObj** %stackaddr$makeclosure53491, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41824, %struct.ScmObj* %_37foldr140131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41824, %struct.ScmObj* %_37foldl140115, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41824, %struct.ScmObj* %_37foldr40136, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41824, %struct.ScmObj* %_37map140162, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41824, %struct.ScmObj* %Ycmb40110, i64 4)
%ae41825 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53493 = alloca %struct.ScmObj*, align 8
%fptrToInt53494 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41826 to i64
%ae41826 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt53494)
store volatile %struct.ScmObj* %ae41826, %struct.ScmObj** %stackaddr$makeclosure53493, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41826, %struct.ScmObj* %_37last40153, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41826, %struct.ScmObj* %_37foldr40136, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41826, %struct.ScmObj* %_37drop_45right40150, i64 2)
%args53045$ae41824$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53495 = alloca %struct.ScmObj*, align 8
%args53045$ae41824$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41826, %struct.ScmObj* %args53045$ae41824$0)
store volatile %struct.ScmObj* %args53045$ae41824$1, %struct.ScmObj** %stackaddr$prim53495, align 8
%stackaddr$prim53496 = alloca %struct.ScmObj*, align 8
%args53045$ae41824$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41825, %struct.ScmObj* %args53045$ae41824$1)
store volatile %struct.ScmObj* %args53045$ae41824$2, %struct.ScmObj** %stackaddr$prim53496, align 8
%clofunc53497 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41824)
musttail call tailcc void %clofunc53497(%struct.ScmObj* %ae41824, %struct.ScmObj* %args53045$ae41824$2)
ret void
}

define tailcc void @proc_clo$ae41824(%struct.ScmObj* %env$ae41824,%struct.ScmObj* %current_45args52565) {
%stackaddr$env-ref53498 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41824, i64 0)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref53498
%stackaddr$env-ref53499 = alloca %struct.ScmObj*, align 8
%_37foldl140115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41824, i64 1)
store %struct.ScmObj* %_37foldl140115, %struct.ScmObj** %stackaddr$env-ref53499
%stackaddr$env-ref53500 = alloca %struct.ScmObj*, align 8
%_37foldr40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41824, i64 2)
store %struct.ScmObj* %_37foldr40136, %struct.ScmObj** %stackaddr$env-ref53500
%stackaddr$env-ref53501 = alloca %struct.ScmObj*, align 8
%_37map140162 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41824, i64 3)
store %struct.ScmObj* %_37map140162, %struct.ScmObj** %stackaddr$env-ref53501
%stackaddr$env-ref53502 = alloca %struct.ScmObj*, align 8
%Ycmb40110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41824, i64 4)
store %struct.ScmObj* %Ycmb40110, %struct.ScmObj** %stackaddr$env-ref53502
%stackaddr$prim53503 = alloca %struct.ScmObj*, align 8
%_95k40457 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52565)
store volatile %struct.ScmObj* %_95k40457, %struct.ScmObj** %stackaddr$prim53503, align 8
%stackaddr$prim53504 = alloca %struct.ScmObj*, align 8
%current_45args52566 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52565)
store volatile %struct.ScmObj* %current_45args52566, %struct.ScmObj** %stackaddr$prim53504, align 8
%stackaddr$prim53505 = alloca %struct.ScmObj*, align 8
%_37map40157 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52566)
store volatile %struct.ScmObj* %_37map40157, %struct.ScmObj** %stackaddr$prim53505, align 8
%stackaddr$makeclosure53506 = alloca %struct.ScmObj*, align 8
%fptrToInt53507 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41970 to i64
%ae41970 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt53507)
store volatile %struct.ScmObj* %ae41970, %struct.ScmObj** %stackaddr$makeclosure53506, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41970, %struct.ScmObj* %_37foldl140115, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41970, %struct.ScmObj* %Ycmb40110, i64 1)
%ae41971 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53508 = alloca %struct.ScmObj*, align 8
%fptrToInt53509 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41972 to i64
%ae41972 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt53509)
store volatile %struct.ScmObj* %ae41972, %struct.ScmObj** %stackaddr$makeclosure53508, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41972, %struct.ScmObj* %_37foldr40136, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41972, %struct.ScmObj* %_37foldr140131, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41972, %struct.ScmObj* %_37map140162, i64 2)
%args53028$ae41970$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53510 = alloca %struct.ScmObj*, align 8
%args53028$ae41970$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41972, %struct.ScmObj* %args53028$ae41970$0)
store volatile %struct.ScmObj* %args53028$ae41970$1, %struct.ScmObj** %stackaddr$prim53510, align 8
%stackaddr$prim53511 = alloca %struct.ScmObj*, align 8
%args53028$ae41970$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41971, %struct.ScmObj* %args53028$ae41970$1)
store volatile %struct.ScmObj* %args53028$ae41970$2, %struct.ScmObj** %stackaddr$prim53511, align 8
%clofunc53512 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41970)
musttail call tailcc void %clofunc53512(%struct.ScmObj* %ae41970, %struct.ScmObj* %args53028$ae41970$2)
ret void
}

define tailcc void @proc_clo$ae41970(%struct.ScmObj* %env$ae41970,%struct.ScmObj* %current_45args52568) {
%stackaddr$env-ref53513 = alloca %struct.ScmObj*, align 8
%_37foldl140115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41970, i64 0)
store %struct.ScmObj* %_37foldl140115, %struct.ScmObj** %stackaddr$env-ref53513
%stackaddr$env-ref53514 = alloca %struct.ScmObj*, align 8
%Ycmb40110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41970, i64 1)
store %struct.ScmObj* %Ycmb40110, %struct.ScmObj** %stackaddr$env-ref53514
%stackaddr$prim53515 = alloca %struct.ScmObj*, align 8
%_95k40458 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52568)
store volatile %struct.ScmObj* %_95k40458, %struct.ScmObj** %stackaddr$prim53515, align 8
%stackaddr$prim53516 = alloca %struct.ScmObj*, align 8
%current_45args52569 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52568)
store volatile %struct.ScmObj* %current_45args52569, %struct.ScmObj** %stackaddr$prim53516, align 8
%stackaddr$prim53517 = alloca %struct.ScmObj*, align 8
%anf_45bind40327 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52569)
store volatile %struct.ScmObj* %anf_45bind40327, %struct.ScmObj** %stackaddr$prim53517, align 8
%stackaddr$makeclosure53518 = alloca %struct.ScmObj*, align 8
%fptrToInt53519 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42362 to i64
%ae42362 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53519)
store volatile %struct.ScmObj* %ae42362, %struct.ScmObj** %stackaddr$makeclosure53518, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42362, %struct.ScmObj* %_37foldl140115, i64 0)
%args52968$Ycmb40110$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53520 = alloca %struct.ScmObj*, align 8
%args52968$Ycmb40110$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40327, %struct.ScmObj* %args52968$Ycmb40110$0)
store volatile %struct.ScmObj* %args52968$Ycmb40110$1, %struct.ScmObj** %stackaddr$prim53520, align 8
%stackaddr$prim53521 = alloca %struct.ScmObj*, align 8
%args52968$Ycmb40110$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42362, %struct.ScmObj* %args52968$Ycmb40110$1)
store volatile %struct.ScmObj* %args52968$Ycmb40110$2, %struct.ScmObj** %stackaddr$prim53521, align 8
%clofunc53522 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40110)
musttail call tailcc void %clofunc53522(%struct.ScmObj* %Ycmb40110, %struct.ScmObj* %args52968$Ycmb40110$2)
ret void
}

define tailcc void @proc_clo$ae42362(%struct.ScmObj* %env$ae42362,%struct.ScmObj* %current_45args52571) {
%stackaddr$env-ref53523 = alloca %struct.ScmObj*, align 8
%_37foldl140115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42362, i64 0)
store %struct.ScmObj* %_37foldl140115, %struct.ScmObj** %stackaddr$env-ref53523
%stackaddr$prim53524 = alloca %struct.ScmObj*, align 8
%_95k40459 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52571)
store volatile %struct.ScmObj* %_95k40459, %struct.ScmObj** %stackaddr$prim53524, align 8
%stackaddr$prim53525 = alloca %struct.ScmObj*, align 8
%current_45args52572 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52571)
store volatile %struct.ScmObj* %current_45args52572, %struct.ScmObj** %stackaddr$prim53525, align 8
%stackaddr$prim53526 = alloca %struct.ScmObj*, align 8
%_37foldl40213 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52572)
store volatile %struct.ScmObj* %_37foldl40213, %struct.ScmObj** %stackaddr$prim53526, align 8
%stackaddr$makeclosure53527 = alloca %struct.ScmObj*, align 8
%fptrToInt53528 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42364 to i64
%ae42364 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53528)
store volatile %struct.ScmObj* %ae42364, %struct.ScmObj** %stackaddr$makeclosure53527, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42364, %struct.ScmObj* %_37foldl140115, i64 0)
%ae42365 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53529 = alloca %struct.ScmObj*, align 8
%fptrToInt53530 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42366 to i64
%ae42366 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53530)
store volatile %struct.ScmObj* %ae42366, %struct.ScmObj** %stackaddr$makeclosure53529, align 8
%args52967$ae42364$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53531 = alloca %struct.ScmObj*, align 8
%args52967$ae42364$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42366, %struct.ScmObj* %args52967$ae42364$0)
store volatile %struct.ScmObj* %args52967$ae42364$1, %struct.ScmObj** %stackaddr$prim53531, align 8
%stackaddr$prim53532 = alloca %struct.ScmObj*, align 8
%args52967$ae42364$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42365, %struct.ScmObj* %args52967$ae42364$1)
store volatile %struct.ScmObj* %args52967$ae42364$2, %struct.ScmObj** %stackaddr$prim53532, align 8
%clofunc53533 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42364)
musttail call tailcc void %clofunc53533(%struct.ScmObj* %ae42364, %struct.ScmObj* %args52967$ae42364$2)
ret void
}

define tailcc void @proc_clo$ae42364(%struct.ScmObj* %env$ae42364,%struct.ScmObj* %current_45args52574) {
%stackaddr$env-ref53534 = alloca %struct.ScmObj*, align 8
%_37foldl140115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42364, i64 0)
store %struct.ScmObj* %_37foldl140115, %struct.ScmObj** %stackaddr$env-ref53534
%stackaddr$prim53535 = alloca %struct.ScmObj*, align 8
%_95k40460 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52574)
store volatile %struct.ScmObj* %_95k40460, %struct.ScmObj** %stackaddr$prim53535, align 8
%stackaddr$prim53536 = alloca %struct.ScmObj*, align 8
%current_45args52575 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52574)
store volatile %struct.ScmObj* %current_45args52575, %struct.ScmObj** %stackaddr$prim53536, align 8
%stackaddr$prim53537 = alloca %struct.ScmObj*, align 8
%_37_6240210 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52575)
store volatile %struct.ScmObj* %_37_6240210, %struct.ScmObj** %stackaddr$prim53537, align 8
%stackaddr$makeclosure53538 = alloca %struct.ScmObj*, align 8
%fptrToInt53539 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42388 to i64
%ae42388 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53539)
store volatile %struct.ScmObj* %ae42388, %struct.ScmObj** %stackaddr$makeclosure53538, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42388, %struct.ScmObj* %_37foldl140115, i64 0)
%ae42389 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53540 = alloca %struct.ScmObj*, align 8
%fptrToInt53541 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42390 to i64
%ae42390 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53541)
store volatile %struct.ScmObj* %ae42390, %struct.ScmObj** %stackaddr$makeclosure53540, align 8
%args52961$ae42388$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53542 = alloca %struct.ScmObj*, align 8
%args52961$ae42388$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42390, %struct.ScmObj* %args52961$ae42388$0)
store volatile %struct.ScmObj* %args52961$ae42388$1, %struct.ScmObj** %stackaddr$prim53542, align 8
%stackaddr$prim53543 = alloca %struct.ScmObj*, align 8
%args52961$ae42388$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42389, %struct.ScmObj* %args52961$ae42388$1)
store volatile %struct.ScmObj* %args52961$ae42388$2, %struct.ScmObj** %stackaddr$prim53543, align 8
%clofunc53544 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42388)
musttail call tailcc void %clofunc53544(%struct.ScmObj* %ae42388, %struct.ScmObj* %args52961$ae42388$2)
ret void
}

define tailcc void @proc_clo$ae42388(%struct.ScmObj* %env$ae42388,%struct.ScmObj* %current_45args52577) {
%stackaddr$env-ref53545 = alloca %struct.ScmObj*, align 8
%_37foldl140115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42388, i64 0)
store %struct.ScmObj* %_37foldl140115, %struct.ScmObj** %stackaddr$env-ref53545
%stackaddr$prim53546 = alloca %struct.ScmObj*, align 8
%_95k40461 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52577)
store volatile %struct.ScmObj* %_95k40461, %struct.ScmObj** %stackaddr$prim53546, align 8
%stackaddr$prim53547 = alloca %struct.ScmObj*, align 8
%current_45args52578 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52577)
store volatile %struct.ScmObj* %current_45args52578, %struct.ScmObj** %stackaddr$prim53547, align 8
%stackaddr$prim53548 = alloca %struct.ScmObj*, align 8
%_37_62_6140207 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52578)
store volatile %struct.ScmObj* %_37_62_6140207, %struct.ScmObj** %stackaddr$prim53548, align 8
%ae42412 = call %struct.ScmObj* @const_init_int(i64 1)
%ae42413 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53549 = alloca %struct.ScmObj*, align 8
%_37append40203 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42412, %struct.ScmObj* %ae42413)
store volatile %struct.ScmObj* %_37append40203, %struct.ScmObj** %stackaddr$prim53549, align 8
%stackaddr$makeclosure53550 = alloca %struct.ScmObj*, align 8
%fptrToInt53551 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42414 to i64
%ae42414 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt53551)
store volatile %struct.ScmObj* %ae42414, %struct.ScmObj** %stackaddr$makeclosure53550, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42414, %struct.ScmObj* %_37foldl140115, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42414, %struct.ScmObj* %_37append40203, i64 1)
%ae42415 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53552 = alloca %struct.ScmObj*, align 8
%fptrToInt53553 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42416 to i64
%ae42416 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53553)
store volatile %struct.ScmObj* %ae42416, %struct.ScmObj** %stackaddr$makeclosure53552, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42416, %struct.ScmObj* %_37append40203, i64 0)
%args52955$ae42414$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53554 = alloca %struct.ScmObj*, align 8
%args52955$ae42414$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42416, %struct.ScmObj* %args52955$ae42414$0)
store volatile %struct.ScmObj* %args52955$ae42414$1, %struct.ScmObj** %stackaddr$prim53554, align 8
%stackaddr$prim53555 = alloca %struct.ScmObj*, align 8
%args52955$ae42414$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42415, %struct.ScmObj* %args52955$ae42414$1)
store volatile %struct.ScmObj* %args52955$ae42414$2, %struct.ScmObj** %stackaddr$prim53555, align 8
%clofunc53556 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42414)
musttail call tailcc void %clofunc53556(%struct.ScmObj* %ae42414, %struct.ScmObj* %args52955$ae42414$2)
ret void
}

define tailcc void @proc_clo$ae42414(%struct.ScmObj* %env$ae42414,%struct.ScmObj* %current_45args52580) {
%stackaddr$env-ref53557 = alloca %struct.ScmObj*, align 8
%_37foldl140115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42414, i64 0)
store %struct.ScmObj* %_37foldl140115, %struct.ScmObj** %stackaddr$env-ref53557
%stackaddr$env-ref53558 = alloca %struct.ScmObj*, align 8
%_37append40203 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42414, i64 1)
store %struct.ScmObj* %_37append40203, %struct.ScmObj** %stackaddr$env-ref53558
%stackaddr$prim53559 = alloca %struct.ScmObj*, align 8
%_95k40462 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52580)
store volatile %struct.ScmObj* %_95k40462, %struct.ScmObj** %stackaddr$prim53559, align 8
%stackaddr$prim53560 = alloca %struct.ScmObj*, align 8
%current_45args52581 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52580)
store volatile %struct.ScmObj* %current_45args52581, %struct.ScmObj** %stackaddr$prim53560, align 8
%stackaddr$prim53561 = alloca %struct.ScmObj*, align 8
%anf_45bind40335 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52581)
store volatile %struct.ScmObj* %anf_45bind40335, %struct.ScmObj** %stackaddr$prim53561, align 8
%ae42482 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim53562 = alloca %struct.ScmObj*, align 8
%_95040204 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append40203, %struct.ScmObj* %ae42482, %struct.ScmObj* %anf_45bind40335)
store volatile %struct.ScmObj* %_95040204, %struct.ScmObj** %stackaddr$prim53562, align 8
%ae42485 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim53563 = alloca %struct.ScmObj*, align 8
%_37append40202 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append40203, %struct.ScmObj* %ae42485)
store volatile %struct.ScmObj* %_37append40202, %struct.ScmObj** %stackaddr$prim53563, align 8
%stackaddr$makeclosure53564 = alloca %struct.ScmObj*, align 8
%fptrToInt53565 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42486 to i64
%ae42486 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53565)
store volatile %struct.ScmObj* %ae42486, %struct.ScmObj** %stackaddr$makeclosure53564, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42486, %struct.ScmObj* %_37foldl140115, i64 0)
%ae42487 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53566 = alloca %struct.ScmObj*, align 8
%fptrToInt53567 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42488 to i64
%ae42488 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53567)
store volatile %struct.ScmObj* %ae42488, %struct.ScmObj** %stackaddr$makeclosure53566, align 8
%args52944$ae42486$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53568 = alloca %struct.ScmObj*, align 8
%args52944$ae42486$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42488, %struct.ScmObj* %args52944$ae42486$0)
store volatile %struct.ScmObj* %args52944$ae42486$1, %struct.ScmObj** %stackaddr$prim53568, align 8
%stackaddr$prim53569 = alloca %struct.ScmObj*, align 8
%args52944$ae42486$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42487, %struct.ScmObj* %args52944$ae42486$1)
store volatile %struct.ScmObj* %args52944$ae42486$2, %struct.ScmObj** %stackaddr$prim53569, align 8
%clofunc53570 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42486)
musttail call tailcc void %clofunc53570(%struct.ScmObj* %ae42486, %struct.ScmObj* %args52944$ae42486$2)
ret void
}

define tailcc void @proc_clo$ae42486(%struct.ScmObj* %env$ae42486,%struct.ScmObj* %current_45args52583) {
%stackaddr$env-ref53571 = alloca %struct.ScmObj*, align 8
%_37foldl140115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42486, i64 0)
store %struct.ScmObj* %_37foldl140115, %struct.ScmObj** %stackaddr$env-ref53571
%stackaddr$prim53572 = alloca %struct.ScmObj*, align 8
%_95k40463 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52583)
store volatile %struct.ScmObj* %_95k40463, %struct.ScmObj** %stackaddr$prim53572, align 8
%stackaddr$prim53573 = alloca %struct.ScmObj*, align 8
%current_45args52584 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52583)
store volatile %struct.ScmObj* %current_45args52584, %struct.ScmObj** %stackaddr$prim53573, align 8
%stackaddr$prim53574 = alloca %struct.ScmObj*, align 8
%_37list_6340195 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52584)
store volatile %struct.ScmObj* %_37list_6340195, %struct.ScmObj** %stackaddr$prim53574, align 8
%stackaddr$makeclosure53575 = alloca %struct.ScmObj*, align 8
%fptrToInt53576 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42902 to i64
%ae42902 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53576)
store volatile %struct.ScmObj* %ae42902, %struct.ScmObj** %stackaddr$makeclosure53575, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42902, %struct.ScmObj* %_37foldl140115, i64 0)
%ae42903 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53577 = alloca %struct.ScmObj*, align 8
%fptrToInt53578 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42904 to i64
%ae42904 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53578)
store volatile %struct.ScmObj* %ae42904, %struct.ScmObj** %stackaddr$makeclosure53577, align 8
%args52919$ae42902$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53579 = alloca %struct.ScmObj*, align 8
%args52919$ae42902$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42904, %struct.ScmObj* %args52919$ae42902$0)
store volatile %struct.ScmObj* %args52919$ae42902$1, %struct.ScmObj** %stackaddr$prim53579, align 8
%stackaddr$prim53580 = alloca %struct.ScmObj*, align 8
%args52919$ae42902$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42903, %struct.ScmObj* %args52919$ae42902$1)
store volatile %struct.ScmObj* %args52919$ae42902$2, %struct.ScmObj** %stackaddr$prim53580, align 8
%clofunc53581 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42902)
musttail call tailcc void %clofunc53581(%struct.ScmObj* %ae42902, %struct.ScmObj* %args52919$ae42902$2)
ret void
}

define tailcc void @proc_clo$ae42902(%struct.ScmObj* %env$ae42902,%struct.ScmObj* %current_45args52586) {
%stackaddr$env-ref53582 = alloca %struct.ScmObj*, align 8
%_37foldl140115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42902, i64 0)
store %struct.ScmObj* %_37foldl140115, %struct.ScmObj** %stackaddr$env-ref53582
%stackaddr$prim53583 = alloca %struct.ScmObj*, align 8
%_95k40464 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52586)
store volatile %struct.ScmObj* %_95k40464, %struct.ScmObj** %stackaddr$prim53583, align 8
%stackaddr$prim53584 = alloca %struct.ScmObj*, align 8
%current_45args52587 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52586)
store volatile %struct.ScmObj* %current_45args52587, %struct.ScmObj** %stackaddr$prim53584, align 8
%stackaddr$prim53585 = alloca %struct.ScmObj*, align 8
%_37drop40186 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52587)
store volatile %struct.ScmObj* %_37drop40186, %struct.ScmObj** %stackaddr$prim53585, align 8
%stackaddr$makeclosure53586 = alloca %struct.ScmObj*, align 8
%fptrToInt53587 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43438 to i64
%ae43438 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53587)
store volatile %struct.ScmObj* %ae43438, %struct.ScmObj** %stackaddr$makeclosure53586, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43438, %struct.ScmObj* %_37foldl140115, i64 0)
%ae43439 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53588 = alloca %struct.ScmObj*, align 8
%fptrToInt53589 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43440 to i64
%ae43440 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53589)
store volatile %struct.ScmObj* %ae43440, %struct.ScmObj** %stackaddr$makeclosure53588, align 8
%args52895$ae43438$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53590 = alloca %struct.ScmObj*, align 8
%args52895$ae43438$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43440, %struct.ScmObj* %args52895$ae43438$0)
store volatile %struct.ScmObj* %args52895$ae43438$1, %struct.ScmObj** %stackaddr$prim53590, align 8
%stackaddr$prim53591 = alloca %struct.ScmObj*, align 8
%args52895$ae43438$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43439, %struct.ScmObj* %args52895$ae43438$1)
store volatile %struct.ScmObj* %args52895$ae43438$2, %struct.ScmObj** %stackaddr$prim53591, align 8
%clofunc53592 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43438)
musttail call tailcc void %clofunc53592(%struct.ScmObj* %ae43438, %struct.ScmObj* %args52895$ae43438$2)
ret void
}

define tailcc void @proc_clo$ae43438(%struct.ScmObj* %env$ae43438,%struct.ScmObj* %current_45args52589) {
%stackaddr$env-ref53593 = alloca %struct.ScmObj*, align 8
%_37foldl140115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43438, i64 0)
store %struct.ScmObj* %_37foldl140115, %struct.ScmObj** %stackaddr$env-ref53593
%stackaddr$prim53594 = alloca %struct.ScmObj*, align 8
%_95k40465 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52589)
store volatile %struct.ScmObj* %_95k40465, %struct.ScmObj** %stackaddr$prim53594, align 8
%stackaddr$prim53595 = alloca %struct.ScmObj*, align 8
%current_45args52590 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52589)
store volatile %struct.ScmObj* %current_45args52590, %struct.ScmObj** %stackaddr$prim53595, align 8
%stackaddr$prim53596 = alloca %struct.ScmObj*, align 8
%_37memv40179 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52590)
store volatile %struct.ScmObj* %_37memv40179, %struct.ScmObj** %stackaddr$prim53596, align 8
%stackaddr$makeclosure53597 = alloca %struct.ScmObj*, align 8
%fptrToInt53598 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43842 to i64
%ae43842 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53598)
store volatile %struct.ScmObj* %ae43842, %struct.ScmObj** %stackaddr$makeclosure53597, align 8
%ae43843 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53599 = alloca %struct.ScmObj*, align 8
%fptrToInt53600 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43844 to i64
%ae43844 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53600)
store volatile %struct.ScmObj* %ae43844, %struct.ScmObj** %stackaddr$makeclosure53599, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43844, %struct.ScmObj* %_37foldl140115, i64 0)
%args52869$ae43842$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53601 = alloca %struct.ScmObj*, align 8
%args52869$ae43842$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43844, %struct.ScmObj* %args52869$ae43842$0)
store volatile %struct.ScmObj* %args52869$ae43842$1, %struct.ScmObj** %stackaddr$prim53601, align 8
%stackaddr$prim53602 = alloca %struct.ScmObj*, align 8
%args52869$ae43842$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43843, %struct.ScmObj* %args52869$ae43842$1)
store volatile %struct.ScmObj* %args52869$ae43842$2, %struct.ScmObj** %stackaddr$prim53602, align 8
%clofunc53603 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43842)
musttail call tailcc void %clofunc53603(%struct.ScmObj* %ae43842, %struct.ScmObj* %args52869$ae43842$2)
ret void
}

define tailcc void @proc_clo$ae43842(%struct.ScmObj* %env$ae43842,%struct.ScmObj* %current_45args52592) {
%stackaddr$prim53604 = alloca %struct.ScmObj*, align 8
%_95k40466 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52592)
store volatile %struct.ScmObj* %_95k40466, %struct.ScmObj** %stackaddr$prim53604, align 8
%stackaddr$prim53605 = alloca %struct.ScmObj*, align 8
%current_45args52593 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52592)
store volatile %struct.ScmObj* %current_45args52593, %struct.ScmObj** %stackaddr$prim53605, align 8
%stackaddr$prim53606 = alloca %struct.ScmObj*, align 8
%_37_4740175 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52593)
store volatile %struct.ScmObj* %_37_4740175, %struct.ScmObj** %stackaddr$prim53606, align 8
%stackaddr$makeclosure53607 = alloca %struct.ScmObj*, align 8
%fptrToInt53608 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43940 to i64
%ae43940 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53608)
store volatile %struct.ScmObj* %ae43940, %struct.ScmObj** %stackaddr$makeclosure53607, align 8
%ae43941 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53609 = alloca %struct.ScmObj*, align 8
%fptrToInt53610 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43942 to i64
%ae43942 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53610)
store volatile %struct.ScmObj* %ae43942, %struct.ScmObj** %stackaddr$makeclosure53609, align 8
%args52856$ae43940$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53611 = alloca %struct.ScmObj*, align 8
%args52856$ae43940$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43942, %struct.ScmObj* %args52856$ae43940$0)
store volatile %struct.ScmObj* %args52856$ae43940$1, %struct.ScmObj** %stackaddr$prim53611, align 8
%stackaddr$prim53612 = alloca %struct.ScmObj*, align 8
%args52856$ae43940$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43941, %struct.ScmObj* %args52856$ae43940$1)
store volatile %struct.ScmObj* %args52856$ae43940$2, %struct.ScmObj** %stackaddr$prim53612, align 8
%clofunc53613 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43940)
musttail call tailcc void %clofunc53613(%struct.ScmObj* %ae43940, %struct.ScmObj* %args52856$ae43940$2)
ret void
}

define tailcc void @proc_clo$ae43940(%struct.ScmObj* %env$ae43940,%struct.ScmObj* %current_45args52595) {
%stackaddr$prim53614 = alloca %struct.ScmObj*, align 8
%_95k40467 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52595)
store volatile %struct.ScmObj* %_95k40467, %struct.ScmObj** %stackaddr$prim53614, align 8
%stackaddr$prim53615 = alloca %struct.ScmObj*, align 8
%current_45args52596 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52595)
store volatile %struct.ScmObj* %current_45args52596, %struct.ScmObj** %stackaddr$prim53615, align 8
%stackaddr$prim53616 = alloca %struct.ScmObj*, align 8
%_37first40173 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52596)
store volatile %struct.ScmObj* %_37first40173, %struct.ScmObj** %stackaddr$prim53616, align 8
%stackaddr$makeclosure53617 = alloca %struct.ScmObj*, align 8
%fptrToInt53618 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43960 to i64
%ae43960 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53618)
store volatile %struct.ScmObj* %ae43960, %struct.ScmObj** %stackaddr$makeclosure53617, align 8
%ae43961 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53619 = alloca %struct.ScmObj*, align 8
%fptrToInt53620 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43962 to i64
%ae43962 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53620)
store volatile %struct.ScmObj* %ae43962, %struct.ScmObj** %stackaddr$makeclosure53619, align 8
%args52851$ae43960$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53621 = alloca %struct.ScmObj*, align 8
%args52851$ae43960$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43962, %struct.ScmObj* %args52851$ae43960$0)
store volatile %struct.ScmObj* %args52851$ae43960$1, %struct.ScmObj** %stackaddr$prim53621, align 8
%stackaddr$prim53622 = alloca %struct.ScmObj*, align 8
%args52851$ae43960$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43961, %struct.ScmObj* %args52851$ae43960$1)
store volatile %struct.ScmObj* %args52851$ae43960$2, %struct.ScmObj** %stackaddr$prim53622, align 8
%clofunc53623 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43960)
musttail call tailcc void %clofunc53623(%struct.ScmObj* %ae43960, %struct.ScmObj* %args52851$ae43960$2)
ret void
}

define tailcc void @proc_clo$ae43960(%struct.ScmObj* %env$ae43960,%struct.ScmObj* %current_45args52598) {
%stackaddr$prim53624 = alloca %struct.ScmObj*, align 8
%_95k40468 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52598)
store volatile %struct.ScmObj* %_95k40468, %struct.ScmObj** %stackaddr$prim53624, align 8
%stackaddr$prim53625 = alloca %struct.ScmObj*, align 8
%current_45args52599 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52598)
store volatile %struct.ScmObj* %current_45args52599, %struct.ScmObj** %stackaddr$prim53625, align 8
%stackaddr$prim53626 = alloca %struct.ScmObj*, align 8
%_37second40171 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52599)
store volatile %struct.ScmObj* %_37second40171, %struct.ScmObj** %stackaddr$prim53626, align 8
%stackaddr$makeclosure53627 = alloca %struct.ScmObj*, align 8
%fptrToInt53628 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43982 to i64
%ae43982 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53628)
store volatile %struct.ScmObj* %ae43982, %struct.ScmObj** %stackaddr$makeclosure53627, align 8
%ae43983 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53629 = alloca %struct.ScmObj*, align 8
%fptrToInt53630 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43984 to i64
%ae43984 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53630)
store volatile %struct.ScmObj* %ae43984, %struct.ScmObj** %stackaddr$makeclosure53629, align 8
%args52846$ae43982$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53631 = alloca %struct.ScmObj*, align 8
%args52846$ae43982$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43984, %struct.ScmObj* %args52846$ae43982$0)
store volatile %struct.ScmObj* %args52846$ae43982$1, %struct.ScmObj** %stackaddr$prim53631, align 8
%stackaddr$prim53632 = alloca %struct.ScmObj*, align 8
%args52846$ae43982$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43983, %struct.ScmObj* %args52846$ae43982$1)
store volatile %struct.ScmObj* %args52846$ae43982$2, %struct.ScmObj** %stackaddr$prim53632, align 8
%clofunc53633 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43982)
musttail call tailcc void %clofunc53633(%struct.ScmObj* %ae43982, %struct.ScmObj* %args52846$ae43982$2)
ret void
}

define tailcc void @proc_clo$ae43982(%struct.ScmObj* %env$ae43982,%struct.ScmObj* %current_45args52601) {
%stackaddr$prim53634 = alloca %struct.ScmObj*, align 8
%_95k40469 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52601)
store volatile %struct.ScmObj* %_95k40469, %struct.ScmObj** %stackaddr$prim53634, align 8
%stackaddr$prim53635 = alloca %struct.ScmObj*, align 8
%current_45args52602 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52601)
store volatile %struct.ScmObj* %current_45args52602, %struct.ScmObj** %stackaddr$prim53635, align 8
%stackaddr$prim53636 = alloca %struct.ScmObj*, align 8
%_37third40169 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52602)
store volatile %struct.ScmObj* %_37third40169, %struct.ScmObj** %stackaddr$prim53636, align 8
%stackaddr$makeclosure53637 = alloca %struct.ScmObj*, align 8
%fptrToInt53638 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44006 to i64
%ae44006 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53638)
store volatile %struct.ScmObj* %ae44006, %struct.ScmObj** %stackaddr$makeclosure53637, align 8
%ae44007 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53639 = alloca %struct.ScmObj*, align 8
%fptrToInt53640 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44008 to i64
%ae44008 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53640)
store volatile %struct.ScmObj* %ae44008, %struct.ScmObj** %stackaddr$makeclosure53639, align 8
%args52841$ae44006$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53641 = alloca %struct.ScmObj*, align 8
%args52841$ae44006$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44008, %struct.ScmObj* %args52841$ae44006$0)
store volatile %struct.ScmObj* %args52841$ae44006$1, %struct.ScmObj** %stackaddr$prim53641, align 8
%stackaddr$prim53642 = alloca %struct.ScmObj*, align 8
%args52841$ae44006$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44007, %struct.ScmObj* %args52841$ae44006$1)
store volatile %struct.ScmObj* %args52841$ae44006$2, %struct.ScmObj** %stackaddr$prim53642, align 8
%clofunc53643 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44006)
musttail call tailcc void %clofunc53643(%struct.ScmObj* %ae44006, %struct.ScmObj* %args52841$ae44006$2)
ret void
}

define tailcc void @proc_clo$ae44006(%struct.ScmObj* %env$ae44006,%struct.ScmObj* %current_45args52604) {
%stackaddr$prim53644 = alloca %struct.ScmObj*, align 8
%_95k40470 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52604)
store volatile %struct.ScmObj* %_95k40470, %struct.ScmObj** %stackaddr$prim53644, align 8
%stackaddr$prim53645 = alloca %struct.ScmObj*, align 8
%current_45args52605 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52604)
store volatile %struct.ScmObj* %current_45args52605, %struct.ScmObj** %stackaddr$prim53645, align 8
%stackaddr$prim53646 = alloca %struct.ScmObj*, align 8
%_37fourth40167 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52605)
store volatile %struct.ScmObj* %_37fourth40167, %struct.ScmObj** %stackaddr$prim53646, align 8
%stackaddr$makeclosure53647 = alloca %struct.ScmObj*, align 8
%fptrToInt53648 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44032 to i64
%ae44032 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53648)
store volatile %struct.ScmObj* %ae44032, %struct.ScmObj** %stackaddr$makeclosure53647, align 8
%ae44033 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53649 = alloca %struct.ScmObj*, align 8
%fptrToInt53650 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44034 to i64
%ae44034 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53650)
store volatile %struct.ScmObj* %ae44034, %struct.ScmObj** %stackaddr$makeclosure53649, align 8
%args52836$ae44032$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53651 = alloca %struct.ScmObj*, align 8
%args52836$ae44032$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44034, %struct.ScmObj* %args52836$ae44032$0)
store volatile %struct.ScmObj* %args52836$ae44032$1, %struct.ScmObj** %stackaddr$prim53651, align 8
%stackaddr$prim53652 = alloca %struct.ScmObj*, align 8
%args52836$ae44032$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44033, %struct.ScmObj* %args52836$ae44032$1)
store volatile %struct.ScmObj* %args52836$ae44032$2, %struct.ScmObj** %stackaddr$prim53652, align 8
%clofunc53653 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44032)
musttail call tailcc void %clofunc53653(%struct.ScmObj* %ae44032, %struct.ScmObj* %args52836$ae44032$2)
ret void
}

define tailcc void @proc_clo$ae44032(%struct.ScmObj* %env$ae44032,%struct.ScmObj* %current_45args52607) {
%stackaddr$prim53654 = alloca %struct.ScmObj*, align 8
%_95k40471 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52607)
store volatile %struct.ScmObj* %_95k40471, %struct.ScmObj** %stackaddr$prim53654, align 8
%stackaddr$prim53655 = alloca %struct.ScmObj*, align 8
%current_45args52608 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52607)
store volatile %struct.ScmObj* %current_45args52608, %struct.ScmObj** %stackaddr$prim53655, align 8
%stackaddr$prim53656 = alloca %struct.ScmObj*, align 8
%promise_6340228 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52608)
store volatile %struct.ScmObj* %promise_6340228, %struct.ScmObj** %stackaddr$prim53656, align 8
%stackaddr$prim53657 = alloca %struct.ScmObj*, align 8
%anf_45bind40375 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind40375, %struct.ScmObj** %stackaddr$prim53657, align 8
%ae44119 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim53658 = alloca %struct.ScmObj*, align 8
%nat_45_62peano40236 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae44119, %struct.ScmObj* %anf_45bind40375)
store volatile %struct.ScmObj* %nat_45_62peano40236, %struct.ScmObj** %stackaddr$prim53658, align 8
%stackaddr$prim53659 = alloca %struct.ScmObj*, align 8
%anf_45bind40376 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind40376, %struct.ScmObj** %stackaddr$prim53659, align 8
%ae44121 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim53660 = alloca %struct.ScmObj*, align 8
%peano_45_62nat40235 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae44121, %struct.ScmObj* %anf_45bind40376)
store volatile %struct.ScmObj* %peano_45_62nat40235, %struct.ScmObj** %stackaddr$prim53660, align 8
%stackaddr$prim53661 = alloca %struct.ScmObj*, align 8
%anf_45bind40377 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind40377, %struct.ScmObj** %stackaddr$prim53661, align 8
%ae44123 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim53662 = alloca %struct.ScmObj*, align 8
%succ40234 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae44123, %struct.ScmObj* %anf_45bind40377)
store volatile %struct.ScmObj* %succ40234, %struct.ScmObj** %stackaddr$prim53662, align 8
%stackaddr$prim53663 = alloca %struct.ScmObj*, align 8
%anf_45bind40378 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind40378, %struct.ScmObj** %stackaddr$prim53663, align 8
%ae44125 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim53664 = alloca %struct.ScmObj*, align 8
%pred40233 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae44125, %struct.ScmObj* %anf_45bind40378)
store volatile %struct.ScmObj* %pred40233, %struct.ScmObj** %stackaddr$prim53664, align 8
%stackaddr$prim53665 = alloca %struct.ScmObj*, align 8
%anf_45bind40379 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind40379, %struct.ScmObj** %stackaddr$prim53665, align 8
%ae44127 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim53666 = alloca %struct.ScmObj*, align 8
%z_6340232 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae44127, %struct.ScmObj* %anf_45bind40379)
store volatile %struct.ScmObj* %z_6340232, %struct.ScmObj** %stackaddr$prim53666, align 8
%stackaddr$prim53667 = alloca %struct.ScmObj*, align 8
%anf_45bind40380 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind40380, %struct.ScmObj** %stackaddr$prim53667, align 8
%ae44129 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim53668 = alloca %struct.ScmObj*, align 8
%addc40231 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae44129, %struct.ScmObj* %anf_45bind40380)
store volatile %struct.ScmObj* %addc40231, %struct.ScmObj** %stackaddr$prim53668, align 8
%stackaddr$prim53669 = alloca %struct.ScmObj*, align 8
%anf_45bind40381 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind40381, %struct.ScmObj** %stackaddr$prim53669, align 8
%ae44131 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim53670 = alloca %struct.ScmObj*, align 8
%fibc40230 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae44131, %struct.ScmObj* %anf_45bind40381)
store volatile %struct.ScmObj* %fibc40230, %struct.ScmObj** %stackaddr$prim53670, align 8
%stackaddr$makeclosure53671 = alloca %struct.ScmObj*, align 8
%fptrToInt53672 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44133 to i64
%ae44133 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt53672)
store volatile %struct.ScmObj* %ae44133, %struct.ScmObj** %stackaddr$makeclosure53671, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44133, %struct.ScmObj* %pred40233, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44133, %struct.ScmObj* %z_6340232, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44133, %struct.ScmObj* %addc40231, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44133, %struct.ScmObj* %fibc40230, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44133, %struct.ScmObj* %nat_45_62peano40236, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44133, %struct.ScmObj* %peano_45_62nat40235, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae44133, %struct.ScmObj* %succ40234, i64 6)
%ae44134 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53673 = alloca %struct.ScmObj*, align 8
%fptrToInt53674 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44135 to i64
%ae44135 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53674)
store volatile %struct.ScmObj* %ae44135, %struct.ScmObj** %stackaddr$makeclosure53673, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44135, %struct.ScmObj* %nat_45_62peano40236, i64 0)
%args52829$ae44133$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53675 = alloca %struct.ScmObj*, align 8
%args52829$ae44133$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44135, %struct.ScmObj* %args52829$ae44133$0)
store volatile %struct.ScmObj* %args52829$ae44133$1, %struct.ScmObj** %stackaddr$prim53675, align 8
%stackaddr$prim53676 = alloca %struct.ScmObj*, align 8
%args52829$ae44133$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44134, %struct.ScmObj* %args52829$ae44133$1)
store volatile %struct.ScmObj* %args52829$ae44133$2, %struct.ScmObj** %stackaddr$prim53676, align 8
%clofunc53677 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44133)
musttail call tailcc void %clofunc53677(%struct.ScmObj* %ae44133, %struct.ScmObj* %args52829$ae44133$2)
ret void
}

define tailcc void @proc_clo$ae44133(%struct.ScmObj* %env$ae44133,%struct.ScmObj* %current_45args52610) {
%stackaddr$env-ref53678 = alloca %struct.ScmObj*, align 8
%pred40233 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44133, i64 0)
store %struct.ScmObj* %pred40233, %struct.ScmObj** %stackaddr$env-ref53678
%stackaddr$env-ref53679 = alloca %struct.ScmObj*, align 8
%z_6340232 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44133, i64 1)
store %struct.ScmObj* %z_6340232, %struct.ScmObj** %stackaddr$env-ref53679
%stackaddr$env-ref53680 = alloca %struct.ScmObj*, align 8
%addc40231 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44133, i64 2)
store %struct.ScmObj* %addc40231, %struct.ScmObj** %stackaddr$env-ref53680
%stackaddr$env-ref53681 = alloca %struct.ScmObj*, align 8
%fibc40230 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44133, i64 3)
store %struct.ScmObj* %fibc40230, %struct.ScmObj** %stackaddr$env-ref53681
%stackaddr$env-ref53682 = alloca %struct.ScmObj*, align 8
%nat_45_62peano40236 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44133, i64 4)
store %struct.ScmObj* %nat_45_62peano40236, %struct.ScmObj** %stackaddr$env-ref53682
%stackaddr$env-ref53683 = alloca %struct.ScmObj*, align 8
%peano_45_62nat40235 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44133, i64 5)
store %struct.ScmObj* %peano_45_62nat40235, %struct.ScmObj** %stackaddr$env-ref53683
%stackaddr$env-ref53684 = alloca %struct.ScmObj*, align 8
%succ40234 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44133, i64 6)
store %struct.ScmObj* %succ40234, %struct.ScmObj** %stackaddr$env-ref53684
%stackaddr$prim53685 = alloca %struct.ScmObj*, align 8
%_95k40472 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52610)
store volatile %struct.ScmObj* %_95k40472, %struct.ScmObj** %stackaddr$prim53685, align 8
%stackaddr$prim53686 = alloca %struct.ScmObj*, align 8
%current_45args52611 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52610)
store volatile %struct.ScmObj* %current_45args52611, %struct.ScmObj** %stackaddr$prim53686, align 8
%stackaddr$prim53687 = alloca %struct.ScmObj*, align 8
%anf_45bind40389 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52611)
store volatile %struct.ScmObj* %anf_45bind40389, %struct.ScmObj** %stackaddr$prim53687, align 8
%ae44342 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim53688 = alloca %struct.ScmObj*, align 8
%t4010940255 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %nat_45_62peano40236, %struct.ScmObj* %ae44342, %struct.ScmObj* %anf_45bind40389)
store volatile %struct.ScmObj* %t4010940255, %struct.ScmObj** %stackaddr$prim53688, align 8
%stackaddr$makeclosure53689 = alloca %struct.ScmObj*, align 8
%fptrToInt53690 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44344 to i64
%ae44344 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt53690)
store volatile %struct.ScmObj* %ae44344, %struct.ScmObj** %stackaddr$makeclosure53689, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44344, %struct.ScmObj* %pred40233, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44344, %struct.ScmObj* %z_6340232, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44344, %struct.ScmObj* %addc40231, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44344, %struct.ScmObj* %fibc40230, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44344, %struct.ScmObj* %nat_45_62peano40236, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44344, %struct.ScmObj* %peano_45_62nat40235, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae44344, %struct.ScmObj* %succ40234, i64 6)
%ae44345 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53691 = alloca %struct.ScmObj*, align 8
%fptrToInt53692 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44346 to i64
%ae44346 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt53692)
store volatile %struct.ScmObj* %ae44346, %struct.ScmObj** %stackaddr$makeclosure53691, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44346, %struct.ScmObj* %pred40233, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44346, %struct.ScmObj* %z_6340232, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44346, %struct.ScmObj* %peano_45_62nat40235, i64 2)
%args52805$ae44344$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53693 = alloca %struct.ScmObj*, align 8
%args52805$ae44344$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44346, %struct.ScmObj* %args52805$ae44344$0)
store volatile %struct.ScmObj* %args52805$ae44344$1, %struct.ScmObj** %stackaddr$prim53693, align 8
%stackaddr$prim53694 = alloca %struct.ScmObj*, align 8
%args52805$ae44344$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44345, %struct.ScmObj* %args52805$ae44344$1)
store volatile %struct.ScmObj* %args52805$ae44344$2, %struct.ScmObj** %stackaddr$prim53694, align 8
%clofunc53695 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44344)
musttail call tailcc void %clofunc53695(%struct.ScmObj* %ae44344, %struct.ScmObj* %args52805$ae44344$2)
ret void
}

define tailcc void @proc_clo$ae44344(%struct.ScmObj* %env$ae44344,%struct.ScmObj* %current_45args52613) {
%stackaddr$env-ref53696 = alloca %struct.ScmObj*, align 8
%pred40233 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44344, i64 0)
store %struct.ScmObj* %pred40233, %struct.ScmObj** %stackaddr$env-ref53696
%stackaddr$env-ref53697 = alloca %struct.ScmObj*, align 8
%z_6340232 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44344, i64 1)
store %struct.ScmObj* %z_6340232, %struct.ScmObj** %stackaddr$env-ref53697
%stackaddr$env-ref53698 = alloca %struct.ScmObj*, align 8
%addc40231 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44344, i64 2)
store %struct.ScmObj* %addc40231, %struct.ScmObj** %stackaddr$env-ref53698
%stackaddr$env-ref53699 = alloca %struct.ScmObj*, align 8
%fibc40230 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44344, i64 3)
store %struct.ScmObj* %fibc40230, %struct.ScmObj** %stackaddr$env-ref53699
%stackaddr$env-ref53700 = alloca %struct.ScmObj*, align 8
%nat_45_62peano40236 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44344, i64 4)
store %struct.ScmObj* %nat_45_62peano40236, %struct.ScmObj** %stackaddr$env-ref53700
%stackaddr$env-ref53701 = alloca %struct.ScmObj*, align 8
%peano_45_62nat40235 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44344, i64 5)
store %struct.ScmObj* %peano_45_62nat40235, %struct.ScmObj** %stackaddr$env-ref53701
%stackaddr$env-ref53702 = alloca %struct.ScmObj*, align 8
%succ40234 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44344, i64 6)
store %struct.ScmObj* %succ40234, %struct.ScmObj** %stackaddr$env-ref53702
%stackaddr$prim53703 = alloca %struct.ScmObj*, align 8
%_95k40473 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52613)
store volatile %struct.ScmObj* %_95k40473, %struct.ScmObj** %stackaddr$prim53703, align 8
%stackaddr$prim53704 = alloca %struct.ScmObj*, align 8
%current_45args52614 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52613)
store volatile %struct.ScmObj* %current_45args52614, %struct.ScmObj** %stackaddr$prim53704, align 8
%stackaddr$prim53705 = alloca %struct.ScmObj*, align 8
%anf_45bind40396 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52614)
store volatile %struct.ScmObj* %anf_45bind40396, %struct.ScmObj** %stackaddr$prim53705, align 8
%ae44477 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim53706 = alloca %struct.ScmObj*, align 8
%t4010840253 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %peano_45_62nat40235, %struct.ScmObj* %ae44477, %struct.ScmObj* %anf_45bind40396)
store volatile %struct.ScmObj* %t4010840253, %struct.ScmObj** %stackaddr$prim53706, align 8
%stackaddr$makeclosure53707 = alloca %struct.ScmObj*, align 8
%fptrToInt53708 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44479 to i64
%ae44479 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt53708)
store volatile %struct.ScmObj* %ae44479, %struct.ScmObj** %stackaddr$makeclosure53707, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44479, %struct.ScmObj* %pred40233, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44479, %struct.ScmObj* %z_6340232, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44479, %struct.ScmObj* %addc40231, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44479, %struct.ScmObj* %fibc40230, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44479, %struct.ScmObj* %nat_45_62peano40236, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44479, %struct.ScmObj* %peano_45_62nat40235, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae44479, %struct.ScmObj* %succ40234, i64 6)
%ae44480 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53709 = alloca %struct.ScmObj*, align 8
%fptrToInt53710 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44481 to i64
%ae44481 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53710)
store volatile %struct.ScmObj* %ae44481, %struct.ScmObj** %stackaddr$makeclosure53709, align 8
%args52787$ae44479$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53711 = alloca %struct.ScmObj*, align 8
%args52787$ae44479$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44481, %struct.ScmObj* %args52787$ae44479$0)
store volatile %struct.ScmObj* %args52787$ae44479$1, %struct.ScmObj** %stackaddr$prim53711, align 8
%stackaddr$prim53712 = alloca %struct.ScmObj*, align 8
%args52787$ae44479$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44480, %struct.ScmObj* %args52787$ae44479$1)
store volatile %struct.ScmObj* %args52787$ae44479$2, %struct.ScmObj** %stackaddr$prim53712, align 8
%clofunc53713 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44479)
musttail call tailcc void %clofunc53713(%struct.ScmObj* %ae44479, %struct.ScmObj* %args52787$ae44479$2)
ret void
}

define tailcc void @proc_clo$ae44479(%struct.ScmObj* %env$ae44479,%struct.ScmObj* %current_45args52616) {
%stackaddr$env-ref53714 = alloca %struct.ScmObj*, align 8
%pred40233 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44479, i64 0)
store %struct.ScmObj* %pred40233, %struct.ScmObj** %stackaddr$env-ref53714
%stackaddr$env-ref53715 = alloca %struct.ScmObj*, align 8
%z_6340232 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44479, i64 1)
store %struct.ScmObj* %z_6340232, %struct.ScmObj** %stackaddr$env-ref53715
%stackaddr$env-ref53716 = alloca %struct.ScmObj*, align 8
%addc40231 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44479, i64 2)
store %struct.ScmObj* %addc40231, %struct.ScmObj** %stackaddr$env-ref53716
%stackaddr$env-ref53717 = alloca %struct.ScmObj*, align 8
%fibc40230 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44479, i64 3)
store %struct.ScmObj* %fibc40230, %struct.ScmObj** %stackaddr$env-ref53717
%stackaddr$env-ref53718 = alloca %struct.ScmObj*, align 8
%nat_45_62peano40236 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44479, i64 4)
store %struct.ScmObj* %nat_45_62peano40236, %struct.ScmObj** %stackaddr$env-ref53718
%stackaddr$env-ref53719 = alloca %struct.ScmObj*, align 8
%peano_45_62nat40235 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44479, i64 5)
store %struct.ScmObj* %peano_45_62nat40235, %struct.ScmObj** %stackaddr$env-ref53719
%stackaddr$env-ref53720 = alloca %struct.ScmObj*, align 8
%succ40234 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44479, i64 6)
store %struct.ScmObj* %succ40234, %struct.ScmObj** %stackaddr$env-ref53720
%stackaddr$prim53721 = alloca %struct.ScmObj*, align 8
%_95k40474 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52616)
store volatile %struct.ScmObj* %_95k40474, %struct.ScmObj** %stackaddr$prim53721, align 8
%stackaddr$prim53722 = alloca %struct.ScmObj*, align 8
%current_45args52617 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52616)
store volatile %struct.ScmObj* %current_45args52617, %struct.ScmObj** %stackaddr$prim53722, align 8
%stackaddr$prim53723 = alloca %struct.ScmObj*, align 8
%anf_45bind40399 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52617)
store volatile %struct.ScmObj* %anf_45bind40399, %struct.ScmObj** %stackaddr$prim53723, align 8
%ae44546 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim53724 = alloca %struct.ScmObj*, align 8
%t4010740250 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %succ40234, %struct.ScmObj* %ae44546, %struct.ScmObj* %anf_45bind40399)
store volatile %struct.ScmObj* %t4010740250, %struct.ScmObj** %stackaddr$prim53724, align 8
%stackaddr$makeclosure53725 = alloca %struct.ScmObj*, align 8
%fptrToInt53726 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44548 to i64
%ae44548 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt53726)
store volatile %struct.ScmObj* %ae44548, %struct.ScmObj** %stackaddr$makeclosure53725, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44548, %struct.ScmObj* %pred40233, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44548, %struct.ScmObj* %z_6340232, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44548, %struct.ScmObj* %addc40231, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44548, %struct.ScmObj* %fibc40230, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44548, %struct.ScmObj* %nat_45_62peano40236, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44548, %struct.ScmObj* %peano_45_62nat40235, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae44548, %struct.ScmObj* %succ40234, i64 6)
%ae44549 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53727 = alloca %struct.ScmObj*, align 8
%fptrToInt53728 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44550 to i64
%ae44550 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53728)
store volatile %struct.ScmObj* %ae44550, %struct.ScmObj** %stackaddr$makeclosure53727, align 8
%args52773$ae44548$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53729 = alloca %struct.ScmObj*, align 8
%args52773$ae44548$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44550, %struct.ScmObj* %args52773$ae44548$0)
store volatile %struct.ScmObj* %args52773$ae44548$1, %struct.ScmObj** %stackaddr$prim53729, align 8
%stackaddr$prim53730 = alloca %struct.ScmObj*, align 8
%args52773$ae44548$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44549, %struct.ScmObj* %args52773$ae44548$1)
store volatile %struct.ScmObj* %args52773$ae44548$2, %struct.ScmObj** %stackaddr$prim53730, align 8
%clofunc53731 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44548)
musttail call tailcc void %clofunc53731(%struct.ScmObj* %ae44548, %struct.ScmObj* %args52773$ae44548$2)
ret void
}

define tailcc void @proc_clo$ae44548(%struct.ScmObj* %env$ae44548,%struct.ScmObj* %current_45args52619) {
%stackaddr$env-ref53732 = alloca %struct.ScmObj*, align 8
%pred40233 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44548, i64 0)
store %struct.ScmObj* %pred40233, %struct.ScmObj** %stackaddr$env-ref53732
%stackaddr$env-ref53733 = alloca %struct.ScmObj*, align 8
%z_6340232 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44548, i64 1)
store %struct.ScmObj* %z_6340232, %struct.ScmObj** %stackaddr$env-ref53733
%stackaddr$env-ref53734 = alloca %struct.ScmObj*, align 8
%addc40231 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44548, i64 2)
store %struct.ScmObj* %addc40231, %struct.ScmObj** %stackaddr$env-ref53734
%stackaddr$env-ref53735 = alloca %struct.ScmObj*, align 8
%fibc40230 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44548, i64 3)
store %struct.ScmObj* %fibc40230, %struct.ScmObj** %stackaddr$env-ref53735
%stackaddr$env-ref53736 = alloca %struct.ScmObj*, align 8
%nat_45_62peano40236 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44548, i64 4)
store %struct.ScmObj* %nat_45_62peano40236, %struct.ScmObj** %stackaddr$env-ref53736
%stackaddr$env-ref53737 = alloca %struct.ScmObj*, align 8
%peano_45_62nat40235 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44548, i64 5)
store %struct.ScmObj* %peano_45_62nat40235, %struct.ScmObj** %stackaddr$env-ref53737
%stackaddr$env-ref53738 = alloca %struct.ScmObj*, align 8
%succ40234 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44548, i64 6)
store %struct.ScmObj* %succ40234, %struct.ScmObj** %stackaddr$env-ref53738
%stackaddr$prim53739 = alloca %struct.ScmObj*, align 8
%_95k40475 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52619)
store volatile %struct.ScmObj* %_95k40475, %struct.ScmObj** %stackaddr$prim53739, align 8
%stackaddr$prim53740 = alloca %struct.ScmObj*, align 8
%current_45args52620 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52619)
store volatile %struct.ScmObj* %current_45args52620, %struct.ScmObj** %stackaddr$prim53740, align 8
%stackaddr$prim53741 = alloca %struct.ScmObj*, align 8
%anf_45bind40400 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52620)
store volatile %struct.ScmObj* %anf_45bind40400, %struct.ScmObj** %stackaddr$prim53741, align 8
%ae44569 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim53742 = alloca %struct.ScmObj*, align 8
%t4010540248 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %pred40233, %struct.ScmObj* %ae44569, %struct.ScmObj* %anf_45bind40400)
store volatile %struct.ScmObj* %t4010540248, %struct.ScmObj** %stackaddr$prim53742, align 8
%stackaddr$makeclosure53743 = alloca %struct.ScmObj*, align 8
%fptrToInt53744 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44571 to i64
%ae44571 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt53744)
store volatile %struct.ScmObj* %ae44571, %struct.ScmObj** %stackaddr$makeclosure53743, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44571, %struct.ScmObj* %pred40233, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44571, %struct.ScmObj* %z_6340232, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44571, %struct.ScmObj* %addc40231, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44571, %struct.ScmObj* %fibc40230, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44571, %struct.ScmObj* %nat_45_62peano40236, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44571, %struct.ScmObj* %peano_45_62nat40235, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae44571, %struct.ScmObj* %succ40234, i64 6)
%ae44572 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53745 = alloca %struct.ScmObj*, align 8
%fptrToInt53746 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44573 to i64
%ae44573 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53746)
store volatile %struct.ScmObj* %ae44573, %struct.ScmObj** %stackaddr$makeclosure53745, align 8
%args52768$ae44571$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53747 = alloca %struct.ScmObj*, align 8
%args52768$ae44571$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44573, %struct.ScmObj* %args52768$ae44571$0)
store volatile %struct.ScmObj* %args52768$ae44571$1, %struct.ScmObj** %stackaddr$prim53747, align 8
%stackaddr$prim53748 = alloca %struct.ScmObj*, align 8
%args52768$ae44571$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44572, %struct.ScmObj* %args52768$ae44571$1)
store volatile %struct.ScmObj* %args52768$ae44571$2, %struct.ScmObj** %stackaddr$prim53748, align 8
%clofunc53749 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44571)
musttail call tailcc void %clofunc53749(%struct.ScmObj* %ae44571, %struct.ScmObj* %args52768$ae44571$2)
ret void
}

define tailcc void @proc_clo$ae44571(%struct.ScmObj* %env$ae44571,%struct.ScmObj* %current_45args52622) {
%stackaddr$env-ref53750 = alloca %struct.ScmObj*, align 8
%pred40233 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44571, i64 0)
store %struct.ScmObj* %pred40233, %struct.ScmObj** %stackaddr$env-ref53750
%stackaddr$env-ref53751 = alloca %struct.ScmObj*, align 8
%z_6340232 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44571, i64 1)
store %struct.ScmObj* %z_6340232, %struct.ScmObj** %stackaddr$env-ref53751
%stackaddr$env-ref53752 = alloca %struct.ScmObj*, align 8
%addc40231 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44571, i64 2)
store %struct.ScmObj* %addc40231, %struct.ScmObj** %stackaddr$env-ref53752
%stackaddr$env-ref53753 = alloca %struct.ScmObj*, align 8
%fibc40230 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44571, i64 3)
store %struct.ScmObj* %fibc40230, %struct.ScmObj** %stackaddr$env-ref53753
%stackaddr$env-ref53754 = alloca %struct.ScmObj*, align 8
%nat_45_62peano40236 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44571, i64 4)
store %struct.ScmObj* %nat_45_62peano40236, %struct.ScmObj** %stackaddr$env-ref53754
%stackaddr$env-ref53755 = alloca %struct.ScmObj*, align 8
%peano_45_62nat40235 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44571, i64 5)
store %struct.ScmObj* %peano_45_62nat40235, %struct.ScmObj** %stackaddr$env-ref53755
%stackaddr$env-ref53756 = alloca %struct.ScmObj*, align 8
%succ40234 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44571, i64 6)
store %struct.ScmObj* %succ40234, %struct.ScmObj** %stackaddr$env-ref53756
%stackaddr$prim53757 = alloca %struct.ScmObj*, align 8
%_95k40476 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52622)
store volatile %struct.ScmObj* %_95k40476, %struct.ScmObj** %stackaddr$prim53757, align 8
%stackaddr$prim53758 = alloca %struct.ScmObj*, align 8
%current_45args52623 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52622)
store volatile %struct.ScmObj* %current_45args52623, %struct.ScmObj** %stackaddr$prim53758, align 8
%stackaddr$prim53759 = alloca %struct.ScmObj*, align 8
%anf_45bind40401 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52623)
store volatile %struct.ScmObj* %anf_45bind40401, %struct.ScmObj** %stackaddr$prim53759, align 8
%ae44592 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim53760 = alloca %struct.ScmObj*, align 8
%t4010440246 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %z_6340232, %struct.ScmObj* %ae44592, %struct.ScmObj* %anf_45bind40401)
store volatile %struct.ScmObj* %t4010440246, %struct.ScmObj** %stackaddr$prim53760, align 8
%stackaddr$makeclosure53761 = alloca %struct.ScmObj*, align 8
%fptrToInt53762 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44594 to i64
%ae44594 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt53762)
store volatile %struct.ScmObj* %ae44594, %struct.ScmObj** %stackaddr$makeclosure53761, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44594, %struct.ScmObj* %pred40233, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44594, %struct.ScmObj* %z_6340232, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44594, %struct.ScmObj* %addc40231, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44594, %struct.ScmObj* %fibc40230, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44594, %struct.ScmObj* %nat_45_62peano40236, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44594, %struct.ScmObj* %peano_45_62nat40235, i64 5)
%ae44595 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53763 = alloca %struct.ScmObj*, align 8
%fptrToInt53764 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44596 to i64
%ae44596 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt53764)
store volatile %struct.ScmObj* %ae44596, %struct.ScmObj** %stackaddr$makeclosure53763, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44596, %struct.ScmObj* %pred40233, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44596, %struct.ScmObj* %z_6340232, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44596, %struct.ScmObj* %addc40231, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44596, %struct.ScmObj* %succ40234, i64 3)
%args52763$ae44594$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53765 = alloca %struct.ScmObj*, align 8
%args52763$ae44594$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44596, %struct.ScmObj* %args52763$ae44594$0)
store volatile %struct.ScmObj* %args52763$ae44594$1, %struct.ScmObj** %stackaddr$prim53765, align 8
%stackaddr$prim53766 = alloca %struct.ScmObj*, align 8
%args52763$ae44594$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44595, %struct.ScmObj* %args52763$ae44594$1)
store volatile %struct.ScmObj* %args52763$ae44594$2, %struct.ScmObj** %stackaddr$prim53766, align 8
%clofunc53767 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44594)
musttail call tailcc void %clofunc53767(%struct.ScmObj* %ae44594, %struct.ScmObj* %args52763$ae44594$2)
ret void
}

define tailcc void @proc_clo$ae44594(%struct.ScmObj* %env$ae44594,%struct.ScmObj* %current_45args52625) {
%stackaddr$env-ref53768 = alloca %struct.ScmObj*, align 8
%pred40233 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44594, i64 0)
store %struct.ScmObj* %pred40233, %struct.ScmObj** %stackaddr$env-ref53768
%stackaddr$env-ref53769 = alloca %struct.ScmObj*, align 8
%z_6340232 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44594, i64 1)
store %struct.ScmObj* %z_6340232, %struct.ScmObj** %stackaddr$env-ref53769
%stackaddr$env-ref53770 = alloca %struct.ScmObj*, align 8
%addc40231 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44594, i64 2)
store %struct.ScmObj* %addc40231, %struct.ScmObj** %stackaddr$env-ref53770
%stackaddr$env-ref53771 = alloca %struct.ScmObj*, align 8
%fibc40230 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44594, i64 3)
store %struct.ScmObj* %fibc40230, %struct.ScmObj** %stackaddr$env-ref53771
%stackaddr$env-ref53772 = alloca %struct.ScmObj*, align 8
%nat_45_62peano40236 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44594, i64 4)
store %struct.ScmObj* %nat_45_62peano40236, %struct.ScmObj** %stackaddr$env-ref53772
%stackaddr$env-ref53773 = alloca %struct.ScmObj*, align 8
%peano_45_62nat40235 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44594, i64 5)
store %struct.ScmObj* %peano_45_62nat40235, %struct.ScmObj** %stackaddr$env-ref53773
%stackaddr$prim53774 = alloca %struct.ScmObj*, align 8
%_95k40477 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52625)
store volatile %struct.ScmObj* %_95k40477, %struct.ScmObj** %stackaddr$prim53774, align 8
%stackaddr$prim53775 = alloca %struct.ScmObj*, align 8
%current_45args52626 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52625)
store volatile %struct.ScmObj* %current_45args52626, %struct.ScmObj** %stackaddr$prim53775, align 8
%stackaddr$prim53776 = alloca %struct.ScmObj*, align 8
%anf_45bind40409 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52626)
store volatile %struct.ScmObj* %anf_45bind40409, %struct.ScmObj** %stackaddr$prim53776, align 8
%ae44733 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim53777 = alloca %struct.ScmObj*, align 8
%t4010340242 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %addc40231, %struct.ScmObj* %ae44733, %struct.ScmObj* %anf_45bind40409)
store volatile %struct.ScmObj* %t4010340242, %struct.ScmObj** %stackaddr$prim53777, align 8
%stackaddr$makeclosure53778 = alloca %struct.ScmObj*, align 8
%fptrToInt53779 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44735 to i64
%ae44735 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt53779)
store volatile %struct.ScmObj* %ae44735, %struct.ScmObj** %stackaddr$makeclosure53778, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44735, %struct.ScmObj* %fibc40230, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44735, %struct.ScmObj* %nat_45_62peano40236, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44735, %struct.ScmObj* %peano_45_62nat40235, i64 2)
%ae44736 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53780 = alloca %struct.ScmObj*, align 8
%fptrToInt53781 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44737 to i64
%ae44737 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt53781)
store volatile %struct.ScmObj* %ae44737, %struct.ScmObj** %stackaddr$makeclosure53780, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44737, %struct.ScmObj* %pred40233, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44737, %struct.ScmObj* %z_6340232, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44737, %struct.ScmObj* %addc40231, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44737, %struct.ScmObj* %fibc40230, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44737, %struct.ScmObj* %nat_45_62peano40236, i64 4)
%args52743$ae44735$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53782 = alloca %struct.ScmObj*, align 8
%args52743$ae44735$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44737, %struct.ScmObj* %args52743$ae44735$0)
store volatile %struct.ScmObj* %args52743$ae44735$1, %struct.ScmObj** %stackaddr$prim53782, align 8
%stackaddr$prim53783 = alloca %struct.ScmObj*, align 8
%args52743$ae44735$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44736, %struct.ScmObj* %args52743$ae44735$1)
store volatile %struct.ScmObj* %args52743$ae44735$2, %struct.ScmObj** %stackaddr$prim53783, align 8
%clofunc53784 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44735)
musttail call tailcc void %clofunc53784(%struct.ScmObj* %ae44735, %struct.ScmObj* %args52743$ae44735$2)
ret void
}

define tailcc void @proc_clo$ae44735(%struct.ScmObj* %env$ae44735,%struct.ScmObj* %current_45args52628) {
%stackaddr$env-ref53785 = alloca %struct.ScmObj*, align 8
%fibc40230 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44735, i64 0)
store %struct.ScmObj* %fibc40230, %struct.ScmObj** %stackaddr$env-ref53785
%stackaddr$env-ref53786 = alloca %struct.ScmObj*, align 8
%nat_45_62peano40236 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44735, i64 1)
store %struct.ScmObj* %nat_45_62peano40236, %struct.ScmObj** %stackaddr$env-ref53786
%stackaddr$env-ref53787 = alloca %struct.ScmObj*, align 8
%peano_45_62nat40235 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44735, i64 2)
store %struct.ScmObj* %peano_45_62nat40235, %struct.ScmObj** %stackaddr$env-ref53787
%stackaddr$prim53788 = alloca %struct.ScmObj*, align 8
%_95k40478 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52628)
store volatile %struct.ScmObj* %_95k40478, %struct.ScmObj** %stackaddr$prim53788, align 8
%stackaddr$prim53789 = alloca %struct.ScmObj*, align 8
%current_45args52629 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52628)
store volatile %struct.ScmObj* %current_45args52629, %struct.ScmObj** %stackaddr$prim53789, align 8
%stackaddr$prim53790 = alloca %struct.ScmObj*, align 8
%anf_45bind40433 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52629)
store volatile %struct.ScmObj* %anf_45bind40433, %struct.ScmObj** %stackaddr$prim53790, align 8
%ae45707 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim53791 = alloca %struct.ScmObj*, align 8
%t4010240237 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %fibc40230, %struct.ScmObj* %ae45707, %struct.ScmObj* %anf_45bind40433)
store volatile %struct.ScmObj* %t4010240237, %struct.ScmObj** %stackaddr$prim53791, align 8
%ae45710 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim53792 = alloca %struct.ScmObj*, align 8
%anf_45bind40434 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %fibc40230, %struct.ScmObj* %ae45710)
store volatile %struct.ScmObj* %anf_45bind40434, %struct.ScmObj** %stackaddr$prim53792, align 8
%ae45712 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim53793 = alloca %struct.ScmObj*, align 8
%anf_45bind40435 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %nat_45_62peano40236, %struct.ScmObj* %ae45712)
store volatile %struct.ScmObj* %anf_45bind40435, %struct.ScmObj** %stackaddr$prim53793, align 8
%stackaddr$makeclosure53794 = alloca %struct.ScmObj*, align 8
%fptrToInt53795 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae45714 to i64
%ae45714 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt53795)
store volatile %struct.ScmObj* %ae45714, %struct.ScmObj** %stackaddr$makeclosure53794, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae45714, %struct.ScmObj* %anf_45bind40434, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae45714, %struct.ScmObj* %peano_45_62nat40235, i64 1)
%ae45715 = call %struct.ScmObj* @const_init_int(i64 13)
%args52647$anf_45bind40435$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53796 = alloca %struct.ScmObj*, align 8
%args52647$anf_45bind40435$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45715, %struct.ScmObj* %args52647$anf_45bind40435$0)
store volatile %struct.ScmObj* %args52647$anf_45bind40435$1, %struct.ScmObj** %stackaddr$prim53796, align 8
%stackaddr$prim53797 = alloca %struct.ScmObj*, align 8
%args52647$anf_45bind40435$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45714, %struct.ScmObj* %args52647$anf_45bind40435$1)
store volatile %struct.ScmObj* %args52647$anf_45bind40435$2, %struct.ScmObj** %stackaddr$prim53797, align 8
%clofunc53798 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40435)
musttail call tailcc void %clofunc53798(%struct.ScmObj* %anf_45bind40435, %struct.ScmObj* %args52647$anf_45bind40435$2)
ret void
}

define tailcc void @proc_clo$ae45714(%struct.ScmObj* %env$ae45714,%struct.ScmObj* %current_45args52631) {
%stackaddr$env-ref53799 = alloca %struct.ScmObj*, align 8
%anf_45bind40434 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45714, i64 0)
store %struct.ScmObj* %anf_45bind40434, %struct.ScmObj** %stackaddr$env-ref53799
%stackaddr$env-ref53800 = alloca %struct.ScmObj*, align 8
%peano_45_62nat40235 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45714, i64 1)
store %struct.ScmObj* %peano_45_62nat40235, %struct.ScmObj** %stackaddr$env-ref53800
%stackaddr$prim53801 = alloca %struct.ScmObj*, align 8
%_95k40479 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52631)
store volatile %struct.ScmObj* %_95k40479, %struct.ScmObj** %stackaddr$prim53801, align 8
%stackaddr$prim53802 = alloca %struct.ScmObj*, align 8
%current_45args52632 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52631)
store volatile %struct.ScmObj* %current_45args52632, %struct.ScmObj** %stackaddr$prim53802, align 8
%stackaddr$prim53803 = alloca %struct.ScmObj*, align 8
%anf_45bind40436 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52632)
store volatile %struct.ScmObj* %anf_45bind40436, %struct.ScmObj** %stackaddr$prim53803, align 8
%stackaddr$makeclosure53804 = alloca %struct.ScmObj*, align 8
%fptrToInt53805 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae45719 to i64
%ae45719 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt53805)
store volatile %struct.ScmObj* %ae45719, %struct.ScmObj** %stackaddr$makeclosure53804, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae45719, %struct.ScmObj* %anf_45bind40436, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae45719, %struct.ScmObj* %anf_45bind40434, i64 1)
%ae45720 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53806 = alloca %struct.ScmObj*, align 8
%fptrToInt53807 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae45721 to i64
%ae45721 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53807)
store volatile %struct.ScmObj* %ae45721, %struct.ScmObj** %stackaddr$makeclosure53806, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae45721, %struct.ScmObj* %peano_45_62nat40235, i64 0)
%args52646$ae45719$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53808 = alloca %struct.ScmObj*, align 8
%args52646$ae45719$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45721, %struct.ScmObj* %args52646$ae45719$0)
store volatile %struct.ScmObj* %args52646$ae45719$1, %struct.ScmObj** %stackaddr$prim53808, align 8
%stackaddr$prim53809 = alloca %struct.ScmObj*, align 8
%args52646$ae45719$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45720, %struct.ScmObj* %args52646$ae45719$1)
store volatile %struct.ScmObj* %args52646$ae45719$2, %struct.ScmObj** %stackaddr$prim53809, align 8
%clofunc53810 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae45719)
musttail call tailcc void %clofunc53810(%struct.ScmObj* %ae45719, %struct.ScmObj* %args52646$ae45719$2)
ret void
}

define tailcc void @proc_clo$ae45719(%struct.ScmObj* %env$ae45719,%struct.ScmObj* %current_45args52634) {
%stackaddr$env-ref53811 = alloca %struct.ScmObj*, align 8
%anf_45bind40436 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45719, i64 0)
store %struct.ScmObj* %anf_45bind40436, %struct.ScmObj** %stackaddr$env-ref53811
%stackaddr$env-ref53812 = alloca %struct.ScmObj*, align 8
%anf_45bind40434 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45719, i64 1)
store %struct.ScmObj* %anf_45bind40434, %struct.ScmObj** %stackaddr$env-ref53812
%stackaddr$prim53813 = alloca %struct.ScmObj*, align 8
%_95k40480 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52634)
store volatile %struct.ScmObj* %_95k40480, %struct.ScmObj** %stackaddr$prim53813, align 8
%stackaddr$prim53814 = alloca %struct.ScmObj*, align 8
%current_45args52635 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52634)
store volatile %struct.ScmObj* %current_45args52635, %struct.ScmObj** %stackaddr$prim53814, align 8
%stackaddr$prim53815 = alloca %struct.ScmObj*, align 8
%anf_45bind40438 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52635)
store volatile %struct.ScmObj* %anf_45bind40438, %struct.ScmObj** %stackaddr$prim53815, align 8
%stackaddr$makeclosure53816 = alloca %struct.ScmObj*, align 8
%fptrToInt53817 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae45744 to i64
%ae45744 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53817)
store volatile %struct.ScmObj* %ae45744, %struct.ScmObj** %stackaddr$makeclosure53816, align 8
%args52641$anf_45bind40434$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53818 = alloca %struct.ScmObj*, align 8
%args52641$anf_45bind40434$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40438, %struct.ScmObj* %args52641$anf_45bind40434$0)
store volatile %struct.ScmObj* %args52641$anf_45bind40434$1, %struct.ScmObj** %stackaddr$prim53818, align 8
%stackaddr$prim53819 = alloca %struct.ScmObj*, align 8
%args52641$anf_45bind40434$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40436, %struct.ScmObj* %args52641$anf_45bind40434$1)
store volatile %struct.ScmObj* %args52641$anf_45bind40434$2, %struct.ScmObj** %stackaddr$prim53819, align 8
%stackaddr$prim53820 = alloca %struct.ScmObj*, align 8
%args52641$anf_45bind40434$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45744, %struct.ScmObj* %args52641$anf_45bind40434$2)
store volatile %struct.ScmObj* %args52641$anf_45bind40434$3, %struct.ScmObj** %stackaddr$prim53820, align 8
%clofunc53821 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40434)
musttail call tailcc void %clofunc53821(%struct.ScmObj* %anf_45bind40434, %struct.ScmObj* %args52641$anf_45bind40434$3)
ret void
}

define tailcc void @proc_clo$ae45744(%struct.ScmObj* %env$ae45744,%struct.ScmObj* %current_45args52637) {
%stackaddr$prim53822 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52637)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim53822, align 8
%stackaddr$prim53823 = alloca %struct.ScmObj*, align 8
%current_45args52638 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52637)
store volatile %struct.ScmObj* %current_45args52638, %struct.ScmObj** %stackaddr$prim53823, align 8
%stackaddr$prim53824 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52638)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim53824, align 8
%stackaddr$prim53825 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim53825, align 8
%args52640$k$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53826 = alloca %struct.ScmObj*, align 8
%args52640$k$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %args52640$k$0)
store volatile %struct.ScmObj* %args52640$k$1, %struct.ScmObj** %stackaddr$prim53826, align 8
%clofunc53827 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc53827(%struct.ScmObj* %k, %struct.ScmObj* %args52640$k$1)
ret void
}

define tailcc void @proc_clo$ae45721(%struct.ScmObj* %env$ae45721,%struct.ScmObj* %current_45args52642) {
%stackaddr$env-ref53828 = alloca %struct.ScmObj*, align 8
%peano_45_62nat40235 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45721, i64 0)
store %struct.ScmObj* %peano_45_62nat40235, %struct.ScmObj** %stackaddr$env-ref53828
%stackaddr$prim53829 = alloca %struct.ScmObj*, align 8
%k40481 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52642)
store volatile %struct.ScmObj* %k40481, %struct.ScmObj** %stackaddr$prim53829, align 8
%stackaddr$prim53830 = alloca %struct.ScmObj*, align 8
%current_45args52643 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52642)
store volatile %struct.ScmObj* %current_45args52643, %struct.ScmObj** %stackaddr$prim53830, align 8
%stackaddr$prim53831 = alloca %struct.ScmObj*, align 8
%x40259 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52643)
store volatile %struct.ScmObj* %x40259, %struct.ScmObj** %stackaddr$prim53831, align 8
%ae45723 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim53832 = alloca %struct.ScmObj*, align 8
%anf_45bind40437 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %peano_45_62nat40235, %struct.ScmObj* %ae45723)
store volatile %struct.ScmObj* %anf_45bind40437, %struct.ScmObj** %stackaddr$prim53832, align 8
%args52645$anf_45bind40437$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53833 = alloca %struct.ScmObj*, align 8
%args52645$anf_45bind40437$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x40259, %struct.ScmObj* %args52645$anf_45bind40437$0)
store volatile %struct.ScmObj* %args52645$anf_45bind40437$1, %struct.ScmObj** %stackaddr$prim53833, align 8
%stackaddr$prim53834 = alloca %struct.ScmObj*, align 8
%args52645$anf_45bind40437$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40481, %struct.ScmObj* %args52645$anf_45bind40437$1)
store volatile %struct.ScmObj* %args52645$anf_45bind40437$2, %struct.ScmObj** %stackaddr$prim53834, align 8
%clofunc53835 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40437)
musttail call tailcc void %clofunc53835(%struct.ScmObj* %anf_45bind40437, %struct.ScmObj* %args52645$anf_45bind40437$2)
ret void
}

define tailcc void @proc_clo$ae44737(%struct.ScmObj* %env$ae44737,%struct.ScmObj* %current_45args52648) {
%stackaddr$env-ref53836 = alloca %struct.ScmObj*, align 8
%pred40233 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44737, i64 0)
store %struct.ScmObj* %pred40233, %struct.ScmObj** %stackaddr$env-ref53836
%stackaddr$env-ref53837 = alloca %struct.ScmObj*, align 8
%z_6340232 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44737, i64 1)
store %struct.ScmObj* %z_6340232, %struct.ScmObj** %stackaddr$env-ref53837
%stackaddr$env-ref53838 = alloca %struct.ScmObj*, align 8
%addc40231 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44737, i64 2)
store %struct.ScmObj* %addc40231, %struct.ScmObj** %stackaddr$env-ref53838
%stackaddr$env-ref53839 = alloca %struct.ScmObj*, align 8
%fibc40230 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44737, i64 3)
store %struct.ScmObj* %fibc40230, %struct.ScmObj** %stackaddr$env-ref53839
%stackaddr$env-ref53840 = alloca %struct.ScmObj*, align 8
%nat_45_62peano40236 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44737, i64 4)
store %struct.ScmObj* %nat_45_62peano40236, %struct.ScmObj** %stackaddr$env-ref53840
%stackaddr$prim53841 = alloca %struct.ScmObj*, align 8
%k40482 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52648)
store volatile %struct.ScmObj* %k40482, %struct.ScmObj** %stackaddr$prim53841, align 8
%stackaddr$prim53842 = alloca %struct.ScmObj*, align 8
%current_45args52649 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52648)
store volatile %struct.ScmObj* %current_45args52649, %struct.ScmObj** %stackaddr$prim53842, align 8
%stackaddr$prim53843 = alloca %struct.ScmObj*, align 8
%x40239 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52649)
store volatile %struct.ScmObj* %x40239, %struct.ScmObj** %stackaddr$prim53843, align 8
%stackaddr$prim53844 = alloca %struct.ScmObj*, align 8
%current_45args52650 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52649)
store volatile %struct.ScmObj* %current_45args52650, %struct.ScmObj** %stackaddr$prim53844, align 8
%stackaddr$prim53845 = alloca %struct.ScmObj*, align 8
%c40238 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52650)
store volatile %struct.ScmObj* %c40238, %struct.ScmObj** %stackaddr$prim53845, align 8
%ae44739 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim53846 = alloca %struct.ScmObj*, align 8
%anf_45bind40410 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %z_6340232, %struct.ScmObj* %ae44739)
store volatile %struct.ScmObj* %anf_45bind40410, %struct.ScmObj** %stackaddr$prim53846, align 8
%stackaddr$makeclosure53847 = alloca %struct.ScmObj*, align 8
%fptrToInt53848 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44741 to i64
%ae44741 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt53848)
store volatile %struct.ScmObj* %ae44741, %struct.ScmObj** %stackaddr$makeclosure53847, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44741, %struct.ScmObj* %pred40233, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44741, %struct.ScmObj* %z_6340232, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44741, %struct.ScmObj* %addc40231, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44741, %struct.ScmObj* %fibc40230, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44741, %struct.ScmObj* %k40482, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44741, %struct.ScmObj* %x40239, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae44741, %struct.ScmObj* %c40238, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae44741, %struct.ScmObj* %nat_45_62peano40236, i64 7)
%args52742$anf_45bind40410$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53849 = alloca %struct.ScmObj*, align 8
%args52742$anf_45bind40410$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x40239, %struct.ScmObj* %args52742$anf_45bind40410$0)
store volatile %struct.ScmObj* %args52742$anf_45bind40410$1, %struct.ScmObj** %stackaddr$prim53849, align 8
%stackaddr$prim53850 = alloca %struct.ScmObj*, align 8
%args52742$anf_45bind40410$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44741, %struct.ScmObj* %args52742$anf_45bind40410$1)
store volatile %struct.ScmObj* %args52742$anf_45bind40410$2, %struct.ScmObj** %stackaddr$prim53850, align 8
%clofunc53851 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40410)
musttail call tailcc void %clofunc53851(%struct.ScmObj* %anf_45bind40410, %struct.ScmObj* %args52742$anf_45bind40410$2)
ret void
}

define tailcc void @proc_clo$ae44741(%struct.ScmObj* %env$ae44741,%struct.ScmObj* %current_45args52652) {
%stackaddr$env-ref53852 = alloca %struct.ScmObj*, align 8
%pred40233 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44741, i64 0)
store %struct.ScmObj* %pred40233, %struct.ScmObj** %stackaddr$env-ref53852
%stackaddr$env-ref53853 = alloca %struct.ScmObj*, align 8
%z_6340232 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44741, i64 1)
store %struct.ScmObj* %z_6340232, %struct.ScmObj** %stackaddr$env-ref53853
%stackaddr$env-ref53854 = alloca %struct.ScmObj*, align 8
%addc40231 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44741, i64 2)
store %struct.ScmObj* %addc40231, %struct.ScmObj** %stackaddr$env-ref53854
%stackaddr$env-ref53855 = alloca %struct.ScmObj*, align 8
%fibc40230 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44741, i64 3)
store %struct.ScmObj* %fibc40230, %struct.ScmObj** %stackaddr$env-ref53855
%stackaddr$env-ref53856 = alloca %struct.ScmObj*, align 8
%k40482 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44741, i64 4)
store %struct.ScmObj* %k40482, %struct.ScmObj** %stackaddr$env-ref53856
%stackaddr$env-ref53857 = alloca %struct.ScmObj*, align 8
%x40239 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44741, i64 5)
store %struct.ScmObj* %x40239, %struct.ScmObj** %stackaddr$env-ref53857
%stackaddr$env-ref53858 = alloca %struct.ScmObj*, align 8
%c40238 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44741, i64 6)
store %struct.ScmObj* %c40238, %struct.ScmObj** %stackaddr$env-ref53858
%stackaddr$env-ref53859 = alloca %struct.ScmObj*, align 8
%nat_45_62peano40236 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44741, i64 7)
store %struct.ScmObj* %nat_45_62peano40236, %struct.ScmObj** %stackaddr$env-ref53859
%stackaddr$prim53860 = alloca %struct.ScmObj*, align 8
%_95k40483 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52652)
store volatile %struct.ScmObj* %_95k40483, %struct.ScmObj** %stackaddr$prim53860, align 8
%stackaddr$prim53861 = alloca %struct.ScmObj*, align 8
%current_45args52653 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52652)
store volatile %struct.ScmObj* %current_45args52653, %struct.ScmObj** %stackaddr$prim53861, align 8
%stackaddr$prim53862 = alloca %struct.ScmObj*, align 8
%anf_45bind40411 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52653)
store volatile %struct.ScmObj* %anf_45bind40411, %struct.ScmObj** %stackaddr$prim53862, align 8
%truthy$cmp53863 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40411)
%cmp$cmp53863 = icmp eq i64 %truthy$cmp53863, 1
br i1 %cmp$cmp53863, label %truebranch$cmp53863, label %falsebranch$cmp53863
truebranch$cmp53863:
%ae44745 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim53864 = alloca %struct.ScmObj*, align 8
%anf_45bind40412 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %nat_45_62peano40236, %struct.ScmObj* %ae44745)
store volatile %struct.ScmObj* %anf_45bind40412, %struct.ScmObj** %stackaddr$prim53864, align 8
%stackaddr$makeclosure53865 = alloca %struct.ScmObj*, align 8
%fptrToInt53866 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44747 to i64
%ae44747 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt53866)
store volatile %struct.ScmObj* %ae44747, %struct.ScmObj** %stackaddr$makeclosure53865, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44747, %struct.ScmObj* %k40482, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44747, %struct.ScmObj* %c40238, i64 1)
%ae44748 = call %struct.ScmObj* @const_init_int(i64 0)
%args52659$anf_45bind40412$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53867 = alloca %struct.ScmObj*, align 8
%args52659$anf_45bind40412$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44748, %struct.ScmObj* %args52659$anf_45bind40412$0)
store volatile %struct.ScmObj* %args52659$anf_45bind40412$1, %struct.ScmObj** %stackaddr$prim53867, align 8
%stackaddr$prim53868 = alloca %struct.ScmObj*, align 8
%args52659$anf_45bind40412$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44747, %struct.ScmObj* %args52659$anf_45bind40412$1)
store volatile %struct.ScmObj* %args52659$anf_45bind40412$2, %struct.ScmObj** %stackaddr$prim53868, align 8
%clofunc53869 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40412)
musttail call tailcc void %clofunc53869(%struct.ScmObj* %anf_45bind40412, %struct.ScmObj* %args52659$anf_45bind40412$2)
ret void
falsebranch$cmp53863:
%ae44767 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim53870 = alloca %struct.ScmObj*, align 8
%anf_45bind40414 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %z_6340232, %struct.ScmObj* %ae44767)
store volatile %struct.ScmObj* %anf_45bind40414, %struct.ScmObj** %stackaddr$prim53870, align 8
%ae44769 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim53871 = alloca %struct.ScmObj*, align 8
%anf_45bind40415 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %pred40233, %struct.ScmObj* %ae44769)
store volatile %struct.ScmObj* %anf_45bind40415, %struct.ScmObj** %stackaddr$prim53871, align 8
%stackaddr$makeclosure53872 = alloca %struct.ScmObj*, align 8
%fptrToInt53873 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44771 to i64
%ae44771 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt53873)
store volatile %struct.ScmObj* %ae44771, %struct.ScmObj** %stackaddr$makeclosure53872, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44771, %struct.ScmObj* %c40238, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44771, %struct.ScmObj* %anf_45bind40414, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44771, %struct.ScmObj* %pred40233, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44771, %struct.ScmObj* %addc40231, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44771, %struct.ScmObj* %fibc40230, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44771, %struct.ScmObj* %k40482, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae44771, %struct.ScmObj* %x40239, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae44771, %struct.ScmObj* %nat_45_62peano40236, i64 7)
%args52741$anf_45bind40415$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53874 = alloca %struct.ScmObj*, align 8
%args52741$anf_45bind40415$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x40239, %struct.ScmObj* %args52741$anf_45bind40415$0)
store volatile %struct.ScmObj* %args52741$anf_45bind40415$1, %struct.ScmObj** %stackaddr$prim53874, align 8
%stackaddr$prim53875 = alloca %struct.ScmObj*, align 8
%args52741$anf_45bind40415$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44771, %struct.ScmObj* %args52741$anf_45bind40415$1)
store volatile %struct.ScmObj* %args52741$anf_45bind40415$2, %struct.ScmObj** %stackaddr$prim53875, align 8
%clofunc53876 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40415)
musttail call tailcc void %clofunc53876(%struct.ScmObj* %anf_45bind40415, %struct.ScmObj* %args52741$anf_45bind40415$2)
ret void
}

define tailcc void @proc_clo$ae44747(%struct.ScmObj* %env$ae44747,%struct.ScmObj* %current_45args52655) {
%stackaddr$env-ref53877 = alloca %struct.ScmObj*, align 8
%k40482 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44747, i64 0)
store %struct.ScmObj* %k40482, %struct.ScmObj** %stackaddr$env-ref53877
%stackaddr$env-ref53878 = alloca %struct.ScmObj*, align 8
%c40238 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44747, i64 1)
store %struct.ScmObj* %c40238, %struct.ScmObj** %stackaddr$env-ref53878
%stackaddr$prim53879 = alloca %struct.ScmObj*, align 8
%_95k40484 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52655)
store volatile %struct.ScmObj* %_95k40484, %struct.ScmObj** %stackaddr$prim53879, align 8
%stackaddr$prim53880 = alloca %struct.ScmObj*, align 8
%current_45args52656 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52655)
store volatile %struct.ScmObj* %current_45args52656, %struct.ScmObj** %stackaddr$prim53880, align 8
%stackaddr$prim53881 = alloca %struct.ScmObj*, align 8
%anf_45bind40413 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52656)
store volatile %struct.ScmObj* %anf_45bind40413, %struct.ScmObj** %stackaddr$prim53881, align 8
%args52658$c40238$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53882 = alloca %struct.ScmObj*, align 8
%args52658$c40238$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40413, %struct.ScmObj* %args52658$c40238$0)
store volatile %struct.ScmObj* %args52658$c40238$1, %struct.ScmObj** %stackaddr$prim53882, align 8
%stackaddr$prim53883 = alloca %struct.ScmObj*, align 8
%args52658$c40238$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40482, %struct.ScmObj* %args52658$c40238$1)
store volatile %struct.ScmObj* %args52658$c40238$2, %struct.ScmObj** %stackaddr$prim53883, align 8
%clofunc53884 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %c40238)
musttail call tailcc void %clofunc53884(%struct.ScmObj* %c40238, %struct.ScmObj* %args52658$c40238$2)
ret void
}

define tailcc void @proc_clo$ae44771(%struct.ScmObj* %env$ae44771,%struct.ScmObj* %current_45args52660) {
%stackaddr$env-ref53885 = alloca %struct.ScmObj*, align 8
%c40238 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44771, i64 0)
store %struct.ScmObj* %c40238, %struct.ScmObj** %stackaddr$env-ref53885
%stackaddr$env-ref53886 = alloca %struct.ScmObj*, align 8
%anf_45bind40414 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44771, i64 1)
store %struct.ScmObj* %anf_45bind40414, %struct.ScmObj** %stackaddr$env-ref53886
%stackaddr$env-ref53887 = alloca %struct.ScmObj*, align 8
%pred40233 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44771, i64 2)
store %struct.ScmObj* %pred40233, %struct.ScmObj** %stackaddr$env-ref53887
%stackaddr$env-ref53888 = alloca %struct.ScmObj*, align 8
%addc40231 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44771, i64 3)
store %struct.ScmObj* %addc40231, %struct.ScmObj** %stackaddr$env-ref53888
%stackaddr$env-ref53889 = alloca %struct.ScmObj*, align 8
%fibc40230 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44771, i64 4)
store %struct.ScmObj* %fibc40230, %struct.ScmObj** %stackaddr$env-ref53889
%stackaddr$env-ref53890 = alloca %struct.ScmObj*, align 8
%k40482 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44771, i64 5)
store %struct.ScmObj* %k40482, %struct.ScmObj** %stackaddr$env-ref53890
%stackaddr$env-ref53891 = alloca %struct.ScmObj*, align 8
%x40239 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44771, i64 6)
store %struct.ScmObj* %x40239, %struct.ScmObj** %stackaddr$env-ref53891
%stackaddr$env-ref53892 = alloca %struct.ScmObj*, align 8
%nat_45_62peano40236 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44771, i64 7)
store %struct.ScmObj* %nat_45_62peano40236, %struct.ScmObj** %stackaddr$env-ref53892
%stackaddr$prim53893 = alloca %struct.ScmObj*, align 8
%_95k40485 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52660)
store volatile %struct.ScmObj* %_95k40485, %struct.ScmObj** %stackaddr$prim53893, align 8
%stackaddr$prim53894 = alloca %struct.ScmObj*, align 8
%current_45args52661 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52660)
store volatile %struct.ScmObj* %current_45args52661, %struct.ScmObj** %stackaddr$prim53894, align 8
%stackaddr$prim53895 = alloca %struct.ScmObj*, align 8
%anf_45bind40416 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52661)
store volatile %struct.ScmObj* %anf_45bind40416, %struct.ScmObj** %stackaddr$prim53895, align 8
%stackaddr$makeclosure53896 = alloca %struct.ScmObj*, align 8
%fptrToInt53897 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44774 to i64
%ae44774 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt53897)
store volatile %struct.ScmObj* %ae44774, %struct.ScmObj** %stackaddr$makeclosure53896, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44774, %struct.ScmObj* %pred40233, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44774, %struct.ScmObj* %addc40231, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44774, %struct.ScmObj* %fibc40230, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44774, %struct.ScmObj* %k40482, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44774, %struct.ScmObj* %x40239, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44774, %struct.ScmObj* %c40238, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae44774, %struct.ScmObj* %nat_45_62peano40236, i64 6)
%args52740$anf_45bind40414$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53898 = alloca %struct.ScmObj*, align 8
%args52740$anf_45bind40414$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40416, %struct.ScmObj* %args52740$anf_45bind40414$0)
store volatile %struct.ScmObj* %args52740$anf_45bind40414$1, %struct.ScmObj** %stackaddr$prim53898, align 8
%stackaddr$prim53899 = alloca %struct.ScmObj*, align 8
%args52740$anf_45bind40414$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44774, %struct.ScmObj* %args52740$anf_45bind40414$1)
store volatile %struct.ScmObj* %args52740$anf_45bind40414$2, %struct.ScmObj** %stackaddr$prim53899, align 8
%clofunc53900 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40414)
musttail call tailcc void %clofunc53900(%struct.ScmObj* %anf_45bind40414, %struct.ScmObj* %args52740$anf_45bind40414$2)
ret void
}

define tailcc void @proc_clo$ae44774(%struct.ScmObj* %env$ae44774,%struct.ScmObj* %current_45args52663) {
%stackaddr$env-ref53901 = alloca %struct.ScmObj*, align 8
%pred40233 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44774, i64 0)
store %struct.ScmObj* %pred40233, %struct.ScmObj** %stackaddr$env-ref53901
%stackaddr$env-ref53902 = alloca %struct.ScmObj*, align 8
%addc40231 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44774, i64 1)
store %struct.ScmObj* %addc40231, %struct.ScmObj** %stackaddr$env-ref53902
%stackaddr$env-ref53903 = alloca %struct.ScmObj*, align 8
%fibc40230 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44774, i64 2)
store %struct.ScmObj* %fibc40230, %struct.ScmObj** %stackaddr$env-ref53903
%stackaddr$env-ref53904 = alloca %struct.ScmObj*, align 8
%k40482 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44774, i64 3)
store %struct.ScmObj* %k40482, %struct.ScmObj** %stackaddr$env-ref53904
%stackaddr$env-ref53905 = alloca %struct.ScmObj*, align 8
%x40239 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44774, i64 4)
store %struct.ScmObj* %x40239, %struct.ScmObj** %stackaddr$env-ref53905
%stackaddr$env-ref53906 = alloca %struct.ScmObj*, align 8
%c40238 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44774, i64 5)
store %struct.ScmObj* %c40238, %struct.ScmObj** %stackaddr$env-ref53906
%stackaddr$env-ref53907 = alloca %struct.ScmObj*, align 8
%nat_45_62peano40236 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44774, i64 6)
store %struct.ScmObj* %nat_45_62peano40236, %struct.ScmObj** %stackaddr$env-ref53907
%stackaddr$prim53908 = alloca %struct.ScmObj*, align 8
%_95k40486 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52663)
store volatile %struct.ScmObj* %_95k40486, %struct.ScmObj** %stackaddr$prim53908, align 8
%stackaddr$prim53909 = alloca %struct.ScmObj*, align 8
%current_45args52664 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52663)
store volatile %struct.ScmObj* %current_45args52664, %struct.ScmObj** %stackaddr$prim53909, align 8
%stackaddr$prim53910 = alloca %struct.ScmObj*, align 8
%anf_45bind40417 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52664)
store volatile %struct.ScmObj* %anf_45bind40417, %struct.ScmObj** %stackaddr$prim53910, align 8
%truthy$cmp53911 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40417)
%cmp$cmp53911 = icmp eq i64 %truthy$cmp53911, 1
br i1 %cmp$cmp53911, label %truebranch$cmp53911, label %falsebranch$cmp53911
truebranch$cmp53911:
%ae44778 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim53912 = alloca %struct.ScmObj*, align 8
%anf_45bind40418 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %nat_45_62peano40236, %struct.ScmObj* %ae44778)
store volatile %struct.ScmObj* %anf_45bind40418, %struct.ScmObj** %stackaddr$prim53912, align 8
%stackaddr$makeclosure53913 = alloca %struct.ScmObj*, align 8
%fptrToInt53914 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44780 to i64
%ae44780 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt53914)
store volatile %struct.ScmObj* %ae44780, %struct.ScmObj** %stackaddr$makeclosure53913, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44780, %struct.ScmObj* %k40482, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44780, %struct.ScmObj* %c40238, i64 1)
%ae44781 = call %struct.ScmObj* @const_init_int(i64 1)
%args52670$anf_45bind40418$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53915 = alloca %struct.ScmObj*, align 8
%args52670$anf_45bind40418$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44781, %struct.ScmObj* %args52670$anf_45bind40418$0)
store volatile %struct.ScmObj* %args52670$anf_45bind40418$1, %struct.ScmObj** %stackaddr$prim53915, align 8
%stackaddr$prim53916 = alloca %struct.ScmObj*, align 8
%args52670$anf_45bind40418$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44780, %struct.ScmObj* %args52670$anf_45bind40418$1)
store volatile %struct.ScmObj* %args52670$anf_45bind40418$2, %struct.ScmObj** %stackaddr$prim53916, align 8
%clofunc53917 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40418)
musttail call tailcc void %clofunc53917(%struct.ScmObj* %anf_45bind40418, %struct.ScmObj* %args52670$anf_45bind40418$2)
ret void
falsebranch$cmp53911:
%ae44800 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim53918 = alloca %struct.ScmObj*, align 8
%anf_45bind40420 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %addc40231, %struct.ScmObj* %ae44800)
store volatile %struct.ScmObj* %anf_45bind40420, %struct.ScmObj** %stackaddr$prim53918, align 8
%stackaddr$makeclosure53919 = alloca %struct.ScmObj*, align 8
%fptrToInt53920 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44801 to i64
%ae44801 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt53920)
store volatile %struct.ScmObj* %ae44801, %struct.ScmObj** %stackaddr$makeclosure53919, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44801, %struct.ScmObj* %pred40233, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44801, %struct.ScmObj* %fibc40230, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44801, %struct.ScmObj* %anf_45bind40420, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44801, %struct.ScmObj* %k40482, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44801, %struct.ScmObj* %x40239, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44801, %struct.ScmObj* %c40238, i64 5)
%ae44802 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53921 = alloca %struct.ScmObj*, align 8
%fptrToInt53922 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44803 to i64
%ae44803 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt53922)
store volatile %struct.ScmObj* %ae44803, %struct.ScmObj** %stackaddr$makeclosure53921, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44803, %struct.ScmObj* %pred40233, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44803, %struct.ScmObj* %fibc40230, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44803, %struct.ScmObj* %x40239, i64 2)
%args52739$ae44801$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53923 = alloca %struct.ScmObj*, align 8
%args52739$ae44801$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44803, %struct.ScmObj* %args52739$ae44801$0)
store volatile %struct.ScmObj* %args52739$ae44801$1, %struct.ScmObj** %stackaddr$prim53923, align 8
%stackaddr$prim53924 = alloca %struct.ScmObj*, align 8
%args52739$ae44801$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44802, %struct.ScmObj* %args52739$ae44801$1)
store volatile %struct.ScmObj* %args52739$ae44801$2, %struct.ScmObj** %stackaddr$prim53924, align 8
%clofunc53925 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44801)
musttail call tailcc void %clofunc53925(%struct.ScmObj* %ae44801, %struct.ScmObj* %args52739$ae44801$2)
ret void
}

define tailcc void @proc_clo$ae44780(%struct.ScmObj* %env$ae44780,%struct.ScmObj* %current_45args52666) {
%stackaddr$env-ref53926 = alloca %struct.ScmObj*, align 8
%k40482 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44780, i64 0)
store %struct.ScmObj* %k40482, %struct.ScmObj** %stackaddr$env-ref53926
%stackaddr$env-ref53927 = alloca %struct.ScmObj*, align 8
%c40238 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44780, i64 1)
store %struct.ScmObj* %c40238, %struct.ScmObj** %stackaddr$env-ref53927
%stackaddr$prim53928 = alloca %struct.ScmObj*, align 8
%_95k40487 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52666)
store volatile %struct.ScmObj* %_95k40487, %struct.ScmObj** %stackaddr$prim53928, align 8
%stackaddr$prim53929 = alloca %struct.ScmObj*, align 8
%current_45args52667 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52666)
store volatile %struct.ScmObj* %current_45args52667, %struct.ScmObj** %stackaddr$prim53929, align 8
%stackaddr$prim53930 = alloca %struct.ScmObj*, align 8
%anf_45bind40419 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52667)
store volatile %struct.ScmObj* %anf_45bind40419, %struct.ScmObj** %stackaddr$prim53930, align 8
%args52669$c40238$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53931 = alloca %struct.ScmObj*, align 8
%args52669$c40238$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40419, %struct.ScmObj* %args52669$c40238$0)
store volatile %struct.ScmObj* %args52669$c40238$1, %struct.ScmObj** %stackaddr$prim53931, align 8
%stackaddr$prim53932 = alloca %struct.ScmObj*, align 8
%args52669$c40238$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40482, %struct.ScmObj* %args52669$c40238$1)
store volatile %struct.ScmObj* %args52669$c40238$2, %struct.ScmObj** %stackaddr$prim53932, align 8
%clofunc53933 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %c40238)
musttail call tailcc void %clofunc53933(%struct.ScmObj* %c40238, %struct.ScmObj* %args52669$c40238$2)
ret void
}

define tailcc void @proc_clo$ae44801(%struct.ScmObj* %env$ae44801,%struct.ScmObj* %current_45args52671) {
%stackaddr$env-ref53934 = alloca %struct.ScmObj*, align 8
%pred40233 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44801, i64 0)
store %struct.ScmObj* %pred40233, %struct.ScmObj** %stackaddr$env-ref53934
%stackaddr$env-ref53935 = alloca %struct.ScmObj*, align 8
%fibc40230 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44801, i64 1)
store %struct.ScmObj* %fibc40230, %struct.ScmObj** %stackaddr$env-ref53935
%stackaddr$env-ref53936 = alloca %struct.ScmObj*, align 8
%anf_45bind40420 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44801, i64 2)
store %struct.ScmObj* %anf_45bind40420, %struct.ScmObj** %stackaddr$env-ref53936
%stackaddr$env-ref53937 = alloca %struct.ScmObj*, align 8
%k40482 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44801, i64 3)
store %struct.ScmObj* %k40482, %struct.ScmObj** %stackaddr$env-ref53937
%stackaddr$env-ref53938 = alloca %struct.ScmObj*, align 8
%x40239 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44801, i64 4)
store %struct.ScmObj* %x40239, %struct.ScmObj** %stackaddr$env-ref53938
%stackaddr$env-ref53939 = alloca %struct.ScmObj*, align 8
%c40238 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44801, i64 5)
store %struct.ScmObj* %c40238, %struct.ScmObj** %stackaddr$env-ref53939
%stackaddr$prim53940 = alloca %struct.ScmObj*, align 8
%_95k40488 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52671)
store volatile %struct.ScmObj* %_95k40488, %struct.ScmObj** %stackaddr$prim53940, align 8
%stackaddr$prim53941 = alloca %struct.ScmObj*, align 8
%current_45args52672 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52671)
store volatile %struct.ScmObj* %current_45args52672, %struct.ScmObj** %stackaddr$prim53941, align 8
%stackaddr$prim53942 = alloca %struct.ScmObj*, align 8
%anf_45bind40424 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52672)
store volatile %struct.ScmObj* %anf_45bind40424, %struct.ScmObj** %stackaddr$prim53942, align 8
%stackaddr$makeclosure53943 = alloca %struct.ScmObj*, align 8
%fptrToInt53944 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44856 to i64
%ae44856 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt53944)
store volatile %struct.ScmObj* %ae44856, %struct.ScmObj** %stackaddr$makeclosure53943, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44856, %struct.ScmObj* %pred40233, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44856, %struct.ScmObj* %fibc40230, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44856, %struct.ScmObj* %anf_45bind40420, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44856, %struct.ScmObj* %k40482, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44856, %struct.ScmObj* %x40239, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44856, %struct.ScmObj* %c40238, i64 5)
%stackaddr$makeclosure53945 = alloca %struct.ScmObj*, align 8
%fptrToInt53946 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44857 to i64
%ae44857 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt53946)
store volatile %struct.ScmObj* %ae44857, %struct.ScmObj** %stackaddr$makeclosure53945, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44857, %struct.ScmObj* %pred40233, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44857, %struct.ScmObj* %fibc40230, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44857, %struct.ScmObj* %anf_45bind40420, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44857, %struct.ScmObj* %k40482, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44857, %struct.ScmObj* %x40239, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44857, %struct.ScmObj* %c40238, i64 5)
%args52730$anf_45bind40424$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53947 = alloca %struct.ScmObj*, align 8
%args52730$anf_45bind40424$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44857, %struct.ScmObj* %args52730$anf_45bind40424$0)
store volatile %struct.ScmObj* %args52730$anf_45bind40424$1, %struct.ScmObj** %stackaddr$prim53947, align 8
%stackaddr$prim53948 = alloca %struct.ScmObj*, align 8
%args52730$anf_45bind40424$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44856, %struct.ScmObj* %args52730$anf_45bind40424$1)
store volatile %struct.ScmObj* %args52730$anf_45bind40424$2, %struct.ScmObj** %stackaddr$prim53948, align 8
%clofunc53949 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40424)
musttail call tailcc void %clofunc53949(%struct.ScmObj* %anf_45bind40424, %struct.ScmObj* %args52730$anf_45bind40424$2)
ret void
}

define tailcc void @proc_clo$ae44856(%struct.ScmObj* %env$ae44856,%struct.ScmObj* %current_45args52674) {
%stackaddr$env-ref53950 = alloca %struct.ScmObj*, align 8
%pred40233 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44856, i64 0)
store %struct.ScmObj* %pred40233, %struct.ScmObj** %stackaddr$env-ref53950
%stackaddr$env-ref53951 = alloca %struct.ScmObj*, align 8
%fibc40230 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44856, i64 1)
store %struct.ScmObj* %fibc40230, %struct.ScmObj** %stackaddr$env-ref53951
%stackaddr$env-ref53952 = alloca %struct.ScmObj*, align 8
%anf_45bind40420 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44856, i64 2)
store %struct.ScmObj* %anf_45bind40420, %struct.ScmObj** %stackaddr$env-ref53952
%stackaddr$env-ref53953 = alloca %struct.ScmObj*, align 8
%k40482 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44856, i64 3)
store %struct.ScmObj* %k40482, %struct.ScmObj** %stackaddr$env-ref53953
%stackaddr$env-ref53954 = alloca %struct.ScmObj*, align 8
%x40239 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44856, i64 4)
store %struct.ScmObj* %x40239, %struct.ScmObj** %stackaddr$env-ref53954
%stackaddr$env-ref53955 = alloca %struct.ScmObj*, align 8
%c40238 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44856, i64 5)
store %struct.ScmObj* %c40238, %struct.ScmObj** %stackaddr$env-ref53955
%stackaddr$prim53956 = alloca %struct.ScmObj*, align 8
%_95k40489 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52674)
store volatile %struct.ScmObj* %_95k40489, %struct.ScmObj** %stackaddr$prim53956, align 8
%stackaddr$prim53957 = alloca %struct.ScmObj*, align 8
%current_45args52675 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52674)
store volatile %struct.ScmObj* %current_45args52675, %struct.ScmObj** %stackaddr$prim53957, align 8
%stackaddr$prim53958 = alloca %struct.ScmObj*, align 8
%anf_45bind40425 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52675)
store volatile %struct.ScmObj* %anf_45bind40425, %struct.ScmObj** %stackaddr$prim53958, align 8
%stackaddr$makeclosure53959 = alloca %struct.ScmObj*, align 8
%fptrToInt53960 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44993 to i64
%ae44993 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt53960)
store volatile %struct.ScmObj* %ae44993, %struct.ScmObj** %stackaddr$makeclosure53959, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44993, %struct.ScmObj* %anf_45bind40425, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44993, %struct.ScmObj* %anf_45bind40420, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44993, %struct.ScmObj* %k40482, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44993, %struct.ScmObj* %c40238, i64 3)
%ae44994 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53961 = alloca %struct.ScmObj*, align 8
%fptrToInt53962 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44995 to i64
%ae44995 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt53962)
store volatile %struct.ScmObj* %ae44995, %struct.ScmObj** %stackaddr$makeclosure53961, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44995, %struct.ScmObj* %pred40233, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44995, %struct.ScmObj* %fibc40230, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44995, %struct.ScmObj* %x40239, i64 2)
%args52701$ae44993$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53963 = alloca %struct.ScmObj*, align 8
%args52701$ae44993$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44995, %struct.ScmObj* %args52701$ae44993$0)
store volatile %struct.ScmObj* %args52701$ae44993$1, %struct.ScmObj** %stackaddr$prim53963, align 8
%stackaddr$prim53964 = alloca %struct.ScmObj*, align 8
%args52701$ae44993$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44994, %struct.ScmObj* %args52701$ae44993$1)
store volatile %struct.ScmObj* %args52701$ae44993$2, %struct.ScmObj** %stackaddr$prim53964, align 8
%clofunc53965 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44993)
musttail call tailcc void %clofunc53965(%struct.ScmObj* %ae44993, %struct.ScmObj* %args52701$ae44993$2)
ret void
}

define tailcc void @proc_clo$ae44993(%struct.ScmObj* %env$ae44993,%struct.ScmObj* %current_45args52677) {
%stackaddr$env-ref53966 = alloca %struct.ScmObj*, align 8
%anf_45bind40425 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44993, i64 0)
store %struct.ScmObj* %anf_45bind40425, %struct.ScmObj** %stackaddr$env-ref53966
%stackaddr$env-ref53967 = alloca %struct.ScmObj*, align 8
%anf_45bind40420 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44993, i64 1)
store %struct.ScmObj* %anf_45bind40420, %struct.ScmObj** %stackaddr$env-ref53967
%stackaddr$env-ref53968 = alloca %struct.ScmObj*, align 8
%k40482 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44993, i64 2)
store %struct.ScmObj* %k40482, %struct.ScmObj** %stackaddr$env-ref53968
%stackaddr$env-ref53969 = alloca %struct.ScmObj*, align 8
%c40238 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44993, i64 3)
store %struct.ScmObj* %c40238, %struct.ScmObj** %stackaddr$env-ref53969
%stackaddr$prim53970 = alloca %struct.ScmObj*, align 8
%_95k40490 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52677)
store volatile %struct.ScmObj* %_95k40490, %struct.ScmObj** %stackaddr$prim53970, align 8
%stackaddr$prim53971 = alloca %struct.ScmObj*, align 8
%current_45args52678 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52677)
store volatile %struct.ScmObj* %current_45args52678, %struct.ScmObj** %stackaddr$prim53971, align 8
%stackaddr$prim53972 = alloca %struct.ScmObj*, align 8
%anf_45bind40431 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52678)
store volatile %struct.ScmObj* %anf_45bind40431, %struct.ScmObj** %stackaddr$prim53972, align 8
%stackaddr$makeclosure53973 = alloca %struct.ScmObj*, align 8
%fptrToInt53974 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae45083 to i64
%ae45083 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt53974)
store volatile %struct.ScmObj* %ae45083, %struct.ScmObj** %stackaddr$makeclosure53973, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae45083, %struct.ScmObj* %anf_45bind40425, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae45083, %struct.ScmObj* %anf_45bind40420, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae45083, %struct.ScmObj* %k40482, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae45083, %struct.ScmObj* %c40238, i64 3)
%stackaddr$makeclosure53975 = alloca %struct.ScmObj*, align 8
%fptrToInt53976 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae45084 to i64
%ae45084 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt53976)
store volatile %struct.ScmObj* %ae45084, %struct.ScmObj** %stackaddr$makeclosure53975, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae45084, %struct.ScmObj* %anf_45bind40425, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae45084, %struct.ScmObj* %anf_45bind40420, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae45084, %struct.ScmObj* %k40482, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae45084, %struct.ScmObj* %c40238, i64 3)
%args52688$anf_45bind40431$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53977 = alloca %struct.ScmObj*, align 8
%args52688$anf_45bind40431$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45084, %struct.ScmObj* %args52688$anf_45bind40431$0)
store volatile %struct.ScmObj* %args52688$anf_45bind40431$1, %struct.ScmObj** %stackaddr$prim53977, align 8
%stackaddr$prim53978 = alloca %struct.ScmObj*, align 8
%args52688$anf_45bind40431$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45083, %struct.ScmObj* %args52688$anf_45bind40431$1)
store volatile %struct.ScmObj* %args52688$anf_45bind40431$2, %struct.ScmObj** %stackaddr$prim53978, align 8
%clofunc53979 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40431)
musttail call tailcc void %clofunc53979(%struct.ScmObj* %anf_45bind40431, %struct.ScmObj* %args52688$anf_45bind40431$2)
ret void
}

define tailcc void @proc_clo$ae45083(%struct.ScmObj* %env$ae45083,%struct.ScmObj* %current_45args52680) {
%stackaddr$env-ref53980 = alloca %struct.ScmObj*, align 8
%anf_45bind40425 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45083, i64 0)
store %struct.ScmObj* %anf_45bind40425, %struct.ScmObj** %stackaddr$env-ref53980
%stackaddr$env-ref53981 = alloca %struct.ScmObj*, align 8
%anf_45bind40420 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45083, i64 1)
store %struct.ScmObj* %anf_45bind40420, %struct.ScmObj** %stackaddr$env-ref53981
%stackaddr$env-ref53982 = alloca %struct.ScmObj*, align 8
%k40482 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45083, i64 2)
store %struct.ScmObj* %k40482, %struct.ScmObj** %stackaddr$env-ref53982
%stackaddr$env-ref53983 = alloca %struct.ScmObj*, align 8
%c40238 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45083, i64 3)
store %struct.ScmObj* %c40238, %struct.ScmObj** %stackaddr$env-ref53983
%stackaddr$prim53984 = alloca %struct.ScmObj*, align 8
%_95k40491 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52680)
store volatile %struct.ScmObj* %_95k40491, %struct.ScmObj** %stackaddr$prim53984, align 8
%stackaddr$prim53985 = alloca %struct.ScmObj*, align 8
%current_45args52681 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52680)
store volatile %struct.ScmObj* %current_45args52681, %struct.ScmObj** %stackaddr$prim53985, align 8
%stackaddr$prim53986 = alloca %struct.ScmObj*, align 8
%anf_45bind40432 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52681)
store volatile %struct.ScmObj* %anf_45bind40432, %struct.ScmObj** %stackaddr$prim53986, align 8
%args52683$anf_45bind40420$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53987 = alloca %struct.ScmObj*, align 8
%args52683$anf_45bind40420$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %c40238, %struct.ScmObj* %args52683$anf_45bind40420$0)
store volatile %struct.ScmObj* %args52683$anf_45bind40420$1, %struct.ScmObj** %stackaddr$prim53987, align 8
%stackaddr$prim53988 = alloca %struct.ScmObj*, align 8
%args52683$anf_45bind40420$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40432, %struct.ScmObj* %args52683$anf_45bind40420$1)
store volatile %struct.ScmObj* %args52683$anf_45bind40420$2, %struct.ScmObj** %stackaddr$prim53988, align 8
%stackaddr$prim53989 = alloca %struct.ScmObj*, align 8
%args52683$anf_45bind40420$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40425, %struct.ScmObj* %args52683$anf_45bind40420$2)
store volatile %struct.ScmObj* %args52683$anf_45bind40420$3, %struct.ScmObj** %stackaddr$prim53989, align 8
%stackaddr$prim53990 = alloca %struct.ScmObj*, align 8
%args52683$anf_45bind40420$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40482, %struct.ScmObj* %args52683$anf_45bind40420$3)
store volatile %struct.ScmObj* %args52683$anf_45bind40420$4, %struct.ScmObj** %stackaddr$prim53990, align 8
%clofunc53991 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40420)
musttail call tailcc void %clofunc53991(%struct.ScmObj* %anf_45bind40420, %struct.ScmObj* %args52683$anf_45bind40420$4)
ret void
}

define tailcc void @proc_clo$ae45084(%struct.ScmObj* %env$ae45084,%struct.ScmObj* %current_45args52684) {
%stackaddr$env-ref53992 = alloca %struct.ScmObj*, align 8
%anf_45bind40425 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45084, i64 0)
store %struct.ScmObj* %anf_45bind40425, %struct.ScmObj** %stackaddr$env-ref53992
%stackaddr$env-ref53993 = alloca %struct.ScmObj*, align 8
%anf_45bind40420 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45084, i64 1)
store %struct.ScmObj* %anf_45bind40420, %struct.ScmObj** %stackaddr$env-ref53993
%stackaddr$env-ref53994 = alloca %struct.ScmObj*, align 8
%k40482 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45084, i64 2)
store %struct.ScmObj* %k40482, %struct.ScmObj** %stackaddr$env-ref53994
%stackaddr$env-ref53995 = alloca %struct.ScmObj*, align 8
%c40238 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45084, i64 3)
store %struct.ScmObj* %c40238, %struct.ScmObj** %stackaddr$env-ref53995
%stackaddr$prim53996 = alloca %struct.ScmObj*, align 8
%_95k40491 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52684)
store volatile %struct.ScmObj* %_95k40491, %struct.ScmObj** %stackaddr$prim53996, align 8
%stackaddr$prim53997 = alloca %struct.ScmObj*, align 8
%current_45args52685 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52684)
store volatile %struct.ScmObj* %current_45args52685, %struct.ScmObj** %stackaddr$prim53997, align 8
%stackaddr$prim53998 = alloca %struct.ScmObj*, align 8
%anf_45bind40432 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52685)
store volatile %struct.ScmObj* %anf_45bind40432, %struct.ScmObj** %stackaddr$prim53998, align 8
%args52687$anf_45bind40420$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53999 = alloca %struct.ScmObj*, align 8
%args52687$anf_45bind40420$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %c40238, %struct.ScmObj* %args52687$anf_45bind40420$0)
store volatile %struct.ScmObj* %args52687$anf_45bind40420$1, %struct.ScmObj** %stackaddr$prim53999, align 8
%stackaddr$prim54000 = alloca %struct.ScmObj*, align 8
%args52687$anf_45bind40420$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40432, %struct.ScmObj* %args52687$anf_45bind40420$1)
store volatile %struct.ScmObj* %args52687$anf_45bind40420$2, %struct.ScmObj** %stackaddr$prim54000, align 8
%stackaddr$prim54001 = alloca %struct.ScmObj*, align 8
%args52687$anf_45bind40420$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40425, %struct.ScmObj* %args52687$anf_45bind40420$2)
store volatile %struct.ScmObj* %args52687$anf_45bind40420$3, %struct.ScmObj** %stackaddr$prim54001, align 8
%stackaddr$prim54002 = alloca %struct.ScmObj*, align 8
%args52687$anf_45bind40420$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40482, %struct.ScmObj* %args52687$anf_45bind40420$3)
store volatile %struct.ScmObj* %args52687$anf_45bind40420$4, %struct.ScmObj** %stackaddr$prim54002, align 8
%clofunc54003 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40420)
musttail call tailcc void %clofunc54003(%struct.ScmObj* %anf_45bind40420, %struct.ScmObj* %args52687$anf_45bind40420$4)
ret void
}

define tailcc void @proc_clo$ae44995(%struct.ScmObj* %env$ae44995,%struct.ScmObj* %current_45args52689) {
%stackaddr$env-ref54004 = alloca %struct.ScmObj*, align 8
%pred40233 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44995, i64 0)
store %struct.ScmObj* %pred40233, %struct.ScmObj** %stackaddr$env-ref54004
%stackaddr$env-ref54005 = alloca %struct.ScmObj*, align 8
%fibc40230 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44995, i64 1)
store %struct.ScmObj* %fibc40230, %struct.ScmObj** %stackaddr$env-ref54005
%stackaddr$env-ref54006 = alloca %struct.ScmObj*, align 8
%x40239 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44995, i64 2)
store %struct.ScmObj* %x40239, %struct.ScmObj** %stackaddr$env-ref54006
%stackaddr$prim54007 = alloca %struct.ScmObj*, align 8
%k40492 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52689)
store volatile %struct.ScmObj* %k40492, %struct.ScmObj** %stackaddr$prim54007, align 8
%stackaddr$prim54008 = alloca %struct.ScmObj*, align 8
%current_45args52690 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52689)
store volatile %struct.ScmObj* %current_45args52690, %struct.ScmObj** %stackaddr$prim54008, align 8
%stackaddr$prim54009 = alloca %struct.ScmObj*, align 8
%c40241 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52690)
store volatile %struct.ScmObj* %c40241, %struct.ScmObj** %stackaddr$prim54009, align 8
%ae44997 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54010 = alloca %struct.ScmObj*, align 8
%anf_45bind40426 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %fibc40230, %struct.ScmObj* %ae44997)
store volatile %struct.ScmObj* %anf_45bind40426, %struct.ScmObj** %stackaddr$prim54010, align 8
%ae44999 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54011 = alloca %struct.ScmObj*, align 8
%anf_45bind40427 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %pred40233, %struct.ScmObj* %ae44999)
store volatile %struct.ScmObj* %anf_45bind40427, %struct.ScmObj** %stackaddr$prim54011, align 8
%ae45001 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54012 = alloca %struct.ScmObj*, align 8
%anf_45bind40428 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %pred40233, %struct.ScmObj* %ae45001)
store volatile %struct.ScmObj* %anf_45bind40428, %struct.ScmObj** %stackaddr$prim54012, align 8
%stackaddr$makeclosure54013 = alloca %struct.ScmObj*, align 8
%fptrToInt54014 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae45003 to i64
%ae45003 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54014)
store volatile %struct.ScmObj* %ae45003, %struct.ScmObj** %stackaddr$makeclosure54013, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae45003, %struct.ScmObj* %c40241, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae45003, %struct.ScmObj* %k40492, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae45003, %struct.ScmObj* %anf_45bind40427, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae45003, %struct.ScmObj* %anf_45bind40426, i64 3)
%args52700$anf_45bind40428$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54015 = alloca %struct.ScmObj*, align 8
%args52700$anf_45bind40428$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x40239, %struct.ScmObj* %args52700$anf_45bind40428$0)
store volatile %struct.ScmObj* %args52700$anf_45bind40428$1, %struct.ScmObj** %stackaddr$prim54015, align 8
%stackaddr$prim54016 = alloca %struct.ScmObj*, align 8
%args52700$anf_45bind40428$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45003, %struct.ScmObj* %args52700$anf_45bind40428$1)
store volatile %struct.ScmObj* %args52700$anf_45bind40428$2, %struct.ScmObj** %stackaddr$prim54016, align 8
%clofunc54017 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40428)
musttail call tailcc void %clofunc54017(%struct.ScmObj* %anf_45bind40428, %struct.ScmObj* %args52700$anf_45bind40428$2)
ret void
}

define tailcc void @proc_clo$ae45003(%struct.ScmObj* %env$ae45003,%struct.ScmObj* %current_45args52692) {
%stackaddr$env-ref54018 = alloca %struct.ScmObj*, align 8
%c40241 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45003, i64 0)
store %struct.ScmObj* %c40241, %struct.ScmObj** %stackaddr$env-ref54018
%stackaddr$env-ref54019 = alloca %struct.ScmObj*, align 8
%k40492 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45003, i64 1)
store %struct.ScmObj* %k40492, %struct.ScmObj** %stackaddr$env-ref54019
%stackaddr$env-ref54020 = alloca %struct.ScmObj*, align 8
%anf_45bind40427 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45003, i64 2)
store %struct.ScmObj* %anf_45bind40427, %struct.ScmObj** %stackaddr$env-ref54020
%stackaddr$env-ref54021 = alloca %struct.ScmObj*, align 8
%anf_45bind40426 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45003, i64 3)
store %struct.ScmObj* %anf_45bind40426, %struct.ScmObj** %stackaddr$env-ref54021
%stackaddr$prim54022 = alloca %struct.ScmObj*, align 8
%_95k40493 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52692)
store volatile %struct.ScmObj* %_95k40493, %struct.ScmObj** %stackaddr$prim54022, align 8
%stackaddr$prim54023 = alloca %struct.ScmObj*, align 8
%current_45args52693 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52692)
store volatile %struct.ScmObj* %current_45args52693, %struct.ScmObj** %stackaddr$prim54023, align 8
%stackaddr$prim54024 = alloca %struct.ScmObj*, align 8
%anf_45bind40429 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52693)
store volatile %struct.ScmObj* %anf_45bind40429, %struct.ScmObj** %stackaddr$prim54024, align 8
%stackaddr$makeclosure54025 = alloca %struct.ScmObj*, align 8
%fptrToInt54026 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae45006 to i64
%ae45006 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54026)
store volatile %struct.ScmObj* %ae45006, %struct.ScmObj** %stackaddr$makeclosure54025, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae45006, %struct.ScmObj* %c40241, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae45006, %struct.ScmObj* %k40492, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae45006, %struct.ScmObj* %anf_45bind40426, i64 2)
%args52699$anf_45bind40427$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54027 = alloca %struct.ScmObj*, align 8
%args52699$anf_45bind40427$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40429, %struct.ScmObj* %args52699$anf_45bind40427$0)
store volatile %struct.ScmObj* %args52699$anf_45bind40427$1, %struct.ScmObj** %stackaddr$prim54027, align 8
%stackaddr$prim54028 = alloca %struct.ScmObj*, align 8
%args52699$anf_45bind40427$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45006, %struct.ScmObj* %args52699$anf_45bind40427$1)
store volatile %struct.ScmObj* %args52699$anf_45bind40427$2, %struct.ScmObj** %stackaddr$prim54028, align 8
%clofunc54029 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40427)
musttail call tailcc void %clofunc54029(%struct.ScmObj* %anf_45bind40427, %struct.ScmObj* %args52699$anf_45bind40427$2)
ret void
}

define tailcc void @proc_clo$ae45006(%struct.ScmObj* %env$ae45006,%struct.ScmObj* %current_45args52695) {
%stackaddr$env-ref54030 = alloca %struct.ScmObj*, align 8
%c40241 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45006, i64 0)
store %struct.ScmObj* %c40241, %struct.ScmObj** %stackaddr$env-ref54030
%stackaddr$env-ref54031 = alloca %struct.ScmObj*, align 8
%k40492 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45006, i64 1)
store %struct.ScmObj* %k40492, %struct.ScmObj** %stackaddr$env-ref54031
%stackaddr$env-ref54032 = alloca %struct.ScmObj*, align 8
%anf_45bind40426 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45006, i64 2)
store %struct.ScmObj* %anf_45bind40426, %struct.ScmObj** %stackaddr$env-ref54032
%stackaddr$prim54033 = alloca %struct.ScmObj*, align 8
%_95k40494 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52695)
store volatile %struct.ScmObj* %_95k40494, %struct.ScmObj** %stackaddr$prim54033, align 8
%stackaddr$prim54034 = alloca %struct.ScmObj*, align 8
%current_45args52696 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52695)
store volatile %struct.ScmObj* %current_45args52696, %struct.ScmObj** %stackaddr$prim54034, align 8
%stackaddr$prim54035 = alloca %struct.ScmObj*, align 8
%anf_45bind40430 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52696)
store volatile %struct.ScmObj* %anf_45bind40430, %struct.ScmObj** %stackaddr$prim54035, align 8
%args52698$anf_45bind40426$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54036 = alloca %struct.ScmObj*, align 8
%args52698$anf_45bind40426$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %c40241, %struct.ScmObj* %args52698$anf_45bind40426$0)
store volatile %struct.ScmObj* %args52698$anf_45bind40426$1, %struct.ScmObj** %stackaddr$prim54036, align 8
%stackaddr$prim54037 = alloca %struct.ScmObj*, align 8
%args52698$anf_45bind40426$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40430, %struct.ScmObj* %args52698$anf_45bind40426$1)
store volatile %struct.ScmObj* %args52698$anf_45bind40426$2, %struct.ScmObj** %stackaddr$prim54037, align 8
%stackaddr$prim54038 = alloca %struct.ScmObj*, align 8
%args52698$anf_45bind40426$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40492, %struct.ScmObj* %args52698$anf_45bind40426$2)
store volatile %struct.ScmObj* %args52698$anf_45bind40426$3, %struct.ScmObj** %stackaddr$prim54038, align 8
%clofunc54039 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40426)
musttail call tailcc void %clofunc54039(%struct.ScmObj* %anf_45bind40426, %struct.ScmObj* %args52698$anf_45bind40426$3)
ret void
}

define tailcc void @proc_clo$ae44857(%struct.ScmObj* %env$ae44857,%struct.ScmObj* %current_45args52702) {
%stackaddr$env-ref54040 = alloca %struct.ScmObj*, align 8
%pred40233 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44857, i64 0)
store %struct.ScmObj* %pred40233, %struct.ScmObj** %stackaddr$env-ref54040
%stackaddr$env-ref54041 = alloca %struct.ScmObj*, align 8
%fibc40230 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44857, i64 1)
store %struct.ScmObj* %fibc40230, %struct.ScmObj** %stackaddr$env-ref54041
%stackaddr$env-ref54042 = alloca %struct.ScmObj*, align 8
%anf_45bind40420 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44857, i64 2)
store %struct.ScmObj* %anf_45bind40420, %struct.ScmObj** %stackaddr$env-ref54042
%stackaddr$env-ref54043 = alloca %struct.ScmObj*, align 8
%k40482 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44857, i64 3)
store %struct.ScmObj* %k40482, %struct.ScmObj** %stackaddr$env-ref54043
%stackaddr$env-ref54044 = alloca %struct.ScmObj*, align 8
%x40239 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44857, i64 4)
store %struct.ScmObj* %x40239, %struct.ScmObj** %stackaddr$env-ref54044
%stackaddr$env-ref54045 = alloca %struct.ScmObj*, align 8
%c40238 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44857, i64 5)
store %struct.ScmObj* %c40238, %struct.ScmObj** %stackaddr$env-ref54045
%stackaddr$prim54046 = alloca %struct.ScmObj*, align 8
%_95k40489 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52702)
store volatile %struct.ScmObj* %_95k40489, %struct.ScmObj** %stackaddr$prim54046, align 8
%stackaddr$prim54047 = alloca %struct.ScmObj*, align 8
%current_45args52703 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52702)
store volatile %struct.ScmObj* %current_45args52703, %struct.ScmObj** %stackaddr$prim54047, align 8
%stackaddr$prim54048 = alloca %struct.ScmObj*, align 8
%anf_45bind40425 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52703)
store volatile %struct.ScmObj* %anf_45bind40425, %struct.ScmObj** %stackaddr$prim54048, align 8
%stackaddr$makeclosure54049 = alloca %struct.ScmObj*, align 8
%fptrToInt54050 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44858 to i64
%ae44858 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54050)
store volatile %struct.ScmObj* %ae44858, %struct.ScmObj** %stackaddr$makeclosure54049, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44858, %struct.ScmObj* %anf_45bind40425, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44858, %struct.ScmObj* %anf_45bind40420, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44858, %struct.ScmObj* %k40482, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44858, %struct.ScmObj* %c40238, i64 3)
%ae44859 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54051 = alloca %struct.ScmObj*, align 8
%fptrToInt54052 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44860 to i64
%ae44860 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54052)
store volatile %struct.ScmObj* %ae44860, %struct.ScmObj** %stackaddr$makeclosure54051, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44860, %struct.ScmObj* %pred40233, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44860, %struct.ScmObj* %fibc40230, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44860, %struct.ScmObj* %x40239, i64 2)
%args52729$ae44858$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54053 = alloca %struct.ScmObj*, align 8
%args52729$ae44858$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44860, %struct.ScmObj* %args52729$ae44858$0)
store volatile %struct.ScmObj* %args52729$ae44858$1, %struct.ScmObj** %stackaddr$prim54053, align 8
%stackaddr$prim54054 = alloca %struct.ScmObj*, align 8
%args52729$ae44858$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44859, %struct.ScmObj* %args52729$ae44858$1)
store volatile %struct.ScmObj* %args52729$ae44858$2, %struct.ScmObj** %stackaddr$prim54054, align 8
%clofunc54055 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44858)
musttail call tailcc void %clofunc54055(%struct.ScmObj* %ae44858, %struct.ScmObj* %args52729$ae44858$2)
ret void
}

define tailcc void @proc_clo$ae44858(%struct.ScmObj* %env$ae44858,%struct.ScmObj* %current_45args52705) {
%stackaddr$env-ref54056 = alloca %struct.ScmObj*, align 8
%anf_45bind40425 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44858, i64 0)
store %struct.ScmObj* %anf_45bind40425, %struct.ScmObj** %stackaddr$env-ref54056
%stackaddr$env-ref54057 = alloca %struct.ScmObj*, align 8
%anf_45bind40420 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44858, i64 1)
store %struct.ScmObj* %anf_45bind40420, %struct.ScmObj** %stackaddr$env-ref54057
%stackaddr$env-ref54058 = alloca %struct.ScmObj*, align 8
%k40482 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44858, i64 2)
store %struct.ScmObj* %k40482, %struct.ScmObj** %stackaddr$env-ref54058
%stackaddr$env-ref54059 = alloca %struct.ScmObj*, align 8
%c40238 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44858, i64 3)
store %struct.ScmObj* %c40238, %struct.ScmObj** %stackaddr$env-ref54059
%stackaddr$prim54060 = alloca %struct.ScmObj*, align 8
%_95k40490 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52705)
store volatile %struct.ScmObj* %_95k40490, %struct.ScmObj** %stackaddr$prim54060, align 8
%stackaddr$prim54061 = alloca %struct.ScmObj*, align 8
%current_45args52706 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52705)
store volatile %struct.ScmObj* %current_45args52706, %struct.ScmObj** %stackaddr$prim54061, align 8
%stackaddr$prim54062 = alloca %struct.ScmObj*, align 8
%anf_45bind40431 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52706)
store volatile %struct.ScmObj* %anf_45bind40431, %struct.ScmObj** %stackaddr$prim54062, align 8
%stackaddr$makeclosure54063 = alloca %struct.ScmObj*, align 8
%fptrToInt54064 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44948 to i64
%ae44948 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54064)
store volatile %struct.ScmObj* %ae44948, %struct.ScmObj** %stackaddr$makeclosure54063, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44948, %struct.ScmObj* %anf_45bind40425, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44948, %struct.ScmObj* %anf_45bind40420, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44948, %struct.ScmObj* %k40482, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44948, %struct.ScmObj* %c40238, i64 3)
%stackaddr$makeclosure54065 = alloca %struct.ScmObj*, align 8
%fptrToInt54066 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44949 to i64
%ae44949 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54066)
store volatile %struct.ScmObj* %ae44949, %struct.ScmObj** %stackaddr$makeclosure54065, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44949, %struct.ScmObj* %anf_45bind40425, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44949, %struct.ScmObj* %anf_45bind40420, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44949, %struct.ScmObj* %k40482, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44949, %struct.ScmObj* %c40238, i64 3)
%args52716$anf_45bind40431$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54067 = alloca %struct.ScmObj*, align 8
%args52716$anf_45bind40431$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44949, %struct.ScmObj* %args52716$anf_45bind40431$0)
store volatile %struct.ScmObj* %args52716$anf_45bind40431$1, %struct.ScmObj** %stackaddr$prim54067, align 8
%stackaddr$prim54068 = alloca %struct.ScmObj*, align 8
%args52716$anf_45bind40431$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44948, %struct.ScmObj* %args52716$anf_45bind40431$1)
store volatile %struct.ScmObj* %args52716$anf_45bind40431$2, %struct.ScmObj** %stackaddr$prim54068, align 8
%clofunc54069 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40431)
musttail call tailcc void %clofunc54069(%struct.ScmObj* %anf_45bind40431, %struct.ScmObj* %args52716$anf_45bind40431$2)
ret void
}

define tailcc void @proc_clo$ae44948(%struct.ScmObj* %env$ae44948,%struct.ScmObj* %current_45args52708) {
%stackaddr$env-ref54070 = alloca %struct.ScmObj*, align 8
%anf_45bind40425 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44948, i64 0)
store %struct.ScmObj* %anf_45bind40425, %struct.ScmObj** %stackaddr$env-ref54070
%stackaddr$env-ref54071 = alloca %struct.ScmObj*, align 8
%anf_45bind40420 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44948, i64 1)
store %struct.ScmObj* %anf_45bind40420, %struct.ScmObj** %stackaddr$env-ref54071
%stackaddr$env-ref54072 = alloca %struct.ScmObj*, align 8
%k40482 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44948, i64 2)
store %struct.ScmObj* %k40482, %struct.ScmObj** %stackaddr$env-ref54072
%stackaddr$env-ref54073 = alloca %struct.ScmObj*, align 8
%c40238 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44948, i64 3)
store %struct.ScmObj* %c40238, %struct.ScmObj** %stackaddr$env-ref54073
%stackaddr$prim54074 = alloca %struct.ScmObj*, align 8
%_95k40491 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52708)
store volatile %struct.ScmObj* %_95k40491, %struct.ScmObj** %stackaddr$prim54074, align 8
%stackaddr$prim54075 = alloca %struct.ScmObj*, align 8
%current_45args52709 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52708)
store volatile %struct.ScmObj* %current_45args52709, %struct.ScmObj** %stackaddr$prim54075, align 8
%stackaddr$prim54076 = alloca %struct.ScmObj*, align 8
%anf_45bind40432 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52709)
store volatile %struct.ScmObj* %anf_45bind40432, %struct.ScmObj** %stackaddr$prim54076, align 8
%args52711$anf_45bind40420$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54077 = alloca %struct.ScmObj*, align 8
%args52711$anf_45bind40420$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %c40238, %struct.ScmObj* %args52711$anf_45bind40420$0)
store volatile %struct.ScmObj* %args52711$anf_45bind40420$1, %struct.ScmObj** %stackaddr$prim54077, align 8
%stackaddr$prim54078 = alloca %struct.ScmObj*, align 8
%args52711$anf_45bind40420$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40432, %struct.ScmObj* %args52711$anf_45bind40420$1)
store volatile %struct.ScmObj* %args52711$anf_45bind40420$2, %struct.ScmObj** %stackaddr$prim54078, align 8
%stackaddr$prim54079 = alloca %struct.ScmObj*, align 8
%args52711$anf_45bind40420$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40425, %struct.ScmObj* %args52711$anf_45bind40420$2)
store volatile %struct.ScmObj* %args52711$anf_45bind40420$3, %struct.ScmObj** %stackaddr$prim54079, align 8
%stackaddr$prim54080 = alloca %struct.ScmObj*, align 8
%args52711$anf_45bind40420$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40482, %struct.ScmObj* %args52711$anf_45bind40420$3)
store volatile %struct.ScmObj* %args52711$anf_45bind40420$4, %struct.ScmObj** %stackaddr$prim54080, align 8
%clofunc54081 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40420)
musttail call tailcc void %clofunc54081(%struct.ScmObj* %anf_45bind40420, %struct.ScmObj* %args52711$anf_45bind40420$4)
ret void
}

define tailcc void @proc_clo$ae44949(%struct.ScmObj* %env$ae44949,%struct.ScmObj* %current_45args52712) {
%stackaddr$env-ref54082 = alloca %struct.ScmObj*, align 8
%anf_45bind40425 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44949, i64 0)
store %struct.ScmObj* %anf_45bind40425, %struct.ScmObj** %stackaddr$env-ref54082
%stackaddr$env-ref54083 = alloca %struct.ScmObj*, align 8
%anf_45bind40420 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44949, i64 1)
store %struct.ScmObj* %anf_45bind40420, %struct.ScmObj** %stackaddr$env-ref54083
%stackaddr$env-ref54084 = alloca %struct.ScmObj*, align 8
%k40482 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44949, i64 2)
store %struct.ScmObj* %k40482, %struct.ScmObj** %stackaddr$env-ref54084
%stackaddr$env-ref54085 = alloca %struct.ScmObj*, align 8
%c40238 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44949, i64 3)
store %struct.ScmObj* %c40238, %struct.ScmObj** %stackaddr$env-ref54085
%stackaddr$prim54086 = alloca %struct.ScmObj*, align 8
%_95k40491 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52712)
store volatile %struct.ScmObj* %_95k40491, %struct.ScmObj** %stackaddr$prim54086, align 8
%stackaddr$prim54087 = alloca %struct.ScmObj*, align 8
%current_45args52713 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52712)
store volatile %struct.ScmObj* %current_45args52713, %struct.ScmObj** %stackaddr$prim54087, align 8
%stackaddr$prim54088 = alloca %struct.ScmObj*, align 8
%anf_45bind40432 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52713)
store volatile %struct.ScmObj* %anf_45bind40432, %struct.ScmObj** %stackaddr$prim54088, align 8
%args52715$anf_45bind40420$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54089 = alloca %struct.ScmObj*, align 8
%args52715$anf_45bind40420$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %c40238, %struct.ScmObj* %args52715$anf_45bind40420$0)
store volatile %struct.ScmObj* %args52715$anf_45bind40420$1, %struct.ScmObj** %stackaddr$prim54089, align 8
%stackaddr$prim54090 = alloca %struct.ScmObj*, align 8
%args52715$anf_45bind40420$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40432, %struct.ScmObj* %args52715$anf_45bind40420$1)
store volatile %struct.ScmObj* %args52715$anf_45bind40420$2, %struct.ScmObj** %stackaddr$prim54090, align 8
%stackaddr$prim54091 = alloca %struct.ScmObj*, align 8
%args52715$anf_45bind40420$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40425, %struct.ScmObj* %args52715$anf_45bind40420$2)
store volatile %struct.ScmObj* %args52715$anf_45bind40420$3, %struct.ScmObj** %stackaddr$prim54091, align 8
%stackaddr$prim54092 = alloca %struct.ScmObj*, align 8
%args52715$anf_45bind40420$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40482, %struct.ScmObj* %args52715$anf_45bind40420$3)
store volatile %struct.ScmObj* %args52715$anf_45bind40420$4, %struct.ScmObj** %stackaddr$prim54092, align 8
%clofunc54093 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40420)
musttail call tailcc void %clofunc54093(%struct.ScmObj* %anf_45bind40420, %struct.ScmObj* %args52715$anf_45bind40420$4)
ret void
}

define tailcc void @proc_clo$ae44860(%struct.ScmObj* %env$ae44860,%struct.ScmObj* %current_45args52717) {
%stackaddr$env-ref54094 = alloca %struct.ScmObj*, align 8
%pred40233 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44860, i64 0)
store %struct.ScmObj* %pred40233, %struct.ScmObj** %stackaddr$env-ref54094
%stackaddr$env-ref54095 = alloca %struct.ScmObj*, align 8
%fibc40230 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44860, i64 1)
store %struct.ScmObj* %fibc40230, %struct.ScmObj** %stackaddr$env-ref54095
%stackaddr$env-ref54096 = alloca %struct.ScmObj*, align 8
%x40239 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44860, i64 2)
store %struct.ScmObj* %x40239, %struct.ScmObj** %stackaddr$env-ref54096
%stackaddr$prim54097 = alloca %struct.ScmObj*, align 8
%k40492 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52717)
store volatile %struct.ScmObj* %k40492, %struct.ScmObj** %stackaddr$prim54097, align 8
%stackaddr$prim54098 = alloca %struct.ScmObj*, align 8
%current_45args52718 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52717)
store volatile %struct.ScmObj* %current_45args52718, %struct.ScmObj** %stackaddr$prim54098, align 8
%stackaddr$prim54099 = alloca %struct.ScmObj*, align 8
%c40241 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52718)
store volatile %struct.ScmObj* %c40241, %struct.ScmObj** %stackaddr$prim54099, align 8
%ae44862 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54100 = alloca %struct.ScmObj*, align 8
%anf_45bind40426 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %fibc40230, %struct.ScmObj* %ae44862)
store volatile %struct.ScmObj* %anf_45bind40426, %struct.ScmObj** %stackaddr$prim54100, align 8
%ae44864 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54101 = alloca %struct.ScmObj*, align 8
%anf_45bind40427 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %pred40233, %struct.ScmObj* %ae44864)
store volatile %struct.ScmObj* %anf_45bind40427, %struct.ScmObj** %stackaddr$prim54101, align 8
%ae44866 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54102 = alloca %struct.ScmObj*, align 8
%anf_45bind40428 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %pred40233, %struct.ScmObj* %ae44866)
store volatile %struct.ScmObj* %anf_45bind40428, %struct.ScmObj** %stackaddr$prim54102, align 8
%stackaddr$makeclosure54103 = alloca %struct.ScmObj*, align 8
%fptrToInt54104 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44868 to i64
%ae44868 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54104)
store volatile %struct.ScmObj* %ae44868, %struct.ScmObj** %stackaddr$makeclosure54103, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44868, %struct.ScmObj* %c40241, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44868, %struct.ScmObj* %k40492, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44868, %struct.ScmObj* %anf_45bind40427, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44868, %struct.ScmObj* %anf_45bind40426, i64 3)
%args52728$anf_45bind40428$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54105 = alloca %struct.ScmObj*, align 8
%args52728$anf_45bind40428$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x40239, %struct.ScmObj* %args52728$anf_45bind40428$0)
store volatile %struct.ScmObj* %args52728$anf_45bind40428$1, %struct.ScmObj** %stackaddr$prim54105, align 8
%stackaddr$prim54106 = alloca %struct.ScmObj*, align 8
%args52728$anf_45bind40428$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44868, %struct.ScmObj* %args52728$anf_45bind40428$1)
store volatile %struct.ScmObj* %args52728$anf_45bind40428$2, %struct.ScmObj** %stackaddr$prim54106, align 8
%clofunc54107 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40428)
musttail call tailcc void %clofunc54107(%struct.ScmObj* %anf_45bind40428, %struct.ScmObj* %args52728$anf_45bind40428$2)
ret void
}

define tailcc void @proc_clo$ae44868(%struct.ScmObj* %env$ae44868,%struct.ScmObj* %current_45args52720) {
%stackaddr$env-ref54108 = alloca %struct.ScmObj*, align 8
%c40241 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44868, i64 0)
store %struct.ScmObj* %c40241, %struct.ScmObj** %stackaddr$env-ref54108
%stackaddr$env-ref54109 = alloca %struct.ScmObj*, align 8
%k40492 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44868, i64 1)
store %struct.ScmObj* %k40492, %struct.ScmObj** %stackaddr$env-ref54109
%stackaddr$env-ref54110 = alloca %struct.ScmObj*, align 8
%anf_45bind40427 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44868, i64 2)
store %struct.ScmObj* %anf_45bind40427, %struct.ScmObj** %stackaddr$env-ref54110
%stackaddr$env-ref54111 = alloca %struct.ScmObj*, align 8
%anf_45bind40426 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44868, i64 3)
store %struct.ScmObj* %anf_45bind40426, %struct.ScmObj** %stackaddr$env-ref54111
%stackaddr$prim54112 = alloca %struct.ScmObj*, align 8
%_95k40493 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52720)
store volatile %struct.ScmObj* %_95k40493, %struct.ScmObj** %stackaddr$prim54112, align 8
%stackaddr$prim54113 = alloca %struct.ScmObj*, align 8
%current_45args52721 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52720)
store volatile %struct.ScmObj* %current_45args52721, %struct.ScmObj** %stackaddr$prim54113, align 8
%stackaddr$prim54114 = alloca %struct.ScmObj*, align 8
%anf_45bind40429 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52721)
store volatile %struct.ScmObj* %anf_45bind40429, %struct.ScmObj** %stackaddr$prim54114, align 8
%stackaddr$makeclosure54115 = alloca %struct.ScmObj*, align 8
%fptrToInt54116 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44871 to i64
%ae44871 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54116)
store volatile %struct.ScmObj* %ae44871, %struct.ScmObj** %stackaddr$makeclosure54115, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44871, %struct.ScmObj* %c40241, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44871, %struct.ScmObj* %k40492, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44871, %struct.ScmObj* %anf_45bind40426, i64 2)
%args52727$anf_45bind40427$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54117 = alloca %struct.ScmObj*, align 8
%args52727$anf_45bind40427$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40429, %struct.ScmObj* %args52727$anf_45bind40427$0)
store volatile %struct.ScmObj* %args52727$anf_45bind40427$1, %struct.ScmObj** %stackaddr$prim54117, align 8
%stackaddr$prim54118 = alloca %struct.ScmObj*, align 8
%args52727$anf_45bind40427$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44871, %struct.ScmObj* %args52727$anf_45bind40427$1)
store volatile %struct.ScmObj* %args52727$anf_45bind40427$2, %struct.ScmObj** %stackaddr$prim54118, align 8
%clofunc54119 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40427)
musttail call tailcc void %clofunc54119(%struct.ScmObj* %anf_45bind40427, %struct.ScmObj* %args52727$anf_45bind40427$2)
ret void
}

define tailcc void @proc_clo$ae44871(%struct.ScmObj* %env$ae44871,%struct.ScmObj* %current_45args52723) {
%stackaddr$env-ref54120 = alloca %struct.ScmObj*, align 8
%c40241 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44871, i64 0)
store %struct.ScmObj* %c40241, %struct.ScmObj** %stackaddr$env-ref54120
%stackaddr$env-ref54121 = alloca %struct.ScmObj*, align 8
%k40492 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44871, i64 1)
store %struct.ScmObj* %k40492, %struct.ScmObj** %stackaddr$env-ref54121
%stackaddr$env-ref54122 = alloca %struct.ScmObj*, align 8
%anf_45bind40426 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44871, i64 2)
store %struct.ScmObj* %anf_45bind40426, %struct.ScmObj** %stackaddr$env-ref54122
%stackaddr$prim54123 = alloca %struct.ScmObj*, align 8
%_95k40494 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52723)
store volatile %struct.ScmObj* %_95k40494, %struct.ScmObj** %stackaddr$prim54123, align 8
%stackaddr$prim54124 = alloca %struct.ScmObj*, align 8
%current_45args52724 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52723)
store volatile %struct.ScmObj* %current_45args52724, %struct.ScmObj** %stackaddr$prim54124, align 8
%stackaddr$prim54125 = alloca %struct.ScmObj*, align 8
%anf_45bind40430 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52724)
store volatile %struct.ScmObj* %anf_45bind40430, %struct.ScmObj** %stackaddr$prim54125, align 8
%args52726$anf_45bind40426$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54126 = alloca %struct.ScmObj*, align 8
%args52726$anf_45bind40426$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %c40241, %struct.ScmObj* %args52726$anf_45bind40426$0)
store volatile %struct.ScmObj* %args52726$anf_45bind40426$1, %struct.ScmObj** %stackaddr$prim54126, align 8
%stackaddr$prim54127 = alloca %struct.ScmObj*, align 8
%args52726$anf_45bind40426$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40430, %struct.ScmObj* %args52726$anf_45bind40426$1)
store volatile %struct.ScmObj* %args52726$anf_45bind40426$2, %struct.ScmObj** %stackaddr$prim54127, align 8
%stackaddr$prim54128 = alloca %struct.ScmObj*, align 8
%args52726$anf_45bind40426$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40492, %struct.ScmObj* %args52726$anf_45bind40426$2)
store volatile %struct.ScmObj* %args52726$anf_45bind40426$3, %struct.ScmObj** %stackaddr$prim54128, align 8
%clofunc54129 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40426)
musttail call tailcc void %clofunc54129(%struct.ScmObj* %anf_45bind40426, %struct.ScmObj* %args52726$anf_45bind40426$3)
ret void
}

define tailcc void @proc_clo$ae44803(%struct.ScmObj* %env$ae44803,%struct.ScmObj* %current_45args52731) {
%stackaddr$env-ref54130 = alloca %struct.ScmObj*, align 8
%pred40233 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44803, i64 0)
store %struct.ScmObj* %pred40233, %struct.ScmObj** %stackaddr$env-ref54130
%stackaddr$env-ref54131 = alloca %struct.ScmObj*, align 8
%fibc40230 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44803, i64 1)
store %struct.ScmObj* %fibc40230, %struct.ScmObj** %stackaddr$env-ref54131
%stackaddr$env-ref54132 = alloca %struct.ScmObj*, align 8
%x40239 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44803, i64 2)
store %struct.ScmObj* %x40239, %struct.ScmObj** %stackaddr$env-ref54132
%stackaddr$prim54133 = alloca %struct.ScmObj*, align 8
%k40495 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52731)
store volatile %struct.ScmObj* %k40495, %struct.ScmObj** %stackaddr$prim54133, align 8
%stackaddr$prim54134 = alloca %struct.ScmObj*, align 8
%current_45args52732 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52731)
store volatile %struct.ScmObj* %current_45args52732, %struct.ScmObj** %stackaddr$prim54134, align 8
%stackaddr$prim54135 = alloca %struct.ScmObj*, align 8
%c40240 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52732)
store volatile %struct.ScmObj* %c40240, %struct.ScmObj** %stackaddr$prim54135, align 8
%ae44805 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54136 = alloca %struct.ScmObj*, align 8
%anf_45bind40421 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %fibc40230, %struct.ScmObj* %ae44805)
store volatile %struct.ScmObj* %anf_45bind40421, %struct.ScmObj** %stackaddr$prim54136, align 8
%ae44807 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54137 = alloca %struct.ScmObj*, align 8
%anf_45bind40422 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %pred40233, %struct.ScmObj* %ae44807)
store volatile %struct.ScmObj* %anf_45bind40422, %struct.ScmObj** %stackaddr$prim54137, align 8
%stackaddr$makeclosure54138 = alloca %struct.ScmObj*, align 8
%fptrToInt54139 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44809 to i64
%ae44809 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54139)
store volatile %struct.ScmObj* %ae44809, %struct.ScmObj** %stackaddr$makeclosure54138, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44809, %struct.ScmObj* %anf_45bind40421, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44809, %struct.ScmObj* %c40240, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44809, %struct.ScmObj* %k40495, i64 2)
%args52738$anf_45bind40422$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54140 = alloca %struct.ScmObj*, align 8
%args52738$anf_45bind40422$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x40239, %struct.ScmObj* %args52738$anf_45bind40422$0)
store volatile %struct.ScmObj* %args52738$anf_45bind40422$1, %struct.ScmObj** %stackaddr$prim54140, align 8
%stackaddr$prim54141 = alloca %struct.ScmObj*, align 8
%args52738$anf_45bind40422$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44809, %struct.ScmObj* %args52738$anf_45bind40422$1)
store volatile %struct.ScmObj* %args52738$anf_45bind40422$2, %struct.ScmObj** %stackaddr$prim54141, align 8
%clofunc54142 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40422)
musttail call tailcc void %clofunc54142(%struct.ScmObj* %anf_45bind40422, %struct.ScmObj* %args52738$anf_45bind40422$2)
ret void
}

define tailcc void @proc_clo$ae44809(%struct.ScmObj* %env$ae44809,%struct.ScmObj* %current_45args52734) {
%stackaddr$env-ref54143 = alloca %struct.ScmObj*, align 8
%anf_45bind40421 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44809, i64 0)
store %struct.ScmObj* %anf_45bind40421, %struct.ScmObj** %stackaddr$env-ref54143
%stackaddr$env-ref54144 = alloca %struct.ScmObj*, align 8
%c40240 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44809, i64 1)
store %struct.ScmObj* %c40240, %struct.ScmObj** %stackaddr$env-ref54144
%stackaddr$env-ref54145 = alloca %struct.ScmObj*, align 8
%k40495 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44809, i64 2)
store %struct.ScmObj* %k40495, %struct.ScmObj** %stackaddr$env-ref54145
%stackaddr$prim54146 = alloca %struct.ScmObj*, align 8
%_95k40496 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52734)
store volatile %struct.ScmObj* %_95k40496, %struct.ScmObj** %stackaddr$prim54146, align 8
%stackaddr$prim54147 = alloca %struct.ScmObj*, align 8
%current_45args52735 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52734)
store volatile %struct.ScmObj* %current_45args52735, %struct.ScmObj** %stackaddr$prim54147, align 8
%stackaddr$prim54148 = alloca %struct.ScmObj*, align 8
%anf_45bind40423 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52735)
store volatile %struct.ScmObj* %anf_45bind40423, %struct.ScmObj** %stackaddr$prim54148, align 8
%args52737$anf_45bind40421$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54149 = alloca %struct.ScmObj*, align 8
%args52737$anf_45bind40421$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %c40240, %struct.ScmObj* %args52737$anf_45bind40421$0)
store volatile %struct.ScmObj* %args52737$anf_45bind40421$1, %struct.ScmObj** %stackaddr$prim54149, align 8
%stackaddr$prim54150 = alloca %struct.ScmObj*, align 8
%args52737$anf_45bind40421$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40423, %struct.ScmObj* %args52737$anf_45bind40421$1)
store volatile %struct.ScmObj* %args52737$anf_45bind40421$2, %struct.ScmObj** %stackaddr$prim54150, align 8
%stackaddr$prim54151 = alloca %struct.ScmObj*, align 8
%args52737$anf_45bind40421$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40495, %struct.ScmObj* %args52737$anf_45bind40421$2)
store volatile %struct.ScmObj* %args52737$anf_45bind40421$3, %struct.ScmObj** %stackaddr$prim54151, align 8
%clofunc54152 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40421)
musttail call tailcc void %clofunc54152(%struct.ScmObj* %anf_45bind40421, %struct.ScmObj* %args52737$anf_45bind40421$3)
ret void
}

define tailcc void @proc_clo$ae44596(%struct.ScmObj* %env$ae44596,%struct.ScmObj* %current_45args52744) {
%stackaddr$env-ref54153 = alloca %struct.ScmObj*, align 8
%pred40233 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44596, i64 0)
store %struct.ScmObj* %pred40233, %struct.ScmObj** %stackaddr$env-ref54153
%stackaddr$env-ref54154 = alloca %struct.ScmObj*, align 8
%z_6340232 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44596, i64 1)
store %struct.ScmObj* %z_6340232, %struct.ScmObj** %stackaddr$env-ref54154
%stackaddr$env-ref54155 = alloca %struct.ScmObj*, align 8
%addc40231 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44596, i64 2)
store %struct.ScmObj* %addc40231, %struct.ScmObj** %stackaddr$env-ref54155
%stackaddr$env-ref54156 = alloca %struct.ScmObj*, align 8
%succ40234 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44596, i64 3)
store %struct.ScmObj* %succ40234, %struct.ScmObj** %stackaddr$env-ref54156
%stackaddr$prim54157 = alloca %struct.ScmObj*, align 8
%k40497 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52744)
store volatile %struct.ScmObj* %k40497, %struct.ScmObj** %stackaddr$prim54157, align 8
%stackaddr$prim54158 = alloca %struct.ScmObj*, align 8
%current_45args52745 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52744)
store volatile %struct.ScmObj* %current_45args52745, %struct.ScmObj** %stackaddr$prim54158, align 8
%stackaddr$prim54159 = alloca %struct.ScmObj*, align 8
%x40245 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52745)
store volatile %struct.ScmObj* %x40245, %struct.ScmObj** %stackaddr$prim54159, align 8
%stackaddr$prim54160 = alloca %struct.ScmObj*, align 8
%current_45args52746 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52745)
store volatile %struct.ScmObj* %current_45args52746, %struct.ScmObj** %stackaddr$prim54160, align 8
%stackaddr$prim54161 = alloca %struct.ScmObj*, align 8
%y40244 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52746)
store volatile %struct.ScmObj* %y40244, %struct.ScmObj** %stackaddr$prim54161, align 8
%stackaddr$prim54162 = alloca %struct.ScmObj*, align 8
%current_45args52747 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52746)
store volatile %struct.ScmObj* %current_45args52747, %struct.ScmObj** %stackaddr$prim54162, align 8
%stackaddr$prim54163 = alloca %struct.ScmObj*, align 8
%k40243 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52747)
store volatile %struct.ScmObj* %k40243, %struct.ScmObj** %stackaddr$prim54163, align 8
%ae44598 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54164 = alloca %struct.ScmObj*, align 8
%anf_45bind40402 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %z_6340232, %struct.ScmObj* %ae44598)
store volatile %struct.ScmObj* %anf_45bind40402, %struct.ScmObj** %stackaddr$prim54164, align 8
%stackaddr$makeclosure54165 = alloca %struct.ScmObj*, align 8
%fptrToInt54166 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44600 to i64
%ae44600 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt54166)
store volatile %struct.ScmObj* %ae44600, %struct.ScmObj** %stackaddr$makeclosure54165, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44600, %struct.ScmObj* %pred40233, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44600, %struct.ScmObj* %addc40231, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44600, %struct.ScmObj* %x40245, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44600, %struct.ScmObj* %y40244, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44600, %struct.ScmObj* %k40243, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44600, %struct.ScmObj* %k40497, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae44600, %struct.ScmObj* %succ40234, i64 6)
%args52762$anf_45bind40402$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54167 = alloca %struct.ScmObj*, align 8
%args52762$anf_45bind40402$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y40244, %struct.ScmObj* %args52762$anf_45bind40402$0)
store volatile %struct.ScmObj* %args52762$anf_45bind40402$1, %struct.ScmObj** %stackaddr$prim54167, align 8
%stackaddr$prim54168 = alloca %struct.ScmObj*, align 8
%args52762$anf_45bind40402$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44600, %struct.ScmObj* %args52762$anf_45bind40402$1)
store volatile %struct.ScmObj* %args52762$anf_45bind40402$2, %struct.ScmObj** %stackaddr$prim54168, align 8
%clofunc54169 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40402)
musttail call tailcc void %clofunc54169(%struct.ScmObj* %anf_45bind40402, %struct.ScmObj* %args52762$anf_45bind40402$2)
ret void
}

define tailcc void @proc_clo$ae44600(%struct.ScmObj* %env$ae44600,%struct.ScmObj* %current_45args52749) {
%stackaddr$env-ref54170 = alloca %struct.ScmObj*, align 8
%pred40233 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44600, i64 0)
store %struct.ScmObj* %pred40233, %struct.ScmObj** %stackaddr$env-ref54170
%stackaddr$env-ref54171 = alloca %struct.ScmObj*, align 8
%addc40231 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44600, i64 1)
store %struct.ScmObj* %addc40231, %struct.ScmObj** %stackaddr$env-ref54171
%stackaddr$env-ref54172 = alloca %struct.ScmObj*, align 8
%x40245 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44600, i64 2)
store %struct.ScmObj* %x40245, %struct.ScmObj** %stackaddr$env-ref54172
%stackaddr$env-ref54173 = alloca %struct.ScmObj*, align 8
%y40244 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44600, i64 3)
store %struct.ScmObj* %y40244, %struct.ScmObj** %stackaddr$env-ref54173
%stackaddr$env-ref54174 = alloca %struct.ScmObj*, align 8
%k40243 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44600, i64 4)
store %struct.ScmObj* %k40243, %struct.ScmObj** %stackaddr$env-ref54174
%stackaddr$env-ref54175 = alloca %struct.ScmObj*, align 8
%k40497 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44600, i64 5)
store %struct.ScmObj* %k40497, %struct.ScmObj** %stackaddr$env-ref54175
%stackaddr$env-ref54176 = alloca %struct.ScmObj*, align 8
%succ40234 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44600, i64 6)
store %struct.ScmObj* %succ40234, %struct.ScmObj** %stackaddr$env-ref54176
%stackaddr$prim54177 = alloca %struct.ScmObj*, align 8
%_95k40498 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52749)
store volatile %struct.ScmObj* %_95k40498, %struct.ScmObj** %stackaddr$prim54177, align 8
%stackaddr$prim54178 = alloca %struct.ScmObj*, align 8
%current_45args52750 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52749)
store volatile %struct.ScmObj* %current_45args52750, %struct.ScmObj** %stackaddr$prim54178, align 8
%stackaddr$prim54179 = alloca %struct.ScmObj*, align 8
%anf_45bind40403 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52750)
store volatile %struct.ScmObj* %anf_45bind40403, %struct.ScmObj** %stackaddr$prim54179, align 8
%truthy$cmp54180 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40403)
%cmp$cmp54180 = icmp eq i64 %truthy$cmp54180, 1
br i1 %cmp$cmp54180, label %truebranch$cmp54180, label %falsebranch$cmp54180
truebranch$cmp54180:
%args52752$k40243$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54181 = alloca %struct.ScmObj*, align 8
%args52752$k40243$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x40245, %struct.ScmObj* %args52752$k40243$0)
store volatile %struct.ScmObj* %args52752$k40243$1, %struct.ScmObj** %stackaddr$prim54181, align 8
%stackaddr$prim54182 = alloca %struct.ScmObj*, align 8
%args52752$k40243$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40497, %struct.ScmObj* %args52752$k40243$1)
store volatile %struct.ScmObj* %args52752$k40243$2, %struct.ScmObj** %stackaddr$prim54182, align 8
%clofunc54183 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40243)
musttail call tailcc void %clofunc54183(%struct.ScmObj* %k40243, %struct.ScmObj* %args52752$k40243$2)
ret void
falsebranch$cmp54180:
%ae44607 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54184 = alloca %struct.ScmObj*, align 8
%anf_45bind40404 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %addc40231, %struct.ScmObj* %ae44607)
store volatile %struct.ScmObj* %anf_45bind40404, %struct.ScmObj** %stackaddr$prim54184, align 8
%ae44609 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54185 = alloca %struct.ScmObj*, align 8
%anf_45bind40405 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %succ40234, %struct.ScmObj* %ae44609)
store volatile %struct.ScmObj* %anf_45bind40405, %struct.ScmObj** %stackaddr$prim54185, align 8
%stackaddr$makeclosure54186 = alloca %struct.ScmObj*, align 8
%fptrToInt54187 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44611 to i64
%ae44611 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54187)
store volatile %struct.ScmObj* %ae44611, %struct.ScmObj** %stackaddr$makeclosure54186, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44611, %struct.ScmObj* %y40244, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44611, %struct.ScmObj* %anf_45bind40404, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44611, %struct.ScmObj* %pred40233, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44611, %struct.ScmObj* %k40243, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44611, %struct.ScmObj* %k40497, i64 4)
%args52761$anf_45bind40405$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54188 = alloca %struct.ScmObj*, align 8
%args52761$anf_45bind40405$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x40245, %struct.ScmObj* %args52761$anf_45bind40405$0)
store volatile %struct.ScmObj* %args52761$anf_45bind40405$1, %struct.ScmObj** %stackaddr$prim54188, align 8
%stackaddr$prim54189 = alloca %struct.ScmObj*, align 8
%args52761$anf_45bind40405$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44611, %struct.ScmObj* %args52761$anf_45bind40405$1)
store volatile %struct.ScmObj* %args52761$anf_45bind40405$2, %struct.ScmObj** %stackaddr$prim54189, align 8
%clofunc54190 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40405)
musttail call tailcc void %clofunc54190(%struct.ScmObj* %anf_45bind40405, %struct.ScmObj* %args52761$anf_45bind40405$2)
ret void
}

define tailcc void @proc_clo$ae44611(%struct.ScmObj* %env$ae44611,%struct.ScmObj* %current_45args52753) {
%stackaddr$env-ref54191 = alloca %struct.ScmObj*, align 8
%y40244 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44611, i64 0)
store %struct.ScmObj* %y40244, %struct.ScmObj** %stackaddr$env-ref54191
%stackaddr$env-ref54192 = alloca %struct.ScmObj*, align 8
%anf_45bind40404 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44611, i64 1)
store %struct.ScmObj* %anf_45bind40404, %struct.ScmObj** %stackaddr$env-ref54192
%stackaddr$env-ref54193 = alloca %struct.ScmObj*, align 8
%pred40233 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44611, i64 2)
store %struct.ScmObj* %pred40233, %struct.ScmObj** %stackaddr$env-ref54193
%stackaddr$env-ref54194 = alloca %struct.ScmObj*, align 8
%k40243 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44611, i64 3)
store %struct.ScmObj* %k40243, %struct.ScmObj** %stackaddr$env-ref54194
%stackaddr$env-ref54195 = alloca %struct.ScmObj*, align 8
%k40497 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44611, i64 4)
store %struct.ScmObj* %k40497, %struct.ScmObj** %stackaddr$env-ref54195
%stackaddr$prim54196 = alloca %struct.ScmObj*, align 8
%_95k40499 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52753)
store volatile %struct.ScmObj* %_95k40499, %struct.ScmObj** %stackaddr$prim54196, align 8
%stackaddr$prim54197 = alloca %struct.ScmObj*, align 8
%current_45args52754 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52753)
store volatile %struct.ScmObj* %current_45args52754, %struct.ScmObj** %stackaddr$prim54197, align 8
%stackaddr$prim54198 = alloca %struct.ScmObj*, align 8
%anf_45bind40406 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52754)
store volatile %struct.ScmObj* %anf_45bind40406, %struct.ScmObj** %stackaddr$prim54198, align 8
%ae44614 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54199 = alloca %struct.ScmObj*, align 8
%anf_45bind40407 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %pred40233, %struct.ScmObj* %ae44614)
store volatile %struct.ScmObj* %anf_45bind40407, %struct.ScmObj** %stackaddr$prim54199, align 8
%stackaddr$makeclosure54200 = alloca %struct.ScmObj*, align 8
%fptrToInt54201 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44616 to i64
%ae44616 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54201)
store volatile %struct.ScmObj* %ae44616, %struct.ScmObj** %stackaddr$makeclosure54200, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44616, %struct.ScmObj* %anf_45bind40406, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44616, %struct.ScmObj* %anf_45bind40404, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44616, %struct.ScmObj* %k40243, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44616, %struct.ScmObj* %k40497, i64 3)
%args52760$anf_45bind40407$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54202 = alloca %struct.ScmObj*, align 8
%args52760$anf_45bind40407$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y40244, %struct.ScmObj* %args52760$anf_45bind40407$0)
store volatile %struct.ScmObj* %args52760$anf_45bind40407$1, %struct.ScmObj** %stackaddr$prim54202, align 8
%stackaddr$prim54203 = alloca %struct.ScmObj*, align 8
%args52760$anf_45bind40407$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44616, %struct.ScmObj* %args52760$anf_45bind40407$1)
store volatile %struct.ScmObj* %args52760$anf_45bind40407$2, %struct.ScmObj** %stackaddr$prim54203, align 8
%clofunc54204 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40407)
musttail call tailcc void %clofunc54204(%struct.ScmObj* %anf_45bind40407, %struct.ScmObj* %args52760$anf_45bind40407$2)
ret void
}

define tailcc void @proc_clo$ae44616(%struct.ScmObj* %env$ae44616,%struct.ScmObj* %current_45args52756) {
%stackaddr$env-ref54205 = alloca %struct.ScmObj*, align 8
%anf_45bind40406 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44616, i64 0)
store %struct.ScmObj* %anf_45bind40406, %struct.ScmObj** %stackaddr$env-ref54205
%stackaddr$env-ref54206 = alloca %struct.ScmObj*, align 8
%anf_45bind40404 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44616, i64 1)
store %struct.ScmObj* %anf_45bind40404, %struct.ScmObj** %stackaddr$env-ref54206
%stackaddr$env-ref54207 = alloca %struct.ScmObj*, align 8
%k40243 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44616, i64 2)
store %struct.ScmObj* %k40243, %struct.ScmObj** %stackaddr$env-ref54207
%stackaddr$env-ref54208 = alloca %struct.ScmObj*, align 8
%k40497 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44616, i64 3)
store %struct.ScmObj* %k40497, %struct.ScmObj** %stackaddr$env-ref54208
%stackaddr$prim54209 = alloca %struct.ScmObj*, align 8
%_95k40500 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52756)
store volatile %struct.ScmObj* %_95k40500, %struct.ScmObj** %stackaddr$prim54209, align 8
%stackaddr$prim54210 = alloca %struct.ScmObj*, align 8
%current_45args52757 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52756)
store volatile %struct.ScmObj* %current_45args52757, %struct.ScmObj** %stackaddr$prim54210, align 8
%stackaddr$prim54211 = alloca %struct.ScmObj*, align 8
%anf_45bind40408 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52757)
store volatile %struct.ScmObj* %anf_45bind40408, %struct.ScmObj** %stackaddr$prim54211, align 8
%args52759$anf_45bind40404$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54212 = alloca %struct.ScmObj*, align 8
%args52759$anf_45bind40404$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40243, %struct.ScmObj* %args52759$anf_45bind40404$0)
store volatile %struct.ScmObj* %args52759$anf_45bind40404$1, %struct.ScmObj** %stackaddr$prim54212, align 8
%stackaddr$prim54213 = alloca %struct.ScmObj*, align 8
%args52759$anf_45bind40404$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40408, %struct.ScmObj* %args52759$anf_45bind40404$1)
store volatile %struct.ScmObj* %args52759$anf_45bind40404$2, %struct.ScmObj** %stackaddr$prim54213, align 8
%stackaddr$prim54214 = alloca %struct.ScmObj*, align 8
%args52759$anf_45bind40404$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40406, %struct.ScmObj* %args52759$anf_45bind40404$2)
store volatile %struct.ScmObj* %args52759$anf_45bind40404$3, %struct.ScmObj** %stackaddr$prim54214, align 8
%stackaddr$prim54215 = alloca %struct.ScmObj*, align 8
%args52759$anf_45bind40404$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40497, %struct.ScmObj* %args52759$anf_45bind40404$3)
store volatile %struct.ScmObj* %args52759$anf_45bind40404$4, %struct.ScmObj** %stackaddr$prim54215, align 8
%clofunc54216 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40404)
musttail call tailcc void %clofunc54216(%struct.ScmObj* %anf_45bind40404, %struct.ScmObj* %args52759$anf_45bind40404$4)
ret void
}

define tailcc void @proc_clo$ae44573(%struct.ScmObj* %env$ae44573,%struct.ScmObj* %current_45args52764) {
%stackaddr$prim54217 = alloca %struct.ScmObj*, align 8
%k40501 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52764)
store volatile %struct.ScmObj* %k40501, %struct.ScmObj** %stackaddr$prim54217, align 8
%stackaddr$prim54218 = alloca %struct.ScmObj*, align 8
%current_45args52765 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52764)
store volatile %struct.ScmObj* %current_45args52765, %struct.ScmObj** %stackaddr$prim54218, align 8
%stackaddr$prim54219 = alloca %struct.ScmObj*, align 8
%n40247 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52765)
store volatile %struct.ScmObj* %n40247, %struct.ScmObj** %stackaddr$prim54219, align 8
%stackaddr$prim54220 = alloca %struct.ScmObj*, align 8
%cpsprim40502 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %n40247)
store volatile %struct.ScmObj* %cpsprim40502, %struct.ScmObj** %stackaddr$prim54220, align 8
%ae44576 = call %struct.ScmObj* @const_init_int(i64 0)
%args52767$k40501$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54221 = alloca %struct.ScmObj*, align 8
%args52767$k40501$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40502, %struct.ScmObj* %args52767$k40501$0)
store volatile %struct.ScmObj* %args52767$k40501$1, %struct.ScmObj** %stackaddr$prim54221, align 8
%stackaddr$prim54222 = alloca %struct.ScmObj*, align 8
%args52767$k40501$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44576, %struct.ScmObj* %args52767$k40501$1)
store volatile %struct.ScmObj* %args52767$k40501$2, %struct.ScmObj** %stackaddr$prim54222, align 8
%clofunc54223 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40501)
musttail call tailcc void %clofunc54223(%struct.ScmObj* %k40501, %struct.ScmObj* %args52767$k40501$2)
ret void
}

define tailcc void @proc_clo$ae44550(%struct.ScmObj* %env$ae44550,%struct.ScmObj* %current_45args52769) {
%stackaddr$prim54224 = alloca %struct.ScmObj*, align 8
%k40503 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52769)
store volatile %struct.ScmObj* %k40503, %struct.ScmObj** %stackaddr$prim54224, align 8
%stackaddr$prim54225 = alloca %struct.ScmObj*, align 8
%current_45args52770 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52769)
store volatile %struct.ScmObj* %current_45args52770, %struct.ScmObj** %stackaddr$prim54225, align 8
%stackaddr$prim54226 = alloca %struct.ScmObj*, align 8
%x4010640249 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52770)
store volatile %struct.ScmObj* %x4010640249, %struct.ScmObj** %stackaddr$prim54226, align 8
%stackaddr$prim54227 = alloca %struct.ScmObj*, align 8
%cpsprim40504 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x4010640249)
store volatile %struct.ScmObj* %cpsprim40504, %struct.ScmObj** %stackaddr$prim54227, align 8
%ae44553 = call %struct.ScmObj* @const_init_int(i64 0)
%args52772$k40503$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54228 = alloca %struct.ScmObj*, align 8
%args52772$k40503$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40504, %struct.ScmObj* %args52772$k40503$0)
store volatile %struct.ScmObj* %args52772$k40503$1, %struct.ScmObj** %stackaddr$prim54228, align 8
%stackaddr$prim54229 = alloca %struct.ScmObj*, align 8
%args52772$k40503$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44553, %struct.ScmObj* %args52772$k40503$1)
store volatile %struct.ScmObj* %args52772$k40503$2, %struct.ScmObj** %stackaddr$prim54229, align 8
%clofunc54230 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40503)
musttail call tailcc void %clofunc54230(%struct.ScmObj* %k40503, %struct.ScmObj* %args52772$k40503$2)
ret void
}

define tailcc void @proc_clo$ae44481(%struct.ScmObj* %env$ae44481,%struct.ScmObj* %current_45args52774) {
%stackaddr$prim54231 = alloca %struct.ScmObj*, align 8
%k40505 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52774)
store volatile %struct.ScmObj* %k40505, %struct.ScmObj** %stackaddr$prim54231, align 8
%stackaddr$prim54232 = alloca %struct.ScmObj*, align 8
%current_45args52775 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52774)
store volatile %struct.ScmObj* %current_45args52775, %struct.ScmObj** %stackaddr$prim54232, align 8
%stackaddr$prim54233 = alloca %struct.ScmObj*, align 8
%n40251 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52775)
store volatile %struct.ScmObj* %n40251, %struct.ScmObj** %stackaddr$prim54233, align 8
%stackaddr$makeclosure54234 = alloca %struct.ScmObj*, align 8
%fptrToInt54235 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44482 to i64
%ae44482 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54235)
store volatile %struct.ScmObj* %ae44482, %struct.ScmObj** %stackaddr$makeclosure54234, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44482, %struct.ScmObj* %k40505, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44482, %struct.ScmObj* %n40251, i64 1)
%ae44483 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54236 = alloca %struct.ScmObj*, align 8
%fptrToInt54237 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44484 to i64
%ae44484 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54237)
store volatile %struct.ScmObj* %ae44484, %struct.ScmObj** %stackaddr$makeclosure54236, align 8
%args52786$ae44482$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54238 = alloca %struct.ScmObj*, align 8
%args52786$ae44482$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44484, %struct.ScmObj* %args52786$ae44482$0)
store volatile %struct.ScmObj* %args52786$ae44482$1, %struct.ScmObj** %stackaddr$prim54238, align 8
%stackaddr$prim54239 = alloca %struct.ScmObj*, align 8
%args52786$ae44482$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44483, %struct.ScmObj* %args52786$ae44482$1)
store volatile %struct.ScmObj* %args52786$ae44482$2, %struct.ScmObj** %stackaddr$prim54239, align 8
%clofunc54240 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44482)
musttail call tailcc void %clofunc54240(%struct.ScmObj* %ae44482, %struct.ScmObj* %args52786$ae44482$2)
ret void
}

define tailcc void @proc_clo$ae44482(%struct.ScmObj* %env$ae44482,%struct.ScmObj* %current_45args52777) {
%stackaddr$env-ref54241 = alloca %struct.ScmObj*, align 8
%k40505 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44482, i64 0)
store %struct.ScmObj* %k40505, %struct.ScmObj** %stackaddr$env-ref54241
%stackaddr$env-ref54242 = alloca %struct.ScmObj*, align 8
%n40251 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44482, i64 1)
store %struct.ScmObj* %n40251, %struct.ScmObj** %stackaddr$env-ref54242
%stackaddr$prim54243 = alloca %struct.ScmObj*, align 8
%_95k40506 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52777)
store volatile %struct.ScmObj* %_95k40506, %struct.ScmObj** %stackaddr$prim54243, align 8
%stackaddr$prim54244 = alloca %struct.ScmObj*, align 8
%current_45args52778 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52777)
store volatile %struct.ScmObj* %current_45args52778, %struct.ScmObj** %stackaddr$prim54244, align 8
%stackaddr$prim54245 = alloca %struct.ScmObj*, align 8
%anf_45bind40397 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52778)
store volatile %struct.ScmObj* %anf_45bind40397, %struct.ScmObj** %stackaddr$prim54245, align 8
%stackaddr$makeclosure54246 = alloca %struct.ScmObj*, align 8
%fptrToInt54247 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44505 to i64
%ae44505 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54247)
store volatile %struct.ScmObj* %ae44505, %struct.ScmObj** %stackaddr$makeclosure54246, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44505, %struct.ScmObj* %k40505, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44505, %struct.ScmObj* %n40251, i64 1)
%args52784$anf_45bind40397$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54248 = alloca %struct.ScmObj*, align 8
%args52784$anf_45bind40397$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44505, %struct.ScmObj* %args52784$anf_45bind40397$0)
store volatile %struct.ScmObj* %args52784$anf_45bind40397$1, %struct.ScmObj** %stackaddr$prim54248, align 8
%clofunc54249 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40397)
musttail call tailcc void %clofunc54249(%struct.ScmObj* %anf_45bind40397, %struct.ScmObj* %args52784$anf_45bind40397$1)
ret void
}

define tailcc void @proc_clo$ae44505(%struct.ScmObj* %env$ae44505,%struct.ScmObj* %current_45args52780) {
%stackaddr$env-ref54250 = alloca %struct.ScmObj*, align 8
%k40505 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44505, i64 0)
store %struct.ScmObj* %k40505, %struct.ScmObj** %stackaddr$env-ref54250
%stackaddr$env-ref54251 = alloca %struct.ScmObj*, align 8
%n40251 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44505, i64 1)
store %struct.ScmObj* %n40251, %struct.ScmObj** %stackaddr$env-ref54251
%stackaddr$prim54252 = alloca %struct.ScmObj*, align 8
%_95k40507 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52780)
store volatile %struct.ScmObj* %_95k40507, %struct.ScmObj** %stackaddr$prim54252, align 8
%stackaddr$prim54253 = alloca %struct.ScmObj*, align 8
%current_45args52781 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52780)
store volatile %struct.ScmObj* %current_45args52781, %struct.ScmObj** %stackaddr$prim54253, align 8
%stackaddr$prim54254 = alloca %struct.ScmObj*, align 8
%anf_45bind40398 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52781)
store volatile %struct.ScmObj* %anf_45bind40398, %struct.ScmObj** %stackaddr$prim54254, align 8
%stackaddr$prim54255 = alloca %struct.ScmObj*, align 8
%cpsprim40508 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %n40251, %struct.ScmObj* %anf_45bind40398)
store volatile %struct.ScmObj* %cpsprim40508, %struct.ScmObj** %stackaddr$prim54255, align 8
%ae44509 = call %struct.ScmObj* @const_init_int(i64 0)
%args52783$k40505$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54256 = alloca %struct.ScmObj*, align 8
%args52783$k40505$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40508, %struct.ScmObj* %args52783$k40505$0)
store volatile %struct.ScmObj* %args52783$k40505$1, %struct.ScmObj** %stackaddr$prim54256, align 8
%stackaddr$prim54257 = alloca %struct.ScmObj*, align 8
%args52783$k40505$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44509, %struct.ScmObj* %args52783$k40505$1)
store volatile %struct.ScmObj* %args52783$k40505$2, %struct.ScmObj** %stackaddr$prim54257, align 8
%clofunc54258 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40505)
musttail call tailcc void %clofunc54258(%struct.ScmObj* %k40505, %struct.ScmObj* %args52783$k40505$2)
ret void
}

define tailcc void @proc_clo$ae44484(%struct.ScmObj* %env$ae44484,%struct.ScmObj* %lst4025240509) {
%stackaddr$prim54259 = alloca %struct.ScmObj*, align 8
%k40510 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4025240509)
store volatile %struct.ScmObj* %k40510, %struct.ScmObj** %stackaddr$prim54259, align 8
%stackaddr$prim54260 = alloca %struct.ScmObj*, align 8
%lst40252 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4025240509)
store volatile %struct.ScmObj* %lst40252, %struct.ScmObj** %stackaddr$prim54260, align 8
%ae44488 = call %struct.ScmObj* @const_init_int(i64 0)
%args52785$k40510$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54261 = alloca %struct.ScmObj*, align 8
%args52785$k40510$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40252, %struct.ScmObj* %args52785$k40510$0)
store volatile %struct.ScmObj* %args52785$k40510$1, %struct.ScmObj** %stackaddr$prim54261, align 8
%stackaddr$prim54262 = alloca %struct.ScmObj*, align 8
%args52785$k40510$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44488, %struct.ScmObj* %args52785$k40510$1)
store volatile %struct.ScmObj* %args52785$k40510$2, %struct.ScmObj** %stackaddr$prim54262, align 8
%clofunc54263 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40510)
musttail call tailcc void %clofunc54263(%struct.ScmObj* %k40510, %struct.ScmObj* %args52785$k40510$2)
ret void
}

define tailcc void @proc_clo$ae44346(%struct.ScmObj* %env$ae44346,%struct.ScmObj* %current_45args52788) {
%stackaddr$env-ref54264 = alloca %struct.ScmObj*, align 8
%pred40233 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44346, i64 0)
store %struct.ScmObj* %pred40233, %struct.ScmObj** %stackaddr$env-ref54264
%stackaddr$env-ref54265 = alloca %struct.ScmObj*, align 8
%z_6340232 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44346, i64 1)
store %struct.ScmObj* %z_6340232, %struct.ScmObj** %stackaddr$env-ref54265
%stackaddr$env-ref54266 = alloca %struct.ScmObj*, align 8
%peano_45_62nat40235 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44346, i64 2)
store %struct.ScmObj* %peano_45_62nat40235, %struct.ScmObj** %stackaddr$env-ref54266
%stackaddr$prim54267 = alloca %struct.ScmObj*, align 8
%k40511 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52788)
store volatile %struct.ScmObj* %k40511, %struct.ScmObj** %stackaddr$prim54267, align 8
%stackaddr$prim54268 = alloca %struct.ScmObj*, align 8
%current_45args52789 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52788)
store volatile %struct.ScmObj* %current_45args52789, %struct.ScmObj** %stackaddr$prim54268, align 8
%stackaddr$prim54269 = alloca %struct.ScmObj*, align 8
%n40254 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52789)
store volatile %struct.ScmObj* %n40254, %struct.ScmObj** %stackaddr$prim54269, align 8
%ae44348 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54270 = alloca %struct.ScmObj*, align 8
%anf_45bind40390 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %z_6340232, %struct.ScmObj* %ae44348)
store volatile %struct.ScmObj* %anf_45bind40390, %struct.ScmObj** %stackaddr$prim54270, align 8
%stackaddr$makeclosure54271 = alloca %struct.ScmObj*, align 8
%fptrToInt54272 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44350 to i64
%ae44350 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54272)
store volatile %struct.ScmObj* %ae44350, %struct.ScmObj** %stackaddr$makeclosure54271, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44350, %struct.ScmObj* %pred40233, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44350, %struct.ScmObj* %k40511, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44350, %struct.ScmObj* %n40254, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44350, %struct.ScmObj* %peano_45_62nat40235, i64 3)
%args52804$anf_45bind40390$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54273 = alloca %struct.ScmObj*, align 8
%args52804$anf_45bind40390$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %n40254, %struct.ScmObj* %args52804$anf_45bind40390$0)
store volatile %struct.ScmObj* %args52804$anf_45bind40390$1, %struct.ScmObj** %stackaddr$prim54273, align 8
%stackaddr$prim54274 = alloca %struct.ScmObj*, align 8
%args52804$anf_45bind40390$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44350, %struct.ScmObj* %args52804$anf_45bind40390$1)
store volatile %struct.ScmObj* %args52804$anf_45bind40390$2, %struct.ScmObj** %stackaddr$prim54274, align 8
%clofunc54275 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40390)
musttail call tailcc void %clofunc54275(%struct.ScmObj* %anf_45bind40390, %struct.ScmObj* %args52804$anf_45bind40390$2)
ret void
}

define tailcc void @proc_clo$ae44350(%struct.ScmObj* %env$ae44350,%struct.ScmObj* %current_45args52791) {
%stackaddr$env-ref54276 = alloca %struct.ScmObj*, align 8
%pred40233 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44350, i64 0)
store %struct.ScmObj* %pred40233, %struct.ScmObj** %stackaddr$env-ref54276
%stackaddr$env-ref54277 = alloca %struct.ScmObj*, align 8
%k40511 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44350, i64 1)
store %struct.ScmObj* %k40511, %struct.ScmObj** %stackaddr$env-ref54277
%stackaddr$env-ref54278 = alloca %struct.ScmObj*, align 8
%n40254 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44350, i64 2)
store %struct.ScmObj* %n40254, %struct.ScmObj** %stackaddr$env-ref54278
%stackaddr$env-ref54279 = alloca %struct.ScmObj*, align 8
%peano_45_62nat40235 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44350, i64 3)
store %struct.ScmObj* %peano_45_62nat40235, %struct.ScmObj** %stackaddr$env-ref54279
%stackaddr$prim54280 = alloca %struct.ScmObj*, align 8
%_95k40512 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52791)
store volatile %struct.ScmObj* %_95k40512, %struct.ScmObj** %stackaddr$prim54280, align 8
%stackaddr$prim54281 = alloca %struct.ScmObj*, align 8
%current_45args52792 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52791)
store volatile %struct.ScmObj* %current_45args52792, %struct.ScmObj** %stackaddr$prim54281, align 8
%stackaddr$prim54282 = alloca %struct.ScmObj*, align 8
%anf_45bind40391 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52792)
store volatile %struct.ScmObj* %anf_45bind40391, %struct.ScmObj** %stackaddr$prim54282, align 8
%truthy$cmp54283 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40391)
%cmp$cmp54283 = icmp eq i64 %truthy$cmp54283, 1
br i1 %cmp$cmp54283, label %truebranch$cmp54283, label %falsebranch$cmp54283
truebranch$cmp54283:
%ae44354 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44355 = call %struct.ScmObj* @const_init_int(i64 0)
%args52794$k40511$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54284 = alloca %struct.ScmObj*, align 8
%args52794$k40511$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44355, %struct.ScmObj* %args52794$k40511$0)
store volatile %struct.ScmObj* %args52794$k40511$1, %struct.ScmObj** %stackaddr$prim54284, align 8
%stackaddr$prim54285 = alloca %struct.ScmObj*, align 8
%args52794$k40511$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44354, %struct.ScmObj* %args52794$k40511$1)
store volatile %struct.ScmObj* %args52794$k40511$2, %struct.ScmObj** %stackaddr$prim54285, align 8
%clofunc54286 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40511)
musttail call tailcc void %clofunc54286(%struct.ScmObj* %k40511, %struct.ScmObj* %args52794$k40511$2)
ret void
falsebranch$cmp54283:
%ae44363 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54287 = alloca %struct.ScmObj*, align 8
%anf_45bind40392 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %peano_45_62nat40235, %struct.ScmObj* %ae44363)
store volatile %struct.ScmObj* %anf_45bind40392, %struct.ScmObj** %stackaddr$prim54287, align 8
%ae44365 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54288 = alloca %struct.ScmObj*, align 8
%anf_45bind40393 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %pred40233, %struct.ScmObj* %ae44365)
store volatile %struct.ScmObj* %anf_45bind40393, %struct.ScmObj** %stackaddr$prim54288, align 8
%stackaddr$makeclosure54289 = alloca %struct.ScmObj*, align 8
%fptrToInt54290 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44367 to i64
%ae44367 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54290)
store volatile %struct.ScmObj* %ae44367, %struct.ScmObj** %stackaddr$makeclosure54289, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44367, %struct.ScmObj* %anf_45bind40392, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44367, %struct.ScmObj* %k40511, i64 1)
%args52803$anf_45bind40393$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54291 = alloca %struct.ScmObj*, align 8
%args52803$anf_45bind40393$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %n40254, %struct.ScmObj* %args52803$anf_45bind40393$0)
store volatile %struct.ScmObj* %args52803$anf_45bind40393$1, %struct.ScmObj** %stackaddr$prim54291, align 8
%stackaddr$prim54292 = alloca %struct.ScmObj*, align 8
%args52803$anf_45bind40393$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44367, %struct.ScmObj* %args52803$anf_45bind40393$1)
store volatile %struct.ScmObj* %args52803$anf_45bind40393$2, %struct.ScmObj** %stackaddr$prim54292, align 8
%clofunc54293 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40393)
musttail call tailcc void %clofunc54293(%struct.ScmObj* %anf_45bind40393, %struct.ScmObj* %args52803$anf_45bind40393$2)
ret void
}

define tailcc void @proc_clo$ae44367(%struct.ScmObj* %env$ae44367,%struct.ScmObj* %current_45args52795) {
%stackaddr$env-ref54294 = alloca %struct.ScmObj*, align 8
%anf_45bind40392 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44367, i64 0)
store %struct.ScmObj* %anf_45bind40392, %struct.ScmObj** %stackaddr$env-ref54294
%stackaddr$env-ref54295 = alloca %struct.ScmObj*, align 8
%k40511 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44367, i64 1)
store %struct.ScmObj* %k40511, %struct.ScmObj** %stackaddr$env-ref54295
%stackaddr$prim54296 = alloca %struct.ScmObj*, align 8
%_95k40513 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52795)
store volatile %struct.ScmObj* %_95k40513, %struct.ScmObj** %stackaddr$prim54296, align 8
%stackaddr$prim54297 = alloca %struct.ScmObj*, align 8
%current_45args52796 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52795)
store volatile %struct.ScmObj* %current_45args52796, %struct.ScmObj** %stackaddr$prim54297, align 8
%stackaddr$prim54298 = alloca %struct.ScmObj*, align 8
%anf_45bind40394 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52796)
store volatile %struct.ScmObj* %anf_45bind40394, %struct.ScmObj** %stackaddr$prim54298, align 8
%stackaddr$makeclosure54299 = alloca %struct.ScmObj*, align 8
%fptrToInt54300 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44370 to i64
%ae44370 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54300)
store volatile %struct.ScmObj* %ae44370, %struct.ScmObj** %stackaddr$makeclosure54299, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44370, %struct.ScmObj* %k40511, i64 0)
%args52802$anf_45bind40392$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54301 = alloca %struct.ScmObj*, align 8
%args52802$anf_45bind40392$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40394, %struct.ScmObj* %args52802$anf_45bind40392$0)
store volatile %struct.ScmObj* %args52802$anf_45bind40392$1, %struct.ScmObj** %stackaddr$prim54301, align 8
%stackaddr$prim54302 = alloca %struct.ScmObj*, align 8
%args52802$anf_45bind40392$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44370, %struct.ScmObj* %args52802$anf_45bind40392$1)
store volatile %struct.ScmObj* %args52802$anf_45bind40392$2, %struct.ScmObj** %stackaddr$prim54302, align 8
%clofunc54303 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40392)
musttail call tailcc void %clofunc54303(%struct.ScmObj* %anf_45bind40392, %struct.ScmObj* %args52802$anf_45bind40392$2)
ret void
}

define tailcc void @proc_clo$ae44370(%struct.ScmObj* %env$ae44370,%struct.ScmObj* %current_45args52798) {
%stackaddr$env-ref54304 = alloca %struct.ScmObj*, align 8
%k40511 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44370, i64 0)
store %struct.ScmObj* %k40511, %struct.ScmObj** %stackaddr$env-ref54304
%stackaddr$prim54305 = alloca %struct.ScmObj*, align 8
%_95k40514 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52798)
store volatile %struct.ScmObj* %_95k40514, %struct.ScmObj** %stackaddr$prim54305, align 8
%stackaddr$prim54306 = alloca %struct.ScmObj*, align 8
%current_45args52799 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52798)
store volatile %struct.ScmObj* %current_45args52799, %struct.ScmObj** %stackaddr$prim54306, align 8
%stackaddr$prim54307 = alloca %struct.ScmObj*, align 8
%anf_45bind40395 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52799)
store volatile %struct.ScmObj* %anf_45bind40395, %struct.ScmObj** %stackaddr$prim54307, align 8
%ae44372 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54308 = alloca %struct.ScmObj*, align 8
%cpsprim40515 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae44372, %struct.ScmObj* %anf_45bind40395)
store volatile %struct.ScmObj* %cpsprim40515, %struct.ScmObj** %stackaddr$prim54308, align 8
%ae44375 = call %struct.ScmObj* @const_init_int(i64 0)
%args52801$k40511$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54309 = alloca %struct.ScmObj*, align 8
%args52801$k40511$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40515, %struct.ScmObj* %args52801$k40511$0)
store volatile %struct.ScmObj* %args52801$k40511$1, %struct.ScmObj** %stackaddr$prim54309, align 8
%stackaddr$prim54310 = alloca %struct.ScmObj*, align 8
%args52801$k40511$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44375, %struct.ScmObj* %args52801$k40511$1)
store volatile %struct.ScmObj* %args52801$k40511$2, %struct.ScmObj** %stackaddr$prim54310, align 8
%clofunc54311 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40511)
musttail call tailcc void %clofunc54311(%struct.ScmObj* %k40511, %struct.ScmObj* %args52801$k40511$2)
ret void
}

define tailcc void @proc_clo$ae44135(%struct.ScmObj* %env$ae44135,%struct.ScmObj* %current_45args52806) {
%stackaddr$env-ref54312 = alloca %struct.ScmObj*, align 8
%nat_45_62peano40236 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44135, i64 0)
store %struct.ScmObj* %nat_45_62peano40236, %struct.ScmObj** %stackaddr$env-ref54312
%stackaddr$prim54313 = alloca %struct.ScmObj*, align 8
%k40516 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52806)
store volatile %struct.ScmObj* %k40516, %struct.ScmObj** %stackaddr$prim54313, align 8
%stackaddr$prim54314 = alloca %struct.ScmObj*, align 8
%current_45args52807 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52806)
store volatile %struct.ScmObj* %current_45args52807, %struct.ScmObj** %stackaddr$prim54314, align 8
%stackaddr$prim54315 = alloca %struct.ScmObj*, align 8
%n40256 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52807)
store volatile %struct.ScmObj* %n40256, %struct.ScmObj** %stackaddr$prim54315, align 8
%ae44136 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54316 = alloca %struct.ScmObj*, align 8
%anf_45bind40382 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae44136, %struct.ScmObj* %n40256)
store volatile %struct.ScmObj* %anf_45bind40382, %struct.ScmObj** %stackaddr$prim54316, align 8
%truthy$cmp54317 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40382)
%cmp$cmp54317 = icmp eq i64 %truthy$cmp54317, 1
br i1 %cmp$cmp54317, label %truebranch$cmp54317, label %falsebranch$cmp54317
truebranch$cmp54317:
%stackaddr$makeclosure54318 = alloca %struct.ScmObj*, align 8
%fptrToInt54319 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44139 to i64
%ae44139 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54319)
store volatile %struct.ScmObj* %ae44139, %struct.ScmObj** %stackaddr$makeclosure54318, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44139, %struct.ScmObj* %k40516, i64 0)
%ae44140 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54320 = alloca %struct.ScmObj*, align 8
%fptrToInt54321 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44141 to i64
%ae44141 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54321)
store volatile %struct.ScmObj* %ae44141, %struct.ScmObj** %stackaddr$makeclosure54320, align 8
%args52814$ae44139$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54322 = alloca %struct.ScmObj*, align 8
%args52814$ae44139$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44141, %struct.ScmObj* %args52814$ae44139$0)
store volatile %struct.ScmObj* %args52814$ae44139$1, %struct.ScmObj** %stackaddr$prim54322, align 8
%stackaddr$prim54323 = alloca %struct.ScmObj*, align 8
%args52814$ae44139$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44140, %struct.ScmObj* %args52814$ae44139$1)
store volatile %struct.ScmObj* %args52814$ae44139$2, %struct.ScmObj** %stackaddr$prim54323, align 8
%clofunc54324 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44139)
musttail call tailcc void %clofunc54324(%struct.ScmObj* %ae44139, %struct.ScmObj* %args52814$ae44139$2)
ret void
falsebranch$cmp54317:
%ae44172 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54325 = alloca %struct.ScmObj*, align 8
%anf_45bind40384 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %nat_45_62peano40236, %struct.ScmObj* %ae44172)
store volatile %struct.ScmObj* %anf_45bind40384, %struct.ScmObj** %stackaddr$prim54325, align 8
%ae44174 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54326 = alloca %struct.ScmObj*, align 8
%anf_45bind40385 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n40256, %struct.ScmObj* %ae44174)
store volatile %struct.ScmObj* %anf_45bind40385, %struct.ScmObj** %stackaddr$prim54326, align 8
%stackaddr$makeclosure54327 = alloca %struct.ScmObj*, align 8
%fptrToInt54328 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44176 to i64
%ae44176 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54328)
store volatile %struct.ScmObj* %ae44176, %struct.ScmObj** %stackaddr$makeclosure54327, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44176, %struct.ScmObj* %k40516, i64 0)
%args52828$anf_45bind40384$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54329 = alloca %struct.ScmObj*, align 8
%args52828$anf_45bind40384$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40385, %struct.ScmObj* %args52828$anf_45bind40384$0)
store volatile %struct.ScmObj* %args52828$anf_45bind40384$1, %struct.ScmObj** %stackaddr$prim54329, align 8
%stackaddr$prim54330 = alloca %struct.ScmObj*, align 8
%args52828$anf_45bind40384$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44176, %struct.ScmObj* %args52828$anf_45bind40384$1)
store volatile %struct.ScmObj* %args52828$anf_45bind40384$2, %struct.ScmObj** %stackaddr$prim54330, align 8
%clofunc54331 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40384)
musttail call tailcc void %clofunc54331(%struct.ScmObj* %anf_45bind40384, %struct.ScmObj* %args52828$anf_45bind40384$2)
ret void
}

define tailcc void @proc_clo$ae44139(%struct.ScmObj* %env$ae44139,%struct.ScmObj* %current_45args52809) {
%stackaddr$env-ref54332 = alloca %struct.ScmObj*, align 8
%k40516 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44139, i64 0)
store %struct.ScmObj* %k40516, %struct.ScmObj** %stackaddr$env-ref54332
%stackaddr$prim54333 = alloca %struct.ScmObj*, align 8
%_95k40517 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52809)
store volatile %struct.ScmObj* %_95k40517, %struct.ScmObj** %stackaddr$prim54333, align 8
%stackaddr$prim54334 = alloca %struct.ScmObj*, align 8
%current_45args52810 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52809)
store volatile %struct.ScmObj* %current_45args52810, %struct.ScmObj** %stackaddr$prim54334, align 8
%stackaddr$prim54335 = alloca %struct.ScmObj*, align 8
%anf_45bind40383 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52810)
store volatile %struct.ScmObj* %anf_45bind40383, %struct.ScmObj** %stackaddr$prim54335, align 8
%args52812$anf_45bind40383$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54336 = alloca %struct.ScmObj*, align 8
%args52812$anf_45bind40383$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40516, %struct.ScmObj* %args52812$anf_45bind40383$0)
store volatile %struct.ScmObj* %args52812$anf_45bind40383$1, %struct.ScmObj** %stackaddr$prim54336, align 8
%clofunc54337 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40383)
musttail call tailcc void %clofunc54337(%struct.ScmObj* %anf_45bind40383, %struct.ScmObj* %args52812$anf_45bind40383$1)
ret void
}

define tailcc void @proc_clo$ae44141(%struct.ScmObj* %env$ae44141,%struct.ScmObj* %lst4025740518) {
%stackaddr$prim54338 = alloca %struct.ScmObj*, align 8
%k40519 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4025740518)
store volatile %struct.ScmObj* %k40519, %struct.ScmObj** %stackaddr$prim54338, align 8
%stackaddr$prim54339 = alloca %struct.ScmObj*, align 8
%lst40257 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4025740518)
store volatile %struct.ScmObj* %lst40257, %struct.ScmObj** %stackaddr$prim54339, align 8
%ae44145 = call %struct.ScmObj* @const_init_int(i64 0)
%args52813$k40519$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54340 = alloca %struct.ScmObj*, align 8
%args52813$k40519$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40257, %struct.ScmObj* %args52813$k40519$0)
store volatile %struct.ScmObj* %args52813$k40519$1, %struct.ScmObj** %stackaddr$prim54340, align 8
%stackaddr$prim54341 = alloca %struct.ScmObj*, align 8
%args52813$k40519$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44145, %struct.ScmObj* %args52813$k40519$1)
store volatile %struct.ScmObj* %args52813$k40519$2, %struct.ScmObj** %stackaddr$prim54341, align 8
%clofunc54342 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40519)
musttail call tailcc void %clofunc54342(%struct.ScmObj* %k40519, %struct.ScmObj* %args52813$k40519$2)
ret void
}

define tailcc void @proc_clo$ae44176(%struct.ScmObj* %env$ae44176,%struct.ScmObj* %current_45args52815) {
%stackaddr$env-ref54343 = alloca %struct.ScmObj*, align 8
%k40516 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44176, i64 0)
store %struct.ScmObj* %k40516, %struct.ScmObj** %stackaddr$env-ref54343
%stackaddr$prim54344 = alloca %struct.ScmObj*, align 8
%_95k40520 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52815)
store volatile %struct.ScmObj* %_95k40520, %struct.ScmObj** %stackaddr$prim54344, align 8
%stackaddr$prim54345 = alloca %struct.ScmObj*, align 8
%current_45args52816 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52815)
store volatile %struct.ScmObj* %current_45args52816, %struct.ScmObj** %stackaddr$prim54345, align 8
%stackaddr$prim54346 = alloca %struct.ScmObj*, align 8
%anf_45bind40386 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52816)
store volatile %struct.ScmObj* %anf_45bind40386, %struct.ScmObj** %stackaddr$prim54346, align 8
%stackaddr$makeclosure54347 = alloca %struct.ScmObj*, align 8
%fptrToInt54348 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44178 to i64
%ae44178 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54348)
store volatile %struct.ScmObj* %ae44178, %struct.ScmObj** %stackaddr$makeclosure54347, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44178, %struct.ScmObj* %k40516, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44178, %struct.ScmObj* %anf_45bind40386, i64 1)
%ae44179 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54349 = alloca %struct.ScmObj*, align 8
%fptrToInt54350 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44180 to i64
%ae44180 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54350)
store volatile %struct.ScmObj* %ae44180, %struct.ScmObj** %stackaddr$makeclosure54349, align 8
%args52827$ae44178$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54351 = alloca %struct.ScmObj*, align 8
%args52827$ae44178$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44180, %struct.ScmObj* %args52827$ae44178$0)
store volatile %struct.ScmObj* %args52827$ae44178$1, %struct.ScmObj** %stackaddr$prim54351, align 8
%stackaddr$prim54352 = alloca %struct.ScmObj*, align 8
%args52827$ae44178$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44179, %struct.ScmObj* %args52827$ae44178$1)
store volatile %struct.ScmObj* %args52827$ae44178$2, %struct.ScmObj** %stackaddr$prim54352, align 8
%clofunc54353 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44178)
musttail call tailcc void %clofunc54353(%struct.ScmObj* %ae44178, %struct.ScmObj* %args52827$ae44178$2)
ret void
}

define tailcc void @proc_clo$ae44178(%struct.ScmObj* %env$ae44178,%struct.ScmObj* %current_45args52818) {
%stackaddr$env-ref54354 = alloca %struct.ScmObj*, align 8
%k40516 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44178, i64 0)
store %struct.ScmObj* %k40516, %struct.ScmObj** %stackaddr$env-ref54354
%stackaddr$env-ref54355 = alloca %struct.ScmObj*, align 8
%anf_45bind40386 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44178, i64 1)
store %struct.ScmObj* %anf_45bind40386, %struct.ScmObj** %stackaddr$env-ref54355
%stackaddr$prim54356 = alloca %struct.ScmObj*, align 8
%_95k40521 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52818)
store volatile %struct.ScmObj* %_95k40521, %struct.ScmObj** %stackaddr$prim54356, align 8
%stackaddr$prim54357 = alloca %struct.ScmObj*, align 8
%current_45args52819 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52818)
store volatile %struct.ScmObj* %current_45args52819, %struct.ScmObj** %stackaddr$prim54357, align 8
%stackaddr$prim54358 = alloca %struct.ScmObj*, align 8
%anf_45bind40387 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52819)
store volatile %struct.ScmObj* %anf_45bind40387, %struct.ScmObj** %stackaddr$prim54358, align 8
%stackaddr$makeclosure54359 = alloca %struct.ScmObj*, align 8
%fptrToInt54360 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44201 to i64
%ae44201 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54360)
store volatile %struct.ScmObj* %ae44201, %struct.ScmObj** %stackaddr$makeclosure54359, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44201, %struct.ScmObj* %k40516, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44201, %struct.ScmObj* %anf_45bind40386, i64 1)
%args52825$anf_45bind40387$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54361 = alloca %struct.ScmObj*, align 8
%args52825$anf_45bind40387$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44201, %struct.ScmObj* %args52825$anf_45bind40387$0)
store volatile %struct.ScmObj* %args52825$anf_45bind40387$1, %struct.ScmObj** %stackaddr$prim54361, align 8
%clofunc54362 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40387)
musttail call tailcc void %clofunc54362(%struct.ScmObj* %anf_45bind40387, %struct.ScmObj* %args52825$anf_45bind40387$1)
ret void
}

define tailcc void @proc_clo$ae44201(%struct.ScmObj* %env$ae44201,%struct.ScmObj* %current_45args52821) {
%stackaddr$env-ref54363 = alloca %struct.ScmObj*, align 8
%k40516 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44201, i64 0)
store %struct.ScmObj* %k40516, %struct.ScmObj** %stackaddr$env-ref54363
%stackaddr$env-ref54364 = alloca %struct.ScmObj*, align 8
%anf_45bind40386 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44201, i64 1)
store %struct.ScmObj* %anf_45bind40386, %struct.ScmObj** %stackaddr$env-ref54364
%stackaddr$prim54365 = alloca %struct.ScmObj*, align 8
%_95k40522 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52821)
store volatile %struct.ScmObj* %_95k40522, %struct.ScmObj** %stackaddr$prim54365, align 8
%stackaddr$prim54366 = alloca %struct.ScmObj*, align 8
%current_45args52822 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52821)
store volatile %struct.ScmObj* %current_45args52822, %struct.ScmObj** %stackaddr$prim54366, align 8
%stackaddr$prim54367 = alloca %struct.ScmObj*, align 8
%anf_45bind40388 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52822)
store volatile %struct.ScmObj* %anf_45bind40388, %struct.ScmObj** %stackaddr$prim54367, align 8
%stackaddr$prim54368 = alloca %struct.ScmObj*, align 8
%cpsprim40523 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40386, %struct.ScmObj* %anf_45bind40388)
store volatile %struct.ScmObj* %cpsprim40523, %struct.ScmObj** %stackaddr$prim54368, align 8
%ae44205 = call %struct.ScmObj* @const_init_int(i64 0)
%args52824$k40516$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54369 = alloca %struct.ScmObj*, align 8
%args52824$k40516$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40523, %struct.ScmObj* %args52824$k40516$0)
store volatile %struct.ScmObj* %args52824$k40516$1, %struct.ScmObj** %stackaddr$prim54369, align 8
%stackaddr$prim54370 = alloca %struct.ScmObj*, align 8
%args52824$k40516$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44205, %struct.ScmObj* %args52824$k40516$1)
store volatile %struct.ScmObj* %args52824$k40516$2, %struct.ScmObj** %stackaddr$prim54370, align 8
%clofunc54371 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40516)
musttail call tailcc void %clofunc54371(%struct.ScmObj* %k40516, %struct.ScmObj* %args52824$k40516$2)
ret void
}

define tailcc void @proc_clo$ae44180(%struct.ScmObj* %env$ae44180,%struct.ScmObj* %lst4025840524) {
%stackaddr$prim54372 = alloca %struct.ScmObj*, align 8
%k40525 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4025840524)
store volatile %struct.ScmObj* %k40525, %struct.ScmObj** %stackaddr$prim54372, align 8
%stackaddr$prim54373 = alloca %struct.ScmObj*, align 8
%lst40258 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4025840524)
store volatile %struct.ScmObj* %lst40258, %struct.ScmObj** %stackaddr$prim54373, align 8
%ae44184 = call %struct.ScmObj* @const_init_int(i64 0)
%args52826$k40525$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54374 = alloca %struct.ScmObj*, align 8
%args52826$k40525$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40258, %struct.ScmObj* %args52826$k40525$0)
store volatile %struct.ScmObj* %args52826$k40525$1, %struct.ScmObj** %stackaddr$prim54374, align 8
%stackaddr$prim54375 = alloca %struct.ScmObj*, align 8
%args52826$k40525$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44184, %struct.ScmObj* %args52826$k40525$1)
store volatile %struct.ScmObj* %args52826$k40525$2, %struct.ScmObj** %stackaddr$prim54375, align 8
%clofunc54376 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40525)
musttail call tailcc void %clofunc54376(%struct.ScmObj* %k40525, %struct.ScmObj* %args52826$k40525$2)
ret void
}

define tailcc void @proc_clo$ae44034(%struct.ScmObj* %env$ae44034,%struct.ScmObj* %current_45args52830) {
%stackaddr$prim54377 = alloca %struct.ScmObj*, align 8
%k40526 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52830)
store volatile %struct.ScmObj* %k40526, %struct.ScmObj** %stackaddr$prim54377, align 8
%stackaddr$prim54378 = alloca %struct.ScmObj*, align 8
%current_45args52831 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52830)
store volatile %struct.ScmObj* %current_45args52831, %struct.ScmObj** %stackaddr$prim54378, align 8
%stackaddr$prim54379 = alloca %struct.ScmObj*, align 8
%thunk40229 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52831)
store volatile %struct.ScmObj* %thunk40229, %struct.ScmObj** %stackaddr$prim54379, align 8
%stackaddr$prim54380 = alloca %struct.ScmObj*, align 8
%anf_45bind40371 = call %struct.ScmObj* @prim_vector_63(%struct.ScmObj* %thunk40229)
store volatile %struct.ScmObj* %anf_45bind40371, %struct.ScmObj** %stackaddr$prim54380, align 8
%truthy$cmp54381 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40371)
%cmp$cmp54381 = icmp eq i64 %truthy$cmp54381, 1
br i1 %cmp$cmp54381, label %truebranch$cmp54381, label %falsebranch$cmp54381
truebranch$cmp54381:
%stackaddr$prim54382 = alloca %struct.ScmObj*, align 8
%anf_45bind40372 = call %struct.ScmObj* @prim_vector_45length(%struct.ScmObj* %thunk40229)
store volatile %struct.ScmObj* %anf_45bind40372, %struct.ScmObj** %stackaddr$prim54382, align 8
%ae44039 = call %struct.ScmObj* @const_init_int(i64 3)
%stackaddr$prim54383 = alloca %struct.ScmObj*, align 8
%anf_45bind40373 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind40372, %struct.ScmObj* %ae44039)
store volatile %struct.ScmObj* %anf_45bind40373, %struct.ScmObj** %stackaddr$prim54383, align 8
%truthy$cmp54384 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40373)
%cmp$cmp54384 = icmp eq i64 %truthy$cmp54384, 1
br i1 %cmp$cmp54384, label %truebranch$cmp54384, label %falsebranch$cmp54384
truebranch$cmp54384:
%ae44042 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54385 = alloca %struct.ScmObj*, align 8
%anf_45bind40374 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk40229, %struct.ScmObj* %ae44042)
store volatile %struct.ScmObj* %anf_45bind40374, %struct.ScmObj** %stackaddr$prim54385, align 8
%ae44044 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @global$sym$ae4404454386, i32 0, i32 0))
%stackaddr$prim54387 = alloca %struct.ScmObj*, align 8
%cpsprim40527 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind40374, %struct.ScmObj* %ae44044)
store volatile %struct.ScmObj* %cpsprim40527, %struct.ScmObj** %stackaddr$prim54387, align 8
%ae44046 = call %struct.ScmObj* @const_init_int(i64 0)
%args52833$k40526$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54388 = alloca %struct.ScmObj*, align 8
%args52833$k40526$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40527, %struct.ScmObj* %args52833$k40526$0)
store volatile %struct.ScmObj* %args52833$k40526$1, %struct.ScmObj** %stackaddr$prim54388, align 8
%stackaddr$prim54389 = alloca %struct.ScmObj*, align 8
%args52833$k40526$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44046, %struct.ScmObj* %args52833$k40526$1)
store volatile %struct.ScmObj* %args52833$k40526$2, %struct.ScmObj** %stackaddr$prim54389, align 8
%clofunc54390 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40526)
musttail call tailcc void %clofunc54390(%struct.ScmObj* %k40526, %struct.ScmObj* %args52833$k40526$2)
ret void
falsebranch$cmp54384:
%ae44064 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44065 = call %struct.ScmObj* @const_init_false()
%args52834$k40526$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54391 = alloca %struct.ScmObj*, align 8
%args52834$k40526$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44065, %struct.ScmObj* %args52834$k40526$0)
store volatile %struct.ScmObj* %args52834$k40526$1, %struct.ScmObj** %stackaddr$prim54391, align 8
%stackaddr$prim54392 = alloca %struct.ScmObj*, align 8
%args52834$k40526$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44064, %struct.ScmObj* %args52834$k40526$1)
store volatile %struct.ScmObj* %args52834$k40526$2, %struct.ScmObj** %stackaddr$prim54392, align 8
%clofunc54393 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40526)
musttail call tailcc void %clofunc54393(%struct.ScmObj* %k40526, %struct.ScmObj* %args52834$k40526$2)
ret void
falsebranch$cmp54381:
%ae44086 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44087 = call %struct.ScmObj* @const_init_false()
%args52835$k40526$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54394 = alloca %struct.ScmObj*, align 8
%args52835$k40526$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44087, %struct.ScmObj* %args52835$k40526$0)
store volatile %struct.ScmObj* %args52835$k40526$1, %struct.ScmObj** %stackaddr$prim54394, align 8
%stackaddr$prim54395 = alloca %struct.ScmObj*, align 8
%args52835$k40526$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44086, %struct.ScmObj* %args52835$k40526$1)
store volatile %struct.ScmObj* %args52835$k40526$2, %struct.ScmObj** %stackaddr$prim54395, align 8
%clofunc54396 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40526)
musttail call tailcc void %clofunc54396(%struct.ScmObj* %k40526, %struct.ScmObj* %args52835$k40526$2)
ret void
}

define tailcc void @proc_clo$ae44008(%struct.ScmObj* %env$ae44008,%struct.ScmObj* %current_45args52837) {
%stackaddr$prim54397 = alloca %struct.ScmObj*, align 8
%k40528 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52837)
store volatile %struct.ScmObj* %k40528, %struct.ScmObj** %stackaddr$prim54397, align 8
%stackaddr$prim54398 = alloca %struct.ScmObj*, align 8
%current_45args52838 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52837)
store volatile %struct.ScmObj* %current_45args52838, %struct.ScmObj** %stackaddr$prim54398, align 8
%stackaddr$prim54399 = alloca %struct.ScmObj*, align 8
%x40168 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52838)
store volatile %struct.ScmObj* %x40168, %struct.ScmObj** %stackaddr$prim54399, align 8
%stackaddr$prim54400 = alloca %struct.ScmObj*, align 8
%anf_45bind40368 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40168)
store volatile %struct.ScmObj* %anf_45bind40368, %struct.ScmObj** %stackaddr$prim54400, align 8
%stackaddr$prim54401 = alloca %struct.ScmObj*, align 8
%anf_45bind40369 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40368)
store volatile %struct.ScmObj* %anf_45bind40369, %struct.ScmObj** %stackaddr$prim54401, align 8
%stackaddr$prim54402 = alloca %struct.ScmObj*, align 8
%anf_45bind40370 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40369)
store volatile %struct.ScmObj* %anf_45bind40370, %struct.ScmObj** %stackaddr$prim54402, align 8
%stackaddr$prim54403 = alloca %struct.ScmObj*, align 8
%cpsprim40529 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40370)
store volatile %struct.ScmObj* %cpsprim40529, %struct.ScmObj** %stackaddr$prim54403, align 8
%ae44014 = call %struct.ScmObj* @const_init_int(i64 0)
%args52840$k40528$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54404 = alloca %struct.ScmObj*, align 8
%args52840$k40528$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40529, %struct.ScmObj* %args52840$k40528$0)
store volatile %struct.ScmObj* %args52840$k40528$1, %struct.ScmObj** %stackaddr$prim54404, align 8
%stackaddr$prim54405 = alloca %struct.ScmObj*, align 8
%args52840$k40528$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44014, %struct.ScmObj* %args52840$k40528$1)
store volatile %struct.ScmObj* %args52840$k40528$2, %struct.ScmObj** %stackaddr$prim54405, align 8
%clofunc54406 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40528)
musttail call tailcc void %clofunc54406(%struct.ScmObj* %k40528, %struct.ScmObj* %args52840$k40528$2)
ret void
}

define tailcc void @proc_clo$ae43984(%struct.ScmObj* %env$ae43984,%struct.ScmObj* %current_45args52842) {
%stackaddr$prim54407 = alloca %struct.ScmObj*, align 8
%k40530 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52842)
store volatile %struct.ScmObj* %k40530, %struct.ScmObj** %stackaddr$prim54407, align 8
%stackaddr$prim54408 = alloca %struct.ScmObj*, align 8
%current_45args52843 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52842)
store volatile %struct.ScmObj* %current_45args52843, %struct.ScmObj** %stackaddr$prim54408, align 8
%stackaddr$prim54409 = alloca %struct.ScmObj*, align 8
%x40170 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52843)
store volatile %struct.ScmObj* %x40170, %struct.ScmObj** %stackaddr$prim54409, align 8
%stackaddr$prim54410 = alloca %struct.ScmObj*, align 8
%anf_45bind40366 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40170)
store volatile %struct.ScmObj* %anf_45bind40366, %struct.ScmObj** %stackaddr$prim54410, align 8
%stackaddr$prim54411 = alloca %struct.ScmObj*, align 8
%anf_45bind40367 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40366)
store volatile %struct.ScmObj* %anf_45bind40367, %struct.ScmObj** %stackaddr$prim54411, align 8
%stackaddr$prim54412 = alloca %struct.ScmObj*, align 8
%cpsprim40531 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40367)
store volatile %struct.ScmObj* %cpsprim40531, %struct.ScmObj** %stackaddr$prim54412, align 8
%ae43989 = call %struct.ScmObj* @const_init_int(i64 0)
%args52845$k40530$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54413 = alloca %struct.ScmObj*, align 8
%args52845$k40530$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40531, %struct.ScmObj* %args52845$k40530$0)
store volatile %struct.ScmObj* %args52845$k40530$1, %struct.ScmObj** %stackaddr$prim54413, align 8
%stackaddr$prim54414 = alloca %struct.ScmObj*, align 8
%args52845$k40530$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43989, %struct.ScmObj* %args52845$k40530$1)
store volatile %struct.ScmObj* %args52845$k40530$2, %struct.ScmObj** %stackaddr$prim54414, align 8
%clofunc54415 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40530)
musttail call tailcc void %clofunc54415(%struct.ScmObj* %k40530, %struct.ScmObj* %args52845$k40530$2)
ret void
}

define tailcc void @proc_clo$ae43962(%struct.ScmObj* %env$ae43962,%struct.ScmObj* %current_45args52847) {
%stackaddr$prim54416 = alloca %struct.ScmObj*, align 8
%k40532 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52847)
store volatile %struct.ScmObj* %k40532, %struct.ScmObj** %stackaddr$prim54416, align 8
%stackaddr$prim54417 = alloca %struct.ScmObj*, align 8
%current_45args52848 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52847)
store volatile %struct.ScmObj* %current_45args52848, %struct.ScmObj** %stackaddr$prim54417, align 8
%stackaddr$prim54418 = alloca %struct.ScmObj*, align 8
%x40172 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52848)
store volatile %struct.ScmObj* %x40172, %struct.ScmObj** %stackaddr$prim54418, align 8
%stackaddr$prim54419 = alloca %struct.ScmObj*, align 8
%anf_45bind40365 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40172)
store volatile %struct.ScmObj* %anf_45bind40365, %struct.ScmObj** %stackaddr$prim54419, align 8
%stackaddr$prim54420 = alloca %struct.ScmObj*, align 8
%cpsprim40533 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40365)
store volatile %struct.ScmObj* %cpsprim40533, %struct.ScmObj** %stackaddr$prim54420, align 8
%ae43966 = call %struct.ScmObj* @const_init_int(i64 0)
%args52850$k40532$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54421 = alloca %struct.ScmObj*, align 8
%args52850$k40532$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40533, %struct.ScmObj* %args52850$k40532$0)
store volatile %struct.ScmObj* %args52850$k40532$1, %struct.ScmObj** %stackaddr$prim54421, align 8
%stackaddr$prim54422 = alloca %struct.ScmObj*, align 8
%args52850$k40532$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43966, %struct.ScmObj* %args52850$k40532$1)
store volatile %struct.ScmObj* %args52850$k40532$2, %struct.ScmObj** %stackaddr$prim54422, align 8
%clofunc54423 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40532)
musttail call tailcc void %clofunc54423(%struct.ScmObj* %k40532, %struct.ScmObj* %args52850$k40532$2)
ret void
}

define tailcc void @proc_clo$ae43942(%struct.ScmObj* %env$ae43942,%struct.ScmObj* %current_45args52852) {
%stackaddr$prim54424 = alloca %struct.ScmObj*, align 8
%k40534 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52852)
store volatile %struct.ScmObj* %k40534, %struct.ScmObj** %stackaddr$prim54424, align 8
%stackaddr$prim54425 = alloca %struct.ScmObj*, align 8
%current_45args52853 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52852)
store volatile %struct.ScmObj* %current_45args52853, %struct.ScmObj** %stackaddr$prim54425, align 8
%stackaddr$prim54426 = alloca %struct.ScmObj*, align 8
%x40174 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52853)
store volatile %struct.ScmObj* %x40174, %struct.ScmObj** %stackaddr$prim54426, align 8
%stackaddr$prim54427 = alloca %struct.ScmObj*, align 8
%cpsprim40535 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40174)
store volatile %struct.ScmObj* %cpsprim40535, %struct.ScmObj** %stackaddr$prim54427, align 8
%ae43945 = call %struct.ScmObj* @const_init_int(i64 0)
%args52855$k40534$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54428 = alloca %struct.ScmObj*, align 8
%args52855$k40534$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40535, %struct.ScmObj* %args52855$k40534$0)
store volatile %struct.ScmObj* %args52855$k40534$1, %struct.ScmObj** %stackaddr$prim54428, align 8
%stackaddr$prim54429 = alloca %struct.ScmObj*, align 8
%args52855$k40534$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43945, %struct.ScmObj* %args52855$k40534$1)
store volatile %struct.ScmObj* %args52855$k40534$2, %struct.ScmObj** %stackaddr$prim54429, align 8
%clofunc54430 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40534)
musttail call tailcc void %clofunc54430(%struct.ScmObj* %k40534, %struct.ScmObj* %args52855$k40534$2)
ret void
}

define tailcc void @proc_clo$ae43844(%struct.ScmObj* %env$ae43844,%struct.ScmObj* %args4017640536) {
%stackaddr$env-ref54431 = alloca %struct.ScmObj*, align 8
%_37foldl140115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43844, i64 0)
store %struct.ScmObj* %_37foldl140115, %struct.ScmObj** %stackaddr$env-ref54431
%stackaddr$prim54432 = alloca %struct.ScmObj*, align 8
%k40537 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4017640536)
store volatile %struct.ScmObj* %k40537, %struct.ScmObj** %stackaddr$prim54432, align 8
%stackaddr$prim54433 = alloca %struct.ScmObj*, align 8
%args40176 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4017640536)
store volatile %struct.ScmObj* %args40176, %struct.ScmObj** %stackaddr$prim54433, align 8
%stackaddr$prim54434 = alloca %struct.ScmObj*, align 8
%anf_45bind40359 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args40176)
store volatile %struct.ScmObj* %anf_45bind40359, %struct.ScmObj** %stackaddr$prim54434, align 8
%truthy$cmp54435 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40359)
%cmp$cmp54435 = icmp eq i64 %truthy$cmp54435, 1
br i1 %cmp$cmp54435, label %truebranch$cmp54435, label %falsebranch$cmp54435
truebranch$cmp54435:
%ae43850 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43851 = call %struct.ScmObj* @const_init_int(i64 1)
%args52857$k40537$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54436 = alloca %struct.ScmObj*, align 8
%args52857$k40537$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43851, %struct.ScmObj* %args52857$k40537$0)
store volatile %struct.ScmObj* %args52857$k40537$1, %struct.ScmObj** %stackaddr$prim54436, align 8
%stackaddr$prim54437 = alloca %struct.ScmObj*, align 8
%args52857$k40537$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43850, %struct.ScmObj* %args52857$k40537$1)
store volatile %struct.ScmObj* %args52857$k40537$2, %struct.ScmObj** %stackaddr$prim54437, align 8
%clofunc54438 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40537)
musttail call tailcc void %clofunc54438(%struct.ScmObj* %k40537, %struct.ScmObj* %args52857$k40537$2)
ret void
falsebranch$cmp54435:
%stackaddr$prim54439 = alloca %struct.ScmObj*, align 8
%anf_45bind40360 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40176)
store volatile %struct.ScmObj* %anf_45bind40360, %struct.ScmObj** %stackaddr$prim54439, align 8
%stackaddr$prim54440 = alloca %struct.ScmObj*, align 8
%anf_45bind40361 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40360)
store volatile %struct.ScmObj* %anf_45bind40361, %struct.ScmObj** %stackaddr$prim54440, align 8
%truthy$cmp54441 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40361)
%cmp$cmp54441 = icmp eq i64 %truthy$cmp54441, 1
br i1 %cmp$cmp54441, label %truebranch$cmp54441, label %falsebranch$cmp54441
truebranch$cmp54441:
%stackaddr$prim54442 = alloca %struct.ScmObj*, align 8
%cpsprim40538 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40176)
store volatile %struct.ScmObj* %cpsprim40538, %struct.ScmObj** %stackaddr$prim54442, align 8
%ae43863 = call %struct.ScmObj* @const_init_int(i64 0)
%args52858$k40537$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54443 = alloca %struct.ScmObj*, align 8
%args52858$k40537$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40538, %struct.ScmObj* %args52858$k40537$0)
store volatile %struct.ScmObj* %args52858$k40537$1, %struct.ScmObj** %stackaddr$prim54443, align 8
%stackaddr$prim54444 = alloca %struct.ScmObj*, align 8
%args52858$k40537$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43863, %struct.ScmObj* %args52858$k40537$1)
store volatile %struct.ScmObj* %args52858$k40537$2, %struct.ScmObj** %stackaddr$prim54444, align 8
%clofunc54445 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40537)
musttail call tailcc void %clofunc54445(%struct.ScmObj* %k40537, %struct.ScmObj* %args52858$k40537$2)
ret void
falsebranch$cmp54441:
%stackaddr$makeclosure54446 = alloca %struct.ScmObj*, align 8
%fptrToInt54447 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43868 to i64
%ae43868 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54447)
store volatile %struct.ScmObj* %ae43868, %struct.ScmObj** %stackaddr$makeclosure54446, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43868, %struct.ScmObj* %k40537, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43868, %struct.ScmObj* %_37foldl140115, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43868, %struct.ScmObj* %args40176, i64 2)
%ae43869 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54448 = alloca %struct.ScmObj*, align 8
%fptrToInt54449 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43870 to i64
%ae43870 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54449)
store volatile %struct.ScmObj* %ae43870, %struct.ScmObj** %stackaddr$makeclosure54448, align 8
%args52868$ae43868$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54450 = alloca %struct.ScmObj*, align 8
%args52868$ae43868$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43870, %struct.ScmObj* %args52868$ae43868$0)
store volatile %struct.ScmObj* %args52868$ae43868$1, %struct.ScmObj** %stackaddr$prim54450, align 8
%stackaddr$prim54451 = alloca %struct.ScmObj*, align 8
%args52868$ae43868$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43869, %struct.ScmObj* %args52868$ae43868$1)
store volatile %struct.ScmObj* %args52868$ae43868$2, %struct.ScmObj** %stackaddr$prim54451, align 8
%clofunc54452 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43868)
musttail call tailcc void %clofunc54452(%struct.ScmObj* %ae43868, %struct.ScmObj* %args52868$ae43868$2)
ret void
}

define tailcc void @proc_clo$ae43868(%struct.ScmObj* %env$ae43868,%struct.ScmObj* %current_45args52859) {
%stackaddr$env-ref54453 = alloca %struct.ScmObj*, align 8
%k40537 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43868, i64 0)
store %struct.ScmObj* %k40537, %struct.ScmObj** %stackaddr$env-ref54453
%stackaddr$env-ref54454 = alloca %struct.ScmObj*, align 8
%_37foldl140115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43868, i64 1)
store %struct.ScmObj* %_37foldl140115, %struct.ScmObj** %stackaddr$env-ref54454
%stackaddr$env-ref54455 = alloca %struct.ScmObj*, align 8
%args40176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43868, i64 2)
store %struct.ScmObj* %args40176, %struct.ScmObj** %stackaddr$env-ref54455
%stackaddr$prim54456 = alloca %struct.ScmObj*, align 8
%_95k40539 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52859)
store volatile %struct.ScmObj* %_95k40539, %struct.ScmObj** %stackaddr$prim54456, align 8
%stackaddr$prim54457 = alloca %struct.ScmObj*, align 8
%current_45args52860 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52859)
store volatile %struct.ScmObj* %current_45args52860, %struct.ScmObj** %stackaddr$prim54457, align 8
%stackaddr$prim54458 = alloca %struct.ScmObj*, align 8
%anf_45bind40362 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52860)
store volatile %struct.ScmObj* %anf_45bind40362, %struct.ScmObj** %stackaddr$prim54458, align 8
%stackaddr$prim54459 = alloca %struct.ScmObj*, align 8
%anf_45bind40363 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40176)
store volatile %struct.ScmObj* %anf_45bind40363, %struct.ScmObj** %stackaddr$prim54459, align 8
%stackaddr$prim54460 = alloca %struct.ScmObj*, align 8
%anf_45bind40364 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40176)
store volatile %struct.ScmObj* %anf_45bind40364, %struct.ScmObj** %stackaddr$prim54460, align 8
%args52862$_37foldl140115$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54461 = alloca %struct.ScmObj*, align 8
%args52862$_37foldl140115$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40364, %struct.ScmObj* %args52862$_37foldl140115$0)
store volatile %struct.ScmObj* %args52862$_37foldl140115$1, %struct.ScmObj** %stackaddr$prim54461, align 8
%stackaddr$prim54462 = alloca %struct.ScmObj*, align 8
%args52862$_37foldl140115$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40363, %struct.ScmObj* %args52862$_37foldl140115$1)
store volatile %struct.ScmObj* %args52862$_37foldl140115$2, %struct.ScmObj** %stackaddr$prim54462, align 8
%stackaddr$prim54463 = alloca %struct.ScmObj*, align 8
%args52862$_37foldl140115$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40362, %struct.ScmObj* %args52862$_37foldl140115$2)
store volatile %struct.ScmObj* %args52862$_37foldl140115$3, %struct.ScmObj** %stackaddr$prim54463, align 8
%stackaddr$prim54464 = alloca %struct.ScmObj*, align 8
%args52862$_37foldl140115$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40537, %struct.ScmObj* %args52862$_37foldl140115$3)
store volatile %struct.ScmObj* %args52862$_37foldl140115$4, %struct.ScmObj** %stackaddr$prim54464, align 8
%clofunc54465 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140115)
musttail call tailcc void %clofunc54465(%struct.ScmObj* %_37foldl140115, %struct.ScmObj* %args52862$_37foldl140115$4)
ret void
}

define tailcc void @proc_clo$ae43870(%struct.ScmObj* %env$ae43870,%struct.ScmObj* %current_45args52863) {
%stackaddr$prim54466 = alloca %struct.ScmObj*, align 8
%k40540 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52863)
store volatile %struct.ScmObj* %k40540, %struct.ScmObj** %stackaddr$prim54466, align 8
%stackaddr$prim54467 = alloca %struct.ScmObj*, align 8
%current_45args52864 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52863)
store volatile %struct.ScmObj* %current_45args52864, %struct.ScmObj** %stackaddr$prim54467, align 8
%stackaddr$prim54468 = alloca %struct.ScmObj*, align 8
%n40178 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52864)
store volatile %struct.ScmObj* %n40178, %struct.ScmObj** %stackaddr$prim54468, align 8
%stackaddr$prim54469 = alloca %struct.ScmObj*, align 8
%current_45args52865 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52864)
store volatile %struct.ScmObj* %current_45args52865, %struct.ScmObj** %stackaddr$prim54469, align 8
%stackaddr$prim54470 = alloca %struct.ScmObj*, align 8
%v40177 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52865)
store volatile %struct.ScmObj* %v40177, %struct.ScmObj** %stackaddr$prim54470, align 8
%stackaddr$prim54471 = alloca %struct.ScmObj*, align 8
%cpsprim40541 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v40177, %struct.ScmObj* %n40178)
store volatile %struct.ScmObj* %cpsprim40541, %struct.ScmObj** %stackaddr$prim54471, align 8
%ae43874 = call %struct.ScmObj* @const_init_int(i64 0)
%args52867$k40540$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54472 = alloca %struct.ScmObj*, align 8
%args52867$k40540$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40541, %struct.ScmObj* %args52867$k40540$0)
store volatile %struct.ScmObj* %args52867$k40540$1, %struct.ScmObj** %stackaddr$prim54472, align 8
%stackaddr$prim54473 = alloca %struct.ScmObj*, align 8
%args52867$k40540$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43874, %struct.ScmObj* %args52867$k40540$1)
store volatile %struct.ScmObj* %args52867$k40540$2, %struct.ScmObj** %stackaddr$prim54473, align 8
%clofunc54474 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40540)
musttail call tailcc void %clofunc54474(%struct.ScmObj* %k40540, %struct.ScmObj* %args52867$k40540$2)
ret void
}

define tailcc void @proc_clo$ae43440(%struct.ScmObj* %env$ae43440,%struct.ScmObj* %current_45args52870) {
%stackaddr$prim54475 = alloca %struct.ScmObj*, align 8
%k40542 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52870)
store volatile %struct.ScmObj* %k40542, %struct.ScmObj** %stackaddr$prim54475, align 8
%stackaddr$prim54476 = alloca %struct.ScmObj*, align 8
%current_45args52871 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52870)
store volatile %struct.ScmObj* %current_45args52871, %struct.ScmObj** %stackaddr$prim54476, align 8
%stackaddr$prim54477 = alloca %struct.ScmObj*, align 8
%v40181 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52871)
store volatile %struct.ScmObj* %v40181, %struct.ScmObj** %stackaddr$prim54477, align 8
%stackaddr$prim54478 = alloca %struct.ScmObj*, align 8
%current_45args52872 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52871)
store volatile %struct.ScmObj* %current_45args52872, %struct.ScmObj** %stackaddr$prim54478, align 8
%stackaddr$prim54479 = alloca %struct.ScmObj*, align 8
%lst40180 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52872)
store volatile %struct.ScmObj* %lst40180, %struct.ScmObj** %stackaddr$prim54479, align 8
%ae43441 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54480 = alloca %struct.ScmObj*, align 8
%lst40182 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae43441, %struct.ScmObj* %lst40180)
store volatile %struct.ScmObj* %lst40182, %struct.ScmObj** %stackaddr$prim54480, align 8
%stackaddr$makeclosure54481 = alloca %struct.ScmObj*, align 8
%fptrToInt54482 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43443 to i64
%ae43443 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54482)
store volatile %struct.ScmObj* %ae43443, %struct.ScmObj** %stackaddr$makeclosure54481, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43443, %struct.ScmObj* %lst40182, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43443, %struct.ScmObj* %v40181, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43443, %struct.ScmObj* %k40542, i64 2)
%ae43444 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54483 = alloca %struct.ScmObj*, align 8
%fptrToInt54484 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43445 to i64
%ae43445 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54484)
store volatile %struct.ScmObj* %ae43445, %struct.ScmObj** %stackaddr$makeclosure54483, align 8
%args52894$ae43443$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54485 = alloca %struct.ScmObj*, align 8
%args52894$ae43443$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43445, %struct.ScmObj* %args52894$ae43443$0)
store volatile %struct.ScmObj* %args52894$ae43443$1, %struct.ScmObj** %stackaddr$prim54485, align 8
%stackaddr$prim54486 = alloca %struct.ScmObj*, align 8
%args52894$ae43443$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43444, %struct.ScmObj* %args52894$ae43443$1)
store volatile %struct.ScmObj* %args52894$ae43443$2, %struct.ScmObj** %stackaddr$prim54486, align 8
%clofunc54487 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43443)
musttail call tailcc void %clofunc54487(%struct.ScmObj* %ae43443, %struct.ScmObj* %args52894$ae43443$2)
ret void
}

define tailcc void @proc_clo$ae43443(%struct.ScmObj* %env$ae43443,%struct.ScmObj* %current_45args52874) {
%stackaddr$env-ref54488 = alloca %struct.ScmObj*, align 8
%lst40182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43443, i64 0)
store %struct.ScmObj* %lst40182, %struct.ScmObj** %stackaddr$env-ref54488
%stackaddr$env-ref54489 = alloca %struct.ScmObj*, align 8
%v40181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43443, i64 1)
store %struct.ScmObj* %v40181, %struct.ScmObj** %stackaddr$env-ref54489
%stackaddr$env-ref54490 = alloca %struct.ScmObj*, align 8
%k40542 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43443, i64 2)
store %struct.ScmObj* %k40542, %struct.ScmObj** %stackaddr$env-ref54490
%stackaddr$prim54491 = alloca %struct.ScmObj*, align 8
%_95k40543 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52874)
store volatile %struct.ScmObj* %_95k40543, %struct.ScmObj** %stackaddr$prim54491, align 8
%stackaddr$prim54492 = alloca %struct.ScmObj*, align 8
%current_45args52875 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52874)
store volatile %struct.ScmObj* %current_45args52875, %struct.ScmObj** %stackaddr$prim54492, align 8
%stackaddr$prim54493 = alloca %struct.ScmObj*, align 8
%anf_45bind40351 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52875)
store volatile %struct.ScmObj* %anf_45bind40351, %struct.ScmObj** %stackaddr$prim54493, align 8
%stackaddr$makeclosure54494 = alloca %struct.ScmObj*, align 8
%fptrToInt54495 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43459 to i64
%ae43459 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54495)
store volatile %struct.ScmObj* %ae43459, %struct.ScmObj** %stackaddr$makeclosure54494, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43459, %struct.ScmObj* %lst40182, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43459, %struct.ScmObj* %v40181, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43459, %struct.ScmObj* %k40542, i64 2)
%stackaddr$makeclosure54496 = alloca %struct.ScmObj*, align 8
%fptrToInt54497 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43460 to i64
%ae43460 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54497)
store volatile %struct.ScmObj* %ae43460, %struct.ScmObj** %stackaddr$makeclosure54496, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43460, %struct.ScmObj* %lst40182, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43460, %struct.ScmObj* %v40181, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43460, %struct.ScmObj* %k40542, i64 2)
%args52889$anf_45bind40351$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54498 = alloca %struct.ScmObj*, align 8
%args52889$anf_45bind40351$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43460, %struct.ScmObj* %args52889$anf_45bind40351$0)
store volatile %struct.ScmObj* %args52889$anf_45bind40351$1, %struct.ScmObj** %stackaddr$prim54498, align 8
%stackaddr$prim54499 = alloca %struct.ScmObj*, align 8
%args52889$anf_45bind40351$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43459, %struct.ScmObj* %args52889$anf_45bind40351$1)
store volatile %struct.ScmObj* %args52889$anf_45bind40351$2, %struct.ScmObj** %stackaddr$prim54499, align 8
%clofunc54500 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40351)
musttail call tailcc void %clofunc54500(%struct.ScmObj* %anf_45bind40351, %struct.ScmObj* %args52889$anf_45bind40351$2)
ret void
}

define tailcc void @proc_clo$ae43459(%struct.ScmObj* %env$ae43459,%struct.ScmObj* %current_45args52877) {
%stackaddr$env-ref54501 = alloca %struct.ScmObj*, align 8
%lst40182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43459, i64 0)
store %struct.ScmObj* %lst40182, %struct.ScmObj** %stackaddr$env-ref54501
%stackaddr$env-ref54502 = alloca %struct.ScmObj*, align 8
%v40181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43459, i64 1)
store %struct.ScmObj* %v40181, %struct.ScmObj** %stackaddr$env-ref54502
%stackaddr$env-ref54503 = alloca %struct.ScmObj*, align 8
%k40542 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43459, i64 2)
store %struct.ScmObj* %k40542, %struct.ScmObj** %stackaddr$env-ref54503
%stackaddr$prim54504 = alloca %struct.ScmObj*, align 8
%_95k40544 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52877)
store volatile %struct.ScmObj* %_95k40544, %struct.ScmObj** %stackaddr$prim54504, align 8
%stackaddr$prim54505 = alloca %struct.ScmObj*, align 8
%current_45args52878 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52877)
store volatile %struct.ScmObj* %current_45args52878, %struct.ScmObj** %stackaddr$prim54505, align 8
%stackaddr$prim54506 = alloca %struct.ScmObj*, align 8
%cc40183 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52878)
store volatile %struct.ScmObj* %cc40183, %struct.ScmObj** %stackaddr$prim54506, align 8
%ae43568 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54507 = alloca %struct.ScmObj*, align 8
%anf_45bind40352 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40182, %struct.ScmObj* %ae43568)
store volatile %struct.ScmObj* %anf_45bind40352, %struct.ScmObj** %stackaddr$prim54507, align 8
%stackaddr$prim54508 = alloca %struct.ScmObj*, align 8
%anf_45bind40353 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40352)
store volatile %struct.ScmObj* %anf_45bind40353, %struct.ScmObj** %stackaddr$prim54508, align 8
%truthy$cmp54509 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40353)
%cmp$cmp54509 = icmp eq i64 %truthy$cmp54509, 1
br i1 %cmp$cmp54509, label %truebranch$cmp54509, label %falsebranch$cmp54509
truebranch$cmp54509:
%ae43572 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43573 = call %struct.ScmObj* @const_init_false()
%args52880$k40542$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54510 = alloca %struct.ScmObj*, align 8
%args52880$k40542$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43573, %struct.ScmObj* %args52880$k40542$0)
store volatile %struct.ScmObj* %args52880$k40542$1, %struct.ScmObj** %stackaddr$prim54510, align 8
%stackaddr$prim54511 = alloca %struct.ScmObj*, align 8
%args52880$k40542$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43572, %struct.ScmObj* %args52880$k40542$1)
store volatile %struct.ScmObj* %args52880$k40542$2, %struct.ScmObj** %stackaddr$prim54511, align 8
%clofunc54512 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40542)
musttail call tailcc void %clofunc54512(%struct.ScmObj* %k40542, %struct.ScmObj* %args52880$k40542$2)
ret void
falsebranch$cmp54509:
%ae43581 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54513 = alloca %struct.ScmObj*, align 8
%anf_45bind40354 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40182, %struct.ScmObj* %ae43581)
store volatile %struct.ScmObj* %anf_45bind40354, %struct.ScmObj** %stackaddr$prim54513, align 8
%stackaddr$prim54514 = alloca %struct.ScmObj*, align 8
%anf_45bind40355 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40354)
store volatile %struct.ScmObj* %anf_45bind40355, %struct.ScmObj** %stackaddr$prim54514, align 8
%stackaddr$prim54515 = alloca %struct.ScmObj*, align 8
%anf_45bind40356 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind40355, %struct.ScmObj* %v40181)
store volatile %struct.ScmObj* %anf_45bind40356, %struct.ScmObj** %stackaddr$prim54515, align 8
%truthy$cmp54516 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40356)
%cmp$cmp54516 = icmp eq i64 %truthy$cmp54516, 1
br i1 %cmp$cmp54516, label %truebranch$cmp54516, label %falsebranch$cmp54516
truebranch$cmp54516:
%ae43587 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54517 = alloca %struct.ScmObj*, align 8
%cpsprim40545 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40182, %struct.ScmObj* %ae43587)
store volatile %struct.ScmObj* %cpsprim40545, %struct.ScmObj** %stackaddr$prim54517, align 8
%ae43589 = call %struct.ScmObj* @const_init_int(i64 0)
%args52881$k40542$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54518 = alloca %struct.ScmObj*, align 8
%args52881$k40542$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40545, %struct.ScmObj* %args52881$k40542$0)
store volatile %struct.ScmObj* %args52881$k40542$1, %struct.ScmObj** %stackaddr$prim54518, align 8
%stackaddr$prim54519 = alloca %struct.ScmObj*, align 8
%args52881$k40542$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43589, %struct.ScmObj* %args52881$k40542$1)
store volatile %struct.ScmObj* %args52881$k40542$2, %struct.ScmObj** %stackaddr$prim54519, align 8
%clofunc54520 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40542)
musttail call tailcc void %clofunc54520(%struct.ScmObj* %k40542, %struct.ScmObj* %args52881$k40542$2)
ret void
falsebranch$cmp54516:
%ae43600 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54521 = alloca %struct.ScmObj*, align 8
%anf_45bind40357 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40182, %struct.ScmObj* %ae43600)
store volatile %struct.ScmObj* %anf_45bind40357, %struct.ScmObj** %stackaddr$prim54521, align 8
%stackaddr$prim54522 = alloca %struct.ScmObj*, align 8
%anf_45bind40358 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40357)
store volatile %struct.ScmObj* %anf_45bind40358, %struct.ScmObj** %stackaddr$prim54522, align 8
%ae43603 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54523 = alloca %struct.ScmObj*, align 8
%_95040185 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40182, %struct.ScmObj* %ae43603, %struct.ScmObj* %anf_45bind40358)
store volatile %struct.ScmObj* %_95040185, %struct.ScmObj** %stackaddr$prim54523, align 8
%args52882$cc40183$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54524 = alloca %struct.ScmObj*, align 8
%args52882$cc40183$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40183, %struct.ScmObj* %args52882$cc40183$0)
store volatile %struct.ScmObj* %args52882$cc40183$1, %struct.ScmObj** %stackaddr$prim54524, align 8
%stackaddr$prim54525 = alloca %struct.ScmObj*, align 8
%args52882$cc40183$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40542, %struct.ScmObj* %args52882$cc40183$1)
store volatile %struct.ScmObj* %args52882$cc40183$2, %struct.ScmObj** %stackaddr$prim54525, align 8
%clofunc54526 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40183)
musttail call tailcc void %clofunc54526(%struct.ScmObj* %cc40183, %struct.ScmObj* %args52882$cc40183$2)
ret void
}

define tailcc void @proc_clo$ae43460(%struct.ScmObj* %env$ae43460,%struct.ScmObj* %current_45args52883) {
%stackaddr$env-ref54527 = alloca %struct.ScmObj*, align 8
%lst40182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43460, i64 0)
store %struct.ScmObj* %lst40182, %struct.ScmObj** %stackaddr$env-ref54527
%stackaddr$env-ref54528 = alloca %struct.ScmObj*, align 8
%v40181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43460, i64 1)
store %struct.ScmObj* %v40181, %struct.ScmObj** %stackaddr$env-ref54528
%stackaddr$env-ref54529 = alloca %struct.ScmObj*, align 8
%k40542 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43460, i64 2)
store %struct.ScmObj* %k40542, %struct.ScmObj** %stackaddr$env-ref54529
%stackaddr$prim54530 = alloca %struct.ScmObj*, align 8
%_95k40544 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52883)
store volatile %struct.ScmObj* %_95k40544, %struct.ScmObj** %stackaddr$prim54530, align 8
%stackaddr$prim54531 = alloca %struct.ScmObj*, align 8
%current_45args52884 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52883)
store volatile %struct.ScmObj* %current_45args52884, %struct.ScmObj** %stackaddr$prim54531, align 8
%stackaddr$prim54532 = alloca %struct.ScmObj*, align 8
%cc40183 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52884)
store volatile %struct.ScmObj* %cc40183, %struct.ScmObj** %stackaddr$prim54532, align 8
%ae43462 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54533 = alloca %struct.ScmObj*, align 8
%anf_45bind40352 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40182, %struct.ScmObj* %ae43462)
store volatile %struct.ScmObj* %anf_45bind40352, %struct.ScmObj** %stackaddr$prim54533, align 8
%stackaddr$prim54534 = alloca %struct.ScmObj*, align 8
%anf_45bind40353 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40352)
store volatile %struct.ScmObj* %anf_45bind40353, %struct.ScmObj** %stackaddr$prim54534, align 8
%truthy$cmp54535 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40353)
%cmp$cmp54535 = icmp eq i64 %truthy$cmp54535, 1
br i1 %cmp$cmp54535, label %truebranch$cmp54535, label %falsebranch$cmp54535
truebranch$cmp54535:
%ae43466 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43467 = call %struct.ScmObj* @const_init_false()
%args52886$k40542$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54536 = alloca %struct.ScmObj*, align 8
%args52886$k40542$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43467, %struct.ScmObj* %args52886$k40542$0)
store volatile %struct.ScmObj* %args52886$k40542$1, %struct.ScmObj** %stackaddr$prim54536, align 8
%stackaddr$prim54537 = alloca %struct.ScmObj*, align 8
%args52886$k40542$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43466, %struct.ScmObj* %args52886$k40542$1)
store volatile %struct.ScmObj* %args52886$k40542$2, %struct.ScmObj** %stackaddr$prim54537, align 8
%clofunc54538 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40542)
musttail call tailcc void %clofunc54538(%struct.ScmObj* %k40542, %struct.ScmObj* %args52886$k40542$2)
ret void
falsebranch$cmp54535:
%ae43475 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54539 = alloca %struct.ScmObj*, align 8
%anf_45bind40354 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40182, %struct.ScmObj* %ae43475)
store volatile %struct.ScmObj* %anf_45bind40354, %struct.ScmObj** %stackaddr$prim54539, align 8
%stackaddr$prim54540 = alloca %struct.ScmObj*, align 8
%anf_45bind40355 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40354)
store volatile %struct.ScmObj* %anf_45bind40355, %struct.ScmObj** %stackaddr$prim54540, align 8
%stackaddr$prim54541 = alloca %struct.ScmObj*, align 8
%anf_45bind40356 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind40355, %struct.ScmObj* %v40181)
store volatile %struct.ScmObj* %anf_45bind40356, %struct.ScmObj** %stackaddr$prim54541, align 8
%truthy$cmp54542 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40356)
%cmp$cmp54542 = icmp eq i64 %truthy$cmp54542, 1
br i1 %cmp$cmp54542, label %truebranch$cmp54542, label %falsebranch$cmp54542
truebranch$cmp54542:
%ae43481 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54543 = alloca %struct.ScmObj*, align 8
%cpsprim40545 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40182, %struct.ScmObj* %ae43481)
store volatile %struct.ScmObj* %cpsprim40545, %struct.ScmObj** %stackaddr$prim54543, align 8
%ae43483 = call %struct.ScmObj* @const_init_int(i64 0)
%args52887$k40542$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54544 = alloca %struct.ScmObj*, align 8
%args52887$k40542$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40545, %struct.ScmObj* %args52887$k40542$0)
store volatile %struct.ScmObj* %args52887$k40542$1, %struct.ScmObj** %stackaddr$prim54544, align 8
%stackaddr$prim54545 = alloca %struct.ScmObj*, align 8
%args52887$k40542$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43483, %struct.ScmObj* %args52887$k40542$1)
store volatile %struct.ScmObj* %args52887$k40542$2, %struct.ScmObj** %stackaddr$prim54545, align 8
%clofunc54546 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40542)
musttail call tailcc void %clofunc54546(%struct.ScmObj* %k40542, %struct.ScmObj* %args52887$k40542$2)
ret void
falsebranch$cmp54542:
%ae43494 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54547 = alloca %struct.ScmObj*, align 8
%anf_45bind40357 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40182, %struct.ScmObj* %ae43494)
store volatile %struct.ScmObj* %anf_45bind40357, %struct.ScmObj** %stackaddr$prim54547, align 8
%stackaddr$prim54548 = alloca %struct.ScmObj*, align 8
%anf_45bind40358 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40357)
store volatile %struct.ScmObj* %anf_45bind40358, %struct.ScmObj** %stackaddr$prim54548, align 8
%ae43497 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54549 = alloca %struct.ScmObj*, align 8
%_95040185 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40182, %struct.ScmObj* %ae43497, %struct.ScmObj* %anf_45bind40358)
store volatile %struct.ScmObj* %_95040185, %struct.ScmObj** %stackaddr$prim54549, align 8
%args52888$cc40183$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54550 = alloca %struct.ScmObj*, align 8
%args52888$cc40183$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40183, %struct.ScmObj* %args52888$cc40183$0)
store volatile %struct.ScmObj* %args52888$cc40183$1, %struct.ScmObj** %stackaddr$prim54550, align 8
%stackaddr$prim54551 = alloca %struct.ScmObj*, align 8
%args52888$cc40183$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40542, %struct.ScmObj* %args52888$cc40183$1)
store volatile %struct.ScmObj* %args52888$cc40183$2, %struct.ScmObj** %stackaddr$prim54551, align 8
%clofunc54552 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40183)
musttail call tailcc void %clofunc54552(%struct.ScmObj* %cc40183, %struct.ScmObj* %args52888$cc40183$2)
ret void
}

define tailcc void @proc_clo$ae43445(%struct.ScmObj* %env$ae43445,%struct.ScmObj* %current_45args52890) {
%stackaddr$prim54553 = alloca %struct.ScmObj*, align 8
%k40546 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52890)
store volatile %struct.ScmObj* %k40546, %struct.ScmObj** %stackaddr$prim54553, align 8
%stackaddr$prim54554 = alloca %struct.ScmObj*, align 8
%current_45args52891 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52890)
store volatile %struct.ScmObj* %current_45args52891, %struct.ScmObj** %stackaddr$prim54554, align 8
%stackaddr$prim54555 = alloca %struct.ScmObj*, align 8
%u40184 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52891)
store volatile %struct.ScmObj* %u40184, %struct.ScmObj** %stackaddr$prim54555, align 8
%args52893$u40184$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54556 = alloca %struct.ScmObj*, align 8
%args52893$u40184$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u40184, %struct.ScmObj* %args52893$u40184$0)
store volatile %struct.ScmObj* %args52893$u40184$1, %struct.ScmObj** %stackaddr$prim54556, align 8
%stackaddr$prim54557 = alloca %struct.ScmObj*, align 8
%args52893$u40184$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40546, %struct.ScmObj* %args52893$u40184$1)
store volatile %struct.ScmObj* %args52893$u40184$2, %struct.ScmObj** %stackaddr$prim54557, align 8
%clofunc54558 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u40184)
musttail call tailcc void %clofunc54558(%struct.ScmObj* %u40184, %struct.ScmObj* %args52893$u40184$2)
ret void
}

define tailcc void @proc_clo$ae42904(%struct.ScmObj* %env$ae42904,%struct.ScmObj* %current_45args52896) {
%stackaddr$prim54559 = alloca %struct.ScmObj*, align 8
%k40547 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52896)
store volatile %struct.ScmObj* %k40547, %struct.ScmObj** %stackaddr$prim54559, align 8
%stackaddr$prim54560 = alloca %struct.ScmObj*, align 8
%current_45args52897 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52896)
store volatile %struct.ScmObj* %current_45args52897, %struct.ScmObj** %stackaddr$prim54560, align 8
%stackaddr$prim54561 = alloca %struct.ScmObj*, align 8
%lst40188 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52897)
store volatile %struct.ScmObj* %lst40188, %struct.ScmObj** %stackaddr$prim54561, align 8
%stackaddr$prim54562 = alloca %struct.ScmObj*, align 8
%current_45args52898 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52897)
store volatile %struct.ScmObj* %current_45args52898, %struct.ScmObj** %stackaddr$prim54562, align 8
%stackaddr$prim54563 = alloca %struct.ScmObj*, align 8
%n40187 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52898)
store volatile %struct.ScmObj* %n40187, %struct.ScmObj** %stackaddr$prim54563, align 8
%ae42905 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54564 = alloca %struct.ScmObj*, align 8
%n40190 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42905, %struct.ScmObj* %n40187)
store volatile %struct.ScmObj* %n40190, %struct.ScmObj** %stackaddr$prim54564, align 8
%ae42907 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54565 = alloca %struct.ScmObj*, align 8
%lst40189 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42907, %struct.ScmObj* %lst40188)
store volatile %struct.ScmObj* %lst40189, %struct.ScmObj** %stackaddr$prim54565, align 8
%stackaddr$makeclosure54566 = alloca %struct.ScmObj*, align 8
%fptrToInt54567 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42909 to i64
%ae42909 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54567)
store volatile %struct.ScmObj* %ae42909, %struct.ScmObj** %stackaddr$makeclosure54566, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42909, %struct.ScmObj* %k40547, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42909, %struct.ScmObj* %n40190, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42909, %struct.ScmObj* %lst40189, i64 2)
%ae42910 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54568 = alloca %struct.ScmObj*, align 8
%fptrToInt54569 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42911 to i64
%ae42911 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54569)
store volatile %struct.ScmObj* %ae42911, %struct.ScmObj** %stackaddr$makeclosure54568, align 8
%args52918$ae42909$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54570 = alloca %struct.ScmObj*, align 8
%args52918$ae42909$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42911, %struct.ScmObj* %args52918$ae42909$0)
store volatile %struct.ScmObj* %args52918$ae42909$1, %struct.ScmObj** %stackaddr$prim54570, align 8
%stackaddr$prim54571 = alloca %struct.ScmObj*, align 8
%args52918$ae42909$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42910, %struct.ScmObj* %args52918$ae42909$1)
store volatile %struct.ScmObj* %args52918$ae42909$2, %struct.ScmObj** %stackaddr$prim54571, align 8
%clofunc54572 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42909)
musttail call tailcc void %clofunc54572(%struct.ScmObj* %ae42909, %struct.ScmObj* %args52918$ae42909$2)
ret void
}

define tailcc void @proc_clo$ae42909(%struct.ScmObj* %env$ae42909,%struct.ScmObj* %current_45args52900) {
%stackaddr$env-ref54573 = alloca %struct.ScmObj*, align 8
%k40547 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42909, i64 0)
store %struct.ScmObj* %k40547, %struct.ScmObj** %stackaddr$env-ref54573
%stackaddr$env-ref54574 = alloca %struct.ScmObj*, align 8
%n40190 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42909, i64 1)
store %struct.ScmObj* %n40190, %struct.ScmObj** %stackaddr$env-ref54574
%stackaddr$env-ref54575 = alloca %struct.ScmObj*, align 8
%lst40189 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42909, i64 2)
store %struct.ScmObj* %lst40189, %struct.ScmObj** %stackaddr$env-ref54575
%stackaddr$prim54576 = alloca %struct.ScmObj*, align 8
%_95k40548 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52900)
store volatile %struct.ScmObj* %_95k40548, %struct.ScmObj** %stackaddr$prim54576, align 8
%stackaddr$prim54577 = alloca %struct.ScmObj*, align 8
%current_45args52901 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52900)
store volatile %struct.ScmObj* %current_45args52901, %struct.ScmObj** %stackaddr$prim54577, align 8
%stackaddr$prim54578 = alloca %struct.ScmObj*, align 8
%anf_45bind40344 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52901)
store volatile %struct.ScmObj* %anf_45bind40344, %struct.ScmObj** %stackaddr$prim54578, align 8
%stackaddr$makeclosure54579 = alloca %struct.ScmObj*, align 8
%fptrToInt54580 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42925 to i64
%ae42925 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54580)
store volatile %struct.ScmObj* %ae42925, %struct.ScmObj** %stackaddr$makeclosure54579, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42925, %struct.ScmObj* %k40547, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42925, %struct.ScmObj* %n40190, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42925, %struct.ScmObj* %lst40189, i64 2)
%stackaddr$makeclosure54581 = alloca %struct.ScmObj*, align 8
%fptrToInt54582 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42926 to i64
%ae42926 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54582)
store volatile %struct.ScmObj* %ae42926, %struct.ScmObj** %stackaddr$makeclosure54581, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42926, %struct.ScmObj* %k40547, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42926, %struct.ScmObj* %n40190, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42926, %struct.ScmObj* %lst40189, i64 2)
%args52913$anf_45bind40344$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54583 = alloca %struct.ScmObj*, align 8
%args52913$anf_45bind40344$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42926, %struct.ScmObj* %args52913$anf_45bind40344$0)
store volatile %struct.ScmObj* %args52913$anf_45bind40344$1, %struct.ScmObj** %stackaddr$prim54583, align 8
%stackaddr$prim54584 = alloca %struct.ScmObj*, align 8
%args52913$anf_45bind40344$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42925, %struct.ScmObj* %args52913$anf_45bind40344$1)
store volatile %struct.ScmObj* %args52913$anf_45bind40344$2, %struct.ScmObj** %stackaddr$prim54584, align 8
%clofunc54585 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40344)
musttail call tailcc void %clofunc54585(%struct.ScmObj* %anf_45bind40344, %struct.ScmObj* %args52913$anf_45bind40344$2)
ret void
}

define tailcc void @proc_clo$ae42925(%struct.ScmObj* %env$ae42925,%struct.ScmObj* %current_45args52903) {
%stackaddr$env-ref54586 = alloca %struct.ScmObj*, align 8
%k40547 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42925, i64 0)
store %struct.ScmObj* %k40547, %struct.ScmObj** %stackaddr$env-ref54586
%stackaddr$env-ref54587 = alloca %struct.ScmObj*, align 8
%n40190 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42925, i64 1)
store %struct.ScmObj* %n40190, %struct.ScmObj** %stackaddr$env-ref54587
%stackaddr$env-ref54588 = alloca %struct.ScmObj*, align 8
%lst40189 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42925, i64 2)
store %struct.ScmObj* %lst40189, %struct.ScmObj** %stackaddr$env-ref54588
%stackaddr$prim54589 = alloca %struct.ScmObj*, align 8
%_95k40549 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52903)
store volatile %struct.ScmObj* %_95k40549, %struct.ScmObj** %stackaddr$prim54589, align 8
%stackaddr$prim54590 = alloca %struct.ScmObj*, align 8
%current_45args52904 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52903)
store volatile %struct.ScmObj* %current_45args52904, %struct.ScmObj** %stackaddr$prim54590, align 8
%stackaddr$prim54591 = alloca %struct.ScmObj*, align 8
%cc40191 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52904)
store volatile %struct.ScmObj* %cc40191, %struct.ScmObj** %stackaddr$prim54591, align 8
%ae43068 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54592 = alloca %struct.ScmObj*, align 8
%anf_45bind40345 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40190, %struct.ScmObj* %ae43068)
store volatile %struct.ScmObj* %anf_45bind40345, %struct.ScmObj** %stackaddr$prim54592, align 8
%ae43069 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54593 = alloca %struct.ScmObj*, align 8
%anf_45bind40346 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae43069, %struct.ScmObj* %anf_45bind40345)
store volatile %struct.ScmObj* %anf_45bind40346, %struct.ScmObj** %stackaddr$prim54593, align 8
%truthy$cmp54594 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40346)
%cmp$cmp54594 = icmp eq i64 %truthy$cmp54594, 1
br i1 %cmp$cmp54594, label %truebranch$cmp54594, label %falsebranch$cmp54594
truebranch$cmp54594:
%ae43073 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54595 = alloca %struct.ScmObj*, align 8
%cpsprim40550 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40189, %struct.ScmObj* %ae43073)
store volatile %struct.ScmObj* %cpsprim40550, %struct.ScmObj** %stackaddr$prim54595, align 8
%ae43075 = call %struct.ScmObj* @const_init_int(i64 0)
%args52906$k40547$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54596 = alloca %struct.ScmObj*, align 8
%args52906$k40547$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40550, %struct.ScmObj* %args52906$k40547$0)
store volatile %struct.ScmObj* %args52906$k40547$1, %struct.ScmObj** %stackaddr$prim54596, align 8
%stackaddr$prim54597 = alloca %struct.ScmObj*, align 8
%args52906$k40547$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43075, %struct.ScmObj* %args52906$k40547$1)
store volatile %struct.ScmObj* %args52906$k40547$2, %struct.ScmObj** %stackaddr$prim54597, align 8
%clofunc54598 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40547)
musttail call tailcc void %clofunc54598(%struct.ScmObj* %k40547, %struct.ScmObj* %args52906$k40547$2)
ret void
falsebranch$cmp54594:
%ae43086 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54599 = alloca %struct.ScmObj*, align 8
%anf_45bind40347 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40189, %struct.ScmObj* %ae43086)
store volatile %struct.ScmObj* %anf_45bind40347, %struct.ScmObj** %stackaddr$prim54599, align 8
%stackaddr$prim54600 = alloca %struct.ScmObj*, align 8
%anf_45bind40348 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40347)
store volatile %struct.ScmObj* %anf_45bind40348, %struct.ScmObj** %stackaddr$prim54600, align 8
%ae43089 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54601 = alloca %struct.ScmObj*, align 8
%_95040194 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40189, %struct.ScmObj* %ae43089, %struct.ScmObj* %anf_45bind40348)
store volatile %struct.ScmObj* %_95040194, %struct.ScmObj** %stackaddr$prim54601, align 8
%ae43092 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54602 = alloca %struct.ScmObj*, align 8
%anf_45bind40349 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40190, %struct.ScmObj* %ae43092)
store volatile %struct.ScmObj* %anf_45bind40349, %struct.ScmObj** %stackaddr$prim54602, align 8
%ae43094 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54603 = alloca %struct.ScmObj*, align 8
%anf_45bind40350 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind40349, %struct.ScmObj* %ae43094)
store volatile %struct.ScmObj* %anf_45bind40350, %struct.ScmObj** %stackaddr$prim54603, align 8
%ae43096 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54604 = alloca %struct.ScmObj*, align 8
%_95140193 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n40190, %struct.ScmObj* %ae43096, %struct.ScmObj* %anf_45bind40350)
store volatile %struct.ScmObj* %_95140193, %struct.ScmObj** %stackaddr$prim54604, align 8
%args52907$cc40191$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54605 = alloca %struct.ScmObj*, align 8
%args52907$cc40191$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40191, %struct.ScmObj* %args52907$cc40191$0)
store volatile %struct.ScmObj* %args52907$cc40191$1, %struct.ScmObj** %stackaddr$prim54605, align 8
%stackaddr$prim54606 = alloca %struct.ScmObj*, align 8
%args52907$cc40191$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40547, %struct.ScmObj* %args52907$cc40191$1)
store volatile %struct.ScmObj* %args52907$cc40191$2, %struct.ScmObj** %stackaddr$prim54606, align 8
%clofunc54607 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40191)
musttail call tailcc void %clofunc54607(%struct.ScmObj* %cc40191, %struct.ScmObj* %args52907$cc40191$2)
ret void
}

define tailcc void @proc_clo$ae42926(%struct.ScmObj* %env$ae42926,%struct.ScmObj* %current_45args52908) {
%stackaddr$env-ref54608 = alloca %struct.ScmObj*, align 8
%k40547 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42926, i64 0)
store %struct.ScmObj* %k40547, %struct.ScmObj** %stackaddr$env-ref54608
%stackaddr$env-ref54609 = alloca %struct.ScmObj*, align 8
%n40190 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42926, i64 1)
store %struct.ScmObj* %n40190, %struct.ScmObj** %stackaddr$env-ref54609
%stackaddr$env-ref54610 = alloca %struct.ScmObj*, align 8
%lst40189 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42926, i64 2)
store %struct.ScmObj* %lst40189, %struct.ScmObj** %stackaddr$env-ref54610
%stackaddr$prim54611 = alloca %struct.ScmObj*, align 8
%_95k40549 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52908)
store volatile %struct.ScmObj* %_95k40549, %struct.ScmObj** %stackaddr$prim54611, align 8
%stackaddr$prim54612 = alloca %struct.ScmObj*, align 8
%current_45args52909 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52908)
store volatile %struct.ScmObj* %current_45args52909, %struct.ScmObj** %stackaddr$prim54612, align 8
%stackaddr$prim54613 = alloca %struct.ScmObj*, align 8
%cc40191 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52909)
store volatile %struct.ScmObj* %cc40191, %struct.ScmObj** %stackaddr$prim54613, align 8
%ae42928 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54614 = alloca %struct.ScmObj*, align 8
%anf_45bind40345 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40190, %struct.ScmObj* %ae42928)
store volatile %struct.ScmObj* %anf_45bind40345, %struct.ScmObj** %stackaddr$prim54614, align 8
%ae42929 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54615 = alloca %struct.ScmObj*, align 8
%anf_45bind40346 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae42929, %struct.ScmObj* %anf_45bind40345)
store volatile %struct.ScmObj* %anf_45bind40346, %struct.ScmObj** %stackaddr$prim54615, align 8
%truthy$cmp54616 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40346)
%cmp$cmp54616 = icmp eq i64 %truthy$cmp54616, 1
br i1 %cmp$cmp54616, label %truebranch$cmp54616, label %falsebranch$cmp54616
truebranch$cmp54616:
%ae42933 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54617 = alloca %struct.ScmObj*, align 8
%cpsprim40550 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40189, %struct.ScmObj* %ae42933)
store volatile %struct.ScmObj* %cpsprim40550, %struct.ScmObj** %stackaddr$prim54617, align 8
%ae42935 = call %struct.ScmObj* @const_init_int(i64 0)
%args52911$k40547$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54618 = alloca %struct.ScmObj*, align 8
%args52911$k40547$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40550, %struct.ScmObj* %args52911$k40547$0)
store volatile %struct.ScmObj* %args52911$k40547$1, %struct.ScmObj** %stackaddr$prim54618, align 8
%stackaddr$prim54619 = alloca %struct.ScmObj*, align 8
%args52911$k40547$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42935, %struct.ScmObj* %args52911$k40547$1)
store volatile %struct.ScmObj* %args52911$k40547$2, %struct.ScmObj** %stackaddr$prim54619, align 8
%clofunc54620 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40547)
musttail call tailcc void %clofunc54620(%struct.ScmObj* %k40547, %struct.ScmObj* %args52911$k40547$2)
ret void
falsebranch$cmp54616:
%ae42946 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54621 = alloca %struct.ScmObj*, align 8
%anf_45bind40347 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40189, %struct.ScmObj* %ae42946)
store volatile %struct.ScmObj* %anf_45bind40347, %struct.ScmObj** %stackaddr$prim54621, align 8
%stackaddr$prim54622 = alloca %struct.ScmObj*, align 8
%anf_45bind40348 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40347)
store volatile %struct.ScmObj* %anf_45bind40348, %struct.ScmObj** %stackaddr$prim54622, align 8
%ae42949 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54623 = alloca %struct.ScmObj*, align 8
%_95040194 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40189, %struct.ScmObj* %ae42949, %struct.ScmObj* %anf_45bind40348)
store volatile %struct.ScmObj* %_95040194, %struct.ScmObj** %stackaddr$prim54623, align 8
%ae42952 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54624 = alloca %struct.ScmObj*, align 8
%anf_45bind40349 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40190, %struct.ScmObj* %ae42952)
store volatile %struct.ScmObj* %anf_45bind40349, %struct.ScmObj** %stackaddr$prim54624, align 8
%ae42954 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54625 = alloca %struct.ScmObj*, align 8
%anf_45bind40350 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind40349, %struct.ScmObj* %ae42954)
store volatile %struct.ScmObj* %anf_45bind40350, %struct.ScmObj** %stackaddr$prim54625, align 8
%ae42956 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54626 = alloca %struct.ScmObj*, align 8
%_95140193 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n40190, %struct.ScmObj* %ae42956, %struct.ScmObj* %anf_45bind40350)
store volatile %struct.ScmObj* %_95140193, %struct.ScmObj** %stackaddr$prim54626, align 8
%args52912$cc40191$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54627 = alloca %struct.ScmObj*, align 8
%args52912$cc40191$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40191, %struct.ScmObj* %args52912$cc40191$0)
store volatile %struct.ScmObj* %args52912$cc40191$1, %struct.ScmObj** %stackaddr$prim54627, align 8
%stackaddr$prim54628 = alloca %struct.ScmObj*, align 8
%args52912$cc40191$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40547, %struct.ScmObj* %args52912$cc40191$1)
store volatile %struct.ScmObj* %args52912$cc40191$2, %struct.ScmObj** %stackaddr$prim54628, align 8
%clofunc54629 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40191)
musttail call tailcc void %clofunc54629(%struct.ScmObj* %cc40191, %struct.ScmObj* %args52912$cc40191$2)
ret void
}

define tailcc void @proc_clo$ae42911(%struct.ScmObj* %env$ae42911,%struct.ScmObj* %current_45args52914) {
%stackaddr$prim54630 = alloca %struct.ScmObj*, align 8
%k40551 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52914)
store volatile %struct.ScmObj* %k40551, %struct.ScmObj** %stackaddr$prim54630, align 8
%stackaddr$prim54631 = alloca %struct.ScmObj*, align 8
%current_45args52915 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52914)
store volatile %struct.ScmObj* %current_45args52915, %struct.ScmObj** %stackaddr$prim54631, align 8
%stackaddr$prim54632 = alloca %struct.ScmObj*, align 8
%u40192 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52915)
store volatile %struct.ScmObj* %u40192, %struct.ScmObj** %stackaddr$prim54632, align 8
%args52917$u40192$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54633 = alloca %struct.ScmObj*, align 8
%args52917$u40192$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u40192, %struct.ScmObj* %args52917$u40192$0)
store volatile %struct.ScmObj* %args52917$u40192$1, %struct.ScmObj** %stackaddr$prim54633, align 8
%stackaddr$prim54634 = alloca %struct.ScmObj*, align 8
%args52917$u40192$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40551, %struct.ScmObj* %args52917$u40192$1)
store volatile %struct.ScmObj* %args52917$u40192$2, %struct.ScmObj** %stackaddr$prim54634, align 8
%clofunc54635 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u40192)
musttail call tailcc void %clofunc54635(%struct.ScmObj* %u40192, %struct.ScmObj* %args52917$u40192$2)
ret void
}

define tailcc void @proc_clo$ae42488(%struct.ScmObj* %env$ae42488,%struct.ScmObj* %current_45args52920) {
%stackaddr$prim54636 = alloca %struct.ScmObj*, align 8
%k40552 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52920)
store volatile %struct.ScmObj* %k40552, %struct.ScmObj** %stackaddr$prim54636, align 8
%stackaddr$prim54637 = alloca %struct.ScmObj*, align 8
%current_45args52921 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52920)
store volatile %struct.ScmObj* %current_45args52921, %struct.ScmObj** %stackaddr$prim54637, align 8
%stackaddr$prim54638 = alloca %struct.ScmObj*, align 8
%a40196 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52921)
store volatile %struct.ScmObj* %a40196, %struct.ScmObj** %stackaddr$prim54638, align 8
%ae42489 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54639 = alloca %struct.ScmObj*, align 8
%a40197 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42489, %struct.ScmObj* %a40196)
store volatile %struct.ScmObj* %a40197, %struct.ScmObj** %stackaddr$prim54639, align 8
%stackaddr$makeclosure54640 = alloca %struct.ScmObj*, align 8
%fptrToInt54641 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42491 to i64
%ae42491 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54641)
store volatile %struct.ScmObj* %ae42491, %struct.ScmObj** %stackaddr$makeclosure54640, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42491, %struct.ScmObj* %k40552, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42491, %struct.ScmObj* %a40197, i64 1)
%ae42492 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54642 = alloca %struct.ScmObj*, align 8
%fptrToInt54643 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42493 to i64
%ae42493 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54643)
store volatile %struct.ScmObj* %ae42493, %struct.ScmObj** %stackaddr$makeclosure54642, align 8
%args52943$ae42491$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54644 = alloca %struct.ScmObj*, align 8
%args52943$ae42491$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42493, %struct.ScmObj* %args52943$ae42491$0)
store volatile %struct.ScmObj* %args52943$ae42491$1, %struct.ScmObj** %stackaddr$prim54644, align 8
%stackaddr$prim54645 = alloca %struct.ScmObj*, align 8
%args52943$ae42491$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42492, %struct.ScmObj* %args52943$ae42491$1)
store volatile %struct.ScmObj* %args52943$ae42491$2, %struct.ScmObj** %stackaddr$prim54645, align 8
%clofunc54646 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42491)
musttail call tailcc void %clofunc54646(%struct.ScmObj* %ae42491, %struct.ScmObj* %args52943$ae42491$2)
ret void
}

define tailcc void @proc_clo$ae42491(%struct.ScmObj* %env$ae42491,%struct.ScmObj* %current_45args52923) {
%stackaddr$env-ref54647 = alloca %struct.ScmObj*, align 8
%k40552 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42491, i64 0)
store %struct.ScmObj* %k40552, %struct.ScmObj** %stackaddr$env-ref54647
%stackaddr$env-ref54648 = alloca %struct.ScmObj*, align 8
%a40197 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42491, i64 1)
store %struct.ScmObj* %a40197, %struct.ScmObj** %stackaddr$env-ref54648
%stackaddr$prim54649 = alloca %struct.ScmObj*, align 8
%_95k40553 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52923)
store volatile %struct.ScmObj* %_95k40553, %struct.ScmObj** %stackaddr$prim54649, align 8
%stackaddr$prim54650 = alloca %struct.ScmObj*, align 8
%current_45args52924 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52923)
store volatile %struct.ScmObj* %current_45args52924, %struct.ScmObj** %stackaddr$prim54650, align 8
%stackaddr$prim54651 = alloca %struct.ScmObj*, align 8
%anf_45bind40336 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52924)
store volatile %struct.ScmObj* %anf_45bind40336, %struct.ScmObj** %stackaddr$prim54651, align 8
%stackaddr$makeclosure54652 = alloca %struct.ScmObj*, align 8
%fptrToInt54653 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42510 to i64
%ae42510 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54653)
store volatile %struct.ScmObj* %ae42510, %struct.ScmObj** %stackaddr$makeclosure54652, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42510, %struct.ScmObj* %k40552, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42510, %struct.ScmObj* %a40197, i64 1)
%stackaddr$makeclosure54654 = alloca %struct.ScmObj*, align 8
%fptrToInt54655 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42511 to i64
%ae42511 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54655)
store volatile %struct.ScmObj* %ae42511, %struct.ScmObj** %stackaddr$makeclosure54654, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42511, %struct.ScmObj* %k40552, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42511, %struct.ScmObj* %a40197, i64 1)
%args52938$anf_45bind40336$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54656 = alloca %struct.ScmObj*, align 8
%args52938$anf_45bind40336$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42511, %struct.ScmObj* %args52938$anf_45bind40336$0)
store volatile %struct.ScmObj* %args52938$anf_45bind40336$1, %struct.ScmObj** %stackaddr$prim54656, align 8
%stackaddr$prim54657 = alloca %struct.ScmObj*, align 8
%args52938$anf_45bind40336$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42510, %struct.ScmObj* %args52938$anf_45bind40336$1)
store volatile %struct.ScmObj* %args52938$anf_45bind40336$2, %struct.ScmObj** %stackaddr$prim54657, align 8
%clofunc54658 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40336)
musttail call tailcc void %clofunc54658(%struct.ScmObj* %anf_45bind40336, %struct.ScmObj* %args52938$anf_45bind40336$2)
ret void
}

define tailcc void @proc_clo$ae42510(%struct.ScmObj* %env$ae42510,%struct.ScmObj* %current_45args52926) {
%stackaddr$env-ref54659 = alloca %struct.ScmObj*, align 8
%k40552 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42510, i64 0)
store %struct.ScmObj* %k40552, %struct.ScmObj** %stackaddr$env-ref54659
%stackaddr$env-ref54660 = alloca %struct.ScmObj*, align 8
%a40197 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42510, i64 1)
store %struct.ScmObj* %a40197, %struct.ScmObj** %stackaddr$env-ref54660
%stackaddr$prim54661 = alloca %struct.ScmObj*, align 8
%_95k40554 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52926)
store volatile %struct.ScmObj* %_95k40554, %struct.ScmObj** %stackaddr$prim54661, align 8
%stackaddr$prim54662 = alloca %struct.ScmObj*, align 8
%current_45args52927 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52926)
store volatile %struct.ScmObj* %current_45args52927, %struct.ScmObj** %stackaddr$prim54662, align 8
%stackaddr$prim54663 = alloca %struct.ScmObj*, align 8
%cc40198 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52927)
store volatile %struct.ScmObj* %cc40198, %struct.ScmObj** %stackaddr$prim54663, align 8
%ae42626 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54664 = alloca %struct.ScmObj*, align 8
%anf_45bind40337 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40197, %struct.ScmObj* %ae42626)
store volatile %struct.ScmObj* %anf_45bind40337, %struct.ScmObj** %stackaddr$prim54664, align 8
%stackaddr$prim54665 = alloca %struct.ScmObj*, align 8
%anf_45bind40338 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40337)
store volatile %struct.ScmObj* %anf_45bind40338, %struct.ScmObj** %stackaddr$prim54665, align 8
%truthy$cmp54666 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40338)
%cmp$cmp54666 = icmp eq i64 %truthy$cmp54666, 1
br i1 %cmp$cmp54666, label %truebranch$cmp54666, label %falsebranch$cmp54666
truebranch$cmp54666:
%ae42630 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42631 = call %struct.ScmObj* @const_init_true()
%args52929$k40552$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54667 = alloca %struct.ScmObj*, align 8
%args52929$k40552$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42631, %struct.ScmObj* %args52929$k40552$0)
store volatile %struct.ScmObj* %args52929$k40552$1, %struct.ScmObj** %stackaddr$prim54667, align 8
%stackaddr$prim54668 = alloca %struct.ScmObj*, align 8
%args52929$k40552$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42630, %struct.ScmObj* %args52929$k40552$1)
store volatile %struct.ScmObj* %args52929$k40552$2, %struct.ScmObj** %stackaddr$prim54668, align 8
%clofunc54669 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40552)
musttail call tailcc void %clofunc54669(%struct.ScmObj* %k40552, %struct.ScmObj* %args52929$k40552$2)
ret void
falsebranch$cmp54666:
%ae42639 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54670 = alloca %struct.ScmObj*, align 8
%anf_45bind40339 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40197, %struct.ScmObj* %ae42639)
store volatile %struct.ScmObj* %anf_45bind40339, %struct.ScmObj** %stackaddr$prim54670, align 8
%stackaddr$prim54671 = alloca %struct.ScmObj*, align 8
%anf_45bind40340 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind40339)
store volatile %struct.ScmObj* %anf_45bind40340, %struct.ScmObj** %stackaddr$prim54671, align 8
%truthy$cmp54672 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40340)
%cmp$cmp54672 = icmp eq i64 %truthy$cmp54672, 1
br i1 %cmp$cmp54672, label %truebranch$cmp54672, label %falsebranch$cmp54672
truebranch$cmp54672:
%ae42643 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54673 = alloca %struct.ScmObj*, align 8
%anf_45bind40341 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40197, %struct.ScmObj* %ae42643)
store volatile %struct.ScmObj* %anf_45bind40341, %struct.ScmObj** %stackaddr$prim54673, align 8
%stackaddr$prim54674 = alloca %struct.ScmObj*, align 8
%b40200 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40341)
store volatile %struct.ScmObj* %b40200, %struct.ScmObj** %stackaddr$prim54674, align 8
%ae42646 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54675 = alloca %struct.ScmObj*, align 8
%anf_45bind40342 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40197, %struct.ScmObj* %ae42646)
store volatile %struct.ScmObj* %anf_45bind40342, %struct.ScmObj** %stackaddr$prim54675, align 8
%stackaddr$prim54676 = alloca %struct.ScmObj*, align 8
%anf_45bind40343 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40342)
store volatile %struct.ScmObj* %anf_45bind40343, %struct.ScmObj** %stackaddr$prim54676, align 8
%ae42649 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54677 = alloca %struct.ScmObj*, align 8
%_95040201 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a40197, %struct.ScmObj* %ae42649, %struct.ScmObj* %anf_45bind40343)
store volatile %struct.ScmObj* %_95040201, %struct.ScmObj** %stackaddr$prim54677, align 8
%args52930$cc40198$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54678 = alloca %struct.ScmObj*, align 8
%args52930$cc40198$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40198, %struct.ScmObj* %args52930$cc40198$0)
store volatile %struct.ScmObj* %args52930$cc40198$1, %struct.ScmObj** %stackaddr$prim54678, align 8
%stackaddr$prim54679 = alloca %struct.ScmObj*, align 8
%args52930$cc40198$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40552, %struct.ScmObj* %args52930$cc40198$1)
store volatile %struct.ScmObj* %args52930$cc40198$2, %struct.ScmObj** %stackaddr$prim54679, align 8
%clofunc54680 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40198)
musttail call tailcc void %clofunc54680(%struct.ScmObj* %cc40198, %struct.ScmObj* %args52930$cc40198$2)
ret void
falsebranch$cmp54672:
%ae42682 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42683 = call %struct.ScmObj* @const_init_false()
%args52931$k40552$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54681 = alloca %struct.ScmObj*, align 8
%args52931$k40552$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42683, %struct.ScmObj* %args52931$k40552$0)
store volatile %struct.ScmObj* %args52931$k40552$1, %struct.ScmObj** %stackaddr$prim54681, align 8
%stackaddr$prim54682 = alloca %struct.ScmObj*, align 8
%args52931$k40552$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42682, %struct.ScmObj* %args52931$k40552$1)
store volatile %struct.ScmObj* %args52931$k40552$2, %struct.ScmObj** %stackaddr$prim54682, align 8
%clofunc54683 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40552)
musttail call tailcc void %clofunc54683(%struct.ScmObj* %k40552, %struct.ScmObj* %args52931$k40552$2)
ret void
}

define tailcc void @proc_clo$ae42511(%struct.ScmObj* %env$ae42511,%struct.ScmObj* %current_45args52932) {
%stackaddr$env-ref54684 = alloca %struct.ScmObj*, align 8
%k40552 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42511, i64 0)
store %struct.ScmObj* %k40552, %struct.ScmObj** %stackaddr$env-ref54684
%stackaddr$env-ref54685 = alloca %struct.ScmObj*, align 8
%a40197 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42511, i64 1)
store %struct.ScmObj* %a40197, %struct.ScmObj** %stackaddr$env-ref54685
%stackaddr$prim54686 = alloca %struct.ScmObj*, align 8
%_95k40554 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52932)
store volatile %struct.ScmObj* %_95k40554, %struct.ScmObj** %stackaddr$prim54686, align 8
%stackaddr$prim54687 = alloca %struct.ScmObj*, align 8
%current_45args52933 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52932)
store volatile %struct.ScmObj* %current_45args52933, %struct.ScmObj** %stackaddr$prim54687, align 8
%stackaddr$prim54688 = alloca %struct.ScmObj*, align 8
%cc40198 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52933)
store volatile %struct.ScmObj* %cc40198, %struct.ScmObj** %stackaddr$prim54688, align 8
%ae42513 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54689 = alloca %struct.ScmObj*, align 8
%anf_45bind40337 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40197, %struct.ScmObj* %ae42513)
store volatile %struct.ScmObj* %anf_45bind40337, %struct.ScmObj** %stackaddr$prim54689, align 8
%stackaddr$prim54690 = alloca %struct.ScmObj*, align 8
%anf_45bind40338 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40337)
store volatile %struct.ScmObj* %anf_45bind40338, %struct.ScmObj** %stackaddr$prim54690, align 8
%truthy$cmp54691 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40338)
%cmp$cmp54691 = icmp eq i64 %truthy$cmp54691, 1
br i1 %cmp$cmp54691, label %truebranch$cmp54691, label %falsebranch$cmp54691
truebranch$cmp54691:
%ae42517 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42518 = call %struct.ScmObj* @const_init_true()
%args52935$k40552$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54692 = alloca %struct.ScmObj*, align 8
%args52935$k40552$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42518, %struct.ScmObj* %args52935$k40552$0)
store volatile %struct.ScmObj* %args52935$k40552$1, %struct.ScmObj** %stackaddr$prim54692, align 8
%stackaddr$prim54693 = alloca %struct.ScmObj*, align 8
%args52935$k40552$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42517, %struct.ScmObj* %args52935$k40552$1)
store volatile %struct.ScmObj* %args52935$k40552$2, %struct.ScmObj** %stackaddr$prim54693, align 8
%clofunc54694 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40552)
musttail call tailcc void %clofunc54694(%struct.ScmObj* %k40552, %struct.ScmObj* %args52935$k40552$2)
ret void
falsebranch$cmp54691:
%ae42526 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54695 = alloca %struct.ScmObj*, align 8
%anf_45bind40339 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40197, %struct.ScmObj* %ae42526)
store volatile %struct.ScmObj* %anf_45bind40339, %struct.ScmObj** %stackaddr$prim54695, align 8
%stackaddr$prim54696 = alloca %struct.ScmObj*, align 8
%anf_45bind40340 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind40339)
store volatile %struct.ScmObj* %anf_45bind40340, %struct.ScmObj** %stackaddr$prim54696, align 8
%truthy$cmp54697 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40340)
%cmp$cmp54697 = icmp eq i64 %truthy$cmp54697, 1
br i1 %cmp$cmp54697, label %truebranch$cmp54697, label %falsebranch$cmp54697
truebranch$cmp54697:
%ae42530 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54698 = alloca %struct.ScmObj*, align 8
%anf_45bind40341 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40197, %struct.ScmObj* %ae42530)
store volatile %struct.ScmObj* %anf_45bind40341, %struct.ScmObj** %stackaddr$prim54698, align 8
%stackaddr$prim54699 = alloca %struct.ScmObj*, align 8
%b40200 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40341)
store volatile %struct.ScmObj* %b40200, %struct.ScmObj** %stackaddr$prim54699, align 8
%ae42533 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54700 = alloca %struct.ScmObj*, align 8
%anf_45bind40342 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40197, %struct.ScmObj* %ae42533)
store volatile %struct.ScmObj* %anf_45bind40342, %struct.ScmObj** %stackaddr$prim54700, align 8
%stackaddr$prim54701 = alloca %struct.ScmObj*, align 8
%anf_45bind40343 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40342)
store volatile %struct.ScmObj* %anf_45bind40343, %struct.ScmObj** %stackaddr$prim54701, align 8
%ae42536 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54702 = alloca %struct.ScmObj*, align 8
%_95040201 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a40197, %struct.ScmObj* %ae42536, %struct.ScmObj* %anf_45bind40343)
store volatile %struct.ScmObj* %_95040201, %struct.ScmObj** %stackaddr$prim54702, align 8
%args52936$cc40198$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54703 = alloca %struct.ScmObj*, align 8
%args52936$cc40198$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40198, %struct.ScmObj* %args52936$cc40198$0)
store volatile %struct.ScmObj* %args52936$cc40198$1, %struct.ScmObj** %stackaddr$prim54703, align 8
%stackaddr$prim54704 = alloca %struct.ScmObj*, align 8
%args52936$cc40198$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40552, %struct.ScmObj* %args52936$cc40198$1)
store volatile %struct.ScmObj* %args52936$cc40198$2, %struct.ScmObj** %stackaddr$prim54704, align 8
%clofunc54705 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40198)
musttail call tailcc void %clofunc54705(%struct.ScmObj* %cc40198, %struct.ScmObj* %args52936$cc40198$2)
ret void
falsebranch$cmp54697:
%ae42569 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42570 = call %struct.ScmObj* @const_init_false()
%args52937$k40552$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54706 = alloca %struct.ScmObj*, align 8
%args52937$k40552$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42570, %struct.ScmObj* %args52937$k40552$0)
store volatile %struct.ScmObj* %args52937$k40552$1, %struct.ScmObj** %stackaddr$prim54706, align 8
%stackaddr$prim54707 = alloca %struct.ScmObj*, align 8
%args52937$k40552$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42569, %struct.ScmObj* %args52937$k40552$1)
store volatile %struct.ScmObj* %args52937$k40552$2, %struct.ScmObj** %stackaddr$prim54707, align 8
%clofunc54708 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40552)
musttail call tailcc void %clofunc54708(%struct.ScmObj* %k40552, %struct.ScmObj* %args52937$k40552$2)
ret void
}

define tailcc void @proc_clo$ae42493(%struct.ScmObj* %env$ae42493,%struct.ScmObj* %current_45args52939) {
%stackaddr$prim54709 = alloca %struct.ScmObj*, align 8
%k40555 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52939)
store volatile %struct.ScmObj* %k40555, %struct.ScmObj** %stackaddr$prim54709, align 8
%stackaddr$prim54710 = alloca %struct.ScmObj*, align 8
%current_45args52940 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52939)
store volatile %struct.ScmObj* %current_45args52940, %struct.ScmObj** %stackaddr$prim54710, align 8
%stackaddr$prim54711 = alloca %struct.ScmObj*, align 8
%k40199 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52940)
store volatile %struct.ScmObj* %k40199, %struct.ScmObj** %stackaddr$prim54711, align 8
%ae42495 = call %struct.ScmObj* @const_init_int(i64 0)
%args52942$k40555$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54712 = alloca %struct.ScmObj*, align 8
%args52942$k40555$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40199, %struct.ScmObj* %args52942$k40555$0)
store volatile %struct.ScmObj* %args52942$k40555$1, %struct.ScmObj** %stackaddr$prim54712, align 8
%stackaddr$prim54713 = alloca %struct.ScmObj*, align 8
%args52942$k40555$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42495, %struct.ScmObj* %args52942$k40555$1)
store volatile %struct.ScmObj* %args52942$k40555$2, %struct.ScmObj** %stackaddr$prim54713, align 8
%clofunc54714 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40555)
musttail call tailcc void %clofunc54714(%struct.ScmObj* %k40555, %struct.ScmObj* %args52942$k40555$2)
ret void
}

define tailcc void @proc_clo$ae42416(%struct.ScmObj* %env$ae42416,%struct.ScmObj* %current_45args52945) {
%stackaddr$env-ref54715 = alloca %struct.ScmObj*, align 8
%_37append40203 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42416, i64 0)
store %struct.ScmObj* %_37append40203, %struct.ScmObj** %stackaddr$env-ref54715
%stackaddr$prim54716 = alloca %struct.ScmObj*, align 8
%k40556 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52945)
store volatile %struct.ScmObj* %k40556, %struct.ScmObj** %stackaddr$prim54716, align 8
%stackaddr$prim54717 = alloca %struct.ScmObj*, align 8
%current_45args52946 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52945)
store volatile %struct.ScmObj* %current_45args52946, %struct.ScmObj** %stackaddr$prim54717, align 8
%stackaddr$prim54718 = alloca %struct.ScmObj*, align 8
%ls040206 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52946)
store volatile %struct.ScmObj* %ls040206, %struct.ScmObj** %stackaddr$prim54718, align 8
%stackaddr$prim54719 = alloca %struct.ScmObj*, align 8
%current_45args52947 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52946)
store volatile %struct.ScmObj* %current_45args52947, %struct.ScmObj** %stackaddr$prim54719, align 8
%stackaddr$prim54720 = alloca %struct.ScmObj*, align 8
%ls140205 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52947)
store volatile %struct.ScmObj* %ls140205, %struct.ScmObj** %stackaddr$prim54720, align 8
%stackaddr$prim54721 = alloca %struct.ScmObj*, align 8
%anf_45bind40330 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls040206)
store volatile %struct.ScmObj* %anf_45bind40330, %struct.ScmObj** %stackaddr$prim54721, align 8
%truthy$cmp54722 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40330)
%cmp$cmp54722 = icmp eq i64 %truthy$cmp54722, 1
br i1 %cmp$cmp54722, label %truebranch$cmp54722, label %falsebranch$cmp54722
truebranch$cmp54722:
%ae42420 = call %struct.ScmObj* @const_init_int(i64 0)
%args52949$k40556$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54723 = alloca %struct.ScmObj*, align 8
%args52949$k40556$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls140205, %struct.ScmObj* %args52949$k40556$0)
store volatile %struct.ScmObj* %args52949$k40556$1, %struct.ScmObj** %stackaddr$prim54723, align 8
%stackaddr$prim54724 = alloca %struct.ScmObj*, align 8
%args52949$k40556$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42420, %struct.ScmObj* %args52949$k40556$1)
store volatile %struct.ScmObj* %args52949$k40556$2, %struct.ScmObj** %stackaddr$prim54724, align 8
%clofunc54725 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40556)
musttail call tailcc void %clofunc54725(%struct.ScmObj* %k40556, %struct.ScmObj* %args52949$k40556$2)
ret void
falsebranch$cmp54722:
%stackaddr$prim54726 = alloca %struct.ScmObj*, align 8
%anf_45bind40331 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls040206)
store volatile %struct.ScmObj* %anf_45bind40331, %struct.ScmObj** %stackaddr$prim54726, align 8
%ae42427 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54727 = alloca %struct.ScmObj*, align 8
%anf_45bind40332 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append40203, %struct.ScmObj* %ae42427)
store volatile %struct.ScmObj* %anf_45bind40332, %struct.ScmObj** %stackaddr$prim54727, align 8
%stackaddr$prim54728 = alloca %struct.ScmObj*, align 8
%anf_45bind40333 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls040206)
store volatile %struct.ScmObj* %anf_45bind40333, %struct.ScmObj** %stackaddr$prim54728, align 8
%stackaddr$makeclosure54729 = alloca %struct.ScmObj*, align 8
%fptrToInt54730 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42430 to i64
%ae42430 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54730)
store volatile %struct.ScmObj* %ae42430, %struct.ScmObj** %stackaddr$makeclosure54729, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42430, %struct.ScmObj* %k40556, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42430, %struct.ScmObj* %anf_45bind40331, i64 1)
%args52954$anf_45bind40332$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54731 = alloca %struct.ScmObj*, align 8
%args52954$anf_45bind40332$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls140205, %struct.ScmObj* %args52954$anf_45bind40332$0)
store volatile %struct.ScmObj* %args52954$anf_45bind40332$1, %struct.ScmObj** %stackaddr$prim54731, align 8
%stackaddr$prim54732 = alloca %struct.ScmObj*, align 8
%args52954$anf_45bind40332$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40333, %struct.ScmObj* %args52954$anf_45bind40332$1)
store volatile %struct.ScmObj* %args52954$anf_45bind40332$2, %struct.ScmObj** %stackaddr$prim54732, align 8
%stackaddr$prim54733 = alloca %struct.ScmObj*, align 8
%args52954$anf_45bind40332$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42430, %struct.ScmObj* %args52954$anf_45bind40332$2)
store volatile %struct.ScmObj* %args52954$anf_45bind40332$3, %struct.ScmObj** %stackaddr$prim54733, align 8
%clofunc54734 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40332)
musttail call tailcc void %clofunc54734(%struct.ScmObj* %anf_45bind40332, %struct.ScmObj* %args52954$anf_45bind40332$3)
ret void
}

define tailcc void @proc_clo$ae42430(%struct.ScmObj* %env$ae42430,%struct.ScmObj* %current_45args52950) {
%stackaddr$env-ref54735 = alloca %struct.ScmObj*, align 8
%k40556 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42430, i64 0)
store %struct.ScmObj* %k40556, %struct.ScmObj** %stackaddr$env-ref54735
%stackaddr$env-ref54736 = alloca %struct.ScmObj*, align 8
%anf_45bind40331 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42430, i64 1)
store %struct.ScmObj* %anf_45bind40331, %struct.ScmObj** %stackaddr$env-ref54736
%stackaddr$prim54737 = alloca %struct.ScmObj*, align 8
%_95k40557 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52950)
store volatile %struct.ScmObj* %_95k40557, %struct.ScmObj** %stackaddr$prim54737, align 8
%stackaddr$prim54738 = alloca %struct.ScmObj*, align 8
%current_45args52951 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52950)
store volatile %struct.ScmObj* %current_45args52951, %struct.ScmObj** %stackaddr$prim54738, align 8
%stackaddr$prim54739 = alloca %struct.ScmObj*, align 8
%anf_45bind40334 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52951)
store volatile %struct.ScmObj* %anf_45bind40334, %struct.ScmObj** %stackaddr$prim54739, align 8
%stackaddr$prim54740 = alloca %struct.ScmObj*, align 8
%cpsprim40558 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40331, %struct.ScmObj* %anf_45bind40334)
store volatile %struct.ScmObj* %cpsprim40558, %struct.ScmObj** %stackaddr$prim54740, align 8
%ae42436 = call %struct.ScmObj* @const_init_int(i64 0)
%args52953$k40556$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54741 = alloca %struct.ScmObj*, align 8
%args52953$k40556$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40558, %struct.ScmObj* %args52953$k40556$0)
store volatile %struct.ScmObj* %args52953$k40556$1, %struct.ScmObj** %stackaddr$prim54741, align 8
%stackaddr$prim54742 = alloca %struct.ScmObj*, align 8
%args52953$k40556$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42436, %struct.ScmObj* %args52953$k40556$1)
store volatile %struct.ScmObj* %args52953$k40556$2, %struct.ScmObj** %stackaddr$prim54742, align 8
%clofunc54743 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40556)
musttail call tailcc void %clofunc54743(%struct.ScmObj* %k40556, %struct.ScmObj* %args52953$k40556$2)
ret void
}

define tailcc void @proc_clo$ae42390(%struct.ScmObj* %env$ae42390,%struct.ScmObj* %current_45args52956) {
%stackaddr$prim54744 = alloca %struct.ScmObj*, align 8
%k40559 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52956)
store volatile %struct.ScmObj* %k40559, %struct.ScmObj** %stackaddr$prim54744, align 8
%stackaddr$prim54745 = alloca %struct.ScmObj*, align 8
%current_45args52957 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52956)
store volatile %struct.ScmObj* %current_45args52957, %struct.ScmObj** %stackaddr$prim54745, align 8
%stackaddr$prim54746 = alloca %struct.ScmObj*, align 8
%a40209 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52957)
store volatile %struct.ScmObj* %a40209, %struct.ScmObj** %stackaddr$prim54746, align 8
%stackaddr$prim54747 = alloca %struct.ScmObj*, align 8
%current_45args52958 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52957)
store volatile %struct.ScmObj* %current_45args52958, %struct.ScmObj** %stackaddr$prim54747, align 8
%stackaddr$prim54748 = alloca %struct.ScmObj*, align 8
%b40208 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52958)
store volatile %struct.ScmObj* %b40208, %struct.ScmObj** %stackaddr$prim54748, align 8
%stackaddr$prim54749 = alloca %struct.ScmObj*, align 8
%anf_45bind40329 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a40209, %struct.ScmObj* %b40208)
store volatile %struct.ScmObj* %anf_45bind40329, %struct.ScmObj** %stackaddr$prim54749, align 8
%stackaddr$prim54750 = alloca %struct.ScmObj*, align 8
%cpsprim40560 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind40329)
store volatile %struct.ScmObj* %cpsprim40560, %struct.ScmObj** %stackaddr$prim54750, align 8
%ae42395 = call %struct.ScmObj* @const_init_int(i64 0)
%args52960$k40559$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54751 = alloca %struct.ScmObj*, align 8
%args52960$k40559$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40560, %struct.ScmObj* %args52960$k40559$0)
store volatile %struct.ScmObj* %args52960$k40559$1, %struct.ScmObj** %stackaddr$prim54751, align 8
%stackaddr$prim54752 = alloca %struct.ScmObj*, align 8
%args52960$k40559$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42395, %struct.ScmObj* %args52960$k40559$1)
store volatile %struct.ScmObj* %args52960$k40559$2, %struct.ScmObj** %stackaddr$prim54752, align 8
%clofunc54753 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40559)
musttail call tailcc void %clofunc54753(%struct.ScmObj* %k40559, %struct.ScmObj* %args52960$k40559$2)
ret void
}

define tailcc void @proc_clo$ae42366(%struct.ScmObj* %env$ae42366,%struct.ScmObj* %current_45args52962) {
%stackaddr$prim54754 = alloca %struct.ScmObj*, align 8
%k40561 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52962)
store volatile %struct.ScmObj* %k40561, %struct.ScmObj** %stackaddr$prim54754, align 8
%stackaddr$prim54755 = alloca %struct.ScmObj*, align 8
%current_45args52963 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52962)
store volatile %struct.ScmObj* %current_45args52963, %struct.ScmObj** %stackaddr$prim54755, align 8
%stackaddr$prim54756 = alloca %struct.ScmObj*, align 8
%a40212 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52963)
store volatile %struct.ScmObj* %a40212, %struct.ScmObj** %stackaddr$prim54756, align 8
%stackaddr$prim54757 = alloca %struct.ScmObj*, align 8
%current_45args52964 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52963)
store volatile %struct.ScmObj* %current_45args52964, %struct.ScmObj** %stackaddr$prim54757, align 8
%stackaddr$prim54758 = alloca %struct.ScmObj*, align 8
%b40211 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52964)
store volatile %struct.ScmObj* %b40211, %struct.ScmObj** %stackaddr$prim54758, align 8
%stackaddr$prim54759 = alloca %struct.ScmObj*, align 8
%anf_45bind40328 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a40212, %struct.ScmObj* %b40211)
store volatile %struct.ScmObj* %anf_45bind40328, %struct.ScmObj** %stackaddr$prim54759, align 8
%stackaddr$prim54760 = alloca %struct.ScmObj*, align 8
%cpsprim40562 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind40328)
store volatile %struct.ScmObj* %cpsprim40562, %struct.ScmObj** %stackaddr$prim54760, align 8
%ae42371 = call %struct.ScmObj* @const_init_int(i64 0)
%args52966$k40561$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54761 = alloca %struct.ScmObj*, align 8
%args52966$k40561$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40562, %struct.ScmObj* %args52966$k40561$0)
store volatile %struct.ScmObj* %args52966$k40561$1, %struct.ScmObj** %stackaddr$prim54761, align 8
%stackaddr$prim54762 = alloca %struct.ScmObj*, align 8
%args52966$k40561$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42371, %struct.ScmObj* %args52966$k40561$1)
store volatile %struct.ScmObj* %args52966$k40561$2, %struct.ScmObj** %stackaddr$prim54762, align 8
%clofunc54763 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40561)
musttail call tailcc void %clofunc54763(%struct.ScmObj* %k40561, %struct.ScmObj* %args52966$k40561$2)
ret void
}

define tailcc void @proc_clo$ae41972(%struct.ScmObj* %env$ae41972,%struct.ScmObj* %current_45args52969) {
%stackaddr$env-ref54764 = alloca %struct.ScmObj*, align 8
%_37foldr40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41972, i64 0)
store %struct.ScmObj* %_37foldr40136, %struct.ScmObj** %stackaddr$env-ref54764
%stackaddr$env-ref54765 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41972, i64 1)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref54765
%stackaddr$env-ref54766 = alloca %struct.ScmObj*, align 8
%_37map140162 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41972, i64 2)
store %struct.ScmObj* %_37map140162, %struct.ScmObj** %stackaddr$env-ref54766
%stackaddr$prim54767 = alloca %struct.ScmObj*, align 8
%k40563 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52969)
store volatile %struct.ScmObj* %k40563, %struct.ScmObj** %stackaddr$prim54767, align 8
%stackaddr$prim54768 = alloca %struct.ScmObj*, align 8
%current_45args52970 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52969)
store volatile %struct.ScmObj* %current_45args52970, %struct.ScmObj** %stackaddr$prim54768, align 8
%stackaddr$prim54769 = alloca %struct.ScmObj*, align 8
%_37foldl40214 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52970)
store volatile %struct.ScmObj* %_37foldl40214, %struct.ScmObj** %stackaddr$prim54769, align 8
%ae41974 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54770 = alloca %struct.ScmObj*, align 8
%fptrToInt54771 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41975 to i64
%ae41975 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54771)
store volatile %struct.ScmObj* %ae41975, %struct.ScmObj** %stackaddr$makeclosure54770, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41975, %struct.ScmObj* %_37foldr40136, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41975, %struct.ScmObj* %_37foldl40214, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41975, %struct.ScmObj* %_37foldr140131, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41975, %struct.ScmObj* %_37map140162, i64 3)
%args53027$k40563$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54772 = alloca %struct.ScmObj*, align 8
%args53027$k40563$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41975, %struct.ScmObj* %args53027$k40563$0)
store volatile %struct.ScmObj* %args53027$k40563$1, %struct.ScmObj** %stackaddr$prim54772, align 8
%stackaddr$prim54773 = alloca %struct.ScmObj*, align 8
%args53027$k40563$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41974, %struct.ScmObj* %args53027$k40563$1)
store volatile %struct.ScmObj* %args53027$k40563$2, %struct.ScmObj** %stackaddr$prim54773, align 8
%clofunc54774 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40563)
musttail call tailcc void %clofunc54774(%struct.ScmObj* %k40563, %struct.ScmObj* %args53027$k40563$2)
ret void
}

define tailcc void @proc_clo$ae41975(%struct.ScmObj* %env$ae41975,%struct.ScmObj* %args4021540564) {
%stackaddr$env-ref54775 = alloca %struct.ScmObj*, align 8
%_37foldr40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41975, i64 0)
store %struct.ScmObj* %_37foldr40136, %struct.ScmObj** %stackaddr$env-ref54775
%stackaddr$env-ref54776 = alloca %struct.ScmObj*, align 8
%_37foldl40214 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41975, i64 1)
store %struct.ScmObj* %_37foldl40214, %struct.ScmObj** %stackaddr$env-ref54776
%stackaddr$env-ref54777 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41975, i64 2)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref54777
%stackaddr$env-ref54778 = alloca %struct.ScmObj*, align 8
%_37map140162 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41975, i64 3)
store %struct.ScmObj* %_37map140162, %struct.ScmObj** %stackaddr$env-ref54778
%stackaddr$prim54779 = alloca %struct.ScmObj*, align 8
%k40565 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4021540564)
store volatile %struct.ScmObj* %k40565, %struct.ScmObj** %stackaddr$prim54779, align 8
%stackaddr$prim54780 = alloca %struct.ScmObj*, align 8
%args40215 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4021540564)
store volatile %struct.ScmObj* %args40215, %struct.ScmObj** %stackaddr$prim54780, align 8
%stackaddr$prim54781 = alloca %struct.ScmObj*, align 8
%f40218 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40215)
store volatile %struct.ScmObj* %f40218, %struct.ScmObj** %stackaddr$prim54781, align 8
%stackaddr$prim54782 = alloca %struct.ScmObj*, align 8
%anf_45bind40316 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40215)
store volatile %struct.ScmObj* %anf_45bind40316, %struct.ScmObj** %stackaddr$prim54782, align 8
%stackaddr$prim54783 = alloca %struct.ScmObj*, align 8
%acc40217 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40316)
store volatile %struct.ScmObj* %acc40217, %struct.ScmObj** %stackaddr$prim54783, align 8
%stackaddr$prim54784 = alloca %struct.ScmObj*, align 8
%anf_45bind40317 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40215)
store volatile %struct.ScmObj* %anf_45bind40317, %struct.ScmObj** %stackaddr$prim54784, align 8
%stackaddr$prim54785 = alloca %struct.ScmObj*, align 8
%lsts40216 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40317)
store volatile %struct.ScmObj* %lsts40216, %struct.ScmObj** %stackaddr$prim54785, align 8
%stackaddr$makeclosure54786 = alloca %struct.ScmObj*, align 8
%fptrToInt54787 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41983 to i64
%ae41983 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt54787)
store volatile %struct.ScmObj* %ae41983, %struct.ScmObj** %stackaddr$makeclosure54786, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41983, %struct.ScmObj* %lsts40216, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41983, %struct.ScmObj* %_37foldr40136, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41983, %struct.ScmObj* %acc40217, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41983, %struct.ScmObj* %_37foldl40214, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41983, %struct.ScmObj* %k40565, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41983, %struct.ScmObj* %_37foldr140131, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41983, %struct.ScmObj* %_37map140162, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae41983, %struct.ScmObj* %f40218, i64 7)
%ae41984 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54788 = alloca %struct.ScmObj*, align 8
%fptrToInt54789 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41985 to i64
%ae41985 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54789)
store volatile %struct.ScmObj* %ae41985, %struct.ScmObj** %stackaddr$makeclosure54788, align 8
%args53026$ae41983$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54790 = alloca %struct.ScmObj*, align 8
%args53026$ae41983$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41985, %struct.ScmObj* %args53026$ae41983$0)
store volatile %struct.ScmObj* %args53026$ae41983$1, %struct.ScmObj** %stackaddr$prim54790, align 8
%stackaddr$prim54791 = alloca %struct.ScmObj*, align 8
%args53026$ae41983$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41984, %struct.ScmObj* %args53026$ae41983$1)
store volatile %struct.ScmObj* %args53026$ae41983$2, %struct.ScmObj** %stackaddr$prim54791, align 8
%clofunc54792 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41983)
musttail call tailcc void %clofunc54792(%struct.ScmObj* %ae41983, %struct.ScmObj* %args53026$ae41983$2)
ret void
}

define tailcc void @proc_clo$ae41983(%struct.ScmObj* %env$ae41983,%struct.ScmObj* %current_45args52972) {
%stackaddr$env-ref54793 = alloca %struct.ScmObj*, align 8
%lsts40216 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41983, i64 0)
store %struct.ScmObj* %lsts40216, %struct.ScmObj** %stackaddr$env-ref54793
%stackaddr$env-ref54794 = alloca %struct.ScmObj*, align 8
%_37foldr40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41983, i64 1)
store %struct.ScmObj* %_37foldr40136, %struct.ScmObj** %stackaddr$env-ref54794
%stackaddr$env-ref54795 = alloca %struct.ScmObj*, align 8
%acc40217 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41983, i64 2)
store %struct.ScmObj* %acc40217, %struct.ScmObj** %stackaddr$env-ref54795
%stackaddr$env-ref54796 = alloca %struct.ScmObj*, align 8
%_37foldl40214 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41983, i64 3)
store %struct.ScmObj* %_37foldl40214, %struct.ScmObj** %stackaddr$env-ref54796
%stackaddr$env-ref54797 = alloca %struct.ScmObj*, align 8
%k40565 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41983, i64 4)
store %struct.ScmObj* %k40565, %struct.ScmObj** %stackaddr$env-ref54797
%stackaddr$env-ref54798 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41983, i64 5)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref54798
%stackaddr$env-ref54799 = alloca %struct.ScmObj*, align 8
%_37map140162 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41983, i64 6)
store %struct.ScmObj* %_37map140162, %struct.ScmObj** %stackaddr$env-ref54799
%stackaddr$env-ref54800 = alloca %struct.ScmObj*, align 8
%f40218 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41983, i64 7)
store %struct.ScmObj* %f40218, %struct.ScmObj** %stackaddr$env-ref54800
%stackaddr$prim54801 = alloca %struct.ScmObj*, align 8
%_95k40566 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52972)
store volatile %struct.ScmObj* %_95k40566, %struct.ScmObj** %stackaddr$prim54801, align 8
%stackaddr$prim54802 = alloca %struct.ScmObj*, align 8
%current_45args52973 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52972)
store volatile %struct.ScmObj* %current_45args52973, %struct.ScmObj** %stackaddr$prim54802, align 8
%stackaddr$prim54803 = alloca %struct.ScmObj*, align 8
%anf_45bind40318 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52973)
store volatile %struct.ScmObj* %anf_45bind40318, %struct.ScmObj** %stackaddr$prim54803, align 8
%stackaddr$makeclosure54804 = alloca %struct.ScmObj*, align 8
%fptrToInt54805 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42015 to i64
%ae42015 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt54805)
store volatile %struct.ScmObj* %ae42015, %struct.ScmObj** %stackaddr$makeclosure54804, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42015, %struct.ScmObj* %lsts40216, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42015, %struct.ScmObj* %_37foldr40136, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42015, %struct.ScmObj* %acc40217, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae42015, %struct.ScmObj* %_37foldl40214, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae42015, %struct.ScmObj* %k40565, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae42015, %struct.ScmObj* %_37map140162, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae42015, %struct.ScmObj* %f40218, i64 6)
%ae42017 = call %struct.ScmObj* @const_init_false()
%args53019$_37foldr140131$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54806 = alloca %struct.ScmObj*, align 8
%args53019$_37foldr140131$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40216, %struct.ScmObj* %args53019$_37foldr140131$0)
store volatile %struct.ScmObj* %args53019$_37foldr140131$1, %struct.ScmObj** %stackaddr$prim54806, align 8
%stackaddr$prim54807 = alloca %struct.ScmObj*, align 8
%args53019$_37foldr140131$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42017, %struct.ScmObj* %args53019$_37foldr140131$1)
store volatile %struct.ScmObj* %args53019$_37foldr140131$2, %struct.ScmObj** %stackaddr$prim54807, align 8
%stackaddr$prim54808 = alloca %struct.ScmObj*, align 8
%args53019$_37foldr140131$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40318, %struct.ScmObj* %args53019$_37foldr140131$2)
store volatile %struct.ScmObj* %args53019$_37foldr140131$3, %struct.ScmObj** %stackaddr$prim54808, align 8
%stackaddr$prim54809 = alloca %struct.ScmObj*, align 8
%args53019$_37foldr140131$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42015, %struct.ScmObj* %args53019$_37foldr140131$3)
store volatile %struct.ScmObj* %args53019$_37foldr140131$4, %struct.ScmObj** %stackaddr$prim54809, align 8
%clofunc54810 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140131)
musttail call tailcc void %clofunc54810(%struct.ScmObj* %_37foldr140131, %struct.ScmObj* %args53019$_37foldr140131$4)
ret void
}

define tailcc void @proc_clo$ae42015(%struct.ScmObj* %env$ae42015,%struct.ScmObj* %current_45args52975) {
%stackaddr$env-ref54811 = alloca %struct.ScmObj*, align 8
%lsts40216 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42015, i64 0)
store %struct.ScmObj* %lsts40216, %struct.ScmObj** %stackaddr$env-ref54811
%stackaddr$env-ref54812 = alloca %struct.ScmObj*, align 8
%_37foldr40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42015, i64 1)
store %struct.ScmObj* %_37foldr40136, %struct.ScmObj** %stackaddr$env-ref54812
%stackaddr$env-ref54813 = alloca %struct.ScmObj*, align 8
%acc40217 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42015, i64 2)
store %struct.ScmObj* %acc40217, %struct.ScmObj** %stackaddr$env-ref54813
%stackaddr$env-ref54814 = alloca %struct.ScmObj*, align 8
%_37foldl40214 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42015, i64 3)
store %struct.ScmObj* %_37foldl40214, %struct.ScmObj** %stackaddr$env-ref54814
%stackaddr$env-ref54815 = alloca %struct.ScmObj*, align 8
%k40565 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42015, i64 4)
store %struct.ScmObj* %k40565, %struct.ScmObj** %stackaddr$env-ref54815
%stackaddr$env-ref54816 = alloca %struct.ScmObj*, align 8
%_37map140162 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42015, i64 5)
store %struct.ScmObj* %_37map140162, %struct.ScmObj** %stackaddr$env-ref54816
%stackaddr$env-ref54817 = alloca %struct.ScmObj*, align 8
%f40218 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42015, i64 6)
store %struct.ScmObj* %f40218, %struct.ScmObj** %stackaddr$env-ref54817
%stackaddr$prim54818 = alloca %struct.ScmObj*, align 8
%_95k40567 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52975)
store volatile %struct.ScmObj* %_95k40567, %struct.ScmObj** %stackaddr$prim54818, align 8
%stackaddr$prim54819 = alloca %struct.ScmObj*, align 8
%current_45args52976 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52975)
store volatile %struct.ScmObj* %current_45args52976, %struct.ScmObj** %stackaddr$prim54819, align 8
%stackaddr$prim54820 = alloca %struct.ScmObj*, align 8
%anf_45bind40319 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52976)
store volatile %struct.ScmObj* %anf_45bind40319, %struct.ScmObj** %stackaddr$prim54820, align 8
%truthy$cmp54821 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40319)
%cmp$cmp54821 = icmp eq i64 %truthy$cmp54821, 1
br i1 %cmp$cmp54821, label %truebranch$cmp54821, label %falsebranch$cmp54821
truebranch$cmp54821:
%ae42026 = call %struct.ScmObj* @const_init_int(i64 0)
%args52978$k40565$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54822 = alloca %struct.ScmObj*, align 8
%args52978$k40565$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40217, %struct.ScmObj* %args52978$k40565$0)
store volatile %struct.ScmObj* %args52978$k40565$1, %struct.ScmObj** %stackaddr$prim54822, align 8
%stackaddr$prim54823 = alloca %struct.ScmObj*, align 8
%args52978$k40565$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42026, %struct.ScmObj* %args52978$k40565$1)
store volatile %struct.ScmObj* %args52978$k40565$2, %struct.ScmObj** %stackaddr$prim54823, align 8
%clofunc54824 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40565)
musttail call tailcc void %clofunc54824(%struct.ScmObj* %k40565, %struct.ScmObj* %args52978$k40565$2)
ret void
falsebranch$cmp54821:
%stackaddr$makeclosure54825 = alloca %struct.ScmObj*, align 8
%fptrToInt54826 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42031 to i64
%ae42031 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt54826)
store volatile %struct.ScmObj* %ae42031, %struct.ScmObj** %stackaddr$makeclosure54825, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42031, %struct.ScmObj* %lsts40216, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42031, %struct.ScmObj* %_37foldr40136, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42031, %struct.ScmObj* %acc40217, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae42031, %struct.ScmObj* %_37foldl40214, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae42031, %struct.ScmObj* %k40565, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae42031, %struct.ScmObj* %_37map140162, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae42031, %struct.ScmObj* %f40218, i64 6)
%ae42032 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54827 = alloca %struct.ScmObj*, align 8
%fptrToInt54828 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42033 to i64
%ae42033 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54828)
store volatile %struct.ScmObj* %ae42033, %struct.ScmObj** %stackaddr$makeclosure54827, align 8
%args53018$ae42031$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54829 = alloca %struct.ScmObj*, align 8
%args53018$ae42031$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42033, %struct.ScmObj* %args53018$ae42031$0)
store volatile %struct.ScmObj* %args53018$ae42031$1, %struct.ScmObj** %stackaddr$prim54829, align 8
%stackaddr$prim54830 = alloca %struct.ScmObj*, align 8
%args53018$ae42031$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42032, %struct.ScmObj* %args53018$ae42031$1)
store volatile %struct.ScmObj* %args53018$ae42031$2, %struct.ScmObj** %stackaddr$prim54830, align 8
%clofunc54831 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42031)
musttail call tailcc void %clofunc54831(%struct.ScmObj* %ae42031, %struct.ScmObj* %args53018$ae42031$2)
ret void
}

define tailcc void @proc_clo$ae42031(%struct.ScmObj* %env$ae42031,%struct.ScmObj* %current_45args52979) {
%stackaddr$env-ref54832 = alloca %struct.ScmObj*, align 8
%lsts40216 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42031, i64 0)
store %struct.ScmObj* %lsts40216, %struct.ScmObj** %stackaddr$env-ref54832
%stackaddr$env-ref54833 = alloca %struct.ScmObj*, align 8
%_37foldr40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42031, i64 1)
store %struct.ScmObj* %_37foldr40136, %struct.ScmObj** %stackaddr$env-ref54833
%stackaddr$env-ref54834 = alloca %struct.ScmObj*, align 8
%acc40217 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42031, i64 2)
store %struct.ScmObj* %acc40217, %struct.ScmObj** %stackaddr$env-ref54834
%stackaddr$env-ref54835 = alloca %struct.ScmObj*, align 8
%_37foldl40214 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42031, i64 3)
store %struct.ScmObj* %_37foldl40214, %struct.ScmObj** %stackaddr$env-ref54835
%stackaddr$env-ref54836 = alloca %struct.ScmObj*, align 8
%k40565 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42031, i64 4)
store %struct.ScmObj* %k40565, %struct.ScmObj** %stackaddr$env-ref54836
%stackaddr$env-ref54837 = alloca %struct.ScmObj*, align 8
%_37map140162 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42031, i64 5)
store %struct.ScmObj* %_37map140162, %struct.ScmObj** %stackaddr$env-ref54837
%stackaddr$env-ref54838 = alloca %struct.ScmObj*, align 8
%f40218 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42031, i64 6)
store %struct.ScmObj* %f40218, %struct.ScmObj** %stackaddr$env-ref54838
%stackaddr$prim54839 = alloca %struct.ScmObj*, align 8
%_95k40568 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52979)
store volatile %struct.ScmObj* %_95k40568, %struct.ScmObj** %stackaddr$prim54839, align 8
%stackaddr$prim54840 = alloca %struct.ScmObj*, align 8
%current_45args52980 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52979)
store volatile %struct.ScmObj* %current_45args52980, %struct.ScmObj** %stackaddr$prim54840, align 8
%stackaddr$prim54841 = alloca %struct.ScmObj*, align 8
%anf_45bind40320 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52980)
store volatile %struct.ScmObj* %anf_45bind40320, %struct.ScmObj** %stackaddr$prim54841, align 8
%stackaddr$makeclosure54842 = alloca %struct.ScmObj*, align 8
%fptrToInt54843 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42052 to i64
%ae42052 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt54843)
store volatile %struct.ScmObj* %ae42052, %struct.ScmObj** %stackaddr$makeclosure54842, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42052, %struct.ScmObj* %lsts40216, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42052, %struct.ScmObj* %_37foldr40136, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42052, %struct.ScmObj* %acc40217, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae42052, %struct.ScmObj* %_37foldl40214, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae42052, %struct.ScmObj* %k40565, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae42052, %struct.ScmObj* %_37map140162, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae42052, %struct.ScmObj* %f40218, i64 6)
%args53013$_37map140162$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54844 = alloca %struct.ScmObj*, align 8
%args53013$_37map140162$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40216, %struct.ScmObj* %args53013$_37map140162$0)
store volatile %struct.ScmObj* %args53013$_37map140162$1, %struct.ScmObj** %stackaddr$prim54844, align 8
%stackaddr$prim54845 = alloca %struct.ScmObj*, align 8
%args53013$_37map140162$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40320, %struct.ScmObj* %args53013$_37map140162$1)
store volatile %struct.ScmObj* %args53013$_37map140162$2, %struct.ScmObj** %stackaddr$prim54845, align 8
%stackaddr$prim54846 = alloca %struct.ScmObj*, align 8
%args53013$_37map140162$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42052, %struct.ScmObj* %args53013$_37map140162$2)
store volatile %struct.ScmObj* %args53013$_37map140162$3, %struct.ScmObj** %stackaddr$prim54846, align 8
%clofunc54847 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140162)
musttail call tailcc void %clofunc54847(%struct.ScmObj* %_37map140162, %struct.ScmObj* %args53013$_37map140162$3)
ret void
}

define tailcc void @proc_clo$ae42052(%struct.ScmObj* %env$ae42052,%struct.ScmObj* %current_45args52982) {
%stackaddr$env-ref54848 = alloca %struct.ScmObj*, align 8
%lsts40216 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42052, i64 0)
store %struct.ScmObj* %lsts40216, %struct.ScmObj** %stackaddr$env-ref54848
%stackaddr$env-ref54849 = alloca %struct.ScmObj*, align 8
%_37foldr40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42052, i64 1)
store %struct.ScmObj* %_37foldr40136, %struct.ScmObj** %stackaddr$env-ref54849
%stackaddr$env-ref54850 = alloca %struct.ScmObj*, align 8
%acc40217 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42052, i64 2)
store %struct.ScmObj* %acc40217, %struct.ScmObj** %stackaddr$env-ref54850
%stackaddr$env-ref54851 = alloca %struct.ScmObj*, align 8
%_37foldl40214 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42052, i64 3)
store %struct.ScmObj* %_37foldl40214, %struct.ScmObj** %stackaddr$env-ref54851
%stackaddr$env-ref54852 = alloca %struct.ScmObj*, align 8
%k40565 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42052, i64 4)
store %struct.ScmObj* %k40565, %struct.ScmObj** %stackaddr$env-ref54852
%stackaddr$env-ref54853 = alloca %struct.ScmObj*, align 8
%_37map140162 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42052, i64 5)
store %struct.ScmObj* %_37map140162, %struct.ScmObj** %stackaddr$env-ref54853
%stackaddr$env-ref54854 = alloca %struct.ScmObj*, align 8
%f40218 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42052, i64 6)
store %struct.ScmObj* %f40218, %struct.ScmObj** %stackaddr$env-ref54854
%stackaddr$prim54855 = alloca %struct.ScmObj*, align 8
%_95k40569 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52982)
store volatile %struct.ScmObj* %_95k40569, %struct.ScmObj** %stackaddr$prim54855, align 8
%stackaddr$prim54856 = alloca %struct.ScmObj*, align 8
%current_45args52983 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52982)
store volatile %struct.ScmObj* %current_45args52983, %struct.ScmObj** %stackaddr$prim54856, align 8
%stackaddr$prim54857 = alloca %struct.ScmObj*, align 8
%lsts_4340223 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52983)
store volatile %struct.ScmObj* %lsts_4340223, %struct.ScmObj** %stackaddr$prim54857, align 8
%stackaddr$makeclosure54858 = alloca %struct.ScmObj*, align 8
%fptrToInt54859 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42055 to i64
%ae42055 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt54859)
store volatile %struct.ScmObj* %ae42055, %struct.ScmObj** %stackaddr$makeclosure54858, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42055, %struct.ScmObj* %lsts40216, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42055, %struct.ScmObj* %_37foldr40136, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42055, %struct.ScmObj* %acc40217, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae42055, %struct.ScmObj* %_37foldl40214, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae42055, %struct.ScmObj* %k40565, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae42055, %struct.ScmObj* %_37map140162, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae42055, %struct.ScmObj* %lsts_4340223, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae42055, %struct.ScmObj* %f40218, i64 7)
%ae42056 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54860 = alloca %struct.ScmObj*, align 8
%fptrToInt54861 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42057 to i64
%ae42057 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54861)
store volatile %struct.ScmObj* %ae42057, %struct.ScmObj** %stackaddr$makeclosure54860, align 8
%args53012$ae42055$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54862 = alloca %struct.ScmObj*, align 8
%args53012$ae42055$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42057, %struct.ScmObj* %args53012$ae42055$0)
store volatile %struct.ScmObj* %args53012$ae42055$1, %struct.ScmObj** %stackaddr$prim54862, align 8
%stackaddr$prim54863 = alloca %struct.ScmObj*, align 8
%args53012$ae42055$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42056, %struct.ScmObj* %args53012$ae42055$1)
store volatile %struct.ScmObj* %args53012$ae42055$2, %struct.ScmObj** %stackaddr$prim54863, align 8
%clofunc54864 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42055)
musttail call tailcc void %clofunc54864(%struct.ScmObj* %ae42055, %struct.ScmObj* %args53012$ae42055$2)
ret void
}

define tailcc void @proc_clo$ae42055(%struct.ScmObj* %env$ae42055,%struct.ScmObj* %current_45args52985) {
%stackaddr$env-ref54865 = alloca %struct.ScmObj*, align 8
%lsts40216 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42055, i64 0)
store %struct.ScmObj* %lsts40216, %struct.ScmObj** %stackaddr$env-ref54865
%stackaddr$env-ref54866 = alloca %struct.ScmObj*, align 8
%_37foldr40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42055, i64 1)
store %struct.ScmObj* %_37foldr40136, %struct.ScmObj** %stackaddr$env-ref54866
%stackaddr$env-ref54867 = alloca %struct.ScmObj*, align 8
%acc40217 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42055, i64 2)
store %struct.ScmObj* %acc40217, %struct.ScmObj** %stackaddr$env-ref54867
%stackaddr$env-ref54868 = alloca %struct.ScmObj*, align 8
%_37foldl40214 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42055, i64 3)
store %struct.ScmObj* %_37foldl40214, %struct.ScmObj** %stackaddr$env-ref54868
%stackaddr$env-ref54869 = alloca %struct.ScmObj*, align 8
%k40565 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42055, i64 4)
store %struct.ScmObj* %k40565, %struct.ScmObj** %stackaddr$env-ref54869
%stackaddr$env-ref54870 = alloca %struct.ScmObj*, align 8
%_37map140162 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42055, i64 5)
store %struct.ScmObj* %_37map140162, %struct.ScmObj** %stackaddr$env-ref54870
%stackaddr$env-ref54871 = alloca %struct.ScmObj*, align 8
%lsts_4340223 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42055, i64 6)
store %struct.ScmObj* %lsts_4340223, %struct.ScmObj** %stackaddr$env-ref54871
%stackaddr$env-ref54872 = alloca %struct.ScmObj*, align 8
%f40218 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42055, i64 7)
store %struct.ScmObj* %f40218, %struct.ScmObj** %stackaddr$env-ref54872
%stackaddr$prim54873 = alloca %struct.ScmObj*, align 8
%_95k40570 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52985)
store volatile %struct.ScmObj* %_95k40570, %struct.ScmObj** %stackaddr$prim54873, align 8
%stackaddr$prim54874 = alloca %struct.ScmObj*, align 8
%current_45args52986 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52985)
store volatile %struct.ScmObj* %current_45args52986, %struct.ScmObj** %stackaddr$prim54874, align 8
%stackaddr$prim54875 = alloca %struct.ScmObj*, align 8
%anf_45bind40321 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52986)
store volatile %struct.ScmObj* %anf_45bind40321, %struct.ScmObj** %stackaddr$prim54875, align 8
%stackaddr$makeclosure54876 = alloca %struct.ScmObj*, align 8
%fptrToInt54877 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42076 to i64
%ae42076 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt54877)
store volatile %struct.ScmObj* %ae42076, %struct.ScmObj** %stackaddr$makeclosure54876, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42076, %struct.ScmObj* %acc40217, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42076, %struct.ScmObj* %_37foldr40136, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42076, %struct.ScmObj* %_37foldl40214, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae42076, %struct.ScmObj* %k40565, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae42076, %struct.ScmObj* %lsts_4340223, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae42076, %struct.ScmObj* %f40218, i64 5)
%args53007$_37map140162$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54878 = alloca %struct.ScmObj*, align 8
%args53007$_37map140162$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40216, %struct.ScmObj* %args53007$_37map140162$0)
store volatile %struct.ScmObj* %args53007$_37map140162$1, %struct.ScmObj** %stackaddr$prim54878, align 8
%stackaddr$prim54879 = alloca %struct.ScmObj*, align 8
%args53007$_37map140162$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40321, %struct.ScmObj* %args53007$_37map140162$1)
store volatile %struct.ScmObj* %args53007$_37map140162$2, %struct.ScmObj** %stackaddr$prim54879, align 8
%stackaddr$prim54880 = alloca %struct.ScmObj*, align 8
%args53007$_37map140162$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42076, %struct.ScmObj* %args53007$_37map140162$2)
store volatile %struct.ScmObj* %args53007$_37map140162$3, %struct.ScmObj** %stackaddr$prim54880, align 8
%clofunc54881 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140162)
musttail call tailcc void %clofunc54881(%struct.ScmObj* %_37map140162, %struct.ScmObj* %args53007$_37map140162$3)
ret void
}

define tailcc void @proc_clo$ae42076(%struct.ScmObj* %env$ae42076,%struct.ScmObj* %current_45args52988) {
%stackaddr$env-ref54882 = alloca %struct.ScmObj*, align 8
%acc40217 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42076, i64 0)
store %struct.ScmObj* %acc40217, %struct.ScmObj** %stackaddr$env-ref54882
%stackaddr$env-ref54883 = alloca %struct.ScmObj*, align 8
%_37foldr40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42076, i64 1)
store %struct.ScmObj* %_37foldr40136, %struct.ScmObj** %stackaddr$env-ref54883
%stackaddr$env-ref54884 = alloca %struct.ScmObj*, align 8
%_37foldl40214 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42076, i64 2)
store %struct.ScmObj* %_37foldl40214, %struct.ScmObj** %stackaddr$env-ref54884
%stackaddr$env-ref54885 = alloca %struct.ScmObj*, align 8
%k40565 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42076, i64 3)
store %struct.ScmObj* %k40565, %struct.ScmObj** %stackaddr$env-ref54885
%stackaddr$env-ref54886 = alloca %struct.ScmObj*, align 8
%lsts_4340223 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42076, i64 4)
store %struct.ScmObj* %lsts_4340223, %struct.ScmObj** %stackaddr$env-ref54886
%stackaddr$env-ref54887 = alloca %struct.ScmObj*, align 8
%f40218 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42076, i64 5)
store %struct.ScmObj* %f40218, %struct.ScmObj** %stackaddr$env-ref54887
%stackaddr$prim54888 = alloca %struct.ScmObj*, align 8
%_95k40571 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52988)
store volatile %struct.ScmObj* %_95k40571, %struct.ScmObj** %stackaddr$prim54888, align 8
%stackaddr$prim54889 = alloca %struct.ScmObj*, align 8
%current_45args52989 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52988)
store volatile %struct.ScmObj* %current_45args52989, %struct.ScmObj** %stackaddr$prim54889, align 8
%stackaddr$prim54890 = alloca %struct.ScmObj*, align 8
%vs40221 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52989)
store volatile %struct.ScmObj* %vs40221, %struct.ScmObj** %stackaddr$prim54890, align 8
%stackaddr$makeclosure54891 = alloca %struct.ScmObj*, align 8
%fptrToInt54892 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42079 to i64
%ae42079 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt54892)
store volatile %struct.ScmObj* %ae42079, %struct.ScmObj** %stackaddr$makeclosure54891, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42079, %struct.ScmObj* %acc40217, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42079, %struct.ScmObj* %_37foldr40136, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42079, %struct.ScmObj* %_37foldl40214, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae42079, %struct.ScmObj* %k40565, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae42079, %struct.ScmObj* %lsts_4340223, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae42079, %struct.ScmObj* %vs40221, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae42079, %struct.ScmObj* %f40218, i64 6)
%ae42080 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54893 = alloca %struct.ScmObj*, align 8
%fptrToInt54894 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42081 to i64
%ae42081 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54894)
store volatile %struct.ScmObj* %ae42081, %struct.ScmObj** %stackaddr$makeclosure54893, align 8
%args53006$ae42079$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54895 = alloca %struct.ScmObj*, align 8
%args53006$ae42079$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42081, %struct.ScmObj* %args53006$ae42079$0)
store volatile %struct.ScmObj* %args53006$ae42079$1, %struct.ScmObj** %stackaddr$prim54895, align 8
%stackaddr$prim54896 = alloca %struct.ScmObj*, align 8
%args53006$ae42079$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42080, %struct.ScmObj* %args53006$ae42079$1)
store volatile %struct.ScmObj* %args53006$ae42079$2, %struct.ScmObj** %stackaddr$prim54896, align 8
%clofunc54897 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42079)
musttail call tailcc void %clofunc54897(%struct.ScmObj* %ae42079, %struct.ScmObj* %args53006$ae42079$2)
ret void
}

define tailcc void @proc_clo$ae42079(%struct.ScmObj* %env$ae42079,%struct.ScmObj* %current_45args52991) {
%stackaddr$env-ref54898 = alloca %struct.ScmObj*, align 8
%acc40217 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42079, i64 0)
store %struct.ScmObj* %acc40217, %struct.ScmObj** %stackaddr$env-ref54898
%stackaddr$env-ref54899 = alloca %struct.ScmObj*, align 8
%_37foldr40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42079, i64 1)
store %struct.ScmObj* %_37foldr40136, %struct.ScmObj** %stackaddr$env-ref54899
%stackaddr$env-ref54900 = alloca %struct.ScmObj*, align 8
%_37foldl40214 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42079, i64 2)
store %struct.ScmObj* %_37foldl40214, %struct.ScmObj** %stackaddr$env-ref54900
%stackaddr$env-ref54901 = alloca %struct.ScmObj*, align 8
%k40565 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42079, i64 3)
store %struct.ScmObj* %k40565, %struct.ScmObj** %stackaddr$env-ref54901
%stackaddr$env-ref54902 = alloca %struct.ScmObj*, align 8
%lsts_4340223 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42079, i64 4)
store %struct.ScmObj* %lsts_4340223, %struct.ScmObj** %stackaddr$env-ref54902
%stackaddr$env-ref54903 = alloca %struct.ScmObj*, align 8
%vs40221 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42079, i64 5)
store %struct.ScmObj* %vs40221, %struct.ScmObj** %stackaddr$env-ref54903
%stackaddr$env-ref54904 = alloca %struct.ScmObj*, align 8
%f40218 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42079, i64 6)
store %struct.ScmObj* %f40218, %struct.ScmObj** %stackaddr$env-ref54904
%stackaddr$prim54905 = alloca %struct.ScmObj*, align 8
%_95k40572 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52991)
store volatile %struct.ScmObj* %_95k40572, %struct.ScmObj** %stackaddr$prim54905, align 8
%stackaddr$prim54906 = alloca %struct.ScmObj*, align 8
%current_45args52992 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52991)
store volatile %struct.ScmObj* %current_45args52992, %struct.ScmObj** %stackaddr$prim54906, align 8
%stackaddr$prim54907 = alloca %struct.ScmObj*, align 8
%anf_45bind40322 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52992)
store volatile %struct.ScmObj* %anf_45bind40322, %struct.ScmObj** %stackaddr$prim54907, align 8
%ae42102 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54908 = alloca %struct.ScmObj*, align 8
%anf_45bind40323 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40217, %struct.ScmObj* %ae42102)
store volatile %struct.ScmObj* %anf_45bind40323, %struct.ScmObj** %stackaddr$prim54908, align 8
%stackaddr$makeclosure54909 = alloca %struct.ScmObj*, align 8
%fptrToInt54910 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42104 to i64
%ae42104 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54910)
store volatile %struct.ScmObj* %ae42104, %struct.ScmObj** %stackaddr$makeclosure54909, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42104, %struct.ScmObj* %_37foldl40214, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42104, %struct.ScmObj* %k40565, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42104, %struct.ScmObj* %lsts_4340223, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae42104, %struct.ScmObj* %f40218, i64 3)
%args53000$_37foldr40136$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54911 = alloca %struct.ScmObj*, align 8
%args53000$_37foldr40136$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs40221, %struct.ScmObj* %args53000$_37foldr40136$0)
store volatile %struct.ScmObj* %args53000$_37foldr40136$1, %struct.ScmObj** %stackaddr$prim54911, align 8
%stackaddr$prim54912 = alloca %struct.ScmObj*, align 8
%args53000$_37foldr40136$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40323, %struct.ScmObj* %args53000$_37foldr40136$1)
store volatile %struct.ScmObj* %args53000$_37foldr40136$2, %struct.ScmObj** %stackaddr$prim54912, align 8
%stackaddr$prim54913 = alloca %struct.ScmObj*, align 8
%args53000$_37foldr40136$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40322, %struct.ScmObj* %args53000$_37foldr40136$2)
store volatile %struct.ScmObj* %args53000$_37foldr40136$3, %struct.ScmObj** %stackaddr$prim54913, align 8
%stackaddr$prim54914 = alloca %struct.ScmObj*, align 8
%args53000$_37foldr40136$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42104, %struct.ScmObj* %args53000$_37foldr40136$3)
store volatile %struct.ScmObj* %args53000$_37foldr40136$4, %struct.ScmObj** %stackaddr$prim54914, align 8
%clofunc54915 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr40136)
musttail call tailcc void %clofunc54915(%struct.ScmObj* %_37foldr40136, %struct.ScmObj* %args53000$_37foldr40136$4)
ret void
}

define tailcc void @proc_clo$ae42104(%struct.ScmObj* %env$ae42104,%struct.ScmObj* %current_45args52994) {
%stackaddr$env-ref54916 = alloca %struct.ScmObj*, align 8
%_37foldl40214 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42104, i64 0)
store %struct.ScmObj* %_37foldl40214, %struct.ScmObj** %stackaddr$env-ref54916
%stackaddr$env-ref54917 = alloca %struct.ScmObj*, align 8
%k40565 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42104, i64 1)
store %struct.ScmObj* %k40565, %struct.ScmObj** %stackaddr$env-ref54917
%stackaddr$env-ref54918 = alloca %struct.ScmObj*, align 8
%lsts_4340223 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42104, i64 2)
store %struct.ScmObj* %lsts_4340223, %struct.ScmObj** %stackaddr$env-ref54918
%stackaddr$env-ref54919 = alloca %struct.ScmObj*, align 8
%f40218 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42104, i64 3)
store %struct.ScmObj* %f40218, %struct.ScmObj** %stackaddr$env-ref54919
%stackaddr$prim54920 = alloca %struct.ScmObj*, align 8
%_95k40573 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52994)
store volatile %struct.ScmObj* %_95k40573, %struct.ScmObj** %stackaddr$prim54920, align 8
%stackaddr$prim54921 = alloca %struct.ScmObj*, align 8
%current_45args52995 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52994)
store volatile %struct.ScmObj* %current_45args52995, %struct.ScmObj** %stackaddr$prim54921, align 8
%stackaddr$prim54922 = alloca %struct.ScmObj*, align 8
%anf_45bind40324 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52995)
store volatile %struct.ScmObj* %anf_45bind40324, %struct.ScmObj** %stackaddr$prim54922, align 8
%stackaddr$makeclosure54923 = alloca %struct.ScmObj*, align 8
%fptrToInt54924 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42108 to i64
%ae42108 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54924)
store volatile %struct.ScmObj* %ae42108, %struct.ScmObj** %stackaddr$makeclosure54923, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42108, %struct.ScmObj* %_37foldl40214, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42108, %struct.ScmObj* %k40565, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42108, %struct.ScmObj* %lsts_4340223, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae42108, %struct.ScmObj* %f40218, i64 3)
%stackaddr$prim54925 = alloca %struct.ScmObj*, align 8
%cpsargs40576 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42108, %struct.ScmObj* %anf_45bind40324)
store volatile %struct.ScmObj* %cpsargs40576, %struct.ScmObj** %stackaddr$prim54925, align 8
%clofunc54926 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40218)
musttail call tailcc void %clofunc54926(%struct.ScmObj* %f40218, %struct.ScmObj* %cpsargs40576)
ret void
}

define tailcc void @proc_clo$ae42108(%struct.ScmObj* %env$ae42108,%struct.ScmObj* %current_45args52997) {
%stackaddr$env-ref54927 = alloca %struct.ScmObj*, align 8
%_37foldl40214 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42108, i64 0)
store %struct.ScmObj* %_37foldl40214, %struct.ScmObj** %stackaddr$env-ref54927
%stackaddr$env-ref54928 = alloca %struct.ScmObj*, align 8
%k40565 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42108, i64 1)
store %struct.ScmObj* %k40565, %struct.ScmObj** %stackaddr$env-ref54928
%stackaddr$env-ref54929 = alloca %struct.ScmObj*, align 8
%lsts_4340223 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42108, i64 2)
store %struct.ScmObj* %lsts_4340223, %struct.ScmObj** %stackaddr$env-ref54929
%stackaddr$env-ref54930 = alloca %struct.ScmObj*, align 8
%f40218 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42108, i64 3)
store %struct.ScmObj* %f40218, %struct.ScmObj** %stackaddr$env-ref54930
%stackaddr$prim54931 = alloca %struct.ScmObj*, align 8
%_95k40574 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52997)
store volatile %struct.ScmObj* %_95k40574, %struct.ScmObj** %stackaddr$prim54931, align 8
%stackaddr$prim54932 = alloca %struct.ScmObj*, align 8
%current_45args52998 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args52997)
store volatile %struct.ScmObj* %current_45args52998, %struct.ScmObj** %stackaddr$prim54932, align 8
%stackaddr$prim54933 = alloca %struct.ScmObj*, align 8
%acc_4340225 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args52998)
store volatile %struct.ScmObj* %acc_4340225, %struct.ScmObj** %stackaddr$prim54933, align 8
%stackaddr$prim54934 = alloca %struct.ScmObj*, align 8
%anf_45bind40325 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4340225, %struct.ScmObj* %lsts_4340223)
store volatile %struct.ScmObj* %anf_45bind40325, %struct.ScmObj** %stackaddr$prim54934, align 8
%stackaddr$prim54935 = alloca %struct.ScmObj*, align 8
%anf_45bind40326 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40218, %struct.ScmObj* %anf_45bind40325)
store volatile %struct.ScmObj* %anf_45bind40326, %struct.ScmObj** %stackaddr$prim54935, align 8
%stackaddr$prim54936 = alloca %struct.ScmObj*, align 8
%cpsargs40575 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40565, %struct.ScmObj* %anf_45bind40326)
store volatile %struct.ScmObj* %cpsargs40575, %struct.ScmObj** %stackaddr$prim54936, align 8
%clofunc54937 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl40214)
musttail call tailcc void %clofunc54937(%struct.ScmObj* %_37foldl40214, %struct.ScmObj* %cpsargs40575)
ret void
}

define tailcc void @proc_clo$ae42081(%struct.ScmObj* %env$ae42081,%struct.ScmObj* %current_45args53001) {
%stackaddr$prim54938 = alloca %struct.ScmObj*, align 8
%k40577 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53001)
store volatile %struct.ScmObj* %k40577, %struct.ScmObj** %stackaddr$prim54938, align 8
%stackaddr$prim54939 = alloca %struct.ScmObj*, align 8
%current_45args53002 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53001)
store volatile %struct.ScmObj* %current_45args53002, %struct.ScmObj** %stackaddr$prim54939, align 8
%stackaddr$prim54940 = alloca %struct.ScmObj*, align 8
%a40227 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53002)
store volatile %struct.ScmObj* %a40227, %struct.ScmObj** %stackaddr$prim54940, align 8
%stackaddr$prim54941 = alloca %struct.ScmObj*, align 8
%current_45args53003 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53002)
store volatile %struct.ScmObj* %current_45args53003, %struct.ScmObj** %stackaddr$prim54941, align 8
%stackaddr$prim54942 = alloca %struct.ScmObj*, align 8
%b40226 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53003)
store volatile %struct.ScmObj* %b40226, %struct.ScmObj** %stackaddr$prim54942, align 8
%stackaddr$prim54943 = alloca %struct.ScmObj*, align 8
%cpsprim40578 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a40227, %struct.ScmObj* %b40226)
store volatile %struct.ScmObj* %cpsprim40578, %struct.ScmObj** %stackaddr$prim54943, align 8
%ae42085 = call %struct.ScmObj* @const_init_int(i64 0)
%args53005$k40577$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54944 = alloca %struct.ScmObj*, align 8
%args53005$k40577$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40578, %struct.ScmObj* %args53005$k40577$0)
store volatile %struct.ScmObj* %args53005$k40577$1, %struct.ScmObj** %stackaddr$prim54944, align 8
%stackaddr$prim54945 = alloca %struct.ScmObj*, align 8
%args53005$k40577$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42085, %struct.ScmObj* %args53005$k40577$1)
store volatile %struct.ScmObj* %args53005$k40577$2, %struct.ScmObj** %stackaddr$prim54945, align 8
%clofunc54946 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40577)
musttail call tailcc void %clofunc54946(%struct.ScmObj* %k40577, %struct.ScmObj* %args53005$k40577$2)
ret void
}

define tailcc void @proc_clo$ae42057(%struct.ScmObj* %env$ae42057,%struct.ScmObj* %current_45args53008) {
%stackaddr$prim54947 = alloca %struct.ScmObj*, align 8
%k40579 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53008)
store volatile %struct.ScmObj* %k40579, %struct.ScmObj** %stackaddr$prim54947, align 8
%stackaddr$prim54948 = alloca %struct.ScmObj*, align 8
%current_45args53009 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53008)
store volatile %struct.ScmObj* %current_45args53009, %struct.ScmObj** %stackaddr$prim54948, align 8
%stackaddr$prim54949 = alloca %struct.ScmObj*, align 8
%x40222 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53009)
store volatile %struct.ScmObj* %x40222, %struct.ScmObj** %stackaddr$prim54949, align 8
%stackaddr$prim54950 = alloca %struct.ScmObj*, align 8
%cpsprim40580 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40222)
store volatile %struct.ScmObj* %cpsprim40580, %struct.ScmObj** %stackaddr$prim54950, align 8
%ae42060 = call %struct.ScmObj* @const_init_int(i64 0)
%args53011$k40579$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54951 = alloca %struct.ScmObj*, align 8
%args53011$k40579$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40580, %struct.ScmObj* %args53011$k40579$0)
store volatile %struct.ScmObj* %args53011$k40579$1, %struct.ScmObj** %stackaddr$prim54951, align 8
%stackaddr$prim54952 = alloca %struct.ScmObj*, align 8
%args53011$k40579$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42060, %struct.ScmObj* %args53011$k40579$1)
store volatile %struct.ScmObj* %args53011$k40579$2, %struct.ScmObj** %stackaddr$prim54952, align 8
%clofunc54953 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40579)
musttail call tailcc void %clofunc54953(%struct.ScmObj* %k40579, %struct.ScmObj* %args53011$k40579$2)
ret void
}

define tailcc void @proc_clo$ae42033(%struct.ScmObj* %env$ae42033,%struct.ScmObj* %current_45args53014) {
%stackaddr$prim54954 = alloca %struct.ScmObj*, align 8
%k40581 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53014)
store volatile %struct.ScmObj* %k40581, %struct.ScmObj** %stackaddr$prim54954, align 8
%stackaddr$prim54955 = alloca %struct.ScmObj*, align 8
%current_45args53015 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53014)
store volatile %struct.ScmObj* %current_45args53015, %struct.ScmObj** %stackaddr$prim54955, align 8
%stackaddr$prim54956 = alloca %struct.ScmObj*, align 8
%x40224 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53015)
store volatile %struct.ScmObj* %x40224, %struct.ScmObj** %stackaddr$prim54956, align 8
%stackaddr$prim54957 = alloca %struct.ScmObj*, align 8
%cpsprim40582 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40224)
store volatile %struct.ScmObj* %cpsprim40582, %struct.ScmObj** %stackaddr$prim54957, align 8
%ae42036 = call %struct.ScmObj* @const_init_int(i64 0)
%args53017$k40581$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54958 = alloca %struct.ScmObj*, align 8
%args53017$k40581$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40582, %struct.ScmObj* %args53017$k40581$0)
store volatile %struct.ScmObj* %args53017$k40581$1, %struct.ScmObj** %stackaddr$prim54958, align 8
%stackaddr$prim54959 = alloca %struct.ScmObj*, align 8
%args53017$k40581$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42036, %struct.ScmObj* %args53017$k40581$1)
store volatile %struct.ScmObj* %args53017$k40581$2, %struct.ScmObj** %stackaddr$prim54959, align 8
%clofunc54960 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40581)
musttail call tailcc void %clofunc54960(%struct.ScmObj* %k40581, %struct.ScmObj* %args53017$k40581$2)
ret void
}

define tailcc void @proc_clo$ae41985(%struct.ScmObj* %env$ae41985,%struct.ScmObj* %current_45args53020) {
%stackaddr$prim54961 = alloca %struct.ScmObj*, align 8
%k40583 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53020)
store volatile %struct.ScmObj* %k40583, %struct.ScmObj** %stackaddr$prim54961, align 8
%stackaddr$prim54962 = alloca %struct.ScmObj*, align 8
%current_45args53021 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53020)
store volatile %struct.ScmObj* %current_45args53021, %struct.ScmObj** %stackaddr$prim54962, align 8
%stackaddr$prim54963 = alloca %struct.ScmObj*, align 8
%lst40220 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53021)
store volatile %struct.ScmObj* %lst40220, %struct.ScmObj** %stackaddr$prim54963, align 8
%stackaddr$prim54964 = alloca %struct.ScmObj*, align 8
%current_45args53022 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53021)
store volatile %struct.ScmObj* %current_45args53022, %struct.ScmObj** %stackaddr$prim54964, align 8
%stackaddr$prim54965 = alloca %struct.ScmObj*, align 8
%b40219 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53022)
store volatile %struct.ScmObj* %b40219, %struct.ScmObj** %stackaddr$prim54965, align 8
%truthy$cmp54966 = call i64 @is_truthy_value(%struct.ScmObj* %b40219)
%cmp$cmp54966 = icmp eq i64 %truthy$cmp54966, 1
br i1 %cmp$cmp54966, label %truebranch$cmp54966, label %falsebranch$cmp54966
truebranch$cmp54966:
%ae41988 = call %struct.ScmObj* @const_init_int(i64 0)
%args53024$k40583$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54967 = alloca %struct.ScmObj*, align 8
%args53024$k40583$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b40219, %struct.ScmObj* %args53024$k40583$0)
store volatile %struct.ScmObj* %args53024$k40583$1, %struct.ScmObj** %stackaddr$prim54967, align 8
%stackaddr$prim54968 = alloca %struct.ScmObj*, align 8
%args53024$k40583$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41988, %struct.ScmObj* %args53024$k40583$1)
store volatile %struct.ScmObj* %args53024$k40583$2, %struct.ScmObj** %stackaddr$prim54968, align 8
%clofunc54969 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40583)
musttail call tailcc void %clofunc54969(%struct.ScmObj* %k40583, %struct.ScmObj* %args53024$k40583$2)
ret void
falsebranch$cmp54966:
%stackaddr$prim54970 = alloca %struct.ScmObj*, align 8
%cpsprim40584 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40220)
store volatile %struct.ScmObj* %cpsprim40584, %struct.ScmObj** %stackaddr$prim54970, align 8
%ae41995 = call %struct.ScmObj* @const_init_int(i64 0)
%args53025$k40583$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54971 = alloca %struct.ScmObj*, align 8
%args53025$k40583$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40584, %struct.ScmObj* %args53025$k40583$0)
store volatile %struct.ScmObj* %args53025$k40583$1, %struct.ScmObj** %stackaddr$prim54971, align 8
%stackaddr$prim54972 = alloca %struct.ScmObj*, align 8
%args53025$k40583$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41995, %struct.ScmObj* %args53025$k40583$1)
store volatile %struct.ScmObj* %args53025$k40583$2, %struct.ScmObj** %stackaddr$prim54972, align 8
%clofunc54973 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40583)
musttail call tailcc void %clofunc54973(%struct.ScmObj* %k40583, %struct.ScmObj* %args53025$k40583$2)
ret void
}

define tailcc void @proc_clo$ae41826(%struct.ScmObj* %env$ae41826,%struct.ScmObj* %args4015840585) {
%stackaddr$env-ref54974 = alloca %struct.ScmObj*, align 8
%_37last40153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41826, i64 0)
store %struct.ScmObj* %_37last40153, %struct.ScmObj** %stackaddr$env-ref54974
%stackaddr$env-ref54975 = alloca %struct.ScmObj*, align 8
%_37foldr40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41826, i64 1)
store %struct.ScmObj* %_37foldr40136, %struct.ScmObj** %stackaddr$env-ref54975
%stackaddr$env-ref54976 = alloca %struct.ScmObj*, align 8
%_37drop_45right40150 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41826, i64 2)
store %struct.ScmObj* %_37drop_45right40150, %struct.ScmObj** %stackaddr$env-ref54976
%stackaddr$prim54977 = alloca %struct.ScmObj*, align 8
%k40586 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4015840585)
store volatile %struct.ScmObj* %k40586, %struct.ScmObj** %stackaddr$prim54977, align 8
%stackaddr$prim54978 = alloca %struct.ScmObj*, align 8
%args40158 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4015840585)
store volatile %struct.ScmObj* %args40158, %struct.ScmObj** %stackaddr$prim54978, align 8
%stackaddr$prim54979 = alloca %struct.ScmObj*, align 8
%f40160 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40158)
store volatile %struct.ScmObj* %f40160, %struct.ScmObj** %stackaddr$prim54979, align 8
%stackaddr$prim54980 = alloca %struct.ScmObj*, align 8
%lsts40159 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40158)
store volatile %struct.ScmObj* %lsts40159, %struct.ScmObj** %stackaddr$prim54980, align 8
%stackaddr$makeclosure54981 = alloca %struct.ScmObj*, align 8
%fptrToInt54982 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41831 to i64
%ae41831 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54982)
store volatile %struct.ScmObj* %ae41831, %struct.ScmObj** %stackaddr$makeclosure54981, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41831, %struct.ScmObj* %_37foldr40136, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41831, %struct.ScmObj* %lsts40159, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41831, %struct.ScmObj* %k40586, i64 2)
%ae41832 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54983 = alloca %struct.ScmObj*, align 8
%fptrToInt54984 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41833 to i64
%ae41833 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54984)
store volatile %struct.ScmObj* %ae41833, %struct.ScmObj** %stackaddr$makeclosure54983, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41833, %struct.ScmObj* %_37last40153, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41833, %struct.ScmObj* %_37drop_45right40150, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41833, %struct.ScmObj* %f40160, i64 2)
%args53044$ae41831$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54985 = alloca %struct.ScmObj*, align 8
%args53044$ae41831$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41833, %struct.ScmObj* %args53044$ae41831$0)
store volatile %struct.ScmObj* %args53044$ae41831$1, %struct.ScmObj** %stackaddr$prim54985, align 8
%stackaddr$prim54986 = alloca %struct.ScmObj*, align 8
%args53044$ae41831$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41832, %struct.ScmObj* %args53044$ae41831$1)
store volatile %struct.ScmObj* %args53044$ae41831$2, %struct.ScmObj** %stackaddr$prim54986, align 8
%clofunc54987 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41831)
musttail call tailcc void %clofunc54987(%struct.ScmObj* %ae41831, %struct.ScmObj* %args53044$ae41831$2)
ret void
}

define tailcc void @proc_clo$ae41831(%struct.ScmObj* %env$ae41831,%struct.ScmObj* %current_45args53029) {
%stackaddr$env-ref54988 = alloca %struct.ScmObj*, align 8
%_37foldr40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41831, i64 0)
store %struct.ScmObj* %_37foldr40136, %struct.ScmObj** %stackaddr$env-ref54988
%stackaddr$env-ref54989 = alloca %struct.ScmObj*, align 8
%lsts40159 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41831, i64 1)
store %struct.ScmObj* %lsts40159, %struct.ScmObj** %stackaddr$env-ref54989
%stackaddr$env-ref54990 = alloca %struct.ScmObj*, align 8
%k40586 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41831, i64 2)
store %struct.ScmObj* %k40586, %struct.ScmObj** %stackaddr$env-ref54990
%stackaddr$prim54991 = alloca %struct.ScmObj*, align 8
%_95k40587 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53029)
store volatile %struct.ScmObj* %_95k40587, %struct.ScmObj** %stackaddr$prim54991, align 8
%stackaddr$prim54992 = alloca %struct.ScmObj*, align 8
%current_45args53030 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53029)
store volatile %struct.ScmObj* %current_45args53030, %struct.ScmObj** %stackaddr$prim54992, align 8
%stackaddr$prim54993 = alloca %struct.ScmObj*, align 8
%anf_45bind40313 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53030)
store volatile %struct.ScmObj* %anf_45bind40313, %struct.ScmObj** %stackaddr$prim54993, align 8
%ae41894 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54994 = alloca %struct.ScmObj*, align 8
%anf_45bind40314 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41894, %struct.ScmObj* %lsts40159)
store volatile %struct.ScmObj* %anf_45bind40314, %struct.ScmObj** %stackaddr$prim54994, align 8
%stackaddr$prim54995 = alloca %struct.ScmObj*, align 8
%anf_45bind40315 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40313, %struct.ScmObj* %anf_45bind40314)
store volatile %struct.ScmObj* %anf_45bind40315, %struct.ScmObj** %stackaddr$prim54995, align 8
%stackaddr$prim54996 = alloca %struct.ScmObj*, align 8
%cpsargs40588 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40586, %struct.ScmObj* %anf_45bind40315)
store volatile %struct.ScmObj* %cpsargs40588, %struct.ScmObj** %stackaddr$prim54996, align 8
%clofunc54997 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr40136)
musttail call tailcc void %clofunc54997(%struct.ScmObj* %_37foldr40136, %struct.ScmObj* %cpsargs40588)
ret void
}

define tailcc void @proc_clo$ae41833(%struct.ScmObj* %env$ae41833,%struct.ScmObj* %fargs4016140589) {
%stackaddr$env-ref54998 = alloca %struct.ScmObj*, align 8
%_37last40153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41833, i64 0)
store %struct.ScmObj* %_37last40153, %struct.ScmObj** %stackaddr$env-ref54998
%stackaddr$env-ref54999 = alloca %struct.ScmObj*, align 8
%_37drop_45right40150 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41833, i64 1)
store %struct.ScmObj* %_37drop_45right40150, %struct.ScmObj** %stackaddr$env-ref54999
%stackaddr$env-ref55000 = alloca %struct.ScmObj*, align 8
%f40160 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41833, i64 2)
store %struct.ScmObj* %f40160, %struct.ScmObj** %stackaddr$env-ref55000
%stackaddr$prim55001 = alloca %struct.ScmObj*, align 8
%k40590 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs4016140589)
store volatile %struct.ScmObj* %k40590, %struct.ScmObj** %stackaddr$prim55001, align 8
%stackaddr$prim55002 = alloca %struct.ScmObj*, align 8
%fargs40161 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs4016140589)
store volatile %struct.ScmObj* %fargs40161, %struct.ScmObj** %stackaddr$prim55002, align 8
%stackaddr$makeclosure55003 = alloca %struct.ScmObj*, align 8
%fptrToInt55004 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41837 to i64
%ae41837 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55004)
store volatile %struct.ScmObj* %ae41837, %struct.ScmObj** %stackaddr$makeclosure55003, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41837, %struct.ScmObj* %_37last40153, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41837, %struct.ScmObj* %fargs40161, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41837, %struct.ScmObj* %f40160, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41837, %struct.ScmObj* %k40590, i64 3)
%ae41839 = call %struct.ScmObj* @const_init_int(i64 1)
%args53043$_37drop_45right40150$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55005 = alloca %struct.ScmObj*, align 8
%args53043$_37drop_45right40150$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41839, %struct.ScmObj* %args53043$_37drop_45right40150$0)
store volatile %struct.ScmObj* %args53043$_37drop_45right40150$1, %struct.ScmObj** %stackaddr$prim55005, align 8
%stackaddr$prim55006 = alloca %struct.ScmObj*, align 8
%args53043$_37drop_45right40150$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs40161, %struct.ScmObj* %args53043$_37drop_45right40150$1)
store volatile %struct.ScmObj* %args53043$_37drop_45right40150$2, %struct.ScmObj** %stackaddr$prim55006, align 8
%stackaddr$prim55007 = alloca %struct.ScmObj*, align 8
%args53043$_37drop_45right40150$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41837, %struct.ScmObj* %args53043$_37drop_45right40150$2)
store volatile %struct.ScmObj* %args53043$_37drop_45right40150$3, %struct.ScmObj** %stackaddr$prim55007, align 8
%clofunc55008 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right40150)
musttail call tailcc void %clofunc55008(%struct.ScmObj* %_37drop_45right40150, %struct.ScmObj* %args53043$_37drop_45right40150$3)
ret void
}

define tailcc void @proc_clo$ae41837(%struct.ScmObj* %env$ae41837,%struct.ScmObj* %current_45args53032) {
%stackaddr$env-ref55009 = alloca %struct.ScmObj*, align 8
%_37last40153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41837, i64 0)
store %struct.ScmObj* %_37last40153, %struct.ScmObj** %stackaddr$env-ref55009
%stackaddr$env-ref55010 = alloca %struct.ScmObj*, align 8
%fargs40161 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41837, i64 1)
store %struct.ScmObj* %fargs40161, %struct.ScmObj** %stackaddr$env-ref55010
%stackaddr$env-ref55011 = alloca %struct.ScmObj*, align 8
%f40160 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41837, i64 2)
store %struct.ScmObj* %f40160, %struct.ScmObj** %stackaddr$env-ref55011
%stackaddr$env-ref55012 = alloca %struct.ScmObj*, align 8
%k40590 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41837, i64 3)
store %struct.ScmObj* %k40590, %struct.ScmObj** %stackaddr$env-ref55012
%stackaddr$prim55013 = alloca %struct.ScmObj*, align 8
%_95k40591 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53032)
store volatile %struct.ScmObj* %_95k40591, %struct.ScmObj** %stackaddr$prim55013, align 8
%stackaddr$prim55014 = alloca %struct.ScmObj*, align 8
%current_45args53033 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53032)
store volatile %struct.ScmObj* %current_45args53033, %struct.ScmObj** %stackaddr$prim55014, align 8
%stackaddr$prim55015 = alloca %struct.ScmObj*, align 8
%anf_45bind40310 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53033)
store volatile %struct.ScmObj* %anf_45bind40310, %struct.ScmObj** %stackaddr$prim55015, align 8
%stackaddr$makeclosure55016 = alloca %struct.ScmObj*, align 8
%fptrToInt55017 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41844 to i64
%ae41844 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55017)
store volatile %struct.ScmObj* %ae41844, %struct.ScmObj** %stackaddr$makeclosure55016, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41844, %struct.ScmObj* %_37last40153, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41844, %struct.ScmObj* %fargs40161, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41844, %struct.ScmObj* %k40590, i64 2)
%stackaddr$prim55018 = alloca %struct.ScmObj*, align 8
%cpsargs40595 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41844, %struct.ScmObj* %anf_45bind40310)
store volatile %struct.ScmObj* %cpsargs40595, %struct.ScmObj** %stackaddr$prim55018, align 8
%clofunc55019 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40160)
musttail call tailcc void %clofunc55019(%struct.ScmObj* %f40160, %struct.ScmObj* %cpsargs40595)
ret void
}

define tailcc void @proc_clo$ae41844(%struct.ScmObj* %env$ae41844,%struct.ScmObj* %current_45args53035) {
%stackaddr$env-ref55020 = alloca %struct.ScmObj*, align 8
%_37last40153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41844, i64 0)
store %struct.ScmObj* %_37last40153, %struct.ScmObj** %stackaddr$env-ref55020
%stackaddr$env-ref55021 = alloca %struct.ScmObj*, align 8
%fargs40161 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41844, i64 1)
store %struct.ScmObj* %fargs40161, %struct.ScmObj** %stackaddr$env-ref55021
%stackaddr$env-ref55022 = alloca %struct.ScmObj*, align 8
%k40590 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41844, i64 2)
store %struct.ScmObj* %k40590, %struct.ScmObj** %stackaddr$env-ref55022
%stackaddr$prim55023 = alloca %struct.ScmObj*, align 8
%_95k40592 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53035)
store volatile %struct.ScmObj* %_95k40592, %struct.ScmObj** %stackaddr$prim55023, align 8
%stackaddr$prim55024 = alloca %struct.ScmObj*, align 8
%current_45args53036 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53035)
store volatile %struct.ScmObj* %current_45args53036, %struct.ScmObj** %stackaddr$prim55024, align 8
%stackaddr$prim55025 = alloca %struct.ScmObj*, align 8
%anf_45bind40311 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53036)
store volatile %struct.ScmObj* %anf_45bind40311, %struct.ScmObj** %stackaddr$prim55025, align 8
%stackaddr$makeclosure55026 = alloca %struct.ScmObj*, align 8
%fptrToInt55027 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41849 to i64
%ae41849 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55027)
store volatile %struct.ScmObj* %ae41849, %struct.ScmObj** %stackaddr$makeclosure55026, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41849, %struct.ScmObj* %anf_45bind40311, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41849, %struct.ScmObj* %k40590, i64 1)
%args53042$_37last40153$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55028 = alloca %struct.ScmObj*, align 8
%args53042$_37last40153$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs40161, %struct.ScmObj* %args53042$_37last40153$0)
store volatile %struct.ScmObj* %args53042$_37last40153$1, %struct.ScmObj** %stackaddr$prim55028, align 8
%stackaddr$prim55029 = alloca %struct.ScmObj*, align 8
%args53042$_37last40153$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41849, %struct.ScmObj* %args53042$_37last40153$1)
store volatile %struct.ScmObj* %args53042$_37last40153$2, %struct.ScmObj** %stackaddr$prim55029, align 8
%clofunc55030 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last40153)
musttail call tailcc void %clofunc55030(%struct.ScmObj* %_37last40153, %struct.ScmObj* %args53042$_37last40153$2)
ret void
}

define tailcc void @proc_clo$ae41849(%struct.ScmObj* %env$ae41849,%struct.ScmObj* %current_45args53038) {
%stackaddr$env-ref55031 = alloca %struct.ScmObj*, align 8
%anf_45bind40311 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41849, i64 0)
store %struct.ScmObj* %anf_45bind40311, %struct.ScmObj** %stackaddr$env-ref55031
%stackaddr$env-ref55032 = alloca %struct.ScmObj*, align 8
%k40590 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41849, i64 1)
store %struct.ScmObj* %k40590, %struct.ScmObj** %stackaddr$env-ref55032
%stackaddr$prim55033 = alloca %struct.ScmObj*, align 8
%_95k40593 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53038)
store volatile %struct.ScmObj* %_95k40593, %struct.ScmObj** %stackaddr$prim55033, align 8
%stackaddr$prim55034 = alloca %struct.ScmObj*, align 8
%current_45args53039 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53038)
store volatile %struct.ScmObj* %current_45args53039, %struct.ScmObj** %stackaddr$prim55034, align 8
%stackaddr$prim55035 = alloca %struct.ScmObj*, align 8
%anf_45bind40312 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53039)
store volatile %struct.ScmObj* %anf_45bind40312, %struct.ScmObj** %stackaddr$prim55035, align 8
%stackaddr$prim55036 = alloca %struct.ScmObj*, align 8
%cpsprim40594 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40311, %struct.ScmObj* %anf_45bind40312)
store volatile %struct.ScmObj* %cpsprim40594, %struct.ScmObj** %stackaddr$prim55036, align 8
%ae41854 = call %struct.ScmObj* @const_init_int(i64 0)
%args53041$k40590$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55037 = alloca %struct.ScmObj*, align 8
%args53041$k40590$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40594, %struct.ScmObj* %args53041$k40590$0)
store volatile %struct.ScmObj* %args53041$k40590$1, %struct.ScmObj** %stackaddr$prim55037, align 8
%stackaddr$prim55038 = alloca %struct.ScmObj*, align 8
%args53041$k40590$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41854, %struct.ScmObj* %args53041$k40590$1)
store volatile %struct.ScmObj* %args53041$k40590$2, %struct.ScmObj** %stackaddr$prim55038, align 8
%clofunc55039 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40590)
musttail call tailcc void %clofunc55039(%struct.ScmObj* %k40590, %struct.ScmObj* %args53041$k40590$2)
ret void
}

define tailcc void @proc_clo$ae41749(%struct.ScmObj* %env$ae41749,%struct.ScmObj* %current_45args53046) {
%stackaddr$env-ref55040 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41749, i64 0)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref55040
%stackaddr$prim55041 = alloca %struct.ScmObj*, align 8
%k40596 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53046)
store volatile %struct.ScmObj* %k40596, %struct.ScmObj** %stackaddr$prim55041, align 8
%stackaddr$prim55042 = alloca %struct.ScmObj*, align 8
%current_45args53047 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53046)
store volatile %struct.ScmObj* %current_45args53047, %struct.ScmObj** %stackaddr$prim55042, align 8
%stackaddr$prim55043 = alloca %struct.ScmObj*, align 8
%f40164 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53047)
store volatile %struct.ScmObj* %f40164, %struct.ScmObj** %stackaddr$prim55043, align 8
%stackaddr$prim55044 = alloca %struct.ScmObj*, align 8
%current_45args53048 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53047)
store volatile %struct.ScmObj* %current_45args53048, %struct.ScmObj** %stackaddr$prim55044, align 8
%stackaddr$prim55045 = alloca %struct.ScmObj*, align 8
%lst40163 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53048)
store volatile %struct.ScmObj* %lst40163, %struct.ScmObj** %stackaddr$prim55045, align 8
%stackaddr$makeclosure55046 = alloca %struct.ScmObj*, align 8
%fptrToInt55047 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41750 to i64
%ae41750 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55047)
store volatile %struct.ScmObj* %ae41750, %struct.ScmObj** %stackaddr$makeclosure55046, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41750, %struct.ScmObj* %lst40163, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41750, %struct.ScmObj* %_37foldr140131, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41750, %struct.ScmObj* %k40596, i64 2)
%ae41751 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55048 = alloca %struct.ScmObj*, align 8
%fptrToInt55049 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41752 to i64
%ae41752 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55049)
store volatile %struct.ScmObj* %ae41752, %struct.ScmObj** %stackaddr$makeclosure55048, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41752, %struct.ScmObj* %f40164, i64 0)
%args53063$ae41750$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55050 = alloca %struct.ScmObj*, align 8
%args53063$ae41750$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41752, %struct.ScmObj* %args53063$ae41750$0)
store volatile %struct.ScmObj* %args53063$ae41750$1, %struct.ScmObj** %stackaddr$prim55050, align 8
%stackaddr$prim55051 = alloca %struct.ScmObj*, align 8
%args53063$ae41750$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41751, %struct.ScmObj* %args53063$ae41750$1)
store volatile %struct.ScmObj* %args53063$ae41750$2, %struct.ScmObj** %stackaddr$prim55051, align 8
%clofunc55052 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41750)
musttail call tailcc void %clofunc55052(%struct.ScmObj* %ae41750, %struct.ScmObj* %args53063$ae41750$2)
ret void
}

define tailcc void @proc_clo$ae41750(%struct.ScmObj* %env$ae41750,%struct.ScmObj* %current_45args53050) {
%stackaddr$env-ref55053 = alloca %struct.ScmObj*, align 8
%lst40163 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41750, i64 0)
store %struct.ScmObj* %lst40163, %struct.ScmObj** %stackaddr$env-ref55053
%stackaddr$env-ref55054 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41750, i64 1)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref55054
%stackaddr$env-ref55055 = alloca %struct.ScmObj*, align 8
%k40596 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41750, i64 2)
store %struct.ScmObj* %k40596, %struct.ScmObj** %stackaddr$env-ref55055
%stackaddr$prim55056 = alloca %struct.ScmObj*, align 8
%_95k40597 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53050)
store volatile %struct.ScmObj* %_95k40597, %struct.ScmObj** %stackaddr$prim55056, align 8
%stackaddr$prim55057 = alloca %struct.ScmObj*, align 8
%current_45args53051 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53050)
store volatile %struct.ScmObj* %current_45args53051, %struct.ScmObj** %stackaddr$prim55057, align 8
%stackaddr$prim55058 = alloca %struct.ScmObj*, align 8
%anf_45bind40309 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53051)
store volatile %struct.ScmObj* %anf_45bind40309, %struct.ScmObj** %stackaddr$prim55058, align 8
%ae41784 = call %struct.ScmObj* @const_init_null()
%args53053$_37foldr140131$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55059 = alloca %struct.ScmObj*, align 8
%args53053$_37foldr140131$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40163, %struct.ScmObj* %args53053$_37foldr140131$0)
store volatile %struct.ScmObj* %args53053$_37foldr140131$1, %struct.ScmObj** %stackaddr$prim55059, align 8
%stackaddr$prim55060 = alloca %struct.ScmObj*, align 8
%args53053$_37foldr140131$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41784, %struct.ScmObj* %args53053$_37foldr140131$1)
store volatile %struct.ScmObj* %args53053$_37foldr140131$2, %struct.ScmObj** %stackaddr$prim55060, align 8
%stackaddr$prim55061 = alloca %struct.ScmObj*, align 8
%args53053$_37foldr140131$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40309, %struct.ScmObj* %args53053$_37foldr140131$2)
store volatile %struct.ScmObj* %args53053$_37foldr140131$3, %struct.ScmObj** %stackaddr$prim55061, align 8
%stackaddr$prim55062 = alloca %struct.ScmObj*, align 8
%args53053$_37foldr140131$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40596, %struct.ScmObj* %args53053$_37foldr140131$3)
store volatile %struct.ScmObj* %args53053$_37foldr140131$4, %struct.ScmObj** %stackaddr$prim55062, align 8
%clofunc55063 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140131)
musttail call tailcc void %clofunc55063(%struct.ScmObj* %_37foldr140131, %struct.ScmObj* %args53053$_37foldr140131$4)
ret void
}

define tailcc void @proc_clo$ae41752(%struct.ScmObj* %env$ae41752,%struct.ScmObj* %current_45args53054) {
%stackaddr$env-ref55064 = alloca %struct.ScmObj*, align 8
%f40164 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41752, i64 0)
store %struct.ScmObj* %f40164, %struct.ScmObj** %stackaddr$env-ref55064
%stackaddr$prim55065 = alloca %struct.ScmObj*, align 8
%k40598 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53054)
store volatile %struct.ScmObj* %k40598, %struct.ScmObj** %stackaddr$prim55065, align 8
%stackaddr$prim55066 = alloca %struct.ScmObj*, align 8
%current_45args53055 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53054)
store volatile %struct.ScmObj* %current_45args53055, %struct.ScmObj** %stackaddr$prim55066, align 8
%stackaddr$prim55067 = alloca %struct.ScmObj*, align 8
%v40166 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53055)
store volatile %struct.ScmObj* %v40166, %struct.ScmObj** %stackaddr$prim55067, align 8
%stackaddr$prim55068 = alloca %struct.ScmObj*, align 8
%current_45args53056 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53055)
store volatile %struct.ScmObj* %current_45args53056, %struct.ScmObj** %stackaddr$prim55068, align 8
%stackaddr$prim55069 = alloca %struct.ScmObj*, align 8
%r40165 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53056)
store volatile %struct.ScmObj* %r40165, %struct.ScmObj** %stackaddr$prim55069, align 8
%stackaddr$makeclosure55070 = alloca %struct.ScmObj*, align 8
%fptrToInt55071 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41754 to i64
%ae41754 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55071)
store volatile %struct.ScmObj* %ae41754, %struct.ScmObj** %stackaddr$makeclosure55070, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41754, %struct.ScmObj* %k40598, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41754, %struct.ScmObj* %r40165, i64 1)
%args53062$f40164$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55072 = alloca %struct.ScmObj*, align 8
%args53062$f40164$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v40166, %struct.ScmObj* %args53062$f40164$0)
store volatile %struct.ScmObj* %args53062$f40164$1, %struct.ScmObj** %stackaddr$prim55072, align 8
%stackaddr$prim55073 = alloca %struct.ScmObj*, align 8
%args53062$f40164$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41754, %struct.ScmObj* %args53062$f40164$1)
store volatile %struct.ScmObj* %args53062$f40164$2, %struct.ScmObj** %stackaddr$prim55073, align 8
%clofunc55074 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40164)
musttail call tailcc void %clofunc55074(%struct.ScmObj* %f40164, %struct.ScmObj* %args53062$f40164$2)
ret void
}

define tailcc void @proc_clo$ae41754(%struct.ScmObj* %env$ae41754,%struct.ScmObj* %current_45args53058) {
%stackaddr$env-ref55075 = alloca %struct.ScmObj*, align 8
%k40598 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41754, i64 0)
store %struct.ScmObj* %k40598, %struct.ScmObj** %stackaddr$env-ref55075
%stackaddr$env-ref55076 = alloca %struct.ScmObj*, align 8
%r40165 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41754, i64 1)
store %struct.ScmObj* %r40165, %struct.ScmObj** %stackaddr$env-ref55076
%stackaddr$prim55077 = alloca %struct.ScmObj*, align 8
%_95k40599 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53058)
store volatile %struct.ScmObj* %_95k40599, %struct.ScmObj** %stackaddr$prim55077, align 8
%stackaddr$prim55078 = alloca %struct.ScmObj*, align 8
%current_45args53059 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53058)
store volatile %struct.ScmObj* %current_45args53059, %struct.ScmObj** %stackaddr$prim55078, align 8
%stackaddr$prim55079 = alloca %struct.ScmObj*, align 8
%anf_45bind40308 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53059)
store volatile %struct.ScmObj* %anf_45bind40308, %struct.ScmObj** %stackaddr$prim55079, align 8
%stackaddr$prim55080 = alloca %struct.ScmObj*, align 8
%cpsprim40600 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40308, %struct.ScmObj* %r40165)
store volatile %struct.ScmObj* %cpsprim40600, %struct.ScmObj** %stackaddr$prim55080, align 8
%ae41759 = call %struct.ScmObj* @const_init_int(i64 0)
%args53061$k40598$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55081 = alloca %struct.ScmObj*, align 8
%args53061$k40598$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40600, %struct.ScmObj* %args53061$k40598$0)
store volatile %struct.ScmObj* %args53061$k40598$1, %struct.ScmObj** %stackaddr$prim55081, align 8
%stackaddr$prim55082 = alloca %struct.ScmObj*, align 8
%args53061$k40598$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41759, %struct.ScmObj* %args53061$k40598$1)
store volatile %struct.ScmObj* %args53061$k40598$2, %struct.ScmObj** %stackaddr$prim55082, align 8
%clofunc55083 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40598)
musttail call tailcc void %clofunc55083(%struct.ScmObj* %k40598, %struct.ScmObj* %args53061$k40598$2)
ret void
}

define tailcc void @proc_clo$ae41363(%struct.ScmObj* %env$ae41363,%struct.ScmObj* %current_45args53066) {
%stackaddr$env-ref55084 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41363, i64 0)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref55084
%stackaddr$env-ref55085 = alloca %struct.ScmObj*, align 8
%_37map140127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41363, i64 1)
store %struct.ScmObj* %_37map140127, %struct.ScmObj** %stackaddr$env-ref55085
%stackaddr$prim55086 = alloca %struct.ScmObj*, align 8
%k40601 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53066)
store volatile %struct.ScmObj* %k40601, %struct.ScmObj** %stackaddr$prim55086, align 8
%stackaddr$prim55087 = alloca %struct.ScmObj*, align 8
%current_45args53067 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53066)
store volatile %struct.ScmObj* %current_45args53067, %struct.ScmObj** %stackaddr$prim55087, align 8
%stackaddr$prim55088 = alloca %struct.ScmObj*, align 8
%_37foldr40137 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53067)
store volatile %struct.ScmObj* %_37foldr40137, %struct.ScmObj** %stackaddr$prim55088, align 8
%ae41365 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55089 = alloca %struct.ScmObj*, align 8
%fptrToInt55090 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41366 to i64
%ae41366 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55090)
store volatile %struct.ScmObj* %ae41366, %struct.ScmObj** %stackaddr$makeclosure55089, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41366, %struct.ScmObj* %_37foldr40137, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41366, %struct.ScmObj* %_37foldr140131, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41366, %struct.ScmObj* %_37map140127, i64 2)
%args53124$k40601$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55091 = alloca %struct.ScmObj*, align 8
%args53124$k40601$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41366, %struct.ScmObj* %args53124$k40601$0)
store volatile %struct.ScmObj* %args53124$k40601$1, %struct.ScmObj** %stackaddr$prim55091, align 8
%stackaddr$prim55092 = alloca %struct.ScmObj*, align 8
%args53124$k40601$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41365, %struct.ScmObj* %args53124$k40601$1)
store volatile %struct.ScmObj* %args53124$k40601$2, %struct.ScmObj** %stackaddr$prim55092, align 8
%clofunc55093 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40601)
musttail call tailcc void %clofunc55093(%struct.ScmObj* %k40601, %struct.ScmObj* %args53124$k40601$2)
ret void
}

define tailcc void @proc_clo$ae41366(%struct.ScmObj* %env$ae41366,%struct.ScmObj* %args4013840602) {
%stackaddr$env-ref55094 = alloca %struct.ScmObj*, align 8
%_37foldr40137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41366, i64 0)
store %struct.ScmObj* %_37foldr40137, %struct.ScmObj** %stackaddr$env-ref55094
%stackaddr$env-ref55095 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41366, i64 1)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref55095
%stackaddr$env-ref55096 = alloca %struct.ScmObj*, align 8
%_37map140127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41366, i64 2)
store %struct.ScmObj* %_37map140127, %struct.ScmObj** %stackaddr$env-ref55096
%stackaddr$prim55097 = alloca %struct.ScmObj*, align 8
%k40603 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4013840602)
store volatile %struct.ScmObj* %k40603, %struct.ScmObj** %stackaddr$prim55097, align 8
%stackaddr$prim55098 = alloca %struct.ScmObj*, align 8
%args40138 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4013840602)
store volatile %struct.ScmObj* %args40138, %struct.ScmObj** %stackaddr$prim55098, align 8
%stackaddr$prim55099 = alloca %struct.ScmObj*, align 8
%f40141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40138)
store volatile %struct.ScmObj* %f40141, %struct.ScmObj** %stackaddr$prim55099, align 8
%stackaddr$prim55100 = alloca %struct.ScmObj*, align 8
%anf_45bind40295 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40138)
store volatile %struct.ScmObj* %anf_45bind40295, %struct.ScmObj** %stackaddr$prim55100, align 8
%stackaddr$prim55101 = alloca %struct.ScmObj*, align 8
%acc40140 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40295)
store volatile %struct.ScmObj* %acc40140, %struct.ScmObj** %stackaddr$prim55101, align 8
%stackaddr$prim55102 = alloca %struct.ScmObj*, align 8
%anf_45bind40296 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40138)
store volatile %struct.ScmObj* %anf_45bind40296, %struct.ScmObj** %stackaddr$prim55102, align 8
%stackaddr$prim55103 = alloca %struct.ScmObj*, align 8
%lsts40139 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40296)
store volatile %struct.ScmObj* %lsts40139, %struct.ScmObj** %stackaddr$prim55103, align 8
%stackaddr$makeclosure55104 = alloca %struct.ScmObj*, align 8
%fptrToInt55105 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41374 to i64
%ae41374 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55105)
store volatile %struct.ScmObj* %ae41374, %struct.ScmObj** %stackaddr$makeclosure55104, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41374, %struct.ScmObj* %lsts40139, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41374, %struct.ScmObj* %k40603, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41374, %struct.ScmObj* %_37foldr40137, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41374, %struct.ScmObj* %_37foldr140131, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41374, %struct.ScmObj* %_37map140127, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41374, %struct.ScmObj* %f40141, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41374, %struct.ScmObj* %acc40140, i64 6)
%ae41375 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55106 = alloca %struct.ScmObj*, align 8
%fptrToInt55107 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41376 to i64
%ae41376 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55107)
store volatile %struct.ScmObj* %ae41376, %struct.ScmObj** %stackaddr$makeclosure55106, align 8
%args53123$ae41374$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55108 = alloca %struct.ScmObj*, align 8
%args53123$ae41374$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41376, %struct.ScmObj* %args53123$ae41374$0)
store volatile %struct.ScmObj* %args53123$ae41374$1, %struct.ScmObj** %stackaddr$prim55108, align 8
%stackaddr$prim55109 = alloca %struct.ScmObj*, align 8
%args53123$ae41374$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41375, %struct.ScmObj* %args53123$ae41374$1)
store volatile %struct.ScmObj* %args53123$ae41374$2, %struct.ScmObj** %stackaddr$prim55109, align 8
%clofunc55110 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41374)
musttail call tailcc void %clofunc55110(%struct.ScmObj* %ae41374, %struct.ScmObj* %args53123$ae41374$2)
ret void
}

define tailcc void @proc_clo$ae41374(%struct.ScmObj* %env$ae41374,%struct.ScmObj* %current_45args53069) {
%stackaddr$env-ref55111 = alloca %struct.ScmObj*, align 8
%lsts40139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41374, i64 0)
store %struct.ScmObj* %lsts40139, %struct.ScmObj** %stackaddr$env-ref55111
%stackaddr$env-ref55112 = alloca %struct.ScmObj*, align 8
%k40603 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41374, i64 1)
store %struct.ScmObj* %k40603, %struct.ScmObj** %stackaddr$env-ref55112
%stackaddr$env-ref55113 = alloca %struct.ScmObj*, align 8
%_37foldr40137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41374, i64 2)
store %struct.ScmObj* %_37foldr40137, %struct.ScmObj** %stackaddr$env-ref55113
%stackaddr$env-ref55114 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41374, i64 3)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref55114
%stackaddr$env-ref55115 = alloca %struct.ScmObj*, align 8
%_37map140127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41374, i64 4)
store %struct.ScmObj* %_37map140127, %struct.ScmObj** %stackaddr$env-ref55115
%stackaddr$env-ref55116 = alloca %struct.ScmObj*, align 8
%f40141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41374, i64 5)
store %struct.ScmObj* %f40141, %struct.ScmObj** %stackaddr$env-ref55116
%stackaddr$env-ref55117 = alloca %struct.ScmObj*, align 8
%acc40140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41374, i64 6)
store %struct.ScmObj* %acc40140, %struct.ScmObj** %stackaddr$env-ref55117
%stackaddr$prim55118 = alloca %struct.ScmObj*, align 8
%_95k40604 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53069)
store volatile %struct.ScmObj* %_95k40604, %struct.ScmObj** %stackaddr$prim55118, align 8
%stackaddr$prim55119 = alloca %struct.ScmObj*, align 8
%current_45args53070 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53069)
store volatile %struct.ScmObj* %current_45args53070, %struct.ScmObj** %stackaddr$prim55119, align 8
%stackaddr$prim55120 = alloca %struct.ScmObj*, align 8
%anf_45bind40297 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53070)
store volatile %struct.ScmObj* %anf_45bind40297, %struct.ScmObj** %stackaddr$prim55120, align 8
%stackaddr$makeclosure55121 = alloca %struct.ScmObj*, align 8
%fptrToInt55122 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41406 to i64
%ae41406 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55122)
store volatile %struct.ScmObj* %ae41406, %struct.ScmObj** %stackaddr$makeclosure55121, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41406, %struct.ScmObj* %lsts40139, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41406, %struct.ScmObj* %k40603, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41406, %struct.ScmObj* %_37foldr40137, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41406, %struct.ScmObj* %_37foldr140131, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41406, %struct.ScmObj* %_37map140127, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41406, %struct.ScmObj* %f40141, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41406, %struct.ScmObj* %acc40140, i64 6)
%ae41408 = call %struct.ScmObj* @const_init_false()
%args53116$_37foldr140131$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55123 = alloca %struct.ScmObj*, align 8
%args53116$_37foldr140131$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40139, %struct.ScmObj* %args53116$_37foldr140131$0)
store volatile %struct.ScmObj* %args53116$_37foldr140131$1, %struct.ScmObj** %stackaddr$prim55123, align 8
%stackaddr$prim55124 = alloca %struct.ScmObj*, align 8
%args53116$_37foldr140131$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41408, %struct.ScmObj* %args53116$_37foldr140131$1)
store volatile %struct.ScmObj* %args53116$_37foldr140131$2, %struct.ScmObj** %stackaddr$prim55124, align 8
%stackaddr$prim55125 = alloca %struct.ScmObj*, align 8
%args53116$_37foldr140131$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40297, %struct.ScmObj* %args53116$_37foldr140131$2)
store volatile %struct.ScmObj* %args53116$_37foldr140131$3, %struct.ScmObj** %stackaddr$prim55125, align 8
%stackaddr$prim55126 = alloca %struct.ScmObj*, align 8
%args53116$_37foldr140131$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41406, %struct.ScmObj* %args53116$_37foldr140131$3)
store volatile %struct.ScmObj* %args53116$_37foldr140131$4, %struct.ScmObj** %stackaddr$prim55126, align 8
%clofunc55127 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140131)
musttail call tailcc void %clofunc55127(%struct.ScmObj* %_37foldr140131, %struct.ScmObj* %args53116$_37foldr140131$4)
ret void
}

define tailcc void @proc_clo$ae41406(%struct.ScmObj* %env$ae41406,%struct.ScmObj* %current_45args53072) {
%stackaddr$env-ref55128 = alloca %struct.ScmObj*, align 8
%lsts40139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41406, i64 0)
store %struct.ScmObj* %lsts40139, %struct.ScmObj** %stackaddr$env-ref55128
%stackaddr$env-ref55129 = alloca %struct.ScmObj*, align 8
%k40603 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41406, i64 1)
store %struct.ScmObj* %k40603, %struct.ScmObj** %stackaddr$env-ref55129
%stackaddr$env-ref55130 = alloca %struct.ScmObj*, align 8
%_37foldr40137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41406, i64 2)
store %struct.ScmObj* %_37foldr40137, %struct.ScmObj** %stackaddr$env-ref55130
%stackaddr$env-ref55131 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41406, i64 3)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref55131
%stackaddr$env-ref55132 = alloca %struct.ScmObj*, align 8
%_37map140127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41406, i64 4)
store %struct.ScmObj* %_37map140127, %struct.ScmObj** %stackaddr$env-ref55132
%stackaddr$env-ref55133 = alloca %struct.ScmObj*, align 8
%f40141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41406, i64 5)
store %struct.ScmObj* %f40141, %struct.ScmObj** %stackaddr$env-ref55133
%stackaddr$env-ref55134 = alloca %struct.ScmObj*, align 8
%acc40140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41406, i64 6)
store %struct.ScmObj* %acc40140, %struct.ScmObj** %stackaddr$env-ref55134
%stackaddr$prim55135 = alloca %struct.ScmObj*, align 8
%_95k40605 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53072)
store volatile %struct.ScmObj* %_95k40605, %struct.ScmObj** %stackaddr$prim55135, align 8
%stackaddr$prim55136 = alloca %struct.ScmObj*, align 8
%current_45args53073 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53072)
store volatile %struct.ScmObj* %current_45args53073, %struct.ScmObj** %stackaddr$prim55136, align 8
%stackaddr$prim55137 = alloca %struct.ScmObj*, align 8
%anf_45bind40298 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53073)
store volatile %struct.ScmObj* %anf_45bind40298, %struct.ScmObj** %stackaddr$prim55137, align 8
%truthy$cmp55138 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40298)
%cmp$cmp55138 = icmp eq i64 %truthy$cmp55138, 1
br i1 %cmp$cmp55138, label %truebranch$cmp55138, label %falsebranch$cmp55138
truebranch$cmp55138:
%ae41417 = call %struct.ScmObj* @const_init_int(i64 0)
%args53075$k40603$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55139 = alloca %struct.ScmObj*, align 8
%args53075$k40603$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40140, %struct.ScmObj* %args53075$k40603$0)
store volatile %struct.ScmObj* %args53075$k40603$1, %struct.ScmObj** %stackaddr$prim55139, align 8
%stackaddr$prim55140 = alloca %struct.ScmObj*, align 8
%args53075$k40603$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41417, %struct.ScmObj* %args53075$k40603$1)
store volatile %struct.ScmObj* %args53075$k40603$2, %struct.ScmObj** %stackaddr$prim55140, align 8
%clofunc55141 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40603)
musttail call tailcc void %clofunc55141(%struct.ScmObj* %k40603, %struct.ScmObj* %args53075$k40603$2)
ret void
falsebranch$cmp55138:
%stackaddr$makeclosure55142 = alloca %struct.ScmObj*, align 8
%fptrToInt55143 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41422 to i64
%ae41422 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55143)
store volatile %struct.ScmObj* %ae41422, %struct.ScmObj** %stackaddr$makeclosure55142, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41422, %struct.ScmObj* %lsts40139, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41422, %struct.ScmObj* %k40603, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41422, %struct.ScmObj* %_37foldr40137, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41422, %struct.ScmObj* %_37foldr140131, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41422, %struct.ScmObj* %_37map140127, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41422, %struct.ScmObj* %f40141, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41422, %struct.ScmObj* %acc40140, i64 6)
%ae41423 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55144 = alloca %struct.ScmObj*, align 8
%fptrToInt55145 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41424 to i64
%ae41424 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55145)
store volatile %struct.ScmObj* %ae41424, %struct.ScmObj** %stackaddr$makeclosure55144, align 8
%args53115$ae41422$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55146 = alloca %struct.ScmObj*, align 8
%args53115$ae41422$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41424, %struct.ScmObj* %args53115$ae41422$0)
store volatile %struct.ScmObj* %args53115$ae41422$1, %struct.ScmObj** %stackaddr$prim55146, align 8
%stackaddr$prim55147 = alloca %struct.ScmObj*, align 8
%args53115$ae41422$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41423, %struct.ScmObj* %args53115$ae41422$1)
store volatile %struct.ScmObj* %args53115$ae41422$2, %struct.ScmObj** %stackaddr$prim55147, align 8
%clofunc55148 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41422)
musttail call tailcc void %clofunc55148(%struct.ScmObj* %ae41422, %struct.ScmObj* %args53115$ae41422$2)
ret void
}

define tailcc void @proc_clo$ae41422(%struct.ScmObj* %env$ae41422,%struct.ScmObj* %current_45args53076) {
%stackaddr$env-ref55149 = alloca %struct.ScmObj*, align 8
%lsts40139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41422, i64 0)
store %struct.ScmObj* %lsts40139, %struct.ScmObj** %stackaddr$env-ref55149
%stackaddr$env-ref55150 = alloca %struct.ScmObj*, align 8
%k40603 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41422, i64 1)
store %struct.ScmObj* %k40603, %struct.ScmObj** %stackaddr$env-ref55150
%stackaddr$env-ref55151 = alloca %struct.ScmObj*, align 8
%_37foldr40137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41422, i64 2)
store %struct.ScmObj* %_37foldr40137, %struct.ScmObj** %stackaddr$env-ref55151
%stackaddr$env-ref55152 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41422, i64 3)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref55152
%stackaddr$env-ref55153 = alloca %struct.ScmObj*, align 8
%_37map140127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41422, i64 4)
store %struct.ScmObj* %_37map140127, %struct.ScmObj** %stackaddr$env-ref55153
%stackaddr$env-ref55154 = alloca %struct.ScmObj*, align 8
%f40141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41422, i64 5)
store %struct.ScmObj* %f40141, %struct.ScmObj** %stackaddr$env-ref55154
%stackaddr$env-ref55155 = alloca %struct.ScmObj*, align 8
%acc40140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41422, i64 6)
store %struct.ScmObj* %acc40140, %struct.ScmObj** %stackaddr$env-ref55155
%stackaddr$prim55156 = alloca %struct.ScmObj*, align 8
%_95k40606 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53076)
store volatile %struct.ScmObj* %_95k40606, %struct.ScmObj** %stackaddr$prim55156, align 8
%stackaddr$prim55157 = alloca %struct.ScmObj*, align 8
%current_45args53077 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53076)
store volatile %struct.ScmObj* %current_45args53077, %struct.ScmObj** %stackaddr$prim55157, align 8
%stackaddr$prim55158 = alloca %struct.ScmObj*, align 8
%anf_45bind40299 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53077)
store volatile %struct.ScmObj* %anf_45bind40299, %struct.ScmObj** %stackaddr$prim55158, align 8
%stackaddr$makeclosure55159 = alloca %struct.ScmObj*, align 8
%fptrToInt55160 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41443 to i64
%ae41443 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55160)
store volatile %struct.ScmObj* %ae41443, %struct.ScmObj** %stackaddr$makeclosure55159, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41443, %struct.ScmObj* %lsts40139, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41443, %struct.ScmObj* %k40603, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41443, %struct.ScmObj* %_37foldr40137, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41443, %struct.ScmObj* %_37foldr140131, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41443, %struct.ScmObj* %_37map140127, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41443, %struct.ScmObj* %f40141, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41443, %struct.ScmObj* %acc40140, i64 6)
%args53110$_37map140127$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55161 = alloca %struct.ScmObj*, align 8
%args53110$_37map140127$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40139, %struct.ScmObj* %args53110$_37map140127$0)
store volatile %struct.ScmObj* %args53110$_37map140127$1, %struct.ScmObj** %stackaddr$prim55161, align 8
%stackaddr$prim55162 = alloca %struct.ScmObj*, align 8
%args53110$_37map140127$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40299, %struct.ScmObj* %args53110$_37map140127$1)
store volatile %struct.ScmObj* %args53110$_37map140127$2, %struct.ScmObj** %stackaddr$prim55162, align 8
%stackaddr$prim55163 = alloca %struct.ScmObj*, align 8
%args53110$_37map140127$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41443, %struct.ScmObj* %args53110$_37map140127$2)
store volatile %struct.ScmObj* %args53110$_37map140127$3, %struct.ScmObj** %stackaddr$prim55163, align 8
%clofunc55164 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140127)
musttail call tailcc void %clofunc55164(%struct.ScmObj* %_37map140127, %struct.ScmObj* %args53110$_37map140127$3)
ret void
}

define tailcc void @proc_clo$ae41443(%struct.ScmObj* %env$ae41443,%struct.ScmObj* %current_45args53079) {
%stackaddr$env-ref55165 = alloca %struct.ScmObj*, align 8
%lsts40139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41443, i64 0)
store %struct.ScmObj* %lsts40139, %struct.ScmObj** %stackaddr$env-ref55165
%stackaddr$env-ref55166 = alloca %struct.ScmObj*, align 8
%k40603 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41443, i64 1)
store %struct.ScmObj* %k40603, %struct.ScmObj** %stackaddr$env-ref55166
%stackaddr$env-ref55167 = alloca %struct.ScmObj*, align 8
%_37foldr40137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41443, i64 2)
store %struct.ScmObj* %_37foldr40137, %struct.ScmObj** %stackaddr$env-ref55167
%stackaddr$env-ref55168 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41443, i64 3)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref55168
%stackaddr$env-ref55169 = alloca %struct.ScmObj*, align 8
%_37map140127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41443, i64 4)
store %struct.ScmObj* %_37map140127, %struct.ScmObj** %stackaddr$env-ref55169
%stackaddr$env-ref55170 = alloca %struct.ScmObj*, align 8
%f40141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41443, i64 5)
store %struct.ScmObj* %f40141, %struct.ScmObj** %stackaddr$env-ref55170
%stackaddr$env-ref55171 = alloca %struct.ScmObj*, align 8
%acc40140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41443, i64 6)
store %struct.ScmObj* %acc40140, %struct.ScmObj** %stackaddr$env-ref55171
%stackaddr$prim55172 = alloca %struct.ScmObj*, align 8
%_95k40607 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53079)
store volatile %struct.ScmObj* %_95k40607, %struct.ScmObj** %stackaddr$prim55172, align 8
%stackaddr$prim55173 = alloca %struct.ScmObj*, align 8
%current_45args53080 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53079)
store volatile %struct.ScmObj* %current_45args53080, %struct.ScmObj** %stackaddr$prim55173, align 8
%stackaddr$prim55174 = alloca %struct.ScmObj*, align 8
%lsts_4340146 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53080)
store volatile %struct.ScmObj* %lsts_4340146, %struct.ScmObj** %stackaddr$prim55174, align 8
%stackaddr$makeclosure55175 = alloca %struct.ScmObj*, align 8
%fptrToInt55176 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41446 to i64
%ae41446 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt55176)
store volatile %struct.ScmObj* %ae41446, %struct.ScmObj** %stackaddr$makeclosure55175, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41446, %struct.ScmObj* %lsts40139, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41446, %struct.ScmObj* %k40603, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41446, %struct.ScmObj* %_37foldr40137, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41446, %struct.ScmObj* %_37foldr140131, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41446, %struct.ScmObj* %lsts_4340146, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41446, %struct.ScmObj* %_37map140127, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41446, %struct.ScmObj* %f40141, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae41446, %struct.ScmObj* %acc40140, i64 7)
%ae41447 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55177 = alloca %struct.ScmObj*, align 8
%fptrToInt55178 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41448 to i64
%ae41448 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55178)
store volatile %struct.ScmObj* %ae41448, %struct.ScmObj** %stackaddr$makeclosure55177, align 8
%args53109$ae41446$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55179 = alloca %struct.ScmObj*, align 8
%args53109$ae41446$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41448, %struct.ScmObj* %args53109$ae41446$0)
store volatile %struct.ScmObj* %args53109$ae41446$1, %struct.ScmObj** %stackaddr$prim55179, align 8
%stackaddr$prim55180 = alloca %struct.ScmObj*, align 8
%args53109$ae41446$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41447, %struct.ScmObj* %args53109$ae41446$1)
store volatile %struct.ScmObj* %args53109$ae41446$2, %struct.ScmObj** %stackaddr$prim55180, align 8
%clofunc55181 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41446)
musttail call tailcc void %clofunc55181(%struct.ScmObj* %ae41446, %struct.ScmObj* %args53109$ae41446$2)
ret void
}

define tailcc void @proc_clo$ae41446(%struct.ScmObj* %env$ae41446,%struct.ScmObj* %current_45args53082) {
%stackaddr$env-ref55182 = alloca %struct.ScmObj*, align 8
%lsts40139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41446, i64 0)
store %struct.ScmObj* %lsts40139, %struct.ScmObj** %stackaddr$env-ref55182
%stackaddr$env-ref55183 = alloca %struct.ScmObj*, align 8
%k40603 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41446, i64 1)
store %struct.ScmObj* %k40603, %struct.ScmObj** %stackaddr$env-ref55183
%stackaddr$env-ref55184 = alloca %struct.ScmObj*, align 8
%_37foldr40137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41446, i64 2)
store %struct.ScmObj* %_37foldr40137, %struct.ScmObj** %stackaddr$env-ref55184
%stackaddr$env-ref55185 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41446, i64 3)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref55185
%stackaddr$env-ref55186 = alloca %struct.ScmObj*, align 8
%lsts_4340146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41446, i64 4)
store %struct.ScmObj* %lsts_4340146, %struct.ScmObj** %stackaddr$env-ref55186
%stackaddr$env-ref55187 = alloca %struct.ScmObj*, align 8
%_37map140127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41446, i64 5)
store %struct.ScmObj* %_37map140127, %struct.ScmObj** %stackaddr$env-ref55187
%stackaddr$env-ref55188 = alloca %struct.ScmObj*, align 8
%f40141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41446, i64 6)
store %struct.ScmObj* %f40141, %struct.ScmObj** %stackaddr$env-ref55188
%stackaddr$env-ref55189 = alloca %struct.ScmObj*, align 8
%acc40140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41446, i64 7)
store %struct.ScmObj* %acc40140, %struct.ScmObj** %stackaddr$env-ref55189
%stackaddr$prim55190 = alloca %struct.ScmObj*, align 8
%_95k40608 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53082)
store volatile %struct.ScmObj* %_95k40608, %struct.ScmObj** %stackaddr$prim55190, align 8
%stackaddr$prim55191 = alloca %struct.ScmObj*, align 8
%current_45args53083 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53082)
store volatile %struct.ScmObj* %current_45args53083, %struct.ScmObj** %stackaddr$prim55191, align 8
%stackaddr$prim55192 = alloca %struct.ScmObj*, align 8
%anf_45bind40300 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53083)
store volatile %struct.ScmObj* %anf_45bind40300, %struct.ScmObj** %stackaddr$prim55192, align 8
%stackaddr$makeclosure55193 = alloca %struct.ScmObj*, align 8
%fptrToInt55194 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41467 to i64
%ae41467 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt55194)
store volatile %struct.ScmObj* %ae41467, %struct.ScmObj** %stackaddr$makeclosure55193, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41467, %struct.ScmObj* %_37foldr40137, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41467, %struct.ScmObj* %_37foldr140131, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41467, %struct.ScmObj* %lsts_4340146, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41467, %struct.ScmObj* %f40141, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41467, %struct.ScmObj* %acc40140, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41467, %struct.ScmObj* %k40603, i64 5)
%args53104$_37map140127$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55195 = alloca %struct.ScmObj*, align 8
%args53104$_37map140127$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40139, %struct.ScmObj* %args53104$_37map140127$0)
store volatile %struct.ScmObj* %args53104$_37map140127$1, %struct.ScmObj** %stackaddr$prim55195, align 8
%stackaddr$prim55196 = alloca %struct.ScmObj*, align 8
%args53104$_37map140127$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40300, %struct.ScmObj* %args53104$_37map140127$1)
store volatile %struct.ScmObj* %args53104$_37map140127$2, %struct.ScmObj** %stackaddr$prim55196, align 8
%stackaddr$prim55197 = alloca %struct.ScmObj*, align 8
%args53104$_37map140127$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41467, %struct.ScmObj* %args53104$_37map140127$2)
store volatile %struct.ScmObj* %args53104$_37map140127$3, %struct.ScmObj** %stackaddr$prim55197, align 8
%clofunc55198 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140127)
musttail call tailcc void %clofunc55198(%struct.ScmObj* %_37map140127, %struct.ScmObj* %args53104$_37map140127$3)
ret void
}

define tailcc void @proc_clo$ae41467(%struct.ScmObj* %env$ae41467,%struct.ScmObj* %current_45args53085) {
%stackaddr$env-ref55199 = alloca %struct.ScmObj*, align 8
%_37foldr40137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41467, i64 0)
store %struct.ScmObj* %_37foldr40137, %struct.ScmObj** %stackaddr$env-ref55199
%stackaddr$env-ref55200 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41467, i64 1)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref55200
%stackaddr$env-ref55201 = alloca %struct.ScmObj*, align 8
%lsts_4340146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41467, i64 2)
store %struct.ScmObj* %lsts_4340146, %struct.ScmObj** %stackaddr$env-ref55201
%stackaddr$env-ref55202 = alloca %struct.ScmObj*, align 8
%f40141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41467, i64 3)
store %struct.ScmObj* %f40141, %struct.ScmObj** %stackaddr$env-ref55202
%stackaddr$env-ref55203 = alloca %struct.ScmObj*, align 8
%acc40140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41467, i64 4)
store %struct.ScmObj* %acc40140, %struct.ScmObj** %stackaddr$env-ref55203
%stackaddr$env-ref55204 = alloca %struct.ScmObj*, align 8
%k40603 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41467, i64 5)
store %struct.ScmObj* %k40603, %struct.ScmObj** %stackaddr$env-ref55204
%stackaddr$prim55205 = alloca %struct.ScmObj*, align 8
%_95k40609 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53085)
store volatile %struct.ScmObj* %_95k40609, %struct.ScmObj** %stackaddr$prim55205, align 8
%stackaddr$prim55206 = alloca %struct.ScmObj*, align 8
%current_45args53086 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53085)
store volatile %struct.ScmObj* %current_45args53086, %struct.ScmObj** %stackaddr$prim55206, align 8
%stackaddr$prim55207 = alloca %struct.ScmObj*, align 8
%vs40144 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53086)
store volatile %struct.ScmObj* %vs40144, %struct.ScmObj** %stackaddr$prim55207, align 8
%stackaddr$makeclosure55208 = alloca %struct.ScmObj*, align 8
%fptrToInt55209 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41470 to i64
%ae41470 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55209)
store volatile %struct.ScmObj* %ae41470, %struct.ScmObj** %stackaddr$makeclosure55208, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41470, %struct.ScmObj* %_37foldr40137, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41470, %struct.ScmObj* %_37foldr140131, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41470, %struct.ScmObj* %lsts_4340146, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41470, %struct.ScmObj* %vs40144, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41470, %struct.ScmObj* %f40141, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41470, %struct.ScmObj* %acc40140, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41470, %struct.ScmObj* %k40603, i64 6)
%ae41471 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55210 = alloca %struct.ScmObj*, align 8
%fptrToInt55211 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41472 to i64
%ae41472 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55211)
store volatile %struct.ScmObj* %ae41472, %struct.ScmObj** %stackaddr$makeclosure55210, align 8
%args53103$ae41470$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55212 = alloca %struct.ScmObj*, align 8
%args53103$ae41470$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41472, %struct.ScmObj* %args53103$ae41470$0)
store volatile %struct.ScmObj* %args53103$ae41470$1, %struct.ScmObj** %stackaddr$prim55212, align 8
%stackaddr$prim55213 = alloca %struct.ScmObj*, align 8
%args53103$ae41470$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41471, %struct.ScmObj* %args53103$ae41470$1)
store volatile %struct.ScmObj* %args53103$ae41470$2, %struct.ScmObj** %stackaddr$prim55213, align 8
%clofunc55214 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41470)
musttail call tailcc void %clofunc55214(%struct.ScmObj* %ae41470, %struct.ScmObj* %args53103$ae41470$2)
ret void
}

define tailcc void @proc_clo$ae41470(%struct.ScmObj* %env$ae41470,%struct.ScmObj* %current_45args53088) {
%stackaddr$env-ref55215 = alloca %struct.ScmObj*, align 8
%_37foldr40137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41470, i64 0)
store %struct.ScmObj* %_37foldr40137, %struct.ScmObj** %stackaddr$env-ref55215
%stackaddr$env-ref55216 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41470, i64 1)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref55216
%stackaddr$env-ref55217 = alloca %struct.ScmObj*, align 8
%lsts_4340146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41470, i64 2)
store %struct.ScmObj* %lsts_4340146, %struct.ScmObj** %stackaddr$env-ref55217
%stackaddr$env-ref55218 = alloca %struct.ScmObj*, align 8
%vs40144 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41470, i64 3)
store %struct.ScmObj* %vs40144, %struct.ScmObj** %stackaddr$env-ref55218
%stackaddr$env-ref55219 = alloca %struct.ScmObj*, align 8
%f40141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41470, i64 4)
store %struct.ScmObj* %f40141, %struct.ScmObj** %stackaddr$env-ref55219
%stackaddr$env-ref55220 = alloca %struct.ScmObj*, align 8
%acc40140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41470, i64 5)
store %struct.ScmObj* %acc40140, %struct.ScmObj** %stackaddr$env-ref55220
%stackaddr$env-ref55221 = alloca %struct.ScmObj*, align 8
%k40603 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41470, i64 6)
store %struct.ScmObj* %k40603, %struct.ScmObj** %stackaddr$env-ref55221
%stackaddr$prim55222 = alloca %struct.ScmObj*, align 8
%_95k40610 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53088)
store volatile %struct.ScmObj* %_95k40610, %struct.ScmObj** %stackaddr$prim55222, align 8
%stackaddr$prim55223 = alloca %struct.ScmObj*, align 8
%current_45args53089 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53088)
store volatile %struct.ScmObj* %current_45args53089, %struct.ScmObj** %stackaddr$prim55223, align 8
%stackaddr$prim55224 = alloca %struct.ScmObj*, align 8
%anf_45bind40301 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53089)
store volatile %struct.ScmObj* %anf_45bind40301, %struct.ScmObj** %stackaddr$prim55224, align 8
%stackaddr$prim55225 = alloca %struct.ScmObj*, align 8
%anf_45bind40302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40140, %struct.ScmObj* %lsts_4340146)
store volatile %struct.ScmObj* %anf_45bind40302, %struct.ScmObj** %stackaddr$prim55225, align 8
%stackaddr$prim55226 = alloca %struct.ScmObj*, align 8
%anf_45bind40303 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40141, %struct.ScmObj* %anf_45bind40302)
store volatile %struct.ScmObj* %anf_45bind40303, %struct.ScmObj** %stackaddr$prim55226, align 8
%stackaddr$makeclosure55227 = alloca %struct.ScmObj*, align 8
%fptrToInt55228 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41496 to i64
%ae41496 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55228)
store volatile %struct.ScmObj* %ae41496, %struct.ScmObj** %stackaddr$makeclosure55227, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41496, %struct.ScmObj* %anf_45bind40301, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41496, %struct.ScmObj* %f40141, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41496, %struct.ScmObj* %_37foldr140131, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41496, %struct.ScmObj* %vs40144, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41496, %struct.ScmObj* %k40603, i64 4)
%stackaddr$prim55229 = alloca %struct.ScmObj*, align 8
%cpsargs40614 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41496, %struct.ScmObj* %anf_45bind40303)
store volatile %struct.ScmObj* %cpsargs40614, %struct.ScmObj** %stackaddr$prim55229, align 8
%clofunc55230 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr40137)
musttail call tailcc void %clofunc55230(%struct.ScmObj* %_37foldr40137, %struct.ScmObj* %cpsargs40614)
ret void
}

define tailcc void @proc_clo$ae41496(%struct.ScmObj* %env$ae41496,%struct.ScmObj* %current_45args53091) {
%stackaddr$env-ref55231 = alloca %struct.ScmObj*, align 8
%anf_45bind40301 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41496, i64 0)
store %struct.ScmObj* %anf_45bind40301, %struct.ScmObj** %stackaddr$env-ref55231
%stackaddr$env-ref55232 = alloca %struct.ScmObj*, align 8
%f40141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41496, i64 1)
store %struct.ScmObj* %f40141, %struct.ScmObj** %stackaddr$env-ref55232
%stackaddr$env-ref55233 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41496, i64 2)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref55233
%stackaddr$env-ref55234 = alloca %struct.ScmObj*, align 8
%vs40144 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41496, i64 3)
store %struct.ScmObj* %vs40144, %struct.ScmObj** %stackaddr$env-ref55234
%stackaddr$env-ref55235 = alloca %struct.ScmObj*, align 8
%k40603 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41496, i64 4)
store %struct.ScmObj* %k40603, %struct.ScmObj** %stackaddr$env-ref55235
%stackaddr$prim55236 = alloca %struct.ScmObj*, align 8
%_95k40611 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53091)
store volatile %struct.ScmObj* %_95k40611, %struct.ScmObj** %stackaddr$prim55236, align 8
%stackaddr$prim55237 = alloca %struct.ScmObj*, align 8
%current_45args53092 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53091)
store volatile %struct.ScmObj* %current_45args53092, %struct.ScmObj** %stackaddr$prim55237, align 8
%stackaddr$prim55238 = alloca %struct.ScmObj*, align 8
%anf_45bind40304 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53092)
store volatile %struct.ScmObj* %anf_45bind40304, %struct.ScmObj** %stackaddr$prim55238, align 8
%ae41501 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55239 = alloca %struct.ScmObj*, align 8
%anf_45bind40305 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40304, %struct.ScmObj* %ae41501)
store volatile %struct.ScmObj* %anf_45bind40305, %struct.ScmObj** %stackaddr$prim55239, align 8
%stackaddr$makeclosure55240 = alloca %struct.ScmObj*, align 8
%fptrToInt55241 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41503 to i64
%ae41503 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55241)
store volatile %struct.ScmObj* %ae41503, %struct.ScmObj** %stackaddr$makeclosure55240, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41503, %struct.ScmObj* %f40141, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41503, %struct.ScmObj* %k40603, i64 1)
%args53097$_37foldr140131$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55242 = alloca %struct.ScmObj*, align 8
%args53097$_37foldr140131$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs40144, %struct.ScmObj* %args53097$_37foldr140131$0)
store volatile %struct.ScmObj* %args53097$_37foldr140131$1, %struct.ScmObj** %stackaddr$prim55242, align 8
%stackaddr$prim55243 = alloca %struct.ScmObj*, align 8
%args53097$_37foldr140131$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40305, %struct.ScmObj* %args53097$_37foldr140131$1)
store volatile %struct.ScmObj* %args53097$_37foldr140131$2, %struct.ScmObj** %stackaddr$prim55243, align 8
%stackaddr$prim55244 = alloca %struct.ScmObj*, align 8
%args53097$_37foldr140131$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40301, %struct.ScmObj* %args53097$_37foldr140131$2)
store volatile %struct.ScmObj* %args53097$_37foldr140131$3, %struct.ScmObj** %stackaddr$prim55244, align 8
%stackaddr$prim55245 = alloca %struct.ScmObj*, align 8
%args53097$_37foldr140131$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41503, %struct.ScmObj* %args53097$_37foldr140131$3)
store volatile %struct.ScmObj* %args53097$_37foldr140131$4, %struct.ScmObj** %stackaddr$prim55245, align 8
%clofunc55246 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140131)
musttail call tailcc void %clofunc55246(%struct.ScmObj* %_37foldr140131, %struct.ScmObj* %args53097$_37foldr140131$4)
ret void
}

define tailcc void @proc_clo$ae41503(%struct.ScmObj* %env$ae41503,%struct.ScmObj* %current_45args53094) {
%stackaddr$env-ref55247 = alloca %struct.ScmObj*, align 8
%f40141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41503, i64 0)
store %struct.ScmObj* %f40141, %struct.ScmObj** %stackaddr$env-ref55247
%stackaddr$env-ref55248 = alloca %struct.ScmObj*, align 8
%k40603 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41503, i64 1)
store %struct.ScmObj* %k40603, %struct.ScmObj** %stackaddr$env-ref55248
%stackaddr$prim55249 = alloca %struct.ScmObj*, align 8
%_95k40612 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53094)
store volatile %struct.ScmObj* %_95k40612, %struct.ScmObj** %stackaddr$prim55249, align 8
%stackaddr$prim55250 = alloca %struct.ScmObj*, align 8
%current_45args53095 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53094)
store volatile %struct.ScmObj* %current_45args53095, %struct.ScmObj** %stackaddr$prim55250, align 8
%stackaddr$prim55251 = alloca %struct.ScmObj*, align 8
%anf_45bind40306 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53095)
store volatile %struct.ScmObj* %anf_45bind40306, %struct.ScmObj** %stackaddr$prim55251, align 8
%stackaddr$prim55252 = alloca %struct.ScmObj*, align 8
%cpsargs40613 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40603, %struct.ScmObj* %anf_45bind40306)
store volatile %struct.ScmObj* %cpsargs40613, %struct.ScmObj** %stackaddr$prim55252, align 8
%clofunc55253 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40141)
musttail call tailcc void %clofunc55253(%struct.ScmObj* %f40141, %struct.ScmObj* %cpsargs40613)
ret void
}

define tailcc void @proc_clo$ae41472(%struct.ScmObj* %env$ae41472,%struct.ScmObj* %current_45args53098) {
%stackaddr$prim55254 = alloca %struct.ScmObj*, align 8
%k40615 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53098)
store volatile %struct.ScmObj* %k40615, %struct.ScmObj** %stackaddr$prim55254, align 8
%stackaddr$prim55255 = alloca %struct.ScmObj*, align 8
%current_45args53099 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53098)
store volatile %struct.ScmObj* %current_45args53099, %struct.ScmObj** %stackaddr$prim55255, align 8
%stackaddr$prim55256 = alloca %struct.ScmObj*, align 8
%a40149 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53099)
store volatile %struct.ScmObj* %a40149, %struct.ScmObj** %stackaddr$prim55256, align 8
%stackaddr$prim55257 = alloca %struct.ScmObj*, align 8
%current_45args53100 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53099)
store volatile %struct.ScmObj* %current_45args53100, %struct.ScmObj** %stackaddr$prim55257, align 8
%stackaddr$prim55258 = alloca %struct.ScmObj*, align 8
%b40148 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53100)
store volatile %struct.ScmObj* %b40148, %struct.ScmObj** %stackaddr$prim55258, align 8
%stackaddr$prim55259 = alloca %struct.ScmObj*, align 8
%cpsprim40616 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a40149, %struct.ScmObj* %b40148)
store volatile %struct.ScmObj* %cpsprim40616, %struct.ScmObj** %stackaddr$prim55259, align 8
%ae41476 = call %struct.ScmObj* @const_init_int(i64 0)
%args53102$k40615$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55260 = alloca %struct.ScmObj*, align 8
%args53102$k40615$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40616, %struct.ScmObj* %args53102$k40615$0)
store volatile %struct.ScmObj* %args53102$k40615$1, %struct.ScmObj** %stackaddr$prim55260, align 8
%stackaddr$prim55261 = alloca %struct.ScmObj*, align 8
%args53102$k40615$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41476, %struct.ScmObj* %args53102$k40615$1)
store volatile %struct.ScmObj* %args53102$k40615$2, %struct.ScmObj** %stackaddr$prim55261, align 8
%clofunc55262 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40615)
musttail call tailcc void %clofunc55262(%struct.ScmObj* %k40615, %struct.ScmObj* %args53102$k40615$2)
ret void
}

define tailcc void @proc_clo$ae41448(%struct.ScmObj* %env$ae41448,%struct.ScmObj* %current_45args53105) {
%stackaddr$prim55263 = alloca %struct.ScmObj*, align 8
%k40617 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53105)
store volatile %struct.ScmObj* %k40617, %struct.ScmObj** %stackaddr$prim55263, align 8
%stackaddr$prim55264 = alloca %struct.ScmObj*, align 8
%current_45args53106 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53105)
store volatile %struct.ScmObj* %current_45args53106, %struct.ScmObj** %stackaddr$prim55264, align 8
%stackaddr$prim55265 = alloca %struct.ScmObj*, align 8
%x40145 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53106)
store volatile %struct.ScmObj* %x40145, %struct.ScmObj** %stackaddr$prim55265, align 8
%stackaddr$prim55266 = alloca %struct.ScmObj*, align 8
%cpsprim40618 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40145)
store volatile %struct.ScmObj* %cpsprim40618, %struct.ScmObj** %stackaddr$prim55266, align 8
%ae41451 = call %struct.ScmObj* @const_init_int(i64 0)
%args53108$k40617$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55267 = alloca %struct.ScmObj*, align 8
%args53108$k40617$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40618, %struct.ScmObj* %args53108$k40617$0)
store volatile %struct.ScmObj* %args53108$k40617$1, %struct.ScmObj** %stackaddr$prim55267, align 8
%stackaddr$prim55268 = alloca %struct.ScmObj*, align 8
%args53108$k40617$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41451, %struct.ScmObj* %args53108$k40617$1)
store volatile %struct.ScmObj* %args53108$k40617$2, %struct.ScmObj** %stackaddr$prim55268, align 8
%clofunc55269 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40617)
musttail call tailcc void %clofunc55269(%struct.ScmObj* %k40617, %struct.ScmObj* %args53108$k40617$2)
ret void
}

define tailcc void @proc_clo$ae41424(%struct.ScmObj* %env$ae41424,%struct.ScmObj* %current_45args53111) {
%stackaddr$prim55270 = alloca %struct.ScmObj*, align 8
%k40619 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53111)
store volatile %struct.ScmObj* %k40619, %struct.ScmObj** %stackaddr$prim55270, align 8
%stackaddr$prim55271 = alloca %struct.ScmObj*, align 8
%current_45args53112 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53111)
store volatile %struct.ScmObj* %current_45args53112, %struct.ScmObj** %stackaddr$prim55271, align 8
%stackaddr$prim55272 = alloca %struct.ScmObj*, align 8
%x40147 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53112)
store volatile %struct.ScmObj* %x40147, %struct.ScmObj** %stackaddr$prim55272, align 8
%stackaddr$prim55273 = alloca %struct.ScmObj*, align 8
%cpsprim40620 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40147)
store volatile %struct.ScmObj* %cpsprim40620, %struct.ScmObj** %stackaddr$prim55273, align 8
%ae41427 = call %struct.ScmObj* @const_init_int(i64 0)
%args53114$k40619$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55274 = alloca %struct.ScmObj*, align 8
%args53114$k40619$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40620, %struct.ScmObj* %args53114$k40619$0)
store volatile %struct.ScmObj* %args53114$k40619$1, %struct.ScmObj** %stackaddr$prim55274, align 8
%stackaddr$prim55275 = alloca %struct.ScmObj*, align 8
%args53114$k40619$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41427, %struct.ScmObj* %args53114$k40619$1)
store volatile %struct.ScmObj* %args53114$k40619$2, %struct.ScmObj** %stackaddr$prim55275, align 8
%clofunc55276 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40619)
musttail call tailcc void %clofunc55276(%struct.ScmObj* %k40619, %struct.ScmObj* %args53114$k40619$2)
ret void
}

define tailcc void @proc_clo$ae41376(%struct.ScmObj* %env$ae41376,%struct.ScmObj* %current_45args53117) {
%stackaddr$prim55277 = alloca %struct.ScmObj*, align 8
%k40621 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53117)
store volatile %struct.ScmObj* %k40621, %struct.ScmObj** %stackaddr$prim55277, align 8
%stackaddr$prim55278 = alloca %struct.ScmObj*, align 8
%current_45args53118 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53117)
store volatile %struct.ScmObj* %current_45args53118, %struct.ScmObj** %stackaddr$prim55278, align 8
%stackaddr$prim55279 = alloca %struct.ScmObj*, align 8
%lst40143 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53118)
store volatile %struct.ScmObj* %lst40143, %struct.ScmObj** %stackaddr$prim55279, align 8
%stackaddr$prim55280 = alloca %struct.ScmObj*, align 8
%current_45args53119 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53118)
store volatile %struct.ScmObj* %current_45args53119, %struct.ScmObj** %stackaddr$prim55280, align 8
%stackaddr$prim55281 = alloca %struct.ScmObj*, align 8
%b40142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53119)
store volatile %struct.ScmObj* %b40142, %struct.ScmObj** %stackaddr$prim55281, align 8
%truthy$cmp55282 = call i64 @is_truthy_value(%struct.ScmObj* %b40142)
%cmp$cmp55282 = icmp eq i64 %truthy$cmp55282, 1
br i1 %cmp$cmp55282, label %truebranch$cmp55282, label %falsebranch$cmp55282
truebranch$cmp55282:
%ae41379 = call %struct.ScmObj* @const_init_int(i64 0)
%args53121$k40621$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55283 = alloca %struct.ScmObj*, align 8
%args53121$k40621$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b40142, %struct.ScmObj* %args53121$k40621$0)
store volatile %struct.ScmObj* %args53121$k40621$1, %struct.ScmObj** %stackaddr$prim55283, align 8
%stackaddr$prim55284 = alloca %struct.ScmObj*, align 8
%args53121$k40621$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41379, %struct.ScmObj* %args53121$k40621$1)
store volatile %struct.ScmObj* %args53121$k40621$2, %struct.ScmObj** %stackaddr$prim55284, align 8
%clofunc55285 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40621)
musttail call tailcc void %clofunc55285(%struct.ScmObj* %k40621, %struct.ScmObj* %args53121$k40621$2)
ret void
falsebranch$cmp55282:
%stackaddr$prim55286 = alloca %struct.ScmObj*, align 8
%cpsprim40622 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40143)
store volatile %struct.ScmObj* %cpsprim40622, %struct.ScmObj** %stackaddr$prim55286, align 8
%ae41386 = call %struct.ScmObj* @const_init_int(i64 0)
%args53122$k40621$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55287 = alloca %struct.ScmObj*, align 8
%args53122$k40621$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40622, %struct.ScmObj* %args53122$k40621$0)
store volatile %struct.ScmObj* %args53122$k40621$1, %struct.ScmObj** %stackaddr$prim55287, align 8
%stackaddr$prim55288 = alloca %struct.ScmObj*, align 8
%args53122$k40621$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41386, %struct.ScmObj* %args53122$k40621$1)
store volatile %struct.ScmObj* %args53122$k40621$2, %struct.ScmObj** %stackaddr$prim55288, align 8
%clofunc55289 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40621)
musttail call tailcc void %clofunc55289(%struct.ScmObj* %k40621, %struct.ScmObj* %args53122$k40621$2)
ret void
}

define tailcc void @proc_clo$ae41333(%struct.ScmObj* %env$ae41333,%struct.ScmObj* %current_45args53126) {
%stackaddr$env-ref55290 = alloca %struct.ScmObj*, align 8
%_37length40120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41333, i64 0)
store %struct.ScmObj* %_37length40120, %struct.ScmObj** %stackaddr$env-ref55290
%stackaddr$env-ref55291 = alloca %struct.ScmObj*, align 8
%_37take40123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41333, i64 1)
store %struct.ScmObj* %_37take40123, %struct.ScmObj** %stackaddr$env-ref55291
%stackaddr$prim55292 = alloca %struct.ScmObj*, align 8
%k40623 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53126)
store volatile %struct.ScmObj* %k40623, %struct.ScmObj** %stackaddr$prim55292, align 8
%stackaddr$prim55293 = alloca %struct.ScmObj*, align 8
%current_45args53127 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53126)
store volatile %struct.ScmObj* %current_45args53127, %struct.ScmObj** %stackaddr$prim55293, align 8
%stackaddr$prim55294 = alloca %struct.ScmObj*, align 8
%lst40152 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53127)
store volatile %struct.ScmObj* %lst40152, %struct.ScmObj** %stackaddr$prim55294, align 8
%stackaddr$prim55295 = alloca %struct.ScmObj*, align 8
%current_45args53128 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53127)
store volatile %struct.ScmObj* %current_45args53128, %struct.ScmObj** %stackaddr$prim55295, align 8
%stackaddr$prim55296 = alloca %struct.ScmObj*, align 8
%n40151 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53128)
store volatile %struct.ScmObj* %n40151, %struct.ScmObj** %stackaddr$prim55296, align 8
%stackaddr$makeclosure55297 = alloca %struct.ScmObj*, align 8
%fptrToInt55298 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41335 to i64
%ae41335 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55298)
store volatile %struct.ScmObj* %ae41335, %struct.ScmObj** %stackaddr$makeclosure55297, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41335, %struct.ScmObj* %lst40152, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41335, %struct.ScmObj* %n40151, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41335, %struct.ScmObj* %k40623, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41335, %struct.ScmObj* %_37take40123, i64 3)
%args53134$_37length40120$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55299 = alloca %struct.ScmObj*, align 8
%args53134$_37length40120$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40152, %struct.ScmObj* %args53134$_37length40120$0)
store volatile %struct.ScmObj* %args53134$_37length40120$1, %struct.ScmObj** %stackaddr$prim55299, align 8
%stackaddr$prim55300 = alloca %struct.ScmObj*, align 8
%args53134$_37length40120$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41335, %struct.ScmObj* %args53134$_37length40120$1)
store volatile %struct.ScmObj* %args53134$_37length40120$2, %struct.ScmObj** %stackaddr$prim55300, align 8
%clofunc55301 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length40120)
musttail call tailcc void %clofunc55301(%struct.ScmObj* %_37length40120, %struct.ScmObj* %args53134$_37length40120$2)
ret void
}

define tailcc void @proc_clo$ae41335(%struct.ScmObj* %env$ae41335,%struct.ScmObj* %current_45args53130) {
%stackaddr$env-ref55302 = alloca %struct.ScmObj*, align 8
%lst40152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41335, i64 0)
store %struct.ScmObj* %lst40152, %struct.ScmObj** %stackaddr$env-ref55302
%stackaddr$env-ref55303 = alloca %struct.ScmObj*, align 8
%n40151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41335, i64 1)
store %struct.ScmObj* %n40151, %struct.ScmObj** %stackaddr$env-ref55303
%stackaddr$env-ref55304 = alloca %struct.ScmObj*, align 8
%k40623 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41335, i64 2)
store %struct.ScmObj* %k40623, %struct.ScmObj** %stackaddr$env-ref55304
%stackaddr$env-ref55305 = alloca %struct.ScmObj*, align 8
%_37take40123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41335, i64 3)
store %struct.ScmObj* %_37take40123, %struct.ScmObj** %stackaddr$env-ref55305
%stackaddr$prim55306 = alloca %struct.ScmObj*, align 8
%_95k40624 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53130)
store volatile %struct.ScmObj* %_95k40624, %struct.ScmObj** %stackaddr$prim55306, align 8
%stackaddr$prim55307 = alloca %struct.ScmObj*, align 8
%current_45args53131 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53130)
store volatile %struct.ScmObj* %current_45args53131, %struct.ScmObj** %stackaddr$prim55307, align 8
%stackaddr$prim55308 = alloca %struct.ScmObj*, align 8
%anf_45bind40293 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53131)
store volatile %struct.ScmObj* %anf_45bind40293, %struct.ScmObj** %stackaddr$prim55308, align 8
%stackaddr$prim55309 = alloca %struct.ScmObj*, align 8
%anf_45bind40294 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind40293, %struct.ScmObj* %n40151)
store volatile %struct.ScmObj* %anf_45bind40294, %struct.ScmObj** %stackaddr$prim55309, align 8
%args53133$_37take40123$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55310 = alloca %struct.ScmObj*, align 8
%args53133$_37take40123$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40294, %struct.ScmObj* %args53133$_37take40123$0)
store volatile %struct.ScmObj* %args53133$_37take40123$1, %struct.ScmObj** %stackaddr$prim55310, align 8
%stackaddr$prim55311 = alloca %struct.ScmObj*, align 8
%args53133$_37take40123$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40152, %struct.ScmObj* %args53133$_37take40123$1)
store volatile %struct.ScmObj* %args53133$_37take40123$2, %struct.ScmObj** %stackaddr$prim55311, align 8
%stackaddr$prim55312 = alloca %struct.ScmObj*, align 8
%args53133$_37take40123$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40623, %struct.ScmObj* %args53133$_37take40123$2)
store volatile %struct.ScmObj* %args53133$_37take40123$3, %struct.ScmObj** %stackaddr$prim55312, align 8
%clofunc55313 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take40123)
musttail call tailcc void %clofunc55313(%struct.ScmObj* %_37take40123, %struct.ScmObj* %args53133$_37take40123$3)
ret void
}

define tailcc void @proc_clo$ae41279(%struct.ScmObj* %env$ae41279,%struct.ScmObj* %current_45args53136) {
%stackaddr$env-ref55314 = alloca %struct.ScmObj*, align 8
%_37foldl140115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41279, i64 0)
store %struct.ScmObj* %_37foldl140115, %struct.ScmObj** %stackaddr$env-ref55314
%stackaddr$prim55315 = alloca %struct.ScmObj*, align 8
%k40625 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53136)
store volatile %struct.ScmObj* %k40625, %struct.ScmObj** %stackaddr$prim55315, align 8
%stackaddr$prim55316 = alloca %struct.ScmObj*, align 8
%current_45args53137 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53136)
store volatile %struct.ScmObj* %current_45args53137, %struct.ScmObj** %stackaddr$prim55316, align 8
%stackaddr$prim55317 = alloca %struct.ScmObj*, align 8
%lst40154 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53137)
store volatile %struct.ScmObj* %lst40154, %struct.ScmObj** %stackaddr$prim55317, align 8
%stackaddr$makeclosure55318 = alloca %struct.ScmObj*, align 8
%fptrToInt55319 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41280 to i64
%ae41280 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55319)
store volatile %struct.ScmObj* %ae41280, %struct.ScmObj** %stackaddr$makeclosure55318, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41280, %struct.ScmObj* %_37foldl140115, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41280, %struct.ScmObj* %k40625, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41280, %struct.ScmObj* %lst40154, i64 2)
%ae41281 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55320 = alloca %struct.ScmObj*, align 8
%fptrToInt55321 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41282 to i64
%ae41282 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55321)
store volatile %struct.ScmObj* %ae41282, %struct.ScmObj** %stackaddr$makeclosure55320, align 8
%args53148$ae41280$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55322 = alloca %struct.ScmObj*, align 8
%args53148$ae41280$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41282, %struct.ScmObj* %args53148$ae41280$0)
store volatile %struct.ScmObj* %args53148$ae41280$1, %struct.ScmObj** %stackaddr$prim55322, align 8
%stackaddr$prim55323 = alloca %struct.ScmObj*, align 8
%args53148$ae41280$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41281, %struct.ScmObj* %args53148$ae41280$1)
store volatile %struct.ScmObj* %args53148$ae41280$2, %struct.ScmObj** %stackaddr$prim55323, align 8
%clofunc55324 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41280)
musttail call tailcc void %clofunc55324(%struct.ScmObj* %ae41280, %struct.ScmObj* %args53148$ae41280$2)
ret void
}

define tailcc void @proc_clo$ae41280(%struct.ScmObj* %env$ae41280,%struct.ScmObj* %current_45args53139) {
%stackaddr$env-ref55325 = alloca %struct.ScmObj*, align 8
%_37foldl140115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41280, i64 0)
store %struct.ScmObj* %_37foldl140115, %struct.ScmObj** %stackaddr$env-ref55325
%stackaddr$env-ref55326 = alloca %struct.ScmObj*, align 8
%k40625 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41280, i64 1)
store %struct.ScmObj* %k40625, %struct.ScmObj** %stackaddr$env-ref55326
%stackaddr$env-ref55327 = alloca %struct.ScmObj*, align 8
%lst40154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41280, i64 2)
store %struct.ScmObj* %lst40154, %struct.ScmObj** %stackaddr$env-ref55327
%stackaddr$prim55328 = alloca %struct.ScmObj*, align 8
%_95k40626 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53139)
store volatile %struct.ScmObj* %_95k40626, %struct.ScmObj** %stackaddr$prim55328, align 8
%stackaddr$prim55329 = alloca %struct.ScmObj*, align 8
%current_45args53140 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53139)
store volatile %struct.ScmObj* %current_45args53140, %struct.ScmObj** %stackaddr$prim55329, align 8
%stackaddr$prim55330 = alloca %struct.ScmObj*, align 8
%anf_45bind40292 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53140)
store volatile %struct.ScmObj* %anf_45bind40292, %struct.ScmObj** %stackaddr$prim55330, align 8
%ae41301 = call %struct.ScmObj* @const_init_null()
%args53142$_37foldl140115$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55331 = alloca %struct.ScmObj*, align 8
%args53142$_37foldl140115$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40154, %struct.ScmObj* %args53142$_37foldl140115$0)
store volatile %struct.ScmObj* %args53142$_37foldl140115$1, %struct.ScmObj** %stackaddr$prim55331, align 8
%stackaddr$prim55332 = alloca %struct.ScmObj*, align 8
%args53142$_37foldl140115$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41301, %struct.ScmObj* %args53142$_37foldl140115$1)
store volatile %struct.ScmObj* %args53142$_37foldl140115$2, %struct.ScmObj** %stackaddr$prim55332, align 8
%stackaddr$prim55333 = alloca %struct.ScmObj*, align 8
%args53142$_37foldl140115$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40292, %struct.ScmObj* %args53142$_37foldl140115$2)
store volatile %struct.ScmObj* %args53142$_37foldl140115$3, %struct.ScmObj** %stackaddr$prim55333, align 8
%stackaddr$prim55334 = alloca %struct.ScmObj*, align 8
%args53142$_37foldl140115$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40625, %struct.ScmObj* %args53142$_37foldl140115$3)
store volatile %struct.ScmObj* %args53142$_37foldl140115$4, %struct.ScmObj** %stackaddr$prim55334, align 8
%clofunc55335 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140115)
musttail call tailcc void %clofunc55335(%struct.ScmObj* %_37foldl140115, %struct.ScmObj* %args53142$_37foldl140115$4)
ret void
}

define tailcc void @proc_clo$ae41282(%struct.ScmObj* %env$ae41282,%struct.ScmObj* %current_45args53143) {
%stackaddr$prim55336 = alloca %struct.ScmObj*, align 8
%k40627 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53143)
store volatile %struct.ScmObj* %k40627, %struct.ScmObj** %stackaddr$prim55336, align 8
%stackaddr$prim55337 = alloca %struct.ScmObj*, align 8
%current_45args53144 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53143)
store volatile %struct.ScmObj* %current_45args53144, %struct.ScmObj** %stackaddr$prim55337, align 8
%stackaddr$prim55338 = alloca %struct.ScmObj*, align 8
%x40156 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53144)
store volatile %struct.ScmObj* %x40156, %struct.ScmObj** %stackaddr$prim55338, align 8
%stackaddr$prim55339 = alloca %struct.ScmObj*, align 8
%current_45args53145 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53144)
store volatile %struct.ScmObj* %current_45args53145, %struct.ScmObj** %stackaddr$prim55339, align 8
%stackaddr$prim55340 = alloca %struct.ScmObj*, align 8
%y40155 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53145)
store volatile %struct.ScmObj* %y40155, %struct.ScmObj** %stackaddr$prim55340, align 8
%ae41284 = call %struct.ScmObj* @const_init_int(i64 0)
%args53147$k40627$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55341 = alloca %struct.ScmObj*, align 8
%args53147$k40627$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x40156, %struct.ScmObj* %args53147$k40627$0)
store volatile %struct.ScmObj* %args53147$k40627$1, %struct.ScmObj** %stackaddr$prim55341, align 8
%stackaddr$prim55342 = alloca %struct.ScmObj*, align 8
%args53147$k40627$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41284, %struct.ScmObj* %args53147$k40627$1)
store volatile %struct.ScmObj* %args53147$k40627$2, %struct.ScmObj** %stackaddr$prim55342, align 8
%clofunc55343 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40627)
musttail call tailcc void %clofunc55343(%struct.ScmObj* %k40627, %struct.ScmObj* %args53147$k40627$2)
ret void
}

define tailcc void @proc_clo$ae41200(%struct.ScmObj* %env$ae41200,%struct.ScmObj* %current_45args53151) {
%stackaddr$prim55344 = alloca %struct.ScmObj*, align 8
%k40628 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53151)
store volatile %struct.ScmObj* %k40628, %struct.ScmObj** %stackaddr$prim55344, align 8
%stackaddr$prim55345 = alloca %struct.ScmObj*, align 8
%current_45args53152 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53151)
store volatile %struct.ScmObj* %current_45args53152, %struct.ScmObj** %stackaddr$prim55345, align 8
%stackaddr$prim55346 = alloca %struct.ScmObj*, align 8
%_37foldl140116 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53152)
store volatile %struct.ScmObj* %_37foldl140116, %struct.ScmObj** %stackaddr$prim55346, align 8
%ae41202 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55347 = alloca %struct.ScmObj*, align 8
%fptrToInt55348 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41203 to i64
%ae41203 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55348)
store volatile %struct.ScmObj* %ae41203, %struct.ScmObj** %stackaddr$makeclosure55347, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41203, %struct.ScmObj* %_37foldl140116, i64 0)
%args53165$k40628$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55349 = alloca %struct.ScmObj*, align 8
%args53165$k40628$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41203, %struct.ScmObj* %args53165$k40628$0)
store volatile %struct.ScmObj* %args53165$k40628$1, %struct.ScmObj** %stackaddr$prim55349, align 8
%stackaddr$prim55350 = alloca %struct.ScmObj*, align 8
%args53165$k40628$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41202, %struct.ScmObj* %args53165$k40628$1)
store volatile %struct.ScmObj* %args53165$k40628$2, %struct.ScmObj** %stackaddr$prim55350, align 8
%clofunc55351 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40628)
musttail call tailcc void %clofunc55351(%struct.ScmObj* %k40628, %struct.ScmObj* %args53165$k40628$2)
ret void
}

define tailcc void @proc_clo$ae41203(%struct.ScmObj* %env$ae41203,%struct.ScmObj* %current_45args53154) {
%stackaddr$env-ref55352 = alloca %struct.ScmObj*, align 8
%_37foldl140116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41203, i64 0)
store %struct.ScmObj* %_37foldl140116, %struct.ScmObj** %stackaddr$env-ref55352
%stackaddr$prim55353 = alloca %struct.ScmObj*, align 8
%k40629 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53154)
store volatile %struct.ScmObj* %k40629, %struct.ScmObj** %stackaddr$prim55353, align 8
%stackaddr$prim55354 = alloca %struct.ScmObj*, align 8
%current_45args53155 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53154)
store volatile %struct.ScmObj* %current_45args53155, %struct.ScmObj** %stackaddr$prim55354, align 8
%stackaddr$prim55355 = alloca %struct.ScmObj*, align 8
%f40119 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53155)
store volatile %struct.ScmObj* %f40119, %struct.ScmObj** %stackaddr$prim55355, align 8
%stackaddr$prim55356 = alloca %struct.ScmObj*, align 8
%current_45args53156 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53155)
store volatile %struct.ScmObj* %current_45args53156, %struct.ScmObj** %stackaddr$prim55356, align 8
%stackaddr$prim55357 = alloca %struct.ScmObj*, align 8
%acc40118 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53156)
store volatile %struct.ScmObj* %acc40118, %struct.ScmObj** %stackaddr$prim55357, align 8
%stackaddr$prim55358 = alloca %struct.ScmObj*, align 8
%current_45args53157 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53156)
store volatile %struct.ScmObj* %current_45args53157, %struct.ScmObj** %stackaddr$prim55358, align 8
%stackaddr$prim55359 = alloca %struct.ScmObj*, align 8
%lst40117 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53157)
store volatile %struct.ScmObj* %lst40117, %struct.ScmObj** %stackaddr$prim55359, align 8
%stackaddr$prim55360 = alloca %struct.ScmObj*, align 8
%anf_45bind40287 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40117)
store volatile %struct.ScmObj* %anf_45bind40287, %struct.ScmObj** %stackaddr$prim55360, align 8
%truthy$cmp55361 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40287)
%cmp$cmp55361 = icmp eq i64 %truthy$cmp55361, 1
br i1 %cmp$cmp55361, label %truebranch$cmp55361, label %falsebranch$cmp55361
truebranch$cmp55361:
%ae41207 = call %struct.ScmObj* @const_init_int(i64 0)
%args53159$k40629$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55362 = alloca %struct.ScmObj*, align 8
%args53159$k40629$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40118, %struct.ScmObj* %args53159$k40629$0)
store volatile %struct.ScmObj* %args53159$k40629$1, %struct.ScmObj** %stackaddr$prim55362, align 8
%stackaddr$prim55363 = alloca %struct.ScmObj*, align 8
%args53159$k40629$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41207, %struct.ScmObj* %args53159$k40629$1)
store volatile %struct.ScmObj* %args53159$k40629$2, %struct.ScmObj** %stackaddr$prim55363, align 8
%clofunc55364 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40629)
musttail call tailcc void %clofunc55364(%struct.ScmObj* %k40629, %struct.ScmObj* %args53159$k40629$2)
ret void
falsebranch$cmp55361:
%stackaddr$prim55365 = alloca %struct.ScmObj*, align 8
%anf_45bind40288 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40117)
store volatile %struct.ScmObj* %anf_45bind40288, %struct.ScmObj** %stackaddr$prim55365, align 8
%stackaddr$makeclosure55366 = alloca %struct.ScmObj*, align 8
%fptrToInt55367 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41214 to i64
%ae41214 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55367)
store volatile %struct.ScmObj* %ae41214, %struct.ScmObj** %stackaddr$makeclosure55366, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41214, %struct.ScmObj* %k40629, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41214, %struct.ScmObj* %lst40117, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41214, %struct.ScmObj* %f40119, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41214, %struct.ScmObj* %_37foldl140116, i64 3)
%args53164$f40119$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55368 = alloca %struct.ScmObj*, align 8
%args53164$f40119$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40118, %struct.ScmObj* %args53164$f40119$0)
store volatile %struct.ScmObj* %args53164$f40119$1, %struct.ScmObj** %stackaddr$prim55368, align 8
%stackaddr$prim55369 = alloca %struct.ScmObj*, align 8
%args53164$f40119$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40288, %struct.ScmObj* %args53164$f40119$1)
store volatile %struct.ScmObj* %args53164$f40119$2, %struct.ScmObj** %stackaddr$prim55369, align 8
%stackaddr$prim55370 = alloca %struct.ScmObj*, align 8
%args53164$f40119$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41214, %struct.ScmObj* %args53164$f40119$2)
store volatile %struct.ScmObj* %args53164$f40119$3, %struct.ScmObj** %stackaddr$prim55370, align 8
%clofunc55371 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40119)
musttail call tailcc void %clofunc55371(%struct.ScmObj* %f40119, %struct.ScmObj* %args53164$f40119$3)
ret void
}

define tailcc void @proc_clo$ae41214(%struct.ScmObj* %env$ae41214,%struct.ScmObj* %current_45args53160) {
%stackaddr$env-ref55372 = alloca %struct.ScmObj*, align 8
%k40629 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41214, i64 0)
store %struct.ScmObj* %k40629, %struct.ScmObj** %stackaddr$env-ref55372
%stackaddr$env-ref55373 = alloca %struct.ScmObj*, align 8
%lst40117 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41214, i64 1)
store %struct.ScmObj* %lst40117, %struct.ScmObj** %stackaddr$env-ref55373
%stackaddr$env-ref55374 = alloca %struct.ScmObj*, align 8
%f40119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41214, i64 2)
store %struct.ScmObj* %f40119, %struct.ScmObj** %stackaddr$env-ref55374
%stackaddr$env-ref55375 = alloca %struct.ScmObj*, align 8
%_37foldl140116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41214, i64 3)
store %struct.ScmObj* %_37foldl140116, %struct.ScmObj** %stackaddr$env-ref55375
%stackaddr$prim55376 = alloca %struct.ScmObj*, align 8
%_95k40630 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53160)
store volatile %struct.ScmObj* %_95k40630, %struct.ScmObj** %stackaddr$prim55376, align 8
%stackaddr$prim55377 = alloca %struct.ScmObj*, align 8
%current_45args53161 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53160)
store volatile %struct.ScmObj* %current_45args53161, %struct.ScmObj** %stackaddr$prim55377, align 8
%stackaddr$prim55378 = alloca %struct.ScmObj*, align 8
%anf_45bind40289 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53161)
store volatile %struct.ScmObj* %anf_45bind40289, %struct.ScmObj** %stackaddr$prim55378, align 8
%stackaddr$prim55379 = alloca %struct.ScmObj*, align 8
%anf_45bind40290 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40117)
store volatile %struct.ScmObj* %anf_45bind40290, %struct.ScmObj** %stackaddr$prim55379, align 8
%args53163$_37foldl140116$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55380 = alloca %struct.ScmObj*, align 8
%args53163$_37foldl140116$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40290, %struct.ScmObj* %args53163$_37foldl140116$0)
store volatile %struct.ScmObj* %args53163$_37foldl140116$1, %struct.ScmObj** %stackaddr$prim55380, align 8
%stackaddr$prim55381 = alloca %struct.ScmObj*, align 8
%args53163$_37foldl140116$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40289, %struct.ScmObj* %args53163$_37foldl140116$1)
store volatile %struct.ScmObj* %args53163$_37foldl140116$2, %struct.ScmObj** %stackaddr$prim55381, align 8
%stackaddr$prim55382 = alloca %struct.ScmObj*, align 8
%args53163$_37foldl140116$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40119, %struct.ScmObj* %args53163$_37foldl140116$2)
store volatile %struct.ScmObj* %args53163$_37foldl140116$3, %struct.ScmObj** %stackaddr$prim55382, align 8
%stackaddr$prim55383 = alloca %struct.ScmObj*, align 8
%args53163$_37foldl140116$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40629, %struct.ScmObj* %args53163$_37foldl140116$3)
store volatile %struct.ScmObj* %args53163$_37foldl140116$4, %struct.ScmObj** %stackaddr$prim55383, align 8
%clofunc55384 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140116)
musttail call tailcc void %clofunc55384(%struct.ScmObj* %_37foldl140116, %struct.ScmObj* %args53163$_37foldl140116$4)
ret void
}

define tailcc void @proc_clo$ae41117(%struct.ScmObj* %env$ae41117,%struct.ScmObj* %current_45args53168) {
%stackaddr$prim55385 = alloca %struct.ScmObj*, align 8
%k40631 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53168)
store volatile %struct.ScmObj* %k40631, %struct.ScmObj** %stackaddr$prim55385, align 8
%stackaddr$prim55386 = alloca %struct.ScmObj*, align 8
%current_45args53169 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53168)
store volatile %struct.ScmObj* %current_45args53169, %struct.ScmObj** %stackaddr$prim55386, align 8
%stackaddr$prim55387 = alloca %struct.ScmObj*, align 8
%_37length40121 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53169)
store volatile %struct.ScmObj* %_37length40121, %struct.ScmObj** %stackaddr$prim55387, align 8
%ae41119 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55388 = alloca %struct.ScmObj*, align 8
%fptrToInt55389 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41120 to i64
%ae41120 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55389)
store volatile %struct.ScmObj* %ae41120, %struct.ScmObj** %stackaddr$makeclosure55388, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41120, %struct.ScmObj* %_37length40121, i64 0)
%args53180$k40631$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55390 = alloca %struct.ScmObj*, align 8
%args53180$k40631$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41120, %struct.ScmObj* %args53180$k40631$0)
store volatile %struct.ScmObj* %args53180$k40631$1, %struct.ScmObj** %stackaddr$prim55390, align 8
%stackaddr$prim55391 = alloca %struct.ScmObj*, align 8
%args53180$k40631$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41119, %struct.ScmObj* %args53180$k40631$1)
store volatile %struct.ScmObj* %args53180$k40631$2, %struct.ScmObj** %stackaddr$prim55391, align 8
%clofunc55392 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40631)
musttail call tailcc void %clofunc55392(%struct.ScmObj* %k40631, %struct.ScmObj* %args53180$k40631$2)
ret void
}

define tailcc void @proc_clo$ae41120(%struct.ScmObj* %env$ae41120,%struct.ScmObj* %current_45args53171) {
%stackaddr$env-ref55393 = alloca %struct.ScmObj*, align 8
%_37length40121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41120, i64 0)
store %struct.ScmObj* %_37length40121, %struct.ScmObj** %stackaddr$env-ref55393
%stackaddr$prim55394 = alloca %struct.ScmObj*, align 8
%k40632 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53171)
store volatile %struct.ScmObj* %k40632, %struct.ScmObj** %stackaddr$prim55394, align 8
%stackaddr$prim55395 = alloca %struct.ScmObj*, align 8
%current_45args53172 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53171)
store volatile %struct.ScmObj* %current_45args53172, %struct.ScmObj** %stackaddr$prim55395, align 8
%stackaddr$prim55396 = alloca %struct.ScmObj*, align 8
%lst40122 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53172)
store volatile %struct.ScmObj* %lst40122, %struct.ScmObj** %stackaddr$prim55396, align 8
%stackaddr$prim55397 = alloca %struct.ScmObj*, align 8
%anf_45bind40283 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40122)
store volatile %struct.ScmObj* %anf_45bind40283, %struct.ScmObj** %stackaddr$prim55397, align 8
%truthy$cmp55398 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40283)
%cmp$cmp55398 = icmp eq i64 %truthy$cmp55398, 1
br i1 %cmp$cmp55398, label %truebranch$cmp55398, label %falsebranch$cmp55398
truebranch$cmp55398:
%ae41124 = call %struct.ScmObj* @const_init_int(i64 0)
%ae41125 = call %struct.ScmObj* @const_init_int(i64 0)
%args53174$k40632$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55399 = alloca %struct.ScmObj*, align 8
%args53174$k40632$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41125, %struct.ScmObj* %args53174$k40632$0)
store volatile %struct.ScmObj* %args53174$k40632$1, %struct.ScmObj** %stackaddr$prim55399, align 8
%stackaddr$prim55400 = alloca %struct.ScmObj*, align 8
%args53174$k40632$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41124, %struct.ScmObj* %args53174$k40632$1)
store volatile %struct.ScmObj* %args53174$k40632$2, %struct.ScmObj** %stackaddr$prim55400, align 8
%clofunc55401 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40632)
musttail call tailcc void %clofunc55401(%struct.ScmObj* %k40632, %struct.ScmObj* %args53174$k40632$2)
ret void
falsebranch$cmp55398:
%stackaddr$prim55402 = alloca %struct.ScmObj*, align 8
%anf_45bind40284 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40122)
store volatile %struct.ScmObj* %anf_45bind40284, %struct.ScmObj** %stackaddr$prim55402, align 8
%stackaddr$makeclosure55403 = alloca %struct.ScmObj*, align 8
%fptrToInt55404 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41134 to i64
%ae41134 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55404)
store volatile %struct.ScmObj* %ae41134, %struct.ScmObj** %stackaddr$makeclosure55403, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41134, %struct.ScmObj* %k40632, i64 0)
%args53179$_37length40121$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55405 = alloca %struct.ScmObj*, align 8
%args53179$_37length40121$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40284, %struct.ScmObj* %args53179$_37length40121$0)
store volatile %struct.ScmObj* %args53179$_37length40121$1, %struct.ScmObj** %stackaddr$prim55405, align 8
%stackaddr$prim55406 = alloca %struct.ScmObj*, align 8
%args53179$_37length40121$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41134, %struct.ScmObj* %args53179$_37length40121$1)
store volatile %struct.ScmObj* %args53179$_37length40121$2, %struct.ScmObj** %stackaddr$prim55406, align 8
%clofunc55407 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length40121)
musttail call tailcc void %clofunc55407(%struct.ScmObj* %_37length40121, %struct.ScmObj* %args53179$_37length40121$2)
ret void
}

define tailcc void @proc_clo$ae41134(%struct.ScmObj* %env$ae41134,%struct.ScmObj* %current_45args53175) {
%stackaddr$env-ref55408 = alloca %struct.ScmObj*, align 8
%k40632 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41134, i64 0)
store %struct.ScmObj* %k40632, %struct.ScmObj** %stackaddr$env-ref55408
%stackaddr$prim55409 = alloca %struct.ScmObj*, align 8
%_95k40633 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53175)
store volatile %struct.ScmObj* %_95k40633, %struct.ScmObj** %stackaddr$prim55409, align 8
%stackaddr$prim55410 = alloca %struct.ScmObj*, align 8
%current_45args53176 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53175)
store volatile %struct.ScmObj* %current_45args53176, %struct.ScmObj** %stackaddr$prim55410, align 8
%stackaddr$prim55411 = alloca %struct.ScmObj*, align 8
%anf_45bind40285 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53176)
store volatile %struct.ScmObj* %anf_45bind40285, %struct.ScmObj** %stackaddr$prim55411, align 8
%ae41136 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55412 = alloca %struct.ScmObj*, align 8
%cpsprim40634 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae41136, %struct.ScmObj* %anf_45bind40285)
store volatile %struct.ScmObj* %cpsprim40634, %struct.ScmObj** %stackaddr$prim55412, align 8
%ae41139 = call %struct.ScmObj* @const_init_int(i64 0)
%args53178$k40632$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55413 = alloca %struct.ScmObj*, align 8
%args53178$k40632$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40634, %struct.ScmObj* %args53178$k40632$0)
store volatile %struct.ScmObj* %args53178$k40632$1, %struct.ScmObj** %stackaddr$prim55413, align 8
%stackaddr$prim55414 = alloca %struct.ScmObj*, align 8
%args53178$k40632$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41139, %struct.ScmObj* %args53178$k40632$1)
store volatile %struct.ScmObj* %args53178$k40632$2, %struct.ScmObj** %stackaddr$prim55414, align 8
%clofunc55415 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40632)
musttail call tailcc void %clofunc55415(%struct.ScmObj* %k40632, %struct.ScmObj* %args53178$k40632$2)
ret void
}

define tailcc void @proc_clo$ae40967(%struct.ScmObj* %env$ae40967,%struct.ScmObj* %current_45args53183) {
%stackaddr$prim55416 = alloca %struct.ScmObj*, align 8
%k40635 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53183)
store volatile %struct.ScmObj* %k40635, %struct.ScmObj** %stackaddr$prim55416, align 8
%stackaddr$prim55417 = alloca %struct.ScmObj*, align 8
%current_45args53184 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53183)
store volatile %struct.ScmObj* %current_45args53184, %struct.ScmObj** %stackaddr$prim55417, align 8
%stackaddr$prim55418 = alloca %struct.ScmObj*, align 8
%_37take40124 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53184)
store volatile %struct.ScmObj* %_37take40124, %struct.ScmObj** %stackaddr$prim55418, align 8
%ae40969 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55419 = alloca %struct.ScmObj*, align 8
%fptrToInt55420 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40970 to i64
%ae40970 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55420)
store volatile %struct.ScmObj* %ae40970, %struct.ScmObj** %stackaddr$makeclosure55419, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40970, %struct.ScmObj* %_37take40124, i64 0)
%args53197$k40635$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55421 = alloca %struct.ScmObj*, align 8
%args53197$k40635$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40970, %struct.ScmObj* %args53197$k40635$0)
store volatile %struct.ScmObj* %args53197$k40635$1, %struct.ScmObj** %stackaddr$prim55421, align 8
%stackaddr$prim55422 = alloca %struct.ScmObj*, align 8
%args53197$k40635$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40969, %struct.ScmObj* %args53197$k40635$1)
store volatile %struct.ScmObj* %args53197$k40635$2, %struct.ScmObj** %stackaddr$prim55422, align 8
%clofunc55423 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40635)
musttail call tailcc void %clofunc55423(%struct.ScmObj* %k40635, %struct.ScmObj* %args53197$k40635$2)
ret void
}

define tailcc void @proc_clo$ae40970(%struct.ScmObj* %env$ae40970,%struct.ScmObj* %current_45args53186) {
%stackaddr$env-ref55424 = alloca %struct.ScmObj*, align 8
%_37take40124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40970, i64 0)
store %struct.ScmObj* %_37take40124, %struct.ScmObj** %stackaddr$env-ref55424
%stackaddr$prim55425 = alloca %struct.ScmObj*, align 8
%k40636 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53186)
store volatile %struct.ScmObj* %k40636, %struct.ScmObj** %stackaddr$prim55425, align 8
%stackaddr$prim55426 = alloca %struct.ScmObj*, align 8
%current_45args53187 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53186)
store volatile %struct.ScmObj* %current_45args53187, %struct.ScmObj** %stackaddr$prim55426, align 8
%stackaddr$prim55427 = alloca %struct.ScmObj*, align 8
%lst40126 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53187)
store volatile %struct.ScmObj* %lst40126, %struct.ScmObj** %stackaddr$prim55427, align 8
%stackaddr$prim55428 = alloca %struct.ScmObj*, align 8
%current_45args53188 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53187)
store volatile %struct.ScmObj* %current_45args53188, %struct.ScmObj** %stackaddr$prim55428, align 8
%stackaddr$prim55429 = alloca %struct.ScmObj*, align 8
%n40125 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53188)
store volatile %struct.ScmObj* %n40125, %struct.ScmObj** %stackaddr$prim55429, align 8
%ae40972 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55430 = alloca %struct.ScmObj*, align 8
%anf_45bind40276 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n40125, %struct.ScmObj* %ae40972)
store volatile %struct.ScmObj* %anf_45bind40276, %struct.ScmObj** %stackaddr$prim55430, align 8
%truthy$cmp55431 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40276)
%cmp$cmp55431 = icmp eq i64 %truthy$cmp55431, 1
br i1 %cmp$cmp55431, label %truebranch$cmp55431, label %falsebranch$cmp55431
truebranch$cmp55431:
%ae40975 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40976 = call %struct.ScmObj* @const_init_null()
%args53190$k40636$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55432 = alloca %struct.ScmObj*, align 8
%args53190$k40636$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40976, %struct.ScmObj* %args53190$k40636$0)
store volatile %struct.ScmObj* %args53190$k40636$1, %struct.ScmObj** %stackaddr$prim55432, align 8
%stackaddr$prim55433 = alloca %struct.ScmObj*, align 8
%args53190$k40636$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40975, %struct.ScmObj* %args53190$k40636$1)
store volatile %struct.ScmObj* %args53190$k40636$2, %struct.ScmObj** %stackaddr$prim55433, align 8
%clofunc55434 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40636)
musttail call tailcc void %clofunc55434(%struct.ScmObj* %k40636, %struct.ScmObj* %args53190$k40636$2)
ret void
falsebranch$cmp55431:
%stackaddr$prim55435 = alloca %struct.ScmObj*, align 8
%anf_45bind40277 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40126)
store volatile %struct.ScmObj* %anf_45bind40277, %struct.ScmObj** %stackaddr$prim55435, align 8
%truthy$cmp55436 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40277)
%cmp$cmp55436 = icmp eq i64 %truthy$cmp55436, 1
br i1 %cmp$cmp55436, label %truebranch$cmp55436, label %falsebranch$cmp55436
truebranch$cmp55436:
%ae40986 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40987 = call %struct.ScmObj* @const_init_null()
%args53191$k40636$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55437 = alloca %struct.ScmObj*, align 8
%args53191$k40636$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40987, %struct.ScmObj* %args53191$k40636$0)
store volatile %struct.ScmObj* %args53191$k40636$1, %struct.ScmObj** %stackaddr$prim55437, align 8
%stackaddr$prim55438 = alloca %struct.ScmObj*, align 8
%args53191$k40636$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40986, %struct.ScmObj* %args53191$k40636$1)
store volatile %struct.ScmObj* %args53191$k40636$2, %struct.ScmObj** %stackaddr$prim55438, align 8
%clofunc55439 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40636)
musttail call tailcc void %clofunc55439(%struct.ScmObj* %k40636, %struct.ScmObj* %args53191$k40636$2)
ret void
falsebranch$cmp55436:
%stackaddr$prim55440 = alloca %struct.ScmObj*, align 8
%anf_45bind40278 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40126)
store volatile %struct.ScmObj* %anf_45bind40278, %struct.ScmObj** %stackaddr$prim55440, align 8
%stackaddr$prim55441 = alloca %struct.ScmObj*, align 8
%anf_45bind40279 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40126)
store volatile %struct.ScmObj* %anf_45bind40279, %struct.ScmObj** %stackaddr$prim55441, align 8
%ae40997 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55442 = alloca %struct.ScmObj*, align 8
%anf_45bind40280 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n40125, %struct.ScmObj* %ae40997)
store volatile %struct.ScmObj* %anf_45bind40280, %struct.ScmObj** %stackaddr$prim55442, align 8
%stackaddr$makeclosure55443 = alloca %struct.ScmObj*, align 8
%fptrToInt55444 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40999 to i64
%ae40999 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55444)
store volatile %struct.ScmObj* %ae40999, %struct.ScmObj** %stackaddr$makeclosure55443, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40999, %struct.ScmObj* %anf_45bind40278, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40999, %struct.ScmObj* %k40636, i64 1)
%args53196$_37take40124$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55445 = alloca %struct.ScmObj*, align 8
%args53196$_37take40124$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40280, %struct.ScmObj* %args53196$_37take40124$0)
store volatile %struct.ScmObj* %args53196$_37take40124$1, %struct.ScmObj** %stackaddr$prim55445, align 8
%stackaddr$prim55446 = alloca %struct.ScmObj*, align 8
%args53196$_37take40124$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40279, %struct.ScmObj* %args53196$_37take40124$1)
store volatile %struct.ScmObj* %args53196$_37take40124$2, %struct.ScmObj** %stackaddr$prim55446, align 8
%stackaddr$prim55447 = alloca %struct.ScmObj*, align 8
%args53196$_37take40124$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40999, %struct.ScmObj* %args53196$_37take40124$2)
store volatile %struct.ScmObj* %args53196$_37take40124$3, %struct.ScmObj** %stackaddr$prim55447, align 8
%clofunc55448 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take40124)
musttail call tailcc void %clofunc55448(%struct.ScmObj* %_37take40124, %struct.ScmObj* %args53196$_37take40124$3)
ret void
}

define tailcc void @proc_clo$ae40999(%struct.ScmObj* %env$ae40999,%struct.ScmObj* %current_45args53192) {
%stackaddr$env-ref55449 = alloca %struct.ScmObj*, align 8
%anf_45bind40278 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40999, i64 0)
store %struct.ScmObj* %anf_45bind40278, %struct.ScmObj** %stackaddr$env-ref55449
%stackaddr$env-ref55450 = alloca %struct.ScmObj*, align 8
%k40636 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40999, i64 1)
store %struct.ScmObj* %k40636, %struct.ScmObj** %stackaddr$env-ref55450
%stackaddr$prim55451 = alloca %struct.ScmObj*, align 8
%_95k40637 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53192)
store volatile %struct.ScmObj* %_95k40637, %struct.ScmObj** %stackaddr$prim55451, align 8
%stackaddr$prim55452 = alloca %struct.ScmObj*, align 8
%current_45args53193 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53192)
store volatile %struct.ScmObj* %current_45args53193, %struct.ScmObj** %stackaddr$prim55452, align 8
%stackaddr$prim55453 = alloca %struct.ScmObj*, align 8
%anf_45bind40281 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53193)
store volatile %struct.ScmObj* %anf_45bind40281, %struct.ScmObj** %stackaddr$prim55453, align 8
%stackaddr$prim55454 = alloca %struct.ScmObj*, align 8
%cpsprim40638 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40278, %struct.ScmObj* %anf_45bind40281)
store volatile %struct.ScmObj* %cpsprim40638, %struct.ScmObj** %stackaddr$prim55454, align 8
%ae41005 = call %struct.ScmObj* @const_init_int(i64 0)
%args53195$k40636$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55455 = alloca %struct.ScmObj*, align 8
%args53195$k40636$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40638, %struct.ScmObj* %args53195$k40636$0)
store volatile %struct.ScmObj* %args53195$k40636$1, %struct.ScmObj** %stackaddr$prim55455, align 8
%stackaddr$prim55456 = alloca %struct.ScmObj*, align 8
%args53195$k40636$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41005, %struct.ScmObj* %args53195$k40636$1)
store volatile %struct.ScmObj* %args53195$k40636$2, %struct.ScmObj** %stackaddr$prim55456, align 8
%clofunc55457 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40636)
musttail call tailcc void %clofunc55457(%struct.ScmObj* %k40636, %struct.ScmObj* %args53195$k40636$2)
ret void
}

define tailcc void @proc_clo$ae40870(%struct.ScmObj* %env$ae40870,%struct.ScmObj* %current_45args53200) {
%stackaddr$prim55458 = alloca %struct.ScmObj*, align 8
%k40639 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53200)
store volatile %struct.ScmObj* %k40639, %struct.ScmObj** %stackaddr$prim55458, align 8
%stackaddr$prim55459 = alloca %struct.ScmObj*, align 8
%current_45args53201 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53200)
store volatile %struct.ScmObj* %current_45args53201, %struct.ScmObj** %stackaddr$prim55459, align 8
%stackaddr$prim55460 = alloca %struct.ScmObj*, align 8
%_37map40128 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53201)
store volatile %struct.ScmObj* %_37map40128, %struct.ScmObj** %stackaddr$prim55460, align 8
%ae40872 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55461 = alloca %struct.ScmObj*, align 8
%fptrToInt55462 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40873 to i64
%ae40873 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55462)
store volatile %struct.ScmObj* %ae40873, %struct.ScmObj** %stackaddr$makeclosure55461, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40873, %struct.ScmObj* %_37map40128, i64 0)
%args53217$k40639$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55463 = alloca %struct.ScmObj*, align 8
%args53217$k40639$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40873, %struct.ScmObj* %args53217$k40639$0)
store volatile %struct.ScmObj* %args53217$k40639$1, %struct.ScmObj** %stackaddr$prim55463, align 8
%stackaddr$prim55464 = alloca %struct.ScmObj*, align 8
%args53217$k40639$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40872, %struct.ScmObj* %args53217$k40639$1)
store volatile %struct.ScmObj* %args53217$k40639$2, %struct.ScmObj** %stackaddr$prim55464, align 8
%clofunc55465 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40639)
musttail call tailcc void %clofunc55465(%struct.ScmObj* %k40639, %struct.ScmObj* %args53217$k40639$2)
ret void
}

define tailcc void @proc_clo$ae40873(%struct.ScmObj* %env$ae40873,%struct.ScmObj* %current_45args53203) {
%stackaddr$env-ref55466 = alloca %struct.ScmObj*, align 8
%_37map40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40873, i64 0)
store %struct.ScmObj* %_37map40128, %struct.ScmObj** %stackaddr$env-ref55466
%stackaddr$prim55467 = alloca %struct.ScmObj*, align 8
%k40640 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53203)
store volatile %struct.ScmObj* %k40640, %struct.ScmObj** %stackaddr$prim55467, align 8
%stackaddr$prim55468 = alloca %struct.ScmObj*, align 8
%current_45args53204 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53203)
store volatile %struct.ScmObj* %current_45args53204, %struct.ScmObj** %stackaddr$prim55468, align 8
%stackaddr$prim55469 = alloca %struct.ScmObj*, align 8
%f40130 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53204)
store volatile %struct.ScmObj* %f40130, %struct.ScmObj** %stackaddr$prim55469, align 8
%stackaddr$prim55470 = alloca %struct.ScmObj*, align 8
%current_45args53205 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53204)
store volatile %struct.ScmObj* %current_45args53205, %struct.ScmObj** %stackaddr$prim55470, align 8
%stackaddr$prim55471 = alloca %struct.ScmObj*, align 8
%lst40129 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53205)
store volatile %struct.ScmObj* %lst40129, %struct.ScmObj** %stackaddr$prim55471, align 8
%stackaddr$prim55472 = alloca %struct.ScmObj*, align 8
%anf_45bind40270 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40129)
store volatile %struct.ScmObj* %anf_45bind40270, %struct.ScmObj** %stackaddr$prim55472, align 8
%truthy$cmp55473 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40270)
%cmp$cmp55473 = icmp eq i64 %truthy$cmp55473, 1
br i1 %cmp$cmp55473, label %truebranch$cmp55473, label %falsebranch$cmp55473
truebranch$cmp55473:
%ae40877 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40878 = call %struct.ScmObj* @const_init_null()
%args53207$k40640$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55474 = alloca %struct.ScmObj*, align 8
%args53207$k40640$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40878, %struct.ScmObj* %args53207$k40640$0)
store volatile %struct.ScmObj* %args53207$k40640$1, %struct.ScmObj** %stackaddr$prim55474, align 8
%stackaddr$prim55475 = alloca %struct.ScmObj*, align 8
%args53207$k40640$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40877, %struct.ScmObj* %args53207$k40640$1)
store volatile %struct.ScmObj* %args53207$k40640$2, %struct.ScmObj** %stackaddr$prim55475, align 8
%clofunc55476 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40640)
musttail call tailcc void %clofunc55476(%struct.ScmObj* %k40640, %struct.ScmObj* %args53207$k40640$2)
ret void
falsebranch$cmp55473:
%stackaddr$prim55477 = alloca %struct.ScmObj*, align 8
%anf_45bind40271 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40129)
store volatile %struct.ScmObj* %anf_45bind40271, %struct.ScmObj** %stackaddr$prim55477, align 8
%stackaddr$makeclosure55478 = alloca %struct.ScmObj*, align 8
%fptrToInt55479 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40887 to i64
%ae40887 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55479)
store volatile %struct.ScmObj* %ae40887, %struct.ScmObj** %stackaddr$makeclosure55478, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40887, %struct.ScmObj* %k40640, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40887, %struct.ScmObj* %_37map40128, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40887, %struct.ScmObj* %f40130, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae40887, %struct.ScmObj* %lst40129, i64 3)
%args53216$f40130$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55480 = alloca %struct.ScmObj*, align 8
%args53216$f40130$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40271, %struct.ScmObj* %args53216$f40130$0)
store volatile %struct.ScmObj* %args53216$f40130$1, %struct.ScmObj** %stackaddr$prim55480, align 8
%stackaddr$prim55481 = alloca %struct.ScmObj*, align 8
%args53216$f40130$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40887, %struct.ScmObj* %args53216$f40130$1)
store volatile %struct.ScmObj* %args53216$f40130$2, %struct.ScmObj** %stackaddr$prim55481, align 8
%clofunc55482 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40130)
musttail call tailcc void %clofunc55482(%struct.ScmObj* %f40130, %struct.ScmObj* %args53216$f40130$2)
ret void
}

define tailcc void @proc_clo$ae40887(%struct.ScmObj* %env$ae40887,%struct.ScmObj* %current_45args53208) {
%stackaddr$env-ref55483 = alloca %struct.ScmObj*, align 8
%k40640 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40887, i64 0)
store %struct.ScmObj* %k40640, %struct.ScmObj** %stackaddr$env-ref55483
%stackaddr$env-ref55484 = alloca %struct.ScmObj*, align 8
%_37map40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40887, i64 1)
store %struct.ScmObj* %_37map40128, %struct.ScmObj** %stackaddr$env-ref55484
%stackaddr$env-ref55485 = alloca %struct.ScmObj*, align 8
%f40130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40887, i64 2)
store %struct.ScmObj* %f40130, %struct.ScmObj** %stackaddr$env-ref55485
%stackaddr$env-ref55486 = alloca %struct.ScmObj*, align 8
%lst40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40887, i64 3)
store %struct.ScmObj* %lst40129, %struct.ScmObj** %stackaddr$env-ref55486
%stackaddr$prim55487 = alloca %struct.ScmObj*, align 8
%_95k40641 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53208)
store volatile %struct.ScmObj* %_95k40641, %struct.ScmObj** %stackaddr$prim55487, align 8
%stackaddr$prim55488 = alloca %struct.ScmObj*, align 8
%current_45args53209 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53208)
store volatile %struct.ScmObj* %current_45args53209, %struct.ScmObj** %stackaddr$prim55488, align 8
%stackaddr$prim55489 = alloca %struct.ScmObj*, align 8
%anf_45bind40272 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53209)
store volatile %struct.ScmObj* %anf_45bind40272, %struct.ScmObj** %stackaddr$prim55489, align 8
%stackaddr$prim55490 = alloca %struct.ScmObj*, align 8
%anf_45bind40273 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40129)
store volatile %struct.ScmObj* %anf_45bind40273, %struct.ScmObj** %stackaddr$prim55490, align 8
%stackaddr$makeclosure55491 = alloca %struct.ScmObj*, align 8
%fptrToInt55492 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40891 to i64
%ae40891 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55492)
store volatile %struct.ScmObj* %ae40891, %struct.ScmObj** %stackaddr$makeclosure55491, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40891, %struct.ScmObj* %anf_45bind40272, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40891, %struct.ScmObj* %k40640, i64 1)
%args53215$_37map40128$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55493 = alloca %struct.ScmObj*, align 8
%args53215$_37map40128$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40273, %struct.ScmObj* %args53215$_37map40128$0)
store volatile %struct.ScmObj* %args53215$_37map40128$1, %struct.ScmObj** %stackaddr$prim55493, align 8
%stackaddr$prim55494 = alloca %struct.ScmObj*, align 8
%args53215$_37map40128$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40130, %struct.ScmObj* %args53215$_37map40128$1)
store volatile %struct.ScmObj* %args53215$_37map40128$2, %struct.ScmObj** %stackaddr$prim55494, align 8
%stackaddr$prim55495 = alloca %struct.ScmObj*, align 8
%args53215$_37map40128$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40891, %struct.ScmObj* %args53215$_37map40128$2)
store volatile %struct.ScmObj* %args53215$_37map40128$3, %struct.ScmObj** %stackaddr$prim55495, align 8
%clofunc55496 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map40128)
musttail call tailcc void %clofunc55496(%struct.ScmObj* %_37map40128, %struct.ScmObj* %args53215$_37map40128$3)
ret void
}

define tailcc void @proc_clo$ae40891(%struct.ScmObj* %env$ae40891,%struct.ScmObj* %current_45args53211) {
%stackaddr$env-ref55497 = alloca %struct.ScmObj*, align 8
%anf_45bind40272 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40891, i64 0)
store %struct.ScmObj* %anf_45bind40272, %struct.ScmObj** %stackaddr$env-ref55497
%stackaddr$env-ref55498 = alloca %struct.ScmObj*, align 8
%k40640 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40891, i64 1)
store %struct.ScmObj* %k40640, %struct.ScmObj** %stackaddr$env-ref55498
%stackaddr$prim55499 = alloca %struct.ScmObj*, align 8
%_95k40642 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53211)
store volatile %struct.ScmObj* %_95k40642, %struct.ScmObj** %stackaddr$prim55499, align 8
%stackaddr$prim55500 = alloca %struct.ScmObj*, align 8
%current_45args53212 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53211)
store volatile %struct.ScmObj* %current_45args53212, %struct.ScmObj** %stackaddr$prim55500, align 8
%stackaddr$prim55501 = alloca %struct.ScmObj*, align 8
%anf_45bind40274 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53212)
store volatile %struct.ScmObj* %anf_45bind40274, %struct.ScmObj** %stackaddr$prim55501, align 8
%stackaddr$prim55502 = alloca %struct.ScmObj*, align 8
%cpsprim40643 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40272, %struct.ScmObj* %anf_45bind40274)
store volatile %struct.ScmObj* %cpsprim40643, %struct.ScmObj** %stackaddr$prim55502, align 8
%ae40897 = call %struct.ScmObj* @const_init_int(i64 0)
%args53214$k40640$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55503 = alloca %struct.ScmObj*, align 8
%args53214$k40640$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40643, %struct.ScmObj* %args53214$k40640$0)
store volatile %struct.ScmObj* %args53214$k40640$1, %struct.ScmObj** %stackaddr$prim55503, align 8
%stackaddr$prim55504 = alloca %struct.ScmObj*, align 8
%args53214$k40640$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40897, %struct.ScmObj* %args53214$k40640$1)
store volatile %struct.ScmObj* %args53214$k40640$2, %struct.ScmObj** %stackaddr$prim55504, align 8
%clofunc55505 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40640)
musttail call tailcc void %clofunc55505(%struct.ScmObj* %k40640, %struct.ScmObj* %args53214$k40640$2)
ret void
}

define tailcc void @proc_clo$ae40790(%struct.ScmObj* %env$ae40790,%struct.ScmObj* %current_45args53220) {
%stackaddr$prim55506 = alloca %struct.ScmObj*, align 8
%k40644 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53220)
store volatile %struct.ScmObj* %k40644, %struct.ScmObj** %stackaddr$prim55506, align 8
%stackaddr$prim55507 = alloca %struct.ScmObj*, align 8
%current_45args53221 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53220)
store volatile %struct.ScmObj* %current_45args53221, %struct.ScmObj** %stackaddr$prim55507, align 8
%stackaddr$prim55508 = alloca %struct.ScmObj*, align 8
%_37foldr140132 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53221)
store volatile %struct.ScmObj* %_37foldr140132, %struct.ScmObj** %stackaddr$prim55508, align 8
%ae40792 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55509 = alloca %struct.ScmObj*, align 8
%fptrToInt55510 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40793 to i64
%ae40793 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55510)
store volatile %struct.ScmObj* %ae40793, %struct.ScmObj** %stackaddr$makeclosure55509, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40793, %struct.ScmObj* %_37foldr140132, i64 0)
%args53234$k40644$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55511 = alloca %struct.ScmObj*, align 8
%args53234$k40644$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40793, %struct.ScmObj* %args53234$k40644$0)
store volatile %struct.ScmObj* %args53234$k40644$1, %struct.ScmObj** %stackaddr$prim55511, align 8
%stackaddr$prim55512 = alloca %struct.ScmObj*, align 8
%args53234$k40644$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40792, %struct.ScmObj* %args53234$k40644$1)
store volatile %struct.ScmObj* %args53234$k40644$2, %struct.ScmObj** %stackaddr$prim55512, align 8
%clofunc55513 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40644)
musttail call tailcc void %clofunc55513(%struct.ScmObj* %k40644, %struct.ScmObj* %args53234$k40644$2)
ret void
}

define tailcc void @proc_clo$ae40793(%struct.ScmObj* %env$ae40793,%struct.ScmObj* %current_45args53223) {
%stackaddr$env-ref55514 = alloca %struct.ScmObj*, align 8
%_37foldr140132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40793, i64 0)
store %struct.ScmObj* %_37foldr140132, %struct.ScmObj** %stackaddr$env-ref55514
%stackaddr$prim55515 = alloca %struct.ScmObj*, align 8
%k40645 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53223)
store volatile %struct.ScmObj* %k40645, %struct.ScmObj** %stackaddr$prim55515, align 8
%stackaddr$prim55516 = alloca %struct.ScmObj*, align 8
%current_45args53224 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53223)
store volatile %struct.ScmObj* %current_45args53224, %struct.ScmObj** %stackaddr$prim55516, align 8
%stackaddr$prim55517 = alloca %struct.ScmObj*, align 8
%f40135 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53224)
store volatile %struct.ScmObj* %f40135, %struct.ScmObj** %stackaddr$prim55517, align 8
%stackaddr$prim55518 = alloca %struct.ScmObj*, align 8
%current_45args53225 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53224)
store volatile %struct.ScmObj* %current_45args53225, %struct.ScmObj** %stackaddr$prim55518, align 8
%stackaddr$prim55519 = alloca %struct.ScmObj*, align 8
%acc40134 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53225)
store volatile %struct.ScmObj* %acc40134, %struct.ScmObj** %stackaddr$prim55519, align 8
%stackaddr$prim55520 = alloca %struct.ScmObj*, align 8
%current_45args53226 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53225)
store volatile %struct.ScmObj* %current_45args53226, %struct.ScmObj** %stackaddr$prim55520, align 8
%stackaddr$prim55521 = alloca %struct.ScmObj*, align 8
%lst40133 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53226)
store volatile %struct.ScmObj* %lst40133, %struct.ScmObj** %stackaddr$prim55521, align 8
%stackaddr$prim55522 = alloca %struct.ScmObj*, align 8
%anf_45bind40265 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40133)
store volatile %struct.ScmObj* %anf_45bind40265, %struct.ScmObj** %stackaddr$prim55522, align 8
%truthy$cmp55523 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40265)
%cmp$cmp55523 = icmp eq i64 %truthy$cmp55523, 1
br i1 %cmp$cmp55523, label %truebranch$cmp55523, label %falsebranch$cmp55523
truebranch$cmp55523:
%ae40797 = call %struct.ScmObj* @const_init_int(i64 0)
%args53228$k40645$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55524 = alloca %struct.ScmObj*, align 8
%args53228$k40645$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40134, %struct.ScmObj* %args53228$k40645$0)
store volatile %struct.ScmObj* %args53228$k40645$1, %struct.ScmObj** %stackaddr$prim55524, align 8
%stackaddr$prim55525 = alloca %struct.ScmObj*, align 8
%args53228$k40645$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40797, %struct.ScmObj* %args53228$k40645$1)
store volatile %struct.ScmObj* %args53228$k40645$2, %struct.ScmObj** %stackaddr$prim55525, align 8
%clofunc55526 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40645)
musttail call tailcc void %clofunc55526(%struct.ScmObj* %k40645, %struct.ScmObj* %args53228$k40645$2)
ret void
falsebranch$cmp55523:
%stackaddr$prim55527 = alloca %struct.ScmObj*, align 8
%anf_45bind40266 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40133)
store volatile %struct.ScmObj* %anf_45bind40266, %struct.ScmObj** %stackaddr$prim55527, align 8
%stackaddr$prim55528 = alloca %struct.ScmObj*, align 8
%anf_45bind40267 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40133)
store volatile %struct.ScmObj* %anf_45bind40267, %struct.ScmObj** %stackaddr$prim55528, align 8
%stackaddr$makeclosure55529 = alloca %struct.ScmObj*, align 8
%fptrToInt55530 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40805 to i64
%ae40805 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55530)
store volatile %struct.ScmObj* %ae40805, %struct.ScmObj** %stackaddr$makeclosure55529, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40805, %struct.ScmObj* %f40135, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40805, %struct.ScmObj* %k40645, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40805, %struct.ScmObj* %anf_45bind40266, i64 2)
%args53233$_37foldr140132$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55531 = alloca %struct.ScmObj*, align 8
%args53233$_37foldr140132$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40267, %struct.ScmObj* %args53233$_37foldr140132$0)
store volatile %struct.ScmObj* %args53233$_37foldr140132$1, %struct.ScmObj** %stackaddr$prim55531, align 8
%stackaddr$prim55532 = alloca %struct.ScmObj*, align 8
%args53233$_37foldr140132$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40134, %struct.ScmObj* %args53233$_37foldr140132$1)
store volatile %struct.ScmObj* %args53233$_37foldr140132$2, %struct.ScmObj** %stackaddr$prim55532, align 8
%stackaddr$prim55533 = alloca %struct.ScmObj*, align 8
%args53233$_37foldr140132$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40135, %struct.ScmObj* %args53233$_37foldr140132$2)
store volatile %struct.ScmObj* %args53233$_37foldr140132$3, %struct.ScmObj** %stackaddr$prim55533, align 8
%stackaddr$prim55534 = alloca %struct.ScmObj*, align 8
%args53233$_37foldr140132$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40805, %struct.ScmObj* %args53233$_37foldr140132$3)
store volatile %struct.ScmObj* %args53233$_37foldr140132$4, %struct.ScmObj** %stackaddr$prim55534, align 8
%clofunc55535 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140132)
musttail call tailcc void %clofunc55535(%struct.ScmObj* %_37foldr140132, %struct.ScmObj* %args53233$_37foldr140132$4)
ret void
}

define tailcc void @proc_clo$ae40805(%struct.ScmObj* %env$ae40805,%struct.ScmObj* %current_45args53229) {
%stackaddr$env-ref55536 = alloca %struct.ScmObj*, align 8
%f40135 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40805, i64 0)
store %struct.ScmObj* %f40135, %struct.ScmObj** %stackaddr$env-ref55536
%stackaddr$env-ref55537 = alloca %struct.ScmObj*, align 8
%k40645 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40805, i64 1)
store %struct.ScmObj* %k40645, %struct.ScmObj** %stackaddr$env-ref55537
%stackaddr$env-ref55538 = alloca %struct.ScmObj*, align 8
%anf_45bind40266 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40805, i64 2)
store %struct.ScmObj* %anf_45bind40266, %struct.ScmObj** %stackaddr$env-ref55538
%stackaddr$prim55539 = alloca %struct.ScmObj*, align 8
%_95k40646 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53229)
store volatile %struct.ScmObj* %_95k40646, %struct.ScmObj** %stackaddr$prim55539, align 8
%stackaddr$prim55540 = alloca %struct.ScmObj*, align 8
%current_45args53230 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53229)
store volatile %struct.ScmObj* %current_45args53230, %struct.ScmObj** %stackaddr$prim55540, align 8
%stackaddr$prim55541 = alloca %struct.ScmObj*, align 8
%anf_45bind40268 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53230)
store volatile %struct.ScmObj* %anf_45bind40268, %struct.ScmObj** %stackaddr$prim55541, align 8
%args53232$f40135$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55542 = alloca %struct.ScmObj*, align 8
%args53232$f40135$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40268, %struct.ScmObj* %args53232$f40135$0)
store volatile %struct.ScmObj* %args53232$f40135$1, %struct.ScmObj** %stackaddr$prim55542, align 8
%stackaddr$prim55543 = alloca %struct.ScmObj*, align 8
%args53232$f40135$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40266, %struct.ScmObj* %args53232$f40135$1)
store volatile %struct.ScmObj* %args53232$f40135$2, %struct.ScmObj** %stackaddr$prim55543, align 8
%stackaddr$prim55544 = alloca %struct.ScmObj*, align 8
%args53232$f40135$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40645, %struct.ScmObj* %args53232$f40135$2)
store volatile %struct.ScmObj* %args53232$f40135$3, %struct.ScmObj** %stackaddr$prim55544, align 8
%clofunc55545 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40135)
musttail call tailcc void %clofunc55545(%struct.ScmObj* %f40135, %struct.ScmObj* %args53232$f40135$3)
ret void
}

define tailcc void @proc_clo$ae40673(%struct.ScmObj* %env$ae40673,%struct.ScmObj* %current_45args53237) {
%stackaddr$prim55546 = alloca %struct.ScmObj*, align 8
%k40647 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53237)
store volatile %struct.ScmObj* %k40647, %struct.ScmObj** %stackaddr$prim55546, align 8
%stackaddr$prim55547 = alloca %struct.ScmObj*, align 8
%current_45args53238 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53237)
store volatile %struct.ScmObj* %current_45args53238, %struct.ScmObj** %stackaddr$prim55547, align 8
%stackaddr$prim55548 = alloca %struct.ScmObj*, align 8
%y40112 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53238)
store volatile %struct.ScmObj* %y40112, %struct.ScmObj** %stackaddr$prim55548, align 8
%ae40675 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55549 = alloca %struct.ScmObj*, align 8
%fptrToInt55550 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40676 to i64
%ae40676 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55550)
store volatile %struct.ScmObj* %ae40676, %struct.ScmObj** %stackaddr$makeclosure55549, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40676, %struct.ScmObj* %y40112, i64 0)
%args53256$k40647$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55551 = alloca %struct.ScmObj*, align 8
%args53256$k40647$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40676, %struct.ScmObj* %args53256$k40647$0)
store volatile %struct.ScmObj* %args53256$k40647$1, %struct.ScmObj** %stackaddr$prim55551, align 8
%stackaddr$prim55552 = alloca %struct.ScmObj*, align 8
%args53256$k40647$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40675, %struct.ScmObj* %args53256$k40647$1)
store volatile %struct.ScmObj* %args53256$k40647$2, %struct.ScmObj** %stackaddr$prim55552, align 8
%clofunc55553 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40647)
musttail call tailcc void %clofunc55553(%struct.ScmObj* %k40647, %struct.ScmObj* %args53256$k40647$2)
ret void
}

define tailcc void @proc_clo$ae40676(%struct.ScmObj* %env$ae40676,%struct.ScmObj* %current_45args53240) {
%stackaddr$env-ref55554 = alloca %struct.ScmObj*, align 8
%y40112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40676, i64 0)
store %struct.ScmObj* %y40112, %struct.ScmObj** %stackaddr$env-ref55554
%stackaddr$prim55555 = alloca %struct.ScmObj*, align 8
%k40648 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53240)
store volatile %struct.ScmObj* %k40648, %struct.ScmObj** %stackaddr$prim55555, align 8
%stackaddr$prim55556 = alloca %struct.ScmObj*, align 8
%current_45args53241 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53240)
store volatile %struct.ScmObj* %current_45args53241, %struct.ScmObj** %stackaddr$prim55556, align 8
%stackaddr$prim55557 = alloca %struct.ScmObj*, align 8
%f40113 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53241)
store volatile %struct.ScmObj* %f40113, %struct.ScmObj** %stackaddr$prim55557, align 8
%stackaddr$makeclosure55558 = alloca %struct.ScmObj*, align 8
%fptrToInt55559 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40677 to i64
%ae40677 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55559)
store volatile %struct.ScmObj* %ae40677, %struct.ScmObj** %stackaddr$makeclosure55558, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40677, %struct.ScmObj* %k40648, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40677, %struct.ScmObj* %f40113, i64 1)
%ae40678 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55560 = alloca %struct.ScmObj*, align 8
%fptrToInt55561 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40679 to i64
%ae40679 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55561)
store volatile %struct.ScmObj* %ae40679, %struct.ScmObj** %stackaddr$makeclosure55560, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40679, %struct.ScmObj* %f40113, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40679, %struct.ScmObj* %y40112, i64 1)
%args53255$ae40677$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55562 = alloca %struct.ScmObj*, align 8
%args53255$ae40677$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40679, %struct.ScmObj* %args53255$ae40677$0)
store volatile %struct.ScmObj* %args53255$ae40677$1, %struct.ScmObj** %stackaddr$prim55562, align 8
%stackaddr$prim55563 = alloca %struct.ScmObj*, align 8
%args53255$ae40677$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40678, %struct.ScmObj* %args53255$ae40677$1)
store volatile %struct.ScmObj* %args53255$ae40677$2, %struct.ScmObj** %stackaddr$prim55563, align 8
%clofunc55564 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40677)
musttail call tailcc void %clofunc55564(%struct.ScmObj* %ae40677, %struct.ScmObj* %args53255$ae40677$2)
ret void
}

define tailcc void @proc_clo$ae40677(%struct.ScmObj* %env$ae40677,%struct.ScmObj* %current_45args53243) {
%stackaddr$env-ref55565 = alloca %struct.ScmObj*, align 8
%k40648 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40677, i64 0)
store %struct.ScmObj* %k40648, %struct.ScmObj** %stackaddr$env-ref55565
%stackaddr$env-ref55566 = alloca %struct.ScmObj*, align 8
%f40113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40677, i64 1)
store %struct.ScmObj* %f40113, %struct.ScmObj** %stackaddr$env-ref55566
%stackaddr$prim55567 = alloca %struct.ScmObj*, align 8
%_95k40649 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53243)
store volatile %struct.ScmObj* %_95k40649, %struct.ScmObj** %stackaddr$prim55567, align 8
%stackaddr$prim55568 = alloca %struct.ScmObj*, align 8
%current_45args53244 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53243)
store volatile %struct.ScmObj* %current_45args53244, %struct.ScmObj** %stackaddr$prim55568, align 8
%stackaddr$prim55569 = alloca %struct.ScmObj*, align 8
%anf_45bind40263 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53244)
store volatile %struct.ScmObj* %anf_45bind40263, %struct.ScmObj** %stackaddr$prim55569, align 8
%args53246$f40113$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55570 = alloca %struct.ScmObj*, align 8
%args53246$f40113$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40263, %struct.ScmObj* %args53246$f40113$0)
store volatile %struct.ScmObj* %args53246$f40113$1, %struct.ScmObj** %stackaddr$prim55570, align 8
%stackaddr$prim55571 = alloca %struct.ScmObj*, align 8
%args53246$f40113$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40648, %struct.ScmObj* %args53246$f40113$1)
store volatile %struct.ScmObj* %args53246$f40113$2, %struct.ScmObj** %stackaddr$prim55571, align 8
%clofunc55572 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40113)
musttail call tailcc void %clofunc55572(%struct.ScmObj* %f40113, %struct.ScmObj* %args53246$f40113$2)
ret void
}

define tailcc void @proc_clo$ae40679(%struct.ScmObj* %env$ae40679,%struct.ScmObj* %args4011440650) {
%stackaddr$env-ref55573 = alloca %struct.ScmObj*, align 8
%f40113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40679, i64 0)
store %struct.ScmObj* %f40113, %struct.ScmObj** %stackaddr$env-ref55573
%stackaddr$env-ref55574 = alloca %struct.ScmObj*, align 8
%y40112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40679, i64 1)
store %struct.ScmObj* %y40112, %struct.ScmObj** %stackaddr$env-ref55574
%stackaddr$prim55575 = alloca %struct.ScmObj*, align 8
%k40651 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4011440650)
store volatile %struct.ScmObj* %k40651, %struct.ScmObj** %stackaddr$prim55575, align 8
%stackaddr$prim55576 = alloca %struct.ScmObj*, align 8
%args40114 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4011440650)
store volatile %struct.ScmObj* %args40114, %struct.ScmObj** %stackaddr$prim55576, align 8
%stackaddr$makeclosure55577 = alloca %struct.ScmObj*, align 8
%fptrToInt55578 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40683 to i64
%ae40683 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55578)
store volatile %struct.ScmObj* %ae40683, %struct.ScmObj** %stackaddr$makeclosure55577, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40683, %struct.ScmObj* %args40114, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40683, %struct.ScmObj* %f40113, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40683, %struct.ScmObj* %k40651, i64 2)
%args53254$y40112$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55579 = alloca %struct.ScmObj*, align 8
%args53254$y40112$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y40112, %struct.ScmObj* %args53254$y40112$0)
store volatile %struct.ScmObj* %args53254$y40112$1, %struct.ScmObj** %stackaddr$prim55579, align 8
%stackaddr$prim55580 = alloca %struct.ScmObj*, align 8
%args53254$y40112$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40683, %struct.ScmObj* %args53254$y40112$1)
store volatile %struct.ScmObj* %args53254$y40112$2, %struct.ScmObj** %stackaddr$prim55580, align 8
%clofunc55581 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y40112)
musttail call tailcc void %clofunc55581(%struct.ScmObj* %y40112, %struct.ScmObj* %args53254$y40112$2)
ret void
}

define tailcc void @proc_clo$ae40683(%struct.ScmObj* %env$ae40683,%struct.ScmObj* %current_45args53247) {
%stackaddr$env-ref55582 = alloca %struct.ScmObj*, align 8
%args40114 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40683, i64 0)
store %struct.ScmObj* %args40114, %struct.ScmObj** %stackaddr$env-ref55582
%stackaddr$env-ref55583 = alloca %struct.ScmObj*, align 8
%f40113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40683, i64 1)
store %struct.ScmObj* %f40113, %struct.ScmObj** %stackaddr$env-ref55583
%stackaddr$env-ref55584 = alloca %struct.ScmObj*, align 8
%k40651 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40683, i64 2)
store %struct.ScmObj* %k40651, %struct.ScmObj** %stackaddr$env-ref55584
%stackaddr$prim55585 = alloca %struct.ScmObj*, align 8
%_95k40652 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53247)
store volatile %struct.ScmObj* %_95k40652, %struct.ScmObj** %stackaddr$prim55585, align 8
%stackaddr$prim55586 = alloca %struct.ScmObj*, align 8
%current_45args53248 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53247)
store volatile %struct.ScmObj* %current_45args53248, %struct.ScmObj** %stackaddr$prim55586, align 8
%stackaddr$prim55587 = alloca %struct.ScmObj*, align 8
%anf_45bind40261 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53248)
store volatile %struct.ScmObj* %anf_45bind40261, %struct.ScmObj** %stackaddr$prim55587, align 8
%stackaddr$makeclosure55588 = alloca %struct.ScmObj*, align 8
%fptrToInt55589 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40686 to i64
%ae40686 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55589)
store volatile %struct.ScmObj* %ae40686, %struct.ScmObj** %stackaddr$makeclosure55588, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40686, %struct.ScmObj* %args40114, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40686, %struct.ScmObj* %k40651, i64 1)
%args53253$anf_45bind40261$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55590 = alloca %struct.ScmObj*, align 8
%args53253$anf_45bind40261$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40113, %struct.ScmObj* %args53253$anf_45bind40261$0)
store volatile %struct.ScmObj* %args53253$anf_45bind40261$1, %struct.ScmObj** %stackaddr$prim55590, align 8
%stackaddr$prim55591 = alloca %struct.ScmObj*, align 8
%args53253$anf_45bind40261$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40686, %struct.ScmObj* %args53253$anf_45bind40261$1)
store volatile %struct.ScmObj* %args53253$anf_45bind40261$2, %struct.ScmObj** %stackaddr$prim55591, align 8
%clofunc55592 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40261)
musttail call tailcc void %clofunc55592(%struct.ScmObj* %anf_45bind40261, %struct.ScmObj* %args53253$anf_45bind40261$2)
ret void
}

define tailcc void @proc_clo$ae40686(%struct.ScmObj* %env$ae40686,%struct.ScmObj* %current_45args53250) {
%stackaddr$env-ref55593 = alloca %struct.ScmObj*, align 8
%args40114 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40686, i64 0)
store %struct.ScmObj* %args40114, %struct.ScmObj** %stackaddr$env-ref55593
%stackaddr$env-ref55594 = alloca %struct.ScmObj*, align 8
%k40651 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40686, i64 1)
store %struct.ScmObj* %k40651, %struct.ScmObj** %stackaddr$env-ref55594
%stackaddr$prim55595 = alloca %struct.ScmObj*, align 8
%_95k40653 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53250)
store volatile %struct.ScmObj* %_95k40653, %struct.ScmObj** %stackaddr$prim55595, align 8
%stackaddr$prim55596 = alloca %struct.ScmObj*, align 8
%current_45args53251 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53250)
store volatile %struct.ScmObj* %current_45args53251, %struct.ScmObj** %stackaddr$prim55596, align 8
%stackaddr$prim55597 = alloca %struct.ScmObj*, align 8
%anf_45bind40262 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53251)
store volatile %struct.ScmObj* %anf_45bind40262, %struct.ScmObj** %stackaddr$prim55597, align 8
%stackaddr$prim55598 = alloca %struct.ScmObj*, align 8
%cpsargs40654 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40651, %struct.ScmObj* %args40114)
store volatile %struct.ScmObj* %cpsargs40654, %struct.ScmObj** %stackaddr$prim55598, align 8
%clofunc55599 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40262)
musttail call tailcc void %clofunc55599(%struct.ScmObj* %anf_45bind40262, %struct.ScmObj* %cpsargs40654)
ret void
}

define tailcc void @proc_clo$ae40658(%struct.ScmObj* %env$ae40658,%struct.ScmObj* %current_45args53258) {
%stackaddr$prim55600 = alloca %struct.ScmObj*, align 8
%k40655 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53258)
store volatile %struct.ScmObj* %k40655, %struct.ScmObj** %stackaddr$prim55600, align 8
%stackaddr$prim55601 = alloca %struct.ScmObj*, align 8
%current_45args53259 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53258)
store volatile %struct.ScmObj* %current_45args53259, %struct.ScmObj** %stackaddr$prim55601, align 8
%stackaddr$prim55602 = alloca %struct.ScmObj*, align 8
%yu40111 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53259)
store volatile %struct.ScmObj* %yu40111, %struct.ScmObj** %stackaddr$prim55602, align 8
%args53261$yu40111$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55603 = alloca %struct.ScmObj*, align 8
%args53261$yu40111$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu40111, %struct.ScmObj* %args53261$yu40111$0)
store volatile %struct.ScmObj* %args53261$yu40111$1, %struct.ScmObj** %stackaddr$prim55603, align 8
%stackaddr$prim55604 = alloca %struct.ScmObj*, align 8
%args53261$yu40111$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40655, %struct.ScmObj* %args53261$yu40111$1)
store volatile %struct.ScmObj* %args53261$yu40111$2, %struct.ScmObj** %stackaddr$prim55604, align 8
%clofunc55605 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu40111)
musttail call tailcc void %clofunc55605(%struct.ScmObj* %yu40111, %struct.ScmObj* %args53261$yu40111$2)
ret void
}