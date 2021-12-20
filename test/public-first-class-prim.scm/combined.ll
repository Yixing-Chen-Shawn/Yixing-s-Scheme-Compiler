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
%mainenv53948 = call %struct.ScmObj* @const_init_null()
%mainargs53949 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv53948, %struct.ScmObj* %mainargs53949)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv53946,%struct.ScmObj* %mainargs53947) {
%stackaddr$makeclosure53950 = alloca %struct.ScmObj*, align 8
%fptrToInt53951 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47504 to i64
%ae47504 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53951)
store volatile %struct.ScmObj* %ae47504, %struct.ScmObj** %stackaddr$makeclosure53950, align 8
%ae47505 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53952 = alloca %struct.ScmObj*, align 8
%fptrToInt53953 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47506 to i64
%ae47506 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53953)
store volatile %struct.ScmObj* %ae47506, %struct.ScmObj** %stackaddr$makeclosure53952, align 8
%argslist53945$ae475040 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53954 = alloca %struct.ScmObj*, align 8
%argslist53945$ae475041 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47506, %struct.ScmObj* %argslist53945$ae475040)
store volatile %struct.ScmObj* %argslist53945$ae475041, %struct.ScmObj** %stackaddr$prim53954, align 8
%stackaddr$prim53955 = alloca %struct.ScmObj*, align 8
%argslist53945$ae475042 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47505, %struct.ScmObj* %argslist53945$ae475041)
store volatile %struct.ScmObj* %argslist53945$ae475042, %struct.ScmObj** %stackaddr$prim53955, align 8
%clofunc53956 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47504)
musttail call tailcc void %clofunc53956(%struct.ScmObj* %ae47504, %struct.ScmObj* %argslist53945$ae475042)
ret void
}

define tailcc void @proc_clo$ae47504(%struct.ScmObj* %env$ae47504,%struct.ScmObj* %current_45args53364) {
%stackaddr$prim53957 = alloca %struct.ScmObj*, align 8
%_95k47318 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53364)
store volatile %struct.ScmObj* %_95k47318, %struct.ScmObj** %stackaddr$prim53957, align 8
%stackaddr$prim53958 = alloca %struct.ScmObj*, align 8
%current_45args53365 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53364)
store volatile %struct.ScmObj* %current_45args53365, %struct.ScmObj** %stackaddr$prim53958, align 8
%stackaddr$prim53959 = alloca %struct.ScmObj*, align 8
%anf_45bind47194 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53365)
store volatile %struct.ScmObj* %anf_45bind47194, %struct.ScmObj** %stackaddr$prim53959, align 8
%stackaddr$makeclosure53960 = alloca %struct.ScmObj*, align 8
%fptrToInt53961 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47519 to i64
%ae47519 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53961)
store volatile %struct.ScmObj* %ae47519, %struct.ScmObj** %stackaddr$makeclosure53960, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47519, %struct.ScmObj* %anf_45bind47194, i64 0)
%ae47520 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53962 = alloca %struct.ScmObj*, align 8
%fptrToInt53963 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47521 to i64
%ae47521 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53963)
store volatile %struct.ScmObj* %ae47521, %struct.ScmObj** %stackaddr$makeclosure53962, align 8
%argslist53940$ae475190 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53964 = alloca %struct.ScmObj*, align 8
%argslist53940$ae475191 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47521, %struct.ScmObj* %argslist53940$ae475190)
store volatile %struct.ScmObj* %argslist53940$ae475191, %struct.ScmObj** %stackaddr$prim53964, align 8
%stackaddr$prim53965 = alloca %struct.ScmObj*, align 8
%argslist53940$ae475192 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47520, %struct.ScmObj* %argslist53940$ae475191)
store volatile %struct.ScmObj* %argslist53940$ae475192, %struct.ScmObj** %stackaddr$prim53965, align 8
%clofunc53966 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47519)
musttail call tailcc void %clofunc53966(%struct.ScmObj* %ae47519, %struct.ScmObj* %argslist53940$ae475192)
ret void
}

define tailcc void @proc_clo$ae47519(%struct.ScmObj* %env$ae47519,%struct.ScmObj* %current_45args53367) {
%stackaddr$env-ref53967 = alloca %struct.ScmObj*, align 8
%anf_45bind47194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47519, i64 0)
store %struct.ScmObj* %anf_45bind47194, %struct.ScmObj** %stackaddr$env-ref53967
%stackaddr$prim53968 = alloca %struct.ScmObj*, align 8
%_95k47319 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53367)
store volatile %struct.ScmObj* %_95k47319, %struct.ScmObj** %stackaddr$prim53968, align 8
%stackaddr$prim53969 = alloca %struct.ScmObj*, align 8
%current_45args53368 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53367)
store volatile %struct.ScmObj* %current_45args53368, %struct.ScmObj** %stackaddr$prim53969, align 8
%stackaddr$prim53970 = alloca %struct.ScmObj*, align 8
%anf_45bind47198 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53368)
store volatile %struct.ScmObj* %anf_45bind47198, %struct.ScmObj** %stackaddr$prim53970, align 8
%stackaddr$makeclosure53971 = alloca %struct.ScmObj*, align 8
%fptrToInt53972 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47634 to i64
%ae47634 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53972)
store volatile %struct.ScmObj* %ae47634, %struct.ScmObj** %stackaddr$makeclosure53971, align 8
%argslist53919$anf_45bind471940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53973 = alloca %struct.ScmObj*, align 8
%argslist53919$anf_45bind471941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47198, %struct.ScmObj* %argslist53919$anf_45bind471940)
store volatile %struct.ScmObj* %argslist53919$anf_45bind471941, %struct.ScmObj** %stackaddr$prim53973, align 8
%stackaddr$prim53974 = alloca %struct.ScmObj*, align 8
%argslist53919$anf_45bind471942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47634, %struct.ScmObj* %argslist53919$anf_45bind471941)
store volatile %struct.ScmObj* %argslist53919$anf_45bind471942, %struct.ScmObj** %stackaddr$prim53974, align 8
%clofunc53975 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47194)
musttail call tailcc void %clofunc53975(%struct.ScmObj* %anf_45bind47194, %struct.ScmObj* %argslist53919$anf_45bind471942)
ret void
}

define tailcc void @proc_clo$ae47634(%struct.ScmObj* %env$ae47634,%struct.ScmObj* %current_45args53370) {
%stackaddr$prim53976 = alloca %struct.ScmObj*, align 8
%_95k47320 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53370)
store volatile %struct.ScmObj* %_95k47320, %struct.ScmObj** %stackaddr$prim53976, align 8
%stackaddr$prim53977 = alloca %struct.ScmObj*, align 8
%current_45args53371 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53370)
store volatile %struct.ScmObj* %current_45args53371, %struct.ScmObj** %stackaddr$prim53977, align 8
%stackaddr$prim53978 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53371)
store volatile %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$prim53978, align 8
%stackaddr$makeclosure53979 = alloca %struct.ScmObj*, align 8
%fptrToInt53980 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47636 to i64
%ae47636 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53980)
store volatile %struct.ScmObj* %ae47636, %struct.ScmObj** %stackaddr$makeclosure53979, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47636, %struct.ScmObj* %Ycmb47068, i64 0)
%ae47637 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53981 = alloca %struct.ScmObj*, align 8
%fptrToInt53982 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47638 to i64
%ae47638 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53982)
store volatile %struct.ScmObj* %ae47638, %struct.ScmObj** %stackaddr$makeclosure53981, align 8
%argslist53918$ae476360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53983 = alloca %struct.ScmObj*, align 8
%argslist53918$ae476361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47638, %struct.ScmObj* %argslist53918$ae476360)
store volatile %struct.ScmObj* %argslist53918$ae476361, %struct.ScmObj** %stackaddr$prim53983, align 8
%stackaddr$prim53984 = alloca %struct.ScmObj*, align 8
%argslist53918$ae476362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47637, %struct.ScmObj* %argslist53918$ae476361)
store volatile %struct.ScmObj* %argslist53918$ae476362, %struct.ScmObj** %stackaddr$prim53984, align 8
%clofunc53985 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47636)
musttail call tailcc void %clofunc53985(%struct.ScmObj* %ae47636, %struct.ScmObj* %argslist53918$ae476362)
ret void
}

define tailcc void @proc_clo$ae47636(%struct.ScmObj* %env$ae47636,%struct.ScmObj* %current_45args53373) {
%stackaddr$env-ref53986 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47636, i64 0)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53986
%stackaddr$prim53987 = alloca %struct.ScmObj*, align 8
%_95k47321 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53373)
store volatile %struct.ScmObj* %_95k47321, %struct.ScmObj** %stackaddr$prim53987, align 8
%stackaddr$prim53988 = alloca %struct.ScmObj*, align 8
%current_45args53374 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53373)
store volatile %struct.ScmObj* %current_45args53374, %struct.ScmObj** %stackaddr$prim53988, align 8
%stackaddr$prim53989 = alloca %struct.ScmObj*, align 8
%anf_45bind47203 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53374)
store volatile %struct.ScmObj* %anf_45bind47203, %struct.ScmObj** %stackaddr$prim53989, align 8
%stackaddr$makeclosure53990 = alloca %struct.ScmObj*, align 8
%fptrToInt53991 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47714 to i64
%ae47714 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53991)
store volatile %struct.ScmObj* %ae47714, %struct.ScmObj** %stackaddr$makeclosure53990, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47714, %struct.ScmObj* %Ycmb47068, i64 0)
%argslist53902$Ycmb470680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53992 = alloca %struct.ScmObj*, align 8
%argslist53902$Ycmb470681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47203, %struct.ScmObj* %argslist53902$Ycmb470680)
store volatile %struct.ScmObj* %argslist53902$Ycmb470681, %struct.ScmObj** %stackaddr$prim53992, align 8
%stackaddr$prim53993 = alloca %struct.ScmObj*, align 8
%argslist53902$Ycmb470682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47714, %struct.ScmObj* %argslist53902$Ycmb470681)
store volatile %struct.ScmObj* %argslist53902$Ycmb470682, %struct.ScmObj** %stackaddr$prim53993, align 8
%clofunc53994 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47068)
musttail call tailcc void %clofunc53994(%struct.ScmObj* %Ycmb47068, %struct.ScmObj* %argslist53902$Ycmb470682)
ret void
}

define tailcc void @proc_clo$ae47714(%struct.ScmObj* %env$ae47714,%struct.ScmObj* %current_45args53376) {
%stackaddr$env-ref53995 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47714, i64 0)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53995
%stackaddr$prim53996 = alloca %struct.ScmObj*, align 8
%_95k47322 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53376)
store volatile %struct.ScmObj* %_95k47322, %struct.ScmObj** %stackaddr$prim53996, align 8
%stackaddr$prim53997 = alloca %struct.ScmObj*, align 8
%current_45args53377 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53376)
store volatile %struct.ScmObj* %current_45args53377, %struct.ScmObj** %stackaddr$prim53997, align 8
%stackaddr$prim53998 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53377)
store volatile %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$prim53998, align 8
%stackaddr$makeclosure53999 = alloca %struct.ScmObj*, align 8
%fptrToInt54000 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47716 to i64
%ae47716 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54000)
store volatile %struct.ScmObj* %ae47716, %struct.ScmObj** %stackaddr$makeclosure53999, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47716, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47716, %struct.ScmObj* %Ycmb47068, i64 1)
%ae47717 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54001 = alloca %struct.ScmObj*, align 8
%fptrToInt54002 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47718 to i64
%ae47718 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54002)
store volatile %struct.ScmObj* %ae47718, %struct.ScmObj** %stackaddr$makeclosure54001, align 8
%argslist53901$ae477160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54003 = alloca %struct.ScmObj*, align 8
%argslist53901$ae477161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47718, %struct.ScmObj* %argslist53901$ae477160)
store volatile %struct.ScmObj* %argslist53901$ae477161, %struct.ScmObj** %stackaddr$prim54003, align 8
%stackaddr$prim54004 = alloca %struct.ScmObj*, align 8
%argslist53901$ae477162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47717, %struct.ScmObj* %argslist53901$ae477161)
store volatile %struct.ScmObj* %argslist53901$ae477162, %struct.ScmObj** %stackaddr$prim54004, align 8
%clofunc54005 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47716)
musttail call tailcc void %clofunc54005(%struct.ScmObj* %ae47716, %struct.ScmObj* %argslist53901$ae477162)
ret void
}

define tailcc void @proc_clo$ae47716(%struct.ScmObj* %env$ae47716,%struct.ScmObj* %current_45args53379) {
%stackaddr$env-ref54006 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47716, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54006
%stackaddr$env-ref54007 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47716, i64 1)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54007
%stackaddr$prim54008 = alloca %struct.ScmObj*, align 8
%_95k47323 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53379)
store volatile %struct.ScmObj* %_95k47323, %struct.ScmObj** %stackaddr$prim54008, align 8
%stackaddr$prim54009 = alloca %struct.ScmObj*, align 8
%current_45args53380 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53379)
store volatile %struct.ScmObj* %current_45args53380, %struct.ScmObj** %stackaddr$prim54009, align 8
%stackaddr$prim54010 = alloca %struct.ScmObj*, align 8
%anf_45bind47209 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53380)
store volatile %struct.ScmObj* %anf_45bind47209, %struct.ScmObj** %stackaddr$prim54010, align 8
%stackaddr$makeclosure54011 = alloca %struct.ScmObj*, align 8
%fptrToInt54012 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47811 to i64
%ae47811 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54012)
store volatile %struct.ScmObj* %ae47811, %struct.ScmObj** %stackaddr$makeclosure54011, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47811, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47811, %struct.ScmObj* %Ycmb47068, i64 1)
%argslist53882$Ycmb470680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54013 = alloca %struct.ScmObj*, align 8
%argslist53882$Ycmb470681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47209, %struct.ScmObj* %argslist53882$Ycmb470680)
store volatile %struct.ScmObj* %argslist53882$Ycmb470681, %struct.ScmObj** %stackaddr$prim54013, align 8
%stackaddr$prim54014 = alloca %struct.ScmObj*, align 8
%argslist53882$Ycmb470682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47811, %struct.ScmObj* %argslist53882$Ycmb470681)
store volatile %struct.ScmObj* %argslist53882$Ycmb470682, %struct.ScmObj** %stackaddr$prim54014, align 8
%clofunc54015 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47068)
musttail call tailcc void %clofunc54015(%struct.ScmObj* %Ycmb47068, %struct.ScmObj* %argslist53882$Ycmb470682)
ret void
}

define tailcc void @proc_clo$ae47811(%struct.ScmObj* %env$ae47811,%struct.ScmObj* %current_45args53382) {
%stackaddr$env-ref54016 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47811, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54016
%stackaddr$env-ref54017 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47811, i64 1)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54017
%stackaddr$prim54018 = alloca %struct.ScmObj*, align 8
%_95k47324 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53382)
store volatile %struct.ScmObj* %_95k47324, %struct.ScmObj** %stackaddr$prim54018, align 8
%stackaddr$prim54019 = alloca %struct.ScmObj*, align 8
%current_45args53383 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53382)
store volatile %struct.ScmObj* %current_45args53383, %struct.ScmObj** %stackaddr$prim54019, align 8
%stackaddr$prim54020 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53383)
store volatile %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$prim54020, align 8
%stackaddr$makeclosure54021 = alloca %struct.ScmObj*, align 8
%fptrToInt54022 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47813 to i64
%ae47813 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54022)
store volatile %struct.ScmObj* %ae47813, %struct.ScmObj** %stackaddr$makeclosure54021, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47813, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47813, %struct.ScmObj* %_37map147085, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47813, %struct.ScmObj* %Ycmb47068, i64 2)
%ae47814 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54023 = alloca %struct.ScmObj*, align 8
%fptrToInt54024 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47815 to i64
%ae47815 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54024)
store volatile %struct.ScmObj* %ae47815, %struct.ScmObj** %stackaddr$makeclosure54023, align 8
%argslist53881$ae478130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54025 = alloca %struct.ScmObj*, align 8
%argslist53881$ae478131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47815, %struct.ScmObj* %argslist53881$ae478130)
store volatile %struct.ScmObj* %argslist53881$ae478131, %struct.ScmObj** %stackaddr$prim54025, align 8
%stackaddr$prim54026 = alloca %struct.ScmObj*, align 8
%argslist53881$ae478132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47814, %struct.ScmObj* %argslist53881$ae478131)
store volatile %struct.ScmObj* %argslist53881$ae478132, %struct.ScmObj** %stackaddr$prim54026, align 8
%clofunc54027 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47813)
musttail call tailcc void %clofunc54027(%struct.ScmObj* %ae47813, %struct.ScmObj* %argslist53881$ae478132)
ret void
}

define tailcc void @proc_clo$ae47813(%struct.ScmObj* %env$ae47813,%struct.ScmObj* %current_45args53385) {
%stackaddr$env-ref54028 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47813, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54028
%stackaddr$env-ref54029 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47813, i64 1)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref54029
%stackaddr$env-ref54030 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47813, i64 2)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54030
%stackaddr$prim54031 = alloca %struct.ScmObj*, align 8
%_95k47325 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53385)
store volatile %struct.ScmObj* %_95k47325, %struct.ScmObj** %stackaddr$prim54031, align 8
%stackaddr$prim54032 = alloca %struct.ScmObj*, align 8
%current_45args53386 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53385)
store volatile %struct.ScmObj* %current_45args53386, %struct.ScmObj** %stackaddr$prim54032, align 8
%stackaddr$prim54033 = alloca %struct.ScmObj*, align 8
%anf_45bind47216 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53386)
store volatile %struct.ScmObj* %anf_45bind47216, %struct.ScmObj** %stackaddr$prim54033, align 8
%stackaddr$makeclosure54034 = alloca %struct.ScmObj*, align 8
%fptrToInt54035 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47961 to i64
%ae47961 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54035)
store volatile %struct.ScmObj* %ae47961, %struct.ScmObj** %stackaddr$makeclosure54034, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47961, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47961, %struct.ScmObj* %_37map147085, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47961, %struct.ScmObj* %Ycmb47068, i64 2)
%argslist53865$Ycmb470680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54036 = alloca %struct.ScmObj*, align 8
%argslist53865$Ycmb470681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47216, %struct.ScmObj* %argslist53865$Ycmb470680)
store volatile %struct.ScmObj* %argslist53865$Ycmb470681, %struct.ScmObj** %stackaddr$prim54036, align 8
%stackaddr$prim54037 = alloca %struct.ScmObj*, align 8
%argslist53865$Ycmb470682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47961, %struct.ScmObj* %argslist53865$Ycmb470681)
store volatile %struct.ScmObj* %argslist53865$Ycmb470682, %struct.ScmObj** %stackaddr$prim54037, align 8
%clofunc54038 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47068)
musttail call tailcc void %clofunc54038(%struct.ScmObj* %Ycmb47068, %struct.ScmObj* %argslist53865$Ycmb470682)
ret void
}

define tailcc void @proc_clo$ae47961(%struct.ScmObj* %env$ae47961,%struct.ScmObj* %current_45args53388) {
%stackaddr$env-ref54039 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47961, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54039
%stackaddr$env-ref54040 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47961, i64 1)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref54040
%stackaddr$env-ref54041 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47961, i64 2)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54041
%stackaddr$prim54042 = alloca %struct.ScmObj*, align 8
%_95k47326 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53388)
store volatile %struct.ScmObj* %_95k47326, %struct.ScmObj** %stackaddr$prim54042, align 8
%stackaddr$prim54043 = alloca %struct.ScmObj*, align 8
%current_45args53389 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53388)
store volatile %struct.ScmObj* %current_45args53389, %struct.ScmObj** %stackaddr$prim54043, align 8
%stackaddr$prim54044 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53389)
store volatile %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$prim54044, align 8
%stackaddr$makeclosure54045 = alloca %struct.ScmObj*, align 8
%fptrToInt54046 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47963 to i64
%ae47963 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54046)
store volatile %struct.ScmObj* %ae47963, %struct.ScmObj** %stackaddr$makeclosure54045, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47963, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47963, %struct.ScmObj* %_37map147085, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47963, %struct.ScmObj* %Ycmb47068, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae47963, %struct.ScmObj* %_37take47081, i64 3)
%ae47964 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54047 = alloca %struct.ScmObj*, align 8
%fptrToInt54048 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47965 to i64
%ae47965 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54048)
store volatile %struct.ScmObj* %ae47965, %struct.ScmObj** %stackaddr$makeclosure54047, align 8
%argslist53864$ae479630 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54049 = alloca %struct.ScmObj*, align 8
%argslist53864$ae479631 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47965, %struct.ScmObj* %argslist53864$ae479630)
store volatile %struct.ScmObj* %argslist53864$ae479631, %struct.ScmObj** %stackaddr$prim54049, align 8
%stackaddr$prim54050 = alloca %struct.ScmObj*, align 8
%argslist53864$ae479632 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47964, %struct.ScmObj* %argslist53864$ae479631)
store volatile %struct.ScmObj* %argslist53864$ae479632, %struct.ScmObj** %stackaddr$prim54050, align 8
%clofunc54051 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47963)
musttail call tailcc void %clofunc54051(%struct.ScmObj* %ae47963, %struct.ScmObj* %argslist53864$ae479632)
ret void
}

define tailcc void @proc_clo$ae47963(%struct.ScmObj* %env$ae47963,%struct.ScmObj* %current_45args53391) {
%stackaddr$env-ref54052 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47963, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54052
%stackaddr$env-ref54053 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47963, i64 1)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref54053
%stackaddr$env-ref54054 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47963, i64 2)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54054
%stackaddr$env-ref54055 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47963, i64 3)
store %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$env-ref54055
%stackaddr$prim54056 = alloca %struct.ScmObj*, align 8
%_95k47327 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53391)
store volatile %struct.ScmObj* %_95k47327, %struct.ScmObj** %stackaddr$prim54056, align 8
%stackaddr$prim54057 = alloca %struct.ScmObj*, align 8
%current_45args53392 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53391)
store volatile %struct.ScmObj* %current_45args53392, %struct.ScmObj** %stackaddr$prim54057, align 8
%stackaddr$prim54058 = alloca %struct.ScmObj*, align 8
%anf_45bind47220 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53392)
store volatile %struct.ScmObj* %anf_45bind47220, %struct.ScmObj** %stackaddr$prim54058, align 8
%stackaddr$makeclosure54059 = alloca %struct.ScmObj*, align 8
%fptrToInt54060 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48044 to i64
%ae48044 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54060)
store volatile %struct.ScmObj* %ae48044, %struct.ScmObj** %stackaddr$makeclosure54059, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48044, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48044, %struct.ScmObj* %_37map147085, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48044, %struct.ScmObj* %Ycmb47068, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48044, %struct.ScmObj* %_37take47081, i64 3)
%argslist53850$Ycmb470680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54061 = alloca %struct.ScmObj*, align 8
%argslist53850$Ycmb470681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47220, %struct.ScmObj* %argslist53850$Ycmb470680)
store volatile %struct.ScmObj* %argslist53850$Ycmb470681, %struct.ScmObj** %stackaddr$prim54061, align 8
%stackaddr$prim54062 = alloca %struct.ScmObj*, align 8
%argslist53850$Ycmb470682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48044, %struct.ScmObj* %argslist53850$Ycmb470681)
store volatile %struct.ScmObj* %argslist53850$Ycmb470682, %struct.ScmObj** %stackaddr$prim54062, align 8
%clofunc54063 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47068)
musttail call tailcc void %clofunc54063(%struct.ScmObj* %Ycmb47068, %struct.ScmObj* %argslist53850$Ycmb470682)
ret void
}

define tailcc void @proc_clo$ae48044(%struct.ScmObj* %env$ae48044,%struct.ScmObj* %current_45args53394) {
%stackaddr$env-ref54064 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48044, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54064
%stackaddr$env-ref54065 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48044, i64 1)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref54065
%stackaddr$env-ref54066 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48044, i64 2)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54066
%stackaddr$env-ref54067 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48044, i64 3)
store %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$env-ref54067
%stackaddr$prim54068 = alloca %struct.ScmObj*, align 8
%_95k47328 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53394)
store volatile %struct.ScmObj* %_95k47328, %struct.ScmObj** %stackaddr$prim54068, align 8
%stackaddr$prim54069 = alloca %struct.ScmObj*, align 8
%current_45args53395 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53394)
store volatile %struct.ScmObj* %current_45args53395, %struct.ScmObj** %stackaddr$prim54069, align 8
%stackaddr$prim54070 = alloca %struct.ScmObj*, align 8
%_37length47078 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53395)
store volatile %struct.ScmObj* %_37length47078, %struct.ScmObj** %stackaddr$prim54070, align 8
%stackaddr$makeclosure54071 = alloca %struct.ScmObj*, align 8
%fptrToInt54072 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48046 to i64
%ae48046 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54072)
store volatile %struct.ScmObj* %ae48046, %struct.ScmObj** %stackaddr$makeclosure54071, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48046, %struct.ScmObj* %_37length47078, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48046, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48046, %struct.ScmObj* %_37map147085, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48046, %struct.ScmObj* %Ycmb47068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48046, %struct.ScmObj* %_37take47081, i64 4)
%ae48047 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54073 = alloca %struct.ScmObj*, align 8
%fptrToInt54074 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48048 to i64
%ae48048 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54074)
store volatile %struct.ScmObj* %ae48048, %struct.ScmObj** %stackaddr$makeclosure54073, align 8
%argslist53849$ae480460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54075 = alloca %struct.ScmObj*, align 8
%argslist53849$ae480461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48048, %struct.ScmObj* %argslist53849$ae480460)
store volatile %struct.ScmObj* %argslist53849$ae480461, %struct.ScmObj** %stackaddr$prim54075, align 8
%stackaddr$prim54076 = alloca %struct.ScmObj*, align 8
%argslist53849$ae480462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48047, %struct.ScmObj* %argslist53849$ae480461)
store volatile %struct.ScmObj* %argslist53849$ae480462, %struct.ScmObj** %stackaddr$prim54076, align 8
%clofunc54077 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48046)
musttail call tailcc void %clofunc54077(%struct.ScmObj* %ae48046, %struct.ScmObj* %argslist53849$ae480462)
ret void
}

define tailcc void @proc_clo$ae48046(%struct.ScmObj* %env$ae48046,%struct.ScmObj* %current_45args53397) {
%stackaddr$env-ref54078 = alloca %struct.ScmObj*, align 8
%_37length47078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48046, i64 0)
store %struct.ScmObj* %_37length47078, %struct.ScmObj** %stackaddr$env-ref54078
%stackaddr$env-ref54079 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48046, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54079
%stackaddr$env-ref54080 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48046, i64 2)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref54080
%stackaddr$env-ref54081 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48046, i64 3)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54081
%stackaddr$env-ref54082 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48046, i64 4)
store %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$env-ref54082
%stackaddr$prim54083 = alloca %struct.ScmObj*, align 8
%_95k47329 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53397)
store volatile %struct.ScmObj* %_95k47329, %struct.ScmObj** %stackaddr$prim54083, align 8
%stackaddr$prim54084 = alloca %struct.ScmObj*, align 8
%current_45args53398 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53397)
store volatile %struct.ScmObj* %current_45args53398, %struct.ScmObj** %stackaddr$prim54084, align 8
%stackaddr$prim54085 = alloca %struct.ScmObj*, align 8
%anf_45bind47225 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53398)
store volatile %struct.ScmObj* %anf_45bind47225, %struct.ScmObj** %stackaddr$prim54085, align 8
%stackaddr$makeclosure54086 = alloca %struct.ScmObj*, align 8
%fptrToInt54087 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48123 to i64
%ae48123 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54087)
store volatile %struct.ScmObj* %ae48123, %struct.ScmObj** %stackaddr$makeclosure54086, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48123, %struct.ScmObj* %_37length47078, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48123, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48123, %struct.ScmObj* %_37map147085, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48123, %struct.ScmObj* %Ycmb47068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48123, %struct.ScmObj* %_37take47081, i64 4)
%argslist53833$Ycmb470680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54088 = alloca %struct.ScmObj*, align 8
%argslist53833$Ycmb470681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47225, %struct.ScmObj* %argslist53833$Ycmb470680)
store volatile %struct.ScmObj* %argslist53833$Ycmb470681, %struct.ScmObj** %stackaddr$prim54088, align 8
%stackaddr$prim54089 = alloca %struct.ScmObj*, align 8
%argslist53833$Ycmb470682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48123, %struct.ScmObj* %argslist53833$Ycmb470681)
store volatile %struct.ScmObj* %argslist53833$Ycmb470682, %struct.ScmObj** %stackaddr$prim54089, align 8
%clofunc54090 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47068)
musttail call tailcc void %clofunc54090(%struct.ScmObj* %Ycmb47068, %struct.ScmObj* %argslist53833$Ycmb470682)
ret void
}

define tailcc void @proc_clo$ae48123(%struct.ScmObj* %env$ae48123,%struct.ScmObj* %current_45args53400) {
%stackaddr$env-ref54091 = alloca %struct.ScmObj*, align 8
%_37length47078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48123, i64 0)
store %struct.ScmObj* %_37length47078, %struct.ScmObj** %stackaddr$env-ref54091
%stackaddr$env-ref54092 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48123, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54092
%stackaddr$env-ref54093 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48123, i64 2)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref54093
%stackaddr$env-ref54094 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48123, i64 3)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54094
%stackaddr$env-ref54095 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48123, i64 4)
store %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$env-ref54095
%stackaddr$prim54096 = alloca %struct.ScmObj*, align 8
%_95k47330 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53400)
store volatile %struct.ScmObj* %_95k47330, %struct.ScmObj** %stackaddr$prim54096, align 8
%stackaddr$prim54097 = alloca %struct.ScmObj*, align 8
%current_45args53401 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53400)
store volatile %struct.ScmObj* %current_45args53401, %struct.ScmObj** %stackaddr$prim54097, align 8
%stackaddr$prim54098 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53401)
store volatile %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$prim54098, align 8
%stackaddr$makeclosure54099 = alloca %struct.ScmObj*, align 8
%fptrToInt54100 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48125 to i64
%ae48125 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt54100)
store volatile %struct.ScmObj* %ae48125, %struct.ScmObj** %stackaddr$makeclosure54099, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48125, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48125, %struct.ScmObj* %_37foldl147073, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48125, %struct.ScmObj* %_37length47078, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48125, %struct.ScmObj* %_37map147085, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48125, %struct.ScmObj* %Ycmb47068, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48125, %struct.ScmObj* %_37take47081, i64 5)
%ae48126 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54101 = alloca %struct.ScmObj*, align 8
%fptrToInt54102 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48127 to i64
%ae48127 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54102)
store volatile %struct.ScmObj* %ae48127, %struct.ScmObj** %stackaddr$makeclosure54101, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48127, %struct.ScmObj* %_37foldl147073, i64 0)
%argslist53832$ae481250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54103 = alloca %struct.ScmObj*, align 8
%argslist53832$ae481251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48127, %struct.ScmObj* %argslist53832$ae481250)
store volatile %struct.ScmObj* %argslist53832$ae481251, %struct.ScmObj** %stackaddr$prim54103, align 8
%stackaddr$prim54104 = alloca %struct.ScmObj*, align 8
%argslist53832$ae481252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48126, %struct.ScmObj* %argslist53832$ae481251)
store volatile %struct.ScmObj* %argslist53832$ae481252, %struct.ScmObj** %stackaddr$prim54104, align 8
%clofunc54105 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48125)
musttail call tailcc void %clofunc54105(%struct.ScmObj* %ae48125, %struct.ScmObj* %argslist53832$ae481252)
ret void
}

define tailcc void @proc_clo$ae48125(%struct.ScmObj* %env$ae48125,%struct.ScmObj* %current_45args53403) {
%stackaddr$env-ref54106 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48125, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54106
%stackaddr$env-ref54107 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48125, i64 1)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54107
%stackaddr$env-ref54108 = alloca %struct.ScmObj*, align 8
%_37length47078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48125, i64 2)
store %struct.ScmObj* %_37length47078, %struct.ScmObj** %stackaddr$env-ref54108
%stackaddr$env-ref54109 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48125, i64 3)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref54109
%stackaddr$env-ref54110 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48125, i64 4)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54110
%stackaddr$env-ref54111 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48125, i64 5)
store %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$env-ref54111
%stackaddr$prim54112 = alloca %struct.ScmObj*, align 8
%_95k47331 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53403)
store volatile %struct.ScmObj* %_95k47331, %struct.ScmObj** %stackaddr$prim54112, align 8
%stackaddr$prim54113 = alloca %struct.ScmObj*, align 8
%current_45args53404 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53403)
store volatile %struct.ScmObj* %current_45args53404, %struct.ScmObj** %stackaddr$prim54113, align 8
%stackaddr$prim54114 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53404)
store volatile %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$prim54114, align 8
%stackaddr$makeclosure54115 = alloca %struct.ScmObj*, align 8
%fptrToInt54116 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48179 to i64
%ae48179 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54116)
store volatile %struct.ScmObj* %ae48179, %struct.ScmObj** %stackaddr$makeclosure54115, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48179, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48179, %struct.ScmObj* %_37foldl147073, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48179, %struct.ScmObj* %_37map147085, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48179, %struct.ScmObj* %Ycmb47068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48179, %struct.ScmObj* %_37last47111, i64 4)
%ae48180 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54117 = alloca %struct.ScmObj*, align 8
%fptrToInt54118 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48181 to i64
%ae48181 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54118)
store volatile %struct.ScmObj* %ae48181, %struct.ScmObj** %stackaddr$makeclosure54117, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48181, %struct.ScmObj* %_37length47078, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48181, %struct.ScmObj* %_37take47081, i64 1)
%argslist53818$ae481790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54119 = alloca %struct.ScmObj*, align 8
%argslist53818$ae481791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48181, %struct.ScmObj* %argslist53818$ae481790)
store volatile %struct.ScmObj* %argslist53818$ae481791, %struct.ScmObj** %stackaddr$prim54119, align 8
%stackaddr$prim54120 = alloca %struct.ScmObj*, align 8
%argslist53818$ae481792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48180, %struct.ScmObj* %argslist53818$ae481791)
store volatile %struct.ScmObj* %argslist53818$ae481792, %struct.ScmObj** %stackaddr$prim54120, align 8
%clofunc54121 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48179)
musttail call tailcc void %clofunc54121(%struct.ScmObj* %ae48179, %struct.ScmObj* %argslist53818$ae481792)
ret void
}

define tailcc void @proc_clo$ae48179(%struct.ScmObj* %env$ae48179,%struct.ScmObj* %current_45args53406) {
%stackaddr$env-ref54122 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48179, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54122
%stackaddr$env-ref54123 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48179, i64 1)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54123
%stackaddr$env-ref54124 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48179, i64 2)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref54124
%stackaddr$env-ref54125 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48179, i64 3)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54125
%stackaddr$env-ref54126 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48179, i64 4)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref54126
%stackaddr$prim54127 = alloca %struct.ScmObj*, align 8
%_95k47332 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53406)
store volatile %struct.ScmObj* %_95k47332, %struct.ScmObj** %stackaddr$prim54127, align 8
%stackaddr$prim54128 = alloca %struct.ScmObj*, align 8
%current_45args53407 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53406)
store volatile %struct.ScmObj* %current_45args53407, %struct.ScmObj** %stackaddr$prim54128, align 8
%stackaddr$prim54129 = alloca %struct.ScmObj*, align 8
%_37drop_45right47108 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53407)
store volatile %struct.ScmObj* %_37drop_45right47108, %struct.ScmObj** %stackaddr$prim54129, align 8
%stackaddr$makeclosure54130 = alloca %struct.ScmObj*, align 8
%fptrToInt54131 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48209 to i64
%ae48209 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54131)
store volatile %struct.ScmObj* %ae48209, %struct.ScmObj** %stackaddr$makeclosure54130, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48209, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48209, %struct.ScmObj* %_37foldl147073, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48209, %struct.ScmObj* %_37drop_45right47108, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48209, %struct.ScmObj* %Ycmb47068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48209, %struct.ScmObj* %_37last47111, i64 4)
%ae48210 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54132 = alloca %struct.ScmObj*, align 8
%fptrToInt54133 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48211 to i64
%ae48211 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54133)
store volatile %struct.ScmObj* %ae48211, %struct.ScmObj** %stackaddr$makeclosure54132, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48211, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48211, %struct.ScmObj* %_37map147085, i64 1)
%argslist53808$ae482090 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54134 = alloca %struct.ScmObj*, align 8
%argslist53808$ae482091 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48211, %struct.ScmObj* %argslist53808$ae482090)
store volatile %struct.ScmObj* %argslist53808$ae482091, %struct.ScmObj** %stackaddr$prim54134, align 8
%stackaddr$prim54135 = alloca %struct.ScmObj*, align 8
%argslist53808$ae482092 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48210, %struct.ScmObj* %argslist53808$ae482091)
store volatile %struct.ScmObj* %argslist53808$ae482092, %struct.ScmObj** %stackaddr$prim54135, align 8
%clofunc54136 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48209)
musttail call tailcc void %clofunc54136(%struct.ScmObj* %ae48209, %struct.ScmObj* %argslist53808$ae482092)
ret void
}

define tailcc void @proc_clo$ae48209(%struct.ScmObj* %env$ae48209,%struct.ScmObj* %current_45args53409) {
%stackaddr$env-ref54137 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48209, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54137
%stackaddr$env-ref54138 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48209, i64 1)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54138
%stackaddr$env-ref54139 = alloca %struct.ScmObj*, align 8
%_37drop_45right47108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48209, i64 2)
store %struct.ScmObj* %_37drop_45right47108, %struct.ScmObj** %stackaddr$env-ref54139
%stackaddr$env-ref54140 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48209, i64 3)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54140
%stackaddr$env-ref54141 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48209, i64 4)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref54141
%stackaddr$prim54142 = alloca %struct.ScmObj*, align 8
%_95k47333 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53409)
store volatile %struct.ScmObj* %_95k47333, %struct.ScmObj** %stackaddr$prim54142, align 8
%stackaddr$prim54143 = alloca %struct.ScmObj*, align 8
%current_45args53410 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53409)
store volatile %struct.ScmObj* %current_45args53410, %struct.ScmObj** %stackaddr$prim54143, align 8
%stackaddr$prim54144 = alloca %struct.ScmObj*, align 8
%anf_45bind47241 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53410)
store volatile %struct.ScmObj* %anf_45bind47241, %struct.ScmObj** %stackaddr$prim54144, align 8
%stackaddr$makeclosure54145 = alloca %struct.ScmObj*, align 8
%fptrToInt54146 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48593 to i64
%ae48593 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54146)
store volatile %struct.ScmObj* %ae48593, %struct.ScmObj** %stackaddr$makeclosure54145, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48593, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48593, %struct.ScmObj* %_37foldl147073, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48593, %struct.ScmObj* %_37drop_45right47108, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48593, %struct.ScmObj* %Ycmb47068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48593, %struct.ScmObj* %_37last47111, i64 4)
%argslist53748$Ycmb470680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54147 = alloca %struct.ScmObj*, align 8
%argslist53748$Ycmb470681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47241, %struct.ScmObj* %argslist53748$Ycmb470680)
store volatile %struct.ScmObj* %argslist53748$Ycmb470681, %struct.ScmObj** %stackaddr$prim54147, align 8
%stackaddr$prim54148 = alloca %struct.ScmObj*, align 8
%argslist53748$Ycmb470682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48593, %struct.ScmObj* %argslist53748$Ycmb470681)
store volatile %struct.ScmObj* %argslist53748$Ycmb470682, %struct.ScmObj** %stackaddr$prim54148, align 8
%clofunc54149 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47068)
musttail call tailcc void %clofunc54149(%struct.ScmObj* %Ycmb47068, %struct.ScmObj* %argslist53748$Ycmb470682)
ret void
}

define tailcc void @proc_clo$ae48593(%struct.ScmObj* %env$ae48593,%struct.ScmObj* %current_45args53412) {
%stackaddr$env-ref54150 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48593, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54150
%stackaddr$env-ref54151 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48593, i64 1)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54151
%stackaddr$env-ref54152 = alloca %struct.ScmObj*, align 8
%_37drop_45right47108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48593, i64 2)
store %struct.ScmObj* %_37drop_45right47108, %struct.ScmObj** %stackaddr$env-ref54152
%stackaddr$env-ref54153 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48593, i64 3)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54153
%stackaddr$env-ref54154 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48593, i64 4)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref54154
%stackaddr$prim54155 = alloca %struct.ScmObj*, align 8
%_95k47334 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53412)
store volatile %struct.ScmObj* %_95k47334, %struct.ScmObj** %stackaddr$prim54155, align 8
%stackaddr$prim54156 = alloca %struct.ScmObj*, align 8
%current_45args53413 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53412)
store volatile %struct.ScmObj* %current_45args53413, %struct.ScmObj** %stackaddr$prim54156, align 8
%stackaddr$prim54157 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53413)
store volatile %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$prim54157, align 8
%stackaddr$makeclosure54158 = alloca %struct.ScmObj*, align 8
%fptrToInt54159 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48595 to i64
%ae48595 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt54159)
store volatile %struct.ScmObj* %ae48595, %struct.ScmObj** %stackaddr$makeclosure54158, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48595, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48595, %struct.ScmObj* %_37foldl147073, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48595, %struct.ScmObj* %_37foldr47094, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48595, %struct.ScmObj* %_37drop_45right47108, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48595, %struct.ScmObj* %Ycmb47068, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48595, %struct.ScmObj* %_37last47111, i64 5)
%ae48596 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54160 = alloca %struct.ScmObj*, align 8
%fptrToInt54161 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48597 to i64
%ae48597 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54161)
store volatile %struct.ScmObj* %ae48597, %struct.ScmObj** %stackaddr$makeclosure54160, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48597, %struct.ScmObj* %_37foldr147089, i64 0)
%argslist53747$ae485950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54162 = alloca %struct.ScmObj*, align 8
%argslist53747$ae485951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48597, %struct.ScmObj* %argslist53747$ae485950)
store volatile %struct.ScmObj* %argslist53747$ae485951, %struct.ScmObj** %stackaddr$prim54162, align 8
%stackaddr$prim54163 = alloca %struct.ScmObj*, align 8
%argslist53747$ae485952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48596, %struct.ScmObj* %argslist53747$ae485951)
store volatile %struct.ScmObj* %argslist53747$ae485952, %struct.ScmObj** %stackaddr$prim54163, align 8
%clofunc54164 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48595)
musttail call tailcc void %clofunc54164(%struct.ScmObj* %ae48595, %struct.ScmObj* %argslist53747$ae485952)
ret void
}

define tailcc void @proc_clo$ae48595(%struct.ScmObj* %env$ae48595,%struct.ScmObj* %current_45args53415) {
%stackaddr$env-ref54165 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48595, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54165
%stackaddr$env-ref54166 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48595, i64 1)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54166
%stackaddr$env-ref54167 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48595, i64 2)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref54167
%stackaddr$env-ref54168 = alloca %struct.ScmObj*, align 8
%_37drop_45right47108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48595, i64 3)
store %struct.ScmObj* %_37drop_45right47108, %struct.ScmObj** %stackaddr$env-ref54168
%stackaddr$env-ref54169 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48595, i64 4)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54169
%stackaddr$env-ref54170 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48595, i64 5)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref54170
%stackaddr$prim54171 = alloca %struct.ScmObj*, align 8
%_95k47335 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53415)
store volatile %struct.ScmObj* %_95k47335, %struct.ScmObj** %stackaddr$prim54171, align 8
%stackaddr$prim54172 = alloca %struct.ScmObj*, align 8
%current_45args53416 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53415)
store volatile %struct.ScmObj* %current_45args53416, %struct.ScmObj** %stackaddr$prim54172, align 8
%stackaddr$prim54173 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53416)
store volatile %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$prim54173, align 8
%stackaddr$makeclosure54174 = alloca %struct.ScmObj*, align 8
%fptrToInt54175 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48672 to i64
%ae48672 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54175)
store volatile %struct.ScmObj* %ae48672, %struct.ScmObj** %stackaddr$makeclosure54174, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48672, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48672, %struct.ScmObj* %_37foldl147073, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48672, %struct.ScmObj* %_37foldr47094, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48672, %struct.ScmObj* %_37map147120, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48672, %struct.ScmObj* %Ycmb47068, i64 4)
%ae48673 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54176 = alloca %struct.ScmObj*, align 8
%fptrToInt54177 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48674 to i64
%ae48674 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54177)
store volatile %struct.ScmObj* %ae48674, %struct.ScmObj** %stackaddr$makeclosure54176, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48674, %struct.ScmObj* %_37foldr47094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48674, %struct.ScmObj* %_37drop_45right47108, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48674, %struct.ScmObj* %_37last47111, i64 2)
%argslist53728$ae486720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54178 = alloca %struct.ScmObj*, align 8
%argslist53728$ae486721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48674, %struct.ScmObj* %argslist53728$ae486720)
store volatile %struct.ScmObj* %argslist53728$ae486721, %struct.ScmObj** %stackaddr$prim54178, align 8
%stackaddr$prim54179 = alloca %struct.ScmObj*, align 8
%argslist53728$ae486722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48673, %struct.ScmObj* %argslist53728$ae486721)
store volatile %struct.ScmObj* %argslist53728$ae486722, %struct.ScmObj** %stackaddr$prim54179, align 8
%clofunc54180 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48672)
musttail call tailcc void %clofunc54180(%struct.ScmObj* %ae48672, %struct.ScmObj* %argslist53728$ae486722)
ret void
}

define tailcc void @proc_clo$ae48672(%struct.ScmObj* %env$ae48672,%struct.ScmObj* %current_45args53418) {
%stackaddr$env-ref54181 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48672, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54181
%stackaddr$env-ref54182 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48672, i64 1)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54182
%stackaddr$env-ref54183 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48672, i64 2)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref54183
%stackaddr$env-ref54184 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48672, i64 3)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54184
%stackaddr$env-ref54185 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48672, i64 4)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54185
%stackaddr$prim54186 = alloca %struct.ScmObj*, align 8
%_95k47336 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53418)
store volatile %struct.ScmObj* %_95k47336, %struct.ScmObj** %stackaddr$prim54186, align 8
%stackaddr$prim54187 = alloca %struct.ScmObj*, align 8
%current_45args53419 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53418)
store volatile %struct.ScmObj* %current_45args53419, %struct.ScmObj** %stackaddr$prim54187, align 8
%stackaddr$prim54188 = alloca %struct.ScmObj*, align 8
%_37map47115 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53419)
store volatile %struct.ScmObj* %_37map47115, %struct.ScmObj** %stackaddr$prim54188, align 8
%stackaddr$makeclosure54189 = alloca %struct.ScmObj*, align 8
%fptrToInt54190 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48818 to i64
%ae48818 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54190)
store volatile %struct.ScmObj* %ae48818, %struct.ScmObj** %stackaddr$makeclosure54189, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48818, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48818, %struct.ScmObj* %_37map147120, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48818, %struct.ScmObj* %Ycmb47068, i64 2)
%ae48819 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54191 = alloca %struct.ScmObj*, align 8
%fptrToInt54192 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48820 to i64
%ae48820 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54192)
store volatile %struct.ScmObj* %ae48820, %struct.ScmObj** %stackaddr$makeclosure54191, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48820, %struct.ScmObj* %_37foldr47094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48820, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48820, %struct.ScmObj* %_37map147120, i64 2)
%argslist53711$ae488180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54193 = alloca %struct.ScmObj*, align 8
%argslist53711$ae488181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48820, %struct.ScmObj* %argslist53711$ae488180)
store volatile %struct.ScmObj* %argslist53711$ae488181, %struct.ScmObj** %stackaddr$prim54193, align 8
%stackaddr$prim54194 = alloca %struct.ScmObj*, align 8
%argslist53711$ae488182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48819, %struct.ScmObj* %argslist53711$ae488181)
store volatile %struct.ScmObj* %argslist53711$ae488182, %struct.ScmObj** %stackaddr$prim54194, align 8
%clofunc54195 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48818)
musttail call tailcc void %clofunc54195(%struct.ScmObj* %ae48818, %struct.ScmObj* %argslist53711$ae488182)
ret void
}

define tailcc void @proc_clo$ae48818(%struct.ScmObj* %env$ae48818,%struct.ScmObj* %current_45args53421) {
%stackaddr$env-ref54196 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48818, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54196
%stackaddr$env-ref54197 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48818, i64 1)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54197
%stackaddr$env-ref54198 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48818, i64 2)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54198
%stackaddr$prim54199 = alloca %struct.ScmObj*, align 8
%_95k47337 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53421)
store volatile %struct.ScmObj* %_95k47337, %struct.ScmObj** %stackaddr$prim54199, align 8
%stackaddr$prim54200 = alloca %struct.ScmObj*, align 8
%current_45args53422 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53421)
store volatile %struct.ScmObj* %current_45args53422, %struct.ScmObj** %stackaddr$prim54200, align 8
%stackaddr$prim54201 = alloca %struct.ScmObj*, align 8
%anf_45bind47261 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53422)
store volatile %struct.ScmObj* %anf_45bind47261, %struct.ScmObj** %stackaddr$prim54201, align 8
%stackaddr$makeclosure54202 = alloca %struct.ScmObj*, align 8
%fptrToInt54203 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49210 to i64
%ae49210 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54203)
store volatile %struct.ScmObj* %ae49210, %struct.ScmObj** %stackaddr$makeclosure54202, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49210, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49210, %struct.ScmObj* %_37map147120, i64 1)
%argslist53651$Ycmb470680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54204 = alloca %struct.ScmObj*, align 8
%argslist53651$Ycmb470681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47261, %struct.ScmObj* %argslist53651$Ycmb470680)
store volatile %struct.ScmObj* %argslist53651$Ycmb470681, %struct.ScmObj** %stackaddr$prim54204, align 8
%stackaddr$prim54205 = alloca %struct.ScmObj*, align 8
%argslist53651$Ycmb470682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49210, %struct.ScmObj* %argslist53651$Ycmb470681)
store volatile %struct.ScmObj* %argslist53651$Ycmb470682, %struct.ScmObj** %stackaddr$prim54205, align 8
%clofunc54206 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47068)
musttail call tailcc void %clofunc54206(%struct.ScmObj* %Ycmb47068, %struct.ScmObj* %argslist53651$Ycmb470682)
ret void
}

define tailcc void @proc_clo$ae49210(%struct.ScmObj* %env$ae49210,%struct.ScmObj* %current_45args53424) {
%stackaddr$env-ref54207 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49210, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54207
%stackaddr$env-ref54208 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49210, i64 1)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54208
%stackaddr$prim54209 = alloca %struct.ScmObj*, align 8
%_95k47338 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53424)
store volatile %struct.ScmObj* %_95k47338, %struct.ScmObj** %stackaddr$prim54209, align 8
%stackaddr$prim54210 = alloca %struct.ScmObj*, align 8
%current_45args53425 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53424)
store volatile %struct.ScmObj* %current_45args53425, %struct.ScmObj** %stackaddr$prim54210, align 8
%stackaddr$prim54211 = alloca %struct.ScmObj*, align 8
%_37foldl47171 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53425)
store volatile %struct.ScmObj* %_37foldl47171, %struct.ScmObj** %stackaddr$prim54211, align 8
%stackaddr$makeclosure54212 = alloca %struct.ScmObj*, align 8
%fptrToInt54213 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49212 to i64
%ae49212 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54213)
store volatile %struct.ScmObj* %ae49212, %struct.ScmObj** %stackaddr$makeclosure54212, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49212, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49212, %struct.ScmObj* %_37map147120, i64 1)
%ae49213 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54214 = alloca %struct.ScmObj*, align 8
%fptrToInt54215 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49214 to i64
%ae49214 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54215)
store volatile %struct.ScmObj* %ae49214, %struct.ScmObj** %stackaddr$makeclosure54214, align 8
%argslist53650$ae492120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54216 = alloca %struct.ScmObj*, align 8
%argslist53650$ae492121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49214, %struct.ScmObj* %argslist53650$ae492120)
store volatile %struct.ScmObj* %argslist53650$ae492121, %struct.ScmObj** %stackaddr$prim54216, align 8
%stackaddr$prim54217 = alloca %struct.ScmObj*, align 8
%argslist53650$ae492122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49213, %struct.ScmObj* %argslist53650$ae492121)
store volatile %struct.ScmObj* %argslist53650$ae492122, %struct.ScmObj** %stackaddr$prim54217, align 8
%clofunc54218 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49212)
musttail call tailcc void %clofunc54218(%struct.ScmObj* %ae49212, %struct.ScmObj* %argslist53650$ae492122)
ret void
}

define tailcc void @proc_clo$ae49212(%struct.ScmObj* %env$ae49212,%struct.ScmObj* %current_45args53427) {
%stackaddr$env-ref54219 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49212, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54219
%stackaddr$env-ref54220 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49212, i64 1)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54220
%stackaddr$prim54221 = alloca %struct.ScmObj*, align 8
%_95k47339 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53427)
store volatile %struct.ScmObj* %_95k47339, %struct.ScmObj** %stackaddr$prim54221, align 8
%stackaddr$prim54222 = alloca %struct.ScmObj*, align 8
%current_45args53428 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53427)
store volatile %struct.ScmObj* %current_45args53428, %struct.ScmObj** %stackaddr$prim54222, align 8
%stackaddr$prim54223 = alloca %struct.ScmObj*, align 8
%_37_6247168 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53428)
store volatile %struct.ScmObj* %_37_6247168, %struct.ScmObj** %stackaddr$prim54223, align 8
%stackaddr$makeclosure54224 = alloca %struct.ScmObj*, align 8
%fptrToInt54225 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49236 to i64
%ae49236 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54225)
store volatile %struct.ScmObj* %ae49236, %struct.ScmObj** %stackaddr$makeclosure54224, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49236, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49236, %struct.ScmObj* %_37map147120, i64 1)
%ae49237 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54226 = alloca %struct.ScmObj*, align 8
%fptrToInt54227 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49238 to i64
%ae49238 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54227)
store volatile %struct.ScmObj* %ae49238, %struct.ScmObj** %stackaddr$makeclosure54226, align 8
%argslist53644$ae492360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54228 = alloca %struct.ScmObj*, align 8
%argslist53644$ae492361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49238, %struct.ScmObj* %argslist53644$ae492360)
store volatile %struct.ScmObj* %argslist53644$ae492361, %struct.ScmObj** %stackaddr$prim54228, align 8
%stackaddr$prim54229 = alloca %struct.ScmObj*, align 8
%argslist53644$ae492362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49237, %struct.ScmObj* %argslist53644$ae492361)
store volatile %struct.ScmObj* %argslist53644$ae492362, %struct.ScmObj** %stackaddr$prim54229, align 8
%clofunc54230 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49236)
musttail call tailcc void %clofunc54230(%struct.ScmObj* %ae49236, %struct.ScmObj* %argslist53644$ae492362)
ret void
}

define tailcc void @proc_clo$ae49236(%struct.ScmObj* %env$ae49236,%struct.ScmObj* %current_45args53430) {
%stackaddr$env-ref54231 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49236, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54231
%stackaddr$env-ref54232 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49236, i64 1)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54232
%stackaddr$prim54233 = alloca %struct.ScmObj*, align 8
%_95k47340 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53430)
store volatile %struct.ScmObj* %_95k47340, %struct.ScmObj** %stackaddr$prim54233, align 8
%stackaddr$prim54234 = alloca %struct.ScmObj*, align 8
%current_45args53431 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53430)
store volatile %struct.ScmObj* %current_45args53431, %struct.ScmObj** %stackaddr$prim54234, align 8
%stackaddr$prim54235 = alloca %struct.ScmObj*, align 8
%_37_62_6147165 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53431)
store volatile %struct.ScmObj* %_37_62_6147165, %struct.ScmObj** %stackaddr$prim54235, align 8
%ae49260 = call %struct.ScmObj* @const_init_int(i64 1)
%ae49261 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54236 = alloca %struct.ScmObj*, align 8
%_37append47161 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49260, %struct.ScmObj* %ae49261)
store volatile %struct.ScmObj* %_37append47161, %struct.ScmObj** %stackaddr$prim54236, align 8
%stackaddr$makeclosure54237 = alloca %struct.ScmObj*, align 8
%fptrToInt54238 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49262 to i64
%ae49262 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54238)
store volatile %struct.ScmObj* %ae49262, %struct.ScmObj** %stackaddr$makeclosure54237, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49262, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49262, %struct.ScmObj* %_37map147120, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49262, %struct.ScmObj* %_37append47161, i64 2)
%ae49263 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54239 = alloca %struct.ScmObj*, align 8
%fptrToInt54240 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49264 to i64
%ae49264 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54240)
store volatile %struct.ScmObj* %ae49264, %struct.ScmObj** %stackaddr$makeclosure54239, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49264, %struct.ScmObj* %_37append47161, i64 0)
%argslist53638$ae492620 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54241 = alloca %struct.ScmObj*, align 8
%argslist53638$ae492621 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49264, %struct.ScmObj* %argslist53638$ae492620)
store volatile %struct.ScmObj* %argslist53638$ae492621, %struct.ScmObj** %stackaddr$prim54241, align 8
%stackaddr$prim54242 = alloca %struct.ScmObj*, align 8
%argslist53638$ae492622 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49263, %struct.ScmObj* %argslist53638$ae492621)
store volatile %struct.ScmObj* %argslist53638$ae492622, %struct.ScmObj** %stackaddr$prim54242, align 8
%clofunc54243 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49262)
musttail call tailcc void %clofunc54243(%struct.ScmObj* %ae49262, %struct.ScmObj* %argslist53638$ae492622)
ret void
}

define tailcc void @proc_clo$ae49262(%struct.ScmObj* %env$ae49262,%struct.ScmObj* %current_45args53433) {
%stackaddr$env-ref54244 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49262, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54244
%stackaddr$env-ref54245 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49262, i64 1)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54245
%stackaddr$env-ref54246 = alloca %struct.ScmObj*, align 8
%_37append47161 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49262, i64 2)
store %struct.ScmObj* %_37append47161, %struct.ScmObj** %stackaddr$env-ref54246
%stackaddr$prim54247 = alloca %struct.ScmObj*, align 8
%_95k47341 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53433)
store volatile %struct.ScmObj* %_95k47341, %struct.ScmObj** %stackaddr$prim54247, align 8
%stackaddr$prim54248 = alloca %struct.ScmObj*, align 8
%current_45args53434 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53433)
store volatile %struct.ScmObj* %current_45args53434, %struct.ScmObj** %stackaddr$prim54248, align 8
%stackaddr$prim54249 = alloca %struct.ScmObj*, align 8
%anf_45bind47269 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53434)
store volatile %struct.ScmObj* %anf_45bind47269, %struct.ScmObj** %stackaddr$prim54249, align 8
%ae49330 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54250 = alloca %struct.ScmObj*, align 8
%_95047162 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append47161, %struct.ScmObj* %ae49330, %struct.ScmObj* %anf_45bind47269)
store volatile %struct.ScmObj* %_95047162, %struct.ScmObj** %stackaddr$prim54250, align 8
%ae49333 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54251 = alloca %struct.ScmObj*, align 8
%_37append47160 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append47161, %struct.ScmObj* %ae49333)
store volatile %struct.ScmObj* %_37append47160, %struct.ScmObj** %stackaddr$prim54251, align 8
%stackaddr$makeclosure54252 = alloca %struct.ScmObj*, align 8
%fptrToInt54253 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49334 to i64
%ae49334 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54253)
store volatile %struct.ScmObj* %ae49334, %struct.ScmObj** %stackaddr$makeclosure54252, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49334, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49334, %struct.ScmObj* %_37map147120, i64 1)
%ae49335 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54254 = alloca %struct.ScmObj*, align 8
%fptrToInt54255 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49336 to i64
%ae49336 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54255)
store volatile %struct.ScmObj* %ae49336, %struct.ScmObj** %stackaddr$makeclosure54254, align 8
%argslist53627$ae493340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54256 = alloca %struct.ScmObj*, align 8
%argslist53627$ae493341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49336, %struct.ScmObj* %argslist53627$ae493340)
store volatile %struct.ScmObj* %argslist53627$ae493341, %struct.ScmObj** %stackaddr$prim54256, align 8
%stackaddr$prim54257 = alloca %struct.ScmObj*, align 8
%argslist53627$ae493342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49335, %struct.ScmObj* %argslist53627$ae493341)
store volatile %struct.ScmObj* %argslist53627$ae493342, %struct.ScmObj** %stackaddr$prim54257, align 8
%clofunc54258 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49334)
musttail call tailcc void %clofunc54258(%struct.ScmObj* %ae49334, %struct.ScmObj* %argslist53627$ae493342)
ret void
}

define tailcc void @proc_clo$ae49334(%struct.ScmObj* %env$ae49334,%struct.ScmObj* %current_45args53436) {
%stackaddr$env-ref54259 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49334, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54259
%stackaddr$env-ref54260 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49334, i64 1)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54260
%stackaddr$prim54261 = alloca %struct.ScmObj*, align 8
%_95k47342 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53436)
store volatile %struct.ScmObj* %_95k47342, %struct.ScmObj** %stackaddr$prim54261, align 8
%stackaddr$prim54262 = alloca %struct.ScmObj*, align 8
%current_45args53437 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53436)
store volatile %struct.ScmObj* %current_45args53437, %struct.ScmObj** %stackaddr$prim54262, align 8
%stackaddr$prim54263 = alloca %struct.ScmObj*, align 8
%_37list_6347153 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53437)
store volatile %struct.ScmObj* %_37list_6347153, %struct.ScmObj** %stackaddr$prim54263, align 8
%stackaddr$makeclosure54264 = alloca %struct.ScmObj*, align 8
%fptrToInt54265 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49750 to i64
%ae49750 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54265)
store volatile %struct.ScmObj* %ae49750, %struct.ScmObj** %stackaddr$makeclosure54264, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49750, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49750, %struct.ScmObj* %_37map147120, i64 1)
%ae49751 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54266 = alloca %struct.ScmObj*, align 8
%fptrToInt54267 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49752 to i64
%ae49752 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54267)
store volatile %struct.ScmObj* %ae49752, %struct.ScmObj** %stackaddr$makeclosure54266, align 8
%argslist53602$ae497500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54268 = alloca %struct.ScmObj*, align 8
%argslist53602$ae497501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49752, %struct.ScmObj* %argslist53602$ae497500)
store volatile %struct.ScmObj* %argslist53602$ae497501, %struct.ScmObj** %stackaddr$prim54268, align 8
%stackaddr$prim54269 = alloca %struct.ScmObj*, align 8
%argslist53602$ae497502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49751, %struct.ScmObj* %argslist53602$ae497501)
store volatile %struct.ScmObj* %argslist53602$ae497502, %struct.ScmObj** %stackaddr$prim54269, align 8
%clofunc54270 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49750)
musttail call tailcc void %clofunc54270(%struct.ScmObj* %ae49750, %struct.ScmObj* %argslist53602$ae497502)
ret void
}

define tailcc void @proc_clo$ae49750(%struct.ScmObj* %env$ae49750,%struct.ScmObj* %current_45args53439) {
%stackaddr$env-ref54271 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49750, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54271
%stackaddr$env-ref54272 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49750, i64 1)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54272
%stackaddr$prim54273 = alloca %struct.ScmObj*, align 8
%_95k47343 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53439)
store volatile %struct.ScmObj* %_95k47343, %struct.ScmObj** %stackaddr$prim54273, align 8
%stackaddr$prim54274 = alloca %struct.ScmObj*, align 8
%current_45args53440 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53439)
store volatile %struct.ScmObj* %current_45args53440, %struct.ScmObj** %stackaddr$prim54274, align 8
%stackaddr$prim54275 = alloca %struct.ScmObj*, align 8
%_37drop47144 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53440)
store volatile %struct.ScmObj* %_37drop47144, %struct.ScmObj** %stackaddr$prim54275, align 8
%stackaddr$makeclosure54276 = alloca %struct.ScmObj*, align 8
%fptrToInt54277 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50286 to i64
%ae50286 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54277)
store volatile %struct.ScmObj* %ae50286, %struct.ScmObj** %stackaddr$makeclosure54276, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50286, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50286, %struct.ScmObj* %_37map147120, i64 1)
%ae50287 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54278 = alloca %struct.ScmObj*, align 8
%fptrToInt54279 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50288 to i64
%ae50288 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54279)
store volatile %struct.ScmObj* %ae50288, %struct.ScmObj** %stackaddr$makeclosure54278, align 8
%argslist53578$ae502860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54280 = alloca %struct.ScmObj*, align 8
%argslist53578$ae502861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50288, %struct.ScmObj* %argslist53578$ae502860)
store volatile %struct.ScmObj* %argslist53578$ae502861, %struct.ScmObj** %stackaddr$prim54280, align 8
%stackaddr$prim54281 = alloca %struct.ScmObj*, align 8
%argslist53578$ae502862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50287, %struct.ScmObj* %argslist53578$ae502861)
store volatile %struct.ScmObj* %argslist53578$ae502862, %struct.ScmObj** %stackaddr$prim54281, align 8
%clofunc54282 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50286)
musttail call tailcc void %clofunc54282(%struct.ScmObj* %ae50286, %struct.ScmObj* %argslist53578$ae502862)
ret void
}

define tailcc void @proc_clo$ae50286(%struct.ScmObj* %env$ae50286,%struct.ScmObj* %current_45args53442) {
%stackaddr$env-ref54283 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50286, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54283
%stackaddr$env-ref54284 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50286, i64 1)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54284
%stackaddr$prim54285 = alloca %struct.ScmObj*, align 8
%_95k47344 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53442)
store volatile %struct.ScmObj* %_95k47344, %struct.ScmObj** %stackaddr$prim54285, align 8
%stackaddr$prim54286 = alloca %struct.ScmObj*, align 8
%current_45args53443 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53442)
store volatile %struct.ScmObj* %current_45args53443, %struct.ScmObj** %stackaddr$prim54286, align 8
%stackaddr$prim54287 = alloca %struct.ScmObj*, align 8
%_37memv47137 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53443)
store volatile %struct.ScmObj* %_37memv47137, %struct.ScmObj** %stackaddr$prim54287, align 8
%stackaddr$makeclosure54288 = alloca %struct.ScmObj*, align 8
%fptrToInt54289 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50690 to i64
%ae50690 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54289)
store volatile %struct.ScmObj* %ae50690, %struct.ScmObj** %stackaddr$makeclosure54288, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50690, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50690, %struct.ScmObj* %_37map147120, i64 1)
%ae50691 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54290 = alloca %struct.ScmObj*, align 8
%fptrToInt54291 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50692 to i64
%ae50692 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54291)
store volatile %struct.ScmObj* %ae50692, %struct.ScmObj** %stackaddr$makeclosure54290, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50692, %struct.ScmObj* %_37foldl147073, i64 0)
%argslist53552$ae506900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54292 = alloca %struct.ScmObj*, align 8
%argslist53552$ae506901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50692, %struct.ScmObj* %argslist53552$ae506900)
store volatile %struct.ScmObj* %argslist53552$ae506901, %struct.ScmObj** %stackaddr$prim54292, align 8
%stackaddr$prim54293 = alloca %struct.ScmObj*, align 8
%argslist53552$ae506902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50691, %struct.ScmObj* %argslist53552$ae506901)
store volatile %struct.ScmObj* %argslist53552$ae506902, %struct.ScmObj** %stackaddr$prim54293, align 8
%clofunc54294 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50690)
musttail call tailcc void %clofunc54294(%struct.ScmObj* %ae50690, %struct.ScmObj* %argslist53552$ae506902)
ret void
}

define tailcc void @proc_clo$ae50690(%struct.ScmObj* %env$ae50690,%struct.ScmObj* %current_45args53445) {
%stackaddr$env-ref54295 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50690, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54295
%stackaddr$env-ref54296 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50690, i64 1)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54296
%stackaddr$prim54297 = alloca %struct.ScmObj*, align 8
%_95k47345 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53445)
store volatile %struct.ScmObj* %_95k47345, %struct.ScmObj** %stackaddr$prim54297, align 8
%stackaddr$prim54298 = alloca %struct.ScmObj*, align 8
%current_45args53446 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53445)
store volatile %struct.ScmObj* %current_45args53446, %struct.ScmObj** %stackaddr$prim54298, align 8
%stackaddr$prim54299 = alloca %struct.ScmObj*, align 8
%_37_4747133 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53446)
store volatile %struct.ScmObj* %_37_4747133, %struct.ScmObj** %stackaddr$prim54299, align 8
%stackaddr$makeclosure54300 = alloca %struct.ScmObj*, align 8
%fptrToInt54301 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50788 to i64
%ae50788 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54301)
store volatile %struct.ScmObj* %ae50788, %struct.ScmObj** %stackaddr$makeclosure54300, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50788, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50788, %struct.ScmObj* %_37map147120, i64 1)
%ae50789 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54302 = alloca %struct.ScmObj*, align 8
%fptrToInt54303 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50790 to i64
%ae50790 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54303)
store volatile %struct.ScmObj* %ae50790, %struct.ScmObj** %stackaddr$makeclosure54302, align 8
%argslist53539$ae507880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54304 = alloca %struct.ScmObj*, align 8
%argslist53539$ae507881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50790, %struct.ScmObj* %argslist53539$ae507880)
store volatile %struct.ScmObj* %argslist53539$ae507881, %struct.ScmObj** %stackaddr$prim54304, align 8
%stackaddr$prim54305 = alloca %struct.ScmObj*, align 8
%argslist53539$ae507882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50789, %struct.ScmObj* %argslist53539$ae507881)
store volatile %struct.ScmObj* %argslist53539$ae507882, %struct.ScmObj** %stackaddr$prim54305, align 8
%clofunc54306 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50788)
musttail call tailcc void %clofunc54306(%struct.ScmObj* %ae50788, %struct.ScmObj* %argslist53539$ae507882)
ret void
}

define tailcc void @proc_clo$ae50788(%struct.ScmObj* %env$ae50788,%struct.ScmObj* %current_45args53448) {
%stackaddr$env-ref54307 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50788, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54307
%stackaddr$env-ref54308 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50788, i64 1)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54308
%stackaddr$prim54309 = alloca %struct.ScmObj*, align 8
%_95k47346 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53448)
store volatile %struct.ScmObj* %_95k47346, %struct.ScmObj** %stackaddr$prim54309, align 8
%stackaddr$prim54310 = alloca %struct.ScmObj*, align 8
%current_45args53449 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53448)
store volatile %struct.ScmObj* %current_45args53449, %struct.ScmObj** %stackaddr$prim54310, align 8
%stackaddr$prim54311 = alloca %struct.ScmObj*, align 8
%_37first47131 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53449)
store volatile %struct.ScmObj* %_37first47131, %struct.ScmObj** %stackaddr$prim54311, align 8
%stackaddr$makeclosure54312 = alloca %struct.ScmObj*, align 8
%fptrToInt54313 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50808 to i64
%ae50808 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54313)
store volatile %struct.ScmObj* %ae50808, %struct.ScmObj** %stackaddr$makeclosure54312, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50808, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50808, %struct.ScmObj* %_37map147120, i64 1)
%ae50809 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54314 = alloca %struct.ScmObj*, align 8
%fptrToInt54315 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50810 to i64
%ae50810 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54315)
store volatile %struct.ScmObj* %ae50810, %struct.ScmObj** %stackaddr$makeclosure54314, align 8
%argslist53534$ae508080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54316 = alloca %struct.ScmObj*, align 8
%argslist53534$ae508081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50810, %struct.ScmObj* %argslist53534$ae508080)
store volatile %struct.ScmObj* %argslist53534$ae508081, %struct.ScmObj** %stackaddr$prim54316, align 8
%stackaddr$prim54317 = alloca %struct.ScmObj*, align 8
%argslist53534$ae508082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50809, %struct.ScmObj* %argslist53534$ae508081)
store volatile %struct.ScmObj* %argslist53534$ae508082, %struct.ScmObj** %stackaddr$prim54317, align 8
%clofunc54318 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50808)
musttail call tailcc void %clofunc54318(%struct.ScmObj* %ae50808, %struct.ScmObj* %argslist53534$ae508082)
ret void
}

define tailcc void @proc_clo$ae50808(%struct.ScmObj* %env$ae50808,%struct.ScmObj* %current_45args53451) {
%stackaddr$env-ref54319 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50808, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54319
%stackaddr$env-ref54320 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50808, i64 1)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54320
%stackaddr$prim54321 = alloca %struct.ScmObj*, align 8
%_95k47347 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53451)
store volatile %struct.ScmObj* %_95k47347, %struct.ScmObj** %stackaddr$prim54321, align 8
%stackaddr$prim54322 = alloca %struct.ScmObj*, align 8
%current_45args53452 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53451)
store volatile %struct.ScmObj* %current_45args53452, %struct.ScmObj** %stackaddr$prim54322, align 8
%stackaddr$prim54323 = alloca %struct.ScmObj*, align 8
%_37second47129 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53452)
store volatile %struct.ScmObj* %_37second47129, %struct.ScmObj** %stackaddr$prim54323, align 8
%stackaddr$makeclosure54324 = alloca %struct.ScmObj*, align 8
%fptrToInt54325 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50830 to i64
%ae50830 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54325)
store volatile %struct.ScmObj* %ae50830, %struct.ScmObj** %stackaddr$makeclosure54324, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50830, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50830, %struct.ScmObj* %_37map147120, i64 1)
%ae50831 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54326 = alloca %struct.ScmObj*, align 8
%fptrToInt54327 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50832 to i64
%ae50832 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54327)
store volatile %struct.ScmObj* %ae50832, %struct.ScmObj** %stackaddr$makeclosure54326, align 8
%argslist53529$ae508300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54328 = alloca %struct.ScmObj*, align 8
%argslist53529$ae508301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50832, %struct.ScmObj* %argslist53529$ae508300)
store volatile %struct.ScmObj* %argslist53529$ae508301, %struct.ScmObj** %stackaddr$prim54328, align 8
%stackaddr$prim54329 = alloca %struct.ScmObj*, align 8
%argslist53529$ae508302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50831, %struct.ScmObj* %argslist53529$ae508301)
store volatile %struct.ScmObj* %argslist53529$ae508302, %struct.ScmObj** %stackaddr$prim54329, align 8
%clofunc54330 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50830)
musttail call tailcc void %clofunc54330(%struct.ScmObj* %ae50830, %struct.ScmObj* %argslist53529$ae508302)
ret void
}

define tailcc void @proc_clo$ae50830(%struct.ScmObj* %env$ae50830,%struct.ScmObj* %current_45args53454) {
%stackaddr$env-ref54331 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50830, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54331
%stackaddr$env-ref54332 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50830, i64 1)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54332
%stackaddr$prim54333 = alloca %struct.ScmObj*, align 8
%_95k47348 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53454)
store volatile %struct.ScmObj* %_95k47348, %struct.ScmObj** %stackaddr$prim54333, align 8
%stackaddr$prim54334 = alloca %struct.ScmObj*, align 8
%current_45args53455 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53454)
store volatile %struct.ScmObj* %current_45args53455, %struct.ScmObj** %stackaddr$prim54334, align 8
%stackaddr$prim54335 = alloca %struct.ScmObj*, align 8
%_37third47127 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53455)
store volatile %struct.ScmObj* %_37third47127, %struct.ScmObj** %stackaddr$prim54335, align 8
%stackaddr$makeclosure54336 = alloca %struct.ScmObj*, align 8
%fptrToInt54337 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50854 to i64
%ae50854 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54337)
store volatile %struct.ScmObj* %ae50854, %struct.ScmObj** %stackaddr$makeclosure54336, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50854, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50854, %struct.ScmObj* %_37map147120, i64 1)
%ae50855 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54338 = alloca %struct.ScmObj*, align 8
%fptrToInt54339 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50856 to i64
%ae50856 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54339)
store volatile %struct.ScmObj* %ae50856, %struct.ScmObj** %stackaddr$makeclosure54338, align 8
%argslist53524$ae508540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54340 = alloca %struct.ScmObj*, align 8
%argslist53524$ae508541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50856, %struct.ScmObj* %argslist53524$ae508540)
store volatile %struct.ScmObj* %argslist53524$ae508541, %struct.ScmObj** %stackaddr$prim54340, align 8
%stackaddr$prim54341 = alloca %struct.ScmObj*, align 8
%argslist53524$ae508542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50855, %struct.ScmObj* %argslist53524$ae508541)
store volatile %struct.ScmObj* %argslist53524$ae508542, %struct.ScmObj** %stackaddr$prim54341, align 8
%clofunc54342 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50854)
musttail call tailcc void %clofunc54342(%struct.ScmObj* %ae50854, %struct.ScmObj* %argslist53524$ae508542)
ret void
}

define tailcc void @proc_clo$ae50854(%struct.ScmObj* %env$ae50854,%struct.ScmObj* %current_45args53457) {
%stackaddr$env-ref54343 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50854, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54343
%stackaddr$env-ref54344 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50854, i64 1)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54344
%stackaddr$prim54345 = alloca %struct.ScmObj*, align 8
%_95k47349 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53457)
store volatile %struct.ScmObj* %_95k47349, %struct.ScmObj** %stackaddr$prim54345, align 8
%stackaddr$prim54346 = alloca %struct.ScmObj*, align 8
%current_45args53458 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53457)
store volatile %struct.ScmObj* %current_45args53458, %struct.ScmObj** %stackaddr$prim54346, align 8
%stackaddr$prim54347 = alloca %struct.ScmObj*, align 8
%_37fourth47125 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53458)
store volatile %struct.ScmObj* %_37fourth47125, %struct.ScmObj** %stackaddr$prim54347, align 8
%stackaddr$makeclosure54348 = alloca %struct.ScmObj*, align 8
%fptrToInt54349 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50880 to i64
%ae50880 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54349)
store volatile %struct.ScmObj* %ae50880, %struct.ScmObj** %stackaddr$makeclosure54348, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50880, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50880, %struct.ScmObj* %_37map147120, i64 1)
%ae50881 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54350 = alloca %struct.ScmObj*, align 8
%fptrToInt54351 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50882 to i64
%ae50882 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54351)
store volatile %struct.ScmObj* %ae50882, %struct.ScmObj** %stackaddr$makeclosure54350, align 8
%argslist53519$ae508800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54352 = alloca %struct.ScmObj*, align 8
%argslist53519$ae508801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50882, %struct.ScmObj* %argslist53519$ae508800)
store volatile %struct.ScmObj* %argslist53519$ae508801, %struct.ScmObj** %stackaddr$prim54352, align 8
%stackaddr$prim54353 = alloca %struct.ScmObj*, align 8
%argslist53519$ae508802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50881, %struct.ScmObj* %argslist53519$ae508801)
store volatile %struct.ScmObj* %argslist53519$ae508802, %struct.ScmObj** %stackaddr$prim54353, align 8
%clofunc54354 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50880)
musttail call tailcc void %clofunc54354(%struct.ScmObj* %ae50880, %struct.ScmObj* %argslist53519$ae508802)
ret void
}

define tailcc void @proc_clo$ae50880(%struct.ScmObj* %env$ae50880,%struct.ScmObj* %current_45args53460) {
%stackaddr$env-ref54355 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50880, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54355
%stackaddr$env-ref54356 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50880, i64 1)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54356
%stackaddr$prim54357 = alloca %struct.ScmObj*, align 8
%_95k47350 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53460)
store volatile %struct.ScmObj* %_95k47350, %struct.ScmObj** %stackaddr$prim54357, align 8
%stackaddr$prim54358 = alloca %struct.ScmObj*, align 8
%current_45args53461 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53460)
store volatile %struct.ScmObj* %current_45args53461, %struct.ScmObj** %stackaddr$prim54358, align 8
%stackaddr$prim54359 = alloca %struct.ScmObj*, align 8
%anf_45bind47305 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53461)
store volatile %struct.ScmObj* %anf_45bind47305, %struct.ScmObj** %stackaddr$prim54359, align 8
%stackaddr$makeclosure54360 = alloca %struct.ScmObj*, align 8
%fptrToInt54361 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50904 to i64
%ae50904 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54361)
store volatile %struct.ScmObj* %ae50904, %struct.ScmObj** %stackaddr$makeclosure54360, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50904, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50904, %struct.ScmObj* %_37map147120, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50904, %struct.ScmObj* %anf_45bind47305, i64 2)
%ae50905 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54362 = alloca %struct.ScmObj*, align 8
%fptrToInt54363 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50906 to i64
%ae50906 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54363)
store volatile %struct.ScmObj* %ae50906, %struct.ScmObj** %stackaddr$makeclosure54362, align 8
%argslist53517$ae509040 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54364 = alloca %struct.ScmObj*, align 8
%argslist53517$ae509041 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50906, %struct.ScmObj* %argslist53517$ae509040)
store volatile %struct.ScmObj* %argslist53517$ae509041, %struct.ScmObj** %stackaddr$prim54364, align 8
%stackaddr$prim54365 = alloca %struct.ScmObj*, align 8
%argslist53517$ae509042 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50905, %struct.ScmObj* %argslist53517$ae509041)
store volatile %struct.ScmObj* %argslist53517$ae509042, %struct.ScmObj** %stackaddr$prim54365, align 8
%clofunc54366 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50904)
musttail call tailcc void %clofunc54366(%struct.ScmObj* %ae50904, %struct.ScmObj* %argslist53517$ae509042)
ret void
}

define tailcc void @proc_clo$ae50904(%struct.ScmObj* %env$ae50904,%struct.ScmObj* %current_45args53463) {
%stackaddr$env-ref54367 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50904, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54367
%stackaddr$env-ref54368 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50904, i64 1)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54368
%stackaddr$env-ref54369 = alloca %struct.ScmObj*, align 8
%anf_45bind47305 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50904, i64 2)
store %struct.ScmObj* %anf_45bind47305, %struct.ScmObj** %stackaddr$env-ref54369
%stackaddr$prim54370 = alloca %struct.ScmObj*, align 8
%_95k47351 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53463)
store volatile %struct.ScmObj* %_95k47351, %struct.ScmObj** %stackaddr$prim54370, align 8
%stackaddr$prim54371 = alloca %struct.ScmObj*, align 8
%current_45args53464 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53463)
store volatile %struct.ScmObj* %current_45args53464, %struct.ScmObj** %stackaddr$prim54371, align 8
%stackaddr$prim54372 = alloca %struct.ScmObj*, align 8
%anf_45bind47306 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53464)
store volatile %struct.ScmObj* %anf_45bind47306, %struct.ScmObj** %stackaddr$prim54372, align 8
%stackaddr$makeclosure54373 = alloca %struct.ScmObj*, align 8
%fptrToInt54374 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50927 to i64
%ae50927 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54374)
store volatile %struct.ScmObj* %ae50927, %struct.ScmObj** %stackaddr$makeclosure54373, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50927, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50927, %struct.ScmObj* %_37map147120, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50927, %struct.ScmObj* %anf_45bind47305, i64 2)
%ae50928 = call %struct.ScmObj* @const_init_int(i64 1)
%ae50929 = call %struct.ScmObj* @const_init_int(i64 2)
%ae50930 = call %struct.ScmObj* @const_init_int(i64 3)
%ae50931 = call %struct.ScmObj* @const_init_int(i64 4)
%argslist53515$anf_45bind473060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54375 = alloca %struct.ScmObj*, align 8
%argslist53515$anf_45bind473061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50931, %struct.ScmObj* %argslist53515$anf_45bind473060)
store volatile %struct.ScmObj* %argslist53515$anf_45bind473061, %struct.ScmObj** %stackaddr$prim54375, align 8
%stackaddr$prim54376 = alloca %struct.ScmObj*, align 8
%argslist53515$anf_45bind473062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50930, %struct.ScmObj* %argslist53515$anf_45bind473061)
store volatile %struct.ScmObj* %argslist53515$anf_45bind473062, %struct.ScmObj** %stackaddr$prim54376, align 8
%stackaddr$prim54377 = alloca %struct.ScmObj*, align 8
%argslist53515$anf_45bind473063 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50929, %struct.ScmObj* %argslist53515$anf_45bind473062)
store volatile %struct.ScmObj* %argslist53515$anf_45bind473063, %struct.ScmObj** %stackaddr$prim54377, align 8
%stackaddr$prim54378 = alloca %struct.ScmObj*, align 8
%argslist53515$anf_45bind473064 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50928, %struct.ScmObj* %argslist53515$anf_45bind473063)
store volatile %struct.ScmObj* %argslist53515$anf_45bind473064, %struct.ScmObj** %stackaddr$prim54378, align 8
%stackaddr$prim54379 = alloca %struct.ScmObj*, align 8
%argslist53515$anf_45bind473065 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50927, %struct.ScmObj* %argslist53515$anf_45bind473064)
store volatile %struct.ScmObj* %argslist53515$anf_45bind473065, %struct.ScmObj** %stackaddr$prim54379, align 8
%clofunc54380 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47306)
musttail call tailcc void %clofunc54380(%struct.ScmObj* %anf_45bind47306, %struct.ScmObj* %argslist53515$anf_45bind473065)
ret void
}

define tailcc void @proc_clo$ae50927(%struct.ScmObj* %env$ae50927,%struct.ScmObj* %current_45args53466) {
%stackaddr$env-ref54381 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50927, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54381
%stackaddr$env-ref54382 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50927, i64 1)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54382
%stackaddr$env-ref54383 = alloca %struct.ScmObj*, align 8
%anf_45bind47305 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50927, i64 2)
store %struct.ScmObj* %anf_45bind47305, %struct.ScmObj** %stackaddr$env-ref54383
%stackaddr$prim54384 = alloca %struct.ScmObj*, align 8
%_95k47352 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53466)
store volatile %struct.ScmObj* %_95k47352, %struct.ScmObj** %stackaddr$prim54384, align 8
%stackaddr$prim54385 = alloca %struct.ScmObj*, align 8
%current_45args53467 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53466)
store volatile %struct.ScmObj* %current_45args53467, %struct.ScmObj** %stackaddr$prim54385, align 8
%stackaddr$prim54386 = alloca %struct.ScmObj*, align 8
%anf_45bind47307 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53467)
store volatile %struct.ScmObj* %anf_45bind47307, %struct.ScmObj** %stackaddr$prim54386, align 8
%stackaddr$makeclosure54387 = alloca %struct.ScmObj*, align 8
%fptrToInt54388 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50957 to i64
%ae50957 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54388)
store volatile %struct.ScmObj* %ae50957, %struct.ScmObj** %stackaddr$makeclosure54387, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50957, %struct.ScmObj* %_37map147120, i64 0)
%ae50959 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53514$_37foldl1470730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54389 = alloca %struct.ScmObj*, align 8
%argslist53514$_37foldl1470731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47307, %struct.ScmObj* %argslist53514$_37foldl1470730)
store volatile %struct.ScmObj* %argslist53514$_37foldl1470731, %struct.ScmObj** %stackaddr$prim54389, align 8
%stackaddr$prim54390 = alloca %struct.ScmObj*, align 8
%argslist53514$_37foldl1470732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50959, %struct.ScmObj* %argslist53514$_37foldl1470731)
store volatile %struct.ScmObj* %argslist53514$_37foldl1470732, %struct.ScmObj** %stackaddr$prim54390, align 8
%stackaddr$prim54391 = alloca %struct.ScmObj*, align 8
%argslist53514$_37foldl1470733 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47305, %struct.ScmObj* %argslist53514$_37foldl1470732)
store volatile %struct.ScmObj* %argslist53514$_37foldl1470733, %struct.ScmObj** %stackaddr$prim54391, align 8
%stackaddr$prim54392 = alloca %struct.ScmObj*, align 8
%argslist53514$_37foldl1470734 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50957, %struct.ScmObj* %argslist53514$_37foldl1470733)
store volatile %struct.ScmObj* %argslist53514$_37foldl1470734, %struct.ScmObj** %stackaddr$prim54392, align 8
%clofunc54393 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147073)
musttail call tailcc void %clofunc54393(%struct.ScmObj* %_37foldl147073, %struct.ScmObj* %argslist53514$_37foldl1470734)
ret void
}

define tailcc void @proc_clo$ae50957(%struct.ScmObj* %env$ae50957,%struct.ScmObj* %current_45args53469) {
%stackaddr$env-ref54394 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50957, i64 0)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54394
%stackaddr$prim54395 = alloca %struct.ScmObj*, align 8
%_95k47353 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53469)
store volatile %struct.ScmObj* %_95k47353, %struct.ScmObj** %stackaddr$prim54395, align 8
%stackaddr$prim54396 = alloca %struct.ScmObj*, align 8
%current_45args53470 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53469)
store volatile %struct.ScmObj* %current_45args53470, %struct.ScmObj** %stackaddr$prim54396, align 8
%stackaddr$prim54397 = alloca %struct.ScmObj*, align 8
%anf_45bind47308 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53470)
store volatile %struct.ScmObj* %anf_45bind47308, %struct.ScmObj** %stackaddr$prim54397, align 8
%stackaddr$makeclosure54398 = alloca %struct.ScmObj*, align 8
%fptrToInt54399 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50966 to i64
%ae50966 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54399)
store volatile %struct.ScmObj* %ae50966, %struct.ScmObj** %stackaddr$makeclosure54398, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50966, %struct.ScmObj* %_37map147120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50966, %struct.ScmObj* %anf_45bind47308, i64 1)
%ae50967 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54400 = alloca %struct.ScmObj*, align 8
%fptrToInt54401 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50968 to i64
%ae50968 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54401)
store volatile %struct.ScmObj* %ae50968, %struct.ScmObj** %stackaddr$makeclosure54400, align 8
%argslist53513$ae509660 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54402 = alloca %struct.ScmObj*, align 8
%argslist53513$ae509661 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50968, %struct.ScmObj* %argslist53513$ae509660)
store volatile %struct.ScmObj* %argslist53513$ae509661, %struct.ScmObj** %stackaddr$prim54402, align 8
%stackaddr$prim54403 = alloca %struct.ScmObj*, align 8
%argslist53513$ae509662 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50967, %struct.ScmObj* %argslist53513$ae509661)
store volatile %struct.ScmObj* %argslist53513$ae509662, %struct.ScmObj** %stackaddr$prim54403, align 8
%clofunc54404 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50966)
musttail call tailcc void %clofunc54404(%struct.ScmObj* %ae50966, %struct.ScmObj* %argslist53513$ae509662)
ret void
}

define tailcc void @proc_clo$ae50966(%struct.ScmObj* %env$ae50966,%struct.ScmObj* %current_45args53472) {
%stackaddr$env-ref54405 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50966, i64 0)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54405
%stackaddr$env-ref54406 = alloca %struct.ScmObj*, align 8
%anf_45bind47308 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50966, i64 1)
store %struct.ScmObj* %anf_45bind47308, %struct.ScmObj** %stackaddr$env-ref54406
%stackaddr$prim54407 = alloca %struct.ScmObj*, align 8
%_95k47354 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53472)
store volatile %struct.ScmObj* %_95k47354, %struct.ScmObj** %stackaddr$prim54407, align 8
%stackaddr$prim54408 = alloca %struct.ScmObj*, align 8
%current_45args53473 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53472)
store volatile %struct.ScmObj* %current_45args53473, %struct.ScmObj** %stackaddr$prim54408, align 8
%stackaddr$prim54409 = alloca %struct.ScmObj*, align 8
%anf_45bind47309 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53473)
store volatile %struct.ScmObj* %anf_45bind47309, %struct.ScmObj** %stackaddr$prim54409, align 8
%stackaddr$makeclosure54410 = alloca %struct.ScmObj*, align 8
%fptrToInt54411 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50990 to i64
%ae50990 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54411)
store volatile %struct.ScmObj* %ae50990, %struct.ScmObj** %stackaddr$makeclosure54410, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50990, %struct.ScmObj* %_37map147120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50990, %struct.ScmObj* %anf_45bind47309, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50990, %struct.ScmObj* %anf_45bind47308, i64 2)
%ae50991 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54412 = alloca %struct.ScmObj*, align 8
%fptrToInt54413 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50992 to i64
%ae50992 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54413)
store volatile %struct.ScmObj* %ae50992, %struct.ScmObj** %stackaddr$makeclosure54412, align 8
%argslist53511$ae509900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54414 = alloca %struct.ScmObj*, align 8
%argslist53511$ae509901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50992, %struct.ScmObj* %argslist53511$ae509900)
store volatile %struct.ScmObj* %argslist53511$ae509901, %struct.ScmObj** %stackaddr$prim54414, align 8
%stackaddr$prim54415 = alloca %struct.ScmObj*, align 8
%argslist53511$ae509902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50991, %struct.ScmObj* %argslist53511$ae509901)
store volatile %struct.ScmObj* %argslist53511$ae509902, %struct.ScmObj** %stackaddr$prim54415, align 8
%clofunc54416 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50990)
musttail call tailcc void %clofunc54416(%struct.ScmObj* %ae50990, %struct.ScmObj* %argslist53511$ae509902)
ret void
}

define tailcc void @proc_clo$ae50990(%struct.ScmObj* %env$ae50990,%struct.ScmObj* %current_45args53475) {
%stackaddr$env-ref54417 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50990, i64 0)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54417
%stackaddr$env-ref54418 = alloca %struct.ScmObj*, align 8
%anf_45bind47309 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50990, i64 1)
store %struct.ScmObj* %anf_45bind47309, %struct.ScmObj** %stackaddr$env-ref54418
%stackaddr$env-ref54419 = alloca %struct.ScmObj*, align 8
%anf_45bind47308 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50990, i64 2)
store %struct.ScmObj* %anf_45bind47308, %struct.ScmObj** %stackaddr$env-ref54419
%stackaddr$prim54420 = alloca %struct.ScmObj*, align 8
%_95k47355 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53475)
store volatile %struct.ScmObj* %_95k47355, %struct.ScmObj** %stackaddr$prim54420, align 8
%stackaddr$prim54421 = alloca %struct.ScmObj*, align 8
%current_45args53476 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53475)
store volatile %struct.ScmObj* %current_45args53476, %struct.ScmObj** %stackaddr$prim54421, align 8
%stackaddr$prim54422 = alloca %struct.ScmObj*, align 8
%anf_45bind47310 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53476)
store volatile %struct.ScmObj* %anf_45bind47310, %struct.ScmObj** %stackaddr$prim54422, align 8
%stackaddr$makeclosure54423 = alloca %struct.ScmObj*, align 8
%fptrToInt54424 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51013 to i64
%ae51013 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54424)
store volatile %struct.ScmObj* %ae51013, %struct.ScmObj** %stackaddr$makeclosure54423, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51013, %struct.ScmObj* %_37map147120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51013, %struct.ScmObj* %anf_45bind47309, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51013, %struct.ScmObj* %anf_45bind47308, i64 2)
%ae51014 = call %struct.ScmObj* @const_init_int(i64 2)
%argslist53509$anf_45bind473100 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54425 = alloca %struct.ScmObj*, align 8
%argslist53509$anf_45bind473101 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51014, %struct.ScmObj* %argslist53509$anf_45bind473100)
store volatile %struct.ScmObj* %argslist53509$anf_45bind473101, %struct.ScmObj** %stackaddr$prim54425, align 8
%stackaddr$prim54426 = alloca %struct.ScmObj*, align 8
%argslist53509$anf_45bind473102 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51013, %struct.ScmObj* %argslist53509$anf_45bind473101)
store volatile %struct.ScmObj* %argslist53509$anf_45bind473102, %struct.ScmObj** %stackaddr$prim54426, align 8
%clofunc54427 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47310)
musttail call tailcc void %clofunc54427(%struct.ScmObj* %anf_45bind47310, %struct.ScmObj* %argslist53509$anf_45bind473102)
ret void
}

define tailcc void @proc_clo$ae51013(%struct.ScmObj* %env$ae51013,%struct.ScmObj* %current_45args53478) {
%stackaddr$env-ref54428 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51013, i64 0)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54428
%stackaddr$env-ref54429 = alloca %struct.ScmObj*, align 8
%anf_45bind47309 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51013, i64 1)
store %struct.ScmObj* %anf_45bind47309, %struct.ScmObj** %stackaddr$env-ref54429
%stackaddr$env-ref54430 = alloca %struct.ScmObj*, align 8
%anf_45bind47308 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51013, i64 2)
store %struct.ScmObj* %anf_45bind47308, %struct.ScmObj** %stackaddr$env-ref54430
%stackaddr$prim54431 = alloca %struct.ScmObj*, align 8
%_95k47356 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53478)
store volatile %struct.ScmObj* %_95k47356, %struct.ScmObj** %stackaddr$prim54431, align 8
%stackaddr$prim54432 = alloca %struct.ScmObj*, align 8
%current_45args53479 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53478)
store volatile %struct.ScmObj* %current_45args53479, %struct.ScmObj** %stackaddr$prim54432, align 8
%stackaddr$prim54433 = alloca %struct.ScmObj*, align 8
%anf_45bind47311 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53479)
store volatile %struct.ScmObj* %anf_45bind47311, %struct.ScmObj** %stackaddr$prim54433, align 8
%stackaddr$makeclosure54434 = alloca %struct.ScmObj*, align 8
%fptrToInt54435 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51019 to i64
%ae51019 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54435)
store volatile %struct.ScmObj* %ae51019, %struct.ScmObj** %stackaddr$makeclosure54434, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51019, %struct.ScmObj* %anf_45bind47308, i64 0)
%argslist53508$_37map1471200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54436 = alloca %struct.ScmObj*, align 8
%argslist53508$_37map1471201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47311, %struct.ScmObj* %argslist53508$_37map1471200)
store volatile %struct.ScmObj* %argslist53508$_37map1471201, %struct.ScmObj** %stackaddr$prim54436, align 8
%stackaddr$prim54437 = alloca %struct.ScmObj*, align 8
%argslist53508$_37map1471202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47309, %struct.ScmObj* %argslist53508$_37map1471201)
store volatile %struct.ScmObj* %argslist53508$_37map1471202, %struct.ScmObj** %stackaddr$prim54437, align 8
%stackaddr$prim54438 = alloca %struct.ScmObj*, align 8
%argslist53508$_37map1471203 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51019, %struct.ScmObj* %argslist53508$_37map1471202)
store volatile %struct.ScmObj* %argslist53508$_37map1471203, %struct.ScmObj** %stackaddr$prim54438, align 8
%clofunc54439 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147120)
musttail call tailcc void %clofunc54439(%struct.ScmObj* %_37map147120, %struct.ScmObj* %argslist53508$_37map1471203)
ret void
}

define tailcc void @proc_clo$ae51019(%struct.ScmObj* %env$ae51019,%struct.ScmObj* %current_45args53481) {
%stackaddr$env-ref54440 = alloca %struct.ScmObj*, align 8
%anf_45bind47308 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51019, i64 0)
store %struct.ScmObj* %anf_45bind47308, %struct.ScmObj** %stackaddr$env-ref54440
%stackaddr$prim54441 = alloca %struct.ScmObj*, align 8
%_95k47357 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53481)
store volatile %struct.ScmObj* %_95k47357, %struct.ScmObj** %stackaddr$prim54441, align 8
%stackaddr$prim54442 = alloca %struct.ScmObj*, align 8
%current_45args53482 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53481)
store volatile %struct.ScmObj* %current_45args53482, %struct.ScmObj** %stackaddr$prim54442, align 8
%stackaddr$prim54443 = alloca %struct.ScmObj*, align 8
%anf_45bind47312 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53482)
store volatile %struct.ScmObj* %anf_45bind47312, %struct.ScmObj** %stackaddr$prim54443, align 8
%stackaddr$prim54444 = alloca %struct.ScmObj*, align 8
%anf_45bind47313 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47312)
store volatile %struct.ScmObj* %anf_45bind47313, %struct.ScmObj** %stackaddr$prim54444, align 8
%stackaddr$prim54445 = alloca %struct.ScmObj*, align 8
%anf_45bind47314 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind47308, %struct.ScmObj* %anf_45bind47313)
store volatile %struct.ScmObj* %anf_45bind47314, %struct.ScmObj** %stackaddr$prim54445, align 8
%stackaddr$makeclosure54446 = alloca %struct.ScmObj*, align 8
%fptrToInt54447 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51025 to i64
%ae51025 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54447)
store volatile %struct.ScmObj* %ae51025, %struct.ScmObj** %stackaddr$makeclosure54446, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51025, %struct.ScmObj* %anf_45bind47314, i64 0)
%ae51026 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54448 = alloca %struct.ScmObj*, align 8
%fptrToInt54449 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51027 to i64
%ae51027 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54449)
store volatile %struct.ScmObj* %ae51027, %struct.ScmObj** %stackaddr$makeclosure54448, align 8
%argslist53507$ae510250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54450 = alloca %struct.ScmObj*, align 8
%argslist53507$ae510251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51027, %struct.ScmObj* %argslist53507$ae510250)
store volatile %struct.ScmObj* %argslist53507$ae510251, %struct.ScmObj** %stackaddr$prim54450, align 8
%stackaddr$prim54451 = alloca %struct.ScmObj*, align 8
%argslist53507$ae510252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51026, %struct.ScmObj* %argslist53507$ae510251)
store volatile %struct.ScmObj* %argslist53507$ae510252, %struct.ScmObj** %stackaddr$prim54451, align 8
%clofunc54452 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51025)
musttail call tailcc void %clofunc54452(%struct.ScmObj* %ae51025, %struct.ScmObj* %argslist53507$ae510252)
ret void
}

define tailcc void @proc_clo$ae51025(%struct.ScmObj* %env$ae51025,%struct.ScmObj* %current_45args53484) {
%stackaddr$env-ref54453 = alloca %struct.ScmObj*, align 8
%anf_45bind47314 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51025, i64 0)
store %struct.ScmObj* %anf_45bind47314, %struct.ScmObj** %stackaddr$env-ref54453
%stackaddr$prim54454 = alloca %struct.ScmObj*, align 8
%_95k47358 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53484)
store volatile %struct.ScmObj* %_95k47358, %struct.ScmObj** %stackaddr$prim54454, align 8
%stackaddr$prim54455 = alloca %struct.ScmObj*, align 8
%current_45args53485 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53484)
store volatile %struct.ScmObj* %current_45args53485, %struct.ScmObj** %stackaddr$prim54455, align 8
%stackaddr$prim54456 = alloca %struct.ScmObj*, align 8
%anf_45bind47315 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53485)
store volatile %struct.ScmObj* %anf_45bind47315, %struct.ScmObj** %stackaddr$prim54456, align 8
%stackaddr$makeclosure54457 = alloca %struct.ScmObj*, align 8
%fptrToInt54458 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51042 to i64
%ae51042 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54458)
store volatile %struct.ScmObj* %ae51042, %struct.ScmObj** %stackaddr$makeclosure54457, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51042, %struct.ScmObj* %anf_45bind47315, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51042, %struct.ScmObj* %anf_45bind47314, i64 1)
%ae51043 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54459 = alloca %struct.ScmObj*, align 8
%fptrToInt54460 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51044 to i64
%ae51044 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54460)
store volatile %struct.ScmObj* %ae51044, %struct.ScmObj** %stackaddr$makeclosure54459, align 8
%argslist53500$ae510420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54461 = alloca %struct.ScmObj*, align 8
%argslist53500$ae510421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51044, %struct.ScmObj* %argslist53500$ae510420)
store volatile %struct.ScmObj* %argslist53500$ae510421, %struct.ScmObj** %stackaddr$prim54461, align 8
%stackaddr$prim54462 = alloca %struct.ScmObj*, align 8
%argslist53500$ae510422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51043, %struct.ScmObj* %argslist53500$ae510421)
store volatile %struct.ScmObj* %argslist53500$ae510422, %struct.ScmObj** %stackaddr$prim54462, align 8
%clofunc54463 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51042)
musttail call tailcc void %clofunc54463(%struct.ScmObj* %ae51042, %struct.ScmObj* %argslist53500$ae510422)
ret void
}

define tailcc void @proc_clo$ae51042(%struct.ScmObj* %env$ae51042,%struct.ScmObj* %current_45args53487) {
%stackaddr$env-ref54464 = alloca %struct.ScmObj*, align 8
%anf_45bind47315 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51042, i64 0)
store %struct.ScmObj* %anf_45bind47315, %struct.ScmObj** %stackaddr$env-ref54464
%stackaddr$env-ref54465 = alloca %struct.ScmObj*, align 8
%anf_45bind47314 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51042, i64 1)
store %struct.ScmObj* %anf_45bind47314, %struct.ScmObj** %stackaddr$env-ref54465
%stackaddr$prim54466 = alloca %struct.ScmObj*, align 8
%_95k47359 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53487)
store volatile %struct.ScmObj* %_95k47359, %struct.ScmObj** %stackaddr$prim54466, align 8
%stackaddr$prim54467 = alloca %struct.ScmObj*, align 8
%current_45args53488 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53487)
store volatile %struct.ScmObj* %current_45args53488, %struct.ScmObj** %stackaddr$prim54467, align 8
%stackaddr$prim54468 = alloca %struct.ScmObj*, align 8
%anf_45bind47316 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53488)
store volatile %struct.ScmObj* %anf_45bind47316, %struct.ScmObj** %stackaddr$prim54468, align 8
%stackaddr$makeclosure54469 = alloca %struct.ScmObj*, align 8
%fptrToInt54470 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51067 to i64
%ae51067 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54470)
store volatile %struct.ScmObj* %ae51067, %struct.ScmObj** %stackaddr$makeclosure54469, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51067, %struct.ScmObj* %anf_45bind47314, i64 0)
%ae51068 = call %struct.ScmObj* @const_init_int(i64 2)
%ae51070 = call %struct.ScmObj* @const_init_int(i64 3)
%argslist53498$anf_45bind473150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54471 = alloca %struct.ScmObj*, align 8
%argslist53498$anf_45bind473151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51070, %struct.ScmObj* %argslist53498$anf_45bind473150)
store volatile %struct.ScmObj* %argslist53498$anf_45bind473151, %struct.ScmObj** %stackaddr$prim54471, align 8
%stackaddr$prim54472 = alloca %struct.ScmObj*, align 8
%argslist53498$anf_45bind473152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47316, %struct.ScmObj* %argslist53498$anf_45bind473151)
store volatile %struct.ScmObj* %argslist53498$anf_45bind473152, %struct.ScmObj** %stackaddr$prim54472, align 8
%stackaddr$prim54473 = alloca %struct.ScmObj*, align 8
%argslist53498$anf_45bind473153 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51068, %struct.ScmObj* %argslist53498$anf_45bind473152)
store volatile %struct.ScmObj* %argslist53498$anf_45bind473153, %struct.ScmObj** %stackaddr$prim54473, align 8
%stackaddr$prim54474 = alloca %struct.ScmObj*, align 8
%argslist53498$anf_45bind473154 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51067, %struct.ScmObj* %argslist53498$anf_45bind473153)
store volatile %struct.ScmObj* %argslist53498$anf_45bind473154, %struct.ScmObj** %stackaddr$prim54474, align 8
%clofunc54475 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47315)
musttail call tailcc void %clofunc54475(%struct.ScmObj* %anf_45bind47315, %struct.ScmObj* %argslist53498$anf_45bind473154)
ret void
}

define tailcc void @proc_clo$ae51067(%struct.ScmObj* %env$ae51067,%struct.ScmObj* %current_45args53490) {
%stackaddr$env-ref54476 = alloca %struct.ScmObj*, align 8
%anf_45bind47314 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51067, i64 0)
store %struct.ScmObj* %anf_45bind47314, %struct.ScmObj** %stackaddr$env-ref54476
%stackaddr$prim54477 = alloca %struct.ScmObj*, align 8
%_95k47360 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53490)
store volatile %struct.ScmObj* %_95k47360, %struct.ScmObj** %stackaddr$prim54477, align 8
%stackaddr$prim54478 = alloca %struct.ScmObj*, align 8
%current_45args53491 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53490)
store volatile %struct.ScmObj* %current_45args53491, %struct.ScmObj** %stackaddr$prim54478, align 8
%stackaddr$prim54479 = alloca %struct.ScmObj*, align 8
%anf_45bind47317 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53491)
store volatile %struct.ScmObj* %anf_45bind47317, %struct.ScmObj** %stackaddr$prim54479, align 8
%stackaddr$prim54480 = alloca %struct.ScmObj*, align 8
%cpsprim47361 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind47314, %struct.ScmObj* %anf_45bind47317)
store volatile %struct.ScmObj* %cpsprim47361, %struct.ScmObj** %stackaddr$prim54480, align 8
%stackaddr$makeclosure54481 = alloca %struct.ScmObj*, align 8
%fptrToInt54482 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51083 to i64
%ae51083 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54482)
store volatile %struct.ScmObj* %ae51083, %struct.ScmObj** %stackaddr$makeclosure54481, align 8
%ae51084 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53497$ae510830 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54483 = alloca %struct.ScmObj*, align 8
%argslist53497$ae510831 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47361, %struct.ScmObj* %argslist53497$ae510830)
store volatile %struct.ScmObj* %argslist53497$ae510831, %struct.ScmObj** %stackaddr$prim54483, align 8
%stackaddr$prim54484 = alloca %struct.ScmObj*, align 8
%argslist53497$ae510832 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51084, %struct.ScmObj* %argslist53497$ae510831)
store volatile %struct.ScmObj* %argslist53497$ae510832, %struct.ScmObj** %stackaddr$prim54484, align 8
%clofunc54485 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51083)
musttail call tailcc void %clofunc54485(%struct.ScmObj* %ae51083, %struct.ScmObj* %argslist53497$ae510832)
ret void
}

define tailcc void @proc_clo$ae51083(%struct.ScmObj* %env$ae51083,%struct.ScmObj* %current_45args53493) {
%stackaddr$prim54486 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53493)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim54486, align 8
%stackaddr$prim54487 = alloca %struct.ScmObj*, align 8
%current_45args53494 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53493)
store volatile %struct.ScmObj* %current_45args53494, %struct.ScmObj** %stackaddr$prim54487, align 8
%stackaddr$prim54488 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53494)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim54488, align 8
%stackaddr$prim54489 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim54489, align 8
%argslist53496$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54490 = alloca %struct.ScmObj*, align 8
%argslist53496$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist53496$k0)
store volatile %struct.ScmObj* %argslist53496$k1, %struct.ScmObj** %stackaddr$prim54490, align 8
%clofunc54491 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc54491(%struct.ScmObj* %k, %struct.ScmObj* %argslist53496$k1)
ret void
}

define tailcc void @proc_clo$ae51044(%struct.ScmObj* %env$ae51044,%struct.ScmObj* %args4719347362) {
%stackaddr$prim54492 = alloca %struct.ScmObj*, align 8
%k47363 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4719347362)
store volatile %struct.ScmObj* %k47363, %struct.ScmObj** %stackaddr$prim54492, align 8
%stackaddr$prim54493 = alloca %struct.ScmObj*, align 8
%args47193 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4719347362)
store volatile %struct.ScmObj* %args47193, %struct.ScmObj** %stackaddr$prim54493, align 8
%stackaddr$applyprim54494 = alloca %struct.ScmObj*, align 8
%cpsaprim47364 = call %struct.ScmObj* @applyprim__42(%struct.ScmObj* %args47193)
store volatile %struct.ScmObj* %cpsaprim47364, %struct.ScmObj** %stackaddr$applyprim54494, align 8
%ae51049 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53499$k473630 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54495 = alloca %struct.ScmObj*, align 8
%argslist53499$k473631 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsaprim47364, %struct.ScmObj* %argslist53499$k473630)
store volatile %struct.ScmObj* %argslist53499$k473631, %struct.ScmObj** %stackaddr$prim54495, align 8
%stackaddr$prim54496 = alloca %struct.ScmObj*, align 8
%argslist53499$k473632 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51049, %struct.ScmObj* %argslist53499$k473631)
store volatile %struct.ScmObj* %argslist53499$k473632, %struct.ScmObj** %stackaddr$prim54496, align 8
%clofunc54497 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47363)
musttail call tailcc void %clofunc54497(%struct.ScmObj* %k47363, %struct.ScmObj* %argslist53499$k473632)
ret void
}

define tailcc void @proc_clo$ae51027(%struct.ScmObj* %env$ae51027,%struct.ScmObj* %current_45args53501) {
%stackaddr$prim54498 = alloca %struct.ScmObj*, align 8
%k47365 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53501)
store volatile %struct.ScmObj* %k47365, %struct.ScmObj** %stackaddr$prim54498, align 8
%stackaddr$prim54499 = alloca %struct.ScmObj*, align 8
%current_45args53502 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53501)
store volatile %struct.ScmObj* %current_45args53502, %struct.ScmObj** %stackaddr$prim54499, align 8
%stackaddr$prim54500 = alloca %struct.ScmObj*, align 8
%x47192 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53502)
store volatile %struct.ScmObj* %x47192, %struct.ScmObj** %stackaddr$prim54500, align 8
%stackaddr$prim54501 = alloca %struct.ScmObj*, align 8
%current_45args53503 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53502)
store volatile %struct.ScmObj* %current_45args53503, %struct.ScmObj** %stackaddr$prim54501, align 8
%stackaddr$prim54502 = alloca %struct.ScmObj*, align 8
%f47191 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53503)
store volatile %struct.ScmObj* %f47191, %struct.ScmObj** %stackaddr$prim54502, align 8
%stackaddr$prim54503 = alloca %struct.ScmObj*, align 8
%current_45args53504 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53503)
store volatile %struct.ScmObj* %current_45args53504, %struct.ScmObj** %stackaddr$prim54503, align 8
%stackaddr$prim54504 = alloca %struct.ScmObj*, align 8
%y47190 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53504)
store volatile %struct.ScmObj* %y47190, %struct.ScmObj** %stackaddr$prim54504, align 8
%argslist53506$f471910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54505 = alloca %struct.ScmObj*, align 8
%argslist53506$f471911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y47190, %struct.ScmObj* %argslist53506$f471910)
store volatile %struct.ScmObj* %argslist53506$f471911, %struct.ScmObj** %stackaddr$prim54505, align 8
%stackaddr$prim54506 = alloca %struct.ScmObj*, align 8
%argslist53506$f471912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x47192, %struct.ScmObj* %argslist53506$f471911)
store volatile %struct.ScmObj* %argslist53506$f471912, %struct.ScmObj** %stackaddr$prim54506, align 8
%stackaddr$prim54507 = alloca %struct.ScmObj*, align 8
%argslist53506$f471913 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47365, %struct.ScmObj* %argslist53506$f471912)
store volatile %struct.ScmObj* %argslist53506$f471913, %struct.ScmObj** %stackaddr$prim54507, align 8
%clofunc54508 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47191)
musttail call tailcc void %clofunc54508(%struct.ScmObj* %f47191, %struct.ScmObj* %argslist53506$f471913)
ret void
}

define tailcc void @proc_clo$ae50992(%struct.ScmObj* %env$ae50992,%struct.ScmObj* %lst4718947366) {
%stackaddr$prim54509 = alloca %struct.ScmObj*, align 8
%k47367 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4718947366)
store volatile %struct.ScmObj* %k47367, %struct.ScmObj** %stackaddr$prim54509, align 8
%stackaddr$prim54510 = alloca %struct.ScmObj*, align 8
%lst47189 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4718947366)
store volatile %struct.ScmObj* %lst47189, %struct.ScmObj** %stackaddr$prim54510, align 8
%ae50996 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53510$k473670 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54511 = alloca %struct.ScmObj*, align 8
%argslist53510$k473671 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47189, %struct.ScmObj* %argslist53510$k473670)
store volatile %struct.ScmObj* %argslist53510$k473671, %struct.ScmObj** %stackaddr$prim54511, align 8
%stackaddr$prim54512 = alloca %struct.ScmObj*, align 8
%argslist53510$k473672 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50996, %struct.ScmObj* %argslist53510$k473671)
store volatile %struct.ScmObj* %argslist53510$k473672, %struct.ScmObj** %stackaddr$prim54512, align 8
%clofunc54513 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47367)
musttail call tailcc void %clofunc54513(%struct.ScmObj* %k47367, %struct.ScmObj* %argslist53510$k473672)
ret void
}

define tailcc void @proc_clo$ae50968(%struct.ScmObj* %env$ae50968,%struct.ScmObj* %args4718847368) {
%stackaddr$prim54514 = alloca %struct.ScmObj*, align 8
%k47369 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4718847368)
store volatile %struct.ScmObj* %k47369, %struct.ScmObj** %stackaddr$prim54514, align 8
%stackaddr$prim54515 = alloca %struct.ScmObj*, align 8
%args47188 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4718847368)
store volatile %struct.ScmObj* %args47188, %struct.ScmObj** %stackaddr$prim54515, align 8
%stackaddr$applyprim54516 = alloca %struct.ScmObj*, align 8
%cpsaprim47370 = call %struct.ScmObj* @applyprim__45(%struct.ScmObj* %args47188)
store volatile %struct.ScmObj* %cpsaprim47370, %struct.ScmObj** %stackaddr$applyprim54516, align 8
%ae50973 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53512$k473690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54517 = alloca %struct.ScmObj*, align 8
%argslist53512$k473691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsaprim47370, %struct.ScmObj* %argslist53512$k473690)
store volatile %struct.ScmObj* %argslist53512$k473691, %struct.ScmObj** %stackaddr$prim54517, align 8
%stackaddr$prim54518 = alloca %struct.ScmObj*, align 8
%argslist53512$k473692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50973, %struct.ScmObj* %argslist53512$k473691)
store volatile %struct.ScmObj* %argslist53512$k473692, %struct.ScmObj** %stackaddr$prim54518, align 8
%clofunc54519 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47369)
musttail call tailcc void %clofunc54519(%struct.ScmObj* %k47369, %struct.ScmObj* %argslist53512$k473692)
ret void
}

define tailcc void @proc_clo$ae50906(%struct.ScmObj* %env$ae50906,%struct.ScmObj* %lst4718747371) {
%stackaddr$prim54520 = alloca %struct.ScmObj*, align 8
%k47372 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4718747371)
store volatile %struct.ScmObj* %k47372, %struct.ScmObj** %stackaddr$prim54520, align 8
%stackaddr$prim54521 = alloca %struct.ScmObj*, align 8
%lst47187 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4718747371)
store volatile %struct.ScmObj* %lst47187, %struct.ScmObj** %stackaddr$prim54521, align 8
%ae50910 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53516$k473720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54522 = alloca %struct.ScmObj*, align 8
%argslist53516$k473721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47187, %struct.ScmObj* %argslist53516$k473720)
store volatile %struct.ScmObj* %argslist53516$k473721, %struct.ScmObj** %stackaddr$prim54522, align 8
%stackaddr$prim54523 = alloca %struct.ScmObj*, align 8
%argslist53516$k473722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50910, %struct.ScmObj* %argslist53516$k473721)
store volatile %struct.ScmObj* %argslist53516$k473722, %struct.ScmObj** %stackaddr$prim54523, align 8
%clofunc54524 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47372)
musttail call tailcc void %clofunc54524(%struct.ScmObj* %k47372, %struct.ScmObj* %argslist53516$k473722)
ret void
}

define tailcc void @proc_clo$ae50882(%struct.ScmObj* %env$ae50882,%struct.ScmObj* %args4718647373) {
%stackaddr$prim54525 = alloca %struct.ScmObj*, align 8
%k47374 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4718647373)
store volatile %struct.ScmObj* %k47374, %struct.ScmObj** %stackaddr$prim54525, align 8
%stackaddr$prim54526 = alloca %struct.ScmObj*, align 8
%args47186 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4718647373)
store volatile %struct.ScmObj* %args47186, %struct.ScmObj** %stackaddr$prim54526, align 8
%stackaddr$applyprim54527 = alloca %struct.ScmObj*, align 8
%cpsaprim47375 = call %struct.ScmObj* @applyprim__43(%struct.ScmObj* %args47186)
store volatile %struct.ScmObj* %cpsaprim47375, %struct.ScmObj** %stackaddr$applyprim54527, align 8
%ae50887 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53518$k473740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54528 = alloca %struct.ScmObj*, align 8
%argslist53518$k473741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsaprim47375, %struct.ScmObj* %argslist53518$k473740)
store volatile %struct.ScmObj* %argslist53518$k473741, %struct.ScmObj** %stackaddr$prim54528, align 8
%stackaddr$prim54529 = alloca %struct.ScmObj*, align 8
%argslist53518$k473742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50887, %struct.ScmObj* %argslist53518$k473741)
store volatile %struct.ScmObj* %argslist53518$k473742, %struct.ScmObj** %stackaddr$prim54529, align 8
%clofunc54530 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47374)
musttail call tailcc void %clofunc54530(%struct.ScmObj* %k47374, %struct.ScmObj* %argslist53518$k473742)
ret void
}

define tailcc void @proc_clo$ae50856(%struct.ScmObj* %env$ae50856,%struct.ScmObj* %current_45args53520) {
%stackaddr$prim54531 = alloca %struct.ScmObj*, align 8
%k47376 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53520)
store volatile %struct.ScmObj* %k47376, %struct.ScmObj** %stackaddr$prim54531, align 8
%stackaddr$prim54532 = alloca %struct.ScmObj*, align 8
%current_45args53521 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53520)
store volatile %struct.ScmObj* %current_45args53521, %struct.ScmObj** %stackaddr$prim54532, align 8
%stackaddr$prim54533 = alloca %struct.ScmObj*, align 8
%x47126 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53521)
store volatile %struct.ScmObj* %x47126, %struct.ScmObj** %stackaddr$prim54533, align 8
%stackaddr$prim54534 = alloca %struct.ScmObj*, align 8
%anf_45bind47302 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47126)
store volatile %struct.ScmObj* %anf_45bind47302, %struct.ScmObj** %stackaddr$prim54534, align 8
%stackaddr$prim54535 = alloca %struct.ScmObj*, align 8
%anf_45bind47303 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47302)
store volatile %struct.ScmObj* %anf_45bind47303, %struct.ScmObj** %stackaddr$prim54535, align 8
%stackaddr$prim54536 = alloca %struct.ScmObj*, align 8
%anf_45bind47304 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47303)
store volatile %struct.ScmObj* %anf_45bind47304, %struct.ScmObj** %stackaddr$prim54536, align 8
%stackaddr$prim54537 = alloca %struct.ScmObj*, align 8
%cpsprim47377 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47304)
store volatile %struct.ScmObj* %cpsprim47377, %struct.ScmObj** %stackaddr$prim54537, align 8
%ae50862 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53523$k473760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54538 = alloca %struct.ScmObj*, align 8
%argslist53523$k473761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47377, %struct.ScmObj* %argslist53523$k473760)
store volatile %struct.ScmObj* %argslist53523$k473761, %struct.ScmObj** %stackaddr$prim54538, align 8
%stackaddr$prim54539 = alloca %struct.ScmObj*, align 8
%argslist53523$k473762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50862, %struct.ScmObj* %argslist53523$k473761)
store volatile %struct.ScmObj* %argslist53523$k473762, %struct.ScmObj** %stackaddr$prim54539, align 8
%clofunc54540 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47376)
musttail call tailcc void %clofunc54540(%struct.ScmObj* %k47376, %struct.ScmObj* %argslist53523$k473762)
ret void
}

define tailcc void @proc_clo$ae50832(%struct.ScmObj* %env$ae50832,%struct.ScmObj* %current_45args53525) {
%stackaddr$prim54541 = alloca %struct.ScmObj*, align 8
%k47378 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53525)
store volatile %struct.ScmObj* %k47378, %struct.ScmObj** %stackaddr$prim54541, align 8
%stackaddr$prim54542 = alloca %struct.ScmObj*, align 8
%current_45args53526 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53525)
store volatile %struct.ScmObj* %current_45args53526, %struct.ScmObj** %stackaddr$prim54542, align 8
%stackaddr$prim54543 = alloca %struct.ScmObj*, align 8
%x47128 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53526)
store volatile %struct.ScmObj* %x47128, %struct.ScmObj** %stackaddr$prim54543, align 8
%stackaddr$prim54544 = alloca %struct.ScmObj*, align 8
%anf_45bind47300 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47128)
store volatile %struct.ScmObj* %anf_45bind47300, %struct.ScmObj** %stackaddr$prim54544, align 8
%stackaddr$prim54545 = alloca %struct.ScmObj*, align 8
%anf_45bind47301 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47300)
store volatile %struct.ScmObj* %anf_45bind47301, %struct.ScmObj** %stackaddr$prim54545, align 8
%stackaddr$prim54546 = alloca %struct.ScmObj*, align 8
%cpsprim47379 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47301)
store volatile %struct.ScmObj* %cpsprim47379, %struct.ScmObj** %stackaddr$prim54546, align 8
%ae50837 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53528$k473780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54547 = alloca %struct.ScmObj*, align 8
%argslist53528$k473781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47379, %struct.ScmObj* %argslist53528$k473780)
store volatile %struct.ScmObj* %argslist53528$k473781, %struct.ScmObj** %stackaddr$prim54547, align 8
%stackaddr$prim54548 = alloca %struct.ScmObj*, align 8
%argslist53528$k473782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50837, %struct.ScmObj* %argslist53528$k473781)
store volatile %struct.ScmObj* %argslist53528$k473782, %struct.ScmObj** %stackaddr$prim54548, align 8
%clofunc54549 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47378)
musttail call tailcc void %clofunc54549(%struct.ScmObj* %k47378, %struct.ScmObj* %argslist53528$k473782)
ret void
}

define tailcc void @proc_clo$ae50810(%struct.ScmObj* %env$ae50810,%struct.ScmObj* %current_45args53530) {
%stackaddr$prim54550 = alloca %struct.ScmObj*, align 8
%k47380 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53530)
store volatile %struct.ScmObj* %k47380, %struct.ScmObj** %stackaddr$prim54550, align 8
%stackaddr$prim54551 = alloca %struct.ScmObj*, align 8
%current_45args53531 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53530)
store volatile %struct.ScmObj* %current_45args53531, %struct.ScmObj** %stackaddr$prim54551, align 8
%stackaddr$prim54552 = alloca %struct.ScmObj*, align 8
%x47130 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53531)
store volatile %struct.ScmObj* %x47130, %struct.ScmObj** %stackaddr$prim54552, align 8
%stackaddr$prim54553 = alloca %struct.ScmObj*, align 8
%anf_45bind47299 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47130)
store volatile %struct.ScmObj* %anf_45bind47299, %struct.ScmObj** %stackaddr$prim54553, align 8
%stackaddr$prim54554 = alloca %struct.ScmObj*, align 8
%cpsprim47381 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47299)
store volatile %struct.ScmObj* %cpsprim47381, %struct.ScmObj** %stackaddr$prim54554, align 8
%ae50814 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53533$k473800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54555 = alloca %struct.ScmObj*, align 8
%argslist53533$k473801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47381, %struct.ScmObj* %argslist53533$k473800)
store volatile %struct.ScmObj* %argslist53533$k473801, %struct.ScmObj** %stackaddr$prim54555, align 8
%stackaddr$prim54556 = alloca %struct.ScmObj*, align 8
%argslist53533$k473802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50814, %struct.ScmObj* %argslist53533$k473801)
store volatile %struct.ScmObj* %argslist53533$k473802, %struct.ScmObj** %stackaddr$prim54556, align 8
%clofunc54557 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47380)
musttail call tailcc void %clofunc54557(%struct.ScmObj* %k47380, %struct.ScmObj* %argslist53533$k473802)
ret void
}

define tailcc void @proc_clo$ae50790(%struct.ScmObj* %env$ae50790,%struct.ScmObj* %current_45args53535) {
%stackaddr$prim54558 = alloca %struct.ScmObj*, align 8
%k47382 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53535)
store volatile %struct.ScmObj* %k47382, %struct.ScmObj** %stackaddr$prim54558, align 8
%stackaddr$prim54559 = alloca %struct.ScmObj*, align 8
%current_45args53536 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53535)
store volatile %struct.ScmObj* %current_45args53536, %struct.ScmObj** %stackaddr$prim54559, align 8
%stackaddr$prim54560 = alloca %struct.ScmObj*, align 8
%x47132 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53536)
store volatile %struct.ScmObj* %x47132, %struct.ScmObj** %stackaddr$prim54560, align 8
%stackaddr$prim54561 = alloca %struct.ScmObj*, align 8
%cpsprim47383 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47132)
store volatile %struct.ScmObj* %cpsprim47383, %struct.ScmObj** %stackaddr$prim54561, align 8
%ae50793 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53538$k473820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54562 = alloca %struct.ScmObj*, align 8
%argslist53538$k473821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47383, %struct.ScmObj* %argslist53538$k473820)
store volatile %struct.ScmObj* %argslist53538$k473821, %struct.ScmObj** %stackaddr$prim54562, align 8
%stackaddr$prim54563 = alloca %struct.ScmObj*, align 8
%argslist53538$k473822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50793, %struct.ScmObj* %argslist53538$k473821)
store volatile %struct.ScmObj* %argslist53538$k473822, %struct.ScmObj** %stackaddr$prim54563, align 8
%clofunc54564 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47382)
musttail call tailcc void %clofunc54564(%struct.ScmObj* %k47382, %struct.ScmObj* %argslist53538$k473822)
ret void
}

define tailcc void @proc_clo$ae50692(%struct.ScmObj* %env$ae50692,%struct.ScmObj* %args4713447384) {
%stackaddr$env-ref54565 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50692, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54565
%stackaddr$prim54566 = alloca %struct.ScmObj*, align 8
%k47385 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4713447384)
store volatile %struct.ScmObj* %k47385, %struct.ScmObj** %stackaddr$prim54566, align 8
%stackaddr$prim54567 = alloca %struct.ScmObj*, align 8
%args47134 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4713447384)
store volatile %struct.ScmObj* %args47134, %struct.ScmObj** %stackaddr$prim54567, align 8
%stackaddr$prim54568 = alloca %struct.ScmObj*, align 8
%anf_45bind47293 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args47134)
store volatile %struct.ScmObj* %anf_45bind47293, %struct.ScmObj** %stackaddr$prim54568, align 8
%truthy$cmp54569 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47293)
%cmp$cmp54569 = icmp eq i64 %truthy$cmp54569, 1
br i1 %cmp$cmp54569, label %truebranch$cmp54569, label %falsebranch$cmp54569
truebranch$cmp54569:
%ae50698 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50699 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist53540$k473850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54570 = alloca %struct.ScmObj*, align 8
%argslist53540$k473851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50699, %struct.ScmObj* %argslist53540$k473850)
store volatile %struct.ScmObj* %argslist53540$k473851, %struct.ScmObj** %stackaddr$prim54570, align 8
%stackaddr$prim54571 = alloca %struct.ScmObj*, align 8
%argslist53540$k473852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50698, %struct.ScmObj* %argslist53540$k473851)
store volatile %struct.ScmObj* %argslist53540$k473852, %struct.ScmObj** %stackaddr$prim54571, align 8
%clofunc54572 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47385)
musttail call tailcc void %clofunc54572(%struct.ScmObj* %k47385, %struct.ScmObj* %argslist53540$k473852)
ret void
falsebranch$cmp54569:
%stackaddr$prim54573 = alloca %struct.ScmObj*, align 8
%anf_45bind47294 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47134)
store volatile %struct.ScmObj* %anf_45bind47294, %struct.ScmObj** %stackaddr$prim54573, align 8
%stackaddr$prim54574 = alloca %struct.ScmObj*, align 8
%anf_45bind47295 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47294)
store volatile %struct.ScmObj* %anf_45bind47295, %struct.ScmObj** %stackaddr$prim54574, align 8
%truthy$cmp54575 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47295)
%cmp$cmp54575 = icmp eq i64 %truthy$cmp54575, 1
br i1 %cmp$cmp54575, label %truebranch$cmp54575, label %falsebranch$cmp54575
truebranch$cmp54575:
%stackaddr$prim54576 = alloca %struct.ScmObj*, align 8
%cpsprim47386 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47134)
store volatile %struct.ScmObj* %cpsprim47386, %struct.ScmObj** %stackaddr$prim54576, align 8
%ae50711 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53541$k473850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54577 = alloca %struct.ScmObj*, align 8
%argslist53541$k473851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47386, %struct.ScmObj* %argslist53541$k473850)
store volatile %struct.ScmObj* %argslist53541$k473851, %struct.ScmObj** %stackaddr$prim54577, align 8
%stackaddr$prim54578 = alloca %struct.ScmObj*, align 8
%argslist53541$k473852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50711, %struct.ScmObj* %argslist53541$k473851)
store volatile %struct.ScmObj* %argslist53541$k473852, %struct.ScmObj** %stackaddr$prim54578, align 8
%clofunc54579 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47385)
musttail call tailcc void %clofunc54579(%struct.ScmObj* %k47385, %struct.ScmObj* %argslist53541$k473852)
ret void
falsebranch$cmp54575:
%stackaddr$makeclosure54580 = alloca %struct.ScmObj*, align 8
%fptrToInt54581 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50716 to i64
%ae50716 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54581)
store volatile %struct.ScmObj* %ae50716, %struct.ScmObj** %stackaddr$makeclosure54580, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50716, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50716, %struct.ScmObj* %args47134, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50716, %struct.ScmObj* %k47385, i64 2)
%ae50717 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54582 = alloca %struct.ScmObj*, align 8
%fptrToInt54583 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50718 to i64
%ae50718 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54583)
store volatile %struct.ScmObj* %ae50718, %struct.ScmObj** %stackaddr$makeclosure54582, align 8
%argslist53551$ae507160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54584 = alloca %struct.ScmObj*, align 8
%argslist53551$ae507161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50718, %struct.ScmObj* %argslist53551$ae507160)
store volatile %struct.ScmObj* %argslist53551$ae507161, %struct.ScmObj** %stackaddr$prim54584, align 8
%stackaddr$prim54585 = alloca %struct.ScmObj*, align 8
%argslist53551$ae507162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50717, %struct.ScmObj* %argslist53551$ae507161)
store volatile %struct.ScmObj* %argslist53551$ae507162, %struct.ScmObj** %stackaddr$prim54585, align 8
%clofunc54586 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50716)
musttail call tailcc void %clofunc54586(%struct.ScmObj* %ae50716, %struct.ScmObj* %argslist53551$ae507162)
ret void
}

define tailcc void @proc_clo$ae50716(%struct.ScmObj* %env$ae50716,%struct.ScmObj* %current_45args53542) {
%stackaddr$env-ref54587 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50716, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54587
%stackaddr$env-ref54588 = alloca %struct.ScmObj*, align 8
%args47134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50716, i64 1)
store %struct.ScmObj* %args47134, %struct.ScmObj** %stackaddr$env-ref54588
%stackaddr$env-ref54589 = alloca %struct.ScmObj*, align 8
%k47385 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50716, i64 2)
store %struct.ScmObj* %k47385, %struct.ScmObj** %stackaddr$env-ref54589
%stackaddr$prim54590 = alloca %struct.ScmObj*, align 8
%_95k47387 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53542)
store volatile %struct.ScmObj* %_95k47387, %struct.ScmObj** %stackaddr$prim54590, align 8
%stackaddr$prim54591 = alloca %struct.ScmObj*, align 8
%current_45args53543 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53542)
store volatile %struct.ScmObj* %current_45args53543, %struct.ScmObj** %stackaddr$prim54591, align 8
%stackaddr$prim54592 = alloca %struct.ScmObj*, align 8
%anf_45bind47296 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53543)
store volatile %struct.ScmObj* %anf_45bind47296, %struct.ScmObj** %stackaddr$prim54592, align 8
%stackaddr$prim54593 = alloca %struct.ScmObj*, align 8
%anf_45bind47297 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47134)
store volatile %struct.ScmObj* %anf_45bind47297, %struct.ScmObj** %stackaddr$prim54593, align 8
%stackaddr$prim54594 = alloca %struct.ScmObj*, align 8
%anf_45bind47298 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47134)
store volatile %struct.ScmObj* %anf_45bind47298, %struct.ScmObj** %stackaddr$prim54594, align 8
%argslist53545$_37foldl1470730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54595 = alloca %struct.ScmObj*, align 8
%argslist53545$_37foldl1470731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47298, %struct.ScmObj* %argslist53545$_37foldl1470730)
store volatile %struct.ScmObj* %argslist53545$_37foldl1470731, %struct.ScmObj** %stackaddr$prim54595, align 8
%stackaddr$prim54596 = alloca %struct.ScmObj*, align 8
%argslist53545$_37foldl1470732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47297, %struct.ScmObj* %argslist53545$_37foldl1470731)
store volatile %struct.ScmObj* %argslist53545$_37foldl1470732, %struct.ScmObj** %stackaddr$prim54596, align 8
%stackaddr$prim54597 = alloca %struct.ScmObj*, align 8
%argslist53545$_37foldl1470733 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47296, %struct.ScmObj* %argslist53545$_37foldl1470732)
store volatile %struct.ScmObj* %argslist53545$_37foldl1470733, %struct.ScmObj** %stackaddr$prim54597, align 8
%stackaddr$prim54598 = alloca %struct.ScmObj*, align 8
%argslist53545$_37foldl1470734 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47385, %struct.ScmObj* %argslist53545$_37foldl1470733)
store volatile %struct.ScmObj* %argslist53545$_37foldl1470734, %struct.ScmObj** %stackaddr$prim54598, align 8
%clofunc54599 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147073)
musttail call tailcc void %clofunc54599(%struct.ScmObj* %_37foldl147073, %struct.ScmObj* %argslist53545$_37foldl1470734)
ret void
}

define tailcc void @proc_clo$ae50718(%struct.ScmObj* %env$ae50718,%struct.ScmObj* %current_45args53546) {
%stackaddr$prim54600 = alloca %struct.ScmObj*, align 8
%k47388 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53546)
store volatile %struct.ScmObj* %k47388, %struct.ScmObj** %stackaddr$prim54600, align 8
%stackaddr$prim54601 = alloca %struct.ScmObj*, align 8
%current_45args53547 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53546)
store volatile %struct.ScmObj* %current_45args53547, %struct.ScmObj** %stackaddr$prim54601, align 8
%stackaddr$prim54602 = alloca %struct.ScmObj*, align 8
%n47136 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53547)
store volatile %struct.ScmObj* %n47136, %struct.ScmObj** %stackaddr$prim54602, align 8
%stackaddr$prim54603 = alloca %struct.ScmObj*, align 8
%current_45args53548 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53547)
store volatile %struct.ScmObj* %current_45args53548, %struct.ScmObj** %stackaddr$prim54603, align 8
%stackaddr$prim54604 = alloca %struct.ScmObj*, align 8
%v47135 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53548)
store volatile %struct.ScmObj* %v47135, %struct.ScmObj** %stackaddr$prim54604, align 8
%stackaddr$prim54605 = alloca %struct.ScmObj*, align 8
%cpsprim47389 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v47135, %struct.ScmObj* %n47136)
store volatile %struct.ScmObj* %cpsprim47389, %struct.ScmObj** %stackaddr$prim54605, align 8
%ae50722 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53550$k473880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54606 = alloca %struct.ScmObj*, align 8
%argslist53550$k473881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47389, %struct.ScmObj* %argslist53550$k473880)
store volatile %struct.ScmObj* %argslist53550$k473881, %struct.ScmObj** %stackaddr$prim54606, align 8
%stackaddr$prim54607 = alloca %struct.ScmObj*, align 8
%argslist53550$k473882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50722, %struct.ScmObj* %argslist53550$k473881)
store volatile %struct.ScmObj* %argslist53550$k473882, %struct.ScmObj** %stackaddr$prim54607, align 8
%clofunc54608 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47388)
musttail call tailcc void %clofunc54608(%struct.ScmObj* %k47388, %struct.ScmObj* %argslist53550$k473882)
ret void
}

define tailcc void @proc_clo$ae50288(%struct.ScmObj* %env$ae50288,%struct.ScmObj* %current_45args53553) {
%stackaddr$prim54609 = alloca %struct.ScmObj*, align 8
%k47390 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53553)
store volatile %struct.ScmObj* %k47390, %struct.ScmObj** %stackaddr$prim54609, align 8
%stackaddr$prim54610 = alloca %struct.ScmObj*, align 8
%current_45args53554 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53553)
store volatile %struct.ScmObj* %current_45args53554, %struct.ScmObj** %stackaddr$prim54610, align 8
%stackaddr$prim54611 = alloca %struct.ScmObj*, align 8
%v47139 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53554)
store volatile %struct.ScmObj* %v47139, %struct.ScmObj** %stackaddr$prim54611, align 8
%stackaddr$prim54612 = alloca %struct.ScmObj*, align 8
%current_45args53555 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53554)
store volatile %struct.ScmObj* %current_45args53555, %struct.ScmObj** %stackaddr$prim54612, align 8
%stackaddr$prim54613 = alloca %struct.ScmObj*, align 8
%lst47138 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53555)
store volatile %struct.ScmObj* %lst47138, %struct.ScmObj** %stackaddr$prim54613, align 8
%ae50289 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54614 = alloca %struct.ScmObj*, align 8
%lst47140 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50289, %struct.ScmObj* %lst47138)
store volatile %struct.ScmObj* %lst47140, %struct.ScmObj** %stackaddr$prim54614, align 8
%stackaddr$makeclosure54615 = alloca %struct.ScmObj*, align 8
%fptrToInt54616 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50291 to i64
%ae50291 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54616)
store volatile %struct.ScmObj* %ae50291, %struct.ScmObj** %stackaddr$makeclosure54615, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50291, %struct.ScmObj* %lst47140, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50291, %struct.ScmObj* %v47139, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50291, %struct.ScmObj* %k47390, i64 2)
%ae50292 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54617 = alloca %struct.ScmObj*, align 8
%fptrToInt54618 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50293 to i64
%ae50293 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54618)
store volatile %struct.ScmObj* %ae50293, %struct.ScmObj** %stackaddr$makeclosure54617, align 8
%argslist53577$ae502910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54619 = alloca %struct.ScmObj*, align 8
%argslist53577$ae502911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50293, %struct.ScmObj* %argslist53577$ae502910)
store volatile %struct.ScmObj* %argslist53577$ae502911, %struct.ScmObj** %stackaddr$prim54619, align 8
%stackaddr$prim54620 = alloca %struct.ScmObj*, align 8
%argslist53577$ae502912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50292, %struct.ScmObj* %argslist53577$ae502911)
store volatile %struct.ScmObj* %argslist53577$ae502912, %struct.ScmObj** %stackaddr$prim54620, align 8
%clofunc54621 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50291)
musttail call tailcc void %clofunc54621(%struct.ScmObj* %ae50291, %struct.ScmObj* %argslist53577$ae502912)
ret void
}

define tailcc void @proc_clo$ae50291(%struct.ScmObj* %env$ae50291,%struct.ScmObj* %current_45args53557) {
%stackaddr$env-ref54622 = alloca %struct.ScmObj*, align 8
%lst47140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50291, i64 0)
store %struct.ScmObj* %lst47140, %struct.ScmObj** %stackaddr$env-ref54622
%stackaddr$env-ref54623 = alloca %struct.ScmObj*, align 8
%v47139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50291, i64 1)
store %struct.ScmObj* %v47139, %struct.ScmObj** %stackaddr$env-ref54623
%stackaddr$env-ref54624 = alloca %struct.ScmObj*, align 8
%k47390 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50291, i64 2)
store %struct.ScmObj* %k47390, %struct.ScmObj** %stackaddr$env-ref54624
%stackaddr$prim54625 = alloca %struct.ScmObj*, align 8
%_95k47391 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53557)
store volatile %struct.ScmObj* %_95k47391, %struct.ScmObj** %stackaddr$prim54625, align 8
%stackaddr$prim54626 = alloca %struct.ScmObj*, align 8
%current_45args53558 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53557)
store volatile %struct.ScmObj* %current_45args53558, %struct.ScmObj** %stackaddr$prim54626, align 8
%stackaddr$prim54627 = alloca %struct.ScmObj*, align 8
%anf_45bind47285 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53558)
store volatile %struct.ScmObj* %anf_45bind47285, %struct.ScmObj** %stackaddr$prim54627, align 8
%stackaddr$makeclosure54628 = alloca %struct.ScmObj*, align 8
%fptrToInt54629 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50307 to i64
%ae50307 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54629)
store volatile %struct.ScmObj* %ae50307, %struct.ScmObj** %stackaddr$makeclosure54628, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50307, %struct.ScmObj* %lst47140, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50307, %struct.ScmObj* %v47139, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50307, %struct.ScmObj* %k47390, i64 2)
%stackaddr$makeclosure54630 = alloca %struct.ScmObj*, align 8
%fptrToInt54631 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50308 to i64
%ae50308 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54631)
store volatile %struct.ScmObj* %ae50308, %struct.ScmObj** %stackaddr$makeclosure54630, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50308, %struct.ScmObj* %lst47140, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50308, %struct.ScmObj* %v47139, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50308, %struct.ScmObj* %k47390, i64 2)
%argslist53572$anf_45bind472850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54632 = alloca %struct.ScmObj*, align 8
%argslist53572$anf_45bind472851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50308, %struct.ScmObj* %argslist53572$anf_45bind472850)
store volatile %struct.ScmObj* %argslist53572$anf_45bind472851, %struct.ScmObj** %stackaddr$prim54632, align 8
%stackaddr$prim54633 = alloca %struct.ScmObj*, align 8
%argslist53572$anf_45bind472852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50307, %struct.ScmObj* %argslist53572$anf_45bind472851)
store volatile %struct.ScmObj* %argslist53572$anf_45bind472852, %struct.ScmObj** %stackaddr$prim54633, align 8
%clofunc54634 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47285)
musttail call tailcc void %clofunc54634(%struct.ScmObj* %anf_45bind47285, %struct.ScmObj* %argslist53572$anf_45bind472852)
ret void
}

define tailcc void @proc_clo$ae50307(%struct.ScmObj* %env$ae50307,%struct.ScmObj* %current_45args53560) {
%stackaddr$env-ref54635 = alloca %struct.ScmObj*, align 8
%lst47140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50307, i64 0)
store %struct.ScmObj* %lst47140, %struct.ScmObj** %stackaddr$env-ref54635
%stackaddr$env-ref54636 = alloca %struct.ScmObj*, align 8
%v47139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50307, i64 1)
store %struct.ScmObj* %v47139, %struct.ScmObj** %stackaddr$env-ref54636
%stackaddr$env-ref54637 = alloca %struct.ScmObj*, align 8
%k47390 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50307, i64 2)
store %struct.ScmObj* %k47390, %struct.ScmObj** %stackaddr$env-ref54637
%stackaddr$prim54638 = alloca %struct.ScmObj*, align 8
%_95k47392 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53560)
store volatile %struct.ScmObj* %_95k47392, %struct.ScmObj** %stackaddr$prim54638, align 8
%stackaddr$prim54639 = alloca %struct.ScmObj*, align 8
%current_45args53561 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53560)
store volatile %struct.ScmObj* %current_45args53561, %struct.ScmObj** %stackaddr$prim54639, align 8
%stackaddr$prim54640 = alloca %struct.ScmObj*, align 8
%cc47141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53561)
store volatile %struct.ScmObj* %cc47141, %struct.ScmObj** %stackaddr$prim54640, align 8
%ae50416 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54641 = alloca %struct.ScmObj*, align 8
%anf_45bind47286 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50416)
store volatile %struct.ScmObj* %anf_45bind47286, %struct.ScmObj** %stackaddr$prim54641, align 8
%stackaddr$prim54642 = alloca %struct.ScmObj*, align 8
%anf_45bind47287 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47286)
store volatile %struct.ScmObj* %anf_45bind47287, %struct.ScmObj** %stackaddr$prim54642, align 8
%truthy$cmp54643 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47287)
%cmp$cmp54643 = icmp eq i64 %truthy$cmp54643, 1
br i1 %cmp$cmp54643, label %truebranch$cmp54643, label %falsebranch$cmp54643
truebranch$cmp54643:
%ae50420 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50421 = call %struct.ScmObj* @const_init_false()
%argslist53563$k473900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54644 = alloca %struct.ScmObj*, align 8
%argslist53563$k473901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50421, %struct.ScmObj* %argslist53563$k473900)
store volatile %struct.ScmObj* %argslist53563$k473901, %struct.ScmObj** %stackaddr$prim54644, align 8
%stackaddr$prim54645 = alloca %struct.ScmObj*, align 8
%argslist53563$k473902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50420, %struct.ScmObj* %argslist53563$k473901)
store volatile %struct.ScmObj* %argslist53563$k473902, %struct.ScmObj** %stackaddr$prim54645, align 8
%clofunc54646 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47390)
musttail call tailcc void %clofunc54646(%struct.ScmObj* %k47390, %struct.ScmObj* %argslist53563$k473902)
ret void
falsebranch$cmp54643:
%ae50429 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54647 = alloca %struct.ScmObj*, align 8
%anf_45bind47288 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50429)
store volatile %struct.ScmObj* %anf_45bind47288, %struct.ScmObj** %stackaddr$prim54647, align 8
%stackaddr$prim54648 = alloca %struct.ScmObj*, align 8
%anf_45bind47289 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47288)
store volatile %struct.ScmObj* %anf_45bind47289, %struct.ScmObj** %stackaddr$prim54648, align 8
%stackaddr$prim54649 = alloca %struct.ScmObj*, align 8
%anf_45bind47290 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind47289, %struct.ScmObj* %v47139)
store volatile %struct.ScmObj* %anf_45bind47290, %struct.ScmObj** %stackaddr$prim54649, align 8
%truthy$cmp54650 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47290)
%cmp$cmp54650 = icmp eq i64 %truthy$cmp54650, 1
br i1 %cmp$cmp54650, label %truebranch$cmp54650, label %falsebranch$cmp54650
truebranch$cmp54650:
%ae50435 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54651 = alloca %struct.ScmObj*, align 8
%cpsprim47393 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50435)
store volatile %struct.ScmObj* %cpsprim47393, %struct.ScmObj** %stackaddr$prim54651, align 8
%ae50437 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53564$k473900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54652 = alloca %struct.ScmObj*, align 8
%argslist53564$k473901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47393, %struct.ScmObj* %argslist53564$k473900)
store volatile %struct.ScmObj* %argslist53564$k473901, %struct.ScmObj** %stackaddr$prim54652, align 8
%stackaddr$prim54653 = alloca %struct.ScmObj*, align 8
%argslist53564$k473902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50437, %struct.ScmObj* %argslist53564$k473901)
store volatile %struct.ScmObj* %argslist53564$k473902, %struct.ScmObj** %stackaddr$prim54653, align 8
%clofunc54654 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47390)
musttail call tailcc void %clofunc54654(%struct.ScmObj* %k47390, %struct.ScmObj* %argslist53564$k473902)
ret void
falsebranch$cmp54650:
%ae50448 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54655 = alloca %struct.ScmObj*, align 8
%anf_45bind47291 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50448)
store volatile %struct.ScmObj* %anf_45bind47291, %struct.ScmObj** %stackaddr$prim54655, align 8
%stackaddr$prim54656 = alloca %struct.ScmObj*, align 8
%anf_45bind47292 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47291)
store volatile %struct.ScmObj* %anf_45bind47292, %struct.ScmObj** %stackaddr$prim54656, align 8
%ae50451 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54657 = alloca %struct.ScmObj*, align 8
%_95047143 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50451, %struct.ScmObj* %anf_45bind47292)
store volatile %struct.ScmObj* %_95047143, %struct.ScmObj** %stackaddr$prim54657, align 8
%argslist53565$cc471410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54658 = alloca %struct.ScmObj*, align 8
%argslist53565$cc471411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47141, %struct.ScmObj* %argslist53565$cc471410)
store volatile %struct.ScmObj* %argslist53565$cc471411, %struct.ScmObj** %stackaddr$prim54658, align 8
%stackaddr$prim54659 = alloca %struct.ScmObj*, align 8
%argslist53565$cc471412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47390, %struct.ScmObj* %argslist53565$cc471411)
store volatile %struct.ScmObj* %argslist53565$cc471412, %struct.ScmObj** %stackaddr$prim54659, align 8
%clofunc54660 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47141)
musttail call tailcc void %clofunc54660(%struct.ScmObj* %cc47141, %struct.ScmObj* %argslist53565$cc471412)
ret void
}

define tailcc void @proc_clo$ae50308(%struct.ScmObj* %env$ae50308,%struct.ScmObj* %current_45args53566) {
%stackaddr$env-ref54661 = alloca %struct.ScmObj*, align 8
%lst47140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50308, i64 0)
store %struct.ScmObj* %lst47140, %struct.ScmObj** %stackaddr$env-ref54661
%stackaddr$env-ref54662 = alloca %struct.ScmObj*, align 8
%v47139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50308, i64 1)
store %struct.ScmObj* %v47139, %struct.ScmObj** %stackaddr$env-ref54662
%stackaddr$env-ref54663 = alloca %struct.ScmObj*, align 8
%k47390 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50308, i64 2)
store %struct.ScmObj* %k47390, %struct.ScmObj** %stackaddr$env-ref54663
%stackaddr$prim54664 = alloca %struct.ScmObj*, align 8
%_95k47392 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53566)
store volatile %struct.ScmObj* %_95k47392, %struct.ScmObj** %stackaddr$prim54664, align 8
%stackaddr$prim54665 = alloca %struct.ScmObj*, align 8
%current_45args53567 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53566)
store volatile %struct.ScmObj* %current_45args53567, %struct.ScmObj** %stackaddr$prim54665, align 8
%stackaddr$prim54666 = alloca %struct.ScmObj*, align 8
%cc47141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53567)
store volatile %struct.ScmObj* %cc47141, %struct.ScmObj** %stackaddr$prim54666, align 8
%ae50310 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54667 = alloca %struct.ScmObj*, align 8
%anf_45bind47286 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50310)
store volatile %struct.ScmObj* %anf_45bind47286, %struct.ScmObj** %stackaddr$prim54667, align 8
%stackaddr$prim54668 = alloca %struct.ScmObj*, align 8
%anf_45bind47287 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47286)
store volatile %struct.ScmObj* %anf_45bind47287, %struct.ScmObj** %stackaddr$prim54668, align 8
%truthy$cmp54669 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47287)
%cmp$cmp54669 = icmp eq i64 %truthy$cmp54669, 1
br i1 %cmp$cmp54669, label %truebranch$cmp54669, label %falsebranch$cmp54669
truebranch$cmp54669:
%ae50314 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50315 = call %struct.ScmObj* @const_init_false()
%argslist53569$k473900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54670 = alloca %struct.ScmObj*, align 8
%argslist53569$k473901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50315, %struct.ScmObj* %argslist53569$k473900)
store volatile %struct.ScmObj* %argslist53569$k473901, %struct.ScmObj** %stackaddr$prim54670, align 8
%stackaddr$prim54671 = alloca %struct.ScmObj*, align 8
%argslist53569$k473902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50314, %struct.ScmObj* %argslist53569$k473901)
store volatile %struct.ScmObj* %argslist53569$k473902, %struct.ScmObj** %stackaddr$prim54671, align 8
%clofunc54672 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47390)
musttail call tailcc void %clofunc54672(%struct.ScmObj* %k47390, %struct.ScmObj* %argslist53569$k473902)
ret void
falsebranch$cmp54669:
%ae50323 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54673 = alloca %struct.ScmObj*, align 8
%anf_45bind47288 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50323)
store volatile %struct.ScmObj* %anf_45bind47288, %struct.ScmObj** %stackaddr$prim54673, align 8
%stackaddr$prim54674 = alloca %struct.ScmObj*, align 8
%anf_45bind47289 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47288)
store volatile %struct.ScmObj* %anf_45bind47289, %struct.ScmObj** %stackaddr$prim54674, align 8
%stackaddr$prim54675 = alloca %struct.ScmObj*, align 8
%anf_45bind47290 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind47289, %struct.ScmObj* %v47139)
store volatile %struct.ScmObj* %anf_45bind47290, %struct.ScmObj** %stackaddr$prim54675, align 8
%truthy$cmp54676 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47290)
%cmp$cmp54676 = icmp eq i64 %truthy$cmp54676, 1
br i1 %cmp$cmp54676, label %truebranch$cmp54676, label %falsebranch$cmp54676
truebranch$cmp54676:
%ae50329 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54677 = alloca %struct.ScmObj*, align 8
%cpsprim47393 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50329)
store volatile %struct.ScmObj* %cpsprim47393, %struct.ScmObj** %stackaddr$prim54677, align 8
%ae50331 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53570$k473900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54678 = alloca %struct.ScmObj*, align 8
%argslist53570$k473901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47393, %struct.ScmObj* %argslist53570$k473900)
store volatile %struct.ScmObj* %argslist53570$k473901, %struct.ScmObj** %stackaddr$prim54678, align 8
%stackaddr$prim54679 = alloca %struct.ScmObj*, align 8
%argslist53570$k473902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50331, %struct.ScmObj* %argslist53570$k473901)
store volatile %struct.ScmObj* %argslist53570$k473902, %struct.ScmObj** %stackaddr$prim54679, align 8
%clofunc54680 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47390)
musttail call tailcc void %clofunc54680(%struct.ScmObj* %k47390, %struct.ScmObj* %argslist53570$k473902)
ret void
falsebranch$cmp54676:
%ae50342 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54681 = alloca %struct.ScmObj*, align 8
%anf_45bind47291 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50342)
store volatile %struct.ScmObj* %anf_45bind47291, %struct.ScmObj** %stackaddr$prim54681, align 8
%stackaddr$prim54682 = alloca %struct.ScmObj*, align 8
%anf_45bind47292 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47291)
store volatile %struct.ScmObj* %anf_45bind47292, %struct.ScmObj** %stackaddr$prim54682, align 8
%ae50345 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54683 = alloca %struct.ScmObj*, align 8
%_95047143 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50345, %struct.ScmObj* %anf_45bind47292)
store volatile %struct.ScmObj* %_95047143, %struct.ScmObj** %stackaddr$prim54683, align 8
%argslist53571$cc471410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54684 = alloca %struct.ScmObj*, align 8
%argslist53571$cc471411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47141, %struct.ScmObj* %argslist53571$cc471410)
store volatile %struct.ScmObj* %argslist53571$cc471411, %struct.ScmObj** %stackaddr$prim54684, align 8
%stackaddr$prim54685 = alloca %struct.ScmObj*, align 8
%argslist53571$cc471412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47390, %struct.ScmObj* %argslist53571$cc471411)
store volatile %struct.ScmObj* %argslist53571$cc471412, %struct.ScmObj** %stackaddr$prim54685, align 8
%clofunc54686 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47141)
musttail call tailcc void %clofunc54686(%struct.ScmObj* %cc47141, %struct.ScmObj* %argslist53571$cc471412)
ret void
}

define tailcc void @proc_clo$ae50293(%struct.ScmObj* %env$ae50293,%struct.ScmObj* %current_45args53573) {
%stackaddr$prim54687 = alloca %struct.ScmObj*, align 8
%k47394 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53573)
store volatile %struct.ScmObj* %k47394, %struct.ScmObj** %stackaddr$prim54687, align 8
%stackaddr$prim54688 = alloca %struct.ScmObj*, align 8
%current_45args53574 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53573)
store volatile %struct.ScmObj* %current_45args53574, %struct.ScmObj** %stackaddr$prim54688, align 8
%stackaddr$prim54689 = alloca %struct.ScmObj*, align 8
%u47142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53574)
store volatile %struct.ScmObj* %u47142, %struct.ScmObj** %stackaddr$prim54689, align 8
%argslist53576$u471420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54690 = alloca %struct.ScmObj*, align 8
%argslist53576$u471421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u47142, %struct.ScmObj* %argslist53576$u471420)
store volatile %struct.ScmObj* %argslist53576$u471421, %struct.ScmObj** %stackaddr$prim54690, align 8
%stackaddr$prim54691 = alloca %struct.ScmObj*, align 8
%argslist53576$u471422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47394, %struct.ScmObj* %argslist53576$u471421)
store volatile %struct.ScmObj* %argslist53576$u471422, %struct.ScmObj** %stackaddr$prim54691, align 8
%clofunc54692 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u47142)
musttail call tailcc void %clofunc54692(%struct.ScmObj* %u47142, %struct.ScmObj* %argslist53576$u471422)
ret void
}

define tailcc void @proc_clo$ae49752(%struct.ScmObj* %env$ae49752,%struct.ScmObj* %current_45args53579) {
%stackaddr$prim54693 = alloca %struct.ScmObj*, align 8
%k47395 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53579)
store volatile %struct.ScmObj* %k47395, %struct.ScmObj** %stackaddr$prim54693, align 8
%stackaddr$prim54694 = alloca %struct.ScmObj*, align 8
%current_45args53580 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53579)
store volatile %struct.ScmObj* %current_45args53580, %struct.ScmObj** %stackaddr$prim54694, align 8
%stackaddr$prim54695 = alloca %struct.ScmObj*, align 8
%lst47146 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53580)
store volatile %struct.ScmObj* %lst47146, %struct.ScmObj** %stackaddr$prim54695, align 8
%stackaddr$prim54696 = alloca %struct.ScmObj*, align 8
%current_45args53581 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53580)
store volatile %struct.ScmObj* %current_45args53581, %struct.ScmObj** %stackaddr$prim54696, align 8
%stackaddr$prim54697 = alloca %struct.ScmObj*, align 8
%n47145 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53581)
store volatile %struct.ScmObj* %n47145, %struct.ScmObj** %stackaddr$prim54697, align 8
%ae49753 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54698 = alloca %struct.ScmObj*, align 8
%n47148 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49753, %struct.ScmObj* %n47145)
store volatile %struct.ScmObj* %n47148, %struct.ScmObj** %stackaddr$prim54698, align 8
%ae49755 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54699 = alloca %struct.ScmObj*, align 8
%lst47147 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49755, %struct.ScmObj* %lst47146)
store volatile %struct.ScmObj* %lst47147, %struct.ScmObj** %stackaddr$prim54699, align 8
%stackaddr$makeclosure54700 = alloca %struct.ScmObj*, align 8
%fptrToInt54701 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49757 to i64
%ae49757 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54701)
store volatile %struct.ScmObj* %ae49757, %struct.ScmObj** %stackaddr$makeclosure54700, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49757, %struct.ScmObj* %k47395, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49757, %struct.ScmObj* %n47148, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49757, %struct.ScmObj* %lst47147, i64 2)
%ae49758 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54702 = alloca %struct.ScmObj*, align 8
%fptrToInt54703 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49759 to i64
%ae49759 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54703)
store volatile %struct.ScmObj* %ae49759, %struct.ScmObj** %stackaddr$makeclosure54702, align 8
%argslist53601$ae497570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54704 = alloca %struct.ScmObj*, align 8
%argslist53601$ae497571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49759, %struct.ScmObj* %argslist53601$ae497570)
store volatile %struct.ScmObj* %argslist53601$ae497571, %struct.ScmObj** %stackaddr$prim54704, align 8
%stackaddr$prim54705 = alloca %struct.ScmObj*, align 8
%argslist53601$ae497572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49758, %struct.ScmObj* %argslist53601$ae497571)
store volatile %struct.ScmObj* %argslist53601$ae497572, %struct.ScmObj** %stackaddr$prim54705, align 8
%clofunc54706 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49757)
musttail call tailcc void %clofunc54706(%struct.ScmObj* %ae49757, %struct.ScmObj* %argslist53601$ae497572)
ret void
}

define tailcc void @proc_clo$ae49757(%struct.ScmObj* %env$ae49757,%struct.ScmObj* %current_45args53583) {
%stackaddr$env-ref54707 = alloca %struct.ScmObj*, align 8
%k47395 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49757, i64 0)
store %struct.ScmObj* %k47395, %struct.ScmObj** %stackaddr$env-ref54707
%stackaddr$env-ref54708 = alloca %struct.ScmObj*, align 8
%n47148 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49757, i64 1)
store %struct.ScmObj* %n47148, %struct.ScmObj** %stackaddr$env-ref54708
%stackaddr$env-ref54709 = alloca %struct.ScmObj*, align 8
%lst47147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49757, i64 2)
store %struct.ScmObj* %lst47147, %struct.ScmObj** %stackaddr$env-ref54709
%stackaddr$prim54710 = alloca %struct.ScmObj*, align 8
%_95k47396 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53583)
store volatile %struct.ScmObj* %_95k47396, %struct.ScmObj** %stackaddr$prim54710, align 8
%stackaddr$prim54711 = alloca %struct.ScmObj*, align 8
%current_45args53584 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53583)
store volatile %struct.ScmObj* %current_45args53584, %struct.ScmObj** %stackaddr$prim54711, align 8
%stackaddr$prim54712 = alloca %struct.ScmObj*, align 8
%anf_45bind47278 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53584)
store volatile %struct.ScmObj* %anf_45bind47278, %struct.ScmObj** %stackaddr$prim54712, align 8
%stackaddr$makeclosure54713 = alloca %struct.ScmObj*, align 8
%fptrToInt54714 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49773 to i64
%ae49773 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54714)
store volatile %struct.ScmObj* %ae49773, %struct.ScmObj** %stackaddr$makeclosure54713, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49773, %struct.ScmObj* %k47395, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49773, %struct.ScmObj* %n47148, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49773, %struct.ScmObj* %lst47147, i64 2)
%stackaddr$makeclosure54715 = alloca %struct.ScmObj*, align 8
%fptrToInt54716 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49774 to i64
%ae49774 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54716)
store volatile %struct.ScmObj* %ae49774, %struct.ScmObj** %stackaddr$makeclosure54715, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49774, %struct.ScmObj* %k47395, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49774, %struct.ScmObj* %n47148, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49774, %struct.ScmObj* %lst47147, i64 2)
%argslist53596$anf_45bind472780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54717 = alloca %struct.ScmObj*, align 8
%argslist53596$anf_45bind472781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49774, %struct.ScmObj* %argslist53596$anf_45bind472780)
store volatile %struct.ScmObj* %argslist53596$anf_45bind472781, %struct.ScmObj** %stackaddr$prim54717, align 8
%stackaddr$prim54718 = alloca %struct.ScmObj*, align 8
%argslist53596$anf_45bind472782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49773, %struct.ScmObj* %argslist53596$anf_45bind472781)
store volatile %struct.ScmObj* %argslist53596$anf_45bind472782, %struct.ScmObj** %stackaddr$prim54718, align 8
%clofunc54719 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47278)
musttail call tailcc void %clofunc54719(%struct.ScmObj* %anf_45bind47278, %struct.ScmObj* %argslist53596$anf_45bind472782)
ret void
}

define tailcc void @proc_clo$ae49773(%struct.ScmObj* %env$ae49773,%struct.ScmObj* %current_45args53586) {
%stackaddr$env-ref54720 = alloca %struct.ScmObj*, align 8
%k47395 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49773, i64 0)
store %struct.ScmObj* %k47395, %struct.ScmObj** %stackaddr$env-ref54720
%stackaddr$env-ref54721 = alloca %struct.ScmObj*, align 8
%n47148 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49773, i64 1)
store %struct.ScmObj* %n47148, %struct.ScmObj** %stackaddr$env-ref54721
%stackaddr$env-ref54722 = alloca %struct.ScmObj*, align 8
%lst47147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49773, i64 2)
store %struct.ScmObj* %lst47147, %struct.ScmObj** %stackaddr$env-ref54722
%stackaddr$prim54723 = alloca %struct.ScmObj*, align 8
%_95k47397 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53586)
store volatile %struct.ScmObj* %_95k47397, %struct.ScmObj** %stackaddr$prim54723, align 8
%stackaddr$prim54724 = alloca %struct.ScmObj*, align 8
%current_45args53587 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53586)
store volatile %struct.ScmObj* %current_45args53587, %struct.ScmObj** %stackaddr$prim54724, align 8
%stackaddr$prim54725 = alloca %struct.ScmObj*, align 8
%cc47149 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53587)
store volatile %struct.ScmObj* %cc47149, %struct.ScmObj** %stackaddr$prim54725, align 8
%ae49916 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54726 = alloca %struct.ScmObj*, align 8
%anf_45bind47279 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47148, %struct.ScmObj* %ae49916)
store volatile %struct.ScmObj* %anf_45bind47279, %struct.ScmObj** %stackaddr$prim54726, align 8
%ae49917 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54727 = alloca %struct.ScmObj*, align 8
%anf_45bind47280 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae49917, %struct.ScmObj* %anf_45bind47279)
store volatile %struct.ScmObj* %anf_45bind47280, %struct.ScmObj** %stackaddr$prim54727, align 8
%truthy$cmp54728 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47280)
%cmp$cmp54728 = icmp eq i64 %truthy$cmp54728, 1
br i1 %cmp$cmp54728, label %truebranch$cmp54728, label %falsebranch$cmp54728
truebranch$cmp54728:
%ae49921 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54729 = alloca %struct.ScmObj*, align 8
%cpsprim47398 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47147, %struct.ScmObj* %ae49921)
store volatile %struct.ScmObj* %cpsprim47398, %struct.ScmObj** %stackaddr$prim54729, align 8
%ae49923 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53589$k473950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54730 = alloca %struct.ScmObj*, align 8
%argslist53589$k473951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47398, %struct.ScmObj* %argslist53589$k473950)
store volatile %struct.ScmObj* %argslist53589$k473951, %struct.ScmObj** %stackaddr$prim54730, align 8
%stackaddr$prim54731 = alloca %struct.ScmObj*, align 8
%argslist53589$k473952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49923, %struct.ScmObj* %argslist53589$k473951)
store volatile %struct.ScmObj* %argslist53589$k473952, %struct.ScmObj** %stackaddr$prim54731, align 8
%clofunc54732 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47395)
musttail call tailcc void %clofunc54732(%struct.ScmObj* %k47395, %struct.ScmObj* %argslist53589$k473952)
ret void
falsebranch$cmp54728:
%ae49934 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54733 = alloca %struct.ScmObj*, align 8
%anf_45bind47281 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47147, %struct.ScmObj* %ae49934)
store volatile %struct.ScmObj* %anf_45bind47281, %struct.ScmObj** %stackaddr$prim54733, align 8
%stackaddr$prim54734 = alloca %struct.ScmObj*, align 8
%anf_45bind47282 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47281)
store volatile %struct.ScmObj* %anf_45bind47282, %struct.ScmObj** %stackaddr$prim54734, align 8
%ae49937 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54735 = alloca %struct.ScmObj*, align 8
%_95047152 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47147, %struct.ScmObj* %ae49937, %struct.ScmObj* %anf_45bind47282)
store volatile %struct.ScmObj* %_95047152, %struct.ScmObj** %stackaddr$prim54735, align 8
%ae49940 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54736 = alloca %struct.ScmObj*, align 8
%anf_45bind47283 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47148, %struct.ScmObj* %ae49940)
store volatile %struct.ScmObj* %anf_45bind47283, %struct.ScmObj** %stackaddr$prim54736, align 8
%ae49942 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54737 = alloca %struct.ScmObj*, align 8
%anf_45bind47284 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind47283, %struct.ScmObj* %ae49942)
store volatile %struct.ScmObj* %anf_45bind47284, %struct.ScmObj** %stackaddr$prim54737, align 8
%ae49944 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54738 = alloca %struct.ScmObj*, align 8
%_95147151 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n47148, %struct.ScmObj* %ae49944, %struct.ScmObj* %anf_45bind47284)
store volatile %struct.ScmObj* %_95147151, %struct.ScmObj** %stackaddr$prim54738, align 8
%argslist53590$cc471490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54739 = alloca %struct.ScmObj*, align 8
%argslist53590$cc471491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47149, %struct.ScmObj* %argslist53590$cc471490)
store volatile %struct.ScmObj* %argslist53590$cc471491, %struct.ScmObj** %stackaddr$prim54739, align 8
%stackaddr$prim54740 = alloca %struct.ScmObj*, align 8
%argslist53590$cc471492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47395, %struct.ScmObj* %argslist53590$cc471491)
store volatile %struct.ScmObj* %argslist53590$cc471492, %struct.ScmObj** %stackaddr$prim54740, align 8
%clofunc54741 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47149)
musttail call tailcc void %clofunc54741(%struct.ScmObj* %cc47149, %struct.ScmObj* %argslist53590$cc471492)
ret void
}

define tailcc void @proc_clo$ae49774(%struct.ScmObj* %env$ae49774,%struct.ScmObj* %current_45args53591) {
%stackaddr$env-ref54742 = alloca %struct.ScmObj*, align 8
%k47395 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49774, i64 0)
store %struct.ScmObj* %k47395, %struct.ScmObj** %stackaddr$env-ref54742
%stackaddr$env-ref54743 = alloca %struct.ScmObj*, align 8
%n47148 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49774, i64 1)
store %struct.ScmObj* %n47148, %struct.ScmObj** %stackaddr$env-ref54743
%stackaddr$env-ref54744 = alloca %struct.ScmObj*, align 8
%lst47147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49774, i64 2)
store %struct.ScmObj* %lst47147, %struct.ScmObj** %stackaddr$env-ref54744
%stackaddr$prim54745 = alloca %struct.ScmObj*, align 8
%_95k47397 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53591)
store volatile %struct.ScmObj* %_95k47397, %struct.ScmObj** %stackaddr$prim54745, align 8
%stackaddr$prim54746 = alloca %struct.ScmObj*, align 8
%current_45args53592 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53591)
store volatile %struct.ScmObj* %current_45args53592, %struct.ScmObj** %stackaddr$prim54746, align 8
%stackaddr$prim54747 = alloca %struct.ScmObj*, align 8
%cc47149 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53592)
store volatile %struct.ScmObj* %cc47149, %struct.ScmObj** %stackaddr$prim54747, align 8
%ae49776 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54748 = alloca %struct.ScmObj*, align 8
%anf_45bind47279 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47148, %struct.ScmObj* %ae49776)
store volatile %struct.ScmObj* %anf_45bind47279, %struct.ScmObj** %stackaddr$prim54748, align 8
%ae49777 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54749 = alloca %struct.ScmObj*, align 8
%anf_45bind47280 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae49777, %struct.ScmObj* %anf_45bind47279)
store volatile %struct.ScmObj* %anf_45bind47280, %struct.ScmObj** %stackaddr$prim54749, align 8
%truthy$cmp54750 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47280)
%cmp$cmp54750 = icmp eq i64 %truthy$cmp54750, 1
br i1 %cmp$cmp54750, label %truebranch$cmp54750, label %falsebranch$cmp54750
truebranch$cmp54750:
%ae49781 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54751 = alloca %struct.ScmObj*, align 8
%cpsprim47398 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47147, %struct.ScmObj* %ae49781)
store volatile %struct.ScmObj* %cpsprim47398, %struct.ScmObj** %stackaddr$prim54751, align 8
%ae49783 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53594$k473950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54752 = alloca %struct.ScmObj*, align 8
%argslist53594$k473951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47398, %struct.ScmObj* %argslist53594$k473950)
store volatile %struct.ScmObj* %argslist53594$k473951, %struct.ScmObj** %stackaddr$prim54752, align 8
%stackaddr$prim54753 = alloca %struct.ScmObj*, align 8
%argslist53594$k473952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49783, %struct.ScmObj* %argslist53594$k473951)
store volatile %struct.ScmObj* %argslist53594$k473952, %struct.ScmObj** %stackaddr$prim54753, align 8
%clofunc54754 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47395)
musttail call tailcc void %clofunc54754(%struct.ScmObj* %k47395, %struct.ScmObj* %argslist53594$k473952)
ret void
falsebranch$cmp54750:
%ae49794 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54755 = alloca %struct.ScmObj*, align 8
%anf_45bind47281 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47147, %struct.ScmObj* %ae49794)
store volatile %struct.ScmObj* %anf_45bind47281, %struct.ScmObj** %stackaddr$prim54755, align 8
%stackaddr$prim54756 = alloca %struct.ScmObj*, align 8
%anf_45bind47282 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47281)
store volatile %struct.ScmObj* %anf_45bind47282, %struct.ScmObj** %stackaddr$prim54756, align 8
%ae49797 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54757 = alloca %struct.ScmObj*, align 8
%_95047152 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47147, %struct.ScmObj* %ae49797, %struct.ScmObj* %anf_45bind47282)
store volatile %struct.ScmObj* %_95047152, %struct.ScmObj** %stackaddr$prim54757, align 8
%ae49800 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54758 = alloca %struct.ScmObj*, align 8
%anf_45bind47283 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47148, %struct.ScmObj* %ae49800)
store volatile %struct.ScmObj* %anf_45bind47283, %struct.ScmObj** %stackaddr$prim54758, align 8
%ae49802 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54759 = alloca %struct.ScmObj*, align 8
%anf_45bind47284 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind47283, %struct.ScmObj* %ae49802)
store volatile %struct.ScmObj* %anf_45bind47284, %struct.ScmObj** %stackaddr$prim54759, align 8
%ae49804 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54760 = alloca %struct.ScmObj*, align 8
%_95147151 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n47148, %struct.ScmObj* %ae49804, %struct.ScmObj* %anf_45bind47284)
store volatile %struct.ScmObj* %_95147151, %struct.ScmObj** %stackaddr$prim54760, align 8
%argslist53595$cc471490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54761 = alloca %struct.ScmObj*, align 8
%argslist53595$cc471491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47149, %struct.ScmObj* %argslist53595$cc471490)
store volatile %struct.ScmObj* %argslist53595$cc471491, %struct.ScmObj** %stackaddr$prim54761, align 8
%stackaddr$prim54762 = alloca %struct.ScmObj*, align 8
%argslist53595$cc471492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47395, %struct.ScmObj* %argslist53595$cc471491)
store volatile %struct.ScmObj* %argslist53595$cc471492, %struct.ScmObj** %stackaddr$prim54762, align 8
%clofunc54763 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47149)
musttail call tailcc void %clofunc54763(%struct.ScmObj* %cc47149, %struct.ScmObj* %argslist53595$cc471492)
ret void
}

define tailcc void @proc_clo$ae49759(%struct.ScmObj* %env$ae49759,%struct.ScmObj* %current_45args53597) {
%stackaddr$prim54764 = alloca %struct.ScmObj*, align 8
%k47399 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53597)
store volatile %struct.ScmObj* %k47399, %struct.ScmObj** %stackaddr$prim54764, align 8
%stackaddr$prim54765 = alloca %struct.ScmObj*, align 8
%current_45args53598 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53597)
store volatile %struct.ScmObj* %current_45args53598, %struct.ScmObj** %stackaddr$prim54765, align 8
%stackaddr$prim54766 = alloca %struct.ScmObj*, align 8
%u47150 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53598)
store volatile %struct.ScmObj* %u47150, %struct.ScmObj** %stackaddr$prim54766, align 8
%argslist53600$u471500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54767 = alloca %struct.ScmObj*, align 8
%argslist53600$u471501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u47150, %struct.ScmObj* %argslist53600$u471500)
store volatile %struct.ScmObj* %argslist53600$u471501, %struct.ScmObj** %stackaddr$prim54767, align 8
%stackaddr$prim54768 = alloca %struct.ScmObj*, align 8
%argslist53600$u471502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47399, %struct.ScmObj* %argslist53600$u471501)
store volatile %struct.ScmObj* %argslist53600$u471502, %struct.ScmObj** %stackaddr$prim54768, align 8
%clofunc54769 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u47150)
musttail call tailcc void %clofunc54769(%struct.ScmObj* %u47150, %struct.ScmObj* %argslist53600$u471502)
ret void
}

define tailcc void @proc_clo$ae49336(%struct.ScmObj* %env$ae49336,%struct.ScmObj* %current_45args53603) {
%stackaddr$prim54770 = alloca %struct.ScmObj*, align 8
%k47400 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53603)
store volatile %struct.ScmObj* %k47400, %struct.ScmObj** %stackaddr$prim54770, align 8
%stackaddr$prim54771 = alloca %struct.ScmObj*, align 8
%current_45args53604 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53603)
store volatile %struct.ScmObj* %current_45args53604, %struct.ScmObj** %stackaddr$prim54771, align 8
%stackaddr$prim54772 = alloca %struct.ScmObj*, align 8
%a47154 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53604)
store volatile %struct.ScmObj* %a47154, %struct.ScmObj** %stackaddr$prim54772, align 8
%ae49337 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54773 = alloca %struct.ScmObj*, align 8
%a47155 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49337, %struct.ScmObj* %a47154)
store volatile %struct.ScmObj* %a47155, %struct.ScmObj** %stackaddr$prim54773, align 8
%stackaddr$makeclosure54774 = alloca %struct.ScmObj*, align 8
%fptrToInt54775 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49339 to i64
%ae49339 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54775)
store volatile %struct.ScmObj* %ae49339, %struct.ScmObj** %stackaddr$makeclosure54774, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49339, %struct.ScmObj* %a47155, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49339, %struct.ScmObj* %k47400, i64 1)
%ae49340 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54776 = alloca %struct.ScmObj*, align 8
%fptrToInt54777 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49341 to i64
%ae49341 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54777)
store volatile %struct.ScmObj* %ae49341, %struct.ScmObj** %stackaddr$makeclosure54776, align 8
%argslist53626$ae493390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54778 = alloca %struct.ScmObj*, align 8
%argslist53626$ae493391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49341, %struct.ScmObj* %argslist53626$ae493390)
store volatile %struct.ScmObj* %argslist53626$ae493391, %struct.ScmObj** %stackaddr$prim54778, align 8
%stackaddr$prim54779 = alloca %struct.ScmObj*, align 8
%argslist53626$ae493392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49340, %struct.ScmObj* %argslist53626$ae493391)
store volatile %struct.ScmObj* %argslist53626$ae493392, %struct.ScmObj** %stackaddr$prim54779, align 8
%clofunc54780 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49339)
musttail call tailcc void %clofunc54780(%struct.ScmObj* %ae49339, %struct.ScmObj* %argslist53626$ae493392)
ret void
}

define tailcc void @proc_clo$ae49339(%struct.ScmObj* %env$ae49339,%struct.ScmObj* %current_45args53606) {
%stackaddr$env-ref54781 = alloca %struct.ScmObj*, align 8
%a47155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49339, i64 0)
store %struct.ScmObj* %a47155, %struct.ScmObj** %stackaddr$env-ref54781
%stackaddr$env-ref54782 = alloca %struct.ScmObj*, align 8
%k47400 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49339, i64 1)
store %struct.ScmObj* %k47400, %struct.ScmObj** %stackaddr$env-ref54782
%stackaddr$prim54783 = alloca %struct.ScmObj*, align 8
%_95k47401 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53606)
store volatile %struct.ScmObj* %_95k47401, %struct.ScmObj** %stackaddr$prim54783, align 8
%stackaddr$prim54784 = alloca %struct.ScmObj*, align 8
%current_45args53607 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53606)
store volatile %struct.ScmObj* %current_45args53607, %struct.ScmObj** %stackaddr$prim54784, align 8
%stackaddr$prim54785 = alloca %struct.ScmObj*, align 8
%anf_45bind47270 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53607)
store volatile %struct.ScmObj* %anf_45bind47270, %struct.ScmObj** %stackaddr$prim54785, align 8
%stackaddr$makeclosure54786 = alloca %struct.ScmObj*, align 8
%fptrToInt54787 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49358 to i64
%ae49358 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54787)
store volatile %struct.ScmObj* %ae49358, %struct.ScmObj** %stackaddr$makeclosure54786, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49358, %struct.ScmObj* %a47155, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49358, %struct.ScmObj* %k47400, i64 1)
%stackaddr$makeclosure54788 = alloca %struct.ScmObj*, align 8
%fptrToInt54789 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49359 to i64
%ae49359 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54789)
store volatile %struct.ScmObj* %ae49359, %struct.ScmObj** %stackaddr$makeclosure54788, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49359, %struct.ScmObj* %a47155, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49359, %struct.ScmObj* %k47400, i64 1)
%argslist53621$anf_45bind472700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54790 = alloca %struct.ScmObj*, align 8
%argslist53621$anf_45bind472701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49359, %struct.ScmObj* %argslist53621$anf_45bind472700)
store volatile %struct.ScmObj* %argslist53621$anf_45bind472701, %struct.ScmObj** %stackaddr$prim54790, align 8
%stackaddr$prim54791 = alloca %struct.ScmObj*, align 8
%argslist53621$anf_45bind472702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49358, %struct.ScmObj* %argslist53621$anf_45bind472701)
store volatile %struct.ScmObj* %argslist53621$anf_45bind472702, %struct.ScmObj** %stackaddr$prim54791, align 8
%clofunc54792 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47270)
musttail call tailcc void %clofunc54792(%struct.ScmObj* %anf_45bind47270, %struct.ScmObj* %argslist53621$anf_45bind472702)
ret void
}

define tailcc void @proc_clo$ae49358(%struct.ScmObj* %env$ae49358,%struct.ScmObj* %current_45args53609) {
%stackaddr$env-ref54793 = alloca %struct.ScmObj*, align 8
%a47155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49358, i64 0)
store %struct.ScmObj* %a47155, %struct.ScmObj** %stackaddr$env-ref54793
%stackaddr$env-ref54794 = alloca %struct.ScmObj*, align 8
%k47400 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49358, i64 1)
store %struct.ScmObj* %k47400, %struct.ScmObj** %stackaddr$env-ref54794
%stackaddr$prim54795 = alloca %struct.ScmObj*, align 8
%_95k47402 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53609)
store volatile %struct.ScmObj* %_95k47402, %struct.ScmObj** %stackaddr$prim54795, align 8
%stackaddr$prim54796 = alloca %struct.ScmObj*, align 8
%current_45args53610 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53609)
store volatile %struct.ScmObj* %current_45args53610, %struct.ScmObj** %stackaddr$prim54796, align 8
%stackaddr$prim54797 = alloca %struct.ScmObj*, align 8
%cc47156 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53610)
store volatile %struct.ScmObj* %cc47156, %struct.ScmObj** %stackaddr$prim54797, align 8
%ae49474 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54798 = alloca %struct.ScmObj*, align 8
%anf_45bind47271 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49474)
store volatile %struct.ScmObj* %anf_45bind47271, %struct.ScmObj** %stackaddr$prim54798, align 8
%stackaddr$prim54799 = alloca %struct.ScmObj*, align 8
%anf_45bind47272 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47271)
store volatile %struct.ScmObj* %anf_45bind47272, %struct.ScmObj** %stackaddr$prim54799, align 8
%truthy$cmp54800 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47272)
%cmp$cmp54800 = icmp eq i64 %truthy$cmp54800, 1
br i1 %cmp$cmp54800, label %truebranch$cmp54800, label %falsebranch$cmp54800
truebranch$cmp54800:
%ae49478 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49479 = call %struct.ScmObj* @const_init_true()
%argslist53612$k474000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54801 = alloca %struct.ScmObj*, align 8
%argslist53612$k474001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49479, %struct.ScmObj* %argslist53612$k474000)
store volatile %struct.ScmObj* %argslist53612$k474001, %struct.ScmObj** %stackaddr$prim54801, align 8
%stackaddr$prim54802 = alloca %struct.ScmObj*, align 8
%argslist53612$k474002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49478, %struct.ScmObj* %argslist53612$k474001)
store volatile %struct.ScmObj* %argslist53612$k474002, %struct.ScmObj** %stackaddr$prim54802, align 8
%clofunc54803 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47400)
musttail call tailcc void %clofunc54803(%struct.ScmObj* %k47400, %struct.ScmObj* %argslist53612$k474002)
ret void
falsebranch$cmp54800:
%ae49487 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54804 = alloca %struct.ScmObj*, align 8
%anf_45bind47273 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49487)
store volatile %struct.ScmObj* %anf_45bind47273, %struct.ScmObj** %stackaddr$prim54804, align 8
%stackaddr$prim54805 = alloca %struct.ScmObj*, align 8
%anf_45bind47274 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind47273)
store volatile %struct.ScmObj* %anf_45bind47274, %struct.ScmObj** %stackaddr$prim54805, align 8
%truthy$cmp54806 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47274)
%cmp$cmp54806 = icmp eq i64 %truthy$cmp54806, 1
br i1 %cmp$cmp54806, label %truebranch$cmp54806, label %falsebranch$cmp54806
truebranch$cmp54806:
%ae49491 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54807 = alloca %struct.ScmObj*, align 8
%anf_45bind47275 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49491)
store volatile %struct.ScmObj* %anf_45bind47275, %struct.ScmObj** %stackaddr$prim54807, align 8
%stackaddr$prim54808 = alloca %struct.ScmObj*, align 8
%b47158 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47275)
store volatile %struct.ScmObj* %b47158, %struct.ScmObj** %stackaddr$prim54808, align 8
%ae49494 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54809 = alloca %struct.ScmObj*, align 8
%anf_45bind47276 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49494)
store volatile %struct.ScmObj* %anf_45bind47276, %struct.ScmObj** %stackaddr$prim54809, align 8
%stackaddr$prim54810 = alloca %struct.ScmObj*, align 8
%anf_45bind47277 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47276)
store volatile %struct.ScmObj* %anf_45bind47277, %struct.ScmObj** %stackaddr$prim54810, align 8
%ae49497 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54811 = alloca %struct.ScmObj*, align 8
%_95047159 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49497, %struct.ScmObj* %anf_45bind47277)
store volatile %struct.ScmObj* %_95047159, %struct.ScmObj** %stackaddr$prim54811, align 8
%argslist53613$cc471560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54812 = alloca %struct.ScmObj*, align 8
%argslist53613$cc471561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47156, %struct.ScmObj* %argslist53613$cc471560)
store volatile %struct.ScmObj* %argslist53613$cc471561, %struct.ScmObj** %stackaddr$prim54812, align 8
%stackaddr$prim54813 = alloca %struct.ScmObj*, align 8
%argslist53613$cc471562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47400, %struct.ScmObj* %argslist53613$cc471561)
store volatile %struct.ScmObj* %argslist53613$cc471562, %struct.ScmObj** %stackaddr$prim54813, align 8
%clofunc54814 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47156)
musttail call tailcc void %clofunc54814(%struct.ScmObj* %cc47156, %struct.ScmObj* %argslist53613$cc471562)
ret void
falsebranch$cmp54806:
%ae49530 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49531 = call %struct.ScmObj* @const_init_false()
%argslist53614$k474000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54815 = alloca %struct.ScmObj*, align 8
%argslist53614$k474001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49531, %struct.ScmObj* %argslist53614$k474000)
store volatile %struct.ScmObj* %argslist53614$k474001, %struct.ScmObj** %stackaddr$prim54815, align 8
%stackaddr$prim54816 = alloca %struct.ScmObj*, align 8
%argslist53614$k474002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49530, %struct.ScmObj* %argslist53614$k474001)
store volatile %struct.ScmObj* %argslist53614$k474002, %struct.ScmObj** %stackaddr$prim54816, align 8
%clofunc54817 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47400)
musttail call tailcc void %clofunc54817(%struct.ScmObj* %k47400, %struct.ScmObj* %argslist53614$k474002)
ret void
}

define tailcc void @proc_clo$ae49359(%struct.ScmObj* %env$ae49359,%struct.ScmObj* %current_45args53615) {
%stackaddr$env-ref54818 = alloca %struct.ScmObj*, align 8
%a47155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49359, i64 0)
store %struct.ScmObj* %a47155, %struct.ScmObj** %stackaddr$env-ref54818
%stackaddr$env-ref54819 = alloca %struct.ScmObj*, align 8
%k47400 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49359, i64 1)
store %struct.ScmObj* %k47400, %struct.ScmObj** %stackaddr$env-ref54819
%stackaddr$prim54820 = alloca %struct.ScmObj*, align 8
%_95k47402 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53615)
store volatile %struct.ScmObj* %_95k47402, %struct.ScmObj** %stackaddr$prim54820, align 8
%stackaddr$prim54821 = alloca %struct.ScmObj*, align 8
%current_45args53616 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53615)
store volatile %struct.ScmObj* %current_45args53616, %struct.ScmObj** %stackaddr$prim54821, align 8
%stackaddr$prim54822 = alloca %struct.ScmObj*, align 8
%cc47156 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53616)
store volatile %struct.ScmObj* %cc47156, %struct.ScmObj** %stackaddr$prim54822, align 8
%ae49361 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54823 = alloca %struct.ScmObj*, align 8
%anf_45bind47271 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49361)
store volatile %struct.ScmObj* %anf_45bind47271, %struct.ScmObj** %stackaddr$prim54823, align 8
%stackaddr$prim54824 = alloca %struct.ScmObj*, align 8
%anf_45bind47272 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47271)
store volatile %struct.ScmObj* %anf_45bind47272, %struct.ScmObj** %stackaddr$prim54824, align 8
%truthy$cmp54825 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47272)
%cmp$cmp54825 = icmp eq i64 %truthy$cmp54825, 1
br i1 %cmp$cmp54825, label %truebranch$cmp54825, label %falsebranch$cmp54825
truebranch$cmp54825:
%ae49365 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49366 = call %struct.ScmObj* @const_init_true()
%argslist53618$k474000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54826 = alloca %struct.ScmObj*, align 8
%argslist53618$k474001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49366, %struct.ScmObj* %argslist53618$k474000)
store volatile %struct.ScmObj* %argslist53618$k474001, %struct.ScmObj** %stackaddr$prim54826, align 8
%stackaddr$prim54827 = alloca %struct.ScmObj*, align 8
%argslist53618$k474002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49365, %struct.ScmObj* %argslist53618$k474001)
store volatile %struct.ScmObj* %argslist53618$k474002, %struct.ScmObj** %stackaddr$prim54827, align 8
%clofunc54828 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47400)
musttail call tailcc void %clofunc54828(%struct.ScmObj* %k47400, %struct.ScmObj* %argslist53618$k474002)
ret void
falsebranch$cmp54825:
%ae49374 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54829 = alloca %struct.ScmObj*, align 8
%anf_45bind47273 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49374)
store volatile %struct.ScmObj* %anf_45bind47273, %struct.ScmObj** %stackaddr$prim54829, align 8
%stackaddr$prim54830 = alloca %struct.ScmObj*, align 8
%anf_45bind47274 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind47273)
store volatile %struct.ScmObj* %anf_45bind47274, %struct.ScmObj** %stackaddr$prim54830, align 8
%truthy$cmp54831 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47274)
%cmp$cmp54831 = icmp eq i64 %truthy$cmp54831, 1
br i1 %cmp$cmp54831, label %truebranch$cmp54831, label %falsebranch$cmp54831
truebranch$cmp54831:
%ae49378 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54832 = alloca %struct.ScmObj*, align 8
%anf_45bind47275 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49378)
store volatile %struct.ScmObj* %anf_45bind47275, %struct.ScmObj** %stackaddr$prim54832, align 8
%stackaddr$prim54833 = alloca %struct.ScmObj*, align 8
%b47158 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47275)
store volatile %struct.ScmObj* %b47158, %struct.ScmObj** %stackaddr$prim54833, align 8
%ae49381 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54834 = alloca %struct.ScmObj*, align 8
%anf_45bind47276 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49381)
store volatile %struct.ScmObj* %anf_45bind47276, %struct.ScmObj** %stackaddr$prim54834, align 8
%stackaddr$prim54835 = alloca %struct.ScmObj*, align 8
%anf_45bind47277 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47276)
store volatile %struct.ScmObj* %anf_45bind47277, %struct.ScmObj** %stackaddr$prim54835, align 8
%ae49384 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54836 = alloca %struct.ScmObj*, align 8
%_95047159 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49384, %struct.ScmObj* %anf_45bind47277)
store volatile %struct.ScmObj* %_95047159, %struct.ScmObj** %stackaddr$prim54836, align 8
%argslist53619$cc471560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54837 = alloca %struct.ScmObj*, align 8
%argslist53619$cc471561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47156, %struct.ScmObj* %argslist53619$cc471560)
store volatile %struct.ScmObj* %argslist53619$cc471561, %struct.ScmObj** %stackaddr$prim54837, align 8
%stackaddr$prim54838 = alloca %struct.ScmObj*, align 8
%argslist53619$cc471562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47400, %struct.ScmObj* %argslist53619$cc471561)
store volatile %struct.ScmObj* %argslist53619$cc471562, %struct.ScmObj** %stackaddr$prim54838, align 8
%clofunc54839 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47156)
musttail call tailcc void %clofunc54839(%struct.ScmObj* %cc47156, %struct.ScmObj* %argslist53619$cc471562)
ret void
falsebranch$cmp54831:
%ae49417 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49418 = call %struct.ScmObj* @const_init_false()
%argslist53620$k474000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54840 = alloca %struct.ScmObj*, align 8
%argslist53620$k474001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49418, %struct.ScmObj* %argslist53620$k474000)
store volatile %struct.ScmObj* %argslist53620$k474001, %struct.ScmObj** %stackaddr$prim54840, align 8
%stackaddr$prim54841 = alloca %struct.ScmObj*, align 8
%argslist53620$k474002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49417, %struct.ScmObj* %argslist53620$k474001)
store volatile %struct.ScmObj* %argslist53620$k474002, %struct.ScmObj** %stackaddr$prim54841, align 8
%clofunc54842 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47400)
musttail call tailcc void %clofunc54842(%struct.ScmObj* %k47400, %struct.ScmObj* %argslist53620$k474002)
ret void
}

define tailcc void @proc_clo$ae49341(%struct.ScmObj* %env$ae49341,%struct.ScmObj* %current_45args53622) {
%stackaddr$prim54843 = alloca %struct.ScmObj*, align 8
%k47403 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53622)
store volatile %struct.ScmObj* %k47403, %struct.ScmObj** %stackaddr$prim54843, align 8
%stackaddr$prim54844 = alloca %struct.ScmObj*, align 8
%current_45args53623 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53622)
store volatile %struct.ScmObj* %current_45args53623, %struct.ScmObj** %stackaddr$prim54844, align 8
%stackaddr$prim54845 = alloca %struct.ScmObj*, align 8
%k47157 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53623)
store volatile %struct.ScmObj* %k47157, %struct.ScmObj** %stackaddr$prim54845, align 8
%ae49343 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53625$k474030 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54846 = alloca %struct.ScmObj*, align 8
%argslist53625$k474031 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47157, %struct.ScmObj* %argslist53625$k474030)
store volatile %struct.ScmObj* %argslist53625$k474031, %struct.ScmObj** %stackaddr$prim54846, align 8
%stackaddr$prim54847 = alloca %struct.ScmObj*, align 8
%argslist53625$k474032 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49343, %struct.ScmObj* %argslist53625$k474031)
store volatile %struct.ScmObj* %argslist53625$k474032, %struct.ScmObj** %stackaddr$prim54847, align 8
%clofunc54848 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47403)
musttail call tailcc void %clofunc54848(%struct.ScmObj* %k47403, %struct.ScmObj* %argslist53625$k474032)
ret void
}

define tailcc void @proc_clo$ae49264(%struct.ScmObj* %env$ae49264,%struct.ScmObj* %current_45args53628) {
%stackaddr$env-ref54849 = alloca %struct.ScmObj*, align 8
%_37append47161 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49264, i64 0)
store %struct.ScmObj* %_37append47161, %struct.ScmObj** %stackaddr$env-ref54849
%stackaddr$prim54850 = alloca %struct.ScmObj*, align 8
%k47404 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53628)
store volatile %struct.ScmObj* %k47404, %struct.ScmObj** %stackaddr$prim54850, align 8
%stackaddr$prim54851 = alloca %struct.ScmObj*, align 8
%current_45args53629 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53628)
store volatile %struct.ScmObj* %current_45args53629, %struct.ScmObj** %stackaddr$prim54851, align 8
%stackaddr$prim54852 = alloca %struct.ScmObj*, align 8
%ls047164 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53629)
store volatile %struct.ScmObj* %ls047164, %struct.ScmObj** %stackaddr$prim54852, align 8
%stackaddr$prim54853 = alloca %struct.ScmObj*, align 8
%current_45args53630 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53629)
store volatile %struct.ScmObj* %current_45args53630, %struct.ScmObj** %stackaddr$prim54853, align 8
%stackaddr$prim54854 = alloca %struct.ScmObj*, align 8
%ls147163 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53630)
store volatile %struct.ScmObj* %ls147163, %struct.ScmObj** %stackaddr$prim54854, align 8
%stackaddr$prim54855 = alloca %struct.ScmObj*, align 8
%anf_45bind47264 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls047164)
store volatile %struct.ScmObj* %anf_45bind47264, %struct.ScmObj** %stackaddr$prim54855, align 8
%truthy$cmp54856 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47264)
%cmp$cmp54856 = icmp eq i64 %truthy$cmp54856, 1
br i1 %cmp$cmp54856, label %truebranch$cmp54856, label %falsebranch$cmp54856
truebranch$cmp54856:
%ae49268 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53632$k474040 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54857 = alloca %struct.ScmObj*, align 8
%argslist53632$k474041 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls147163, %struct.ScmObj* %argslist53632$k474040)
store volatile %struct.ScmObj* %argslist53632$k474041, %struct.ScmObj** %stackaddr$prim54857, align 8
%stackaddr$prim54858 = alloca %struct.ScmObj*, align 8
%argslist53632$k474042 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49268, %struct.ScmObj* %argslist53632$k474041)
store volatile %struct.ScmObj* %argslist53632$k474042, %struct.ScmObj** %stackaddr$prim54858, align 8
%clofunc54859 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47404)
musttail call tailcc void %clofunc54859(%struct.ScmObj* %k47404, %struct.ScmObj* %argslist53632$k474042)
ret void
falsebranch$cmp54856:
%stackaddr$prim54860 = alloca %struct.ScmObj*, align 8
%anf_45bind47265 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls047164)
store volatile %struct.ScmObj* %anf_45bind47265, %struct.ScmObj** %stackaddr$prim54860, align 8
%ae49275 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54861 = alloca %struct.ScmObj*, align 8
%anf_45bind47266 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append47161, %struct.ScmObj* %ae49275)
store volatile %struct.ScmObj* %anf_45bind47266, %struct.ScmObj** %stackaddr$prim54861, align 8
%stackaddr$prim54862 = alloca %struct.ScmObj*, align 8
%anf_45bind47267 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls047164)
store volatile %struct.ScmObj* %anf_45bind47267, %struct.ScmObj** %stackaddr$prim54862, align 8
%stackaddr$makeclosure54863 = alloca %struct.ScmObj*, align 8
%fptrToInt54864 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49278 to i64
%ae49278 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54864)
store volatile %struct.ScmObj* %ae49278, %struct.ScmObj** %stackaddr$makeclosure54863, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49278, %struct.ScmObj* %anf_45bind47265, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49278, %struct.ScmObj* %k47404, i64 1)
%argslist53637$anf_45bind472660 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54865 = alloca %struct.ScmObj*, align 8
%argslist53637$anf_45bind472661 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls147163, %struct.ScmObj* %argslist53637$anf_45bind472660)
store volatile %struct.ScmObj* %argslist53637$anf_45bind472661, %struct.ScmObj** %stackaddr$prim54865, align 8
%stackaddr$prim54866 = alloca %struct.ScmObj*, align 8
%argslist53637$anf_45bind472662 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47267, %struct.ScmObj* %argslist53637$anf_45bind472661)
store volatile %struct.ScmObj* %argslist53637$anf_45bind472662, %struct.ScmObj** %stackaddr$prim54866, align 8
%stackaddr$prim54867 = alloca %struct.ScmObj*, align 8
%argslist53637$anf_45bind472663 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49278, %struct.ScmObj* %argslist53637$anf_45bind472662)
store volatile %struct.ScmObj* %argslist53637$anf_45bind472663, %struct.ScmObj** %stackaddr$prim54867, align 8
%clofunc54868 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47266)
musttail call tailcc void %clofunc54868(%struct.ScmObj* %anf_45bind47266, %struct.ScmObj* %argslist53637$anf_45bind472663)
ret void
}

define tailcc void @proc_clo$ae49278(%struct.ScmObj* %env$ae49278,%struct.ScmObj* %current_45args53633) {
%stackaddr$env-ref54869 = alloca %struct.ScmObj*, align 8
%anf_45bind47265 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49278, i64 0)
store %struct.ScmObj* %anf_45bind47265, %struct.ScmObj** %stackaddr$env-ref54869
%stackaddr$env-ref54870 = alloca %struct.ScmObj*, align 8
%k47404 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49278, i64 1)
store %struct.ScmObj* %k47404, %struct.ScmObj** %stackaddr$env-ref54870
%stackaddr$prim54871 = alloca %struct.ScmObj*, align 8
%_95k47405 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53633)
store volatile %struct.ScmObj* %_95k47405, %struct.ScmObj** %stackaddr$prim54871, align 8
%stackaddr$prim54872 = alloca %struct.ScmObj*, align 8
%current_45args53634 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53633)
store volatile %struct.ScmObj* %current_45args53634, %struct.ScmObj** %stackaddr$prim54872, align 8
%stackaddr$prim54873 = alloca %struct.ScmObj*, align 8
%anf_45bind47268 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53634)
store volatile %struct.ScmObj* %anf_45bind47268, %struct.ScmObj** %stackaddr$prim54873, align 8
%stackaddr$prim54874 = alloca %struct.ScmObj*, align 8
%cpsprim47406 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47265, %struct.ScmObj* %anf_45bind47268)
store volatile %struct.ScmObj* %cpsprim47406, %struct.ScmObj** %stackaddr$prim54874, align 8
%ae49284 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53636$k474040 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54875 = alloca %struct.ScmObj*, align 8
%argslist53636$k474041 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47406, %struct.ScmObj* %argslist53636$k474040)
store volatile %struct.ScmObj* %argslist53636$k474041, %struct.ScmObj** %stackaddr$prim54875, align 8
%stackaddr$prim54876 = alloca %struct.ScmObj*, align 8
%argslist53636$k474042 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49284, %struct.ScmObj* %argslist53636$k474041)
store volatile %struct.ScmObj* %argslist53636$k474042, %struct.ScmObj** %stackaddr$prim54876, align 8
%clofunc54877 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47404)
musttail call tailcc void %clofunc54877(%struct.ScmObj* %k47404, %struct.ScmObj* %argslist53636$k474042)
ret void
}

define tailcc void @proc_clo$ae49238(%struct.ScmObj* %env$ae49238,%struct.ScmObj* %current_45args53639) {
%stackaddr$prim54878 = alloca %struct.ScmObj*, align 8
%k47407 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53639)
store volatile %struct.ScmObj* %k47407, %struct.ScmObj** %stackaddr$prim54878, align 8
%stackaddr$prim54879 = alloca %struct.ScmObj*, align 8
%current_45args53640 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53639)
store volatile %struct.ScmObj* %current_45args53640, %struct.ScmObj** %stackaddr$prim54879, align 8
%stackaddr$prim54880 = alloca %struct.ScmObj*, align 8
%a47167 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53640)
store volatile %struct.ScmObj* %a47167, %struct.ScmObj** %stackaddr$prim54880, align 8
%stackaddr$prim54881 = alloca %struct.ScmObj*, align 8
%current_45args53641 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53640)
store volatile %struct.ScmObj* %current_45args53641, %struct.ScmObj** %stackaddr$prim54881, align 8
%stackaddr$prim54882 = alloca %struct.ScmObj*, align 8
%b47166 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53641)
store volatile %struct.ScmObj* %b47166, %struct.ScmObj** %stackaddr$prim54882, align 8
%stackaddr$prim54883 = alloca %struct.ScmObj*, align 8
%anf_45bind47263 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a47167, %struct.ScmObj* %b47166)
store volatile %struct.ScmObj* %anf_45bind47263, %struct.ScmObj** %stackaddr$prim54883, align 8
%stackaddr$prim54884 = alloca %struct.ScmObj*, align 8
%cpsprim47408 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind47263)
store volatile %struct.ScmObj* %cpsprim47408, %struct.ScmObj** %stackaddr$prim54884, align 8
%ae49243 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53643$k474070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54885 = alloca %struct.ScmObj*, align 8
%argslist53643$k474071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47408, %struct.ScmObj* %argslist53643$k474070)
store volatile %struct.ScmObj* %argslist53643$k474071, %struct.ScmObj** %stackaddr$prim54885, align 8
%stackaddr$prim54886 = alloca %struct.ScmObj*, align 8
%argslist53643$k474072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49243, %struct.ScmObj* %argslist53643$k474071)
store volatile %struct.ScmObj* %argslist53643$k474072, %struct.ScmObj** %stackaddr$prim54886, align 8
%clofunc54887 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47407)
musttail call tailcc void %clofunc54887(%struct.ScmObj* %k47407, %struct.ScmObj* %argslist53643$k474072)
ret void
}

define tailcc void @proc_clo$ae49214(%struct.ScmObj* %env$ae49214,%struct.ScmObj* %current_45args53645) {
%stackaddr$prim54888 = alloca %struct.ScmObj*, align 8
%k47409 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53645)
store volatile %struct.ScmObj* %k47409, %struct.ScmObj** %stackaddr$prim54888, align 8
%stackaddr$prim54889 = alloca %struct.ScmObj*, align 8
%current_45args53646 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53645)
store volatile %struct.ScmObj* %current_45args53646, %struct.ScmObj** %stackaddr$prim54889, align 8
%stackaddr$prim54890 = alloca %struct.ScmObj*, align 8
%a47170 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53646)
store volatile %struct.ScmObj* %a47170, %struct.ScmObj** %stackaddr$prim54890, align 8
%stackaddr$prim54891 = alloca %struct.ScmObj*, align 8
%current_45args53647 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53646)
store volatile %struct.ScmObj* %current_45args53647, %struct.ScmObj** %stackaddr$prim54891, align 8
%stackaddr$prim54892 = alloca %struct.ScmObj*, align 8
%b47169 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53647)
store volatile %struct.ScmObj* %b47169, %struct.ScmObj** %stackaddr$prim54892, align 8
%stackaddr$prim54893 = alloca %struct.ScmObj*, align 8
%anf_45bind47262 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a47170, %struct.ScmObj* %b47169)
store volatile %struct.ScmObj* %anf_45bind47262, %struct.ScmObj** %stackaddr$prim54893, align 8
%stackaddr$prim54894 = alloca %struct.ScmObj*, align 8
%cpsprim47410 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind47262)
store volatile %struct.ScmObj* %cpsprim47410, %struct.ScmObj** %stackaddr$prim54894, align 8
%ae49219 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53649$k474090 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54895 = alloca %struct.ScmObj*, align 8
%argslist53649$k474091 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47410, %struct.ScmObj* %argslist53649$k474090)
store volatile %struct.ScmObj* %argslist53649$k474091, %struct.ScmObj** %stackaddr$prim54895, align 8
%stackaddr$prim54896 = alloca %struct.ScmObj*, align 8
%argslist53649$k474092 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49219, %struct.ScmObj* %argslist53649$k474091)
store volatile %struct.ScmObj* %argslist53649$k474092, %struct.ScmObj** %stackaddr$prim54896, align 8
%clofunc54897 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47409)
musttail call tailcc void %clofunc54897(%struct.ScmObj* %k47409, %struct.ScmObj* %argslist53649$k474092)
ret void
}

define tailcc void @proc_clo$ae48820(%struct.ScmObj* %env$ae48820,%struct.ScmObj* %current_45args53652) {
%stackaddr$env-ref54898 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48820, i64 0)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref54898
%stackaddr$env-ref54899 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48820, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54899
%stackaddr$env-ref54900 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48820, i64 2)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54900
%stackaddr$prim54901 = alloca %struct.ScmObj*, align 8
%k47411 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53652)
store volatile %struct.ScmObj* %k47411, %struct.ScmObj** %stackaddr$prim54901, align 8
%stackaddr$prim54902 = alloca %struct.ScmObj*, align 8
%current_45args53653 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53652)
store volatile %struct.ScmObj* %current_45args53653, %struct.ScmObj** %stackaddr$prim54902, align 8
%stackaddr$prim54903 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53653)
store volatile %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$prim54903, align 8
%ae48822 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54904 = alloca %struct.ScmObj*, align 8
%fptrToInt54905 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48823 to i64
%ae48823 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54905)
store volatile %struct.ScmObj* %ae48823, %struct.ScmObj** %stackaddr$makeclosure54904, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48823, %struct.ScmObj* %_37foldr47094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48823, %struct.ScmObj* %_37foldl47172, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48823, %struct.ScmObj* %_37foldr147089, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48823, %struct.ScmObj* %_37map147120, i64 3)
%argslist53710$k474110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54906 = alloca %struct.ScmObj*, align 8
%argslist53710$k474111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48823, %struct.ScmObj* %argslist53710$k474110)
store volatile %struct.ScmObj* %argslist53710$k474111, %struct.ScmObj** %stackaddr$prim54906, align 8
%stackaddr$prim54907 = alloca %struct.ScmObj*, align 8
%argslist53710$k474112 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48822, %struct.ScmObj* %argslist53710$k474111)
store volatile %struct.ScmObj* %argslist53710$k474112, %struct.ScmObj** %stackaddr$prim54907, align 8
%clofunc54908 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47411)
musttail call tailcc void %clofunc54908(%struct.ScmObj* %k47411, %struct.ScmObj* %argslist53710$k474112)
ret void
}

define tailcc void @proc_clo$ae48823(%struct.ScmObj* %env$ae48823,%struct.ScmObj* %args4717347412) {
%stackaddr$env-ref54909 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48823, i64 0)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref54909
%stackaddr$env-ref54910 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48823, i64 1)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref54910
%stackaddr$env-ref54911 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48823, i64 2)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54911
%stackaddr$env-ref54912 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48823, i64 3)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54912
%stackaddr$prim54913 = alloca %struct.ScmObj*, align 8
%k47413 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4717347412)
store volatile %struct.ScmObj* %k47413, %struct.ScmObj** %stackaddr$prim54913, align 8
%stackaddr$prim54914 = alloca %struct.ScmObj*, align 8
%args47173 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4717347412)
store volatile %struct.ScmObj* %args47173, %struct.ScmObj** %stackaddr$prim54914, align 8
%stackaddr$prim54915 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47173)
store volatile %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$prim54915, align 8
%stackaddr$prim54916 = alloca %struct.ScmObj*, align 8
%anf_45bind47250 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47173)
store volatile %struct.ScmObj* %anf_45bind47250, %struct.ScmObj** %stackaddr$prim54916, align 8
%stackaddr$prim54917 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47250)
store volatile %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$prim54917, align 8
%stackaddr$prim54918 = alloca %struct.ScmObj*, align 8
%anf_45bind47251 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47173)
store volatile %struct.ScmObj* %anf_45bind47251, %struct.ScmObj** %stackaddr$prim54918, align 8
%stackaddr$prim54919 = alloca %struct.ScmObj*, align 8
%lsts47174 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47251)
store volatile %struct.ScmObj* %lsts47174, %struct.ScmObj** %stackaddr$prim54919, align 8
%stackaddr$makeclosure54920 = alloca %struct.ScmObj*, align 8
%fptrToInt54921 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48831 to i64
%ae48831 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt54921)
store volatile %struct.ScmObj* %ae48831, %struct.ScmObj** %stackaddr$makeclosure54920, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48831, %struct.ScmObj* %lsts47174, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48831, %struct.ScmObj* %_37foldr47094, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48831, %struct.ScmObj* %k47413, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48831, %struct.ScmObj* %_37foldl47172, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48831, %struct.ScmObj* %_37foldr147089, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48831, %struct.ScmObj* %_37map147120, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48831, %struct.ScmObj* %f47176, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae48831, %struct.ScmObj* %acc47175, i64 7)
%ae48832 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54922 = alloca %struct.ScmObj*, align 8
%fptrToInt54923 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48833 to i64
%ae48833 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54923)
store volatile %struct.ScmObj* %ae48833, %struct.ScmObj** %stackaddr$makeclosure54922, align 8
%argslist53709$ae488310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54924 = alloca %struct.ScmObj*, align 8
%argslist53709$ae488311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48833, %struct.ScmObj* %argslist53709$ae488310)
store volatile %struct.ScmObj* %argslist53709$ae488311, %struct.ScmObj** %stackaddr$prim54924, align 8
%stackaddr$prim54925 = alloca %struct.ScmObj*, align 8
%argslist53709$ae488312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48832, %struct.ScmObj* %argslist53709$ae488311)
store volatile %struct.ScmObj* %argslist53709$ae488312, %struct.ScmObj** %stackaddr$prim54925, align 8
%clofunc54926 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48831)
musttail call tailcc void %clofunc54926(%struct.ScmObj* %ae48831, %struct.ScmObj* %argslist53709$ae488312)
ret void
}

define tailcc void @proc_clo$ae48831(%struct.ScmObj* %env$ae48831,%struct.ScmObj* %current_45args53655) {
%stackaddr$env-ref54927 = alloca %struct.ScmObj*, align 8
%lsts47174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48831, i64 0)
store %struct.ScmObj* %lsts47174, %struct.ScmObj** %stackaddr$env-ref54927
%stackaddr$env-ref54928 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48831, i64 1)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref54928
%stackaddr$env-ref54929 = alloca %struct.ScmObj*, align 8
%k47413 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48831, i64 2)
store %struct.ScmObj* %k47413, %struct.ScmObj** %stackaddr$env-ref54929
%stackaddr$env-ref54930 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48831, i64 3)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref54930
%stackaddr$env-ref54931 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48831, i64 4)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54931
%stackaddr$env-ref54932 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48831, i64 5)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54932
%stackaddr$env-ref54933 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48831, i64 6)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref54933
%stackaddr$env-ref54934 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48831, i64 7)
store %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$env-ref54934
%stackaddr$prim54935 = alloca %struct.ScmObj*, align 8
%_95k47414 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53655)
store volatile %struct.ScmObj* %_95k47414, %struct.ScmObj** %stackaddr$prim54935, align 8
%stackaddr$prim54936 = alloca %struct.ScmObj*, align 8
%current_45args53656 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53655)
store volatile %struct.ScmObj* %current_45args53656, %struct.ScmObj** %stackaddr$prim54936, align 8
%stackaddr$prim54937 = alloca %struct.ScmObj*, align 8
%anf_45bind47252 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53656)
store volatile %struct.ScmObj* %anf_45bind47252, %struct.ScmObj** %stackaddr$prim54937, align 8
%stackaddr$makeclosure54938 = alloca %struct.ScmObj*, align 8
%fptrToInt54939 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48863 to i64
%ae48863 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt54939)
store volatile %struct.ScmObj* %ae48863, %struct.ScmObj** %stackaddr$makeclosure54938, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48863, %struct.ScmObj* %lsts47174, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48863, %struct.ScmObj* %_37foldr47094, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48863, %struct.ScmObj* %k47413, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48863, %struct.ScmObj* %_37foldl47172, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48863, %struct.ScmObj* %_37map147120, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48863, %struct.ScmObj* %f47176, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48863, %struct.ScmObj* %acc47175, i64 6)
%ae48865 = call %struct.ScmObj* @const_init_false()
%argslist53702$_37foldr1470890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54940 = alloca %struct.ScmObj*, align 8
%argslist53702$_37foldr1470891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47174, %struct.ScmObj* %argslist53702$_37foldr1470890)
store volatile %struct.ScmObj* %argslist53702$_37foldr1470891, %struct.ScmObj** %stackaddr$prim54940, align 8
%stackaddr$prim54941 = alloca %struct.ScmObj*, align 8
%argslist53702$_37foldr1470892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48865, %struct.ScmObj* %argslist53702$_37foldr1470891)
store volatile %struct.ScmObj* %argslist53702$_37foldr1470892, %struct.ScmObj** %stackaddr$prim54941, align 8
%stackaddr$prim54942 = alloca %struct.ScmObj*, align 8
%argslist53702$_37foldr1470893 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47252, %struct.ScmObj* %argslist53702$_37foldr1470892)
store volatile %struct.ScmObj* %argslist53702$_37foldr1470893, %struct.ScmObj** %stackaddr$prim54942, align 8
%stackaddr$prim54943 = alloca %struct.ScmObj*, align 8
%argslist53702$_37foldr1470894 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48863, %struct.ScmObj* %argslist53702$_37foldr1470893)
store volatile %struct.ScmObj* %argslist53702$_37foldr1470894, %struct.ScmObj** %stackaddr$prim54943, align 8
%clofunc54944 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147089)
musttail call tailcc void %clofunc54944(%struct.ScmObj* %_37foldr147089, %struct.ScmObj* %argslist53702$_37foldr1470894)
ret void
}

define tailcc void @proc_clo$ae48863(%struct.ScmObj* %env$ae48863,%struct.ScmObj* %current_45args53658) {
%stackaddr$env-ref54945 = alloca %struct.ScmObj*, align 8
%lsts47174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48863, i64 0)
store %struct.ScmObj* %lsts47174, %struct.ScmObj** %stackaddr$env-ref54945
%stackaddr$env-ref54946 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48863, i64 1)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref54946
%stackaddr$env-ref54947 = alloca %struct.ScmObj*, align 8
%k47413 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48863, i64 2)
store %struct.ScmObj* %k47413, %struct.ScmObj** %stackaddr$env-ref54947
%stackaddr$env-ref54948 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48863, i64 3)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref54948
%stackaddr$env-ref54949 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48863, i64 4)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54949
%stackaddr$env-ref54950 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48863, i64 5)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref54950
%stackaddr$env-ref54951 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48863, i64 6)
store %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$env-ref54951
%stackaddr$prim54952 = alloca %struct.ScmObj*, align 8
%_95k47415 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53658)
store volatile %struct.ScmObj* %_95k47415, %struct.ScmObj** %stackaddr$prim54952, align 8
%stackaddr$prim54953 = alloca %struct.ScmObj*, align 8
%current_45args53659 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53658)
store volatile %struct.ScmObj* %current_45args53659, %struct.ScmObj** %stackaddr$prim54953, align 8
%stackaddr$prim54954 = alloca %struct.ScmObj*, align 8
%anf_45bind47253 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53659)
store volatile %struct.ScmObj* %anf_45bind47253, %struct.ScmObj** %stackaddr$prim54954, align 8
%truthy$cmp54955 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47253)
%cmp$cmp54955 = icmp eq i64 %truthy$cmp54955, 1
br i1 %cmp$cmp54955, label %truebranch$cmp54955, label %falsebranch$cmp54955
truebranch$cmp54955:
%ae48874 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53661$k474130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54956 = alloca %struct.ScmObj*, align 8
%argslist53661$k474131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47175, %struct.ScmObj* %argslist53661$k474130)
store volatile %struct.ScmObj* %argslist53661$k474131, %struct.ScmObj** %stackaddr$prim54956, align 8
%stackaddr$prim54957 = alloca %struct.ScmObj*, align 8
%argslist53661$k474132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48874, %struct.ScmObj* %argslist53661$k474131)
store volatile %struct.ScmObj* %argslist53661$k474132, %struct.ScmObj** %stackaddr$prim54957, align 8
%clofunc54958 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47413)
musttail call tailcc void %clofunc54958(%struct.ScmObj* %k47413, %struct.ScmObj* %argslist53661$k474132)
ret void
falsebranch$cmp54955:
%stackaddr$makeclosure54959 = alloca %struct.ScmObj*, align 8
%fptrToInt54960 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48879 to i64
%ae48879 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt54960)
store volatile %struct.ScmObj* %ae48879, %struct.ScmObj** %stackaddr$makeclosure54959, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48879, %struct.ScmObj* %lsts47174, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48879, %struct.ScmObj* %_37foldr47094, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48879, %struct.ScmObj* %k47413, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48879, %struct.ScmObj* %_37foldl47172, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48879, %struct.ScmObj* %_37map147120, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48879, %struct.ScmObj* %f47176, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48879, %struct.ScmObj* %acc47175, i64 6)
%ae48880 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54961 = alloca %struct.ScmObj*, align 8
%fptrToInt54962 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48881 to i64
%ae48881 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54962)
store volatile %struct.ScmObj* %ae48881, %struct.ScmObj** %stackaddr$makeclosure54961, align 8
%argslist53701$ae488790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54963 = alloca %struct.ScmObj*, align 8
%argslist53701$ae488791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48881, %struct.ScmObj* %argslist53701$ae488790)
store volatile %struct.ScmObj* %argslist53701$ae488791, %struct.ScmObj** %stackaddr$prim54963, align 8
%stackaddr$prim54964 = alloca %struct.ScmObj*, align 8
%argslist53701$ae488792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48880, %struct.ScmObj* %argslist53701$ae488791)
store volatile %struct.ScmObj* %argslist53701$ae488792, %struct.ScmObj** %stackaddr$prim54964, align 8
%clofunc54965 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48879)
musttail call tailcc void %clofunc54965(%struct.ScmObj* %ae48879, %struct.ScmObj* %argslist53701$ae488792)
ret void
}

define tailcc void @proc_clo$ae48879(%struct.ScmObj* %env$ae48879,%struct.ScmObj* %current_45args53662) {
%stackaddr$env-ref54966 = alloca %struct.ScmObj*, align 8
%lsts47174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48879, i64 0)
store %struct.ScmObj* %lsts47174, %struct.ScmObj** %stackaddr$env-ref54966
%stackaddr$env-ref54967 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48879, i64 1)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref54967
%stackaddr$env-ref54968 = alloca %struct.ScmObj*, align 8
%k47413 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48879, i64 2)
store %struct.ScmObj* %k47413, %struct.ScmObj** %stackaddr$env-ref54968
%stackaddr$env-ref54969 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48879, i64 3)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref54969
%stackaddr$env-ref54970 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48879, i64 4)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54970
%stackaddr$env-ref54971 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48879, i64 5)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref54971
%stackaddr$env-ref54972 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48879, i64 6)
store %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$env-ref54972
%stackaddr$prim54973 = alloca %struct.ScmObj*, align 8
%_95k47416 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53662)
store volatile %struct.ScmObj* %_95k47416, %struct.ScmObj** %stackaddr$prim54973, align 8
%stackaddr$prim54974 = alloca %struct.ScmObj*, align 8
%current_45args53663 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53662)
store volatile %struct.ScmObj* %current_45args53663, %struct.ScmObj** %stackaddr$prim54974, align 8
%stackaddr$prim54975 = alloca %struct.ScmObj*, align 8
%anf_45bind47254 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53663)
store volatile %struct.ScmObj* %anf_45bind47254, %struct.ScmObj** %stackaddr$prim54975, align 8
%stackaddr$makeclosure54976 = alloca %struct.ScmObj*, align 8
%fptrToInt54977 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48900 to i64
%ae48900 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt54977)
store volatile %struct.ScmObj* %ae48900, %struct.ScmObj** %stackaddr$makeclosure54976, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48900, %struct.ScmObj* %lsts47174, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48900, %struct.ScmObj* %_37foldr47094, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48900, %struct.ScmObj* %k47413, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48900, %struct.ScmObj* %_37foldl47172, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48900, %struct.ScmObj* %_37map147120, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48900, %struct.ScmObj* %f47176, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48900, %struct.ScmObj* %acc47175, i64 6)
%argslist53696$_37map1471200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54978 = alloca %struct.ScmObj*, align 8
%argslist53696$_37map1471201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47174, %struct.ScmObj* %argslist53696$_37map1471200)
store volatile %struct.ScmObj* %argslist53696$_37map1471201, %struct.ScmObj** %stackaddr$prim54978, align 8
%stackaddr$prim54979 = alloca %struct.ScmObj*, align 8
%argslist53696$_37map1471202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47254, %struct.ScmObj* %argslist53696$_37map1471201)
store volatile %struct.ScmObj* %argslist53696$_37map1471202, %struct.ScmObj** %stackaddr$prim54979, align 8
%stackaddr$prim54980 = alloca %struct.ScmObj*, align 8
%argslist53696$_37map1471203 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48900, %struct.ScmObj* %argslist53696$_37map1471202)
store volatile %struct.ScmObj* %argslist53696$_37map1471203, %struct.ScmObj** %stackaddr$prim54980, align 8
%clofunc54981 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147120)
musttail call tailcc void %clofunc54981(%struct.ScmObj* %_37map147120, %struct.ScmObj* %argslist53696$_37map1471203)
ret void
}

define tailcc void @proc_clo$ae48900(%struct.ScmObj* %env$ae48900,%struct.ScmObj* %current_45args53665) {
%stackaddr$env-ref54982 = alloca %struct.ScmObj*, align 8
%lsts47174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48900, i64 0)
store %struct.ScmObj* %lsts47174, %struct.ScmObj** %stackaddr$env-ref54982
%stackaddr$env-ref54983 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48900, i64 1)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref54983
%stackaddr$env-ref54984 = alloca %struct.ScmObj*, align 8
%k47413 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48900, i64 2)
store %struct.ScmObj* %k47413, %struct.ScmObj** %stackaddr$env-ref54984
%stackaddr$env-ref54985 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48900, i64 3)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref54985
%stackaddr$env-ref54986 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48900, i64 4)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54986
%stackaddr$env-ref54987 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48900, i64 5)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref54987
%stackaddr$env-ref54988 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48900, i64 6)
store %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$env-ref54988
%stackaddr$prim54989 = alloca %struct.ScmObj*, align 8
%_95k47417 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53665)
store volatile %struct.ScmObj* %_95k47417, %struct.ScmObj** %stackaddr$prim54989, align 8
%stackaddr$prim54990 = alloca %struct.ScmObj*, align 8
%current_45args53666 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53665)
store volatile %struct.ScmObj* %current_45args53666, %struct.ScmObj** %stackaddr$prim54990, align 8
%stackaddr$prim54991 = alloca %struct.ScmObj*, align 8
%lsts_4347181 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53666)
store volatile %struct.ScmObj* %lsts_4347181, %struct.ScmObj** %stackaddr$prim54991, align 8
%stackaddr$makeclosure54992 = alloca %struct.ScmObj*, align 8
%fptrToInt54993 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48903 to i64
%ae48903 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt54993)
store volatile %struct.ScmObj* %ae48903, %struct.ScmObj** %stackaddr$makeclosure54992, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48903, %struct.ScmObj* %lsts47174, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48903, %struct.ScmObj* %_37foldr47094, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48903, %struct.ScmObj* %k47413, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48903, %struct.ScmObj* %_37foldl47172, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48903, %struct.ScmObj* %_37map147120, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48903, %struct.ScmObj* %lsts_4347181, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48903, %struct.ScmObj* %f47176, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae48903, %struct.ScmObj* %acc47175, i64 7)
%ae48904 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54994 = alloca %struct.ScmObj*, align 8
%fptrToInt54995 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48905 to i64
%ae48905 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54995)
store volatile %struct.ScmObj* %ae48905, %struct.ScmObj** %stackaddr$makeclosure54994, align 8
%argslist53695$ae489030 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54996 = alloca %struct.ScmObj*, align 8
%argslist53695$ae489031 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48905, %struct.ScmObj* %argslist53695$ae489030)
store volatile %struct.ScmObj* %argslist53695$ae489031, %struct.ScmObj** %stackaddr$prim54996, align 8
%stackaddr$prim54997 = alloca %struct.ScmObj*, align 8
%argslist53695$ae489032 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48904, %struct.ScmObj* %argslist53695$ae489031)
store volatile %struct.ScmObj* %argslist53695$ae489032, %struct.ScmObj** %stackaddr$prim54997, align 8
%clofunc54998 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48903)
musttail call tailcc void %clofunc54998(%struct.ScmObj* %ae48903, %struct.ScmObj* %argslist53695$ae489032)
ret void
}

define tailcc void @proc_clo$ae48903(%struct.ScmObj* %env$ae48903,%struct.ScmObj* %current_45args53668) {
%stackaddr$env-ref54999 = alloca %struct.ScmObj*, align 8
%lsts47174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48903, i64 0)
store %struct.ScmObj* %lsts47174, %struct.ScmObj** %stackaddr$env-ref54999
%stackaddr$env-ref55000 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48903, i64 1)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref55000
%stackaddr$env-ref55001 = alloca %struct.ScmObj*, align 8
%k47413 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48903, i64 2)
store %struct.ScmObj* %k47413, %struct.ScmObj** %stackaddr$env-ref55001
%stackaddr$env-ref55002 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48903, i64 3)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref55002
%stackaddr$env-ref55003 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48903, i64 4)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref55003
%stackaddr$env-ref55004 = alloca %struct.ScmObj*, align 8
%lsts_4347181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48903, i64 5)
store %struct.ScmObj* %lsts_4347181, %struct.ScmObj** %stackaddr$env-ref55004
%stackaddr$env-ref55005 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48903, i64 6)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref55005
%stackaddr$env-ref55006 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48903, i64 7)
store %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$env-ref55006
%stackaddr$prim55007 = alloca %struct.ScmObj*, align 8
%_95k47418 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53668)
store volatile %struct.ScmObj* %_95k47418, %struct.ScmObj** %stackaddr$prim55007, align 8
%stackaddr$prim55008 = alloca %struct.ScmObj*, align 8
%current_45args53669 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53668)
store volatile %struct.ScmObj* %current_45args53669, %struct.ScmObj** %stackaddr$prim55008, align 8
%stackaddr$prim55009 = alloca %struct.ScmObj*, align 8
%anf_45bind47255 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53669)
store volatile %struct.ScmObj* %anf_45bind47255, %struct.ScmObj** %stackaddr$prim55009, align 8
%stackaddr$makeclosure55010 = alloca %struct.ScmObj*, align 8
%fptrToInt55011 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48924 to i64
%ae48924 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt55011)
store volatile %struct.ScmObj* %ae48924, %struct.ScmObj** %stackaddr$makeclosure55010, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48924, %struct.ScmObj* %_37foldr47094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48924, %struct.ScmObj* %k47413, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48924, %struct.ScmObj* %_37foldl47172, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48924, %struct.ScmObj* %lsts_4347181, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48924, %struct.ScmObj* %f47176, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48924, %struct.ScmObj* %acc47175, i64 5)
%argslist53690$_37map1471200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55012 = alloca %struct.ScmObj*, align 8
%argslist53690$_37map1471201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47174, %struct.ScmObj* %argslist53690$_37map1471200)
store volatile %struct.ScmObj* %argslist53690$_37map1471201, %struct.ScmObj** %stackaddr$prim55012, align 8
%stackaddr$prim55013 = alloca %struct.ScmObj*, align 8
%argslist53690$_37map1471202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47255, %struct.ScmObj* %argslist53690$_37map1471201)
store volatile %struct.ScmObj* %argslist53690$_37map1471202, %struct.ScmObj** %stackaddr$prim55013, align 8
%stackaddr$prim55014 = alloca %struct.ScmObj*, align 8
%argslist53690$_37map1471203 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48924, %struct.ScmObj* %argslist53690$_37map1471202)
store volatile %struct.ScmObj* %argslist53690$_37map1471203, %struct.ScmObj** %stackaddr$prim55014, align 8
%clofunc55015 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147120)
musttail call tailcc void %clofunc55015(%struct.ScmObj* %_37map147120, %struct.ScmObj* %argslist53690$_37map1471203)
ret void
}

define tailcc void @proc_clo$ae48924(%struct.ScmObj* %env$ae48924,%struct.ScmObj* %current_45args53671) {
%stackaddr$env-ref55016 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48924, i64 0)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref55016
%stackaddr$env-ref55017 = alloca %struct.ScmObj*, align 8
%k47413 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48924, i64 1)
store %struct.ScmObj* %k47413, %struct.ScmObj** %stackaddr$env-ref55017
%stackaddr$env-ref55018 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48924, i64 2)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref55018
%stackaddr$env-ref55019 = alloca %struct.ScmObj*, align 8
%lsts_4347181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48924, i64 3)
store %struct.ScmObj* %lsts_4347181, %struct.ScmObj** %stackaddr$env-ref55019
%stackaddr$env-ref55020 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48924, i64 4)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref55020
%stackaddr$env-ref55021 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48924, i64 5)
store %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$env-ref55021
%stackaddr$prim55022 = alloca %struct.ScmObj*, align 8
%_95k47419 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53671)
store volatile %struct.ScmObj* %_95k47419, %struct.ScmObj** %stackaddr$prim55022, align 8
%stackaddr$prim55023 = alloca %struct.ScmObj*, align 8
%current_45args53672 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53671)
store volatile %struct.ScmObj* %current_45args53672, %struct.ScmObj** %stackaddr$prim55023, align 8
%stackaddr$prim55024 = alloca %struct.ScmObj*, align 8
%vs47179 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53672)
store volatile %struct.ScmObj* %vs47179, %struct.ScmObj** %stackaddr$prim55024, align 8
%stackaddr$makeclosure55025 = alloca %struct.ScmObj*, align 8
%fptrToInt55026 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48927 to i64
%ae48927 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55026)
store volatile %struct.ScmObj* %ae48927, %struct.ScmObj** %stackaddr$makeclosure55025, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48927, %struct.ScmObj* %_37foldr47094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48927, %struct.ScmObj* %k47413, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48927, %struct.ScmObj* %_37foldl47172, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48927, %struct.ScmObj* %lsts_4347181, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48927, %struct.ScmObj* %vs47179, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48927, %struct.ScmObj* %f47176, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48927, %struct.ScmObj* %acc47175, i64 6)
%ae48928 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55027 = alloca %struct.ScmObj*, align 8
%fptrToInt55028 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48929 to i64
%ae48929 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55028)
store volatile %struct.ScmObj* %ae48929, %struct.ScmObj** %stackaddr$makeclosure55027, align 8
%argslist53689$ae489270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55029 = alloca %struct.ScmObj*, align 8
%argslist53689$ae489271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48929, %struct.ScmObj* %argslist53689$ae489270)
store volatile %struct.ScmObj* %argslist53689$ae489271, %struct.ScmObj** %stackaddr$prim55029, align 8
%stackaddr$prim55030 = alloca %struct.ScmObj*, align 8
%argslist53689$ae489272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48928, %struct.ScmObj* %argslist53689$ae489271)
store volatile %struct.ScmObj* %argslist53689$ae489272, %struct.ScmObj** %stackaddr$prim55030, align 8
%clofunc55031 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48927)
musttail call tailcc void %clofunc55031(%struct.ScmObj* %ae48927, %struct.ScmObj* %argslist53689$ae489272)
ret void
}

define tailcc void @proc_clo$ae48927(%struct.ScmObj* %env$ae48927,%struct.ScmObj* %current_45args53674) {
%stackaddr$env-ref55032 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48927, i64 0)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref55032
%stackaddr$env-ref55033 = alloca %struct.ScmObj*, align 8
%k47413 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48927, i64 1)
store %struct.ScmObj* %k47413, %struct.ScmObj** %stackaddr$env-ref55033
%stackaddr$env-ref55034 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48927, i64 2)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref55034
%stackaddr$env-ref55035 = alloca %struct.ScmObj*, align 8
%lsts_4347181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48927, i64 3)
store %struct.ScmObj* %lsts_4347181, %struct.ScmObj** %stackaddr$env-ref55035
%stackaddr$env-ref55036 = alloca %struct.ScmObj*, align 8
%vs47179 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48927, i64 4)
store %struct.ScmObj* %vs47179, %struct.ScmObj** %stackaddr$env-ref55036
%stackaddr$env-ref55037 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48927, i64 5)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref55037
%stackaddr$env-ref55038 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48927, i64 6)
store %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$env-ref55038
%stackaddr$prim55039 = alloca %struct.ScmObj*, align 8
%_95k47420 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53674)
store volatile %struct.ScmObj* %_95k47420, %struct.ScmObj** %stackaddr$prim55039, align 8
%stackaddr$prim55040 = alloca %struct.ScmObj*, align 8
%current_45args53675 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53674)
store volatile %struct.ScmObj* %current_45args53675, %struct.ScmObj** %stackaddr$prim55040, align 8
%stackaddr$prim55041 = alloca %struct.ScmObj*, align 8
%anf_45bind47256 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53675)
store volatile %struct.ScmObj* %anf_45bind47256, %struct.ScmObj** %stackaddr$prim55041, align 8
%ae48950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55042 = alloca %struct.ScmObj*, align 8
%anf_45bind47257 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47175, %struct.ScmObj* %ae48950)
store volatile %struct.ScmObj* %anf_45bind47257, %struct.ScmObj** %stackaddr$prim55042, align 8
%stackaddr$makeclosure55043 = alloca %struct.ScmObj*, align 8
%fptrToInt55044 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48952 to i64
%ae48952 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55044)
store volatile %struct.ScmObj* %ae48952, %struct.ScmObj** %stackaddr$makeclosure55043, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48952, %struct.ScmObj* %k47413, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48952, %struct.ScmObj* %_37foldl47172, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48952, %struct.ScmObj* %lsts_4347181, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48952, %struct.ScmObj* %f47176, i64 3)
%argslist53683$_37foldr470940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55045 = alloca %struct.ScmObj*, align 8
%argslist53683$_37foldr470941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs47179, %struct.ScmObj* %argslist53683$_37foldr470940)
store volatile %struct.ScmObj* %argslist53683$_37foldr470941, %struct.ScmObj** %stackaddr$prim55045, align 8
%stackaddr$prim55046 = alloca %struct.ScmObj*, align 8
%argslist53683$_37foldr470942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47257, %struct.ScmObj* %argslist53683$_37foldr470941)
store volatile %struct.ScmObj* %argslist53683$_37foldr470942, %struct.ScmObj** %stackaddr$prim55046, align 8
%stackaddr$prim55047 = alloca %struct.ScmObj*, align 8
%argslist53683$_37foldr470943 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47256, %struct.ScmObj* %argslist53683$_37foldr470942)
store volatile %struct.ScmObj* %argslist53683$_37foldr470943, %struct.ScmObj** %stackaddr$prim55047, align 8
%stackaddr$prim55048 = alloca %struct.ScmObj*, align 8
%argslist53683$_37foldr470944 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48952, %struct.ScmObj* %argslist53683$_37foldr470943)
store volatile %struct.ScmObj* %argslist53683$_37foldr470944, %struct.ScmObj** %stackaddr$prim55048, align 8
%clofunc55049 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr47094)
musttail call tailcc void %clofunc55049(%struct.ScmObj* %_37foldr47094, %struct.ScmObj* %argslist53683$_37foldr470944)
ret void
}

define tailcc void @proc_clo$ae48952(%struct.ScmObj* %env$ae48952,%struct.ScmObj* %current_45args53677) {
%stackaddr$env-ref55050 = alloca %struct.ScmObj*, align 8
%k47413 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48952, i64 0)
store %struct.ScmObj* %k47413, %struct.ScmObj** %stackaddr$env-ref55050
%stackaddr$env-ref55051 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48952, i64 1)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref55051
%stackaddr$env-ref55052 = alloca %struct.ScmObj*, align 8
%lsts_4347181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48952, i64 2)
store %struct.ScmObj* %lsts_4347181, %struct.ScmObj** %stackaddr$env-ref55052
%stackaddr$env-ref55053 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48952, i64 3)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref55053
%stackaddr$prim55054 = alloca %struct.ScmObj*, align 8
%_95k47421 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53677)
store volatile %struct.ScmObj* %_95k47421, %struct.ScmObj** %stackaddr$prim55054, align 8
%stackaddr$prim55055 = alloca %struct.ScmObj*, align 8
%current_45args53678 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53677)
store volatile %struct.ScmObj* %current_45args53678, %struct.ScmObj** %stackaddr$prim55055, align 8
%stackaddr$prim55056 = alloca %struct.ScmObj*, align 8
%anf_45bind47258 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53678)
store volatile %struct.ScmObj* %anf_45bind47258, %struct.ScmObj** %stackaddr$prim55056, align 8
%stackaddr$makeclosure55057 = alloca %struct.ScmObj*, align 8
%fptrToInt55058 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48956 to i64
%ae48956 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55058)
store volatile %struct.ScmObj* %ae48956, %struct.ScmObj** %stackaddr$makeclosure55057, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48956, %struct.ScmObj* %k47413, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48956, %struct.ScmObj* %_37foldl47172, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48956, %struct.ScmObj* %lsts_4347181, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48956, %struct.ScmObj* %f47176, i64 3)
%stackaddr$prim55059 = alloca %struct.ScmObj*, align 8
%cpsargs47424 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48956, %struct.ScmObj* %anf_45bind47258)
store volatile %struct.ScmObj* %cpsargs47424, %struct.ScmObj** %stackaddr$prim55059, align 8
%clofunc55060 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47176)
musttail call tailcc void %clofunc55060(%struct.ScmObj* %f47176, %struct.ScmObj* %cpsargs47424)
ret void
}

define tailcc void @proc_clo$ae48956(%struct.ScmObj* %env$ae48956,%struct.ScmObj* %current_45args53680) {
%stackaddr$env-ref55061 = alloca %struct.ScmObj*, align 8
%k47413 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48956, i64 0)
store %struct.ScmObj* %k47413, %struct.ScmObj** %stackaddr$env-ref55061
%stackaddr$env-ref55062 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48956, i64 1)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref55062
%stackaddr$env-ref55063 = alloca %struct.ScmObj*, align 8
%lsts_4347181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48956, i64 2)
store %struct.ScmObj* %lsts_4347181, %struct.ScmObj** %stackaddr$env-ref55063
%stackaddr$env-ref55064 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48956, i64 3)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref55064
%stackaddr$prim55065 = alloca %struct.ScmObj*, align 8
%_95k47422 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53680)
store volatile %struct.ScmObj* %_95k47422, %struct.ScmObj** %stackaddr$prim55065, align 8
%stackaddr$prim55066 = alloca %struct.ScmObj*, align 8
%current_45args53681 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53680)
store volatile %struct.ScmObj* %current_45args53681, %struct.ScmObj** %stackaddr$prim55066, align 8
%stackaddr$prim55067 = alloca %struct.ScmObj*, align 8
%acc_4347183 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53681)
store volatile %struct.ScmObj* %acc_4347183, %struct.ScmObj** %stackaddr$prim55067, align 8
%stackaddr$prim55068 = alloca %struct.ScmObj*, align 8
%anf_45bind47259 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4347183, %struct.ScmObj* %lsts_4347181)
store volatile %struct.ScmObj* %anf_45bind47259, %struct.ScmObj** %stackaddr$prim55068, align 8
%stackaddr$prim55069 = alloca %struct.ScmObj*, align 8
%anf_45bind47260 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47176, %struct.ScmObj* %anf_45bind47259)
store volatile %struct.ScmObj* %anf_45bind47260, %struct.ScmObj** %stackaddr$prim55069, align 8
%stackaddr$prim55070 = alloca %struct.ScmObj*, align 8
%cpsargs47423 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47413, %struct.ScmObj* %anf_45bind47260)
store volatile %struct.ScmObj* %cpsargs47423, %struct.ScmObj** %stackaddr$prim55070, align 8
%clofunc55071 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl47172)
musttail call tailcc void %clofunc55071(%struct.ScmObj* %_37foldl47172, %struct.ScmObj* %cpsargs47423)
ret void
}

define tailcc void @proc_clo$ae48929(%struct.ScmObj* %env$ae48929,%struct.ScmObj* %current_45args53684) {
%stackaddr$prim55072 = alloca %struct.ScmObj*, align 8
%k47425 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53684)
store volatile %struct.ScmObj* %k47425, %struct.ScmObj** %stackaddr$prim55072, align 8
%stackaddr$prim55073 = alloca %struct.ScmObj*, align 8
%current_45args53685 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53684)
store volatile %struct.ScmObj* %current_45args53685, %struct.ScmObj** %stackaddr$prim55073, align 8
%stackaddr$prim55074 = alloca %struct.ScmObj*, align 8
%a47185 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53685)
store volatile %struct.ScmObj* %a47185, %struct.ScmObj** %stackaddr$prim55074, align 8
%stackaddr$prim55075 = alloca %struct.ScmObj*, align 8
%current_45args53686 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53685)
store volatile %struct.ScmObj* %current_45args53686, %struct.ScmObj** %stackaddr$prim55075, align 8
%stackaddr$prim55076 = alloca %struct.ScmObj*, align 8
%b47184 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53686)
store volatile %struct.ScmObj* %b47184, %struct.ScmObj** %stackaddr$prim55076, align 8
%stackaddr$prim55077 = alloca %struct.ScmObj*, align 8
%cpsprim47426 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a47185, %struct.ScmObj* %b47184)
store volatile %struct.ScmObj* %cpsprim47426, %struct.ScmObj** %stackaddr$prim55077, align 8
%ae48933 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53688$k474250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55078 = alloca %struct.ScmObj*, align 8
%argslist53688$k474251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47426, %struct.ScmObj* %argslist53688$k474250)
store volatile %struct.ScmObj* %argslist53688$k474251, %struct.ScmObj** %stackaddr$prim55078, align 8
%stackaddr$prim55079 = alloca %struct.ScmObj*, align 8
%argslist53688$k474252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48933, %struct.ScmObj* %argslist53688$k474251)
store volatile %struct.ScmObj* %argslist53688$k474252, %struct.ScmObj** %stackaddr$prim55079, align 8
%clofunc55080 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47425)
musttail call tailcc void %clofunc55080(%struct.ScmObj* %k47425, %struct.ScmObj* %argslist53688$k474252)
ret void
}

define tailcc void @proc_clo$ae48905(%struct.ScmObj* %env$ae48905,%struct.ScmObj* %current_45args53691) {
%stackaddr$prim55081 = alloca %struct.ScmObj*, align 8
%k47427 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53691)
store volatile %struct.ScmObj* %k47427, %struct.ScmObj** %stackaddr$prim55081, align 8
%stackaddr$prim55082 = alloca %struct.ScmObj*, align 8
%current_45args53692 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53691)
store volatile %struct.ScmObj* %current_45args53692, %struct.ScmObj** %stackaddr$prim55082, align 8
%stackaddr$prim55083 = alloca %struct.ScmObj*, align 8
%x47180 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53692)
store volatile %struct.ScmObj* %x47180, %struct.ScmObj** %stackaddr$prim55083, align 8
%stackaddr$prim55084 = alloca %struct.ScmObj*, align 8
%cpsprim47428 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47180)
store volatile %struct.ScmObj* %cpsprim47428, %struct.ScmObj** %stackaddr$prim55084, align 8
%ae48908 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53694$k474270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55085 = alloca %struct.ScmObj*, align 8
%argslist53694$k474271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47428, %struct.ScmObj* %argslist53694$k474270)
store volatile %struct.ScmObj* %argslist53694$k474271, %struct.ScmObj** %stackaddr$prim55085, align 8
%stackaddr$prim55086 = alloca %struct.ScmObj*, align 8
%argslist53694$k474272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48908, %struct.ScmObj* %argslist53694$k474271)
store volatile %struct.ScmObj* %argslist53694$k474272, %struct.ScmObj** %stackaddr$prim55086, align 8
%clofunc55087 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47427)
musttail call tailcc void %clofunc55087(%struct.ScmObj* %k47427, %struct.ScmObj* %argslist53694$k474272)
ret void
}

define tailcc void @proc_clo$ae48881(%struct.ScmObj* %env$ae48881,%struct.ScmObj* %current_45args53697) {
%stackaddr$prim55088 = alloca %struct.ScmObj*, align 8
%k47429 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53697)
store volatile %struct.ScmObj* %k47429, %struct.ScmObj** %stackaddr$prim55088, align 8
%stackaddr$prim55089 = alloca %struct.ScmObj*, align 8
%current_45args53698 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53697)
store volatile %struct.ScmObj* %current_45args53698, %struct.ScmObj** %stackaddr$prim55089, align 8
%stackaddr$prim55090 = alloca %struct.ScmObj*, align 8
%x47182 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53698)
store volatile %struct.ScmObj* %x47182, %struct.ScmObj** %stackaddr$prim55090, align 8
%stackaddr$prim55091 = alloca %struct.ScmObj*, align 8
%cpsprim47430 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47182)
store volatile %struct.ScmObj* %cpsprim47430, %struct.ScmObj** %stackaddr$prim55091, align 8
%ae48884 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53700$k474290 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55092 = alloca %struct.ScmObj*, align 8
%argslist53700$k474291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47430, %struct.ScmObj* %argslist53700$k474290)
store volatile %struct.ScmObj* %argslist53700$k474291, %struct.ScmObj** %stackaddr$prim55092, align 8
%stackaddr$prim55093 = alloca %struct.ScmObj*, align 8
%argslist53700$k474292 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48884, %struct.ScmObj* %argslist53700$k474291)
store volatile %struct.ScmObj* %argslist53700$k474292, %struct.ScmObj** %stackaddr$prim55093, align 8
%clofunc55094 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47429)
musttail call tailcc void %clofunc55094(%struct.ScmObj* %k47429, %struct.ScmObj* %argslist53700$k474292)
ret void
}

define tailcc void @proc_clo$ae48833(%struct.ScmObj* %env$ae48833,%struct.ScmObj* %current_45args53703) {
%stackaddr$prim55095 = alloca %struct.ScmObj*, align 8
%k47431 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53703)
store volatile %struct.ScmObj* %k47431, %struct.ScmObj** %stackaddr$prim55095, align 8
%stackaddr$prim55096 = alloca %struct.ScmObj*, align 8
%current_45args53704 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53703)
store volatile %struct.ScmObj* %current_45args53704, %struct.ScmObj** %stackaddr$prim55096, align 8
%stackaddr$prim55097 = alloca %struct.ScmObj*, align 8
%lst47178 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53704)
store volatile %struct.ScmObj* %lst47178, %struct.ScmObj** %stackaddr$prim55097, align 8
%stackaddr$prim55098 = alloca %struct.ScmObj*, align 8
%current_45args53705 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53704)
store volatile %struct.ScmObj* %current_45args53705, %struct.ScmObj** %stackaddr$prim55098, align 8
%stackaddr$prim55099 = alloca %struct.ScmObj*, align 8
%b47177 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53705)
store volatile %struct.ScmObj* %b47177, %struct.ScmObj** %stackaddr$prim55099, align 8
%truthy$cmp55100 = call i64 @is_truthy_value(%struct.ScmObj* %b47177)
%cmp$cmp55100 = icmp eq i64 %truthy$cmp55100, 1
br i1 %cmp$cmp55100, label %truebranch$cmp55100, label %falsebranch$cmp55100
truebranch$cmp55100:
%ae48836 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53707$k474310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55101 = alloca %struct.ScmObj*, align 8
%argslist53707$k474311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b47177, %struct.ScmObj* %argslist53707$k474310)
store volatile %struct.ScmObj* %argslist53707$k474311, %struct.ScmObj** %stackaddr$prim55101, align 8
%stackaddr$prim55102 = alloca %struct.ScmObj*, align 8
%argslist53707$k474312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48836, %struct.ScmObj* %argslist53707$k474311)
store volatile %struct.ScmObj* %argslist53707$k474312, %struct.ScmObj** %stackaddr$prim55102, align 8
%clofunc55103 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47431)
musttail call tailcc void %clofunc55103(%struct.ScmObj* %k47431, %struct.ScmObj* %argslist53707$k474312)
ret void
falsebranch$cmp55100:
%stackaddr$prim55104 = alloca %struct.ScmObj*, align 8
%cpsprim47432 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47178)
store volatile %struct.ScmObj* %cpsprim47432, %struct.ScmObj** %stackaddr$prim55104, align 8
%ae48843 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53708$k474310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55105 = alloca %struct.ScmObj*, align 8
%argslist53708$k474311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47432, %struct.ScmObj* %argslist53708$k474310)
store volatile %struct.ScmObj* %argslist53708$k474311, %struct.ScmObj** %stackaddr$prim55105, align 8
%stackaddr$prim55106 = alloca %struct.ScmObj*, align 8
%argslist53708$k474312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48843, %struct.ScmObj* %argslist53708$k474311)
store volatile %struct.ScmObj* %argslist53708$k474312, %struct.ScmObj** %stackaddr$prim55106, align 8
%clofunc55107 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47431)
musttail call tailcc void %clofunc55107(%struct.ScmObj* %k47431, %struct.ScmObj* %argslist53708$k474312)
ret void
}

define tailcc void @proc_clo$ae48674(%struct.ScmObj* %env$ae48674,%struct.ScmObj* %args4711647433) {
%stackaddr$env-ref55108 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48674, i64 0)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref55108
%stackaddr$env-ref55109 = alloca %struct.ScmObj*, align 8
%_37drop_45right47108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48674, i64 1)
store %struct.ScmObj* %_37drop_45right47108, %struct.ScmObj** %stackaddr$env-ref55109
%stackaddr$env-ref55110 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48674, i64 2)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref55110
%stackaddr$prim55111 = alloca %struct.ScmObj*, align 8
%k47434 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4711647433)
store volatile %struct.ScmObj* %k47434, %struct.ScmObj** %stackaddr$prim55111, align 8
%stackaddr$prim55112 = alloca %struct.ScmObj*, align 8
%args47116 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4711647433)
store volatile %struct.ScmObj* %args47116, %struct.ScmObj** %stackaddr$prim55112, align 8
%stackaddr$prim55113 = alloca %struct.ScmObj*, align 8
%f47118 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47116)
store volatile %struct.ScmObj* %f47118, %struct.ScmObj** %stackaddr$prim55113, align 8
%stackaddr$prim55114 = alloca %struct.ScmObj*, align 8
%lsts47117 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47116)
store volatile %struct.ScmObj* %lsts47117, %struct.ScmObj** %stackaddr$prim55114, align 8
%stackaddr$makeclosure55115 = alloca %struct.ScmObj*, align 8
%fptrToInt55116 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48679 to i64
%ae48679 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55116)
store volatile %struct.ScmObj* %ae48679, %struct.ScmObj** %stackaddr$makeclosure55115, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48679, %struct.ScmObj* %_37foldr47094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48679, %struct.ScmObj* %lsts47117, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48679, %struct.ScmObj* %k47434, i64 2)
%ae48680 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55117 = alloca %struct.ScmObj*, align 8
%fptrToInt55118 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48681 to i64
%ae48681 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55118)
store volatile %struct.ScmObj* %ae48681, %struct.ScmObj** %stackaddr$makeclosure55117, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48681, %struct.ScmObj* %_37drop_45right47108, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48681, %struct.ScmObj* %f47118, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48681, %struct.ScmObj* %_37last47111, i64 2)
%argslist53727$ae486790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55119 = alloca %struct.ScmObj*, align 8
%argslist53727$ae486791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48681, %struct.ScmObj* %argslist53727$ae486790)
store volatile %struct.ScmObj* %argslist53727$ae486791, %struct.ScmObj** %stackaddr$prim55119, align 8
%stackaddr$prim55120 = alloca %struct.ScmObj*, align 8
%argslist53727$ae486792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48680, %struct.ScmObj* %argslist53727$ae486791)
store volatile %struct.ScmObj* %argslist53727$ae486792, %struct.ScmObj** %stackaddr$prim55120, align 8
%clofunc55121 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48679)
musttail call tailcc void %clofunc55121(%struct.ScmObj* %ae48679, %struct.ScmObj* %argslist53727$ae486792)
ret void
}

define tailcc void @proc_clo$ae48679(%struct.ScmObj* %env$ae48679,%struct.ScmObj* %current_45args53712) {
%stackaddr$env-ref55122 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48679, i64 0)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref55122
%stackaddr$env-ref55123 = alloca %struct.ScmObj*, align 8
%lsts47117 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48679, i64 1)
store %struct.ScmObj* %lsts47117, %struct.ScmObj** %stackaddr$env-ref55123
%stackaddr$env-ref55124 = alloca %struct.ScmObj*, align 8
%k47434 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48679, i64 2)
store %struct.ScmObj* %k47434, %struct.ScmObj** %stackaddr$env-ref55124
%stackaddr$prim55125 = alloca %struct.ScmObj*, align 8
%_95k47435 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53712)
store volatile %struct.ScmObj* %_95k47435, %struct.ScmObj** %stackaddr$prim55125, align 8
%stackaddr$prim55126 = alloca %struct.ScmObj*, align 8
%current_45args53713 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53712)
store volatile %struct.ScmObj* %current_45args53713, %struct.ScmObj** %stackaddr$prim55126, align 8
%stackaddr$prim55127 = alloca %struct.ScmObj*, align 8
%anf_45bind47247 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53713)
store volatile %struct.ScmObj* %anf_45bind47247, %struct.ScmObj** %stackaddr$prim55127, align 8
%ae48742 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55128 = alloca %struct.ScmObj*, align 8
%anf_45bind47248 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48742, %struct.ScmObj* %lsts47117)
store volatile %struct.ScmObj* %anf_45bind47248, %struct.ScmObj** %stackaddr$prim55128, align 8
%stackaddr$prim55129 = alloca %struct.ScmObj*, align 8
%anf_45bind47249 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47247, %struct.ScmObj* %anf_45bind47248)
store volatile %struct.ScmObj* %anf_45bind47249, %struct.ScmObj** %stackaddr$prim55129, align 8
%stackaddr$prim55130 = alloca %struct.ScmObj*, align 8
%cpsargs47436 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47434, %struct.ScmObj* %anf_45bind47249)
store volatile %struct.ScmObj* %cpsargs47436, %struct.ScmObj** %stackaddr$prim55130, align 8
%clofunc55131 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr47094)
musttail call tailcc void %clofunc55131(%struct.ScmObj* %_37foldr47094, %struct.ScmObj* %cpsargs47436)
ret void
}

define tailcc void @proc_clo$ae48681(%struct.ScmObj* %env$ae48681,%struct.ScmObj* %fargs4711947437) {
%stackaddr$env-ref55132 = alloca %struct.ScmObj*, align 8
%_37drop_45right47108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48681, i64 0)
store %struct.ScmObj* %_37drop_45right47108, %struct.ScmObj** %stackaddr$env-ref55132
%stackaddr$env-ref55133 = alloca %struct.ScmObj*, align 8
%f47118 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48681, i64 1)
store %struct.ScmObj* %f47118, %struct.ScmObj** %stackaddr$env-ref55133
%stackaddr$env-ref55134 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48681, i64 2)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref55134
%stackaddr$prim55135 = alloca %struct.ScmObj*, align 8
%k47438 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs4711947437)
store volatile %struct.ScmObj* %k47438, %struct.ScmObj** %stackaddr$prim55135, align 8
%stackaddr$prim55136 = alloca %struct.ScmObj*, align 8
%fargs47119 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs4711947437)
store volatile %struct.ScmObj* %fargs47119, %struct.ScmObj** %stackaddr$prim55136, align 8
%stackaddr$makeclosure55137 = alloca %struct.ScmObj*, align 8
%fptrToInt55138 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48685 to i64
%ae48685 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55138)
store volatile %struct.ScmObj* %ae48685, %struct.ScmObj** %stackaddr$makeclosure55137, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48685, %struct.ScmObj* %k47438, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48685, %struct.ScmObj* %f47118, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48685, %struct.ScmObj* %fargs47119, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48685, %struct.ScmObj* %_37last47111, i64 3)
%ae48687 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist53726$_37drop_45right471080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55139 = alloca %struct.ScmObj*, align 8
%argslist53726$_37drop_45right471081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48687, %struct.ScmObj* %argslist53726$_37drop_45right471080)
store volatile %struct.ScmObj* %argslist53726$_37drop_45right471081, %struct.ScmObj** %stackaddr$prim55139, align 8
%stackaddr$prim55140 = alloca %struct.ScmObj*, align 8
%argslist53726$_37drop_45right471082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs47119, %struct.ScmObj* %argslist53726$_37drop_45right471081)
store volatile %struct.ScmObj* %argslist53726$_37drop_45right471082, %struct.ScmObj** %stackaddr$prim55140, align 8
%stackaddr$prim55141 = alloca %struct.ScmObj*, align 8
%argslist53726$_37drop_45right471083 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48685, %struct.ScmObj* %argslist53726$_37drop_45right471082)
store volatile %struct.ScmObj* %argslist53726$_37drop_45right471083, %struct.ScmObj** %stackaddr$prim55141, align 8
%clofunc55142 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right47108)
musttail call tailcc void %clofunc55142(%struct.ScmObj* %_37drop_45right47108, %struct.ScmObj* %argslist53726$_37drop_45right471083)
ret void
}

define tailcc void @proc_clo$ae48685(%struct.ScmObj* %env$ae48685,%struct.ScmObj* %current_45args53715) {
%stackaddr$env-ref55143 = alloca %struct.ScmObj*, align 8
%k47438 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48685, i64 0)
store %struct.ScmObj* %k47438, %struct.ScmObj** %stackaddr$env-ref55143
%stackaddr$env-ref55144 = alloca %struct.ScmObj*, align 8
%f47118 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48685, i64 1)
store %struct.ScmObj* %f47118, %struct.ScmObj** %stackaddr$env-ref55144
%stackaddr$env-ref55145 = alloca %struct.ScmObj*, align 8
%fargs47119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48685, i64 2)
store %struct.ScmObj* %fargs47119, %struct.ScmObj** %stackaddr$env-ref55145
%stackaddr$env-ref55146 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48685, i64 3)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref55146
%stackaddr$prim55147 = alloca %struct.ScmObj*, align 8
%_95k47439 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53715)
store volatile %struct.ScmObj* %_95k47439, %struct.ScmObj** %stackaddr$prim55147, align 8
%stackaddr$prim55148 = alloca %struct.ScmObj*, align 8
%current_45args53716 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53715)
store volatile %struct.ScmObj* %current_45args53716, %struct.ScmObj** %stackaddr$prim55148, align 8
%stackaddr$prim55149 = alloca %struct.ScmObj*, align 8
%anf_45bind47244 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53716)
store volatile %struct.ScmObj* %anf_45bind47244, %struct.ScmObj** %stackaddr$prim55149, align 8
%stackaddr$makeclosure55150 = alloca %struct.ScmObj*, align 8
%fptrToInt55151 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48692 to i64
%ae48692 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55151)
store volatile %struct.ScmObj* %ae48692, %struct.ScmObj** %stackaddr$makeclosure55150, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48692, %struct.ScmObj* %fargs47119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48692, %struct.ScmObj* %k47438, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48692, %struct.ScmObj* %_37last47111, i64 2)
%stackaddr$prim55152 = alloca %struct.ScmObj*, align 8
%cpsargs47443 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48692, %struct.ScmObj* %anf_45bind47244)
store volatile %struct.ScmObj* %cpsargs47443, %struct.ScmObj** %stackaddr$prim55152, align 8
%clofunc55153 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47118)
musttail call tailcc void %clofunc55153(%struct.ScmObj* %f47118, %struct.ScmObj* %cpsargs47443)
ret void
}

define tailcc void @proc_clo$ae48692(%struct.ScmObj* %env$ae48692,%struct.ScmObj* %current_45args53718) {
%stackaddr$env-ref55154 = alloca %struct.ScmObj*, align 8
%fargs47119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48692, i64 0)
store %struct.ScmObj* %fargs47119, %struct.ScmObj** %stackaddr$env-ref55154
%stackaddr$env-ref55155 = alloca %struct.ScmObj*, align 8
%k47438 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48692, i64 1)
store %struct.ScmObj* %k47438, %struct.ScmObj** %stackaddr$env-ref55155
%stackaddr$env-ref55156 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48692, i64 2)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref55156
%stackaddr$prim55157 = alloca %struct.ScmObj*, align 8
%_95k47440 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53718)
store volatile %struct.ScmObj* %_95k47440, %struct.ScmObj** %stackaddr$prim55157, align 8
%stackaddr$prim55158 = alloca %struct.ScmObj*, align 8
%current_45args53719 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53718)
store volatile %struct.ScmObj* %current_45args53719, %struct.ScmObj** %stackaddr$prim55158, align 8
%stackaddr$prim55159 = alloca %struct.ScmObj*, align 8
%anf_45bind47245 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53719)
store volatile %struct.ScmObj* %anf_45bind47245, %struct.ScmObj** %stackaddr$prim55159, align 8
%stackaddr$makeclosure55160 = alloca %struct.ScmObj*, align 8
%fptrToInt55161 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48697 to i64
%ae48697 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55161)
store volatile %struct.ScmObj* %ae48697, %struct.ScmObj** %stackaddr$makeclosure55160, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48697, %struct.ScmObj* %k47438, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48697, %struct.ScmObj* %anf_45bind47245, i64 1)
%argslist53725$_37last471110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55162 = alloca %struct.ScmObj*, align 8
%argslist53725$_37last471111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs47119, %struct.ScmObj* %argslist53725$_37last471110)
store volatile %struct.ScmObj* %argslist53725$_37last471111, %struct.ScmObj** %stackaddr$prim55162, align 8
%stackaddr$prim55163 = alloca %struct.ScmObj*, align 8
%argslist53725$_37last471112 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48697, %struct.ScmObj* %argslist53725$_37last471111)
store volatile %struct.ScmObj* %argslist53725$_37last471112, %struct.ScmObj** %stackaddr$prim55163, align 8
%clofunc55164 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last47111)
musttail call tailcc void %clofunc55164(%struct.ScmObj* %_37last47111, %struct.ScmObj* %argslist53725$_37last471112)
ret void
}

define tailcc void @proc_clo$ae48697(%struct.ScmObj* %env$ae48697,%struct.ScmObj* %current_45args53721) {
%stackaddr$env-ref55165 = alloca %struct.ScmObj*, align 8
%k47438 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48697, i64 0)
store %struct.ScmObj* %k47438, %struct.ScmObj** %stackaddr$env-ref55165
%stackaddr$env-ref55166 = alloca %struct.ScmObj*, align 8
%anf_45bind47245 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48697, i64 1)
store %struct.ScmObj* %anf_45bind47245, %struct.ScmObj** %stackaddr$env-ref55166
%stackaddr$prim55167 = alloca %struct.ScmObj*, align 8
%_95k47441 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53721)
store volatile %struct.ScmObj* %_95k47441, %struct.ScmObj** %stackaddr$prim55167, align 8
%stackaddr$prim55168 = alloca %struct.ScmObj*, align 8
%current_45args53722 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53721)
store volatile %struct.ScmObj* %current_45args53722, %struct.ScmObj** %stackaddr$prim55168, align 8
%stackaddr$prim55169 = alloca %struct.ScmObj*, align 8
%anf_45bind47246 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53722)
store volatile %struct.ScmObj* %anf_45bind47246, %struct.ScmObj** %stackaddr$prim55169, align 8
%stackaddr$prim55170 = alloca %struct.ScmObj*, align 8
%cpsprim47442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47245, %struct.ScmObj* %anf_45bind47246)
store volatile %struct.ScmObj* %cpsprim47442, %struct.ScmObj** %stackaddr$prim55170, align 8
%ae48702 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53724$k474380 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55171 = alloca %struct.ScmObj*, align 8
%argslist53724$k474381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47442, %struct.ScmObj* %argslist53724$k474380)
store volatile %struct.ScmObj* %argslist53724$k474381, %struct.ScmObj** %stackaddr$prim55171, align 8
%stackaddr$prim55172 = alloca %struct.ScmObj*, align 8
%argslist53724$k474382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48702, %struct.ScmObj* %argslist53724$k474381)
store volatile %struct.ScmObj* %argslist53724$k474382, %struct.ScmObj** %stackaddr$prim55172, align 8
%clofunc55173 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47438)
musttail call tailcc void %clofunc55173(%struct.ScmObj* %k47438, %struct.ScmObj* %argslist53724$k474382)
ret void
}

define tailcc void @proc_clo$ae48597(%struct.ScmObj* %env$ae48597,%struct.ScmObj* %current_45args53729) {
%stackaddr$env-ref55174 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48597, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55174
%stackaddr$prim55175 = alloca %struct.ScmObj*, align 8
%k47444 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53729)
store volatile %struct.ScmObj* %k47444, %struct.ScmObj** %stackaddr$prim55175, align 8
%stackaddr$prim55176 = alloca %struct.ScmObj*, align 8
%current_45args53730 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53729)
store volatile %struct.ScmObj* %current_45args53730, %struct.ScmObj** %stackaddr$prim55176, align 8
%stackaddr$prim55177 = alloca %struct.ScmObj*, align 8
%f47122 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53730)
store volatile %struct.ScmObj* %f47122, %struct.ScmObj** %stackaddr$prim55177, align 8
%stackaddr$prim55178 = alloca %struct.ScmObj*, align 8
%current_45args53731 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53730)
store volatile %struct.ScmObj* %current_45args53731, %struct.ScmObj** %stackaddr$prim55178, align 8
%stackaddr$prim55179 = alloca %struct.ScmObj*, align 8
%lst47121 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53731)
store volatile %struct.ScmObj* %lst47121, %struct.ScmObj** %stackaddr$prim55179, align 8
%stackaddr$makeclosure55180 = alloca %struct.ScmObj*, align 8
%fptrToInt55181 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48598 to i64
%ae48598 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55181)
store volatile %struct.ScmObj* %ae48598, %struct.ScmObj** %stackaddr$makeclosure55180, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48598, %struct.ScmObj* %lst47121, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48598, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48598, %struct.ScmObj* %k47444, i64 2)
%ae48599 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55182 = alloca %struct.ScmObj*, align 8
%fptrToInt55183 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48600 to i64
%ae48600 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55183)
store volatile %struct.ScmObj* %ae48600, %struct.ScmObj** %stackaddr$makeclosure55182, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48600, %struct.ScmObj* %f47122, i64 0)
%argslist53746$ae485980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55184 = alloca %struct.ScmObj*, align 8
%argslist53746$ae485981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48600, %struct.ScmObj* %argslist53746$ae485980)
store volatile %struct.ScmObj* %argslist53746$ae485981, %struct.ScmObj** %stackaddr$prim55184, align 8
%stackaddr$prim55185 = alloca %struct.ScmObj*, align 8
%argslist53746$ae485982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48599, %struct.ScmObj* %argslist53746$ae485981)
store volatile %struct.ScmObj* %argslist53746$ae485982, %struct.ScmObj** %stackaddr$prim55185, align 8
%clofunc55186 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48598)
musttail call tailcc void %clofunc55186(%struct.ScmObj* %ae48598, %struct.ScmObj* %argslist53746$ae485982)
ret void
}

define tailcc void @proc_clo$ae48598(%struct.ScmObj* %env$ae48598,%struct.ScmObj* %current_45args53733) {
%stackaddr$env-ref55187 = alloca %struct.ScmObj*, align 8
%lst47121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48598, i64 0)
store %struct.ScmObj* %lst47121, %struct.ScmObj** %stackaddr$env-ref55187
%stackaddr$env-ref55188 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48598, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55188
%stackaddr$env-ref55189 = alloca %struct.ScmObj*, align 8
%k47444 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48598, i64 2)
store %struct.ScmObj* %k47444, %struct.ScmObj** %stackaddr$env-ref55189
%stackaddr$prim55190 = alloca %struct.ScmObj*, align 8
%_95k47445 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53733)
store volatile %struct.ScmObj* %_95k47445, %struct.ScmObj** %stackaddr$prim55190, align 8
%stackaddr$prim55191 = alloca %struct.ScmObj*, align 8
%current_45args53734 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53733)
store volatile %struct.ScmObj* %current_45args53734, %struct.ScmObj** %stackaddr$prim55191, align 8
%stackaddr$prim55192 = alloca %struct.ScmObj*, align 8
%anf_45bind47243 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53734)
store volatile %struct.ScmObj* %anf_45bind47243, %struct.ScmObj** %stackaddr$prim55192, align 8
%ae48632 = call %struct.ScmObj* @const_init_null()
%argslist53736$_37foldr1470890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55193 = alloca %struct.ScmObj*, align 8
%argslist53736$_37foldr1470891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47121, %struct.ScmObj* %argslist53736$_37foldr1470890)
store volatile %struct.ScmObj* %argslist53736$_37foldr1470891, %struct.ScmObj** %stackaddr$prim55193, align 8
%stackaddr$prim55194 = alloca %struct.ScmObj*, align 8
%argslist53736$_37foldr1470892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48632, %struct.ScmObj* %argslist53736$_37foldr1470891)
store volatile %struct.ScmObj* %argslist53736$_37foldr1470892, %struct.ScmObj** %stackaddr$prim55194, align 8
%stackaddr$prim55195 = alloca %struct.ScmObj*, align 8
%argslist53736$_37foldr1470893 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47243, %struct.ScmObj* %argslist53736$_37foldr1470892)
store volatile %struct.ScmObj* %argslist53736$_37foldr1470893, %struct.ScmObj** %stackaddr$prim55195, align 8
%stackaddr$prim55196 = alloca %struct.ScmObj*, align 8
%argslist53736$_37foldr1470894 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47444, %struct.ScmObj* %argslist53736$_37foldr1470893)
store volatile %struct.ScmObj* %argslist53736$_37foldr1470894, %struct.ScmObj** %stackaddr$prim55196, align 8
%clofunc55197 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147089)
musttail call tailcc void %clofunc55197(%struct.ScmObj* %_37foldr147089, %struct.ScmObj* %argslist53736$_37foldr1470894)
ret void
}

define tailcc void @proc_clo$ae48600(%struct.ScmObj* %env$ae48600,%struct.ScmObj* %current_45args53737) {
%stackaddr$env-ref55198 = alloca %struct.ScmObj*, align 8
%f47122 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48600, i64 0)
store %struct.ScmObj* %f47122, %struct.ScmObj** %stackaddr$env-ref55198
%stackaddr$prim55199 = alloca %struct.ScmObj*, align 8
%k47446 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53737)
store volatile %struct.ScmObj* %k47446, %struct.ScmObj** %stackaddr$prim55199, align 8
%stackaddr$prim55200 = alloca %struct.ScmObj*, align 8
%current_45args53738 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53737)
store volatile %struct.ScmObj* %current_45args53738, %struct.ScmObj** %stackaddr$prim55200, align 8
%stackaddr$prim55201 = alloca %struct.ScmObj*, align 8
%v47124 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53738)
store volatile %struct.ScmObj* %v47124, %struct.ScmObj** %stackaddr$prim55201, align 8
%stackaddr$prim55202 = alloca %struct.ScmObj*, align 8
%current_45args53739 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53738)
store volatile %struct.ScmObj* %current_45args53739, %struct.ScmObj** %stackaddr$prim55202, align 8
%stackaddr$prim55203 = alloca %struct.ScmObj*, align 8
%r47123 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53739)
store volatile %struct.ScmObj* %r47123, %struct.ScmObj** %stackaddr$prim55203, align 8
%stackaddr$makeclosure55204 = alloca %struct.ScmObj*, align 8
%fptrToInt55205 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48602 to i64
%ae48602 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55205)
store volatile %struct.ScmObj* %ae48602, %struct.ScmObj** %stackaddr$makeclosure55204, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48602, %struct.ScmObj* %k47446, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48602, %struct.ScmObj* %r47123, i64 1)
%argslist53745$f471220 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55206 = alloca %struct.ScmObj*, align 8
%argslist53745$f471221 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v47124, %struct.ScmObj* %argslist53745$f471220)
store volatile %struct.ScmObj* %argslist53745$f471221, %struct.ScmObj** %stackaddr$prim55206, align 8
%stackaddr$prim55207 = alloca %struct.ScmObj*, align 8
%argslist53745$f471222 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48602, %struct.ScmObj* %argslist53745$f471221)
store volatile %struct.ScmObj* %argslist53745$f471222, %struct.ScmObj** %stackaddr$prim55207, align 8
%clofunc55208 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47122)
musttail call tailcc void %clofunc55208(%struct.ScmObj* %f47122, %struct.ScmObj* %argslist53745$f471222)
ret void
}

define tailcc void @proc_clo$ae48602(%struct.ScmObj* %env$ae48602,%struct.ScmObj* %current_45args53741) {
%stackaddr$env-ref55209 = alloca %struct.ScmObj*, align 8
%k47446 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48602, i64 0)
store %struct.ScmObj* %k47446, %struct.ScmObj** %stackaddr$env-ref55209
%stackaddr$env-ref55210 = alloca %struct.ScmObj*, align 8
%r47123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48602, i64 1)
store %struct.ScmObj* %r47123, %struct.ScmObj** %stackaddr$env-ref55210
%stackaddr$prim55211 = alloca %struct.ScmObj*, align 8
%_95k47447 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53741)
store volatile %struct.ScmObj* %_95k47447, %struct.ScmObj** %stackaddr$prim55211, align 8
%stackaddr$prim55212 = alloca %struct.ScmObj*, align 8
%current_45args53742 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53741)
store volatile %struct.ScmObj* %current_45args53742, %struct.ScmObj** %stackaddr$prim55212, align 8
%stackaddr$prim55213 = alloca %struct.ScmObj*, align 8
%anf_45bind47242 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53742)
store volatile %struct.ScmObj* %anf_45bind47242, %struct.ScmObj** %stackaddr$prim55213, align 8
%stackaddr$prim55214 = alloca %struct.ScmObj*, align 8
%cpsprim47448 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47242, %struct.ScmObj* %r47123)
store volatile %struct.ScmObj* %cpsprim47448, %struct.ScmObj** %stackaddr$prim55214, align 8
%ae48607 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53744$k474460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55215 = alloca %struct.ScmObj*, align 8
%argslist53744$k474461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47448, %struct.ScmObj* %argslist53744$k474460)
store volatile %struct.ScmObj* %argslist53744$k474461, %struct.ScmObj** %stackaddr$prim55215, align 8
%stackaddr$prim55216 = alloca %struct.ScmObj*, align 8
%argslist53744$k474462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48607, %struct.ScmObj* %argslist53744$k474461)
store volatile %struct.ScmObj* %argslist53744$k474462, %struct.ScmObj** %stackaddr$prim55216, align 8
%clofunc55217 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47446)
musttail call tailcc void %clofunc55217(%struct.ScmObj* %k47446, %struct.ScmObj* %argslist53744$k474462)
ret void
}

define tailcc void @proc_clo$ae48211(%struct.ScmObj* %env$ae48211,%struct.ScmObj* %current_45args53749) {
%stackaddr$env-ref55218 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48211, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55218
%stackaddr$env-ref55219 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48211, i64 1)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref55219
%stackaddr$prim55220 = alloca %struct.ScmObj*, align 8
%k47449 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53749)
store volatile %struct.ScmObj* %k47449, %struct.ScmObj** %stackaddr$prim55220, align 8
%stackaddr$prim55221 = alloca %struct.ScmObj*, align 8
%current_45args53750 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53749)
store volatile %struct.ScmObj* %current_45args53750, %struct.ScmObj** %stackaddr$prim55221, align 8
%stackaddr$prim55222 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53750)
store volatile %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$prim55222, align 8
%ae48213 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55223 = alloca %struct.ScmObj*, align 8
%fptrToInt55224 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48214 to i64
%ae48214 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55224)
store volatile %struct.ScmObj* %ae48214, %struct.ScmObj** %stackaddr$makeclosure55223, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48214, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48214, %struct.ScmObj* %_37map147085, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48214, %struct.ScmObj* %_37foldr47095, i64 2)
%argslist53807$k474490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55225 = alloca %struct.ScmObj*, align 8
%argslist53807$k474491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48214, %struct.ScmObj* %argslist53807$k474490)
store volatile %struct.ScmObj* %argslist53807$k474491, %struct.ScmObj** %stackaddr$prim55225, align 8
%stackaddr$prim55226 = alloca %struct.ScmObj*, align 8
%argslist53807$k474492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48213, %struct.ScmObj* %argslist53807$k474491)
store volatile %struct.ScmObj* %argslist53807$k474492, %struct.ScmObj** %stackaddr$prim55226, align 8
%clofunc55227 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47449)
musttail call tailcc void %clofunc55227(%struct.ScmObj* %k47449, %struct.ScmObj* %argslist53807$k474492)
ret void
}

define tailcc void @proc_clo$ae48214(%struct.ScmObj* %env$ae48214,%struct.ScmObj* %args4709647450) {
%stackaddr$env-ref55228 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48214, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55228
%stackaddr$env-ref55229 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48214, i64 1)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref55229
%stackaddr$env-ref55230 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48214, i64 2)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55230
%stackaddr$prim55231 = alloca %struct.ScmObj*, align 8
%k47451 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4709647450)
store volatile %struct.ScmObj* %k47451, %struct.ScmObj** %stackaddr$prim55231, align 8
%stackaddr$prim55232 = alloca %struct.ScmObj*, align 8
%args47096 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4709647450)
store volatile %struct.ScmObj* %args47096, %struct.ScmObj** %stackaddr$prim55232, align 8
%stackaddr$prim55233 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47096)
store volatile %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$prim55233, align 8
%stackaddr$prim55234 = alloca %struct.ScmObj*, align 8
%anf_45bind47229 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47096)
store volatile %struct.ScmObj* %anf_45bind47229, %struct.ScmObj** %stackaddr$prim55234, align 8
%stackaddr$prim55235 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47229)
store volatile %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$prim55235, align 8
%stackaddr$prim55236 = alloca %struct.ScmObj*, align 8
%anf_45bind47230 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47096)
store volatile %struct.ScmObj* %anf_45bind47230, %struct.ScmObj** %stackaddr$prim55236, align 8
%stackaddr$prim55237 = alloca %struct.ScmObj*, align 8
%lsts47097 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47230)
store volatile %struct.ScmObj* %lsts47097, %struct.ScmObj** %stackaddr$prim55237, align 8
%stackaddr$makeclosure55238 = alloca %struct.ScmObj*, align 8
%fptrToInt55239 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48222 to i64
%ae48222 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55239)
store volatile %struct.ScmObj* %ae48222, %struct.ScmObj** %stackaddr$makeclosure55238, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48222, %struct.ScmObj* %k47451, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48222, %struct.ScmObj* %f47099, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48222, %struct.ScmObj* %_37foldr147089, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48222, %struct.ScmObj* %_37map147085, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48222, %struct.ScmObj* %acc47098, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48222, %struct.ScmObj* %lsts47097, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48222, %struct.ScmObj* %_37foldr47095, i64 6)
%ae48223 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55240 = alloca %struct.ScmObj*, align 8
%fptrToInt55241 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48224 to i64
%ae48224 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55241)
store volatile %struct.ScmObj* %ae48224, %struct.ScmObj** %stackaddr$makeclosure55240, align 8
%argslist53806$ae482220 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55242 = alloca %struct.ScmObj*, align 8
%argslist53806$ae482221 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48224, %struct.ScmObj* %argslist53806$ae482220)
store volatile %struct.ScmObj* %argslist53806$ae482221, %struct.ScmObj** %stackaddr$prim55242, align 8
%stackaddr$prim55243 = alloca %struct.ScmObj*, align 8
%argslist53806$ae482222 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48223, %struct.ScmObj* %argslist53806$ae482221)
store volatile %struct.ScmObj* %argslist53806$ae482222, %struct.ScmObj** %stackaddr$prim55243, align 8
%clofunc55244 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48222)
musttail call tailcc void %clofunc55244(%struct.ScmObj* %ae48222, %struct.ScmObj* %argslist53806$ae482222)
ret void
}

define tailcc void @proc_clo$ae48222(%struct.ScmObj* %env$ae48222,%struct.ScmObj* %current_45args53752) {
%stackaddr$env-ref55245 = alloca %struct.ScmObj*, align 8
%k47451 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48222, i64 0)
store %struct.ScmObj* %k47451, %struct.ScmObj** %stackaddr$env-ref55245
%stackaddr$env-ref55246 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48222, i64 1)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref55246
%stackaddr$env-ref55247 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48222, i64 2)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55247
%stackaddr$env-ref55248 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48222, i64 3)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref55248
%stackaddr$env-ref55249 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48222, i64 4)
store %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$env-ref55249
%stackaddr$env-ref55250 = alloca %struct.ScmObj*, align 8
%lsts47097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48222, i64 5)
store %struct.ScmObj* %lsts47097, %struct.ScmObj** %stackaddr$env-ref55250
%stackaddr$env-ref55251 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48222, i64 6)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55251
%stackaddr$prim55252 = alloca %struct.ScmObj*, align 8
%_95k47452 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53752)
store volatile %struct.ScmObj* %_95k47452, %struct.ScmObj** %stackaddr$prim55252, align 8
%stackaddr$prim55253 = alloca %struct.ScmObj*, align 8
%current_45args53753 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53752)
store volatile %struct.ScmObj* %current_45args53753, %struct.ScmObj** %stackaddr$prim55253, align 8
%stackaddr$prim55254 = alloca %struct.ScmObj*, align 8
%anf_45bind47231 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53753)
store volatile %struct.ScmObj* %anf_45bind47231, %struct.ScmObj** %stackaddr$prim55254, align 8
%stackaddr$makeclosure55255 = alloca %struct.ScmObj*, align 8
%fptrToInt55256 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48254 to i64
%ae48254 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55256)
store volatile %struct.ScmObj* %ae48254, %struct.ScmObj** %stackaddr$makeclosure55255, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48254, %struct.ScmObj* %k47451, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48254, %struct.ScmObj* %f47099, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48254, %struct.ScmObj* %_37foldr147089, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48254, %struct.ScmObj* %_37map147085, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48254, %struct.ScmObj* %acc47098, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48254, %struct.ScmObj* %lsts47097, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48254, %struct.ScmObj* %_37foldr47095, i64 6)
%ae48256 = call %struct.ScmObj* @const_init_false()
%argslist53799$_37foldr1470890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55257 = alloca %struct.ScmObj*, align 8
%argslist53799$_37foldr1470891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47097, %struct.ScmObj* %argslist53799$_37foldr1470890)
store volatile %struct.ScmObj* %argslist53799$_37foldr1470891, %struct.ScmObj** %stackaddr$prim55257, align 8
%stackaddr$prim55258 = alloca %struct.ScmObj*, align 8
%argslist53799$_37foldr1470892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48256, %struct.ScmObj* %argslist53799$_37foldr1470891)
store volatile %struct.ScmObj* %argslist53799$_37foldr1470892, %struct.ScmObj** %stackaddr$prim55258, align 8
%stackaddr$prim55259 = alloca %struct.ScmObj*, align 8
%argslist53799$_37foldr1470893 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47231, %struct.ScmObj* %argslist53799$_37foldr1470892)
store volatile %struct.ScmObj* %argslist53799$_37foldr1470893, %struct.ScmObj** %stackaddr$prim55259, align 8
%stackaddr$prim55260 = alloca %struct.ScmObj*, align 8
%argslist53799$_37foldr1470894 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48254, %struct.ScmObj* %argslist53799$_37foldr1470893)
store volatile %struct.ScmObj* %argslist53799$_37foldr1470894, %struct.ScmObj** %stackaddr$prim55260, align 8
%clofunc55261 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147089)
musttail call tailcc void %clofunc55261(%struct.ScmObj* %_37foldr147089, %struct.ScmObj* %argslist53799$_37foldr1470894)
ret void
}

define tailcc void @proc_clo$ae48254(%struct.ScmObj* %env$ae48254,%struct.ScmObj* %current_45args53755) {
%stackaddr$env-ref55262 = alloca %struct.ScmObj*, align 8
%k47451 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48254, i64 0)
store %struct.ScmObj* %k47451, %struct.ScmObj** %stackaddr$env-ref55262
%stackaddr$env-ref55263 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48254, i64 1)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref55263
%stackaddr$env-ref55264 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48254, i64 2)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55264
%stackaddr$env-ref55265 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48254, i64 3)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref55265
%stackaddr$env-ref55266 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48254, i64 4)
store %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$env-ref55266
%stackaddr$env-ref55267 = alloca %struct.ScmObj*, align 8
%lsts47097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48254, i64 5)
store %struct.ScmObj* %lsts47097, %struct.ScmObj** %stackaddr$env-ref55267
%stackaddr$env-ref55268 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48254, i64 6)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55268
%stackaddr$prim55269 = alloca %struct.ScmObj*, align 8
%_95k47453 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53755)
store volatile %struct.ScmObj* %_95k47453, %struct.ScmObj** %stackaddr$prim55269, align 8
%stackaddr$prim55270 = alloca %struct.ScmObj*, align 8
%current_45args53756 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53755)
store volatile %struct.ScmObj* %current_45args53756, %struct.ScmObj** %stackaddr$prim55270, align 8
%stackaddr$prim55271 = alloca %struct.ScmObj*, align 8
%anf_45bind47232 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53756)
store volatile %struct.ScmObj* %anf_45bind47232, %struct.ScmObj** %stackaddr$prim55271, align 8
%truthy$cmp55272 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47232)
%cmp$cmp55272 = icmp eq i64 %truthy$cmp55272, 1
br i1 %cmp$cmp55272, label %truebranch$cmp55272, label %falsebranch$cmp55272
truebranch$cmp55272:
%ae48265 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53758$k474510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55273 = alloca %struct.ScmObj*, align 8
%argslist53758$k474511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47098, %struct.ScmObj* %argslist53758$k474510)
store volatile %struct.ScmObj* %argslist53758$k474511, %struct.ScmObj** %stackaddr$prim55273, align 8
%stackaddr$prim55274 = alloca %struct.ScmObj*, align 8
%argslist53758$k474512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48265, %struct.ScmObj* %argslist53758$k474511)
store volatile %struct.ScmObj* %argslist53758$k474512, %struct.ScmObj** %stackaddr$prim55274, align 8
%clofunc55275 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47451)
musttail call tailcc void %clofunc55275(%struct.ScmObj* %k47451, %struct.ScmObj* %argslist53758$k474512)
ret void
falsebranch$cmp55272:
%stackaddr$makeclosure55276 = alloca %struct.ScmObj*, align 8
%fptrToInt55277 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48270 to i64
%ae48270 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55277)
store volatile %struct.ScmObj* %ae48270, %struct.ScmObj** %stackaddr$makeclosure55276, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48270, %struct.ScmObj* %k47451, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48270, %struct.ScmObj* %f47099, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48270, %struct.ScmObj* %_37foldr147089, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48270, %struct.ScmObj* %_37map147085, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48270, %struct.ScmObj* %acc47098, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48270, %struct.ScmObj* %lsts47097, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48270, %struct.ScmObj* %_37foldr47095, i64 6)
%ae48271 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55278 = alloca %struct.ScmObj*, align 8
%fptrToInt55279 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48272 to i64
%ae48272 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55279)
store volatile %struct.ScmObj* %ae48272, %struct.ScmObj** %stackaddr$makeclosure55278, align 8
%argslist53798$ae482700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55280 = alloca %struct.ScmObj*, align 8
%argslist53798$ae482701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48272, %struct.ScmObj* %argslist53798$ae482700)
store volatile %struct.ScmObj* %argslist53798$ae482701, %struct.ScmObj** %stackaddr$prim55280, align 8
%stackaddr$prim55281 = alloca %struct.ScmObj*, align 8
%argslist53798$ae482702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48271, %struct.ScmObj* %argslist53798$ae482701)
store volatile %struct.ScmObj* %argslist53798$ae482702, %struct.ScmObj** %stackaddr$prim55281, align 8
%clofunc55282 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48270)
musttail call tailcc void %clofunc55282(%struct.ScmObj* %ae48270, %struct.ScmObj* %argslist53798$ae482702)
ret void
}

define tailcc void @proc_clo$ae48270(%struct.ScmObj* %env$ae48270,%struct.ScmObj* %current_45args53759) {
%stackaddr$env-ref55283 = alloca %struct.ScmObj*, align 8
%k47451 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48270, i64 0)
store %struct.ScmObj* %k47451, %struct.ScmObj** %stackaddr$env-ref55283
%stackaddr$env-ref55284 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48270, i64 1)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref55284
%stackaddr$env-ref55285 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48270, i64 2)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55285
%stackaddr$env-ref55286 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48270, i64 3)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref55286
%stackaddr$env-ref55287 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48270, i64 4)
store %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$env-ref55287
%stackaddr$env-ref55288 = alloca %struct.ScmObj*, align 8
%lsts47097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48270, i64 5)
store %struct.ScmObj* %lsts47097, %struct.ScmObj** %stackaddr$env-ref55288
%stackaddr$env-ref55289 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48270, i64 6)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55289
%stackaddr$prim55290 = alloca %struct.ScmObj*, align 8
%_95k47454 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53759)
store volatile %struct.ScmObj* %_95k47454, %struct.ScmObj** %stackaddr$prim55290, align 8
%stackaddr$prim55291 = alloca %struct.ScmObj*, align 8
%current_45args53760 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53759)
store volatile %struct.ScmObj* %current_45args53760, %struct.ScmObj** %stackaddr$prim55291, align 8
%stackaddr$prim55292 = alloca %struct.ScmObj*, align 8
%anf_45bind47233 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53760)
store volatile %struct.ScmObj* %anf_45bind47233, %struct.ScmObj** %stackaddr$prim55292, align 8
%stackaddr$makeclosure55293 = alloca %struct.ScmObj*, align 8
%fptrToInt55294 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48291 to i64
%ae48291 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55294)
store volatile %struct.ScmObj* %ae48291, %struct.ScmObj** %stackaddr$makeclosure55293, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48291, %struct.ScmObj* %k47451, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48291, %struct.ScmObj* %f47099, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48291, %struct.ScmObj* %_37foldr147089, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48291, %struct.ScmObj* %_37map147085, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48291, %struct.ScmObj* %acc47098, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48291, %struct.ScmObj* %lsts47097, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48291, %struct.ScmObj* %_37foldr47095, i64 6)
%argslist53793$_37map1470850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55295 = alloca %struct.ScmObj*, align 8
%argslist53793$_37map1470851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47097, %struct.ScmObj* %argslist53793$_37map1470850)
store volatile %struct.ScmObj* %argslist53793$_37map1470851, %struct.ScmObj** %stackaddr$prim55295, align 8
%stackaddr$prim55296 = alloca %struct.ScmObj*, align 8
%argslist53793$_37map1470852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47233, %struct.ScmObj* %argslist53793$_37map1470851)
store volatile %struct.ScmObj* %argslist53793$_37map1470852, %struct.ScmObj** %stackaddr$prim55296, align 8
%stackaddr$prim55297 = alloca %struct.ScmObj*, align 8
%argslist53793$_37map1470853 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48291, %struct.ScmObj* %argslist53793$_37map1470852)
store volatile %struct.ScmObj* %argslist53793$_37map1470853, %struct.ScmObj** %stackaddr$prim55297, align 8
%clofunc55298 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147085)
musttail call tailcc void %clofunc55298(%struct.ScmObj* %_37map147085, %struct.ScmObj* %argslist53793$_37map1470853)
ret void
}

define tailcc void @proc_clo$ae48291(%struct.ScmObj* %env$ae48291,%struct.ScmObj* %current_45args53762) {
%stackaddr$env-ref55299 = alloca %struct.ScmObj*, align 8
%k47451 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48291, i64 0)
store %struct.ScmObj* %k47451, %struct.ScmObj** %stackaddr$env-ref55299
%stackaddr$env-ref55300 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48291, i64 1)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref55300
%stackaddr$env-ref55301 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48291, i64 2)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55301
%stackaddr$env-ref55302 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48291, i64 3)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref55302
%stackaddr$env-ref55303 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48291, i64 4)
store %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$env-ref55303
%stackaddr$env-ref55304 = alloca %struct.ScmObj*, align 8
%lsts47097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48291, i64 5)
store %struct.ScmObj* %lsts47097, %struct.ScmObj** %stackaddr$env-ref55304
%stackaddr$env-ref55305 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48291, i64 6)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55305
%stackaddr$prim55306 = alloca %struct.ScmObj*, align 8
%_95k47455 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53762)
store volatile %struct.ScmObj* %_95k47455, %struct.ScmObj** %stackaddr$prim55306, align 8
%stackaddr$prim55307 = alloca %struct.ScmObj*, align 8
%current_45args53763 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53762)
store volatile %struct.ScmObj* %current_45args53763, %struct.ScmObj** %stackaddr$prim55307, align 8
%stackaddr$prim55308 = alloca %struct.ScmObj*, align 8
%lsts_4347104 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53763)
store volatile %struct.ScmObj* %lsts_4347104, %struct.ScmObj** %stackaddr$prim55308, align 8
%stackaddr$makeclosure55309 = alloca %struct.ScmObj*, align 8
%fptrToInt55310 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48294 to i64
%ae48294 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt55310)
store volatile %struct.ScmObj* %ae48294, %struct.ScmObj** %stackaddr$makeclosure55309, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48294, %struct.ScmObj* %k47451, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48294, %struct.ScmObj* %f47099, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48294, %struct.ScmObj* %_37foldr147089, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48294, %struct.ScmObj* %lsts_4347104, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48294, %struct.ScmObj* %_37map147085, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48294, %struct.ScmObj* %acc47098, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48294, %struct.ScmObj* %lsts47097, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae48294, %struct.ScmObj* %_37foldr47095, i64 7)
%ae48295 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55311 = alloca %struct.ScmObj*, align 8
%fptrToInt55312 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48296 to i64
%ae48296 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55312)
store volatile %struct.ScmObj* %ae48296, %struct.ScmObj** %stackaddr$makeclosure55311, align 8
%argslist53792$ae482940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55313 = alloca %struct.ScmObj*, align 8
%argslist53792$ae482941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48296, %struct.ScmObj* %argslist53792$ae482940)
store volatile %struct.ScmObj* %argslist53792$ae482941, %struct.ScmObj** %stackaddr$prim55313, align 8
%stackaddr$prim55314 = alloca %struct.ScmObj*, align 8
%argslist53792$ae482942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48295, %struct.ScmObj* %argslist53792$ae482941)
store volatile %struct.ScmObj* %argslist53792$ae482942, %struct.ScmObj** %stackaddr$prim55314, align 8
%clofunc55315 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48294)
musttail call tailcc void %clofunc55315(%struct.ScmObj* %ae48294, %struct.ScmObj* %argslist53792$ae482942)
ret void
}

define tailcc void @proc_clo$ae48294(%struct.ScmObj* %env$ae48294,%struct.ScmObj* %current_45args53765) {
%stackaddr$env-ref55316 = alloca %struct.ScmObj*, align 8
%k47451 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48294, i64 0)
store %struct.ScmObj* %k47451, %struct.ScmObj** %stackaddr$env-ref55316
%stackaddr$env-ref55317 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48294, i64 1)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref55317
%stackaddr$env-ref55318 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48294, i64 2)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55318
%stackaddr$env-ref55319 = alloca %struct.ScmObj*, align 8
%lsts_4347104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48294, i64 3)
store %struct.ScmObj* %lsts_4347104, %struct.ScmObj** %stackaddr$env-ref55319
%stackaddr$env-ref55320 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48294, i64 4)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref55320
%stackaddr$env-ref55321 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48294, i64 5)
store %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$env-ref55321
%stackaddr$env-ref55322 = alloca %struct.ScmObj*, align 8
%lsts47097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48294, i64 6)
store %struct.ScmObj* %lsts47097, %struct.ScmObj** %stackaddr$env-ref55322
%stackaddr$env-ref55323 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48294, i64 7)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55323
%stackaddr$prim55324 = alloca %struct.ScmObj*, align 8
%_95k47456 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53765)
store volatile %struct.ScmObj* %_95k47456, %struct.ScmObj** %stackaddr$prim55324, align 8
%stackaddr$prim55325 = alloca %struct.ScmObj*, align 8
%current_45args53766 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53765)
store volatile %struct.ScmObj* %current_45args53766, %struct.ScmObj** %stackaddr$prim55325, align 8
%stackaddr$prim55326 = alloca %struct.ScmObj*, align 8
%anf_45bind47234 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53766)
store volatile %struct.ScmObj* %anf_45bind47234, %struct.ScmObj** %stackaddr$prim55326, align 8
%stackaddr$makeclosure55327 = alloca %struct.ScmObj*, align 8
%fptrToInt55328 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48315 to i64
%ae48315 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt55328)
store volatile %struct.ScmObj* %ae48315, %struct.ScmObj** %stackaddr$makeclosure55327, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48315, %struct.ScmObj* %k47451, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48315, %struct.ScmObj* %f47099, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48315, %struct.ScmObj* %_37foldr147089, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48315, %struct.ScmObj* %lsts_4347104, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48315, %struct.ScmObj* %acc47098, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48315, %struct.ScmObj* %_37foldr47095, i64 5)
%argslist53787$_37map1470850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55329 = alloca %struct.ScmObj*, align 8
%argslist53787$_37map1470851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47097, %struct.ScmObj* %argslist53787$_37map1470850)
store volatile %struct.ScmObj* %argslist53787$_37map1470851, %struct.ScmObj** %stackaddr$prim55329, align 8
%stackaddr$prim55330 = alloca %struct.ScmObj*, align 8
%argslist53787$_37map1470852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47234, %struct.ScmObj* %argslist53787$_37map1470851)
store volatile %struct.ScmObj* %argslist53787$_37map1470852, %struct.ScmObj** %stackaddr$prim55330, align 8
%stackaddr$prim55331 = alloca %struct.ScmObj*, align 8
%argslist53787$_37map1470853 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48315, %struct.ScmObj* %argslist53787$_37map1470852)
store volatile %struct.ScmObj* %argslist53787$_37map1470853, %struct.ScmObj** %stackaddr$prim55331, align 8
%clofunc55332 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147085)
musttail call tailcc void %clofunc55332(%struct.ScmObj* %_37map147085, %struct.ScmObj* %argslist53787$_37map1470853)
ret void
}

define tailcc void @proc_clo$ae48315(%struct.ScmObj* %env$ae48315,%struct.ScmObj* %current_45args53768) {
%stackaddr$env-ref55333 = alloca %struct.ScmObj*, align 8
%k47451 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48315, i64 0)
store %struct.ScmObj* %k47451, %struct.ScmObj** %stackaddr$env-ref55333
%stackaddr$env-ref55334 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48315, i64 1)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref55334
%stackaddr$env-ref55335 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48315, i64 2)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55335
%stackaddr$env-ref55336 = alloca %struct.ScmObj*, align 8
%lsts_4347104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48315, i64 3)
store %struct.ScmObj* %lsts_4347104, %struct.ScmObj** %stackaddr$env-ref55336
%stackaddr$env-ref55337 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48315, i64 4)
store %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$env-ref55337
%stackaddr$env-ref55338 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48315, i64 5)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55338
%stackaddr$prim55339 = alloca %struct.ScmObj*, align 8
%_95k47457 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53768)
store volatile %struct.ScmObj* %_95k47457, %struct.ScmObj** %stackaddr$prim55339, align 8
%stackaddr$prim55340 = alloca %struct.ScmObj*, align 8
%current_45args53769 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53768)
store volatile %struct.ScmObj* %current_45args53769, %struct.ScmObj** %stackaddr$prim55340, align 8
%stackaddr$prim55341 = alloca %struct.ScmObj*, align 8
%vs47102 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53769)
store volatile %struct.ScmObj* %vs47102, %struct.ScmObj** %stackaddr$prim55341, align 8
%stackaddr$makeclosure55342 = alloca %struct.ScmObj*, align 8
%fptrToInt55343 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48318 to i64
%ae48318 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55343)
store volatile %struct.ScmObj* %ae48318, %struct.ScmObj** %stackaddr$makeclosure55342, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48318, %struct.ScmObj* %k47451, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48318, %struct.ScmObj* %f47099, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48318, %struct.ScmObj* %_37foldr147089, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48318, %struct.ScmObj* %lsts_4347104, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48318, %struct.ScmObj* %vs47102, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48318, %struct.ScmObj* %acc47098, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48318, %struct.ScmObj* %_37foldr47095, i64 6)
%ae48319 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55344 = alloca %struct.ScmObj*, align 8
%fptrToInt55345 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48320 to i64
%ae48320 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55345)
store volatile %struct.ScmObj* %ae48320, %struct.ScmObj** %stackaddr$makeclosure55344, align 8
%argslist53786$ae483180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55346 = alloca %struct.ScmObj*, align 8
%argslist53786$ae483181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48320, %struct.ScmObj* %argslist53786$ae483180)
store volatile %struct.ScmObj* %argslist53786$ae483181, %struct.ScmObj** %stackaddr$prim55346, align 8
%stackaddr$prim55347 = alloca %struct.ScmObj*, align 8
%argslist53786$ae483182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48319, %struct.ScmObj* %argslist53786$ae483181)
store volatile %struct.ScmObj* %argslist53786$ae483182, %struct.ScmObj** %stackaddr$prim55347, align 8
%clofunc55348 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48318)
musttail call tailcc void %clofunc55348(%struct.ScmObj* %ae48318, %struct.ScmObj* %argslist53786$ae483182)
ret void
}

define tailcc void @proc_clo$ae48318(%struct.ScmObj* %env$ae48318,%struct.ScmObj* %current_45args53771) {
%stackaddr$env-ref55349 = alloca %struct.ScmObj*, align 8
%k47451 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48318, i64 0)
store %struct.ScmObj* %k47451, %struct.ScmObj** %stackaddr$env-ref55349
%stackaddr$env-ref55350 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48318, i64 1)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref55350
%stackaddr$env-ref55351 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48318, i64 2)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55351
%stackaddr$env-ref55352 = alloca %struct.ScmObj*, align 8
%lsts_4347104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48318, i64 3)
store %struct.ScmObj* %lsts_4347104, %struct.ScmObj** %stackaddr$env-ref55352
%stackaddr$env-ref55353 = alloca %struct.ScmObj*, align 8
%vs47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48318, i64 4)
store %struct.ScmObj* %vs47102, %struct.ScmObj** %stackaddr$env-ref55353
%stackaddr$env-ref55354 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48318, i64 5)
store %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$env-ref55354
%stackaddr$env-ref55355 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48318, i64 6)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55355
%stackaddr$prim55356 = alloca %struct.ScmObj*, align 8
%_95k47458 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53771)
store volatile %struct.ScmObj* %_95k47458, %struct.ScmObj** %stackaddr$prim55356, align 8
%stackaddr$prim55357 = alloca %struct.ScmObj*, align 8
%current_45args53772 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53771)
store volatile %struct.ScmObj* %current_45args53772, %struct.ScmObj** %stackaddr$prim55357, align 8
%stackaddr$prim55358 = alloca %struct.ScmObj*, align 8
%anf_45bind47235 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53772)
store volatile %struct.ScmObj* %anf_45bind47235, %struct.ScmObj** %stackaddr$prim55358, align 8
%stackaddr$prim55359 = alloca %struct.ScmObj*, align 8
%anf_45bind47236 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47098, %struct.ScmObj* %lsts_4347104)
store volatile %struct.ScmObj* %anf_45bind47236, %struct.ScmObj** %stackaddr$prim55359, align 8
%stackaddr$prim55360 = alloca %struct.ScmObj*, align 8
%anf_45bind47237 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47099, %struct.ScmObj* %anf_45bind47236)
store volatile %struct.ScmObj* %anf_45bind47237, %struct.ScmObj** %stackaddr$prim55360, align 8
%stackaddr$makeclosure55361 = alloca %struct.ScmObj*, align 8
%fptrToInt55362 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48344 to i64
%ae48344 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55362)
store volatile %struct.ScmObj* %ae48344, %struct.ScmObj** %stackaddr$makeclosure55361, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48344, %struct.ScmObj* %k47451, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48344, %struct.ScmObj* %f47099, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48344, %struct.ScmObj* %anf_45bind47235, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48344, %struct.ScmObj* %_37foldr147089, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48344, %struct.ScmObj* %vs47102, i64 4)
%stackaddr$prim55363 = alloca %struct.ScmObj*, align 8
%cpsargs47462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48344, %struct.ScmObj* %anf_45bind47237)
store volatile %struct.ScmObj* %cpsargs47462, %struct.ScmObj** %stackaddr$prim55363, align 8
%clofunc55364 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr47095)
musttail call tailcc void %clofunc55364(%struct.ScmObj* %_37foldr47095, %struct.ScmObj* %cpsargs47462)
ret void
}

define tailcc void @proc_clo$ae48344(%struct.ScmObj* %env$ae48344,%struct.ScmObj* %current_45args53774) {
%stackaddr$env-ref55365 = alloca %struct.ScmObj*, align 8
%k47451 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48344, i64 0)
store %struct.ScmObj* %k47451, %struct.ScmObj** %stackaddr$env-ref55365
%stackaddr$env-ref55366 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48344, i64 1)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref55366
%stackaddr$env-ref55367 = alloca %struct.ScmObj*, align 8
%anf_45bind47235 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48344, i64 2)
store %struct.ScmObj* %anf_45bind47235, %struct.ScmObj** %stackaddr$env-ref55367
%stackaddr$env-ref55368 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48344, i64 3)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55368
%stackaddr$env-ref55369 = alloca %struct.ScmObj*, align 8
%vs47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48344, i64 4)
store %struct.ScmObj* %vs47102, %struct.ScmObj** %stackaddr$env-ref55369
%stackaddr$prim55370 = alloca %struct.ScmObj*, align 8
%_95k47459 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53774)
store volatile %struct.ScmObj* %_95k47459, %struct.ScmObj** %stackaddr$prim55370, align 8
%stackaddr$prim55371 = alloca %struct.ScmObj*, align 8
%current_45args53775 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53774)
store volatile %struct.ScmObj* %current_45args53775, %struct.ScmObj** %stackaddr$prim55371, align 8
%stackaddr$prim55372 = alloca %struct.ScmObj*, align 8
%anf_45bind47238 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53775)
store volatile %struct.ScmObj* %anf_45bind47238, %struct.ScmObj** %stackaddr$prim55372, align 8
%ae48349 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55373 = alloca %struct.ScmObj*, align 8
%anf_45bind47239 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47238, %struct.ScmObj* %ae48349)
store volatile %struct.ScmObj* %anf_45bind47239, %struct.ScmObj** %stackaddr$prim55373, align 8
%stackaddr$makeclosure55374 = alloca %struct.ScmObj*, align 8
%fptrToInt55375 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48351 to i64
%ae48351 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55375)
store volatile %struct.ScmObj* %ae48351, %struct.ScmObj** %stackaddr$makeclosure55374, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48351, %struct.ScmObj* %k47451, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48351, %struct.ScmObj* %f47099, i64 1)
%argslist53780$_37foldr1470890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55376 = alloca %struct.ScmObj*, align 8
%argslist53780$_37foldr1470891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs47102, %struct.ScmObj* %argslist53780$_37foldr1470890)
store volatile %struct.ScmObj* %argslist53780$_37foldr1470891, %struct.ScmObj** %stackaddr$prim55376, align 8
%stackaddr$prim55377 = alloca %struct.ScmObj*, align 8
%argslist53780$_37foldr1470892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47239, %struct.ScmObj* %argslist53780$_37foldr1470891)
store volatile %struct.ScmObj* %argslist53780$_37foldr1470892, %struct.ScmObj** %stackaddr$prim55377, align 8
%stackaddr$prim55378 = alloca %struct.ScmObj*, align 8
%argslist53780$_37foldr1470893 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47235, %struct.ScmObj* %argslist53780$_37foldr1470892)
store volatile %struct.ScmObj* %argslist53780$_37foldr1470893, %struct.ScmObj** %stackaddr$prim55378, align 8
%stackaddr$prim55379 = alloca %struct.ScmObj*, align 8
%argslist53780$_37foldr1470894 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48351, %struct.ScmObj* %argslist53780$_37foldr1470893)
store volatile %struct.ScmObj* %argslist53780$_37foldr1470894, %struct.ScmObj** %stackaddr$prim55379, align 8
%clofunc55380 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147089)
musttail call tailcc void %clofunc55380(%struct.ScmObj* %_37foldr147089, %struct.ScmObj* %argslist53780$_37foldr1470894)
ret void
}

define tailcc void @proc_clo$ae48351(%struct.ScmObj* %env$ae48351,%struct.ScmObj* %current_45args53777) {
%stackaddr$env-ref55381 = alloca %struct.ScmObj*, align 8
%k47451 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48351, i64 0)
store %struct.ScmObj* %k47451, %struct.ScmObj** %stackaddr$env-ref55381
%stackaddr$env-ref55382 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48351, i64 1)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref55382
%stackaddr$prim55383 = alloca %struct.ScmObj*, align 8
%_95k47460 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53777)
store volatile %struct.ScmObj* %_95k47460, %struct.ScmObj** %stackaddr$prim55383, align 8
%stackaddr$prim55384 = alloca %struct.ScmObj*, align 8
%current_45args53778 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53777)
store volatile %struct.ScmObj* %current_45args53778, %struct.ScmObj** %stackaddr$prim55384, align 8
%stackaddr$prim55385 = alloca %struct.ScmObj*, align 8
%anf_45bind47240 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53778)
store volatile %struct.ScmObj* %anf_45bind47240, %struct.ScmObj** %stackaddr$prim55385, align 8
%stackaddr$prim55386 = alloca %struct.ScmObj*, align 8
%cpsargs47461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47451, %struct.ScmObj* %anf_45bind47240)
store volatile %struct.ScmObj* %cpsargs47461, %struct.ScmObj** %stackaddr$prim55386, align 8
%clofunc55387 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47099)
musttail call tailcc void %clofunc55387(%struct.ScmObj* %f47099, %struct.ScmObj* %cpsargs47461)
ret void
}

define tailcc void @proc_clo$ae48320(%struct.ScmObj* %env$ae48320,%struct.ScmObj* %current_45args53781) {
%stackaddr$prim55388 = alloca %struct.ScmObj*, align 8
%k47463 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53781)
store volatile %struct.ScmObj* %k47463, %struct.ScmObj** %stackaddr$prim55388, align 8
%stackaddr$prim55389 = alloca %struct.ScmObj*, align 8
%current_45args53782 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53781)
store volatile %struct.ScmObj* %current_45args53782, %struct.ScmObj** %stackaddr$prim55389, align 8
%stackaddr$prim55390 = alloca %struct.ScmObj*, align 8
%a47107 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53782)
store volatile %struct.ScmObj* %a47107, %struct.ScmObj** %stackaddr$prim55390, align 8
%stackaddr$prim55391 = alloca %struct.ScmObj*, align 8
%current_45args53783 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53782)
store volatile %struct.ScmObj* %current_45args53783, %struct.ScmObj** %stackaddr$prim55391, align 8
%stackaddr$prim55392 = alloca %struct.ScmObj*, align 8
%b47106 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53783)
store volatile %struct.ScmObj* %b47106, %struct.ScmObj** %stackaddr$prim55392, align 8
%stackaddr$prim55393 = alloca %struct.ScmObj*, align 8
%cpsprim47464 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a47107, %struct.ScmObj* %b47106)
store volatile %struct.ScmObj* %cpsprim47464, %struct.ScmObj** %stackaddr$prim55393, align 8
%ae48324 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53785$k474630 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55394 = alloca %struct.ScmObj*, align 8
%argslist53785$k474631 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47464, %struct.ScmObj* %argslist53785$k474630)
store volatile %struct.ScmObj* %argslist53785$k474631, %struct.ScmObj** %stackaddr$prim55394, align 8
%stackaddr$prim55395 = alloca %struct.ScmObj*, align 8
%argslist53785$k474632 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48324, %struct.ScmObj* %argslist53785$k474631)
store volatile %struct.ScmObj* %argslist53785$k474632, %struct.ScmObj** %stackaddr$prim55395, align 8
%clofunc55396 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47463)
musttail call tailcc void %clofunc55396(%struct.ScmObj* %k47463, %struct.ScmObj* %argslist53785$k474632)
ret void
}

define tailcc void @proc_clo$ae48296(%struct.ScmObj* %env$ae48296,%struct.ScmObj* %current_45args53788) {
%stackaddr$prim55397 = alloca %struct.ScmObj*, align 8
%k47465 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53788)
store volatile %struct.ScmObj* %k47465, %struct.ScmObj** %stackaddr$prim55397, align 8
%stackaddr$prim55398 = alloca %struct.ScmObj*, align 8
%current_45args53789 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53788)
store volatile %struct.ScmObj* %current_45args53789, %struct.ScmObj** %stackaddr$prim55398, align 8
%stackaddr$prim55399 = alloca %struct.ScmObj*, align 8
%x47103 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53789)
store volatile %struct.ScmObj* %x47103, %struct.ScmObj** %stackaddr$prim55399, align 8
%stackaddr$prim55400 = alloca %struct.ScmObj*, align 8
%cpsprim47466 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47103)
store volatile %struct.ScmObj* %cpsprim47466, %struct.ScmObj** %stackaddr$prim55400, align 8
%ae48299 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53791$k474650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55401 = alloca %struct.ScmObj*, align 8
%argslist53791$k474651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47466, %struct.ScmObj* %argslist53791$k474650)
store volatile %struct.ScmObj* %argslist53791$k474651, %struct.ScmObj** %stackaddr$prim55401, align 8
%stackaddr$prim55402 = alloca %struct.ScmObj*, align 8
%argslist53791$k474652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48299, %struct.ScmObj* %argslist53791$k474651)
store volatile %struct.ScmObj* %argslist53791$k474652, %struct.ScmObj** %stackaddr$prim55402, align 8
%clofunc55403 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47465)
musttail call tailcc void %clofunc55403(%struct.ScmObj* %k47465, %struct.ScmObj* %argslist53791$k474652)
ret void
}

define tailcc void @proc_clo$ae48272(%struct.ScmObj* %env$ae48272,%struct.ScmObj* %current_45args53794) {
%stackaddr$prim55404 = alloca %struct.ScmObj*, align 8
%k47467 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53794)
store volatile %struct.ScmObj* %k47467, %struct.ScmObj** %stackaddr$prim55404, align 8
%stackaddr$prim55405 = alloca %struct.ScmObj*, align 8
%current_45args53795 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53794)
store volatile %struct.ScmObj* %current_45args53795, %struct.ScmObj** %stackaddr$prim55405, align 8
%stackaddr$prim55406 = alloca %struct.ScmObj*, align 8
%x47105 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53795)
store volatile %struct.ScmObj* %x47105, %struct.ScmObj** %stackaddr$prim55406, align 8
%stackaddr$prim55407 = alloca %struct.ScmObj*, align 8
%cpsprim47468 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47105)
store volatile %struct.ScmObj* %cpsprim47468, %struct.ScmObj** %stackaddr$prim55407, align 8
%ae48275 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53797$k474670 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55408 = alloca %struct.ScmObj*, align 8
%argslist53797$k474671 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47468, %struct.ScmObj* %argslist53797$k474670)
store volatile %struct.ScmObj* %argslist53797$k474671, %struct.ScmObj** %stackaddr$prim55408, align 8
%stackaddr$prim55409 = alloca %struct.ScmObj*, align 8
%argslist53797$k474672 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48275, %struct.ScmObj* %argslist53797$k474671)
store volatile %struct.ScmObj* %argslist53797$k474672, %struct.ScmObj** %stackaddr$prim55409, align 8
%clofunc55410 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47467)
musttail call tailcc void %clofunc55410(%struct.ScmObj* %k47467, %struct.ScmObj* %argslist53797$k474672)
ret void
}

define tailcc void @proc_clo$ae48224(%struct.ScmObj* %env$ae48224,%struct.ScmObj* %current_45args53800) {
%stackaddr$prim55411 = alloca %struct.ScmObj*, align 8
%k47469 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53800)
store volatile %struct.ScmObj* %k47469, %struct.ScmObj** %stackaddr$prim55411, align 8
%stackaddr$prim55412 = alloca %struct.ScmObj*, align 8
%current_45args53801 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53800)
store volatile %struct.ScmObj* %current_45args53801, %struct.ScmObj** %stackaddr$prim55412, align 8
%stackaddr$prim55413 = alloca %struct.ScmObj*, align 8
%lst47101 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53801)
store volatile %struct.ScmObj* %lst47101, %struct.ScmObj** %stackaddr$prim55413, align 8
%stackaddr$prim55414 = alloca %struct.ScmObj*, align 8
%current_45args53802 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53801)
store volatile %struct.ScmObj* %current_45args53802, %struct.ScmObj** %stackaddr$prim55414, align 8
%stackaddr$prim55415 = alloca %struct.ScmObj*, align 8
%b47100 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53802)
store volatile %struct.ScmObj* %b47100, %struct.ScmObj** %stackaddr$prim55415, align 8
%truthy$cmp55416 = call i64 @is_truthy_value(%struct.ScmObj* %b47100)
%cmp$cmp55416 = icmp eq i64 %truthy$cmp55416, 1
br i1 %cmp$cmp55416, label %truebranch$cmp55416, label %falsebranch$cmp55416
truebranch$cmp55416:
%ae48227 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53804$k474690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55417 = alloca %struct.ScmObj*, align 8
%argslist53804$k474691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b47100, %struct.ScmObj* %argslist53804$k474690)
store volatile %struct.ScmObj* %argslist53804$k474691, %struct.ScmObj** %stackaddr$prim55417, align 8
%stackaddr$prim55418 = alloca %struct.ScmObj*, align 8
%argslist53804$k474692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48227, %struct.ScmObj* %argslist53804$k474691)
store volatile %struct.ScmObj* %argslist53804$k474692, %struct.ScmObj** %stackaddr$prim55418, align 8
%clofunc55419 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47469)
musttail call tailcc void %clofunc55419(%struct.ScmObj* %k47469, %struct.ScmObj* %argslist53804$k474692)
ret void
falsebranch$cmp55416:
%stackaddr$prim55420 = alloca %struct.ScmObj*, align 8
%cpsprim47470 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47101)
store volatile %struct.ScmObj* %cpsprim47470, %struct.ScmObj** %stackaddr$prim55420, align 8
%ae48234 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53805$k474690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55421 = alloca %struct.ScmObj*, align 8
%argslist53805$k474691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47470, %struct.ScmObj* %argslist53805$k474690)
store volatile %struct.ScmObj* %argslist53805$k474691, %struct.ScmObj** %stackaddr$prim55421, align 8
%stackaddr$prim55422 = alloca %struct.ScmObj*, align 8
%argslist53805$k474692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48234, %struct.ScmObj* %argslist53805$k474691)
store volatile %struct.ScmObj* %argslist53805$k474692, %struct.ScmObj** %stackaddr$prim55422, align 8
%clofunc55423 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47469)
musttail call tailcc void %clofunc55423(%struct.ScmObj* %k47469, %struct.ScmObj* %argslist53805$k474692)
ret void
}

define tailcc void @proc_clo$ae48181(%struct.ScmObj* %env$ae48181,%struct.ScmObj* %current_45args53809) {
%stackaddr$env-ref55424 = alloca %struct.ScmObj*, align 8
%_37length47078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48181, i64 0)
store %struct.ScmObj* %_37length47078, %struct.ScmObj** %stackaddr$env-ref55424
%stackaddr$env-ref55425 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48181, i64 1)
store %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$env-ref55425
%stackaddr$prim55426 = alloca %struct.ScmObj*, align 8
%k47471 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53809)
store volatile %struct.ScmObj* %k47471, %struct.ScmObj** %stackaddr$prim55426, align 8
%stackaddr$prim55427 = alloca %struct.ScmObj*, align 8
%current_45args53810 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53809)
store volatile %struct.ScmObj* %current_45args53810, %struct.ScmObj** %stackaddr$prim55427, align 8
%stackaddr$prim55428 = alloca %struct.ScmObj*, align 8
%lst47110 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53810)
store volatile %struct.ScmObj* %lst47110, %struct.ScmObj** %stackaddr$prim55428, align 8
%stackaddr$prim55429 = alloca %struct.ScmObj*, align 8
%current_45args53811 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53810)
store volatile %struct.ScmObj* %current_45args53811, %struct.ScmObj** %stackaddr$prim55429, align 8
%stackaddr$prim55430 = alloca %struct.ScmObj*, align 8
%n47109 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53811)
store volatile %struct.ScmObj* %n47109, %struct.ScmObj** %stackaddr$prim55430, align 8
%stackaddr$makeclosure55431 = alloca %struct.ScmObj*, align 8
%fptrToInt55432 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48183 to i64
%ae48183 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55432)
store volatile %struct.ScmObj* %ae48183, %struct.ScmObj** %stackaddr$makeclosure55431, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48183, %struct.ScmObj* %lst47110, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48183, %struct.ScmObj* %n47109, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48183, %struct.ScmObj* %k47471, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48183, %struct.ScmObj* %_37take47081, i64 3)
%argslist53817$_37length470780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55433 = alloca %struct.ScmObj*, align 8
%argslist53817$_37length470781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47110, %struct.ScmObj* %argslist53817$_37length470780)
store volatile %struct.ScmObj* %argslist53817$_37length470781, %struct.ScmObj** %stackaddr$prim55433, align 8
%stackaddr$prim55434 = alloca %struct.ScmObj*, align 8
%argslist53817$_37length470782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48183, %struct.ScmObj* %argslist53817$_37length470781)
store volatile %struct.ScmObj* %argslist53817$_37length470782, %struct.ScmObj** %stackaddr$prim55434, align 8
%clofunc55435 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length47078)
musttail call tailcc void %clofunc55435(%struct.ScmObj* %_37length47078, %struct.ScmObj* %argslist53817$_37length470782)
ret void
}

define tailcc void @proc_clo$ae48183(%struct.ScmObj* %env$ae48183,%struct.ScmObj* %current_45args53813) {
%stackaddr$env-ref55436 = alloca %struct.ScmObj*, align 8
%lst47110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48183, i64 0)
store %struct.ScmObj* %lst47110, %struct.ScmObj** %stackaddr$env-ref55436
%stackaddr$env-ref55437 = alloca %struct.ScmObj*, align 8
%n47109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48183, i64 1)
store %struct.ScmObj* %n47109, %struct.ScmObj** %stackaddr$env-ref55437
%stackaddr$env-ref55438 = alloca %struct.ScmObj*, align 8
%k47471 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48183, i64 2)
store %struct.ScmObj* %k47471, %struct.ScmObj** %stackaddr$env-ref55438
%stackaddr$env-ref55439 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48183, i64 3)
store %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$env-ref55439
%stackaddr$prim55440 = alloca %struct.ScmObj*, align 8
%_95k47472 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53813)
store volatile %struct.ScmObj* %_95k47472, %struct.ScmObj** %stackaddr$prim55440, align 8
%stackaddr$prim55441 = alloca %struct.ScmObj*, align 8
%current_45args53814 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53813)
store volatile %struct.ScmObj* %current_45args53814, %struct.ScmObj** %stackaddr$prim55441, align 8
%stackaddr$prim55442 = alloca %struct.ScmObj*, align 8
%anf_45bind47227 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53814)
store volatile %struct.ScmObj* %anf_45bind47227, %struct.ScmObj** %stackaddr$prim55442, align 8
%stackaddr$prim55443 = alloca %struct.ScmObj*, align 8
%anf_45bind47228 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind47227, %struct.ScmObj* %n47109)
store volatile %struct.ScmObj* %anf_45bind47228, %struct.ScmObj** %stackaddr$prim55443, align 8
%argslist53816$_37take470810 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55444 = alloca %struct.ScmObj*, align 8
%argslist53816$_37take470811 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47228, %struct.ScmObj* %argslist53816$_37take470810)
store volatile %struct.ScmObj* %argslist53816$_37take470811, %struct.ScmObj** %stackaddr$prim55444, align 8
%stackaddr$prim55445 = alloca %struct.ScmObj*, align 8
%argslist53816$_37take470812 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47110, %struct.ScmObj* %argslist53816$_37take470811)
store volatile %struct.ScmObj* %argslist53816$_37take470812, %struct.ScmObj** %stackaddr$prim55445, align 8
%stackaddr$prim55446 = alloca %struct.ScmObj*, align 8
%argslist53816$_37take470813 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47471, %struct.ScmObj* %argslist53816$_37take470812)
store volatile %struct.ScmObj* %argslist53816$_37take470813, %struct.ScmObj** %stackaddr$prim55446, align 8
%clofunc55447 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take47081)
musttail call tailcc void %clofunc55447(%struct.ScmObj* %_37take47081, %struct.ScmObj* %argslist53816$_37take470813)
ret void
}

define tailcc void @proc_clo$ae48127(%struct.ScmObj* %env$ae48127,%struct.ScmObj* %current_45args53819) {
%stackaddr$env-ref55448 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48127, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref55448
%stackaddr$prim55449 = alloca %struct.ScmObj*, align 8
%k47473 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53819)
store volatile %struct.ScmObj* %k47473, %struct.ScmObj** %stackaddr$prim55449, align 8
%stackaddr$prim55450 = alloca %struct.ScmObj*, align 8
%current_45args53820 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53819)
store volatile %struct.ScmObj* %current_45args53820, %struct.ScmObj** %stackaddr$prim55450, align 8
%stackaddr$prim55451 = alloca %struct.ScmObj*, align 8
%lst47112 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53820)
store volatile %struct.ScmObj* %lst47112, %struct.ScmObj** %stackaddr$prim55451, align 8
%stackaddr$makeclosure55452 = alloca %struct.ScmObj*, align 8
%fptrToInt55453 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48128 to i64
%ae48128 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55453)
store volatile %struct.ScmObj* %ae48128, %struct.ScmObj** %stackaddr$makeclosure55452, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48128, %struct.ScmObj* %k47473, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48128, %struct.ScmObj* %_37foldl147073, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48128, %struct.ScmObj* %lst47112, i64 2)
%ae48129 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55454 = alloca %struct.ScmObj*, align 8
%fptrToInt55455 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48130 to i64
%ae48130 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55455)
store volatile %struct.ScmObj* %ae48130, %struct.ScmObj** %stackaddr$makeclosure55454, align 8
%argslist53831$ae481280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55456 = alloca %struct.ScmObj*, align 8
%argslist53831$ae481281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48130, %struct.ScmObj* %argslist53831$ae481280)
store volatile %struct.ScmObj* %argslist53831$ae481281, %struct.ScmObj** %stackaddr$prim55456, align 8
%stackaddr$prim55457 = alloca %struct.ScmObj*, align 8
%argslist53831$ae481282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48129, %struct.ScmObj* %argslist53831$ae481281)
store volatile %struct.ScmObj* %argslist53831$ae481282, %struct.ScmObj** %stackaddr$prim55457, align 8
%clofunc55458 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48128)
musttail call tailcc void %clofunc55458(%struct.ScmObj* %ae48128, %struct.ScmObj* %argslist53831$ae481282)
ret void
}

define tailcc void @proc_clo$ae48128(%struct.ScmObj* %env$ae48128,%struct.ScmObj* %current_45args53822) {
%stackaddr$env-ref55459 = alloca %struct.ScmObj*, align 8
%k47473 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48128, i64 0)
store %struct.ScmObj* %k47473, %struct.ScmObj** %stackaddr$env-ref55459
%stackaddr$env-ref55460 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48128, i64 1)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref55460
%stackaddr$env-ref55461 = alloca %struct.ScmObj*, align 8
%lst47112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48128, i64 2)
store %struct.ScmObj* %lst47112, %struct.ScmObj** %stackaddr$env-ref55461
%stackaddr$prim55462 = alloca %struct.ScmObj*, align 8
%_95k47474 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53822)
store volatile %struct.ScmObj* %_95k47474, %struct.ScmObj** %stackaddr$prim55462, align 8
%stackaddr$prim55463 = alloca %struct.ScmObj*, align 8
%current_45args53823 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53822)
store volatile %struct.ScmObj* %current_45args53823, %struct.ScmObj** %stackaddr$prim55463, align 8
%stackaddr$prim55464 = alloca %struct.ScmObj*, align 8
%anf_45bind47226 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53823)
store volatile %struct.ScmObj* %anf_45bind47226, %struct.ScmObj** %stackaddr$prim55464, align 8
%ae48149 = call %struct.ScmObj* @const_init_null()
%argslist53825$_37foldl1470730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55465 = alloca %struct.ScmObj*, align 8
%argslist53825$_37foldl1470731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47112, %struct.ScmObj* %argslist53825$_37foldl1470730)
store volatile %struct.ScmObj* %argslist53825$_37foldl1470731, %struct.ScmObj** %stackaddr$prim55465, align 8
%stackaddr$prim55466 = alloca %struct.ScmObj*, align 8
%argslist53825$_37foldl1470732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48149, %struct.ScmObj* %argslist53825$_37foldl1470731)
store volatile %struct.ScmObj* %argslist53825$_37foldl1470732, %struct.ScmObj** %stackaddr$prim55466, align 8
%stackaddr$prim55467 = alloca %struct.ScmObj*, align 8
%argslist53825$_37foldl1470733 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47226, %struct.ScmObj* %argslist53825$_37foldl1470732)
store volatile %struct.ScmObj* %argslist53825$_37foldl1470733, %struct.ScmObj** %stackaddr$prim55467, align 8
%stackaddr$prim55468 = alloca %struct.ScmObj*, align 8
%argslist53825$_37foldl1470734 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47473, %struct.ScmObj* %argslist53825$_37foldl1470733)
store volatile %struct.ScmObj* %argslist53825$_37foldl1470734, %struct.ScmObj** %stackaddr$prim55468, align 8
%clofunc55469 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147073)
musttail call tailcc void %clofunc55469(%struct.ScmObj* %_37foldl147073, %struct.ScmObj* %argslist53825$_37foldl1470734)
ret void
}

define tailcc void @proc_clo$ae48130(%struct.ScmObj* %env$ae48130,%struct.ScmObj* %current_45args53826) {
%stackaddr$prim55470 = alloca %struct.ScmObj*, align 8
%k47475 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53826)
store volatile %struct.ScmObj* %k47475, %struct.ScmObj** %stackaddr$prim55470, align 8
%stackaddr$prim55471 = alloca %struct.ScmObj*, align 8
%current_45args53827 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53826)
store volatile %struct.ScmObj* %current_45args53827, %struct.ScmObj** %stackaddr$prim55471, align 8
%stackaddr$prim55472 = alloca %struct.ScmObj*, align 8
%x47114 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53827)
store volatile %struct.ScmObj* %x47114, %struct.ScmObj** %stackaddr$prim55472, align 8
%stackaddr$prim55473 = alloca %struct.ScmObj*, align 8
%current_45args53828 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53827)
store volatile %struct.ScmObj* %current_45args53828, %struct.ScmObj** %stackaddr$prim55473, align 8
%stackaddr$prim55474 = alloca %struct.ScmObj*, align 8
%y47113 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53828)
store volatile %struct.ScmObj* %y47113, %struct.ScmObj** %stackaddr$prim55474, align 8
%ae48132 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53830$k474750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55475 = alloca %struct.ScmObj*, align 8
%argslist53830$k474751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x47114, %struct.ScmObj* %argslist53830$k474750)
store volatile %struct.ScmObj* %argslist53830$k474751, %struct.ScmObj** %stackaddr$prim55475, align 8
%stackaddr$prim55476 = alloca %struct.ScmObj*, align 8
%argslist53830$k474752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48132, %struct.ScmObj* %argslist53830$k474751)
store volatile %struct.ScmObj* %argslist53830$k474752, %struct.ScmObj** %stackaddr$prim55476, align 8
%clofunc55477 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47475)
musttail call tailcc void %clofunc55477(%struct.ScmObj* %k47475, %struct.ScmObj* %argslist53830$k474752)
ret void
}

define tailcc void @proc_clo$ae48048(%struct.ScmObj* %env$ae48048,%struct.ScmObj* %current_45args53834) {
%stackaddr$prim55478 = alloca %struct.ScmObj*, align 8
%k47476 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53834)
store volatile %struct.ScmObj* %k47476, %struct.ScmObj** %stackaddr$prim55478, align 8
%stackaddr$prim55479 = alloca %struct.ScmObj*, align 8
%current_45args53835 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53834)
store volatile %struct.ScmObj* %current_45args53835, %struct.ScmObj** %stackaddr$prim55479, align 8
%stackaddr$prim55480 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53835)
store volatile %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$prim55480, align 8
%ae48050 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55481 = alloca %struct.ScmObj*, align 8
%fptrToInt55482 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48051 to i64
%ae48051 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55482)
store volatile %struct.ScmObj* %ae48051, %struct.ScmObj** %stackaddr$makeclosure55481, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48051, %struct.ScmObj* %_37foldl147074, i64 0)
%argslist53848$k474760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55483 = alloca %struct.ScmObj*, align 8
%argslist53848$k474761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48051, %struct.ScmObj* %argslist53848$k474760)
store volatile %struct.ScmObj* %argslist53848$k474761, %struct.ScmObj** %stackaddr$prim55483, align 8
%stackaddr$prim55484 = alloca %struct.ScmObj*, align 8
%argslist53848$k474762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48050, %struct.ScmObj* %argslist53848$k474761)
store volatile %struct.ScmObj* %argslist53848$k474762, %struct.ScmObj** %stackaddr$prim55484, align 8
%clofunc55485 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47476)
musttail call tailcc void %clofunc55485(%struct.ScmObj* %k47476, %struct.ScmObj* %argslist53848$k474762)
ret void
}

define tailcc void @proc_clo$ae48051(%struct.ScmObj* %env$ae48051,%struct.ScmObj* %current_45args53837) {
%stackaddr$env-ref55486 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48051, i64 0)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref55486
%stackaddr$prim55487 = alloca %struct.ScmObj*, align 8
%k47477 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53837)
store volatile %struct.ScmObj* %k47477, %struct.ScmObj** %stackaddr$prim55487, align 8
%stackaddr$prim55488 = alloca %struct.ScmObj*, align 8
%current_45args53838 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53837)
store volatile %struct.ScmObj* %current_45args53838, %struct.ScmObj** %stackaddr$prim55488, align 8
%stackaddr$prim55489 = alloca %struct.ScmObj*, align 8
%f47077 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53838)
store volatile %struct.ScmObj* %f47077, %struct.ScmObj** %stackaddr$prim55489, align 8
%stackaddr$prim55490 = alloca %struct.ScmObj*, align 8
%current_45args53839 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53838)
store volatile %struct.ScmObj* %current_45args53839, %struct.ScmObj** %stackaddr$prim55490, align 8
%stackaddr$prim55491 = alloca %struct.ScmObj*, align 8
%acc47076 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53839)
store volatile %struct.ScmObj* %acc47076, %struct.ScmObj** %stackaddr$prim55491, align 8
%stackaddr$prim55492 = alloca %struct.ScmObj*, align 8
%current_45args53840 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53839)
store volatile %struct.ScmObj* %current_45args53840, %struct.ScmObj** %stackaddr$prim55492, align 8
%stackaddr$prim55493 = alloca %struct.ScmObj*, align 8
%lst47075 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53840)
store volatile %struct.ScmObj* %lst47075, %struct.ScmObj** %stackaddr$prim55493, align 8
%stackaddr$prim55494 = alloca %struct.ScmObj*, align 8
%anf_45bind47221 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47075)
store volatile %struct.ScmObj* %anf_45bind47221, %struct.ScmObj** %stackaddr$prim55494, align 8
%truthy$cmp55495 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47221)
%cmp$cmp55495 = icmp eq i64 %truthy$cmp55495, 1
br i1 %cmp$cmp55495, label %truebranch$cmp55495, label %falsebranch$cmp55495
truebranch$cmp55495:
%ae48055 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53842$k474770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55496 = alloca %struct.ScmObj*, align 8
%argslist53842$k474771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47076, %struct.ScmObj* %argslist53842$k474770)
store volatile %struct.ScmObj* %argslist53842$k474771, %struct.ScmObj** %stackaddr$prim55496, align 8
%stackaddr$prim55497 = alloca %struct.ScmObj*, align 8
%argslist53842$k474772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48055, %struct.ScmObj* %argslist53842$k474771)
store volatile %struct.ScmObj* %argslist53842$k474772, %struct.ScmObj** %stackaddr$prim55497, align 8
%clofunc55498 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47477)
musttail call tailcc void %clofunc55498(%struct.ScmObj* %k47477, %struct.ScmObj* %argslist53842$k474772)
ret void
falsebranch$cmp55495:
%stackaddr$prim55499 = alloca %struct.ScmObj*, align 8
%anf_45bind47222 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47075)
store volatile %struct.ScmObj* %anf_45bind47222, %struct.ScmObj** %stackaddr$prim55499, align 8
%stackaddr$makeclosure55500 = alloca %struct.ScmObj*, align 8
%fptrToInt55501 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48062 to i64
%ae48062 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55501)
store volatile %struct.ScmObj* %ae48062, %struct.ScmObj** %stackaddr$makeclosure55500, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48062, %struct.ScmObj* %k47477, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48062, %struct.ScmObj* %f47077, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48062, %struct.ScmObj* %lst47075, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48062, %struct.ScmObj* %_37foldl147074, i64 3)
%argslist53847$f470770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55502 = alloca %struct.ScmObj*, align 8
%argslist53847$f470771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47076, %struct.ScmObj* %argslist53847$f470770)
store volatile %struct.ScmObj* %argslist53847$f470771, %struct.ScmObj** %stackaddr$prim55502, align 8
%stackaddr$prim55503 = alloca %struct.ScmObj*, align 8
%argslist53847$f470772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47222, %struct.ScmObj* %argslist53847$f470771)
store volatile %struct.ScmObj* %argslist53847$f470772, %struct.ScmObj** %stackaddr$prim55503, align 8
%stackaddr$prim55504 = alloca %struct.ScmObj*, align 8
%argslist53847$f470773 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48062, %struct.ScmObj* %argslist53847$f470772)
store volatile %struct.ScmObj* %argslist53847$f470773, %struct.ScmObj** %stackaddr$prim55504, align 8
%clofunc55505 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47077)
musttail call tailcc void %clofunc55505(%struct.ScmObj* %f47077, %struct.ScmObj* %argslist53847$f470773)
ret void
}

define tailcc void @proc_clo$ae48062(%struct.ScmObj* %env$ae48062,%struct.ScmObj* %current_45args53843) {
%stackaddr$env-ref55506 = alloca %struct.ScmObj*, align 8
%k47477 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48062, i64 0)
store %struct.ScmObj* %k47477, %struct.ScmObj** %stackaddr$env-ref55506
%stackaddr$env-ref55507 = alloca %struct.ScmObj*, align 8
%f47077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48062, i64 1)
store %struct.ScmObj* %f47077, %struct.ScmObj** %stackaddr$env-ref55507
%stackaddr$env-ref55508 = alloca %struct.ScmObj*, align 8
%lst47075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48062, i64 2)
store %struct.ScmObj* %lst47075, %struct.ScmObj** %stackaddr$env-ref55508
%stackaddr$env-ref55509 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48062, i64 3)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref55509
%stackaddr$prim55510 = alloca %struct.ScmObj*, align 8
%_95k47478 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53843)
store volatile %struct.ScmObj* %_95k47478, %struct.ScmObj** %stackaddr$prim55510, align 8
%stackaddr$prim55511 = alloca %struct.ScmObj*, align 8
%current_45args53844 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53843)
store volatile %struct.ScmObj* %current_45args53844, %struct.ScmObj** %stackaddr$prim55511, align 8
%stackaddr$prim55512 = alloca %struct.ScmObj*, align 8
%anf_45bind47223 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53844)
store volatile %struct.ScmObj* %anf_45bind47223, %struct.ScmObj** %stackaddr$prim55512, align 8
%stackaddr$prim55513 = alloca %struct.ScmObj*, align 8
%anf_45bind47224 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47075)
store volatile %struct.ScmObj* %anf_45bind47224, %struct.ScmObj** %stackaddr$prim55513, align 8
%argslist53846$_37foldl1470740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55514 = alloca %struct.ScmObj*, align 8
%argslist53846$_37foldl1470741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47224, %struct.ScmObj* %argslist53846$_37foldl1470740)
store volatile %struct.ScmObj* %argslist53846$_37foldl1470741, %struct.ScmObj** %stackaddr$prim55514, align 8
%stackaddr$prim55515 = alloca %struct.ScmObj*, align 8
%argslist53846$_37foldl1470742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47223, %struct.ScmObj* %argslist53846$_37foldl1470741)
store volatile %struct.ScmObj* %argslist53846$_37foldl1470742, %struct.ScmObj** %stackaddr$prim55515, align 8
%stackaddr$prim55516 = alloca %struct.ScmObj*, align 8
%argslist53846$_37foldl1470743 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47077, %struct.ScmObj* %argslist53846$_37foldl1470742)
store volatile %struct.ScmObj* %argslist53846$_37foldl1470743, %struct.ScmObj** %stackaddr$prim55516, align 8
%stackaddr$prim55517 = alloca %struct.ScmObj*, align 8
%argslist53846$_37foldl1470744 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47477, %struct.ScmObj* %argslist53846$_37foldl1470743)
store volatile %struct.ScmObj* %argslist53846$_37foldl1470744, %struct.ScmObj** %stackaddr$prim55517, align 8
%clofunc55518 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147074)
musttail call tailcc void %clofunc55518(%struct.ScmObj* %_37foldl147074, %struct.ScmObj* %argslist53846$_37foldl1470744)
ret void
}

define tailcc void @proc_clo$ae47965(%struct.ScmObj* %env$ae47965,%struct.ScmObj* %current_45args53851) {
%stackaddr$prim55519 = alloca %struct.ScmObj*, align 8
%k47479 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53851)
store volatile %struct.ScmObj* %k47479, %struct.ScmObj** %stackaddr$prim55519, align 8
%stackaddr$prim55520 = alloca %struct.ScmObj*, align 8
%current_45args53852 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53851)
store volatile %struct.ScmObj* %current_45args53852, %struct.ScmObj** %stackaddr$prim55520, align 8
%stackaddr$prim55521 = alloca %struct.ScmObj*, align 8
%_37length47079 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53852)
store volatile %struct.ScmObj* %_37length47079, %struct.ScmObj** %stackaddr$prim55521, align 8
%ae47967 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55522 = alloca %struct.ScmObj*, align 8
%fptrToInt55523 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47968 to i64
%ae47968 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55523)
store volatile %struct.ScmObj* %ae47968, %struct.ScmObj** %stackaddr$makeclosure55522, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47968, %struct.ScmObj* %_37length47079, i64 0)
%argslist53863$k474790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55524 = alloca %struct.ScmObj*, align 8
%argslist53863$k474791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47968, %struct.ScmObj* %argslist53863$k474790)
store volatile %struct.ScmObj* %argslist53863$k474791, %struct.ScmObj** %stackaddr$prim55524, align 8
%stackaddr$prim55525 = alloca %struct.ScmObj*, align 8
%argslist53863$k474792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47967, %struct.ScmObj* %argslist53863$k474791)
store volatile %struct.ScmObj* %argslist53863$k474792, %struct.ScmObj** %stackaddr$prim55525, align 8
%clofunc55526 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47479)
musttail call tailcc void %clofunc55526(%struct.ScmObj* %k47479, %struct.ScmObj* %argslist53863$k474792)
ret void
}

define tailcc void @proc_clo$ae47968(%struct.ScmObj* %env$ae47968,%struct.ScmObj* %current_45args53854) {
%stackaddr$env-ref55527 = alloca %struct.ScmObj*, align 8
%_37length47079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47968, i64 0)
store %struct.ScmObj* %_37length47079, %struct.ScmObj** %stackaddr$env-ref55527
%stackaddr$prim55528 = alloca %struct.ScmObj*, align 8
%k47480 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53854)
store volatile %struct.ScmObj* %k47480, %struct.ScmObj** %stackaddr$prim55528, align 8
%stackaddr$prim55529 = alloca %struct.ScmObj*, align 8
%current_45args53855 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53854)
store volatile %struct.ScmObj* %current_45args53855, %struct.ScmObj** %stackaddr$prim55529, align 8
%stackaddr$prim55530 = alloca %struct.ScmObj*, align 8
%lst47080 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53855)
store volatile %struct.ScmObj* %lst47080, %struct.ScmObj** %stackaddr$prim55530, align 8
%stackaddr$prim55531 = alloca %struct.ScmObj*, align 8
%anf_45bind47217 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47080)
store volatile %struct.ScmObj* %anf_45bind47217, %struct.ScmObj** %stackaddr$prim55531, align 8
%truthy$cmp55532 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47217)
%cmp$cmp55532 = icmp eq i64 %truthy$cmp55532, 1
br i1 %cmp$cmp55532, label %truebranch$cmp55532, label %falsebranch$cmp55532
truebranch$cmp55532:
%ae47972 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47973 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53857$k474800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55533 = alloca %struct.ScmObj*, align 8
%argslist53857$k474801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47973, %struct.ScmObj* %argslist53857$k474800)
store volatile %struct.ScmObj* %argslist53857$k474801, %struct.ScmObj** %stackaddr$prim55533, align 8
%stackaddr$prim55534 = alloca %struct.ScmObj*, align 8
%argslist53857$k474802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47972, %struct.ScmObj* %argslist53857$k474801)
store volatile %struct.ScmObj* %argslist53857$k474802, %struct.ScmObj** %stackaddr$prim55534, align 8
%clofunc55535 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47480)
musttail call tailcc void %clofunc55535(%struct.ScmObj* %k47480, %struct.ScmObj* %argslist53857$k474802)
ret void
falsebranch$cmp55532:
%stackaddr$prim55536 = alloca %struct.ScmObj*, align 8
%anf_45bind47218 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47080)
store volatile %struct.ScmObj* %anf_45bind47218, %struct.ScmObj** %stackaddr$prim55536, align 8
%stackaddr$makeclosure55537 = alloca %struct.ScmObj*, align 8
%fptrToInt55538 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47982 to i64
%ae47982 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55538)
store volatile %struct.ScmObj* %ae47982, %struct.ScmObj** %stackaddr$makeclosure55537, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47982, %struct.ScmObj* %k47480, i64 0)
%argslist53862$_37length470790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55539 = alloca %struct.ScmObj*, align 8
%argslist53862$_37length470791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47218, %struct.ScmObj* %argslist53862$_37length470790)
store volatile %struct.ScmObj* %argslist53862$_37length470791, %struct.ScmObj** %stackaddr$prim55539, align 8
%stackaddr$prim55540 = alloca %struct.ScmObj*, align 8
%argslist53862$_37length470792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47982, %struct.ScmObj* %argslist53862$_37length470791)
store volatile %struct.ScmObj* %argslist53862$_37length470792, %struct.ScmObj** %stackaddr$prim55540, align 8
%clofunc55541 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length47079)
musttail call tailcc void %clofunc55541(%struct.ScmObj* %_37length47079, %struct.ScmObj* %argslist53862$_37length470792)
ret void
}

define tailcc void @proc_clo$ae47982(%struct.ScmObj* %env$ae47982,%struct.ScmObj* %current_45args53858) {
%stackaddr$env-ref55542 = alloca %struct.ScmObj*, align 8
%k47480 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47982, i64 0)
store %struct.ScmObj* %k47480, %struct.ScmObj** %stackaddr$env-ref55542
%stackaddr$prim55543 = alloca %struct.ScmObj*, align 8
%_95k47481 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53858)
store volatile %struct.ScmObj* %_95k47481, %struct.ScmObj** %stackaddr$prim55543, align 8
%stackaddr$prim55544 = alloca %struct.ScmObj*, align 8
%current_45args53859 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53858)
store volatile %struct.ScmObj* %current_45args53859, %struct.ScmObj** %stackaddr$prim55544, align 8
%stackaddr$prim55545 = alloca %struct.ScmObj*, align 8
%anf_45bind47219 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53859)
store volatile %struct.ScmObj* %anf_45bind47219, %struct.ScmObj** %stackaddr$prim55545, align 8
%ae47984 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55546 = alloca %struct.ScmObj*, align 8
%cpsprim47482 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae47984, %struct.ScmObj* %anf_45bind47219)
store volatile %struct.ScmObj* %cpsprim47482, %struct.ScmObj** %stackaddr$prim55546, align 8
%ae47987 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53861$k474800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55547 = alloca %struct.ScmObj*, align 8
%argslist53861$k474801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47482, %struct.ScmObj* %argslist53861$k474800)
store volatile %struct.ScmObj* %argslist53861$k474801, %struct.ScmObj** %stackaddr$prim55547, align 8
%stackaddr$prim55548 = alloca %struct.ScmObj*, align 8
%argslist53861$k474802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47987, %struct.ScmObj* %argslist53861$k474801)
store volatile %struct.ScmObj* %argslist53861$k474802, %struct.ScmObj** %stackaddr$prim55548, align 8
%clofunc55549 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47480)
musttail call tailcc void %clofunc55549(%struct.ScmObj* %k47480, %struct.ScmObj* %argslist53861$k474802)
ret void
}

define tailcc void @proc_clo$ae47815(%struct.ScmObj* %env$ae47815,%struct.ScmObj* %current_45args53866) {
%stackaddr$prim55550 = alloca %struct.ScmObj*, align 8
%k47483 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53866)
store volatile %struct.ScmObj* %k47483, %struct.ScmObj** %stackaddr$prim55550, align 8
%stackaddr$prim55551 = alloca %struct.ScmObj*, align 8
%current_45args53867 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53866)
store volatile %struct.ScmObj* %current_45args53867, %struct.ScmObj** %stackaddr$prim55551, align 8
%stackaddr$prim55552 = alloca %struct.ScmObj*, align 8
%_37take47082 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53867)
store volatile %struct.ScmObj* %_37take47082, %struct.ScmObj** %stackaddr$prim55552, align 8
%ae47817 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55553 = alloca %struct.ScmObj*, align 8
%fptrToInt55554 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47818 to i64
%ae47818 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55554)
store volatile %struct.ScmObj* %ae47818, %struct.ScmObj** %stackaddr$makeclosure55553, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47818, %struct.ScmObj* %_37take47082, i64 0)
%argslist53880$k474830 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55555 = alloca %struct.ScmObj*, align 8
%argslist53880$k474831 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47818, %struct.ScmObj* %argslist53880$k474830)
store volatile %struct.ScmObj* %argslist53880$k474831, %struct.ScmObj** %stackaddr$prim55555, align 8
%stackaddr$prim55556 = alloca %struct.ScmObj*, align 8
%argslist53880$k474832 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47817, %struct.ScmObj* %argslist53880$k474831)
store volatile %struct.ScmObj* %argslist53880$k474832, %struct.ScmObj** %stackaddr$prim55556, align 8
%clofunc55557 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47483)
musttail call tailcc void %clofunc55557(%struct.ScmObj* %k47483, %struct.ScmObj* %argslist53880$k474832)
ret void
}

define tailcc void @proc_clo$ae47818(%struct.ScmObj* %env$ae47818,%struct.ScmObj* %current_45args53869) {
%stackaddr$env-ref55558 = alloca %struct.ScmObj*, align 8
%_37take47082 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47818, i64 0)
store %struct.ScmObj* %_37take47082, %struct.ScmObj** %stackaddr$env-ref55558
%stackaddr$prim55559 = alloca %struct.ScmObj*, align 8
%k47484 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53869)
store volatile %struct.ScmObj* %k47484, %struct.ScmObj** %stackaddr$prim55559, align 8
%stackaddr$prim55560 = alloca %struct.ScmObj*, align 8
%current_45args53870 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53869)
store volatile %struct.ScmObj* %current_45args53870, %struct.ScmObj** %stackaddr$prim55560, align 8
%stackaddr$prim55561 = alloca %struct.ScmObj*, align 8
%lst47084 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53870)
store volatile %struct.ScmObj* %lst47084, %struct.ScmObj** %stackaddr$prim55561, align 8
%stackaddr$prim55562 = alloca %struct.ScmObj*, align 8
%current_45args53871 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53870)
store volatile %struct.ScmObj* %current_45args53871, %struct.ScmObj** %stackaddr$prim55562, align 8
%stackaddr$prim55563 = alloca %struct.ScmObj*, align 8
%n47083 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53871)
store volatile %struct.ScmObj* %n47083, %struct.ScmObj** %stackaddr$prim55563, align 8
%ae47820 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55564 = alloca %struct.ScmObj*, align 8
%anf_45bind47210 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n47083, %struct.ScmObj* %ae47820)
store volatile %struct.ScmObj* %anf_45bind47210, %struct.ScmObj** %stackaddr$prim55564, align 8
%truthy$cmp55565 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47210)
%cmp$cmp55565 = icmp eq i64 %truthy$cmp55565, 1
br i1 %cmp$cmp55565, label %truebranch$cmp55565, label %falsebranch$cmp55565
truebranch$cmp55565:
%ae47823 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47824 = call %struct.ScmObj* @const_init_null()
%argslist53873$k474840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55566 = alloca %struct.ScmObj*, align 8
%argslist53873$k474841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47824, %struct.ScmObj* %argslist53873$k474840)
store volatile %struct.ScmObj* %argslist53873$k474841, %struct.ScmObj** %stackaddr$prim55566, align 8
%stackaddr$prim55567 = alloca %struct.ScmObj*, align 8
%argslist53873$k474842 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47823, %struct.ScmObj* %argslist53873$k474841)
store volatile %struct.ScmObj* %argslist53873$k474842, %struct.ScmObj** %stackaddr$prim55567, align 8
%clofunc55568 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47484)
musttail call tailcc void %clofunc55568(%struct.ScmObj* %k47484, %struct.ScmObj* %argslist53873$k474842)
ret void
falsebranch$cmp55565:
%stackaddr$prim55569 = alloca %struct.ScmObj*, align 8
%anf_45bind47211 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47084)
store volatile %struct.ScmObj* %anf_45bind47211, %struct.ScmObj** %stackaddr$prim55569, align 8
%truthy$cmp55570 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47211)
%cmp$cmp55570 = icmp eq i64 %truthy$cmp55570, 1
br i1 %cmp$cmp55570, label %truebranch$cmp55570, label %falsebranch$cmp55570
truebranch$cmp55570:
%ae47834 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47835 = call %struct.ScmObj* @const_init_null()
%argslist53874$k474840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55571 = alloca %struct.ScmObj*, align 8
%argslist53874$k474841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47835, %struct.ScmObj* %argslist53874$k474840)
store volatile %struct.ScmObj* %argslist53874$k474841, %struct.ScmObj** %stackaddr$prim55571, align 8
%stackaddr$prim55572 = alloca %struct.ScmObj*, align 8
%argslist53874$k474842 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47834, %struct.ScmObj* %argslist53874$k474841)
store volatile %struct.ScmObj* %argslist53874$k474842, %struct.ScmObj** %stackaddr$prim55572, align 8
%clofunc55573 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47484)
musttail call tailcc void %clofunc55573(%struct.ScmObj* %k47484, %struct.ScmObj* %argslist53874$k474842)
ret void
falsebranch$cmp55570:
%stackaddr$prim55574 = alloca %struct.ScmObj*, align 8
%anf_45bind47212 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47084)
store volatile %struct.ScmObj* %anf_45bind47212, %struct.ScmObj** %stackaddr$prim55574, align 8
%stackaddr$prim55575 = alloca %struct.ScmObj*, align 8
%anf_45bind47213 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47084)
store volatile %struct.ScmObj* %anf_45bind47213, %struct.ScmObj** %stackaddr$prim55575, align 8
%ae47845 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55576 = alloca %struct.ScmObj*, align 8
%anf_45bind47214 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n47083, %struct.ScmObj* %ae47845)
store volatile %struct.ScmObj* %anf_45bind47214, %struct.ScmObj** %stackaddr$prim55576, align 8
%stackaddr$makeclosure55577 = alloca %struct.ScmObj*, align 8
%fptrToInt55578 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47847 to i64
%ae47847 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55578)
store volatile %struct.ScmObj* %ae47847, %struct.ScmObj** %stackaddr$makeclosure55577, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47847, %struct.ScmObj* %k47484, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47847, %struct.ScmObj* %anf_45bind47212, i64 1)
%argslist53879$_37take470820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55579 = alloca %struct.ScmObj*, align 8
%argslist53879$_37take470821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47214, %struct.ScmObj* %argslist53879$_37take470820)
store volatile %struct.ScmObj* %argslist53879$_37take470821, %struct.ScmObj** %stackaddr$prim55579, align 8
%stackaddr$prim55580 = alloca %struct.ScmObj*, align 8
%argslist53879$_37take470822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47213, %struct.ScmObj* %argslist53879$_37take470821)
store volatile %struct.ScmObj* %argslist53879$_37take470822, %struct.ScmObj** %stackaddr$prim55580, align 8
%stackaddr$prim55581 = alloca %struct.ScmObj*, align 8
%argslist53879$_37take470823 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47847, %struct.ScmObj* %argslist53879$_37take470822)
store volatile %struct.ScmObj* %argslist53879$_37take470823, %struct.ScmObj** %stackaddr$prim55581, align 8
%clofunc55582 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take47082)
musttail call tailcc void %clofunc55582(%struct.ScmObj* %_37take47082, %struct.ScmObj* %argslist53879$_37take470823)
ret void
}

define tailcc void @proc_clo$ae47847(%struct.ScmObj* %env$ae47847,%struct.ScmObj* %current_45args53875) {
%stackaddr$env-ref55583 = alloca %struct.ScmObj*, align 8
%k47484 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47847, i64 0)
store %struct.ScmObj* %k47484, %struct.ScmObj** %stackaddr$env-ref55583
%stackaddr$env-ref55584 = alloca %struct.ScmObj*, align 8
%anf_45bind47212 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47847, i64 1)
store %struct.ScmObj* %anf_45bind47212, %struct.ScmObj** %stackaddr$env-ref55584
%stackaddr$prim55585 = alloca %struct.ScmObj*, align 8
%_95k47485 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53875)
store volatile %struct.ScmObj* %_95k47485, %struct.ScmObj** %stackaddr$prim55585, align 8
%stackaddr$prim55586 = alloca %struct.ScmObj*, align 8
%current_45args53876 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53875)
store volatile %struct.ScmObj* %current_45args53876, %struct.ScmObj** %stackaddr$prim55586, align 8
%stackaddr$prim55587 = alloca %struct.ScmObj*, align 8
%anf_45bind47215 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53876)
store volatile %struct.ScmObj* %anf_45bind47215, %struct.ScmObj** %stackaddr$prim55587, align 8
%stackaddr$prim55588 = alloca %struct.ScmObj*, align 8
%cpsprim47486 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47212, %struct.ScmObj* %anf_45bind47215)
store volatile %struct.ScmObj* %cpsprim47486, %struct.ScmObj** %stackaddr$prim55588, align 8
%ae47853 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53878$k474840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55589 = alloca %struct.ScmObj*, align 8
%argslist53878$k474841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47486, %struct.ScmObj* %argslist53878$k474840)
store volatile %struct.ScmObj* %argslist53878$k474841, %struct.ScmObj** %stackaddr$prim55589, align 8
%stackaddr$prim55590 = alloca %struct.ScmObj*, align 8
%argslist53878$k474842 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47853, %struct.ScmObj* %argslist53878$k474841)
store volatile %struct.ScmObj* %argslist53878$k474842, %struct.ScmObj** %stackaddr$prim55590, align 8
%clofunc55591 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47484)
musttail call tailcc void %clofunc55591(%struct.ScmObj* %k47484, %struct.ScmObj* %argslist53878$k474842)
ret void
}

define tailcc void @proc_clo$ae47718(%struct.ScmObj* %env$ae47718,%struct.ScmObj* %current_45args53883) {
%stackaddr$prim55592 = alloca %struct.ScmObj*, align 8
%k47487 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53883)
store volatile %struct.ScmObj* %k47487, %struct.ScmObj** %stackaddr$prim55592, align 8
%stackaddr$prim55593 = alloca %struct.ScmObj*, align 8
%current_45args53884 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53883)
store volatile %struct.ScmObj* %current_45args53884, %struct.ScmObj** %stackaddr$prim55593, align 8
%stackaddr$prim55594 = alloca %struct.ScmObj*, align 8
%_37map47086 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53884)
store volatile %struct.ScmObj* %_37map47086, %struct.ScmObj** %stackaddr$prim55594, align 8
%ae47720 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55595 = alloca %struct.ScmObj*, align 8
%fptrToInt55596 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47721 to i64
%ae47721 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55596)
store volatile %struct.ScmObj* %ae47721, %struct.ScmObj** %stackaddr$makeclosure55595, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47721, %struct.ScmObj* %_37map47086, i64 0)
%argslist53900$k474870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55597 = alloca %struct.ScmObj*, align 8
%argslist53900$k474871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47721, %struct.ScmObj* %argslist53900$k474870)
store volatile %struct.ScmObj* %argslist53900$k474871, %struct.ScmObj** %stackaddr$prim55597, align 8
%stackaddr$prim55598 = alloca %struct.ScmObj*, align 8
%argslist53900$k474872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47720, %struct.ScmObj* %argslist53900$k474871)
store volatile %struct.ScmObj* %argslist53900$k474872, %struct.ScmObj** %stackaddr$prim55598, align 8
%clofunc55599 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47487)
musttail call tailcc void %clofunc55599(%struct.ScmObj* %k47487, %struct.ScmObj* %argslist53900$k474872)
ret void
}

define tailcc void @proc_clo$ae47721(%struct.ScmObj* %env$ae47721,%struct.ScmObj* %current_45args53886) {
%stackaddr$env-ref55600 = alloca %struct.ScmObj*, align 8
%_37map47086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47721, i64 0)
store %struct.ScmObj* %_37map47086, %struct.ScmObj** %stackaddr$env-ref55600
%stackaddr$prim55601 = alloca %struct.ScmObj*, align 8
%k47488 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53886)
store volatile %struct.ScmObj* %k47488, %struct.ScmObj** %stackaddr$prim55601, align 8
%stackaddr$prim55602 = alloca %struct.ScmObj*, align 8
%current_45args53887 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53886)
store volatile %struct.ScmObj* %current_45args53887, %struct.ScmObj** %stackaddr$prim55602, align 8
%stackaddr$prim55603 = alloca %struct.ScmObj*, align 8
%f47088 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53887)
store volatile %struct.ScmObj* %f47088, %struct.ScmObj** %stackaddr$prim55603, align 8
%stackaddr$prim55604 = alloca %struct.ScmObj*, align 8
%current_45args53888 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53887)
store volatile %struct.ScmObj* %current_45args53888, %struct.ScmObj** %stackaddr$prim55604, align 8
%stackaddr$prim55605 = alloca %struct.ScmObj*, align 8
%lst47087 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53888)
store volatile %struct.ScmObj* %lst47087, %struct.ScmObj** %stackaddr$prim55605, align 8
%stackaddr$prim55606 = alloca %struct.ScmObj*, align 8
%anf_45bind47204 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47087)
store volatile %struct.ScmObj* %anf_45bind47204, %struct.ScmObj** %stackaddr$prim55606, align 8
%truthy$cmp55607 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47204)
%cmp$cmp55607 = icmp eq i64 %truthy$cmp55607, 1
br i1 %cmp$cmp55607, label %truebranch$cmp55607, label %falsebranch$cmp55607
truebranch$cmp55607:
%ae47725 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47726 = call %struct.ScmObj* @const_init_null()
%argslist53890$k474880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55608 = alloca %struct.ScmObj*, align 8
%argslist53890$k474881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47726, %struct.ScmObj* %argslist53890$k474880)
store volatile %struct.ScmObj* %argslist53890$k474881, %struct.ScmObj** %stackaddr$prim55608, align 8
%stackaddr$prim55609 = alloca %struct.ScmObj*, align 8
%argslist53890$k474882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47725, %struct.ScmObj* %argslist53890$k474881)
store volatile %struct.ScmObj* %argslist53890$k474882, %struct.ScmObj** %stackaddr$prim55609, align 8
%clofunc55610 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47488)
musttail call tailcc void %clofunc55610(%struct.ScmObj* %k47488, %struct.ScmObj* %argslist53890$k474882)
ret void
falsebranch$cmp55607:
%stackaddr$prim55611 = alloca %struct.ScmObj*, align 8
%anf_45bind47205 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47087)
store volatile %struct.ScmObj* %anf_45bind47205, %struct.ScmObj** %stackaddr$prim55611, align 8
%stackaddr$makeclosure55612 = alloca %struct.ScmObj*, align 8
%fptrToInt55613 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47735 to i64
%ae47735 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55613)
store volatile %struct.ScmObj* %ae47735, %struct.ScmObj** %stackaddr$makeclosure55612, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47735, %struct.ScmObj* %k47488, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47735, %struct.ScmObj* %f47088, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47735, %struct.ScmObj* %lst47087, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae47735, %struct.ScmObj* %_37map47086, i64 3)
%argslist53899$f470880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55614 = alloca %struct.ScmObj*, align 8
%argslist53899$f470881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47205, %struct.ScmObj* %argslist53899$f470880)
store volatile %struct.ScmObj* %argslist53899$f470881, %struct.ScmObj** %stackaddr$prim55614, align 8
%stackaddr$prim55615 = alloca %struct.ScmObj*, align 8
%argslist53899$f470882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47735, %struct.ScmObj* %argslist53899$f470881)
store volatile %struct.ScmObj* %argslist53899$f470882, %struct.ScmObj** %stackaddr$prim55615, align 8
%clofunc55616 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47088)
musttail call tailcc void %clofunc55616(%struct.ScmObj* %f47088, %struct.ScmObj* %argslist53899$f470882)
ret void
}

define tailcc void @proc_clo$ae47735(%struct.ScmObj* %env$ae47735,%struct.ScmObj* %current_45args53891) {
%stackaddr$env-ref55617 = alloca %struct.ScmObj*, align 8
%k47488 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47735, i64 0)
store %struct.ScmObj* %k47488, %struct.ScmObj** %stackaddr$env-ref55617
%stackaddr$env-ref55618 = alloca %struct.ScmObj*, align 8
%f47088 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47735, i64 1)
store %struct.ScmObj* %f47088, %struct.ScmObj** %stackaddr$env-ref55618
%stackaddr$env-ref55619 = alloca %struct.ScmObj*, align 8
%lst47087 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47735, i64 2)
store %struct.ScmObj* %lst47087, %struct.ScmObj** %stackaddr$env-ref55619
%stackaddr$env-ref55620 = alloca %struct.ScmObj*, align 8
%_37map47086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47735, i64 3)
store %struct.ScmObj* %_37map47086, %struct.ScmObj** %stackaddr$env-ref55620
%stackaddr$prim55621 = alloca %struct.ScmObj*, align 8
%_95k47489 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53891)
store volatile %struct.ScmObj* %_95k47489, %struct.ScmObj** %stackaddr$prim55621, align 8
%stackaddr$prim55622 = alloca %struct.ScmObj*, align 8
%current_45args53892 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53891)
store volatile %struct.ScmObj* %current_45args53892, %struct.ScmObj** %stackaddr$prim55622, align 8
%stackaddr$prim55623 = alloca %struct.ScmObj*, align 8
%anf_45bind47206 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53892)
store volatile %struct.ScmObj* %anf_45bind47206, %struct.ScmObj** %stackaddr$prim55623, align 8
%stackaddr$prim55624 = alloca %struct.ScmObj*, align 8
%anf_45bind47207 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47087)
store volatile %struct.ScmObj* %anf_45bind47207, %struct.ScmObj** %stackaddr$prim55624, align 8
%stackaddr$makeclosure55625 = alloca %struct.ScmObj*, align 8
%fptrToInt55626 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47739 to i64
%ae47739 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55626)
store volatile %struct.ScmObj* %ae47739, %struct.ScmObj** %stackaddr$makeclosure55625, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47739, %struct.ScmObj* %anf_45bind47206, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47739, %struct.ScmObj* %k47488, i64 1)
%argslist53898$_37map470860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55627 = alloca %struct.ScmObj*, align 8
%argslist53898$_37map470861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47207, %struct.ScmObj* %argslist53898$_37map470860)
store volatile %struct.ScmObj* %argslist53898$_37map470861, %struct.ScmObj** %stackaddr$prim55627, align 8
%stackaddr$prim55628 = alloca %struct.ScmObj*, align 8
%argslist53898$_37map470862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47088, %struct.ScmObj* %argslist53898$_37map470861)
store volatile %struct.ScmObj* %argslist53898$_37map470862, %struct.ScmObj** %stackaddr$prim55628, align 8
%stackaddr$prim55629 = alloca %struct.ScmObj*, align 8
%argslist53898$_37map470863 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47739, %struct.ScmObj* %argslist53898$_37map470862)
store volatile %struct.ScmObj* %argslist53898$_37map470863, %struct.ScmObj** %stackaddr$prim55629, align 8
%clofunc55630 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map47086)
musttail call tailcc void %clofunc55630(%struct.ScmObj* %_37map47086, %struct.ScmObj* %argslist53898$_37map470863)
ret void
}

define tailcc void @proc_clo$ae47739(%struct.ScmObj* %env$ae47739,%struct.ScmObj* %current_45args53894) {
%stackaddr$env-ref55631 = alloca %struct.ScmObj*, align 8
%anf_45bind47206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47739, i64 0)
store %struct.ScmObj* %anf_45bind47206, %struct.ScmObj** %stackaddr$env-ref55631
%stackaddr$env-ref55632 = alloca %struct.ScmObj*, align 8
%k47488 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47739, i64 1)
store %struct.ScmObj* %k47488, %struct.ScmObj** %stackaddr$env-ref55632
%stackaddr$prim55633 = alloca %struct.ScmObj*, align 8
%_95k47490 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53894)
store volatile %struct.ScmObj* %_95k47490, %struct.ScmObj** %stackaddr$prim55633, align 8
%stackaddr$prim55634 = alloca %struct.ScmObj*, align 8
%current_45args53895 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53894)
store volatile %struct.ScmObj* %current_45args53895, %struct.ScmObj** %stackaddr$prim55634, align 8
%stackaddr$prim55635 = alloca %struct.ScmObj*, align 8
%anf_45bind47208 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53895)
store volatile %struct.ScmObj* %anf_45bind47208, %struct.ScmObj** %stackaddr$prim55635, align 8
%stackaddr$prim55636 = alloca %struct.ScmObj*, align 8
%cpsprim47491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47206, %struct.ScmObj* %anf_45bind47208)
store volatile %struct.ScmObj* %cpsprim47491, %struct.ScmObj** %stackaddr$prim55636, align 8
%ae47745 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53897$k474880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55637 = alloca %struct.ScmObj*, align 8
%argslist53897$k474881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47491, %struct.ScmObj* %argslist53897$k474880)
store volatile %struct.ScmObj* %argslist53897$k474881, %struct.ScmObj** %stackaddr$prim55637, align 8
%stackaddr$prim55638 = alloca %struct.ScmObj*, align 8
%argslist53897$k474882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47745, %struct.ScmObj* %argslist53897$k474881)
store volatile %struct.ScmObj* %argslist53897$k474882, %struct.ScmObj** %stackaddr$prim55638, align 8
%clofunc55639 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47488)
musttail call tailcc void %clofunc55639(%struct.ScmObj* %k47488, %struct.ScmObj* %argslist53897$k474882)
ret void
}

define tailcc void @proc_clo$ae47638(%struct.ScmObj* %env$ae47638,%struct.ScmObj* %current_45args53903) {
%stackaddr$prim55640 = alloca %struct.ScmObj*, align 8
%k47492 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53903)
store volatile %struct.ScmObj* %k47492, %struct.ScmObj** %stackaddr$prim55640, align 8
%stackaddr$prim55641 = alloca %struct.ScmObj*, align 8
%current_45args53904 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53903)
store volatile %struct.ScmObj* %current_45args53904, %struct.ScmObj** %stackaddr$prim55641, align 8
%stackaddr$prim55642 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53904)
store volatile %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$prim55642, align 8
%ae47640 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55643 = alloca %struct.ScmObj*, align 8
%fptrToInt55644 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47641 to i64
%ae47641 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55644)
store volatile %struct.ScmObj* %ae47641, %struct.ScmObj** %stackaddr$makeclosure55643, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47641, %struct.ScmObj* %_37foldr147090, i64 0)
%argslist53917$k474920 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55645 = alloca %struct.ScmObj*, align 8
%argslist53917$k474921 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47641, %struct.ScmObj* %argslist53917$k474920)
store volatile %struct.ScmObj* %argslist53917$k474921, %struct.ScmObj** %stackaddr$prim55645, align 8
%stackaddr$prim55646 = alloca %struct.ScmObj*, align 8
%argslist53917$k474922 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47640, %struct.ScmObj* %argslist53917$k474921)
store volatile %struct.ScmObj* %argslist53917$k474922, %struct.ScmObj** %stackaddr$prim55646, align 8
%clofunc55647 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47492)
musttail call tailcc void %clofunc55647(%struct.ScmObj* %k47492, %struct.ScmObj* %argslist53917$k474922)
ret void
}

define tailcc void @proc_clo$ae47641(%struct.ScmObj* %env$ae47641,%struct.ScmObj* %current_45args53906) {
%stackaddr$env-ref55648 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47641, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref55648
%stackaddr$prim55649 = alloca %struct.ScmObj*, align 8
%k47493 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53906)
store volatile %struct.ScmObj* %k47493, %struct.ScmObj** %stackaddr$prim55649, align 8
%stackaddr$prim55650 = alloca %struct.ScmObj*, align 8
%current_45args53907 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53906)
store volatile %struct.ScmObj* %current_45args53907, %struct.ScmObj** %stackaddr$prim55650, align 8
%stackaddr$prim55651 = alloca %struct.ScmObj*, align 8
%f47093 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53907)
store volatile %struct.ScmObj* %f47093, %struct.ScmObj** %stackaddr$prim55651, align 8
%stackaddr$prim55652 = alloca %struct.ScmObj*, align 8
%current_45args53908 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53907)
store volatile %struct.ScmObj* %current_45args53908, %struct.ScmObj** %stackaddr$prim55652, align 8
%stackaddr$prim55653 = alloca %struct.ScmObj*, align 8
%acc47092 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53908)
store volatile %struct.ScmObj* %acc47092, %struct.ScmObj** %stackaddr$prim55653, align 8
%stackaddr$prim55654 = alloca %struct.ScmObj*, align 8
%current_45args53909 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53908)
store volatile %struct.ScmObj* %current_45args53909, %struct.ScmObj** %stackaddr$prim55654, align 8
%stackaddr$prim55655 = alloca %struct.ScmObj*, align 8
%lst47091 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53909)
store volatile %struct.ScmObj* %lst47091, %struct.ScmObj** %stackaddr$prim55655, align 8
%stackaddr$prim55656 = alloca %struct.ScmObj*, align 8
%anf_45bind47199 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47091)
store volatile %struct.ScmObj* %anf_45bind47199, %struct.ScmObj** %stackaddr$prim55656, align 8
%truthy$cmp55657 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47199)
%cmp$cmp55657 = icmp eq i64 %truthy$cmp55657, 1
br i1 %cmp$cmp55657, label %truebranch$cmp55657, label %falsebranch$cmp55657
truebranch$cmp55657:
%ae47645 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53911$k474930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55658 = alloca %struct.ScmObj*, align 8
%argslist53911$k474931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47092, %struct.ScmObj* %argslist53911$k474930)
store volatile %struct.ScmObj* %argslist53911$k474931, %struct.ScmObj** %stackaddr$prim55658, align 8
%stackaddr$prim55659 = alloca %struct.ScmObj*, align 8
%argslist53911$k474932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47645, %struct.ScmObj* %argslist53911$k474931)
store volatile %struct.ScmObj* %argslist53911$k474932, %struct.ScmObj** %stackaddr$prim55659, align 8
%clofunc55660 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47493)
musttail call tailcc void %clofunc55660(%struct.ScmObj* %k47493, %struct.ScmObj* %argslist53911$k474932)
ret void
falsebranch$cmp55657:
%stackaddr$prim55661 = alloca %struct.ScmObj*, align 8
%anf_45bind47200 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47091)
store volatile %struct.ScmObj* %anf_45bind47200, %struct.ScmObj** %stackaddr$prim55661, align 8
%stackaddr$prim55662 = alloca %struct.ScmObj*, align 8
%anf_45bind47201 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47091)
store volatile %struct.ScmObj* %anf_45bind47201, %struct.ScmObj** %stackaddr$prim55662, align 8
%stackaddr$makeclosure55663 = alloca %struct.ScmObj*, align 8
%fptrToInt55664 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47653 to i64
%ae47653 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55664)
store volatile %struct.ScmObj* %ae47653, %struct.ScmObj** %stackaddr$makeclosure55663, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47653, %struct.ScmObj* %k47493, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47653, %struct.ScmObj* %f47093, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47653, %struct.ScmObj* %anf_45bind47200, i64 2)
%argslist53916$_37foldr1470900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55665 = alloca %struct.ScmObj*, align 8
%argslist53916$_37foldr1470901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47201, %struct.ScmObj* %argslist53916$_37foldr1470900)
store volatile %struct.ScmObj* %argslist53916$_37foldr1470901, %struct.ScmObj** %stackaddr$prim55665, align 8
%stackaddr$prim55666 = alloca %struct.ScmObj*, align 8
%argslist53916$_37foldr1470902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47092, %struct.ScmObj* %argslist53916$_37foldr1470901)
store volatile %struct.ScmObj* %argslist53916$_37foldr1470902, %struct.ScmObj** %stackaddr$prim55666, align 8
%stackaddr$prim55667 = alloca %struct.ScmObj*, align 8
%argslist53916$_37foldr1470903 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47093, %struct.ScmObj* %argslist53916$_37foldr1470902)
store volatile %struct.ScmObj* %argslist53916$_37foldr1470903, %struct.ScmObj** %stackaddr$prim55667, align 8
%stackaddr$prim55668 = alloca %struct.ScmObj*, align 8
%argslist53916$_37foldr1470904 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47653, %struct.ScmObj* %argslist53916$_37foldr1470903)
store volatile %struct.ScmObj* %argslist53916$_37foldr1470904, %struct.ScmObj** %stackaddr$prim55668, align 8
%clofunc55669 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147090)
musttail call tailcc void %clofunc55669(%struct.ScmObj* %_37foldr147090, %struct.ScmObj* %argslist53916$_37foldr1470904)
ret void
}

define tailcc void @proc_clo$ae47653(%struct.ScmObj* %env$ae47653,%struct.ScmObj* %current_45args53912) {
%stackaddr$env-ref55670 = alloca %struct.ScmObj*, align 8
%k47493 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47653, i64 0)
store %struct.ScmObj* %k47493, %struct.ScmObj** %stackaddr$env-ref55670
%stackaddr$env-ref55671 = alloca %struct.ScmObj*, align 8
%f47093 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47653, i64 1)
store %struct.ScmObj* %f47093, %struct.ScmObj** %stackaddr$env-ref55671
%stackaddr$env-ref55672 = alloca %struct.ScmObj*, align 8
%anf_45bind47200 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47653, i64 2)
store %struct.ScmObj* %anf_45bind47200, %struct.ScmObj** %stackaddr$env-ref55672
%stackaddr$prim55673 = alloca %struct.ScmObj*, align 8
%_95k47494 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53912)
store volatile %struct.ScmObj* %_95k47494, %struct.ScmObj** %stackaddr$prim55673, align 8
%stackaddr$prim55674 = alloca %struct.ScmObj*, align 8
%current_45args53913 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53912)
store volatile %struct.ScmObj* %current_45args53913, %struct.ScmObj** %stackaddr$prim55674, align 8
%stackaddr$prim55675 = alloca %struct.ScmObj*, align 8
%anf_45bind47202 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53913)
store volatile %struct.ScmObj* %anf_45bind47202, %struct.ScmObj** %stackaddr$prim55675, align 8
%argslist53915$f470930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55676 = alloca %struct.ScmObj*, align 8
%argslist53915$f470931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47202, %struct.ScmObj* %argslist53915$f470930)
store volatile %struct.ScmObj* %argslist53915$f470931, %struct.ScmObj** %stackaddr$prim55676, align 8
%stackaddr$prim55677 = alloca %struct.ScmObj*, align 8
%argslist53915$f470932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47200, %struct.ScmObj* %argslist53915$f470931)
store volatile %struct.ScmObj* %argslist53915$f470932, %struct.ScmObj** %stackaddr$prim55677, align 8
%stackaddr$prim55678 = alloca %struct.ScmObj*, align 8
%argslist53915$f470933 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47493, %struct.ScmObj* %argslist53915$f470932)
store volatile %struct.ScmObj* %argslist53915$f470933, %struct.ScmObj** %stackaddr$prim55678, align 8
%clofunc55679 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47093)
musttail call tailcc void %clofunc55679(%struct.ScmObj* %f47093, %struct.ScmObj* %argslist53915$f470933)
ret void
}

define tailcc void @proc_clo$ae47521(%struct.ScmObj* %env$ae47521,%struct.ScmObj* %current_45args53920) {
%stackaddr$prim55680 = alloca %struct.ScmObj*, align 8
%k47495 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53920)
store volatile %struct.ScmObj* %k47495, %struct.ScmObj** %stackaddr$prim55680, align 8
%stackaddr$prim55681 = alloca %struct.ScmObj*, align 8
%current_45args53921 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53920)
store volatile %struct.ScmObj* %current_45args53921, %struct.ScmObj** %stackaddr$prim55681, align 8
%stackaddr$prim55682 = alloca %struct.ScmObj*, align 8
%y47070 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53921)
store volatile %struct.ScmObj* %y47070, %struct.ScmObj** %stackaddr$prim55682, align 8
%ae47523 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55683 = alloca %struct.ScmObj*, align 8
%fptrToInt55684 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47524 to i64
%ae47524 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55684)
store volatile %struct.ScmObj* %ae47524, %struct.ScmObj** %stackaddr$makeclosure55683, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47524, %struct.ScmObj* %y47070, i64 0)
%argslist53939$k474950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55685 = alloca %struct.ScmObj*, align 8
%argslist53939$k474951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47524, %struct.ScmObj* %argslist53939$k474950)
store volatile %struct.ScmObj* %argslist53939$k474951, %struct.ScmObj** %stackaddr$prim55685, align 8
%stackaddr$prim55686 = alloca %struct.ScmObj*, align 8
%argslist53939$k474952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47523, %struct.ScmObj* %argslist53939$k474951)
store volatile %struct.ScmObj* %argslist53939$k474952, %struct.ScmObj** %stackaddr$prim55686, align 8
%clofunc55687 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47495)
musttail call tailcc void %clofunc55687(%struct.ScmObj* %k47495, %struct.ScmObj* %argslist53939$k474952)
ret void
}

define tailcc void @proc_clo$ae47524(%struct.ScmObj* %env$ae47524,%struct.ScmObj* %current_45args53923) {
%stackaddr$env-ref55688 = alloca %struct.ScmObj*, align 8
%y47070 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47524, i64 0)
store %struct.ScmObj* %y47070, %struct.ScmObj** %stackaddr$env-ref55688
%stackaddr$prim55689 = alloca %struct.ScmObj*, align 8
%k47496 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53923)
store volatile %struct.ScmObj* %k47496, %struct.ScmObj** %stackaddr$prim55689, align 8
%stackaddr$prim55690 = alloca %struct.ScmObj*, align 8
%current_45args53924 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53923)
store volatile %struct.ScmObj* %current_45args53924, %struct.ScmObj** %stackaddr$prim55690, align 8
%stackaddr$prim55691 = alloca %struct.ScmObj*, align 8
%f47071 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53924)
store volatile %struct.ScmObj* %f47071, %struct.ScmObj** %stackaddr$prim55691, align 8
%stackaddr$makeclosure55692 = alloca %struct.ScmObj*, align 8
%fptrToInt55693 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47525 to i64
%ae47525 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55693)
store volatile %struct.ScmObj* %ae47525, %struct.ScmObj** %stackaddr$makeclosure55692, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47525, %struct.ScmObj* %f47071, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47525, %struct.ScmObj* %k47496, i64 1)
%ae47526 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55694 = alloca %struct.ScmObj*, align 8
%fptrToInt55695 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47527 to i64
%ae47527 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55695)
store volatile %struct.ScmObj* %ae47527, %struct.ScmObj** %stackaddr$makeclosure55694, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47527, %struct.ScmObj* %f47071, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47527, %struct.ScmObj* %y47070, i64 1)
%argslist53938$ae475250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55696 = alloca %struct.ScmObj*, align 8
%argslist53938$ae475251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47527, %struct.ScmObj* %argslist53938$ae475250)
store volatile %struct.ScmObj* %argslist53938$ae475251, %struct.ScmObj** %stackaddr$prim55696, align 8
%stackaddr$prim55697 = alloca %struct.ScmObj*, align 8
%argslist53938$ae475252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47526, %struct.ScmObj* %argslist53938$ae475251)
store volatile %struct.ScmObj* %argslist53938$ae475252, %struct.ScmObj** %stackaddr$prim55697, align 8
%clofunc55698 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47525)
musttail call tailcc void %clofunc55698(%struct.ScmObj* %ae47525, %struct.ScmObj* %argslist53938$ae475252)
ret void
}

define tailcc void @proc_clo$ae47525(%struct.ScmObj* %env$ae47525,%struct.ScmObj* %current_45args53926) {
%stackaddr$env-ref55699 = alloca %struct.ScmObj*, align 8
%f47071 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47525, i64 0)
store %struct.ScmObj* %f47071, %struct.ScmObj** %stackaddr$env-ref55699
%stackaddr$env-ref55700 = alloca %struct.ScmObj*, align 8
%k47496 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47525, i64 1)
store %struct.ScmObj* %k47496, %struct.ScmObj** %stackaddr$env-ref55700
%stackaddr$prim55701 = alloca %struct.ScmObj*, align 8
%_95k47497 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53926)
store volatile %struct.ScmObj* %_95k47497, %struct.ScmObj** %stackaddr$prim55701, align 8
%stackaddr$prim55702 = alloca %struct.ScmObj*, align 8
%current_45args53927 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53926)
store volatile %struct.ScmObj* %current_45args53927, %struct.ScmObj** %stackaddr$prim55702, align 8
%stackaddr$prim55703 = alloca %struct.ScmObj*, align 8
%anf_45bind47197 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53927)
store volatile %struct.ScmObj* %anf_45bind47197, %struct.ScmObj** %stackaddr$prim55703, align 8
%argslist53929$f470710 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55704 = alloca %struct.ScmObj*, align 8
%argslist53929$f470711 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47197, %struct.ScmObj* %argslist53929$f470710)
store volatile %struct.ScmObj* %argslist53929$f470711, %struct.ScmObj** %stackaddr$prim55704, align 8
%stackaddr$prim55705 = alloca %struct.ScmObj*, align 8
%argslist53929$f470712 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47496, %struct.ScmObj* %argslist53929$f470711)
store volatile %struct.ScmObj* %argslist53929$f470712, %struct.ScmObj** %stackaddr$prim55705, align 8
%clofunc55706 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47071)
musttail call tailcc void %clofunc55706(%struct.ScmObj* %f47071, %struct.ScmObj* %argslist53929$f470712)
ret void
}

define tailcc void @proc_clo$ae47527(%struct.ScmObj* %env$ae47527,%struct.ScmObj* %args4707247498) {
%stackaddr$env-ref55707 = alloca %struct.ScmObj*, align 8
%f47071 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47527, i64 0)
store %struct.ScmObj* %f47071, %struct.ScmObj** %stackaddr$env-ref55707
%stackaddr$env-ref55708 = alloca %struct.ScmObj*, align 8
%y47070 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47527, i64 1)
store %struct.ScmObj* %y47070, %struct.ScmObj** %stackaddr$env-ref55708
%stackaddr$prim55709 = alloca %struct.ScmObj*, align 8
%k47499 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4707247498)
store volatile %struct.ScmObj* %k47499, %struct.ScmObj** %stackaddr$prim55709, align 8
%stackaddr$prim55710 = alloca %struct.ScmObj*, align 8
%args47072 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4707247498)
store volatile %struct.ScmObj* %args47072, %struct.ScmObj** %stackaddr$prim55710, align 8
%stackaddr$makeclosure55711 = alloca %struct.ScmObj*, align 8
%fptrToInt55712 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47531 to i64
%ae47531 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55712)
store volatile %struct.ScmObj* %ae47531, %struct.ScmObj** %stackaddr$makeclosure55711, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47531, %struct.ScmObj* %args47072, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47531, %struct.ScmObj* %f47071, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47531, %struct.ScmObj* %k47499, i64 2)
%argslist53937$y470700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55713 = alloca %struct.ScmObj*, align 8
%argslist53937$y470701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y47070, %struct.ScmObj* %argslist53937$y470700)
store volatile %struct.ScmObj* %argslist53937$y470701, %struct.ScmObj** %stackaddr$prim55713, align 8
%stackaddr$prim55714 = alloca %struct.ScmObj*, align 8
%argslist53937$y470702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47531, %struct.ScmObj* %argslist53937$y470701)
store volatile %struct.ScmObj* %argslist53937$y470702, %struct.ScmObj** %stackaddr$prim55714, align 8
%clofunc55715 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y47070)
musttail call tailcc void %clofunc55715(%struct.ScmObj* %y47070, %struct.ScmObj* %argslist53937$y470702)
ret void
}

define tailcc void @proc_clo$ae47531(%struct.ScmObj* %env$ae47531,%struct.ScmObj* %current_45args53930) {
%stackaddr$env-ref55716 = alloca %struct.ScmObj*, align 8
%args47072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47531, i64 0)
store %struct.ScmObj* %args47072, %struct.ScmObj** %stackaddr$env-ref55716
%stackaddr$env-ref55717 = alloca %struct.ScmObj*, align 8
%f47071 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47531, i64 1)
store %struct.ScmObj* %f47071, %struct.ScmObj** %stackaddr$env-ref55717
%stackaddr$env-ref55718 = alloca %struct.ScmObj*, align 8
%k47499 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47531, i64 2)
store %struct.ScmObj* %k47499, %struct.ScmObj** %stackaddr$env-ref55718
%stackaddr$prim55719 = alloca %struct.ScmObj*, align 8
%_95k47500 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53930)
store volatile %struct.ScmObj* %_95k47500, %struct.ScmObj** %stackaddr$prim55719, align 8
%stackaddr$prim55720 = alloca %struct.ScmObj*, align 8
%current_45args53931 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53930)
store volatile %struct.ScmObj* %current_45args53931, %struct.ScmObj** %stackaddr$prim55720, align 8
%stackaddr$prim55721 = alloca %struct.ScmObj*, align 8
%anf_45bind47195 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53931)
store volatile %struct.ScmObj* %anf_45bind47195, %struct.ScmObj** %stackaddr$prim55721, align 8
%stackaddr$makeclosure55722 = alloca %struct.ScmObj*, align 8
%fptrToInt55723 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47534 to i64
%ae47534 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55723)
store volatile %struct.ScmObj* %ae47534, %struct.ScmObj** %stackaddr$makeclosure55722, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47534, %struct.ScmObj* %args47072, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47534, %struct.ScmObj* %k47499, i64 1)
%argslist53936$anf_45bind471950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55724 = alloca %struct.ScmObj*, align 8
%argslist53936$anf_45bind471951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47071, %struct.ScmObj* %argslist53936$anf_45bind471950)
store volatile %struct.ScmObj* %argslist53936$anf_45bind471951, %struct.ScmObj** %stackaddr$prim55724, align 8
%stackaddr$prim55725 = alloca %struct.ScmObj*, align 8
%argslist53936$anf_45bind471952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47534, %struct.ScmObj* %argslist53936$anf_45bind471951)
store volatile %struct.ScmObj* %argslist53936$anf_45bind471952, %struct.ScmObj** %stackaddr$prim55725, align 8
%clofunc55726 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47195)
musttail call tailcc void %clofunc55726(%struct.ScmObj* %anf_45bind47195, %struct.ScmObj* %argslist53936$anf_45bind471952)
ret void
}

define tailcc void @proc_clo$ae47534(%struct.ScmObj* %env$ae47534,%struct.ScmObj* %current_45args53933) {
%stackaddr$env-ref55727 = alloca %struct.ScmObj*, align 8
%args47072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47534, i64 0)
store %struct.ScmObj* %args47072, %struct.ScmObj** %stackaddr$env-ref55727
%stackaddr$env-ref55728 = alloca %struct.ScmObj*, align 8
%k47499 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47534, i64 1)
store %struct.ScmObj* %k47499, %struct.ScmObj** %stackaddr$env-ref55728
%stackaddr$prim55729 = alloca %struct.ScmObj*, align 8
%_95k47501 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53933)
store volatile %struct.ScmObj* %_95k47501, %struct.ScmObj** %stackaddr$prim55729, align 8
%stackaddr$prim55730 = alloca %struct.ScmObj*, align 8
%current_45args53934 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53933)
store volatile %struct.ScmObj* %current_45args53934, %struct.ScmObj** %stackaddr$prim55730, align 8
%stackaddr$prim55731 = alloca %struct.ScmObj*, align 8
%anf_45bind47196 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53934)
store volatile %struct.ScmObj* %anf_45bind47196, %struct.ScmObj** %stackaddr$prim55731, align 8
%stackaddr$prim55732 = alloca %struct.ScmObj*, align 8
%cpsargs47502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47499, %struct.ScmObj* %args47072)
store volatile %struct.ScmObj* %cpsargs47502, %struct.ScmObj** %stackaddr$prim55732, align 8
%clofunc55733 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47196)
musttail call tailcc void %clofunc55733(%struct.ScmObj* %anf_45bind47196, %struct.ScmObj* %cpsargs47502)
ret void
}

define tailcc void @proc_clo$ae47506(%struct.ScmObj* %env$ae47506,%struct.ScmObj* %current_45args53941) {
%stackaddr$prim55734 = alloca %struct.ScmObj*, align 8
%k47503 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53941)
store volatile %struct.ScmObj* %k47503, %struct.ScmObj** %stackaddr$prim55734, align 8
%stackaddr$prim55735 = alloca %struct.ScmObj*, align 8
%current_45args53942 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53941)
store volatile %struct.ScmObj* %current_45args53942, %struct.ScmObj** %stackaddr$prim55735, align 8
%stackaddr$prim55736 = alloca %struct.ScmObj*, align 8
%yu47069 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53942)
store volatile %struct.ScmObj* %yu47069, %struct.ScmObj** %stackaddr$prim55736, align 8
%argslist53944$yu470690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55737 = alloca %struct.ScmObj*, align 8
%argslist53944$yu470691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu47069, %struct.ScmObj* %argslist53944$yu470690)
store volatile %struct.ScmObj* %argslist53944$yu470691, %struct.ScmObj** %stackaddr$prim55737, align 8
%stackaddr$prim55738 = alloca %struct.ScmObj*, align 8
%argslist53944$yu470692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47503, %struct.ScmObj* %argslist53944$yu470691)
store volatile %struct.ScmObj* %argslist53944$yu470692, %struct.ScmObj** %stackaddr$prim55738, align 8
%clofunc55739 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu47069)
musttail call tailcc void %clofunc55739(%struct.ScmObj* %yu47069, %struct.ScmObj* %argslist53944$yu470692)
ret void
}