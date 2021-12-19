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
%mainenv54905 = call %struct.ScmObj* @const_init_null()
%mainargs54906 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv54905, %struct.ScmObj* %mainargs54906)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv54903,%struct.ScmObj* %mainargs54904) {
%stackaddr$makeclosure54907 = alloca %struct.ScmObj*, align 8
%fptrToInt54908 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48461 to i64
%ae48461 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54908)
store volatile %struct.ScmObj* %ae48461, %struct.ScmObj** %stackaddr$makeclosure54907, align 8
%ae48462 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54909 = alloca %struct.ScmObj*, align 8
%fptrToInt54910 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48463 to i64
%ae48463 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54910)
store volatile %struct.ScmObj* %ae48463, %struct.ScmObj** %stackaddr$makeclosure54909, align 8
%argslist54902$ae484610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54911 = alloca %struct.ScmObj*, align 8
%argslist54902$ae484611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48463, %struct.ScmObj* %argslist54902$ae484610)
store volatile %struct.ScmObj* %argslist54902$ae484611, %struct.ScmObj** %stackaddr$prim54911, align 8
%stackaddr$prim54912 = alloca %struct.ScmObj*, align 8
%argslist54902$ae484612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48462, %struct.ScmObj* %argslist54902$ae484611)
store volatile %struct.ScmObj* %argslist54902$ae484612, %struct.ScmObj** %stackaddr$prim54912, align 8
%clofunc54913 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48461)
musttail call tailcc void %clofunc54913(%struct.ScmObj* %ae48461, %struct.ScmObj* %argslist54902$ae484612)
ret void
}

define tailcc void @proc_clo$ae48461(%struct.ScmObj* %env$ae48461,%struct.ScmObj* %current_45args54321) {
%stackaddr$prim54914 = alloca %struct.ScmObj*, align 8
%_95k48275 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54321)
store volatile %struct.ScmObj* %_95k48275, %struct.ScmObj** %stackaddr$prim54914, align 8
%stackaddr$prim54915 = alloca %struct.ScmObj*, align 8
%current_45args54322 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54321)
store volatile %struct.ScmObj* %current_45args54322, %struct.ScmObj** %stackaddr$prim54915, align 8
%stackaddr$prim54916 = alloca %struct.ScmObj*, align 8
%anf_45bind48151 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54322)
store volatile %struct.ScmObj* %anf_45bind48151, %struct.ScmObj** %stackaddr$prim54916, align 8
%stackaddr$makeclosure54917 = alloca %struct.ScmObj*, align 8
%fptrToInt54918 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48476 to i64
%ae48476 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54918)
store volatile %struct.ScmObj* %ae48476, %struct.ScmObj** %stackaddr$makeclosure54917, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48476, %struct.ScmObj* %anf_45bind48151, i64 0)
%ae48477 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54919 = alloca %struct.ScmObj*, align 8
%fptrToInt54920 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48478 to i64
%ae48478 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54920)
store volatile %struct.ScmObj* %ae48478, %struct.ScmObj** %stackaddr$makeclosure54919, align 8
%argslist54897$ae484760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54921 = alloca %struct.ScmObj*, align 8
%argslist54897$ae484761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48478, %struct.ScmObj* %argslist54897$ae484760)
store volatile %struct.ScmObj* %argslist54897$ae484761, %struct.ScmObj** %stackaddr$prim54921, align 8
%stackaddr$prim54922 = alloca %struct.ScmObj*, align 8
%argslist54897$ae484762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48477, %struct.ScmObj* %argslist54897$ae484761)
store volatile %struct.ScmObj* %argslist54897$ae484762, %struct.ScmObj** %stackaddr$prim54922, align 8
%clofunc54923 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48476)
musttail call tailcc void %clofunc54923(%struct.ScmObj* %ae48476, %struct.ScmObj* %argslist54897$ae484762)
ret void
}

define tailcc void @proc_clo$ae48476(%struct.ScmObj* %env$ae48476,%struct.ScmObj* %current_45args54324) {
%stackaddr$env-ref54924 = alloca %struct.ScmObj*, align 8
%anf_45bind48151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48476, i64 0)
store %struct.ScmObj* %anf_45bind48151, %struct.ScmObj** %stackaddr$env-ref54924
%stackaddr$prim54925 = alloca %struct.ScmObj*, align 8
%_95k48276 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54324)
store volatile %struct.ScmObj* %_95k48276, %struct.ScmObj** %stackaddr$prim54925, align 8
%stackaddr$prim54926 = alloca %struct.ScmObj*, align 8
%current_45args54325 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54324)
store volatile %struct.ScmObj* %current_45args54325, %struct.ScmObj** %stackaddr$prim54926, align 8
%stackaddr$prim54927 = alloca %struct.ScmObj*, align 8
%anf_45bind48155 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54325)
store volatile %struct.ScmObj* %anf_45bind48155, %struct.ScmObj** %stackaddr$prim54927, align 8
%stackaddr$makeclosure54928 = alloca %struct.ScmObj*, align 8
%fptrToInt54929 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48591 to i64
%ae48591 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54929)
store volatile %struct.ScmObj* %ae48591, %struct.ScmObj** %stackaddr$makeclosure54928, align 8
%argslist54876$anf_45bind481510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54930 = alloca %struct.ScmObj*, align 8
%argslist54876$anf_45bind481511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48155, %struct.ScmObj* %argslist54876$anf_45bind481510)
store volatile %struct.ScmObj* %argslist54876$anf_45bind481511, %struct.ScmObj** %stackaddr$prim54930, align 8
%stackaddr$prim54931 = alloca %struct.ScmObj*, align 8
%argslist54876$anf_45bind481512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48591, %struct.ScmObj* %argslist54876$anf_45bind481511)
store volatile %struct.ScmObj* %argslist54876$anf_45bind481512, %struct.ScmObj** %stackaddr$prim54931, align 8
%clofunc54932 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48151)
musttail call tailcc void %clofunc54932(%struct.ScmObj* %anf_45bind48151, %struct.ScmObj* %argslist54876$anf_45bind481512)
ret void
}

define tailcc void @proc_clo$ae48591(%struct.ScmObj* %env$ae48591,%struct.ScmObj* %current_45args54327) {
%stackaddr$prim54933 = alloca %struct.ScmObj*, align 8
%_95k48277 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54327)
store volatile %struct.ScmObj* %_95k48277, %struct.ScmObj** %stackaddr$prim54933, align 8
%stackaddr$prim54934 = alloca %struct.ScmObj*, align 8
%current_45args54328 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54327)
store volatile %struct.ScmObj* %current_45args54328, %struct.ScmObj** %stackaddr$prim54934, align 8
%stackaddr$prim54935 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54328)
store volatile %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$prim54935, align 8
%stackaddr$makeclosure54936 = alloca %struct.ScmObj*, align 8
%fptrToInt54937 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48593 to i64
%ae48593 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54937)
store volatile %struct.ScmObj* %ae48593, %struct.ScmObj** %stackaddr$makeclosure54936, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48593, %struct.ScmObj* %Ycmb48025, i64 0)
%ae48594 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54938 = alloca %struct.ScmObj*, align 8
%fptrToInt54939 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48595 to i64
%ae48595 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54939)
store volatile %struct.ScmObj* %ae48595, %struct.ScmObj** %stackaddr$makeclosure54938, align 8
%argslist54875$ae485930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54940 = alloca %struct.ScmObj*, align 8
%argslist54875$ae485931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48595, %struct.ScmObj* %argslist54875$ae485930)
store volatile %struct.ScmObj* %argslist54875$ae485931, %struct.ScmObj** %stackaddr$prim54940, align 8
%stackaddr$prim54941 = alloca %struct.ScmObj*, align 8
%argslist54875$ae485932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48594, %struct.ScmObj* %argslist54875$ae485931)
store volatile %struct.ScmObj* %argslist54875$ae485932, %struct.ScmObj** %stackaddr$prim54941, align 8
%clofunc54942 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48593)
musttail call tailcc void %clofunc54942(%struct.ScmObj* %ae48593, %struct.ScmObj* %argslist54875$ae485932)
ret void
}

define tailcc void @proc_clo$ae48593(%struct.ScmObj* %env$ae48593,%struct.ScmObj* %current_45args54330) {
%stackaddr$env-ref54943 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48593, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54943
%stackaddr$prim54944 = alloca %struct.ScmObj*, align 8
%_95k48278 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54330)
store volatile %struct.ScmObj* %_95k48278, %struct.ScmObj** %stackaddr$prim54944, align 8
%stackaddr$prim54945 = alloca %struct.ScmObj*, align 8
%current_45args54331 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54330)
store volatile %struct.ScmObj* %current_45args54331, %struct.ScmObj** %stackaddr$prim54945, align 8
%stackaddr$prim54946 = alloca %struct.ScmObj*, align 8
%anf_45bind48160 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54331)
store volatile %struct.ScmObj* %anf_45bind48160, %struct.ScmObj** %stackaddr$prim54946, align 8
%stackaddr$makeclosure54947 = alloca %struct.ScmObj*, align 8
%fptrToInt54948 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48671 to i64
%ae48671 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54948)
store volatile %struct.ScmObj* %ae48671, %struct.ScmObj** %stackaddr$makeclosure54947, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48671, %struct.ScmObj* %Ycmb48025, i64 0)
%argslist54859$Ycmb480250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54949 = alloca %struct.ScmObj*, align 8
%argslist54859$Ycmb480251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48160, %struct.ScmObj* %argslist54859$Ycmb480250)
store volatile %struct.ScmObj* %argslist54859$Ycmb480251, %struct.ScmObj** %stackaddr$prim54949, align 8
%stackaddr$prim54950 = alloca %struct.ScmObj*, align 8
%argslist54859$Ycmb480252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48671, %struct.ScmObj* %argslist54859$Ycmb480251)
store volatile %struct.ScmObj* %argslist54859$Ycmb480252, %struct.ScmObj** %stackaddr$prim54950, align 8
%clofunc54951 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48025)
musttail call tailcc void %clofunc54951(%struct.ScmObj* %Ycmb48025, %struct.ScmObj* %argslist54859$Ycmb480252)
ret void
}

define tailcc void @proc_clo$ae48671(%struct.ScmObj* %env$ae48671,%struct.ScmObj* %current_45args54333) {
%stackaddr$env-ref54952 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48671, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54952
%stackaddr$prim54953 = alloca %struct.ScmObj*, align 8
%_95k48279 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54333)
store volatile %struct.ScmObj* %_95k48279, %struct.ScmObj** %stackaddr$prim54953, align 8
%stackaddr$prim54954 = alloca %struct.ScmObj*, align 8
%current_45args54334 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54333)
store volatile %struct.ScmObj* %current_45args54334, %struct.ScmObj** %stackaddr$prim54954, align 8
%stackaddr$prim54955 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54334)
store volatile %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$prim54955, align 8
%stackaddr$makeclosure54956 = alloca %struct.ScmObj*, align 8
%fptrToInt54957 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48673 to i64
%ae48673 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54957)
store volatile %struct.ScmObj* %ae48673, %struct.ScmObj** %stackaddr$makeclosure54956, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48673, %struct.ScmObj* %Ycmb48025, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48673, %struct.ScmObj* %_37foldr148046, i64 1)
%ae48674 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54958 = alloca %struct.ScmObj*, align 8
%fptrToInt54959 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48675 to i64
%ae48675 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54959)
store volatile %struct.ScmObj* %ae48675, %struct.ScmObj** %stackaddr$makeclosure54958, align 8
%argslist54858$ae486730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54960 = alloca %struct.ScmObj*, align 8
%argslist54858$ae486731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48675, %struct.ScmObj* %argslist54858$ae486730)
store volatile %struct.ScmObj* %argslist54858$ae486731, %struct.ScmObj** %stackaddr$prim54960, align 8
%stackaddr$prim54961 = alloca %struct.ScmObj*, align 8
%argslist54858$ae486732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48674, %struct.ScmObj* %argslist54858$ae486731)
store volatile %struct.ScmObj* %argslist54858$ae486732, %struct.ScmObj** %stackaddr$prim54961, align 8
%clofunc54962 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48673)
musttail call tailcc void %clofunc54962(%struct.ScmObj* %ae48673, %struct.ScmObj* %argslist54858$ae486732)
ret void
}

define tailcc void @proc_clo$ae48673(%struct.ScmObj* %env$ae48673,%struct.ScmObj* %current_45args54336) {
%stackaddr$env-ref54963 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48673, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54963
%stackaddr$env-ref54964 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48673, i64 1)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref54964
%stackaddr$prim54965 = alloca %struct.ScmObj*, align 8
%_95k48280 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54336)
store volatile %struct.ScmObj* %_95k48280, %struct.ScmObj** %stackaddr$prim54965, align 8
%stackaddr$prim54966 = alloca %struct.ScmObj*, align 8
%current_45args54337 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54336)
store volatile %struct.ScmObj* %current_45args54337, %struct.ScmObj** %stackaddr$prim54966, align 8
%stackaddr$prim54967 = alloca %struct.ScmObj*, align 8
%anf_45bind48166 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54337)
store volatile %struct.ScmObj* %anf_45bind48166, %struct.ScmObj** %stackaddr$prim54967, align 8
%stackaddr$makeclosure54968 = alloca %struct.ScmObj*, align 8
%fptrToInt54969 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48768 to i64
%ae48768 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54969)
store volatile %struct.ScmObj* %ae48768, %struct.ScmObj** %stackaddr$makeclosure54968, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48768, %struct.ScmObj* %Ycmb48025, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48768, %struct.ScmObj* %_37foldr148046, i64 1)
%argslist54839$Ycmb480250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54970 = alloca %struct.ScmObj*, align 8
%argslist54839$Ycmb480251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48166, %struct.ScmObj* %argslist54839$Ycmb480250)
store volatile %struct.ScmObj* %argslist54839$Ycmb480251, %struct.ScmObj** %stackaddr$prim54970, align 8
%stackaddr$prim54971 = alloca %struct.ScmObj*, align 8
%argslist54839$Ycmb480252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48768, %struct.ScmObj* %argslist54839$Ycmb480251)
store volatile %struct.ScmObj* %argslist54839$Ycmb480252, %struct.ScmObj** %stackaddr$prim54971, align 8
%clofunc54972 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48025)
musttail call tailcc void %clofunc54972(%struct.ScmObj* %Ycmb48025, %struct.ScmObj* %argslist54839$Ycmb480252)
ret void
}

define tailcc void @proc_clo$ae48768(%struct.ScmObj* %env$ae48768,%struct.ScmObj* %current_45args54339) {
%stackaddr$env-ref54973 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48768, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54973
%stackaddr$env-ref54974 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48768, i64 1)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref54974
%stackaddr$prim54975 = alloca %struct.ScmObj*, align 8
%_95k48281 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54339)
store volatile %struct.ScmObj* %_95k48281, %struct.ScmObj** %stackaddr$prim54975, align 8
%stackaddr$prim54976 = alloca %struct.ScmObj*, align 8
%current_45args54340 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54339)
store volatile %struct.ScmObj* %current_45args54340, %struct.ScmObj** %stackaddr$prim54976, align 8
%stackaddr$prim54977 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54340)
store volatile %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$prim54977, align 8
%stackaddr$makeclosure54978 = alloca %struct.ScmObj*, align 8
%fptrToInt54979 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48770 to i64
%ae48770 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54979)
store volatile %struct.ScmObj* %ae48770, %struct.ScmObj** %stackaddr$makeclosure54978, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48770, %struct.ScmObj* %Ycmb48025, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48770, %struct.ScmObj* %_37foldr148046, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48770, %struct.ScmObj* %_37map148042, i64 2)
%ae48771 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54980 = alloca %struct.ScmObj*, align 8
%fptrToInt54981 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48772 to i64
%ae48772 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54981)
store volatile %struct.ScmObj* %ae48772, %struct.ScmObj** %stackaddr$makeclosure54980, align 8
%argslist54838$ae487700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54982 = alloca %struct.ScmObj*, align 8
%argslist54838$ae487701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48772, %struct.ScmObj* %argslist54838$ae487700)
store volatile %struct.ScmObj* %argslist54838$ae487701, %struct.ScmObj** %stackaddr$prim54982, align 8
%stackaddr$prim54983 = alloca %struct.ScmObj*, align 8
%argslist54838$ae487702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48771, %struct.ScmObj* %argslist54838$ae487701)
store volatile %struct.ScmObj* %argslist54838$ae487702, %struct.ScmObj** %stackaddr$prim54983, align 8
%clofunc54984 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48770)
musttail call tailcc void %clofunc54984(%struct.ScmObj* %ae48770, %struct.ScmObj* %argslist54838$ae487702)
ret void
}

define tailcc void @proc_clo$ae48770(%struct.ScmObj* %env$ae48770,%struct.ScmObj* %current_45args54342) {
%stackaddr$env-ref54985 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48770, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54985
%stackaddr$env-ref54986 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48770, i64 1)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref54986
%stackaddr$env-ref54987 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48770, i64 2)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref54987
%stackaddr$prim54988 = alloca %struct.ScmObj*, align 8
%_95k48282 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54342)
store volatile %struct.ScmObj* %_95k48282, %struct.ScmObj** %stackaddr$prim54988, align 8
%stackaddr$prim54989 = alloca %struct.ScmObj*, align 8
%current_45args54343 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54342)
store volatile %struct.ScmObj* %current_45args54343, %struct.ScmObj** %stackaddr$prim54989, align 8
%stackaddr$prim54990 = alloca %struct.ScmObj*, align 8
%anf_45bind48173 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54343)
store volatile %struct.ScmObj* %anf_45bind48173, %struct.ScmObj** %stackaddr$prim54990, align 8
%stackaddr$makeclosure54991 = alloca %struct.ScmObj*, align 8
%fptrToInt54992 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48918 to i64
%ae48918 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54992)
store volatile %struct.ScmObj* %ae48918, %struct.ScmObj** %stackaddr$makeclosure54991, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48918, %struct.ScmObj* %Ycmb48025, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48918, %struct.ScmObj* %_37foldr148046, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48918, %struct.ScmObj* %_37map148042, i64 2)
%argslist54822$Ycmb480250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54993 = alloca %struct.ScmObj*, align 8
%argslist54822$Ycmb480251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48173, %struct.ScmObj* %argslist54822$Ycmb480250)
store volatile %struct.ScmObj* %argslist54822$Ycmb480251, %struct.ScmObj** %stackaddr$prim54993, align 8
%stackaddr$prim54994 = alloca %struct.ScmObj*, align 8
%argslist54822$Ycmb480252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48918, %struct.ScmObj* %argslist54822$Ycmb480251)
store volatile %struct.ScmObj* %argslist54822$Ycmb480252, %struct.ScmObj** %stackaddr$prim54994, align 8
%clofunc54995 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48025)
musttail call tailcc void %clofunc54995(%struct.ScmObj* %Ycmb48025, %struct.ScmObj* %argslist54822$Ycmb480252)
ret void
}

define tailcc void @proc_clo$ae48918(%struct.ScmObj* %env$ae48918,%struct.ScmObj* %current_45args54345) {
%stackaddr$env-ref54996 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48918, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54996
%stackaddr$env-ref54997 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48918, i64 1)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref54997
%stackaddr$env-ref54998 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48918, i64 2)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref54998
%stackaddr$prim54999 = alloca %struct.ScmObj*, align 8
%_95k48283 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54345)
store volatile %struct.ScmObj* %_95k48283, %struct.ScmObj** %stackaddr$prim54999, align 8
%stackaddr$prim55000 = alloca %struct.ScmObj*, align 8
%current_45args54346 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54345)
store volatile %struct.ScmObj* %current_45args54346, %struct.ScmObj** %stackaddr$prim55000, align 8
%stackaddr$prim55001 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54346)
store volatile %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$prim55001, align 8
%stackaddr$makeclosure55002 = alloca %struct.ScmObj*, align 8
%fptrToInt55003 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48920 to i64
%ae48920 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55003)
store volatile %struct.ScmObj* %ae48920, %struct.ScmObj** %stackaddr$makeclosure55002, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48920, %struct.ScmObj* %Ycmb48025, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48920, %struct.ScmObj* %_37take48038, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48920, %struct.ScmObj* %_37foldr148046, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48920, %struct.ScmObj* %_37map148042, i64 3)
%ae48921 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55004 = alloca %struct.ScmObj*, align 8
%fptrToInt55005 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48922 to i64
%ae48922 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55005)
store volatile %struct.ScmObj* %ae48922, %struct.ScmObj** %stackaddr$makeclosure55004, align 8
%argslist54821$ae489200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55006 = alloca %struct.ScmObj*, align 8
%argslist54821$ae489201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48922, %struct.ScmObj* %argslist54821$ae489200)
store volatile %struct.ScmObj* %argslist54821$ae489201, %struct.ScmObj** %stackaddr$prim55006, align 8
%stackaddr$prim55007 = alloca %struct.ScmObj*, align 8
%argslist54821$ae489202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48921, %struct.ScmObj* %argslist54821$ae489201)
store volatile %struct.ScmObj* %argslist54821$ae489202, %struct.ScmObj** %stackaddr$prim55007, align 8
%clofunc55008 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48920)
musttail call tailcc void %clofunc55008(%struct.ScmObj* %ae48920, %struct.ScmObj* %argslist54821$ae489202)
ret void
}

define tailcc void @proc_clo$ae48920(%struct.ScmObj* %env$ae48920,%struct.ScmObj* %current_45args54348) {
%stackaddr$env-ref55009 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48920, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref55009
%stackaddr$env-ref55010 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48920, i64 1)
store %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$env-ref55010
%stackaddr$env-ref55011 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48920, i64 2)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55011
%stackaddr$env-ref55012 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48920, i64 3)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref55012
%stackaddr$prim55013 = alloca %struct.ScmObj*, align 8
%_95k48284 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54348)
store volatile %struct.ScmObj* %_95k48284, %struct.ScmObj** %stackaddr$prim55013, align 8
%stackaddr$prim55014 = alloca %struct.ScmObj*, align 8
%current_45args54349 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54348)
store volatile %struct.ScmObj* %current_45args54349, %struct.ScmObj** %stackaddr$prim55014, align 8
%stackaddr$prim55015 = alloca %struct.ScmObj*, align 8
%anf_45bind48177 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54349)
store volatile %struct.ScmObj* %anf_45bind48177, %struct.ScmObj** %stackaddr$prim55015, align 8
%stackaddr$makeclosure55016 = alloca %struct.ScmObj*, align 8
%fptrToInt55017 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49001 to i64
%ae49001 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55017)
store volatile %struct.ScmObj* %ae49001, %struct.ScmObj** %stackaddr$makeclosure55016, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49001, %struct.ScmObj* %Ycmb48025, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49001, %struct.ScmObj* %_37take48038, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49001, %struct.ScmObj* %_37foldr148046, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49001, %struct.ScmObj* %_37map148042, i64 3)
%argslist54807$Ycmb480250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55018 = alloca %struct.ScmObj*, align 8
%argslist54807$Ycmb480251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48177, %struct.ScmObj* %argslist54807$Ycmb480250)
store volatile %struct.ScmObj* %argslist54807$Ycmb480251, %struct.ScmObj** %stackaddr$prim55018, align 8
%stackaddr$prim55019 = alloca %struct.ScmObj*, align 8
%argslist54807$Ycmb480252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49001, %struct.ScmObj* %argslist54807$Ycmb480251)
store volatile %struct.ScmObj* %argslist54807$Ycmb480252, %struct.ScmObj** %stackaddr$prim55019, align 8
%clofunc55020 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48025)
musttail call tailcc void %clofunc55020(%struct.ScmObj* %Ycmb48025, %struct.ScmObj* %argslist54807$Ycmb480252)
ret void
}

define tailcc void @proc_clo$ae49001(%struct.ScmObj* %env$ae49001,%struct.ScmObj* %current_45args54351) {
%stackaddr$env-ref55021 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49001, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref55021
%stackaddr$env-ref55022 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49001, i64 1)
store %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$env-ref55022
%stackaddr$env-ref55023 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49001, i64 2)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55023
%stackaddr$env-ref55024 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49001, i64 3)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref55024
%stackaddr$prim55025 = alloca %struct.ScmObj*, align 8
%_95k48285 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54351)
store volatile %struct.ScmObj* %_95k48285, %struct.ScmObj** %stackaddr$prim55025, align 8
%stackaddr$prim55026 = alloca %struct.ScmObj*, align 8
%current_45args54352 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54351)
store volatile %struct.ScmObj* %current_45args54352, %struct.ScmObj** %stackaddr$prim55026, align 8
%stackaddr$prim55027 = alloca %struct.ScmObj*, align 8
%_37length48035 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54352)
store volatile %struct.ScmObj* %_37length48035, %struct.ScmObj** %stackaddr$prim55027, align 8
%stackaddr$makeclosure55028 = alloca %struct.ScmObj*, align 8
%fptrToInt55029 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49003 to i64
%ae49003 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55029)
store volatile %struct.ScmObj* %ae49003, %struct.ScmObj** %stackaddr$makeclosure55028, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49003, %struct.ScmObj* %Ycmb48025, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49003, %struct.ScmObj* %_37take48038, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49003, %struct.ScmObj* %_37length48035, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49003, %struct.ScmObj* %_37foldr148046, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49003, %struct.ScmObj* %_37map148042, i64 4)
%ae49004 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55030 = alloca %struct.ScmObj*, align 8
%fptrToInt55031 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49005 to i64
%ae49005 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55031)
store volatile %struct.ScmObj* %ae49005, %struct.ScmObj** %stackaddr$makeclosure55030, align 8
%argslist54806$ae490030 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55032 = alloca %struct.ScmObj*, align 8
%argslist54806$ae490031 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49005, %struct.ScmObj* %argslist54806$ae490030)
store volatile %struct.ScmObj* %argslist54806$ae490031, %struct.ScmObj** %stackaddr$prim55032, align 8
%stackaddr$prim55033 = alloca %struct.ScmObj*, align 8
%argslist54806$ae490032 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49004, %struct.ScmObj* %argslist54806$ae490031)
store volatile %struct.ScmObj* %argslist54806$ae490032, %struct.ScmObj** %stackaddr$prim55033, align 8
%clofunc55034 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49003)
musttail call tailcc void %clofunc55034(%struct.ScmObj* %ae49003, %struct.ScmObj* %argslist54806$ae490032)
ret void
}

define tailcc void @proc_clo$ae49003(%struct.ScmObj* %env$ae49003,%struct.ScmObj* %current_45args54354) {
%stackaddr$env-ref55035 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49003, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref55035
%stackaddr$env-ref55036 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49003, i64 1)
store %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$env-ref55036
%stackaddr$env-ref55037 = alloca %struct.ScmObj*, align 8
%_37length48035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49003, i64 2)
store %struct.ScmObj* %_37length48035, %struct.ScmObj** %stackaddr$env-ref55037
%stackaddr$env-ref55038 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49003, i64 3)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55038
%stackaddr$env-ref55039 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49003, i64 4)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref55039
%stackaddr$prim55040 = alloca %struct.ScmObj*, align 8
%_95k48286 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54354)
store volatile %struct.ScmObj* %_95k48286, %struct.ScmObj** %stackaddr$prim55040, align 8
%stackaddr$prim55041 = alloca %struct.ScmObj*, align 8
%current_45args54355 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54354)
store volatile %struct.ScmObj* %current_45args54355, %struct.ScmObj** %stackaddr$prim55041, align 8
%stackaddr$prim55042 = alloca %struct.ScmObj*, align 8
%anf_45bind48182 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54355)
store volatile %struct.ScmObj* %anf_45bind48182, %struct.ScmObj** %stackaddr$prim55042, align 8
%stackaddr$makeclosure55043 = alloca %struct.ScmObj*, align 8
%fptrToInt55044 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49080 to i64
%ae49080 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55044)
store volatile %struct.ScmObj* %ae49080, %struct.ScmObj** %stackaddr$makeclosure55043, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49080, %struct.ScmObj* %Ycmb48025, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49080, %struct.ScmObj* %_37take48038, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49080, %struct.ScmObj* %_37length48035, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49080, %struct.ScmObj* %_37foldr148046, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49080, %struct.ScmObj* %_37map148042, i64 4)
%argslist54790$Ycmb480250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55045 = alloca %struct.ScmObj*, align 8
%argslist54790$Ycmb480251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48182, %struct.ScmObj* %argslist54790$Ycmb480250)
store volatile %struct.ScmObj* %argslist54790$Ycmb480251, %struct.ScmObj** %stackaddr$prim55045, align 8
%stackaddr$prim55046 = alloca %struct.ScmObj*, align 8
%argslist54790$Ycmb480252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49080, %struct.ScmObj* %argslist54790$Ycmb480251)
store volatile %struct.ScmObj* %argslist54790$Ycmb480252, %struct.ScmObj** %stackaddr$prim55046, align 8
%clofunc55047 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48025)
musttail call tailcc void %clofunc55047(%struct.ScmObj* %Ycmb48025, %struct.ScmObj* %argslist54790$Ycmb480252)
ret void
}

define tailcc void @proc_clo$ae49080(%struct.ScmObj* %env$ae49080,%struct.ScmObj* %current_45args54357) {
%stackaddr$env-ref55048 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49080, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref55048
%stackaddr$env-ref55049 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49080, i64 1)
store %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$env-ref55049
%stackaddr$env-ref55050 = alloca %struct.ScmObj*, align 8
%_37length48035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49080, i64 2)
store %struct.ScmObj* %_37length48035, %struct.ScmObj** %stackaddr$env-ref55050
%stackaddr$env-ref55051 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49080, i64 3)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55051
%stackaddr$env-ref55052 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49080, i64 4)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref55052
%stackaddr$prim55053 = alloca %struct.ScmObj*, align 8
%_95k48287 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54357)
store volatile %struct.ScmObj* %_95k48287, %struct.ScmObj** %stackaddr$prim55053, align 8
%stackaddr$prim55054 = alloca %struct.ScmObj*, align 8
%current_45args54358 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54357)
store volatile %struct.ScmObj* %current_45args54358, %struct.ScmObj** %stackaddr$prim55054, align 8
%stackaddr$prim55055 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54358)
store volatile %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$prim55055, align 8
%stackaddr$makeclosure55056 = alloca %struct.ScmObj*, align 8
%fptrToInt55057 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49082 to i64
%ae49082 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt55057)
store volatile %struct.ScmObj* %ae49082, %struct.ScmObj** %stackaddr$makeclosure55056, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49082, %struct.ScmObj* %_37foldr148046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49082, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49082, %struct.ScmObj* %Ycmb48025, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49082, %struct.ScmObj* %_37take48038, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49082, %struct.ScmObj* %_37length48035, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49082, %struct.ScmObj* %_37map148042, i64 5)
%ae49083 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55058 = alloca %struct.ScmObj*, align 8
%fptrToInt55059 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49084 to i64
%ae49084 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55059)
store volatile %struct.ScmObj* %ae49084, %struct.ScmObj** %stackaddr$makeclosure55058, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49084, %struct.ScmObj* %_37foldl148030, i64 0)
%argslist54789$ae490820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55060 = alloca %struct.ScmObj*, align 8
%argslist54789$ae490821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49084, %struct.ScmObj* %argslist54789$ae490820)
store volatile %struct.ScmObj* %argslist54789$ae490821, %struct.ScmObj** %stackaddr$prim55060, align 8
%stackaddr$prim55061 = alloca %struct.ScmObj*, align 8
%argslist54789$ae490822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49083, %struct.ScmObj* %argslist54789$ae490821)
store volatile %struct.ScmObj* %argslist54789$ae490822, %struct.ScmObj** %stackaddr$prim55061, align 8
%clofunc55062 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49082)
musttail call tailcc void %clofunc55062(%struct.ScmObj* %ae49082, %struct.ScmObj* %argslist54789$ae490822)
ret void
}

define tailcc void @proc_clo$ae49082(%struct.ScmObj* %env$ae49082,%struct.ScmObj* %current_45args54360) {
%stackaddr$env-ref55063 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49082, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55063
%stackaddr$env-ref55064 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49082, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55064
%stackaddr$env-ref55065 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49082, i64 2)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref55065
%stackaddr$env-ref55066 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49082, i64 3)
store %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$env-ref55066
%stackaddr$env-ref55067 = alloca %struct.ScmObj*, align 8
%_37length48035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49082, i64 4)
store %struct.ScmObj* %_37length48035, %struct.ScmObj** %stackaddr$env-ref55067
%stackaddr$env-ref55068 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49082, i64 5)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref55068
%stackaddr$prim55069 = alloca %struct.ScmObj*, align 8
%_95k48288 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54360)
store volatile %struct.ScmObj* %_95k48288, %struct.ScmObj** %stackaddr$prim55069, align 8
%stackaddr$prim55070 = alloca %struct.ScmObj*, align 8
%current_45args54361 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54360)
store volatile %struct.ScmObj* %current_45args54361, %struct.ScmObj** %stackaddr$prim55070, align 8
%stackaddr$prim55071 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54361)
store volatile %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$prim55071, align 8
%stackaddr$makeclosure55072 = alloca %struct.ScmObj*, align 8
%fptrToInt55073 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49136 to i64
%ae49136 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55073)
store volatile %struct.ScmObj* %ae49136, %struct.ScmObj** %stackaddr$makeclosure55072, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49136, %struct.ScmObj* %_37foldr148046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49136, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49136, %struct.ScmObj* %Ycmb48025, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49136, %struct.ScmObj* %_37last48068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49136, %struct.ScmObj* %_37map148042, i64 4)
%ae49137 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55074 = alloca %struct.ScmObj*, align 8
%fptrToInt55075 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49138 to i64
%ae49138 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55075)
store volatile %struct.ScmObj* %ae49138, %struct.ScmObj** %stackaddr$makeclosure55074, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49138, %struct.ScmObj* %_37take48038, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49138, %struct.ScmObj* %_37length48035, i64 1)
%argslist54775$ae491360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55076 = alloca %struct.ScmObj*, align 8
%argslist54775$ae491361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49138, %struct.ScmObj* %argslist54775$ae491360)
store volatile %struct.ScmObj* %argslist54775$ae491361, %struct.ScmObj** %stackaddr$prim55076, align 8
%stackaddr$prim55077 = alloca %struct.ScmObj*, align 8
%argslist54775$ae491362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49137, %struct.ScmObj* %argslist54775$ae491361)
store volatile %struct.ScmObj* %argslist54775$ae491362, %struct.ScmObj** %stackaddr$prim55077, align 8
%clofunc55078 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49136)
musttail call tailcc void %clofunc55078(%struct.ScmObj* %ae49136, %struct.ScmObj* %argslist54775$ae491362)
ret void
}

define tailcc void @proc_clo$ae49136(%struct.ScmObj* %env$ae49136,%struct.ScmObj* %current_45args54363) {
%stackaddr$env-ref55079 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49136, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55079
%stackaddr$env-ref55080 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49136, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55080
%stackaddr$env-ref55081 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49136, i64 2)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref55081
%stackaddr$env-ref55082 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49136, i64 3)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref55082
%stackaddr$env-ref55083 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49136, i64 4)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref55083
%stackaddr$prim55084 = alloca %struct.ScmObj*, align 8
%_95k48289 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54363)
store volatile %struct.ScmObj* %_95k48289, %struct.ScmObj** %stackaddr$prim55084, align 8
%stackaddr$prim55085 = alloca %struct.ScmObj*, align 8
%current_45args54364 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54363)
store volatile %struct.ScmObj* %current_45args54364, %struct.ScmObj** %stackaddr$prim55085, align 8
%stackaddr$prim55086 = alloca %struct.ScmObj*, align 8
%_37drop_45right48065 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54364)
store volatile %struct.ScmObj* %_37drop_45right48065, %struct.ScmObj** %stackaddr$prim55086, align 8
%stackaddr$makeclosure55087 = alloca %struct.ScmObj*, align 8
%fptrToInt55088 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49166 to i64
%ae49166 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55088)
store volatile %struct.ScmObj* %ae49166, %struct.ScmObj** %stackaddr$makeclosure55087, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49166, %struct.ScmObj* %_37foldr148046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49166, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49166, %struct.ScmObj* %Ycmb48025, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49166, %struct.ScmObj* %_37last48068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49166, %struct.ScmObj* %_37drop_45right48065, i64 4)
%ae49167 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55089 = alloca %struct.ScmObj*, align 8
%fptrToInt55090 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49168 to i64
%ae49168 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55090)
store volatile %struct.ScmObj* %ae49168, %struct.ScmObj** %stackaddr$makeclosure55089, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49168, %struct.ScmObj* %_37foldr148046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49168, %struct.ScmObj* %_37map148042, i64 1)
%argslist54765$ae491660 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55091 = alloca %struct.ScmObj*, align 8
%argslist54765$ae491661 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49168, %struct.ScmObj* %argslist54765$ae491660)
store volatile %struct.ScmObj* %argslist54765$ae491661, %struct.ScmObj** %stackaddr$prim55091, align 8
%stackaddr$prim55092 = alloca %struct.ScmObj*, align 8
%argslist54765$ae491662 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49167, %struct.ScmObj* %argslist54765$ae491661)
store volatile %struct.ScmObj* %argslist54765$ae491662, %struct.ScmObj** %stackaddr$prim55092, align 8
%clofunc55093 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49166)
musttail call tailcc void %clofunc55093(%struct.ScmObj* %ae49166, %struct.ScmObj* %argslist54765$ae491662)
ret void
}

define tailcc void @proc_clo$ae49166(%struct.ScmObj* %env$ae49166,%struct.ScmObj* %current_45args54366) {
%stackaddr$env-ref55094 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49166, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55094
%stackaddr$env-ref55095 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49166, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55095
%stackaddr$env-ref55096 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49166, i64 2)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref55096
%stackaddr$env-ref55097 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49166, i64 3)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref55097
%stackaddr$env-ref55098 = alloca %struct.ScmObj*, align 8
%_37drop_45right48065 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49166, i64 4)
store %struct.ScmObj* %_37drop_45right48065, %struct.ScmObj** %stackaddr$env-ref55098
%stackaddr$prim55099 = alloca %struct.ScmObj*, align 8
%_95k48290 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54366)
store volatile %struct.ScmObj* %_95k48290, %struct.ScmObj** %stackaddr$prim55099, align 8
%stackaddr$prim55100 = alloca %struct.ScmObj*, align 8
%current_45args54367 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54366)
store volatile %struct.ScmObj* %current_45args54367, %struct.ScmObj** %stackaddr$prim55100, align 8
%stackaddr$prim55101 = alloca %struct.ScmObj*, align 8
%anf_45bind48198 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54367)
store volatile %struct.ScmObj* %anf_45bind48198, %struct.ScmObj** %stackaddr$prim55101, align 8
%stackaddr$makeclosure55102 = alloca %struct.ScmObj*, align 8
%fptrToInt55103 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49550 to i64
%ae49550 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55103)
store volatile %struct.ScmObj* %ae49550, %struct.ScmObj** %stackaddr$makeclosure55102, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49550, %struct.ScmObj* %_37foldr148046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49550, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49550, %struct.ScmObj* %Ycmb48025, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49550, %struct.ScmObj* %_37last48068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49550, %struct.ScmObj* %_37drop_45right48065, i64 4)
%argslist54705$Ycmb480250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55104 = alloca %struct.ScmObj*, align 8
%argslist54705$Ycmb480251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48198, %struct.ScmObj* %argslist54705$Ycmb480250)
store volatile %struct.ScmObj* %argslist54705$Ycmb480251, %struct.ScmObj** %stackaddr$prim55104, align 8
%stackaddr$prim55105 = alloca %struct.ScmObj*, align 8
%argslist54705$Ycmb480252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49550, %struct.ScmObj* %argslist54705$Ycmb480251)
store volatile %struct.ScmObj* %argslist54705$Ycmb480252, %struct.ScmObj** %stackaddr$prim55105, align 8
%clofunc55106 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48025)
musttail call tailcc void %clofunc55106(%struct.ScmObj* %Ycmb48025, %struct.ScmObj* %argslist54705$Ycmb480252)
ret void
}

define tailcc void @proc_clo$ae49550(%struct.ScmObj* %env$ae49550,%struct.ScmObj* %current_45args54369) {
%stackaddr$env-ref55107 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49550, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55107
%stackaddr$env-ref55108 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49550, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55108
%stackaddr$env-ref55109 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49550, i64 2)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref55109
%stackaddr$env-ref55110 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49550, i64 3)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref55110
%stackaddr$env-ref55111 = alloca %struct.ScmObj*, align 8
%_37drop_45right48065 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49550, i64 4)
store %struct.ScmObj* %_37drop_45right48065, %struct.ScmObj** %stackaddr$env-ref55111
%stackaddr$prim55112 = alloca %struct.ScmObj*, align 8
%_95k48291 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54369)
store volatile %struct.ScmObj* %_95k48291, %struct.ScmObj** %stackaddr$prim55112, align 8
%stackaddr$prim55113 = alloca %struct.ScmObj*, align 8
%current_45args54370 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54369)
store volatile %struct.ScmObj* %current_45args54370, %struct.ScmObj** %stackaddr$prim55113, align 8
%stackaddr$prim55114 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54370)
store volatile %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$prim55114, align 8
%stackaddr$makeclosure55115 = alloca %struct.ScmObj*, align 8
%fptrToInt55116 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49552 to i64
%ae49552 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt55116)
store volatile %struct.ScmObj* %ae49552, %struct.ScmObj** %stackaddr$makeclosure55115, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49552, %struct.ScmObj* %_37foldr148046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49552, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49552, %struct.ScmObj* %Ycmb48025, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49552, %struct.ScmObj* %_37last48068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49552, %struct.ScmObj* %_37foldr48051, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49552, %struct.ScmObj* %_37drop_45right48065, i64 5)
%ae49553 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55117 = alloca %struct.ScmObj*, align 8
%fptrToInt55118 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49554 to i64
%ae49554 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55118)
store volatile %struct.ScmObj* %ae49554, %struct.ScmObj** %stackaddr$makeclosure55117, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49554, %struct.ScmObj* %_37foldr148046, i64 0)
%argslist54704$ae495520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55119 = alloca %struct.ScmObj*, align 8
%argslist54704$ae495521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49554, %struct.ScmObj* %argslist54704$ae495520)
store volatile %struct.ScmObj* %argslist54704$ae495521, %struct.ScmObj** %stackaddr$prim55119, align 8
%stackaddr$prim55120 = alloca %struct.ScmObj*, align 8
%argslist54704$ae495522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49553, %struct.ScmObj* %argslist54704$ae495521)
store volatile %struct.ScmObj* %argslist54704$ae495522, %struct.ScmObj** %stackaddr$prim55120, align 8
%clofunc55121 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49552)
musttail call tailcc void %clofunc55121(%struct.ScmObj* %ae49552, %struct.ScmObj* %argslist54704$ae495522)
ret void
}

define tailcc void @proc_clo$ae49552(%struct.ScmObj* %env$ae49552,%struct.ScmObj* %current_45args54372) {
%stackaddr$env-ref55122 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49552, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55122
%stackaddr$env-ref55123 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49552, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55123
%stackaddr$env-ref55124 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49552, i64 2)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref55124
%stackaddr$env-ref55125 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49552, i64 3)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref55125
%stackaddr$env-ref55126 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49552, i64 4)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55126
%stackaddr$env-ref55127 = alloca %struct.ScmObj*, align 8
%_37drop_45right48065 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49552, i64 5)
store %struct.ScmObj* %_37drop_45right48065, %struct.ScmObj** %stackaddr$env-ref55127
%stackaddr$prim55128 = alloca %struct.ScmObj*, align 8
%_95k48292 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54372)
store volatile %struct.ScmObj* %_95k48292, %struct.ScmObj** %stackaddr$prim55128, align 8
%stackaddr$prim55129 = alloca %struct.ScmObj*, align 8
%current_45args54373 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54372)
store volatile %struct.ScmObj* %current_45args54373, %struct.ScmObj** %stackaddr$prim55129, align 8
%stackaddr$prim55130 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54373)
store volatile %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$prim55130, align 8
%stackaddr$makeclosure55131 = alloca %struct.ScmObj*, align 8
%fptrToInt55132 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49629 to i64
%ae49629 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55132)
store volatile %struct.ScmObj* %ae49629, %struct.ScmObj** %stackaddr$makeclosure55131, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49629, %struct.ScmObj* %_37foldr148046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49629, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49629, %struct.ScmObj* %Ycmb48025, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49629, %struct.ScmObj* %_37foldr48051, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49629, %struct.ScmObj* %_37map148077, i64 4)
%ae49630 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55133 = alloca %struct.ScmObj*, align 8
%fptrToInt55134 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49631 to i64
%ae49631 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55134)
store volatile %struct.ScmObj* %ae49631, %struct.ScmObj** %stackaddr$makeclosure55133, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49631, %struct.ScmObj* %_37last48068, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49631, %struct.ScmObj* %_37foldr48051, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49631, %struct.ScmObj* %_37drop_45right48065, i64 2)
%argslist54685$ae496290 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55135 = alloca %struct.ScmObj*, align 8
%argslist54685$ae496291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49631, %struct.ScmObj* %argslist54685$ae496290)
store volatile %struct.ScmObj* %argslist54685$ae496291, %struct.ScmObj** %stackaddr$prim55135, align 8
%stackaddr$prim55136 = alloca %struct.ScmObj*, align 8
%argslist54685$ae496292 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49630, %struct.ScmObj* %argslist54685$ae496291)
store volatile %struct.ScmObj* %argslist54685$ae496292, %struct.ScmObj** %stackaddr$prim55136, align 8
%clofunc55137 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49629)
musttail call tailcc void %clofunc55137(%struct.ScmObj* %ae49629, %struct.ScmObj* %argslist54685$ae496292)
ret void
}

define tailcc void @proc_clo$ae49629(%struct.ScmObj* %env$ae49629,%struct.ScmObj* %current_45args54375) {
%stackaddr$env-ref55138 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49629, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55138
%stackaddr$env-ref55139 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49629, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55139
%stackaddr$env-ref55140 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49629, i64 2)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref55140
%stackaddr$env-ref55141 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49629, i64 3)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55141
%stackaddr$env-ref55142 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49629, i64 4)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55142
%stackaddr$prim55143 = alloca %struct.ScmObj*, align 8
%_95k48293 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54375)
store volatile %struct.ScmObj* %_95k48293, %struct.ScmObj** %stackaddr$prim55143, align 8
%stackaddr$prim55144 = alloca %struct.ScmObj*, align 8
%current_45args54376 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54375)
store volatile %struct.ScmObj* %current_45args54376, %struct.ScmObj** %stackaddr$prim55144, align 8
%stackaddr$prim55145 = alloca %struct.ScmObj*, align 8
%_37map48072 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54376)
store volatile %struct.ScmObj* %_37map48072, %struct.ScmObj** %stackaddr$prim55145, align 8
%stackaddr$makeclosure55146 = alloca %struct.ScmObj*, align 8
%fptrToInt55147 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49775 to i64
%ae49775 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55147)
store volatile %struct.ScmObj* %ae49775, %struct.ScmObj** %stackaddr$makeclosure55146, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49775, %struct.ScmObj* %Ycmb48025, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49775, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49775, %struct.ScmObj* %_37map148077, i64 2)
%ae49776 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55148 = alloca %struct.ScmObj*, align 8
%fptrToInt55149 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49777 to i64
%ae49777 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55149)
store volatile %struct.ScmObj* %ae49777, %struct.ScmObj** %stackaddr$makeclosure55148, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49777, %struct.ScmObj* %_37foldr48051, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49777, %struct.ScmObj* %_37foldr148046, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49777, %struct.ScmObj* %_37map148077, i64 2)
%argslist54668$ae497750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55150 = alloca %struct.ScmObj*, align 8
%argslist54668$ae497751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49777, %struct.ScmObj* %argslist54668$ae497750)
store volatile %struct.ScmObj* %argslist54668$ae497751, %struct.ScmObj** %stackaddr$prim55150, align 8
%stackaddr$prim55151 = alloca %struct.ScmObj*, align 8
%argslist54668$ae497752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49776, %struct.ScmObj* %argslist54668$ae497751)
store volatile %struct.ScmObj* %argslist54668$ae497752, %struct.ScmObj** %stackaddr$prim55151, align 8
%clofunc55152 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49775)
musttail call tailcc void %clofunc55152(%struct.ScmObj* %ae49775, %struct.ScmObj* %argslist54668$ae497752)
ret void
}

define tailcc void @proc_clo$ae49775(%struct.ScmObj* %env$ae49775,%struct.ScmObj* %current_45args54378) {
%stackaddr$env-ref55153 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49775, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref55153
%stackaddr$env-ref55154 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49775, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55154
%stackaddr$env-ref55155 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49775, i64 2)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55155
%stackaddr$prim55156 = alloca %struct.ScmObj*, align 8
%_95k48294 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54378)
store volatile %struct.ScmObj* %_95k48294, %struct.ScmObj** %stackaddr$prim55156, align 8
%stackaddr$prim55157 = alloca %struct.ScmObj*, align 8
%current_45args54379 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54378)
store volatile %struct.ScmObj* %current_45args54379, %struct.ScmObj** %stackaddr$prim55157, align 8
%stackaddr$prim55158 = alloca %struct.ScmObj*, align 8
%anf_45bind48218 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54379)
store volatile %struct.ScmObj* %anf_45bind48218, %struct.ScmObj** %stackaddr$prim55158, align 8
%stackaddr$makeclosure55159 = alloca %struct.ScmObj*, align 8
%fptrToInt55160 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50167 to i64
%ae50167 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55160)
store volatile %struct.ScmObj* %ae50167, %struct.ScmObj** %stackaddr$makeclosure55159, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50167, %struct.ScmObj* %_37foldl148030, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50167, %struct.ScmObj* %_37map148077, i64 1)
%argslist54608$Ycmb480250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55161 = alloca %struct.ScmObj*, align 8
%argslist54608$Ycmb480251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48218, %struct.ScmObj* %argslist54608$Ycmb480250)
store volatile %struct.ScmObj* %argslist54608$Ycmb480251, %struct.ScmObj** %stackaddr$prim55161, align 8
%stackaddr$prim55162 = alloca %struct.ScmObj*, align 8
%argslist54608$Ycmb480252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50167, %struct.ScmObj* %argslist54608$Ycmb480251)
store volatile %struct.ScmObj* %argslist54608$Ycmb480252, %struct.ScmObj** %stackaddr$prim55162, align 8
%clofunc55163 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48025)
musttail call tailcc void %clofunc55163(%struct.ScmObj* %Ycmb48025, %struct.ScmObj* %argslist54608$Ycmb480252)
ret void
}

define tailcc void @proc_clo$ae50167(%struct.ScmObj* %env$ae50167,%struct.ScmObj* %current_45args54381) {
%stackaddr$env-ref55164 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50167, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55164
%stackaddr$env-ref55165 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50167, i64 1)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55165
%stackaddr$prim55166 = alloca %struct.ScmObj*, align 8
%_95k48295 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54381)
store volatile %struct.ScmObj* %_95k48295, %struct.ScmObj** %stackaddr$prim55166, align 8
%stackaddr$prim55167 = alloca %struct.ScmObj*, align 8
%current_45args54382 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54381)
store volatile %struct.ScmObj* %current_45args54382, %struct.ScmObj** %stackaddr$prim55167, align 8
%stackaddr$prim55168 = alloca %struct.ScmObj*, align 8
%_37foldl48128 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54382)
store volatile %struct.ScmObj* %_37foldl48128, %struct.ScmObj** %stackaddr$prim55168, align 8
%stackaddr$makeclosure55169 = alloca %struct.ScmObj*, align 8
%fptrToInt55170 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50169 to i64
%ae50169 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55170)
store volatile %struct.ScmObj* %ae50169, %struct.ScmObj** %stackaddr$makeclosure55169, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50169, %struct.ScmObj* %_37foldl148030, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50169, %struct.ScmObj* %_37map148077, i64 1)
%ae50170 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55171 = alloca %struct.ScmObj*, align 8
%fptrToInt55172 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50171 to i64
%ae50171 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55172)
store volatile %struct.ScmObj* %ae50171, %struct.ScmObj** %stackaddr$makeclosure55171, align 8
%argslist54607$ae501690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55173 = alloca %struct.ScmObj*, align 8
%argslist54607$ae501691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50171, %struct.ScmObj* %argslist54607$ae501690)
store volatile %struct.ScmObj* %argslist54607$ae501691, %struct.ScmObj** %stackaddr$prim55173, align 8
%stackaddr$prim55174 = alloca %struct.ScmObj*, align 8
%argslist54607$ae501692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50170, %struct.ScmObj* %argslist54607$ae501691)
store volatile %struct.ScmObj* %argslist54607$ae501692, %struct.ScmObj** %stackaddr$prim55174, align 8
%clofunc55175 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50169)
musttail call tailcc void %clofunc55175(%struct.ScmObj* %ae50169, %struct.ScmObj* %argslist54607$ae501692)
ret void
}

define tailcc void @proc_clo$ae50169(%struct.ScmObj* %env$ae50169,%struct.ScmObj* %current_45args54384) {
%stackaddr$env-ref55176 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50169, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55176
%stackaddr$env-ref55177 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50169, i64 1)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55177
%stackaddr$prim55178 = alloca %struct.ScmObj*, align 8
%_95k48296 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54384)
store volatile %struct.ScmObj* %_95k48296, %struct.ScmObj** %stackaddr$prim55178, align 8
%stackaddr$prim55179 = alloca %struct.ScmObj*, align 8
%current_45args54385 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54384)
store volatile %struct.ScmObj* %current_45args54385, %struct.ScmObj** %stackaddr$prim55179, align 8
%stackaddr$prim55180 = alloca %struct.ScmObj*, align 8
%_37_6248125 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54385)
store volatile %struct.ScmObj* %_37_6248125, %struct.ScmObj** %stackaddr$prim55180, align 8
%stackaddr$makeclosure55181 = alloca %struct.ScmObj*, align 8
%fptrToInt55182 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50193 to i64
%ae50193 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55182)
store volatile %struct.ScmObj* %ae50193, %struct.ScmObj** %stackaddr$makeclosure55181, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50193, %struct.ScmObj* %_37foldl148030, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50193, %struct.ScmObj* %_37map148077, i64 1)
%ae50194 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55183 = alloca %struct.ScmObj*, align 8
%fptrToInt55184 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50195 to i64
%ae50195 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55184)
store volatile %struct.ScmObj* %ae50195, %struct.ScmObj** %stackaddr$makeclosure55183, align 8
%argslist54601$ae501930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55185 = alloca %struct.ScmObj*, align 8
%argslist54601$ae501931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50195, %struct.ScmObj* %argslist54601$ae501930)
store volatile %struct.ScmObj* %argslist54601$ae501931, %struct.ScmObj** %stackaddr$prim55185, align 8
%stackaddr$prim55186 = alloca %struct.ScmObj*, align 8
%argslist54601$ae501932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50194, %struct.ScmObj* %argslist54601$ae501931)
store volatile %struct.ScmObj* %argslist54601$ae501932, %struct.ScmObj** %stackaddr$prim55186, align 8
%clofunc55187 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50193)
musttail call tailcc void %clofunc55187(%struct.ScmObj* %ae50193, %struct.ScmObj* %argslist54601$ae501932)
ret void
}

define tailcc void @proc_clo$ae50193(%struct.ScmObj* %env$ae50193,%struct.ScmObj* %current_45args54387) {
%stackaddr$env-ref55188 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50193, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55188
%stackaddr$env-ref55189 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50193, i64 1)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55189
%stackaddr$prim55190 = alloca %struct.ScmObj*, align 8
%_95k48297 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54387)
store volatile %struct.ScmObj* %_95k48297, %struct.ScmObj** %stackaddr$prim55190, align 8
%stackaddr$prim55191 = alloca %struct.ScmObj*, align 8
%current_45args54388 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54387)
store volatile %struct.ScmObj* %current_45args54388, %struct.ScmObj** %stackaddr$prim55191, align 8
%stackaddr$prim55192 = alloca %struct.ScmObj*, align 8
%_37_62_6148122 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54388)
store volatile %struct.ScmObj* %_37_62_6148122, %struct.ScmObj** %stackaddr$prim55192, align 8
%ae50217 = call %struct.ScmObj* @const_init_int(i64 1)
%ae50218 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55193 = alloca %struct.ScmObj*, align 8
%_37append48118 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50217, %struct.ScmObj* %ae50218)
store volatile %struct.ScmObj* %_37append48118, %struct.ScmObj** %stackaddr$prim55193, align 8
%stackaddr$makeclosure55194 = alloca %struct.ScmObj*, align 8
%fptrToInt55195 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50219 to i64
%ae50219 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55195)
store volatile %struct.ScmObj* %ae50219, %struct.ScmObj** %stackaddr$makeclosure55194, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50219, %struct.ScmObj* %_37append48118, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50219, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50219, %struct.ScmObj* %_37map148077, i64 2)
%ae50220 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55196 = alloca %struct.ScmObj*, align 8
%fptrToInt55197 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50221 to i64
%ae50221 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55197)
store volatile %struct.ScmObj* %ae50221, %struct.ScmObj** %stackaddr$makeclosure55196, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50221, %struct.ScmObj* %_37append48118, i64 0)
%argslist54595$ae502190 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55198 = alloca %struct.ScmObj*, align 8
%argslist54595$ae502191 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50221, %struct.ScmObj* %argslist54595$ae502190)
store volatile %struct.ScmObj* %argslist54595$ae502191, %struct.ScmObj** %stackaddr$prim55198, align 8
%stackaddr$prim55199 = alloca %struct.ScmObj*, align 8
%argslist54595$ae502192 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50220, %struct.ScmObj* %argslist54595$ae502191)
store volatile %struct.ScmObj* %argslist54595$ae502192, %struct.ScmObj** %stackaddr$prim55199, align 8
%clofunc55200 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50219)
musttail call tailcc void %clofunc55200(%struct.ScmObj* %ae50219, %struct.ScmObj* %argslist54595$ae502192)
ret void
}

define tailcc void @proc_clo$ae50219(%struct.ScmObj* %env$ae50219,%struct.ScmObj* %current_45args54390) {
%stackaddr$env-ref55201 = alloca %struct.ScmObj*, align 8
%_37append48118 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50219, i64 0)
store %struct.ScmObj* %_37append48118, %struct.ScmObj** %stackaddr$env-ref55201
%stackaddr$env-ref55202 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50219, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55202
%stackaddr$env-ref55203 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50219, i64 2)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55203
%stackaddr$prim55204 = alloca %struct.ScmObj*, align 8
%_95k48298 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54390)
store volatile %struct.ScmObj* %_95k48298, %struct.ScmObj** %stackaddr$prim55204, align 8
%stackaddr$prim55205 = alloca %struct.ScmObj*, align 8
%current_45args54391 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54390)
store volatile %struct.ScmObj* %current_45args54391, %struct.ScmObj** %stackaddr$prim55205, align 8
%stackaddr$prim55206 = alloca %struct.ScmObj*, align 8
%anf_45bind48226 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54391)
store volatile %struct.ScmObj* %anf_45bind48226, %struct.ScmObj** %stackaddr$prim55206, align 8
%ae50287 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55207 = alloca %struct.ScmObj*, align 8
%_95048119 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append48118, %struct.ScmObj* %ae50287, %struct.ScmObj* %anf_45bind48226)
store volatile %struct.ScmObj* %_95048119, %struct.ScmObj** %stackaddr$prim55207, align 8
%ae50290 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55208 = alloca %struct.ScmObj*, align 8
%_37append48117 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append48118, %struct.ScmObj* %ae50290)
store volatile %struct.ScmObj* %_37append48117, %struct.ScmObj** %stackaddr$prim55208, align 8
%stackaddr$makeclosure55209 = alloca %struct.ScmObj*, align 8
%fptrToInt55210 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50291 to i64
%ae50291 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55210)
store volatile %struct.ScmObj* %ae50291, %struct.ScmObj** %stackaddr$makeclosure55209, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50291, %struct.ScmObj* %_37foldl148030, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50291, %struct.ScmObj* %_37map148077, i64 1)
%ae50292 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55211 = alloca %struct.ScmObj*, align 8
%fptrToInt55212 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50293 to i64
%ae50293 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55212)
store volatile %struct.ScmObj* %ae50293, %struct.ScmObj** %stackaddr$makeclosure55211, align 8
%argslist54584$ae502910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55213 = alloca %struct.ScmObj*, align 8
%argslist54584$ae502911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50293, %struct.ScmObj* %argslist54584$ae502910)
store volatile %struct.ScmObj* %argslist54584$ae502911, %struct.ScmObj** %stackaddr$prim55213, align 8
%stackaddr$prim55214 = alloca %struct.ScmObj*, align 8
%argslist54584$ae502912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50292, %struct.ScmObj* %argslist54584$ae502911)
store volatile %struct.ScmObj* %argslist54584$ae502912, %struct.ScmObj** %stackaddr$prim55214, align 8
%clofunc55215 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50291)
musttail call tailcc void %clofunc55215(%struct.ScmObj* %ae50291, %struct.ScmObj* %argslist54584$ae502912)
ret void
}

define tailcc void @proc_clo$ae50291(%struct.ScmObj* %env$ae50291,%struct.ScmObj* %current_45args54393) {
%stackaddr$env-ref55216 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50291, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55216
%stackaddr$env-ref55217 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50291, i64 1)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55217
%stackaddr$prim55218 = alloca %struct.ScmObj*, align 8
%_95k48299 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54393)
store volatile %struct.ScmObj* %_95k48299, %struct.ScmObj** %stackaddr$prim55218, align 8
%stackaddr$prim55219 = alloca %struct.ScmObj*, align 8
%current_45args54394 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54393)
store volatile %struct.ScmObj* %current_45args54394, %struct.ScmObj** %stackaddr$prim55219, align 8
%stackaddr$prim55220 = alloca %struct.ScmObj*, align 8
%_37list_6348110 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54394)
store volatile %struct.ScmObj* %_37list_6348110, %struct.ScmObj** %stackaddr$prim55220, align 8
%stackaddr$makeclosure55221 = alloca %struct.ScmObj*, align 8
%fptrToInt55222 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50707 to i64
%ae50707 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55222)
store volatile %struct.ScmObj* %ae50707, %struct.ScmObj** %stackaddr$makeclosure55221, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50707, %struct.ScmObj* %_37foldl148030, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50707, %struct.ScmObj* %_37map148077, i64 1)
%ae50708 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55223 = alloca %struct.ScmObj*, align 8
%fptrToInt55224 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50709 to i64
%ae50709 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55224)
store volatile %struct.ScmObj* %ae50709, %struct.ScmObj** %stackaddr$makeclosure55223, align 8
%argslist54559$ae507070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55225 = alloca %struct.ScmObj*, align 8
%argslist54559$ae507071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50709, %struct.ScmObj* %argslist54559$ae507070)
store volatile %struct.ScmObj* %argslist54559$ae507071, %struct.ScmObj** %stackaddr$prim55225, align 8
%stackaddr$prim55226 = alloca %struct.ScmObj*, align 8
%argslist54559$ae507072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50708, %struct.ScmObj* %argslist54559$ae507071)
store volatile %struct.ScmObj* %argslist54559$ae507072, %struct.ScmObj** %stackaddr$prim55226, align 8
%clofunc55227 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50707)
musttail call tailcc void %clofunc55227(%struct.ScmObj* %ae50707, %struct.ScmObj* %argslist54559$ae507072)
ret void
}

define tailcc void @proc_clo$ae50707(%struct.ScmObj* %env$ae50707,%struct.ScmObj* %current_45args54396) {
%stackaddr$env-ref55228 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50707, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55228
%stackaddr$env-ref55229 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50707, i64 1)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55229
%stackaddr$prim55230 = alloca %struct.ScmObj*, align 8
%_95k48300 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54396)
store volatile %struct.ScmObj* %_95k48300, %struct.ScmObj** %stackaddr$prim55230, align 8
%stackaddr$prim55231 = alloca %struct.ScmObj*, align 8
%current_45args54397 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54396)
store volatile %struct.ScmObj* %current_45args54397, %struct.ScmObj** %stackaddr$prim55231, align 8
%stackaddr$prim55232 = alloca %struct.ScmObj*, align 8
%_37drop48101 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54397)
store volatile %struct.ScmObj* %_37drop48101, %struct.ScmObj** %stackaddr$prim55232, align 8
%stackaddr$makeclosure55233 = alloca %struct.ScmObj*, align 8
%fptrToInt55234 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51243 to i64
%ae51243 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55234)
store volatile %struct.ScmObj* %ae51243, %struct.ScmObj** %stackaddr$makeclosure55233, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51243, %struct.ScmObj* %_37foldl148030, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51243, %struct.ScmObj* %_37map148077, i64 1)
%ae51244 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55235 = alloca %struct.ScmObj*, align 8
%fptrToInt55236 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51245 to i64
%ae51245 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55236)
store volatile %struct.ScmObj* %ae51245, %struct.ScmObj** %stackaddr$makeclosure55235, align 8
%argslist54535$ae512430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55237 = alloca %struct.ScmObj*, align 8
%argslist54535$ae512431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51245, %struct.ScmObj* %argslist54535$ae512430)
store volatile %struct.ScmObj* %argslist54535$ae512431, %struct.ScmObj** %stackaddr$prim55237, align 8
%stackaddr$prim55238 = alloca %struct.ScmObj*, align 8
%argslist54535$ae512432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51244, %struct.ScmObj* %argslist54535$ae512431)
store volatile %struct.ScmObj* %argslist54535$ae512432, %struct.ScmObj** %stackaddr$prim55238, align 8
%clofunc55239 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51243)
musttail call tailcc void %clofunc55239(%struct.ScmObj* %ae51243, %struct.ScmObj* %argslist54535$ae512432)
ret void
}

define tailcc void @proc_clo$ae51243(%struct.ScmObj* %env$ae51243,%struct.ScmObj* %current_45args54399) {
%stackaddr$env-ref55240 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51243, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55240
%stackaddr$env-ref55241 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51243, i64 1)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55241
%stackaddr$prim55242 = alloca %struct.ScmObj*, align 8
%_95k48301 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54399)
store volatile %struct.ScmObj* %_95k48301, %struct.ScmObj** %stackaddr$prim55242, align 8
%stackaddr$prim55243 = alloca %struct.ScmObj*, align 8
%current_45args54400 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54399)
store volatile %struct.ScmObj* %current_45args54400, %struct.ScmObj** %stackaddr$prim55243, align 8
%stackaddr$prim55244 = alloca %struct.ScmObj*, align 8
%_37memv48094 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54400)
store volatile %struct.ScmObj* %_37memv48094, %struct.ScmObj** %stackaddr$prim55244, align 8
%stackaddr$makeclosure55245 = alloca %struct.ScmObj*, align 8
%fptrToInt55246 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51647 to i64
%ae51647 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55246)
store volatile %struct.ScmObj* %ae51647, %struct.ScmObj** %stackaddr$makeclosure55245, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51647, %struct.ScmObj* %_37foldl148030, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51647, %struct.ScmObj* %_37map148077, i64 1)
%ae51648 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55247 = alloca %struct.ScmObj*, align 8
%fptrToInt55248 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51649 to i64
%ae51649 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55248)
store volatile %struct.ScmObj* %ae51649, %struct.ScmObj** %stackaddr$makeclosure55247, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51649, %struct.ScmObj* %_37foldl148030, i64 0)
%argslist54509$ae516470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55249 = alloca %struct.ScmObj*, align 8
%argslist54509$ae516471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51649, %struct.ScmObj* %argslist54509$ae516470)
store volatile %struct.ScmObj* %argslist54509$ae516471, %struct.ScmObj** %stackaddr$prim55249, align 8
%stackaddr$prim55250 = alloca %struct.ScmObj*, align 8
%argslist54509$ae516472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51648, %struct.ScmObj* %argslist54509$ae516471)
store volatile %struct.ScmObj* %argslist54509$ae516472, %struct.ScmObj** %stackaddr$prim55250, align 8
%clofunc55251 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51647)
musttail call tailcc void %clofunc55251(%struct.ScmObj* %ae51647, %struct.ScmObj* %argslist54509$ae516472)
ret void
}

define tailcc void @proc_clo$ae51647(%struct.ScmObj* %env$ae51647,%struct.ScmObj* %current_45args54402) {
%stackaddr$env-ref55252 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51647, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55252
%stackaddr$env-ref55253 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51647, i64 1)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55253
%stackaddr$prim55254 = alloca %struct.ScmObj*, align 8
%_95k48302 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54402)
store volatile %struct.ScmObj* %_95k48302, %struct.ScmObj** %stackaddr$prim55254, align 8
%stackaddr$prim55255 = alloca %struct.ScmObj*, align 8
%current_45args54403 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54402)
store volatile %struct.ScmObj* %current_45args54403, %struct.ScmObj** %stackaddr$prim55255, align 8
%stackaddr$prim55256 = alloca %struct.ScmObj*, align 8
%_37_4748090 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54403)
store volatile %struct.ScmObj* %_37_4748090, %struct.ScmObj** %stackaddr$prim55256, align 8
%stackaddr$makeclosure55257 = alloca %struct.ScmObj*, align 8
%fptrToInt55258 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51745 to i64
%ae51745 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55258)
store volatile %struct.ScmObj* %ae51745, %struct.ScmObj** %stackaddr$makeclosure55257, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51745, %struct.ScmObj* %_37foldl148030, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51745, %struct.ScmObj* %_37map148077, i64 1)
%ae51746 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55259 = alloca %struct.ScmObj*, align 8
%fptrToInt55260 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51747 to i64
%ae51747 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55260)
store volatile %struct.ScmObj* %ae51747, %struct.ScmObj** %stackaddr$makeclosure55259, align 8
%argslist54496$ae517450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55261 = alloca %struct.ScmObj*, align 8
%argslist54496$ae517451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51747, %struct.ScmObj* %argslist54496$ae517450)
store volatile %struct.ScmObj* %argslist54496$ae517451, %struct.ScmObj** %stackaddr$prim55261, align 8
%stackaddr$prim55262 = alloca %struct.ScmObj*, align 8
%argslist54496$ae517452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51746, %struct.ScmObj* %argslist54496$ae517451)
store volatile %struct.ScmObj* %argslist54496$ae517452, %struct.ScmObj** %stackaddr$prim55262, align 8
%clofunc55263 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51745)
musttail call tailcc void %clofunc55263(%struct.ScmObj* %ae51745, %struct.ScmObj* %argslist54496$ae517452)
ret void
}

define tailcc void @proc_clo$ae51745(%struct.ScmObj* %env$ae51745,%struct.ScmObj* %current_45args54405) {
%stackaddr$env-ref55264 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51745, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55264
%stackaddr$env-ref55265 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51745, i64 1)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55265
%stackaddr$prim55266 = alloca %struct.ScmObj*, align 8
%_95k48303 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54405)
store volatile %struct.ScmObj* %_95k48303, %struct.ScmObj** %stackaddr$prim55266, align 8
%stackaddr$prim55267 = alloca %struct.ScmObj*, align 8
%current_45args54406 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54405)
store volatile %struct.ScmObj* %current_45args54406, %struct.ScmObj** %stackaddr$prim55267, align 8
%stackaddr$prim55268 = alloca %struct.ScmObj*, align 8
%_37first48088 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54406)
store volatile %struct.ScmObj* %_37first48088, %struct.ScmObj** %stackaddr$prim55268, align 8
%stackaddr$makeclosure55269 = alloca %struct.ScmObj*, align 8
%fptrToInt55270 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51765 to i64
%ae51765 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55270)
store volatile %struct.ScmObj* %ae51765, %struct.ScmObj** %stackaddr$makeclosure55269, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51765, %struct.ScmObj* %_37foldl148030, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51765, %struct.ScmObj* %_37map148077, i64 1)
%ae51766 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55271 = alloca %struct.ScmObj*, align 8
%fptrToInt55272 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51767 to i64
%ae51767 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55272)
store volatile %struct.ScmObj* %ae51767, %struct.ScmObj** %stackaddr$makeclosure55271, align 8
%argslist54491$ae517650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55273 = alloca %struct.ScmObj*, align 8
%argslist54491$ae517651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51767, %struct.ScmObj* %argslist54491$ae517650)
store volatile %struct.ScmObj* %argslist54491$ae517651, %struct.ScmObj** %stackaddr$prim55273, align 8
%stackaddr$prim55274 = alloca %struct.ScmObj*, align 8
%argslist54491$ae517652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51766, %struct.ScmObj* %argslist54491$ae517651)
store volatile %struct.ScmObj* %argslist54491$ae517652, %struct.ScmObj** %stackaddr$prim55274, align 8
%clofunc55275 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51765)
musttail call tailcc void %clofunc55275(%struct.ScmObj* %ae51765, %struct.ScmObj* %argslist54491$ae517652)
ret void
}

define tailcc void @proc_clo$ae51765(%struct.ScmObj* %env$ae51765,%struct.ScmObj* %current_45args54408) {
%stackaddr$env-ref55276 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51765, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55276
%stackaddr$env-ref55277 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51765, i64 1)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55277
%stackaddr$prim55278 = alloca %struct.ScmObj*, align 8
%_95k48304 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54408)
store volatile %struct.ScmObj* %_95k48304, %struct.ScmObj** %stackaddr$prim55278, align 8
%stackaddr$prim55279 = alloca %struct.ScmObj*, align 8
%current_45args54409 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54408)
store volatile %struct.ScmObj* %current_45args54409, %struct.ScmObj** %stackaddr$prim55279, align 8
%stackaddr$prim55280 = alloca %struct.ScmObj*, align 8
%_37second48086 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54409)
store volatile %struct.ScmObj* %_37second48086, %struct.ScmObj** %stackaddr$prim55280, align 8
%stackaddr$makeclosure55281 = alloca %struct.ScmObj*, align 8
%fptrToInt55282 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51787 to i64
%ae51787 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55282)
store volatile %struct.ScmObj* %ae51787, %struct.ScmObj** %stackaddr$makeclosure55281, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51787, %struct.ScmObj* %_37foldl148030, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51787, %struct.ScmObj* %_37map148077, i64 1)
%ae51788 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55283 = alloca %struct.ScmObj*, align 8
%fptrToInt55284 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51789 to i64
%ae51789 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55284)
store volatile %struct.ScmObj* %ae51789, %struct.ScmObj** %stackaddr$makeclosure55283, align 8
%argslist54486$ae517870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55285 = alloca %struct.ScmObj*, align 8
%argslist54486$ae517871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51789, %struct.ScmObj* %argslist54486$ae517870)
store volatile %struct.ScmObj* %argslist54486$ae517871, %struct.ScmObj** %stackaddr$prim55285, align 8
%stackaddr$prim55286 = alloca %struct.ScmObj*, align 8
%argslist54486$ae517872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51788, %struct.ScmObj* %argslist54486$ae517871)
store volatile %struct.ScmObj* %argslist54486$ae517872, %struct.ScmObj** %stackaddr$prim55286, align 8
%clofunc55287 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51787)
musttail call tailcc void %clofunc55287(%struct.ScmObj* %ae51787, %struct.ScmObj* %argslist54486$ae517872)
ret void
}

define tailcc void @proc_clo$ae51787(%struct.ScmObj* %env$ae51787,%struct.ScmObj* %current_45args54411) {
%stackaddr$env-ref55288 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51787, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55288
%stackaddr$env-ref55289 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51787, i64 1)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55289
%stackaddr$prim55290 = alloca %struct.ScmObj*, align 8
%_95k48305 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54411)
store volatile %struct.ScmObj* %_95k48305, %struct.ScmObj** %stackaddr$prim55290, align 8
%stackaddr$prim55291 = alloca %struct.ScmObj*, align 8
%current_45args54412 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54411)
store volatile %struct.ScmObj* %current_45args54412, %struct.ScmObj** %stackaddr$prim55291, align 8
%stackaddr$prim55292 = alloca %struct.ScmObj*, align 8
%_37third48084 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54412)
store volatile %struct.ScmObj* %_37third48084, %struct.ScmObj** %stackaddr$prim55292, align 8
%stackaddr$makeclosure55293 = alloca %struct.ScmObj*, align 8
%fptrToInt55294 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51811 to i64
%ae51811 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55294)
store volatile %struct.ScmObj* %ae51811, %struct.ScmObj** %stackaddr$makeclosure55293, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51811, %struct.ScmObj* %_37foldl148030, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51811, %struct.ScmObj* %_37map148077, i64 1)
%ae51812 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55295 = alloca %struct.ScmObj*, align 8
%fptrToInt55296 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51813 to i64
%ae51813 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55296)
store volatile %struct.ScmObj* %ae51813, %struct.ScmObj** %stackaddr$makeclosure55295, align 8
%argslist54481$ae518110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55297 = alloca %struct.ScmObj*, align 8
%argslist54481$ae518111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51813, %struct.ScmObj* %argslist54481$ae518110)
store volatile %struct.ScmObj* %argslist54481$ae518111, %struct.ScmObj** %stackaddr$prim55297, align 8
%stackaddr$prim55298 = alloca %struct.ScmObj*, align 8
%argslist54481$ae518112 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51812, %struct.ScmObj* %argslist54481$ae518111)
store volatile %struct.ScmObj* %argslist54481$ae518112, %struct.ScmObj** %stackaddr$prim55298, align 8
%clofunc55299 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51811)
musttail call tailcc void %clofunc55299(%struct.ScmObj* %ae51811, %struct.ScmObj* %argslist54481$ae518112)
ret void
}

define tailcc void @proc_clo$ae51811(%struct.ScmObj* %env$ae51811,%struct.ScmObj* %current_45args54414) {
%stackaddr$env-ref55300 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51811, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55300
%stackaddr$env-ref55301 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51811, i64 1)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55301
%stackaddr$prim55302 = alloca %struct.ScmObj*, align 8
%_95k48306 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54414)
store volatile %struct.ScmObj* %_95k48306, %struct.ScmObj** %stackaddr$prim55302, align 8
%stackaddr$prim55303 = alloca %struct.ScmObj*, align 8
%current_45args54415 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54414)
store volatile %struct.ScmObj* %current_45args54415, %struct.ScmObj** %stackaddr$prim55303, align 8
%stackaddr$prim55304 = alloca %struct.ScmObj*, align 8
%_37fourth48082 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54415)
store volatile %struct.ScmObj* %_37fourth48082, %struct.ScmObj** %stackaddr$prim55304, align 8
%stackaddr$makeclosure55305 = alloca %struct.ScmObj*, align 8
%fptrToInt55306 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51837 to i64
%ae51837 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55306)
store volatile %struct.ScmObj* %ae51837, %struct.ScmObj** %stackaddr$makeclosure55305, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51837, %struct.ScmObj* %_37foldl148030, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51837, %struct.ScmObj* %_37map148077, i64 1)
%ae51838 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55307 = alloca %struct.ScmObj*, align 8
%fptrToInt55308 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51839 to i64
%ae51839 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55308)
store volatile %struct.ScmObj* %ae51839, %struct.ScmObj** %stackaddr$makeclosure55307, align 8
%argslist54476$ae518370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55309 = alloca %struct.ScmObj*, align 8
%argslist54476$ae518371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51839, %struct.ScmObj* %argslist54476$ae518370)
store volatile %struct.ScmObj* %argslist54476$ae518371, %struct.ScmObj** %stackaddr$prim55309, align 8
%stackaddr$prim55310 = alloca %struct.ScmObj*, align 8
%argslist54476$ae518372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51838, %struct.ScmObj* %argslist54476$ae518371)
store volatile %struct.ScmObj* %argslist54476$ae518372, %struct.ScmObj** %stackaddr$prim55310, align 8
%clofunc55311 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51837)
musttail call tailcc void %clofunc55311(%struct.ScmObj* %ae51837, %struct.ScmObj* %argslist54476$ae518372)
ret void
}

define tailcc void @proc_clo$ae51837(%struct.ScmObj* %env$ae51837,%struct.ScmObj* %current_45args54417) {
%stackaddr$env-ref55312 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51837, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55312
%stackaddr$env-ref55313 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51837, i64 1)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55313
%stackaddr$prim55314 = alloca %struct.ScmObj*, align 8
%_95k48307 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54417)
store volatile %struct.ScmObj* %_95k48307, %struct.ScmObj** %stackaddr$prim55314, align 8
%stackaddr$prim55315 = alloca %struct.ScmObj*, align 8
%current_45args54418 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54417)
store volatile %struct.ScmObj* %current_45args54418, %struct.ScmObj** %stackaddr$prim55315, align 8
%stackaddr$prim55316 = alloca %struct.ScmObj*, align 8
%anf_45bind48262 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54418)
store volatile %struct.ScmObj* %anf_45bind48262, %struct.ScmObj** %stackaddr$prim55316, align 8
%stackaddr$makeclosure55317 = alloca %struct.ScmObj*, align 8
%fptrToInt55318 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51861 to i64
%ae51861 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55318)
store volatile %struct.ScmObj* %ae51861, %struct.ScmObj** %stackaddr$makeclosure55317, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51861, %struct.ScmObj* %anf_45bind48262, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51861, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51861, %struct.ScmObj* %_37map148077, i64 2)
%ae51862 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55319 = alloca %struct.ScmObj*, align 8
%fptrToInt55320 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51863 to i64
%ae51863 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55320)
store volatile %struct.ScmObj* %ae51863, %struct.ScmObj** %stackaddr$makeclosure55319, align 8
%argslist54474$ae518610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55321 = alloca %struct.ScmObj*, align 8
%argslist54474$ae518611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51863, %struct.ScmObj* %argslist54474$ae518610)
store volatile %struct.ScmObj* %argslist54474$ae518611, %struct.ScmObj** %stackaddr$prim55321, align 8
%stackaddr$prim55322 = alloca %struct.ScmObj*, align 8
%argslist54474$ae518612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51862, %struct.ScmObj* %argslist54474$ae518611)
store volatile %struct.ScmObj* %argslist54474$ae518612, %struct.ScmObj** %stackaddr$prim55322, align 8
%clofunc55323 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51861)
musttail call tailcc void %clofunc55323(%struct.ScmObj* %ae51861, %struct.ScmObj* %argslist54474$ae518612)
ret void
}

define tailcc void @proc_clo$ae51861(%struct.ScmObj* %env$ae51861,%struct.ScmObj* %current_45args54420) {
%stackaddr$env-ref55324 = alloca %struct.ScmObj*, align 8
%anf_45bind48262 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51861, i64 0)
store %struct.ScmObj* %anf_45bind48262, %struct.ScmObj** %stackaddr$env-ref55324
%stackaddr$env-ref55325 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51861, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55325
%stackaddr$env-ref55326 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51861, i64 2)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55326
%stackaddr$prim55327 = alloca %struct.ScmObj*, align 8
%_95k48308 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54420)
store volatile %struct.ScmObj* %_95k48308, %struct.ScmObj** %stackaddr$prim55327, align 8
%stackaddr$prim55328 = alloca %struct.ScmObj*, align 8
%current_45args54421 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54420)
store volatile %struct.ScmObj* %current_45args54421, %struct.ScmObj** %stackaddr$prim55328, align 8
%stackaddr$prim55329 = alloca %struct.ScmObj*, align 8
%anf_45bind48263 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54421)
store volatile %struct.ScmObj* %anf_45bind48263, %struct.ScmObj** %stackaddr$prim55329, align 8
%stackaddr$makeclosure55330 = alloca %struct.ScmObj*, align 8
%fptrToInt55331 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51884 to i64
%ae51884 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55331)
store volatile %struct.ScmObj* %ae51884, %struct.ScmObj** %stackaddr$makeclosure55330, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51884, %struct.ScmObj* %anf_45bind48262, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51884, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51884, %struct.ScmObj* %_37map148077, i64 2)
%ae51885 = call %struct.ScmObj* @const_init_int(i64 1)
%ae51886 = call %struct.ScmObj* @const_init_int(i64 2)
%ae51887 = call %struct.ScmObj* @const_init_int(i64 3)
%ae51888 = call %struct.ScmObj* @const_init_int(i64 4)
%argslist54472$anf_45bind482630 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55332 = alloca %struct.ScmObj*, align 8
%argslist54472$anf_45bind482631 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51888, %struct.ScmObj* %argslist54472$anf_45bind482630)
store volatile %struct.ScmObj* %argslist54472$anf_45bind482631, %struct.ScmObj** %stackaddr$prim55332, align 8
%stackaddr$prim55333 = alloca %struct.ScmObj*, align 8
%argslist54472$anf_45bind482632 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51887, %struct.ScmObj* %argslist54472$anf_45bind482631)
store volatile %struct.ScmObj* %argslist54472$anf_45bind482632, %struct.ScmObj** %stackaddr$prim55333, align 8
%stackaddr$prim55334 = alloca %struct.ScmObj*, align 8
%argslist54472$anf_45bind482633 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51886, %struct.ScmObj* %argslist54472$anf_45bind482632)
store volatile %struct.ScmObj* %argslist54472$anf_45bind482633, %struct.ScmObj** %stackaddr$prim55334, align 8
%stackaddr$prim55335 = alloca %struct.ScmObj*, align 8
%argslist54472$anf_45bind482634 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51885, %struct.ScmObj* %argslist54472$anf_45bind482633)
store volatile %struct.ScmObj* %argslist54472$anf_45bind482634, %struct.ScmObj** %stackaddr$prim55335, align 8
%stackaddr$prim55336 = alloca %struct.ScmObj*, align 8
%argslist54472$anf_45bind482635 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51884, %struct.ScmObj* %argslist54472$anf_45bind482634)
store volatile %struct.ScmObj* %argslist54472$anf_45bind482635, %struct.ScmObj** %stackaddr$prim55336, align 8
%clofunc55337 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48263)
musttail call tailcc void %clofunc55337(%struct.ScmObj* %anf_45bind48263, %struct.ScmObj* %argslist54472$anf_45bind482635)
ret void
}

define tailcc void @proc_clo$ae51884(%struct.ScmObj* %env$ae51884,%struct.ScmObj* %current_45args54423) {
%stackaddr$env-ref55338 = alloca %struct.ScmObj*, align 8
%anf_45bind48262 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51884, i64 0)
store %struct.ScmObj* %anf_45bind48262, %struct.ScmObj** %stackaddr$env-ref55338
%stackaddr$env-ref55339 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51884, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55339
%stackaddr$env-ref55340 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51884, i64 2)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55340
%stackaddr$prim55341 = alloca %struct.ScmObj*, align 8
%_95k48309 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54423)
store volatile %struct.ScmObj* %_95k48309, %struct.ScmObj** %stackaddr$prim55341, align 8
%stackaddr$prim55342 = alloca %struct.ScmObj*, align 8
%current_45args54424 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54423)
store volatile %struct.ScmObj* %current_45args54424, %struct.ScmObj** %stackaddr$prim55342, align 8
%stackaddr$prim55343 = alloca %struct.ScmObj*, align 8
%anf_45bind48264 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54424)
store volatile %struct.ScmObj* %anf_45bind48264, %struct.ScmObj** %stackaddr$prim55343, align 8
%stackaddr$makeclosure55344 = alloca %struct.ScmObj*, align 8
%fptrToInt55345 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51914 to i64
%ae51914 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55345)
store volatile %struct.ScmObj* %ae51914, %struct.ScmObj** %stackaddr$makeclosure55344, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51914, %struct.ScmObj* %_37map148077, i64 0)
%ae51916 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54471$_37foldl1480300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55346 = alloca %struct.ScmObj*, align 8
%argslist54471$_37foldl1480301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48264, %struct.ScmObj* %argslist54471$_37foldl1480300)
store volatile %struct.ScmObj* %argslist54471$_37foldl1480301, %struct.ScmObj** %stackaddr$prim55346, align 8
%stackaddr$prim55347 = alloca %struct.ScmObj*, align 8
%argslist54471$_37foldl1480302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51916, %struct.ScmObj* %argslist54471$_37foldl1480301)
store volatile %struct.ScmObj* %argslist54471$_37foldl1480302, %struct.ScmObj** %stackaddr$prim55347, align 8
%stackaddr$prim55348 = alloca %struct.ScmObj*, align 8
%argslist54471$_37foldl1480303 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48262, %struct.ScmObj* %argslist54471$_37foldl1480302)
store volatile %struct.ScmObj* %argslist54471$_37foldl1480303, %struct.ScmObj** %stackaddr$prim55348, align 8
%stackaddr$prim55349 = alloca %struct.ScmObj*, align 8
%argslist54471$_37foldl1480304 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51914, %struct.ScmObj* %argslist54471$_37foldl1480303)
store volatile %struct.ScmObj* %argslist54471$_37foldl1480304, %struct.ScmObj** %stackaddr$prim55349, align 8
%clofunc55350 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148030)
musttail call tailcc void %clofunc55350(%struct.ScmObj* %_37foldl148030, %struct.ScmObj* %argslist54471$_37foldl1480304)
ret void
}

define tailcc void @proc_clo$ae51914(%struct.ScmObj* %env$ae51914,%struct.ScmObj* %current_45args54426) {
%stackaddr$env-ref55351 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51914, i64 0)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55351
%stackaddr$prim55352 = alloca %struct.ScmObj*, align 8
%_95k48310 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54426)
store volatile %struct.ScmObj* %_95k48310, %struct.ScmObj** %stackaddr$prim55352, align 8
%stackaddr$prim55353 = alloca %struct.ScmObj*, align 8
%current_45args54427 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54426)
store volatile %struct.ScmObj* %current_45args54427, %struct.ScmObj** %stackaddr$prim55353, align 8
%stackaddr$prim55354 = alloca %struct.ScmObj*, align 8
%anf_45bind48265 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54427)
store volatile %struct.ScmObj* %anf_45bind48265, %struct.ScmObj** %stackaddr$prim55354, align 8
%stackaddr$makeclosure55355 = alloca %struct.ScmObj*, align 8
%fptrToInt55356 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51923 to i64
%ae51923 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55356)
store volatile %struct.ScmObj* %ae51923, %struct.ScmObj** %stackaddr$makeclosure55355, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51923, %struct.ScmObj* %anf_45bind48265, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51923, %struct.ScmObj* %_37map148077, i64 1)
%ae51924 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55357 = alloca %struct.ScmObj*, align 8
%fptrToInt55358 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51925 to i64
%ae51925 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55358)
store volatile %struct.ScmObj* %ae51925, %struct.ScmObj** %stackaddr$makeclosure55357, align 8
%argslist54470$ae519230 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55359 = alloca %struct.ScmObj*, align 8
%argslist54470$ae519231 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51925, %struct.ScmObj* %argslist54470$ae519230)
store volatile %struct.ScmObj* %argslist54470$ae519231, %struct.ScmObj** %stackaddr$prim55359, align 8
%stackaddr$prim55360 = alloca %struct.ScmObj*, align 8
%argslist54470$ae519232 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51924, %struct.ScmObj* %argslist54470$ae519231)
store volatile %struct.ScmObj* %argslist54470$ae519232, %struct.ScmObj** %stackaddr$prim55360, align 8
%clofunc55361 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51923)
musttail call tailcc void %clofunc55361(%struct.ScmObj* %ae51923, %struct.ScmObj* %argslist54470$ae519232)
ret void
}

define tailcc void @proc_clo$ae51923(%struct.ScmObj* %env$ae51923,%struct.ScmObj* %current_45args54429) {
%stackaddr$env-ref55362 = alloca %struct.ScmObj*, align 8
%anf_45bind48265 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51923, i64 0)
store %struct.ScmObj* %anf_45bind48265, %struct.ScmObj** %stackaddr$env-ref55362
%stackaddr$env-ref55363 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51923, i64 1)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55363
%stackaddr$prim55364 = alloca %struct.ScmObj*, align 8
%_95k48311 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54429)
store volatile %struct.ScmObj* %_95k48311, %struct.ScmObj** %stackaddr$prim55364, align 8
%stackaddr$prim55365 = alloca %struct.ScmObj*, align 8
%current_45args54430 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54429)
store volatile %struct.ScmObj* %current_45args54430, %struct.ScmObj** %stackaddr$prim55365, align 8
%stackaddr$prim55366 = alloca %struct.ScmObj*, align 8
%anf_45bind48266 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54430)
store volatile %struct.ScmObj* %anf_45bind48266, %struct.ScmObj** %stackaddr$prim55366, align 8
%stackaddr$makeclosure55367 = alloca %struct.ScmObj*, align 8
%fptrToInt55368 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51947 to i64
%ae51947 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55368)
store volatile %struct.ScmObj* %ae51947, %struct.ScmObj** %stackaddr$makeclosure55367, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51947, %struct.ScmObj* %anf_45bind48265, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51947, %struct.ScmObj* %_37map148077, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51947, %struct.ScmObj* %anf_45bind48266, i64 2)
%ae51948 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55369 = alloca %struct.ScmObj*, align 8
%fptrToInt55370 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51949 to i64
%ae51949 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55370)
store volatile %struct.ScmObj* %ae51949, %struct.ScmObj** %stackaddr$makeclosure55369, align 8
%argslist54468$ae519470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55371 = alloca %struct.ScmObj*, align 8
%argslist54468$ae519471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51949, %struct.ScmObj* %argslist54468$ae519470)
store volatile %struct.ScmObj* %argslist54468$ae519471, %struct.ScmObj** %stackaddr$prim55371, align 8
%stackaddr$prim55372 = alloca %struct.ScmObj*, align 8
%argslist54468$ae519472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51948, %struct.ScmObj* %argslist54468$ae519471)
store volatile %struct.ScmObj* %argslist54468$ae519472, %struct.ScmObj** %stackaddr$prim55372, align 8
%clofunc55373 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51947)
musttail call tailcc void %clofunc55373(%struct.ScmObj* %ae51947, %struct.ScmObj* %argslist54468$ae519472)
ret void
}

define tailcc void @proc_clo$ae51947(%struct.ScmObj* %env$ae51947,%struct.ScmObj* %current_45args54432) {
%stackaddr$env-ref55374 = alloca %struct.ScmObj*, align 8
%anf_45bind48265 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51947, i64 0)
store %struct.ScmObj* %anf_45bind48265, %struct.ScmObj** %stackaddr$env-ref55374
%stackaddr$env-ref55375 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51947, i64 1)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55375
%stackaddr$env-ref55376 = alloca %struct.ScmObj*, align 8
%anf_45bind48266 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51947, i64 2)
store %struct.ScmObj* %anf_45bind48266, %struct.ScmObj** %stackaddr$env-ref55376
%stackaddr$prim55377 = alloca %struct.ScmObj*, align 8
%_95k48312 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54432)
store volatile %struct.ScmObj* %_95k48312, %struct.ScmObj** %stackaddr$prim55377, align 8
%stackaddr$prim55378 = alloca %struct.ScmObj*, align 8
%current_45args54433 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54432)
store volatile %struct.ScmObj* %current_45args54433, %struct.ScmObj** %stackaddr$prim55378, align 8
%stackaddr$prim55379 = alloca %struct.ScmObj*, align 8
%anf_45bind48267 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54433)
store volatile %struct.ScmObj* %anf_45bind48267, %struct.ScmObj** %stackaddr$prim55379, align 8
%stackaddr$makeclosure55380 = alloca %struct.ScmObj*, align 8
%fptrToInt55381 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51970 to i64
%ae51970 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55381)
store volatile %struct.ScmObj* %ae51970, %struct.ScmObj** %stackaddr$makeclosure55380, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51970, %struct.ScmObj* %anf_45bind48265, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51970, %struct.ScmObj* %_37map148077, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51970, %struct.ScmObj* %anf_45bind48266, i64 2)
%ae51971 = call %struct.ScmObj* @const_init_int(i64 2)
%argslist54466$anf_45bind482670 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55382 = alloca %struct.ScmObj*, align 8
%argslist54466$anf_45bind482671 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51971, %struct.ScmObj* %argslist54466$anf_45bind482670)
store volatile %struct.ScmObj* %argslist54466$anf_45bind482671, %struct.ScmObj** %stackaddr$prim55382, align 8
%stackaddr$prim55383 = alloca %struct.ScmObj*, align 8
%argslist54466$anf_45bind482672 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51970, %struct.ScmObj* %argslist54466$anf_45bind482671)
store volatile %struct.ScmObj* %argslist54466$anf_45bind482672, %struct.ScmObj** %stackaddr$prim55383, align 8
%clofunc55384 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48267)
musttail call tailcc void %clofunc55384(%struct.ScmObj* %anf_45bind48267, %struct.ScmObj* %argslist54466$anf_45bind482672)
ret void
}

define tailcc void @proc_clo$ae51970(%struct.ScmObj* %env$ae51970,%struct.ScmObj* %current_45args54435) {
%stackaddr$env-ref55385 = alloca %struct.ScmObj*, align 8
%anf_45bind48265 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51970, i64 0)
store %struct.ScmObj* %anf_45bind48265, %struct.ScmObj** %stackaddr$env-ref55385
%stackaddr$env-ref55386 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51970, i64 1)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55386
%stackaddr$env-ref55387 = alloca %struct.ScmObj*, align 8
%anf_45bind48266 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51970, i64 2)
store %struct.ScmObj* %anf_45bind48266, %struct.ScmObj** %stackaddr$env-ref55387
%stackaddr$prim55388 = alloca %struct.ScmObj*, align 8
%_95k48313 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54435)
store volatile %struct.ScmObj* %_95k48313, %struct.ScmObj** %stackaddr$prim55388, align 8
%stackaddr$prim55389 = alloca %struct.ScmObj*, align 8
%current_45args54436 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54435)
store volatile %struct.ScmObj* %current_45args54436, %struct.ScmObj** %stackaddr$prim55389, align 8
%stackaddr$prim55390 = alloca %struct.ScmObj*, align 8
%anf_45bind48268 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54436)
store volatile %struct.ScmObj* %anf_45bind48268, %struct.ScmObj** %stackaddr$prim55390, align 8
%stackaddr$makeclosure55391 = alloca %struct.ScmObj*, align 8
%fptrToInt55392 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51976 to i64
%ae51976 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55392)
store volatile %struct.ScmObj* %ae51976, %struct.ScmObj** %stackaddr$makeclosure55391, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51976, %struct.ScmObj* %anf_45bind48265, i64 0)
%argslist54465$_37map1480770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55393 = alloca %struct.ScmObj*, align 8
%argslist54465$_37map1480771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48268, %struct.ScmObj* %argslist54465$_37map1480770)
store volatile %struct.ScmObj* %argslist54465$_37map1480771, %struct.ScmObj** %stackaddr$prim55393, align 8
%stackaddr$prim55394 = alloca %struct.ScmObj*, align 8
%argslist54465$_37map1480772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48266, %struct.ScmObj* %argslist54465$_37map1480771)
store volatile %struct.ScmObj* %argslist54465$_37map1480772, %struct.ScmObj** %stackaddr$prim55394, align 8
%stackaddr$prim55395 = alloca %struct.ScmObj*, align 8
%argslist54465$_37map1480773 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51976, %struct.ScmObj* %argslist54465$_37map1480772)
store volatile %struct.ScmObj* %argslist54465$_37map1480773, %struct.ScmObj** %stackaddr$prim55395, align 8
%clofunc55396 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148077)
musttail call tailcc void %clofunc55396(%struct.ScmObj* %_37map148077, %struct.ScmObj* %argslist54465$_37map1480773)
ret void
}

define tailcc void @proc_clo$ae51976(%struct.ScmObj* %env$ae51976,%struct.ScmObj* %current_45args54438) {
%stackaddr$env-ref55397 = alloca %struct.ScmObj*, align 8
%anf_45bind48265 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51976, i64 0)
store %struct.ScmObj* %anf_45bind48265, %struct.ScmObj** %stackaddr$env-ref55397
%stackaddr$prim55398 = alloca %struct.ScmObj*, align 8
%_95k48314 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54438)
store volatile %struct.ScmObj* %_95k48314, %struct.ScmObj** %stackaddr$prim55398, align 8
%stackaddr$prim55399 = alloca %struct.ScmObj*, align 8
%current_45args54439 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54438)
store volatile %struct.ScmObj* %current_45args54439, %struct.ScmObj** %stackaddr$prim55399, align 8
%stackaddr$prim55400 = alloca %struct.ScmObj*, align 8
%anf_45bind48269 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54439)
store volatile %struct.ScmObj* %anf_45bind48269, %struct.ScmObj** %stackaddr$prim55400, align 8
%stackaddr$prim55401 = alloca %struct.ScmObj*, align 8
%anf_45bind48270 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48269)
store volatile %struct.ScmObj* %anf_45bind48270, %struct.ScmObj** %stackaddr$prim55401, align 8
%stackaddr$prim55402 = alloca %struct.ScmObj*, align 8
%anf_45bind48271 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind48265, %struct.ScmObj* %anf_45bind48270)
store volatile %struct.ScmObj* %anf_45bind48271, %struct.ScmObj** %stackaddr$prim55402, align 8
%stackaddr$makeclosure55403 = alloca %struct.ScmObj*, align 8
%fptrToInt55404 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51982 to i64
%ae51982 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55404)
store volatile %struct.ScmObj* %ae51982, %struct.ScmObj** %stackaddr$makeclosure55403, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51982, %struct.ScmObj* %anf_45bind48271, i64 0)
%ae51983 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55405 = alloca %struct.ScmObj*, align 8
%fptrToInt55406 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51984 to i64
%ae51984 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55406)
store volatile %struct.ScmObj* %ae51984, %struct.ScmObj** %stackaddr$makeclosure55405, align 8
%argslist54464$ae519820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55407 = alloca %struct.ScmObj*, align 8
%argslist54464$ae519821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51984, %struct.ScmObj* %argslist54464$ae519820)
store volatile %struct.ScmObj* %argslist54464$ae519821, %struct.ScmObj** %stackaddr$prim55407, align 8
%stackaddr$prim55408 = alloca %struct.ScmObj*, align 8
%argslist54464$ae519822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51983, %struct.ScmObj* %argslist54464$ae519821)
store volatile %struct.ScmObj* %argslist54464$ae519822, %struct.ScmObj** %stackaddr$prim55408, align 8
%clofunc55409 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51982)
musttail call tailcc void %clofunc55409(%struct.ScmObj* %ae51982, %struct.ScmObj* %argslist54464$ae519822)
ret void
}

define tailcc void @proc_clo$ae51982(%struct.ScmObj* %env$ae51982,%struct.ScmObj* %current_45args54441) {
%stackaddr$env-ref55410 = alloca %struct.ScmObj*, align 8
%anf_45bind48271 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51982, i64 0)
store %struct.ScmObj* %anf_45bind48271, %struct.ScmObj** %stackaddr$env-ref55410
%stackaddr$prim55411 = alloca %struct.ScmObj*, align 8
%_95k48315 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54441)
store volatile %struct.ScmObj* %_95k48315, %struct.ScmObj** %stackaddr$prim55411, align 8
%stackaddr$prim55412 = alloca %struct.ScmObj*, align 8
%current_45args54442 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54441)
store volatile %struct.ScmObj* %current_45args54442, %struct.ScmObj** %stackaddr$prim55412, align 8
%stackaddr$prim55413 = alloca %struct.ScmObj*, align 8
%anf_45bind48272 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54442)
store volatile %struct.ScmObj* %anf_45bind48272, %struct.ScmObj** %stackaddr$prim55413, align 8
%stackaddr$makeclosure55414 = alloca %struct.ScmObj*, align 8
%fptrToInt55415 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51999 to i64
%ae51999 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55415)
store volatile %struct.ScmObj* %ae51999, %struct.ScmObj** %stackaddr$makeclosure55414, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51999, %struct.ScmObj* %anf_45bind48272, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51999, %struct.ScmObj* %anf_45bind48271, i64 1)
%ae52000 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55416 = alloca %struct.ScmObj*, align 8
%fptrToInt55417 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52001 to i64
%ae52001 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55417)
store volatile %struct.ScmObj* %ae52001, %struct.ScmObj** %stackaddr$makeclosure55416, align 8
%argslist54457$ae519990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55418 = alloca %struct.ScmObj*, align 8
%argslist54457$ae519991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52001, %struct.ScmObj* %argslist54457$ae519990)
store volatile %struct.ScmObj* %argslist54457$ae519991, %struct.ScmObj** %stackaddr$prim55418, align 8
%stackaddr$prim55419 = alloca %struct.ScmObj*, align 8
%argslist54457$ae519992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52000, %struct.ScmObj* %argslist54457$ae519991)
store volatile %struct.ScmObj* %argslist54457$ae519992, %struct.ScmObj** %stackaddr$prim55419, align 8
%clofunc55420 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51999)
musttail call tailcc void %clofunc55420(%struct.ScmObj* %ae51999, %struct.ScmObj* %argslist54457$ae519992)
ret void
}

define tailcc void @proc_clo$ae51999(%struct.ScmObj* %env$ae51999,%struct.ScmObj* %current_45args54444) {
%stackaddr$env-ref55421 = alloca %struct.ScmObj*, align 8
%anf_45bind48272 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51999, i64 0)
store %struct.ScmObj* %anf_45bind48272, %struct.ScmObj** %stackaddr$env-ref55421
%stackaddr$env-ref55422 = alloca %struct.ScmObj*, align 8
%anf_45bind48271 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51999, i64 1)
store %struct.ScmObj* %anf_45bind48271, %struct.ScmObj** %stackaddr$env-ref55422
%stackaddr$prim55423 = alloca %struct.ScmObj*, align 8
%_95k48316 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54444)
store volatile %struct.ScmObj* %_95k48316, %struct.ScmObj** %stackaddr$prim55423, align 8
%stackaddr$prim55424 = alloca %struct.ScmObj*, align 8
%current_45args54445 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54444)
store volatile %struct.ScmObj* %current_45args54445, %struct.ScmObj** %stackaddr$prim55424, align 8
%stackaddr$prim55425 = alloca %struct.ScmObj*, align 8
%anf_45bind48273 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54445)
store volatile %struct.ScmObj* %anf_45bind48273, %struct.ScmObj** %stackaddr$prim55425, align 8
%stackaddr$makeclosure55426 = alloca %struct.ScmObj*, align 8
%fptrToInt55427 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52024 to i64
%ae52024 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55427)
store volatile %struct.ScmObj* %ae52024, %struct.ScmObj** %stackaddr$makeclosure55426, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52024, %struct.ScmObj* %anf_45bind48271, i64 0)
%ae52025 = call %struct.ScmObj* @const_init_int(i64 2)
%ae52027 = call %struct.ScmObj* @const_init_int(i64 3)
%argslist54455$anf_45bind482720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55428 = alloca %struct.ScmObj*, align 8
%argslist54455$anf_45bind482721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52027, %struct.ScmObj* %argslist54455$anf_45bind482720)
store volatile %struct.ScmObj* %argslist54455$anf_45bind482721, %struct.ScmObj** %stackaddr$prim55428, align 8
%stackaddr$prim55429 = alloca %struct.ScmObj*, align 8
%argslist54455$anf_45bind482722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48273, %struct.ScmObj* %argslist54455$anf_45bind482721)
store volatile %struct.ScmObj* %argslist54455$anf_45bind482722, %struct.ScmObj** %stackaddr$prim55429, align 8
%stackaddr$prim55430 = alloca %struct.ScmObj*, align 8
%argslist54455$anf_45bind482723 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52025, %struct.ScmObj* %argslist54455$anf_45bind482722)
store volatile %struct.ScmObj* %argslist54455$anf_45bind482723, %struct.ScmObj** %stackaddr$prim55430, align 8
%stackaddr$prim55431 = alloca %struct.ScmObj*, align 8
%argslist54455$anf_45bind482724 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52024, %struct.ScmObj* %argslist54455$anf_45bind482723)
store volatile %struct.ScmObj* %argslist54455$anf_45bind482724, %struct.ScmObj** %stackaddr$prim55431, align 8
%clofunc55432 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48272)
musttail call tailcc void %clofunc55432(%struct.ScmObj* %anf_45bind48272, %struct.ScmObj* %argslist54455$anf_45bind482724)
ret void
}

define tailcc void @proc_clo$ae52024(%struct.ScmObj* %env$ae52024,%struct.ScmObj* %current_45args54447) {
%stackaddr$env-ref55433 = alloca %struct.ScmObj*, align 8
%anf_45bind48271 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52024, i64 0)
store %struct.ScmObj* %anf_45bind48271, %struct.ScmObj** %stackaddr$env-ref55433
%stackaddr$prim55434 = alloca %struct.ScmObj*, align 8
%_95k48317 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54447)
store volatile %struct.ScmObj* %_95k48317, %struct.ScmObj** %stackaddr$prim55434, align 8
%stackaddr$prim55435 = alloca %struct.ScmObj*, align 8
%current_45args54448 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54447)
store volatile %struct.ScmObj* %current_45args54448, %struct.ScmObj** %stackaddr$prim55435, align 8
%stackaddr$prim55436 = alloca %struct.ScmObj*, align 8
%anf_45bind48274 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54448)
store volatile %struct.ScmObj* %anf_45bind48274, %struct.ScmObj** %stackaddr$prim55436, align 8
%stackaddr$prim55437 = alloca %struct.ScmObj*, align 8
%cpsprim48318 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind48271, %struct.ScmObj* %anf_45bind48274)
store volatile %struct.ScmObj* %cpsprim48318, %struct.ScmObj** %stackaddr$prim55437, align 8
%stackaddr$makeclosure55438 = alloca %struct.ScmObj*, align 8
%fptrToInt55439 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52040 to i64
%ae52040 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55439)
store volatile %struct.ScmObj* %ae52040, %struct.ScmObj** %stackaddr$makeclosure55438, align 8
%ae52041 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54454$ae520400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55440 = alloca %struct.ScmObj*, align 8
%argslist54454$ae520401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48318, %struct.ScmObj* %argslist54454$ae520400)
store volatile %struct.ScmObj* %argslist54454$ae520401, %struct.ScmObj** %stackaddr$prim55440, align 8
%stackaddr$prim55441 = alloca %struct.ScmObj*, align 8
%argslist54454$ae520402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52041, %struct.ScmObj* %argslist54454$ae520401)
store volatile %struct.ScmObj* %argslist54454$ae520402, %struct.ScmObj** %stackaddr$prim55441, align 8
%clofunc55442 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52040)
musttail call tailcc void %clofunc55442(%struct.ScmObj* %ae52040, %struct.ScmObj* %argslist54454$ae520402)
ret void
}

define tailcc void @proc_clo$ae52040(%struct.ScmObj* %env$ae52040,%struct.ScmObj* %current_45args54450) {
%stackaddr$prim55443 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54450)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim55443, align 8
%stackaddr$prim55444 = alloca %struct.ScmObj*, align 8
%current_45args54451 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54450)
store volatile %struct.ScmObj* %current_45args54451, %struct.ScmObj** %stackaddr$prim55444, align 8
%stackaddr$prim55445 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54451)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim55445, align 8
%stackaddr$prim55446 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim55446, align 8
%argslist54453$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55447 = alloca %struct.ScmObj*, align 8
%argslist54453$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist54453$k0)
store volatile %struct.ScmObj* %argslist54453$k1, %struct.ScmObj** %stackaddr$prim55447, align 8
%clofunc55448 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc55448(%struct.ScmObj* %k, %struct.ScmObj* %argslist54453$k1)
ret void
}

define tailcc void @proc_clo$ae52001(%struct.ScmObj* %env$ae52001,%struct.ScmObj* %args4815048319) {
%stackaddr$prim55449 = alloca %struct.ScmObj*, align 8
%k48320 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4815048319)
store volatile %struct.ScmObj* %k48320, %struct.ScmObj** %stackaddr$prim55449, align 8
%stackaddr$prim55450 = alloca %struct.ScmObj*, align 8
%args48150 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4815048319)
store volatile %struct.ScmObj* %args48150, %struct.ScmObj** %stackaddr$prim55450, align 8
%stackaddr$applyprim55451 = alloca %struct.ScmObj*, align 8
%cpsaprim48321 = call %struct.ScmObj* @applyprim__42(%struct.ScmObj* %args48150)
store volatile %struct.ScmObj* %cpsaprim48321, %struct.ScmObj** %stackaddr$applyprim55451, align 8
%ae52006 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54456$k483200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55452 = alloca %struct.ScmObj*, align 8
%argslist54456$k483201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsaprim48321, %struct.ScmObj* %argslist54456$k483200)
store volatile %struct.ScmObj* %argslist54456$k483201, %struct.ScmObj** %stackaddr$prim55452, align 8
%stackaddr$prim55453 = alloca %struct.ScmObj*, align 8
%argslist54456$k483202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52006, %struct.ScmObj* %argslist54456$k483201)
store volatile %struct.ScmObj* %argslist54456$k483202, %struct.ScmObj** %stackaddr$prim55453, align 8
%clofunc55454 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48320)
musttail call tailcc void %clofunc55454(%struct.ScmObj* %k48320, %struct.ScmObj* %argslist54456$k483202)
ret void
}

define tailcc void @proc_clo$ae51984(%struct.ScmObj* %env$ae51984,%struct.ScmObj* %current_45args54458) {
%stackaddr$prim55455 = alloca %struct.ScmObj*, align 8
%k48322 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54458)
store volatile %struct.ScmObj* %k48322, %struct.ScmObj** %stackaddr$prim55455, align 8
%stackaddr$prim55456 = alloca %struct.ScmObj*, align 8
%current_45args54459 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54458)
store volatile %struct.ScmObj* %current_45args54459, %struct.ScmObj** %stackaddr$prim55456, align 8
%stackaddr$prim55457 = alloca %struct.ScmObj*, align 8
%x48149 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54459)
store volatile %struct.ScmObj* %x48149, %struct.ScmObj** %stackaddr$prim55457, align 8
%stackaddr$prim55458 = alloca %struct.ScmObj*, align 8
%current_45args54460 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54459)
store volatile %struct.ScmObj* %current_45args54460, %struct.ScmObj** %stackaddr$prim55458, align 8
%stackaddr$prim55459 = alloca %struct.ScmObj*, align 8
%f48148 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54460)
store volatile %struct.ScmObj* %f48148, %struct.ScmObj** %stackaddr$prim55459, align 8
%stackaddr$prim55460 = alloca %struct.ScmObj*, align 8
%current_45args54461 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54460)
store volatile %struct.ScmObj* %current_45args54461, %struct.ScmObj** %stackaddr$prim55460, align 8
%stackaddr$prim55461 = alloca %struct.ScmObj*, align 8
%y48147 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54461)
store volatile %struct.ScmObj* %y48147, %struct.ScmObj** %stackaddr$prim55461, align 8
%argslist54463$f481480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55462 = alloca %struct.ScmObj*, align 8
%argslist54463$f481481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y48147, %struct.ScmObj* %argslist54463$f481480)
store volatile %struct.ScmObj* %argslist54463$f481481, %struct.ScmObj** %stackaddr$prim55462, align 8
%stackaddr$prim55463 = alloca %struct.ScmObj*, align 8
%argslist54463$f481482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x48149, %struct.ScmObj* %argslist54463$f481481)
store volatile %struct.ScmObj* %argslist54463$f481482, %struct.ScmObj** %stackaddr$prim55463, align 8
%stackaddr$prim55464 = alloca %struct.ScmObj*, align 8
%argslist54463$f481483 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48322, %struct.ScmObj* %argslist54463$f481482)
store volatile %struct.ScmObj* %argslist54463$f481483, %struct.ScmObj** %stackaddr$prim55464, align 8
%clofunc55465 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48148)
musttail call tailcc void %clofunc55465(%struct.ScmObj* %f48148, %struct.ScmObj* %argslist54463$f481483)
ret void
}

define tailcc void @proc_clo$ae51949(%struct.ScmObj* %env$ae51949,%struct.ScmObj* %lst4814648323) {
%stackaddr$prim55466 = alloca %struct.ScmObj*, align 8
%k48324 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4814648323)
store volatile %struct.ScmObj* %k48324, %struct.ScmObj** %stackaddr$prim55466, align 8
%stackaddr$prim55467 = alloca %struct.ScmObj*, align 8
%lst48146 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4814648323)
store volatile %struct.ScmObj* %lst48146, %struct.ScmObj** %stackaddr$prim55467, align 8
%ae51953 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54467$k483240 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55468 = alloca %struct.ScmObj*, align 8
%argslist54467$k483241 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48146, %struct.ScmObj* %argslist54467$k483240)
store volatile %struct.ScmObj* %argslist54467$k483241, %struct.ScmObj** %stackaddr$prim55468, align 8
%stackaddr$prim55469 = alloca %struct.ScmObj*, align 8
%argslist54467$k483242 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51953, %struct.ScmObj* %argslist54467$k483241)
store volatile %struct.ScmObj* %argslist54467$k483242, %struct.ScmObj** %stackaddr$prim55469, align 8
%clofunc55470 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48324)
musttail call tailcc void %clofunc55470(%struct.ScmObj* %k48324, %struct.ScmObj* %argslist54467$k483242)
ret void
}

define tailcc void @proc_clo$ae51925(%struct.ScmObj* %env$ae51925,%struct.ScmObj* %args4814548325) {
%stackaddr$prim55471 = alloca %struct.ScmObj*, align 8
%k48326 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4814548325)
store volatile %struct.ScmObj* %k48326, %struct.ScmObj** %stackaddr$prim55471, align 8
%stackaddr$prim55472 = alloca %struct.ScmObj*, align 8
%args48145 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4814548325)
store volatile %struct.ScmObj* %args48145, %struct.ScmObj** %stackaddr$prim55472, align 8
%stackaddr$applyprim55473 = alloca %struct.ScmObj*, align 8
%cpsaprim48327 = call %struct.ScmObj* @applyprim__45(%struct.ScmObj* %args48145)
store volatile %struct.ScmObj* %cpsaprim48327, %struct.ScmObj** %stackaddr$applyprim55473, align 8
%ae51930 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54469$k483260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55474 = alloca %struct.ScmObj*, align 8
%argslist54469$k483261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsaprim48327, %struct.ScmObj* %argslist54469$k483260)
store volatile %struct.ScmObj* %argslist54469$k483261, %struct.ScmObj** %stackaddr$prim55474, align 8
%stackaddr$prim55475 = alloca %struct.ScmObj*, align 8
%argslist54469$k483262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51930, %struct.ScmObj* %argslist54469$k483261)
store volatile %struct.ScmObj* %argslist54469$k483262, %struct.ScmObj** %stackaddr$prim55475, align 8
%clofunc55476 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48326)
musttail call tailcc void %clofunc55476(%struct.ScmObj* %k48326, %struct.ScmObj* %argslist54469$k483262)
ret void
}

define tailcc void @proc_clo$ae51863(%struct.ScmObj* %env$ae51863,%struct.ScmObj* %lst4814448328) {
%stackaddr$prim55477 = alloca %struct.ScmObj*, align 8
%k48329 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4814448328)
store volatile %struct.ScmObj* %k48329, %struct.ScmObj** %stackaddr$prim55477, align 8
%stackaddr$prim55478 = alloca %struct.ScmObj*, align 8
%lst48144 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4814448328)
store volatile %struct.ScmObj* %lst48144, %struct.ScmObj** %stackaddr$prim55478, align 8
%ae51867 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54473$k483290 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55479 = alloca %struct.ScmObj*, align 8
%argslist54473$k483291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48144, %struct.ScmObj* %argslist54473$k483290)
store volatile %struct.ScmObj* %argslist54473$k483291, %struct.ScmObj** %stackaddr$prim55479, align 8
%stackaddr$prim55480 = alloca %struct.ScmObj*, align 8
%argslist54473$k483292 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51867, %struct.ScmObj* %argslist54473$k483291)
store volatile %struct.ScmObj* %argslist54473$k483292, %struct.ScmObj** %stackaddr$prim55480, align 8
%clofunc55481 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48329)
musttail call tailcc void %clofunc55481(%struct.ScmObj* %k48329, %struct.ScmObj* %argslist54473$k483292)
ret void
}

define tailcc void @proc_clo$ae51839(%struct.ScmObj* %env$ae51839,%struct.ScmObj* %args4814348330) {
%stackaddr$prim55482 = alloca %struct.ScmObj*, align 8
%k48331 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4814348330)
store volatile %struct.ScmObj* %k48331, %struct.ScmObj** %stackaddr$prim55482, align 8
%stackaddr$prim55483 = alloca %struct.ScmObj*, align 8
%args48143 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4814348330)
store volatile %struct.ScmObj* %args48143, %struct.ScmObj** %stackaddr$prim55483, align 8
%stackaddr$applyprim55484 = alloca %struct.ScmObj*, align 8
%cpsaprim48332 = call %struct.ScmObj* @applyprim__43(%struct.ScmObj* %args48143)
store volatile %struct.ScmObj* %cpsaprim48332, %struct.ScmObj** %stackaddr$applyprim55484, align 8
%ae51844 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54475$k483310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55485 = alloca %struct.ScmObj*, align 8
%argslist54475$k483311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsaprim48332, %struct.ScmObj* %argslist54475$k483310)
store volatile %struct.ScmObj* %argslist54475$k483311, %struct.ScmObj** %stackaddr$prim55485, align 8
%stackaddr$prim55486 = alloca %struct.ScmObj*, align 8
%argslist54475$k483312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51844, %struct.ScmObj* %argslist54475$k483311)
store volatile %struct.ScmObj* %argslist54475$k483312, %struct.ScmObj** %stackaddr$prim55486, align 8
%clofunc55487 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48331)
musttail call tailcc void %clofunc55487(%struct.ScmObj* %k48331, %struct.ScmObj* %argslist54475$k483312)
ret void
}

define tailcc void @proc_clo$ae51813(%struct.ScmObj* %env$ae51813,%struct.ScmObj* %current_45args54477) {
%stackaddr$prim55488 = alloca %struct.ScmObj*, align 8
%k48333 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54477)
store volatile %struct.ScmObj* %k48333, %struct.ScmObj** %stackaddr$prim55488, align 8
%stackaddr$prim55489 = alloca %struct.ScmObj*, align 8
%current_45args54478 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54477)
store volatile %struct.ScmObj* %current_45args54478, %struct.ScmObj** %stackaddr$prim55489, align 8
%stackaddr$prim55490 = alloca %struct.ScmObj*, align 8
%x48083 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54478)
store volatile %struct.ScmObj* %x48083, %struct.ScmObj** %stackaddr$prim55490, align 8
%stackaddr$prim55491 = alloca %struct.ScmObj*, align 8
%anf_45bind48259 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48083)
store volatile %struct.ScmObj* %anf_45bind48259, %struct.ScmObj** %stackaddr$prim55491, align 8
%stackaddr$prim55492 = alloca %struct.ScmObj*, align 8
%anf_45bind48260 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48259)
store volatile %struct.ScmObj* %anf_45bind48260, %struct.ScmObj** %stackaddr$prim55492, align 8
%stackaddr$prim55493 = alloca %struct.ScmObj*, align 8
%anf_45bind48261 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48260)
store volatile %struct.ScmObj* %anf_45bind48261, %struct.ScmObj** %stackaddr$prim55493, align 8
%stackaddr$prim55494 = alloca %struct.ScmObj*, align 8
%cpsprim48334 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48261)
store volatile %struct.ScmObj* %cpsprim48334, %struct.ScmObj** %stackaddr$prim55494, align 8
%ae51819 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54480$k483330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55495 = alloca %struct.ScmObj*, align 8
%argslist54480$k483331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48334, %struct.ScmObj* %argslist54480$k483330)
store volatile %struct.ScmObj* %argslist54480$k483331, %struct.ScmObj** %stackaddr$prim55495, align 8
%stackaddr$prim55496 = alloca %struct.ScmObj*, align 8
%argslist54480$k483332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51819, %struct.ScmObj* %argslist54480$k483331)
store volatile %struct.ScmObj* %argslist54480$k483332, %struct.ScmObj** %stackaddr$prim55496, align 8
%clofunc55497 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48333)
musttail call tailcc void %clofunc55497(%struct.ScmObj* %k48333, %struct.ScmObj* %argslist54480$k483332)
ret void
}

define tailcc void @proc_clo$ae51789(%struct.ScmObj* %env$ae51789,%struct.ScmObj* %current_45args54482) {
%stackaddr$prim55498 = alloca %struct.ScmObj*, align 8
%k48335 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54482)
store volatile %struct.ScmObj* %k48335, %struct.ScmObj** %stackaddr$prim55498, align 8
%stackaddr$prim55499 = alloca %struct.ScmObj*, align 8
%current_45args54483 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54482)
store volatile %struct.ScmObj* %current_45args54483, %struct.ScmObj** %stackaddr$prim55499, align 8
%stackaddr$prim55500 = alloca %struct.ScmObj*, align 8
%x48085 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54483)
store volatile %struct.ScmObj* %x48085, %struct.ScmObj** %stackaddr$prim55500, align 8
%stackaddr$prim55501 = alloca %struct.ScmObj*, align 8
%anf_45bind48257 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48085)
store volatile %struct.ScmObj* %anf_45bind48257, %struct.ScmObj** %stackaddr$prim55501, align 8
%stackaddr$prim55502 = alloca %struct.ScmObj*, align 8
%anf_45bind48258 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48257)
store volatile %struct.ScmObj* %anf_45bind48258, %struct.ScmObj** %stackaddr$prim55502, align 8
%stackaddr$prim55503 = alloca %struct.ScmObj*, align 8
%cpsprim48336 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48258)
store volatile %struct.ScmObj* %cpsprim48336, %struct.ScmObj** %stackaddr$prim55503, align 8
%ae51794 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54485$k483350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55504 = alloca %struct.ScmObj*, align 8
%argslist54485$k483351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48336, %struct.ScmObj* %argslist54485$k483350)
store volatile %struct.ScmObj* %argslist54485$k483351, %struct.ScmObj** %stackaddr$prim55504, align 8
%stackaddr$prim55505 = alloca %struct.ScmObj*, align 8
%argslist54485$k483352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51794, %struct.ScmObj* %argslist54485$k483351)
store volatile %struct.ScmObj* %argslist54485$k483352, %struct.ScmObj** %stackaddr$prim55505, align 8
%clofunc55506 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48335)
musttail call tailcc void %clofunc55506(%struct.ScmObj* %k48335, %struct.ScmObj* %argslist54485$k483352)
ret void
}

define tailcc void @proc_clo$ae51767(%struct.ScmObj* %env$ae51767,%struct.ScmObj* %current_45args54487) {
%stackaddr$prim55507 = alloca %struct.ScmObj*, align 8
%k48337 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54487)
store volatile %struct.ScmObj* %k48337, %struct.ScmObj** %stackaddr$prim55507, align 8
%stackaddr$prim55508 = alloca %struct.ScmObj*, align 8
%current_45args54488 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54487)
store volatile %struct.ScmObj* %current_45args54488, %struct.ScmObj** %stackaddr$prim55508, align 8
%stackaddr$prim55509 = alloca %struct.ScmObj*, align 8
%x48087 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54488)
store volatile %struct.ScmObj* %x48087, %struct.ScmObj** %stackaddr$prim55509, align 8
%stackaddr$prim55510 = alloca %struct.ScmObj*, align 8
%anf_45bind48256 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48087)
store volatile %struct.ScmObj* %anf_45bind48256, %struct.ScmObj** %stackaddr$prim55510, align 8
%stackaddr$prim55511 = alloca %struct.ScmObj*, align 8
%cpsprim48338 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48256)
store volatile %struct.ScmObj* %cpsprim48338, %struct.ScmObj** %stackaddr$prim55511, align 8
%ae51771 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54490$k483370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55512 = alloca %struct.ScmObj*, align 8
%argslist54490$k483371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48338, %struct.ScmObj* %argslist54490$k483370)
store volatile %struct.ScmObj* %argslist54490$k483371, %struct.ScmObj** %stackaddr$prim55512, align 8
%stackaddr$prim55513 = alloca %struct.ScmObj*, align 8
%argslist54490$k483372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51771, %struct.ScmObj* %argslist54490$k483371)
store volatile %struct.ScmObj* %argslist54490$k483372, %struct.ScmObj** %stackaddr$prim55513, align 8
%clofunc55514 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48337)
musttail call tailcc void %clofunc55514(%struct.ScmObj* %k48337, %struct.ScmObj* %argslist54490$k483372)
ret void
}

define tailcc void @proc_clo$ae51747(%struct.ScmObj* %env$ae51747,%struct.ScmObj* %current_45args54492) {
%stackaddr$prim55515 = alloca %struct.ScmObj*, align 8
%k48339 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54492)
store volatile %struct.ScmObj* %k48339, %struct.ScmObj** %stackaddr$prim55515, align 8
%stackaddr$prim55516 = alloca %struct.ScmObj*, align 8
%current_45args54493 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54492)
store volatile %struct.ScmObj* %current_45args54493, %struct.ScmObj** %stackaddr$prim55516, align 8
%stackaddr$prim55517 = alloca %struct.ScmObj*, align 8
%x48089 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54493)
store volatile %struct.ScmObj* %x48089, %struct.ScmObj** %stackaddr$prim55517, align 8
%stackaddr$prim55518 = alloca %struct.ScmObj*, align 8
%cpsprim48340 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48089)
store volatile %struct.ScmObj* %cpsprim48340, %struct.ScmObj** %stackaddr$prim55518, align 8
%ae51750 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54495$k483390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55519 = alloca %struct.ScmObj*, align 8
%argslist54495$k483391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48340, %struct.ScmObj* %argslist54495$k483390)
store volatile %struct.ScmObj* %argslist54495$k483391, %struct.ScmObj** %stackaddr$prim55519, align 8
%stackaddr$prim55520 = alloca %struct.ScmObj*, align 8
%argslist54495$k483392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51750, %struct.ScmObj* %argslist54495$k483391)
store volatile %struct.ScmObj* %argslist54495$k483392, %struct.ScmObj** %stackaddr$prim55520, align 8
%clofunc55521 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48339)
musttail call tailcc void %clofunc55521(%struct.ScmObj* %k48339, %struct.ScmObj* %argslist54495$k483392)
ret void
}

define tailcc void @proc_clo$ae51649(%struct.ScmObj* %env$ae51649,%struct.ScmObj* %args4809148341) {
%stackaddr$env-ref55522 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51649, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55522
%stackaddr$prim55523 = alloca %struct.ScmObj*, align 8
%k48342 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4809148341)
store volatile %struct.ScmObj* %k48342, %struct.ScmObj** %stackaddr$prim55523, align 8
%stackaddr$prim55524 = alloca %struct.ScmObj*, align 8
%args48091 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4809148341)
store volatile %struct.ScmObj* %args48091, %struct.ScmObj** %stackaddr$prim55524, align 8
%stackaddr$prim55525 = alloca %struct.ScmObj*, align 8
%anf_45bind48250 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args48091)
store volatile %struct.ScmObj* %anf_45bind48250, %struct.ScmObj** %stackaddr$prim55525, align 8
%truthy$cmp55526 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48250)
%cmp$cmp55526 = icmp eq i64 %truthy$cmp55526, 1
br i1 %cmp$cmp55526, label %truebranch$cmp55526, label %falsebranch$cmp55526
truebranch$cmp55526:
%ae51655 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51656 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist54497$k483420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55527 = alloca %struct.ScmObj*, align 8
%argslist54497$k483421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51656, %struct.ScmObj* %argslist54497$k483420)
store volatile %struct.ScmObj* %argslist54497$k483421, %struct.ScmObj** %stackaddr$prim55527, align 8
%stackaddr$prim55528 = alloca %struct.ScmObj*, align 8
%argslist54497$k483422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51655, %struct.ScmObj* %argslist54497$k483421)
store volatile %struct.ScmObj* %argslist54497$k483422, %struct.ScmObj** %stackaddr$prim55528, align 8
%clofunc55529 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48342)
musttail call tailcc void %clofunc55529(%struct.ScmObj* %k48342, %struct.ScmObj* %argslist54497$k483422)
ret void
falsebranch$cmp55526:
%stackaddr$prim55530 = alloca %struct.ScmObj*, align 8
%anf_45bind48251 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48091)
store volatile %struct.ScmObj* %anf_45bind48251, %struct.ScmObj** %stackaddr$prim55530, align 8
%stackaddr$prim55531 = alloca %struct.ScmObj*, align 8
%anf_45bind48252 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48251)
store volatile %struct.ScmObj* %anf_45bind48252, %struct.ScmObj** %stackaddr$prim55531, align 8
%truthy$cmp55532 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48252)
%cmp$cmp55532 = icmp eq i64 %truthy$cmp55532, 1
br i1 %cmp$cmp55532, label %truebranch$cmp55532, label %falsebranch$cmp55532
truebranch$cmp55532:
%stackaddr$prim55533 = alloca %struct.ScmObj*, align 8
%cpsprim48343 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48091)
store volatile %struct.ScmObj* %cpsprim48343, %struct.ScmObj** %stackaddr$prim55533, align 8
%ae51668 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54498$k483420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55534 = alloca %struct.ScmObj*, align 8
%argslist54498$k483421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48343, %struct.ScmObj* %argslist54498$k483420)
store volatile %struct.ScmObj* %argslist54498$k483421, %struct.ScmObj** %stackaddr$prim55534, align 8
%stackaddr$prim55535 = alloca %struct.ScmObj*, align 8
%argslist54498$k483422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51668, %struct.ScmObj* %argslist54498$k483421)
store volatile %struct.ScmObj* %argslist54498$k483422, %struct.ScmObj** %stackaddr$prim55535, align 8
%clofunc55536 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48342)
musttail call tailcc void %clofunc55536(%struct.ScmObj* %k48342, %struct.ScmObj* %argslist54498$k483422)
ret void
falsebranch$cmp55532:
%stackaddr$makeclosure55537 = alloca %struct.ScmObj*, align 8
%fptrToInt55538 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51673 to i64
%ae51673 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55538)
store volatile %struct.ScmObj* %ae51673, %struct.ScmObj** %stackaddr$makeclosure55537, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51673, %struct.ScmObj* %k48342, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51673, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51673, %struct.ScmObj* %args48091, i64 2)
%ae51674 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55539 = alloca %struct.ScmObj*, align 8
%fptrToInt55540 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51675 to i64
%ae51675 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55540)
store volatile %struct.ScmObj* %ae51675, %struct.ScmObj** %stackaddr$makeclosure55539, align 8
%argslist54508$ae516730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55541 = alloca %struct.ScmObj*, align 8
%argslist54508$ae516731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51675, %struct.ScmObj* %argslist54508$ae516730)
store volatile %struct.ScmObj* %argslist54508$ae516731, %struct.ScmObj** %stackaddr$prim55541, align 8
%stackaddr$prim55542 = alloca %struct.ScmObj*, align 8
%argslist54508$ae516732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51674, %struct.ScmObj* %argslist54508$ae516731)
store volatile %struct.ScmObj* %argslist54508$ae516732, %struct.ScmObj** %stackaddr$prim55542, align 8
%clofunc55543 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51673)
musttail call tailcc void %clofunc55543(%struct.ScmObj* %ae51673, %struct.ScmObj* %argslist54508$ae516732)
ret void
}

define tailcc void @proc_clo$ae51673(%struct.ScmObj* %env$ae51673,%struct.ScmObj* %current_45args54499) {
%stackaddr$env-ref55544 = alloca %struct.ScmObj*, align 8
%k48342 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51673, i64 0)
store %struct.ScmObj* %k48342, %struct.ScmObj** %stackaddr$env-ref55544
%stackaddr$env-ref55545 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51673, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55545
%stackaddr$env-ref55546 = alloca %struct.ScmObj*, align 8
%args48091 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51673, i64 2)
store %struct.ScmObj* %args48091, %struct.ScmObj** %stackaddr$env-ref55546
%stackaddr$prim55547 = alloca %struct.ScmObj*, align 8
%_95k48344 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54499)
store volatile %struct.ScmObj* %_95k48344, %struct.ScmObj** %stackaddr$prim55547, align 8
%stackaddr$prim55548 = alloca %struct.ScmObj*, align 8
%current_45args54500 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54499)
store volatile %struct.ScmObj* %current_45args54500, %struct.ScmObj** %stackaddr$prim55548, align 8
%stackaddr$prim55549 = alloca %struct.ScmObj*, align 8
%anf_45bind48253 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54500)
store volatile %struct.ScmObj* %anf_45bind48253, %struct.ScmObj** %stackaddr$prim55549, align 8
%stackaddr$prim55550 = alloca %struct.ScmObj*, align 8
%anf_45bind48254 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48091)
store volatile %struct.ScmObj* %anf_45bind48254, %struct.ScmObj** %stackaddr$prim55550, align 8
%stackaddr$prim55551 = alloca %struct.ScmObj*, align 8
%anf_45bind48255 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48091)
store volatile %struct.ScmObj* %anf_45bind48255, %struct.ScmObj** %stackaddr$prim55551, align 8
%argslist54502$_37foldl1480300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55552 = alloca %struct.ScmObj*, align 8
%argslist54502$_37foldl1480301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48255, %struct.ScmObj* %argslist54502$_37foldl1480300)
store volatile %struct.ScmObj* %argslist54502$_37foldl1480301, %struct.ScmObj** %stackaddr$prim55552, align 8
%stackaddr$prim55553 = alloca %struct.ScmObj*, align 8
%argslist54502$_37foldl1480302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48254, %struct.ScmObj* %argslist54502$_37foldl1480301)
store volatile %struct.ScmObj* %argslist54502$_37foldl1480302, %struct.ScmObj** %stackaddr$prim55553, align 8
%stackaddr$prim55554 = alloca %struct.ScmObj*, align 8
%argslist54502$_37foldl1480303 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48253, %struct.ScmObj* %argslist54502$_37foldl1480302)
store volatile %struct.ScmObj* %argslist54502$_37foldl1480303, %struct.ScmObj** %stackaddr$prim55554, align 8
%stackaddr$prim55555 = alloca %struct.ScmObj*, align 8
%argslist54502$_37foldl1480304 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48342, %struct.ScmObj* %argslist54502$_37foldl1480303)
store volatile %struct.ScmObj* %argslist54502$_37foldl1480304, %struct.ScmObj** %stackaddr$prim55555, align 8
%clofunc55556 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148030)
musttail call tailcc void %clofunc55556(%struct.ScmObj* %_37foldl148030, %struct.ScmObj* %argslist54502$_37foldl1480304)
ret void
}

define tailcc void @proc_clo$ae51675(%struct.ScmObj* %env$ae51675,%struct.ScmObj* %current_45args54503) {
%stackaddr$prim55557 = alloca %struct.ScmObj*, align 8
%k48345 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54503)
store volatile %struct.ScmObj* %k48345, %struct.ScmObj** %stackaddr$prim55557, align 8
%stackaddr$prim55558 = alloca %struct.ScmObj*, align 8
%current_45args54504 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54503)
store volatile %struct.ScmObj* %current_45args54504, %struct.ScmObj** %stackaddr$prim55558, align 8
%stackaddr$prim55559 = alloca %struct.ScmObj*, align 8
%n48093 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54504)
store volatile %struct.ScmObj* %n48093, %struct.ScmObj** %stackaddr$prim55559, align 8
%stackaddr$prim55560 = alloca %struct.ScmObj*, align 8
%current_45args54505 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54504)
store volatile %struct.ScmObj* %current_45args54505, %struct.ScmObj** %stackaddr$prim55560, align 8
%stackaddr$prim55561 = alloca %struct.ScmObj*, align 8
%v48092 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54505)
store volatile %struct.ScmObj* %v48092, %struct.ScmObj** %stackaddr$prim55561, align 8
%stackaddr$prim55562 = alloca %struct.ScmObj*, align 8
%cpsprim48346 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v48092, %struct.ScmObj* %n48093)
store volatile %struct.ScmObj* %cpsprim48346, %struct.ScmObj** %stackaddr$prim55562, align 8
%ae51679 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54507$k483450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55563 = alloca %struct.ScmObj*, align 8
%argslist54507$k483451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48346, %struct.ScmObj* %argslist54507$k483450)
store volatile %struct.ScmObj* %argslist54507$k483451, %struct.ScmObj** %stackaddr$prim55563, align 8
%stackaddr$prim55564 = alloca %struct.ScmObj*, align 8
%argslist54507$k483452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51679, %struct.ScmObj* %argslist54507$k483451)
store volatile %struct.ScmObj* %argslist54507$k483452, %struct.ScmObj** %stackaddr$prim55564, align 8
%clofunc55565 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48345)
musttail call tailcc void %clofunc55565(%struct.ScmObj* %k48345, %struct.ScmObj* %argslist54507$k483452)
ret void
}

define tailcc void @proc_clo$ae51245(%struct.ScmObj* %env$ae51245,%struct.ScmObj* %current_45args54510) {
%stackaddr$prim55566 = alloca %struct.ScmObj*, align 8
%k48347 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54510)
store volatile %struct.ScmObj* %k48347, %struct.ScmObj** %stackaddr$prim55566, align 8
%stackaddr$prim55567 = alloca %struct.ScmObj*, align 8
%current_45args54511 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54510)
store volatile %struct.ScmObj* %current_45args54511, %struct.ScmObj** %stackaddr$prim55567, align 8
%stackaddr$prim55568 = alloca %struct.ScmObj*, align 8
%v48096 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54511)
store volatile %struct.ScmObj* %v48096, %struct.ScmObj** %stackaddr$prim55568, align 8
%stackaddr$prim55569 = alloca %struct.ScmObj*, align 8
%current_45args54512 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54511)
store volatile %struct.ScmObj* %current_45args54512, %struct.ScmObj** %stackaddr$prim55569, align 8
%stackaddr$prim55570 = alloca %struct.ScmObj*, align 8
%lst48095 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54512)
store volatile %struct.ScmObj* %lst48095, %struct.ScmObj** %stackaddr$prim55570, align 8
%ae51246 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55571 = alloca %struct.ScmObj*, align 8
%lst48097 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51246, %struct.ScmObj* %lst48095)
store volatile %struct.ScmObj* %lst48097, %struct.ScmObj** %stackaddr$prim55571, align 8
%stackaddr$makeclosure55572 = alloca %struct.ScmObj*, align 8
%fptrToInt55573 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51248 to i64
%ae51248 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55573)
store volatile %struct.ScmObj* %ae51248, %struct.ScmObj** %stackaddr$makeclosure55572, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51248, %struct.ScmObj* %lst48097, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51248, %struct.ScmObj* %v48096, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51248, %struct.ScmObj* %k48347, i64 2)
%ae51249 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55574 = alloca %struct.ScmObj*, align 8
%fptrToInt55575 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51250 to i64
%ae51250 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55575)
store volatile %struct.ScmObj* %ae51250, %struct.ScmObj** %stackaddr$makeclosure55574, align 8
%argslist54534$ae512480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55576 = alloca %struct.ScmObj*, align 8
%argslist54534$ae512481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51250, %struct.ScmObj* %argslist54534$ae512480)
store volatile %struct.ScmObj* %argslist54534$ae512481, %struct.ScmObj** %stackaddr$prim55576, align 8
%stackaddr$prim55577 = alloca %struct.ScmObj*, align 8
%argslist54534$ae512482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51249, %struct.ScmObj* %argslist54534$ae512481)
store volatile %struct.ScmObj* %argslist54534$ae512482, %struct.ScmObj** %stackaddr$prim55577, align 8
%clofunc55578 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51248)
musttail call tailcc void %clofunc55578(%struct.ScmObj* %ae51248, %struct.ScmObj* %argslist54534$ae512482)
ret void
}

define tailcc void @proc_clo$ae51248(%struct.ScmObj* %env$ae51248,%struct.ScmObj* %current_45args54514) {
%stackaddr$env-ref55579 = alloca %struct.ScmObj*, align 8
%lst48097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51248, i64 0)
store %struct.ScmObj* %lst48097, %struct.ScmObj** %stackaddr$env-ref55579
%stackaddr$env-ref55580 = alloca %struct.ScmObj*, align 8
%v48096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51248, i64 1)
store %struct.ScmObj* %v48096, %struct.ScmObj** %stackaddr$env-ref55580
%stackaddr$env-ref55581 = alloca %struct.ScmObj*, align 8
%k48347 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51248, i64 2)
store %struct.ScmObj* %k48347, %struct.ScmObj** %stackaddr$env-ref55581
%stackaddr$prim55582 = alloca %struct.ScmObj*, align 8
%_95k48348 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54514)
store volatile %struct.ScmObj* %_95k48348, %struct.ScmObj** %stackaddr$prim55582, align 8
%stackaddr$prim55583 = alloca %struct.ScmObj*, align 8
%current_45args54515 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54514)
store volatile %struct.ScmObj* %current_45args54515, %struct.ScmObj** %stackaddr$prim55583, align 8
%stackaddr$prim55584 = alloca %struct.ScmObj*, align 8
%anf_45bind48242 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54515)
store volatile %struct.ScmObj* %anf_45bind48242, %struct.ScmObj** %stackaddr$prim55584, align 8
%stackaddr$makeclosure55585 = alloca %struct.ScmObj*, align 8
%fptrToInt55586 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51264 to i64
%ae51264 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55586)
store volatile %struct.ScmObj* %ae51264, %struct.ScmObj** %stackaddr$makeclosure55585, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51264, %struct.ScmObj* %lst48097, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51264, %struct.ScmObj* %v48096, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51264, %struct.ScmObj* %k48347, i64 2)
%stackaddr$makeclosure55587 = alloca %struct.ScmObj*, align 8
%fptrToInt55588 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51265 to i64
%ae51265 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55588)
store volatile %struct.ScmObj* %ae51265, %struct.ScmObj** %stackaddr$makeclosure55587, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51265, %struct.ScmObj* %lst48097, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51265, %struct.ScmObj* %v48096, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51265, %struct.ScmObj* %k48347, i64 2)
%argslist54529$anf_45bind482420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55589 = alloca %struct.ScmObj*, align 8
%argslist54529$anf_45bind482421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51265, %struct.ScmObj* %argslist54529$anf_45bind482420)
store volatile %struct.ScmObj* %argslist54529$anf_45bind482421, %struct.ScmObj** %stackaddr$prim55589, align 8
%stackaddr$prim55590 = alloca %struct.ScmObj*, align 8
%argslist54529$anf_45bind482422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51264, %struct.ScmObj* %argslist54529$anf_45bind482421)
store volatile %struct.ScmObj* %argslist54529$anf_45bind482422, %struct.ScmObj** %stackaddr$prim55590, align 8
%clofunc55591 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48242)
musttail call tailcc void %clofunc55591(%struct.ScmObj* %anf_45bind48242, %struct.ScmObj* %argslist54529$anf_45bind482422)
ret void
}

define tailcc void @proc_clo$ae51264(%struct.ScmObj* %env$ae51264,%struct.ScmObj* %current_45args54517) {
%stackaddr$env-ref55592 = alloca %struct.ScmObj*, align 8
%lst48097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51264, i64 0)
store %struct.ScmObj* %lst48097, %struct.ScmObj** %stackaddr$env-ref55592
%stackaddr$env-ref55593 = alloca %struct.ScmObj*, align 8
%v48096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51264, i64 1)
store %struct.ScmObj* %v48096, %struct.ScmObj** %stackaddr$env-ref55593
%stackaddr$env-ref55594 = alloca %struct.ScmObj*, align 8
%k48347 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51264, i64 2)
store %struct.ScmObj* %k48347, %struct.ScmObj** %stackaddr$env-ref55594
%stackaddr$prim55595 = alloca %struct.ScmObj*, align 8
%_95k48349 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54517)
store volatile %struct.ScmObj* %_95k48349, %struct.ScmObj** %stackaddr$prim55595, align 8
%stackaddr$prim55596 = alloca %struct.ScmObj*, align 8
%current_45args54518 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54517)
store volatile %struct.ScmObj* %current_45args54518, %struct.ScmObj** %stackaddr$prim55596, align 8
%stackaddr$prim55597 = alloca %struct.ScmObj*, align 8
%cc48098 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54518)
store volatile %struct.ScmObj* %cc48098, %struct.ScmObj** %stackaddr$prim55597, align 8
%ae51373 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55598 = alloca %struct.ScmObj*, align 8
%anf_45bind48243 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51373)
store volatile %struct.ScmObj* %anf_45bind48243, %struct.ScmObj** %stackaddr$prim55598, align 8
%stackaddr$prim55599 = alloca %struct.ScmObj*, align 8
%anf_45bind48244 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48243)
store volatile %struct.ScmObj* %anf_45bind48244, %struct.ScmObj** %stackaddr$prim55599, align 8
%truthy$cmp55600 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48244)
%cmp$cmp55600 = icmp eq i64 %truthy$cmp55600, 1
br i1 %cmp$cmp55600, label %truebranch$cmp55600, label %falsebranch$cmp55600
truebranch$cmp55600:
%ae51377 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51378 = call %struct.ScmObj* @const_init_false()
%argslist54520$k483470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55601 = alloca %struct.ScmObj*, align 8
%argslist54520$k483471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51378, %struct.ScmObj* %argslist54520$k483470)
store volatile %struct.ScmObj* %argslist54520$k483471, %struct.ScmObj** %stackaddr$prim55601, align 8
%stackaddr$prim55602 = alloca %struct.ScmObj*, align 8
%argslist54520$k483472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51377, %struct.ScmObj* %argslist54520$k483471)
store volatile %struct.ScmObj* %argslist54520$k483472, %struct.ScmObj** %stackaddr$prim55602, align 8
%clofunc55603 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48347)
musttail call tailcc void %clofunc55603(%struct.ScmObj* %k48347, %struct.ScmObj* %argslist54520$k483472)
ret void
falsebranch$cmp55600:
%ae51386 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55604 = alloca %struct.ScmObj*, align 8
%anf_45bind48245 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51386)
store volatile %struct.ScmObj* %anf_45bind48245, %struct.ScmObj** %stackaddr$prim55604, align 8
%stackaddr$prim55605 = alloca %struct.ScmObj*, align 8
%anf_45bind48246 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48245)
store volatile %struct.ScmObj* %anf_45bind48246, %struct.ScmObj** %stackaddr$prim55605, align 8
%stackaddr$prim55606 = alloca %struct.ScmObj*, align 8
%anf_45bind48247 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind48246, %struct.ScmObj* %v48096)
store volatile %struct.ScmObj* %anf_45bind48247, %struct.ScmObj** %stackaddr$prim55606, align 8
%truthy$cmp55607 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48247)
%cmp$cmp55607 = icmp eq i64 %truthy$cmp55607, 1
br i1 %cmp$cmp55607, label %truebranch$cmp55607, label %falsebranch$cmp55607
truebranch$cmp55607:
%ae51392 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55608 = alloca %struct.ScmObj*, align 8
%cpsprim48350 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51392)
store volatile %struct.ScmObj* %cpsprim48350, %struct.ScmObj** %stackaddr$prim55608, align 8
%ae51394 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54521$k483470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55609 = alloca %struct.ScmObj*, align 8
%argslist54521$k483471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48350, %struct.ScmObj* %argslist54521$k483470)
store volatile %struct.ScmObj* %argslist54521$k483471, %struct.ScmObj** %stackaddr$prim55609, align 8
%stackaddr$prim55610 = alloca %struct.ScmObj*, align 8
%argslist54521$k483472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51394, %struct.ScmObj* %argslist54521$k483471)
store volatile %struct.ScmObj* %argslist54521$k483472, %struct.ScmObj** %stackaddr$prim55610, align 8
%clofunc55611 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48347)
musttail call tailcc void %clofunc55611(%struct.ScmObj* %k48347, %struct.ScmObj* %argslist54521$k483472)
ret void
falsebranch$cmp55607:
%ae51405 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55612 = alloca %struct.ScmObj*, align 8
%anf_45bind48248 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51405)
store volatile %struct.ScmObj* %anf_45bind48248, %struct.ScmObj** %stackaddr$prim55612, align 8
%stackaddr$prim55613 = alloca %struct.ScmObj*, align 8
%anf_45bind48249 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48248)
store volatile %struct.ScmObj* %anf_45bind48249, %struct.ScmObj** %stackaddr$prim55613, align 8
%ae51408 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55614 = alloca %struct.ScmObj*, align 8
%_95048100 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51408, %struct.ScmObj* %anf_45bind48249)
store volatile %struct.ScmObj* %_95048100, %struct.ScmObj** %stackaddr$prim55614, align 8
%argslist54522$cc480980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55615 = alloca %struct.ScmObj*, align 8
%argslist54522$cc480981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48098, %struct.ScmObj* %argslist54522$cc480980)
store volatile %struct.ScmObj* %argslist54522$cc480981, %struct.ScmObj** %stackaddr$prim55615, align 8
%stackaddr$prim55616 = alloca %struct.ScmObj*, align 8
%argslist54522$cc480982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48347, %struct.ScmObj* %argslist54522$cc480981)
store volatile %struct.ScmObj* %argslist54522$cc480982, %struct.ScmObj** %stackaddr$prim55616, align 8
%clofunc55617 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48098)
musttail call tailcc void %clofunc55617(%struct.ScmObj* %cc48098, %struct.ScmObj* %argslist54522$cc480982)
ret void
}

define tailcc void @proc_clo$ae51265(%struct.ScmObj* %env$ae51265,%struct.ScmObj* %current_45args54523) {
%stackaddr$env-ref55618 = alloca %struct.ScmObj*, align 8
%lst48097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51265, i64 0)
store %struct.ScmObj* %lst48097, %struct.ScmObj** %stackaddr$env-ref55618
%stackaddr$env-ref55619 = alloca %struct.ScmObj*, align 8
%v48096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51265, i64 1)
store %struct.ScmObj* %v48096, %struct.ScmObj** %stackaddr$env-ref55619
%stackaddr$env-ref55620 = alloca %struct.ScmObj*, align 8
%k48347 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51265, i64 2)
store %struct.ScmObj* %k48347, %struct.ScmObj** %stackaddr$env-ref55620
%stackaddr$prim55621 = alloca %struct.ScmObj*, align 8
%_95k48349 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54523)
store volatile %struct.ScmObj* %_95k48349, %struct.ScmObj** %stackaddr$prim55621, align 8
%stackaddr$prim55622 = alloca %struct.ScmObj*, align 8
%current_45args54524 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54523)
store volatile %struct.ScmObj* %current_45args54524, %struct.ScmObj** %stackaddr$prim55622, align 8
%stackaddr$prim55623 = alloca %struct.ScmObj*, align 8
%cc48098 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54524)
store volatile %struct.ScmObj* %cc48098, %struct.ScmObj** %stackaddr$prim55623, align 8
%ae51267 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55624 = alloca %struct.ScmObj*, align 8
%anf_45bind48243 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51267)
store volatile %struct.ScmObj* %anf_45bind48243, %struct.ScmObj** %stackaddr$prim55624, align 8
%stackaddr$prim55625 = alloca %struct.ScmObj*, align 8
%anf_45bind48244 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48243)
store volatile %struct.ScmObj* %anf_45bind48244, %struct.ScmObj** %stackaddr$prim55625, align 8
%truthy$cmp55626 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48244)
%cmp$cmp55626 = icmp eq i64 %truthy$cmp55626, 1
br i1 %cmp$cmp55626, label %truebranch$cmp55626, label %falsebranch$cmp55626
truebranch$cmp55626:
%ae51271 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51272 = call %struct.ScmObj* @const_init_false()
%argslist54526$k483470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55627 = alloca %struct.ScmObj*, align 8
%argslist54526$k483471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51272, %struct.ScmObj* %argslist54526$k483470)
store volatile %struct.ScmObj* %argslist54526$k483471, %struct.ScmObj** %stackaddr$prim55627, align 8
%stackaddr$prim55628 = alloca %struct.ScmObj*, align 8
%argslist54526$k483472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51271, %struct.ScmObj* %argslist54526$k483471)
store volatile %struct.ScmObj* %argslist54526$k483472, %struct.ScmObj** %stackaddr$prim55628, align 8
%clofunc55629 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48347)
musttail call tailcc void %clofunc55629(%struct.ScmObj* %k48347, %struct.ScmObj* %argslist54526$k483472)
ret void
falsebranch$cmp55626:
%ae51280 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55630 = alloca %struct.ScmObj*, align 8
%anf_45bind48245 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51280)
store volatile %struct.ScmObj* %anf_45bind48245, %struct.ScmObj** %stackaddr$prim55630, align 8
%stackaddr$prim55631 = alloca %struct.ScmObj*, align 8
%anf_45bind48246 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48245)
store volatile %struct.ScmObj* %anf_45bind48246, %struct.ScmObj** %stackaddr$prim55631, align 8
%stackaddr$prim55632 = alloca %struct.ScmObj*, align 8
%anf_45bind48247 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind48246, %struct.ScmObj* %v48096)
store volatile %struct.ScmObj* %anf_45bind48247, %struct.ScmObj** %stackaddr$prim55632, align 8
%truthy$cmp55633 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48247)
%cmp$cmp55633 = icmp eq i64 %truthy$cmp55633, 1
br i1 %cmp$cmp55633, label %truebranch$cmp55633, label %falsebranch$cmp55633
truebranch$cmp55633:
%ae51286 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55634 = alloca %struct.ScmObj*, align 8
%cpsprim48350 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51286)
store volatile %struct.ScmObj* %cpsprim48350, %struct.ScmObj** %stackaddr$prim55634, align 8
%ae51288 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54527$k483470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55635 = alloca %struct.ScmObj*, align 8
%argslist54527$k483471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48350, %struct.ScmObj* %argslist54527$k483470)
store volatile %struct.ScmObj* %argslist54527$k483471, %struct.ScmObj** %stackaddr$prim55635, align 8
%stackaddr$prim55636 = alloca %struct.ScmObj*, align 8
%argslist54527$k483472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51288, %struct.ScmObj* %argslist54527$k483471)
store volatile %struct.ScmObj* %argslist54527$k483472, %struct.ScmObj** %stackaddr$prim55636, align 8
%clofunc55637 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48347)
musttail call tailcc void %clofunc55637(%struct.ScmObj* %k48347, %struct.ScmObj* %argslist54527$k483472)
ret void
falsebranch$cmp55633:
%ae51299 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55638 = alloca %struct.ScmObj*, align 8
%anf_45bind48248 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51299)
store volatile %struct.ScmObj* %anf_45bind48248, %struct.ScmObj** %stackaddr$prim55638, align 8
%stackaddr$prim55639 = alloca %struct.ScmObj*, align 8
%anf_45bind48249 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48248)
store volatile %struct.ScmObj* %anf_45bind48249, %struct.ScmObj** %stackaddr$prim55639, align 8
%ae51302 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55640 = alloca %struct.ScmObj*, align 8
%_95048100 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51302, %struct.ScmObj* %anf_45bind48249)
store volatile %struct.ScmObj* %_95048100, %struct.ScmObj** %stackaddr$prim55640, align 8
%argslist54528$cc480980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55641 = alloca %struct.ScmObj*, align 8
%argslist54528$cc480981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48098, %struct.ScmObj* %argslist54528$cc480980)
store volatile %struct.ScmObj* %argslist54528$cc480981, %struct.ScmObj** %stackaddr$prim55641, align 8
%stackaddr$prim55642 = alloca %struct.ScmObj*, align 8
%argslist54528$cc480982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48347, %struct.ScmObj* %argslist54528$cc480981)
store volatile %struct.ScmObj* %argslist54528$cc480982, %struct.ScmObj** %stackaddr$prim55642, align 8
%clofunc55643 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48098)
musttail call tailcc void %clofunc55643(%struct.ScmObj* %cc48098, %struct.ScmObj* %argslist54528$cc480982)
ret void
}

define tailcc void @proc_clo$ae51250(%struct.ScmObj* %env$ae51250,%struct.ScmObj* %current_45args54530) {
%stackaddr$prim55644 = alloca %struct.ScmObj*, align 8
%k48351 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54530)
store volatile %struct.ScmObj* %k48351, %struct.ScmObj** %stackaddr$prim55644, align 8
%stackaddr$prim55645 = alloca %struct.ScmObj*, align 8
%current_45args54531 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54530)
store volatile %struct.ScmObj* %current_45args54531, %struct.ScmObj** %stackaddr$prim55645, align 8
%stackaddr$prim55646 = alloca %struct.ScmObj*, align 8
%u48099 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54531)
store volatile %struct.ScmObj* %u48099, %struct.ScmObj** %stackaddr$prim55646, align 8
%argslist54533$u480990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55647 = alloca %struct.ScmObj*, align 8
%argslist54533$u480991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u48099, %struct.ScmObj* %argslist54533$u480990)
store volatile %struct.ScmObj* %argslist54533$u480991, %struct.ScmObj** %stackaddr$prim55647, align 8
%stackaddr$prim55648 = alloca %struct.ScmObj*, align 8
%argslist54533$u480992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48351, %struct.ScmObj* %argslist54533$u480991)
store volatile %struct.ScmObj* %argslist54533$u480992, %struct.ScmObj** %stackaddr$prim55648, align 8
%clofunc55649 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u48099)
musttail call tailcc void %clofunc55649(%struct.ScmObj* %u48099, %struct.ScmObj* %argslist54533$u480992)
ret void
}

define tailcc void @proc_clo$ae50709(%struct.ScmObj* %env$ae50709,%struct.ScmObj* %current_45args54536) {
%stackaddr$prim55650 = alloca %struct.ScmObj*, align 8
%k48352 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54536)
store volatile %struct.ScmObj* %k48352, %struct.ScmObj** %stackaddr$prim55650, align 8
%stackaddr$prim55651 = alloca %struct.ScmObj*, align 8
%current_45args54537 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54536)
store volatile %struct.ScmObj* %current_45args54537, %struct.ScmObj** %stackaddr$prim55651, align 8
%stackaddr$prim55652 = alloca %struct.ScmObj*, align 8
%lst48103 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54537)
store volatile %struct.ScmObj* %lst48103, %struct.ScmObj** %stackaddr$prim55652, align 8
%stackaddr$prim55653 = alloca %struct.ScmObj*, align 8
%current_45args54538 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54537)
store volatile %struct.ScmObj* %current_45args54538, %struct.ScmObj** %stackaddr$prim55653, align 8
%stackaddr$prim55654 = alloca %struct.ScmObj*, align 8
%n48102 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54538)
store volatile %struct.ScmObj* %n48102, %struct.ScmObj** %stackaddr$prim55654, align 8
%ae50710 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55655 = alloca %struct.ScmObj*, align 8
%n48105 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50710, %struct.ScmObj* %n48102)
store volatile %struct.ScmObj* %n48105, %struct.ScmObj** %stackaddr$prim55655, align 8
%ae50712 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55656 = alloca %struct.ScmObj*, align 8
%lst48104 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50712, %struct.ScmObj* %lst48103)
store volatile %struct.ScmObj* %lst48104, %struct.ScmObj** %stackaddr$prim55656, align 8
%stackaddr$makeclosure55657 = alloca %struct.ScmObj*, align 8
%fptrToInt55658 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50714 to i64
%ae50714 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55658)
store volatile %struct.ScmObj* %ae50714, %struct.ScmObj** %stackaddr$makeclosure55657, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50714, %struct.ScmObj* %n48105, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50714, %struct.ScmObj* %lst48104, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50714, %struct.ScmObj* %k48352, i64 2)
%ae50715 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55659 = alloca %struct.ScmObj*, align 8
%fptrToInt55660 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50716 to i64
%ae50716 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55660)
store volatile %struct.ScmObj* %ae50716, %struct.ScmObj** %stackaddr$makeclosure55659, align 8
%argslist54558$ae507140 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55661 = alloca %struct.ScmObj*, align 8
%argslist54558$ae507141 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50716, %struct.ScmObj* %argslist54558$ae507140)
store volatile %struct.ScmObj* %argslist54558$ae507141, %struct.ScmObj** %stackaddr$prim55661, align 8
%stackaddr$prim55662 = alloca %struct.ScmObj*, align 8
%argslist54558$ae507142 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50715, %struct.ScmObj* %argslist54558$ae507141)
store volatile %struct.ScmObj* %argslist54558$ae507142, %struct.ScmObj** %stackaddr$prim55662, align 8
%clofunc55663 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50714)
musttail call tailcc void %clofunc55663(%struct.ScmObj* %ae50714, %struct.ScmObj* %argslist54558$ae507142)
ret void
}

define tailcc void @proc_clo$ae50714(%struct.ScmObj* %env$ae50714,%struct.ScmObj* %current_45args54540) {
%stackaddr$env-ref55664 = alloca %struct.ScmObj*, align 8
%n48105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50714, i64 0)
store %struct.ScmObj* %n48105, %struct.ScmObj** %stackaddr$env-ref55664
%stackaddr$env-ref55665 = alloca %struct.ScmObj*, align 8
%lst48104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50714, i64 1)
store %struct.ScmObj* %lst48104, %struct.ScmObj** %stackaddr$env-ref55665
%stackaddr$env-ref55666 = alloca %struct.ScmObj*, align 8
%k48352 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50714, i64 2)
store %struct.ScmObj* %k48352, %struct.ScmObj** %stackaddr$env-ref55666
%stackaddr$prim55667 = alloca %struct.ScmObj*, align 8
%_95k48353 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54540)
store volatile %struct.ScmObj* %_95k48353, %struct.ScmObj** %stackaddr$prim55667, align 8
%stackaddr$prim55668 = alloca %struct.ScmObj*, align 8
%current_45args54541 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54540)
store volatile %struct.ScmObj* %current_45args54541, %struct.ScmObj** %stackaddr$prim55668, align 8
%stackaddr$prim55669 = alloca %struct.ScmObj*, align 8
%anf_45bind48235 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54541)
store volatile %struct.ScmObj* %anf_45bind48235, %struct.ScmObj** %stackaddr$prim55669, align 8
%stackaddr$makeclosure55670 = alloca %struct.ScmObj*, align 8
%fptrToInt55671 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50730 to i64
%ae50730 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55671)
store volatile %struct.ScmObj* %ae50730, %struct.ScmObj** %stackaddr$makeclosure55670, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50730, %struct.ScmObj* %n48105, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50730, %struct.ScmObj* %lst48104, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50730, %struct.ScmObj* %k48352, i64 2)
%stackaddr$makeclosure55672 = alloca %struct.ScmObj*, align 8
%fptrToInt55673 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50731 to i64
%ae50731 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55673)
store volatile %struct.ScmObj* %ae50731, %struct.ScmObj** %stackaddr$makeclosure55672, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50731, %struct.ScmObj* %n48105, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50731, %struct.ScmObj* %lst48104, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50731, %struct.ScmObj* %k48352, i64 2)
%argslist54553$anf_45bind482350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55674 = alloca %struct.ScmObj*, align 8
%argslist54553$anf_45bind482351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50731, %struct.ScmObj* %argslist54553$anf_45bind482350)
store volatile %struct.ScmObj* %argslist54553$anf_45bind482351, %struct.ScmObj** %stackaddr$prim55674, align 8
%stackaddr$prim55675 = alloca %struct.ScmObj*, align 8
%argslist54553$anf_45bind482352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50730, %struct.ScmObj* %argslist54553$anf_45bind482351)
store volatile %struct.ScmObj* %argslist54553$anf_45bind482352, %struct.ScmObj** %stackaddr$prim55675, align 8
%clofunc55676 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48235)
musttail call tailcc void %clofunc55676(%struct.ScmObj* %anf_45bind48235, %struct.ScmObj* %argslist54553$anf_45bind482352)
ret void
}

define tailcc void @proc_clo$ae50730(%struct.ScmObj* %env$ae50730,%struct.ScmObj* %current_45args54543) {
%stackaddr$env-ref55677 = alloca %struct.ScmObj*, align 8
%n48105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50730, i64 0)
store %struct.ScmObj* %n48105, %struct.ScmObj** %stackaddr$env-ref55677
%stackaddr$env-ref55678 = alloca %struct.ScmObj*, align 8
%lst48104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50730, i64 1)
store %struct.ScmObj* %lst48104, %struct.ScmObj** %stackaddr$env-ref55678
%stackaddr$env-ref55679 = alloca %struct.ScmObj*, align 8
%k48352 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50730, i64 2)
store %struct.ScmObj* %k48352, %struct.ScmObj** %stackaddr$env-ref55679
%stackaddr$prim55680 = alloca %struct.ScmObj*, align 8
%_95k48354 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54543)
store volatile %struct.ScmObj* %_95k48354, %struct.ScmObj** %stackaddr$prim55680, align 8
%stackaddr$prim55681 = alloca %struct.ScmObj*, align 8
%current_45args54544 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54543)
store volatile %struct.ScmObj* %current_45args54544, %struct.ScmObj** %stackaddr$prim55681, align 8
%stackaddr$prim55682 = alloca %struct.ScmObj*, align 8
%cc48106 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54544)
store volatile %struct.ScmObj* %cc48106, %struct.ScmObj** %stackaddr$prim55682, align 8
%ae50873 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55683 = alloca %struct.ScmObj*, align 8
%anf_45bind48236 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48105, %struct.ScmObj* %ae50873)
store volatile %struct.ScmObj* %anf_45bind48236, %struct.ScmObj** %stackaddr$prim55683, align 8
%ae50874 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55684 = alloca %struct.ScmObj*, align 8
%anf_45bind48237 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae50874, %struct.ScmObj* %anf_45bind48236)
store volatile %struct.ScmObj* %anf_45bind48237, %struct.ScmObj** %stackaddr$prim55684, align 8
%truthy$cmp55685 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48237)
%cmp$cmp55685 = icmp eq i64 %truthy$cmp55685, 1
br i1 %cmp$cmp55685, label %truebranch$cmp55685, label %falsebranch$cmp55685
truebranch$cmp55685:
%ae50878 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55686 = alloca %struct.ScmObj*, align 8
%cpsprim48355 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48104, %struct.ScmObj* %ae50878)
store volatile %struct.ScmObj* %cpsprim48355, %struct.ScmObj** %stackaddr$prim55686, align 8
%ae50880 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54546$k483520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55687 = alloca %struct.ScmObj*, align 8
%argslist54546$k483521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48355, %struct.ScmObj* %argslist54546$k483520)
store volatile %struct.ScmObj* %argslist54546$k483521, %struct.ScmObj** %stackaddr$prim55687, align 8
%stackaddr$prim55688 = alloca %struct.ScmObj*, align 8
%argslist54546$k483522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50880, %struct.ScmObj* %argslist54546$k483521)
store volatile %struct.ScmObj* %argslist54546$k483522, %struct.ScmObj** %stackaddr$prim55688, align 8
%clofunc55689 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48352)
musttail call tailcc void %clofunc55689(%struct.ScmObj* %k48352, %struct.ScmObj* %argslist54546$k483522)
ret void
falsebranch$cmp55685:
%ae50891 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55690 = alloca %struct.ScmObj*, align 8
%anf_45bind48238 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48104, %struct.ScmObj* %ae50891)
store volatile %struct.ScmObj* %anf_45bind48238, %struct.ScmObj** %stackaddr$prim55690, align 8
%stackaddr$prim55691 = alloca %struct.ScmObj*, align 8
%anf_45bind48239 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48238)
store volatile %struct.ScmObj* %anf_45bind48239, %struct.ScmObj** %stackaddr$prim55691, align 8
%ae50894 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55692 = alloca %struct.ScmObj*, align 8
%_95048109 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48104, %struct.ScmObj* %ae50894, %struct.ScmObj* %anf_45bind48239)
store volatile %struct.ScmObj* %_95048109, %struct.ScmObj** %stackaddr$prim55692, align 8
%ae50897 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55693 = alloca %struct.ScmObj*, align 8
%anf_45bind48240 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48105, %struct.ScmObj* %ae50897)
store volatile %struct.ScmObj* %anf_45bind48240, %struct.ScmObj** %stackaddr$prim55693, align 8
%ae50899 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55694 = alloca %struct.ScmObj*, align 8
%anf_45bind48241 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind48240, %struct.ScmObj* %ae50899)
store volatile %struct.ScmObj* %anf_45bind48241, %struct.ScmObj** %stackaddr$prim55694, align 8
%ae50901 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55695 = alloca %struct.ScmObj*, align 8
%_95148108 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n48105, %struct.ScmObj* %ae50901, %struct.ScmObj* %anf_45bind48241)
store volatile %struct.ScmObj* %_95148108, %struct.ScmObj** %stackaddr$prim55695, align 8
%argslist54547$cc481060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55696 = alloca %struct.ScmObj*, align 8
%argslist54547$cc481061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48106, %struct.ScmObj* %argslist54547$cc481060)
store volatile %struct.ScmObj* %argslist54547$cc481061, %struct.ScmObj** %stackaddr$prim55696, align 8
%stackaddr$prim55697 = alloca %struct.ScmObj*, align 8
%argslist54547$cc481062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48352, %struct.ScmObj* %argslist54547$cc481061)
store volatile %struct.ScmObj* %argslist54547$cc481062, %struct.ScmObj** %stackaddr$prim55697, align 8
%clofunc55698 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48106)
musttail call tailcc void %clofunc55698(%struct.ScmObj* %cc48106, %struct.ScmObj* %argslist54547$cc481062)
ret void
}

define tailcc void @proc_clo$ae50731(%struct.ScmObj* %env$ae50731,%struct.ScmObj* %current_45args54548) {
%stackaddr$env-ref55699 = alloca %struct.ScmObj*, align 8
%n48105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50731, i64 0)
store %struct.ScmObj* %n48105, %struct.ScmObj** %stackaddr$env-ref55699
%stackaddr$env-ref55700 = alloca %struct.ScmObj*, align 8
%lst48104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50731, i64 1)
store %struct.ScmObj* %lst48104, %struct.ScmObj** %stackaddr$env-ref55700
%stackaddr$env-ref55701 = alloca %struct.ScmObj*, align 8
%k48352 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50731, i64 2)
store %struct.ScmObj* %k48352, %struct.ScmObj** %stackaddr$env-ref55701
%stackaddr$prim55702 = alloca %struct.ScmObj*, align 8
%_95k48354 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54548)
store volatile %struct.ScmObj* %_95k48354, %struct.ScmObj** %stackaddr$prim55702, align 8
%stackaddr$prim55703 = alloca %struct.ScmObj*, align 8
%current_45args54549 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54548)
store volatile %struct.ScmObj* %current_45args54549, %struct.ScmObj** %stackaddr$prim55703, align 8
%stackaddr$prim55704 = alloca %struct.ScmObj*, align 8
%cc48106 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54549)
store volatile %struct.ScmObj* %cc48106, %struct.ScmObj** %stackaddr$prim55704, align 8
%ae50733 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55705 = alloca %struct.ScmObj*, align 8
%anf_45bind48236 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48105, %struct.ScmObj* %ae50733)
store volatile %struct.ScmObj* %anf_45bind48236, %struct.ScmObj** %stackaddr$prim55705, align 8
%ae50734 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55706 = alloca %struct.ScmObj*, align 8
%anf_45bind48237 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae50734, %struct.ScmObj* %anf_45bind48236)
store volatile %struct.ScmObj* %anf_45bind48237, %struct.ScmObj** %stackaddr$prim55706, align 8
%truthy$cmp55707 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48237)
%cmp$cmp55707 = icmp eq i64 %truthy$cmp55707, 1
br i1 %cmp$cmp55707, label %truebranch$cmp55707, label %falsebranch$cmp55707
truebranch$cmp55707:
%ae50738 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55708 = alloca %struct.ScmObj*, align 8
%cpsprim48355 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48104, %struct.ScmObj* %ae50738)
store volatile %struct.ScmObj* %cpsprim48355, %struct.ScmObj** %stackaddr$prim55708, align 8
%ae50740 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54551$k483520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55709 = alloca %struct.ScmObj*, align 8
%argslist54551$k483521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48355, %struct.ScmObj* %argslist54551$k483520)
store volatile %struct.ScmObj* %argslist54551$k483521, %struct.ScmObj** %stackaddr$prim55709, align 8
%stackaddr$prim55710 = alloca %struct.ScmObj*, align 8
%argslist54551$k483522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50740, %struct.ScmObj* %argslist54551$k483521)
store volatile %struct.ScmObj* %argslist54551$k483522, %struct.ScmObj** %stackaddr$prim55710, align 8
%clofunc55711 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48352)
musttail call tailcc void %clofunc55711(%struct.ScmObj* %k48352, %struct.ScmObj* %argslist54551$k483522)
ret void
falsebranch$cmp55707:
%ae50751 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55712 = alloca %struct.ScmObj*, align 8
%anf_45bind48238 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48104, %struct.ScmObj* %ae50751)
store volatile %struct.ScmObj* %anf_45bind48238, %struct.ScmObj** %stackaddr$prim55712, align 8
%stackaddr$prim55713 = alloca %struct.ScmObj*, align 8
%anf_45bind48239 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48238)
store volatile %struct.ScmObj* %anf_45bind48239, %struct.ScmObj** %stackaddr$prim55713, align 8
%ae50754 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55714 = alloca %struct.ScmObj*, align 8
%_95048109 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48104, %struct.ScmObj* %ae50754, %struct.ScmObj* %anf_45bind48239)
store volatile %struct.ScmObj* %_95048109, %struct.ScmObj** %stackaddr$prim55714, align 8
%ae50757 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55715 = alloca %struct.ScmObj*, align 8
%anf_45bind48240 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48105, %struct.ScmObj* %ae50757)
store volatile %struct.ScmObj* %anf_45bind48240, %struct.ScmObj** %stackaddr$prim55715, align 8
%ae50759 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55716 = alloca %struct.ScmObj*, align 8
%anf_45bind48241 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind48240, %struct.ScmObj* %ae50759)
store volatile %struct.ScmObj* %anf_45bind48241, %struct.ScmObj** %stackaddr$prim55716, align 8
%ae50761 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55717 = alloca %struct.ScmObj*, align 8
%_95148108 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n48105, %struct.ScmObj* %ae50761, %struct.ScmObj* %anf_45bind48241)
store volatile %struct.ScmObj* %_95148108, %struct.ScmObj** %stackaddr$prim55717, align 8
%argslist54552$cc481060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55718 = alloca %struct.ScmObj*, align 8
%argslist54552$cc481061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48106, %struct.ScmObj* %argslist54552$cc481060)
store volatile %struct.ScmObj* %argslist54552$cc481061, %struct.ScmObj** %stackaddr$prim55718, align 8
%stackaddr$prim55719 = alloca %struct.ScmObj*, align 8
%argslist54552$cc481062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48352, %struct.ScmObj* %argslist54552$cc481061)
store volatile %struct.ScmObj* %argslist54552$cc481062, %struct.ScmObj** %stackaddr$prim55719, align 8
%clofunc55720 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48106)
musttail call tailcc void %clofunc55720(%struct.ScmObj* %cc48106, %struct.ScmObj* %argslist54552$cc481062)
ret void
}

define tailcc void @proc_clo$ae50716(%struct.ScmObj* %env$ae50716,%struct.ScmObj* %current_45args54554) {
%stackaddr$prim55721 = alloca %struct.ScmObj*, align 8
%k48356 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54554)
store volatile %struct.ScmObj* %k48356, %struct.ScmObj** %stackaddr$prim55721, align 8
%stackaddr$prim55722 = alloca %struct.ScmObj*, align 8
%current_45args54555 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54554)
store volatile %struct.ScmObj* %current_45args54555, %struct.ScmObj** %stackaddr$prim55722, align 8
%stackaddr$prim55723 = alloca %struct.ScmObj*, align 8
%u48107 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54555)
store volatile %struct.ScmObj* %u48107, %struct.ScmObj** %stackaddr$prim55723, align 8
%argslist54557$u481070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55724 = alloca %struct.ScmObj*, align 8
%argslist54557$u481071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u48107, %struct.ScmObj* %argslist54557$u481070)
store volatile %struct.ScmObj* %argslist54557$u481071, %struct.ScmObj** %stackaddr$prim55724, align 8
%stackaddr$prim55725 = alloca %struct.ScmObj*, align 8
%argslist54557$u481072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48356, %struct.ScmObj* %argslist54557$u481071)
store volatile %struct.ScmObj* %argslist54557$u481072, %struct.ScmObj** %stackaddr$prim55725, align 8
%clofunc55726 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u48107)
musttail call tailcc void %clofunc55726(%struct.ScmObj* %u48107, %struct.ScmObj* %argslist54557$u481072)
ret void
}

define tailcc void @proc_clo$ae50293(%struct.ScmObj* %env$ae50293,%struct.ScmObj* %current_45args54560) {
%stackaddr$prim55727 = alloca %struct.ScmObj*, align 8
%k48357 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54560)
store volatile %struct.ScmObj* %k48357, %struct.ScmObj** %stackaddr$prim55727, align 8
%stackaddr$prim55728 = alloca %struct.ScmObj*, align 8
%current_45args54561 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54560)
store volatile %struct.ScmObj* %current_45args54561, %struct.ScmObj** %stackaddr$prim55728, align 8
%stackaddr$prim55729 = alloca %struct.ScmObj*, align 8
%a48111 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54561)
store volatile %struct.ScmObj* %a48111, %struct.ScmObj** %stackaddr$prim55729, align 8
%ae50294 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55730 = alloca %struct.ScmObj*, align 8
%a48112 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50294, %struct.ScmObj* %a48111)
store volatile %struct.ScmObj* %a48112, %struct.ScmObj** %stackaddr$prim55730, align 8
%stackaddr$makeclosure55731 = alloca %struct.ScmObj*, align 8
%fptrToInt55732 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50296 to i64
%ae50296 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55732)
store volatile %struct.ScmObj* %ae50296, %struct.ScmObj** %stackaddr$makeclosure55731, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50296, %struct.ScmObj* %k48357, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50296, %struct.ScmObj* %a48112, i64 1)
%ae50297 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55733 = alloca %struct.ScmObj*, align 8
%fptrToInt55734 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50298 to i64
%ae50298 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55734)
store volatile %struct.ScmObj* %ae50298, %struct.ScmObj** %stackaddr$makeclosure55733, align 8
%argslist54583$ae502960 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55735 = alloca %struct.ScmObj*, align 8
%argslist54583$ae502961 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50298, %struct.ScmObj* %argslist54583$ae502960)
store volatile %struct.ScmObj* %argslist54583$ae502961, %struct.ScmObj** %stackaddr$prim55735, align 8
%stackaddr$prim55736 = alloca %struct.ScmObj*, align 8
%argslist54583$ae502962 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50297, %struct.ScmObj* %argslist54583$ae502961)
store volatile %struct.ScmObj* %argslist54583$ae502962, %struct.ScmObj** %stackaddr$prim55736, align 8
%clofunc55737 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50296)
musttail call tailcc void %clofunc55737(%struct.ScmObj* %ae50296, %struct.ScmObj* %argslist54583$ae502962)
ret void
}

define tailcc void @proc_clo$ae50296(%struct.ScmObj* %env$ae50296,%struct.ScmObj* %current_45args54563) {
%stackaddr$env-ref55738 = alloca %struct.ScmObj*, align 8
%k48357 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50296, i64 0)
store %struct.ScmObj* %k48357, %struct.ScmObj** %stackaddr$env-ref55738
%stackaddr$env-ref55739 = alloca %struct.ScmObj*, align 8
%a48112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50296, i64 1)
store %struct.ScmObj* %a48112, %struct.ScmObj** %stackaddr$env-ref55739
%stackaddr$prim55740 = alloca %struct.ScmObj*, align 8
%_95k48358 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54563)
store volatile %struct.ScmObj* %_95k48358, %struct.ScmObj** %stackaddr$prim55740, align 8
%stackaddr$prim55741 = alloca %struct.ScmObj*, align 8
%current_45args54564 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54563)
store volatile %struct.ScmObj* %current_45args54564, %struct.ScmObj** %stackaddr$prim55741, align 8
%stackaddr$prim55742 = alloca %struct.ScmObj*, align 8
%anf_45bind48227 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54564)
store volatile %struct.ScmObj* %anf_45bind48227, %struct.ScmObj** %stackaddr$prim55742, align 8
%stackaddr$makeclosure55743 = alloca %struct.ScmObj*, align 8
%fptrToInt55744 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50315 to i64
%ae50315 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55744)
store volatile %struct.ScmObj* %ae50315, %struct.ScmObj** %stackaddr$makeclosure55743, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50315, %struct.ScmObj* %k48357, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50315, %struct.ScmObj* %a48112, i64 1)
%stackaddr$makeclosure55745 = alloca %struct.ScmObj*, align 8
%fptrToInt55746 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50316 to i64
%ae50316 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55746)
store volatile %struct.ScmObj* %ae50316, %struct.ScmObj** %stackaddr$makeclosure55745, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50316, %struct.ScmObj* %k48357, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50316, %struct.ScmObj* %a48112, i64 1)
%argslist54578$anf_45bind482270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55747 = alloca %struct.ScmObj*, align 8
%argslist54578$anf_45bind482271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50316, %struct.ScmObj* %argslist54578$anf_45bind482270)
store volatile %struct.ScmObj* %argslist54578$anf_45bind482271, %struct.ScmObj** %stackaddr$prim55747, align 8
%stackaddr$prim55748 = alloca %struct.ScmObj*, align 8
%argslist54578$anf_45bind482272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50315, %struct.ScmObj* %argslist54578$anf_45bind482271)
store volatile %struct.ScmObj* %argslist54578$anf_45bind482272, %struct.ScmObj** %stackaddr$prim55748, align 8
%clofunc55749 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48227)
musttail call tailcc void %clofunc55749(%struct.ScmObj* %anf_45bind48227, %struct.ScmObj* %argslist54578$anf_45bind482272)
ret void
}

define tailcc void @proc_clo$ae50315(%struct.ScmObj* %env$ae50315,%struct.ScmObj* %current_45args54566) {
%stackaddr$env-ref55750 = alloca %struct.ScmObj*, align 8
%k48357 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50315, i64 0)
store %struct.ScmObj* %k48357, %struct.ScmObj** %stackaddr$env-ref55750
%stackaddr$env-ref55751 = alloca %struct.ScmObj*, align 8
%a48112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50315, i64 1)
store %struct.ScmObj* %a48112, %struct.ScmObj** %stackaddr$env-ref55751
%stackaddr$prim55752 = alloca %struct.ScmObj*, align 8
%_95k48359 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54566)
store volatile %struct.ScmObj* %_95k48359, %struct.ScmObj** %stackaddr$prim55752, align 8
%stackaddr$prim55753 = alloca %struct.ScmObj*, align 8
%current_45args54567 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54566)
store volatile %struct.ScmObj* %current_45args54567, %struct.ScmObj** %stackaddr$prim55753, align 8
%stackaddr$prim55754 = alloca %struct.ScmObj*, align 8
%cc48113 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54567)
store volatile %struct.ScmObj* %cc48113, %struct.ScmObj** %stackaddr$prim55754, align 8
%ae50431 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55755 = alloca %struct.ScmObj*, align 8
%anf_45bind48228 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50431)
store volatile %struct.ScmObj* %anf_45bind48228, %struct.ScmObj** %stackaddr$prim55755, align 8
%stackaddr$prim55756 = alloca %struct.ScmObj*, align 8
%anf_45bind48229 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48228)
store volatile %struct.ScmObj* %anf_45bind48229, %struct.ScmObj** %stackaddr$prim55756, align 8
%truthy$cmp55757 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48229)
%cmp$cmp55757 = icmp eq i64 %truthy$cmp55757, 1
br i1 %cmp$cmp55757, label %truebranch$cmp55757, label %falsebranch$cmp55757
truebranch$cmp55757:
%ae50435 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50436 = call %struct.ScmObj* @const_init_true()
%argslist54569$k483570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55758 = alloca %struct.ScmObj*, align 8
%argslist54569$k483571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50436, %struct.ScmObj* %argslist54569$k483570)
store volatile %struct.ScmObj* %argslist54569$k483571, %struct.ScmObj** %stackaddr$prim55758, align 8
%stackaddr$prim55759 = alloca %struct.ScmObj*, align 8
%argslist54569$k483572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50435, %struct.ScmObj* %argslist54569$k483571)
store volatile %struct.ScmObj* %argslist54569$k483572, %struct.ScmObj** %stackaddr$prim55759, align 8
%clofunc55760 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48357)
musttail call tailcc void %clofunc55760(%struct.ScmObj* %k48357, %struct.ScmObj* %argslist54569$k483572)
ret void
falsebranch$cmp55757:
%ae50444 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55761 = alloca %struct.ScmObj*, align 8
%anf_45bind48230 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50444)
store volatile %struct.ScmObj* %anf_45bind48230, %struct.ScmObj** %stackaddr$prim55761, align 8
%stackaddr$prim55762 = alloca %struct.ScmObj*, align 8
%anf_45bind48231 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind48230)
store volatile %struct.ScmObj* %anf_45bind48231, %struct.ScmObj** %stackaddr$prim55762, align 8
%truthy$cmp55763 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48231)
%cmp$cmp55763 = icmp eq i64 %truthy$cmp55763, 1
br i1 %cmp$cmp55763, label %truebranch$cmp55763, label %falsebranch$cmp55763
truebranch$cmp55763:
%ae50448 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55764 = alloca %struct.ScmObj*, align 8
%anf_45bind48232 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50448)
store volatile %struct.ScmObj* %anf_45bind48232, %struct.ScmObj** %stackaddr$prim55764, align 8
%stackaddr$prim55765 = alloca %struct.ScmObj*, align 8
%b48115 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48232)
store volatile %struct.ScmObj* %b48115, %struct.ScmObj** %stackaddr$prim55765, align 8
%ae50451 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55766 = alloca %struct.ScmObj*, align 8
%anf_45bind48233 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50451)
store volatile %struct.ScmObj* %anf_45bind48233, %struct.ScmObj** %stackaddr$prim55766, align 8
%stackaddr$prim55767 = alloca %struct.ScmObj*, align 8
%anf_45bind48234 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48233)
store volatile %struct.ScmObj* %anf_45bind48234, %struct.ScmObj** %stackaddr$prim55767, align 8
%ae50454 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55768 = alloca %struct.ScmObj*, align 8
%_95048116 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50454, %struct.ScmObj* %anf_45bind48234)
store volatile %struct.ScmObj* %_95048116, %struct.ScmObj** %stackaddr$prim55768, align 8
%argslist54570$cc481130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55769 = alloca %struct.ScmObj*, align 8
%argslist54570$cc481131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48113, %struct.ScmObj* %argslist54570$cc481130)
store volatile %struct.ScmObj* %argslist54570$cc481131, %struct.ScmObj** %stackaddr$prim55769, align 8
%stackaddr$prim55770 = alloca %struct.ScmObj*, align 8
%argslist54570$cc481132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48357, %struct.ScmObj* %argslist54570$cc481131)
store volatile %struct.ScmObj* %argslist54570$cc481132, %struct.ScmObj** %stackaddr$prim55770, align 8
%clofunc55771 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48113)
musttail call tailcc void %clofunc55771(%struct.ScmObj* %cc48113, %struct.ScmObj* %argslist54570$cc481132)
ret void
falsebranch$cmp55763:
%ae50487 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50488 = call %struct.ScmObj* @const_init_false()
%argslist54571$k483570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55772 = alloca %struct.ScmObj*, align 8
%argslist54571$k483571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50488, %struct.ScmObj* %argslist54571$k483570)
store volatile %struct.ScmObj* %argslist54571$k483571, %struct.ScmObj** %stackaddr$prim55772, align 8
%stackaddr$prim55773 = alloca %struct.ScmObj*, align 8
%argslist54571$k483572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50487, %struct.ScmObj* %argslist54571$k483571)
store volatile %struct.ScmObj* %argslist54571$k483572, %struct.ScmObj** %stackaddr$prim55773, align 8
%clofunc55774 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48357)
musttail call tailcc void %clofunc55774(%struct.ScmObj* %k48357, %struct.ScmObj* %argslist54571$k483572)
ret void
}

define tailcc void @proc_clo$ae50316(%struct.ScmObj* %env$ae50316,%struct.ScmObj* %current_45args54572) {
%stackaddr$env-ref55775 = alloca %struct.ScmObj*, align 8
%k48357 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50316, i64 0)
store %struct.ScmObj* %k48357, %struct.ScmObj** %stackaddr$env-ref55775
%stackaddr$env-ref55776 = alloca %struct.ScmObj*, align 8
%a48112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50316, i64 1)
store %struct.ScmObj* %a48112, %struct.ScmObj** %stackaddr$env-ref55776
%stackaddr$prim55777 = alloca %struct.ScmObj*, align 8
%_95k48359 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54572)
store volatile %struct.ScmObj* %_95k48359, %struct.ScmObj** %stackaddr$prim55777, align 8
%stackaddr$prim55778 = alloca %struct.ScmObj*, align 8
%current_45args54573 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54572)
store volatile %struct.ScmObj* %current_45args54573, %struct.ScmObj** %stackaddr$prim55778, align 8
%stackaddr$prim55779 = alloca %struct.ScmObj*, align 8
%cc48113 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54573)
store volatile %struct.ScmObj* %cc48113, %struct.ScmObj** %stackaddr$prim55779, align 8
%ae50318 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55780 = alloca %struct.ScmObj*, align 8
%anf_45bind48228 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50318)
store volatile %struct.ScmObj* %anf_45bind48228, %struct.ScmObj** %stackaddr$prim55780, align 8
%stackaddr$prim55781 = alloca %struct.ScmObj*, align 8
%anf_45bind48229 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48228)
store volatile %struct.ScmObj* %anf_45bind48229, %struct.ScmObj** %stackaddr$prim55781, align 8
%truthy$cmp55782 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48229)
%cmp$cmp55782 = icmp eq i64 %truthy$cmp55782, 1
br i1 %cmp$cmp55782, label %truebranch$cmp55782, label %falsebranch$cmp55782
truebranch$cmp55782:
%ae50322 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50323 = call %struct.ScmObj* @const_init_true()
%argslist54575$k483570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55783 = alloca %struct.ScmObj*, align 8
%argslist54575$k483571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50323, %struct.ScmObj* %argslist54575$k483570)
store volatile %struct.ScmObj* %argslist54575$k483571, %struct.ScmObj** %stackaddr$prim55783, align 8
%stackaddr$prim55784 = alloca %struct.ScmObj*, align 8
%argslist54575$k483572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50322, %struct.ScmObj* %argslist54575$k483571)
store volatile %struct.ScmObj* %argslist54575$k483572, %struct.ScmObj** %stackaddr$prim55784, align 8
%clofunc55785 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48357)
musttail call tailcc void %clofunc55785(%struct.ScmObj* %k48357, %struct.ScmObj* %argslist54575$k483572)
ret void
falsebranch$cmp55782:
%ae50331 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55786 = alloca %struct.ScmObj*, align 8
%anf_45bind48230 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50331)
store volatile %struct.ScmObj* %anf_45bind48230, %struct.ScmObj** %stackaddr$prim55786, align 8
%stackaddr$prim55787 = alloca %struct.ScmObj*, align 8
%anf_45bind48231 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind48230)
store volatile %struct.ScmObj* %anf_45bind48231, %struct.ScmObj** %stackaddr$prim55787, align 8
%truthy$cmp55788 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48231)
%cmp$cmp55788 = icmp eq i64 %truthy$cmp55788, 1
br i1 %cmp$cmp55788, label %truebranch$cmp55788, label %falsebranch$cmp55788
truebranch$cmp55788:
%ae50335 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55789 = alloca %struct.ScmObj*, align 8
%anf_45bind48232 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50335)
store volatile %struct.ScmObj* %anf_45bind48232, %struct.ScmObj** %stackaddr$prim55789, align 8
%stackaddr$prim55790 = alloca %struct.ScmObj*, align 8
%b48115 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48232)
store volatile %struct.ScmObj* %b48115, %struct.ScmObj** %stackaddr$prim55790, align 8
%ae50338 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55791 = alloca %struct.ScmObj*, align 8
%anf_45bind48233 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50338)
store volatile %struct.ScmObj* %anf_45bind48233, %struct.ScmObj** %stackaddr$prim55791, align 8
%stackaddr$prim55792 = alloca %struct.ScmObj*, align 8
%anf_45bind48234 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48233)
store volatile %struct.ScmObj* %anf_45bind48234, %struct.ScmObj** %stackaddr$prim55792, align 8
%ae50341 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55793 = alloca %struct.ScmObj*, align 8
%_95048116 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50341, %struct.ScmObj* %anf_45bind48234)
store volatile %struct.ScmObj* %_95048116, %struct.ScmObj** %stackaddr$prim55793, align 8
%argslist54576$cc481130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55794 = alloca %struct.ScmObj*, align 8
%argslist54576$cc481131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48113, %struct.ScmObj* %argslist54576$cc481130)
store volatile %struct.ScmObj* %argslist54576$cc481131, %struct.ScmObj** %stackaddr$prim55794, align 8
%stackaddr$prim55795 = alloca %struct.ScmObj*, align 8
%argslist54576$cc481132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48357, %struct.ScmObj* %argslist54576$cc481131)
store volatile %struct.ScmObj* %argslist54576$cc481132, %struct.ScmObj** %stackaddr$prim55795, align 8
%clofunc55796 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48113)
musttail call tailcc void %clofunc55796(%struct.ScmObj* %cc48113, %struct.ScmObj* %argslist54576$cc481132)
ret void
falsebranch$cmp55788:
%ae50374 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50375 = call %struct.ScmObj* @const_init_false()
%argslist54577$k483570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55797 = alloca %struct.ScmObj*, align 8
%argslist54577$k483571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50375, %struct.ScmObj* %argslist54577$k483570)
store volatile %struct.ScmObj* %argslist54577$k483571, %struct.ScmObj** %stackaddr$prim55797, align 8
%stackaddr$prim55798 = alloca %struct.ScmObj*, align 8
%argslist54577$k483572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50374, %struct.ScmObj* %argslist54577$k483571)
store volatile %struct.ScmObj* %argslist54577$k483572, %struct.ScmObj** %stackaddr$prim55798, align 8
%clofunc55799 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48357)
musttail call tailcc void %clofunc55799(%struct.ScmObj* %k48357, %struct.ScmObj* %argslist54577$k483572)
ret void
}

define tailcc void @proc_clo$ae50298(%struct.ScmObj* %env$ae50298,%struct.ScmObj* %current_45args54579) {
%stackaddr$prim55800 = alloca %struct.ScmObj*, align 8
%k48360 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54579)
store volatile %struct.ScmObj* %k48360, %struct.ScmObj** %stackaddr$prim55800, align 8
%stackaddr$prim55801 = alloca %struct.ScmObj*, align 8
%current_45args54580 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54579)
store volatile %struct.ScmObj* %current_45args54580, %struct.ScmObj** %stackaddr$prim55801, align 8
%stackaddr$prim55802 = alloca %struct.ScmObj*, align 8
%k48114 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54580)
store volatile %struct.ScmObj* %k48114, %struct.ScmObj** %stackaddr$prim55802, align 8
%ae50300 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54582$k483600 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55803 = alloca %struct.ScmObj*, align 8
%argslist54582$k483601 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48114, %struct.ScmObj* %argslist54582$k483600)
store volatile %struct.ScmObj* %argslist54582$k483601, %struct.ScmObj** %stackaddr$prim55803, align 8
%stackaddr$prim55804 = alloca %struct.ScmObj*, align 8
%argslist54582$k483602 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50300, %struct.ScmObj* %argslist54582$k483601)
store volatile %struct.ScmObj* %argslist54582$k483602, %struct.ScmObj** %stackaddr$prim55804, align 8
%clofunc55805 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48360)
musttail call tailcc void %clofunc55805(%struct.ScmObj* %k48360, %struct.ScmObj* %argslist54582$k483602)
ret void
}

define tailcc void @proc_clo$ae50221(%struct.ScmObj* %env$ae50221,%struct.ScmObj* %current_45args54585) {
%stackaddr$env-ref55806 = alloca %struct.ScmObj*, align 8
%_37append48118 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50221, i64 0)
store %struct.ScmObj* %_37append48118, %struct.ScmObj** %stackaddr$env-ref55806
%stackaddr$prim55807 = alloca %struct.ScmObj*, align 8
%k48361 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54585)
store volatile %struct.ScmObj* %k48361, %struct.ScmObj** %stackaddr$prim55807, align 8
%stackaddr$prim55808 = alloca %struct.ScmObj*, align 8
%current_45args54586 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54585)
store volatile %struct.ScmObj* %current_45args54586, %struct.ScmObj** %stackaddr$prim55808, align 8
%stackaddr$prim55809 = alloca %struct.ScmObj*, align 8
%ls048121 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54586)
store volatile %struct.ScmObj* %ls048121, %struct.ScmObj** %stackaddr$prim55809, align 8
%stackaddr$prim55810 = alloca %struct.ScmObj*, align 8
%current_45args54587 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54586)
store volatile %struct.ScmObj* %current_45args54587, %struct.ScmObj** %stackaddr$prim55810, align 8
%stackaddr$prim55811 = alloca %struct.ScmObj*, align 8
%ls148120 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54587)
store volatile %struct.ScmObj* %ls148120, %struct.ScmObj** %stackaddr$prim55811, align 8
%stackaddr$prim55812 = alloca %struct.ScmObj*, align 8
%anf_45bind48221 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls048121)
store volatile %struct.ScmObj* %anf_45bind48221, %struct.ScmObj** %stackaddr$prim55812, align 8
%truthy$cmp55813 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48221)
%cmp$cmp55813 = icmp eq i64 %truthy$cmp55813, 1
br i1 %cmp$cmp55813, label %truebranch$cmp55813, label %falsebranch$cmp55813
truebranch$cmp55813:
%ae50225 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54589$k483610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55814 = alloca %struct.ScmObj*, align 8
%argslist54589$k483611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls148120, %struct.ScmObj* %argslist54589$k483610)
store volatile %struct.ScmObj* %argslist54589$k483611, %struct.ScmObj** %stackaddr$prim55814, align 8
%stackaddr$prim55815 = alloca %struct.ScmObj*, align 8
%argslist54589$k483612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50225, %struct.ScmObj* %argslist54589$k483611)
store volatile %struct.ScmObj* %argslist54589$k483612, %struct.ScmObj** %stackaddr$prim55815, align 8
%clofunc55816 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48361)
musttail call tailcc void %clofunc55816(%struct.ScmObj* %k48361, %struct.ScmObj* %argslist54589$k483612)
ret void
falsebranch$cmp55813:
%stackaddr$prim55817 = alloca %struct.ScmObj*, align 8
%anf_45bind48222 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls048121)
store volatile %struct.ScmObj* %anf_45bind48222, %struct.ScmObj** %stackaddr$prim55817, align 8
%ae50232 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55818 = alloca %struct.ScmObj*, align 8
%anf_45bind48223 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append48118, %struct.ScmObj* %ae50232)
store volatile %struct.ScmObj* %anf_45bind48223, %struct.ScmObj** %stackaddr$prim55818, align 8
%stackaddr$prim55819 = alloca %struct.ScmObj*, align 8
%anf_45bind48224 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls048121)
store volatile %struct.ScmObj* %anf_45bind48224, %struct.ScmObj** %stackaddr$prim55819, align 8
%stackaddr$makeclosure55820 = alloca %struct.ScmObj*, align 8
%fptrToInt55821 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50235 to i64
%ae50235 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55821)
store volatile %struct.ScmObj* %ae50235, %struct.ScmObj** %stackaddr$makeclosure55820, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50235, %struct.ScmObj* %k48361, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50235, %struct.ScmObj* %anf_45bind48222, i64 1)
%argslist54594$anf_45bind482230 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55822 = alloca %struct.ScmObj*, align 8
%argslist54594$anf_45bind482231 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls148120, %struct.ScmObj* %argslist54594$anf_45bind482230)
store volatile %struct.ScmObj* %argslist54594$anf_45bind482231, %struct.ScmObj** %stackaddr$prim55822, align 8
%stackaddr$prim55823 = alloca %struct.ScmObj*, align 8
%argslist54594$anf_45bind482232 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48224, %struct.ScmObj* %argslist54594$anf_45bind482231)
store volatile %struct.ScmObj* %argslist54594$anf_45bind482232, %struct.ScmObj** %stackaddr$prim55823, align 8
%stackaddr$prim55824 = alloca %struct.ScmObj*, align 8
%argslist54594$anf_45bind482233 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50235, %struct.ScmObj* %argslist54594$anf_45bind482232)
store volatile %struct.ScmObj* %argslist54594$anf_45bind482233, %struct.ScmObj** %stackaddr$prim55824, align 8
%clofunc55825 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48223)
musttail call tailcc void %clofunc55825(%struct.ScmObj* %anf_45bind48223, %struct.ScmObj* %argslist54594$anf_45bind482233)
ret void
}

define tailcc void @proc_clo$ae50235(%struct.ScmObj* %env$ae50235,%struct.ScmObj* %current_45args54590) {
%stackaddr$env-ref55826 = alloca %struct.ScmObj*, align 8
%k48361 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50235, i64 0)
store %struct.ScmObj* %k48361, %struct.ScmObj** %stackaddr$env-ref55826
%stackaddr$env-ref55827 = alloca %struct.ScmObj*, align 8
%anf_45bind48222 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50235, i64 1)
store %struct.ScmObj* %anf_45bind48222, %struct.ScmObj** %stackaddr$env-ref55827
%stackaddr$prim55828 = alloca %struct.ScmObj*, align 8
%_95k48362 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54590)
store volatile %struct.ScmObj* %_95k48362, %struct.ScmObj** %stackaddr$prim55828, align 8
%stackaddr$prim55829 = alloca %struct.ScmObj*, align 8
%current_45args54591 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54590)
store volatile %struct.ScmObj* %current_45args54591, %struct.ScmObj** %stackaddr$prim55829, align 8
%stackaddr$prim55830 = alloca %struct.ScmObj*, align 8
%anf_45bind48225 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54591)
store volatile %struct.ScmObj* %anf_45bind48225, %struct.ScmObj** %stackaddr$prim55830, align 8
%stackaddr$prim55831 = alloca %struct.ScmObj*, align 8
%cpsprim48363 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48222, %struct.ScmObj* %anf_45bind48225)
store volatile %struct.ScmObj* %cpsprim48363, %struct.ScmObj** %stackaddr$prim55831, align 8
%ae50241 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54593$k483610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55832 = alloca %struct.ScmObj*, align 8
%argslist54593$k483611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48363, %struct.ScmObj* %argslist54593$k483610)
store volatile %struct.ScmObj* %argslist54593$k483611, %struct.ScmObj** %stackaddr$prim55832, align 8
%stackaddr$prim55833 = alloca %struct.ScmObj*, align 8
%argslist54593$k483612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50241, %struct.ScmObj* %argslist54593$k483611)
store volatile %struct.ScmObj* %argslist54593$k483612, %struct.ScmObj** %stackaddr$prim55833, align 8
%clofunc55834 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48361)
musttail call tailcc void %clofunc55834(%struct.ScmObj* %k48361, %struct.ScmObj* %argslist54593$k483612)
ret void
}

define tailcc void @proc_clo$ae50195(%struct.ScmObj* %env$ae50195,%struct.ScmObj* %current_45args54596) {
%stackaddr$prim55835 = alloca %struct.ScmObj*, align 8
%k48364 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54596)
store volatile %struct.ScmObj* %k48364, %struct.ScmObj** %stackaddr$prim55835, align 8
%stackaddr$prim55836 = alloca %struct.ScmObj*, align 8
%current_45args54597 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54596)
store volatile %struct.ScmObj* %current_45args54597, %struct.ScmObj** %stackaddr$prim55836, align 8
%stackaddr$prim55837 = alloca %struct.ScmObj*, align 8
%a48124 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54597)
store volatile %struct.ScmObj* %a48124, %struct.ScmObj** %stackaddr$prim55837, align 8
%stackaddr$prim55838 = alloca %struct.ScmObj*, align 8
%current_45args54598 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54597)
store volatile %struct.ScmObj* %current_45args54598, %struct.ScmObj** %stackaddr$prim55838, align 8
%stackaddr$prim55839 = alloca %struct.ScmObj*, align 8
%b48123 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54598)
store volatile %struct.ScmObj* %b48123, %struct.ScmObj** %stackaddr$prim55839, align 8
%stackaddr$prim55840 = alloca %struct.ScmObj*, align 8
%anf_45bind48220 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a48124, %struct.ScmObj* %b48123)
store volatile %struct.ScmObj* %anf_45bind48220, %struct.ScmObj** %stackaddr$prim55840, align 8
%stackaddr$prim55841 = alloca %struct.ScmObj*, align 8
%cpsprim48365 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind48220)
store volatile %struct.ScmObj* %cpsprim48365, %struct.ScmObj** %stackaddr$prim55841, align 8
%ae50200 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54600$k483640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55842 = alloca %struct.ScmObj*, align 8
%argslist54600$k483641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48365, %struct.ScmObj* %argslist54600$k483640)
store volatile %struct.ScmObj* %argslist54600$k483641, %struct.ScmObj** %stackaddr$prim55842, align 8
%stackaddr$prim55843 = alloca %struct.ScmObj*, align 8
%argslist54600$k483642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50200, %struct.ScmObj* %argslist54600$k483641)
store volatile %struct.ScmObj* %argslist54600$k483642, %struct.ScmObj** %stackaddr$prim55843, align 8
%clofunc55844 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48364)
musttail call tailcc void %clofunc55844(%struct.ScmObj* %k48364, %struct.ScmObj* %argslist54600$k483642)
ret void
}

define tailcc void @proc_clo$ae50171(%struct.ScmObj* %env$ae50171,%struct.ScmObj* %current_45args54602) {
%stackaddr$prim55845 = alloca %struct.ScmObj*, align 8
%k48366 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54602)
store volatile %struct.ScmObj* %k48366, %struct.ScmObj** %stackaddr$prim55845, align 8
%stackaddr$prim55846 = alloca %struct.ScmObj*, align 8
%current_45args54603 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54602)
store volatile %struct.ScmObj* %current_45args54603, %struct.ScmObj** %stackaddr$prim55846, align 8
%stackaddr$prim55847 = alloca %struct.ScmObj*, align 8
%a48127 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54603)
store volatile %struct.ScmObj* %a48127, %struct.ScmObj** %stackaddr$prim55847, align 8
%stackaddr$prim55848 = alloca %struct.ScmObj*, align 8
%current_45args54604 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54603)
store volatile %struct.ScmObj* %current_45args54604, %struct.ScmObj** %stackaddr$prim55848, align 8
%stackaddr$prim55849 = alloca %struct.ScmObj*, align 8
%b48126 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54604)
store volatile %struct.ScmObj* %b48126, %struct.ScmObj** %stackaddr$prim55849, align 8
%stackaddr$prim55850 = alloca %struct.ScmObj*, align 8
%anf_45bind48219 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a48127, %struct.ScmObj* %b48126)
store volatile %struct.ScmObj* %anf_45bind48219, %struct.ScmObj** %stackaddr$prim55850, align 8
%stackaddr$prim55851 = alloca %struct.ScmObj*, align 8
%cpsprim48367 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind48219)
store volatile %struct.ScmObj* %cpsprim48367, %struct.ScmObj** %stackaddr$prim55851, align 8
%ae50176 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54606$k483660 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55852 = alloca %struct.ScmObj*, align 8
%argslist54606$k483661 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48367, %struct.ScmObj* %argslist54606$k483660)
store volatile %struct.ScmObj* %argslist54606$k483661, %struct.ScmObj** %stackaddr$prim55852, align 8
%stackaddr$prim55853 = alloca %struct.ScmObj*, align 8
%argslist54606$k483662 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50176, %struct.ScmObj* %argslist54606$k483661)
store volatile %struct.ScmObj* %argslist54606$k483662, %struct.ScmObj** %stackaddr$prim55853, align 8
%clofunc55854 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48366)
musttail call tailcc void %clofunc55854(%struct.ScmObj* %k48366, %struct.ScmObj* %argslist54606$k483662)
ret void
}

define tailcc void @proc_clo$ae49777(%struct.ScmObj* %env$ae49777,%struct.ScmObj* %current_45args54609) {
%stackaddr$env-ref55855 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49777, i64 0)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55855
%stackaddr$env-ref55856 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49777, i64 1)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55856
%stackaddr$env-ref55857 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49777, i64 2)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55857
%stackaddr$prim55858 = alloca %struct.ScmObj*, align 8
%k48368 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54609)
store volatile %struct.ScmObj* %k48368, %struct.ScmObj** %stackaddr$prim55858, align 8
%stackaddr$prim55859 = alloca %struct.ScmObj*, align 8
%current_45args54610 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54609)
store volatile %struct.ScmObj* %current_45args54610, %struct.ScmObj** %stackaddr$prim55859, align 8
%stackaddr$prim55860 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54610)
store volatile %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$prim55860, align 8
%ae49779 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55861 = alloca %struct.ScmObj*, align 8
%fptrToInt55862 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49780 to i64
%ae49780 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55862)
store volatile %struct.ScmObj* %ae49780, %struct.ScmObj** %stackaddr$makeclosure55861, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49780, %struct.ScmObj* %_37foldr48051, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49780, %struct.ScmObj* %_37foldl48129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49780, %struct.ScmObj* %_37foldr148046, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49780, %struct.ScmObj* %_37map148077, i64 3)
%argslist54667$k483680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55863 = alloca %struct.ScmObj*, align 8
%argslist54667$k483681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49780, %struct.ScmObj* %argslist54667$k483680)
store volatile %struct.ScmObj* %argslist54667$k483681, %struct.ScmObj** %stackaddr$prim55863, align 8
%stackaddr$prim55864 = alloca %struct.ScmObj*, align 8
%argslist54667$k483682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49779, %struct.ScmObj* %argslist54667$k483681)
store volatile %struct.ScmObj* %argslist54667$k483682, %struct.ScmObj** %stackaddr$prim55864, align 8
%clofunc55865 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48368)
musttail call tailcc void %clofunc55865(%struct.ScmObj* %k48368, %struct.ScmObj* %argslist54667$k483682)
ret void
}

define tailcc void @proc_clo$ae49780(%struct.ScmObj* %env$ae49780,%struct.ScmObj* %args4813048369) {
%stackaddr$env-ref55866 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49780, i64 0)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55866
%stackaddr$env-ref55867 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49780, i64 1)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55867
%stackaddr$env-ref55868 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49780, i64 2)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55868
%stackaddr$env-ref55869 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49780, i64 3)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55869
%stackaddr$prim55870 = alloca %struct.ScmObj*, align 8
%k48370 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4813048369)
store volatile %struct.ScmObj* %k48370, %struct.ScmObj** %stackaddr$prim55870, align 8
%stackaddr$prim55871 = alloca %struct.ScmObj*, align 8
%args48130 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4813048369)
store volatile %struct.ScmObj* %args48130, %struct.ScmObj** %stackaddr$prim55871, align 8
%stackaddr$prim55872 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48130)
store volatile %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$prim55872, align 8
%stackaddr$prim55873 = alloca %struct.ScmObj*, align 8
%anf_45bind48207 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48130)
store volatile %struct.ScmObj* %anf_45bind48207, %struct.ScmObj** %stackaddr$prim55873, align 8
%stackaddr$prim55874 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48207)
store volatile %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$prim55874, align 8
%stackaddr$prim55875 = alloca %struct.ScmObj*, align 8
%anf_45bind48208 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48130)
store volatile %struct.ScmObj* %anf_45bind48208, %struct.ScmObj** %stackaddr$prim55875, align 8
%stackaddr$prim55876 = alloca %struct.ScmObj*, align 8
%lsts48131 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48208)
store volatile %struct.ScmObj* %lsts48131, %struct.ScmObj** %stackaddr$prim55876, align 8
%stackaddr$makeclosure55877 = alloca %struct.ScmObj*, align 8
%fptrToInt55878 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49788 to i64
%ae49788 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt55878)
store volatile %struct.ScmObj* %ae49788, %struct.ScmObj** %stackaddr$makeclosure55877, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49788, %struct.ScmObj* %lsts48131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49788, %struct.ScmObj* %_37foldr48051, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49788, %struct.ScmObj* %f48133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49788, %struct.ScmObj* %acc48132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49788, %struct.ScmObj* %k48370, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49788, %struct.ScmObj* %_37foldl48129, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49788, %struct.ScmObj* %_37foldr148046, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae49788, %struct.ScmObj* %_37map148077, i64 7)
%ae49789 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55879 = alloca %struct.ScmObj*, align 8
%fptrToInt55880 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49790 to i64
%ae49790 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55880)
store volatile %struct.ScmObj* %ae49790, %struct.ScmObj** %stackaddr$makeclosure55879, align 8
%argslist54666$ae497880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55881 = alloca %struct.ScmObj*, align 8
%argslist54666$ae497881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49790, %struct.ScmObj* %argslist54666$ae497880)
store volatile %struct.ScmObj* %argslist54666$ae497881, %struct.ScmObj** %stackaddr$prim55881, align 8
%stackaddr$prim55882 = alloca %struct.ScmObj*, align 8
%argslist54666$ae497882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49789, %struct.ScmObj* %argslist54666$ae497881)
store volatile %struct.ScmObj* %argslist54666$ae497882, %struct.ScmObj** %stackaddr$prim55882, align 8
%clofunc55883 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49788)
musttail call tailcc void %clofunc55883(%struct.ScmObj* %ae49788, %struct.ScmObj* %argslist54666$ae497882)
ret void
}

define tailcc void @proc_clo$ae49788(%struct.ScmObj* %env$ae49788,%struct.ScmObj* %current_45args54612) {
%stackaddr$env-ref55884 = alloca %struct.ScmObj*, align 8
%lsts48131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49788, i64 0)
store %struct.ScmObj* %lsts48131, %struct.ScmObj** %stackaddr$env-ref55884
%stackaddr$env-ref55885 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49788, i64 1)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55885
%stackaddr$env-ref55886 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49788, i64 2)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref55886
%stackaddr$env-ref55887 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49788, i64 3)
store %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$env-ref55887
%stackaddr$env-ref55888 = alloca %struct.ScmObj*, align 8
%k48370 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49788, i64 4)
store %struct.ScmObj* %k48370, %struct.ScmObj** %stackaddr$env-ref55888
%stackaddr$env-ref55889 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49788, i64 5)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55889
%stackaddr$env-ref55890 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49788, i64 6)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55890
%stackaddr$env-ref55891 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49788, i64 7)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55891
%stackaddr$prim55892 = alloca %struct.ScmObj*, align 8
%_95k48371 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54612)
store volatile %struct.ScmObj* %_95k48371, %struct.ScmObj** %stackaddr$prim55892, align 8
%stackaddr$prim55893 = alloca %struct.ScmObj*, align 8
%current_45args54613 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54612)
store volatile %struct.ScmObj* %current_45args54613, %struct.ScmObj** %stackaddr$prim55893, align 8
%stackaddr$prim55894 = alloca %struct.ScmObj*, align 8
%anf_45bind48209 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54613)
store volatile %struct.ScmObj* %anf_45bind48209, %struct.ScmObj** %stackaddr$prim55894, align 8
%stackaddr$makeclosure55895 = alloca %struct.ScmObj*, align 8
%fptrToInt55896 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49820 to i64
%ae49820 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55896)
store volatile %struct.ScmObj* %ae49820, %struct.ScmObj** %stackaddr$makeclosure55895, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49820, %struct.ScmObj* %lsts48131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49820, %struct.ScmObj* %_37foldr48051, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49820, %struct.ScmObj* %f48133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49820, %struct.ScmObj* %acc48132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49820, %struct.ScmObj* %k48370, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49820, %struct.ScmObj* %_37foldl48129, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49820, %struct.ScmObj* %_37map148077, i64 6)
%ae49822 = call %struct.ScmObj* @const_init_false()
%argslist54659$_37foldr1480460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55897 = alloca %struct.ScmObj*, align 8
%argslist54659$_37foldr1480461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48131, %struct.ScmObj* %argslist54659$_37foldr1480460)
store volatile %struct.ScmObj* %argslist54659$_37foldr1480461, %struct.ScmObj** %stackaddr$prim55897, align 8
%stackaddr$prim55898 = alloca %struct.ScmObj*, align 8
%argslist54659$_37foldr1480462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49822, %struct.ScmObj* %argslist54659$_37foldr1480461)
store volatile %struct.ScmObj* %argslist54659$_37foldr1480462, %struct.ScmObj** %stackaddr$prim55898, align 8
%stackaddr$prim55899 = alloca %struct.ScmObj*, align 8
%argslist54659$_37foldr1480463 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48209, %struct.ScmObj* %argslist54659$_37foldr1480462)
store volatile %struct.ScmObj* %argslist54659$_37foldr1480463, %struct.ScmObj** %stackaddr$prim55899, align 8
%stackaddr$prim55900 = alloca %struct.ScmObj*, align 8
%argslist54659$_37foldr1480464 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49820, %struct.ScmObj* %argslist54659$_37foldr1480463)
store volatile %struct.ScmObj* %argslist54659$_37foldr1480464, %struct.ScmObj** %stackaddr$prim55900, align 8
%clofunc55901 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148046)
musttail call tailcc void %clofunc55901(%struct.ScmObj* %_37foldr148046, %struct.ScmObj* %argslist54659$_37foldr1480464)
ret void
}

define tailcc void @proc_clo$ae49820(%struct.ScmObj* %env$ae49820,%struct.ScmObj* %current_45args54615) {
%stackaddr$env-ref55902 = alloca %struct.ScmObj*, align 8
%lsts48131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49820, i64 0)
store %struct.ScmObj* %lsts48131, %struct.ScmObj** %stackaddr$env-ref55902
%stackaddr$env-ref55903 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49820, i64 1)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55903
%stackaddr$env-ref55904 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49820, i64 2)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref55904
%stackaddr$env-ref55905 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49820, i64 3)
store %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$env-ref55905
%stackaddr$env-ref55906 = alloca %struct.ScmObj*, align 8
%k48370 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49820, i64 4)
store %struct.ScmObj* %k48370, %struct.ScmObj** %stackaddr$env-ref55906
%stackaddr$env-ref55907 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49820, i64 5)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55907
%stackaddr$env-ref55908 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49820, i64 6)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55908
%stackaddr$prim55909 = alloca %struct.ScmObj*, align 8
%_95k48372 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54615)
store volatile %struct.ScmObj* %_95k48372, %struct.ScmObj** %stackaddr$prim55909, align 8
%stackaddr$prim55910 = alloca %struct.ScmObj*, align 8
%current_45args54616 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54615)
store volatile %struct.ScmObj* %current_45args54616, %struct.ScmObj** %stackaddr$prim55910, align 8
%stackaddr$prim55911 = alloca %struct.ScmObj*, align 8
%anf_45bind48210 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54616)
store volatile %struct.ScmObj* %anf_45bind48210, %struct.ScmObj** %stackaddr$prim55911, align 8
%truthy$cmp55912 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48210)
%cmp$cmp55912 = icmp eq i64 %truthy$cmp55912, 1
br i1 %cmp$cmp55912, label %truebranch$cmp55912, label %falsebranch$cmp55912
truebranch$cmp55912:
%ae49831 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54618$k483700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55913 = alloca %struct.ScmObj*, align 8
%argslist54618$k483701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48132, %struct.ScmObj* %argslist54618$k483700)
store volatile %struct.ScmObj* %argslist54618$k483701, %struct.ScmObj** %stackaddr$prim55913, align 8
%stackaddr$prim55914 = alloca %struct.ScmObj*, align 8
%argslist54618$k483702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49831, %struct.ScmObj* %argslist54618$k483701)
store volatile %struct.ScmObj* %argslist54618$k483702, %struct.ScmObj** %stackaddr$prim55914, align 8
%clofunc55915 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48370)
musttail call tailcc void %clofunc55915(%struct.ScmObj* %k48370, %struct.ScmObj* %argslist54618$k483702)
ret void
falsebranch$cmp55912:
%stackaddr$makeclosure55916 = alloca %struct.ScmObj*, align 8
%fptrToInt55917 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49836 to i64
%ae49836 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55917)
store volatile %struct.ScmObj* %ae49836, %struct.ScmObj** %stackaddr$makeclosure55916, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49836, %struct.ScmObj* %lsts48131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49836, %struct.ScmObj* %_37foldr48051, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49836, %struct.ScmObj* %f48133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49836, %struct.ScmObj* %acc48132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49836, %struct.ScmObj* %k48370, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49836, %struct.ScmObj* %_37foldl48129, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49836, %struct.ScmObj* %_37map148077, i64 6)
%ae49837 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55918 = alloca %struct.ScmObj*, align 8
%fptrToInt55919 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49838 to i64
%ae49838 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55919)
store volatile %struct.ScmObj* %ae49838, %struct.ScmObj** %stackaddr$makeclosure55918, align 8
%argslist54658$ae498360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55920 = alloca %struct.ScmObj*, align 8
%argslist54658$ae498361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49838, %struct.ScmObj* %argslist54658$ae498360)
store volatile %struct.ScmObj* %argslist54658$ae498361, %struct.ScmObj** %stackaddr$prim55920, align 8
%stackaddr$prim55921 = alloca %struct.ScmObj*, align 8
%argslist54658$ae498362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49837, %struct.ScmObj* %argslist54658$ae498361)
store volatile %struct.ScmObj* %argslist54658$ae498362, %struct.ScmObj** %stackaddr$prim55921, align 8
%clofunc55922 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49836)
musttail call tailcc void %clofunc55922(%struct.ScmObj* %ae49836, %struct.ScmObj* %argslist54658$ae498362)
ret void
}

define tailcc void @proc_clo$ae49836(%struct.ScmObj* %env$ae49836,%struct.ScmObj* %current_45args54619) {
%stackaddr$env-ref55923 = alloca %struct.ScmObj*, align 8
%lsts48131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49836, i64 0)
store %struct.ScmObj* %lsts48131, %struct.ScmObj** %stackaddr$env-ref55923
%stackaddr$env-ref55924 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49836, i64 1)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55924
%stackaddr$env-ref55925 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49836, i64 2)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref55925
%stackaddr$env-ref55926 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49836, i64 3)
store %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$env-ref55926
%stackaddr$env-ref55927 = alloca %struct.ScmObj*, align 8
%k48370 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49836, i64 4)
store %struct.ScmObj* %k48370, %struct.ScmObj** %stackaddr$env-ref55927
%stackaddr$env-ref55928 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49836, i64 5)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55928
%stackaddr$env-ref55929 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49836, i64 6)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55929
%stackaddr$prim55930 = alloca %struct.ScmObj*, align 8
%_95k48373 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54619)
store volatile %struct.ScmObj* %_95k48373, %struct.ScmObj** %stackaddr$prim55930, align 8
%stackaddr$prim55931 = alloca %struct.ScmObj*, align 8
%current_45args54620 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54619)
store volatile %struct.ScmObj* %current_45args54620, %struct.ScmObj** %stackaddr$prim55931, align 8
%stackaddr$prim55932 = alloca %struct.ScmObj*, align 8
%anf_45bind48211 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54620)
store volatile %struct.ScmObj* %anf_45bind48211, %struct.ScmObj** %stackaddr$prim55932, align 8
%stackaddr$makeclosure55933 = alloca %struct.ScmObj*, align 8
%fptrToInt55934 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49857 to i64
%ae49857 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55934)
store volatile %struct.ScmObj* %ae49857, %struct.ScmObj** %stackaddr$makeclosure55933, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49857, %struct.ScmObj* %lsts48131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49857, %struct.ScmObj* %_37foldr48051, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49857, %struct.ScmObj* %f48133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49857, %struct.ScmObj* %acc48132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49857, %struct.ScmObj* %k48370, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49857, %struct.ScmObj* %_37foldl48129, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49857, %struct.ScmObj* %_37map148077, i64 6)
%argslist54653$_37map1480770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55935 = alloca %struct.ScmObj*, align 8
%argslist54653$_37map1480771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48131, %struct.ScmObj* %argslist54653$_37map1480770)
store volatile %struct.ScmObj* %argslist54653$_37map1480771, %struct.ScmObj** %stackaddr$prim55935, align 8
%stackaddr$prim55936 = alloca %struct.ScmObj*, align 8
%argslist54653$_37map1480772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48211, %struct.ScmObj* %argslist54653$_37map1480771)
store volatile %struct.ScmObj* %argslist54653$_37map1480772, %struct.ScmObj** %stackaddr$prim55936, align 8
%stackaddr$prim55937 = alloca %struct.ScmObj*, align 8
%argslist54653$_37map1480773 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49857, %struct.ScmObj* %argslist54653$_37map1480772)
store volatile %struct.ScmObj* %argslist54653$_37map1480773, %struct.ScmObj** %stackaddr$prim55937, align 8
%clofunc55938 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148077)
musttail call tailcc void %clofunc55938(%struct.ScmObj* %_37map148077, %struct.ScmObj* %argslist54653$_37map1480773)
ret void
}

define tailcc void @proc_clo$ae49857(%struct.ScmObj* %env$ae49857,%struct.ScmObj* %current_45args54622) {
%stackaddr$env-ref55939 = alloca %struct.ScmObj*, align 8
%lsts48131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49857, i64 0)
store %struct.ScmObj* %lsts48131, %struct.ScmObj** %stackaddr$env-ref55939
%stackaddr$env-ref55940 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49857, i64 1)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55940
%stackaddr$env-ref55941 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49857, i64 2)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref55941
%stackaddr$env-ref55942 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49857, i64 3)
store %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$env-ref55942
%stackaddr$env-ref55943 = alloca %struct.ScmObj*, align 8
%k48370 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49857, i64 4)
store %struct.ScmObj* %k48370, %struct.ScmObj** %stackaddr$env-ref55943
%stackaddr$env-ref55944 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49857, i64 5)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55944
%stackaddr$env-ref55945 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49857, i64 6)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55945
%stackaddr$prim55946 = alloca %struct.ScmObj*, align 8
%_95k48374 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54622)
store volatile %struct.ScmObj* %_95k48374, %struct.ScmObj** %stackaddr$prim55946, align 8
%stackaddr$prim55947 = alloca %struct.ScmObj*, align 8
%current_45args54623 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54622)
store volatile %struct.ScmObj* %current_45args54623, %struct.ScmObj** %stackaddr$prim55947, align 8
%stackaddr$prim55948 = alloca %struct.ScmObj*, align 8
%lsts_4348138 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54623)
store volatile %struct.ScmObj* %lsts_4348138, %struct.ScmObj** %stackaddr$prim55948, align 8
%stackaddr$makeclosure55949 = alloca %struct.ScmObj*, align 8
%fptrToInt55950 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49860 to i64
%ae49860 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt55950)
store volatile %struct.ScmObj* %ae49860, %struct.ScmObj** %stackaddr$makeclosure55949, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49860, %struct.ScmObj* %lsts48131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49860, %struct.ScmObj* %_37foldr48051, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49860, %struct.ScmObj* %f48133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49860, %struct.ScmObj* %acc48132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49860, %struct.ScmObj* %k48370, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49860, %struct.ScmObj* %_37foldl48129, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49860, %struct.ScmObj* %_37map148077, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae49860, %struct.ScmObj* %lsts_4348138, i64 7)
%ae49861 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55951 = alloca %struct.ScmObj*, align 8
%fptrToInt55952 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49862 to i64
%ae49862 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55952)
store volatile %struct.ScmObj* %ae49862, %struct.ScmObj** %stackaddr$makeclosure55951, align 8
%argslist54652$ae498600 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55953 = alloca %struct.ScmObj*, align 8
%argslist54652$ae498601 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49862, %struct.ScmObj* %argslist54652$ae498600)
store volatile %struct.ScmObj* %argslist54652$ae498601, %struct.ScmObj** %stackaddr$prim55953, align 8
%stackaddr$prim55954 = alloca %struct.ScmObj*, align 8
%argslist54652$ae498602 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49861, %struct.ScmObj* %argslist54652$ae498601)
store volatile %struct.ScmObj* %argslist54652$ae498602, %struct.ScmObj** %stackaddr$prim55954, align 8
%clofunc55955 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49860)
musttail call tailcc void %clofunc55955(%struct.ScmObj* %ae49860, %struct.ScmObj* %argslist54652$ae498602)
ret void
}

define tailcc void @proc_clo$ae49860(%struct.ScmObj* %env$ae49860,%struct.ScmObj* %current_45args54625) {
%stackaddr$env-ref55956 = alloca %struct.ScmObj*, align 8
%lsts48131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49860, i64 0)
store %struct.ScmObj* %lsts48131, %struct.ScmObj** %stackaddr$env-ref55956
%stackaddr$env-ref55957 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49860, i64 1)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55957
%stackaddr$env-ref55958 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49860, i64 2)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref55958
%stackaddr$env-ref55959 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49860, i64 3)
store %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$env-ref55959
%stackaddr$env-ref55960 = alloca %struct.ScmObj*, align 8
%k48370 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49860, i64 4)
store %struct.ScmObj* %k48370, %struct.ScmObj** %stackaddr$env-ref55960
%stackaddr$env-ref55961 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49860, i64 5)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55961
%stackaddr$env-ref55962 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49860, i64 6)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55962
%stackaddr$env-ref55963 = alloca %struct.ScmObj*, align 8
%lsts_4348138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49860, i64 7)
store %struct.ScmObj* %lsts_4348138, %struct.ScmObj** %stackaddr$env-ref55963
%stackaddr$prim55964 = alloca %struct.ScmObj*, align 8
%_95k48375 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54625)
store volatile %struct.ScmObj* %_95k48375, %struct.ScmObj** %stackaddr$prim55964, align 8
%stackaddr$prim55965 = alloca %struct.ScmObj*, align 8
%current_45args54626 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54625)
store volatile %struct.ScmObj* %current_45args54626, %struct.ScmObj** %stackaddr$prim55965, align 8
%stackaddr$prim55966 = alloca %struct.ScmObj*, align 8
%anf_45bind48212 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54626)
store volatile %struct.ScmObj* %anf_45bind48212, %struct.ScmObj** %stackaddr$prim55966, align 8
%stackaddr$makeclosure55967 = alloca %struct.ScmObj*, align 8
%fptrToInt55968 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49881 to i64
%ae49881 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt55968)
store volatile %struct.ScmObj* %ae49881, %struct.ScmObj** %stackaddr$makeclosure55967, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49881, %struct.ScmObj* %f48133, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49881, %struct.ScmObj* %acc48132, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49881, %struct.ScmObj* %_37foldr48051, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49881, %struct.ScmObj* %k48370, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49881, %struct.ScmObj* %_37foldl48129, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49881, %struct.ScmObj* %lsts_4348138, i64 5)
%argslist54647$_37map1480770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55969 = alloca %struct.ScmObj*, align 8
%argslist54647$_37map1480771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48131, %struct.ScmObj* %argslist54647$_37map1480770)
store volatile %struct.ScmObj* %argslist54647$_37map1480771, %struct.ScmObj** %stackaddr$prim55969, align 8
%stackaddr$prim55970 = alloca %struct.ScmObj*, align 8
%argslist54647$_37map1480772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48212, %struct.ScmObj* %argslist54647$_37map1480771)
store volatile %struct.ScmObj* %argslist54647$_37map1480772, %struct.ScmObj** %stackaddr$prim55970, align 8
%stackaddr$prim55971 = alloca %struct.ScmObj*, align 8
%argslist54647$_37map1480773 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49881, %struct.ScmObj* %argslist54647$_37map1480772)
store volatile %struct.ScmObj* %argslist54647$_37map1480773, %struct.ScmObj** %stackaddr$prim55971, align 8
%clofunc55972 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148077)
musttail call tailcc void %clofunc55972(%struct.ScmObj* %_37map148077, %struct.ScmObj* %argslist54647$_37map1480773)
ret void
}

define tailcc void @proc_clo$ae49881(%struct.ScmObj* %env$ae49881,%struct.ScmObj* %current_45args54628) {
%stackaddr$env-ref55973 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49881, i64 0)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref55973
%stackaddr$env-ref55974 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49881, i64 1)
store %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$env-ref55974
%stackaddr$env-ref55975 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49881, i64 2)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55975
%stackaddr$env-ref55976 = alloca %struct.ScmObj*, align 8
%k48370 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49881, i64 3)
store %struct.ScmObj* %k48370, %struct.ScmObj** %stackaddr$env-ref55976
%stackaddr$env-ref55977 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49881, i64 4)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55977
%stackaddr$env-ref55978 = alloca %struct.ScmObj*, align 8
%lsts_4348138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49881, i64 5)
store %struct.ScmObj* %lsts_4348138, %struct.ScmObj** %stackaddr$env-ref55978
%stackaddr$prim55979 = alloca %struct.ScmObj*, align 8
%_95k48376 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54628)
store volatile %struct.ScmObj* %_95k48376, %struct.ScmObj** %stackaddr$prim55979, align 8
%stackaddr$prim55980 = alloca %struct.ScmObj*, align 8
%current_45args54629 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54628)
store volatile %struct.ScmObj* %current_45args54629, %struct.ScmObj** %stackaddr$prim55980, align 8
%stackaddr$prim55981 = alloca %struct.ScmObj*, align 8
%vs48136 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54629)
store volatile %struct.ScmObj* %vs48136, %struct.ScmObj** %stackaddr$prim55981, align 8
%stackaddr$makeclosure55982 = alloca %struct.ScmObj*, align 8
%fptrToInt55983 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49884 to i64
%ae49884 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55983)
store volatile %struct.ScmObj* %ae49884, %struct.ScmObj** %stackaddr$makeclosure55982, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49884, %struct.ScmObj* %vs48136, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49884, %struct.ScmObj* %f48133, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49884, %struct.ScmObj* %acc48132, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49884, %struct.ScmObj* %_37foldr48051, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49884, %struct.ScmObj* %k48370, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49884, %struct.ScmObj* %_37foldl48129, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49884, %struct.ScmObj* %lsts_4348138, i64 6)
%ae49885 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55984 = alloca %struct.ScmObj*, align 8
%fptrToInt55985 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49886 to i64
%ae49886 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55985)
store volatile %struct.ScmObj* %ae49886, %struct.ScmObj** %stackaddr$makeclosure55984, align 8
%argslist54646$ae498840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55986 = alloca %struct.ScmObj*, align 8
%argslist54646$ae498841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49886, %struct.ScmObj* %argslist54646$ae498840)
store volatile %struct.ScmObj* %argslist54646$ae498841, %struct.ScmObj** %stackaddr$prim55986, align 8
%stackaddr$prim55987 = alloca %struct.ScmObj*, align 8
%argslist54646$ae498842 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49885, %struct.ScmObj* %argslist54646$ae498841)
store volatile %struct.ScmObj* %argslist54646$ae498842, %struct.ScmObj** %stackaddr$prim55987, align 8
%clofunc55988 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49884)
musttail call tailcc void %clofunc55988(%struct.ScmObj* %ae49884, %struct.ScmObj* %argslist54646$ae498842)
ret void
}

define tailcc void @proc_clo$ae49884(%struct.ScmObj* %env$ae49884,%struct.ScmObj* %current_45args54631) {
%stackaddr$env-ref55989 = alloca %struct.ScmObj*, align 8
%vs48136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49884, i64 0)
store %struct.ScmObj* %vs48136, %struct.ScmObj** %stackaddr$env-ref55989
%stackaddr$env-ref55990 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49884, i64 1)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref55990
%stackaddr$env-ref55991 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49884, i64 2)
store %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$env-ref55991
%stackaddr$env-ref55992 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49884, i64 3)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55992
%stackaddr$env-ref55993 = alloca %struct.ScmObj*, align 8
%k48370 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49884, i64 4)
store %struct.ScmObj* %k48370, %struct.ScmObj** %stackaddr$env-ref55993
%stackaddr$env-ref55994 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49884, i64 5)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55994
%stackaddr$env-ref55995 = alloca %struct.ScmObj*, align 8
%lsts_4348138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49884, i64 6)
store %struct.ScmObj* %lsts_4348138, %struct.ScmObj** %stackaddr$env-ref55995
%stackaddr$prim55996 = alloca %struct.ScmObj*, align 8
%_95k48377 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54631)
store volatile %struct.ScmObj* %_95k48377, %struct.ScmObj** %stackaddr$prim55996, align 8
%stackaddr$prim55997 = alloca %struct.ScmObj*, align 8
%current_45args54632 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54631)
store volatile %struct.ScmObj* %current_45args54632, %struct.ScmObj** %stackaddr$prim55997, align 8
%stackaddr$prim55998 = alloca %struct.ScmObj*, align 8
%anf_45bind48213 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54632)
store volatile %struct.ScmObj* %anf_45bind48213, %struct.ScmObj** %stackaddr$prim55998, align 8
%ae49907 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55999 = alloca %struct.ScmObj*, align 8
%anf_45bind48214 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48132, %struct.ScmObj* %ae49907)
store volatile %struct.ScmObj* %anf_45bind48214, %struct.ScmObj** %stackaddr$prim55999, align 8
%stackaddr$makeclosure56000 = alloca %struct.ScmObj*, align 8
%fptrToInt56001 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49909 to i64
%ae49909 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56001)
store volatile %struct.ScmObj* %ae49909, %struct.ScmObj** %stackaddr$makeclosure56000, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49909, %struct.ScmObj* %f48133, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49909, %struct.ScmObj* %k48370, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49909, %struct.ScmObj* %_37foldl48129, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49909, %struct.ScmObj* %lsts_4348138, i64 3)
%argslist54640$_37foldr480510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56002 = alloca %struct.ScmObj*, align 8
%argslist54640$_37foldr480511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs48136, %struct.ScmObj* %argslist54640$_37foldr480510)
store volatile %struct.ScmObj* %argslist54640$_37foldr480511, %struct.ScmObj** %stackaddr$prim56002, align 8
%stackaddr$prim56003 = alloca %struct.ScmObj*, align 8
%argslist54640$_37foldr480512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48214, %struct.ScmObj* %argslist54640$_37foldr480511)
store volatile %struct.ScmObj* %argslist54640$_37foldr480512, %struct.ScmObj** %stackaddr$prim56003, align 8
%stackaddr$prim56004 = alloca %struct.ScmObj*, align 8
%argslist54640$_37foldr480513 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48213, %struct.ScmObj* %argslist54640$_37foldr480512)
store volatile %struct.ScmObj* %argslist54640$_37foldr480513, %struct.ScmObj** %stackaddr$prim56004, align 8
%stackaddr$prim56005 = alloca %struct.ScmObj*, align 8
%argslist54640$_37foldr480514 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49909, %struct.ScmObj* %argslist54640$_37foldr480513)
store volatile %struct.ScmObj* %argslist54640$_37foldr480514, %struct.ScmObj** %stackaddr$prim56005, align 8
%clofunc56006 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr48051)
musttail call tailcc void %clofunc56006(%struct.ScmObj* %_37foldr48051, %struct.ScmObj* %argslist54640$_37foldr480514)
ret void
}

define tailcc void @proc_clo$ae49909(%struct.ScmObj* %env$ae49909,%struct.ScmObj* %current_45args54634) {
%stackaddr$env-ref56007 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49909, i64 0)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref56007
%stackaddr$env-ref56008 = alloca %struct.ScmObj*, align 8
%k48370 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49909, i64 1)
store %struct.ScmObj* %k48370, %struct.ScmObj** %stackaddr$env-ref56008
%stackaddr$env-ref56009 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49909, i64 2)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref56009
%stackaddr$env-ref56010 = alloca %struct.ScmObj*, align 8
%lsts_4348138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49909, i64 3)
store %struct.ScmObj* %lsts_4348138, %struct.ScmObj** %stackaddr$env-ref56010
%stackaddr$prim56011 = alloca %struct.ScmObj*, align 8
%_95k48378 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54634)
store volatile %struct.ScmObj* %_95k48378, %struct.ScmObj** %stackaddr$prim56011, align 8
%stackaddr$prim56012 = alloca %struct.ScmObj*, align 8
%current_45args54635 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54634)
store volatile %struct.ScmObj* %current_45args54635, %struct.ScmObj** %stackaddr$prim56012, align 8
%stackaddr$prim56013 = alloca %struct.ScmObj*, align 8
%anf_45bind48215 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54635)
store volatile %struct.ScmObj* %anf_45bind48215, %struct.ScmObj** %stackaddr$prim56013, align 8
%stackaddr$makeclosure56014 = alloca %struct.ScmObj*, align 8
%fptrToInt56015 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49913 to i64
%ae49913 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56015)
store volatile %struct.ScmObj* %ae49913, %struct.ScmObj** %stackaddr$makeclosure56014, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49913, %struct.ScmObj* %f48133, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49913, %struct.ScmObj* %k48370, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49913, %struct.ScmObj* %_37foldl48129, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49913, %struct.ScmObj* %lsts_4348138, i64 3)
%stackaddr$prim56016 = alloca %struct.ScmObj*, align 8
%cpsargs48381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49913, %struct.ScmObj* %anf_45bind48215)
store volatile %struct.ScmObj* %cpsargs48381, %struct.ScmObj** %stackaddr$prim56016, align 8
%clofunc56017 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48133)
musttail call tailcc void %clofunc56017(%struct.ScmObj* %f48133, %struct.ScmObj* %cpsargs48381)
ret void
}

define tailcc void @proc_clo$ae49913(%struct.ScmObj* %env$ae49913,%struct.ScmObj* %current_45args54637) {
%stackaddr$env-ref56018 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49913, i64 0)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref56018
%stackaddr$env-ref56019 = alloca %struct.ScmObj*, align 8
%k48370 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49913, i64 1)
store %struct.ScmObj* %k48370, %struct.ScmObj** %stackaddr$env-ref56019
%stackaddr$env-ref56020 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49913, i64 2)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref56020
%stackaddr$env-ref56021 = alloca %struct.ScmObj*, align 8
%lsts_4348138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49913, i64 3)
store %struct.ScmObj* %lsts_4348138, %struct.ScmObj** %stackaddr$env-ref56021
%stackaddr$prim56022 = alloca %struct.ScmObj*, align 8
%_95k48379 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54637)
store volatile %struct.ScmObj* %_95k48379, %struct.ScmObj** %stackaddr$prim56022, align 8
%stackaddr$prim56023 = alloca %struct.ScmObj*, align 8
%current_45args54638 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54637)
store volatile %struct.ScmObj* %current_45args54638, %struct.ScmObj** %stackaddr$prim56023, align 8
%stackaddr$prim56024 = alloca %struct.ScmObj*, align 8
%acc_4348140 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54638)
store volatile %struct.ScmObj* %acc_4348140, %struct.ScmObj** %stackaddr$prim56024, align 8
%stackaddr$prim56025 = alloca %struct.ScmObj*, align 8
%anf_45bind48216 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4348140, %struct.ScmObj* %lsts_4348138)
store volatile %struct.ScmObj* %anf_45bind48216, %struct.ScmObj** %stackaddr$prim56025, align 8
%stackaddr$prim56026 = alloca %struct.ScmObj*, align 8
%anf_45bind48217 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48133, %struct.ScmObj* %anf_45bind48216)
store volatile %struct.ScmObj* %anf_45bind48217, %struct.ScmObj** %stackaddr$prim56026, align 8
%stackaddr$prim56027 = alloca %struct.ScmObj*, align 8
%cpsargs48380 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48370, %struct.ScmObj* %anf_45bind48217)
store volatile %struct.ScmObj* %cpsargs48380, %struct.ScmObj** %stackaddr$prim56027, align 8
%clofunc56028 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl48129)
musttail call tailcc void %clofunc56028(%struct.ScmObj* %_37foldl48129, %struct.ScmObj* %cpsargs48380)
ret void
}

define tailcc void @proc_clo$ae49886(%struct.ScmObj* %env$ae49886,%struct.ScmObj* %current_45args54641) {
%stackaddr$prim56029 = alloca %struct.ScmObj*, align 8
%k48382 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54641)
store volatile %struct.ScmObj* %k48382, %struct.ScmObj** %stackaddr$prim56029, align 8
%stackaddr$prim56030 = alloca %struct.ScmObj*, align 8
%current_45args54642 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54641)
store volatile %struct.ScmObj* %current_45args54642, %struct.ScmObj** %stackaddr$prim56030, align 8
%stackaddr$prim56031 = alloca %struct.ScmObj*, align 8
%a48142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54642)
store volatile %struct.ScmObj* %a48142, %struct.ScmObj** %stackaddr$prim56031, align 8
%stackaddr$prim56032 = alloca %struct.ScmObj*, align 8
%current_45args54643 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54642)
store volatile %struct.ScmObj* %current_45args54643, %struct.ScmObj** %stackaddr$prim56032, align 8
%stackaddr$prim56033 = alloca %struct.ScmObj*, align 8
%b48141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54643)
store volatile %struct.ScmObj* %b48141, %struct.ScmObj** %stackaddr$prim56033, align 8
%stackaddr$prim56034 = alloca %struct.ScmObj*, align 8
%cpsprim48383 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a48142, %struct.ScmObj* %b48141)
store volatile %struct.ScmObj* %cpsprim48383, %struct.ScmObj** %stackaddr$prim56034, align 8
%ae49890 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54645$k483820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56035 = alloca %struct.ScmObj*, align 8
%argslist54645$k483821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48383, %struct.ScmObj* %argslist54645$k483820)
store volatile %struct.ScmObj* %argslist54645$k483821, %struct.ScmObj** %stackaddr$prim56035, align 8
%stackaddr$prim56036 = alloca %struct.ScmObj*, align 8
%argslist54645$k483822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49890, %struct.ScmObj* %argslist54645$k483821)
store volatile %struct.ScmObj* %argslist54645$k483822, %struct.ScmObj** %stackaddr$prim56036, align 8
%clofunc56037 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48382)
musttail call tailcc void %clofunc56037(%struct.ScmObj* %k48382, %struct.ScmObj* %argslist54645$k483822)
ret void
}

define tailcc void @proc_clo$ae49862(%struct.ScmObj* %env$ae49862,%struct.ScmObj* %current_45args54648) {
%stackaddr$prim56038 = alloca %struct.ScmObj*, align 8
%k48384 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54648)
store volatile %struct.ScmObj* %k48384, %struct.ScmObj** %stackaddr$prim56038, align 8
%stackaddr$prim56039 = alloca %struct.ScmObj*, align 8
%current_45args54649 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54648)
store volatile %struct.ScmObj* %current_45args54649, %struct.ScmObj** %stackaddr$prim56039, align 8
%stackaddr$prim56040 = alloca %struct.ScmObj*, align 8
%x48137 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54649)
store volatile %struct.ScmObj* %x48137, %struct.ScmObj** %stackaddr$prim56040, align 8
%stackaddr$prim56041 = alloca %struct.ScmObj*, align 8
%cpsprim48385 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48137)
store volatile %struct.ScmObj* %cpsprim48385, %struct.ScmObj** %stackaddr$prim56041, align 8
%ae49865 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54651$k483840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56042 = alloca %struct.ScmObj*, align 8
%argslist54651$k483841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48385, %struct.ScmObj* %argslist54651$k483840)
store volatile %struct.ScmObj* %argslist54651$k483841, %struct.ScmObj** %stackaddr$prim56042, align 8
%stackaddr$prim56043 = alloca %struct.ScmObj*, align 8
%argslist54651$k483842 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49865, %struct.ScmObj* %argslist54651$k483841)
store volatile %struct.ScmObj* %argslist54651$k483842, %struct.ScmObj** %stackaddr$prim56043, align 8
%clofunc56044 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48384)
musttail call tailcc void %clofunc56044(%struct.ScmObj* %k48384, %struct.ScmObj* %argslist54651$k483842)
ret void
}

define tailcc void @proc_clo$ae49838(%struct.ScmObj* %env$ae49838,%struct.ScmObj* %current_45args54654) {
%stackaddr$prim56045 = alloca %struct.ScmObj*, align 8
%k48386 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54654)
store volatile %struct.ScmObj* %k48386, %struct.ScmObj** %stackaddr$prim56045, align 8
%stackaddr$prim56046 = alloca %struct.ScmObj*, align 8
%current_45args54655 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54654)
store volatile %struct.ScmObj* %current_45args54655, %struct.ScmObj** %stackaddr$prim56046, align 8
%stackaddr$prim56047 = alloca %struct.ScmObj*, align 8
%x48139 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54655)
store volatile %struct.ScmObj* %x48139, %struct.ScmObj** %stackaddr$prim56047, align 8
%stackaddr$prim56048 = alloca %struct.ScmObj*, align 8
%cpsprim48387 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48139)
store volatile %struct.ScmObj* %cpsprim48387, %struct.ScmObj** %stackaddr$prim56048, align 8
%ae49841 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54657$k483860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56049 = alloca %struct.ScmObj*, align 8
%argslist54657$k483861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48387, %struct.ScmObj* %argslist54657$k483860)
store volatile %struct.ScmObj* %argslist54657$k483861, %struct.ScmObj** %stackaddr$prim56049, align 8
%stackaddr$prim56050 = alloca %struct.ScmObj*, align 8
%argslist54657$k483862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49841, %struct.ScmObj* %argslist54657$k483861)
store volatile %struct.ScmObj* %argslist54657$k483862, %struct.ScmObj** %stackaddr$prim56050, align 8
%clofunc56051 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48386)
musttail call tailcc void %clofunc56051(%struct.ScmObj* %k48386, %struct.ScmObj* %argslist54657$k483862)
ret void
}

define tailcc void @proc_clo$ae49790(%struct.ScmObj* %env$ae49790,%struct.ScmObj* %current_45args54660) {
%stackaddr$prim56052 = alloca %struct.ScmObj*, align 8
%k48388 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54660)
store volatile %struct.ScmObj* %k48388, %struct.ScmObj** %stackaddr$prim56052, align 8
%stackaddr$prim56053 = alloca %struct.ScmObj*, align 8
%current_45args54661 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54660)
store volatile %struct.ScmObj* %current_45args54661, %struct.ScmObj** %stackaddr$prim56053, align 8
%stackaddr$prim56054 = alloca %struct.ScmObj*, align 8
%lst48135 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54661)
store volatile %struct.ScmObj* %lst48135, %struct.ScmObj** %stackaddr$prim56054, align 8
%stackaddr$prim56055 = alloca %struct.ScmObj*, align 8
%current_45args54662 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54661)
store volatile %struct.ScmObj* %current_45args54662, %struct.ScmObj** %stackaddr$prim56055, align 8
%stackaddr$prim56056 = alloca %struct.ScmObj*, align 8
%b48134 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54662)
store volatile %struct.ScmObj* %b48134, %struct.ScmObj** %stackaddr$prim56056, align 8
%truthy$cmp56057 = call i64 @is_truthy_value(%struct.ScmObj* %b48134)
%cmp$cmp56057 = icmp eq i64 %truthy$cmp56057, 1
br i1 %cmp$cmp56057, label %truebranch$cmp56057, label %falsebranch$cmp56057
truebranch$cmp56057:
%ae49793 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54664$k483880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56058 = alloca %struct.ScmObj*, align 8
%argslist54664$k483881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b48134, %struct.ScmObj* %argslist54664$k483880)
store volatile %struct.ScmObj* %argslist54664$k483881, %struct.ScmObj** %stackaddr$prim56058, align 8
%stackaddr$prim56059 = alloca %struct.ScmObj*, align 8
%argslist54664$k483882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49793, %struct.ScmObj* %argslist54664$k483881)
store volatile %struct.ScmObj* %argslist54664$k483882, %struct.ScmObj** %stackaddr$prim56059, align 8
%clofunc56060 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48388)
musttail call tailcc void %clofunc56060(%struct.ScmObj* %k48388, %struct.ScmObj* %argslist54664$k483882)
ret void
falsebranch$cmp56057:
%stackaddr$prim56061 = alloca %struct.ScmObj*, align 8
%cpsprim48389 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48135)
store volatile %struct.ScmObj* %cpsprim48389, %struct.ScmObj** %stackaddr$prim56061, align 8
%ae49800 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54665$k483880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56062 = alloca %struct.ScmObj*, align 8
%argslist54665$k483881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48389, %struct.ScmObj* %argslist54665$k483880)
store volatile %struct.ScmObj* %argslist54665$k483881, %struct.ScmObj** %stackaddr$prim56062, align 8
%stackaddr$prim56063 = alloca %struct.ScmObj*, align 8
%argslist54665$k483882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49800, %struct.ScmObj* %argslist54665$k483881)
store volatile %struct.ScmObj* %argslist54665$k483882, %struct.ScmObj** %stackaddr$prim56063, align 8
%clofunc56064 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48388)
musttail call tailcc void %clofunc56064(%struct.ScmObj* %k48388, %struct.ScmObj* %argslist54665$k483882)
ret void
}

define tailcc void @proc_clo$ae49631(%struct.ScmObj* %env$ae49631,%struct.ScmObj* %args4807348390) {
%stackaddr$env-ref56065 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49631, i64 0)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref56065
%stackaddr$env-ref56066 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49631, i64 1)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref56066
%stackaddr$env-ref56067 = alloca %struct.ScmObj*, align 8
%_37drop_45right48065 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49631, i64 2)
store %struct.ScmObj* %_37drop_45right48065, %struct.ScmObj** %stackaddr$env-ref56067
%stackaddr$prim56068 = alloca %struct.ScmObj*, align 8
%k48391 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4807348390)
store volatile %struct.ScmObj* %k48391, %struct.ScmObj** %stackaddr$prim56068, align 8
%stackaddr$prim56069 = alloca %struct.ScmObj*, align 8
%args48073 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4807348390)
store volatile %struct.ScmObj* %args48073, %struct.ScmObj** %stackaddr$prim56069, align 8
%stackaddr$prim56070 = alloca %struct.ScmObj*, align 8
%f48075 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48073)
store volatile %struct.ScmObj* %f48075, %struct.ScmObj** %stackaddr$prim56070, align 8
%stackaddr$prim56071 = alloca %struct.ScmObj*, align 8
%lsts48074 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48073)
store volatile %struct.ScmObj* %lsts48074, %struct.ScmObj** %stackaddr$prim56071, align 8
%stackaddr$makeclosure56072 = alloca %struct.ScmObj*, align 8
%fptrToInt56073 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49636 to i64
%ae49636 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56073)
store volatile %struct.ScmObj* %ae49636, %struct.ScmObj** %stackaddr$makeclosure56072, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49636, %struct.ScmObj* %k48391, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49636, %struct.ScmObj* %_37foldr48051, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49636, %struct.ScmObj* %lsts48074, i64 2)
%ae49637 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56074 = alloca %struct.ScmObj*, align 8
%fptrToInt56075 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49638 to i64
%ae49638 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56075)
store volatile %struct.ScmObj* %ae49638, %struct.ScmObj** %stackaddr$makeclosure56074, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49638, %struct.ScmObj* %_37last48068, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49638, %struct.ScmObj* %_37drop_45right48065, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49638, %struct.ScmObj* %f48075, i64 2)
%argslist54684$ae496360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56076 = alloca %struct.ScmObj*, align 8
%argslist54684$ae496361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49638, %struct.ScmObj* %argslist54684$ae496360)
store volatile %struct.ScmObj* %argslist54684$ae496361, %struct.ScmObj** %stackaddr$prim56076, align 8
%stackaddr$prim56077 = alloca %struct.ScmObj*, align 8
%argslist54684$ae496362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49637, %struct.ScmObj* %argslist54684$ae496361)
store volatile %struct.ScmObj* %argslist54684$ae496362, %struct.ScmObj** %stackaddr$prim56077, align 8
%clofunc56078 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49636)
musttail call tailcc void %clofunc56078(%struct.ScmObj* %ae49636, %struct.ScmObj* %argslist54684$ae496362)
ret void
}

define tailcc void @proc_clo$ae49636(%struct.ScmObj* %env$ae49636,%struct.ScmObj* %current_45args54669) {
%stackaddr$env-ref56079 = alloca %struct.ScmObj*, align 8
%k48391 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49636, i64 0)
store %struct.ScmObj* %k48391, %struct.ScmObj** %stackaddr$env-ref56079
%stackaddr$env-ref56080 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49636, i64 1)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref56080
%stackaddr$env-ref56081 = alloca %struct.ScmObj*, align 8
%lsts48074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49636, i64 2)
store %struct.ScmObj* %lsts48074, %struct.ScmObj** %stackaddr$env-ref56081
%stackaddr$prim56082 = alloca %struct.ScmObj*, align 8
%_95k48392 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54669)
store volatile %struct.ScmObj* %_95k48392, %struct.ScmObj** %stackaddr$prim56082, align 8
%stackaddr$prim56083 = alloca %struct.ScmObj*, align 8
%current_45args54670 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54669)
store volatile %struct.ScmObj* %current_45args54670, %struct.ScmObj** %stackaddr$prim56083, align 8
%stackaddr$prim56084 = alloca %struct.ScmObj*, align 8
%anf_45bind48204 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54670)
store volatile %struct.ScmObj* %anf_45bind48204, %struct.ScmObj** %stackaddr$prim56084, align 8
%ae49699 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56085 = alloca %struct.ScmObj*, align 8
%anf_45bind48205 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49699, %struct.ScmObj* %lsts48074)
store volatile %struct.ScmObj* %anf_45bind48205, %struct.ScmObj** %stackaddr$prim56085, align 8
%stackaddr$prim56086 = alloca %struct.ScmObj*, align 8
%anf_45bind48206 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48204, %struct.ScmObj* %anf_45bind48205)
store volatile %struct.ScmObj* %anf_45bind48206, %struct.ScmObj** %stackaddr$prim56086, align 8
%stackaddr$prim56087 = alloca %struct.ScmObj*, align 8
%cpsargs48393 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48391, %struct.ScmObj* %anf_45bind48206)
store volatile %struct.ScmObj* %cpsargs48393, %struct.ScmObj** %stackaddr$prim56087, align 8
%clofunc56088 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr48051)
musttail call tailcc void %clofunc56088(%struct.ScmObj* %_37foldr48051, %struct.ScmObj* %cpsargs48393)
ret void
}

define tailcc void @proc_clo$ae49638(%struct.ScmObj* %env$ae49638,%struct.ScmObj* %fargs4807648394) {
%stackaddr$env-ref56089 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49638, i64 0)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref56089
%stackaddr$env-ref56090 = alloca %struct.ScmObj*, align 8
%_37drop_45right48065 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49638, i64 1)
store %struct.ScmObj* %_37drop_45right48065, %struct.ScmObj** %stackaddr$env-ref56090
%stackaddr$env-ref56091 = alloca %struct.ScmObj*, align 8
%f48075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49638, i64 2)
store %struct.ScmObj* %f48075, %struct.ScmObj** %stackaddr$env-ref56091
%stackaddr$prim56092 = alloca %struct.ScmObj*, align 8
%k48395 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs4807648394)
store volatile %struct.ScmObj* %k48395, %struct.ScmObj** %stackaddr$prim56092, align 8
%stackaddr$prim56093 = alloca %struct.ScmObj*, align 8
%fargs48076 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs4807648394)
store volatile %struct.ScmObj* %fargs48076, %struct.ScmObj** %stackaddr$prim56093, align 8
%stackaddr$makeclosure56094 = alloca %struct.ScmObj*, align 8
%fptrToInt56095 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49642 to i64
%ae49642 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56095)
store volatile %struct.ScmObj* %ae49642, %struct.ScmObj** %stackaddr$makeclosure56094, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49642, %struct.ScmObj* %k48395, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49642, %struct.ScmObj* %f48075, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49642, %struct.ScmObj* %_37last48068, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49642, %struct.ScmObj* %fargs48076, i64 3)
%ae49644 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist54683$_37drop_45right480650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56096 = alloca %struct.ScmObj*, align 8
%argslist54683$_37drop_45right480651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49644, %struct.ScmObj* %argslist54683$_37drop_45right480650)
store volatile %struct.ScmObj* %argslist54683$_37drop_45right480651, %struct.ScmObj** %stackaddr$prim56096, align 8
%stackaddr$prim56097 = alloca %struct.ScmObj*, align 8
%argslist54683$_37drop_45right480652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs48076, %struct.ScmObj* %argslist54683$_37drop_45right480651)
store volatile %struct.ScmObj* %argslist54683$_37drop_45right480652, %struct.ScmObj** %stackaddr$prim56097, align 8
%stackaddr$prim56098 = alloca %struct.ScmObj*, align 8
%argslist54683$_37drop_45right480653 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49642, %struct.ScmObj* %argslist54683$_37drop_45right480652)
store volatile %struct.ScmObj* %argslist54683$_37drop_45right480653, %struct.ScmObj** %stackaddr$prim56098, align 8
%clofunc56099 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right48065)
musttail call tailcc void %clofunc56099(%struct.ScmObj* %_37drop_45right48065, %struct.ScmObj* %argslist54683$_37drop_45right480653)
ret void
}

define tailcc void @proc_clo$ae49642(%struct.ScmObj* %env$ae49642,%struct.ScmObj* %current_45args54672) {
%stackaddr$env-ref56100 = alloca %struct.ScmObj*, align 8
%k48395 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49642, i64 0)
store %struct.ScmObj* %k48395, %struct.ScmObj** %stackaddr$env-ref56100
%stackaddr$env-ref56101 = alloca %struct.ScmObj*, align 8
%f48075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49642, i64 1)
store %struct.ScmObj* %f48075, %struct.ScmObj** %stackaddr$env-ref56101
%stackaddr$env-ref56102 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49642, i64 2)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref56102
%stackaddr$env-ref56103 = alloca %struct.ScmObj*, align 8
%fargs48076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49642, i64 3)
store %struct.ScmObj* %fargs48076, %struct.ScmObj** %stackaddr$env-ref56103
%stackaddr$prim56104 = alloca %struct.ScmObj*, align 8
%_95k48396 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54672)
store volatile %struct.ScmObj* %_95k48396, %struct.ScmObj** %stackaddr$prim56104, align 8
%stackaddr$prim56105 = alloca %struct.ScmObj*, align 8
%current_45args54673 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54672)
store volatile %struct.ScmObj* %current_45args54673, %struct.ScmObj** %stackaddr$prim56105, align 8
%stackaddr$prim56106 = alloca %struct.ScmObj*, align 8
%anf_45bind48201 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54673)
store volatile %struct.ScmObj* %anf_45bind48201, %struct.ScmObj** %stackaddr$prim56106, align 8
%stackaddr$makeclosure56107 = alloca %struct.ScmObj*, align 8
%fptrToInt56108 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49649 to i64
%ae49649 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56108)
store volatile %struct.ScmObj* %ae49649, %struct.ScmObj** %stackaddr$makeclosure56107, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49649, %struct.ScmObj* %_37last48068, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49649, %struct.ScmObj* %fargs48076, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49649, %struct.ScmObj* %k48395, i64 2)
%stackaddr$prim56109 = alloca %struct.ScmObj*, align 8
%cpsargs48400 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49649, %struct.ScmObj* %anf_45bind48201)
store volatile %struct.ScmObj* %cpsargs48400, %struct.ScmObj** %stackaddr$prim56109, align 8
%clofunc56110 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48075)
musttail call tailcc void %clofunc56110(%struct.ScmObj* %f48075, %struct.ScmObj* %cpsargs48400)
ret void
}

define tailcc void @proc_clo$ae49649(%struct.ScmObj* %env$ae49649,%struct.ScmObj* %current_45args54675) {
%stackaddr$env-ref56111 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49649, i64 0)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref56111
%stackaddr$env-ref56112 = alloca %struct.ScmObj*, align 8
%fargs48076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49649, i64 1)
store %struct.ScmObj* %fargs48076, %struct.ScmObj** %stackaddr$env-ref56112
%stackaddr$env-ref56113 = alloca %struct.ScmObj*, align 8
%k48395 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49649, i64 2)
store %struct.ScmObj* %k48395, %struct.ScmObj** %stackaddr$env-ref56113
%stackaddr$prim56114 = alloca %struct.ScmObj*, align 8
%_95k48397 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54675)
store volatile %struct.ScmObj* %_95k48397, %struct.ScmObj** %stackaddr$prim56114, align 8
%stackaddr$prim56115 = alloca %struct.ScmObj*, align 8
%current_45args54676 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54675)
store volatile %struct.ScmObj* %current_45args54676, %struct.ScmObj** %stackaddr$prim56115, align 8
%stackaddr$prim56116 = alloca %struct.ScmObj*, align 8
%anf_45bind48202 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54676)
store volatile %struct.ScmObj* %anf_45bind48202, %struct.ScmObj** %stackaddr$prim56116, align 8
%stackaddr$makeclosure56117 = alloca %struct.ScmObj*, align 8
%fptrToInt56118 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49654 to i64
%ae49654 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56118)
store volatile %struct.ScmObj* %ae49654, %struct.ScmObj** %stackaddr$makeclosure56117, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49654, %struct.ScmObj* %k48395, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49654, %struct.ScmObj* %anf_45bind48202, i64 1)
%argslist54682$_37last480680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56119 = alloca %struct.ScmObj*, align 8
%argslist54682$_37last480681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs48076, %struct.ScmObj* %argslist54682$_37last480680)
store volatile %struct.ScmObj* %argslist54682$_37last480681, %struct.ScmObj** %stackaddr$prim56119, align 8
%stackaddr$prim56120 = alloca %struct.ScmObj*, align 8
%argslist54682$_37last480682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49654, %struct.ScmObj* %argslist54682$_37last480681)
store volatile %struct.ScmObj* %argslist54682$_37last480682, %struct.ScmObj** %stackaddr$prim56120, align 8
%clofunc56121 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last48068)
musttail call tailcc void %clofunc56121(%struct.ScmObj* %_37last48068, %struct.ScmObj* %argslist54682$_37last480682)
ret void
}

define tailcc void @proc_clo$ae49654(%struct.ScmObj* %env$ae49654,%struct.ScmObj* %current_45args54678) {
%stackaddr$env-ref56122 = alloca %struct.ScmObj*, align 8
%k48395 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49654, i64 0)
store %struct.ScmObj* %k48395, %struct.ScmObj** %stackaddr$env-ref56122
%stackaddr$env-ref56123 = alloca %struct.ScmObj*, align 8
%anf_45bind48202 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49654, i64 1)
store %struct.ScmObj* %anf_45bind48202, %struct.ScmObj** %stackaddr$env-ref56123
%stackaddr$prim56124 = alloca %struct.ScmObj*, align 8
%_95k48398 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54678)
store volatile %struct.ScmObj* %_95k48398, %struct.ScmObj** %stackaddr$prim56124, align 8
%stackaddr$prim56125 = alloca %struct.ScmObj*, align 8
%current_45args54679 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54678)
store volatile %struct.ScmObj* %current_45args54679, %struct.ScmObj** %stackaddr$prim56125, align 8
%stackaddr$prim56126 = alloca %struct.ScmObj*, align 8
%anf_45bind48203 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54679)
store volatile %struct.ScmObj* %anf_45bind48203, %struct.ScmObj** %stackaddr$prim56126, align 8
%stackaddr$prim56127 = alloca %struct.ScmObj*, align 8
%cpsprim48399 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48202, %struct.ScmObj* %anf_45bind48203)
store volatile %struct.ScmObj* %cpsprim48399, %struct.ScmObj** %stackaddr$prim56127, align 8
%ae49659 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54681$k483950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56128 = alloca %struct.ScmObj*, align 8
%argslist54681$k483951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48399, %struct.ScmObj* %argslist54681$k483950)
store volatile %struct.ScmObj* %argslist54681$k483951, %struct.ScmObj** %stackaddr$prim56128, align 8
%stackaddr$prim56129 = alloca %struct.ScmObj*, align 8
%argslist54681$k483952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49659, %struct.ScmObj* %argslist54681$k483951)
store volatile %struct.ScmObj* %argslist54681$k483952, %struct.ScmObj** %stackaddr$prim56129, align 8
%clofunc56130 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48395)
musttail call tailcc void %clofunc56130(%struct.ScmObj* %k48395, %struct.ScmObj* %argslist54681$k483952)
ret void
}

define tailcc void @proc_clo$ae49554(%struct.ScmObj* %env$ae49554,%struct.ScmObj* %current_45args54686) {
%stackaddr$env-ref56131 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49554, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56131
%stackaddr$prim56132 = alloca %struct.ScmObj*, align 8
%k48401 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54686)
store volatile %struct.ScmObj* %k48401, %struct.ScmObj** %stackaddr$prim56132, align 8
%stackaddr$prim56133 = alloca %struct.ScmObj*, align 8
%current_45args54687 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54686)
store volatile %struct.ScmObj* %current_45args54687, %struct.ScmObj** %stackaddr$prim56133, align 8
%stackaddr$prim56134 = alloca %struct.ScmObj*, align 8
%f48079 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54687)
store volatile %struct.ScmObj* %f48079, %struct.ScmObj** %stackaddr$prim56134, align 8
%stackaddr$prim56135 = alloca %struct.ScmObj*, align 8
%current_45args54688 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54687)
store volatile %struct.ScmObj* %current_45args54688, %struct.ScmObj** %stackaddr$prim56135, align 8
%stackaddr$prim56136 = alloca %struct.ScmObj*, align 8
%lst48078 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54688)
store volatile %struct.ScmObj* %lst48078, %struct.ScmObj** %stackaddr$prim56136, align 8
%stackaddr$makeclosure56137 = alloca %struct.ScmObj*, align 8
%fptrToInt56138 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49555 to i64
%ae49555 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56138)
store volatile %struct.ScmObj* %ae49555, %struct.ScmObj** %stackaddr$makeclosure56137, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49555, %struct.ScmObj* %lst48078, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49555, %struct.ScmObj* %_37foldr148046, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49555, %struct.ScmObj* %k48401, i64 2)
%ae49556 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56139 = alloca %struct.ScmObj*, align 8
%fptrToInt56140 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49557 to i64
%ae49557 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56140)
store volatile %struct.ScmObj* %ae49557, %struct.ScmObj** %stackaddr$makeclosure56139, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49557, %struct.ScmObj* %f48079, i64 0)
%argslist54703$ae495550 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56141 = alloca %struct.ScmObj*, align 8
%argslist54703$ae495551 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49557, %struct.ScmObj* %argslist54703$ae495550)
store volatile %struct.ScmObj* %argslist54703$ae495551, %struct.ScmObj** %stackaddr$prim56141, align 8
%stackaddr$prim56142 = alloca %struct.ScmObj*, align 8
%argslist54703$ae495552 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49556, %struct.ScmObj* %argslist54703$ae495551)
store volatile %struct.ScmObj* %argslist54703$ae495552, %struct.ScmObj** %stackaddr$prim56142, align 8
%clofunc56143 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49555)
musttail call tailcc void %clofunc56143(%struct.ScmObj* %ae49555, %struct.ScmObj* %argslist54703$ae495552)
ret void
}

define tailcc void @proc_clo$ae49555(%struct.ScmObj* %env$ae49555,%struct.ScmObj* %current_45args54690) {
%stackaddr$env-ref56144 = alloca %struct.ScmObj*, align 8
%lst48078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49555, i64 0)
store %struct.ScmObj* %lst48078, %struct.ScmObj** %stackaddr$env-ref56144
%stackaddr$env-ref56145 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49555, i64 1)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56145
%stackaddr$env-ref56146 = alloca %struct.ScmObj*, align 8
%k48401 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49555, i64 2)
store %struct.ScmObj* %k48401, %struct.ScmObj** %stackaddr$env-ref56146
%stackaddr$prim56147 = alloca %struct.ScmObj*, align 8
%_95k48402 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54690)
store volatile %struct.ScmObj* %_95k48402, %struct.ScmObj** %stackaddr$prim56147, align 8
%stackaddr$prim56148 = alloca %struct.ScmObj*, align 8
%current_45args54691 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54690)
store volatile %struct.ScmObj* %current_45args54691, %struct.ScmObj** %stackaddr$prim56148, align 8
%stackaddr$prim56149 = alloca %struct.ScmObj*, align 8
%anf_45bind48200 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54691)
store volatile %struct.ScmObj* %anf_45bind48200, %struct.ScmObj** %stackaddr$prim56149, align 8
%ae49589 = call %struct.ScmObj* @const_init_null()
%argslist54693$_37foldr1480460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56150 = alloca %struct.ScmObj*, align 8
%argslist54693$_37foldr1480461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48078, %struct.ScmObj* %argslist54693$_37foldr1480460)
store volatile %struct.ScmObj* %argslist54693$_37foldr1480461, %struct.ScmObj** %stackaddr$prim56150, align 8
%stackaddr$prim56151 = alloca %struct.ScmObj*, align 8
%argslist54693$_37foldr1480462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49589, %struct.ScmObj* %argslist54693$_37foldr1480461)
store volatile %struct.ScmObj* %argslist54693$_37foldr1480462, %struct.ScmObj** %stackaddr$prim56151, align 8
%stackaddr$prim56152 = alloca %struct.ScmObj*, align 8
%argslist54693$_37foldr1480463 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48200, %struct.ScmObj* %argslist54693$_37foldr1480462)
store volatile %struct.ScmObj* %argslist54693$_37foldr1480463, %struct.ScmObj** %stackaddr$prim56152, align 8
%stackaddr$prim56153 = alloca %struct.ScmObj*, align 8
%argslist54693$_37foldr1480464 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48401, %struct.ScmObj* %argslist54693$_37foldr1480463)
store volatile %struct.ScmObj* %argslist54693$_37foldr1480464, %struct.ScmObj** %stackaddr$prim56153, align 8
%clofunc56154 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148046)
musttail call tailcc void %clofunc56154(%struct.ScmObj* %_37foldr148046, %struct.ScmObj* %argslist54693$_37foldr1480464)
ret void
}

define tailcc void @proc_clo$ae49557(%struct.ScmObj* %env$ae49557,%struct.ScmObj* %current_45args54694) {
%stackaddr$env-ref56155 = alloca %struct.ScmObj*, align 8
%f48079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49557, i64 0)
store %struct.ScmObj* %f48079, %struct.ScmObj** %stackaddr$env-ref56155
%stackaddr$prim56156 = alloca %struct.ScmObj*, align 8
%k48403 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54694)
store volatile %struct.ScmObj* %k48403, %struct.ScmObj** %stackaddr$prim56156, align 8
%stackaddr$prim56157 = alloca %struct.ScmObj*, align 8
%current_45args54695 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54694)
store volatile %struct.ScmObj* %current_45args54695, %struct.ScmObj** %stackaddr$prim56157, align 8
%stackaddr$prim56158 = alloca %struct.ScmObj*, align 8
%v48081 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54695)
store volatile %struct.ScmObj* %v48081, %struct.ScmObj** %stackaddr$prim56158, align 8
%stackaddr$prim56159 = alloca %struct.ScmObj*, align 8
%current_45args54696 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54695)
store volatile %struct.ScmObj* %current_45args54696, %struct.ScmObj** %stackaddr$prim56159, align 8
%stackaddr$prim56160 = alloca %struct.ScmObj*, align 8
%r48080 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54696)
store volatile %struct.ScmObj* %r48080, %struct.ScmObj** %stackaddr$prim56160, align 8
%stackaddr$makeclosure56161 = alloca %struct.ScmObj*, align 8
%fptrToInt56162 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49559 to i64
%ae49559 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56162)
store volatile %struct.ScmObj* %ae49559, %struct.ScmObj** %stackaddr$makeclosure56161, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49559, %struct.ScmObj* %k48403, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49559, %struct.ScmObj* %r48080, i64 1)
%argslist54702$f480790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56163 = alloca %struct.ScmObj*, align 8
%argslist54702$f480791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v48081, %struct.ScmObj* %argslist54702$f480790)
store volatile %struct.ScmObj* %argslist54702$f480791, %struct.ScmObj** %stackaddr$prim56163, align 8
%stackaddr$prim56164 = alloca %struct.ScmObj*, align 8
%argslist54702$f480792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49559, %struct.ScmObj* %argslist54702$f480791)
store volatile %struct.ScmObj* %argslist54702$f480792, %struct.ScmObj** %stackaddr$prim56164, align 8
%clofunc56165 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48079)
musttail call tailcc void %clofunc56165(%struct.ScmObj* %f48079, %struct.ScmObj* %argslist54702$f480792)
ret void
}

define tailcc void @proc_clo$ae49559(%struct.ScmObj* %env$ae49559,%struct.ScmObj* %current_45args54698) {
%stackaddr$env-ref56166 = alloca %struct.ScmObj*, align 8
%k48403 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49559, i64 0)
store %struct.ScmObj* %k48403, %struct.ScmObj** %stackaddr$env-ref56166
%stackaddr$env-ref56167 = alloca %struct.ScmObj*, align 8
%r48080 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49559, i64 1)
store %struct.ScmObj* %r48080, %struct.ScmObj** %stackaddr$env-ref56167
%stackaddr$prim56168 = alloca %struct.ScmObj*, align 8
%_95k48404 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54698)
store volatile %struct.ScmObj* %_95k48404, %struct.ScmObj** %stackaddr$prim56168, align 8
%stackaddr$prim56169 = alloca %struct.ScmObj*, align 8
%current_45args54699 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54698)
store volatile %struct.ScmObj* %current_45args54699, %struct.ScmObj** %stackaddr$prim56169, align 8
%stackaddr$prim56170 = alloca %struct.ScmObj*, align 8
%anf_45bind48199 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54699)
store volatile %struct.ScmObj* %anf_45bind48199, %struct.ScmObj** %stackaddr$prim56170, align 8
%stackaddr$prim56171 = alloca %struct.ScmObj*, align 8
%cpsprim48405 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48199, %struct.ScmObj* %r48080)
store volatile %struct.ScmObj* %cpsprim48405, %struct.ScmObj** %stackaddr$prim56171, align 8
%ae49564 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54701$k484030 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56172 = alloca %struct.ScmObj*, align 8
%argslist54701$k484031 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48405, %struct.ScmObj* %argslist54701$k484030)
store volatile %struct.ScmObj* %argslist54701$k484031, %struct.ScmObj** %stackaddr$prim56172, align 8
%stackaddr$prim56173 = alloca %struct.ScmObj*, align 8
%argslist54701$k484032 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49564, %struct.ScmObj* %argslist54701$k484031)
store volatile %struct.ScmObj* %argslist54701$k484032, %struct.ScmObj** %stackaddr$prim56173, align 8
%clofunc56174 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48403)
musttail call tailcc void %clofunc56174(%struct.ScmObj* %k48403, %struct.ScmObj* %argslist54701$k484032)
ret void
}

define tailcc void @proc_clo$ae49168(%struct.ScmObj* %env$ae49168,%struct.ScmObj* %current_45args54706) {
%stackaddr$env-ref56175 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49168, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56175
%stackaddr$env-ref56176 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49168, i64 1)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref56176
%stackaddr$prim56177 = alloca %struct.ScmObj*, align 8
%k48406 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54706)
store volatile %struct.ScmObj* %k48406, %struct.ScmObj** %stackaddr$prim56177, align 8
%stackaddr$prim56178 = alloca %struct.ScmObj*, align 8
%current_45args54707 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54706)
store volatile %struct.ScmObj* %current_45args54707, %struct.ScmObj** %stackaddr$prim56178, align 8
%stackaddr$prim56179 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54707)
store volatile %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$prim56179, align 8
%ae49170 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56180 = alloca %struct.ScmObj*, align 8
%fptrToInt56181 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49171 to i64
%ae49171 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56181)
store volatile %struct.ScmObj* %ae49171, %struct.ScmObj** %stackaddr$makeclosure56180, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49171, %struct.ScmObj* %_37foldr48052, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49171, %struct.ScmObj* %_37foldr148046, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49171, %struct.ScmObj* %_37map148042, i64 2)
%argslist54764$k484060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56182 = alloca %struct.ScmObj*, align 8
%argslist54764$k484061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49171, %struct.ScmObj* %argslist54764$k484060)
store volatile %struct.ScmObj* %argslist54764$k484061, %struct.ScmObj** %stackaddr$prim56182, align 8
%stackaddr$prim56183 = alloca %struct.ScmObj*, align 8
%argslist54764$k484062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49170, %struct.ScmObj* %argslist54764$k484061)
store volatile %struct.ScmObj* %argslist54764$k484062, %struct.ScmObj** %stackaddr$prim56183, align 8
%clofunc56184 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48406)
musttail call tailcc void %clofunc56184(%struct.ScmObj* %k48406, %struct.ScmObj* %argslist54764$k484062)
ret void
}

define tailcc void @proc_clo$ae49171(%struct.ScmObj* %env$ae49171,%struct.ScmObj* %args4805348407) {
%stackaddr$env-ref56185 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49171, i64 0)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref56185
%stackaddr$env-ref56186 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49171, i64 1)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56186
%stackaddr$env-ref56187 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49171, i64 2)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref56187
%stackaddr$prim56188 = alloca %struct.ScmObj*, align 8
%k48408 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4805348407)
store volatile %struct.ScmObj* %k48408, %struct.ScmObj** %stackaddr$prim56188, align 8
%stackaddr$prim56189 = alloca %struct.ScmObj*, align 8
%args48053 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4805348407)
store volatile %struct.ScmObj* %args48053, %struct.ScmObj** %stackaddr$prim56189, align 8
%stackaddr$prim56190 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48053)
store volatile %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$prim56190, align 8
%stackaddr$prim56191 = alloca %struct.ScmObj*, align 8
%anf_45bind48186 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48053)
store volatile %struct.ScmObj* %anf_45bind48186, %struct.ScmObj** %stackaddr$prim56191, align 8
%stackaddr$prim56192 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48186)
store volatile %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$prim56192, align 8
%stackaddr$prim56193 = alloca %struct.ScmObj*, align 8
%anf_45bind48187 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48053)
store volatile %struct.ScmObj* %anf_45bind48187, %struct.ScmObj** %stackaddr$prim56193, align 8
%stackaddr$prim56194 = alloca %struct.ScmObj*, align 8
%lsts48054 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48187)
store volatile %struct.ScmObj* %lsts48054, %struct.ScmObj** %stackaddr$prim56194, align 8
%stackaddr$makeclosure56195 = alloca %struct.ScmObj*, align 8
%fptrToInt56196 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49179 to i64
%ae49179 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56196)
store volatile %struct.ScmObj* %ae49179, %struct.ScmObj** %stackaddr$makeclosure56195, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49179, %struct.ScmObj* %k48408, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49179, %struct.ScmObj* %f48056, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49179, %struct.ScmObj* %acc48055, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49179, %struct.ScmObj* %lsts48054, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49179, %struct.ScmObj* %_37foldr48052, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49179, %struct.ScmObj* %_37foldr148046, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49179, %struct.ScmObj* %_37map148042, i64 6)
%ae49180 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56197 = alloca %struct.ScmObj*, align 8
%fptrToInt56198 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49181 to i64
%ae49181 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56198)
store volatile %struct.ScmObj* %ae49181, %struct.ScmObj** %stackaddr$makeclosure56197, align 8
%argslist54763$ae491790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56199 = alloca %struct.ScmObj*, align 8
%argslist54763$ae491791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49181, %struct.ScmObj* %argslist54763$ae491790)
store volatile %struct.ScmObj* %argslist54763$ae491791, %struct.ScmObj** %stackaddr$prim56199, align 8
%stackaddr$prim56200 = alloca %struct.ScmObj*, align 8
%argslist54763$ae491792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49180, %struct.ScmObj* %argslist54763$ae491791)
store volatile %struct.ScmObj* %argslist54763$ae491792, %struct.ScmObj** %stackaddr$prim56200, align 8
%clofunc56201 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49179)
musttail call tailcc void %clofunc56201(%struct.ScmObj* %ae49179, %struct.ScmObj* %argslist54763$ae491792)
ret void
}

define tailcc void @proc_clo$ae49179(%struct.ScmObj* %env$ae49179,%struct.ScmObj* %current_45args54709) {
%stackaddr$env-ref56202 = alloca %struct.ScmObj*, align 8
%k48408 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49179, i64 0)
store %struct.ScmObj* %k48408, %struct.ScmObj** %stackaddr$env-ref56202
%stackaddr$env-ref56203 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49179, i64 1)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref56203
%stackaddr$env-ref56204 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49179, i64 2)
store %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$env-ref56204
%stackaddr$env-ref56205 = alloca %struct.ScmObj*, align 8
%lsts48054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49179, i64 3)
store %struct.ScmObj* %lsts48054, %struct.ScmObj** %stackaddr$env-ref56205
%stackaddr$env-ref56206 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49179, i64 4)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref56206
%stackaddr$env-ref56207 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49179, i64 5)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56207
%stackaddr$env-ref56208 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49179, i64 6)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref56208
%stackaddr$prim56209 = alloca %struct.ScmObj*, align 8
%_95k48409 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54709)
store volatile %struct.ScmObj* %_95k48409, %struct.ScmObj** %stackaddr$prim56209, align 8
%stackaddr$prim56210 = alloca %struct.ScmObj*, align 8
%current_45args54710 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54709)
store volatile %struct.ScmObj* %current_45args54710, %struct.ScmObj** %stackaddr$prim56210, align 8
%stackaddr$prim56211 = alloca %struct.ScmObj*, align 8
%anf_45bind48188 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54710)
store volatile %struct.ScmObj* %anf_45bind48188, %struct.ScmObj** %stackaddr$prim56211, align 8
%stackaddr$makeclosure56212 = alloca %struct.ScmObj*, align 8
%fptrToInt56213 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49211 to i64
%ae49211 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56213)
store volatile %struct.ScmObj* %ae49211, %struct.ScmObj** %stackaddr$makeclosure56212, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49211, %struct.ScmObj* %k48408, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49211, %struct.ScmObj* %f48056, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49211, %struct.ScmObj* %acc48055, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49211, %struct.ScmObj* %lsts48054, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49211, %struct.ScmObj* %_37foldr48052, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49211, %struct.ScmObj* %_37foldr148046, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49211, %struct.ScmObj* %_37map148042, i64 6)
%ae49213 = call %struct.ScmObj* @const_init_false()
%argslist54756$_37foldr1480460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56214 = alloca %struct.ScmObj*, align 8
%argslist54756$_37foldr1480461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48054, %struct.ScmObj* %argslist54756$_37foldr1480460)
store volatile %struct.ScmObj* %argslist54756$_37foldr1480461, %struct.ScmObj** %stackaddr$prim56214, align 8
%stackaddr$prim56215 = alloca %struct.ScmObj*, align 8
%argslist54756$_37foldr1480462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49213, %struct.ScmObj* %argslist54756$_37foldr1480461)
store volatile %struct.ScmObj* %argslist54756$_37foldr1480462, %struct.ScmObj** %stackaddr$prim56215, align 8
%stackaddr$prim56216 = alloca %struct.ScmObj*, align 8
%argslist54756$_37foldr1480463 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48188, %struct.ScmObj* %argslist54756$_37foldr1480462)
store volatile %struct.ScmObj* %argslist54756$_37foldr1480463, %struct.ScmObj** %stackaddr$prim56216, align 8
%stackaddr$prim56217 = alloca %struct.ScmObj*, align 8
%argslist54756$_37foldr1480464 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49211, %struct.ScmObj* %argslist54756$_37foldr1480463)
store volatile %struct.ScmObj* %argslist54756$_37foldr1480464, %struct.ScmObj** %stackaddr$prim56217, align 8
%clofunc56218 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148046)
musttail call tailcc void %clofunc56218(%struct.ScmObj* %_37foldr148046, %struct.ScmObj* %argslist54756$_37foldr1480464)
ret void
}

define tailcc void @proc_clo$ae49211(%struct.ScmObj* %env$ae49211,%struct.ScmObj* %current_45args54712) {
%stackaddr$env-ref56219 = alloca %struct.ScmObj*, align 8
%k48408 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49211, i64 0)
store %struct.ScmObj* %k48408, %struct.ScmObj** %stackaddr$env-ref56219
%stackaddr$env-ref56220 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49211, i64 1)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref56220
%stackaddr$env-ref56221 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49211, i64 2)
store %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$env-ref56221
%stackaddr$env-ref56222 = alloca %struct.ScmObj*, align 8
%lsts48054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49211, i64 3)
store %struct.ScmObj* %lsts48054, %struct.ScmObj** %stackaddr$env-ref56222
%stackaddr$env-ref56223 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49211, i64 4)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref56223
%stackaddr$env-ref56224 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49211, i64 5)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56224
%stackaddr$env-ref56225 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49211, i64 6)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref56225
%stackaddr$prim56226 = alloca %struct.ScmObj*, align 8
%_95k48410 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54712)
store volatile %struct.ScmObj* %_95k48410, %struct.ScmObj** %stackaddr$prim56226, align 8
%stackaddr$prim56227 = alloca %struct.ScmObj*, align 8
%current_45args54713 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54712)
store volatile %struct.ScmObj* %current_45args54713, %struct.ScmObj** %stackaddr$prim56227, align 8
%stackaddr$prim56228 = alloca %struct.ScmObj*, align 8
%anf_45bind48189 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54713)
store volatile %struct.ScmObj* %anf_45bind48189, %struct.ScmObj** %stackaddr$prim56228, align 8
%truthy$cmp56229 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48189)
%cmp$cmp56229 = icmp eq i64 %truthy$cmp56229, 1
br i1 %cmp$cmp56229, label %truebranch$cmp56229, label %falsebranch$cmp56229
truebranch$cmp56229:
%ae49222 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54715$k484080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56230 = alloca %struct.ScmObj*, align 8
%argslist54715$k484081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48055, %struct.ScmObj* %argslist54715$k484080)
store volatile %struct.ScmObj* %argslist54715$k484081, %struct.ScmObj** %stackaddr$prim56230, align 8
%stackaddr$prim56231 = alloca %struct.ScmObj*, align 8
%argslist54715$k484082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49222, %struct.ScmObj* %argslist54715$k484081)
store volatile %struct.ScmObj* %argslist54715$k484082, %struct.ScmObj** %stackaddr$prim56231, align 8
%clofunc56232 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48408)
musttail call tailcc void %clofunc56232(%struct.ScmObj* %k48408, %struct.ScmObj* %argslist54715$k484082)
ret void
falsebranch$cmp56229:
%stackaddr$makeclosure56233 = alloca %struct.ScmObj*, align 8
%fptrToInt56234 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49227 to i64
%ae49227 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56234)
store volatile %struct.ScmObj* %ae49227, %struct.ScmObj** %stackaddr$makeclosure56233, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49227, %struct.ScmObj* %k48408, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49227, %struct.ScmObj* %f48056, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49227, %struct.ScmObj* %acc48055, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49227, %struct.ScmObj* %lsts48054, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49227, %struct.ScmObj* %_37foldr48052, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49227, %struct.ScmObj* %_37foldr148046, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49227, %struct.ScmObj* %_37map148042, i64 6)
%ae49228 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56235 = alloca %struct.ScmObj*, align 8
%fptrToInt56236 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49229 to i64
%ae49229 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56236)
store volatile %struct.ScmObj* %ae49229, %struct.ScmObj** %stackaddr$makeclosure56235, align 8
%argslist54755$ae492270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56237 = alloca %struct.ScmObj*, align 8
%argslist54755$ae492271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49229, %struct.ScmObj* %argslist54755$ae492270)
store volatile %struct.ScmObj* %argslist54755$ae492271, %struct.ScmObj** %stackaddr$prim56237, align 8
%stackaddr$prim56238 = alloca %struct.ScmObj*, align 8
%argslist54755$ae492272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49228, %struct.ScmObj* %argslist54755$ae492271)
store volatile %struct.ScmObj* %argslist54755$ae492272, %struct.ScmObj** %stackaddr$prim56238, align 8
%clofunc56239 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49227)
musttail call tailcc void %clofunc56239(%struct.ScmObj* %ae49227, %struct.ScmObj* %argslist54755$ae492272)
ret void
}

define tailcc void @proc_clo$ae49227(%struct.ScmObj* %env$ae49227,%struct.ScmObj* %current_45args54716) {
%stackaddr$env-ref56240 = alloca %struct.ScmObj*, align 8
%k48408 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49227, i64 0)
store %struct.ScmObj* %k48408, %struct.ScmObj** %stackaddr$env-ref56240
%stackaddr$env-ref56241 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49227, i64 1)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref56241
%stackaddr$env-ref56242 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49227, i64 2)
store %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$env-ref56242
%stackaddr$env-ref56243 = alloca %struct.ScmObj*, align 8
%lsts48054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49227, i64 3)
store %struct.ScmObj* %lsts48054, %struct.ScmObj** %stackaddr$env-ref56243
%stackaddr$env-ref56244 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49227, i64 4)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref56244
%stackaddr$env-ref56245 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49227, i64 5)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56245
%stackaddr$env-ref56246 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49227, i64 6)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref56246
%stackaddr$prim56247 = alloca %struct.ScmObj*, align 8
%_95k48411 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54716)
store volatile %struct.ScmObj* %_95k48411, %struct.ScmObj** %stackaddr$prim56247, align 8
%stackaddr$prim56248 = alloca %struct.ScmObj*, align 8
%current_45args54717 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54716)
store volatile %struct.ScmObj* %current_45args54717, %struct.ScmObj** %stackaddr$prim56248, align 8
%stackaddr$prim56249 = alloca %struct.ScmObj*, align 8
%anf_45bind48190 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54717)
store volatile %struct.ScmObj* %anf_45bind48190, %struct.ScmObj** %stackaddr$prim56249, align 8
%stackaddr$makeclosure56250 = alloca %struct.ScmObj*, align 8
%fptrToInt56251 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49248 to i64
%ae49248 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56251)
store volatile %struct.ScmObj* %ae49248, %struct.ScmObj** %stackaddr$makeclosure56250, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49248, %struct.ScmObj* %k48408, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49248, %struct.ScmObj* %f48056, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49248, %struct.ScmObj* %acc48055, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49248, %struct.ScmObj* %lsts48054, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49248, %struct.ScmObj* %_37foldr48052, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49248, %struct.ScmObj* %_37foldr148046, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49248, %struct.ScmObj* %_37map148042, i64 6)
%argslist54750$_37map1480420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56252 = alloca %struct.ScmObj*, align 8
%argslist54750$_37map1480421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48054, %struct.ScmObj* %argslist54750$_37map1480420)
store volatile %struct.ScmObj* %argslist54750$_37map1480421, %struct.ScmObj** %stackaddr$prim56252, align 8
%stackaddr$prim56253 = alloca %struct.ScmObj*, align 8
%argslist54750$_37map1480422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48190, %struct.ScmObj* %argslist54750$_37map1480421)
store volatile %struct.ScmObj* %argslist54750$_37map1480422, %struct.ScmObj** %stackaddr$prim56253, align 8
%stackaddr$prim56254 = alloca %struct.ScmObj*, align 8
%argslist54750$_37map1480423 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49248, %struct.ScmObj* %argslist54750$_37map1480422)
store volatile %struct.ScmObj* %argslist54750$_37map1480423, %struct.ScmObj** %stackaddr$prim56254, align 8
%clofunc56255 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148042)
musttail call tailcc void %clofunc56255(%struct.ScmObj* %_37map148042, %struct.ScmObj* %argslist54750$_37map1480423)
ret void
}

define tailcc void @proc_clo$ae49248(%struct.ScmObj* %env$ae49248,%struct.ScmObj* %current_45args54719) {
%stackaddr$env-ref56256 = alloca %struct.ScmObj*, align 8
%k48408 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49248, i64 0)
store %struct.ScmObj* %k48408, %struct.ScmObj** %stackaddr$env-ref56256
%stackaddr$env-ref56257 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49248, i64 1)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref56257
%stackaddr$env-ref56258 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49248, i64 2)
store %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$env-ref56258
%stackaddr$env-ref56259 = alloca %struct.ScmObj*, align 8
%lsts48054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49248, i64 3)
store %struct.ScmObj* %lsts48054, %struct.ScmObj** %stackaddr$env-ref56259
%stackaddr$env-ref56260 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49248, i64 4)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref56260
%stackaddr$env-ref56261 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49248, i64 5)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56261
%stackaddr$env-ref56262 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49248, i64 6)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref56262
%stackaddr$prim56263 = alloca %struct.ScmObj*, align 8
%_95k48412 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54719)
store volatile %struct.ScmObj* %_95k48412, %struct.ScmObj** %stackaddr$prim56263, align 8
%stackaddr$prim56264 = alloca %struct.ScmObj*, align 8
%current_45args54720 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54719)
store volatile %struct.ScmObj* %current_45args54720, %struct.ScmObj** %stackaddr$prim56264, align 8
%stackaddr$prim56265 = alloca %struct.ScmObj*, align 8
%lsts_4348061 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54720)
store volatile %struct.ScmObj* %lsts_4348061, %struct.ScmObj** %stackaddr$prim56265, align 8
%stackaddr$makeclosure56266 = alloca %struct.ScmObj*, align 8
%fptrToInt56267 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49251 to i64
%ae49251 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt56267)
store volatile %struct.ScmObj* %ae49251, %struct.ScmObj** %stackaddr$makeclosure56266, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49251, %struct.ScmObj* %k48408, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49251, %struct.ScmObj* %f48056, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49251, %struct.ScmObj* %acc48055, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49251, %struct.ScmObj* %lsts48054, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49251, %struct.ScmObj* %_37foldr48052, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49251, %struct.ScmObj* %_37foldr148046, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49251, %struct.ScmObj* %lsts_4348061, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae49251, %struct.ScmObj* %_37map148042, i64 7)
%ae49252 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56268 = alloca %struct.ScmObj*, align 8
%fptrToInt56269 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49253 to i64
%ae49253 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56269)
store volatile %struct.ScmObj* %ae49253, %struct.ScmObj** %stackaddr$makeclosure56268, align 8
%argslist54749$ae492510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56270 = alloca %struct.ScmObj*, align 8
%argslist54749$ae492511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49253, %struct.ScmObj* %argslist54749$ae492510)
store volatile %struct.ScmObj* %argslist54749$ae492511, %struct.ScmObj** %stackaddr$prim56270, align 8
%stackaddr$prim56271 = alloca %struct.ScmObj*, align 8
%argslist54749$ae492512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49252, %struct.ScmObj* %argslist54749$ae492511)
store volatile %struct.ScmObj* %argslist54749$ae492512, %struct.ScmObj** %stackaddr$prim56271, align 8
%clofunc56272 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49251)
musttail call tailcc void %clofunc56272(%struct.ScmObj* %ae49251, %struct.ScmObj* %argslist54749$ae492512)
ret void
}

define tailcc void @proc_clo$ae49251(%struct.ScmObj* %env$ae49251,%struct.ScmObj* %current_45args54722) {
%stackaddr$env-ref56273 = alloca %struct.ScmObj*, align 8
%k48408 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49251, i64 0)
store %struct.ScmObj* %k48408, %struct.ScmObj** %stackaddr$env-ref56273
%stackaddr$env-ref56274 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49251, i64 1)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref56274
%stackaddr$env-ref56275 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49251, i64 2)
store %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$env-ref56275
%stackaddr$env-ref56276 = alloca %struct.ScmObj*, align 8
%lsts48054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49251, i64 3)
store %struct.ScmObj* %lsts48054, %struct.ScmObj** %stackaddr$env-ref56276
%stackaddr$env-ref56277 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49251, i64 4)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref56277
%stackaddr$env-ref56278 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49251, i64 5)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56278
%stackaddr$env-ref56279 = alloca %struct.ScmObj*, align 8
%lsts_4348061 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49251, i64 6)
store %struct.ScmObj* %lsts_4348061, %struct.ScmObj** %stackaddr$env-ref56279
%stackaddr$env-ref56280 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49251, i64 7)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref56280
%stackaddr$prim56281 = alloca %struct.ScmObj*, align 8
%_95k48413 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54722)
store volatile %struct.ScmObj* %_95k48413, %struct.ScmObj** %stackaddr$prim56281, align 8
%stackaddr$prim56282 = alloca %struct.ScmObj*, align 8
%current_45args54723 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54722)
store volatile %struct.ScmObj* %current_45args54723, %struct.ScmObj** %stackaddr$prim56282, align 8
%stackaddr$prim56283 = alloca %struct.ScmObj*, align 8
%anf_45bind48191 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54723)
store volatile %struct.ScmObj* %anf_45bind48191, %struct.ScmObj** %stackaddr$prim56283, align 8
%stackaddr$makeclosure56284 = alloca %struct.ScmObj*, align 8
%fptrToInt56285 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49272 to i64
%ae49272 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt56285)
store volatile %struct.ScmObj* %ae49272, %struct.ScmObj** %stackaddr$makeclosure56284, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49272, %struct.ScmObj* %k48408, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49272, %struct.ScmObj* %f48056, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49272, %struct.ScmObj* %acc48055, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49272, %struct.ScmObj* %_37foldr48052, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49272, %struct.ScmObj* %_37foldr148046, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49272, %struct.ScmObj* %lsts_4348061, i64 5)
%argslist54744$_37map1480420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56286 = alloca %struct.ScmObj*, align 8
%argslist54744$_37map1480421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48054, %struct.ScmObj* %argslist54744$_37map1480420)
store volatile %struct.ScmObj* %argslist54744$_37map1480421, %struct.ScmObj** %stackaddr$prim56286, align 8
%stackaddr$prim56287 = alloca %struct.ScmObj*, align 8
%argslist54744$_37map1480422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48191, %struct.ScmObj* %argslist54744$_37map1480421)
store volatile %struct.ScmObj* %argslist54744$_37map1480422, %struct.ScmObj** %stackaddr$prim56287, align 8
%stackaddr$prim56288 = alloca %struct.ScmObj*, align 8
%argslist54744$_37map1480423 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49272, %struct.ScmObj* %argslist54744$_37map1480422)
store volatile %struct.ScmObj* %argslist54744$_37map1480423, %struct.ScmObj** %stackaddr$prim56288, align 8
%clofunc56289 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148042)
musttail call tailcc void %clofunc56289(%struct.ScmObj* %_37map148042, %struct.ScmObj* %argslist54744$_37map1480423)
ret void
}

define tailcc void @proc_clo$ae49272(%struct.ScmObj* %env$ae49272,%struct.ScmObj* %current_45args54725) {
%stackaddr$env-ref56290 = alloca %struct.ScmObj*, align 8
%k48408 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49272, i64 0)
store %struct.ScmObj* %k48408, %struct.ScmObj** %stackaddr$env-ref56290
%stackaddr$env-ref56291 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49272, i64 1)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref56291
%stackaddr$env-ref56292 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49272, i64 2)
store %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$env-ref56292
%stackaddr$env-ref56293 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49272, i64 3)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref56293
%stackaddr$env-ref56294 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49272, i64 4)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56294
%stackaddr$env-ref56295 = alloca %struct.ScmObj*, align 8
%lsts_4348061 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49272, i64 5)
store %struct.ScmObj* %lsts_4348061, %struct.ScmObj** %stackaddr$env-ref56295
%stackaddr$prim56296 = alloca %struct.ScmObj*, align 8
%_95k48414 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54725)
store volatile %struct.ScmObj* %_95k48414, %struct.ScmObj** %stackaddr$prim56296, align 8
%stackaddr$prim56297 = alloca %struct.ScmObj*, align 8
%current_45args54726 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54725)
store volatile %struct.ScmObj* %current_45args54726, %struct.ScmObj** %stackaddr$prim56297, align 8
%stackaddr$prim56298 = alloca %struct.ScmObj*, align 8
%vs48059 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54726)
store volatile %struct.ScmObj* %vs48059, %struct.ScmObj** %stackaddr$prim56298, align 8
%stackaddr$makeclosure56299 = alloca %struct.ScmObj*, align 8
%fptrToInt56300 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49275 to i64
%ae49275 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56300)
store volatile %struct.ScmObj* %ae49275, %struct.ScmObj** %stackaddr$makeclosure56299, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49275, %struct.ScmObj* %k48408, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49275, %struct.ScmObj* %f48056, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49275, %struct.ScmObj* %acc48055, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49275, %struct.ScmObj* %_37foldr48052, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49275, %struct.ScmObj* %_37foldr148046, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49275, %struct.ScmObj* %lsts_4348061, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49275, %struct.ScmObj* %vs48059, i64 6)
%ae49276 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56301 = alloca %struct.ScmObj*, align 8
%fptrToInt56302 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49277 to i64
%ae49277 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56302)
store volatile %struct.ScmObj* %ae49277, %struct.ScmObj** %stackaddr$makeclosure56301, align 8
%argslist54743$ae492750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56303 = alloca %struct.ScmObj*, align 8
%argslist54743$ae492751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49277, %struct.ScmObj* %argslist54743$ae492750)
store volatile %struct.ScmObj* %argslist54743$ae492751, %struct.ScmObj** %stackaddr$prim56303, align 8
%stackaddr$prim56304 = alloca %struct.ScmObj*, align 8
%argslist54743$ae492752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49276, %struct.ScmObj* %argslist54743$ae492751)
store volatile %struct.ScmObj* %argslist54743$ae492752, %struct.ScmObj** %stackaddr$prim56304, align 8
%clofunc56305 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49275)
musttail call tailcc void %clofunc56305(%struct.ScmObj* %ae49275, %struct.ScmObj* %argslist54743$ae492752)
ret void
}

define tailcc void @proc_clo$ae49275(%struct.ScmObj* %env$ae49275,%struct.ScmObj* %current_45args54728) {
%stackaddr$env-ref56306 = alloca %struct.ScmObj*, align 8
%k48408 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49275, i64 0)
store %struct.ScmObj* %k48408, %struct.ScmObj** %stackaddr$env-ref56306
%stackaddr$env-ref56307 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49275, i64 1)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref56307
%stackaddr$env-ref56308 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49275, i64 2)
store %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$env-ref56308
%stackaddr$env-ref56309 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49275, i64 3)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref56309
%stackaddr$env-ref56310 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49275, i64 4)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56310
%stackaddr$env-ref56311 = alloca %struct.ScmObj*, align 8
%lsts_4348061 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49275, i64 5)
store %struct.ScmObj* %lsts_4348061, %struct.ScmObj** %stackaddr$env-ref56311
%stackaddr$env-ref56312 = alloca %struct.ScmObj*, align 8
%vs48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49275, i64 6)
store %struct.ScmObj* %vs48059, %struct.ScmObj** %stackaddr$env-ref56312
%stackaddr$prim56313 = alloca %struct.ScmObj*, align 8
%_95k48415 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54728)
store volatile %struct.ScmObj* %_95k48415, %struct.ScmObj** %stackaddr$prim56313, align 8
%stackaddr$prim56314 = alloca %struct.ScmObj*, align 8
%current_45args54729 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54728)
store volatile %struct.ScmObj* %current_45args54729, %struct.ScmObj** %stackaddr$prim56314, align 8
%stackaddr$prim56315 = alloca %struct.ScmObj*, align 8
%anf_45bind48192 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54729)
store volatile %struct.ScmObj* %anf_45bind48192, %struct.ScmObj** %stackaddr$prim56315, align 8
%stackaddr$prim56316 = alloca %struct.ScmObj*, align 8
%anf_45bind48193 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48055, %struct.ScmObj* %lsts_4348061)
store volatile %struct.ScmObj* %anf_45bind48193, %struct.ScmObj** %stackaddr$prim56316, align 8
%stackaddr$prim56317 = alloca %struct.ScmObj*, align 8
%anf_45bind48194 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48056, %struct.ScmObj* %anf_45bind48193)
store volatile %struct.ScmObj* %anf_45bind48194, %struct.ScmObj** %stackaddr$prim56317, align 8
%stackaddr$makeclosure56318 = alloca %struct.ScmObj*, align 8
%fptrToInt56319 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49301 to i64
%ae49301 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt56319)
store volatile %struct.ScmObj* %ae49301, %struct.ScmObj** %stackaddr$makeclosure56318, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49301, %struct.ScmObj* %k48408, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49301, %struct.ScmObj* %f48056, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49301, %struct.ScmObj* %anf_45bind48192, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49301, %struct.ScmObj* %_37foldr148046, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49301, %struct.ScmObj* %vs48059, i64 4)
%stackaddr$prim56320 = alloca %struct.ScmObj*, align 8
%cpsargs48419 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49301, %struct.ScmObj* %anf_45bind48194)
store volatile %struct.ScmObj* %cpsargs48419, %struct.ScmObj** %stackaddr$prim56320, align 8
%clofunc56321 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr48052)
musttail call tailcc void %clofunc56321(%struct.ScmObj* %_37foldr48052, %struct.ScmObj* %cpsargs48419)
ret void
}

define tailcc void @proc_clo$ae49301(%struct.ScmObj* %env$ae49301,%struct.ScmObj* %current_45args54731) {
%stackaddr$env-ref56322 = alloca %struct.ScmObj*, align 8
%k48408 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49301, i64 0)
store %struct.ScmObj* %k48408, %struct.ScmObj** %stackaddr$env-ref56322
%stackaddr$env-ref56323 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49301, i64 1)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref56323
%stackaddr$env-ref56324 = alloca %struct.ScmObj*, align 8
%anf_45bind48192 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49301, i64 2)
store %struct.ScmObj* %anf_45bind48192, %struct.ScmObj** %stackaddr$env-ref56324
%stackaddr$env-ref56325 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49301, i64 3)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56325
%stackaddr$env-ref56326 = alloca %struct.ScmObj*, align 8
%vs48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49301, i64 4)
store %struct.ScmObj* %vs48059, %struct.ScmObj** %stackaddr$env-ref56326
%stackaddr$prim56327 = alloca %struct.ScmObj*, align 8
%_95k48416 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54731)
store volatile %struct.ScmObj* %_95k48416, %struct.ScmObj** %stackaddr$prim56327, align 8
%stackaddr$prim56328 = alloca %struct.ScmObj*, align 8
%current_45args54732 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54731)
store volatile %struct.ScmObj* %current_45args54732, %struct.ScmObj** %stackaddr$prim56328, align 8
%stackaddr$prim56329 = alloca %struct.ScmObj*, align 8
%anf_45bind48195 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54732)
store volatile %struct.ScmObj* %anf_45bind48195, %struct.ScmObj** %stackaddr$prim56329, align 8
%ae49306 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56330 = alloca %struct.ScmObj*, align 8
%anf_45bind48196 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48195, %struct.ScmObj* %ae49306)
store volatile %struct.ScmObj* %anf_45bind48196, %struct.ScmObj** %stackaddr$prim56330, align 8
%stackaddr$makeclosure56331 = alloca %struct.ScmObj*, align 8
%fptrToInt56332 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49308 to i64
%ae49308 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56332)
store volatile %struct.ScmObj* %ae49308, %struct.ScmObj** %stackaddr$makeclosure56331, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49308, %struct.ScmObj* %k48408, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49308, %struct.ScmObj* %f48056, i64 1)
%argslist54737$_37foldr1480460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56333 = alloca %struct.ScmObj*, align 8
%argslist54737$_37foldr1480461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs48059, %struct.ScmObj* %argslist54737$_37foldr1480460)
store volatile %struct.ScmObj* %argslist54737$_37foldr1480461, %struct.ScmObj** %stackaddr$prim56333, align 8
%stackaddr$prim56334 = alloca %struct.ScmObj*, align 8
%argslist54737$_37foldr1480462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48196, %struct.ScmObj* %argslist54737$_37foldr1480461)
store volatile %struct.ScmObj* %argslist54737$_37foldr1480462, %struct.ScmObj** %stackaddr$prim56334, align 8
%stackaddr$prim56335 = alloca %struct.ScmObj*, align 8
%argslist54737$_37foldr1480463 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48192, %struct.ScmObj* %argslist54737$_37foldr1480462)
store volatile %struct.ScmObj* %argslist54737$_37foldr1480463, %struct.ScmObj** %stackaddr$prim56335, align 8
%stackaddr$prim56336 = alloca %struct.ScmObj*, align 8
%argslist54737$_37foldr1480464 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49308, %struct.ScmObj* %argslist54737$_37foldr1480463)
store volatile %struct.ScmObj* %argslist54737$_37foldr1480464, %struct.ScmObj** %stackaddr$prim56336, align 8
%clofunc56337 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148046)
musttail call tailcc void %clofunc56337(%struct.ScmObj* %_37foldr148046, %struct.ScmObj* %argslist54737$_37foldr1480464)
ret void
}

define tailcc void @proc_clo$ae49308(%struct.ScmObj* %env$ae49308,%struct.ScmObj* %current_45args54734) {
%stackaddr$env-ref56338 = alloca %struct.ScmObj*, align 8
%k48408 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49308, i64 0)
store %struct.ScmObj* %k48408, %struct.ScmObj** %stackaddr$env-ref56338
%stackaddr$env-ref56339 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49308, i64 1)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref56339
%stackaddr$prim56340 = alloca %struct.ScmObj*, align 8
%_95k48417 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54734)
store volatile %struct.ScmObj* %_95k48417, %struct.ScmObj** %stackaddr$prim56340, align 8
%stackaddr$prim56341 = alloca %struct.ScmObj*, align 8
%current_45args54735 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54734)
store volatile %struct.ScmObj* %current_45args54735, %struct.ScmObj** %stackaddr$prim56341, align 8
%stackaddr$prim56342 = alloca %struct.ScmObj*, align 8
%anf_45bind48197 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54735)
store volatile %struct.ScmObj* %anf_45bind48197, %struct.ScmObj** %stackaddr$prim56342, align 8
%stackaddr$prim56343 = alloca %struct.ScmObj*, align 8
%cpsargs48418 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48408, %struct.ScmObj* %anf_45bind48197)
store volatile %struct.ScmObj* %cpsargs48418, %struct.ScmObj** %stackaddr$prim56343, align 8
%clofunc56344 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48056)
musttail call tailcc void %clofunc56344(%struct.ScmObj* %f48056, %struct.ScmObj* %cpsargs48418)
ret void
}

define tailcc void @proc_clo$ae49277(%struct.ScmObj* %env$ae49277,%struct.ScmObj* %current_45args54738) {
%stackaddr$prim56345 = alloca %struct.ScmObj*, align 8
%k48420 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54738)
store volatile %struct.ScmObj* %k48420, %struct.ScmObj** %stackaddr$prim56345, align 8
%stackaddr$prim56346 = alloca %struct.ScmObj*, align 8
%current_45args54739 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54738)
store volatile %struct.ScmObj* %current_45args54739, %struct.ScmObj** %stackaddr$prim56346, align 8
%stackaddr$prim56347 = alloca %struct.ScmObj*, align 8
%a48064 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54739)
store volatile %struct.ScmObj* %a48064, %struct.ScmObj** %stackaddr$prim56347, align 8
%stackaddr$prim56348 = alloca %struct.ScmObj*, align 8
%current_45args54740 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54739)
store volatile %struct.ScmObj* %current_45args54740, %struct.ScmObj** %stackaddr$prim56348, align 8
%stackaddr$prim56349 = alloca %struct.ScmObj*, align 8
%b48063 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54740)
store volatile %struct.ScmObj* %b48063, %struct.ScmObj** %stackaddr$prim56349, align 8
%stackaddr$prim56350 = alloca %struct.ScmObj*, align 8
%cpsprim48421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a48064, %struct.ScmObj* %b48063)
store volatile %struct.ScmObj* %cpsprim48421, %struct.ScmObj** %stackaddr$prim56350, align 8
%ae49281 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54742$k484200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56351 = alloca %struct.ScmObj*, align 8
%argslist54742$k484201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48421, %struct.ScmObj* %argslist54742$k484200)
store volatile %struct.ScmObj* %argslist54742$k484201, %struct.ScmObj** %stackaddr$prim56351, align 8
%stackaddr$prim56352 = alloca %struct.ScmObj*, align 8
%argslist54742$k484202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49281, %struct.ScmObj* %argslist54742$k484201)
store volatile %struct.ScmObj* %argslist54742$k484202, %struct.ScmObj** %stackaddr$prim56352, align 8
%clofunc56353 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48420)
musttail call tailcc void %clofunc56353(%struct.ScmObj* %k48420, %struct.ScmObj* %argslist54742$k484202)
ret void
}

define tailcc void @proc_clo$ae49253(%struct.ScmObj* %env$ae49253,%struct.ScmObj* %current_45args54745) {
%stackaddr$prim56354 = alloca %struct.ScmObj*, align 8
%k48422 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54745)
store volatile %struct.ScmObj* %k48422, %struct.ScmObj** %stackaddr$prim56354, align 8
%stackaddr$prim56355 = alloca %struct.ScmObj*, align 8
%current_45args54746 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54745)
store volatile %struct.ScmObj* %current_45args54746, %struct.ScmObj** %stackaddr$prim56355, align 8
%stackaddr$prim56356 = alloca %struct.ScmObj*, align 8
%x48060 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54746)
store volatile %struct.ScmObj* %x48060, %struct.ScmObj** %stackaddr$prim56356, align 8
%stackaddr$prim56357 = alloca %struct.ScmObj*, align 8
%cpsprim48423 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48060)
store volatile %struct.ScmObj* %cpsprim48423, %struct.ScmObj** %stackaddr$prim56357, align 8
%ae49256 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54748$k484220 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56358 = alloca %struct.ScmObj*, align 8
%argslist54748$k484221 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48423, %struct.ScmObj* %argslist54748$k484220)
store volatile %struct.ScmObj* %argslist54748$k484221, %struct.ScmObj** %stackaddr$prim56358, align 8
%stackaddr$prim56359 = alloca %struct.ScmObj*, align 8
%argslist54748$k484222 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49256, %struct.ScmObj* %argslist54748$k484221)
store volatile %struct.ScmObj* %argslist54748$k484222, %struct.ScmObj** %stackaddr$prim56359, align 8
%clofunc56360 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48422)
musttail call tailcc void %clofunc56360(%struct.ScmObj* %k48422, %struct.ScmObj* %argslist54748$k484222)
ret void
}

define tailcc void @proc_clo$ae49229(%struct.ScmObj* %env$ae49229,%struct.ScmObj* %current_45args54751) {
%stackaddr$prim56361 = alloca %struct.ScmObj*, align 8
%k48424 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54751)
store volatile %struct.ScmObj* %k48424, %struct.ScmObj** %stackaddr$prim56361, align 8
%stackaddr$prim56362 = alloca %struct.ScmObj*, align 8
%current_45args54752 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54751)
store volatile %struct.ScmObj* %current_45args54752, %struct.ScmObj** %stackaddr$prim56362, align 8
%stackaddr$prim56363 = alloca %struct.ScmObj*, align 8
%x48062 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54752)
store volatile %struct.ScmObj* %x48062, %struct.ScmObj** %stackaddr$prim56363, align 8
%stackaddr$prim56364 = alloca %struct.ScmObj*, align 8
%cpsprim48425 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48062)
store volatile %struct.ScmObj* %cpsprim48425, %struct.ScmObj** %stackaddr$prim56364, align 8
%ae49232 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54754$k484240 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56365 = alloca %struct.ScmObj*, align 8
%argslist54754$k484241 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48425, %struct.ScmObj* %argslist54754$k484240)
store volatile %struct.ScmObj* %argslist54754$k484241, %struct.ScmObj** %stackaddr$prim56365, align 8
%stackaddr$prim56366 = alloca %struct.ScmObj*, align 8
%argslist54754$k484242 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49232, %struct.ScmObj* %argslist54754$k484241)
store volatile %struct.ScmObj* %argslist54754$k484242, %struct.ScmObj** %stackaddr$prim56366, align 8
%clofunc56367 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48424)
musttail call tailcc void %clofunc56367(%struct.ScmObj* %k48424, %struct.ScmObj* %argslist54754$k484242)
ret void
}

define tailcc void @proc_clo$ae49181(%struct.ScmObj* %env$ae49181,%struct.ScmObj* %current_45args54757) {
%stackaddr$prim56368 = alloca %struct.ScmObj*, align 8
%k48426 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54757)
store volatile %struct.ScmObj* %k48426, %struct.ScmObj** %stackaddr$prim56368, align 8
%stackaddr$prim56369 = alloca %struct.ScmObj*, align 8
%current_45args54758 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54757)
store volatile %struct.ScmObj* %current_45args54758, %struct.ScmObj** %stackaddr$prim56369, align 8
%stackaddr$prim56370 = alloca %struct.ScmObj*, align 8
%lst48058 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54758)
store volatile %struct.ScmObj* %lst48058, %struct.ScmObj** %stackaddr$prim56370, align 8
%stackaddr$prim56371 = alloca %struct.ScmObj*, align 8
%current_45args54759 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54758)
store volatile %struct.ScmObj* %current_45args54759, %struct.ScmObj** %stackaddr$prim56371, align 8
%stackaddr$prim56372 = alloca %struct.ScmObj*, align 8
%b48057 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54759)
store volatile %struct.ScmObj* %b48057, %struct.ScmObj** %stackaddr$prim56372, align 8
%truthy$cmp56373 = call i64 @is_truthy_value(%struct.ScmObj* %b48057)
%cmp$cmp56373 = icmp eq i64 %truthy$cmp56373, 1
br i1 %cmp$cmp56373, label %truebranch$cmp56373, label %falsebranch$cmp56373
truebranch$cmp56373:
%ae49184 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54761$k484260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56374 = alloca %struct.ScmObj*, align 8
%argslist54761$k484261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b48057, %struct.ScmObj* %argslist54761$k484260)
store volatile %struct.ScmObj* %argslist54761$k484261, %struct.ScmObj** %stackaddr$prim56374, align 8
%stackaddr$prim56375 = alloca %struct.ScmObj*, align 8
%argslist54761$k484262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49184, %struct.ScmObj* %argslist54761$k484261)
store volatile %struct.ScmObj* %argslist54761$k484262, %struct.ScmObj** %stackaddr$prim56375, align 8
%clofunc56376 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48426)
musttail call tailcc void %clofunc56376(%struct.ScmObj* %k48426, %struct.ScmObj* %argslist54761$k484262)
ret void
falsebranch$cmp56373:
%stackaddr$prim56377 = alloca %struct.ScmObj*, align 8
%cpsprim48427 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48058)
store volatile %struct.ScmObj* %cpsprim48427, %struct.ScmObj** %stackaddr$prim56377, align 8
%ae49191 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54762$k484260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56378 = alloca %struct.ScmObj*, align 8
%argslist54762$k484261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48427, %struct.ScmObj* %argslist54762$k484260)
store volatile %struct.ScmObj* %argslist54762$k484261, %struct.ScmObj** %stackaddr$prim56378, align 8
%stackaddr$prim56379 = alloca %struct.ScmObj*, align 8
%argslist54762$k484262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49191, %struct.ScmObj* %argslist54762$k484261)
store volatile %struct.ScmObj* %argslist54762$k484262, %struct.ScmObj** %stackaddr$prim56379, align 8
%clofunc56380 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48426)
musttail call tailcc void %clofunc56380(%struct.ScmObj* %k48426, %struct.ScmObj* %argslist54762$k484262)
ret void
}

define tailcc void @proc_clo$ae49138(%struct.ScmObj* %env$ae49138,%struct.ScmObj* %current_45args54766) {
%stackaddr$env-ref56381 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49138, i64 0)
store %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$env-ref56381
%stackaddr$env-ref56382 = alloca %struct.ScmObj*, align 8
%_37length48035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49138, i64 1)
store %struct.ScmObj* %_37length48035, %struct.ScmObj** %stackaddr$env-ref56382
%stackaddr$prim56383 = alloca %struct.ScmObj*, align 8
%k48428 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54766)
store volatile %struct.ScmObj* %k48428, %struct.ScmObj** %stackaddr$prim56383, align 8
%stackaddr$prim56384 = alloca %struct.ScmObj*, align 8
%current_45args54767 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54766)
store volatile %struct.ScmObj* %current_45args54767, %struct.ScmObj** %stackaddr$prim56384, align 8
%stackaddr$prim56385 = alloca %struct.ScmObj*, align 8
%lst48067 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54767)
store volatile %struct.ScmObj* %lst48067, %struct.ScmObj** %stackaddr$prim56385, align 8
%stackaddr$prim56386 = alloca %struct.ScmObj*, align 8
%current_45args54768 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54767)
store volatile %struct.ScmObj* %current_45args54768, %struct.ScmObj** %stackaddr$prim56386, align 8
%stackaddr$prim56387 = alloca %struct.ScmObj*, align 8
%n48066 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54768)
store volatile %struct.ScmObj* %n48066, %struct.ScmObj** %stackaddr$prim56387, align 8
%stackaddr$makeclosure56388 = alloca %struct.ScmObj*, align 8
%fptrToInt56389 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49140 to i64
%ae49140 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56389)
store volatile %struct.ScmObj* %ae49140, %struct.ScmObj** %stackaddr$makeclosure56388, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49140, %struct.ScmObj* %_37take48038, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49140, %struct.ScmObj* %lst48067, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49140, %struct.ScmObj* %n48066, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49140, %struct.ScmObj* %k48428, i64 3)
%argslist54774$_37length480350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56390 = alloca %struct.ScmObj*, align 8
%argslist54774$_37length480351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48067, %struct.ScmObj* %argslist54774$_37length480350)
store volatile %struct.ScmObj* %argslist54774$_37length480351, %struct.ScmObj** %stackaddr$prim56390, align 8
%stackaddr$prim56391 = alloca %struct.ScmObj*, align 8
%argslist54774$_37length480352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49140, %struct.ScmObj* %argslist54774$_37length480351)
store volatile %struct.ScmObj* %argslist54774$_37length480352, %struct.ScmObj** %stackaddr$prim56391, align 8
%clofunc56392 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length48035)
musttail call tailcc void %clofunc56392(%struct.ScmObj* %_37length48035, %struct.ScmObj* %argslist54774$_37length480352)
ret void
}

define tailcc void @proc_clo$ae49140(%struct.ScmObj* %env$ae49140,%struct.ScmObj* %current_45args54770) {
%stackaddr$env-ref56393 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49140, i64 0)
store %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$env-ref56393
%stackaddr$env-ref56394 = alloca %struct.ScmObj*, align 8
%lst48067 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49140, i64 1)
store %struct.ScmObj* %lst48067, %struct.ScmObj** %stackaddr$env-ref56394
%stackaddr$env-ref56395 = alloca %struct.ScmObj*, align 8
%n48066 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49140, i64 2)
store %struct.ScmObj* %n48066, %struct.ScmObj** %stackaddr$env-ref56395
%stackaddr$env-ref56396 = alloca %struct.ScmObj*, align 8
%k48428 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49140, i64 3)
store %struct.ScmObj* %k48428, %struct.ScmObj** %stackaddr$env-ref56396
%stackaddr$prim56397 = alloca %struct.ScmObj*, align 8
%_95k48429 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54770)
store volatile %struct.ScmObj* %_95k48429, %struct.ScmObj** %stackaddr$prim56397, align 8
%stackaddr$prim56398 = alloca %struct.ScmObj*, align 8
%current_45args54771 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54770)
store volatile %struct.ScmObj* %current_45args54771, %struct.ScmObj** %stackaddr$prim56398, align 8
%stackaddr$prim56399 = alloca %struct.ScmObj*, align 8
%anf_45bind48184 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54771)
store volatile %struct.ScmObj* %anf_45bind48184, %struct.ScmObj** %stackaddr$prim56399, align 8
%stackaddr$prim56400 = alloca %struct.ScmObj*, align 8
%anf_45bind48185 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind48184, %struct.ScmObj* %n48066)
store volatile %struct.ScmObj* %anf_45bind48185, %struct.ScmObj** %stackaddr$prim56400, align 8
%argslist54773$_37take480380 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56401 = alloca %struct.ScmObj*, align 8
%argslist54773$_37take480381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48185, %struct.ScmObj* %argslist54773$_37take480380)
store volatile %struct.ScmObj* %argslist54773$_37take480381, %struct.ScmObj** %stackaddr$prim56401, align 8
%stackaddr$prim56402 = alloca %struct.ScmObj*, align 8
%argslist54773$_37take480382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48067, %struct.ScmObj* %argslist54773$_37take480381)
store volatile %struct.ScmObj* %argslist54773$_37take480382, %struct.ScmObj** %stackaddr$prim56402, align 8
%stackaddr$prim56403 = alloca %struct.ScmObj*, align 8
%argslist54773$_37take480383 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48428, %struct.ScmObj* %argslist54773$_37take480382)
store volatile %struct.ScmObj* %argslist54773$_37take480383, %struct.ScmObj** %stackaddr$prim56403, align 8
%clofunc56404 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take48038)
musttail call tailcc void %clofunc56404(%struct.ScmObj* %_37take48038, %struct.ScmObj* %argslist54773$_37take480383)
ret void
}

define tailcc void @proc_clo$ae49084(%struct.ScmObj* %env$ae49084,%struct.ScmObj* %current_45args54776) {
%stackaddr$env-ref56405 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49084, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref56405
%stackaddr$prim56406 = alloca %struct.ScmObj*, align 8
%k48430 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54776)
store volatile %struct.ScmObj* %k48430, %struct.ScmObj** %stackaddr$prim56406, align 8
%stackaddr$prim56407 = alloca %struct.ScmObj*, align 8
%current_45args54777 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54776)
store volatile %struct.ScmObj* %current_45args54777, %struct.ScmObj** %stackaddr$prim56407, align 8
%stackaddr$prim56408 = alloca %struct.ScmObj*, align 8
%lst48069 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54777)
store volatile %struct.ScmObj* %lst48069, %struct.ScmObj** %stackaddr$prim56408, align 8
%stackaddr$makeclosure56409 = alloca %struct.ScmObj*, align 8
%fptrToInt56410 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49085 to i64
%ae49085 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56410)
store volatile %struct.ScmObj* %ae49085, %struct.ScmObj** %stackaddr$makeclosure56409, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49085, %struct.ScmObj* %_37foldl148030, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49085, %struct.ScmObj* %k48430, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49085, %struct.ScmObj* %lst48069, i64 2)
%ae49086 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56411 = alloca %struct.ScmObj*, align 8
%fptrToInt56412 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49087 to i64
%ae49087 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56412)
store volatile %struct.ScmObj* %ae49087, %struct.ScmObj** %stackaddr$makeclosure56411, align 8
%argslist54788$ae490850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56413 = alloca %struct.ScmObj*, align 8
%argslist54788$ae490851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49087, %struct.ScmObj* %argslist54788$ae490850)
store volatile %struct.ScmObj* %argslist54788$ae490851, %struct.ScmObj** %stackaddr$prim56413, align 8
%stackaddr$prim56414 = alloca %struct.ScmObj*, align 8
%argslist54788$ae490852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49086, %struct.ScmObj* %argslist54788$ae490851)
store volatile %struct.ScmObj* %argslist54788$ae490852, %struct.ScmObj** %stackaddr$prim56414, align 8
%clofunc56415 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49085)
musttail call tailcc void %clofunc56415(%struct.ScmObj* %ae49085, %struct.ScmObj* %argslist54788$ae490852)
ret void
}

define tailcc void @proc_clo$ae49085(%struct.ScmObj* %env$ae49085,%struct.ScmObj* %current_45args54779) {
%stackaddr$env-ref56416 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49085, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref56416
%stackaddr$env-ref56417 = alloca %struct.ScmObj*, align 8
%k48430 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49085, i64 1)
store %struct.ScmObj* %k48430, %struct.ScmObj** %stackaddr$env-ref56417
%stackaddr$env-ref56418 = alloca %struct.ScmObj*, align 8
%lst48069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49085, i64 2)
store %struct.ScmObj* %lst48069, %struct.ScmObj** %stackaddr$env-ref56418
%stackaddr$prim56419 = alloca %struct.ScmObj*, align 8
%_95k48431 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54779)
store volatile %struct.ScmObj* %_95k48431, %struct.ScmObj** %stackaddr$prim56419, align 8
%stackaddr$prim56420 = alloca %struct.ScmObj*, align 8
%current_45args54780 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54779)
store volatile %struct.ScmObj* %current_45args54780, %struct.ScmObj** %stackaddr$prim56420, align 8
%stackaddr$prim56421 = alloca %struct.ScmObj*, align 8
%anf_45bind48183 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54780)
store volatile %struct.ScmObj* %anf_45bind48183, %struct.ScmObj** %stackaddr$prim56421, align 8
%ae49106 = call %struct.ScmObj* @const_init_null()
%argslist54782$_37foldl1480300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56422 = alloca %struct.ScmObj*, align 8
%argslist54782$_37foldl1480301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48069, %struct.ScmObj* %argslist54782$_37foldl1480300)
store volatile %struct.ScmObj* %argslist54782$_37foldl1480301, %struct.ScmObj** %stackaddr$prim56422, align 8
%stackaddr$prim56423 = alloca %struct.ScmObj*, align 8
%argslist54782$_37foldl1480302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49106, %struct.ScmObj* %argslist54782$_37foldl1480301)
store volatile %struct.ScmObj* %argslist54782$_37foldl1480302, %struct.ScmObj** %stackaddr$prim56423, align 8
%stackaddr$prim56424 = alloca %struct.ScmObj*, align 8
%argslist54782$_37foldl1480303 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48183, %struct.ScmObj* %argslist54782$_37foldl1480302)
store volatile %struct.ScmObj* %argslist54782$_37foldl1480303, %struct.ScmObj** %stackaddr$prim56424, align 8
%stackaddr$prim56425 = alloca %struct.ScmObj*, align 8
%argslist54782$_37foldl1480304 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48430, %struct.ScmObj* %argslist54782$_37foldl1480303)
store volatile %struct.ScmObj* %argslist54782$_37foldl1480304, %struct.ScmObj** %stackaddr$prim56425, align 8
%clofunc56426 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148030)
musttail call tailcc void %clofunc56426(%struct.ScmObj* %_37foldl148030, %struct.ScmObj* %argslist54782$_37foldl1480304)
ret void
}

define tailcc void @proc_clo$ae49087(%struct.ScmObj* %env$ae49087,%struct.ScmObj* %current_45args54783) {
%stackaddr$prim56427 = alloca %struct.ScmObj*, align 8
%k48432 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54783)
store volatile %struct.ScmObj* %k48432, %struct.ScmObj** %stackaddr$prim56427, align 8
%stackaddr$prim56428 = alloca %struct.ScmObj*, align 8
%current_45args54784 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54783)
store volatile %struct.ScmObj* %current_45args54784, %struct.ScmObj** %stackaddr$prim56428, align 8
%stackaddr$prim56429 = alloca %struct.ScmObj*, align 8
%x48071 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54784)
store volatile %struct.ScmObj* %x48071, %struct.ScmObj** %stackaddr$prim56429, align 8
%stackaddr$prim56430 = alloca %struct.ScmObj*, align 8
%current_45args54785 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54784)
store volatile %struct.ScmObj* %current_45args54785, %struct.ScmObj** %stackaddr$prim56430, align 8
%stackaddr$prim56431 = alloca %struct.ScmObj*, align 8
%y48070 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54785)
store volatile %struct.ScmObj* %y48070, %struct.ScmObj** %stackaddr$prim56431, align 8
%ae49089 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54787$k484320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56432 = alloca %struct.ScmObj*, align 8
%argslist54787$k484321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x48071, %struct.ScmObj* %argslist54787$k484320)
store volatile %struct.ScmObj* %argslist54787$k484321, %struct.ScmObj** %stackaddr$prim56432, align 8
%stackaddr$prim56433 = alloca %struct.ScmObj*, align 8
%argslist54787$k484322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49089, %struct.ScmObj* %argslist54787$k484321)
store volatile %struct.ScmObj* %argslist54787$k484322, %struct.ScmObj** %stackaddr$prim56433, align 8
%clofunc56434 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48432)
musttail call tailcc void %clofunc56434(%struct.ScmObj* %k48432, %struct.ScmObj* %argslist54787$k484322)
ret void
}

define tailcc void @proc_clo$ae49005(%struct.ScmObj* %env$ae49005,%struct.ScmObj* %current_45args54791) {
%stackaddr$prim56435 = alloca %struct.ScmObj*, align 8
%k48433 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54791)
store volatile %struct.ScmObj* %k48433, %struct.ScmObj** %stackaddr$prim56435, align 8
%stackaddr$prim56436 = alloca %struct.ScmObj*, align 8
%current_45args54792 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54791)
store volatile %struct.ScmObj* %current_45args54792, %struct.ScmObj** %stackaddr$prim56436, align 8
%stackaddr$prim56437 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54792)
store volatile %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$prim56437, align 8
%ae49007 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56438 = alloca %struct.ScmObj*, align 8
%fptrToInt56439 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49008 to i64
%ae49008 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56439)
store volatile %struct.ScmObj* %ae49008, %struct.ScmObj** %stackaddr$makeclosure56438, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49008, %struct.ScmObj* %_37foldl148031, i64 0)
%argslist54805$k484330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56440 = alloca %struct.ScmObj*, align 8
%argslist54805$k484331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49008, %struct.ScmObj* %argslist54805$k484330)
store volatile %struct.ScmObj* %argslist54805$k484331, %struct.ScmObj** %stackaddr$prim56440, align 8
%stackaddr$prim56441 = alloca %struct.ScmObj*, align 8
%argslist54805$k484332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49007, %struct.ScmObj* %argslist54805$k484331)
store volatile %struct.ScmObj* %argslist54805$k484332, %struct.ScmObj** %stackaddr$prim56441, align 8
%clofunc56442 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48433)
musttail call tailcc void %clofunc56442(%struct.ScmObj* %k48433, %struct.ScmObj* %argslist54805$k484332)
ret void
}

define tailcc void @proc_clo$ae49008(%struct.ScmObj* %env$ae49008,%struct.ScmObj* %current_45args54794) {
%stackaddr$env-ref56443 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49008, i64 0)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref56443
%stackaddr$prim56444 = alloca %struct.ScmObj*, align 8
%k48434 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54794)
store volatile %struct.ScmObj* %k48434, %struct.ScmObj** %stackaddr$prim56444, align 8
%stackaddr$prim56445 = alloca %struct.ScmObj*, align 8
%current_45args54795 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54794)
store volatile %struct.ScmObj* %current_45args54795, %struct.ScmObj** %stackaddr$prim56445, align 8
%stackaddr$prim56446 = alloca %struct.ScmObj*, align 8
%f48034 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54795)
store volatile %struct.ScmObj* %f48034, %struct.ScmObj** %stackaddr$prim56446, align 8
%stackaddr$prim56447 = alloca %struct.ScmObj*, align 8
%current_45args54796 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54795)
store volatile %struct.ScmObj* %current_45args54796, %struct.ScmObj** %stackaddr$prim56447, align 8
%stackaddr$prim56448 = alloca %struct.ScmObj*, align 8
%acc48033 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54796)
store volatile %struct.ScmObj* %acc48033, %struct.ScmObj** %stackaddr$prim56448, align 8
%stackaddr$prim56449 = alloca %struct.ScmObj*, align 8
%current_45args54797 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54796)
store volatile %struct.ScmObj* %current_45args54797, %struct.ScmObj** %stackaddr$prim56449, align 8
%stackaddr$prim56450 = alloca %struct.ScmObj*, align 8
%lst48032 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54797)
store volatile %struct.ScmObj* %lst48032, %struct.ScmObj** %stackaddr$prim56450, align 8
%stackaddr$prim56451 = alloca %struct.ScmObj*, align 8
%anf_45bind48178 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48032)
store volatile %struct.ScmObj* %anf_45bind48178, %struct.ScmObj** %stackaddr$prim56451, align 8
%truthy$cmp56452 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48178)
%cmp$cmp56452 = icmp eq i64 %truthy$cmp56452, 1
br i1 %cmp$cmp56452, label %truebranch$cmp56452, label %falsebranch$cmp56452
truebranch$cmp56452:
%ae49012 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54799$k484340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56453 = alloca %struct.ScmObj*, align 8
%argslist54799$k484341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48033, %struct.ScmObj* %argslist54799$k484340)
store volatile %struct.ScmObj* %argslist54799$k484341, %struct.ScmObj** %stackaddr$prim56453, align 8
%stackaddr$prim56454 = alloca %struct.ScmObj*, align 8
%argslist54799$k484342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49012, %struct.ScmObj* %argslist54799$k484341)
store volatile %struct.ScmObj* %argslist54799$k484342, %struct.ScmObj** %stackaddr$prim56454, align 8
%clofunc56455 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48434)
musttail call tailcc void %clofunc56455(%struct.ScmObj* %k48434, %struct.ScmObj* %argslist54799$k484342)
ret void
falsebranch$cmp56452:
%stackaddr$prim56456 = alloca %struct.ScmObj*, align 8
%anf_45bind48179 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48032)
store volatile %struct.ScmObj* %anf_45bind48179, %struct.ScmObj** %stackaddr$prim56456, align 8
%stackaddr$makeclosure56457 = alloca %struct.ScmObj*, align 8
%fptrToInt56458 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49019 to i64
%ae49019 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56458)
store volatile %struct.ScmObj* %ae49019, %struct.ScmObj** %stackaddr$makeclosure56457, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49019, %struct.ScmObj* %f48034, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49019, %struct.ScmObj* %k48434, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49019, %struct.ScmObj* %lst48032, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49019, %struct.ScmObj* %_37foldl148031, i64 3)
%argslist54804$f480340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56459 = alloca %struct.ScmObj*, align 8
%argslist54804$f480341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48033, %struct.ScmObj* %argslist54804$f480340)
store volatile %struct.ScmObj* %argslist54804$f480341, %struct.ScmObj** %stackaddr$prim56459, align 8
%stackaddr$prim56460 = alloca %struct.ScmObj*, align 8
%argslist54804$f480342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48179, %struct.ScmObj* %argslist54804$f480341)
store volatile %struct.ScmObj* %argslist54804$f480342, %struct.ScmObj** %stackaddr$prim56460, align 8
%stackaddr$prim56461 = alloca %struct.ScmObj*, align 8
%argslist54804$f480343 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49019, %struct.ScmObj* %argslist54804$f480342)
store volatile %struct.ScmObj* %argslist54804$f480343, %struct.ScmObj** %stackaddr$prim56461, align 8
%clofunc56462 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48034)
musttail call tailcc void %clofunc56462(%struct.ScmObj* %f48034, %struct.ScmObj* %argslist54804$f480343)
ret void
}

define tailcc void @proc_clo$ae49019(%struct.ScmObj* %env$ae49019,%struct.ScmObj* %current_45args54800) {
%stackaddr$env-ref56463 = alloca %struct.ScmObj*, align 8
%f48034 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49019, i64 0)
store %struct.ScmObj* %f48034, %struct.ScmObj** %stackaddr$env-ref56463
%stackaddr$env-ref56464 = alloca %struct.ScmObj*, align 8
%k48434 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49019, i64 1)
store %struct.ScmObj* %k48434, %struct.ScmObj** %stackaddr$env-ref56464
%stackaddr$env-ref56465 = alloca %struct.ScmObj*, align 8
%lst48032 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49019, i64 2)
store %struct.ScmObj* %lst48032, %struct.ScmObj** %stackaddr$env-ref56465
%stackaddr$env-ref56466 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49019, i64 3)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref56466
%stackaddr$prim56467 = alloca %struct.ScmObj*, align 8
%_95k48435 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54800)
store volatile %struct.ScmObj* %_95k48435, %struct.ScmObj** %stackaddr$prim56467, align 8
%stackaddr$prim56468 = alloca %struct.ScmObj*, align 8
%current_45args54801 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54800)
store volatile %struct.ScmObj* %current_45args54801, %struct.ScmObj** %stackaddr$prim56468, align 8
%stackaddr$prim56469 = alloca %struct.ScmObj*, align 8
%anf_45bind48180 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54801)
store volatile %struct.ScmObj* %anf_45bind48180, %struct.ScmObj** %stackaddr$prim56469, align 8
%stackaddr$prim56470 = alloca %struct.ScmObj*, align 8
%anf_45bind48181 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48032)
store volatile %struct.ScmObj* %anf_45bind48181, %struct.ScmObj** %stackaddr$prim56470, align 8
%argslist54803$_37foldl1480310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56471 = alloca %struct.ScmObj*, align 8
%argslist54803$_37foldl1480311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48181, %struct.ScmObj* %argslist54803$_37foldl1480310)
store volatile %struct.ScmObj* %argslist54803$_37foldl1480311, %struct.ScmObj** %stackaddr$prim56471, align 8
%stackaddr$prim56472 = alloca %struct.ScmObj*, align 8
%argslist54803$_37foldl1480312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48180, %struct.ScmObj* %argslist54803$_37foldl1480311)
store volatile %struct.ScmObj* %argslist54803$_37foldl1480312, %struct.ScmObj** %stackaddr$prim56472, align 8
%stackaddr$prim56473 = alloca %struct.ScmObj*, align 8
%argslist54803$_37foldl1480313 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48034, %struct.ScmObj* %argslist54803$_37foldl1480312)
store volatile %struct.ScmObj* %argslist54803$_37foldl1480313, %struct.ScmObj** %stackaddr$prim56473, align 8
%stackaddr$prim56474 = alloca %struct.ScmObj*, align 8
%argslist54803$_37foldl1480314 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48434, %struct.ScmObj* %argslist54803$_37foldl1480313)
store volatile %struct.ScmObj* %argslist54803$_37foldl1480314, %struct.ScmObj** %stackaddr$prim56474, align 8
%clofunc56475 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148031)
musttail call tailcc void %clofunc56475(%struct.ScmObj* %_37foldl148031, %struct.ScmObj* %argslist54803$_37foldl1480314)
ret void
}

define tailcc void @proc_clo$ae48922(%struct.ScmObj* %env$ae48922,%struct.ScmObj* %current_45args54808) {
%stackaddr$prim56476 = alloca %struct.ScmObj*, align 8
%k48436 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54808)
store volatile %struct.ScmObj* %k48436, %struct.ScmObj** %stackaddr$prim56476, align 8
%stackaddr$prim56477 = alloca %struct.ScmObj*, align 8
%current_45args54809 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54808)
store volatile %struct.ScmObj* %current_45args54809, %struct.ScmObj** %stackaddr$prim56477, align 8
%stackaddr$prim56478 = alloca %struct.ScmObj*, align 8
%_37length48036 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54809)
store volatile %struct.ScmObj* %_37length48036, %struct.ScmObj** %stackaddr$prim56478, align 8
%ae48924 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56479 = alloca %struct.ScmObj*, align 8
%fptrToInt56480 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48925 to i64
%ae48925 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56480)
store volatile %struct.ScmObj* %ae48925, %struct.ScmObj** %stackaddr$makeclosure56479, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48925, %struct.ScmObj* %_37length48036, i64 0)
%argslist54820$k484360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56481 = alloca %struct.ScmObj*, align 8
%argslist54820$k484361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48925, %struct.ScmObj* %argslist54820$k484360)
store volatile %struct.ScmObj* %argslist54820$k484361, %struct.ScmObj** %stackaddr$prim56481, align 8
%stackaddr$prim56482 = alloca %struct.ScmObj*, align 8
%argslist54820$k484362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48924, %struct.ScmObj* %argslist54820$k484361)
store volatile %struct.ScmObj* %argslist54820$k484362, %struct.ScmObj** %stackaddr$prim56482, align 8
%clofunc56483 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48436)
musttail call tailcc void %clofunc56483(%struct.ScmObj* %k48436, %struct.ScmObj* %argslist54820$k484362)
ret void
}

define tailcc void @proc_clo$ae48925(%struct.ScmObj* %env$ae48925,%struct.ScmObj* %current_45args54811) {
%stackaddr$env-ref56484 = alloca %struct.ScmObj*, align 8
%_37length48036 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48925, i64 0)
store %struct.ScmObj* %_37length48036, %struct.ScmObj** %stackaddr$env-ref56484
%stackaddr$prim56485 = alloca %struct.ScmObj*, align 8
%k48437 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54811)
store volatile %struct.ScmObj* %k48437, %struct.ScmObj** %stackaddr$prim56485, align 8
%stackaddr$prim56486 = alloca %struct.ScmObj*, align 8
%current_45args54812 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54811)
store volatile %struct.ScmObj* %current_45args54812, %struct.ScmObj** %stackaddr$prim56486, align 8
%stackaddr$prim56487 = alloca %struct.ScmObj*, align 8
%lst48037 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54812)
store volatile %struct.ScmObj* %lst48037, %struct.ScmObj** %stackaddr$prim56487, align 8
%stackaddr$prim56488 = alloca %struct.ScmObj*, align 8
%anf_45bind48174 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48037)
store volatile %struct.ScmObj* %anf_45bind48174, %struct.ScmObj** %stackaddr$prim56488, align 8
%truthy$cmp56489 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48174)
%cmp$cmp56489 = icmp eq i64 %truthy$cmp56489, 1
br i1 %cmp$cmp56489, label %truebranch$cmp56489, label %falsebranch$cmp56489
truebranch$cmp56489:
%ae48929 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48930 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54814$k484370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56490 = alloca %struct.ScmObj*, align 8
%argslist54814$k484371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48930, %struct.ScmObj* %argslist54814$k484370)
store volatile %struct.ScmObj* %argslist54814$k484371, %struct.ScmObj** %stackaddr$prim56490, align 8
%stackaddr$prim56491 = alloca %struct.ScmObj*, align 8
%argslist54814$k484372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48929, %struct.ScmObj* %argslist54814$k484371)
store volatile %struct.ScmObj* %argslist54814$k484372, %struct.ScmObj** %stackaddr$prim56491, align 8
%clofunc56492 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48437)
musttail call tailcc void %clofunc56492(%struct.ScmObj* %k48437, %struct.ScmObj* %argslist54814$k484372)
ret void
falsebranch$cmp56489:
%stackaddr$prim56493 = alloca %struct.ScmObj*, align 8
%anf_45bind48175 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48037)
store volatile %struct.ScmObj* %anf_45bind48175, %struct.ScmObj** %stackaddr$prim56493, align 8
%stackaddr$makeclosure56494 = alloca %struct.ScmObj*, align 8
%fptrToInt56495 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48939 to i64
%ae48939 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56495)
store volatile %struct.ScmObj* %ae48939, %struct.ScmObj** %stackaddr$makeclosure56494, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48939, %struct.ScmObj* %k48437, i64 0)
%argslist54819$_37length480360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56496 = alloca %struct.ScmObj*, align 8
%argslist54819$_37length480361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48175, %struct.ScmObj* %argslist54819$_37length480360)
store volatile %struct.ScmObj* %argslist54819$_37length480361, %struct.ScmObj** %stackaddr$prim56496, align 8
%stackaddr$prim56497 = alloca %struct.ScmObj*, align 8
%argslist54819$_37length480362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48939, %struct.ScmObj* %argslist54819$_37length480361)
store volatile %struct.ScmObj* %argslist54819$_37length480362, %struct.ScmObj** %stackaddr$prim56497, align 8
%clofunc56498 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length48036)
musttail call tailcc void %clofunc56498(%struct.ScmObj* %_37length48036, %struct.ScmObj* %argslist54819$_37length480362)
ret void
}

define tailcc void @proc_clo$ae48939(%struct.ScmObj* %env$ae48939,%struct.ScmObj* %current_45args54815) {
%stackaddr$env-ref56499 = alloca %struct.ScmObj*, align 8
%k48437 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48939, i64 0)
store %struct.ScmObj* %k48437, %struct.ScmObj** %stackaddr$env-ref56499
%stackaddr$prim56500 = alloca %struct.ScmObj*, align 8
%_95k48438 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54815)
store volatile %struct.ScmObj* %_95k48438, %struct.ScmObj** %stackaddr$prim56500, align 8
%stackaddr$prim56501 = alloca %struct.ScmObj*, align 8
%current_45args54816 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54815)
store volatile %struct.ScmObj* %current_45args54816, %struct.ScmObj** %stackaddr$prim56501, align 8
%stackaddr$prim56502 = alloca %struct.ScmObj*, align 8
%anf_45bind48176 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54816)
store volatile %struct.ScmObj* %anf_45bind48176, %struct.ScmObj** %stackaddr$prim56502, align 8
%ae48941 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56503 = alloca %struct.ScmObj*, align 8
%cpsprim48439 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae48941, %struct.ScmObj* %anf_45bind48176)
store volatile %struct.ScmObj* %cpsprim48439, %struct.ScmObj** %stackaddr$prim56503, align 8
%ae48944 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54818$k484370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56504 = alloca %struct.ScmObj*, align 8
%argslist54818$k484371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48439, %struct.ScmObj* %argslist54818$k484370)
store volatile %struct.ScmObj* %argslist54818$k484371, %struct.ScmObj** %stackaddr$prim56504, align 8
%stackaddr$prim56505 = alloca %struct.ScmObj*, align 8
%argslist54818$k484372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48944, %struct.ScmObj* %argslist54818$k484371)
store volatile %struct.ScmObj* %argslist54818$k484372, %struct.ScmObj** %stackaddr$prim56505, align 8
%clofunc56506 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48437)
musttail call tailcc void %clofunc56506(%struct.ScmObj* %k48437, %struct.ScmObj* %argslist54818$k484372)
ret void
}

define tailcc void @proc_clo$ae48772(%struct.ScmObj* %env$ae48772,%struct.ScmObj* %current_45args54823) {
%stackaddr$prim56507 = alloca %struct.ScmObj*, align 8
%k48440 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54823)
store volatile %struct.ScmObj* %k48440, %struct.ScmObj** %stackaddr$prim56507, align 8
%stackaddr$prim56508 = alloca %struct.ScmObj*, align 8
%current_45args54824 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54823)
store volatile %struct.ScmObj* %current_45args54824, %struct.ScmObj** %stackaddr$prim56508, align 8
%stackaddr$prim56509 = alloca %struct.ScmObj*, align 8
%_37take48039 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54824)
store volatile %struct.ScmObj* %_37take48039, %struct.ScmObj** %stackaddr$prim56509, align 8
%ae48774 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56510 = alloca %struct.ScmObj*, align 8
%fptrToInt56511 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48775 to i64
%ae48775 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56511)
store volatile %struct.ScmObj* %ae48775, %struct.ScmObj** %stackaddr$makeclosure56510, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48775, %struct.ScmObj* %_37take48039, i64 0)
%argslist54837$k484400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56512 = alloca %struct.ScmObj*, align 8
%argslist54837$k484401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48775, %struct.ScmObj* %argslist54837$k484400)
store volatile %struct.ScmObj* %argslist54837$k484401, %struct.ScmObj** %stackaddr$prim56512, align 8
%stackaddr$prim56513 = alloca %struct.ScmObj*, align 8
%argslist54837$k484402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48774, %struct.ScmObj* %argslist54837$k484401)
store volatile %struct.ScmObj* %argslist54837$k484402, %struct.ScmObj** %stackaddr$prim56513, align 8
%clofunc56514 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48440)
musttail call tailcc void %clofunc56514(%struct.ScmObj* %k48440, %struct.ScmObj* %argslist54837$k484402)
ret void
}

define tailcc void @proc_clo$ae48775(%struct.ScmObj* %env$ae48775,%struct.ScmObj* %current_45args54826) {
%stackaddr$env-ref56515 = alloca %struct.ScmObj*, align 8
%_37take48039 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48775, i64 0)
store %struct.ScmObj* %_37take48039, %struct.ScmObj** %stackaddr$env-ref56515
%stackaddr$prim56516 = alloca %struct.ScmObj*, align 8
%k48441 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54826)
store volatile %struct.ScmObj* %k48441, %struct.ScmObj** %stackaddr$prim56516, align 8
%stackaddr$prim56517 = alloca %struct.ScmObj*, align 8
%current_45args54827 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54826)
store volatile %struct.ScmObj* %current_45args54827, %struct.ScmObj** %stackaddr$prim56517, align 8
%stackaddr$prim56518 = alloca %struct.ScmObj*, align 8
%lst48041 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54827)
store volatile %struct.ScmObj* %lst48041, %struct.ScmObj** %stackaddr$prim56518, align 8
%stackaddr$prim56519 = alloca %struct.ScmObj*, align 8
%current_45args54828 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54827)
store volatile %struct.ScmObj* %current_45args54828, %struct.ScmObj** %stackaddr$prim56519, align 8
%stackaddr$prim56520 = alloca %struct.ScmObj*, align 8
%n48040 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54828)
store volatile %struct.ScmObj* %n48040, %struct.ScmObj** %stackaddr$prim56520, align 8
%ae48777 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56521 = alloca %struct.ScmObj*, align 8
%anf_45bind48167 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n48040, %struct.ScmObj* %ae48777)
store volatile %struct.ScmObj* %anf_45bind48167, %struct.ScmObj** %stackaddr$prim56521, align 8
%truthy$cmp56522 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48167)
%cmp$cmp56522 = icmp eq i64 %truthy$cmp56522, 1
br i1 %cmp$cmp56522, label %truebranch$cmp56522, label %falsebranch$cmp56522
truebranch$cmp56522:
%ae48780 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48781 = call %struct.ScmObj* @const_init_null()
%argslist54830$k484410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56523 = alloca %struct.ScmObj*, align 8
%argslist54830$k484411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48781, %struct.ScmObj* %argslist54830$k484410)
store volatile %struct.ScmObj* %argslist54830$k484411, %struct.ScmObj** %stackaddr$prim56523, align 8
%stackaddr$prim56524 = alloca %struct.ScmObj*, align 8
%argslist54830$k484412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48780, %struct.ScmObj* %argslist54830$k484411)
store volatile %struct.ScmObj* %argslist54830$k484412, %struct.ScmObj** %stackaddr$prim56524, align 8
%clofunc56525 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48441)
musttail call tailcc void %clofunc56525(%struct.ScmObj* %k48441, %struct.ScmObj* %argslist54830$k484412)
ret void
falsebranch$cmp56522:
%stackaddr$prim56526 = alloca %struct.ScmObj*, align 8
%anf_45bind48168 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48041)
store volatile %struct.ScmObj* %anf_45bind48168, %struct.ScmObj** %stackaddr$prim56526, align 8
%truthy$cmp56527 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48168)
%cmp$cmp56527 = icmp eq i64 %truthy$cmp56527, 1
br i1 %cmp$cmp56527, label %truebranch$cmp56527, label %falsebranch$cmp56527
truebranch$cmp56527:
%ae48791 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48792 = call %struct.ScmObj* @const_init_null()
%argslist54831$k484410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56528 = alloca %struct.ScmObj*, align 8
%argslist54831$k484411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48792, %struct.ScmObj* %argslist54831$k484410)
store volatile %struct.ScmObj* %argslist54831$k484411, %struct.ScmObj** %stackaddr$prim56528, align 8
%stackaddr$prim56529 = alloca %struct.ScmObj*, align 8
%argslist54831$k484412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48791, %struct.ScmObj* %argslist54831$k484411)
store volatile %struct.ScmObj* %argslist54831$k484412, %struct.ScmObj** %stackaddr$prim56529, align 8
%clofunc56530 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48441)
musttail call tailcc void %clofunc56530(%struct.ScmObj* %k48441, %struct.ScmObj* %argslist54831$k484412)
ret void
falsebranch$cmp56527:
%stackaddr$prim56531 = alloca %struct.ScmObj*, align 8
%anf_45bind48169 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48041)
store volatile %struct.ScmObj* %anf_45bind48169, %struct.ScmObj** %stackaddr$prim56531, align 8
%stackaddr$prim56532 = alloca %struct.ScmObj*, align 8
%anf_45bind48170 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48041)
store volatile %struct.ScmObj* %anf_45bind48170, %struct.ScmObj** %stackaddr$prim56532, align 8
%ae48802 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56533 = alloca %struct.ScmObj*, align 8
%anf_45bind48171 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n48040, %struct.ScmObj* %ae48802)
store volatile %struct.ScmObj* %anf_45bind48171, %struct.ScmObj** %stackaddr$prim56533, align 8
%stackaddr$makeclosure56534 = alloca %struct.ScmObj*, align 8
%fptrToInt56535 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48804 to i64
%ae48804 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56535)
store volatile %struct.ScmObj* %ae48804, %struct.ScmObj** %stackaddr$makeclosure56534, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48804, %struct.ScmObj* %k48441, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48804, %struct.ScmObj* %anf_45bind48169, i64 1)
%argslist54836$_37take480390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56536 = alloca %struct.ScmObj*, align 8
%argslist54836$_37take480391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48171, %struct.ScmObj* %argslist54836$_37take480390)
store volatile %struct.ScmObj* %argslist54836$_37take480391, %struct.ScmObj** %stackaddr$prim56536, align 8
%stackaddr$prim56537 = alloca %struct.ScmObj*, align 8
%argslist54836$_37take480392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48170, %struct.ScmObj* %argslist54836$_37take480391)
store volatile %struct.ScmObj* %argslist54836$_37take480392, %struct.ScmObj** %stackaddr$prim56537, align 8
%stackaddr$prim56538 = alloca %struct.ScmObj*, align 8
%argslist54836$_37take480393 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48804, %struct.ScmObj* %argslist54836$_37take480392)
store volatile %struct.ScmObj* %argslist54836$_37take480393, %struct.ScmObj** %stackaddr$prim56538, align 8
%clofunc56539 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take48039)
musttail call tailcc void %clofunc56539(%struct.ScmObj* %_37take48039, %struct.ScmObj* %argslist54836$_37take480393)
ret void
}

define tailcc void @proc_clo$ae48804(%struct.ScmObj* %env$ae48804,%struct.ScmObj* %current_45args54832) {
%stackaddr$env-ref56540 = alloca %struct.ScmObj*, align 8
%k48441 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48804, i64 0)
store %struct.ScmObj* %k48441, %struct.ScmObj** %stackaddr$env-ref56540
%stackaddr$env-ref56541 = alloca %struct.ScmObj*, align 8
%anf_45bind48169 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48804, i64 1)
store %struct.ScmObj* %anf_45bind48169, %struct.ScmObj** %stackaddr$env-ref56541
%stackaddr$prim56542 = alloca %struct.ScmObj*, align 8
%_95k48442 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54832)
store volatile %struct.ScmObj* %_95k48442, %struct.ScmObj** %stackaddr$prim56542, align 8
%stackaddr$prim56543 = alloca %struct.ScmObj*, align 8
%current_45args54833 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54832)
store volatile %struct.ScmObj* %current_45args54833, %struct.ScmObj** %stackaddr$prim56543, align 8
%stackaddr$prim56544 = alloca %struct.ScmObj*, align 8
%anf_45bind48172 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54833)
store volatile %struct.ScmObj* %anf_45bind48172, %struct.ScmObj** %stackaddr$prim56544, align 8
%stackaddr$prim56545 = alloca %struct.ScmObj*, align 8
%cpsprim48443 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48169, %struct.ScmObj* %anf_45bind48172)
store volatile %struct.ScmObj* %cpsprim48443, %struct.ScmObj** %stackaddr$prim56545, align 8
%ae48810 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54835$k484410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56546 = alloca %struct.ScmObj*, align 8
%argslist54835$k484411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48443, %struct.ScmObj* %argslist54835$k484410)
store volatile %struct.ScmObj* %argslist54835$k484411, %struct.ScmObj** %stackaddr$prim56546, align 8
%stackaddr$prim56547 = alloca %struct.ScmObj*, align 8
%argslist54835$k484412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48810, %struct.ScmObj* %argslist54835$k484411)
store volatile %struct.ScmObj* %argslist54835$k484412, %struct.ScmObj** %stackaddr$prim56547, align 8
%clofunc56548 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48441)
musttail call tailcc void %clofunc56548(%struct.ScmObj* %k48441, %struct.ScmObj* %argslist54835$k484412)
ret void
}

define tailcc void @proc_clo$ae48675(%struct.ScmObj* %env$ae48675,%struct.ScmObj* %current_45args54840) {
%stackaddr$prim56549 = alloca %struct.ScmObj*, align 8
%k48444 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54840)
store volatile %struct.ScmObj* %k48444, %struct.ScmObj** %stackaddr$prim56549, align 8
%stackaddr$prim56550 = alloca %struct.ScmObj*, align 8
%current_45args54841 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54840)
store volatile %struct.ScmObj* %current_45args54841, %struct.ScmObj** %stackaddr$prim56550, align 8
%stackaddr$prim56551 = alloca %struct.ScmObj*, align 8
%_37map48043 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54841)
store volatile %struct.ScmObj* %_37map48043, %struct.ScmObj** %stackaddr$prim56551, align 8
%ae48677 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56552 = alloca %struct.ScmObj*, align 8
%fptrToInt56553 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48678 to i64
%ae48678 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56553)
store volatile %struct.ScmObj* %ae48678, %struct.ScmObj** %stackaddr$makeclosure56552, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48678, %struct.ScmObj* %_37map48043, i64 0)
%argslist54857$k484440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56554 = alloca %struct.ScmObj*, align 8
%argslist54857$k484441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48678, %struct.ScmObj* %argslist54857$k484440)
store volatile %struct.ScmObj* %argslist54857$k484441, %struct.ScmObj** %stackaddr$prim56554, align 8
%stackaddr$prim56555 = alloca %struct.ScmObj*, align 8
%argslist54857$k484442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48677, %struct.ScmObj* %argslist54857$k484441)
store volatile %struct.ScmObj* %argslist54857$k484442, %struct.ScmObj** %stackaddr$prim56555, align 8
%clofunc56556 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48444)
musttail call tailcc void %clofunc56556(%struct.ScmObj* %k48444, %struct.ScmObj* %argslist54857$k484442)
ret void
}

define tailcc void @proc_clo$ae48678(%struct.ScmObj* %env$ae48678,%struct.ScmObj* %current_45args54843) {
%stackaddr$env-ref56557 = alloca %struct.ScmObj*, align 8
%_37map48043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48678, i64 0)
store %struct.ScmObj* %_37map48043, %struct.ScmObj** %stackaddr$env-ref56557
%stackaddr$prim56558 = alloca %struct.ScmObj*, align 8
%k48445 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54843)
store volatile %struct.ScmObj* %k48445, %struct.ScmObj** %stackaddr$prim56558, align 8
%stackaddr$prim56559 = alloca %struct.ScmObj*, align 8
%current_45args54844 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54843)
store volatile %struct.ScmObj* %current_45args54844, %struct.ScmObj** %stackaddr$prim56559, align 8
%stackaddr$prim56560 = alloca %struct.ScmObj*, align 8
%f48045 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54844)
store volatile %struct.ScmObj* %f48045, %struct.ScmObj** %stackaddr$prim56560, align 8
%stackaddr$prim56561 = alloca %struct.ScmObj*, align 8
%current_45args54845 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54844)
store volatile %struct.ScmObj* %current_45args54845, %struct.ScmObj** %stackaddr$prim56561, align 8
%stackaddr$prim56562 = alloca %struct.ScmObj*, align 8
%lst48044 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54845)
store volatile %struct.ScmObj* %lst48044, %struct.ScmObj** %stackaddr$prim56562, align 8
%stackaddr$prim56563 = alloca %struct.ScmObj*, align 8
%anf_45bind48161 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48044)
store volatile %struct.ScmObj* %anf_45bind48161, %struct.ScmObj** %stackaddr$prim56563, align 8
%truthy$cmp56564 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48161)
%cmp$cmp56564 = icmp eq i64 %truthy$cmp56564, 1
br i1 %cmp$cmp56564, label %truebranch$cmp56564, label %falsebranch$cmp56564
truebranch$cmp56564:
%ae48682 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48683 = call %struct.ScmObj* @const_init_null()
%argslist54847$k484450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56565 = alloca %struct.ScmObj*, align 8
%argslist54847$k484451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48683, %struct.ScmObj* %argslist54847$k484450)
store volatile %struct.ScmObj* %argslist54847$k484451, %struct.ScmObj** %stackaddr$prim56565, align 8
%stackaddr$prim56566 = alloca %struct.ScmObj*, align 8
%argslist54847$k484452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48682, %struct.ScmObj* %argslist54847$k484451)
store volatile %struct.ScmObj* %argslist54847$k484452, %struct.ScmObj** %stackaddr$prim56566, align 8
%clofunc56567 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48445)
musttail call tailcc void %clofunc56567(%struct.ScmObj* %k48445, %struct.ScmObj* %argslist54847$k484452)
ret void
falsebranch$cmp56564:
%stackaddr$prim56568 = alloca %struct.ScmObj*, align 8
%anf_45bind48162 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48044)
store volatile %struct.ScmObj* %anf_45bind48162, %struct.ScmObj** %stackaddr$prim56568, align 8
%stackaddr$makeclosure56569 = alloca %struct.ScmObj*, align 8
%fptrToInt56570 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48692 to i64
%ae48692 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56570)
store volatile %struct.ScmObj* %ae48692, %struct.ScmObj** %stackaddr$makeclosure56569, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48692, %struct.ScmObj* %f48045, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48692, %struct.ScmObj* %k48445, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48692, %struct.ScmObj* %lst48044, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48692, %struct.ScmObj* %_37map48043, i64 3)
%argslist54856$f480450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56571 = alloca %struct.ScmObj*, align 8
%argslist54856$f480451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48162, %struct.ScmObj* %argslist54856$f480450)
store volatile %struct.ScmObj* %argslist54856$f480451, %struct.ScmObj** %stackaddr$prim56571, align 8
%stackaddr$prim56572 = alloca %struct.ScmObj*, align 8
%argslist54856$f480452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48692, %struct.ScmObj* %argslist54856$f480451)
store volatile %struct.ScmObj* %argslist54856$f480452, %struct.ScmObj** %stackaddr$prim56572, align 8
%clofunc56573 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48045)
musttail call tailcc void %clofunc56573(%struct.ScmObj* %f48045, %struct.ScmObj* %argslist54856$f480452)
ret void
}

define tailcc void @proc_clo$ae48692(%struct.ScmObj* %env$ae48692,%struct.ScmObj* %current_45args54848) {
%stackaddr$env-ref56574 = alloca %struct.ScmObj*, align 8
%f48045 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48692, i64 0)
store %struct.ScmObj* %f48045, %struct.ScmObj** %stackaddr$env-ref56574
%stackaddr$env-ref56575 = alloca %struct.ScmObj*, align 8
%k48445 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48692, i64 1)
store %struct.ScmObj* %k48445, %struct.ScmObj** %stackaddr$env-ref56575
%stackaddr$env-ref56576 = alloca %struct.ScmObj*, align 8
%lst48044 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48692, i64 2)
store %struct.ScmObj* %lst48044, %struct.ScmObj** %stackaddr$env-ref56576
%stackaddr$env-ref56577 = alloca %struct.ScmObj*, align 8
%_37map48043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48692, i64 3)
store %struct.ScmObj* %_37map48043, %struct.ScmObj** %stackaddr$env-ref56577
%stackaddr$prim56578 = alloca %struct.ScmObj*, align 8
%_95k48446 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54848)
store volatile %struct.ScmObj* %_95k48446, %struct.ScmObj** %stackaddr$prim56578, align 8
%stackaddr$prim56579 = alloca %struct.ScmObj*, align 8
%current_45args54849 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54848)
store volatile %struct.ScmObj* %current_45args54849, %struct.ScmObj** %stackaddr$prim56579, align 8
%stackaddr$prim56580 = alloca %struct.ScmObj*, align 8
%anf_45bind48163 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54849)
store volatile %struct.ScmObj* %anf_45bind48163, %struct.ScmObj** %stackaddr$prim56580, align 8
%stackaddr$prim56581 = alloca %struct.ScmObj*, align 8
%anf_45bind48164 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48044)
store volatile %struct.ScmObj* %anf_45bind48164, %struct.ScmObj** %stackaddr$prim56581, align 8
%stackaddr$makeclosure56582 = alloca %struct.ScmObj*, align 8
%fptrToInt56583 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48696 to i64
%ae48696 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56583)
store volatile %struct.ScmObj* %ae48696, %struct.ScmObj** %stackaddr$makeclosure56582, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48696, %struct.ScmObj* %anf_45bind48163, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48696, %struct.ScmObj* %k48445, i64 1)
%argslist54855$_37map480430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56584 = alloca %struct.ScmObj*, align 8
%argslist54855$_37map480431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48164, %struct.ScmObj* %argslist54855$_37map480430)
store volatile %struct.ScmObj* %argslist54855$_37map480431, %struct.ScmObj** %stackaddr$prim56584, align 8
%stackaddr$prim56585 = alloca %struct.ScmObj*, align 8
%argslist54855$_37map480432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48045, %struct.ScmObj* %argslist54855$_37map480431)
store volatile %struct.ScmObj* %argslist54855$_37map480432, %struct.ScmObj** %stackaddr$prim56585, align 8
%stackaddr$prim56586 = alloca %struct.ScmObj*, align 8
%argslist54855$_37map480433 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48696, %struct.ScmObj* %argslist54855$_37map480432)
store volatile %struct.ScmObj* %argslist54855$_37map480433, %struct.ScmObj** %stackaddr$prim56586, align 8
%clofunc56587 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map48043)
musttail call tailcc void %clofunc56587(%struct.ScmObj* %_37map48043, %struct.ScmObj* %argslist54855$_37map480433)
ret void
}

define tailcc void @proc_clo$ae48696(%struct.ScmObj* %env$ae48696,%struct.ScmObj* %current_45args54851) {
%stackaddr$env-ref56588 = alloca %struct.ScmObj*, align 8
%anf_45bind48163 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48696, i64 0)
store %struct.ScmObj* %anf_45bind48163, %struct.ScmObj** %stackaddr$env-ref56588
%stackaddr$env-ref56589 = alloca %struct.ScmObj*, align 8
%k48445 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48696, i64 1)
store %struct.ScmObj* %k48445, %struct.ScmObj** %stackaddr$env-ref56589
%stackaddr$prim56590 = alloca %struct.ScmObj*, align 8
%_95k48447 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54851)
store volatile %struct.ScmObj* %_95k48447, %struct.ScmObj** %stackaddr$prim56590, align 8
%stackaddr$prim56591 = alloca %struct.ScmObj*, align 8
%current_45args54852 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54851)
store volatile %struct.ScmObj* %current_45args54852, %struct.ScmObj** %stackaddr$prim56591, align 8
%stackaddr$prim56592 = alloca %struct.ScmObj*, align 8
%anf_45bind48165 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54852)
store volatile %struct.ScmObj* %anf_45bind48165, %struct.ScmObj** %stackaddr$prim56592, align 8
%stackaddr$prim56593 = alloca %struct.ScmObj*, align 8
%cpsprim48448 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48163, %struct.ScmObj* %anf_45bind48165)
store volatile %struct.ScmObj* %cpsprim48448, %struct.ScmObj** %stackaddr$prim56593, align 8
%ae48702 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54854$k484450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56594 = alloca %struct.ScmObj*, align 8
%argslist54854$k484451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48448, %struct.ScmObj* %argslist54854$k484450)
store volatile %struct.ScmObj* %argslist54854$k484451, %struct.ScmObj** %stackaddr$prim56594, align 8
%stackaddr$prim56595 = alloca %struct.ScmObj*, align 8
%argslist54854$k484452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48702, %struct.ScmObj* %argslist54854$k484451)
store volatile %struct.ScmObj* %argslist54854$k484452, %struct.ScmObj** %stackaddr$prim56595, align 8
%clofunc56596 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48445)
musttail call tailcc void %clofunc56596(%struct.ScmObj* %k48445, %struct.ScmObj* %argslist54854$k484452)
ret void
}

define tailcc void @proc_clo$ae48595(%struct.ScmObj* %env$ae48595,%struct.ScmObj* %current_45args54860) {
%stackaddr$prim56597 = alloca %struct.ScmObj*, align 8
%k48449 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54860)
store volatile %struct.ScmObj* %k48449, %struct.ScmObj** %stackaddr$prim56597, align 8
%stackaddr$prim56598 = alloca %struct.ScmObj*, align 8
%current_45args54861 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54860)
store volatile %struct.ScmObj* %current_45args54861, %struct.ScmObj** %stackaddr$prim56598, align 8
%stackaddr$prim56599 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54861)
store volatile %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$prim56599, align 8
%ae48597 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56600 = alloca %struct.ScmObj*, align 8
%fptrToInt56601 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48598 to i64
%ae48598 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56601)
store volatile %struct.ScmObj* %ae48598, %struct.ScmObj** %stackaddr$makeclosure56600, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48598, %struct.ScmObj* %_37foldr148047, i64 0)
%argslist54874$k484490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56602 = alloca %struct.ScmObj*, align 8
%argslist54874$k484491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48598, %struct.ScmObj* %argslist54874$k484490)
store volatile %struct.ScmObj* %argslist54874$k484491, %struct.ScmObj** %stackaddr$prim56602, align 8
%stackaddr$prim56603 = alloca %struct.ScmObj*, align 8
%argslist54874$k484492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48597, %struct.ScmObj* %argslist54874$k484491)
store volatile %struct.ScmObj* %argslist54874$k484492, %struct.ScmObj** %stackaddr$prim56603, align 8
%clofunc56604 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48449)
musttail call tailcc void %clofunc56604(%struct.ScmObj* %k48449, %struct.ScmObj* %argslist54874$k484492)
ret void
}

define tailcc void @proc_clo$ae48598(%struct.ScmObj* %env$ae48598,%struct.ScmObj* %current_45args54863) {
%stackaddr$env-ref56605 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48598, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref56605
%stackaddr$prim56606 = alloca %struct.ScmObj*, align 8
%k48450 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54863)
store volatile %struct.ScmObj* %k48450, %struct.ScmObj** %stackaddr$prim56606, align 8
%stackaddr$prim56607 = alloca %struct.ScmObj*, align 8
%current_45args54864 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54863)
store volatile %struct.ScmObj* %current_45args54864, %struct.ScmObj** %stackaddr$prim56607, align 8
%stackaddr$prim56608 = alloca %struct.ScmObj*, align 8
%f48050 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54864)
store volatile %struct.ScmObj* %f48050, %struct.ScmObj** %stackaddr$prim56608, align 8
%stackaddr$prim56609 = alloca %struct.ScmObj*, align 8
%current_45args54865 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54864)
store volatile %struct.ScmObj* %current_45args54865, %struct.ScmObj** %stackaddr$prim56609, align 8
%stackaddr$prim56610 = alloca %struct.ScmObj*, align 8
%acc48049 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54865)
store volatile %struct.ScmObj* %acc48049, %struct.ScmObj** %stackaddr$prim56610, align 8
%stackaddr$prim56611 = alloca %struct.ScmObj*, align 8
%current_45args54866 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54865)
store volatile %struct.ScmObj* %current_45args54866, %struct.ScmObj** %stackaddr$prim56611, align 8
%stackaddr$prim56612 = alloca %struct.ScmObj*, align 8
%lst48048 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54866)
store volatile %struct.ScmObj* %lst48048, %struct.ScmObj** %stackaddr$prim56612, align 8
%stackaddr$prim56613 = alloca %struct.ScmObj*, align 8
%anf_45bind48156 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48048)
store volatile %struct.ScmObj* %anf_45bind48156, %struct.ScmObj** %stackaddr$prim56613, align 8
%truthy$cmp56614 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48156)
%cmp$cmp56614 = icmp eq i64 %truthy$cmp56614, 1
br i1 %cmp$cmp56614, label %truebranch$cmp56614, label %falsebranch$cmp56614
truebranch$cmp56614:
%ae48602 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54868$k484500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56615 = alloca %struct.ScmObj*, align 8
%argslist54868$k484501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48049, %struct.ScmObj* %argslist54868$k484500)
store volatile %struct.ScmObj* %argslist54868$k484501, %struct.ScmObj** %stackaddr$prim56615, align 8
%stackaddr$prim56616 = alloca %struct.ScmObj*, align 8
%argslist54868$k484502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48602, %struct.ScmObj* %argslist54868$k484501)
store volatile %struct.ScmObj* %argslist54868$k484502, %struct.ScmObj** %stackaddr$prim56616, align 8
%clofunc56617 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48450)
musttail call tailcc void %clofunc56617(%struct.ScmObj* %k48450, %struct.ScmObj* %argslist54868$k484502)
ret void
falsebranch$cmp56614:
%stackaddr$prim56618 = alloca %struct.ScmObj*, align 8
%anf_45bind48157 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48048)
store volatile %struct.ScmObj* %anf_45bind48157, %struct.ScmObj** %stackaddr$prim56618, align 8
%stackaddr$prim56619 = alloca %struct.ScmObj*, align 8
%anf_45bind48158 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48048)
store volatile %struct.ScmObj* %anf_45bind48158, %struct.ScmObj** %stackaddr$prim56619, align 8
%stackaddr$makeclosure56620 = alloca %struct.ScmObj*, align 8
%fptrToInt56621 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48610 to i64
%ae48610 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56621)
store volatile %struct.ScmObj* %ae48610, %struct.ScmObj** %stackaddr$makeclosure56620, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48610, %struct.ScmObj* %f48050, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48610, %struct.ScmObj* %k48450, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48610, %struct.ScmObj* %anf_45bind48157, i64 2)
%argslist54873$_37foldr1480470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56622 = alloca %struct.ScmObj*, align 8
%argslist54873$_37foldr1480471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48158, %struct.ScmObj* %argslist54873$_37foldr1480470)
store volatile %struct.ScmObj* %argslist54873$_37foldr1480471, %struct.ScmObj** %stackaddr$prim56622, align 8
%stackaddr$prim56623 = alloca %struct.ScmObj*, align 8
%argslist54873$_37foldr1480472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48049, %struct.ScmObj* %argslist54873$_37foldr1480471)
store volatile %struct.ScmObj* %argslist54873$_37foldr1480472, %struct.ScmObj** %stackaddr$prim56623, align 8
%stackaddr$prim56624 = alloca %struct.ScmObj*, align 8
%argslist54873$_37foldr1480473 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48050, %struct.ScmObj* %argslist54873$_37foldr1480472)
store volatile %struct.ScmObj* %argslist54873$_37foldr1480473, %struct.ScmObj** %stackaddr$prim56624, align 8
%stackaddr$prim56625 = alloca %struct.ScmObj*, align 8
%argslist54873$_37foldr1480474 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48610, %struct.ScmObj* %argslist54873$_37foldr1480473)
store volatile %struct.ScmObj* %argslist54873$_37foldr1480474, %struct.ScmObj** %stackaddr$prim56625, align 8
%clofunc56626 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148047)
musttail call tailcc void %clofunc56626(%struct.ScmObj* %_37foldr148047, %struct.ScmObj* %argslist54873$_37foldr1480474)
ret void
}

define tailcc void @proc_clo$ae48610(%struct.ScmObj* %env$ae48610,%struct.ScmObj* %current_45args54869) {
%stackaddr$env-ref56627 = alloca %struct.ScmObj*, align 8
%f48050 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48610, i64 0)
store %struct.ScmObj* %f48050, %struct.ScmObj** %stackaddr$env-ref56627
%stackaddr$env-ref56628 = alloca %struct.ScmObj*, align 8
%k48450 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48610, i64 1)
store %struct.ScmObj* %k48450, %struct.ScmObj** %stackaddr$env-ref56628
%stackaddr$env-ref56629 = alloca %struct.ScmObj*, align 8
%anf_45bind48157 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48610, i64 2)
store %struct.ScmObj* %anf_45bind48157, %struct.ScmObj** %stackaddr$env-ref56629
%stackaddr$prim56630 = alloca %struct.ScmObj*, align 8
%_95k48451 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54869)
store volatile %struct.ScmObj* %_95k48451, %struct.ScmObj** %stackaddr$prim56630, align 8
%stackaddr$prim56631 = alloca %struct.ScmObj*, align 8
%current_45args54870 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54869)
store volatile %struct.ScmObj* %current_45args54870, %struct.ScmObj** %stackaddr$prim56631, align 8
%stackaddr$prim56632 = alloca %struct.ScmObj*, align 8
%anf_45bind48159 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54870)
store volatile %struct.ScmObj* %anf_45bind48159, %struct.ScmObj** %stackaddr$prim56632, align 8
%argslist54872$f480500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56633 = alloca %struct.ScmObj*, align 8
%argslist54872$f480501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48159, %struct.ScmObj* %argslist54872$f480500)
store volatile %struct.ScmObj* %argslist54872$f480501, %struct.ScmObj** %stackaddr$prim56633, align 8
%stackaddr$prim56634 = alloca %struct.ScmObj*, align 8
%argslist54872$f480502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48157, %struct.ScmObj* %argslist54872$f480501)
store volatile %struct.ScmObj* %argslist54872$f480502, %struct.ScmObj** %stackaddr$prim56634, align 8
%stackaddr$prim56635 = alloca %struct.ScmObj*, align 8
%argslist54872$f480503 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48450, %struct.ScmObj* %argslist54872$f480502)
store volatile %struct.ScmObj* %argslist54872$f480503, %struct.ScmObj** %stackaddr$prim56635, align 8
%clofunc56636 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48050)
musttail call tailcc void %clofunc56636(%struct.ScmObj* %f48050, %struct.ScmObj* %argslist54872$f480503)
ret void
}

define tailcc void @proc_clo$ae48478(%struct.ScmObj* %env$ae48478,%struct.ScmObj* %current_45args54877) {
%stackaddr$prim56637 = alloca %struct.ScmObj*, align 8
%k48452 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54877)
store volatile %struct.ScmObj* %k48452, %struct.ScmObj** %stackaddr$prim56637, align 8
%stackaddr$prim56638 = alloca %struct.ScmObj*, align 8
%current_45args54878 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54877)
store volatile %struct.ScmObj* %current_45args54878, %struct.ScmObj** %stackaddr$prim56638, align 8
%stackaddr$prim56639 = alloca %struct.ScmObj*, align 8
%y48027 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54878)
store volatile %struct.ScmObj* %y48027, %struct.ScmObj** %stackaddr$prim56639, align 8
%ae48480 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56640 = alloca %struct.ScmObj*, align 8
%fptrToInt56641 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48481 to i64
%ae48481 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56641)
store volatile %struct.ScmObj* %ae48481, %struct.ScmObj** %stackaddr$makeclosure56640, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48481, %struct.ScmObj* %y48027, i64 0)
%argslist54896$k484520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56642 = alloca %struct.ScmObj*, align 8
%argslist54896$k484521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48481, %struct.ScmObj* %argslist54896$k484520)
store volatile %struct.ScmObj* %argslist54896$k484521, %struct.ScmObj** %stackaddr$prim56642, align 8
%stackaddr$prim56643 = alloca %struct.ScmObj*, align 8
%argslist54896$k484522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48480, %struct.ScmObj* %argslist54896$k484521)
store volatile %struct.ScmObj* %argslist54896$k484522, %struct.ScmObj** %stackaddr$prim56643, align 8
%clofunc56644 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48452)
musttail call tailcc void %clofunc56644(%struct.ScmObj* %k48452, %struct.ScmObj* %argslist54896$k484522)
ret void
}

define tailcc void @proc_clo$ae48481(%struct.ScmObj* %env$ae48481,%struct.ScmObj* %current_45args54880) {
%stackaddr$env-ref56645 = alloca %struct.ScmObj*, align 8
%y48027 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48481, i64 0)
store %struct.ScmObj* %y48027, %struct.ScmObj** %stackaddr$env-ref56645
%stackaddr$prim56646 = alloca %struct.ScmObj*, align 8
%k48453 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54880)
store volatile %struct.ScmObj* %k48453, %struct.ScmObj** %stackaddr$prim56646, align 8
%stackaddr$prim56647 = alloca %struct.ScmObj*, align 8
%current_45args54881 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54880)
store volatile %struct.ScmObj* %current_45args54881, %struct.ScmObj** %stackaddr$prim56647, align 8
%stackaddr$prim56648 = alloca %struct.ScmObj*, align 8
%f48028 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54881)
store volatile %struct.ScmObj* %f48028, %struct.ScmObj** %stackaddr$prim56648, align 8
%stackaddr$makeclosure56649 = alloca %struct.ScmObj*, align 8
%fptrToInt56650 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48482 to i64
%ae48482 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56650)
store volatile %struct.ScmObj* %ae48482, %struct.ScmObj** %stackaddr$makeclosure56649, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48482, %struct.ScmObj* %k48453, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48482, %struct.ScmObj* %f48028, i64 1)
%ae48483 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56651 = alloca %struct.ScmObj*, align 8
%fptrToInt56652 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48484 to i64
%ae48484 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56652)
store volatile %struct.ScmObj* %ae48484, %struct.ScmObj** %stackaddr$makeclosure56651, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48484, %struct.ScmObj* %f48028, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48484, %struct.ScmObj* %y48027, i64 1)
%argslist54895$ae484820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56653 = alloca %struct.ScmObj*, align 8
%argslist54895$ae484821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48484, %struct.ScmObj* %argslist54895$ae484820)
store volatile %struct.ScmObj* %argslist54895$ae484821, %struct.ScmObj** %stackaddr$prim56653, align 8
%stackaddr$prim56654 = alloca %struct.ScmObj*, align 8
%argslist54895$ae484822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48483, %struct.ScmObj* %argslist54895$ae484821)
store volatile %struct.ScmObj* %argslist54895$ae484822, %struct.ScmObj** %stackaddr$prim56654, align 8
%clofunc56655 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48482)
musttail call tailcc void %clofunc56655(%struct.ScmObj* %ae48482, %struct.ScmObj* %argslist54895$ae484822)
ret void
}

define tailcc void @proc_clo$ae48482(%struct.ScmObj* %env$ae48482,%struct.ScmObj* %current_45args54883) {
%stackaddr$env-ref56656 = alloca %struct.ScmObj*, align 8
%k48453 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48482, i64 0)
store %struct.ScmObj* %k48453, %struct.ScmObj** %stackaddr$env-ref56656
%stackaddr$env-ref56657 = alloca %struct.ScmObj*, align 8
%f48028 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48482, i64 1)
store %struct.ScmObj* %f48028, %struct.ScmObj** %stackaddr$env-ref56657
%stackaddr$prim56658 = alloca %struct.ScmObj*, align 8
%_95k48454 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54883)
store volatile %struct.ScmObj* %_95k48454, %struct.ScmObj** %stackaddr$prim56658, align 8
%stackaddr$prim56659 = alloca %struct.ScmObj*, align 8
%current_45args54884 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54883)
store volatile %struct.ScmObj* %current_45args54884, %struct.ScmObj** %stackaddr$prim56659, align 8
%stackaddr$prim56660 = alloca %struct.ScmObj*, align 8
%anf_45bind48154 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54884)
store volatile %struct.ScmObj* %anf_45bind48154, %struct.ScmObj** %stackaddr$prim56660, align 8
%argslist54886$f480280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56661 = alloca %struct.ScmObj*, align 8
%argslist54886$f480281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48154, %struct.ScmObj* %argslist54886$f480280)
store volatile %struct.ScmObj* %argslist54886$f480281, %struct.ScmObj** %stackaddr$prim56661, align 8
%stackaddr$prim56662 = alloca %struct.ScmObj*, align 8
%argslist54886$f480282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48453, %struct.ScmObj* %argslist54886$f480281)
store volatile %struct.ScmObj* %argslist54886$f480282, %struct.ScmObj** %stackaddr$prim56662, align 8
%clofunc56663 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48028)
musttail call tailcc void %clofunc56663(%struct.ScmObj* %f48028, %struct.ScmObj* %argslist54886$f480282)
ret void
}

define tailcc void @proc_clo$ae48484(%struct.ScmObj* %env$ae48484,%struct.ScmObj* %args4802948455) {
%stackaddr$env-ref56664 = alloca %struct.ScmObj*, align 8
%f48028 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48484, i64 0)
store %struct.ScmObj* %f48028, %struct.ScmObj** %stackaddr$env-ref56664
%stackaddr$env-ref56665 = alloca %struct.ScmObj*, align 8
%y48027 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48484, i64 1)
store %struct.ScmObj* %y48027, %struct.ScmObj** %stackaddr$env-ref56665
%stackaddr$prim56666 = alloca %struct.ScmObj*, align 8
%k48456 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4802948455)
store volatile %struct.ScmObj* %k48456, %struct.ScmObj** %stackaddr$prim56666, align 8
%stackaddr$prim56667 = alloca %struct.ScmObj*, align 8
%args48029 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4802948455)
store volatile %struct.ScmObj* %args48029, %struct.ScmObj** %stackaddr$prim56667, align 8
%stackaddr$makeclosure56668 = alloca %struct.ScmObj*, align 8
%fptrToInt56669 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48488 to i64
%ae48488 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56669)
store volatile %struct.ScmObj* %ae48488, %struct.ScmObj** %stackaddr$makeclosure56668, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48488, %struct.ScmObj* %k48456, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48488, %struct.ScmObj* %args48029, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48488, %struct.ScmObj* %f48028, i64 2)
%argslist54894$y480270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56670 = alloca %struct.ScmObj*, align 8
%argslist54894$y480271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y48027, %struct.ScmObj* %argslist54894$y480270)
store volatile %struct.ScmObj* %argslist54894$y480271, %struct.ScmObj** %stackaddr$prim56670, align 8
%stackaddr$prim56671 = alloca %struct.ScmObj*, align 8
%argslist54894$y480272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48488, %struct.ScmObj* %argslist54894$y480271)
store volatile %struct.ScmObj* %argslist54894$y480272, %struct.ScmObj** %stackaddr$prim56671, align 8
%clofunc56672 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y48027)
musttail call tailcc void %clofunc56672(%struct.ScmObj* %y48027, %struct.ScmObj* %argslist54894$y480272)
ret void
}

define tailcc void @proc_clo$ae48488(%struct.ScmObj* %env$ae48488,%struct.ScmObj* %current_45args54887) {
%stackaddr$env-ref56673 = alloca %struct.ScmObj*, align 8
%k48456 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48488, i64 0)
store %struct.ScmObj* %k48456, %struct.ScmObj** %stackaddr$env-ref56673
%stackaddr$env-ref56674 = alloca %struct.ScmObj*, align 8
%args48029 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48488, i64 1)
store %struct.ScmObj* %args48029, %struct.ScmObj** %stackaddr$env-ref56674
%stackaddr$env-ref56675 = alloca %struct.ScmObj*, align 8
%f48028 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48488, i64 2)
store %struct.ScmObj* %f48028, %struct.ScmObj** %stackaddr$env-ref56675
%stackaddr$prim56676 = alloca %struct.ScmObj*, align 8
%_95k48457 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54887)
store volatile %struct.ScmObj* %_95k48457, %struct.ScmObj** %stackaddr$prim56676, align 8
%stackaddr$prim56677 = alloca %struct.ScmObj*, align 8
%current_45args54888 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54887)
store volatile %struct.ScmObj* %current_45args54888, %struct.ScmObj** %stackaddr$prim56677, align 8
%stackaddr$prim56678 = alloca %struct.ScmObj*, align 8
%anf_45bind48152 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54888)
store volatile %struct.ScmObj* %anf_45bind48152, %struct.ScmObj** %stackaddr$prim56678, align 8
%stackaddr$makeclosure56679 = alloca %struct.ScmObj*, align 8
%fptrToInt56680 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48491 to i64
%ae48491 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56680)
store volatile %struct.ScmObj* %ae48491, %struct.ScmObj** %stackaddr$makeclosure56679, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48491, %struct.ScmObj* %k48456, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48491, %struct.ScmObj* %args48029, i64 1)
%argslist54893$anf_45bind481520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56681 = alloca %struct.ScmObj*, align 8
%argslist54893$anf_45bind481521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48028, %struct.ScmObj* %argslist54893$anf_45bind481520)
store volatile %struct.ScmObj* %argslist54893$anf_45bind481521, %struct.ScmObj** %stackaddr$prim56681, align 8
%stackaddr$prim56682 = alloca %struct.ScmObj*, align 8
%argslist54893$anf_45bind481522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48491, %struct.ScmObj* %argslist54893$anf_45bind481521)
store volatile %struct.ScmObj* %argslist54893$anf_45bind481522, %struct.ScmObj** %stackaddr$prim56682, align 8
%clofunc56683 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48152)
musttail call tailcc void %clofunc56683(%struct.ScmObj* %anf_45bind48152, %struct.ScmObj* %argslist54893$anf_45bind481522)
ret void
}

define tailcc void @proc_clo$ae48491(%struct.ScmObj* %env$ae48491,%struct.ScmObj* %current_45args54890) {
%stackaddr$env-ref56684 = alloca %struct.ScmObj*, align 8
%k48456 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48491, i64 0)
store %struct.ScmObj* %k48456, %struct.ScmObj** %stackaddr$env-ref56684
%stackaddr$env-ref56685 = alloca %struct.ScmObj*, align 8
%args48029 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48491, i64 1)
store %struct.ScmObj* %args48029, %struct.ScmObj** %stackaddr$env-ref56685
%stackaddr$prim56686 = alloca %struct.ScmObj*, align 8
%_95k48458 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54890)
store volatile %struct.ScmObj* %_95k48458, %struct.ScmObj** %stackaddr$prim56686, align 8
%stackaddr$prim56687 = alloca %struct.ScmObj*, align 8
%current_45args54891 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54890)
store volatile %struct.ScmObj* %current_45args54891, %struct.ScmObj** %stackaddr$prim56687, align 8
%stackaddr$prim56688 = alloca %struct.ScmObj*, align 8
%anf_45bind48153 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54891)
store volatile %struct.ScmObj* %anf_45bind48153, %struct.ScmObj** %stackaddr$prim56688, align 8
%stackaddr$prim56689 = alloca %struct.ScmObj*, align 8
%cpsargs48459 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48456, %struct.ScmObj* %args48029)
store volatile %struct.ScmObj* %cpsargs48459, %struct.ScmObj** %stackaddr$prim56689, align 8
%clofunc56690 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48153)
musttail call tailcc void %clofunc56690(%struct.ScmObj* %anf_45bind48153, %struct.ScmObj* %cpsargs48459)
ret void
}

define tailcc void @proc_clo$ae48463(%struct.ScmObj* %env$ae48463,%struct.ScmObj* %current_45args54898) {
%stackaddr$prim56691 = alloca %struct.ScmObj*, align 8
%k48460 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54898)
store volatile %struct.ScmObj* %k48460, %struct.ScmObj** %stackaddr$prim56691, align 8
%stackaddr$prim56692 = alloca %struct.ScmObj*, align 8
%current_45args54899 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54898)
store volatile %struct.ScmObj* %current_45args54899, %struct.ScmObj** %stackaddr$prim56692, align 8
%stackaddr$prim56693 = alloca %struct.ScmObj*, align 8
%yu48026 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54899)
store volatile %struct.ScmObj* %yu48026, %struct.ScmObj** %stackaddr$prim56693, align 8
%argslist54901$yu480260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56694 = alloca %struct.ScmObj*, align 8
%argslist54901$yu480261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu48026, %struct.ScmObj* %argslist54901$yu480260)
store volatile %struct.ScmObj* %argslist54901$yu480261, %struct.ScmObj** %stackaddr$prim56694, align 8
%stackaddr$prim56695 = alloca %struct.ScmObj*, align 8
%argslist54901$yu480262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48460, %struct.ScmObj* %argslist54901$yu480261)
store volatile %struct.ScmObj* %argslist54901$yu480262, %struct.ScmObj** %stackaddr$prim56695, align 8
%clofunc56696 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu48026)
musttail call tailcc void %clofunc56696(%struct.ScmObj* %yu48026, %struct.ScmObj* %argslist54901$yu480262)
ret void
}