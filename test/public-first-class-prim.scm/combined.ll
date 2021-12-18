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

@global$sym$ae4393547802 = private unnamed_addr constant [8 x i8] c"promise\00", align 8

define ccc i32 @main() {
%mainenv47198 = call %struct.ScmObj* @const_init_null()
%mainargs47199 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv47198, %struct.ScmObj* %mainargs47199)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv47196,%struct.ScmObj* %mainargs47197) {
%stackaddr$makeclosure47200 = alloca %struct.ScmObj*, align 8
%fptrToInt47201 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40547 to i64
%ae40547 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47201)
store volatile %struct.ScmObj* %ae40547, %struct.ScmObj** %stackaddr$makeclosure47200, align 8
%ae40548 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47202 = alloca %struct.ScmObj*, align 8
%fptrToInt47203 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40549 to i64
%ae40549 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47203)
store volatile %struct.ScmObj* %ae40549, %struct.ScmObj** %stackaddr$makeclosure47202, align 8
%args47195$ae40547$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47204 = alloca %struct.ScmObj*, align 8
%args47195$ae40547$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40549, %struct.ScmObj* %args47195$ae40547$0)
store volatile %struct.ScmObj* %args47195$ae40547$1, %struct.ScmObj** %stackaddr$prim47204, align 8
%stackaddr$prim47205 = alloca %struct.ScmObj*, align 8
%args47195$ae40547$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40548, %struct.ScmObj* %args47195$ae40547$1)
store volatile %struct.ScmObj* %args47195$ae40547$2, %struct.ScmObj** %stackaddr$prim47205, align 8
%clofunc47206 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40547)
musttail call tailcc void %clofunc47206(%struct.ScmObj* %ae40547, %struct.ScmObj* %args47195$ae40547$2)
ret void
}

define tailcc void @proc_clo$ae40547(%struct.ScmObj* %env$ae40547,%struct.ScmObj* %current_45args46604) {
%stackaddr$prim47207 = alloca %struct.ScmObj*, align 8
%_95k40358 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46604)
store volatile %struct.ScmObj* %_95k40358, %struct.ScmObj** %stackaddr$prim47207, align 8
%stackaddr$prim47208 = alloca %struct.ScmObj*, align 8
%current_45args46605 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46604)
store volatile %struct.ScmObj* %current_45args46605, %struct.ScmObj** %stackaddr$prim47208, align 8
%stackaddr$prim47209 = alloca %struct.ScmObj*, align 8
%anf_45bind40230 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46605)
store volatile %struct.ScmObj* %anf_45bind40230, %struct.ScmObj** %stackaddr$prim47209, align 8
%stackaddr$makeclosure47210 = alloca %struct.ScmObj*, align 8
%fptrToInt47211 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40562 to i64
%ae40562 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47211)
store volatile %struct.ScmObj* %ae40562, %struct.ScmObj** %stackaddr$makeclosure47210, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40562, %struct.ScmObj* %anf_45bind40230, i64 0)
%ae40563 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47212 = alloca %struct.ScmObj*, align 8
%fptrToInt47213 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40564 to i64
%ae40564 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47213)
store volatile %struct.ScmObj* %ae40564, %struct.ScmObj** %stackaddr$makeclosure47212, align 8
%args47190$ae40562$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47214 = alloca %struct.ScmObj*, align 8
%args47190$ae40562$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40564, %struct.ScmObj* %args47190$ae40562$0)
store volatile %struct.ScmObj* %args47190$ae40562$1, %struct.ScmObj** %stackaddr$prim47214, align 8
%stackaddr$prim47215 = alloca %struct.ScmObj*, align 8
%args47190$ae40562$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40563, %struct.ScmObj* %args47190$ae40562$1)
store volatile %struct.ScmObj* %args47190$ae40562$2, %struct.ScmObj** %stackaddr$prim47215, align 8
%clofunc47216 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40562)
musttail call tailcc void %clofunc47216(%struct.ScmObj* %ae40562, %struct.ScmObj* %args47190$ae40562$2)
ret void
}

define tailcc void @proc_clo$ae40562(%struct.ScmObj* %env$ae40562,%struct.ScmObj* %current_45args46607) {
%stackaddr$env-ref47217 = alloca %struct.ScmObj*, align 8
%anf_45bind40230 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40562, i64 0)
store %struct.ScmObj* %anf_45bind40230, %struct.ScmObj** %stackaddr$env-ref47217
%stackaddr$prim47218 = alloca %struct.ScmObj*, align 8
%_95k40359 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46607)
store volatile %struct.ScmObj* %_95k40359, %struct.ScmObj** %stackaddr$prim47218, align 8
%stackaddr$prim47219 = alloca %struct.ScmObj*, align 8
%current_45args46608 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46607)
store volatile %struct.ScmObj* %current_45args46608, %struct.ScmObj** %stackaddr$prim47219, align 8
%stackaddr$prim47220 = alloca %struct.ScmObj*, align 8
%anf_45bind40234 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46608)
store volatile %struct.ScmObj* %anf_45bind40234, %struct.ScmObj** %stackaddr$prim47220, align 8
%stackaddr$makeclosure47221 = alloca %struct.ScmObj*, align 8
%fptrToInt47222 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40677 to i64
%ae40677 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47222)
store volatile %struct.ScmObj* %ae40677, %struct.ScmObj** %stackaddr$makeclosure47221, align 8
%args47169$anf_45bind40230$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47223 = alloca %struct.ScmObj*, align 8
%args47169$anf_45bind40230$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40234, %struct.ScmObj* %args47169$anf_45bind40230$0)
store volatile %struct.ScmObj* %args47169$anf_45bind40230$1, %struct.ScmObj** %stackaddr$prim47223, align 8
%stackaddr$prim47224 = alloca %struct.ScmObj*, align 8
%args47169$anf_45bind40230$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40677, %struct.ScmObj* %args47169$anf_45bind40230$1)
store volatile %struct.ScmObj* %args47169$anf_45bind40230$2, %struct.ScmObj** %stackaddr$prim47224, align 8
%clofunc47225 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40230)
musttail call tailcc void %clofunc47225(%struct.ScmObj* %anf_45bind40230, %struct.ScmObj* %args47169$anf_45bind40230$2)
ret void
}

define tailcc void @proc_clo$ae40677(%struct.ScmObj* %env$ae40677,%struct.ScmObj* %current_45args46610) {
%stackaddr$prim47226 = alloca %struct.ScmObj*, align 8
%_95k40360 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46610)
store volatile %struct.ScmObj* %_95k40360, %struct.ScmObj** %stackaddr$prim47226, align 8
%stackaddr$prim47227 = alloca %struct.ScmObj*, align 8
%current_45args46611 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46610)
store volatile %struct.ScmObj* %current_45args46611, %struct.ScmObj** %stackaddr$prim47227, align 8
%stackaddr$prim47228 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46611)
store volatile %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$prim47228, align 8
%stackaddr$makeclosure47229 = alloca %struct.ScmObj*, align 8
%fptrToInt47230 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40679 to i64
%ae40679 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47230)
store volatile %struct.ScmObj* %ae40679, %struct.ScmObj** %stackaddr$makeclosure47229, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40679, %struct.ScmObj* %Ycmb40102, i64 0)
%ae40680 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47231 = alloca %struct.ScmObj*, align 8
%fptrToInt47232 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40681 to i64
%ae40681 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47232)
store volatile %struct.ScmObj* %ae40681, %struct.ScmObj** %stackaddr$makeclosure47231, align 8
%args47168$ae40679$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47233 = alloca %struct.ScmObj*, align 8
%args47168$ae40679$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40681, %struct.ScmObj* %args47168$ae40679$0)
store volatile %struct.ScmObj* %args47168$ae40679$1, %struct.ScmObj** %stackaddr$prim47233, align 8
%stackaddr$prim47234 = alloca %struct.ScmObj*, align 8
%args47168$ae40679$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40680, %struct.ScmObj* %args47168$ae40679$1)
store volatile %struct.ScmObj* %args47168$ae40679$2, %struct.ScmObj** %stackaddr$prim47234, align 8
%clofunc47235 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40679)
musttail call tailcc void %clofunc47235(%struct.ScmObj* %ae40679, %struct.ScmObj* %args47168$ae40679$2)
ret void
}

define tailcc void @proc_clo$ae40679(%struct.ScmObj* %env$ae40679,%struct.ScmObj* %current_45args46613) {
%stackaddr$env-ref47236 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40679, i64 0)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47236
%stackaddr$prim47237 = alloca %struct.ScmObj*, align 8
%_95k40361 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46613)
store volatile %struct.ScmObj* %_95k40361, %struct.ScmObj** %stackaddr$prim47237, align 8
%stackaddr$prim47238 = alloca %struct.ScmObj*, align 8
%current_45args46614 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46613)
store volatile %struct.ScmObj* %current_45args46614, %struct.ScmObj** %stackaddr$prim47238, align 8
%stackaddr$prim47239 = alloca %struct.ScmObj*, align 8
%anf_45bind40239 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46614)
store volatile %struct.ScmObj* %anf_45bind40239, %struct.ScmObj** %stackaddr$prim47239, align 8
%stackaddr$makeclosure47240 = alloca %struct.ScmObj*, align 8
%fptrToInt47241 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40757 to i64
%ae40757 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47241)
store volatile %struct.ScmObj* %ae40757, %struct.ScmObj** %stackaddr$makeclosure47240, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40757, %struct.ScmObj* %Ycmb40102, i64 0)
%args47152$Ycmb40102$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47242 = alloca %struct.ScmObj*, align 8
%args47152$Ycmb40102$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40239, %struct.ScmObj* %args47152$Ycmb40102$0)
store volatile %struct.ScmObj* %args47152$Ycmb40102$1, %struct.ScmObj** %stackaddr$prim47242, align 8
%stackaddr$prim47243 = alloca %struct.ScmObj*, align 8
%args47152$Ycmb40102$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40757, %struct.ScmObj* %args47152$Ycmb40102$1)
store volatile %struct.ScmObj* %args47152$Ycmb40102$2, %struct.ScmObj** %stackaddr$prim47243, align 8
%clofunc47244 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40102)
musttail call tailcc void %clofunc47244(%struct.ScmObj* %Ycmb40102, %struct.ScmObj* %args47152$Ycmb40102$2)
ret void
}

define tailcc void @proc_clo$ae40757(%struct.ScmObj* %env$ae40757,%struct.ScmObj* %current_45args46616) {
%stackaddr$env-ref47245 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40757, i64 0)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47245
%stackaddr$prim47246 = alloca %struct.ScmObj*, align 8
%_95k40362 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46616)
store volatile %struct.ScmObj* %_95k40362, %struct.ScmObj** %stackaddr$prim47246, align 8
%stackaddr$prim47247 = alloca %struct.ScmObj*, align 8
%current_45args46617 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46616)
store volatile %struct.ScmObj* %current_45args46617, %struct.ScmObj** %stackaddr$prim47247, align 8
%stackaddr$prim47248 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46617)
store volatile %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$prim47248, align 8
%stackaddr$makeclosure47249 = alloca %struct.ScmObj*, align 8
%fptrToInt47250 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40759 to i64
%ae40759 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47250)
store volatile %struct.ScmObj* %ae40759, %struct.ScmObj** %stackaddr$makeclosure47249, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40759, %struct.ScmObj* %Ycmb40102, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40759, %struct.ScmObj* %_37foldr140123, i64 1)
%ae40760 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47251 = alloca %struct.ScmObj*, align 8
%fptrToInt47252 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40761 to i64
%ae40761 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47252)
store volatile %struct.ScmObj* %ae40761, %struct.ScmObj** %stackaddr$makeclosure47251, align 8
%args47151$ae40759$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47253 = alloca %struct.ScmObj*, align 8
%args47151$ae40759$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40761, %struct.ScmObj* %args47151$ae40759$0)
store volatile %struct.ScmObj* %args47151$ae40759$1, %struct.ScmObj** %stackaddr$prim47253, align 8
%stackaddr$prim47254 = alloca %struct.ScmObj*, align 8
%args47151$ae40759$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40760, %struct.ScmObj* %args47151$ae40759$1)
store volatile %struct.ScmObj* %args47151$ae40759$2, %struct.ScmObj** %stackaddr$prim47254, align 8
%clofunc47255 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40759)
musttail call tailcc void %clofunc47255(%struct.ScmObj* %ae40759, %struct.ScmObj* %args47151$ae40759$2)
ret void
}

define tailcc void @proc_clo$ae40759(%struct.ScmObj* %env$ae40759,%struct.ScmObj* %current_45args46619) {
%stackaddr$env-ref47256 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40759, i64 0)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47256
%stackaddr$env-ref47257 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40759, i64 1)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47257
%stackaddr$prim47258 = alloca %struct.ScmObj*, align 8
%_95k40363 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46619)
store volatile %struct.ScmObj* %_95k40363, %struct.ScmObj** %stackaddr$prim47258, align 8
%stackaddr$prim47259 = alloca %struct.ScmObj*, align 8
%current_45args46620 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46619)
store volatile %struct.ScmObj* %current_45args46620, %struct.ScmObj** %stackaddr$prim47259, align 8
%stackaddr$prim47260 = alloca %struct.ScmObj*, align 8
%anf_45bind40245 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46620)
store volatile %struct.ScmObj* %anf_45bind40245, %struct.ScmObj** %stackaddr$prim47260, align 8
%stackaddr$makeclosure47261 = alloca %struct.ScmObj*, align 8
%fptrToInt47262 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40854 to i64
%ae40854 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47262)
store volatile %struct.ScmObj* %ae40854, %struct.ScmObj** %stackaddr$makeclosure47261, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40854, %struct.ScmObj* %Ycmb40102, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40854, %struct.ScmObj* %_37foldr140123, i64 1)
%args47132$Ycmb40102$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47263 = alloca %struct.ScmObj*, align 8
%args47132$Ycmb40102$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40245, %struct.ScmObj* %args47132$Ycmb40102$0)
store volatile %struct.ScmObj* %args47132$Ycmb40102$1, %struct.ScmObj** %stackaddr$prim47263, align 8
%stackaddr$prim47264 = alloca %struct.ScmObj*, align 8
%args47132$Ycmb40102$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40854, %struct.ScmObj* %args47132$Ycmb40102$1)
store volatile %struct.ScmObj* %args47132$Ycmb40102$2, %struct.ScmObj** %stackaddr$prim47264, align 8
%clofunc47265 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40102)
musttail call tailcc void %clofunc47265(%struct.ScmObj* %Ycmb40102, %struct.ScmObj* %args47132$Ycmb40102$2)
ret void
}

define tailcc void @proc_clo$ae40854(%struct.ScmObj* %env$ae40854,%struct.ScmObj* %current_45args46622) {
%stackaddr$env-ref47266 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40854, i64 0)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47266
%stackaddr$env-ref47267 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40854, i64 1)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47267
%stackaddr$prim47268 = alloca %struct.ScmObj*, align 8
%_95k40364 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46622)
store volatile %struct.ScmObj* %_95k40364, %struct.ScmObj** %stackaddr$prim47268, align 8
%stackaddr$prim47269 = alloca %struct.ScmObj*, align 8
%current_45args46623 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46622)
store volatile %struct.ScmObj* %current_45args46623, %struct.ScmObj** %stackaddr$prim47269, align 8
%stackaddr$prim47270 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46623)
store volatile %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$prim47270, align 8
%stackaddr$makeclosure47271 = alloca %struct.ScmObj*, align 8
%fptrToInt47272 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40856 to i64
%ae40856 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47272)
store volatile %struct.ScmObj* %ae40856, %struct.ScmObj** %stackaddr$makeclosure47271, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40856, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40856, %struct.ScmObj* %Ycmb40102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40856, %struct.ScmObj* %_37foldr140123, i64 2)
%ae40857 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47273 = alloca %struct.ScmObj*, align 8
%fptrToInt47274 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40858 to i64
%ae40858 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47274)
store volatile %struct.ScmObj* %ae40858, %struct.ScmObj** %stackaddr$makeclosure47273, align 8
%args47131$ae40856$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47275 = alloca %struct.ScmObj*, align 8
%args47131$ae40856$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40858, %struct.ScmObj* %args47131$ae40856$0)
store volatile %struct.ScmObj* %args47131$ae40856$1, %struct.ScmObj** %stackaddr$prim47275, align 8
%stackaddr$prim47276 = alloca %struct.ScmObj*, align 8
%args47131$ae40856$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40857, %struct.ScmObj* %args47131$ae40856$1)
store volatile %struct.ScmObj* %args47131$ae40856$2, %struct.ScmObj** %stackaddr$prim47276, align 8
%clofunc47277 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40856)
musttail call tailcc void %clofunc47277(%struct.ScmObj* %ae40856, %struct.ScmObj* %args47131$ae40856$2)
ret void
}

define tailcc void @proc_clo$ae40856(%struct.ScmObj* %env$ae40856,%struct.ScmObj* %current_45args46625) {
%stackaddr$env-ref47278 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40856, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47278
%stackaddr$env-ref47279 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40856, i64 1)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47279
%stackaddr$env-ref47280 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40856, i64 2)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47280
%stackaddr$prim47281 = alloca %struct.ScmObj*, align 8
%_95k40365 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46625)
store volatile %struct.ScmObj* %_95k40365, %struct.ScmObj** %stackaddr$prim47281, align 8
%stackaddr$prim47282 = alloca %struct.ScmObj*, align 8
%current_45args46626 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46625)
store volatile %struct.ScmObj* %current_45args46626, %struct.ScmObj** %stackaddr$prim47282, align 8
%stackaddr$prim47283 = alloca %struct.ScmObj*, align 8
%anf_45bind40252 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46626)
store volatile %struct.ScmObj* %anf_45bind40252, %struct.ScmObj** %stackaddr$prim47283, align 8
%stackaddr$makeclosure47284 = alloca %struct.ScmObj*, align 8
%fptrToInt47285 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41004 to i64
%ae41004 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47285)
store volatile %struct.ScmObj* %ae41004, %struct.ScmObj** %stackaddr$makeclosure47284, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41004, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41004, %struct.ScmObj* %Ycmb40102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41004, %struct.ScmObj* %_37foldr140123, i64 2)
%args47115$Ycmb40102$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47286 = alloca %struct.ScmObj*, align 8
%args47115$Ycmb40102$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40252, %struct.ScmObj* %args47115$Ycmb40102$0)
store volatile %struct.ScmObj* %args47115$Ycmb40102$1, %struct.ScmObj** %stackaddr$prim47286, align 8
%stackaddr$prim47287 = alloca %struct.ScmObj*, align 8
%args47115$Ycmb40102$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41004, %struct.ScmObj* %args47115$Ycmb40102$1)
store volatile %struct.ScmObj* %args47115$Ycmb40102$2, %struct.ScmObj** %stackaddr$prim47287, align 8
%clofunc47288 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40102)
musttail call tailcc void %clofunc47288(%struct.ScmObj* %Ycmb40102, %struct.ScmObj* %args47115$Ycmb40102$2)
ret void
}

define tailcc void @proc_clo$ae41004(%struct.ScmObj* %env$ae41004,%struct.ScmObj* %current_45args46628) {
%stackaddr$env-ref47289 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41004, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47289
%stackaddr$env-ref47290 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41004, i64 1)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47290
%stackaddr$env-ref47291 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41004, i64 2)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47291
%stackaddr$prim47292 = alloca %struct.ScmObj*, align 8
%_95k40366 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46628)
store volatile %struct.ScmObj* %_95k40366, %struct.ScmObj** %stackaddr$prim47292, align 8
%stackaddr$prim47293 = alloca %struct.ScmObj*, align 8
%current_45args46629 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46628)
store volatile %struct.ScmObj* %current_45args46629, %struct.ScmObj** %stackaddr$prim47293, align 8
%stackaddr$prim47294 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46629)
store volatile %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$prim47294, align 8
%stackaddr$makeclosure47295 = alloca %struct.ScmObj*, align 8
%fptrToInt47296 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41006 to i64
%ae41006 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt47296)
store volatile %struct.ScmObj* %ae41006, %struct.ScmObj** %stackaddr$makeclosure47295, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41006, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41006, %struct.ScmObj* %Ycmb40102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41006, %struct.ScmObj* %_37take40115, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41006, %struct.ScmObj* %_37foldr140123, i64 3)
%ae41007 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47297 = alloca %struct.ScmObj*, align 8
%fptrToInt47298 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41008 to i64
%ae41008 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47298)
store volatile %struct.ScmObj* %ae41008, %struct.ScmObj** %stackaddr$makeclosure47297, align 8
%args47114$ae41006$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47299 = alloca %struct.ScmObj*, align 8
%args47114$ae41006$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41008, %struct.ScmObj* %args47114$ae41006$0)
store volatile %struct.ScmObj* %args47114$ae41006$1, %struct.ScmObj** %stackaddr$prim47299, align 8
%stackaddr$prim47300 = alloca %struct.ScmObj*, align 8
%args47114$ae41006$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41007, %struct.ScmObj* %args47114$ae41006$1)
store volatile %struct.ScmObj* %args47114$ae41006$2, %struct.ScmObj** %stackaddr$prim47300, align 8
%clofunc47301 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41006)
musttail call tailcc void %clofunc47301(%struct.ScmObj* %ae41006, %struct.ScmObj* %args47114$ae41006$2)
ret void
}

define tailcc void @proc_clo$ae41006(%struct.ScmObj* %env$ae41006,%struct.ScmObj* %current_45args46631) {
%stackaddr$env-ref47302 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41006, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47302
%stackaddr$env-ref47303 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41006, i64 1)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47303
%stackaddr$env-ref47304 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41006, i64 2)
store %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$env-ref47304
%stackaddr$env-ref47305 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41006, i64 3)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47305
%stackaddr$prim47306 = alloca %struct.ScmObj*, align 8
%_95k40367 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46631)
store volatile %struct.ScmObj* %_95k40367, %struct.ScmObj** %stackaddr$prim47306, align 8
%stackaddr$prim47307 = alloca %struct.ScmObj*, align 8
%current_45args46632 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46631)
store volatile %struct.ScmObj* %current_45args46632, %struct.ScmObj** %stackaddr$prim47307, align 8
%stackaddr$prim47308 = alloca %struct.ScmObj*, align 8
%anf_45bind40256 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46632)
store volatile %struct.ScmObj* %anf_45bind40256, %struct.ScmObj** %stackaddr$prim47308, align 8
%stackaddr$makeclosure47309 = alloca %struct.ScmObj*, align 8
%fptrToInt47310 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41087 to i64
%ae41087 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt47310)
store volatile %struct.ScmObj* %ae41087, %struct.ScmObj** %stackaddr$makeclosure47309, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41087, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41087, %struct.ScmObj* %Ycmb40102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41087, %struct.ScmObj* %_37take40115, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41087, %struct.ScmObj* %_37foldr140123, i64 3)
%args47100$Ycmb40102$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47311 = alloca %struct.ScmObj*, align 8
%args47100$Ycmb40102$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40256, %struct.ScmObj* %args47100$Ycmb40102$0)
store volatile %struct.ScmObj* %args47100$Ycmb40102$1, %struct.ScmObj** %stackaddr$prim47311, align 8
%stackaddr$prim47312 = alloca %struct.ScmObj*, align 8
%args47100$Ycmb40102$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41087, %struct.ScmObj* %args47100$Ycmb40102$1)
store volatile %struct.ScmObj* %args47100$Ycmb40102$2, %struct.ScmObj** %stackaddr$prim47312, align 8
%clofunc47313 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40102)
musttail call tailcc void %clofunc47313(%struct.ScmObj* %Ycmb40102, %struct.ScmObj* %args47100$Ycmb40102$2)
ret void
}

define tailcc void @proc_clo$ae41087(%struct.ScmObj* %env$ae41087,%struct.ScmObj* %current_45args46634) {
%stackaddr$env-ref47314 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41087, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47314
%stackaddr$env-ref47315 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41087, i64 1)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47315
%stackaddr$env-ref47316 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41087, i64 2)
store %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$env-ref47316
%stackaddr$env-ref47317 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41087, i64 3)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47317
%stackaddr$prim47318 = alloca %struct.ScmObj*, align 8
%_95k40368 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46634)
store volatile %struct.ScmObj* %_95k40368, %struct.ScmObj** %stackaddr$prim47318, align 8
%stackaddr$prim47319 = alloca %struct.ScmObj*, align 8
%current_45args46635 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46634)
store volatile %struct.ScmObj* %current_45args46635, %struct.ScmObj** %stackaddr$prim47319, align 8
%stackaddr$prim47320 = alloca %struct.ScmObj*, align 8
%_37length40112 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46635)
store volatile %struct.ScmObj* %_37length40112, %struct.ScmObj** %stackaddr$prim47320, align 8
%stackaddr$makeclosure47321 = alloca %struct.ScmObj*, align 8
%fptrToInt47322 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41089 to i64
%ae41089 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt47322)
store volatile %struct.ScmObj* %ae41089, %struct.ScmObj** %stackaddr$makeclosure47321, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41089, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41089, %struct.ScmObj* %Ycmb40102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41089, %struct.ScmObj* %_37take40115, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41089, %struct.ScmObj* %_37length40112, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41089, %struct.ScmObj* %_37foldr140123, i64 4)
%ae41090 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47323 = alloca %struct.ScmObj*, align 8
%fptrToInt47324 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41091 to i64
%ae41091 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47324)
store volatile %struct.ScmObj* %ae41091, %struct.ScmObj** %stackaddr$makeclosure47323, align 8
%args47099$ae41089$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47325 = alloca %struct.ScmObj*, align 8
%args47099$ae41089$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41091, %struct.ScmObj* %args47099$ae41089$0)
store volatile %struct.ScmObj* %args47099$ae41089$1, %struct.ScmObj** %stackaddr$prim47325, align 8
%stackaddr$prim47326 = alloca %struct.ScmObj*, align 8
%args47099$ae41089$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41090, %struct.ScmObj* %args47099$ae41089$1)
store volatile %struct.ScmObj* %args47099$ae41089$2, %struct.ScmObj** %stackaddr$prim47326, align 8
%clofunc47327 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41089)
musttail call tailcc void %clofunc47327(%struct.ScmObj* %ae41089, %struct.ScmObj* %args47099$ae41089$2)
ret void
}

define tailcc void @proc_clo$ae41089(%struct.ScmObj* %env$ae41089,%struct.ScmObj* %current_45args46637) {
%stackaddr$env-ref47328 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41089, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47328
%stackaddr$env-ref47329 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41089, i64 1)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47329
%stackaddr$env-ref47330 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41089, i64 2)
store %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$env-ref47330
%stackaddr$env-ref47331 = alloca %struct.ScmObj*, align 8
%_37length40112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41089, i64 3)
store %struct.ScmObj* %_37length40112, %struct.ScmObj** %stackaddr$env-ref47331
%stackaddr$env-ref47332 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41089, i64 4)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47332
%stackaddr$prim47333 = alloca %struct.ScmObj*, align 8
%_95k40369 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46637)
store volatile %struct.ScmObj* %_95k40369, %struct.ScmObj** %stackaddr$prim47333, align 8
%stackaddr$prim47334 = alloca %struct.ScmObj*, align 8
%current_45args46638 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46637)
store volatile %struct.ScmObj* %current_45args46638, %struct.ScmObj** %stackaddr$prim47334, align 8
%stackaddr$prim47335 = alloca %struct.ScmObj*, align 8
%anf_45bind40261 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46638)
store volatile %struct.ScmObj* %anf_45bind40261, %struct.ScmObj** %stackaddr$prim47335, align 8
%stackaddr$makeclosure47336 = alloca %struct.ScmObj*, align 8
%fptrToInt47337 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41166 to i64
%ae41166 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt47337)
store volatile %struct.ScmObj* %ae41166, %struct.ScmObj** %stackaddr$makeclosure47336, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41166, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41166, %struct.ScmObj* %Ycmb40102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41166, %struct.ScmObj* %_37take40115, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41166, %struct.ScmObj* %_37length40112, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41166, %struct.ScmObj* %_37foldr140123, i64 4)
%args47083$Ycmb40102$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47338 = alloca %struct.ScmObj*, align 8
%args47083$Ycmb40102$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40261, %struct.ScmObj* %args47083$Ycmb40102$0)
store volatile %struct.ScmObj* %args47083$Ycmb40102$1, %struct.ScmObj** %stackaddr$prim47338, align 8
%stackaddr$prim47339 = alloca %struct.ScmObj*, align 8
%args47083$Ycmb40102$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41166, %struct.ScmObj* %args47083$Ycmb40102$1)
store volatile %struct.ScmObj* %args47083$Ycmb40102$2, %struct.ScmObj** %stackaddr$prim47339, align 8
%clofunc47340 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40102)
musttail call tailcc void %clofunc47340(%struct.ScmObj* %Ycmb40102, %struct.ScmObj* %args47083$Ycmb40102$2)
ret void
}

define tailcc void @proc_clo$ae41166(%struct.ScmObj* %env$ae41166,%struct.ScmObj* %current_45args46640) {
%stackaddr$env-ref47341 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41166, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47341
%stackaddr$env-ref47342 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41166, i64 1)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47342
%stackaddr$env-ref47343 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41166, i64 2)
store %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$env-ref47343
%stackaddr$env-ref47344 = alloca %struct.ScmObj*, align 8
%_37length40112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41166, i64 3)
store %struct.ScmObj* %_37length40112, %struct.ScmObj** %stackaddr$env-ref47344
%stackaddr$env-ref47345 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41166, i64 4)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47345
%stackaddr$prim47346 = alloca %struct.ScmObj*, align 8
%_95k40370 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46640)
store volatile %struct.ScmObj* %_95k40370, %struct.ScmObj** %stackaddr$prim47346, align 8
%stackaddr$prim47347 = alloca %struct.ScmObj*, align 8
%current_45args46641 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46640)
store volatile %struct.ScmObj* %current_45args46641, %struct.ScmObj** %stackaddr$prim47347, align 8
%stackaddr$prim47348 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46641)
store volatile %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$prim47348, align 8
%stackaddr$makeclosure47349 = alloca %struct.ScmObj*, align 8
%fptrToInt47350 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41168 to i64
%ae41168 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt47350)
store volatile %struct.ScmObj* %ae41168, %struct.ScmObj** %stackaddr$makeclosure47349, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41168, %struct.ScmObj* %_37foldr140123, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41168, %struct.ScmObj* %_37foldl140107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41168, %struct.ScmObj* %_37map140119, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41168, %struct.ScmObj* %Ycmb40102, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41168, %struct.ScmObj* %_37take40115, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41168, %struct.ScmObj* %_37length40112, i64 5)
%ae41169 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47351 = alloca %struct.ScmObj*, align 8
%fptrToInt47352 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41170 to i64
%ae41170 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47352)
store volatile %struct.ScmObj* %ae41170, %struct.ScmObj** %stackaddr$makeclosure47351, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41170, %struct.ScmObj* %_37foldl140107, i64 0)
%args47082$ae41168$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47353 = alloca %struct.ScmObj*, align 8
%args47082$ae41168$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41170, %struct.ScmObj* %args47082$ae41168$0)
store volatile %struct.ScmObj* %args47082$ae41168$1, %struct.ScmObj** %stackaddr$prim47353, align 8
%stackaddr$prim47354 = alloca %struct.ScmObj*, align 8
%args47082$ae41168$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41169, %struct.ScmObj* %args47082$ae41168$1)
store volatile %struct.ScmObj* %args47082$ae41168$2, %struct.ScmObj** %stackaddr$prim47354, align 8
%clofunc47355 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41168)
musttail call tailcc void %clofunc47355(%struct.ScmObj* %ae41168, %struct.ScmObj* %args47082$ae41168$2)
ret void
}

define tailcc void @proc_clo$ae41168(%struct.ScmObj* %env$ae41168,%struct.ScmObj* %current_45args46643) {
%stackaddr$env-ref47356 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41168, i64 0)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47356
%stackaddr$env-ref47357 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41168, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47357
%stackaddr$env-ref47358 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41168, i64 2)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47358
%stackaddr$env-ref47359 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41168, i64 3)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47359
%stackaddr$env-ref47360 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41168, i64 4)
store %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$env-ref47360
%stackaddr$env-ref47361 = alloca %struct.ScmObj*, align 8
%_37length40112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41168, i64 5)
store %struct.ScmObj* %_37length40112, %struct.ScmObj** %stackaddr$env-ref47361
%stackaddr$prim47362 = alloca %struct.ScmObj*, align 8
%_95k40371 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46643)
store volatile %struct.ScmObj* %_95k40371, %struct.ScmObj** %stackaddr$prim47362, align 8
%stackaddr$prim47363 = alloca %struct.ScmObj*, align 8
%current_45args46644 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46643)
store volatile %struct.ScmObj* %current_45args46644, %struct.ScmObj** %stackaddr$prim47363, align 8
%stackaddr$prim47364 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46644)
store volatile %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$prim47364, align 8
%stackaddr$makeclosure47365 = alloca %struct.ScmObj*, align 8
%fptrToInt47366 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41222 to i64
%ae41222 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt47366)
store volatile %struct.ScmObj* %ae41222, %struct.ScmObj** %stackaddr$makeclosure47365, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41222, %struct.ScmObj* %_37foldr140123, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41222, %struct.ScmObj* %_37foldl140107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41222, %struct.ScmObj* %_37map140119, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41222, %struct.ScmObj* %Ycmb40102, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41222, %struct.ScmObj* %_37last40145, i64 4)
%ae41223 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47367 = alloca %struct.ScmObj*, align 8
%fptrToInt47368 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41224 to i64
%ae41224 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47368)
store volatile %struct.ScmObj* %ae41224, %struct.ScmObj** %stackaddr$makeclosure47367, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41224, %struct.ScmObj* %_37take40115, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41224, %struct.ScmObj* %_37length40112, i64 1)
%args47068$ae41222$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47369 = alloca %struct.ScmObj*, align 8
%args47068$ae41222$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41224, %struct.ScmObj* %args47068$ae41222$0)
store volatile %struct.ScmObj* %args47068$ae41222$1, %struct.ScmObj** %stackaddr$prim47369, align 8
%stackaddr$prim47370 = alloca %struct.ScmObj*, align 8
%args47068$ae41222$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41223, %struct.ScmObj* %args47068$ae41222$1)
store volatile %struct.ScmObj* %args47068$ae41222$2, %struct.ScmObj** %stackaddr$prim47370, align 8
%clofunc47371 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41222)
musttail call tailcc void %clofunc47371(%struct.ScmObj* %ae41222, %struct.ScmObj* %args47068$ae41222$2)
ret void
}

define tailcc void @proc_clo$ae41222(%struct.ScmObj* %env$ae41222,%struct.ScmObj* %current_45args46646) {
%stackaddr$env-ref47372 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41222, i64 0)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47372
%stackaddr$env-ref47373 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41222, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47373
%stackaddr$env-ref47374 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41222, i64 2)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47374
%stackaddr$env-ref47375 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41222, i64 3)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47375
%stackaddr$env-ref47376 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41222, i64 4)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref47376
%stackaddr$prim47377 = alloca %struct.ScmObj*, align 8
%_95k40372 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46646)
store volatile %struct.ScmObj* %_95k40372, %struct.ScmObj** %stackaddr$prim47377, align 8
%stackaddr$prim47378 = alloca %struct.ScmObj*, align 8
%current_45args46647 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46646)
store volatile %struct.ScmObj* %current_45args46647, %struct.ScmObj** %stackaddr$prim47378, align 8
%stackaddr$prim47379 = alloca %struct.ScmObj*, align 8
%_37drop_45right40142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46647)
store volatile %struct.ScmObj* %_37drop_45right40142, %struct.ScmObj** %stackaddr$prim47379, align 8
%stackaddr$makeclosure47380 = alloca %struct.ScmObj*, align 8
%fptrToInt47381 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41252 to i64
%ae41252 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt47381)
store volatile %struct.ScmObj* %ae41252, %struct.ScmObj** %stackaddr$makeclosure47380, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41252, %struct.ScmObj* %_37foldr140123, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41252, %struct.ScmObj* %_37foldl140107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41252, %struct.ScmObj* %Ycmb40102, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41252, %struct.ScmObj* %_37last40145, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41252, %struct.ScmObj* %_37drop_45right40142, i64 4)
%ae41253 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47382 = alloca %struct.ScmObj*, align 8
%fptrToInt47383 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41254 to i64
%ae41254 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47383)
store volatile %struct.ScmObj* %ae41254, %struct.ScmObj** %stackaddr$makeclosure47382, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41254, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41254, %struct.ScmObj* %_37foldr140123, i64 1)
%args47058$ae41252$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47384 = alloca %struct.ScmObj*, align 8
%args47058$ae41252$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41254, %struct.ScmObj* %args47058$ae41252$0)
store volatile %struct.ScmObj* %args47058$ae41252$1, %struct.ScmObj** %stackaddr$prim47384, align 8
%stackaddr$prim47385 = alloca %struct.ScmObj*, align 8
%args47058$ae41252$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41253, %struct.ScmObj* %args47058$ae41252$1)
store volatile %struct.ScmObj* %args47058$ae41252$2, %struct.ScmObj** %stackaddr$prim47385, align 8
%clofunc47386 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41252)
musttail call tailcc void %clofunc47386(%struct.ScmObj* %ae41252, %struct.ScmObj* %args47058$ae41252$2)
ret void
}

define tailcc void @proc_clo$ae41252(%struct.ScmObj* %env$ae41252,%struct.ScmObj* %current_45args46649) {
%stackaddr$env-ref47387 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41252, i64 0)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47387
%stackaddr$env-ref47388 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41252, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47388
%stackaddr$env-ref47389 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41252, i64 2)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47389
%stackaddr$env-ref47390 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41252, i64 3)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref47390
%stackaddr$env-ref47391 = alloca %struct.ScmObj*, align 8
%_37drop_45right40142 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41252, i64 4)
store %struct.ScmObj* %_37drop_45right40142, %struct.ScmObj** %stackaddr$env-ref47391
%stackaddr$prim47392 = alloca %struct.ScmObj*, align 8
%_95k40373 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46649)
store volatile %struct.ScmObj* %_95k40373, %struct.ScmObj** %stackaddr$prim47392, align 8
%stackaddr$prim47393 = alloca %struct.ScmObj*, align 8
%current_45args46650 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46649)
store volatile %struct.ScmObj* %current_45args46650, %struct.ScmObj** %stackaddr$prim47393, align 8
%stackaddr$prim47394 = alloca %struct.ScmObj*, align 8
%anf_45bind40277 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46650)
store volatile %struct.ScmObj* %anf_45bind40277, %struct.ScmObj** %stackaddr$prim47394, align 8
%stackaddr$makeclosure47395 = alloca %struct.ScmObj*, align 8
%fptrToInt47396 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41636 to i64
%ae41636 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt47396)
store volatile %struct.ScmObj* %ae41636, %struct.ScmObj** %stackaddr$makeclosure47395, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41636, %struct.ScmObj* %_37foldr140123, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41636, %struct.ScmObj* %_37foldl140107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41636, %struct.ScmObj* %Ycmb40102, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41636, %struct.ScmObj* %_37last40145, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41636, %struct.ScmObj* %_37drop_45right40142, i64 4)
%args46998$Ycmb40102$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47397 = alloca %struct.ScmObj*, align 8
%args46998$Ycmb40102$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40277, %struct.ScmObj* %args46998$Ycmb40102$0)
store volatile %struct.ScmObj* %args46998$Ycmb40102$1, %struct.ScmObj** %stackaddr$prim47397, align 8
%stackaddr$prim47398 = alloca %struct.ScmObj*, align 8
%args46998$Ycmb40102$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41636, %struct.ScmObj* %args46998$Ycmb40102$1)
store volatile %struct.ScmObj* %args46998$Ycmb40102$2, %struct.ScmObj** %stackaddr$prim47398, align 8
%clofunc47399 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40102)
musttail call tailcc void %clofunc47399(%struct.ScmObj* %Ycmb40102, %struct.ScmObj* %args46998$Ycmb40102$2)
ret void
}

define tailcc void @proc_clo$ae41636(%struct.ScmObj* %env$ae41636,%struct.ScmObj* %current_45args46652) {
%stackaddr$env-ref47400 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41636, i64 0)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47400
%stackaddr$env-ref47401 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41636, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47401
%stackaddr$env-ref47402 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41636, i64 2)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47402
%stackaddr$env-ref47403 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41636, i64 3)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref47403
%stackaddr$env-ref47404 = alloca %struct.ScmObj*, align 8
%_37drop_45right40142 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41636, i64 4)
store %struct.ScmObj* %_37drop_45right40142, %struct.ScmObj** %stackaddr$env-ref47404
%stackaddr$prim47405 = alloca %struct.ScmObj*, align 8
%_95k40374 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46652)
store volatile %struct.ScmObj* %_95k40374, %struct.ScmObj** %stackaddr$prim47405, align 8
%stackaddr$prim47406 = alloca %struct.ScmObj*, align 8
%current_45args46653 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46652)
store volatile %struct.ScmObj* %current_45args46653, %struct.ScmObj** %stackaddr$prim47406, align 8
%stackaddr$prim47407 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46653)
store volatile %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$prim47407, align 8
%stackaddr$makeclosure47408 = alloca %struct.ScmObj*, align 8
%fptrToInt47409 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41638 to i64
%ae41638 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt47409)
store volatile %struct.ScmObj* %ae41638, %struct.ScmObj** %stackaddr$makeclosure47408, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41638, %struct.ScmObj* %_37foldr140123, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41638, %struct.ScmObj* %_37foldl140107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41638, %struct.ScmObj* %Ycmb40102, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41638, %struct.ScmObj* %_37last40145, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41638, %struct.ScmObj* %_37foldr40128, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41638, %struct.ScmObj* %_37drop_45right40142, i64 5)
%ae41639 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47410 = alloca %struct.ScmObj*, align 8
%fptrToInt47411 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41640 to i64
%ae41640 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47411)
store volatile %struct.ScmObj* %ae41640, %struct.ScmObj** %stackaddr$makeclosure47410, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41640, %struct.ScmObj* %_37foldr140123, i64 0)
%args46997$ae41638$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47412 = alloca %struct.ScmObj*, align 8
%args46997$ae41638$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41640, %struct.ScmObj* %args46997$ae41638$0)
store volatile %struct.ScmObj* %args46997$ae41638$1, %struct.ScmObj** %stackaddr$prim47412, align 8
%stackaddr$prim47413 = alloca %struct.ScmObj*, align 8
%args46997$ae41638$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41639, %struct.ScmObj* %args46997$ae41638$1)
store volatile %struct.ScmObj* %args46997$ae41638$2, %struct.ScmObj** %stackaddr$prim47413, align 8
%clofunc47414 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41638)
musttail call tailcc void %clofunc47414(%struct.ScmObj* %ae41638, %struct.ScmObj* %args46997$ae41638$2)
ret void
}

define tailcc void @proc_clo$ae41638(%struct.ScmObj* %env$ae41638,%struct.ScmObj* %current_45args46655) {
%stackaddr$env-ref47415 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41638, i64 0)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47415
%stackaddr$env-ref47416 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41638, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47416
%stackaddr$env-ref47417 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41638, i64 2)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47417
%stackaddr$env-ref47418 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41638, i64 3)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref47418
%stackaddr$env-ref47419 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41638, i64 4)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref47419
%stackaddr$env-ref47420 = alloca %struct.ScmObj*, align 8
%_37drop_45right40142 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41638, i64 5)
store %struct.ScmObj* %_37drop_45right40142, %struct.ScmObj** %stackaddr$env-ref47420
%stackaddr$prim47421 = alloca %struct.ScmObj*, align 8
%_95k40375 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46655)
store volatile %struct.ScmObj* %_95k40375, %struct.ScmObj** %stackaddr$prim47421, align 8
%stackaddr$prim47422 = alloca %struct.ScmObj*, align 8
%current_45args46656 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46655)
store volatile %struct.ScmObj* %current_45args46656, %struct.ScmObj** %stackaddr$prim47422, align 8
%stackaddr$prim47423 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46656)
store volatile %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$prim47423, align 8
%stackaddr$makeclosure47424 = alloca %struct.ScmObj*, align 8
%fptrToInt47425 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41715 to i64
%ae41715 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt47425)
store volatile %struct.ScmObj* %ae41715, %struct.ScmObj** %stackaddr$makeclosure47424, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41715, %struct.ScmObj* %_37foldr140123, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41715, %struct.ScmObj* %_37foldl140107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41715, %struct.ScmObj* %Ycmb40102, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41715, %struct.ScmObj* %_37foldr40128, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41715, %struct.ScmObj* %_37map140154, i64 4)
%ae41716 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47426 = alloca %struct.ScmObj*, align 8
%fptrToInt47427 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41717 to i64
%ae41717 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47427)
store volatile %struct.ScmObj* %ae41717, %struct.ScmObj** %stackaddr$makeclosure47426, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41717, %struct.ScmObj* %_37last40145, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41717, %struct.ScmObj* %_37foldr40128, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41717, %struct.ScmObj* %_37drop_45right40142, i64 2)
%args46978$ae41715$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47428 = alloca %struct.ScmObj*, align 8
%args46978$ae41715$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41717, %struct.ScmObj* %args46978$ae41715$0)
store volatile %struct.ScmObj* %args46978$ae41715$1, %struct.ScmObj** %stackaddr$prim47428, align 8
%stackaddr$prim47429 = alloca %struct.ScmObj*, align 8
%args46978$ae41715$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41716, %struct.ScmObj* %args46978$ae41715$1)
store volatile %struct.ScmObj* %args46978$ae41715$2, %struct.ScmObj** %stackaddr$prim47429, align 8
%clofunc47430 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41715)
musttail call tailcc void %clofunc47430(%struct.ScmObj* %ae41715, %struct.ScmObj* %args46978$ae41715$2)
ret void
}

define tailcc void @proc_clo$ae41715(%struct.ScmObj* %env$ae41715,%struct.ScmObj* %current_45args46658) {
%stackaddr$env-ref47431 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41715, i64 0)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47431
%stackaddr$env-ref47432 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41715, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47432
%stackaddr$env-ref47433 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41715, i64 2)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47433
%stackaddr$env-ref47434 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41715, i64 3)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref47434
%stackaddr$env-ref47435 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41715, i64 4)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47435
%stackaddr$prim47436 = alloca %struct.ScmObj*, align 8
%_95k40376 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46658)
store volatile %struct.ScmObj* %_95k40376, %struct.ScmObj** %stackaddr$prim47436, align 8
%stackaddr$prim47437 = alloca %struct.ScmObj*, align 8
%current_45args46659 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46658)
store volatile %struct.ScmObj* %current_45args46659, %struct.ScmObj** %stackaddr$prim47437, align 8
%stackaddr$prim47438 = alloca %struct.ScmObj*, align 8
%_37map40149 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46659)
store volatile %struct.ScmObj* %_37map40149, %struct.ScmObj** %stackaddr$prim47438, align 8
%stackaddr$makeclosure47439 = alloca %struct.ScmObj*, align 8
%fptrToInt47440 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41861 to i64
%ae41861 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47440)
store volatile %struct.ScmObj* %ae41861, %struct.ScmObj** %stackaddr$makeclosure47439, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41861, %struct.ScmObj* %Ycmb40102, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41861, %struct.ScmObj* %_37foldl140107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41861, %struct.ScmObj* %_37map140154, i64 2)
%ae41862 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47441 = alloca %struct.ScmObj*, align 8
%fptrToInt47442 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41863 to i64
%ae41863 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47442)
store volatile %struct.ScmObj* %ae41863, %struct.ScmObj** %stackaddr$makeclosure47441, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41863, %struct.ScmObj* %_37foldr40128, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41863, %struct.ScmObj* %_37foldr140123, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41863, %struct.ScmObj* %_37map140154, i64 2)
%args46961$ae41861$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47443 = alloca %struct.ScmObj*, align 8
%args46961$ae41861$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41863, %struct.ScmObj* %args46961$ae41861$0)
store volatile %struct.ScmObj* %args46961$ae41861$1, %struct.ScmObj** %stackaddr$prim47443, align 8
%stackaddr$prim47444 = alloca %struct.ScmObj*, align 8
%args46961$ae41861$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41862, %struct.ScmObj* %args46961$ae41861$1)
store volatile %struct.ScmObj* %args46961$ae41861$2, %struct.ScmObj** %stackaddr$prim47444, align 8
%clofunc47445 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41861)
musttail call tailcc void %clofunc47445(%struct.ScmObj* %ae41861, %struct.ScmObj* %args46961$ae41861$2)
ret void
}

define tailcc void @proc_clo$ae41861(%struct.ScmObj* %env$ae41861,%struct.ScmObj* %current_45args46661) {
%stackaddr$env-ref47446 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41861, i64 0)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47446
%stackaddr$env-ref47447 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41861, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47447
%stackaddr$env-ref47448 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41861, i64 2)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47448
%stackaddr$prim47449 = alloca %struct.ScmObj*, align 8
%_95k40377 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46661)
store volatile %struct.ScmObj* %_95k40377, %struct.ScmObj** %stackaddr$prim47449, align 8
%stackaddr$prim47450 = alloca %struct.ScmObj*, align 8
%current_45args46662 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46661)
store volatile %struct.ScmObj* %current_45args46662, %struct.ScmObj** %stackaddr$prim47450, align 8
%stackaddr$prim47451 = alloca %struct.ScmObj*, align 8
%anf_45bind40297 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46662)
store volatile %struct.ScmObj* %anf_45bind40297, %struct.ScmObj** %stackaddr$prim47451, align 8
%stackaddr$makeclosure47452 = alloca %struct.ScmObj*, align 8
%fptrToInt47453 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42253 to i64
%ae42253 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47453)
store volatile %struct.ScmObj* %ae42253, %struct.ScmObj** %stackaddr$makeclosure47452, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42253, %struct.ScmObj* %_37foldl140107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42253, %struct.ScmObj* %_37map140154, i64 1)
%args46901$Ycmb40102$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47454 = alloca %struct.ScmObj*, align 8
%args46901$Ycmb40102$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40297, %struct.ScmObj* %args46901$Ycmb40102$0)
store volatile %struct.ScmObj* %args46901$Ycmb40102$1, %struct.ScmObj** %stackaddr$prim47454, align 8
%stackaddr$prim47455 = alloca %struct.ScmObj*, align 8
%args46901$Ycmb40102$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42253, %struct.ScmObj* %args46901$Ycmb40102$1)
store volatile %struct.ScmObj* %args46901$Ycmb40102$2, %struct.ScmObj** %stackaddr$prim47455, align 8
%clofunc47456 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40102)
musttail call tailcc void %clofunc47456(%struct.ScmObj* %Ycmb40102, %struct.ScmObj* %args46901$Ycmb40102$2)
ret void
}

define tailcc void @proc_clo$ae42253(%struct.ScmObj* %env$ae42253,%struct.ScmObj* %current_45args46664) {
%stackaddr$env-ref47457 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42253, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47457
%stackaddr$env-ref47458 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42253, i64 1)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47458
%stackaddr$prim47459 = alloca %struct.ScmObj*, align 8
%_95k40378 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46664)
store volatile %struct.ScmObj* %_95k40378, %struct.ScmObj** %stackaddr$prim47459, align 8
%stackaddr$prim47460 = alloca %struct.ScmObj*, align 8
%current_45args46665 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46664)
store volatile %struct.ScmObj* %current_45args46665, %struct.ScmObj** %stackaddr$prim47460, align 8
%stackaddr$prim47461 = alloca %struct.ScmObj*, align 8
%_37foldl40205 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46665)
store volatile %struct.ScmObj* %_37foldl40205, %struct.ScmObj** %stackaddr$prim47461, align 8
%stackaddr$makeclosure47462 = alloca %struct.ScmObj*, align 8
%fptrToInt47463 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42255 to i64
%ae42255 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47463)
store volatile %struct.ScmObj* %ae42255, %struct.ScmObj** %stackaddr$makeclosure47462, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42255, %struct.ScmObj* %_37foldl140107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42255, %struct.ScmObj* %_37map140154, i64 1)
%ae42256 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47464 = alloca %struct.ScmObj*, align 8
%fptrToInt47465 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42257 to i64
%ae42257 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47465)
store volatile %struct.ScmObj* %ae42257, %struct.ScmObj** %stackaddr$makeclosure47464, align 8
%args46900$ae42255$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47466 = alloca %struct.ScmObj*, align 8
%args46900$ae42255$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42257, %struct.ScmObj* %args46900$ae42255$0)
store volatile %struct.ScmObj* %args46900$ae42255$1, %struct.ScmObj** %stackaddr$prim47466, align 8
%stackaddr$prim47467 = alloca %struct.ScmObj*, align 8
%args46900$ae42255$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42256, %struct.ScmObj* %args46900$ae42255$1)
store volatile %struct.ScmObj* %args46900$ae42255$2, %struct.ScmObj** %stackaddr$prim47467, align 8
%clofunc47468 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42255)
musttail call tailcc void %clofunc47468(%struct.ScmObj* %ae42255, %struct.ScmObj* %args46900$ae42255$2)
ret void
}

define tailcc void @proc_clo$ae42255(%struct.ScmObj* %env$ae42255,%struct.ScmObj* %current_45args46667) {
%stackaddr$env-ref47469 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42255, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47469
%stackaddr$env-ref47470 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42255, i64 1)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47470
%stackaddr$prim47471 = alloca %struct.ScmObj*, align 8
%_95k40379 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46667)
store volatile %struct.ScmObj* %_95k40379, %struct.ScmObj** %stackaddr$prim47471, align 8
%stackaddr$prim47472 = alloca %struct.ScmObj*, align 8
%current_45args46668 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46667)
store volatile %struct.ScmObj* %current_45args46668, %struct.ScmObj** %stackaddr$prim47472, align 8
%stackaddr$prim47473 = alloca %struct.ScmObj*, align 8
%_37_6240202 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46668)
store volatile %struct.ScmObj* %_37_6240202, %struct.ScmObj** %stackaddr$prim47473, align 8
%stackaddr$makeclosure47474 = alloca %struct.ScmObj*, align 8
%fptrToInt47475 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42279 to i64
%ae42279 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47475)
store volatile %struct.ScmObj* %ae42279, %struct.ScmObj** %stackaddr$makeclosure47474, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42279, %struct.ScmObj* %_37foldl140107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42279, %struct.ScmObj* %_37map140154, i64 1)
%ae42280 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47476 = alloca %struct.ScmObj*, align 8
%fptrToInt47477 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42281 to i64
%ae42281 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47477)
store volatile %struct.ScmObj* %ae42281, %struct.ScmObj** %stackaddr$makeclosure47476, align 8
%args46894$ae42279$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47478 = alloca %struct.ScmObj*, align 8
%args46894$ae42279$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42281, %struct.ScmObj* %args46894$ae42279$0)
store volatile %struct.ScmObj* %args46894$ae42279$1, %struct.ScmObj** %stackaddr$prim47478, align 8
%stackaddr$prim47479 = alloca %struct.ScmObj*, align 8
%args46894$ae42279$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42280, %struct.ScmObj* %args46894$ae42279$1)
store volatile %struct.ScmObj* %args46894$ae42279$2, %struct.ScmObj** %stackaddr$prim47479, align 8
%clofunc47480 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42279)
musttail call tailcc void %clofunc47480(%struct.ScmObj* %ae42279, %struct.ScmObj* %args46894$ae42279$2)
ret void
}

define tailcc void @proc_clo$ae42279(%struct.ScmObj* %env$ae42279,%struct.ScmObj* %current_45args46670) {
%stackaddr$env-ref47481 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42279, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47481
%stackaddr$env-ref47482 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42279, i64 1)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47482
%stackaddr$prim47483 = alloca %struct.ScmObj*, align 8
%_95k40380 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46670)
store volatile %struct.ScmObj* %_95k40380, %struct.ScmObj** %stackaddr$prim47483, align 8
%stackaddr$prim47484 = alloca %struct.ScmObj*, align 8
%current_45args46671 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46670)
store volatile %struct.ScmObj* %current_45args46671, %struct.ScmObj** %stackaddr$prim47484, align 8
%stackaddr$prim47485 = alloca %struct.ScmObj*, align 8
%_37_62_6140199 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46671)
store volatile %struct.ScmObj* %_37_62_6140199, %struct.ScmObj** %stackaddr$prim47485, align 8
%ae42303 = call %struct.ScmObj* @const_init_int(i64 1)
%ae42304 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47486 = alloca %struct.ScmObj*, align 8
%_37append40195 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42303, %struct.ScmObj* %ae42304)
store volatile %struct.ScmObj* %_37append40195, %struct.ScmObj** %stackaddr$prim47486, align 8
%stackaddr$makeclosure47487 = alloca %struct.ScmObj*, align 8
%fptrToInt47488 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42305 to i64
%ae42305 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47488)
store volatile %struct.ScmObj* %ae42305, %struct.ScmObj** %stackaddr$makeclosure47487, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42305, %struct.ScmObj* %_37append40195, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42305, %struct.ScmObj* %_37foldl140107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42305, %struct.ScmObj* %_37map140154, i64 2)
%ae42306 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47489 = alloca %struct.ScmObj*, align 8
%fptrToInt47490 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42307 to i64
%ae42307 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47490)
store volatile %struct.ScmObj* %ae42307, %struct.ScmObj** %stackaddr$makeclosure47489, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42307, %struct.ScmObj* %_37append40195, i64 0)
%args46888$ae42305$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47491 = alloca %struct.ScmObj*, align 8
%args46888$ae42305$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42307, %struct.ScmObj* %args46888$ae42305$0)
store volatile %struct.ScmObj* %args46888$ae42305$1, %struct.ScmObj** %stackaddr$prim47491, align 8
%stackaddr$prim47492 = alloca %struct.ScmObj*, align 8
%args46888$ae42305$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42306, %struct.ScmObj* %args46888$ae42305$1)
store volatile %struct.ScmObj* %args46888$ae42305$2, %struct.ScmObj** %stackaddr$prim47492, align 8
%clofunc47493 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42305)
musttail call tailcc void %clofunc47493(%struct.ScmObj* %ae42305, %struct.ScmObj* %args46888$ae42305$2)
ret void
}

define tailcc void @proc_clo$ae42305(%struct.ScmObj* %env$ae42305,%struct.ScmObj* %current_45args46673) {
%stackaddr$env-ref47494 = alloca %struct.ScmObj*, align 8
%_37append40195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42305, i64 0)
store %struct.ScmObj* %_37append40195, %struct.ScmObj** %stackaddr$env-ref47494
%stackaddr$env-ref47495 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42305, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47495
%stackaddr$env-ref47496 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42305, i64 2)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47496
%stackaddr$prim47497 = alloca %struct.ScmObj*, align 8
%_95k40381 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46673)
store volatile %struct.ScmObj* %_95k40381, %struct.ScmObj** %stackaddr$prim47497, align 8
%stackaddr$prim47498 = alloca %struct.ScmObj*, align 8
%current_45args46674 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46673)
store volatile %struct.ScmObj* %current_45args46674, %struct.ScmObj** %stackaddr$prim47498, align 8
%stackaddr$prim47499 = alloca %struct.ScmObj*, align 8
%anf_45bind40305 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46674)
store volatile %struct.ScmObj* %anf_45bind40305, %struct.ScmObj** %stackaddr$prim47499, align 8
%ae42373 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47500 = alloca %struct.ScmObj*, align 8
%_95040196 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append40195, %struct.ScmObj* %ae42373, %struct.ScmObj* %anf_45bind40305)
store volatile %struct.ScmObj* %_95040196, %struct.ScmObj** %stackaddr$prim47500, align 8
%ae42376 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47501 = alloca %struct.ScmObj*, align 8
%_37append40194 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append40195, %struct.ScmObj* %ae42376)
store volatile %struct.ScmObj* %_37append40194, %struct.ScmObj** %stackaddr$prim47501, align 8
%stackaddr$makeclosure47502 = alloca %struct.ScmObj*, align 8
%fptrToInt47503 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42377 to i64
%ae42377 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47503)
store volatile %struct.ScmObj* %ae42377, %struct.ScmObj** %stackaddr$makeclosure47502, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42377, %struct.ScmObj* %_37foldl140107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42377, %struct.ScmObj* %_37map140154, i64 1)
%ae42378 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47504 = alloca %struct.ScmObj*, align 8
%fptrToInt47505 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42379 to i64
%ae42379 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47505)
store volatile %struct.ScmObj* %ae42379, %struct.ScmObj** %stackaddr$makeclosure47504, align 8
%args46877$ae42377$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47506 = alloca %struct.ScmObj*, align 8
%args46877$ae42377$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42379, %struct.ScmObj* %args46877$ae42377$0)
store volatile %struct.ScmObj* %args46877$ae42377$1, %struct.ScmObj** %stackaddr$prim47506, align 8
%stackaddr$prim47507 = alloca %struct.ScmObj*, align 8
%args46877$ae42377$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42378, %struct.ScmObj* %args46877$ae42377$1)
store volatile %struct.ScmObj* %args46877$ae42377$2, %struct.ScmObj** %stackaddr$prim47507, align 8
%clofunc47508 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42377)
musttail call tailcc void %clofunc47508(%struct.ScmObj* %ae42377, %struct.ScmObj* %args46877$ae42377$2)
ret void
}

define tailcc void @proc_clo$ae42377(%struct.ScmObj* %env$ae42377,%struct.ScmObj* %current_45args46676) {
%stackaddr$env-ref47509 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42377, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47509
%stackaddr$env-ref47510 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42377, i64 1)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47510
%stackaddr$prim47511 = alloca %struct.ScmObj*, align 8
%_95k40382 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46676)
store volatile %struct.ScmObj* %_95k40382, %struct.ScmObj** %stackaddr$prim47511, align 8
%stackaddr$prim47512 = alloca %struct.ScmObj*, align 8
%current_45args46677 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46676)
store volatile %struct.ScmObj* %current_45args46677, %struct.ScmObj** %stackaddr$prim47512, align 8
%stackaddr$prim47513 = alloca %struct.ScmObj*, align 8
%_37list_6340187 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46677)
store volatile %struct.ScmObj* %_37list_6340187, %struct.ScmObj** %stackaddr$prim47513, align 8
%stackaddr$makeclosure47514 = alloca %struct.ScmObj*, align 8
%fptrToInt47515 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42793 to i64
%ae42793 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47515)
store volatile %struct.ScmObj* %ae42793, %struct.ScmObj** %stackaddr$makeclosure47514, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42793, %struct.ScmObj* %_37foldl140107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42793, %struct.ScmObj* %_37map140154, i64 1)
%ae42794 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47516 = alloca %struct.ScmObj*, align 8
%fptrToInt47517 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42795 to i64
%ae42795 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47517)
store volatile %struct.ScmObj* %ae42795, %struct.ScmObj** %stackaddr$makeclosure47516, align 8
%args46852$ae42793$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47518 = alloca %struct.ScmObj*, align 8
%args46852$ae42793$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42795, %struct.ScmObj* %args46852$ae42793$0)
store volatile %struct.ScmObj* %args46852$ae42793$1, %struct.ScmObj** %stackaddr$prim47518, align 8
%stackaddr$prim47519 = alloca %struct.ScmObj*, align 8
%args46852$ae42793$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42794, %struct.ScmObj* %args46852$ae42793$1)
store volatile %struct.ScmObj* %args46852$ae42793$2, %struct.ScmObj** %stackaddr$prim47519, align 8
%clofunc47520 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42793)
musttail call tailcc void %clofunc47520(%struct.ScmObj* %ae42793, %struct.ScmObj* %args46852$ae42793$2)
ret void
}

define tailcc void @proc_clo$ae42793(%struct.ScmObj* %env$ae42793,%struct.ScmObj* %current_45args46679) {
%stackaddr$env-ref47521 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42793, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47521
%stackaddr$env-ref47522 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42793, i64 1)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47522
%stackaddr$prim47523 = alloca %struct.ScmObj*, align 8
%_95k40383 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46679)
store volatile %struct.ScmObj* %_95k40383, %struct.ScmObj** %stackaddr$prim47523, align 8
%stackaddr$prim47524 = alloca %struct.ScmObj*, align 8
%current_45args46680 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46679)
store volatile %struct.ScmObj* %current_45args46680, %struct.ScmObj** %stackaddr$prim47524, align 8
%stackaddr$prim47525 = alloca %struct.ScmObj*, align 8
%_37drop40178 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46680)
store volatile %struct.ScmObj* %_37drop40178, %struct.ScmObj** %stackaddr$prim47525, align 8
%stackaddr$makeclosure47526 = alloca %struct.ScmObj*, align 8
%fptrToInt47527 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43329 to i64
%ae43329 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47527)
store volatile %struct.ScmObj* %ae43329, %struct.ScmObj** %stackaddr$makeclosure47526, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43329, %struct.ScmObj* %_37foldl140107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43329, %struct.ScmObj* %_37map140154, i64 1)
%ae43330 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47528 = alloca %struct.ScmObj*, align 8
%fptrToInt47529 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43331 to i64
%ae43331 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47529)
store volatile %struct.ScmObj* %ae43331, %struct.ScmObj** %stackaddr$makeclosure47528, align 8
%args46828$ae43329$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47530 = alloca %struct.ScmObj*, align 8
%args46828$ae43329$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43331, %struct.ScmObj* %args46828$ae43329$0)
store volatile %struct.ScmObj* %args46828$ae43329$1, %struct.ScmObj** %stackaddr$prim47530, align 8
%stackaddr$prim47531 = alloca %struct.ScmObj*, align 8
%args46828$ae43329$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43330, %struct.ScmObj* %args46828$ae43329$1)
store volatile %struct.ScmObj* %args46828$ae43329$2, %struct.ScmObj** %stackaddr$prim47531, align 8
%clofunc47532 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43329)
musttail call tailcc void %clofunc47532(%struct.ScmObj* %ae43329, %struct.ScmObj* %args46828$ae43329$2)
ret void
}

define tailcc void @proc_clo$ae43329(%struct.ScmObj* %env$ae43329,%struct.ScmObj* %current_45args46682) {
%stackaddr$env-ref47533 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43329, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47533
%stackaddr$env-ref47534 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43329, i64 1)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47534
%stackaddr$prim47535 = alloca %struct.ScmObj*, align 8
%_95k40384 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46682)
store volatile %struct.ScmObj* %_95k40384, %struct.ScmObj** %stackaddr$prim47535, align 8
%stackaddr$prim47536 = alloca %struct.ScmObj*, align 8
%current_45args46683 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46682)
store volatile %struct.ScmObj* %current_45args46683, %struct.ScmObj** %stackaddr$prim47536, align 8
%stackaddr$prim47537 = alloca %struct.ScmObj*, align 8
%_37memv40171 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46683)
store volatile %struct.ScmObj* %_37memv40171, %struct.ScmObj** %stackaddr$prim47537, align 8
%stackaddr$makeclosure47538 = alloca %struct.ScmObj*, align 8
%fptrToInt47539 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43733 to i64
%ae43733 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47539)
store volatile %struct.ScmObj* %ae43733, %struct.ScmObj** %stackaddr$makeclosure47538, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43733, %struct.ScmObj* %_37foldl140107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43733, %struct.ScmObj* %_37map140154, i64 1)
%ae43734 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47540 = alloca %struct.ScmObj*, align 8
%fptrToInt47541 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43735 to i64
%ae43735 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47541)
store volatile %struct.ScmObj* %ae43735, %struct.ScmObj** %stackaddr$makeclosure47540, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43735, %struct.ScmObj* %_37foldl140107, i64 0)
%args46802$ae43733$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47542 = alloca %struct.ScmObj*, align 8
%args46802$ae43733$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43735, %struct.ScmObj* %args46802$ae43733$0)
store volatile %struct.ScmObj* %args46802$ae43733$1, %struct.ScmObj** %stackaddr$prim47542, align 8
%stackaddr$prim47543 = alloca %struct.ScmObj*, align 8
%args46802$ae43733$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43734, %struct.ScmObj* %args46802$ae43733$1)
store volatile %struct.ScmObj* %args46802$ae43733$2, %struct.ScmObj** %stackaddr$prim47543, align 8
%clofunc47544 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43733)
musttail call tailcc void %clofunc47544(%struct.ScmObj* %ae43733, %struct.ScmObj* %args46802$ae43733$2)
ret void
}

define tailcc void @proc_clo$ae43733(%struct.ScmObj* %env$ae43733,%struct.ScmObj* %current_45args46685) {
%stackaddr$env-ref47545 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43733, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47545
%stackaddr$env-ref47546 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43733, i64 1)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47546
%stackaddr$prim47547 = alloca %struct.ScmObj*, align 8
%_95k40385 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46685)
store volatile %struct.ScmObj* %_95k40385, %struct.ScmObj** %stackaddr$prim47547, align 8
%stackaddr$prim47548 = alloca %struct.ScmObj*, align 8
%current_45args46686 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46685)
store volatile %struct.ScmObj* %current_45args46686, %struct.ScmObj** %stackaddr$prim47548, align 8
%stackaddr$prim47549 = alloca %struct.ScmObj*, align 8
%_37_4740167 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46686)
store volatile %struct.ScmObj* %_37_4740167, %struct.ScmObj** %stackaddr$prim47549, align 8
%stackaddr$makeclosure47550 = alloca %struct.ScmObj*, align 8
%fptrToInt47551 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43831 to i64
%ae43831 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47551)
store volatile %struct.ScmObj* %ae43831, %struct.ScmObj** %stackaddr$makeclosure47550, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43831, %struct.ScmObj* %_37foldl140107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43831, %struct.ScmObj* %_37map140154, i64 1)
%ae43832 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47552 = alloca %struct.ScmObj*, align 8
%fptrToInt47553 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43833 to i64
%ae43833 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47553)
store volatile %struct.ScmObj* %ae43833, %struct.ScmObj** %stackaddr$makeclosure47552, align 8
%args46789$ae43831$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47554 = alloca %struct.ScmObj*, align 8
%args46789$ae43831$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43833, %struct.ScmObj* %args46789$ae43831$0)
store volatile %struct.ScmObj* %args46789$ae43831$1, %struct.ScmObj** %stackaddr$prim47554, align 8
%stackaddr$prim47555 = alloca %struct.ScmObj*, align 8
%args46789$ae43831$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43832, %struct.ScmObj* %args46789$ae43831$1)
store volatile %struct.ScmObj* %args46789$ae43831$2, %struct.ScmObj** %stackaddr$prim47555, align 8
%clofunc47556 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43831)
musttail call tailcc void %clofunc47556(%struct.ScmObj* %ae43831, %struct.ScmObj* %args46789$ae43831$2)
ret void
}

define tailcc void @proc_clo$ae43831(%struct.ScmObj* %env$ae43831,%struct.ScmObj* %current_45args46688) {
%stackaddr$env-ref47557 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43831, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47557
%stackaddr$env-ref47558 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43831, i64 1)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47558
%stackaddr$prim47559 = alloca %struct.ScmObj*, align 8
%_95k40386 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46688)
store volatile %struct.ScmObj* %_95k40386, %struct.ScmObj** %stackaddr$prim47559, align 8
%stackaddr$prim47560 = alloca %struct.ScmObj*, align 8
%current_45args46689 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46688)
store volatile %struct.ScmObj* %current_45args46689, %struct.ScmObj** %stackaddr$prim47560, align 8
%stackaddr$prim47561 = alloca %struct.ScmObj*, align 8
%_37first40165 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46689)
store volatile %struct.ScmObj* %_37first40165, %struct.ScmObj** %stackaddr$prim47561, align 8
%stackaddr$makeclosure47562 = alloca %struct.ScmObj*, align 8
%fptrToInt47563 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43851 to i64
%ae43851 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47563)
store volatile %struct.ScmObj* %ae43851, %struct.ScmObj** %stackaddr$makeclosure47562, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43851, %struct.ScmObj* %_37foldl140107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43851, %struct.ScmObj* %_37map140154, i64 1)
%ae43852 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47564 = alloca %struct.ScmObj*, align 8
%fptrToInt47565 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43853 to i64
%ae43853 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47565)
store volatile %struct.ScmObj* %ae43853, %struct.ScmObj** %stackaddr$makeclosure47564, align 8
%args46784$ae43851$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47566 = alloca %struct.ScmObj*, align 8
%args46784$ae43851$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43853, %struct.ScmObj* %args46784$ae43851$0)
store volatile %struct.ScmObj* %args46784$ae43851$1, %struct.ScmObj** %stackaddr$prim47566, align 8
%stackaddr$prim47567 = alloca %struct.ScmObj*, align 8
%args46784$ae43851$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43852, %struct.ScmObj* %args46784$ae43851$1)
store volatile %struct.ScmObj* %args46784$ae43851$2, %struct.ScmObj** %stackaddr$prim47567, align 8
%clofunc47568 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43851)
musttail call tailcc void %clofunc47568(%struct.ScmObj* %ae43851, %struct.ScmObj* %args46784$ae43851$2)
ret void
}

define tailcc void @proc_clo$ae43851(%struct.ScmObj* %env$ae43851,%struct.ScmObj* %current_45args46691) {
%stackaddr$env-ref47569 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43851, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47569
%stackaddr$env-ref47570 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43851, i64 1)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47570
%stackaddr$prim47571 = alloca %struct.ScmObj*, align 8
%_95k40387 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46691)
store volatile %struct.ScmObj* %_95k40387, %struct.ScmObj** %stackaddr$prim47571, align 8
%stackaddr$prim47572 = alloca %struct.ScmObj*, align 8
%current_45args46692 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46691)
store volatile %struct.ScmObj* %current_45args46692, %struct.ScmObj** %stackaddr$prim47572, align 8
%stackaddr$prim47573 = alloca %struct.ScmObj*, align 8
%_37second40163 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46692)
store volatile %struct.ScmObj* %_37second40163, %struct.ScmObj** %stackaddr$prim47573, align 8
%stackaddr$makeclosure47574 = alloca %struct.ScmObj*, align 8
%fptrToInt47575 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43873 to i64
%ae43873 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47575)
store volatile %struct.ScmObj* %ae43873, %struct.ScmObj** %stackaddr$makeclosure47574, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43873, %struct.ScmObj* %_37foldl140107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43873, %struct.ScmObj* %_37map140154, i64 1)
%ae43874 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47576 = alloca %struct.ScmObj*, align 8
%fptrToInt47577 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43875 to i64
%ae43875 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47577)
store volatile %struct.ScmObj* %ae43875, %struct.ScmObj** %stackaddr$makeclosure47576, align 8
%args46779$ae43873$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47578 = alloca %struct.ScmObj*, align 8
%args46779$ae43873$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43875, %struct.ScmObj* %args46779$ae43873$0)
store volatile %struct.ScmObj* %args46779$ae43873$1, %struct.ScmObj** %stackaddr$prim47578, align 8
%stackaddr$prim47579 = alloca %struct.ScmObj*, align 8
%args46779$ae43873$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43874, %struct.ScmObj* %args46779$ae43873$1)
store volatile %struct.ScmObj* %args46779$ae43873$2, %struct.ScmObj** %stackaddr$prim47579, align 8
%clofunc47580 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43873)
musttail call tailcc void %clofunc47580(%struct.ScmObj* %ae43873, %struct.ScmObj* %args46779$ae43873$2)
ret void
}

define tailcc void @proc_clo$ae43873(%struct.ScmObj* %env$ae43873,%struct.ScmObj* %current_45args46694) {
%stackaddr$env-ref47581 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43873, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47581
%stackaddr$env-ref47582 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43873, i64 1)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47582
%stackaddr$prim47583 = alloca %struct.ScmObj*, align 8
%_95k40388 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46694)
store volatile %struct.ScmObj* %_95k40388, %struct.ScmObj** %stackaddr$prim47583, align 8
%stackaddr$prim47584 = alloca %struct.ScmObj*, align 8
%current_45args46695 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46694)
store volatile %struct.ScmObj* %current_45args46695, %struct.ScmObj** %stackaddr$prim47584, align 8
%stackaddr$prim47585 = alloca %struct.ScmObj*, align 8
%_37third40161 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46695)
store volatile %struct.ScmObj* %_37third40161, %struct.ScmObj** %stackaddr$prim47585, align 8
%stackaddr$makeclosure47586 = alloca %struct.ScmObj*, align 8
%fptrToInt47587 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43897 to i64
%ae43897 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47587)
store volatile %struct.ScmObj* %ae43897, %struct.ScmObj** %stackaddr$makeclosure47586, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43897, %struct.ScmObj* %_37foldl140107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43897, %struct.ScmObj* %_37map140154, i64 1)
%ae43898 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47588 = alloca %struct.ScmObj*, align 8
%fptrToInt47589 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43899 to i64
%ae43899 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47589)
store volatile %struct.ScmObj* %ae43899, %struct.ScmObj** %stackaddr$makeclosure47588, align 8
%args46774$ae43897$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47590 = alloca %struct.ScmObj*, align 8
%args46774$ae43897$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43899, %struct.ScmObj* %args46774$ae43897$0)
store volatile %struct.ScmObj* %args46774$ae43897$1, %struct.ScmObj** %stackaddr$prim47590, align 8
%stackaddr$prim47591 = alloca %struct.ScmObj*, align 8
%args46774$ae43897$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43898, %struct.ScmObj* %args46774$ae43897$1)
store volatile %struct.ScmObj* %args46774$ae43897$2, %struct.ScmObj** %stackaddr$prim47591, align 8
%clofunc47592 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43897)
musttail call tailcc void %clofunc47592(%struct.ScmObj* %ae43897, %struct.ScmObj* %args46774$ae43897$2)
ret void
}

define tailcc void @proc_clo$ae43897(%struct.ScmObj* %env$ae43897,%struct.ScmObj* %current_45args46697) {
%stackaddr$env-ref47593 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43897, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47593
%stackaddr$env-ref47594 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43897, i64 1)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47594
%stackaddr$prim47595 = alloca %struct.ScmObj*, align 8
%_95k40389 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46697)
store volatile %struct.ScmObj* %_95k40389, %struct.ScmObj** %stackaddr$prim47595, align 8
%stackaddr$prim47596 = alloca %struct.ScmObj*, align 8
%current_45args46698 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46697)
store volatile %struct.ScmObj* %current_45args46698, %struct.ScmObj** %stackaddr$prim47596, align 8
%stackaddr$prim47597 = alloca %struct.ScmObj*, align 8
%_37fourth40159 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46698)
store volatile %struct.ScmObj* %_37fourth40159, %struct.ScmObj** %stackaddr$prim47597, align 8
%stackaddr$makeclosure47598 = alloca %struct.ScmObj*, align 8
%fptrToInt47599 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43923 to i64
%ae43923 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47599)
store volatile %struct.ScmObj* %ae43923, %struct.ScmObj** %stackaddr$makeclosure47598, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43923, %struct.ScmObj* %_37foldl140107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43923, %struct.ScmObj* %_37map140154, i64 1)
%ae43924 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47600 = alloca %struct.ScmObj*, align 8
%fptrToInt47601 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43925 to i64
%ae43925 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47601)
store volatile %struct.ScmObj* %ae43925, %struct.ScmObj** %stackaddr$makeclosure47600, align 8
%args46769$ae43923$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47602 = alloca %struct.ScmObj*, align 8
%args46769$ae43923$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43925, %struct.ScmObj* %args46769$ae43923$0)
store volatile %struct.ScmObj* %args46769$ae43923$1, %struct.ScmObj** %stackaddr$prim47602, align 8
%stackaddr$prim47603 = alloca %struct.ScmObj*, align 8
%args46769$ae43923$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43924, %struct.ScmObj* %args46769$ae43923$1)
store volatile %struct.ScmObj* %args46769$ae43923$2, %struct.ScmObj** %stackaddr$prim47603, align 8
%clofunc47604 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43923)
musttail call tailcc void %clofunc47604(%struct.ScmObj* %ae43923, %struct.ScmObj* %args46769$ae43923$2)
ret void
}

define tailcc void @proc_clo$ae43923(%struct.ScmObj* %env$ae43923,%struct.ScmObj* %current_45args46700) {
%stackaddr$env-ref47605 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43923, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47605
%stackaddr$env-ref47606 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43923, i64 1)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47606
%stackaddr$prim47607 = alloca %struct.ScmObj*, align 8
%_95k40390 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46700)
store volatile %struct.ScmObj* %_95k40390, %struct.ScmObj** %stackaddr$prim47607, align 8
%stackaddr$prim47608 = alloca %struct.ScmObj*, align 8
%current_45args46701 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46700)
store volatile %struct.ScmObj* %current_45args46701, %struct.ScmObj** %stackaddr$prim47608, align 8
%stackaddr$prim47609 = alloca %struct.ScmObj*, align 8
%promise_6340220 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46701)
store volatile %struct.ScmObj* %promise_6340220, %struct.ScmObj** %stackaddr$prim47609, align 8
%stackaddr$makeclosure47610 = alloca %struct.ScmObj*, align 8
%fptrToInt47611 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44010 to i64
%ae44010 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47611)
store volatile %struct.ScmObj* %ae44010, %struct.ScmObj** %stackaddr$makeclosure47610, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44010, %struct.ScmObj* %_37foldl140107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44010, %struct.ScmObj* %_37map140154, i64 1)
%ae44011 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47612 = alloca %struct.ScmObj*, align 8
%fptrToInt47613 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44012 to i64
%ae44012 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47613)
store volatile %struct.ScmObj* %ae44012, %struct.ScmObj** %stackaddr$makeclosure47612, align 8
%args46762$ae44010$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47614 = alloca %struct.ScmObj*, align 8
%args46762$ae44010$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44012, %struct.ScmObj* %args46762$ae44010$0)
store volatile %struct.ScmObj* %args46762$ae44010$1, %struct.ScmObj** %stackaddr$prim47614, align 8
%stackaddr$prim47615 = alloca %struct.ScmObj*, align 8
%args46762$ae44010$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44011, %struct.ScmObj* %args46762$ae44010$1)
store volatile %struct.ScmObj* %args46762$ae44010$2, %struct.ScmObj** %stackaddr$prim47615, align 8
%clofunc47616 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44010)
musttail call tailcc void %clofunc47616(%struct.ScmObj* %ae44010, %struct.ScmObj* %args46762$ae44010$2)
ret void
}

define tailcc void @proc_clo$ae44010(%struct.ScmObj* %env$ae44010,%struct.ScmObj* %current_45args46703) {
%stackaddr$env-ref47617 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44010, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47617
%stackaddr$env-ref47618 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44010, i64 1)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47618
%stackaddr$prim47619 = alloca %struct.ScmObj*, align 8
%_95k40391 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46703)
store volatile %struct.ScmObj* %_95k40391, %struct.ScmObj** %stackaddr$prim47619, align 8
%stackaddr$prim47620 = alloca %struct.ScmObj*, align 8
%current_45args46704 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46703)
store volatile %struct.ScmObj* %current_45args46704, %struct.ScmObj** %stackaddr$prim47620, align 8
%stackaddr$prim47621 = alloca %struct.ScmObj*, align 8
%anf_45bind40345 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46704)
store volatile %struct.ScmObj* %anf_45bind40345, %struct.ScmObj** %stackaddr$prim47621, align 8
%stackaddr$makeclosure47622 = alloca %struct.ScmObj*, align 8
%fptrToInt47623 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44034 to i64
%ae44034 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47623)
store volatile %struct.ScmObj* %ae44034, %struct.ScmObj** %stackaddr$makeclosure47622, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44034, %struct.ScmObj* %_37foldl140107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44034, %struct.ScmObj* %_37map140154, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44034, %struct.ScmObj* %anf_45bind40345, i64 2)
%ae44035 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47624 = alloca %struct.ScmObj*, align 8
%fptrToInt47625 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44036 to i64
%ae44036 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47625)
store volatile %struct.ScmObj* %ae44036, %struct.ScmObj** %stackaddr$makeclosure47624, align 8
%args46760$ae44034$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47626 = alloca %struct.ScmObj*, align 8
%args46760$ae44034$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44036, %struct.ScmObj* %args46760$ae44034$0)
store volatile %struct.ScmObj* %args46760$ae44034$1, %struct.ScmObj** %stackaddr$prim47626, align 8
%stackaddr$prim47627 = alloca %struct.ScmObj*, align 8
%args46760$ae44034$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44035, %struct.ScmObj* %args46760$ae44034$1)
store volatile %struct.ScmObj* %args46760$ae44034$2, %struct.ScmObj** %stackaddr$prim47627, align 8
%clofunc47628 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44034)
musttail call tailcc void %clofunc47628(%struct.ScmObj* %ae44034, %struct.ScmObj* %args46760$ae44034$2)
ret void
}

define tailcc void @proc_clo$ae44034(%struct.ScmObj* %env$ae44034,%struct.ScmObj* %current_45args46706) {
%stackaddr$env-ref47629 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44034, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47629
%stackaddr$env-ref47630 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44034, i64 1)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47630
%stackaddr$env-ref47631 = alloca %struct.ScmObj*, align 8
%anf_45bind40345 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44034, i64 2)
store %struct.ScmObj* %anf_45bind40345, %struct.ScmObj** %stackaddr$env-ref47631
%stackaddr$prim47632 = alloca %struct.ScmObj*, align 8
%_95k40392 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46706)
store volatile %struct.ScmObj* %_95k40392, %struct.ScmObj** %stackaddr$prim47632, align 8
%stackaddr$prim47633 = alloca %struct.ScmObj*, align 8
%current_45args46707 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46706)
store volatile %struct.ScmObj* %current_45args46707, %struct.ScmObj** %stackaddr$prim47633, align 8
%stackaddr$prim47634 = alloca %struct.ScmObj*, align 8
%anf_45bind40346 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46707)
store volatile %struct.ScmObj* %anf_45bind40346, %struct.ScmObj** %stackaddr$prim47634, align 8
%stackaddr$makeclosure47635 = alloca %struct.ScmObj*, align 8
%fptrToInt47636 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44057 to i64
%ae44057 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47636)
store volatile %struct.ScmObj* %ae44057, %struct.ScmObj** %stackaddr$makeclosure47635, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44057, %struct.ScmObj* %_37foldl140107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44057, %struct.ScmObj* %_37map140154, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44057, %struct.ScmObj* %anf_45bind40345, i64 2)
%ae44058 = call %struct.ScmObj* @const_init_int(i64 1)
%ae44059 = call %struct.ScmObj* @const_init_int(i64 2)
%ae44060 = call %struct.ScmObj* @const_init_int(i64 3)
%ae44061 = call %struct.ScmObj* @const_init_int(i64 4)
%args46758$anf_45bind40346$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47637 = alloca %struct.ScmObj*, align 8
%args46758$anf_45bind40346$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44061, %struct.ScmObj* %args46758$anf_45bind40346$0)
store volatile %struct.ScmObj* %args46758$anf_45bind40346$1, %struct.ScmObj** %stackaddr$prim47637, align 8
%stackaddr$prim47638 = alloca %struct.ScmObj*, align 8
%args46758$anf_45bind40346$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44060, %struct.ScmObj* %args46758$anf_45bind40346$1)
store volatile %struct.ScmObj* %args46758$anf_45bind40346$2, %struct.ScmObj** %stackaddr$prim47638, align 8
%stackaddr$prim47639 = alloca %struct.ScmObj*, align 8
%args46758$anf_45bind40346$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44059, %struct.ScmObj* %args46758$anf_45bind40346$2)
store volatile %struct.ScmObj* %args46758$anf_45bind40346$3, %struct.ScmObj** %stackaddr$prim47639, align 8
%stackaddr$prim47640 = alloca %struct.ScmObj*, align 8
%args46758$anf_45bind40346$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44058, %struct.ScmObj* %args46758$anf_45bind40346$3)
store volatile %struct.ScmObj* %args46758$anf_45bind40346$4, %struct.ScmObj** %stackaddr$prim47640, align 8
%stackaddr$prim47641 = alloca %struct.ScmObj*, align 8
%args46758$anf_45bind40346$5 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44057, %struct.ScmObj* %args46758$anf_45bind40346$4)
store volatile %struct.ScmObj* %args46758$anf_45bind40346$5, %struct.ScmObj** %stackaddr$prim47641, align 8
%clofunc47642 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40346)
musttail call tailcc void %clofunc47642(%struct.ScmObj* %anf_45bind40346, %struct.ScmObj* %args46758$anf_45bind40346$5)
ret void
}

define tailcc void @proc_clo$ae44057(%struct.ScmObj* %env$ae44057,%struct.ScmObj* %current_45args46709) {
%stackaddr$env-ref47643 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44057, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47643
%stackaddr$env-ref47644 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44057, i64 1)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47644
%stackaddr$env-ref47645 = alloca %struct.ScmObj*, align 8
%anf_45bind40345 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44057, i64 2)
store %struct.ScmObj* %anf_45bind40345, %struct.ScmObj** %stackaddr$env-ref47645
%stackaddr$prim47646 = alloca %struct.ScmObj*, align 8
%_95k40393 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46709)
store volatile %struct.ScmObj* %_95k40393, %struct.ScmObj** %stackaddr$prim47646, align 8
%stackaddr$prim47647 = alloca %struct.ScmObj*, align 8
%current_45args46710 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46709)
store volatile %struct.ScmObj* %current_45args46710, %struct.ScmObj** %stackaddr$prim47647, align 8
%stackaddr$prim47648 = alloca %struct.ScmObj*, align 8
%anf_45bind40347 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46710)
store volatile %struct.ScmObj* %anf_45bind40347, %struct.ScmObj** %stackaddr$prim47648, align 8
%stackaddr$makeclosure47649 = alloca %struct.ScmObj*, align 8
%fptrToInt47650 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44087 to i64
%ae44087 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47650)
store volatile %struct.ScmObj* %ae44087, %struct.ScmObj** %stackaddr$makeclosure47649, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44087, %struct.ScmObj* %_37map140154, i64 0)
%ae44089 = call %struct.ScmObj* @const_init_int(i64 0)
%args46757$_37foldl140107$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47651 = alloca %struct.ScmObj*, align 8
%args46757$_37foldl140107$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40347, %struct.ScmObj* %args46757$_37foldl140107$0)
store volatile %struct.ScmObj* %args46757$_37foldl140107$1, %struct.ScmObj** %stackaddr$prim47651, align 8
%stackaddr$prim47652 = alloca %struct.ScmObj*, align 8
%args46757$_37foldl140107$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44089, %struct.ScmObj* %args46757$_37foldl140107$1)
store volatile %struct.ScmObj* %args46757$_37foldl140107$2, %struct.ScmObj** %stackaddr$prim47652, align 8
%stackaddr$prim47653 = alloca %struct.ScmObj*, align 8
%args46757$_37foldl140107$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40345, %struct.ScmObj* %args46757$_37foldl140107$2)
store volatile %struct.ScmObj* %args46757$_37foldl140107$3, %struct.ScmObj** %stackaddr$prim47653, align 8
%stackaddr$prim47654 = alloca %struct.ScmObj*, align 8
%args46757$_37foldl140107$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44087, %struct.ScmObj* %args46757$_37foldl140107$3)
store volatile %struct.ScmObj* %args46757$_37foldl140107$4, %struct.ScmObj** %stackaddr$prim47654, align 8
%clofunc47655 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140107)
musttail call tailcc void %clofunc47655(%struct.ScmObj* %_37foldl140107, %struct.ScmObj* %args46757$_37foldl140107$4)
ret void
}

define tailcc void @proc_clo$ae44087(%struct.ScmObj* %env$ae44087,%struct.ScmObj* %current_45args46712) {
%stackaddr$env-ref47656 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44087, i64 0)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47656
%stackaddr$prim47657 = alloca %struct.ScmObj*, align 8
%_95k40394 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46712)
store volatile %struct.ScmObj* %_95k40394, %struct.ScmObj** %stackaddr$prim47657, align 8
%stackaddr$prim47658 = alloca %struct.ScmObj*, align 8
%current_45args46713 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46712)
store volatile %struct.ScmObj* %current_45args46713, %struct.ScmObj** %stackaddr$prim47658, align 8
%stackaddr$prim47659 = alloca %struct.ScmObj*, align 8
%anf_45bind40348 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46713)
store volatile %struct.ScmObj* %anf_45bind40348, %struct.ScmObj** %stackaddr$prim47659, align 8
%stackaddr$makeclosure47660 = alloca %struct.ScmObj*, align 8
%fptrToInt47661 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44096 to i64
%ae44096 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47661)
store volatile %struct.ScmObj* %ae44096, %struct.ScmObj** %stackaddr$makeclosure47660, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44096, %struct.ScmObj* %anf_45bind40348, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44096, %struct.ScmObj* %_37map140154, i64 1)
%ae44097 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47662 = alloca %struct.ScmObj*, align 8
%fptrToInt47663 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44098 to i64
%ae44098 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47663)
store volatile %struct.ScmObj* %ae44098, %struct.ScmObj** %stackaddr$makeclosure47662, align 8
%args46756$ae44096$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47664 = alloca %struct.ScmObj*, align 8
%args46756$ae44096$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44098, %struct.ScmObj* %args46756$ae44096$0)
store volatile %struct.ScmObj* %args46756$ae44096$1, %struct.ScmObj** %stackaddr$prim47664, align 8
%stackaddr$prim47665 = alloca %struct.ScmObj*, align 8
%args46756$ae44096$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44097, %struct.ScmObj* %args46756$ae44096$1)
store volatile %struct.ScmObj* %args46756$ae44096$2, %struct.ScmObj** %stackaddr$prim47665, align 8
%clofunc47666 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44096)
musttail call tailcc void %clofunc47666(%struct.ScmObj* %ae44096, %struct.ScmObj* %args46756$ae44096$2)
ret void
}

define tailcc void @proc_clo$ae44096(%struct.ScmObj* %env$ae44096,%struct.ScmObj* %current_45args46715) {
%stackaddr$env-ref47667 = alloca %struct.ScmObj*, align 8
%anf_45bind40348 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44096, i64 0)
store %struct.ScmObj* %anf_45bind40348, %struct.ScmObj** %stackaddr$env-ref47667
%stackaddr$env-ref47668 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44096, i64 1)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47668
%stackaddr$prim47669 = alloca %struct.ScmObj*, align 8
%_95k40395 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46715)
store volatile %struct.ScmObj* %_95k40395, %struct.ScmObj** %stackaddr$prim47669, align 8
%stackaddr$prim47670 = alloca %struct.ScmObj*, align 8
%current_45args46716 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46715)
store volatile %struct.ScmObj* %current_45args46716, %struct.ScmObj** %stackaddr$prim47670, align 8
%stackaddr$prim47671 = alloca %struct.ScmObj*, align 8
%anf_45bind40349 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46716)
store volatile %struct.ScmObj* %anf_45bind40349, %struct.ScmObj** %stackaddr$prim47671, align 8
%stackaddr$makeclosure47672 = alloca %struct.ScmObj*, align 8
%fptrToInt47673 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44120 to i64
%ae44120 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47673)
store volatile %struct.ScmObj* %ae44120, %struct.ScmObj** %stackaddr$makeclosure47672, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44120, %struct.ScmObj* %anf_45bind40349, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44120, %struct.ScmObj* %anf_45bind40348, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44120, %struct.ScmObj* %_37map140154, i64 2)
%ae44121 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47674 = alloca %struct.ScmObj*, align 8
%fptrToInt47675 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44122 to i64
%ae44122 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47675)
store volatile %struct.ScmObj* %ae44122, %struct.ScmObj** %stackaddr$makeclosure47674, align 8
%args46754$ae44120$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47676 = alloca %struct.ScmObj*, align 8
%args46754$ae44120$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44122, %struct.ScmObj* %args46754$ae44120$0)
store volatile %struct.ScmObj* %args46754$ae44120$1, %struct.ScmObj** %stackaddr$prim47676, align 8
%stackaddr$prim47677 = alloca %struct.ScmObj*, align 8
%args46754$ae44120$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44121, %struct.ScmObj* %args46754$ae44120$1)
store volatile %struct.ScmObj* %args46754$ae44120$2, %struct.ScmObj** %stackaddr$prim47677, align 8
%clofunc47678 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44120)
musttail call tailcc void %clofunc47678(%struct.ScmObj* %ae44120, %struct.ScmObj* %args46754$ae44120$2)
ret void
}

define tailcc void @proc_clo$ae44120(%struct.ScmObj* %env$ae44120,%struct.ScmObj* %current_45args46718) {
%stackaddr$env-ref47679 = alloca %struct.ScmObj*, align 8
%anf_45bind40349 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44120, i64 0)
store %struct.ScmObj* %anf_45bind40349, %struct.ScmObj** %stackaddr$env-ref47679
%stackaddr$env-ref47680 = alloca %struct.ScmObj*, align 8
%anf_45bind40348 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44120, i64 1)
store %struct.ScmObj* %anf_45bind40348, %struct.ScmObj** %stackaddr$env-ref47680
%stackaddr$env-ref47681 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44120, i64 2)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47681
%stackaddr$prim47682 = alloca %struct.ScmObj*, align 8
%_95k40396 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46718)
store volatile %struct.ScmObj* %_95k40396, %struct.ScmObj** %stackaddr$prim47682, align 8
%stackaddr$prim47683 = alloca %struct.ScmObj*, align 8
%current_45args46719 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46718)
store volatile %struct.ScmObj* %current_45args46719, %struct.ScmObj** %stackaddr$prim47683, align 8
%stackaddr$prim47684 = alloca %struct.ScmObj*, align 8
%anf_45bind40350 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46719)
store volatile %struct.ScmObj* %anf_45bind40350, %struct.ScmObj** %stackaddr$prim47684, align 8
%stackaddr$makeclosure47685 = alloca %struct.ScmObj*, align 8
%fptrToInt47686 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44143 to i64
%ae44143 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47686)
store volatile %struct.ScmObj* %ae44143, %struct.ScmObj** %stackaddr$makeclosure47685, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44143, %struct.ScmObj* %anf_45bind40349, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44143, %struct.ScmObj* %anf_45bind40348, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44143, %struct.ScmObj* %_37map140154, i64 2)
%ae44144 = call %struct.ScmObj* @const_init_int(i64 2)
%args46752$anf_45bind40350$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47687 = alloca %struct.ScmObj*, align 8
%args46752$anf_45bind40350$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44144, %struct.ScmObj* %args46752$anf_45bind40350$0)
store volatile %struct.ScmObj* %args46752$anf_45bind40350$1, %struct.ScmObj** %stackaddr$prim47687, align 8
%stackaddr$prim47688 = alloca %struct.ScmObj*, align 8
%args46752$anf_45bind40350$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44143, %struct.ScmObj* %args46752$anf_45bind40350$1)
store volatile %struct.ScmObj* %args46752$anf_45bind40350$2, %struct.ScmObj** %stackaddr$prim47688, align 8
%clofunc47689 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40350)
musttail call tailcc void %clofunc47689(%struct.ScmObj* %anf_45bind40350, %struct.ScmObj* %args46752$anf_45bind40350$2)
ret void
}

define tailcc void @proc_clo$ae44143(%struct.ScmObj* %env$ae44143,%struct.ScmObj* %current_45args46721) {
%stackaddr$env-ref47690 = alloca %struct.ScmObj*, align 8
%anf_45bind40349 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44143, i64 0)
store %struct.ScmObj* %anf_45bind40349, %struct.ScmObj** %stackaddr$env-ref47690
%stackaddr$env-ref47691 = alloca %struct.ScmObj*, align 8
%anf_45bind40348 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44143, i64 1)
store %struct.ScmObj* %anf_45bind40348, %struct.ScmObj** %stackaddr$env-ref47691
%stackaddr$env-ref47692 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44143, i64 2)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47692
%stackaddr$prim47693 = alloca %struct.ScmObj*, align 8
%_95k40397 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46721)
store volatile %struct.ScmObj* %_95k40397, %struct.ScmObj** %stackaddr$prim47693, align 8
%stackaddr$prim47694 = alloca %struct.ScmObj*, align 8
%current_45args46722 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46721)
store volatile %struct.ScmObj* %current_45args46722, %struct.ScmObj** %stackaddr$prim47694, align 8
%stackaddr$prim47695 = alloca %struct.ScmObj*, align 8
%anf_45bind40351 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46722)
store volatile %struct.ScmObj* %anf_45bind40351, %struct.ScmObj** %stackaddr$prim47695, align 8
%stackaddr$makeclosure47696 = alloca %struct.ScmObj*, align 8
%fptrToInt47697 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44149 to i64
%ae44149 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47697)
store volatile %struct.ScmObj* %ae44149, %struct.ScmObj** %stackaddr$makeclosure47696, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44149, %struct.ScmObj* %anf_45bind40348, i64 0)
%args46751$_37map140154$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47698 = alloca %struct.ScmObj*, align 8
%args46751$_37map140154$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40351, %struct.ScmObj* %args46751$_37map140154$0)
store volatile %struct.ScmObj* %args46751$_37map140154$1, %struct.ScmObj** %stackaddr$prim47698, align 8
%stackaddr$prim47699 = alloca %struct.ScmObj*, align 8
%args46751$_37map140154$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40349, %struct.ScmObj* %args46751$_37map140154$1)
store volatile %struct.ScmObj* %args46751$_37map140154$2, %struct.ScmObj** %stackaddr$prim47699, align 8
%stackaddr$prim47700 = alloca %struct.ScmObj*, align 8
%args46751$_37map140154$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44149, %struct.ScmObj* %args46751$_37map140154$2)
store volatile %struct.ScmObj* %args46751$_37map140154$3, %struct.ScmObj** %stackaddr$prim47700, align 8
%clofunc47701 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140154)
musttail call tailcc void %clofunc47701(%struct.ScmObj* %_37map140154, %struct.ScmObj* %args46751$_37map140154$3)
ret void
}

define tailcc void @proc_clo$ae44149(%struct.ScmObj* %env$ae44149,%struct.ScmObj* %current_45args46724) {
%stackaddr$env-ref47702 = alloca %struct.ScmObj*, align 8
%anf_45bind40348 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44149, i64 0)
store %struct.ScmObj* %anf_45bind40348, %struct.ScmObj** %stackaddr$env-ref47702
%stackaddr$prim47703 = alloca %struct.ScmObj*, align 8
%_95k40398 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46724)
store volatile %struct.ScmObj* %_95k40398, %struct.ScmObj** %stackaddr$prim47703, align 8
%stackaddr$prim47704 = alloca %struct.ScmObj*, align 8
%current_45args46725 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46724)
store volatile %struct.ScmObj* %current_45args46725, %struct.ScmObj** %stackaddr$prim47704, align 8
%stackaddr$prim47705 = alloca %struct.ScmObj*, align 8
%anf_45bind40352 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46725)
store volatile %struct.ScmObj* %anf_45bind40352, %struct.ScmObj** %stackaddr$prim47705, align 8
%stackaddr$prim47706 = alloca %struct.ScmObj*, align 8
%anf_45bind40353 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40352)
store volatile %struct.ScmObj* %anf_45bind40353, %struct.ScmObj** %stackaddr$prim47706, align 8
%stackaddr$prim47707 = alloca %struct.ScmObj*, align 8
%anf_45bind40354 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind40348, %struct.ScmObj* %anf_45bind40353)
store volatile %struct.ScmObj* %anf_45bind40354, %struct.ScmObj** %stackaddr$prim47707, align 8
%stackaddr$makeclosure47708 = alloca %struct.ScmObj*, align 8
%fptrToInt47709 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44155 to i64
%ae44155 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47709)
store volatile %struct.ScmObj* %ae44155, %struct.ScmObj** %stackaddr$makeclosure47708, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44155, %struct.ScmObj* %anf_45bind40354, i64 0)
%ae44156 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47710 = alloca %struct.ScmObj*, align 8
%fptrToInt47711 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44157 to i64
%ae44157 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47711)
store volatile %struct.ScmObj* %ae44157, %struct.ScmObj** %stackaddr$makeclosure47710, align 8
%args46750$ae44155$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47712 = alloca %struct.ScmObj*, align 8
%args46750$ae44155$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44157, %struct.ScmObj* %args46750$ae44155$0)
store volatile %struct.ScmObj* %args46750$ae44155$1, %struct.ScmObj** %stackaddr$prim47712, align 8
%stackaddr$prim47713 = alloca %struct.ScmObj*, align 8
%args46750$ae44155$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44156, %struct.ScmObj* %args46750$ae44155$1)
store volatile %struct.ScmObj* %args46750$ae44155$2, %struct.ScmObj** %stackaddr$prim47713, align 8
%clofunc47714 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44155)
musttail call tailcc void %clofunc47714(%struct.ScmObj* %ae44155, %struct.ScmObj* %args46750$ae44155$2)
ret void
}

define tailcc void @proc_clo$ae44155(%struct.ScmObj* %env$ae44155,%struct.ScmObj* %current_45args46727) {
%stackaddr$env-ref47715 = alloca %struct.ScmObj*, align 8
%anf_45bind40354 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44155, i64 0)
store %struct.ScmObj* %anf_45bind40354, %struct.ScmObj** %stackaddr$env-ref47715
%stackaddr$prim47716 = alloca %struct.ScmObj*, align 8
%_95k40399 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46727)
store volatile %struct.ScmObj* %_95k40399, %struct.ScmObj** %stackaddr$prim47716, align 8
%stackaddr$prim47717 = alloca %struct.ScmObj*, align 8
%current_45args46728 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46727)
store volatile %struct.ScmObj* %current_45args46728, %struct.ScmObj** %stackaddr$prim47717, align 8
%stackaddr$prim47718 = alloca %struct.ScmObj*, align 8
%anf_45bind40355 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46728)
store volatile %struct.ScmObj* %anf_45bind40355, %struct.ScmObj** %stackaddr$prim47718, align 8
%stackaddr$makeclosure47719 = alloca %struct.ScmObj*, align 8
%fptrToInt47720 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44172 to i64
%ae44172 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47720)
store volatile %struct.ScmObj* %ae44172, %struct.ScmObj** %stackaddr$makeclosure47719, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44172, %struct.ScmObj* %anf_45bind40355, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44172, %struct.ScmObj* %anf_45bind40354, i64 1)
%ae44173 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47721 = alloca %struct.ScmObj*, align 8
%fptrToInt47722 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44174 to i64
%ae44174 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47722)
store volatile %struct.ScmObj* %ae44174, %struct.ScmObj** %stackaddr$makeclosure47721, align 8
%args46743$ae44172$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47723 = alloca %struct.ScmObj*, align 8
%args46743$ae44172$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44174, %struct.ScmObj* %args46743$ae44172$0)
store volatile %struct.ScmObj* %args46743$ae44172$1, %struct.ScmObj** %stackaddr$prim47723, align 8
%stackaddr$prim47724 = alloca %struct.ScmObj*, align 8
%args46743$ae44172$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44173, %struct.ScmObj* %args46743$ae44172$1)
store volatile %struct.ScmObj* %args46743$ae44172$2, %struct.ScmObj** %stackaddr$prim47724, align 8
%clofunc47725 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44172)
musttail call tailcc void %clofunc47725(%struct.ScmObj* %ae44172, %struct.ScmObj* %args46743$ae44172$2)
ret void
}

define tailcc void @proc_clo$ae44172(%struct.ScmObj* %env$ae44172,%struct.ScmObj* %current_45args46730) {
%stackaddr$env-ref47726 = alloca %struct.ScmObj*, align 8
%anf_45bind40355 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44172, i64 0)
store %struct.ScmObj* %anf_45bind40355, %struct.ScmObj** %stackaddr$env-ref47726
%stackaddr$env-ref47727 = alloca %struct.ScmObj*, align 8
%anf_45bind40354 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44172, i64 1)
store %struct.ScmObj* %anf_45bind40354, %struct.ScmObj** %stackaddr$env-ref47727
%stackaddr$prim47728 = alloca %struct.ScmObj*, align 8
%_95k40400 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46730)
store volatile %struct.ScmObj* %_95k40400, %struct.ScmObj** %stackaddr$prim47728, align 8
%stackaddr$prim47729 = alloca %struct.ScmObj*, align 8
%current_45args46731 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46730)
store volatile %struct.ScmObj* %current_45args46731, %struct.ScmObj** %stackaddr$prim47729, align 8
%stackaddr$prim47730 = alloca %struct.ScmObj*, align 8
%anf_45bind40356 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46731)
store volatile %struct.ScmObj* %anf_45bind40356, %struct.ScmObj** %stackaddr$prim47730, align 8
%stackaddr$makeclosure47731 = alloca %struct.ScmObj*, align 8
%fptrToInt47732 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44197 to i64
%ae44197 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47732)
store volatile %struct.ScmObj* %ae44197, %struct.ScmObj** %stackaddr$makeclosure47731, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44197, %struct.ScmObj* %anf_45bind40354, i64 0)
%ae44198 = call %struct.ScmObj* @const_init_int(i64 2)
%ae44200 = call %struct.ScmObj* @const_init_int(i64 3)
%args46741$anf_45bind40355$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47733 = alloca %struct.ScmObj*, align 8
%args46741$anf_45bind40355$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44200, %struct.ScmObj* %args46741$anf_45bind40355$0)
store volatile %struct.ScmObj* %args46741$anf_45bind40355$1, %struct.ScmObj** %stackaddr$prim47733, align 8
%stackaddr$prim47734 = alloca %struct.ScmObj*, align 8
%args46741$anf_45bind40355$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40356, %struct.ScmObj* %args46741$anf_45bind40355$1)
store volatile %struct.ScmObj* %args46741$anf_45bind40355$2, %struct.ScmObj** %stackaddr$prim47734, align 8
%stackaddr$prim47735 = alloca %struct.ScmObj*, align 8
%args46741$anf_45bind40355$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44198, %struct.ScmObj* %args46741$anf_45bind40355$2)
store volatile %struct.ScmObj* %args46741$anf_45bind40355$3, %struct.ScmObj** %stackaddr$prim47735, align 8
%stackaddr$prim47736 = alloca %struct.ScmObj*, align 8
%args46741$anf_45bind40355$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44197, %struct.ScmObj* %args46741$anf_45bind40355$3)
store volatile %struct.ScmObj* %args46741$anf_45bind40355$4, %struct.ScmObj** %stackaddr$prim47736, align 8
%clofunc47737 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40355)
musttail call tailcc void %clofunc47737(%struct.ScmObj* %anf_45bind40355, %struct.ScmObj* %args46741$anf_45bind40355$4)
ret void
}

define tailcc void @proc_clo$ae44197(%struct.ScmObj* %env$ae44197,%struct.ScmObj* %current_45args46733) {
%stackaddr$env-ref47738 = alloca %struct.ScmObj*, align 8
%anf_45bind40354 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44197, i64 0)
store %struct.ScmObj* %anf_45bind40354, %struct.ScmObj** %stackaddr$env-ref47738
%stackaddr$prim47739 = alloca %struct.ScmObj*, align 8
%_95k40401 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46733)
store volatile %struct.ScmObj* %_95k40401, %struct.ScmObj** %stackaddr$prim47739, align 8
%stackaddr$prim47740 = alloca %struct.ScmObj*, align 8
%current_45args46734 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46733)
store volatile %struct.ScmObj* %current_45args46734, %struct.ScmObj** %stackaddr$prim47740, align 8
%stackaddr$prim47741 = alloca %struct.ScmObj*, align 8
%anf_45bind40357 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46734)
store volatile %struct.ScmObj* %anf_45bind40357, %struct.ScmObj** %stackaddr$prim47741, align 8
%stackaddr$prim47742 = alloca %struct.ScmObj*, align 8
%cpsprim40402 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind40354, %struct.ScmObj* %anf_45bind40357)
store volatile %struct.ScmObj* %cpsprim40402, %struct.ScmObj** %stackaddr$prim47742, align 8
%stackaddr$makeclosure47743 = alloca %struct.ScmObj*, align 8
%fptrToInt47744 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44213 to i64
%ae44213 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47744)
store volatile %struct.ScmObj* %ae44213, %struct.ScmObj** %stackaddr$makeclosure47743, align 8
%ae44214 = call %struct.ScmObj* @const_init_int(i64 0)
%args46740$ae44213$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47745 = alloca %struct.ScmObj*, align 8
%args46740$ae44213$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40402, %struct.ScmObj* %args46740$ae44213$0)
store volatile %struct.ScmObj* %args46740$ae44213$1, %struct.ScmObj** %stackaddr$prim47745, align 8
%stackaddr$prim47746 = alloca %struct.ScmObj*, align 8
%args46740$ae44213$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44214, %struct.ScmObj* %args46740$ae44213$1)
store volatile %struct.ScmObj* %args46740$ae44213$2, %struct.ScmObj** %stackaddr$prim47746, align 8
%clofunc47747 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44213)
musttail call tailcc void %clofunc47747(%struct.ScmObj* %ae44213, %struct.ScmObj* %args46740$ae44213$2)
ret void
}

define tailcc void @proc_clo$ae44213(%struct.ScmObj* %env$ae44213,%struct.ScmObj* %current_45args46736) {
%stackaddr$prim47748 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46736)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim47748, align 8
%stackaddr$prim47749 = alloca %struct.ScmObj*, align 8
%current_45args46737 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46736)
store volatile %struct.ScmObj* %current_45args46737, %struct.ScmObj** %stackaddr$prim47749, align 8
%stackaddr$prim47750 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46737)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim47750, align 8
%stackaddr$prim47751 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim47751, align 8
%args46739$k$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47752 = alloca %struct.ScmObj*, align 8
%args46739$k$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %args46739$k$0)
store volatile %struct.ScmObj* %args46739$k$1, %struct.ScmObj** %stackaddr$prim47752, align 8
%clofunc47753 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc47753(%struct.ScmObj* %k, %struct.ScmObj* %args46739$k$1)
ret void
}

define tailcc void @proc_clo$ae44174(%struct.ScmObj* %env$ae44174,%struct.ScmObj* %args4022940403) {
%stackaddr$prim47754 = alloca %struct.ScmObj*, align 8
%k40404 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4022940403)
store volatile %struct.ScmObj* %k40404, %struct.ScmObj** %stackaddr$prim47754, align 8
%stackaddr$prim47755 = alloca %struct.ScmObj*, align 8
%args40229 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4022940403)
store volatile %struct.ScmObj* %args40229, %struct.ScmObj** %stackaddr$prim47755, align 8
%stackaddr$applyprim47756 = alloca %struct.ScmObj*, align 8
%cpsaprim40405 = call %struct.ScmObj* @applyprim__42(%struct.ScmObj* %args40229)
store volatile %struct.ScmObj* %cpsaprim40405, %struct.ScmObj** %stackaddr$applyprim47756, align 8
%ae44179 = call %struct.ScmObj* @const_init_int(i64 0)
%args46742$k40404$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47757 = alloca %struct.ScmObj*, align 8
%args46742$k40404$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsaprim40405, %struct.ScmObj* %args46742$k40404$0)
store volatile %struct.ScmObj* %args46742$k40404$1, %struct.ScmObj** %stackaddr$prim47757, align 8
%stackaddr$prim47758 = alloca %struct.ScmObj*, align 8
%args46742$k40404$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44179, %struct.ScmObj* %args46742$k40404$1)
store volatile %struct.ScmObj* %args46742$k40404$2, %struct.ScmObj** %stackaddr$prim47758, align 8
%clofunc47759 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40404)
musttail call tailcc void %clofunc47759(%struct.ScmObj* %k40404, %struct.ScmObj* %args46742$k40404$2)
ret void
}

define tailcc void @proc_clo$ae44157(%struct.ScmObj* %env$ae44157,%struct.ScmObj* %current_45args46744) {
%stackaddr$prim47760 = alloca %struct.ScmObj*, align 8
%k40406 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46744)
store volatile %struct.ScmObj* %k40406, %struct.ScmObj** %stackaddr$prim47760, align 8
%stackaddr$prim47761 = alloca %struct.ScmObj*, align 8
%current_45args46745 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46744)
store volatile %struct.ScmObj* %current_45args46745, %struct.ScmObj** %stackaddr$prim47761, align 8
%stackaddr$prim47762 = alloca %struct.ScmObj*, align 8
%x40228 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46745)
store volatile %struct.ScmObj* %x40228, %struct.ScmObj** %stackaddr$prim47762, align 8
%stackaddr$prim47763 = alloca %struct.ScmObj*, align 8
%current_45args46746 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46745)
store volatile %struct.ScmObj* %current_45args46746, %struct.ScmObj** %stackaddr$prim47763, align 8
%stackaddr$prim47764 = alloca %struct.ScmObj*, align 8
%f40227 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46746)
store volatile %struct.ScmObj* %f40227, %struct.ScmObj** %stackaddr$prim47764, align 8
%stackaddr$prim47765 = alloca %struct.ScmObj*, align 8
%current_45args46747 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46746)
store volatile %struct.ScmObj* %current_45args46747, %struct.ScmObj** %stackaddr$prim47765, align 8
%stackaddr$prim47766 = alloca %struct.ScmObj*, align 8
%y40226 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46747)
store volatile %struct.ScmObj* %y40226, %struct.ScmObj** %stackaddr$prim47766, align 8
%args46749$f40227$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47767 = alloca %struct.ScmObj*, align 8
%args46749$f40227$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y40226, %struct.ScmObj* %args46749$f40227$0)
store volatile %struct.ScmObj* %args46749$f40227$1, %struct.ScmObj** %stackaddr$prim47767, align 8
%stackaddr$prim47768 = alloca %struct.ScmObj*, align 8
%args46749$f40227$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x40228, %struct.ScmObj* %args46749$f40227$1)
store volatile %struct.ScmObj* %args46749$f40227$2, %struct.ScmObj** %stackaddr$prim47768, align 8
%stackaddr$prim47769 = alloca %struct.ScmObj*, align 8
%args46749$f40227$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40406, %struct.ScmObj* %args46749$f40227$2)
store volatile %struct.ScmObj* %args46749$f40227$3, %struct.ScmObj** %stackaddr$prim47769, align 8
%clofunc47770 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40227)
musttail call tailcc void %clofunc47770(%struct.ScmObj* %f40227, %struct.ScmObj* %args46749$f40227$3)
ret void
}

define tailcc void @proc_clo$ae44122(%struct.ScmObj* %env$ae44122,%struct.ScmObj* %lst4022540407) {
%stackaddr$prim47771 = alloca %struct.ScmObj*, align 8
%k40408 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4022540407)
store volatile %struct.ScmObj* %k40408, %struct.ScmObj** %stackaddr$prim47771, align 8
%stackaddr$prim47772 = alloca %struct.ScmObj*, align 8
%lst40225 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4022540407)
store volatile %struct.ScmObj* %lst40225, %struct.ScmObj** %stackaddr$prim47772, align 8
%ae44126 = call %struct.ScmObj* @const_init_int(i64 0)
%args46753$k40408$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47773 = alloca %struct.ScmObj*, align 8
%args46753$k40408$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40225, %struct.ScmObj* %args46753$k40408$0)
store volatile %struct.ScmObj* %args46753$k40408$1, %struct.ScmObj** %stackaddr$prim47773, align 8
%stackaddr$prim47774 = alloca %struct.ScmObj*, align 8
%args46753$k40408$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44126, %struct.ScmObj* %args46753$k40408$1)
store volatile %struct.ScmObj* %args46753$k40408$2, %struct.ScmObj** %stackaddr$prim47774, align 8
%clofunc47775 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40408)
musttail call tailcc void %clofunc47775(%struct.ScmObj* %k40408, %struct.ScmObj* %args46753$k40408$2)
ret void
}

define tailcc void @proc_clo$ae44098(%struct.ScmObj* %env$ae44098,%struct.ScmObj* %args4022440409) {
%stackaddr$prim47776 = alloca %struct.ScmObj*, align 8
%k40410 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4022440409)
store volatile %struct.ScmObj* %k40410, %struct.ScmObj** %stackaddr$prim47776, align 8
%stackaddr$prim47777 = alloca %struct.ScmObj*, align 8
%args40224 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4022440409)
store volatile %struct.ScmObj* %args40224, %struct.ScmObj** %stackaddr$prim47777, align 8
%stackaddr$applyprim47778 = alloca %struct.ScmObj*, align 8
%cpsaprim40411 = call %struct.ScmObj* @applyprim__45(%struct.ScmObj* %args40224)
store volatile %struct.ScmObj* %cpsaprim40411, %struct.ScmObj** %stackaddr$applyprim47778, align 8
%ae44103 = call %struct.ScmObj* @const_init_int(i64 0)
%args46755$k40410$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47779 = alloca %struct.ScmObj*, align 8
%args46755$k40410$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsaprim40411, %struct.ScmObj* %args46755$k40410$0)
store volatile %struct.ScmObj* %args46755$k40410$1, %struct.ScmObj** %stackaddr$prim47779, align 8
%stackaddr$prim47780 = alloca %struct.ScmObj*, align 8
%args46755$k40410$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44103, %struct.ScmObj* %args46755$k40410$1)
store volatile %struct.ScmObj* %args46755$k40410$2, %struct.ScmObj** %stackaddr$prim47780, align 8
%clofunc47781 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40410)
musttail call tailcc void %clofunc47781(%struct.ScmObj* %k40410, %struct.ScmObj* %args46755$k40410$2)
ret void
}

define tailcc void @proc_clo$ae44036(%struct.ScmObj* %env$ae44036,%struct.ScmObj* %lst4022340412) {
%stackaddr$prim47782 = alloca %struct.ScmObj*, align 8
%k40413 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4022340412)
store volatile %struct.ScmObj* %k40413, %struct.ScmObj** %stackaddr$prim47782, align 8
%stackaddr$prim47783 = alloca %struct.ScmObj*, align 8
%lst40223 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4022340412)
store volatile %struct.ScmObj* %lst40223, %struct.ScmObj** %stackaddr$prim47783, align 8
%ae44040 = call %struct.ScmObj* @const_init_int(i64 0)
%args46759$k40413$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47784 = alloca %struct.ScmObj*, align 8
%args46759$k40413$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40223, %struct.ScmObj* %args46759$k40413$0)
store volatile %struct.ScmObj* %args46759$k40413$1, %struct.ScmObj** %stackaddr$prim47784, align 8
%stackaddr$prim47785 = alloca %struct.ScmObj*, align 8
%args46759$k40413$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44040, %struct.ScmObj* %args46759$k40413$1)
store volatile %struct.ScmObj* %args46759$k40413$2, %struct.ScmObj** %stackaddr$prim47785, align 8
%clofunc47786 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40413)
musttail call tailcc void %clofunc47786(%struct.ScmObj* %k40413, %struct.ScmObj* %args46759$k40413$2)
ret void
}

define tailcc void @proc_clo$ae44012(%struct.ScmObj* %env$ae44012,%struct.ScmObj* %args4022240414) {
%stackaddr$prim47787 = alloca %struct.ScmObj*, align 8
%k40415 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4022240414)
store volatile %struct.ScmObj* %k40415, %struct.ScmObj** %stackaddr$prim47787, align 8
%stackaddr$prim47788 = alloca %struct.ScmObj*, align 8
%args40222 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4022240414)
store volatile %struct.ScmObj* %args40222, %struct.ScmObj** %stackaddr$prim47788, align 8
%stackaddr$applyprim47789 = alloca %struct.ScmObj*, align 8
%cpsaprim40416 = call %struct.ScmObj* @applyprim__43(%struct.ScmObj* %args40222)
store volatile %struct.ScmObj* %cpsaprim40416, %struct.ScmObj** %stackaddr$applyprim47789, align 8
%ae44017 = call %struct.ScmObj* @const_init_int(i64 0)
%args46761$k40415$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47790 = alloca %struct.ScmObj*, align 8
%args46761$k40415$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsaprim40416, %struct.ScmObj* %args46761$k40415$0)
store volatile %struct.ScmObj* %args46761$k40415$1, %struct.ScmObj** %stackaddr$prim47790, align 8
%stackaddr$prim47791 = alloca %struct.ScmObj*, align 8
%args46761$k40415$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44017, %struct.ScmObj* %args46761$k40415$1)
store volatile %struct.ScmObj* %args46761$k40415$2, %struct.ScmObj** %stackaddr$prim47791, align 8
%clofunc47792 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40415)
musttail call tailcc void %clofunc47792(%struct.ScmObj* %k40415, %struct.ScmObj* %args46761$k40415$2)
ret void
}

define tailcc void @proc_clo$ae43925(%struct.ScmObj* %env$ae43925,%struct.ScmObj* %current_45args46763) {
%stackaddr$prim47793 = alloca %struct.ScmObj*, align 8
%k40417 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46763)
store volatile %struct.ScmObj* %k40417, %struct.ScmObj** %stackaddr$prim47793, align 8
%stackaddr$prim47794 = alloca %struct.ScmObj*, align 8
%current_45args46764 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46763)
store volatile %struct.ScmObj* %current_45args46764, %struct.ScmObj** %stackaddr$prim47794, align 8
%stackaddr$prim47795 = alloca %struct.ScmObj*, align 8
%thunk40221 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46764)
store volatile %struct.ScmObj* %thunk40221, %struct.ScmObj** %stackaddr$prim47795, align 8
%stackaddr$prim47796 = alloca %struct.ScmObj*, align 8
%anf_45bind40341 = call %struct.ScmObj* @prim_vector_63(%struct.ScmObj* %thunk40221)
store volatile %struct.ScmObj* %anf_45bind40341, %struct.ScmObj** %stackaddr$prim47796, align 8
%truthy$cmp47797 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40341)
%cmp$cmp47797 = icmp eq i64 %truthy$cmp47797, 1
br i1 %cmp$cmp47797, label %truebranch$cmp47797, label %falsebranch$cmp47797
truebranch$cmp47797:
%stackaddr$prim47798 = alloca %struct.ScmObj*, align 8
%anf_45bind40342 = call %struct.ScmObj* @prim_vector_45length(%struct.ScmObj* %thunk40221)
store volatile %struct.ScmObj* %anf_45bind40342, %struct.ScmObj** %stackaddr$prim47798, align 8
%ae43930 = call %struct.ScmObj* @const_init_int(i64 3)
%stackaddr$prim47799 = alloca %struct.ScmObj*, align 8
%anf_45bind40343 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind40342, %struct.ScmObj* %ae43930)
store volatile %struct.ScmObj* %anf_45bind40343, %struct.ScmObj** %stackaddr$prim47799, align 8
%truthy$cmp47800 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40343)
%cmp$cmp47800 = icmp eq i64 %truthy$cmp47800, 1
br i1 %cmp$cmp47800, label %truebranch$cmp47800, label %falsebranch$cmp47800
truebranch$cmp47800:
%ae43933 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47801 = alloca %struct.ScmObj*, align 8
%anf_45bind40344 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk40221, %struct.ScmObj* %ae43933)
store volatile %struct.ScmObj* %anf_45bind40344, %struct.ScmObj** %stackaddr$prim47801, align 8
%ae43935 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @global$sym$ae4393547802, i32 0, i32 0))
%stackaddr$prim47803 = alloca %struct.ScmObj*, align 8
%cpsprim40418 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind40344, %struct.ScmObj* %ae43935)
store volatile %struct.ScmObj* %cpsprim40418, %struct.ScmObj** %stackaddr$prim47803, align 8
%ae43937 = call %struct.ScmObj* @const_init_int(i64 0)
%args46766$k40417$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47804 = alloca %struct.ScmObj*, align 8
%args46766$k40417$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40418, %struct.ScmObj* %args46766$k40417$0)
store volatile %struct.ScmObj* %args46766$k40417$1, %struct.ScmObj** %stackaddr$prim47804, align 8
%stackaddr$prim47805 = alloca %struct.ScmObj*, align 8
%args46766$k40417$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43937, %struct.ScmObj* %args46766$k40417$1)
store volatile %struct.ScmObj* %args46766$k40417$2, %struct.ScmObj** %stackaddr$prim47805, align 8
%clofunc47806 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40417)
musttail call tailcc void %clofunc47806(%struct.ScmObj* %k40417, %struct.ScmObj* %args46766$k40417$2)
ret void
falsebranch$cmp47800:
%ae43955 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43956 = call %struct.ScmObj* @const_init_false()
%args46767$k40417$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47807 = alloca %struct.ScmObj*, align 8
%args46767$k40417$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43956, %struct.ScmObj* %args46767$k40417$0)
store volatile %struct.ScmObj* %args46767$k40417$1, %struct.ScmObj** %stackaddr$prim47807, align 8
%stackaddr$prim47808 = alloca %struct.ScmObj*, align 8
%args46767$k40417$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43955, %struct.ScmObj* %args46767$k40417$1)
store volatile %struct.ScmObj* %args46767$k40417$2, %struct.ScmObj** %stackaddr$prim47808, align 8
%clofunc47809 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40417)
musttail call tailcc void %clofunc47809(%struct.ScmObj* %k40417, %struct.ScmObj* %args46767$k40417$2)
ret void
falsebranch$cmp47797:
%ae43977 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43978 = call %struct.ScmObj* @const_init_false()
%args46768$k40417$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47810 = alloca %struct.ScmObj*, align 8
%args46768$k40417$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43978, %struct.ScmObj* %args46768$k40417$0)
store volatile %struct.ScmObj* %args46768$k40417$1, %struct.ScmObj** %stackaddr$prim47810, align 8
%stackaddr$prim47811 = alloca %struct.ScmObj*, align 8
%args46768$k40417$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43977, %struct.ScmObj* %args46768$k40417$1)
store volatile %struct.ScmObj* %args46768$k40417$2, %struct.ScmObj** %stackaddr$prim47811, align 8
%clofunc47812 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40417)
musttail call tailcc void %clofunc47812(%struct.ScmObj* %k40417, %struct.ScmObj* %args46768$k40417$2)
ret void
}

define tailcc void @proc_clo$ae43899(%struct.ScmObj* %env$ae43899,%struct.ScmObj* %current_45args46770) {
%stackaddr$prim47813 = alloca %struct.ScmObj*, align 8
%k40419 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46770)
store volatile %struct.ScmObj* %k40419, %struct.ScmObj** %stackaddr$prim47813, align 8
%stackaddr$prim47814 = alloca %struct.ScmObj*, align 8
%current_45args46771 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46770)
store volatile %struct.ScmObj* %current_45args46771, %struct.ScmObj** %stackaddr$prim47814, align 8
%stackaddr$prim47815 = alloca %struct.ScmObj*, align 8
%x40160 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46771)
store volatile %struct.ScmObj* %x40160, %struct.ScmObj** %stackaddr$prim47815, align 8
%stackaddr$prim47816 = alloca %struct.ScmObj*, align 8
%anf_45bind40338 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40160)
store volatile %struct.ScmObj* %anf_45bind40338, %struct.ScmObj** %stackaddr$prim47816, align 8
%stackaddr$prim47817 = alloca %struct.ScmObj*, align 8
%anf_45bind40339 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40338)
store volatile %struct.ScmObj* %anf_45bind40339, %struct.ScmObj** %stackaddr$prim47817, align 8
%stackaddr$prim47818 = alloca %struct.ScmObj*, align 8
%anf_45bind40340 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40339)
store volatile %struct.ScmObj* %anf_45bind40340, %struct.ScmObj** %stackaddr$prim47818, align 8
%stackaddr$prim47819 = alloca %struct.ScmObj*, align 8
%cpsprim40420 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40340)
store volatile %struct.ScmObj* %cpsprim40420, %struct.ScmObj** %stackaddr$prim47819, align 8
%ae43905 = call %struct.ScmObj* @const_init_int(i64 0)
%args46773$k40419$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47820 = alloca %struct.ScmObj*, align 8
%args46773$k40419$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40420, %struct.ScmObj* %args46773$k40419$0)
store volatile %struct.ScmObj* %args46773$k40419$1, %struct.ScmObj** %stackaddr$prim47820, align 8
%stackaddr$prim47821 = alloca %struct.ScmObj*, align 8
%args46773$k40419$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43905, %struct.ScmObj* %args46773$k40419$1)
store volatile %struct.ScmObj* %args46773$k40419$2, %struct.ScmObj** %stackaddr$prim47821, align 8
%clofunc47822 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40419)
musttail call tailcc void %clofunc47822(%struct.ScmObj* %k40419, %struct.ScmObj* %args46773$k40419$2)
ret void
}

define tailcc void @proc_clo$ae43875(%struct.ScmObj* %env$ae43875,%struct.ScmObj* %current_45args46775) {
%stackaddr$prim47823 = alloca %struct.ScmObj*, align 8
%k40421 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46775)
store volatile %struct.ScmObj* %k40421, %struct.ScmObj** %stackaddr$prim47823, align 8
%stackaddr$prim47824 = alloca %struct.ScmObj*, align 8
%current_45args46776 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46775)
store volatile %struct.ScmObj* %current_45args46776, %struct.ScmObj** %stackaddr$prim47824, align 8
%stackaddr$prim47825 = alloca %struct.ScmObj*, align 8
%x40162 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46776)
store volatile %struct.ScmObj* %x40162, %struct.ScmObj** %stackaddr$prim47825, align 8
%stackaddr$prim47826 = alloca %struct.ScmObj*, align 8
%anf_45bind40336 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40162)
store volatile %struct.ScmObj* %anf_45bind40336, %struct.ScmObj** %stackaddr$prim47826, align 8
%stackaddr$prim47827 = alloca %struct.ScmObj*, align 8
%anf_45bind40337 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40336)
store volatile %struct.ScmObj* %anf_45bind40337, %struct.ScmObj** %stackaddr$prim47827, align 8
%stackaddr$prim47828 = alloca %struct.ScmObj*, align 8
%cpsprim40422 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40337)
store volatile %struct.ScmObj* %cpsprim40422, %struct.ScmObj** %stackaddr$prim47828, align 8
%ae43880 = call %struct.ScmObj* @const_init_int(i64 0)
%args46778$k40421$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47829 = alloca %struct.ScmObj*, align 8
%args46778$k40421$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40422, %struct.ScmObj* %args46778$k40421$0)
store volatile %struct.ScmObj* %args46778$k40421$1, %struct.ScmObj** %stackaddr$prim47829, align 8
%stackaddr$prim47830 = alloca %struct.ScmObj*, align 8
%args46778$k40421$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43880, %struct.ScmObj* %args46778$k40421$1)
store volatile %struct.ScmObj* %args46778$k40421$2, %struct.ScmObj** %stackaddr$prim47830, align 8
%clofunc47831 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40421)
musttail call tailcc void %clofunc47831(%struct.ScmObj* %k40421, %struct.ScmObj* %args46778$k40421$2)
ret void
}

define tailcc void @proc_clo$ae43853(%struct.ScmObj* %env$ae43853,%struct.ScmObj* %current_45args46780) {
%stackaddr$prim47832 = alloca %struct.ScmObj*, align 8
%k40423 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46780)
store volatile %struct.ScmObj* %k40423, %struct.ScmObj** %stackaddr$prim47832, align 8
%stackaddr$prim47833 = alloca %struct.ScmObj*, align 8
%current_45args46781 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46780)
store volatile %struct.ScmObj* %current_45args46781, %struct.ScmObj** %stackaddr$prim47833, align 8
%stackaddr$prim47834 = alloca %struct.ScmObj*, align 8
%x40164 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46781)
store volatile %struct.ScmObj* %x40164, %struct.ScmObj** %stackaddr$prim47834, align 8
%stackaddr$prim47835 = alloca %struct.ScmObj*, align 8
%anf_45bind40335 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40164)
store volatile %struct.ScmObj* %anf_45bind40335, %struct.ScmObj** %stackaddr$prim47835, align 8
%stackaddr$prim47836 = alloca %struct.ScmObj*, align 8
%cpsprim40424 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40335)
store volatile %struct.ScmObj* %cpsprim40424, %struct.ScmObj** %stackaddr$prim47836, align 8
%ae43857 = call %struct.ScmObj* @const_init_int(i64 0)
%args46783$k40423$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47837 = alloca %struct.ScmObj*, align 8
%args46783$k40423$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40424, %struct.ScmObj* %args46783$k40423$0)
store volatile %struct.ScmObj* %args46783$k40423$1, %struct.ScmObj** %stackaddr$prim47837, align 8
%stackaddr$prim47838 = alloca %struct.ScmObj*, align 8
%args46783$k40423$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43857, %struct.ScmObj* %args46783$k40423$1)
store volatile %struct.ScmObj* %args46783$k40423$2, %struct.ScmObj** %stackaddr$prim47838, align 8
%clofunc47839 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40423)
musttail call tailcc void %clofunc47839(%struct.ScmObj* %k40423, %struct.ScmObj* %args46783$k40423$2)
ret void
}

define tailcc void @proc_clo$ae43833(%struct.ScmObj* %env$ae43833,%struct.ScmObj* %current_45args46785) {
%stackaddr$prim47840 = alloca %struct.ScmObj*, align 8
%k40425 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46785)
store volatile %struct.ScmObj* %k40425, %struct.ScmObj** %stackaddr$prim47840, align 8
%stackaddr$prim47841 = alloca %struct.ScmObj*, align 8
%current_45args46786 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46785)
store volatile %struct.ScmObj* %current_45args46786, %struct.ScmObj** %stackaddr$prim47841, align 8
%stackaddr$prim47842 = alloca %struct.ScmObj*, align 8
%x40166 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46786)
store volatile %struct.ScmObj* %x40166, %struct.ScmObj** %stackaddr$prim47842, align 8
%stackaddr$prim47843 = alloca %struct.ScmObj*, align 8
%cpsprim40426 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40166)
store volatile %struct.ScmObj* %cpsprim40426, %struct.ScmObj** %stackaddr$prim47843, align 8
%ae43836 = call %struct.ScmObj* @const_init_int(i64 0)
%args46788$k40425$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47844 = alloca %struct.ScmObj*, align 8
%args46788$k40425$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40426, %struct.ScmObj* %args46788$k40425$0)
store volatile %struct.ScmObj* %args46788$k40425$1, %struct.ScmObj** %stackaddr$prim47844, align 8
%stackaddr$prim47845 = alloca %struct.ScmObj*, align 8
%args46788$k40425$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43836, %struct.ScmObj* %args46788$k40425$1)
store volatile %struct.ScmObj* %args46788$k40425$2, %struct.ScmObj** %stackaddr$prim47845, align 8
%clofunc47846 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40425)
musttail call tailcc void %clofunc47846(%struct.ScmObj* %k40425, %struct.ScmObj* %args46788$k40425$2)
ret void
}

define tailcc void @proc_clo$ae43735(%struct.ScmObj* %env$ae43735,%struct.ScmObj* %args4016840427) {
%stackaddr$env-ref47847 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43735, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47847
%stackaddr$prim47848 = alloca %struct.ScmObj*, align 8
%k40428 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4016840427)
store volatile %struct.ScmObj* %k40428, %struct.ScmObj** %stackaddr$prim47848, align 8
%stackaddr$prim47849 = alloca %struct.ScmObj*, align 8
%args40168 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4016840427)
store volatile %struct.ScmObj* %args40168, %struct.ScmObj** %stackaddr$prim47849, align 8
%stackaddr$prim47850 = alloca %struct.ScmObj*, align 8
%anf_45bind40329 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args40168)
store volatile %struct.ScmObj* %anf_45bind40329, %struct.ScmObj** %stackaddr$prim47850, align 8
%truthy$cmp47851 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40329)
%cmp$cmp47851 = icmp eq i64 %truthy$cmp47851, 1
br i1 %cmp$cmp47851, label %truebranch$cmp47851, label %falsebranch$cmp47851
truebranch$cmp47851:
%ae43741 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43742 = call %struct.ScmObj* @const_init_int(i64 1)
%args46790$k40428$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47852 = alloca %struct.ScmObj*, align 8
%args46790$k40428$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43742, %struct.ScmObj* %args46790$k40428$0)
store volatile %struct.ScmObj* %args46790$k40428$1, %struct.ScmObj** %stackaddr$prim47852, align 8
%stackaddr$prim47853 = alloca %struct.ScmObj*, align 8
%args46790$k40428$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43741, %struct.ScmObj* %args46790$k40428$1)
store volatile %struct.ScmObj* %args46790$k40428$2, %struct.ScmObj** %stackaddr$prim47853, align 8
%clofunc47854 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40428)
musttail call tailcc void %clofunc47854(%struct.ScmObj* %k40428, %struct.ScmObj* %args46790$k40428$2)
ret void
falsebranch$cmp47851:
%stackaddr$prim47855 = alloca %struct.ScmObj*, align 8
%anf_45bind40330 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40168)
store volatile %struct.ScmObj* %anf_45bind40330, %struct.ScmObj** %stackaddr$prim47855, align 8
%stackaddr$prim47856 = alloca %struct.ScmObj*, align 8
%anf_45bind40331 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40330)
store volatile %struct.ScmObj* %anf_45bind40331, %struct.ScmObj** %stackaddr$prim47856, align 8
%truthy$cmp47857 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40331)
%cmp$cmp47857 = icmp eq i64 %truthy$cmp47857, 1
br i1 %cmp$cmp47857, label %truebranch$cmp47857, label %falsebranch$cmp47857
truebranch$cmp47857:
%stackaddr$prim47858 = alloca %struct.ScmObj*, align 8
%cpsprim40429 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40168)
store volatile %struct.ScmObj* %cpsprim40429, %struct.ScmObj** %stackaddr$prim47858, align 8
%ae43754 = call %struct.ScmObj* @const_init_int(i64 0)
%args46791$k40428$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47859 = alloca %struct.ScmObj*, align 8
%args46791$k40428$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40429, %struct.ScmObj* %args46791$k40428$0)
store volatile %struct.ScmObj* %args46791$k40428$1, %struct.ScmObj** %stackaddr$prim47859, align 8
%stackaddr$prim47860 = alloca %struct.ScmObj*, align 8
%args46791$k40428$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43754, %struct.ScmObj* %args46791$k40428$1)
store volatile %struct.ScmObj* %args46791$k40428$2, %struct.ScmObj** %stackaddr$prim47860, align 8
%clofunc47861 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40428)
musttail call tailcc void %clofunc47861(%struct.ScmObj* %k40428, %struct.ScmObj* %args46791$k40428$2)
ret void
falsebranch$cmp47857:
%stackaddr$makeclosure47862 = alloca %struct.ScmObj*, align 8
%fptrToInt47863 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43759 to i64
%ae43759 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47863)
store volatile %struct.ScmObj* %ae43759, %struct.ScmObj** %stackaddr$makeclosure47862, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43759, %struct.ScmObj* %k40428, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43759, %struct.ScmObj* %_37foldl140107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43759, %struct.ScmObj* %args40168, i64 2)
%ae43760 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47864 = alloca %struct.ScmObj*, align 8
%fptrToInt47865 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43761 to i64
%ae43761 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47865)
store volatile %struct.ScmObj* %ae43761, %struct.ScmObj** %stackaddr$makeclosure47864, align 8
%args46801$ae43759$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47866 = alloca %struct.ScmObj*, align 8
%args46801$ae43759$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43761, %struct.ScmObj* %args46801$ae43759$0)
store volatile %struct.ScmObj* %args46801$ae43759$1, %struct.ScmObj** %stackaddr$prim47866, align 8
%stackaddr$prim47867 = alloca %struct.ScmObj*, align 8
%args46801$ae43759$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43760, %struct.ScmObj* %args46801$ae43759$1)
store volatile %struct.ScmObj* %args46801$ae43759$2, %struct.ScmObj** %stackaddr$prim47867, align 8
%clofunc47868 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43759)
musttail call tailcc void %clofunc47868(%struct.ScmObj* %ae43759, %struct.ScmObj* %args46801$ae43759$2)
ret void
}

define tailcc void @proc_clo$ae43759(%struct.ScmObj* %env$ae43759,%struct.ScmObj* %current_45args46792) {
%stackaddr$env-ref47869 = alloca %struct.ScmObj*, align 8
%k40428 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43759, i64 0)
store %struct.ScmObj* %k40428, %struct.ScmObj** %stackaddr$env-ref47869
%stackaddr$env-ref47870 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43759, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47870
%stackaddr$env-ref47871 = alloca %struct.ScmObj*, align 8
%args40168 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43759, i64 2)
store %struct.ScmObj* %args40168, %struct.ScmObj** %stackaddr$env-ref47871
%stackaddr$prim47872 = alloca %struct.ScmObj*, align 8
%_95k40430 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46792)
store volatile %struct.ScmObj* %_95k40430, %struct.ScmObj** %stackaddr$prim47872, align 8
%stackaddr$prim47873 = alloca %struct.ScmObj*, align 8
%current_45args46793 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46792)
store volatile %struct.ScmObj* %current_45args46793, %struct.ScmObj** %stackaddr$prim47873, align 8
%stackaddr$prim47874 = alloca %struct.ScmObj*, align 8
%anf_45bind40332 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46793)
store volatile %struct.ScmObj* %anf_45bind40332, %struct.ScmObj** %stackaddr$prim47874, align 8
%stackaddr$prim47875 = alloca %struct.ScmObj*, align 8
%anf_45bind40333 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40168)
store volatile %struct.ScmObj* %anf_45bind40333, %struct.ScmObj** %stackaddr$prim47875, align 8
%stackaddr$prim47876 = alloca %struct.ScmObj*, align 8
%anf_45bind40334 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40168)
store volatile %struct.ScmObj* %anf_45bind40334, %struct.ScmObj** %stackaddr$prim47876, align 8
%args46795$_37foldl140107$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47877 = alloca %struct.ScmObj*, align 8
%args46795$_37foldl140107$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40334, %struct.ScmObj* %args46795$_37foldl140107$0)
store volatile %struct.ScmObj* %args46795$_37foldl140107$1, %struct.ScmObj** %stackaddr$prim47877, align 8
%stackaddr$prim47878 = alloca %struct.ScmObj*, align 8
%args46795$_37foldl140107$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40333, %struct.ScmObj* %args46795$_37foldl140107$1)
store volatile %struct.ScmObj* %args46795$_37foldl140107$2, %struct.ScmObj** %stackaddr$prim47878, align 8
%stackaddr$prim47879 = alloca %struct.ScmObj*, align 8
%args46795$_37foldl140107$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40332, %struct.ScmObj* %args46795$_37foldl140107$2)
store volatile %struct.ScmObj* %args46795$_37foldl140107$3, %struct.ScmObj** %stackaddr$prim47879, align 8
%stackaddr$prim47880 = alloca %struct.ScmObj*, align 8
%args46795$_37foldl140107$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40428, %struct.ScmObj* %args46795$_37foldl140107$3)
store volatile %struct.ScmObj* %args46795$_37foldl140107$4, %struct.ScmObj** %stackaddr$prim47880, align 8
%clofunc47881 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140107)
musttail call tailcc void %clofunc47881(%struct.ScmObj* %_37foldl140107, %struct.ScmObj* %args46795$_37foldl140107$4)
ret void
}

define tailcc void @proc_clo$ae43761(%struct.ScmObj* %env$ae43761,%struct.ScmObj* %current_45args46796) {
%stackaddr$prim47882 = alloca %struct.ScmObj*, align 8
%k40431 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46796)
store volatile %struct.ScmObj* %k40431, %struct.ScmObj** %stackaddr$prim47882, align 8
%stackaddr$prim47883 = alloca %struct.ScmObj*, align 8
%current_45args46797 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46796)
store volatile %struct.ScmObj* %current_45args46797, %struct.ScmObj** %stackaddr$prim47883, align 8
%stackaddr$prim47884 = alloca %struct.ScmObj*, align 8
%n40170 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46797)
store volatile %struct.ScmObj* %n40170, %struct.ScmObj** %stackaddr$prim47884, align 8
%stackaddr$prim47885 = alloca %struct.ScmObj*, align 8
%current_45args46798 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46797)
store volatile %struct.ScmObj* %current_45args46798, %struct.ScmObj** %stackaddr$prim47885, align 8
%stackaddr$prim47886 = alloca %struct.ScmObj*, align 8
%v40169 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46798)
store volatile %struct.ScmObj* %v40169, %struct.ScmObj** %stackaddr$prim47886, align 8
%stackaddr$prim47887 = alloca %struct.ScmObj*, align 8
%cpsprim40432 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v40169, %struct.ScmObj* %n40170)
store volatile %struct.ScmObj* %cpsprim40432, %struct.ScmObj** %stackaddr$prim47887, align 8
%ae43765 = call %struct.ScmObj* @const_init_int(i64 0)
%args46800$k40431$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47888 = alloca %struct.ScmObj*, align 8
%args46800$k40431$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40432, %struct.ScmObj* %args46800$k40431$0)
store volatile %struct.ScmObj* %args46800$k40431$1, %struct.ScmObj** %stackaddr$prim47888, align 8
%stackaddr$prim47889 = alloca %struct.ScmObj*, align 8
%args46800$k40431$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43765, %struct.ScmObj* %args46800$k40431$1)
store volatile %struct.ScmObj* %args46800$k40431$2, %struct.ScmObj** %stackaddr$prim47889, align 8
%clofunc47890 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40431)
musttail call tailcc void %clofunc47890(%struct.ScmObj* %k40431, %struct.ScmObj* %args46800$k40431$2)
ret void
}

define tailcc void @proc_clo$ae43331(%struct.ScmObj* %env$ae43331,%struct.ScmObj* %current_45args46803) {
%stackaddr$prim47891 = alloca %struct.ScmObj*, align 8
%k40433 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46803)
store volatile %struct.ScmObj* %k40433, %struct.ScmObj** %stackaddr$prim47891, align 8
%stackaddr$prim47892 = alloca %struct.ScmObj*, align 8
%current_45args46804 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46803)
store volatile %struct.ScmObj* %current_45args46804, %struct.ScmObj** %stackaddr$prim47892, align 8
%stackaddr$prim47893 = alloca %struct.ScmObj*, align 8
%v40173 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46804)
store volatile %struct.ScmObj* %v40173, %struct.ScmObj** %stackaddr$prim47893, align 8
%stackaddr$prim47894 = alloca %struct.ScmObj*, align 8
%current_45args46805 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46804)
store volatile %struct.ScmObj* %current_45args46805, %struct.ScmObj** %stackaddr$prim47894, align 8
%stackaddr$prim47895 = alloca %struct.ScmObj*, align 8
%lst40172 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46805)
store volatile %struct.ScmObj* %lst40172, %struct.ScmObj** %stackaddr$prim47895, align 8
%ae43332 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim47896 = alloca %struct.ScmObj*, align 8
%lst40174 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae43332, %struct.ScmObj* %lst40172)
store volatile %struct.ScmObj* %lst40174, %struct.ScmObj** %stackaddr$prim47896, align 8
%stackaddr$makeclosure47897 = alloca %struct.ScmObj*, align 8
%fptrToInt47898 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43334 to i64
%ae43334 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47898)
store volatile %struct.ScmObj* %ae43334, %struct.ScmObj** %stackaddr$makeclosure47897, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43334, %struct.ScmObj* %k40433, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43334, %struct.ScmObj* %lst40174, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43334, %struct.ScmObj* %v40173, i64 2)
%ae43335 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47899 = alloca %struct.ScmObj*, align 8
%fptrToInt47900 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43336 to i64
%ae43336 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47900)
store volatile %struct.ScmObj* %ae43336, %struct.ScmObj** %stackaddr$makeclosure47899, align 8
%args46827$ae43334$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47901 = alloca %struct.ScmObj*, align 8
%args46827$ae43334$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43336, %struct.ScmObj* %args46827$ae43334$0)
store volatile %struct.ScmObj* %args46827$ae43334$1, %struct.ScmObj** %stackaddr$prim47901, align 8
%stackaddr$prim47902 = alloca %struct.ScmObj*, align 8
%args46827$ae43334$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43335, %struct.ScmObj* %args46827$ae43334$1)
store volatile %struct.ScmObj* %args46827$ae43334$2, %struct.ScmObj** %stackaddr$prim47902, align 8
%clofunc47903 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43334)
musttail call tailcc void %clofunc47903(%struct.ScmObj* %ae43334, %struct.ScmObj* %args46827$ae43334$2)
ret void
}

define tailcc void @proc_clo$ae43334(%struct.ScmObj* %env$ae43334,%struct.ScmObj* %current_45args46807) {
%stackaddr$env-ref47904 = alloca %struct.ScmObj*, align 8
%k40433 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43334, i64 0)
store %struct.ScmObj* %k40433, %struct.ScmObj** %stackaddr$env-ref47904
%stackaddr$env-ref47905 = alloca %struct.ScmObj*, align 8
%lst40174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43334, i64 1)
store %struct.ScmObj* %lst40174, %struct.ScmObj** %stackaddr$env-ref47905
%stackaddr$env-ref47906 = alloca %struct.ScmObj*, align 8
%v40173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43334, i64 2)
store %struct.ScmObj* %v40173, %struct.ScmObj** %stackaddr$env-ref47906
%stackaddr$prim47907 = alloca %struct.ScmObj*, align 8
%_95k40434 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46807)
store volatile %struct.ScmObj* %_95k40434, %struct.ScmObj** %stackaddr$prim47907, align 8
%stackaddr$prim47908 = alloca %struct.ScmObj*, align 8
%current_45args46808 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46807)
store volatile %struct.ScmObj* %current_45args46808, %struct.ScmObj** %stackaddr$prim47908, align 8
%stackaddr$prim47909 = alloca %struct.ScmObj*, align 8
%anf_45bind40321 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46808)
store volatile %struct.ScmObj* %anf_45bind40321, %struct.ScmObj** %stackaddr$prim47909, align 8
%stackaddr$makeclosure47910 = alloca %struct.ScmObj*, align 8
%fptrToInt47911 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43350 to i64
%ae43350 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47911)
store volatile %struct.ScmObj* %ae43350, %struct.ScmObj** %stackaddr$makeclosure47910, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43350, %struct.ScmObj* %k40433, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43350, %struct.ScmObj* %lst40174, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43350, %struct.ScmObj* %v40173, i64 2)
%stackaddr$makeclosure47912 = alloca %struct.ScmObj*, align 8
%fptrToInt47913 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43351 to i64
%ae43351 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47913)
store volatile %struct.ScmObj* %ae43351, %struct.ScmObj** %stackaddr$makeclosure47912, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43351, %struct.ScmObj* %k40433, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43351, %struct.ScmObj* %lst40174, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43351, %struct.ScmObj* %v40173, i64 2)
%args46822$anf_45bind40321$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47914 = alloca %struct.ScmObj*, align 8
%args46822$anf_45bind40321$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43351, %struct.ScmObj* %args46822$anf_45bind40321$0)
store volatile %struct.ScmObj* %args46822$anf_45bind40321$1, %struct.ScmObj** %stackaddr$prim47914, align 8
%stackaddr$prim47915 = alloca %struct.ScmObj*, align 8
%args46822$anf_45bind40321$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43350, %struct.ScmObj* %args46822$anf_45bind40321$1)
store volatile %struct.ScmObj* %args46822$anf_45bind40321$2, %struct.ScmObj** %stackaddr$prim47915, align 8
%clofunc47916 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40321)
musttail call tailcc void %clofunc47916(%struct.ScmObj* %anf_45bind40321, %struct.ScmObj* %args46822$anf_45bind40321$2)
ret void
}

define tailcc void @proc_clo$ae43350(%struct.ScmObj* %env$ae43350,%struct.ScmObj* %current_45args46810) {
%stackaddr$env-ref47917 = alloca %struct.ScmObj*, align 8
%k40433 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43350, i64 0)
store %struct.ScmObj* %k40433, %struct.ScmObj** %stackaddr$env-ref47917
%stackaddr$env-ref47918 = alloca %struct.ScmObj*, align 8
%lst40174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43350, i64 1)
store %struct.ScmObj* %lst40174, %struct.ScmObj** %stackaddr$env-ref47918
%stackaddr$env-ref47919 = alloca %struct.ScmObj*, align 8
%v40173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43350, i64 2)
store %struct.ScmObj* %v40173, %struct.ScmObj** %stackaddr$env-ref47919
%stackaddr$prim47920 = alloca %struct.ScmObj*, align 8
%_95k40435 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46810)
store volatile %struct.ScmObj* %_95k40435, %struct.ScmObj** %stackaddr$prim47920, align 8
%stackaddr$prim47921 = alloca %struct.ScmObj*, align 8
%current_45args46811 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46810)
store volatile %struct.ScmObj* %current_45args46811, %struct.ScmObj** %stackaddr$prim47921, align 8
%stackaddr$prim47922 = alloca %struct.ScmObj*, align 8
%cc40175 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46811)
store volatile %struct.ScmObj* %cc40175, %struct.ScmObj** %stackaddr$prim47922, align 8
%ae43459 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47923 = alloca %struct.ScmObj*, align 8
%anf_45bind40322 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43459)
store volatile %struct.ScmObj* %anf_45bind40322, %struct.ScmObj** %stackaddr$prim47923, align 8
%stackaddr$prim47924 = alloca %struct.ScmObj*, align 8
%anf_45bind40323 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40322)
store volatile %struct.ScmObj* %anf_45bind40323, %struct.ScmObj** %stackaddr$prim47924, align 8
%truthy$cmp47925 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40323)
%cmp$cmp47925 = icmp eq i64 %truthy$cmp47925, 1
br i1 %cmp$cmp47925, label %truebranch$cmp47925, label %falsebranch$cmp47925
truebranch$cmp47925:
%ae43463 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43464 = call %struct.ScmObj* @const_init_false()
%args46813$k40433$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47926 = alloca %struct.ScmObj*, align 8
%args46813$k40433$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43464, %struct.ScmObj* %args46813$k40433$0)
store volatile %struct.ScmObj* %args46813$k40433$1, %struct.ScmObj** %stackaddr$prim47926, align 8
%stackaddr$prim47927 = alloca %struct.ScmObj*, align 8
%args46813$k40433$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43463, %struct.ScmObj* %args46813$k40433$1)
store volatile %struct.ScmObj* %args46813$k40433$2, %struct.ScmObj** %stackaddr$prim47927, align 8
%clofunc47928 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40433)
musttail call tailcc void %clofunc47928(%struct.ScmObj* %k40433, %struct.ScmObj* %args46813$k40433$2)
ret void
falsebranch$cmp47925:
%ae43472 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47929 = alloca %struct.ScmObj*, align 8
%anf_45bind40324 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43472)
store volatile %struct.ScmObj* %anf_45bind40324, %struct.ScmObj** %stackaddr$prim47929, align 8
%stackaddr$prim47930 = alloca %struct.ScmObj*, align 8
%anf_45bind40325 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40324)
store volatile %struct.ScmObj* %anf_45bind40325, %struct.ScmObj** %stackaddr$prim47930, align 8
%stackaddr$prim47931 = alloca %struct.ScmObj*, align 8
%anf_45bind40326 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind40325, %struct.ScmObj* %v40173)
store volatile %struct.ScmObj* %anf_45bind40326, %struct.ScmObj** %stackaddr$prim47931, align 8
%truthy$cmp47932 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40326)
%cmp$cmp47932 = icmp eq i64 %truthy$cmp47932, 1
br i1 %cmp$cmp47932, label %truebranch$cmp47932, label %falsebranch$cmp47932
truebranch$cmp47932:
%ae43478 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47933 = alloca %struct.ScmObj*, align 8
%cpsprim40436 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43478)
store volatile %struct.ScmObj* %cpsprim40436, %struct.ScmObj** %stackaddr$prim47933, align 8
%ae43480 = call %struct.ScmObj* @const_init_int(i64 0)
%args46814$k40433$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47934 = alloca %struct.ScmObj*, align 8
%args46814$k40433$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40436, %struct.ScmObj* %args46814$k40433$0)
store volatile %struct.ScmObj* %args46814$k40433$1, %struct.ScmObj** %stackaddr$prim47934, align 8
%stackaddr$prim47935 = alloca %struct.ScmObj*, align 8
%args46814$k40433$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43480, %struct.ScmObj* %args46814$k40433$1)
store volatile %struct.ScmObj* %args46814$k40433$2, %struct.ScmObj** %stackaddr$prim47935, align 8
%clofunc47936 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40433)
musttail call tailcc void %clofunc47936(%struct.ScmObj* %k40433, %struct.ScmObj* %args46814$k40433$2)
ret void
falsebranch$cmp47932:
%ae43491 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47937 = alloca %struct.ScmObj*, align 8
%anf_45bind40327 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43491)
store volatile %struct.ScmObj* %anf_45bind40327, %struct.ScmObj** %stackaddr$prim47937, align 8
%stackaddr$prim47938 = alloca %struct.ScmObj*, align 8
%anf_45bind40328 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40327)
store volatile %struct.ScmObj* %anf_45bind40328, %struct.ScmObj** %stackaddr$prim47938, align 8
%ae43494 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47939 = alloca %struct.ScmObj*, align 8
%_95040177 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43494, %struct.ScmObj* %anf_45bind40328)
store volatile %struct.ScmObj* %_95040177, %struct.ScmObj** %stackaddr$prim47939, align 8
%args46815$cc40175$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47940 = alloca %struct.ScmObj*, align 8
%args46815$cc40175$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40175, %struct.ScmObj* %args46815$cc40175$0)
store volatile %struct.ScmObj* %args46815$cc40175$1, %struct.ScmObj** %stackaddr$prim47940, align 8
%stackaddr$prim47941 = alloca %struct.ScmObj*, align 8
%args46815$cc40175$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40433, %struct.ScmObj* %args46815$cc40175$1)
store volatile %struct.ScmObj* %args46815$cc40175$2, %struct.ScmObj** %stackaddr$prim47941, align 8
%clofunc47942 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40175)
musttail call tailcc void %clofunc47942(%struct.ScmObj* %cc40175, %struct.ScmObj* %args46815$cc40175$2)
ret void
}

define tailcc void @proc_clo$ae43351(%struct.ScmObj* %env$ae43351,%struct.ScmObj* %current_45args46816) {
%stackaddr$env-ref47943 = alloca %struct.ScmObj*, align 8
%k40433 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43351, i64 0)
store %struct.ScmObj* %k40433, %struct.ScmObj** %stackaddr$env-ref47943
%stackaddr$env-ref47944 = alloca %struct.ScmObj*, align 8
%lst40174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43351, i64 1)
store %struct.ScmObj* %lst40174, %struct.ScmObj** %stackaddr$env-ref47944
%stackaddr$env-ref47945 = alloca %struct.ScmObj*, align 8
%v40173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43351, i64 2)
store %struct.ScmObj* %v40173, %struct.ScmObj** %stackaddr$env-ref47945
%stackaddr$prim47946 = alloca %struct.ScmObj*, align 8
%_95k40435 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46816)
store volatile %struct.ScmObj* %_95k40435, %struct.ScmObj** %stackaddr$prim47946, align 8
%stackaddr$prim47947 = alloca %struct.ScmObj*, align 8
%current_45args46817 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46816)
store volatile %struct.ScmObj* %current_45args46817, %struct.ScmObj** %stackaddr$prim47947, align 8
%stackaddr$prim47948 = alloca %struct.ScmObj*, align 8
%cc40175 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46817)
store volatile %struct.ScmObj* %cc40175, %struct.ScmObj** %stackaddr$prim47948, align 8
%ae43353 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47949 = alloca %struct.ScmObj*, align 8
%anf_45bind40322 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43353)
store volatile %struct.ScmObj* %anf_45bind40322, %struct.ScmObj** %stackaddr$prim47949, align 8
%stackaddr$prim47950 = alloca %struct.ScmObj*, align 8
%anf_45bind40323 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40322)
store volatile %struct.ScmObj* %anf_45bind40323, %struct.ScmObj** %stackaddr$prim47950, align 8
%truthy$cmp47951 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40323)
%cmp$cmp47951 = icmp eq i64 %truthy$cmp47951, 1
br i1 %cmp$cmp47951, label %truebranch$cmp47951, label %falsebranch$cmp47951
truebranch$cmp47951:
%ae43357 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43358 = call %struct.ScmObj* @const_init_false()
%args46819$k40433$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47952 = alloca %struct.ScmObj*, align 8
%args46819$k40433$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43358, %struct.ScmObj* %args46819$k40433$0)
store volatile %struct.ScmObj* %args46819$k40433$1, %struct.ScmObj** %stackaddr$prim47952, align 8
%stackaddr$prim47953 = alloca %struct.ScmObj*, align 8
%args46819$k40433$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43357, %struct.ScmObj* %args46819$k40433$1)
store volatile %struct.ScmObj* %args46819$k40433$2, %struct.ScmObj** %stackaddr$prim47953, align 8
%clofunc47954 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40433)
musttail call tailcc void %clofunc47954(%struct.ScmObj* %k40433, %struct.ScmObj* %args46819$k40433$2)
ret void
falsebranch$cmp47951:
%ae43366 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47955 = alloca %struct.ScmObj*, align 8
%anf_45bind40324 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43366)
store volatile %struct.ScmObj* %anf_45bind40324, %struct.ScmObj** %stackaddr$prim47955, align 8
%stackaddr$prim47956 = alloca %struct.ScmObj*, align 8
%anf_45bind40325 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40324)
store volatile %struct.ScmObj* %anf_45bind40325, %struct.ScmObj** %stackaddr$prim47956, align 8
%stackaddr$prim47957 = alloca %struct.ScmObj*, align 8
%anf_45bind40326 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind40325, %struct.ScmObj* %v40173)
store volatile %struct.ScmObj* %anf_45bind40326, %struct.ScmObj** %stackaddr$prim47957, align 8
%truthy$cmp47958 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40326)
%cmp$cmp47958 = icmp eq i64 %truthy$cmp47958, 1
br i1 %cmp$cmp47958, label %truebranch$cmp47958, label %falsebranch$cmp47958
truebranch$cmp47958:
%ae43372 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47959 = alloca %struct.ScmObj*, align 8
%cpsprim40436 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43372)
store volatile %struct.ScmObj* %cpsprim40436, %struct.ScmObj** %stackaddr$prim47959, align 8
%ae43374 = call %struct.ScmObj* @const_init_int(i64 0)
%args46820$k40433$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47960 = alloca %struct.ScmObj*, align 8
%args46820$k40433$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40436, %struct.ScmObj* %args46820$k40433$0)
store volatile %struct.ScmObj* %args46820$k40433$1, %struct.ScmObj** %stackaddr$prim47960, align 8
%stackaddr$prim47961 = alloca %struct.ScmObj*, align 8
%args46820$k40433$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43374, %struct.ScmObj* %args46820$k40433$1)
store volatile %struct.ScmObj* %args46820$k40433$2, %struct.ScmObj** %stackaddr$prim47961, align 8
%clofunc47962 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40433)
musttail call tailcc void %clofunc47962(%struct.ScmObj* %k40433, %struct.ScmObj* %args46820$k40433$2)
ret void
falsebranch$cmp47958:
%ae43385 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47963 = alloca %struct.ScmObj*, align 8
%anf_45bind40327 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43385)
store volatile %struct.ScmObj* %anf_45bind40327, %struct.ScmObj** %stackaddr$prim47963, align 8
%stackaddr$prim47964 = alloca %struct.ScmObj*, align 8
%anf_45bind40328 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40327)
store volatile %struct.ScmObj* %anf_45bind40328, %struct.ScmObj** %stackaddr$prim47964, align 8
%ae43388 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47965 = alloca %struct.ScmObj*, align 8
%_95040177 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43388, %struct.ScmObj* %anf_45bind40328)
store volatile %struct.ScmObj* %_95040177, %struct.ScmObj** %stackaddr$prim47965, align 8
%args46821$cc40175$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47966 = alloca %struct.ScmObj*, align 8
%args46821$cc40175$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40175, %struct.ScmObj* %args46821$cc40175$0)
store volatile %struct.ScmObj* %args46821$cc40175$1, %struct.ScmObj** %stackaddr$prim47966, align 8
%stackaddr$prim47967 = alloca %struct.ScmObj*, align 8
%args46821$cc40175$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40433, %struct.ScmObj* %args46821$cc40175$1)
store volatile %struct.ScmObj* %args46821$cc40175$2, %struct.ScmObj** %stackaddr$prim47967, align 8
%clofunc47968 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40175)
musttail call tailcc void %clofunc47968(%struct.ScmObj* %cc40175, %struct.ScmObj* %args46821$cc40175$2)
ret void
}

define tailcc void @proc_clo$ae43336(%struct.ScmObj* %env$ae43336,%struct.ScmObj* %current_45args46823) {
%stackaddr$prim47969 = alloca %struct.ScmObj*, align 8
%k40437 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46823)
store volatile %struct.ScmObj* %k40437, %struct.ScmObj** %stackaddr$prim47969, align 8
%stackaddr$prim47970 = alloca %struct.ScmObj*, align 8
%current_45args46824 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46823)
store volatile %struct.ScmObj* %current_45args46824, %struct.ScmObj** %stackaddr$prim47970, align 8
%stackaddr$prim47971 = alloca %struct.ScmObj*, align 8
%u40176 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46824)
store volatile %struct.ScmObj* %u40176, %struct.ScmObj** %stackaddr$prim47971, align 8
%args46826$u40176$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47972 = alloca %struct.ScmObj*, align 8
%args46826$u40176$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u40176, %struct.ScmObj* %args46826$u40176$0)
store volatile %struct.ScmObj* %args46826$u40176$1, %struct.ScmObj** %stackaddr$prim47972, align 8
%stackaddr$prim47973 = alloca %struct.ScmObj*, align 8
%args46826$u40176$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40437, %struct.ScmObj* %args46826$u40176$1)
store volatile %struct.ScmObj* %args46826$u40176$2, %struct.ScmObj** %stackaddr$prim47973, align 8
%clofunc47974 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u40176)
musttail call tailcc void %clofunc47974(%struct.ScmObj* %u40176, %struct.ScmObj* %args46826$u40176$2)
ret void
}

define tailcc void @proc_clo$ae42795(%struct.ScmObj* %env$ae42795,%struct.ScmObj* %current_45args46829) {
%stackaddr$prim47975 = alloca %struct.ScmObj*, align 8
%k40438 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46829)
store volatile %struct.ScmObj* %k40438, %struct.ScmObj** %stackaddr$prim47975, align 8
%stackaddr$prim47976 = alloca %struct.ScmObj*, align 8
%current_45args46830 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46829)
store volatile %struct.ScmObj* %current_45args46830, %struct.ScmObj** %stackaddr$prim47976, align 8
%stackaddr$prim47977 = alloca %struct.ScmObj*, align 8
%lst40180 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46830)
store volatile %struct.ScmObj* %lst40180, %struct.ScmObj** %stackaddr$prim47977, align 8
%stackaddr$prim47978 = alloca %struct.ScmObj*, align 8
%current_45args46831 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46830)
store volatile %struct.ScmObj* %current_45args46831, %struct.ScmObj** %stackaddr$prim47978, align 8
%stackaddr$prim47979 = alloca %struct.ScmObj*, align 8
%n40179 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46831)
store volatile %struct.ScmObj* %n40179, %struct.ScmObj** %stackaddr$prim47979, align 8
%ae42796 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim47980 = alloca %struct.ScmObj*, align 8
%n40182 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42796, %struct.ScmObj* %n40179)
store volatile %struct.ScmObj* %n40182, %struct.ScmObj** %stackaddr$prim47980, align 8
%ae42798 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim47981 = alloca %struct.ScmObj*, align 8
%lst40181 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42798, %struct.ScmObj* %lst40180)
store volatile %struct.ScmObj* %lst40181, %struct.ScmObj** %stackaddr$prim47981, align 8
%stackaddr$makeclosure47982 = alloca %struct.ScmObj*, align 8
%fptrToInt47983 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42800 to i64
%ae42800 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47983)
store volatile %struct.ScmObj* %ae42800, %struct.ScmObj** %stackaddr$makeclosure47982, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42800, %struct.ScmObj* %k40438, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42800, %struct.ScmObj* %n40182, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42800, %struct.ScmObj* %lst40181, i64 2)
%ae42801 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47984 = alloca %struct.ScmObj*, align 8
%fptrToInt47985 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42802 to i64
%ae42802 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47985)
store volatile %struct.ScmObj* %ae42802, %struct.ScmObj** %stackaddr$makeclosure47984, align 8
%args46851$ae42800$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47986 = alloca %struct.ScmObj*, align 8
%args46851$ae42800$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42802, %struct.ScmObj* %args46851$ae42800$0)
store volatile %struct.ScmObj* %args46851$ae42800$1, %struct.ScmObj** %stackaddr$prim47986, align 8
%stackaddr$prim47987 = alloca %struct.ScmObj*, align 8
%args46851$ae42800$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42801, %struct.ScmObj* %args46851$ae42800$1)
store volatile %struct.ScmObj* %args46851$ae42800$2, %struct.ScmObj** %stackaddr$prim47987, align 8
%clofunc47988 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42800)
musttail call tailcc void %clofunc47988(%struct.ScmObj* %ae42800, %struct.ScmObj* %args46851$ae42800$2)
ret void
}

define tailcc void @proc_clo$ae42800(%struct.ScmObj* %env$ae42800,%struct.ScmObj* %current_45args46833) {
%stackaddr$env-ref47989 = alloca %struct.ScmObj*, align 8
%k40438 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42800, i64 0)
store %struct.ScmObj* %k40438, %struct.ScmObj** %stackaddr$env-ref47989
%stackaddr$env-ref47990 = alloca %struct.ScmObj*, align 8
%n40182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42800, i64 1)
store %struct.ScmObj* %n40182, %struct.ScmObj** %stackaddr$env-ref47990
%stackaddr$env-ref47991 = alloca %struct.ScmObj*, align 8
%lst40181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42800, i64 2)
store %struct.ScmObj* %lst40181, %struct.ScmObj** %stackaddr$env-ref47991
%stackaddr$prim47992 = alloca %struct.ScmObj*, align 8
%_95k40439 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46833)
store volatile %struct.ScmObj* %_95k40439, %struct.ScmObj** %stackaddr$prim47992, align 8
%stackaddr$prim47993 = alloca %struct.ScmObj*, align 8
%current_45args46834 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46833)
store volatile %struct.ScmObj* %current_45args46834, %struct.ScmObj** %stackaddr$prim47993, align 8
%stackaddr$prim47994 = alloca %struct.ScmObj*, align 8
%anf_45bind40314 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46834)
store volatile %struct.ScmObj* %anf_45bind40314, %struct.ScmObj** %stackaddr$prim47994, align 8
%stackaddr$makeclosure47995 = alloca %struct.ScmObj*, align 8
%fptrToInt47996 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42816 to i64
%ae42816 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47996)
store volatile %struct.ScmObj* %ae42816, %struct.ScmObj** %stackaddr$makeclosure47995, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42816, %struct.ScmObj* %k40438, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42816, %struct.ScmObj* %n40182, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42816, %struct.ScmObj* %lst40181, i64 2)
%stackaddr$makeclosure47997 = alloca %struct.ScmObj*, align 8
%fptrToInt47998 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42817 to i64
%ae42817 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47998)
store volatile %struct.ScmObj* %ae42817, %struct.ScmObj** %stackaddr$makeclosure47997, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42817, %struct.ScmObj* %k40438, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42817, %struct.ScmObj* %n40182, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42817, %struct.ScmObj* %lst40181, i64 2)
%args46846$anf_45bind40314$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47999 = alloca %struct.ScmObj*, align 8
%args46846$anf_45bind40314$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42817, %struct.ScmObj* %args46846$anf_45bind40314$0)
store volatile %struct.ScmObj* %args46846$anf_45bind40314$1, %struct.ScmObj** %stackaddr$prim47999, align 8
%stackaddr$prim48000 = alloca %struct.ScmObj*, align 8
%args46846$anf_45bind40314$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42816, %struct.ScmObj* %args46846$anf_45bind40314$1)
store volatile %struct.ScmObj* %args46846$anf_45bind40314$2, %struct.ScmObj** %stackaddr$prim48000, align 8
%clofunc48001 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40314)
musttail call tailcc void %clofunc48001(%struct.ScmObj* %anf_45bind40314, %struct.ScmObj* %args46846$anf_45bind40314$2)
ret void
}

define tailcc void @proc_clo$ae42816(%struct.ScmObj* %env$ae42816,%struct.ScmObj* %current_45args46836) {
%stackaddr$env-ref48002 = alloca %struct.ScmObj*, align 8
%k40438 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42816, i64 0)
store %struct.ScmObj* %k40438, %struct.ScmObj** %stackaddr$env-ref48002
%stackaddr$env-ref48003 = alloca %struct.ScmObj*, align 8
%n40182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42816, i64 1)
store %struct.ScmObj* %n40182, %struct.ScmObj** %stackaddr$env-ref48003
%stackaddr$env-ref48004 = alloca %struct.ScmObj*, align 8
%lst40181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42816, i64 2)
store %struct.ScmObj* %lst40181, %struct.ScmObj** %stackaddr$env-ref48004
%stackaddr$prim48005 = alloca %struct.ScmObj*, align 8
%_95k40440 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46836)
store volatile %struct.ScmObj* %_95k40440, %struct.ScmObj** %stackaddr$prim48005, align 8
%stackaddr$prim48006 = alloca %struct.ScmObj*, align 8
%current_45args46837 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46836)
store volatile %struct.ScmObj* %current_45args46837, %struct.ScmObj** %stackaddr$prim48006, align 8
%stackaddr$prim48007 = alloca %struct.ScmObj*, align 8
%cc40183 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46837)
store volatile %struct.ScmObj* %cc40183, %struct.ScmObj** %stackaddr$prim48007, align 8
%ae42959 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48008 = alloca %struct.ScmObj*, align 8
%anf_45bind40315 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40182, %struct.ScmObj* %ae42959)
store volatile %struct.ScmObj* %anf_45bind40315, %struct.ScmObj** %stackaddr$prim48008, align 8
%ae42960 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48009 = alloca %struct.ScmObj*, align 8
%anf_45bind40316 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae42960, %struct.ScmObj* %anf_45bind40315)
store volatile %struct.ScmObj* %anf_45bind40316, %struct.ScmObj** %stackaddr$prim48009, align 8
%truthy$cmp48010 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40316)
%cmp$cmp48010 = icmp eq i64 %truthy$cmp48010, 1
br i1 %cmp$cmp48010, label %truebranch$cmp48010, label %falsebranch$cmp48010
truebranch$cmp48010:
%ae42964 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48011 = alloca %struct.ScmObj*, align 8
%cpsprim40441 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40181, %struct.ScmObj* %ae42964)
store volatile %struct.ScmObj* %cpsprim40441, %struct.ScmObj** %stackaddr$prim48011, align 8
%ae42966 = call %struct.ScmObj* @const_init_int(i64 0)
%args46839$k40438$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48012 = alloca %struct.ScmObj*, align 8
%args46839$k40438$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40441, %struct.ScmObj* %args46839$k40438$0)
store volatile %struct.ScmObj* %args46839$k40438$1, %struct.ScmObj** %stackaddr$prim48012, align 8
%stackaddr$prim48013 = alloca %struct.ScmObj*, align 8
%args46839$k40438$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42966, %struct.ScmObj* %args46839$k40438$1)
store volatile %struct.ScmObj* %args46839$k40438$2, %struct.ScmObj** %stackaddr$prim48013, align 8
%clofunc48014 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40438)
musttail call tailcc void %clofunc48014(%struct.ScmObj* %k40438, %struct.ScmObj* %args46839$k40438$2)
ret void
falsebranch$cmp48010:
%ae42977 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48015 = alloca %struct.ScmObj*, align 8
%anf_45bind40317 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40181, %struct.ScmObj* %ae42977)
store volatile %struct.ScmObj* %anf_45bind40317, %struct.ScmObj** %stackaddr$prim48015, align 8
%stackaddr$prim48016 = alloca %struct.ScmObj*, align 8
%anf_45bind40318 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40317)
store volatile %struct.ScmObj* %anf_45bind40318, %struct.ScmObj** %stackaddr$prim48016, align 8
%ae42980 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48017 = alloca %struct.ScmObj*, align 8
%_95040186 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40181, %struct.ScmObj* %ae42980, %struct.ScmObj* %anf_45bind40318)
store volatile %struct.ScmObj* %_95040186, %struct.ScmObj** %stackaddr$prim48017, align 8
%ae42983 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48018 = alloca %struct.ScmObj*, align 8
%anf_45bind40319 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40182, %struct.ScmObj* %ae42983)
store volatile %struct.ScmObj* %anf_45bind40319, %struct.ScmObj** %stackaddr$prim48018, align 8
%ae42985 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim48019 = alloca %struct.ScmObj*, align 8
%anf_45bind40320 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind40319, %struct.ScmObj* %ae42985)
store volatile %struct.ScmObj* %anf_45bind40320, %struct.ScmObj** %stackaddr$prim48019, align 8
%ae42987 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48020 = alloca %struct.ScmObj*, align 8
%_95140185 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n40182, %struct.ScmObj* %ae42987, %struct.ScmObj* %anf_45bind40320)
store volatile %struct.ScmObj* %_95140185, %struct.ScmObj** %stackaddr$prim48020, align 8
%args46840$cc40183$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48021 = alloca %struct.ScmObj*, align 8
%args46840$cc40183$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40183, %struct.ScmObj* %args46840$cc40183$0)
store volatile %struct.ScmObj* %args46840$cc40183$1, %struct.ScmObj** %stackaddr$prim48021, align 8
%stackaddr$prim48022 = alloca %struct.ScmObj*, align 8
%args46840$cc40183$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40438, %struct.ScmObj* %args46840$cc40183$1)
store volatile %struct.ScmObj* %args46840$cc40183$2, %struct.ScmObj** %stackaddr$prim48022, align 8
%clofunc48023 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40183)
musttail call tailcc void %clofunc48023(%struct.ScmObj* %cc40183, %struct.ScmObj* %args46840$cc40183$2)
ret void
}

define tailcc void @proc_clo$ae42817(%struct.ScmObj* %env$ae42817,%struct.ScmObj* %current_45args46841) {
%stackaddr$env-ref48024 = alloca %struct.ScmObj*, align 8
%k40438 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42817, i64 0)
store %struct.ScmObj* %k40438, %struct.ScmObj** %stackaddr$env-ref48024
%stackaddr$env-ref48025 = alloca %struct.ScmObj*, align 8
%n40182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42817, i64 1)
store %struct.ScmObj* %n40182, %struct.ScmObj** %stackaddr$env-ref48025
%stackaddr$env-ref48026 = alloca %struct.ScmObj*, align 8
%lst40181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42817, i64 2)
store %struct.ScmObj* %lst40181, %struct.ScmObj** %stackaddr$env-ref48026
%stackaddr$prim48027 = alloca %struct.ScmObj*, align 8
%_95k40440 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46841)
store volatile %struct.ScmObj* %_95k40440, %struct.ScmObj** %stackaddr$prim48027, align 8
%stackaddr$prim48028 = alloca %struct.ScmObj*, align 8
%current_45args46842 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46841)
store volatile %struct.ScmObj* %current_45args46842, %struct.ScmObj** %stackaddr$prim48028, align 8
%stackaddr$prim48029 = alloca %struct.ScmObj*, align 8
%cc40183 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46842)
store volatile %struct.ScmObj* %cc40183, %struct.ScmObj** %stackaddr$prim48029, align 8
%ae42819 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48030 = alloca %struct.ScmObj*, align 8
%anf_45bind40315 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40182, %struct.ScmObj* %ae42819)
store volatile %struct.ScmObj* %anf_45bind40315, %struct.ScmObj** %stackaddr$prim48030, align 8
%ae42820 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48031 = alloca %struct.ScmObj*, align 8
%anf_45bind40316 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae42820, %struct.ScmObj* %anf_45bind40315)
store volatile %struct.ScmObj* %anf_45bind40316, %struct.ScmObj** %stackaddr$prim48031, align 8
%truthy$cmp48032 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40316)
%cmp$cmp48032 = icmp eq i64 %truthy$cmp48032, 1
br i1 %cmp$cmp48032, label %truebranch$cmp48032, label %falsebranch$cmp48032
truebranch$cmp48032:
%ae42824 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48033 = alloca %struct.ScmObj*, align 8
%cpsprim40441 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40181, %struct.ScmObj* %ae42824)
store volatile %struct.ScmObj* %cpsprim40441, %struct.ScmObj** %stackaddr$prim48033, align 8
%ae42826 = call %struct.ScmObj* @const_init_int(i64 0)
%args46844$k40438$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48034 = alloca %struct.ScmObj*, align 8
%args46844$k40438$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40441, %struct.ScmObj* %args46844$k40438$0)
store volatile %struct.ScmObj* %args46844$k40438$1, %struct.ScmObj** %stackaddr$prim48034, align 8
%stackaddr$prim48035 = alloca %struct.ScmObj*, align 8
%args46844$k40438$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42826, %struct.ScmObj* %args46844$k40438$1)
store volatile %struct.ScmObj* %args46844$k40438$2, %struct.ScmObj** %stackaddr$prim48035, align 8
%clofunc48036 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40438)
musttail call tailcc void %clofunc48036(%struct.ScmObj* %k40438, %struct.ScmObj* %args46844$k40438$2)
ret void
falsebranch$cmp48032:
%ae42837 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48037 = alloca %struct.ScmObj*, align 8
%anf_45bind40317 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40181, %struct.ScmObj* %ae42837)
store volatile %struct.ScmObj* %anf_45bind40317, %struct.ScmObj** %stackaddr$prim48037, align 8
%stackaddr$prim48038 = alloca %struct.ScmObj*, align 8
%anf_45bind40318 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40317)
store volatile %struct.ScmObj* %anf_45bind40318, %struct.ScmObj** %stackaddr$prim48038, align 8
%ae42840 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48039 = alloca %struct.ScmObj*, align 8
%_95040186 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40181, %struct.ScmObj* %ae42840, %struct.ScmObj* %anf_45bind40318)
store volatile %struct.ScmObj* %_95040186, %struct.ScmObj** %stackaddr$prim48039, align 8
%ae42843 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48040 = alloca %struct.ScmObj*, align 8
%anf_45bind40319 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40182, %struct.ScmObj* %ae42843)
store volatile %struct.ScmObj* %anf_45bind40319, %struct.ScmObj** %stackaddr$prim48040, align 8
%ae42845 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim48041 = alloca %struct.ScmObj*, align 8
%anf_45bind40320 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind40319, %struct.ScmObj* %ae42845)
store volatile %struct.ScmObj* %anf_45bind40320, %struct.ScmObj** %stackaddr$prim48041, align 8
%ae42847 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48042 = alloca %struct.ScmObj*, align 8
%_95140185 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n40182, %struct.ScmObj* %ae42847, %struct.ScmObj* %anf_45bind40320)
store volatile %struct.ScmObj* %_95140185, %struct.ScmObj** %stackaddr$prim48042, align 8
%args46845$cc40183$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48043 = alloca %struct.ScmObj*, align 8
%args46845$cc40183$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40183, %struct.ScmObj* %args46845$cc40183$0)
store volatile %struct.ScmObj* %args46845$cc40183$1, %struct.ScmObj** %stackaddr$prim48043, align 8
%stackaddr$prim48044 = alloca %struct.ScmObj*, align 8
%args46845$cc40183$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40438, %struct.ScmObj* %args46845$cc40183$1)
store volatile %struct.ScmObj* %args46845$cc40183$2, %struct.ScmObj** %stackaddr$prim48044, align 8
%clofunc48045 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40183)
musttail call tailcc void %clofunc48045(%struct.ScmObj* %cc40183, %struct.ScmObj* %args46845$cc40183$2)
ret void
}

define tailcc void @proc_clo$ae42802(%struct.ScmObj* %env$ae42802,%struct.ScmObj* %current_45args46847) {
%stackaddr$prim48046 = alloca %struct.ScmObj*, align 8
%k40442 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46847)
store volatile %struct.ScmObj* %k40442, %struct.ScmObj** %stackaddr$prim48046, align 8
%stackaddr$prim48047 = alloca %struct.ScmObj*, align 8
%current_45args46848 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46847)
store volatile %struct.ScmObj* %current_45args46848, %struct.ScmObj** %stackaddr$prim48047, align 8
%stackaddr$prim48048 = alloca %struct.ScmObj*, align 8
%u40184 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46848)
store volatile %struct.ScmObj* %u40184, %struct.ScmObj** %stackaddr$prim48048, align 8
%args46850$u40184$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48049 = alloca %struct.ScmObj*, align 8
%args46850$u40184$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u40184, %struct.ScmObj* %args46850$u40184$0)
store volatile %struct.ScmObj* %args46850$u40184$1, %struct.ScmObj** %stackaddr$prim48049, align 8
%stackaddr$prim48050 = alloca %struct.ScmObj*, align 8
%args46850$u40184$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40442, %struct.ScmObj* %args46850$u40184$1)
store volatile %struct.ScmObj* %args46850$u40184$2, %struct.ScmObj** %stackaddr$prim48050, align 8
%clofunc48051 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u40184)
musttail call tailcc void %clofunc48051(%struct.ScmObj* %u40184, %struct.ScmObj* %args46850$u40184$2)
ret void
}

define tailcc void @proc_clo$ae42379(%struct.ScmObj* %env$ae42379,%struct.ScmObj* %current_45args46853) {
%stackaddr$prim48052 = alloca %struct.ScmObj*, align 8
%k40443 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46853)
store volatile %struct.ScmObj* %k40443, %struct.ScmObj** %stackaddr$prim48052, align 8
%stackaddr$prim48053 = alloca %struct.ScmObj*, align 8
%current_45args46854 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46853)
store volatile %struct.ScmObj* %current_45args46854, %struct.ScmObj** %stackaddr$prim48053, align 8
%stackaddr$prim48054 = alloca %struct.ScmObj*, align 8
%a40188 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46854)
store volatile %struct.ScmObj* %a40188, %struct.ScmObj** %stackaddr$prim48054, align 8
%ae42380 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim48055 = alloca %struct.ScmObj*, align 8
%a40189 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42380, %struct.ScmObj* %a40188)
store volatile %struct.ScmObj* %a40189, %struct.ScmObj** %stackaddr$prim48055, align 8
%stackaddr$makeclosure48056 = alloca %struct.ScmObj*, align 8
%fptrToInt48057 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42382 to i64
%ae42382 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48057)
store volatile %struct.ScmObj* %ae42382, %struct.ScmObj** %stackaddr$makeclosure48056, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42382, %struct.ScmObj* %a40189, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42382, %struct.ScmObj* %k40443, i64 1)
%ae42383 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48058 = alloca %struct.ScmObj*, align 8
%fptrToInt48059 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42384 to i64
%ae42384 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48059)
store volatile %struct.ScmObj* %ae42384, %struct.ScmObj** %stackaddr$makeclosure48058, align 8
%args46876$ae42382$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48060 = alloca %struct.ScmObj*, align 8
%args46876$ae42382$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42384, %struct.ScmObj* %args46876$ae42382$0)
store volatile %struct.ScmObj* %args46876$ae42382$1, %struct.ScmObj** %stackaddr$prim48060, align 8
%stackaddr$prim48061 = alloca %struct.ScmObj*, align 8
%args46876$ae42382$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42383, %struct.ScmObj* %args46876$ae42382$1)
store volatile %struct.ScmObj* %args46876$ae42382$2, %struct.ScmObj** %stackaddr$prim48061, align 8
%clofunc48062 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42382)
musttail call tailcc void %clofunc48062(%struct.ScmObj* %ae42382, %struct.ScmObj* %args46876$ae42382$2)
ret void
}

define tailcc void @proc_clo$ae42382(%struct.ScmObj* %env$ae42382,%struct.ScmObj* %current_45args46856) {
%stackaddr$env-ref48063 = alloca %struct.ScmObj*, align 8
%a40189 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42382, i64 0)
store %struct.ScmObj* %a40189, %struct.ScmObj** %stackaddr$env-ref48063
%stackaddr$env-ref48064 = alloca %struct.ScmObj*, align 8
%k40443 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42382, i64 1)
store %struct.ScmObj* %k40443, %struct.ScmObj** %stackaddr$env-ref48064
%stackaddr$prim48065 = alloca %struct.ScmObj*, align 8
%_95k40444 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46856)
store volatile %struct.ScmObj* %_95k40444, %struct.ScmObj** %stackaddr$prim48065, align 8
%stackaddr$prim48066 = alloca %struct.ScmObj*, align 8
%current_45args46857 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46856)
store volatile %struct.ScmObj* %current_45args46857, %struct.ScmObj** %stackaddr$prim48066, align 8
%stackaddr$prim48067 = alloca %struct.ScmObj*, align 8
%anf_45bind40306 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46857)
store volatile %struct.ScmObj* %anf_45bind40306, %struct.ScmObj** %stackaddr$prim48067, align 8
%stackaddr$makeclosure48068 = alloca %struct.ScmObj*, align 8
%fptrToInt48069 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42401 to i64
%ae42401 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48069)
store volatile %struct.ScmObj* %ae42401, %struct.ScmObj** %stackaddr$makeclosure48068, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42401, %struct.ScmObj* %a40189, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42401, %struct.ScmObj* %k40443, i64 1)
%stackaddr$makeclosure48070 = alloca %struct.ScmObj*, align 8
%fptrToInt48071 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42402 to i64
%ae42402 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48071)
store volatile %struct.ScmObj* %ae42402, %struct.ScmObj** %stackaddr$makeclosure48070, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42402, %struct.ScmObj* %a40189, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42402, %struct.ScmObj* %k40443, i64 1)
%args46871$anf_45bind40306$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48072 = alloca %struct.ScmObj*, align 8
%args46871$anf_45bind40306$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42402, %struct.ScmObj* %args46871$anf_45bind40306$0)
store volatile %struct.ScmObj* %args46871$anf_45bind40306$1, %struct.ScmObj** %stackaddr$prim48072, align 8
%stackaddr$prim48073 = alloca %struct.ScmObj*, align 8
%args46871$anf_45bind40306$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42401, %struct.ScmObj* %args46871$anf_45bind40306$1)
store volatile %struct.ScmObj* %args46871$anf_45bind40306$2, %struct.ScmObj** %stackaddr$prim48073, align 8
%clofunc48074 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40306)
musttail call tailcc void %clofunc48074(%struct.ScmObj* %anf_45bind40306, %struct.ScmObj* %args46871$anf_45bind40306$2)
ret void
}

define tailcc void @proc_clo$ae42401(%struct.ScmObj* %env$ae42401,%struct.ScmObj* %current_45args46859) {
%stackaddr$env-ref48075 = alloca %struct.ScmObj*, align 8
%a40189 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42401, i64 0)
store %struct.ScmObj* %a40189, %struct.ScmObj** %stackaddr$env-ref48075
%stackaddr$env-ref48076 = alloca %struct.ScmObj*, align 8
%k40443 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42401, i64 1)
store %struct.ScmObj* %k40443, %struct.ScmObj** %stackaddr$env-ref48076
%stackaddr$prim48077 = alloca %struct.ScmObj*, align 8
%_95k40445 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46859)
store volatile %struct.ScmObj* %_95k40445, %struct.ScmObj** %stackaddr$prim48077, align 8
%stackaddr$prim48078 = alloca %struct.ScmObj*, align 8
%current_45args46860 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46859)
store volatile %struct.ScmObj* %current_45args46860, %struct.ScmObj** %stackaddr$prim48078, align 8
%stackaddr$prim48079 = alloca %struct.ScmObj*, align 8
%cc40190 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46860)
store volatile %struct.ScmObj* %cc40190, %struct.ScmObj** %stackaddr$prim48079, align 8
%ae42517 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48080 = alloca %struct.ScmObj*, align 8
%anf_45bind40307 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42517)
store volatile %struct.ScmObj* %anf_45bind40307, %struct.ScmObj** %stackaddr$prim48080, align 8
%stackaddr$prim48081 = alloca %struct.ScmObj*, align 8
%anf_45bind40308 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40307)
store volatile %struct.ScmObj* %anf_45bind40308, %struct.ScmObj** %stackaddr$prim48081, align 8
%truthy$cmp48082 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40308)
%cmp$cmp48082 = icmp eq i64 %truthy$cmp48082, 1
br i1 %cmp$cmp48082, label %truebranch$cmp48082, label %falsebranch$cmp48082
truebranch$cmp48082:
%ae42521 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42522 = call %struct.ScmObj* @const_init_true()
%args46862$k40443$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48083 = alloca %struct.ScmObj*, align 8
%args46862$k40443$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42522, %struct.ScmObj* %args46862$k40443$0)
store volatile %struct.ScmObj* %args46862$k40443$1, %struct.ScmObj** %stackaddr$prim48083, align 8
%stackaddr$prim48084 = alloca %struct.ScmObj*, align 8
%args46862$k40443$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42521, %struct.ScmObj* %args46862$k40443$1)
store volatile %struct.ScmObj* %args46862$k40443$2, %struct.ScmObj** %stackaddr$prim48084, align 8
%clofunc48085 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40443)
musttail call tailcc void %clofunc48085(%struct.ScmObj* %k40443, %struct.ScmObj* %args46862$k40443$2)
ret void
falsebranch$cmp48082:
%ae42530 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48086 = alloca %struct.ScmObj*, align 8
%anf_45bind40309 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42530)
store volatile %struct.ScmObj* %anf_45bind40309, %struct.ScmObj** %stackaddr$prim48086, align 8
%stackaddr$prim48087 = alloca %struct.ScmObj*, align 8
%anf_45bind40310 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind40309)
store volatile %struct.ScmObj* %anf_45bind40310, %struct.ScmObj** %stackaddr$prim48087, align 8
%truthy$cmp48088 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40310)
%cmp$cmp48088 = icmp eq i64 %truthy$cmp48088, 1
br i1 %cmp$cmp48088, label %truebranch$cmp48088, label %falsebranch$cmp48088
truebranch$cmp48088:
%ae42534 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48089 = alloca %struct.ScmObj*, align 8
%anf_45bind40311 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42534)
store volatile %struct.ScmObj* %anf_45bind40311, %struct.ScmObj** %stackaddr$prim48089, align 8
%stackaddr$prim48090 = alloca %struct.ScmObj*, align 8
%b40192 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40311)
store volatile %struct.ScmObj* %b40192, %struct.ScmObj** %stackaddr$prim48090, align 8
%ae42537 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48091 = alloca %struct.ScmObj*, align 8
%anf_45bind40312 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42537)
store volatile %struct.ScmObj* %anf_45bind40312, %struct.ScmObj** %stackaddr$prim48091, align 8
%stackaddr$prim48092 = alloca %struct.ScmObj*, align 8
%anf_45bind40313 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40312)
store volatile %struct.ScmObj* %anf_45bind40313, %struct.ScmObj** %stackaddr$prim48092, align 8
%ae42540 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48093 = alloca %struct.ScmObj*, align 8
%_95040193 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42540, %struct.ScmObj* %anf_45bind40313)
store volatile %struct.ScmObj* %_95040193, %struct.ScmObj** %stackaddr$prim48093, align 8
%args46863$cc40190$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48094 = alloca %struct.ScmObj*, align 8
%args46863$cc40190$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40190, %struct.ScmObj* %args46863$cc40190$0)
store volatile %struct.ScmObj* %args46863$cc40190$1, %struct.ScmObj** %stackaddr$prim48094, align 8
%stackaddr$prim48095 = alloca %struct.ScmObj*, align 8
%args46863$cc40190$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40443, %struct.ScmObj* %args46863$cc40190$1)
store volatile %struct.ScmObj* %args46863$cc40190$2, %struct.ScmObj** %stackaddr$prim48095, align 8
%clofunc48096 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40190)
musttail call tailcc void %clofunc48096(%struct.ScmObj* %cc40190, %struct.ScmObj* %args46863$cc40190$2)
ret void
falsebranch$cmp48088:
%ae42573 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42574 = call %struct.ScmObj* @const_init_false()
%args46864$k40443$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48097 = alloca %struct.ScmObj*, align 8
%args46864$k40443$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42574, %struct.ScmObj* %args46864$k40443$0)
store volatile %struct.ScmObj* %args46864$k40443$1, %struct.ScmObj** %stackaddr$prim48097, align 8
%stackaddr$prim48098 = alloca %struct.ScmObj*, align 8
%args46864$k40443$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42573, %struct.ScmObj* %args46864$k40443$1)
store volatile %struct.ScmObj* %args46864$k40443$2, %struct.ScmObj** %stackaddr$prim48098, align 8
%clofunc48099 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40443)
musttail call tailcc void %clofunc48099(%struct.ScmObj* %k40443, %struct.ScmObj* %args46864$k40443$2)
ret void
}

define tailcc void @proc_clo$ae42402(%struct.ScmObj* %env$ae42402,%struct.ScmObj* %current_45args46865) {
%stackaddr$env-ref48100 = alloca %struct.ScmObj*, align 8
%a40189 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42402, i64 0)
store %struct.ScmObj* %a40189, %struct.ScmObj** %stackaddr$env-ref48100
%stackaddr$env-ref48101 = alloca %struct.ScmObj*, align 8
%k40443 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42402, i64 1)
store %struct.ScmObj* %k40443, %struct.ScmObj** %stackaddr$env-ref48101
%stackaddr$prim48102 = alloca %struct.ScmObj*, align 8
%_95k40445 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46865)
store volatile %struct.ScmObj* %_95k40445, %struct.ScmObj** %stackaddr$prim48102, align 8
%stackaddr$prim48103 = alloca %struct.ScmObj*, align 8
%current_45args46866 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46865)
store volatile %struct.ScmObj* %current_45args46866, %struct.ScmObj** %stackaddr$prim48103, align 8
%stackaddr$prim48104 = alloca %struct.ScmObj*, align 8
%cc40190 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46866)
store volatile %struct.ScmObj* %cc40190, %struct.ScmObj** %stackaddr$prim48104, align 8
%ae42404 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48105 = alloca %struct.ScmObj*, align 8
%anf_45bind40307 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42404)
store volatile %struct.ScmObj* %anf_45bind40307, %struct.ScmObj** %stackaddr$prim48105, align 8
%stackaddr$prim48106 = alloca %struct.ScmObj*, align 8
%anf_45bind40308 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40307)
store volatile %struct.ScmObj* %anf_45bind40308, %struct.ScmObj** %stackaddr$prim48106, align 8
%truthy$cmp48107 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40308)
%cmp$cmp48107 = icmp eq i64 %truthy$cmp48107, 1
br i1 %cmp$cmp48107, label %truebranch$cmp48107, label %falsebranch$cmp48107
truebranch$cmp48107:
%ae42408 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42409 = call %struct.ScmObj* @const_init_true()
%args46868$k40443$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48108 = alloca %struct.ScmObj*, align 8
%args46868$k40443$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42409, %struct.ScmObj* %args46868$k40443$0)
store volatile %struct.ScmObj* %args46868$k40443$1, %struct.ScmObj** %stackaddr$prim48108, align 8
%stackaddr$prim48109 = alloca %struct.ScmObj*, align 8
%args46868$k40443$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42408, %struct.ScmObj* %args46868$k40443$1)
store volatile %struct.ScmObj* %args46868$k40443$2, %struct.ScmObj** %stackaddr$prim48109, align 8
%clofunc48110 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40443)
musttail call tailcc void %clofunc48110(%struct.ScmObj* %k40443, %struct.ScmObj* %args46868$k40443$2)
ret void
falsebranch$cmp48107:
%ae42417 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48111 = alloca %struct.ScmObj*, align 8
%anf_45bind40309 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42417)
store volatile %struct.ScmObj* %anf_45bind40309, %struct.ScmObj** %stackaddr$prim48111, align 8
%stackaddr$prim48112 = alloca %struct.ScmObj*, align 8
%anf_45bind40310 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind40309)
store volatile %struct.ScmObj* %anf_45bind40310, %struct.ScmObj** %stackaddr$prim48112, align 8
%truthy$cmp48113 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40310)
%cmp$cmp48113 = icmp eq i64 %truthy$cmp48113, 1
br i1 %cmp$cmp48113, label %truebranch$cmp48113, label %falsebranch$cmp48113
truebranch$cmp48113:
%ae42421 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48114 = alloca %struct.ScmObj*, align 8
%anf_45bind40311 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42421)
store volatile %struct.ScmObj* %anf_45bind40311, %struct.ScmObj** %stackaddr$prim48114, align 8
%stackaddr$prim48115 = alloca %struct.ScmObj*, align 8
%b40192 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40311)
store volatile %struct.ScmObj* %b40192, %struct.ScmObj** %stackaddr$prim48115, align 8
%ae42424 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48116 = alloca %struct.ScmObj*, align 8
%anf_45bind40312 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42424)
store volatile %struct.ScmObj* %anf_45bind40312, %struct.ScmObj** %stackaddr$prim48116, align 8
%stackaddr$prim48117 = alloca %struct.ScmObj*, align 8
%anf_45bind40313 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40312)
store volatile %struct.ScmObj* %anf_45bind40313, %struct.ScmObj** %stackaddr$prim48117, align 8
%ae42427 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48118 = alloca %struct.ScmObj*, align 8
%_95040193 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42427, %struct.ScmObj* %anf_45bind40313)
store volatile %struct.ScmObj* %_95040193, %struct.ScmObj** %stackaddr$prim48118, align 8
%args46869$cc40190$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48119 = alloca %struct.ScmObj*, align 8
%args46869$cc40190$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40190, %struct.ScmObj* %args46869$cc40190$0)
store volatile %struct.ScmObj* %args46869$cc40190$1, %struct.ScmObj** %stackaddr$prim48119, align 8
%stackaddr$prim48120 = alloca %struct.ScmObj*, align 8
%args46869$cc40190$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40443, %struct.ScmObj* %args46869$cc40190$1)
store volatile %struct.ScmObj* %args46869$cc40190$2, %struct.ScmObj** %stackaddr$prim48120, align 8
%clofunc48121 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40190)
musttail call tailcc void %clofunc48121(%struct.ScmObj* %cc40190, %struct.ScmObj* %args46869$cc40190$2)
ret void
falsebranch$cmp48113:
%ae42460 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42461 = call %struct.ScmObj* @const_init_false()
%args46870$k40443$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48122 = alloca %struct.ScmObj*, align 8
%args46870$k40443$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42461, %struct.ScmObj* %args46870$k40443$0)
store volatile %struct.ScmObj* %args46870$k40443$1, %struct.ScmObj** %stackaddr$prim48122, align 8
%stackaddr$prim48123 = alloca %struct.ScmObj*, align 8
%args46870$k40443$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42460, %struct.ScmObj* %args46870$k40443$1)
store volatile %struct.ScmObj* %args46870$k40443$2, %struct.ScmObj** %stackaddr$prim48123, align 8
%clofunc48124 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40443)
musttail call tailcc void %clofunc48124(%struct.ScmObj* %k40443, %struct.ScmObj* %args46870$k40443$2)
ret void
}

define tailcc void @proc_clo$ae42384(%struct.ScmObj* %env$ae42384,%struct.ScmObj* %current_45args46872) {
%stackaddr$prim48125 = alloca %struct.ScmObj*, align 8
%k40446 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46872)
store volatile %struct.ScmObj* %k40446, %struct.ScmObj** %stackaddr$prim48125, align 8
%stackaddr$prim48126 = alloca %struct.ScmObj*, align 8
%current_45args46873 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46872)
store volatile %struct.ScmObj* %current_45args46873, %struct.ScmObj** %stackaddr$prim48126, align 8
%stackaddr$prim48127 = alloca %struct.ScmObj*, align 8
%k40191 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46873)
store volatile %struct.ScmObj* %k40191, %struct.ScmObj** %stackaddr$prim48127, align 8
%ae42386 = call %struct.ScmObj* @const_init_int(i64 0)
%args46875$k40446$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48128 = alloca %struct.ScmObj*, align 8
%args46875$k40446$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40191, %struct.ScmObj* %args46875$k40446$0)
store volatile %struct.ScmObj* %args46875$k40446$1, %struct.ScmObj** %stackaddr$prim48128, align 8
%stackaddr$prim48129 = alloca %struct.ScmObj*, align 8
%args46875$k40446$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42386, %struct.ScmObj* %args46875$k40446$1)
store volatile %struct.ScmObj* %args46875$k40446$2, %struct.ScmObj** %stackaddr$prim48129, align 8
%clofunc48130 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40446)
musttail call tailcc void %clofunc48130(%struct.ScmObj* %k40446, %struct.ScmObj* %args46875$k40446$2)
ret void
}

define tailcc void @proc_clo$ae42307(%struct.ScmObj* %env$ae42307,%struct.ScmObj* %current_45args46878) {
%stackaddr$env-ref48131 = alloca %struct.ScmObj*, align 8
%_37append40195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42307, i64 0)
store %struct.ScmObj* %_37append40195, %struct.ScmObj** %stackaddr$env-ref48131
%stackaddr$prim48132 = alloca %struct.ScmObj*, align 8
%k40447 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46878)
store volatile %struct.ScmObj* %k40447, %struct.ScmObj** %stackaddr$prim48132, align 8
%stackaddr$prim48133 = alloca %struct.ScmObj*, align 8
%current_45args46879 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46878)
store volatile %struct.ScmObj* %current_45args46879, %struct.ScmObj** %stackaddr$prim48133, align 8
%stackaddr$prim48134 = alloca %struct.ScmObj*, align 8
%ls040198 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46879)
store volatile %struct.ScmObj* %ls040198, %struct.ScmObj** %stackaddr$prim48134, align 8
%stackaddr$prim48135 = alloca %struct.ScmObj*, align 8
%current_45args46880 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46879)
store volatile %struct.ScmObj* %current_45args46880, %struct.ScmObj** %stackaddr$prim48135, align 8
%stackaddr$prim48136 = alloca %struct.ScmObj*, align 8
%ls140197 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46880)
store volatile %struct.ScmObj* %ls140197, %struct.ScmObj** %stackaddr$prim48136, align 8
%stackaddr$prim48137 = alloca %struct.ScmObj*, align 8
%anf_45bind40300 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls040198)
store volatile %struct.ScmObj* %anf_45bind40300, %struct.ScmObj** %stackaddr$prim48137, align 8
%truthy$cmp48138 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40300)
%cmp$cmp48138 = icmp eq i64 %truthy$cmp48138, 1
br i1 %cmp$cmp48138, label %truebranch$cmp48138, label %falsebranch$cmp48138
truebranch$cmp48138:
%ae42311 = call %struct.ScmObj* @const_init_int(i64 0)
%args46882$k40447$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48139 = alloca %struct.ScmObj*, align 8
%args46882$k40447$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls140197, %struct.ScmObj* %args46882$k40447$0)
store volatile %struct.ScmObj* %args46882$k40447$1, %struct.ScmObj** %stackaddr$prim48139, align 8
%stackaddr$prim48140 = alloca %struct.ScmObj*, align 8
%args46882$k40447$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42311, %struct.ScmObj* %args46882$k40447$1)
store volatile %struct.ScmObj* %args46882$k40447$2, %struct.ScmObj** %stackaddr$prim48140, align 8
%clofunc48141 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40447)
musttail call tailcc void %clofunc48141(%struct.ScmObj* %k40447, %struct.ScmObj* %args46882$k40447$2)
ret void
falsebranch$cmp48138:
%stackaddr$prim48142 = alloca %struct.ScmObj*, align 8
%anf_45bind40301 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls040198)
store volatile %struct.ScmObj* %anf_45bind40301, %struct.ScmObj** %stackaddr$prim48142, align 8
%ae42318 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48143 = alloca %struct.ScmObj*, align 8
%anf_45bind40302 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append40195, %struct.ScmObj* %ae42318)
store volatile %struct.ScmObj* %anf_45bind40302, %struct.ScmObj** %stackaddr$prim48143, align 8
%stackaddr$prim48144 = alloca %struct.ScmObj*, align 8
%anf_45bind40303 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls040198)
store volatile %struct.ScmObj* %anf_45bind40303, %struct.ScmObj** %stackaddr$prim48144, align 8
%stackaddr$makeclosure48145 = alloca %struct.ScmObj*, align 8
%fptrToInt48146 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42321 to i64
%ae42321 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48146)
store volatile %struct.ScmObj* %ae42321, %struct.ScmObj** %stackaddr$makeclosure48145, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42321, %struct.ScmObj* %k40447, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42321, %struct.ScmObj* %anf_45bind40301, i64 1)
%args46887$anf_45bind40302$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48147 = alloca %struct.ScmObj*, align 8
%args46887$anf_45bind40302$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls140197, %struct.ScmObj* %args46887$anf_45bind40302$0)
store volatile %struct.ScmObj* %args46887$anf_45bind40302$1, %struct.ScmObj** %stackaddr$prim48147, align 8
%stackaddr$prim48148 = alloca %struct.ScmObj*, align 8
%args46887$anf_45bind40302$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40303, %struct.ScmObj* %args46887$anf_45bind40302$1)
store volatile %struct.ScmObj* %args46887$anf_45bind40302$2, %struct.ScmObj** %stackaddr$prim48148, align 8
%stackaddr$prim48149 = alloca %struct.ScmObj*, align 8
%args46887$anf_45bind40302$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42321, %struct.ScmObj* %args46887$anf_45bind40302$2)
store volatile %struct.ScmObj* %args46887$anf_45bind40302$3, %struct.ScmObj** %stackaddr$prim48149, align 8
%clofunc48150 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40302)
musttail call tailcc void %clofunc48150(%struct.ScmObj* %anf_45bind40302, %struct.ScmObj* %args46887$anf_45bind40302$3)
ret void
}

define tailcc void @proc_clo$ae42321(%struct.ScmObj* %env$ae42321,%struct.ScmObj* %current_45args46883) {
%stackaddr$env-ref48151 = alloca %struct.ScmObj*, align 8
%k40447 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42321, i64 0)
store %struct.ScmObj* %k40447, %struct.ScmObj** %stackaddr$env-ref48151
%stackaddr$env-ref48152 = alloca %struct.ScmObj*, align 8
%anf_45bind40301 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42321, i64 1)
store %struct.ScmObj* %anf_45bind40301, %struct.ScmObj** %stackaddr$env-ref48152
%stackaddr$prim48153 = alloca %struct.ScmObj*, align 8
%_95k40448 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46883)
store volatile %struct.ScmObj* %_95k40448, %struct.ScmObj** %stackaddr$prim48153, align 8
%stackaddr$prim48154 = alloca %struct.ScmObj*, align 8
%current_45args46884 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46883)
store volatile %struct.ScmObj* %current_45args46884, %struct.ScmObj** %stackaddr$prim48154, align 8
%stackaddr$prim48155 = alloca %struct.ScmObj*, align 8
%anf_45bind40304 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46884)
store volatile %struct.ScmObj* %anf_45bind40304, %struct.ScmObj** %stackaddr$prim48155, align 8
%stackaddr$prim48156 = alloca %struct.ScmObj*, align 8
%cpsprim40449 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40301, %struct.ScmObj* %anf_45bind40304)
store volatile %struct.ScmObj* %cpsprim40449, %struct.ScmObj** %stackaddr$prim48156, align 8
%ae42327 = call %struct.ScmObj* @const_init_int(i64 0)
%args46886$k40447$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48157 = alloca %struct.ScmObj*, align 8
%args46886$k40447$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40449, %struct.ScmObj* %args46886$k40447$0)
store volatile %struct.ScmObj* %args46886$k40447$1, %struct.ScmObj** %stackaddr$prim48157, align 8
%stackaddr$prim48158 = alloca %struct.ScmObj*, align 8
%args46886$k40447$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42327, %struct.ScmObj* %args46886$k40447$1)
store volatile %struct.ScmObj* %args46886$k40447$2, %struct.ScmObj** %stackaddr$prim48158, align 8
%clofunc48159 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40447)
musttail call tailcc void %clofunc48159(%struct.ScmObj* %k40447, %struct.ScmObj* %args46886$k40447$2)
ret void
}

define tailcc void @proc_clo$ae42281(%struct.ScmObj* %env$ae42281,%struct.ScmObj* %current_45args46889) {
%stackaddr$prim48160 = alloca %struct.ScmObj*, align 8
%k40450 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46889)
store volatile %struct.ScmObj* %k40450, %struct.ScmObj** %stackaddr$prim48160, align 8
%stackaddr$prim48161 = alloca %struct.ScmObj*, align 8
%current_45args46890 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46889)
store volatile %struct.ScmObj* %current_45args46890, %struct.ScmObj** %stackaddr$prim48161, align 8
%stackaddr$prim48162 = alloca %struct.ScmObj*, align 8
%a40201 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46890)
store volatile %struct.ScmObj* %a40201, %struct.ScmObj** %stackaddr$prim48162, align 8
%stackaddr$prim48163 = alloca %struct.ScmObj*, align 8
%current_45args46891 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46890)
store volatile %struct.ScmObj* %current_45args46891, %struct.ScmObj** %stackaddr$prim48163, align 8
%stackaddr$prim48164 = alloca %struct.ScmObj*, align 8
%b40200 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46891)
store volatile %struct.ScmObj* %b40200, %struct.ScmObj** %stackaddr$prim48164, align 8
%stackaddr$prim48165 = alloca %struct.ScmObj*, align 8
%anf_45bind40299 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a40201, %struct.ScmObj* %b40200)
store volatile %struct.ScmObj* %anf_45bind40299, %struct.ScmObj** %stackaddr$prim48165, align 8
%stackaddr$prim48166 = alloca %struct.ScmObj*, align 8
%cpsprim40451 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind40299)
store volatile %struct.ScmObj* %cpsprim40451, %struct.ScmObj** %stackaddr$prim48166, align 8
%ae42286 = call %struct.ScmObj* @const_init_int(i64 0)
%args46893$k40450$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48167 = alloca %struct.ScmObj*, align 8
%args46893$k40450$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40451, %struct.ScmObj* %args46893$k40450$0)
store volatile %struct.ScmObj* %args46893$k40450$1, %struct.ScmObj** %stackaddr$prim48167, align 8
%stackaddr$prim48168 = alloca %struct.ScmObj*, align 8
%args46893$k40450$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42286, %struct.ScmObj* %args46893$k40450$1)
store volatile %struct.ScmObj* %args46893$k40450$2, %struct.ScmObj** %stackaddr$prim48168, align 8
%clofunc48169 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40450)
musttail call tailcc void %clofunc48169(%struct.ScmObj* %k40450, %struct.ScmObj* %args46893$k40450$2)
ret void
}

define tailcc void @proc_clo$ae42257(%struct.ScmObj* %env$ae42257,%struct.ScmObj* %current_45args46895) {
%stackaddr$prim48170 = alloca %struct.ScmObj*, align 8
%k40452 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46895)
store volatile %struct.ScmObj* %k40452, %struct.ScmObj** %stackaddr$prim48170, align 8
%stackaddr$prim48171 = alloca %struct.ScmObj*, align 8
%current_45args46896 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46895)
store volatile %struct.ScmObj* %current_45args46896, %struct.ScmObj** %stackaddr$prim48171, align 8
%stackaddr$prim48172 = alloca %struct.ScmObj*, align 8
%a40204 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46896)
store volatile %struct.ScmObj* %a40204, %struct.ScmObj** %stackaddr$prim48172, align 8
%stackaddr$prim48173 = alloca %struct.ScmObj*, align 8
%current_45args46897 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46896)
store volatile %struct.ScmObj* %current_45args46897, %struct.ScmObj** %stackaddr$prim48173, align 8
%stackaddr$prim48174 = alloca %struct.ScmObj*, align 8
%b40203 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46897)
store volatile %struct.ScmObj* %b40203, %struct.ScmObj** %stackaddr$prim48174, align 8
%stackaddr$prim48175 = alloca %struct.ScmObj*, align 8
%anf_45bind40298 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a40204, %struct.ScmObj* %b40203)
store volatile %struct.ScmObj* %anf_45bind40298, %struct.ScmObj** %stackaddr$prim48175, align 8
%stackaddr$prim48176 = alloca %struct.ScmObj*, align 8
%cpsprim40453 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind40298)
store volatile %struct.ScmObj* %cpsprim40453, %struct.ScmObj** %stackaddr$prim48176, align 8
%ae42262 = call %struct.ScmObj* @const_init_int(i64 0)
%args46899$k40452$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48177 = alloca %struct.ScmObj*, align 8
%args46899$k40452$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40453, %struct.ScmObj* %args46899$k40452$0)
store volatile %struct.ScmObj* %args46899$k40452$1, %struct.ScmObj** %stackaddr$prim48177, align 8
%stackaddr$prim48178 = alloca %struct.ScmObj*, align 8
%args46899$k40452$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42262, %struct.ScmObj* %args46899$k40452$1)
store volatile %struct.ScmObj* %args46899$k40452$2, %struct.ScmObj** %stackaddr$prim48178, align 8
%clofunc48179 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40452)
musttail call tailcc void %clofunc48179(%struct.ScmObj* %k40452, %struct.ScmObj* %args46899$k40452$2)
ret void
}

define tailcc void @proc_clo$ae41863(%struct.ScmObj* %env$ae41863,%struct.ScmObj* %current_45args46902) {
%stackaddr$env-ref48180 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41863, i64 0)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref48180
%stackaddr$env-ref48181 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41863, i64 1)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48181
%stackaddr$env-ref48182 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41863, i64 2)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref48182
%stackaddr$prim48183 = alloca %struct.ScmObj*, align 8
%k40454 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46902)
store volatile %struct.ScmObj* %k40454, %struct.ScmObj** %stackaddr$prim48183, align 8
%stackaddr$prim48184 = alloca %struct.ScmObj*, align 8
%current_45args46903 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46902)
store volatile %struct.ScmObj* %current_45args46903, %struct.ScmObj** %stackaddr$prim48184, align 8
%stackaddr$prim48185 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46903)
store volatile %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$prim48185, align 8
%ae41865 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48186 = alloca %struct.ScmObj*, align 8
%fptrToInt48187 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41866 to i64
%ae41866 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48187)
store volatile %struct.ScmObj* %ae41866, %struct.ScmObj** %stackaddr$makeclosure48186, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41866, %struct.ScmObj* %_37foldr40128, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41866, %struct.ScmObj* %_37foldl40206, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41866, %struct.ScmObj* %_37foldr140123, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41866, %struct.ScmObj* %_37map140154, i64 3)
%args46960$k40454$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48188 = alloca %struct.ScmObj*, align 8
%args46960$k40454$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41866, %struct.ScmObj* %args46960$k40454$0)
store volatile %struct.ScmObj* %args46960$k40454$1, %struct.ScmObj** %stackaddr$prim48188, align 8
%stackaddr$prim48189 = alloca %struct.ScmObj*, align 8
%args46960$k40454$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41865, %struct.ScmObj* %args46960$k40454$1)
store volatile %struct.ScmObj* %args46960$k40454$2, %struct.ScmObj** %stackaddr$prim48189, align 8
%clofunc48190 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40454)
musttail call tailcc void %clofunc48190(%struct.ScmObj* %k40454, %struct.ScmObj* %args46960$k40454$2)
ret void
}

define tailcc void @proc_clo$ae41866(%struct.ScmObj* %env$ae41866,%struct.ScmObj* %args4020740455) {
%stackaddr$env-ref48191 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41866, i64 0)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref48191
%stackaddr$env-ref48192 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41866, i64 1)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48192
%stackaddr$env-ref48193 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41866, i64 2)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48193
%stackaddr$env-ref48194 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41866, i64 3)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref48194
%stackaddr$prim48195 = alloca %struct.ScmObj*, align 8
%k40456 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4020740455)
store volatile %struct.ScmObj* %k40456, %struct.ScmObj** %stackaddr$prim48195, align 8
%stackaddr$prim48196 = alloca %struct.ScmObj*, align 8
%args40207 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4020740455)
store volatile %struct.ScmObj* %args40207, %struct.ScmObj** %stackaddr$prim48196, align 8
%stackaddr$prim48197 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40207)
store volatile %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$prim48197, align 8
%stackaddr$prim48198 = alloca %struct.ScmObj*, align 8
%anf_45bind40286 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40207)
store volatile %struct.ScmObj* %anf_45bind40286, %struct.ScmObj** %stackaddr$prim48198, align 8
%stackaddr$prim48199 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40286)
store volatile %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$prim48199, align 8
%stackaddr$prim48200 = alloca %struct.ScmObj*, align 8
%anf_45bind40287 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40207)
store volatile %struct.ScmObj* %anf_45bind40287, %struct.ScmObj** %stackaddr$prim48200, align 8
%stackaddr$prim48201 = alloca %struct.ScmObj*, align 8
%lsts40208 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40287)
store volatile %struct.ScmObj* %lsts40208, %struct.ScmObj** %stackaddr$prim48201, align 8
%stackaddr$makeclosure48202 = alloca %struct.ScmObj*, align 8
%fptrToInt48203 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41874 to i64
%ae41874 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt48203)
store volatile %struct.ScmObj* %ae41874, %struct.ScmObj** %stackaddr$makeclosure48202, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41874, %struct.ScmObj* %lsts40208, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41874, %struct.ScmObj* %_37foldr40128, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41874, %struct.ScmObj* %f40210, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41874, %struct.ScmObj* %acc40209, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41874, %struct.ScmObj* %_37foldl40206, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41874, %struct.ScmObj* %_37foldr140123, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41874, %struct.ScmObj* %_37map140154, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae41874, %struct.ScmObj* %k40456, i64 7)
%ae41875 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48204 = alloca %struct.ScmObj*, align 8
%fptrToInt48205 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41876 to i64
%ae41876 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48205)
store volatile %struct.ScmObj* %ae41876, %struct.ScmObj** %stackaddr$makeclosure48204, align 8
%args46959$ae41874$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48206 = alloca %struct.ScmObj*, align 8
%args46959$ae41874$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41876, %struct.ScmObj* %args46959$ae41874$0)
store volatile %struct.ScmObj* %args46959$ae41874$1, %struct.ScmObj** %stackaddr$prim48206, align 8
%stackaddr$prim48207 = alloca %struct.ScmObj*, align 8
%args46959$ae41874$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41875, %struct.ScmObj* %args46959$ae41874$1)
store volatile %struct.ScmObj* %args46959$ae41874$2, %struct.ScmObj** %stackaddr$prim48207, align 8
%clofunc48208 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41874)
musttail call tailcc void %clofunc48208(%struct.ScmObj* %ae41874, %struct.ScmObj* %args46959$ae41874$2)
ret void
}

define tailcc void @proc_clo$ae41874(%struct.ScmObj* %env$ae41874,%struct.ScmObj* %current_45args46905) {
%stackaddr$env-ref48209 = alloca %struct.ScmObj*, align 8
%lsts40208 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41874, i64 0)
store %struct.ScmObj* %lsts40208, %struct.ScmObj** %stackaddr$env-ref48209
%stackaddr$env-ref48210 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41874, i64 1)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref48210
%stackaddr$env-ref48211 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41874, i64 2)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref48211
%stackaddr$env-ref48212 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41874, i64 3)
store %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$env-ref48212
%stackaddr$env-ref48213 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41874, i64 4)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48213
%stackaddr$env-ref48214 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41874, i64 5)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48214
%stackaddr$env-ref48215 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41874, i64 6)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref48215
%stackaddr$env-ref48216 = alloca %struct.ScmObj*, align 8
%k40456 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41874, i64 7)
store %struct.ScmObj* %k40456, %struct.ScmObj** %stackaddr$env-ref48216
%stackaddr$prim48217 = alloca %struct.ScmObj*, align 8
%_95k40457 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46905)
store volatile %struct.ScmObj* %_95k40457, %struct.ScmObj** %stackaddr$prim48217, align 8
%stackaddr$prim48218 = alloca %struct.ScmObj*, align 8
%current_45args46906 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46905)
store volatile %struct.ScmObj* %current_45args46906, %struct.ScmObj** %stackaddr$prim48218, align 8
%stackaddr$prim48219 = alloca %struct.ScmObj*, align 8
%anf_45bind40288 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46906)
store volatile %struct.ScmObj* %anf_45bind40288, %struct.ScmObj** %stackaddr$prim48219, align 8
%stackaddr$makeclosure48220 = alloca %struct.ScmObj*, align 8
%fptrToInt48221 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41906 to i64
%ae41906 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt48221)
store volatile %struct.ScmObj* %ae41906, %struct.ScmObj** %stackaddr$makeclosure48220, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41906, %struct.ScmObj* %lsts40208, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41906, %struct.ScmObj* %_37foldr40128, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41906, %struct.ScmObj* %f40210, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41906, %struct.ScmObj* %acc40209, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41906, %struct.ScmObj* %_37foldl40206, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41906, %struct.ScmObj* %_37map140154, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41906, %struct.ScmObj* %k40456, i64 6)
%ae41908 = call %struct.ScmObj* @const_init_false()
%args46952$_37foldr140123$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48222 = alloca %struct.ScmObj*, align 8
%args46952$_37foldr140123$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40208, %struct.ScmObj* %args46952$_37foldr140123$0)
store volatile %struct.ScmObj* %args46952$_37foldr140123$1, %struct.ScmObj** %stackaddr$prim48222, align 8
%stackaddr$prim48223 = alloca %struct.ScmObj*, align 8
%args46952$_37foldr140123$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41908, %struct.ScmObj* %args46952$_37foldr140123$1)
store volatile %struct.ScmObj* %args46952$_37foldr140123$2, %struct.ScmObj** %stackaddr$prim48223, align 8
%stackaddr$prim48224 = alloca %struct.ScmObj*, align 8
%args46952$_37foldr140123$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40288, %struct.ScmObj* %args46952$_37foldr140123$2)
store volatile %struct.ScmObj* %args46952$_37foldr140123$3, %struct.ScmObj** %stackaddr$prim48224, align 8
%stackaddr$prim48225 = alloca %struct.ScmObj*, align 8
%args46952$_37foldr140123$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41906, %struct.ScmObj* %args46952$_37foldr140123$3)
store volatile %struct.ScmObj* %args46952$_37foldr140123$4, %struct.ScmObj** %stackaddr$prim48225, align 8
%clofunc48226 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140123)
musttail call tailcc void %clofunc48226(%struct.ScmObj* %_37foldr140123, %struct.ScmObj* %args46952$_37foldr140123$4)
ret void
}

define tailcc void @proc_clo$ae41906(%struct.ScmObj* %env$ae41906,%struct.ScmObj* %current_45args46908) {
%stackaddr$env-ref48227 = alloca %struct.ScmObj*, align 8
%lsts40208 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41906, i64 0)
store %struct.ScmObj* %lsts40208, %struct.ScmObj** %stackaddr$env-ref48227
%stackaddr$env-ref48228 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41906, i64 1)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref48228
%stackaddr$env-ref48229 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41906, i64 2)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref48229
%stackaddr$env-ref48230 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41906, i64 3)
store %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$env-ref48230
%stackaddr$env-ref48231 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41906, i64 4)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48231
%stackaddr$env-ref48232 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41906, i64 5)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref48232
%stackaddr$env-ref48233 = alloca %struct.ScmObj*, align 8
%k40456 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41906, i64 6)
store %struct.ScmObj* %k40456, %struct.ScmObj** %stackaddr$env-ref48233
%stackaddr$prim48234 = alloca %struct.ScmObj*, align 8
%_95k40458 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46908)
store volatile %struct.ScmObj* %_95k40458, %struct.ScmObj** %stackaddr$prim48234, align 8
%stackaddr$prim48235 = alloca %struct.ScmObj*, align 8
%current_45args46909 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46908)
store volatile %struct.ScmObj* %current_45args46909, %struct.ScmObj** %stackaddr$prim48235, align 8
%stackaddr$prim48236 = alloca %struct.ScmObj*, align 8
%anf_45bind40289 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46909)
store volatile %struct.ScmObj* %anf_45bind40289, %struct.ScmObj** %stackaddr$prim48236, align 8
%truthy$cmp48237 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40289)
%cmp$cmp48237 = icmp eq i64 %truthy$cmp48237, 1
br i1 %cmp$cmp48237, label %truebranch$cmp48237, label %falsebranch$cmp48237
truebranch$cmp48237:
%ae41917 = call %struct.ScmObj* @const_init_int(i64 0)
%args46911$k40456$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48238 = alloca %struct.ScmObj*, align 8
%args46911$k40456$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40209, %struct.ScmObj* %args46911$k40456$0)
store volatile %struct.ScmObj* %args46911$k40456$1, %struct.ScmObj** %stackaddr$prim48238, align 8
%stackaddr$prim48239 = alloca %struct.ScmObj*, align 8
%args46911$k40456$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41917, %struct.ScmObj* %args46911$k40456$1)
store volatile %struct.ScmObj* %args46911$k40456$2, %struct.ScmObj** %stackaddr$prim48239, align 8
%clofunc48240 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40456)
musttail call tailcc void %clofunc48240(%struct.ScmObj* %k40456, %struct.ScmObj* %args46911$k40456$2)
ret void
falsebranch$cmp48237:
%stackaddr$makeclosure48241 = alloca %struct.ScmObj*, align 8
%fptrToInt48242 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41922 to i64
%ae41922 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt48242)
store volatile %struct.ScmObj* %ae41922, %struct.ScmObj** %stackaddr$makeclosure48241, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41922, %struct.ScmObj* %lsts40208, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41922, %struct.ScmObj* %_37foldr40128, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41922, %struct.ScmObj* %f40210, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41922, %struct.ScmObj* %acc40209, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41922, %struct.ScmObj* %_37foldl40206, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41922, %struct.ScmObj* %_37map140154, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41922, %struct.ScmObj* %k40456, i64 6)
%ae41923 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48243 = alloca %struct.ScmObj*, align 8
%fptrToInt48244 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41924 to i64
%ae41924 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48244)
store volatile %struct.ScmObj* %ae41924, %struct.ScmObj** %stackaddr$makeclosure48243, align 8
%args46951$ae41922$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48245 = alloca %struct.ScmObj*, align 8
%args46951$ae41922$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41924, %struct.ScmObj* %args46951$ae41922$0)
store volatile %struct.ScmObj* %args46951$ae41922$1, %struct.ScmObj** %stackaddr$prim48245, align 8
%stackaddr$prim48246 = alloca %struct.ScmObj*, align 8
%args46951$ae41922$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41923, %struct.ScmObj* %args46951$ae41922$1)
store volatile %struct.ScmObj* %args46951$ae41922$2, %struct.ScmObj** %stackaddr$prim48246, align 8
%clofunc48247 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41922)
musttail call tailcc void %clofunc48247(%struct.ScmObj* %ae41922, %struct.ScmObj* %args46951$ae41922$2)
ret void
}

define tailcc void @proc_clo$ae41922(%struct.ScmObj* %env$ae41922,%struct.ScmObj* %current_45args46912) {
%stackaddr$env-ref48248 = alloca %struct.ScmObj*, align 8
%lsts40208 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41922, i64 0)
store %struct.ScmObj* %lsts40208, %struct.ScmObj** %stackaddr$env-ref48248
%stackaddr$env-ref48249 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41922, i64 1)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref48249
%stackaddr$env-ref48250 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41922, i64 2)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref48250
%stackaddr$env-ref48251 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41922, i64 3)
store %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$env-ref48251
%stackaddr$env-ref48252 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41922, i64 4)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48252
%stackaddr$env-ref48253 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41922, i64 5)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref48253
%stackaddr$env-ref48254 = alloca %struct.ScmObj*, align 8
%k40456 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41922, i64 6)
store %struct.ScmObj* %k40456, %struct.ScmObj** %stackaddr$env-ref48254
%stackaddr$prim48255 = alloca %struct.ScmObj*, align 8
%_95k40459 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46912)
store volatile %struct.ScmObj* %_95k40459, %struct.ScmObj** %stackaddr$prim48255, align 8
%stackaddr$prim48256 = alloca %struct.ScmObj*, align 8
%current_45args46913 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46912)
store volatile %struct.ScmObj* %current_45args46913, %struct.ScmObj** %stackaddr$prim48256, align 8
%stackaddr$prim48257 = alloca %struct.ScmObj*, align 8
%anf_45bind40290 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46913)
store volatile %struct.ScmObj* %anf_45bind40290, %struct.ScmObj** %stackaddr$prim48257, align 8
%stackaddr$makeclosure48258 = alloca %struct.ScmObj*, align 8
%fptrToInt48259 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41943 to i64
%ae41943 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt48259)
store volatile %struct.ScmObj* %ae41943, %struct.ScmObj** %stackaddr$makeclosure48258, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41943, %struct.ScmObj* %lsts40208, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41943, %struct.ScmObj* %_37foldr40128, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41943, %struct.ScmObj* %f40210, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41943, %struct.ScmObj* %acc40209, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41943, %struct.ScmObj* %_37foldl40206, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41943, %struct.ScmObj* %_37map140154, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41943, %struct.ScmObj* %k40456, i64 6)
%args46946$_37map140154$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48260 = alloca %struct.ScmObj*, align 8
%args46946$_37map140154$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40208, %struct.ScmObj* %args46946$_37map140154$0)
store volatile %struct.ScmObj* %args46946$_37map140154$1, %struct.ScmObj** %stackaddr$prim48260, align 8
%stackaddr$prim48261 = alloca %struct.ScmObj*, align 8
%args46946$_37map140154$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40290, %struct.ScmObj* %args46946$_37map140154$1)
store volatile %struct.ScmObj* %args46946$_37map140154$2, %struct.ScmObj** %stackaddr$prim48261, align 8
%stackaddr$prim48262 = alloca %struct.ScmObj*, align 8
%args46946$_37map140154$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41943, %struct.ScmObj* %args46946$_37map140154$2)
store volatile %struct.ScmObj* %args46946$_37map140154$3, %struct.ScmObj** %stackaddr$prim48262, align 8
%clofunc48263 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140154)
musttail call tailcc void %clofunc48263(%struct.ScmObj* %_37map140154, %struct.ScmObj* %args46946$_37map140154$3)
ret void
}

define tailcc void @proc_clo$ae41943(%struct.ScmObj* %env$ae41943,%struct.ScmObj* %current_45args46915) {
%stackaddr$env-ref48264 = alloca %struct.ScmObj*, align 8
%lsts40208 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41943, i64 0)
store %struct.ScmObj* %lsts40208, %struct.ScmObj** %stackaddr$env-ref48264
%stackaddr$env-ref48265 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41943, i64 1)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref48265
%stackaddr$env-ref48266 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41943, i64 2)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref48266
%stackaddr$env-ref48267 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41943, i64 3)
store %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$env-ref48267
%stackaddr$env-ref48268 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41943, i64 4)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48268
%stackaddr$env-ref48269 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41943, i64 5)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref48269
%stackaddr$env-ref48270 = alloca %struct.ScmObj*, align 8
%k40456 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41943, i64 6)
store %struct.ScmObj* %k40456, %struct.ScmObj** %stackaddr$env-ref48270
%stackaddr$prim48271 = alloca %struct.ScmObj*, align 8
%_95k40460 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46915)
store volatile %struct.ScmObj* %_95k40460, %struct.ScmObj** %stackaddr$prim48271, align 8
%stackaddr$prim48272 = alloca %struct.ScmObj*, align 8
%current_45args46916 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46915)
store volatile %struct.ScmObj* %current_45args46916, %struct.ScmObj** %stackaddr$prim48272, align 8
%stackaddr$prim48273 = alloca %struct.ScmObj*, align 8
%lsts_4340215 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46916)
store volatile %struct.ScmObj* %lsts_4340215, %struct.ScmObj** %stackaddr$prim48273, align 8
%stackaddr$makeclosure48274 = alloca %struct.ScmObj*, align 8
%fptrToInt48275 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41946 to i64
%ae41946 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt48275)
store volatile %struct.ScmObj* %ae41946, %struct.ScmObj** %stackaddr$makeclosure48274, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41946, %struct.ScmObj* %lsts40208, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41946, %struct.ScmObj* %_37foldr40128, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41946, %struct.ScmObj* %lsts_4340215, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41946, %struct.ScmObj* %f40210, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41946, %struct.ScmObj* %acc40209, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41946, %struct.ScmObj* %_37foldl40206, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41946, %struct.ScmObj* %_37map140154, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae41946, %struct.ScmObj* %k40456, i64 7)
%ae41947 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48276 = alloca %struct.ScmObj*, align 8
%fptrToInt48277 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41948 to i64
%ae41948 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48277)
store volatile %struct.ScmObj* %ae41948, %struct.ScmObj** %stackaddr$makeclosure48276, align 8
%args46945$ae41946$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48278 = alloca %struct.ScmObj*, align 8
%args46945$ae41946$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41948, %struct.ScmObj* %args46945$ae41946$0)
store volatile %struct.ScmObj* %args46945$ae41946$1, %struct.ScmObj** %stackaddr$prim48278, align 8
%stackaddr$prim48279 = alloca %struct.ScmObj*, align 8
%args46945$ae41946$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41947, %struct.ScmObj* %args46945$ae41946$1)
store volatile %struct.ScmObj* %args46945$ae41946$2, %struct.ScmObj** %stackaddr$prim48279, align 8
%clofunc48280 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41946)
musttail call tailcc void %clofunc48280(%struct.ScmObj* %ae41946, %struct.ScmObj* %args46945$ae41946$2)
ret void
}

define tailcc void @proc_clo$ae41946(%struct.ScmObj* %env$ae41946,%struct.ScmObj* %current_45args46918) {
%stackaddr$env-ref48281 = alloca %struct.ScmObj*, align 8
%lsts40208 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41946, i64 0)
store %struct.ScmObj* %lsts40208, %struct.ScmObj** %stackaddr$env-ref48281
%stackaddr$env-ref48282 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41946, i64 1)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref48282
%stackaddr$env-ref48283 = alloca %struct.ScmObj*, align 8
%lsts_4340215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41946, i64 2)
store %struct.ScmObj* %lsts_4340215, %struct.ScmObj** %stackaddr$env-ref48283
%stackaddr$env-ref48284 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41946, i64 3)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref48284
%stackaddr$env-ref48285 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41946, i64 4)
store %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$env-ref48285
%stackaddr$env-ref48286 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41946, i64 5)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48286
%stackaddr$env-ref48287 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41946, i64 6)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref48287
%stackaddr$env-ref48288 = alloca %struct.ScmObj*, align 8
%k40456 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41946, i64 7)
store %struct.ScmObj* %k40456, %struct.ScmObj** %stackaddr$env-ref48288
%stackaddr$prim48289 = alloca %struct.ScmObj*, align 8
%_95k40461 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46918)
store volatile %struct.ScmObj* %_95k40461, %struct.ScmObj** %stackaddr$prim48289, align 8
%stackaddr$prim48290 = alloca %struct.ScmObj*, align 8
%current_45args46919 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46918)
store volatile %struct.ScmObj* %current_45args46919, %struct.ScmObj** %stackaddr$prim48290, align 8
%stackaddr$prim48291 = alloca %struct.ScmObj*, align 8
%anf_45bind40291 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46919)
store volatile %struct.ScmObj* %anf_45bind40291, %struct.ScmObj** %stackaddr$prim48291, align 8
%stackaddr$makeclosure48292 = alloca %struct.ScmObj*, align 8
%fptrToInt48293 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41967 to i64
%ae41967 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt48293)
store volatile %struct.ScmObj* %ae41967, %struct.ScmObj** %stackaddr$makeclosure48292, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41967, %struct.ScmObj* %lsts_4340215, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41967, %struct.ScmObj* %f40210, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41967, %struct.ScmObj* %acc40209, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41967, %struct.ScmObj* %_37foldr40128, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41967, %struct.ScmObj* %_37foldl40206, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41967, %struct.ScmObj* %k40456, i64 5)
%args46940$_37map140154$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48294 = alloca %struct.ScmObj*, align 8
%args46940$_37map140154$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40208, %struct.ScmObj* %args46940$_37map140154$0)
store volatile %struct.ScmObj* %args46940$_37map140154$1, %struct.ScmObj** %stackaddr$prim48294, align 8
%stackaddr$prim48295 = alloca %struct.ScmObj*, align 8
%args46940$_37map140154$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40291, %struct.ScmObj* %args46940$_37map140154$1)
store volatile %struct.ScmObj* %args46940$_37map140154$2, %struct.ScmObj** %stackaddr$prim48295, align 8
%stackaddr$prim48296 = alloca %struct.ScmObj*, align 8
%args46940$_37map140154$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41967, %struct.ScmObj* %args46940$_37map140154$2)
store volatile %struct.ScmObj* %args46940$_37map140154$3, %struct.ScmObj** %stackaddr$prim48296, align 8
%clofunc48297 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140154)
musttail call tailcc void %clofunc48297(%struct.ScmObj* %_37map140154, %struct.ScmObj* %args46940$_37map140154$3)
ret void
}

define tailcc void @proc_clo$ae41967(%struct.ScmObj* %env$ae41967,%struct.ScmObj* %current_45args46921) {
%stackaddr$env-ref48298 = alloca %struct.ScmObj*, align 8
%lsts_4340215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41967, i64 0)
store %struct.ScmObj* %lsts_4340215, %struct.ScmObj** %stackaddr$env-ref48298
%stackaddr$env-ref48299 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41967, i64 1)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref48299
%stackaddr$env-ref48300 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41967, i64 2)
store %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$env-ref48300
%stackaddr$env-ref48301 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41967, i64 3)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref48301
%stackaddr$env-ref48302 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41967, i64 4)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48302
%stackaddr$env-ref48303 = alloca %struct.ScmObj*, align 8
%k40456 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41967, i64 5)
store %struct.ScmObj* %k40456, %struct.ScmObj** %stackaddr$env-ref48303
%stackaddr$prim48304 = alloca %struct.ScmObj*, align 8
%_95k40462 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46921)
store volatile %struct.ScmObj* %_95k40462, %struct.ScmObj** %stackaddr$prim48304, align 8
%stackaddr$prim48305 = alloca %struct.ScmObj*, align 8
%current_45args46922 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46921)
store volatile %struct.ScmObj* %current_45args46922, %struct.ScmObj** %stackaddr$prim48305, align 8
%stackaddr$prim48306 = alloca %struct.ScmObj*, align 8
%vs40213 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46922)
store volatile %struct.ScmObj* %vs40213, %struct.ScmObj** %stackaddr$prim48306, align 8
%stackaddr$makeclosure48307 = alloca %struct.ScmObj*, align 8
%fptrToInt48308 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41970 to i64
%ae41970 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt48308)
store volatile %struct.ScmObj* %ae41970, %struct.ScmObj** %stackaddr$makeclosure48307, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41970, %struct.ScmObj* %lsts_4340215, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41970, %struct.ScmObj* %vs40213, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41970, %struct.ScmObj* %f40210, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41970, %struct.ScmObj* %acc40209, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41970, %struct.ScmObj* %_37foldr40128, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41970, %struct.ScmObj* %_37foldl40206, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41970, %struct.ScmObj* %k40456, i64 6)
%ae41971 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48309 = alloca %struct.ScmObj*, align 8
%fptrToInt48310 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41972 to i64
%ae41972 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48310)
store volatile %struct.ScmObj* %ae41972, %struct.ScmObj** %stackaddr$makeclosure48309, align 8
%args46939$ae41970$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48311 = alloca %struct.ScmObj*, align 8
%args46939$ae41970$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41972, %struct.ScmObj* %args46939$ae41970$0)
store volatile %struct.ScmObj* %args46939$ae41970$1, %struct.ScmObj** %stackaddr$prim48311, align 8
%stackaddr$prim48312 = alloca %struct.ScmObj*, align 8
%args46939$ae41970$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41971, %struct.ScmObj* %args46939$ae41970$1)
store volatile %struct.ScmObj* %args46939$ae41970$2, %struct.ScmObj** %stackaddr$prim48312, align 8
%clofunc48313 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41970)
musttail call tailcc void %clofunc48313(%struct.ScmObj* %ae41970, %struct.ScmObj* %args46939$ae41970$2)
ret void
}

define tailcc void @proc_clo$ae41970(%struct.ScmObj* %env$ae41970,%struct.ScmObj* %current_45args46924) {
%stackaddr$env-ref48314 = alloca %struct.ScmObj*, align 8
%lsts_4340215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41970, i64 0)
store %struct.ScmObj* %lsts_4340215, %struct.ScmObj** %stackaddr$env-ref48314
%stackaddr$env-ref48315 = alloca %struct.ScmObj*, align 8
%vs40213 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41970, i64 1)
store %struct.ScmObj* %vs40213, %struct.ScmObj** %stackaddr$env-ref48315
%stackaddr$env-ref48316 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41970, i64 2)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref48316
%stackaddr$env-ref48317 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41970, i64 3)
store %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$env-ref48317
%stackaddr$env-ref48318 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41970, i64 4)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref48318
%stackaddr$env-ref48319 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41970, i64 5)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48319
%stackaddr$env-ref48320 = alloca %struct.ScmObj*, align 8
%k40456 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41970, i64 6)
store %struct.ScmObj* %k40456, %struct.ScmObj** %stackaddr$env-ref48320
%stackaddr$prim48321 = alloca %struct.ScmObj*, align 8
%_95k40463 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46924)
store volatile %struct.ScmObj* %_95k40463, %struct.ScmObj** %stackaddr$prim48321, align 8
%stackaddr$prim48322 = alloca %struct.ScmObj*, align 8
%current_45args46925 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46924)
store volatile %struct.ScmObj* %current_45args46925, %struct.ScmObj** %stackaddr$prim48322, align 8
%stackaddr$prim48323 = alloca %struct.ScmObj*, align 8
%anf_45bind40292 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46925)
store volatile %struct.ScmObj* %anf_45bind40292, %struct.ScmObj** %stackaddr$prim48323, align 8
%ae41993 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48324 = alloca %struct.ScmObj*, align 8
%anf_45bind40293 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40209, %struct.ScmObj* %ae41993)
store volatile %struct.ScmObj* %anf_45bind40293, %struct.ScmObj** %stackaddr$prim48324, align 8
%stackaddr$makeclosure48325 = alloca %struct.ScmObj*, align 8
%fptrToInt48326 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41995 to i64
%ae41995 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48326)
store volatile %struct.ScmObj* %ae41995, %struct.ScmObj** %stackaddr$makeclosure48325, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41995, %struct.ScmObj* %lsts_4340215, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41995, %struct.ScmObj* %f40210, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41995, %struct.ScmObj* %_37foldl40206, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41995, %struct.ScmObj* %k40456, i64 3)
%args46933$_37foldr40128$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48327 = alloca %struct.ScmObj*, align 8
%args46933$_37foldr40128$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs40213, %struct.ScmObj* %args46933$_37foldr40128$0)
store volatile %struct.ScmObj* %args46933$_37foldr40128$1, %struct.ScmObj** %stackaddr$prim48327, align 8
%stackaddr$prim48328 = alloca %struct.ScmObj*, align 8
%args46933$_37foldr40128$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40293, %struct.ScmObj* %args46933$_37foldr40128$1)
store volatile %struct.ScmObj* %args46933$_37foldr40128$2, %struct.ScmObj** %stackaddr$prim48328, align 8
%stackaddr$prim48329 = alloca %struct.ScmObj*, align 8
%args46933$_37foldr40128$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40292, %struct.ScmObj* %args46933$_37foldr40128$2)
store volatile %struct.ScmObj* %args46933$_37foldr40128$3, %struct.ScmObj** %stackaddr$prim48329, align 8
%stackaddr$prim48330 = alloca %struct.ScmObj*, align 8
%args46933$_37foldr40128$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41995, %struct.ScmObj* %args46933$_37foldr40128$3)
store volatile %struct.ScmObj* %args46933$_37foldr40128$4, %struct.ScmObj** %stackaddr$prim48330, align 8
%clofunc48331 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr40128)
musttail call tailcc void %clofunc48331(%struct.ScmObj* %_37foldr40128, %struct.ScmObj* %args46933$_37foldr40128$4)
ret void
}

define tailcc void @proc_clo$ae41995(%struct.ScmObj* %env$ae41995,%struct.ScmObj* %current_45args46927) {
%stackaddr$env-ref48332 = alloca %struct.ScmObj*, align 8
%lsts_4340215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41995, i64 0)
store %struct.ScmObj* %lsts_4340215, %struct.ScmObj** %stackaddr$env-ref48332
%stackaddr$env-ref48333 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41995, i64 1)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref48333
%stackaddr$env-ref48334 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41995, i64 2)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48334
%stackaddr$env-ref48335 = alloca %struct.ScmObj*, align 8
%k40456 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41995, i64 3)
store %struct.ScmObj* %k40456, %struct.ScmObj** %stackaddr$env-ref48335
%stackaddr$prim48336 = alloca %struct.ScmObj*, align 8
%_95k40464 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46927)
store volatile %struct.ScmObj* %_95k40464, %struct.ScmObj** %stackaddr$prim48336, align 8
%stackaddr$prim48337 = alloca %struct.ScmObj*, align 8
%current_45args46928 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46927)
store volatile %struct.ScmObj* %current_45args46928, %struct.ScmObj** %stackaddr$prim48337, align 8
%stackaddr$prim48338 = alloca %struct.ScmObj*, align 8
%anf_45bind40294 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46928)
store volatile %struct.ScmObj* %anf_45bind40294, %struct.ScmObj** %stackaddr$prim48338, align 8
%stackaddr$makeclosure48339 = alloca %struct.ScmObj*, align 8
%fptrToInt48340 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41999 to i64
%ae41999 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48340)
store volatile %struct.ScmObj* %ae41999, %struct.ScmObj** %stackaddr$makeclosure48339, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41999, %struct.ScmObj* %lsts_4340215, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41999, %struct.ScmObj* %f40210, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41999, %struct.ScmObj* %_37foldl40206, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41999, %struct.ScmObj* %k40456, i64 3)
%stackaddr$prim48341 = alloca %struct.ScmObj*, align 8
%cpsargs40467 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41999, %struct.ScmObj* %anf_45bind40294)
store volatile %struct.ScmObj* %cpsargs40467, %struct.ScmObj** %stackaddr$prim48341, align 8
%clofunc48342 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40210)
musttail call tailcc void %clofunc48342(%struct.ScmObj* %f40210, %struct.ScmObj* %cpsargs40467)
ret void
}

define tailcc void @proc_clo$ae41999(%struct.ScmObj* %env$ae41999,%struct.ScmObj* %current_45args46930) {
%stackaddr$env-ref48343 = alloca %struct.ScmObj*, align 8
%lsts_4340215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41999, i64 0)
store %struct.ScmObj* %lsts_4340215, %struct.ScmObj** %stackaddr$env-ref48343
%stackaddr$env-ref48344 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41999, i64 1)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref48344
%stackaddr$env-ref48345 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41999, i64 2)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48345
%stackaddr$env-ref48346 = alloca %struct.ScmObj*, align 8
%k40456 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41999, i64 3)
store %struct.ScmObj* %k40456, %struct.ScmObj** %stackaddr$env-ref48346
%stackaddr$prim48347 = alloca %struct.ScmObj*, align 8
%_95k40465 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46930)
store volatile %struct.ScmObj* %_95k40465, %struct.ScmObj** %stackaddr$prim48347, align 8
%stackaddr$prim48348 = alloca %struct.ScmObj*, align 8
%current_45args46931 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46930)
store volatile %struct.ScmObj* %current_45args46931, %struct.ScmObj** %stackaddr$prim48348, align 8
%stackaddr$prim48349 = alloca %struct.ScmObj*, align 8
%acc_4340217 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46931)
store volatile %struct.ScmObj* %acc_4340217, %struct.ScmObj** %stackaddr$prim48349, align 8
%stackaddr$prim48350 = alloca %struct.ScmObj*, align 8
%anf_45bind40295 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4340217, %struct.ScmObj* %lsts_4340215)
store volatile %struct.ScmObj* %anf_45bind40295, %struct.ScmObj** %stackaddr$prim48350, align 8
%stackaddr$prim48351 = alloca %struct.ScmObj*, align 8
%anf_45bind40296 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40210, %struct.ScmObj* %anf_45bind40295)
store volatile %struct.ScmObj* %anf_45bind40296, %struct.ScmObj** %stackaddr$prim48351, align 8
%stackaddr$prim48352 = alloca %struct.ScmObj*, align 8
%cpsargs40466 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40456, %struct.ScmObj* %anf_45bind40296)
store volatile %struct.ScmObj* %cpsargs40466, %struct.ScmObj** %stackaddr$prim48352, align 8
%clofunc48353 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl40206)
musttail call tailcc void %clofunc48353(%struct.ScmObj* %_37foldl40206, %struct.ScmObj* %cpsargs40466)
ret void
}

define tailcc void @proc_clo$ae41972(%struct.ScmObj* %env$ae41972,%struct.ScmObj* %current_45args46934) {
%stackaddr$prim48354 = alloca %struct.ScmObj*, align 8
%k40468 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46934)
store volatile %struct.ScmObj* %k40468, %struct.ScmObj** %stackaddr$prim48354, align 8
%stackaddr$prim48355 = alloca %struct.ScmObj*, align 8
%current_45args46935 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46934)
store volatile %struct.ScmObj* %current_45args46935, %struct.ScmObj** %stackaddr$prim48355, align 8
%stackaddr$prim48356 = alloca %struct.ScmObj*, align 8
%a40219 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46935)
store volatile %struct.ScmObj* %a40219, %struct.ScmObj** %stackaddr$prim48356, align 8
%stackaddr$prim48357 = alloca %struct.ScmObj*, align 8
%current_45args46936 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46935)
store volatile %struct.ScmObj* %current_45args46936, %struct.ScmObj** %stackaddr$prim48357, align 8
%stackaddr$prim48358 = alloca %struct.ScmObj*, align 8
%b40218 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46936)
store volatile %struct.ScmObj* %b40218, %struct.ScmObj** %stackaddr$prim48358, align 8
%stackaddr$prim48359 = alloca %struct.ScmObj*, align 8
%cpsprim40469 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a40219, %struct.ScmObj* %b40218)
store volatile %struct.ScmObj* %cpsprim40469, %struct.ScmObj** %stackaddr$prim48359, align 8
%ae41976 = call %struct.ScmObj* @const_init_int(i64 0)
%args46938$k40468$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48360 = alloca %struct.ScmObj*, align 8
%args46938$k40468$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40469, %struct.ScmObj* %args46938$k40468$0)
store volatile %struct.ScmObj* %args46938$k40468$1, %struct.ScmObj** %stackaddr$prim48360, align 8
%stackaddr$prim48361 = alloca %struct.ScmObj*, align 8
%args46938$k40468$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41976, %struct.ScmObj* %args46938$k40468$1)
store volatile %struct.ScmObj* %args46938$k40468$2, %struct.ScmObj** %stackaddr$prim48361, align 8
%clofunc48362 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40468)
musttail call tailcc void %clofunc48362(%struct.ScmObj* %k40468, %struct.ScmObj* %args46938$k40468$2)
ret void
}

define tailcc void @proc_clo$ae41948(%struct.ScmObj* %env$ae41948,%struct.ScmObj* %current_45args46941) {
%stackaddr$prim48363 = alloca %struct.ScmObj*, align 8
%k40470 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46941)
store volatile %struct.ScmObj* %k40470, %struct.ScmObj** %stackaddr$prim48363, align 8
%stackaddr$prim48364 = alloca %struct.ScmObj*, align 8
%current_45args46942 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46941)
store volatile %struct.ScmObj* %current_45args46942, %struct.ScmObj** %stackaddr$prim48364, align 8
%stackaddr$prim48365 = alloca %struct.ScmObj*, align 8
%x40214 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46942)
store volatile %struct.ScmObj* %x40214, %struct.ScmObj** %stackaddr$prim48365, align 8
%stackaddr$prim48366 = alloca %struct.ScmObj*, align 8
%cpsprim40471 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40214)
store volatile %struct.ScmObj* %cpsprim40471, %struct.ScmObj** %stackaddr$prim48366, align 8
%ae41951 = call %struct.ScmObj* @const_init_int(i64 0)
%args46944$k40470$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48367 = alloca %struct.ScmObj*, align 8
%args46944$k40470$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40471, %struct.ScmObj* %args46944$k40470$0)
store volatile %struct.ScmObj* %args46944$k40470$1, %struct.ScmObj** %stackaddr$prim48367, align 8
%stackaddr$prim48368 = alloca %struct.ScmObj*, align 8
%args46944$k40470$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41951, %struct.ScmObj* %args46944$k40470$1)
store volatile %struct.ScmObj* %args46944$k40470$2, %struct.ScmObj** %stackaddr$prim48368, align 8
%clofunc48369 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40470)
musttail call tailcc void %clofunc48369(%struct.ScmObj* %k40470, %struct.ScmObj* %args46944$k40470$2)
ret void
}

define tailcc void @proc_clo$ae41924(%struct.ScmObj* %env$ae41924,%struct.ScmObj* %current_45args46947) {
%stackaddr$prim48370 = alloca %struct.ScmObj*, align 8
%k40472 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46947)
store volatile %struct.ScmObj* %k40472, %struct.ScmObj** %stackaddr$prim48370, align 8
%stackaddr$prim48371 = alloca %struct.ScmObj*, align 8
%current_45args46948 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46947)
store volatile %struct.ScmObj* %current_45args46948, %struct.ScmObj** %stackaddr$prim48371, align 8
%stackaddr$prim48372 = alloca %struct.ScmObj*, align 8
%x40216 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46948)
store volatile %struct.ScmObj* %x40216, %struct.ScmObj** %stackaddr$prim48372, align 8
%stackaddr$prim48373 = alloca %struct.ScmObj*, align 8
%cpsprim40473 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40216)
store volatile %struct.ScmObj* %cpsprim40473, %struct.ScmObj** %stackaddr$prim48373, align 8
%ae41927 = call %struct.ScmObj* @const_init_int(i64 0)
%args46950$k40472$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48374 = alloca %struct.ScmObj*, align 8
%args46950$k40472$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40473, %struct.ScmObj* %args46950$k40472$0)
store volatile %struct.ScmObj* %args46950$k40472$1, %struct.ScmObj** %stackaddr$prim48374, align 8
%stackaddr$prim48375 = alloca %struct.ScmObj*, align 8
%args46950$k40472$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41927, %struct.ScmObj* %args46950$k40472$1)
store volatile %struct.ScmObj* %args46950$k40472$2, %struct.ScmObj** %stackaddr$prim48375, align 8
%clofunc48376 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40472)
musttail call tailcc void %clofunc48376(%struct.ScmObj* %k40472, %struct.ScmObj* %args46950$k40472$2)
ret void
}

define tailcc void @proc_clo$ae41876(%struct.ScmObj* %env$ae41876,%struct.ScmObj* %current_45args46953) {
%stackaddr$prim48377 = alloca %struct.ScmObj*, align 8
%k40474 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46953)
store volatile %struct.ScmObj* %k40474, %struct.ScmObj** %stackaddr$prim48377, align 8
%stackaddr$prim48378 = alloca %struct.ScmObj*, align 8
%current_45args46954 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46953)
store volatile %struct.ScmObj* %current_45args46954, %struct.ScmObj** %stackaddr$prim48378, align 8
%stackaddr$prim48379 = alloca %struct.ScmObj*, align 8
%lst40212 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46954)
store volatile %struct.ScmObj* %lst40212, %struct.ScmObj** %stackaddr$prim48379, align 8
%stackaddr$prim48380 = alloca %struct.ScmObj*, align 8
%current_45args46955 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46954)
store volatile %struct.ScmObj* %current_45args46955, %struct.ScmObj** %stackaddr$prim48380, align 8
%stackaddr$prim48381 = alloca %struct.ScmObj*, align 8
%b40211 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46955)
store volatile %struct.ScmObj* %b40211, %struct.ScmObj** %stackaddr$prim48381, align 8
%truthy$cmp48382 = call i64 @is_truthy_value(%struct.ScmObj* %b40211)
%cmp$cmp48382 = icmp eq i64 %truthy$cmp48382, 1
br i1 %cmp$cmp48382, label %truebranch$cmp48382, label %falsebranch$cmp48382
truebranch$cmp48382:
%ae41879 = call %struct.ScmObj* @const_init_int(i64 0)
%args46957$k40474$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48383 = alloca %struct.ScmObj*, align 8
%args46957$k40474$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b40211, %struct.ScmObj* %args46957$k40474$0)
store volatile %struct.ScmObj* %args46957$k40474$1, %struct.ScmObj** %stackaddr$prim48383, align 8
%stackaddr$prim48384 = alloca %struct.ScmObj*, align 8
%args46957$k40474$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41879, %struct.ScmObj* %args46957$k40474$1)
store volatile %struct.ScmObj* %args46957$k40474$2, %struct.ScmObj** %stackaddr$prim48384, align 8
%clofunc48385 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40474)
musttail call tailcc void %clofunc48385(%struct.ScmObj* %k40474, %struct.ScmObj* %args46957$k40474$2)
ret void
falsebranch$cmp48382:
%stackaddr$prim48386 = alloca %struct.ScmObj*, align 8
%cpsprim40475 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40212)
store volatile %struct.ScmObj* %cpsprim40475, %struct.ScmObj** %stackaddr$prim48386, align 8
%ae41886 = call %struct.ScmObj* @const_init_int(i64 0)
%args46958$k40474$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48387 = alloca %struct.ScmObj*, align 8
%args46958$k40474$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40475, %struct.ScmObj* %args46958$k40474$0)
store volatile %struct.ScmObj* %args46958$k40474$1, %struct.ScmObj** %stackaddr$prim48387, align 8
%stackaddr$prim48388 = alloca %struct.ScmObj*, align 8
%args46958$k40474$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41886, %struct.ScmObj* %args46958$k40474$1)
store volatile %struct.ScmObj* %args46958$k40474$2, %struct.ScmObj** %stackaddr$prim48388, align 8
%clofunc48389 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40474)
musttail call tailcc void %clofunc48389(%struct.ScmObj* %k40474, %struct.ScmObj* %args46958$k40474$2)
ret void
}

define tailcc void @proc_clo$ae41717(%struct.ScmObj* %env$ae41717,%struct.ScmObj* %args4015040476) {
%stackaddr$env-ref48390 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41717, i64 0)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref48390
%stackaddr$env-ref48391 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41717, i64 1)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref48391
%stackaddr$env-ref48392 = alloca %struct.ScmObj*, align 8
%_37drop_45right40142 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41717, i64 2)
store %struct.ScmObj* %_37drop_45right40142, %struct.ScmObj** %stackaddr$env-ref48392
%stackaddr$prim48393 = alloca %struct.ScmObj*, align 8
%k40477 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4015040476)
store volatile %struct.ScmObj* %k40477, %struct.ScmObj** %stackaddr$prim48393, align 8
%stackaddr$prim48394 = alloca %struct.ScmObj*, align 8
%args40150 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4015040476)
store volatile %struct.ScmObj* %args40150, %struct.ScmObj** %stackaddr$prim48394, align 8
%stackaddr$prim48395 = alloca %struct.ScmObj*, align 8
%f40152 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40150)
store volatile %struct.ScmObj* %f40152, %struct.ScmObj** %stackaddr$prim48395, align 8
%stackaddr$prim48396 = alloca %struct.ScmObj*, align 8
%lsts40151 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40150)
store volatile %struct.ScmObj* %lsts40151, %struct.ScmObj** %stackaddr$prim48396, align 8
%stackaddr$makeclosure48397 = alloca %struct.ScmObj*, align 8
%fptrToInt48398 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41722 to i64
%ae41722 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48398)
store volatile %struct.ScmObj* %ae41722, %struct.ScmObj** %stackaddr$makeclosure48397, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41722, %struct.ScmObj* %lsts40151, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41722, %struct.ScmObj* %_37foldr40128, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41722, %struct.ScmObj* %k40477, i64 2)
%ae41723 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48399 = alloca %struct.ScmObj*, align 8
%fptrToInt48400 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41724 to i64
%ae41724 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48400)
store volatile %struct.ScmObj* %ae41724, %struct.ScmObj** %stackaddr$makeclosure48399, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41724, %struct.ScmObj* %_37last40145, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41724, %struct.ScmObj* %_37drop_45right40142, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41724, %struct.ScmObj* %f40152, i64 2)
%args46977$ae41722$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48401 = alloca %struct.ScmObj*, align 8
%args46977$ae41722$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41724, %struct.ScmObj* %args46977$ae41722$0)
store volatile %struct.ScmObj* %args46977$ae41722$1, %struct.ScmObj** %stackaddr$prim48401, align 8
%stackaddr$prim48402 = alloca %struct.ScmObj*, align 8
%args46977$ae41722$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41723, %struct.ScmObj* %args46977$ae41722$1)
store volatile %struct.ScmObj* %args46977$ae41722$2, %struct.ScmObj** %stackaddr$prim48402, align 8
%clofunc48403 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41722)
musttail call tailcc void %clofunc48403(%struct.ScmObj* %ae41722, %struct.ScmObj* %args46977$ae41722$2)
ret void
}

define tailcc void @proc_clo$ae41722(%struct.ScmObj* %env$ae41722,%struct.ScmObj* %current_45args46962) {
%stackaddr$env-ref48404 = alloca %struct.ScmObj*, align 8
%lsts40151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41722, i64 0)
store %struct.ScmObj* %lsts40151, %struct.ScmObj** %stackaddr$env-ref48404
%stackaddr$env-ref48405 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41722, i64 1)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref48405
%stackaddr$env-ref48406 = alloca %struct.ScmObj*, align 8
%k40477 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41722, i64 2)
store %struct.ScmObj* %k40477, %struct.ScmObj** %stackaddr$env-ref48406
%stackaddr$prim48407 = alloca %struct.ScmObj*, align 8
%_95k40478 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46962)
store volatile %struct.ScmObj* %_95k40478, %struct.ScmObj** %stackaddr$prim48407, align 8
%stackaddr$prim48408 = alloca %struct.ScmObj*, align 8
%current_45args46963 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46962)
store volatile %struct.ScmObj* %current_45args46963, %struct.ScmObj** %stackaddr$prim48408, align 8
%stackaddr$prim48409 = alloca %struct.ScmObj*, align 8
%anf_45bind40283 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46963)
store volatile %struct.ScmObj* %anf_45bind40283, %struct.ScmObj** %stackaddr$prim48409, align 8
%ae41785 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48410 = alloca %struct.ScmObj*, align 8
%anf_45bind40284 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41785, %struct.ScmObj* %lsts40151)
store volatile %struct.ScmObj* %anf_45bind40284, %struct.ScmObj** %stackaddr$prim48410, align 8
%stackaddr$prim48411 = alloca %struct.ScmObj*, align 8
%anf_45bind40285 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40283, %struct.ScmObj* %anf_45bind40284)
store volatile %struct.ScmObj* %anf_45bind40285, %struct.ScmObj** %stackaddr$prim48411, align 8
%stackaddr$prim48412 = alloca %struct.ScmObj*, align 8
%cpsargs40479 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40477, %struct.ScmObj* %anf_45bind40285)
store volatile %struct.ScmObj* %cpsargs40479, %struct.ScmObj** %stackaddr$prim48412, align 8
%clofunc48413 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr40128)
musttail call tailcc void %clofunc48413(%struct.ScmObj* %_37foldr40128, %struct.ScmObj* %cpsargs40479)
ret void
}

define tailcc void @proc_clo$ae41724(%struct.ScmObj* %env$ae41724,%struct.ScmObj* %fargs4015340480) {
%stackaddr$env-ref48414 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41724, i64 0)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref48414
%stackaddr$env-ref48415 = alloca %struct.ScmObj*, align 8
%_37drop_45right40142 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41724, i64 1)
store %struct.ScmObj* %_37drop_45right40142, %struct.ScmObj** %stackaddr$env-ref48415
%stackaddr$env-ref48416 = alloca %struct.ScmObj*, align 8
%f40152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41724, i64 2)
store %struct.ScmObj* %f40152, %struct.ScmObj** %stackaddr$env-ref48416
%stackaddr$prim48417 = alloca %struct.ScmObj*, align 8
%k40481 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs4015340480)
store volatile %struct.ScmObj* %k40481, %struct.ScmObj** %stackaddr$prim48417, align 8
%stackaddr$prim48418 = alloca %struct.ScmObj*, align 8
%fargs40153 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs4015340480)
store volatile %struct.ScmObj* %fargs40153, %struct.ScmObj** %stackaddr$prim48418, align 8
%stackaddr$makeclosure48419 = alloca %struct.ScmObj*, align 8
%fptrToInt48420 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41728 to i64
%ae41728 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48420)
store volatile %struct.ScmObj* %ae41728, %struct.ScmObj** %stackaddr$makeclosure48419, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41728, %struct.ScmObj* %k40481, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41728, %struct.ScmObj* %_37last40145, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41728, %struct.ScmObj* %fargs40153, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41728, %struct.ScmObj* %f40152, i64 3)
%ae41730 = call %struct.ScmObj* @const_init_int(i64 1)
%args46976$_37drop_45right40142$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48421 = alloca %struct.ScmObj*, align 8
%args46976$_37drop_45right40142$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41730, %struct.ScmObj* %args46976$_37drop_45right40142$0)
store volatile %struct.ScmObj* %args46976$_37drop_45right40142$1, %struct.ScmObj** %stackaddr$prim48421, align 8
%stackaddr$prim48422 = alloca %struct.ScmObj*, align 8
%args46976$_37drop_45right40142$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs40153, %struct.ScmObj* %args46976$_37drop_45right40142$1)
store volatile %struct.ScmObj* %args46976$_37drop_45right40142$2, %struct.ScmObj** %stackaddr$prim48422, align 8
%stackaddr$prim48423 = alloca %struct.ScmObj*, align 8
%args46976$_37drop_45right40142$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41728, %struct.ScmObj* %args46976$_37drop_45right40142$2)
store volatile %struct.ScmObj* %args46976$_37drop_45right40142$3, %struct.ScmObj** %stackaddr$prim48423, align 8
%clofunc48424 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right40142)
musttail call tailcc void %clofunc48424(%struct.ScmObj* %_37drop_45right40142, %struct.ScmObj* %args46976$_37drop_45right40142$3)
ret void
}

define tailcc void @proc_clo$ae41728(%struct.ScmObj* %env$ae41728,%struct.ScmObj* %current_45args46965) {
%stackaddr$env-ref48425 = alloca %struct.ScmObj*, align 8
%k40481 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41728, i64 0)
store %struct.ScmObj* %k40481, %struct.ScmObj** %stackaddr$env-ref48425
%stackaddr$env-ref48426 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41728, i64 1)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref48426
%stackaddr$env-ref48427 = alloca %struct.ScmObj*, align 8
%fargs40153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41728, i64 2)
store %struct.ScmObj* %fargs40153, %struct.ScmObj** %stackaddr$env-ref48427
%stackaddr$env-ref48428 = alloca %struct.ScmObj*, align 8
%f40152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41728, i64 3)
store %struct.ScmObj* %f40152, %struct.ScmObj** %stackaddr$env-ref48428
%stackaddr$prim48429 = alloca %struct.ScmObj*, align 8
%_95k40482 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46965)
store volatile %struct.ScmObj* %_95k40482, %struct.ScmObj** %stackaddr$prim48429, align 8
%stackaddr$prim48430 = alloca %struct.ScmObj*, align 8
%current_45args46966 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46965)
store volatile %struct.ScmObj* %current_45args46966, %struct.ScmObj** %stackaddr$prim48430, align 8
%stackaddr$prim48431 = alloca %struct.ScmObj*, align 8
%anf_45bind40280 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46966)
store volatile %struct.ScmObj* %anf_45bind40280, %struct.ScmObj** %stackaddr$prim48431, align 8
%stackaddr$makeclosure48432 = alloca %struct.ScmObj*, align 8
%fptrToInt48433 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41735 to i64
%ae41735 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48433)
store volatile %struct.ScmObj* %ae41735, %struct.ScmObj** %stackaddr$makeclosure48432, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41735, %struct.ScmObj* %k40481, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41735, %struct.ScmObj* %_37last40145, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41735, %struct.ScmObj* %fargs40153, i64 2)
%stackaddr$prim48434 = alloca %struct.ScmObj*, align 8
%cpsargs40486 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41735, %struct.ScmObj* %anf_45bind40280)
store volatile %struct.ScmObj* %cpsargs40486, %struct.ScmObj** %stackaddr$prim48434, align 8
%clofunc48435 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40152)
musttail call tailcc void %clofunc48435(%struct.ScmObj* %f40152, %struct.ScmObj* %cpsargs40486)
ret void
}

define tailcc void @proc_clo$ae41735(%struct.ScmObj* %env$ae41735,%struct.ScmObj* %current_45args46968) {
%stackaddr$env-ref48436 = alloca %struct.ScmObj*, align 8
%k40481 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41735, i64 0)
store %struct.ScmObj* %k40481, %struct.ScmObj** %stackaddr$env-ref48436
%stackaddr$env-ref48437 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41735, i64 1)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref48437
%stackaddr$env-ref48438 = alloca %struct.ScmObj*, align 8
%fargs40153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41735, i64 2)
store %struct.ScmObj* %fargs40153, %struct.ScmObj** %stackaddr$env-ref48438
%stackaddr$prim48439 = alloca %struct.ScmObj*, align 8
%_95k40483 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46968)
store volatile %struct.ScmObj* %_95k40483, %struct.ScmObj** %stackaddr$prim48439, align 8
%stackaddr$prim48440 = alloca %struct.ScmObj*, align 8
%current_45args46969 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46968)
store volatile %struct.ScmObj* %current_45args46969, %struct.ScmObj** %stackaddr$prim48440, align 8
%stackaddr$prim48441 = alloca %struct.ScmObj*, align 8
%anf_45bind40281 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46969)
store volatile %struct.ScmObj* %anf_45bind40281, %struct.ScmObj** %stackaddr$prim48441, align 8
%stackaddr$makeclosure48442 = alloca %struct.ScmObj*, align 8
%fptrToInt48443 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41740 to i64
%ae41740 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48443)
store volatile %struct.ScmObj* %ae41740, %struct.ScmObj** %stackaddr$makeclosure48442, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41740, %struct.ScmObj* %k40481, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41740, %struct.ScmObj* %anf_45bind40281, i64 1)
%args46975$_37last40145$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48444 = alloca %struct.ScmObj*, align 8
%args46975$_37last40145$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs40153, %struct.ScmObj* %args46975$_37last40145$0)
store volatile %struct.ScmObj* %args46975$_37last40145$1, %struct.ScmObj** %stackaddr$prim48444, align 8
%stackaddr$prim48445 = alloca %struct.ScmObj*, align 8
%args46975$_37last40145$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41740, %struct.ScmObj* %args46975$_37last40145$1)
store volatile %struct.ScmObj* %args46975$_37last40145$2, %struct.ScmObj** %stackaddr$prim48445, align 8
%clofunc48446 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last40145)
musttail call tailcc void %clofunc48446(%struct.ScmObj* %_37last40145, %struct.ScmObj* %args46975$_37last40145$2)
ret void
}

define tailcc void @proc_clo$ae41740(%struct.ScmObj* %env$ae41740,%struct.ScmObj* %current_45args46971) {
%stackaddr$env-ref48447 = alloca %struct.ScmObj*, align 8
%k40481 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41740, i64 0)
store %struct.ScmObj* %k40481, %struct.ScmObj** %stackaddr$env-ref48447
%stackaddr$env-ref48448 = alloca %struct.ScmObj*, align 8
%anf_45bind40281 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41740, i64 1)
store %struct.ScmObj* %anf_45bind40281, %struct.ScmObj** %stackaddr$env-ref48448
%stackaddr$prim48449 = alloca %struct.ScmObj*, align 8
%_95k40484 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46971)
store volatile %struct.ScmObj* %_95k40484, %struct.ScmObj** %stackaddr$prim48449, align 8
%stackaddr$prim48450 = alloca %struct.ScmObj*, align 8
%current_45args46972 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46971)
store volatile %struct.ScmObj* %current_45args46972, %struct.ScmObj** %stackaddr$prim48450, align 8
%stackaddr$prim48451 = alloca %struct.ScmObj*, align 8
%anf_45bind40282 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46972)
store volatile %struct.ScmObj* %anf_45bind40282, %struct.ScmObj** %stackaddr$prim48451, align 8
%stackaddr$prim48452 = alloca %struct.ScmObj*, align 8
%cpsprim40485 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40281, %struct.ScmObj* %anf_45bind40282)
store volatile %struct.ScmObj* %cpsprim40485, %struct.ScmObj** %stackaddr$prim48452, align 8
%ae41745 = call %struct.ScmObj* @const_init_int(i64 0)
%args46974$k40481$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48453 = alloca %struct.ScmObj*, align 8
%args46974$k40481$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40485, %struct.ScmObj* %args46974$k40481$0)
store volatile %struct.ScmObj* %args46974$k40481$1, %struct.ScmObj** %stackaddr$prim48453, align 8
%stackaddr$prim48454 = alloca %struct.ScmObj*, align 8
%args46974$k40481$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41745, %struct.ScmObj* %args46974$k40481$1)
store volatile %struct.ScmObj* %args46974$k40481$2, %struct.ScmObj** %stackaddr$prim48454, align 8
%clofunc48455 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40481)
musttail call tailcc void %clofunc48455(%struct.ScmObj* %k40481, %struct.ScmObj* %args46974$k40481$2)
ret void
}

define tailcc void @proc_clo$ae41640(%struct.ScmObj* %env$ae41640,%struct.ScmObj* %current_45args46979) {
%stackaddr$env-ref48456 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41640, i64 0)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48456
%stackaddr$prim48457 = alloca %struct.ScmObj*, align 8
%k40487 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46979)
store volatile %struct.ScmObj* %k40487, %struct.ScmObj** %stackaddr$prim48457, align 8
%stackaddr$prim48458 = alloca %struct.ScmObj*, align 8
%current_45args46980 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46979)
store volatile %struct.ScmObj* %current_45args46980, %struct.ScmObj** %stackaddr$prim48458, align 8
%stackaddr$prim48459 = alloca %struct.ScmObj*, align 8
%f40156 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46980)
store volatile %struct.ScmObj* %f40156, %struct.ScmObj** %stackaddr$prim48459, align 8
%stackaddr$prim48460 = alloca %struct.ScmObj*, align 8
%current_45args46981 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46980)
store volatile %struct.ScmObj* %current_45args46981, %struct.ScmObj** %stackaddr$prim48460, align 8
%stackaddr$prim48461 = alloca %struct.ScmObj*, align 8
%lst40155 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46981)
store volatile %struct.ScmObj* %lst40155, %struct.ScmObj** %stackaddr$prim48461, align 8
%stackaddr$makeclosure48462 = alloca %struct.ScmObj*, align 8
%fptrToInt48463 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41641 to i64
%ae41641 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48463)
store volatile %struct.ScmObj* %ae41641, %struct.ScmObj** %stackaddr$makeclosure48462, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41641, %struct.ScmObj* %lst40155, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41641, %struct.ScmObj* %_37foldr140123, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41641, %struct.ScmObj* %k40487, i64 2)
%ae41642 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48464 = alloca %struct.ScmObj*, align 8
%fptrToInt48465 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41643 to i64
%ae41643 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48465)
store volatile %struct.ScmObj* %ae41643, %struct.ScmObj** %stackaddr$makeclosure48464, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41643, %struct.ScmObj* %f40156, i64 0)
%args46996$ae41641$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48466 = alloca %struct.ScmObj*, align 8
%args46996$ae41641$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41643, %struct.ScmObj* %args46996$ae41641$0)
store volatile %struct.ScmObj* %args46996$ae41641$1, %struct.ScmObj** %stackaddr$prim48466, align 8
%stackaddr$prim48467 = alloca %struct.ScmObj*, align 8
%args46996$ae41641$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41642, %struct.ScmObj* %args46996$ae41641$1)
store volatile %struct.ScmObj* %args46996$ae41641$2, %struct.ScmObj** %stackaddr$prim48467, align 8
%clofunc48468 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41641)
musttail call tailcc void %clofunc48468(%struct.ScmObj* %ae41641, %struct.ScmObj* %args46996$ae41641$2)
ret void
}

define tailcc void @proc_clo$ae41641(%struct.ScmObj* %env$ae41641,%struct.ScmObj* %current_45args46983) {
%stackaddr$env-ref48469 = alloca %struct.ScmObj*, align 8
%lst40155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41641, i64 0)
store %struct.ScmObj* %lst40155, %struct.ScmObj** %stackaddr$env-ref48469
%stackaddr$env-ref48470 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41641, i64 1)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48470
%stackaddr$env-ref48471 = alloca %struct.ScmObj*, align 8
%k40487 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41641, i64 2)
store %struct.ScmObj* %k40487, %struct.ScmObj** %stackaddr$env-ref48471
%stackaddr$prim48472 = alloca %struct.ScmObj*, align 8
%_95k40488 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46983)
store volatile %struct.ScmObj* %_95k40488, %struct.ScmObj** %stackaddr$prim48472, align 8
%stackaddr$prim48473 = alloca %struct.ScmObj*, align 8
%current_45args46984 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46983)
store volatile %struct.ScmObj* %current_45args46984, %struct.ScmObj** %stackaddr$prim48473, align 8
%stackaddr$prim48474 = alloca %struct.ScmObj*, align 8
%anf_45bind40279 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46984)
store volatile %struct.ScmObj* %anf_45bind40279, %struct.ScmObj** %stackaddr$prim48474, align 8
%ae41675 = call %struct.ScmObj* @const_init_null()
%args46986$_37foldr140123$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48475 = alloca %struct.ScmObj*, align 8
%args46986$_37foldr140123$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40155, %struct.ScmObj* %args46986$_37foldr140123$0)
store volatile %struct.ScmObj* %args46986$_37foldr140123$1, %struct.ScmObj** %stackaddr$prim48475, align 8
%stackaddr$prim48476 = alloca %struct.ScmObj*, align 8
%args46986$_37foldr140123$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41675, %struct.ScmObj* %args46986$_37foldr140123$1)
store volatile %struct.ScmObj* %args46986$_37foldr140123$2, %struct.ScmObj** %stackaddr$prim48476, align 8
%stackaddr$prim48477 = alloca %struct.ScmObj*, align 8
%args46986$_37foldr140123$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40279, %struct.ScmObj* %args46986$_37foldr140123$2)
store volatile %struct.ScmObj* %args46986$_37foldr140123$3, %struct.ScmObj** %stackaddr$prim48477, align 8
%stackaddr$prim48478 = alloca %struct.ScmObj*, align 8
%args46986$_37foldr140123$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40487, %struct.ScmObj* %args46986$_37foldr140123$3)
store volatile %struct.ScmObj* %args46986$_37foldr140123$4, %struct.ScmObj** %stackaddr$prim48478, align 8
%clofunc48479 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140123)
musttail call tailcc void %clofunc48479(%struct.ScmObj* %_37foldr140123, %struct.ScmObj* %args46986$_37foldr140123$4)
ret void
}

define tailcc void @proc_clo$ae41643(%struct.ScmObj* %env$ae41643,%struct.ScmObj* %current_45args46987) {
%stackaddr$env-ref48480 = alloca %struct.ScmObj*, align 8
%f40156 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41643, i64 0)
store %struct.ScmObj* %f40156, %struct.ScmObj** %stackaddr$env-ref48480
%stackaddr$prim48481 = alloca %struct.ScmObj*, align 8
%k40489 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46987)
store volatile %struct.ScmObj* %k40489, %struct.ScmObj** %stackaddr$prim48481, align 8
%stackaddr$prim48482 = alloca %struct.ScmObj*, align 8
%current_45args46988 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46987)
store volatile %struct.ScmObj* %current_45args46988, %struct.ScmObj** %stackaddr$prim48482, align 8
%stackaddr$prim48483 = alloca %struct.ScmObj*, align 8
%v40158 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46988)
store volatile %struct.ScmObj* %v40158, %struct.ScmObj** %stackaddr$prim48483, align 8
%stackaddr$prim48484 = alloca %struct.ScmObj*, align 8
%current_45args46989 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46988)
store volatile %struct.ScmObj* %current_45args46989, %struct.ScmObj** %stackaddr$prim48484, align 8
%stackaddr$prim48485 = alloca %struct.ScmObj*, align 8
%r40157 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46989)
store volatile %struct.ScmObj* %r40157, %struct.ScmObj** %stackaddr$prim48485, align 8
%stackaddr$makeclosure48486 = alloca %struct.ScmObj*, align 8
%fptrToInt48487 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41645 to i64
%ae41645 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48487)
store volatile %struct.ScmObj* %ae41645, %struct.ScmObj** %stackaddr$makeclosure48486, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41645, %struct.ScmObj* %r40157, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41645, %struct.ScmObj* %k40489, i64 1)
%args46995$f40156$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48488 = alloca %struct.ScmObj*, align 8
%args46995$f40156$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v40158, %struct.ScmObj* %args46995$f40156$0)
store volatile %struct.ScmObj* %args46995$f40156$1, %struct.ScmObj** %stackaddr$prim48488, align 8
%stackaddr$prim48489 = alloca %struct.ScmObj*, align 8
%args46995$f40156$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41645, %struct.ScmObj* %args46995$f40156$1)
store volatile %struct.ScmObj* %args46995$f40156$2, %struct.ScmObj** %stackaddr$prim48489, align 8
%clofunc48490 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40156)
musttail call tailcc void %clofunc48490(%struct.ScmObj* %f40156, %struct.ScmObj* %args46995$f40156$2)
ret void
}

define tailcc void @proc_clo$ae41645(%struct.ScmObj* %env$ae41645,%struct.ScmObj* %current_45args46991) {
%stackaddr$env-ref48491 = alloca %struct.ScmObj*, align 8
%r40157 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41645, i64 0)
store %struct.ScmObj* %r40157, %struct.ScmObj** %stackaddr$env-ref48491
%stackaddr$env-ref48492 = alloca %struct.ScmObj*, align 8
%k40489 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41645, i64 1)
store %struct.ScmObj* %k40489, %struct.ScmObj** %stackaddr$env-ref48492
%stackaddr$prim48493 = alloca %struct.ScmObj*, align 8
%_95k40490 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46991)
store volatile %struct.ScmObj* %_95k40490, %struct.ScmObj** %stackaddr$prim48493, align 8
%stackaddr$prim48494 = alloca %struct.ScmObj*, align 8
%current_45args46992 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46991)
store volatile %struct.ScmObj* %current_45args46992, %struct.ScmObj** %stackaddr$prim48494, align 8
%stackaddr$prim48495 = alloca %struct.ScmObj*, align 8
%anf_45bind40278 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46992)
store volatile %struct.ScmObj* %anf_45bind40278, %struct.ScmObj** %stackaddr$prim48495, align 8
%stackaddr$prim48496 = alloca %struct.ScmObj*, align 8
%cpsprim40491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40278, %struct.ScmObj* %r40157)
store volatile %struct.ScmObj* %cpsprim40491, %struct.ScmObj** %stackaddr$prim48496, align 8
%ae41650 = call %struct.ScmObj* @const_init_int(i64 0)
%args46994$k40489$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48497 = alloca %struct.ScmObj*, align 8
%args46994$k40489$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40491, %struct.ScmObj* %args46994$k40489$0)
store volatile %struct.ScmObj* %args46994$k40489$1, %struct.ScmObj** %stackaddr$prim48497, align 8
%stackaddr$prim48498 = alloca %struct.ScmObj*, align 8
%args46994$k40489$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41650, %struct.ScmObj* %args46994$k40489$1)
store volatile %struct.ScmObj* %args46994$k40489$2, %struct.ScmObj** %stackaddr$prim48498, align 8
%clofunc48499 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40489)
musttail call tailcc void %clofunc48499(%struct.ScmObj* %k40489, %struct.ScmObj* %args46994$k40489$2)
ret void
}

define tailcc void @proc_clo$ae41254(%struct.ScmObj* %env$ae41254,%struct.ScmObj* %current_45args46999) {
%stackaddr$env-ref48500 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41254, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref48500
%stackaddr$env-ref48501 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41254, i64 1)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48501
%stackaddr$prim48502 = alloca %struct.ScmObj*, align 8
%k40492 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46999)
store volatile %struct.ScmObj* %k40492, %struct.ScmObj** %stackaddr$prim48502, align 8
%stackaddr$prim48503 = alloca %struct.ScmObj*, align 8
%current_45args47000 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46999)
store volatile %struct.ScmObj* %current_45args47000, %struct.ScmObj** %stackaddr$prim48503, align 8
%stackaddr$prim48504 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47000)
store volatile %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$prim48504, align 8
%ae41256 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48505 = alloca %struct.ScmObj*, align 8
%fptrToInt48506 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41257 to i64
%ae41257 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48506)
store volatile %struct.ScmObj* %ae41257, %struct.ScmObj** %stackaddr$makeclosure48505, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41257, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41257, %struct.ScmObj* %_37foldr40129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41257, %struct.ScmObj* %_37foldr140123, i64 2)
%args47057$k40492$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48507 = alloca %struct.ScmObj*, align 8
%args47057$k40492$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41257, %struct.ScmObj* %args47057$k40492$0)
store volatile %struct.ScmObj* %args47057$k40492$1, %struct.ScmObj** %stackaddr$prim48507, align 8
%stackaddr$prim48508 = alloca %struct.ScmObj*, align 8
%args47057$k40492$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41256, %struct.ScmObj* %args47057$k40492$1)
store volatile %struct.ScmObj* %args47057$k40492$2, %struct.ScmObj** %stackaddr$prim48508, align 8
%clofunc48509 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40492)
musttail call tailcc void %clofunc48509(%struct.ScmObj* %k40492, %struct.ScmObj* %args47057$k40492$2)
ret void
}

define tailcc void @proc_clo$ae41257(%struct.ScmObj* %env$ae41257,%struct.ScmObj* %args4013040493) {
%stackaddr$env-ref48510 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41257, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref48510
%stackaddr$env-ref48511 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41257, i64 1)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref48511
%stackaddr$env-ref48512 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41257, i64 2)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48512
%stackaddr$prim48513 = alloca %struct.ScmObj*, align 8
%k40494 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4013040493)
store volatile %struct.ScmObj* %k40494, %struct.ScmObj** %stackaddr$prim48513, align 8
%stackaddr$prim48514 = alloca %struct.ScmObj*, align 8
%args40130 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4013040493)
store volatile %struct.ScmObj* %args40130, %struct.ScmObj** %stackaddr$prim48514, align 8
%stackaddr$prim48515 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40130)
store volatile %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$prim48515, align 8
%stackaddr$prim48516 = alloca %struct.ScmObj*, align 8
%anf_45bind40265 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40130)
store volatile %struct.ScmObj* %anf_45bind40265, %struct.ScmObj** %stackaddr$prim48516, align 8
%stackaddr$prim48517 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40265)
store volatile %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$prim48517, align 8
%stackaddr$prim48518 = alloca %struct.ScmObj*, align 8
%anf_45bind40266 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40130)
store volatile %struct.ScmObj* %anf_45bind40266, %struct.ScmObj** %stackaddr$prim48518, align 8
%stackaddr$prim48519 = alloca %struct.ScmObj*, align 8
%lsts40131 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40266)
store volatile %struct.ScmObj* %lsts40131, %struct.ScmObj** %stackaddr$prim48519, align 8
%stackaddr$makeclosure48520 = alloca %struct.ScmObj*, align 8
%fptrToInt48521 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41265 to i64
%ae41265 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt48521)
store volatile %struct.ScmObj* %ae41265, %struct.ScmObj** %stackaddr$makeclosure48520, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41265, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41265, %struct.ScmObj* %f40133, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41265, %struct.ScmObj* %acc40132, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41265, %struct.ScmObj* %lsts40131, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41265, %struct.ScmObj* %_37foldr40129, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41265, %struct.ScmObj* %k40494, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41265, %struct.ScmObj* %_37foldr140123, i64 6)
%ae41266 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48522 = alloca %struct.ScmObj*, align 8
%fptrToInt48523 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41267 to i64
%ae41267 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48523)
store volatile %struct.ScmObj* %ae41267, %struct.ScmObj** %stackaddr$makeclosure48522, align 8
%args47056$ae41265$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48524 = alloca %struct.ScmObj*, align 8
%args47056$ae41265$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41267, %struct.ScmObj* %args47056$ae41265$0)
store volatile %struct.ScmObj* %args47056$ae41265$1, %struct.ScmObj** %stackaddr$prim48524, align 8
%stackaddr$prim48525 = alloca %struct.ScmObj*, align 8
%args47056$ae41265$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41266, %struct.ScmObj* %args47056$ae41265$1)
store volatile %struct.ScmObj* %args47056$ae41265$2, %struct.ScmObj** %stackaddr$prim48525, align 8
%clofunc48526 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41265)
musttail call tailcc void %clofunc48526(%struct.ScmObj* %ae41265, %struct.ScmObj* %args47056$ae41265$2)
ret void
}

define tailcc void @proc_clo$ae41265(%struct.ScmObj* %env$ae41265,%struct.ScmObj* %current_45args47002) {
%stackaddr$env-ref48527 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41265, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref48527
%stackaddr$env-ref48528 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41265, i64 1)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref48528
%stackaddr$env-ref48529 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41265, i64 2)
store %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$env-ref48529
%stackaddr$env-ref48530 = alloca %struct.ScmObj*, align 8
%lsts40131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41265, i64 3)
store %struct.ScmObj* %lsts40131, %struct.ScmObj** %stackaddr$env-ref48530
%stackaddr$env-ref48531 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41265, i64 4)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref48531
%stackaddr$env-ref48532 = alloca %struct.ScmObj*, align 8
%k40494 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41265, i64 5)
store %struct.ScmObj* %k40494, %struct.ScmObj** %stackaddr$env-ref48532
%stackaddr$env-ref48533 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41265, i64 6)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48533
%stackaddr$prim48534 = alloca %struct.ScmObj*, align 8
%_95k40495 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47002)
store volatile %struct.ScmObj* %_95k40495, %struct.ScmObj** %stackaddr$prim48534, align 8
%stackaddr$prim48535 = alloca %struct.ScmObj*, align 8
%current_45args47003 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47002)
store volatile %struct.ScmObj* %current_45args47003, %struct.ScmObj** %stackaddr$prim48535, align 8
%stackaddr$prim48536 = alloca %struct.ScmObj*, align 8
%anf_45bind40267 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47003)
store volatile %struct.ScmObj* %anf_45bind40267, %struct.ScmObj** %stackaddr$prim48536, align 8
%stackaddr$makeclosure48537 = alloca %struct.ScmObj*, align 8
%fptrToInt48538 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41297 to i64
%ae41297 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt48538)
store volatile %struct.ScmObj* %ae41297, %struct.ScmObj** %stackaddr$makeclosure48537, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41297, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41297, %struct.ScmObj* %f40133, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41297, %struct.ScmObj* %acc40132, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41297, %struct.ScmObj* %lsts40131, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41297, %struct.ScmObj* %_37foldr40129, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41297, %struct.ScmObj* %k40494, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41297, %struct.ScmObj* %_37foldr140123, i64 6)
%ae41299 = call %struct.ScmObj* @const_init_false()
%args47049$_37foldr140123$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48539 = alloca %struct.ScmObj*, align 8
%args47049$_37foldr140123$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40131, %struct.ScmObj* %args47049$_37foldr140123$0)
store volatile %struct.ScmObj* %args47049$_37foldr140123$1, %struct.ScmObj** %stackaddr$prim48539, align 8
%stackaddr$prim48540 = alloca %struct.ScmObj*, align 8
%args47049$_37foldr140123$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41299, %struct.ScmObj* %args47049$_37foldr140123$1)
store volatile %struct.ScmObj* %args47049$_37foldr140123$2, %struct.ScmObj** %stackaddr$prim48540, align 8
%stackaddr$prim48541 = alloca %struct.ScmObj*, align 8
%args47049$_37foldr140123$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40267, %struct.ScmObj* %args47049$_37foldr140123$2)
store volatile %struct.ScmObj* %args47049$_37foldr140123$3, %struct.ScmObj** %stackaddr$prim48541, align 8
%stackaddr$prim48542 = alloca %struct.ScmObj*, align 8
%args47049$_37foldr140123$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41297, %struct.ScmObj* %args47049$_37foldr140123$3)
store volatile %struct.ScmObj* %args47049$_37foldr140123$4, %struct.ScmObj** %stackaddr$prim48542, align 8
%clofunc48543 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140123)
musttail call tailcc void %clofunc48543(%struct.ScmObj* %_37foldr140123, %struct.ScmObj* %args47049$_37foldr140123$4)
ret void
}

define tailcc void @proc_clo$ae41297(%struct.ScmObj* %env$ae41297,%struct.ScmObj* %current_45args47005) {
%stackaddr$env-ref48544 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41297, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref48544
%stackaddr$env-ref48545 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41297, i64 1)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref48545
%stackaddr$env-ref48546 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41297, i64 2)
store %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$env-ref48546
%stackaddr$env-ref48547 = alloca %struct.ScmObj*, align 8
%lsts40131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41297, i64 3)
store %struct.ScmObj* %lsts40131, %struct.ScmObj** %stackaddr$env-ref48547
%stackaddr$env-ref48548 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41297, i64 4)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref48548
%stackaddr$env-ref48549 = alloca %struct.ScmObj*, align 8
%k40494 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41297, i64 5)
store %struct.ScmObj* %k40494, %struct.ScmObj** %stackaddr$env-ref48549
%stackaddr$env-ref48550 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41297, i64 6)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48550
%stackaddr$prim48551 = alloca %struct.ScmObj*, align 8
%_95k40496 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47005)
store volatile %struct.ScmObj* %_95k40496, %struct.ScmObj** %stackaddr$prim48551, align 8
%stackaddr$prim48552 = alloca %struct.ScmObj*, align 8
%current_45args47006 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47005)
store volatile %struct.ScmObj* %current_45args47006, %struct.ScmObj** %stackaddr$prim48552, align 8
%stackaddr$prim48553 = alloca %struct.ScmObj*, align 8
%anf_45bind40268 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47006)
store volatile %struct.ScmObj* %anf_45bind40268, %struct.ScmObj** %stackaddr$prim48553, align 8
%truthy$cmp48554 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40268)
%cmp$cmp48554 = icmp eq i64 %truthy$cmp48554, 1
br i1 %cmp$cmp48554, label %truebranch$cmp48554, label %falsebranch$cmp48554
truebranch$cmp48554:
%ae41308 = call %struct.ScmObj* @const_init_int(i64 0)
%args47008$k40494$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48555 = alloca %struct.ScmObj*, align 8
%args47008$k40494$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40132, %struct.ScmObj* %args47008$k40494$0)
store volatile %struct.ScmObj* %args47008$k40494$1, %struct.ScmObj** %stackaddr$prim48555, align 8
%stackaddr$prim48556 = alloca %struct.ScmObj*, align 8
%args47008$k40494$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41308, %struct.ScmObj* %args47008$k40494$1)
store volatile %struct.ScmObj* %args47008$k40494$2, %struct.ScmObj** %stackaddr$prim48556, align 8
%clofunc48557 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40494)
musttail call tailcc void %clofunc48557(%struct.ScmObj* %k40494, %struct.ScmObj* %args47008$k40494$2)
ret void
falsebranch$cmp48554:
%stackaddr$makeclosure48558 = alloca %struct.ScmObj*, align 8
%fptrToInt48559 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41313 to i64
%ae41313 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt48559)
store volatile %struct.ScmObj* %ae41313, %struct.ScmObj** %stackaddr$makeclosure48558, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41313, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41313, %struct.ScmObj* %f40133, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41313, %struct.ScmObj* %acc40132, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41313, %struct.ScmObj* %lsts40131, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41313, %struct.ScmObj* %_37foldr40129, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41313, %struct.ScmObj* %k40494, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41313, %struct.ScmObj* %_37foldr140123, i64 6)
%ae41314 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48560 = alloca %struct.ScmObj*, align 8
%fptrToInt48561 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41315 to i64
%ae41315 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48561)
store volatile %struct.ScmObj* %ae41315, %struct.ScmObj** %stackaddr$makeclosure48560, align 8
%args47048$ae41313$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48562 = alloca %struct.ScmObj*, align 8
%args47048$ae41313$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41315, %struct.ScmObj* %args47048$ae41313$0)
store volatile %struct.ScmObj* %args47048$ae41313$1, %struct.ScmObj** %stackaddr$prim48562, align 8
%stackaddr$prim48563 = alloca %struct.ScmObj*, align 8
%args47048$ae41313$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41314, %struct.ScmObj* %args47048$ae41313$1)
store volatile %struct.ScmObj* %args47048$ae41313$2, %struct.ScmObj** %stackaddr$prim48563, align 8
%clofunc48564 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41313)
musttail call tailcc void %clofunc48564(%struct.ScmObj* %ae41313, %struct.ScmObj* %args47048$ae41313$2)
ret void
}

define tailcc void @proc_clo$ae41313(%struct.ScmObj* %env$ae41313,%struct.ScmObj* %current_45args47009) {
%stackaddr$env-ref48565 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41313, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref48565
%stackaddr$env-ref48566 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41313, i64 1)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref48566
%stackaddr$env-ref48567 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41313, i64 2)
store %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$env-ref48567
%stackaddr$env-ref48568 = alloca %struct.ScmObj*, align 8
%lsts40131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41313, i64 3)
store %struct.ScmObj* %lsts40131, %struct.ScmObj** %stackaddr$env-ref48568
%stackaddr$env-ref48569 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41313, i64 4)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref48569
%stackaddr$env-ref48570 = alloca %struct.ScmObj*, align 8
%k40494 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41313, i64 5)
store %struct.ScmObj* %k40494, %struct.ScmObj** %stackaddr$env-ref48570
%stackaddr$env-ref48571 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41313, i64 6)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48571
%stackaddr$prim48572 = alloca %struct.ScmObj*, align 8
%_95k40497 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47009)
store volatile %struct.ScmObj* %_95k40497, %struct.ScmObj** %stackaddr$prim48572, align 8
%stackaddr$prim48573 = alloca %struct.ScmObj*, align 8
%current_45args47010 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47009)
store volatile %struct.ScmObj* %current_45args47010, %struct.ScmObj** %stackaddr$prim48573, align 8
%stackaddr$prim48574 = alloca %struct.ScmObj*, align 8
%anf_45bind40269 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47010)
store volatile %struct.ScmObj* %anf_45bind40269, %struct.ScmObj** %stackaddr$prim48574, align 8
%stackaddr$makeclosure48575 = alloca %struct.ScmObj*, align 8
%fptrToInt48576 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41334 to i64
%ae41334 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt48576)
store volatile %struct.ScmObj* %ae41334, %struct.ScmObj** %stackaddr$makeclosure48575, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41334, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41334, %struct.ScmObj* %f40133, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41334, %struct.ScmObj* %acc40132, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41334, %struct.ScmObj* %lsts40131, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41334, %struct.ScmObj* %_37foldr40129, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41334, %struct.ScmObj* %k40494, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41334, %struct.ScmObj* %_37foldr140123, i64 6)
%args47043$_37map140119$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48577 = alloca %struct.ScmObj*, align 8
%args47043$_37map140119$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40131, %struct.ScmObj* %args47043$_37map140119$0)
store volatile %struct.ScmObj* %args47043$_37map140119$1, %struct.ScmObj** %stackaddr$prim48577, align 8
%stackaddr$prim48578 = alloca %struct.ScmObj*, align 8
%args47043$_37map140119$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40269, %struct.ScmObj* %args47043$_37map140119$1)
store volatile %struct.ScmObj* %args47043$_37map140119$2, %struct.ScmObj** %stackaddr$prim48578, align 8
%stackaddr$prim48579 = alloca %struct.ScmObj*, align 8
%args47043$_37map140119$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41334, %struct.ScmObj* %args47043$_37map140119$2)
store volatile %struct.ScmObj* %args47043$_37map140119$3, %struct.ScmObj** %stackaddr$prim48579, align 8
%clofunc48580 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140119)
musttail call tailcc void %clofunc48580(%struct.ScmObj* %_37map140119, %struct.ScmObj* %args47043$_37map140119$3)
ret void
}

define tailcc void @proc_clo$ae41334(%struct.ScmObj* %env$ae41334,%struct.ScmObj* %current_45args47012) {
%stackaddr$env-ref48581 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41334, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref48581
%stackaddr$env-ref48582 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41334, i64 1)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref48582
%stackaddr$env-ref48583 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41334, i64 2)
store %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$env-ref48583
%stackaddr$env-ref48584 = alloca %struct.ScmObj*, align 8
%lsts40131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41334, i64 3)
store %struct.ScmObj* %lsts40131, %struct.ScmObj** %stackaddr$env-ref48584
%stackaddr$env-ref48585 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41334, i64 4)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref48585
%stackaddr$env-ref48586 = alloca %struct.ScmObj*, align 8
%k40494 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41334, i64 5)
store %struct.ScmObj* %k40494, %struct.ScmObj** %stackaddr$env-ref48586
%stackaddr$env-ref48587 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41334, i64 6)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48587
%stackaddr$prim48588 = alloca %struct.ScmObj*, align 8
%_95k40498 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47012)
store volatile %struct.ScmObj* %_95k40498, %struct.ScmObj** %stackaddr$prim48588, align 8
%stackaddr$prim48589 = alloca %struct.ScmObj*, align 8
%current_45args47013 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47012)
store volatile %struct.ScmObj* %current_45args47013, %struct.ScmObj** %stackaddr$prim48589, align 8
%stackaddr$prim48590 = alloca %struct.ScmObj*, align 8
%lsts_4340138 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47013)
store volatile %struct.ScmObj* %lsts_4340138, %struct.ScmObj** %stackaddr$prim48590, align 8
%stackaddr$makeclosure48591 = alloca %struct.ScmObj*, align 8
%fptrToInt48592 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41337 to i64
%ae41337 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt48592)
store volatile %struct.ScmObj* %ae41337, %struct.ScmObj** %stackaddr$makeclosure48591, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41337, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41337, %struct.ScmObj* %f40133, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41337, %struct.ScmObj* %acc40132, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41337, %struct.ScmObj* %lsts40131, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41337, %struct.ScmObj* %_37foldr40129, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41337, %struct.ScmObj* %k40494, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41337, %struct.ScmObj* %_37foldr140123, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae41337, %struct.ScmObj* %lsts_4340138, i64 7)
%ae41338 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48593 = alloca %struct.ScmObj*, align 8
%fptrToInt48594 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41339 to i64
%ae41339 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48594)
store volatile %struct.ScmObj* %ae41339, %struct.ScmObj** %stackaddr$makeclosure48593, align 8
%args47042$ae41337$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48595 = alloca %struct.ScmObj*, align 8
%args47042$ae41337$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41339, %struct.ScmObj* %args47042$ae41337$0)
store volatile %struct.ScmObj* %args47042$ae41337$1, %struct.ScmObj** %stackaddr$prim48595, align 8
%stackaddr$prim48596 = alloca %struct.ScmObj*, align 8
%args47042$ae41337$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41338, %struct.ScmObj* %args47042$ae41337$1)
store volatile %struct.ScmObj* %args47042$ae41337$2, %struct.ScmObj** %stackaddr$prim48596, align 8
%clofunc48597 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41337)
musttail call tailcc void %clofunc48597(%struct.ScmObj* %ae41337, %struct.ScmObj* %args47042$ae41337$2)
ret void
}

define tailcc void @proc_clo$ae41337(%struct.ScmObj* %env$ae41337,%struct.ScmObj* %current_45args47015) {
%stackaddr$env-ref48598 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41337, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref48598
%stackaddr$env-ref48599 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41337, i64 1)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref48599
%stackaddr$env-ref48600 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41337, i64 2)
store %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$env-ref48600
%stackaddr$env-ref48601 = alloca %struct.ScmObj*, align 8
%lsts40131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41337, i64 3)
store %struct.ScmObj* %lsts40131, %struct.ScmObj** %stackaddr$env-ref48601
%stackaddr$env-ref48602 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41337, i64 4)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref48602
%stackaddr$env-ref48603 = alloca %struct.ScmObj*, align 8
%k40494 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41337, i64 5)
store %struct.ScmObj* %k40494, %struct.ScmObj** %stackaddr$env-ref48603
%stackaddr$env-ref48604 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41337, i64 6)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48604
%stackaddr$env-ref48605 = alloca %struct.ScmObj*, align 8
%lsts_4340138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41337, i64 7)
store %struct.ScmObj* %lsts_4340138, %struct.ScmObj** %stackaddr$env-ref48605
%stackaddr$prim48606 = alloca %struct.ScmObj*, align 8
%_95k40499 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47015)
store volatile %struct.ScmObj* %_95k40499, %struct.ScmObj** %stackaddr$prim48606, align 8
%stackaddr$prim48607 = alloca %struct.ScmObj*, align 8
%current_45args47016 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47015)
store volatile %struct.ScmObj* %current_45args47016, %struct.ScmObj** %stackaddr$prim48607, align 8
%stackaddr$prim48608 = alloca %struct.ScmObj*, align 8
%anf_45bind40270 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47016)
store volatile %struct.ScmObj* %anf_45bind40270, %struct.ScmObj** %stackaddr$prim48608, align 8
%stackaddr$makeclosure48609 = alloca %struct.ScmObj*, align 8
%fptrToInt48610 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41358 to i64
%ae41358 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt48610)
store volatile %struct.ScmObj* %ae41358, %struct.ScmObj** %stackaddr$makeclosure48609, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41358, %struct.ScmObj* %f40133, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41358, %struct.ScmObj* %acc40132, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41358, %struct.ScmObj* %_37foldr40129, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41358, %struct.ScmObj* %k40494, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41358, %struct.ScmObj* %_37foldr140123, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41358, %struct.ScmObj* %lsts_4340138, i64 5)
%args47037$_37map140119$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48611 = alloca %struct.ScmObj*, align 8
%args47037$_37map140119$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40131, %struct.ScmObj* %args47037$_37map140119$0)
store volatile %struct.ScmObj* %args47037$_37map140119$1, %struct.ScmObj** %stackaddr$prim48611, align 8
%stackaddr$prim48612 = alloca %struct.ScmObj*, align 8
%args47037$_37map140119$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40270, %struct.ScmObj* %args47037$_37map140119$1)
store volatile %struct.ScmObj* %args47037$_37map140119$2, %struct.ScmObj** %stackaddr$prim48612, align 8
%stackaddr$prim48613 = alloca %struct.ScmObj*, align 8
%args47037$_37map140119$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41358, %struct.ScmObj* %args47037$_37map140119$2)
store volatile %struct.ScmObj* %args47037$_37map140119$3, %struct.ScmObj** %stackaddr$prim48613, align 8
%clofunc48614 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140119)
musttail call tailcc void %clofunc48614(%struct.ScmObj* %_37map140119, %struct.ScmObj* %args47037$_37map140119$3)
ret void
}

define tailcc void @proc_clo$ae41358(%struct.ScmObj* %env$ae41358,%struct.ScmObj* %current_45args47018) {
%stackaddr$env-ref48615 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41358, i64 0)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref48615
%stackaddr$env-ref48616 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41358, i64 1)
store %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$env-ref48616
%stackaddr$env-ref48617 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41358, i64 2)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref48617
%stackaddr$env-ref48618 = alloca %struct.ScmObj*, align 8
%k40494 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41358, i64 3)
store %struct.ScmObj* %k40494, %struct.ScmObj** %stackaddr$env-ref48618
%stackaddr$env-ref48619 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41358, i64 4)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48619
%stackaddr$env-ref48620 = alloca %struct.ScmObj*, align 8
%lsts_4340138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41358, i64 5)
store %struct.ScmObj* %lsts_4340138, %struct.ScmObj** %stackaddr$env-ref48620
%stackaddr$prim48621 = alloca %struct.ScmObj*, align 8
%_95k40500 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47018)
store volatile %struct.ScmObj* %_95k40500, %struct.ScmObj** %stackaddr$prim48621, align 8
%stackaddr$prim48622 = alloca %struct.ScmObj*, align 8
%current_45args47019 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47018)
store volatile %struct.ScmObj* %current_45args47019, %struct.ScmObj** %stackaddr$prim48622, align 8
%stackaddr$prim48623 = alloca %struct.ScmObj*, align 8
%vs40136 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47019)
store volatile %struct.ScmObj* %vs40136, %struct.ScmObj** %stackaddr$prim48623, align 8
%stackaddr$makeclosure48624 = alloca %struct.ScmObj*, align 8
%fptrToInt48625 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41361 to i64
%ae41361 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt48625)
store volatile %struct.ScmObj* %ae41361, %struct.ScmObj** %stackaddr$makeclosure48624, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41361, %struct.ScmObj* %f40133, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41361, %struct.ScmObj* %acc40132, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41361, %struct.ScmObj* %_37foldr40129, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41361, %struct.ScmObj* %k40494, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41361, %struct.ScmObj* %_37foldr140123, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41361, %struct.ScmObj* %lsts_4340138, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41361, %struct.ScmObj* %vs40136, i64 6)
%ae41362 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48626 = alloca %struct.ScmObj*, align 8
%fptrToInt48627 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41363 to i64
%ae41363 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48627)
store volatile %struct.ScmObj* %ae41363, %struct.ScmObj** %stackaddr$makeclosure48626, align 8
%args47036$ae41361$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48628 = alloca %struct.ScmObj*, align 8
%args47036$ae41361$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41363, %struct.ScmObj* %args47036$ae41361$0)
store volatile %struct.ScmObj* %args47036$ae41361$1, %struct.ScmObj** %stackaddr$prim48628, align 8
%stackaddr$prim48629 = alloca %struct.ScmObj*, align 8
%args47036$ae41361$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41362, %struct.ScmObj* %args47036$ae41361$1)
store volatile %struct.ScmObj* %args47036$ae41361$2, %struct.ScmObj** %stackaddr$prim48629, align 8
%clofunc48630 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41361)
musttail call tailcc void %clofunc48630(%struct.ScmObj* %ae41361, %struct.ScmObj* %args47036$ae41361$2)
ret void
}

define tailcc void @proc_clo$ae41361(%struct.ScmObj* %env$ae41361,%struct.ScmObj* %current_45args47021) {
%stackaddr$env-ref48631 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41361, i64 0)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref48631
%stackaddr$env-ref48632 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41361, i64 1)
store %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$env-ref48632
%stackaddr$env-ref48633 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41361, i64 2)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref48633
%stackaddr$env-ref48634 = alloca %struct.ScmObj*, align 8
%k40494 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41361, i64 3)
store %struct.ScmObj* %k40494, %struct.ScmObj** %stackaddr$env-ref48634
%stackaddr$env-ref48635 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41361, i64 4)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48635
%stackaddr$env-ref48636 = alloca %struct.ScmObj*, align 8
%lsts_4340138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41361, i64 5)
store %struct.ScmObj* %lsts_4340138, %struct.ScmObj** %stackaddr$env-ref48636
%stackaddr$env-ref48637 = alloca %struct.ScmObj*, align 8
%vs40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41361, i64 6)
store %struct.ScmObj* %vs40136, %struct.ScmObj** %stackaddr$env-ref48637
%stackaddr$prim48638 = alloca %struct.ScmObj*, align 8
%_95k40501 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47021)
store volatile %struct.ScmObj* %_95k40501, %struct.ScmObj** %stackaddr$prim48638, align 8
%stackaddr$prim48639 = alloca %struct.ScmObj*, align 8
%current_45args47022 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47021)
store volatile %struct.ScmObj* %current_45args47022, %struct.ScmObj** %stackaddr$prim48639, align 8
%stackaddr$prim48640 = alloca %struct.ScmObj*, align 8
%anf_45bind40271 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47022)
store volatile %struct.ScmObj* %anf_45bind40271, %struct.ScmObj** %stackaddr$prim48640, align 8
%stackaddr$prim48641 = alloca %struct.ScmObj*, align 8
%anf_45bind40272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40132, %struct.ScmObj* %lsts_4340138)
store volatile %struct.ScmObj* %anf_45bind40272, %struct.ScmObj** %stackaddr$prim48641, align 8
%stackaddr$prim48642 = alloca %struct.ScmObj*, align 8
%anf_45bind40273 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40133, %struct.ScmObj* %anf_45bind40272)
store volatile %struct.ScmObj* %anf_45bind40273, %struct.ScmObj** %stackaddr$prim48642, align 8
%stackaddr$makeclosure48643 = alloca %struct.ScmObj*, align 8
%fptrToInt48644 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41387 to i64
%ae41387 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt48644)
store volatile %struct.ScmObj* %ae41387, %struct.ScmObj** %stackaddr$makeclosure48643, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41387, %struct.ScmObj* %f40133, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41387, %struct.ScmObj* %anf_45bind40271, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41387, %struct.ScmObj* %k40494, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41387, %struct.ScmObj* %_37foldr140123, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41387, %struct.ScmObj* %vs40136, i64 4)
%stackaddr$prim48645 = alloca %struct.ScmObj*, align 8
%cpsargs40505 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41387, %struct.ScmObj* %anf_45bind40273)
store volatile %struct.ScmObj* %cpsargs40505, %struct.ScmObj** %stackaddr$prim48645, align 8
%clofunc48646 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr40129)
musttail call tailcc void %clofunc48646(%struct.ScmObj* %_37foldr40129, %struct.ScmObj* %cpsargs40505)
ret void
}

define tailcc void @proc_clo$ae41387(%struct.ScmObj* %env$ae41387,%struct.ScmObj* %current_45args47024) {
%stackaddr$env-ref48647 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41387, i64 0)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref48647
%stackaddr$env-ref48648 = alloca %struct.ScmObj*, align 8
%anf_45bind40271 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41387, i64 1)
store %struct.ScmObj* %anf_45bind40271, %struct.ScmObj** %stackaddr$env-ref48648
%stackaddr$env-ref48649 = alloca %struct.ScmObj*, align 8
%k40494 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41387, i64 2)
store %struct.ScmObj* %k40494, %struct.ScmObj** %stackaddr$env-ref48649
%stackaddr$env-ref48650 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41387, i64 3)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48650
%stackaddr$env-ref48651 = alloca %struct.ScmObj*, align 8
%vs40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41387, i64 4)
store %struct.ScmObj* %vs40136, %struct.ScmObj** %stackaddr$env-ref48651
%stackaddr$prim48652 = alloca %struct.ScmObj*, align 8
%_95k40502 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47024)
store volatile %struct.ScmObj* %_95k40502, %struct.ScmObj** %stackaddr$prim48652, align 8
%stackaddr$prim48653 = alloca %struct.ScmObj*, align 8
%current_45args47025 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47024)
store volatile %struct.ScmObj* %current_45args47025, %struct.ScmObj** %stackaddr$prim48653, align 8
%stackaddr$prim48654 = alloca %struct.ScmObj*, align 8
%anf_45bind40274 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47025)
store volatile %struct.ScmObj* %anf_45bind40274, %struct.ScmObj** %stackaddr$prim48654, align 8
%ae41392 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48655 = alloca %struct.ScmObj*, align 8
%anf_45bind40275 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40274, %struct.ScmObj* %ae41392)
store volatile %struct.ScmObj* %anf_45bind40275, %struct.ScmObj** %stackaddr$prim48655, align 8
%stackaddr$makeclosure48656 = alloca %struct.ScmObj*, align 8
%fptrToInt48657 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41394 to i64
%ae41394 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48657)
store volatile %struct.ScmObj* %ae41394, %struct.ScmObj** %stackaddr$makeclosure48656, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41394, %struct.ScmObj* %f40133, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41394, %struct.ScmObj* %k40494, i64 1)
%args47030$_37foldr140123$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48658 = alloca %struct.ScmObj*, align 8
%args47030$_37foldr140123$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs40136, %struct.ScmObj* %args47030$_37foldr140123$0)
store volatile %struct.ScmObj* %args47030$_37foldr140123$1, %struct.ScmObj** %stackaddr$prim48658, align 8
%stackaddr$prim48659 = alloca %struct.ScmObj*, align 8
%args47030$_37foldr140123$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40275, %struct.ScmObj* %args47030$_37foldr140123$1)
store volatile %struct.ScmObj* %args47030$_37foldr140123$2, %struct.ScmObj** %stackaddr$prim48659, align 8
%stackaddr$prim48660 = alloca %struct.ScmObj*, align 8
%args47030$_37foldr140123$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40271, %struct.ScmObj* %args47030$_37foldr140123$2)
store volatile %struct.ScmObj* %args47030$_37foldr140123$3, %struct.ScmObj** %stackaddr$prim48660, align 8
%stackaddr$prim48661 = alloca %struct.ScmObj*, align 8
%args47030$_37foldr140123$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41394, %struct.ScmObj* %args47030$_37foldr140123$3)
store volatile %struct.ScmObj* %args47030$_37foldr140123$4, %struct.ScmObj** %stackaddr$prim48661, align 8
%clofunc48662 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140123)
musttail call tailcc void %clofunc48662(%struct.ScmObj* %_37foldr140123, %struct.ScmObj* %args47030$_37foldr140123$4)
ret void
}

define tailcc void @proc_clo$ae41394(%struct.ScmObj* %env$ae41394,%struct.ScmObj* %current_45args47027) {
%stackaddr$env-ref48663 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41394, i64 0)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref48663
%stackaddr$env-ref48664 = alloca %struct.ScmObj*, align 8
%k40494 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41394, i64 1)
store %struct.ScmObj* %k40494, %struct.ScmObj** %stackaddr$env-ref48664
%stackaddr$prim48665 = alloca %struct.ScmObj*, align 8
%_95k40503 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47027)
store volatile %struct.ScmObj* %_95k40503, %struct.ScmObj** %stackaddr$prim48665, align 8
%stackaddr$prim48666 = alloca %struct.ScmObj*, align 8
%current_45args47028 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47027)
store volatile %struct.ScmObj* %current_45args47028, %struct.ScmObj** %stackaddr$prim48666, align 8
%stackaddr$prim48667 = alloca %struct.ScmObj*, align 8
%anf_45bind40276 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47028)
store volatile %struct.ScmObj* %anf_45bind40276, %struct.ScmObj** %stackaddr$prim48667, align 8
%stackaddr$prim48668 = alloca %struct.ScmObj*, align 8
%cpsargs40504 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40494, %struct.ScmObj* %anf_45bind40276)
store volatile %struct.ScmObj* %cpsargs40504, %struct.ScmObj** %stackaddr$prim48668, align 8
%clofunc48669 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40133)
musttail call tailcc void %clofunc48669(%struct.ScmObj* %f40133, %struct.ScmObj* %cpsargs40504)
ret void
}

define tailcc void @proc_clo$ae41363(%struct.ScmObj* %env$ae41363,%struct.ScmObj* %current_45args47031) {
%stackaddr$prim48670 = alloca %struct.ScmObj*, align 8
%k40506 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47031)
store volatile %struct.ScmObj* %k40506, %struct.ScmObj** %stackaddr$prim48670, align 8
%stackaddr$prim48671 = alloca %struct.ScmObj*, align 8
%current_45args47032 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47031)
store volatile %struct.ScmObj* %current_45args47032, %struct.ScmObj** %stackaddr$prim48671, align 8
%stackaddr$prim48672 = alloca %struct.ScmObj*, align 8
%a40141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47032)
store volatile %struct.ScmObj* %a40141, %struct.ScmObj** %stackaddr$prim48672, align 8
%stackaddr$prim48673 = alloca %struct.ScmObj*, align 8
%current_45args47033 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47032)
store volatile %struct.ScmObj* %current_45args47033, %struct.ScmObj** %stackaddr$prim48673, align 8
%stackaddr$prim48674 = alloca %struct.ScmObj*, align 8
%b40140 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47033)
store volatile %struct.ScmObj* %b40140, %struct.ScmObj** %stackaddr$prim48674, align 8
%stackaddr$prim48675 = alloca %struct.ScmObj*, align 8
%cpsprim40507 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a40141, %struct.ScmObj* %b40140)
store volatile %struct.ScmObj* %cpsprim40507, %struct.ScmObj** %stackaddr$prim48675, align 8
%ae41367 = call %struct.ScmObj* @const_init_int(i64 0)
%args47035$k40506$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48676 = alloca %struct.ScmObj*, align 8
%args47035$k40506$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40507, %struct.ScmObj* %args47035$k40506$0)
store volatile %struct.ScmObj* %args47035$k40506$1, %struct.ScmObj** %stackaddr$prim48676, align 8
%stackaddr$prim48677 = alloca %struct.ScmObj*, align 8
%args47035$k40506$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41367, %struct.ScmObj* %args47035$k40506$1)
store volatile %struct.ScmObj* %args47035$k40506$2, %struct.ScmObj** %stackaddr$prim48677, align 8
%clofunc48678 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40506)
musttail call tailcc void %clofunc48678(%struct.ScmObj* %k40506, %struct.ScmObj* %args47035$k40506$2)
ret void
}

define tailcc void @proc_clo$ae41339(%struct.ScmObj* %env$ae41339,%struct.ScmObj* %current_45args47038) {
%stackaddr$prim48679 = alloca %struct.ScmObj*, align 8
%k40508 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47038)
store volatile %struct.ScmObj* %k40508, %struct.ScmObj** %stackaddr$prim48679, align 8
%stackaddr$prim48680 = alloca %struct.ScmObj*, align 8
%current_45args47039 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47038)
store volatile %struct.ScmObj* %current_45args47039, %struct.ScmObj** %stackaddr$prim48680, align 8
%stackaddr$prim48681 = alloca %struct.ScmObj*, align 8
%x40137 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47039)
store volatile %struct.ScmObj* %x40137, %struct.ScmObj** %stackaddr$prim48681, align 8
%stackaddr$prim48682 = alloca %struct.ScmObj*, align 8
%cpsprim40509 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40137)
store volatile %struct.ScmObj* %cpsprim40509, %struct.ScmObj** %stackaddr$prim48682, align 8
%ae41342 = call %struct.ScmObj* @const_init_int(i64 0)
%args47041$k40508$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48683 = alloca %struct.ScmObj*, align 8
%args47041$k40508$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40509, %struct.ScmObj* %args47041$k40508$0)
store volatile %struct.ScmObj* %args47041$k40508$1, %struct.ScmObj** %stackaddr$prim48683, align 8
%stackaddr$prim48684 = alloca %struct.ScmObj*, align 8
%args47041$k40508$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41342, %struct.ScmObj* %args47041$k40508$1)
store volatile %struct.ScmObj* %args47041$k40508$2, %struct.ScmObj** %stackaddr$prim48684, align 8
%clofunc48685 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40508)
musttail call tailcc void %clofunc48685(%struct.ScmObj* %k40508, %struct.ScmObj* %args47041$k40508$2)
ret void
}

define tailcc void @proc_clo$ae41315(%struct.ScmObj* %env$ae41315,%struct.ScmObj* %current_45args47044) {
%stackaddr$prim48686 = alloca %struct.ScmObj*, align 8
%k40510 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47044)
store volatile %struct.ScmObj* %k40510, %struct.ScmObj** %stackaddr$prim48686, align 8
%stackaddr$prim48687 = alloca %struct.ScmObj*, align 8
%current_45args47045 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47044)
store volatile %struct.ScmObj* %current_45args47045, %struct.ScmObj** %stackaddr$prim48687, align 8
%stackaddr$prim48688 = alloca %struct.ScmObj*, align 8
%x40139 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47045)
store volatile %struct.ScmObj* %x40139, %struct.ScmObj** %stackaddr$prim48688, align 8
%stackaddr$prim48689 = alloca %struct.ScmObj*, align 8
%cpsprim40511 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40139)
store volatile %struct.ScmObj* %cpsprim40511, %struct.ScmObj** %stackaddr$prim48689, align 8
%ae41318 = call %struct.ScmObj* @const_init_int(i64 0)
%args47047$k40510$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48690 = alloca %struct.ScmObj*, align 8
%args47047$k40510$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40511, %struct.ScmObj* %args47047$k40510$0)
store volatile %struct.ScmObj* %args47047$k40510$1, %struct.ScmObj** %stackaddr$prim48690, align 8
%stackaddr$prim48691 = alloca %struct.ScmObj*, align 8
%args47047$k40510$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41318, %struct.ScmObj* %args47047$k40510$1)
store volatile %struct.ScmObj* %args47047$k40510$2, %struct.ScmObj** %stackaddr$prim48691, align 8
%clofunc48692 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40510)
musttail call tailcc void %clofunc48692(%struct.ScmObj* %k40510, %struct.ScmObj* %args47047$k40510$2)
ret void
}

define tailcc void @proc_clo$ae41267(%struct.ScmObj* %env$ae41267,%struct.ScmObj* %current_45args47050) {
%stackaddr$prim48693 = alloca %struct.ScmObj*, align 8
%k40512 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47050)
store volatile %struct.ScmObj* %k40512, %struct.ScmObj** %stackaddr$prim48693, align 8
%stackaddr$prim48694 = alloca %struct.ScmObj*, align 8
%current_45args47051 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47050)
store volatile %struct.ScmObj* %current_45args47051, %struct.ScmObj** %stackaddr$prim48694, align 8
%stackaddr$prim48695 = alloca %struct.ScmObj*, align 8
%lst40135 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47051)
store volatile %struct.ScmObj* %lst40135, %struct.ScmObj** %stackaddr$prim48695, align 8
%stackaddr$prim48696 = alloca %struct.ScmObj*, align 8
%current_45args47052 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47051)
store volatile %struct.ScmObj* %current_45args47052, %struct.ScmObj** %stackaddr$prim48696, align 8
%stackaddr$prim48697 = alloca %struct.ScmObj*, align 8
%b40134 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47052)
store volatile %struct.ScmObj* %b40134, %struct.ScmObj** %stackaddr$prim48697, align 8
%truthy$cmp48698 = call i64 @is_truthy_value(%struct.ScmObj* %b40134)
%cmp$cmp48698 = icmp eq i64 %truthy$cmp48698, 1
br i1 %cmp$cmp48698, label %truebranch$cmp48698, label %falsebranch$cmp48698
truebranch$cmp48698:
%ae41270 = call %struct.ScmObj* @const_init_int(i64 0)
%args47054$k40512$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48699 = alloca %struct.ScmObj*, align 8
%args47054$k40512$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b40134, %struct.ScmObj* %args47054$k40512$0)
store volatile %struct.ScmObj* %args47054$k40512$1, %struct.ScmObj** %stackaddr$prim48699, align 8
%stackaddr$prim48700 = alloca %struct.ScmObj*, align 8
%args47054$k40512$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41270, %struct.ScmObj* %args47054$k40512$1)
store volatile %struct.ScmObj* %args47054$k40512$2, %struct.ScmObj** %stackaddr$prim48700, align 8
%clofunc48701 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40512)
musttail call tailcc void %clofunc48701(%struct.ScmObj* %k40512, %struct.ScmObj* %args47054$k40512$2)
ret void
falsebranch$cmp48698:
%stackaddr$prim48702 = alloca %struct.ScmObj*, align 8
%cpsprim40513 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40135)
store volatile %struct.ScmObj* %cpsprim40513, %struct.ScmObj** %stackaddr$prim48702, align 8
%ae41277 = call %struct.ScmObj* @const_init_int(i64 0)
%args47055$k40512$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48703 = alloca %struct.ScmObj*, align 8
%args47055$k40512$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40513, %struct.ScmObj* %args47055$k40512$0)
store volatile %struct.ScmObj* %args47055$k40512$1, %struct.ScmObj** %stackaddr$prim48703, align 8
%stackaddr$prim48704 = alloca %struct.ScmObj*, align 8
%args47055$k40512$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41277, %struct.ScmObj* %args47055$k40512$1)
store volatile %struct.ScmObj* %args47055$k40512$2, %struct.ScmObj** %stackaddr$prim48704, align 8
%clofunc48705 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40512)
musttail call tailcc void %clofunc48705(%struct.ScmObj* %k40512, %struct.ScmObj* %args47055$k40512$2)
ret void
}

define tailcc void @proc_clo$ae41224(%struct.ScmObj* %env$ae41224,%struct.ScmObj* %current_45args47059) {
%stackaddr$env-ref48706 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41224, i64 0)
store %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$env-ref48706
%stackaddr$env-ref48707 = alloca %struct.ScmObj*, align 8
%_37length40112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41224, i64 1)
store %struct.ScmObj* %_37length40112, %struct.ScmObj** %stackaddr$env-ref48707
%stackaddr$prim48708 = alloca %struct.ScmObj*, align 8
%k40514 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47059)
store volatile %struct.ScmObj* %k40514, %struct.ScmObj** %stackaddr$prim48708, align 8
%stackaddr$prim48709 = alloca %struct.ScmObj*, align 8
%current_45args47060 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47059)
store volatile %struct.ScmObj* %current_45args47060, %struct.ScmObj** %stackaddr$prim48709, align 8
%stackaddr$prim48710 = alloca %struct.ScmObj*, align 8
%lst40144 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47060)
store volatile %struct.ScmObj* %lst40144, %struct.ScmObj** %stackaddr$prim48710, align 8
%stackaddr$prim48711 = alloca %struct.ScmObj*, align 8
%current_45args47061 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47060)
store volatile %struct.ScmObj* %current_45args47061, %struct.ScmObj** %stackaddr$prim48711, align 8
%stackaddr$prim48712 = alloca %struct.ScmObj*, align 8
%n40143 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47061)
store volatile %struct.ScmObj* %n40143, %struct.ScmObj** %stackaddr$prim48712, align 8
%stackaddr$makeclosure48713 = alloca %struct.ScmObj*, align 8
%fptrToInt48714 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41226 to i64
%ae41226 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48714)
store volatile %struct.ScmObj* %ae41226, %struct.ScmObj** %stackaddr$makeclosure48713, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41226, %struct.ScmObj* %_37take40115, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41226, %struct.ScmObj* %k40514, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41226, %struct.ScmObj* %lst40144, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41226, %struct.ScmObj* %n40143, i64 3)
%args47067$_37length40112$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48715 = alloca %struct.ScmObj*, align 8
%args47067$_37length40112$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40144, %struct.ScmObj* %args47067$_37length40112$0)
store volatile %struct.ScmObj* %args47067$_37length40112$1, %struct.ScmObj** %stackaddr$prim48715, align 8
%stackaddr$prim48716 = alloca %struct.ScmObj*, align 8
%args47067$_37length40112$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41226, %struct.ScmObj* %args47067$_37length40112$1)
store volatile %struct.ScmObj* %args47067$_37length40112$2, %struct.ScmObj** %stackaddr$prim48716, align 8
%clofunc48717 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length40112)
musttail call tailcc void %clofunc48717(%struct.ScmObj* %_37length40112, %struct.ScmObj* %args47067$_37length40112$2)
ret void
}

define tailcc void @proc_clo$ae41226(%struct.ScmObj* %env$ae41226,%struct.ScmObj* %current_45args47063) {
%stackaddr$env-ref48718 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41226, i64 0)
store %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$env-ref48718
%stackaddr$env-ref48719 = alloca %struct.ScmObj*, align 8
%k40514 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41226, i64 1)
store %struct.ScmObj* %k40514, %struct.ScmObj** %stackaddr$env-ref48719
%stackaddr$env-ref48720 = alloca %struct.ScmObj*, align 8
%lst40144 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41226, i64 2)
store %struct.ScmObj* %lst40144, %struct.ScmObj** %stackaddr$env-ref48720
%stackaddr$env-ref48721 = alloca %struct.ScmObj*, align 8
%n40143 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41226, i64 3)
store %struct.ScmObj* %n40143, %struct.ScmObj** %stackaddr$env-ref48721
%stackaddr$prim48722 = alloca %struct.ScmObj*, align 8
%_95k40515 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47063)
store volatile %struct.ScmObj* %_95k40515, %struct.ScmObj** %stackaddr$prim48722, align 8
%stackaddr$prim48723 = alloca %struct.ScmObj*, align 8
%current_45args47064 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47063)
store volatile %struct.ScmObj* %current_45args47064, %struct.ScmObj** %stackaddr$prim48723, align 8
%stackaddr$prim48724 = alloca %struct.ScmObj*, align 8
%anf_45bind40263 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47064)
store volatile %struct.ScmObj* %anf_45bind40263, %struct.ScmObj** %stackaddr$prim48724, align 8
%stackaddr$prim48725 = alloca %struct.ScmObj*, align 8
%anf_45bind40264 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind40263, %struct.ScmObj* %n40143)
store volatile %struct.ScmObj* %anf_45bind40264, %struct.ScmObj** %stackaddr$prim48725, align 8
%args47066$_37take40115$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48726 = alloca %struct.ScmObj*, align 8
%args47066$_37take40115$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40264, %struct.ScmObj* %args47066$_37take40115$0)
store volatile %struct.ScmObj* %args47066$_37take40115$1, %struct.ScmObj** %stackaddr$prim48726, align 8
%stackaddr$prim48727 = alloca %struct.ScmObj*, align 8
%args47066$_37take40115$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40144, %struct.ScmObj* %args47066$_37take40115$1)
store volatile %struct.ScmObj* %args47066$_37take40115$2, %struct.ScmObj** %stackaddr$prim48727, align 8
%stackaddr$prim48728 = alloca %struct.ScmObj*, align 8
%args47066$_37take40115$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40514, %struct.ScmObj* %args47066$_37take40115$2)
store volatile %struct.ScmObj* %args47066$_37take40115$3, %struct.ScmObj** %stackaddr$prim48728, align 8
%clofunc48729 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take40115)
musttail call tailcc void %clofunc48729(%struct.ScmObj* %_37take40115, %struct.ScmObj* %args47066$_37take40115$3)
ret void
}

define tailcc void @proc_clo$ae41170(%struct.ScmObj* %env$ae41170,%struct.ScmObj* %current_45args47069) {
%stackaddr$env-ref48730 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41170, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref48730
%stackaddr$prim48731 = alloca %struct.ScmObj*, align 8
%k40516 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47069)
store volatile %struct.ScmObj* %k40516, %struct.ScmObj** %stackaddr$prim48731, align 8
%stackaddr$prim48732 = alloca %struct.ScmObj*, align 8
%current_45args47070 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47069)
store volatile %struct.ScmObj* %current_45args47070, %struct.ScmObj** %stackaddr$prim48732, align 8
%stackaddr$prim48733 = alloca %struct.ScmObj*, align 8
%lst40146 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47070)
store volatile %struct.ScmObj* %lst40146, %struct.ScmObj** %stackaddr$prim48733, align 8
%stackaddr$makeclosure48734 = alloca %struct.ScmObj*, align 8
%fptrToInt48735 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41171 to i64
%ae41171 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48735)
store volatile %struct.ScmObj* %ae41171, %struct.ScmObj** %stackaddr$makeclosure48734, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41171, %struct.ScmObj* %k40516, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41171, %struct.ScmObj* %lst40146, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41171, %struct.ScmObj* %_37foldl140107, i64 2)
%ae41172 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48736 = alloca %struct.ScmObj*, align 8
%fptrToInt48737 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41173 to i64
%ae41173 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48737)
store volatile %struct.ScmObj* %ae41173, %struct.ScmObj** %stackaddr$makeclosure48736, align 8
%args47081$ae41171$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48738 = alloca %struct.ScmObj*, align 8
%args47081$ae41171$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41173, %struct.ScmObj* %args47081$ae41171$0)
store volatile %struct.ScmObj* %args47081$ae41171$1, %struct.ScmObj** %stackaddr$prim48738, align 8
%stackaddr$prim48739 = alloca %struct.ScmObj*, align 8
%args47081$ae41171$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41172, %struct.ScmObj* %args47081$ae41171$1)
store volatile %struct.ScmObj* %args47081$ae41171$2, %struct.ScmObj** %stackaddr$prim48739, align 8
%clofunc48740 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41171)
musttail call tailcc void %clofunc48740(%struct.ScmObj* %ae41171, %struct.ScmObj* %args47081$ae41171$2)
ret void
}

define tailcc void @proc_clo$ae41171(%struct.ScmObj* %env$ae41171,%struct.ScmObj* %current_45args47072) {
%stackaddr$env-ref48741 = alloca %struct.ScmObj*, align 8
%k40516 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41171, i64 0)
store %struct.ScmObj* %k40516, %struct.ScmObj** %stackaddr$env-ref48741
%stackaddr$env-ref48742 = alloca %struct.ScmObj*, align 8
%lst40146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41171, i64 1)
store %struct.ScmObj* %lst40146, %struct.ScmObj** %stackaddr$env-ref48742
%stackaddr$env-ref48743 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41171, i64 2)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref48743
%stackaddr$prim48744 = alloca %struct.ScmObj*, align 8
%_95k40517 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47072)
store volatile %struct.ScmObj* %_95k40517, %struct.ScmObj** %stackaddr$prim48744, align 8
%stackaddr$prim48745 = alloca %struct.ScmObj*, align 8
%current_45args47073 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47072)
store volatile %struct.ScmObj* %current_45args47073, %struct.ScmObj** %stackaddr$prim48745, align 8
%stackaddr$prim48746 = alloca %struct.ScmObj*, align 8
%anf_45bind40262 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47073)
store volatile %struct.ScmObj* %anf_45bind40262, %struct.ScmObj** %stackaddr$prim48746, align 8
%ae41192 = call %struct.ScmObj* @const_init_null()
%args47075$_37foldl140107$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48747 = alloca %struct.ScmObj*, align 8
%args47075$_37foldl140107$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40146, %struct.ScmObj* %args47075$_37foldl140107$0)
store volatile %struct.ScmObj* %args47075$_37foldl140107$1, %struct.ScmObj** %stackaddr$prim48747, align 8
%stackaddr$prim48748 = alloca %struct.ScmObj*, align 8
%args47075$_37foldl140107$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41192, %struct.ScmObj* %args47075$_37foldl140107$1)
store volatile %struct.ScmObj* %args47075$_37foldl140107$2, %struct.ScmObj** %stackaddr$prim48748, align 8
%stackaddr$prim48749 = alloca %struct.ScmObj*, align 8
%args47075$_37foldl140107$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40262, %struct.ScmObj* %args47075$_37foldl140107$2)
store volatile %struct.ScmObj* %args47075$_37foldl140107$3, %struct.ScmObj** %stackaddr$prim48749, align 8
%stackaddr$prim48750 = alloca %struct.ScmObj*, align 8
%args47075$_37foldl140107$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40516, %struct.ScmObj* %args47075$_37foldl140107$3)
store volatile %struct.ScmObj* %args47075$_37foldl140107$4, %struct.ScmObj** %stackaddr$prim48750, align 8
%clofunc48751 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140107)
musttail call tailcc void %clofunc48751(%struct.ScmObj* %_37foldl140107, %struct.ScmObj* %args47075$_37foldl140107$4)
ret void
}

define tailcc void @proc_clo$ae41173(%struct.ScmObj* %env$ae41173,%struct.ScmObj* %current_45args47076) {
%stackaddr$prim48752 = alloca %struct.ScmObj*, align 8
%k40518 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47076)
store volatile %struct.ScmObj* %k40518, %struct.ScmObj** %stackaddr$prim48752, align 8
%stackaddr$prim48753 = alloca %struct.ScmObj*, align 8
%current_45args47077 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47076)
store volatile %struct.ScmObj* %current_45args47077, %struct.ScmObj** %stackaddr$prim48753, align 8
%stackaddr$prim48754 = alloca %struct.ScmObj*, align 8
%x40148 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47077)
store volatile %struct.ScmObj* %x40148, %struct.ScmObj** %stackaddr$prim48754, align 8
%stackaddr$prim48755 = alloca %struct.ScmObj*, align 8
%current_45args47078 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47077)
store volatile %struct.ScmObj* %current_45args47078, %struct.ScmObj** %stackaddr$prim48755, align 8
%stackaddr$prim48756 = alloca %struct.ScmObj*, align 8
%y40147 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47078)
store volatile %struct.ScmObj* %y40147, %struct.ScmObj** %stackaddr$prim48756, align 8
%ae41175 = call %struct.ScmObj* @const_init_int(i64 0)
%args47080$k40518$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48757 = alloca %struct.ScmObj*, align 8
%args47080$k40518$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x40148, %struct.ScmObj* %args47080$k40518$0)
store volatile %struct.ScmObj* %args47080$k40518$1, %struct.ScmObj** %stackaddr$prim48757, align 8
%stackaddr$prim48758 = alloca %struct.ScmObj*, align 8
%args47080$k40518$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41175, %struct.ScmObj* %args47080$k40518$1)
store volatile %struct.ScmObj* %args47080$k40518$2, %struct.ScmObj** %stackaddr$prim48758, align 8
%clofunc48759 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40518)
musttail call tailcc void %clofunc48759(%struct.ScmObj* %k40518, %struct.ScmObj* %args47080$k40518$2)
ret void
}

define tailcc void @proc_clo$ae41091(%struct.ScmObj* %env$ae41091,%struct.ScmObj* %current_45args47084) {
%stackaddr$prim48760 = alloca %struct.ScmObj*, align 8
%k40519 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47084)
store volatile %struct.ScmObj* %k40519, %struct.ScmObj** %stackaddr$prim48760, align 8
%stackaddr$prim48761 = alloca %struct.ScmObj*, align 8
%current_45args47085 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47084)
store volatile %struct.ScmObj* %current_45args47085, %struct.ScmObj** %stackaddr$prim48761, align 8
%stackaddr$prim48762 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47085)
store volatile %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$prim48762, align 8
%ae41093 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48763 = alloca %struct.ScmObj*, align 8
%fptrToInt48764 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41094 to i64
%ae41094 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48764)
store volatile %struct.ScmObj* %ae41094, %struct.ScmObj** %stackaddr$makeclosure48763, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41094, %struct.ScmObj* %_37foldl140108, i64 0)
%args47098$k40519$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48765 = alloca %struct.ScmObj*, align 8
%args47098$k40519$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41094, %struct.ScmObj* %args47098$k40519$0)
store volatile %struct.ScmObj* %args47098$k40519$1, %struct.ScmObj** %stackaddr$prim48765, align 8
%stackaddr$prim48766 = alloca %struct.ScmObj*, align 8
%args47098$k40519$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41093, %struct.ScmObj* %args47098$k40519$1)
store volatile %struct.ScmObj* %args47098$k40519$2, %struct.ScmObj** %stackaddr$prim48766, align 8
%clofunc48767 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40519)
musttail call tailcc void %clofunc48767(%struct.ScmObj* %k40519, %struct.ScmObj* %args47098$k40519$2)
ret void
}

define tailcc void @proc_clo$ae41094(%struct.ScmObj* %env$ae41094,%struct.ScmObj* %current_45args47087) {
%stackaddr$env-ref48768 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41094, i64 0)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref48768
%stackaddr$prim48769 = alloca %struct.ScmObj*, align 8
%k40520 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47087)
store volatile %struct.ScmObj* %k40520, %struct.ScmObj** %stackaddr$prim48769, align 8
%stackaddr$prim48770 = alloca %struct.ScmObj*, align 8
%current_45args47088 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47087)
store volatile %struct.ScmObj* %current_45args47088, %struct.ScmObj** %stackaddr$prim48770, align 8
%stackaddr$prim48771 = alloca %struct.ScmObj*, align 8
%f40111 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47088)
store volatile %struct.ScmObj* %f40111, %struct.ScmObj** %stackaddr$prim48771, align 8
%stackaddr$prim48772 = alloca %struct.ScmObj*, align 8
%current_45args47089 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47088)
store volatile %struct.ScmObj* %current_45args47089, %struct.ScmObj** %stackaddr$prim48772, align 8
%stackaddr$prim48773 = alloca %struct.ScmObj*, align 8
%acc40110 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47089)
store volatile %struct.ScmObj* %acc40110, %struct.ScmObj** %stackaddr$prim48773, align 8
%stackaddr$prim48774 = alloca %struct.ScmObj*, align 8
%current_45args47090 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47089)
store volatile %struct.ScmObj* %current_45args47090, %struct.ScmObj** %stackaddr$prim48774, align 8
%stackaddr$prim48775 = alloca %struct.ScmObj*, align 8
%lst40109 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47090)
store volatile %struct.ScmObj* %lst40109, %struct.ScmObj** %stackaddr$prim48775, align 8
%stackaddr$prim48776 = alloca %struct.ScmObj*, align 8
%anf_45bind40257 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40109)
store volatile %struct.ScmObj* %anf_45bind40257, %struct.ScmObj** %stackaddr$prim48776, align 8
%truthy$cmp48777 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40257)
%cmp$cmp48777 = icmp eq i64 %truthy$cmp48777, 1
br i1 %cmp$cmp48777, label %truebranch$cmp48777, label %falsebranch$cmp48777
truebranch$cmp48777:
%ae41098 = call %struct.ScmObj* @const_init_int(i64 0)
%args47092$k40520$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48778 = alloca %struct.ScmObj*, align 8
%args47092$k40520$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40110, %struct.ScmObj* %args47092$k40520$0)
store volatile %struct.ScmObj* %args47092$k40520$1, %struct.ScmObj** %stackaddr$prim48778, align 8
%stackaddr$prim48779 = alloca %struct.ScmObj*, align 8
%args47092$k40520$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41098, %struct.ScmObj* %args47092$k40520$1)
store volatile %struct.ScmObj* %args47092$k40520$2, %struct.ScmObj** %stackaddr$prim48779, align 8
%clofunc48780 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40520)
musttail call tailcc void %clofunc48780(%struct.ScmObj* %k40520, %struct.ScmObj* %args47092$k40520$2)
ret void
falsebranch$cmp48777:
%stackaddr$prim48781 = alloca %struct.ScmObj*, align 8
%anf_45bind40258 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40109)
store volatile %struct.ScmObj* %anf_45bind40258, %struct.ScmObj** %stackaddr$prim48781, align 8
%stackaddr$makeclosure48782 = alloca %struct.ScmObj*, align 8
%fptrToInt48783 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41105 to i64
%ae41105 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48783)
store volatile %struct.ScmObj* %ae41105, %struct.ScmObj** %stackaddr$makeclosure48782, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41105, %struct.ScmObj* %f40111, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41105, %struct.ScmObj* %lst40109, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41105, %struct.ScmObj* %_37foldl140108, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41105, %struct.ScmObj* %k40520, i64 3)
%args47097$f40111$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48784 = alloca %struct.ScmObj*, align 8
%args47097$f40111$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40110, %struct.ScmObj* %args47097$f40111$0)
store volatile %struct.ScmObj* %args47097$f40111$1, %struct.ScmObj** %stackaddr$prim48784, align 8
%stackaddr$prim48785 = alloca %struct.ScmObj*, align 8
%args47097$f40111$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40258, %struct.ScmObj* %args47097$f40111$1)
store volatile %struct.ScmObj* %args47097$f40111$2, %struct.ScmObj** %stackaddr$prim48785, align 8
%stackaddr$prim48786 = alloca %struct.ScmObj*, align 8
%args47097$f40111$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41105, %struct.ScmObj* %args47097$f40111$2)
store volatile %struct.ScmObj* %args47097$f40111$3, %struct.ScmObj** %stackaddr$prim48786, align 8
%clofunc48787 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40111)
musttail call tailcc void %clofunc48787(%struct.ScmObj* %f40111, %struct.ScmObj* %args47097$f40111$3)
ret void
}

define tailcc void @proc_clo$ae41105(%struct.ScmObj* %env$ae41105,%struct.ScmObj* %current_45args47093) {
%stackaddr$env-ref48788 = alloca %struct.ScmObj*, align 8
%f40111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41105, i64 0)
store %struct.ScmObj* %f40111, %struct.ScmObj** %stackaddr$env-ref48788
%stackaddr$env-ref48789 = alloca %struct.ScmObj*, align 8
%lst40109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41105, i64 1)
store %struct.ScmObj* %lst40109, %struct.ScmObj** %stackaddr$env-ref48789
%stackaddr$env-ref48790 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41105, i64 2)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref48790
%stackaddr$env-ref48791 = alloca %struct.ScmObj*, align 8
%k40520 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41105, i64 3)
store %struct.ScmObj* %k40520, %struct.ScmObj** %stackaddr$env-ref48791
%stackaddr$prim48792 = alloca %struct.ScmObj*, align 8
%_95k40521 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47093)
store volatile %struct.ScmObj* %_95k40521, %struct.ScmObj** %stackaddr$prim48792, align 8
%stackaddr$prim48793 = alloca %struct.ScmObj*, align 8
%current_45args47094 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47093)
store volatile %struct.ScmObj* %current_45args47094, %struct.ScmObj** %stackaddr$prim48793, align 8
%stackaddr$prim48794 = alloca %struct.ScmObj*, align 8
%anf_45bind40259 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47094)
store volatile %struct.ScmObj* %anf_45bind40259, %struct.ScmObj** %stackaddr$prim48794, align 8
%stackaddr$prim48795 = alloca %struct.ScmObj*, align 8
%anf_45bind40260 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40109)
store volatile %struct.ScmObj* %anf_45bind40260, %struct.ScmObj** %stackaddr$prim48795, align 8
%args47096$_37foldl140108$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48796 = alloca %struct.ScmObj*, align 8
%args47096$_37foldl140108$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40260, %struct.ScmObj* %args47096$_37foldl140108$0)
store volatile %struct.ScmObj* %args47096$_37foldl140108$1, %struct.ScmObj** %stackaddr$prim48796, align 8
%stackaddr$prim48797 = alloca %struct.ScmObj*, align 8
%args47096$_37foldl140108$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40259, %struct.ScmObj* %args47096$_37foldl140108$1)
store volatile %struct.ScmObj* %args47096$_37foldl140108$2, %struct.ScmObj** %stackaddr$prim48797, align 8
%stackaddr$prim48798 = alloca %struct.ScmObj*, align 8
%args47096$_37foldl140108$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40111, %struct.ScmObj* %args47096$_37foldl140108$2)
store volatile %struct.ScmObj* %args47096$_37foldl140108$3, %struct.ScmObj** %stackaddr$prim48798, align 8
%stackaddr$prim48799 = alloca %struct.ScmObj*, align 8
%args47096$_37foldl140108$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40520, %struct.ScmObj* %args47096$_37foldl140108$3)
store volatile %struct.ScmObj* %args47096$_37foldl140108$4, %struct.ScmObj** %stackaddr$prim48799, align 8
%clofunc48800 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140108)
musttail call tailcc void %clofunc48800(%struct.ScmObj* %_37foldl140108, %struct.ScmObj* %args47096$_37foldl140108$4)
ret void
}

define tailcc void @proc_clo$ae41008(%struct.ScmObj* %env$ae41008,%struct.ScmObj* %current_45args47101) {
%stackaddr$prim48801 = alloca %struct.ScmObj*, align 8
%k40522 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47101)
store volatile %struct.ScmObj* %k40522, %struct.ScmObj** %stackaddr$prim48801, align 8
%stackaddr$prim48802 = alloca %struct.ScmObj*, align 8
%current_45args47102 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47101)
store volatile %struct.ScmObj* %current_45args47102, %struct.ScmObj** %stackaddr$prim48802, align 8
%stackaddr$prim48803 = alloca %struct.ScmObj*, align 8
%_37length40113 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47102)
store volatile %struct.ScmObj* %_37length40113, %struct.ScmObj** %stackaddr$prim48803, align 8
%ae41010 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48804 = alloca %struct.ScmObj*, align 8
%fptrToInt48805 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41011 to i64
%ae41011 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48805)
store volatile %struct.ScmObj* %ae41011, %struct.ScmObj** %stackaddr$makeclosure48804, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41011, %struct.ScmObj* %_37length40113, i64 0)
%args47113$k40522$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48806 = alloca %struct.ScmObj*, align 8
%args47113$k40522$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41011, %struct.ScmObj* %args47113$k40522$0)
store volatile %struct.ScmObj* %args47113$k40522$1, %struct.ScmObj** %stackaddr$prim48806, align 8
%stackaddr$prim48807 = alloca %struct.ScmObj*, align 8
%args47113$k40522$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41010, %struct.ScmObj* %args47113$k40522$1)
store volatile %struct.ScmObj* %args47113$k40522$2, %struct.ScmObj** %stackaddr$prim48807, align 8
%clofunc48808 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40522)
musttail call tailcc void %clofunc48808(%struct.ScmObj* %k40522, %struct.ScmObj* %args47113$k40522$2)
ret void
}

define tailcc void @proc_clo$ae41011(%struct.ScmObj* %env$ae41011,%struct.ScmObj* %current_45args47104) {
%stackaddr$env-ref48809 = alloca %struct.ScmObj*, align 8
%_37length40113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41011, i64 0)
store %struct.ScmObj* %_37length40113, %struct.ScmObj** %stackaddr$env-ref48809
%stackaddr$prim48810 = alloca %struct.ScmObj*, align 8
%k40523 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47104)
store volatile %struct.ScmObj* %k40523, %struct.ScmObj** %stackaddr$prim48810, align 8
%stackaddr$prim48811 = alloca %struct.ScmObj*, align 8
%current_45args47105 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47104)
store volatile %struct.ScmObj* %current_45args47105, %struct.ScmObj** %stackaddr$prim48811, align 8
%stackaddr$prim48812 = alloca %struct.ScmObj*, align 8
%lst40114 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47105)
store volatile %struct.ScmObj* %lst40114, %struct.ScmObj** %stackaddr$prim48812, align 8
%stackaddr$prim48813 = alloca %struct.ScmObj*, align 8
%anf_45bind40253 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40114)
store volatile %struct.ScmObj* %anf_45bind40253, %struct.ScmObj** %stackaddr$prim48813, align 8
%truthy$cmp48814 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40253)
%cmp$cmp48814 = icmp eq i64 %truthy$cmp48814, 1
br i1 %cmp$cmp48814, label %truebranch$cmp48814, label %falsebranch$cmp48814
truebranch$cmp48814:
%ae41015 = call %struct.ScmObj* @const_init_int(i64 0)
%ae41016 = call %struct.ScmObj* @const_init_int(i64 0)
%args47107$k40523$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48815 = alloca %struct.ScmObj*, align 8
%args47107$k40523$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41016, %struct.ScmObj* %args47107$k40523$0)
store volatile %struct.ScmObj* %args47107$k40523$1, %struct.ScmObj** %stackaddr$prim48815, align 8
%stackaddr$prim48816 = alloca %struct.ScmObj*, align 8
%args47107$k40523$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41015, %struct.ScmObj* %args47107$k40523$1)
store volatile %struct.ScmObj* %args47107$k40523$2, %struct.ScmObj** %stackaddr$prim48816, align 8
%clofunc48817 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40523)
musttail call tailcc void %clofunc48817(%struct.ScmObj* %k40523, %struct.ScmObj* %args47107$k40523$2)
ret void
falsebranch$cmp48814:
%stackaddr$prim48818 = alloca %struct.ScmObj*, align 8
%anf_45bind40254 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40114)
store volatile %struct.ScmObj* %anf_45bind40254, %struct.ScmObj** %stackaddr$prim48818, align 8
%stackaddr$makeclosure48819 = alloca %struct.ScmObj*, align 8
%fptrToInt48820 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41025 to i64
%ae41025 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48820)
store volatile %struct.ScmObj* %ae41025, %struct.ScmObj** %stackaddr$makeclosure48819, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41025, %struct.ScmObj* %k40523, i64 0)
%args47112$_37length40113$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48821 = alloca %struct.ScmObj*, align 8
%args47112$_37length40113$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40254, %struct.ScmObj* %args47112$_37length40113$0)
store volatile %struct.ScmObj* %args47112$_37length40113$1, %struct.ScmObj** %stackaddr$prim48821, align 8
%stackaddr$prim48822 = alloca %struct.ScmObj*, align 8
%args47112$_37length40113$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41025, %struct.ScmObj* %args47112$_37length40113$1)
store volatile %struct.ScmObj* %args47112$_37length40113$2, %struct.ScmObj** %stackaddr$prim48822, align 8
%clofunc48823 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length40113)
musttail call tailcc void %clofunc48823(%struct.ScmObj* %_37length40113, %struct.ScmObj* %args47112$_37length40113$2)
ret void
}

define tailcc void @proc_clo$ae41025(%struct.ScmObj* %env$ae41025,%struct.ScmObj* %current_45args47108) {
%stackaddr$env-ref48824 = alloca %struct.ScmObj*, align 8
%k40523 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41025, i64 0)
store %struct.ScmObj* %k40523, %struct.ScmObj** %stackaddr$env-ref48824
%stackaddr$prim48825 = alloca %struct.ScmObj*, align 8
%_95k40524 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47108)
store volatile %struct.ScmObj* %_95k40524, %struct.ScmObj** %stackaddr$prim48825, align 8
%stackaddr$prim48826 = alloca %struct.ScmObj*, align 8
%current_45args47109 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47108)
store volatile %struct.ScmObj* %current_45args47109, %struct.ScmObj** %stackaddr$prim48826, align 8
%stackaddr$prim48827 = alloca %struct.ScmObj*, align 8
%anf_45bind40255 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47109)
store volatile %struct.ScmObj* %anf_45bind40255, %struct.ScmObj** %stackaddr$prim48827, align 8
%ae41027 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim48828 = alloca %struct.ScmObj*, align 8
%cpsprim40525 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae41027, %struct.ScmObj* %anf_45bind40255)
store volatile %struct.ScmObj* %cpsprim40525, %struct.ScmObj** %stackaddr$prim48828, align 8
%ae41030 = call %struct.ScmObj* @const_init_int(i64 0)
%args47111$k40523$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48829 = alloca %struct.ScmObj*, align 8
%args47111$k40523$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40525, %struct.ScmObj* %args47111$k40523$0)
store volatile %struct.ScmObj* %args47111$k40523$1, %struct.ScmObj** %stackaddr$prim48829, align 8
%stackaddr$prim48830 = alloca %struct.ScmObj*, align 8
%args47111$k40523$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41030, %struct.ScmObj* %args47111$k40523$1)
store volatile %struct.ScmObj* %args47111$k40523$2, %struct.ScmObj** %stackaddr$prim48830, align 8
%clofunc48831 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40523)
musttail call tailcc void %clofunc48831(%struct.ScmObj* %k40523, %struct.ScmObj* %args47111$k40523$2)
ret void
}

define tailcc void @proc_clo$ae40858(%struct.ScmObj* %env$ae40858,%struct.ScmObj* %current_45args47116) {
%stackaddr$prim48832 = alloca %struct.ScmObj*, align 8
%k40526 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47116)
store volatile %struct.ScmObj* %k40526, %struct.ScmObj** %stackaddr$prim48832, align 8
%stackaddr$prim48833 = alloca %struct.ScmObj*, align 8
%current_45args47117 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47116)
store volatile %struct.ScmObj* %current_45args47117, %struct.ScmObj** %stackaddr$prim48833, align 8
%stackaddr$prim48834 = alloca %struct.ScmObj*, align 8
%_37take40116 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47117)
store volatile %struct.ScmObj* %_37take40116, %struct.ScmObj** %stackaddr$prim48834, align 8
%ae40860 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48835 = alloca %struct.ScmObj*, align 8
%fptrToInt48836 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40861 to i64
%ae40861 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48836)
store volatile %struct.ScmObj* %ae40861, %struct.ScmObj** %stackaddr$makeclosure48835, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40861, %struct.ScmObj* %_37take40116, i64 0)
%args47130$k40526$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48837 = alloca %struct.ScmObj*, align 8
%args47130$k40526$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40861, %struct.ScmObj* %args47130$k40526$0)
store volatile %struct.ScmObj* %args47130$k40526$1, %struct.ScmObj** %stackaddr$prim48837, align 8
%stackaddr$prim48838 = alloca %struct.ScmObj*, align 8
%args47130$k40526$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40860, %struct.ScmObj* %args47130$k40526$1)
store volatile %struct.ScmObj* %args47130$k40526$2, %struct.ScmObj** %stackaddr$prim48838, align 8
%clofunc48839 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40526)
musttail call tailcc void %clofunc48839(%struct.ScmObj* %k40526, %struct.ScmObj* %args47130$k40526$2)
ret void
}

define tailcc void @proc_clo$ae40861(%struct.ScmObj* %env$ae40861,%struct.ScmObj* %current_45args47119) {
%stackaddr$env-ref48840 = alloca %struct.ScmObj*, align 8
%_37take40116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40861, i64 0)
store %struct.ScmObj* %_37take40116, %struct.ScmObj** %stackaddr$env-ref48840
%stackaddr$prim48841 = alloca %struct.ScmObj*, align 8
%k40527 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47119)
store volatile %struct.ScmObj* %k40527, %struct.ScmObj** %stackaddr$prim48841, align 8
%stackaddr$prim48842 = alloca %struct.ScmObj*, align 8
%current_45args47120 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47119)
store volatile %struct.ScmObj* %current_45args47120, %struct.ScmObj** %stackaddr$prim48842, align 8
%stackaddr$prim48843 = alloca %struct.ScmObj*, align 8
%lst40118 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47120)
store volatile %struct.ScmObj* %lst40118, %struct.ScmObj** %stackaddr$prim48843, align 8
%stackaddr$prim48844 = alloca %struct.ScmObj*, align 8
%current_45args47121 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47120)
store volatile %struct.ScmObj* %current_45args47121, %struct.ScmObj** %stackaddr$prim48844, align 8
%stackaddr$prim48845 = alloca %struct.ScmObj*, align 8
%n40117 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47121)
store volatile %struct.ScmObj* %n40117, %struct.ScmObj** %stackaddr$prim48845, align 8
%ae40863 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48846 = alloca %struct.ScmObj*, align 8
%anf_45bind40246 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n40117, %struct.ScmObj* %ae40863)
store volatile %struct.ScmObj* %anf_45bind40246, %struct.ScmObj** %stackaddr$prim48846, align 8
%truthy$cmp48847 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40246)
%cmp$cmp48847 = icmp eq i64 %truthy$cmp48847, 1
br i1 %cmp$cmp48847, label %truebranch$cmp48847, label %falsebranch$cmp48847
truebranch$cmp48847:
%ae40866 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40867 = call %struct.ScmObj* @const_init_null()
%args47123$k40527$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48848 = alloca %struct.ScmObj*, align 8
%args47123$k40527$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40867, %struct.ScmObj* %args47123$k40527$0)
store volatile %struct.ScmObj* %args47123$k40527$1, %struct.ScmObj** %stackaddr$prim48848, align 8
%stackaddr$prim48849 = alloca %struct.ScmObj*, align 8
%args47123$k40527$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40866, %struct.ScmObj* %args47123$k40527$1)
store volatile %struct.ScmObj* %args47123$k40527$2, %struct.ScmObj** %stackaddr$prim48849, align 8
%clofunc48850 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40527)
musttail call tailcc void %clofunc48850(%struct.ScmObj* %k40527, %struct.ScmObj* %args47123$k40527$2)
ret void
falsebranch$cmp48847:
%stackaddr$prim48851 = alloca %struct.ScmObj*, align 8
%anf_45bind40247 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40118)
store volatile %struct.ScmObj* %anf_45bind40247, %struct.ScmObj** %stackaddr$prim48851, align 8
%truthy$cmp48852 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40247)
%cmp$cmp48852 = icmp eq i64 %truthy$cmp48852, 1
br i1 %cmp$cmp48852, label %truebranch$cmp48852, label %falsebranch$cmp48852
truebranch$cmp48852:
%ae40877 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40878 = call %struct.ScmObj* @const_init_null()
%args47124$k40527$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48853 = alloca %struct.ScmObj*, align 8
%args47124$k40527$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40878, %struct.ScmObj* %args47124$k40527$0)
store volatile %struct.ScmObj* %args47124$k40527$1, %struct.ScmObj** %stackaddr$prim48853, align 8
%stackaddr$prim48854 = alloca %struct.ScmObj*, align 8
%args47124$k40527$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40877, %struct.ScmObj* %args47124$k40527$1)
store volatile %struct.ScmObj* %args47124$k40527$2, %struct.ScmObj** %stackaddr$prim48854, align 8
%clofunc48855 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40527)
musttail call tailcc void %clofunc48855(%struct.ScmObj* %k40527, %struct.ScmObj* %args47124$k40527$2)
ret void
falsebranch$cmp48852:
%stackaddr$prim48856 = alloca %struct.ScmObj*, align 8
%anf_45bind40248 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40118)
store volatile %struct.ScmObj* %anf_45bind40248, %struct.ScmObj** %stackaddr$prim48856, align 8
%stackaddr$prim48857 = alloca %struct.ScmObj*, align 8
%anf_45bind40249 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40118)
store volatile %struct.ScmObj* %anf_45bind40249, %struct.ScmObj** %stackaddr$prim48857, align 8
%ae40888 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim48858 = alloca %struct.ScmObj*, align 8
%anf_45bind40250 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n40117, %struct.ScmObj* %ae40888)
store volatile %struct.ScmObj* %anf_45bind40250, %struct.ScmObj** %stackaddr$prim48858, align 8
%stackaddr$makeclosure48859 = alloca %struct.ScmObj*, align 8
%fptrToInt48860 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40890 to i64
%ae40890 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48860)
store volatile %struct.ScmObj* %ae40890, %struct.ScmObj** %stackaddr$makeclosure48859, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40890, %struct.ScmObj* %k40527, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40890, %struct.ScmObj* %anf_45bind40248, i64 1)
%args47129$_37take40116$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48861 = alloca %struct.ScmObj*, align 8
%args47129$_37take40116$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40250, %struct.ScmObj* %args47129$_37take40116$0)
store volatile %struct.ScmObj* %args47129$_37take40116$1, %struct.ScmObj** %stackaddr$prim48861, align 8
%stackaddr$prim48862 = alloca %struct.ScmObj*, align 8
%args47129$_37take40116$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40249, %struct.ScmObj* %args47129$_37take40116$1)
store volatile %struct.ScmObj* %args47129$_37take40116$2, %struct.ScmObj** %stackaddr$prim48862, align 8
%stackaddr$prim48863 = alloca %struct.ScmObj*, align 8
%args47129$_37take40116$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40890, %struct.ScmObj* %args47129$_37take40116$2)
store volatile %struct.ScmObj* %args47129$_37take40116$3, %struct.ScmObj** %stackaddr$prim48863, align 8
%clofunc48864 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take40116)
musttail call tailcc void %clofunc48864(%struct.ScmObj* %_37take40116, %struct.ScmObj* %args47129$_37take40116$3)
ret void
}

define tailcc void @proc_clo$ae40890(%struct.ScmObj* %env$ae40890,%struct.ScmObj* %current_45args47125) {
%stackaddr$env-ref48865 = alloca %struct.ScmObj*, align 8
%k40527 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40890, i64 0)
store %struct.ScmObj* %k40527, %struct.ScmObj** %stackaddr$env-ref48865
%stackaddr$env-ref48866 = alloca %struct.ScmObj*, align 8
%anf_45bind40248 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40890, i64 1)
store %struct.ScmObj* %anf_45bind40248, %struct.ScmObj** %stackaddr$env-ref48866
%stackaddr$prim48867 = alloca %struct.ScmObj*, align 8
%_95k40528 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47125)
store volatile %struct.ScmObj* %_95k40528, %struct.ScmObj** %stackaddr$prim48867, align 8
%stackaddr$prim48868 = alloca %struct.ScmObj*, align 8
%current_45args47126 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47125)
store volatile %struct.ScmObj* %current_45args47126, %struct.ScmObj** %stackaddr$prim48868, align 8
%stackaddr$prim48869 = alloca %struct.ScmObj*, align 8
%anf_45bind40251 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47126)
store volatile %struct.ScmObj* %anf_45bind40251, %struct.ScmObj** %stackaddr$prim48869, align 8
%stackaddr$prim48870 = alloca %struct.ScmObj*, align 8
%cpsprim40529 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40248, %struct.ScmObj* %anf_45bind40251)
store volatile %struct.ScmObj* %cpsprim40529, %struct.ScmObj** %stackaddr$prim48870, align 8
%ae40896 = call %struct.ScmObj* @const_init_int(i64 0)
%args47128$k40527$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48871 = alloca %struct.ScmObj*, align 8
%args47128$k40527$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40529, %struct.ScmObj* %args47128$k40527$0)
store volatile %struct.ScmObj* %args47128$k40527$1, %struct.ScmObj** %stackaddr$prim48871, align 8
%stackaddr$prim48872 = alloca %struct.ScmObj*, align 8
%args47128$k40527$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40896, %struct.ScmObj* %args47128$k40527$1)
store volatile %struct.ScmObj* %args47128$k40527$2, %struct.ScmObj** %stackaddr$prim48872, align 8
%clofunc48873 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40527)
musttail call tailcc void %clofunc48873(%struct.ScmObj* %k40527, %struct.ScmObj* %args47128$k40527$2)
ret void
}

define tailcc void @proc_clo$ae40761(%struct.ScmObj* %env$ae40761,%struct.ScmObj* %current_45args47133) {
%stackaddr$prim48874 = alloca %struct.ScmObj*, align 8
%k40530 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47133)
store volatile %struct.ScmObj* %k40530, %struct.ScmObj** %stackaddr$prim48874, align 8
%stackaddr$prim48875 = alloca %struct.ScmObj*, align 8
%current_45args47134 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47133)
store volatile %struct.ScmObj* %current_45args47134, %struct.ScmObj** %stackaddr$prim48875, align 8
%stackaddr$prim48876 = alloca %struct.ScmObj*, align 8
%_37map40120 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47134)
store volatile %struct.ScmObj* %_37map40120, %struct.ScmObj** %stackaddr$prim48876, align 8
%ae40763 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48877 = alloca %struct.ScmObj*, align 8
%fptrToInt48878 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40764 to i64
%ae40764 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48878)
store volatile %struct.ScmObj* %ae40764, %struct.ScmObj** %stackaddr$makeclosure48877, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40764, %struct.ScmObj* %_37map40120, i64 0)
%args47150$k40530$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48879 = alloca %struct.ScmObj*, align 8
%args47150$k40530$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40764, %struct.ScmObj* %args47150$k40530$0)
store volatile %struct.ScmObj* %args47150$k40530$1, %struct.ScmObj** %stackaddr$prim48879, align 8
%stackaddr$prim48880 = alloca %struct.ScmObj*, align 8
%args47150$k40530$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40763, %struct.ScmObj* %args47150$k40530$1)
store volatile %struct.ScmObj* %args47150$k40530$2, %struct.ScmObj** %stackaddr$prim48880, align 8
%clofunc48881 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40530)
musttail call tailcc void %clofunc48881(%struct.ScmObj* %k40530, %struct.ScmObj* %args47150$k40530$2)
ret void
}

define tailcc void @proc_clo$ae40764(%struct.ScmObj* %env$ae40764,%struct.ScmObj* %current_45args47136) {
%stackaddr$env-ref48882 = alloca %struct.ScmObj*, align 8
%_37map40120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40764, i64 0)
store %struct.ScmObj* %_37map40120, %struct.ScmObj** %stackaddr$env-ref48882
%stackaddr$prim48883 = alloca %struct.ScmObj*, align 8
%k40531 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47136)
store volatile %struct.ScmObj* %k40531, %struct.ScmObj** %stackaddr$prim48883, align 8
%stackaddr$prim48884 = alloca %struct.ScmObj*, align 8
%current_45args47137 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47136)
store volatile %struct.ScmObj* %current_45args47137, %struct.ScmObj** %stackaddr$prim48884, align 8
%stackaddr$prim48885 = alloca %struct.ScmObj*, align 8
%f40122 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47137)
store volatile %struct.ScmObj* %f40122, %struct.ScmObj** %stackaddr$prim48885, align 8
%stackaddr$prim48886 = alloca %struct.ScmObj*, align 8
%current_45args47138 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47137)
store volatile %struct.ScmObj* %current_45args47138, %struct.ScmObj** %stackaddr$prim48886, align 8
%stackaddr$prim48887 = alloca %struct.ScmObj*, align 8
%lst40121 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47138)
store volatile %struct.ScmObj* %lst40121, %struct.ScmObj** %stackaddr$prim48887, align 8
%stackaddr$prim48888 = alloca %struct.ScmObj*, align 8
%anf_45bind40240 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40121)
store volatile %struct.ScmObj* %anf_45bind40240, %struct.ScmObj** %stackaddr$prim48888, align 8
%truthy$cmp48889 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40240)
%cmp$cmp48889 = icmp eq i64 %truthy$cmp48889, 1
br i1 %cmp$cmp48889, label %truebranch$cmp48889, label %falsebranch$cmp48889
truebranch$cmp48889:
%ae40768 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40769 = call %struct.ScmObj* @const_init_null()
%args47140$k40531$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48890 = alloca %struct.ScmObj*, align 8
%args47140$k40531$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40769, %struct.ScmObj* %args47140$k40531$0)
store volatile %struct.ScmObj* %args47140$k40531$1, %struct.ScmObj** %stackaddr$prim48890, align 8
%stackaddr$prim48891 = alloca %struct.ScmObj*, align 8
%args47140$k40531$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40768, %struct.ScmObj* %args47140$k40531$1)
store volatile %struct.ScmObj* %args47140$k40531$2, %struct.ScmObj** %stackaddr$prim48891, align 8
%clofunc48892 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40531)
musttail call tailcc void %clofunc48892(%struct.ScmObj* %k40531, %struct.ScmObj* %args47140$k40531$2)
ret void
falsebranch$cmp48889:
%stackaddr$prim48893 = alloca %struct.ScmObj*, align 8
%anf_45bind40241 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40121)
store volatile %struct.ScmObj* %anf_45bind40241, %struct.ScmObj** %stackaddr$prim48893, align 8
%stackaddr$makeclosure48894 = alloca %struct.ScmObj*, align 8
%fptrToInt48895 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40778 to i64
%ae40778 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48895)
store volatile %struct.ScmObj* %ae40778, %struct.ScmObj** %stackaddr$makeclosure48894, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40778, %struct.ScmObj* %k40531, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40778, %struct.ScmObj* %f40122, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40778, %struct.ScmObj* %lst40121, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae40778, %struct.ScmObj* %_37map40120, i64 3)
%args47149$f40122$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48896 = alloca %struct.ScmObj*, align 8
%args47149$f40122$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40241, %struct.ScmObj* %args47149$f40122$0)
store volatile %struct.ScmObj* %args47149$f40122$1, %struct.ScmObj** %stackaddr$prim48896, align 8
%stackaddr$prim48897 = alloca %struct.ScmObj*, align 8
%args47149$f40122$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40778, %struct.ScmObj* %args47149$f40122$1)
store volatile %struct.ScmObj* %args47149$f40122$2, %struct.ScmObj** %stackaddr$prim48897, align 8
%clofunc48898 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40122)
musttail call tailcc void %clofunc48898(%struct.ScmObj* %f40122, %struct.ScmObj* %args47149$f40122$2)
ret void
}

define tailcc void @proc_clo$ae40778(%struct.ScmObj* %env$ae40778,%struct.ScmObj* %current_45args47141) {
%stackaddr$env-ref48899 = alloca %struct.ScmObj*, align 8
%k40531 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40778, i64 0)
store %struct.ScmObj* %k40531, %struct.ScmObj** %stackaddr$env-ref48899
%stackaddr$env-ref48900 = alloca %struct.ScmObj*, align 8
%f40122 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40778, i64 1)
store %struct.ScmObj* %f40122, %struct.ScmObj** %stackaddr$env-ref48900
%stackaddr$env-ref48901 = alloca %struct.ScmObj*, align 8
%lst40121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40778, i64 2)
store %struct.ScmObj* %lst40121, %struct.ScmObj** %stackaddr$env-ref48901
%stackaddr$env-ref48902 = alloca %struct.ScmObj*, align 8
%_37map40120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40778, i64 3)
store %struct.ScmObj* %_37map40120, %struct.ScmObj** %stackaddr$env-ref48902
%stackaddr$prim48903 = alloca %struct.ScmObj*, align 8
%_95k40532 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47141)
store volatile %struct.ScmObj* %_95k40532, %struct.ScmObj** %stackaddr$prim48903, align 8
%stackaddr$prim48904 = alloca %struct.ScmObj*, align 8
%current_45args47142 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47141)
store volatile %struct.ScmObj* %current_45args47142, %struct.ScmObj** %stackaddr$prim48904, align 8
%stackaddr$prim48905 = alloca %struct.ScmObj*, align 8
%anf_45bind40242 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47142)
store volatile %struct.ScmObj* %anf_45bind40242, %struct.ScmObj** %stackaddr$prim48905, align 8
%stackaddr$prim48906 = alloca %struct.ScmObj*, align 8
%anf_45bind40243 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40121)
store volatile %struct.ScmObj* %anf_45bind40243, %struct.ScmObj** %stackaddr$prim48906, align 8
%stackaddr$makeclosure48907 = alloca %struct.ScmObj*, align 8
%fptrToInt48908 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40782 to i64
%ae40782 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48908)
store volatile %struct.ScmObj* %ae40782, %struct.ScmObj** %stackaddr$makeclosure48907, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40782, %struct.ScmObj* %k40531, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40782, %struct.ScmObj* %anf_45bind40242, i64 1)
%args47148$_37map40120$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48909 = alloca %struct.ScmObj*, align 8
%args47148$_37map40120$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40243, %struct.ScmObj* %args47148$_37map40120$0)
store volatile %struct.ScmObj* %args47148$_37map40120$1, %struct.ScmObj** %stackaddr$prim48909, align 8
%stackaddr$prim48910 = alloca %struct.ScmObj*, align 8
%args47148$_37map40120$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40122, %struct.ScmObj* %args47148$_37map40120$1)
store volatile %struct.ScmObj* %args47148$_37map40120$2, %struct.ScmObj** %stackaddr$prim48910, align 8
%stackaddr$prim48911 = alloca %struct.ScmObj*, align 8
%args47148$_37map40120$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40782, %struct.ScmObj* %args47148$_37map40120$2)
store volatile %struct.ScmObj* %args47148$_37map40120$3, %struct.ScmObj** %stackaddr$prim48911, align 8
%clofunc48912 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map40120)
musttail call tailcc void %clofunc48912(%struct.ScmObj* %_37map40120, %struct.ScmObj* %args47148$_37map40120$3)
ret void
}

define tailcc void @proc_clo$ae40782(%struct.ScmObj* %env$ae40782,%struct.ScmObj* %current_45args47144) {
%stackaddr$env-ref48913 = alloca %struct.ScmObj*, align 8
%k40531 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40782, i64 0)
store %struct.ScmObj* %k40531, %struct.ScmObj** %stackaddr$env-ref48913
%stackaddr$env-ref48914 = alloca %struct.ScmObj*, align 8
%anf_45bind40242 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40782, i64 1)
store %struct.ScmObj* %anf_45bind40242, %struct.ScmObj** %stackaddr$env-ref48914
%stackaddr$prim48915 = alloca %struct.ScmObj*, align 8
%_95k40533 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47144)
store volatile %struct.ScmObj* %_95k40533, %struct.ScmObj** %stackaddr$prim48915, align 8
%stackaddr$prim48916 = alloca %struct.ScmObj*, align 8
%current_45args47145 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47144)
store volatile %struct.ScmObj* %current_45args47145, %struct.ScmObj** %stackaddr$prim48916, align 8
%stackaddr$prim48917 = alloca %struct.ScmObj*, align 8
%anf_45bind40244 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47145)
store volatile %struct.ScmObj* %anf_45bind40244, %struct.ScmObj** %stackaddr$prim48917, align 8
%stackaddr$prim48918 = alloca %struct.ScmObj*, align 8
%cpsprim40534 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40242, %struct.ScmObj* %anf_45bind40244)
store volatile %struct.ScmObj* %cpsprim40534, %struct.ScmObj** %stackaddr$prim48918, align 8
%ae40788 = call %struct.ScmObj* @const_init_int(i64 0)
%args47147$k40531$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48919 = alloca %struct.ScmObj*, align 8
%args47147$k40531$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40534, %struct.ScmObj* %args47147$k40531$0)
store volatile %struct.ScmObj* %args47147$k40531$1, %struct.ScmObj** %stackaddr$prim48919, align 8
%stackaddr$prim48920 = alloca %struct.ScmObj*, align 8
%args47147$k40531$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40788, %struct.ScmObj* %args47147$k40531$1)
store volatile %struct.ScmObj* %args47147$k40531$2, %struct.ScmObj** %stackaddr$prim48920, align 8
%clofunc48921 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40531)
musttail call tailcc void %clofunc48921(%struct.ScmObj* %k40531, %struct.ScmObj* %args47147$k40531$2)
ret void
}

define tailcc void @proc_clo$ae40681(%struct.ScmObj* %env$ae40681,%struct.ScmObj* %current_45args47153) {
%stackaddr$prim48922 = alloca %struct.ScmObj*, align 8
%k40535 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47153)
store volatile %struct.ScmObj* %k40535, %struct.ScmObj** %stackaddr$prim48922, align 8
%stackaddr$prim48923 = alloca %struct.ScmObj*, align 8
%current_45args47154 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47153)
store volatile %struct.ScmObj* %current_45args47154, %struct.ScmObj** %stackaddr$prim48923, align 8
%stackaddr$prim48924 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47154)
store volatile %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$prim48924, align 8
%ae40683 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48925 = alloca %struct.ScmObj*, align 8
%fptrToInt48926 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40684 to i64
%ae40684 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48926)
store volatile %struct.ScmObj* %ae40684, %struct.ScmObj** %stackaddr$makeclosure48925, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40684, %struct.ScmObj* %_37foldr140124, i64 0)
%args47167$k40535$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48927 = alloca %struct.ScmObj*, align 8
%args47167$k40535$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40684, %struct.ScmObj* %args47167$k40535$0)
store volatile %struct.ScmObj* %args47167$k40535$1, %struct.ScmObj** %stackaddr$prim48927, align 8
%stackaddr$prim48928 = alloca %struct.ScmObj*, align 8
%args47167$k40535$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40683, %struct.ScmObj* %args47167$k40535$1)
store volatile %struct.ScmObj* %args47167$k40535$2, %struct.ScmObj** %stackaddr$prim48928, align 8
%clofunc48929 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40535)
musttail call tailcc void %clofunc48929(%struct.ScmObj* %k40535, %struct.ScmObj* %args47167$k40535$2)
ret void
}

define tailcc void @proc_clo$ae40684(%struct.ScmObj* %env$ae40684,%struct.ScmObj* %current_45args47156) {
%stackaddr$env-ref48930 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40684, i64 0)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48930
%stackaddr$prim48931 = alloca %struct.ScmObj*, align 8
%k40536 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47156)
store volatile %struct.ScmObj* %k40536, %struct.ScmObj** %stackaddr$prim48931, align 8
%stackaddr$prim48932 = alloca %struct.ScmObj*, align 8
%current_45args47157 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47156)
store volatile %struct.ScmObj* %current_45args47157, %struct.ScmObj** %stackaddr$prim48932, align 8
%stackaddr$prim48933 = alloca %struct.ScmObj*, align 8
%f40127 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47157)
store volatile %struct.ScmObj* %f40127, %struct.ScmObj** %stackaddr$prim48933, align 8
%stackaddr$prim48934 = alloca %struct.ScmObj*, align 8
%current_45args47158 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47157)
store volatile %struct.ScmObj* %current_45args47158, %struct.ScmObj** %stackaddr$prim48934, align 8
%stackaddr$prim48935 = alloca %struct.ScmObj*, align 8
%acc40126 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47158)
store volatile %struct.ScmObj* %acc40126, %struct.ScmObj** %stackaddr$prim48935, align 8
%stackaddr$prim48936 = alloca %struct.ScmObj*, align 8
%current_45args47159 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47158)
store volatile %struct.ScmObj* %current_45args47159, %struct.ScmObj** %stackaddr$prim48936, align 8
%stackaddr$prim48937 = alloca %struct.ScmObj*, align 8
%lst40125 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47159)
store volatile %struct.ScmObj* %lst40125, %struct.ScmObj** %stackaddr$prim48937, align 8
%stackaddr$prim48938 = alloca %struct.ScmObj*, align 8
%anf_45bind40235 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40125)
store volatile %struct.ScmObj* %anf_45bind40235, %struct.ScmObj** %stackaddr$prim48938, align 8
%truthy$cmp48939 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40235)
%cmp$cmp48939 = icmp eq i64 %truthy$cmp48939, 1
br i1 %cmp$cmp48939, label %truebranch$cmp48939, label %falsebranch$cmp48939
truebranch$cmp48939:
%ae40688 = call %struct.ScmObj* @const_init_int(i64 0)
%args47161$k40536$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48940 = alloca %struct.ScmObj*, align 8
%args47161$k40536$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40126, %struct.ScmObj* %args47161$k40536$0)
store volatile %struct.ScmObj* %args47161$k40536$1, %struct.ScmObj** %stackaddr$prim48940, align 8
%stackaddr$prim48941 = alloca %struct.ScmObj*, align 8
%args47161$k40536$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40688, %struct.ScmObj* %args47161$k40536$1)
store volatile %struct.ScmObj* %args47161$k40536$2, %struct.ScmObj** %stackaddr$prim48941, align 8
%clofunc48942 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40536)
musttail call tailcc void %clofunc48942(%struct.ScmObj* %k40536, %struct.ScmObj* %args47161$k40536$2)
ret void
falsebranch$cmp48939:
%stackaddr$prim48943 = alloca %struct.ScmObj*, align 8
%anf_45bind40236 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40125)
store volatile %struct.ScmObj* %anf_45bind40236, %struct.ScmObj** %stackaddr$prim48943, align 8
%stackaddr$prim48944 = alloca %struct.ScmObj*, align 8
%anf_45bind40237 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40125)
store volatile %struct.ScmObj* %anf_45bind40237, %struct.ScmObj** %stackaddr$prim48944, align 8
%stackaddr$makeclosure48945 = alloca %struct.ScmObj*, align 8
%fptrToInt48946 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40696 to i64
%ae40696 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48946)
store volatile %struct.ScmObj* %ae40696, %struct.ScmObj** %stackaddr$makeclosure48945, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40696, %struct.ScmObj* %f40127, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40696, %struct.ScmObj* %anf_45bind40236, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40696, %struct.ScmObj* %k40536, i64 2)
%args47166$_37foldr140124$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48947 = alloca %struct.ScmObj*, align 8
%args47166$_37foldr140124$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40237, %struct.ScmObj* %args47166$_37foldr140124$0)
store volatile %struct.ScmObj* %args47166$_37foldr140124$1, %struct.ScmObj** %stackaddr$prim48947, align 8
%stackaddr$prim48948 = alloca %struct.ScmObj*, align 8
%args47166$_37foldr140124$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40126, %struct.ScmObj* %args47166$_37foldr140124$1)
store volatile %struct.ScmObj* %args47166$_37foldr140124$2, %struct.ScmObj** %stackaddr$prim48948, align 8
%stackaddr$prim48949 = alloca %struct.ScmObj*, align 8
%args47166$_37foldr140124$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40127, %struct.ScmObj* %args47166$_37foldr140124$2)
store volatile %struct.ScmObj* %args47166$_37foldr140124$3, %struct.ScmObj** %stackaddr$prim48949, align 8
%stackaddr$prim48950 = alloca %struct.ScmObj*, align 8
%args47166$_37foldr140124$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40696, %struct.ScmObj* %args47166$_37foldr140124$3)
store volatile %struct.ScmObj* %args47166$_37foldr140124$4, %struct.ScmObj** %stackaddr$prim48950, align 8
%clofunc48951 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140124)
musttail call tailcc void %clofunc48951(%struct.ScmObj* %_37foldr140124, %struct.ScmObj* %args47166$_37foldr140124$4)
ret void
}

define tailcc void @proc_clo$ae40696(%struct.ScmObj* %env$ae40696,%struct.ScmObj* %current_45args47162) {
%stackaddr$env-ref48952 = alloca %struct.ScmObj*, align 8
%f40127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40696, i64 0)
store %struct.ScmObj* %f40127, %struct.ScmObj** %stackaddr$env-ref48952
%stackaddr$env-ref48953 = alloca %struct.ScmObj*, align 8
%anf_45bind40236 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40696, i64 1)
store %struct.ScmObj* %anf_45bind40236, %struct.ScmObj** %stackaddr$env-ref48953
%stackaddr$env-ref48954 = alloca %struct.ScmObj*, align 8
%k40536 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40696, i64 2)
store %struct.ScmObj* %k40536, %struct.ScmObj** %stackaddr$env-ref48954
%stackaddr$prim48955 = alloca %struct.ScmObj*, align 8
%_95k40537 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47162)
store volatile %struct.ScmObj* %_95k40537, %struct.ScmObj** %stackaddr$prim48955, align 8
%stackaddr$prim48956 = alloca %struct.ScmObj*, align 8
%current_45args47163 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47162)
store volatile %struct.ScmObj* %current_45args47163, %struct.ScmObj** %stackaddr$prim48956, align 8
%stackaddr$prim48957 = alloca %struct.ScmObj*, align 8
%anf_45bind40238 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47163)
store volatile %struct.ScmObj* %anf_45bind40238, %struct.ScmObj** %stackaddr$prim48957, align 8
%args47165$f40127$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48958 = alloca %struct.ScmObj*, align 8
%args47165$f40127$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40238, %struct.ScmObj* %args47165$f40127$0)
store volatile %struct.ScmObj* %args47165$f40127$1, %struct.ScmObj** %stackaddr$prim48958, align 8
%stackaddr$prim48959 = alloca %struct.ScmObj*, align 8
%args47165$f40127$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40236, %struct.ScmObj* %args47165$f40127$1)
store volatile %struct.ScmObj* %args47165$f40127$2, %struct.ScmObj** %stackaddr$prim48959, align 8
%stackaddr$prim48960 = alloca %struct.ScmObj*, align 8
%args47165$f40127$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40536, %struct.ScmObj* %args47165$f40127$2)
store volatile %struct.ScmObj* %args47165$f40127$3, %struct.ScmObj** %stackaddr$prim48960, align 8
%clofunc48961 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40127)
musttail call tailcc void %clofunc48961(%struct.ScmObj* %f40127, %struct.ScmObj* %args47165$f40127$3)
ret void
}

define tailcc void @proc_clo$ae40564(%struct.ScmObj* %env$ae40564,%struct.ScmObj* %current_45args47170) {
%stackaddr$prim48962 = alloca %struct.ScmObj*, align 8
%k40538 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47170)
store volatile %struct.ScmObj* %k40538, %struct.ScmObj** %stackaddr$prim48962, align 8
%stackaddr$prim48963 = alloca %struct.ScmObj*, align 8
%current_45args47171 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47170)
store volatile %struct.ScmObj* %current_45args47171, %struct.ScmObj** %stackaddr$prim48963, align 8
%stackaddr$prim48964 = alloca %struct.ScmObj*, align 8
%y40104 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47171)
store volatile %struct.ScmObj* %y40104, %struct.ScmObj** %stackaddr$prim48964, align 8
%ae40566 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48965 = alloca %struct.ScmObj*, align 8
%fptrToInt48966 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40567 to i64
%ae40567 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48966)
store volatile %struct.ScmObj* %ae40567, %struct.ScmObj** %stackaddr$makeclosure48965, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40567, %struct.ScmObj* %y40104, i64 0)
%args47189$k40538$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48967 = alloca %struct.ScmObj*, align 8
%args47189$k40538$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40567, %struct.ScmObj* %args47189$k40538$0)
store volatile %struct.ScmObj* %args47189$k40538$1, %struct.ScmObj** %stackaddr$prim48967, align 8
%stackaddr$prim48968 = alloca %struct.ScmObj*, align 8
%args47189$k40538$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40566, %struct.ScmObj* %args47189$k40538$1)
store volatile %struct.ScmObj* %args47189$k40538$2, %struct.ScmObj** %stackaddr$prim48968, align 8
%clofunc48969 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40538)
musttail call tailcc void %clofunc48969(%struct.ScmObj* %k40538, %struct.ScmObj* %args47189$k40538$2)
ret void
}

define tailcc void @proc_clo$ae40567(%struct.ScmObj* %env$ae40567,%struct.ScmObj* %current_45args47173) {
%stackaddr$env-ref48970 = alloca %struct.ScmObj*, align 8
%y40104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40567, i64 0)
store %struct.ScmObj* %y40104, %struct.ScmObj** %stackaddr$env-ref48970
%stackaddr$prim48971 = alloca %struct.ScmObj*, align 8
%k40539 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47173)
store volatile %struct.ScmObj* %k40539, %struct.ScmObj** %stackaddr$prim48971, align 8
%stackaddr$prim48972 = alloca %struct.ScmObj*, align 8
%current_45args47174 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47173)
store volatile %struct.ScmObj* %current_45args47174, %struct.ScmObj** %stackaddr$prim48972, align 8
%stackaddr$prim48973 = alloca %struct.ScmObj*, align 8
%f40105 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47174)
store volatile %struct.ScmObj* %f40105, %struct.ScmObj** %stackaddr$prim48973, align 8
%stackaddr$makeclosure48974 = alloca %struct.ScmObj*, align 8
%fptrToInt48975 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40568 to i64
%ae40568 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48975)
store volatile %struct.ScmObj* %ae40568, %struct.ScmObj** %stackaddr$makeclosure48974, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40568, %struct.ScmObj* %k40539, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40568, %struct.ScmObj* %f40105, i64 1)
%ae40569 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48976 = alloca %struct.ScmObj*, align 8
%fptrToInt48977 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40570 to i64
%ae40570 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48977)
store volatile %struct.ScmObj* %ae40570, %struct.ScmObj** %stackaddr$makeclosure48976, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40570, %struct.ScmObj* %f40105, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40570, %struct.ScmObj* %y40104, i64 1)
%args47188$ae40568$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48978 = alloca %struct.ScmObj*, align 8
%args47188$ae40568$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40570, %struct.ScmObj* %args47188$ae40568$0)
store volatile %struct.ScmObj* %args47188$ae40568$1, %struct.ScmObj** %stackaddr$prim48978, align 8
%stackaddr$prim48979 = alloca %struct.ScmObj*, align 8
%args47188$ae40568$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40569, %struct.ScmObj* %args47188$ae40568$1)
store volatile %struct.ScmObj* %args47188$ae40568$2, %struct.ScmObj** %stackaddr$prim48979, align 8
%clofunc48980 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40568)
musttail call tailcc void %clofunc48980(%struct.ScmObj* %ae40568, %struct.ScmObj* %args47188$ae40568$2)
ret void
}

define tailcc void @proc_clo$ae40568(%struct.ScmObj* %env$ae40568,%struct.ScmObj* %current_45args47176) {
%stackaddr$env-ref48981 = alloca %struct.ScmObj*, align 8
%k40539 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40568, i64 0)
store %struct.ScmObj* %k40539, %struct.ScmObj** %stackaddr$env-ref48981
%stackaddr$env-ref48982 = alloca %struct.ScmObj*, align 8
%f40105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40568, i64 1)
store %struct.ScmObj* %f40105, %struct.ScmObj** %stackaddr$env-ref48982
%stackaddr$prim48983 = alloca %struct.ScmObj*, align 8
%_95k40540 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47176)
store volatile %struct.ScmObj* %_95k40540, %struct.ScmObj** %stackaddr$prim48983, align 8
%stackaddr$prim48984 = alloca %struct.ScmObj*, align 8
%current_45args47177 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47176)
store volatile %struct.ScmObj* %current_45args47177, %struct.ScmObj** %stackaddr$prim48984, align 8
%stackaddr$prim48985 = alloca %struct.ScmObj*, align 8
%anf_45bind40233 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47177)
store volatile %struct.ScmObj* %anf_45bind40233, %struct.ScmObj** %stackaddr$prim48985, align 8
%args47179$f40105$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48986 = alloca %struct.ScmObj*, align 8
%args47179$f40105$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40233, %struct.ScmObj* %args47179$f40105$0)
store volatile %struct.ScmObj* %args47179$f40105$1, %struct.ScmObj** %stackaddr$prim48986, align 8
%stackaddr$prim48987 = alloca %struct.ScmObj*, align 8
%args47179$f40105$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40539, %struct.ScmObj* %args47179$f40105$1)
store volatile %struct.ScmObj* %args47179$f40105$2, %struct.ScmObj** %stackaddr$prim48987, align 8
%clofunc48988 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40105)
musttail call tailcc void %clofunc48988(%struct.ScmObj* %f40105, %struct.ScmObj* %args47179$f40105$2)
ret void
}

define tailcc void @proc_clo$ae40570(%struct.ScmObj* %env$ae40570,%struct.ScmObj* %args4010640541) {
%stackaddr$env-ref48989 = alloca %struct.ScmObj*, align 8
%f40105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40570, i64 0)
store %struct.ScmObj* %f40105, %struct.ScmObj** %stackaddr$env-ref48989
%stackaddr$env-ref48990 = alloca %struct.ScmObj*, align 8
%y40104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40570, i64 1)
store %struct.ScmObj* %y40104, %struct.ScmObj** %stackaddr$env-ref48990
%stackaddr$prim48991 = alloca %struct.ScmObj*, align 8
%k40542 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4010640541)
store volatile %struct.ScmObj* %k40542, %struct.ScmObj** %stackaddr$prim48991, align 8
%stackaddr$prim48992 = alloca %struct.ScmObj*, align 8
%args40106 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4010640541)
store volatile %struct.ScmObj* %args40106, %struct.ScmObj** %stackaddr$prim48992, align 8
%stackaddr$makeclosure48993 = alloca %struct.ScmObj*, align 8
%fptrToInt48994 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40574 to i64
%ae40574 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48994)
store volatile %struct.ScmObj* %ae40574, %struct.ScmObj** %stackaddr$makeclosure48993, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40574, %struct.ScmObj* %k40542, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40574, %struct.ScmObj* %args40106, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40574, %struct.ScmObj* %f40105, i64 2)
%args47187$y40104$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48995 = alloca %struct.ScmObj*, align 8
%args47187$y40104$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y40104, %struct.ScmObj* %args47187$y40104$0)
store volatile %struct.ScmObj* %args47187$y40104$1, %struct.ScmObj** %stackaddr$prim48995, align 8
%stackaddr$prim48996 = alloca %struct.ScmObj*, align 8
%args47187$y40104$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40574, %struct.ScmObj* %args47187$y40104$1)
store volatile %struct.ScmObj* %args47187$y40104$2, %struct.ScmObj** %stackaddr$prim48996, align 8
%clofunc48997 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y40104)
musttail call tailcc void %clofunc48997(%struct.ScmObj* %y40104, %struct.ScmObj* %args47187$y40104$2)
ret void
}

define tailcc void @proc_clo$ae40574(%struct.ScmObj* %env$ae40574,%struct.ScmObj* %current_45args47180) {
%stackaddr$env-ref48998 = alloca %struct.ScmObj*, align 8
%k40542 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40574, i64 0)
store %struct.ScmObj* %k40542, %struct.ScmObj** %stackaddr$env-ref48998
%stackaddr$env-ref48999 = alloca %struct.ScmObj*, align 8
%args40106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40574, i64 1)
store %struct.ScmObj* %args40106, %struct.ScmObj** %stackaddr$env-ref48999
%stackaddr$env-ref49000 = alloca %struct.ScmObj*, align 8
%f40105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40574, i64 2)
store %struct.ScmObj* %f40105, %struct.ScmObj** %stackaddr$env-ref49000
%stackaddr$prim49001 = alloca %struct.ScmObj*, align 8
%_95k40543 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47180)
store volatile %struct.ScmObj* %_95k40543, %struct.ScmObj** %stackaddr$prim49001, align 8
%stackaddr$prim49002 = alloca %struct.ScmObj*, align 8
%current_45args47181 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47180)
store volatile %struct.ScmObj* %current_45args47181, %struct.ScmObj** %stackaddr$prim49002, align 8
%stackaddr$prim49003 = alloca %struct.ScmObj*, align 8
%anf_45bind40231 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47181)
store volatile %struct.ScmObj* %anf_45bind40231, %struct.ScmObj** %stackaddr$prim49003, align 8
%stackaddr$makeclosure49004 = alloca %struct.ScmObj*, align 8
%fptrToInt49005 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40577 to i64
%ae40577 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49005)
store volatile %struct.ScmObj* %ae40577, %struct.ScmObj** %stackaddr$makeclosure49004, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40577, %struct.ScmObj* %k40542, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40577, %struct.ScmObj* %args40106, i64 1)
%args47186$anf_45bind40231$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49006 = alloca %struct.ScmObj*, align 8
%args47186$anf_45bind40231$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40105, %struct.ScmObj* %args47186$anf_45bind40231$0)
store volatile %struct.ScmObj* %args47186$anf_45bind40231$1, %struct.ScmObj** %stackaddr$prim49006, align 8
%stackaddr$prim49007 = alloca %struct.ScmObj*, align 8
%args47186$anf_45bind40231$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40577, %struct.ScmObj* %args47186$anf_45bind40231$1)
store volatile %struct.ScmObj* %args47186$anf_45bind40231$2, %struct.ScmObj** %stackaddr$prim49007, align 8
%clofunc49008 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40231)
musttail call tailcc void %clofunc49008(%struct.ScmObj* %anf_45bind40231, %struct.ScmObj* %args47186$anf_45bind40231$2)
ret void
}

define tailcc void @proc_clo$ae40577(%struct.ScmObj* %env$ae40577,%struct.ScmObj* %current_45args47183) {
%stackaddr$env-ref49009 = alloca %struct.ScmObj*, align 8
%k40542 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40577, i64 0)
store %struct.ScmObj* %k40542, %struct.ScmObj** %stackaddr$env-ref49009
%stackaddr$env-ref49010 = alloca %struct.ScmObj*, align 8
%args40106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40577, i64 1)
store %struct.ScmObj* %args40106, %struct.ScmObj** %stackaddr$env-ref49010
%stackaddr$prim49011 = alloca %struct.ScmObj*, align 8
%_95k40544 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47183)
store volatile %struct.ScmObj* %_95k40544, %struct.ScmObj** %stackaddr$prim49011, align 8
%stackaddr$prim49012 = alloca %struct.ScmObj*, align 8
%current_45args47184 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47183)
store volatile %struct.ScmObj* %current_45args47184, %struct.ScmObj** %stackaddr$prim49012, align 8
%stackaddr$prim49013 = alloca %struct.ScmObj*, align 8
%anf_45bind40232 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47184)
store volatile %struct.ScmObj* %anf_45bind40232, %struct.ScmObj** %stackaddr$prim49013, align 8
%stackaddr$prim49014 = alloca %struct.ScmObj*, align 8
%cpsargs40545 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40542, %struct.ScmObj* %args40106)
store volatile %struct.ScmObj* %cpsargs40545, %struct.ScmObj** %stackaddr$prim49014, align 8
%clofunc49015 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40232)
musttail call tailcc void %clofunc49015(%struct.ScmObj* %anf_45bind40232, %struct.ScmObj* %cpsargs40545)
ret void
}

define tailcc void @proc_clo$ae40549(%struct.ScmObj* %env$ae40549,%struct.ScmObj* %current_45args47191) {
%stackaddr$prim49016 = alloca %struct.ScmObj*, align 8
%k40546 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47191)
store volatile %struct.ScmObj* %k40546, %struct.ScmObj** %stackaddr$prim49016, align 8
%stackaddr$prim49017 = alloca %struct.ScmObj*, align 8
%current_45args47192 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47191)
store volatile %struct.ScmObj* %current_45args47192, %struct.ScmObj** %stackaddr$prim49017, align 8
%stackaddr$prim49018 = alloca %struct.ScmObj*, align 8
%yu40103 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47192)
store volatile %struct.ScmObj* %yu40103, %struct.ScmObj** %stackaddr$prim49018, align 8
%args47194$yu40103$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49019 = alloca %struct.ScmObj*, align 8
%args47194$yu40103$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu40103, %struct.ScmObj* %args47194$yu40103$0)
store volatile %struct.ScmObj* %args47194$yu40103$1, %struct.ScmObj** %stackaddr$prim49019, align 8
%stackaddr$prim49020 = alloca %struct.ScmObj*, align 8
%args47194$yu40103$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40546, %struct.ScmObj* %args47194$yu40103$1)
store volatile %struct.ScmObj* %args47194$yu40103$2, %struct.ScmObj** %stackaddr$prim49020, align 8
%clofunc49021 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu40103)
musttail call tailcc void %clofunc49021(%struct.ScmObj* %yu40103, %struct.ScmObj* %args47194$yu40103$2)
ret void
}