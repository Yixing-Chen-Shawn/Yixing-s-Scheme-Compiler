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

@global$sym$ae4391747531 = private unnamed_addr constant [8 x i8] c"promise\00", align 8

define ccc i32 @main() {
%mainenv46976 = call %struct.ScmObj* @const_init_null()
%mainargs46977 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv46976, %struct.ScmObj* %mainargs46977)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv46974,%struct.ScmObj* %mainargs46975) {
%stackaddr$makeclosure46978 = alloca %struct.ScmObj*, align 8
%fptrToInt46979 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40529 to i64
%ae40529 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt46979)
store volatile %struct.ScmObj* %ae40529, %struct.ScmObj** %stackaddr$makeclosure46978, align 8
%ae40530 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure46980 = alloca %struct.ScmObj*, align 8
%fptrToInt46981 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40531 to i64
%ae40531 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt46981)
store volatile %struct.ScmObj* %ae40531, %struct.ScmObj** %stackaddr$makeclosure46980, align 8
%args46973$ae40529$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim46982 = alloca %struct.ScmObj*, align 8
%args46973$ae40529$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40531, %struct.ScmObj* %args46973$ae40529$0)
store volatile %struct.ScmObj* %args46973$ae40529$1, %struct.ScmObj** %stackaddr$prim46982, align 8
%stackaddr$prim46983 = alloca %struct.ScmObj*, align 8
%args46973$ae40529$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40530, %struct.ScmObj* %args46973$ae40529$1)
store volatile %struct.ScmObj* %args46973$ae40529$2, %struct.ScmObj** %stackaddr$prim46983, align 8
%clofunc46984 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40529)
musttail call tailcc void %clofunc46984(%struct.ScmObj* %ae40529, %struct.ScmObj* %args46973$ae40529$2)
ret void
}

define tailcc void @proc_clo$ae40529(%struct.ScmObj* %env$ae40529,%struct.ScmObj* %current_45args46401) {
%stackaddr$prim46985 = alloca %struct.ScmObj*, align 8
%_95k40350 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46401)
store volatile %struct.ScmObj* %_95k40350, %struct.ScmObj** %stackaddr$prim46985, align 8
%stackaddr$prim46986 = alloca %struct.ScmObj*, align 8
%current_45args46402 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46401)
store volatile %struct.ScmObj* %current_45args46402, %struct.ScmObj** %stackaddr$prim46986, align 8
%stackaddr$prim46987 = alloca %struct.ScmObj*, align 8
%anf_45bind40227 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46402)
store volatile %struct.ScmObj* %anf_45bind40227, %struct.ScmObj** %stackaddr$prim46987, align 8
%stackaddr$makeclosure46988 = alloca %struct.ScmObj*, align 8
%fptrToInt46989 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40544 to i64
%ae40544 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt46989)
store volatile %struct.ScmObj* %ae40544, %struct.ScmObj** %stackaddr$makeclosure46988, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40544, %struct.ScmObj* %anf_45bind40227, i64 0)
%ae40545 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure46990 = alloca %struct.ScmObj*, align 8
%fptrToInt46991 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40546 to i64
%ae40546 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt46991)
store volatile %struct.ScmObj* %ae40546, %struct.ScmObj** %stackaddr$makeclosure46990, align 8
%args46968$ae40544$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim46992 = alloca %struct.ScmObj*, align 8
%args46968$ae40544$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40546, %struct.ScmObj* %args46968$ae40544$0)
store volatile %struct.ScmObj* %args46968$ae40544$1, %struct.ScmObj** %stackaddr$prim46992, align 8
%stackaddr$prim46993 = alloca %struct.ScmObj*, align 8
%args46968$ae40544$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40545, %struct.ScmObj* %args46968$ae40544$1)
store volatile %struct.ScmObj* %args46968$ae40544$2, %struct.ScmObj** %stackaddr$prim46993, align 8
%clofunc46994 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40544)
musttail call tailcc void %clofunc46994(%struct.ScmObj* %ae40544, %struct.ScmObj* %args46968$ae40544$2)
ret void
}

define tailcc void @proc_clo$ae40544(%struct.ScmObj* %env$ae40544,%struct.ScmObj* %current_45args46404) {
%stackaddr$env-ref46995 = alloca %struct.ScmObj*, align 8
%anf_45bind40227 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40544, i64 0)
store %struct.ScmObj* %anf_45bind40227, %struct.ScmObj** %stackaddr$env-ref46995
%stackaddr$prim46996 = alloca %struct.ScmObj*, align 8
%_95k40351 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46404)
store volatile %struct.ScmObj* %_95k40351, %struct.ScmObj** %stackaddr$prim46996, align 8
%stackaddr$prim46997 = alloca %struct.ScmObj*, align 8
%current_45args46405 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46404)
store volatile %struct.ScmObj* %current_45args46405, %struct.ScmObj** %stackaddr$prim46997, align 8
%stackaddr$prim46998 = alloca %struct.ScmObj*, align 8
%anf_45bind40231 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46405)
store volatile %struct.ScmObj* %anf_45bind40231, %struct.ScmObj** %stackaddr$prim46998, align 8
%stackaddr$makeclosure46999 = alloca %struct.ScmObj*, align 8
%fptrToInt47000 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40659 to i64
%ae40659 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47000)
store volatile %struct.ScmObj* %ae40659, %struct.ScmObj** %stackaddr$makeclosure46999, align 8
%args46947$anf_45bind40227$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47001 = alloca %struct.ScmObj*, align 8
%args46947$anf_45bind40227$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40231, %struct.ScmObj* %args46947$anf_45bind40227$0)
store volatile %struct.ScmObj* %args46947$anf_45bind40227$1, %struct.ScmObj** %stackaddr$prim47001, align 8
%stackaddr$prim47002 = alloca %struct.ScmObj*, align 8
%args46947$anf_45bind40227$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40659, %struct.ScmObj* %args46947$anf_45bind40227$1)
store volatile %struct.ScmObj* %args46947$anf_45bind40227$2, %struct.ScmObj** %stackaddr$prim47002, align 8
%clofunc47003 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40227)
musttail call tailcc void %clofunc47003(%struct.ScmObj* %anf_45bind40227, %struct.ScmObj* %args46947$anf_45bind40227$2)
ret void
}

define tailcc void @proc_clo$ae40659(%struct.ScmObj* %env$ae40659,%struct.ScmObj* %current_45args46407) {
%stackaddr$prim47004 = alloca %struct.ScmObj*, align 8
%_95k40352 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46407)
store volatile %struct.ScmObj* %_95k40352, %struct.ScmObj** %stackaddr$prim47004, align 8
%stackaddr$prim47005 = alloca %struct.ScmObj*, align 8
%current_45args46408 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46407)
store volatile %struct.ScmObj* %current_45args46408, %struct.ScmObj** %stackaddr$prim47005, align 8
%stackaddr$prim47006 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46408)
store volatile %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$prim47006, align 8
%stackaddr$makeclosure47007 = alloca %struct.ScmObj*, align 8
%fptrToInt47008 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40661 to i64
%ae40661 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47008)
store volatile %struct.ScmObj* %ae40661, %struct.ScmObj** %stackaddr$makeclosure47007, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40661, %struct.ScmObj* %Ycmb40102, i64 0)
%ae40662 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47009 = alloca %struct.ScmObj*, align 8
%fptrToInt47010 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40663 to i64
%ae40663 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47010)
store volatile %struct.ScmObj* %ae40663, %struct.ScmObj** %stackaddr$makeclosure47009, align 8
%args46946$ae40661$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47011 = alloca %struct.ScmObj*, align 8
%args46946$ae40661$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40663, %struct.ScmObj* %args46946$ae40661$0)
store volatile %struct.ScmObj* %args46946$ae40661$1, %struct.ScmObj** %stackaddr$prim47011, align 8
%stackaddr$prim47012 = alloca %struct.ScmObj*, align 8
%args46946$ae40661$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40662, %struct.ScmObj* %args46946$ae40661$1)
store volatile %struct.ScmObj* %args46946$ae40661$2, %struct.ScmObj** %stackaddr$prim47012, align 8
%clofunc47013 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40661)
musttail call tailcc void %clofunc47013(%struct.ScmObj* %ae40661, %struct.ScmObj* %args46946$ae40661$2)
ret void
}

define tailcc void @proc_clo$ae40661(%struct.ScmObj* %env$ae40661,%struct.ScmObj* %current_45args46410) {
%stackaddr$env-ref47014 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40661, i64 0)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47014
%stackaddr$prim47015 = alloca %struct.ScmObj*, align 8
%_95k40353 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46410)
store volatile %struct.ScmObj* %_95k40353, %struct.ScmObj** %stackaddr$prim47015, align 8
%stackaddr$prim47016 = alloca %struct.ScmObj*, align 8
%current_45args46411 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46410)
store volatile %struct.ScmObj* %current_45args46411, %struct.ScmObj** %stackaddr$prim47016, align 8
%stackaddr$prim47017 = alloca %struct.ScmObj*, align 8
%anf_45bind40236 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46411)
store volatile %struct.ScmObj* %anf_45bind40236, %struct.ScmObj** %stackaddr$prim47017, align 8
%stackaddr$makeclosure47018 = alloca %struct.ScmObj*, align 8
%fptrToInt47019 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40739 to i64
%ae40739 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47019)
store volatile %struct.ScmObj* %ae40739, %struct.ScmObj** %stackaddr$makeclosure47018, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40739, %struct.ScmObj* %Ycmb40102, i64 0)
%args46930$Ycmb40102$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47020 = alloca %struct.ScmObj*, align 8
%args46930$Ycmb40102$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40236, %struct.ScmObj* %args46930$Ycmb40102$0)
store volatile %struct.ScmObj* %args46930$Ycmb40102$1, %struct.ScmObj** %stackaddr$prim47020, align 8
%stackaddr$prim47021 = alloca %struct.ScmObj*, align 8
%args46930$Ycmb40102$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40739, %struct.ScmObj* %args46930$Ycmb40102$1)
store volatile %struct.ScmObj* %args46930$Ycmb40102$2, %struct.ScmObj** %stackaddr$prim47021, align 8
%clofunc47022 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40102)
musttail call tailcc void %clofunc47022(%struct.ScmObj* %Ycmb40102, %struct.ScmObj* %args46930$Ycmb40102$2)
ret void
}

define tailcc void @proc_clo$ae40739(%struct.ScmObj* %env$ae40739,%struct.ScmObj* %current_45args46413) {
%stackaddr$env-ref47023 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40739, i64 0)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47023
%stackaddr$prim47024 = alloca %struct.ScmObj*, align 8
%_95k40354 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46413)
store volatile %struct.ScmObj* %_95k40354, %struct.ScmObj** %stackaddr$prim47024, align 8
%stackaddr$prim47025 = alloca %struct.ScmObj*, align 8
%current_45args46414 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46413)
store volatile %struct.ScmObj* %current_45args46414, %struct.ScmObj** %stackaddr$prim47025, align 8
%stackaddr$prim47026 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46414)
store volatile %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$prim47026, align 8
%stackaddr$makeclosure47027 = alloca %struct.ScmObj*, align 8
%fptrToInt47028 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40741 to i64
%ae40741 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47028)
store volatile %struct.ScmObj* %ae40741, %struct.ScmObj** %stackaddr$makeclosure47027, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40741, %struct.ScmObj* %Ycmb40102, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40741, %struct.ScmObj* %_37foldr140123, i64 1)
%ae40742 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47029 = alloca %struct.ScmObj*, align 8
%fptrToInt47030 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40743 to i64
%ae40743 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47030)
store volatile %struct.ScmObj* %ae40743, %struct.ScmObj** %stackaddr$makeclosure47029, align 8
%args46929$ae40741$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47031 = alloca %struct.ScmObj*, align 8
%args46929$ae40741$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40743, %struct.ScmObj* %args46929$ae40741$0)
store volatile %struct.ScmObj* %args46929$ae40741$1, %struct.ScmObj** %stackaddr$prim47031, align 8
%stackaddr$prim47032 = alloca %struct.ScmObj*, align 8
%args46929$ae40741$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40742, %struct.ScmObj* %args46929$ae40741$1)
store volatile %struct.ScmObj* %args46929$ae40741$2, %struct.ScmObj** %stackaddr$prim47032, align 8
%clofunc47033 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40741)
musttail call tailcc void %clofunc47033(%struct.ScmObj* %ae40741, %struct.ScmObj* %args46929$ae40741$2)
ret void
}

define tailcc void @proc_clo$ae40741(%struct.ScmObj* %env$ae40741,%struct.ScmObj* %current_45args46416) {
%stackaddr$env-ref47034 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40741, i64 0)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47034
%stackaddr$env-ref47035 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40741, i64 1)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47035
%stackaddr$prim47036 = alloca %struct.ScmObj*, align 8
%_95k40355 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46416)
store volatile %struct.ScmObj* %_95k40355, %struct.ScmObj** %stackaddr$prim47036, align 8
%stackaddr$prim47037 = alloca %struct.ScmObj*, align 8
%current_45args46417 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46416)
store volatile %struct.ScmObj* %current_45args46417, %struct.ScmObj** %stackaddr$prim47037, align 8
%stackaddr$prim47038 = alloca %struct.ScmObj*, align 8
%anf_45bind40242 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46417)
store volatile %struct.ScmObj* %anf_45bind40242, %struct.ScmObj** %stackaddr$prim47038, align 8
%stackaddr$makeclosure47039 = alloca %struct.ScmObj*, align 8
%fptrToInt47040 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40836 to i64
%ae40836 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47040)
store volatile %struct.ScmObj* %ae40836, %struct.ScmObj** %stackaddr$makeclosure47039, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40836, %struct.ScmObj* %Ycmb40102, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40836, %struct.ScmObj* %_37foldr140123, i64 1)
%args46910$Ycmb40102$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47041 = alloca %struct.ScmObj*, align 8
%args46910$Ycmb40102$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40242, %struct.ScmObj* %args46910$Ycmb40102$0)
store volatile %struct.ScmObj* %args46910$Ycmb40102$1, %struct.ScmObj** %stackaddr$prim47041, align 8
%stackaddr$prim47042 = alloca %struct.ScmObj*, align 8
%args46910$Ycmb40102$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40836, %struct.ScmObj* %args46910$Ycmb40102$1)
store volatile %struct.ScmObj* %args46910$Ycmb40102$2, %struct.ScmObj** %stackaddr$prim47042, align 8
%clofunc47043 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40102)
musttail call tailcc void %clofunc47043(%struct.ScmObj* %Ycmb40102, %struct.ScmObj* %args46910$Ycmb40102$2)
ret void
}

define tailcc void @proc_clo$ae40836(%struct.ScmObj* %env$ae40836,%struct.ScmObj* %current_45args46419) {
%stackaddr$env-ref47044 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40836, i64 0)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47044
%stackaddr$env-ref47045 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40836, i64 1)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47045
%stackaddr$prim47046 = alloca %struct.ScmObj*, align 8
%_95k40356 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46419)
store volatile %struct.ScmObj* %_95k40356, %struct.ScmObj** %stackaddr$prim47046, align 8
%stackaddr$prim47047 = alloca %struct.ScmObj*, align 8
%current_45args46420 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46419)
store volatile %struct.ScmObj* %current_45args46420, %struct.ScmObj** %stackaddr$prim47047, align 8
%stackaddr$prim47048 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46420)
store volatile %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$prim47048, align 8
%stackaddr$makeclosure47049 = alloca %struct.ScmObj*, align 8
%fptrToInt47050 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40838 to i64
%ae40838 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47050)
store volatile %struct.ScmObj* %ae40838, %struct.ScmObj** %stackaddr$makeclosure47049, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40838, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40838, %struct.ScmObj* %Ycmb40102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40838, %struct.ScmObj* %_37foldr140123, i64 2)
%ae40839 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47051 = alloca %struct.ScmObj*, align 8
%fptrToInt47052 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40840 to i64
%ae40840 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47052)
store volatile %struct.ScmObj* %ae40840, %struct.ScmObj** %stackaddr$makeclosure47051, align 8
%args46909$ae40838$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47053 = alloca %struct.ScmObj*, align 8
%args46909$ae40838$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40840, %struct.ScmObj* %args46909$ae40838$0)
store volatile %struct.ScmObj* %args46909$ae40838$1, %struct.ScmObj** %stackaddr$prim47053, align 8
%stackaddr$prim47054 = alloca %struct.ScmObj*, align 8
%args46909$ae40838$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40839, %struct.ScmObj* %args46909$ae40838$1)
store volatile %struct.ScmObj* %args46909$ae40838$2, %struct.ScmObj** %stackaddr$prim47054, align 8
%clofunc47055 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40838)
musttail call tailcc void %clofunc47055(%struct.ScmObj* %ae40838, %struct.ScmObj* %args46909$ae40838$2)
ret void
}

define tailcc void @proc_clo$ae40838(%struct.ScmObj* %env$ae40838,%struct.ScmObj* %current_45args46422) {
%stackaddr$env-ref47056 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40838, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47056
%stackaddr$env-ref47057 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40838, i64 1)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47057
%stackaddr$env-ref47058 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40838, i64 2)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47058
%stackaddr$prim47059 = alloca %struct.ScmObj*, align 8
%_95k40357 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46422)
store volatile %struct.ScmObj* %_95k40357, %struct.ScmObj** %stackaddr$prim47059, align 8
%stackaddr$prim47060 = alloca %struct.ScmObj*, align 8
%current_45args46423 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46422)
store volatile %struct.ScmObj* %current_45args46423, %struct.ScmObj** %stackaddr$prim47060, align 8
%stackaddr$prim47061 = alloca %struct.ScmObj*, align 8
%anf_45bind40249 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46423)
store volatile %struct.ScmObj* %anf_45bind40249, %struct.ScmObj** %stackaddr$prim47061, align 8
%stackaddr$makeclosure47062 = alloca %struct.ScmObj*, align 8
%fptrToInt47063 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40986 to i64
%ae40986 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47063)
store volatile %struct.ScmObj* %ae40986, %struct.ScmObj** %stackaddr$makeclosure47062, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40986, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40986, %struct.ScmObj* %Ycmb40102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40986, %struct.ScmObj* %_37foldr140123, i64 2)
%args46893$Ycmb40102$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47064 = alloca %struct.ScmObj*, align 8
%args46893$Ycmb40102$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40249, %struct.ScmObj* %args46893$Ycmb40102$0)
store volatile %struct.ScmObj* %args46893$Ycmb40102$1, %struct.ScmObj** %stackaddr$prim47064, align 8
%stackaddr$prim47065 = alloca %struct.ScmObj*, align 8
%args46893$Ycmb40102$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40986, %struct.ScmObj* %args46893$Ycmb40102$1)
store volatile %struct.ScmObj* %args46893$Ycmb40102$2, %struct.ScmObj** %stackaddr$prim47065, align 8
%clofunc47066 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40102)
musttail call tailcc void %clofunc47066(%struct.ScmObj* %Ycmb40102, %struct.ScmObj* %args46893$Ycmb40102$2)
ret void
}

define tailcc void @proc_clo$ae40986(%struct.ScmObj* %env$ae40986,%struct.ScmObj* %current_45args46425) {
%stackaddr$env-ref47067 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40986, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47067
%stackaddr$env-ref47068 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40986, i64 1)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47068
%stackaddr$env-ref47069 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40986, i64 2)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47069
%stackaddr$prim47070 = alloca %struct.ScmObj*, align 8
%_95k40358 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46425)
store volatile %struct.ScmObj* %_95k40358, %struct.ScmObj** %stackaddr$prim47070, align 8
%stackaddr$prim47071 = alloca %struct.ScmObj*, align 8
%current_45args46426 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46425)
store volatile %struct.ScmObj* %current_45args46426, %struct.ScmObj** %stackaddr$prim47071, align 8
%stackaddr$prim47072 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46426)
store volatile %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$prim47072, align 8
%stackaddr$makeclosure47073 = alloca %struct.ScmObj*, align 8
%fptrToInt47074 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40988 to i64
%ae40988 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt47074)
store volatile %struct.ScmObj* %ae40988, %struct.ScmObj** %stackaddr$makeclosure47073, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40988, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40988, %struct.ScmObj* %Ycmb40102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40988, %struct.ScmObj* %_37take40115, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae40988, %struct.ScmObj* %_37foldr140123, i64 3)
%ae40989 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47075 = alloca %struct.ScmObj*, align 8
%fptrToInt47076 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40990 to i64
%ae40990 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47076)
store volatile %struct.ScmObj* %ae40990, %struct.ScmObj** %stackaddr$makeclosure47075, align 8
%args46892$ae40988$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47077 = alloca %struct.ScmObj*, align 8
%args46892$ae40988$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40990, %struct.ScmObj* %args46892$ae40988$0)
store volatile %struct.ScmObj* %args46892$ae40988$1, %struct.ScmObj** %stackaddr$prim47077, align 8
%stackaddr$prim47078 = alloca %struct.ScmObj*, align 8
%args46892$ae40988$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40989, %struct.ScmObj* %args46892$ae40988$1)
store volatile %struct.ScmObj* %args46892$ae40988$2, %struct.ScmObj** %stackaddr$prim47078, align 8
%clofunc47079 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40988)
musttail call tailcc void %clofunc47079(%struct.ScmObj* %ae40988, %struct.ScmObj* %args46892$ae40988$2)
ret void
}

define tailcc void @proc_clo$ae40988(%struct.ScmObj* %env$ae40988,%struct.ScmObj* %current_45args46428) {
%stackaddr$env-ref47080 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40988, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47080
%stackaddr$env-ref47081 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40988, i64 1)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47081
%stackaddr$env-ref47082 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40988, i64 2)
store %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$env-ref47082
%stackaddr$env-ref47083 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40988, i64 3)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47083
%stackaddr$prim47084 = alloca %struct.ScmObj*, align 8
%_95k40359 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46428)
store volatile %struct.ScmObj* %_95k40359, %struct.ScmObj** %stackaddr$prim47084, align 8
%stackaddr$prim47085 = alloca %struct.ScmObj*, align 8
%current_45args46429 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46428)
store volatile %struct.ScmObj* %current_45args46429, %struct.ScmObj** %stackaddr$prim47085, align 8
%stackaddr$prim47086 = alloca %struct.ScmObj*, align 8
%anf_45bind40253 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46429)
store volatile %struct.ScmObj* %anf_45bind40253, %struct.ScmObj** %stackaddr$prim47086, align 8
%stackaddr$makeclosure47087 = alloca %struct.ScmObj*, align 8
%fptrToInt47088 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41069 to i64
%ae41069 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt47088)
store volatile %struct.ScmObj* %ae41069, %struct.ScmObj** %stackaddr$makeclosure47087, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41069, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41069, %struct.ScmObj* %Ycmb40102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41069, %struct.ScmObj* %_37take40115, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41069, %struct.ScmObj* %_37foldr140123, i64 3)
%args46878$Ycmb40102$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47089 = alloca %struct.ScmObj*, align 8
%args46878$Ycmb40102$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40253, %struct.ScmObj* %args46878$Ycmb40102$0)
store volatile %struct.ScmObj* %args46878$Ycmb40102$1, %struct.ScmObj** %stackaddr$prim47089, align 8
%stackaddr$prim47090 = alloca %struct.ScmObj*, align 8
%args46878$Ycmb40102$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41069, %struct.ScmObj* %args46878$Ycmb40102$1)
store volatile %struct.ScmObj* %args46878$Ycmb40102$2, %struct.ScmObj** %stackaddr$prim47090, align 8
%clofunc47091 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40102)
musttail call tailcc void %clofunc47091(%struct.ScmObj* %Ycmb40102, %struct.ScmObj* %args46878$Ycmb40102$2)
ret void
}

define tailcc void @proc_clo$ae41069(%struct.ScmObj* %env$ae41069,%struct.ScmObj* %current_45args46431) {
%stackaddr$env-ref47092 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41069, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47092
%stackaddr$env-ref47093 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41069, i64 1)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47093
%stackaddr$env-ref47094 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41069, i64 2)
store %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$env-ref47094
%stackaddr$env-ref47095 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41069, i64 3)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47095
%stackaddr$prim47096 = alloca %struct.ScmObj*, align 8
%_95k40360 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46431)
store volatile %struct.ScmObj* %_95k40360, %struct.ScmObj** %stackaddr$prim47096, align 8
%stackaddr$prim47097 = alloca %struct.ScmObj*, align 8
%current_45args46432 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46431)
store volatile %struct.ScmObj* %current_45args46432, %struct.ScmObj** %stackaddr$prim47097, align 8
%stackaddr$prim47098 = alloca %struct.ScmObj*, align 8
%_37length40112 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46432)
store volatile %struct.ScmObj* %_37length40112, %struct.ScmObj** %stackaddr$prim47098, align 8
%stackaddr$makeclosure47099 = alloca %struct.ScmObj*, align 8
%fptrToInt47100 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41071 to i64
%ae41071 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt47100)
store volatile %struct.ScmObj* %ae41071, %struct.ScmObj** %stackaddr$makeclosure47099, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41071, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41071, %struct.ScmObj* %Ycmb40102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41071, %struct.ScmObj* %_37take40115, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41071, %struct.ScmObj* %_37length40112, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41071, %struct.ScmObj* %_37foldr140123, i64 4)
%ae41072 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47101 = alloca %struct.ScmObj*, align 8
%fptrToInt47102 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41073 to i64
%ae41073 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47102)
store volatile %struct.ScmObj* %ae41073, %struct.ScmObj** %stackaddr$makeclosure47101, align 8
%args46877$ae41071$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47103 = alloca %struct.ScmObj*, align 8
%args46877$ae41071$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41073, %struct.ScmObj* %args46877$ae41071$0)
store volatile %struct.ScmObj* %args46877$ae41071$1, %struct.ScmObj** %stackaddr$prim47103, align 8
%stackaddr$prim47104 = alloca %struct.ScmObj*, align 8
%args46877$ae41071$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41072, %struct.ScmObj* %args46877$ae41071$1)
store volatile %struct.ScmObj* %args46877$ae41071$2, %struct.ScmObj** %stackaddr$prim47104, align 8
%clofunc47105 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41071)
musttail call tailcc void %clofunc47105(%struct.ScmObj* %ae41071, %struct.ScmObj* %args46877$ae41071$2)
ret void
}

define tailcc void @proc_clo$ae41071(%struct.ScmObj* %env$ae41071,%struct.ScmObj* %current_45args46434) {
%stackaddr$env-ref47106 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41071, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47106
%stackaddr$env-ref47107 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41071, i64 1)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47107
%stackaddr$env-ref47108 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41071, i64 2)
store %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$env-ref47108
%stackaddr$env-ref47109 = alloca %struct.ScmObj*, align 8
%_37length40112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41071, i64 3)
store %struct.ScmObj* %_37length40112, %struct.ScmObj** %stackaddr$env-ref47109
%stackaddr$env-ref47110 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41071, i64 4)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47110
%stackaddr$prim47111 = alloca %struct.ScmObj*, align 8
%_95k40361 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46434)
store volatile %struct.ScmObj* %_95k40361, %struct.ScmObj** %stackaddr$prim47111, align 8
%stackaddr$prim47112 = alloca %struct.ScmObj*, align 8
%current_45args46435 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46434)
store volatile %struct.ScmObj* %current_45args46435, %struct.ScmObj** %stackaddr$prim47112, align 8
%stackaddr$prim47113 = alloca %struct.ScmObj*, align 8
%anf_45bind40258 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46435)
store volatile %struct.ScmObj* %anf_45bind40258, %struct.ScmObj** %stackaddr$prim47113, align 8
%stackaddr$makeclosure47114 = alloca %struct.ScmObj*, align 8
%fptrToInt47115 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41148 to i64
%ae41148 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt47115)
store volatile %struct.ScmObj* %ae41148, %struct.ScmObj** %stackaddr$makeclosure47114, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41148, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41148, %struct.ScmObj* %Ycmb40102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41148, %struct.ScmObj* %_37take40115, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41148, %struct.ScmObj* %_37length40112, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41148, %struct.ScmObj* %_37foldr140123, i64 4)
%args46861$Ycmb40102$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47116 = alloca %struct.ScmObj*, align 8
%args46861$Ycmb40102$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40258, %struct.ScmObj* %args46861$Ycmb40102$0)
store volatile %struct.ScmObj* %args46861$Ycmb40102$1, %struct.ScmObj** %stackaddr$prim47116, align 8
%stackaddr$prim47117 = alloca %struct.ScmObj*, align 8
%args46861$Ycmb40102$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41148, %struct.ScmObj* %args46861$Ycmb40102$1)
store volatile %struct.ScmObj* %args46861$Ycmb40102$2, %struct.ScmObj** %stackaddr$prim47117, align 8
%clofunc47118 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40102)
musttail call tailcc void %clofunc47118(%struct.ScmObj* %Ycmb40102, %struct.ScmObj* %args46861$Ycmb40102$2)
ret void
}

define tailcc void @proc_clo$ae41148(%struct.ScmObj* %env$ae41148,%struct.ScmObj* %current_45args46437) {
%stackaddr$env-ref47119 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41148, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47119
%stackaddr$env-ref47120 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41148, i64 1)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47120
%stackaddr$env-ref47121 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41148, i64 2)
store %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$env-ref47121
%stackaddr$env-ref47122 = alloca %struct.ScmObj*, align 8
%_37length40112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41148, i64 3)
store %struct.ScmObj* %_37length40112, %struct.ScmObj** %stackaddr$env-ref47122
%stackaddr$env-ref47123 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41148, i64 4)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47123
%stackaddr$prim47124 = alloca %struct.ScmObj*, align 8
%_95k40362 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46437)
store volatile %struct.ScmObj* %_95k40362, %struct.ScmObj** %stackaddr$prim47124, align 8
%stackaddr$prim47125 = alloca %struct.ScmObj*, align 8
%current_45args46438 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46437)
store volatile %struct.ScmObj* %current_45args46438, %struct.ScmObj** %stackaddr$prim47125, align 8
%stackaddr$prim47126 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46438)
store volatile %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$prim47126, align 8
%stackaddr$makeclosure47127 = alloca %struct.ScmObj*, align 8
%fptrToInt47128 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41150 to i64
%ae41150 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt47128)
store volatile %struct.ScmObj* %ae41150, %struct.ScmObj** %stackaddr$makeclosure47127, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41150, %struct.ScmObj* %_37foldr140123, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41150, %struct.ScmObj* %_37foldl140107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41150, %struct.ScmObj* %_37map140119, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41150, %struct.ScmObj* %Ycmb40102, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41150, %struct.ScmObj* %_37take40115, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41150, %struct.ScmObj* %_37length40112, i64 5)
%ae41151 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47129 = alloca %struct.ScmObj*, align 8
%fptrToInt47130 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41152 to i64
%ae41152 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47130)
store volatile %struct.ScmObj* %ae41152, %struct.ScmObj** %stackaddr$makeclosure47129, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41152, %struct.ScmObj* %_37foldl140107, i64 0)
%args46860$ae41150$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47131 = alloca %struct.ScmObj*, align 8
%args46860$ae41150$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41152, %struct.ScmObj* %args46860$ae41150$0)
store volatile %struct.ScmObj* %args46860$ae41150$1, %struct.ScmObj** %stackaddr$prim47131, align 8
%stackaddr$prim47132 = alloca %struct.ScmObj*, align 8
%args46860$ae41150$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41151, %struct.ScmObj* %args46860$ae41150$1)
store volatile %struct.ScmObj* %args46860$ae41150$2, %struct.ScmObj** %stackaddr$prim47132, align 8
%clofunc47133 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41150)
musttail call tailcc void %clofunc47133(%struct.ScmObj* %ae41150, %struct.ScmObj* %args46860$ae41150$2)
ret void
}

define tailcc void @proc_clo$ae41150(%struct.ScmObj* %env$ae41150,%struct.ScmObj* %current_45args46440) {
%stackaddr$env-ref47134 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41150, i64 0)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47134
%stackaddr$env-ref47135 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41150, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47135
%stackaddr$env-ref47136 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41150, i64 2)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47136
%stackaddr$env-ref47137 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41150, i64 3)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47137
%stackaddr$env-ref47138 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41150, i64 4)
store %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$env-ref47138
%stackaddr$env-ref47139 = alloca %struct.ScmObj*, align 8
%_37length40112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41150, i64 5)
store %struct.ScmObj* %_37length40112, %struct.ScmObj** %stackaddr$env-ref47139
%stackaddr$prim47140 = alloca %struct.ScmObj*, align 8
%_95k40363 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46440)
store volatile %struct.ScmObj* %_95k40363, %struct.ScmObj** %stackaddr$prim47140, align 8
%stackaddr$prim47141 = alloca %struct.ScmObj*, align 8
%current_45args46441 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46440)
store volatile %struct.ScmObj* %current_45args46441, %struct.ScmObj** %stackaddr$prim47141, align 8
%stackaddr$prim47142 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46441)
store volatile %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$prim47142, align 8
%stackaddr$makeclosure47143 = alloca %struct.ScmObj*, align 8
%fptrToInt47144 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41204 to i64
%ae41204 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt47144)
store volatile %struct.ScmObj* %ae41204, %struct.ScmObj** %stackaddr$makeclosure47143, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41204, %struct.ScmObj* %_37foldr140123, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41204, %struct.ScmObj* %_37foldl140107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41204, %struct.ScmObj* %_37map140119, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41204, %struct.ScmObj* %Ycmb40102, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41204, %struct.ScmObj* %_37last40145, i64 4)
%ae41205 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47145 = alloca %struct.ScmObj*, align 8
%fptrToInt47146 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41206 to i64
%ae41206 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47146)
store volatile %struct.ScmObj* %ae41206, %struct.ScmObj** %stackaddr$makeclosure47145, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41206, %struct.ScmObj* %_37take40115, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41206, %struct.ScmObj* %_37length40112, i64 1)
%args46846$ae41204$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47147 = alloca %struct.ScmObj*, align 8
%args46846$ae41204$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41206, %struct.ScmObj* %args46846$ae41204$0)
store volatile %struct.ScmObj* %args46846$ae41204$1, %struct.ScmObj** %stackaddr$prim47147, align 8
%stackaddr$prim47148 = alloca %struct.ScmObj*, align 8
%args46846$ae41204$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41205, %struct.ScmObj* %args46846$ae41204$1)
store volatile %struct.ScmObj* %args46846$ae41204$2, %struct.ScmObj** %stackaddr$prim47148, align 8
%clofunc47149 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41204)
musttail call tailcc void %clofunc47149(%struct.ScmObj* %ae41204, %struct.ScmObj* %args46846$ae41204$2)
ret void
}

define tailcc void @proc_clo$ae41204(%struct.ScmObj* %env$ae41204,%struct.ScmObj* %current_45args46443) {
%stackaddr$env-ref47150 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41204, i64 0)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47150
%stackaddr$env-ref47151 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41204, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47151
%stackaddr$env-ref47152 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41204, i64 2)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47152
%stackaddr$env-ref47153 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41204, i64 3)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47153
%stackaddr$env-ref47154 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41204, i64 4)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref47154
%stackaddr$prim47155 = alloca %struct.ScmObj*, align 8
%_95k40364 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46443)
store volatile %struct.ScmObj* %_95k40364, %struct.ScmObj** %stackaddr$prim47155, align 8
%stackaddr$prim47156 = alloca %struct.ScmObj*, align 8
%current_45args46444 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46443)
store volatile %struct.ScmObj* %current_45args46444, %struct.ScmObj** %stackaddr$prim47156, align 8
%stackaddr$prim47157 = alloca %struct.ScmObj*, align 8
%_37drop_45right40142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46444)
store volatile %struct.ScmObj* %_37drop_45right40142, %struct.ScmObj** %stackaddr$prim47157, align 8
%stackaddr$makeclosure47158 = alloca %struct.ScmObj*, align 8
%fptrToInt47159 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41234 to i64
%ae41234 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt47159)
store volatile %struct.ScmObj* %ae41234, %struct.ScmObj** %stackaddr$makeclosure47158, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41234, %struct.ScmObj* %_37foldr140123, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41234, %struct.ScmObj* %_37foldl140107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41234, %struct.ScmObj* %Ycmb40102, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41234, %struct.ScmObj* %_37last40145, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41234, %struct.ScmObj* %_37drop_45right40142, i64 4)
%ae41235 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47160 = alloca %struct.ScmObj*, align 8
%fptrToInt47161 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41236 to i64
%ae41236 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47161)
store volatile %struct.ScmObj* %ae41236, %struct.ScmObj** %stackaddr$makeclosure47160, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41236, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41236, %struct.ScmObj* %_37foldr140123, i64 1)
%args46836$ae41234$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47162 = alloca %struct.ScmObj*, align 8
%args46836$ae41234$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41236, %struct.ScmObj* %args46836$ae41234$0)
store volatile %struct.ScmObj* %args46836$ae41234$1, %struct.ScmObj** %stackaddr$prim47162, align 8
%stackaddr$prim47163 = alloca %struct.ScmObj*, align 8
%args46836$ae41234$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41235, %struct.ScmObj* %args46836$ae41234$1)
store volatile %struct.ScmObj* %args46836$ae41234$2, %struct.ScmObj** %stackaddr$prim47163, align 8
%clofunc47164 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41234)
musttail call tailcc void %clofunc47164(%struct.ScmObj* %ae41234, %struct.ScmObj* %args46836$ae41234$2)
ret void
}

define tailcc void @proc_clo$ae41234(%struct.ScmObj* %env$ae41234,%struct.ScmObj* %current_45args46446) {
%stackaddr$env-ref47165 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41234, i64 0)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47165
%stackaddr$env-ref47166 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41234, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47166
%stackaddr$env-ref47167 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41234, i64 2)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47167
%stackaddr$env-ref47168 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41234, i64 3)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref47168
%stackaddr$env-ref47169 = alloca %struct.ScmObj*, align 8
%_37drop_45right40142 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41234, i64 4)
store %struct.ScmObj* %_37drop_45right40142, %struct.ScmObj** %stackaddr$env-ref47169
%stackaddr$prim47170 = alloca %struct.ScmObj*, align 8
%_95k40365 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46446)
store volatile %struct.ScmObj* %_95k40365, %struct.ScmObj** %stackaddr$prim47170, align 8
%stackaddr$prim47171 = alloca %struct.ScmObj*, align 8
%current_45args46447 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46446)
store volatile %struct.ScmObj* %current_45args46447, %struct.ScmObj** %stackaddr$prim47171, align 8
%stackaddr$prim47172 = alloca %struct.ScmObj*, align 8
%anf_45bind40274 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46447)
store volatile %struct.ScmObj* %anf_45bind40274, %struct.ScmObj** %stackaddr$prim47172, align 8
%stackaddr$makeclosure47173 = alloca %struct.ScmObj*, align 8
%fptrToInt47174 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41618 to i64
%ae41618 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt47174)
store volatile %struct.ScmObj* %ae41618, %struct.ScmObj** %stackaddr$makeclosure47173, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41618, %struct.ScmObj* %_37foldr140123, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41618, %struct.ScmObj* %_37foldl140107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41618, %struct.ScmObj* %Ycmb40102, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41618, %struct.ScmObj* %_37last40145, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41618, %struct.ScmObj* %_37drop_45right40142, i64 4)
%args46776$Ycmb40102$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47175 = alloca %struct.ScmObj*, align 8
%args46776$Ycmb40102$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40274, %struct.ScmObj* %args46776$Ycmb40102$0)
store volatile %struct.ScmObj* %args46776$Ycmb40102$1, %struct.ScmObj** %stackaddr$prim47175, align 8
%stackaddr$prim47176 = alloca %struct.ScmObj*, align 8
%args46776$Ycmb40102$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41618, %struct.ScmObj* %args46776$Ycmb40102$1)
store volatile %struct.ScmObj* %args46776$Ycmb40102$2, %struct.ScmObj** %stackaddr$prim47176, align 8
%clofunc47177 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40102)
musttail call tailcc void %clofunc47177(%struct.ScmObj* %Ycmb40102, %struct.ScmObj* %args46776$Ycmb40102$2)
ret void
}

define tailcc void @proc_clo$ae41618(%struct.ScmObj* %env$ae41618,%struct.ScmObj* %current_45args46449) {
%stackaddr$env-ref47178 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41618, i64 0)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47178
%stackaddr$env-ref47179 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41618, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47179
%stackaddr$env-ref47180 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41618, i64 2)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47180
%stackaddr$env-ref47181 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41618, i64 3)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref47181
%stackaddr$env-ref47182 = alloca %struct.ScmObj*, align 8
%_37drop_45right40142 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41618, i64 4)
store %struct.ScmObj* %_37drop_45right40142, %struct.ScmObj** %stackaddr$env-ref47182
%stackaddr$prim47183 = alloca %struct.ScmObj*, align 8
%_95k40366 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46449)
store volatile %struct.ScmObj* %_95k40366, %struct.ScmObj** %stackaddr$prim47183, align 8
%stackaddr$prim47184 = alloca %struct.ScmObj*, align 8
%current_45args46450 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46449)
store volatile %struct.ScmObj* %current_45args46450, %struct.ScmObj** %stackaddr$prim47184, align 8
%stackaddr$prim47185 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46450)
store volatile %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$prim47185, align 8
%stackaddr$makeclosure47186 = alloca %struct.ScmObj*, align 8
%fptrToInt47187 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41620 to i64
%ae41620 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt47187)
store volatile %struct.ScmObj* %ae41620, %struct.ScmObj** %stackaddr$makeclosure47186, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41620, %struct.ScmObj* %_37foldr140123, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41620, %struct.ScmObj* %_37foldl140107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41620, %struct.ScmObj* %Ycmb40102, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41620, %struct.ScmObj* %_37last40145, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41620, %struct.ScmObj* %_37foldr40128, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41620, %struct.ScmObj* %_37drop_45right40142, i64 5)
%ae41621 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47188 = alloca %struct.ScmObj*, align 8
%fptrToInt47189 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41622 to i64
%ae41622 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47189)
store volatile %struct.ScmObj* %ae41622, %struct.ScmObj** %stackaddr$makeclosure47188, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41622, %struct.ScmObj* %_37foldr140123, i64 0)
%args46775$ae41620$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47190 = alloca %struct.ScmObj*, align 8
%args46775$ae41620$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41622, %struct.ScmObj* %args46775$ae41620$0)
store volatile %struct.ScmObj* %args46775$ae41620$1, %struct.ScmObj** %stackaddr$prim47190, align 8
%stackaddr$prim47191 = alloca %struct.ScmObj*, align 8
%args46775$ae41620$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41621, %struct.ScmObj* %args46775$ae41620$1)
store volatile %struct.ScmObj* %args46775$ae41620$2, %struct.ScmObj** %stackaddr$prim47191, align 8
%clofunc47192 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41620)
musttail call tailcc void %clofunc47192(%struct.ScmObj* %ae41620, %struct.ScmObj* %args46775$ae41620$2)
ret void
}

define tailcc void @proc_clo$ae41620(%struct.ScmObj* %env$ae41620,%struct.ScmObj* %current_45args46452) {
%stackaddr$env-ref47193 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41620, i64 0)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47193
%stackaddr$env-ref47194 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41620, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47194
%stackaddr$env-ref47195 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41620, i64 2)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47195
%stackaddr$env-ref47196 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41620, i64 3)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref47196
%stackaddr$env-ref47197 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41620, i64 4)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref47197
%stackaddr$env-ref47198 = alloca %struct.ScmObj*, align 8
%_37drop_45right40142 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41620, i64 5)
store %struct.ScmObj* %_37drop_45right40142, %struct.ScmObj** %stackaddr$env-ref47198
%stackaddr$prim47199 = alloca %struct.ScmObj*, align 8
%_95k40367 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46452)
store volatile %struct.ScmObj* %_95k40367, %struct.ScmObj** %stackaddr$prim47199, align 8
%stackaddr$prim47200 = alloca %struct.ScmObj*, align 8
%current_45args46453 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46452)
store volatile %struct.ScmObj* %current_45args46453, %struct.ScmObj** %stackaddr$prim47200, align 8
%stackaddr$prim47201 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46453)
store volatile %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$prim47201, align 8
%stackaddr$makeclosure47202 = alloca %struct.ScmObj*, align 8
%fptrToInt47203 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41697 to i64
%ae41697 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt47203)
store volatile %struct.ScmObj* %ae41697, %struct.ScmObj** %stackaddr$makeclosure47202, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41697, %struct.ScmObj* %_37foldr140123, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41697, %struct.ScmObj* %_37foldl140107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41697, %struct.ScmObj* %Ycmb40102, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41697, %struct.ScmObj* %_37foldr40128, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41697, %struct.ScmObj* %_37map140154, i64 4)
%ae41698 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47204 = alloca %struct.ScmObj*, align 8
%fptrToInt47205 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41699 to i64
%ae41699 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47205)
store volatile %struct.ScmObj* %ae41699, %struct.ScmObj** %stackaddr$makeclosure47204, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41699, %struct.ScmObj* %_37last40145, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41699, %struct.ScmObj* %_37foldr40128, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41699, %struct.ScmObj* %_37drop_45right40142, i64 2)
%args46756$ae41697$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47206 = alloca %struct.ScmObj*, align 8
%args46756$ae41697$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41699, %struct.ScmObj* %args46756$ae41697$0)
store volatile %struct.ScmObj* %args46756$ae41697$1, %struct.ScmObj** %stackaddr$prim47206, align 8
%stackaddr$prim47207 = alloca %struct.ScmObj*, align 8
%args46756$ae41697$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41698, %struct.ScmObj* %args46756$ae41697$1)
store volatile %struct.ScmObj* %args46756$ae41697$2, %struct.ScmObj** %stackaddr$prim47207, align 8
%clofunc47208 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41697)
musttail call tailcc void %clofunc47208(%struct.ScmObj* %ae41697, %struct.ScmObj* %args46756$ae41697$2)
ret void
}

define tailcc void @proc_clo$ae41697(%struct.ScmObj* %env$ae41697,%struct.ScmObj* %current_45args46455) {
%stackaddr$env-ref47209 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41697, i64 0)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47209
%stackaddr$env-ref47210 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41697, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47210
%stackaddr$env-ref47211 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41697, i64 2)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47211
%stackaddr$env-ref47212 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41697, i64 3)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref47212
%stackaddr$env-ref47213 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41697, i64 4)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47213
%stackaddr$prim47214 = alloca %struct.ScmObj*, align 8
%_95k40368 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46455)
store volatile %struct.ScmObj* %_95k40368, %struct.ScmObj** %stackaddr$prim47214, align 8
%stackaddr$prim47215 = alloca %struct.ScmObj*, align 8
%current_45args46456 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46455)
store volatile %struct.ScmObj* %current_45args46456, %struct.ScmObj** %stackaddr$prim47215, align 8
%stackaddr$prim47216 = alloca %struct.ScmObj*, align 8
%_37map40149 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46456)
store volatile %struct.ScmObj* %_37map40149, %struct.ScmObj** %stackaddr$prim47216, align 8
%stackaddr$makeclosure47217 = alloca %struct.ScmObj*, align 8
%fptrToInt47218 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41843 to i64
%ae41843 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47218)
store volatile %struct.ScmObj* %ae41843, %struct.ScmObj** %stackaddr$makeclosure47217, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41843, %struct.ScmObj* %Ycmb40102, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41843, %struct.ScmObj* %_37map40149, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41843, %struct.ScmObj* %_37foldl140107, i64 2)
%ae41844 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47219 = alloca %struct.ScmObj*, align 8
%fptrToInt47220 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41845 to i64
%ae41845 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47220)
store volatile %struct.ScmObj* %ae41845, %struct.ScmObj** %stackaddr$makeclosure47219, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41845, %struct.ScmObj* %_37foldr40128, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41845, %struct.ScmObj* %_37foldr140123, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41845, %struct.ScmObj* %_37map140154, i64 2)
%args46739$ae41843$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47221 = alloca %struct.ScmObj*, align 8
%args46739$ae41843$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41845, %struct.ScmObj* %args46739$ae41843$0)
store volatile %struct.ScmObj* %args46739$ae41843$1, %struct.ScmObj** %stackaddr$prim47221, align 8
%stackaddr$prim47222 = alloca %struct.ScmObj*, align 8
%args46739$ae41843$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41844, %struct.ScmObj* %args46739$ae41843$1)
store volatile %struct.ScmObj* %args46739$ae41843$2, %struct.ScmObj** %stackaddr$prim47222, align 8
%clofunc47223 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41843)
musttail call tailcc void %clofunc47223(%struct.ScmObj* %ae41843, %struct.ScmObj* %args46739$ae41843$2)
ret void
}

define tailcc void @proc_clo$ae41843(%struct.ScmObj* %env$ae41843,%struct.ScmObj* %current_45args46458) {
%stackaddr$env-ref47224 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41843, i64 0)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47224
%stackaddr$env-ref47225 = alloca %struct.ScmObj*, align 8
%_37map40149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41843, i64 1)
store %struct.ScmObj* %_37map40149, %struct.ScmObj** %stackaddr$env-ref47225
%stackaddr$env-ref47226 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41843, i64 2)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47226
%stackaddr$prim47227 = alloca %struct.ScmObj*, align 8
%_95k40369 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46458)
store volatile %struct.ScmObj* %_95k40369, %struct.ScmObj** %stackaddr$prim47227, align 8
%stackaddr$prim47228 = alloca %struct.ScmObj*, align 8
%current_45args46459 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46458)
store volatile %struct.ScmObj* %current_45args46459, %struct.ScmObj** %stackaddr$prim47228, align 8
%stackaddr$prim47229 = alloca %struct.ScmObj*, align 8
%anf_45bind40294 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46459)
store volatile %struct.ScmObj* %anf_45bind40294, %struct.ScmObj** %stackaddr$prim47229, align 8
%stackaddr$makeclosure47230 = alloca %struct.ScmObj*, align 8
%fptrToInt47231 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42235 to i64
%ae42235 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47231)
store volatile %struct.ScmObj* %ae42235, %struct.ScmObj** %stackaddr$makeclosure47230, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42235, %struct.ScmObj* %_37map40149, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42235, %struct.ScmObj* %_37foldl140107, i64 1)
%args46679$Ycmb40102$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47232 = alloca %struct.ScmObj*, align 8
%args46679$Ycmb40102$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40294, %struct.ScmObj* %args46679$Ycmb40102$0)
store volatile %struct.ScmObj* %args46679$Ycmb40102$1, %struct.ScmObj** %stackaddr$prim47232, align 8
%stackaddr$prim47233 = alloca %struct.ScmObj*, align 8
%args46679$Ycmb40102$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42235, %struct.ScmObj* %args46679$Ycmb40102$1)
store volatile %struct.ScmObj* %args46679$Ycmb40102$2, %struct.ScmObj** %stackaddr$prim47233, align 8
%clofunc47234 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40102)
musttail call tailcc void %clofunc47234(%struct.ScmObj* %Ycmb40102, %struct.ScmObj* %args46679$Ycmb40102$2)
ret void
}

define tailcc void @proc_clo$ae42235(%struct.ScmObj* %env$ae42235,%struct.ScmObj* %current_45args46461) {
%stackaddr$env-ref47235 = alloca %struct.ScmObj*, align 8
%_37map40149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42235, i64 0)
store %struct.ScmObj* %_37map40149, %struct.ScmObj** %stackaddr$env-ref47235
%stackaddr$env-ref47236 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42235, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47236
%stackaddr$prim47237 = alloca %struct.ScmObj*, align 8
%_95k40370 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46461)
store volatile %struct.ScmObj* %_95k40370, %struct.ScmObj** %stackaddr$prim47237, align 8
%stackaddr$prim47238 = alloca %struct.ScmObj*, align 8
%current_45args46462 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46461)
store volatile %struct.ScmObj* %current_45args46462, %struct.ScmObj** %stackaddr$prim47238, align 8
%stackaddr$prim47239 = alloca %struct.ScmObj*, align 8
%_37foldl40205 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46462)
store volatile %struct.ScmObj* %_37foldl40205, %struct.ScmObj** %stackaddr$prim47239, align 8
%stackaddr$makeclosure47240 = alloca %struct.ScmObj*, align 8
%fptrToInt47241 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42237 to i64
%ae42237 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47241)
store volatile %struct.ScmObj* %ae42237, %struct.ScmObj** %stackaddr$makeclosure47240, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42237, %struct.ScmObj* %_37map40149, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42237, %struct.ScmObj* %_37foldl140107, i64 1)
%ae42238 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47242 = alloca %struct.ScmObj*, align 8
%fptrToInt47243 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42239 to i64
%ae42239 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47243)
store volatile %struct.ScmObj* %ae42239, %struct.ScmObj** %stackaddr$makeclosure47242, align 8
%args46678$ae42237$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47244 = alloca %struct.ScmObj*, align 8
%args46678$ae42237$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42239, %struct.ScmObj* %args46678$ae42237$0)
store volatile %struct.ScmObj* %args46678$ae42237$1, %struct.ScmObj** %stackaddr$prim47244, align 8
%stackaddr$prim47245 = alloca %struct.ScmObj*, align 8
%args46678$ae42237$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42238, %struct.ScmObj* %args46678$ae42237$1)
store volatile %struct.ScmObj* %args46678$ae42237$2, %struct.ScmObj** %stackaddr$prim47245, align 8
%clofunc47246 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42237)
musttail call tailcc void %clofunc47246(%struct.ScmObj* %ae42237, %struct.ScmObj* %args46678$ae42237$2)
ret void
}

define tailcc void @proc_clo$ae42237(%struct.ScmObj* %env$ae42237,%struct.ScmObj* %current_45args46464) {
%stackaddr$env-ref47247 = alloca %struct.ScmObj*, align 8
%_37map40149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42237, i64 0)
store %struct.ScmObj* %_37map40149, %struct.ScmObj** %stackaddr$env-ref47247
%stackaddr$env-ref47248 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42237, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47248
%stackaddr$prim47249 = alloca %struct.ScmObj*, align 8
%_95k40371 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46464)
store volatile %struct.ScmObj* %_95k40371, %struct.ScmObj** %stackaddr$prim47249, align 8
%stackaddr$prim47250 = alloca %struct.ScmObj*, align 8
%current_45args46465 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46464)
store volatile %struct.ScmObj* %current_45args46465, %struct.ScmObj** %stackaddr$prim47250, align 8
%stackaddr$prim47251 = alloca %struct.ScmObj*, align 8
%_37_6240202 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46465)
store volatile %struct.ScmObj* %_37_6240202, %struct.ScmObj** %stackaddr$prim47251, align 8
%stackaddr$makeclosure47252 = alloca %struct.ScmObj*, align 8
%fptrToInt47253 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42261 to i64
%ae42261 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47253)
store volatile %struct.ScmObj* %ae42261, %struct.ScmObj** %stackaddr$makeclosure47252, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42261, %struct.ScmObj* %_37map40149, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42261, %struct.ScmObj* %_37foldl140107, i64 1)
%ae42262 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47254 = alloca %struct.ScmObj*, align 8
%fptrToInt47255 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42263 to i64
%ae42263 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47255)
store volatile %struct.ScmObj* %ae42263, %struct.ScmObj** %stackaddr$makeclosure47254, align 8
%args46672$ae42261$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47256 = alloca %struct.ScmObj*, align 8
%args46672$ae42261$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42263, %struct.ScmObj* %args46672$ae42261$0)
store volatile %struct.ScmObj* %args46672$ae42261$1, %struct.ScmObj** %stackaddr$prim47256, align 8
%stackaddr$prim47257 = alloca %struct.ScmObj*, align 8
%args46672$ae42261$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42262, %struct.ScmObj* %args46672$ae42261$1)
store volatile %struct.ScmObj* %args46672$ae42261$2, %struct.ScmObj** %stackaddr$prim47257, align 8
%clofunc47258 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42261)
musttail call tailcc void %clofunc47258(%struct.ScmObj* %ae42261, %struct.ScmObj* %args46672$ae42261$2)
ret void
}

define tailcc void @proc_clo$ae42261(%struct.ScmObj* %env$ae42261,%struct.ScmObj* %current_45args46467) {
%stackaddr$env-ref47259 = alloca %struct.ScmObj*, align 8
%_37map40149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42261, i64 0)
store %struct.ScmObj* %_37map40149, %struct.ScmObj** %stackaddr$env-ref47259
%stackaddr$env-ref47260 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42261, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47260
%stackaddr$prim47261 = alloca %struct.ScmObj*, align 8
%_95k40372 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46467)
store volatile %struct.ScmObj* %_95k40372, %struct.ScmObj** %stackaddr$prim47261, align 8
%stackaddr$prim47262 = alloca %struct.ScmObj*, align 8
%current_45args46468 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46467)
store volatile %struct.ScmObj* %current_45args46468, %struct.ScmObj** %stackaddr$prim47262, align 8
%stackaddr$prim47263 = alloca %struct.ScmObj*, align 8
%_37_62_6140199 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46468)
store volatile %struct.ScmObj* %_37_62_6140199, %struct.ScmObj** %stackaddr$prim47263, align 8
%ae42285 = call %struct.ScmObj* @const_init_int(i64 1)
%ae42286 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47264 = alloca %struct.ScmObj*, align 8
%_37append40195 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42285, %struct.ScmObj* %ae42286)
store volatile %struct.ScmObj* %_37append40195, %struct.ScmObj** %stackaddr$prim47264, align 8
%stackaddr$makeclosure47265 = alloca %struct.ScmObj*, align 8
%fptrToInt47266 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42287 to i64
%ae42287 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47266)
store volatile %struct.ScmObj* %ae42287, %struct.ScmObj** %stackaddr$makeclosure47265, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42287, %struct.ScmObj* %_37map40149, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42287, %struct.ScmObj* %_37append40195, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42287, %struct.ScmObj* %_37foldl140107, i64 2)
%ae42288 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47267 = alloca %struct.ScmObj*, align 8
%fptrToInt47268 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42289 to i64
%ae42289 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47268)
store volatile %struct.ScmObj* %ae42289, %struct.ScmObj** %stackaddr$makeclosure47267, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42289, %struct.ScmObj* %_37append40195, i64 0)
%args46666$ae42287$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47269 = alloca %struct.ScmObj*, align 8
%args46666$ae42287$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42289, %struct.ScmObj* %args46666$ae42287$0)
store volatile %struct.ScmObj* %args46666$ae42287$1, %struct.ScmObj** %stackaddr$prim47269, align 8
%stackaddr$prim47270 = alloca %struct.ScmObj*, align 8
%args46666$ae42287$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42288, %struct.ScmObj* %args46666$ae42287$1)
store volatile %struct.ScmObj* %args46666$ae42287$2, %struct.ScmObj** %stackaddr$prim47270, align 8
%clofunc47271 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42287)
musttail call tailcc void %clofunc47271(%struct.ScmObj* %ae42287, %struct.ScmObj* %args46666$ae42287$2)
ret void
}

define tailcc void @proc_clo$ae42287(%struct.ScmObj* %env$ae42287,%struct.ScmObj* %current_45args46470) {
%stackaddr$env-ref47272 = alloca %struct.ScmObj*, align 8
%_37map40149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42287, i64 0)
store %struct.ScmObj* %_37map40149, %struct.ScmObj** %stackaddr$env-ref47272
%stackaddr$env-ref47273 = alloca %struct.ScmObj*, align 8
%_37append40195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42287, i64 1)
store %struct.ScmObj* %_37append40195, %struct.ScmObj** %stackaddr$env-ref47273
%stackaddr$env-ref47274 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42287, i64 2)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47274
%stackaddr$prim47275 = alloca %struct.ScmObj*, align 8
%_95k40373 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46470)
store volatile %struct.ScmObj* %_95k40373, %struct.ScmObj** %stackaddr$prim47275, align 8
%stackaddr$prim47276 = alloca %struct.ScmObj*, align 8
%current_45args46471 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46470)
store volatile %struct.ScmObj* %current_45args46471, %struct.ScmObj** %stackaddr$prim47276, align 8
%stackaddr$prim47277 = alloca %struct.ScmObj*, align 8
%anf_45bind40302 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46471)
store volatile %struct.ScmObj* %anf_45bind40302, %struct.ScmObj** %stackaddr$prim47277, align 8
%ae42355 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47278 = alloca %struct.ScmObj*, align 8
%_95040196 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append40195, %struct.ScmObj* %ae42355, %struct.ScmObj* %anf_45bind40302)
store volatile %struct.ScmObj* %_95040196, %struct.ScmObj** %stackaddr$prim47278, align 8
%ae42358 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47279 = alloca %struct.ScmObj*, align 8
%_37append40194 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append40195, %struct.ScmObj* %ae42358)
store volatile %struct.ScmObj* %_37append40194, %struct.ScmObj** %stackaddr$prim47279, align 8
%stackaddr$makeclosure47280 = alloca %struct.ScmObj*, align 8
%fptrToInt47281 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42359 to i64
%ae42359 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47281)
store volatile %struct.ScmObj* %ae42359, %struct.ScmObj** %stackaddr$makeclosure47280, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42359, %struct.ScmObj* %_37map40149, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42359, %struct.ScmObj* %_37foldl140107, i64 1)
%ae42360 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47282 = alloca %struct.ScmObj*, align 8
%fptrToInt47283 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42361 to i64
%ae42361 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47283)
store volatile %struct.ScmObj* %ae42361, %struct.ScmObj** %stackaddr$makeclosure47282, align 8
%args46655$ae42359$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47284 = alloca %struct.ScmObj*, align 8
%args46655$ae42359$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42361, %struct.ScmObj* %args46655$ae42359$0)
store volatile %struct.ScmObj* %args46655$ae42359$1, %struct.ScmObj** %stackaddr$prim47284, align 8
%stackaddr$prim47285 = alloca %struct.ScmObj*, align 8
%args46655$ae42359$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42360, %struct.ScmObj* %args46655$ae42359$1)
store volatile %struct.ScmObj* %args46655$ae42359$2, %struct.ScmObj** %stackaddr$prim47285, align 8
%clofunc47286 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42359)
musttail call tailcc void %clofunc47286(%struct.ScmObj* %ae42359, %struct.ScmObj* %args46655$ae42359$2)
ret void
}

define tailcc void @proc_clo$ae42359(%struct.ScmObj* %env$ae42359,%struct.ScmObj* %current_45args46473) {
%stackaddr$env-ref47287 = alloca %struct.ScmObj*, align 8
%_37map40149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42359, i64 0)
store %struct.ScmObj* %_37map40149, %struct.ScmObj** %stackaddr$env-ref47287
%stackaddr$env-ref47288 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42359, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47288
%stackaddr$prim47289 = alloca %struct.ScmObj*, align 8
%_95k40374 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46473)
store volatile %struct.ScmObj* %_95k40374, %struct.ScmObj** %stackaddr$prim47289, align 8
%stackaddr$prim47290 = alloca %struct.ScmObj*, align 8
%current_45args46474 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46473)
store volatile %struct.ScmObj* %current_45args46474, %struct.ScmObj** %stackaddr$prim47290, align 8
%stackaddr$prim47291 = alloca %struct.ScmObj*, align 8
%_37list_6340187 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46474)
store volatile %struct.ScmObj* %_37list_6340187, %struct.ScmObj** %stackaddr$prim47291, align 8
%stackaddr$makeclosure47292 = alloca %struct.ScmObj*, align 8
%fptrToInt47293 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42775 to i64
%ae42775 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47293)
store volatile %struct.ScmObj* %ae42775, %struct.ScmObj** %stackaddr$makeclosure47292, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42775, %struct.ScmObj* %_37map40149, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42775, %struct.ScmObj* %_37foldl140107, i64 1)
%ae42776 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47294 = alloca %struct.ScmObj*, align 8
%fptrToInt47295 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42777 to i64
%ae42777 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47295)
store volatile %struct.ScmObj* %ae42777, %struct.ScmObj** %stackaddr$makeclosure47294, align 8
%args46630$ae42775$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47296 = alloca %struct.ScmObj*, align 8
%args46630$ae42775$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42777, %struct.ScmObj* %args46630$ae42775$0)
store volatile %struct.ScmObj* %args46630$ae42775$1, %struct.ScmObj** %stackaddr$prim47296, align 8
%stackaddr$prim47297 = alloca %struct.ScmObj*, align 8
%args46630$ae42775$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42776, %struct.ScmObj* %args46630$ae42775$1)
store volatile %struct.ScmObj* %args46630$ae42775$2, %struct.ScmObj** %stackaddr$prim47297, align 8
%clofunc47298 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42775)
musttail call tailcc void %clofunc47298(%struct.ScmObj* %ae42775, %struct.ScmObj* %args46630$ae42775$2)
ret void
}

define tailcc void @proc_clo$ae42775(%struct.ScmObj* %env$ae42775,%struct.ScmObj* %current_45args46476) {
%stackaddr$env-ref47299 = alloca %struct.ScmObj*, align 8
%_37map40149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42775, i64 0)
store %struct.ScmObj* %_37map40149, %struct.ScmObj** %stackaddr$env-ref47299
%stackaddr$env-ref47300 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42775, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47300
%stackaddr$prim47301 = alloca %struct.ScmObj*, align 8
%_95k40375 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46476)
store volatile %struct.ScmObj* %_95k40375, %struct.ScmObj** %stackaddr$prim47301, align 8
%stackaddr$prim47302 = alloca %struct.ScmObj*, align 8
%current_45args46477 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46476)
store volatile %struct.ScmObj* %current_45args46477, %struct.ScmObj** %stackaddr$prim47302, align 8
%stackaddr$prim47303 = alloca %struct.ScmObj*, align 8
%_37drop40178 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46477)
store volatile %struct.ScmObj* %_37drop40178, %struct.ScmObj** %stackaddr$prim47303, align 8
%stackaddr$makeclosure47304 = alloca %struct.ScmObj*, align 8
%fptrToInt47305 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43311 to i64
%ae43311 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47305)
store volatile %struct.ScmObj* %ae43311, %struct.ScmObj** %stackaddr$makeclosure47304, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43311, %struct.ScmObj* %_37map40149, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43311, %struct.ScmObj* %_37foldl140107, i64 1)
%ae43312 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47306 = alloca %struct.ScmObj*, align 8
%fptrToInt47307 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43313 to i64
%ae43313 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47307)
store volatile %struct.ScmObj* %ae43313, %struct.ScmObj** %stackaddr$makeclosure47306, align 8
%args46606$ae43311$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47308 = alloca %struct.ScmObj*, align 8
%args46606$ae43311$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43313, %struct.ScmObj* %args46606$ae43311$0)
store volatile %struct.ScmObj* %args46606$ae43311$1, %struct.ScmObj** %stackaddr$prim47308, align 8
%stackaddr$prim47309 = alloca %struct.ScmObj*, align 8
%args46606$ae43311$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43312, %struct.ScmObj* %args46606$ae43311$1)
store volatile %struct.ScmObj* %args46606$ae43311$2, %struct.ScmObj** %stackaddr$prim47309, align 8
%clofunc47310 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43311)
musttail call tailcc void %clofunc47310(%struct.ScmObj* %ae43311, %struct.ScmObj* %args46606$ae43311$2)
ret void
}

define tailcc void @proc_clo$ae43311(%struct.ScmObj* %env$ae43311,%struct.ScmObj* %current_45args46479) {
%stackaddr$env-ref47311 = alloca %struct.ScmObj*, align 8
%_37map40149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43311, i64 0)
store %struct.ScmObj* %_37map40149, %struct.ScmObj** %stackaddr$env-ref47311
%stackaddr$env-ref47312 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43311, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47312
%stackaddr$prim47313 = alloca %struct.ScmObj*, align 8
%_95k40376 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46479)
store volatile %struct.ScmObj* %_95k40376, %struct.ScmObj** %stackaddr$prim47313, align 8
%stackaddr$prim47314 = alloca %struct.ScmObj*, align 8
%current_45args46480 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46479)
store volatile %struct.ScmObj* %current_45args46480, %struct.ScmObj** %stackaddr$prim47314, align 8
%stackaddr$prim47315 = alloca %struct.ScmObj*, align 8
%_37memv40171 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46480)
store volatile %struct.ScmObj* %_37memv40171, %struct.ScmObj** %stackaddr$prim47315, align 8
%stackaddr$makeclosure47316 = alloca %struct.ScmObj*, align 8
%fptrToInt47317 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43715 to i64
%ae43715 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47317)
store volatile %struct.ScmObj* %ae43715, %struct.ScmObj** %stackaddr$makeclosure47316, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43715, %struct.ScmObj* %_37map40149, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43715, %struct.ScmObj* %_37foldl140107, i64 1)
%ae43716 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47318 = alloca %struct.ScmObj*, align 8
%fptrToInt47319 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43717 to i64
%ae43717 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47319)
store volatile %struct.ScmObj* %ae43717, %struct.ScmObj** %stackaddr$makeclosure47318, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43717, %struct.ScmObj* %_37foldl140107, i64 0)
%args46580$ae43715$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47320 = alloca %struct.ScmObj*, align 8
%args46580$ae43715$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43717, %struct.ScmObj* %args46580$ae43715$0)
store volatile %struct.ScmObj* %args46580$ae43715$1, %struct.ScmObj** %stackaddr$prim47320, align 8
%stackaddr$prim47321 = alloca %struct.ScmObj*, align 8
%args46580$ae43715$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43716, %struct.ScmObj* %args46580$ae43715$1)
store volatile %struct.ScmObj* %args46580$ae43715$2, %struct.ScmObj** %stackaddr$prim47321, align 8
%clofunc47322 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43715)
musttail call tailcc void %clofunc47322(%struct.ScmObj* %ae43715, %struct.ScmObj* %args46580$ae43715$2)
ret void
}

define tailcc void @proc_clo$ae43715(%struct.ScmObj* %env$ae43715,%struct.ScmObj* %current_45args46482) {
%stackaddr$env-ref47323 = alloca %struct.ScmObj*, align 8
%_37map40149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43715, i64 0)
store %struct.ScmObj* %_37map40149, %struct.ScmObj** %stackaddr$env-ref47323
%stackaddr$env-ref47324 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43715, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47324
%stackaddr$prim47325 = alloca %struct.ScmObj*, align 8
%_95k40377 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46482)
store volatile %struct.ScmObj* %_95k40377, %struct.ScmObj** %stackaddr$prim47325, align 8
%stackaddr$prim47326 = alloca %struct.ScmObj*, align 8
%current_45args46483 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46482)
store volatile %struct.ScmObj* %current_45args46483, %struct.ScmObj** %stackaddr$prim47326, align 8
%stackaddr$prim47327 = alloca %struct.ScmObj*, align 8
%_37_4740167 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46483)
store volatile %struct.ScmObj* %_37_4740167, %struct.ScmObj** %stackaddr$prim47327, align 8
%stackaddr$makeclosure47328 = alloca %struct.ScmObj*, align 8
%fptrToInt47329 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43813 to i64
%ae43813 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47329)
store volatile %struct.ScmObj* %ae43813, %struct.ScmObj** %stackaddr$makeclosure47328, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43813, %struct.ScmObj* %_37map40149, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43813, %struct.ScmObj* %_37foldl140107, i64 1)
%ae43814 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47330 = alloca %struct.ScmObj*, align 8
%fptrToInt47331 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43815 to i64
%ae43815 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47331)
store volatile %struct.ScmObj* %ae43815, %struct.ScmObj** %stackaddr$makeclosure47330, align 8
%args46567$ae43813$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47332 = alloca %struct.ScmObj*, align 8
%args46567$ae43813$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43815, %struct.ScmObj* %args46567$ae43813$0)
store volatile %struct.ScmObj* %args46567$ae43813$1, %struct.ScmObj** %stackaddr$prim47332, align 8
%stackaddr$prim47333 = alloca %struct.ScmObj*, align 8
%args46567$ae43813$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43814, %struct.ScmObj* %args46567$ae43813$1)
store volatile %struct.ScmObj* %args46567$ae43813$2, %struct.ScmObj** %stackaddr$prim47333, align 8
%clofunc47334 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43813)
musttail call tailcc void %clofunc47334(%struct.ScmObj* %ae43813, %struct.ScmObj* %args46567$ae43813$2)
ret void
}

define tailcc void @proc_clo$ae43813(%struct.ScmObj* %env$ae43813,%struct.ScmObj* %current_45args46485) {
%stackaddr$env-ref47335 = alloca %struct.ScmObj*, align 8
%_37map40149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43813, i64 0)
store %struct.ScmObj* %_37map40149, %struct.ScmObj** %stackaddr$env-ref47335
%stackaddr$env-ref47336 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43813, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47336
%stackaddr$prim47337 = alloca %struct.ScmObj*, align 8
%_95k40378 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46485)
store volatile %struct.ScmObj* %_95k40378, %struct.ScmObj** %stackaddr$prim47337, align 8
%stackaddr$prim47338 = alloca %struct.ScmObj*, align 8
%current_45args46486 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46485)
store volatile %struct.ScmObj* %current_45args46486, %struct.ScmObj** %stackaddr$prim47338, align 8
%stackaddr$prim47339 = alloca %struct.ScmObj*, align 8
%_37first40165 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46486)
store volatile %struct.ScmObj* %_37first40165, %struct.ScmObj** %stackaddr$prim47339, align 8
%stackaddr$makeclosure47340 = alloca %struct.ScmObj*, align 8
%fptrToInt47341 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43833 to i64
%ae43833 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47341)
store volatile %struct.ScmObj* %ae43833, %struct.ScmObj** %stackaddr$makeclosure47340, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43833, %struct.ScmObj* %_37map40149, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43833, %struct.ScmObj* %_37foldl140107, i64 1)
%ae43834 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47342 = alloca %struct.ScmObj*, align 8
%fptrToInt47343 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43835 to i64
%ae43835 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47343)
store volatile %struct.ScmObj* %ae43835, %struct.ScmObj** %stackaddr$makeclosure47342, align 8
%args46562$ae43833$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47344 = alloca %struct.ScmObj*, align 8
%args46562$ae43833$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43835, %struct.ScmObj* %args46562$ae43833$0)
store volatile %struct.ScmObj* %args46562$ae43833$1, %struct.ScmObj** %stackaddr$prim47344, align 8
%stackaddr$prim47345 = alloca %struct.ScmObj*, align 8
%args46562$ae43833$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43834, %struct.ScmObj* %args46562$ae43833$1)
store volatile %struct.ScmObj* %args46562$ae43833$2, %struct.ScmObj** %stackaddr$prim47345, align 8
%clofunc47346 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43833)
musttail call tailcc void %clofunc47346(%struct.ScmObj* %ae43833, %struct.ScmObj* %args46562$ae43833$2)
ret void
}

define tailcc void @proc_clo$ae43833(%struct.ScmObj* %env$ae43833,%struct.ScmObj* %current_45args46488) {
%stackaddr$env-ref47347 = alloca %struct.ScmObj*, align 8
%_37map40149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43833, i64 0)
store %struct.ScmObj* %_37map40149, %struct.ScmObj** %stackaddr$env-ref47347
%stackaddr$env-ref47348 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43833, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47348
%stackaddr$prim47349 = alloca %struct.ScmObj*, align 8
%_95k40379 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46488)
store volatile %struct.ScmObj* %_95k40379, %struct.ScmObj** %stackaddr$prim47349, align 8
%stackaddr$prim47350 = alloca %struct.ScmObj*, align 8
%current_45args46489 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46488)
store volatile %struct.ScmObj* %current_45args46489, %struct.ScmObj** %stackaddr$prim47350, align 8
%stackaddr$prim47351 = alloca %struct.ScmObj*, align 8
%_37second40163 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46489)
store volatile %struct.ScmObj* %_37second40163, %struct.ScmObj** %stackaddr$prim47351, align 8
%stackaddr$makeclosure47352 = alloca %struct.ScmObj*, align 8
%fptrToInt47353 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43855 to i64
%ae43855 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47353)
store volatile %struct.ScmObj* %ae43855, %struct.ScmObj** %stackaddr$makeclosure47352, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43855, %struct.ScmObj* %_37map40149, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43855, %struct.ScmObj* %_37foldl140107, i64 1)
%ae43856 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47354 = alloca %struct.ScmObj*, align 8
%fptrToInt47355 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43857 to i64
%ae43857 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47355)
store volatile %struct.ScmObj* %ae43857, %struct.ScmObj** %stackaddr$makeclosure47354, align 8
%args46557$ae43855$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47356 = alloca %struct.ScmObj*, align 8
%args46557$ae43855$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43857, %struct.ScmObj* %args46557$ae43855$0)
store volatile %struct.ScmObj* %args46557$ae43855$1, %struct.ScmObj** %stackaddr$prim47356, align 8
%stackaddr$prim47357 = alloca %struct.ScmObj*, align 8
%args46557$ae43855$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43856, %struct.ScmObj* %args46557$ae43855$1)
store volatile %struct.ScmObj* %args46557$ae43855$2, %struct.ScmObj** %stackaddr$prim47357, align 8
%clofunc47358 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43855)
musttail call tailcc void %clofunc47358(%struct.ScmObj* %ae43855, %struct.ScmObj* %args46557$ae43855$2)
ret void
}

define tailcc void @proc_clo$ae43855(%struct.ScmObj* %env$ae43855,%struct.ScmObj* %current_45args46491) {
%stackaddr$env-ref47359 = alloca %struct.ScmObj*, align 8
%_37map40149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43855, i64 0)
store %struct.ScmObj* %_37map40149, %struct.ScmObj** %stackaddr$env-ref47359
%stackaddr$env-ref47360 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43855, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47360
%stackaddr$prim47361 = alloca %struct.ScmObj*, align 8
%_95k40380 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46491)
store volatile %struct.ScmObj* %_95k40380, %struct.ScmObj** %stackaddr$prim47361, align 8
%stackaddr$prim47362 = alloca %struct.ScmObj*, align 8
%current_45args46492 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46491)
store volatile %struct.ScmObj* %current_45args46492, %struct.ScmObj** %stackaddr$prim47362, align 8
%stackaddr$prim47363 = alloca %struct.ScmObj*, align 8
%_37third40161 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46492)
store volatile %struct.ScmObj* %_37third40161, %struct.ScmObj** %stackaddr$prim47363, align 8
%stackaddr$makeclosure47364 = alloca %struct.ScmObj*, align 8
%fptrToInt47365 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43879 to i64
%ae43879 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47365)
store volatile %struct.ScmObj* %ae43879, %struct.ScmObj** %stackaddr$makeclosure47364, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43879, %struct.ScmObj* %_37map40149, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43879, %struct.ScmObj* %_37foldl140107, i64 1)
%ae43880 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47366 = alloca %struct.ScmObj*, align 8
%fptrToInt47367 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43881 to i64
%ae43881 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47367)
store volatile %struct.ScmObj* %ae43881, %struct.ScmObj** %stackaddr$makeclosure47366, align 8
%args46552$ae43879$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47368 = alloca %struct.ScmObj*, align 8
%args46552$ae43879$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43881, %struct.ScmObj* %args46552$ae43879$0)
store volatile %struct.ScmObj* %args46552$ae43879$1, %struct.ScmObj** %stackaddr$prim47368, align 8
%stackaddr$prim47369 = alloca %struct.ScmObj*, align 8
%args46552$ae43879$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43880, %struct.ScmObj* %args46552$ae43879$1)
store volatile %struct.ScmObj* %args46552$ae43879$2, %struct.ScmObj** %stackaddr$prim47369, align 8
%clofunc47370 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43879)
musttail call tailcc void %clofunc47370(%struct.ScmObj* %ae43879, %struct.ScmObj* %args46552$ae43879$2)
ret void
}

define tailcc void @proc_clo$ae43879(%struct.ScmObj* %env$ae43879,%struct.ScmObj* %current_45args46494) {
%stackaddr$env-ref47371 = alloca %struct.ScmObj*, align 8
%_37map40149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43879, i64 0)
store %struct.ScmObj* %_37map40149, %struct.ScmObj** %stackaddr$env-ref47371
%stackaddr$env-ref47372 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43879, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47372
%stackaddr$prim47373 = alloca %struct.ScmObj*, align 8
%_95k40381 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46494)
store volatile %struct.ScmObj* %_95k40381, %struct.ScmObj** %stackaddr$prim47373, align 8
%stackaddr$prim47374 = alloca %struct.ScmObj*, align 8
%current_45args46495 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46494)
store volatile %struct.ScmObj* %current_45args46495, %struct.ScmObj** %stackaddr$prim47374, align 8
%stackaddr$prim47375 = alloca %struct.ScmObj*, align 8
%_37fourth40159 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46495)
store volatile %struct.ScmObj* %_37fourth40159, %struct.ScmObj** %stackaddr$prim47375, align 8
%stackaddr$makeclosure47376 = alloca %struct.ScmObj*, align 8
%fptrToInt47377 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43905 to i64
%ae43905 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47377)
store volatile %struct.ScmObj* %ae43905, %struct.ScmObj** %stackaddr$makeclosure47376, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43905, %struct.ScmObj* %_37map40149, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43905, %struct.ScmObj* %_37foldl140107, i64 1)
%ae43906 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47378 = alloca %struct.ScmObj*, align 8
%fptrToInt47379 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43907 to i64
%ae43907 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47379)
store volatile %struct.ScmObj* %ae43907, %struct.ScmObj** %stackaddr$makeclosure47378, align 8
%args46547$ae43905$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47380 = alloca %struct.ScmObj*, align 8
%args46547$ae43905$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43907, %struct.ScmObj* %args46547$ae43905$0)
store volatile %struct.ScmObj* %args46547$ae43905$1, %struct.ScmObj** %stackaddr$prim47380, align 8
%stackaddr$prim47381 = alloca %struct.ScmObj*, align 8
%args46547$ae43905$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43906, %struct.ScmObj* %args46547$ae43905$1)
store volatile %struct.ScmObj* %args46547$ae43905$2, %struct.ScmObj** %stackaddr$prim47381, align 8
%clofunc47382 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43905)
musttail call tailcc void %clofunc47382(%struct.ScmObj* %ae43905, %struct.ScmObj* %args46547$ae43905$2)
ret void
}

define tailcc void @proc_clo$ae43905(%struct.ScmObj* %env$ae43905,%struct.ScmObj* %current_45args46497) {
%stackaddr$env-ref47383 = alloca %struct.ScmObj*, align 8
%_37map40149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43905, i64 0)
store %struct.ScmObj* %_37map40149, %struct.ScmObj** %stackaddr$env-ref47383
%stackaddr$env-ref47384 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43905, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47384
%stackaddr$prim47385 = alloca %struct.ScmObj*, align 8
%_95k40382 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46497)
store volatile %struct.ScmObj* %_95k40382, %struct.ScmObj** %stackaddr$prim47385, align 8
%stackaddr$prim47386 = alloca %struct.ScmObj*, align 8
%current_45args46498 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46497)
store volatile %struct.ScmObj* %current_45args46498, %struct.ScmObj** %stackaddr$prim47386, align 8
%stackaddr$prim47387 = alloca %struct.ScmObj*, align 8
%promise_6340220 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46498)
store volatile %struct.ScmObj* %promise_6340220, %struct.ScmObj** %stackaddr$prim47387, align 8
%stackaddr$makeclosure47388 = alloca %struct.ScmObj*, align 8
%fptrToInt47389 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43992 to i64
%ae43992 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47389)
store volatile %struct.ScmObj* %ae43992, %struct.ScmObj** %stackaddr$makeclosure47388, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43992, %struct.ScmObj* %_37map40149, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43992, %struct.ScmObj* %_37foldl140107, i64 1)
%ae43993 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47390 = alloca %struct.ScmObj*, align 8
%fptrToInt47391 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43994 to i64
%ae43994 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47391)
store volatile %struct.ScmObj* %ae43994, %struct.ScmObj** %stackaddr$makeclosure47390, align 8
%args46540$ae43992$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47392 = alloca %struct.ScmObj*, align 8
%args46540$ae43992$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43994, %struct.ScmObj* %args46540$ae43992$0)
store volatile %struct.ScmObj* %args46540$ae43992$1, %struct.ScmObj** %stackaddr$prim47392, align 8
%stackaddr$prim47393 = alloca %struct.ScmObj*, align 8
%args46540$ae43992$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43993, %struct.ScmObj* %args46540$ae43992$1)
store volatile %struct.ScmObj* %args46540$ae43992$2, %struct.ScmObj** %stackaddr$prim47393, align 8
%clofunc47394 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43992)
musttail call tailcc void %clofunc47394(%struct.ScmObj* %ae43992, %struct.ScmObj* %args46540$ae43992$2)
ret void
}

define tailcc void @proc_clo$ae43992(%struct.ScmObj* %env$ae43992,%struct.ScmObj* %current_45args46500) {
%stackaddr$env-ref47395 = alloca %struct.ScmObj*, align 8
%_37map40149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43992, i64 0)
store %struct.ScmObj* %_37map40149, %struct.ScmObj** %stackaddr$env-ref47395
%stackaddr$env-ref47396 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43992, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47396
%stackaddr$prim47397 = alloca %struct.ScmObj*, align 8
%_95k40383 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46500)
store volatile %struct.ScmObj* %_95k40383, %struct.ScmObj** %stackaddr$prim47397, align 8
%stackaddr$prim47398 = alloca %struct.ScmObj*, align 8
%current_45args46501 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46500)
store volatile %struct.ScmObj* %current_45args46501, %struct.ScmObj** %stackaddr$prim47398, align 8
%stackaddr$prim47399 = alloca %struct.ScmObj*, align 8
%anf_45bind40342 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46501)
store volatile %struct.ScmObj* %anf_45bind40342, %struct.ScmObj** %stackaddr$prim47399, align 8
%stackaddr$makeclosure47400 = alloca %struct.ScmObj*, align 8
%fptrToInt47401 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44016 to i64
%ae44016 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47401)
store volatile %struct.ScmObj* %ae44016, %struct.ScmObj** %stackaddr$makeclosure47400, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44016, %struct.ScmObj* %anf_45bind40342, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44016, %struct.ScmObj* %_37map40149, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44016, %struct.ScmObj* %_37foldl140107, i64 2)
%ae44017 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47402 = alloca %struct.ScmObj*, align 8
%fptrToInt47403 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44018 to i64
%ae44018 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47403)
store volatile %struct.ScmObj* %ae44018, %struct.ScmObj** %stackaddr$makeclosure47402, align 8
%args46538$ae44016$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47404 = alloca %struct.ScmObj*, align 8
%args46538$ae44016$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44018, %struct.ScmObj* %args46538$ae44016$0)
store volatile %struct.ScmObj* %args46538$ae44016$1, %struct.ScmObj** %stackaddr$prim47404, align 8
%stackaddr$prim47405 = alloca %struct.ScmObj*, align 8
%args46538$ae44016$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44017, %struct.ScmObj* %args46538$ae44016$1)
store volatile %struct.ScmObj* %args46538$ae44016$2, %struct.ScmObj** %stackaddr$prim47405, align 8
%clofunc47406 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44016)
musttail call tailcc void %clofunc47406(%struct.ScmObj* %ae44016, %struct.ScmObj* %args46538$ae44016$2)
ret void
}

define tailcc void @proc_clo$ae44016(%struct.ScmObj* %env$ae44016,%struct.ScmObj* %current_45args46503) {
%stackaddr$env-ref47407 = alloca %struct.ScmObj*, align 8
%anf_45bind40342 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44016, i64 0)
store %struct.ScmObj* %anf_45bind40342, %struct.ScmObj** %stackaddr$env-ref47407
%stackaddr$env-ref47408 = alloca %struct.ScmObj*, align 8
%_37map40149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44016, i64 1)
store %struct.ScmObj* %_37map40149, %struct.ScmObj** %stackaddr$env-ref47408
%stackaddr$env-ref47409 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44016, i64 2)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47409
%stackaddr$prim47410 = alloca %struct.ScmObj*, align 8
%_95k40384 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46503)
store volatile %struct.ScmObj* %_95k40384, %struct.ScmObj** %stackaddr$prim47410, align 8
%stackaddr$prim47411 = alloca %struct.ScmObj*, align 8
%current_45args46504 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46503)
store volatile %struct.ScmObj* %current_45args46504, %struct.ScmObj** %stackaddr$prim47411, align 8
%stackaddr$prim47412 = alloca %struct.ScmObj*, align 8
%anf_45bind40344 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46504)
store volatile %struct.ScmObj* %anf_45bind40344, %struct.ScmObj** %stackaddr$prim47412, align 8
%stackaddr$makeclosure47413 = alloca %struct.ScmObj*, align 8
%fptrToInt47414 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44042 to i64
%ae44042 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt47414)
store volatile %struct.ScmObj* %ae44042, %struct.ScmObj** %stackaddr$makeclosure47413, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44042, %struct.ScmObj* %anf_45bind40342, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44042, %struct.ScmObj* %_37map40149, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44042, %struct.ScmObj* %_37foldl140107, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44042, %struct.ScmObj* %anf_45bind40344, i64 3)
%ae44043 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47415 = alloca %struct.ScmObj*, align 8
%fptrToInt47416 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44044 to i64
%ae44044 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47416)
store volatile %struct.ScmObj* %ae44044, %struct.ScmObj** %stackaddr$makeclosure47415, align 8
%args46532$ae44042$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47417 = alloca %struct.ScmObj*, align 8
%args46532$ae44042$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44044, %struct.ScmObj* %args46532$ae44042$0)
store volatile %struct.ScmObj* %args46532$ae44042$1, %struct.ScmObj** %stackaddr$prim47417, align 8
%stackaddr$prim47418 = alloca %struct.ScmObj*, align 8
%args46532$ae44042$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44043, %struct.ScmObj* %args46532$ae44042$1)
store volatile %struct.ScmObj* %args46532$ae44042$2, %struct.ScmObj** %stackaddr$prim47418, align 8
%clofunc47419 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44042)
musttail call tailcc void %clofunc47419(%struct.ScmObj* %ae44042, %struct.ScmObj* %args46532$ae44042$2)
ret void
}

define tailcc void @proc_clo$ae44042(%struct.ScmObj* %env$ae44042,%struct.ScmObj* %current_45args46506) {
%stackaddr$env-ref47420 = alloca %struct.ScmObj*, align 8
%anf_45bind40342 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44042, i64 0)
store %struct.ScmObj* %anf_45bind40342, %struct.ScmObj** %stackaddr$env-ref47420
%stackaddr$env-ref47421 = alloca %struct.ScmObj*, align 8
%_37map40149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44042, i64 1)
store %struct.ScmObj* %_37map40149, %struct.ScmObj** %stackaddr$env-ref47421
%stackaddr$env-ref47422 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44042, i64 2)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47422
%stackaddr$env-ref47423 = alloca %struct.ScmObj*, align 8
%anf_45bind40344 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44042, i64 3)
store %struct.ScmObj* %anf_45bind40344, %struct.ScmObj** %stackaddr$env-ref47423
%stackaddr$prim47424 = alloca %struct.ScmObj*, align 8
%_95k40385 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46506)
store volatile %struct.ScmObj* %_95k40385, %struct.ScmObj** %stackaddr$prim47424, align 8
%stackaddr$prim47425 = alloca %struct.ScmObj*, align 8
%current_45args46507 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46506)
store volatile %struct.ScmObj* %current_45args46507, %struct.ScmObj** %stackaddr$prim47425, align 8
%stackaddr$prim47426 = alloca %struct.ScmObj*, align 8
%anf_45bind40345 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46507)
store volatile %struct.ScmObj* %anf_45bind40345, %struct.ScmObj** %stackaddr$prim47426, align 8
%stackaddr$makeclosure47427 = alloca %struct.ScmObj*, align 8
%fptrToInt47428 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44065 to i64
%ae44065 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt47428)
store volatile %struct.ScmObj* %ae44065, %struct.ScmObj** %stackaddr$makeclosure47427, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44065, %struct.ScmObj* %anf_45bind40342, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44065, %struct.ScmObj* %_37map40149, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44065, %struct.ScmObj* %_37foldl140107, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44065, %struct.ScmObj* %anf_45bind40344, i64 3)
%ae44066 = call %struct.ScmObj* @const_init_int(i64 2)
%ae44067 = call %struct.ScmObj* @const_init_int(i64 4)
%ae44068 = call %struct.ScmObj* @const_init_int(i64 6)
%args46530$anf_45bind40345$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47429 = alloca %struct.ScmObj*, align 8
%args46530$anf_45bind40345$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44068, %struct.ScmObj* %args46530$anf_45bind40345$0)
store volatile %struct.ScmObj* %args46530$anf_45bind40345$1, %struct.ScmObj** %stackaddr$prim47429, align 8
%stackaddr$prim47430 = alloca %struct.ScmObj*, align 8
%args46530$anf_45bind40345$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44067, %struct.ScmObj* %args46530$anf_45bind40345$1)
store volatile %struct.ScmObj* %args46530$anf_45bind40345$2, %struct.ScmObj** %stackaddr$prim47430, align 8
%stackaddr$prim47431 = alloca %struct.ScmObj*, align 8
%args46530$anf_45bind40345$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44066, %struct.ScmObj* %args46530$anf_45bind40345$2)
store volatile %struct.ScmObj* %args46530$anf_45bind40345$3, %struct.ScmObj** %stackaddr$prim47431, align 8
%stackaddr$prim47432 = alloca %struct.ScmObj*, align 8
%args46530$anf_45bind40345$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44065, %struct.ScmObj* %args46530$anf_45bind40345$3)
store volatile %struct.ScmObj* %args46530$anf_45bind40345$4, %struct.ScmObj** %stackaddr$prim47432, align 8
%clofunc47433 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40345)
musttail call tailcc void %clofunc47433(%struct.ScmObj* %anf_45bind40345, %struct.ScmObj* %args46530$anf_45bind40345$4)
ret void
}

define tailcc void @proc_clo$ae44065(%struct.ScmObj* %env$ae44065,%struct.ScmObj* %current_45args46509) {
%stackaddr$env-ref47434 = alloca %struct.ScmObj*, align 8
%anf_45bind40342 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44065, i64 0)
store %struct.ScmObj* %anf_45bind40342, %struct.ScmObj** %stackaddr$env-ref47434
%stackaddr$env-ref47435 = alloca %struct.ScmObj*, align 8
%_37map40149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44065, i64 1)
store %struct.ScmObj* %_37map40149, %struct.ScmObj** %stackaddr$env-ref47435
%stackaddr$env-ref47436 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44065, i64 2)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47436
%stackaddr$env-ref47437 = alloca %struct.ScmObj*, align 8
%anf_45bind40344 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44065, i64 3)
store %struct.ScmObj* %anf_45bind40344, %struct.ScmObj** %stackaddr$env-ref47437
%stackaddr$prim47438 = alloca %struct.ScmObj*, align 8
%_95k40386 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46509)
store volatile %struct.ScmObj* %_95k40386, %struct.ScmObj** %stackaddr$prim47438, align 8
%stackaddr$prim47439 = alloca %struct.ScmObj*, align 8
%current_45args46510 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46509)
store volatile %struct.ScmObj* %current_45args46510, %struct.ScmObj** %stackaddr$prim47439, align 8
%stackaddr$prim47440 = alloca %struct.ScmObj*, align 8
%anf_45bind40346 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46510)
store volatile %struct.ScmObj* %anf_45bind40346, %struct.ScmObj** %stackaddr$prim47440, align 8
%stackaddr$makeclosure47441 = alloca %struct.ScmObj*, align 8
%fptrToInt47442 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44084 to i64
%ae44084 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt47442)
store volatile %struct.ScmObj* %ae44084, %struct.ScmObj** %stackaddr$makeclosure47441, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44084, %struct.ScmObj* %anf_45bind40342, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44084, %struct.ScmObj* %_37map40149, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44084, %struct.ScmObj* %_37foldl140107, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44084, %struct.ScmObj* %anf_45bind40346, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44084, %struct.ScmObj* %anf_45bind40344, i64 4)
%ae44085 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47443 = alloca %struct.ScmObj*, align 8
%fptrToInt47444 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44086 to i64
%ae44086 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47444)
store volatile %struct.ScmObj* %ae44086, %struct.ScmObj** %stackaddr$makeclosure47443, align 8
%args46529$ae44084$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47445 = alloca %struct.ScmObj*, align 8
%args46529$ae44084$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44086, %struct.ScmObj* %args46529$ae44084$0)
store volatile %struct.ScmObj* %args46529$ae44084$1, %struct.ScmObj** %stackaddr$prim47445, align 8
%stackaddr$prim47446 = alloca %struct.ScmObj*, align 8
%args46529$ae44084$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44085, %struct.ScmObj* %args46529$ae44084$1)
store volatile %struct.ScmObj* %args46529$ae44084$2, %struct.ScmObj** %stackaddr$prim47446, align 8
%clofunc47447 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44084)
musttail call tailcc void %clofunc47447(%struct.ScmObj* %ae44084, %struct.ScmObj* %args46529$ae44084$2)
ret void
}

define tailcc void @proc_clo$ae44084(%struct.ScmObj* %env$ae44084,%struct.ScmObj* %current_45args46512) {
%stackaddr$env-ref47448 = alloca %struct.ScmObj*, align 8
%anf_45bind40342 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44084, i64 0)
store %struct.ScmObj* %anf_45bind40342, %struct.ScmObj** %stackaddr$env-ref47448
%stackaddr$env-ref47449 = alloca %struct.ScmObj*, align 8
%_37map40149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44084, i64 1)
store %struct.ScmObj* %_37map40149, %struct.ScmObj** %stackaddr$env-ref47449
%stackaddr$env-ref47450 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44084, i64 2)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47450
%stackaddr$env-ref47451 = alloca %struct.ScmObj*, align 8
%anf_45bind40346 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44084, i64 3)
store %struct.ScmObj* %anf_45bind40346, %struct.ScmObj** %stackaddr$env-ref47451
%stackaddr$env-ref47452 = alloca %struct.ScmObj*, align 8
%anf_45bind40344 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44084, i64 4)
store %struct.ScmObj* %anf_45bind40344, %struct.ScmObj** %stackaddr$env-ref47452
%stackaddr$prim47453 = alloca %struct.ScmObj*, align 8
%_95k40387 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46512)
store volatile %struct.ScmObj* %_95k40387, %struct.ScmObj** %stackaddr$prim47453, align 8
%stackaddr$prim47454 = alloca %struct.ScmObj*, align 8
%current_45args46513 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46512)
store volatile %struct.ScmObj* %current_45args46513, %struct.ScmObj** %stackaddr$prim47454, align 8
%stackaddr$prim47455 = alloca %struct.ScmObj*, align 8
%anf_45bind40347 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46513)
store volatile %struct.ScmObj* %anf_45bind40347, %struct.ScmObj** %stackaddr$prim47455, align 8
%stackaddr$makeclosure47456 = alloca %struct.ScmObj*, align 8
%fptrToInt47457 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44107 to i64
%ae44107 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt47457)
store volatile %struct.ScmObj* %ae44107, %struct.ScmObj** %stackaddr$makeclosure47456, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44107, %struct.ScmObj* %anf_45bind40342, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44107, %struct.ScmObj* %_37map40149, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44107, %struct.ScmObj* %_37foldl140107, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44107, %struct.ScmObj* %anf_45bind40346, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44107, %struct.ScmObj* %anf_45bind40344, i64 4)
%ae44108 = call %struct.ScmObj* @const_init_int(i64 7)
%ae44109 = call %struct.ScmObj* @const_init_int(i64 8)
%ae44110 = call %struct.ScmObj* @const_init_int(i64 9)
%args46527$anf_45bind40347$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47458 = alloca %struct.ScmObj*, align 8
%args46527$anf_45bind40347$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44110, %struct.ScmObj* %args46527$anf_45bind40347$0)
store volatile %struct.ScmObj* %args46527$anf_45bind40347$1, %struct.ScmObj** %stackaddr$prim47458, align 8
%stackaddr$prim47459 = alloca %struct.ScmObj*, align 8
%args46527$anf_45bind40347$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44109, %struct.ScmObj* %args46527$anf_45bind40347$1)
store volatile %struct.ScmObj* %args46527$anf_45bind40347$2, %struct.ScmObj** %stackaddr$prim47459, align 8
%stackaddr$prim47460 = alloca %struct.ScmObj*, align 8
%args46527$anf_45bind40347$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44108, %struct.ScmObj* %args46527$anf_45bind40347$2)
store volatile %struct.ScmObj* %args46527$anf_45bind40347$3, %struct.ScmObj** %stackaddr$prim47460, align 8
%stackaddr$prim47461 = alloca %struct.ScmObj*, align 8
%args46527$anf_45bind40347$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44107, %struct.ScmObj* %args46527$anf_45bind40347$3)
store volatile %struct.ScmObj* %args46527$anf_45bind40347$4, %struct.ScmObj** %stackaddr$prim47461, align 8
%clofunc47462 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40347)
musttail call tailcc void %clofunc47462(%struct.ScmObj* %anf_45bind40347, %struct.ScmObj* %args46527$anf_45bind40347$4)
ret void
}

define tailcc void @proc_clo$ae44107(%struct.ScmObj* %env$ae44107,%struct.ScmObj* %current_45args46515) {
%stackaddr$env-ref47463 = alloca %struct.ScmObj*, align 8
%anf_45bind40342 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44107, i64 0)
store %struct.ScmObj* %anf_45bind40342, %struct.ScmObj** %stackaddr$env-ref47463
%stackaddr$env-ref47464 = alloca %struct.ScmObj*, align 8
%_37map40149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44107, i64 1)
store %struct.ScmObj* %_37map40149, %struct.ScmObj** %stackaddr$env-ref47464
%stackaddr$env-ref47465 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44107, i64 2)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47465
%stackaddr$env-ref47466 = alloca %struct.ScmObj*, align 8
%anf_45bind40346 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44107, i64 3)
store %struct.ScmObj* %anf_45bind40346, %struct.ScmObj** %stackaddr$env-ref47466
%stackaddr$env-ref47467 = alloca %struct.ScmObj*, align 8
%anf_45bind40344 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44107, i64 4)
store %struct.ScmObj* %anf_45bind40344, %struct.ScmObj** %stackaddr$env-ref47467
%stackaddr$prim47468 = alloca %struct.ScmObj*, align 8
%_95k40388 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46515)
store volatile %struct.ScmObj* %_95k40388, %struct.ScmObj** %stackaddr$prim47468, align 8
%stackaddr$prim47469 = alloca %struct.ScmObj*, align 8
%current_45args46516 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46515)
store volatile %struct.ScmObj* %current_45args46516, %struct.ScmObj** %stackaddr$prim47469, align 8
%stackaddr$prim47470 = alloca %struct.ScmObj*, align 8
%anf_45bind40348 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46516)
store volatile %struct.ScmObj* %anf_45bind40348, %struct.ScmObj** %stackaddr$prim47470, align 8
%stackaddr$makeclosure47471 = alloca %struct.ScmObj*, align 8
%fptrToInt47472 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44127 to i64
%ae44127 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47472)
store volatile %struct.ScmObj* %ae44127, %struct.ScmObj** %stackaddr$makeclosure47471, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44127, %struct.ScmObj* %anf_45bind40342, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44127, %struct.ScmObj* %_37foldl140107, i64 1)
%args46526$_37map40149$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47473 = alloca %struct.ScmObj*, align 8
%args46526$_37map40149$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40348, %struct.ScmObj* %args46526$_37map40149$0)
store volatile %struct.ScmObj* %args46526$_37map40149$1, %struct.ScmObj** %stackaddr$prim47473, align 8
%stackaddr$prim47474 = alloca %struct.ScmObj*, align 8
%args46526$_37map40149$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40346, %struct.ScmObj* %args46526$_37map40149$1)
store volatile %struct.ScmObj* %args46526$_37map40149$2, %struct.ScmObj** %stackaddr$prim47474, align 8
%stackaddr$prim47475 = alloca %struct.ScmObj*, align 8
%args46526$_37map40149$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40344, %struct.ScmObj* %args46526$_37map40149$2)
store volatile %struct.ScmObj* %args46526$_37map40149$3, %struct.ScmObj** %stackaddr$prim47475, align 8
%stackaddr$prim47476 = alloca %struct.ScmObj*, align 8
%args46526$_37map40149$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44127, %struct.ScmObj* %args46526$_37map40149$3)
store volatile %struct.ScmObj* %args46526$_37map40149$4, %struct.ScmObj** %stackaddr$prim47476, align 8
%clofunc47477 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map40149)
musttail call tailcc void %clofunc47477(%struct.ScmObj* %_37map40149, %struct.ScmObj* %args46526$_37map40149$4)
ret void
}

define tailcc void @proc_clo$ae44127(%struct.ScmObj* %env$ae44127,%struct.ScmObj* %current_45args46518) {
%stackaddr$env-ref47478 = alloca %struct.ScmObj*, align 8
%anf_45bind40342 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44127, i64 0)
store %struct.ScmObj* %anf_45bind40342, %struct.ScmObj** %stackaddr$env-ref47478
%stackaddr$env-ref47479 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44127, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47479
%stackaddr$prim47480 = alloca %struct.ScmObj*, align 8
%_95k40389 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46518)
store volatile %struct.ScmObj* %_95k40389, %struct.ScmObj** %stackaddr$prim47480, align 8
%stackaddr$prim47481 = alloca %struct.ScmObj*, align 8
%current_45args46519 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46518)
store volatile %struct.ScmObj* %current_45args46519, %struct.ScmObj** %stackaddr$prim47481, align 8
%stackaddr$prim47482 = alloca %struct.ScmObj*, align 8
%anf_45bind40349 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46519)
store volatile %struct.ScmObj* %anf_45bind40349, %struct.ScmObj** %stackaddr$prim47482, align 8
%stackaddr$makeclosure47483 = alloca %struct.ScmObj*, align 8
%fptrToInt47484 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44132 to i64
%ae44132 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47484)
store volatile %struct.ScmObj* %ae44132, %struct.ScmObj** %stackaddr$makeclosure47483, align 8
%ae44134 = call %struct.ScmObj* @const_init_int(i64 0)
%args46525$_37foldl140107$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47485 = alloca %struct.ScmObj*, align 8
%args46525$_37foldl140107$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40349, %struct.ScmObj* %args46525$_37foldl140107$0)
store volatile %struct.ScmObj* %args46525$_37foldl140107$1, %struct.ScmObj** %stackaddr$prim47485, align 8
%stackaddr$prim47486 = alloca %struct.ScmObj*, align 8
%args46525$_37foldl140107$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44134, %struct.ScmObj* %args46525$_37foldl140107$1)
store volatile %struct.ScmObj* %args46525$_37foldl140107$2, %struct.ScmObj** %stackaddr$prim47486, align 8
%stackaddr$prim47487 = alloca %struct.ScmObj*, align 8
%args46525$_37foldl140107$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40342, %struct.ScmObj* %args46525$_37foldl140107$2)
store volatile %struct.ScmObj* %args46525$_37foldl140107$3, %struct.ScmObj** %stackaddr$prim47487, align 8
%stackaddr$prim47488 = alloca %struct.ScmObj*, align 8
%args46525$_37foldl140107$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44132, %struct.ScmObj* %args46525$_37foldl140107$3)
store volatile %struct.ScmObj* %args46525$_37foldl140107$4, %struct.ScmObj** %stackaddr$prim47488, align 8
%clofunc47489 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140107)
musttail call tailcc void %clofunc47489(%struct.ScmObj* %_37foldl140107, %struct.ScmObj* %args46525$_37foldl140107$4)
ret void
}

define tailcc void @proc_clo$ae44132(%struct.ScmObj* %env$ae44132,%struct.ScmObj* %current_45args46521) {
%stackaddr$prim47490 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46521)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim47490, align 8
%stackaddr$prim47491 = alloca %struct.ScmObj*, align 8
%current_45args46522 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46521)
store volatile %struct.ScmObj* %current_45args46522, %struct.ScmObj** %stackaddr$prim47491, align 8
%stackaddr$prim47492 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46522)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim47492, align 8
%stackaddr$prim47493 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim47493, align 8
%args46524$k$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47494 = alloca %struct.ScmObj*, align 8
%args46524$k$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %args46524$k$0)
store volatile %struct.ScmObj* %args46524$k$1, %struct.ScmObj** %stackaddr$prim47494, align 8
%clofunc47495 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc47495(%struct.ScmObj* %k, %struct.ScmObj* %args46524$k$1)
ret void
}

define tailcc void @proc_clo$ae44086(%struct.ScmObj* %env$ae44086,%struct.ScmObj* %lst4022640390) {
%stackaddr$prim47496 = alloca %struct.ScmObj*, align 8
%k40391 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4022640390)
store volatile %struct.ScmObj* %k40391, %struct.ScmObj** %stackaddr$prim47496, align 8
%stackaddr$prim47497 = alloca %struct.ScmObj*, align 8
%lst40226 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4022640390)
store volatile %struct.ScmObj* %lst40226, %struct.ScmObj** %stackaddr$prim47497, align 8
%ae44090 = call %struct.ScmObj* @const_init_int(i64 0)
%args46528$k40391$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47498 = alloca %struct.ScmObj*, align 8
%args46528$k40391$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40226, %struct.ScmObj* %args46528$k40391$0)
store volatile %struct.ScmObj* %args46528$k40391$1, %struct.ScmObj** %stackaddr$prim47498, align 8
%stackaddr$prim47499 = alloca %struct.ScmObj*, align 8
%args46528$k40391$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44090, %struct.ScmObj* %args46528$k40391$1)
store volatile %struct.ScmObj* %args46528$k40391$2, %struct.ScmObj** %stackaddr$prim47499, align 8
%clofunc47500 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40391)
musttail call tailcc void %clofunc47500(%struct.ScmObj* %k40391, %struct.ScmObj* %args46528$k40391$2)
ret void
}

define tailcc void @proc_clo$ae44044(%struct.ScmObj* %env$ae44044,%struct.ScmObj* %lst4022540392) {
%stackaddr$prim47501 = alloca %struct.ScmObj*, align 8
%k40393 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4022540392)
store volatile %struct.ScmObj* %k40393, %struct.ScmObj** %stackaddr$prim47501, align 8
%stackaddr$prim47502 = alloca %struct.ScmObj*, align 8
%lst40225 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4022540392)
store volatile %struct.ScmObj* %lst40225, %struct.ScmObj** %stackaddr$prim47502, align 8
%ae44048 = call %struct.ScmObj* @const_init_int(i64 0)
%args46531$k40393$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47503 = alloca %struct.ScmObj*, align 8
%args46531$k40393$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40225, %struct.ScmObj* %args46531$k40393$0)
store volatile %struct.ScmObj* %args46531$k40393$1, %struct.ScmObj** %stackaddr$prim47503, align 8
%stackaddr$prim47504 = alloca %struct.ScmObj*, align 8
%args46531$k40393$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44048, %struct.ScmObj* %args46531$k40393$1)
store volatile %struct.ScmObj* %args46531$k40393$2, %struct.ScmObj** %stackaddr$prim47504, align 8
%clofunc47505 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40393)
musttail call tailcc void %clofunc47505(%struct.ScmObj* %k40393, %struct.ScmObj* %args46531$k40393$2)
ret void
}

define tailcc void @proc_clo$ae44018(%struct.ScmObj* %env$ae44018,%struct.ScmObj* %current_45args46533) {
%stackaddr$prim47506 = alloca %struct.ScmObj*, align 8
%k40394 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46533)
store volatile %struct.ScmObj* %k40394, %struct.ScmObj** %stackaddr$prim47506, align 8
%stackaddr$prim47507 = alloca %struct.ScmObj*, align 8
%current_45args46534 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46533)
store volatile %struct.ScmObj* %current_45args46534, %struct.ScmObj** %stackaddr$prim47507, align 8
%stackaddr$prim47508 = alloca %struct.ScmObj*, align 8
%x40224 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46534)
store volatile %struct.ScmObj* %x40224, %struct.ScmObj** %stackaddr$prim47508, align 8
%stackaddr$prim47509 = alloca %struct.ScmObj*, align 8
%current_45args46535 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46534)
store volatile %struct.ScmObj* %current_45args46535, %struct.ScmObj** %stackaddr$prim47509, align 8
%stackaddr$prim47510 = alloca %struct.ScmObj*, align 8
%y40223 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46535)
store volatile %struct.ScmObj* %y40223, %struct.ScmObj** %stackaddr$prim47510, align 8
%stackaddr$prim47511 = alloca %struct.ScmObj*, align 8
%anf_45bind40343 = call %struct.ScmObj* @prim__42(%struct.ScmObj* %x40224, %struct.ScmObj* %x40224)
store volatile %struct.ScmObj* %anf_45bind40343, %struct.ScmObj** %stackaddr$prim47511, align 8
%stackaddr$prim47512 = alloca %struct.ScmObj*, align 8
%cpsprim40395 = call %struct.ScmObj* @prim__42(%struct.ScmObj* %y40223, %struct.ScmObj* %anf_45bind40343)
store volatile %struct.ScmObj* %cpsprim40395, %struct.ScmObj** %stackaddr$prim47512, align 8
%ae44024 = call %struct.ScmObj* @const_init_int(i64 0)
%args46537$k40394$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47513 = alloca %struct.ScmObj*, align 8
%args46537$k40394$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40395, %struct.ScmObj* %args46537$k40394$0)
store volatile %struct.ScmObj* %args46537$k40394$1, %struct.ScmObj** %stackaddr$prim47513, align 8
%stackaddr$prim47514 = alloca %struct.ScmObj*, align 8
%args46537$k40394$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44024, %struct.ScmObj* %args46537$k40394$1)
store volatile %struct.ScmObj* %args46537$k40394$2, %struct.ScmObj** %stackaddr$prim47514, align 8
%clofunc47515 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40394)
musttail call tailcc void %clofunc47515(%struct.ScmObj* %k40394, %struct.ScmObj* %args46537$k40394$2)
ret void
}

define tailcc void @proc_clo$ae43994(%struct.ScmObj* %env$ae43994,%struct.ScmObj* %args4022240396) {
%stackaddr$prim47516 = alloca %struct.ScmObj*, align 8
%k40397 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4022240396)
store volatile %struct.ScmObj* %k40397, %struct.ScmObj** %stackaddr$prim47516, align 8
%stackaddr$prim47517 = alloca %struct.ScmObj*, align 8
%args40222 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4022240396)
store volatile %struct.ScmObj* %args40222, %struct.ScmObj** %stackaddr$prim47517, align 8
%stackaddr$applyprim47518 = alloca %struct.ScmObj*, align 8
%cpsaprim40398 = call %struct.ScmObj* @applyprim__43(%struct.ScmObj* %args40222)
store volatile %struct.ScmObj* %cpsaprim40398, %struct.ScmObj** %stackaddr$applyprim47518, align 8
%ae43999 = call %struct.ScmObj* @const_init_int(i64 0)
%args46539$k40397$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47519 = alloca %struct.ScmObj*, align 8
%args46539$k40397$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsaprim40398, %struct.ScmObj* %args46539$k40397$0)
store volatile %struct.ScmObj* %args46539$k40397$1, %struct.ScmObj** %stackaddr$prim47519, align 8
%stackaddr$prim47520 = alloca %struct.ScmObj*, align 8
%args46539$k40397$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43999, %struct.ScmObj* %args46539$k40397$1)
store volatile %struct.ScmObj* %args46539$k40397$2, %struct.ScmObj** %stackaddr$prim47520, align 8
%clofunc47521 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40397)
musttail call tailcc void %clofunc47521(%struct.ScmObj* %k40397, %struct.ScmObj* %args46539$k40397$2)
ret void
}

define tailcc void @proc_clo$ae43907(%struct.ScmObj* %env$ae43907,%struct.ScmObj* %current_45args46541) {
%stackaddr$prim47522 = alloca %struct.ScmObj*, align 8
%k40399 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46541)
store volatile %struct.ScmObj* %k40399, %struct.ScmObj** %stackaddr$prim47522, align 8
%stackaddr$prim47523 = alloca %struct.ScmObj*, align 8
%current_45args46542 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46541)
store volatile %struct.ScmObj* %current_45args46542, %struct.ScmObj** %stackaddr$prim47523, align 8
%stackaddr$prim47524 = alloca %struct.ScmObj*, align 8
%thunk40221 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46542)
store volatile %struct.ScmObj* %thunk40221, %struct.ScmObj** %stackaddr$prim47524, align 8
%stackaddr$prim47525 = alloca %struct.ScmObj*, align 8
%anf_45bind40338 = call %struct.ScmObj* @prim_vector_63(%struct.ScmObj* %thunk40221)
store volatile %struct.ScmObj* %anf_45bind40338, %struct.ScmObj** %stackaddr$prim47525, align 8
%truthy$cmp47526 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40338)
%cmp$cmp47526 = icmp eq i64 %truthy$cmp47526, 1
br i1 %cmp$cmp47526, label %truebranch$cmp47526, label %falsebranch$cmp47526
truebranch$cmp47526:
%stackaddr$prim47527 = alloca %struct.ScmObj*, align 8
%anf_45bind40339 = call %struct.ScmObj* @prim_vector_45length(%struct.ScmObj* %thunk40221)
store volatile %struct.ScmObj* %anf_45bind40339, %struct.ScmObj** %stackaddr$prim47527, align 8
%ae43912 = call %struct.ScmObj* @const_init_int(i64 3)
%stackaddr$prim47528 = alloca %struct.ScmObj*, align 8
%anf_45bind40340 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind40339, %struct.ScmObj* %ae43912)
store volatile %struct.ScmObj* %anf_45bind40340, %struct.ScmObj** %stackaddr$prim47528, align 8
%truthy$cmp47529 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40340)
%cmp$cmp47529 = icmp eq i64 %truthy$cmp47529, 1
br i1 %cmp$cmp47529, label %truebranch$cmp47529, label %falsebranch$cmp47529
truebranch$cmp47529:
%ae43915 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47530 = alloca %struct.ScmObj*, align 8
%anf_45bind40341 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk40221, %struct.ScmObj* %ae43915)
store volatile %struct.ScmObj* %anf_45bind40341, %struct.ScmObj** %stackaddr$prim47530, align 8
%ae43917 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @global$sym$ae4391747531, i32 0, i32 0))
%stackaddr$prim47532 = alloca %struct.ScmObj*, align 8
%cpsprim40400 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind40341, %struct.ScmObj* %ae43917)
store volatile %struct.ScmObj* %cpsprim40400, %struct.ScmObj** %stackaddr$prim47532, align 8
%ae43919 = call %struct.ScmObj* @const_init_int(i64 0)
%args46544$k40399$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47533 = alloca %struct.ScmObj*, align 8
%args46544$k40399$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40400, %struct.ScmObj* %args46544$k40399$0)
store volatile %struct.ScmObj* %args46544$k40399$1, %struct.ScmObj** %stackaddr$prim47533, align 8
%stackaddr$prim47534 = alloca %struct.ScmObj*, align 8
%args46544$k40399$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43919, %struct.ScmObj* %args46544$k40399$1)
store volatile %struct.ScmObj* %args46544$k40399$2, %struct.ScmObj** %stackaddr$prim47534, align 8
%clofunc47535 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40399)
musttail call tailcc void %clofunc47535(%struct.ScmObj* %k40399, %struct.ScmObj* %args46544$k40399$2)
ret void
falsebranch$cmp47529:
%ae43937 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43938 = call %struct.ScmObj* @const_init_false()
%args46545$k40399$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47536 = alloca %struct.ScmObj*, align 8
%args46545$k40399$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43938, %struct.ScmObj* %args46545$k40399$0)
store volatile %struct.ScmObj* %args46545$k40399$1, %struct.ScmObj** %stackaddr$prim47536, align 8
%stackaddr$prim47537 = alloca %struct.ScmObj*, align 8
%args46545$k40399$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43937, %struct.ScmObj* %args46545$k40399$1)
store volatile %struct.ScmObj* %args46545$k40399$2, %struct.ScmObj** %stackaddr$prim47537, align 8
%clofunc47538 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40399)
musttail call tailcc void %clofunc47538(%struct.ScmObj* %k40399, %struct.ScmObj* %args46545$k40399$2)
ret void
falsebranch$cmp47526:
%ae43959 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43960 = call %struct.ScmObj* @const_init_false()
%args46546$k40399$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47539 = alloca %struct.ScmObj*, align 8
%args46546$k40399$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43960, %struct.ScmObj* %args46546$k40399$0)
store volatile %struct.ScmObj* %args46546$k40399$1, %struct.ScmObj** %stackaddr$prim47539, align 8
%stackaddr$prim47540 = alloca %struct.ScmObj*, align 8
%args46546$k40399$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43959, %struct.ScmObj* %args46546$k40399$1)
store volatile %struct.ScmObj* %args46546$k40399$2, %struct.ScmObj** %stackaddr$prim47540, align 8
%clofunc47541 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40399)
musttail call tailcc void %clofunc47541(%struct.ScmObj* %k40399, %struct.ScmObj* %args46546$k40399$2)
ret void
}

define tailcc void @proc_clo$ae43881(%struct.ScmObj* %env$ae43881,%struct.ScmObj* %current_45args46548) {
%stackaddr$prim47542 = alloca %struct.ScmObj*, align 8
%k40401 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46548)
store volatile %struct.ScmObj* %k40401, %struct.ScmObj** %stackaddr$prim47542, align 8
%stackaddr$prim47543 = alloca %struct.ScmObj*, align 8
%current_45args46549 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46548)
store volatile %struct.ScmObj* %current_45args46549, %struct.ScmObj** %stackaddr$prim47543, align 8
%stackaddr$prim47544 = alloca %struct.ScmObj*, align 8
%x40160 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46549)
store volatile %struct.ScmObj* %x40160, %struct.ScmObj** %stackaddr$prim47544, align 8
%stackaddr$prim47545 = alloca %struct.ScmObj*, align 8
%anf_45bind40335 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40160)
store volatile %struct.ScmObj* %anf_45bind40335, %struct.ScmObj** %stackaddr$prim47545, align 8
%stackaddr$prim47546 = alloca %struct.ScmObj*, align 8
%anf_45bind40336 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40335)
store volatile %struct.ScmObj* %anf_45bind40336, %struct.ScmObj** %stackaddr$prim47546, align 8
%stackaddr$prim47547 = alloca %struct.ScmObj*, align 8
%anf_45bind40337 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40336)
store volatile %struct.ScmObj* %anf_45bind40337, %struct.ScmObj** %stackaddr$prim47547, align 8
%stackaddr$prim47548 = alloca %struct.ScmObj*, align 8
%cpsprim40402 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40337)
store volatile %struct.ScmObj* %cpsprim40402, %struct.ScmObj** %stackaddr$prim47548, align 8
%ae43887 = call %struct.ScmObj* @const_init_int(i64 0)
%args46551$k40401$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47549 = alloca %struct.ScmObj*, align 8
%args46551$k40401$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40402, %struct.ScmObj* %args46551$k40401$0)
store volatile %struct.ScmObj* %args46551$k40401$1, %struct.ScmObj** %stackaddr$prim47549, align 8
%stackaddr$prim47550 = alloca %struct.ScmObj*, align 8
%args46551$k40401$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43887, %struct.ScmObj* %args46551$k40401$1)
store volatile %struct.ScmObj* %args46551$k40401$2, %struct.ScmObj** %stackaddr$prim47550, align 8
%clofunc47551 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40401)
musttail call tailcc void %clofunc47551(%struct.ScmObj* %k40401, %struct.ScmObj* %args46551$k40401$2)
ret void
}

define tailcc void @proc_clo$ae43857(%struct.ScmObj* %env$ae43857,%struct.ScmObj* %current_45args46553) {
%stackaddr$prim47552 = alloca %struct.ScmObj*, align 8
%k40403 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46553)
store volatile %struct.ScmObj* %k40403, %struct.ScmObj** %stackaddr$prim47552, align 8
%stackaddr$prim47553 = alloca %struct.ScmObj*, align 8
%current_45args46554 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46553)
store volatile %struct.ScmObj* %current_45args46554, %struct.ScmObj** %stackaddr$prim47553, align 8
%stackaddr$prim47554 = alloca %struct.ScmObj*, align 8
%x40162 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46554)
store volatile %struct.ScmObj* %x40162, %struct.ScmObj** %stackaddr$prim47554, align 8
%stackaddr$prim47555 = alloca %struct.ScmObj*, align 8
%anf_45bind40333 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40162)
store volatile %struct.ScmObj* %anf_45bind40333, %struct.ScmObj** %stackaddr$prim47555, align 8
%stackaddr$prim47556 = alloca %struct.ScmObj*, align 8
%anf_45bind40334 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40333)
store volatile %struct.ScmObj* %anf_45bind40334, %struct.ScmObj** %stackaddr$prim47556, align 8
%stackaddr$prim47557 = alloca %struct.ScmObj*, align 8
%cpsprim40404 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40334)
store volatile %struct.ScmObj* %cpsprim40404, %struct.ScmObj** %stackaddr$prim47557, align 8
%ae43862 = call %struct.ScmObj* @const_init_int(i64 0)
%args46556$k40403$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47558 = alloca %struct.ScmObj*, align 8
%args46556$k40403$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40404, %struct.ScmObj* %args46556$k40403$0)
store volatile %struct.ScmObj* %args46556$k40403$1, %struct.ScmObj** %stackaddr$prim47558, align 8
%stackaddr$prim47559 = alloca %struct.ScmObj*, align 8
%args46556$k40403$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43862, %struct.ScmObj* %args46556$k40403$1)
store volatile %struct.ScmObj* %args46556$k40403$2, %struct.ScmObj** %stackaddr$prim47559, align 8
%clofunc47560 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40403)
musttail call tailcc void %clofunc47560(%struct.ScmObj* %k40403, %struct.ScmObj* %args46556$k40403$2)
ret void
}

define tailcc void @proc_clo$ae43835(%struct.ScmObj* %env$ae43835,%struct.ScmObj* %current_45args46558) {
%stackaddr$prim47561 = alloca %struct.ScmObj*, align 8
%k40405 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46558)
store volatile %struct.ScmObj* %k40405, %struct.ScmObj** %stackaddr$prim47561, align 8
%stackaddr$prim47562 = alloca %struct.ScmObj*, align 8
%current_45args46559 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46558)
store volatile %struct.ScmObj* %current_45args46559, %struct.ScmObj** %stackaddr$prim47562, align 8
%stackaddr$prim47563 = alloca %struct.ScmObj*, align 8
%x40164 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46559)
store volatile %struct.ScmObj* %x40164, %struct.ScmObj** %stackaddr$prim47563, align 8
%stackaddr$prim47564 = alloca %struct.ScmObj*, align 8
%anf_45bind40332 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40164)
store volatile %struct.ScmObj* %anf_45bind40332, %struct.ScmObj** %stackaddr$prim47564, align 8
%stackaddr$prim47565 = alloca %struct.ScmObj*, align 8
%cpsprim40406 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40332)
store volatile %struct.ScmObj* %cpsprim40406, %struct.ScmObj** %stackaddr$prim47565, align 8
%ae43839 = call %struct.ScmObj* @const_init_int(i64 0)
%args46561$k40405$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47566 = alloca %struct.ScmObj*, align 8
%args46561$k40405$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40406, %struct.ScmObj* %args46561$k40405$0)
store volatile %struct.ScmObj* %args46561$k40405$1, %struct.ScmObj** %stackaddr$prim47566, align 8
%stackaddr$prim47567 = alloca %struct.ScmObj*, align 8
%args46561$k40405$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43839, %struct.ScmObj* %args46561$k40405$1)
store volatile %struct.ScmObj* %args46561$k40405$2, %struct.ScmObj** %stackaddr$prim47567, align 8
%clofunc47568 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40405)
musttail call tailcc void %clofunc47568(%struct.ScmObj* %k40405, %struct.ScmObj* %args46561$k40405$2)
ret void
}

define tailcc void @proc_clo$ae43815(%struct.ScmObj* %env$ae43815,%struct.ScmObj* %current_45args46563) {
%stackaddr$prim47569 = alloca %struct.ScmObj*, align 8
%k40407 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46563)
store volatile %struct.ScmObj* %k40407, %struct.ScmObj** %stackaddr$prim47569, align 8
%stackaddr$prim47570 = alloca %struct.ScmObj*, align 8
%current_45args46564 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46563)
store volatile %struct.ScmObj* %current_45args46564, %struct.ScmObj** %stackaddr$prim47570, align 8
%stackaddr$prim47571 = alloca %struct.ScmObj*, align 8
%x40166 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46564)
store volatile %struct.ScmObj* %x40166, %struct.ScmObj** %stackaddr$prim47571, align 8
%stackaddr$prim47572 = alloca %struct.ScmObj*, align 8
%cpsprim40408 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40166)
store volatile %struct.ScmObj* %cpsprim40408, %struct.ScmObj** %stackaddr$prim47572, align 8
%ae43818 = call %struct.ScmObj* @const_init_int(i64 0)
%args46566$k40407$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47573 = alloca %struct.ScmObj*, align 8
%args46566$k40407$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40408, %struct.ScmObj* %args46566$k40407$0)
store volatile %struct.ScmObj* %args46566$k40407$1, %struct.ScmObj** %stackaddr$prim47573, align 8
%stackaddr$prim47574 = alloca %struct.ScmObj*, align 8
%args46566$k40407$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43818, %struct.ScmObj* %args46566$k40407$1)
store volatile %struct.ScmObj* %args46566$k40407$2, %struct.ScmObj** %stackaddr$prim47574, align 8
%clofunc47575 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40407)
musttail call tailcc void %clofunc47575(%struct.ScmObj* %k40407, %struct.ScmObj* %args46566$k40407$2)
ret void
}

define tailcc void @proc_clo$ae43717(%struct.ScmObj* %env$ae43717,%struct.ScmObj* %args4016840409) {
%stackaddr$env-ref47576 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43717, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47576
%stackaddr$prim47577 = alloca %struct.ScmObj*, align 8
%k40410 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4016840409)
store volatile %struct.ScmObj* %k40410, %struct.ScmObj** %stackaddr$prim47577, align 8
%stackaddr$prim47578 = alloca %struct.ScmObj*, align 8
%args40168 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4016840409)
store volatile %struct.ScmObj* %args40168, %struct.ScmObj** %stackaddr$prim47578, align 8
%stackaddr$prim47579 = alloca %struct.ScmObj*, align 8
%anf_45bind40326 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args40168)
store volatile %struct.ScmObj* %anf_45bind40326, %struct.ScmObj** %stackaddr$prim47579, align 8
%truthy$cmp47580 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40326)
%cmp$cmp47580 = icmp eq i64 %truthy$cmp47580, 1
br i1 %cmp$cmp47580, label %truebranch$cmp47580, label %falsebranch$cmp47580
truebranch$cmp47580:
%ae43723 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43724 = call %struct.ScmObj* @const_init_int(i64 1)
%args46568$k40410$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47581 = alloca %struct.ScmObj*, align 8
%args46568$k40410$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43724, %struct.ScmObj* %args46568$k40410$0)
store volatile %struct.ScmObj* %args46568$k40410$1, %struct.ScmObj** %stackaddr$prim47581, align 8
%stackaddr$prim47582 = alloca %struct.ScmObj*, align 8
%args46568$k40410$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43723, %struct.ScmObj* %args46568$k40410$1)
store volatile %struct.ScmObj* %args46568$k40410$2, %struct.ScmObj** %stackaddr$prim47582, align 8
%clofunc47583 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40410)
musttail call tailcc void %clofunc47583(%struct.ScmObj* %k40410, %struct.ScmObj* %args46568$k40410$2)
ret void
falsebranch$cmp47580:
%stackaddr$prim47584 = alloca %struct.ScmObj*, align 8
%anf_45bind40327 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40168)
store volatile %struct.ScmObj* %anf_45bind40327, %struct.ScmObj** %stackaddr$prim47584, align 8
%stackaddr$prim47585 = alloca %struct.ScmObj*, align 8
%anf_45bind40328 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40327)
store volatile %struct.ScmObj* %anf_45bind40328, %struct.ScmObj** %stackaddr$prim47585, align 8
%truthy$cmp47586 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40328)
%cmp$cmp47586 = icmp eq i64 %truthy$cmp47586, 1
br i1 %cmp$cmp47586, label %truebranch$cmp47586, label %falsebranch$cmp47586
truebranch$cmp47586:
%stackaddr$prim47587 = alloca %struct.ScmObj*, align 8
%cpsprim40411 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40168)
store volatile %struct.ScmObj* %cpsprim40411, %struct.ScmObj** %stackaddr$prim47587, align 8
%ae43736 = call %struct.ScmObj* @const_init_int(i64 0)
%args46569$k40410$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47588 = alloca %struct.ScmObj*, align 8
%args46569$k40410$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40411, %struct.ScmObj* %args46569$k40410$0)
store volatile %struct.ScmObj* %args46569$k40410$1, %struct.ScmObj** %stackaddr$prim47588, align 8
%stackaddr$prim47589 = alloca %struct.ScmObj*, align 8
%args46569$k40410$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43736, %struct.ScmObj* %args46569$k40410$1)
store volatile %struct.ScmObj* %args46569$k40410$2, %struct.ScmObj** %stackaddr$prim47589, align 8
%clofunc47590 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40410)
musttail call tailcc void %clofunc47590(%struct.ScmObj* %k40410, %struct.ScmObj* %args46569$k40410$2)
ret void
falsebranch$cmp47586:
%stackaddr$makeclosure47591 = alloca %struct.ScmObj*, align 8
%fptrToInt47592 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43741 to i64
%ae43741 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47592)
store volatile %struct.ScmObj* %ae43741, %struct.ScmObj** %stackaddr$makeclosure47591, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43741, %struct.ScmObj* %_37foldl140107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43741, %struct.ScmObj* %k40410, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43741, %struct.ScmObj* %args40168, i64 2)
%ae43742 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47593 = alloca %struct.ScmObj*, align 8
%fptrToInt47594 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43743 to i64
%ae43743 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47594)
store volatile %struct.ScmObj* %ae43743, %struct.ScmObj** %stackaddr$makeclosure47593, align 8
%args46579$ae43741$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47595 = alloca %struct.ScmObj*, align 8
%args46579$ae43741$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43743, %struct.ScmObj* %args46579$ae43741$0)
store volatile %struct.ScmObj* %args46579$ae43741$1, %struct.ScmObj** %stackaddr$prim47595, align 8
%stackaddr$prim47596 = alloca %struct.ScmObj*, align 8
%args46579$ae43741$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43742, %struct.ScmObj* %args46579$ae43741$1)
store volatile %struct.ScmObj* %args46579$ae43741$2, %struct.ScmObj** %stackaddr$prim47596, align 8
%clofunc47597 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43741)
musttail call tailcc void %clofunc47597(%struct.ScmObj* %ae43741, %struct.ScmObj* %args46579$ae43741$2)
ret void
}

define tailcc void @proc_clo$ae43741(%struct.ScmObj* %env$ae43741,%struct.ScmObj* %current_45args46570) {
%stackaddr$env-ref47598 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43741, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47598
%stackaddr$env-ref47599 = alloca %struct.ScmObj*, align 8
%k40410 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43741, i64 1)
store %struct.ScmObj* %k40410, %struct.ScmObj** %stackaddr$env-ref47599
%stackaddr$env-ref47600 = alloca %struct.ScmObj*, align 8
%args40168 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43741, i64 2)
store %struct.ScmObj* %args40168, %struct.ScmObj** %stackaddr$env-ref47600
%stackaddr$prim47601 = alloca %struct.ScmObj*, align 8
%_95k40412 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46570)
store volatile %struct.ScmObj* %_95k40412, %struct.ScmObj** %stackaddr$prim47601, align 8
%stackaddr$prim47602 = alloca %struct.ScmObj*, align 8
%current_45args46571 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46570)
store volatile %struct.ScmObj* %current_45args46571, %struct.ScmObj** %stackaddr$prim47602, align 8
%stackaddr$prim47603 = alloca %struct.ScmObj*, align 8
%anf_45bind40329 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46571)
store volatile %struct.ScmObj* %anf_45bind40329, %struct.ScmObj** %stackaddr$prim47603, align 8
%stackaddr$prim47604 = alloca %struct.ScmObj*, align 8
%anf_45bind40330 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40168)
store volatile %struct.ScmObj* %anf_45bind40330, %struct.ScmObj** %stackaddr$prim47604, align 8
%stackaddr$prim47605 = alloca %struct.ScmObj*, align 8
%anf_45bind40331 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40168)
store volatile %struct.ScmObj* %anf_45bind40331, %struct.ScmObj** %stackaddr$prim47605, align 8
%args46573$_37foldl140107$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47606 = alloca %struct.ScmObj*, align 8
%args46573$_37foldl140107$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40331, %struct.ScmObj* %args46573$_37foldl140107$0)
store volatile %struct.ScmObj* %args46573$_37foldl140107$1, %struct.ScmObj** %stackaddr$prim47606, align 8
%stackaddr$prim47607 = alloca %struct.ScmObj*, align 8
%args46573$_37foldl140107$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40330, %struct.ScmObj* %args46573$_37foldl140107$1)
store volatile %struct.ScmObj* %args46573$_37foldl140107$2, %struct.ScmObj** %stackaddr$prim47607, align 8
%stackaddr$prim47608 = alloca %struct.ScmObj*, align 8
%args46573$_37foldl140107$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40329, %struct.ScmObj* %args46573$_37foldl140107$2)
store volatile %struct.ScmObj* %args46573$_37foldl140107$3, %struct.ScmObj** %stackaddr$prim47608, align 8
%stackaddr$prim47609 = alloca %struct.ScmObj*, align 8
%args46573$_37foldl140107$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40410, %struct.ScmObj* %args46573$_37foldl140107$3)
store volatile %struct.ScmObj* %args46573$_37foldl140107$4, %struct.ScmObj** %stackaddr$prim47609, align 8
%clofunc47610 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140107)
musttail call tailcc void %clofunc47610(%struct.ScmObj* %_37foldl140107, %struct.ScmObj* %args46573$_37foldl140107$4)
ret void
}

define tailcc void @proc_clo$ae43743(%struct.ScmObj* %env$ae43743,%struct.ScmObj* %current_45args46574) {
%stackaddr$prim47611 = alloca %struct.ScmObj*, align 8
%k40413 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46574)
store volatile %struct.ScmObj* %k40413, %struct.ScmObj** %stackaddr$prim47611, align 8
%stackaddr$prim47612 = alloca %struct.ScmObj*, align 8
%current_45args46575 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46574)
store volatile %struct.ScmObj* %current_45args46575, %struct.ScmObj** %stackaddr$prim47612, align 8
%stackaddr$prim47613 = alloca %struct.ScmObj*, align 8
%n40170 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46575)
store volatile %struct.ScmObj* %n40170, %struct.ScmObj** %stackaddr$prim47613, align 8
%stackaddr$prim47614 = alloca %struct.ScmObj*, align 8
%current_45args46576 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46575)
store volatile %struct.ScmObj* %current_45args46576, %struct.ScmObj** %stackaddr$prim47614, align 8
%stackaddr$prim47615 = alloca %struct.ScmObj*, align 8
%v40169 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46576)
store volatile %struct.ScmObj* %v40169, %struct.ScmObj** %stackaddr$prim47615, align 8
%stackaddr$prim47616 = alloca %struct.ScmObj*, align 8
%cpsprim40414 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v40169, %struct.ScmObj* %n40170)
store volatile %struct.ScmObj* %cpsprim40414, %struct.ScmObj** %stackaddr$prim47616, align 8
%ae43747 = call %struct.ScmObj* @const_init_int(i64 0)
%args46578$k40413$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47617 = alloca %struct.ScmObj*, align 8
%args46578$k40413$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40414, %struct.ScmObj* %args46578$k40413$0)
store volatile %struct.ScmObj* %args46578$k40413$1, %struct.ScmObj** %stackaddr$prim47617, align 8
%stackaddr$prim47618 = alloca %struct.ScmObj*, align 8
%args46578$k40413$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43747, %struct.ScmObj* %args46578$k40413$1)
store volatile %struct.ScmObj* %args46578$k40413$2, %struct.ScmObj** %stackaddr$prim47618, align 8
%clofunc47619 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40413)
musttail call tailcc void %clofunc47619(%struct.ScmObj* %k40413, %struct.ScmObj* %args46578$k40413$2)
ret void
}

define tailcc void @proc_clo$ae43313(%struct.ScmObj* %env$ae43313,%struct.ScmObj* %current_45args46581) {
%stackaddr$prim47620 = alloca %struct.ScmObj*, align 8
%k40415 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46581)
store volatile %struct.ScmObj* %k40415, %struct.ScmObj** %stackaddr$prim47620, align 8
%stackaddr$prim47621 = alloca %struct.ScmObj*, align 8
%current_45args46582 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46581)
store volatile %struct.ScmObj* %current_45args46582, %struct.ScmObj** %stackaddr$prim47621, align 8
%stackaddr$prim47622 = alloca %struct.ScmObj*, align 8
%v40173 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46582)
store volatile %struct.ScmObj* %v40173, %struct.ScmObj** %stackaddr$prim47622, align 8
%stackaddr$prim47623 = alloca %struct.ScmObj*, align 8
%current_45args46583 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46582)
store volatile %struct.ScmObj* %current_45args46583, %struct.ScmObj** %stackaddr$prim47623, align 8
%stackaddr$prim47624 = alloca %struct.ScmObj*, align 8
%lst40172 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46583)
store volatile %struct.ScmObj* %lst40172, %struct.ScmObj** %stackaddr$prim47624, align 8
%ae43314 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim47625 = alloca %struct.ScmObj*, align 8
%lst40174 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae43314, %struct.ScmObj* %lst40172)
store volatile %struct.ScmObj* %lst40174, %struct.ScmObj** %stackaddr$prim47625, align 8
%stackaddr$makeclosure47626 = alloca %struct.ScmObj*, align 8
%fptrToInt47627 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43316 to i64
%ae43316 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47627)
store volatile %struct.ScmObj* %ae43316, %struct.ScmObj** %stackaddr$makeclosure47626, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43316, %struct.ScmObj* %k40415, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43316, %struct.ScmObj* %lst40174, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43316, %struct.ScmObj* %v40173, i64 2)
%ae43317 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47628 = alloca %struct.ScmObj*, align 8
%fptrToInt47629 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43318 to i64
%ae43318 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47629)
store volatile %struct.ScmObj* %ae43318, %struct.ScmObj** %stackaddr$makeclosure47628, align 8
%args46605$ae43316$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47630 = alloca %struct.ScmObj*, align 8
%args46605$ae43316$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43318, %struct.ScmObj* %args46605$ae43316$0)
store volatile %struct.ScmObj* %args46605$ae43316$1, %struct.ScmObj** %stackaddr$prim47630, align 8
%stackaddr$prim47631 = alloca %struct.ScmObj*, align 8
%args46605$ae43316$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43317, %struct.ScmObj* %args46605$ae43316$1)
store volatile %struct.ScmObj* %args46605$ae43316$2, %struct.ScmObj** %stackaddr$prim47631, align 8
%clofunc47632 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43316)
musttail call tailcc void %clofunc47632(%struct.ScmObj* %ae43316, %struct.ScmObj* %args46605$ae43316$2)
ret void
}

define tailcc void @proc_clo$ae43316(%struct.ScmObj* %env$ae43316,%struct.ScmObj* %current_45args46585) {
%stackaddr$env-ref47633 = alloca %struct.ScmObj*, align 8
%k40415 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43316, i64 0)
store %struct.ScmObj* %k40415, %struct.ScmObj** %stackaddr$env-ref47633
%stackaddr$env-ref47634 = alloca %struct.ScmObj*, align 8
%lst40174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43316, i64 1)
store %struct.ScmObj* %lst40174, %struct.ScmObj** %stackaddr$env-ref47634
%stackaddr$env-ref47635 = alloca %struct.ScmObj*, align 8
%v40173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43316, i64 2)
store %struct.ScmObj* %v40173, %struct.ScmObj** %stackaddr$env-ref47635
%stackaddr$prim47636 = alloca %struct.ScmObj*, align 8
%_95k40416 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46585)
store volatile %struct.ScmObj* %_95k40416, %struct.ScmObj** %stackaddr$prim47636, align 8
%stackaddr$prim47637 = alloca %struct.ScmObj*, align 8
%current_45args46586 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46585)
store volatile %struct.ScmObj* %current_45args46586, %struct.ScmObj** %stackaddr$prim47637, align 8
%stackaddr$prim47638 = alloca %struct.ScmObj*, align 8
%anf_45bind40318 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46586)
store volatile %struct.ScmObj* %anf_45bind40318, %struct.ScmObj** %stackaddr$prim47638, align 8
%stackaddr$makeclosure47639 = alloca %struct.ScmObj*, align 8
%fptrToInt47640 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43332 to i64
%ae43332 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47640)
store volatile %struct.ScmObj* %ae43332, %struct.ScmObj** %stackaddr$makeclosure47639, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43332, %struct.ScmObj* %k40415, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43332, %struct.ScmObj* %lst40174, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43332, %struct.ScmObj* %v40173, i64 2)
%stackaddr$makeclosure47641 = alloca %struct.ScmObj*, align 8
%fptrToInt47642 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43333 to i64
%ae43333 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47642)
store volatile %struct.ScmObj* %ae43333, %struct.ScmObj** %stackaddr$makeclosure47641, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43333, %struct.ScmObj* %k40415, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43333, %struct.ScmObj* %lst40174, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43333, %struct.ScmObj* %v40173, i64 2)
%args46600$anf_45bind40318$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47643 = alloca %struct.ScmObj*, align 8
%args46600$anf_45bind40318$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43333, %struct.ScmObj* %args46600$anf_45bind40318$0)
store volatile %struct.ScmObj* %args46600$anf_45bind40318$1, %struct.ScmObj** %stackaddr$prim47643, align 8
%stackaddr$prim47644 = alloca %struct.ScmObj*, align 8
%args46600$anf_45bind40318$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43332, %struct.ScmObj* %args46600$anf_45bind40318$1)
store volatile %struct.ScmObj* %args46600$anf_45bind40318$2, %struct.ScmObj** %stackaddr$prim47644, align 8
%clofunc47645 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40318)
musttail call tailcc void %clofunc47645(%struct.ScmObj* %anf_45bind40318, %struct.ScmObj* %args46600$anf_45bind40318$2)
ret void
}

define tailcc void @proc_clo$ae43332(%struct.ScmObj* %env$ae43332,%struct.ScmObj* %current_45args46588) {
%stackaddr$env-ref47646 = alloca %struct.ScmObj*, align 8
%k40415 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43332, i64 0)
store %struct.ScmObj* %k40415, %struct.ScmObj** %stackaddr$env-ref47646
%stackaddr$env-ref47647 = alloca %struct.ScmObj*, align 8
%lst40174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43332, i64 1)
store %struct.ScmObj* %lst40174, %struct.ScmObj** %stackaddr$env-ref47647
%stackaddr$env-ref47648 = alloca %struct.ScmObj*, align 8
%v40173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43332, i64 2)
store %struct.ScmObj* %v40173, %struct.ScmObj** %stackaddr$env-ref47648
%stackaddr$prim47649 = alloca %struct.ScmObj*, align 8
%_95k40417 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46588)
store volatile %struct.ScmObj* %_95k40417, %struct.ScmObj** %stackaddr$prim47649, align 8
%stackaddr$prim47650 = alloca %struct.ScmObj*, align 8
%current_45args46589 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46588)
store volatile %struct.ScmObj* %current_45args46589, %struct.ScmObj** %stackaddr$prim47650, align 8
%stackaddr$prim47651 = alloca %struct.ScmObj*, align 8
%cc40175 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46589)
store volatile %struct.ScmObj* %cc40175, %struct.ScmObj** %stackaddr$prim47651, align 8
%ae43441 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47652 = alloca %struct.ScmObj*, align 8
%anf_45bind40319 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43441)
store volatile %struct.ScmObj* %anf_45bind40319, %struct.ScmObj** %stackaddr$prim47652, align 8
%stackaddr$prim47653 = alloca %struct.ScmObj*, align 8
%anf_45bind40320 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40319)
store volatile %struct.ScmObj* %anf_45bind40320, %struct.ScmObj** %stackaddr$prim47653, align 8
%truthy$cmp47654 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40320)
%cmp$cmp47654 = icmp eq i64 %truthy$cmp47654, 1
br i1 %cmp$cmp47654, label %truebranch$cmp47654, label %falsebranch$cmp47654
truebranch$cmp47654:
%ae43445 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43446 = call %struct.ScmObj* @const_init_false()
%args46591$k40415$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47655 = alloca %struct.ScmObj*, align 8
%args46591$k40415$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43446, %struct.ScmObj* %args46591$k40415$0)
store volatile %struct.ScmObj* %args46591$k40415$1, %struct.ScmObj** %stackaddr$prim47655, align 8
%stackaddr$prim47656 = alloca %struct.ScmObj*, align 8
%args46591$k40415$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43445, %struct.ScmObj* %args46591$k40415$1)
store volatile %struct.ScmObj* %args46591$k40415$2, %struct.ScmObj** %stackaddr$prim47656, align 8
%clofunc47657 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40415)
musttail call tailcc void %clofunc47657(%struct.ScmObj* %k40415, %struct.ScmObj* %args46591$k40415$2)
ret void
falsebranch$cmp47654:
%ae43454 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47658 = alloca %struct.ScmObj*, align 8
%anf_45bind40321 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43454)
store volatile %struct.ScmObj* %anf_45bind40321, %struct.ScmObj** %stackaddr$prim47658, align 8
%stackaddr$prim47659 = alloca %struct.ScmObj*, align 8
%anf_45bind40322 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40321)
store volatile %struct.ScmObj* %anf_45bind40322, %struct.ScmObj** %stackaddr$prim47659, align 8
%stackaddr$prim47660 = alloca %struct.ScmObj*, align 8
%anf_45bind40323 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind40322, %struct.ScmObj* %v40173)
store volatile %struct.ScmObj* %anf_45bind40323, %struct.ScmObj** %stackaddr$prim47660, align 8
%truthy$cmp47661 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40323)
%cmp$cmp47661 = icmp eq i64 %truthy$cmp47661, 1
br i1 %cmp$cmp47661, label %truebranch$cmp47661, label %falsebranch$cmp47661
truebranch$cmp47661:
%ae43460 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47662 = alloca %struct.ScmObj*, align 8
%cpsprim40418 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43460)
store volatile %struct.ScmObj* %cpsprim40418, %struct.ScmObj** %stackaddr$prim47662, align 8
%ae43462 = call %struct.ScmObj* @const_init_int(i64 0)
%args46592$k40415$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47663 = alloca %struct.ScmObj*, align 8
%args46592$k40415$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40418, %struct.ScmObj* %args46592$k40415$0)
store volatile %struct.ScmObj* %args46592$k40415$1, %struct.ScmObj** %stackaddr$prim47663, align 8
%stackaddr$prim47664 = alloca %struct.ScmObj*, align 8
%args46592$k40415$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43462, %struct.ScmObj* %args46592$k40415$1)
store volatile %struct.ScmObj* %args46592$k40415$2, %struct.ScmObj** %stackaddr$prim47664, align 8
%clofunc47665 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40415)
musttail call tailcc void %clofunc47665(%struct.ScmObj* %k40415, %struct.ScmObj* %args46592$k40415$2)
ret void
falsebranch$cmp47661:
%ae43473 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47666 = alloca %struct.ScmObj*, align 8
%anf_45bind40324 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43473)
store volatile %struct.ScmObj* %anf_45bind40324, %struct.ScmObj** %stackaddr$prim47666, align 8
%stackaddr$prim47667 = alloca %struct.ScmObj*, align 8
%anf_45bind40325 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40324)
store volatile %struct.ScmObj* %anf_45bind40325, %struct.ScmObj** %stackaddr$prim47667, align 8
%ae43476 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47668 = alloca %struct.ScmObj*, align 8
%_95040177 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43476, %struct.ScmObj* %anf_45bind40325)
store volatile %struct.ScmObj* %_95040177, %struct.ScmObj** %stackaddr$prim47668, align 8
%args46593$cc40175$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47669 = alloca %struct.ScmObj*, align 8
%args46593$cc40175$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40175, %struct.ScmObj* %args46593$cc40175$0)
store volatile %struct.ScmObj* %args46593$cc40175$1, %struct.ScmObj** %stackaddr$prim47669, align 8
%stackaddr$prim47670 = alloca %struct.ScmObj*, align 8
%args46593$cc40175$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40415, %struct.ScmObj* %args46593$cc40175$1)
store volatile %struct.ScmObj* %args46593$cc40175$2, %struct.ScmObj** %stackaddr$prim47670, align 8
%clofunc47671 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40175)
musttail call tailcc void %clofunc47671(%struct.ScmObj* %cc40175, %struct.ScmObj* %args46593$cc40175$2)
ret void
}

define tailcc void @proc_clo$ae43333(%struct.ScmObj* %env$ae43333,%struct.ScmObj* %current_45args46594) {
%stackaddr$env-ref47672 = alloca %struct.ScmObj*, align 8
%k40415 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43333, i64 0)
store %struct.ScmObj* %k40415, %struct.ScmObj** %stackaddr$env-ref47672
%stackaddr$env-ref47673 = alloca %struct.ScmObj*, align 8
%lst40174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43333, i64 1)
store %struct.ScmObj* %lst40174, %struct.ScmObj** %stackaddr$env-ref47673
%stackaddr$env-ref47674 = alloca %struct.ScmObj*, align 8
%v40173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43333, i64 2)
store %struct.ScmObj* %v40173, %struct.ScmObj** %stackaddr$env-ref47674
%stackaddr$prim47675 = alloca %struct.ScmObj*, align 8
%_95k40417 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46594)
store volatile %struct.ScmObj* %_95k40417, %struct.ScmObj** %stackaddr$prim47675, align 8
%stackaddr$prim47676 = alloca %struct.ScmObj*, align 8
%current_45args46595 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46594)
store volatile %struct.ScmObj* %current_45args46595, %struct.ScmObj** %stackaddr$prim47676, align 8
%stackaddr$prim47677 = alloca %struct.ScmObj*, align 8
%cc40175 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46595)
store volatile %struct.ScmObj* %cc40175, %struct.ScmObj** %stackaddr$prim47677, align 8
%ae43335 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47678 = alloca %struct.ScmObj*, align 8
%anf_45bind40319 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43335)
store volatile %struct.ScmObj* %anf_45bind40319, %struct.ScmObj** %stackaddr$prim47678, align 8
%stackaddr$prim47679 = alloca %struct.ScmObj*, align 8
%anf_45bind40320 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40319)
store volatile %struct.ScmObj* %anf_45bind40320, %struct.ScmObj** %stackaddr$prim47679, align 8
%truthy$cmp47680 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40320)
%cmp$cmp47680 = icmp eq i64 %truthy$cmp47680, 1
br i1 %cmp$cmp47680, label %truebranch$cmp47680, label %falsebranch$cmp47680
truebranch$cmp47680:
%ae43339 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43340 = call %struct.ScmObj* @const_init_false()
%args46597$k40415$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47681 = alloca %struct.ScmObj*, align 8
%args46597$k40415$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43340, %struct.ScmObj* %args46597$k40415$0)
store volatile %struct.ScmObj* %args46597$k40415$1, %struct.ScmObj** %stackaddr$prim47681, align 8
%stackaddr$prim47682 = alloca %struct.ScmObj*, align 8
%args46597$k40415$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43339, %struct.ScmObj* %args46597$k40415$1)
store volatile %struct.ScmObj* %args46597$k40415$2, %struct.ScmObj** %stackaddr$prim47682, align 8
%clofunc47683 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40415)
musttail call tailcc void %clofunc47683(%struct.ScmObj* %k40415, %struct.ScmObj* %args46597$k40415$2)
ret void
falsebranch$cmp47680:
%ae43348 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47684 = alloca %struct.ScmObj*, align 8
%anf_45bind40321 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43348)
store volatile %struct.ScmObj* %anf_45bind40321, %struct.ScmObj** %stackaddr$prim47684, align 8
%stackaddr$prim47685 = alloca %struct.ScmObj*, align 8
%anf_45bind40322 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40321)
store volatile %struct.ScmObj* %anf_45bind40322, %struct.ScmObj** %stackaddr$prim47685, align 8
%stackaddr$prim47686 = alloca %struct.ScmObj*, align 8
%anf_45bind40323 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind40322, %struct.ScmObj* %v40173)
store volatile %struct.ScmObj* %anf_45bind40323, %struct.ScmObj** %stackaddr$prim47686, align 8
%truthy$cmp47687 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40323)
%cmp$cmp47687 = icmp eq i64 %truthy$cmp47687, 1
br i1 %cmp$cmp47687, label %truebranch$cmp47687, label %falsebranch$cmp47687
truebranch$cmp47687:
%ae43354 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47688 = alloca %struct.ScmObj*, align 8
%cpsprim40418 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43354)
store volatile %struct.ScmObj* %cpsprim40418, %struct.ScmObj** %stackaddr$prim47688, align 8
%ae43356 = call %struct.ScmObj* @const_init_int(i64 0)
%args46598$k40415$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47689 = alloca %struct.ScmObj*, align 8
%args46598$k40415$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40418, %struct.ScmObj* %args46598$k40415$0)
store volatile %struct.ScmObj* %args46598$k40415$1, %struct.ScmObj** %stackaddr$prim47689, align 8
%stackaddr$prim47690 = alloca %struct.ScmObj*, align 8
%args46598$k40415$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43356, %struct.ScmObj* %args46598$k40415$1)
store volatile %struct.ScmObj* %args46598$k40415$2, %struct.ScmObj** %stackaddr$prim47690, align 8
%clofunc47691 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40415)
musttail call tailcc void %clofunc47691(%struct.ScmObj* %k40415, %struct.ScmObj* %args46598$k40415$2)
ret void
falsebranch$cmp47687:
%ae43367 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47692 = alloca %struct.ScmObj*, align 8
%anf_45bind40324 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43367)
store volatile %struct.ScmObj* %anf_45bind40324, %struct.ScmObj** %stackaddr$prim47692, align 8
%stackaddr$prim47693 = alloca %struct.ScmObj*, align 8
%anf_45bind40325 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40324)
store volatile %struct.ScmObj* %anf_45bind40325, %struct.ScmObj** %stackaddr$prim47693, align 8
%ae43370 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47694 = alloca %struct.ScmObj*, align 8
%_95040177 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43370, %struct.ScmObj* %anf_45bind40325)
store volatile %struct.ScmObj* %_95040177, %struct.ScmObj** %stackaddr$prim47694, align 8
%args46599$cc40175$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47695 = alloca %struct.ScmObj*, align 8
%args46599$cc40175$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40175, %struct.ScmObj* %args46599$cc40175$0)
store volatile %struct.ScmObj* %args46599$cc40175$1, %struct.ScmObj** %stackaddr$prim47695, align 8
%stackaddr$prim47696 = alloca %struct.ScmObj*, align 8
%args46599$cc40175$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40415, %struct.ScmObj* %args46599$cc40175$1)
store volatile %struct.ScmObj* %args46599$cc40175$2, %struct.ScmObj** %stackaddr$prim47696, align 8
%clofunc47697 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40175)
musttail call tailcc void %clofunc47697(%struct.ScmObj* %cc40175, %struct.ScmObj* %args46599$cc40175$2)
ret void
}

define tailcc void @proc_clo$ae43318(%struct.ScmObj* %env$ae43318,%struct.ScmObj* %current_45args46601) {
%stackaddr$prim47698 = alloca %struct.ScmObj*, align 8
%k40419 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46601)
store volatile %struct.ScmObj* %k40419, %struct.ScmObj** %stackaddr$prim47698, align 8
%stackaddr$prim47699 = alloca %struct.ScmObj*, align 8
%current_45args46602 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46601)
store volatile %struct.ScmObj* %current_45args46602, %struct.ScmObj** %stackaddr$prim47699, align 8
%stackaddr$prim47700 = alloca %struct.ScmObj*, align 8
%u40176 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46602)
store volatile %struct.ScmObj* %u40176, %struct.ScmObj** %stackaddr$prim47700, align 8
%args46604$u40176$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47701 = alloca %struct.ScmObj*, align 8
%args46604$u40176$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u40176, %struct.ScmObj* %args46604$u40176$0)
store volatile %struct.ScmObj* %args46604$u40176$1, %struct.ScmObj** %stackaddr$prim47701, align 8
%stackaddr$prim47702 = alloca %struct.ScmObj*, align 8
%args46604$u40176$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40419, %struct.ScmObj* %args46604$u40176$1)
store volatile %struct.ScmObj* %args46604$u40176$2, %struct.ScmObj** %stackaddr$prim47702, align 8
%clofunc47703 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u40176)
musttail call tailcc void %clofunc47703(%struct.ScmObj* %u40176, %struct.ScmObj* %args46604$u40176$2)
ret void
}

define tailcc void @proc_clo$ae42777(%struct.ScmObj* %env$ae42777,%struct.ScmObj* %current_45args46607) {
%stackaddr$prim47704 = alloca %struct.ScmObj*, align 8
%k40420 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46607)
store volatile %struct.ScmObj* %k40420, %struct.ScmObj** %stackaddr$prim47704, align 8
%stackaddr$prim47705 = alloca %struct.ScmObj*, align 8
%current_45args46608 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46607)
store volatile %struct.ScmObj* %current_45args46608, %struct.ScmObj** %stackaddr$prim47705, align 8
%stackaddr$prim47706 = alloca %struct.ScmObj*, align 8
%lst40180 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46608)
store volatile %struct.ScmObj* %lst40180, %struct.ScmObj** %stackaddr$prim47706, align 8
%stackaddr$prim47707 = alloca %struct.ScmObj*, align 8
%current_45args46609 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46608)
store volatile %struct.ScmObj* %current_45args46609, %struct.ScmObj** %stackaddr$prim47707, align 8
%stackaddr$prim47708 = alloca %struct.ScmObj*, align 8
%n40179 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46609)
store volatile %struct.ScmObj* %n40179, %struct.ScmObj** %stackaddr$prim47708, align 8
%ae42778 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim47709 = alloca %struct.ScmObj*, align 8
%n40182 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42778, %struct.ScmObj* %n40179)
store volatile %struct.ScmObj* %n40182, %struct.ScmObj** %stackaddr$prim47709, align 8
%ae42780 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim47710 = alloca %struct.ScmObj*, align 8
%lst40181 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42780, %struct.ScmObj* %lst40180)
store volatile %struct.ScmObj* %lst40181, %struct.ScmObj** %stackaddr$prim47710, align 8
%stackaddr$makeclosure47711 = alloca %struct.ScmObj*, align 8
%fptrToInt47712 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42782 to i64
%ae42782 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47712)
store volatile %struct.ScmObj* %ae42782, %struct.ScmObj** %stackaddr$makeclosure47711, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42782, %struct.ScmObj* %n40182, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42782, %struct.ScmObj* %lst40181, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42782, %struct.ScmObj* %k40420, i64 2)
%ae42783 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47713 = alloca %struct.ScmObj*, align 8
%fptrToInt47714 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42784 to i64
%ae42784 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47714)
store volatile %struct.ScmObj* %ae42784, %struct.ScmObj** %stackaddr$makeclosure47713, align 8
%args46629$ae42782$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47715 = alloca %struct.ScmObj*, align 8
%args46629$ae42782$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42784, %struct.ScmObj* %args46629$ae42782$0)
store volatile %struct.ScmObj* %args46629$ae42782$1, %struct.ScmObj** %stackaddr$prim47715, align 8
%stackaddr$prim47716 = alloca %struct.ScmObj*, align 8
%args46629$ae42782$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42783, %struct.ScmObj* %args46629$ae42782$1)
store volatile %struct.ScmObj* %args46629$ae42782$2, %struct.ScmObj** %stackaddr$prim47716, align 8
%clofunc47717 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42782)
musttail call tailcc void %clofunc47717(%struct.ScmObj* %ae42782, %struct.ScmObj* %args46629$ae42782$2)
ret void
}

define tailcc void @proc_clo$ae42782(%struct.ScmObj* %env$ae42782,%struct.ScmObj* %current_45args46611) {
%stackaddr$env-ref47718 = alloca %struct.ScmObj*, align 8
%n40182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42782, i64 0)
store %struct.ScmObj* %n40182, %struct.ScmObj** %stackaddr$env-ref47718
%stackaddr$env-ref47719 = alloca %struct.ScmObj*, align 8
%lst40181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42782, i64 1)
store %struct.ScmObj* %lst40181, %struct.ScmObj** %stackaddr$env-ref47719
%stackaddr$env-ref47720 = alloca %struct.ScmObj*, align 8
%k40420 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42782, i64 2)
store %struct.ScmObj* %k40420, %struct.ScmObj** %stackaddr$env-ref47720
%stackaddr$prim47721 = alloca %struct.ScmObj*, align 8
%_95k40421 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46611)
store volatile %struct.ScmObj* %_95k40421, %struct.ScmObj** %stackaddr$prim47721, align 8
%stackaddr$prim47722 = alloca %struct.ScmObj*, align 8
%current_45args46612 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46611)
store volatile %struct.ScmObj* %current_45args46612, %struct.ScmObj** %stackaddr$prim47722, align 8
%stackaddr$prim47723 = alloca %struct.ScmObj*, align 8
%anf_45bind40311 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46612)
store volatile %struct.ScmObj* %anf_45bind40311, %struct.ScmObj** %stackaddr$prim47723, align 8
%stackaddr$makeclosure47724 = alloca %struct.ScmObj*, align 8
%fptrToInt47725 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42798 to i64
%ae42798 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47725)
store volatile %struct.ScmObj* %ae42798, %struct.ScmObj** %stackaddr$makeclosure47724, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42798, %struct.ScmObj* %n40182, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42798, %struct.ScmObj* %lst40181, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42798, %struct.ScmObj* %k40420, i64 2)
%stackaddr$makeclosure47726 = alloca %struct.ScmObj*, align 8
%fptrToInt47727 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42799 to i64
%ae42799 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47727)
store volatile %struct.ScmObj* %ae42799, %struct.ScmObj** %stackaddr$makeclosure47726, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42799, %struct.ScmObj* %n40182, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42799, %struct.ScmObj* %lst40181, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42799, %struct.ScmObj* %k40420, i64 2)
%args46624$anf_45bind40311$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47728 = alloca %struct.ScmObj*, align 8
%args46624$anf_45bind40311$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42799, %struct.ScmObj* %args46624$anf_45bind40311$0)
store volatile %struct.ScmObj* %args46624$anf_45bind40311$1, %struct.ScmObj** %stackaddr$prim47728, align 8
%stackaddr$prim47729 = alloca %struct.ScmObj*, align 8
%args46624$anf_45bind40311$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42798, %struct.ScmObj* %args46624$anf_45bind40311$1)
store volatile %struct.ScmObj* %args46624$anf_45bind40311$2, %struct.ScmObj** %stackaddr$prim47729, align 8
%clofunc47730 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40311)
musttail call tailcc void %clofunc47730(%struct.ScmObj* %anf_45bind40311, %struct.ScmObj* %args46624$anf_45bind40311$2)
ret void
}

define tailcc void @proc_clo$ae42798(%struct.ScmObj* %env$ae42798,%struct.ScmObj* %current_45args46614) {
%stackaddr$env-ref47731 = alloca %struct.ScmObj*, align 8
%n40182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42798, i64 0)
store %struct.ScmObj* %n40182, %struct.ScmObj** %stackaddr$env-ref47731
%stackaddr$env-ref47732 = alloca %struct.ScmObj*, align 8
%lst40181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42798, i64 1)
store %struct.ScmObj* %lst40181, %struct.ScmObj** %stackaddr$env-ref47732
%stackaddr$env-ref47733 = alloca %struct.ScmObj*, align 8
%k40420 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42798, i64 2)
store %struct.ScmObj* %k40420, %struct.ScmObj** %stackaddr$env-ref47733
%stackaddr$prim47734 = alloca %struct.ScmObj*, align 8
%_95k40422 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46614)
store volatile %struct.ScmObj* %_95k40422, %struct.ScmObj** %stackaddr$prim47734, align 8
%stackaddr$prim47735 = alloca %struct.ScmObj*, align 8
%current_45args46615 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46614)
store volatile %struct.ScmObj* %current_45args46615, %struct.ScmObj** %stackaddr$prim47735, align 8
%stackaddr$prim47736 = alloca %struct.ScmObj*, align 8
%cc40183 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46615)
store volatile %struct.ScmObj* %cc40183, %struct.ScmObj** %stackaddr$prim47736, align 8
%ae42941 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47737 = alloca %struct.ScmObj*, align 8
%anf_45bind40312 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40182, %struct.ScmObj* %ae42941)
store volatile %struct.ScmObj* %anf_45bind40312, %struct.ScmObj** %stackaddr$prim47737, align 8
%ae42942 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47738 = alloca %struct.ScmObj*, align 8
%anf_45bind40313 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae42942, %struct.ScmObj* %anf_45bind40312)
store volatile %struct.ScmObj* %anf_45bind40313, %struct.ScmObj** %stackaddr$prim47738, align 8
%truthy$cmp47739 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40313)
%cmp$cmp47739 = icmp eq i64 %truthy$cmp47739, 1
br i1 %cmp$cmp47739, label %truebranch$cmp47739, label %falsebranch$cmp47739
truebranch$cmp47739:
%ae42946 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47740 = alloca %struct.ScmObj*, align 8
%cpsprim40423 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40181, %struct.ScmObj* %ae42946)
store volatile %struct.ScmObj* %cpsprim40423, %struct.ScmObj** %stackaddr$prim47740, align 8
%ae42948 = call %struct.ScmObj* @const_init_int(i64 0)
%args46617$k40420$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47741 = alloca %struct.ScmObj*, align 8
%args46617$k40420$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40423, %struct.ScmObj* %args46617$k40420$0)
store volatile %struct.ScmObj* %args46617$k40420$1, %struct.ScmObj** %stackaddr$prim47741, align 8
%stackaddr$prim47742 = alloca %struct.ScmObj*, align 8
%args46617$k40420$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42948, %struct.ScmObj* %args46617$k40420$1)
store volatile %struct.ScmObj* %args46617$k40420$2, %struct.ScmObj** %stackaddr$prim47742, align 8
%clofunc47743 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40420)
musttail call tailcc void %clofunc47743(%struct.ScmObj* %k40420, %struct.ScmObj* %args46617$k40420$2)
ret void
falsebranch$cmp47739:
%ae42959 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47744 = alloca %struct.ScmObj*, align 8
%anf_45bind40314 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40181, %struct.ScmObj* %ae42959)
store volatile %struct.ScmObj* %anf_45bind40314, %struct.ScmObj** %stackaddr$prim47744, align 8
%stackaddr$prim47745 = alloca %struct.ScmObj*, align 8
%anf_45bind40315 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40314)
store volatile %struct.ScmObj* %anf_45bind40315, %struct.ScmObj** %stackaddr$prim47745, align 8
%ae42962 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47746 = alloca %struct.ScmObj*, align 8
%_95040186 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40181, %struct.ScmObj* %ae42962, %struct.ScmObj* %anf_45bind40315)
store volatile %struct.ScmObj* %_95040186, %struct.ScmObj** %stackaddr$prim47746, align 8
%ae42965 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47747 = alloca %struct.ScmObj*, align 8
%anf_45bind40316 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40182, %struct.ScmObj* %ae42965)
store volatile %struct.ScmObj* %anf_45bind40316, %struct.ScmObj** %stackaddr$prim47747, align 8
%ae42967 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim47748 = alloca %struct.ScmObj*, align 8
%anf_45bind40317 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind40316, %struct.ScmObj* %ae42967)
store volatile %struct.ScmObj* %anf_45bind40317, %struct.ScmObj** %stackaddr$prim47748, align 8
%ae42969 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47749 = alloca %struct.ScmObj*, align 8
%_95140185 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n40182, %struct.ScmObj* %ae42969, %struct.ScmObj* %anf_45bind40317)
store volatile %struct.ScmObj* %_95140185, %struct.ScmObj** %stackaddr$prim47749, align 8
%args46618$cc40183$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47750 = alloca %struct.ScmObj*, align 8
%args46618$cc40183$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40183, %struct.ScmObj* %args46618$cc40183$0)
store volatile %struct.ScmObj* %args46618$cc40183$1, %struct.ScmObj** %stackaddr$prim47750, align 8
%stackaddr$prim47751 = alloca %struct.ScmObj*, align 8
%args46618$cc40183$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40420, %struct.ScmObj* %args46618$cc40183$1)
store volatile %struct.ScmObj* %args46618$cc40183$2, %struct.ScmObj** %stackaddr$prim47751, align 8
%clofunc47752 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40183)
musttail call tailcc void %clofunc47752(%struct.ScmObj* %cc40183, %struct.ScmObj* %args46618$cc40183$2)
ret void
}

define tailcc void @proc_clo$ae42799(%struct.ScmObj* %env$ae42799,%struct.ScmObj* %current_45args46619) {
%stackaddr$env-ref47753 = alloca %struct.ScmObj*, align 8
%n40182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42799, i64 0)
store %struct.ScmObj* %n40182, %struct.ScmObj** %stackaddr$env-ref47753
%stackaddr$env-ref47754 = alloca %struct.ScmObj*, align 8
%lst40181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42799, i64 1)
store %struct.ScmObj* %lst40181, %struct.ScmObj** %stackaddr$env-ref47754
%stackaddr$env-ref47755 = alloca %struct.ScmObj*, align 8
%k40420 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42799, i64 2)
store %struct.ScmObj* %k40420, %struct.ScmObj** %stackaddr$env-ref47755
%stackaddr$prim47756 = alloca %struct.ScmObj*, align 8
%_95k40422 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46619)
store volatile %struct.ScmObj* %_95k40422, %struct.ScmObj** %stackaddr$prim47756, align 8
%stackaddr$prim47757 = alloca %struct.ScmObj*, align 8
%current_45args46620 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46619)
store volatile %struct.ScmObj* %current_45args46620, %struct.ScmObj** %stackaddr$prim47757, align 8
%stackaddr$prim47758 = alloca %struct.ScmObj*, align 8
%cc40183 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46620)
store volatile %struct.ScmObj* %cc40183, %struct.ScmObj** %stackaddr$prim47758, align 8
%ae42801 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47759 = alloca %struct.ScmObj*, align 8
%anf_45bind40312 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40182, %struct.ScmObj* %ae42801)
store volatile %struct.ScmObj* %anf_45bind40312, %struct.ScmObj** %stackaddr$prim47759, align 8
%ae42802 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47760 = alloca %struct.ScmObj*, align 8
%anf_45bind40313 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae42802, %struct.ScmObj* %anf_45bind40312)
store volatile %struct.ScmObj* %anf_45bind40313, %struct.ScmObj** %stackaddr$prim47760, align 8
%truthy$cmp47761 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40313)
%cmp$cmp47761 = icmp eq i64 %truthy$cmp47761, 1
br i1 %cmp$cmp47761, label %truebranch$cmp47761, label %falsebranch$cmp47761
truebranch$cmp47761:
%ae42806 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47762 = alloca %struct.ScmObj*, align 8
%cpsprim40423 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40181, %struct.ScmObj* %ae42806)
store volatile %struct.ScmObj* %cpsprim40423, %struct.ScmObj** %stackaddr$prim47762, align 8
%ae42808 = call %struct.ScmObj* @const_init_int(i64 0)
%args46622$k40420$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47763 = alloca %struct.ScmObj*, align 8
%args46622$k40420$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40423, %struct.ScmObj* %args46622$k40420$0)
store volatile %struct.ScmObj* %args46622$k40420$1, %struct.ScmObj** %stackaddr$prim47763, align 8
%stackaddr$prim47764 = alloca %struct.ScmObj*, align 8
%args46622$k40420$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42808, %struct.ScmObj* %args46622$k40420$1)
store volatile %struct.ScmObj* %args46622$k40420$2, %struct.ScmObj** %stackaddr$prim47764, align 8
%clofunc47765 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40420)
musttail call tailcc void %clofunc47765(%struct.ScmObj* %k40420, %struct.ScmObj* %args46622$k40420$2)
ret void
falsebranch$cmp47761:
%ae42819 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47766 = alloca %struct.ScmObj*, align 8
%anf_45bind40314 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40181, %struct.ScmObj* %ae42819)
store volatile %struct.ScmObj* %anf_45bind40314, %struct.ScmObj** %stackaddr$prim47766, align 8
%stackaddr$prim47767 = alloca %struct.ScmObj*, align 8
%anf_45bind40315 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40314)
store volatile %struct.ScmObj* %anf_45bind40315, %struct.ScmObj** %stackaddr$prim47767, align 8
%ae42822 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47768 = alloca %struct.ScmObj*, align 8
%_95040186 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40181, %struct.ScmObj* %ae42822, %struct.ScmObj* %anf_45bind40315)
store volatile %struct.ScmObj* %_95040186, %struct.ScmObj** %stackaddr$prim47768, align 8
%ae42825 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47769 = alloca %struct.ScmObj*, align 8
%anf_45bind40316 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40182, %struct.ScmObj* %ae42825)
store volatile %struct.ScmObj* %anf_45bind40316, %struct.ScmObj** %stackaddr$prim47769, align 8
%ae42827 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim47770 = alloca %struct.ScmObj*, align 8
%anf_45bind40317 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind40316, %struct.ScmObj* %ae42827)
store volatile %struct.ScmObj* %anf_45bind40317, %struct.ScmObj** %stackaddr$prim47770, align 8
%ae42829 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47771 = alloca %struct.ScmObj*, align 8
%_95140185 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n40182, %struct.ScmObj* %ae42829, %struct.ScmObj* %anf_45bind40317)
store volatile %struct.ScmObj* %_95140185, %struct.ScmObj** %stackaddr$prim47771, align 8
%args46623$cc40183$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47772 = alloca %struct.ScmObj*, align 8
%args46623$cc40183$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40183, %struct.ScmObj* %args46623$cc40183$0)
store volatile %struct.ScmObj* %args46623$cc40183$1, %struct.ScmObj** %stackaddr$prim47772, align 8
%stackaddr$prim47773 = alloca %struct.ScmObj*, align 8
%args46623$cc40183$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40420, %struct.ScmObj* %args46623$cc40183$1)
store volatile %struct.ScmObj* %args46623$cc40183$2, %struct.ScmObj** %stackaddr$prim47773, align 8
%clofunc47774 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40183)
musttail call tailcc void %clofunc47774(%struct.ScmObj* %cc40183, %struct.ScmObj* %args46623$cc40183$2)
ret void
}

define tailcc void @proc_clo$ae42784(%struct.ScmObj* %env$ae42784,%struct.ScmObj* %current_45args46625) {
%stackaddr$prim47775 = alloca %struct.ScmObj*, align 8
%k40424 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46625)
store volatile %struct.ScmObj* %k40424, %struct.ScmObj** %stackaddr$prim47775, align 8
%stackaddr$prim47776 = alloca %struct.ScmObj*, align 8
%current_45args46626 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46625)
store volatile %struct.ScmObj* %current_45args46626, %struct.ScmObj** %stackaddr$prim47776, align 8
%stackaddr$prim47777 = alloca %struct.ScmObj*, align 8
%u40184 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46626)
store volatile %struct.ScmObj* %u40184, %struct.ScmObj** %stackaddr$prim47777, align 8
%args46628$u40184$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47778 = alloca %struct.ScmObj*, align 8
%args46628$u40184$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u40184, %struct.ScmObj* %args46628$u40184$0)
store volatile %struct.ScmObj* %args46628$u40184$1, %struct.ScmObj** %stackaddr$prim47778, align 8
%stackaddr$prim47779 = alloca %struct.ScmObj*, align 8
%args46628$u40184$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40424, %struct.ScmObj* %args46628$u40184$1)
store volatile %struct.ScmObj* %args46628$u40184$2, %struct.ScmObj** %stackaddr$prim47779, align 8
%clofunc47780 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u40184)
musttail call tailcc void %clofunc47780(%struct.ScmObj* %u40184, %struct.ScmObj* %args46628$u40184$2)
ret void
}

define tailcc void @proc_clo$ae42361(%struct.ScmObj* %env$ae42361,%struct.ScmObj* %current_45args46631) {
%stackaddr$prim47781 = alloca %struct.ScmObj*, align 8
%k40425 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46631)
store volatile %struct.ScmObj* %k40425, %struct.ScmObj** %stackaddr$prim47781, align 8
%stackaddr$prim47782 = alloca %struct.ScmObj*, align 8
%current_45args46632 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46631)
store volatile %struct.ScmObj* %current_45args46632, %struct.ScmObj** %stackaddr$prim47782, align 8
%stackaddr$prim47783 = alloca %struct.ScmObj*, align 8
%a40188 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46632)
store volatile %struct.ScmObj* %a40188, %struct.ScmObj** %stackaddr$prim47783, align 8
%ae42362 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim47784 = alloca %struct.ScmObj*, align 8
%a40189 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42362, %struct.ScmObj* %a40188)
store volatile %struct.ScmObj* %a40189, %struct.ScmObj** %stackaddr$prim47784, align 8
%stackaddr$makeclosure47785 = alloca %struct.ScmObj*, align 8
%fptrToInt47786 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42364 to i64
%ae42364 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47786)
store volatile %struct.ScmObj* %ae42364, %struct.ScmObj** %stackaddr$makeclosure47785, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42364, %struct.ScmObj* %a40189, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42364, %struct.ScmObj* %k40425, i64 1)
%ae42365 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47787 = alloca %struct.ScmObj*, align 8
%fptrToInt47788 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42366 to i64
%ae42366 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47788)
store volatile %struct.ScmObj* %ae42366, %struct.ScmObj** %stackaddr$makeclosure47787, align 8
%args46654$ae42364$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47789 = alloca %struct.ScmObj*, align 8
%args46654$ae42364$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42366, %struct.ScmObj* %args46654$ae42364$0)
store volatile %struct.ScmObj* %args46654$ae42364$1, %struct.ScmObj** %stackaddr$prim47789, align 8
%stackaddr$prim47790 = alloca %struct.ScmObj*, align 8
%args46654$ae42364$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42365, %struct.ScmObj* %args46654$ae42364$1)
store volatile %struct.ScmObj* %args46654$ae42364$2, %struct.ScmObj** %stackaddr$prim47790, align 8
%clofunc47791 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42364)
musttail call tailcc void %clofunc47791(%struct.ScmObj* %ae42364, %struct.ScmObj* %args46654$ae42364$2)
ret void
}

define tailcc void @proc_clo$ae42364(%struct.ScmObj* %env$ae42364,%struct.ScmObj* %current_45args46634) {
%stackaddr$env-ref47792 = alloca %struct.ScmObj*, align 8
%a40189 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42364, i64 0)
store %struct.ScmObj* %a40189, %struct.ScmObj** %stackaddr$env-ref47792
%stackaddr$env-ref47793 = alloca %struct.ScmObj*, align 8
%k40425 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42364, i64 1)
store %struct.ScmObj* %k40425, %struct.ScmObj** %stackaddr$env-ref47793
%stackaddr$prim47794 = alloca %struct.ScmObj*, align 8
%_95k40426 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46634)
store volatile %struct.ScmObj* %_95k40426, %struct.ScmObj** %stackaddr$prim47794, align 8
%stackaddr$prim47795 = alloca %struct.ScmObj*, align 8
%current_45args46635 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46634)
store volatile %struct.ScmObj* %current_45args46635, %struct.ScmObj** %stackaddr$prim47795, align 8
%stackaddr$prim47796 = alloca %struct.ScmObj*, align 8
%anf_45bind40303 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46635)
store volatile %struct.ScmObj* %anf_45bind40303, %struct.ScmObj** %stackaddr$prim47796, align 8
%stackaddr$makeclosure47797 = alloca %struct.ScmObj*, align 8
%fptrToInt47798 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42383 to i64
%ae42383 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47798)
store volatile %struct.ScmObj* %ae42383, %struct.ScmObj** %stackaddr$makeclosure47797, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42383, %struct.ScmObj* %a40189, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42383, %struct.ScmObj* %k40425, i64 1)
%stackaddr$makeclosure47799 = alloca %struct.ScmObj*, align 8
%fptrToInt47800 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42384 to i64
%ae42384 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47800)
store volatile %struct.ScmObj* %ae42384, %struct.ScmObj** %stackaddr$makeclosure47799, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42384, %struct.ScmObj* %a40189, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42384, %struct.ScmObj* %k40425, i64 1)
%args46649$anf_45bind40303$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47801 = alloca %struct.ScmObj*, align 8
%args46649$anf_45bind40303$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42384, %struct.ScmObj* %args46649$anf_45bind40303$0)
store volatile %struct.ScmObj* %args46649$anf_45bind40303$1, %struct.ScmObj** %stackaddr$prim47801, align 8
%stackaddr$prim47802 = alloca %struct.ScmObj*, align 8
%args46649$anf_45bind40303$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42383, %struct.ScmObj* %args46649$anf_45bind40303$1)
store volatile %struct.ScmObj* %args46649$anf_45bind40303$2, %struct.ScmObj** %stackaddr$prim47802, align 8
%clofunc47803 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40303)
musttail call tailcc void %clofunc47803(%struct.ScmObj* %anf_45bind40303, %struct.ScmObj* %args46649$anf_45bind40303$2)
ret void
}

define tailcc void @proc_clo$ae42383(%struct.ScmObj* %env$ae42383,%struct.ScmObj* %current_45args46637) {
%stackaddr$env-ref47804 = alloca %struct.ScmObj*, align 8
%a40189 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42383, i64 0)
store %struct.ScmObj* %a40189, %struct.ScmObj** %stackaddr$env-ref47804
%stackaddr$env-ref47805 = alloca %struct.ScmObj*, align 8
%k40425 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42383, i64 1)
store %struct.ScmObj* %k40425, %struct.ScmObj** %stackaddr$env-ref47805
%stackaddr$prim47806 = alloca %struct.ScmObj*, align 8
%_95k40427 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46637)
store volatile %struct.ScmObj* %_95k40427, %struct.ScmObj** %stackaddr$prim47806, align 8
%stackaddr$prim47807 = alloca %struct.ScmObj*, align 8
%current_45args46638 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46637)
store volatile %struct.ScmObj* %current_45args46638, %struct.ScmObj** %stackaddr$prim47807, align 8
%stackaddr$prim47808 = alloca %struct.ScmObj*, align 8
%cc40190 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46638)
store volatile %struct.ScmObj* %cc40190, %struct.ScmObj** %stackaddr$prim47808, align 8
%ae42499 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47809 = alloca %struct.ScmObj*, align 8
%anf_45bind40304 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42499)
store volatile %struct.ScmObj* %anf_45bind40304, %struct.ScmObj** %stackaddr$prim47809, align 8
%stackaddr$prim47810 = alloca %struct.ScmObj*, align 8
%anf_45bind40305 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40304)
store volatile %struct.ScmObj* %anf_45bind40305, %struct.ScmObj** %stackaddr$prim47810, align 8
%truthy$cmp47811 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40305)
%cmp$cmp47811 = icmp eq i64 %truthy$cmp47811, 1
br i1 %cmp$cmp47811, label %truebranch$cmp47811, label %falsebranch$cmp47811
truebranch$cmp47811:
%ae42503 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42504 = call %struct.ScmObj* @const_init_true()
%args46640$k40425$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47812 = alloca %struct.ScmObj*, align 8
%args46640$k40425$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42504, %struct.ScmObj* %args46640$k40425$0)
store volatile %struct.ScmObj* %args46640$k40425$1, %struct.ScmObj** %stackaddr$prim47812, align 8
%stackaddr$prim47813 = alloca %struct.ScmObj*, align 8
%args46640$k40425$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42503, %struct.ScmObj* %args46640$k40425$1)
store volatile %struct.ScmObj* %args46640$k40425$2, %struct.ScmObj** %stackaddr$prim47813, align 8
%clofunc47814 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40425)
musttail call tailcc void %clofunc47814(%struct.ScmObj* %k40425, %struct.ScmObj* %args46640$k40425$2)
ret void
falsebranch$cmp47811:
%ae42512 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47815 = alloca %struct.ScmObj*, align 8
%anf_45bind40306 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42512)
store volatile %struct.ScmObj* %anf_45bind40306, %struct.ScmObj** %stackaddr$prim47815, align 8
%stackaddr$prim47816 = alloca %struct.ScmObj*, align 8
%anf_45bind40307 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind40306)
store volatile %struct.ScmObj* %anf_45bind40307, %struct.ScmObj** %stackaddr$prim47816, align 8
%truthy$cmp47817 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40307)
%cmp$cmp47817 = icmp eq i64 %truthy$cmp47817, 1
br i1 %cmp$cmp47817, label %truebranch$cmp47817, label %falsebranch$cmp47817
truebranch$cmp47817:
%ae42516 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47818 = alloca %struct.ScmObj*, align 8
%anf_45bind40308 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42516)
store volatile %struct.ScmObj* %anf_45bind40308, %struct.ScmObj** %stackaddr$prim47818, align 8
%stackaddr$prim47819 = alloca %struct.ScmObj*, align 8
%b40192 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40308)
store volatile %struct.ScmObj* %b40192, %struct.ScmObj** %stackaddr$prim47819, align 8
%ae42519 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47820 = alloca %struct.ScmObj*, align 8
%anf_45bind40309 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42519)
store volatile %struct.ScmObj* %anf_45bind40309, %struct.ScmObj** %stackaddr$prim47820, align 8
%stackaddr$prim47821 = alloca %struct.ScmObj*, align 8
%anf_45bind40310 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40309)
store volatile %struct.ScmObj* %anf_45bind40310, %struct.ScmObj** %stackaddr$prim47821, align 8
%ae42522 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47822 = alloca %struct.ScmObj*, align 8
%_95040193 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42522, %struct.ScmObj* %anf_45bind40310)
store volatile %struct.ScmObj* %_95040193, %struct.ScmObj** %stackaddr$prim47822, align 8
%args46641$cc40190$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47823 = alloca %struct.ScmObj*, align 8
%args46641$cc40190$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40190, %struct.ScmObj* %args46641$cc40190$0)
store volatile %struct.ScmObj* %args46641$cc40190$1, %struct.ScmObj** %stackaddr$prim47823, align 8
%stackaddr$prim47824 = alloca %struct.ScmObj*, align 8
%args46641$cc40190$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40425, %struct.ScmObj* %args46641$cc40190$1)
store volatile %struct.ScmObj* %args46641$cc40190$2, %struct.ScmObj** %stackaddr$prim47824, align 8
%clofunc47825 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40190)
musttail call tailcc void %clofunc47825(%struct.ScmObj* %cc40190, %struct.ScmObj* %args46641$cc40190$2)
ret void
falsebranch$cmp47817:
%ae42555 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42556 = call %struct.ScmObj* @const_init_false()
%args46642$k40425$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47826 = alloca %struct.ScmObj*, align 8
%args46642$k40425$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42556, %struct.ScmObj* %args46642$k40425$0)
store volatile %struct.ScmObj* %args46642$k40425$1, %struct.ScmObj** %stackaddr$prim47826, align 8
%stackaddr$prim47827 = alloca %struct.ScmObj*, align 8
%args46642$k40425$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42555, %struct.ScmObj* %args46642$k40425$1)
store volatile %struct.ScmObj* %args46642$k40425$2, %struct.ScmObj** %stackaddr$prim47827, align 8
%clofunc47828 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40425)
musttail call tailcc void %clofunc47828(%struct.ScmObj* %k40425, %struct.ScmObj* %args46642$k40425$2)
ret void
}

define tailcc void @proc_clo$ae42384(%struct.ScmObj* %env$ae42384,%struct.ScmObj* %current_45args46643) {
%stackaddr$env-ref47829 = alloca %struct.ScmObj*, align 8
%a40189 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42384, i64 0)
store %struct.ScmObj* %a40189, %struct.ScmObj** %stackaddr$env-ref47829
%stackaddr$env-ref47830 = alloca %struct.ScmObj*, align 8
%k40425 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42384, i64 1)
store %struct.ScmObj* %k40425, %struct.ScmObj** %stackaddr$env-ref47830
%stackaddr$prim47831 = alloca %struct.ScmObj*, align 8
%_95k40427 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46643)
store volatile %struct.ScmObj* %_95k40427, %struct.ScmObj** %stackaddr$prim47831, align 8
%stackaddr$prim47832 = alloca %struct.ScmObj*, align 8
%current_45args46644 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46643)
store volatile %struct.ScmObj* %current_45args46644, %struct.ScmObj** %stackaddr$prim47832, align 8
%stackaddr$prim47833 = alloca %struct.ScmObj*, align 8
%cc40190 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46644)
store volatile %struct.ScmObj* %cc40190, %struct.ScmObj** %stackaddr$prim47833, align 8
%ae42386 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47834 = alloca %struct.ScmObj*, align 8
%anf_45bind40304 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42386)
store volatile %struct.ScmObj* %anf_45bind40304, %struct.ScmObj** %stackaddr$prim47834, align 8
%stackaddr$prim47835 = alloca %struct.ScmObj*, align 8
%anf_45bind40305 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40304)
store volatile %struct.ScmObj* %anf_45bind40305, %struct.ScmObj** %stackaddr$prim47835, align 8
%truthy$cmp47836 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40305)
%cmp$cmp47836 = icmp eq i64 %truthy$cmp47836, 1
br i1 %cmp$cmp47836, label %truebranch$cmp47836, label %falsebranch$cmp47836
truebranch$cmp47836:
%ae42390 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42391 = call %struct.ScmObj* @const_init_true()
%args46646$k40425$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47837 = alloca %struct.ScmObj*, align 8
%args46646$k40425$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42391, %struct.ScmObj* %args46646$k40425$0)
store volatile %struct.ScmObj* %args46646$k40425$1, %struct.ScmObj** %stackaddr$prim47837, align 8
%stackaddr$prim47838 = alloca %struct.ScmObj*, align 8
%args46646$k40425$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42390, %struct.ScmObj* %args46646$k40425$1)
store volatile %struct.ScmObj* %args46646$k40425$2, %struct.ScmObj** %stackaddr$prim47838, align 8
%clofunc47839 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40425)
musttail call tailcc void %clofunc47839(%struct.ScmObj* %k40425, %struct.ScmObj* %args46646$k40425$2)
ret void
falsebranch$cmp47836:
%ae42399 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47840 = alloca %struct.ScmObj*, align 8
%anf_45bind40306 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42399)
store volatile %struct.ScmObj* %anf_45bind40306, %struct.ScmObj** %stackaddr$prim47840, align 8
%stackaddr$prim47841 = alloca %struct.ScmObj*, align 8
%anf_45bind40307 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind40306)
store volatile %struct.ScmObj* %anf_45bind40307, %struct.ScmObj** %stackaddr$prim47841, align 8
%truthy$cmp47842 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40307)
%cmp$cmp47842 = icmp eq i64 %truthy$cmp47842, 1
br i1 %cmp$cmp47842, label %truebranch$cmp47842, label %falsebranch$cmp47842
truebranch$cmp47842:
%ae42403 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47843 = alloca %struct.ScmObj*, align 8
%anf_45bind40308 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42403)
store volatile %struct.ScmObj* %anf_45bind40308, %struct.ScmObj** %stackaddr$prim47843, align 8
%stackaddr$prim47844 = alloca %struct.ScmObj*, align 8
%b40192 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40308)
store volatile %struct.ScmObj* %b40192, %struct.ScmObj** %stackaddr$prim47844, align 8
%ae42406 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47845 = alloca %struct.ScmObj*, align 8
%anf_45bind40309 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42406)
store volatile %struct.ScmObj* %anf_45bind40309, %struct.ScmObj** %stackaddr$prim47845, align 8
%stackaddr$prim47846 = alloca %struct.ScmObj*, align 8
%anf_45bind40310 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40309)
store volatile %struct.ScmObj* %anf_45bind40310, %struct.ScmObj** %stackaddr$prim47846, align 8
%ae42409 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47847 = alloca %struct.ScmObj*, align 8
%_95040193 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42409, %struct.ScmObj* %anf_45bind40310)
store volatile %struct.ScmObj* %_95040193, %struct.ScmObj** %stackaddr$prim47847, align 8
%args46647$cc40190$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47848 = alloca %struct.ScmObj*, align 8
%args46647$cc40190$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40190, %struct.ScmObj* %args46647$cc40190$0)
store volatile %struct.ScmObj* %args46647$cc40190$1, %struct.ScmObj** %stackaddr$prim47848, align 8
%stackaddr$prim47849 = alloca %struct.ScmObj*, align 8
%args46647$cc40190$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40425, %struct.ScmObj* %args46647$cc40190$1)
store volatile %struct.ScmObj* %args46647$cc40190$2, %struct.ScmObj** %stackaddr$prim47849, align 8
%clofunc47850 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40190)
musttail call tailcc void %clofunc47850(%struct.ScmObj* %cc40190, %struct.ScmObj* %args46647$cc40190$2)
ret void
falsebranch$cmp47842:
%ae42442 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42443 = call %struct.ScmObj* @const_init_false()
%args46648$k40425$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47851 = alloca %struct.ScmObj*, align 8
%args46648$k40425$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42443, %struct.ScmObj* %args46648$k40425$0)
store volatile %struct.ScmObj* %args46648$k40425$1, %struct.ScmObj** %stackaddr$prim47851, align 8
%stackaddr$prim47852 = alloca %struct.ScmObj*, align 8
%args46648$k40425$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42442, %struct.ScmObj* %args46648$k40425$1)
store volatile %struct.ScmObj* %args46648$k40425$2, %struct.ScmObj** %stackaddr$prim47852, align 8
%clofunc47853 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40425)
musttail call tailcc void %clofunc47853(%struct.ScmObj* %k40425, %struct.ScmObj* %args46648$k40425$2)
ret void
}

define tailcc void @proc_clo$ae42366(%struct.ScmObj* %env$ae42366,%struct.ScmObj* %current_45args46650) {
%stackaddr$prim47854 = alloca %struct.ScmObj*, align 8
%k40428 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46650)
store volatile %struct.ScmObj* %k40428, %struct.ScmObj** %stackaddr$prim47854, align 8
%stackaddr$prim47855 = alloca %struct.ScmObj*, align 8
%current_45args46651 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46650)
store volatile %struct.ScmObj* %current_45args46651, %struct.ScmObj** %stackaddr$prim47855, align 8
%stackaddr$prim47856 = alloca %struct.ScmObj*, align 8
%k40191 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46651)
store volatile %struct.ScmObj* %k40191, %struct.ScmObj** %stackaddr$prim47856, align 8
%ae42368 = call %struct.ScmObj* @const_init_int(i64 0)
%args46653$k40428$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47857 = alloca %struct.ScmObj*, align 8
%args46653$k40428$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40191, %struct.ScmObj* %args46653$k40428$0)
store volatile %struct.ScmObj* %args46653$k40428$1, %struct.ScmObj** %stackaddr$prim47857, align 8
%stackaddr$prim47858 = alloca %struct.ScmObj*, align 8
%args46653$k40428$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42368, %struct.ScmObj* %args46653$k40428$1)
store volatile %struct.ScmObj* %args46653$k40428$2, %struct.ScmObj** %stackaddr$prim47858, align 8
%clofunc47859 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40428)
musttail call tailcc void %clofunc47859(%struct.ScmObj* %k40428, %struct.ScmObj* %args46653$k40428$2)
ret void
}

define tailcc void @proc_clo$ae42289(%struct.ScmObj* %env$ae42289,%struct.ScmObj* %current_45args46656) {
%stackaddr$env-ref47860 = alloca %struct.ScmObj*, align 8
%_37append40195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42289, i64 0)
store %struct.ScmObj* %_37append40195, %struct.ScmObj** %stackaddr$env-ref47860
%stackaddr$prim47861 = alloca %struct.ScmObj*, align 8
%k40429 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46656)
store volatile %struct.ScmObj* %k40429, %struct.ScmObj** %stackaddr$prim47861, align 8
%stackaddr$prim47862 = alloca %struct.ScmObj*, align 8
%current_45args46657 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46656)
store volatile %struct.ScmObj* %current_45args46657, %struct.ScmObj** %stackaddr$prim47862, align 8
%stackaddr$prim47863 = alloca %struct.ScmObj*, align 8
%ls040198 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46657)
store volatile %struct.ScmObj* %ls040198, %struct.ScmObj** %stackaddr$prim47863, align 8
%stackaddr$prim47864 = alloca %struct.ScmObj*, align 8
%current_45args46658 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46657)
store volatile %struct.ScmObj* %current_45args46658, %struct.ScmObj** %stackaddr$prim47864, align 8
%stackaddr$prim47865 = alloca %struct.ScmObj*, align 8
%ls140197 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46658)
store volatile %struct.ScmObj* %ls140197, %struct.ScmObj** %stackaddr$prim47865, align 8
%stackaddr$prim47866 = alloca %struct.ScmObj*, align 8
%anf_45bind40297 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls040198)
store volatile %struct.ScmObj* %anf_45bind40297, %struct.ScmObj** %stackaddr$prim47866, align 8
%truthy$cmp47867 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40297)
%cmp$cmp47867 = icmp eq i64 %truthy$cmp47867, 1
br i1 %cmp$cmp47867, label %truebranch$cmp47867, label %falsebranch$cmp47867
truebranch$cmp47867:
%ae42293 = call %struct.ScmObj* @const_init_int(i64 0)
%args46660$k40429$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47868 = alloca %struct.ScmObj*, align 8
%args46660$k40429$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls140197, %struct.ScmObj* %args46660$k40429$0)
store volatile %struct.ScmObj* %args46660$k40429$1, %struct.ScmObj** %stackaddr$prim47868, align 8
%stackaddr$prim47869 = alloca %struct.ScmObj*, align 8
%args46660$k40429$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42293, %struct.ScmObj* %args46660$k40429$1)
store volatile %struct.ScmObj* %args46660$k40429$2, %struct.ScmObj** %stackaddr$prim47869, align 8
%clofunc47870 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40429)
musttail call tailcc void %clofunc47870(%struct.ScmObj* %k40429, %struct.ScmObj* %args46660$k40429$2)
ret void
falsebranch$cmp47867:
%stackaddr$prim47871 = alloca %struct.ScmObj*, align 8
%anf_45bind40298 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls040198)
store volatile %struct.ScmObj* %anf_45bind40298, %struct.ScmObj** %stackaddr$prim47871, align 8
%ae42300 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47872 = alloca %struct.ScmObj*, align 8
%anf_45bind40299 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append40195, %struct.ScmObj* %ae42300)
store volatile %struct.ScmObj* %anf_45bind40299, %struct.ScmObj** %stackaddr$prim47872, align 8
%stackaddr$prim47873 = alloca %struct.ScmObj*, align 8
%anf_45bind40300 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls040198)
store volatile %struct.ScmObj* %anf_45bind40300, %struct.ScmObj** %stackaddr$prim47873, align 8
%stackaddr$makeclosure47874 = alloca %struct.ScmObj*, align 8
%fptrToInt47875 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42303 to i64
%ae42303 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47875)
store volatile %struct.ScmObj* %ae42303, %struct.ScmObj** %stackaddr$makeclosure47874, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42303, %struct.ScmObj* %k40429, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42303, %struct.ScmObj* %anf_45bind40298, i64 1)
%args46665$anf_45bind40299$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47876 = alloca %struct.ScmObj*, align 8
%args46665$anf_45bind40299$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls140197, %struct.ScmObj* %args46665$anf_45bind40299$0)
store volatile %struct.ScmObj* %args46665$anf_45bind40299$1, %struct.ScmObj** %stackaddr$prim47876, align 8
%stackaddr$prim47877 = alloca %struct.ScmObj*, align 8
%args46665$anf_45bind40299$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40300, %struct.ScmObj* %args46665$anf_45bind40299$1)
store volatile %struct.ScmObj* %args46665$anf_45bind40299$2, %struct.ScmObj** %stackaddr$prim47877, align 8
%stackaddr$prim47878 = alloca %struct.ScmObj*, align 8
%args46665$anf_45bind40299$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42303, %struct.ScmObj* %args46665$anf_45bind40299$2)
store volatile %struct.ScmObj* %args46665$anf_45bind40299$3, %struct.ScmObj** %stackaddr$prim47878, align 8
%clofunc47879 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40299)
musttail call tailcc void %clofunc47879(%struct.ScmObj* %anf_45bind40299, %struct.ScmObj* %args46665$anf_45bind40299$3)
ret void
}

define tailcc void @proc_clo$ae42303(%struct.ScmObj* %env$ae42303,%struct.ScmObj* %current_45args46661) {
%stackaddr$env-ref47880 = alloca %struct.ScmObj*, align 8
%k40429 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42303, i64 0)
store %struct.ScmObj* %k40429, %struct.ScmObj** %stackaddr$env-ref47880
%stackaddr$env-ref47881 = alloca %struct.ScmObj*, align 8
%anf_45bind40298 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42303, i64 1)
store %struct.ScmObj* %anf_45bind40298, %struct.ScmObj** %stackaddr$env-ref47881
%stackaddr$prim47882 = alloca %struct.ScmObj*, align 8
%_95k40430 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46661)
store volatile %struct.ScmObj* %_95k40430, %struct.ScmObj** %stackaddr$prim47882, align 8
%stackaddr$prim47883 = alloca %struct.ScmObj*, align 8
%current_45args46662 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46661)
store volatile %struct.ScmObj* %current_45args46662, %struct.ScmObj** %stackaddr$prim47883, align 8
%stackaddr$prim47884 = alloca %struct.ScmObj*, align 8
%anf_45bind40301 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46662)
store volatile %struct.ScmObj* %anf_45bind40301, %struct.ScmObj** %stackaddr$prim47884, align 8
%stackaddr$prim47885 = alloca %struct.ScmObj*, align 8
%cpsprim40431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40298, %struct.ScmObj* %anf_45bind40301)
store volatile %struct.ScmObj* %cpsprim40431, %struct.ScmObj** %stackaddr$prim47885, align 8
%ae42309 = call %struct.ScmObj* @const_init_int(i64 0)
%args46664$k40429$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47886 = alloca %struct.ScmObj*, align 8
%args46664$k40429$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40431, %struct.ScmObj* %args46664$k40429$0)
store volatile %struct.ScmObj* %args46664$k40429$1, %struct.ScmObj** %stackaddr$prim47886, align 8
%stackaddr$prim47887 = alloca %struct.ScmObj*, align 8
%args46664$k40429$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42309, %struct.ScmObj* %args46664$k40429$1)
store volatile %struct.ScmObj* %args46664$k40429$2, %struct.ScmObj** %stackaddr$prim47887, align 8
%clofunc47888 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40429)
musttail call tailcc void %clofunc47888(%struct.ScmObj* %k40429, %struct.ScmObj* %args46664$k40429$2)
ret void
}

define tailcc void @proc_clo$ae42263(%struct.ScmObj* %env$ae42263,%struct.ScmObj* %current_45args46667) {
%stackaddr$prim47889 = alloca %struct.ScmObj*, align 8
%k40432 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46667)
store volatile %struct.ScmObj* %k40432, %struct.ScmObj** %stackaddr$prim47889, align 8
%stackaddr$prim47890 = alloca %struct.ScmObj*, align 8
%current_45args46668 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46667)
store volatile %struct.ScmObj* %current_45args46668, %struct.ScmObj** %stackaddr$prim47890, align 8
%stackaddr$prim47891 = alloca %struct.ScmObj*, align 8
%a40201 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46668)
store volatile %struct.ScmObj* %a40201, %struct.ScmObj** %stackaddr$prim47891, align 8
%stackaddr$prim47892 = alloca %struct.ScmObj*, align 8
%current_45args46669 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46668)
store volatile %struct.ScmObj* %current_45args46669, %struct.ScmObj** %stackaddr$prim47892, align 8
%stackaddr$prim47893 = alloca %struct.ScmObj*, align 8
%b40200 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46669)
store volatile %struct.ScmObj* %b40200, %struct.ScmObj** %stackaddr$prim47893, align 8
%stackaddr$prim47894 = alloca %struct.ScmObj*, align 8
%anf_45bind40296 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a40201, %struct.ScmObj* %b40200)
store volatile %struct.ScmObj* %anf_45bind40296, %struct.ScmObj** %stackaddr$prim47894, align 8
%stackaddr$prim47895 = alloca %struct.ScmObj*, align 8
%cpsprim40433 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind40296)
store volatile %struct.ScmObj* %cpsprim40433, %struct.ScmObj** %stackaddr$prim47895, align 8
%ae42268 = call %struct.ScmObj* @const_init_int(i64 0)
%args46671$k40432$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47896 = alloca %struct.ScmObj*, align 8
%args46671$k40432$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40433, %struct.ScmObj* %args46671$k40432$0)
store volatile %struct.ScmObj* %args46671$k40432$1, %struct.ScmObj** %stackaddr$prim47896, align 8
%stackaddr$prim47897 = alloca %struct.ScmObj*, align 8
%args46671$k40432$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42268, %struct.ScmObj* %args46671$k40432$1)
store volatile %struct.ScmObj* %args46671$k40432$2, %struct.ScmObj** %stackaddr$prim47897, align 8
%clofunc47898 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40432)
musttail call tailcc void %clofunc47898(%struct.ScmObj* %k40432, %struct.ScmObj* %args46671$k40432$2)
ret void
}

define tailcc void @proc_clo$ae42239(%struct.ScmObj* %env$ae42239,%struct.ScmObj* %current_45args46673) {
%stackaddr$prim47899 = alloca %struct.ScmObj*, align 8
%k40434 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46673)
store volatile %struct.ScmObj* %k40434, %struct.ScmObj** %stackaddr$prim47899, align 8
%stackaddr$prim47900 = alloca %struct.ScmObj*, align 8
%current_45args46674 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46673)
store volatile %struct.ScmObj* %current_45args46674, %struct.ScmObj** %stackaddr$prim47900, align 8
%stackaddr$prim47901 = alloca %struct.ScmObj*, align 8
%a40204 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46674)
store volatile %struct.ScmObj* %a40204, %struct.ScmObj** %stackaddr$prim47901, align 8
%stackaddr$prim47902 = alloca %struct.ScmObj*, align 8
%current_45args46675 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46674)
store volatile %struct.ScmObj* %current_45args46675, %struct.ScmObj** %stackaddr$prim47902, align 8
%stackaddr$prim47903 = alloca %struct.ScmObj*, align 8
%b40203 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46675)
store volatile %struct.ScmObj* %b40203, %struct.ScmObj** %stackaddr$prim47903, align 8
%stackaddr$prim47904 = alloca %struct.ScmObj*, align 8
%anf_45bind40295 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a40204, %struct.ScmObj* %b40203)
store volatile %struct.ScmObj* %anf_45bind40295, %struct.ScmObj** %stackaddr$prim47904, align 8
%stackaddr$prim47905 = alloca %struct.ScmObj*, align 8
%cpsprim40435 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind40295)
store volatile %struct.ScmObj* %cpsprim40435, %struct.ScmObj** %stackaddr$prim47905, align 8
%ae42244 = call %struct.ScmObj* @const_init_int(i64 0)
%args46677$k40434$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47906 = alloca %struct.ScmObj*, align 8
%args46677$k40434$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40435, %struct.ScmObj* %args46677$k40434$0)
store volatile %struct.ScmObj* %args46677$k40434$1, %struct.ScmObj** %stackaddr$prim47906, align 8
%stackaddr$prim47907 = alloca %struct.ScmObj*, align 8
%args46677$k40434$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42244, %struct.ScmObj* %args46677$k40434$1)
store volatile %struct.ScmObj* %args46677$k40434$2, %struct.ScmObj** %stackaddr$prim47907, align 8
%clofunc47908 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40434)
musttail call tailcc void %clofunc47908(%struct.ScmObj* %k40434, %struct.ScmObj* %args46677$k40434$2)
ret void
}

define tailcc void @proc_clo$ae41845(%struct.ScmObj* %env$ae41845,%struct.ScmObj* %current_45args46680) {
%stackaddr$env-ref47909 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41845, i64 0)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref47909
%stackaddr$env-ref47910 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41845, i64 1)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47910
%stackaddr$env-ref47911 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41845, i64 2)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47911
%stackaddr$prim47912 = alloca %struct.ScmObj*, align 8
%k40436 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46680)
store volatile %struct.ScmObj* %k40436, %struct.ScmObj** %stackaddr$prim47912, align 8
%stackaddr$prim47913 = alloca %struct.ScmObj*, align 8
%current_45args46681 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46680)
store volatile %struct.ScmObj* %current_45args46681, %struct.ScmObj** %stackaddr$prim47913, align 8
%stackaddr$prim47914 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46681)
store volatile %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$prim47914, align 8
%ae41847 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47915 = alloca %struct.ScmObj*, align 8
%fptrToInt47916 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41848 to i64
%ae41848 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt47916)
store volatile %struct.ScmObj* %ae41848, %struct.ScmObj** %stackaddr$makeclosure47915, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41848, %struct.ScmObj* %_37foldr40128, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41848, %struct.ScmObj* %_37foldl40206, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41848, %struct.ScmObj* %_37foldr140123, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41848, %struct.ScmObj* %_37map140154, i64 3)
%args46738$k40436$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47917 = alloca %struct.ScmObj*, align 8
%args46738$k40436$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41848, %struct.ScmObj* %args46738$k40436$0)
store volatile %struct.ScmObj* %args46738$k40436$1, %struct.ScmObj** %stackaddr$prim47917, align 8
%stackaddr$prim47918 = alloca %struct.ScmObj*, align 8
%args46738$k40436$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41847, %struct.ScmObj* %args46738$k40436$1)
store volatile %struct.ScmObj* %args46738$k40436$2, %struct.ScmObj** %stackaddr$prim47918, align 8
%clofunc47919 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40436)
musttail call tailcc void %clofunc47919(%struct.ScmObj* %k40436, %struct.ScmObj* %args46738$k40436$2)
ret void
}

define tailcc void @proc_clo$ae41848(%struct.ScmObj* %env$ae41848,%struct.ScmObj* %args4020740437) {
%stackaddr$env-ref47920 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41848, i64 0)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref47920
%stackaddr$env-ref47921 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41848, i64 1)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref47921
%stackaddr$env-ref47922 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41848, i64 2)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47922
%stackaddr$env-ref47923 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41848, i64 3)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47923
%stackaddr$prim47924 = alloca %struct.ScmObj*, align 8
%k40438 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4020740437)
store volatile %struct.ScmObj* %k40438, %struct.ScmObj** %stackaddr$prim47924, align 8
%stackaddr$prim47925 = alloca %struct.ScmObj*, align 8
%args40207 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4020740437)
store volatile %struct.ScmObj* %args40207, %struct.ScmObj** %stackaddr$prim47925, align 8
%stackaddr$prim47926 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40207)
store volatile %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$prim47926, align 8
%stackaddr$prim47927 = alloca %struct.ScmObj*, align 8
%anf_45bind40283 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40207)
store volatile %struct.ScmObj* %anf_45bind40283, %struct.ScmObj** %stackaddr$prim47927, align 8
%stackaddr$prim47928 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40283)
store volatile %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$prim47928, align 8
%stackaddr$prim47929 = alloca %struct.ScmObj*, align 8
%anf_45bind40284 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40207)
store volatile %struct.ScmObj* %anf_45bind40284, %struct.ScmObj** %stackaddr$prim47929, align 8
%stackaddr$prim47930 = alloca %struct.ScmObj*, align 8
%lsts40208 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40284)
store volatile %struct.ScmObj* %lsts40208, %struct.ScmObj** %stackaddr$prim47930, align 8
%stackaddr$makeclosure47931 = alloca %struct.ScmObj*, align 8
%fptrToInt47932 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41856 to i64
%ae41856 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt47932)
store volatile %struct.ScmObj* %ae41856, %struct.ScmObj** %stackaddr$makeclosure47931, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41856, %struct.ScmObj* %lsts40208, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41856, %struct.ScmObj* %_37foldr40128, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41856, %struct.ScmObj* %k40438, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41856, %struct.ScmObj* %f40210, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41856, %struct.ScmObj* %acc40209, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41856, %struct.ScmObj* %_37foldl40206, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41856, %struct.ScmObj* %_37foldr140123, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae41856, %struct.ScmObj* %_37map140154, i64 7)
%ae41857 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47933 = alloca %struct.ScmObj*, align 8
%fptrToInt47934 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41858 to i64
%ae41858 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47934)
store volatile %struct.ScmObj* %ae41858, %struct.ScmObj** %stackaddr$makeclosure47933, align 8
%args46737$ae41856$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47935 = alloca %struct.ScmObj*, align 8
%args46737$ae41856$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41858, %struct.ScmObj* %args46737$ae41856$0)
store volatile %struct.ScmObj* %args46737$ae41856$1, %struct.ScmObj** %stackaddr$prim47935, align 8
%stackaddr$prim47936 = alloca %struct.ScmObj*, align 8
%args46737$ae41856$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41857, %struct.ScmObj* %args46737$ae41856$1)
store volatile %struct.ScmObj* %args46737$ae41856$2, %struct.ScmObj** %stackaddr$prim47936, align 8
%clofunc47937 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41856)
musttail call tailcc void %clofunc47937(%struct.ScmObj* %ae41856, %struct.ScmObj* %args46737$ae41856$2)
ret void
}

define tailcc void @proc_clo$ae41856(%struct.ScmObj* %env$ae41856,%struct.ScmObj* %current_45args46683) {
%stackaddr$env-ref47938 = alloca %struct.ScmObj*, align 8
%lsts40208 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41856, i64 0)
store %struct.ScmObj* %lsts40208, %struct.ScmObj** %stackaddr$env-ref47938
%stackaddr$env-ref47939 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41856, i64 1)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref47939
%stackaddr$env-ref47940 = alloca %struct.ScmObj*, align 8
%k40438 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41856, i64 2)
store %struct.ScmObj* %k40438, %struct.ScmObj** %stackaddr$env-ref47940
%stackaddr$env-ref47941 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41856, i64 3)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref47941
%stackaddr$env-ref47942 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41856, i64 4)
store %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$env-ref47942
%stackaddr$env-ref47943 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41856, i64 5)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref47943
%stackaddr$env-ref47944 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41856, i64 6)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47944
%stackaddr$env-ref47945 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41856, i64 7)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47945
%stackaddr$prim47946 = alloca %struct.ScmObj*, align 8
%_95k40439 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46683)
store volatile %struct.ScmObj* %_95k40439, %struct.ScmObj** %stackaddr$prim47946, align 8
%stackaddr$prim47947 = alloca %struct.ScmObj*, align 8
%current_45args46684 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46683)
store volatile %struct.ScmObj* %current_45args46684, %struct.ScmObj** %stackaddr$prim47947, align 8
%stackaddr$prim47948 = alloca %struct.ScmObj*, align 8
%anf_45bind40285 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46684)
store volatile %struct.ScmObj* %anf_45bind40285, %struct.ScmObj** %stackaddr$prim47948, align 8
%stackaddr$makeclosure47949 = alloca %struct.ScmObj*, align 8
%fptrToInt47950 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41888 to i64
%ae41888 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt47950)
store volatile %struct.ScmObj* %ae41888, %struct.ScmObj** %stackaddr$makeclosure47949, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41888, %struct.ScmObj* %lsts40208, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41888, %struct.ScmObj* %_37foldr40128, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41888, %struct.ScmObj* %k40438, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41888, %struct.ScmObj* %f40210, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41888, %struct.ScmObj* %acc40209, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41888, %struct.ScmObj* %_37foldl40206, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41888, %struct.ScmObj* %_37map140154, i64 6)
%ae41890 = call %struct.ScmObj* @const_init_false()
%args46730$_37foldr140123$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47951 = alloca %struct.ScmObj*, align 8
%args46730$_37foldr140123$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40208, %struct.ScmObj* %args46730$_37foldr140123$0)
store volatile %struct.ScmObj* %args46730$_37foldr140123$1, %struct.ScmObj** %stackaddr$prim47951, align 8
%stackaddr$prim47952 = alloca %struct.ScmObj*, align 8
%args46730$_37foldr140123$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41890, %struct.ScmObj* %args46730$_37foldr140123$1)
store volatile %struct.ScmObj* %args46730$_37foldr140123$2, %struct.ScmObj** %stackaddr$prim47952, align 8
%stackaddr$prim47953 = alloca %struct.ScmObj*, align 8
%args46730$_37foldr140123$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40285, %struct.ScmObj* %args46730$_37foldr140123$2)
store volatile %struct.ScmObj* %args46730$_37foldr140123$3, %struct.ScmObj** %stackaddr$prim47953, align 8
%stackaddr$prim47954 = alloca %struct.ScmObj*, align 8
%args46730$_37foldr140123$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41888, %struct.ScmObj* %args46730$_37foldr140123$3)
store volatile %struct.ScmObj* %args46730$_37foldr140123$4, %struct.ScmObj** %stackaddr$prim47954, align 8
%clofunc47955 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140123)
musttail call tailcc void %clofunc47955(%struct.ScmObj* %_37foldr140123, %struct.ScmObj* %args46730$_37foldr140123$4)
ret void
}

define tailcc void @proc_clo$ae41888(%struct.ScmObj* %env$ae41888,%struct.ScmObj* %current_45args46686) {
%stackaddr$env-ref47956 = alloca %struct.ScmObj*, align 8
%lsts40208 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41888, i64 0)
store %struct.ScmObj* %lsts40208, %struct.ScmObj** %stackaddr$env-ref47956
%stackaddr$env-ref47957 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41888, i64 1)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref47957
%stackaddr$env-ref47958 = alloca %struct.ScmObj*, align 8
%k40438 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41888, i64 2)
store %struct.ScmObj* %k40438, %struct.ScmObj** %stackaddr$env-ref47958
%stackaddr$env-ref47959 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41888, i64 3)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref47959
%stackaddr$env-ref47960 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41888, i64 4)
store %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$env-ref47960
%stackaddr$env-ref47961 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41888, i64 5)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref47961
%stackaddr$env-ref47962 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41888, i64 6)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47962
%stackaddr$prim47963 = alloca %struct.ScmObj*, align 8
%_95k40440 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46686)
store volatile %struct.ScmObj* %_95k40440, %struct.ScmObj** %stackaddr$prim47963, align 8
%stackaddr$prim47964 = alloca %struct.ScmObj*, align 8
%current_45args46687 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46686)
store volatile %struct.ScmObj* %current_45args46687, %struct.ScmObj** %stackaddr$prim47964, align 8
%stackaddr$prim47965 = alloca %struct.ScmObj*, align 8
%anf_45bind40286 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46687)
store volatile %struct.ScmObj* %anf_45bind40286, %struct.ScmObj** %stackaddr$prim47965, align 8
%truthy$cmp47966 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40286)
%cmp$cmp47966 = icmp eq i64 %truthy$cmp47966, 1
br i1 %cmp$cmp47966, label %truebranch$cmp47966, label %falsebranch$cmp47966
truebranch$cmp47966:
%ae41899 = call %struct.ScmObj* @const_init_int(i64 0)
%args46689$k40438$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47967 = alloca %struct.ScmObj*, align 8
%args46689$k40438$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40209, %struct.ScmObj* %args46689$k40438$0)
store volatile %struct.ScmObj* %args46689$k40438$1, %struct.ScmObj** %stackaddr$prim47967, align 8
%stackaddr$prim47968 = alloca %struct.ScmObj*, align 8
%args46689$k40438$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41899, %struct.ScmObj* %args46689$k40438$1)
store volatile %struct.ScmObj* %args46689$k40438$2, %struct.ScmObj** %stackaddr$prim47968, align 8
%clofunc47969 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40438)
musttail call tailcc void %clofunc47969(%struct.ScmObj* %k40438, %struct.ScmObj* %args46689$k40438$2)
ret void
falsebranch$cmp47966:
%stackaddr$makeclosure47970 = alloca %struct.ScmObj*, align 8
%fptrToInt47971 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41904 to i64
%ae41904 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt47971)
store volatile %struct.ScmObj* %ae41904, %struct.ScmObj** %stackaddr$makeclosure47970, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41904, %struct.ScmObj* %lsts40208, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41904, %struct.ScmObj* %_37foldr40128, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41904, %struct.ScmObj* %k40438, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41904, %struct.ScmObj* %f40210, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41904, %struct.ScmObj* %acc40209, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41904, %struct.ScmObj* %_37foldl40206, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41904, %struct.ScmObj* %_37map140154, i64 6)
%ae41905 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47972 = alloca %struct.ScmObj*, align 8
%fptrToInt47973 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41906 to i64
%ae41906 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47973)
store volatile %struct.ScmObj* %ae41906, %struct.ScmObj** %stackaddr$makeclosure47972, align 8
%args46729$ae41904$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47974 = alloca %struct.ScmObj*, align 8
%args46729$ae41904$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41906, %struct.ScmObj* %args46729$ae41904$0)
store volatile %struct.ScmObj* %args46729$ae41904$1, %struct.ScmObj** %stackaddr$prim47974, align 8
%stackaddr$prim47975 = alloca %struct.ScmObj*, align 8
%args46729$ae41904$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41905, %struct.ScmObj* %args46729$ae41904$1)
store volatile %struct.ScmObj* %args46729$ae41904$2, %struct.ScmObj** %stackaddr$prim47975, align 8
%clofunc47976 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41904)
musttail call tailcc void %clofunc47976(%struct.ScmObj* %ae41904, %struct.ScmObj* %args46729$ae41904$2)
ret void
}

define tailcc void @proc_clo$ae41904(%struct.ScmObj* %env$ae41904,%struct.ScmObj* %current_45args46690) {
%stackaddr$env-ref47977 = alloca %struct.ScmObj*, align 8
%lsts40208 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41904, i64 0)
store %struct.ScmObj* %lsts40208, %struct.ScmObj** %stackaddr$env-ref47977
%stackaddr$env-ref47978 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41904, i64 1)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref47978
%stackaddr$env-ref47979 = alloca %struct.ScmObj*, align 8
%k40438 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41904, i64 2)
store %struct.ScmObj* %k40438, %struct.ScmObj** %stackaddr$env-ref47979
%stackaddr$env-ref47980 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41904, i64 3)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref47980
%stackaddr$env-ref47981 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41904, i64 4)
store %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$env-ref47981
%stackaddr$env-ref47982 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41904, i64 5)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref47982
%stackaddr$env-ref47983 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41904, i64 6)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47983
%stackaddr$prim47984 = alloca %struct.ScmObj*, align 8
%_95k40441 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46690)
store volatile %struct.ScmObj* %_95k40441, %struct.ScmObj** %stackaddr$prim47984, align 8
%stackaddr$prim47985 = alloca %struct.ScmObj*, align 8
%current_45args46691 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46690)
store volatile %struct.ScmObj* %current_45args46691, %struct.ScmObj** %stackaddr$prim47985, align 8
%stackaddr$prim47986 = alloca %struct.ScmObj*, align 8
%anf_45bind40287 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46691)
store volatile %struct.ScmObj* %anf_45bind40287, %struct.ScmObj** %stackaddr$prim47986, align 8
%stackaddr$makeclosure47987 = alloca %struct.ScmObj*, align 8
%fptrToInt47988 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41925 to i64
%ae41925 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt47988)
store volatile %struct.ScmObj* %ae41925, %struct.ScmObj** %stackaddr$makeclosure47987, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41925, %struct.ScmObj* %lsts40208, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41925, %struct.ScmObj* %_37foldr40128, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41925, %struct.ScmObj* %k40438, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41925, %struct.ScmObj* %f40210, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41925, %struct.ScmObj* %acc40209, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41925, %struct.ScmObj* %_37foldl40206, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41925, %struct.ScmObj* %_37map140154, i64 6)
%args46724$_37map140154$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47989 = alloca %struct.ScmObj*, align 8
%args46724$_37map140154$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40208, %struct.ScmObj* %args46724$_37map140154$0)
store volatile %struct.ScmObj* %args46724$_37map140154$1, %struct.ScmObj** %stackaddr$prim47989, align 8
%stackaddr$prim47990 = alloca %struct.ScmObj*, align 8
%args46724$_37map140154$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40287, %struct.ScmObj* %args46724$_37map140154$1)
store volatile %struct.ScmObj* %args46724$_37map140154$2, %struct.ScmObj** %stackaddr$prim47990, align 8
%stackaddr$prim47991 = alloca %struct.ScmObj*, align 8
%args46724$_37map140154$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41925, %struct.ScmObj* %args46724$_37map140154$2)
store volatile %struct.ScmObj* %args46724$_37map140154$3, %struct.ScmObj** %stackaddr$prim47991, align 8
%clofunc47992 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140154)
musttail call tailcc void %clofunc47992(%struct.ScmObj* %_37map140154, %struct.ScmObj* %args46724$_37map140154$3)
ret void
}

define tailcc void @proc_clo$ae41925(%struct.ScmObj* %env$ae41925,%struct.ScmObj* %current_45args46693) {
%stackaddr$env-ref47993 = alloca %struct.ScmObj*, align 8
%lsts40208 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41925, i64 0)
store %struct.ScmObj* %lsts40208, %struct.ScmObj** %stackaddr$env-ref47993
%stackaddr$env-ref47994 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41925, i64 1)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref47994
%stackaddr$env-ref47995 = alloca %struct.ScmObj*, align 8
%k40438 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41925, i64 2)
store %struct.ScmObj* %k40438, %struct.ScmObj** %stackaddr$env-ref47995
%stackaddr$env-ref47996 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41925, i64 3)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref47996
%stackaddr$env-ref47997 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41925, i64 4)
store %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$env-ref47997
%stackaddr$env-ref47998 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41925, i64 5)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref47998
%stackaddr$env-ref47999 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41925, i64 6)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47999
%stackaddr$prim48000 = alloca %struct.ScmObj*, align 8
%_95k40442 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46693)
store volatile %struct.ScmObj* %_95k40442, %struct.ScmObj** %stackaddr$prim48000, align 8
%stackaddr$prim48001 = alloca %struct.ScmObj*, align 8
%current_45args46694 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46693)
store volatile %struct.ScmObj* %current_45args46694, %struct.ScmObj** %stackaddr$prim48001, align 8
%stackaddr$prim48002 = alloca %struct.ScmObj*, align 8
%lsts_4340215 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46694)
store volatile %struct.ScmObj* %lsts_4340215, %struct.ScmObj** %stackaddr$prim48002, align 8
%stackaddr$makeclosure48003 = alloca %struct.ScmObj*, align 8
%fptrToInt48004 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41928 to i64
%ae41928 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt48004)
store volatile %struct.ScmObj* %ae41928, %struct.ScmObj** %stackaddr$makeclosure48003, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41928, %struct.ScmObj* %lsts40208, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41928, %struct.ScmObj* %_37foldr40128, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41928, %struct.ScmObj* %lsts_4340215, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41928, %struct.ScmObj* %k40438, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41928, %struct.ScmObj* %f40210, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41928, %struct.ScmObj* %acc40209, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41928, %struct.ScmObj* %_37foldl40206, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae41928, %struct.ScmObj* %_37map140154, i64 7)
%ae41929 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48005 = alloca %struct.ScmObj*, align 8
%fptrToInt48006 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41930 to i64
%ae41930 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48006)
store volatile %struct.ScmObj* %ae41930, %struct.ScmObj** %stackaddr$makeclosure48005, align 8
%args46723$ae41928$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48007 = alloca %struct.ScmObj*, align 8
%args46723$ae41928$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41930, %struct.ScmObj* %args46723$ae41928$0)
store volatile %struct.ScmObj* %args46723$ae41928$1, %struct.ScmObj** %stackaddr$prim48007, align 8
%stackaddr$prim48008 = alloca %struct.ScmObj*, align 8
%args46723$ae41928$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41929, %struct.ScmObj* %args46723$ae41928$1)
store volatile %struct.ScmObj* %args46723$ae41928$2, %struct.ScmObj** %stackaddr$prim48008, align 8
%clofunc48009 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41928)
musttail call tailcc void %clofunc48009(%struct.ScmObj* %ae41928, %struct.ScmObj* %args46723$ae41928$2)
ret void
}

define tailcc void @proc_clo$ae41928(%struct.ScmObj* %env$ae41928,%struct.ScmObj* %current_45args46696) {
%stackaddr$env-ref48010 = alloca %struct.ScmObj*, align 8
%lsts40208 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41928, i64 0)
store %struct.ScmObj* %lsts40208, %struct.ScmObj** %stackaddr$env-ref48010
%stackaddr$env-ref48011 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41928, i64 1)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref48011
%stackaddr$env-ref48012 = alloca %struct.ScmObj*, align 8
%lsts_4340215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41928, i64 2)
store %struct.ScmObj* %lsts_4340215, %struct.ScmObj** %stackaddr$env-ref48012
%stackaddr$env-ref48013 = alloca %struct.ScmObj*, align 8
%k40438 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41928, i64 3)
store %struct.ScmObj* %k40438, %struct.ScmObj** %stackaddr$env-ref48013
%stackaddr$env-ref48014 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41928, i64 4)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref48014
%stackaddr$env-ref48015 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41928, i64 5)
store %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$env-ref48015
%stackaddr$env-ref48016 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41928, i64 6)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48016
%stackaddr$env-ref48017 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41928, i64 7)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref48017
%stackaddr$prim48018 = alloca %struct.ScmObj*, align 8
%_95k40443 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46696)
store volatile %struct.ScmObj* %_95k40443, %struct.ScmObj** %stackaddr$prim48018, align 8
%stackaddr$prim48019 = alloca %struct.ScmObj*, align 8
%current_45args46697 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46696)
store volatile %struct.ScmObj* %current_45args46697, %struct.ScmObj** %stackaddr$prim48019, align 8
%stackaddr$prim48020 = alloca %struct.ScmObj*, align 8
%anf_45bind40288 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46697)
store volatile %struct.ScmObj* %anf_45bind40288, %struct.ScmObj** %stackaddr$prim48020, align 8
%stackaddr$makeclosure48021 = alloca %struct.ScmObj*, align 8
%fptrToInt48022 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41949 to i64
%ae41949 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt48022)
store volatile %struct.ScmObj* %ae41949, %struct.ScmObj** %stackaddr$makeclosure48021, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41949, %struct.ScmObj* %lsts_4340215, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41949, %struct.ScmObj* %k40438, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41949, %struct.ScmObj* %f40210, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41949, %struct.ScmObj* %acc40209, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41949, %struct.ScmObj* %_37foldr40128, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41949, %struct.ScmObj* %_37foldl40206, i64 5)
%args46718$_37map140154$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48023 = alloca %struct.ScmObj*, align 8
%args46718$_37map140154$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40208, %struct.ScmObj* %args46718$_37map140154$0)
store volatile %struct.ScmObj* %args46718$_37map140154$1, %struct.ScmObj** %stackaddr$prim48023, align 8
%stackaddr$prim48024 = alloca %struct.ScmObj*, align 8
%args46718$_37map140154$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40288, %struct.ScmObj* %args46718$_37map140154$1)
store volatile %struct.ScmObj* %args46718$_37map140154$2, %struct.ScmObj** %stackaddr$prim48024, align 8
%stackaddr$prim48025 = alloca %struct.ScmObj*, align 8
%args46718$_37map140154$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41949, %struct.ScmObj* %args46718$_37map140154$2)
store volatile %struct.ScmObj* %args46718$_37map140154$3, %struct.ScmObj** %stackaddr$prim48025, align 8
%clofunc48026 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140154)
musttail call tailcc void %clofunc48026(%struct.ScmObj* %_37map140154, %struct.ScmObj* %args46718$_37map140154$3)
ret void
}

define tailcc void @proc_clo$ae41949(%struct.ScmObj* %env$ae41949,%struct.ScmObj* %current_45args46699) {
%stackaddr$env-ref48027 = alloca %struct.ScmObj*, align 8
%lsts_4340215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41949, i64 0)
store %struct.ScmObj* %lsts_4340215, %struct.ScmObj** %stackaddr$env-ref48027
%stackaddr$env-ref48028 = alloca %struct.ScmObj*, align 8
%k40438 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41949, i64 1)
store %struct.ScmObj* %k40438, %struct.ScmObj** %stackaddr$env-ref48028
%stackaddr$env-ref48029 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41949, i64 2)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref48029
%stackaddr$env-ref48030 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41949, i64 3)
store %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$env-ref48030
%stackaddr$env-ref48031 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41949, i64 4)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref48031
%stackaddr$env-ref48032 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41949, i64 5)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48032
%stackaddr$prim48033 = alloca %struct.ScmObj*, align 8
%_95k40444 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46699)
store volatile %struct.ScmObj* %_95k40444, %struct.ScmObj** %stackaddr$prim48033, align 8
%stackaddr$prim48034 = alloca %struct.ScmObj*, align 8
%current_45args46700 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46699)
store volatile %struct.ScmObj* %current_45args46700, %struct.ScmObj** %stackaddr$prim48034, align 8
%stackaddr$prim48035 = alloca %struct.ScmObj*, align 8
%vs40213 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46700)
store volatile %struct.ScmObj* %vs40213, %struct.ScmObj** %stackaddr$prim48035, align 8
%stackaddr$makeclosure48036 = alloca %struct.ScmObj*, align 8
%fptrToInt48037 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41952 to i64
%ae41952 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt48037)
store volatile %struct.ScmObj* %ae41952, %struct.ScmObj** %stackaddr$makeclosure48036, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41952, %struct.ScmObj* %lsts_4340215, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41952, %struct.ScmObj* %k40438, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41952, %struct.ScmObj* %vs40213, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41952, %struct.ScmObj* %f40210, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41952, %struct.ScmObj* %acc40209, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41952, %struct.ScmObj* %_37foldr40128, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41952, %struct.ScmObj* %_37foldl40206, i64 6)
%ae41953 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48038 = alloca %struct.ScmObj*, align 8
%fptrToInt48039 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41954 to i64
%ae41954 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48039)
store volatile %struct.ScmObj* %ae41954, %struct.ScmObj** %stackaddr$makeclosure48038, align 8
%args46717$ae41952$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48040 = alloca %struct.ScmObj*, align 8
%args46717$ae41952$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41954, %struct.ScmObj* %args46717$ae41952$0)
store volatile %struct.ScmObj* %args46717$ae41952$1, %struct.ScmObj** %stackaddr$prim48040, align 8
%stackaddr$prim48041 = alloca %struct.ScmObj*, align 8
%args46717$ae41952$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41953, %struct.ScmObj* %args46717$ae41952$1)
store volatile %struct.ScmObj* %args46717$ae41952$2, %struct.ScmObj** %stackaddr$prim48041, align 8
%clofunc48042 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41952)
musttail call tailcc void %clofunc48042(%struct.ScmObj* %ae41952, %struct.ScmObj* %args46717$ae41952$2)
ret void
}

define tailcc void @proc_clo$ae41952(%struct.ScmObj* %env$ae41952,%struct.ScmObj* %current_45args46702) {
%stackaddr$env-ref48043 = alloca %struct.ScmObj*, align 8
%lsts_4340215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41952, i64 0)
store %struct.ScmObj* %lsts_4340215, %struct.ScmObj** %stackaddr$env-ref48043
%stackaddr$env-ref48044 = alloca %struct.ScmObj*, align 8
%k40438 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41952, i64 1)
store %struct.ScmObj* %k40438, %struct.ScmObj** %stackaddr$env-ref48044
%stackaddr$env-ref48045 = alloca %struct.ScmObj*, align 8
%vs40213 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41952, i64 2)
store %struct.ScmObj* %vs40213, %struct.ScmObj** %stackaddr$env-ref48045
%stackaddr$env-ref48046 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41952, i64 3)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref48046
%stackaddr$env-ref48047 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41952, i64 4)
store %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$env-ref48047
%stackaddr$env-ref48048 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41952, i64 5)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref48048
%stackaddr$env-ref48049 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41952, i64 6)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48049
%stackaddr$prim48050 = alloca %struct.ScmObj*, align 8
%_95k40445 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46702)
store volatile %struct.ScmObj* %_95k40445, %struct.ScmObj** %stackaddr$prim48050, align 8
%stackaddr$prim48051 = alloca %struct.ScmObj*, align 8
%current_45args46703 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46702)
store volatile %struct.ScmObj* %current_45args46703, %struct.ScmObj** %stackaddr$prim48051, align 8
%stackaddr$prim48052 = alloca %struct.ScmObj*, align 8
%anf_45bind40289 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46703)
store volatile %struct.ScmObj* %anf_45bind40289, %struct.ScmObj** %stackaddr$prim48052, align 8
%ae41975 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48053 = alloca %struct.ScmObj*, align 8
%anf_45bind40290 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40209, %struct.ScmObj* %ae41975)
store volatile %struct.ScmObj* %anf_45bind40290, %struct.ScmObj** %stackaddr$prim48053, align 8
%stackaddr$makeclosure48054 = alloca %struct.ScmObj*, align 8
%fptrToInt48055 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41977 to i64
%ae41977 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48055)
store volatile %struct.ScmObj* %ae41977, %struct.ScmObj** %stackaddr$makeclosure48054, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41977, %struct.ScmObj* %lsts_4340215, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41977, %struct.ScmObj* %k40438, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41977, %struct.ScmObj* %f40210, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41977, %struct.ScmObj* %_37foldl40206, i64 3)
%args46711$_37foldr40128$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48056 = alloca %struct.ScmObj*, align 8
%args46711$_37foldr40128$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs40213, %struct.ScmObj* %args46711$_37foldr40128$0)
store volatile %struct.ScmObj* %args46711$_37foldr40128$1, %struct.ScmObj** %stackaddr$prim48056, align 8
%stackaddr$prim48057 = alloca %struct.ScmObj*, align 8
%args46711$_37foldr40128$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40290, %struct.ScmObj* %args46711$_37foldr40128$1)
store volatile %struct.ScmObj* %args46711$_37foldr40128$2, %struct.ScmObj** %stackaddr$prim48057, align 8
%stackaddr$prim48058 = alloca %struct.ScmObj*, align 8
%args46711$_37foldr40128$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40289, %struct.ScmObj* %args46711$_37foldr40128$2)
store volatile %struct.ScmObj* %args46711$_37foldr40128$3, %struct.ScmObj** %stackaddr$prim48058, align 8
%stackaddr$prim48059 = alloca %struct.ScmObj*, align 8
%args46711$_37foldr40128$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41977, %struct.ScmObj* %args46711$_37foldr40128$3)
store volatile %struct.ScmObj* %args46711$_37foldr40128$4, %struct.ScmObj** %stackaddr$prim48059, align 8
%clofunc48060 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr40128)
musttail call tailcc void %clofunc48060(%struct.ScmObj* %_37foldr40128, %struct.ScmObj* %args46711$_37foldr40128$4)
ret void
}

define tailcc void @proc_clo$ae41977(%struct.ScmObj* %env$ae41977,%struct.ScmObj* %current_45args46705) {
%stackaddr$env-ref48061 = alloca %struct.ScmObj*, align 8
%lsts_4340215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41977, i64 0)
store %struct.ScmObj* %lsts_4340215, %struct.ScmObj** %stackaddr$env-ref48061
%stackaddr$env-ref48062 = alloca %struct.ScmObj*, align 8
%k40438 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41977, i64 1)
store %struct.ScmObj* %k40438, %struct.ScmObj** %stackaddr$env-ref48062
%stackaddr$env-ref48063 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41977, i64 2)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref48063
%stackaddr$env-ref48064 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41977, i64 3)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48064
%stackaddr$prim48065 = alloca %struct.ScmObj*, align 8
%_95k40446 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46705)
store volatile %struct.ScmObj* %_95k40446, %struct.ScmObj** %stackaddr$prim48065, align 8
%stackaddr$prim48066 = alloca %struct.ScmObj*, align 8
%current_45args46706 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46705)
store volatile %struct.ScmObj* %current_45args46706, %struct.ScmObj** %stackaddr$prim48066, align 8
%stackaddr$prim48067 = alloca %struct.ScmObj*, align 8
%anf_45bind40291 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46706)
store volatile %struct.ScmObj* %anf_45bind40291, %struct.ScmObj** %stackaddr$prim48067, align 8
%stackaddr$makeclosure48068 = alloca %struct.ScmObj*, align 8
%fptrToInt48069 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41981 to i64
%ae41981 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48069)
store volatile %struct.ScmObj* %ae41981, %struct.ScmObj** %stackaddr$makeclosure48068, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41981, %struct.ScmObj* %lsts_4340215, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41981, %struct.ScmObj* %k40438, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41981, %struct.ScmObj* %f40210, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41981, %struct.ScmObj* %_37foldl40206, i64 3)
%stackaddr$prim48070 = alloca %struct.ScmObj*, align 8
%cpsargs40449 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41981, %struct.ScmObj* %anf_45bind40291)
store volatile %struct.ScmObj* %cpsargs40449, %struct.ScmObj** %stackaddr$prim48070, align 8
%clofunc48071 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40210)
musttail call tailcc void %clofunc48071(%struct.ScmObj* %f40210, %struct.ScmObj* %cpsargs40449)
ret void
}

define tailcc void @proc_clo$ae41981(%struct.ScmObj* %env$ae41981,%struct.ScmObj* %current_45args46708) {
%stackaddr$env-ref48072 = alloca %struct.ScmObj*, align 8
%lsts_4340215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41981, i64 0)
store %struct.ScmObj* %lsts_4340215, %struct.ScmObj** %stackaddr$env-ref48072
%stackaddr$env-ref48073 = alloca %struct.ScmObj*, align 8
%k40438 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41981, i64 1)
store %struct.ScmObj* %k40438, %struct.ScmObj** %stackaddr$env-ref48073
%stackaddr$env-ref48074 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41981, i64 2)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref48074
%stackaddr$env-ref48075 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41981, i64 3)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48075
%stackaddr$prim48076 = alloca %struct.ScmObj*, align 8
%_95k40447 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46708)
store volatile %struct.ScmObj* %_95k40447, %struct.ScmObj** %stackaddr$prim48076, align 8
%stackaddr$prim48077 = alloca %struct.ScmObj*, align 8
%current_45args46709 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46708)
store volatile %struct.ScmObj* %current_45args46709, %struct.ScmObj** %stackaddr$prim48077, align 8
%stackaddr$prim48078 = alloca %struct.ScmObj*, align 8
%acc_4340217 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46709)
store volatile %struct.ScmObj* %acc_4340217, %struct.ScmObj** %stackaddr$prim48078, align 8
%stackaddr$prim48079 = alloca %struct.ScmObj*, align 8
%anf_45bind40292 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4340217, %struct.ScmObj* %lsts_4340215)
store volatile %struct.ScmObj* %anf_45bind40292, %struct.ScmObj** %stackaddr$prim48079, align 8
%stackaddr$prim48080 = alloca %struct.ScmObj*, align 8
%anf_45bind40293 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40210, %struct.ScmObj* %anf_45bind40292)
store volatile %struct.ScmObj* %anf_45bind40293, %struct.ScmObj** %stackaddr$prim48080, align 8
%stackaddr$prim48081 = alloca %struct.ScmObj*, align 8
%cpsargs40448 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40438, %struct.ScmObj* %anf_45bind40293)
store volatile %struct.ScmObj* %cpsargs40448, %struct.ScmObj** %stackaddr$prim48081, align 8
%clofunc48082 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl40206)
musttail call tailcc void %clofunc48082(%struct.ScmObj* %_37foldl40206, %struct.ScmObj* %cpsargs40448)
ret void
}

define tailcc void @proc_clo$ae41954(%struct.ScmObj* %env$ae41954,%struct.ScmObj* %current_45args46712) {
%stackaddr$prim48083 = alloca %struct.ScmObj*, align 8
%k40450 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46712)
store volatile %struct.ScmObj* %k40450, %struct.ScmObj** %stackaddr$prim48083, align 8
%stackaddr$prim48084 = alloca %struct.ScmObj*, align 8
%current_45args46713 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46712)
store volatile %struct.ScmObj* %current_45args46713, %struct.ScmObj** %stackaddr$prim48084, align 8
%stackaddr$prim48085 = alloca %struct.ScmObj*, align 8
%a40219 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46713)
store volatile %struct.ScmObj* %a40219, %struct.ScmObj** %stackaddr$prim48085, align 8
%stackaddr$prim48086 = alloca %struct.ScmObj*, align 8
%current_45args46714 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46713)
store volatile %struct.ScmObj* %current_45args46714, %struct.ScmObj** %stackaddr$prim48086, align 8
%stackaddr$prim48087 = alloca %struct.ScmObj*, align 8
%b40218 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46714)
store volatile %struct.ScmObj* %b40218, %struct.ScmObj** %stackaddr$prim48087, align 8
%stackaddr$prim48088 = alloca %struct.ScmObj*, align 8
%cpsprim40451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a40219, %struct.ScmObj* %b40218)
store volatile %struct.ScmObj* %cpsprim40451, %struct.ScmObj** %stackaddr$prim48088, align 8
%ae41958 = call %struct.ScmObj* @const_init_int(i64 0)
%args46716$k40450$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48089 = alloca %struct.ScmObj*, align 8
%args46716$k40450$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40451, %struct.ScmObj* %args46716$k40450$0)
store volatile %struct.ScmObj* %args46716$k40450$1, %struct.ScmObj** %stackaddr$prim48089, align 8
%stackaddr$prim48090 = alloca %struct.ScmObj*, align 8
%args46716$k40450$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41958, %struct.ScmObj* %args46716$k40450$1)
store volatile %struct.ScmObj* %args46716$k40450$2, %struct.ScmObj** %stackaddr$prim48090, align 8
%clofunc48091 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40450)
musttail call tailcc void %clofunc48091(%struct.ScmObj* %k40450, %struct.ScmObj* %args46716$k40450$2)
ret void
}

define tailcc void @proc_clo$ae41930(%struct.ScmObj* %env$ae41930,%struct.ScmObj* %current_45args46719) {
%stackaddr$prim48092 = alloca %struct.ScmObj*, align 8
%k40452 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46719)
store volatile %struct.ScmObj* %k40452, %struct.ScmObj** %stackaddr$prim48092, align 8
%stackaddr$prim48093 = alloca %struct.ScmObj*, align 8
%current_45args46720 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46719)
store volatile %struct.ScmObj* %current_45args46720, %struct.ScmObj** %stackaddr$prim48093, align 8
%stackaddr$prim48094 = alloca %struct.ScmObj*, align 8
%x40214 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46720)
store volatile %struct.ScmObj* %x40214, %struct.ScmObj** %stackaddr$prim48094, align 8
%stackaddr$prim48095 = alloca %struct.ScmObj*, align 8
%cpsprim40453 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40214)
store volatile %struct.ScmObj* %cpsprim40453, %struct.ScmObj** %stackaddr$prim48095, align 8
%ae41933 = call %struct.ScmObj* @const_init_int(i64 0)
%args46722$k40452$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48096 = alloca %struct.ScmObj*, align 8
%args46722$k40452$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40453, %struct.ScmObj* %args46722$k40452$0)
store volatile %struct.ScmObj* %args46722$k40452$1, %struct.ScmObj** %stackaddr$prim48096, align 8
%stackaddr$prim48097 = alloca %struct.ScmObj*, align 8
%args46722$k40452$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41933, %struct.ScmObj* %args46722$k40452$1)
store volatile %struct.ScmObj* %args46722$k40452$2, %struct.ScmObj** %stackaddr$prim48097, align 8
%clofunc48098 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40452)
musttail call tailcc void %clofunc48098(%struct.ScmObj* %k40452, %struct.ScmObj* %args46722$k40452$2)
ret void
}

define tailcc void @proc_clo$ae41906(%struct.ScmObj* %env$ae41906,%struct.ScmObj* %current_45args46725) {
%stackaddr$prim48099 = alloca %struct.ScmObj*, align 8
%k40454 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46725)
store volatile %struct.ScmObj* %k40454, %struct.ScmObj** %stackaddr$prim48099, align 8
%stackaddr$prim48100 = alloca %struct.ScmObj*, align 8
%current_45args46726 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46725)
store volatile %struct.ScmObj* %current_45args46726, %struct.ScmObj** %stackaddr$prim48100, align 8
%stackaddr$prim48101 = alloca %struct.ScmObj*, align 8
%x40216 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46726)
store volatile %struct.ScmObj* %x40216, %struct.ScmObj** %stackaddr$prim48101, align 8
%stackaddr$prim48102 = alloca %struct.ScmObj*, align 8
%cpsprim40455 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40216)
store volatile %struct.ScmObj* %cpsprim40455, %struct.ScmObj** %stackaddr$prim48102, align 8
%ae41909 = call %struct.ScmObj* @const_init_int(i64 0)
%args46728$k40454$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48103 = alloca %struct.ScmObj*, align 8
%args46728$k40454$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40455, %struct.ScmObj* %args46728$k40454$0)
store volatile %struct.ScmObj* %args46728$k40454$1, %struct.ScmObj** %stackaddr$prim48103, align 8
%stackaddr$prim48104 = alloca %struct.ScmObj*, align 8
%args46728$k40454$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41909, %struct.ScmObj* %args46728$k40454$1)
store volatile %struct.ScmObj* %args46728$k40454$2, %struct.ScmObj** %stackaddr$prim48104, align 8
%clofunc48105 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40454)
musttail call tailcc void %clofunc48105(%struct.ScmObj* %k40454, %struct.ScmObj* %args46728$k40454$2)
ret void
}

define tailcc void @proc_clo$ae41858(%struct.ScmObj* %env$ae41858,%struct.ScmObj* %current_45args46731) {
%stackaddr$prim48106 = alloca %struct.ScmObj*, align 8
%k40456 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46731)
store volatile %struct.ScmObj* %k40456, %struct.ScmObj** %stackaddr$prim48106, align 8
%stackaddr$prim48107 = alloca %struct.ScmObj*, align 8
%current_45args46732 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46731)
store volatile %struct.ScmObj* %current_45args46732, %struct.ScmObj** %stackaddr$prim48107, align 8
%stackaddr$prim48108 = alloca %struct.ScmObj*, align 8
%lst40212 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46732)
store volatile %struct.ScmObj* %lst40212, %struct.ScmObj** %stackaddr$prim48108, align 8
%stackaddr$prim48109 = alloca %struct.ScmObj*, align 8
%current_45args46733 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46732)
store volatile %struct.ScmObj* %current_45args46733, %struct.ScmObj** %stackaddr$prim48109, align 8
%stackaddr$prim48110 = alloca %struct.ScmObj*, align 8
%b40211 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46733)
store volatile %struct.ScmObj* %b40211, %struct.ScmObj** %stackaddr$prim48110, align 8
%truthy$cmp48111 = call i64 @is_truthy_value(%struct.ScmObj* %b40211)
%cmp$cmp48111 = icmp eq i64 %truthy$cmp48111, 1
br i1 %cmp$cmp48111, label %truebranch$cmp48111, label %falsebranch$cmp48111
truebranch$cmp48111:
%ae41861 = call %struct.ScmObj* @const_init_int(i64 0)
%args46735$k40456$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48112 = alloca %struct.ScmObj*, align 8
%args46735$k40456$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b40211, %struct.ScmObj* %args46735$k40456$0)
store volatile %struct.ScmObj* %args46735$k40456$1, %struct.ScmObj** %stackaddr$prim48112, align 8
%stackaddr$prim48113 = alloca %struct.ScmObj*, align 8
%args46735$k40456$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41861, %struct.ScmObj* %args46735$k40456$1)
store volatile %struct.ScmObj* %args46735$k40456$2, %struct.ScmObj** %stackaddr$prim48113, align 8
%clofunc48114 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40456)
musttail call tailcc void %clofunc48114(%struct.ScmObj* %k40456, %struct.ScmObj* %args46735$k40456$2)
ret void
falsebranch$cmp48111:
%stackaddr$prim48115 = alloca %struct.ScmObj*, align 8
%cpsprim40457 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40212)
store volatile %struct.ScmObj* %cpsprim40457, %struct.ScmObj** %stackaddr$prim48115, align 8
%ae41868 = call %struct.ScmObj* @const_init_int(i64 0)
%args46736$k40456$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48116 = alloca %struct.ScmObj*, align 8
%args46736$k40456$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40457, %struct.ScmObj* %args46736$k40456$0)
store volatile %struct.ScmObj* %args46736$k40456$1, %struct.ScmObj** %stackaddr$prim48116, align 8
%stackaddr$prim48117 = alloca %struct.ScmObj*, align 8
%args46736$k40456$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41868, %struct.ScmObj* %args46736$k40456$1)
store volatile %struct.ScmObj* %args46736$k40456$2, %struct.ScmObj** %stackaddr$prim48117, align 8
%clofunc48118 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40456)
musttail call tailcc void %clofunc48118(%struct.ScmObj* %k40456, %struct.ScmObj* %args46736$k40456$2)
ret void
}

define tailcc void @proc_clo$ae41699(%struct.ScmObj* %env$ae41699,%struct.ScmObj* %args4015040458) {
%stackaddr$env-ref48119 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41699, i64 0)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref48119
%stackaddr$env-ref48120 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41699, i64 1)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref48120
%stackaddr$env-ref48121 = alloca %struct.ScmObj*, align 8
%_37drop_45right40142 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41699, i64 2)
store %struct.ScmObj* %_37drop_45right40142, %struct.ScmObj** %stackaddr$env-ref48121
%stackaddr$prim48122 = alloca %struct.ScmObj*, align 8
%k40459 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4015040458)
store volatile %struct.ScmObj* %k40459, %struct.ScmObj** %stackaddr$prim48122, align 8
%stackaddr$prim48123 = alloca %struct.ScmObj*, align 8
%args40150 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4015040458)
store volatile %struct.ScmObj* %args40150, %struct.ScmObj** %stackaddr$prim48123, align 8
%stackaddr$prim48124 = alloca %struct.ScmObj*, align 8
%f40152 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40150)
store volatile %struct.ScmObj* %f40152, %struct.ScmObj** %stackaddr$prim48124, align 8
%stackaddr$prim48125 = alloca %struct.ScmObj*, align 8
%lsts40151 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40150)
store volatile %struct.ScmObj* %lsts40151, %struct.ScmObj** %stackaddr$prim48125, align 8
%stackaddr$makeclosure48126 = alloca %struct.ScmObj*, align 8
%fptrToInt48127 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41704 to i64
%ae41704 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48127)
store volatile %struct.ScmObj* %ae41704, %struct.ScmObj** %stackaddr$makeclosure48126, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41704, %struct.ScmObj* %lsts40151, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41704, %struct.ScmObj* %_37foldr40128, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41704, %struct.ScmObj* %k40459, i64 2)
%ae41705 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48128 = alloca %struct.ScmObj*, align 8
%fptrToInt48129 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41706 to i64
%ae41706 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48129)
store volatile %struct.ScmObj* %ae41706, %struct.ScmObj** %stackaddr$makeclosure48128, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41706, %struct.ScmObj* %_37last40145, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41706, %struct.ScmObj* %_37drop_45right40142, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41706, %struct.ScmObj* %f40152, i64 2)
%args46755$ae41704$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48130 = alloca %struct.ScmObj*, align 8
%args46755$ae41704$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41706, %struct.ScmObj* %args46755$ae41704$0)
store volatile %struct.ScmObj* %args46755$ae41704$1, %struct.ScmObj** %stackaddr$prim48130, align 8
%stackaddr$prim48131 = alloca %struct.ScmObj*, align 8
%args46755$ae41704$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41705, %struct.ScmObj* %args46755$ae41704$1)
store volatile %struct.ScmObj* %args46755$ae41704$2, %struct.ScmObj** %stackaddr$prim48131, align 8
%clofunc48132 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41704)
musttail call tailcc void %clofunc48132(%struct.ScmObj* %ae41704, %struct.ScmObj* %args46755$ae41704$2)
ret void
}

define tailcc void @proc_clo$ae41704(%struct.ScmObj* %env$ae41704,%struct.ScmObj* %current_45args46740) {
%stackaddr$env-ref48133 = alloca %struct.ScmObj*, align 8
%lsts40151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41704, i64 0)
store %struct.ScmObj* %lsts40151, %struct.ScmObj** %stackaddr$env-ref48133
%stackaddr$env-ref48134 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41704, i64 1)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref48134
%stackaddr$env-ref48135 = alloca %struct.ScmObj*, align 8
%k40459 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41704, i64 2)
store %struct.ScmObj* %k40459, %struct.ScmObj** %stackaddr$env-ref48135
%stackaddr$prim48136 = alloca %struct.ScmObj*, align 8
%_95k40460 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46740)
store volatile %struct.ScmObj* %_95k40460, %struct.ScmObj** %stackaddr$prim48136, align 8
%stackaddr$prim48137 = alloca %struct.ScmObj*, align 8
%current_45args46741 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46740)
store volatile %struct.ScmObj* %current_45args46741, %struct.ScmObj** %stackaddr$prim48137, align 8
%stackaddr$prim48138 = alloca %struct.ScmObj*, align 8
%anf_45bind40280 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46741)
store volatile %struct.ScmObj* %anf_45bind40280, %struct.ScmObj** %stackaddr$prim48138, align 8
%ae41767 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48139 = alloca %struct.ScmObj*, align 8
%anf_45bind40281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41767, %struct.ScmObj* %lsts40151)
store volatile %struct.ScmObj* %anf_45bind40281, %struct.ScmObj** %stackaddr$prim48139, align 8
%stackaddr$prim48140 = alloca %struct.ScmObj*, align 8
%anf_45bind40282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40280, %struct.ScmObj* %anf_45bind40281)
store volatile %struct.ScmObj* %anf_45bind40282, %struct.ScmObj** %stackaddr$prim48140, align 8
%stackaddr$prim48141 = alloca %struct.ScmObj*, align 8
%cpsargs40461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40459, %struct.ScmObj* %anf_45bind40282)
store volatile %struct.ScmObj* %cpsargs40461, %struct.ScmObj** %stackaddr$prim48141, align 8
%clofunc48142 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr40128)
musttail call tailcc void %clofunc48142(%struct.ScmObj* %_37foldr40128, %struct.ScmObj* %cpsargs40461)
ret void
}

define tailcc void @proc_clo$ae41706(%struct.ScmObj* %env$ae41706,%struct.ScmObj* %fargs4015340462) {
%stackaddr$env-ref48143 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41706, i64 0)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref48143
%stackaddr$env-ref48144 = alloca %struct.ScmObj*, align 8
%_37drop_45right40142 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41706, i64 1)
store %struct.ScmObj* %_37drop_45right40142, %struct.ScmObj** %stackaddr$env-ref48144
%stackaddr$env-ref48145 = alloca %struct.ScmObj*, align 8
%f40152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41706, i64 2)
store %struct.ScmObj* %f40152, %struct.ScmObj** %stackaddr$env-ref48145
%stackaddr$prim48146 = alloca %struct.ScmObj*, align 8
%k40463 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs4015340462)
store volatile %struct.ScmObj* %k40463, %struct.ScmObj** %stackaddr$prim48146, align 8
%stackaddr$prim48147 = alloca %struct.ScmObj*, align 8
%fargs40153 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs4015340462)
store volatile %struct.ScmObj* %fargs40153, %struct.ScmObj** %stackaddr$prim48147, align 8
%stackaddr$makeclosure48148 = alloca %struct.ScmObj*, align 8
%fptrToInt48149 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41710 to i64
%ae41710 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48149)
store volatile %struct.ScmObj* %ae41710, %struct.ScmObj** %stackaddr$makeclosure48148, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41710, %struct.ScmObj* %_37last40145, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41710, %struct.ScmObj* %k40463, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41710, %struct.ScmObj* %fargs40153, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41710, %struct.ScmObj* %f40152, i64 3)
%ae41712 = call %struct.ScmObj* @const_init_int(i64 1)
%args46754$_37drop_45right40142$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48150 = alloca %struct.ScmObj*, align 8
%args46754$_37drop_45right40142$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41712, %struct.ScmObj* %args46754$_37drop_45right40142$0)
store volatile %struct.ScmObj* %args46754$_37drop_45right40142$1, %struct.ScmObj** %stackaddr$prim48150, align 8
%stackaddr$prim48151 = alloca %struct.ScmObj*, align 8
%args46754$_37drop_45right40142$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs40153, %struct.ScmObj* %args46754$_37drop_45right40142$1)
store volatile %struct.ScmObj* %args46754$_37drop_45right40142$2, %struct.ScmObj** %stackaddr$prim48151, align 8
%stackaddr$prim48152 = alloca %struct.ScmObj*, align 8
%args46754$_37drop_45right40142$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41710, %struct.ScmObj* %args46754$_37drop_45right40142$2)
store volatile %struct.ScmObj* %args46754$_37drop_45right40142$3, %struct.ScmObj** %stackaddr$prim48152, align 8
%clofunc48153 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right40142)
musttail call tailcc void %clofunc48153(%struct.ScmObj* %_37drop_45right40142, %struct.ScmObj* %args46754$_37drop_45right40142$3)
ret void
}

define tailcc void @proc_clo$ae41710(%struct.ScmObj* %env$ae41710,%struct.ScmObj* %current_45args46743) {
%stackaddr$env-ref48154 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41710, i64 0)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref48154
%stackaddr$env-ref48155 = alloca %struct.ScmObj*, align 8
%k40463 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41710, i64 1)
store %struct.ScmObj* %k40463, %struct.ScmObj** %stackaddr$env-ref48155
%stackaddr$env-ref48156 = alloca %struct.ScmObj*, align 8
%fargs40153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41710, i64 2)
store %struct.ScmObj* %fargs40153, %struct.ScmObj** %stackaddr$env-ref48156
%stackaddr$env-ref48157 = alloca %struct.ScmObj*, align 8
%f40152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41710, i64 3)
store %struct.ScmObj* %f40152, %struct.ScmObj** %stackaddr$env-ref48157
%stackaddr$prim48158 = alloca %struct.ScmObj*, align 8
%_95k40464 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46743)
store volatile %struct.ScmObj* %_95k40464, %struct.ScmObj** %stackaddr$prim48158, align 8
%stackaddr$prim48159 = alloca %struct.ScmObj*, align 8
%current_45args46744 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46743)
store volatile %struct.ScmObj* %current_45args46744, %struct.ScmObj** %stackaddr$prim48159, align 8
%stackaddr$prim48160 = alloca %struct.ScmObj*, align 8
%anf_45bind40277 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46744)
store volatile %struct.ScmObj* %anf_45bind40277, %struct.ScmObj** %stackaddr$prim48160, align 8
%stackaddr$makeclosure48161 = alloca %struct.ScmObj*, align 8
%fptrToInt48162 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41717 to i64
%ae41717 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48162)
store volatile %struct.ScmObj* %ae41717, %struct.ScmObj** %stackaddr$makeclosure48161, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41717, %struct.ScmObj* %_37last40145, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41717, %struct.ScmObj* %k40463, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41717, %struct.ScmObj* %fargs40153, i64 2)
%stackaddr$prim48163 = alloca %struct.ScmObj*, align 8
%cpsargs40468 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41717, %struct.ScmObj* %anf_45bind40277)
store volatile %struct.ScmObj* %cpsargs40468, %struct.ScmObj** %stackaddr$prim48163, align 8
%clofunc48164 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40152)
musttail call tailcc void %clofunc48164(%struct.ScmObj* %f40152, %struct.ScmObj* %cpsargs40468)
ret void
}

define tailcc void @proc_clo$ae41717(%struct.ScmObj* %env$ae41717,%struct.ScmObj* %current_45args46746) {
%stackaddr$env-ref48165 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41717, i64 0)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref48165
%stackaddr$env-ref48166 = alloca %struct.ScmObj*, align 8
%k40463 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41717, i64 1)
store %struct.ScmObj* %k40463, %struct.ScmObj** %stackaddr$env-ref48166
%stackaddr$env-ref48167 = alloca %struct.ScmObj*, align 8
%fargs40153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41717, i64 2)
store %struct.ScmObj* %fargs40153, %struct.ScmObj** %stackaddr$env-ref48167
%stackaddr$prim48168 = alloca %struct.ScmObj*, align 8
%_95k40465 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46746)
store volatile %struct.ScmObj* %_95k40465, %struct.ScmObj** %stackaddr$prim48168, align 8
%stackaddr$prim48169 = alloca %struct.ScmObj*, align 8
%current_45args46747 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46746)
store volatile %struct.ScmObj* %current_45args46747, %struct.ScmObj** %stackaddr$prim48169, align 8
%stackaddr$prim48170 = alloca %struct.ScmObj*, align 8
%anf_45bind40278 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46747)
store volatile %struct.ScmObj* %anf_45bind40278, %struct.ScmObj** %stackaddr$prim48170, align 8
%stackaddr$makeclosure48171 = alloca %struct.ScmObj*, align 8
%fptrToInt48172 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41722 to i64
%ae41722 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48172)
store volatile %struct.ScmObj* %ae41722, %struct.ScmObj** %stackaddr$makeclosure48171, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41722, %struct.ScmObj* %anf_45bind40278, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41722, %struct.ScmObj* %k40463, i64 1)
%args46753$_37last40145$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48173 = alloca %struct.ScmObj*, align 8
%args46753$_37last40145$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs40153, %struct.ScmObj* %args46753$_37last40145$0)
store volatile %struct.ScmObj* %args46753$_37last40145$1, %struct.ScmObj** %stackaddr$prim48173, align 8
%stackaddr$prim48174 = alloca %struct.ScmObj*, align 8
%args46753$_37last40145$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41722, %struct.ScmObj* %args46753$_37last40145$1)
store volatile %struct.ScmObj* %args46753$_37last40145$2, %struct.ScmObj** %stackaddr$prim48174, align 8
%clofunc48175 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last40145)
musttail call tailcc void %clofunc48175(%struct.ScmObj* %_37last40145, %struct.ScmObj* %args46753$_37last40145$2)
ret void
}

define tailcc void @proc_clo$ae41722(%struct.ScmObj* %env$ae41722,%struct.ScmObj* %current_45args46749) {
%stackaddr$env-ref48176 = alloca %struct.ScmObj*, align 8
%anf_45bind40278 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41722, i64 0)
store %struct.ScmObj* %anf_45bind40278, %struct.ScmObj** %stackaddr$env-ref48176
%stackaddr$env-ref48177 = alloca %struct.ScmObj*, align 8
%k40463 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41722, i64 1)
store %struct.ScmObj* %k40463, %struct.ScmObj** %stackaddr$env-ref48177
%stackaddr$prim48178 = alloca %struct.ScmObj*, align 8
%_95k40466 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46749)
store volatile %struct.ScmObj* %_95k40466, %struct.ScmObj** %stackaddr$prim48178, align 8
%stackaddr$prim48179 = alloca %struct.ScmObj*, align 8
%current_45args46750 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46749)
store volatile %struct.ScmObj* %current_45args46750, %struct.ScmObj** %stackaddr$prim48179, align 8
%stackaddr$prim48180 = alloca %struct.ScmObj*, align 8
%anf_45bind40279 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46750)
store volatile %struct.ScmObj* %anf_45bind40279, %struct.ScmObj** %stackaddr$prim48180, align 8
%stackaddr$prim48181 = alloca %struct.ScmObj*, align 8
%cpsprim40467 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40278, %struct.ScmObj* %anf_45bind40279)
store volatile %struct.ScmObj* %cpsprim40467, %struct.ScmObj** %stackaddr$prim48181, align 8
%ae41727 = call %struct.ScmObj* @const_init_int(i64 0)
%args46752$k40463$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48182 = alloca %struct.ScmObj*, align 8
%args46752$k40463$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40467, %struct.ScmObj* %args46752$k40463$0)
store volatile %struct.ScmObj* %args46752$k40463$1, %struct.ScmObj** %stackaddr$prim48182, align 8
%stackaddr$prim48183 = alloca %struct.ScmObj*, align 8
%args46752$k40463$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41727, %struct.ScmObj* %args46752$k40463$1)
store volatile %struct.ScmObj* %args46752$k40463$2, %struct.ScmObj** %stackaddr$prim48183, align 8
%clofunc48184 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40463)
musttail call tailcc void %clofunc48184(%struct.ScmObj* %k40463, %struct.ScmObj* %args46752$k40463$2)
ret void
}

define tailcc void @proc_clo$ae41622(%struct.ScmObj* %env$ae41622,%struct.ScmObj* %current_45args46757) {
%stackaddr$env-ref48185 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41622, i64 0)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48185
%stackaddr$prim48186 = alloca %struct.ScmObj*, align 8
%k40469 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46757)
store volatile %struct.ScmObj* %k40469, %struct.ScmObj** %stackaddr$prim48186, align 8
%stackaddr$prim48187 = alloca %struct.ScmObj*, align 8
%current_45args46758 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46757)
store volatile %struct.ScmObj* %current_45args46758, %struct.ScmObj** %stackaddr$prim48187, align 8
%stackaddr$prim48188 = alloca %struct.ScmObj*, align 8
%f40156 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46758)
store volatile %struct.ScmObj* %f40156, %struct.ScmObj** %stackaddr$prim48188, align 8
%stackaddr$prim48189 = alloca %struct.ScmObj*, align 8
%current_45args46759 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46758)
store volatile %struct.ScmObj* %current_45args46759, %struct.ScmObj** %stackaddr$prim48189, align 8
%stackaddr$prim48190 = alloca %struct.ScmObj*, align 8
%lst40155 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46759)
store volatile %struct.ScmObj* %lst40155, %struct.ScmObj** %stackaddr$prim48190, align 8
%stackaddr$makeclosure48191 = alloca %struct.ScmObj*, align 8
%fptrToInt48192 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41623 to i64
%ae41623 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48192)
store volatile %struct.ScmObj* %ae41623, %struct.ScmObj** %stackaddr$makeclosure48191, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41623, %struct.ScmObj* %lst40155, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41623, %struct.ScmObj* %_37foldr140123, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41623, %struct.ScmObj* %k40469, i64 2)
%ae41624 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48193 = alloca %struct.ScmObj*, align 8
%fptrToInt48194 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41625 to i64
%ae41625 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48194)
store volatile %struct.ScmObj* %ae41625, %struct.ScmObj** %stackaddr$makeclosure48193, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41625, %struct.ScmObj* %f40156, i64 0)
%args46774$ae41623$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48195 = alloca %struct.ScmObj*, align 8
%args46774$ae41623$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41625, %struct.ScmObj* %args46774$ae41623$0)
store volatile %struct.ScmObj* %args46774$ae41623$1, %struct.ScmObj** %stackaddr$prim48195, align 8
%stackaddr$prim48196 = alloca %struct.ScmObj*, align 8
%args46774$ae41623$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41624, %struct.ScmObj* %args46774$ae41623$1)
store volatile %struct.ScmObj* %args46774$ae41623$2, %struct.ScmObj** %stackaddr$prim48196, align 8
%clofunc48197 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41623)
musttail call tailcc void %clofunc48197(%struct.ScmObj* %ae41623, %struct.ScmObj* %args46774$ae41623$2)
ret void
}

define tailcc void @proc_clo$ae41623(%struct.ScmObj* %env$ae41623,%struct.ScmObj* %current_45args46761) {
%stackaddr$env-ref48198 = alloca %struct.ScmObj*, align 8
%lst40155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41623, i64 0)
store %struct.ScmObj* %lst40155, %struct.ScmObj** %stackaddr$env-ref48198
%stackaddr$env-ref48199 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41623, i64 1)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48199
%stackaddr$env-ref48200 = alloca %struct.ScmObj*, align 8
%k40469 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41623, i64 2)
store %struct.ScmObj* %k40469, %struct.ScmObj** %stackaddr$env-ref48200
%stackaddr$prim48201 = alloca %struct.ScmObj*, align 8
%_95k40470 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46761)
store volatile %struct.ScmObj* %_95k40470, %struct.ScmObj** %stackaddr$prim48201, align 8
%stackaddr$prim48202 = alloca %struct.ScmObj*, align 8
%current_45args46762 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46761)
store volatile %struct.ScmObj* %current_45args46762, %struct.ScmObj** %stackaddr$prim48202, align 8
%stackaddr$prim48203 = alloca %struct.ScmObj*, align 8
%anf_45bind40276 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46762)
store volatile %struct.ScmObj* %anf_45bind40276, %struct.ScmObj** %stackaddr$prim48203, align 8
%ae41657 = call %struct.ScmObj* @const_init_null()
%args46764$_37foldr140123$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48204 = alloca %struct.ScmObj*, align 8
%args46764$_37foldr140123$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40155, %struct.ScmObj* %args46764$_37foldr140123$0)
store volatile %struct.ScmObj* %args46764$_37foldr140123$1, %struct.ScmObj** %stackaddr$prim48204, align 8
%stackaddr$prim48205 = alloca %struct.ScmObj*, align 8
%args46764$_37foldr140123$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41657, %struct.ScmObj* %args46764$_37foldr140123$1)
store volatile %struct.ScmObj* %args46764$_37foldr140123$2, %struct.ScmObj** %stackaddr$prim48205, align 8
%stackaddr$prim48206 = alloca %struct.ScmObj*, align 8
%args46764$_37foldr140123$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40276, %struct.ScmObj* %args46764$_37foldr140123$2)
store volatile %struct.ScmObj* %args46764$_37foldr140123$3, %struct.ScmObj** %stackaddr$prim48206, align 8
%stackaddr$prim48207 = alloca %struct.ScmObj*, align 8
%args46764$_37foldr140123$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40469, %struct.ScmObj* %args46764$_37foldr140123$3)
store volatile %struct.ScmObj* %args46764$_37foldr140123$4, %struct.ScmObj** %stackaddr$prim48207, align 8
%clofunc48208 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140123)
musttail call tailcc void %clofunc48208(%struct.ScmObj* %_37foldr140123, %struct.ScmObj* %args46764$_37foldr140123$4)
ret void
}

define tailcc void @proc_clo$ae41625(%struct.ScmObj* %env$ae41625,%struct.ScmObj* %current_45args46765) {
%stackaddr$env-ref48209 = alloca %struct.ScmObj*, align 8
%f40156 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41625, i64 0)
store %struct.ScmObj* %f40156, %struct.ScmObj** %stackaddr$env-ref48209
%stackaddr$prim48210 = alloca %struct.ScmObj*, align 8
%k40471 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46765)
store volatile %struct.ScmObj* %k40471, %struct.ScmObj** %stackaddr$prim48210, align 8
%stackaddr$prim48211 = alloca %struct.ScmObj*, align 8
%current_45args46766 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46765)
store volatile %struct.ScmObj* %current_45args46766, %struct.ScmObj** %stackaddr$prim48211, align 8
%stackaddr$prim48212 = alloca %struct.ScmObj*, align 8
%v40158 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46766)
store volatile %struct.ScmObj* %v40158, %struct.ScmObj** %stackaddr$prim48212, align 8
%stackaddr$prim48213 = alloca %struct.ScmObj*, align 8
%current_45args46767 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46766)
store volatile %struct.ScmObj* %current_45args46767, %struct.ScmObj** %stackaddr$prim48213, align 8
%stackaddr$prim48214 = alloca %struct.ScmObj*, align 8
%r40157 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46767)
store volatile %struct.ScmObj* %r40157, %struct.ScmObj** %stackaddr$prim48214, align 8
%stackaddr$makeclosure48215 = alloca %struct.ScmObj*, align 8
%fptrToInt48216 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41627 to i64
%ae41627 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48216)
store volatile %struct.ScmObj* %ae41627, %struct.ScmObj** %stackaddr$makeclosure48215, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41627, %struct.ScmObj* %k40471, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41627, %struct.ScmObj* %r40157, i64 1)
%args46773$f40156$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48217 = alloca %struct.ScmObj*, align 8
%args46773$f40156$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v40158, %struct.ScmObj* %args46773$f40156$0)
store volatile %struct.ScmObj* %args46773$f40156$1, %struct.ScmObj** %stackaddr$prim48217, align 8
%stackaddr$prim48218 = alloca %struct.ScmObj*, align 8
%args46773$f40156$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41627, %struct.ScmObj* %args46773$f40156$1)
store volatile %struct.ScmObj* %args46773$f40156$2, %struct.ScmObj** %stackaddr$prim48218, align 8
%clofunc48219 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40156)
musttail call tailcc void %clofunc48219(%struct.ScmObj* %f40156, %struct.ScmObj* %args46773$f40156$2)
ret void
}

define tailcc void @proc_clo$ae41627(%struct.ScmObj* %env$ae41627,%struct.ScmObj* %current_45args46769) {
%stackaddr$env-ref48220 = alloca %struct.ScmObj*, align 8
%k40471 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41627, i64 0)
store %struct.ScmObj* %k40471, %struct.ScmObj** %stackaddr$env-ref48220
%stackaddr$env-ref48221 = alloca %struct.ScmObj*, align 8
%r40157 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41627, i64 1)
store %struct.ScmObj* %r40157, %struct.ScmObj** %stackaddr$env-ref48221
%stackaddr$prim48222 = alloca %struct.ScmObj*, align 8
%_95k40472 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46769)
store volatile %struct.ScmObj* %_95k40472, %struct.ScmObj** %stackaddr$prim48222, align 8
%stackaddr$prim48223 = alloca %struct.ScmObj*, align 8
%current_45args46770 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46769)
store volatile %struct.ScmObj* %current_45args46770, %struct.ScmObj** %stackaddr$prim48223, align 8
%stackaddr$prim48224 = alloca %struct.ScmObj*, align 8
%anf_45bind40275 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46770)
store volatile %struct.ScmObj* %anf_45bind40275, %struct.ScmObj** %stackaddr$prim48224, align 8
%stackaddr$prim48225 = alloca %struct.ScmObj*, align 8
%cpsprim40473 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40275, %struct.ScmObj* %r40157)
store volatile %struct.ScmObj* %cpsprim40473, %struct.ScmObj** %stackaddr$prim48225, align 8
%ae41632 = call %struct.ScmObj* @const_init_int(i64 0)
%args46772$k40471$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48226 = alloca %struct.ScmObj*, align 8
%args46772$k40471$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40473, %struct.ScmObj* %args46772$k40471$0)
store volatile %struct.ScmObj* %args46772$k40471$1, %struct.ScmObj** %stackaddr$prim48226, align 8
%stackaddr$prim48227 = alloca %struct.ScmObj*, align 8
%args46772$k40471$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41632, %struct.ScmObj* %args46772$k40471$1)
store volatile %struct.ScmObj* %args46772$k40471$2, %struct.ScmObj** %stackaddr$prim48227, align 8
%clofunc48228 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40471)
musttail call tailcc void %clofunc48228(%struct.ScmObj* %k40471, %struct.ScmObj* %args46772$k40471$2)
ret void
}

define tailcc void @proc_clo$ae41236(%struct.ScmObj* %env$ae41236,%struct.ScmObj* %current_45args46777) {
%stackaddr$env-ref48229 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41236, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref48229
%stackaddr$env-ref48230 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41236, i64 1)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48230
%stackaddr$prim48231 = alloca %struct.ScmObj*, align 8
%k40474 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46777)
store volatile %struct.ScmObj* %k40474, %struct.ScmObj** %stackaddr$prim48231, align 8
%stackaddr$prim48232 = alloca %struct.ScmObj*, align 8
%current_45args46778 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46777)
store volatile %struct.ScmObj* %current_45args46778, %struct.ScmObj** %stackaddr$prim48232, align 8
%stackaddr$prim48233 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46778)
store volatile %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$prim48233, align 8
%ae41238 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48234 = alloca %struct.ScmObj*, align 8
%fptrToInt48235 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41239 to i64
%ae41239 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48235)
store volatile %struct.ScmObj* %ae41239, %struct.ScmObj** %stackaddr$makeclosure48234, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41239, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41239, %struct.ScmObj* %_37foldr40129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41239, %struct.ScmObj* %_37foldr140123, i64 2)
%args46835$k40474$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48236 = alloca %struct.ScmObj*, align 8
%args46835$k40474$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41239, %struct.ScmObj* %args46835$k40474$0)
store volatile %struct.ScmObj* %args46835$k40474$1, %struct.ScmObj** %stackaddr$prim48236, align 8
%stackaddr$prim48237 = alloca %struct.ScmObj*, align 8
%args46835$k40474$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41238, %struct.ScmObj* %args46835$k40474$1)
store volatile %struct.ScmObj* %args46835$k40474$2, %struct.ScmObj** %stackaddr$prim48237, align 8
%clofunc48238 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40474)
musttail call tailcc void %clofunc48238(%struct.ScmObj* %k40474, %struct.ScmObj* %args46835$k40474$2)
ret void
}

define tailcc void @proc_clo$ae41239(%struct.ScmObj* %env$ae41239,%struct.ScmObj* %args4013040475) {
%stackaddr$env-ref48239 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41239, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref48239
%stackaddr$env-ref48240 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41239, i64 1)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref48240
%stackaddr$env-ref48241 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41239, i64 2)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48241
%stackaddr$prim48242 = alloca %struct.ScmObj*, align 8
%k40476 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4013040475)
store volatile %struct.ScmObj* %k40476, %struct.ScmObj** %stackaddr$prim48242, align 8
%stackaddr$prim48243 = alloca %struct.ScmObj*, align 8
%args40130 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4013040475)
store volatile %struct.ScmObj* %args40130, %struct.ScmObj** %stackaddr$prim48243, align 8
%stackaddr$prim48244 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40130)
store volatile %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$prim48244, align 8
%stackaddr$prim48245 = alloca %struct.ScmObj*, align 8
%anf_45bind40262 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40130)
store volatile %struct.ScmObj* %anf_45bind40262, %struct.ScmObj** %stackaddr$prim48245, align 8
%stackaddr$prim48246 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40262)
store volatile %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$prim48246, align 8
%stackaddr$prim48247 = alloca %struct.ScmObj*, align 8
%anf_45bind40263 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40130)
store volatile %struct.ScmObj* %anf_45bind40263, %struct.ScmObj** %stackaddr$prim48247, align 8
%stackaddr$prim48248 = alloca %struct.ScmObj*, align 8
%lsts40131 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40263)
store volatile %struct.ScmObj* %lsts40131, %struct.ScmObj** %stackaddr$prim48248, align 8
%stackaddr$makeclosure48249 = alloca %struct.ScmObj*, align 8
%fptrToInt48250 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41247 to i64
%ae41247 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt48250)
store volatile %struct.ScmObj* %ae41247, %struct.ScmObj** %stackaddr$makeclosure48249, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41247, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41247, %struct.ScmObj* %f40133, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41247, %struct.ScmObj* %acc40132, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41247, %struct.ScmObj* %lsts40131, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41247, %struct.ScmObj* %_37foldr40129, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41247, %struct.ScmObj* %k40476, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41247, %struct.ScmObj* %_37foldr140123, i64 6)
%ae41248 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48251 = alloca %struct.ScmObj*, align 8
%fptrToInt48252 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41249 to i64
%ae41249 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48252)
store volatile %struct.ScmObj* %ae41249, %struct.ScmObj** %stackaddr$makeclosure48251, align 8
%args46834$ae41247$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48253 = alloca %struct.ScmObj*, align 8
%args46834$ae41247$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41249, %struct.ScmObj* %args46834$ae41247$0)
store volatile %struct.ScmObj* %args46834$ae41247$1, %struct.ScmObj** %stackaddr$prim48253, align 8
%stackaddr$prim48254 = alloca %struct.ScmObj*, align 8
%args46834$ae41247$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41248, %struct.ScmObj* %args46834$ae41247$1)
store volatile %struct.ScmObj* %args46834$ae41247$2, %struct.ScmObj** %stackaddr$prim48254, align 8
%clofunc48255 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41247)
musttail call tailcc void %clofunc48255(%struct.ScmObj* %ae41247, %struct.ScmObj* %args46834$ae41247$2)
ret void
}

define tailcc void @proc_clo$ae41247(%struct.ScmObj* %env$ae41247,%struct.ScmObj* %current_45args46780) {
%stackaddr$env-ref48256 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41247, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref48256
%stackaddr$env-ref48257 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41247, i64 1)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref48257
%stackaddr$env-ref48258 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41247, i64 2)
store %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$env-ref48258
%stackaddr$env-ref48259 = alloca %struct.ScmObj*, align 8
%lsts40131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41247, i64 3)
store %struct.ScmObj* %lsts40131, %struct.ScmObj** %stackaddr$env-ref48259
%stackaddr$env-ref48260 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41247, i64 4)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref48260
%stackaddr$env-ref48261 = alloca %struct.ScmObj*, align 8
%k40476 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41247, i64 5)
store %struct.ScmObj* %k40476, %struct.ScmObj** %stackaddr$env-ref48261
%stackaddr$env-ref48262 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41247, i64 6)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48262
%stackaddr$prim48263 = alloca %struct.ScmObj*, align 8
%_95k40477 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46780)
store volatile %struct.ScmObj* %_95k40477, %struct.ScmObj** %stackaddr$prim48263, align 8
%stackaddr$prim48264 = alloca %struct.ScmObj*, align 8
%current_45args46781 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46780)
store volatile %struct.ScmObj* %current_45args46781, %struct.ScmObj** %stackaddr$prim48264, align 8
%stackaddr$prim48265 = alloca %struct.ScmObj*, align 8
%anf_45bind40264 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46781)
store volatile %struct.ScmObj* %anf_45bind40264, %struct.ScmObj** %stackaddr$prim48265, align 8
%stackaddr$makeclosure48266 = alloca %struct.ScmObj*, align 8
%fptrToInt48267 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41279 to i64
%ae41279 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt48267)
store volatile %struct.ScmObj* %ae41279, %struct.ScmObj** %stackaddr$makeclosure48266, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41279, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41279, %struct.ScmObj* %f40133, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41279, %struct.ScmObj* %acc40132, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41279, %struct.ScmObj* %lsts40131, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41279, %struct.ScmObj* %_37foldr40129, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41279, %struct.ScmObj* %k40476, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41279, %struct.ScmObj* %_37foldr140123, i64 6)
%ae41281 = call %struct.ScmObj* @const_init_false()
%args46827$_37foldr140123$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48268 = alloca %struct.ScmObj*, align 8
%args46827$_37foldr140123$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40131, %struct.ScmObj* %args46827$_37foldr140123$0)
store volatile %struct.ScmObj* %args46827$_37foldr140123$1, %struct.ScmObj** %stackaddr$prim48268, align 8
%stackaddr$prim48269 = alloca %struct.ScmObj*, align 8
%args46827$_37foldr140123$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41281, %struct.ScmObj* %args46827$_37foldr140123$1)
store volatile %struct.ScmObj* %args46827$_37foldr140123$2, %struct.ScmObj** %stackaddr$prim48269, align 8
%stackaddr$prim48270 = alloca %struct.ScmObj*, align 8
%args46827$_37foldr140123$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40264, %struct.ScmObj* %args46827$_37foldr140123$2)
store volatile %struct.ScmObj* %args46827$_37foldr140123$3, %struct.ScmObj** %stackaddr$prim48270, align 8
%stackaddr$prim48271 = alloca %struct.ScmObj*, align 8
%args46827$_37foldr140123$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41279, %struct.ScmObj* %args46827$_37foldr140123$3)
store volatile %struct.ScmObj* %args46827$_37foldr140123$4, %struct.ScmObj** %stackaddr$prim48271, align 8
%clofunc48272 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140123)
musttail call tailcc void %clofunc48272(%struct.ScmObj* %_37foldr140123, %struct.ScmObj* %args46827$_37foldr140123$4)
ret void
}

define tailcc void @proc_clo$ae41279(%struct.ScmObj* %env$ae41279,%struct.ScmObj* %current_45args46783) {
%stackaddr$env-ref48273 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41279, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref48273
%stackaddr$env-ref48274 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41279, i64 1)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref48274
%stackaddr$env-ref48275 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41279, i64 2)
store %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$env-ref48275
%stackaddr$env-ref48276 = alloca %struct.ScmObj*, align 8
%lsts40131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41279, i64 3)
store %struct.ScmObj* %lsts40131, %struct.ScmObj** %stackaddr$env-ref48276
%stackaddr$env-ref48277 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41279, i64 4)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref48277
%stackaddr$env-ref48278 = alloca %struct.ScmObj*, align 8
%k40476 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41279, i64 5)
store %struct.ScmObj* %k40476, %struct.ScmObj** %stackaddr$env-ref48278
%stackaddr$env-ref48279 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41279, i64 6)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48279
%stackaddr$prim48280 = alloca %struct.ScmObj*, align 8
%_95k40478 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46783)
store volatile %struct.ScmObj* %_95k40478, %struct.ScmObj** %stackaddr$prim48280, align 8
%stackaddr$prim48281 = alloca %struct.ScmObj*, align 8
%current_45args46784 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46783)
store volatile %struct.ScmObj* %current_45args46784, %struct.ScmObj** %stackaddr$prim48281, align 8
%stackaddr$prim48282 = alloca %struct.ScmObj*, align 8
%anf_45bind40265 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46784)
store volatile %struct.ScmObj* %anf_45bind40265, %struct.ScmObj** %stackaddr$prim48282, align 8
%truthy$cmp48283 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40265)
%cmp$cmp48283 = icmp eq i64 %truthy$cmp48283, 1
br i1 %cmp$cmp48283, label %truebranch$cmp48283, label %falsebranch$cmp48283
truebranch$cmp48283:
%ae41290 = call %struct.ScmObj* @const_init_int(i64 0)
%args46786$k40476$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48284 = alloca %struct.ScmObj*, align 8
%args46786$k40476$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40132, %struct.ScmObj* %args46786$k40476$0)
store volatile %struct.ScmObj* %args46786$k40476$1, %struct.ScmObj** %stackaddr$prim48284, align 8
%stackaddr$prim48285 = alloca %struct.ScmObj*, align 8
%args46786$k40476$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41290, %struct.ScmObj* %args46786$k40476$1)
store volatile %struct.ScmObj* %args46786$k40476$2, %struct.ScmObj** %stackaddr$prim48285, align 8
%clofunc48286 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40476)
musttail call tailcc void %clofunc48286(%struct.ScmObj* %k40476, %struct.ScmObj* %args46786$k40476$2)
ret void
falsebranch$cmp48283:
%stackaddr$makeclosure48287 = alloca %struct.ScmObj*, align 8
%fptrToInt48288 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41295 to i64
%ae41295 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt48288)
store volatile %struct.ScmObj* %ae41295, %struct.ScmObj** %stackaddr$makeclosure48287, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41295, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41295, %struct.ScmObj* %f40133, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41295, %struct.ScmObj* %acc40132, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41295, %struct.ScmObj* %lsts40131, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41295, %struct.ScmObj* %_37foldr40129, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41295, %struct.ScmObj* %k40476, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41295, %struct.ScmObj* %_37foldr140123, i64 6)
%ae41296 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48289 = alloca %struct.ScmObj*, align 8
%fptrToInt48290 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41297 to i64
%ae41297 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48290)
store volatile %struct.ScmObj* %ae41297, %struct.ScmObj** %stackaddr$makeclosure48289, align 8
%args46826$ae41295$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48291 = alloca %struct.ScmObj*, align 8
%args46826$ae41295$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41297, %struct.ScmObj* %args46826$ae41295$0)
store volatile %struct.ScmObj* %args46826$ae41295$1, %struct.ScmObj** %stackaddr$prim48291, align 8
%stackaddr$prim48292 = alloca %struct.ScmObj*, align 8
%args46826$ae41295$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41296, %struct.ScmObj* %args46826$ae41295$1)
store volatile %struct.ScmObj* %args46826$ae41295$2, %struct.ScmObj** %stackaddr$prim48292, align 8
%clofunc48293 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41295)
musttail call tailcc void %clofunc48293(%struct.ScmObj* %ae41295, %struct.ScmObj* %args46826$ae41295$2)
ret void
}

define tailcc void @proc_clo$ae41295(%struct.ScmObj* %env$ae41295,%struct.ScmObj* %current_45args46787) {
%stackaddr$env-ref48294 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41295, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref48294
%stackaddr$env-ref48295 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41295, i64 1)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref48295
%stackaddr$env-ref48296 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41295, i64 2)
store %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$env-ref48296
%stackaddr$env-ref48297 = alloca %struct.ScmObj*, align 8
%lsts40131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41295, i64 3)
store %struct.ScmObj* %lsts40131, %struct.ScmObj** %stackaddr$env-ref48297
%stackaddr$env-ref48298 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41295, i64 4)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref48298
%stackaddr$env-ref48299 = alloca %struct.ScmObj*, align 8
%k40476 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41295, i64 5)
store %struct.ScmObj* %k40476, %struct.ScmObj** %stackaddr$env-ref48299
%stackaddr$env-ref48300 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41295, i64 6)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48300
%stackaddr$prim48301 = alloca %struct.ScmObj*, align 8
%_95k40479 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46787)
store volatile %struct.ScmObj* %_95k40479, %struct.ScmObj** %stackaddr$prim48301, align 8
%stackaddr$prim48302 = alloca %struct.ScmObj*, align 8
%current_45args46788 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46787)
store volatile %struct.ScmObj* %current_45args46788, %struct.ScmObj** %stackaddr$prim48302, align 8
%stackaddr$prim48303 = alloca %struct.ScmObj*, align 8
%anf_45bind40266 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46788)
store volatile %struct.ScmObj* %anf_45bind40266, %struct.ScmObj** %stackaddr$prim48303, align 8
%stackaddr$makeclosure48304 = alloca %struct.ScmObj*, align 8
%fptrToInt48305 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41316 to i64
%ae41316 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt48305)
store volatile %struct.ScmObj* %ae41316, %struct.ScmObj** %stackaddr$makeclosure48304, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41316, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41316, %struct.ScmObj* %f40133, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41316, %struct.ScmObj* %acc40132, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41316, %struct.ScmObj* %lsts40131, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41316, %struct.ScmObj* %_37foldr40129, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41316, %struct.ScmObj* %k40476, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41316, %struct.ScmObj* %_37foldr140123, i64 6)
%args46821$_37map140119$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48306 = alloca %struct.ScmObj*, align 8
%args46821$_37map140119$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40131, %struct.ScmObj* %args46821$_37map140119$0)
store volatile %struct.ScmObj* %args46821$_37map140119$1, %struct.ScmObj** %stackaddr$prim48306, align 8
%stackaddr$prim48307 = alloca %struct.ScmObj*, align 8
%args46821$_37map140119$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40266, %struct.ScmObj* %args46821$_37map140119$1)
store volatile %struct.ScmObj* %args46821$_37map140119$2, %struct.ScmObj** %stackaddr$prim48307, align 8
%stackaddr$prim48308 = alloca %struct.ScmObj*, align 8
%args46821$_37map140119$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41316, %struct.ScmObj* %args46821$_37map140119$2)
store volatile %struct.ScmObj* %args46821$_37map140119$3, %struct.ScmObj** %stackaddr$prim48308, align 8
%clofunc48309 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140119)
musttail call tailcc void %clofunc48309(%struct.ScmObj* %_37map140119, %struct.ScmObj* %args46821$_37map140119$3)
ret void
}

define tailcc void @proc_clo$ae41316(%struct.ScmObj* %env$ae41316,%struct.ScmObj* %current_45args46790) {
%stackaddr$env-ref48310 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41316, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref48310
%stackaddr$env-ref48311 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41316, i64 1)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref48311
%stackaddr$env-ref48312 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41316, i64 2)
store %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$env-ref48312
%stackaddr$env-ref48313 = alloca %struct.ScmObj*, align 8
%lsts40131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41316, i64 3)
store %struct.ScmObj* %lsts40131, %struct.ScmObj** %stackaddr$env-ref48313
%stackaddr$env-ref48314 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41316, i64 4)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref48314
%stackaddr$env-ref48315 = alloca %struct.ScmObj*, align 8
%k40476 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41316, i64 5)
store %struct.ScmObj* %k40476, %struct.ScmObj** %stackaddr$env-ref48315
%stackaddr$env-ref48316 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41316, i64 6)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48316
%stackaddr$prim48317 = alloca %struct.ScmObj*, align 8
%_95k40480 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46790)
store volatile %struct.ScmObj* %_95k40480, %struct.ScmObj** %stackaddr$prim48317, align 8
%stackaddr$prim48318 = alloca %struct.ScmObj*, align 8
%current_45args46791 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46790)
store volatile %struct.ScmObj* %current_45args46791, %struct.ScmObj** %stackaddr$prim48318, align 8
%stackaddr$prim48319 = alloca %struct.ScmObj*, align 8
%lsts_4340138 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46791)
store volatile %struct.ScmObj* %lsts_4340138, %struct.ScmObj** %stackaddr$prim48319, align 8
%stackaddr$makeclosure48320 = alloca %struct.ScmObj*, align 8
%fptrToInt48321 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41319 to i64
%ae41319 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt48321)
store volatile %struct.ScmObj* %ae41319, %struct.ScmObj** %stackaddr$makeclosure48320, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41319, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41319, %struct.ScmObj* %f40133, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41319, %struct.ScmObj* %acc40132, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41319, %struct.ScmObj* %lsts40131, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41319, %struct.ScmObj* %_37foldr40129, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41319, %struct.ScmObj* %k40476, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41319, %struct.ScmObj* %_37foldr140123, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae41319, %struct.ScmObj* %lsts_4340138, i64 7)
%ae41320 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48322 = alloca %struct.ScmObj*, align 8
%fptrToInt48323 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41321 to i64
%ae41321 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48323)
store volatile %struct.ScmObj* %ae41321, %struct.ScmObj** %stackaddr$makeclosure48322, align 8
%args46820$ae41319$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48324 = alloca %struct.ScmObj*, align 8
%args46820$ae41319$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41321, %struct.ScmObj* %args46820$ae41319$0)
store volatile %struct.ScmObj* %args46820$ae41319$1, %struct.ScmObj** %stackaddr$prim48324, align 8
%stackaddr$prim48325 = alloca %struct.ScmObj*, align 8
%args46820$ae41319$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41320, %struct.ScmObj* %args46820$ae41319$1)
store volatile %struct.ScmObj* %args46820$ae41319$2, %struct.ScmObj** %stackaddr$prim48325, align 8
%clofunc48326 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41319)
musttail call tailcc void %clofunc48326(%struct.ScmObj* %ae41319, %struct.ScmObj* %args46820$ae41319$2)
ret void
}

define tailcc void @proc_clo$ae41319(%struct.ScmObj* %env$ae41319,%struct.ScmObj* %current_45args46793) {
%stackaddr$env-ref48327 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41319, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref48327
%stackaddr$env-ref48328 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41319, i64 1)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref48328
%stackaddr$env-ref48329 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41319, i64 2)
store %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$env-ref48329
%stackaddr$env-ref48330 = alloca %struct.ScmObj*, align 8
%lsts40131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41319, i64 3)
store %struct.ScmObj* %lsts40131, %struct.ScmObj** %stackaddr$env-ref48330
%stackaddr$env-ref48331 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41319, i64 4)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref48331
%stackaddr$env-ref48332 = alloca %struct.ScmObj*, align 8
%k40476 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41319, i64 5)
store %struct.ScmObj* %k40476, %struct.ScmObj** %stackaddr$env-ref48332
%stackaddr$env-ref48333 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41319, i64 6)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48333
%stackaddr$env-ref48334 = alloca %struct.ScmObj*, align 8
%lsts_4340138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41319, i64 7)
store %struct.ScmObj* %lsts_4340138, %struct.ScmObj** %stackaddr$env-ref48334
%stackaddr$prim48335 = alloca %struct.ScmObj*, align 8
%_95k40481 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46793)
store volatile %struct.ScmObj* %_95k40481, %struct.ScmObj** %stackaddr$prim48335, align 8
%stackaddr$prim48336 = alloca %struct.ScmObj*, align 8
%current_45args46794 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46793)
store volatile %struct.ScmObj* %current_45args46794, %struct.ScmObj** %stackaddr$prim48336, align 8
%stackaddr$prim48337 = alloca %struct.ScmObj*, align 8
%anf_45bind40267 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46794)
store volatile %struct.ScmObj* %anf_45bind40267, %struct.ScmObj** %stackaddr$prim48337, align 8
%stackaddr$makeclosure48338 = alloca %struct.ScmObj*, align 8
%fptrToInt48339 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41340 to i64
%ae41340 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt48339)
store volatile %struct.ScmObj* %ae41340, %struct.ScmObj** %stackaddr$makeclosure48338, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41340, %struct.ScmObj* %f40133, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41340, %struct.ScmObj* %acc40132, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41340, %struct.ScmObj* %_37foldr40129, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41340, %struct.ScmObj* %k40476, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41340, %struct.ScmObj* %_37foldr140123, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41340, %struct.ScmObj* %lsts_4340138, i64 5)
%args46815$_37map140119$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48340 = alloca %struct.ScmObj*, align 8
%args46815$_37map140119$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40131, %struct.ScmObj* %args46815$_37map140119$0)
store volatile %struct.ScmObj* %args46815$_37map140119$1, %struct.ScmObj** %stackaddr$prim48340, align 8
%stackaddr$prim48341 = alloca %struct.ScmObj*, align 8
%args46815$_37map140119$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40267, %struct.ScmObj* %args46815$_37map140119$1)
store volatile %struct.ScmObj* %args46815$_37map140119$2, %struct.ScmObj** %stackaddr$prim48341, align 8
%stackaddr$prim48342 = alloca %struct.ScmObj*, align 8
%args46815$_37map140119$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41340, %struct.ScmObj* %args46815$_37map140119$2)
store volatile %struct.ScmObj* %args46815$_37map140119$3, %struct.ScmObj** %stackaddr$prim48342, align 8
%clofunc48343 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140119)
musttail call tailcc void %clofunc48343(%struct.ScmObj* %_37map140119, %struct.ScmObj* %args46815$_37map140119$3)
ret void
}

define tailcc void @proc_clo$ae41340(%struct.ScmObj* %env$ae41340,%struct.ScmObj* %current_45args46796) {
%stackaddr$env-ref48344 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41340, i64 0)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref48344
%stackaddr$env-ref48345 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41340, i64 1)
store %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$env-ref48345
%stackaddr$env-ref48346 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41340, i64 2)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref48346
%stackaddr$env-ref48347 = alloca %struct.ScmObj*, align 8
%k40476 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41340, i64 3)
store %struct.ScmObj* %k40476, %struct.ScmObj** %stackaddr$env-ref48347
%stackaddr$env-ref48348 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41340, i64 4)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48348
%stackaddr$env-ref48349 = alloca %struct.ScmObj*, align 8
%lsts_4340138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41340, i64 5)
store %struct.ScmObj* %lsts_4340138, %struct.ScmObj** %stackaddr$env-ref48349
%stackaddr$prim48350 = alloca %struct.ScmObj*, align 8
%_95k40482 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46796)
store volatile %struct.ScmObj* %_95k40482, %struct.ScmObj** %stackaddr$prim48350, align 8
%stackaddr$prim48351 = alloca %struct.ScmObj*, align 8
%current_45args46797 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46796)
store volatile %struct.ScmObj* %current_45args46797, %struct.ScmObj** %stackaddr$prim48351, align 8
%stackaddr$prim48352 = alloca %struct.ScmObj*, align 8
%vs40136 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46797)
store volatile %struct.ScmObj* %vs40136, %struct.ScmObj** %stackaddr$prim48352, align 8
%stackaddr$makeclosure48353 = alloca %struct.ScmObj*, align 8
%fptrToInt48354 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41343 to i64
%ae41343 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt48354)
store volatile %struct.ScmObj* %ae41343, %struct.ScmObj** %stackaddr$makeclosure48353, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41343, %struct.ScmObj* %f40133, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41343, %struct.ScmObj* %acc40132, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41343, %struct.ScmObj* %_37foldr40129, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41343, %struct.ScmObj* %k40476, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41343, %struct.ScmObj* %_37foldr140123, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41343, %struct.ScmObj* %lsts_4340138, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41343, %struct.ScmObj* %vs40136, i64 6)
%ae41344 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48355 = alloca %struct.ScmObj*, align 8
%fptrToInt48356 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41345 to i64
%ae41345 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48356)
store volatile %struct.ScmObj* %ae41345, %struct.ScmObj** %stackaddr$makeclosure48355, align 8
%args46814$ae41343$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48357 = alloca %struct.ScmObj*, align 8
%args46814$ae41343$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41345, %struct.ScmObj* %args46814$ae41343$0)
store volatile %struct.ScmObj* %args46814$ae41343$1, %struct.ScmObj** %stackaddr$prim48357, align 8
%stackaddr$prim48358 = alloca %struct.ScmObj*, align 8
%args46814$ae41343$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41344, %struct.ScmObj* %args46814$ae41343$1)
store volatile %struct.ScmObj* %args46814$ae41343$2, %struct.ScmObj** %stackaddr$prim48358, align 8
%clofunc48359 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41343)
musttail call tailcc void %clofunc48359(%struct.ScmObj* %ae41343, %struct.ScmObj* %args46814$ae41343$2)
ret void
}

define tailcc void @proc_clo$ae41343(%struct.ScmObj* %env$ae41343,%struct.ScmObj* %current_45args46799) {
%stackaddr$env-ref48360 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41343, i64 0)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref48360
%stackaddr$env-ref48361 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41343, i64 1)
store %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$env-ref48361
%stackaddr$env-ref48362 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41343, i64 2)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref48362
%stackaddr$env-ref48363 = alloca %struct.ScmObj*, align 8
%k40476 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41343, i64 3)
store %struct.ScmObj* %k40476, %struct.ScmObj** %stackaddr$env-ref48363
%stackaddr$env-ref48364 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41343, i64 4)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48364
%stackaddr$env-ref48365 = alloca %struct.ScmObj*, align 8
%lsts_4340138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41343, i64 5)
store %struct.ScmObj* %lsts_4340138, %struct.ScmObj** %stackaddr$env-ref48365
%stackaddr$env-ref48366 = alloca %struct.ScmObj*, align 8
%vs40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41343, i64 6)
store %struct.ScmObj* %vs40136, %struct.ScmObj** %stackaddr$env-ref48366
%stackaddr$prim48367 = alloca %struct.ScmObj*, align 8
%_95k40483 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46799)
store volatile %struct.ScmObj* %_95k40483, %struct.ScmObj** %stackaddr$prim48367, align 8
%stackaddr$prim48368 = alloca %struct.ScmObj*, align 8
%current_45args46800 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46799)
store volatile %struct.ScmObj* %current_45args46800, %struct.ScmObj** %stackaddr$prim48368, align 8
%stackaddr$prim48369 = alloca %struct.ScmObj*, align 8
%anf_45bind40268 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46800)
store volatile %struct.ScmObj* %anf_45bind40268, %struct.ScmObj** %stackaddr$prim48369, align 8
%stackaddr$prim48370 = alloca %struct.ScmObj*, align 8
%anf_45bind40269 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40132, %struct.ScmObj* %lsts_4340138)
store volatile %struct.ScmObj* %anf_45bind40269, %struct.ScmObj** %stackaddr$prim48370, align 8
%stackaddr$prim48371 = alloca %struct.ScmObj*, align 8
%anf_45bind40270 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40133, %struct.ScmObj* %anf_45bind40269)
store volatile %struct.ScmObj* %anf_45bind40270, %struct.ScmObj** %stackaddr$prim48371, align 8
%stackaddr$makeclosure48372 = alloca %struct.ScmObj*, align 8
%fptrToInt48373 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41369 to i64
%ae41369 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt48373)
store volatile %struct.ScmObj* %ae41369, %struct.ScmObj** %stackaddr$makeclosure48372, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41369, %struct.ScmObj* %anf_45bind40268, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41369, %struct.ScmObj* %k40476, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41369, %struct.ScmObj* %f40133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41369, %struct.ScmObj* %_37foldr140123, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41369, %struct.ScmObj* %vs40136, i64 4)
%stackaddr$prim48374 = alloca %struct.ScmObj*, align 8
%cpsargs40487 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41369, %struct.ScmObj* %anf_45bind40270)
store volatile %struct.ScmObj* %cpsargs40487, %struct.ScmObj** %stackaddr$prim48374, align 8
%clofunc48375 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr40129)
musttail call tailcc void %clofunc48375(%struct.ScmObj* %_37foldr40129, %struct.ScmObj* %cpsargs40487)
ret void
}

define tailcc void @proc_clo$ae41369(%struct.ScmObj* %env$ae41369,%struct.ScmObj* %current_45args46802) {
%stackaddr$env-ref48376 = alloca %struct.ScmObj*, align 8
%anf_45bind40268 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41369, i64 0)
store %struct.ScmObj* %anf_45bind40268, %struct.ScmObj** %stackaddr$env-ref48376
%stackaddr$env-ref48377 = alloca %struct.ScmObj*, align 8
%k40476 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41369, i64 1)
store %struct.ScmObj* %k40476, %struct.ScmObj** %stackaddr$env-ref48377
%stackaddr$env-ref48378 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41369, i64 2)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref48378
%stackaddr$env-ref48379 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41369, i64 3)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48379
%stackaddr$env-ref48380 = alloca %struct.ScmObj*, align 8
%vs40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41369, i64 4)
store %struct.ScmObj* %vs40136, %struct.ScmObj** %stackaddr$env-ref48380
%stackaddr$prim48381 = alloca %struct.ScmObj*, align 8
%_95k40484 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46802)
store volatile %struct.ScmObj* %_95k40484, %struct.ScmObj** %stackaddr$prim48381, align 8
%stackaddr$prim48382 = alloca %struct.ScmObj*, align 8
%current_45args46803 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46802)
store volatile %struct.ScmObj* %current_45args46803, %struct.ScmObj** %stackaddr$prim48382, align 8
%stackaddr$prim48383 = alloca %struct.ScmObj*, align 8
%anf_45bind40271 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46803)
store volatile %struct.ScmObj* %anf_45bind40271, %struct.ScmObj** %stackaddr$prim48383, align 8
%ae41374 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48384 = alloca %struct.ScmObj*, align 8
%anf_45bind40272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40271, %struct.ScmObj* %ae41374)
store volatile %struct.ScmObj* %anf_45bind40272, %struct.ScmObj** %stackaddr$prim48384, align 8
%stackaddr$makeclosure48385 = alloca %struct.ScmObj*, align 8
%fptrToInt48386 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41376 to i64
%ae41376 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48386)
store volatile %struct.ScmObj* %ae41376, %struct.ScmObj** %stackaddr$makeclosure48385, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41376, %struct.ScmObj* %f40133, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41376, %struct.ScmObj* %k40476, i64 1)
%args46808$_37foldr140123$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48387 = alloca %struct.ScmObj*, align 8
%args46808$_37foldr140123$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs40136, %struct.ScmObj* %args46808$_37foldr140123$0)
store volatile %struct.ScmObj* %args46808$_37foldr140123$1, %struct.ScmObj** %stackaddr$prim48387, align 8
%stackaddr$prim48388 = alloca %struct.ScmObj*, align 8
%args46808$_37foldr140123$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40272, %struct.ScmObj* %args46808$_37foldr140123$1)
store volatile %struct.ScmObj* %args46808$_37foldr140123$2, %struct.ScmObj** %stackaddr$prim48388, align 8
%stackaddr$prim48389 = alloca %struct.ScmObj*, align 8
%args46808$_37foldr140123$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40268, %struct.ScmObj* %args46808$_37foldr140123$2)
store volatile %struct.ScmObj* %args46808$_37foldr140123$3, %struct.ScmObj** %stackaddr$prim48389, align 8
%stackaddr$prim48390 = alloca %struct.ScmObj*, align 8
%args46808$_37foldr140123$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41376, %struct.ScmObj* %args46808$_37foldr140123$3)
store volatile %struct.ScmObj* %args46808$_37foldr140123$4, %struct.ScmObj** %stackaddr$prim48390, align 8
%clofunc48391 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140123)
musttail call tailcc void %clofunc48391(%struct.ScmObj* %_37foldr140123, %struct.ScmObj* %args46808$_37foldr140123$4)
ret void
}

define tailcc void @proc_clo$ae41376(%struct.ScmObj* %env$ae41376,%struct.ScmObj* %current_45args46805) {
%stackaddr$env-ref48392 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41376, i64 0)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref48392
%stackaddr$env-ref48393 = alloca %struct.ScmObj*, align 8
%k40476 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41376, i64 1)
store %struct.ScmObj* %k40476, %struct.ScmObj** %stackaddr$env-ref48393
%stackaddr$prim48394 = alloca %struct.ScmObj*, align 8
%_95k40485 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46805)
store volatile %struct.ScmObj* %_95k40485, %struct.ScmObj** %stackaddr$prim48394, align 8
%stackaddr$prim48395 = alloca %struct.ScmObj*, align 8
%current_45args46806 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46805)
store volatile %struct.ScmObj* %current_45args46806, %struct.ScmObj** %stackaddr$prim48395, align 8
%stackaddr$prim48396 = alloca %struct.ScmObj*, align 8
%anf_45bind40273 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46806)
store volatile %struct.ScmObj* %anf_45bind40273, %struct.ScmObj** %stackaddr$prim48396, align 8
%stackaddr$prim48397 = alloca %struct.ScmObj*, align 8
%cpsargs40486 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40476, %struct.ScmObj* %anf_45bind40273)
store volatile %struct.ScmObj* %cpsargs40486, %struct.ScmObj** %stackaddr$prim48397, align 8
%clofunc48398 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40133)
musttail call tailcc void %clofunc48398(%struct.ScmObj* %f40133, %struct.ScmObj* %cpsargs40486)
ret void
}

define tailcc void @proc_clo$ae41345(%struct.ScmObj* %env$ae41345,%struct.ScmObj* %current_45args46809) {
%stackaddr$prim48399 = alloca %struct.ScmObj*, align 8
%k40488 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46809)
store volatile %struct.ScmObj* %k40488, %struct.ScmObj** %stackaddr$prim48399, align 8
%stackaddr$prim48400 = alloca %struct.ScmObj*, align 8
%current_45args46810 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46809)
store volatile %struct.ScmObj* %current_45args46810, %struct.ScmObj** %stackaddr$prim48400, align 8
%stackaddr$prim48401 = alloca %struct.ScmObj*, align 8
%a40141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46810)
store volatile %struct.ScmObj* %a40141, %struct.ScmObj** %stackaddr$prim48401, align 8
%stackaddr$prim48402 = alloca %struct.ScmObj*, align 8
%current_45args46811 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46810)
store volatile %struct.ScmObj* %current_45args46811, %struct.ScmObj** %stackaddr$prim48402, align 8
%stackaddr$prim48403 = alloca %struct.ScmObj*, align 8
%b40140 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46811)
store volatile %struct.ScmObj* %b40140, %struct.ScmObj** %stackaddr$prim48403, align 8
%stackaddr$prim48404 = alloca %struct.ScmObj*, align 8
%cpsprim40489 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a40141, %struct.ScmObj* %b40140)
store volatile %struct.ScmObj* %cpsprim40489, %struct.ScmObj** %stackaddr$prim48404, align 8
%ae41349 = call %struct.ScmObj* @const_init_int(i64 0)
%args46813$k40488$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48405 = alloca %struct.ScmObj*, align 8
%args46813$k40488$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40489, %struct.ScmObj* %args46813$k40488$0)
store volatile %struct.ScmObj* %args46813$k40488$1, %struct.ScmObj** %stackaddr$prim48405, align 8
%stackaddr$prim48406 = alloca %struct.ScmObj*, align 8
%args46813$k40488$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41349, %struct.ScmObj* %args46813$k40488$1)
store volatile %struct.ScmObj* %args46813$k40488$2, %struct.ScmObj** %stackaddr$prim48406, align 8
%clofunc48407 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40488)
musttail call tailcc void %clofunc48407(%struct.ScmObj* %k40488, %struct.ScmObj* %args46813$k40488$2)
ret void
}

define tailcc void @proc_clo$ae41321(%struct.ScmObj* %env$ae41321,%struct.ScmObj* %current_45args46816) {
%stackaddr$prim48408 = alloca %struct.ScmObj*, align 8
%k40490 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46816)
store volatile %struct.ScmObj* %k40490, %struct.ScmObj** %stackaddr$prim48408, align 8
%stackaddr$prim48409 = alloca %struct.ScmObj*, align 8
%current_45args46817 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46816)
store volatile %struct.ScmObj* %current_45args46817, %struct.ScmObj** %stackaddr$prim48409, align 8
%stackaddr$prim48410 = alloca %struct.ScmObj*, align 8
%x40137 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46817)
store volatile %struct.ScmObj* %x40137, %struct.ScmObj** %stackaddr$prim48410, align 8
%stackaddr$prim48411 = alloca %struct.ScmObj*, align 8
%cpsprim40491 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40137)
store volatile %struct.ScmObj* %cpsprim40491, %struct.ScmObj** %stackaddr$prim48411, align 8
%ae41324 = call %struct.ScmObj* @const_init_int(i64 0)
%args46819$k40490$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48412 = alloca %struct.ScmObj*, align 8
%args46819$k40490$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40491, %struct.ScmObj* %args46819$k40490$0)
store volatile %struct.ScmObj* %args46819$k40490$1, %struct.ScmObj** %stackaddr$prim48412, align 8
%stackaddr$prim48413 = alloca %struct.ScmObj*, align 8
%args46819$k40490$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41324, %struct.ScmObj* %args46819$k40490$1)
store volatile %struct.ScmObj* %args46819$k40490$2, %struct.ScmObj** %stackaddr$prim48413, align 8
%clofunc48414 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40490)
musttail call tailcc void %clofunc48414(%struct.ScmObj* %k40490, %struct.ScmObj* %args46819$k40490$2)
ret void
}

define tailcc void @proc_clo$ae41297(%struct.ScmObj* %env$ae41297,%struct.ScmObj* %current_45args46822) {
%stackaddr$prim48415 = alloca %struct.ScmObj*, align 8
%k40492 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46822)
store volatile %struct.ScmObj* %k40492, %struct.ScmObj** %stackaddr$prim48415, align 8
%stackaddr$prim48416 = alloca %struct.ScmObj*, align 8
%current_45args46823 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46822)
store volatile %struct.ScmObj* %current_45args46823, %struct.ScmObj** %stackaddr$prim48416, align 8
%stackaddr$prim48417 = alloca %struct.ScmObj*, align 8
%x40139 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46823)
store volatile %struct.ScmObj* %x40139, %struct.ScmObj** %stackaddr$prim48417, align 8
%stackaddr$prim48418 = alloca %struct.ScmObj*, align 8
%cpsprim40493 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40139)
store volatile %struct.ScmObj* %cpsprim40493, %struct.ScmObj** %stackaddr$prim48418, align 8
%ae41300 = call %struct.ScmObj* @const_init_int(i64 0)
%args46825$k40492$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48419 = alloca %struct.ScmObj*, align 8
%args46825$k40492$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40493, %struct.ScmObj* %args46825$k40492$0)
store volatile %struct.ScmObj* %args46825$k40492$1, %struct.ScmObj** %stackaddr$prim48419, align 8
%stackaddr$prim48420 = alloca %struct.ScmObj*, align 8
%args46825$k40492$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41300, %struct.ScmObj* %args46825$k40492$1)
store volatile %struct.ScmObj* %args46825$k40492$2, %struct.ScmObj** %stackaddr$prim48420, align 8
%clofunc48421 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40492)
musttail call tailcc void %clofunc48421(%struct.ScmObj* %k40492, %struct.ScmObj* %args46825$k40492$2)
ret void
}

define tailcc void @proc_clo$ae41249(%struct.ScmObj* %env$ae41249,%struct.ScmObj* %current_45args46828) {
%stackaddr$prim48422 = alloca %struct.ScmObj*, align 8
%k40494 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46828)
store volatile %struct.ScmObj* %k40494, %struct.ScmObj** %stackaddr$prim48422, align 8
%stackaddr$prim48423 = alloca %struct.ScmObj*, align 8
%current_45args46829 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46828)
store volatile %struct.ScmObj* %current_45args46829, %struct.ScmObj** %stackaddr$prim48423, align 8
%stackaddr$prim48424 = alloca %struct.ScmObj*, align 8
%lst40135 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46829)
store volatile %struct.ScmObj* %lst40135, %struct.ScmObj** %stackaddr$prim48424, align 8
%stackaddr$prim48425 = alloca %struct.ScmObj*, align 8
%current_45args46830 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46829)
store volatile %struct.ScmObj* %current_45args46830, %struct.ScmObj** %stackaddr$prim48425, align 8
%stackaddr$prim48426 = alloca %struct.ScmObj*, align 8
%b40134 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46830)
store volatile %struct.ScmObj* %b40134, %struct.ScmObj** %stackaddr$prim48426, align 8
%truthy$cmp48427 = call i64 @is_truthy_value(%struct.ScmObj* %b40134)
%cmp$cmp48427 = icmp eq i64 %truthy$cmp48427, 1
br i1 %cmp$cmp48427, label %truebranch$cmp48427, label %falsebranch$cmp48427
truebranch$cmp48427:
%ae41252 = call %struct.ScmObj* @const_init_int(i64 0)
%args46832$k40494$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48428 = alloca %struct.ScmObj*, align 8
%args46832$k40494$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b40134, %struct.ScmObj* %args46832$k40494$0)
store volatile %struct.ScmObj* %args46832$k40494$1, %struct.ScmObj** %stackaddr$prim48428, align 8
%stackaddr$prim48429 = alloca %struct.ScmObj*, align 8
%args46832$k40494$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41252, %struct.ScmObj* %args46832$k40494$1)
store volatile %struct.ScmObj* %args46832$k40494$2, %struct.ScmObj** %stackaddr$prim48429, align 8
%clofunc48430 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40494)
musttail call tailcc void %clofunc48430(%struct.ScmObj* %k40494, %struct.ScmObj* %args46832$k40494$2)
ret void
falsebranch$cmp48427:
%stackaddr$prim48431 = alloca %struct.ScmObj*, align 8
%cpsprim40495 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40135)
store volatile %struct.ScmObj* %cpsprim40495, %struct.ScmObj** %stackaddr$prim48431, align 8
%ae41259 = call %struct.ScmObj* @const_init_int(i64 0)
%args46833$k40494$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48432 = alloca %struct.ScmObj*, align 8
%args46833$k40494$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40495, %struct.ScmObj* %args46833$k40494$0)
store volatile %struct.ScmObj* %args46833$k40494$1, %struct.ScmObj** %stackaddr$prim48432, align 8
%stackaddr$prim48433 = alloca %struct.ScmObj*, align 8
%args46833$k40494$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41259, %struct.ScmObj* %args46833$k40494$1)
store volatile %struct.ScmObj* %args46833$k40494$2, %struct.ScmObj** %stackaddr$prim48433, align 8
%clofunc48434 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40494)
musttail call tailcc void %clofunc48434(%struct.ScmObj* %k40494, %struct.ScmObj* %args46833$k40494$2)
ret void
}

define tailcc void @proc_clo$ae41206(%struct.ScmObj* %env$ae41206,%struct.ScmObj* %current_45args46837) {
%stackaddr$env-ref48435 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41206, i64 0)
store %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$env-ref48435
%stackaddr$env-ref48436 = alloca %struct.ScmObj*, align 8
%_37length40112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41206, i64 1)
store %struct.ScmObj* %_37length40112, %struct.ScmObj** %stackaddr$env-ref48436
%stackaddr$prim48437 = alloca %struct.ScmObj*, align 8
%k40496 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46837)
store volatile %struct.ScmObj* %k40496, %struct.ScmObj** %stackaddr$prim48437, align 8
%stackaddr$prim48438 = alloca %struct.ScmObj*, align 8
%current_45args46838 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46837)
store volatile %struct.ScmObj* %current_45args46838, %struct.ScmObj** %stackaddr$prim48438, align 8
%stackaddr$prim48439 = alloca %struct.ScmObj*, align 8
%lst40144 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46838)
store volatile %struct.ScmObj* %lst40144, %struct.ScmObj** %stackaddr$prim48439, align 8
%stackaddr$prim48440 = alloca %struct.ScmObj*, align 8
%current_45args46839 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46838)
store volatile %struct.ScmObj* %current_45args46839, %struct.ScmObj** %stackaddr$prim48440, align 8
%stackaddr$prim48441 = alloca %struct.ScmObj*, align 8
%n40143 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46839)
store volatile %struct.ScmObj* %n40143, %struct.ScmObj** %stackaddr$prim48441, align 8
%stackaddr$makeclosure48442 = alloca %struct.ScmObj*, align 8
%fptrToInt48443 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41208 to i64
%ae41208 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48443)
store volatile %struct.ScmObj* %ae41208, %struct.ScmObj** %stackaddr$makeclosure48442, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41208, %struct.ScmObj* %k40496, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41208, %struct.ScmObj* %lst40144, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41208, %struct.ScmObj* %_37take40115, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41208, %struct.ScmObj* %n40143, i64 3)
%args46845$_37length40112$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48444 = alloca %struct.ScmObj*, align 8
%args46845$_37length40112$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40144, %struct.ScmObj* %args46845$_37length40112$0)
store volatile %struct.ScmObj* %args46845$_37length40112$1, %struct.ScmObj** %stackaddr$prim48444, align 8
%stackaddr$prim48445 = alloca %struct.ScmObj*, align 8
%args46845$_37length40112$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41208, %struct.ScmObj* %args46845$_37length40112$1)
store volatile %struct.ScmObj* %args46845$_37length40112$2, %struct.ScmObj** %stackaddr$prim48445, align 8
%clofunc48446 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length40112)
musttail call tailcc void %clofunc48446(%struct.ScmObj* %_37length40112, %struct.ScmObj* %args46845$_37length40112$2)
ret void
}

define tailcc void @proc_clo$ae41208(%struct.ScmObj* %env$ae41208,%struct.ScmObj* %current_45args46841) {
%stackaddr$env-ref48447 = alloca %struct.ScmObj*, align 8
%k40496 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41208, i64 0)
store %struct.ScmObj* %k40496, %struct.ScmObj** %stackaddr$env-ref48447
%stackaddr$env-ref48448 = alloca %struct.ScmObj*, align 8
%lst40144 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41208, i64 1)
store %struct.ScmObj* %lst40144, %struct.ScmObj** %stackaddr$env-ref48448
%stackaddr$env-ref48449 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41208, i64 2)
store %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$env-ref48449
%stackaddr$env-ref48450 = alloca %struct.ScmObj*, align 8
%n40143 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41208, i64 3)
store %struct.ScmObj* %n40143, %struct.ScmObj** %stackaddr$env-ref48450
%stackaddr$prim48451 = alloca %struct.ScmObj*, align 8
%_95k40497 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46841)
store volatile %struct.ScmObj* %_95k40497, %struct.ScmObj** %stackaddr$prim48451, align 8
%stackaddr$prim48452 = alloca %struct.ScmObj*, align 8
%current_45args46842 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46841)
store volatile %struct.ScmObj* %current_45args46842, %struct.ScmObj** %stackaddr$prim48452, align 8
%stackaddr$prim48453 = alloca %struct.ScmObj*, align 8
%anf_45bind40260 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46842)
store volatile %struct.ScmObj* %anf_45bind40260, %struct.ScmObj** %stackaddr$prim48453, align 8
%stackaddr$prim48454 = alloca %struct.ScmObj*, align 8
%anf_45bind40261 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind40260, %struct.ScmObj* %n40143)
store volatile %struct.ScmObj* %anf_45bind40261, %struct.ScmObj** %stackaddr$prim48454, align 8
%args46844$_37take40115$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48455 = alloca %struct.ScmObj*, align 8
%args46844$_37take40115$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40261, %struct.ScmObj* %args46844$_37take40115$0)
store volatile %struct.ScmObj* %args46844$_37take40115$1, %struct.ScmObj** %stackaddr$prim48455, align 8
%stackaddr$prim48456 = alloca %struct.ScmObj*, align 8
%args46844$_37take40115$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40144, %struct.ScmObj* %args46844$_37take40115$1)
store volatile %struct.ScmObj* %args46844$_37take40115$2, %struct.ScmObj** %stackaddr$prim48456, align 8
%stackaddr$prim48457 = alloca %struct.ScmObj*, align 8
%args46844$_37take40115$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40496, %struct.ScmObj* %args46844$_37take40115$2)
store volatile %struct.ScmObj* %args46844$_37take40115$3, %struct.ScmObj** %stackaddr$prim48457, align 8
%clofunc48458 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take40115)
musttail call tailcc void %clofunc48458(%struct.ScmObj* %_37take40115, %struct.ScmObj* %args46844$_37take40115$3)
ret void
}

define tailcc void @proc_clo$ae41152(%struct.ScmObj* %env$ae41152,%struct.ScmObj* %current_45args46847) {
%stackaddr$env-ref48459 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41152, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref48459
%stackaddr$prim48460 = alloca %struct.ScmObj*, align 8
%k40498 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46847)
store volatile %struct.ScmObj* %k40498, %struct.ScmObj** %stackaddr$prim48460, align 8
%stackaddr$prim48461 = alloca %struct.ScmObj*, align 8
%current_45args46848 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46847)
store volatile %struct.ScmObj* %current_45args46848, %struct.ScmObj** %stackaddr$prim48461, align 8
%stackaddr$prim48462 = alloca %struct.ScmObj*, align 8
%lst40146 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46848)
store volatile %struct.ScmObj* %lst40146, %struct.ScmObj** %stackaddr$prim48462, align 8
%stackaddr$makeclosure48463 = alloca %struct.ScmObj*, align 8
%fptrToInt48464 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41153 to i64
%ae41153 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48464)
store volatile %struct.ScmObj* %ae41153, %struct.ScmObj** %stackaddr$makeclosure48463, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41153, %struct.ScmObj* %k40498, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41153, %struct.ScmObj* %lst40146, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41153, %struct.ScmObj* %_37foldl140107, i64 2)
%ae41154 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48465 = alloca %struct.ScmObj*, align 8
%fptrToInt48466 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41155 to i64
%ae41155 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48466)
store volatile %struct.ScmObj* %ae41155, %struct.ScmObj** %stackaddr$makeclosure48465, align 8
%args46859$ae41153$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48467 = alloca %struct.ScmObj*, align 8
%args46859$ae41153$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41155, %struct.ScmObj* %args46859$ae41153$0)
store volatile %struct.ScmObj* %args46859$ae41153$1, %struct.ScmObj** %stackaddr$prim48467, align 8
%stackaddr$prim48468 = alloca %struct.ScmObj*, align 8
%args46859$ae41153$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41154, %struct.ScmObj* %args46859$ae41153$1)
store volatile %struct.ScmObj* %args46859$ae41153$2, %struct.ScmObj** %stackaddr$prim48468, align 8
%clofunc48469 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41153)
musttail call tailcc void %clofunc48469(%struct.ScmObj* %ae41153, %struct.ScmObj* %args46859$ae41153$2)
ret void
}

define tailcc void @proc_clo$ae41153(%struct.ScmObj* %env$ae41153,%struct.ScmObj* %current_45args46850) {
%stackaddr$env-ref48470 = alloca %struct.ScmObj*, align 8
%k40498 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41153, i64 0)
store %struct.ScmObj* %k40498, %struct.ScmObj** %stackaddr$env-ref48470
%stackaddr$env-ref48471 = alloca %struct.ScmObj*, align 8
%lst40146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41153, i64 1)
store %struct.ScmObj* %lst40146, %struct.ScmObj** %stackaddr$env-ref48471
%stackaddr$env-ref48472 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41153, i64 2)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref48472
%stackaddr$prim48473 = alloca %struct.ScmObj*, align 8
%_95k40499 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46850)
store volatile %struct.ScmObj* %_95k40499, %struct.ScmObj** %stackaddr$prim48473, align 8
%stackaddr$prim48474 = alloca %struct.ScmObj*, align 8
%current_45args46851 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46850)
store volatile %struct.ScmObj* %current_45args46851, %struct.ScmObj** %stackaddr$prim48474, align 8
%stackaddr$prim48475 = alloca %struct.ScmObj*, align 8
%anf_45bind40259 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46851)
store volatile %struct.ScmObj* %anf_45bind40259, %struct.ScmObj** %stackaddr$prim48475, align 8
%ae41174 = call %struct.ScmObj* @const_init_null()
%args46853$_37foldl140107$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48476 = alloca %struct.ScmObj*, align 8
%args46853$_37foldl140107$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40146, %struct.ScmObj* %args46853$_37foldl140107$0)
store volatile %struct.ScmObj* %args46853$_37foldl140107$1, %struct.ScmObj** %stackaddr$prim48476, align 8
%stackaddr$prim48477 = alloca %struct.ScmObj*, align 8
%args46853$_37foldl140107$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41174, %struct.ScmObj* %args46853$_37foldl140107$1)
store volatile %struct.ScmObj* %args46853$_37foldl140107$2, %struct.ScmObj** %stackaddr$prim48477, align 8
%stackaddr$prim48478 = alloca %struct.ScmObj*, align 8
%args46853$_37foldl140107$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40259, %struct.ScmObj* %args46853$_37foldl140107$2)
store volatile %struct.ScmObj* %args46853$_37foldl140107$3, %struct.ScmObj** %stackaddr$prim48478, align 8
%stackaddr$prim48479 = alloca %struct.ScmObj*, align 8
%args46853$_37foldl140107$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40498, %struct.ScmObj* %args46853$_37foldl140107$3)
store volatile %struct.ScmObj* %args46853$_37foldl140107$4, %struct.ScmObj** %stackaddr$prim48479, align 8
%clofunc48480 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140107)
musttail call tailcc void %clofunc48480(%struct.ScmObj* %_37foldl140107, %struct.ScmObj* %args46853$_37foldl140107$4)
ret void
}

define tailcc void @proc_clo$ae41155(%struct.ScmObj* %env$ae41155,%struct.ScmObj* %current_45args46854) {
%stackaddr$prim48481 = alloca %struct.ScmObj*, align 8
%k40500 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46854)
store volatile %struct.ScmObj* %k40500, %struct.ScmObj** %stackaddr$prim48481, align 8
%stackaddr$prim48482 = alloca %struct.ScmObj*, align 8
%current_45args46855 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46854)
store volatile %struct.ScmObj* %current_45args46855, %struct.ScmObj** %stackaddr$prim48482, align 8
%stackaddr$prim48483 = alloca %struct.ScmObj*, align 8
%x40148 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46855)
store volatile %struct.ScmObj* %x40148, %struct.ScmObj** %stackaddr$prim48483, align 8
%stackaddr$prim48484 = alloca %struct.ScmObj*, align 8
%current_45args46856 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46855)
store volatile %struct.ScmObj* %current_45args46856, %struct.ScmObj** %stackaddr$prim48484, align 8
%stackaddr$prim48485 = alloca %struct.ScmObj*, align 8
%y40147 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46856)
store volatile %struct.ScmObj* %y40147, %struct.ScmObj** %stackaddr$prim48485, align 8
%ae41157 = call %struct.ScmObj* @const_init_int(i64 0)
%args46858$k40500$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48486 = alloca %struct.ScmObj*, align 8
%args46858$k40500$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x40148, %struct.ScmObj* %args46858$k40500$0)
store volatile %struct.ScmObj* %args46858$k40500$1, %struct.ScmObj** %stackaddr$prim48486, align 8
%stackaddr$prim48487 = alloca %struct.ScmObj*, align 8
%args46858$k40500$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41157, %struct.ScmObj* %args46858$k40500$1)
store volatile %struct.ScmObj* %args46858$k40500$2, %struct.ScmObj** %stackaddr$prim48487, align 8
%clofunc48488 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40500)
musttail call tailcc void %clofunc48488(%struct.ScmObj* %k40500, %struct.ScmObj* %args46858$k40500$2)
ret void
}

define tailcc void @proc_clo$ae41073(%struct.ScmObj* %env$ae41073,%struct.ScmObj* %current_45args46862) {
%stackaddr$prim48489 = alloca %struct.ScmObj*, align 8
%k40501 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46862)
store volatile %struct.ScmObj* %k40501, %struct.ScmObj** %stackaddr$prim48489, align 8
%stackaddr$prim48490 = alloca %struct.ScmObj*, align 8
%current_45args46863 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46862)
store volatile %struct.ScmObj* %current_45args46863, %struct.ScmObj** %stackaddr$prim48490, align 8
%stackaddr$prim48491 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46863)
store volatile %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$prim48491, align 8
%ae41075 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48492 = alloca %struct.ScmObj*, align 8
%fptrToInt48493 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41076 to i64
%ae41076 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48493)
store volatile %struct.ScmObj* %ae41076, %struct.ScmObj** %stackaddr$makeclosure48492, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41076, %struct.ScmObj* %_37foldl140108, i64 0)
%args46876$k40501$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48494 = alloca %struct.ScmObj*, align 8
%args46876$k40501$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41076, %struct.ScmObj* %args46876$k40501$0)
store volatile %struct.ScmObj* %args46876$k40501$1, %struct.ScmObj** %stackaddr$prim48494, align 8
%stackaddr$prim48495 = alloca %struct.ScmObj*, align 8
%args46876$k40501$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41075, %struct.ScmObj* %args46876$k40501$1)
store volatile %struct.ScmObj* %args46876$k40501$2, %struct.ScmObj** %stackaddr$prim48495, align 8
%clofunc48496 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40501)
musttail call tailcc void %clofunc48496(%struct.ScmObj* %k40501, %struct.ScmObj* %args46876$k40501$2)
ret void
}

define tailcc void @proc_clo$ae41076(%struct.ScmObj* %env$ae41076,%struct.ScmObj* %current_45args46865) {
%stackaddr$env-ref48497 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41076, i64 0)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref48497
%stackaddr$prim48498 = alloca %struct.ScmObj*, align 8
%k40502 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46865)
store volatile %struct.ScmObj* %k40502, %struct.ScmObj** %stackaddr$prim48498, align 8
%stackaddr$prim48499 = alloca %struct.ScmObj*, align 8
%current_45args46866 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46865)
store volatile %struct.ScmObj* %current_45args46866, %struct.ScmObj** %stackaddr$prim48499, align 8
%stackaddr$prim48500 = alloca %struct.ScmObj*, align 8
%f40111 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46866)
store volatile %struct.ScmObj* %f40111, %struct.ScmObj** %stackaddr$prim48500, align 8
%stackaddr$prim48501 = alloca %struct.ScmObj*, align 8
%current_45args46867 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46866)
store volatile %struct.ScmObj* %current_45args46867, %struct.ScmObj** %stackaddr$prim48501, align 8
%stackaddr$prim48502 = alloca %struct.ScmObj*, align 8
%acc40110 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46867)
store volatile %struct.ScmObj* %acc40110, %struct.ScmObj** %stackaddr$prim48502, align 8
%stackaddr$prim48503 = alloca %struct.ScmObj*, align 8
%current_45args46868 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46867)
store volatile %struct.ScmObj* %current_45args46868, %struct.ScmObj** %stackaddr$prim48503, align 8
%stackaddr$prim48504 = alloca %struct.ScmObj*, align 8
%lst40109 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46868)
store volatile %struct.ScmObj* %lst40109, %struct.ScmObj** %stackaddr$prim48504, align 8
%stackaddr$prim48505 = alloca %struct.ScmObj*, align 8
%anf_45bind40254 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40109)
store volatile %struct.ScmObj* %anf_45bind40254, %struct.ScmObj** %stackaddr$prim48505, align 8
%truthy$cmp48506 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40254)
%cmp$cmp48506 = icmp eq i64 %truthy$cmp48506, 1
br i1 %cmp$cmp48506, label %truebranch$cmp48506, label %falsebranch$cmp48506
truebranch$cmp48506:
%ae41080 = call %struct.ScmObj* @const_init_int(i64 0)
%args46870$k40502$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48507 = alloca %struct.ScmObj*, align 8
%args46870$k40502$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40110, %struct.ScmObj* %args46870$k40502$0)
store volatile %struct.ScmObj* %args46870$k40502$1, %struct.ScmObj** %stackaddr$prim48507, align 8
%stackaddr$prim48508 = alloca %struct.ScmObj*, align 8
%args46870$k40502$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41080, %struct.ScmObj* %args46870$k40502$1)
store volatile %struct.ScmObj* %args46870$k40502$2, %struct.ScmObj** %stackaddr$prim48508, align 8
%clofunc48509 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40502)
musttail call tailcc void %clofunc48509(%struct.ScmObj* %k40502, %struct.ScmObj* %args46870$k40502$2)
ret void
falsebranch$cmp48506:
%stackaddr$prim48510 = alloca %struct.ScmObj*, align 8
%anf_45bind40255 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40109)
store volatile %struct.ScmObj* %anf_45bind40255, %struct.ScmObj** %stackaddr$prim48510, align 8
%stackaddr$makeclosure48511 = alloca %struct.ScmObj*, align 8
%fptrToInt48512 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41087 to i64
%ae41087 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48512)
store volatile %struct.ScmObj* %ae41087, %struct.ScmObj** %stackaddr$makeclosure48511, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41087, %struct.ScmObj* %k40502, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41087, %struct.ScmObj* %f40111, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41087, %struct.ScmObj* %lst40109, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41087, %struct.ScmObj* %_37foldl140108, i64 3)
%args46875$f40111$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48513 = alloca %struct.ScmObj*, align 8
%args46875$f40111$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40110, %struct.ScmObj* %args46875$f40111$0)
store volatile %struct.ScmObj* %args46875$f40111$1, %struct.ScmObj** %stackaddr$prim48513, align 8
%stackaddr$prim48514 = alloca %struct.ScmObj*, align 8
%args46875$f40111$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40255, %struct.ScmObj* %args46875$f40111$1)
store volatile %struct.ScmObj* %args46875$f40111$2, %struct.ScmObj** %stackaddr$prim48514, align 8
%stackaddr$prim48515 = alloca %struct.ScmObj*, align 8
%args46875$f40111$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41087, %struct.ScmObj* %args46875$f40111$2)
store volatile %struct.ScmObj* %args46875$f40111$3, %struct.ScmObj** %stackaddr$prim48515, align 8
%clofunc48516 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40111)
musttail call tailcc void %clofunc48516(%struct.ScmObj* %f40111, %struct.ScmObj* %args46875$f40111$3)
ret void
}

define tailcc void @proc_clo$ae41087(%struct.ScmObj* %env$ae41087,%struct.ScmObj* %current_45args46871) {
%stackaddr$env-ref48517 = alloca %struct.ScmObj*, align 8
%k40502 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41087, i64 0)
store %struct.ScmObj* %k40502, %struct.ScmObj** %stackaddr$env-ref48517
%stackaddr$env-ref48518 = alloca %struct.ScmObj*, align 8
%f40111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41087, i64 1)
store %struct.ScmObj* %f40111, %struct.ScmObj** %stackaddr$env-ref48518
%stackaddr$env-ref48519 = alloca %struct.ScmObj*, align 8
%lst40109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41087, i64 2)
store %struct.ScmObj* %lst40109, %struct.ScmObj** %stackaddr$env-ref48519
%stackaddr$env-ref48520 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41087, i64 3)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref48520
%stackaddr$prim48521 = alloca %struct.ScmObj*, align 8
%_95k40503 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46871)
store volatile %struct.ScmObj* %_95k40503, %struct.ScmObj** %stackaddr$prim48521, align 8
%stackaddr$prim48522 = alloca %struct.ScmObj*, align 8
%current_45args46872 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46871)
store volatile %struct.ScmObj* %current_45args46872, %struct.ScmObj** %stackaddr$prim48522, align 8
%stackaddr$prim48523 = alloca %struct.ScmObj*, align 8
%anf_45bind40256 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46872)
store volatile %struct.ScmObj* %anf_45bind40256, %struct.ScmObj** %stackaddr$prim48523, align 8
%stackaddr$prim48524 = alloca %struct.ScmObj*, align 8
%anf_45bind40257 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40109)
store volatile %struct.ScmObj* %anf_45bind40257, %struct.ScmObj** %stackaddr$prim48524, align 8
%args46874$_37foldl140108$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48525 = alloca %struct.ScmObj*, align 8
%args46874$_37foldl140108$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40257, %struct.ScmObj* %args46874$_37foldl140108$0)
store volatile %struct.ScmObj* %args46874$_37foldl140108$1, %struct.ScmObj** %stackaddr$prim48525, align 8
%stackaddr$prim48526 = alloca %struct.ScmObj*, align 8
%args46874$_37foldl140108$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40256, %struct.ScmObj* %args46874$_37foldl140108$1)
store volatile %struct.ScmObj* %args46874$_37foldl140108$2, %struct.ScmObj** %stackaddr$prim48526, align 8
%stackaddr$prim48527 = alloca %struct.ScmObj*, align 8
%args46874$_37foldl140108$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40111, %struct.ScmObj* %args46874$_37foldl140108$2)
store volatile %struct.ScmObj* %args46874$_37foldl140108$3, %struct.ScmObj** %stackaddr$prim48527, align 8
%stackaddr$prim48528 = alloca %struct.ScmObj*, align 8
%args46874$_37foldl140108$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40502, %struct.ScmObj* %args46874$_37foldl140108$3)
store volatile %struct.ScmObj* %args46874$_37foldl140108$4, %struct.ScmObj** %stackaddr$prim48528, align 8
%clofunc48529 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140108)
musttail call tailcc void %clofunc48529(%struct.ScmObj* %_37foldl140108, %struct.ScmObj* %args46874$_37foldl140108$4)
ret void
}

define tailcc void @proc_clo$ae40990(%struct.ScmObj* %env$ae40990,%struct.ScmObj* %current_45args46879) {
%stackaddr$prim48530 = alloca %struct.ScmObj*, align 8
%k40504 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46879)
store volatile %struct.ScmObj* %k40504, %struct.ScmObj** %stackaddr$prim48530, align 8
%stackaddr$prim48531 = alloca %struct.ScmObj*, align 8
%current_45args46880 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46879)
store volatile %struct.ScmObj* %current_45args46880, %struct.ScmObj** %stackaddr$prim48531, align 8
%stackaddr$prim48532 = alloca %struct.ScmObj*, align 8
%_37length40113 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46880)
store volatile %struct.ScmObj* %_37length40113, %struct.ScmObj** %stackaddr$prim48532, align 8
%ae40992 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48533 = alloca %struct.ScmObj*, align 8
%fptrToInt48534 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40993 to i64
%ae40993 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48534)
store volatile %struct.ScmObj* %ae40993, %struct.ScmObj** %stackaddr$makeclosure48533, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40993, %struct.ScmObj* %_37length40113, i64 0)
%args46891$k40504$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48535 = alloca %struct.ScmObj*, align 8
%args46891$k40504$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40993, %struct.ScmObj* %args46891$k40504$0)
store volatile %struct.ScmObj* %args46891$k40504$1, %struct.ScmObj** %stackaddr$prim48535, align 8
%stackaddr$prim48536 = alloca %struct.ScmObj*, align 8
%args46891$k40504$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40992, %struct.ScmObj* %args46891$k40504$1)
store volatile %struct.ScmObj* %args46891$k40504$2, %struct.ScmObj** %stackaddr$prim48536, align 8
%clofunc48537 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40504)
musttail call tailcc void %clofunc48537(%struct.ScmObj* %k40504, %struct.ScmObj* %args46891$k40504$2)
ret void
}

define tailcc void @proc_clo$ae40993(%struct.ScmObj* %env$ae40993,%struct.ScmObj* %current_45args46882) {
%stackaddr$env-ref48538 = alloca %struct.ScmObj*, align 8
%_37length40113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40993, i64 0)
store %struct.ScmObj* %_37length40113, %struct.ScmObj** %stackaddr$env-ref48538
%stackaddr$prim48539 = alloca %struct.ScmObj*, align 8
%k40505 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46882)
store volatile %struct.ScmObj* %k40505, %struct.ScmObj** %stackaddr$prim48539, align 8
%stackaddr$prim48540 = alloca %struct.ScmObj*, align 8
%current_45args46883 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46882)
store volatile %struct.ScmObj* %current_45args46883, %struct.ScmObj** %stackaddr$prim48540, align 8
%stackaddr$prim48541 = alloca %struct.ScmObj*, align 8
%lst40114 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46883)
store volatile %struct.ScmObj* %lst40114, %struct.ScmObj** %stackaddr$prim48541, align 8
%stackaddr$prim48542 = alloca %struct.ScmObj*, align 8
%anf_45bind40250 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40114)
store volatile %struct.ScmObj* %anf_45bind40250, %struct.ScmObj** %stackaddr$prim48542, align 8
%truthy$cmp48543 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40250)
%cmp$cmp48543 = icmp eq i64 %truthy$cmp48543, 1
br i1 %cmp$cmp48543, label %truebranch$cmp48543, label %falsebranch$cmp48543
truebranch$cmp48543:
%ae40997 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40998 = call %struct.ScmObj* @const_init_int(i64 0)
%args46885$k40505$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48544 = alloca %struct.ScmObj*, align 8
%args46885$k40505$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40998, %struct.ScmObj* %args46885$k40505$0)
store volatile %struct.ScmObj* %args46885$k40505$1, %struct.ScmObj** %stackaddr$prim48544, align 8
%stackaddr$prim48545 = alloca %struct.ScmObj*, align 8
%args46885$k40505$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40997, %struct.ScmObj* %args46885$k40505$1)
store volatile %struct.ScmObj* %args46885$k40505$2, %struct.ScmObj** %stackaddr$prim48545, align 8
%clofunc48546 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40505)
musttail call tailcc void %clofunc48546(%struct.ScmObj* %k40505, %struct.ScmObj* %args46885$k40505$2)
ret void
falsebranch$cmp48543:
%stackaddr$prim48547 = alloca %struct.ScmObj*, align 8
%anf_45bind40251 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40114)
store volatile %struct.ScmObj* %anf_45bind40251, %struct.ScmObj** %stackaddr$prim48547, align 8
%stackaddr$makeclosure48548 = alloca %struct.ScmObj*, align 8
%fptrToInt48549 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41007 to i64
%ae41007 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48549)
store volatile %struct.ScmObj* %ae41007, %struct.ScmObj** %stackaddr$makeclosure48548, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41007, %struct.ScmObj* %k40505, i64 0)
%args46890$_37length40113$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48550 = alloca %struct.ScmObj*, align 8
%args46890$_37length40113$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40251, %struct.ScmObj* %args46890$_37length40113$0)
store volatile %struct.ScmObj* %args46890$_37length40113$1, %struct.ScmObj** %stackaddr$prim48550, align 8
%stackaddr$prim48551 = alloca %struct.ScmObj*, align 8
%args46890$_37length40113$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41007, %struct.ScmObj* %args46890$_37length40113$1)
store volatile %struct.ScmObj* %args46890$_37length40113$2, %struct.ScmObj** %stackaddr$prim48551, align 8
%clofunc48552 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length40113)
musttail call tailcc void %clofunc48552(%struct.ScmObj* %_37length40113, %struct.ScmObj* %args46890$_37length40113$2)
ret void
}

define tailcc void @proc_clo$ae41007(%struct.ScmObj* %env$ae41007,%struct.ScmObj* %current_45args46886) {
%stackaddr$env-ref48553 = alloca %struct.ScmObj*, align 8
%k40505 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41007, i64 0)
store %struct.ScmObj* %k40505, %struct.ScmObj** %stackaddr$env-ref48553
%stackaddr$prim48554 = alloca %struct.ScmObj*, align 8
%_95k40506 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46886)
store volatile %struct.ScmObj* %_95k40506, %struct.ScmObj** %stackaddr$prim48554, align 8
%stackaddr$prim48555 = alloca %struct.ScmObj*, align 8
%current_45args46887 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46886)
store volatile %struct.ScmObj* %current_45args46887, %struct.ScmObj** %stackaddr$prim48555, align 8
%stackaddr$prim48556 = alloca %struct.ScmObj*, align 8
%anf_45bind40252 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46887)
store volatile %struct.ScmObj* %anf_45bind40252, %struct.ScmObj** %stackaddr$prim48556, align 8
%ae41009 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim48557 = alloca %struct.ScmObj*, align 8
%cpsprim40507 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae41009, %struct.ScmObj* %anf_45bind40252)
store volatile %struct.ScmObj* %cpsprim40507, %struct.ScmObj** %stackaddr$prim48557, align 8
%ae41012 = call %struct.ScmObj* @const_init_int(i64 0)
%args46889$k40505$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48558 = alloca %struct.ScmObj*, align 8
%args46889$k40505$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40507, %struct.ScmObj* %args46889$k40505$0)
store volatile %struct.ScmObj* %args46889$k40505$1, %struct.ScmObj** %stackaddr$prim48558, align 8
%stackaddr$prim48559 = alloca %struct.ScmObj*, align 8
%args46889$k40505$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41012, %struct.ScmObj* %args46889$k40505$1)
store volatile %struct.ScmObj* %args46889$k40505$2, %struct.ScmObj** %stackaddr$prim48559, align 8
%clofunc48560 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40505)
musttail call tailcc void %clofunc48560(%struct.ScmObj* %k40505, %struct.ScmObj* %args46889$k40505$2)
ret void
}

define tailcc void @proc_clo$ae40840(%struct.ScmObj* %env$ae40840,%struct.ScmObj* %current_45args46894) {
%stackaddr$prim48561 = alloca %struct.ScmObj*, align 8
%k40508 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46894)
store volatile %struct.ScmObj* %k40508, %struct.ScmObj** %stackaddr$prim48561, align 8
%stackaddr$prim48562 = alloca %struct.ScmObj*, align 8
%current_45args46895 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46894)
store volatile %struct.ScmObj* %current_45args46895, %struct.ScmObj** %stackaddr$prim48562, align 8
%stackaddr$prim48563 = alloca %struct.ScmObj*, align 8
%_37take40116 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46895)
store volatile %struct.ScmObj* %_37take40116, %struct.ScmObj** %stackaddr$prim48563, align 8
%ae40842 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48564 = alloca %struct.ScmObj*, align 8
%fptrToInt48565 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40843 to i64
%ae40843 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48565)
store volatile %struct.ScmObj* %ae40843, %struct.ScmObj** %stackaddr$makeclosure48564, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40843, %struct.ScmObj* %_37take40116, i64 0)
%args46908$k40508$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48566 = alloca %struct.ScmObj*, align 8
%args46908$k40508$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40843, %struct.ScmObj* %args46908$k40508$0)
store volatile %struct.ScmObj* %args46908$k40508$1, %struct.ScmObj** %stackaddr$prim48566, align 8
%stackaddr$prim48567 = alloca %struct.ScmObj*, align 8
%args46908$k40508$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40842, %struct.ScmObj* %args46908$k40508$1)
store volatile %struct.ScmObj* %args46908$k40508$2, %struct.ScmObj** %stackaddr$prim48567, align 8
%clofunc48568 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40508)
musttail call tailcc void %clofunc48568(%struct.ScmObj* %k40508, %struct.ScmObj* %args46908$k40508$2)
ret void
}

define tailcc void @proc_clo$ae40843(%struct.ScmObj* %env$ae40843,%struct.ScmObj* %current_45args46897) {
%stackaddr$env-ref48569 = alloca %struct.ScmObj*, align 8
%_37take40116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40843, i64 0)
store %struct.ScmObj* %_37take40116, %struct.ScmObj** %stackaddr$env-ref48569
%stackaddr$prim48570 = alloca %struct.ScmObj*, align 8
%k40509 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46897)
store volatile %struct.ScmObj* %k40509, %struct.ScmObj** %stackaddr$prim48570, align 8
%stackaddr$prim48571 = alloca %struct.ScmObj*, align 8
%current_45args46898 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46897)
store volatile %struct.ScmObj* %current_45args46898, %struct.ScmObj** %stackaddr$prim48571, align 8
%stackaddr$prim48572 = alloca %struct.ScmObj*, align 8
%lst40118 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46898)
store volatile %struct.ScmObj* %lst40118, %struct.ScmObj** %stackaddr$prim48572, align 8
%stackaddr$prim48573 = alloca %struct.ScmObj*, align 8
%current_45args46899 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46898)
store volatile %struct.ScmObj* %current_45args46899, %struct.ScmObj** %stackaddr$prim48573, align 8
%stackaddr$prim48574 = alloca %struct.ScmObj*, align 8
%n40117 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46899)
store volatile %struct.ScmObj* %n40117, %struct.ScmObj** %stackaddr$prim48574, align 8
%ae40845 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48575 = alloca %struct.ScmObj*, align 8
%anf_45bind40243 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n40117, %struct.ScmObj* %ae40845)
store volatile %struct.ScmObj* %anf_45bind40243, %struct.ScmObj** %stackaddr$prim48575, align 8
%truthy$cmp48576 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40243)
%cmp$cmp48576 = icmp eq i64 %truthy$cmp48576, 1
br i1 %cmp$cmp48576, label %truebranch$cmp48576, label %falsebranch$cmp48576
truebranch$cmp48576:
%ae40848 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40849 = call %struct.ScmObj* @const_init_null()
%args46901$k40509$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48577 = alloca %struct.ScmObj*, align 8
%args46901$k40509$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40849, %struct.ScmObj* %args46901$k40509$0)
store volatile %struct.ScmObj* %args46901$k40509$1, %struct.ScmObj** %stackaddr$prim48577, align 8
%stackaddr$prim48578 = alloca %struct.ScmObj*, align 8
%args46901$k40509$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40848, %struct.ScmObj* %args46901$k40509$1)
store volatile %struct.ScmObj* %args46901$k40509$2, %struct.ScmObj** %stackaddr$prim48578, align 8
%clofunc48579 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40509)
musttail call tailcc void %clofunc48579(%struct.ScmObj* %k40509, %struct.ScmObj* %args46901$k40509$2)
ret void
falsebranch$cmp48576:
%stackaddr$prim48580 = alloca %struct.ScmObj*, align 8
%anf_45bind40244 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40118)
store volatile %struct.ScmObj* %anf_45bind40244, %struct.ScmObj** %stackaddr$prim48580, align 8
%truthy$cmp48581 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40244)
%cmp$cmp48581 = icmp eq i64 %truthy$cmp48581, 1
br i1 %cmp$cmp48581, label %truebranch$cmp48581, label %falsebranch$cmp48581
truebranch$cmp48581:
%ae40859 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40860 = call %struct.ScmObj* @const_init_null()
%args46902$k40509$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48582 = alloca %struct.ScmObj*, align 8
%args46902$k40509$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40860, %struct.ScmObj* %args46902$k40509$0)
store volatile %struct.ScmObj* %args46902$k40509$1, %struct.ScmObj** %stackaddr$prim48582, align 8
%stackaddr$prim48583 = alloca %struct.ScmObj*, align 8
%args46902$k40509$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40859, %struct.ScmObj* %args46902$k40509$1)
store volatile %struct.ScmObj* %args46902$k40509$2, %struct.ScmObj** %stackaddr$prim48583, align 8
%clofunc48584 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40509)
musttail call tailcc void %clofunc48584(%struct.ScmObj* %k40509, %struct.ScmObj* %args46902$k40509$2)
ret void
falsebranch$cmp48581:
%stackaddr$prim48585 = alloca %struct.ScmObj*, align 8
%anf_45bind40245 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40118)
store volatile %struct.ScmObj* %anf_45bind40245, %struct.ScmObj** %stackaddr$prim48585, align 8
%stackaddr$prim48586 = alloca %struct.ScmObj*, align 8
%anf_45bind40246 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40118)
store volatile %struct.ScmObj* %anf_45bind40246, %struct.ScmObj** %stackaddr$prim48586, align 8
%ae40870 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim48587 = alloca %struct.ScmObj*, align 8
%anf_45bind40247 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n40117, %struct.ScmObj* %ae40870)
store volatile %struct.ScmObj* %anf_45bind40247, %struct.ScmObj** %stackaddr$prim48587, align 8
%stackaddr$makeclosure48588 = alloca %struct.ScmObj*, align 8
%fptrToInt48589 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40872 to i64
%ae40872 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48589)
store volatile %struct.ScmObj* %ae40872, %struct.ScmObj** %stackaddr$makeclosure48588, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40872, %struct.ScmObj* %anf_45bind40245, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40872, %struct.ScmObj* %k40509, i64 1)
%args46907$_37take40116$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48590 = alloca %struct.ScmObj*, align 8
%args46907$_37take40116$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40247, %struct.ScmObj* %args46907$_37take40116$0)
store volatile %struct.ScmObj* %args46907$_37take40116$1, %struct.ScmObj** %stackaddr$prim48590, align 8
%stackaddr$prim48591 = alloca %struct.ScmObj*, align 8
%args46907$_37take40116$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40246, %struct.ScmObj* %args46907$_37take40116$1)
store volatile %struct.ScmObj* %args46907$_37take40116$2, %struct.ScmObj** %stackaddr$prim48591, align 8
%stackaddr$prim48592 = alloca %struct.ScmObj*, align 8
%args46907$_37take40116$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40872, %struct.ScmObj* %args46907$_37take40116$2)
store volatile %struct.ScmObj* %args46907$_37take40116$3, %struct.ScmObj** %stackaddr$prim48592, align 8
%clofunc48593 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take40116)
musttail call tailcc void %clofunc48593(%struct.ScmObj* %_37take40116, %struct.ScmObj* %args46907$_37take40116$3)
ret void
}

define tailcc void @proc_clo$ae40872(%struct.ScmObj* %env$ae40872,%struct.ScmObj* %current_45args46903) {
%stackaddr$env-ref48594 = alloca %struct.ScmObj*, align 8
%anf_45bind40245 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40872, i64 0)
store %struct.ScmObj* %anf_45bind40245, %struct.ScmObj** %stackaddr$env-ref48594
%stackaddr$env-ref48595 = alloca %struct.ScmObj*, align 8
%k40509 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40872, i64 1)
store %struct.ScmObj* %k40509, %struct.ScmObj** %stackaddr$env-ref48595
%stackaddr$prim48596 = alloca %struct.ScmObj*, align 8
%_95k40510 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46903)
store volatile %struct.ScmObj* %_95k40510, %struct.ScmObj** %stackaddr$prim48596, align 8
%stackaddr$prim48597 = alloca %struct.ScmObj*, align 8
%current_45args46904 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46903)
store volatile %struct.ScmObj* %current_45args46904, %struct.ScmObj** %stackaddr$prim48597, align 8
%stackaddr$prim48598 = alloca %struct.ScmObj*, align 8
%anf_45bind40248 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46904)
store volatile %struct.ScmObj* %anf_45bind40248, %struct.ScmObj** %stackaddr$prim48598, align 8
%stackaddr$prim48599 = alloca %struct.ScmObj*, align 8
%cpsprim40511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40245, %struct.ScmObj* %anf_45bind40248)
store volatile %struct.ScmObj* %cpsprim40511, %struct.ScmObj** %stackaddr$prim48599, align 8
%ae40878 = call %struct.ScmObj* @const_init_int(i64 0)
%args46906$k40509$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48600 = alloca %struct.ScmObj*, align 8
%args46906$k40509$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40511, %struct.ScmObj* %args46906$k40509$0)
store volatile %struct.ScmObj* %args46906$k40509$1, %struct.ScmObj** %stackaddr$prim48600, align 8
%stackaddr$prim48601 = alloca %struct.ScmObj*, align 8
%args46906$k40509$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40878, %struct.ScmObj* %args46906$k40509$1)
store volatile %struct.ScmObj* %args46906$k40509$2, %struct.ScmObj** %stackaddr$prim48601, align 8
%clofunc48602 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40509)
musttail call tailcc void %clofunc48602(%struct.ScmObj* %k40509, %struct.ScmObj* %args46906$k40509$2)
ret void
}

define tailcc void @proc_clo$ae40743(%struct.ScmObj* %env$ae40743,%struct.ScmObj* %current_45args46911) {
%stackaddr$prim48603 = alloca %struct.ScmObj*, align 8
%k40512 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46911)
store volatile %struct.ScmObj* %k40512, %struct.ScmObj** %stackaddr$prim48603, align 8
%stackaddr$prim48604 = alloca %struct.ScmObj*, align 8
%current_45args46912 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46911)
store volatile %struct.ScmObj* %current_45args46912, %struct.ScmObj** %stackaddr$prim48604, align 8
%stackaddr$prim48605 = alloca %struct.ScmObj*, align 8
%_37map40120 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46912)
store volatile %struct.ScmObj* %_37map40120, %struct.ScmObj** %stackaddr$prim48605, align 8
%ae40745 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48606 = alloca %struct.ScmObj*, align 8
%fptrToInt48607 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40746 to i64
%ae40746 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48607)
store volatile %struct.ScmObj* %ae40746, %struct.ScmObj** %stackaddr$makeclosure48606, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40746, %struct.ScmObj* %_37map40120, i64 0)
%args46928$k40512$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48608 = alloca %struct.ScmObj*, align 8
%args46928$k40512$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40746, %struct.ScmObj* %args46928$k40512$0)
store volatile %struct.ScmObj* %args46928$k40512$1, %struct.ScmObj** %stackaddr$prim48608, align 8
%stackaddr$prim48609 = alloca %struct.ScmObj*, align 8
%args46928$k40512$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40745, %struct.ScmObj* %args46928$k40512$1)
store volatile %struct.ScmObj* %args46928$k40512$2, %struct.ScmObj** %stackaddr$prim48609, align 8
%clofunc48610 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40512)
musttail call tailcc void %clofunc48610(%struct.ScmObj* %k40512, %struct.ScmObj* %args46928$k40512$2)
ret void
}

define tailcc void @proc_clo$ae40746(%struct.ScmObj* %env$ae40746,%struct.ScmObj* %current_45args46914) {
%stackaddr$env-ref48611 = alloca %struct.ScmObj*, align 8
%_37map40120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40746, i64 0)
store %struct.ScmObj* %_37map40120, %struct.ScmObj** %stackaddr$env-ref48611
%stackaddr$prim48612 = alloca %struct.ScmObj*, align 8
%k40513 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46914)
store volatile %struct.ScmObj* %k40513, %struct.ScmObj** %stackaddr$prim48612, align 8
%stackaddr$prim48613 = alloca %struct.ScmObj*, align 8
%current_45args46915 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46914)
store volatile %struct.ScmObj* %current_45args46915, %struct.ScmObj** %stackaddr$prim48613, align 8
%stackaddr$prim48614 = alloca %struct.ScmObj*, align 8
%f40122 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46915)
store volatile %struct.ScmObj* %f40122, %struct.ScmObj** %stackaddr$prim48614, align 8
%stackaddr$prim48615 = alloca %struct.ScmObj*, align 8
%current_45args46916 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46915)
store volatile %struct.ScmObj* %current_45args46916, %struct.ScmObj** %stackaddr$prim48615, align 8
%stackaddr$prim48616 = alloca %struct.ScmObj*, align 8
%lst40121 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46916)
store volatile %struct.ScmObj* %lst40121, %struct.ScmObj** %stackaddr$prim48616, align 8
%stackaddr$prim48617 = alloca %struct.ScmObj*, align 8
%anf_45bind40237 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40121)
store volatile %struct.ScmObj* %anf_45bind40237, %struct.ScmObj** %stackaddr$prim48617, align 8
%truthy$cmp48618 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40237)
%cmp$cmp48618 = icmp eq i64 %truthy$cmp48618, 1
br i1 %cmp$cmp48618, label %truebranch$cmp48618, label %falsebranch$cmp48618
truebranch$cmp48618:
%ae40750 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40751 = call %struct.ScmObj* @const_init_null()
%args46918$k40513$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48619 = alloca %struct.ScmObj*, align 8
%args46918$k40513$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40751, %struct.ScmObj* %args46918$k40513$0)
store volatile %struct.ScmObj* %args46918$k40513$1, %struct.ScmObj** %stackaddr$prim48619, align 8
%stackaddr$prim48620 = alloca %struct.ScmObj*, align 8
%args46918$k40513$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40750, %struct.ScmObj* %args46918$k40513$1)
store volatile %struct.ScmObj* %args46918$k40513$2, %struct.ScmObj** %stackaddr$prim48620, align 8
%clofunc48621 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40513)
musttail call tailcc void %clofunc48621(%struct.ScmObj* %k40513, %struct.ScmObj* %args46918$k40513$2)
ret void
falsebranch$cmp48618:
%stackaddr$prim48622 = alloca %struct.ScmObj*, align 8
%anf_45bind40238 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40121)
store volatile %struct.ScmObj* %anf_45bind40238, %struct.ScmObj** %stackaddr$prim48622, align 8
%stackaddr$makeclosure48623 = alloca %struct.ScmObj*, align 8
%fptrToInt48624 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40760 to i64
%ae40760 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48624)
store volatile %struct.ScmObj* %ae40760, %struct.ScmObj** %stackaddr$makeclosure48623, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40760, %struct.ScmObj* %k40513, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40760, %struct.ScmObj* %f40122, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40760, %struct.ScmObj* %lst40121, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae40760, %struct.ScmObj* %_37map40120, i64 3)
%args46927$f40122$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48625 = alloca %struct.ScmObj*, align 8
%args46927$f40122$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40238, %struct.ScmObj* %args46927$f40122$0)
store volatile %struct.ScmObj* %args46927$f40122$1, %struct.ScmObj** %stackaddr$prim48625, align 8
%stackaddr$prim48626 = alloca %struct.ScmObj*, align 8
%args46927$f40122$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40760, %struct.ScmObj* %args46927$f40122$1)
store volatile %struct.ScmObj* %args46927$f40122$2, %struct.ScmObj** %stackaddr$prim48626, align 8
%clofunc48627 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40122)
musttail call tailcc void %clofunc48627(%struct.ScmObj* %f40122, %struct.ScmObj* %args46927$f40122$2)
ret void
}

define tailcc void @proc_clo$ae40760(%struct.ScmObj* %env$ae40760,%struct.ScmObj* %current_45args46919) {
%stackaddr$env-ref48628 = alloca %struct.ScmObj*, align 8
%k40513 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40760, i64 0)
store %struct.ScmObj* %k40513, %struct.ScmObj** %stackaddr$env-ref48628
%stackaddr$env-ref48629 = alloca %struct.ScmObj*, align 8
%f40122 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40760, i64 1)
store %struct.ScmObj* %f40122, %struct.ScmObj** %stackaddr$env-ref48629
%stackaddr$env-ref48630 = alloca %struct.ScmObj*, align 8
%lst40121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40760, i64 2)
store %struct.ScmObj* %lst40121, %struct.ScmObj** %stackaddr$env-ref48630
%stackaddr$env-ref48631 = alloca %struct.ScmObj*, align 8
%_37map40120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40760, i64 3)
store %struct.ScmObj* %_37map40120, %struct.ScmObj** %stackaddr$env-ref48631
%stackaddr$prim48632 = alloca %struct.ScmObj*, align 8
%_95k40514 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46919)
store volatile %struct.ScmObj* %_95k40514, %struct.ScmObj** %stackaddr$prim48632, align 8
%stackaddr$prim48633 = alloca %struct.ScmObj*, align 8
%current_45args46920 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46919)
store volatile %struct.ScmObj* %current_45args46920, %struct.ScmObj** %stackaddr$prim48633, align 8
%stackaddr$prim48634 = alloca %struct.ScmObj*, align 8
%anf_45bind40239 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46920)
store volatile %struct.ScmObj* %anf_45bind40239, %struct.ScmObj** %stackaddr$prim48634, align 8
%stackaddr$prim48635 = alloca %struct.ScmObj*, align 8
%anf_45bind40240 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40121)
store volatile %struct.ScmObj* %anf_45bind40240, %struct.ScmObj** %stackaddr$prim48635, align 8
%stackaddr$makeclosure48636 = alloca %struct.ScmObj*, align 8
%fptrToInt48637 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40764 to i64
%ae40764 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48637)
store volatile %struct.ScmObj* %ae40764, %struct.ScmObj** %stackaddr$makeclosure48636, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40764, %struct.ScmObj* %k40513, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40764, %struct.ScmObj* %anf_45bind40239, i64 1)
%args46926$_37map40120$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48638 = alloca %struct.ScmObj*, align 8
%args46926$_37map40120$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40240, %struct.ScmObj* %args46926$_37map40120$0)
store volatile %struct.ScmObj* %args46926$_37map40120$1, %struct.ScmObj** %stackaddr$prim48638, align 8
%stackaddr$prim48639 = alloca %struct.ScmObj*, align 8
%args46926$_37map40120$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40122, %struct.ScmObj* %args46926$_37map40120$1)
store volatile %struct.ScmObj* %args46926$_37map40120$2, %struct.ScmObj** %stackaddr$prim48639, align 8
%stackaddr$prim48640 = alloca %struct.ScmObj*, align 8
%args46926$_37map40120$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40764, %struct.ScmObj* %args46926$_37map40120$2)
store volatile %struct.ScmObj* %args46926$_37map40120$3, %struct.ScmObj** %stackaddr$prim48640, align 8
%clofunc48641 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map40120)
musttail call tailcc void %clofunc48641(%struct.ScmObj* %_37map40120, %struct.ScmObj* %args46926$_37map40120$3)
ret void
}

define tailcc void @proc_clo$ae40764(%struct.ScmObj* %env$ae40764,%struct.ScmObj* %current_45args46922) {
%stackaddr$env-ref48642 = alloca %struct.ScmObj*, align 8
%k40513 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40764, i64 0)
store %struct.ScmObj* %k40513, %struct.ScmObj** %stackaddr$env-ref48642
%stackaddr$env-ref48643 = alloca %struct.ScmObj*, align 8
%anf_45bind40239 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40764, i64 1)
store %struct.ScmObj* %anf_45bind40239, %struct.ScmObj** %stackaddr$env-ref48643
%stackaddr$prim48644 = alloca %struct.ScmObj*, align 8
%_95k40515 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46922)
store volatile %struct.ScmObj* %_95k40515, %struct.ScmObj** %stackaddr$prim48644, align 8
%stackaddr$prim48645 = alloca %struct.ScmObj*, align 8
%current_45args46923 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46922)
store volatile %struct.ScmObj* %current_45args46923, %struct.ScmObj** %stackaddr$prim48645, align 8
%stackaddr$prim48646 = alloca %struct.ScmObj*, align 8
%anf_45bind40241 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46923)
store volatile %struct.ScmObj* %anf_45bind40241, %struct.ScmObj** %stackaddr$prim48646, align 8
%stackaddr$prim48647 = alloca %struct.ScmObj*, align 8
%cpsprim40516 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40239, %struct.ScmObj* %anf_45bind40241)
store volatile %struct.ScmObj* %cpsprim40516, %struct.ScmObj** %stackaddr$prim48647, align 8
%ae40770 = call %struct.ScmObj* @const_init_int(i64 0)
%args46925$k40513$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48648 = alloca %struct.ScmObj*, align 8
%args46925$k40513$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40516, %struct.ScmObj* %args46925$k40513$0)
store volatile %struct.ScmObj* %args46925$k40513$1, %struct.ScmObj** %stackaddr$prim48648, align 8
%stackaddr$prim48649 = alloca %struct.ScmObj*, align 8
%args46925$k40513$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40770, %struct.ScmObj* %args46925$k40513$1)
store volatile %struct.ScmObj* %args46925$k40513$2, %struct.ScmObj** %stackaddr$prim48649, align 8
%clofunc48650 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40513)
musttail call tailcc void %clofunc48650(%struct.ScmObj* %k40513, %struct.ScmObj* %args46925$k40513$2)
ret void
}

define tailcc void @proc_clo$ae40663(%struct.ScmObj* %env$ae40663,%struct.ScmObj* %current_45args46931) {
%stackaddr$prim48651 = alloca %struct.ScmObj*, align 8
%k40517 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46931)
store volatile %struct.ScmObj* %k40517, %struct.ScmObj** %stackaddr$prim48651, align 8
%stackaddr$prim48652 = alloca %struct.ScmObj*, align 8
%current_45args46932 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46931)
store volatile %struct.ScmObj* %current_45args46932, %struct.ScmObj** %stackaddr$prim48652, align 8
%stackaddr$prim48653 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46932)
store volatile %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$prim48653, align 8
%ae40665 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48654 = alloca %struct.ScmObj*, align 8
%fptrToInt48655 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40666 to i64
%ae40666 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48655)
store volatile %struct.ScmObj* %ae40666, %struct.ScmObj** %stackaddr$makeclosure48654, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40666, %struct.ScmObj* %_37foldr140124, i64 0)
%args46945$k40517$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48656 = alloca %struct.ScmObj*, align 8
%args46945$k40517$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40666, %struct.ScmObj* %args46945$k40517$0)
store volatile %struct.ScmObj* %args46945$k40517$1, %struct.ScmObj** %stackaddr$prim48656, align 8
%stackaddr$prim48657 = alloca %struct.ScmObj*, align 8
%args46945$k40517$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40665, %struct.ScmObj* %args46945$k40517$1)
store volatile %struct.ScmObj* %args46945$k40517$2, %struct.ScmObj** %stackaddr$prim48657, align 8
%clofunc48658 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40517)
musttail call tailcc void %clofunc48658(%struct.ScmObj* %k40517, %struct.ScmObj* %args46945$k40517$2)
ret void
}

define tailcc void @proc_clo$ae40666(%struct.ScmObj* %env$ae40666,%struct.ScmObj* %current_45args46934) {
%stackaddr$env-ref48659 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40666, i64 0)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48659
%stackaddr$prim48660 = alloca %struct.ScmObj*, align 8
%k40518 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46934)
store volatile %struct.ScmObj* %k40518, %struct.ScmObj** %stackaddr$prim48660, align 8
%stackaddr$prim48661 = alloca %struct.ScmObj*, align 8
%current_45args46935 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46934)
store volatile %struct.ScmObj* %current_45args46935, %struct.ScmObj** %stackaddr$prim48661, align 8
%stackaddr$prim48662 = alloca %struct.ScmObj*, align 8
%f40127 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46935)
store volatile %struct.ScmObj* %f40127, %struct.ScmObj** %stackaddr$prim48662, align 8
%stackaddr$prim48663 = alloca %struct.ScmObj*, align 8
%current_45args46936 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46935)
store volatile %struct.ScmObj* %current_45args46936, %struct.ScmObj** %stackaddr$prim48663, align 8
%stackaddr$prim48664 = alloca %struct.ScmObj*, align 8
%acc40126 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46936)
store volatile %struct.ScmObj* %acc40126, %struct.ScmObj** %stackaddr$prim48664, align 8
%stackaddr$prim48665 = alloca %struct.ScmObj*, align 8
%current_45args46937 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46936)
store volatile %struct.ScmObj* %current_45args46937, %struct.ScmObj** %stackaddr$prim48665, align 8
%stackaddr$prim48666 = alloca %struct.ScmObj*, align 8
%lst40125 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46937)
store volatile %struct.ScmObj* %lst40125, %struct.ScmObj** %stackaddr$prim48666, align 8
%stackaddr$prim48667 = alloca %struct.ScmObj*, align 8
%anf_45bind40232 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40125)
store volatile %struct.ScmObj* %anf_45bind40232, %struct.ScmObj** %stackaddr$prim48667, align 8
%truthy$cmp48668 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40232)
%cmp$cmp48668 = icmp eq i64 %truthy$cmp48668, 1
br i1 %cmp$cmp48668, label %truebranch$cmp48668, label %falsebranch$cmp48668
truebranch$cmp48668:
%ae40670 = call %struct.ScmObj* @const_init_int(i64 0)
%args46939$k40518$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48669 = alloca %struct.ScmObj*, align 8
%args46939$k40518$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40126, %struct.ScmObj* %args46939$k40518$0)
store volatile %struct.ScmObj* %args46939$k40518$1, %struct.ScmObj** %stackaddr$prim48669, align 8
%stackaddr$prim48670 = alloca %struct.ScmObj*, align 8
%args46939$k40518$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40670, %struct.ScmObj* %args46939$k40518$1)
store volatile %struct.ScmObj* %args46939$k40518$2, %struct.ScmObj** %stackaddr$prim48670, align 8
%clofunc48671 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40518)
musttail call tailcc void %clofunc48671(%struct.ScmObj* %k40518, %struct.ScmObj* %args46939$k40518$2)
ret void
falsebranch$cmp48668:
%stackaddr$prim48672 = alloca %struct.ScmObj*, align 8
%anf_45bind40233 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40125)
store volatile %struct.ScmObj* %anf_45bind40233, %struct.ScmObj** %stackaddr$prim48672, align 8
%stackaddr$prim48673 = alloca %struct.ScmObj*, align 8
%anf_45bind40234 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40125)
store volatile %struct.ScmObj* %anf_45bind40234, %struct.ScmObj** %stackaddr$prim48673, align 8
%stackaddr$makeclosure48674 = alloca %struct.ScmObj*, align 8
%fptrToInt48675 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40678 to i64
%ae40678 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48675)
store volatile %struct.ScmObj* %ae40678, %struct.ScmObj** %stackaddr$makeclosure48674, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40678, %struct.ScmObj* %k40518, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40678, %struct.ScmObj* %f40127, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40678, %struct.ScmObj* %anf_45bind40233, i64 2)
%args46944$_37foldr140124$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48676 = alloca %struct.ScmObj*, align 8
%args46944$_37foldr140124$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40234, %struct.ScmObj* %args46944$_37foldr140124$0)
store volatile %struct.ScmObj* %args46944$_37foldr140124$1, %struct.ScmObj** %stackaddr$prim48676, align 8
%stackaddr$prim48677 = alloca %struct.ScmObj*, align 8
%args46944$_37foldr140124$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40126, %struct.ScmObj* %args46944$_37foldr140124$1)
store volatile %struct.ScmObj* %args46944$_37foldr140124$2, %struct.ScmObj** %stackaddr$prim48677, align 8
%stackaddr$prim48678 = alloca %struct.ScmObj*, align 8
%args46944$_37foldr140124$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40127, %struct.ScmObj* %args46944$_37foldr140124$2)
store volatile %struct.ScmObj* %args46944$_37foldr140124$3, %struct.ScmObj** %stackaddr$prim48678, align 8
%stackaddr$prim48679 = alloca %struct.ScmObj*, align 8
%args46944$_37foldr140124$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40678, %struct.ScmObj* %args46944$_37foldr140124$3)
store volatile %struct.ScmObj* %args46944$_37foldr140124$4, %struct.ScmObj** %stackaddr$prim48679, align 8
%clofunc48680 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140124)
musttail call tailcc void %clofunc48680(%struct.ScmObj* %_37foldr140124, %struct.ScmObj* %args46944$_37foldr140124$4)
ret void
}

define tailcc void @proc_clo$ae40678(%struct.ScmObj* %env$ae40678,%struct.ScmObj* %current_45args46940) {
%stackaddr$env-ref48681 = alloca %struct.ScmObj*, align 8
%k40518 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40678, i64 0)
store %struct.ScmObj* %k40518, %struct.ScmObj** %stackaddr$env-ref48681
%stackaddr$env-ref48682 = alloca %struct.ScmObj*, align 8
%f40127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40678, i64 1)
store %struct.ScmObj* %f40127, %struct.ScmObj** %stackaddr$env-ref48682
%stackaddr$env-ref48683 = alloca %struct.ScmObj*, align 8
%anf_45bind40233 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40678, i64 2)
store %struct.ScmObj* %anf_45bind40233, %struct.ScmObj** %stackaddr$env-ref48683
%stackaddr$prim48684 = alloca %struct.ScmObj*, align 8
%_95k40519 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46940)
store volatile %struct.ScmObj* %_95k40519, %struct.ScmObj** %stackaddr$prim48684, align 8
%stackaddr$prim48685 = alloca %struct.ScmObj*, align 8
%current_45args46941 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46940)
store volatile %struct.ScmObj* %current_45args46941, %struct.ScmObj** %stackaddr$prim48685, align 8
%stackaddr$prim48686 = alloca %struct.ScmObj*, align 8
%anf_45bind40235 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46941)
store volatile %struct.ScmObj* %anf_45bind40235, %struct.ScmObj** %stackaddr$prim48686, align 8
%args46943$f40127$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48687 = alloca %struct.ScmObj*, align 8
%args46943$f40127$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40235, %struct.ScmObj* %args46943$f40127$0)
store volatile %struct.ScmObj* %args46943$f40127$1, %struct.ScmObj** %stackaddr$prim48687, align 8
%stackaddr$prim48688 = alloca %struct.ScmObj*, align 8
%args46943$f40127$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40233, %struct.ScmObj* %args46943$f40127$1)
store volatile %struct.ScmObj* %args46943$f40127$2, %struct.ScmObj** %stackaddr$prim48688, align 8
%stackaddr$prim48689 = alloca %struct.ScmObj*, align 8
%args46943$f40127$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40518, %struct.ScmObj* %args46943$f40127$2)
store volatile %struct.ScmObj* %args46943$f40127$3, %struct.ScmObj** %stackaddr$prim48689, align 8
%clofunc48690 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40127)
musttail call tailcc void %clofunc48690(%struct.ScmObj* %f40127, %struct.ScmObj* %args46943$f40127$3)
ret void
}

define tailcc void @proc_clo$ae40546(%struct.ScmObj* %env$ae40546,%struct.ScmObj* %current_45args46948) {
%stackaddr$prim48691 = alloca %struct.ScmObj*, align 8
%k40520 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46948)
store volatile %struct.ScmObj* %k40520, %struct.ScmObj** %stackaddr$prim48691, align 8
%stackaddr$prim48692 = alloca %struct.ScmObj*, align 8
%current_45args46949 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46948)
store volatile %struct.ScmObj* %current_45args46949, %struct.ScmObj** %stackaddr$prim48692, align 8
%stackaddr$prim48693 = alloca %struct.ScmObj*, align 8
%y40104 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46949)
store volatile %struct.ScmObj* %y40104, %struct.ScmObj** %stackaddr$prim48693, align 8
%ae40548 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48694 = alloca %struct.ScmObj*, align 8
%fptrToInt48695 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40549 to i64
%ae40549 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48695)
store volatile %struct.ScmObj* %ae40549, %struct.ScmObj** %stackaddr$makeclosure48694, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40549, %struct.ScmObj* %y40104, i64 0)
%args46967$k40520$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48696 = alloca %struct.ScmObj*, align 8
%args46967$k40520$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40549, %struct.ScmObj* %args46967$k40520$0)
store volatile %struct.ScmObj* %args46967$k40520$1, %struct.ScmObj** %stackaddr$prim48696, align 8
%stackaddr$prim48697 = alloca %struct.ScmObj*, align 8
%args46967$k40520$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40548, %struct.ScmObj* %args46967$k40520$1)
store volatile %struct.ScmObj* %args46967$k40520$2, %struct.ScmObj** %stackaddr$prim48697, align 8
%clofunc48698 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40520)
musttail call tailcc void %clofunc48698(%struct.ScmObj* %k40520, %struct.ScmObj* %args46967$k40520$2)
ret void
}

define tailcc void @proc_clo$ae40549(%struct.ScmObj* %env$ae40549,%struct.ScmObj* %current_45args46951) {
%stackaddr$env-ref48699 = alloca %struct.ScmObj*, align 8
%y40104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40549, i64 0)
store %struct.ScmObj* %y40104, %struct.ScmObj** %stackaddr$env-ref48699
%stackaddr$prim48700 = alloca %struct.ScmObj*, align 8
%k40521 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46951)
store volatile %struct.ScmObj* %k40521, %struct.ScmObj** %stackaddr$prim48700, align 8
%stackaddr$prim48701 = alloca %struct.ScmObj*, align 8
%current_45args46952 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46951)
store volatile %struct.ScmObj* %current_45args46952, %struct.ScmObj** %stackaddr$prim48701, align 8
%stackaddr$prim48702 = alloca %struct.ScmObj*, align 8
%f40105 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46952)
store volatile %struct.ScmObj* %f40105, %struct.ScmObj** %stackaddr$prim48702, align 8
%stackaddr$makeclosure48703 = alloca %struct.ScmObj*, align 8
%fptrToInt48704 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40550 to i64
%ae40550 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48704)
store volatile %struct.ScmObj* %ae40550, %struct.ScmObj** %stackaddr$makeclosure48703, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40550, %struct.ScmObj* %k40521, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40550, %struct.ScmObj* %f40105, i64 1)
%ae40551 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48705 = alloca %struct.ScmObj*, align 8
%fptrToInt48706 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40552 to i64
%ae40552 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48706)
store volatile %struct.ScmObj* %ae40552, %struct.ScmObj** %stackaddr$makeclosure48705, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40552, %struct.ScmObj* %f40105, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40552, %struct.ScmObj* %y40104, i64 1)
%args46966$ae40550$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48707 = alloca %struct.ScmObj*, align 8
%args46966$ae40550$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40552, %struct.ScmObj* %args46966$ae40550$0)
store volatile %struct.ScmObj* %args46966$ae40550$1, %struct.ScmObj** %stackaddr$prim48707, align 8
%stackaddr$prim48708 = alloca %struct.ScmObj*, align 8
%args46966$ae40550$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40551, %struct.ScmObj* %args46966$ae40550$1)
store volatile %struct.ScmObj* %args46966$ae40550$2, %struct.ScmObj** %stackaddr$prim48708, align 8
%clofunc48709 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40550)
musttail call tailcc void %clofunc48709(%struct.ScmObj* %ae40550, %struct.ScmObj* %args46966$ae40550$2)
ret void
}

define tailcc void @proc_clo$ae40550(%struct.ScmObj* %env$ae40550,%struct.ScmObj* %current_45args46954) {
%stackaddr$env-ref48710 = alloca %struct.ScmObj*, align 8
%k40521 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40550, i64 0)
store %struct.ScmObj* %k40521, %struct.ScmObj** %stackaddr$env-ref48710
%stackaddr$env-ref48711 = alloca %struct.ScmObj*, align 8
%f40105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40550, i64 1)
store %struct.ScmObj* %f40105, %struct.ScmObj** %stackaddr$env-ref48711
%stackaddr$prim48712 = alloca %struct.ScmObj*, align 8
%_95k40522 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46954)
store volatile %struct.ScmObj* %_95k40522, %struct.ScmObj** %stackaddr$prim48712, align 8
%stackaddr$prim48713 = alloca %struct.ScmObj*, align 8
%current_45args46955 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46954)
store volatile %struct.ScmObj* %current_45args46955, %struct.ScmObj** %stackaddr$prim48713, align 8
%stackaddr$prim48714 = alloca %struct.ScmObj*, align 8
%anf_45bind40230 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46955)
store volatile %struct.ScmObj* %anf_45bind40230, %struct.ScmObj** %stackaddr$prim48714, align 8
%args46957$f40105$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48715 = alloca %struct.ScmObj*, align 8
%args46957$f40105$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40230, %struct.ScmObj* %args46957$f40105$0)
store volatile %struct.ScmObj* %args46957$f40105$1, %struct.ScmObj** %stackaddr$prim48715, align 8
%stackaddr$prim48716 = alloca %struct.ScmObj*, align 8
%args46957$f40105$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40521, %struct.ScmObj* %args46957$f40105$1)
store volatile %struct.ScmObj* %args46957$f40105$2, %struct.ScmObj** %stackaddr$prim48716, align 8
%clofunc48717 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40105)
musttail call tailcc void %clofunc48717(%struct.ScmObj* %f40105, %struct.ScmObj* %args46957$f40105$2)
ret void
}

define tailcc void @proc_clo$ae40552(%struct.ScmObj* %env$ae40552,%struct.ScmObj* %args4010640523) {
%stackaddr$env-ref48718 = alloca %struct.ScmObj*, align 8
%f40105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40552, i64 0)
store %struct.ScmObj* %f40105, %struct.ScmObj** %stackaddr$env-ref48718
%stackaddr$env-ref48719 = alloca %struct.ScmObj*, align 8
%y40104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40552, i64 1)
store %struct.ScmObj* %y40104, %struct.ScmObj** %stackaddr$env-ref48719
%stackaddr$prim48720 = alloca %struct.ScmObj*, align 8
%k40524 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4010640523)
store volatile %struct.ScmObj* %k40524, %struct.ScmObj** %stackaddr$prim48720, align 8
%stackaddr$prim48721 = alloca %struct.ScmObj*, align 8
%args40106 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4010640523)
store volatile %struct.ScmObj* %args40106, %struct.ScmObj** %stackaddr$prim48721, align 8
%stackaddr$makeclosure48722 = alloca %struct.ScmObj*, align 8
%fptrToInt48723 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40556 to i64
%ae40556 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48723)
store volatile %struct.ScmObj* %ae40556, %struct.ScmObj** %stackaddr$makeclosure48722, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40556, %struct.ScmObj* %k40524, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40556, %struct.ScmObj* %args40106, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40556, %struct.ScmObj* %f40105, i64 2)
%args46965$y40104$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48724 = alloca %struct.ScmObj*, align 8
%args46965$y40104$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y40104, %struct.ScmObj* %args46965$y40104$0)
store volatile %struct.ScmObj* %args46965$y40104$1, %struct.ScmObj** %stackaddr$prim48724, align 8
%stackaddr$prim48725 = alloca %struct.ScmObj*, align 8
%args46965$y40104$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40556, %struct.ScmObj* %args46965$y40104$1)
store volatile %struct.ScmObj* %args46965$y40104$2, %struct.ScmObj** %stackaddr$prim48725, align 8
%clofunc48726 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y40104)
musttail call tailcc void %clofunc48726(%struct.ScmObj* %y40104, %struct.ScmObj* %args46965$y40104$2)
ret void
}

define tailcc void @proc_clo$ae40556(%struct.ScmObj* %env$ae40556,%struct.ScmObj* %current_45args46958) {
%stackaddr$env-ref48727 = alloca %struct.ScmObj*, align 8
%k40524 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40556, i64 0)
store %struct.ScmObj* %k40524, %struct.ScmObj** %stackaddr$env-ref48727
%stackaddr$env-ref48728 = alloca %struct.ScmObj*, align 8
%args40106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40556, i64 1)
store %struct.ScmObj* %args40106, %struct.ScmObj** %stackaddr$env-ref48728
%stackaddr$env-ref48729 = alloca %struct.ScmObj*, align 8
%f40105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40556, i64 2)
store %struct.ScmObj* %f40105, %struct.ScmObj** %stackaddr$env-ref48729
%stackaddr$prim48730 = alloca %struct.ScmObj*, align 8
%_95k40525 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46958)
store volatile %struct.ScmObj* %_95k40525, %struct.ScmObj** %stackaddr$prim48730, align 8
%stackaddr$prim48731 = alloca %struct.ScmObj*, align 8
%current_45args46959 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46958)
store volatile %struct.ScmObj* %current_45args46959, %struct.ScmObj** %stackaddr$prim48731, align 8
%stackaddr$prim48732 = alloca %struct.ScmObj*, align 8
%anf_45bind40228 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46959)
store volatile %struct.ScmObj* %anf_45bind40228, %struct.ScmObj** %stackaddr$prim48732, align 8
%stackaddr$makeclosure48733 = alloca %struct.ScmObj*, align 8
%fptrToInt48734 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40559 to i64
%ae40559 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48734)
store volatile %struct.ScmObj* %ae40559, %struct.ScmObj** %stackaddr$makeclosure48733, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40559, %struct.ScmObj* %k40524, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40559, %struct.ScmObj* %args40106, i64 1)
%args46964$anf_45bind40228$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48735 = alloca %struct.ScmObj*, align 8
%args46964$anf_45bind40228$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40105, %struct.ScmObj* %args46964$anf_45bind40228$0)
store volatile %struct.ScmObj* %args46964$anf_45bind40228$1, %struct.ScmObj** %stackaddr$prim48735, align 8
%stackaddr$prim48736 = alloca %struct.ScmObj*, align 8
%args46964$anf_45bind40228$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40559, %struct.ScmObj* %args46964$anf_45bind40228$1)
store volatile %struct.ScmObj* %args46964$anf_45bind40228$2, %struct.ScmObj** %stackaddr$prim48736, align 8
%clofunc48737 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40228)
musttail call tailcc void %clofunc48737(%struct.ScmObj* %anf_45bind40228, %struct.ScmObj* %args46964$anf_45bind40228$2)
ret void
}

define tailcc void @proc_clo$ae40559(%struct.ScmObj* %env$ae40559,%struct.ScmObj* %current_45args46961) {
%stackaddr$env-ref48738 = alloca %struct.ScmObj*, align 8
%k40524 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40559, i64 0)
store %struct.ScmObj* %k40524, %struct.ScmObj** %stackaddr$env-ref48738
%stackaddr$env-ref48739 = alloca %struct.ScmObj*, align 8
%args40106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40559, i64 1)
store %struct.ScmObj* %args40106, %struct.ScmObj** %stackaddr$env-ref48739
%stackaddr$prim48740 = alloca %struct.ScmObj*, align 8
%_95k40526 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46961)
store volatile %struct.ScmObj* %_95k40526, %struct.ScmObj** %stackaddr$prim48740, align 8
%stackaddr$prim48741 = alloca %struct.ScmObj*, align 8
%current_45args46962 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46961)
store volatile %struct.ScmObj* %current_45args46962, %struct.ScmObj** %stackaddr$prim48741, align 8
%stackaddr$prim48742 = alloca %struct.ScmObj*, align 8
%anf_45bind40229 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46962)
store volatile %struct.ScmObj* %anf_45bind40229, %struct.ScmObj** %stackaddr$prim48742, align 8
%stackaddr$prim48743 = alloca %struct.ScmObj*, align 8
%cpsargs40527 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40524, %struct.ScmObj* %args40106)
store volatile %struct.ScmObj* %cpsargs40527, %struct.ScmObj** %stackaddr$prim48743, align 8
%clofunc48744 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40229)
musttail call tailcc void %clofunc48744(%struct.ScmObj* %anf_45bind40229, %struct.ScmObj* %cpsargs40527)
ret void
}

define tailcc void @proc_clo$ae40531(%struct.ScmObj* %env$ae40531,%struct.ScmObj* %current_45args46969) {
%stackaddr$prim48745 = alloca %struct.ScmObj*, align 8
%k40528 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46969)
store volatile %struct.ScmObj* %k40528, %struct.ScmObj** %stackaddr$prim48745, align 8
%stackaddr$prim48746 = alloca %struct.ScmObj*, align 8
%current_45args46970 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46969)
store volatile %struct.ScmObj* %current_45args46970, %struct.ScmObj** %stackaddr$prim48746, align 8
%stackaddr$prim48747 = alloca %struct.ScmObj*, align 8
%yu40103 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46970)
store volatile %struct.ScmObj* %yu40103, %struct.ScmObj** %stackaddr$prim48747, align 8
%args46972$yu40103$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48748 = alloca %struct.ScmObj*, align 8
%args46972$yu40103$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu40103, %struct.ScmObj* %args46972$yu40103$0)
store volatile %struct.ScmObj* %args46972$yu40103$1, %struct.ScmObj** %stackaddr$prim48748, align 8
%stackaddr$prim48749 = alloca %struct.ScmObj*, align 8
%args46972$yu40103$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40528, %struct.ScmObj* %args46972$yu40103$1)
store volatile %struct.ScmObj* %args46972$yu40103$2, %struct.ScmObj** %stackaddr$prim48749, align 8
%clofunc48750 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu40103)
musttail call tailcc void %clofunc48750(%struct.ScmObj* %yu40103, %struct.ScmObj* %args46972$yu40103$2)
ret void
}