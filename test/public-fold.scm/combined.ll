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
%mainenv54683 = call %struct.ScmObj* @const_init_null()
%mainargs54684 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv54683, %struct.ScmObj* %mainargs54684)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv54681,%struct.ScmObj* %mainargs54682) {
%stackaddr$makeclosure54685 = alloca %struct.ScmObj*, align 8
%fptrToInt54686 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48443 to i64
%ae48443 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54686)
store volatile %struct.ScmObj* %ae48443, %struct.ScmObj** %stackaddr$makeclosure54685, align 8
%ae48444 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54687 = alloca %struct.ScmObj*, align 8
%fptrToInt54688 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48445 to i64
%ae48445 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54688)
store volatile %struct.ScmObj* %ae48445, %struct.ScmObj** %stackaddr$makeclosure54687, align 8
%argslist54680$ae484430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54689 = alloca %struct.ScmObj*, align 8
%argslist54680$ae484431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48445, %struct.ScmObj* %argslist54680$ae484430)
store volatile %struct.ScmObj* %argslist54680$ae484431, %struct.ScmObj** %stackaddr$prim54689, align 8
%stackaddr$prim54690 = alloca %struct.ScmObj*, align 8
%argslist54680$ae484432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48444, %struct.ScmObj* %argslist54680$ae484431)
store volatile %struct.ScmObj* %argslist54680$ae484432, %struct.ScmObj** %stackaddr$prim54690, align 8
%clofunc54691 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48443)
musttail call tailcc void %clofunc54691(%struct.ScmObj* %ae48443, %struct.ScmObj* %argslist54680$ae484432)
ret void
}

define tailcc void @proc_clo$ae48443(%struct.ScmObj* %env$ae48443,%struct.ScmObj* %current_45args54118) {
%stackaddr$prim54692 = alloca %struct.ScmObj*, align 8
%_95k48267 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54118)
store volatile %struct.ScmObj* %_95k48267, %struct.ScmObj** %stackaddr$prim54692, align 8
%stackaddr$prim54693 = alloca %struct.ScmObj*, align 8
%current_45args54119 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54118)
store volatile %struct.ScmObj* %current_45args54119, %struct.ScmObj** %stackaddr$prim54693, align 8
%stackaddr$prim54694 = alloca %struct.ScmObj*, align 8
%anf_45bind48148 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54119)
store volatile %struct.ScmObj* %anf_45bind48148, %struct.ScmObj** %stackaddr$prim54694, align 8
%stackaddr$makeclosure54695 = alloca %struct.ScmObj*, align 8
%fptrToInt54696 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48458 to i64
%ae48458 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54696)
store volatile %struct.ScmObj* %ae48458, %struct.ScmObj** %stackaddr$makeclosure54695, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48458, %struct.ScmObj* %anf_45bind48148, i64 0)
%ae48459 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54697 = alloca %struct.ScmObj*, align 8
%fptrToInt54698 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48460 to i64
%ae48460 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54698)
store volatile %struct.ScmObj* %ae48460, %struct.ScmObj** %stackaddr$makeclosure54697, align 8
%argslist54675$ae484580 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54699 = alloca %struct.ScmObj*, align 8
%argslist54675$ae484581 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48460, %struct.ScmObj* %argslist54675$ae484580)
store volatile %struct.ScmObj* %argslist54675$ae484581, %struct.ScmObj** %stackaddr$prim54699, align 8
%stackaddr$prim54700 = alloca %struct.ScmObj*, align 8
%argslist54675$ae484582 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48459, %struct.ScmObj* %argslist54675$ae484581)
store volatile %struct.ScmObj* %argslist54675$ae484582, %struct.ScmObj** %stackaddr$prim54700, align 8
%clofunc54701 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48458)
musttail call tailcc void %clofunc54701(%struct.ScmObj* %ae48458, %struct.ScmObj* %argslist54675$ae484582)
ret void
}

define tailcc void @proc_clo$ae48458(%struct.ScmObj* %env$ae48458,%struct.ScmObj* %current_45args54121) {
%stackaddr$env-ref54702 = alloca %struct.ScmObj*, align 8
%anf_45bind48148 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48458, i64 0)
store %struct.ScmObj* %anf_45bind48148, %struct.ScmObj** %stackaddr$env-ref54702
%stackaddr$prim54703 = alloca %struct.ScmObj*, align 8
%_95k48268 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54121)
store volatile %struct.ScmObj* %_95k48268, %struct.ScmObj** %stackaddr$prim54703, align 8
%stackaddr$prim54704 = alloca %struct.ScmObj*, align 8
%current_45args54122 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54121)
store volatile %struct.ScmObj* %current_45args54122, %struct.ScmObj** %stackaddr$prim54704, align 8
%stackaddr$prim54705 = alloca %struct.ScmObj*, align 8
%anf_45bind48152 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54122)
store volatile %struct.ScmObj* %anf_45bind48152, %struct.ScmObj** %stackaddr$prim54705, align 8
%stackaddr$makeclosure54706 = alloca %struct.ScmObj*, align 8
%fptrToInt54707 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48573 to i64
%ae48573 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54707)
store volatile %struct.ScmObj* %ae48573, %struct.ScmObj** %stackaddr$makeclosure54706, align 8
%argslist54654$anf_45bind481480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54708 = alloca %struct.ScmObj*, align 8
%argslist54654$anf_45bind481481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48152, %struct.ScmObj* %argslist54654$anf_45bind481480)
store volatile %struct.ScmObj* %argslist54654$anf_45bind481481, %struct.ScmObj** %stackaddr$prim54708, align 8
%stackaddr$prim54709 = alloca %struct.ScmObj*, align 8
%argslist54654$anf_45bind481482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48573, %struct.ScmObj* %argslist54654$anf_45bind481481)
store volatile %struct.ScmObj* %argslist54654$anf_45bind481482, %struct.ScmObj** %stackaddr$prim54709, align 8
%clofunc54710 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48148)
musttail call tailcc void %clofunc54710(%struct.ScmObj* %anf_45bind48148, %struct.ScmObj* %argslist54654$anf_45bind481482)
ret void
}

define tailcc void @proc_clo$ae48573(%struct.ScmObj* %env$ae48573,%struct.ScmObj* %current_45args54124) {
%stackaddr$prim54711 = alloca %struct.ScmObj*, align 8
%_95k48269 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54124)
store volatile %struct.ScmObj* %_95k48269, %struct.ScmObj** %stackaddr$prim54711, align 8
%stackaddr$prim54712 = alloca %struct.ScmObj*, align 8
%current_45args54125 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54124)
store volatile %struct.ScmObj* %current_45args54125, %struct.ScmObj** %stackaddr$prim54712, align 8
%stackaddr$prim54713 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54125)
store volatile %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$prim54713, align 8
%stackaddr$makeclosure54714 = alloca %struct.ScmObj*, align 8
%fptrToInt54715 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48575 to i64
%ae48575 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54715)
store volatile %struct.ScmObj* %ae48575, %struct.ScmObj** %stackaddr$makeclosure54714, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48575, %struct.ScmObj* %Ycmb48025, i64 0)
%ae48576 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54716 = alloca %struct.ScmObj*, align 8
%fptrToInt54717 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48577 to i64
%ae48577 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54717)
store volatile %struct.ScmObj* %ae48577, %struct.ScmObj** %stackaddr$makeclosure54716, align 8
%argslist54653$ae485750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54718 = alloca %struct.ScmObj*, align 8
%argslist54653$ae485751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48577, %struct.ScmObj* %argslist54653$ae485750)
store volatile %struct.ScmObj* %argslist54653$ae485751, %struct.ScmObj** %stackaddr$prim54718, align 8
%stackaddr$prim54719 = alloca %struct.ScmObj*, align 8
%argslist54653$ae485752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48576, %struct.ScmObj* %argslist54653$ae485751)
store volatile %struct.ScmObj* %argslist54653$ae485752, %struct.ScmObj** %stackaddr$prim54719, align 8
%clofunc54720 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48575)
musttail call tailcc void %clofunc54720(%struct.ScmObj* %ae48575, %struct.ScmObj* %argslist54653$ae485752)
ret void
}

define tailcc void @proc_clo$ae48575(%struct.ScmObj* %env$ae48575,%struct.ScmObj* %current_45args54127) {
%stackaddr$env-ref54721 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48575, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54721
%stackaddr$prim54722 = alloca %struct.ScmObj*, align 8
%_95k48270 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54127)
store volatile %struct.ScmObj* %_95k48270, %struct.ScmObj** %stackaddr$prim54722, align 8
%stackaddr$prim54723 = alloca %struct.ScmObj*, align 8
%current_45args54128 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54127)
store volatile %struct.ScmObj* %current_45args54128, %struct.ScmObj** %stackaddr$prim54723, align 8
%stackaddr$prim54724 = alloca %struct.ScmObj*, align 8
%anf_45bind48157 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54128)
store volatile %struct.ScmObj* %anf_45bind48157, %struct.ScmObj** %stackaddr$prim54724, align 8
%stackaddr$makeclosure54725 = alloca %struct.ScmObj*, align 8
%fptrToInt54726 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48653 to i64
%ae48653 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54726)
store volatile %struct.ScmObj* %ae48653, %struct.ScmObj** %stackaddr$makeclosure54725, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48653, %struct.ScmObj* %Ycmb48025, i64 0)
%argslist54637$Ycmb480250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54727 = alloca %struct.ScmObj*, align 8
%argslist54637$Ycmb480251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48157, %struct.ScmObj* %argslist54637$Ycmb480250)
store volatile %struct.ScmObj* %argslist54637$Ycmb480251, %struct.ScmObj** %stackaddr$prim54727, align 8
%stackaddr$prim54728 = alloca %struct.ScmObj*, align 8
%argslist54637$Ycmb480252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48653, %struct.ScmObj* %argslist54637$Ycmb480251)
store volatile %struct.ScmObj* %argslist54637$Ycmb480252, %struct.ScmObj** %stackaddr$prim54728, align 8
%clofunc54729 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48025)
musttail call tailcc void %clofunc54729(%struct.ScmObj* %Ycmb48025, %struct.ScmObj* %argslist54637$Ycmb480252)
ret void
}

define tailcc void @proc_clo$ae48653(%struct.ScmObj* %env$ae48653,%struct.ScmObj* %current_45args54130) {
%stackaddr$env-ref54730 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48653, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54730
%stackaddr$prim54731 = alloca %struct.ScmObj*, align 8
%_95k48271 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54130)
store volatile %struct.ScmObj* %_95k48271, %struct.ScmObj** %stackaddr$prim54731, align 8
%stackaddr$prim54732 = alloca %struct.ScmObj*, align 8
%current_45args54131 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54130)
store volatile %struct.ScmObj* %current_45args54131, %struct.ScmObj** %stackaddr$prim54732, align 8
%stackaddr$prim54733 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54131)
store volatile %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$prim54733, align 8
%stackaddr$makeclosure54734 = alloca %struct.ScmObj*, align 8
%fptrToInt54735 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48655 to i64
%ae48655 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54735)
store volatile %struct.ScmObj* %ae48655, %struct.ScmObj** %stackaddr$makeclosure54734, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48655, %struct.ScmObj* %_37foldr148046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48655, %struct.ScmObj* %Ycmb48025, i64 1)
%ae48656 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54736 = alloca %struct.ScmObj*, align 8
%fptrToInt54737 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48657 to i64
%ae48657 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54737)
store volatile %struct.ScmObj* %ae48657, %struct.ScmObj** %stackaddr$makeclosure54736, align 8
%argslist54636$ae486550 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54738 = alloca %struct.ScmObj*, align 8
%argslist54636$ae486551 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48657, %struct.ScmObj* %argslist54636$ae486550)
store volatile %struct.ScmObj* %argslist54636$ae486551, %struct.ScmObj** %stackaddr$prim54738, align 8
%stackaddr$prim54739 = alloca %struct.ScmObj*, align 8
%argslist54636$ae486552 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48656, %struct.ScmObj* %argslist54636$ae486551)
store volatile %struct.ScmObj* %argslist54636$ae486552, %struct.ScmObj** %stackaddr$prim54739, align 8
%clofunc54740 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48655)
musttail call tailcc void %clofunc54740(%struct.ScmObj* %ae48655, %struct.ScmObj* %argslist54636$ae486552)
ret void
}

define tailcc void @proc_clo$ae48655(%struct.ScmObj* %env$ae48655,%struct.ScmObj* %current_45args54133) {
%stackaddr$env-ref54741 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48655, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref54741
%stackaddr$env-ref54742 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48655, i64 1)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54742
%stackaddr$prim54743 = alloca %struct.ScmObj*, align 8
%_95k48272 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54133)
store volatile %struct.ScmObj* %_95k48272, %struct.ScmObj** %stackaddr$prim54743, align 8
%stackaddr$prim54744 = alloca %struct.ScmObj*, align 8
%current_45args54134 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54133)
store volatile %struct.ScmObj* %current_45args54134, %struct.ScmObj** %stackaddr$prim54744, align 8
%stackaddr$prim54745 = alloca %struct.ScmObj*, align 8
%anf_45bind48163 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54134)
store volatile %struct.ScmObj* %anf_45bind48163, %struct.ScmObj** %stackaddr$prim54745, align 8
%stackaddr$makeclosure54746 = alloca %struct.ScmObj*, align 8
%fptrToInt54747 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48750 to i64
%ae48750 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54747)
store volatile %struct.ScmObj* %ae48750, %struct.ScmObj** %stackaddr$makeclosure54746, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48750, %struct.ScmObj* %_37foldr148046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48750, %struct.ScmObj* %Ycmb48025, i64 1)
%argslist54617$Ycmb480250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54748 = alloca %struct.ScmObj*, align 8
%argslist54617$Ycmb480251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48163, %struct.ScmObj* %argslist54617$Ycmb480250)
store volatile %struct.ScmObj* %argslist54617$Ycmb480251, %struct.ScmObj** %stackaddr$prim54748, align 8
%stackaddr$prim54749 = alloca %struct.ScmObj*, align 8
%argslist54617$Ycmb480252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48750, %struct.ScmObj* %argslist54617$Ycmb480251)
store volatile %struct.ScmObj* %argslist54617$Ycmb480252, %struct.ScmObj** %stackaddr$prim54749, align 8
%clofunc54750 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48025)
musttail call tailcc void %clofunc54750(%struct.ScmObj* %Ycmb48025, %struct.ScmObj* %argslist54617$Ycmb480252)
ret void
}

define tailcc void @proc_clo$ae48750(%struct.ScmObj* %env$ae48750,%struct.ScmObj* %current_45args54136) {
%stackaddr$env-ref54751 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48750, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref54751
%stackaddr$env-ref54752 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48750, i64 1)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54752
%stackaddr$prim54753 = alloca %struct.ScmObj*, align 8
%_95k48273 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54136)
store volatile %struct.ScmObj* %_95k48273, %struct.ScmObj** %stackaddr$prim54753, align 8
%stackaddr$prim54754 = alloca %struct.ScmObj*, align 8
%current_45args54137 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54136)
store volatile %struct.ScmObj* %current_45args54137, %struct.ScmObj** %stackaddr$prim54754, align 8
%stackaddr$prim54755 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54137)
store volatile %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$prim54755, align 8
%stackaddr$makeclosure54756 = alloca %struct.ScmObj*, align 8
%fptrToInt54757 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48752 to i64
%ae48752 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54757)
store volatile %struct.ScmObj* %ae48752, %struct.ScmObj** %stackaddr$makeclosure54756, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48752, %struct.ScmObj* %_37foldr148046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48752, %struct.ScmObj* %_37map148042, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48752, %struct.ScmObj* %Ycmb48025, i64 2)
%ae48753 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54758 = alloca %struct.ScmObj*, align 8
%fptrToInt54759 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48754 to i64
%ae48754 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54759)
store volatile %struct.ScmObj* %ae48754, %struct.ScmObj** %stackaddr$makeclosure54758, align 8
%argslist54616$ae487520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54760 = alloca %struct.ScmObj*, align 8
%argslist54616$ae487521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48754, %struct.ScmObj* %argslist54616$ae487520)
store volatile %struct.ScmObj* %argslist54616$ae487521, %struct.ScmObj** %stackaddr$prim54760, align 8
%stackaddr$prim54761 = alloca %struct.ScmObj*, align 8
%argslist54616$ae487522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48753, %struct.ScmObj* %argslist54616$ae487521)
store volatile %struct.ScmObj* %argslist54616$ae487522, %struct.ScmObj** %stackaddr$prim54761, align 8
%clofunc54762 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48752)
musttail call tailcc void %clofunc54762(%struct.ScmObj* %ae48752, %struct.ScmObj* %argslist54616$ae487522)
ret void
}

define tailcc void @proc_clo$ae48752(%struct.ScmObj* %env$ae48752,%struct.ScmObj* %current_45args54139) {
%stackaddr$env-ref54763 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48752, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref54763
%stackaddr$env-ref54764 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48752, i64 1)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref54764
%stackaddr$env-ref54765 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48752, i64 2)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54765
%stackaddr$prim54766 = alloca %struct.ScmObj*, align 8
%_95k48274 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54139)
store volatile %struct.ScmObj* %_95k48274, %struct.ScmObj** %stackaddr$prim54766, align 8
%stackaddr$prim54767 = alloca %struct.ScmObj*, align 8
%current_45args54140 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54139)
store volatile %struct.ScmObj* %current_45args54140, %struct.ScmObj** %stackaddr$prim54767, align 8
%stackaddr$prim54768 = alloca %struct.ScmObj*, align 8
%anf_45bind48170 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54140)
store volatile %struct.ScmObj* %anf_45bind48170, %struct.ScmObj** %stackaddr$prim54768, align 8
%stackaddr$makeclosure54769 = alloca %struct.ScmObj*, align 8
%fptrToInt54770 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48900 to i64
%ae48900 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54770)
store volatile %struct.ScmObj* %ae48900, %struct.ScmObj** %stackaddr$makeclosure54769, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48900, %struct.ScmObj* %_37foldr148046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48900, %struct.ScmObj* %_37map148042, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48900, %struct.ScmObj* %Ycmb48025, i64 2)
%argslist54600$Ycmb480250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54771 = alloca %struct.ScmObj*, align 8
%argslist54600$Ycmb480251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48170, %struct.ScmObj* %argslist54600$Ycmb480250)
store volatile %struct.ScmObj* %argslist54600$Ycmb480251, %struct.ScmObj** %stackaddr$prim54771, align 8
%stackaddr$prim54772 = alloca %struct.ScmObj*, align 8
%argslist54600$Ycmb480252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48900, %struct.ScmObj* %argslist54600$Ycmb480251)
store volatile %struct.ScmObj* %argslist54600$Ycmb480252, %struct.ScmObj** %stackaddr$prim54772, align 8
%clofunc54773 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48025)
musttail call tailcc void %clofunc54773(%struct.ScmObj* %Ycmb48025, %struct.ScmObj* %argslist54600$Ycmb480252)
ret void
}

define tailcc void @proc_clo$ae48900(%struct.ScmObj* %env$ae48900,%struct.ScmObj* %current_45args54142) {
%stackaddr$env-ref54774 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48900, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref54774
%stackaddr$env-ref54775 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48900, i64 1)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref54775
%stackaddr$env-ref54776 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48900, i64 2)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54776
%stackaddr$prim54777 = alloca %struct.ScmObj*, align 8
%_95k48275 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54142)
store volatile %struct.ScmObj* %_95k48275, %struct.ScmObj** %stackaddr$prim54777, align 8
%stackaddr$prim54778 = alloca %struct.ScmObj*, align 8
%current_45args54143 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54142)
store volatile %struct.ScmObj* %current_45args54143, %struct.ScmObj** %stackaddr$prim54778, align 8
%stackaddr$prim54779 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54143)
store volatile %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$prim54779, align 8
%stackaddr$makeclosure54780 = alloca %struct.ScmObj*, align 8
%fptrToInt54781 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48902 to i64
%ae48902 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54781)
store volatile %struct.ScmObj* %ae48902, %struct.ScmObj** %stackaddr$makeclosure54780, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48902, %struct.ScmObj* %_37take48038, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48902, %struct.ScmObj* %_37foldr148046, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48902, %struct.ScmObj* %_37map148042, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48902, %struct.ScmObj* %Ycmb48025, i64 3)
%ae48903 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54782 = alloca %struct.ScmObj*, align 8
%fptrToInt54783 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48904 to i64
%ae48904 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54783)
store volatile %struct.ScmObj* %ae48904, %struct.ScmObj** %stackaddr$makeclosure54782, align 8
%argslist54599$ae489020 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54784 = alloca %struct.ScmObj*, align 8
%argslist54599$ae489021 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48904, %struct.ScmObj* %argslist54599$ae489020)
store volatile %struct.ScmObj* %argslist54599$ae489021, %struct.ScmObj** %stackaddr$prim54784, align 8
%stackaddr$prim54785 = alloca %struct.ScmObj*, align 8
%argslist54599$ae489022 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48903, %struct.ScmObj* %argslist54599$ae489021)
store volatile %struct.ScmObj* %argslist54599$ae489022, %struct.ScmObj** %stackaddr$prim54785, align 8
%clofunc54786 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48902)
musttail call tailcc void %clofunc54786(%struct.ScmObj* %ae48902, %struct.ScmObj* %argslist54599$ae489022)
ret void
}

define tailcc void @proc_clo$ae48902(%struct.ScmObj* %env$ae48902,%struct.ScmObj* %current_45args54145) {
%stackaddr$env-ref54787 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48902, i64 0)
store %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$env-ref54787
%stackaddr$env-ref54788 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48902, i64 1)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref54788
%stackaddr$env-ref54789 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48902, i64 2)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref54789
%stackaddr$env-ref54790 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48902, i64 3)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54790
%stackaddr$prim54791 = alloca %struct.ScmObj*, align 8
%_95k48276 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54145)
store volatile %struct.ScmObj* %_95k48276, %struct.ScmObj** %stackaddr$prim54791, align 8
%stackaddr$prim54792 = alloca %struct.ScmObj*, align 8
%current_45args54146 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54145)
store volatile %struct.ScmObj* %current_45args54146, %struct.ScmObj** %stackaddr$prim54792, align 8
%stackaddr$prim54793 = alloca %struct.ScmObj*, align 8
%anf_45bind48174 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54146)
store volatile %struct.ScmObj* %anf_45bind48174, %struct.ScmObj** %stackaddr$prim54793, align 8
%stackaddr$makeclosure54794 = alloca %struct.ScmObj*, align 8
%fptrToInt54795 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48983 to i64
%ae48983 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54795)
store volatile %struct.ScmObj* %ae48983, %struct.ScmObj** %stackaddr$makeclosure54794, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48983, %struct.ScmObj* %_37take48038, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48983, %struct.ScmObj* %_37foldr148046, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48983, %struct.ScmObj* %_37map148042, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48983, %struct.ScmObj* %Ycmb48025, i64 3)
%argslist54585$Ycmb480250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54796 = alloca %struct.ScmObj*, align 8
%argslist54585$Ycmb480251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48174, %struct.ScmObj* %argslist54585$Ycmb480250)
store volatile %struct.ScmObj* %argslist54585$Ycmb480251, %struct.ScmObj** %stackaddr$prim54796, align 8
%stackaddr$prim54797 = alloca %struct.ScmObj*, align 8
%argslist54585$Ycmb480252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48983, %struct.ScmObj* %argslist54585$Ycmb480251)
store volatile %struct.ScmObj* %argslist54585$Ycmb480252, %struct.ScmObj** %stackaddr$prim54797, align 8
%clofunc54798 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48025)
musttail call tailcc void %clofunc54798(%struct.ScmObj* %Ycmb48025, %struct.ScmObj* %argslist54585$Ycmb480252)
ret void
}

define tailcc void @proc_clo$ae48983(%struct.ScmObj* %env$ae48983,%struct.ScmObj* %current_45args54148) {
%stackaddr$env-ref54799 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48983, i64 0)
store %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$env-ref54799
%stackaddr$env-ref54800 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48983, i64 1)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref54800
%stackaddr$env-ref54801 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48983, i64 2)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref54801
%stackaddr$env-ref54802 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48983, i64 3)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54802
%stackaddr$prim54803 = alloca %struct.ScmObj*, align 8
%_95k48277 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54148)
store volatile %struct.ScmObj* %_95k48277, %struct.ScmObj** %stackaddr$prim54803, align 8
%stackaddr$prim54804 = alloca %struct.ScmObj*, align 8
%current_45args54149 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54148)
store volatile %struct.ScmObj* %current_45args54149, %struct.ScmObj** %stackaddr$prim54804, align 8
%stackaddr$prim54805 = alloca %struct.ScmObj*, align 8
%_37length48035 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54149)
store volatile %struct.ScmObj* %_37length48035, %struct.ScmObj** %stackaddr$prim54805, align 8
%stackaddr$makeclosure54806 = alloca %struct.ScmObj*, align 8
%fptrToInt54807 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48985 to i64
%ae48985 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54807)
store volatile %struct.ScmObj* %ae48985, %struct.ScmObj** %stackaddr$makeclosure54806, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48985, %struct.ScmObj* %_37take48038, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48985, %struct.ScmObj* %_37length48035, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48985, %struct.ScmObj* %_37foldr148046, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48985, %struct.ScmObj* %_37map148042, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48985, %struct.ScmObj* %Ycmb48025, i64 4)
%ae48986 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54808 = alloca %struct.ScmObj*, align 8
%fptrToInt54809 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48987 to i64
%ae48987 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54809)
store volatile %struct.ScmObj* %ae48987, %struct.ScmObj** %stackaddr$makeclosure54808, align 8
%argslist54584$ae489850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54810 = alloca %struct.ScmObj*, align 8
%argslist54584$ae489851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48987, %struct.ScmObj* %argslist54584$ae489850)
store volatile %struct.ScmObj* %argslist54584$ae489851, %struct.ScmObj** %stackaddr$prim54810, align 8
%stackaddr$prim54811 = alloca %struct.ScmObj*, align 8
%argslist54584$ae489852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48986, %struct.ScmObj* %argslist54584$ae489851)
store volatile %struct.ScmObj* %argslist54584$ae489852, %struct.ScmObj** %stackaddr$prim54811, align 8
%clofunc54812 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48985)
musttail call tailcc void %clofunc54812(%struct.ScmObj* %ae48985, %struct.ScmObj* %argslist54584$ae489852)
ret void
}

define tailcc void @proc_clo$ae48985(%struct.ScmObj* %env$ae48985,%struct.ScmObj* %current_45args54151) {
%stackaddr$env-ref54813 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48985, i64 0)
store %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$env-ref54813
%stackaddr$env-ref54814 = alloca %struct.ScmObj*, align 8
%_37length48035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48985, i64 1)
store %struct.ScmObj* %_37length48035, %struct.ScmObj** %stackaddr$env-ref54814
%stackaddr$env-ref54815 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48985, i64 2)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref54815
%stackaddr$env-ref54816 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48985, i64 3)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref54816
%stackaddr$env-ref54817 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48985, i64 4)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54817
%stackaddr$prim54818 = alloca %struct.ScmObj*, align 8
%_95k48278 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54151)
store volatile %struct.ScmObj* %_95k48278, %struct.ScmObj** %stackaddr$prim54818, align 8
%stackaddr$prim54819 = alloca %struct.ScmObj*, align 8
%current_45args54152 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54151)
store volatile %struct.ScmObj* %current_45args54152, %struct.ScmObj** %stackaddr$prim54819, align 8
%stackaddr$prim54820 = alloca %struct.ScmObj*, align 8
%anf_45bind48179 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54152)
store volatile %struct.ScmObj* %anf_45bind48179, %struct.ScmObj** %stackaddr$prim54820, align 8
%stackaddr$makeclosure54821 = alloca %struct.ScmObj*, align 8
%fptrToInt54822 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49062 to i64
%ae49062 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54822)
store volatile %struct.ScmObj* %ae49062, %struct.ScmObj** %stackaddr$makeclosure54821, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49062, %struct.ScmObj* %_37take48038, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49062, %struct.ScmObj* %_37length48035, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49062, %struct.ScmObj* %_37foldr148046, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49062, %struct.ScmObj* %_37map148042, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49062, %struct.ScmObj* %Ycmb48025, i64 4)
%argslist54568$Ycmb480250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54823 = alloca %struct.ScmObj*, align 8
%argslist54568$Ycmb480251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48179, %struct.ScmObj* %argslist54568$Ycmb480250)
store volatile %struct.ScmObj* %argslist54568$Ycmb480251, %struct.ScmObj** %stackaddr$prim54823, align 8
%stackaddr$prim54824 = alloca %struct.ScmObj*, align 8
%argslist54568$Ycmb480252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49062, %struct.ScmObj* %argslist54568$Ycmb480251)
store volatile %struct.ScmObj* %argslist54568$Ycmb480252, %struct.ScmObj** %stackaddr$prim54824, align 8
%clofunc54825 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48025)
musttail call tailcc void %clofunc54825(%struct.ScmObj* %Ycmb48025, %struct.ScmObj* %argslist54568$Ycmb480252)
ret void
}

define tailcc void @proc_clo$ae49062(%struct.ScmObj* %env$ae49062,%struct.ScmObj* %current_45args54154) {
%stackaddr$env-ref54826 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49062, i64 0)
store %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$env-ref54826
%stackaddr$env-ref54827 = alloca %struct.ScmObj*, align 8
%_37length48035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49062, i64 1)
store %struct.ScmObj* %_37length48035, %struct.ScmObj** %stackaddr$env-ref54827
%stackaddr$env-ref54828 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49062, i64 2)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref54828
%stackaddr$env-ref54829 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49062, i64 3)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref54829
%stackaddr$env-ref54830 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49062, i64 4)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54830
%stackaddr$prim54831 = alloca %struct.ScmObj*, align 8
%_95k48279 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54154)
store volatile %struct.ScmObj* %_95k48279, %struct.ScmObj** %stackaddr$prim54831, align 8
%stackaddr$prim54832 = alloca %struct.ScmObj*, align 8
%current_45args54155 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54154)
store volatile %struct.ScmObj* %current_45args54155, %struct.ScmObj** %stackaddr$prim54832, align 8
%stackaddr$prim54833 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54155)
store volatile %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$prim54833, align 8
%stackaddr$makeclosure54834 = alloca %struct.ScmObj*, align 8
%fptrToInt54835 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49064 to i64
%ae49064 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt54835)
store volatile %struct.ScmObj* %ae49064, %struct.ScmObj** %stackaddr$makeclosure54834, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49064, %struct.ScmObj* %_37foldr148046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49064, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49064, %struct.ScmObj* %_37take48038, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49064, %struct.ScmObj* %_37length48035, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49064, %struct.ScmObj* %_37map148042, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49064, %struct.ScmObj* %Ycmb48025, i64 5)
%ae49065 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54836 = alloca %struct.ScmObj*, align 8
%fptrToInt54837 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49066 to i64
%ae49066 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54837)
store volatile %struct.ScmObj* %ae49066, %struct.ScmObj** %stackaddr$makeclosure54836, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49066, %struct.ScmObj* %_37foldl148030, i64 0)
%argslist54567$ae490640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54838 = alloca %struct.ScmObj*, align 8
%argslist54567$ae490641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49066, %struct.ScmObj* %argslist54567$ae490640)
store volatile %struct.ScmObj* %argslist54567$ae490641, %struct.ScmObj** %stackaddr$prim54838, align 8
%stackaddr$prim54839 = alloca %struct.ScmObj*, align 8
%argslist54567$ae490642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49065, %struct.ScmObj* %argslist54567$ae490641)
store volatile %struct.ScmObj* %argslist54567$ae490642, %struct.ScmObj** %stackaddr$prim54839, align 8
%clofunc54840 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49064)
musttail call tailcc void %clofunc54840(%struct.ScmObj* %ae49064, %struct.ScmObj* %argslist54567$ae490642)
ret void
}

define tailcc void @proc_clo$ae49064(%struct.ScmObj* %env$ae49064,%struct.ScmObj* %current_45args54157) {
%stackaddr$env-ref54841 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49064, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref54841
%stackaddr$env-ref54842 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49064, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref54842
%stackaddr$env-ref54843 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49064, i64 2)
store %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$env-ref54843
%stackaddr$env-ref54844 = alloca %struct.ScmObj*, align 8
%_37length48035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49064, i64 3)
store %struct.ScmObj* %_37length48035, %struct.ScmObj** %stackaddr$env-ref54844
%stackaddr$env-ref54845 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49064, i64 4)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref54845
%stackaddr$env-ref54846 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49064, i64 5)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54846
%stackaddr$prim54847 = alloca %struct.ScmObj*, align 8
%_95k48280 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54157)
store volatile %struct.ScmObj* %_95k48280, %struct.ScmObj** %stackaddr$prim54847, align 8
%stackaddr$prim54848 = alloca %struct.ScmObj*, align 8
%current_45args54158 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54157)
store volatile %struct.ScmObj* %current_45args54158, %struct.ScmObj** %stackaddr$prim54848, align 8
%stackaddr$prim54849 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54158)
store volatile %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$prim54849, align 8
%stackaddr$makeclosure54850 = alloca %struct.ScmObj*, align 8
%fptrToInt54851 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49118 to i64
%ae49118 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54851)
store volatile %struct.ScmObj* %ae49118, %struct.ScmObj** %stackaddr$makeclosure54850, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49118, %struct.ScmObj* %_37foldr148046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49118, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49118, %struct.ScmObj* %_37last48068, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49118, %struct.ScmObj* %_37map148042, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49118, %struct.ScmObj* %Ycmb48025, i64 4)
%ae49119 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54852 = alloca %struct.ScmObj*, align 8
%fptrToInt54853 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49120 to i64
%ae49120 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54853)
store volatile %struct.ScmObj* %ae49120, %struct.ScmObj** %stackaddr$makeclosure54852, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49120, %struct.ScmObj* %_37take48038, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49120, %struct.ScmObj* %_37length48035, i64 1)
%argslist54553$ae491180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54854 = alloca %struct.ScmObj*, align 8
%argslist54553$ae491181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49120, %struct.ScmObj* %argslist54553$ae491180)
store volatile %struct.ScmObj* %argslist54553$ae491181, %struct.ScmObj** %stackaddr$prim54854, align 8
%stackaddr$prim54855 = alloca %struct.ScmObj*, align 8
%argslist54553$ae491182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49119, %struct.ScmObj* %argslist54553$ae491181)
store volatile %struct.ScmObj* %argslist54553$ae491182, %struct.ScmObj** %stackaddr$prim54855, align 8
%clofunc54856 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49118)
musttail call tailcc void %clofunc54856(%struct.ScmObj* %ae49118, %struct.ScmObj* %argslist54553$ae491182)
ret void
}

define tailcc void @proc_clo$ae49118(%struct.ScmObj* %env$ae49118,%struct.ScmObj* %current_45args54160) {
%stackaddr$env-ref54857 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49118, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref54857
%stackaddr$env-ref54858 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49118, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref54858
%stackaddr$env-ref54859 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49118, i64 2)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref54859
%stackaddr$env-ref54860 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49118, i64 3)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref54860
%stackaddr$env-ref54861 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49118, i64 4)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54861
%stackaddr$prim54862 = alloca %struct.ScmObj*, align 8
%_95k48281 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54160)
store volatile %struct.ScmObj* %_95k48281, %struct.ScmObj** %stackaddr$prim54862, align 8
%stackaddr$prim54863 = alloca %struct.ScmObj*, align 8
%current_45args54161 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54160)
store volatile %struct.ScmObj* %current_45args54161, %struct.ScmObj** %stackaddr$prim54863, align 8
%stackaddr$prim54864 = alloca %struct.ScmObj*, align 8
%_37drop_45right48065 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54161)
store volatile %struct.ScmObj* %_37drop_45right48065, %struct.ScmObj** %stackaddr$prim54864, align 8
%stackaddr$makeclosure54865 = alloca %struct.ScmObj*, align 8
%fptrToInt54866 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49148 to i64
%ae49148 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54866)
store volatile %struct.ScmObj* %ae49148, %struct.ScmObj** %stackaddr$makeclosure54865, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49148, %struct.ScmObj* %_37foldr148046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49148, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49148, %struct.ScmObj* %_37last48068, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49148, %struct.ScmObj* %_37drop_45right48065, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49148, %struct.ScmObj* %Ycmb48025, i64 4)
%ae49149 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54867 = alloca %struct.ScmObj*, align 8
%fptrToInt54868 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49150 to i64
%ae49150 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54868)
store volatile %struct.ScmObj* %ae49150, %struct.ScmObj** %stackaddr$makeclosure54867, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49150, %struct.ScmObj* %_37foldr148046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49150, %struct.ScmObj* %_37map148042, i64 1)
%argslist54543$ae491480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54869 = alloca %struct.ScmObj*, align 8
%argslist54543$ae491481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49150, %struct.ScmObj* %argslist54543$ae491480)
store volatile %struct.ScmObj* %argslist54543$ae491481, %struct.ScmObj** %stackaddr$prim54869, align 8
%stackaddr$prim54870 = alloca %struct.ScmObj*, align 8
%argslist54543$ae491482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49149, %struct.ScmObj* %argslist54543$ae491481)
store volatile %struct.ScmObj* %argslist54543$ae491482, %struct.ScmObj** %stackaddr$prim54870, align 8
%clofunc54871 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49148)
musttail call tailcc void %clofunc54871(%struct.ScmObj* %ae49148, %struct.ScmObj* %argslist54543$ae491482)
ret void
}

define tailcc void @proc_clo$ae49148(%struct.ScmObj* %env$ae49148,%struct.ScmObj* %current_45args54163) {
%stackaddr$env-ref54872 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49148, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref54872
%stackaddr$env-ref54873 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49148, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref54873
%stackaddr$env-ref54874 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49148, i64 2)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref54874
%stackaddr$env-ref54875 = alloca %struct.ScmObj*, align 8
%_37drop_45right48065 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49148, i64 3)
store %struct.ScmObj* %_37drop_45right48065, %struct.ScmObj** %stackaddr$env-ref54875
%stackaddr$env-ref54876 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49148, i64 4)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54876
%stackaddr$prim54877 = alloca %struct.ScmObj*, align 8
%_95k48282 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54163)
store volatile %struct.ScmObj* %_95k48282, %struct.ScmObj** %stackaddr$prim54877, align 8
%stackaddr$prim54878 = alloca %struct.ScmObj*, align 8
%current_45args54164 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54163)
store volatile %struct.ScmObj* %current_45args54164, %struct.ScmObj** %stackaddr$prim54878, align 8
%stackaddr$prim54879 = alloca %struct.ScmObj*, align 8
%anf_45bind48195 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54164)
store volatile %struct.ScmObj* %anf_45bind48195, %struct.ScmObj** %stackaddr$prim54879, align 8
%stackaddr$makeclosure54880 = alloca %struct.ScmObj*, align 8
%fptrToInt54881 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49532 to i64
%ae49532 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54881)
store volatile %struct.ScmObj* %ae49532, %struct.ScmObj** %stackaddr$makeclosure54880, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49532, %struct.ScmObj* %_37foldr148046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49532, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49532, %struct.ScmObj* %_37last48068, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49532, %struct.ScmObj* %_37drop_45right48065, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49532, %struct.ScmObj* %Ycmb48025, i64 4)
%argslist54483$Ycmb480250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54882 = alloca %struct.ScmObj*, align 8
%argslist54483$Ycmb480251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48195, %struct.ScmObj* %argslist54483$Ycmb480250)
store volatile %struct.ScmObj* %argslist54483$Ycmb480251, %struct.ScmObj** %stackaddr$prim54882, align 8
%stackaddr$prim54883 = alloca %struct.ScmObj*, align 8
%argslist54483$Ycmb480252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49532, %struct.ScmObj* %argslist54483$Ycmb480251)
store volatile %struct.ScmObj* %argslist54483$Ycmb480252, %struct.ScmObj** %stackaddr$prim54883, align 8
%clofunc54884 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48025)
musttail call tailcc void %clofunc54884(%struct.ScmObj* %Ycmb48025, %struct.ScmObj* %argslist54483$Ycmb480252)
ret void
}

define tailcc void @proc_clo$ae49532(%struct.ScmObj* %env$ae49532,%struct.ScmObj* %current_45args54166) {
%stackaddr$env-ref54885 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49532, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref54885
%stackaddr$env-ref54886 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49532, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref54886
%stackaddr$env-ref54887 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49532, i64 2)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref54887
%stackaddr$env-ref54888 = alloca %struct.ScmObj*, align 8
%_37drop_45right48065 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49532, i64 3)
store %struct.ScmObj* %_37drop_45right48065, %struct.ScmObj** %stackaddr$env-ref54888
%stackaddr$env-ref54889 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49532, i64 4)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54889
%stackaddr$prim54890 = alloca %struct.ScmObj*, align 8
%_95k48283 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54166)
store volatile %struct.ScmObj* %_95k48283, %struct.ScmObj** %stackaddr$prim54890, align 8
%stackaddr$prim54891 = alloca %struct.ScmObj*, align 8
%current_45args54167 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54166)
store volatile %struct.ScmObj* %current_45args54167, %struct.ScmObj** %stackaddr$prim54891, align 8
%stackaddr$prim54892 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54167)
store volatile %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$prim54892, align 8
%stackaddr$makeclosure54893 = alloca %struct.ScmObj*, align 8
%fptrToInt54894 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49534 to i64
%ae49534 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt54894)
store volatile %struct.ScmObj* %ae49534, %struct.ScmObj** %stackaddr$makeclosure54893, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49534, %struct.ScmObj* %_37foldr148046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49534, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49534, %struct.ScmObj* %_37last48068, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49534, %struct.ScmObj* %_37foldr48051, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49534, %struct.ScmObj* %_37drop_45right48065, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49534, %struct.ScmObj* %Ycmb48025, i64 5)
%ae49535 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54895 = alloca %struct.ScmObj*, align 8
%fptrToInt54896 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49536 to i64
%ae49536 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54896)
store volatile %struct.ScmObj* %ae49536, %struct.ScmObj** %stackaddr$makeclosure54895, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49536, %struct.ScmObj* %_37foldr148046, i64 0)
%argslist54482$ae495340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54897 = alloca %struct.ScmObj*, align 8
%argslist54482$ae495341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49536, %struct.ScmObj* %argslist54482$ae495340)
store volatile %struct.ScmObj* %argslist54482$ae495341, %struct.ScmObj** %stackaddr$prim54897, align 8
%stackaddr$prim54898 = alloca %struct.ScmObj*, align 8
%argslist54482$ae495342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49535, %struct.ScmObj* %argslist54482$ae495341)
store volatile %struct.ScmObj* %argslist54482$ae495342, %struct.ScmObj** %stackaddr$prim54898, align 8
%clofunc54899 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49534)
musttail call tailcc void %clofunc54899(%struct.ScmObj* %ae49534, %struct.ScmObj* %argslist54482$ae495342)
ret void
}

define tailcc void @proc_clo$ae49534(%struct.ScmObj* %env$ae49534,%struct.ScmObj* %current_45args54169) {
%stackaddr$env-ref54900 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49534, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref54900
%stackaddr$env-ref54901 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49534, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref54901
%stackaddr$env-ref54902 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49534, i64 2)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref54902
%stackaddr$env-ref54903 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49534, i64 3)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref54903
%stackaddr$env-ref54904 = alloca %struct.ScmObj*, align 8
%_37drop_45right48065 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49534, i64 4)
store %struct.ScmObj* %_37drop_45right48065, %struct.ScmObj** %stackaddr$env-ref54904
%stackaddr$env-ref54905 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49534, i64 5)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54905
%stackaddr$prim54906 = alloca %struct.ScmObj*, align 8
%_95k48284 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54169)
store volatile %struct.ScmObj* %_95k48284, %struct.ScmObj** %stackaddr$prim54906, align 8
%stackaddr$prim54907 = alloca %struct.ScmObj*, align 8
%current_45args54170 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54169)
store volatile %struct.ScmObj* %current_45args54170, %struct.ScmObj** %stackaddr$prim54907, align 8
%stackaddr$prim54908 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54170)
store volatile %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$prim54908, align 8
%stackaddr$makeclosure54909 = alloca %struct.ScmObj*, align 8
%fptrToInt54910 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49611 to i64
%ae49611 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54910)
store volatile %struct.ScmObj* %ae49611, %struct.ScmObj** %stackaddr$makeclosure54909, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49611, %struct.ScmObj* %_37foldr148046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49611, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49611, %struct.ScmObj* %_37foldr48051, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49611, %struct.ScmObj* %_37map148077, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49611, %struct.ScmObj* %Ycmb48025, i64 4)
%ae49612 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54911 = alloca %struct.ScmObj*, align 8
%fptrToInt54912 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49613 to i64
%ae49613 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54912)
store volatile %struct.ScmObj* %ae49613, %struct.ScmObj** %stackaddr$makeclosure54911, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49613, %struct.ScmObj* %_37last48068, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49613, %struct.ScmObj* %_37foldr48051, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49613, %struct.ScmObj* %_37drop_45right48065, i64 2)
%argslist54463$ae496110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54913 = alloca %struct.ScmObj*, align 8
%argslist54463$ae496111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49613, %struct.ScmObj* %argslist54463$ae496110)
store volatile %struct.ScmObj* %argslist54463$ae496111, %struct.ScmObj** %stackaddr$prim54913, align 8
%stackaddr$prim54914 = alloca %struct.ScmObj*, align 8
%argslist54463$ae496112 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49612, %struct.ScmObj* %argslist54463$ae496111)
store volatile %struct.ScmObj* %argslist54463$ae496112, %struct.ScmObj** %stackaddr$prim54914, align 8
%clofunc54915 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49611)
musttail call tailcc void %clofunc54915(%struct.ScmObj* %ae49611, %struct.ScmObj* %argslist54463$ae496112)
ret void
}

define tailcc void @proc_clo$ae49611(%struct.ScmObj* %env$ae49611,%struct.ScmObj* %current_45args54172) {
%stackaddr$env-ref54916 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49611, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref54916
%stackaddr$env-ref54917 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49611, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref54917
%stackaddr$env-ref54918 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49611, i64 2)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref54918
%stackaddr$env-ref54919 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49611, i64 3)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref54919
%stackaddr$env-ref54920 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49611, i64 4)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54920
%stackaddr$prim54921 = alloca %struct.ScmObj*, align 8
%_95k48285 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54172)
store volatile %struct.ScmObj* %_95k48285, %struct.ScmObj** %stackaddr$prim54921, align 8
%stackaddr$prim54922 = alloca %struct.ScmObj*, align 8
%current_45args54173 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54172)
store volatile %struct.ScmObj* %current_45args54173, %struct.ScmObj** %stackaddr$prim54922, align 8
%stackaddr$prim54923 = alloca %struct.ScmObj*, align 8
%_37map48072 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54173)
store volatile %struct.ScmObj* %_37map48072, %struct.ScmObj** %stackaddr$prim54923, align 8
%stackaddr$makeclosure54924 = alloca %struct.ScmObj*, align 8
%fptrToInt54925 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49757 to i64
%ae49757 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54925)
store volatile %struct.ScmObj* %ae49757, %struct.ScmObj** %stackaddr$makeclosure54924, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49757, %struct.ScmObj* %_37foldl148030, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49757, %struct.ScmObj* %Ycmb48025, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49757, %struct.ScmObj* %_37map48072, i64 2)
%ae49758 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54926 = alloca %struct.ScmObj*, align 8
%fptrToInt54927 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49759 to i64
%ae49759 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54927)
store volatile %struct.ScmObj* %ae49759, %struct.ScmObj** %stackaddr$makeclosure54926, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49759, %struct.ScmObj* %_37foldr48051, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49759, %struct.ScmObj* %_37foldr148046, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49759, %struct.ScmObj* %_37map148077, i64 2)
%argslist54446$ae497570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54928 = alloca %struct.ScmObj*, align 8
%argslist54446$ae497571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49759, %struct.ScmObj* %argslist54446$ae497570)
store volatile %struct.ScmObj* %argslist54446$ae497571, %struct.ScmObj** %stackaddr$prim54928, align 8
%stackaddr$prim54929 = alloca %struct.ScmObj*, align 8
%argslist54446$ae497572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49758, %struct.ScmObj* %argslist54446$ae497571)
store volatile %struct.ScmObj* %argslist54446$ae497572, %struct.ScmObj** %stackaddr$prim54929, align 8
%clofunc54930 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49757)
musttail call tailcc void %clofunc54930(%struct.ScmObj* %ae49757, %struct.ScmObj* %argslist54446$ae497572)
ret void
}

define tailcc void @proc_clo$ae49757(%struct.ScmObj* %env$ae49757,%struct.ScmObj* %current_45args54175) {
%stackaddr$env-ref54931 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49757, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref54931
%stackaddr$env-ref54932 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49757, i64 1)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54932
%stackaddr$env-ref54933 = alloca %struct.ScmObj*, align 8
%_37map48072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49757, i64 2)
store %struct.ScmObj* %_37map48072, %struct.ScmObj** %stackaddr$env-ref54933
%stackaddr$prim54934 = alloca %struct.ScmObj*, align 8
%_95k48286 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54175)
store volatile %struct.ScmObj* %_95k48286, %struct.ScmObj** %stackaddr$prim54934, align 8
%stackaddr$prim54935 = alloca %struct.ScmObj*, align 8
%current_45args54176 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54175)
store volatile %struct.ScmObj* %current_45args54176, %struct.ScmObj** %stackaddr$prim54935, align 8
%stackaddr$prim54936 = alloca %struct.ScmObj*, align 8
%anf_45bind48215 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54176)
store volatile %struct.ScmObj* %anf_45bind48215, %struct.ScmObj** %stackaddr$prim54936, align 8
%stackaddr$makeclosure54937 = alloca %struct.ScmObj*, align 8
%fptrToInt54938 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50149 to i64
%ae50149 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54938)
store volatile %struct.ScmObj* %ae50149, %struct.ScmObj** %stackaddr$makeclosure54937, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50149, %struct.ScmObj* %_37foldl148030, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50149, %struct.ScmObj* %_37map48072, i64 1)
%argslist54386$Ycmb480250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54939 = alloca %struct.ScmObj*, align 8
%argslist54386$Ycmb480251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48215, %struct.ScmObj* %argslist54386$Ycmb480250)
store volatile %struct.ScmObj* %argslist54386$Ycmb480251, %struct.ScmObj** %stackaddr$prim54939, align 8
%stackaddr$prim54940 = alloca %struct.ScmObj*, align 8
%argslist54386$Ycmb480252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50149, %struct.ScmObj* %argslist54386$Ycmb480251)
store volatile %struct.ScmObj* %argslist54386$Ycmb480252, %struct.ScmObj** %stackaddr$prim54940, align 8
%clofunc54941 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48025)
musttail call tailcc void %clofunc54941(%struct.ScmObj* %Ycmb48025, %struct.ScmObj* %argslist54386$Ycmb480252)
ret void
}

define tailcc void @proc_clo$ae50149(%struct.ScmObj* %env$ae50149,%struct.ScmObj* %current_45args54178) {
%stackaddr$env-ref54942 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50149, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref54942
%stackaddr$env-ref54943 = alloca %struct.ScmObj*, align 8
%_37map48072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50149, i64 1)
store %struct.ScmObj* %_37map48072, %struct.ScmObj** %stackaddr$env-ref54943
%stackaddr$prim54944 = alloca %struct.ScmObj*, align 8
%_95k48287 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54178)
store volatile %struct.ScmObj* %_95k48287, %struct.ScmObj** %stackaddr$prim54944, align 8
%stackaddr$prim54945 = alloca %struct.ScmObj*, align 8
%current_45args54179 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54178)
store volatile %struct.ScmObj* %current_45args54179, %struct.ScmObj** %stackaddr$prim54945, align 8
%stackaddr$prim54946 = alloca %struct.ScmObj*, align 8
%_37foldl48128 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54179)
store volatile %struct.ScmObj* %_37foldl48128, %struct.ScmObj** %stackaddr$prim54946, align 8
%stackaddr$makeclosure54947 = alloca %struct.ScmObj*, align 8
%fptrToInt54948 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50151 to i64
%ae50151 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54948)
store volatile %struct.ScmObj* %ae50151, %struct.ScmObj** %stackaddr$makeclosure54947, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50151, %struct.ScmObj* %_37foldl148030, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50151, %struct.ScmObj* %_37map48072, i64 1)
%ae50152 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54949 = alloca %struct.ScmObj*, align 8
%fptrToInt54950 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50153 to i64
%ae50153 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54950)
store volatile %struct.ScmObj* %ae50153, %struct.ScmObj** %stackaddr$makeclosure54949, align 8
%argslist54385$ae501510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54951 = alloca %struct.ScmObj*, align 8
%argslist54385$ae501511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50153, %struct.ScmObj* %argslist54385$ae501510)
store volatile %struct.ScmObj* %argslist54385$ae501511, %struct.ScmObj** %stackaddr$prim54951, align 8
%stackaddr$prim54952 = alloca %struct.ScmObj*, align 8
%argslist54385$ae501512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50152, %struct.ScmObj* %argslist54385$ae501511)
store volatile %struct.ScmObj* %argslist54385$ae501512, %struct.ScmObj** %stackaddr$prim54952, align 8
%clofunc54953 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50151)
musttail call tailcc void %clofunc54953(%struct.ScmObj* %ae50151, %struct.ScmObj* %argslist54385$ae501512)
ret void
}

define tailcc void @proc_clo$ae50151(%struct.ScmObj* %env$ae50151,%struct.ScmObj* %current_45args54181) {
%stackaddr$env-ref54954 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50151, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref54954
%stackaddr$env-ref54955 = alloca %struct.ScmObj*, align 8
%_37map48072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50151, i64 1)
store %struct.ScmObj* %_37map48072, %struct.ScmObj** %stackaddr$env-ref54955
%stackaddr$prim54956 = alloca %struct.ScmObj*, align 8
%_95k48288 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54181)
store volatile %struct.ScmObj* %_95k48288, %struct.ScmObj** %stackaddr$prim54956, align 8
%stackaddr$prim54957 = alloca %struct.ScmObj*, align 8
%current_45args54182 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54181)
store volatile %struct.ScmObj* %current_45args54182, %struct.ScmObj** %stackaddr$prim54957, align 8
%stackaddr$prim54958 = alloca %struct.ScmObj*, align 8
%_37_6248125 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54182)
store volatile %struct.ScmObj* %_37_6248125, %struct.ScmObj** %stackaddr$prim54958, align 8
%stackaddr$makeclosure54959 = alloca %struct.ScmObj*, align 8
%fptrToInt54960 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50175 to i64
%ae50175 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54960)
store volatile %struct.ScmObj* %ae50175, %struct.ScmObj** %stackaddr$makeclosure54959, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50175, %struct.ScmObj* %_37foldl148030, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50175, %struct.ScmObj* %_37map48072, i64 1)
%ae50176 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54961 = alloca %struct.ScmObj*, align 8
%fptrToInt54962 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50177 to i64
%ae50177 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54962)
store volatile %struct.ScmObj* %ae50177, %struct.ScmObj** %stackaddr$makeclosure54961, align 8
%argslist54379$ae501750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54963 = alloca %struct.ScmObj*, align 8
%argslist54379$ae501751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50177, %struct.ScmObj* %argslist54379$ae501750)
store volatile %struct.ScmObj* %argslist54379$ae501751, %struct.ScmObj** %stackaddr$prim54963, align 8
%stackaddr$prim54964 = alloca %struct.ScmObj*, align 8
%argslist54379$ae501752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50176, %struct.ScmObj* %argslist54379$ae501751)
store volatile %struct.ScmObj* %argslist54379$ae501752, %struct.ScmObj** %stackaddr$prim54964, align 8
%clofunc54965 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50175)
musttail call tailcc void %clofunc54965(%struct.ScmObj* %ae50175, %struct.ScmObj* %argslist54379$ae501752)
ret void
}

define tailcc void @proc_clo$ae50175(%struct.ScmObj* %env$ae50175,%struct.ScmObj* %current_45args54184) {
%stackaddr$env-ref54966 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50175, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref54966
%stackaddr$env-ref54967 = alloca %struct.ScmObj*, align 8
%_37map48072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50175, i64 1)
store %struct.ScmObj* %_37map48072, %struct.ScmObj** %stackaddr$env-ref54967
%stackaddr$prim54968 = alloca %struct.ScmObj*, align 8
%_95k48289 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54184)
store volatile %struct.ScmObj* %_95k48289, %struct.ScmObj** %stackaddr$prim54968, align 8
%stackaddr$prim54969 = alloca %struct.ScmObj*, align 8
%current_45args54185 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54184)
store volatile %struct.ScmObj* %current_45args54185, %struct.ScmObj** %stackaddr$prim54969, align 8
%stackaddr$prim54970 = alloca %struct.ScmObj*, align 8
%_37_62_6148122 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54185)
store volatile %struct.ScmObj* %_37_62_6148122, %struct.ScmObj** %stackaddr$prim54970, align 8
%ae50199 = call %struct.ScmObj* @const_init_int(i64 1)
%ae50200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54971 = alloca %struct.ScmObj*, align 8
%_37append48118 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50199, %struct.ScmObj* %ae50200)
store volatile %struct.ScmObj* %_37append48118, %struct.ScmObj** %stackaddr$prim54971, align 8
%stackaddr$makeclosure54972 = alloca %struct.ScmObj*, align 8
%fptrToInt54973 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50201 to i64
%ae50201 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54973)
store volatile %struct.ScmObj* %ae50201, %struct.ScmObj** %stackaddr$makeclosure54972, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50201, %struct.ScmObj* %_37append48118, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50201, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50201, %struct.ScmObj* %_37map48072, i64 2)
%ae50202 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54974 = alloca %struct.ScmObj*, align 8
%fptrToInt54975 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50203 to i64
%ae50203 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54975)
store volatile %struct.ScmObj* %ae50203, %struct.ScmObj** %stackaddr$makeclosure54974, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50203, %struct.ScmObj* %_37append48118, i64 0)
%argslist54373$ae502010 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54976 = alloca %struct.ScmObj*, align 8
%argslist54373$ae502011 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50203, %struct.ScmObj* %argslist54373$ae502010)
store volatile %struct.ScmObj* %argslist54373$ae502011, %struct.ScmObj** %stackaddr$prim54976, align 8
%stackaddr$prim54977 = alloca %struct.ScmObj*, align 8
%argslist54373$ae502012 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50202, %struct.ScmObj* %argslist54373$ae502011)
store volatile %struct.ScmObj* %argslist54373$ae502012, %struct.ScmObj** %stackaddr$prim54977, align 8
%clofunc54978 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50201)
musttail call tailcc void %clofunc54978(%struct.ScmObj* %ae50201, %struct.ScmObj* %argslist54373$ae502012)
ret void
}

define tailcc void @proc_clo$ae50201(%struct.ScmObj* %env$ae50201,%struct.ScmObj* %current_45args54187) {
%stackaddr$env-ref54979 = alloca %struct.ScmObj*, align 8
%_37append48118 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50201, i64 0)
store %struct.ScmObj* %_37append48118, %struct.ScmObj** %stackaddr$env-ref54979
%stackaddr$env-ref54980 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50201, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref54980
%stackaddr$env-ref54981 = alloca %struct.ScmObj*, align 8
%_37map48072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50201, i64 2)
store %struct.ScmObj* %_37map48072, %struct.ScmObj** %stackaddr$env-ref54981
%stackaddr$prim54982 = alloca %struct.ScmObj*, align 8
%_95k48290 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54187)
store volatile %struct.ScmObj* %_95k48290, %struct.ScmObj** %stackaddr$prim54982, align 8
%stackaddr$prim54983 = alloca %struct.ScmObj*, align 8
%current_45args54188 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54187)
store volatile %struct.ScmObj* %current_45args54188, %struct.ScmObj** %stackaddr$prim54983, align 8
%stackaddr$prim54984 = alloca %struct.ScmObj*, align 8
%anf_45bind48223 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54188)
store volatile %struct.ScmObj* %anf_45bind48223, %struct.ScmObj** %stackaddr$prim54984, align 8
%ae50269 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54985 = alloca %struct.ScmObj*, align 8
%_95048119 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append48118, %struct.ScmObj* %ae50269, %struct.ScmObj* %anf_45bind48223)
store volatile %struct.ScmObj* %_95048119, %struct.ScmObj** %stackaddr$prim54985, align 8
%ae50272 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54986 = alloca %struct.ScmObj*, align 8
%_37append48117 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append48118, %struct.ScmObj* %ae50272)
store volatile %struct.ScmObj* %_37append48117, %struct.ScmObj** %stackaddr$prim54986, align 8
%stackaddr$makeclosure54987 = alloca %struct.ScmObj*, align 8
%fptrToInt54988 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50273 to i64
%ae50273 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54988)
store volatile %struct.ScmObj* %ae50273, %struct.ScmObj** %stackaddr$makeclosure54987, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50273, %struct.ScmObj* %_37foldl148030, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50273, %struct.ScmObj* %_37map48072, i64 1)
%ae50274 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54989 = alloca %struct.ScmObj*, align 8
%fptrToInt54990 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50275 to i64
%ae50275 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54990)
store volatile %struct.ScmObj* %ae50275, %struct.ScmObj** %stackaddr$makeclosure54989, align 8
%argslist54362$ae502730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54991 = alloca %struct.ScmObj*, align 8
%argslist54362$ae502731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50275, %struct.ScmObj* %argslist54362$ae502730)
store volatile %struct.ScmObj* %argslist54362$ae502731, %struct.ScmObj** %stackaddr$prim54991, align 8
%stackaddr$prim54992 = alloca %struct.ScmObj*, align 8
%argslist54362$ae502732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50274, %struct.ScmObj* %argslist54362$ae502731)
store volatile %struct.ScmObj* %argslist54362$ae502732, %struct.ScmObj** %stackaddr$prim54992, align 8
%clofunc54993 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50273)
musttail call tailcc void %clofunc54993(%struct.ScmObj* %ae50273, %struct.ScmObj* %argslist54362$ae502732)
ret void
}

define tailcc void @proc_clo$ae50273(%struct.ScmObj* %env$ae50273,%struct.ScmObj* %current_45args54190) {
%stackaddr$env-ref54994 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50273, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref54994
%stackaddr$env-ref54995 = alloca %struct.ScmObj*, align 8
%_37map48072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50273, i64 1)
store %struct.ScmObj* %_37map48072, %struct.ScmObj** %stackaddr$env-ref54995
%stackaddr$prim54996 = alloca %struct.ScmObj*, align 8
%_95k48291 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54190)
store volatile %struct.ScmObj* %_95k48291, %struct.ScmObj** %stackaddr$prim54996, align 8
%stackaddr$prim54997 = alloca %struct.ScmObj*, align 8
%current_45args54191 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54190)
store volatile %struct.ScmObj* %current_45args54191, %struct.ScmObj** %stackaddr$prim54997, align 8
%stackaddr$prim54998 = alloca %struct.ScmObj*, align 8
%_37list_6348110 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54191)
store volatile %struct.ScmObj* %_37list_6348110, %struct.ScmObj** %stackaddr$prim54998, align 8
%stackaddr$makeclosure54999 = alloca %struct.ScmObj*, align 8
%fptrToInt55000 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50689 to i64
%ae50689 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55000)
store volatile %struct.ScmObj* %ae50689, %struct.ScmObj** %stackaddr$makeclosure54999, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50689, %struct.ScmObj* %_37foldl148030, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50689, %struct.ScmObj* %_37map48072, i64 1)
%ae50690 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55001 = alloca %struct.ScmObj*, align 8
%fptrToInt55002 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50691 to i64
%ae50691 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55002)
store volatile %struct.ScmObj* %ae50691, %struct.ScmObj** %stackaddr$makeclosure55001, align 8
%argslist54337$ae506890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55003 = alloca %struct.ScmObj*, align 8
%argslist54337$ae506891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50691, %struct.ScmObj* %argslist54337$ae506890)
store volatile %struct.ScmObj* %argslist54337$ae506891, %struct.ScmObj** %stackaddr$prim55003, align 8
%stackaddr$prim55004 = alloca %struct.ScmObj*, align 8
%argslist54337$ae506892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50690, %struct.ScmObj* %argslist54337$ae506891)
store volatile %struct.ScmObj* %argslist54337$ae506892, %struct.ScmObj** %stackaddr$prim55004, align 8
%clofunc55005 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50689)
musttail call tailcc void %clofunc55005(%struct.ScmObj* %ae50689, %struct.ScmObj* %argslist54337$ae506892)
ret void
}

define tailcc void @proc_clo$ae50689(%struct.ScmObj* %env$ae50689,%struct.ScmObj* %current_45args54193) {
%stackaddr$env-ref55006 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50689, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55006
%stackaddr$env-ref55007 = alloca %struct.ScmObj*, align 8
%_37map48072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50689, i64 1)
store %struct.ScmObj* %_37map48072, %struct.ScmObj** %stackaddr$env-ref55007
%stackaddr$prim55008 = alloca %struct.ScmObj*, align 8
%_95k48292 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54193)
store volatile %struct.ScmObj* %_95k48292, %struct.ScmObj** %stackaddr$prim55008, align 8
%stackaddr$prim55009 = alloca %struct.ScmObj*, align 8
%current_45args54194 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54193)
store volatile %struct.ScmObj* %current_45args54194, %struct.ScmObj** %stackaddr$prim55009, align 8
%stackaddr$prim55010 = alloca %struct.ScmObj*, align 8
%_37drop48101 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54194)
store volatile %struct.ScmObj* %_37drop48101, %struct.ScmObj** %stackaddr$prim55010, align 8
%stackaddr$makeclosure55011 = alloca %struct.ScmObj*, align 8
%fptrToInt55012 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51225 to i64
%ae51225 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55012)
store volatile %struct.ScmObj* %ae51225, %struct.ScmObj** %stackaddr$makeclosure55011, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51225, %struct.ScmObj* %_37foldl148030, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51225, %struct.ScmObj* %_37map48072, i64 1)
%ae51226 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55013 = alloca %struct.ScmObj*, align 8
%fptrToInt55014 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51227 to i64
%ae51227 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55014)
store volatile %struct.ScmObj* %ae51227, %struct.ScmObj** %stackaddr$makeclosure55013, align 8
%argslist54313$ae512250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55015 = alloca %struct.ScmObj*, align 8
%argslist54313$ae512251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51227, %struct.ScmObj* %argslist54313$ae512250)
store volatile %struct.ScmObj* %argslist54313$ae512251, %struct.ScmObj** %stackaddr$prim55015, align 8
%stackaddr$prim55016 = alloca %struct.ScmObj*, align 8
%argslist54313$ae512252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51226, %struct.ScmObj* %argslist54313$ae512251)
store volatile %struct.ScmObj* %argslist54313$ae512252, %struct.ScmObj** %stackaddr$prim55016, align 8
%clofunc55017 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51225)
musttail call tailcc void %clofunc55017(%struct.ScmObj* %ae51225, %struct.ScmObj* %argslist54313$ae512252)
ret void
}

define tailcc void @proc_clo$ae51225(%struct.ScmObj* %env$ae51225,%struct.ScmObj* %current_45args54196) {
%stackaddr$env-ref55018 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51225, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55018
%stackaddr$env-ref55019 = alloca %struct.ScmObj*, align 8
%_37map48072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51225, i64 1)
store %struct.ScmObj* %_37map48072, %struct.ScmObj** %stackaddr$env-ref55019
%stackaddr$prim55020 = alloca %struct.ScmObj*, align 8
%_95k48293 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54196)
store volatile %struct.ScmObj* %_95k48293, %struct.ScmObj** %stackaddr$prim55020, align 8
%stackaddr$prim55021 = alloca %struct.ScmObj*, align 8
%current_45args54197 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54196)
store volatile %struct.ScmObj* %current_45args54197, %struct.ScmObj** %stackaddr$prim55021, align 8
%stackaddr$prim55022 = alloca %struct.ScmObj*, align 8
%_37memv48094 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54197)
store volatile %struct.ScmObj* %_37memv48094, %struct.ScmObj** %stackaddr$prim55022, align 8
%stackaddr$makeclosure55023 = alloca %struct.ScmObj*, align 8
%fptrToInt55024 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51629 to i64
%ae51629 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55024)
store volatile %struct.ScmObj* %ae51629, %struct.ScmObj** %stackaddr$makeclosure55023, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51629, %struct.ScmObj* %_37foldl148030, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51629, %struct.ScmObj* %_37map48072, i64 1)
%ae51630 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55025 = alloca %struct.ScmObj*, align 8
%fptrToInt55026 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51631 to i64
%ae51631 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55026)
store volatile %struct.ScmObj* %ae51631, %struct.ScmObj** %stackaddr$makeclosure55025, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51631, %struct.ScmObj* %_37foldl148030, i64 0)
%argslist54287$ae516290 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55027 = alloca %struct.ScmObj*, align 8
%argslist54287$ae516291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51631, %struct.ScmObj* %argslist54287$ae516290)
store volatile %struct.ScmObj* %argslist54287$ae516291, %struct.ScmObj** %stackaddr$prim55027, align 8
%stackaddr$prim55028 = alloca %struct.ScmObj*, align 8
%argslist54287$ae516292 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51630, %struct.ScmObj* %argslist54287$ae516291)
store volatile %struct.ScmObj* %argslist54287$ae516292, %struct.ScmObj** %stackaddr$prim55028, align 8
%clofunc55029 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51629)
musttail call tailcc void %clofunc55029(%struct.ScmObj* %ae51629, %struct.ScmObj* %argslist54287$ae516292)
ret void
}

define tailcc void @proc_clo$ae51629(%struct.ScmObj* %env$ae51629,%struct.ScmObj* %current_45args54199) {
%stackaddr$env-ref55030 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51629, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55030
%stackaddr$env-ref55031 = alloca %struct.ScmObj*, align 8
%_37map48072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51629, i64 1)
store %struct.ScmObj* %_37map48072, %struct.ScmObj** %stackaddr$env-ref55031
%stackaddr$prim55032 = alloca %struct.ScmObj*, align 8
%_95k48294 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54199)
store volatile %struct.ScmObj* %_95k48294, %struct.ScmObj** %stackaddr$prim55032, align 8
%stackaddr$prim55033 = alloca %struct.ScmObj*, align 8
%current_45args54200 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54199)
store volatile %struct.ScmObj* %current_45args54200, %struct.ScmObj** %stackaddr$prim55033, align 8
%stackaddr$prim55034 = alloca %struct.ScmObj*, align 8
%_37_4748090 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54200)
store volatile %struct.ScmObj* %_37_4748090, %struct.ScmObj** %stackaddr$prim55034, align 8
%stackaddr$makeclosure55035 = alloca %struct.ScmObj*, align 8
%fptrToInt55036 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51727 to i64
%ae51727 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55036)
store volatile %struct.ScmObj* %ae51727, %struct.ScmObj** %stackaddr$makeclosure55035, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51727, %struct.ScmObj* %_37foldl148030, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51727, %struct.ScmObj* %_37map48072, i64 1)
%ae51728 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55037 = alloca %struct.ScmObj*, align 8
%fptrToInt55038 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51729 to i64
%ae51729 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55038)
store volatile %struct.ScmObj* %ae51729, %struct.ScmObj** %stackaddr$makeclosure55037, align 8
%argslist54274$ae517270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55039 = alloca %struct.ScmObj*, align 8
%argslist54274$ae517271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51729, %struct.ScmObj* %argslist54274$ae517270)
store volatile %struct.ScmObj* %argslist54274$ae517271, %struct.ScmObj** %stackaddr$prim55039, align 8
%stackaddr$prim55040 = alloca %struct.ScmObj*, align 8
%argslist54274$ae517272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51728, %struct.ScmObj* %argslist54274$ae517271)
store volatile %struct.ScmObj* %argslist54274$ae517272, %struct.ScmObj** %stackaddr$prim55040, align 8
%clofunc55041 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51727)
musttail call tailcc void %clofunc55041(%struct.ScmObj* %ae51727, %struct.ScmObj* %argslist54274$ae517272)
ret void
}

define tailcc void @proc_clo$ae51727(%struct.ScmObj* %env$ae51727,%struct.ScmObj* %current_45args54202) {
%stackaddr$env-ref55042 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51727, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55042
%stackaddr$env-ref55043 = alloca %struct.ScmObj*, align 8
%_37map48072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51727, i64 1)
store %struct.ScmObj* %_37map48072, %struct.ScmObj** %stackaddr$env-ref55043
%stackaddr$prim55044 = alloca %struct.ScmObj*, align 8
%_95k48295 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54202)
store volatile %struct.ScmObj* %_95k48295, %struct.ScmObj** %stackaddr$prim55044, align 8
%stackaddr$prim55045 = alloca %struct.ScmObj*, align 8
%current_45args54203 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54202)
store volatile %struct.ScmObj* %current_45args54203, %struct.ScmObj** %stackaddr$prim55045, align 8
%stackaddr$prim55046 = alloca %struct.ScmObj*, align 8
%_37first48088 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54203)
store volatile %struct.ScmObj* %_37first48088, %struct.ScmObj** %stackaddr$prim55046, align 8
%stackaddr$makeclosure55047 = alloca %struct.ScmObj*, align 8
%fptrToInt55048 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51747 to i64
%ae51747 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55048)
store volatile %struct.ScmObj* %ae51747, %struct.ScmObj** %stackaddr$makeclosure55047, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51747, %struct.ScmObj* %_37foldl148030, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51747, %struct.ScmObj* %_37map48072, i64 1)
%ae51748 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55049 = alloca %struct.ScmObj*, align 8
%fptrToInt55050 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51749 to i64
%ae51749 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55050)
store volatile %struct.ScmObj* %ae51749, %struct.ScmObj** %stackaddr$makeclosure55049, align 8
%argslist54269$ae517470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55051 = alloca %struct.ScmObj*, align 8
%argslist54269$ae517471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51749, %struct.ScmObj* %argslist54269$ae517470)
store volatile %struct.ScmObj* %argslist54269$ae517471, %struct.ScmObj** %stackaddr$prim55051, align 8
%stackaddr$prim55052 = alloca %struct.ScmObj*, align 8
%argslist54269$ae517472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51748, %struct.ScmObj* %argslist54269$ae517471)
store volatile %struct.ScmObj* %argslist54269$ae517472, %struct.ScmObj** %stackaddr$prim55052, align 8
%clofunc55053 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51747)
musttail call tailcc void %clofunc55053(%struct.ScmObj* %ae51747, %struct.ScmObj* %argslist54269$ae517472)
ret void
}

define tailcc void @proc_clo$ae51747(%struct.ScmObj* %env$ae51747,%struct.ScmObj* %current_45args54205) {
%stackaddr$env-ref55054 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51747, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55054
%stackaddr$env-ref55055 = alloca %struct.ScmObj*, align 8
%_37map48072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51747, i64 1)
store %struct.ScmObj* %_37map48072, %struct.ScmObj** %stackaddr$env-ref55055
%stackaddr$prim55056 = alloca %struct.ScmObj*, align 8
%_95k48296 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54205)
store volatile %struct.ScmObj* %_95k48296, %struct.ScmObj** %stackaddr$prim55056, align 8
%stackaddr$prim55057 = alloca %struct.ScmObj*, align 8
%current_45args54206 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54205)
store volatile %struct.ScmObj* %current_45args54206, %struct.ScmObj** %stackaddr$prim55057, align 8
%stackaddr$prim55058 = alloca %struct.ScmObj*, align 8
%_37second48086 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54206)
store volatile %struct.ScmObj* %_37second48086, %struct.ScmObj** %stackaddr$prim55058, align 8
%stackaddr$makeclosure55059 = alloca %struct.ScmObj*, align 8
%fptrToInt55060 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51769 to i64
%ae51769 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55060)
store volatile %struct.ScmObj* %ae51769, %struct.ScmObj** %stackaddr$makeclosure55059, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51769, %struct.ScmObj* %_37foldl148030, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51769, %struct.ScmObj* %_37map48072, i64 1)
%ae51770 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55061 = alloca %struct.ScmObj*, align 8
%fptrToInt55062 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51771 to i64
%ae51771 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55062)
store volatile %struct.ScmObj* %ae51771, %struct.ScmObj** %stackaddr$makeclosure55061, align 8
%argslist54264$ae517690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55063 = alloca %struct.ScmObj*, align 8
%argslist54264$ae517691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51771, %struct.ScmObj* %argslist54264$ae517690)
store volatile %struct.ScmObj* %argslist54264$ae517691, %struct.ScmObj** %stackaddr$prim55063, align 8
%stackaddr$prim55064 = alloca %struct.ScmObj*, align 8
%argslist54264$ae517692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51770, %struct.ScmObj* %argslist54264$ae517691)
store volatile %struct.ScmObj* %argslist54264$ae517692, %struct.ScmObj** %stackaddr$prim55064, align 8
%clofunc55065 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51769)
musttail call tailcc void %clofunc55065(%struct.ScmObj* %ae51769, %struct.ScmObj* %argslist54264$ae517692)
ret void
}

define tailcc void @proc_clo$ae51769(%struct.ScmObj* %env$ae51769,%struct.ScmObj* %current_45args54208) {
%stackaddr$env-ref55066 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51769, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55066
%stackaddr$env-ref55067 = alloca %struct.ScmObj*, align 8
%_37map48072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51769, i64 1)
store %struct.ScmObj* %_37map48072, %struct.ScmObj** %stackaddr$env-ref55067
%stackaddr$prim55068 = alloca %struct.ScmObj*, align 8
%_95k48297 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54208)
store volatile %struct.ScmObj* %_95k48297, %struct.ScmObj** %stackaddr$prim55068, align 8
%stackaddr$prim55069 = alloca %struct.ScmObj*, align 8
%current_45args54209 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54208)
store volatile %struct.ScmObj* %current_45args54209, %struct.ScmObj** %stackaddr$prim55069, align 8
%stackaddr$prim55070 = alloca %struct.ScmObj*, align 8
%_37third48084 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54209)
store volatile %struct.ScmObj* %_37third48084, %struct.ScmObj** %stackaddr$prim55070, align 8
%stackaddr$makeclosure55071 = alloca %struct.ScmObj*, align 8
%fptrToInt55072 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51793 to i64
%ae51793 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55072)
store volatile %struct.ScmObj* %ae51793, %struct.ScmObj** %stackaddr$makeclosure55071, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51793, %struct.ScmObj* %_37foldl148030, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51793, %struct.ScmObj* %_37map48072, i64 1)
%ae51794 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55073 = alloca %struct.ScmObj*, align 8
%fptrToInt55074 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51795 to i64
%ae51795 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55074)
store volatile %struct.ScmObj* %ae51795, %struct.ScmObj** %stackaddr$makeclosure55073, align 8
%argslist54259$ae517930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55075 = alloca %struct.ScmObj*, align 8
%argslist54259$ae517931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51795, %struct.ScmObj* %argslist54259$ae517930)
store volatile %struct.ScmObj* %argslist54259$ae517931, %struct.ScmObj** %stackaddr$prim55075, align 8
%stackaddr$prim55076 = alloca %struct.ScmObj*, align 8
%argslist54259$ae517932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51794, %struct.ScmObj* %argslist54259$ae517931)
store volatile %struct.ScmObj* %argslist54259$ae517932, %struct.ScmObj** %stackaddr$prim55076, align 8
%clofunc55077 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51793)
musttail call tailcc void %clofunc55077(%struct.ScmObj* %ae51793, %struct.ScmObj* %argslist54259$ae517932)
ret void
}

define tailcc void @proc_clo$ae51793(%struct.ScmObj* %env$ae51793,%struct.ScmObj* %current_45args54211) {
%stackaddr$env-ref55078 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51793, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55078
%stackaddr$env-ref55079 = alloca %struct.ScmObj*, align 8
%_37map48072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51793, i64 1)
store %struct.ScmObj* %_37map48072, %struct.ScmObj** %stackaddr$env-ref55079
%stackaddr$prim55080 = alloca %struct.ScmObj*, align 8
%_95k48298 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54211)
store volatile %struct.ScmObj* %_95k48298, %struct.ScmObj** %stackaddr$prim55080, align 8
%stackaddr$prim55081 = alloca %struct.ScmObj*, align 8
%current_45args54212 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54211)
store volatile %struct.ScmObj* %current_45args54212, %struct.ScmObj** %stackaddr$prim55081, align 8
%stackaddr$prim55082 = alloca %struct.ScmObj*, align 8
%_37fourth48082 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54212)
store volatile %struct.ScmObj* %_37fourth48082, %struct.ScmObj** %stackaddr$prim55082, align 8
%stackaddr$makeclosure55083 = alloca %struct.ScmObj*, align 8
%fptrToInt55084 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51819 to i64
%ae51819 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55084)
store volatile %struct.ScmObj* %ae51819, %struct.ScmObj** %stackaddr$makeclosure55083, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51819, %struct.ScmObj* %_37foldl148030, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51819, %struct.ScmObj* %_37map48072, i64 1)
%ae51820 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55085 = alloca %struct.ScmObj*, align 8
%fptrToInt55086 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51821 to i64
%ae51821 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55086)
store volatile %struct.ScmObj* %ae51821, %struct.ScmObj** %stackaddr$makeclosure55085, align 8
%argslist54254$ae518190 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55087 = alloca %struct.ScmObj*, align 8
%argslist54254$ae518191 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51821, %struct.ScmObj* %argslist54254$ae518190)
store volatile %struct.ScmObj* %argslist54254$ae518191, %struct.ScmObj** %stackaddr$prim55087, align 8
%stackaddr$prim55088 = alloca %struct.ScmObj*, align 8
%argslist54254$ae518192 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51820, %struct.ScmObj* %argslist54254$ae518191)
store volatile %struct.ScmObj* %argslist54254$ae518192, %struct.ScmObj** %stackaddr$prim55088, align 8
%clofunc55089 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51819)
musttail call tailcc void %clofunc55089(%struct.ScmObj* %ae51819, %struct.ScmObj* %argslist54254$ae518192)
ret void
}

define tailcc void @proc_clo$ae51819(%struct.ScmObj* %env$ae51819,%struct.ScmObj* %current_45args54214) {
%stackaddr$env-ref55090 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51819, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55090
%stackaddr$env-ref55091 = alloca %struct.ScmObj*, align 8
%_37map48072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51819, i64 1)
store %struct.ScmObj* %_37map48072, %struct.ScmObj** %stackaddr$env-ref55091
%stackaddr$prim55092 = alloca %struct.ScmObj*, align 8
%_95k48299 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54214)
store volatile %struct.ScmObj* %_95k48299, %struct.ScmObj** %stackaddr$prim55092, align 8
%stackaddr$prim55093 = alloca %struct.ScmObj*, align 8
%current_45args54215 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54214)
store volatile %struct.ScmObj* %current_45args54215, %struct.ScmObj** %stackaddr$prim55093, align 8
%stackaddr$prim55094 = alloca %struct.ScmObj*, align 8
%anf_45bind48259 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54215)
store volatile %struct.ScmObj* %anf_45bind48259, %struct.ScmObj** %stackaddr$prim55094, align 8
%stackaddr$makeclosure55095 = alloca %struct.ScmObj*, align 8
%fptrToInt55096 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51843 to i64
%ae51843 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55096)
store volatile %struct.ScmObj* %ae51843, %struct.ScmObj** %stackaddr$makeclosure55095, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51843, %struct.ScmObj* %anf_45bind48259, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51843, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51843, %struct.ScmObj* %_37map48072, i64 2)
%ae51844 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55097 = alloca %struct.ScmObj*, align 8
%fptrToInt55098 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51845 to i64
%ae51845 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55098)
store volatile %struct.ScmObj* %ae51845, %struct.ScmObj** %stackaddr$makeclosure55097, align 8
%argslist54252$ae518430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55099 = alloca %struct.ScmObj*, align 8
%argslist54252$ae518431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51845, %struct.ScmObj* %argslist54252$ae518430)
store volatile %struct.ScmObj* %argslist54252$ae518431, %struct.ScmObj** %stackaddr$prim55099, align 8
%stackaddr$prim55100 = alloca %struct.ScmObj*, align 8
%argslist54252$ae518432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51844, %struct.ScmObj* %argslist54252$ae518431)
store volatile %struct.ScmObj* %argslist54252$ae518432, %struct.ScmObj** %stackaddr$prim55100, align 8
%clofunc55101 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51843)
musttail call tailcc void %clofunc55101(%struct.ScmObj* %ae51843, %struct.ScmObj* %argslist54252$ae518432)
ret void
}

define tailcc void @proc_clo$ae51843(%struct.ScmObj* %env$ae51843,%struct.ScmObj* %current_45args54217) {
%stackaddr$env-ref55102 = alloca %struct.ScmObj*, align 8
%anf_45bind48259 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51843, i64 0)
store %struct.ScmObj* %anf_45bind48259, %struct.ScmObj** %stackaddr$env-ref55102
%stackaddr$env-ref55103 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51843, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55103
%stackaddr$env-ref55104 = alloca %struct.ScmObj*, align 8
%_37map48072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51843, i64 2)
store %struct.ScmObj* %_37map48072, %struct.ScmObj** %stackaddr$env-ref55104
%stackaddr$prim55105 = alloca %struct.ScmObj*, align 8
%_95k48300 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54217)
store volatile %struct.ScmObj* %_95k48300, %struct.ScmObj** %stackaddr$prim55105, align 8
%stackaddr$prim55106 = alloca %struct.ScmObj*, align 8
%current_45args54218 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54217)
store volatile %struct.ScmObj* %current_45args54218, %struct.ScmObj** %stackaddr$prim55106, align 8
%stackaddr$prim55107 = alloca %struct.ScmObj*, align 8
%anf_45bind48261 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54218)
store volatile %struct.ScmObj* %anf_45bind48261, %struct.ScmObj** %stackaddr$prim55107, align 8
%stackaddr$makeclosure55108 = alloca %struct.ScmObj*, align 8
%fptrToInt55109 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51869 to i64
%ae51869 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55109)
store volatile %struct.ScmObj* %ae51869, %struct.ScmObj** %stackaddr$makeclosure55108, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51869, %struct.ScmObj* %anf_45bind48261, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51869, %struct.ScmObj* %anf_45bind48259, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51869, %struct.ScmObj* %_37foldl148030, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51869, %struct.ScmObj* %_37map48072, i64 3)
%ae51870 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55110 = alloca %struct.ScmObj*, align 8
%fptrToInt55111 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51871 to i64
%ae51871 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55111)
store volatile %struct.ScmObj* %ae51871, %struct.ScmObj** %stackaddr$makeclosure55110, align 8
%argslist54246$ae518690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55112 = alloca %struct.ScmObj*, align 8
%argslist54246$ae518691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51871, %struct.ScmObj* %argslist54246$ae518690)
store volatile %struct.ScmObj* %argslist54246$ae518691, %struct.ScmObj** %stackaddr$prim55112, align 8
%stackaddr$prim55113 = alloca %struct.ScmObj*, align 8
%argslist54246$ae518692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51870, %struct.ScmObj* %argslist54246$ae518691)
store volatile %struct.ScmObj* %argslist54246$ae518692, %struct.ScmObj** %stackaddr$prim55113, align 8
%clofunc55114 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51869)
musttail call tailcc void %clofunc55114(%struct.ScmObj* %ae51869, %struct.ScmObj* %argslist54246$ae518692)
ret void
}

define tailcc void @proc_clo$ae51869(%struct.ScmObj* %env$ae51869,%struct.ScmObj* %current_45args54220) {
%stackaddr$env-ref55115 = alloca %struct.ScmObj*, align 8
%anf_45bind48261 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51869, i64 0)
store %struct.ScmObj* %anf_45bind48261, %struct.ScmObj** %stackaddr$env-ref55115
%stackaddr$env-ref55116 = alloca %struct.ScmObj*, align 8
%anf_45bind48259 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51869, i64 1)
store %struct.ScmObj* %anf_45bind48259, %struct.ScmObj** %stackaddr$env-ref55116
%stackaddr$env-ref55117 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51869, i64 2)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55117
%stackaddr$env-ref55118 = alloca %struct.ScmObj*, align 8
%_37map48072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51869, i64 3)
store %struct.ScmObj* %_37map48072, %struct.ScmObj** %stackaddr$env-ref55118
%stackaddr$prim55119 = alloca %struct.ScmObj*, align 8
%_95k48301 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54220)
store volatile %struct.ScmObj* %_95k48301, %struct.ScmObj** %stackaddr$prim55119, align 8
%stackaddr$prim55120 = alloca %struct.ScmObj*, align 8
%current_45args54221 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54220)
store volatile %struct.ScmObj* %current_45args54221, %struct.ScmObj** %stackaddr$prim55120, align 8
%stackaddr$prim55121 = alloca %struct.ScmObj*, align 8
%anf_45bind48262 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54221)
store volatile %struct.ScmObj* %anf_45bind48262, %struct.ScmObj** %stackaddr$prim55121, align 8
%stackaddr$makeclosure55122 = alloca %struct.ScmObj*, align 8
%fptrToInt55123 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51892 to i64
%ae51892 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55123)
store volatile %struct.ScmObj* %ae51892, %struct.ScmObj** %stackaddr$makeclosure55122, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51892, %struct.ScmObj* %anf_45bind48261, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51892, %struct.ScmObj* %anf_45bind48259, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51892, %struct.ScmObj* %_37foldl148030, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51892, %struct.ScmObj* %_37map48072, i64 3)
%ae51893 = call %struct.ScmObj* @const_init_int(i64 2)
%ae51894 = call %struct.ScmObj* @const_init_int(i64 4)
%ae51895 = call %struct.ScmObj* @const_init_int(i64 6)
%argslist54244$anf_45bind482620 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55124 = alloca %struct.ScmObj*, align 8
%argslist54244$anf_45bind482621 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51895, %struct.ScmObj* %argslist54244$anf_45bind482620)
store volatile %struct.ScmObj* %argslist54244$anf_45bind482621, %struct.ScmObj** %stackaddr$prim55124, align 8
%stackaddr$prim55125 = alloca %struct.ScmObj*, align 8
%argslist54244$anf_45bind482622 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51894, %struct.ScmObj* %argslist54244$anf_45bind482621)
store volatile %struct.ScmObj* %argslist54244$anf_45bind482622, %struct.ScmObj** %stackaddr$prim55125, align 8
%stackaddr$prim55126 = alloca %struct.ScmObj*, align 8
%argslist54244$anf_45bind482623 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51893, %struct.ScmObj* %argslist54244$anf_45bind482622)
store volatile %struct.ScmObj* %argslist54244$anf_45bind482623, %struct.ScmObj** %stackaddr$prim55126, align 8
%stackaddr$prim55127 = alloca %struct.ScmObj*, align 8
%argslist54244$anf_45bind482624 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51892, %struct.ScmObj* %argslist54244$anf_45bind482623)
store volatile %struct.ScmObj* %argslist54244$anf_45bind482624, %struct.ScmObj** %stackaddr$prim55127, align 8
%clofunc55128 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48262)
musttail call tailcc void %clofunc55128(%struct.ScmObj* %anf_45bind48262, %struct.ScmObj* %argslist54244$anf_45bind482624)
ret void
}

define tailcc void @proc_clo$ae51892(%struct.ScmObj* %env$ae51892,%struct.ScmObj* %current_45args54223) {
%stackaddr$env-ref55129 = alloca %struct.ScmObj*, align 8
%anf_45bind48261 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51892, i64 0)
store %struct.ScmObj* %anf_45bind48261, %struct.ScmObj** %stackaddr$env-ref55129
%stackaddr$env-ref55130 = alloca %struct.ScmObj*, align 8
%anf_45bind48259 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51892, i64 1)
store %struct.ScmObj* %anf_45bind48259, %struct.ScmObj** %stackaddr$env-ref55130
%stackaddr$env-ref55131 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51892, i64 2)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55131
%stackaddr$env-ref55132 = alloca %struct.ScmObj*, align 8
%_37map48072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51892, i64 3)
store %struct.ScmObj* %_37map48072, %struct.ScmObj** %stackaddr$env-ref55132
%stackaddr$prim55133 = alloca %struct.ScmObj*, align 8
%_95k48302 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54223)
store volatile %struct.ScmObj* %_95k48302, %struct.ScmObj** %stackaddr$prim55133, align 8
%stackaddr$prim55134 = alloca %struct.ScmObj*, align 8
%current_45args54224 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54223)
store volatile %struct.ScmObj* %current_45args54224, %struct.ScmObj** %stackaddr$prim55134, align 8
%stackaddr$prim55135 = alloca %struct.ScmObj*, align 8
%anf_45bind48263 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54224)
store volatile %struct.ScmObj* %anf_45bind48263, %struct.ScmObj** %stackaddr$prim55135, align 8
%stackaddr$makeclosure55136 = alloca %struct.ScmObj*, align 8
%fptrToInt55137 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51911 to i64
%ae51911 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55137)
store volatile %struct.ScmObj* %ae51911, %struct.ScmObj** %stackaddr$makeclosure55136, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51911, %struct.ScmObj* %anf_45bind48263, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51911, %struct.ScmObj* %anf_45bind48261, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51911, %struct.ScmObj* %anf_45bind48259, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51911, %struct.ScmObj* %_37foldl148030, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51911, %struct.ScmObj* %_37map48072, i64 4)
%ae51912 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55138 = alloca %struct.ScmObj*, align 8
%fptrToInt55139 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51913 to i64
%ae51913 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55139)
store volatile %struct.ScmObj* %ae51913, %struct.ScmObj** %stackaddr$makeclosure55138, align 8
%argslist54243$ae519110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55140 = alloca %struct.ScmObj*, align 8
%argslist54243$ae519111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51913, %struct.ScmObj* %argslist54243$ae519110)
store volatile %struct.ScmObj* %argslist54243$ae519111, %struct.ScmObj** %stackaddr$prim55140, align 8
%stackaddr$prim55141 = alloca %struct.ScmObj*, align 8
%argslist54243$ae519112 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51912, %struct.ScmObj* %argslist54243$ae519111)
store volatile %struct.ScmObj* %argslist54243$ae519112, %struct.ScmObj** %stackaddr$prim55141, align 8
%clofunc55142 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51911)
musttail call tailcc void %clofunc55142(%struct.ScmObj* %ae51911, %struct.ScmObj* %argslist54243$ae519112)
ret void
}

define tailcc void @proc_clo$ae51911(%struct.ScmObj* %env$ae51911,%struct.ScmObj* %current_45args54226) {
%stackaddr$env-ref55143 = alloca %struct.ScmObj*, align 8
%anf_45bind48263 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51911, i64 0)
store %struct.ScmObj* %anf_45bind48263, %struct.ScmObj** %stackaddr$env-ref55143
%stackaddr$env-ref55144 = alloca %struct.ScmObj*, align 8
%anf_45bind48261 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51911, i64 1)
store %struct.ScmObj* %anf_45bind48261, %struct.ScmObj** %stackaddr$env-ref55144
%stackaddr$env-ref55145 = alloca %struct.ScmObj*, align 8
%anf_45bind48259 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51911, i64 2)
store %struct.ScmObj* %anf_45bind48259, %struct.ScmObj** %stackaddr$env-ref55145
%stackaddr$env-ref55146 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51911, i64 3)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55146
%stackaddr$env-ref55147 = alloca %struct.ScmObj*, align 8
%_37map48072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51911, i64 4)
store %struct.ScmObj* %_37map48072, %struct.ScmObj** %stackaddr$env-ref55147
%stackaddr$prim55148 = alloca %struct.ScmObj*, align 8
%_95k48303 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54226)
store volatile %struct.ScmObj* %_95k48303, %struct.ScmObj** %stackaddr$prim55148, align 8
%stackaddr$prim55149 = alloca %struct.ScmObj*, align 8
%current_45args54227 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54226)
store volatile %struct.ScmObj* %current_45args54227, %struct.ScmObj** %stackaddr$prim55149, align 8
%stackaddr$prim55150 = alloca %struct.ScmObj*, align 8
%anf_45bind48264 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54227)
store volatile %struct.ScmObj* %anf_45bind48264, %struct.ScmObj** %stackaddr$prim55150, align 8
%stackaddr$makeclosure55151 = alloca %struct.ScmObj*, align 8
%fptrToInt55152 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51934 to i64
%ae51934 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55152)
store volatile %struct.ScmObj* %ae51934, %struct.ScmObj** %stackaddr$makeclosure55151, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51934, %struct.ScmObj* %anf_45bind48263, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51934, %struct.ScmObj* %anf_45bind48261, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51934, %struct.ScmObj* %anf_45bind48259, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51934, %struct.ScmObj* %_37foldl148030, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51934, %struct.ScmObj* %_37map48072, i64 4)
%ae51935 = call %struct.ScmObj* @const_init_int(i64 7)
%ae51936 = call %struct.ScmObj* @const_init_int(i64 8)
%ae51937 = call %struct.ScmObj* @const_init_int(i64 9)
%argslist54241$anf_45bind482640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55153 = alloca %struct.ScmObj*, align 8
%argslist54241$anf_45bind482641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51937, %struct.ScmObj* %argslist54241$anf_45bind482640)
store volatile %struct.ScmObj* %argslist54241$anf_45bind482641, %struct.ScmObj** %stackaddr$prim55153, align 8
%stackaddr$prim55154 = alloca %struct.ScmObj*, align 8
%argslist54241$anf_45bind482642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51936, %struct.ScmObj* %argslist54241$anf_45bind482641)
store volatile %struct.ScmObj* %argslist54241$anf_45bind482642, %struct.ScmObj** %stackaddr$prim55154, align 8
%stackaddr$prim55155 = alloca %struct.ScmObj*, align 8
%argslist54241$anf_45bind482643 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51935, %struct.ScmObj* %argslist54241$anf_45bind482642)
store volatile %struct.ScmObj* %argslist54241$anf_45bind482643, %struct.ScmObj** %stackaddr$prim55155, align 8
%stackaddr$prim55156 = alloca %struct.ScmObj*, align 8
%argslist54241$anf_45bind482644 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51934, %struct.ScmObj* %argslist54241$anf_45bind482643)
store volatile %struct.ScmObj* %argslist54241$anf_45bind482644, %struct.ScmObj** %stackaddr$prim55156, align 8
%clofunc55157 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48264)
musttail call tailcc void %clofunc55157(%struct.ScmObj* %anf_45bind48264, %struct.ScmObj* %argslist54241$anf_45bind482644)
ret void
}

define tailcc void @proc_clo$ae51934(%struct.ScmObj* %env$ae51934,%struct.ScmObj* %current_45args54229) {
%stackaddr$env-ref55158 = alloca %struct.ScmObj*, align 8
%anf_45bind48263 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51934, i64 0)
store %struct.ScmObj* %anf_45bind48263, %struct.ScmObj** %stackaddr$env-ref55158
%stackaddr$env-ref55159 = alloca %struct.ScmObj*, align 8
%anf_45bind48261 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51934, i64 1)
store %struct.ScmObj* %anf_45bind48261, %struct.ScmObj** %stackaddr$env-ref55159
%stackaddr$env-ref55160 = alloca %struct.ScmObj*, align 8
%anf_45bind48259 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51934, i64 2)
store %struct.ScmObj* %anf_45bind48259, %struct.ScmObj** %stackaddr$env-ref55160
%stackaddr$env-ref55161 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51934, i64 3)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55161
%stackaddr$env-ref55162 = alloca %struct.ScmObj*, align 8
%_37map48072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51934, i64 4)
store %struct.ScmObj* %_37map48072, %struct.ScmObj** %stackaddr$env-ref55162
%stackaddr$prim55163 = alloca %struct.ScmObj*, align 8
%_95k48304 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54229)
store volatile %struct.ScmObj* %_95k48304, %struct.ScmObj** %stackaddr$prim55163, align 8
%stackaddr$prim55164 = alloca %struct.ScmObj*, align 8
%current_45args54230 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54229)
store volatile %struct.ScmObj* %current_45args54230, %struct.ScmObj** %stackaddr$prim55164, align 8
%stackaddr$prim55165 = alloca %struct.ScmObj*, align 8
%anf_45bind48265 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54230)
store volatile %struct.ScmObj* %anf_45bind48265, %struct.ScmObj** %stackaddr$prim55165, align 8
%stackaddr$makeclosure55166 = alloca %struct.ScmObj*, align 8
%fptrToInt55167 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51954 to i64
%ae51954 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55167)
store volatile %struct.ScmObj* %ae51954, %struct.ScmObj** %stackaddr$makeclosure55166, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51954, %struct.ScmObj* %anf_45bind48259, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51954, %struct.ScmObj* %_37foldl148030, i64 1)
%argslist54240$_37map480720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55168 = alloca %struct.ScmObj*, align 8
%argslist54240$_37map480721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48265, %struct.ScmObj* %argslist54240$_37map480720)
store volatile %struct.ScmObj* %argslist54240$_37map480721, %struct.ScmObj** %stackaddr$prim55168, align 8
%stackaddr$prim55169 = alloca %struct.ScmObj*, align 8
%argslist54240$_37map480722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48263, %struct.ScmObj* %argslist54240$_37map480721)
store volatile %struct.ScmObj* %argslist54240$_37map480722, %struct.ScmObj** %stackaddr$prim55169, align 8
%stackaddr$prim55170 = alloca %struct.ScmObj*, align 8
%argslist54240$_37map480723 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48261, %struct.ScmObj* %argslist54240$_37map480722)
store volatile %struct.ScmObj* %argslist54240$_37map480723, %struct.ScmObj** %stackaddr$prim55170, align 8
%stackaddr$prim55171 = alloca %struct.ScmObj*, align 8
%argslist54240$_37map480724 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51954, %struct.ScmObj* %argslist54240$_37map480723)
store volatile %struct.ScmObj* %argslist54240$_37map480724, %struct.ScmObj** %stackaddr$prim55171, align 8
%clofunc55172 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map48072)
musttail call tailcc void %clofunc55172(%struct.ScmObj* %_37map48072, %struct.ScmObj* %argslist54240$_37map480724)
ret void
}

define tailcc void @proc_clo$ae51954(%struct.ScmObj* %env$ae51954,%struct.ScmObj* %current_45args54232) {
%stackaddr$env-ref55173 = alloca %struct.ScmObj*, align 8
%anf_45bind48259 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51954, i64 0)
store %struct.ScmObj* %anf_45bind48259, %struct.ScmObj** %stackaddr$env-ref55173
%stackaddr$env-ref55174 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51954, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55174
%stackaddr$prim55175 = alloca %struct.ScmObj*, align 8
%_95k48305 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54232)
store volatile %struct.ScmObj* %_95k48305, %struct.ScmObj** %stackaddr$prim55175, align 8
%stackaddr$prim55176 = alloca %struct.ScmObj*, align 8
%current_45args54233 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54232)
store volatile %struct.ScmObj* %current_45args54233, %struct.ScmObj** %stackaddr$prim55176, align 8
%stackaddr$prim55177 = alloca %struct.ScmObj*, align 8
%anf_45bind48266 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54233)
store volatile %struct.ScmObj* %anf_45bind48266, %struct.ScmObj** %stackaddr$prim55177, align 8
%stackaddr$makeclosure55178 = alloca %struct.ScmObj*, align 8
%fptrToInt55179 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51959 to i64
%ae51959 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55179)
store volatile %struct.ScmObj* %ae51959, %struct.ScmObj** %stackaddr$makeclosure55178, align 8
%ae51961 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54239$_37foldl1480300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55180 = alloca %struct.ScmObj*, align 8
%argslist54239$_37foldl1480301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48266, %struct.ScmObj* %argslist54239$_37foldl1480300)
store volatile %struct.ScmObj* %argslist54239$_37foldl1480301, %struct.ScmObj** %stackaddr$prim55180, align 8
%stackaddr$prim55181 = alloca %struct.ScmObj*, align 8
%argslist54239$_37foldl1480302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51961, %struct.ScmObj* %argslist54239$_37foldl1480301)
store volatile %struct.ScmObj* %argslist54239$_37foldl1480302, %struct.ScmObj** %stackaddr$prim55181, align 8
%stackaddr$prim55182 = alloca %struct.ScmObj*, align 8
%argslist54239$_37foldl1480303 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48259, %struct.ScmObj* %argslist54239$_37foldl1480302)
store volatile %struct.ScmObj* %argslist54239$_37foldl1480303, %struct.ScmObj** %stackaddr$prim55182, align 8
%stackaddr$prim55183 = alloca %struct.ScmObj*, align 8
%argslist54239$_37foldl1480304 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51959, %struct.ScmObj* %argslist54239$_37foldl1480303)
store volatile %struct.ScmObj* %argslist54239$_37foldl1480304, %struct.ScmObj** %stackaddr$prim55183, align 8
%clofunc55184 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148030)
musttail call tailcc void %clofunc55184(%struct.ScmObj* %_37foldl148030, %struct.ScmObj* %argslist54239$_37foldl1480304)
ret void
}

define tailcc void @proc_clo$ae51959(%struct.ScmObj* %env$ae51959,%struct.ScmObj* %current_45args54235) {
%stackaddr$prim55185 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54235)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim55185, align 8
%stackaddr$prim55186 = alloca %struct.ScmObj*, align 8
%current_45args54236 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54235)
store volatile %struct.ScmObj* %current_45args54236, %struct.ScmObj** %stackaddr$prim55186, align 8
%stackaddr$prim55187 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54236)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim55187, align 8
%stackaddr$prim55188 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim55188, align 8
%argslist54238$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55189 = alloca %struct.ScmObj*, align 8
%argslist54238$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist54238$k0)
store volatile %struct.ScmObj* %argslist54238$k1, %struct.ScmObj** %stackaddr$prim55189, align 8
%clofunc55190 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc55190(%struct.ScmObj* %k, %struct.ScmObj* %argslist54238$k1)
ret void
}

define tailcc void @proc_clo$ae51913(%struct.ScmObj* %env$ae51913,%struct.ScmObj* %lst4814748306) {
%stackaddr$prim55191 = alloca %struct.ScmObj*, align 8
%k48307 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4814748306)
store volatile %struct.ScmObj* %k48307, %struct.ScmObj** %stackaddr$prim55191, align 8
%stackaddr$prim55192 = alloca %struct.ScmObj*, align 8
%lst48147 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4814748306)
store volatile %struct.ScmObj* %lst48147, %struct.ScmObj** %stackaddr$prim55192, align 8
%ae51917 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54242$k483070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55193 = alloca %struct.ScmObj*, align 8
%argslist54242$k483071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48147, %struct.ScmObj* %argslist54242$k483070)
store volatile %struct.ScmObj* %argslist54242$k483071, %struct.ScmObj** %stackaddr$prim55193, align 8
%stackaddr$prim55194 = alloca %struct.ScmObj*, align 8
%argslist54242$k483072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51917, %struct.ScmObj* %argslist54242$k483071)
store volatile %struct.ScmObj* %argslist54242$k483072, %struct.ScmObj** %stackaddr$prim55194, align 8
%clofunc55195 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48307)
musttail call tailcc void %clofunc55195(%struct.ScmObj* %k48307, %struct.ScmObj* %argslist54242$k483072)
ret void
}

define tailcc void @proc_clo$ae51871(%struct.ScmObj* %env$ae51871,%struct.ScmObj* %lst4814648308) {
%stackaddr$prim55196 = alloca %struct.ScmObj*, align 8
%k48309 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4814648308)
store volatile %struct.ScmObj* %k48309, %struct.ScmObj** %stackaddr$prim55196, align 8
%stackaddr$prim55197 = alloca %struct.ScmObj*, align 8
%lst48146 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4814648308)
store volatile %struct.ScmObj* %lst48146, %struct.ScmObj** %stackaddr$prim55197, align 8
%ae51875 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54245$k483090 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55198 = alloca %struct.ScmObj*, align 8
%argslist54245$k483091 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48146, %struct.ScmObj* %argslist54245$k483090)
store volatile %struct.ScmObj* %argslist54245$k483091, %struct.ScmObj** %stackaddr$prim55198, align 8
%stackaddr$prim55199 = alloca %struct.ScmObj*, align 8
%argslist54245$k483092 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51875, %struct.ScmObj* %argslist54245$k483091)
store volatile %struct.ScmObj* %argslist54245$k483092, %struct.ScmObj** %stackaddr$prim55199, align 8
%clofunc55200 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48309)
musttail call tailcc void %clofunc55200(%struct.ScmObj* %k48309, %struct.ScmObj* %argslist54245$k483092)
ret void
}

define tailcc void @proc_clo$ae51845(%struct.ScmObj* %env$ae51845,%struct.ScmObj* %current_45args54247) {
%stackaddr$prim55201 = alloca %struct.ScmObj*, align 8
%k48310 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54247)
store volatile %struct.ScmObj* %k48310, %struct.ScmObj** %stackaddr$prim55201, align 8
%stackaddr$prim55202 = alloca %struct.ScmObj*, align 8
%current_45args54248 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54247)
store volatile %struct.ScmObj* %current_45args54248, %struct.ScmObj** %stackaddr$prim55202, align 8
%stackaddr$prim55203 = alloca %struct.ScmObj*, align 8
%x48145 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54248)
store volatile %struct.ScmObj* %x48145, %struct.ScmObj** %stackaddr$prim55203, align 8
%stackaddr$prim55204 = alloca %struct.ScmObj*, align 8
%current_45args54249 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54248)
store volatile %struct.ScmObj* %current_45args54249, %struct.ScmObj** %stackaddr$prim55204, align 8
%stackaddr$prim55205 = alloca %struct.ScmObj*, align 8
%y48144 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54249)
store volatile %struct.ScmObj* %y48144, %struct.ScmObj** %stackaddr$prim55205, align 8
%stackaddr$prim55206 = alloca %struct.ScmObj*, align 8
%anf_45bind48260 = call %struct.ScmObj* @prim__42(%struct.ScmObj* %x48145, %struct.ScmObj* %x48145)
store volatile %struct.ScmObj* %anf_45bind48260, %struct.ScmObj** %stackaddr$prim55206, align 8
%stackaddr$prim55207 = alloca %struct.ScmObj*, align 8
%cpsprim48311 = call %struct.ScmObj* @prim__42(%struct.ScmObj* %y48144, %struct.ScmObj* %anf_45bind48260)
store volatile %struct.ScmObj* %cpsprim48311, %struct.ScmObj** %stackaddr$prim55207, align 8
%ae51851 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54251$k483100 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55208 = alloca %struct.ScmObj*, align 8
%argslist54251$k483101 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48311, %struct.ScmObj* %argslist54251$k483100)
store volatile %struct.ScmObj* %argslist54251$k483101, %struct.ScmObj** %stackaddr$prim55208, align 8
%stackaddr$prim55209 = alloca %struct.ScmObj*, align 8
%argslist54251$k483102 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51851, %struct.ScmObj* %argslist54251$k483101)
store volatile %struct.ScmObj* %argslist54251$k483102, %struct.ScmObj** %stackaddr$prim55209, align 8
%clofunc55210 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48310)
musttail call tailcc void %clofunc55210(%struct.ScmObj* %k48310, %struct.ScmObj* %argslist54251$k483102)
ret void
}

define tailcc void @proc_clo$ae51821(%struct.ScmObj* %env$ae51821,%struct.ScmObj* %args4814348312) {
%stackaddr$prim55211 = alloca %struct.ScmObj*, align 8
%k48313 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4814348312)
store volatile %struct.ScmObj* %k48313, %struct.ScmObj** %stackaddr$prim55211, align 8
%stackaddr$prim55212 = alloca %struct.ScmObj*, align 8
%args48143 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4814348312)
store volatile %struct.ScmObj* %args48143, %struct.ScmObj** %stackaddr$prim55212, align 8
%stackaddr$applyprim55213 = alloca %struct.ScmObj*, align 8
%cpsaprim48314 = call %struct.ScmObj* @applyprim__43(%struct.ScmObj* %args48143)
store volatile %struct.ScmObj* %cpsaprim48314, %struct.ScmObj** %stackaddr$applyprim55213, align 8
%ae51826 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54253$k483130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55214 = alloca %struct.ScmObj*, align 8
%argslist54253$k483131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsaprim48314, %struct.ScmObj* %argslist54253$k483130)
store volatile %struct.ScmObj* %argslist54253$k483131, %struct.ScmObj** %stackaddr$prim55214, align 8
%stackaddr$prim55215 = alloca %struct.ScmObj*, align 8
%argslist54253$k483132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51826, %struct.ScmObj* %argslist54253$k483131)
store volatile %struct.ScmObj* %argslist54253$k483132, %struct.ScmObj** %stackaddr$prim55215, align 8
%clofunc55216 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48313)
musttail call tailcc void %clofunc55216(%struct.ScmObj* %k48313, %struct.ScmObj* %argslist54253$k483132)
ret void
}

define tailcc void @proc_clo$ae51795(%struct.ScmObj* %env$ae51795,%struct.ScmObj* %current_45args54255) {
%stackaddr$prim55217 = alloca %struct.ScmObj*, align 8
%k48315 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54255)
store volatile %struct.ScmObj* %k48315, %struct.ScmObj** %stackaddr$prim55217, align 8
%stackaddr$prim55218 = alloca %struct.ScmObj*, align 8
%current_45args54256 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54255)
store volatile %struct.ScmObj* %current_45args54256, %struct.ScmObj** %stackaddr$prim55218, align 8
%stackaddr$prim55219 = alloca %struct.ScmObj*, align 8
%x48083 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54256)
store volatile %struct.ScmObj* %x48083, %struct.ScmObj** %stackaddr$prim55219, align 8
%stackaddr$prim55220 = alloca %struct.ScmObj*, align 8
%anf_45bind48256 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48083)
store volatile %struct.ScmObj* %anf_45bind48256, %struct.ScmObj** %stackaddr$prim55220, align 8
%stackaddr$prim55221 = alloca %struct.ScmObj*, align 8
%anf_45bind48257 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48256)
store volatile %struct.ScmObj* %anf_45bind48257, %struct.ScmObj** %stackaddr$prim55221, align 8
%stackaddr$prim55222 = alloca %struct.ScmObj*, align 8
%anf_45bind48258 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48257)
store volatile %struct.ScmObj* %anf_45bind48258, %struct.ScmObj** %stackaddr$prim55222, align 8
%stackaddr$prim55223 = alloca %struct.ScmObj*, align 8
%cpsprim48316 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48258)
store volatile %struct.ScmObj* %cpsprim48316, %struct.ScmObj** %stackaddr$prim55223, align 8
%ae51801 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54258$k483150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55224 = alloca %struct.ScmObj*, align 8
%argslist54258$k483151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48316, %struct.ScmObj* %argslist54258$k483150)
store volatile %struct.ScmObj* %argslist54258$k483151, %struct.ScmObj** %stackaddr$prim55224, align 8
%stackaddr$prim55225 = alloca %struct.ScmObj*, align 8
%argslist54258$k483152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51801, %struct.ScmObj* %argslist54258$k483151)
store volatile %struct.ScmObj* %argslist54258$k483152, %struct.ScmObj** %stackaddr$prim55225, align 8
%clofunc55226 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48315)
musttail call tailcc void %clofunc55226(%struct.ScmObj* %k48315, %struct.ScmObj* %argslist54258$k483152)
ret void
}

define tailcc void @proc_clo$ae51771(%struct.ScmObj* %env$ae51771,%struct.ScmObj* %current_45args54260) {
%stackaddr$prim55227 = alloca %struct.ScmObj*, align 8
%k48317 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54260)
store volatile %struct.ScmObj* %k48317, %struct.ScmObj** %stackaddr$prim55227, align 8
%stackaddr$prim55228 = alloca %struct.ScmObj*, align 8
%current_45args54261 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54260)
store volatile %struct.ScmObj* %current_45args54261, %struct.ScmObj** %stackaddr$prim55228, align 8
%stackaddr$prim55229 = alloca %struct.ScmObj*, align 8
%x48085 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54261)
store volatile %struct.ScmObj* %x48085, %struct.ScmObj** %stackaddr$prim55229, align 8
%stackaddr$prim55230 = alloca %struct.ScmObj*, align 8
%anf_45bind48254 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48085)
store volatile %struct.ScmObj* %anf_45bind48254, %struct.ScmObj** %stackaddr$prim55230, align 8
%stackaddr$prim55231 = alloca %struct.ScmObj*, align 8
%anf_45bind48255 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48254)
store volatile %struct.ScmObj* %anf_45bind48255, %struct.ScmObj** %stackaddr$prim55231, align 8
%stackaddr$prim55232 = alloca %struct.ScmObj*, align 8
%cpsprim48318 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48255)
store volatile %struct.ScmObj* %cpsprim48318, %struct.ScmObj** %stackaddr$prim55232, align 8
%ae51776 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54263$k483170 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55233 = alloca %struct.ScmObj*, align 8
%argslist54263$k483171 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48318, %struct.ScmObj* %argslist54263$k483170)
store volatile %struct.ScmObj* %argslist54263$k483171, %struct.ScmObj** %stackaddr$prim55233, align 8
%stackaddr$prim55234 = alloca %struct.ScmObj*, align 8
%argslist54263$k483172 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51776, %struct.ScmObj* %argslist54263$k483171)
store volatile %struct.ScmObj* %argslist54263$k483172, %struct.ScmObj** %stackaddr$prim55234, align 8
%clofunc55235 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48317)
musttail call tailcc void %clofunc55235(%struct.ScmObj* %k48317, %struct.ScmObj* %argslist54263$k483172)
ret void
}

define tailcc void @proc_clo$ae51749(%struct.ScmObj* %env$ae51749,%struct.ScmObj* %current_45args54265) {
%stackaddr$prim55236 = alloca %struct.ScmObj*, align 8
%k48319 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54265)
store volatile %struct.ScmObj* %k48319, %struct.ScmObj** %stackaddr$prim55236, align 8
%stackaddr$prim55237 = alloca %struct.ScmObj*, align 8
%current_45args54266 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54265)
store volatile %struct.ScmObj* %current_45args54266, %struct.ScmObj** %stackaddr$prim55237, align 8
%stackaddr$prim55238 = alloca %struct.ScmObj*, align 8
%x48087 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54266)
store volatile %struct.ScmObj* %x48087, %struct.ScmObj** %stackaddr$prim55238, align 8
%stackaddr$prim55239 = alloca %struct.ScmObj*, align 8
%anf_45bind48253 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48087)
store volatile %struct.ScmObj* %anf_45bind48253, %struct.ScmObj** %stackaddr$prim55239, align 8
%stackaddr$prim55240 = alloca %struct.ScmObj*, align 8
%cpsprim48320 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48253)
store volatile %struct.ScmObj* %cpsprim48320, %struct.ScmObj** %stackaddr$prim55240, align 8
%ae51753 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54268$k483190 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55241 = alloca %struct.ScmObj*, align 8
%argslist54268$k483191 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48320, %struct.ScmObj* %argslist54268$k483190)
store volatile %struct.ScmObj* %argslist54268$k483191, %struct.ScmObj** %stackaddr$prim55241, align 8
%stackaddr$prim55242 = alloca %struct.ScmObj*, align 8
%argslist54268$k483192 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51753, %struct.ScmObj* %argslist54268$k483191)
store volatile %struct.ScmObj* %argslist54268$k483192, %struct.ScmObj** %stackaddr$prim55242, align 8
%clofunc55243 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48319)
musttail call tailcc void %clofunc55243(%struct.ScmObj* %k48319, %struct.ScmObj* %argslist54268$k483192)
ret void
}

define tailcc void @proc_clo$ae51729(%struct.ScmObj* %env$ae51729,%struct.ScmObj* %current_45args54270) {
%stackaddr$prim55244 = alloca %struct.ScmObj*, align 8
%k48321 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54270)
store volatile %struct.ScmObj* %k48321, %struct.ScmObj** %stackaddr$prim55244, align 8
%stackaddr$prim55245 = alloca %struct.ScmObj*, align 8
%current_45args54271 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54270)
store volatile %struct.ScmObj* %current_45args54271, %struct.ScmObj** %stackaddr$prim55245, align 8
%stackaddr$prim55246 = alloca %struct.ScmObj*, align 8
%x48089 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54271)
store volatile %struct.ScmObj* %x48089, %struct.ScmObj** %stackaddr$prim55246, align 8
%stackaddr$prim55247 = alloca %struct.ScmObj*, align 8
%cpsprim48322 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48089)
store volatile %struct.ScmObj* %cpsprim48322, %struct.ScmObj** %stackaddr$prim55247, align 8
%ae51732 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54273$k483210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55248 = alloca %struct.ScmObj*, align 8
%argslist54273$k483211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48322, %struct.ScmObj* %argslist54273$k483210)
store volatile %struct.ScmObj* %argslist54273$k483211, %struct.ScmObj** %stackaddr$prim55248, align 8
%stackaddr$prim55249 = alloca %struct.ScmObj*, align 8
%argslist54273$k483212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51732, %struct.ScmObj* %argslist54273$k483211)
store volatile %struct.ScmObj* %argslist54273$k483212, %struct.ScmObj** %stackaddr$prim55249, align 8
%clofunc55250 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48321)
musttail call tailcc void %clofunc55250(%struct.ScmObj* %k48321, %struct.ScmObj* %argslist54273$k483212)
ret void
}

define tailcc void @proc_clo$ae51631(%struct.ScmObj* %env$ae51631,%struct.ScmObj* %args4809148323) {
%stackaddr$env-ref55251 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51631, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55251
%stackaddr$prim55252 = alloca %struct.ScmObj*, align 8
%k48324 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4809148323)
store volatile %struct.ScmObj* %k48324, %struct.ScmObj** %stackaddr$prim55252, align 8
%stackaddr$prim55253 = alloca %struct.ScmObj*, align 8
%args48091 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4809148323)
store volatile %struct.ScmObj* %args48091, %struct.ScmObj** %stackaddr$prim55253, align 8
%stackaddr$prim55254 = alloca %struct.ScmObj*, align 8
%anf_45bind48247 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args48091)
store volatile %struct.ScmObj* %anf_45bind48247, %struct.ScmObj** %stackaddr$prim55254, align 8
%truthy$cmp55255 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48247)
%cmp$cmp55255 = icmp eq i64 %truthy$cmp55255, 1
br i1 %cmp$cmp55255, label %truebranch$cmp55255, label %falsebranch$cmp55255
truebranch$cmp55255:
%ae51637 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51638 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist54275$k483240 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55256 = alloca %struct.ScmObj*, align 8
%argslist54275$k483241 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51638, %struct.ScmObj* %argslist54275$k483240)
store volatile %struct.ScmObj* %argslist54275$k483241, %struct.ScmObj** %stackaddr$prim55256, align 8
%stackaddr$prim55257 = alloca %struct.ScmObj*, align 8
%argslist54275$k483242 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51637, %struct.ScmObj* %argslist54275$k483241)
store volatile %struct.ScmObj* %argslist54275$k483242, %struct.ScmObj** %stackaddr$prim55257, align 8
%clofunc55258 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48324)
musttail call tailcc void %clofunc55258(%struct.ScmObj* %k48324, %struct.ScmObj* %argslist54275$k483242)
ret void
falsebranch$cmp55255:
%stackaddr$prim55259 = alloca %struct.ScmObj*, align 8
%anf_45bind48248 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48091)
store volatile %struct.ScmObj* %anf_45bind48248, %struct.ScmObj** %stackaddr$prim55259, align 8
%stackaddr$prim55260 = alloca %struct.ScmObj*, align 8
%anf_45bind48249 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48248)
store volatile %struct.ScmObj* %anf_45bind48249, %struct.ScmObj** %stackaddr$prim55260, align 8
%truthy$cmp55261 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48249)
%cmp$cmp55261 = icmp eq i64 %truthy$cmp55261, 1
br i1 %cmp$cmp55261, label %truebranch$cmp55261, label %falsebranch$cmp55261
truebranch$cmp55261:
%stackaddr$prim55262 = alloca %struct.ScmObj*, align 8
%cpsprim48325 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48091)
store volatile %struct.ScmObj* %cpsprim48325, %struct.ScmObj** %stackaddr$prim55262, align 8
%ae51650 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54276$k483240 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55263 = alloca %struct.ScmObj*, align 8
%argslist54276$k483241 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48325, %struct.ScmObj* %argslist54276$k483240)
store volatile %struct.ScmObj* %argslist54276$k483241, %struct.ScmObj** %stackaddr$prim55263, align 8
%stackaddr$prim55264 = alloca %struct.ScmObj*, align 8
%argslist54276$k483242 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51650, %struct.ScmObj* %argslist54276$k483241)
store volatile %struct.ScmObj* %argslist54276$k483242, %struct.ScmObj** %stackaddr$prim55264, align 8
%clofunc55265 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48324)
musttail call tailcc void %clofunc55265(%struct.ScmObj* %k48324, %struct.ScmObj* %argslist54276$k483242)
ret void
falsebranch$cmp55261:
%stackaddr$makeclosure55266 = alloca %struct.ScmObj*, align 8
%fptrToInt55267 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51655 to i64
%ae51655 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55267)
store volatile %struct.ScmObj* %ae51655, %struct.ScmObj** %stackaddr$makeclosure55266, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51655, %struct.ScmObj* %k48324, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51655, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51655, %struct.ScmObj* %args48091, i64 2)
%ae51656 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55268 = alloca %struct.ScmObj*, align 8
%fptrToInt55269 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51657 to i64
%ae51657 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55269)
store volatile %struct.ScmObj* %ae51657, %struct.ScmObj** %stackaddr$makeclosure55268, align 8
%argslist54286$ae516550 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55270 = alloca %struct.ScmObj*, align 8
%argslist54286$ae516551 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51657, %struct.ScmObj* %argslist54286$ae516550)
store volatile %struct.ScmObj* %argslist54286$ae516551, %struct.ScmObj** %stackaddr$prim55270, align 8
%stackaddr$prim55271 = alloca %struct.ScmObj*, align 8
%argslist54286$ae516552 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51656, %struct.ScmObj* %argslist54286$ae516551)
store volatile %struct.ScmObj* %argslist54286$ae516552, %struct.ScmObj** %stackaddr$prim55271, align 8
%clofunc55272 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51655)
musttail call tailcc void %clofunc55272(%struct.ScmObj* %ae51655, %struct.ScmObj* %argslist54286$ae516552)
ret void
}

define tailcc void @proc_clo$ae51655(%struct.ScmObj* %env$ae51655,%struct.ScmObj* %current_45args54277) {
%stackaddr$env-ref55273 = alloca %struct.ScmObj*, align 8
%k48324 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51655, i64 0)
store %struct.ScmObj* %k48324, %struct.ScmObj** %stackaddr$env-ref55273
%stackaddr$env-ref55274 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51655, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55274
%stackaddr$env-ref55275 = alloca %struct.ScmObj*, align 8
%args48091 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51655, i64 2)
store %struct.ScmObj* %args48091, %struct.ScmObj** %stackaddr$env-ref55275
%stackaddr$prim55276 = alloca %struct.ScmObj*, align 8
%_95k48326 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54277)
store volatile %struct.ScmObj* %_95k48326, %struct.ScmObj** %stackaddr$prim55276, align 8
%stackaddr$prim55277 = alloca %struct.ScmObj*, align 8
%current_45args54278 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54277)
store volatile %struct.ScmObj* %current_45args54278, %struct.ScmObj** %stackaddr$prim55277, align 8
%stackaddr$prim55278 = alloca %struct.ScmObj*, align 8
%anf_45bind48250 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54278)
store volatile %struct.ScmObj* %anf_45bind48250, %struct.ScmObj** %stackaddr$prim55278, align 8
%stackaddr$prim55279 = alloca %struct.ScmObj*, align 8
%anf_45bind48251 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48091)
store volatile %struct.ScmObj* %anf_45bind48251, %struct.ScmObj** %stackaddr$prim55279, align 8
%stackaddr$prim55280 = alloca %struct.ScmObj*, align 8
%anf_45bind48252 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48091)
store volatile %struct.ScmObj* %anf_45bind48252, %struct.ScmObj** %stackaddr$prim55280, align 8
%argslist54280$_37foldl1480300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55281 = alloca %struct.ScmObj*, align 8
%argslist54280$_37foldl1480301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48252, %struct.ScmObj* %argslist54280$_37foldl1480300)
store volatile %struct.ScmObj* %argslist54280$_37foldl1480301, %struct.ScmObj** %stackaddr$prim55281, align 8
%stackaddr$prim55282 = alloca %struct.ScmObj*, align 8
%argslist54280$_37foldl1480302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48251, %struct.ScmObj* %argslist54280$_37foldl1480301)
store volatile %struct.ScmObj* %argslist54280$_37foldl1480302, %struct.ScmObj** %stackaddr$prim55282, align 8
%stackaddr$prim55283 = alloca %struct.ScmObj*, align 8
%argslist54280$_37foldl1480303 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48250, %struct.ScmObj* %argslist54280$_37foldl1480302)
store volatile %struct.ScmObj* %argslist54280$_37foldl1480303, %struct.ScmObj** %stackaddr$prim55283, align 8
%stackaddr$prim55284 = alloca %struct.ScmObj*, align 8
%argslist54280$_37foldl1480304 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48324, %struct.ScmObj* %argslist54280$_37foldl1480303)
store volatile %struct.ScmObj* %argslist54280$_37foldl1480304, %struct.ScmObj** %stackaddr$prim55284, align 8
%clofunc55285 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148030)
musttail call tailcc void %clofunc55285(%struct.ScmObj* %_37foldl148030, %struct.ScmObj* %argslist54280$_37foldl1480304)
ret void
}

define tailcc void @proc_clo$ae51657(%struct.ScmObj* %env$ae51657,%struct.ScmObj* %current_45args54281) {
%stackaddr$prim55286 = alloca %struct.ScmObj*, align 8
%k48327 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54281)
store volatile %struct.ScmObj* %k48327, %struct.ScmObj** %stackaddr$prim55286, align 8
%stackaddr$prim55287 = alloca %struct.ScmObj*, align 8
%current_45args54282 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54281)
store volatile %struct.ScmObj* %current_45args54282, %struct.ScmObj** %stackaddr$prim55287, align 8
%stackaddr$prim55288 = alloca %struct.ScmObj*, align 8
%n48093 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54282)
store volatile %struct.ScmObj* %n48093, %struct.ScmObj** %stackaddr$prim55288, align 8
%stackaddr$prim55289 = alloca %struct.ScmObj*, align 8
%current_45args54283 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54282)
store volatile %struct.ScmObj* %current_45args54283, %struct.ScmObj** %stackaddr$prim55289, align 8
%stackaddr$prim55290 = alloca %struct.ScmObj*, align 8
%v48092 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54283)
store volatile %struct.ScmObj* %v48092, %struct.ScmObj** %stackaddr$prim55290, align 8
%stackaddr$prim55291 = alloca %struct.ScmObj*, align 8
%cpsprim48328 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v48092, %struct.ScmObj* %n48093)
store volatile %struct.ScmObj* %cpsprim48328, %struct.ScmObj** %stackaddr$prim55291, align 8
%ae51661 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54285$k483270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55292 = alloca %struct.ScmObj*, align 8
%argslist54285$k483271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48328, %struct.ScmObj* %argslist54285$k483270)
store volatile %struct.ScmObj* %argslist54285$k483271, %struct.ScmObj** %stackaddr$prim55292, align 8
%stackaddr$prim55293 = alloca %struct.ScmObj*, align 8
%argslist54285$k483272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51661, %struct.ScmObj* %argslist54285$k483271)
store volatile %struct.ScmObj* %argslist54285$k483272, %struct.ScmObj** %stackaddr$prim55293, align 8
%clofunc55294 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48327)
musttail call tailcc void %clofunc55294(%struct.ScmObj* %k48327, %struct.ScmObj* %argslist54285$k483272)
ret void
}

define tailcc void @proc_clo$ae51227(%struct.ScmObj* %env$ae51227,%struct.ScmObj* %current_45args54288) {
%stackaddr$prim55295 = alloca %struct.ScmObj*, align 8
%k48329 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54288)
store volatile %struct.ScmObj* %k48329, %struct.ScmObj** %stackaddr$prim55295, align 8
%stackaddr$prim55296 = alloca %struct.ScmObj*, align 8
%current_45args54289 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54288)
store volatile %struct.ScmObj* %current_45args54289, %struct.ScmObj** %stackaddr$prim55296, align 8
%stackaddr$prim55297 = alloca %struct.ScmObj*, align 8
%v48096 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54289)
store volatile %struct.ScmObj* %v48096, %struct.ScmObj** %stackaddr$prim55297, align 8
%stackaddr$prim55298 = alloca %struct.ScmObj*, align 8
%current_45args54290 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54289)
store volatile %struct.ScmObj* %current_45args54290, %struct.ScmObj** %stackaddr$prim55298, align 8
%stackaddr$prim55299 = alloca %struct.ScmObj*, align 8
%lst48095 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54290)
store volatile %struct.ScmObj* %lst48095, %struct.ScmObj** %stackaddr$prim55299, align 8
%ae51228 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55300 = alloca %struct.ScmObj*, align 8
%lst48097 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51228, %struct.ScmObj* %lst48095)
store volatile %struct.ScmObj* %lst48097, %struct.ScmObj** %stackaddr$prim55300, align 8
%stackaddr$makeclosure55301 = alloca %struct.ScmObj*, align 8
%fptrToInt55302 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51230 to i64
%ae51230 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55302)
store volatile %struct.ScmObj* %ae51230, %struct.ScmObj** %stackaddr$makeclosure55301, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51230, %struct.ScmObj* %lst48097, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51230, %struct.ScmObj* %v48096, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51230, %struct.ScmObj* %k48329, i64 2)
%ae51231 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55303 = alloca %struct.ScmObj*, align 8
%fptrToInt55304 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51232 to i64
%ae51232 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55304)
store volatile %struct.ScmObj* %ae51232, %struct.ScmObj** %stackaddr$makeclosure55303, align 8
%argslist54312$ae512300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55305 = alloca %struct.ScmObj*, align 8
%argslist54312$ae512301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51232, %struct.ScmObj* %argslist54312$ae512300)
store volatile %struct.ScmObj* %argslist54312$ae512301, %struct.ScmObj** %stackaddr$prim55305, align 8
%stackaddr$prim55306 = alloca %struct.ScmObj*, align 8
%argslist54312$ae512302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51231, %struct.ScmObj* %argslist54312$ae512301)
store volatile %struct.ScmObj* %argslist54312$ae512302, %struct.ScmObj** %stackaddr$prim55306, align 8
%clofunc55307 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51230)
musttail call tailcc void %clofunc55307(%struct.ScmObj* %ae51230, %struct.ScmObj* %argslist54312$ae512302)
ret void
}

define tailcc void @proc_clo$ae51230(%struct.ScmObj* %env$ae51230,%struct.ScmObj* %current_45args54292) {
%stackaddr$env-ref55308 = alloca %struct.ScmObj*, align 8
%lst48097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51230, i64 0)
store %struct.ScmObj* %lst48097, %struct.ScmObj** %stackaddr$env-ref55308
%stackaddr$env-ref55309 = alloca %struct.ScmObj*, align 8
%v48096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51230, i64 1)
store %struct.ScmObj* %v48096, %struct.ScmObj** %stackaddr$env-ref55309
%stackaddr$env-ref55310 = alloca %struct.ScmObj*, align 8
%k48329 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51230, i64 2)
store %struct.ScmObj* %k48329, %struct.ScmObj** %stackaddr$env-ref55310
%stackaddr$prim55311 = alloca %struct.ScmObj*, align 8
%_95k48330 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54292)
store volatile %struct.ScmObj* %_95k48330, %struct.ScmObj** %stackaddr$prim55311, align 8
%stackaddr$prim55312 = alloca %struct.ScmObj*, align 8
%current_45args54293 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54292)
store volatile %struct.ScmObj* %current_45args54293, %struct.ScmObj** %stackaddr$prim55312, align 8
%stackaddr$prim55313 = alloca %struct.ScmObj*, align 8
%anf_45bind48239 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54293)
store volatile %struct.ScmObj* %anf_45bind48239, %struct.ScmObj** %stackaddr$prim55313, align 8
%stackaddr$makeclosure55314 = alloca %struct.ScmObj*, align 8
%fptrToInt55315 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51246 to i64
%ae51246 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55315)
store volatile %struct.ScmObj* %ae51246, %struct.ScmObj** %stackaddr$makeclosure55314, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51246, %struct.ScmObj* %lst48097, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51246, %struct.ScmObj* %v48096, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51246, %struct.ScmObj* %k48329, i64 2)
%stackaddr$makeclosure55316 = alloca %struct.ScmObj*, align 8
%fptrToInt55317 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51247 to i64
%ae51247 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55317)
store volatile %struct.ScmObj* %ae51247, %struct.ScmObj** %stackaddr$makeclosure55316, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51247, %struct.ScmObj* %lst48097, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51247, %struct.ScmObj* %v48096, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51247, %struct.ScmObj* %k48329, i64 2)
%argslist54307$anf_45bind482390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55318 = alloca %struct.ScmObj*, align 8
%argslist54307$anf_45bind482391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51247, %struct.ScmObj* %argslist54307$anf_45bind482390)
store volatile %struct.ScmObj* %argslist54307$anf_45bind482391, %struct.ScmObj** %stackaddr$prim55318, align 8
%stackaddr$prim55319 = alloca %struct.ScmObj*, align 8
%argslist54307$anf_45bind482392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51246, %struct.ScmObj* %argslist54307$anf_45bind482391)
store volatile %struct.ScmObj* %argslist54307$anf_45bind482392, %struct.ScmObj** %stackaddr$prim55319, align 8
%clofunc55320 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48239)
musttail call tailcc void %clofunc55320(%struct.ScmObj* %anf_45bind48239, %struct.ScmObj* %argslist54307$anf_45bind482392)
ret void
}

define tailcc void @proc_clo$ae51246(%struct.ScmObj* %env$ae51246,%struct.ScmObj* %current_45args54295) {
%stackaddr$env-ref55321 = alloca %struct.ScmObj*, align 8
%lst48097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51246, i64 0)
store %struct.ScmObj* %lst48097, %struct.ScmObj** %stackaddr$env-ref55321
%stackaddr$env-ref55322 = alloca %struct.ScmObj*, align 8
%v48096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51246, i64 1)
store %struct.ScmObj* %v48096, %struct.ScmObj** %stackaddr$env-ref55322
%stackaddr$env-ref55323 = alloca %struct.ScmObj*, align 8
%k48329 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51246, i64 2)
store %struct.ScmObj* %k48329, %struct.ScmObj** %stackaddr$env-ref55323
%stackaddr$prim55324 = alloca %struct.ScmObj*, align 8
%_95k48331 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54295)
store volatile %struct.ScmObj* %_95k48331, %struct.ScmObj** %stackaddr$prim55324, align 8
%stackaddr$prim55325 = alloca %struct.ScmObj*, align 8
%current_45args54296 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54295)
store volatile %struct.ScmObj* %current_45args54296, %struct.ScmObj** %stackaddr$prim55325, align 8
%stackaddr$prim55326 = alloca %struct.ScmObj*, align 8
%cc48098 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54296)
store volatile %struct.ScmObj* %cc48098, %struct.ScmObj** %stackaddr$prim55326, align 8
%ae51355 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55327 = alloca %struct.ScmObj*, align 8
%anf_45bind48240 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51355)
store volatile %struct.ScmObj* %anf_45bind48240, %struct.ScmObj** %stackaddr$prim55327, align 8
%stackaddr$prim55328 = alloca %struct.ScmObj*, align 8
%anf_45bind48241 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48240)
store volatile %struct.ScmObj* %anf_45bind48241, %struct.ScmObj** %stackaddr$prim55328, align 8
%truthy$cmp55329 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48241)
%cmp$cmp55329 = icmp eq i64 %truthy$cmp55329, 1
br i1 %cmp$cmp55329, label %truebranch$cmp55329, label %falsebranch$cmp55329
truebranch$cmp55329:
%ae51359 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51360 = call %struct.ScmObj* @const_init_false()
%argslist54298$k483290 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55330 = alloca %struct.ScmObj*, align 8
%argslist54298$k483291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51360, %struct.ScmObj* %argslist54298$k483290)
store volatile %struct.ScmObj* %argslist54298$k483291, %struct.ScmObj** %stackaddr$prim55330, align 8
%stackaddr$prim55331 = alloca %struct.ScmObj*, align 8
%argslist54298$k483292 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51359, %struct.ScmObj* %argslist54298$k483291)
store volatile %struct.ScmObj* %argslist54298$k483292, %struct.ScmObj** %stackaddr$prim55331, align 8
%clofunc55332 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48329)
musttail call tailcc void %clofunc55332(%struct.ScmObj* %k48329, %struct.ScmObj* %argslist54298$k483292)
ret void
falsebranch$cmp55329:
%ae51368 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55333 = alloca %struct.ScmObj*, align 8
%anf_45bind48242 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51368)
store volatile %struct.ScmObj* %anf_45bind48242, %struct.ScmObj** %stackaddr$prim55333, align 8
%stackaddr$prim55334 = alloca %struct.ScmObj*, align 8
%anf_45bind48243 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48242)
store volatile %struct.ScmObj* %anf_45bind48243, %struct.ScmObj** %stackaddr$prim55334, align 8
%stackaddr$prim55335 = alloca %struct.ScmObj*, align 8
%anf_45bind48244 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind48243, %struct.ScmObj* %v48096)
store volatile %struct.ScmObj* %anf_45bind48244, %struct.ScmObj** %stackaddr$prim55335, align 8
%truthy$cmp55336 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48244)
%cmp$cmp55336 = icmp eq i64 %truthy$cmp55336, 1
br i1 %cmp$cmp55336, label %truebranch$cmp55336, label %falsebranch$cmp55336
truebranch$cmp55336:
%ae51374 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55337 = alloca %struct.ScmObj*, align 8
%cpsprim48332 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51374)
store volatile %struct.ScmObj* %cpsprim48332, %struct.ScmObj** %stackaddr$prim55337, align 8
%ae51376 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54299$k483290 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55338 = alloca %struct.ScmObj*, align 8
%argslist54299$k483291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48332, %struct.ScmObj* %argslist54299$k483290)
store volatile %struct.ScmObj* %argslist54299$k483291, %struct.ScmObj** %stackaddr$prim55338, align 8
%stackaddr$prim55339 = alloca %struct.ScmObj*, align 8
%argslist54299$k483292 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51376, %struct.ScmObj* %argslist54299$k483291)
store volatile %struct.ScmObj* %argslist54299$k483292, %struct.ScmObj** %stackaddr$prim55339, align 8
%clofunc55340 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48329)
musttail call tailcc void %clofunc55340(%struct.ScmObj* %k48329, %struct.ScmObj* %argslist54299$k483292)
ret void
falsebranch$cmp55336:
%ae51387 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55341 = alloca %struct.ScmObj*, align 8
%anf_45bind48245 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51387)
store volatile %struct.ScmObj* %anf_45bind48245, %struct.ScmObj** %stackaddr$prim55341, align 8
%stackaddr$prim55342 = alloca %struct.ScmObj*, align 8
%anf_45bind48246 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48245)
store volatile %struct.ScmObj* %anf_45bind48246, %struct.ScmObj** %stackaddr$prim55342, align 8
%ae51390 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55343 = alloca %struct.ScmObj*, align 8
%_95048100 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51390, %struct.ScmObj* %anf_45bind48246)
store volatile %struct.ScmObj* %_95048100, %struct.ScmObj** %stackaddr$prim55343, align 8
%argslist54300$cc480980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55344 = alloca %struct.ScmObj*, align 8
%argslist54300$cc480981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48098, %struct.ScmObj* %argslist54300$cc480980)
store volatile %struct.ScmObj* %argslist54300$cc480981, %struct.ScmObj** %stackaddr$prim55344, align 8
%stackaddr$prim55345 = alloca %struct.ScmObj*, align 8
%argslist54300$cc480982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48329, %struct.ScmObj* %argslist54300$cc480981)
store volatile %struct.ScmObj* %argslist54300$cc480982, %struct.ScmObj** %stackaddr$prim55345, align 8
%clofunc55346 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48098)
musttail call tailcc void %clofunc55346(%struct.ScmObj* %cc48098, %struct.ScmObj* %argslist54300$cc480982)
ret void
}

define tailcc void @proc_clo$ae51247(%struct.ScmObj* %env$ae51247,%struct.ScmObj* %current_45args54301) {
%stackaddr$env-ref55347 = alloca %struct.ScmObj*, align 8
%lst48097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51247, i64 0)
store %struct.ScmObj* %lst48097, %struct.ScmObj** %stackaddr$env-ref55347
%stackaddr$env-ref55348 = alloca %struct.ScmObj*, align 8
%v48096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51247, i64 1)
store %struct.ScmObj* %v48096, %struct.ScmObj** %stackaddr$env-ref55348
%stackaddr$env-ref55349 = alloca %struct.ScmObj*, align 8
%k48329 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51247, i64 2)
store %struct.ScmObj* %k48329, %struct.ScmObj** %stackaddr$env-ref55349
%stackaddr$prim55350 = alloca %struct.ScmObj*, align 8
%_95k48331 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54301)
store volatile %struct.ScmObj* %_95k48331, %struct.ScmObj** %stackaddr$prim55350, align 8
%stackaddr$prim55351 = alloca %struct.ScmObj*, align 8
%current_45args54302 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54301)
store volatile %struct.ScmObj* %current_45args54302, %struct.ScmObj** %stackaddr$prim55351, align 8
%stackaddr$prim55352 = alloca %struct.ScmObj*, align 8
%cc48098 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54302)
store volatile %struct.ScmObj* %cc48098, %struct.ScmObj** %stackaddr$prim55352, align 8
%ae51249 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55353 = alloca %struct.ScmObj*, align 8
%anf_45bind48240 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51249)
store volatile %struct.ScmObj* %anf_45bind48240, %struct.ScmObj** %stackaddr$prim55353, align 8
%stackaddr$prim55354 = alloca %struct.ScmObj*, align 8
%anf_45bind48241 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48240)
store volatile %struct.ScmObj* %anf_45bind48241, %struct.ScmObj** %stackaddr$prim55354, align 8
%truthy$cmp55355 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48241)
%cmp$cmp55355 = icmp eq i64 %truthy$cmp55355, 1
br i1 %cmp$cmp55355, label %truebranch$cmp55355, label %falsebranch$cmp55355
truebranch$cmp55355:
%ae51253 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51254 = call %struct.ScmObj* @const_init_false()
%argslist54304$k483290 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55356 = alloca %struct.ScmObj*, align 8
%argslist54304$k483291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51254, %struct.ScmObj* %argslist54304$k483290)
store volatile %struct.ScmObj* %argslist54304$k483291, %struct.ScmObj** %stackaddr$prim55356, align 8
%stackaddr$prim55357 = alloca %struct.ScmObj*, align 8
%argslist54304$k483292 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51253, %struct.ScmObj* %argslist54304$k483291)
store volatile %struct.ScmObj* %argslist54304$k483292, %struct.ScmObj** %stackaddr$prim55357, align 8
%clofunc55358 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48329)
musttail call tailcc void %clofunc55358(%struct.ScmObj* %k48329, %struct.ScmObj* %argslist54304$k483292)
ret void
falsebranch$cmp55355:
%ae51262 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55359 = alloca %struct.ScmObj*, align 8
%anf_45bind48242 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51262)
store volatile %struct.ScmObj* %anf_45bind48242, %struct.ScmObj** %stackaddr$prim55359, align 8
%stackaddr$prim55360 = alloca %struct.ScmObj*, align 8
%anf_45bind48243 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48242)
store volatile %struct.ScmObj* %anf_45bind48243, %struct.ScmObj** %stackaddr$prim55360, align 8
%stackaddr$prim55361 = alloca %struct.ScmObj*, align 8
%anf_45bind48244 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind48243, %struct.ScmObj* %v48096)
store volatile %struct.ScmObj* %anf_45bind48244, %struct.ScmObj** %stackaddr$prim55361, align 8
%truthy$cmp55362 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48244)
%cmp$cmp55362 = icmp eq i64 %truthy$cmp55362, 1
br i1 %cmp$cmp55362, label %truebranch$cmp55362, label %falsebranch$cmp55362
truebranch$cmp55362:
%ae51268 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55363 = alloca %struct.ScmObj*, align 8
%cpsprim48332 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51268)
store volatile %struct.ScmObj* %cpsprim48332, %struct.ScmObj** %stackaddr$prim55363, align 8
%ae51270 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54305$k483290 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55364 = alloca %struct.ScmObj*, align 8
%argslist54305$k483291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48332, %struct.ScmObj* %argslist54305$k483290)
store volatile %struct.ScmObj* %argslist54305$k483291, %struct.ScmObj** %stackaddr$prim55364, align 8
%stackaddr$prim55365 = alloca %struct.ScmObj*, align 8
%argslist54305$k483292 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51270, %struct.ScmObj* %argslist54305$k483291)
store volatile %struct.ScmObj* %argslist54305$k483292, %struct.ScmObj** %stackaddr$prim55365, align 8
%clofunc55366 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48329)
musttail call tailcc void %clofunc55366(%struct.ScmObj* %k48329, %struct.ScmObj* %argslist54305$k483292)
ret void
falsebranch$cmp55362:
%ae51281 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55367 = alloca %struct.ScmObj*, align 8
%anf_45bind48245 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51281)
store volatile %struct.ScmObj* %anf_45bind48245, %struct.ScmObj** %stackaddr$prim55367, align 8
%stackaddr$prim55368 = alloca %struct.ScmObj*, align 8
%anf_45bind48246 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48245)
store volatile %struct.ScmObj* %anf_45bind48246, %struct.ScmObj** %stackaddr$prim55368, align 8
%ae51284 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55369 = alloca %struct.ScmObj*, align 8
%_95048100 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51284, %struct.ScmObj* %anf_45bind48246)
store volatile %struct.ScmObj* %_95048100, %struct.ScmObj** %stackaddr$prim55369, align 8
%argslist54306$cc480980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55370 = alloca %struct.ScmObj*, align 8
%argslist54306$cc480981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48098, %struct.ScmObj* %argslist54306$cc480980)
store volatile %struct.ScmObj* %argslist54306$cc480981, %struct.ScmObj** %stackaddr$prim55370, align 8
%stackaddr$prim55371 = alloca %struct.ScmObj*, align 8
%argslist54306$cc480982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48329, %struct.ScmObj* %argslist54306$cc480981)
store volatile %struct.ScmObj* %argslist54306$cc480982, %struct.ScmObj** %stackaddr$prim55371, align 8
%clofunc55372 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48098)
musttail call tailcc void %clofunc55372(%struct.ScmObj* %cc48098, %struct.ScmObj* %argslist54306$cc480982)
ret void
}

define tailcc void @proc_clo$ae51232(%struct.ScmObj* %env$ae51232,%struct.ScmObj* %current_45args54308) {
%stackaddr$prim55373 = alloca %struct.ScmObj*, align 8
%k48333 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54308)
store volatile %struct.ScmObj* %k48333, %struct.ScmObj** %stackaddr$prim55373, align 8
%stackaddr$prim55374 = alloca %struct.ScmObj*, align 8
%current_45args54309 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54308)
store volatile %struct.ScmObj* %current_45args54309, %struct.ScmObj** %stackaddr$prim55374, align 8
%stackaddr$prim55375 = alloca %struct.ScmObj*, align 8
%u48099 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54309)
store volatile %struct.ScmObj* %u48099, %struct.ScmObj** %stackaddr$prim55375, align 8
%argslist54311$u480990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55376 = alloca %struct.ScmObj*, align 8
%argslist54311$u480991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u48099, %struct.ScmObj* %argslist54311$u480990)
store volatile %struct.ScmObj* %argslist54311$u480991, %struct.ScmObj** %stackaddr$prim55376, align 8
%stackaddr$prim55377 = alloca %struct.ScmObj*, align 8
%argslist54311$u480992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48333, %struct.ScmObj* %argslist54311$u480991)
store volatile %struct.ScmObj* %argslist54311$u480992, %struct.ScmObj** %stackaddr$prim55377, align 8
%clofunc55378 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u48099)
musttail call tailcc void %clofunc55378(%struct.ScmObj* %u48099, %struct.ScmObj* %argslist54311$u480992)
ret void
}

define tailcc void @proc_clo$ae50691(%struct.ScmObj* %env$ae50691,%struct.ScmObj* %current_45args54314) {
%stackaddr$prim55379 = alloca %struct.ScmObj*, align 8
%k48334 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54314)
store volatile %struct.ScmObj* %k48334, %struct.ScmObj** %stackaddr$prim55379, align 8
%stackaddr$prim55380 = alloca %struct.ScmObj*, align 8
%current_45args54315 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54314)
store volatile %struct.ScmObj* %current_45args54315, %struct.ScmObj** %stackaddr$prim55380, align 8
%stackaddr$prim55381 = alloca %struct.ScmObj*, align 8
%lst48103 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54315)
store volatile %struct.ScmObj* %lst48103, %struct.ScmObj** %stackaddr$prim55381, align 8
%stackaddr$prim55382 = alloca %struct.ScmObj*, align 8
%current_45args54316 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54315)
store volatile %struct.ScmObj* %current_45args54316, %struct.ScmObj** %stackaddr$prim55382, align 8
%stackaddr$prim55383 = alloca %struct.ScmObj*, align 8
%n48102 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54316)
store volatile %struct.ScmObj* %n48102, %struct.ScmObj** %stackaddr$prim55383, align 8
%ae50692 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55384 = alloca %struct.ScmObj*, align 8
%n48105 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50692, %struct.ScmObj* %n48102)
store volatile %struct.ScmObj* %n48105, %struct.ScmObj** %stackaddr$prim55384, align 8
%ae50694 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55385 = alloca %struct.ScmObj*, align 8
%lst48104 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50694, %struct.ScmObj* %lst48103)
store volatile %struct.ScmObj* %lst48104, %struct.ScmObj** %stackaddr$prim55385, align 8
%stackaddr$makeclosure55386 = alloca %struct.ScmObj*, align 8
%fptrToInt55387 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50696 to i64
%ae50696 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55387)
store volatile %struct.ScmObj* %ae50696, %struct.ScmObj** %stackaddr$makeclosure55386, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50696, %struct.ScmObj* %k48334, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50696, %struct.ScmObj* %n48105, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50696, %struct.ScmObj* %lst48104, i64 2)
%ae50697 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55388 = alloca %struct.ScmObj*, align 8
%fptrToInt55389 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50698 to i64
%ae50698 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55389)
store volatile %struct.ScmObj* %ae50698, %struct.ScmObj** %stackaddr$makeclosure55388, align 8
%argslist54336$ae506960 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55390 = alloca %struct.ScmObj*, align 8
%argslist54336$ae506961 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50698, %struct.ScmObj* %argslist54336$ae506960)
store volatile %struct.ScmObj* %argslist54336$ae506961, %struct.ScmObj** %stackaddr$prim55390, align 8
%stackaddr$prim55391 = alloca %struct.ScmObj*, align 8
%argslist54336$ae506962 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50697, %struct.ScmObj* %argslist54336$ae506961)
store volatile %struct.ScmObj* %argslist54336$ae506962, %struct.ScmObj** %stackaddr$prim55391, align 8
%clofunc55392 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50696)
musttail call tailcc void %clofunc55392(%struct.ScmObj* %ae50696, %struct.ScmObj* %argslist54336$ae506962)
ret void
}

define tailcc void @proc_clo$ae50696(%struct.ScmObj* %env$ae50696,%struct.ScmObj* %current_45args54318) {
%stackaddr$env-ref55393 = alloca %struct.ScmObj*, align 8
%k48334 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50696, i64 0)
store %struct.ScmObj* %k48334, %struct.ScmObj** %stackaddr$env-ref55393
%stackaddr$env-ref55394 = alloca %struct.ScmObj*, align 8
%n48105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50696, i64 1)
store %struct.ScmObj* %n48105, %struct.ScmObj** %stackaddr$env-ref55394
%stackaddr$env-ref55395 = alloca %struct.ScmObj*, align 8
%lst48104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50696, i64 2)
store %struct.ScmObj* %lst48104, %struct.ScmObj** %stackaddr$env-ref55395
%stackaddr$prim55396 = alloca %struct.ScmObj*, align 8
%_95k48335 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54318)
store volatile %struct.ScmObj* %_95k48335, %struct.ScmObj** %stackaddr$prim55396, align 8
%stackaddr$prim55397 = alloca %struct.ScmObj*, align 8
%current_45args54319 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54318)
store volatile %struct.ScmObj* %current_45args54319, %struct.ScmObj** %stackaddr$prim55397, align 8
%stackaddr$prim55398 = alloca %struct.ScmObj*, align 8
%anf_45bind48232 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54319)
store volatile %struct.ScmObj* %anf_45bind48232, %struct.ScmObj** %stackaddr$prim55398, align 8
%stackaddr$makeclosure55399 = alloca %struct.ScmObj*, align 8
%fptrToInt55400 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50712 to i64
%ae50712 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55400)
store volatile %struct.ScmObj* %ae50712, %struct.ScmObj** %stackaddr$makeclosure55399, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50712, %struct.ScmObj* %k48334, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50712, %struct.ScmObj* %n48105, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50712, %struct.ScmObj* %lst48104, i64 2)
%stackaddr$makeclosure55401 = alloca %struct.ScmObj*, align 8
%fptrToInt55402 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50713 to i64
%ae50713 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55402)
store volatile %struct.ScmObj* %ae50713, %struct.ScmObj** %stackaddr$makeclosure55401, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50713, %struct.ScmObj* %k48334, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50713, %struct.ScmObj* %n48105, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50713, %struct.ScmObj* %lst48104, i64 2)
%argslist54331$anf_45bind482320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55403 = alloca %struct.ScmObj*, align 8
%argslist54331$anf_45bind482321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50713, %struct.ScmObj* %argslist54331$anf_45bind482320)
store volatile %struct.ScmObj* %argslist54331$anf_45bind482321, %struct.ScmObj** %stackaddr$prim55403, align 8
%stackaddr$prim55404 = alloca %struct.ScmObj*, align 8
%argslist54331$anf_45bind482322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50712, %struct.ScmObj* %argslist54331$anf_45bind482321)
store volatile %struct.ScmObj* %argslist54331$anf_45bind482322, %struct.ScmObj** %stackaddr$prim55404, align 8
%clofunc55405 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48232)
musttail call tailcc void %clofunc55405(%struct.ScmObj* %anf_45bind48232, %struct.ScmObj* %argslist54331$anf_45bind482322)
ret void
}

define tailcc void @proc_clo$ae50712(%struct.ScmObj* %env$ae50712,%struct.ScmObj* %current_45args54321) {
%stackaddr$env-ref55406 = alloca %struct.ScmObj*, align 8
%k48334 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50712, i64 0)
store %struct.ScmObj* %k48334, %struct.ScmObj** %stackaddr$env-ref55406
%stackaddr$env-ref55407 = alloca %struct.ScmObj*, align 8
%n48105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50712, i64 1)
store %struct.ScmObj* %n48105, %struct.ScmObj** %stackaddr$env-ref55407
%stackaddr$env-ref55408 = alloca %struct.ScmObj*, align 8
%lst48104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50712, i64 2)
store %struct.ScmObj* %lst48104, %struct.ScmObj** %stackaddr$env-ref55408
%stackaddr$prim55409 = alloca %struct.ScmObj*, align 8
%_95k48336 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54321)
store volatile %struct.ScmObj* %_95k48336, %struct.ScmObj** %stackaddr$prim55409, align 8
%stackaddr$prim55410 = alloca %struct.ScmObj*, align 8
%current_45args54322 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54321)
store volatile %struct.ScmObj* %current_45args54322, %struct.ScmObj** %stackaddr$prim55410, align 8
%stackaddr$prim55411 = alloca %struct.ScmObj*, align 8
%cc48106 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54322)
store volatile %struct.ScmObj* %cc48106, %struct.ScmObj** %stackaddr$prim55411, align 8
%ae50855 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55412 = alloca %struct.ScmObj*, align 8
%anf_45bind48233 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48105, %struct.ScmObj* %ae50855)
store volatile %struct.ScmObj* %anf_45bind48233, %struct.ScmObj** %stackaddr$prim55412, align 8
%ae50856 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55413 = alloca %struct.ScmObj*, align 8
%anf_45bind48234 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae50856, %struct.ScmObj* %anf_45bind48233)
store volatile %struct.ScmObj* %anf_45bind48234, %struct.ScmObj** %stackaddr$prim55413, align 8
%truthy$cmp55414 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48234)
%cmp$cmp55414 = icmp eq i64 %truthy$cmp55414, 1
br i1 %cmp$cmp55414, label %truebranch$cmp55414, label %falsebranch$cmp55414
truebranch$cmp55414:
%ae50860 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55415 = alloca %struct.ScmObj*, align 8
%cpsprim48337 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48104, %struct.ScmObj* %ae50860)
store volatile %struct.ScmObj* %cpsprim48337, %struct.ScmObj** %stackaddr$prim55415, align 8
%ae50862 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54324$k483340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55416 = alloca %struct.ScmObj*, align 8
%argslist54324$k483341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48337, %struct.ScmObj* %argslist54324$k483340)
store volatile %struct.ScmObj* %argslist54324$k483341, %struct.ScmObj** %stackaddr$prim55416, align 8
%stackaddr$prim55417 = alloca %struct.ScmObj*, align 8
%argslist54324$k483342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50862, %struct.ScmObj* %argslist54324$k483341)
store volatile %struct.ScmObj* %argslist54324$k483342, %struct.ScmObj** %stackaddr$prim55417, align 8
%clofunc55418 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48334)
musttail call tailcc void %clofunc55418(%struct.ScmObj* %k48334, %struct.ScmObj* %argslist54324$k483342)
ret void
falsebranch$cmp55414:
%ae50873 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55419 = alloca %struct.ScmObj*, align 8
%anf_45bind48235 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48104, %struct.ScmObj* %ae50873)
store volatile %struct.ScmObj* %anf_45bind48235, %struct.ScmObj** %stackaddr$prim55419, align 8
%stackaddr$prim55420 = alloca %struct.ScmObj*, align 8
%anf_45bind48236 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48235)
store volatile %struct.ScmObj* %anf_45bind48236, %struct.ScmObj** %stackaddr$prim55420, align 8
%ae50876 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55421 = alloca %struct.ScmObj*, align 8
%_95048109 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48104, %struct.ScmObj* %ae50876, %struct.ScmObj* %anf_45bind48236)
store volatile %struct.ScmObj* %_95048109, %struct.ScmObj** %stackaddr$prim55421, align 8
%ae50879 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55422 = alloca %struct.ScmObj*, align 8
%anf_45bind48237 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48105, %struct.ScmObj* %ae50879)
store volatile %struct.ScmObj* %anf_45bind48237, %struct.ScmObj** %stackaddr$prim55422, align 8
%ae50881 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55423 = alloca %struct.ScmObj*, align 8
%anf_45bind48238 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind48237, %struct.ScmObj* %ae50881)
store volatile %struct.ScmObj* %anf_45bind48238, %struct.ScmObj** %stackaddr$prim55423, align 8
%ae50883 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55424 = alloca %struct.ScmObj*, align 8
%_95148108 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n48105, %struct.ScmObj* %ae50883, %struct.ScmObj* %anf_45bind48238)
store volatile %struct.ScmObj* %_95148108, %struct.ScmObj** %stackaddr$prim55424, align 8
%argslist54325$cc481060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55425 = alloca %struct.ScmObj*, align 8
%argslist54325$cc481061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48106, %struct.ScmObj* %argslist54325$cc481060)
store volatile %struct.ScmObj* %argslist54325$cc481061, %struct.ScmObj** %stackaddr$prim55425, align 8
%stackaddr$prim55426 = alloca %struct.ScmObj*, align 8
%argslist54325$cc481062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48334, %struct.ScmObj* %argslist54325$cc481061)
store volatile %struct.ScmObj* %argslist54325$cc481062, %struct.ScmObj** %stackaddr$prim55426, align 8
%clofunc55427 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48106)
musttail call tailcc void %clofunc55427(%struct.ScmObj* %cc48106, %struct.ScmObj* %argslist54325$cc481062)
ret void
}

define tailcc void @proc_clo$ae50713(%struct.ScmObj* %env$ae50713,%struct.ScmObj* %current_45args54326) {
%stackaddr$env-ref55428 = alloca %struct.ScmObj*, align 8
%k48334 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50713, i64 0)
store %struct.ScmObj* %k48334, %struct.ScmObj** %stackaddr$env-ref55428
%stackaddr$env-ref55429 = alloca %struct.ScmObj*, align 8
%n48105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50713, i64 1)
store %struct.ScmObj* %n48105, %struct.ScmObj** %stackaddr$env-ref55429
%stackaddr$env-ref55430 = alloca %struct.ScmObj*, align 8
%lst48104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50713, i64 2)
store %struct.ScmObj* %lst48104, %struct.ScmObj** %stackaddr$env-ref55430
%stackaddr$prim55431 = alloca %struct.ScmObj*, align 8
%_95k48336 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54326)
store volatile %struct.ScmObj* %_95k48336, %struct.ScmObj** %stackaddr$prim55431, align 8
%stackaddr$prim55432 = alloca %struct.ScmObj*, align 8
%current_45args54327 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54326)
store volatile %struct.ScmObj* %current_45args54327, %struct.ScmObj** %stackaddr$prim55432, align 8
%stackaddr$prim55433 = alloca %struct.ScmObj*, align 8
%cc48106 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54327)
store volatile %struct.ScmObj* %cc48106, %struct.ScmObj** %stackaddr$prim55433, align 8
%ae50715 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55434 = alloca %struct.ScmObj*, align 8
%anf_45bind48233 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48105, %struct.ScmObj* %ae50715)
store volatile %struct.ScmObj* %anf_45bind48233, %struct.ScmObj** %stackaddr$prim55434, align 8
%ae50716 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55435 = alloca %struct.ScmObj*, align 8
%anf_45bind48234 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae50716, %struct.ScmObj* %anf_45bind48233)
store volatile %struct.ScmObj* %anf_45bind48234, %struct.ScmObj** %stackaddr$prim55435, align 8
%truthy$cmp55436 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48234)
%cmp$cmp55436 = icmp eq i64 %truthy$cmp55436, 1
br i1 %cmp$cmp55436, label %truebranch$cmp55436, label %falsebranch$cmp55436
truebranch$cmp55436:
%ae50720 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55437 = alloca %struct.ScmObj*, align 8
%cpsprim48337 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48104, %struct.ScmObj* %ae50720)
store volatile %struct.ScmObj* %cpsprim48337, %struct.ScmObj** %stackaddr$prim55437, align 8
%ae50722 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54329$k483340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55438 = alloca %struct.ScmObj*, align 8
%argslist54329$k483341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48337, %struct.ScmObj* %argslist54329$k483340)
store volatile %struct.ScmObj* %argslist54329$k483341, %struct.ScmObj** %stackaddr$prim55438, align 8
%stackaddr$prim55439 = alloca %struct.ScmObj*, align 8
%argslist54329$k483342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50722, %struct.ScmObj* %argslist54329$k483341)
store volatile %struct.ScmObj* %argslist54329$k483342, %struct.ScmObj** %stackaddr$prim55439, align 8
%clofunc55440 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48334)
musttail call tailcc void %clofunc55440(%struct.ScmObj* %k48334, %struct.ScmObj* %argslist54329$k483342)
ret void
falsebranch$cmp55436:
%ae50733 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55441 = alloca %struct.ScmObj*, align 8
%anf_45bind48235 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48104, %struct.ScmObj* %ae50733)
store volatile %struct.ScmObj* %anf_45bind48235, %struct.ScmObj** %stackaddr$prim55441, align 8
%stackaddr$prim55442 = alloca %struct.ScmObj*, align 8
%anf_45bind48236 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48235)
store volatile %struct.ScmObj* %anf_45bind48236, %struct.ScmObj** %stackaddr$prim55442, align 8
%ae50736 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55443 = alloca %struct.ScmObj*, align 8
%_95048109 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48104, %struct.ScmObj* %ae50736, %struct.ScmObj* %anf_45bind48236)
store volatile %struct.ScmObj* %_95048109, %struct.ScmObj** %stackaddr$prim55443, align 8
%ae50739 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55444 = alloca %struct.ScmObj*, align 8
%anf_45bind48237 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48105, %struct.ScmObj* %ae50739)
store volatile %struct.ScmObj* %anf_45bind48237, %struct.ScmObj** %stackaddr$prim55444, align 8
%ae50741 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55445 = alloca %struct.ScmObj*, align 8
%anf_45bind48238 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind48237, %struct.ScmObj* %ae50741)
store volatile %struct.ScmObj* %anf_45bind48238, %struct.ScmObj** %stackaddr$prim55445, align 8
%ae50743 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55446 = alloca %struct.ScmObj*, align 8
%_95148108 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n48105, %struct.ScmObj* %ae50743, %struct.ScmObj* %anf_45bind48238)
store volatile %struct.ScmObj* %_95148108, %struct.ScmObj** %stackaddr$prim55446, align 8
%argslist54330$cc481060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55447 = alloca %struct.ScmObj*, align 8
%argslist54330$cc481061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48106, %struct.ScmObj* %argslist54330$cc481060)
store volatile %struct.ScmObj* %argslist54330$cc481061, %struct.ScmObj** %stackaddr$prim55447, align 8
%stackaddr$prim55448 = alloca %struct.ScmObj*, align 8
%argslist54330$cc481062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48334, %struct.ScmObj* %argslist54330$cc481061)
store volatile %struct.ScmObj* %argslist54330$cc481062, %struct.ScmObj** %stackaddr$prim55448, align 8
%clofunc55449 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48106)
musttail call tailcc void %clofunc55449(%struct.ScmObj* %cc48106, %struct.ScmObj* %argslist54330$cc481062)
ret void
}

define tailcc void @proc_clo$ae50698(%struct.ScmObj* %env$ae50698,%struct.ScmObj* %current_45args54332) {
%stackaddr$prim55450 = alloca %struct.ScmObj*, align 8
%k48338 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54332)
store volatile %struct.ScmObj* %k48338, %struct.ScmObj** %stackaddr$prim55450, align 8
%stackaddr$prim55451 = alloca %struct.ScmObj*, align 8
%current_45args54333 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54332)
store volatile %struct.ScmObj* %current_45args54333, %struct.ScmObj** %stackaddr$prim55451, align 8
%stackaddr$prim55452 = alloca %struct.ScmObj*, align 8
%u48107 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54333)
store volatile %struct.ScmObj* %u48107, %struct.ScmObj** %stackaddr$prim55452, align 8
%argslist54335$u481070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55453 = alloca %struct.ScmObj*, align 8
%argslist54335$u481071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u48107, %struct.ScmObj* %argslist54335$u481070)
store volatile %struct.ScmObj* %argslist54335$u481071, %struct.ScmObj** %stackaddr$prim55453, align 8
%stackaddr$prim55454 = alloca %struct.ScmObj*, align 8
%argslist54335$u481072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48338, %struct.ScmObj* %argslist54335$u481071)
store volatile %struct.ScmObj* %argslist54335$u481072, %struct.ScmObj** %stackaddr$prim55454, align 8
%clofunc55455 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u48107)
musttail call tailcc void %clofunc55455(%struct.ScmObj* %u48107, %struct.ScmObj* %argslist54335$u481072)
ret void
}

define tailcc void @proc_clo$ae50275(%struct.ScmObj* %env$ae50275,%struct.ScmObj* %current_45args54338) {
%stackaddr$prim55456 = alloca %struct.ScmObj*, align 8
%k48339 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54338)
store volatile %struct.ScmObj* %k48339, %struct.ScmObj** %stackaddr$prim55456, align 8
%stackaddr$prim55457 = alloca %struct.ScmObj*, align 8
%current_45args54339 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54338)
store volatile %struct.ScmObj* %current_45args54339, %struct.ScmObj** %stackaddr$prim55457, align 8
%stackaddr$prim55458 = alloca %struct.ScmObj*, align 8
%a48111 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54339)
store volatile %struct.ScmObj* %a48111, %struct.ScmObj** %stackaddr$prim55458, align 8
%ae50276 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55459 = alloca %struct.ScmObj*, align 8
%a48112 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50276, %struct.ScmObj* %a48111)
store volatile %struct.ScmObj* %a48112, %struct.ScmObj** %stackaddr$prim55459, align 8
%stackaddr$makeclosure55460 = alloca %struct.ScmObj*, align 8
%fptrToInt55461 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50278 to i64
%ae50278 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55461)
store volatile %struct.ScmObj* %ae50278, %struct.ScmObj** %stackaddr$makeclosure55460, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50278, %struct.ScmObj* %k48339, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50278, %struct.ScmObj* %a48112, i64 1)
%ae50279 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55462 = alloca %struct.ScmObj*, align 8
%fptrToInt55463 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50280 to i64
%ae50280 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55463)
store volatile %struct.ScmObj* %ae50280, %struct.ScmObj** %stackaddr$makeclosure55462, align 8
%argslist54361$ae502780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55464 = alloca %struct.ScmObj*, align 8
%argslist54361$ae502781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50280, %struct.ScmObj* %argslist54361$ae502780)
store volatile %struct.ScmObj* %argslist54361$ae502781, %struct.ScmObj** %stackaddr$prim55464, align 8
%stackaddr$prim55465 = alloca %struct.ScmObj*, align 8
%argslist54361$ae502782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50279, %struct.ScmObj* %argslist54361$ae502781)
store volatile %struct.ScmObj* %argslist54361$ae502782, %struct.ScmObj** %stackaddr$prim55465, align 8
%clofunc55466 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50278)
musttail call tailcc void %clofunc55466(%struct.ScmObj* %ae50278, %struct.ScmObj* %argslist54361$ae502782)
ret void
}

define tailcc void @proc_clo$ae50278(%struct.ScmObj* %env$ae50278,%struct.ScmObj* %current_45args54341) {
%stackaddr$env-ref55467 = alloca %struct.ScmObj*, align 8
%k48339 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50278, i64 0)
store %struct.ScmObj* %k48339, %struct.ScmObj** %stackaddr$env-ref55467
%stackaddr$env-ref55468 = alloca %struct.ScmObj*, align 8
%a48112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50278, i64 1)
store %struct.ScmObj* %a48112, %struct.ScmObj** %stackaddr$env-ref55468
%stackaddr$prim55469 = alloca %struct.ScmObj*, align 8
%_95k48340 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54341)
store volatile %struct.ScmObj* %_95k48340, %struct.ScmObj** %stackaddr$prim55469, align 8
%stackaddr$prim55470 = alloca %struct.ScmObj*, align 8
%current_45args54342 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54341)
store volatile %struct.ScmObj* %current_45args54342, %struct.ScmObj** %stackaddr$prim55470, align 8
%stackaddr$prim55471 = alloca %struct.ScmObj*, align 8
%anf_45bind48224 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54342)
store volatile %struct.ScmObj* %anf_45bind48224, %struct.ScmObj** %stackaddr$prim55471, align 8
%stackaddr$makeclosure55472 = alloca %struct.ScmObj*, align 8
%fptrToInt55473 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50297 to i64
%ae50297 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55473)
store volatile %struct.ScmObj* %ae50297, %struct.ScmObj** %stackaddr$makeclosure55472, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50297, %struct.ScmObj* %k48339, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50297, %struct.ScmObj* %a48112, i64 1)
%stackaddr$makeclosure55474 = alloca %struct.ScmObj*, align 8
%fptrToInt55475 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50298 to i64
%ae50298 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55475)
store volatile %struct.ScmObj* %ae50298, %struct.ScmObj** %stackaddr$makeclosure55474, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50298, %struct.ScmObj* %k48339, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50298, %struct.ScmObj* %a48112, i64 1)
%argslist54356$anf_45bind482240 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55476 = alloca %struct.ScmObj*, align 8
%argslist54356$anf_45bind482241 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50298, %struct.ScmObj* %argslist54356$anf_45bind482240)
store volatile %struct.ScmObj* %argslist54356$anf_45bind482241, %struct.ScmObj** %stackaddr$prim55476, align 8
%stackaddr$prim55477 = alloca %struct.ScmObj*, align 8
%argslist54356$anf_45bind482242 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50297, %struct.ScmObj* %argslist54356$anf_45bind482241)
store volatile %struct.ScmObj* %argslist54356$anf_45bind482242, %struct.ScmObj** %stackaddr$prim55477, align 8
%clofunc55478 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48224)
musttail call tailcc void %clofunc55478(%struct.ScmObj* %anf_45bind48224, %struct.ScmObj* %argslist54356$anf_45bind482242)
ret void
}

define tailcc void @proc_clo$ae50297(%struct.ScmObj* %env$ae50297,%struct.ScmObj* %current_45args54344) {
%stackaddr$env-ref55479 = alloca %struct.ScmObj*, align 8
%k48339 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50297, i64 0)
store %struct.ScmObj* %k48339, %struct.ScmObj** %stackaddr$env-ref55479
%stackaddr$env-ref55480 = alloca %struct.ScmObj*, align 8
%a48112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50297, i64 1)
store %struct.ScmObj* %a48112, %struct.ScmObj** %stackaddr$env-ref55480
%stackaddr$prim55481 = alloca %struct.ScmObj*, align 8
%_95k48341 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54344)
store volatile %struct.ScmObj* %_95k48341, %struct.ScmObj** %stackaddr$prim55481, align 8
%stackaddr$prim55482 = alloca %struct.ScmObj*, align 8
%current_45args54345 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54344)
store volatile %struct.ScmObj* %current_45args54345, %struct.ScmObj** %stackaddr$prim55482, align 8
%stackaddr$prim55483 = alloca %struct.ScmObj*, align 8
%cc48113 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54345)
store volatile %struct.ScmObj* %cc48113, %struct.ScmObj** %stackaddr$prim55483, align 8
%ae50413 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55484 = alloca %struct.ScmObj*, align 8
%anf_45bind48225 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50413)
store volatile %struct.ScmObj* %anf_45bind48225, %struct.ScmObj** %stackaddr$prim55484, align 8
%stackaddr$prim55485 = alloca %struct.ScmObj*, align 8
%anf_45bind48226 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48225)
store volatile %struct.ScmObj* %anf_45bind48226, %struct.ScmObj** %stackaddr$prim55485, align 8
%truthy$cmp55486 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48226)
%cmp$cmp55486 = icmp eq i64 %truthy$cmp55486, 1
br i1 %cmp$cmp55486, label %truebranch$cmp55486, label %falsebranch$cmp55486
truebranch$cmp55486:
%ae50417 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50418 = call %struct.ScmObj* @const_init_true()
%argslist54347$k483390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55487 = alloca %struct.ScmObj*, align 8
%argslist54347$k483391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50418, %struct.ScmObj* %argslist54347$k483390)
store volatile %struct.ScmObj* %argslist54347$k483391, %struct.ScmObj** %stackaddr$prim55487, align 8
%stackaddr$prim55488 = alloca %struct.ScmObj*, align 8
%argslist54347$k483392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50417, %struct.ScmObj* %argslist54347$k483391)
store volatile %struct.ScmObj* %argslist54347$k483392, %struct.ScmObj** %stackaddr$prim55488, align 8
%clofunc55489 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48339)
musttail call tailcc void %clofunc55489(%struct.ScmObj* %k48339, %struct.ScmObj* %argslist54347$k483392)
ret void
falsebranch$cmp55486:
%ae50426 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55490 = alloca %struct.ScmObj*, align 8
%anf_45bind48227 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50426)
store volatile %struct.ScmObj* %anf_45bind48227, %struct.ScmObj** %stackaddr$prim55490, align 8
%stackaddr$prim55491 = alloca %struct.ScmObj*, align 8
%anf_45bind48228 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind48227)
store volatile %struct.ScmObj* %anf_45bind48228, %struct.ScmObj** %stackaddr$prim55491, align 8
%truthy$cmp55492 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48228)
%cmp$cmp55492 = icmp eq i64 %truthy$cmp55492, 1
br i1 %cmp$cmp55492, label %truebranch$cmp55492, label %falsebranch$cmp55492
truebranch$cmp55492:
%ae50430 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55493 = alloca %struct.ScmObj*, align 8
%anf_45bind48229 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50430)
store volatile %struct.ScmObj* %anf_45bind48229, %struct.ScmObj** %stackaddr$prim55493, align 8
%stackaddr$prim55494 = alloca %struct.ScmObj*, align 8
%b48115 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48229)
store volatile %struct.ScmObj* %b48115, %struct.ScmObj** %stackaddr$prim55494, align 8
%ae50433 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55495 = alloca %struct.ScmObj*, align 8
%anf_45bind48230 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50433)
store volatile %struct.ScmObj* %anf_45bind48230, %struct.ScmObj** %stackaddr$prim55495, align 8
%stackaddr$prim55496 = alloca %struct.ScmObj*, align 8
%anf_45bind48231 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48230)
store volatile %struct.ScmObj* %anf_45bind48231, %struct.ScmObj** %stackaddr$prim55496, align 8
%ae50436 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55497 = alloca %struct.ScmObj*, align 8
%_95048116 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50436, %struct.ScmObj* %anf_45bind48231)
store volatile %struct.ScmObj* %_95048116, %struct.ScmObj** %stackaddr$prim55497, align 8
%argslist54348$cc481130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55498 = alloca %struct.ScmObj*, align 8
%argslist54348$cc481131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48113, %struct.ScmObj* %argslist54348$cc481130)
store volatile %struct.ScmObj* %argslist54348$cc481131, %struct.ScmObj** %stackaddr$prim55498, align 8
%stackaddr$prim55499 = alloca %struct.ScmObj*, align 8
%argslist54348$cc481132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48339, %struct.ScmObj* %argslist54348$cc481131)
store volatile %struct.ScmObj* %argslist54348$cc481132, %struct.ScmObj** %stackaddr$prim55499, align 8
%clofunc55500 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48113)
musttail call tailcc void %clofunc55500(%struct.ScmObj* %cc48113, %struct.ScmObj* %argslist54348$cc481132)
ret void
falsebranch$cmp55492:
%ae50469 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50470 = call %struct.ScmObj* @const_init_false()
%argslist54349$k483390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55501 = alloca %struct.ScmObj*, align 8
%argslist54349$k483391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50470, %struct.ScmObj* %argslist54349$k483390)
store volatile %struct.ScmObj* %argslist54349$k483391, %struct.ScmObj** %stackaddr$prim55501, align 8
%stackaddr$prim55502 = alloca %struct.ScmObj*, align 8
%argslist54349$k483392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50469, %struct.ScmObj* %argslist54349$k483391)
store volatile %struct.ScmObj* %argslist54349$k483392, %struct.ScmObj** %stackaddr$prim55502, align 8
%clofunc55503 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48339)
musttail call tailcc void %clofunc55503(%struct.ScmObj* %k48339, %struct.ScmObj* %argslist54349$k483392)
ret void
}

define tailcc void @proc_clo$ae50298(%struct.ScmObj* %env$ae50298,%struct.ScmObj* %current_45args54350) {
%stackaddr$env-ref55504 = alloca %struct.ScmObj*, align 8
%k48339 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50298, i64 0)
store %struct.ScmObj* %k48339, %struct.ScmObj** %stackaddr$env-ref55504
%stackaddr$env-ref55505 = alloca %struct.ScmObj*, align 8
%a48112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50298, i64 1)
store %struct.ScmObj* %a48112, %struct.ScmObj** %stackaddr$env-ref55505
%stackaddr$prim55506 = alloca %struct.ScmObj*, align 8
%_95k48341 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54350)
store volatile %struct.ScmObj* %_95k48341, %struct.ScmObj** %stackaddr$prim55506, align 8
%stackaddr$prim55507 = alloca %struct.ScmObj*, align 8
%current_45args54351 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54350)
store volatile %struct.ScmObj* %current_45args54351, %struct.ScmObj** %stackaddr$prim55507, align 8
%stackaddr$prim55508 = alloca %struct.ScmObj*, align 8
%cc48113 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54351)
store volatile %struct.ScmObj* %cc48113, %struct.ScmObj** %stackaddr$prim55508, align 8
%ae50300 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55509 = alloca %struct.ScmObj*, align 8
%anf_45bind48225 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50300)
store volatile %struct.ScmObj* %anf_45bind48225, %struct.ScmObj** %stackaddr$prim55509, align 8
%stackaddr$prim55510 = alloca %struct.ScmObj*, align 8
%anf_45bind48226 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48225)
store volatile %struct.ScmObj* %anf_45bind48226, %struct.ScmObj** %stackaddr$prim55510, align 8
%truthy$cmp55511 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48226)
%cmp$cmp55511 = icmp eq i64 %truthy$cmp55511, 1
br i1 %cmp$cmp55511, label %truebranch$cmp55511, label %falsebranch$cmp55511
truebranch$cmp55511:
%ae50304 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50305 = call %struct.ScmObj* @const_init_true()
%argslist54353$k483390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55512 = alloca %struct.ScmObj*, align 8
%argslist54353$k483391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50305, %struct.ScmObj* %argslist54353$k483390)
store volatile %struct.ScmObj* %argslist54353$k483391, %struct.ScmObj** %stackaddr$prim55512, align 8
%stackaddr$prim55513 = alloca %struct.ScmObj*, align 8
%argslist54353$k483392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50304, %struct.ScmObj* %argslist54353$k483391)
store volatile %struct.ScmObj* %argslist54353$k483392, %struct.ScmObj** %stackaddr$prim55513, align 8
%clofunc55514 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48339)
musttail call tailcc void %clofunc55514(%struct.ScmObj* %k48339, %struct.ScmObj* %argslist54353$k483392)
ret void
falsebranch$cmp55511:
%ae50313 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55515 = alloca %struct.ScmObj*, align 8
%anf_45bind48227 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50313)
store volatile %struct.ScmObj* %anf_45bind48227, %struct.ScmObj** %stackaddr$prim55515, align 8
%stackaddr$prim55516 = alloca %struct.ScmObj*, align 8
%anf_45bind48228 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind48227)
store volatile %struct.ScmObj* %anf_45bind48228, %struct.ScmObj** %stackaddr$prim55516, align 8
%truthy$cmp55517 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48228)
%cmp$cmp55517 = icmp eq i64 %truthy$cmp55517, 1
br i1 %cmp$cmp55517, label %truebranch$cmp55517, label %falsebranch$cmp55517
truebranch$cmp55517:
%ae50317 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55518 = alloca %struct.ScmObj*, align 8
%anf_45bind48229 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50317)
store volatile %struct.ScmObj* %anf_45bind48229, %struct.ScmObj** %stackaddr$prim55518, align 8
%stackaddr$prim55519 = alloca %struct.ScmObj*, align 8
%b48115 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48229)
store volatile %struct.ScmObj* %b48115, %struct.ScmObj** %stackaddr$prim55519, align 8
%ae50320 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55520 = alloca %struct.ScmObj*, align 8
%anf_45bind48230 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50320)
store volatile %struct.ScmObj* %anf_45bind48230, %struct.ScmObj** %stackaddr$prim55520, align 8
%stackaddr$prim55521 = alloca %struct.ScmObj*, align 8
%anf_45bind48231 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48230)
store volatile %struct.ScmObj* %anf_45bind48231, %struct.ScmObj** %stackaddr$prim55521, align 8
%ae50323 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55522 = alloca %struct.ScmObj*, align 8
%_95048116 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50323, %struct.ScmObj* %anf_45bind48231)
store volatile %struct.ScmObj* %_95048116, %struct.ScmObj** %stackaddr$prim55522, align 8
%argslist54354$cc481130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55523 = alloca %struct.ScmObj*, align 8
%argslist54354$cc481131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48113, %struct.ScmObj* %argslist54354$cc481130)
store volatile %struct.ScmObj* %argslist54354$cc481131, %struct.ScmObj** %stackaddr$prim55523, align 8
%stackaddr$prim55524 = alloca %struct.ScmObj*, align 8
%argslist54354$cc481132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48339, %struct.ScmObj* %argslist54354$cc481131)
store volatile %struct.ScmObj* %argslist54354$cc481132, %struct.ScmObj** %stackaddr$prim55524, align 8
%clofunc55525 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48113)
musttail call tailcc void %clofunc55525(%struct.ScmObj* %cc48113, %struct.ScmObj* %argslist54354$cc481132)
ret void
falsebranch$cmp55517:
%ae50356 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50357 = call %struct.ScmObj* @const_init_false()
%argslist54355$k483390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55526 = alloca %struct.ScmObj*, align 8
%argslist54355$k483391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50357, %struct.ScmObj* %argslist54355$k483390)
store volatile %struct.ScmObj* %argslist54355$k483391, %struct.ScmObj** %stackaddr$prim55526, align 8
%stackaddr$prim55527 = alloca %struct.ScmObj*, align 8
%argslist54355$k483392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50356, %struct.ScmObj* %argslist54355$k483391)
store volatile %struct.ScmObj* %argslist54355$k483392, %struct.ScmObj** %stackaddr$prim55527, align 8
%clofunc55528 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48339)
musttail call tailcc void %clofunc55528(%struct.ScmObj* %k48339, %struct.ScmObj* %argslist54355$k483392)
ret void
}

define tailcc void @proc_clo$ae50280(%struct.ScmObj* %env$ae50280,%struct.ScmObj* %current_45args54357) {
%stackaddr$prim55529 = alloca %struct.ScmObj*, align 8
%k48342 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54357)
store volatile %struct.ScmObj* %k48342, %struct.ScmObj** %stackaddr$prim55529, align 8
%stackaddr$prim55530 = alloca %struct.ScmObj*, align 8
%current_45args54358 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54357)
store volatile %struct.ScmObj* %current_45args54358, %struct.ScmObj** %stackaddr$prim55530, align 8
%stackaddr$prim55531 = alloca %struct.ScmObj*, align 8
%k48114 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54358)
store volatile %struct.ScmObj* %k48114, %struct.ScmObj** %stackaddr$prim55531, align 8
%ae50282 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54360$k483420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55532 = alloca %struct.ScmObj*, align 8
%argslist54360$k483421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48114, %struct.ScmObj* %argslist54360$k483420)
store volatile %struct.ScmObj* %argslist54360$k483421, %struct.ScmObj** %stackaddr$prim55532, align 8
%stackaddr$prim55533 = alloca %struct.ScmObj*, align 8
%argslist54360$k483422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50282, %struct.ScmObj* %argslist54360$k483421)
store volatile %struct.ScmObj* %argslist54360$k483422, %struct.ScmObj** %stackaddr$prim55533, align 8
%clofunc55534 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48342)
musttail call tailcc void %clofunc55534(%struct.ScmObj* %k48342, %struct.ScmObj* %argslist54360$k483422)
ret void
}

define tailcc void @proc_clo$ae50203(%struct.ScmObj* %env$ae50203,%struct.ScmObj* %current_45args54363) {
%stackaddr$env-ref55535 = alloca %struct.ScmObj*, align 8
%_37append48118 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50203, i64 0)
store %struct.ScmObj* %_37append48118, %struct.ScmObj** %stackaddr$env-ref55535
%stackaddr$prim55536 = alloca %struct.ScmObj*, align 8
%k48343 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54363)
store volatile %struct.ScmObj* %k48343, %struct.ScmObj** %stackaddr$prim55536, align 8
%stackaddr$prim55537 = alloca %struct.ScmObj*, align 8
%current_45args54364 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54363)
store volatile %struct.ScmObj* %current_45args54364, %struct.ScmObj** %stackaddr$prim55537, align 8
%stackaddr$prim55538 = alloca %struct.ScmObj*, align 8
%ls048121 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54364)
store volatile %struct.ScmObj* %ls048121, %struct.ScmObj** %stackaddr$prim55538, align 8
%stackaddr$prim55539 = alloca %struct.ScmObj*, align 8
%current_45args54365 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54364)
store volatile %struct.ScmObj* %current_45args54365, %struct.ScmObj** %stackaddr$prim55539, align 8
%stackaddr$prim55540 = alloca %struct.ScmObj*, align 8
%ls148120 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54365)
store volatile %struct.ScmObj* %ls148120, %struct.ScmObj** %stackaddr$prim55540, align 8
%stackaddr$prim55541 = alloca %struct.ScmObj*, align 8
%anf_45bind48218 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls048121)
store volatile %struct.ScmObj* %anf_45bind48218, %struct.ScmObj** %stackaddr$prim55541, align 8
%truthy$cmp55542 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48218)
%cmp$cmp55542 = icmp eq i64 %truthy$cmp55542, 1
br i1 %cmp$cmp55542, label %truebranch$cmp55542, label %falsebranch$cmp55542
truebranch$cmp55542:
%ae50207 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54367$k483430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55543 = alloca %struct.ScmObj*, align 8
%argslist54367$k483431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls148120, %struct.ScmObj* %argslist54367$k483430)
store volatile %struct.ScmObj* %argslist54367$k483431, %struct.ScmObj** %stackaddr$prim55543, align 8
%stackaddr$prim55544 = alloca %struct.ScmObj*, align 8
%argslist54367$k483432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50207, %struct.ScmObj* %argslist54367$k483431)
store volatile %struct.ScmObj* %argslist54367$k483432, %struct.ScmObj** %stackaddr$prim55544, align 8
%clofunc55545 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48343)
musttail call tailcc void %clofunc55545(%struct.ScmObj* %k48343, %struct.ScmObj* %argslist54367$k483432)
ret void
falsebranch$cmp55542:
%stackaddr$prim55546 = alloca %struct.ScmObj*, align 8
%anf_45bind48219 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls048121)
store volatile %struct.ScmObj* %anf_45bind48219, %struct.ScmObj** %stackaddr$prim55546, align 8
%ae50214 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55547 = alloca %struct.ScmObj*, align 8
%anf_45bind48220 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append48118, %struct.ScmObj* %ae50214)
store volatile %struct.ScmObj* %anf_45bind48220, %struct.ScmObj** %stackaddr$prim55547, align 8
%stackaddr$prim55548 = alloca %struct.ScmObj*, align 8
%anf_45bind48221 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls048121)
store volatile %struct.ScmObj* %anf_45bind48221, %struct.ScmObj** %stackaddr$prim55548, align 8
%stackaddr$makeclosure55549 = alloca %struct.ScmObj*, align 8
%fptrToInt55550 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50217 to i64
%ae50217 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55550)
store volatile %struct.ScmObj* %ae50217, %struct.ScmObj** %stackaddr$makeclosure55549, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50217, %struct.ScmObj* %k48343, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50217, %struct.ScmObj* %anf_45bind48219, i64 1)
%argslist54372$anf_45bind482200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55551 = alloca %struct.ScmObj*, align 8
%argslist54372$anf_45bind482201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls148120, %struct.ScmObj* %argslist54372$anf_45bind482200)
store volatile %struct.ScmObj* %argslist54372$anf_45bind482201, %struct.ScmObj** %stackaddr$prim55551, align 8
%stackaddr$prim55552 = alloca %struct.ScmObj*, align 8
%argslist54372$anf_45bind482202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48221, %struct.ScmObj* %argslist54372$anf_45bind482201)
store volatile %struct.ScmObj* %argslist54372$anf_45bind482202, %struct.ScmObj** %stackaddr$prim55552, align 8
%stackaddr$prim55553 = alloca %struct.ScmObj*, align 8
%argslist54372$anf_45bind482203 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50217, %struct.ScmObj* %argslist54372$anf_45bind482202)
store volatile %struct.ScmObj* %argslist54372$anf_45bind482203, %struct.ScmObj** %stackaddr$prim55553, align 8
%clofunc55554 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48220)
musttail call tailcc void %clofunc55554(%struct.ScmObj* %anf_45bind48220, %struct.ScmObj* %argslist54372$anf_45bind482203)
ret void
}

define tailcc void @proc_clo$ae50217(%struct.ScmObj* %env$ae50217,%struct.ScmObj* %current_45args54368) {
%stackaddr$env-ref55555 = alloca %struct.ScmObj*, align 8
%k48343 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50217, i64 0)
store %struct.ScmObj* %k48343, %struct.ScmObj** %stackaddr$env-ref55555
%stackaddr$env-ref55556 = alloca %struct.ScmObj*, align 8
%anf_45bind48219 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50217, i64 1)
store %struct.ScmObj* %anf_45bind48219, %struct.ScmObj** %stackaddr$env-ref55556
%stackaddr$prim55557 = alloca %struct.ScmObj*, align 8
%_95k48344 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54368)
store volatile %struct.ScmObj* %_95k48344, %struct.ScmObj** %stackaddr$prim55557, align 8
%stackaddr$prim55558 = alloca %struct.ScmObj*, align 8
%current_45args54369 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54368)
store volatile %struct.ScmObj* %current_45args54369, %struct.ScmObj** %stackaddr$prim55558, align 8
%stackaddr$prim55559 = alloca %struct.ScmObj*, align 8
%anf_45bind48222 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54369)
store volatile %struct.ScmObj* %anf_45bind48222, %struct.ScmObj** %stackaddr$prim55559, align 8
%stackaddr$prim55560 = alloca %struct.ScmObj*, align 8
%cpsprim48345 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48219, %struct.ScmObj* %anf_45bind48222)
store volatile %struct.ScmObj* %cpsprim48345, %struct.ScmObj** %stackaddr$prim55560, align 8
%ae50223 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54371$k483430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55561 = alloca %struct.ScmObj*, align 8
%argslist54371$k483431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48345, %struct.ScmObj* %argslist54371$k483430)
store volatile %struct.ScmObj* %argslist54371$k483431, %struct.ScmObj** %stackaddr$prim55561, align 8
%stackaddr$prim55562 = alloca %struct.ScmObj*, align 8
%argslist54371$k483432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50223, %struct.ScmObj* %argslist54371$k483431)
store volatile %struct.ScmObj* %argslist54371$k483432, %struct.ScmObj** %stackaddr$prim55562, align 8
%clofunc55563 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48343)
musttail call tailcc void %clofunc55563(%struct.ScmObj* %k48343, %struct.ScmObj* %argslist54371$k483432)
ret void
}

define tailcc void @proc_clo$ae50177(%struct.ScmObj* %env$ae50177,%struct.ScmObj* %current_45args54374) {
%stackaddr$prim55564 = alloca %struct.ScmObj*, align 8
%k48346 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54374)
store volatile %struct.ScmObj* %k48346, %struct.ScmObj** %stackaddr$prim55564, align 8
%stackaddr$prim55565 = alloca %struct.ScmObj*, align 8
%current_45args54375 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54374)
store volatile %struct.ScmObj* %current_45args54375, %struct.ScmObj** %stackaddr$prim55565, align 8
%stackaddr$prim55566 = alloca %struct.ScmObj*, align 8
%a48124 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54375)
store volatile %struct.ScmObj* %a48124, %struct.ScmObj** %stackaddr$prim55566, align 8
%stackaddr$prim55567 = alloca %struct.ScmObj*, align 8
%current_45args54376 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54375)
store volatile %struct.ScmObj* %current_45args54376, %struct.ScmObj** %stackaddr$prim55567, align 8
%stackaddr$prim55568 = alloca %struct.ScmObj*, align 8
%b48123 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54376)
store volatile %struct.ScmObj* %b48123, %struct.ScmObj** %stackaddr$prim55568, align 8
%stackaddr$prim55569 = alloca %struct.ScmObj*, align 8
%anf_45bind48217 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a48124, %struct.ScmObj* %b48123)
store volatile %struct.ScmObj* %anf_45bind48217, %struct.ScmObj** %stackaddr$prim55569, align 8
%stackaddr$prim55570 = alloca %struct.ScmObj*, align 8
%cpsprim48347 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind48217)
store volatile %struct.ScmObj* %cpsprim48347, %struct.ScmObj** %stackaddr$prim55570, align 8
%ae50182 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54378$k483460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55571 = alloca %struct.ScmObj*, align 8
%argslist54378$k483461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48347, %struct.ScmObj* %argslist54378$k483460)
store volatile %struct.ScmObj* %argslist54378$k483461, %struct.ScmObj** %stackaddr$prim55571, align 8
%stackaddr$prim55572 = alloca %struct.ScmObj*, align 8
%argslist54378$k483462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50182, %struct.ScmObj* %argslist54378$k483461)
store volatile %struct.ScmObj* %argslist54378$k483462, %struct.ScmObj** %stackaddr$prim55572, align 8
%clofunc55573 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48346)
musttail call tailcc void %clofunc55573(%struct.ScmObj* %k48346, %struct.ScmObj* %argslist54378$k483462)
ret void
}

define tailcc void @proc_clo$ae50153(%struct.ScmObj* %env$ae50153,%struct.ScmObj* %current_45args54380) {
%stackaddr$prim55574 = alloca %struct.ScmObj*, align 8
%k48348 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54380)
store volatile %struct.ScmObj* %k48348, %struct.ScmObj** %stackaddr$prim55574, align 8
%stackaddr$prim55575 = alloca %struct.ScmObj*, align 8
%current_45args54381 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54380)
store volatile %struct.ScmObj* %current_45args54381, %struct.ScmObj** %stackaddr$prim55575, align 8
%stackaddr$prim55576 = alloca %struct.ScmObj*, align 8
%a48127 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54381)
store volatile %struct.ScmObj* %a48127, %struct.ScmObj** %stackaddr$prim55576, align 8
%stackaddr$prim55577 = alloca %struct.ScmObj*, align 8
%current_45args54382 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54381)
store volatile %struct.ScmObj* %current_45args54382, %struct.ScmObj** %stackaddr$prim55577, align 8
%stackaddr$prim55578 = alloca %struct.ScmObj*, align 8
%b48126 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54382)
store volatile %struct.ScmObj* %b48126, %struct.ScmObj** %stackaddr$prim55578, align 8
%stackaddr$prim55579 = alloca %struct.ScmObj*, align 8
%anf_45bind48216 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a48127, %struct.ScmObj* %b48126)
store volatile %struct.ScmObj* %anf_45bind48216, %struct.ScmObj** %stackaddr$prim55579, align 8
%stackaddr$prim55580 = alloca %struct.ScmObj*, align 8
%cpsprim48349 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind48216)
store volatile %struct.ScmObj* %cpsprim48349, %struct.ScmObj** %stackaddr$prim55580, align 8
%ae50158 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54384$k483480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55581 = alloca %struct.ScmObj*, align 8
%argslist54384$k483481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48349, %struct.ScmObj* %argslist54384$k483480)
store volatile %struct.ScmObj* %argslist54384$k483481, %struct.ScmObj** %stackaddr$prim55581, align 8
%stackaddr$prim55582 = alloca %struct.ScmObj*, align 8
%argslist54384$k483482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50158, %struct.ScmObj* %argslist54384$k483481)
store volatile %struct.ScmObj* %argslist54384$k483482, %struct.ScmObj** %stackaddr$prim55582, align 8
%clofunc55583 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48348)
musttail call tailcc void %clofunc55583(%struct.ScmObj* %k48348, %struct.ScmObj* %argslist54384$k483482)
ret void
}

define tailcc void @proc_clo$ae49759(%struct.ScmObj* %env$ae49759,%struct.ScmObj* %current_45args54387) {
%stackaddr$env-ref55584 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49759, i64 0)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55584
%stackaddr$env-ref55585 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49759, i64 1)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55585
%stackaddr$env-ref55586 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49759, i64 2)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55586
%stackaddr$prim55587 = alloca %struct.ScmObj*, align 8
%k48350 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54387)
store volatile %struct.ScmObj* %k48350, %struct.ScmObj** %stackaddr$prim55587, align 8
%stackaddr$prim55588 = alloca %struct.ScmObj*, align 8
%current_45args54388 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54387)
store volatile %struct.ScmObj* %current_45args54388, %struct.ScmObj** %stackaddr$prim55588, align 8
%stackaddr$prim55589 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54388)
store volatile %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$prim55589, align 8
%ae49761 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55590 = alloca %struct.ScmObj*, align 8
%fptrToInt55591 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49762 to i64
%ae49762 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55591)
store volatile %struct.ScmObj* %ae49762, %struct.ScmObj** %stackaddr$makeclosure55590, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49762, %struct.ScmObj* %_37foldr48051, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49762, %struct.ScmObj* %_37foldl48129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49762, %struct.ScmObj* %_37foldr148046, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49762, %struct.ScmObj* %_37map148077, i64 3)
%argslist54445$k483500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55592 = alloca %struct.ScmObj*, align 8
%argslist54445$k483501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49762, %struct.ScmObj* %argslist54445$k483500)
store volatile %struct.ScmObj* %argslist54445$k483501, %struct.ScmObj** %stackaddr$prim55592, align 8
%stackaddr$prim55593 = alloca %struct.ScmObj*, align 8
%argslist54445$k483502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49761, %struct.ScmObj* %argslist54445$k483501)
store volatile %struct.ScmObj* %argslist54445$k483502, %struct.ScmObj** %stackaddr$prim55593, align 8
%clofunc55594 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48350)
musttail call tailcc void %clofunc55594(%struct.ScmObj* %k48350, %struct.ScmObj* %argslist54445$k483502)
ret void
}

define tailcc void @proc_clo$ae49762(%struct.ScmObj* %env$ae49762,%struct.ScmObj* %args4813048351) {
%stackaddr$env-ref55595 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49762, i64 0)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55595
%stackaddr$env-ref55596 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49762, i64 1)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55596
%stackaddr$env-ref55597 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49762, i64 2)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55597
%stackaddr$env-ref55598 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49762, i64 3)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55598
%stackaddr$prim55599 = alloca %struct.ScmObj*, align 8
%k48352 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4813048351)
store volatile %struct.ScmObj* %k48352, %struct.ScmObj** %stackaddr$prim55599, align 8
%stackaddr$prim55600 = alloca %struct.ScmObj*, align 8
%args48130 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4813048351)
store volatile %struct.ScmObj* %args48130, %struct.ScmObj** %stackaddr$prim55600, align 8
%stackaddr$prim55601 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48130)
store volatile %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$prim55601, align 8
%stackaddr$prim55602 = alloca %struct.ScmObj*, align 8
%anf_45bind48204 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48130)
store volatile %struct.ScmObj* %anf_45bind48204, %struct.ScmObj** %stackaddr$prim55602, align 8
%stackaddr$prim55603 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48204)
store volatile %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$prim55603, align 8
%stackaddr$prim55604 = alloca %struct.ScmObj*, align 8
%anf_45bind48205 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48130)
store volatile %struct.ScmObj* %anf_45bind48205, %struct.ScmObj** %stackaddr$prim55604, align 8
%stackaddr$prim55605 = alloca %struct.ScmObj*, align 8
%lsts48131 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48205)
store volatile %struct.ScmObj* %lsts48131, %struct.ScmObj** %stackaddr$prim55605, align 8
%stackaddr$makeclosure55606 = alloca %struct.ScmObj*, align 8
%fptrToInt55607 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49770 to i64
%ae49770 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt55607)
store volatile %struct.ScmObj* %ae49770, %struct.ScmObj** %stackaddr$makeclosure55606, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49770, %struct.ScmObj* %lsts48131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49770, %struct.ScmObj* %_37foldr48051, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49770, %struct.ScmObj* %f48133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49770, %struct.ScmObj* %acc48132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49770, %struct.ScmObj* %_37foldl48129, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49770, %struct.ScmObj* %k48352, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49770, %struct.ScmObj* %_37foldr148046, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae49770, %struct.ScmObj* %_37map148077, i64 7)
%ae49771 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55608 = alloca %struct.ScmObj*, align 8
%fptrToInt55609 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49772 to i64
%ae49772 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55609)
store volatile %struct.ScmObj* %ae49772, %struct.ScmObj** %stackaddr$makeclosure55608, align 8
%argslist54444$ae497700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55610 = alloca %struct.ScmObj*, align 8
%argslist54444$ae497701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49772, %struct.ScmObj* %argslist54444$ae497700)
store volatile %struct.ScmObj* %argslist54444$ae497701, %struct.ScmObj** %stackaddr$prim55610, align 8
%stackaddr$prim55611 = alloca %struct.ScmObj*, align 8
%argslist54444$ae497702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49771, %struct.ScmObj* %argslist54444$ae497701)
store volatile %struct.ScmObj* %argslist54444$ae497702, %struct.ScmObj** %stackaddr$prim55611, align 8
%clofunc55612 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49770)
musttail call tailcc void %clofunc55612(%struct.ScmObj* %ae49770, %struct.ScmObj* %argslist54444$ae497702)
ret void
}

define tailcc void @proc_clo$ae49770(%struct.ScmObj* %env$ae49770,%struct.ScmObj* %current_45args54390) {
%stackaddr$env-ref55613 = alloca %struct.ScmObj*, align 8
%lsts48131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49770, i64 0)
store %struct.ScmObj* %lsts48131, %struct.ScmObj** %stackaddr$env-ref55613
%stackaddr$env-ref55614 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49770, i64 1)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55614
%stackaddr$env-ref55615 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49770, i64 2)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref55615
%stackaddr$env-ref55616 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49770, i64 3)
store %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$env-ref55616
%stackaddr$env-ref55617 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49770, i64 4)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55617
%stackaddr$env-ref55618 = alloca %struct.ScmObj*, align 8
%k48352 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49770, i64 5)
store %struct.ScmObj* %k48352, %struct.ScmObj** %stackaddr$env-ref55618
%stackaddr$env-ref55619 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49770, i64 6)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55619
%stackaddr$env-ref55620 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49770, i64 7)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55620
%stackaddr$prim55621 = alloca %struct.ScmObj*, align 8
%_95k48353 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54390)
store volatile %struct.ScmObj* %_95k48353, %struct.ScmObj** %stackaddr$prim55621, align 8
%stackaddr$prim55622 = alloca %struct.ScmObj*, align 8
%current_45args54391 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54390)
store volatile %struct.ScmObj* %current_45args54391, %struct.ScmObj** %stackaddr$prim55622, align 8
%stackaddr$prim55623 = alloca %struct.ScmObj*, align 8
%anf_45bind48206 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54391)
store volatile %struct.ScmObj* %anf_45bind48206, %struct.ScmObj** %stackaddr$prim55623, align 8
%stackaddr$makeclosure55624 = alloca %struct.ScmObj*, align 8
%fptrToInt55625 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49802 to i64
%ae49802 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55625)
store volatile %struct.ScmObj* %ae49802, %struct.ScmObj** %stackaddr$makeclosure55624, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49802, %struct.ScmObj* %lsts48131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49802, %struct.ScmObj* %_37foldr48051, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49802, %struct.ScmObj* %f48133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49802, %struct.ScmObj* %acc48132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49802, %struct.ScmObj* %_37foldl48129, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49802, %struct.ScmObj* %k48352, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49802, %struct.ScmObj* %_37map148077, i64 6)
%ae49804 = call %struct.ScmObj* @const_init_false()
%argslist54437$_37foldr1480460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55626 = alloca %struct.ScmObj*, align 8
%argslist54437$_37foldr1480461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48131, %struct.ScmObj* %argslist54437$_37foldr1480460)
store volatile %struct.ScmObj* %argslist54437$_37foldr1480461, %struct.ScmObj** %stackaddr$prim55626, align 8
%stackaddr$prim55627 = alloca %struct.ScmObj*, align 8
%argslist54437$_37foldr1480462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49804, %struct.ScmObj* %argslist54437$_37foldr1480461)
store volatile %struct.ScmObj* %argslist54437$_37foldr1480462, %struct.ScmObj** %stackaddr$prim55627, align 8
%stackaddr$prim55628 = alloca %struct.ScmObj*, align 8
%argslist54437$_37foldr1480463 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48206, %struct.ScmObj* %argslist54437$_37foldr1480462)
store volatile %struct.ScmObj* %argslist54437$_37foldr1480463, %struct.ScmObj** %stackaddr$prim55628, align 8
%stackaddr$prim55629 = alloca %struct.ScmObj*, align 8
%argslist54437$_37foldr1480464 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49802, %struct.ScmObj* %argslist54437$_37foldr1480463)
store volatile %struct.ScmObj* %argslist54437$_37foldr1480464, %struct.ScmObj** %stackaddr$prim55629, align 8
%clofunc55630 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148046)
musttail call tailcc void %clofunc55630(%struct.ScmObj* %_37foldr148046, %struct.ScmObj* %argslist54437$_37foldr1480464)
ret void
}

define tailcc void @proc_clo$ae49802(%struct.ScmObj* %env$ae49802,%struct.ScmObj* %current_45args54393) {
%stackaddr$env-ref55631 = alloca %struct.ScmObj*, align 8
%lsts48131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49802, i64 0)
store %struct.ScmObj* %lsts48131, %struct.ScmObj** %stackaddr$env-ref55631
%stackaddr$env-ref55632 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49802, i64 1)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55632
%stackaddr$env-ref55633 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49802, i64 2)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref55633
%stackaddr$env-ref55634 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49802, i64 3)
store %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$env-ref55634
%stackaddr$env-ref55635 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49802, i64 4)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55635
%stackaddr$env-ref55636 = alloca %struct.ScmObj*, align 8
%k48352 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49802, i64 5)
store %struct.ScmObj* %k48352, %struct.ScmObj** %stackaddr$env-ref55636
%stackaddr$env-ref55637 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49802, i64 6)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55637
%stackaddr$prim55638 = alloca %struct.ScmObj*, align 8
%_95k48354 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54393)
store volatile %struct.ScmObj* %_95k48354, %struct.ScmObj** %stackaddr$prim55638, align 8
%stackaddr$prim55639 = alloca %struct.ScmObj*, align 8
%current_45args54394 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54393)
store volatile %struct.ScmObj* %current_45args54394, %struct.ScmObj** %stackaddr$prim55639, align 8
%stackaddr$prim55640 = alloca %struct.ScmObj*, align 8
%anf_45bind48207 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54394)
store volatile %struct.ScmObj* %anf_45bind48207, %struct.ScmObj** %stackaddr$prim55640, align 8
%truthy$cmp55641 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48207)
%cmp$cmp55641 = icmp eq i64 %truthy$cmp55641, 1
br i1 %cmp$cmp55641, label %truebranch$cmp55641, label %falsebranch$cmp55641
truebranch$cmp55641:
%ae49813 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54396$k483520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55642 = alloca %struct.ScmObj*, align 8
%argslist54396$k483521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48132, %struct.ScmObj* %argslist54396$k483520)
store volatile %struct.ScmObj* %argslist54396$k483521, %struct.ScmObj** %stackaddr$prim55642, align 8
%stackaddr$prim55643 = alloca %struct.ScmObj*, align 8
%argslist54396$k483522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49813, %struct.ScmObj* %argslist54396$k483521)
store volatile %struct.ScmObj* %argslist54396$k483522, %struct.ScmObj** %stackaddr$prim55643, align 8
%clofunc55644 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48352)
musttail call tailcc void %clofunc55644(%struct.ScmObj* %k48352, %struct.ScmObj* %argslist54396$k483522)
ret void
falsebranch$cmp55641:
%stackaddr$makeclosure55645 = alloca %struct.ScmObj*, align 8
%fptrToInt55646 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49818 to i64
%ae49818 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55646)
store volatile %struct.ScmObj* %ae49818, %struct.ScmObj** %stackaddr$makeclosure55645, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49818, %struct.ScmObj* %lsts48131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49818, %struct.ScmObj* %_37foldr48051, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49818, %struct.ScmObj* %f48133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49818, %struct.ScmObj* %acc48132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49818, %struct.ScmObj* %_37foldl48129, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49818, %struct.ScmObj* %k48352, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49818, %struct.ScmObj* %_37map148077, i64 6)
%ae49819 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55647 = alloca %struct.ScmObj*, align 8
%fptrToInt55648 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49820 to i64
%ae49820 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55648)
store volatile %struct.ScmObj* %ae49820, %struct.ScmObj** %stackaddr$makeclosure55647, align 8
%argslist54436$ae498180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55649 = alloca %struct.ScmObj*, align 8
%argslist54436$ae498181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49820, %struct.ScmObj* %argslist54436$ae498180)
store volatile %struct.ScmObj* %argslist54436$ae498181, %struct.ScmObj** %stackaddr$prim55649, align 8
%stackaddr$prim55650 = alloca %struct.ScmObj*, align 8
%argslist54436$ae498182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49819, %struct.ScmObj* %argslist54436$ae498181)
store volatile %struct.ScmObj* %argslist54436$ae498182, %struct.ScmObj** %stackaddr$prim55650, align 8
%clofunc55651 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49818)
musttail call tailcc void %clofunc55651(%struct.ScmObj* %ae49818, %struct.ScmObj* %argslist54436$ae498182)
ret void
}

define tailcc void @proc_clo$ae49818(%struct.ScmObj* %env$ae49818,%struct.ScmObj* %current_45args54397) {
%stackaddr$env-ref55652 = alloca %struct.ScmObj*, align 8
%lsts48131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49818, i64 0)
store %struct.ScmObj* %lsts48131, %struct.ScmObj** %stackaddr$env-ref55652
%stackaddr$env-ref55653 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49818, i64 1)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55653
%stackaddr$env-ref55654 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49818, i64 2)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref55654
%stackaddr$env-ref55655 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49818, i64 3)
store %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$env-ref55655
%stackaddr$env-ref55656 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49818, i64 4)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55656
%stackaddr$env-ref55657 = alloca %struct.ScmObj*, align 8
%k48352 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49818, i64 5)
store %struct.ScmObj* %k48352, %struct.ScmObj** %stackaddr$env-ref55657
%stackaddr$env-ref55658 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49818, i64 6)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55658
%stackaddr$prim55659 = alloca %struct.ScmObj*, align 8
%_95k48355 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54397)
store volatile %struct.ScmObj* %_95k48355, %struct.ScmObj** %stackaddr$prim55659, align 8
%stackaddr$prim55660 = alloca %struct.ScmObj*, align 8
%current_45args54398 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54397)
store volatile %struct.ScmObj* %current_45args54398, %struct.ScmObj** %stackaddr$prim55660, align 8
%stackaddr$prim55661 = alloca %struct.ScmObj*, align 8
%anf_45bind48208 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54398)
store volatile %struct.ScmObj* %anf_45bind48208, %struct.ScmObj** %stackaddr$prim55661, align 8
%stackaddr$makeclosure55662 = alloca %struct.ScmObj*, align 8
%fptrToInt55663 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49839 to i64
%ae49839 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55663)
store volatile %struct.ScmObj* %ae49839, %struct.ScmObj** %stackaddr$makeclosure55662, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49839, %struct.ScmObj* %lsts48131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49839, %struct.ScmObj* %_37foldr48051, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49839, %struct.ScmObj* %f48133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49839, %struct.ScmObj* %acc48132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49839, %struct.ScmObj* %_37foldl48129, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49839, %struct.ScmObj* %k48352, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49839, %struct.ScmObj* %_37map148077, i64 6)
%argslist54431$_37map1480770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55664 = alloca %struct.ScmObj*, align 8
%argslist54431$_37map1480771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48131, %struct.ScmObj* %argslist54431$_37map1480770)
store volatile %struct.ScmObj* %argslist54431$_37map1480771, %struct.ScmObj** %stackaddr$prim55664, align 8
%stackaddr$prim55665 = alloca %struct.ScmObj*, align 8
%argslist54431$_37map1480772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48208, %struct.ScmObj* %argslist54431$_37map1480771)
store volatile %struct.ScmObj* %argslist54431$_37map1480772, %struct.ScmObj** %stackaddr$prim55665, align 8
%stackaddr$prim55666 = alloca %struct.ScmObj*, align 8
%argslist54431$_37map1480773 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49839, %struct.ScmObj* %argslist54431$_37map1480772)
store volatile %struct.ScmObj* %argslist54431$_37map1480773, %struct.ScmObj** %stackaddr$prim55666, align 8
%clofunc55667 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148077)
musttail call tailcc void %clofunc55667(%struct.ScmObj* %_37map148077, %struct.ScmObj* %argslist54431$_37map1480773)
ret void
}

define tailcc void @proc_clo$ae49839(%struct.ScmObj* %env$ae49839,%struct.ScmObj* %current_45args54400) {
%stackaddr$env-ref55668 = alloca %struct.ScmObj*, align 8
%lsts48131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49839, i64 0)
store %struct.ScmObj* %lsts48131, %struct.ScmObj** %stackaddr$env-ref55668
%stackaddr$env-ref55669 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49839, i64 1)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55669
%stackaddr$env-ref55670 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49839, i64 2)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref55670
%stackaddr$env-ref55671 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49839, i64 3)
store %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$env-ref55671
%stackaddr$env-ref55672 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49839, i64 4)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55672
%stackaddr$env-ref55673 = alloca %struct.ScmObj*, align 8
%k48352 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49839, i64 5)
store %struct.ScmObj* %k48352, %struct.ScmObj** %stackaddr$env-ref55673
%stackaddr$env-ref55674 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49839, i64 6)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55674
%stackaddr$prim55675 = alloca %struct.ScmObj*, align 8
%_95k48356 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54400)
store volatile %struct.ScmObj* %_95k48356, %struct.ScmObj** %stackaddr$prim55675, align 8
%stackaddr$prim55676 = alloca %struct.ScmObj*, align 8
%current_45args54401 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54400)
store volatile %struct.ScmObj* %current_45args54401, %struct.ScmObj** %stackaddr$prim55676, align 8
%stackaddr$prim55677 = alloca %struct.ScmObj*, align 8
%lsts_4348138 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54401)
store volatile %struct.ScmObj* %lsts_4348138, %struct.ScmObj** %stackaddr$prim55677, align 8
%stackaddr$makeclosure55678 = alloca %struct.ScmObj*, align 8
%fptrToInt55679 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49842 to i64
%ae49842 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt55679)
store volatile %struct.ScmObj* %ae49842, %struct.ScmObj** %stackaddr$makeclosure55678, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49842, %struct.ScmObj* %lsts48131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49842, %struct.ScmObj* %_37foldr48051, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49842, %struct.ScmObj* %f48133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49842, %struct.ScmObj* %acc48132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49842, %struct.ScmObj* %_37foldl48129, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49842, %struct.ScmObj* %k48352, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49842, %struct.ScmObj* %_37map148077, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae49842, %struct.ScmObj* %lsts_4348138, i64 7)
%ae49843 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55680 = alloca %struct.ScmObj*, align 8
%fptrToInt55681 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49844 to i64
%ae49844 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55681)
store volatile %struct.ScmObj* %ae49844, %struct.ScmObj** %stackaddr$makeclosure55680, align 8
%argslist54430$ae498420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55682 = alloca %struct.ScmObj*, align 8
%argslist54430$ae498421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49844, %struct.ScmObj* %argslist54430$ae498420)
store volatile %struct.ScmObj* %argslist54430$ae498421, %struct.ScmObj** %stackaddr$prim55682, align 8
%stackaddr$prim55683 = alloca %struct.ScmObj*, align 8
%argslist54430$ae498422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49843, %struct.ScmObj* %argslist54430$ae498421)
store volatile %struct.ScmObj* %argslist54430$ae498422, %struct.ScmObj** %stackaddr$prim55683, align 8
%clofunc55684 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49842)
musttail call tailcc void %clofunc55684(%struct.ScmObj* %ae49842, %struct.ScmObj* %argslist54430$ae498422)
ret void
}

define tailcc void @proc_clo$ae49842(%struct.ScmObj* %env$ae49842,%struct.ScmObj* %current_45args54403) {
%stackaddr$env-ref55685 = alloca %struct.ScmObj*, align 8
%lsts48131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49842, i64 0)
store %struct.ScmObj* %lsts48131, %struct.ScmObj** %stackaddr$env-ref55685
%stackaddr$env-ref55686 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49842, i64 1)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55686
%stackaddr$env-ref55687 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49842, i64 2)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref55687
%stackaddr$env-ref55688 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49842, i64 3)
store %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$env-ref55688
%stackaddr$env-ref55689 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49842, i64 4)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55689
%stackaddr$env-ref55690 = alloca %struct.ScmObj*, align 8
%k48352 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49842, i64 5)
store %struct.ScmObj* %k48352, %struct.ScmObj** %stackaddr$env-ref55690
%stackaddr$env-ref55691 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49842, i64 6)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55691
%stackaddr$env-ref55692 = alloca %struct.ScmObj*, align 8
%lsts_4348138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49842, i64 7)
store %struct.ScmObj* %lsts_4348138, %struct.ScmObj** %stackaddr$env-ref55692
%stackaddr$prim55693 = alloca %struct.ScmObj*, align 8
%_95k48357 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54403)
store volatile %struct.ScmObj* %_95k48357, %struct.ScmObj** %stackaddr$prim55693, align 8
%stackaddr$prim55694 = alloca %struct.ScmObj*, align 8
%current_45args54404 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54403)
store volatile %struct.ScmObj* %current_45args54404, %struct.ScmObj** %stackaddr$prim55694, align 8
%stackaddr$prim55695 = alloca %struct.ScmObj*, align 8
%anf_45bind48209 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54404)
store volatile %struct.ScmObj* %anf_45bind48209, %struct.ScmObj** %stackaddr$prim55695, align 8
%stackaddr$makeclosure55696 = alloca %struct.ScmObj*, align 8
%fptrToInt55697 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49863 to i64
%ae49863 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt55697)
store volatile %struct.ScmObj* %ae49863, %struct.ScmObj** %stackaddr$makeclosure55696, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49863, %struct.ScmObj* %f48133, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49863, %struct.ScmObj* %acc48132, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49863, %struct.ScmObj* %_37foldr48051, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49863, %struct.ScmObj* %_37foldl48129, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49863, %struct.ScmObj* %k48352, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49863, %struct.ScmObj* %lsts_4348138, i64 5)
%argslist54425$_37map1480770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55698 = alloca %struct.ScmObj*, align 8
%argslist54425$_37map1480771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48131, %struct.ScmObj* %argslist54425$_37map1480770)
store volatile %struct.ScmObj* %argslist54425$_37map1480771, %struct.ScmObj** %stackaddr$prim55698, align 8
%stackaddr$prim55699 = alloca %struct.ScmObj*, align 8
%argslist54425$_37map1480772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48209, %struct.ScmObj* %argslist54425$_37map1480771)
store volatile %struct.ScmObj* %argslist54425$_37map1480772, %struct.ScmObj** %stackaddr$prim55699, align 8
%stackaddr$prim55700 = alloca %struct.ScmObj*, align 8
%argslist54425$_37map1480773 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49863, %struct.ScmObj* %argslist54425$_37map1480772)
store volatile %struct.ScmObj* %argslist54425$_37map1480773, %struct.ScmObj** %stackaddr$prim55700, align 8
%clofunc55701 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148077)
musttail call tailcc void %clofunc55701(%struct.ScmObj* %_37map148077, %struct.ScmObj* %argslist54425$_37map1480773)
ret void
}

define tailcc void @proc_clo$ae49863(%struct.ScmObj* %env$ae49863,%struct.ScmObj* %current_45args54406) {
%stackaddr$env-ref55702 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49863, i64 0)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref55702
%stackaddr$env-ref55703 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49863, i64 1)
store %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$env-ref55703
%stackaddr$env-ref55704 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49863, i64 2)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55704
%stackaddr$env-ref55705 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49863, i64 3)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55705
%stackaddr$env-ref55706 = alloca %struct.ScmObj*, align 8
%k48352 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49863, i64 4)
store %struct.ScmObj* %k48352, %struct.ScmObj** %stackaddr$env-ref55706
%stackaddr$env-ref55707 = alloca %struct.ScmObj*, align 8
%lsts_4348138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49863, i64 5)
store %struct.ScmObj* %lsts_4348138, %struct.ScmObj** %stackaddr$env-ref55707
%stackaddr$prim55708 = alloca %struct.ScmObj*, align 8
%_95k48358 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54406)
store volatile %struct.ScmObj* %_95k48358, %struct.ScmObj** %stackaddr$prim55708, align 8
%stackaddr$prim55709 = alloca %struct.ScmObj*, align 8
%current_45args54407 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54406)
store volatile %struct.ScmObj* %current_45args54407, %struct.ScmObj** %stackaddr$prim55709, align 8
%stackaddr$prim55710 = alloca %struct.ScmObj*, align 8
%vs48136 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54407)
store volatile %struct.ScmObj* %vs48136, %struct.ScmObj** %stackaddr$prim55710, align 8
%stackaddr$makeclosure55711 = alloca %struct.ScmObj*, align 8
%fptrToInt55712 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49866 to i64
%ae49866 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55712)
store volatile %struct.ScmObj* %ae49866, %struct.ScmObj** %stackaddr$makeclosure55711, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49866, %struct.ScmObj* %f48133, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49866, %struct.ScmObj* %acc48132, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49866, %struct.ScmObj* %_37foldr48051, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49866, %struct.ScmObj* %_37foldl48129, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49866, %struct.ScmObj* %k48352, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49866, %struct.ScmObj* %lsts_4348138, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49866, %struct.ScmObj* %vs48136, i64 6)
%ae49867 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55713 = alloca %struct.ScmObj*, align 8
%fptrToInt55714 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49868 to i64
%ae49868 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55714)
store volatile %struct.ScmObj* %ae49868, %struct.ScmObj** %stackaddr$makeclosure55713, align 8
%argslist54424$ae498660 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55715 = alloca %struct.ScmObj*, align 8
%argslist54424$ae498661 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49868, %struct.ScmObj* %argslist54424$ae498660)
store volatile %struct.ScmObj* %argslist54424$ae498661, %struct.ScmObj** %stackaddr$prim55715, align 8
%stackaddr$prim55716 = alloca %struct.ScmObj*, align 8
%argslist54424$ae498662 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49867, %struct.ScmObj* %argslist54424$ae498661)
store volatile %struct.ScmObj* %argslist54424$ae498662, %struct.ScmObj** %stackaddr$prim55716, align 8
%clofunc55717 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49866)
musttail call tailcc void %clofunc55717(%struct.ScmObj* %ae49866, %struct.ScmObj* %argslist54424$ae498662)
ret void
}

define tailcc void @proc_clo$ae49866(%struct.ScmObj* %env$ae49866,%struct.ScmObj* %current_45args54409) {
%stackaddr$env-ref55718 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49866, i64 0)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref55718
%stackaddr$env-ref55719 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49866, i64 1)
store %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$env-ref55719
%stackaddr$env-ref55720 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49866, i64 2)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55720
%stackaddr$env-ref55721 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49866, i64 3)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55721
%stackaddr$env-ref55722 = alloca %struct.ScmObj*, align 8
%k48352 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49866, i64 4)
store %struct.ScmObj* %k48352, %struct.ScmObj** %stackaddr$env-ref55722
%stackaddr$env-ref55723 = alloca %struct.ScmObj*, align 8
%lsts_4348138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49866, i64 5)
store %struct.ScmObj* %lsts_4348138, %struct.ScmObj** %stackaddr$env-ref55723
%stackaddr$env-ref55724 = alloca %struct.ScmObj*, align 8
%vs48136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49866, i64 6)
store %struct.ScmObj* %vs48136, %struct.ScmObj** %stackaddr$env-ref55724
%stackaddr$prim55725 = alloca %struct.ScmObj*, align 8
%_95k48359 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54409)
store volatile %struct.ScmObj* %_95k48359, %struct.ScmObj** %stackaddr$prim55725, align 8
%stackaddr$prim55726 = alloca %struct.ScmObj*, align 8
%current_45args54410 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54409)
store volatile %struct.ScmObj* %current_45args54410, %struct.ScmObj** %stackaddr$prim55726, align 8
%stackaddr$prim55727 = alloca %struct.ScmObj*, align 8
%anf_45bind48210 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54410)
store volatile %struct.ScmObj* %anf_45bind48210, %struct.ScmObj** %stackaddr$prim55727, align 8
%ae49889 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55728 = alloca %struct.ScmObj*, align 8
%anf_45bind48211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48132, %struct.ScmObj* %ae49889)
store volatile %struct.ScmObj* %anf_45bind48211, %struct.ScmObj** %stackaddr$prim55728, align 8
%stackaddr$makeclosure55729 = alloca %struct.ScmObj*, align 8
%fptrToInt55730 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49891 to i64
%ae49891 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55730)
store volatile %struct.ScmObj* %ae49891, %struct.ScmObj** %stackaddr$makeclosure55729, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49891, %struct.ScmObj* %f48133, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49891, %struct.ScmObj* %_37foldl48129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49891, %struct.ScmObj* %k48352, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49891, %struct.ScmObj* %lsts_4348138, i64 3)
%argslist54418$_37foldr480510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55731 = alloca %struct.ScmObj*, align 8
%argslist54418$_37foldr480511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs48136, %struct.ScmObj* %argslist54418$_37foldr480510)
store volatile %struct.ScmObj* %argslist54418$_37foldr480511, %struct.ScmObj** %stackaddr$prim55731, align 8
%stackaddr$prim55732 = alloca %struct.ScmObj*, align 8
%argslist54418$_37foldr480512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48211, %struct.ScmObj* %argslist54418$_37foldr480511)
store volatile %struct.ScmObj* %argslist54418$_37foldr480512, %struct.ScmObj** %stackaddr$prim55732, align 8
%stackaddr$prim55733 = alloca %struct.ScmObj*, align 8
%argslist54418$_37foldr480513 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48210, %struct.ScmObj* %argslist54418$_37foldr480512)
store volatile %struct.ScmObj* %argslist54418$_37foldr480513, %struct.ScmObj** %stackaddr$prim55733, align 8
%stackaddr$prim55734 = alloca %struct.ScmObj*, align 8
%argslist54418$_37foldr480514 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49891, %struct.ScmObj* %argslist54418$_37foldr480513)
store volatile %struct.ScmObj* %argslist54418$_37foldr480514, %struct.ScmObj** %stackaddr$prim55734, align 8
%clofunc55735 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr48051)
musttail call tailcc void %clofunc55735(%struct.ScmObj* %_37foldr48051, %struct.ScmObj* %argslist54418$_37foldr480514)
ret void
}

define tailcc void @proc_clo$ae49891(%struct.ScmObj* %env$ae49891,%struct.ScmObj* %current_45args54412) {
%stackaddr$env-ref55736 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49891, i64 0)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref55736
%stackaddr$env-ref55737 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49891, i64 1)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55737
%stackaddr$env-ref55738 = alloca %struct.ScmObj*, align 8
%k48352 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49891, i64 2)
store %struct.ScmObj* %k48352, %struct.ScmObj** %stackaddr$env-ref55738
%stackaddr$env-ref55739 = alloca %struct.ScmObj*, align 8
%lsts_4348138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49891, i64 3)
store %struct.ScmObj* %lsts_4348138, %struct.ScmObj** %stackaddr$env-ref55739
%stackaddr$prim55740 = alloca %struct.ScmObj*, align 8
%_95k48360 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54412)
store volatile %struct.ScmObj* %_95k48360, %struct.ScmObj** %stackaddr$prim55740, align 8
%stackaddr$prim55741 = alloca %struct.ScmObj*, align 8
%current_45args54413 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54412)
store volatile %struct.ScmObj* %current_45args54413, %struct.ScmObj** %stackaddr$prim55741, align 8
%stackaddr$prim55742 = alloca %struct.ScmObj*, align 8
%anf_45bind48212 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54413)
store volatile %struct.ScmObj* %anf_45bind48212, %struct.ScmObj** %stackaddr$prim55742, align 8
%stackaddr$makeclosure55743 = alloca %struct.ScmObj*, align 8
%fptrToInt55744 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49895 to i64
%ae49895 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55744)
store volatile %struct.ScmObj* %ae49895, %struct.ScmObj** %stackaddr$makeclosure55743, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49895, %struct.ScmObj* %f48133, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49895, %struct.ScmObj* %_37foldl48129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49895, %struct.ScmObj* %k48352, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49895, %struct.ScmObj* %lsts_4348138, i64 3)
%stackaddr$prim55745 = alloca %struct.ScmObj*, align 8
%cpsargs48363 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49895, %struct.ScmObj* %anf_45bind48212)
store volatile %struct.ScmObj* %cpsargs48363, %struct.ScmObj** %stackaddr$prim55745, align 8
%clofunc55746 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48133)
musttail call tailcc void %clofunc55746(%struct.ScmObj* %f48133, %struct.ScmObj* %cpsargs48363)
ret void
}

define tailcc void @proc_clo$ae49895(%struct.ScmObj* %env$ae49895,%struct.ScmObj* %current_45args54415) {
%stackaddr$env-ref55747 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49895, i64 0)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref55747
%stackaddr$env-ref55748 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49895, i64 1)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55748
%stackaddr$env-ref55749 = alloca %struct.ScmObj*, align 8
%k48352 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49895, i64 2)
store %struct.ScmObj* %k48352, %struct.ScmObj** %stackaddr$env-ref55749
%stackaddr$env-ref55750 = alloca %struct.ScmObj*, align 8
%lsts_4348138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49895, i64 3)
store %struct.ScmObj* %lsts_4348138, %struct.ScmObj** %stackaddr$env-ref55750
%stackaddr$prim55751 = alloca %struct.ScmObj*, align 8
%_95k48361 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54415)
store volatile %struct.ScmObj* %_95k48361, %struct.ScmObj** %stackaddr$prim55751, align 8
%stackaddr$prim55752 = alloca %struct.ScmObj*, align 8
%current_45args54416 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54415)
store volatile %struct.ScmObj* %current_45args54416, %struct.ScmObj** %stackaddr$prim55752, align 8
%stackaddr$prim55753 = alloca %struct.ScmObj*, align 8
%acc_4348140 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54416)
store volatile %struct.ScmObj* %acc_4348140, %struct.ScmObj** %stackaddr$prim55753, align 8
%stackaddr$prim55754 = alloca %struct.ScmObj*, align 8
%anf_45bind48213 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4348140, %struct.ScmObj* %lsts_4348138)
store volatile %struct.ScmObj* %anf_45bind48213, %struct.ScmObj** %stackaddr$prim55754, align 8
%stackaddr$prim55755 = alloca %struct.ScmObj*, align 8
%anf_45bind48214 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48133, %struct.ScmObj* %anf_45bind48213)
store volatile %struct.ScmObj* %anf_45bind48214, %struct.ScmObj** %stackaddr$prim55755, align 8
%stackaddr$prim55756 = alloca %struct.ScmObj*, align 8
%cpsargs48362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48352, %struct.ScmObj* %anf_45bind48214)
store volatile %struct.ScmObj* %cpsargs48362, %struct.ScmObj** %stackaddr$prim55756, align 8
%clofunc55757 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl48129)
musttail call tailcc void %clofunc55757(%struct.ScmObj* %_37foldl48129, %struct.ScmObj* %cpsargs48362)
ret void
}

define tailcc void @proc_clo$ae49868(%struct.ScmObj* %env$ae49868,%struct.ScmObj* %current_45args54419) {
%stackaddr$prim55758 = alloca %struct.ScmObj*, align 8
%k48364 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54419)
store volatile %struct.ScmObj* %k48364, %struct.ScmObj** %stackaddr$prim55758, align 8
%stackaddr$prim55759 = alloca %struct.ScmObj*, align 8
%current_45args54420 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54419)
store volatile %struct.ScmObj* %current_45args54420, %struct.ScmObj** %stackaddr$prim55759, align 8
%stackaddr$prim55760 = alloca %struct.ScmObj*, align 8
%a48142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54420)
store volatile %struct.ScmObj* %a48142, %struct.ScmObj** %stackaddr$prim55760, align 8
%stackaddr$prim55761 = alloca %struct.ScmObj*, align 8
%current_45args54421 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54420)
store volatile %struct.ScmObj* %current_45args54421, %struct.ScmObj** %stackaddr$prim55761, align 8
%stackaddr$prim55762 = alloca %struct.ScmObj*, align 8
%b48141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54421)
store volatile %struct.ScmObj* %b48141, %struct.ScmObj** %stackaddr$prim55762, align 8
%stackaddr$prim55763 = alloca %struct.ScmObj*, align 8
%cpsprim48365 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a48142, %struct.ScmObj* %b48141)
store volatile %struct.ScmObj* %cpsprim48365, %struct.ScmObj** %stackaddr$prim55763, align 8
%ae49872 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54423$k483640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55764 = alloca %struct.ScmObj*, align 8
%argslist54423$k483641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48365, %struct.ScmObj* %argslist54423$k483640)
store volatile %struct.ScmObj* %argslist54423$k483641, %struct.ScmObj** %stackaddr$prim55764, align 8
%stackaddr$prim55765 = alloca %struct.ScmObj*, align 8
%argslist54423$k483642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49872, %struct.ScmObj* %argslist54423$k483641)
store volatile %struct.ScmObj* %argslist54423$k483642, %struct.ScmObj** %stackaddr$prim55765, align 8
%clofunc55766 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48364)
musttail call tailcc void %clofunc55766(%struct.ScmObj* %k48364, %struct.ScmObj* %argslist54423$k483642)
ret void
}

define tailcc void @proc_clo$ae49844(%struct.ScmObj* %env$ae49844,%struct.ScmObj* %current_45args54426) {
%stackaddr$prim55767 = alloca %struct.ScmObj*, align 8
%k48366 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54426)
store volatile %struct.ScmObj* %k48366, %struct.ScmObj** %stackaddr$prim55767, align 8
%stackaddr$prim55768 = alloca %struct.ScmObj*, align 8
%current_45args54427 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54426)
store volatile %struct.ScmObj* %current_45args54427, %struct.ScmObj** %stackaddr$prim55768, align 8
%stackaddr$prim55769 = alloca %struct.ScmObj*, align 8
%x48137 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54427)
store volatile %struct.ScmObj* %x48137, %struct.ScmObj** %stackaddr$prim55769, align 8
%stackaddr$prim55770 = alloca %struct.ScmObj*, align 8
%cpsprim48367 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48137)
store volatile %struct.ScmObj* %cpsprim48367, %struct.ScmObj** %stackaddr$prim55770, align 8
%ae49847 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54429$k483660 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55771 = alloca %struct.ScmObj*, align 8
%argslist54429$k483661 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48367, %struct.ScmObj* %argslist54429$k483660)
store volatile %struct.ScmObj* %argslist54429$k483661, %struct.ScmObj** %stackaddr$prim55771, align 8
%stackaddr$prim55772 = alloca %struct.ScmObj*, align 8
%argslist54429$k483662 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49847, %struct.ScmObj* %argslist54429$k483661)
store volatile %struct.ScmObj* %argslist54429$k483662, %struct.ScmObj** %stackaddr$prim55772, align 8
%clofunc55773 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48366)
musttail call tailcc void %clofunc55773(%struct.ScmObj* %k48366, %struct.ScmObj* %argslist54429$k483662)
ret void
}

define tailcc void @proc_clo$ae49820(%struct.ScmObj* %env$ae49820,%struct.ScmObj* %current_45args54432) {
%stackaddr$prim55774 = alloca %struct.ScmObj*, align 8
%k48368 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54432)
store volatile %struct.ScmObj* %k48368, %struct.ScmObj** %stackaddr$prim55774, align 8
%stackaddr$prim55775 = alloca %struct.ScmObj*, align 8
%current_45args54433 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54432)
store volatile %struct.ScmObj* %current_45args54433, %struct.ScmObj** %stackaddr$prim55775, align 8
%stackaddr$prim55776 = alloca %struct.ScmObj*, align 8
%x48139 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54433)
store volatile %struct.ScmObj* %x48139, %struct.ScmObj** %stackaddr$prim55776, align 8
%stackaddr$prim55777 = alloca %struct.ScmObj*, align 8
%cpsprim48369 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48139)
store volatile %struct.ScmObj* %cpsprim48369, %struct.ScmObj** %stackaddr$prim55777, align 8
%ae49823 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54435$k483680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55778 = alloca %struct.ScmObj*, align 8
%argslist54435$k483681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48369, %struct.ScmObj* %argslist54435$k483680)
store volatile %struct.ScmObj* %argslist54435$k483681, %struct.ScmObj** %stackaddr$prim55778, align 8
%stackaddr$prim55779 = alloca %struct.ScmObj*, align 8
%argslist54435$k483682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49823, %struct.ScmObj* %argslist54435$k483681)
store volatile %struct.ScmObj* %argslist54435$k483682, %struct.ScmObj** %stackaddr$prim55779, align 8
%clofunc55780 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48368)
musttail call tailcc void %clofunc55780(%struct.ScmObj* %k48368, %struct.ScmObj* %argslist54435$k483682)
ret void
}

define tailcc void @proc_clo$ae49772(%struct.ScmObj* %env$ae49772,%struct.ScmObj* %current_45args54438) {
%stackaddr$prim55781 = alloca %struct.ScmObj*, align 8
%k48370 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54438)
store volatile %struct.ScmObj* %k48370, %struct.ScmObj** %stackaddr$prim55781, align 8
%stackaddr$prim55782 = alloca %struct.ScmObj*, align 8
%current_45args54439 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54438)
store volatile %struct.ScmObj* %current_45args54439, %struct.ScmObj** %stackaddr$prim55782, align 8
%stackaddr$prim55783 = alloca %struct.ScmObj*, align 8
%lst48135 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54439)
store volatile %struct.ScmObj* %lst48135, %struct.ScmObj** %stackaddr$prim55783, align 8
%stackaddr$prim55784 = alloca %struct.ScmObj*, align 8
%current_45args54440 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54439)
store volatile %struct.ScmObj* %current_45args54440, %struct.ScmObj** %stackaddr$prim55784, align 8
%stackaddr$prim55785 = alloca %struct.ScmObj*, align 8
%b48134 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54440)
store volatile %struct.ScmObj* %b48134, %struct.ScmObj** %stackaddr$prim55785, align 8
%truthy$cmp55786 = call i64 @is_truthy_value(%struct.ScmObj* %b48134)
%cmp$cmp55786 = icmp eq i64 %truthy$cmp55786, 1
br i1 %cmp$cmp55786, label %truebranch$cmp55786, label %falsebranch$cmp55786
truebranch$cmp55786:
%ae49775 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54442$k483700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55787 = alloca %struct.ScmObj*, align 8
%argslist54442$k483701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b48134, %struct.ScmObj* %argslist54442$k483700)
store volatile %struct.ScmObj* %argslist54442$k483701, %struct.ScmObj** %stackaddr$prim55787, align 8
%stackaddr$prim55788 = alloca %struct.ScmObj*, align 8
%argslist54442$k483702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49775, %struct.ScmObj* %argslist54442$k483701)
store volatile %struct.ScmObj* %argslist54442$k483702, %struct.ScmObj** %stackaddr$prim55788, align 8
%clofunc55789 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48370)
musttail call tailcc void %clofunc55789(%struct.ScmObj* %k48370, %struct.ScmObj* %argslist54442$k483702)
ret void
falsebranch$cmp55786:
%stackaddr$prim55790 = alloca %struct.ScmObj*, align 8
%cpsprim48371 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48135)
store volatile %struct.ScmObj* %cpsprim48371, %struct.ScmObj** %stackaddr$prim55790, align 8
%ae49782 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54443$k483700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55791 = alloca %struct.ScmObj*, align 8
%argslist54443$k483701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48371, %struct.ScmObj* %argslist54443$k483700)
store volatile %struct.ScmObj* %argslist54443$k483701, %struct.ScmObj** %stackaddr$prim55791, align 8
%stackaddr$prim55792 = alloca %struct.ScmObj*, align 8
%argslist54443$k483702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49782, %struct.ScmObj* %argslist54443$k483701)
store volatile %struct.ScmObj* %argslist54443$k483702, %struct.ScmObj** %stackaddr$prim55792, align 8
%clofunc55793 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48370)
musttail call tailcc void %clofunc55793(%struct.ScmObj* %k48370, %struct.ScmObj* %argslist54443$k483702)
ret void
}

define tailcc void @proc_clo$ae49613(%struct.ScmObj* %env$ae49613,%struct.ScmObj* %args4807348372) {
%stackaddr$env-ref55794 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49613, i64 0)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref55794
%stackaddr$env-ref55795 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49613, i64 1)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55795
%stackaddr$env-ref55796 = alloca %struct.ScmObj*, align 8
%_37drop_45right48065 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49613, i64 2)
store %struct.ScmObj* %_37drop_45right48065, %struct.ScmObj** %stackaddr$env-ref55796
%stackaddr$prim55797 = alloca %struct.ScmObj*, align 8
%k48373 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4807348372)
store volatile %struct.ScmObj* %k48373, %struct.ScmObj** %stackaddr$prim55797, align 8
%stackaddr$prim55798 = alloca %struct.ScmObj*, align 8
%args48073 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4807348372)
store volatile %struct.ScmObj* %args48073, %struct.ScmObj** %stackaddr$prim55798, align 8
%stackaddr$prim55799 = alloca %struct.ScmObj*, align 8
%f48075 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48073)
store volatile %struct.ScmObj* %f48075, %struct.ScmObj** %stackaddr$prim55799, align 8
%stackaddr$prim55800 = alloca %struct.ScmObj*, align 8
%lsts48074 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48073)
store volatile %struct.ScmObj* %lsts48074, %struct.ScmObj** %stackaddr$prim55800, align 8
%stackaddr$makeclosure55801 = alloca %struct.ScmObj*, align 8
%fptrToInt55802 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49618 to i64
%ae49618 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55802)
store volatile %struct.ScmObj* %ae49618, %struct.ScmObj** %stackaddr$makeclosure55801, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49618, %struct.ScmObj* %k48373, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49618, %struct.ScmObj* %_37foldr48051, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49618, %struct.ScmObj* %lsts48074, i64 2)
%ae49619 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55803 = alloca %struct.ScmObj*, align 8
%fptrToInt55804 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49620 to i64
%ae49620 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55804)
store volatile %struct.ScmObj* %ae49620, %struct.ScmObj** %stackaddr$makeclosure55803, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49620, %struct.ScmObj* %_37last48068, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49620, %struct.ScmObj* %_37drop_45right48065, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49620, %struct.ScmObj* %f48075, i64 2)
%argslist54462$ae496180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55805 = alloca %struct.ScmObj*, align 8
%argslist54462$ae496181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49620, %struct.ScmObj* %argslist54462$ae496180)
store volatile %struct.ScmObj* %argslist54462$ae496181, %struct.ScmObj** %stackaddr$prim55805, align 8
%stackaddr$prim55806 = alloca %struct.ScmObj*, align 8
%argslist54462$ae496182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49619, %struct.ScmObj* %argslist54462$ae496181)
store volatile %struct.ScmObj* %argslist54462$ae496182, %struct.ScmObj** %stackaddr$prim55806, align 8
%clofunc55807 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49618)
musttail call tailcc void %clofunc55807(%struct.ScmObj* %ae49618, %struct.ScmObj* %argslist54462$ae496182)
ret void
}

define tailcc void @proc_clo$ae49618(%struct.ScmObj* %env$ae49618,%struct.ScmObj* %current_45args54447) {
%stackaddr$env-ref55808 = alloca %struct.ScmObj*, align 8
%k48373 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49618, i64 0)
store %struct.ScmObj* %k48373, %struct.ScmObj** %stackaddr$env-ref55808
%stackaddr$env-ref55809 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49618, i64 1)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55809
%stackaddr$env-ref55810 = alloca %struct.ScmObj*, align 8
%lsts48074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49618, i64 2)
store %struct.ScmObj* %lsts48074, %struct.ScmObj** %stackaddr$env-ref55810
%stackaddr$prim55811 = alloca %struct.ScmObj*, align 8
%_95k48374 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54447)
store volatile %struct.ScmObj* %_95k48374, %struct.ScmObj** %stackaddr$prim55811, align 8
%stackaddr$prim55812 = alloca %struct.ScmObj*, align 8
%current_45args54448 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54447)
store volatile %struct.ScmObj* %current_45args54448, %struct.ScmObj** %stackaddr$prim55812, align 8
%stackaddr$prim55813 = alloca %struct.ScmObj*, align 8
%anf_45bind48201 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54448)
store volatile %struct.ScmObj* %anf_45bind48201, %struct.ScmObj** %stackaddr$prim55813, align 8
%ae49681 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55814 = alloca %struct.ScmObj*, align 8
%anf_45bind48202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49681, %struct.ScmObj* %lsts48074)
store volatile %struct.ScmObj* %anf_45bind48202, %struct.ScmObj** %stackaddr$prim55814, align 8
%stackaddr$prim55815 = alloca %struct.ScmObj*, align 8
%anf_45bind48203 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48201, %struct.ScmObj* %anf_45bind48202)
store volatile %struct.ScmObj* %anf_45bind48203, %struct.ScmObj** %stackaddr$prim55815, align 8
%stackaddr$prim55816 = alloca %struct.ScmObj*, align 8
%cpsargs48375 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48373, %struct.ScmObj* %anf_45bind48203)
store volatile %struct.ScmObj* %cpsargs48375, %struct.ScmObj** %stackaddr$prim55816, align 8
%clofunc55817 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr48051)
musttail call tailcc void %clofunc55817(%struct.ScmObj* %_37foldr48051, %struct.ScmObj* %cpsargs48375)
ret void
}

define tailcc void @proc_clo$ae49620(%struct.ScmObj* %env$ae49620,%struct.ScmObj* %fargs4807648376) {
%stackaddr$env-ref55818 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49620, i64 0)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref55818
%stackaddr$env-ref55819 = alloca %struct.ScmObj*, align 8
%_37drop_45right48065 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49620, i64 1)
store %struct.ScmObj* %_37drop_45right48065, %struct.ScmObj** %stackaddr$env-ref55819
%stackaddr$env-ref55820 = alloca %struct.ScmObj*, align 8
%f48075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49620, i64 2)
store %struct.ScmObj* %f48075, %struct.ScmObj** %stackaddr$env-ref55820
%stackaddr$prim55821 = alloca %struct.ScmObj*, align 8
%k48377 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs4807648376)
store volatile %struct.ScmObj* %k48377, %struct.ScmObj** %stackaddr$prim55821, align 8
%stackaddr$prim55822 = alloca %struct.ScmObj*, align 8
%fargs48076 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs4807648376)
store volatile %struct.ScmObj* %fargs48076, %struct.ScmObj** %stackaddr$prim55822, align 8
%stackaddr$makeclosure55823 = alloca %struct.ScmObj*, align 8
%fptrToInt55824 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49624 to i64
%ae49624 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55824)
store volatile %struct.ScmObj* %ae49624, %struct.ScmObj** %stackaddr$makeclosure55823, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49624, %struct.ScmObj* %_37last48068, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49624, %struct.ScmObj* %fargs48076, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49624, %struct.ScmObj* %f48075, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49624, %struct.ScmObj* %k48377, i64 3)
%ae49626 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist54461$_37drop_45right480650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55825 = alloca %struct.ScmObj*, align 8
%argslist54461$_37drop_45right480651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49626, %struct.ScmObj* %argslist54461$_37drop_45right480650)
store volatile %struct.ScmObj* %argslist54461$_37drop_45right480651, %struct.ScmObj** %stackaddr$prim55825, align 8
%stackaddr$prim55826 = alloca %struct.ScmObj*, align 8
%argslist54461$_37drop_45right480652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs48076, %struct.ScmObj* %argslist54461$_37drop_45right480651)
store volatile %struct.ScmObj* %argslist54461$_37drop_45right480652, %struct.ScmObj** %stackaddr$prim55826, align 8
%stackaddr$prim55827 = alloca %struct.ScmObj*, align 8
%argslist54461$_37drop_45right480653 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49624, %struct.ScmObj* %argslist54461$_37drop_45right480652)
store volatile %struct.ScmObj* %argslist54461$_37drop_45right480653, %struct.ScmObj** %stackaddr$prim55827, align 8
%clofunc55828 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right48065)
musttail call tailcc void %clofunc55828(%struct.ScmObj* %_37drop_45right48065, %struct.ScmObj* %argslist54461$_37drop_45right480653)
ret void
}

define tailcc void @proc_clo$ae49624(%struct.ScmObj* %env$ae49624,%struct.ScmObj* %current_45args54450) {
%stackaddr$env-ref55829 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49624, i64 0)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref55829
%stackaddr$env-ref55830 = alloca %struct.ScmObj*, align 8
%fargs48076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49624, i64 1)
store %struct.ScmObj* %fargs48076, %struct.ScmObj** %stackaddr$env-ref55830
%stackaddr$env-ref55831 = alloca %struct.ScmObj*, align 8
%f48075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49624, i64 2)
store %struct.ScmObj* %f48075, %struct.ScmObj** %stackaddr$env-ref55831
%stackaddr$env-ref55832 = alloca %struct.ScmObj*, align 8
%k48377 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49624, i64 3)
store %struct.ScmObj* %k48377, %struct.ScmObj** %stackaddr$env-ref55832
%stackaddr$prim55833 = alloca %struct.ScmObj*, align 8
%_95k48378 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54450)
store volatile %struct.ScmObj* %_95k48378, %struct.ScmObj** %stackaddr$prim55833, align 8
%stackaddr$prim55834 = alloca %struct.ScmObj*, align 8
%current_45args54451 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54450)
store volatile %struct.ScmObj* %current_45args54451, %struct.ScmObj** %stackaddr$prim55834, align 8
%stackaddr$prim55835 = alloca %struct.ScmObj*, align 8
%anf_45bind48198 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54451)
store volatile %struct.ScmObj* %anf_45bind48198, %struct.ScmObj** %stackaddr$prim55835, align 8
%stackaddr$makeclosure55836 = alloca %struct.ScmObj*, align 8
%fptrToInt55837 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49631 to i64
%ae49631 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55837)
store volatile %struct.ScmObj* %ae49631, %struct.ScmObj** %stackaddr$makeclosure55836, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49631, %struct.ScmObj* %_37last48068, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49631, %struct.ScmObj* %fargs48076, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49631, %struct.ScmObj* %k48377, i64 2)
%stackaddr$prim55838 = alloca %struct.ScmObj*, align 8
%cpsargs48382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49631, %struct.ScmObj* %anf_45bind48198)
store volatile %struct.ScmObj* %cpsargs48382, %struct.ScmObj** %stackaddr$prim55838, align 8
%clofunc55839 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48075)
musttail call tailcc void %clofunc55839(%struct.ScmObj* %f48075, %struct.ScmObj* %cpsargs48382)
ret void
}

define tailcc void @proc_clo$ae49631(%struct.ScmObj* %env$ae49631,%struct.ScmObj* %current_45args54453) {
%stackaddr$env-ref55840 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49631, i64 0)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref55840
%stackaddr$env-ref55841 = alloca %struct.ScmObj*, align 8
%fargs48076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49631, i64 1)
store %struct.ScmObj* %fargs48076, %struct.ScmObj** %stackaddr$env-ref55841
%stackaddr$env-ref55842 = alloca %struct.ScmObj*, align 8
%k48377 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49631, i64 2)
store %struct.ScmObj* %k48377, %struct.ScmObj** %stackaddr$env-ref55842
%stackaddr$prim55843 = alloca %struct.ScmObj*, align 8
%_95k48379 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54453)
store volatile %struct.ScmObj* %_95k48379, %struct.ScmObj** %stackaddr$prim55843, align 8
%stackaddr$prim55844 = alloca %struct.ScmObj*, align 8
%current_45args54454 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54453)
store volatile %struct.ScmObj* %current_45args54454, %struct.ScmObj** %stackaddr$prim55844, align 8
%stackaddr$prim55845 = alloca %struct.ScmObj*, align 8
%anf_45bind48199 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54454)
store volatile %struct.ScmObj* %anf_45bind48199, %struct.ScmObj** %stackaddr$prim55845, align 8
%stackaddr$makeclosure55846 = alloca %struct.ScmObj*, align 8
%fptrToInt55847 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49636 to i64
%ae49636 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55847)
store volatile %struct.ScmObj* %ae49636, %struct.ScmObj** %stackaddr$makeclosure55846, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49636, %struct.ScmObj* %anf_45bind48199, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49636, %struct.ScmObj* %k48377, i64 1)
%argslist54460$_37last480680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55848 = alloca %struct.ScmObj*, align 8
%argslist54460$_37last480681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs48076, %struct.ScmObj* %argslist54460$_37last480680)
store volatile %struct.ScmObj* %argslist54460$_37last480681, %struct.ScmObj** %stackaddr$prim55848, align 8
%stackaddr$prim55849 = alloca %struct.ScmObj*, align 8
%argslist54460$_37last480682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49636, %struct.ScmObj* %argslist54460$_37last480681)
store volatile %struct.ScmObj* %argslist54460$_37last480682, %struct.ScmObj** %stackaddr$prim55849, align 8
%clofunc55850 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last48068)
musttail call tailcc void %clofunc55850(%struct.ScmObj* %_37last48068, %struct.ScmObj* %argslist54460$_37last480682)
ret void
}

define tailcc void @proc_clo$ae49636(%struct.ScmObj* %env$ae49636,%struct.ScmObj* %current_45args54456) {
%stackaddr$env-ref55851 = alloca %struct.ScmObj*, align 8
%anf_45bind48199 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49636, i64 0)
store %struct.ScmObj* %anf_45bind48199, %struct.ScmObj** %stackaddr$env-ref55851
%stackaddr$env-ref55852 = alloca %struct.ScmObj*, align 8
%k48377 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49636, i64 1)
store %struct.ScmObj* %k48377, %struct.ScmObj** %stackaddr$env-ref55852
%stackaddr$prim55853 = alloca %struct.ScmObj*, align 8
%_95k48380 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54456)
store volatile %struct.ScmObj* %_95k48380, %struct.ScmObj** %stackaddr$prim55853, align 8
%stackaddr$prim55854 = alloca %struct.ScmObj*, align 8
%current_45args54457 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54456)
store volatile %struct.ScmObj* %current_45args54457, %struct.ScmObj** %stackaddr$prim55854, align 8
%stackaddr$prim55855 = alloca %struct.ScmObj*, align 8
%anf_45bind48200 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54457)
store volatile %struct.ScmObj* %anf_45bind48200, %struct.ScmObj** %stackaddr$prim55855, align 8
%stackaddr$prim55856 = alloca %struct.ScmObj*, align 8
%cpsprim48381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48199, %struct.ScmObj* %anf_45bind48200)
store volatile %struct.ScmObj* %cpsprim48381, %struct.ScmObj** %stackaddr$prim55856, align 8
%ae49641 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54459$k483770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55857 = alloca %struct.ScmObj*, align 8
%argslist54459$k483771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48381, %struct.ScmObj* %argslist54459$k483770)
store volatile %struct.ScmObj* %argslist54459$k483771, %struct.ScmObj** %stackaddr$prim55857, align 8
%stackaddr$prim55858 = alloca %struct.ScmObj*, align 8
%argslist54459$k483772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49641, %struct.ScmObj* %argslist54459$k483771)
store volatile %struct.ScmObj* %argslist54459$k483772, %struct.ScmObj** %stackaddr$prim55858, align 8
%clofunc55859 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48377)
musttail call tailcc void %clofunc55859(%struct.ScmObj* %k48377, %struct.ScmObj* %argslist54459$k483772)
ret void
}

define tailcc void @proc_clo$ae49536(%struct.ScmObj* %env$ae49536,%struct.ScmObj* %current_45args54464) {
%stackaddr$env-ref55860 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49536, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55860
%stackaddr$prim55861 = alloca %struct.ScmObj*, align 8
%k48383 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54464)
store volatile %struct.ScmObj* %k48383, %struct.ScmObj** %stackaddr$prim55861, align 8
%stackaddr$prim55862 = alloca %struct.ScmObj*, align 8
%current_45args54465 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54464)
store volatile %struct.ScmObj* %current_45args54465, %struct.ScmObj** %stackaddr$prim55862, align 8
%stackaddr$prim55863 = alloca %struct.ScmObj*, align 8
%f48079 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54465)
store volatile %struct.ScmObj* %f48079, %struct.ScmObj** %stackaddr$prim55863, align 8
%stackaddr$prim55864 = alloca %struct.ScmObj*, align 8
%current_45args54466 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54465)
store volatile %struct.ScmObj* %current_45args54466, %struct.ScmObj** %stackaddr$prim55864, align 8
%stackaddr$prim55865 = alloca %struct.ScmObj*, align 8
%lst48078 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54466)
store volatile %struct.ScmObj* %lst48078, %struct.ScmObj** %stackaddr$prim55865, align 8
%stackaddr$makeclosure55866 = alloca %struct.ScmObj*, align 8
%fptrToInt55867 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49537 to i64
%ae49537 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55867)
store volatile %struct.ScmObj* %ae49537, %struct.ScmObj** %stackaddr$makeclosure55866, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49537, %struct.ScmObj* %lst48078, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49537, %struct.ScmObj* %_37foldr148046, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49537, %struct.ScmObj* %k48383, i64 2)
%ae49538 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55868 = alloca %struct.ScmObj*, align 8
%fptrToInt55869 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49539 to i64
%ae49539 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55869)
store volatile %struct.ScmObj* %ae49539, %struct.ScmObj** %stackaddr$makeclosure55868, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49539, %struct.ScmObj* %f48079, i64 0)
%argslist54481$ae495370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55870 = alloca %struct.ScmObj*, align 8
%argslist54481$ae495371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49539, %struct.ScmObj* %argslist54481$ae495370)
store volatile %struct.ScmObj* %argslist54481$ae495371, %struct.ScmObj** %stackaddr$prim55870, align 8
%stackaddr$prim55871 = alloca %struct.ScmObj*, align 8
%argslist54481$ae495372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49538, %struct.ScmObj* %argslist54481$ae495371)
store volatile %struct.ScmObj* %argslist54481$ae495372, %struct.ScmObj** %stackaddr$prim55871, align 8
%clofunc55872 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49537)
musttail call tailcc void %clofunc55872(%struct.ScmObj* %ae49537, %struct.ScmObj* %argslist54481$ae495372)
ret void
}

define tailcc void @proc_clo$ae49537(%struct.ScmObj* %env$ae49537,%struct.ScmObj* %current_45args54468) {
%stackaddr$env-ref55873 = alloca %struct.ScmObj*, align 8
%lst48078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49537, i64 0)
store %struct.ScmObj* %lst48078, %struct.ScmObj** %stackaddr$env-ref55873
%stackaddr$env-ref55874 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49537, i64 1)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55874
%stackaddr$env-ref55875 = alloca %struct.ScmObj*, align 8
%k48383 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49537, i64 2)
store %struct.ScmObj* %k48383, %struct.ScmObj** %stackaddr$env-ref55875
%stackaddr$prim55876 = alloca %struct.ScmObj*, align 8
%_95k48384 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54468)
store volatile %struct.ScmObj* %_95k48384, %struct.ScmObj** %stackaddr$prim55876, align 8
%stackaddr$prim55877 = alloca %struct.ScmObj*, align 8
%current_45args54469 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54468)
store volatile %struct.ScmObj* %current_45args54469, %struct.ScmObj** %stackaddr$prim55877, align 8
%stackaddr$prim55878 = alloca %struct.ScmObj*, align 8
%anf_45bind48197 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54469)
store volatile %struct.ScmObj* %anf_45bind48197, %struct.ScmObj** %stackaddr$prim55878, align 8
%ae49571 = call %struct.ScmObj* @const_init_null()
%argslist54471$_37foldr1480460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55879 = alloca %struct.ScmObj*, align 8
%argslist54471$_37foldr1480461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48078, %struct.ScmObj* %argslist54471$_37foldr1480460)
store volatile %struct.ScmObj* %argslist54471$_37foldr1480461, %struct.ScmObj** %stackaddr$prim55879, align 8
%stackaddr$prim55880 = alloca %struct.ScmObj*, align 8
%argslist54471$_37foldr1480462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49571, %struct.ScmObj* %argslist54471$_37foldr1480461)
store volatile %struct.ScmObj* %argslist54471$_37foldr1480462, %struct.ScmObj** %stackaddr$prim55880, align 8
%stackaddr$prim55881 = alloca %struct.ScmObj*, align 8
%argslist54471$_37foldr1480463 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48197, %struct.ScmObj* %argslist54471$_37foldr1480462)
store volatile %struct.ScmObj* %argslist54471$_37foldr1480463, %struct.ScmObj** %stackaddr$prim55881, align 8
%stackaddr$prim55882 = alloca %struct.ScmObj*, align 8
%argslist54471$_37foldr1480464 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48383, %struct.ScmObj* %argslist54471$_37foldr1480463)
store volatile %struct.ScmObj* %argslist54471$_37foldr1480464, %struct.ScmObj** %stackaddr$prim55882, align 8
%clofunc55883 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148046)
musttail call tailcc void %clofunc55883(%struct.ScmObj* %_37foldr148046, %struct.ScmObj* %argslist54471$_37foldr1480464)
ret void
}

define tailcc void @proc_clo$ae49539(%struct.ScmObj* %env$ae49539,%struct.ScmObj* %current_45args54472) {
%stackaddr$env-ref55884 = alloca %struct.ScmObj*, align 8
%f48079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49539, i64 0)
store %struct.ScmObj* %f48079, %struct.ScmObj** %stackaddr$env-ref55884
%stackaddr$prim55885 = alloca %struct.ScmObj*, align 8
%k48385 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54472)
store volatile %struct.ScmObj* %k48385, %struct.ScmObj** %stackaddr$prim55885, align 8
%stackaddr$prim55886 = alloca %struct.ScmObj*, align 8
%current_45args54473 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54472)
store volatile %struct.ScmObj* %current_45args54473, %struct.ScmObj** %stackaddr$prim55886, align 8
%stackaddr$prim55887 = alloca %struct.ScmObj*, align 8
%v48081 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54473)
store volatile %struct.ScmObj* %v48081, %struct.ScmObj** %stackaddr$prim55887, align 8
%stackaddr$prim55888 = alloca %struct.ScmObj*, align 8
%current_45args54474 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54473)
store volatile %struct.ScmObj* %current_45args54474, %struct.ScmObj** %stackaddr$prim55888, align 8
%stackaddr$prim55889 = alloca %struct.ScmObj*, align 8
%r48080 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54474)
store volatile %struct.ScmObj* %r48080, %struct.ScmObj** %stackaddr$prim55889, align 8
%stackaddr$makeclosure55890 = alloca %struct.ScmObj*, align 8
%fptrToInt55891 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49541 to i64
%ae49541 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55891)
store volatile %struct.ScmObj* %ae49541, %struct.ScmObj** %stackaddr$makeclosure55890, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49541, %struct.ScmObj* %k48385, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49541, %struct.ScmObj* %r48080, i64 1)
%argslist54480$f480790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55892 = alloca %struct.ScmObj*, align 8
%argslist54480$f480791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v48081, %struct.ScmObj* %argslist54480$f480790)
store volatile %struct.ScmObj* %argslist54480$f480791, %struct.ScmObj** %stackaddr$prim55892, align 8
%stackaddr$prim55893 = alloca %struct.ScmObj*, align 8
%argslist54480$f480792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49541, %struct.ScmObj* %argslist54480$f480791)
store volatile %struct.ScmObj* %argslist54480$f480792, %struct.ScmObj** %stackaddr$prim55893, align 8
%clofunc55894 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48079)
musttail call tailcc void %clofunc55894(%struct.ScmObj* %f48079, %struct.ScmObj* %argslist54480$f480792)
ret void
}

define tailcc void @proc_clo$ae49541(%struct.ScmObj* %env$ae49541,%struct.ScmObj* %current_45args54476) {
%stackaddr$env-ref55895 = alloca %struct.ScmObj*, align 8
%k48385 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49541, i64 0)
store %struct.ScmObj* %k48385, %struct.ScmObj** %stackaddr$env-ref55895
%stackaddr$env-ref55896 = alloca %struct.ScmObj*, align 8
%r48080 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49541, i64 1)
store %struct.ScmObj* %r48080, %struct.ScmObj** %stackaddr$env-ref55896
%stackaddr$prim55897 = alloca %struct.ScmObj*, align 8
%_95k48386 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54476)
store volatile %struct.ScmObj* %_95k48386, %struct.ScmObj** %stackaddr$prim55897, align 8
%stackaddr$prim55898 = alloca %struct.ScmObj*, align 8
%current_45args54477 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54476)
store volatile %struct.ScmObj* %current_45args54477, %struct.ScmObj** %stackaddr$prim55898, align 8
%stackaddr$prim55899 = alloca %struct.ScmObj*, align 8
%anf_45bind48196 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54477)
store volatile %struct.ScmObj* %anf_45bind48196, %struct.ScmObj** %stackaddr$prim55899, align 8
%stackaddr$prim55900 = alloca %struct.ScmObj*, align 8
%cpsprim48387 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48196, %struct.ScmObj* %r48080)
store volatile %struct.ScmObj* %cpsprim48387, %struct.ScmObj** %stackaddr$prim55900, align 8
%ae49546 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54479$k483850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55901 = alloca %struct.ScmObj*, align 8
%argslist54479$k483851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48387, %struct.ScmObj* %argslist54479$k483850)
store volatile %struct.ScmObj* %argslist54479$k483851, %struct.ScmObj** %stackaddr$prim55901, align 8
%stackaddr$prim55902 = alloca %struct.ScmObj*, align 8
%argslist54479$k483852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49546, %struct.ScmObj* %argslist54479$k483851)
store volatile %struct.ScmObj* %argslist54479$k483852, %struct.ScmObj** %stackaddr$prim55902, align 8
%clofunc55903 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48385)
musttail call tailcc void %clofunc55903(%struct.ScmObj* %k48385, %struct.ScmObj* %argslist54479$k483852)
ret void
}

define tailcc void @proc_clo$ae49150(%struct.ScmObj* %env$ae49150,%struct.ScmObj* %current_45args54484) {
%stackaddr$env-ref55904 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49150, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55904
%stackaddr$env-ref55905 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49150, i64 1)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref55905
%stackaddr$prim55906 = alloca %struct.ScmObj*, align 8
%k48388 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54484)
store volatile %struct.ScmObj* %k48388, %struct.ScmObj** %stackaddr$prim55906, align 8
%stackaddr$prim55907 = alloca %struct.ScmObj*, align 8
%current_45args54485 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54484)
store volatile %struct.ScmObj* %current_45args54485, %struct.ScmObj** %stackaddr$prim55907, align 8
%stackaddr$prim55908 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54485)
store volatile %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$prim55908, align 8
%ae49152 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55909 = alloca %struct.ScmObj*, align 8
%fptrToInt55910 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49153 to i64
%ae49153 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55910)
store volatile %struct.ScmObj* %ae49153, %struct.ScmObj** %stackaddr$makeclosure55909, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49153, %struct.ScmObj* %_37foldr48052, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49153, %struct.ScmObj* %_37foldr148046, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49153, %struct.ScmObj* %_37map148042, i64 2)
%argslist54542$k483880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55911 = alloca %struct.ScmObj*, align 8
%argslist54542$k483881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49153, %struct.ScmObj* %argslist54542$k483880)
store volatile %struct.ScmObj* %argslist54542$k483881, %struct.ScmObj** %stackaddr$prim55911, align 8
%stackaddr$prim55912 = alloca %struct.ScmObj*, align 8
%argslist54542$k483882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49152, %struct.ScmObj* %argslist54542$k483881)
store volatile %struct.ScmObj* %argslist54542$k483882, %struct.ScmObj** %stackaddr$prim55912, align 8
%clofunc55913 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48388)
musttail call tailcc void %clofunc55913(%struct.ScmObj* %k48388, %struct.ScmObj* %argslist54542$k483882)
ret void
}

define tailcc void @proc_clo$ae49153(%struct.ScmObj* %env$ae49153,%struct.ScmObj* %args4805348389) {
%stackaddr$env-ref55914 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49153, i64 0)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref55914
%stackaddr$env-ref55915 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49153, i64 1)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55915
%stackaddr$env-ref55916 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49153, i64 2)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref55916
%stackaddr$prim55917 = alloca %struct.ScmObj*, align 8
%k48390 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4805348389)
store volatile %struct.ScmObj* %k48390, %struct.ScmObj** %stackaddr$prim55917, align 8
%stackaddr$prim55918 = alloca %struct.ScmObj*, align 8
%args48053 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4805348389)
store volatile %struct.ScmObj* %args48053, %struct.ScmObj** %stackaddr$prim55918, align 8
%stackaddr$prim55919 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48053)
store volatile %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$prim55919, align 8
%stackaddr$prim55920 = alloca %struct.ScmObj*, align 8
%anf_45bind48183 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48053)
store volatile %struct.ScmObj* %anf_45bind48183, %struct.ScmObj** %stackaddr$prim55920, align 8
%stackaddr$prim55921 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48183)
store volatile %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$prim55921, align 8
%stackaddr$prim55922 = alloca %struct.ScmObj*, align 8
%anf_45bind48184 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48053)
store volatile %struct.ScmObj* %anf_45bind48184, %struct.ScmObj** %stackaddr$prim55922, align 8
%stackaddr$prim55923 = alloca %struct.ScmObj*, align 8
%lsts48054 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48184)
store volatile %struct.ScmObj* %lsts48054, %struct.ScmObj** %stackaddr$prim55923, align 8
%stackaddr$makeclosure55924 = alloca %struct.ScmObj*, align 8
%fptrToInt55925 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49161 to i64
%ae49161 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55925)
store volatile %struct.ScmObj* %ae49161, %struct.ScmObj** %stackaddr$makeclosure55924, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49161, %struct.ScmObj* %k48390, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49161, %struct.ScmObj* %lsts48054, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49161, %struct.ScmObj* %acc48055, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49161, %struct.ScmObj* %_37foldr48052, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49161, %struct.ScmObj* %_37foldr148046, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49161, %struct.ScmObj* %_37map148042, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49161, %struct.ScmObj* %f48056, i64 6)
%ae49162 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55926 = alloca %struct.ScmObj*, align 8
%fptrToInt55927 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49163 to i64
%ae49163 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55927)
store volatile %struct.ScmObj* %ae49163, %struct.ScmObj** %stackaddr$makeclosure55926, align 8
%argslist54541$ae491610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55928 = alloca %struct.ScmObj*, align 8
%argslist54541$ae491611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49163, %struct.ScmObj* %argslist54541$ae491610)
store volatile %struct.ScmObj* %argslist54541$ae491611, %struct.ScmObj** %stackaddr$prim55928, align 8
%stackaddr$prim55929 = alloca %struct.ScmObj*, align 8
%argslist54541$ae491612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49162, %struct.ScmObj* %argslist54541$ae491611)
store volatile %struct.ScmObj* %argslist54541$ae491612, %struct.ScmObj** %stackaddr$prim55929, align 8
%clofunc55930 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49161)
musttail call tailcc void %clofunc55930(%struct.ScmObj* %ae49161, %struct.ScmObj* %argslist54541$ae491612)
ret void
}

define tailcc void @proc_clo$ae49161(%struct.ScmObj* %env$ae49161,%struct.ScmObj* %current_45args54487) {
%stackaddr$env-ref55931 = alloca %struct.ScmObj*, align 8
%k48390 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49161, i64 0)
store %struct.ScmObj* %k48390, %struct.ScmObj** %stackaddr$env-ref55931
%stackaddr$env-ref55932 = alloca %struct.ScmObj*, align 8
%lsts48054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49161, i64 1)
store %struct.ScmObj* %lsts48054, %struct.ScmObj** %stackaddr$env-ref55932
%stackaddr$env-ref55933 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49161, i64 2)
store %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$env-ref55933
%stackaddr$env-ref55934 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49161, i64 3)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref55934
%stackaddr$env-ref55935 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49161, i64 4)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55935
%stackaddr$env-ref55936 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49161, i64 5)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref55936
%stackaddr$env-ref55937 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49161, i64 6)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref55937
%stackaddr$prim55938 = alloca %struct.ScmObj*, align 8
%_95k48391 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54487)
store volatile %struct.ScmObj* %_95k48391, %struct.ScmObj** %stackaddr$prim55938, align 8
%stackaddr$prim55939 = alloca %struct.ScmObj*, align 8
%current_45args54488 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54487)
store volatile %struct.ScmObj* %current_45args54488, %struct.ScmObj** %stackaddr$prim55939, align 8
%stackaddr$prim55940 = alloca %struct.ScmObj*, align 8
%anf_45bind48185 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54488)
store volatile %struct.ScmObj* %anf_45bind48185, %struct.ScmObj** %stackaddr$prim55940, align 8
%stackaddr$makeclosure55941 = alloca %struct.ScmObj*, align 8
%fptrToInt55942 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49193 to i64
%ae49193 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55942)
store volatile %struct.ScmObj* %ae49193, %struct.ScmObj** %stackaddr$makeclosure55941, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49193, %struct.ScmObj* %k48390, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49193, %struct.ScmObj* %lsts48054, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49193, %struct.ScmObj* %acc48055, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49193, %struct.ScmObj* %_37foldr48052, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49193, %struct.ScmObj* %_37foldr148046, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49193, %struct.ScmObj* %_37map148042, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49193, %struct.ScmObj* %f48056, i64 6)
%ae49195 = call %struct.ScmObj* @const_init_false()
%argslist54534$_37foldr1480460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55943 = alloca %struct.ScmObj*, align 8
%argslist54534$_37foldr1480461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48054, %struct.ScmObj* %argslist54534$_37foldr1480460)
store volatile %struct.ScmObj* %argslist54534$_37foldr1480461, %struct.ScmObj** %stackaddr$prim55943, align 8
%stackaddr$prim55944 = alloca %struct.ScmObj*, align 8
%argslist54534$_37foldr1480462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49195, %struct.ScmObj* %argslist54534$_37foldr1480461)
store volatile %struct.ScmObj* %argslist54534$_37foldr1480462, %struct.ScmObj** %stackaddr$prim55944, align 8
%stackaddr$prim55945 = alloca %struct.ScmObj*, align 8
%argslist54534$_37foldr1480463 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48185, %struct.ScmObj* %argslist54534$_37foldr1480462)
store volatile %struct.ScmObj* %argslist54534$_37foldr1480463, %struct.ScmObj** %stackaddr$prim55945, align 8
%stackaddr$prim55946 = alloca %struct.ScmObj*, align 8
%argslist54534$_37foldr1480464 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49193, %struct.ScmObj* %argslist54534$_37foldr1480463)
store volatile %struct.ScmObj* %argslist54534$_37foldr1480464, %struct.ScmObj** %stackaddr$prim55946, align 8
%clofunc55947 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148046)
musttail call tailcc void %clofunc55947(%struct.ScmObj* %_37foldr148046, %struct.ScmObj* %argslist54534$_37foldr1480464)
ret void
}

define tailcc void @proc_clo$ae49193(%struct.ScmObj* %env$ae49193,%struct.ScmObj* %current_45args54490) {
%stackaddr$env-ref55948 = alloca %struct.ScmObj*, align 8
%k48390 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49193, i64 0)
store %struct.ScmObj* %k48390, %struct.ScmObj** %stackaddr$env-ref55948
%stackaddr$env-ref55949 = alloca %struct.ScmObj*, align 8
%lsts48054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49193, i64 1)
store %struct.ScmObj* %lsts48054, %struct.ScmObj** %stackaddr$env-ref55949
%stackaddr$env-ref55950 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49193, i64 2)
store %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$env-ref55950
%stackaddr$env-ref55951 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49193, i64 3)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref55951
%stackaddr$env-ref55952 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49193, i64 4)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55952
%stackaddr$env-ref55953 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49193, i64 5)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref55953
%stackaddr$env-ref55954 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49193, i64 6)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref55954
%stackaddr$prim55955 = alloca %struct.ScmObj*, align 8
%_95k48392 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54490)
store volatile %struct.ScmObj* %_95k48392, %struct.ScmObj** %stackaddr$prim55955, align 8
%stackaddr$prim55956 = alloca %struct.ScmObj*, align 8
%current_45args54491 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54490)
store volatile %struct.ScmObj* %current_45args54491, %struct.ScmObj** %stackaddr$prim55956, align 8
%stackaddr$prim55957 = alloca %struct.ScmObj*, align 8
%anf_45bind48186 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54491)
store volatile %struct.ScmObj* %anf_45bind48186, %struct.ScmObj** %stackaddr$prim55957, align 8
%truthy$cmp55958 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48186)
%cmp$cmp55958 = icmp eq i64 %truthy$cmp55958, 1
br i1 %cmp$cmp55958, label %truebranch$cmp55958, label %falsebranch$cmp55958
truebranch$cmp55958:
%ae49204 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54493$k483900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55959 = alloca %struct.ScmObj*, align 8
%argslist54493$k483901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48055, %struct.ScmObj* %argslist54493$k483900)
store volatile %struct.ScmObj* %argslist54493$k483901, %struct.ScmObj** %stackaddr$prim55959, align 8
%stackaddr$prim55960 = alloca %struct.ScmObj*, align 8
%argslist54493$k483902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49204, %struct.ScmObj* %argslist54493$k483901)
store volatile %struct.ScmObj* %argslist54493$k483902, %struct.ScmObj** %stackaddr$prim55960, align 8
%clofunc55961 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48390)
musttail call tailcc void %clofunc55961(%struct.ScmObj* %k48390, %struct.ScmObj* %argslist54493$k483902)
ret void
falsebranch$cmp55958:
%stackaddr$makeclosure55962 = alloca %struct.ScmObj*, align 8
%fptrToInt55963 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49209 to i64
%ae49209 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55963)
store volatile %struct.ScmObj* %ae49209, %struct.ScmObj** %stackaddr$makeclosure55962, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49209, %struct.ScmObj* %k48390, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49209, %struct.ScmObj* %lsts48054, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49209, %struct.ScmObj* %acc48055, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49209, %struct.ScmObj* %_37foldr48052, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49209, %struct.ScmObj* %_37foldr148046, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49209, %struct.ScmObj* %_37map148042, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49209, %struct.ScmObj* %f48056, i64 6)
%ae49210 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55964 = alloca %struct.ScmObj*, align 8
%fptrToInt55965 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49211 to i64
%ae49211 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55965)
store volatile %struct.ScmObj* %ae49211, %struct.ScmObj** %stackaddr$makeclosure55964, align 8
%argslist54533$ae492090 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55966 = alloca %struct.ScmObj*, align 8
%argslist54533$ae492091 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49211, %struct.ScmObj* %argslist54533$ae492090)
store volatile %struct.ScmObj* %argslist54533$ae492091, %struct.ScmObj** %stackaddr$prim55966, align 8
%stackaddr$prim55967 = alloca %struct.ScmObj*, align 8
%argslist54533$ae492092 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49210, %struct.ScmObj* %argslist54533$ae492091)
store volatile %struct.ScmObj* %argslist54533$ae492092, %struct.ScmObj** %stackaddr$prim55967, align 8
%clofunc55968 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49209)
musttail call tailcc void %clofunc55968(%struct.ScmObj* %ae49209, %struct.ScmObj* %argslist54533$ae492092)
ret void
}

define tailcc void @proc_clo$ae49209(%struct.ScmObj* %env$ae49209,%struct.ScmObj* %current_45args54494) {
%stackaddr$env-ref55969 = alloca %struct.ScmObj*, align 8
%k48390 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49209, i64 0)
store %struct.ScmObj* %k48390, %struct.ScmObj** %stackaddr$env-ref55969
%stackaddr$env-ref55970 = alloca %struct.ScmObj*, align 8
%lsts48054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49209, i64 1)
store %struct.ScmObj* %lsts48054, %struct.ScmObj** %stackaddr$env-ref55970
%stackaddr$env-ref55971 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49209, i64 2)
store %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$env-ref55971
%stackaddr$env-ref55972 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49209, i64 3)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref55972
%stackaddr$env-ref55973 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49209, i64 4)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55973
%stackaddr$env-ref55974 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49209, i64 5)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref55974
%stackaddr$env-ref55975 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49209, i64 6)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref55975
%stackaddr$prim55976 = alloca %struct.ScmObj*, align 8
%_95k48393 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54494)
store volatile %struct.ScmObj* %_95k48393, %struct.ScmObj** %stackaddr$prim55976, align 8
%stackaddr$prim55977 = alloca %struct.ScmObj*, align 8
%current_45args54495 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54494)
store volatile %struct.ScmObj* %current_45args54495, %struct.ScmObj** %stackaddr$prim55977, align 8
%stackaddr$prim55978 = alloca %struct.ScmObj*, align 8
%anf_45bind48187 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54495)
store volatile %struct.ScmObj* %anf_45bind48187, %struct.ScmObj** %stackaddr$prim55978, align 8
%stackaddr$makeclosure55979 = alloca %struct.ScmObj*, align 8
%fptrToInt55980 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49230 to i64
%ae49230 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55980)
store volatile %struct.ScmObj* %ae49230, %struct.ScmObj** %stackaddr$makeclosure55979, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49230, %struct.ScmObj* %k48390, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49230, %struct.ScmObj* %lsts48054, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49230, %struct.ScmObj* %acc48055, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49230, %struct.ScmObj* %_37foldr48052, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49230, %struct.ScmObj* %_37foldr148046, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49230, %struct.ScmObj* %_37map148042, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49230, %struct.ScmObj* %f48056, i64 6)
%argslist54528$_37map1480420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55981 = alloca %struct.ScmObj*, align 8
%argslist54528$_37map1480421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48054, %struct.ScmObj* %argslist54528$_37map1480420)
store volatile %struct.ScmObj* %argslist54528$_37map1480421, %struct.ScmObj** %stackaddr$prim55981, align 8
%stackaddr$prim55982 = alloca %struct.ScmObj*, align 8
%argslist54528$_37map1480422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48187, %struct.ScmObj* %argslist54528$_37map1480421)
store volatile %struct.ScmObj* %argslist54528$_37map1480422, %struct.ScmObj** %stackaddr$prim55982, align 8
%stackaddr$prim55983 = alloca %struct.ScmObj*, align 8
%argslist54528$_37map1480423 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49230, %struct.ScmObj* %argslist54528$_37map1480422)
store volatile %struct.ScmObj* %argslist54528$_37map1480423, %struct.ScmObj** %stackaddr$prim55983, align 8
%clofunc55984 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148042)
musttail call tailcc void %clofunc55984(%struct.ScmObj* %_37map148042, %struct.ScmObj* %argslist54528$_37map1480423)
ret void
}

define tailcc void @proc_clo$ae49230(%struct.ScmObj* %env$ae49230,%struct.ScmObj* %current_45args54497) {
%stackaddr$env-ref55985 = alloca %struct.ScmObj*, align 8
%k48390 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49230, i64 0)
store %struct.ScmObj* %k48390, %struct.ScmObj** %stackaddr$env-ref55985
%stackaddr$env-ref55986 = alloca %struct.ScmObj*, align 8
%lsts48054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49230, i64 1)
store %struct.ScmObj* %lsts48054, %struct.ScmObj** %stackaddr$env-ref55986
%stackaddr$env-ref55987 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49230, i64 2)
store %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$env-ref55987
%stackaddr$env-ref55988 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49230, i64 3)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref55988
%stackaddr$env-ref55989 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49230, i64 4)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55989
%stackaddr$env-ref55990 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49230, i64 5)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref55990
%stackaddr$env-ref55991 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49230, i64 6)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref55991
%stackaddr$prim55992 = alloca %struct.ScmObj*, align 8
%_95k48394 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54497)
store volatile %struct.ScmObj* %_95k48394, %struct.ScmObj** %stackaddr$prim55992, align 8
%stackaddr$prim55993 = alloca %struct.ScmObj*, align 8
%current_45args54498 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54497)
store volatile %struct.ScmObj* %current_45args54498, %struct.ScmObj** %stackaddr$prim55993, align 8
%stackaddr$prim55994 = alloca %struct.ScmObj*, align 8
%lsts_4348061 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54498)
store volatile %struct.ScmObj* %lsts_4348061, %struct.ScmObj** %stackaddr$prim55994, align 8
%stackaddr$makeclosure55995 = alloca %struct.ScmObj*, align 8
%fptrToInt55996 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49233 to i64
%ae49233 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt55996)
store volatile %struct.ScmObj* %ae49233, %struct.ScmObj** %stackaddr$makeclosure55995, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49233, %struct.ScmObj* %k48390, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49233, %struct.ScmObj* %lsts48054, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49233, %struct.ScmObj* %acc48055, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49233, %struct.ScmObj* %_37foldr48052, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49233, %struct.ScmObj* %_37foldr148046, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49233, %struct.ScmObj* %lsts_4348061, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49233, %struct.ScmObj* %_37map148042, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae49233, %struct.ScmObj* %f48056, i64 7)
%ae49234 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55997 = alloca %struct.ScmObj*, align 8
%fptrToInt55998 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49235 to i64
%ae49235 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55998)
store volatile %struct.ScmObj* %ae49235, %struct.ScmObj** %stackaddr$makeclosure55997, align 8
%argslist54527$ae492330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55999 = alloca %struct.ScmObj*, align 8
%argslist54527$ae492331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49235, %struct.ScmObj* %argslist54527$ae492330)
store volatile %struct.ScmObj* %argslist54527$ae492331, %struct.ScmObj** %stackaddr$prim55999, align 8
%stackaddr$prim56000 = alloca %struct.ScmObj*, align 8
%argslist54527$ae492332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49234, %struct.ScmObj* %argslist54527$ae492331)
store volatile %struct.ScmObj* %argslist54527$ae492332, %struct.ScmObj** %stackaddr$prim56000, align 8
%clofunc56001 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49233)
musttail call tailcc void %clofunc56001(%struct.ScmObj* %ae49233, %struct.ScmObj* %argslist54527$ae492332)
ret void
}

define tailcc void @proc_clo$ae49233(%struct.ScmObj* %env$ae49233,%struct.ScmObj* %current_45args54500) {
%stackaddr$env-ref56002 = alloca %struct.ScmObj*, align 8
%k48390 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49233, i64 0)
store %struct.ScmObj* %k48390, %struct.ScmObj** %stackaddr$env-ref56002
%stackaddr$env-ref56003 = alloca %struct.ScmObj*, align 8
%lsts48054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49233, i64 1)
store %struct.ScmObj* %lsts48054, %struct.ScmObj** %stackaddr$env-ref56003
%stackaddr$env-ref56004 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49233, i64 2)
store %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$env-ref56004
%stackaddr$env-ref56005 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49233, i64 3)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref56005
%stackaddr$env-ref56006 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49233, i64 4)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56006
%stackaddr$env-ref56007 = alloca %struct.ScmObj*, align 8
%lsts_4348061 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49233, i64 5)
store %struct.ScmObj* %lsts_4348061, %struct.ScmObj** %stackaddr$env-ref56007
%stackaddr$env-ref56008 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49233, i64 6)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref56008
%stackaddr$env-ref56009 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49233, i64 7)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref56009
%stackaddr$prim56010 = alloca %struct.ScmObj*, align 8
%_95k48395 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54500)
store volatile %struct.ScmObj* %_95k48395, %struct.ScmObj** %stackaddr$prim56010, align 8
%stackaddr$prim56011 = alloca %struct.ScmObj*, align 8
%current_45args54501 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54500)
store volatile %struct.ScmObj* %current_45args54501, %struct.ScmObj** %stackaddr$prim56011, align 8
%stackaddr$prim56012 = alloca %struct.ScmObj*, align 8
%anf_45bind48188 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54501)
store volatile %struct.ScmObj* %anf_45bind48188, %struct.ScmObj** %stackaddr$prim56012, align 8
%stackaddr$makeclosure56013 = alloca %struct.ScmObj*, align 8
%fptrToInt56014 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49254 to i64
%ae49254 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt56014)
store volatile %struct.ScmObj* %ae49254, %struct.ScmObj** %stackaddr$makeclosure56013, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49254, %struct.ScmObj* %acc48055, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49254, %struct.ScmObj* %k48390, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49254, %struct.ScmObj* %_37foldr48052, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49254, %struct.ScmObj* %_37foldr148046, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49254, %struct.ScmObj* %lsts_4348061, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49254, %struct.ScmObj* %f48056, i64 5)
%argslist54522$_37map1480420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56015 = alloca %struct.ScmObj*, align 8
%argslist54522$_37map1480421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48054, %struct.ScmObj* %argslist54522$_37map1480420)
store volatile %struct.ScmObj* %argslist54522$_37map1480421, %struct.ScmObj** %stackaddr$prim56015, align 8
%stackaddr$prim56016 = alloca %struct.ScmObj*, align 8
%argslist54522$_37map1480422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48188, %struct.ScmObj* %argslist54522$_37map1480421)
store volatile %struct.ScmObj* %argslist54522$_37map1480422, %struct.ScmObj** %stackaddr$prim56016, align 8
%stackaddr$prim56017 = alloca %struct.ScmObj*, align 8
%argslist54522$_37map1480423 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49254, %struct.ScmObj* %argslist54522$_37map1480422)
store volatile %struct.ScmObj* %argslist54522$_37map1480423, %struct.ScmObj** %stackaddr$prim56017, align 8
%clofunc56018 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148042)
musttail call tailcc void %clofunc56018(%struct.ScmObj* %_37map148042, %struct.ScmObj* %argslist54522$_37map1480423)
ret void
}

define tailcc void @proc_clo$ae49254(%struct.ScmObj* %env$ae49254,%struct.ScmObj* %current_45args54503) {
%stackaddr$env-ref56019 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49254, i64 0)
store %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$env-ref56019
%stackaddr$env-ref56020 = alloca %struct.ScmObj*, align 8
%k48390 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49254, i64 1)
store %struct.ScmObj* %k48390, %struct.ScmObj** %stackaddr$env-ref56020
%stackaddr$env-ref56021 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49254, i64 2)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref56021
%stackaddr$env-ref56022 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49254, i64 3)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56022
%stackaddr$env-ref56023 = alloca %struct.ScmObj*, align 8
%lsts_4348061 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49254, i64 4)
store %struct.ScmObj* %lsts_4348061, %struct.ScmObj** %stackaddr$env-ref56023
%stackaddr$env-ref56024 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49254, i64 5)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref56024
%stackaddr$prim56025 = alloca %struct.ScmObj*, align 8
%_95k48396 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54503)
store volatile %struct.ScmObj* %_95k48396, %struct.ScmObj** %stackaddr$prim56025, align 8
%stackaddr$prim56026 = alloca %struct.ScmObj*, align 8
%current_45args54504 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54503)
store volatile %struct.ScmObj* %current_45args54504, %struct.ScmObj** %stackaddr$prim56026, align 8
%stackaddr$prim56027 = alloca %struct.ScmObj*, align 8
%vs48059 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54504)
store volatile %struct.ScmObj* %vs48059, %struct.ScmObj** %stackaddr$prim56027, align 8
%stackaddr$makeclosure56028 = alloca %struct.ScmObj*, align 8
%fptrToInt56029 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49257 to i64
%ae49257 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56029)
store volatile %struct.ScmObj* %ae49257, %struct.ScmObj** %stackaddr$makeclosure56028, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49257, %struct.ScmObj* %acc48055, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49257, %struct.ScmObj* %k48390, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49257, %struct.ScmObj* %_37foldr48052, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49257, %struct.ScmObj* %_37foldr148046, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49257, %struct.ScmObj* %lsts_4348061, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49257, %struct.ScmObj* %vs48059, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49257, %struct.ScmObj* %f48056, i64 6)
%ae49258 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56030 = alloca %struct.ScmObj*, align 8
%fptrToInt56031 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49259 to i64
%ae49259 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56031)
store volatile %struct.ScmObj* %ae49259, %struct.ScmObj** %stackaddr$makeclosure56030, align 8
%argslist54521$ae492570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56032 = alloca %struct.ScmObj*, align 8
%argslist54521$ae492571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49259, %struct.ScmObj* %argslist54521$ae492570)
store volatile %struct.ScmObj* %argslist54521$ae492571, %struct.ScmObj** %stackaddr$prim56032, align 8
%stackaddr$prim56033 = alloca %struct.ScmObj*, align 8
%argslist54521$ae492572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49258, %struct.ScmObj* %argslist54521$ae492571)
store volatile %struct.ScmObj* %argslist54521$ae492572, %struct.ScmObj** %stackaddr$prim56033, align 8
%clofunc56034 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49257)
musttail call tailcc void %clofunc56034(%struct.ScmObj* %ae49257, %struct.ScmObj* %argslist54521$ae492572)
ret void
}

define tailcc void @proc_clo$ae49257(%struct.ScmObj* %env$ae49257,%struct.ScmObj* %current_45args54506) {
%stackaddr$env-ref56035 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49257, i64 0)
store %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$env-ref56035
%stackaddr$env-ref56036 = alloca %struct.ScmObj*, align 8
%k48390 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49257, i64 1)
store %struct.ScmObj* %k48390, %struct.ScmObj** %stackaddr$env-ref56036
%stackaddr$env-ref56037 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49257, i64 2)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref56037
%stackaddr$env-ref56038 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49257, i64 3)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56038
%stackaddr$env-ref56039 = alloca %struct.ScmObj*, align 8
%lsts_4348061 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49257, i64 4)
store %struct.ScmObj* %lsts_4348061, %struct.ScmObj** %stackaddr$env-ref56039
%stackaddr$env-ref56040 = alloca %struct.ScmObj*, align 8
%vs48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49257, i64 5)
store %struct.ScmObj* %vs48059, %struct.ScmObj** %stackaddr$env-ref56040
%stackaddr$env-ref56041 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49257, i64 6)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref56041
%stackaddr$prim56042 = alloca %struct.ScmObj*, align 8
%_95k48397 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54506)
store volatile %struct.ScmObj* %_95k48397, %struct.ScmObj** %stackaddr$prim56042, align 8
%stackaddr$prim56043 = alloca %struct.ScmObj*, align 8
%current_45args54507 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54506)
store volatile %struct.ScmObj* %current_45args54507, %struct.ScmObj** %stackaddr$prim56043, align 8
%stackaddr$prim56044 = alloca %struct.ScmObj*, align 8
%anf_45bind48189 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54507)
store volatile %struct.ScmObj* %anf_45bind48189, %struct.ScmObj** %stackaddr$prim56044, align 8
%stackaddr$prim56045 = alloca %struct.ScmObj*, align 8
%anf_45bind48190 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48055, %struct.ScmObj* %lsts_4348061)
store volatile %struct.ScmObj* %anf_45bind48190, %struct.ScmObj** %stackaddr$prim56045, align 8
%stackaddr$prim56046 = alloca %struct.ScmObj*, align 8
%anf_45bind48191 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48056, %struct.ScmObj* %anf_45bind48190)
store volatile %struct.ScmObj* %anf_45bind48191, %struct.ScmObj** %stackaddr$prim56046, align 8
%stackaddr$makeclosure56047 = alloca %struct.ScmObj*, align 8
%fptrToInt56048 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49283 to i64
%ae49283 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt56048)
store volatile %struct.ScmObj* %ae49283, %struct.ScmObj** %stackaddr$makeclosure56047, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49283, %struct.ScmObj* %k48390, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49283, %struct.ScmObj* %_37foldr148046, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49283, %struct.ScmObj* %anf_45bind48189, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49283, %struct.ScmObj* %vs48059, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49283, %struct.ScmObj* %f48056, i64 4)
%stackaddr$prim56049 = alloca %struct.ScmObj*, align 8
%cpsargs48401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49283, %struct.ScmObj* %anf_45bind48191)
store volatile %struct.ScmObj* %cpsargs48401, %struct.ScmObj** %stackaddr$prim56049, align 8
%clofunc56050 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr48052)
musttail call tailcc void %clofunc56050(%struct.ScmObj* %_37foldr48052, %struct.ScmObj* %cpsargs48401)
ret void
}

define tailcc void @proc_clo$ae49283(%struct.ScmObj* %env$ae49283,%struct.ScmObj* %current_45args54509) {
%stackaddr$env-ref56051 = alloca %struct.ScmObj*, align 8
%k48390 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49283, i64 0)
store %struct.ScmObj* %k48390, %struct.ScmObj** %stackaddr$env-ref56051
%stackaddr$env-ref56052 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49283, i64 1)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56052
%stackaddr$env-ref56053 = alloca %struct.ScmObj*, align 8
%anf_45bind48189 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49283, i64 2)
store %struct.ScmObj* %anf_45bind48189, %struct.ScmObj** %stackaddr$env-ref56053
%stackaddr$env-ref56054 = alloca %struct.ScmObj*, align 8
%vs48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49283, i64 3)
store %struct.ScmObj* %vs48059, %struct.ScmObj** %stackaddr$env-ref56054
%stackaddr$env-ref56055 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49283, i64 4)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref56055
%stackaddr$prim56056 = alloca %struct.ScmObj*, align 8
%_95k48398 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54509)
store volatile %struct.ScmObj* %_95k48398, %struct.ScmObj** %stackaddr$prim56056, align 8
%stackaddr$prim56057 = alloca %struct.ScmObj*, align 8
%current_45args54510 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54509)
store volatile %struct.ScmObj* %current_45args54510, %struct.ScmObj** %stackaddr$prim56057, align 8
%stackaddr$prim56058 = alloca %struct.ScmObj*, align 8
%anf_45bind48192 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54510)
store volatile %struct.ScmObj* %anf_45bind48192, %struct.ScmObj** %stackaddr$prim56058, align 8
%ae49288 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56059 = alloca %struct.ScmObj*, align 8
%anf_45bind48193 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48192, %struct.ScmObj* %ae49288)
store volatile %struct.ScmObj* %anf_45bind48193, %struct.ScmObj** %stackaddr$prim56059, align 8
%stackaddr$makeclosure56060 = alloca %struct.ScmObj*, align 8
%fptrToInt56061 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49290 to i64
%ae49290 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56061)
store volatile %struct.ScmObj* %ae49290, %struct.ScmObj** %stackaddr$makeclosure56060, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49290, %struct.ScmObj* %k48390, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49290, %struct.ScmObj* %f48056, i64 1)
%argslist54515$_37foldr1480460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56062 = alloca %struct.ScmObj*, align 8
%argslist54515$_37foldr1480461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs48059, %struct.ScmObj* %argslist54515$_37foldr1480460)
store volatile %struct.ScmObj* %argslist54515$_37foldr1480461, %struct.ScmObj** %stackaddr$prim56062, align 8
%stackaddr$prim56063 = alloca %struct.ScmObj*, align 8
%argslist54515$_37foldr1480462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48193, %struct.ScmObj* %argslist54515$_37foldr1480461)
store volatile %struct.ScmObj* %argslist54515$_37foldr1480462, %struct.ScmObj** %stackaddr$prim56063, align 8
%stackaddr$prim56064 = alloca %struct.ScmObj*, align 8
%argslist54515$_37foldr1480463 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48189, %struct.ScmObj* %argslist54515$_37foldr1480462)
store volatile %struct.ScmObj* %argslist54515$_37foldr1480463, %struct.ScmObj** %stackaddr$prim56064, align 8
%stackaddr$prim56065 = alloca %struct.ScmObj*, align 8
%argslist54515$_37foldr1480464 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49290, %struct.ScmObj* %argslist54515$_37foldr1480463)
store volatile %struct.ScmObj* %argslist54515$_37foldr1480464, %struct.ScmObj** %stackaddr$prim56065, align 8
%clofunc56066 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148046)
musttail call tailcc void %clofunc56066(%struct.ScmObj* %_37foldr148046, %struct.ScmObj* %argslist54515$_37foldr1480464)
ret void
}

define tailcc void @proc_clo$ae49290(%struct.ScmObj* %env$ae49290,%struct.ScmObj* %current_45args54512) {
%stackaddr$env-ref56067 = alloca %struct.ScmObj*, align 8
%k48390 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49290, i64 0)
store %struct.ScmObj* %k48390, %struct.ScmObj** %stackaddr$env-ref56067
%stackaddr$env-ref56068 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49290, i64 1)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref56068
%stackaddr$prim56069 = alloca %struct.ScmObj*, align 8
%_95k48399 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54512)
store volatile %struct.ScmObj* %_95k48399, %struct.ScmObj** %stackaddr$prim56069, align 8
%stackaddr$prim56070 = alloca %struct.ScmObj*, align 8
%current_45args54513 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54512)
store volatile %struct.ScmObj* %current_45args54513, %struct.ScmObj** %stackaddr$prim56070, align 8
%stackaddr$prim56071 = alloca %struct.ScmObj*, align 8
%anf_45bind48194 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54513)
store volatile %struct.ScmObj* %anf_45bind48194, %struct.ScmObj** %stackaddr$prim56071, align 8
%stackaddr$prim56072 = alloca %struct.ScmObj*, align 8
%cpsargs48400 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48390, %struct.ScmObj* %anf_45bind48194)
store volatile %struct.ScmObj* %cpsargs48400, %struct.ScmObj** %stackaddr$prim56072, align 8
%clofunc56073 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48056)
musttail call tailcc void %clofunc56073(%struct.ScmObj* %f48056, %struct.ScmObj* %cpsargs48400)
ret void
}

define tailcc void @proc_clo$ae49259(%struct.ScmObj* %env$ae49259,%struct.ScmObj* %current_45args54516) {
%stackaddr$prim56074 = alloca %struct.ScmObj*, align 8
%k48402 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54516)
store volatile %struct.ScmObj* %k48402, %struct.ScmObj** %stackaddr$prim56074, align 8
%stackaddr$prim56075 = alloca %struct.ScmObj*, align 8
%current_45args54517 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54516)
store volatile %struct.ScmObj* %current_45args54517, %struct.ScmObj** %stackaddr$prim56075, align 8
%stackaddr$prim56076 = alloca %struct.ScmObj*, align 8
%a48064 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54517)
store volatile %struct.ScmObj* %a48064, %struct.ScmObj** %stackaddr$prim56076, align 8
%stackaddr$prim56077 = alloca %struct.ScmObj*, align 8
%current_45args54518 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54517)
store volatile %struct.ScmObj* %current_45args54518, %struct.ScmObj** %stackaddr$prim56077, align 8
%stackaddr$prim56078 = alloca %struct.ScmObj*, align 8
%b48063 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54518)
store volatile %struct.ScmObj* %b48063, %struct.ScmObj** %stackaddr$prim56078, align 8
%stackaddr$prim56079 = alloca %struct.ScmObj*, align 8
%cpsprim48403 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a48064, %struct.ScmObj* %b48063)
store volatile %struct.ScmObj* %cpsprim48403, %struct.ScmObj** %stackaddr$prim56079, align 8
%ae49263 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54520$k484020 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56080 = alloca %struct.ScmObj*, align 8
%argslist54520$k484021 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48403, %struct.ScmObj* %argslist54520$k484020)
store volatile %struct.ScmObj* %argslist54520$k484021, %struct.ScmObj** %stackaddr$prim56080, align 8
%stackaddr$prim56081 = alloca %struct.ScmObj*, align 8
%argslist54520$k484022 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49263, %struct.ScmObj* %argslist54520$k484021)
store volatile %struct.ScmObj* %argslist54520$k484022, %struct.ScmObj** %stackaddr$prim56081, align 8
%clofunc56082 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48402)
musttail call tailcc void %clofunc56082(%struct.ScmObj* %k48402, %struct.ScmObj* %argslist54520$k484022)
ret void
}

define tailcc void @proc_clo$ae49235(%struct.ScmObj* %env$ae49235,%struct.ScmObj* %current_45args54523) {
%stackaddr$prim56083 = alloca %struct.ScmObj*, align 8
%k48404 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54523)
store volatile %struct.ScmObj* %k48404, %struct.ScmObj** %stackaddr$prim56083, align 8
%stackaddr$prim56084 = alloca %struct.ScmObj*, align 8
%current_45args54524 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54523)
store volatile %struct.ScmObj* %current_45args54524, %struct.ScmObj** %stackaddr$prim56084, align 8
%stackaddr$prim56085 = alloca %struct.ScmObj*, align 8
%x48060 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54524)
store volatile %struct.ScmObj* %x48060, %struct.ScmObj** %stackaddr$prim56085, align 8
%stackaddr$prim56086 = alloca %struct.ScmObj*, align 8
%cpsprim48405 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48060)
store volatile %struct.ScmObj* %cpsprim48405, %struct.ScmObj** %stackaddr$prim56086, align 8
%ae49238 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54526$k484040 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56087 = alloca %struct.ScmObj*, align 8
%argslist54526$k484041 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48405, %struct.ScmObj* %argslist54526$k484040)
store volatile %struct.ScmObj* %argslist54526$k484041, %struct.ScmObj** %stackaddr$prim56087, align 8
%stackaddr$prim56088 = alloca %struct.ScmObj*, align 8
%argslist54526$k484042 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49238, %struct.ScmObj* %argslist54526$k484041)
store volatile %struct.ScmObj* %argslist54526$k484042, %struct.ScmObj** %stackaddr$prim56088, align 8
%clofunc56089 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48404)
musttail call tailcc void %clofunc56089(%struct.ScmObj* %k48404, %struct.ScmObj* %argslist54526$k484042)
ret void
}

define tailcc void @proc_clo$ae49211(%struct.ScmObj* %env$ae49211,%struct.ScmObj* %current_45args54529) {
%stackaddr$prim56090 = alloca %struct.ScmObj*, align 8
%k48406 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54529)
store volatile %struct.ScmObj* %k48406, %struct.ScmObj** %stackaddr$prim56090, align 8
%stackaddr$prim56091 = alloca %struct.ScmObj*, align 8
%current_45args54530 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54529)
store volatile %struct.ScmObj* %current_45args54530, %struct.ScmObj** %stackaddr$prim56091, align 8
%stackaddr$prim56092 = alloca %struct.ScmObj*, align 8
%x48062 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54530)
store volatile %struct.ScmObj* %x48062, %struct.ScmObj** %stackaddr$prim56092, align 8
%stackaddr$prim56093 = alloca %struct.ScmObj*, align 8
%cpsprim48407 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48062)
store volatile %struct.ScmObj* %cpsprim48407, %struct.ScmObj** %stackaddr$prim56093, align 8
%ae49214 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54532$k484060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56094 = alloca %struct.ScmObj*, align 8
%argslist54532$k484061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48407, %struct.ScmObj* %argslist54532$k484060)
store volatile %struct.ScmObj* %argslist54532$k484061, %struct.ScmObj** %stackaddr$prim56094, align 8
%stackaddr$prim56095 = alloca %struct.ScmObj*, align 8
%argslist54532$k484062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49214, %struct.ScmObj* %argslist54532$k484061)
store volatile %struct.ScmObj* %argslist54532$k484062, %struct.ScmObj** %stackaddr$prim56095, align 8
%clofunc56096 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48406)
musttail call tailcc void %clofunc56096(%struct.ScmObj* %k48406, %struct.ScmObj* %argslist54532$k484062)
ret void
}

define tailcc void @proc_clo$ae49163(%struct.ScmObj* %env$ae49163,%struct.ScmObj* %current_45args54535) {
%stackaddr$prim56097 = alloca %struct.ScmObj*, align 8
%k48408 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54535)
store volatile %struct.ScmObj* %k48408, %struct.ScmObj** %stackaddr$prim56097, align 8
%stackaddr$prim56098 = alloca %struct.ScmObj*, align 8
%current_45args54536 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54535)
store volatile %struct.ScmObj* %current_45args54536, %struct.ScmObj** %stackaddr$prim56098, align 8
%stackaddr$prim56099 = alloca %struct.ScmObj*, align 8
%lst48058 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54536)
store volatile %struct.ScmObj* %lst48058, %struct.ScmObj** %stackaddr$prim56099, align 8
%stackaddr$prim56100 = alloca %struct.ScmObj*, align 8
%current_45args54537 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54536)
store volatile %struct.ScmObj* %current_45args54537, %struct.ScmObj** %stackaddr$prim56100, align 8
%stackaddr$prim56101 = alloca %struct.ScmObj*, align 8
%b48057 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54537)
store volatile %struct.ScmObj* %b48057, %struct.ScmObj** %stackaddr$prim56101, align 8
%truthy$cmp56102 = call i64 @is_truthy_value(%struct.ScmObj* %b48057)
%cmp$cmp56102 = icmp eq i64 %truthy$cmp56102, 1
br i1 %cmp$cmp56102, label %truebranch$cmp56102, label %falsebranch$cmp56102
truebranch$cmp56102:
%ae49166 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54539$k484080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56103 = alloca %struct.ScmObj*, align 8
%argslist54539$k484081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b48057, %struct.ScmObj* %argslist54539$k484080)
store volatile %struct.ScmObj* %argslist54539$k484081, %struct.ScmObj** %stackaddr$prim56103, align 8
%stackaddr$prim56104 = alloca %struct.ScmObj*, align 8
%argslist54539$k484082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49166, %struct.ScmObj* %argslist54539$k484081)
store volatile %struct.ScmObj* %argslist54539$k484082, %struct.ScmObj** %stackaddr$prim56104, align 8
%clofunc56105 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48408)
musttail call tailcc void %clofunc56105(%struct.ScmObj* %k48408, %struct.ScmObj* %argslist54539$k484082)
ret void
falsebranch$cmp56102:
%stackaddr$prim56106 = alloca %struct.ScmObj*, align 8
%cpsprim48409 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48058)
store volatile %struct.ScmObj* %cpsprim48409, %struct.ScmObj** %stackaddr$prim56106, align 8
%ae49173 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54540$k484080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56107 = alloca %struct.ScmObj*, align 8
%argslist54540$k484081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48409, %struct.ScmObj* %argslist54540$k484080)
store volatile %struct.ScmObj* %argslist54540$k484081, %struct.ScmObj** %stackaddr$prim56107, align 8
%stackaddr$prim56108 = alloca %struct.ScmObj*, align 8
%argslist54540$k484082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49173, %struct.ScmObj* %argslist54540$k484081)
store volatile %struct.ScmObj* %argslist54540$k484082, %struct.ScmObj** %stackaddr$prim56108, align 8
%clofunc56109 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48408)
musttail call tailcc void %clofunc56109(%struct.ScmObj* %k48408, %struct.ScmObj* %argslist54540$k484082)
ret void
}

define tailcc void @proc_clo$ae49120(%struct.ScmObj* %env$ae49120,%struct.ScmObj* %current_45args54544) {
%stackaddr$env-ref56110 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49120, i64 0)
store %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$env-ref56110
%stackaddr$env-ref56111 = alloca %struct.ScmObj*, align 8
%_37length48035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49120, i64 1)
store %struct.ScmObj* %_37length48035, %struct.ScmObj** %stackaddr$env-ref56111
%stackaddr$prim56112 = alloca %struct.ScmObj*, align 8
%k48410 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54544)
store volatile %struct.ScmObj* %k48410, %struct.ScmObj** %stackaddr$prim56112, align 8
%stackaddr$prim56113 = alloca %struct.ScmObj*, align 8
%current_45args54545 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54544)
store volatile %struct.ScmObj* %current_45args54545, %struct.ScmObj** %stackaddr$prim56113, align 8
%stackaddr$prim56114 = alloca %struct.ScmObj*, align 8
%lst48067 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54545)
store volatile %struct.ScmObj* %lst48067, %struct.ScmObj** %stackaddr$prim56114, align 8
%stackaddr$prim56115 = alloca %struct.ScmObj*, align 8
%current_45args54546 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54545)
store volatile %struct.ScmObj* %current_45args54546, %struct.ScmObj** %stackaddr$prim56115, align 8
%stackaddr$prim56116 = alloca %struct.ScmObj*, align 8
%n48066 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54546)
store volatile %struct.ScmObj* %n48066, %struct.ScmObj** %stackaddr$prim56116, align 8
%stackaddr$makeclosure56117 = alloca %struct.ScmObj*, align 8
%fptrToInt56118 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49122 to i64
%ae49122 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56118)
store volatile %struct.ScmObj* %ae49122, %struct.ScmObj** %stackaddr$makeclosure56117, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49122, %struct.ScmObj* %_37take48038, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49122, %struct.ScmObj* %lst48067, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49122, %struct.ScmObj* %n48066, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49122, %struct.ScmObj* %k48410, i64 3)
%argslist54552$_37length480350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56119 = alloca %struct.ScmObj*, align 8
%argslist54552$_37length480351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48067, %struct.ScmObj* %argslist54552$_37length480350)
store volatile %struct.ScmObj* %argslist54552$_37length480351, %struct.ScmObj** %stackaddr$prim56119, align 8
%stackaddr$prim56120 = alloca %struct.ScmObj*, align 8
%argslist54552$_37length480352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49122, %struct.ScmObj* %argslist54552$_37length480351)
store volatile %struct.ScmObj* %argslist54552$_37length480352, %struct.ScmObj** %stackaddr$prim56120, align 8
%clofunc56121 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length48035)
musttail call tailcc void %clofunc56121(%struct.ScmObj* %_37length48035, %struct.ScmObj* %argslist54552$_37length480352)
ret void
}

define tailcc void @proc_clo$ae49122(%struct.ScmObj* %env$ae49122,%struct.ScmObj* %current_45args54548) {
%stackaddr$env-ref56122 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49122, i64 0)
store %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$env-ref56122
%stackaddr$env-ref56123 = alloca %struct.ScmObj*, align 8
%lst48067 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49122, i64 1)
store %struct.ScmObj* %lst48067, %struct.ScmObj** %stackaddr$env-ref56123
%stackaddr$env-ref56124 = alloca %struct.ScmObj*, align 8
%n48066 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49122, i64 2)
store %struct.ScmObj* %n48066, %struct.ScmObj** %stackaddr$env-ref56124
%stackaddr$env-ref56125 = alloca %struct.ScmObj*, align 8
%k48410 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49122, i64 3)
store %struct.ScmObj* %k48410, %struct.ScmObj** %stackaddr$env-ref56125
%stackaddr$prim56126 = alloca %struct.ScmObj*, align 8
%_95k48411 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54548)
store volatile %struct.ScmObj* %_95k48411, %struct.ScmObj** %stackaddr$prim56126, align 8
%stackaddr$prim56127 = alloca %struct.ScmObj*, align 8
%current_45args54549 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54548)
store volatile %struct.ScmObj* %current_45args54549, %struct.ScmObj** %stackaddr$prim56127, align 8
%stackaddr$prim56128 = alloca %struct.ScmObj*, align 8
%anf_45bind48181 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54549)
store volatile %struct.ScmObj* %anf_45bind48181, %struct.ScmObj** %stackaddr$prim56128, align 8
%stackaddr$prim56129 = alloca %struct.ScmObj*, align 8
%anf_45bind48182 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind48181, %struct.ScmObj* %n48066)
store volatile %struct.ScmObj* %anf_45bind48182, %struct.ScmObj** %stackaddr$prim56129, align 8
%argslist54551$_37take480380 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56130 = alloca %struct.ScmObj*, align 8
%argslist54551$_37take480381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48182, %struct.ScmObj* %argslist54551$_37take480380)
store volatile %struct.ScmObj* %argslist54551$_37take480381, %struct.ScmObj** %stackaddr$prim56130, align 8
%stackaddr$prim56131 = alloca %struct.ScmObj*, align 8
%argslist54551$_37take480382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48067, %struct.ScmObj* %argslist54551$_37take480381)
store volatile %struct.ScmObj* %argslist54551$_37take480382, %struct.ScmObj** %stackaddr$prim56131, align 8
%stackaddr$prim56132 = alloca %struct.ScmObj*, align 8
%argslist54551$_37take480383 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48410, %struct.ScmObj* %argslist54551$_37take480382)
store volatile %struct.ScmObj* %argslist54551$_37take480383, %struct.ScmObj** %stackaddr$prim56132, align 8
%clofunc56133 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take48038)
musttail call tailcc void %clofunc56133(%struct.ScmObj* %_37take48038, %struct.ScmObj* %argslist54551$_37take480383)
ret void
}

define tailcc void @proc_clo$ae49066(%struct.ScmObj* %env$ae49066,%struct.ScmObj* %current_45args54554) {
%stackaddr$env-ref56134 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49066, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref56134
%stackaddr$prim56135 = alloca %struct.ScmObj*, align 8
%k48412 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54554)
store volatile %struct.ScmObj* %k48412, %struct.ScmObj** %stackaddr$prim56135, align 8
%stackaddr$prim56136 = alloca %struct.ScmObj*, align 8
%current_45args54555 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54554)
store volatile %struct.ScmObj* %current_45args54555, %struct.ScmObj** %stackaddr$prim56136, align 8
%stackaddr$prim56137 = alloca %struct.ScmObj*, align 8
%lst48069 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54555)
store volatile %struct.ScmObj* %lst48069, %struct.ScmObj** %stackaddr$prim56137, align 8
%stackaddr$makeclosure56138 = alloca %struct.ScmObj*, align 8
%fptrToInt56139 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49067 to i64
%ae49067 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56139)
store volatile %struct.ScmObj* %ae49067, %struct.ScmObj** %stackaddr$makeclosure56138, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49067, %struct.ScmObj* %lst48069, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49067, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49067, %struct.ScmObj* %k48412, i64 2)
%ae49068 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56140 = alloca %struct.ScmObj*, align 8
%fptrToInt56141 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49069 to i64
%ae49069 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56141)
store volatile %struct.ScmObj* %ae49069, %struct.ScmObj** %stackaddr$makeclosure56140, align 8
%argslist54566$ae490670 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56142 = alloca %struct.ScmObj*, align 8
%argslist54566$ae490671 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49069, %struct.ScmObj* %argslist54566$ae490670)
store volatile %struct.ScmObj* %argslist54566$ae490671, %struct.ScmObj** %stackaddr$prim56142, align 8
%stackaddr$prim56143 = alloca %struct.ScmObj*, align 8
%argslist54566$ae490672 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49068, %struct.ScmObj* %argslist54566$ae490671)
store volatile %struct.ScmObj* %argslist54566$ae490672, %struct.ScmObj** %stackaddr$prim56143, align 8
%clofunc56144 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49067)
musttail call tailcc void %clofunc56144(%struct.ScmObj* %ae49067, %struct.ScmObj* %argslist54566$ae490672)
ret void
}

define tailcc void @proc_clo$ae49067(%struct.ScmObj* %env$ae49067,%struct.ScmObj* %current_45args54557) {
%stackaddr$env-ref56145 = alloca %struct.ScmObj*, align 8
%lst48069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49067, i64 0)
store %struct.ScmObj* %lst48069, %struct.ScmObj** %stackaddr$env-ref56145
%stackaddr$env-ref56146 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49067, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref56146
%stackaddr$env-ref56147 = alloca %struct.ScmObj*, align 8
%k48412 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49067, i64 2)
store %struct.ScmObj* %k48412, %struct.ScmObj** %stackaddr$env-ref56147
%stackaddr$prim56148 = alloca %struct.ScmObj*, align 8
%_95k48413 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54557)
store volatile %struct.ScmObj* %_95k48413, %struct.ScmObj** %stackaddr$prim56148, align 8
%stackaddr$prim56149 = alloca %struct.ScmObj*, align 8
%current_45args54558 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54557)
store volatile %struct.ScmObj* %current_45args54558, %struct.ScmObj** %stackaddr$prim56149, align 8
%stackaddr$prim56150 = alloca %struct.ScmObj*, align 8
%anf_45bind48180 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54558)
store volatile %struct.ScmObj* %anf_45bind48180, %struct.ScmObj** %stackaddr$prim56150, align 8
%ae49088 = call %struct.ScmObj* @const_init_null()
%argslist54560$_37foldl1480300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56151 = alloca %struct.ScmObj*, align 8
%argslist54560$_37foldl1480301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48069, %struct.ScmObj* %argslist54560$_37foldl1480300)
store volatile %struct.ScmObj* %argslist54560$_37foldl1480301, %struct.ScmObj** %stackaddr$prim56151, align 8
%stackaddr$prim56152 = alloca %struct.ScmObj*, align 8
%argslist54560$_37foldl1480302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49088, %struct.ScmObj* %argslist54560$_37foldl1480301)
store volatile %struct.ScmObj* %argslist54560$_37foldl1480302, %struct.ScmObj** %stackaddr$prim56152, align 8
%stackaddr$prim56153 = alloca %struct.ScmObj*, align 8
%argslist54560$_37foldl1480303 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48180, %struct.ScmObj* %argslist54560$_37foldl1480302)
store volatile %struct.ScmObj* %argslist54560$_37foldl1480303, %struct.ScmObj** %stackaddr$prim56153, align 8
%stackaddr$prim56154 = alloca %struct.ScmObj*, align 8
%argslist54560$_37foldl1480304 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48412, %struct.ScmObj* %argslist54560$_37foldl1480303)
store volatile %struct.ScmObj* %argslist54560$_37foldl1480304, %struct.ScmObj** %stackaddr$prim56154, align 8
%clofunc56155 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148030)
musttail call tailcc void %clofunc56155(%struct.ScmObj* %_37foldl148030, %struct.ScmObj* %argslist54560$_37foldl1480304)
ret void
}

define tailcc void @proc_clo$ae49069(%struct.ScmObj* %env$ae49069,%struct.ScmObj* %current_45args54561) {
%stackaddr$prim56156 = alloca %struct.ScmObj*, align 8
%k48414 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54561)
store volatile %struct.ScmObj* %k48414, %struct.ScmObj** %stackaddr$prim56156, align 8
%stackaddr$prim56157 = alloca %struct.ScmObj*, align 8
%current_45args54562 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54561)
store volatile %struct.ScmObj* %current_45args54562, %struct.ScmObj** %stackaddr$prim56157, align 8
%stackaddr$prim56158 = alloca %struct.ScmObj*, align 8
%x48071 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54562)
store volatile %struct.ScmObj* %x48071, %struct.ScmObj** %stackaddr$prim56158, align 8
%stackaddr$prim56159 = alloca %struct.ScmObj*, align 8
%current_45args54563 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54562)
store volatile %struct.ScmObj* %current_45args54563, %struct.ScmObj** %stackaddr$prim56159, align 8
%stackaddr$prim56160 = alloca %struct.ScmObj*, align 8
%y48070 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54563)
store volatile %struct.ScmObj* %y48070, %struct.ScmObj** %stackaddr$prim56160, align 8
%ae49071 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54565$k484140 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56161 = alloca %struct.ScmObj*, align 8
%argslist54565$k484141 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x48071, %struct.ScmObj* %argslist54565$k484140)
store volatile %struct.ScmObj* %argslist54565$k484141, %struct.ScmObj** %stackaddr$prim56161, align 8
%stackaddr$prim56162 = alloca %struct.ScmObj*, align 8
%argslist54565$k484142 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49071, %struct.ScmObj* %argslist54565$k484141)
store volatile %struct.ScmObj* %argslist54565$k484142, %struct.ScmObj** %stackaddr$prim56162, align 8
%clofunc56163 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48414)
musttail call tailcc void %clofunc56163(%struct.ScmObj* %k48414, %struct.ScmObj* %argslist54565$k484142)
ret void
}

define tailcc void @proc_clo$ae48987(%struct.ScmObj* %env$ae48987,%struct.ScmObj* %current_45args54569) {
%stackaddr$prim56164 = alloca %struct.ScmObj*, align 8
%k48415 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54569)
store volatile %struct.ScmObj* %k48415, %struct.ScmObj** %stackaddr$prim56164, align 8
%stackaddr$prim56165 = alloca %struct.ScmObj*, align 8
%current_45args54570 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54569)
store volatile %struct.ScmObj* %current_45args54570, %struct.ScmObj** %stackaddr$prim56165, align 8
%stackaddr$prim56166 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54570)
store volatile %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$prim56166, align 8
%ae48989 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56167 = alloca %struct.ScmObj*, align 8
%fptrToInt56168 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48990 to i64
%ae48990 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56168)
store volatile %struct.ScmObj* %ae48990, %struct.ScmObj** %stackaddr$makeclosure56167, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48990, %struct.ScmObj* %_37foldl148031, i64 0)
%argslist54583$k484150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56169 = alloca %struct.ScmObj*, align 8
%argslist54583$k484151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48990, %struct.ScmObj* %argslist54583$k484150)
store volatile %struct.ScmObj* %argslist54583$k484151, %struct.ScmObj** %stackaddr$prim56169, align 8
%stackaddr$prim56170 = alloca %struct.ScmObj*, align 8
%argslist54583$k484152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48989, %struct.ScmObj* %argslist54583$k484151)
store volatile %struct.ScmObj* %argslist54583$k484152, %struct.ScmObj** %stackaddr$prim56170, align 8
%clofunc56171 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48415)
musttail call tailcc void %clofunc56171(%struct.ScmObj* %k48415, %struct.ScmObj* %argslist54583$k484152)
ret void
}

define tailcc void @proc_clo$ae48990(%struct.ScmObj* %env$ae48990,%struct.ScmObj* %current_45args54572) {
%stackaddr$env-ref56172 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48990, i64 0)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref56172
%stackaddr$prim56173 = alloca %struct.ScmObj*, align 8
%k48416 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54572)
store volatile %struct.ScmObj* %k48416, %struct.ScmObj** %stackaddr$prim56173, align 8
%stackaddr$prim56174 = alloca %struct.ScmObj*, align 8
%current_45args54573 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54572)
store volatile %struct.ScmObj* %current_45args54573, %struct.ScmObj** %stackaddr$prim56174, align 8
%stackaddr$prim56175 = alloca %struct.ScmObj*, align 8
%f48034 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54573)
store volatile %struct.ScmObj* %f48034, %struct.ScmObj** %stackaddr$prim56175, align 8
%stackaddr$prim56176 = alloca %struct.ScmObj*, align 8
%current_45args54574 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54573)
store volatile %struct.ScmObj* %current_45args54574, %struct.ScmObj** %stackaddr$prim56176, align 8
%stackaddr$prim56177 = alloca %struct.ScmObj*, align 8
%acc48033 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54574)
store volatile %struct.ScmObj* %acc48033, %struct.ScmObj** %stackaddr$prim56177, align 8
%stackaddr$prim56178 = alloca %struct.ScmObj*, align 8
%current_45args54575 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54574)
store volatile %struct.ScmObj* %current_45args54575, %struct.ScmObj** %stackaddr$prim56178, align 8
%stackaddr$prim56179 = alloca %struct.ScmObj*, align 8
%lst48032 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54575)
store volatile %struct.ScmObj* %lst48032, %struct.ScmObj** %stackaddr$prim56179, align 8
%stackaddr$prim56180 = alloca %struct.ScmObj*, align 8
%anf_45bind48175 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48032)
store volatile %struct.ScmObj* %anf_45bind48175, %struct.ScmObj** %stackaddr$prim56180, align 8
%truthy$cmp56181 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48175)
%cmp$cmp56181 = icmp eq i64 %truthy$cmp56181, 1
br i1 %cmp$cmp56181, label %truebranch$cmp56181, label %falsebranch$cmp56181
truebranch$cmp56181:
%ae48994 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54577$k484160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56182 = alloca %struct.ScmObj*, align 8
%argslist54577$k484161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48033, %struct.ScmObj* %argslist54577$k484160)
store volatile %struct.ScmObj* %argslist54577$k484161, %struct.ScmObj** %stackaddr$prim56182, align 8
%stackaddr$prim56183 = alloca %struct.ScmObj*, align 8
%argslist54577$k484162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48994, %struct.ScmObj* %argslist54577$k484161)
store volatile %struct.ScmObj* %argslist54577$k484162, %struct.ScmObj** %stackaddr$prim56183, align 8
%clofunc56184 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48416)
musttail call tailcc void %clofunc56184(%struct.ScmObj* %k48416, %struct.ScmObj* %argslist54577$k484162)
ret void
falsebranch$cmp56181:
%stackaddr$prim56185 = alloca %struct.ScmObj*, align 8
%anf_45bind48176 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48032)
store volatile %struct.ScmObj* %anf_45bind48176, %struct.ScmObj** %stackaddr$prim56185, align 8
%stackaddr$makeclosure56186 = alloca %struct.ScmObj*, align 8
%fptrToInt56187 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49001 to i64
%ae49001 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56187)
store volatile %struct.ScmObj* %ae49001, %struct.ScmObj** %stackaddr$makeclosure56186, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49001, %struct.ScmObj* %lst48032, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49001, %struct.ScmObj* %k48416, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49001, %struct.ScmObj* %f48034, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49001, %struct.ScmObj* %_37foldl148031, i64 3)
%argslist54582$f480340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56188 = alloca %struct.ScmObj*, align 8
%argslist54582$f480341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48033, %struct.ScmObj* %argslist54582$f480340)
store volatile %struct.ScmObj* %argslist54582$f480341, %struct.ScmObj** %stackaddr$prim56188, align 8
%stackaddr$prim56189 = alloca %struct.ScmObj*, align 8
%argslist54582$f480342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48176, %struct.ScmObj* %argslist54582$f480341)
store volatile %struct.ScmObj* %argslist54582$f480342, %struct.ScmObj** %stackaddr$prim56189, align 8
%stackaddr$prim56190 = alloca %struct.ScmObj*, align 8
%argslist54582$f480343 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49001, %struct.ScmObj* %argslist54582$f480342)
store volatile %struct.ScmObj* %argslist54582$f480343, %struct.ScmObj** %stackaddr$prim56190, align 8
%clofunc56191 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48034)
musttail call tailcc void %clofunc56191(%struct.ScmObj* %f48034, %struct.ScmObj* %argslist54582$f480343)
ret void
}

define tailcc void @proc_clo$ae49001(%struct.ScmObj* %env$ae49001,%struct.ScmObj* %current_45args54578) {
%stackaddr$env-ref56192 = alloca %struct.ScmObj*, align 8
%lst48032 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49001, i64 0)
store %struct.ScmObj* %lst48032, %struct.ScmObj** %stackaddr$env-ref56192
%stackaddr$env-ref56193 = alloca %struct.ScmObj*, align 8
%k48416 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49001, i64 1)
store %struct.ScmObj* %k48416, %struct.ScmObj** %stackaddr$env-ref56193
%stackaddr$env-ref56194 = alloca %struct.ScmObj*, align 8
%f48034 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49001, i64 2)
store %struct.ScmObj* %f48034, %struct.ScmObj** %stackaddr$env-ref56194
%stackaddr$env-ref56195 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49001, i64 3)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref56195
%stackaddr$prim56196 = alloca %struct.ScmObj*, align 8
%_95k48417 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54578)
store volatile %struct.ScmObj* %_95k48417, %struct.ScmObj** %stackaddr$prim56196, align 8
%stackaddr$prim56197 = alloca %struct.ScmObj*, align 8
%current_45args54579 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54578)
store volatile %struct.ScmObj* %current_45args54579, %struct.ScmObj** %stackaddr$prim56197, align 8
%stackaddr$prim56198 = alloca %struct.ScmObj*, align 8
%anf_45bind48177 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54579)
store volatile %struct.ScmObj* %anf_45bind48177, %struct.ScmObj** %stackaddr$prim56198, align 8
%stackaddr$prim56199 = alloca %struct.ScmObj*, align 8
%anf_45bind48178 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48032)
store volatile %struct.ScmObj* %anf_45bind48178, %struct.ScmObj** %stackaddr$prim56199, align 8
%argslist54581$_37foldl1480310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56200 = alloca %struct.ScmObj*, align 8
%argslist54581$_37foldl1480311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48178, %struct.ScmObj* %argslist54581$_37foldl1480310)
store volatile %struct.ScmObj* %argslist54581$_37foldl1480311, %struct.ScmObj** %stackaddr$prim56200, align 8
%stackaddr$prim56201 = alloca %struct.ScmObj*, align 8
%argslist54581$_37foldl1480312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48177, %struct.ScmObj* %argslist54581$_37foldl1480311)
store volatile %struct.ScmObj* %argslist54581$_37foldl1480312, %struct.ScmObj** %stackaddr$prim56201, align 8
%stackaddr$prim56202 = alloca %struct.ScmObj*, align 8
%argslist54581$_37foldl1480313 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48034, %struct.ScmObj* %argslist54581$_37foldl1480312)
store volatile %struct.ScmObj* %argslist54581$_37foldl1480313, %struct.ScmObj** %stackaddr$prim56202, align 8
%stackaddr$prim56203 = alloca %struct.ScmObj*, align 8
%argslist54581$_37foldl1480314 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48416, %struct.ScmObj* %argslist54581$_37foldl1480313)
store volatile %struct.ScmObj* %argslist54581$_37foldl1480314, %struct.ScmObj** %stackaddr$prim56203, align 8
%clofunc56204 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148031)
musttail call tailcc void %clofunc56204(%struct.ScmObj* %_37foldl148031, %struct.ScmObj* %argslist54581$_37foldl1480314)
ret void
}

define tailcc void @proc_clo$ae48904(%struct.ScmObj* %env$ae48904,%struct.ScmObj* %current_45args54586) {
%stackaddr$prim56205 = alloca %struct.ScmObj*, align 8
%k48418 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54586)
store volatile %struct.ScmObj* %k48418, %struct.ScmObj** %stackaddr$prim56205, align 8
%stackaddr$prim56206 = alloca %struct.ScmObj*, align 8
%current_45args54587 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54586)
store volatile %struct.ScmObj* %current_45args54587, %struct.ScmObj** %stackaddr$prim56206, align 8
%stackaddr$prim56207 = alloca %struct.ScmObj*, align 8
%_37length48036 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54587)
store volatile %struct.ScmObj* %_37length48036, %struct.ScmObj** %stackaddr$prim56207, align 8
%ae48906 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56208 = alloca %struct.ScmObj*, align 8
%fptrToInt56209 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48907 to i64
%ae48907 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56209)
store volatile %struct.ScmObj* %ae48907, %struct.ScmObj** %stackaddr$makeclosure56208, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48907, %struct.ScmObj* %_37length48036, i64 0)
%argslist54598$k484180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56210 = alloca %struct.ScmObj*, align 8
%argslist54598$k484181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48907, %struct.ScmObj* %argslist54598$k484180)
store volatile %struct.ScmObj* %argslist54598$k484181, %struct.ScmObj** %stackaddr$prim56210, align 8
%stackaddr$prim56211 = alloca %struct.ScmObj*, align 8
%argslist54598$k484182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48906, %struct.ScmObj* %argslist54598$k484181)
store volatile %struct.ScmObj* %argslist54598$k484182, %struct.ScmObj** %stackaddr$prim56211, align 8
%clofunc56212 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48418)
musttail call tailcc void %clofunc56212(%struct.ScmObj* %k48418, %struct.ScmObj* %argslist54598$k484182)
ret void
}

define tailcc void @proc_clo$ae48907(%struct.ScmObj* %env$ae48907,%struct.ScmObj* %current_45args54589) {
%stackaddr$env-ref56213 = alloca %struct.ScmObj*, align 8
%_37length48036 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48907, i64 0)
store %struct.ScmObj* %_37length48036, %struct.ScmObj** %stackaddr$env-ref56213
%stackaddr$prim56214 = alloca %struct.ScmObj*, align 8
%k48419 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54589)
store volatile %struct.ScmObj* %k48419, %struct.ScmObj** %stackaddr$prim56214, align 8
%stackaddr$prim56215 = alloca %struct.ScmObj*, align 8
%current_45args54590 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54589)
store volatile %struct.ScmObj* %current_45args54590, %struct.ScmObj** %stackaddr$prim56215, align 8
%stackaddr$prim56216 = alloca %struct.ScmObj*, align 8
%lst48037 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54590)
store volatile %struct.ScmObj* %lst48037, %struct.ScmObj** %stackaddr$prim56216, align 8
%stackaddr$prim56217 = alloca %struct.ScmObj*, align 8
%anf_45bind48171 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48037)
store volatile %struct.ScmObj* %anf_45bind48171, %struct.ScmObj** %stackaddr$prim56217, align 8
%truthy$cmp56218 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48171)
%cmp$cmp56218 = icmp eq i64 %truthy$cmp56218, 1
br i1 %cmp$cmp56218, label %truebranch$cmp56218, label %falsebranch$cmp56218
truebranch$cmp56218:
%ae48911 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48912 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54592$k484190 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56219 = alloca %struct.ScmObj*, align 8
%argslist54592$k484191 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48912, %struct.ScmObj* %argslist54592$k484190)
store volatile %struct.ScmObj* %argslist54592$k484191, %struct.ScmObj** %stackaddr$prim56219, align 8
%stackaddr$prim56220 = alloca %struct.ScmObj*, align 8
%argslist54592$k484192 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48911, %struct.ScmObj* %argslist54592$k484191)
store volatile %struct.ScmObj* %argslist54592$k484192, %struct.ScmObj** %stackaddr$prim56220, align 8
%clofunc56221 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48419)
musttail call tailcc void %clofunc56221(%struct.ScmObj* %k48419, %struct.ScmObj* %argslist54592$k484192)
ret void
falsebranch$cmp56218:
%stackaddr$prim56222 = alloca %struct.ScmObj*, align 8
%anf_45bind48172 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48037)
store volatile %struct.ScmObj* %anf_45bind48172, %struct.ScmObj** %stackaddr$prim56222, align 8
%stackaddr$makeclosure56223 = alloca %struct.ScmObj*, align 8
%fptrToInt56224 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48921 to i64
%ae48921 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56224)
store volatile %struct.ScmObj* %ae48921, %struct.ScmObj** %stackaddr$makeclosure56223, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48921, %struct.ScmObj* %k48419, i64 0)
%argslist54597$_37length480360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56225 = alloca %struct.ScmObj*, align 8
%argslist54597$_37length480361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48172, %struct.ScmObj* %argslist54597$_37length480360)
store volatile %struct.ScmObj* %argslist54597$_37length480361, %struct.ScmObj** %stackaddr$prim56225, align 8
%stackaddr$prim56226 = alloca %struct.ScmObj*, align 8
%argslist54597$_37length480362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48921, %struct.ScmObj* %argslist54597$_37length480361)
store volatile %struct.ScmObj* %argslist54597$_37length480362, %struct.ScmObj** %stackaddr$prim56226, align 8
%clofunc56227 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length48036)
musttail call tailcc void %clofunc56227(%struct.ScmObj* %_37length48036, %struct.ScmObj* %argslist54597$_37length480362)
ret void
}

define tailcc void @proc_clo$ae48921(%struct.ScmObj* %env$ae48921,%struct.ScmObj* %current_45args54593) {
%stackaddr$env-ref56228 = alloca %struct.ScmObj*, align 8
%k48419 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48921, i64 0)
store %struct.ScmObj* %k48419, %struct.ScmObj** %stackaddr$env-ref56228
%stackaddr$prim56229 = alloca %struct.ScmObj*, align 8
%_95k48420 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54593)
store volatile %struct.ScmObj* %_95k48420, %struct.ScmObj** %stackaddr$prim56229, align 8
%stackaddr$prim56230 = alloca %struct.ScmObj*, align 8
%current_45args54594 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54593)
store volatile %struct.ScmObj* %current_45args54594, %struct.ScmObj** %stackaddr$prim56230, align 8
%stackaddr$prim56231 = alloca %struct.ScmObj*, align 8
%anf_45bind48173 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54594)
store volatile %struct.ScmObj* %anf_45bind48173, %struct.ScmObj** %stackaddr$prim56231, align 8
%ae48923 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56232 = alloca %struct.ScmObj*, align 8
%cpsprim48421 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae48923, %struct.ScmObj* %anf_45bind48173)
store volatile %struct.ScmObj* %cpsprim48421, %struct.ScmObj** %stackaddr$prim56232, align 8
%ae48926 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54596$k484190 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56233 = alloca %struct.ScmObj*, align 8
%argslist54596$k484191 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48421, %struct.ScmObj* %argslist54596$k484190)
store volatile %struct.ScmObj* %argslist54596$k484191, %struct.ScmObj** %stackaddr$prim56233, align 8
%stackaddr$prim56234 = alloca %struct.ScmObj*, align 8
%argslist54596$k484192 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48926, %struct.ScmObj* %argslist54596$k484191)
store volatile %struct.ScmObj* %argslist54596$k484192, %struct.ScmObj** %stackaddr$prim56234, align 8
%clofunc56235 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48419)
musttail call tailcc void %clofunc56235(%struct.ScmObj* %k48419, %struct.ScmObj* %argslist54596$k484192)
ret void
}

define tailcc void @proc_clo$ae48754(%struct.ScmObj* %env$ae48754,%struct.ScmObj* %current_45args54601) {
%stackaddr$prim56236 = alloca %struct.ScmObj*, align 8
%k48422 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54601)
store volatile %struct.ScmObj* %k48422, %struct.ScmObj** %stackaddr$prim56236, align 8
%stackaddr$prim56237 = alloca %struct.ScmObj*, align 8
%current_45args54602 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54601)
store volatile %struct.ScmObj* %current_45args54602, %struct.ScmObj** %stackaddr$prim56237, align 8
%stackaddr$prim56238 = alloca %struct.ScmObj*, align 8
%_37take48039 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54602)
store volatile %struct.ScmObj* %_37take48039, %struct.ScmObj** %stackaddr$prim56238, align 8
%ae48756 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56239 = alloca %struct.ScmObj*, align 8
%fptrToInt56240 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48757 to i64
%ae48757 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56240)
store volatile %struct.ScmObj* %ae48757, %struct.ScmObj** %stackaddr$makeclosure56239, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48757, %struct.ScmObj* %_37take48039, i64 0)
%argslist54615$k484220 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56241 = alloca %struct.ScmObj*, align 8
%argslist54615$k484221 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48757, %struct.ScmObj* %argslist54615$k484220)
store volatile %struct.ScmObj* %argslist54615$k484221, %struct.ScmObj** %stackaddr$prim56241, align 8
%stackaddr$prim56242 = alloca %struct.ScmObj*, align 8
%argslist54615$k484222 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48756, %struct.ScmObj* %argslist54615$k484221)
store volatile %struct.ScmObj* %argslist54615$k484222, %struct.ScmObj** %stackaddr$prim56242, align 8
%clofunc56243 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48422)
musttail call tailcc void %clofunc56243(%struct.ScmObj* %k48422, %struct.ScmObj* %argslist54615$k484222)
ret void
}

define tailcc void @proc_clo$ae48757(%struct.ScmObj* %env$ae48757,%struct.ScmObj* %current_45args54604) {
%stackaddr$env-ref56244 = alloca %struct.ScmObj*, align 8
%_37take48039 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48757, i64 0)
store %struct.ScmObj* %_37take48039, %struct.ScmObj** %stackaddr$env-ref56244
%stackaddr$prim56245 = alloca %struct.ScmObj*, align 8
%k48423 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54604)
store volatile %struct.ScmObj* %k48423, %struct.ScmObj** %stackaddr$prim56245, align 8
%stackaddr$prim56246 = alloca %struct.ScmObj*, align 8
%current_45args54605 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54604)
store volatile %struct.ScmObj* %current_45args54605, %struct.ScmObj** %stackaddr$prim56246, align 8
%stackaddr$prim56247 = alloca %struct.ScmObj*, align 8
%lst48041 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54605)
store volatile %struct.ScmObj* %lst48041, %struct.ScmObj** %stackaddr$prim56247, align 8
%stackaddr$prim56248 = alloca %struct.ScmObj*, align 8
%current_45args54606 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54605)
store volatile %struct.ScmObj* %current_45args54606, %struct.ScmObj** %stackaddr$prim56248, align 8
%stackaddr$prim56249 = alloca %struct.ScmObj*, align 8
%n48040 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54606)
store volatile %struct.ScmObj* %n48040, %struct.ScmObj** %stackaddr$prim56249, align 8
%ae48759 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56250 = alloca %struct.ScmObj*, align 8
%anf_45bind48164 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n48040, %struct.ScmObj* %ae48759)
store volatile %struct.ScmObj* %anf_45bind48164, %struct.ScmObj** %stackaddr$prim56250, align 8
%truthy$cmp56251 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48164)
%cmp$cmp56251 = icmp eq i64 %truthy$cmp56251, 1
br i1 %cmp$cmp56251, label %truebranch$cmp56251, label %falsebranch$cmp56251
truebranch$cmp56251:
%ae48762 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48763 = call %struct.ScmObj* @const_init_null()
%argslist54608$k484230 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56252 = alloca %struct.ScmObj*, align 8
%argslist54608$k484231 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48763, %struct.ScmObj* %argslist54608$k484230)
store volatile %struct.ScmObj* %argslist54608$k484231, %struct.ScmObj** %stackaddr$prim56252, align 8
%stackaddr$prim56253 = alloca %struct.ScmObj*, align 8
%argslist54608$k484232 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48762, %struct.ScmObj* %argslist54608$k484231)
store volatile %struct.ScmObj* %argslist54608$k484232, %struct.ScmObj** %stackaddr$prim56253, align 8
%clofunc56254 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48423)
musttail call tailcc void %clofunc56254(%struct.ScmObj* %k48423, %struct.ScmObj* %argslist54608$k484232)
ret void
falsebranch$cmp56251:
%stackaddr$prim56255 = alloca %struct.ScmObj*, align 8
%anf_45bind48165 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48041)
store volatile %struct.ScmObj* %anf_45bind48165, %struct.ScmObj** %stackaddr$prim56255, align 8
%truthy$cmp56256 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48165)
%cmp$cmp56256 = icmp eq i64 %truthy$cmp56256, 1
br i1 %cmp$cmp56256, label %truebranch$cmp56256, label %falsebranch$cmp56256
truebranch$cmp56256:
%ae48773 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48774 = call %struct.ScmObj* @const_init_null()
%argslist54609$k484230 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56257 = alloca %struct.ScmObj*, align 8
%argslist54609$k484231 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48774, %struct.ScmObj* %argslist54609$k484230)
store volatile %struct.ScmObj* %argslist54609$k484231, %struct.ScmObj** %stackaddr$prim56257, align 8
%stackaddr$prim56258 = alloca %struct.ScmObj*, align 8
%argslist54609$k484232 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48773, %struct.ScmObj* %argslist54609$k484231)
store volatile %struct.ScmObj* %argslist54609$k484232, %struct.ScmObj** %stackaddr$prim56258, align 8
%clofunc56259 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48423)
musttail call tailcc void %clofunc56259(%struct.ScmObj* %k48423, %struct.ScmObj* %argslist54609$k484232)
ret void
falsebranch$cmp56256:
%stackaddr$prim56260 = alloca %struct.ScmObj*, align 8
%anf_45bind48166 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48041)
store volatile %struct.ScmObj* %anf_45bind48166, %struct.ScmObj** %stackaddr$prim56260, align 8
%stackaddr$prim56261 = alloca %struct.ScmObj*, align 8
%anf_45bind48167 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48041)
store volatile %struct.ScmObj* %anf_45bind48167, %struct.ScmObj** %stackaddr$prim56261, align 8
%ae48784 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56262 = alloca %struct.ScmObj*, align 8
%anf_45bind48168 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n48040, %struct.ScmObj* %ae48784)
store volatile %struct.ScmObj* %anf_45bind48168, %struct.ScmObj** %stackaddr$prim56262, align 8
%stackaddr$makeclosure56263 = alloca %struct.ScmObj*, align 8
%fptrToInt56264 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48786 to i64
%ae48786 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56264)
store volatile %struct.ScmObj* %ae48786, %struct.ScmObj** %stackaddr$makeclosure56263, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48786, %struct.ScmObj* %k48423, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48786, %struct.ScmObj* %anf_45bind48166, i64 1)
%argslist54614$_37take480390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56265 = alloca %struct.ScmObj*, align 8
%argslist54614$_37take480391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48168, %struct.ScmObj* %argslist54614$_37take480390)
store volatile %struct.ScmObj* %argslist54614$_37take480391, %struct.ScmObj** %stackaddr$prim56265, align 8
%stackaddr$prim56266 = alloca %struct.ScmObj*, align 8
%argslist54614$_37take480392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48167, %struct.ScmObj* %argslist54614$_37take480391)
store volatile %struct.ScmObj* %argslist54614$_37take480392, %struct.ScmObj** %stackaddr$prim56266, align 8
%stackaddr$prim56267 = alloca %struct.ScmObj*, align 8
%argslist54614$_37take480393 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48786, %struct.ScmObj* %argslist54614$_37take480392)
store volatile %struct.ScmObj* %argslist54614$_37take480393, %struct.ScmObj** %stackaddr$prim56267, align 8
%clofunc56268 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take48039)
musttail call tailcc void %clofunc56268(%struct.ScmObj* %_37take48039, %struct.ScmObj* %argslist54614$_37take480393)
ret void
}

define tailcc void @proc_clo$ae48786(%struct.ScmObj* %env$ae48786,%struct.ScmObj* %current_45args54610) {
%stackaddr$env-ref56269 = alloca %struct.ScmObj*, align 8
%k48423 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48786, i64 0)
store %struct.ScmObj* %k48423, %struct.ScmObj** %stackaddr$env-ref56269
%stackaddr$env-ref56270 = alloca %struct.ScmObj*, align 8
%anf_45bind48166 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48786, i64 1)
store %struct.ScmObj* %anf_45bind48166, %struct.ScmObj** %stackaddr$env-ref56270
%stackaddr$prim56271 = alloca %struct.ScmObj*, align 8
%_95k48424 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54610)
store volatile %struct.ScmObj* %_95k48424, %struct.ScmObj** %stackaddr$prim56271, align 8
%stackaddr$prim56272 = alloca %struct.ScmObj*, align 8
%current_45args54611 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54610)
store volatile %struct.ScmObj* %current_45args54611, %struct.ScmObj** %stackaddr$prim56272, align 8
%stackaddr$prim56273 = alloca %struct.ScmObj*, align 8
%anf_45bind48169 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54611)
store volatile %struct.ScmObj* %anf_45bind48169, %struct.ScmObj** %stackaddr$prim56273, align 8
%stackaddr$prim56274 = alloca %struct.ScmObj*, align 8
%cpsprim48425 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48166, %struct.ScmObj* %anf_45bind48169)
store volatile %struct.ScmObj* %cpsprim48425, %struct.ScmObj** %stackaddr$prim56274, align 8
%ae48792 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54613$k484230 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56275 = alloca %struct.ScmObj*, align 8
%argslist54613$k484231 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48425, %struct.ScmObj* %argslist54613$k484230)
store volatile %struct.ScmObj* %argslist54613$k484231, %struct.ScmObj** %stackaddr$prim56275, align 8
%stackaddr$prim56276 = alloca %struct.ScmObj*, align 8
%argslist54613$k484232 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48792, %struct.ScmObj* %argslist54613$k484231)
store volatile %struct.ScmObj* %argslist54613$k484232, %struct.ScmObj** %stackaddr$prim56276, align 8
%clofunc56277 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48423)
musttail call tailcc void %clofunc56277(%struct.ScmObj* %k48423, %struct.ScmObj* %argslist54613$k484232)
ret void
}

define tailcc void @proc_clo$ae48657(%struct.ScmObj* %env$ae48657,%struct.ScmObj* %current_45args54618) {
%stackaddr$prim56278 = alloca %struct.ScmObj*, align 8
%k48426 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54618)
store volatile %struct.ScmObj* %k48426, %struct.ScmObj** %stackaddr$prim56278, align 8
%stackaddr$prim56279 = alloca %struct.ScmObj*, align 8
%current_45args54619 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54618)
store volatile %struct.ScmObj* %current_45args54619, %struct.ScmObj** %stackaddr$prim56279, align 8
%stackaddr$prim56280 = alloca %struct.ScmObj*, align 8
%_37map48043 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54619)
store volatile %struct.ScmObj* %_37map48043, %struct.ScmObj** %stackaddr$prim56280, align 8
%ae48659 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56281 = alloca %struct.ScmObj*, align 8
%fptrToInt56282 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48660 to i64
%ae48660 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56282)
store volatile %struct.ScmObj* %ae48660, %struct.ScmObj** %stackaddr$makeclosure56281, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48660, %struct.ScmObj* %_37map48043, i64 0)
%argslist54635$k484260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56283 = alloca %struct.ScmObj*, align 8
%argslist54635$k484261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48660, %struct.ScmObj* %argslist54635$k484260)
store volatile %struct.ScmObj* %argslist54635$k484261, %struct.ScmObj** %stackaddr$prim56283, align 8
%stackaddr$prim56284 = alloca %struct.ScmObj*, align 8
%argslist54635$k484262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48659, %struct.ScmObj* %argslist54635$k484261)
store volatile %struct.ScmObj* %argslist54635$k484262, %struct.ScmObj** %stackaddr$prim56284, align 8
%clofunc56285 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48426)
musttail call tailcc void %clofunc56285(%struct.ScmObj* %k48426, %struct.ScmObj* %argslist54635$k484262)
ret void
}

define tailcc void @proc_clo$ae48660(%struct.ScmObj* %env$ae48660,%struct.ScmObj* %current_45args54621) {
%stackaddr$env-ref56286 = alloca %struct.ScmObj*, align 8
%_37map48043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48660, i64 0)
store %struct.ScmObj* %_37map48043, %struct.ScmObj** %stackaddr$env-ref56286
%stackaddr$prim56287 = alloca %struct.ScmObj*, align 8
%k48427 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54621)
store volatile %struct.ScmObj* %k48427, %struct.ScmObj** %stackaddr$prim56287, align 8
%stackaddr$prim56288 = alloca %struct.ScmObj*, align 8
%current_45args54622 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54621)
store volatile %struct.ScmObj* %current_45args54622, %struct.ScmObj** %stackaddr$prim56288, align 8
%stackaddr$prim56289 = alloca %struct.ScmObj*, align 8
%f48045 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54622)
store volatile %struct.ScmObj* %f48045, %struct.ScmObj** %stackaddr$prim56289, align 8
%stackaddr$prim56290 = alloca %struct.ScmObj*, align 8
%current_45args54623 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54622)
store volatile %struct.ScmObj* %current_45args54623, %struct.ScmObj** %stackaddr$prim56290, align 8
%stackaddr$prim56291 = alloca %struct.ScmObj*, align 8
%lst48044 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54623)
store volatile %struct.ScmObj* %lst48044, %struct.ScmObj** %stackaddr$prim56291, align 8
%stackaddr$prim56292 = alloca %struct.ScmObj*, align 8
%anf_45bind48158 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48044)
store volatile %struct.ScmObj* %anf_45bind48158, %struct.ScmObj** %stackaddr$prim56292, align 8
%truthy$cmp56293 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48158)
%cmp$cmp56293 = icmp eq i64 %truthy$cmp56293, 1
br i1 %cmp$cmp56293, label %truebranch$cmp56293, label %falsebranch$cmp56293
truebranch$cmp56293:
%ae48664 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48665 = call %struct.ScmObj* @const_init_null()
%argslist54625$k484270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56294 = alloca %struct.ScmObj*, align 8
%argslist54625$k484271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48665, %struct.ScmObj* %argslist54625$k484270)
store volatile %struct.ScmObj* %argslist54625$k484271, %struct.ScmObj** %stackaddr$prim56294, align 8
%stackaddr$prim56295 = alloca %struct.ScmObj*, align 8
%argslist54625$k484272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48664, %struct.ScmObj* %argslist54625$k484271)
store volatile %struct.ScmObj* %argslist54625$k484272, %struct.ScmObj** %stackaddr$prim56295, align 8
%clofunc56296 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48427)
musttail call tailcc void %clofunc56296(%struct.ScmObj* %k48427, %struct.ScmObj* %argslist54625$k484272)
ret void
falsebranch$cmp56293:
%stackaddr$prim56297 = alloca %struct.ScmObj*, align 8
%anf_45bind48159 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48044)
store volatile %struct.ScmObj* %anf_45bind48159, %struct.ScmObj** %stackaddr$prim56297, align 8
%stackaddr$makeclosure56298 = alloca %struct.ScmObj*, align 8
%fptrToInt56299 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48674 to i64
%ae48674 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56299)
store volatile %struct.ScmObj* %ae48674, %struct.ScmObj** %stackaddr$makeclosure56298, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48674, %struct.ScmObj* %_37map48043, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48674, %struct.ScmObj* %k48427, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48674, %struct.ScmObj* %f48045, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48674, %struct.ScmObj* %lst48044, i64 3)
%argslist54634$f480450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56300 = alloca %struct.ScmObj*, align 8
%argslist54634$f480451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48159, %struct.ScmObj* %argslist54634$f480450)
store volatile %struct.ScmObj* %argslist54634$f480451, %struct.ScmObj** %stackaddr$prim56300, align 8
%stackaddr$prim56301 = alloca %struct.ScmObj*, align 8
%argslist54634$f480452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48674, %struct.ScmObj* %argslist54634$f480451)
store volatile %struct.ScmObj* %argslist54634$f480452, %struct.ScmObj** %stackaddr$prim56301, align 8
%clofunc56302 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48045)
musttail call tailcc void %clofunc56302(%struct.ScmObj* %f48045, %struct.ScmObj* %argslist54634$f480452)
ret void
}

define tailcc void @proc_clo$ae48674(%struct.ScmObj* %env$ae48674,%struct.ScmObj* %current_45args54626) {
%stackaddr$env-ref56303 = alloca %struct.ScmObj*, align 8
%_37map48043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48674, i64 0)
store %struct.ScmObj* %_37map48043, %struct.ScmObj** %stackaddr$env-ref56303
%stackaddr$env-ref56304 = alloca %struct.ScmObj*, align 8
%k48427 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48674, i64 1)
store %struct.ScmObj* %k48427, %struct.ScmObj** %stackaddr$env-ref56304
%stackaddr$env-ref56305 = alloca %struct.ScmObj*, align 8
%f48045 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48674, i64 2)
store %struct.ScmObj* %f48045, %struct.ScmObj** %stackaddr$env-ref56305
%stackaddr$env-ref56306 = alloca %struct.ScmObj*, align 8
%lst48044 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48674, i64 3)
store %struct.ScmObj* %lst48044, %struct.ScmObj** %stackaddr$env-ref56306
%stackaddr$prim56307 = alloca %struct.ScmObj*, align 8
%_95k48428 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54626)
store volatile %struct.ScmObj* %_95k48428, %struct.ScmObj** %stackaddr$prim56307, align 8
%stackaddr$prim56308 = alloca %struct.ScmObj*, align 8
%current_45args54627 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54626)
store volatile %struct.ScmObj* %current_45args54627, %struct.ScmObj** %stackaddr$prim56308, align 8
%stackaddr$prim56309 = alloca %struct.ScmObj*, align 8
%anf_45bind48160 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54627)
store volatile %struct.ScmObj* %anf_45bind48160, %struct.ScmObj** %stackaddr$prim56309, align 8
%stackaddr$prim56310 = alloca %struct.ScmObj*, align 8
%anf_45bind48161 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48044)
store volatile %struct.ScmObj* %anf_45bind48161, %struct.ScmObj** %stackaddr$prim56310, align 8
%stackaddr$makeclosure56311 = alloca %struct.ScmObj*, align 8
%fptrToInt56312 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48678 to i64
%ae48678 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56312)
store volatile %struct.ScmObj* %ae48678, %struct.ScmObj** %stackaddr$makeclosure56311, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48678, %struct.ScmObj* %anf_45bind48160, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48678, %struct.ScmObj* %k48427, i64 1)
%argslist54633$_37map480430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56313 = alloca %struct.ScmObj*, align 8
%argslist54633$_37map480431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48161, %struct.ScmObj* %argslist54633$_37map480430)
store volatile %struct.ScmObj* %argslist54633$_37map480431, %struct.ScmObj** %stackaddr$prim56313, align 8
%stackaddr$prim56314 = alloca %struct.ScmObj*, align 8
%argslist54633$_37map480432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48045, %struct.ScmObj* %argslist54633$_37map480431)
store volatile %struct.ScmObj* %argslist54633$_37map480432, %struct.ScmObj** %stackaddr$prim56314, align 8
%stackaddr$prim56315 = alloca %struct.ScmObj*, align 8
%argslist54633$_37map480433 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48678, %struct.ScmObj* %argslist54633$_37map480432)
store volatile %struct.ScmObj* %argslist54633$_37map480433, %struct.ScmObj** %stackaddr$prim56315, align 8
%clofunc56316 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map48043)
musttail call tailcc void %clofunc56316(%struct.ScmObj* %_37map48043, %struct.ScmObj* %argslist54633$_37map480433)
ret void
}

define tailcc void @proc_clo$ae48678(%struct.ScmObj* %env$ae48678,%struct.ScmObj* %current_45args54629) {
%stackaddr$env-ref56317 = alloca %struct.ScmObj*, align 8
%anf_45bind48160 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48678, i64 0)
store %struct.ScmObj* %anf_45bind48160, %struct.ScmObj** %stackaddr$env-ref56317
%stackaddr$env-ref56318 = alloca %struct.ScmObj*, align 8
%k48427 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48678, i64 1)
store %struct.ScmObj* %k48427, %struct.ScmObj** %stackaddr$env-ref56318
%stackaddr$prim56319 = alloca %struct.ScmObj*, align 8
%_95k48429 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54629)
store volatile %struct.ScmObj* %_95k48429, %struct.ScmObj** %stackaddr$prim56319, align 8
%stackaddr$prim56320 = alloca %struct.ScmObj*, align 8
%current_45args54630 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54629)
store volatile %struct.ScmObj* %current_45args54630, %struct.ScmObj** %stackaddr$prim56320, align 8
%stackaddr$prim56321 = alloca %struct.ScmObj*, align 8
%anf_45bind48162 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54630)
store volatile %struct.ScmObj* %anf_45bind48162, %struct.ScmObj** %stackaddr$prim56321, align 8
%stackaddr$prim56322 = alloca %struct.ScmObj*, align 8
%cpsprim48430 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48160, %struct.ScmObj* %anf_45bind48162)
store volatile %struct.ScmObj* %cpsprim48430, %struct.ScmObj** %stackaddr$prim56322, align 8
%ae48684 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54632$k484270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56323 = alloca %struct.ScmObj*, align 8
%argslist54632$k484271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48430, %struct.ScmObj* %argslist54632$k484270)
store volatile %struct.ScmObj* %argslist54632$k484271, %struct.ScmObj** %stackaddr$prim56323, align 8
%stackaddr$prim56324 = alloca %struct.ScmObj*, align 8
%argslist54632$k484272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48684, %struct.ScmObj* %argslist54632$k484271)
store volatile %struct.ScmObj* %argslist54632$k484272, %struct.ScmObj** %stackaddr$prim56324, align 8
%clofunc56325 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48427)
musttail call tailcc void %clofunc56325(%struct.ScmObj* %k48427, %struct.ScmObj* %argslist54632$k484272)
ret void
}

define tailcc void @proc_clo$ae48577(%struct.ScmObj* %env$ae48577,%struct.ScmObj* %current_45args54638) {
%stackaddr$prim56326 = alloca %struct.ScmObj*, align 8
%k48431 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54638)
store volatile %struct.ScmObj* %k48431, %struct.ScmObj** %stackaddr$prim56326, align 8
%stackaddr$prim56327 = alloca %struct.ScmObj*, align 8
%current_45args54639 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54638)
store volatile %struct.ScmObj* %current_45args54639, %struct.ScmObj** %stackaddr$prim56327, align 8
%stackaddr$prim56328 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54639)
store volatile %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$prim56328, align 8
%ae48579 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56329 = alloca %struct.ScmObj*, align 8
%fptrToInt56330 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48580 to i64
%ae48580 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56330)
store volatile %struct.ScmObj* %ae48580, %struct.ScmObj** %stackaddr$makeclosure56329, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48580, %struct.ScmObj* %_37foldr148047, i64 0)
%argslist54652$k484310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56331 = alloca %struct.ScmObj*, align 8
%argslist54652$k484311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48580, %struct.ScmObj* %argslist54652$k484310)
store volatile %struct.ScmObj* %argslist54652$k484311, %struct.ScmObj** %stackaddr$prim56331, align 8
%stackaddr$prim56332 = alloca %struct.ScmObj*, align 8
%argslist54652$k484312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48579, %struct.ScmObj* %argslist54652$k484311)
store volatile %struct.ScmObj* %argslist54652$k484312, %struct.ScmObj** %stackaddr$prim56332, align 8
%clofunc56333 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48431)
musttail call tailcc void %clofunc56333(%struct.ScmObj* %k48431, %struct.ScmObj* %argslist54652$k484312)
ret void
}

define tailcc void @proc_clo$ae48580(%struct.ScmObj* %env$ae48580,%struct.ScmObj* %current_45args54641) {
%stackaddr$env-ref56334 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48580, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref56334
%stackaddr$prim56335 = alloca %struct.ScmObj*, align 8
%k48432 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54641)
store volatile %struct.ScmObj* %k48432, %struct.ScmObj** %stackaddr$prim56335, align 8
%stackaddr$prim56336 = alloca %struct.ScmObj*, align 8
%current_45args54642 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54641)
store volatile %struct.ScmObj* %current_45args54642, %struct.ScmObj** %stackaddr$prim56336, align 8
%stackaddr$prim56337 = alloca %struct.ScmObj*, align 8
%f48050 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54642)
store volatile %struct.ScmObj* %f48050, %struct.ScmObj** %stackaddr$prim56337, align 8
%stackaddr$prim56338 = alloca %struct.ScmObj*, align 8
%current_45args54643 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54642)
store volatile %struct.ScmObj* %current_45args54643, %struct.ScmObj** %stackaddr$prim56338, align 8
%stackaddr$prim56339 = alloca %struct.ScmObj*, align 8
%acc48049 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54643)
store volatile %struct.ScmObj* %acc48049, %struct.ScmObj** %stackaddr$prim56339, align 8
%stackaddr$prim56340 = alloca %struct.ScmObj*, align 8
%current_45args54644 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54643)
store volatile %struct.ScmObj* %current_45args54644, %struct.ScmObj** %stackaddr$prim56340, align 8
%stackaddr$prim56341 = alloca %struct.ScmObj*, align 8
%lst48048 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54644)
store volatile %struct.ScmObj* %lst48048, %struct.ScmObj** %stackaddr$prim56341, align 8
%stackaddr$prim56342 = alloca %struct.ScmObj*, align 8
%anf_45bind48153 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48048)
store volatile %struct.ScmObj* %anf_45bind48153, %struct.ScmObj** %stackaddr$prim56342, align 8
%truthy$cmp56343 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48153)
%cmp$cmp56343 = icmp eq i64 %truthy$cmp56343, 1
br i1 %cmp$cmp56343, label %truebranch$cmp56343, label %falsebranch$cmp56343
truebranch$cmp56343:
%ae48584 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54646$k484320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56344 = alloca %struct.ScmObj*, align 8
%argslist54646$k484321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48049, %struct.ScmObj* %argslist54646$k484320)
store volatile %struct.ScmObj* %argslist54646$k484321, %struct.ScmObj** %stackaddr$prim56344, align 8
%stackaddr$prim56345 = alloca %struct.ScmObj*, align 8
%argslist54646$k484322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48584, %struct.ScmObj* %argslist54646$k484321)
store volatile %struct.ScmObj* %argslist54646$k484322, %struct.ScmObj** %stackaddr$prim56345, align 8
%clofunc56346 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48432)
musttail call tailcc void %clofunc56346(%struct.ScmObj* %k48432, %struct.ScmObj* %argslist54646$k484322)
ret void
falsebranch$cmp56343:
%stackaddr$prim56347 = alloca %struct.ScmObj*, align 8
%anf_45bind48154 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48048)
store volatile %struct.ScmObj* %anf_45bind48154, %struct.ScmObj** %stackaddr$prim56347, align 8
%stackaddr$prim56348 = alloca %struct.ScmObj*, align 8
%anf_45bind48155 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48048)
store volatile %struct.ScmObj* %anf_45bind48155, %struct.ScmObj** %stackaddr$prim56348, align 8
%stackaddr$makeclosure56349 = alloca %struct.ScmObj*, align 8
%fptrToInt56350 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48592 to i64
%ae48592 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56350)
store volatile %struct.ScmObj* %ae48592, %struct.ScmObj** %stackaddr$makeclosure56349, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48592, %struct.ScmObj* %f48050, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48592, %struct.ScmObj* %k48432, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48592, %struct.ScmObj* %anf_45bind48154, i64 2)
%argslist54651$_37foldr1480470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56351 = alloca %struct.ScmObj*, align 8
%argslist54651$_37foldr1480471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48155, %struct.ScmObj* %argslist54651$_37foldr1480470)
store volatile %struct.ScmObj* %argslist54651$_37foldr1480471, %struct.ScmObj** %stackaddr$prim56351, align 8
%stackaddr$prim56352 = alloca %struct.ScmObj*, align 8
%argslist54651$_37foldr1480472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48049, %struct.ScmObj* %argslist54651$_37foldr1480471)
store volatile %struct.ScmObj* %argslist54651$_37foldr1480472, %struct.ScmObj** %stackaddr$prim56352, align 8
%stackaddr$prim56353 = alloca %struct.ScmObj*, align 8
%argslist54651$_37foldr1480473 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48050, %struct.ScmObj* %argslist54651$_37foldr1480472)
store volatile %struct.ScmObj* %argslist54651$_37foldr1480473, %struct.ScmObj** %stackaddr$prim56353, align 8
%stackaddr$prim56354 = alloca %struct.ScmObj*, align 8
%argslist54651$_37foldr1480474 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48592, %struct.ScmObj* %argslist54651$_37foldr1480473)
store volatile %struct.ScmObj* %argslist54651$_37foldr1480474, %struct.ScmObj** %stackaddr$prim56354, align 8
%clofunc56355 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148047)
musttail call tailcc void %clofunc56355(%struct.ScmObj* %_37foldr148047, %struct.ScmObj* %argslist54651$_37foldr1480474)
ret void
}

define tailcc void @proc_clo$ae48592(%struct.ScmObj* %env$ae48592,%struct.ScmObj* %current_45args54647) {
%stackaddr$env-ref56356 = alloca %struct.ScmObj*, align 8
%f48050 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48592, i64 0)
store %struct.ScmObj* %f48050, %struct.ScmObj** %stackaddr$env-ref56356
%stackaddr$env-ref56357 = alloca %struct.ScmObj*, align 8
%k48432 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48592, i64 1)
store %struct.ScmObj* %k48432, %struct.ScmObj** %stackaddr$env-ref56357
%stackaddr$env-ref56358 = alloca %struct.ScmObj*, align 8
%anf_45bind48154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48592, i64 2)
store %struct.ScmObj* %anf_45bind48154, %struct.ScmObj** %stackaddr$env-ref56358
%stackaddr$prim56359 = alloca %struct.ScmObj*, align 8
%_95k48433 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54647)
store volatile %struct.ScmObj* %_95k48433, %struct.ScmObj** %stackaddr$prim56359, align 8
%stackaddr$prim56360 = alloca %struct.ScmObj*, align 8
%current_45args54648 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54647)
store volatile %struct.ScmObj* %current_45args54648, %struct.ScmObj** %stackaddr$prim56360, align 8
%stackaddr$prim56361 = alloca %struct.ScmObj*, align 8
%anf_45bind48156 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54648)
store volatile %struct.ScmObj* %anf_45bind48156, %struct.ScmObj** %stackaddr$prim56361, align 8
%argslist54650$f480500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56362 = alloca %struct.ScmObj*, align 8
%argslist54650$f480501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48156, %struct.ScmObj* %argslist54650$f480500)
store volatile %struct.ScmObj* %argslist54650$f480501, %struct.ScmObj** %stackaddr$prim56362, align 8
%stackaddr$prim56363 = alloca %struct.ScmObj*, align 8
%argslist54650$f480502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48154, %struct.ScmObj* %argslist54650$f480501)
store volatile %struct.ScmObj* %argslist54650$f480502, %struct.ScmObj** %stackaddr$prim56363, align 8
%stackaddr$prim56364 = alloca %struct.ScmObj*, align 8
%argslist54650$f480503 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48432, %struct.ScmObj* %argslist54650$f480502)
store volatile %struct.ScmObj* %argslist54650$f480503, %struct.ScmObj** %stackaddr$prim56364, align 8
%clofunc56365 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48050)
musttail call tailcc void %clofunc56365(%struct.ScmObj* %f48050, %struct.ScmObj* %argslist54650$f480503)
ret void
}

define tailcc void @proc_clo$ae48460(%struct.ScmObj* %env$ae48460,%struct.ScmObj* %current_45args54655) {
%stackaddr$prim56366 = alloca %struct.ScmObj*, align 8
%k48434 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54655)
store volatile %struct.ScmObj* %k48434, %struct.ScmObj** %stackaddr$prim56366, align 8
%stackaddr$prim56367 = alloca %struct.ScmObj*, align 8
%current_45args54656 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54655)
store volatile %struct.ScmObj* %current_45args54656, %struct.ScmObj** %stackaddr$prim56367, align 8
%stackaddr$prim56368 = alloca %struct.ScmObj*, align 8
%y48027 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54656)
store volatile %struct.ScmObj* %y48027, %struct.ScmObj** %stackaddr$prim56368, align 8
%ae48462 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56369 = alloca %struct.ScmObj*, align 8
%fptrToInt56370 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48463 to i64
%ae48463 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56370)
store volatile %struct.ScmObj* %ae48463, %struct.ScmObj** %stackaddr$makeclosure56369, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48463, %struct.ScmObj* %y48027, i64 0)
%argslist54674$k484340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56371 = alloca %struct.ScmObj*, align 8
%argslist54674$k484341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48463, %struct.ScmObj* %argslist54674$k484340)
store volatile %struct.ScmObj* %argslist54674$k484341, %struct.ScmObj** %stackaddr$prim56371, align 8
%stackaddr$prim56372 = alloca %struct.ScmObj*, align 8
%argslist54674$k484342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48462, %struct.ScmObj* %argslist54674$k484341)
store volatile %struct.ScmObj* %argslist54674$k484342, %struct.ScmObj** %stackaddr$prim56372, align 8
%clofunc56373 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48434)
musttail call tailcc void %clofunc56373(%struct.ScmObj* %k48434, %struct.ScmObj* %argslist54674$k484342)
ret void
}

define tailcc void @proc_clo$ae48463(%struct.ScmObj* %env$ae48463,%struct.ScmObj* %current_45args54658) {
%stackaddr$env-ref56374 = alloca %struct.ScmObj*, align 8
%y48027 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48463, i64 0)
store %struct.ScmObj* %y48027, %struct.ScmObj** %stackaddr$env-ref56374
%stackaddr$prim56375 = alloca %struct.ScmObj*, align 8
%k48435 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54658)
store volatile %struct.ScmObj* %k48435, %struct.ScmObj** %stackaddr$prim56375, align 8
%stackaddr$prim56376 = alloca %struct.ScmObj*, align 8
%current_45args54659 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54658)
store volatile %struct.ScmObj* %current_45args54659, %struct.ScmObj** %stackaddr$prim56376, align 8
%stackaddr$prim56377 = alloca %struct.ScmObj*, align 8
%f48028 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54659)
store volatile %struct.ScmObj* %f48028, %struct.ScmObj** %stackaddr$prim56377, align 8
%stackaddr$makeclosure56378 = alloca %struct.ScmObj*, align 8
%fptrToInt56379 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48464 to i64
%ae48464 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56379)
store volatile %struct.ScmObj* %ae48464, %struct.ScmObj** %stackaddr$makeclosure56378, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48464, %struct.ScmObj* %k48435, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48464, %struct.ScmObj* %f48028, i64 1)
%ae48465 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56380 = alloca %struct.ScmObj*, align 8
%fptrToInt56381 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48466 to i64
%ae48466 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56381)
store volatile %struct.ScmObj* %ae48466, %struct.ScmObj** %stackaddr$makeclosure56380, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48466, %struct.ScmObj* %f48028, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48466, %struct.ScmObj* %y48027, i64 1)
%argslist54673$ae484640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56382 = alloca %struct.ScmObj*, align 8
%argslist54673$ae484641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48466, %struct.ScmObj* %argslist54673$ae484640)
store volatile %struct.ScmObj* %argslist54673$ae484641, %struct.ScmObj** %stackaddr$prim56382, align 8
%stackaddr$prim56383 = alloca %struct.ScmObj*, align 8
%argslist54673$ae484642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48465, %struct.ScmObj* %argslist54673$ae484641)
store volatile %struct.ScmObj* %argslist54673$ae484642, %struct.ScmObj** %stackaddr$prim56383, align 8
%clofunc56384 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48464)
musttail call tailcc void %clofunc56384(%struct.ScmObj* %ae48464, %struct.ScmObj* %argslist54673$ae484642)
ret void
}

define tailcc void @proc_clo$ae48464(%struct.ScmObj* %env$ae48464,%struct.ScmObj* %current_45args54661) {
%stackaddr$env-ref56385 = alloca %struct.ScmObj*, align 8
%k48435 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48464, i64 0)
store %struct.ScmObj* %k48435, %struct.ScmObj** %stackaddr$env-ref56385
%stackaddr$env-ref56386 = alloca %struct.ScmObj*, align 8
%f48028 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48464, i64 1)
store %struct.ScmObj* %f48028, %struct.ScmObj** %stackaddr$env-ref56386
%stackaddr$prim56387 = alloca %struct.ScmObj*, align 8
%_95k48436 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54661)
store volatile %struct.ScmObj* %_95k48436, %struct.ScmObj** %stackaddr$prim56387, align 8
%stackaddr$prim56388 = alloca %struct.ScmObj*, align 8
%current_45args54662 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54661)
store volatile %struct.ScmObj* %current_45args54662, %struct.ScmObj** %stackaddr$prim56388, align 8
%stackaddr$prim56389 = alloca %struct.ScmObj*, align 8
%anf_45bind48151 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54662)
store volatile %struct.ScmObj* %anf_45bind48151, %struct.ScmObj** %stackaddr$prim56389, align 8
%argslist54664$f480280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56390 = alloca %struct.ScmObj*, align 8
%argslist54664$f480281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48151, %struct.ScmObj* %argslist54664$f480280)
store volatile %struct.ScmObj* %argslist54664$f480281, %struct.ScmObj** %stackaddr$prim56390, align 8
%stackaddr$prim56391 = alloca %struct.ScmObj*, align 8
%argslist54664$f480282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48435, %struct.ScmObj* %argslist54664$f480281)
store volatile %struct.ScmObj* %argslist54664$f480282, %struct.ScmObj** %stackaddr$prim56391, align 8
%clofunc56392 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48028)
musttail call tailcc void %clofunc56392(%struct.ScmObj* %f48028, %struct.ScmObj* %argslist54664$f480282)
ret void
}

define tailcc void @proc_clo$ae48466(%struct.ScmObj* %env$ae48466,%struct.ScmObj* %args4802948437) {
%stackaddr$env-ref56393 = alloca %struct.ScmObj*, align 8
%f48028 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48466, i64 0)
store %struct.ScmObj* %f48028, %struct.ScmObj** %stackaddr$env-ref56393
%stackaddr$env-ref56394 = alloca %struct.ScmObj*, align 8
%y48027 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48466, i64 1)
store %struct.ScmObj* %y48027, %struct.ScmObj** %stackaddr$env-ref56394
%stackaddr$prim56395 = alloca %struct.ScmObj*, align 8
%k48438 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4802948437)
store volatile %struct.ScmObj* %k48438, %struct.ScmObj** %stackaddr$prim56395, align 8
%stackaddr$prim56396 = alloca %struct.ScmObj*, align 8
%args48029 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4802948437)
store volatile %struct.ScmObj* %args48029, %struct.ScmObj** %stackaddr$prim56396, align 8
%stackaddr$makeclosure56397 = alloca %struct.ScmObj*, align 8
%fptrToInt56398 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48470 to i64
%ae48470 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56398)
store volatile %struct.ScmObj* %ae48470, %struct.ScmObj** %stackaddr$makeclosure56397, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48470, %struct.ScmObj* %k48438, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48470, %struct.ScmObj* %args48029, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48470, %struct.ScmObj* %f48028, i64 2)
%argslist54672$y480270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56399 = alloca %struct.ScmObj*, align 8
%argslist54672$y480271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y48027, %struct.ScmObj* %argslist54672$y480270)
store volatile %struct.ScmObj* %argslist54672$y480271, %struct.ScmObj** %stackaddr$prim56399, align 8
%stackaddr$prim56400 = alloca %struct.ScmObj*, align 8
%argslist54672$y480272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48470, %struct.ScmObj* %argslist54672$y480271)
store volatile %struct.ScmObj* %argslist54672$y480272, %struct.ScmObj** %stackaddr$prim56400, align 8
%clofunc56401 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y48027)
musttail call tailcc void %clofunc56401(%struct.ScmObj* %y48027, %struct.ScmObj* %argslist54672$y480272)
ret void
}

define tailcc void @proc_clo$ae48470(%struct.ScmObj* %env$ae48470,%struct.ScmObj* %current_45args54665) {
%stackaddr$env-ref56402 = alloca %struct.ScmObj*, align 8
%k48438 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48470, i64 0)
store %struct.ScmObj* %k48438, %struct.ScmObj** %stackaddr$env-ref56402
%stackaddr$env-ref56403 = alloca %struct.ScmObj*, align 8
%args48029 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48470, i64 1)
store %struct.ScmObj* %args48029, %struct.ScmObj** %stackaddr$env-ref56403
%stackaddr$env-ref56404 = alloca %struct.ScmObj*, align 8
%f48028 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48470, i64 2)
store %struct.ScmObj* %f48028, %struct.ScmObj** %stackaddr$env-ref56404
%stackaddr$prim56405 = alloca %struct.ScmObj*, align 8
%_95k48439 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54665)
store volatile %struct.ScmObj* %_95k48439, %struct.ScmObj** %stackaddr$prim56405, align 8
%stackaddr$prim56406 = alloca %struct.ScmObj*, align 8
%current_45args54666 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54665)
store volatile %struct.ScmObj* %current_45args54666, %struct.ScmObj** %stackaddr$prim56406, align 8
%stackaddr$prim56407 = alloca %struct.ScmObj*, align 8
%anf_45bind48149 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54666)
store volatile %struct.ScmObj* %anf_45bind48149, %struct.ScmObj** %stackaddr$prim56407, align 8
%stackaddr$makeclosure56408 = alloca %struct.ScmObj*, align 8
%fptrToInt56409 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48473 to i64
%ae48473 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56409)
store volatile %struct.ScmObj* %ae48473, %struct.ScmObj** %stackaddr$makeclosure56408, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48473, %struct.ScmObj* %k48438, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48473, %struct.ScmObj* %args48029, i64 1)
%argslist54671$anf_45bind481490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56410 = alloca %struct.ScmObj*, align 8
%argslist54671$anf_45bind481491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48028, %struct.ScmObj* %argslist54671$anf_45bind481490)
store volatile %struct.ScmObj* %argslist54671$anf_45bind481491, %struct.ScmObj** %stackaddr$prim56410, align 8
%stackaddr$prim56411 = alloca %struct.ScmObj*, align 8
%argslist54671$anf_45bind481492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48473, %struct.ScmObj* %argslist54671$anf_45bind481491)
store volatile %struct.ScmObj* %argslist54671$anf_45bind481492, %struct.ScmObj** %stackaddr$prim56411, align 8
%clofunc56412 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48149)
musttail call tailcc void %clofunc56412(%struct.ScmObj* %anf_45bind48149, %struct.ScmObj* %argslist54671$anf_45bind481492)
ret void
}

define tailcc void @proc_clo$ae48473(%struct.ScmObj* %env$ae48473,%struct.ScmObj* %current_45args54668) {
%stackaddr$env-ref56413 = alloca %struct.ScmObj*, align 8
%k48438 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48473, i64 0)
store %struct.ScmObj* %k48438, %struct.ScmObj** %stackaddr$env-ref56413
%stackaddr$env-ref56414 = alloca %struct.ScmObj*, align 8
%args48029 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48473, i64 1)
store %struct.ScmObj* %args48029, %struct.ScmObj** %stackaddr$env-ref56414
%stackaddr$prim56415 = alloca %struct.ScmObj*, align 8
%_95k48440 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54668)
store volatile %struct.ScmObj* %_95k48440, %struct.ScmObj** %stackaddr$prim56415, align 8
%stackaddr$prim56416 = alloca %struct.ScmObj*, align 8
%current_45args54669 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54668)
store volatile %struct.ScmObj* %current_45args54669, %struct.ScmObj** %stackaddr$prim56416, align 8
%stackaddr$prim56417 = alloca %struct.ScmObj*, align 8
%anf_45bind48150 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54669)
store volatile %struct.ScmObj* %anf_45bind48150, %struct.ScmObj** %stackaddr$prim56417, align 8
%stackaddr$prim56418 = alloca %struct.ScmObj*, align 8
%cpsargs48441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48438, %struct.ScmObj* %args48029)
store volatile %struct.ScmObj* %cpsargs48441, %struct.ScmObj** %stackaddr$prim56418, align 8
%clofunc56419 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48150)
musttail call tailcc void %clofunc56419(%struct.ScmObj* %anf_45bind48150, %struct.ScmObj* %cpsargs48441)
ret void
}

define tailcc void @proc_clo$ae48445(%struct.ScmObj* %env$ae48445,%struct.ScmObj* %current_45args54676) {
%stackaddr$prim56420 = alloca %struct.ScmObj*, align 8
%k48442 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54676)
store volatile %struct.ScmObj* %k48442, %struct.ScmObj** %stackaddr$prim56420, align 8
%stackaddr$prim56421 = alloca %struct.ScmObj*, align 8
%current_45args54677 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54676)
store volatile %struct.ScmObj* %current_45args54677, %struct.ScmObj** %stackaddr$prim56421, align 8
%stackaddr$prim56422 = alloca %struct.ScmObj*, align 8
%yu48026 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54677)
store volatile %struct.ScmObj* %yu48026, %struct.ScmObj** %stackaddr$prim56422, align 8
%argslist54679$yu480260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56423 = alloca %struct.ScmObj*, align 8
%argslist54679$yu480261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu48026, %struct.ScmObj* %argslist54679$yu480260)
store volatile %struct.ScmObj* %argslist54679$yu480261, %struct.ScmObj** %stackaddr$prim56423, align 8
%stackaddr$prim56424 = alloca %struct.ScmObj*, align 8
%argslist54679$yu480262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48442, %struct.ScmObj* %argslist54679$yu480261)
store volatile %struct.ScmObj* %argslist54679$yu480262, %struct.ScmObj** %stackaddr$prim56424, align 8
%clofunc56425 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu48026)
musttail call tailcc void %clofunc56425(%struct.ScmObj* %yu48026, %struct.ScmObj* %argslist54679$yu480262)
ret void
}