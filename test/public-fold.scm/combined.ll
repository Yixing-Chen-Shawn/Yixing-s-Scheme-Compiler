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
%mainenv53726 = call %struct.ScmObj* @const_init_null()
%mainargs53727 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv53726, %struct.ScmObj* %mainargs53727)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv53724,%struct.ScmObj* %mainargs53725) {
%stackaddr$makeclosure53728 = alloca %struct.ScmObj*, align 8
%fptrToInt53729 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47486 to i64
%ae47486 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53729)
store volatile %struct.ScmObj* %ae47486, %struct.ScmObj** %stackaddr$makeclosure53728, align 8
%ae47487 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53730 = alloca %struct.ScmObj*, align 8
%fptrToInt53731 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47488 to i64
%ae47488 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53731)
store volatile %struct.ScmObj* %ae47488, %struct.ScmObj** %stackaddr$makeclosure53730, align 8
%argslist53723$ae474860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53732 = alloca %struct.ScmObj*, align 8
%argslist53723$ae474861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47488, %struct.ScmObj* %argslist53723$ae474860)
store volatile %struct.ScmObj* %argslist53723$ae474861, %struct.ScmObj** %stackaddr$prim53732, align 8
%stackaddr$prim53733 = alloca %struct.ScmObj*, align 8
%argslist53723$ae474862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47487, %struct.ScmObj* %argslist53723$ae474861)
store volatile %struct.ScmObj* %argslist53723$ae474862, %struct.ScmObj** %stackaddr$prim53733, align 8
%clofunc53734 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47486)
musttail call tailcc void %clofunc53734(%struct.ScmObj* %ae47486, %struct.ScmObj* %argslist53723$ae474862)
ret void
}

define tailcc void @proc_clo$ae47486(%struct.ScmObj* %env$ae47486,%struct.ScmObj* %current_45args53161) {
%stackaddr$prim53735 = alloca %struct.ScmObj*, align 8
%_95k47310 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53161)
store volatile %struct.ScmObj* %_95k47310, %struct.ScmObj** %stackaddr$prim53735, align 8
%stackaddr$prim53736 = alloca %struct.ScmObj*, align 8
%current_45args53162 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53161)
store volatile %struct.ScmObj* %current_45args53162, %struct.ScmObj** %stackaddr$prim53736, align 8
%stackaddr$prim53737 = alloca %struct.ScmObj*, align 8
%anf_45bind47191 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53162)
store volatile %struct.ScmObj* %anf_45bind47191, %struct.ScmObj** %stackaddr$prim53737, align 8
%stackaddr$makeclosure53738 = alloca %struct.ScmObj*, align 8
%fptrToInt53739 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47501 to i64
%ae47501 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53739)
store volatile %struct.ScmObj* %ae47501, %struct.ScmObj** %stackaddr$makeclosure53738, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47501, %struct.ScmObj* %anf_45bind47191, i64 0)
%ae47502 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53740 = alloca %struct.ScmObj*, align 8
%fptrToInt53741 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47503 to i64
%ae47503 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53741)
store volatile %struct.ScmObj* %ae47503, %struct.ScmObj** %stackaddr$makeclosure53740, align 8
%argslist53718$ae475010 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53742 = alloca %struct.ScmObj*, align 8
%argslist53718$ae475011 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47503, %struct.ScmObj* %argslist53718$ae475010)
store volatile %struct.ScmObj* %argslist53718$ae475011, %struct.ScmObj** %stackaddr$prim53742, align 8
%stackaddr$prim53743 = alloca %struct.ScmObj*, align 8
%argslist53718$ae475012 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47502, %struct.ScmObj* %argslist53718$ae475011)
store volatile %struct.ScmObj* %argslist53718$ae475012, %struct.ScmObj** %stackaddr$prim53743, align 8
%clofunc53744 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47501)
musttail call tailcc void %clofunc53744(%struct.ScmObj* %ae47501, %struct.ScmObj* %argslist53718$ae475012)
ret void
}

define tailcc void @proc_clo$ae47501(%struct.ScmObj* %env$ae47501,%struct.ScmObj* %current_45args53164) {
%stackaddr$env-ref53745 = alloca %struct.ScmObj*, align 8
%anf_45bind47191 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47501, i64 0)
store %struct.ScmObj* %anf_45bind47191, %struct.ScmObj** %stackaddr$env-ref53745
%stackaddr$prim53746 = alloca %struct.ScmObj*, align 8
%_95k47311 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53164)
store volatile %struct.ScmObj* %_95k47311, %struct.ScmObj** %stackaddr$prim53746, align 8
%stackaddr$prim53747 = alloca %struct.ScmObj*, align 8
%current_45args53165 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53164)
store volatile %struct.ScmObj* %current_45args53165, %struct.ScmObj** %stackaddr$prim53747, align 8
%stackaddr$prim53748 = alloca %struct.ScmObj*, align 8
%anf_45bind47195 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53165)
store volatile %struct.ScmObj* %anf_45bind47195, %struct.ScmObj** %stackaddr$prim53748, align 8
%stackaddr$makeclosure53749 = alloca %struct.ScmObj*, align 8
%fptrToInt53750 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47616 to i64
%ae47616 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53750)
store volatile %struct.ScmObj* %ae47616, %struct.ScmObj** %stackaddr$makeclosure53749, align 8
%argslist53697$anf_45bind471910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53751 = alloca %struct.ScmObj*, align 8
%argslist53697$anf_45bind471911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47195, %struct.ScmObj* %argslist53697$anf_45bind471910)
store volatile %struct.ScmObj* %argslist53697$anf_45bind471911, %struct.ScmObj** %stackaddr$prim53751, align 8
%stackaddr$prim53752 = alloca %struct.ScmObj*, align 8
%argslist53697$anf_45bind471912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47616, %struct.ScmObj* %argslist53697$anf_45bind471911)
store volatile %struct.ScmObj* %argslist53697$anf_45bind471912, %struct.ScmObj** %stackaddr$prim53752, align 8
%clofunc53753 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47191)
musttail call tailcc void %clofunc53753(%struct.ScmObj* %anf_45bind47191, %struct.ScmObj* %argslist53697$anf_45bind471912)
ret void
}

define tailcc void @proc_clo$ae47616(%struct.ScmObj* %env$ae47616,%struct.ScmObj* %current_45args53167) {
%stackaddr$prim53754 = alloca %struct.ScmObj*, align 8
%_95k47312 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53167)
store volatile %struct.ScmObj* %_95k47312, %struct.ScmObj** %stackaddr$prim53754, align 8
%stackaddr$prim53755 = alloca %struct.ScmObj*, align 8
%current_45args53168 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53167)
store volatile %struct.ScmObj* %current_45args53168, %struct.ScmObj** %stackaddr$prim53755, align 8
%stackaddr$prim53756 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53168)
store volatile %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$prim53756, align 8
%stackaddr$makeclosure53757 = alloca %struct.ScmObj*, align 8
%fptrToInt53758 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47618 to i64
%ae47618 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53758)
store volatile %struct.ScmObj* %ae47618, %struct.ScmObj** %stackaddr$makeclosure53757, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47618, %struct.ScmObj* %Ycmb47068, i64 0)
%ae47619 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53759 = alloca %struct.ScmObj*, align 8
%fptrToInt53760 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47620 to i64
%ae47620 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53760)
store volatile %struct.ScmObj* %ae47620, %struct.ScmObj** %stackaddr$makeclosure53759, align 8
%argslist53696$ae476180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53761 = alloca %struct.ScmObj*, align 8
%argslist53696$ae476181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47620, %struct.ScmObj* %argslist53696$ae476180)
store volatile %struct.ScmObj* %argslist53696$ae476181, %struct.ScmObj** %stackaddr$prim53761, align 8
%stackaddr$prim53762 = alloca %struct.ScmObj*, align 8
%argslist53696$ae476182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47619, %struct.ScmObj* %argslist53696$ae476181)
store volatile %struct.ScmObj* %argslist53696$ae476182, %struct.ScmObj** %stackaddr$prim53762, align 8
%clofunc53763 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47618)
musttail call tailcc void %clofunc53763(%struct.ScmObj* %ae47618, %struct.ScmObj* %argslist53696$ae476182)
ret void
}

define tailcc void @proc_clo$ae47618(%struct.ScmObj* %env$ae47618,%struct.ScmObj* %current_45args53170) {
%stackaddr$env-ref53764 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47618, i64 0)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53764
%stackaddr$prim53765 = alloca %struct.ScmObj*, align 8
%_95k47313 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53170)
store volatile %struct.ScmObj* %_95k47313, %struct.ScmObj** %stackaddr$prim53765, align 8
%stackaddr$prim53766 = alloca %struct.ScmObj*, align 8
%current_45args53171 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53170)
store volatile %struct.ScmObj* %current_45args53171, %struct.ScmObj** %stackaddr$prim53766, align 8
%stackaddr$prim53767 = alloca %struct.ScmObj*, align 8
%anf_45bind47200 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53171)
store volatile %struct.ScmObj* %anf_45bind47200, %struct.ScmObj** %stackaddr$prim53767, align 8
%stackaddr$makeclosure53768 = alloca %struct.ScmObj*, align 8
%fptrToInt53769 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47696 to i64
%ae47696 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53769)
store volatile %struct.ScmObj* %ae47696, %struct.ScmObj** %stackaddr$makeclosure53768, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47696, %struct.ScmObj* %Ycmb47068, i64 0)
%argslist53680$Ycmb470680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53770 = alloca %struct.ScmObj*, align 8
%argslist53680$Ycmb470681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47200, %struct.ScmObj* %argslist53680$Ycmb470680)
store volatile %struct.ScmObj* %argslist53680$Ycmb470681, %struct.ScmObj** %stackaddr$prim53770, align 8
%stackaddr$prim53771 = alloca %struct.ScmObj*, align 8
%argslist53680$Ycmb470682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47696, %struct.ScmObj* %argslist53680$Ycmb470681)
store volatile %struct.ScmObj* %argslist53680$Ycmb470682, %struct.ScmObj** %stackaddr$prim53771, align 8
%clofunc53772 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47068)
musttail call tailcc void %clofunc53772(%struct.ScmObj* %Ycmb47068, %struct.ScmObj* %argslist53680$Ycmb470682)
ret void
}

define tailcc void @proc_clo$ae47696(%struct.ScmObj* %env$ae47696,%struct.ScmObj* %current_45args53173) {
%stackaddr$env-ref53773 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47696, i64 0)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53773
%stackaddr$prim53774 = alloca %struct.ScmObj*, align 8
%_95k47314 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53173)
store volatile %struct.ScmObj* %_95k47314, %struct.ScmObj** %stackaddr$prim53774, align 8
%stackaddr$prim53775 = alloca %struct.ScmObj*, align 8
%current_45args53174 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53173)
store volatile %struct.ScmObj* %current_45args53174, %struct.ScmObj** %stackaddr$prim53775, align 8
%stackaddr$prim53776 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53174)
store volatile %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$prim53776, align 8
%stackaddr$makeclosure53777 = alloca %struct.ScmObj*, align 8
%fptrToInt53778 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47698 to i64
%ae47698 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt53778)
store volatile %struct.ScmObj* %ae47698, %struct.ScmObj** %stackaddr$makeclosure53777, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47698, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47698, %struct.ScmObj* %Ycmb47068, i64 1)
%ae47699 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53779 = alloca %struct.ScmObj*, align 8
%fptrToInt53780 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47700 to i64
%ae47700 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53780)
store volatile %struct.ScmObj* %ae47700, %struct.ScmObj** %stackaddr$makeclosure53779, align 8
%argslist53679$ae476980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53781 = alloca %struct.ScmObj*, align 8
%argslist53679$ae476981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47700, %struct.ScmObj* %argslist53679$ae476980)
store volatile %struct.ScmObj* %argslist53679$ae476981, %struct.ScmObj** %stackaddr$prim53781, align 8
%stackaddr$prim53782 = alloca %struct.ScmObj*, align 8
%argslist53679$ae476982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47699, %struct.ScmObj* %argslist53679$ae476981)
store volatile %struct.ScmObj* %argslist53679$ae476982, %struct.ScmObj** %stackaddr$prim53782, align 8
%clofunc53783 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47698)
musttail call tailcc void %clofunc53783(%struct.ScmObj* %ae47698, %struct.ScmObj* %argslist53679$ae476982)
ret void
}

define tailcc void @proc_clo$ae47698(%struct.ScmObj* %env$ae47698,%struct.ScmObj* %current_45args53176) {
%stackaddr$env-ref53784 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47698, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref53784
%stackaddr$env-ref53785 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47698, i64 1)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53785
%stackaddr$prim53786 = alloca %struct.ScmObj*, align 8
%_95k47315 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53176)
store volatile %struct.ScmObj* %_95k47315, %struct.ScmObj** %stackaddr$prim53786, align 8
%stackaddr$prim53787 = alloca %struct.ScmObj*, align 8
%current_45args53177 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53176)
store volatile %struct.ScmObj* %current_45args53177, %struct.ScmObj** %stackaddr$prim53787, align 8
%stackaddr$prim53788 = alloca %struct.ScmObj*, align 8
%anf_45bind47206 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53177)
store volatile %struct.ScmObj* %anf_45bind47206, %struct.ScmObj** %stackaddr$prim53788, align 8
%stackaddr$makeclosure53789 = alloca %struct.ScmObj*, align 8
%fptrToInt53790 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47793 to i64
%ae47793 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt53790)
store volatile %struct.ScmObj* %ae47793, %struct.ScmObj** %stackaddr$makeclosure53789, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47793, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47793, %struct.ScmObj* %Ycmb47068, i64 1)
%argslist53660$Ycmb470680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53791 = alloca %struct.ScmObj*, align 8
%argslist53660$Ycmb470681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47206, %struct.ScmObj* %argslist53660$Ycmb470680)
store volatile %struct.ScmObj* %argslist53660$Ycmb470681, %struct.ScmObj** %stackaddr$prim53791, align 8
%stackaddr$prim53792 = alloca %struct.ScmObj*, align 8
%argslist53660$Ycmb470682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47793, %struct.ScmObj* %argslist53660$Ycmb470681)
store volatile %struct.ScmObj* %argslist53660$Ycmb470682, %struct.ScmObj** %stackaddr$prim53792, align 8
%clofunc53793 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47068)
musttail call tailcc void %clofunc53793(%struct.ScmObj* %Ycmb47068, %struct.ScmObj* %argslist53660$Ycmb470682)
ret void
}

define tailcc void @proc_clo$ae47793(%struct.ScmObj* %env$ae47793,%struct.ScmObj* %current_45args53179) {
%stackaddr$env-ref53794 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47793, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref53794
%stackaddr$env-ref53795 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47793, i64 1)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53795
%stackaddr$prim53796 = alloca %struct.ScmObj*, align 8
%_95k47316 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53179)
store volatile %struct.ScmObj* %_95k47316, %struct.ScmObj** %stackaddr$prim53796, align 8
%stackaddr$prim53797 = alloca %struct.ScmObj*, align 8
%current_45args53180 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53179)
store volatile %struct.ScmObj* %current_45args53180, %struct.ScmObj** %stackaddr$prim53797, align 8
%stackaddr$prim53798 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53180)
store volatile %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$prim53798, align 8
%stackaddr$makeclosure53799 = alloca %struct.ScmObj*, align 8
%fptrToInt53800 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47795 to i64
%ae47795 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt53800)
store volatile %struct.ScmObj* %ae47795, %struct.ScmObj** %stackaddr$makeclosure53799, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47795, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47795, %struct.ScmObj* %_37map147085, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47795, %struct.ScmObj* %Ycmb47068, i64 2)
%ae47796 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53801 = alloca %struct.ScmObj*, align 8
%fptrToInt53802 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47797 to i64
%ae47797 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53802)
store volatile %struct.ScmObj* %ae47797, %struct.ScmObj** %stackaddr$makeclosure53801, align 8
%argslist53659$ae477950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53803 = alloca %struct.ScmObj*, align 8
%argslist53659$ae477951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47797, %struct.ScmObj* %argslist53659$ae477950)
store volatile %struct.ScmObj* %argslist53659$ae477951, %struct.ScmObj** %stackaddr$prim53803, align 8
%stackaddr$prim53804 = alloca %struct.ScmObj*, align 8
%argslist53659$ae477952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47796, %struct.ScmObj* %argslist53659$ae477951)
store volatile %struct.ScmObj* %argslist53659$ae477952, %struct.ScmObj** %stackaddr$prim53804, align 8
%clofunc53805 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47795)
musttail call tailcc void %clofunc53805(%struct.ScmObj* %ae47795, %struct.ScmObj* %argslist53659$ae477952)
ret void
}

define tailcc void @proc_clo$ae47795(%struct.ScmObj* %env$ae47795,%struct.ScmObj* %current_45args53182) {
%stackaddr$env-ref53806 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47795, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref53806
%stackaddr$env-ref53807 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47795, i64 1)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref53807
%stackaddr$env-ref53808 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47795, i64 2)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53808
%stackaddr$prim53809 = alloca %struct.ScmObj*, align 8
%_95k47317 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53182)
store volatile %struct.ScmObj* %_95k47317, %struct.ScmObj** %stackaddr$prim53809, align 8
%stackaddr$prim53810 = alloca %struct.ScmObj*, align 8
%current_45args53183 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53182)
store volatile %struct.ScmObj* %current_45args53183, %struct.ScmObj** %stackaddr$prim53810, align 8
%stackaddr$prim53811 = alloca %struct.ScmObj*, align 8
%anf_45bind47213 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53183)
store volatile %struct.ScmObj* %anf_45bind47213, %struct.ScmObj** %stackaddr$prim53811, align 8
%stackaddr$makeclosure53812 = alloca %struct.ScmObj*, align 8
%fptrToInt53813 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47943 to i64
%ae47943 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt53813)
store volatile %struct.ScmObj* %ae47943, %struct.ScmObj** %stackaddr$makeclosure53812, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47943, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47943, %struct.ScmObj* %_37map147085, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47943, %struct.ScmObj* %Ycmb47068, i64 2)
%argslist53643$Ycmb470680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53814 = alloca %struct.ScmObj*, align 8
%argslist53643$Ycmb470681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47213, %struct.ScmObj* %argslist53643$Ycmb470680)
store volatile %struct.ScmObj* %argslist53643$Ycmb470681, %struct.ScmObj** %stackaddr$prim53814, align 8
%stackaddr$prim53815 = alloca %struct.ScmObj*, align 8
%argslist53643$Ycmb470682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47943, %struct.ScmObj* %argslist53643$Ycmb470681)
store volatile %struct.ScmObj* %argslist53643$Ycmb470682, %struct.ScmObj** %stackaddr$prim53815, align 8
%clofunc53816 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47068)
musttail call tailcc void %clofunc53816(%struct.ScmObj* %Ycmb47068, %struct.ScmObj* %argslist53643$Ycmb470682)
ret void
}

define tailcc void @proc_clo$ae47943(%struct.ScmObj* %env$ae47943,%struct.ScmObj* %current_45args53185) {
%stackaddr$env-ref53817 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47943, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref53817
%stackaddr$env-ref53818 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47943, i64 1)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref53818
%stackaddr$env-ref53819 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47943, i64 2)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53819
%stackaddr$prim53820 = alloca %struct.ScmObj*, align 8
%_95k47318 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53185)
store volatile %struct.ScmObj* %_95k47318, %struct.ScmObj** %stackaddr$prim53820, align 8
%stackaddr$prim53821 = alloca %struct.ScmObj*, align 8
%current_45args53186 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53185)
store volatile %struct.ScmObj* %current_45args53186, %struct.ScmObj** %stackaddr$prim53821, align 8
%stackaddr$prim53822 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53186)
store volatile %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$prim53822, align 8
%stackaddr$makeclosure53823 = alloca %struct.ScmObj*, align 8
%fptrToInt53824 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47945 to i64
%ae47945 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt53824)
store volatile %struct.ScmObj* %ae47945, %struct.ScmObj** %stackaddr$makeclosure53823, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47945, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47945, %struct.ScmObj* %_37map147085, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47945, %struct.ScmObj* %Ycmb47068, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae47945, %struct.ScmObj* %_37take47081, i64 3)
%ae47946 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53825 = alloca %struct.ScmObj*, align 8
%fptrToInt53826 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47947 to i64
%ae47947 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53826)
store volatile %struct.ScmObj* %ae47947, %struct.ScmObj** %stackaddr$makeclosure53825, align 8
%argslist53642$ae479450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53827 = alloca %struct.ScmObj*, align 8
%argslist53642$ae479451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47947, %struct.ScmObj* %argslist53642$ae479450)
store volatile %struct.ScmObj* %argslist53642$ae479451, %struct.ScmObj** %stackaddr$prim53827, align 8
%stackaddr$prim53828 = alloca %struct.ScmObj*, align 8
%argslist53642$ae479452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47946, %struct.ScmObj* %argslist53642$ae479451)
store volatile %struct.ScmObj* %argslist53642$ae479452, %struct.ScmObj** %stackaddr$prim53828, align 8
%clofunc53829 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47945)
musttail call tailcc void %clofunc53829(%struct.ScmObj* %ae47945, %struct.ScmObj* %argslist53642$ae479452)
ret void
}

define tailcc void @proc_clo$ae47945(%struct.ScmObj* %env$ae47945,%struct.ScmObj* %current_45args53188) {
%stackaddr$env-ref53830 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47945, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref53830
%stackaddr$env-ref53831 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47945, i64 1)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref53831
%stackaddr$env-ref53832 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47945, i64 2)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53832
%stackaddr$env-ref53833 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47945, i64 3)
store %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$env-ref53833
%stackaddr$prim53834 = alloca %struct.ScmObj*, align 8
%_95k47319 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53188)
store volatile %struct.ScmObj* %_95k47319, %struct.ScmObj** %stackaddr$prim53834, align 8
%stackaddr$prim53835 = alloca %struct.ScmObj*, align 8
%current_45args53189 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53188)
store volatile %struct.ScmObj* %current_45args53189, %struct.ScmObj** %stackaddr$prim53835, align 8
%stackaddr$prim53836 = alloca %struct.ScmObj*, align 8
%anf_45bind47217 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53189)
store volatile %struct.ScmObj* %anf_45bind47217, %struct.ScmObj** %stackaddr$prim53836, align 8
%stackaddr$makeclosure53837 = alloca %struct.ScmObj*, align 8
%fptrToInt53838 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48026 to i64
%ae48026 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt53838)
store volatile %struct.ScmObj* %ae48026, %struct.ScmObj** %stackaddr$makeclosure53837, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48026, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48026, %struct.ScmObj* %_37map147085, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48026, %struct.ScmObj* %Ycmb47068, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48026, %struct.ScmObj* %_37take47081, i64 3)
%argslist53628$Ycmb470680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53839 = alloca %struct.ScmObj*, align 8
%argslist53628$Ycmb470681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47217, %struct.ScmObj* %argslist53628$Ycmb470680)
store volatile %struct.ScmObj* %argslist53628$Ycmb470681, %struct.ScmObj** %stackaddr$prim53839, align 8
%stackaddr$prim53840 = alloca %struct.ScmObj*, align 8
%argslist53628$Ycmb470682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48026, %struct.ScmObj* %argslist53628$Ycmb470681)
store volatile %struct.ScmObj* %argslist53628$Ycmb470682, %struct.ScmObj** %stackaddr$prim53840, align 8
%clofunc53841 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47068)
musttail call tailcc void %clofunc53841(%struct.ScmObj* %Ycmb47068, %struct.ScmObj* %argslist53628$Ycmb470682)
ret void
}

define tailcc void @proc_clo$ae48026(%struct.ScmObj* %env$ae48026,%struct.ScmObj* %current_45args53191) {
%stackaddr$env-ref53842 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48026, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref53842
%stackaddr$env-ref53843 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48026, i64 1)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref53843
%stackaddr$env-ref53844 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48026, i64 2)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53844
%stackaddr$env-ref53845 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48026, i64 3)
store %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$env-ref53845
%stackaddr$prim53846 = alloca %struct.ScmObj*, align 8
%_95k47320 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53191)
store volatile %struct.ScmObj* %_95k47320, %struct.ScmObj** %stackaddr$prim53846, align 8
%stackaddr$prim53847 = alloca %struct.ScmObj*, align 8
%current_45args53192 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53191)
store volatile %struct.ScmObj* %current_45args53192, %struct.ScmObj** %stackaddr$prim53847, align 8
%stackaddr$prim53848 = alloca %struct.ScmObj*, align 8
%_37length47078 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53192)
store volatile %struct.ScmObj* %_37length47078, %struct.ScmObj** %stackaddr$prim53848, align 8
%stackaddr$makeclosure53849 = alloca %struct.ScmObj*, align 8
%fptrToInt53850 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48028 to i64
%ae48028 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt53850)
store volatile %struct.ScmObj* %ae48028, %struct.ScmObj** %stackaddr$makeclosure53849, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48028, %struct.ScmObj* %_37length47078, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48028, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48028, %struct.ScmObj* %_37map147085, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48028, %struct.ScmObj* %Ycmb47068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48028, %struct.ScmObj* %_37take47081, i64 4)
%ae48029 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53851 = alloca %struct.ScmObj*, align 8
%fptrToInt53852 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48030 to i64
%ae48030 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53852)
store volatile %struct.ScmObj* %ae48030, %struct.ScmObj** %stackaddr$makeclosure53851, align 8
%argslist53627$ae480280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53853 = alloca %struct.ScmObj*, align 8
%argslist53627$ae480281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48030, %struct.ScmObj* %argslist53627$ae480280)
store volatile %struct.ScmObj* %argslist53627$ae480281, %struct.ScmObj** %stackaddr$prim53853, align 8
%stackaddr$prim53854 = alloca %struct.ScmObj*, align 8
%argslist53627$ae480282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48029, %struct.ScmObj* %argslist53627$ae480281)
store volatile %struct.ScmObj* %argslist53627$ae480282, %struct.ScmObj** %stackaddr$prim53854, align 8
%clofunc53855 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48028)
musttail call tailcc void %clofunc53855(%struct.ScmObj* %ae48028, %struct.ScmObj* %argslist53627$ae480282)
ret void
}

define tailcc void @proc_clo$ae48028(%struct.ScmObj* %env$ae48028,%struct.ScmObj* %current_45args53194) {
%stackaddr$env-ref53856 = alloca %struct.ScmObj*, align 8
%_37length47078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48028, i64 0)
store %struct.ScmObj* %_37length47078, %struct.ScmObj** %stackaddr$env-ref53856
%stackaddr$env-ref53857 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48028, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref53857
%stackaddr$env-ref53858 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48028, i64 2)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref53858
%stackaddr$env-ref53859 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48028, i64 3)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53859
%stackaddr$env-ref53860 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48028, i64 4)
store %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$env-ref53860
%stackaddr$prim53861 = alloca %struct.ScmObj*, align 8
%_95k47321 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53194)
store volatile %struct.ScmObj* %_95k47321, %struct.ScmObj** %stackaddr$prim53861, align 8
%stackaddr$prim53862 = alloca %struct.ScmObj*, align 8
%current_45args53195 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53194)
store volatile %struct.ScmObj* %current_45args53195, %struct.ScmObj** %stackaddr$prim53862, align 8
%stackaddr$prim53863 = alloca %struct.ScmObj*, align 8
%anf_45bind47222 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53195)
store volatile %struct.ScmObj* %anf_45bind47222, %struct.ScmObj** %stackaddr$prim53863, align 8
%stackaddr$makeclosure53864 = alloca %struct.ScmObj*, align 8
%fptrToInt53865 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48105 to i64
%ae48105 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt53865)
store volatile %struct.ScmObj* %ae48105, %struct.ScmObj** %stackaddr$makeclosure53864, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48105, %struct.ScmObj* %_37length47078, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48105, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48105, %struct.ScmObj* %_37map147085, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48105, %struct.ScmObj* %Ycmb47068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48105, %struct.ScmObj* %_37take47081, i64 4)
%argslist53611$Ycmb470680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53866 = alloca %struct.ScmObj*, align 8
%argslist53611$Ycmb470681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47222, %struct.ScmObj* %argslist53611$Ycmb470680)
store volatile %struct.ScmObj* %argslist53611$Ycmb470681, %struct.ScmObj** %stackaddr$prim53866, align 8
%stackaddr$prim53867 = alloca %struct.ScmObj*, align 8
%argslist53611$Ycmb470682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48105, %struct.ScmObj* %argslist53611$Ycmb470681)
store volatile %struct.ScmObj* %argslist53611$Ycmb470682, %struct.ScmObj** %stackaddr$prim53867, align 8
%clofunc53868 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47068)
musttail call tailcc void %clofunc53868(%struct.ScmObj* %Ycmb47068, %struct.ScmObj* %argslist53611$Ycmb470682)
ret void
}

define tailcc void @proc_clo$ae48105(%struct.ScmObj* %env$ae48105,%struct.ScmObj* %current_45args53197) {
%stackaddr$env-ref53869 = alloca %struct.ScmObj*, align 8
%_37length47078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48105, i64 0)
store %struct.ScmObj* %_37length47078, %struct.ScmObj** %stackaddr$env-ref53869
%stackaddr$env-ref53870 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48105, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref53870
%stackaddr$env-ref53871 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48105, i64 2)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref53871
%stackaddr$env-ref53872 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48105, i64 3)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53872
%stackaddr$env-ref53873 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48105, i64 4)
store %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$env-ref53873
%stackaddr$prim53874 = alloca %struct.ScmObj*, align 8
%_95k47322 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53197)
store volatile %struct.ScmObj* %_95k47322, %struct.ScmObj** %stackaddr$prim53874, align 8
%stackaddr$prim53875 = alloca %struct.ScmObj*, align 8
%current_45args53198 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53197)
store volatile %struct.ScmObj* %current_45args53198, %struct.ScmObj** %stackaddr$prim53875, align 8
%stackaddr$prim53876 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53198)
store volatile %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$prim53876, align 8
%stackaddr$makeclosure53877 = alloca %struct.ScmObj*, align 8
%fptrToInt53878 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48107 to i64
%ae48107 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt53878)
store volatile %struct.ScmObj* %ae48107, %struct.ScmObj** %stackaddr$makeclosure53877, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48107, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48107, %struct.ScmObj* %_37foldl147073, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48107, %struct.ScmObj* %_37length47078, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48107, %struct.ScmObj* %_37map147085, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48107, %struct.ScmObj* %Ycmb47068, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48107, %struct.ScmObj* %_37take47081, i64 5)
%ae48108 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53879 = alloca %struct.ScmObj*, align 8
%fptrToInt53880 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48109 to i64
%ae48109 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53880)
store volatile %struct.ScmObj* %ae48109, %struct.ScmObj** %stackaddr$makeclosure53879, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48109, %struct.ScmObj* %_37foldl147073, i64 0)
%argslist53610$ae481070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53881 = alloca %struct.ScmObj*, align 8
%argslist53610$ae481071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48109, %struct.ScmObj* %argslist53610$ae481070)
store volatile %struct.ScmObj* %argslist53610$ae481071, %struct.ScmObj** %stackaddr$prim53881, align 8
%stackaddr$prim53882 = alloca %struct.ScmObj*, align 8
%argslist53610$ae481072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48108, %struct.ScmObj* %argslist53610$ae481071)
store volatile %struct.ScmObj* %argslist53610$ae481072, %struct.ScmObj** %stackaddr$prim53882, align 8
%clofunc53883 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48107)
musttail call tailcc void %clofunc53883(%struct.ScmObj* %ae48107, %struct.ScmObj* %argslist53610$ae481072)
ret void
}

define tailcc void @proc_clo$ae48107(%struct.ScmObj* %env$ae48107,%struct.ScmObj* %current_45args53200) {
%stackaddr$env-ref53884 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48107, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref53884
%stackaddr$env-ref53885 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48107, i64 1)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref53885
%stackaddr$env-ref53886 = alloca %struct.ScmObj*, align 8
%_37length47078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48107, i64 2)
store %struct.ScmObj* %_37length47078, %struct.ScmObj** %stackaddr$env-ref53886
%stackaddr$env-ref53887 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48107, i64 3)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref53887
%stackaddr$env-ref53888 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48107, i64 4)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53888
%stackaddr$env-ref53889 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48107, i64 5)
store %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$env-ref53889
%stackaddr$prim53890 = alloca %struct.ScmObj*, align 8
%_95k47323 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53200)
store volatile %struct.ScmObj* %_95k47323, %struct.ScmObj** %stackaddr$prim53890, align 8
%stackaddr$prim53891 = alloca %struct.ScmObj*, align 8
%current_45args53201 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53200)
store volatile %struct.ScmObj* %current_45args53201, %struct.ScmObj** %stackaddr$prim53891, align 8
%stackaddr$prim53892 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53201)
store volatile %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$prim53892, align 8
%stackaddr$makeclosure53893 = alloca %struct.ScmObj*, align 8
%fptrToInt53894 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48161 to i64
%ae48161 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt53894)
store volatile %struct.ScmObj* %ae48161, %struct.ScmObj** %stackaddr$makeclosure53893, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48161, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48161, %struct.ScmObj* %_37foldl147073, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48161, %struct.ScmObj* %_37map147085, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48161, %struct.ScmObj* %Ycmb47068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48161, %struct.ScmObj* %_37last47111, i64 4)
%ae48162 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53895 = alloca %struct.ScmObj*, align 8
%fptrToInt53896 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48163 to i64
%ae48163 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt53896)
store volatile %struct.ScmObj* %ae48163, %struct.ScmObj** %stackaddr$makeclosure53895, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48163, %struct.ScmObj* %_37length47078, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48163, %struct.ScmObj* %_37take47081, i64 1)
%argslist53596$ae481610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53897 = alloca %struct.ScmObj*, align 8
%argslist53596$ae481611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48163, %struct.ScmObj* %argslist53596$ae481610)
store volatile %struct.ScmObj* %argslist53596$ae481611, %struct.ScmObj** %stackaddr$prim53897, align 8
%stackaddr$prim53898 = alloca %struct.ScmObj*, align 8
%argslist53596$ae481612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48162, %struct.ScmObj* %argslist53596$ae481611)
store volatile %struct.ScmObj* %argslist53596$ae481612, %struct.ScmObj** %stackaddr$prim53898, align 8
%clofunc53899 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48161)
musttail call tailcc void %clofunc53899(%struct.ScmObj* %ae48161, %struct.ScmObj* %argslist53596$ae481612)
ret void
}

define tailcc void @proc_clo$ae48161(%struct.ScmObj* %env$ae48161,%struct.ScmObj* %current_45args53203) {
%stackaddr$env-ref53900 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48161, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref53900
%stackaddr$env-ref53901 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48161, i64 1)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref53901
%stackaddr$env-ref53902 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48161, i64 2)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref53902
%stackaddr$env-ref53903 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48161, i64 3)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53903
%stackaddr$env-ref53904 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48161, i64 4)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref53904
%stackaddr$prim53905 = alloca %struct.ScmObj*, align 8
%_95k47324 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53203)
store volatile %struct.ScmObj* %_95k47324, %struct.ScmObj** %stackaddr$prim53905, align 8
%stackaddr$prim53906 = alloca %struct.ScmObj*, align 8
%current_45args53204 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53203)
store volatile %struct.ScmObj* %current_45args53204, %struct.ScmObj** %stackaddr$prim53906, align 8
%stackaddr$prim53907 = alloca %struct.ScmObj*, align 8
%_37drop_45right47108 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53204)
store volatile %struct.ScmObj* %_37drop_45right47108, %struct.ScmObj** %stackaddr$prim53907, align 8
%stackaddr$makeclosure53908 = alloca %struct.ScmObj*, align 8
%fptrToInt53909 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48191 to i64
%ae48191 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt53909)
store volatile %struct.ScmObj* %ae48191, %struct.ScmObj** %stackaddr$makeclosure53908, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48191, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48191, %struct.ScmObj* %_37foldl147073, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48191, %struct.ScmObj* %_37drop_45right47108, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48191, %struct.ScmObj* %Ycmb47068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48191, %struct.ScmObj* %_37last47111, i64 4)
%ae48192 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53910 = alloca %struct.ScmObj*, align 8
%fptrToInt53911 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48193 to i64
%ae48193 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt53911)
store volatile %struct.ScmObj* %ae48193, %struct.ScmObj** %stackaddr$makeclosure53910, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48193, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48193, %struct.ScmObj* %_37map147085, i64 1)
%argslist53586$ae481910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53912 = alloca %struct.ScmObj*, align 8
%argslist53586$ae481911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48193, %struct.ScmObj* %argslist53586$ae481910)
store volatile %struct.ScmObj* %argslist53586$ae481911, %struct.ScmObj** %stackaddr$prim53912, align 8
%stackaddr$prim53913 = alloca %struct.ScmObj*, align 8
%argslist53586$ae481912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48192, %struct.ScmObj* %argslist53586$ae481911)
store volatile %struct.ScmObj* %argslist53586$ae481912, %struct.ScmObj** %stackaddr$prim53913, align 8
%clofunc53914 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48191)
musttail call tailcc void %clofunc53914(%struct.ScmObj* %ae48191, %struct.ScmObj* %argslist53586$ae481912)
ret void
}

define tailcc void @proc_clo$ae48191(%struct.ScmObj* %env$ae48191,%struct.ScmObj* %current_45args53206) {
%stackaddr$env-ref53915 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48191, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref53915
%stackaddr$env-ref53916 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48191, i64 1)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref53916
%stackaddr$env-ref53917 = alloca %struct.ScmObj*, align 8
%_37drop_45right47108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48191, i64 2)
store %struct.ScmObj* %_37drop_45right47108, %struct.ScmObj** %stackaddr$env-ref53917
%stackaddr$env-ref53918 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48191, i64 3)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53918
%stackaddr$env-ref53919 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48191, i64 4)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref53919
%stackaddr$prim53920 = alloca %struct.ScmObj*, align 8
%_95k47325 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53206)
store volatile %struct.ScmObj* %_95k47325, %struct.ScmObj** %stackaddr$prim53920, align 8
%stackaddr$prim53921 = alloca %struct.ScmObj*, align 8
%current_45args53207 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53206)
store volatile %struct.ScmObj* %current_45args53207, %struct.ScmObj** %stackaddr$prim53921, align 8
%stackaddr$prim53922 = alloca %struct.ScmObj*, align 8
%anf_45bind47238 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53207)
store volatile %struct.ScmObj* %anf_45bind47238, %struct.ScmObj** %stackaddr$prim53922, align 8
%stackaddr$makeclosure53923 = alloca %struct.ScmObj*, align 8
%fptrToInt53924 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48575 to i64
%ae48575 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt53924)
store volatile %struct.ScmObj* %ae48575, %struct.ScmObj** %stackaddr$makeclosure53923, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48575, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48575, %struct.ScmObj* %_37foldl147073, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48575, %struct.ScmObj* %_37drop_45right47108, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48575, %struct.ScmObj* %Ycmb47068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48575, %struct.ScmObj* %_37last47111, i64 4)
%argslist53526$Ycmb470680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53925 = alloca %struct.ScmObj*, align 8
%argslist53526$Ycmb470681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47238, %struct.ScmObj* %argslist53526$Ycmb470680)
store volatile %struct.ScmObj* %argslist53526$Ycmb470681, %struct.ScmObj** %stackaddr$prim53925, align 8
%stackaddr$prim53926 = alloca %struct.ScmObj*, align 8
%argslist53526$Ycmb470682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48575, %struct.ScmObj* %argslist53526$Ycmb470681)
store volatile %struct.ScmObj* %argslist53526$Ycmb470682, %struct.ScmObj** %stackaddr$prim53926, align 8
%clofunc53927 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47068)
musttail call tailcc void %clofunc53927(%struct.ScmObj* %Ycmb47068, %struct.ScmObj* %argslist53526$Ycmb470682)
ret void
}

define tailcc void @proc_clo$ae48575(%struct.ScmObj* %env$ae48575,%struct.ScmObj* %current_45args53209) {
%stackaddr$env-ref53928 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48575, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref53928
%stackaddr$env-ref53929 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48575, i64 1)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref53929
%stackaddr$env-ref53930 = alloca %struct.ScmObj*, align 8
%_37drop_45right47108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48575, i64 2)
store %struct.ScmObj* %_37drop_45right47108, %struct.ScmObj** %stackaddr$env-ref53930
%stackaddr$env-ref53931 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48575, i64 3)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53931
%stackaddr$env-ref53932 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48575, i64 4)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref53932
%stackaddr$prim53933 = alloca %struct.ScmObj*, align 8
%_95k47326 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53209)
store volatile %struct.ScmObj* %_95k47326, %struct.ScmObj** %stackaddr$prim53933, align 8
%stackaddr$prim53934 = alloca %struct.ScmObj*, align 8
%current_45args53210 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53209)
store volatile %struct.ScmObj* %current_45args53210, %struct.ScmObj** %stackaddr$prim53934, align 8
%stackaddr$prim53935 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53210)
store volatile %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$prim53935, align 8
%stackaddr$makeclosure53936 = alloca %struct.ScmObj*, align 8
%fptrToInt53937 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48577 to i64
%ae48577 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt53937)
store volatile %struct.ScmObj* %ae48577, %struct.ScmObj** %stackaddr$makeclosure53936, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48577, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48577, %struct.ScmObj* %_37foldl147073, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48577, %struct.ScmObj* %_37foldr47094, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48577, %struct.ScmObj* %_37drop_45right47108, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48577, %struct.ScmObj* %Ycmb47068, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48577, %struct.ScmObj* %_37last47111, i64 5)
%ae48578 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53938 = alloca %struct.ScmObj*, align 8
%fptrToInt53939 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48579 to i64
%ae48579 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53939)
store volatile %struct.ScmObj* %ae48579, %struct.ScmObj** %stackaddr$makeclosure53938, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48579, %struct.ScmObj* %_37foldr147089, i64 0)
%argslist53525$ae485770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53940 = alloca %struct.ScmObj*, align 8
%argslist53525$ae485771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48579, %struct.ScmObj* %argslist53525$ae485770)
store volatile %struct.ScmObj* %argslist53525$ae485771, %struct.ScmObj** %stackaddr$prim53940, align 8
%stackaddr$prim53941 = alloca %struct.ScmObj*, align 8
%argslist53525$ae485772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48578, %struct.ScmObj* %argslist53525$ae485771)
store volatile %struct.ScmObj* %argslist53525$ae485772, %struct.ScmObj** %stackaddr$prim53941, align 8
%clofunc53942 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48577)
musttail call tailcc void %clofunc53942(%struct.ScmObj* %ae48577, %struct.ScmObj* %argslist53525$ae485772)
ret void
}

define tailcc void @proc_clo$ae48577(%struct.ScmObj* %env$ae48577,%struct.ScmObj* %current_45args53212) {
%stackaddr$env-ref53943 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48577, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref53943
%stackaddr$env-ref53944 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48577, i64 1)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref53944
%stackaddr$env-ref53945 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48577, i64 2)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref53945
%stackaddr$env-ref53946 = alloca %struct.ScmObj*, align 8
%_37drop_45right47108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48577, i64 3)
store %struct.ScmObj* %_37drop_45right47108, %struct.ScmObj** %stackaddr$env-ref53946
%stackaddr$env-ref53947 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48577, i64 4)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53947
%stackaddr$env-ref53948 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48577, i64 5)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref53948
%stackaddr$prim53949 = alloca %struct.ScmObj*, align 8
%_95k47327 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53212)
store volatile %struct.ScmObj* %_95k47327, %struct.ScmObj** %stackaddr$prim53949, align 8
%stackaddr$prim53950 = alloca %struct.ScmObj*, align 8
%current_45args53213 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53212)
store volatile %struct.ScmObj* %current_45args53213, %struct.ScmObj** %stackaddr$prim53950, align 8
%stackaddr$prim53951 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53213)
store volatile %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$prim53951, align 8
%stackaddr$makeclosure53952 = alloca %struct.ScmObj*, align 8
%fptrToInt53953 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48654 to i64
%ae48654 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt53953)
store volatile %struct.ScmObj* %ae48654, %struct.ScmObj** %stackaddr$makeclosure53952, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48654, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48654, %struct.ScmObj* %_37foldl147073, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48654, %struct.ScmObj* %_37foldr47094, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48654, %struct.ScmObj* %_37map147120, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48654, %struct.ScmObj* %Ycmb47068, i64 4)
%ae48655 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53954 = alloca %struct.ScmObj*, align 8
%fptrToInt53955 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48656 to i64
%ae48656 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt53955)
store volatile %struct.ScmObj* %ae48656, %struct.ScmObj** %stackaddr$makeclosure53954, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48656, %struct.ScmObj* %_37foldr47094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48656, %struct.ScmObj* %_37drop_45right47108, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48656, %struct.ScmObj* %_37last47111, i64 2)
%argslist53506$ae486540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53956 = alloca %struct.ScmObj*, align 8
%argslist53506$ae486541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48656, %struct.ScmObj* %argslist53506$ae486540)
store volatile %struct.ScmObj* %argslist53506$ae486541, %struct.ScmObj** %stackaddr$prim53956, align 8
%stackaddr$prim53957 = alloca %struct.ScmObj*, align 8
%argslist53506$ae486542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48655, %struct.ScmObj* %argslist53506$ae486541)
store volatile %struct.ScmObj* %argslist53506$ae486542, %struct.ScmObj** %stackaddr$prim53957, align 8
%clofunc53958 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48654)
musttail call tailcc void %clofunc53958(%struct.ScmObj* %ae48654, %struct.ScmObj* %argslist53506$ae486542)
ret void
}

define tailcc void @proc_clo$ae48654(%struct.ScmObj* %env$ae48654,%struct.ScmObj* %current_45args53215) {
%stackaddr$env-ref53959 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48654, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref53959
%stackaddr$env-ref53960 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48654, i64 1)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref53960
%stackaddr$env-ref53961 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48654, i64 2)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref53961
%stackaddr$env-ref53962 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48654, i64 3)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref53962
%stackaddr$env-ref53963 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48654, i64 4)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53963
%stackaddr$prim53964 = alloca %struct.ScmObj*, align 8
%_95k47328 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53215)
store volatile %struct.ScmObj* %_95k47328, %struct.ScmObj** %stackaddr$prim53964, align 8
%stackaddr$prim53965 = alloca %struct.ScmObj*, align 8
%current_45args53216 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53215)
store volatile %struct.ScmObj* %current_45args53216, %struct.ScmObj** %stackaddr$prim53965, align 8
%stackaddr$prim53966 = alloca %struct.ScmObj*, align 8
%_37map47115 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53216)
store volatile %struct.ScmObj* %_37map47115, %struct.ScmObj** %stackaddr$prim53966, align 8
%stackaddr$makeclosure53967 = alloca %struct.ScmObj*, align 8
%fptrToInt53968 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48800 to i64
%ae48800 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt53968)
store volatile %struct.ScmObj* %ae48800, %struct.ScmObj** %stackaddr$makeclosure53967, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48800, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48800, %struct.ScmObj* %Ycmb47068, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48800, %struct.ScmObj* %_37map47115, i64 2)
%ae48801 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53969 = alloca %struct.ScmObj*, align 8
%fptrToInt53970 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48802 to i64
%ae48802 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt53970)
store volatile %struct.ScmObj* %ae48802, %struct.ScmObj** %stackaddr$makeclosure53969, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48802, %struct.ScmObj* %_37foldr47094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48802, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48802, %struct.ScmObj* %_37map147120, i64 2)
%argslist53489$ae488000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53971 = alloca %struct.ScmObj*, align 8
%argslist53489$ae488001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48802, %struct.ScmObj* %argslist53489$ae488000)
store volatile %struct.ScmObj* %argslist53489$ae488001, %struct.ScmObj** %stackaddr$prim53971, align 8
%stackaddr$prim53972 = alloca %struct.ScmObj*, align 8
%argslist53489$ae488002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48801, %struct.ScmObj* %argslist53489$ae488001)
store volatile %struct.ScmObj* %argslist53489$ae488002, %struct.ScmObj** %stackaddr$prim53972, align 8
%clofunc53973 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48800)
musttail call tailcc void %clofunc53973(%struct.ScmObj* %ae48800, %struct.ScmObj* %argslist53489$ae488002)
ret void
}

define tailcc void @proc_clo$ae48800(%struct.ScmObj* %env$ae48800,%struct.ScmObj* %current_45args53218) {
%stackaddr$env-ref53974 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48800, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref53974
%stackaddr$env-ref53975 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48800, i64 1)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53975
%stackaddr$env-ref53976 = alloca %struct.ScmObj*, align 8
%_37map47115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48800, i64 2)
store %struct.ScmObj* %_37map47115, %struct.ScmObj** %stackaddr$env-ref53976
%stackaddr$prim53977 = alloca %struct.ScmObj*, align 8
%_95k47329 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53218)
store volatile %struct.ScmObj* %_95k47329, %struct.ScmObj** %stackaddr$prim53977, align 8
%stackaddr$prim53978 = alloca %struct.ScmObj*, align 8
%current_45args53219 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53218)
store volatile %struct.ScmObj* %current_45args53219, %struct.ScmObj** %stackaddr$prim53978, align 8
%stackaddr$prim53979 = alloca %struct.ScmObj*, align 8
%anf_45bind47258 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53219)
store volatile %struct.ScmObj* %anf_45bind47258, %struct.ScmObj** %stackaddr$prim53979, align 8
%stackaddr$makeclosure53980 = alloca %struct.ScmObj*, align 8
%fptrToInt53981 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49192 to i64
%ae49192 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt53981)
store volatile %struct.ScmObj* %ae49192, %struct.ScmObj** %stackaddr$makeclosure53980, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49192, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49192, %struct.ScmObj* %_37map47115, i64 1)
%argslist53429$Ycmb470680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53982 = alloca %struct.ScmObj*, align 8
%argslist53429$Ycmb470681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47258, %struct.ScmObj* %argslist53429$Ycmb470680)
store volatile %struct.ScmObj* %argslist53429$Ycmb470681, %struct.ScmObj** %stackaddr$prim53982, align 8
%stackaddr$prim53983 = alloca %struct.ScmObj*, align 8
%argslist53429$Ycmb470682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49192, %struct.ScmObj* %argslist53429$Ycmb470681)
store volatile %struct.ScmObj* %argslist53429$Ycmb470682, %struct.ScmObj** %stackaddr$prim53983, align 8
%clofunc53984 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47068)
musttail call tailcc void %clofunc53984(%struct.ScmObj* %Ycmb47068, %struct.ScmObj* %argslist53429$Ycmb470682)
ret void
}

define tailcc void @proc_clo$ae49192(%struct.ScmObj* %env$ae49192,%struct.ScmObj* %current_45args53221) {
%stackaddr$env-ref53985 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49192, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref53985
%stackaddr$env-ref53986 = alloca %struct.ScmObj*, align 8
%_37map47115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49192, i64 1)
store %struct.ScmObj* %_37map47115, %struct.ScmObj** %stackaddr$env-ref53986
%stackaddr$prim53987 = alloca %struct.ScmObj*, align 8
%_95k47330 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53221)
store volatile %struct.ScmObj* %_95k47330, %struct.ScmObj** %stackaddr$prim53987, align 8
%stackaddr$prim53988 = alloca %struct.ScmObj*, align 8
%current_45args53222 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53221)
store volatile %struct.ScmObj* %current_45args53222, %struct.ScmObj** %stackaddr$prim53988, align 8
%stackaddr$prim53989 = alloca %struct.ScmObj*, align 8
%_37foldl47171 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53222)
store volatile %struct.ScmObj* %_37foldl47171, %struct.ScmObj** %stackaddr$prim53989, align 8
%stackaddr$makeclosure53990 = alloca %struct.ScmObj*, align 8
%fptrToInt53991 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49194 to i64
%ae49194 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt53991)
store volatile %struct.ScmObj* %ae49194, %struct.ScmObj** %stackaddr$makeclosure53990, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49194, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49194, %struct.ScmObj* %_37map47115, i64 1)
%ae49195 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53992 = alloca %struct.ScmObj*, align 8
%fptrToInt53993 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49196 to i64
%ae49196 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53993)
store volatile %struct.ScmObj* %ae49196, %struct.ScmObj** %stackaddr$makeclosure53992, align 8
%argslist53428$ae491940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53994 = alloca %struct.ScmObj*, align 8
%argslist53428$ae491941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49196, %struct.ScmObj* %argslist53428$ae491940)
store volatile %struct.ScmObj* %argslist53428$ae491941, %struct.ScmObj** %stackaddr$prim53994, align 8
%stackaddr$prim53995 = alloca %struct.ScmObj*, align 8
%argslist53428$ae491942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49195, %struct.ScmObj* %argslist53428$ae491941)
store volatile %struct.ScmObj* %argslist53428$ae491942, %struct.ScmObj** %stackaddr$prim53995, align 8
%clofunc53996 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49194)
musttail call tailcc void %clofunc53996(%struct.ScmObj* %ae49194, %struct.ScmObj* %argslist53428$ae491942)
ret void
}

define tailcc void @proc_clo$ae49194(%struct.ScmObj* %env$ae49194,%struct.ScmObj* %current_45args53224) {
%stackaddr$env-ref53997 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49194, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref53997
%stackaddr$env-ref53998 = alloca %struct.ScmObj*, align 8
%_37map47115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49194, i64 1)
store %struct.ScmObj* %_37map47115, %struct.ScmObj** %stackaddr$env-ref53998
%stackaddr$prim53999 = alloca %struct.ScmObj*, align 8
%_95k47331 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53224)
store volatile %struct.ScmObj* %_95k47331, %struct.ScmObj** %stackaddr$prim53999, align 8
%stackaddr$prim54000 = alloca %struct.ScmObj*, align 8
%current_45args53225 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53224)
store volatile %struct.ScmObj* %current_45args53225, %struct.ScmObj** %stackaddr$prim54000, align 8
%stackaddr$prim54001 = alloca %struct.ScmObj*, align 8
%_37_6247168 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53225)
store volatile %struct.ScmObj* %_37_6247168, %struct.ScmObj** %stackaddr$prim54001, align 8
%stackaddr$makeclosure54002 = alloca %struct.ScmObj*, align 8
%fptrToInt54003 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49218 to i64
%ae49218 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54003)
store volatile %struct.ScmObj* %ae49218, %struct.ScmObj** %stackaddr$makeclosure54002, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49218, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49218, %struct.ScmObj* %_37map47115, i64 1)
%ae49219 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54004 = alloca %struct.ScmObj*, align 8
%fptrToInt54005 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49220 to i64
%ae49220 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54005)
store volatile %struct.ScmObj* %ae49220, %struct.ScmObj** %stackaddr$makeclosure54004, align 8
%argslist53422$ae492180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54006 = alloca %struct.ScmObj*, align 8
%argslist53422$ae492181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49220, %struct.ScmObj* %argslist53422$ae492180)
store volatile %struct.ScmObj* %argslist53422$ae492181, %struct.ScmObj** %stackaddr$prim54006, align 8
%stackaddr$prim54007 = alloca %struct.ScmObj*, align 8
%argslist53422$ae492182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49219, %struct.ScmObj* %argslist53422$ae492181)
store volatile %struct.ScmObj* %argslist53422$ae492182, %struct.ScmObj** %stackaddr$prim54007, align 8
%clofunc54008 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49218)
musttail call tailcc void %clofunc54008(%struct.ScmObj* %ae49218, %struct.ScmObj* %argslist53422$ae492182)
ret void
}

define tailcc void @proc_clo$ae49218(%struct.ScmObj* %env$ae49218,%struct.ScmObj* %current_45args53227) {
%stackaddr$env-ref54009 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49218, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54009
%stackaddr$env-ref54010 = alloca %struct.ScmObj*, align 8
%_37map47115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49218, i64 1)
store %struct.ScmObj* %_37map47115, %struct.ScmObj** %stackaddr$env-ref54010
%stackaddr$prim54011 = alloca %struct.ScmObj*, align 8
%_95k47332 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53227)
store volatile %struct.ScmObj* %_95k47332, %struct.ScmObj** %stackaddr$prim54011, align 8
%stackaddr$prim54012 = alloca %struct.ScmObj*, align 8
%current_45args53228 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53227)
store volatile %struct.ScmObj* %current_45args53228, %struct.ScmObj** %stackaddr$prim54012, align 8
%stackaddr$prim54013 = alloca %struct.ScmObj*, align 8
%_37_62_6147165 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53228)
store volatile %struct.ScmObj* %_37_62_6147165, %struct.ScmObj** %stackaddr$prim54013, align 8
%ae49242 = call %struct.ScmObj* @const_init_int(i64 1)
%ae49243 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54014 = alloca %struct.ScmObj*, align 8
%_37append47161 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49242, %struct.ScmObj* %ae49243)
store volatile %struct.ScmObj* %_37append47161, %struct.ScmObj** %stackaddr$prim54014, align 8
%stackaddr$makeclosure54015 = alloca %struct.ScmObj*, align 8
%fptrToInt54016 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49244 to i64
%ae49244 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54016)
store volatile %struct.ScmObj* %ae49244, %struct.ScmObj** %stackaddr$makeclosure54015, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49244, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49244, %struct.ScmObj* %_37map47115, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49244, %struct.ScmObj* %_37append47161, i64 2)
%ae49245 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54017 = alloca %struct.ScmObj*, align 8
%fptrToInt54018 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49246 to i64
%ae49246 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54018)
store volatile %struct.ScmObj* %ae49246, %struct.ScmObj** %stackaddr$makeclosure54017, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49246, %struct.ScmObj* %_37append47161, i64 0)
%argslist53416$ae492440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54019 = alloca %struct.ScmObj*, align 8
%argslist53416$ae492441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49246, %struct.ScmObj* %argslist53416$ae492440)
store volatile %struct.ScmObj* %argslist53416$ae492441, %struct.ScmObj** %stackaddr$prim54019, align 8
%stackaddr$prim54020 = alloca %struct.ScmObj*, align 8
%argslist53416$ae492442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49245, %struct.ScmObj* %argslist53416$ae492441)
store volatile %struct.ScmObj* %argslist53416$ae492442, %struct.ScmObj** %stackaddr$prim54020, align 8
%clofunc54021 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49244)
musttail call tailcc void %clofunc54021(%struct.ScmObj* %ae49244, %struct.ScmObj* %argslist53416$ae492442)
ret void
}

define tailcc void @proc_clo$ae49244(%struct.ScmObj* %env$ae49244,%struct.ScmObj* %current_45args53230) {
%stackaddr$env-ref54022 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49244, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54022
%stackaddr$env-ref54023 = alloca %struct.ScmObj*, align 8
%_37map47115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49244, i64 1)
store %struct.ScmObj* %_37map47115, %struct.ScmObj** %stackaddr$env-ref54023
%stackaddr$env-ref54024 = alloca %struct.ScmObj*, align 8
%_37append47161 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49244, i64 2)
store %struct.ScmObj* %_37append47161, %struct.ScmObj** %stackaddr$env-ref54024
%stackaddr$prim54025 = alloca %struct.ScmObj*, align 8
%_95k47333 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53230)
store volatile %struct.ScmObj* %_95k47333, %struct.ScmObj** %stackaddr$prim54025, align 8
%stackaddr$prim54026 = alloca %struct.ScmObj*, align 8
%current_45args53231 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53230)
store volatile %struct.ScmObj* %current_45args53231, %struct.ScmObj** %stackaddr$prim54026, align 8
%stackaddr$prim54027 = alloca %struct.ScmObj*, align 8
%anf_45bind47266 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53231)
store volatile %struct.ScmObj* %anf_45bind47266, %struct.ScmObj** %stackaddr$prim54027, align 8
%ae49312 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54028 = alloca %struct.ScmObj*, align 8
%_95047162 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append47161, %struct.ScmObj* %ae49312, %struct.ScmObj* %anf_45bind47266)
store volatile %struct.ScmObj* %_95047162, %struct.ScmObj** %stackaddr$prim54028, align 8
%ae49315 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54029 = alloca %struct.ScmObj*, align 8
%_37append47160 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append47161, %struct.ScmObj* %ae49315)
store volatile %struct.ScmObj* %_37append47160, %struct.ScmObj** %stackaddr$prim54029, align 8
%stackaddr$makeclosure54030 = alloca %struct.ScmObj*, align 8
%fptrToInt54031 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49316 to i64
%ae49316 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54031)
store volatile %struct.ScmObj* %ae49316, %struct.ScmObj** %stackaddr$makeclosure54030, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49316, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49316, %struct.ScmObj* %_37map47115, i64 1)
%ae49317 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54032 = alloca %struct.ScmObj*, align 8
%fptrToInt54033 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49318 to i64
%ae49318 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54033)
store volatile %struct.ScmObj* %ae49318, %struct.ScmObj** %stackaddr$makeclosure54032, align 8
%argslist53405$ae493160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54034 = alloca %struct.ScmObj*, align 8
%argslist53405$ae493161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49318, %struct.ScmObj* %argslist53405$ae493160)
store volatile %struct.ScmObj* %argslist53405$ae493161, %struct.ScmObj** %stackaddr$prim54034, align 8
%stackaddr$prim54035 = alloca %struct.ScmObj*, align 8
%argslist53405$ae493162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49317, %struct.ScmObj* %argslist53405$ae493161)
store volatile %struct.ScmObj* %argslist53405$ae493162, %struct.ScmObj** %stackaddr$prim54035, align 8
%clofunc54036 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49316)
musttail call tailcc void %clofunc54036(%struct.ScmObj* %ae49316, %struct.ScmObj* %argslist53405$ae493162)
ret void
}

define tailcc void @proc_clo$ae49316(%struct.ScmObj* %env$ae49316,%struct.ScmObj* %current_45args53233) {
%stackaddr$env-ref54037 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49316, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54037
%stackaddr$env-ref54038 = alloca %struct.ScmObj*, align 8
%_37map47115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49316, i64 1)
store %struct.ScmObj* %_37map47115, %struct.ScmObj** %stackaddr$env-ref54038
%stackaddr$prim54039 = alloca %struct.ScmObj*, align 8
%_95k47334 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53233)
store volatile %struct.ScmObj* %_95k47334, %struct.ScmObj** %stackaddr$prim54039, align 8
%stackaddr$prim54040 = alloca %struct.ScmObj*, align 8
%current_45args53234 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53233)
store volatile %struct.ScmObj* %current_45args53234, %struct.ScmObj** %stackaddr$prim54040, align 8
%stackaddr$prim54041 = alloca %struct.ScmObj*, align 8
%_37list_6347153 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53234)
store volatile %struct.ScmObj* %_37list_6347153, %struct.ScmObj** %stackaddr$prim54041, align 8
%stackaddr$makeclosure54042 = alloca %struct.ScmObj*, align 8
%fptrToInt54043 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49732 to i64
%ae49732 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54043)
store volatile %struct.ScmObj* %ae49732, %struct.ScmObj** %stackaddr$makeclosure54042, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49732, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49732, %struct.ScmObj* %_37map47115, i64 1)
%ae49733 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54044 = alloca %struct.ScmObj*, align 8
%fptrToInt54045 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49734 to i64
%ae49734 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54045)
store volatile %struct.ScmObj* %ae49734, %struct.ScmObj** %stackaddr$makeclosure54044, align 8
%argslist53380$ae497320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54046 = alloca %struct.ScmObj*, align 8
%argslist53380$ae497321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49734, %struct.ScmObj* %argslist53380$ae497320)
store volatile %struct.ScmObj* %argslist53380$ae497321, %struct.ScmObj** %stackaddr$prim54046, align 8
%stackaddr$prim54047 = alloca %struct.ScmObj*, align 8
%argslist53380$ae497322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49733, %struct.ScmObj* %argslist53380$ae497321)
store volatile %struct.ScmObj* %argslist53380$ae497322, %struct.ScmObj** %stackaddr$prim54047, align 8
%clofunc54048 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49732)
musttail call tailcc void %clofunc54048(%struct.ScmObj* %ae49732, %struct.ScmObj* %argslist53380$ae497322)
ret void
}

define tailcc void @proc_clo$ae49732(%struct.ScmObj* %env$ae49732,%struct.ScmObj* %current_45args53236) {
%stackaddr$env-ref54049 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49732, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54049
%stackaddr$env-ref54050 = alloca %struct.ScmObj*, align 8
%_37map47115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49732, i64 1)
store %struct.ScmObj* %_37map47115, %struct.ScmObj** %stackaddr$env-ref54050
%stackaddr$prim54051 = alloca %struct.ScmObj*, align 8
%_95k47335 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53236)
store volatile %struct.ScmObj* %_95k47335, %struct.ScmObj** %stackaddr$prim54051, align 8
%stackaddr$prim54052 = alloca %struct.ScmObj*, align 8
%current_45args53237 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53236)
store volatile %struct.ScmObj* %current_45args53237, %struct.ScmObj** %stackaddr$prim54052, align 8
%stackaddr$prim54053 = alloca %struct.ScmObj*, align 8
%_37drop47144 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53237)
store volatile %struct.ScmObj* %_37drop47144, %struct.ScmObj** %stackaddr$prim54053, align 8
%stackaddr$makeclosure54054 = alloca %struct.ScmObj*, align 8
%fptrToInt54055 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50268 to i64
%ae50268 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54055)
store volatile %struct.ScmObj* %ae50268, %struct.ScmObj** %stackaddr$makeclosure54054, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50268, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50268, %struct.ScmObj* %_37map47115, i64 1)
%ae50269 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54056 = alloca %struct.ScmObj*, align 8
%fptrToInt54057 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50270 to i64
%ae50270 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54057)
store volatile %struct.ScmObj* %ae50270, %struct.ScmObj** %stackaddr$makeclosure54056, align 8
%argslist53356$ae502680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54058 = alloca %struct.ScmObj*, align 8
%argslist53356$ae502681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50270, %struct.ScmObj* %argslist53356$ae502680)
store volatile %struct.ScmObj* %argslist53356$ae502681, %struct.ScmObj** %stackaddr$prim54058, align 8
%stackaddr$prim54059 = alloca %struct.ScmObj*, align 8
%argslist53356$ae502682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50269, %struct.ScmObj* %argslist53356$ae502681)
store volatile %struct.ScmObj* %argslist53356$ae502682, %struct.ScmObj** %stackaddr$prim54059, align 8
%clofunc54060 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50268)
musttail call tailcc void %clofunc54060(%struct.ScmObj* %ae50268, %struct.ScmObj* %argslist53356$ae502682)
ret void
}

define tailcc void @proc_clo$ae50268(%struct.ScmObj* %env$ae50268,%struct.ScmObj* %current_45args53239) {
%stackaddr$env-ref54061 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50268, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54061
%stackaddr$env-ref54062 = alloca %struct.ScmObj*, align 8
%_37map47115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50268, i64 1)
store %struct.ScmObj* %_37map47115, %struct.ScmObj** %stackaddr$env-ref54062
%stackaddr$prim54063 = alloca %struct.ScmObj*, align 8
%_95k47336 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53239)
store volatile %struct.ScmObj* %_95k47336, %struct.ScmObj** %stackaddr$prim54063, align 8
%stackaddr$prim54064 = alloca %struct.ScmObj*, align 8
%current_45args53240 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53239)
store volatile %struct.ScmObj* %current_45args53240, %struct.ScmObj** %stackaddr$prim54064, align 8
%stackaddr$prim54065 = alloca %struct.ScmObj*, align 8
%_37memv47137 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53240)
store volatile %struct.ScmObj* %_37memv47137, %struct.ScmObj** %stackaddr$prim54065, align 8
%stackaddr$makeclosure54066 = alloca %struct.ScmObj*, align 8
%fptrToInt54067 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50672 to i64
%ae50672 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54067)
store volatile %struct.ScmObj* %ae50672, %struct.ScmObj** %stackaddr$makeclosure54066, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50672, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50672, %struct.ScmObj* %_37map47115, i64 1)
%ae50673 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54068 = alloca %struct.ScmObj*, align 8
%fptrToInt54069 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50674 to i64
%ae50674 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54069)
store volatile %struct.ScmObj* %ae50674, %struct.ScmObj** %stackaddr$makeclosure54068, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50674, %struct.ScmObj* %_37foldl147073, i64 0)
%argslist53330$ae506720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54070 = alloca %struct.ScmObj*, align 8
%argslist53330$ae506721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50674, %struct.ScmObj* %argslist53330$ae506720)
store volatile %struct.ScmObj* %argslist53330$ae506721, %struct.ScmObj** %stackaddr$prim54070, align 8
%stackaddr$prim54071 = alloca %struct.ScmObj*, align 8
%argslist53330$ae506722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50673, %struct.ScmObj* %argslist53330$ae506721)
store volatile %struct.ScmObj* %argslist53330$ae506722, %struct.ScmObj** %stackaddr$prim54071, align 8
%clofunc54072 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50672)
musttail call tailcc void %clofunc54072(%struct.ScmObj* %ae50672, %struct.ScmObj* %argslist53330$ae506722)
ret void
}

define tailcc void @proc_clo$ae50672(%struct.ScmObj* %env$ae50672,%struct.ScmObj* %current_45args53242) {
%stackaddr$env-ref54073 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50672, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54073
%stackaddr$env-ref54074 = alloca %struct.ScmObj*, align 8
%_37map47115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50672, i64 1)
store %struct.ScmObj* %_37map47115, %struct.ScmObj** %stackaddr$env-ref54074
%stackaddr$prim54075 = alloca %struct.ScmObj*, align 8
%_95k47337 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53242)
store volatile %struct.ScmObj* %_95k47337, %struct.ScmObj** %stackaddr$prim54075, align 8
%stackaddr$prim54076 = alloca %struct.ScmObj*, align 8
%current_45args53243 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53242)
store volatile %struct.ScmObj* %current_45args53243, %struct.ScmObj** %stackaddr$prim54076, align 8
%stackaddr$prim54077 = alloca %struct.ScmObj*, align 8
%_37_4747133 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53243)
store volatile %struct.ScmObj* %_37_4747133, %struct.ScmObj** %stackaddr$prim54077, align 8
%stackaddr$makeclosure54078 = alloca %struct.ScmObj*, align 8
%fptrToInt54079 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50770 to i64
%ae50770 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54079)
store volatile %struct.ScmObj* %ae50770, %struct.ScmObj** %stackaddr$makeclosure54078, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50770, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50770, %struct.ScmObj* %_37map47115, i64 1)
%ae50771 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54080 = alloca %struct.ScmObj*, align 8
%fptrToInt54081 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50772 to i64
%ae50772 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54081)
store volatile %struct.ScmObj* %ae50772, %struct.ScmObj** %stackaddr$makeclosure54080, align 8
%argslist53317$ae507700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54082 = alloca %struct.ScmObj*, align 8
%argslist53317$ae507701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50772, %struct.ScmObj* %argslist53317$ae507700)
store volatile %struct.ScmObj* %argslist53317$ae507701, %struct.ScmObj** %stackaddr$prim54082, align 8
%stackaddr$prim54083 = alloca %struct.ScmObj*, align 8
%argslist53317$ae507702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50771, %struct.ScmObj* %argslist53317$ae507701)
store volatile %struct.ScmObj* %argslist53317$ae507702, %struct.ScmObj** %stackaddr$prim54083, align 8
%clofunc54084 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50770)
musttail call tailcc void %clofunc54084(%struct.ScmObj* %ae50770, %struct.ScmObj* %argslist53317$ae507702)
ret void
}

define tailcc void @proc_clo$ae50770(%struct.ScmObj* %env$ae50770,%struct.ScmObj* %current_45args53245) {
%stackaddr$env-ref54085 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50770, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54085
%stackaddr$env-ref54086 = alloca %struct.ScmObj*, align 8
%_37map47115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50770, i64 1)
store %struct.ScmObj* %_37map47115, %struct.ScmObj** %stackaddr$env-ref54086
%stackaddr$prim54087 = alloca %struct.ScmObj*, align 8
%_95k47338 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53245)
store volatile %struct.ScmObj* %_95k47338, %struct.ScmObj** %stackaddr$prim54087, align 8
%stackaddr$prim54088 = alloca %struct.ScmObj*, align 8
%current_45args53246 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53245)
store volatile %struct.ScmObj* %current_45args53246, %struct.ScmObj** %stackaddr$prim54088, align 8
%stackaddr$prim54089 = alloca %struct.ScmObj*, align 8
%_37first47131 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53246)
store volatile %struct.ScmObj* %_37first47131, %struct.ScmObj** %stackaddr$prim54089, align 8
%stackaddr$makeclosure54090 = alloca %struct.ScmObj*, align 8
%fptrToInt54091 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50790 to i64
%ae50790 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54091)
store volatile %struct.ScmObj* %ae50790, %struct.ScmObj** %stackaddr$makeclosure54090, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50790, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50790, %struct.ScmObj* %_37map47115, i64 1)
%ae50791 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54092 = alloca %struct.ScmObj*, align 8
%fptrToInt54093 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50792 to i64
%ae50792 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54093)
store volatile %struct.ScmObj* %ae50792, %struct.ScmObj** %stackaddr$makeclosure54092, align 8
%argslist53312$ae507900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54094 = alloca %struct.ScmObj*, align 8
%argslist53312$ae507901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50792, %struct.ScmObj* %argslist53312$ae507900)
store volatile %struct.ScmObj* %argslist53312$ae507901, %struct.ScmObj** %stackaddr$prim54094, align 8
%stackaddr$prim54095 = alloca %struct.ScmObj*, align 8
%argslist53312$ae507902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50791, %struct.ScmObj* %argslist53312$ae507901)
store volatile %struct.ScmObj* %argslist53312$ae507902, %struct.ScmObj** %stackaddr$prim54095, align 8
%clofunc54096 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50790)
musttail call tailcc void %clofunc54096(%struct.ScmObj* %ae50790, %struct.ScmObj* %argslist53312$ae507902)
ret void
}

define tailcc void @proc_clo$ae50790(%struct.ScmObj* %env$ae50790,%struct.ScmObj* %current_45args53248) {
%stackaddr$env-ref54097 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50790, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54097
%stackaddr$env-ref54098 = alloca %struct.ScmObj*, align 8
%_37map47115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50790, i64 1)
store %struct.ScmObj* %_37map47115, %struct.ScmObj** %stackaddr$env-ref54098
%stackaddr$prim54099 = alloca %struct.ScmObj*, align 8
%_95k47339 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53248)
store volatile %struct.ScmObj* %_95k47339, %struct.ScmObj** %stackaddr$prim54099, align 8
%stackaddr$prim54100 = alloca %struct.ScmObj*, align 8
%current_45args53249 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53248)
store volatile %struct.ScmObj* %current_45args53249, %struct.ScmObj** %stackaddr$prim54100, align 8
%stackaddr$prim54101 = alloca %struct.ScmObj*, align 8
%_37second47129 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53249)
store volatile %struct.ScmObj* %_37second47129, %struct.ScmObj** %stackaddr$prim54101, align 8
%stackaddr$makeclosure54102 = alloca %struct.ScmObj*, align 8
%fptrToInt54103 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50812 to i64
%ae50812 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54103)
store volatile %struct.ScmObj* %ae50812, %struct.ScmObj** %stackaddr$makeclosure54102, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50812, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50812, %struct.ScmObj* %_37map47115, i64 1)
%ae50813 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54104 = alloca %struct.ScmObj*, align 8
%fptrToInt54105 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50814 to i64
%ae50814 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54105)
store volatile %struct.ScmObj* %ae50814, %struct.ScmObj** %stackaddr$makeclosure54104, align 8
%argslist53307$ae508120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54106 = alloca %struct.ScmObj*, align 8
%argslist53307$ae508121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50814, %struct.ScmObj* %argslist53307$ae508120)
store volatile %struct.ScmObj* %argslist53307$ae508121, %struct.ScmObj** %stackaddr$prim54106, align 8
%stackaddr$prim54107 = alloca %struct.ScmObj*, align 8
%argslist53307$ae508122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50813, %struct.ScmObj* %argslist53307$ae508121)
store volatile %struct.ScmObj* %argslist53307$ae508122, %struct.ScmObj** %stackaddr$prim54107, align 8
%clofunc54108 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50812)
musttail call tailcc void %clofunc54108(%struct.ScmObj* %ae50812, %struct.ScmObj* %argslist53307$ae508122)
ret void
}

define tailcc void @proc_clo$ae50812(%struct.ScmObj* %env$ae50812,%struct.ScmObj* %current_45args53251) {
%stackaddr$env-ref54109 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50812, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54109
%stackaddr$env-ref54110 = alloca %struct.ScmObj*, align 8
%_37map47115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50812, i64 1)
store %struct.ScmObj* %_37map47115, %struct.ScmObj** %stackaddr$env-ref54110
%stackaddr$prim54111 = alloca %struct.ScmObj*, align 8
%_95k47340 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53251)
store volatile %struct.ScmObj* %_95k47340, %struct.ScmObj** %stackaddr$prim54111, align 8
%stackaddr$prim54112 = alloca %struct.ScmObj*, align 8
%current_45args53252 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53251)
store volatile %struct.ScmObj* %current_45args53252, %struct.ScmObj** %stackaddr$prim54112, align 8
%stackaddr$prim54113 = alloca %struct.ScmObj*, align 8
%_37third47127 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53252)
store volatile %struct.ScmObj* %_37third47127, %struct.ScmObj** %stackaddr$prim54113, align 8
%stackaddr$makeclosure54114 = alloca %struct.ScmObj*, align 8
%fptrToInt54115 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50836 to i64
%ae50836 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54115)
store volatile %struct.ScmObj* %ae50836, %struct.ScmObj** %stackaddr$makeclosure54114, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50836, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50836, %struct.ScmObj* %_37map47115, i64 1)
%ae50837 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54116 = alloca %struct.ScmObj*, align 8
%fptrToInt54117 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50838 to i64
%ae50838 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54117)
store volatile %struct.ScmObj* %ae50838, %struct.ScmObj** %stackaddr$makeclosure54116, align 8
%argslist53302$ae508360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54118 = alloca %struct.ScmObj*, align 8
%argslist53302$ae508361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50838, %struct.ScmObj* %argslist53302$ae508360)
store volatile %struct.ScmObj* %argslist53302$ae508361, %struct.ScmObj** %stackaddr$prim54118, align 8
%stackaddr$prim54119 = alloca %struct.ScmObj*, align 8
%argslist53302$ae508362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50837, %struct.ScmObj* %argslist53302$ae508361)
store volatile %struct.ScmObj* %argslist53302$ae508362, %struct.ScmObj** %stackaddr$prim54119, align 8
%clofunc54120 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50836)
musttail call tailcc void %clofunc54120(%struct.ScmObj* %ae50836, %struct.ScmObj* %argslist53302$ae508362)
ret void
}

define tailcc void @proc_clo$ae50836(%struct.ScmObj* %env$ae50836,%struct.ScmObj* %current_45args53254) {
%stackaddr$env-ref54121 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50836, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54121
%stackaddr$env-ref54122 = alloca %struct.ScmObj*, align 8
%_37map47115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50836, i64 1)
store %struct.ScmObj* %_37map47115, %struct.ScmObj** %stackaddr$env-ref54122
%stackaddr$prim54123 = alloca %struct.ScmObj*, align 8
%_95k47341 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53254)
store volatile %struct.ScmObj* %_95k47341, %struct.ScmObj** %stackaddr$prim54123, align 8
%stackaddr$prim54124 = alloca %struct.ScmObj*, align 8
%current_45args53255 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53254)
store volatile %struct.ScmObj* %current_45args53255, %struct.ScmObj** %stackaddr$prim54124, align 8
%stackaddr$prim54125 = alloca %struct.ScmObj*, align 8
%_37fourth47125 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53255)
store volatile %struct.ScmObj* %_37fourth47125, %struct.ScmObj** %stackaddr$prim54125, align 8
%stackaddr$makeclosure54126 = alloca %struct.ScmObj*, align 8
%fptrToInt54127 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50862 to i64
%ae50862 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54127)
store volatile %struct.ScmObj* %ae50862, %struct.ScmObj** %stackaddr$makeclosure54126, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50862, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50862, %struct.ScmObj* %_37map47115, i64 1)
%ae50863 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54128 = alloca %struct.ScmObj*, align 8
%fptrToInt54129 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50864 to i64
%ae50864 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54129)
store volatile %struct.ScmObj* %ae50864, %struct.ScmObj** %stackaddr$makeclosure54128, align 8
%argslist53297$ae508620 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54130 = alloca %struct.ScmObj*, align 8
%argslist53297$ae508621 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50864, %struct.ScmObj* %argslist53297$ae508620)
store volatile %struct.ScmObj* %argslist53297$ae508621, %struct.ScmObj** %stackaddr$prim54130, align 8
%stackaddr$prim54131 = alloca %struct.ScmObj*, align 8
%argslist53297$ae508622 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50863, %struct.ScmObj* %argslist53297$ae508621)
store volatile %struct.ScmObj* %argslist53297$ae508622, %struct.ScmObj** %stackaddr$prim54131, align 8
%clofunc54132 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50862)
musttail call tailcc void %clofunc54132(%struct.ScmObj* %ae50862, %struct.ScmObj* %argslist53297$ae508622)
ret void
}

define tailcc void @proc_clo$ae50862(%struct.ScmObj* %env$ae50862,%struct.ScmObj* %current_45args53257) {
%stackaddr$env-ref54133 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50862, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54133
%stackaddr$env-ref54134 = alloca %struct.ScmObj*, align 8
%_37map47115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50862, i64 1)
store %struct.ScmObj* %_37map47115, %struct.ScmObj** %stackaddr$env-ref54134
%stackaddr$prim54135 = alloca %struct.ScmObj*, align 8
%_95k47342 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53257)
store volatile %struct.ScmObj* %_95k47342, %struct.ScmObj** %stackaddr$prim54135, align 8
%stackaddr$prim54136 = alloca %struct.ScmObj*, align 8
%current_45args53258 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53257)
store volatile %struct.ScmObj* %current_45args53258, %struct.ScmObj** %stackaddr$prim54136, align 8
%stackaddr$prim54137 = alloca %struct.ScmObj*, align 8
%anf_45bind47302 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53258)
store volatile %struct.ScmObj* %anf_45bind47302, %struct.ScmObj** %stackaddr$prim54137, align 8
%stackaddr$makeclosure54138 = alloca %struct.ScmObj*, align 8
%fptrToInt54139 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50886 to i64
%ae50886 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54139)
store volatile %struct.ScmObj* %ae50886, %struct.ScmObj** %stackaddr$makeclosure54138, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50886, %struct.ScmObj* %anf_45bind47302, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50886, %struct.ScmObj* %_37foldl147073, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50886, %struct.ScmObj* %_37map47115, i64 2)
%ae50887 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54140 = alloca %struct.ScmObj*, align 8
%fptrToInt54141 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50888 to i64
%ae50888 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54141)
store volatile %struct.ScmObj* %ae50888, %struct.ScmObj** %stackaddr$makeclosure54140, align 8
%argslist53295$ae508860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54142 = alloca %struct.ScmObj*, align 8
%argslist53295$ae508861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50888, %struct.ScmObj* %argslist53295$ae508860)
store volatile %struct.ScmObj* %argslist53295$ae508861, %struct.ScmObj** %stackaddr$prim54142, align 8
%stackaddr$prim54143 = alloca %struct.ScmObj*, align 8
%argslist53295$ae508862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50887, %struct.ScmObj* %argslist53295$ae508861)
store volatile %struct.ScmObj* %argslist53295$ae508862, %struct.ScmObj** %stackaddr$prim54143, align 8
%clofunc54144 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50886)
musttail call tailcc void %clofunc54144(%struct.ScmObj* %ae50886, %struct.ScmObj* %argslist53295$ae508862)
ret void
}

define tailcc void @proc_clo$ae50886(%struct.ScmObj* %env$ae50886,%struct.ScmObj* %current_45args53260) {
%stackaddr$env-ref54145 = alloca %struct.ScmObj*, align 8
%anf_45bind47302 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50886, i64 0)
store %struct.ScmObj* %anf_45bind47302, %struct.ScmObj** %stackaddr$env-ref54145
%stackaddr$env-ref54146 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50886, i64 1)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54146
%stackaddr$env-ref54147 = alloca %struct.ScmObj*, align 8
%_37map47115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50886, i64 2)
store %struct.ScmObj* %_37map47115, %struct.ScmObj** %stackaddr$env-ref54147
%stackaddr$prim54148 = alloca %struct.ScmObj*, align 8
%_95k47343 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53260)
store volatile %struct.ScmObj* %_95k47343, %struct.ScmObj** %stackaddr$prim54148, align 8
%stackaddr$prim54149 = alloca %struct.ScmObj*, align 8
%current_45args53261 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53260)
store volatile %struct.ScmObj* %current_45args53261, %struct.ScmObj** %stackaddr$prim54149, align 8
%stackaddr$prim54150 = alloca %struct.ScmObj*, align 8
%anf_45bind47304 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53261)
store volatile %struct.ScmObj* %anf_45bind47304, %struct.ScmObj** %stackaddr$prim54150, align 8
%stackaddr$makeclosure54151 = alloca %struct.ScmObj*, align 8
%fptrToInt54152 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50912 to i64
%ae50912 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54152)
store volatile %struct.ScmObj* %ae50912, %struct.ScmObj** %stackaddr$makeclosure54151, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50912, %struct.ScmObj* %anf_45bind47302, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50912, %struct.ScmObj* %_37foldl147073, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50912, %struct.ScmObj* %_37map47115, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae50912, %struct.ScmObj* %anf_45bind47304, i64 3)
%ae50913 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54153 = alloca %struct.ScmObj*, align 8
%fptrToInt54154 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50914 to i64
%ae50914 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54154)
store volatile %struct.ScmObj* %ae50914, %struct.ScmObj** %stackaddr$makeclosure54153, align 8
%argslist53289$ae509120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54155 = alloca %struct.ScmObj*, align 8
%argslist53289$ae509121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50914, %struct.ScmObj* %argslist53289$ae509120)
store volatile %struct.ScmObj* %argslist53289$ae509121, %struct.ScmObj** %stackaddr$prim54155, align 8
%stackaddr$prim54156 = alloca %struct.ScmObj*, align 8
%argslist53289$ae509122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50913, %struct.ScmObj* %argslist53289$ae509121)
store volatile %struct.ScmObj* %argslist53289$ae509122, %struct.ScmObj** %stackaddr$prim54156, align 8
%clofunc54157 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50912)
musttail call tailcc void %clofunc54157(%struct.ScmObj* %ae50912, %struct.ScmObj* %argslist53289$ae509122)
ret void
}

define tailcc void @proc_clo$ae50912(%struct.ScmObj* %env$ae50912,%struct.ScmObj* %current_45args53263) {
%stackaddr$env-ref54158 = alloca %struct.ScmObj*, align 8
%anf_45bind47302 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50912, i64 0)
store %struct.ScmObj* %anf_45bind47302, %struct.ScmObj** %stackaddr$env-ref54158
%stackaddr$env-ref54159 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50912, i64 1)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54159
%stackaddr$env-ref54160 = alloca %struct.ScmObj*, align 8
%_37map47115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50912, i64 2)
store %struct.ScmObj* %_37map47115, %struct.ScmObj** %stackaddr$env-ref54160
%stackaddr$env-ref54161 = alloca %struct.ScmObj*, align 8
%anf_45bind47304 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50912, i64 3)
store %struct.ScmObj* %anf_45bind47304, %struct.ScmObj** %stackaddr$env-ref54161
%stackaddr$prim54162 = alloca %struct.ScmObj*, align 8
%_95k47344 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53263)
store volatile %struct.ScmObj* %_95k47344, %struct.ScmObj** %stackaddr$prim54162, align 8
%stackaddr$prim54163 = alloca %struct.ScmObj*, align 8
%current_45args53264 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53263)
store volatile %struct.ScmObj* %current_45args53264, %struct.ScmObj** %stackaddr$prim54163, align 8
%stackaddr$prim54164 = alloca %struct.ScmObj*, align 8
%anf_45bind47305 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53264)
store volatile %struct.ScmObj* %anf_45bind47305, %struct.ScmObj** %stackaddr$prim54164, align 8
%stackaddr$makeclosure54165 = alloca %struct.ScmObj*, align 8
%fptrToInt54166 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50935 to i64
%ae50935 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54166)
store volatile %struct.ScmObj* %ae50935, %struct.ScmObj** %stackaddr$makeclosure54165, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50935, %struct.ScmObj* %anf_45bind47302, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50935, %struct.ScmObj* %_37foldl147073, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50935, %struct.ScmObj* %_37map47115, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae50935, %struct.ScmObj* %anf_45bind47304, i64 3)
%ae50936 = call %struct.ScmObj* @const_init_int(i64 2)
%ae50937 = call %struct.ScmObj* @const_init_int(i64 4)
%ae50938 = call %struct.ScmObj* @const_init_int(i64 6)
%argslist53287$anf_45bind473050 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54167 = alloca %struct.ScmObj*, align 8
%argslist53287$anf_45bind473051 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50938, %struct.ScmObj* %argslist53287$anf_45bind473050)
store volatile %struct.ScmObj* %argslist53287$anf_45bind473051, %struct.ScmObj** %stackaddr$prim54167, align 8
%stackaddr$prim54168 = alloca %struct.ScmObj*, align 8
%argslist53287$anf_45bind473052 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50937, %struct.ScmObj* %argslist53287$anf_45bind473051)
store volatile %struct.ScmObj* %argslist53287$anf_45bind473052, %struct.ScmObj** %stackaddr$prim54168, align 8
%stackaddr$prim54169 = alloca %struct.ScmObj*, align 8
%argslist53287$anf_45bind473053 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50936, %struct.ScmObj* %argslist53287$anf_45bind473052)
store volatile %struct.ScmObj* %argslist53287$anf_45bind473053, %struct.ScmObj** %stackaddr$prim54169, align 8
%stackaddr$prim54170 = alloca %struct.ScmObj*, align 8
%argslist53287$anf_45bind473054 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50935, %struct.ScmObj* %argslist53287$anf_45bind473053)
store volatile %struct.ScmObj* %argslist53287$anf_45bind473054, %struct.ScmObj** %stackaddr$prim54170, align 8
%clofunc54171 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47305)
musttail call tailcc void %clofunc54171(%struct.ScmObj* %anf_45bind47305, %struct.ScmObj* %argslist53287$anf_45bind473054)
ret void
}

define tailcc void @proc_clo$ae50935(%struct.ScmObj* %env$ae50935,%struct.ScmObj* %current_45args53266) {
%stackaddr$env-ref54172 = alloca %struct.ScmObj*, align 8
%anf_45bind47302 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50935, i64 0)
store %struct.ScmObj* %anf_45bind47302, %struct.ScmObj** %stackaddr$env-ref54172
%stackaddr$env-ref54173 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50935, i64 1)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54173
%stackaddr$env-ref54174 = alloca %struct.ScmObj*, align 8
%_37map47115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50935, i64 2)
store %struct.ScmObj* %_37map47115, %struct.ScmObj** %stackaddr$env-ref54174
%stackaddr$env-ref54175 = alloca %struct.ScmObj*, align 8
%anf_45bind47304 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50935, i64 3)
store %struct.ScmObj* %anf_45bind47304, %struct.ScmObj** %stackaddr$env-ref54175
%stackaddr$prim54176 = alloca %struct.ScmObj*, align 8
%_95k47345 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53266)
store volatile %struct.ScmObj* %_95k47345, %struct.ScmObj** %stackaddr$prim54176, align 8
%stackaddr$prim54177 = alloca %struct.ScmObj*, align 8
%current_45args53267 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53266)
store volatile %struct.ScmObj* %current_45args53267, %struct.ScmObj** %stackaddr$prim54177, align 8
%stackaddr$prim54178 = alloca %struct.ScmObj*, align 8
%anf_45bind47306 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53267)
store volatile %struct.ScmObj* %anf_45bind47306, %struct.ScmObj** %stackaddr$prim54178, align 8
%stackaddr$makeclosure54179 = alloca %struct.ScmObj*, align 8
%fptrToInt54180 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50954 to i64
%ae50954 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54180)
store volatile %struct.ScmObj* %ae50954, %struct.ScmObj** %stackaddr$makeclosure54179, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50954, %struct.ScmObj* %anf_45bind47302, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50954, %struct.ScmObj* %_37foldl147073, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50954, %struct.ScmObj* %_37map47115, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae50954, %struct.ScmObj* %anf_45bind47306, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae50954, %struct.ScmObj* %anf_45bind47304, i64 4)
%ae50955 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54181 = alloca %struct.ScmObj*, align 8
%fptrToInt54182 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50956 to i64
%ae50956 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54182)
store volatile %struct.ScmObj* %ae50956, %struct.ScmObj** %stackaddr$makeclosure54181, align 8
%argslist53286$ae509540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54183 = alloca %struct.ScmObj*, align 8
%argslist53286$ae509541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50956, %struct.ScmObj* %argslist53286$ae509540)
store volatile %struct.ScmObj* %argslist53286$ae509541, %struct.ScmObj** %stackaddr$prim54183, align 8
%stackaddr$prim54184 = alloca %struct.ScmObj*, align 8
%argslist53286$ae509542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50955, %struct.ScmObj* %argslist53286$ae509541)
store volatile %struct.ScmObj* %argslist53286$ae509542, %struct.ScmObj** %stackaddr$prim54184, align 8
%clofunc54185 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50954)
musttail call tailcc void %clofunc54185(%struct.ScmObj* %ae50954, %struct.ScmObj* %argslist53286$ae509542)
ret void
}

define tailcc void @proc_clo$ae50954(%struct.ScmObj* %env$ae50954,%struct.ScmObj* %current_45args53269) {
%stackaddr$env-ref54186 = alloca %struct.ScmObj*, align 8
%anf_45bind47302 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50954, i64 0)
store %struct.ScmObj* %anf_45bind47302, %struct.ScmObj** %stackaddr$env-ref54186
%stackaddr$env-ref54187 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50954, i64 1)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54187
%stackaddr$env-ref54188 = alloca %struct.ScmObj*, align 8
%_37map47115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50954, i64 2)
store %struct.ScmObj* %_37map47115, %struct.ScmObj** %stackaddr$env-ref54188
%stackaddr$env-ref54189 = alloca %struct.ScmObj*, align 8
%anf_45bind47306 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50954, i64 3)
store %struct.ScmObj* %anf_45bind47306, %struct.ScmObj** %stackaddr$env-ref54189
%stackaddr$env-ref54190 = alloca %struct.ScmObj*, align 8
%anf_45bind47304 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50954, i64 4)
store %struct.ScmObj* %anf_45bind47304, %struct.ScmObj** %stackaddr$env-ref54190
%stackaddr$prim54191 = alloca %struct.ScmObj*, align 8
%_95k47346 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53269)
store volatile %struct.ScmObj* %_95k47346, %struct.ScmObj** %stackaddr$prim54191, align 8
%stackaddr$prim54192 = alloca %struct.ScmObj*, align 8
%current_45args53270 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53269)
store volatile %struct.ScmObj* %current_45args53270, %struct.ScmObj** %stackaddr$prim54192, align 8
%stackaddr$prim54193 = alloca %struct.ScmObj*, align 8
%anf_45bind47307 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53270)
store volatile %struct.ScmObj* %anf_45bind47307, %struct.ScmObj** %stackaddr$prim54193, align 8
%stackaddr$makeclosure54194 = alloca %struct.ScmObj*, align 8
%fptrToInt54195 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50977 to i64
%ae50977 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54195)
store volatile %struct.ScmObj* %ae50977, %struct.ScmObj** %stackaddr$makeclosure54194, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50977, %struct.ScmObj* %anf_45bind47302, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50977, %struct.ScmObj* %_37foldl147073, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50977, %struct.ScmObj* %_37map47115, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae50977, %struct.ScmObj* %anf_45bind47306, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae50977, %struct.ScmObj* %anf_45bind47304, i64 4)
%ae50978 = call %struct.ScmObj* @const_init_int(i64 7)
%ae50979 = call %struct.ScmObj* @const_init_int(i64 8)
%ae50980 = call %struct.ScmObj* @const_init_int(i64 9)
%argslist53284$anf_45bind473070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54196 = alloca %struct.ScmObj*, align 8
%argslist53284$anf_45bind473071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50980, %struct.ScmObj* %argslist53284$anf_45bind473070)
store volatile %struct.ScmObj* %argslist53284$anf_45bind473071, %struct.ScmObj** %stackaddr$prim54196, align 8
%stackaddr$prim54197 = alloca %struct.ScmObj*, align 8
%argslist53284$anf_45bind473072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50979, %struct.ScmObj* %argslist53284$anf_45bind473071)
store volatile %struct.ScmObj* %argslist53284$anf_45bind473072, %struct.ScmObj** %stackaddr$prim54197, align 8
%stackaddr$prim54198 = alloca %struct.ScmObj*, align 8
%argslist53284$anf_45bind473073 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50978, %struct.ScmObj* %argslist53284$anf_45bind473072)
store volatile %struct.ScmObj* %argslist53284$anf_45bind473073, %struct.ScmObj** %stackaddr$prim54198, align 8
%stackaddr$prim54199 = alloca %struct.ScmObj*, align 8
%argslist53284$anf_45bind473074 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50977, %struct.ScmObj* %argslist53284$anf_45bind473073)
store volatile %struct.ScmObj* %argslist53284$anf_45bind473074, %struct.ScmObj** %stackaddr$prim54199, align 8
%clofunc54200 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47307)
musttail call tailcc void %clofunc54200(%struct.ScmObj* %anf_45bind47307, %struct.ScmObj* %argslist53284$anf_45bind473074)
ret void
}

define tailcc void @proc_clo$ae50977(%struct.ScmObj* %env$ae50977,%struct.ScmObj* %current_45args53272) {
%stackaddr$env-ref54201 = alloca %struct.ScmObj*, align 8
%anf_45bind47302 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50977, i64 0)
store %struct.ScmObj* %anf_45bind47302, %struct.ScmObj** %stackaddr$env-ref54201
%stackaddr$env-ref54202 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50977, i64 1)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54202
%stackaddr$env-ref54203 = alloca %struct.ScmObj*, align 8
%_37map47115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50977, i64 2)
store %struct.ScmObj* %_37map47115, %struct.ScmObj** %stackaddr$env-ref54203
%stackaddr$env-ref54204 = alloca %struct.ScmObj*, align 8
%anf_45bind47306 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50977, i64 3)
store %struct.ScmObj* %anf_45bind47306, %struct.ScmObj** %stackaddr$env-ref54204
%stackaddr$env-ref54205 = alloca %struct.ScmObj*, align 8
%anf_45bind47304 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50977, i64 4)
store %struct.ScmObj* %anf_45bind47304, %struct.ScmObj** %stackaddr$env-ref54205
%stackaddr$prim54206 = alloca %struct.ScmObj*, align 8
%_95k47347 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53272)
store volatile %struct.ScmObj* %_95k47347, %struct.ScmObj** %stackaddr$prim54206, align 8
%stackaddr$prim54207 = alloca %struct.ScmObj*, align 8
%current_45args53273 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53272)
store volatile %struct.ScmObj* %current_45args53273, %struct.ScmObj** %stackaddr$prim54207, align 8
%stackaddr$prim54208 = alloca %struct.ScmObj*, align 8
%anf_45bind47308 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53273)
store volatile %struct.ScmObj* %anf_45bind47308, %struct.ScmObj** %stackaddr$prim54208, align 8
%stackaddr$makeclosure54209 = alloca %struct.ScmObj*, align 8
%fptrToInt54210 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50997 to i64
%ae50997 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54210)
store volatile %struct.ScmObj* %ae50997, %struct.ScmObj** %stackaddr$makeclosure54209, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50997, %struct.ScmObj* %anf_45bind47302, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50997, %struct.ScmObj* %_37foldl147073, i64 1)
%argslist53283$_37map471150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54211 = alloca %struct.ScmObj*, align 8
%argslist53283$_37map471151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47308, %struct.ScmObj* %argslist53283$_37map471150)
store volatile %struct.ScmObj* %argslist53283$_37map471151, %struct.ScmObj** %stackaddr$prim54211, align 8
%stackaddr$prim54212 = alloca %struct.ScmObj*, align 8
%argslist53283$_37map471152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47306, %struct.ScmObj* %argslist53283$_37map471151)
store volatile %struct.ScmObj* %argslist53283$_37map471152, %struct.ScmObj** %stackaddr$prim54212, align 8
%stackaddr$prim54213 = alloca %struct.ScmObj*, align 8
%argslist53283$_37map471153 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47304, %struct.ScmObj* %argslist53283$_37map471152)
store volatile %struct.ScmObj* %argslist53283$_37map471153, %struct.ScmObj** %stackaddr$prim54213, align 8
%stackaddr$prim54214 = alloca %struct.ScmObj*, align 8
%argslist53283$_37map471154 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50997, %struct.ScmObj* %argslist53283$_37map471153)
store volatile %struct.ScmObj* %argslist53283$_37map471154, %struct.ScmObj** %stackaddr$prim54214, align 8
%clofunc54215 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map47115)
musttail call tailcc void %clofunc54215(%struct.ScmObj* %_37map47115, %struct.ScmObj* %argslist53283$_37map471154)
ret void
}

define tailcc void @proc_clo$ae50997(%struct.ScmObj* %env$ae50997,%struct.ScmObj* %current_45args53275) {
%stackaddr$env-ref54216 = alloca %struct.ScmObj*, align 8
%anf_45bind47302 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50997, i64 0)
store %struct.ScmObj* %anf_45bind47302, %struct.ScmObj** %stackaddr$env-ref54216
%stackaddr$env-ref54217 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50997, i64 1)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54217
%stackaddr$prim54218 = alloca %struct.ScmObj*, align 8
%_95k47348 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53275)
store volatile %struct.ScmObj* %_95k47348, %struct.ScmObj** %stackaddr$prim54218, align 8
%stackaddr$prim54219 = alloca %struct.ScmObj*, align 8
%current_45args53276 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53275)
store volatile %struct.ScmObj* %current_45args53276, %struct.ScmObj** %stackaddr$prim54219, align 8
%stackaddr$prim54220 = alloca %struct.ScmObj*, align 8
%anf_45bind47309 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53276)
store volatile %struct.ScmObj* %anf_45bind47309, %struct.ScmObj** %stackaddr$prim54220, align 8
%stackaddr$makeclosure54221 = alloca %struct.ScmObj*, align 8
%fptrToInt54222 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51002 to i64
%ae51002 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54222)
store volatile %struct.ScmObj* %ae51002, %struct.ScmObj** %stackaddr$makeclosure54221, align 8
%ae51004 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53282$_37foldl1470730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54223 = alloca %struct.ScmObj*, align 8
%argslist53282$_37foldl1470731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47309, %struct.ScmObj* %argslist53282$_37foldl1470730)
store volatile %struct.ScmObj* %argslist53282$_37foldl1470731, %struct.ScmObj** %stackaddr$prim54223, align 8
%stackaddr$prim54224 = alloca %struct.ScmObj*, align 8
%argslist53282$_37foldl1470732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51004, %struct.ScmObj* %argslist53282$_37foldl1470731)
store volatile %struct.ScmObj* %argslist53282$_37foldl1470732, %struct.ScmObj** %stackaddr$prim54224, align 8
%stackaddr$prim54225 = alloca %struct.ScmObj*, align 8
%argslist53282$_37foldl1470733 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47302, %struct.ScmObj* %argslist53282$_37foldl1470732)
store volatile %struct.ScmObj* %argslist53282$_37foldl1470733, %struct.ScmObj** %stackaddr$prim54225, align 8
%stackaddr$prim54226 = alloca %struct.ScmObj*, align 8
%argslist53282$_37foldl1470734 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51002, %struct.ScmObj* %argslist53282$_37foldl1470733)
store volatile %struct.ScmObj* %argslist53282$_37foldl1470734, %struct.ScmObj** %stackaddr$prim54226, align 8
%clofunc54227 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147073)
musttail call tailcc void %clofunc54227(%struct.ScmObj* %_37foldl147073, %struct.ScmObj* %argslist53282$_37foldl1470734)
ret void
}

define tailcc void @proc_clo$ae51002(%struct.ScmObj* %env$ae51002,%struct.ScmObj* %current_45args53278) {
%stackaddr$prim54228 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53278)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim54228, align 8
%stackaddr$prim54229 = alloca %struct.ScmObj*, align 8
%current_45args53279 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53278)
store volatile %struct.ScmObj* %current_45args53279, %struct.ScmObj** %stackaddr$prim54229, align 8
%stackaddr$prim54230 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53279)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim54230, align 8
%stackaddr$prim54231 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim54231, align 8
%argslist53281$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54232 = alloca %struct.ScmObj*, align 8
%argslist53281$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist53281$k0)
store volatile %struct.ScmObj* %argslist53281$k1, %struct.ScmObj** %stackaddr$prim54232, align 8
%clofunc54233 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc54233(%struct.ScmObj* %k, %struct.ScmObj* %argslist53281$k1)
ret void
}

define tailcc void @proc_clo$ae50956(%struct.ScmObj* %env$ae50956,%struct.ScmObj* %lst4719047349) {
%stackaddr$prim54234 = alloca %struct.ScmObj*, align 8
%k47350 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4719047349)
store volatile %struct.ScmObj* %k47350, %struct.ScmObj** %stackaddr$prim54234, align 8
%stackaddr$prim54235 = alloca %struct.ScmObj*, align 8
%lst47190 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4719047349)
store volatile %struct.ScmObj* %lst47190, %struct.ScmObj** %stackaddr$prim54235, align 8
%ae50960 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53285$k473500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54236 = alloca %struct.ScmObj*, align 8
%argslist53285$k473501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47190, %struct.ScmObj* %argslist53285$k473500)
store volatile %struct.ScmObj* %argslist53285$k473501, %struct.ScmObj** %stackaddr$prim54236, align 8
%stackaddr$prim54237 = alloca %struct.ScmObj*, align 8
%argslist53285$k473502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50960, %struct.ScmObj* %argslist53285$k473501)
store volatile %struct.ScmObj* %argslist53285$k473502, %struct.ScmObj** %stackaddr$prim54237, align 8
%clofunc54238 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47350)
musttail call tailcc void %clofunc54238(%struct.ScmObj* %k47350, %struct.ScmObj* %argslist53285$k473502)
ret void
}

define tailcc void @proc_clo$ae50914(%struct.ScmObj* %env$ae50914,%struct.ScmObj* %lst4718947351) {
%stackaddr$prim54239 = alloca %struct.ScmObj*, align 8
%k47352 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4718947351)
store volatile %struct.ScmObj* %k47352, %struct.ScmObj** %stackaddr$prim54239, align 8
%stackaddr$prim54240 = alloca %struct.ScmObj*, align 8
%lst47189 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4718947351)
store volatile %struct.ScmObj* %lst47189, %struct.ScmObj** %stackaddr$prim54240, align 8
%ae50918 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53288$k473520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54241 = alloca %struct.ScmObj*, align 8
%argslist53288$k473521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47189, %struct.ScmObj* %argslist53288$k473520)
store volatile %struct.ScmObj* %argslist53288$k473521, %struct.ScmObj** %stackaddr$prim54241, align 8
%stackaddr$prim54242 = alloca %struct.ScmObj*, align 8
%argslist53288$k473522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50918, %struct.ScmObj* %argslist53288$k473521)
store volatile %struct.ScmObj* %argslist53288$k473522, %struct.ScmObj** %stackaddr$prim54242, align 8
%clofunc54243 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47352)
musttail call tailcc void %clofunc54243(%struct.ScmObj* %k47352, %struct.ScmObj* %argslist53288$k473522)
ret void
}

define tailcc void @proc_clo$ae50888(%struct.ScmObj* %env$ae50888,%struct.ScmObj* %current_45args53290) {
%stackaddr$prim54244 = alloca %struct.ScmObj*, align 8
%k47353 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53290)
store volatile %struct.ScmObj* %k47353, %struct.ScmObj** %stackaddr$prim54244, align 8
%stackaddr$prim54245 = alloca %struct.ScmObj*, align 8
%current_45args53291 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53290)
store volatile %struct.ScmObj* %current_45args53291, %struct.ScmObj** %stackaddr$prim54245, align 8
%stackaddr$prim54246 = alloca %struct.ScmObj*, align 8
%x47188 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53291)
store volatile %struct.ScmObj* %x47188, %struct.ScmObj** %stackaddr$prim54246, align 8
%stackaddr$prim54247 = alloca %struct.ScmObj*, align 8
%current_45args53292 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53291)
store volatile %struct.ScmObj* %current_45args53292, %struct.ScmObj** %stackaddr$prim54247, align 8
%stackaddr$prim54248 = alloca %struct.ScmObj*, align 8
%y47187 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53292)
store volatile %struct.ScmObj* %y47187, %struct.ScmObj** %stackaddr$prim54248, align 8
%stackaddr$prim54249 = alloca %struct.ScmObj*, align 8
%anf_45bind47303 = call %struct.ScmObj* @prim__42(%struct.ScmObj* %x47188, %struct.ScmObj* %x47188)
store volatile %struct.ScmObj* %anf_45bind47303, %struct.ScmObj** %stackaddr$prim54249, align 8
%stackaddr$prim54250 = alloca %struct.ScmObj*, align 8
%cpsprim47354 = call %struct.ScmObj* @prim__42(%struct.ScmObj* %y47187, %struct.ScmObj* %anf_45bind47303)
store volatile %struct.ScmObj* %cpsprim47354, %struct.ScmObj** %stackaddr$prim54250, align 8
%ae50894 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53294$k473530 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54251 = alloca %struct.ScmObj*, align 8
%argslist53294$k473531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47354, %struct.ScmObj* %argslist53294$k473530)
store volatile %struct.ScmObj* %argslist53294$k473531, %struct.ScmObj** %stackaddr$prim54251, align 8
%stackaddr$prim54252 = alloca %struct.ScmObj*, align 8
%argslist53294$k473532 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50894, %struct.ScmObj* %argslist53294$k473531)
store volatile %struct.ScmObj* %argslist53294$k473532, %struct.ScmObj** %stackaddr$prim54252, align 8
%clofunc54253 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47353)
musttail call tailcc void %clofunc54253(%struct.ScmObj* %k47353, %struct.ScmObj* %argslist53294$k473532)
ret void
}

define tailcc void @proc_clo$ae50864(%struct.ScmObj* %env$ae50864,%struct.ScmObj* %args4718647355) {
%stackaddr$prim54254 = alloca %struct.ScmObj*, align 8
%k47356 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4718647355)
store volatile %struct.ScmObj* %k47356, %struct.ScmObj** %stackaddr$prim54254, align 8
%stackaddr$prim54255 = alloca %struct.ScmObj*, align 8
%args47186 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4718647355)
store volatile %struct.ScmObj* %args47186, %struct.ScmObj** %stackaddr$prim54255, align 8
%stackaddr$applyprim54256 = alloca %struct.ScmObj*, align 8
%cpsaprim47357 = call %struct.ScmObj* @applyprim__43(%struct.ScmObj* %args47186)
store volatile %struct.ScmObj* %cpsaprim47357, %struct.ScmObj** %stackaddr$applyprim54256, align 8
%ae50869 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53296$k473560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54257 = alloca %struct.ScmObj*, align 8
%argslist53296$k473561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsaprim47357, %struct.ScmObj* %argslist53296$k473560)
store volatile %struct.ScmObj* %argslist53296$k473561, %struct.ScmObj** %stackaddr$prim54257, align 8
%stackaddr$prim54258 = alloca %struct.ScmObj*, align 8
%argslist53296$k473562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50869, %struct.ScmObj* %argslist53296$k473561)
store volatile %struct.ScmObj* %argslist53296$k473562, %struct.ScmObj** %stackaddr$prim54258, align 8
%clofunc54259 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47356)
musttail call tailcc void %clofunc54259(%struct.ScmObj* %k47356, %struct.ScmObj* %argslist53296$k473562)
ret void
}

define tailcc void @proc_clo$ae50838(%struct.ScmObj* %env$ae50838,%struct.ScmObj* %current_45args53298) {
%stackaddr$prim54260 = alloca %struct.ScmObj*, align 8
%k47358 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53298)
store volatile %struct.ScmObj* %k47358, %struct.ScmObj** %stackaddr$prim54260, align 8
%stackaddr$prim54261 = alloca %struct.ScmObj*, align 8
%current_45args53299 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53298)
store volatile %struct.ScmObj* %current_45args53299, %struct.ScmObj** %stackaddr$prim54261, align 8
%stackaddr$prim54262 = alloca %struct.ScmObj*, align 8
%x47126 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53299)
store volatile %struct.ScmObj* %x47126, %struct.ScmObj** %stackaddr$prim54262, align 8
%stackaddr$prim54263 = alloca %struct.ScmObj*, align 8
%anf_45bind47299 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47126)
store volatile %struct.ScmObj* %anf_45bind47299, %struct.ScmObj** %stackaddr$prim54263, align 8
%stackaddr$prim54264 = alloca %struct.ScmObj*, align 8
%anf_45bind47300 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47299)
store volatile %struct.ScmObj* %anf_45bind47300, %struct.ScmObj** %stackaddr$prim54264, align 8
%stackaddr$prim54265 = alloca %struct.ScmObj*, align 8
%anf_45bind47301 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47300)
store volatile %struct.ScmObj* %anf_45bind47301, %struct.ScmObj** %stackaddr$prim54265, align 8
%stackaddr$prim54266 = alloca %struct.ScmObj*, align 8
%cpsprim47359 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47301)
store volatile %struct.ScmObj* %cpsprim47359, %struct.ScmObj** %stackaddr$prim54266, align 8
%ae50844 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53301$k473580 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54267 = alloca %struct.ScmObj*, align 8
%argslist53301$k473581 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47359, %struct.ScmObj* %argslist53301$k473580)
store volatile %struct.ScmObj* %argslist53301$k473581, %struct.ScmObj** %stackaddr$prim54267, align 8
%stackaddr$prim54268 = alloca %struct.ScmObj*, align 8
%argslist53301$k473582 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50844, %struct.ScmObj* %argslist53301$k473581)
store volatile %struct.ScmObj* %argslist53301$k473582, %struct.ScmObj** %stackaddr$prim54268, align 8
%clofunc54269 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47358)
musttail call tailcc void %clofunc54269(%struct.ScmObj* %k47358, %struct.ScmObj* %argslist53301$k473582)
ret void
}

define tailcc void @proc_clo$ae50814(%struct.ScmObj* %env$ae50814,%struct.ScmObj* %current_45args53303) {
%stackaddr$prim54270 = alloca %struct.ScmObj*, align 8
%k47360 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53303)
store volatile %struct.ScmObj* %k47360, %struct.ScmObj** %stackaddr$prim54270, align 8
%stackaddr$prim54271 = alloca %struct.ScmObj*, align 8
%current_45args53304 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53303)
store volatile %struct.ScmObj* %current_45args53304, %struct.ScmObj** %stackaddr$prim54271, align 8
%stackaddr$prim54272 = alloca %struct.ScmObj*, align 8
%x47128 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53304)
store volatile %struct.ScmObj* %x47128, %struct.ScmObj** %stackaddr$prim54272, align 8
%stackaddr$prim54273 = alloca %struct.ScmObj*, align 8
%anf_45bind47297 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47128)
store volatile %struct.ScmObj* %anf_45bind47297, %struct.ScmObj** %stackaddr$prim54273, align 8
%stackaddr$prim54274 = alloca %struct.ScmObj*, align 8
%anf_45bind47298 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47297)
store volatile %struct.ScmObj* %anf_45bind47298, %struct.ScmObj** %stackaddr$prim54274, align 8
%stackaddr$prim54275 = alloca %struct.ScmObj*, align 8
%cpsprim47361 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47298)
store volatile %struct.ScmObj* %cpsprim47361, %struct.ScmObj** %stackaddr$prim54275, align 8
%ae50819 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53306$k473600 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54276 = alloca %struct.ScmObj*, align 8
%argslist53306$k473601 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47361, %struct.ScmObj* %argslist53306$k473600)
store volatile %struct.ScmObj* %argslist53306$k473601, %struct.ScmObj** %stackaddr$prim54276, align 8
%stackaddr$prim54277 = alloca %struct.ScmObj*, align 8
%argslist53306$k473602 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50819, %struct.ScmObj* %argslist53306$k473601)
store volatile %struct.ScmObj* %argslist53306$k473602, %struct.ScmObj** %stackaddr$prim54277, align 8
%clofunc54278 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47360)
musttail call tailcc void %clofunc54278(%struct.ScmObj* %k47360, %struct.ScmObj* %argslist53306$k473602)
ret void
}

define tailcc void @proc_clo$ae50792(%struct.ScmObj* %env$ae50792,%struct.ScmObj* %current_45args53308) {
%stackaddr$prim54279 = alloca %struct.ScmObj*, align 8
%k47362 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53308)
store volatile %struct.ScmObj* %k47362, %struct.ScmObj** %stackaddr$prim54279, align 8
%stackaddr$prim54280 = alloca %struct.ScmObj*, align 8
%current_45args53309 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53308)
store volatile %struct.ScmObj* %current_45args53309, %struct.ScmObj** %stackaddr$prim54280, align 8
%stackaddr$prim54281 = alloca %struct.ScmObj*, align 8
%x47130 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53309)
store volatile %struct.ScmObj* %x47130, %struct.ScmObj** %stackaddr$prim54281, align 8
%stackaddr$prim54282 = alloca %struct.ScmObj*, align 8
%anf_45bind47296 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47130)
store volatile %struct.ScmObj* %anf_45bind47296, %struct.ScmObj** %stackaddr$prim54282, align 8
%stackaddr$prim54283 = alloca %struct.ScmObj*, align 8
%cpsprim47363 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47296)
store volatile %struct.ScmObj* %cpsprim47363, %struct.ScmObj** %stackaddr$prim54283, align 8
%ae50796 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53311$k473620 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54284 = alloca %struct.ScmObj*, align 8
%argslist53311$k473621 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47363, %struct.ScmObj* %argslist53311$k473620)
store volatile %struct.ScmObj* %argslist53311$k473621, %struct.ScmObj** %stackaddr$prim54284, align 8
%stackaddr$prim54285 = alloca %struct.ScmObj*, align 8
%argslist53311$k473622 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50796, %struct.ScmObj* %argslist53311$k473621)
store volatile %struct.ScmObj* %argslist53311$k473622, %struct.ScmObj** %stackaddr$prim54285, align 8
%clofunc54286 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47362)
musttail call tailcc void %clofunc54286(%struct.ScmObj* %k47362, %struct.ScmObj* %argslist53311$k473622)
ret void
}

define tailcc void @proc_clo$ae50772(%struct.ScmObj* %env$ae50772,%struct.ScmObj* %current_45args53313) {
%stackaddr$prim54287 = alloca %struct.ScmObj*, align 8
%k47364 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53313)
store volatile %struct.ScmObj* %k47364, %struct.ScmObj** %stackaddr$prim54287, align 8
%stackaddr$prim54288 = alloca %struct.ScmObj*, align 8
%current_45args53314 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53313)
store volatile %struct.ScmObj* %current_45args53314, %struct.ScmObj** %stackaddr$prim54288, align 8
%stackaddr$prim54289 = alloca %struct.ScmObj*, align 8
%x47132 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53314)
store volatile %struct.ScmObj* %x47132, %struct.ScmObj** %stackaddr$prim54289, align 8
%stackaddr$prim54290 = alloca %struct.ScmObj*, align 8
%cpsprim47365 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47132)
store volatile %struct.ScmObj* %cpsprim47365, %struct.ScmObj** %stackaddr$prim54290, align 8
%ae50775 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53316$k473640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54291 = alloca %struct.ScmObj*, align 8
%argslist53316$k473641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47365, %struct.ScmObj* %argslist53316$k473640)
store volatile %struct.ScmObj* %argslist53316$k473641, %struct.ScmObj** %stackaddr$prim54291, align 8
%stackaddr$prim54292 = alloca %struct.ScmObj*, align 8
%argslist53316$k473642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50775, %struct.ScmObj* %argslist53316$k473641)
store volatile %struct.ScmObj* %argslist53316$k473642, %struct.ScmObj** %stackaddr$prim54292, align 8
%clofunc54293 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47364)
musttail call tailcc void %clofunc54293(%struct.ScmObj* %k47364, %struct.ScmObj* %argslist53316$k473642)
ret void
}

define tailcc void @proc_clo$ae50674(%struct.ScmObj* %env$ae50674,%struct.ScmObj* %args4713447366) {
%stackaddr$env-ref54294 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50674, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54294
%stackaddr$prim54295 = alloca %struct.ScmObj*, align 8
%k47367 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4713447366)
store volatile %struct.ScmObj* %k47367, %struct.ScmObj** %stackaddr$prim54295, align 8
%stackaddr$prim54296 = alloca %struct.ScmObj*, align 8
%args47134 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4713447366)
store volatile %struct.ScmObj* %args47134, %struct.ScmObj** %stackaddr$prim54296, align 8
%stackaddr$prim54297 = alloca %struct.ScmObj*, align 8
%anf_45bind47290 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args47134)
store volatile %struct.ScmObj* %anf_45bind47290, %struct.ScmObj** %stackaddr$prim54297, align 8
%truthy$cmp54298 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47290)
%cmp$cmp54298 = icmp eq i64 %truthy$cmp54298, 1
br i1 %cmp$cmp54298, label %truebranch$cmp54298, label %falsebranch$cmp54298
truebranch$cmp54298:
%ae50680 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50681 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist53318$k473670 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54299 = alloca %struct.ScmObj*, align 8
%argslist53318$k473671 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50681, %struct.ScmObj* %argslist53318$k473670)
store volatile %struct.ScmObj* %argslist53318$k473671, %struct.ScmObj** %stackaddr$prim54299, align 8
%stackaddr$prim54300 = alloca %struct.ScmObj*, align 8
%argslist53318$k473672 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50680, %struct.ScmObj* %argslist53318$k473671)
store volatile %struct.ScmObj* %argslist53318$k473672, %struct.ScmObj** %stackaddr$prim54300, align 8
%clofunc54301 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47367)
musttail call tailcc void %clofunc54301(%struct.ScmObj* %k47367, %struct.ScmObj* %argslist53318$k473672)
ret void
falsebranch$cmp54298:
%stackaddr$prim54302 = alloca %struct.ScmObj*, align 8
%anf_45bind47291 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47134)
store volatile %struct.ScmObj* %anf_45bind47291, %struct.ScmObj** %stackaddr$prim54302, align 8
%stackaddr$prim54303 = alloca %struct.ScmObj*, align 8
%anf_45bind47292 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47291)
store volatile %struct.ScmObj* %anf_45bind47292, %struct.ScmObj** %stackaddr$prim54303, align 8
%truthy$cmp54304 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47292)
%cmp$cmp54304 = icmp eq i64 %truthy$cmp54304, 1
br i1 %cmp$cmp54304, label %truebranch$cmp54304, label %falsebranch$cmp54304
truebranch$cmp54304:
%stackaddr$prim54305 = alloca %struct.ScmObj*, align 8
%cpsprim47368 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47134)
store volatile %struct.ScmObj* %cpsprim47368, %struct.ScmObj** %stackaddr$prim54305, align 8
%ae50693 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53319$k473670 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54306 = alloca %struct.ScmObj*, align 8
%argslist53319$k473671 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47368, %struct.ScmObj* %argslist53319$k473670)
store volatile %struct.ScmObj* %argslist53319$k473671, %struct.ScmObj** %stackaddr$prim54306, align 8
%stackaddr$prim54307 = alloca %struct.ScmObj*, align 8
%argslist53319$k473672 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50693, %struct.ScmObj* %argslist53319$k473671)
store volatile %struct.ScmObj* %argslist53319$k473672, %struct.ScmObj** %stackaddr$prim54307, align 8
%clofunc54308 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47367)
musttail call tailcc void %clofunc54308(%struct.ScmObj* %k47367, %struct.ScmObj* %argslist53319$k473672)
ret void
falsebranch$cmp54304:
%stackaddr$makeclosure54309 = alloca %struct.ScmObj*, align 8
%fptrToInt54310 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50698 to i64
%ae50698 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54310)
store volatile %struct.ScmObj* %ae50698, %struct.ScmObj** %stackaddr$makeclosure54309, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50698, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50698, %struct.ScmObj* %args47134, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50698, %struct.ScmObj* %k47367, i64 2)
%ae50699 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54311 = alloca %struct.ScmObj*, align 8
%fptrToInt54312 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50700 to i64
%ae50700 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54312)
store volatile %struct.ScmObj* %ae50700, %struct.ScmObj** %stackaddr$makeclosure54311, align 8
%argslist53329$ae506980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54313 = alloca %struct.ScmObj*, align 8
%argslist53329$ae506981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50700, %struct.ScmObj* %argslist53329$ae506980)
store volatile %struct.ScmObj* %argslist53329$ae506981, %struct.ScmObj** %stackaddr$prim54313, align 8
%stackaddr$prim54314 = alloca %struct.ScmObj*, align 8
%argslist53329$ae506982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50699, %struct.ScmObj* %argslist53329$ae506981)
store volatile %struct.ScmObj* %argslist53329$ae506982, %struct.ScmObj** %stackaddr$prim54314, align 8
%clofunc54315 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50698)
musttail call tailcc void %clofunc54315(%struct.ScmObj* %ae50698, %struct.ScmObj* %argslist53329$ae506982)
ret void
}

define tailcc void @proc_clo$ae50698(%struct.ScmObj* %env$ae50698,%struct.ScmObj* %current_45args53320) {
%stackaddr$env-ref54316 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50698, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54316
%stackaddr$env-ref54317 = alloca %struct.ScmObj*, align 8
%args47134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50698, i64 1)
store %struct.ScmObj* %args47134, %struct.ScmObj** %stackaddr$env-ref54317
%stackaddr$env-ref54318 = alloca %struct.ScmObj*, align 8
%k47367 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50698, i64 2)
store %struct.ScmObj* %k47367, %struct.ScmObj** %stackaddr$env-ref54318
%stackaddr$prim54319 = alloca %struct.ScmObj*, align 8
%_95k47369 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53320)
store volatile %struct.ScmObj* %_95k47369, %struct.ScmObj** %stackaddr$prim54319, align 8
%stackaddr$prim54320 = alloca %struct.ScmObj*, align 8
%current_45args53321 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53320)
store volatile %struct.ScmObj* %current_45args53321, %struct.ScmObj** %stackaddr$prim54320, align 8
%stackaddr$prim54321 = alloca %struct.ScmObj*, align 8
%anf_45bind47293 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53321)
store volatile %struct.ScmObj* %anf_45bind47293, %struct.ScmObj** %stackaddr$prim54321, align 8
%stackaddr$prim54322 = alloca %struct.ScmObj*, align 8
%anf_45bind47294 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47134)
store volatile %struct.ScmObj* %anf_45bind47294, %struct.ScmObj** %stackaddr$prim54322, align 8
%stackaddr$prim54323 = alloca %struct.ScmObj*, align 8
%anf_45bind47295 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47134)
store volatile %struct.ScmObj* %anf_45bind47295, %struct.ScmObj** %stackaddr$prim54323, align 8
%argslist53323$_37foldl1470730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54324 = alloca %struct.ScmObj*, align 8
%argslist53323$_37foldl1470731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47295, %struct.ScmObj* %argslist53323$_37foldl1470730)
store volatile %struct.ScmObj* %argslist53323$_37foldl1470731, %struct.ScmObj** %stackaddr$prim54324, align 8
%stackaddr$prim54325 = alloca %struct.ScmObj*, align 8
%argslist53323$_37foldl1470732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47294, %struct.ScmObj* %argslist53323$_37foldl1470731)
store volatile %struct.ScmObj* %argslist53323$_37foldl1470732, %struct.ScmObj** %stackaddr$prim54325, align 8
%stackaddr$prim54326 = alloca %struct.ScmObj*, align 8
%argslist53323$_37foldl1470733 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47293, %struct.ScmObj* %argslist53323$_37foldl1470732)
store volatile %struct.ScmObj* %argslist53323$_37foldl1470733, %struct.ScmObj** %stackaddr$prim54326, align 8
%stackaddr$prim54327 = alloca %struct.ScmObj*, align 8
%argslist53323$_37foldl1470734 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47367, %struct.ScmObj* %argslist53323$_37foldl1470733)
store volatile %struct.ScmObj* %argslist53323$_37foldl1470734, %struct.ScmObj** %stackaddr$prim54327, align 8
%clofunc54328 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147073)
musttail call tailcc void %clofunc54328(%struct.ScmObj* %_37foldl147073, %struct.ScmObj* %argslist53323$_37foldl1470734)
ret void
}

define tailcc void @proc_clo$ae50700(%struct.ScmObj* %env$ae50700,%struct.ScmObj* %current_45args53324) {
%stackaddr$prim54329 = alloca %struct.ScmObj*, align 8
%k47370 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53324)
store volatile %struct.ScmObj* %k47370, %struct.ScmObj** %stackaddr$prim54329, align 8
%stackaddr$prim54330 = alloca %struct.ScmObj*, align 8
%current_45args53325 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53324)
store volatile %struct.ScmObj* %current_45args53325, %struct.ScmObj** %stackaddr$prim54330, align 8
%stackaddr$prim54331 = alloca %struct.ScmObj*, align 8
%n47136 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53325)
store volatile %struct.ScmObj* %n47136, %struct.ScmObj** %stackaddr$prim54331, align 8
%stackaddr$prim54332 = alloca %struct.ScmObj*, align 8
%current_45args53326 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53325)
store volatile %struct.ScmObj* %current_45args53326, %struct.ScmObj** %stackaddr$prim54332, align 8
%stackaddr$prim54333 = alloca %struct.ScmObj*, align 8
%v47135 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53326)
store volatile %struct.ScmObj* %v47135, %struct.ScmObj** %stackaddr$prim54333, align 8
%stackaddr$prim54334 = alloca %struct.ScmObj*, align 8
%cpsprim47371 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v47135, %struct.ScmObj* %n47136)
store volatile %struct.ScmObj* %cpsprim47371, %struct.ScmObj** %stackaddr$prim54334, align 8
%ae50704 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53328$k473700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54335 = alloca %struct.ScmObj*, align 8
%argslist53328$k473701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47371, %struct.ScmObj* %argslist53328$k473700)
store volatile %struct.ScmObj* %argslist53328$k473701, %struct.ScmObj** %stackaddr$prim54335, align 8
%stackaddr$prim54336 = alloca %struct.ScmObj*, align 8
%argslist53328$k473702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50704, %struct.ScmObj* %argslist53328$k473701)
store volatile %struct.ScmObj* %argslist53328$k473702, %struct.ScmObj** %stackaddr$prim54336, align 8
%clofunc54337 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47370)
musttail call tailcc void %clofunc54337(%struct.ScmObj* %k47370, %struct.ScmObj* %argslist53328$k473702)
ret void
}

define tailcc void @proc_clo$ae50270(%struct.ScmObj* %env$ae50270,%struct.ScmObj* %current_45args53331) {
%stackaddr$prim54338 = alloca %struct.ScmObj*, align 8
%k47372 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53331)
store volatile %struct.ScmObj* %k47372, %struct.ScmObj** %stackaddr$prim54338, align 8
%stackaddr$prim54339 = alloca %struct.ScmObj*, align 8
%current_45args53332 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53331)
store volatile %struct.ScmObj* %current_45args53332, %struct.ScmObj** %stackaddr$prim54339, align 8
%stackaddr$prim54340 = alloca %struct.ScmObj*, align 8
%v47139 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53332)
store volatile %struct.ScmObj* %v47139, %struct.ScmObj** %stackaddr$prim54340, align 8
%stackaddr$prim54341 = alloca %struct.ScmObj*, align 8
%current_45args53333 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53332)
store volatile %struct.ScmObj* %current_45args53333, %struct.ScmObj** %stackaddr$prim54341, align 8
%stackaddr$prim54342 = alloca %struct.ScmObj*, align 8
%lst47138 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53333)
store volatile %struct.ScmObj* %lst47138, %struct.ScmObj** %stackaddr$prim54342, align 8
%ae50271 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54343 = alloca %struct.ScmObj*, align 8
%lst47140 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50271, %struct.ScmObj* %lst47138)
store volatile %struct.ScmObj* %lst47140, %struct.ScmObj** %stackaddr$prim54343, align 8
%stackaddr$makeclosure54344 = alloca %struct.ScmObj*, align 8
%fptrToInt54345 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50273 to i64
%ae50273 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54345)
store volatile %struct.ScmObj* %ae50273, %struct.ScmObj** %stackaddr$makeclosure54344, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50273, %struct.ScmObj* %lst47140, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50273, %struct.ScmObj* %v47139, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50273, %struct.ScmObj* %k47372, i64 2)
%ae50274 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54346 = alloca %struct.ScmObj*, align 8
%fptrToInt54347 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50275 to i64
%ae50275 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54347)
store volatile %struct.ScmObj* %ae50275, %struct.ScmObj** %stackaddr$makeclosure54346, align 8
%argslist53355$ae502730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54348 = alloca %struct.ScmObj*, align 8
%argslist53355$ae502731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50275, %struct.ScmObj* %argslist53355$ae502730)
store volatile %struct.ScmObj* %argslist53355$ae502731, %struct.ScmObj** %stackaddr$prim54348, align 8
%stackaddr$prim54349 = alloca %struct.ScmObj*, align 8
%argslist53355$ae502732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50274, %struct.ScmObj* %argslist53355$ae502731)
store volatile %struct.ScmObj* %argslist53355$ae502732, %struct.ScmObj** %stackaddr$prim54349, align 8
%clofunc54350 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50273)
musttail call tailcc void %clofunc54350(%struct.ScmObj* %ae50273, %struct.ScmObj* %argslist53355$ae502732)
ret void
}

define tailcc void @proc_clo$ae50273(%struct.ScmObj* %env$ae50273,%struct.ScmObj* %current_45args53335) {
%stackaddr$env-ref54351 = alloca %struct.ScmObj*, align 8
%lst47140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50273, i64 0)
store %struct.ScmObj* %lst47140, %struct.ScmObj** %stackaddr$env-ref54351
%stackaddr$env-ref54352 = alloca %struct.ScmObj*, align 8
%v47139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50273, i64 1)
store %struct.ScmObj* %v47139, %struct.ScmObj** %stackaddr$env-ref54352
%stackaddr$env-ref54353 = alloca %struct.ScmObj*, align 8
%k47372 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50273, i64 2)
store %struct.ScmObj* %k47372, %struct.ScmObj** %stackaddr$env-ref54353
%stackaddr$prim54354 = alloca %struct.ScmObj*, align 8
%_95k47373 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53335)
store volatile %struct.ScmObj* %_95k47373, %struct.ScmObj** %stackaddr$prim54354, align 8
%stackaddr$prim54355 = alloca %struct.ScmObj*, align 8
%current_45args53336 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53335)
store volatile %struct.ScmObj* %current_45args53336, %struct.ScmObj** %stackaddr$prim54355, align 8
%stackaddr$prim54356 = alloca %struct.ScmObj*, align 8
%anf_45bind47282 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53336)
store volatile %struct.ScmObj* %anf_45bind47282, %struct.ScmObj** %stackaddr$prim54356, align 8
%stackaddr$makeclosure54357 = alloca %struct.ScmObj*, align 8
%fptrToInt54358 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50289 to i64
%ae50289 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54358)
store volatile %struct.ScmObj* %ae50289, %struct.ScmObj** %stackaddr$makeclosure54357, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50289, %struct.ScmObj* %lst47140, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50289, %struct.ScmObj* %v47139, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50289, %struct.ScmObj* %k47372, i64 2)
%stackaddr$makeclosure54359 = alloca %struct.ScmObj*, align 8
%fptrToInt54360 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50290 to i64
%ae50290 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54360)
store volatile %struct.ScmObj* %ae50290, %struct.ScmObj** %stackaddr$makeclosure54359, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50290, %struct.ScmObj* %lst47140, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50290, %struct.ScmObj* %v47139, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50290, %struct.ScmObj* %k47372, i64 2)
%argslist53350$anf_45bind472820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54361 = alloca %struct.ScmObj*, align 8
%argslist53350$anf_45bind472821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50290, %struct.ScmObj* %argslist53350$anf_45bind472820)
store volatile %struct.ScmObj* %argslist53350$anf_45bind472821, %struct.ScmObj** %stackaddr$prim54361, align 8
%stackaddr$prim54362 = alloca %struct.ScmObj*, align 8
%argslist53350$anf_45bind472822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50289, %struct.ScmObj* %argslist53350$anf_45bind472821)
store volatile %struct.ScmObj* %argslist53350$anf_45bind472822, %struct.ScmObj** %stackaddr$prim54362, align 8
%clofunc54363 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47282)
musttail call tailcc void %clofunc54363(%struct.ScmObj* %anf_45bind47282, %struct.ScmObj* %argslist53350$anf_45bind472822)
ret void
}

define tailcc void @proc_clo$ae50289(%struct.ScmObj* %env$ae50289,%struct.ScmObj* %current_45args53338) {
%stackaddr$env-ref54364 = alloca %struct.ScmObj*, align 8
%lst47140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50289, i64 0)
store %struct.ScmObj* %lst47140, %struct.ScmObj** %stackaddr$env-ref54364
%stackaddr$env-ref54365 = alloca %struct.ScmObj*, align 8
%v47139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50289, i64 1)
store %struct.ScmObj* %v47139, %struct.ScmObj** %stackaddr$env-ref54365
%stackaddr$env-ref54366 = alloca %struct.ScmObj*, align 8
%k47372 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50289, i64 2)
store %struct.ScmObj* %k47372, %struct.ScmObj** %stackaddr$env-ref54366
%stackaddr$prim54367 = alloca %struct.ScmObj*, align 8
%_95k47374 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53338)
store volatile %struct.ScmObj* %_95k47374, %struct.ScmObj** %stackaddr$prim54367, align 8
%stackaddr$prim54368 = alloca %struct.ScmObj*, align 8
%current_45args53339 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53338)
store volatile %struct.ScmObj* %current_45args53339, %struct.ScmObj** %stackaddr$prim54368, align 8
%stackaddr$prim54369 = alloca %struct.ScmObj*, align 8
%cc47141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53339)
store volatile %struct.ScmObj* %cc47141, %struct.ScmObj** %stackaddr$prim54369, align 8
%ae50398 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54370 = alloca %struct.ScmObj*, align 8
%anf_45bind47283 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50398)
store volatile %struct.ScmObj* %anf_45bind47283, %struct.ScmObj** %stackaddr$prim54370, align 8
%stackaddr$prim54371 = alloca %struct.ScmObj*, align 8
%anf_45bind47284 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47283)
store volatile %struct.ScmObj* %anf_45bind47284, %struct.ScmObj** %stackaddr$prim54371, align 8
%truthy$cmp54372 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47284)
%cmp$cmp54372 = icmp eq i64 %truthy$cmp54372, 1
br i1 %cmp$cmp54372, label %truebranch$cmp54372, label %falsebranch$cmp54372
truebranch$cmp54372:
%ae50402 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50403 = call %struct.ScmObj* @const_init_false()
%argslist53341$k473720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54373 = alloca %struct.ScmObj*, align 8
%argslist53341$k473721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50403, %struct.ScmObj* %argslist53341$k473720)
store volatile %struct.ScmObj* %argslist53341$k473721, %struct.ScmObj** %stackaddr$prim54373, align 8
%stackaddr$prim54374 = alloca %struct.ScmObj*, align 8
%argslist53341$k473722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50402, %struct.ScmObj* %argslist53341$k473721)
store volatile %struct.ScmObj* %argslist53341$k473722, %struct.ScmObj** %stackaddr$prim54374, align 8
%clofunc54375 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47372)
musttail call tailcc void %clofunc54375(%struct.ScmObj* %k47372, %struct.ScmObj* %argslist53341$k473722)
ret void
falsebranch$cmp54372:
%ae50411 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54376 = alloca %struct.ScmObj*, align 8
%anf_45bind47285 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50411)
store volatile %struct.ScmObj* %anf_45bind47285, %struct.ScmObj** %stackaddr$prim54376, align 8
%stackaddr$prim54377 = alloca %struct.ScmObj*, align 8
%anf_45bind47286 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47285)
store volatile %struct.ScmObj* %anf_45bind47286, %struct.ScmObj** %stackaddr$prim54377, align 8
%stackaddr$prim54378 = alloca %struct.ScmObj*, align 8
%anf_45bind47287 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind47286, %struct.ScmObj* %v47139)
store volatile %struct.ScmObj* %anf_45bind47287, %struct.ScmObj** %stackaddr$prim54378, align 8
%truthy$cmp54379 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47287)
%cmp$cmp54379 = icmp eq i64 %truthy$cmp54379, 1
br i1 %cmp$cmp54379, label %truebranch$cmp54379, label %falsebranch$cmp54379
truebranch$cmp54379:
%ae50417 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54380 = alloca %struct.ScmObj*, align 8
%cpsprim47375 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50417)
store volatile %struct.ScmObj* %cpsprim47375, %struct.ScmObj** %stackaddr$prim54380, align 8
%ae50419 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53342$k473720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54381 = alloca %struct.ScmObj*, align 8
%argslist53342$k473721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47375, %struct.ScmObj* %argslist53342$k473720)
store volatile %struct.ScmObj* %argslist53342$k473721, %struct.ScmObj** %stackaddr$prim54381, align 8
%stackaddr$prim54382 = alloca %struct.ScmObj*, align 8
%argslist53342$k473722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50419, %struct.ScmObj* %argslist53342$k473721)
store volatile %struct.ScmObj* %argslist53342$k473722, %struct.ScmObj** %stackaddr$prim54382, align 8
%clofunc54383 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47372)
musttail call tailcc void %clofunc54383(%struct.ScmObj* %k47372, %struct.ScmObj* %argslist53342$k473722)
ret void
falsebranch$cmp54379:
%ae50430 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54384 = alloca %struct.ScmObj*, align 8
%anf_45bind47288 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50430)
store volatile %struct.ScmObj* %anf_45bind47288, %struct.ScmObj** %stackaddr$prim54384, align 8
%stackaddr$prim54385 = alloca %struct.ScmObj*, align 8
%anf_45bind47289 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47288)
store volatile %struct.ScmObj* %anf_45bind47289, %struct.ScmObj** %stackaddr$prim54385, align 8
%ae50433 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54386 = alloca %struct.ScmObj*, align 8
%_95047143 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50433, %struct.ScmObj* %anf_45bind47289)
store volatile %struct.ScmObj* %_95047143, %struct.ScmObj** %stackaddr$prim54386, align 8
%argslist53343$cc471410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54387 = alloca %struct.ScmObj*, align 8
%argslist53343$cc471411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47141, %struct.ScmObj* %argslist53343$cc471410)
store volatile %struct.ScmObj* %argslist53343$cc471411, %struct.ScmObj** %stackaddr$prim54387, align 8
%stackaddr$prim54388 = alloca %struct.ScmObj*, align 8
%argslist53343$cc471412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47372, %struct.ScmObj* %argslist53343$cc471411)
store volatile %struct.ScmObj* %argslist53343$cc471412, %struct.ScmObj** %stackaddr$prim54388, align 8
%clofunc54389 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47141)
musttail call tailcc void %clofunc54389(%struct.ScmObj* %cc47141, %struct.ScmObj* %argslist53343$cc471412)
ret void
}

define tailcc void @proc_clo$ae50290(%struct.ScmObj* %env$ae50290,%struct.ScmObj* %current_45args53344) {
%stackaddr$env-ref54390 = alloca %struct.ScmObj*, align 8
%lst47140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50290, i64 0)
store %struct.ScmObj* %lst47140, %struct.ScmObj** %stackaddr$env-ref54390
%stackaddr$env-ref54391 = alloca %struct.ScmObj*, align 8
%v47139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50290, i64 1)
store %struct.ScmObj* %v47139, %struct.ScmObj** %stackaddr$env-ref54391
%stackaddr$env-ref54392 = alloca %struct.ScmObj*, align 8
%k47372 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50290, i64 2)
store %struct.ScmObj* %k47372, %struct.ScmObj** %stackaddr$env-ref54392
%stackaddr$prim54393 = alloca %struct.ScmObj*, align 8
%_95k47374 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53344)
store volatile %struct.ScmObj* %_95k47374, %struct.ScmObj** %stackaddr$prim54393, align 8
%stackaddr$prim54394 = alloca %struct.ScmObj*, align 8
%current_45args53345 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53344)
store volatile %struct.ScmObj* %current_45args53345, %struct.ScmObj** %stackaddr$prim54394, align 8
%stackaddr$prim54395 = alloca %struct.ScmObj*, align 8
%cc47141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53345)
store volatile %struct.ScmObj* %cc47141, %struct.ScmObj** %stackaddr$prim54395, align 8
%ae50292 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54396 = alloca %struct.ScmObj*, align 8
%anf_45bind47283 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50292)
store volatile %struct.ScmObj* %anf_45bind47283, %struct.ScmObj** %stackaddr$prim54396, align 8
%stackaddr$prim54397 = alloca %struct.ScmObj*, align 8
%anf_45bind47284 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47283)
store volatile %struct.ScmObj* %anf_45bind47284, %struct.ScmObj** %stackaddr$prim54397, align 8
%truthy$cmp54398 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47284)
%cmp$cmp54398 = icmp eq i64 %truthy$cmp54398, 1
br i1 %cmp$cmp54398, label %truebranch$cmp54398, label %falsebranch$cmp54398
truebranch$cmp54398:
%ae50296 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50297 = call %struct.ScmObj* @const_init_false()
%argslist53347$k473720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54399 = alloca %struct.ScmObj*, align 8
%argslist53347$k473721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50297, %struct.ScmObj* %argslist53347$k473720)
store volatile %struct.ScmObj* %argslist53347$k473721, %struct.ScmObj** %stackaddr$prim54399, align 8
%stackaddr$prim54400 = alloca %struct.ScmObj*, align 8
%argslist53347$k473722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50296, %struct.ScmObj* %argslist53347$k473721)
store volatile %struct.ScmObj* %argslist53347$k473722, %struct.ScmObj** %stackaddr$prim54400, align 8
%clofunc54401 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47372)
musttail call tailcc void %clofunc54401(%struct.ScmObj* %k47372, %struct.ScmObj* %argslist53347$k473722)
ret void
falsebranch$cmp54398:
%ae50305 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54402 = alloca %struct.ScmObj*, align 8
%anf_45bind47285 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50305)
store volatile %struct.ScmObj* %anf_45bind47285, %struct.ScmObj** %stackaddr$prim54402, align 8
%stackaddr$prim54403 = alloca %struct.ScmObj*, align 8
%anf_45bind47286 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47285)
store volatile %struct.ScmObj* %anf_45bind47286, %struct.ScmObj** %stackaddr$prim54403, align 8
%stackaddr$prim54404 = alloca %struct.ScmObj*, align 8
%anf_45bind47287 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind47286, %struct.ScmObj* %v47139)
store volatile %struct.ScmObj* %anf_45bind47287, %struct.ScmObj** %stackaddr$prim54404, align 8
%truthy$cmp54405 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47287)
%cmp$cmp54405 = icmp eq i64 %truthy$cmp54405, 1
br i1 %cmp$cmp54405, label %truebranch$cmp54405, label %falsebranch$cmp54405
truebranch$cmp54405:
%ae50311 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54406 = alloca %struct.ScmObj*, align 8
%cpsprim47375 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50311)
store volatile %struct.ScmObj* %cpsprim47375, %struct.ScmObj** %stackaddr$prim54406, align 8
%ae50313 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53348$k473720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54407 = alloca %struct.ScmObj*, align 8
%argslist53348$k473721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47375, %struct.ScmObj* %argslist53348$k473720)
store volatile %struct.ScmObj* %argslist53348$k473721, %struct.ScmObj** %stackaddr$prim54407, align 8
%stackaddr$prim54408 = alloca %struct.ScmObj*, align 8
%argslist53348$k473722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50313, %struct.ScmObj* %argslist53348$k473721)
store volatile %struct.ScmObj* %argslist53348$k473722, %struct.ScmObj** %stackaddr$prim54408, align 8
%clofunc54409 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47372)
musttail call tailcc void %clofunc54409(%struct.ScmObj* %k47372, %struct.ScmObj* %argslist53348$k473722)
ret void
falsebranch$cmp54405:
%ae50324 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54410 = alloca %struct.ScmObj*, align 8
%anf_45bind47288 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50324)
store volatile %struct.ScmObj* %anf_45bind47288, %struct.ScmObj** %stackaddr$prim54410, align 8
%stackaddr$prim54411 = alloca %struct.ScmObj*, align 8
%anf_45bind47289 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47288)
store volatile %struct.ScmObj* %anf_45bind47289, %struct.ScmObj** %stackaddr$prim54411, align 8
%ae50327 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54412 = alloca %struct.ScmObj*, align 8
%_95047143 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50327, %struct.ScmObj* %anf_45bind47289)
store volatile %struct.ScmObj* %_95047143, %struct.ScmObj** %stackaddr$prim54412, align 8
%argslist53349$cc471410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54413 = alloca %struct.ScmObj*, align 8
%argslist53349$cc471411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47141, %struct.ScmObj* %argslist53349$cc471410)
store volatile %struct.ScmObj* %argslist53349$cc471411, %struct.ScmObj** %stackaddr$prim54413, align 8
%stackaddr$prim54414 = alloca %struct.ScmObj*, align 8
%argslist53349$cc471412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47372, %struct.ScmObj* %argslist53349$cc471411)
store volatile %struct.ScmObj* %argslist53349$cc471412, %struct.ScmObj** %stackaddr$prim54414, align 8
%clofunc54415 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47141)
musttail call tailcc void %clofunc54415(%struct.ScmObj* %cc47141, %struct.ScmObj* %argslist53349$cc471412)
ret void
}

define tailcc void @proc_clo$ae50275(%struct.ScmObj* %env$ae50275,%struct.ScmObj* %current_45args53351) {
%stackaddr$prim54416 = alloca %struct.ScmObj*, align 8
%k47376 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53351)
store volatile %struct.ScmObj* %k47376, %struct.ScmObj** %stackaddr$prim54416, align 8
%stackaddr$prim54417 = alloca %struct.ScmObj*, align 8
%current_45args53352 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53351)
store volatile %struct.ScmObj* %current_45args53352, %struct.ScmObj** %stackaddr$prim54417, align 8
%stackaddr$prim54418 = alloca %struct.ScmObj*, align 8
%u47142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53352)
store volatile %struct.ScmObj* %u47142, %struct.ScmObj** %stackaddr$prim54418, align 8
%argslist53354$u471420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54419 = alloca %struct.ScmObj*, align 8
%argslist53354$u471421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u47142, %struct.ScmObj* %argslist53354$u471420)
store volatile %struct.ScmObj* %argslist53354$u471421, %struct.ScmObj** %stackaddr$prim54419, align 8
%stackaddr$prim54420 = alloca %struct.ScmObj*, align 8
%argslist53354$u471422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47376, %struct.ScmObj* %argslist53354$u471421)
store volatile %struct.ScmObj* %argslist53354$u471422, %struct.ScmObj** %stackaddr$prim54420, align 8
%clofunc54421 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u47142)
musttail call tailcc void %clofunc54421(%struct.ScmObj* %u47142, %struct.ScmObj* %argslist53354$u471422)
ret void
}

define tailcc void @proc_clo$ae49734(%struct.ScmObj* %env$ae49734,%struct.ScmObj* %current_45args53357) {
%stackaddr$prim54422 = alloca %struct.ScmObj*, align 8
%k47377 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53357)
store volatile %struct.ScmObj* %k47377, %struct.ScmObj** %stackaddr$prim54422, align 8
%stackaddr$prim54423 = alloca %struct.ScmObj*, align 8
%current_45args53358 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53357)
store volatile %struct.ScmObj* %current_45args53358, %struct.ScmObj** %stackaddr$prim54423, align 8
%stackaddr$prim54424 = alloca %struct.ScmObj*, align 8
%lst47146 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53358)
store volatile %struct.ScmObj* %lst47146, %struct.ScmObj** %stackaddr$prim54424, align 8
%stackaddr$prim54425 = alloca %struct.ScmObj*, align 8
%current_45args53359 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53358)
store volatile %struct.ScmObj* %current_45args53359, %struct.ScmObj** %stackaddr$prim54425, align 8
%stackaddr$prim54426 = alloca %struct.ScmObj*, align 8
%n47145 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53359)
store volatile %struct.ScmObj* %n47145, %struct.ScmObj** %stackaddr$prim54426, align 8
%ae49735 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54427 = alloca %struct.ScmObj*, align 8
%n47148 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49735, %struct.ScmObj* %n47145)
store volatile %struct.ScmObj* %n47148, %struct.ScmObj** %stackaddr$prim54427, align 8
%ae49737 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54428 = alloca %struct.ScmObj*, align 8
%lst47147 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49737, %struct.ScmObj* %lst47146)
store volatile %struct.ScmObj* %lst47147, %struct.ScmObj** %stackaddr$prim54428, align 8
%stackaddr$makeclosure54429 = alloca %struct.ScmObj*, align 8
%fptrToInt54430 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49739 to i64
%ae49739 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54430)
store volatile %struct.ScmObj* %ae49739, %struct.ScmObj** %stackaddr$makeclosure54429, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49739, %struct.ScmObj* %k47377, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49739, %struct.ScmObj* %n47148, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49739, %struct.ScmObj* %lst47147, i64 2)
%ae49740 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54431 = alloca %struct.ScmObj*, align 8
%fptrToInt54432 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49741 to i64
%ae49741 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54432)
store volatile %struct.ScmObj* %ae49741, %struct.ScmObj** %stackaddr$makeclosure54431, align 8
%argslist53379$ae497390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54433 = alloca %struct.ScmObj*, align 8
%argslist53379$ae497391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49741, %struct.ScmObj* %argslist53379$ae497390)
store volatile %struct.ScmObj* %argslist53379$ae497391, %struct.ScmObj** %stackaddr$prim54433, align 8
%stackaddr$prim54434 = alloca %struct.ScmObj*, align 8
%argslist53379$ae497392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49740, %struct.ScmObj* %argslist53379$ae497391)
store volatile %struct.ScmObj* %argslist53379$ae497392, %struct.ScmObj** %stackaddr$prim54434, align 8
%clofunc54435 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49739)
musttail call tailcc void %clofunc54435(%struct.ScmObj* %ae49739, %struct.ScmObj* %argslist53379$ae497392)
ret void
}

define tailcc void @proc_clo$ae49739(%struct.ScmObj* %env$ae49739,%struct.ScmObj* %current_45args53361) {
%stackaddr$env-ref54436 = alloca %struct.ScmObj*, align 8
%k47377 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49739, i64 0)
store %struct.ScmObj* %k47377, %struct.ScmObj** %stackaddr$env-ref54436
%stackaddr$env-ref54437 = alloca %struct.ScmObj*, align 8
%n47148 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49739, i64 1)
store %struct.ScmObj* %n47148, %struct.ScmObj** %stackaddr$env-ref54437
%stackaddr$env-ref54438 = alloca %struct.ScmObj*, align 8
%lst47147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49739, i64 2)
store %struct.ScmObj* %lst47147, %struct.ScmObj** %stackaddr$env-ref54438
%stackaddr$prim54439 = alloca %struct.ScmObj*, align 8
%_95k47378 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53361)
store volatile %struct.ScmObj* %_95k47378, %struct.ScmObj** %stackaddr$prim54439, align 8
%stackaddr$prim54440 = alloca %struct.ScmObj*, align 8
%current_45args53362 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53361)
store volatile %struct.ScmObj* %current_45args53362, %struct.ScmObj** %stackaddr$prim54440, align 8
%stackaddr$prim54441 = alloca %struct.ScmObj*, align 8
%anf_45bind47275 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53362)
store volatile %struct.ScmObj* %anf_45bind47275, %struct.ScmObj** %stackaddr$prim54441, align 8
%stackaddr$makeclosure54442 = alloca %struct.ScmObj*, align 8
%fptrToInt54443 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49755 to i64
%ae49755 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54443)
store volatile %struct.ScmObj* %ae49755, %struct.ScmObj** %stackaddr$makeclosure54442, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49755, %struct.ScmObj* %k47377, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49755, %struct.ScmObj* %n47148, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49755, %struct.ScmObj* %lst47147, i64 2)
%stackaddr$makeclosure54444 = alloca %struct.ScmObj*, align 8
%fptrToInt54445 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49756 to i64
%ae49756 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54445)
store volatile %struct.ScmObj* %ae49756, %struct.ScmObj** %stackaddr$makeclosure54444, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49756, %struct.ScmObj* %k47377, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49756, %struct.ScmObj* %n47148, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49756, %struct.ScmObj* %lst47147, i64 2)
%argslist53374$anf_45bind472750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54446 = alloca %struct.ScmObj*, align 8
%argslist53374$anf_45bind472751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49756, %struct.ScmObj* %argslist53374$anf_45bind472750)
store volatile %struct.ScmObj* %argslist53374$anf_45bind472751, %struct.ScmObj** %stackaddr$prim54446, align 8
%stackaddr$prim54447 = alloca %struct.ScmObj*, align 8
%argslist53374$anf_45bind472752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49755, %struct.ScmObj* %argslist53374$anf_45bind472751)
store volatile %struct.ScmObj* %argslist53374$anf_45bind472752, %struct.ScmObj** %stackaddr$prim54447, align 8
%clofunc54448 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47275)
musttail call tailcc void %clofunc54448(%struct.ScmObj* %anf_45bind47275, %struct.ScmObj* %argslist53374$anf_45bind472752)
ret void
}

define tailcc void @proc_clo$ae49755(%struct.ScmObj* %env$ae49755,%struct.ScmObj* %current_45args53364) {
%stackaddr$env-ref54449 = alloca %struct.ScmObj*, align 8
%k47377 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49755, i64 0)
store %struct.ScmObj* %k47377, %struct.ScmObj** %stackaddr$env-ref54449
%stackaddr$env-ref54450 = alloca %struct.ScmObj*, align 8
%n47148 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49755, i64 1)
store %struct.ScmObj* %n47148, %struct.ScmObj** %stackaddr$env-ref54450
%stackaddr$env-ref54451 = alloca %struct.ScmObj*, align 8
%lst47147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49755, i64 2)
store %struct.ScmObj* %lst47147, %struct.ScmObj** %stackaddr$env-ref54451
%stackaddr$prim54452 = alloca %struct.ScmObj*, align 8
%_95k47379 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53364)
store volatile %struct.ScmObj* %_95k47379, %struct.ScmObj** %stackaddr$prim54452, align 8
%stackaddr$prim54453 = alloca %struct.ScmObj*, align 8
%current_45args53365 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53364)
store volatile %struct.ScmObj* %current_45args53365, %struct.ScmObj** %stackaddr$prim54453, align 8
%stackaddr$prim54454 = alloca %struct.ScmObj*, align 8
%cc47149 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53365)
store volatile %struct.ScmObj* %cc47149, %struct.ScmObj** %stackaddr$prim54454, align 8
%ae49898 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54455 = alloca %struct.ScmObj*, align 8
%anf_45bind47276 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47148, %struct.ScmObj* %ae49898)
store volatile %struct.ScmObj* %anf_45bind47276, %struct.ScmObj** %stackaddr$prim54455, align 8
%ae49899 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54456 = alloca %struct.ScmObj*, align 8
%anf_45bind47277 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae49899, %struct.ScmObj* %anf_45bind47276)
store volatile %struct.ScmObj* %anf_45bind47277, %struct.ScmObj** %stackaddr$prim54456, align 8
%truthy$cmp54457 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47277)
%cmp$cmp54457 = icmp eq i64 %truthy$cmp54457, 1
br i1 %cmp$cmp54457, label %truebranch$cmp54457, label %falsebranch$cmp54457
truebranch$cmp54457:
%ae49903 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54458 = alloca %struct.ScmObj*, align 8
%cpsprim47380 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47147, %struct.ScmObj* %ae49903)
store volatile %struct.ScmObj* %cpsprim47380, %struct.ScmObj** %stackaddr$prim54458, align 8
%ae49905 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53367$k473770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54459 = alloca %struct.ScmObj*, align 8
%argslist53367$k473771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47380, %struct.ScmObj* %argslist53367$k473770)
store volatile %struct.ScmObj* %argslist53367$k473771, %struct.ScmObj** %stackaddr$prim54459, align 8
%stackaddr$prim54460 = alloca %struct.ScmObj*, align 8
%argslist53367$k473772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49905, %struct.ScmObj* %argslist53367$k473771)
store volatile %struct.ScmObj* %argslist53367$k473772, %struct.ScmObj** %stackaddr$prim54460, align 8
%clofunc54461 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47377)
musttail call tailcc void %clofunc54461(%struct.ScmObj* %k47377, %struct.ScmObj* %argslist53367$k473772)
ret void
falsebranch$cmp54457:
%ae49916 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54462 = alloca %struct.ScmObj*, align 8
%anf_45bind47278 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47147, %struct.ScmObj* %ae49916)
store volatile %struct.ScmObj* %anf_45bind47278, %struct.ScmObj** %stackaddr$prim54462, align 8
%stackaddr$prim54463 = alloca %struct.ScmObj*, align 8
%anf_45bind47279 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47278)
store volatile %struct.ScmObj* %anf_45bind47279, %struct.ScmObj** %stackaddr$prim54463, align 8
%ae49919 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54464 = alloca %struct.ScmObj*, align 8
%_95047152 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47147, %struct.ScmObj* %ae49919, %struct.ScmObj* %anf_45bind47279)
store volatile %struct.ScmObj* %_95047152, %struct.ScmObj** %stackaddr$prim54464, align 8
%ae49922 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54465 = alloca %struct.ScmObj*, align 8
%anf_45bind47280 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47148, %struct.ScmObj* %ae49922)
store volatile %struct.ScmObj* %anf_45bind47280, %struct.ScmObj** %stackaddr$prim54465, align 8
%ae49924 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54466 = alloca %struct.ScmObj*, align 8
%anf_45bind47281 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind47280, %struct.ScmObj* %ae49924)
store volatile %struct.ScmObj* %anf_45bind47281, %struct.ScmObj** %stackaddr$prim54466, align 8
%ae49926 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54467 = alloca %struct.ScmObj*, align 8
%_95147151 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n47148, %struct.ScmObj* %ae49926, %struct.ScmObj* %anf_45bind47281)
store volatile %struct.ScmObj* %_95147151, %struct.ScmObj** %stackaddr$prim54467, align 8
%argslist53368$cc471490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54468 = alloca %struct.ScmObj*, align 8
%argslist53368$cc471491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47149, %struct.ScmObj* %argslist53368$cc471490)
store volatile %struct.ScmObj* %argslist53368$cc471491, %struct.ScmObj** %stackaddr$prim54468, align 8
%stackaddr$prim54469 = alloca %struct.ScmObj*, align 8
%argslist53368$cc471492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47377, %struct.ScmObj* %argslist53368$cc471491)
store volatile %struct.ScmObj* %argslist53368$cc471492, %struct.ScmObj** %stackaddr$prim54469, align 8
%clofunc54470 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47149)
musttail call tailcc void %clofunc54470(%struct.ScmObj* %cc47149, %struct.ScmObj* %argslist53368$cc471492)
ret void
}

define tailcc void @proc_clo$ae49756(%struct.ScmObj* %env$ae49756,%struct.ScmObj* %current_45args53369) {
%stackaddr$env-ref54471 = alloca %struct.ScmObj*, align 8
%k47377 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49756, i64 0)
store %struct.ScmObj* %k47377, %struct.ScmObj** %stackaddr$env-ref54471
%stackaddr$env-ref54472 = alloca %struct.ScmObj*, align 8
%n47148 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49756, i64 1)
store %struct.ScmObj* %n47148, %struct.ScmObj** %stackaddr$env-ref54472
%stackaddr$env-ref54473 = alloca %struct.ScmObj*, align 8
%lst47147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49756, i64 2)
store %struct.ScmObj* %lst47147, %struct.ScmObj** %stackaddr$env-ref54473
%stackaddr$prim54474 = alloca %struct.ScmObj*, align 8
%_95k47379 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53369)
store volatile %struct.ScmObj* %_95k47379, %struct.ScmObj** %stackaddr$prim54474, align 8
%stackaddr$prim54475 = alloca %struct.ScmObj*, align 8
%current_45args53370 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53369)
store volatile %struct.ScmObj* %current_45args53370, %struct.ScmObj** %stackaddr$prim54475, align 8
%stackaddr$prim54476 = alloca %struct.ScmObj*, align 8
%cc47149 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53370)
store volatile %struct.ScmObj* %cc47149, %struct.ScmObj** %stackaddr$prim54476, align 8
%ae49758 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54477 = alloca %struct.ScmObj*, align 8
%anf_45bind47276 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47148, %struct.ScmObj* %ae49758)
store volatile %struct.ScmObj* %anf_45bind47276, %struct.ScmObj** %stackaddr$prim54477, align 8
%ae49759 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54478 = alloca %struct.ScmObj*, align 8
%anf_45bind47277 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae49759, %struct.ScmObj* %anf_45bind47276)
store volatile %struct.ScmObj* %anf_45bind47277, %struct.ScmObj** %stackaddr$prim54478, align 8
%truthy$cmp54479 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47277)
%cmp$cmp54479 = icmp eq i64 %truthy$cmp54479, 1
br i1 %cmp$cmp54479, label %truebranch$cmp54479, label %falsebranch$cmp54479
truebranch$cmp54479:
%ae49763 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54480 = alloca %struct.ScmObj*, align 8
%cpsprim47380 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47147, %struct.ScmObj* %ae49763)
store volatile %struct.ScmObj* %cpsprim47380, %struct.ScmObj** %stackaddr$prim54480, align 8
%ae49765 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53372$k473770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54481 = alloca %struct.ScmObj*, align 8
%argslist53372$k473771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47380, %struct.ScmObj* %argslist53372$k473770)
store volatile %struct.ScmObj* %argslist53372$k473771, %struct.ScmObj** %stackaddr$prim54481, align 8
%stackaddr$prim54482 = alloca %struct.ScmObj*, align 8
%argslist53372$k473772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49765, %struct.ScmObj* %argslist53372$k473771)
store volatile %struct.ScmObj* %argslist53372$k473772, %struct.ScmObj** %stackaddr$prim54482, align 8
%clofunc54483 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47377)
musttail call tailcc void %clofunc54483(%struct.ScmObj* %k47377, %struct.ScmObj* %argslist53372$k473772)
ret void
falsebranch$cmp54479:
%ae49776 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54484 = alloca %struct.ScmObj*, align 8
%anf_45bind47278 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47147, %struct.ScmObj* %ae49776)
store volatile %struct.ScmObj* %anf_45bind47278, %struct.ScmObj** %stackaddr$prim54484, align 8
%stackaddr$prim54485 = alloca %struct.ScmObj*, align 8
%anf_45bind47279 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47278)
store volatile %struct.ScmObj* %anf_45bind47279, %struct.ScmObj** %stackaddr$prim54485, align 8
%ae49779 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54486 = alloca %struct.ScmObj*, align 8
%_95047152 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47147, %struct.ScmObj* %ae49779, %struct.ScmObj* %anf_45bind47279)
store volatile %struct.ScmObj* %_95047152, %struct.ScmObj** %stackaddr$prim54486, align 8
%ae49782 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54487 = alloca %struct.ScmObj*, align 8
%anf_45bind47280 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47148, %struct.ScmObj* %ae49782)
store volatile %struct.ScmObj* %anf_45bind47280, %struct.ScmObj** %stackaddr$prim54487, align 8
%ae49784 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54488 = alloca %struct.ScmObj*, align 8
%anf_45bind47281 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind47280, %struct.ScmObj* %ae49784)
store volatile %struct.ScmObj* %anf_45bind47281, %struct.ScmObj** %stackaddr$prim54488, align 8
%ae49786 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54489 = alloca %struct.ScmObj*, align 8
%_95147151 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n47148, %struct.ScmObj* %ae49786, %struct.ScmObj* %anf_45bind47281)
store volatile %struct.ScmObj* %_95147151, %struct.ScmObj** %stackaddr$prim54489, align 8
%argslist53373$cc471490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54490 = alloca %struct.ScmObj*, align 8
%argslist53373$cc471491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47149, %struct.ScmObj* %argslist53373$cc471490)
store volatile %struct.ScmObj* %argslist53373$cc471491, %struct.ScmObj** %stackaddr$prim54490, align 8
%stackaddr$prim54491 = alloca %struct.ScmObj*, align 8
%argslist53373$cc471492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47377, %struct.ScmObj* %argslist53373$cc471491)
store volatile %struct.ScmObj* %argslist53373$cc471492, %struct.ScmObj** %stackaddr$prim54491, align 8
%clofunc54492 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47149)
musttail call tailcc void %clofunc54492(%struct.ScmObj* %cc47149, %struct.ScmObj* %argslist53373$cc471492)
ret void
}

define tailcc void @proc_clo$ae49741(%struct.ScmObj* %env$ae49741,%struct.ScmObj* %current_45args53375) {
%stackaddr$prim54493 = alloca %struct.ScmObj*, align 8
%k47381 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53375)
store volatile %struct.ScmObj* %k47381, %struct.ScmObj** %stackaddr$prim54493, align 8
%stackaddr$prim54494 = alloca %struct.ScmObj*, align 8
%current_45args53376 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53375)
store volatile %struct.ScmObj* %current_45args53376, %struct.ScmObj** %stackaddr$prim54494, align 8
%stackaddr$prim54495 = alloca %struct.ScmObj*, align 8
%u47150 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53376)
store volatile %struct.ScmObj* %u47150, %struct.ScmObj** %stackaddr$prim54495, align 8
%argslist53378$u471500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54496 = alloca %struct.ScmObj*, align 8
%argslist53378$u471501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u47150, %struct.ScmObj* %argslist53378$u471500)
store volatile %struct.ScmObj* %argslist53378$u471501, %struct.ScmObj** %stackaddr$prim54496, align 8
%stackaddr$prim54497 = alloca %struct.ScmObj*, align 8
%argslist53378$u471502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47381, %struct.ScmObj* %argslist53378$u471501)
store volatile %struct.ScmObj* %argslist53378$u471502, %struct.ScmObj** %stackaddr$prim54497, align 8
%clofunc54498 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u47150)
musttail call tailcc void %clofunc54498(%struct.ScmObj* %u47150, %struct.ScmObj* %argslist53378$u471502)
ret void
}

define tailcc void @proc_clo$ae49318(%struct.ScmObj* %env$ae49318,%struct.ScmObj* %current_45args53381) {
%stackaddr$prim54499 = alloca %struct.ScmObj*, align 8
%k47382 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53381)
store volatile %struct.ScmObj* %k47382, %struct.ScmObj** %stackaddr$prim54499, align 8
%stackaddr$prim54500 = alloca %struct.ScmObj*, align 8
%current_45args53382 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53381)
store volatile %struct.ScmObj* %current_45args53382, %struct.ScmObj** %stackaddr$prim54500, align 8
%stackaddr$prim54501 = alloca %struct.ScmObj*, align 8
%a47154 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53382)
store volatile %struct.ScmObj* %a47154, %struct.ScmObj** %stackaddr$prim54501, align 8
%ae49319 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54502 = alloca %struct.ScmObj*, align 8
%a47155 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49319, %struct.ScmObj* %a47154)
store volatile %struct.ScmObj* %a47155, %struct.ScmObj** %stackaddr$prim54502, align 8
%stackaddr$makeclosure54503 = alloca %struct.ScmObj*, align 8
%fptrToInt54504 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49321 to i64
%ae49321 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54504)
store volatile %struct.ScmObj* %ae49321, %struct.ScmObj** %stackaddr$makeclosure54503, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49321, %struct.ScmObj* %k47382, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49321, %struct.ScmObj* %a47155, i64 1)
%ae49322 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54505 = alloca %struct.ScmObj*, align 8
%fptrToInt54506 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49323 to i64
%ae49323 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54506)
store volatile %struct.ScmObj* %ae49323, %struct.ScmObj** %stackaddr$makeclosure54505, align 8
%argslist53404$ae493210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54507 = alloca %struct.ScmObj*, align 8
%argslist53404$ae493211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49323, %struct.ScmObj* %argslist53404$ae493210)
store volatile %struct.ScmObj* %argslist53404$ae493211, %struct.ScmObj** %stackaddr$prim54507, align 8
%stackaddr$prim54508 = alloca %struct.ScmObj*, align 8
%argslist53404$ae493212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49322, %struct.ScmObj* %argslist53404$ae493211)
store volatile %struct.ScmObj* %argslist53404$ae493212, %struct.ScmObj** %stackaddr$prim54508, align 8
%clofunc54509 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49321)
musttail call tailcc void %clofunc54509(%struct.ScmObj* %ae49321, %struct.ScmObj* %argslist53404$ae493212)
ret void
}

define tailcc void @proc_clo$ae49321(%struct.ScmObj* %env$ae49321,%struct.ScmObj* %current_45args53384) {
%stackaddr$env-ref54510 = alloca %struct.ScmObj*, align 8
%k47382 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49321, i64 0)
store %struct.ScmObj* %k47382, %struct.ScmObj** %stackaddr$env-ref54510
%stackaddr$env-ref54511 = alloca %struct.ScmObj*, align 8
%a47155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49321, i64 1)
store %struct.ScmObj* %a47155, %struct.ScmObj** %stackaddr$env-ref54511
%stackaddr$prim54512 = alloca %struct.ScmObj*, align 8
%_95k47383 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53384)
store volatile %struct.ScmObj* %_95k47383, %struct.ScmObj** %stackaddr$prim54512, align 8
%stackaddr$prim54513 = alloca %struct.ScmObj*, align 8
%current_45args53385 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53384)
store volatile %struct.ScmObj* %current_45args53385, %struct.ScmObj** %stackaddr$prim54513, align 8
%stackaddr$prim54514 = alloca %struct.ScmObj*, align 8
%anf_45bind47267 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53385)
store volatile %struct.ScmObj* %anf_45bind47267, %struct.ScmObj** %stackaddr$prim54514, align 8
%stackaddr$makeclosure54515 = alloca %struct.ScmObj*, align 8
%fptrToInt54516 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49340 to i64
%ae49340 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54516)
store volatile %struct.ScmObj* %ae49340, %struct.ScmObj** %stackaddr$makeclosure54515, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49340, %struct.ScmObj* %k47382, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49340, %struct.ScmObj* %a47155, i64 1)
%stackaddr$makeclosure54517 = alloca %struct.ScmObj*, align 8
%fptrToInt54518 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49341 to i64
%ae49341 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54518)
store volatile %struct.ScmObj* %ae49341, %struct.ScmObj** %stackaddr$makeclosure54517, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49341, %struct.ScmObj* %k47382, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49341, %struct.ScmObj* %a47155, i64 1)
%argslist53399$anf_45bind472670 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54519 = alloca %struct.ScmObj*, align 8
%argslist53399$anf_45bind472671 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49341, %struct.ScmObj* %argslist53399$anf_45bind472670)
store volatile %struct.ScmObj* %argslist53399$anf_45bind472671, %struct.ScmObj** %stackaddr$prim54519, align 8
%stackaddr$prim54520 = alloca %struct.ScmObj*, align 8
%argslist53399$anf_45bind472672 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49340, %struct.ScmObj* %argslist53399$anf_45bind472671)
store volatile %struct.ScmObj* %argslist53399$anf_45bind472672, %struct.ScmObj** %stackaddr$prim54520, align 8
%clofunc54521 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47267)
musttail call tailcc void %clofunc54521(%struct.ScmObj* %anf_45bind47267, %struct.ScmObj* %argslist53399$anf_45bind472672)
ret void
}

define tailcc void @proc_clo$ae49340(%struct.ScmObj* %env$ae49340,%struct.ScmObj* %current_45args53387) {
%stackaddr$env-ref54522 = alloca %struct.ScmObj*, align 8
%k47382 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49340, i64 0)
store %struct.ScmObj* %k47382, %struct.ScmObj** %stackaddr$env-ref54522
%stackaddr$env-ref54523 = alloca %struct.ScmObj*, align 8
%a47155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49340, i64 1)
store %struct.ScmObj* %a47155, %struct.ScmObj** %stackaddr$env-ref54523
%stackaddr$prim54524 = alloca %struct.ScmObj*, align 8
%_95k47384 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53387)
store volatile %struct.ScmObj* %_95k47384, %struct.ScmObj** %stackaddr$prim54524, align 8
%stackaddr$prim54525 = alloca %struct.ScmObj*, align 8
%current_45args53388 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53387)
store volatile %struct.ScmObj* %current_45args53388, %struct.ScmObj** %stackaddr$prim54525, align 8
%stackaddr$prim54526 = alloca %struct.ScmObj*, align 8
%cc47156 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53388)
store volatile %struct.ScmObj* %cc47156, %struct.ScmObj** %stackaddr$prim54526, align 8
%ae49456 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54527 = alloca %struct.ScmObj*, align 8
%anf_45bind47268 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49456)
store volatile %struct.ScmObj* %anf_45bind47268, %struct.ScmObj** %stackaddr$prim54527, align 8
%stackaddr$prim54528 = alloca %struct.ScmObj*, align 8
%anf_45bind47269 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47268)
store volatile %struct.ScmObj* %anf_45bind47269, %struct.ScmObj** %stackaddr$prim54528, align 8
%truthy$cmp54529 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47269)
%cmp$cmp54529 = icmp eq i64 %truthy$cmp54529, 1
br i1 %cmp$cmp54529, label %truebranch$cmp54529, label %falsebranch$cmp54529
truebranch$cmp54529:
%ae49460 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49461 = call %struct.ScmObj* @const_init_true()
%argslist53390$k473820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54530 = alloca %struct.ScmObj*, align 8
%argslist53390$k473821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49461, %struct.ScmObj* %argslist53390$k473820)
store volatile %struct.ScmObj* %argslist53390$k473821, %struct.ScmObj** %stackaddr$prim54530, align 8
%stackaddr$prim54531 = alloca %struct.ScmObj*, align 8
%argslist53390$k473822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49460, %struct.ScmObj* %argslist53390$k473821)
store volatile %struct.ScmObj* %argslist53390$k473822, %struct.ScmObj** %stackaddr$prim54531, align 8
%clofunc54532 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47382)
musttail call tailcc void %clofunc54532(%struct.ScmObj* %k47382, %struct.ScmObj* %argslist53390$k473822)
ret void
falsebranch$cmp54529:
%ae49469 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54533 = alloca %struct.ScmObj*, align 8
%anf_45bind47270 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49469)
store volatile %struct.ScmObj* %anf_45bind47270, %struct.ScmObj** %stackaddr$prim54533, align 8
%stackaddr$prim54534 = alloca %struct.ScmObj*, align 8
%anf_45bind47271 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind47270)
store volatile %struct.ScmObj* %anf_45bind47271, %struct.ScmObj** %stackaddr$prim54534, align 8
%truthy$cmp54535 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47271)
%cmp$cmp54535 = icmp eq i64 %truthy$cmp54535, 1
br i1 %cmp$cmp54535, label %truebranch$cmp54535, label %falsebranch$cmp54535
truebranch$cmp54535:
%ae49473 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54536 = alloca %struct.ScmObj*, align 8
%anf_45bind47272 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49473)
store volatile %struct.ScmObj* %anf_45bind47272, %struct.ScmObj** %stackaddr$prim54536, align 8
%stackaddr$prim54537 = alloca %struct.ScmObj*, align 8
%b47158 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47272)
store volatile %struct.ScmObj* %b47158, %struct.ScmObj** %stackaddr$prim54537, align 8
%ae49476 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54538 = alloca %struct.ScmObj*, align 8
%anf_45bind47273 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49476)
store volatile %struct.ScmObj* %anf_45bind47273, %struct.ScmObj** %stackaddr$prim54538, align 8
%stackaddr$prim54539 = alloca %struct.ScmObj*, align 8
%anf_45bind47274 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47273)
store volatile %struct.ScmObj* %anf_45bind47274, %struct.ScmObj** %stackaddr$prim54539, align 8
%ae49479 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54540 = alloca %struct.ScmObj*, align 8
%_95047159 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49479, %struct.ScmObj* %anf_45bind47274)
store volatile %struct.ScmObj* %_95047159, %struct.ScmObj** %stackaddr$prim54540, align 8
%argslist53391$cc471560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54541 = alloca %struct.ScmObj*, align 8
%argslist53391$cc471561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47156, %struct.ScmObj* %argslist53391$cc471560)
store volatile %struct.ScmObj* %argslist53391$cc471561, %struct.ScmObj** %stackaddr$prim54541, align 8
%stackaddr$prim54542 = alloca %struct.ScmObj*, align 8
%argslist53391$cc471562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47382, %struct.ScmObj* %argslist53391$cc471561)
store volatile %struct.ScmObj* %argslist53391$cc471562, %struct.ScmObj** %stackaddr$prim54542, align 8
%clofunc54543 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47156)
musttail call tailcc void %clofunc54543(%struct.ScmObj* %cc47156, %struct.ScmObj* %argslist53391$cc471562)
ret void
falsebranch$cmp54535:
%ae49512 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49513 = call %struct.ScmObj* @const_init_false()
%argslist53392$k473820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54544 = alloca %struct.ScmObj*, align 8
%argslist53392$k473821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49513, %struct.ScmObj* %argslist53392$k473820)
store volatile %struct.ScmObj* %argslist53392$k473821, %struct.ScmObj** %stackaddr$prim54544, align 8
%stackaddr$prim54545 = alloca %struct.ScmObj*, align 8
%argslist53392$k473822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49512, %struct.ScmObj* %argslist53392$k473821)
store volatile %struct.ScmObj* %argslist53392$k473822, %struct.ScmObj** %stackaddr$prim54545, align 8
%clofunc54546 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47382)
musttail call tailcc void %clofunc54546(%struct.ScmObj* %k47382, %struct.ScmObj* %argslist53392$k473822)
ret void
}

define tailcc void @proc_clo$ae49341(%struct.ScmObj* %env$ae49341,%struct.ScmObj* %current_45args53393) {
%stackaddr$env-ref54547 = alloca %struct.ScmObj*, align 8
%k47382 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49341, i64 0)
store %struct.ScmObj* %k47382, %struct.ScmObj** %stackaddr$env-ref54547
%stackaddr$env-ref54548 = alloca %struct.ScmObj*, align 8
%a47155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49341, i64 1)
store %struct.ScmObj* %a47155, %struct.ScmObj** %stackaddr$env-ref54548
%stackaddr$prim54549 = alloca %struct.ScmObj*, align 8
%_95k47384 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53393)
store volatile %struct.ScmObj* %_95k47384, %struct.ScmObj** %stackaddr$prim54549, align 8
%stackaddr$prim54550 = alloca %struct.ScmObj*, align 8
%current_45args53394 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53393)
store volatile %struct.ScmObj* %current_45args53394, %struct.ScmObj** %stackaddr$prim54550, align 8
%stackaddr$prim54551 = alloca %struct.ScmObj*, align 8
%cc47156 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53394)
store volatile %struct.ScmObj* %cc47156, %struct.ScmObj** %stackaddr$prim54551, align 8
%ae49343 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54552 = alloca %struct.ScmObj*, align 8
%anf_45bind47268 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49343)
store volatile %struct.ScmObj* %anf_45bind47268, %struct.ScmObj** %stackaddr$prim54552, align 8
%stackaddr$prim54553 = alloca %struct.ScmObj*, align 8
%anf_45bind47269 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47268)
store volatile %struct.ScmObj* %anf_45bind47269, %struct.ScmObj** %stackaddr$prim54553, align 8
%truthy$cmp54554 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47269)
%cmp$cmp54554 = icmp eq i64 %truthy$cmp54554, 1
br i1 %cmp$cmp54554, label %truebranch$cmp54554, label %falsebranch$cmp54554
truebranch$cmp54554:
%ae49347 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49348 = call %struct.ScmObj* @const_init_true()
%argslist53396$k473820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54555 = alloca %struct.ScmObj*, align 8
%argslist53396$k473821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49348, %struct.ScmObj* %argslist53396$k473820)
store volatile %struct.ScmObj* %argslist53396$k473821, %struct.ScmObj** %stackaddr$prim54555, align 8
%stackaddr$prim54556 = alloca %struct.ScmObj*, align 8
%argslist53396$k473822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49347, %struct.ScmObj* %argslist53396$k473821)
store volatile %struct.ScmObj* %argslist53396$k473822, %struct.ScmObj** %stackaddr$prim54556, align 8
%clofunc54557 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47382)
musttail call tailcc void %clofunc54557(%struct.ScmObj* %k47382, %struct.ScmObj* %argslist53396$k473822)
ret void
falsebranch$cmp54554:
%ae49356 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54558 = alloca %struct.ScmObj*, align 8
%anf_45bind47270 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49356)
store volatile %struct.ScmObj* %anf_45bind47270, %struct.ScmObj** %stackaddr$prim54558, align 8
%stackaddr$prim54559 = alloca %struct.ScmObj*, align 8
%anf_45bind47271 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind47270)
store volatile %struct.ScmObj* %anf_45bind47271, %struct.ScmObj** %stackaddr$prim54559, align 8
%truthy$cmp54560 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47271)
%cmp$cmp54560 = icmp eq i64 %truthy$cmp54560, 1
br i1 %cmp$cmp54560, label %truebranch$cmp54560, label %falsebranch$cmp54560
truebranch$cmp54560:
%ae49360 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54561 = alloca %struct.ScmObj*, align 8
%anf_45bind47272 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49360)
store volatile %struct.ScmObj* %anf_45bind47272, %struct.ScmObj** %stackaddr$prim54561, align 8
%stackaddr$prim54562 = alloca %struct.ScmObj*, align 8
%b47158 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47272)
store volatile %struct.ScmObj* %b47158, %struct.ScmObj** %stackaddr$prim54562, align 8
%ae49363 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54563 = alloca %struct.ScmObj*, align 8
%anf_45bind47273 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49363)
store volatile %struct.ScmObj* %anf_45bind47273, %struct.ScmObj** %stackaddr$prim54563, align 8
%stackaddr$prim54564 = alloca %struct.ScmObj*, align 8
%anf_45bind47274 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47273)
store volatile %struct.ScmObj* %anf_45bind47274, %struct.ScmObj** %stackaddr$prim54564, align 8
%ae49366 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54565 = alloca %struct.ScmObj*, align 8
%_95047159 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49366, %struct.ScmObj* %anf_45bind47274)
store volatile %struct.ScmObj* %_95047159, %struct.ScmObj** %stackaddr$prim54565, align 8
%argslist53397$cc471560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54566 = alloca %struct.ScmObj*, align 8
%argslist53397$cc471561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47156, %struct.ScmObj* %argslist53397$cc471560)
store volatile %struct.ScmObj* %argslist53397$cc471561, %struct.ScmObj** %stackaddr$prim54566, align 8
%stackaddr$prim54567 = alloca %struct.ScmObj*, align 8
%argslist53397$cc471562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47382, %struct.ScmObj* %argslist53397$cc471561)
store volatile %struct.ScmObj* %argslist53397$cc471562, %struct.ScmObj** %stackaddr$prim54567, align 8
%clofunc54568 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47156)
musttail call tailcc void %clofunc54568(%struct.ScmObj* %cc47156, %struct.ScmObj* %argslist53397$cc471562)
ret void
falsebranch$cmp54560:
%ae49399 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49400 = call %struct.ScmObj* @const_init_false()
%argslist53398$k473820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54569 = alloca %struct.ScmObj*, align 8
%argslist53398$k473821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49400, %struct.ScmObj* %argslist53398$k473820)
store volatile %struct.ScmObj* %argslist53398$k473821, %struct.ScmObj** %stackaddr$prim54569, align 8
%stackaddr$prim54570 = alloca %struct.ScmObj*, align 8
%argslist53398$k473822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49399, %struct.ScmObj* %argslist53398$k473821)
store volatile %struct.ScmObj* %argslist53398$k473822, %struct.ScmObj** %stackaddr$prim54570, align 8
%clofunc54571 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47382)
musttail call tailcc void %clofunc54571(%struct.ScmObj* %k47382, %struct.ScmObj* %argslist53398$k473822)
ret void
}

define tailcc void @proc_clo$ae49323(%struct.ScmObj* %env$ae49323,%struct.ScmObj* %current_45args53400) {
%stackaddr$prim54572 = alloca %struct.ScmObj*, align 8
%k47385 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53400)
store volatile %struct.ScmObj* %k47385, %struct.ScmObj** %stackaddr$prim54572, align 8
%stackaddr$prim54573 = alloca %struct.ScmObj*, align 8
%current_45args53401 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53400)
store volatile %struct.ScmObj* %current_45args53401, %struct.ScmObj** %stackaddr$prim54573, align 8
%stackaddr$prim54574 = alloca %struct.ScmObj*, align 8
%k47157 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53401)
store volatile %struct.ScmObj* %k47157, %struct.ScmObj** %stackaddr$prim54574, align 8
%ae49325 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53403$k473850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54575 = alloca %struct.ScmObj*, align 8
%argslist53403$k473851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47157, %struct.ScmObj* %argslist53403$k473850)
store volatile %struct.ScmObj* %argslist53403$k473851, %struct.ScmObj** %stackaddr$prim54575, align 8
%stackaddr$prim54576 = alloca %struct.ScmObj*, align 8
%argslist53403$k473852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49325, %struct.ScmObj* %argslist53403$k473851)
store volatile %struct.ScmObj* %argslist53403$k473852, %struct.ScmObj** %stackaddr$prim54576, align 8
%clofunc54577 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47385)
musttail call tailcc void %clofunc54577(%struct.ScmObj* %k47385, %struct.ScmObj* %argslist53403$k473852)
ret void
}

define tailcc void @proc_clo$ae49246(%struct.ScmObj* %env$ae49246,%struct.ScmObj* %current_45args53406) {
%stackaddr$env-ref54578 = alloca %struct.ScmObj*, align 8
%_37append47161 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49246, i64 0)
store %struct.ScmObj* %_37append47161, %struct.ScmObj** %stackaddr$env-ref54578
%stackaddr$prim54579 = alloca %struct.ScmObj*, align 8
%k47386 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53406)
store volatile %struct.ScmObj* %k47386, %struct.ScmObj** %stackaddr$prim54579, align 8
%stackaddr$prim54580 = alloca %struct.ScmObj*, align 8
%current_45args53407 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53406)
store volatile %struct.ScmObj* %current_45args53407, %struct.ScmObj** %stackaddr$prim54580, align 8
%stackaddr$prim54581 = alloca %struct.ScmObj*, align 8
%ls047164 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53407)
store volatile %struct.ScmObj* %ls047164, %struct.ScmObj** %stackaddr$prim54581, align 8
%stackaddr$prim54582 = alloca %struct.ScmObj*, align 8
%current_45args53408 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53407)
store volatile %struct.ScmObj* %current_45args53408, %struct.ScmObj** %stackaddr$prim54582, align 8
%stackaddr$prim54583 = alloca %struct.ScmObj*, align 8
%ls147163 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53408)
store volatile %struct.ScmObj* %ls147163, %struct.ScmObj** %stackaddr$prim54583, align 8
%stackaddr$prim54584 = alloca %struct.ScmObj*, align 8
%anf_45bind47261 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls047164)
store volatile %struct.ScmObj* %anf_45bind47261, %struct.ScmObj** %stackaddr$prim54584, align 8
%truthy$cmp54585 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47261)
%cmp$cmp54585 = icmp eq i64 %truthy$cmp54585, 1
br i1 %cmp$cmp54585, label %truebranch$cmp54585, label %falsebranch$cmp54585
truebranch$cmp54585:
%ae49250 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53410$k473860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54586 = alloca %struct.ScmObj*, align 8
%argslist53410$k473861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls147163, %struct.ScmObj* %argslist53410$k473860)
store volatile %struct.ScmObj* %argslist53410$k473861, %struct.ScmObj** %stackaddr$prim54586, align 8
%stackaddr$prim54587 = alloca %struct.ScmObj*, align 8
%argslist53410$k473862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49250, %struct.ScmObj* %argslist53410$k473861)
store volatile %struct.ScmObj* %argslist53410$k473862, %struct.ScmObj** %stackaddr$prim54587, align 8
%clofunc54588 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47386)
musttail call tailcc void %clofunc54588(%struct.ScmObj* %k47386, %struct.ScmObj* %argslist53410$k473862)
ret void
falsebranch$cmp54585:
%stackaddr$prim54589 = alloca %struct.ScmObj*, align 8
%anf_45bind47262 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls047164)
store volatile %struct.ScmObj* %anf_45bind47262, %struct.ScmObj** %stackaddr$prim54589, align 8
%ae49257 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54590 = alloca %struct.ScmObj*, align 8
%anf_45bind47263 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append47161, %struct.ScmObj* %ae49257)
store volatile %struct.ScmObj* %anf_45bind47263, %struct.ScmObj** %stackaddr$prim54590, align 8
%stackaddr$prim54591 = alloca %struct.ScmObj*, align 8
%anf_45bind47264 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls047164)
store volatile %struct.ScmObj* %anf_45bind47264, %struct.ScmObj** %stackaddr$prim54591, align 8
%stackaddr$makeclosure54592 = alloca %struct.ScmObj*, align 8
%fptrToInt54593 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49260 to i64
%ae49260 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54593)
store volatile %struct.ScmObj* %ae49260, %struct.ScmObj** %stackaddr$makeclosure54592, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49260, %struct.ScmObj* %anf_45bind47262, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49260, %struct.ScmObj* %k47386, i64 1)
%argslist53415$anf_45bind472630 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54594 = alloca %struct.ScmObj*, align 8
%argslist53415$anf_45bind472631 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls147163, %struct.ScmObj* %argslist53415$anf_45bind472630)
store volatile %struct.ScmObj* %argslist53415$anf_45bind472631, %struct.ScmObj** %stackaddr$prim54594, align 8
%stackaddr$prim54595 = alloca %struct.ScmObj*, align 8
%argslist53415$anf_45bind472632 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47264, %struct.ScmObj* %argslist53415$anf_45bind472631)
store volatile %struct.ScmObj* %argslist53415$anf_45bind472632, %struct.ScmObj** %stackaddr$prim54595, align 8
%stackaddr$prim54596 = alloca %struct.ScmObj*, align 8
%argslist53415$anf_45bind472633 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49260, %struct.ScmObj* %argslist53415$anf_45bind472632)
store volatile %struct.ScmObj* %argslist53415$anf_45bind472633, %struct.ScmObj** %stackaddr$prim54596, align 8
%clofunc54597 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47263)
musttail call tailcc void %clofunc54597(%struct.ScmObj* %anf_45bind47263, %struct.ScmObj* %argslist53415$anf_45bind472633)
ret void
}

define tailcc void @proc_clo$ae49260(%struct.ScmObj* %env$ae49260,%struct.ScmObj* %current_45args53411) {
%stackaddr$env-ref54598 = alloca %struct.ScmObj*, align 8
%anf_45bind47262 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49260, i64 0)
store %struct.ScmObj* %anf_45bind47262, %struct.ScmObj** %stackaddr$env-ref54598
%stackaddr$env-ref54599 = alloca %struct.ScmObj*, align 8
%k47386 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49260, i64 1)
store %struct.ScmObj* %k47386, %struct.ScmObj** %stackaddr$env-ref54599
%stackaddr$prim54600 = alloca %struct.ScmObj*, align 8
%_95k47387 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53411)
store volatile %struct.ScmObj* %_95k47387, %struct.ScmObj** %stackaddr$prim54600, align 8
%stackaddr$prim54601 = alloca %struct.ScmObj*, align 8
%current_45args53412 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53411)
store volatile %struct.ScmObj* %current_45args53412, %struct.ScmObj** %stackaddr$prim54601, align 8
%stackaddr$prim54602 = alloca %struct.ScmObj*, align 8
%anf_45bind47265 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53412)
store volatile %struct.ScmObj* %anf_45bind47265, %struct.ScmObj** %stackaddr$prim54602, align 8
%stackaddr$prim54603 = alloca %struct.ScmObj*, align 8
%cpsprim47388 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47262, %struct.ScmObj* %anf_45bind47265)
store volatile %struct.ScmObj* %cpsprim47388, %struct.ScmObj** %stackaddr$prim54603, align 8
%ae49266 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53414$k473860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54604 = alloca %struct.ScmObj*, align 8
%argslist53414$k473861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47388, %struct.ScmObj* %argslist53414$k473860)
store volatile %struct.ScmObj* %argslist53414$k473861, %struct.ScmObj** %stackaddr$prim54604, align 8
%stackaddr$prim54605 = alloca %struct.ScmObj*, align 8
%argslist53414$k473862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49266, %struct.ScmObj* %argslist53414$k473861)
store volatile %struct.ScmObj* %argslist53414$k473862, %struct.ScmObj** %stackaddr$prim54605, align 8
%clofunc54606 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47386)
musttail call tailcc void %clofunc54606(%struct.ScmObj* %k47386, %struct.ScmObj* %argslist53414$k473862)
ret void
}

define tailcc void @proc_clo$ae49220(%struct.ScmObj* %env$ae49220,%struct.ScmObj* %current_45args53417) {
%stackaddr$prim54607 = alloca %struct.ScmObj*, align 8
%k47389 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53417)
store volatile %struct.ScmObj* %k47389, %struct.ScmObj** %stackaddr$prim54607, align 8
%stackaddr$prim54608 = alloca %struct.ScmObj*, align 8
%current_45args53418 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53417)
store volatile %struct.ScmObj* %current_45args53418, %struct.ScmObj** %stackaddr$prim54608, align 8
%stackaddr$prim54609 = alloca %struct.ScmObj*, align 8
%a47167 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53418)
store volatile %struct.ScmObj* %a47167, %struct.ScmObj** %stackaddr$prim54609, align 8
%stackaddr$prim54610 = alloca %struct.ScmObj*, align 8
%current_45args53419 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53418)
store volatile %struct.ScmObj* %current_45args53419, %struct.ScmObj** %stackaddr$prim54610, align 8
%stackaddr$prim54611 = alloca %struct.ScmObj*, align 8
%b47166 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53419)
store volatile %struct.ScmObj* %b47166, %struct.ScmObj** %stackaddr$prim54611, align 8
%stackaddr$prim54612 = alloca %struct.ScmObj*, align 8
%anf_45bind47260 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a47167, %struct.ScmObj* %b47166)
store volatile %struct.ScmObj* %anf_45bind47260, %struct.ScmObj** %stackaddr$prim54612, align 8
%stackaddr$prim54613 = alloca %struct.ScmObj*, align 8
%cpsprim47390 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind47260)
store volatile %struct.ScmObj* %cpsprim47390, %struct.ScmObj** %stackaddr$prim54613, align 8
%ae49225 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53421$k473890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54614 = alloca %struct.ScmObj*, align 8
%argslist53421$k473891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47390, %struct.ScmObj* %argslist53421$k473890)
store volatile %struct.ScmObj* %argslist53421$k473891, %struct.ScmObj** %stackaddr$prim54614, align 8
%stackaddr$prim54615 = alloca %struct.ScmObj*, align 8
%argslist53421$k473892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49225, %struct.ScmObj* %argslist53421$k473891)
store volatile %struct.ScmObj* %argslist53421$k473892, %struct.ScmObj** %stackaddr$prim54615, align 8
%clofunc54616 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47389)
musttail call tailcc void %clofunc54616(%struct.ScmObj* %k47389, %struct.ScmObj* %argslist53421$k473892)
ret void
}

define tailcc void @proc_clo$ae49196(%struct.ScmObj* %env$ae49196,%struct.ScmObj* %current_45args53423) {
%stackaddr$prim54617 = alloca %struct.ScmObj*, align 8
%k47391 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53423)
store volatile %struct.ScmObj* %k47391, %struct.ScmObj** %stackaddr$prim54617, align 8
%stackaddr$prim54618 = alloca %struct.ScmObj*, align 8
%current_45args53424 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53423)
store volatile %struct.ScmObj* %current_45args53424, %struct.ScmObj** %stackaddr$prim54618, align 8
%stackaddr$prim54619 = alloca %struct.ScmObj*, align 8
%a47170 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53424)
store volatile %struct.ScmObj* %a47170, %struct.ScmObj** %stackaddr$prim54619, align 8
%stackaddr$prim54620 = alloca %struct.ScmObj*, align 8
%current_45args53425 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53424)
store volatile %struct.ScmObj* %current_45args53425, %struct.ScmObj** %stackaddr$prim54620, align 8
%stackaddr$prim54621 = alloca %struct.ScmObj*, align 8
%b47169 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53425)
store volatile %struct.ScmObj* %b47169, %struct.ScmObj** %stackaddr$prim54621, align 8
%stackaddr$prim54622 = alloca %struct.ScmObj*, align 8
%anf_45bind47259 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a47170, %struct.ScmObj* %b47169)
store volatile %struct.ScmObj* %anf_45bind47259, %struct.ScmObj** %stackaddr$prim54622, align 8
%stackaddr$prim54623 = alloca %struct.ScmObj*, align 8
%cpsprim47392 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind47259)
store volatile %struct.ScmObj* %cpsprim47392, %struct.ScmObj** %stackaddr$prim54623, align 8
%ae49201 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53427$k473910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54624 = alloca %struct.ScmObj*, align 8
%argslist53427$k473911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47392, %struct.ScmObj* %argslist53427$k473910)
store volatile %struct.ScmObj* %argslist53427$k473911, %struct.ScmObj** %stackaddr$prim54624, align 8
%stackaddr$prim54625 = alloca %struct.ScmObj*, align 8
%argslist53427$k473912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49201, %struct.ScmObj* %argslist53427$k473911)
store volatile %struct.ScmObj* %argslist53427$k473912, %struct.ScmObj** %stackaddr$prim54625, align 8
%clofunc54626 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47391)
musttail call tailcc void %clofunc54626(%struct.ScmObj* %k47391, %struct.ScmObj* %argslist53427$k473912)
ret void
}

define tailcc void @proc_clo$ae48802(%struct.ScmObj* %env$ae48802,%struct.ScmObj* %current_45args53430) {
%stackaddr$env-ref54627 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48802, i64 0)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref54627
%stackaddr$env-ref54628 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48802, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54628
%stackaddr$env-ref54629 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48802, i64 2)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54629
%stackaddr$prim54630 = alloca %struct.ScmObj*, align 8
%k47393 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53430)
store volatile %struct.ScmObj* %k47393, %struct.ScmObj** %stackaddr$prim54630, align 8
%stackaddr$prim54631 = alloca %struct.ScmObj*, align 8
%current_45args53431 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53430)
store volatile %struct.ScmObj* %current_45args53431, %struct.ScmObj** %stackaddr$prim54631, align 8
%stackaddr$prim54632 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53431)
store volatile %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$prim54632, align 8
%ae48804 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54633 = alloca %struct.ScmObj*, align 8
%fptrToInt54634 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48805 to i64
%ae48805 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54634)
store volatile %struct.ScmObj* %ae48805, %struct.ScmObj** %stackaddr$makeclosure54633, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48805, %struct.ScmObj* %_37foldr47094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48805, %struct.ScmObj* %_37foldl47172, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48805, %struct.ScmObj* %_37foldr147089, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48805, %struct.ScmObj* %_37map147120, i64 3)
%argslist53488$k473930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54635 = alloca %struct.ScmObj*, align 8
%argslist53488$k473931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48805, %struct.ScmObj* %argslist53488$k473930)
store volatile %struct.ScmObj* %argslist53488$k473931, %struct.ScmObj** %stackaddr$prim54635, align 8
%stackaddr$prim54636 = alloca %struct.ScmObj*, align 8
%argslist53488$k473932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48804, %struct.ScmObj* %argslist53488$k473931)
store volatile %struct.ScmObj* %argslist53488$k473932, %struct.ScmObj** %stackaddr$prim54636, align 8
%clofunc54637 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47393)
musttail call tailcc void %clofunc54637(%struct.ScmObj* %k47393, %struct.ScmObj* %argslist53488$k473932)
ret void
}

define tailcc void @proc_clo$ae48805(%struct.ScmObj* %env$ae48805,%struct.ScmObj* %args4717347394) {
%stackaddr$env-ref54638 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48805, i64 0)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref54638
%stackaddr$env-ref54639 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48805, i64 1)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref54639
%stackaddr$env-ref54640 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48805, i64 2)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54640
%stackaddr$env-ref54641 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48805, i64 3)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54641
%stackaddr$prim54642 = alloca %struct.ScmObj*, align 8
%k47395 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4717347394)
store volatile %struct.ScmObj* %k47395, %struct.ScmObj** %stackaddr$prim54642, align 8
%stackaddr$prim54643 = alloca %struct.ScmObj*, align 8
%args47173 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4717347394)
store volatile %struct.ScmObj* %args47173, %struct.ScmObj** %stackaddr$prim54643, align 8
%stackaddr$prim54644 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47173)
store volatile %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$prim54644, align 8
%stackaddr$prim54645 = alloca %struct.ScmObj*, align 8
%anf_45bind47247 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47173)
store volatile %struct.ScmObj* %anf_45bind47247, %struct.ScmObj** %stackaddr$prim54645, align 8
%stackaddr$prim54646 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47247)
store volatile %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$prim54646, align 8
%stackaddr$prim54647 = alloca %struct.ScmObj*, align 8
%anf_45bind47248 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47173)
store volatile %struct.ScmObj* %anf_45bind47248, %struct.ScmObj** %stackaddr$prim54647, align 8
%stackaddr$prim54648 = alloca %struct.ScmObj*, align 8
%lsts47174 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47248)
store volatile %struct.ScmObj* %lsts47174, %struct.ScmObj** %stackaddr$prim54648, align 8
%stackaddr$makeclosure54649 = alloca %struct.ScmObj*, align 8
%fptrToInt54650 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48813 to i64
%ae48813 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt54650)
store volatile %struct.ScmObj* %ae48813, %struct.ScmObj** %stackaddr$makeclosure54649, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48813, %struct.ScmObj* %lsts47174, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48813, %struct.ScmObj* %_37foldr47094, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48813, %struct.ScmObj* %_37foldl47172, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48813, %struct.ScmObj* %k47395, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48813, %struct.ScmObj* %_37foldr147089, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48813, %struct.ScmObj* %_37map147120, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48813, %struct.ScmObj* %f47176, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae48813, %struct.ScmObj* %acc47175, i64 7)
%ae48814 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54651 = alloca %struct.ScmObj*, align 8
%fptrToInt54652 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48815 to i64
%ae48815 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54652)
store volatile %struct.ScmObj* %ae48815, %struct.ScmObj** %stackaddr$makeclosure54651, align 8
%argslist53487$ae488130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54653 = alloca %struct.ScmObj*, align 8
%argslist53487$ae488131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48815, %struct.ScmObj* %argslist53487$ae488130)
store volatile %struct.ScmObj* %argslist53487$ae488131, %struct.ScmObj** %stackaddr$prim54653, align 8
%stackaddr$prim54654 = alloca %struct.ScmObj*, align 8
%argslist53487$ae488132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48814, %struct.ScmObj* %argslist53487$ae488131)
store volatile %struct.ScmObj* %argslist53487$ae488132, %struct.ScmObj** %stackaddr$prim54654, align 8
%clofunc54655 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48813)
musttail call tailcc void %clofunc54655(%struct.ScmObj* %ae48813, %struct.ScmObj* %argslist53487$ae488132)
ret void
}

define tailcc void @proc_clo$ae48813(%struct.ScmObj* %env$ae48813,%struct.ScmObj* %current_45args53433) {
%stackaddr$env-ref54656 = alloca %struct.ScmObj*, align 8
%lsts47174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48813, i64 0)
store %struct.ScmObj* %lsts47174, %struct.ScmObj** %stackaddr$env-ref54656
%stackaddr$env-ref54657 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48813, i64 1)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref54657
%stackaddr$env-ref54658 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48813, i64 2)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref54658
%stackaddr$env-ref54659 = alloca %struct.ScmObj*, align 8
%k47395 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48813, i64 3)
store %struct.ScmObj* %k47395, %struct.ScmObj** %stackaddr$env-ref54659
%stackaddr$env-ref54660 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48813, i64 4)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54660
%stackaddr$env-ref54661 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48813, i64 5)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54661
%stackaddr$env-ref54662 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48813, i64 6)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref54662
%stackaddr$env-ref54663 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48813, i64 7)
store %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$env-ref54663
%stackaddr$prim54664 = alloca %struct.ScmObj*, align 8
%_95k47396 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53433)
store volatile %struct.ScmObj* %_95k47396, %struct.ScmObj** %stackaddr$prim54664, align 8
%stackaddr$prim54665 = alloca %struct.ScmObj*, align 8
%current_45args53434 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53433)
store volatile %struct.ScmObj* %current_45args53434, %struct.ScmObj** %stackaddr$prim54665, align 8
%stackaddr$prim54666 = alloca %struct.ScmObj*, align 8
%anf_45bind47249 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53434)
store volatile %struct.ScmObj* %anf_45bind47249, %struct.ScmObj** %stackaddr$prim54666, align 8
%stackaddr$makeclosure54667 = alloca %struct.ScmObj*, align 8
%fptrToInt54668 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48845 to i64
%ae48845 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt54668)
store volatile %struct.ScmObj* %ae48845, %struct.ScmObj** %stackaddr$makeclosure54667, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48845, %struct.ScmObj* %lsts47174, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48845, %struct.ScmObj* %_37foldr47094, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48845, %struct.ScmObj* %_37foldl47172, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48845, %struct.ScmObj* %k47395, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48845, %struct.ScmObj* %_37map147120, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48845, %struct.ScmObj* %f47176, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48845, %struct.ScmObj* %acc47175, i64 6)
%ae48847 = call %struct.ScmObj* @const_init_false()
%argslist53480$_37foldr1470890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54669 = alloca %struct.ScmObj*, align 8
%argslist53480$_37foldr1470891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47174, %struct.ScmObj* %argslist53480$_37foldr1470890)
store volatile %struct.ScmObj* %argslist53480$_37foldr1470891, %struct.ScmObj** %stackaddr$prim54669, align 8
%stackaddr$prim54670 = alloca %struct.ScmObj*, align 8
%argslist53480$_37foldr1470892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48847, %struct.ScmObj* %argslist53480$_37foldr1470891)
store volatile %struct.ScmObj* %argslist53480$_37foldr1470892, %struct.ScmObj** %stackaddr$prim54670, align 8
%stackaddr$prim54671 = alloca %struct.ScmObj*, align 8
%argslist53480$_37foldr1470893 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47249, %struct.ScmObj* %argslist53480$_37foldr1470892)
store volatile %struct.ScmObj* %argslist53480$_37foldr1470893, %struct.ScmObj** %stackaddr$prim54671, align 8
%stackaddr$prim54672 = alloca %struct.ScmObj*, align 8
%argslist53480$_37foldr1470894 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48845, %struct.ScmObj* %argslist53480$_37foldr1470893)
store volatile %struct.ScmObj* %argslist53480$_37foldr1470894, %struct.ScmObj** %stackaddr$prim54672, align 8
%clofunc54673 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147089)
musttail call tailcc void %clofunc54673(%struct.ScmObj* %_37foldr147089, %struct.ScmObj* %argslist53480$_37foldr1470894)
ret void
}

define tailcc void @proc_clo$ae48845(%struct.ScmObj* %env$ae48845,%struct.ScmObj* %current_45args53436) {
%stackaddr$env-ref54674 = alloca %struct.ScmObj*, align 8
%lsts47174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48845, i64 0)
store %struct.ScmObj* %lsts47174, %struct.ScmObj** %stackaddr$env-ref54674
%stackaddr$env-ref54675 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48845, i64 1)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref54675
%stackaddr$env-ref54676 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48845, i64 2)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref54676
%stackaddr$env-ref54677 = alloca %struct.ScmObj*, align 8
%k47395 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48845, i64 3)
store %struct.ScmObj* %k47395, %struct.ScmObj** %stackaddr$env-ref54677
%stackaddr$env-ref54678 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48845, i64 4)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54678
%stackaddr$env-ref54679 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48845, i64 5)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref54679
%stackaddr$env-ref54680 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48845, i64 6)
store %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$env-ref54680
%stackaddr$prim54681 = alloca %struct.ScmObj*, align 8
%_95k47397 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53436)
store volatile %struct.ScmObj* %_95k47397, %struct.ScmObj** %stackaddr$prim54681, align 8
%stackaddr$prim54682 = alloca %struct.ScmObj*, align 8
%current_45args53437 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53436)
store volatile %struct.ScmObj* %current_45args53437, %struct.ScmObj** %stackaddr$prim54682, align 8
%stackaddr$prim54683 = alloca %struct.ScmObj*, align 8
%anf_45bind47250 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53437)
store volatile %struct.ScmObj* %anf_45bind47250, %struct.ScmObj** %stackaddr$prim54683, align 8
%truthy$cmp54684 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47250)
%cmp$cmp54684 = icmp eq i64 %truthy$cmp54684, 1
br i1 %cmp$cmp54684, label %truebranch$cmp54684, label %falsebranch$cmp54684
truebranch$cmp54684:
%ae48856 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53439$k473950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54685 = alloca %struct.ScmObj*, align 8
%argslist53439$k473951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47175, %struct.ScmObj* %argslist53439$k473950)
store volatile %struct.ScmObj* %argslist53439$k473951, %struct.ScmObj** %stackaddr$prim54685, align 8
%stackaddr$prim54686 = alloca %struct.ScmObj*, align 8
%argslist53439$k473952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48856, %struct.ScmObj* %argslist53439$k473951)
store volatile %struct.ScmObj* %argslist53439$k473952, %struct.ScmObj** %stackaddr$prim54686, align 8
%clofunc54687 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47395)
musttail call tailcc void %clofunc54687(%struct.ScmObj* %k47395, %struct.ScmObj* %argslist53439$k473952)
ret void
falsebranch$cmp54684:
%stackaddr$makeclosure54688 = alloca %struct.ScmObj*, align 8
%fptrToInt54689 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48861 to i64
%ae48861 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt54689)
store volatile %struct.ScmObj* %ae48861, %struct.ScmObj** %stackaddr$makeclosure54688, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48861, %struct.ScmObj* %lsts47174, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48861, %struct.ScmObj* %_37foldr47094, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48861, %struct.ScmObj* %_37foldl47172, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48861, %struct.ScmObj* %k47395, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48861, %struct.ScmObj* %_37map147120, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48861, %struct.ScmObj* %f47176, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48861, %struct.ScmObj* %acc47175, i64 6)
%ae48862 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54690 = alloca %struct.ScmObj*, align 8
%fptrToInt54691 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48863 to i64
%ae48863 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54691)
store volatile %struct.ScmObj* %ae48863, %struct.ScmObj** %stackaddr$makeclosure54690, align 8
%argslist53479$ae488610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54692 = alloca %struct.ScmObj*, align 8
%argslist53479$ae488611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48863, %struct.ScmObj* %argslist53479$ae488610)
store volatile %struct.ScmObj* %argslist53479$ae488611, %struct.ScmObj** %stackaddr$prim54692, align 8
%stackaddr$prim54693 = alloca %struct.ScmObj*, align 8
%argslist53479$ae488612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48862, %struct.ScmObj* %argslist53479$ae488611)
store volatile %struct.ScmObj* %argslist53479$ae488612, %struct.ScmObj** %stackaddr$prim54693, align 8
%clofunc54694 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48861)
musttail call tailcc void %clofunc54694(%struct.ScmObj* %ae48861, %struct.ScmObj* %argslist53479$ae488612)
ret void
}

define tailcc void @proc_clo$ae48861(%struct.ScmObj* %env$ae48861,%struct.ScmObj* %current_45args53440) {
%stackaddr$env-ref54695 = alloca %struct.ScmObj*, align 8
%lsts47174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48861, i64 0)
store %struct.ScmObj* %lsts47174, %struct.ScmObj** %stackaddr$env-ref54695
%stackaddr$env-ref54696 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48861, i64 1)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref54696
%stackaddr$env-ref54697 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48861, i64 2)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref54697
%stackaddr$env-ref54698 = alloca %struct.ScmObj*, align 8
%k47395 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48861, i64 3)
store %struct.ScmObj* %k47395, %struct.ScmObj** %stackaddr$env-ref54698
%stackaddr$env-ref54699 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48861, i64 4)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54699
%stackaddr$env-ref54700 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48861, i64 5)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref54700
%stackaddr$env-ref54701 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48861, i64 6)
store %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$env-ref54701
%stackaddr$prim54702 = alloca %struct.ScmObj*, align 8
%_95k47398 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53440)
store volatile %struct.ScmObj* %_95k47398, %struct.ScmObj** %stackaddr$prim54702, align 8
%stackaddr$prim54703 = alloca %struct.ScmObj*, align 8
%current_45args53441 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53440)
store volatile %struct.ScmObj* %current_45args53441, %struct.ScmObj** %stackaddr$prim54703, align 8
%stackaddr$prim54704 = alloca %struct.ScmObj*, align 8
%anf_45bind47251 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53441)
store volatile %struct.ScmObj* %anf_45bind47251, %struct.ScmObj** %stackaddr$prim54704, align 8
%stackaddr$makeclosure54705 = alloca %struct.ScmObj*, align 8
%fptrToInt54706 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48882 to i64
%ae48882 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt54706)
store volatile %struct.ScmObj* %ae48882, %struct.ScmObj** %stackaddr$makeclosure54705, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48882, %struct.ScmObj* %lsts47174, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48882, %struct.ScmObj* %_37foldr47094, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48882, %struct.ScmObj* %_37foldl47172, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48882, %struct.ScmObj* %k47395, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48882, %struct.ScmObj* %_37map147120, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48882, %struct.ScmObj* %f47176, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48882, %struct.ScmObj* %acc47175, i64 6)
%argslist53474$_37map1471200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54707 = alloca %struct.ScmObj*, align 8
%argslist53474$_37map1471201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47174, %struct.ScmObj* %argslist53474$_37map1471200)
store volatile %struct.ScmObj* %argslist53474$_37map1471201, %struct.ScmObj** %stackaddr$prim54707, align 8
%stackaddr$prim54708 = alloca %struct.ScmObj*, align 8
%argslist53474$_37map1471202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47251, %struct.ScmObj* %argslist53474$_37map1471201)
store volatile %struct.ScmObj* %argslist53474$_37map1471202, %struct.ScmObj** %stackaddr$prim54708, align 8
%stackaddr$prim54709 = alloca %struct.ScmObj*, align 8
%argslist53474$_37map1471203 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48882, %struct.ScmObj* %argslist53474$_37map1471202)
store volatile %struct.ScmObj* %argslist53474$_37map1471203, %struct.ScmObj** %stackaddr$prim54709, align 8
%clofunc54710 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147120)
musttail call tailcc void %clofunc54710(%struct.ScmObj* %_37map147120, %struct.ScmObj* %argslist53474$_37map1471203)
ret void
}

define tailcc void @proc_clo$ae48882(%struct.ScmObj* %env$ae48882,%struct.ScmObj* %current_45args53443) {
%stackaddr$env-ref54711 = alloca %struct.ScmObj*, align 8
%lsts47174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48882, i64 0)
store %struct.ScmObj* %lsts47174, %struct.ScmObj** %stackaddr$env-ref54711
%stackaddr$env-ref54712 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48882, i64 1)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref54712
%stackaddr$env-ref54713 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48882, i64 2)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref54713
%stackaddr$env-ref54714 = alloca %struct.ScmObj*, align 8
%k47395 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48882, i64 3)
store %struct.ScmObj* %k47395, %struct.ScmObj** %stackaddr$env-ref54714
%stackaddr$env-ref54715 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48882, i64 4)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54715
%stackaddr$env-ref54716 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48882, i64 5)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref54716
%stackaddr$env-ref54717 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48882, i64 6)
store %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$env-ref54717
%stackaddr$prim54718 = alloca %struct.ScmObj*, align 8
%_95k47399 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53443)
store volatile %struct.ScmObj* %_95k47399, %struct.ScmObj** %stackaddr$prim54718, align 8
%stackaddr$prim54719 = alloca %struct.ScmObj*, align 8
%current_45args53444 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53443)
store volatile %struct.ScmObj* %current_45args53444, %struct.ScmObj** %stackaddr$prim54719, align 8
%stackaddr$prim54720 = alloca %struct.ScmObj*, align 8
%lsts_4347181 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53444)
store volatile %struct.ScmObj* %lsts_4347181, %struct.ScmObj** %stackaddr$prim54720, align 8
%stackaddr$makeclosure54721 = alloca %struct.ScmObj*, align 8
%fptrToInt54722 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48885 to i64
%ae48885 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt54722)
store volatile %struct.ScmObj* %ae48885, %struct.ScmObj** %stackaddr$makeclosure54721, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48885, %struct.ScmObj* %lsts47174, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48885, %struct.ScmObj* %_37foldr47094, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48885, %struct.ScmObj* %_37foldl47172, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48885, %struct.ScmObj* %k47395, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48885, %struct.ScmObj* %_37map147120, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48885, %struct.ScmObj* %lsts_4347181, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48885, %struct.ScmObj* %f47176, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae48885, %struct.ScmObj* %acc47175, i64 7)
%ae48886 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54723 = alloca %struct.ScmObj*, align 8
%fptrToInt54724 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48887 to i64
%ae48887 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54724)
store volatile %struct.ScmObj* %ae48887, %struct.ScmObj** %stackaddr$makeclosure54723, align 8
%argslist53473$ae488850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54725 = alloca %struct.ScmObj*, align 8
%argslist53473$ae488851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48887, %struct.ScmObj* %argslist53473$ae488850)
store volatile %struct.ScmObj* %argslist53473$ae488851, %struct.ScmObj** %stackaddr$prim54725, align 8
%stackaddr$prim54726 = alloca %struct.ScmObj*, align 8
%argslist53473$ae488852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48886, %struct.ScmObj* %argslist53473$ae488851)
store volatile %struct.ScmObj* %argslist53473$ae488852, %struct.ScmObj** %stackaddr$prim54726, align 8
%clofunc54727 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48885)
musttail call tailcc void %clofunc54727(%struct.ScmObj* %ae48885, %struct.ScmObj* %argslist53473$ae488852)
ret void
}

define tailcc void @proc_clo$ae48885(%struct.ScmObj* %env$ae48885,%struct.ScmObj* %current_45args53446) {
%stackaddr$env-ref54728 = alloca %struct.ScmObj*, align 8
%lsts47174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48885, i64 0)
store %struct.ScmObj* %lsts47174, %struct.ScmObj** %stackaddr$env-ref54728
%stackaddr$env-ref54729 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48885, i64 1)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref54729
%stackaddr$env-ref54730 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48885, i64 2)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref54730
%stackaddr$env-ref54731 = alloca %struct.ScmObj*, align 8
%k47395 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48885, i64 3)
store %struct.ScmObj* %k47395, %struct.ScmObj** %stackaddr$env-ref54731
%stackaddr$env-ref54732 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48885, i64 4)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54732
%stackaddr$env-ref54733 = alloca %struct.ScmObj*, align 8
%lsts_4347181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48885, i64 5)
store %struct.ScmObj* %lsts_4347181, %struct.ScmObj** %stackaddr$env-ref54733
%stackaddr$env-ref54734 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48885, i64 6)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref54734
%stackaddr$env-ref54735 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48885, i64 7)
store %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$env-ref54735
%stackaddr$prim54736 = alloca %struct.ScmObj*, align 8
%_95k47400 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53446)
store volatile %struct.ScmObj* %_95k47400, %struct.ScmObj** %stackaddr$prim54736, align 8
%stackaddr$prim54737 = alloca %struct.ScmObj*, align 8
%current_45args53447 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53446)
store volatile %struct.ScmObj* %current_45args53447, %struct.ScmObj** %stackaddr$prim54737, align 8
%stackaddr$prim54738 = alloca %struct.ScmObj*, align 8
%anf_45bind47252 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53447)
store volatile %struct.ScmObj* %anf_45bind47252, %struct.ScmObj** %stackaddr$prim54738, align 8
%stackaddr$makeclosure54739 = alloca %struct.ScmObj*, align 8
%fptrToInt54740 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48906 to i64
%ae48906 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt54740)
store volatile %struct.ScmObj* %ae48906, %struct.ScmObj** %stackaddr$makeclosure54739, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48906, %struct.ScmObj* %_37foldr47094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48906, %struct.ScmObj* %_37foldl47172, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48906, %struct.ScmObj* %k47395, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48906, %struct.ScmObj* %lsts_4347181, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48906, %struct.ScmObj* %f47176, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48906, %struct.ScmObj* %acc47175, i64 5)
%argslist53468$_37map1471200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54741 = alloca %struct.ScmObj*, align 8
%argslist53468$_37map1471201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47174, %struct.ScmObj* %argslist53468$_37map1471200)
store volatile %struct.ScmObj* %argslist53468$_37map1471201, %struct.ScmObj** %stackaddr$prim54741, align 8
%stackaddr$prim54742 = alloca %struct.ScmObj*, align 8
%argslist53468$_37map1471202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47252, %struct.ScmObj* %argslist53468$_37map1471201)
store volatile %struct.ScmObj* %argslist53468$_37map1471202, %struct.ScmObj** %stackaddr$prim54742, align 8
%stackaddr$prim54743 = alloca %struct.ScmObj*, align 8
%argslist53468$_37map1471203 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48906, %struct.ScmObj* %argslist53468$_37map1471202)
store volatile %struct.ScmObj* %argslist53468$_37map1471203, %struct.ScmObj** %stackaddr$prim54743, align 8
%clofunc54744 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147120)
musttail call tailcc void %clofunc54744(%struct.ScmObj* %_37map147120, %struct.ScmObj* %argslist53468$_37map1471203)
ret void
}

define tailcc void @proc_clo$ae48906(%struct.ScmObj* %env$ae48906,%struct.ScmObj* %current_45args53449) {
%stackaddr$env-ref54745 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48906, i64 0)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref54745
%stackaddr$env-ref54746 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48906, i64 1)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref54746
%stackaddr$env-ref54747 = alloca %struct.ScmObj*, align 8
%k47395 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48906, i64 2)
store %struct.ScmObj* %k47395, %struct.ScmObj** %stackaddr$env-ref54747
%stackaddr$env-ref54748 = alloca %struct.ScmObj*, align 8
%lsts_4347181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48906, i64 3)
store %struct.ScmObj* %lsts_4347181, %struct.ScmObj** %stackaddr$env-ref54748
%stackaddr$env-ref54749 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48906, i64 4)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref54749
%stackaddr$env-ref54750 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48906, i64 5)
store %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$env-ref54750
%stackaddr$prim54751 = alloca %struct.ScmObj*, align 8
%_95k47401 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53449)
store volatile %struct.ScmObj* %_95k47401, %struct.ScmObj** %stackaddr$prim54751, align 8
%stackaddr$prim54752 = alloca %struct.ScmObj*, align 8
%current_45args53450 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53449)
store volatile %struct.ScmObj* %current_45args53450, %struct.ScmObj** %stackaddr$prim54752, align 8
%stackaddr$prim54753 = alloca %struct.ScmObj*, align 8
%vs47179 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53450)
store volatile %struct.ScmObj* %vs47179, %struct.ScmObj** %stackaddr$prim54753, align 8
%stackaddr$makeclosure54754 = alloca %struct.ScmObj*, align 8
%fptrToInt54755 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48909 to i64
%ae48909 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt54755)
store volatile %struct.ScmObj* %ae48909, %struct.ScmObj** %stackaddr$makeclosure54754, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48909, %struct.ScmObj* %_37foldr47094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48909, %struct.ScmObj* %_37foldl47172, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48909, %struct.ScmObj* %k47395, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48909, %struct.ScmObj* %lsts_4347181, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48909, %struct.ScmObj* %vs47179, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48909, %struct.ScmObj* %f47176, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48909, %struct.ScmObj* %acc47175, i64 6)
%ae48910 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54756 = alloca %struct.ScmObj*, align 8
%fptrToInt54757 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48911 to i64
%ae48911 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54757)
store volatile %struct.ScmObj* %ae48911, %struct.ScmObj** %stackaddr$makeclosure54756, align 8
%argslist53467$ae489090 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54758 = alloca %struct.ScmObj*, align 8
%argslist53467$ae489091 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48911, %struct.ScmObj* %argslist53467$ae489090)
store volatile %struct.ScmObj* %argslist53467$ae489091, %struct.ScmObj** %stackaddr$prim54758, align 8
%stackaddr$prim54759 = alloca %struct.ScmObj*, align 8
%argslist53467$ae489092 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48910, %struct.ScmObj* %argslist53467$ae489091)
store volatile %struct.ScmObj* %argslist53467$ae489092, %struct.ScmObj** %stackaddr$prim54759, align 8
%clofunc54760 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48909)
musttail call tailcc void %clofunc54760(%struct.ScmObj* %ae48909, %struct.ScmObj* %argslist53467$ae489092)
ret void
}

define tailcc void @proc_clo$ae48909(%struct.ScmObj* %env$ae48909,%struct.ScmObj* %current_45args53452) {
%stackaddr$env-ref54761 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48909, i64 0)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref54761
%stackaddr$env-ref54762 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48909, i64 1)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref54762
%stackaddr$env-ref54763 = alloca %struct.ScmObj*, align 8
%k47395 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48909, i64 2)
store %struct.ScmObj* %k47395, %struct.ScmObj** %stackaddr$env-ref54763
%stackaddr$env-ref54764 = alloca %struct.ScmObj*, align 8
%lsts_4347181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48909, i64 3)
store %struct.ScmObj* %lsts_4347181, %struct.ScmObj** %stackaddr$env-ref54764
%stackaddr$env-ref54765 = alloca %struct.ScmObj*, align 8
%vs47179 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48909, i64 4)
store %struct.ScmObj* %vs47179, %struct.ScmObj** %stackaddr$env-ref54765
%stackaddr$env-ref54766 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48909, i64 5)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref54766
%stackaddr$env-ref54767 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48909, i64 6)
store %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$env-ref54767
%stackaddr$prim54768 = alloca %struct.ScmObj*, align 8
%_95k47402 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53452)
store volatile %struct.ScmObj* %_95k47402, %struct.ScmObj** %stackaddr$prim54768, align 8
%stackaddr$prim54769 = alloca %struct.ScmObj*, align 8
%current_45args53453 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53452)
store volatile %struct.ScmObj* %current_45args53453, %struct.ScmObj** %stackaddr$prim54769, align 8
%stackaddr$prim54770 = alloca %struct.ScmObj*, align 8
%anf_45bind47253 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53453)
store volatile %struct.ScmObj* %anf_45bind47253, %struct.ScmObj** %stackaddr$prim54770, align 8
%ae48932 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54771 = alloca %struct.ScmObj*, align 8
%anf_45bind47254 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47175, %struct.ScmObj* %ae48932)
store volatile %struct.ScmObj* %anf_45bind47254, %struct.ScmObj** %stackaddr$prim54771, align 8
%stackaddr$makeclosure54772 = alloca %struct.ScmObj*, align 8
%fptrToInt54773 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48934 to i64
%ae48934 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54773)
store volatile %struct.ScmObj* %ae48934, %struct.ScmObj** %stackaddr$makeclosure54772, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48934, %struct.ScmObj* %_37foldl47172, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48934, %struct.ScmObj* %k47395, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48934, %struct.ScmObj* %lsts_4347181, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48934, %struct.ScmObj* %f47176, i64 3)
%argslist53461$_37foldr470940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54774 = alloca %struct.ScmObj*, align 8
%argslist53461$_37foldr470941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs47179, %struct.ScmObj* %argslist53461$_37foldr470940)
store volatile %struct.ScmObj* %argslist53461$_37foldr470941, %struct.ScmObj** %stackaddr$prim54774, align 8
%stackaddr$prim54775 = alloca %struct.ScmObj*, align 8
%argslist53461$_37foldr470942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47254, %struct.ScmObj* %argslist53461$_37foldr470941)
store volatile %struct.ScmObj* %argslist53461$_37foldr470942, %struct.ScmObj** %stackaddr$prim54775, align 8
%stackaddr$prim54776 = alloca %struct.ScmObj*, align 8
%argslist53461$_37foldr470943 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47253, %struct.ScmObj* %argslist53461$_37foldr470942)
store volatile %struct.ScmObj* %argslist53461$_37foldr470943, %struct.ScmObj** %stackaddr$prim54776, align 8
%stackaddr$prim54777 = alloca %struct.ScmObj*, align 8
%argslist53461$_37foldr470944 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48934, %struct.ScmObj* %argslist53461$_37foldr470943)
store volatile %struct.ScmObj* %argslist53461$_37foldr470944, %struct.ScmObj** %stackaddr$prim54777, align 8
%clofunc54778 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr47094)
musttail call tailcc void %clofunc54778(%struct.ScmObj* %_37foldr47094, %struct.ScmObj* %argslist53461$_37foldr470944)
ret void
}

define tailcc void @proc_clo$ae48934(%struct.ScmObj* %env$ae48934,%struct.ScmObj* %current_45args53455) {
%stackaddr$env-ref54779 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48934, i64 0)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref54779
%stackaddr$env-ref54780 = alloca %struct.ScmObj*, align 8
%k47395 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48934, i64 1)
store %struct.ScmObj* %k47395, %struct.ScmObj** %stackaddr$env-ref54780
%stackaddr$env-ref54781 = alloca %struct.ScmObj*, align 8
%lsts_4347181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48934, i64 2)
store %struct.ScmObj* %lsts_4347181, %struct.ScmObj** %stackaddr$env-ref54781
%stackaddr$env-ref54782 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48934, i64 3)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref54782
%stackaddr$prim54783 = alloca %struct.ScmObj*, align 8
%_95k47403 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53455)
store volatile %struct.ScmObj* %_95k47403, %struct.ScmObj** %stackaddr$prim54783, align 8
%stackaddr$prim54784 = alloca %struct.ScmObj*, align 8
%current_45args53456 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53455)
store volatile %struct.ScmObj* %current_45args53456, %struct.ScmObj** %stackaddr$prim54784, align 8
%stackaddr$prim54785 = alloca %struct.ScmObj*, align 8
%anf_45bind47255 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53456)
store volatile %struct.ScmObj* %anf_45bind47255, %struct.ScmObj** %stackaddr$prim54785, align 8
%stackaddr$makeclosure54786 = alloca %struct.ScmObj*, align 8
%fptrToInt54787 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48938 to i64
%ae48938 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54787)
store volatile %struct.ScmObj* %ae48938, %struct.ScmObj** %stackaddr$makeclosure54786, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48938, %struct.ScmObj* %_37foldl47172, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48938, %struct.ScmObj* %k47395, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48938, %struct.ScmObj* %lsts_4347181, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48938, %struct.ScmObj* %f47176, i64 3)
%stackaddr$prim54788 = alloca %struct.ScmObj*, align 8
%cpsargs47406 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48938, %struct.ScmObj* %anf_45bind47255)
store volatile %struct.ScmObj* %cpsargs47406, %struct.ScmObj** %stackaddr$prim54788, align 8
%clofunc54789 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47176)
musttail call tailcc void %clofunc54789(%struct.ScmObj* %f47176, %struct.ScmObj* %cpsargs47406)
ret void
}

define tailcc void @proc_clo$ae48938(%struct.ScmObj* %env$ae48938,%struct.ScmObj* %current_45args53458) {
%stackaddr$env-ref54790 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48938, i64 0)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref54790
%stackaddr$env-ref54791 = alloca %struct.ScmObj*, align 8
%k47395 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48938, i64 1)
store %struct.ScmObj* %k47395, %struct.ScmObj** %stackaddr$env-ref54791
%stackaddr$env-ref54792 = alloca %struct.ScmObj*, align 8
%lsts_4347181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48938, i64 2)
store %struct.ScmObj* %lsts_4347181, %struct.ScmObj** %stackaddr$env-ref54792
%stackaddr$env-ref54793 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48938, i64 3)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref54793
%stackaddr$prim54794 = alloca %struct.ScmObj*, align 8
%_95k47404 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53458)
store volatile %struct.ScmObj* %_95k47404, %struct.ScmObj** %stackaddr$prim54794, align 8
%stackaddr$prim54795 = alloca %struct.ScmObj*, align 8
%current_45args53459 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53458)
store volatile %struct.ScmObj* %current_45args53459, %struct.ScmObj** %stackaddr$prim54795, align 8
%stackaddr$prim54796 = alloca %struct.ScmObj*, align 8
%acc_4347183 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53459)
store volatile %struct.ScmObj* %acc_4347183, %struct.ScmObj** %stackaddr$prim54796, align 8
%stackaddr$prim54797 = alloca %struct.ScmObj*, align 8
%anf_45bind47256 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4347183, %struct.ScmObj* %lsts_4347181)
store volatile %struct.ScmObj* %anf_45bind47256, %struct.ScmObj** %stackaddr$prim54797, align 8
%stackaddr$prim54798 = alloca %struct.ScmObj*, align 8
%anf_45bind47257 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47176, %struct.ScmObj* %anf_45bind47256)
store volatile %struct.ScmObj* %anf_45bind47257, %struct.ScmObj** %stackaddr$prim54798, align 8
%stackaddr$prim54799 = alloca %struct.ScmObj*, align 8
%cpsargs47405 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47395, %struct.ScmObj* %anf_45bind47257)
store volatile %struct.ScmObj* %cpsargs47405, %struct.ScmObj** %stackaddr$prim54799, align 8
%clofunc54800 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl47172)
musttail call tailcc void %clofunc54800(%struct.ScmObj* %_37foldl47172, %struct.ScmObj* %cpsargs47405)
ret void
}

define tailcc void @proc_clo$ae48911(%struct.ScmObj* %env$ae48911,%struct.ScmObj* %current_45args53462) {
%stackaddr$prim54801 = alloca %struct.ScmObj*, align 8
%k47407 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53462)
store volatile %struct.ScmObj* %k47407, %struct.ScmObj** %stackaddr$prim54801, align 8
%stackaddr$prim54802 = alloca %struct.ScmObj*, align 8
%current_45args53463 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53462)
store volatile %struct.ScmObj* %current_45args53463, %struct.ScmObj** %stackaddr$prim54802, align 8
%stackaddr$prim54803 = alloca %struct.ScmObj*, align 8
%a47185 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53463)
store volatile %struct.ScmObj* %a47185, %struct.ScmObj** %stackaddr$prim54803, align 8
%stackaddr$prim54804 = alloca %struct.ScmObj*, align 8
%current_45args53464 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53463)
store volatile %struct.ScmObj* %current_45args53464, %struct.ScmObj** %stackaddr$prim54804, align 8
%stackaddr$prim54805 = alloca %struct.ScmObj*, align 8
%b47184 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53464)
store volatile %struct.ScmObj* %b47184, %struct.ScmObj** %stackaddr$prim54805, align 8
%stackaddr$prim54806 = alloca %struct.ScmObj*, align 8
%cpsprim47408 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a47185, %struct.ScmObj* %b47184)
store volatile %struct.ScmObj* %cpsprim47408, %struct.ScmObj** %stackaddr$prim54806, align 8
%ae48915 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53466$k474070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54807 = alloca %struct.ScmObj*, align 8
%argslist53466$k474071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47408, %struct.ScmObj* %argslist53466$k474070)
store volatile %struct.ScmObj* %argslist53466$k474071, %struct.ScmObj** %stackaddr$prim54807, align 8
%stackaddr$prim54808 = alloca %struct.ScmObj*, align 8
%argslist53466$k474072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48915, %struct.ScmObj* %argslist53466$k474071)
store volatile %struct.ScmObj* %argslist53466$k474072, %struct.ScmObj** %stackaddr$prim54808, align 8
%clofunc54809 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47407)
musttail call tailcc void %clofunc54809(%struct.ScmObj* %k47407, %struct.ScmObj* %argslist53466$k474072)
ret void
}

define tailcc void @proc_clo$ae48887(%struct.ScmObj* %env$ae48887,%struct.ScmObj* %current_45args53469) {
%stackaddr$prim54810 = alloca %struct.ScmObj*, align 8
%k47409 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53469)
store volatile %struct.ScmObj* %k47409, %struct.ScmObj** %stackaddr$prim54810, align 8
%stackaddr$prim54811 = alloca %struct.ScmObj*, align 8
%current_45args53470 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53469)
store volatile %struct.ScmObj* %current_45args53470, %struct.ScmObj** %stackaddr$prim54811, align 8
%stackaddr$prim54812 = alloca %struct.ScmObj*, align 8
%x47180 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53470)
store volatile %struct.ScmObj* %x47180, %struct.ScmObj** %stackaddr$prim54812, align 8
%stackaddr$prim54813 = alloca %struct.ScmObj*, align 8
%cpsprim47410 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47180)
store volatile %struct.ScmObj* %cpsprim47410, %struct.ScmObj** %stackaddr$prim54813, align 8
%ae48890 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53472$k474090 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54814 = alloca %struct.ScmObj*, align 8
%argslist53472$k474091 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47410, %struct.ScmObj* %argslist53472$k474090)
store volatile %struct.ScmObj* %argslist53472$k474091, %struct.ScmObj** %stackaddr$prim54814, align 8
%stackaddr$prim54815 = alloca %struct.ScmObj*, align 8
%argslist53472$k474092 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48890, %struct.ScmObj* %argslist53472$k474091)
store volatile %struct.ScmObj* %argslist53472$k474092, %struct.ScmObj** %stackaddr$prim54815, align 8
%clofunc54816 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47409)
musttail call tailcc void %clofunc54816(%struct.ScmObj* %k47409, %struct.ScmObj* %argslist53472$k474092)
ret void
}

define tailcc void @proc_clo$ae48863(%struct.ScmObj* %env$ae48863,%struct.ScmObj* %current_45args53475) {
%stackaddr$prim54817 = alloca %struct.ScmObj*, align 8
%k47411 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53475)
store volatile %struct.ScmObj* %k47411, %struct.ScmObj** %stackaddr$prim54817, align 8
%stackaddr$prim54818 = alloca %struct.ScmObj*, align 8
%current_45args53476 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53475)
store volatile %struct.ScmObj* %current_45args53476, %struct.ScmObj** %stackaddr$prim54818, align 8
%stackaddr$prim54819 = alloca %struct.ScmObj*, align 8
%x47182 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53476)
store volatile %struct.ScmObj* %x47182, %struct.ScmObj** %stackaddr$prim54819, align 8
%stackaddr$prim54820 = alloca %struct.ScmObj*, align 8
%cpsprim47412 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47182)
store volatile %struct.ScmObj* %cpsprim47412, %struct.ScmObj** %stackaddr$prim54820, align 8
%ae48866 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53478$k474110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54821 = alloca %struct.ScmObj*, align 8
%argslist53478$k474111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47412, %struct.ScmObj* %argslist53478$k474110)
store volatile %struct.ScmObj* %argslist53478$k474111, %struct.ScmObj** %stackaddr$prim54821, align 8
%stackaddr$prim54822 = alloca %struct.ScmObj*, align 8
%argslist53478$k474112 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48866, %struct.ScmObj* %argslist53478$k474111)
store volatile %struct.ScmObj* %argslist53478$k474112, %struct.ScmObj** %stackaddr$prim54822, align 8
%clofunc54823 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47411)
musttail call tailcc void %clofunc54823(%struct.ScmObj* %k47411, %struct.ScmObj* %argslist53478$k474112)
ret void
}

define tailcc void @proc_clo$ae48815(%struct.ScmObj* %env$ae48815,%struct.ScmObj* %current_45args53481) {
%stackaddr$prim54824 = alloca %struct.ScmObj*, align 8
%k47413 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53481)
store volatile %struct.ScmObj* %k47413, %struct.ScmObj** %stackaddr$prim54824, align 8
%stackaddr$prim54825 = alloca %struct.ScmObj*, align 8
%current_45args53482 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53481)
store volatile %struct.ScmObj* %current_45args53482, %struct.ScmObj** %stackaddr$prim54825, align 8
%stackaddr$prim54826 = alloca %struct.ScmObj*, align 8
%lst47178 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53482)
store volatile %struct.ScmObj* %lst47178, %struct.ScmObj** %stackaddr$prim54826, align 8
%stackaddr$prim54827 = alloca %struct.ScmObj*, align 8
%current_45args53483 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53482)
store volatile %struct.ScmObj* %current_45args53483, %struct.ScmObj** %stackaddr$prim54827, align 8
%stackaddr$prim54828 = alloca %struct.ScmObj*, align 8
%b47177 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53483)
store volatile %struct.ScmObj* %b47177, %struct.ScmObj** %stackaddr$prim54828, align 8
%truthy$cmp54829 = call i64 @is_truthy_value(%struct.ScmObj* %b47177)
%cmp$cmp54829 = icmp eq i64 %truthy$cmp54829, 1
br i1 %cmp$cmp54829, label %truebranch$cmp54829, label %falsebranch$cmp54829
truebranch$cmp54829:
%ae48818 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53485$k474130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54830 = alloca %struct.ScmObj*, align 8
%argslist53485$k474131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b47177, %struct.ScmObj* %argslist53485$k474130)
store volatile %struct.ScmObj* %argslist53485$k474131, %struct.ScmObj** %stackaddr$prim54830, align 8
%stackaddr$prim54831 = alloca %struct.ScmObj*, align 8
%argslist53485$k474132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48818, %struct.ScmObj* %argslist53485$k474131)
store volatile %struct.ScmObj* %argslist53485$k474132, %struct.ScmObj** %stackaddr$prim54831, align 8
%clofunc54832 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47413)
musttail call tailcc void %clofunc54832(%struct.ScmObj* %k47413, %struct.ScmObj* %argslist53485$k474132)
ret void
falsebranch$cmp54829:
%stackaddr$prim54833 = alloca %struct.ScmObj*, align 8
%cpsprim47414 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47178)
store volatile %struct.ScmObj* %cpsprim47414, %struct.ScmObj** %stackaddr$prim54833, align 8
%ae48825 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53486$k474130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54834 = alloca %struct.ScmObj*, align 8
%argslist53486$k474131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47414, %struct.ScmObj* %argslist53486$k474130)
store volatile %struct.ScmObj* %argslist53486$k474131, %struct.ScmObj** %stackaddr$prim54834, align 8
%stackaddr$prim54835 = alloca %struct.ScmObj*, align 8
%argslist53486$k474132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48825, %struct.ScmObj* %argslist53486$k474131)
store volatile %struct.ScmObj* %argslist53486$k474132, %struct.ScmObj** %stackaddr$prim54835, align 8
%clofunc54836 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47413)
musttail call tailcc void %clofunc54836(%struct.ScmObj* %k47413, %struct.ScmObj* %argslist53486$k474132)
ret void
}

define tailcc void @proc_clo$ae48656(%struct.ScmObj* %env$ae48656,%struct.ScmObj* %args4711647415) {
%stackaddr$env-ref54837 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48656, i64 0)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref54837
%stackaddr$env-ref54838 = alloca %struct.ScmObj*, align 8
%_37drop_45right47108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48656, i64 1)
store %struct.ScmObj* %_37drop_45right47108, %struct.ScmObj** %stackaddr$env-ref54838
%stackaddr$env-ref54839 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48656, i64 2)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref54839
%stackaddr$prim54840 = alloca %struct.ScmObj*, align 8
%k47416 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4711647415)
store volatile %struct.ScmObj* %k47416, %struct.ScmObj** %stackaddr$prim54840, align 8
%stackaddr$prim54841 = alloca %struct.ScmObj*, align 8
%args47116 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4711647415)
store volatile %struct.ScmObj* %args47116, %struct.ScmObj** %stackaddr$prim54841, align 8
%stackaddr$prim54842 = alloca %struct.ScmObj*, align 8
%f47118 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47116)
store volatile %struct.ScmObj* %f47118, %struct.ScmObj** %stackaddr$prim54842, align 8
%stackaddr$prim54843 = alloca %struct.ScmObj*, align 8
%lsts47117 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47116)
store volatile %struct.ScmObj* %lsts47117, %struct.ScmObj** %stackaddr$prim54843, align 8
%stackaddr$makeclosure54844 = alloca %struct.ScmObj*, align 8
%fptrToInt54845 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48661 to i64
%ae48661 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54845)
store volatile %struct.ScmObj* %ae48661, %struct.ScmObj** %stackaddr$makeclosure54844, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48661, %struct.ScmObj* %_37foldr47094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48661, %struct.ScmObj* %lsts47117, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48661, %struct.ScmObj* %k47416, i64 2)
%ae48662 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54846 = alloca %struct.ScmObj*, align 8
%fptrToInt54847 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48663 to i64
%ae48663 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54847)
store volatile %struct.ScmObj* %ae48663, %struct.ScmObj** %stackaddr$makeclosure54846, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48663, %struct.ScmObj* %_37drop_45right47108, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48663, %struct.ScmObj* %f47118, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48663, %struct.ScmObj* %_37last47111, i64 2)
%argslist53505$ae486610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54848 = alloca %struct.ScmObj*, align 8
%argslist53505$ae486611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48663, %struct.ScmObj* %argslist53505$ae486610)
store volatile %struct.ScmObj* %argslist53505$ae486611, %struct.ScmObj** %stackaddr$prim54848, align 8
%stackaddr$prim54849 = alloca %struct.ScmObj*, align 8
%argslist53505$ae486612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48662, %struct.ScmObj* %argslist53505$ae486611)
store volatile %struct.ScmObj* %argslist53505$ae486612, %struct.ScmObj** %stackaddr$prim54849, align 8
%clofunc54850 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48661)
musttail call tailcc void %clofunc54850(%struct.ScmObj* %ae48661, %struct.ScmObj* %argslist53505$ae486612)
ret void
}

define tailcc void @proc_clo$ae48661(%struct.ScmObj* %env$ae48661,%struct.ScmObj* %current_45args53490) {
%stackaddr$env-ref54851 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48661, i64 0)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref54851
%stackaddr$env-ref54852 = alloca %struct.ScmObj*, align 8
%lsts47117 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48661, i64 1)
store %struct.ScmObj* %lsts47117, %struct.ScmObj** %stackaddr$env-ref54852
%stackaddr$env-ref54853 = alloca %struct.ScmObj*, align 8
%k47416 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48661, i64 2)
store %struct.ScmObj* %k47416, %struct.ScmObj** %stackaddr$env-ref54853
%stackaddr$prim54854 = alloca %struct.ScmObj*, align 8
%_95k47417 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53490)
store volatile %struct.ScmObj* %_95k47417, %struct.ScmObj** %stackaddr$prim54854, align 8
%stackaddr$prim54855 = alloca %struct.ScmObj*, align 8
%current_45args53491 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53490)
store volatile %struct.ScmObj* %current_45args53491, %struct.ScmObj** %stackaddr$prim54855, align 8
%stackaddr$prim54856 = alloca %struct.ScmObj*, align 8
%anf_45bind47244 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53491)
store volatile %struct.ScmObj* %anf_45bind47244, %struct.ScmObj** %stackaddr$prim54856, align 8
%ae48724 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54857 = alloca %struct.ScmObj*, align 8
%anf_45bind47245 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48724, %struct.ScmObj* %lsts47117)
store volatile %struct.ScmObj* %anf_45bind47245, %struct.ScmObj** %stackaddr$prim54857, align 8
%stackaddr$prim54858 = alloca %struct.ScmObj*, align 8
%anf_45bind47246 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47244, %struct.ScmObj* %anf_45bind47245)
store volatile %struct.ScmObj* %anf_45bind47246, %struct.ScmObj** %stackaddr$prim54858, align 8
%stackaddr$prim54859 = alloca %struct.ScmObj*, align 8
%cpsargs47418 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47416, %struct.ScmObj* %anf_45bind47246)
store volatile %struct.ScmObj* %cpsargs47418, %struct.ScmObj** %stackaddr$prim54859, align 8
%clofunc54860 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr47094)
musttail call tailcc void %clofunc54860(%struct.ScmObj* %_37foldr47094, %struct.ScmObj* %cpsargs47418)
ret void
}

define tailcc void @proc_clo$ae48663(%struct.ScmObj* %env$ae48663,%struct.ScmObj* %fargs4711947419) {
%stackaddr$env-ref54861 = alloca %struct.ScmObj*, align 8
%_37drop_45right47108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48663, i64 0)
store %struct.ScmObj* %_37drop_45right47108, %struct.ScmObj** %stackaddr$env-ref54861
%stackaddr$env-ref54862 = alloca %struct.ScmObj*, align 8
%f47118 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48663, i64 1)
store %struct.ScmObj* %f47118, %struct.ScmObj** %stackaddr$env-ref54862
%stackaddr$env-ref54863 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48663, i64 2)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref54863
%stackaddr$prim54864 = alloca %struct.ScmObj*, align 8
%k47420 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs4711947419)
store volatile %struct.ScmObj* %k47420, %struct.ScmObj** %stackaddr$prim54864, align 8
%stackaddr$prim54865 = alloca %struct.ScmObj*, align 8
%fargs47119 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs4711947419)
store volatile %struct.ScmObj* %fargs47119, %struct.ScmObj** %stackaddr$prim54865, align 8
%stackaddr$makeclosure54866 = alloca %struct.ScmObj*, align 8
%fptrToInt54867 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48667 to i64
%ae48667 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54867)
store volatile %struct.ScmObj* %ae48667, %struct.ScmObj** %stackaddr$makeclosure54866, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48667, %struct.ScmObj* %fargs47119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48667, %struct.ScmObj* %f47118, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48667, %struct.ScmObj* %k47420, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48667, %struct.ScmObj* %_37last47111, i64 3)
%ae48669 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist53504$_37drop_45right471080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54868 = alloca %struct.ScmObj*, align 8
%argslist53504$_37drop_45right471081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48669, %struct.ScmObj* %argslist53504$_37drop_45right471080)
store volatile %struct.ScmObj* %argslist53504$_37drop_45right471081, %struct.ScmObj** %stackaddr$prim54868, align 8
%stackaddr$prim54869 = alloca %struct.ScmObj*, align 8
%argslist53504$_37drop_45right471082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs47119, %struct.ScmObj* %argslist53504$_37drop_45right471081)
store volatile %struct.ScmObj* %argslist53504$_37drop_45right471082, %struct.ScmObj** %stackaddr$prim54869, align 8
%stackaddr$prim54870 = alloca %struct.ScmObj*, align 8
%argslist53504$_37drop_45right471083 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48667, %struct.ScmObj* %argslist53504$_37drop_45right471082)
store volatile %struct.ScmObj* %argslist53504$_37drop_45right471083, %struct.ScmObj** %stackaddr$prim54870, align 8
%clofunc54871 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right47108)
musttail call tailcc void %clofunc54871(%struct.ScmObj* %_37drop_45right47108, %struct.ScmObj* %argslist53504$_37drop_45right471083)
ret void
}

define tailcc void @proc_clo$ae48667(%struct.ScmObj* %env$ae48667,%struct.ScmObj* %current_45args53493) {
%stackaddr$env-ref54872 = alloca %struct.ScmObj*, align 8
%fargs47119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48667, i64 0)
store %struct.ScmObj* %fargs47119, %struct.ScmObj** %stackaddr$env-ref54872
%stackaddr$env-ref54873 = alloca %struct.ScmObj*, align 8
%f47118 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48667, i64 1)
store %struct.ScmObj* %f47118, %struct.ScmObj** %stackaddr$env-ref54873
%stackaddr$env-ref54874 = alloca %struct.ScmObj*, align 8
%k47420 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48667, i64 2)
store %struct.ScmObj* %k47420, %struct.ScmObj** %stackaddr$env-ref54874
%stackaddr$env-ref54875 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48667, i64 3)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref54875
%stackaddr$prim54876 = alloca %struct.ScmObj*, align 8
%_95k47421 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53493)
store volatile %struct.ScmObj* %_95k47421, %struct.ScmObj** %stackaddr$prim54876, align 8
%stackaddr$prim54877 = alloca %struct.ScmObj*, align 8
%current_45args53494 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53493)
store volatile %struct.ScmObj* %current_45args53494, %struct.ScmObj** %stackaddr$prim54877, align 8
%stackaddr$prim54878 = alloca %struct.ScmObj*, align 8
%anf_45bind47241 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53494)
store volatile %struct.ScmObj* %anf_45bind47241, %struct.ScmObj** %stackaddr$prim54878, align 8
%stackaddr$makeclosure54879 = alloca %struct.ScmObj*, align 8
%fptrToInt54880 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48674 to i64
%ae48674 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54880)
store volatile %struct.ScmObj* %ae48674, %struct.ScmObj** %stackaddr$makeclosure54879, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48674, %struct.ScmObj* %fargs47119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48674, %struct.ScmObj* %k47420, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48674, %struct.ScmObj* %_37last47111, i64 2)
%stackaddr$prim54881 = alloca %struct.ScmObj*, align 8
%cpsargs47425 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48674, %struct.ScmObj* %anf_45bind47241)
store volatile %struct.ScmObj* %cpsargs47425, %struct.ScmObj** %stackaddr$prim54881, align 8
%clofunc54882 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47118)
musttail call tailcc void %clofunc54882(%struct.ScmObj* %f47118, %struct.ScmObj* %cpsargs47425)
ret void
}

define tailcc void @proc_clo$ae48674(%struct.ScmObj* %env$ae48674,%struct.ScmObj* %current_45args53496) {
%stackaddr$env-ref54883 = alloca %struct.ScmObj*, align 8
%fargs47119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48674, i64 0)
store %struct.ScmObj* %fargs47119, %struct.ScmObj** %stackaddr$env-ref54883
%stackaddr$env-ref54884 = alloca %struct.ScmObj*, align 8
%k47420 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48674, i64 1)
store %struct.ScmObj* %k47420, %struct.ScmObj** %stackaddr$env-ref54884
%stackaddr$env-ref54885 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48674, i64 2)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref54885
%stackaddr$prim54886 = alloca %struct.ScmObj*, align 8
%_95k47422 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53496)
store volatile %struct.ScmObj* %_95k47422, %struct.ScmObj** %stackaddr$prim54886, align 8
%stackaddr$prim54887 = alloca %struct.ScmObj*, align 8
%current_45args53497 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53496)
store volatile %struct.ScmObj* %current_45args53497, %struct.ScmObj** %stackaddr$prim54887, align 8
%stackaddr$prim54888 = alloca %struct.ScmObj*, align 8
%anf_45bind47242 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53497)
store volatile %struct.ScmObj* %anf_45bind47242, %struct.ScmObj** %stackaddr$prim54888, align 8
%stackaddr$makeclosure54889 = alloca %struct.ScmObj*, align 8
%fptrToInt54890 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48679 to i64
%ae48679 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54890)
store volatile %struct.ScmObj* %ae48679, %struct.ScmObj** %stackaddr$makeclosure54889, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48679, %struct.ScmObj* %k47420, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48679, %struct.ScmObj* %anf_45bind47242, i64 1)
%argslist53503$_37last471110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54891 = alloca %struct.ScmObj*, align 8
%argslist53503$_37last471111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs47119, %struct.ScmObj* %argslist53503$_37last471110)
store volatile %struct.ScmObj* %argslist53503$_37last471111, %struct.ScmObj** %stackaddr$prim54891, align 8
%stackaddr$prim54892 = alloca %struct.ScmObj*, align 8
%argslist53503$_37last471112 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48679, %struct.ScmObj* %argslist53503$_37last471111)
store volatile %struct.ScmObj* %argslist53503$_37last471112, %struct.ScmObj** %stackaddr$prim54892, align 8
%clofunc54893 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last47111)
musttail call tailcc void %clofunc54893(%struct.ScmObj* %_37last47111, %struct.ScmObj* %argslist53503$_37last471112)
ret void
}

define tailcc void @proc_clo$ae48679(%struct.ScmObj* %env$ae48679,%struct.ScmObj* %current_45args53499) {
%stackaddr$env-ref54894 = alloca %struct.ScmObj*, align 8
%k47420 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48679, i64 0)
store %struct.ScmObj* %k47420, %struct.ScmObj** %stackaddr$env-ref54894
%stackaddr$env-ref54895 = alloca %struct.ScmObj*, align 8
%anf_45bind47242 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48679, i64 1)
store %struct.ScmObj* %anf_45bind47242, %struct.ScmObj** %stackaddr$env-ref54895
%stackaddr$prim54896 = alloca %struct.ScmObj*, align 8
%_95k47423 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53499)
store volatile %struct.ScmObj* %_95k47423, %struct.ScmObj** %stackaddr$prim54896, align 8
%stackaddr$prim54897 = alloca %struct.ScmObj*, align 8
%current_45args53500 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53499)
store volatile %struct.ScmObj* %current_45args53500, %struct.ScmObj** %stackaddr$prim54897, align 8
%stackaddr$prim54898 = alloca %struct.ScmObj*, align 8
%anf_45bind47243 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53500)
store volatile %struct.ScmObj* %anf_45bind47243, %struct.ScmObj** %stackaddr$prim54898, align 8
%stackaddr$prim54899 = alloca %struct.ScmObj*, align 8
%cpsprim47424 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47242, %struct.ScmObj* %anf_45bind47243)
store volatile %struct.ScmObj* %cpsprim47424, %struct.ScmObj** %stackaddr$prim54899, align 8
%ae48684 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53502$k474200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54900 = alloca %struct.ScmObj*, align 8
%argslist53502$k474201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47424, %struct.ScmObj* %argslist53502$k474200)
store volatile %struct.ScmObj* %argslist53502$k474201, %struct.ScmObj** %stackaddr$prim54900, align 8
%stackaddr$prim54901 = alloca %struct.ScmObj*, align 8
%argslist53502$k474202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48684, %struct.ScmObj* %argslist53502$k474201)
store volatile %struct.ScmObj* %argslist53502$k474202, %struct.ScmObj** %stackaddr$prim54901, align 8
%clofunc54902 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47420)
musttail call tailcc void %clofunc54902(%struct.ScmObj* %k47420, %struct.ScmObj* %argslist53502$k474202)
ret void
}

define tailcc void @proc_clo$ae48579(%struct.ScmObj* %env$ae48579,%struct.ScmObj* %current_45args53507) {
%stackaddr$env-ref54903 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48579, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54903
%stackaddr$prim54904 = alloca %struct.ScmObj*, align 8
%k47426 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53507)
store volatile %struct.ScmObj* %k47426, %struct.ScmObj** %stackaddr$prim54904, align 8
%stackaddr$prim54905 = alloca %struct.ScmObj*, align 8
%current_45args53508 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53507)
store volatile %struct.ScmObj* %current_45args53508, %struct.ScmObj** %stackaddr$prim54905, align 8
%stackaddr$prim54906 = alloca %struct.ScmObj*, align 8
%f47122 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53508)
store volatile %struct.ScmObj* %f47122, %struct.ScmObj** %stackaddr$prim54906, align 8
%stackaddr$prim54907 = alloca %struct.ScmObj*, align 8
%current_45args53509 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53508)
store volatile %struct.ScmObj* %current_45args53509, %struct.ScmObj** %stackaddr$prim54907, align 8
%stackaddr$prim54908 = alloca %struct.ScmObj*, align 8
%lst47121 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53509)
store volatile %struct.ScmObj* %lst47121, %struct.ScmObj** %stackaddr$prim54908, align 8
%stackaddr$makeclosure54909 = alloca %struct.ScmObj*, align 8
%fptrToInt54910 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48580 to i64
%ae48580 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54910)
store volatile %struct.ScmObj* %ae48580, %struct.ScmObj** %stackaddr$makeclosure54909, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48580, %struct.ScmObj* %lst47121, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48580, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48580, %struct.ScmObj* %k47426, i64 2)
%ae48581 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54911 = alloca %struct.ScmObj*, align 8
%fptrToInt54912 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48582 to i64
%ae48582 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54912)
store volatile %struct.ScmObj* %ae48582, %struct.ScmObj** %stackaddr$makeclosure54911, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48582, %struct.ScmObj* %f47122, i64 0)
%argslist53524$ae485800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54913 = alloca %struct.ScmObj*, align 8
%argslist53524$ae485801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48582, %struct.ScmObj* %argslist53524$ae485800)
store volatile %struct.ScmObj* %argslist53524$ae485801, %struct.ScmObj** %stackaddr$prim54913, align 8
%stackaddr$prim54914 = alloca %struct.ScmObj*, align 8
%argslist53524$ae485802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48581, %struct.ScmObj* %argslist53524$ae485801)
store volatile %struct.ScmObj* %argslist53524$ae485802, %struct.ScmObj** %stackaddr$prim54914, align 8
%clofunc54915 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48580)
musttail call tailcc void %clofunc54915(%struct.ScmObj* %ae48580, %struct.ScmObj* %argslist53524$ae485802)
ret void
}

define tailcc void @proc_clo$ae48580(%struct.ScmObj* %env$ae48580,%struct.ScmObj* %current_45args53511) {
%stackaddr$env-ref54916 = alloca %struct.ScmObj*, align 8
%lst47121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48580, i64 0)
store %struct.ScmObj* %lst47121, %struct.ScmObj** %stackaddr$env-ref54916
%stackaddr$env-ref54917 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48580, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54917
%stackaddr$env-ref54918 = alloca %struct.ScmObj*, align 8
%k47426 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48580, i64 2)
store %struct.ScmObj* %k47426, %struct.ScmObj** %stackaddr$env-ref54918
%stackaddr$prim54919 = alloca %struct.ScmObj*, align 8
%_95k47427 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53511)
store volatile %struct.ScmObj* %_95k47427, %struct.ScmObj** %stackaddr$prim54919, align 8
%stackaddr$prim54920 = alloca %struct.ScmObj*, align 8
%current_45args53512 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53511)
store volatile %struct.ScmObj* %current_45args53512, %struct.ScmObj** %stackaddr$prim54920, align 8
%stackaddr$prim54921 = alloca %struct.ScmObj*, align 8
%anf_45bind47240 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53512)
store volatile %struct.ScmObj* %anf_45bind47240, %struct.ScmObj** %stackaddr$prim54921, align 8
%ae48614 = call %struct.ScmObj* @const_init_null()
%argslist53514$_37foldr1470890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54922 = alloca %struct.ScmObj*, align 8
%argslist53514$_37foldr1470891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47121, %struct.ScmObj* %argslist53514$_37foldr1470890)
store volatile %struct.ScmObj* %argslist53514$_37foldr1470891, %struct.ScmObj** %stackaddr$prim54922, align 8
%stackaddr$prim54923 = alloca %struct.ScmObj*, align 8
%argslist53514$_37foldr1470892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48614, %struct.ScmObj* %argslist53514$_37foldr1470891)
store volatile %struct.ScmObj* %argslist53514$_37foldr1470892, %struct.ScmObj** %stackaddr$prim54923, align 8
%stackaddr$prim54924 = alloca %struct.ScmObj*, align 8
%argslist53514$_37foldr1470893 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47240, %struct.ScmObj* %argslist53514$_37foldr1470892)
store volatile %struct.ScmObj* %argslist53514$_37foldr1470893, %struct.ScmObj** %stackaddr$prim54924, align 8
%stackaddr$prim54925 = alloca %struct.ScmObj*, align 8
%argslist53514$_37foldr1470894 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47426, %struct.ScmObj* %argslist53514$_37foldr1470893)
store volatile %struct.ScmObj* %argslist53514$_37foldr1470894, %struct.ScmObj** %stackaddr$prim54925, align 8
%clofunc54926 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147089)
musttail call tailcc void %clofunc54926(%struct.ScmObj* %_37foldr147089, %struct.ScmObj* %argslist53514$_37foldr1470894)
ret void
}

define tailcc void @proc_clo$ae48582(%struct.ScmObj* %env$ae48582,%struct.ScmObj* %current_45args53515) {
%stackaddr$env-ref54927 = alloca %struct.ScmObj*, align 8
%f47122 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48582, i64 0)
store %struct.ScmObj* %f47122, %struct.ScmObj** %stackaddr$env-ref54927
%stackaddr$prim54928 = alloca %struct.ScmObj*, align 8
%k47428 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53515)
store volatile %struct.ScmObj* %k47428, %struct.ScmObj** %stackaddr$prim54928, align 8
%stackaddr$prim54929 = alloca %struct.ScmObj*, align 8
%current_45args53516 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53515)
store volatile %struct.ScmObj* %current_45args53516, %struct.ScmObj** %stackaddr$prim54929, align 8
%stackaddr$prim54930 = alloca %struct.ScmObj*, align 8
%v47124 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53516)
store volatile %struct.ScmObj* %v47124, %struct.ScmObj** %stackaddr$prim54930, align 8
%stackaddr$prim54931 = alloca %struct.ScmObj*, align 8
%current_45args53517 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53516)
store volatile %struct.ScmObj* %current_45args53517, %struct.ScmObj** %stackaddr$prim54931, align 8
%stackaddr$prim54932 = alloca %struct.ScmObj*, align 8
%r47123 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53517)
store volatile %struct.ScmObj* %r47123, %struct.ScmObj** %stackaddr$prim54932, align 8
%stackaddr$makeclosure54933 = alloca %struct.ScmObj*, align 8
%fptrToInt54934 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48584 to i64
%ae48584 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54934)
store volatile %struct.ScmObj* %ae48584, %struct.ScmObj** %stackaddr$makeclosure54933, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48584, %struct.ScmObj* %k47428, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48584, %struct.ScmObj* %r47123, i64 1)
%argslist53523$f471220 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54935 = alloca %struct.ScmObj*, align 8
%argslist53523$f471221 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v47124, %struct.ScmObj* %argslist53523$f471220)
store volatile %struct.ScmObj* %argslist53523$f471221, %struct.ScmObj** %stackaddr$prim54935, align 8
%stackaddr$prim54936 = alloca %struct.ScmObj*, align 8
%argslist53523$f471222 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48584, %struct.ScmObj* %argslist53523$f471221)
store volatile %struct.ScmObj* %argslist53523$f471222, %struct.ScmObj** %stackaddr$prim54936, align 8
%clofunc54937 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47122)
musttail call tailcc void %clofunc54937(%struct.ScmObj* %f47122, %struct.ScmObj* %argslist53523$f471222)
ret void
}

define tailcc void @proc_clo$ae48584(%struct.ScmObj* %env$ae48584,%struct.ScmObj* %current_45args53519) {
%stackaddr$env-ref54938 = alloca %struct.ScmObj*, align 8
%k47428 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48584, i64 0)
store %struct.ScmObj* %k47428, %struct.ScmObj** %stackaddr$env-ref54938
%stackaddr$env-ref54939 = alloca %struct.ScmObj*, align 8
%r47123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48584, i64 1)
store %struct.ScmObj* %r47123, %struct.ScmObj** %stackaddr$env-ref54939
%stackaddr$prim54940 = alloca %struct.ScmObj*, align 8
%_95k47429 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53519)
store volatile %struct.ScmObj* %_95k47429, %struct.ScmObj** %stackaddr$prim54940, align 8
%stackaddr$prim54941 = alloca %struct.ScmObj*, align 8
%current_45args53520 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53519)
store volatile %struct.ScmObj* %current_45args53520, %struct.ScmObj** %stackaddr$prim54941, align 8
%stackaddr$prim54942 = alloca %struct.ScmObj*, align 8
%anf_45bind47239 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53520)
store volatile %struct.ScmObj* %anf_45bind47239, %struct.ScmObj** %stackaddr$prim54942, align 8
%stackaddr$prim54943 = alloca %struct.ScmObj*, align 8
%cpsprim47430 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47239, %struct.ScmObj* %r47123)
store volatile %struct.ScmObj* %cpsprim47430, %struct.ScmObj** %stackaddr$prim54943, align 8
%ae48589 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53522$k474280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54944 = alloca %struct.ScmObj*, align 8
%argslist53522$k474281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47430, %struct.ScmObj* %argslist53522$k474280)
store volatile %struct.ScmObj* %argslist53522$k474281, %struct.ScmObj** %stackaddr$prim54944, align 8
%stackaddr$prim54945 = alloca %struct.ScmObj*, align 8
%argslist53522$k474282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48589, %struct.ScmObj* %argslist53522$k474281)
store volatile %struct.ScmObj* %argslist53522$k474282, %struct.ScmObj** %stackaddr$prim54945, align 8
%clofunc54946 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47428)
musttail call tailcc void %clofunc54946(%struct.ScmObj* %k47428, %struct.ScmObj* %argslist53522$k474282)
ret void
}

define tailcc void @proc_clo$ae48193(%struct.ScmObj* %env$ae48193,%struct.ScmObj* %current_45args53527) {
%stackaddr$env-ref54947 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48193, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54947
%stackaddr$env-ref54948 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48193, i64 1)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref54948
%stackaddr$prim54949 = alloca %struct.ScmObj*, align 8
%k47431 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53527)
store volatile %struct.ScmObj* %k47431, %struct.ScmObj** %stackaddr$prim54949, align 8
%stackaddr$prim54950 = alloca %struct.ScmObj*, align 8
%current_45args53528 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53527)
store volatile %struct.ScmObj* %current_45args53528, %struct.ScmObj** %stackaddr$prim54950, align 8
%stackaddr$prim54951 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53528)
store volatile %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$prim54951, align 8
%ae48195 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54952 = alloca %struct.ScmObj*, align 8
%fptrToInt54953 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48196 to i64
%ae48196 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54953)
store volatile %struct.ScmObj* %ae48196, %struct.ScmObj** %stackaddr$makeclosure54952, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48196, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48196, %struct.ScmObj* %_37map147085, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48196, %struct.ScmObj* %_37foldr47095, i64 2)
%argslist53585$k474310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54954 = alloca %struct.ScmObj*, align 8
%argslist53585$k474311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48196, %struct.ScmObj* %argslist53585$k474310)
store volatile %struct.ScmObj* %argslist53585$k474311, %struct.ScmObj** %stackaddr$prim54954, align 8
%stackaddr$prim54955 = alloca %struct.ScmObj*, align 8
%argslist53585$k474312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48195, %struct.ScmObj* %argslist53585$k474311)
store volatile %struct.ScmObj* %argslist53585$k474312, %struct.ScmObj** %stackaddr$prim54955, align 8
%clofunc54956 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47431)
musttail call tailcc void %clofunc54956(%struct.ScmObj* %k47431, %struct.ScmObj* %argslist53585$k474312)
ret void
}

define tailcc void @proc_clo$ae48196(%struct.ScmObj* %env$ae48196,%struct.ScmObj* %args4709647432) {
%stackaddr$env-ref54957 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48196, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54957
%stackaddr$env-ref54958 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48196, i64 1)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref54958
%stackaddr$env-ref54959 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48196, i64 2)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref54959
%stackaddr$prim54960 = alloca %struct.ScmObj*, align 8
%k47433 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4709647432)
store volatile %struct.ScmObj* %k47433, %struct.ScmObj** %stackaddr$prim54960, align 8
%stackaddr$prim54961 = alloca %struct.ScmObj*, align 8
%args47096 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4709647432)
store volatile %struct.ScmObj* %args47096, %struct.ScmObj** %stackaddr$prim54961, align 8
%stackaddr$prim54962 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47096)
store volatile %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$prim54962, align 8
%stackaddr$prim54963 = alloca %struct.ScmObj*, align 8
%anf_45bind47226 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47096)
store volatile %struct.ScmObj* %anf_45bind47226, %struct.ScmObj** %stackaddr$prim54963, align 8
%stackaddr$prim54964 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47226)
store volatile %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$prim54964, align 8
%stackaddr$prim54965 = alloca %struct.ScmObj*, align 8
%anf_45bind47227 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47096)
store volatile %struct.ScmObj* %anf_45bind47227, %struct.ScmObj** %stackaddr$prim54965, align 8
%stackaddr$prim54966 = alloca %struct.ScmObj*, align 8
%lsts47097 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47227)
store volatile %struct.ScmObj* %lsts47097, %struct.ScmObj** %stackaddr$prim54966, align 8
%stackaddr$makeclosure54967 = alloca %struct.ScmObj*, align 8
%fptrToInt54968 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48204 to i64
%ae48204 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt54968)
store volatile %struct.ScmObj* %ae48204, %struct.ScmObj** %stackaddr$makeclosure54967, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48204, %struct.ScmObj* %k47433, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48204, %struct.ScmObj* %lsts47097, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48204, %struct.ScmObj* %_37foldr147089, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48204, %struct.ScmObj* %_37map147085, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48204, %struct.ScmObj* %f47099, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48204, %struct.ScmObj* %acc47098, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48204, %struct.ScmObj* %_37foldr47095, i64 6)
%ae48205 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54969 = alloca %struct.ScmObj*, align 8
%fptrToInt54970 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48206 to i64
%ae48206 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54970)
store volatile %struct.ScmObj* %ae48206, %struct.ScmObj** %stackaddr$makeclosure54969, align 8
%argslist53584$ae482040 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54971 = alloca %struct.ScmObj*, align 8
%argslist53584$ae482041 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48206, %struct.ScmObj* %argslist53584$ae482040)
store volatile %struct.ScmObj* %argslist53584$ae482041, %struct.ScmObj** %stackaddr$prim54971, align 8
%stackaddr$prim54972 = alloca %struct.ScmObj*, align 8
%argslist53584$ae482042 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48205, %struct.ScmObj* %argslist53584$ae482041)
store volatile %struct.ScmObj* %argslist53584$ae482042, %struct.ScmObj** %stackaddr$prim54972, align 8
%clofunc54973 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48204)
musttail call tailcc void %clofunc54973(%struct.ScmObj* %ae48204, %struct.ScmObj* %argslist53584$ae482042)
ret void
}

define tailcc void @proc_clo$ae48204(%struct.ScmObj* %env$ae48204,%struct.ScmObj* %current_45args53530) {
%stackaddr$env-ref54974 = alloca %struct.ScmObj*, align 8
%k47433 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48204, i64 0)
store %struct.ScmObj* %k47433, %struct.ScmObj** %stackaddr$env-ref54974
%stackaddr$env-ref54975 = alloca %struct.ScmObj*, align 8
%lsts47097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48204, i64 1)
store %struct.ScmObj* %lsts47097, %struct.ScmObj** %stackaddr$env-ref54975
%stackaddr$env-ref54976 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48204, i64 2)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54976
%stackaddr$env-ref54977 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48204, i64 3)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref54977
%stackaddr$env-ref54978 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48204, i64 4)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref54978
%stackaddr$env-ref54979 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48204, i64 5)
store %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$env-ref54979
%stackaddr$env-ref54980 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48204, i64 6)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref54980
%stackaddr$prim54981 = alloca %struct.ScmObj*, align 8
%_95k47434 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53530)
store volatile %struct.ScmObj* %_95k47434, %struct.ScmObj** %stackaddr$prim54981, align 8
%stackaddr$prim54982 = alloca %struct.ScmObj*, align 8
%current_45args53531 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53530)
store volatile %struct.ScmObj* %current_45args53531, %struct.ScmObj** %stackaddr$prim54982, align 8
%stackaddr$prim54983 = alloca %struct.ScmObj*, align 8
%anf_45bind47228 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53531)
store volatile %struct.ScmObj* %anf_45bind47228, %struct.ScmObj** %stackaddr$prim54983, align 8
%stackaddr$makeclosure54984 = alloca %struct.ScmObj*, align 8
%fptrToInt54985 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48236 to i64
%ae48236 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt54985)
store volatile %struct.ScmObj* %ae48236, %struct.ScmObj** %stackaddr$makeclosure54984, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48236, %struct.ScmObj* %k47433, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48236, %struct.ScmObj* %lsts47097, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48236, %struct.ScmObj* %_37foldr147089, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48236, %struct.ScmObj* %_37map147085, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48236, %struct.ScmObj* %f47099, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48236, %struct.ScmObj* %acc47098, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48236, %struct.ScmObj* %_37foldr47095, i64 6)
%ae48238 = call %struct.ScmObj* @const_init_false()
%argslist53577$_37foldr1470890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54986 = alloca %struct.ScmObj*, align 8
%argslist53577$_37foldr1470891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47097, %struct.ScmObj* %argslist53577$_37foldr1470890)
store volatile %struct.ScmObj* %argslist53577$_37foldr1470891, %struct.ScmObj** %stackaddr$prim54986, align 8
%stackaddr$prim54987 = alloca %struct.ScmObj*, align 8
%argslist53577$_37foldr1470892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48238, %struct.ScmObj* %argslist53577$_37foldr1470891)
store volatile %struct.ScmObj* %argslist53577$_37foldr1470892, %struct.ScmObj** %stackaddr$prim54987, align 8
%stackaddr$prim54988 = alloca %struct.ScmObj*, align 8
%argslist53577$_37foldr1470893 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47228, %struct.ScmObj* %argslist53577$_37foldr1470892)
store volatile %struct.ScmObj* %argslist53577$_37foldr1470893, %struct.ScmObj** %stackaddr$prim54988, align 8
%stackaddr$prim54989 = alloca %struct.ScmObj*, align 8
%argslist53577$_37foldr1470894 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48236, %struct.ScmObj* %argslist53577$_37foldr1470893)
store volatile %struct.ScmObj* %argslist53577$_37foldr1470894, %struct.ScmObj** %stackaddr$prim54989, align 8
%clofunc54990 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147089)
musttail call tailcc void %clofunc54990(%struct.ScmObj* %_37foldr147089, %struct.ScmObj* %argslist53577$_37foldr1470894)
ret void
}

define tailcc void @proc_clo$ae48236(%struct.ScmObj* %env$ae48236,%struct.ScmObj* %current_45args53533) {
%stackaddr$env-ref54991 = alloca %struct.ScmObj*, align 8
%k47433 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48236, i64 0)
store %struct.ScmObj* %k47433, %struct.ScmObj** %stackaddr$env-ref54991
%stackaddr$env-ref54992 = alloca %struct.ScmObj*, align 8
%lsts47097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48236, i64 1)
store %struct.ScmObj* %lsts47097, %struct.ScmObj** %stackaddr$env-ref54992
%stackaddr$env-ref54993 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48236, i64 2)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54993
%stackaddr$env-ref54994 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48236, i64 3)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref54994
%stackaddr$env-ref54995 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48236, i64 4)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref54995
%stackaddr$env-ref54996 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48236, i64 5)
store %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$env-ref54996
%stackaddr$env-ref54997 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48236, i64 6)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref54997
%stackaddr$prim54998 = alloca %struct.ScmObj*, align 8
%_95k47435 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53533)
store volatile %struct.ScmObj* %_95k47435, %struct.ScmObj** %stackaddr$prim54998, align 8
%stackaddr$prim54999 = alloca %struct.ScmObj*, align 8
%current_45args53534 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53533)
store volatile %struct.ScmObj* %current_45args53534, %struct.ScmObj** %stackaddr$prim54999, align 8
%stackaddr$prim55000 = alloca %struct.ScmObj*, align 8
%anf_45bind47229 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53534)
store volatile %struct.ScmObj* %anf_45bind47229, %struct.ScmObj** %stackaddr$prim55000, align 8
%truthy$cmp55001 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47229)
%cmp$cmp55001 = icmp eq i64 %truthy$cmp55001, 1
br i1 %cmp$cmp55001, label %truebranch$cmp55001, label %falsebranch$cmp55001
truebranch$cmp55001:
%ae48247 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53536$k474330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55002 = alloca %struct.ScmObj*, align 8
%argslist53536$k474331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47098, %struct.ScmObj* %argslist53536$k474330)
store volatile %struct.ScmObj* %argslist53536$k474331, %struct.ScmObj** %stackaddr$prim55002, align 8
%stackaddr$prim55003 = alloca %struct.ScmObj*, align 8
%argslist53536$k474332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48247, %struct.ScmObj* %argslist53536$k474331)
store volatile %struct.ScmObj* %argslist53536$k474332, %struct.ScmObj** %stackaddr$prim55003, align 8
%clofunc55004 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47433)
musttail call tailcc void %clofunc55004(%struct.ScmObj* %k47433, %struct.ScmObj* %argslist53536$k474332)
ret void
falsebranch$cmp55001:
%stackaddr$makeclosure55005 = alloca %struct.ScmObj*, align 8
%fptrToInt55006 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48252 to i64
%ae48252 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55006)
store volatile %struct.ScmObj* %ae48252, %struct.ScmObj** %stackaddr$makeclosure55005, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48252, %struct.ScmObj* %k47433, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48252, %struct.ScmObj* %lsts47097, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48252, %struct.ScmObj* %_37foldr147089, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48252, %struct.ScmObj* %_37map147085, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48252, %struct.ScmObj* %f47099, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48252, %struct.ScmObj* %acc47098, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48252, %struct.ScmObj* %_37foldr47095, i64 6)
%ae48253 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55007 = alloca %struct.ScmObj*, align 8
%fptrToInt55008 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48254 to i64
%ae48254 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55008)
store volatile %struct.ScmObj* %ae48254, %struct.ScmObj** %stackaddr$makeclosure55007, align 8
%argslist53576$ae482520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55009 = alloca %struct.ScmObj*, align 8
%argslist53576$ae482521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48254, %struct.ScmObj* %argslist53576$ae482520)
store volatile %struct.ScmObj* %argslist53576$ae482521, %struct.ScmObj** %stackaddr$prim55009, align 8
%stackaddr$prim55010 = alloca %struct.ScmObj*, align 8
%argslist53576$ae482522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48253, %struct.ScmObj* %argslist53576$ae482521)
store volatile %struct.ScmObj* %argslist53576$ae482522, %struct.ScmObj** %stackaddr$prim55010, align 8
%clofunc55011 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48252)
musttail call tailcc void %clofunc55011(%struct.ScmObj* %ae48252, %struct.ScmObj* %argslist53576$ae482522)
ret void
}

define tailcc void @proc_clo$ae48252(%struct.ScmObj* %env$ae48252,%struct.ScmObj* %current_45args53537) {
%stackaddr$env-ref55012 = alloca %struct.ScmObj*, align 8
%k47433 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48252, i64 0)
store %struct.ScmObj* %k47433, %struct.ScmObj** %stackaddr$env-ref55012
%stackaddr$env-ref55013 = alloca %struct.ScmObj*, align 8
%lsts47097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48252, i64 1)
store %struct.ScmObj* %lsts47097, %struct.ScmObj** %stackaddr$env-ref55013
%stackaddr$env-ref55014 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48252, i64 2)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55014
%stackaddr$env-ref55015 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48252, i64 3)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref55015
%stackaddr$env-ref55016 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48252, i64 4)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref55016
%stackaddr$env-ref55017 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48252, i64 5)
store %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$env-ref55017
%stackaddr$env-ref55018 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48252, i64 6)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55018
%stackaddr$prim55019 = alloca %struct.ScmObj*, align 8
%_95k47436 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53537)
store volatile %struct.ScmObj* %_95k47436, %struct.ScmObj** %stackaddr$prim55019, align 8
%stackaddr$prim55020 = alloca %struct.ScmObj*, align 8
%current_45args53538 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53537)
store volatile %struct.ScmObj* %current_45args53538, %struct.ScmObj** %stackaddr$prim55020, align 8
%stackaddr$prim55021 = alloca %struct.ScmObj*, align 8
%anf_45bind47230 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53538)
store volatile %struct.ScmObj* %anf_45bind47230, %struct.ScmObj** %stackaddr$prim55021, align 8
%stackaddr$makeclosure55022 = alloca %struct.ScmObj*, align 8
%fptrToInt55023 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48273 to i64
%ae48273 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55023)
store volatile %struct.ScmObj* %ae48273, %struct.ScmObj** %stackaddr$makeclosure55022, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48273, %struct.ScmObj* %k47433, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48273, %struct.ScmObj* %lsts47097, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48273, %struct.ScmObj* %_37foldr147089, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48273, %struct.ScmObj* %_37map147085, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48273, %struct.ScmObj* %f47099, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48273, %struct.ScmObj* %acc47098, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48273, %struct.ScmObj* %_37foldr47095, i64 6)
%argslist53571$_37map1470850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55024 = alloca %struct.ScmObj*, align 8
%argslist53571$_37map1470851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47097, %struct.ScmObj* %argslist53571$_37map1470850)
store volatile %struct.ScmObj* %argslist53571$_37map1470851, %struct.ScmObj** %stackaddr$prim55024, align 8
%stackaddr$prim55025 = alloca %struct.ScmObj*, align 8
%argslist53571$_37map1470852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47230, %struct.ScmObj* %argslist53571$_37map1470851)
store volatile %struct.ScmObj* %argslist53571$_37map1470852, %struct.ScmObj** %stackaddr$prim55025, align 8
%stackaddr$prim55026 = alloca %struct.ScmObj*, align 8
%argslist53571$_37map1470853 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48273, %struct.ScmObj* %argslist53571$_37map1470852)
store volatile %struct.ScmObj* %argslist53571$_37map1470853, %struct.ScmObj** %stackaddr$prim55026, align 8
%clofunc55027 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147085)
musttail call tailcc void %clofunc55027(%struct.ScmObj* %_37map147085, %struct.ScmObj* %argslist53571$_37map1470853)
ret void
}

define tailcc void @proc_clo$ae48273(%struct.ScmObj* %env$ae48273,%struct.ScmObj* %current_45args53540) {
%stackaddr$env-ref55028 = alloca %struct.ScmObj*, align 8
%k47433 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48273, i64 0)
store %struct.ScmObj* %k47433, %struct.ScmObj** %stackaddr$env-ref55028
%stackaddr$env-ref55029 = alloca %struct.ScmObj*, align 8
%lsts47097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48273, i64 1)
store %struct.ScmObj* %lsts47097, %struct.ScmObj** %stackaddr$env-ref55029
%stackaddr$env-ref55030 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48273, i64 2)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55030
%stackaddr$env-ref55031 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48273, i64 3)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref55031
%stackaddr$env-ref55032 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48273, i64 4)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref55032
%stackaddr$env-ref55033 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48273, i64 5)
store %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$env-ref55033
%stackaddr$env-ref55034 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48273, i64 6)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55034
%stackaddr$prim55035 = alloca %struct.ScmObj*, align 8
%_95k47437 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53540)
store volatile %struct.ScmObj* %_95k47437, %struct.ScmObj** %stackaddr$prim55035, align 8
%stackaddr$prim55036 = alloca %struct.ScmObj*, align 8
%current_45args53541 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53540)
store volatile %struct.ScmObj* %current_45args53541, %struct.ScmObj** %stackaddr$prim55036, align 8
%stackaddr$prim55037 = alloca %struct.ScmObj*, align 8
%lsts_4347104 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53541)
store volatile %struct.ScmObj* %lsts_4347104, %struct.ScmObj** %stackaddr$prim55037, align 8
%stackaddr$makeclosure55038 = alloca %struct.ScmObj*, align 8
%fptrToInt55039 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48276 to i64
%ae48276 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt55039)
store volatile %struct.ScmObj* %ae48276, %struct.ScmObj** %stackaddr$makeclosure55038, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48276, %struct.ScmObj* %k47433, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48276, %struct.ScmObj* %lsts47097, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48276, %struct.ScmObj* %_37foldr147089, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48276, %struct.ScmObj* %lsts_4347104, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48276, %struct.ScmObj* %_37map147085, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48276, %struct.ScmObj* %f47099, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48276, %struct.ScmObj* %acc47098, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae48276, %struct.ScmObj* %_37foldr47095, i64 7)
%ae48277 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55040 = alloca %struct.ScmObj*, align 8
%fptrToInt55041 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48278 to i64
%ae48278 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55041)
store volatile %struct.ScmObj* %ae48278, %struct.ScmObj** %stackaddr$makeclosure55040, align 8
%argslist53570$ae482760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55042 = alloca %struct.ScmObj*, align 8
%argslist53570$ae482761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48278, %struct.ScmObj* %argslist53570$ae482760)
store volatile %struct.ScmObj* %argslist53570$ae482761, %struct.ScmObj** %stackaddr$prim55042, align 8
%stackaddr$prim55043 = alloca %struct.ScmObj*, align 8
%argslist53570$ae482762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48277, %struct.ScmObj* %argslist53570$ae482761)
store volatile %struct.ScmObj* %argslist53570$ae482762, %struct.ScmObj** %stackaddr$prim55043, align 8
%clofunc55044 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48276)
musttail call tailcc void %clofunc55044(%struct.ScmObj* %ae48276, %struct.ScmObj* %argslist53570$ae482762)
ret void
}

define tailcc void @proc_clo$ae48276(%struct.ScmObj* %env$ae48276,%struct.ScmObj* %current_45args53543) {
%stackaddr$env-ref55045 = alloca %struct.ScmObj*, align 8
%k47433 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48276, i64 0)
store %struct.ScmObj* %k47433, %struct.ScmObj** %stackaddr$env-ref55045
%stackaddr$env-ref55046 = alloca %struct.ScmObj*, align 8
%lsts47097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48276, i64 1)
store %struct.ScmObj* %lsts47097, %struct.ScmObj** %stackaddr$env-ref55046
%stackaddr$env-ref55047 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48276, i64 2)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55047
%stackaddr$env-ref55048 = alloca %struct.ScmObj*, align 8
%lsts_4347104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48276, i64 3)
store %struct.ScmObj* %lsts_4347104, %struct.ScmObj** %stackaddr$env-ref55048
%stackaddr$env-ref55049 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48276, i64 4)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref55049
%stackaddr$env-ref55050 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48276, i64 5)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref55050
%stackaddr$env-ref55051 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48276, i64 6)
store %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$env-ref55051
%stackaddr$env-ref55052 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48276, i64 7)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55052
%stackaddr$prim55053 = alloca %struct.ScmObj*, align 8
%_95k47438 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53543)
store volatile %struct.ScmObj* %_95k47438, %struct.ScmObj** %stackaddr$prim55053, align 8
%stackaddr$prim55054 = alloca %struct.ScmObj*, align 8
%current_45args53544 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53543)
store volatile %struct.ScmObj* %current_45args53544, %struct.ScmObj** %stackaddr$prim55054, align 8
%stackaddr$prim55055 = alloca %struct.ScmObj*, align 8
%anf_45bind47231 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53544)
store volatile %struct.ScmObj* %anf_45bind47231, %struct.ScmObj** %stackaddr$prim55055, align 8
%stackaddr$makeclosure55056 = alloca %struct.ScmObj*, align 8
%fptrToInt55057 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48297 to i64
%ae48297 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt55057)
store volatile %struct.ScmObj* %ae48297, %struct.ScmObj** %stackaddr$makeclosure55056, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48297, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48297, %struct.ScmObj* %lsts_4347104, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48297, %struct.ScmObj* %f47099, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48297, %struct.ScmObj* %acc47098, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48297, %struct.ScmObj* %k47433, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48297, %struct.ScmObj* %_37foldr47095, i64 5)
%argslist53565$_37map1470850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55058 = alloca %struct.ScmObj*, align 8
%argslist53565$_37map1470851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47097, %struct.ScmObj* %argslist53565$_37map1470850)
store volatile %struct.ScmObj* %argslist53565$_37map1470851, %struct.ScmObj** %stackaddr$prim55058, align 8
%stackaddr$prim55059 = alloca %struct.ScmObj*, align 8
%argslist53565$_37map1470852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47231, %struct.ScmObj* %argslist53565$_37map1470851)
store volatile %struct.ScmObj* %argslist53565$_37map1470852, %struct.ScmObj** %stackaddr$prim55059, align 8
%stackaddr$prim55060 = alloca %struct.ScmObj*, align 8
%argslist53565$_37map1470853 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48297, %struct.ScmObj* %argslist53565$_37map1470852)
store volatile %struct.ScmObj* %argslist53565$_37map1470853, %struct.ScmObj** %stackaddr$prim55060, align 8
%clofunc55061 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147085)
musttail call tailcc void %clofunc55061(%struct.ScmObj* %_37map147085, %struct.ScmObj* %argslist53565$_37map1470853)
ret void
}

define tailcc void @proc_clo$ae48297(%struct.ScmObj* %env$ae48297,%struct.ScmObj* %current_45args53546) {
%stackaddr$env-ref55062 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48297, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55062
%stackaddr$env-ref55063 = alloca %struct.ScmObj*, align 8
%lsts_4347104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48297, i64 1)
store %struct.ScmObj* %lsts_4347104, %struct.ScmObj** %stackaddr$env-ref55063
%stackaddr$env-ref55064 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48297, i64 2)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref55064
%stackaddr$env-ref55065 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48297, i64 3)
store %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$env-ref55065
%stackaddr$env-ref55066 = alloca %struct.ScmObj*, align 8
%k47433 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48297, i64 4)
store %struct.ScmObj* %k47433, %struct.ScmObj** %stackaddr$env-ref55066
%stackaddr$env-ref55067 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48297, i64 5)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55067
%stackaddr$prim55068 = alloca %struct.ScmObj*, align 8
%_95k47439 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53546)
store volatile %struct.ScmObj* %_95k47439, %struct.ScmObj** %stackaddr$prim55068, align 8
%stackaddr$prim55069 = alloca %struct.ScmObj*, align 8
%current_45args53547 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53546)
store volatile %struct.ScmObj* %current_45args53547, %struct.ScmObj** %stackaddr$prim55069, align 8
%stackaddr$prim55070 = alloca %struct.ScmObj*, align 8
%vs47102 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53547)
store volatile %struct.ScmObj* %vs47102, %struct.ScmObj** %stackaddr$prim55070, align 8
%stackaddr$makeclosure55071 = alloca %struct.ScmObj*, align 8
%fptrToInt55072 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48300 to i64
%ae48300 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55072)
store volatile %struct.ScmObj* %ae48300, %struct.ScmObj** %stackaddr$makeclosure55071, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48300, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48300, %struct.ScmObj* %lsts_4347104, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48300, %struct.ScmObj* %vs47102, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48300, %struct.ScmObj* %f47099, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48300, %struct.ScmObj* %acc47098, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48300, %struct.ScmObj* %k47433, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48300, %struct.ScmObj* %_37foldr47095, i64 6)
%ae48301 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55073 = alloca %struct.ScmObj*, align 8
%fptrToInt55074 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48302 to i64
%ae48302 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55074)
store volatile %struct.ScmObj* %ae48302, %struct.ScmObj** %stackaddr$makeclosure55073, align 8
%argslist53564$ae483000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55075 = alloca %struct.ScmObj*, align 8
%argslist53564$ae483001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48302, %struct.ScmObj* %argslist53564$ae483000)
store volatile %struct.ScmObj* %argslist53564$ae483001, %struct.ScmObj** %stackaddr$prim55075, align 8
%stackaddr$prim55076 = alloca %struct.ScmObj*, align 8
%argslist53564$ae483002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48301, %struct.ScmObj* %argslist53564$ae483001)
store volatile %struct.ScmObj* %argslist53564$ae483002, %struct.ScmObj** %stackaddr$prim55076, align 8
%clofunc55077 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48300)
musttail call tailcc void %clofunc55077(%struct.ScmObj* %ae48300, %struct.ScmObj* %argslist53564$ae483002)
ret void
}

define tailcc void @proc_clo$ae48300(%struct.ScmObj* %env$ae48300,%struct.ScmObj* %current_45args53549) {
%stackaddr$env-ref55078 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48300, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55078
%stackaddr$env-ref55079 = alloca %struct.ScmObj*, align 8
%lsts_4347104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48300, i64 1)
store %struct.ScmObj* %lsts_4347104, %struct.ScmObj** %stackaddr$env-ref55079
%stackaddr$env-ref55080 = alloca %struct.ScmObj*, align 8
%vs47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48300, i64 2)
store %struct.ScmObj* %vs47102, %struct.ScmObj** %stackaddr$env-ref55080
%stackaddr$env-ref55081 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48300, i64 3)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref55081
%stackaddr$env-ref55082 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48300, i64 4)
store %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$env-ref55082
%stackaddr$env-ref55083 = alloca %struct.ScmObj*, align 8
%k47433 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48300, i64 5)
store %struct.ScmObj* %k47433, %struct.ScmObj** %stackaddr$env-ref55083
%stackaddr$env-ref55084 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48300, i64 6)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55084
%stackaddr$prim55085 = alloca %struct.ScmObj*, align 8
%_95k47440 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53549)
store volatile %struct.ScmObj* %_95k47440, %struct.ScmObj** %stackaddr$prim55085, align 8
%stackaddr$prim55086 = alloca %struct.ScmObj*, align 8
%current_45args53550 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53549)
store volatile %struct.ScmObj* %current_45args53550, %struct.ScmObj** %stackaddr$prim55086, align 8
%stackaddr$prim55087 = alloca %struct.ScmObj*, align 8
%anf_45bind47232 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53550)
store volatile %struct.ScmObj* %anf_45bind47232, %struct.ScmObj** %stackaddr$prim55087, align 8
%stackaddr$prim55088 = alloca %struct.ScmObj*, align 8
%anf_45bind47233 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47098, %struct.ScmObj* %lsts_4347104)
store volatile %struct.ScmObj* %anf_45bind47233, %struct.ScmObj** %stackaddr$prim55088, align 8
%stackaddr$prim55089 = alloca %struct.ScmObj*, align 8
%anf_45bind47234 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47099, %struct.ScmObj* %anf_45bind47233)
store volatile %struct.ScmObj* %anf_45bind47234, %struct.ScmObj** %stackaddr$prim55089, align 8
%stackaddr$makeclosure55090 = alloca %struct.ScmObj*, align 8
%fptrToInt55091 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48326 to i64
%ae48326 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55091)
store volatile %struct.ScmObj* %ae48326, %struct.ScmObj** %stackaddr$makeclosure55090, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48326, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48326, %struct.ScmObj* %anf_45bind47232, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48326, %struct.ScmObj* %vs47102, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48326, %struct.ScmObj* %f47099, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48326, %struct.ScmObj* %k47433, i64 4)
%stackaddr$prim55092 = alloca %struct.ScmObj*, align 8
%cpsargs47444 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48326, %struct.ScmObj* %anf_45bind47234)
store volatile %struct.ScmObj* %cpsargs47444, %struct.ScmObj** %stackaddr$prim55092, align 8
%clofunc55093 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr47095)
musttail call tailcc void %clofunc55093(%struct.ScmObj* %_37foldr47095, %struct.ScmObj* %cpsargs47444)
ret void
}

define tailcc void @proc_clo$ae48326(%struct.ScmObj* %env$ae48326,%struct.ScmObj* %current_45args53552) {
%stackaddr$env-ref55094 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48326, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55094
%stackaddr$env-ref55095 = alloca %struct.ScmObj*, align 8
%anf_45bind47232 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48326, i64 1)
store %struct.ScmObj* %anf_45bind47232, %struct.ScmObj** %stackaddr$env-ref55095
%stackaddr$env-ref55096 = alloca %struct.ScmObj*, align 8
%vs47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48326, i64 2)
store %struct.ScmObj* %vs47102, %struct.ScmObj** %stackaddr$env-ref55096
%stackaddr$env-ref55097 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48326, i64 3)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref55097
%stackaddr$env-ref55098 = alloca %struct.ScmObj*, align 8
%k47433 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48326, i64 4)
store %struct.ScmObj* %k47433, %struct.ScmObj** %stackaddr$env-ref55098
%stackaddr$prim55099 = alloca %struct.ScmObj*, align 8
%_95k47441 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53552)
store volatile %struct.ScmObj* %_95k47441, %struct.ScmObj** %stackaddr$prim55099, align 8
%stackaddr$prim55100 = alloca %struct.ScmObj*, align 8
%current_45args53553 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53552)
store volatile %struct.ScmObj* %current_45args53553, %struct.ScmObj** %stackaddr$prim55100, align 8
%stackaddr$prim55101 = alloca %struct.ScmObj*, align 8
%anf_45bind47235 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53553)
store volatile %struct.ScmObj* %anf_45bind47235, %struct.ScmObj** %stackaddr$prim55101, align 8
%ae48331 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55102 = alloca %struct.ScmObj*, align 8
%anf_45bind47236 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47235, %struct.ScmObj* %ae48331)
store volatile %struct.ScmObj* %anf_45bind47236, %struct.ScmObj** %stackaddr$prim55102, align 8
%stackaddr$makeclosure55103 = alloca %struct.ScmObj*, align 8
%fptrToInt55104 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48333 to i64
%ae48333 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55104)
store volatile %struct.ScmObj* %ae48333, %struct.ScmObj** %stackaddr$makeclosure55103, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48333, %struct.ScmObj* %f47099, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48333, %struct.ScmObj* %k47433, i64 1)
%argslist53558$_37foldr1470890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55105 = alloca %struct.ScmObj*, align 8
%argslist53558$_37foldr1470891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs47102, %struct.ScmObj* %argslist53558$_37foldr1470890)
store volatile %struct.ScmObj* %argslist53558$_37foldr1470891, %struct.ScmObj** %stackaddr$prim55105, align 8
%stackaddr$prim55106 = alloca %struct.ScmObj*, align 8
%argslist53558$_37foldr1470892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47236, %struct.ScmObj* %argslist53558$_37foldr1470891)
store volatile %struct.ScmObj* %argslist53558$_37foldr1470892, %struct.ScmObj** %stackaddr$prim55106, align 8
%stackaddr$prim55107 = alloca %struct.ScmObj*, align 8
%argslist53558$_37foldr1470893 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47232, %struct.ScmObj* %argslist53558$_37foldr1470892)
store volatile %struct.ScmObj* %argslist53558$_37foldr1470893, %struct.ScmObj** %stackaddr$prim55107, align 8
%stackaddr$prim55108 = alloca %struct.ScmObj*, align 8
%argslist53558$_37foldr1470894 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48333, %struct.ScmObj* %argslist53558$_37foldr1470893)
store volatile %struct.ScmObj* %argslist53558$_37foldr1470894, %struct.ScmObj** %stackaddr$prim55108, align 8
%clofunc55109 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147089)
musttail call tailcc void %clofunc55109(%struct.ScmObj* %_37foldr147089, %struct.ScmObj* %argslist53558$_37foldr1470894)
ret void
}

define tailcc void @proc_clo$ae48333(%struct.ScmObj* %env$ae48333,%struct.ScmObj* %current_45args53555) {
%stackaddr$env-ref55110 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48333, i64 0)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref55110
%stackaddr$env-ref55111 = alloca %struct.ScmObj*, align 8
%k47433 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48333, i64 1)
store %struct.ScmObj* %k47433, %struct.ScmObj** %stackaddr$env-ref55111
%stackaddr$prim55112 = alloca %struct.ScmObj*, align 8
%_95k47442 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53555)
store volatile %struct.ScmObj* %_95k47442, %struct.ScmObj** %stackaddr$prim55112, align 8
%stackaddr$prim55113 = alloca %struct.ScmObj*, align 8
%current_45args53556 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53555)
store volatile %struct.ScmObj* %current_45args53556, %struct.ScmObj** %stackaddr$prim55113, align 8
%stackaddr$prim55114 = alloca %struct.ScmObj*, align 8
%anf_45bind47237 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53556)
store volatile %struct.ScmObj* %anf_45bind47237, %struct.ScmObj** %stackaddr$prim55114, align 8
%stackaddr$prim55115 = alloca %struct.ScmObj*, align 8
%cpsargs47443 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47433, %struct.ScmObj* %anf_45bind47237)
store volatile %struct.ScmObj* %cpsargs47443, %struct.ScmObj** %stackaddr$prim55115, align 8
%clofunc55116 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47099)
musttail call tailcc void %clofunc55116(%struct.ScmObj* %f47099, %struct.ScmObj* %cpsargs47443)
ret void
}

define tailcc void @proc_clo$ae48302(%struct.ScmObj* %env$ae48302,%struct.ScmObj* %current_45args53559) {
%stackaddr$prim55117 = alloca %struct.ScmObj*, align 8
%k47445 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53559)
store volatile %struct.ScmObj* %k47445, %struct.ScmObj** %stackaddr$prim55117, align 8
%stackaddr$prim55118 = alloca %struct.ScmObj*, align 8
%current_45args53560 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53559)
store volatile %struct.ScmObj* %current_45args53560, %struct.ScmObj** %stackaddr$prim55118, align 8
%stackaddr$prim55119 = alloca %struct.ScmObj*, align 8
%a47107 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53560)
store volatile %struct.ScmObj* %a47107, %struct.ScmObj** %stackaddr$prim55119, align 8
%stackaddr$prim55120 = alloca %struct.ScmObj*, align 8
%current_45args53561 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53560)
store volatile %struct.ScmObj* %current_45args53561, %struct.ScmObj** %stackaddr$prim55120, align 8
%stackaddr$prim55121 = alloca %struct.ScmObj*, align 8
%b47106 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53561)
store volatile %struct.ScmObj* %b47106, %struct.ScmObj** %stackaddr$prim55121, align 8
%stackaddr$prim55122 = alloca %struct.ScmObj*, align 8
%cpsprim47446 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a47107, %struct.ScmObj* %b47106)
store volatile %struct.ScmObj* %cpsprim47446, %struct.ScmObj** %stackaddr$prim55122, align 8
%ae48306 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53563$k474450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55123 = alloca %struct.ScmObj*, align 8
%argslist53563$k474451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47446, %struct.ScmObj* %argslist53563$k474450)
store volatile %struct.ScmObj* %argslist53563$k474451, %struct.ScmObj** %stackaddr$prim55123, align 8
%stackaddr$prim55124 = alloca %struct.ScmObj*, align 8
%argslist53563$k474452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48306, %struct.ScmObj* %argslist53563$k474451)
store volatile %struct.ScmObj* %argslist53563$k474452, %struct.ScmObj** %stackaddr$prim55124, align 8
%clofunc55125 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47445)
musttail call tailcc void %clofunc55125(%struct.ScmObj* %k47445, %struct.ScmObj* %argslist53563$k474452)
ret void
}

define tailcc void @proc_clo$ae48278(%struct.ScmObj* %env$ae48278,%struct.ScmObj* %current_45args53566) {
%stackaddr$prim55126 = alloca %struct.ScmObj*, align 8
%k47447 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53566)
store volatile %struct.ScmObj* %k47447, %struct.ScmObj** %stackaddr$prim55126, align 8
%stackaddr$prim55127 = alloca %struct.ScmObj*, align 8
%current_45args53567 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53566)
store volatile %struct.ScmObj* %current_45args53567, %struct.ScmObj** %stackaddr$prim55127, align 8
%stackaddr$prim55128 = alloca %struct.ScmObj*, align 8
%x47103 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53567)
store volatile %struct.ScmObj* %x47103, %struct.ScmObj** %stackaddr$prim55128, align 8
%stackaddr$prim55129 = alloca %struct.ScmObj*, align 8
%cpsprim47448 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47103)
store volatile %struct.ScmObj* %cpsprim47448, %struct.ScmObj** %stackaddr$prim55129, align 8
%ae48281 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53569$k474470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55130 = alloca %struct.ScmObj*, align 8
%argslist53569$k474471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47448, %struct.ScmObj* %argslist53569$k474470)
store volatile %struct.ScmObj* %argslist53569$k474471, %struct.ScmObj** %stackaddr$prim55130, align 8
%stackaddr$prim55131 = alloca %struct.ScmObj*, align 8
%argslist53569$k474472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48281, %struct.ScmObj* %argslist53569$k474471)
store volatile %struct.ScmObj* %argslist53569$k474472, %struct.ScmObj** %stackaddr$prim55131, align 8
%clofunc55132 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47447)
musttail call tailcc void %clofunc55132(%struct.ScmObj* %k47447, %struct.ScmObj* %argslist53569$k474472)
ret void
}

define tailcc void @proc_clo$ae48254(%struct.ScmObj* %env$ae48254,%struct.ScmObj* %current_45args53572) {
%stackaddr$prim55133 = alloca %struct.ScmObj*, align 8
%k47449 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53572)
store volatile %struct.ScmObj* %k47449, %struct.ScmObj** %stackaddr$prim55133, align 8
%stackaddr$prim55134 = alloca %struct.ScmObj*, align 8
%current_45args53573 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53572)
store volatile %struct.ScmObj* %current_45args53573, %struct.ScmObj** %stackaddr$prim55134, align 8
%stackaddr$prim55135 = alloca %struct.ScmObj*, align 8
%x47105 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53573)
store volatile %struct.ScmObj* %x47105, %struct.ScmObj** %stackaddr$prim55135, align 8
%stackaddr$prim55136 = alloca %struct.ScmObj*, align 8
%cpsprim47450 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47105)
store volatile %struct.ScmObj* %cpsprim47450, %struct.ScmObj** %stackaddr$prim55136, align 8
%ae48257 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53575$k474490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55137 = alloca %struct.ScmObj*, align 8
%argslist53575$k474491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47450, %struct.ScmObj* %argslist53575$k474490)
store volatile %struct.ScmObj* %argslist53575$k474491, %struct.ScmObj** %stackaddr$prim55137, align 8
%stackaddr$prim55138 = alloca %struct.ScmObj*, align 8
%argslist53575$k474492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48257, %struct.ScmObj* %argslist53575$k474491)
store volatile %struct.ScmObj* %argslist53575$k474492, %struct.ScmObj** %stackaddr$prim55138, align 8
%clofunc55139 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47449)
musttail call tailcc void %clofunc55139(%struct.ScmObj* %k47449, %struct.ScmObj* %argslist53575$k474492)
ret void
}

define tailcc void @proc_clo$ae48206(%struct.ScmObj* %env$ae48206,%struct.ScmObj* %current_45args53578) {
%stackaddr$prim55140 = alloca %struct.ScmObj*, align 8
%k47451 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53578)
store volatile %struct.ScmObj* %k47451, %struct.ScmObj** %stackaddr$prim55140, align 8
%stackaddr$prim55141 = alloca %struct.ScmObj*, align 8
%current_45args53579 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53578)
store volatile %struct.ScmObj* %current_45args53579, %struct.ScmObj** %stackaddr$prim55141, align 8
%stackaddr$prim55142 = alloca %struct.ScmObj*, align 8
%lst47101 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53579)
store volatile %struct.ScmObj* %lst47101, %struct.ScmObj** %stackaddr$prim55142, align 8
%stackaddr$prim55143 = alloca %struct.ScmObj*, align 8
%current_45args53580 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53579)
store volatile %struct.ScmObj* %current_45args53580, %struct.ScmObj** %stackaddr$prim55143, align 8
%stackaddr$prim55144 = alloca %struct.ScmObj*, align 8
%b47100 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53580)
store volatile %struct.ScmObj* %b47100, %struct.ScmObj** %stackaddr$prim55144, align 8
%truthy$cmp55145 = call i64 @is_truthy_value(%struct.ScmObj* %b47100)
%cmp$cmp55145 = icmp eq i64 %truthy$cmp55145, 1
br i1 %cmp$cmp55145, label %truebranch$cmp55145, label %falsebranch$cmp55145
truebranch$cmp55145:
%ae48209 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53582$k474510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55146 = alloca %struct.ScmObj*, align 8
%argslist53582$k474511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b47100, %struct.ScmObj* %argslist53582$k474510)
store volatile %struct.ScmObj* %argslist53582$k474511, %struct.ScmObj** %stackaddr$prim55146, align 8
%stackaddr$prim55147 = alloca %struct.ScmObj*, align 8
%argslist53582$k474512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48209, %struct.ScmObj* %argslist53582$k474511)
store volatile %struct.ScmObj* %argslist53582$k474512, %struct.ScmObj** %stackaddr$prim55147, align 8
%clofunc55148 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47451)
musttail call tailcc void %clofunc55148(%struct.ScmObj* %k47451, %struct.ScmObj* %argslist53582$k474512)
ret void
falsebranch$cmp55145:
%stackaddr$prim55149 = alloca %struct.ScmObj*, align 8
%cpsprim47452 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47101)
store volatile %struct.ScmObj* %cpsprim47452, %struct.ScmObj** %stackaddr$prim55149, align 8
%ae48216 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53583$k474510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55150 = alloca %struct.ScmObj*, align 8
%argslist53583$k474511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47452, %struct.ScmObj* %argslist53583$k474510)
store volatile %struct.ScmObj* %argslist53583$k474511, %struct.ScmObj** %stackaddr$prim55150, align 8
%stackaddr$prim55151 = alloca %struct.ScmObj*, align 8
%argslist53583$k474512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48216, %struct.ScmObj* %argslist53583$k474511)
store volatile %struct.ScmObj* %argslist53583$k474512, %struct.ScmObj** %stackaddr$prim55151, align 8
%clofunc55152 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47451)
musttail call tailcc void %clofunc55152(%struct.ScmObj* %k47451, %struct.ScmObj* %argslist53583$k474512)
ret void
}

define tailcc void @proc_clo$ae48163(%struct.ScmObj* %env$ae48163,%struct.ScmObj* %current_45args53587) {
%stackaddr$env-ref55153 = alloca %struct.ScmObj*, align 8
%_37length47078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48163, i64 0)
store %struct.ScmObj* %_37length47078, %struct.ScmObj** %stackaddr$env-ref55153
%stackaddr$env-ref55154 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48163, i64 1)
store %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$env-ref55154
%stackaddr$prim55155 = alloca %struct.ScmObj*, align 8
%k47453 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53587)
store volatile %struct.ScmObj* %k47453, %struct.ScmObj** %stackaddr$prim55155, align 8
%stackaddr$prim55156 = alloca %struct.ScmObj*, align 8
%current_45args53588 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53587)
store volatile %struct.ScmObj* %current_45args53588, %struct.ScmObj** %stackaddr$prim55156, align 8
%stackaddr$prim55157 = alloca %struct.ScmObj*, align 8
%lst47110 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53588)
store volatile %struct.ScmObj* %lst47110, %struct.ScmObj** %stackaddr$prim55157, align 8
%stackaddr$prim55158 = alloca %struct.ScmObj*, align 8
%current_45args53589 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53588)
store volatile %struct.ScmObj* %current_45args53589, %struct.ScmObj** %stackaddr$prim55158, align 8
%stackaddr$prim55159 = alloca %struct.ScmObj*, align 8
%n47109 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53589)
store volatile %struct.ScmObj* %n47109, %struct.ScmObj** %stackaddr$prim55159, align 8
%stackaddr$makeclosure55160 = alloca %struct.ScmObj*, align 8
%fptrToInt55161 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48165 to i64
%ae48165 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55161)
store volatile %struct.ScmObj* %ae48165, %struct.ScmObj** %stackaddr$makeclosure55160, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48165, %struct.ScmObj* %lst47110, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48165, %struct.ScmObj* %n47109, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48165, %struct.ScmObj* %k47453, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48165, %struct.ScmObj* %_37take47081, i64 3)
%argslist53595$_37length470780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55162 = alloca %struct.ScmObj*, align 8
%argslist53595$_37length470781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47110, %struct.ScmObj* %argslist53595$_37length470780)
store volatile %struct.ScmObj* %argslist53595$_37length470781, %struct.ScmObj** %stackaddr$prim55162, align 8
%stackaddr$prim55163 = alloca %struct.ScmObj*, align 8
%argslist53595$_37length470782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48165, %struct.ScmObj* %argslist53595$_37length470781)
store volatile %struct.ScmObj* %argslist53595$_37length470782, %struct.ScmObj** %stackaddr$prim55163, align 8
%clofunc55164 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length47078)
musttail call tailcc void %clofunc55164(%struct.ScmObj* %_37length47078, %struct.ScmObj* %argslist53595$_37length470782)
ret void
}

define tailcc void @proc_clo$ae48165(%struct.ScmObj* %env$ae48165,%struct.ScmObj* %current_45args53591) {
%stackaddr$env-ref55165 = alloca %struct.ScmObj*, align 8
%lst47110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48165, i64 0)
store %struct.ScmObj* %lst47110, %struct.ScmObj** %stackaddr$env-ref55165
%stackaddr$env-ref55166 = alloca %struct.ScmObj*, align 8
%n47109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48165, i64 1)
store %struct.ScmObj* %n47109, %struct.ScmObj** %stackaddr$env-ref55166
%stackaddr$env-ref55167 = alloca %struct.ScmObj*, align 8
%k47453 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48165, i64 2)
store %struct.ScmObj* %k47453, %struct.ScmObj** %stackaddr$env-ref55167
%stackaddr$env-ref55168 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48165, i64 3)
store %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$env-ref55168
%stackaddr$prim55169 = alloca %struct.ScmObj*, align 8
%_95k47454 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53591)
store volatile %struct.ScmObj* %_95k47454, %struct.ScmObj** %stackaddr$prim55169, align 8
%stackaddr$prim55170 = alloca %struct.ScmObj*, align 8
%current_45args53592 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53591)
store volatile %struct.ScmObj* %current_45args53592, %struct.ScmObj** %stackaddr$prim55170, align 8
%stackaddr$prim55171 = alloca %struct.ScmObj*, align 8
%anf_45bind47224 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53592)
store volatile %struct.ScmObj* %anf_45bind47224, %struct.ScmObj** %stackaddr$prim55171, align 8
%stackaddr$prim55172 = alloca %struct.ScmObj*, align 8
%anf_45bind47225 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind47224, %struct.ScmObj* %n47109)
store volatile %struct.ScmObj* %anf_45bind47225, %struct.ScmObj** %stackaddr$prim55172, align 8
%argslist53594$_37take470810 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55173 = alloca %struct.ScmObj*, align 8
%argslist53594$_37take470811 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47225, %struct.ScmObj* %argslist53594$_37take470810)
store volatile %struct.ScmObj* %argslist53594$_37take470811, %struct.ScmObj** %stackaddr$prim55173, align 8
%stackaddr$prim55174 = alloca %struct.ScmObj*, align 8
%argslist53594$_37take470812 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47110, %struct.ScmObj* %argslist53594$_37take470811)
store volatile %struct.ScmObj* %argslist53594$_37take470812, %struct.ScmObj** %stackaddr$prim55174, align 8
%stackaddr$prim55175 = alloca %struct.ScmObj*, align 8
%argslist53594$_37take470813 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47453, %struct.ScmObj* %argslist53594$_37take470812)
store volatile %struct.ScmObj* %argslist53594$_37take470813, %struct.ScmObj** %stackaddr$prim55175, align 8
%clofunc55176 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take47081)
musttail call tailcc void %clofunc55176(%struct.ScmObj* %_37take47081, %struct.ScmObj* %argslist53594$_37take470813)
ret void
}

define tailcc void @proc_clo$ae48109(%struct.ScmObj* %env$ae48109,%struct.ScmObj* %current_45args53597) {
%stackaddr$env-ref55177 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48109, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref55177
%stackaddr$prim55178 = alloca %struct.ScmObj*, align 8
%k47455 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53597)
store volatile %struct.ScmObj* %k47455, %struct.ScmObj** %stackaddr$prim55178, align 8
%stackaddr$prim55179 = alloca %struct.ScmObj*, align 8
%current_45args53598 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53597)
store volatile %struct.ScmObj* %current_45args53598, %struct.ScmObj** %stackaddr$prim55179, align 8
%stackaddr$prim55180 = alloca %struct.ScmObj*, align 8
%lst47112 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53598)
store volatile %struct.ScmObj* %lst47112, %struct.ScmObj** %stackaddr$prim55180, align 8
%stackaddr$makeclosure55181 = alloca %struct.ScmObj*, align 8
%fptrToInt55182 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48110 to i64
%ae48110 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55182)
store volatile %struct.ScmObj* %ae48110, %struct.ScmObj** %stackaddr$makeclosure55181, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48110, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48110, %struct.ScmObj* %k47455, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48110, %struct.ScmObj* %lst47112, i64 2)
%ae48111 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55183 = alloca %struct.ScmObj*, align 8
%fptrToInt55184 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48112 to i64
%ae48112 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55184)
store volatile %struct.ScmObj* %ae48112, %struct.ScmObj** %stackaddr$makeclosure55183, align 8
%argslist53609$ae481100 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55185 = alloca %struct.ScmObj*, align 8
%argslist53609$ae481101 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48112, %struct.ScmObj* %argslist53609$ae481100)
store volatile %struct.ScmObj* %argslist53609$ae481101, %struct.ScmObj** %stackaddr$prim55185, align 8
%stackaddr$prim55186 = alloca %struct.ScmObj*, align 8
%argslist53609$ae481102 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48111, %struct.ScmObj* %argslist53609$ae481101)
store volatile %struct.ScmObj* %argslist53609$ae481102, %struct.ScmObj** %stackaddr$prim55186, align 8
%clofunc55187 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48110)
musttail call tailcc void %clofunc55187(%struct.ScmObj* %ae48110, %struct.ScmObj* %argslist53609$ae481102)
ret void
}

define tailcc void @proc_clo$ae48110(%struct.ScmObj* %env$ae48110,%struct.ScmObj* %current_45args53600) {
%stackaddr$env-ref55188 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48110, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref55188
%stackaddr$env-ref55189 = alloca %struct.ScmObj*, align 8
%k47455 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48110, i64 1)
store %struct.ScmObj* %k47455, %struct.ScmObj** %stackaddr$env-ref55189
%stackaddr$env-ref55190 = alloca %struct.ScmObj*, align 8
%lst47112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48110, i64 2)
store %struct.ScmObj* %lst47112, %struct.ScmObj** %stackaddr$env-ref55190
%stackaddr$prim55191 = alloca %struct.ScmObj*, align 8
%_95k47456 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53600)
store volatile %struct.ScmObj* %_95k47456, %struct.ScmObj** %stackaddr$prim55191, align 8
%stackaddr$prim55192 = alloca %struct.ScmObj*, align 8
%current_45args53601 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53600)
store volatile %struct.ScmObj* %current_45args53601, %struct.ScmObj** %stackaddr$prim55192, align 8
%stackaddr$prim55193 = alloca %struct.ScmObj*, align 8
%anf_45bind47223 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53601)
store volatile %struct.ScmObj* %anf_45bind47223, %struct.ScmObj** %stackaddr$prim55193, align 8
%ae48131 = call %struct.ScmObj* @const_init_null()
%argslist53603$_37foldl1470730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55194 = alloca %struct.ScmObj*, align 8
%argslist53603$_37foldl1470731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47112, %struct.ScmObj* %argslist53603$_37foldl1470730)
store volatile %struct.ScmObj* %argslist53603$_37foldl1470731, %struct.ScmObj** %stackaddr$prim55194, align 8
%stackaddr$prim55195 = alloca %struct.ScmObj*, align 8
%argslist53603$_37foldl1470732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48131, %struct.ScmObj* %argslist53603$_37foldl1470731)
store volatile %struct.ScmObj* %argslist53603$_37foldl1470732, %struct.ScmObj** %stackaddr$prim55195, align 8
%stackaddr$prim55196 = alloca %struct.ScmObj*, align 8
%argslist53603$_37foldl1470733 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47223, %struct.ScmObj* %argslist53603$_37foldl1470732)
store volatile %struct.ScmObj* %argslist53603$_37foldl1470733, %struct.ScmObj** %stackaddr$prim55196, align 8
%stackaddr$prim55197 = alloca %struct.ScmObj*, align 8
%argslist53603$_37foldl1470734 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47455, %struct.ScmObj* %argslist53603$_37foldl1470733)
store volatile %struct.ScmObj* %argslist53603$_37foldl1470734, %struct.ScmObj** %stackaddr$prim55197, align 8
%clofunc55198 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147073)
musttail call tailcc void %clofunc55198(%struct.ScmObj* %_37foldl147073, %struct.ScmObj* %argslist53603$_37foldl1470734)
ret void
}

define tailcc void @proc_clo$ae48112(%struct.ScmObj* %env$ae48112,%struct.ScmObj* %current_45args53604) {
%stackaddr$prim55199 = alloca %struct.ScmObj*, align 8
%k47457 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53604)
store volatile %struct.ScmObj* %k47457, %struct.ScmObj** %stackaddr$prim55199, align 8
%stackaddr$prim55200 = alloca %struct.ScmObj*, align 8
%current_45args53605 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53604)
store volatile %struct.ScmObj* %current_45args53605, %struct.ScmObj** %stackaddr$prim55200, align 8
%stackaddr$prim55201 = alloca %struct.ScmObj*, align 8
%x47114 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53605)
store volatile %struct.ScmObj* %x47114, %struct.ScmObj** %stackaddr$prim55201, align 8
%stackaddr$prim55202 = alloca %struct.ScmObj*, align 8
%current_45args53606 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53605)
store volatile %struct.ScmObj* %current_45args53606, %struct.ScmObj** %stackaddr$prim55202, align 8
%stackaddr$prim55203 = alloca %struct.ScmObj*, align 8
%y47113 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53606)
store volatile %struct.ScmObj* %y47113, %struct.ScmObj** %stackaddr$prim55203, align 8
%ae48114 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53608$k474570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55204 = alloca %struct.ScmObj*, align 8
%argslist53608$k474571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x47114, %struct.ScmObj* %argslist53608$k474570)
store volatile %struct.ScmObj* %argslist53608$k474571, %struct.ScmObj** %stackaddr$prim55204, align 8
%stackaddr$prim55205 = alloca %struct.ScmObj*, align 8
%argslist53608$k474572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48114, %struct.ScmObj* %argslist53608$k474571)
store volatile %struct.ScmObj* %argslist53608$k474572, %struct.ScmObj** %stackaddr$prim55205, align 8
%clofunc55206 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47457)
musttail call tailcc void %clofunc55206(%struct.ScmObj* %k47457, %struct.ScmObj* %argslist53608$k474572)
ret void
}

define tailcc void @proc_clo$ae48030(%struct.ScmObj* %env$ae48030,%struct.ScmObj* %current_45args53612) {
%stackaddr$prim55207 = alloca %struct.ScmObj*, align 8
%k47458 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53612)
store volatile %struct.ScmObj* %k47458, %struct.ScmObj** %stackaddr$prim55207, align 8
%stackaddr$prim55208 = alloca %struct.ScmObj*, align 8
%current_45args53613 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53612)
store volatile %struct.ScmObj* %current_45args53613, %struct.ScmObj** %stackaddr$prim55208, align 8
%stackaddr$prim55209 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53613)
store volatile %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$prim55209, align 8
%ae48032 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55210 = alloca %struct.ScmObj*, align 8
%fptrToInt55211 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48033 to i64
%ae48033 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55211)
store volatile %struct.ScmObj* %ae48033, %struct.ScmObj** %stackaddr$makeclosure55210, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48033, %struct.ScmObj* %_37foldl147074, i64 0)
%argslist53626$k474580 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55212 = alloca %struct.ScmObj*, align 8
%argslist53626$k474581 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48033, %struct.ScmObj* %argslist53626$k474580)
store volatile %struct.ScmObj* %argslist53626$k474581, %struct.ScmObj** %stackaddr$prim55212, align 8
%stackaddr$prim55213 = alloca %struct.ScmObj*, align 8
%argslist53626$k474582 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48032, %struct.ScmObj* %argslist53626$k474581)
store volatile %struct.ScmObj* %argslist53626$k474582, %struct.ScmObj** %stackaddr$prim55213, align 8
%clofunc55214 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47458)
musttail call tailcc void %clofunc55214(%struct.ScmObj* %k47458, %struct.ScmObj* %argslist53626$k474582)
ret void
}

define tailcc void @proc_clo$ae48033(%struct.ScmObj* %env$ae48033,%struct.ScmObj* %current_45args53615) {
%stackaddr$env-ref55215 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48033, i64 0)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref55215
%stackaddr$prim55216 = alloca %struct.ScmObj*, align 8
%k47459 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53615)
store volatile %struct.ScmObj* %k47459, %struct.ScmObj** %stackaddr$prim55216, align 8
%stackaddr$prim55217 = alloca %struct.ScmObj*, align 8
%current_45args53616 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53615)
store volatile %struct.ScmObj* %current_45args53616, %struct.ScmObj** %stackaddr$prim55217, align 8
%stackaddr$prim55218 = alloca %struct.ScmObj*, align 8
%f47077 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53616)
store volatile %struct.ScmObj* %f47077, %struct.ScmObj** %stackaddr$prim55218, align 8
%stackaddr$prim55219 = alloca %struct.ScmObj*, align 8
%current_45args53617 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53616)
store volatile %struct.ScmObj* %current_45args53617, %struct.ScmObj** %stackaddr$prim55219, align 8
%stackaddr$prim55220 = alloca %struct.ScmObj*, align 8
%acc47076 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53617)
store volatile %struct.ScmObj* %acc47076, %struct.ScmObj** %stackaddr$prim55220, align 8
%stackaddr$prim55221 = alloca %struct.ScmObj*, align 8
%current_45args53618 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53617)
store volatile %struct.ScmObj* %current_45args53618, %struct.ScmObj** %stackaddr$prim55221, align 8
%stackaddr$prim55222 = alloca %struct.ScmObj*, align 8
%lst47075 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53618)
store volatile %struct.ScmObj* %lst47075, %struct.ScmObj** %stackaddr$prim55222, align 8
%stackaddr$prim55223 = alloca %struct.ScmObj*, align 8
%anf_45bind47218 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47075)
store volatile %struct.ScmObj* %anf_45bind47218, %struct.ScmObj** %stackaddr$prim55223, align 8
%truthy$cmp55224 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47218)
%cmp$cmp55224 = icmp eq i64 %truthy$cmp55224, 1
br i1 %cmp$cmp55224, label %truebranch$cmp55224, label %falsebranch$cmp55224
truebranch$cmp55224:
%ae48037 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53620$k474590 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55225 = alloca %struct.ScmObj*, align 8
%argslist53620$k474591 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47076, %struct.ScmObj* %argslist53620$k474590)
store volatile %struct.ScmObj* %argslist53620$k474591, %struct.ScmObj** %stackaddr$prim55225, align 8
%stackaddr$prim55226 = alloca %struct.ScmObj*, align 8
%argslist53620$k474592 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48037, %struct.ScmObj* %argslist53620$k474591)
store volatile %struct.ScmObj* %argslist53620$k474592, %struct.ScmObj** %stackaddr$prim55226, align 8
%clofunc55227 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47459)
musttail call tailcc void %clofunc55227(%struct.ScmObj* %k47459, %struct.ScmObj* %argslist53620$k474592)
ret void
falsebranch$cmp55224:
%stackaddr$prim55228 = alloca %struct.ScmObj*, align 8
%anf_45bind47219 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47075)
store volatile %struct.ScmObj* %anf_45bind47219, %struct.ScmObj** %stackaddr$prim55228, align 8
%stackaddr$makeclosure55229 = alloca %struct.ScmObj*, align 8
%fptrToInt55230 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48044 to i64
%ae48044 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55230)
store volatile %struct.ScmObj* %ae48044, %struct.ScmObj** %stackaddr$makeclosure55229, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48044, %struct.ScmObj* %k47459, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48044, %struct.ScmObj* %lst47075, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48044, %struct.ScmObj* %f47077, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48044, %struct.ScmObj* %_37foldl147074, i64 3)
%argslist53625$f470770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55231 = alloca %struct.ScmObj*, align 8
%argslist53625$f470771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47076, %struct.ScmObj* %argslist53625$f470770)
store volatile %struct.ScmObj* %argslist53625$f470771, %struct.ScmObj** %stackaddr$prim55231, align 8
%stackaddr$prim55232 = alloca %struct.ScmObj*, align 8
%argslist53625$f470772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47219, %struct.ScmObj* %argslist53625$f470771)
store volatile %struct.ScmObj* %argslist53625$f470772, %struct.ScmObj** %stackaddr$prim55232, align 8
%stackaddr$prim55233 = alloca %struct.ScmObj*, align 8
%argslist53625$f470773 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48044, %struct.ScmObj* %argslist53625$f470772)
store volatile %struct.ScmObj* %argslist53625$f470773, %struct.ScmObj** %stackaddr$prim55233, align 8
%clofunc55234 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47077)
musttail call tailcc void %clofunc55234(%struct.ScmObj* %f47077, %struct.ScmObj* %argslist53625$f470773)
ret void
}

define tailcc void @proc_clo$ae48044(%struct.ScmObj* %env$ae48044,%struct.ScmObj* %current_45args53621) {
%stackaddr$env-ref55235 = alloca %struct.ScmObj*, align 8
%k47459 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48044, i64 0)
store %struct.ScmObj* %k47459, %struct.ScmObj** %stackaddr$env-ref55235
%stackaddr$env-ref55236 = alloca %struct.ScmObj*, align 8
%lst47075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48044, i64 1)
store %struct.ScmObj* %lst47075, %struct.ScmObj** %stackaddr$env-ref55236
%stackaddr$env-ref55237 = alloca %struct.ScmObj*, align 8
%f47077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48044, i64 2)
store %struct.ScmObj* %f47077, %struct.ScmObj** %stackaddr$env-ref55237
%stackaddr$env-ref55238 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48044, i64 3)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref55238
%stackaddr$prim55239 = alloca %struct.ScmObj*, align 8
%_95k47460 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53621)
store volatile %struct.ScmObj* %_95k47460, %struct.ScmObj** %stackaddr$prim55239, align 8
%stackaddr$prim55240 = alloca %struct.ScmObj*, align 8
%current_45args53622 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53621)
store volatile %struct.ScmObj* %current_45args53622, %struct.ScmObj** %stackaddr$prim55240, align 8
%stackaddr$prim55241 = alloca %struct.ScmObj*, align 8
%anf_45bind47220 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53622)
store volatile %struct.ScmObj* %anf_45bind47220, %struct.ScmObj** %stackaddr$prim55241, align 8
%stackaddr$prim55242 = alloca %struct.ScmObj*, align 8
%anf_45bind47221 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47075)
store volatile %struct.ScmObj* %anf_45bind47221, %struct.ScmObj** %stackaddr$prim55242, align 8
%argslist53624$_37foldl1470740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55243 = alloca %struct.ScmObj*, align 8
%argslist53624$_37foldl1470741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47221, %struct.ScmObj* %argslist53624$_37foldl1470740)
store volatile %struct.ScmObj* %argslist53624$_37foldl1470741, %struct.ScmObj** %stackaddr$prim55243, align 8
%stackaddr$prim55244 = alloca %struct.ScmObj*, align 8
%argslist53624$_37foldl1470742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47220, %struct.ScmObj* %argslist53624$_37foldl1470741)
store volatile %struct.ScmObj* %argslist53624$_37foldl1470742, %struct.ScmObj** %stackaddr$prim55244, align 8
%stackaddr$prim55245 = alloca %struct.ScmObj*, align 8
%argslist53624$_37foldl1470743 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47077, %struct.ScmObj* %argslist53624$_37foldl1470742)
store volatile %struct.ScmObj* %argslist53624$_37foldl1470743, %struct.ScmObj** %stackaddr$prim55245, align 8
%stackaddr$prim55246 = alloca %struct.ScmObj*, align 8
%argslist53624$_37foldl1470744 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47459, %struct.ScmObj* %argslist53624$_37foldl1470743)
store volatile %struct.ScmObj* %argslist53624$_37foldl1470744, %struct.ScmObj** %stackaddr$prim55246, align 8
%clofunc55247 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147074)
musttail call tailcc void %clofunc55247(%struct.ScmObj* %_37foldl147074, %struct.ScmObj* %argslist53624$_37foldl1470744)
ret void
}

define tailcc void @proc_clo$ae47947(%struct.ScmObj* %env$ae47947,%struct.ScmObj* %current_45args53629) {
%stackaddr$prim55248 = alloca %struct.ScmObj*, align 8
%k47461 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53629)
store volatile %struct.ScmObj* %k47461, %struct.ScmObj** %stackaddr$prim55248, align 8
%stackaddr$prim55249 = alloca %struct.ScmObj*, align 8
%current_45args53630 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53629)
store volatile %struct.ScmObj* %current_45args53630, %struct.ScmObj** %stackaddr$prim55249, align 8
%stackaddr$prim55250 = alloca %struct.ScmObj*, align 8
%_37length47079 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53630)
store volatile %struct.ScmObj* %_37length47079, %struct.ScmObj** %stackaddr$prim55250, align 8
%ae47949 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55251 = alloca %struct.ScmObj*, align 8
%fptrToInt55252 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47950 to i64
%ae47950 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55252)
store volatile %struct.ScmObj* %ae47950, %struct.ScmObj** %stackaddr$makeclosure55251, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47950, %struct.ScmObj* %_37length47079, i64 0)
%argslist53641$k474610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55253 = alloca %struct.ScmObj*, align 8
%argslist53641$k474611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47950, %struct.ScmObj* %argslist53641$k474610)
store volatile %struct.ScmObj* %argslist53641$k474611, %struct.ScmObj** %stackaddr$prim55253, align 8
%stackaddr$prim55254 = alloca %struct.ScmObj*, align 8
%argslist53641$k474612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47949, %struct.ScmObj* %argslist53641$k474611)
store volatile %struct.ScmObj* %argslist53641$k474612, %struct.ScmObj** %stackaddr$prim55254, align 8
%clofunc55255 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47461)
musttail call tailcc void %clofunc55255(%struct.ScmObj* %k47461, %struct.ScmObj* %argslist53641$k474612)
ret void
}

define tailcc void @proc_clo$ae47950(%struct.ScmObj* %env$ae47950,%struct.ScmObj* %current_45args53632) {
%stackaddr$env-ref55256 = alloca %struct.ScmObj*, align 8
%_37length47079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47950, i64 0)
store %struct.ScmObj* %_37length47079, %struct.ScmObj** %stackaddr$env-ref55256
%stackaddr$prim55257 = alloca %struct.ScmObj*, align 8
%k47462 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53632)
store volatile %struct.ScmObj* %k47462, %struct.ScmObj** %stackaddr$prim55257, align 8
%stackaddr$prim55258 = alloca %struct.ScmObj*, align 8
%current_45args53633 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53632)
store volatile %struct.ScmObj* %current_45args53633, %struct.ScmObj** %stackaddr$prim55258, align 8
%stackaddr$prim55259 = alloca %struct.ScmObj*, align 8
%lst47080 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53633)
store volatile %struct.ScmObj* %lst47080, %struct.ScmObj** %stackaddr$prim55259, align 8
%stackaddr$prim55260 = alloca %struct.ScmObj*, align 8
%anf_45bind47214 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47080)
store volatile %struct.ScmObj* %anf_45bind47214, %struct.ScmObj** %stackaddr$prim55260, align 8
%truthy$cmp55261 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47214)
%cmp$cmp55261 = icmp eq i64 %truthy$cmp55261, 1
br i1 %cmp$cmp55261, label %truebranch$cmp55261, label %falsebranch$cmp55261
truebranch$cmp55261:
%ae47954 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47955 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53635$k474620 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55262 = alloca %struct.ScmObj*, align 8
%argslist53635$k474621 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47955, %struct.ScmObj* %argslist53635$k474620)
store volatile %struct.ScmObj* %argslist53635$k474621, %struct.ScmObj** %stackaddr$prim55262, align 8
%stackaddr$prim55263 = alloca %struct.ScmObj*, align 8
%argslist53635$k474622 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47954, %struct.ScmObj* %argslist53635$k474621)
store volatile %struct.ScmObj* %argslist53635$k474622, %struct.ScmObj** %stackaddr$prim55263, align 8
%clofunc55264 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47462)
musttail call tailcc void %clofunc55264(%struct.ScmObj* %k47462, %struct.ScmObj* %argslist53635$k474622)
ret void
falsebranch$cmp55261:
%stackaddr$prim55265 = alloca %struct.ScmObj*, align 8
%anf_45bind47215 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47080)
store volatile %struct.ScmObj* %anf_45bind47215, %struct.ScmObj** %stackaddr$prim55265, align 8
%stackaddr$makeclosure55266 = alloca %struct.ScmObj*, align 8
%fptrToInt55267 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47964 to i64
%ae47964 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55267)
store volatile %struct.ScmObj* %ae47964, %struct.ScmObj** %stackaddr$makeclosure55266, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47964, %struct.ScmObj* %k47462, i64 0)
%argslist53640$_37length470790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55268 = alloca %struct.ScmObj*, align 8
%argslist53640$_37length470791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47215, %struct.ScmObj* %argslist53640$_37length470790)
store volatile %struct.ScmObj* %argslist53640$_37length470791, %struct.ScmObj** %stackaddr$prim55268, align 8
%stackaddr$prim55269 = alloca %struct.ScmObj*, align 8
%argslist53640$_37length470792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47964, %struct.ScmObj* %argslist53640$_37length470791)
store volatile %struct.ScmObj* %argslist53640$_37length470792, %struct.ScmObj** %stackaddr$prim55269, align 8
%clofunc55270 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length47079)
musttail call tailcc void %clofunc55270(%struct.ScmObj* %_37length47079, %struct.ScmObj* %argslist53640$_37length470792)
ret void
}

define tailcc void @proc_clo$ae47964(%struct.ScmObj* %env$ae47964,%struct.ScmObj* %current_45args53636) {
%stackaddr$env-ref55271 = alloca %struct.ScmObj*, align 8
%k47462 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47964, i64 0)
store %struct.ScmObj* %k47462, %struct.ScmObj** %stackaddr$env-ref55271
%stackaddr$prim55272 = alloca %struct.ScmObj*, align 8
%_95k47463 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53636)
store volatile %struct.ScmObj* %_95k47463, %struct.ScmObj** %stackaddr$prim55272, align 8
%stackaddr$prim55273 = alloca %struct.ScmObj*, align 8
%current_45args53637 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53636)
store volatile %struct.ScmObj* %current_45args53637, %struct.ScmObj** %stackaddr$prim55273, align 8
%stackaddr$prim55274 = alloca %struct.ScmObj*, align 8
%anf_45bind47216 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53637)
store volatile %struct.ScmObj* %anf_45bind47216, %struct.ScmObj** %stackaddr$prim55274, align 8
%ae47966 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55275 = alloca %struct.ScmObj*, align 8
%cpsprim47464 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae47966, %struct.ScmObj* %anf_45bind47216)
store volatile %struct.ScmObj* %cpsprim47464, %struct.ScmObj** %stackaddr$prim55275, align 8
%ae47969 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53639$k474620 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55276 = alloca %struct.ScmObj*, align 8
%argslist53639$k474621 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47464, %struct.ScmObj* %argslist53639$k474620)
store volatile %struct.ScmObj* %argslist53639$k474621, %struct.ScmObj** %stackaddr$prim55276, align 8
%stackaddr$prim55277 = alloca %struct.ScmObj*, align 8
%argslist53639$k474622 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47969, %struct.ScmObj* %argslist53639$k474621)
store volatile %struct.ScmObj* %argslist53639$k474622, %struct.ScmObj** %stackaddr$prim55277, align 8
%clofunc55278 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47462)
musttail call tailcc void %clofunc55278(%struct.ScmObj* %k47462, %struct.ScmObj* %argslist53639$k474622)
ret void
}

define tailcc void @proc_clo$ae47797(%struct.ScmObj* %env$ae47797,%struct.ScmObj* %current_45args53644) {
%stackaddr$prim55279 = alloca %struct.ScmObj*, align 8
%k47465 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53644)
store volatile %struct.ScmObj* %k47465, %struct.ScmObj** %stackaddr$prim55279, align 8
%stackaddr$prim55280 = alloca %struct.ScmObj*, align 8
%current_45args53645 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53644)
store volatile %struct.ScmObj* %current_45args53645, %struct.ScmObj** %stackaddr$prim55280, align 8
%stackaddr$prim55281 = alloca %struct.ScmObj*, align 8
%_37take47082 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53645)
store volatile %struct.ScmObj* %_37take47082, %struct.ScmObj** %stackaddr$prim55281, align 8
%ae47799 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55282 = alloca %struct.ScmObj*, align 8
%fptrToInt55283 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47800 to i64
%ae47800 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55283)
store volatile %struct.ScmObj* %ae47800, %struct.ScmObj** %stackaddr$makeclosure55282, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47800, %struct.ScmObj* %_37take47082, i64 0)
%argslist53658$k474650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55284 = alloca %struct.ScmObj*, align 8
%argslist53658$k474651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47800, %struct.ScmObj* %argslist53658$k474650)
store volatile %struct.ScmObj* %argslist53658$k474651, %struct.ScmObj** %stackaddr$prim55284, align 8
%stackaddr$prim55285 = alloca %struct.ScmObj*, align 8
%argslist53658$k474652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47799, %struct.ScmObj* %argslist53658$k474651)
store volatile %struct.ScmObj* %argslist53658$k474652, %struct.ScmObj** %stackaddr$prim55285, align 8
%clofunc55286 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47465)
musttail call tailcc void %clofunc55286(%struct.ScmObj* %k47465, %struct.ScmObj* %argslist53658$k474652)
ret void
}

define tailcc void @proc_clo$ae47800(%struct.ScmObj* %env$ae47800,%struct.ScmObj* %current_45args53647) {
%stackaddr$env-ref55287 = alloca %struct.ScmObj*, align 8
%_37take47082 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47800, i64 0)
store %struct.ScmObj* %_37take47082, %struct.ScmObj** %stackaddr$env-ref55287
%stackaddr$prim55288 = alloca %struct.ScmObj*, align 8
%k47466 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53647)
store volatile %struct.ScmObj* %k47466, %struct.ScmObj** %stackaddr$prim55288, align 8
%stackaddr$prim55289 = alloca %struct.ScmObj*, align 8
%current_45args53648 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53647)
store volatile %struct.ScmObj* %current_45args53648, %struct.ScmObj** %stackaddr$prim55289, align 8
%stackaddr$prim55290 = alloca %struct.ScmObj*, align 8
%lst47084 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53648)
store volatile %struct.ScmObj* %lst47084, %struct.ScmObj** %stackaddr$prim55290, align 8
%stackaddr$prim55291 = alloca %struct.ScmObj*, align 8
%current_45args53649 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53648)
store volatile %struct.ScmObj* %current_45args53649, %struct.ScmObj** %stackaddr$prim55291, align 8
%stackaddr$prim55292 = alloca %struct.ScmObj*, align 8
%n47083 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53649)
store volatile %struct.ScmObj* %n47083, %struct.ScmObj** %stackaddr$prim55292, align 8
%ae47802 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55293 = alloca %struct.ScmObj*, align 8
%anf_45bind47207 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n47083, %struct.ScmObj* %ae47802)
store volatile %struct.ScmObj* %anf_45bind47207, %struct.ScmObj** %stackaddr$prim55293, align 8
%truthy$cmp55294 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47207)
%cmp$cmp55294 = icmp eq i64 %truthy$cmp55294, 1
br i1 %cmp$cmp55294, label %truebranch$cmp55294, label %falsebranch$cmp55294
truebranch$cmp55294:
%ae47805 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47806 = call %struct.ScmObj* @const_init_null()
%argslist53651$k474660 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55295 = alloca %struct.ScmObj*, align 8
%argslist53651$k474661 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47806, %struct.ScmObj* %argslist53651$k474660)
store volatile %struct.ScmObj* %argslist53651$k474661, %struct.ScmObj** %stackaddr$prim55295, align 8
%stackaddr$prim55296 = alloca %struct.ScmObj*, align 8
%argslist53651$k474662 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47805, %struct.ScmObj* %argslist53651$k474661)
store volatile %struct.ScmObj* %argslist53651$k474662, %struct.ScmObj** %stackaddr$prim55296, align 8
%clofunc55297 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47466)
musttail call tailcc void %clofunc55297(%struct.ScmObj* %k47466, %struct.ScmObj* %argslist53651$k474662)
ret void
falsebranch$cmp55294:
%stackaddr$prim55298 = alloca %struct.ScmObj*, align 8
%anf_45bind47208 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47084)
store volatile %struct.ScmObj* %anf_45bind47208, %struct.ScmObj** %stackaddr$prim55298, align 8
%truthy$cmp55299 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47208)
%cmp$cmp55299 = icmp eq i64 %truthy$cmp55299, 1
br i1 %cmp$cmp55299, label %truebranch$cmp55299, label %falsebranch$cmp55299
truebranch$cmp55299:
%ae47816 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47817 = call %struct.ScmObj* @const_init_null()
%argslist53652$k474660 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55300 = alloca %struct.ScmObj*, align 8
%argslist53652$k474661 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47817, %struct.ScmObj* %argslist53652$k474660)
store volatile %struct.ScmObj* %argslist53652$k474661, %struct.ScmObj** %stackaddr$prim55300, align 8
%stackaddr$prim55301 = alloca %struct.ScmObj*, align 8
%argslist53652$k474662 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47816, %struct.ScmObj* %argslist53652$k474661)
store volatile %struct.ScmObj* %argslist53652$k474662, %struct.ScmObj** %stackaddr$prim55301, align 8
%clofunc55302 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47466)
musttail call tailcc void %clofunc55302(%struct.ScmObj* %k47466, %struct.ScmObj* %argslist53652$k474662)
ret void
falsebranch$cmp55299:
%stackaddr$prim55303 = alloca %struct.ScmObj*, align 8
%anf_45bind47209 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47084)
store volatile %struct.ScmObj* %anf_45bind47209, %struct.ScmObj** %stackaddr$prim55303, align 8
%stackaddr$prim55304 = alloca %struct.ScmObj*, align 8
%anf_45bind47210 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47084)
store volatile %struct.ScmObj* %anf_45bind47210, %struct.ScmObj** %stackaddr$prim55304, align 8
%ae47827 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55305 = alloca %struct.ScmObj*, align 8
%anf_45bind47211 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n47083, %struct.ScmObj* %ae47827)
store volatile %struct.ScmObj* %anf_45bind47211, %struct.ScmObj** %stackaddr$prim55305, align 8
%stackaddr$makeclosure55306 = alloca %struct.ScmObj*, align 8
%fptrToInt55307 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47829 to i64
%ae47829 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55307)
store volatile %struct.ScmObj* %ae47829, %struct.ScmObj** %stackaddr$makeclosure55306, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47829, %struct.ScmObj* %k47466, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47829, %struct.ScmObj* %anf_45bind47209, i64 1)
%argslist53657$_37take470820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55308 = alloca %struct.ScmObj*, align 8
%argslist53657$_37take470821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47211, %struct.ScmObj* %argslist53657$_37take470820)
store volatile %struct.ScmObj* %argslist53657$_37take470821, %struct.ScmObj** %stackaddr$prim55308, align 8
%stackaddr$prim55309 = alloca %struct.ScmObj*, align 8
%argslist53657$_37take470822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47210, %struct.ScmObj* %argslist53657$_37take470821)
store volatile %struct.ScmObj* %argslist53657$_37take470822, %struct.ScmObj** %stackaddr$prim55309, align 8
%stackaddr$prim55310 = alloca %struct.ScmObj*, align 8
%argslist53657$_37take470823 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47829, %struct.ScmObj* %argslist53657$_37take470822)
store volatile %struct.ScmObj* %argslist53657$_37take470823, %struct.ScmObj** %stackaddr$prim55310, align 8
%clofunc55311 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take47082)
musttail call tailcc void %clofunc55311(%struct.ScmObj* %_37take47082, %struct.ScmObj* %argslist53657$_37take470823)
ret void
}

define tailcc void @proc_clo$ae47829(%struct.ScmObj* %env$ae47829,%struct.ScmObj* %current_45args53653) {
%stackaddr$env-ref55312 = alloca %struct.ScmObj*, align 8
%k47466 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47829, i64 0)
store %struct.ScmObj* %k47466, %struct.ScmObj** %stackaddr$env-ref55312
%stackaddr$env-ref55313 = alloca %struct.ScmObj*, align 8
%anf_45bind47209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47829, i64 1)
store %struct.ScmObj* %anf_45bind47209, %struct.ScmObj** %stackaddr$env-ref55313
%stackaddr$prim55314 = alloca %struct.ScmObj*, align 8
%_95k47467 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53653)
store volatile %struct.ScmObj* %_95k47467, %struct.ScmObj** %stackaddr$prim55314, align 8
%stackaddr$prim55315 = alloca %struct.ScmObj*, align 8
%current_45args53654 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53653)
store volatile %struct.ScmObj* %current_45args53654, %struct.ScmObj** %stackaddr$prim55315, align 8
%stackaddr$prim55316 = alloca %struct.ScmObj*, align 8
%anf_45bind47212 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53654)
store volatile %struct.ScmObj* %anf_45bind47212, %struct.ScmObj** %stackaddr$prim55316, align 8
%stackaddr$prim55317 = alloca %struct.ScmObj*, align 8
%cpsprim47468 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47209, %struct.ScmObj* %anf_45bind47212)
store volatile %struct.ScmObj* %cpsprim47468, %struct.ScmObj** %stackaddr$prim55317, align 8
%ae47835 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53656$k474660 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55318 = alloca %struct.ScmObj*, align 8
%argslist53656$k474661 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47468, %struct.ScmObj* %argslist53656$k474660)
store volatile %struct.ScmObj* %argslist53656$k474661, %struct.ScmObj** %stackaddr$prim55318, align 8
%stackaddr$prim55319 = alloca %struct.ScmObj*, align 8
%argslist53656$k474662 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47835, %struct.ScmObj* %argslist53656$k474661)
store volatile %struct.ScmObj* %argslist53656$k474662, %struct.ScmObj** %stackaddr$prim55319, align 8
%clofunc55320 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47466)
musttail call tailcc void %clofunc55320(%struct.ScmObj* %k47466, %struct.ScmObj* %argslist53656$k474662)
ret void
}

define tailcc void @proc_clo$ae47700(%struct.ScmObj* %env$ae47700,%struct.ScmObj* %current_45args53661) {
%stackaddr$prim55321 = alloca %struct.ScmObj*, align 8
%k47469 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53661)
store volatile %struct.ScmObj* %k47469, %struct.ScmObj** %stackaddr$prim55321, align 8
%stackaddr$prim55322 = alloca %struct.ScmObj*, align 8
%current_45args53662 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53661)
store volatile %struct.ScmObj* %current_45args53662, %struct.ScmObj** %stackaddr$prim55322, align 8
%stackaddr$prim55323 = alloca %struct.ScmObj*, align 8
%_37map47086 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53662)
store volatile %struct.ScmObj* %_37map47086, %struct.ScmObj** %stackaddr$prim55323, align 8
%ae47702 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55324 = alloca %struct.ScmObj*, align 8
%fptrToInt55325 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47703 to i64
%ae47703 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55325)
store volatile %struct.ScmObj* %ae47703, %struct.ScmObj** %stackaddr$makeclosure55324, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47703, %struct.ScmObj* %_37map47086, i64 0)
%argslist53678$k474690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55326 = alloca %struct.ScmObj*, align 8
%argslist53678$k474691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47703, %struct.ScmObj* %argslist53678$k474690)
store volatile %struct.ScmObj* %argslist53678$k474691, %struct.ScmObj** %stackaddr$prim55326, align 8
%stackaddr$prim55327 = alloca %struct.ScmObj*, align 8
%argslist53678$k474692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47702, %struct.ScmObj* %argslist53678$k474691)
store volatile %struct.ScmObj* %argslist53678$k474692, %struct.ScmObj** %stackaddr$prim55327, align 8
%clofunc55328 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47469)
musttail call tailcc void %clofunc55328(%struct.ScmObj* %k47469, %struct.ScmObj* %argslist53678$k474692)
ret void
}

define tailcc void @proc_clo$ae47703(%struct.ScmObj* %env$ae47703,%struct.ScmObj* %current_45args53664) {
%stackaddr$env-ref55329 = alloca %struct.ScmObj*, align 8
%_37map47086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47703, i64 0)
store %struct.ScmObj* %_37map47086, %struct.ScmObj** %stackaddr$env-ref55329
%stackaddr$prim55330 = alloca %struct.ScmObj*, align 8
%k47470 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53664)
store volatile %struct.ScmObj* %k47470, %struct.ScmObj** %stackaddr$prim55330, align 8
%stackaddr$prim55331 = alloca %struct.ScmObj*, align 8
%current_45args53665 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53664)
store volatile %struct.ScmObj* %current_45args53665, %struct.ScmObj** %stackaddr$prim55331, align 8
%stackaddr$prim55332 = alloca %struct.ScmObj*, align 8
%f47088 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53665)
store volatile %struct.ScmObj* %f47088, %struct.ScmObj** %stackaddr$prim55332, align 8
%stackaddr$prim55333 = alloca %struct.ScmObj*, align 8
%current_45args53666 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53665)
store volatile %struct.ScmObj* %current_45args53666, %struct.ScmObj** %stackaddr$prim55333, align 8
%stackaddr$prim55334 = alloca %struct.ScmObj*, align 8
%lst47087 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53666)
store volatile %struct.ScmObj* %lst47087, %struct.ScmObj** %stackaddr$prim55334, align 8
%stackaddr$prim55335 = alloca %struct.ScmObj*, align 8
%anf_45bind47201 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47087)
store volatile %struct.ScmObj* %anf_45bind47201, %struct.ScmObj** %stackaddr$prim55335, align 8
%truthy$cmp55336 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47201)
%cmp$cmp55336 = icmp eq i64 %truthy$cmp55336, 1
br i1 %cmp$cmp55336, label %truebranch$cmp55336, label %falsebranch$cmp55336
truebranch$cmp55336:
%ae47707 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47708 = call %struct.ScmObj* @const_init_null()
%argslist53668$k474700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55337 = alloca %struct.ScmObj*, align 8
%argslist53668$k474701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47708, %struct.ScmObj* %argslist53668$k474700)
store volatile %struct.ScmObj* %argslist53668$k474701, %struct.ScmObj** %stackaddr$prim55337, align 8
%stackaddr$prim55338 = alloca %struct.ScmObj*, align 8
%argslist53668$k474702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47707, %struct.ScmObj* %argslist53668$k474701)
store volatile %struct.ScmObj* %argslist53668$k474702, %struct.ScmObj** %stackaddr$prim55338, align 8
%clofunc55339 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47470)
musttail call tailcc void %clofunc55339(%struct.ScmObj* %k47470, %struct.ScmObj* %argslist53668$k474702)
ret void
falsebranch$cmp55336:
%stackaddr$prim55340 = alloca %struct.ScmObj*, align 8
%anf_45bind47202 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47087)
store volatile %struct.ScmObj* %anf_45bind47202, %struct.ScmObj** %stackaddr$prim55340, align 8
%stackaddr$makeclosure55341 = alloca %struct.ScmObj*, align 8
%fptrToInt55342 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47717 to i64
%ae47717 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55342)
store volatile %struct.ScmObj* %ae47717, %struct.ScmObj** %stackaddr$makeclosure55341, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47717, %struct.ScmObj* %k47470, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47717, %struct.ScmObj* %_37map47086, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47717, %struct.ScmObj* %f47088, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae47717, %struct.ScmObj* %lst47087, i64 3)
%argslist53677$f470880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55343 = alloca %struct.ScmObj*, align 8
%argslist53677$f470881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47202, %struct.ScmObj* %argslist53677$f470880)
store volatile %struct.ScmObj* %argslist53677$f470881, %struct.ScmObj** %stackaddr$prim55343, align 8
%stackaddr$prim55344 = alloca %struct.ScmObj*, align 8
%argslist53677$f470882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47717, %struct.ScmObj* %argslist53677$f470881)
store volatile %struct.ScmObj* %argslist53677$f470882, %struct.ScmObj** %stackaddr$prim55344, align 8
%clofunc55345 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47088)
musttail call tailcc void %clofunc55345(%struct.ScmObj* %f47088, %struct.ScmObj* %argslist53677$f470882)
ret void
}

define tailcc void @proc_clo$ae47717(%struct.ScmObj* %env$ae47717,%struct.ScmObj* %current_45args53669) {
%stackaddr$env-ref55346 = alloca %struct.ScmObj*, align 8
%k47470 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47717, i64 0)
store %struct.ScmObj* %k47470, %struct.ScmObj** %stackaddr$env-ref55346
%stackaddr$env-ref55347 = alloca %struct.ScmObj*, align 8
%_37map47086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47717, i64 1)
store %struct.ScmObj* %_37map47086, %struct.ScmObj** %stackaddr$env-ref55347
%stackaddr$env-ref55348 = alloca %struct.ScmObj*, align 8
%f47088 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47717, i64 2)
store %struct.ScmObj* %f47088, %struct.ScmObj** %stackaddr$env-ref55348
%stackaddr$env-ref55349 = alloca %struct.ScmObj*, align 8
%lst47087 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47717, i64 3)
store %struct.ScmObj* %lst47087, %struct.ScmObj** %stackaddr$env-ref55349
%stackaddr$prim55350 = alloca %struct.ScmObj*, align 8
%_95k47471 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53669)
store volatile %struct.ScmObj* %_95k47471, %struct.ScmObj** %stackaddr$prim55350, align 8
%stackaddr$prim55351 = alloca %struct.ScmObj*, align 8
%current_45args53670 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53669)
store volatile %struct.ScmObj* %current_45args53670, %struct.ScmObj** %stackaddr$prim55351, align 8
%stackaddr$prim55352 = alloca %struct.ScmObj*, align 8
%anf_45bind47203 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53670)
store volatile %struct.ScmObj* %anf_45bind47203, %struct.ScmObj** %stackaddr$prim55352, align 8
%stackaddr$prim55353 = alloca %struct.ScmObj*, align 8
%anf_45bind47204 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47087)
store volatile %struct.ScmObj* %anf_45bind47204, %struct.ScmObj** %stackaddr$prim55353, align 8
%stackaddr$makeclosure55354 = alloca %struct.ScmObj*, align 8
%fptrToInt55355 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47721 to i64
%ae47721 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55355)
store volatile %struct.ScmObj* %ae47721, %struct.ScmObj** %stackaddr$makeclosure55354, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47721, %struct.ScmObj* %anf_45bind47203, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47721, %struct.ScmObj* %k47470, i64 1)
%argslist53676$_37map470860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55356 = alloca %struct.ScmObj*, align 8
%argslist53676$_37map470861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47204, %struct.ScmObj* %argslist53676$_37map470860)
store volatile %struct.ScmObj* %argslist53676$_37map470861, %struct.ScmObj** %stackaddr$prim55356, align 8
%stackaddr$prim55357 = alloca %struct.ScmObj*, align 8
%argslist53676$_37map470862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47088, %struct.ScmObj* %argslist53676$_37map470861)
store volatile %struct.ScmObj* %argslist53676$_37map470862, %struct.ScmObj** %stackaddr$prim55357, align 8
%stackaddr$prim55358 = alloca %struct.ScmObj*, align 8
%argslist53676$_37map470863 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47721, %struct.ScmObj* %argslist53676$_37map470862)
store volatile %struct.ScmObj* %argslist53676$_37map470863, %struct.ScmObj** %stackaddr$prim55358, align 8
%clofunc55359 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map47086)
musttail call tailcc void %clofunc55359(%struct.ScmObj* %_37map47086, %struct.ScmObj* %argslist53676$_37map470863)
ret void
}

define tailcc void @proc_clo$ae47721(%struct.ScmObj* %env$ae47721,%struct.ScmObj* %current_45args53672) {
%stackaddr$env-ref55360 = alloca %struct.ScmObj*, align 8
%anf_45bind47203 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47721, i64 0)
store %struct.ScmObj* %anf_45bind47203, %struct.ScmObj** %stackaddr$env-ref55360
%stackaddr$env-ref55361 = alloca %struct.ScmObj*, align 8
%k47470 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47721, i64 1)
store %struct.ScmObj* %k47470, %struct.ScmObj** %stackaddr$env-ref55361
%stackaddr$prim55362 = alloca %struct.ScmObj*, align 8
%_95k47472 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53672)
store volatile %struct.ScmObj* %_95k47472, %struct.ScmObj** %stackaddr$prim55362, align 8
%stackaddr$prim55363 = alloca %struct.ScmObj*, align 8
%current_45args53673 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53672)
store volatile %struct.ScmObj* %current_45args53673, %struct.ScmObj** %stackaddr$prim55363, align 8
%stackaddr$prim55364 = alloca %struct.ScmObj*, align 8
%anf_45bind47205 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53673)
store volatile %struct.ScmObj* %anf_45bind47205, %struct.ScmObj** %stackaddr$prim55364, align 8
%stackaddr$prim55365 = alloca %struct.ScmObj*, align 8
%cpsprim47473 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47203, %struct.ScmObj* %anf_45bind47205)
store volatile %struct.ScmObj* %cpsprim47473, %struct.ScmObj** %stackaddr$prim55365, align 8
%ae47727 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53675$k474700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55366 = alloca %struct.ScmObj*, align 8
%argslist53675$k474701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47473, %struct.ScmObj* %argslist53675$k474700)
store volatile %struct.ScmObj* %argslist53675$k474701, %struct.ScmObj** %stackaddr$prim55366, align 8
%stackaddr$prim55367 = alloca %struct.ScmObj*, align 8
%argslist53675$k474702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47727, %struct.ScmObj* %argslist53675$k474701)
store volatile %struct.ScmObj* %argslist53675$k474702, %struct.ScmObj** %stackaddr$prim55367, align 8
%clofunc55368 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47470)
musttail call tailcc void %clofunc55368(%struct.ScmObj* %k47470, %struct.ScmObj* %argslist53675$k474702)
ret void
}

define tailcc void @proc_clo$ae47620(%struct.ScmObj* %env$ae47620,%struct.ScmObj* %current_45args53681) {
%stackaddr$prim55369 = alloca %struct.ScmObj*, align 8
%k47474 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53681)
store volatile %struct.ScmObj* %k47474, %struct.ScmObj** %stackaddr$prim55369, align 8
%stackaddr$prim55370 = alloca %struct.ScmObj*, align 8
%current_45args53682 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53681)
store volatile %struct.ScmObj* %current_45args53682, %struct.ScmObj** %stackaddr$prim55370, align 8
%stackaddr$prim55371 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53682)
store volatile %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$prim55371, align 8
%ae47622 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55372 = alloca %struct.ScmObj*, align 8
%fptrToInt55373 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47623 to i64
%ae47623 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55373)
store volatile %struct.ScmObj* %ae47623, %struct.ScmObj** %stackaddr$makeclosure55372, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47623, %struct.ScmObj* %_37foldr147090, i64 0)
%argslist53695$k474740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55374 = alloca %struct.ScmObj*, align 8
%argslist53695$k474741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47623, %struct.ScmObj* %argslist53695$k474740)
store volatile %struct.ScmObj* %argslist53695$k474741, %struct.ScmObj** %stackaddr$prim55374, align 8
%stackaddr$prim55375 = alloca %struct.ScmObj*, align 8
%argslist53695$k474742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47622, %struct.ScmObj* %argslist53695$k474741)
store volatile %struct.ScmObj* %argslist53695$k474742, %struct.ScmObj** %stackaddr$prim55375, align 8
%clofunc55376 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47474)
musttail call tailcc void %clofunc55376(%struct.ScmObj* %k47474, %struct.ScmObj* %argslist53695$k474742)
ret void
}

define tailcc void @proc_clo$ae47623(%struct.ScmObj* %env$ae47623,%struct.ScmObj* %current_45args53684) {
%stackaddr$env-ref55377 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47623, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref55377
%stackaddr$prim55378 = alloca %struct.ScmObj*, align 8
%k47475 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53684)
store volatile %struct.ScmObj* %k47475, %struct.ScmObj** %stackaddr$prim55378, align 8
%stackaddr$prim55379 = alloca %struct.ScmObj*, align 8
%current_45args53685 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53684)
store volatile %struct.ScmObj* %current_45args53685, %struct.ScmObj** %stackaddr$prim55379, align 8
%stackaddr$prim55380 = alloca %struct.ScmObj*, align 8
%f47093 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53685)
store volatile %struct.ScmObj* %f47093, %struct.ScmObj** %stackaddr$prim55380, align 8
%stackaddr$prim55381 = alloca %struct.ScmObj*, align 8
%current_45args53686 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53685)
store volatile %struct.ScmObj* %current_45args53686, %struct.ScmObj** %stackaddr$prim55381, align 8
%stackaddr$prim55382 = alloca %struct.ScmObj*, align 8
%acc47092 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53686)
store volatile %struct.ScmObj* %acc47092, %struct.ScmObj** %stackaddr$prim55382, align 8
%stackaddr$prim55383 = alloca %struct.ScmObj*, align 8
%current_45args53687 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53686)
store volatile %struct.ScmObj* %current_45args53687, %struct.ScmObj** %stackaddr$prim55383, align 8
%stackaddr$prim55384 = alloca %struct.ScmObj*, align 8
%lst47091 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53687)
store volatile %struct.ScmObj* %lst47091, %struct.ScmObj** %stackaddr$prim55384, align 8
%stackaddr$prim55385 = alloca %struct.ScmObj*, align 8
%anf_45bind47196 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47091)
store volatile %struct.ScmObj* %anf_45bind47196, %struct.ScmObj** %stackaddr$prim55385, align 8
%truthy$cmp55386 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47196)
%cmp$cmp55386 = icmp eq i64 %truthy$cmp55386, 1
br i1 %cmp$cmp55386, label %truebranch$cmp55386, label %falsebranch$cmp55386
truebranch$cmp55386:
%ae47627 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53689$k474750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55387 = alloca %struct.ScmObj*, align 8
%argslist53689$k474751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47092, %struct.ScmObj* %argslist53689$k474750)
store volatile %struct.ScmObj* %argslist53689$k474751, %struct.ScmObj** %stackaddr$prim55387, align 8
%stackaddr$prim55388 = alloca %struct.ScmObj*, align 8
%argslist53689$k474752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47627, %struct.ScmObj* %argslist53689$k474751)
store volatile %struct.ScmObj* %argslist53689$k474752, %struct.ScmObj** %stackaddr$prim55388, align 8
%clofunc55389 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47475)
musttail call tailcc void %clofunc55389(%struct.ScmObj* %k47475, %struct.ScmObj* %argslist53689$k474752)
ret void
falsebranch$cmp55386:
%stackaddr$prim55390 = alloca %struct.ScmObj*, align 8
%anf_45bind47197 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47091)
store volatile %struct.ScmObj* %anf_45bind47197, %struct.ScmObj** %stackaddr$prim55390, align 8
%stackaddr$prim55391 = alloca %struct.ScmObj*, align 8
%anf_45bind47198 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47091)
store volatile %struct.ScmObj* %anf_45bind47198, %struct.ScmObj** %stackaddr$prim55391, align 8
%stackaddr$makeclosure55392 = alloca %struct.ScmObj*, align 8
%fptrToInt55393 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47635 to i64
%ae47635 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55393)
store volatile %struct.ScmObj* %ae47635, %struct.ScmObj** %stackaddr$makeclosure55392, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47635, %struct.ScmObj* %f47093, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47635, %struct.ScmObj* %k47475, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47635, %struct.ScmObj* %anf_45bind47197, i64 2)
%argslist53694$_37foldr1470900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55394 = alloca %struct.ScmObj*, align 8
%argslist53694$_37foldr1470901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47198, %struct.ScmObj* %argslist53694$_37foldr1470900)
store volatile %struct.ScmObj* %argslist53694$_37foldr1470901, %struct.ScmObj** %stackaddr$prim55394, align 8
%stackaddr$prim55395 = alloca %struct.ScmObj*, align 8
%argslist53694$_37foldr1470902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47092, %struct.ScmObj* %argslist53694$_37foldr1470901)
store volatile %struct.ScmObj* %argslist53694$_37foldr1470902, %struct.ScmObj** %stackaddr$prim55395, align 8
%stackaddr$prim55396 = alloca %struct.ScmObj*, align 8
%argslist53694$_37foldr1470903 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47093, %struct.ScmObj* %argslist53694$_37foldr1470902)
store volatile %struct.ScmObj* %argslist53694$_37foldr1470903, %struct.ScmObj** %stackaddr$prim55396, align 8
%stackaddr$prim55397 = alloca %struct.ScmObj*, align 8
%argslist53694$_37foldr1470904 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47635, %struct.ScmObj* %argslist53694$_37foldr1470903)
store volatile %struct.ScmObj* %argslist53694$_37foldr1470904, %struct.ScmObj** %stackaddr$prim55397, align 8
%clofunc55398 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147090)
musttail call tailcc void %clofunc55398(%struct.ScmObj* %_37foldr147090, %struct.ScmObj* %argslist53694$_37foldr1470904)
ret void
}

define tailcc void @proc_clo$ae47635(%struct.ScmObj* %env$ae47635,%struct.ScmObj* %current_45args53690) {
%stackaddr$env-ref55399 = alloca %struct.ScmObj*, align 8
%f47093 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47635, i64 0)
store %struct.ScmObj* %f47093, %struct.ScmObj** %stackaddr$env-ref55399
%stackaddr$env-ref55400 = alloca %struct.ScmObj*, align 8
%k47475 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47635, i64 1)
store %struct.ScmObj* %k47475, %struct.ScmObj** %stackaddr$env-ref55400
%stackaddr$env-ref55401 = alloca %struct.ScmObj*, align 8
%anf_45bind47197 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47635, i64 2)
store %struct.ScmObj* %anf_45bind47197, %struct.ScmObj** %stackaddr$env-ref55401
%stackaddr$prim55402 = alloca %struct.ScmObj*, align 8
%_95k47476 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53690)
store volatile %struct.ScmObj* %_95k47476, %struct.ScmObj** %stackaddr$prim55402, align 8
%stackaddr$prim55403 = alloca %struct.ScmObj*, align 8
%current_45args53691 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53690)
store volatile %struct.ScmObj* %current_45args53691, %struct.ScmObj** %stackaddr$prim55403, align 8
%stackaddr$prim55404 = alloca %struct.ScmObj*, align 8
%anf_45bind47199 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53691)
store volatile %struct.ScmObj* %anf_45bind47199, %struct.ScmObj** %stackaddr$prim55404, align 8
%argslist53693$f470930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55405 = alloca %struct.ScmObj*, align 8
%argslist53693$f470931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47199, %struct.ScmObj* %argslist53693$f470930)
store volatile %struct.ScmObj* %argslist53693$f470931, %struct.ScmObj** %stackaddr$prim55405, align 8
%stackaddr$prim55406 = alloca %struct.ScmObj*, align 8
%argslist53693$f470932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47197, %struct.ScmObj* %argslist53693$f470931)
store volatile %struct.ScmObj* %argslist53693$f470932, %struct.ScmObj** %stackaddr$prim55406, align 8
%stackaddr$prim55407 = alloca %struct.ScmObj*, align 8
%argslist53693$f470933 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47475, %struct.ScmObj* %argslist53693$f470932)
store volatile %struct.ScmObj* %argslist53693$f470933, %struct.ScmObj** %stackaddr$prim55407, align 8
%clofunc55408 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47093)
musttail call tailcc void %clofunc55408(%struct.ScmObj* %f47093, %struct.ScmObj* %argslist53693$f470933)
ret void
}

define tailcc void @proc_clo$ae47503(%struct.ScmObj* %env$ae47503,%struct.ScmObj* %current_45args53698) {
%stackaddr$prim55409 = alloca %struct.ScmObj*, align 8
%k47477 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53698)
store volatile %struct.ScmObj* %k47477, %struct.ScmObj** %stackaddr$prim55409, align 8
%stackaddr$prim55410 = alloca %struct.ScmObj*, align 8
%current_45args53699 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53698)
store volatile %struct.ScmObj* %current_45args53699, %struct.ScmObj** %stackaddr$prim55410, align 8
%stackaddr$prim55411 = alloca %struct.ScmObj*, align 8
%y47070 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53699)
store volatile %struct.ScmObj* %y47070, %struct.ScmObj** %stackaddr$prim55411, align 8
%ae47505 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55412 = alloca %struct.ScmObj*, align 8
%fptrToInt55413 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47506 to i64
%ae47506 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55413)
store volatile %struct.ScmObj* %ae47506, %struct.ScmObj** %stackaddr$makeclosure55412, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47506, %struct.ScmObj* %y47070, i64 0)
%argslist53717$k474770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55414 = alloca %struct.ScmObj*, align 8
%argslist53717$k474771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47506, %struct.ScmObj* %argslist53717$k474770)
store volatile %struct.ScmObj* %argslist53717$k474771, %struct.ScmObj** %stackaddr$prim55414, align 8
%stackaddr$prim55415 = alloca %struct.ScmObj*, align 8
%argslist53717$k474772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47505, %struct.ScmObj* %argslist53717$k474771)
store volatile %struct.ScmObj* %argslist53717$k474772, %struct.ScmObj** %stackaddr$prim55415, align 8
%clofunc55416 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47477)
musttail call tailcc void %clofunc55416(%struct.ScmObj* %k47477, %struct.ScmObj* %argslist53717$k474772)
ret void
}

define tailcc void @proc_clo$ae47506(%struct.ScmObj* %env$ae47506,%struct.ScmObj* %current_45args53701) {
%stackaddr$env-ref55417 = alloca %struct.ScmObj*, align 8
%y47070 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47506, i64 0)
store %struct.ScmObj* %y47070, %struct.ScmObj** %stackaddr$env-ref55417
%stackaddr$prim55418 = alloca %struct.ScmObj*, align 8
%k47478 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53701)
store volatile %struct.ScmObj* %k47478, %struct.ScmObj** %stackaddr$prim55418, align 8
%stackaddr$prim55419 = alloca %struct.ScmObj*, align 8
%current_45args53702 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53701)
store volatile %struct.ScmObj* %current_45args53702, %struct.ScmObj** %stackaddr$prim55419, align 8
%stackaddr$prim55420 = alloca %struct.ScmObj*, align 8
%f47071 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53702)
store volatile %struct.ScmObj* %f47071, %struct.ScmObj** %stackaddr$prim55420, align 8
%stackaddr$makeclosure55421 = alloca %struct.ScmObj*, align 8
%fptrToInt55422 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47507 to i64
%ae47507 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55422)
store volatile %struct.ScmObj* %ae47507, %struct.ScmObj** %stackaddr$makeclosure55421, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47507, %struct.ScmObj* %k47478, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47507, %struct.ScmObj* %f47071, i64 1)
%ae47508 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55423 = alloca %struct.ScmObj*, align 8
%fptrToInt55424 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47509 to i64
%ae47509 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55424)
store volatile %struct.ScmObj* %ae47509, %struct.ScmObj** %stackaddr$makeclosure55423, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47509, %struct.ScmObj* %f47071, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47509, %struct.ScmObj* %y47070, i64 1)
%argslist53716$ae475070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55425 = alloca %struct.ScmObj*, align 8
%argslist53716$ae475071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47509, %struct.ScmObj* %argslist53716$ae475070)
store volatile %struct.ScmObj* %argslist53716$ae475071, %struct.ScmObj** %stackaddr$prim55425, align 8
%stackaddr$prim55426 = alloca %struct.ScmObj*, align 8
%argslist53716$ae475072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47508, %struct.ScmObj* %argslist53716$ae475071)
store volatile %struct.ScmObj* %argslist53716$ae475072, %struct.ScmObj** %stackaddr$prim55426, align 8
%clofunc55427 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47507)
musttail call tailcc void %clofunc55427(%struct.ScmObj* %ae47507, %struct.ScmObj* %argslist53716$ae475072)
ret void
}

define tailcc void @proc_clo$ae47507(%struct.ScmObj* %env$ae47507,%struct.ScmObj* %current_45args53704) {
%stackaddr$env-ref55428 = alloca %struct.ScmObj*, align 8
%k47478 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47507, i64 0)
store %struct.ScmObj* %k47478, %struct.ScmObj** %stackaddr$env-ref55428
%stackaddr$env-ref55429 = alloca %struct.ScmObj*, align 8
%f47071 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47507, i64 1)
store %struct.ScmObj* %f47071, %struct.ScmObj** %stackaddr$env-ref55429
%stackaddr$prim55430 = alloca %struct.ScmObj*, align 8
%_95k47479 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53704)
store volatile %struct.ScmObj* %_95k47479, %struct.ScmObj** %stackaddr$prim55430, align 8
%stackaddr$prim55431 = alloca %struct.ScmObj*, align 8
%current_45args53705 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53704)
store volatile %struct.ScmObj* %current_45args53705, %struct.ScmObj** %stackaddr$prim55431, align 8
%stackaddr$prim55432 = alloca %struct.ScmObj*, align 8
%anf_45bind47194 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53705)
store volatile %struct.ScmObj* %anf_45bind47194, %struct.ScmObj** %stackaddr$prim55432, align 8
%argslist53707$f470710 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55433 = alloca %struct.ScmObj*, align 8
%argslist53707$f470711 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47194, %struct.ScmObj* %argslist53707$f470710)
store volatile %struct.ScmObj* %argslist53707$f470711, %struct.ScmObj** %stackaddr$prim55433, align 8
%stackaddr$prim55434 = alloca %struct.ScmObj*, align 8
%argslist53707$f470712 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47478, %struct.ScmObj* %argslist53707$f470711)
store volatile %struct.ScmObj* %argslist53707$f470712, %struct.ScmObj** %stackaddr$prim55434, align 8
%clofunc55435 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47071)
musttail call tailcc void %clofunc55435(%struct.ScmObj* %f47071, %struct.ScmObj* %argslist53707$f470712)
ret void
}

define tailcc void @proc_clo$ae47509(%struct.ScmObj* %env$ae47509,%struct.ScmObj* %args4707247480) {
%stackaddr$env-ref55436 = alloca %struct.ScmObj*, align 8
%f47071 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47509, i64 0)
store %struct.ScmObj* %f47071, %struct.ScmObj** %stackaddr$env-ref55436
%stackaddr$env-ref55437 = alloca %struct.ScmObj*, align 8
%y47070 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47509, i64 1)
store %struct.ScmObj* %y47070, %struct.ScmObj** %stackaddr$env-ref55437
%stackaddr$prim55438 = alloca %struct.ScmObj*, align 8
%k47481 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4707247480)
store volatile %struct.ScmObj* %k47481, %struct.ScmObj** %stackaddr$prim55438, align 8
%stackaddr$prim55439 = alloca %struct.ScmObj*, align 8
%args47072 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4707247480)
store volatile %struct.ScmObj* %args47072, %struct.ScmObj** %stackaddr$prim55439, align 8
%stackaddr$makeclosure55440 = alloca %struct.ScmObj*, align 8
%fptrToInt55441 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47513 to i64
%ae47513 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55441)
store volatile %struct.ScmObj* %ae47513, %struct.ScmObj** %stackaddr$makeclosure55440, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47513, %struct.ScmObj* %args47072, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47513, %struct.ScmObj* %f47071, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47513, %struct.ScmObj* %k47481, i64 2)
%argslist53715$y470700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55442 = alloca %struct.ScmObj*, align 8
%argslist53715$y470701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y47070, %struct.ScmObj* %argslist53715$y470700)
store volatile %struct.ScmObj* %argslist53715$y470701, %struct.ScmObj** %stackaddr$prim55442, align 8
%stackaddr$prim55443 = alloca %struct.ScmObj*, align 8
%argslist53715$y470702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47513, %struct.ScmObj* %argslist53715$y470701)
store volatile %struct.ScmObj* %argslist53715$y470702, %struct.ScmObj** %stackaddr$prim55443, align 8
%clofunc55444 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y47070)
musttail call tailcc void %clofunc55444(%struct.ScmObj* %y47070, %struct.ScmObj* %argslist53715$y470702)
ret void
}

define tailcc void @proc_clo$ae47513(%struct.ScmObj* %env$ae47513,%struct.ScmObj* %current_45args53708) {
%stackaddr$env-ref55445 = alloca %struct.ScmObj*, align 8
%args47072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47513, i64 0)
store %struct.ScmObj* %args47072, %struct.ScmObj** %stackaddr$env-ref55445
%stackaddr$env-ref55446 = alloca %struct.ScmObj*, align 8
%f47071 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47513, i64 1)
store %struct.ScmObj* %f47071, %struct.ScmObj** %stackaddr$env-ref55446
%stackaddr$env-ref55447 = alloca %struct.ScmObj*, align 8
%k47481 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47513, i64 2)
store %struct.ScmObj* %k47481, %struct.ScmObj** %stackaddr$env-ref55447
%stackaddr$prim55448 = alloca %struct.ScmObj*, align 8
%_95k47482 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53708)
store volatile %struct.ScmObj* %_95k47482, %struct.ScmObj** %stackaddr$prim55448, align 8
%stackaddr$prim55449 = alloca %struct.ScmObj*, align 8
%current_45args53709 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53708)
store volatile %struct.ScmObj* %current_45args53709, %struct.ScmObj** %stackaddr$prim55449, align 8
%stackaddr$prim55450 = alloca %struct.ScmObj*, align 8
%anf_45bind47192 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53709)
store volatile %struct.ScmObj* %anf_45bind47192, %struct.ScmObj** %stackaddr$prim55450, align 8
%stackaddr$makeclosure55451 = alloca %struct.ScmObj*, align 8
%fptrToInt55452 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47516 to i64
%ae47516 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55452)
store volatile %struct.ScmObj* %ae47516, %struct.ScmObj** %stackaddr$makeclosure55451, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47516, %struct.ScmObj* %args47072, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47516, %struct.ScmObj* %k47481, i64 1)
%argslist53714$anf_45bind471920 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55453 = alloca %struct.ScmObj*, align 8
%argslist53714$anf_45bind471921 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47071, %struct.ScmObj* %argslist53714$anf_45bind471920)
store volatile %struct.ScmObj* %argslist53714$anf_45bind471921, %struct.ScmObj** %stackaddr$prim55453, align 8
%stackaddr$prim55454 = alloca %struct.ScmObj*, align 8
%argslist53714$anf_45bind471922 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47516, %struct.ScmObj* %argslist53714$anf_45bind471921)
store volatile %struct.ScmObj* %argslist53714$anf_45bind471922, %struct.ScmObj** %stackaddr$prim55454, align 8
%clofunc55455 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47192)
musttail call tailcc void %clofunc55455(%struct.ScmObj* %anf_45bind47192, %struct.ScmObj* %argslist53714$anf_45bind471922)
ret void
}

define tailcc void @proc_clo$ae47516(%struct.ScmObj* %env$ae47516,%struct.ScmObj* %current_45args53711) {
%stackaddr$env-ref55456 = alloca %struct.ScmObj*, align 8
%args47072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47516, i64 0)
store %struct.ScmObj* %args47072, %struct.ScmObj** %stackaddr$env-ref55456
%stackaddr$env-ref55457 = alloca %struct.ScmObj*, align 8
%k47481 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47516, i64 1)
store %struct.ScmObj* %k47481, %struct.ScmObj** %stackaddr$env-ref55457
%stackaddr$prim55458 = alloca %struct.ScmObj*, align 8
%_95k47483 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53711)
store volatile %struct.ScmObj* %_95k47483, %struct.ScmObj** %stackaddr$prim55458, align 8
%stackaddr$prim55459 = alloca %struct.ScmObj*, align 8
%current_45args53712 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53711)
store volatile %struct.ScmObj* %current_45args53712, %struct.ScmObj** %stackaddr$prim55459, align 8
%stackaddr$prim55460 = alloca %struct.ScmObj*, align 8
%anf_45bind47193 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53712)
store volatile %struct.ScmObj* %anf_45bind47193, %struct.ScmObj** %stackaddr$prim55460, align 8
%stackaddr$prim55461 = alloca %struct.ScmObj*, align 8
%cpsargs47484 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47481, %struct.ScmObj* %args47072)
store volatile %struct.ScmObj* %cpsargs47484, %struct.ScmObj** %stackaddr$prim55461, align 8
%clofunc55462 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47193)
musttail call tailcc void %clofunc55462(%struct.ScmObj* %anf_45bind47193, %struct.ScmObj* %cpsargs47484)
ret void
}

define tailcc void @proc_clo$ae47488(%struct.ScmObj* %env$ae47488,%struct.ScmObj* %current_45args53719) {
%stackaddr$prim55463 = alloca %struct.ScmObj*, align 8
%k47485 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53719)
store volatile %struct.ScmObj* %k47485, %struct.ScmObj** %stackaddr$prim55463, align 8
%stackaddr$prim55464 = alloca %struct.ScmObj*, align 8
%current_45args53720 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53719)
store volatile %struct.ScmObj* %current_45args53720, %struct.ScmObj** %stackaddr$prim55464, align 8
%stackaddr$prim55465 = alloca %struct.ScmObj*, align 8
%yu47069 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53720)
store volatile %struct.ScmObj* %yu47069, %struct.ScmObj** %stackaddr$prim55465, align 8
%argslist53722$yu470690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55466 = alloca %struct.ScmObj*, align 8
%argslist53722$yu470691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu47069, %struct.ScmObj* %argslist53722$yu470690)
store volatile %struct.ScmObj* %argslist53722$yu470691, %struct.ScmObj** %stackaddr$prim55466, align 8
%stackaddr$prim55467 = alloca %struct.ScmObj*, align 8
%argslist53722$yu470692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47485, %struct.ScmObj* %argslist53722$yu470691)
store volatile %struct.ScmObj* %argslist53722$yu470692, %struct.ScmObj** %stackaddr$prim55467, align 8
%clofunc55468 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu47069)
musttail call tailcc void %clofunc55468(%struct.ScmObj* %yu47069, %struct.ScmObj* %argslist53722$yu470692)
ret void
}