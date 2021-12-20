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
%mainenv53685 = call %struct.ScmObj* @const_init_null()
%mainargs53686 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv53685, %struct.ScmObj* %mainargs53686)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv53683,%struct.ScmObj* %mainargs53684) {
%stackaddr$makeclosure53687 = alloca %struct.ScmObj*, align 8
%fptrToInt53688 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47479 to i64
%ae47479 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53688)
store volatile %struct.ScmObj* %ae47479, %struct.ScmObj** %stackaddr$makeclosure53687, align 8
%ae47480 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53689 = alloca %struct.ScmObj*, align 8
%fptrToInt53690 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47481 to i64
%ae47481 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53690)
store volatile %struct.ScmObj* %ae47481, %struct.ScmObj** %stackaddr$makeclosure53689, align 8
%argslist53682$ae474790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53691 = alloca %struct.ScmObj*, align 8
%argslist53682$ae474791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47481, %struct.ScmObj* %argslist53682$ae474790)
store volatile %struct.ScmObj* %argslist53682$ae474791, %struct.ScmObj** %stackaddr$prim53691, align 8
%stackaddr$prim53692 = alloca %struct.ScmObj*, align 8
%argslist53682$ae474792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47480, %struct.ScmObj* %argslist53682$ae474791)
store volatile %struct.ScmObj* %argslist53682$ae474792, %struct.ScmObj** %stackaddr$prim53692, align 8
%clofunc53693 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47479)
musttail call tailcc void %clofunc53693(%struct.ScmObj* %ae47479, %struct.ScmObj* %argslist53682$ae474792)
ret void
}

define tailcc void @proc_clo$ae47479(%struct.ScmObj* %env$ae47479,%struct.ScmObj* %current_45args53133) {
%stackaddr$prim53694 = alloca %struct.ScmObj*, align 8
%_95k47311 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53133)
store volatile %struct.ScmObj* %_95k47311, %struct.ScmObj** %stackaddr$prim53694, align 8
%stackaddr$prim53695 = alloca %struct.ScmObj*, align 8
%current_45args53134 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53133)
store volatile %struct.ScmObj* %current_45args53134, %struct.ScmObj** %stackaddr$prim53695, align 8
%stackaddr$prim53696 = alloca %struct.ScmObj*, align 8
%anf_45bind47192 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53134)
store volatile %struct.ScmObj* %anf_45bind47192, %struct.ScmObj** %stackaddr$prim53696, align 8
%stackaddr$makeclosure53697 = alloca %struct.ScmObj*, align 8
%fptrToInt53698 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47494 to i64
%ae47494 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53698)
store volatile %struct.ScmObj* %ae47494, %struct.ScmObj** %stackaddr$makeclosure53697, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47494, %struct.ScmObj* %anf_45bind47192, i64 0)
%ae47495 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53699 = alloca %struct.ScmObj*, align 8
%fptrToInt53700 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47496 to i64
%ae47496 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53700)
store volatile %struct.ScmObj* %ae47496, %struct.ScmObj** %stackaddr$makeclosure53699, align 8
%argslist53677$ae474940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53701 = alloca %struct.ScmObj*, align 8
%argslist53677$ae474941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47496, %struct.ScmObj* %argslist53677$ae474940)
store volatile %struct.ScmObj* %argslist53677$ae474941, %struct.ScmObj** %stackaddr$prim53701, align 8
%stackaddr$prim53702 = alloca %struct.ScmObj*, align 8
%argslist53677$ae474942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47495, %struct.ScmObj* %argslist53677$ae474941)
store volatile %struct.ScmObj* %argslist53677$ae474942, %struct.ScmObj** %stackaddr$prim53702, align 8
%clofunc53703 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47494)
musttail call tailcc void %clofunc53703(%struct.ScmObj* %ae47494, %struct.ScmObj* %argslist53677$ae474942)
ret void
}

define tailcc void @proc_clo$ae47494(%struct.ScmObj* %env$ae47494,%struct.ScmObj* %current_45args53136) {
%stackaddr$env-ref53704 = alloca %struct.ScmObj*, align 8
%anf_45bind47192 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47494, i64 0)
store %struct.ScmObj* %anf_45bind47192, %struct.ScmObj** %stackaddr$env-ref53704
%stackaddr$prim53705 = alloca %struct.ScmObj*, align 8
%_95k47312 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53136)
store volatile %struct.ScmObj* %_95k47312, %struct.ScmObj** %stackaddr$prim53705, align 8
%stackaddr$prim53706 = alloca %struct.ScmObj*, align 8
%current_45args53137 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53136)
store volatile %struct.ScmObj* %current_45args53137, %struct.ScmObj** %stackaddr$prim53706, align 8
%stackaddr$prim53707 = alloca %struct.ScmObj*, align 8
%anf_45bind47196 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53137)
store volatile %struct.ScmObj* %anf_45bind47196, %struct.ScmObj** %stackaddr$prim53707, align 8
%stackaddr$makeclosure53708 = alloca %struct.ScmObj*, align 8
%fptrToInt53709 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47609 to i64
%ae47609 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53709)
store volatile %struct.ScmObj* %ae47609, %struct.ScmObj** %stackaddr$makeclosure53708, align 8
%argslist53656$anf_45bind471920 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53710 = alloca %struct.ScmObj*, align 8
%argslist53656$anf_45bind471921 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47196, %struct.ScmObj* %argslist53656$anf_45bind471920)
store volatile %struct.ScmObj* %argslist53656$anf_45bind471921, %struct.ScmObj** %stackaddr$prim53710, align 8
%stackaddr$prim53711 = alloca %struct.ScmObj*, align 8
%argslist53656$anf_45bind471922 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47609, %struct.ScmObj* %argslist53656$anf_45bind471921)
store volatile %struct.ScmObj* %argslist53656$anf_45bind471922, %struct.ScmObj** %stackaddr$prim53711, align 8
%clofunc53712 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47192)
musttail call tailcc void %clofunc53712(%struct.ScmObj* %anf_45bind47192, %struct.ScmObj* %argslist53656$anf_45bind471922)
ret void
}

define tailcc void @proc_clo$ae47609(%struct.ScmObj* %env$ae47609,%struct.ScmObj* %current_45args53139) {
%stackaddr$prim53713 = alloca %struct.ScmObj*, align 8
%_95k47313 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53139)
store volatile %struct.ScmObj* %_95k47313, %struct.ScmObj** %stackaddr$prim53713, align 8
%stackaddr$prim53714 = alloca %struct.ScmObj*, align 8
%current_45args53140 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53139)
store volatile %struct.ScmObj* %current_45args53140, %struct.ScmObj** %stackaddr$prim53714, align 8
%stackaddr$prim53715 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53140)
store volatile %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$prim53715, align 8
%stackaddr$makeclosure53716 = alloca %struct.ScmObj*, align 8
%fptrToInt53717 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47611 to i64
%ae47611 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53717)
store volatile %struct.ScmObj* %ae47611, %struct.ScmObj** %stackaddr$makeclosure53716, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47611, %struct.ScmObj* %Ycmb47069, i64 0)
%ae47612 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53718 = alloca %struct.ScmObj*, align 8
%fptrToInt53719 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47613 to i64
%ae47613 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53719)
store volatile %struct.ScmObj* %ae47613, %struct.ScmObj** %stackaddr$makeclosure53718, align 8
%argslist53655$ae476110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53720 = alloca %struct.ScmObj*, align 8
%argslist53655$ae476111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47613, %struct.ScmObj* %argslist53655$ae476110)
store volatile %struct.ScmObj* %argslist53655$ae476111, %struct.ScmObj** %stackaddr$prim53720, align 8
%stackaddr$prim53721 = alloca %struct.ScmObj*, align 8
%argslist53655$ae476112 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47612, %struct.ScmObj* %argslist53655$ae476111)
store volatile %struct.ScmObj* %argslist53655$ae476112, %struct.ScmObj** %stackaddr$prim53721, align 8
%clofunc53722 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47611)
musttail call tailcc void %clofunc53722(%struct.ScmObj* %ae47611, %struct.ScmObj* %argslist53655$ae476112)
ret void
}

define tailcc void @proc_clo$ae47611(%struct.ScmObj* %env$ae47611,%struct.ScmObj* %current_45args53142) {
%stackaddr$env-ref53723 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47611, i64 0)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref53723
%stackaddr$prim53724 = alloca %struct.ScmObj*, align 8
%_95k47314 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53142)
store volatile %struct.ScmObj* %_95k47314, %struct.ScmObj** %stackaddr$prim53724, align 8
%stackaddr$prim53725 = alloca %struct.ScmObj*, align 8
%current_45args53143 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53142)
store volatile %struct.ScmObj* %current_45args53143, %struct.ScmObj** %stackaddr$prim53725, align 8
%stackaddr$prim53726 = alloca %struct.ScmObj*, align 8
%anf_45bind47201 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53143)
store volatile %struct.ScmObj* %anf_45bind47201, %struct.ScmObj** %stackaddr$prim53726, align 8
%stackaddr$makeclosure53727 = alloca %struct.ScmObj*, align 8
%fptrToInt53728 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47689 to i64
%ae47689 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53728)
store volatile %struct.ScmObj* %ae47689, %struct.ScmObj** %stackaddr$makeclosure53727, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47689, %struct.ScmObj* %Ycmb47069, i64 0)
%argslist53639$Ycmb470690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53729 = alloca %struct.ScmObj*, align 8
%argslist53639$Ycmb470691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47201, %struct.ScmObj* %argslist53639$Ycmb470690)
store volatile %struct.ScmObj* %argslist53639$Ycmb470691, %struct.ScmObj** %stackaddr$prim53729, align 8
%stackaddr$prim53730 = alloca %struct.ScmObj*, align 8
%argslist53639$Ycmb470692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47689, %struct.ScmObj* %argslist53639$Ycmb470691)
store volatile %struct.ScmObj* %argslist53639$Ycmb470692, %struct.ScmObj** %stackaddr$prim53730, align 8
%clofunc53731 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47069)
musttail call tailcc void %clofunc53731(%struct.ScmObj* %Ycmb47069, %struct.ScmObj* %argslist53639$Ycmb470692)
ret void
}

define tailcc void @proc_clo$ae47689(%struct.ScmObj* %env$ae47689,%struct.ScmObj* %current_45args53145) {
%stackaddr$env-ref53732 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47689, i64 0)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref53732
%stackaddr$prim53733 = alloca %struct.ScmObj*, align 8
%_95k47315 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53145)
store volatile %struct.ScmObj* %_95k47315, %struct.ScmObj** %stackaddr$prim53733, align 8
%stackaddr$prim53734 = alloca %struct.ScmObj*, align 8
%current_45args53146 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53145)
store volatile %struct.ScmObj* %current_45args53146, %struct.ScmObj** %stackaddr$prim53734, align 8
%stackaddr$prim53735 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53146)
store volatile %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$prim53735, align 8
%stackaddr$makeclosure53736 = alloca %struct.ScmObj*, align 8
%fptrToInt53737 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47691 to i64
%ae47691 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt53737)
store volatile %struct.ScmObj* %ae47691, %struct.ScmObj** %stackaddr$makeclosure53736, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47691, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47691, %struct.ScmObj* %Ycmb47069, i64 1)
%ae47692 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53738 = alloca %struct.ScmObj*, align 8
%fptrToInt53739 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47693 to i64
%ae47693 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53739)
store volatile %struct.ScmObj* %ae47693, %struct.ScmObj** %stackaddr$makeclosure53738, align 8
%argslist53638$ae476910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53740 = alloca %struct.ScmObj*, align 8
%argslist53638$ae476911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47693, %struct.ScmObj* %argslist53638$ae476910)
store volatile %struct.ScmObj* %argslist53638$ae476911, %struct.ScmObj** %stackaddr$prim53740, align 8
%stackaddr$prim53741 = alloca %struct.ScmObj*, align 8
%argslist53638$ae476912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47692, %struct.ScmObj* %argslist53638$ae476911)
store volatile %struct.ScmObj* %argslist53638$ae476912, %struct.ScmObj** %stackaddr$prim53741, align 8
%clofunc53742 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47691)
musttail call tailcc void %clofunc53742(%struct.ScmObj* %ae47691, %struct.ScmObj* %argslist53638$ae476912)
ret void
}

define tailcc void @proc_clo$ae47691(%struct.ScmObj* %env$ae47691,%struct.ScmObj* %current_45args53148) {
%stackaddr$env-ref53743 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47691, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref53743
%stackaddr$env-ref53744 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47691, i64 1)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref53744
%stackaddr$prim53745 = alloca %struct.ScmObj*, align 8
%_95k47316 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53148)
store volatile %struct.ScmObj* %_95k47316, %struct.ScmObj** %stackaddr$prim53745, align 8
%stackaddr$prim53746 = alloca %struct.ScmObj*, align 8
%current_45args53149 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53148)
store volatile %struct.ScmObj* %current_45args53149, %struct.ScmObj** %stackaddr$prim53746, align 8
%stackaddr$prim53747 = alloca %struct.ScmObj*, align 8
%anf_45bind47207 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53149)
store volatile %struct.ScmObj* %anf_45bind47207, %struct.ScmObj** %stackaddr$prim53747, align 8
%stackaddr$makeclosure53748 = alloca %struct.ScmObj*, align 8
%fptrToInt53749 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47786 to i64
%ae47786 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt53749)
store volatile %struct.ScmObj* %ae47786, %struct.ScmObj** %stackaddr$makeclosure53748, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47786, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47786, %struct.ScmObj* %Ycmb47069, i64 1)
%argslist53619$Ycmb470690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53750 = alloca %struct.ScmObj*, align 8
%argslist53619$Ycmb470691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47207, %struct.ScmObj* %argslist53619$Ycmb470690)
store volatile %struct.ScmObj* %argslist53619$Ycmb470691, %struct.ScmObj** %stackaddr$prim53750, align 8
%stackaddr$prim53751 = alloca %struct.ScmObj*, align 8
%argslist53619$Ycmb470692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47786, %struct.ScmObj* %argslist53619$Ycmb470691)
store volatile %struct.ScmObj* %argslist53619$Ycmb470692, %struct.ScmObj** %stackaddr$prim53751, align 8
%clofunc53752 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47069)
musttail call tailcc void %clofunc53752(%struct.ScmObj* %Ycmb47069, %struct.ScmObj* %argslist53619$Ycmb470692)
ret void
}

define tailcc void @proc_clo$ae47786(%struct.ScmObj* %env$ae47786,%struct.ScmObj* %current_45args53151) {
%stackaddr$env-ref53753 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47786, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref53753
%stackaddr$env-ref53754 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47786, i64 1)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref53754
%stackaddr$prim53755 = alloca %struct.ScmObj*, align 8
%_95k47317 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53151)
store volatile %struct.ScmObj* %_95k47317, %struct.ScmObj** %stackaddr$prim53755, align 8
%stackaddr$prim53756 = alloca %struct.ScmObj*, align 8
%current_45args53152 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53151)
store volatile %struct.ScmObj* %current_45args53152, %struct.ScmObj** %stackaddr$prim53756, align 8
%stackaddr$prim53757 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53152)
store volatile %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$prim53757, align 8
%stackaddr$makeclosure53758 = alloca %struct.ScmObj*, align 8
%fptrToInt53759 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47788 to i64
%ae47788 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt53759)
store volatile %struct.ScmObj* %ae47788, %struct.ScmObj** %stackaddr$makeclosure53758, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47788, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47788, %struct.ScmObj* %_37map147086, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47788, %struct.ScmObj* %Ycmb47069, i64 2)
%ae47789 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53760 = alloca %struct.ScmObj*, align 8
%fptrToInt53761 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47790 to i64
%ae47790 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53761)
store volatile %struct.ScmObj* %ae47790, %struct.ScmObj** %stackaddr$makeclosure53760, align 8
%argslist53618$ae477880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53762 = alloca %struct.ScmObj*, align 8
%argslist53618$ae477881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47790, %struct.ScmObj* %argslist53618$ae477880)
store volatile %struct.ScmObj* %argslist53618$ae477881, %struct.ScmObj** %stackaddr$prim53762, align 8
%stackaddr$prim53763 = alloca %struct.ScmObj*, align 8
%argslist53618$ae477882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47789, %struct.ScmObj* %argslist53618$ae477881)
store volatile %struct.ScmObj* %argslist53618$ae477882, %struct.ScmObj** %stackaddr$prim53763, align 8
%clofunc53764 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47788)
musttail call tailcc void %clofunc53764(%struct.ScmObj* %ae47788, %struct.ScmObj* %argslist53618$ae477882)
ret void
}

define tailcc void @proc_clo$ae47788(%struct.ScmObj* %env$ae47788,%struct.ScmObj* %current_45args53154) {
%stackaddr$env-ref53765 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47788, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref53765
%stackaddr$env-ref53766 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47788, i64 1)
store %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$env-ref53766
%stackaddr$env-ref53767 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47788, i64 2)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref53767
%stackaddr$prim53768 = alloca %struct.ScmObj*, align 8
%_95k47318 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53154)
store volatile %struct.ScmObj* %_95k47318, %struct.ScmObj** %stackaddr$prim53768, align 8
%stackaddr$prim53769 = alloca %struct.ScmObj*, align 8
%current_45args53155 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53154)
store volatile %struct.ScmObj* %current_45args53155, %struct.ScmObj** %stackaddr$prim53769, align 8
%stackaddr$prim53770 = alloca %struct.ScmObj*, align 8
%anf_45bind47214 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53155)
store volatile %struct.ScmObj* %anf_45bind47214, %struct.ScmObj** %stackaddr$prim53770, align 8
%stackaddr$makeclosure53771 = alloca %struct.ScmObj*, align 8
%fptrToInt53772 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47936 to i64
%ae47936 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt53772)
store volatile %struct.ScmObj* %ae47936, %struct.ScmObj** %stackaddr$makeclosure53771, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47936, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47936, %struct.ScmObj* %_37map147086, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47936, %struct.ScmObj* %Ycmb47069, i64 2)
%argslist53602$Ycmb470690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53773 = alloca %struct.ScmObj*, align 8
%argslist53602$Ycmb470691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47214, %struct.ScmObj* %argslist53602$Ycmb470690)
store volatile %struct.ScmObj* %argslist53602$Ycmb470691, %struct.ScmObj** %stackaddr$prim53773, align 8
%stackaddr$prim53774 = alloca %struct.ScmObj*, align 8
%argslist53602$Ycmb470692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47936, %struct.ScmObj* %argslist53602$Ycmb470691)
store volatile %struct.ScmObj* %argslist53602$Ycmb470692, %struct.ScmObj** %stackaddr$prim53774, align 8
%clofunc53775 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47069)
musttail call tailcc void %clofunc53775(%struct.ScmObj* %Ycmb47069, %struct.ScmObj* %argslist53602$Ycmb470692)
ret void
}

define tailcc void @proc_clo$ae47936(%struct.ScmObj* %env$ae47936,%struct.ScmObj* %current_45args53157) {
%stackaddr$env-ref53776 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47936, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref53776
%stackaddr$env-ref53777 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47936, i64 1)
store %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$env-ref53777
%stackaddr$env-ref53778 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47936, i64 2)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref53778
%stackaddr$prim53779 = alloca %struct.ScmObj*, align 8
%_95k47319 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53157)
store volatile %struct.ScmObj* %_95k47319, %struct.ScmObj** %stackaddr$prim53779, align 8
%stackaddr$prim53780 = alloca %struct.ScmObj*, align 8
%current_45args53158 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53157)
store volatile %struct.ScmObj* %current_45args53158, %struct.ScmObj** %stackaddr$prim53780, align 8
%stackaddr$prim53781 = alloca %struct.ScmObj*, align 8
%_37take47082 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53158)
store volatile %struct.ScmObj* %_37take47082, %struct.ScmObj** %stackaddr$prim53781, align 8
%stackaddr$makeclosure53782 = alloca %struct.ScmObj*, align 8
%fptrToInt53783 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47938 to i64
%ae47938 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt53783)
store volatile %struct.ScmObj* %ae47938, %struct.ScmObj** %stackaddr$makeclosure53782, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47938, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47938, %struct.ScmObj* %_37map147086, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47938, %struct.ScmObj* %Ycmb47069, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae47938, %struct.ScmObj* %_37take47082, i64 3)
%ae47939 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53784 = alloca %struct.ScmObj*, align 8
%fptrToInt53785 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47940 to i64
%ae47940 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53785)
store volatile %struct.ScmObj* %ae47940, %struct.ScmObj** %stackaddr$makeclosure53784, align 8
%argslist53601$ae479380 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53786 = alloca %struct.ScmObj*, align 8
%argslist53601$ae479381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47940, %struct.ScmObj* %argslist53601$ae479380)
store volatile %struct.ScmObj* %argslist53601$ae479381, %struct.ScmObj** %stackaddr$prim53786, align 8
%stackaddr$prim53787 = alloca %struct.ScmObj*, align 8
%argslist53601$ae479382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47939, %struct.ScmObj* %argslist53601$ae479381)
store volatile %struct.ScmObj* %argslist53601$ae479382, %struct.ScmObj** %stackaddr$prim53787, align 8
%clofunc53788 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47938)
musttail call tailcc void %clofunc53788(%struct.ScmObj* %ae47938, %struct.ScmObj* %argslist53601$ae479382)
ret void
}

define tailcc void @proc_clo$ae47938(%struct.ScmObj* %env$ae47938,%struct.ScmObj* %current_45args53160) {
%stackaddr$env-ref53789 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47938, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref53789
%stackaddr$env-ref53790 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47938, i64 1)
store %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$env-ref53790
%stackaddr$env-ref53791 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47938, i64 2)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref53791
%stackaddr$env-ref53792 = alloca %struct.ScmObj*, align 8
%_37take47082 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47938, i64 3)
store %struct.ScmObj* %_37take47082, %struct.ScmObj** %stackaddr$env-ref53792
%stackaddr$prim53793 = alloca %struct.ScmObj*, align 8
%_95k47320 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53160)
store volatile %struct.ScmObj* %_95k47320, %struct.ScmObj** %stackaddr$prim53793, align 8
%stackaddr$prim53794 = alloca %struct.ScmObj*, align 8
%current_45args53161 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53160)
store volatile %struct.ScmObj* %current_45args53161, %struct.ScmObj** %stackaddr$prim53794, align 8
%stackaddr$prim53795 = alloca %struct.ScmObj*, align 8
%anf_45bind47218 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53161)
store volatile %struct.ScmObj* %anf_45bind47218, %struct.ScmObj** %stackaddr$prim53795, align 8
%stackaddr$makeclosure53796 = alloca %struct.ScmObj*, align 8
%fptrToInt53797 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48019 to i64
%ae48019 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt53797)
store volatile %struct.ScmObj* %ae48019, %struct.ScmObj** %stackaddr$makeclosure53796, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48019, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48019, %struct.ScmObj* %_37map147086, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48019, %struct.ScmObj* %Ycmb47069, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48019, %struct.ScmObj* %_37take47082, i64 3)
%argslist53587$Ycmb470690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53798 = alloca %struct.ScmObj*, align 8
%argslist53587$Ycmb470691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47218, %struct.ScmObj* %argslist53587$Ycmb470690)
store volatile %struct.ScmObj* %argslist53587$Ycmb470691, %struct.ScmObj** %stackaddr$prim53798, align 8
%stackaddr$prim53799 = alloca %struct.ScmObj*, align 8
%argslist53587$Ycmb470692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48019, %struct.ScmObj* %argslist53587$Ycmb470691)
store volatile %struct.ScmObj* %argslist53587$Ycmb470692, %struct.ScmObj** %stackaddr$prim53799, align 8
%clofunc53800 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47069)
musttail call tailcc void %clofunc53800(%struct.ScmObj* %Ycmb47069, %struct.ScmObj* %argslist53587$Ycmb470692)
ret void
}

define tailcc void @proc_clo$ae48019(%struct.ScmObj* %env$ae48019,%struct.ScmObj* %current_45args53163) {
%stackaddr$env-ref53801 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48019, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref53801
%stackaddr$env-ref53802 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48019, i64 1)
store %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$env-ref53802
%stackaddr$env-ref53803 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48019, i64 2)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref53803
%stackaddr$env-ref53804 = alloca %struct.ScmObj*, align 8
%_37take47082 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48019, i64 3)
store %struct.ScmObj* %_37take47082, %struct.ScmObj** %stackaddr$env-ref53804
%stackaddr$prim53805 = alloca %struct.ScmObj*, align 8
%_95k47321 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53163)
store volatile %struct.ScmObj* %_95k47321, %struct.ScmObj** %stackaddr$prim53805, align 8
%stackaddr$prim53806 = alloca %struct.ScmObj*, align 8
%current_45args53164 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53163)
store volatile %struct.ScmObj* %current_45args53164, %struct.ScmObj** %stackaddr$prim53806, align 8
%stackaddr$prim53807 = alloca %struct.ScmObj*, align 8
%_37length47079 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53164)
store volatile %struct.ScmObj* %_37length47079, %struct.ScmObj** %stackaddr$prim53807, align 8
%stackaddr$makeclosure53808 = alloca %struct.ScmObj*, align 8
%fptrToInt53809 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48021 to i64
%ae48021 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt53809)
store volatile %struct.ScmObj* %ae48021, %struct.ScmObj** %stackaddr$makeclosure53808, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48021, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48021, %struct.ScmObj* %_37map147086, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48021, %struct.ScmObj* %Ycmb47069, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48021, %struct.ScmObj* %_37take47082, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48021, %struct.ScmObj* %_37length47079, i64 4)
%ae48022 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53810 = alloca %struct.ScmObj*, align 8
%fptrToInt53811 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48023 to i64
%ae48023 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53811)
store volatile %struct.ScmObj* %ae48023, %struct.ScmObj** %stackaddr$makeclosure53810, align 8
%argslist53586$ae480210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53812 = alloca %struct.ScmObj*, align 8
%argslist53586$ae480211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48023, %struct.ScmObj* %argslist53586$ae480210)
store volatile %struct.ScmObj* %argslist53586$ae480211, %struct.ScmObj** %stackaddr$prim53812, align 8
%stackaddr$prim53813 = alloca %struct.ScmObj*, align 8
%argslist53586$ae480212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48022, %struct.ScmObj* %argslist53586$ae480211)
store volatile %struct.ScmObj* %argslist53586$ae480212, %struct.ScmObj** %stackaddr$prim53813, align 8
%clofunc53814 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48021)
musttail call tailcc void %clofunc53814(%struct.ScmObj* %ae48021, %struct.ScmObj* %argslist53586$ae480212)
ret void
}

define tailcc void @proc_clo$ae48021(%struct.ScmObj* %env$ae48021,%struct.ScmObj* %current_45args53166) {
%stackaddr$env-ref53815 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48021, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref53815
%stackaddr$env-ref53816 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48021, i64 1)
store %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$env-ref53816
%stackaddr$env-ref53817 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48021, i64 2)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref53817
%stackaddr$env-ref53818 = alloca %struct.ScmObj*, align 8
%_37take47082 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48021, i64 3)
store %struct.ScmObj* %_37take47082, %struct.ScmObj** %stackaddr$env-ref53818
%stackaddr$env-ref53819 = alloca %struct.ScmObj*, align 8
%_37length47079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48021, i64 4)
store %struct.ScmObj* %_37length47079, %struct.ScmObj** %stackaddr$env-ref53819
%stackaddr$prim53820 = alloca %struct.ScmObj*, align 8
%_95k47322 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53166)
store volatile %struct.ScmObj* %_95k47322, %struct.ScmObj** %stackaddr$prim53820, align 8
%stackaddr$prim53821 = alloca %struct.ScmObj*, align 8
%current_45args53167 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53166)
store volatile %struct.ScmObj* %current_45args53167, %struct.ScmObj** %stackaddr$prim53821, align 8
%stackaddr$prim53822 = alloca %struct.ScmObj*, align 8
%anf_45bind47223 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53167)
store volatile %struct.ScmObj* %anf_45bind47223, %struct.ScmObj** %stackaddr$prim53822, align 8
%stackaddr$makeclosure53823 = alloca %struct.ScmObj*, align 8
%fptrToInt53824 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48098 to i64
%ae48098 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt53824)
store volatile %struct.ScmObj* %ae48098, %struct.ScmObj** %stackaddr$makeclosure53823, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48098, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48098, %struct.ScmObj* %_37map147086, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48098, %struct.ScmObj* %Ycmb47069, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48098, %struct.ScmObj* %_37take47082, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48098, %struct.ScmObj* %_37length47079, i64 4)
%argslist53570$Ycmb470690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53825 = alloca %struct.ScmObj*, align 8
%argslist53570$Ycmb470691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47223, %struct.ScmObj* %argslist53570$Ycmb470690)
store volatile %struct.ScmObj* %argslist53570$Ycmb470691, %struct.ScmObj** %stackaddr$prim53825, align 8
%stackaddr$prim53826 = alloca %struct.ScmObj*, align 8
%argslist53570$Ycmb470692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48098, %struct.ScmObj* %argslist53570$Ycmb470691)
store volatile %struct.ScmObj* %argslist53570$Ycmb470692, %struct.ScmObj** %stackaddr$prim53826, align 8
%clofunc53827 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47069)
musttail call tailcc void %clofunc53827(%struct.ScmObj* %Ycmb47069, %struct.ScmObj* %argslist53570$Ycmb470692)
ret void
}

define tailcc void @proc_clo$ae48098(%struct.ScmObj* %env$ae48098,%struct.ScmObj* %current_45args53169) {
%stackaddr$env-ref53828 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48098, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref53828
%stackaddr$env-ref53829 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48098, i64 1)
store %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$env-ref53829
%stackaddr$env-ref53830 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48098, i64 2)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref53830
%stackaddr$env-ref53831 = alloca %struct.ScmObj*, align 8
%_37take47082 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48098, i64 3)
store %struct.ScmObj* %_37take47082, %struct.ScmObj** %stackaddr$env-ref53831
%stackaddr$env-ref53832 = alloca %struct.ScmObj*, align 8
%_37length47079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48098, i64 4)
store %struct.ScmObj* %_37length47079, %struct.ScmObj** %stackaddr$env-ref53832
%stackaddr$prim53833 = alloca %struct.ScmObj*, align 8
%_95k47323 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53169)
store volatile %struct.ScmObj* %_95k47323, %struct.ScmObj** %stackaddr$prim53833, align 8
%stackaddr$prim53834 = alloca %struct.ScmObj*, align 8
%current_45args53170 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53169)
store volatile %struct.ScmObj* %current_45args53170, %struct.ScmObj** %stackaddr$prim53834, align 8
%stackaddr$prim53835 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53170)
store volatile %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$prim53835, align 8
%stackaddr$makeclosure53836 = alloca %struct.ScmObj*, align 8
%fptrToInt53837 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48100 to i64
%ae48100 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt53837)
store volatile %struct.ScmObj* %ae48100, %struct.ScmObj** %stackaddr$makeclosure53836, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48100, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48100, %struct.ScmObj* %_37foldl147074, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48100, %struct.ScmObj* %_37map147086, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48100, %struct.ScmObj* %Ycmb47069, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48100, %struct.ScmObj* %_37take47082, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48100, %struct.ScmObj* %_37length47079, i64 5)
%ae48101 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53838 = alloca %struct.ScmObj*, align 8
%fptrToInt53839 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48102 to i64
%ae48102 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53839)
store volatile %struct.ScmObj* %ae48102, %struct.ScmObj** %stackaddr$makeclosure53838, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48102, %struct.ScmObj* %_37foldl147074, i64 0)
%argslist53569$ae481000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53840 = alloca %struct.ScmObj*, align 8
%argslist53569$ae481001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48102, %struct.ScmObj* %argslist53569$ae481000)
store volatile %struct.ScmObj* %argslist53569$ae481001, %struct.ScmObj** %stackaddr$prim53840, align 8
%stackaddr$prim53841 = alloca %struct.ScmObj*, align 8
%argslist53569$ae481002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48101, %struct.ScmObj* %argslist53569$ae481001)
store volatile %struct.ScmObj* %argslist53569$ae481002, %struct.ScmObj** %stackaddr$prim53841, align 8
%clofunc53842 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48100)
musttail call tailcc void %clofunc53842(%struct.ScmObj* %ae48100, %struct.ScmObj* %argslist53569$ae481002)
ret void
}

define tailcc void @proc_clo$ae48100(%struct.ScmObj* %env$ae48100,%struct.ScmObj* %current_45args53172) {
%stackaddr$env-ref53843 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48100, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref53843
%stackaddr$env-ref53844 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48100, i64 1)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref53844
%stackaddr$env-ref53845 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48100, i64 2)
store %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$env-ref53845
%stackaddr$env-ref53846 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48100, i64 3)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref53846
%stackaddr$env-ref53847 = alloca %struct.ScmObj*, align 8
%_37take47082 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48100, i64 4)
store %struct.ScmObj* %_37take47082, %struct.ScmObj** %stackaddr$env-ref53847
%stackaddr$env-ref53848 = alloca %struct.ScmObj*, align 8
%_37length47079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48100, i64 5)
store %struct.ScmObj* %_37length47079, %struct.ScmObj** %stackaddr$env-ref53848
%stackaddr$prim53849 = alloca %struct.ScmObj*, align 8
%_95k47324 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53172)
store volatile %struct.ScmObj* %_95k47324, %struct.ScmObj** %stackaddr$prim53849, align 8
%stackaddr$prim53850 = alloca %struct.ScmObj*, align 8
%current_45args53173 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53172)
store volatile %struct.ScmObj* %current_45args53173, %struct.ScmObj** %stackaddr$prim53850, align 8
%stackaddr$prim53851 = alloca %struct.ScmObj*, align 8
%_37last47112 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53173)
store volatile %struct.ScmObj* %_37last47112, %struct.ScmObj** %stackaddr$prim53851, align 8
%stackaddr$makeclosure53852 = alloca %struct.ScmObj*, align 8
%fptrToInt53853 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48154 to i64
%ae48154 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt53853)
store volatile %struct.ScmObj* %ae48154, %struct.ScmObj** %stackaddr$makeclosure53852, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48154, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48154, %struct.ScmObj* %_37foldl147074, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48154, %struct.ScmObj* %_37map147086, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48154, %struct.ScmObj* %Ycmb47069, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48154, %struct.ScmObj* %_37last47112, i64 4)
%ae48155 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53854 = alloca %struct.ScmObj*, align 8
%fptrToInt53855 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48156 to i64
%ae48156 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt53855)
store volatile %struct.ScmObj* %ae48156, %struct.ScmObj** %stackaddr$makeclosure53854, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48156, %struct.ScmObj* %_37take47082, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48156, %struct.ScmObj* %_37length47079, i64 1)
%argslist53555$ae481540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53856 = alloca %struct.ScmObj*, align 8
%argslist53555$ae481541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48156, %struct.ScmObj* %argslist53555$ae481540)
store volatile %struct.ScmObj* %argslist53555$ae481541, %struct.ScmObj** %stackaddr$prim53856, align 8
%stackaddr$prim53857 = alloca %struct.ScmObj*, align 8
%argslist53555$ae481542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48155, %struct.ScmObj* %argslist53555$ae481541)
store volatile %struct.ScmObj* %argslist53555$ae481542, %struct.ScmObj** %stackaddr$prim53857, align 8
%clofunc53858 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48154)
musttail call tailcc void %clofunc53858(%struct.ScmObj* %ae48154, %struct.ScmObj* %argslist53555$ae481542)
ret void
}

define tailcc void @proc_clo$ae48154(%struct.ScmObj* %env$ae48154,%struct.ScmObj* %current_45args53175) {
%stackaddr$env-ref53859 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48154, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref53859
%stackaddr$env-ref53860 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48154, i64 1)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref53860
%stackaddr$env-ref53861 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48154, i64 2)
store %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$env-ref53861
%stackaddr$env-ref53862 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48154, i64 3)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref53862
%stackaddr$env-ref53863 = alloca %struct.ScmObj*, align 8
%_37last47112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48154, i64 4)
store %struct.ScmObj* %_37last47112, %struct.ScmObj** %stackaddr$env-ref53863
%stackaddr$prim53864 = alloca %struct.ScmObj*, align 8
%_95k47325 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53175)
store volatile %struct.ScmObj* %_95k47325, %struct.ScmObj** %stackaddr$prim53864, align 8
%stackaddr$prim53865 = alloca %struct.ScmObj*, align 8
%current_45args53176 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53175)
store volatile %struct.ScmObj* %current_45args53176, %struct.ScmObj** %stackaddr$prim53865, align 8
%stackaddr$prim53866 = alloca %struct.ScmObj*, align 8
%_37drop_45right47109 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53176)
store volatile %struct.ScmObj* %_37drop_45right47109, %struct.ScmObj** %stackaddr$prim53866, align 8
%stackaddr$makeclosure53867 = alloca %struct.ScmObj*, align 8
%fptrToInt53868 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48184 to i64
%ae48184 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt53868)
store volatile %struct.ScmObj* %ae48184, %struct.ScmObj** %stackaddr$makeclosure53867, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48184, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48184, %struct.ScmObj* %_37foldl147074, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48184, %struct.ScmObj* %_37drop_45right47109, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48184, %struct.ScmObj* %Ycmb47069, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48184, %struct.ScmObj* %_37last47112, i64 4)
%ae48185 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53869 = alloca %struct.ScmObj*, align 8
%fptrToInt53870 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48186 to i64
%ae48186 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt53870)
store volatile %struct.ScmObj* %ae48186, %struct.ScmObj** %stackaddr$makeclosure53869, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48186, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48186, %struct.ScmObj* %_37map147086, i64 1)
%argslist53545$ae481840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53871 = alloca %struct.ScmObj*, align 8
%argslist53545$ae481841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48186, %struct.ScmObj* %argslist53545$ae481840)
store volatile %struct.ScmObj* %argslist53545$ae481841, %struct.ScmObj** %stackaddr$prim53871, align 8
%stackaddr$prim53872 = alloca %struct.ScmObj*, align 8
%argslist53545$ae481842 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48185, %struct.ScmObj* %argslist53545$ae481841)
store volatile %struct.ScmObj* %argslist53545$ae481842, %struct.ScmObj** %stackaddr$prim53872, align 8
%clofunc53873 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48184)
musttail call tailcc void %clofunc53873(%struct.ScmObj* %ae48184, %struct.ScmObj* %argslist53545$ae481842)
ret void
}

define tailcc void @proc_clo$ae48184(%struct.ScmObj* %env$ae48184,%struct.ScmObj* %current_45args53178) {
%stackaddr$env-ref53874 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48184, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref53874
%stackaddr$env-ref53875 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48184, i64 1)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref53875
%stackaddr$env-ref53876 = alloca %struct.ScmObj*, align 8
%_37drop_45right47109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48184, i64 2)
store %struct.ScmObj* %_37drop_45right47109, %struct.ScmObj** %stackaddr$env-ref53876
%stackaddr$env-ref53877 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48184, i64 3)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref53877
%stackaddr$env-ref53878 = alloca %struct.ScmObj*, align 8
%_37last47112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48184, i64 4)
store %struct.ScmObj* %_37last47112, %struct.ScmObj** %stackaddr$env-ref53878
%stackaddr$prim53879 = alloca %struct.ScmObj*, align 8
%_95k47326 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53178)
store volatile %struct.ScmObj* %_95k47326, %struct.ScmObj** %stackaddr$prim53879, align 8
%stackaddr$prim53880 = alloca %struct.ScmObj*, align 8
%current_45args53179 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53178)
store volatile %struct.ScmObj* %current_45args53179, %struct.ScmObj** %stackaddr$prim53880, align 8
%stackaddr$prim53881 = alloca %struct.ScmObj*, align 8
%anf_45bind47239 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53179)
store volatile %struct.ScmObj* %anf_45bind47239, %struct.ScmObj** %stackaddr$prim53881, align 8
%stackaddr$makeclosure53882 = alloca %struct.ScmObj*, align 8
%fptrToInt53883 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48568 to i64
%ae48568 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt53883)
store volatile %struct.ScmObj* %ae48568, %struct.ScmObj** %stackaddr$makeclosure53882, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48568, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48568, %struct.ScmObj* %_37foldl147074, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48568, %struct.ScmObj* %_37drop_45right47109, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48568, %struct.ScmObj* %Ycmb47069, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48568, %struct.ScmObj* %_37last47112, i64 4)
%argslist53485$Ycmb470690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53884 = alloca %struct.ScmObj*, align 8
%argslist53485$Ycmb470691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47239, %struct.ScmObj* %argslist53485$Ycmb470690)
store volatile %struct.ScmObj* %argslist53485$Ycmb470691, %struct.ScmObj** %stackaddr$prim53884, align 8
%stackaddr$prim53885 = alloca %struct.ScmObj*, align 8
%argslist53485$Ycmb470692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48568, %struct.ScmObj* %argslist53485$Ycmb470691)
store volatile %struct.ScmObj* %argslist53485$Ycmb470692, %struct.ScmObj** %stackaddr$prim53885, align 8
%clofunc53886 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47069)
musttail call tailcc void %clofunc53886(%struct.ScmObj* %Ycmb47069, %struct.ScmObj* %argslist53485$Ycmb470692)
ret void
}

define tailcc void @proc_clo$ae48568(%struct.ScmObj* %env$ae48568,%struct.ScmObj* %current_45args53181) {
%stackaddr$env-ref53887 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48568, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref53887
%stackaddr$env-ref53888 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48568, i64 1)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref53888
%stackaddr$env-ref53889 = alloca %struct.ScmObj*, align 8
%_37drop_45right47109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48568, i64 2)
store %struct.ScmObj* %_37drop_45right47109, %struct.ScmObj** %stackaddr$env-ref53889
%stackaddr$env-ref53890 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48568, i64 3)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref53890
%stackaddr$env-ref53891 = alloca %struct.ScmObj*, align 8
%_37last47112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48568, i64 4)
store %struct.ScmObj* %_37last47112, %struct.ScmObj** %stackaddr$env-ref53891
%stackaddr$prim53892 = alloca %struct.ScmObj*, align 8
%_95k47327 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53181)
store volatile %struct.ScmObj* %_95k47327, %struct.ScmObj** %stackaddr$prim53892, align 8
%stackaddr$prim53893 = alloca %struct.ScmObj*, align 8
%current_45args53182 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53181)
store volatile %struct.ScmObj* %current_45args53182, %struct.ScmObj** %stackaddr$prim53893, align 8
%stackaddr$prim53894 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53182)
store volatile %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$prim53894, align 8
%stackaddr$makeclosure53895 = alloca %struct.ScmObj*, align 8
%fptrToInt53896 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48570 to i64
%ae48570 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt53896)
store volatile %struct.ScmObj* %ae48570, %struct.ScmObj** %stackaddr$makeclosure53895, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48570, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48570, %struct.ScmObj* %_37foldl147074, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48570, %struct.ScmObj* %_37drop_45right47109, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48570, %struct.ScmObj* %Ycmb47069, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48570, %struct.ScmObj* %_37last47112, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48570, %struct.ScmObj* %_37foldr47095, i64 5)
%ae48571 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53897 = alloca %struct.ScmObj*, align 8
%fptrToInt53898 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48572 to i64
%ae48572 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53898)
store volatile %struct.ScmObj* %ae48572, %struct.ScmObj** %stackaddr$makeclosure53897, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48572, %struct.ScmObj* %_37foldr147090, i64 0)
%argslist53484$ae485700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53899 = alloca %struct.ScmObj*, align 8
%argslist53484$ae485701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48572, %struct.ScmObj* %argslist53484$ae485700)
store volatile %struct.ScmObj* %argslist53484$ae485701, %struct.ScmObj** %stackaddr$prim53899, align 8
%stackaddr$prim53900 = alloca %struct.ScmObj*, align 8
%argslist53484$ae485702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48571, %struct.ScmObj* %argslist53484$ae485701)
store volatile %struct.ScmObj* %argslist53484$ae485702, %struct.ScmObj** %stackaddr$prim53900, align 8
%clofunc53901 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48570)
musttail call tailcc void %clofunc53901(%struct.ScmObj* %ae48570, %struct.ScmObj* %argslist53484$ae485702)
ret void
}

define tailcc void @proc_clo$ae48570(%struct.ScmObj* %env$ae48570,%struct.ScmObj* %current_45args53184) {
%stackaddr$env-ref53902 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48570, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref53902
%stackaddr$env-ref53903 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48570, i64 1)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref53903
%stackaddr$env-ref53904 = alloca %struct.ScmObj*, align 8
%_37drop_45right47109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48570, i64 2)
store %struct.ScmObj* %_37drop_45right47109, %struct.ScmObj** %stackaddr$env-ref53904
%stackaddr$env-ref53905 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48570, i64 3)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref53905
%stackaddr$env-ref53906 = alloca %struct.ScmObj*, align 8
%_37last47112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48570, i64 4)
store %struct.ScmObj* %_37last47112, %struct.ScmObj** %stackaddr$env-ref53906
%stackaddr$env-ref53907 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48570, i64 5)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref53907
%stackaddr$prim53908 = alloca %struct.ScmObj*, align 8
%_95k47328 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53184)
store volatile %struct.ScmObj* %_95k47328, %struct.ScmObj** %stackaddr$prim53908, align 8
%stackaddr$prim53909 = alloca %struct.ScmObj*, align 8
%current_45args53185 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53184)
store volatile %struct.ScmObj* %current_45args53185, %struct.ScmObj** %stackaddr$prim53909, align 8
%stackaddr$prim53910 = alloca %struct.ScmObj*, align 8
%_37map147121 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53185)
store volatile %struct.ScmObj* %_37map147121, %struct.ScmObj** %stackaddr$prim53910, align 8
%stackaddr$makeclosure53911 = alloca %struct.ScmObj*, align 8
%fptrToInt53912 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48647 to i64
%ae48647 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt53912)
store volatile %struct.ScmObj* %ae48647, %struct.ScmObj** %stackaddr$makeclosure53911, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48647, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48647, %struct.ScmObj* %_37foldl147074, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48647, %struct.ScmObj* %_37map147121, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48647, %struct.ScmObj* %Ycmb47069, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48647, %struct.ScmObj* %_37foldr47095, i64 4)
%ae48648 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53913 = alloca %struct.ScmObj*, align 8
%fptrToInt53914 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48649 to i64
%ae48649 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt53914)
store volatile %struct.ScmObj* %ae48649, %struct.ScmObj** %stackaddr$makeclosure53913, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48649, %struct.ScmObj* %_37drop_45right47109, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48649, %struct.ScmObj* %_37last47112, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48649, %struct.ScmObj* %_37foldr47095, i64 2)
%argslist53465$ae486470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53915 = alloca %struct.ScmObj*, align 8
%argslist53465$ae486471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48649, %struct.ScmObj* %argslist53465$ae486470)
store volatile %struct.ScmObj* %argslist53465$ae486471, %struct.ScmObj** %stackaddr$prim53915, align 8
%stackaddr$prim53916 = alloca %struct.ScmObj*, align 8
%argslist53465$ae486472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48648, %struct.ScmObj* %argslist53465$ae486471)
store volatile %struct.ScmObj* %argslist53465$ae486472, %struct.ScmObj** %stackaddr$prim53916, align 8
%clofunc53917 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48647)
musttail call tailcc void %clofunc53917(%struct.ScmObj* %ae48647, %struct.ScmObj* %argslist53465$ae486472)
ret void
}

define tailcc void @proc_clo$ae48647(%struct.ScmObj* %env$ae48647,%struct.ScmObj* %current_45args53187) {
%stackaddr$env-ref53918 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48647, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref53918
%stackaddr$env-ref53919 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48647, i64 1)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref53919
%stackaddr$env-ref53920 = alloca %struct.ScmObj*, align 8
%_37map147121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48647, i64 2)
store %struct.ScmObj* %_37map147121, %struct.ScmObj** %stackaddr$env-ref53920
%stackaddr$env-ref53921 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48647, i64 3)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref53921
%stackaddr$env-ref53922 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48647, i64 4)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref53922
%stackaddr$prim53923 = alloca %struct.ScmObj*, align 8
%_95k47329 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53187)
store volatile %struct.ScmObj* %_95k47329, %struct.ScmObj** %stackaddr$prim53923, align 8
%stackaddr$prim53924 = alloca %struct.ScmObj*, align 8
%current_45args53188 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53187)
store volatile %struct.ScmObj* %current_45args53188, %struct.ScmObj** %stackaddr$prim53924, align 8
%stackaddr$prim53925 = alloca %struct.ScmObj*, align 8
%_37map47116 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53188)
store volatile %struct.ScmObj* %_37map47116, %struct.ScmObj** %stackaddr$prim53925, align 8
%stackaddr$makeclosure53926 = alloca %struct.ScmObj*, align 8
%fptrToInt53927 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48793 to i64
%ae48793 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt53927)
store volatile %struct.ScmObj* %ae48793, %struct.ScmObj** %stackaddr$makeclosure53926, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48793, %struct.ScmObj* %_37foldl147074, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48793, %struct.ScmObj* %Ycmb47069, i64 1)
%ae48794 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53928 = alloca %struct.ScmObj*, align 8
%fptrToInt53929 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48795 to i64
%ae48795 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt53929)
store volatile %struct.ScmObj* %ae48795, %struct.ScmObj** %stackaddr$makeclosure53928, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48795, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48795, %struct.ScmObj* %_37map147121, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48795, %struct.ScmObj* %_37foldr47095, i64 2)
%argslist53448$ae487930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53930 = alloca %struct.ScmObj*, align 8
%argslist53448$ae487931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48795, %struct.ScmObj* %argslist53448$ae487930)
store volatile %struct.ScmObj* %argslist53448$ae487931, %struct.ScmObj** %stackaddr$prim53930, align 8
%stackaddr$prim53931 = alloca %struct.ScmObj*, align 8
%argslist53448$ae487932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48794, %struct.ScmObj* %argslist53448$ae487931)
store volatile %struct.ScmObj* %argslist53448$ae487932, %struct.ScmObj** %stackaddr$prim53931, align 8
%clofunc53932 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48793)
musttail call tailcc void %clofunc53932(%struct.ScmObj* %ae48793, %struct.ScmObj* %argslist53448$ae487932)
ret void
}

define tailcc void @proc_clo$ae48793(%struct.ScmObj* %env$ae48793,%struct.ScmObj* %current_45args53190) {
%stackaddr$env-ref53933 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48793, i64 0)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref53933
%stackaddr$env-ref53934 = alloca %struct.ScmObj*, align 8
%Ycmb47069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48793, i64 1)
store %struct.ScmObj* %Ycmb47069, %struct.ScmObj** %stackaddr$env-ref53934
%stackaddr$prim53935 = alloca %struct.ScmObj*, align 8
%_95k47330 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53190)
store volatile %struct.ScmObj* %_95k47330, %struct.ScmObj** %stackaddr$prim53935, align 8
%stackaddr$prim53936 = alloca %struct.ScmObj*, align 8
%current_45args53191 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53190)
store volatile %struct.ScmObj* %current_45args53191, %struct.ScmObj** %stackaddr$prim53936, align 8
%stackaddr$prim53937 = alloca %struct.ScmObj*, align 8
%anf_45bind47259 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53191)
store volatile %struct.ScmObj* %anf_45bind47259, %struct.ScmObj** %stackaddr$prim53937, align 8
%stackaddr$makeclosure53938 = alloca %struct.ScmObj*, align 8
%fptrToInt53939 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49185 to i64
%ae49185 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53939)
store volatile %struct.ScmObj* %ae49185, %struct.ScmObj** %stackaddr$makeclosure53938, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49185, %struct.ScmObj* %_37foldl147074, i64 0)
%argslist53388$Ycmb470690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53940 = alloca %struct.ScmObj*, align 8
%argslist53388$Ycmb470691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47259, %struct.ScmObj* %argslist53388$Ycmb470690)
store volatile %struct.ScmObj* %argslist53388$Ycmb470691, %struct.ScmObj** %stackaddr$prim53940, align 8
%stackaddr$prim53941 = alloca %struct.ScmObj*, align 8
%argslist53388$Ycmb470692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49185, %struct.ScmObj* %argslist53388$Ycmb470691)
store volatile %struct.ScmObj* %argslist53388$Ycmb470692, %struct.ScmObj** %stackaddr$prim53941, align 8
%clofunc53942 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47069)
musttail call tailcc void %clofunc53942(%struct.ScmObj* %Ycmb47069, %struct.ScmObj* %argslist53388$Ycmb470692)
ret void
}

define tailcc void @proc_clo$ae49185(%struct.ScmObj* %env$ae49185,%struct.ScmObj* %current_45args53193) {
%stackaddr$env-ref53943 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49185, i64 0)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref53943
%stackaddr$prim53944 = alloca %struct.ScmObj*, align 8
%_95k47331 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53193)
store volatile %struct.ScmObj* %_95k47331, %struct.ScmObj** %stackaddr$prim53944, align 8
%stackaddr$prim53945 = alloca %struct.ScmObj*, align 8
%current_45args53194 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53193)
store volatile %struct.ScmObj* %current_45args53194, %struct.ScmObj** %stackaddr$prim53945, align 8
%stackaddr$prim53946 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53194)
store volatile %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$prim53946, align 8
%stackaddr$makeclosure53947 = alloca %struct.ScmObj*, align 8
%fptrToInt53948 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49187 to i64
%ae49187 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53948)
store volatile %struct.ScmObj* %ae49187, %struct.ScmObj** %stackaddr$makeclosure53947, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49187, %struct.ScmObj* %_37foldl147074, i64 0)
%ae49188 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53949 = alloca %struct.ScmObj*, align 8
%fptrToInt53950 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49189 to i64
%ae49189 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53950)
store volatile %struct.ScmObj* %ae49189, %struct.ScmObj** %stackaddr$makeclosure53949, align 8
%argslist53387$ae491870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53951 = alloca %struct.ScmObj*, align 8
%argslist53387$ae491871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49189, %struct.ScmObj* %argslist53387$ae491870)
store volatile %struct.ScmObj* %argslist53387$ae491871, %struct.ScmObj** %stackaddr$prim53951, align 8
%stackaddr$prim53952 = alloca %struct.ScmObj*, align 8
%argslist53387$ae491872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49188, %struct.ScmObj* %argslist53387$ae491871)
store volatile %struct.ScmObj* %argslist53387$ae491872, %struct.ScmObj** %stackaddr$prim53952, align 8
%clofunc53953 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49187)
musttail call tailcc void %clofunc53953(%struct.ScmObj* %ae49187, %struct.ScmObj* %argslist53387$ae491872)
ret void
}

define tailcc void @proc_clo$ae49187(%struct.ScmObj* %env$ae49187,%struct.ScmObj* %current_45args53196) {
%stackaddr$env-ref53954 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49187, i64 0)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref53954
%stackaddr$prim53955 = alloca %struct.ScmObj*, align 8
%_95k47332 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53196)
store volatile %struct.ScmObj* %_95k47332, %struct.ScmObj** %stackaddr$prim53955, align 8
%stackaddr$prim53956 = alloca %struct.ScmObj*, align 8
%current_45args53197 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53196)
store volatile %struct.ScmObj* %current_45args53197, %struct.ScmObj** %stackaddr$prim53956, align 8
%stackaddr$prim53957 = alloca %struct.ScmObj*, align 8
%_37_6247169 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53197)
store volatile %struct.ScmObj* %_37_6247169, %struct.ScmObj** %stackaddr$prim53957, align 8
%stackaddr$makeclosure53958 = alloca %struct.ScmObj*, align 8
%fptrToInt53959 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49211 to i64
%ae49211 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt53959)
store volatile %struct.ScmObj* %ae49211, %struct.ScmObj** %stackaddr$makeclosure53958, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49211, %struct.ScmObj* %_37foldl147074, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49211, %struct.ScmObj* %_37_6247169, i64 1)
%ae49212 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53960 = alloca %struct.ScmObj*, align 8
%fptrToInt53961 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49213 to i64
%ae49213 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53961)
store volatile %struct.ScmObj* %ae49213, %struct.ScmObj** %stackaddr$makeclosure53960, align 8
%argslist53381$ae492110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53962 = alloca %struct.ScmObj*, align 8
%argslist53381$ae492111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49213, %struct.ScmObj* %argslist53381$ae492110)
store volatile %struct.ScmObj* %argslist53381$ae492111, %struct.ScmObj** %stackaddr$prim53962, align 8
%stackaddr$prim53963 = alloca %struct.ScmObj*, align 8
%argslist53381$ae492112 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49212, %struct.ScmObj* %argslist53381$ae492111)
store volatile %struct.ScmObj* %argslist53381$ae492112, %struct.ScmObj** %stackaddr$prim53963, align 8
%clofunc53964 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49211)
musttail call tailcc void %clofunc53964(%struct.ScmObj* %ae49211, %struct.ScmObj* %argslist53381$ae492112)
ret void
}

define tailcc void @proc_clo$ae49211(%struct.ScmObj* %env$ae49211,%struct.ScmObj* %current_45args53199) {
%stackaddr$env-ref53965 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49211, i64 0)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref53965
%stackaddr$env-ref53966 = alloca %struct.ScmObj*, align 8
%_37_6247169 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49211, i64 1)
store %struct.ScmObj* %_37_6247169, %struct.ScmObj** %stackaddr$env-ref53966
%stackaddr$prim53967 = alloca %struct.ScmObj*, align 8
%_95k47333 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53199)
store volatile %struct.ScmObj* %_95k47333, %struct.ScmObj** %stackaddr$prim53967, align 8
%stackaddr$prim53968 = alloca %struct.ScmObj*, align 8
%current_45args53200 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53199)
store volatile %struct.ScmObj* %current_45args53200, %struct.ScmObj** %stackaddr$prim53968, align 8
%stackaddr$prim53969 = alloca %struct.ScmObj*, align 8
%_37_62_6147166 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53200)
store volatile %struct.ScmObj* %_37_62_6147166, %struct.ScmObj** %stackaddr$prim53969, align 8
%ae49235 = call %struct.ScmObj* @const_init_int(i64 1)
%ae49236 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53970 = alloca %struct.ScmObj*, align 8
%_37append47162 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49235, %struct.ScmObj* %ae49236)
store volatile %struct.ScmObj* %_37append47162, %struct.ScmObj** %stackaddr$prim53970, align 8
%stackaddr$makeclosure53971 = alloca %struct.ScmObj*, align 8
%fptrToInt53972 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49237 to i64
%ae49237 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt53972)
store volatile %struct.ScmObj* %ae49237, %struct.ScmObj** %stackaddr$makeclosure53971, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49237, %struct.ScmObj* %_37foldl147074, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49237, %struct.ScmObj* %_37_6247169, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49237, %struct.ScmObj* %_37append47162, i64 2)
%ae49238 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53973 = alloca %struct.ScmObj*, align 8
%fptrToInt53974 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49239 to i64
%ae49239 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53974)
store volatile %struct.ScmObj* %ae49239, %struct.ScmObj** %stackaddr$makeclosure53973, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49239, %struct.ScmObj* %_37append47162, i64 0)
%argslist53375$ae492370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53975 = alloca %struct.ScmObj*, align 8
%argslist53375$ae492371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49239, %struct.ScmObj* %argslist53375$ae492370)
store volatile %struct.ScmObj* %argslist53375$ae492371, %struct.ScmObj** %stackaddr$prim53975, align 8
%stackaddr$prim53976 = alloca %struct.ScmObj*, align 8
%argslist53375$ae492372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49238, %struct.ScmObj* %argslist53375$ae492371)
store volatile %struct.ScmObj* %argslist53375$ae492372, %struct.ScmObj** %stackaddr$prim53976, align 8
%clofunc53977 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49237)
musttail call tailcc void %clofunc53977(%struct.ScmObj* %ae49237, %struct.ScmObj* %argslist53375$ae492372)
ret void
}

define tailcc void @proc_clo$ae49237(%struct.ScmObj* %env$ae49237,%struct.ScmObj* %current_45args53202) {
%stackaddr$env-ref53978 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49237, i64 0)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref53978
%stackaddr$env-ref53979 = alloca %struct.ScmObj*, align 8
%_37_6247169 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49237, i64 1)
store %struct.ScmObj* %_37_6247169, %struct.ScmObj** %stackaddr$env-ref53979
%stackaddr$env-ref53980 = alloca %struct.ScmObj*, align 8
%_37append47162 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49237, i64 2)
store %struct.ScmObj* %_37append47162, %struct.ScmObj** %stackaddr$env-ref53980
%stackaddr$prim53981 = alloca %struct.ScmObj*, align 8
%_95k47334 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53202)
store volatile %struct.ScmObj* %_95k47334, %struct.ScmObj** %stackaddr$prim53981, align 8
%stackaddr$prim53982 = alloca %struct.ScmObj*, align 8
%current_45args53203 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53202)
store volatile %struct.ScmObj* %current_45args53203, %struct.ScmObj** %stackaddr$prim53982, align 8
%stackaddr$prim53983 = alloca %struct.ScmObj*, align 8
%anf_45bind47267 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53203)
store volatile %struct.ScmObj* %anf_45bind47267, %struct.ScmObj** %stackaddr$prim53983, align 8
%ae49305 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim53984 = alloca %struct.ScmObj*, align 8
%_95047163 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append47162, %struct.ScmObj* %ae49305, %struct.ScmObj* %anf_45bind47267)
store volatile %struct.ScmObj* %_95047163, %struct.ScmObj** %stackaddr$prim53984, align 8
%ae49308 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim53985 = alloca %struct.ScmObj*, align 8
%_37append47161 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append47162, %struct.ScmObj* %ae49308)
store volatile %struct.ScmObj* %_37append47161, %struct.ScmObj** %stackaddr$prim53985, align 8
%stackaddr$makeclosure53986 = alloca %struct.ScmObj*, align 8
%fptrToInt53987 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49309 to i64
%ae49309 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt53987)
store volatile %struct.ScmObj* %ae49309, %struct.ScmObj** %stackaddr$makeclosure53986, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49309, %struct.ScmObj* %_37foldl147074, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49309, %struct.ScmObj* %_37_6247169, i64 1)
%ae49310 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53988 = alloca %struct.ScmObj*, align 8
%fptrToInt53989 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49311 to i64
%ae49311 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53989)
store volatile %struct.ScmObj* %ae49311, %struct.ScmObj** %stackaddr$makeclosure53988, align 8
%argslist53364$ae493090 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53990 = alloca %struct.ScmObj*, align 8
%argslist53364$ae493091 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49311, %struct.ScmObj* %argslist53364$ae493090)
store volatile %struct.ScmObj* %argslist53364$ae493091, %struct.ScmObj** %stackaddr$prim53990, align 8
%stackaddr$prim53991 = alloca %struct.ScmObj*, align 8
%argslist53364$ae493092 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49310, %struct.ScmObj* %argslist53364$ae493091)
store volatile %struct.ScmObj* %argslist53364$ae493092, %struct.ScmObj** %stackaddr$prim53991, align 8
%clofunc53992 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49309)
musttail call tailcc void %clofunc53992(%struct.ScmObj* %ae49309, %struct.ScmObj* %argslist53364$ae493092)
ret void
}

define tailcc void @proc_clo$ae49309(%struct.ScmObj* %env$ae49309,%struct.ScmObj* %current_45args53205) {
%stackaddr$env-ref53993 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49309, i64 0)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref53993
%stackaddr$env-ref53994 = alloca %struct.ScmObj*, align 8
%_37_6247169 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49309, i64 1)
store %struct.ScmObj* %_37_6247169, %struct.ScmObj** %stackaddr$env-ref53994
%stackaddr$prim53995 = alloca %struct.ScmObj*, align 8
%_95k47335 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53205)
store volatile %struct.ScmObj* %_95k47335, %struct.ScmObj** %stackaddr$prim53995, align 8
%stackaddr$prim53996 = alloca %struct.ScmObj*, align 8
%current_45args53206 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53205)
store volatile %struct.ScmObj* %current_45args53206, %struct.ScmObj** %stackaddr$prim53996, align 8
%stackaddr$prim53997 = alloca %struct.ScmObj*, align 8
%_37list_6347154 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53206)
store volatile %struct.ScmObj* %_37list_6347154, %struct.ScmObj** %stackaddr$prim53997, align 8
%stackaddr$makeclosure53998 = alloca %struct.ScmObj*, align 8
%fptrToInt53999 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49725 to i64
%ae49725 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt53999)
store volatile %struct.ScmObj* %ae49725, %struct.ScmObj** %stackaddr$makeclosure53998, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49725, %struct.ScmObj* %_37foldl147074, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49725, %struct.ScmObj* %_37_6247169, i64 1)
%ae49726 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54000 = alloca %struct.ScmObj*, align 8
%fptrToInt54001 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49727 to i64
%ae49727 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54001)
store volatile %struct.ScmObj* %ae49727, %struct.ScmObj** %stackaddr$makeclosure54000, align 8
%argslist53339$ae497250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54002 = alloca %struct.ScmObj*, align 8
%argslist53339$ae497251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49727, %struct.ScmObj* %argslist53339$ae497250)
store volatile %struct.ScmObj* %argslist53339$ae497251, %struct.ScmObj** %stackaddr$prim54002, align 8
%stackaddr$prim54003 = alloca %struct.ScmObj*, align 8
%argslist53339$ae497252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49726, %struct.ScmObj* %argslist53339$ae497251)
store volatile %struct.ScmObj* %argslist53339$ae497252, %struct.ScmObj** %stackaddr$prim54003, align 8
%clofunc54004 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49725)
musttail call tailcc void %clofunc54004(%struct.ScmObj* %ae49725, %struct.ScmObj* %argslist53339$ae497252)
ret void
}

define tailcc void @proc_clo$ae49725(%struct.ScmObj* %env$ae49725,%struct.ScmObj* %current_45args53208) {
%stackaddr$env-ref54005 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49725, i64 0)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref54005
%stackaddr$env-ref54006 = alloca %struct.ScmObj*, align 8
%_37_6247169 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49725, i64 1)
store %struct.ScmObj* %_37_6247169, %struct.ScmObj** %stackaddr$env-ref54006
%stackaddr$prim54007 = alloca %struct.ScmObj*, align 8
%_95k47336 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53208)
store volatile %struct.ScmObj* %_95k47336, %struct.ScmObj** %stackaddr$prim54007, align 8
%stackaddr$prim54008 = alloca %struct.ScmObj*, align 8
%current_45args53209 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53208)
store volatile %struct.ScmObj* %current_45args53209, %struct.ScmObj** %stackaddr$prim54008, align 8
%stackaddr$prim54009 = alloca %struct.ScmObj*, align 8
%_37drop47145 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53209)
store volatile %struct.ScmObj* %_37drop47145, %struct.ScmObj** %stackaddr$prim54009, align 8
%stackaddr$makeclosure54010 = alloca %struct.ScmObj*, align 8
%fptrToInt54011 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50261 to i64
%ae50261 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54011)
store volatile %struct.ScmObj* %ae50261, %struct.ScmObj** %stackaddr$makeclosure54010, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50261, %struct.ScmObj* %_37foldl147074, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50261, %struct.ScmObj* %_37_6247169, i64 1)
%ae50262 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54012 = alloca %struct.ScmObj*, align 8
%fptrToInt54013 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50263 to i64
%ae50263 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54013)
store volatile %struct.ScmObj* %ae50263, %struct.ScmObj** %stackaddr$makeclosure54012, align 8
%argslist53315$ae502610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54014 = alloca %struct.ScmObj*, align 8
%argslist53315$ae502611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50263, %struct.ScmObj* %argslist53315$ae502610)
store volatile %struct.ScmObj* %argslist53315$ae502611, %struct.ScmObj** %stackaddr$prim54014, align 8
%stackaddr$prim54015 = alloca %struct.ScmObj*, align 8
%argslist53315$ae502612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50262, %struct.ScmObj* %argslist53315$ae502611)
store volatile %struct.ScmObj* %argslist53315$ae502612, %struct.ScmObj** %stackaddr$prim54015, align 8
%clofunc54016 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50261)
musttail call tailcc void %clofunc54016(%struct.ScmObj* %ae50261, %struct.ScmObj* %argslist53315$ae502612)
ret void
}

define tailcc void @proc_clo$ae50261(%struct.ScmObj* %env$ae50261,%struct.ScmObj* %current_45args53211) {
%stackaddr$env-ref54017 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50261, i64 0)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref54017
%stackaddr$env-ref54018 = alloca %struct.ScmObj*, align 8
%_37_6247169 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50261, i64 1)
store %struct.ScmObj* %_37_6247169, %struct.ScmObj** %stackaddr$env-ref54018
%stackaddr$prim54019 = alloca %struct.ScmObj*, align 8
%_95k47337 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53211)
store volatile %struct.ScmObj* %_95k47337, %struct.ScmObj** %stackaddr$prim54019, align 8
%stackaddr$prim54020 = alloca %struct.ScmObj*, align 8
%current_45args53212 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53211)
store volatile %struct.ScmObj* %current_45args53212, %struct.ScmObj** %stackaddr$prim54020, align 8
%stackaddr$prim54021 = alloca %struct.ScmObj*, align 8
%_37memv47138 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53212)
store volatile %struct.ScmObj* %_37memv47138, %struct.ScmObj** %stackaddr$prim54021, align 8
%stackaddr$makeclosure54022 = alloca %struct.ScmObj*, align 8
%fptrToInt54023 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50665 to i64
%ae50665 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54023)
store volatile %struct.ScmObj* %ae50665, %struct.ScmObj** %stackaddr$makeclosure54022, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50665, %struct.ScmObj* %_37_6247169, i64 0)
%ae50666 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54024 = alloca %struct.ScmObj*, align 8
%fptrToInt54025 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50667 to i64
%ae50667 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54025)
store volatile %struct.ScmObj* %ae50667, %struct.ScmObj** %stackaddr$makeclosure54024, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50667, %struct.ScmObj* %_37foldl147074, i64 0)
%argslist53289$ae506650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54026 = alloca %struct.ScmObj*, align 8
%argslist53289$ae506651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50667, %struct.ScmObj* %argslist53289$ae506650)
store volatile %struct.ScmObj* %argslist53289$ae506651, %struct.ScmObj** %stackaddr$prim54026, align 8
%stackaddr$prim54027 = alloca %struct.ScmObj*, align 8
%argslist53289$ae506652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50666, %struct.ScmObj* %argslist53289$ae506651)
store volatile %struct.ScmObj* %argslist53289$ae506652, %struct.ScmObj** %stackaddr$prim54027, align 8
%clofunc54028 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50665)
musttail call tailcc void %clofunc54028(%struct.ScmObj* %ae50665, %struct.ScmObj* %argslist53289$ae506652)
ret void
}

define tailcc void @proc_clo$ae50665(%struct.ScmObj* %env$ae50665,%struct.ScmObj* %current_45args53214) {
%stackaddr$env-ref54029 = alloca %struct.ScmObj*, align 8
%_37_6247169 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50665, i64 0)
store %struct.ScmObj* %_37_6247169, %struct.ScmObj** %stackaddr$env-ref54029
%stackaddr$prim54030 = alloca %struct.ScmObj*, align 8
%_95k47338 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53214)
store volatile %struct.ScmObj* %_95k47338, %struct.ScmObj** %stackaddr$prim54030, align 8
%stackaddr$prim54031 = alloca %struct.ScmObj*, align 8
%current_45args53215 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53214)
store volatile %struct.ScmObj* %current_45args53215, %struct.ScmObj** %stackaddr$prim54031, align 8
%stackaddr$prim54032 = alloca %struct.ScmObj*, align 8
%_37_4747134 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53215)
store volatile %struct.ScmObj* %_37_4747134, %struct.ScmObj** %stackaddr$prim54032, align 8
%stackaddr$makeclosure54033 = alloca %struct.ScmObj*, align 8
%fptrToInt54034 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50763 to i64
%ae50763 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54034)
store volatile %struct.ScmObj* %ae50763, %struct.ScmObj** %stackaddr$makeclosure54033, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50763, %struct.ScmObj* %_37_6247169, i64 0)
%ae50764 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54035 = alloca %struct.ScmObj*, align 8
%fptrToInt54036 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50765 to i64
%ae50765 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54036)
store volatile %struct.ScmObj* %ae50765, %struct.ScmObj** %stackaddr$makeclosure54035, align 8
%argslist53276$ae507630 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54037 = alloca %struct.ScmObj*, align 8
%argslist53276$ae507631 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50765, %struct.ScmObj* %argslist53276$ae507630)
store volatile %struct.ScmObj* %argslist53276$ae507631, %struct.ScmObj** %stackaddr$prim54037, align 8
%stackaddr$prim54038 = alloca %struct.ScmObj*, align 8
%argslist53276$ae507632 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50764, %struct.ScmObj* %argslist53276$ae507631)
store volatile %struct.ScmObj* %argslist53276$ae507632, %struct.ScmObj** %stackaddr$prim54038, align 8
%clofunc54039 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50763)
musttail call tailcc void %clofunc54039(%struct.ScmObj* %ae50763, %struct.ScmObj* %argslist53276$ae507632)
ret void
}

define tailcc void @proc_clo$ae50763(%struct.ScmObj* %env$ae50763,%struct.ScmObj* %current_45args53217) {
%stackaddr$env-ref54040 = alloca %struct.ScmObj*, align 8
%_37_6247169 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50763, i64 0)
store %struct.ScmObj* %_37_6247169, %struct.ScmObj** %stackaddr$env-ref54040
%stackaddr$prim54041 = alloca %struct.ScmObj*, align 8
%_95k47339 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53217)
store volatile %struct.ScmObj* %_95k47339, %struct.ScmObj** %stackaddr$prim54041, align 8
%stackaddr$prim54042 = alloca %struct.ScmObj*, align 8
%current_45args53218 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53217)
store volatile %struct.ScmObj* %current_45args53218, %struct.ScmObj** %stackaddr$prim54042, align 8
%stackaddr$prim54043 = alloca %struct.ScmObj*, align 8
%_37first47132 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53218)
store volatile %struct.ScmObj* %_37first47132, %struct.ScmObj** %stackaddr$prim54043, align 8
%stackaddr$makeclosure54044 = alloca %struct.ScmObj*, align 8
%fptrToInt54045 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50783 to i64
%ae50783 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54045)
store volatile %struct.ScmObj* %ae50783, %struct.ScmObj** %stackaddr$makeclosure54044, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50783, %struct.ScmObj* %_37_6247169, i64 0)
%ae50784 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54046 = alloca %struct.ScmObj*, align 8
%fptrToInt54047 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50785 to i64
%ae50785 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54047)
store volatile %struct.ScmObj* %ae50785, %struct.ScmObj** %stackaddr$makeclosure54046, align 8
%argslist53271$ae507830 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54048 = alloca %struct.ScmObj*, align 8
%argslist53271$ae507831 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50785, %struct.ScmObj* %argslist53271$ae507830)
store volatile %struct.ScmObj* %argslist53271$ae507831, %struct.ScmObj** %stackaddr$prim54048, align 8
%stackaddr$prim54049 = alloca %struct.ScmObj*, align 8
%argslist53271$ae507832 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50784, %struct.ScmObj* %argslist53271$ae507831)
store volatile %struct.ScmObj* %argslist53271$ae507832, %struct.ScmObj** %stackaddr$prim54049, align 8
%clofunc54050 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50783)
musttail call tailcc void %clofunc54050(%struct.ScmObj* %ae50783, %struct.ScmObj* %argslist53271$ae507832)
ret void
}

define tailcc void @proc_clo$ae50783(%struct.ScmObj* %env$ae50783,%struct.ScmObj* %current_45args53220) {
%stackaddr$env-ref54051 = alloca %struct.ScmObj*, align 8
%_37_6247169 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50783, i64 0)
store %struct.ScmObj* %_37_6247169, %struct.ScmObj** %stackaddr$env-ref54051
%stackaddr$prim54052 = alloca %struct.ScmObj*, align 8
%_95k47340 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53220)
store volatile %struct.ScmObj* %_95k47340, %struct.ScmObj** %stackaddr$prim54052, align 8
%stackaddr$prim54053 = alloca %struct.ScmObj*, align 8
%current_45args53221 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53220)
store volatile %struct.ScmObj* %current_45args53221, %struct.ScmObj** %stackaddr$prim54053, align 8
%stackaddr$prim54054 = alloca %struct.ScmObj*, align 8
%_37second47130 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53221)
store volatile %struct.ScmObj* %_37second47130, %struct.ScmObj** %stackaddr$prim54054, align 8
%stackaddr$makeclosure54055 = alloca %struct.ScmObj*, align 8
%fptrToInt54056 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50805 to i64
%ae50805 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54056)
store volatile %struct.ScmObj* %ae50805, %struct.ScmObj** %stackaddr$makeclosure54055, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50805, %struct.ScmObj* %_37_6247169, i64 0)
%ae50806 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54057 = alloca %struct.ScmObj*, align 8
%fptrToInt54058 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50807 to i64
%ae50807 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54058)
store volatile %struct.ScmObj* %ae50807, %struct.ScmObj** %stackaddr$makeclosure54057, align 8
%argslist53266$ae508050 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54059 = alloca %struct.ScmObj*, align 8
%argslist53266$ae508051 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50807, %struct.ScmObj* %argslist53266$ae508050)
store volatile %struct.ScmObj* %argslist53266$ae508051, %struct.ScmObj** %stackaddr$prim54059, align 8
%stackaddr$prim54060 = alloca %struct.ScmObj*, align 8
%argslist53266$ae508052 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50806, %struct.ScmObj* %argslist53266$ae508051)
store volatile %struct.ScmObj* %argslist53266$ae508052, %struct.ScmObj** %stackaddr$prim54060, align 8
%clofunc54061 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50805)
musttail call tailcc void %clofunc54061(%struct.ScmObj* %ae50805, %struct.ScmObj* %argslist53266$ae508052)
ret void
}

define tailcc void @proc_clo$ae50805(%struct.ScmObj* %env$ae50805,%struct.ScmObj* %current_45args53223) {
%stackaddr$env-ref54062 = alloca %struct.ScmObj*, align 8
%_37_6247169 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50805, i64 0)
store %struct.ScmObj* %_37_6247169, %struct.ScmObj** %stackaddr$env-ref54062
%stackaddr$prim54063 = alloca %struct.ScmObj*, align 8
%_95k47341 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53223)
store volatile %struct.ScmObj* %_95k47341, %struct.ScmObj** %stackaddr$prim54063, align 8
%stackaddr$prim54064 = alloca %struct.ScmObj*, align 8
%current_45args53224 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53223)
store volatile %struct.ScmObj* %current_45args53224, %struct.ScmObj** %stackaddr$prim54064, align 8
%stackaddr$prim54065 = alloca %struct.ScmObj*, align 8
%_37third47128 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53224)
store volatile %struct.ScmObj* %_37third47128, %struct.ScmObj** %stackaddr$prim54065, align 8
%stackaddr$makeclosure54066 = alloca %struct.ScmObj*, align 8
%fptrToInt54067 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50829 to i64
%ae50829 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54067)
store volatile %struct.ScmObj* %ae50829, %struct.ScmObj** %stackaddr$makeclosure54066, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50829, %struct.ScmObj* %_37_6247169, i64 0)
%ae50830 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54068 = alloca %struct.ScmObj*, align 8
%fptrToInt54069 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50831 to i64
%ae50831 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54069)
store volatile %struct.ScmObj* %ae50831, %struct.ScmObj** %stackaddr$makeclosure54068, align 8
%argslist53261$ae508290 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54070 = alloca %struct.ScmObj*, align 8
%argslist53261$ae508291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50831, %struct.ScmObj* %argslist53261$ae508290)
store volatile %struct.ScmObj* %argslist53261$ae508291, %struct.ScmObj** %stackaddr$prim54070, align 8
%stackaddr$prim54071 = alloca %struct.ScmObj*, align 8
%argslist53261$ae508292 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50830, %struct.ScmObj* %argslist53261$ae508291)
store volatile %struct.ScmObj* %argslist53261$ae508292, %struct.ScmObj** %stackaddr$prim54071, align 8
%clofunc54072 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50829)
musttail call tailcc void %clofunc54072(%struct.ScmObj* %ae50829, %struct.ScmObj* %argslist53261$ae508292)
ret void
}

define tailcc void @proc_clo$ae50829(%struct.ScmObj* %env$ae50829,%struct.ScmObj* %current_45args53226) {
%stackaddr$env-ref54073 = alloca %struct.ScmObj*, align 8
%_37_6247169 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50829, i64 0)
store %struct.ScmObj* %_37_6247169, %struct.ScmObj** %stackaddr$env-ref54073
%stackaddr$prim54074 = alloca %struct.ScmObj*, align 8
%_95k47342 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53226)
store volatile %struct.ScmObj* %_95k47342, %struct.ScmObj** %stackaddr$prim54074, align 8
%stackaddr$prim54075 = alloca %struct.ScmObj*, align 8
%current_45args53227 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53226)
store volatile %struct.ScmObj* %current_45args53227, %struct.ScmObj** %stackaddr$prim54075, align 8
%stackaddr$prim54076 = alloca %struct.ScmObj*, align 8
%_37fourth47126 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53227)
store volatile %struct.ScmObj* %_37fourth47126, %struct.ScmObj** %stackaddr$prim54076, align 8
%stackaddr$prim54077 = alloca %struct.ScmObj*, align 8
%anf_45bind47303 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind47303, %struct.ScmObj** %stackaddr$prim54077, align 8
%ae50855 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54078 = alloca %struct.ScmObj*, align 8
%loop47187 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50855, %struct.ScmObj* %anf_45bind47303)
store volatile %struct.ScmObj* %loop47187, %struct.ScmObj** %stackaddr$prim54078, align 8
%stackaddr$makeclosure54079 = alloca %struct.ScmObj*, align 8
%fptrToInt54080 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50857 to i64
%ae50857 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54080)
store volatile %struct.ScmObj* %ae50857, %struct.ScmObj** %stackaddr$makeclosure54079, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50857, %struct.ScmObj* %loop47187, i64 0)
%ae50858 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54081 = alloca %struct.ScmObj*, align 8
%fptrToInt54082 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50859 to i64
%ae50859 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54082)
store volatile %struct.ScmObj* %ae50859, %struct.ScmObj** %stackaddr$makeclosure54081, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50859, %struct.ScmObj* %loop47187, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50859, %struct.ScmObj* %_37_6247169, i64 1)
%argslist53256$ae508570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54083 = alloca %struct.ScmObj*, align 8
%argslist53256$ae508571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50859, %struct.ScmObj* %argslist53256$ae508570)
store volatile %struct.ScmObj* %argslist53256$ae508571, %struct.ScmObj** %stackaddr$prim54083, align 8
%stackaddr$prim54084 = alloca %struct.ScmObj*, align 8
%argslist53256$ae508572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50858, %struct.ScmObj* %argslist53256$ae508571)
store volatile %struct.ScmObj* %argslist53256$ae508572, %struct.ScmObj** %stackaddr$prim54084, align 8
%clofunc54085 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50857)
musttail call tailcc void %clofunc54085(%struct.ScmObj* %ae50857, %struct.ScmObj* %argslist53256$ae508572)
ret void
}

define tailcc void @proc_clo$ae50857(%struct.ScmObj* %env$ae50857,%struct.ScmObj* %current_45args53229) {
%stackaddr$env-ref54086 = alloca %struct.ScmObj*, align 8
%loop47187 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50857, i64 0)
store %struct.ScmObj* %loop47187, %struct.ScmObj** %stackaddr$env-ref54086
%stackaddr$prim54087 = alloca %struct.ScmObj*, align 8
%_95k47343 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53229)
store volatile %struct.ScmObj* %_95k47343, %struct.ScmObj** %stackaddr$prim54087, align 8
%stackaddr$prim54088 = alloca %struct.ScmObj*, align 8
%current_45args53230 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53229)
store volatile %struct.ScmObj* %current_45args53230, %struct.ScmObj** %stackaddr$prim54088, align 8
%stackaddr$prim54089 = alloca %struct.ScmObj*, align 8
%anf_45bind47309 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53230)
store volatile %struct.ScmObj* %anf_45bind47309, %struct.ScmObj** %stackaddr$prim54089, align 8
%ae50973 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54090 = alloca %struct.ScmObj*, align 8
%t4706847188 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %loop47187, %struct.ScmObj* %ae50973, %struct.ScmObj* %anf_45bind47309)
store volatile %struct.ScmObj* %t4706847188, %struct.ScmObj** %stackaddr$prim54090, align 8
%ae50976 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54091 = alloca %struct.ScmObj*, align 8
%anf_45bind47310 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %loop47187, %struct.ScmObj* %ae50976)
store volatile %struct.ScmObj* %anf_45bind47310, %struct.ScmObj** %stackaddr$prim54091, align 8
%stackaddr$makeclosure54092 = alloca %struct.ScmObj*, align 8
%fptrToInt54093 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50978 to i64
%ae50978 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54093)
store volatile %struct.ScmObj* %ae50978, %struct.ScmObj** %stackaddr$makeclosure54092, align 8
%ae50979 = call %struct.ScmObj* @const_init_int(i64 1)
%ae50980 = call %struct.ScmObj* @const_init_int(i64 2)
%argslist53236$anf_45bind473100 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54094 = alloca %struct.ScmObj*, align 8
%argslist53236$anf_45bind473101 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50980, %struct.ScmObj* %argslist53236$anf_45bind473100)
store volatile %struct.ScmObj* %argslist53236$anf_45bind473101, %struct.ScmObj** %stackaddr$prim54094, align 8
%stackaddr$prim54095 = alloca %struct.ScmObj*, align 8
%argslist53236$anf_45bind473102 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50979, %struct.ScmObj* %argslist53236$anf_45bind473101)
store volatile %struct.ScmObj* %argslist53236$anf_45bind473102, %struct.ScmObj** %stackaddr$prim54095, align 8
%stackaddr$prim54096 = alloca %struct.ScmObj*, align 8
%argslist53236$anf_45bind473103 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50978, %struct.ScmObj* %argslist53236$anf_45bind473102)
store volatile %struct.ScmObj* %argslist53236$anf_45bind473103, %struct.ScmObj** %stackaddr$prim54096, align 8
%clofunc54097 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47310)
musttail call tailcc void %clofunc54097(%struct.ScmObj* %anf_45bind47310, %struct.ScmObj* %argslist53236$anf_45bind473103)
ret void
}

define tailcc void @proc_clo$ae50978(%struct.ScmObj* %env$ae50978,%struct.ScmObj* %current_45args53232) {
%stackaddr$prim54098 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53232)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim54098, align 8
%stackaddr$prim54099 = alloca %struct.ScmObj*, align 8
%current_45args53233 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53232)
store volatile %struct.ScmObj* %current_45args53233, %struct.ScmObj** %stackaddr$prim54099, align 8
%stackaddr$prim54100 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53233)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim54100, align 8
%stackaddr$prim54101 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim54101, align 8
%argslist53235$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54102 = alloca %struct.ScmObj*, align 8
%argslist53235$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist53235$k0)
store volatile %struct.ScmObj* %argslist53235$k1, %struct.ScmObj** %stackaddr$prim54102, align 8
%clofunc54103 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc54103(%struct.ScmObj* %k, %struct.ScmObj* %argslist53235$k1)
ret void
}

define tailcc void @proc_clo$ae50859(%struct.ScmObj* %env$ae50859,%struct.ScmObj* %current_45args53237) {
%stackaddr$env-ref54104 = alloca %struct.ScmObj*, align 8
%loop47187 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50859, i64 0)
store %struct.ScmObj* %loop47187, %struct.ScmObj** %stackaddr$env-ref54104
%stackaddr$env-ref54105 = alloca %struct.ScmObj*, align 8
%_37_6247169 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50859, i64 1)
store %struct.ScmObj* %_37_6247169, %struct.ScmObj** %stackaddr$env-ref54105
%stackaddr$prim54106 = alloca %struct.ScmObj*, align 8
%k47344 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53237)
store volatile %struct.ScmObj* %k47344, %struct.ScmObj** %stackaddr$prim54106, align 8
%stackaddr$prim54107 = alloca %struct.ScmObj*, align 8
%current_45args53238 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53237)
store volatile %struct.ScmObj* %current_45args53238, %struct.ScmObj** %stackaddr$prim54107, align 8
%stackaddr$prim54108 = alloca %struct.ScmObj*, align 8
%m47190 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53238)
store volatile %struct.ScmObj* %m47190, %struct.ScmObj** %stackaddr$prim54108, align 8
%stackaddr$prim54109 = alloca %struct.ScmObj*, align 8
%current_45args53239 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53238)
store volatile %struct.ScmObj* %current_45args53239, %struct.ScmObj** %stackaddr$prim54109, align 8
%stackaddr$prim54110 = alloca %struct.ScmObj*, align 8
%n47189 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53239)
store volatile %struct.ScmObj* %n47189, %struct.ScmObj** %stackaddr$prim54110, align 8
%stackaddr$makeclosure54111 = alloca %struct.ScmObj*, align 8
%fptrToInt54112 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50861 to i64
%ae50861 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54112)
store volatile %struct.ScmObj* %ae50861, %struct.ScmObj** %stackaddr$makeclosure54111, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50861, %struct.ScmObj* %m47190, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50861, %struct.ScmObj* %n47189, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50861, %struct.ScmObj* %loop47187, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae50861, %struct.ScmObj* %k47344, i64 3)
%ae50863 = call %struct.ScmObj* @const_init_int(i64 1000)
%argslist53255$_37_62471690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54113 = alloca %struct.ScmObj*, align 8
%argslist53255$_37_62471691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50863, %struct.ScmObj* %argslist53255$_37_62471690)
store volatile %struct.ScmObj* %argslist53255$_37_62471691, %struct.ScmObj** %stackaddr$prim54113, align 8
%stackaddr$prim54114 = alloca %struct.ScmObj*, align 8
%argslist53255$_37_62471692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %m47190, %struct.ScmObj* %argslist53255$_37_62471691)
store volatile %struct.ScmObj* %argslist53255$_37_62471692, %struct.ScmObj** %stackaddr$prim54114, align 8
%stackaddr$prim54115 = alloca %struct.ScmObj*, align 8
%argslist53255$_37_62471693 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50861, %struct.ScmObj* %argslist53255$_37_62471692)
store volatile %struct.ScmObj* %argslist53255$_37_62471693, %struct.ScmObj** %stackaddr$prim54115, align 8
%clofunc54116 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37_6247169)
musttail call tailcc void %clofunc54116(%struct.ScmObj* %_37_6247169, %struct.ScmObj* %argslist53255$_37_62471693)
ret void
}

define tailcc void @proc_clo$ae50861(%struct.ScmObj* %env$ae50861,%struct.ScmObj* %current_45args53241) {
%stackaddr$env-ref54117 = alloca %struct.ScmObj*, align 8
%m47190 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50861, i64 0)
store %struct.ScmObj* %m47190, %struct.ScmObj** %stackaddr$env-ref54117
%stackaddr$env-ref54118 = alloca %struct.ScmObj*, align 8
%n47189 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50861, i64 1)
store %struct.ScmObj* %n47189, %struct.ScmObj** %stackaddr$env-ref54118
%stackaddr$env-ref54119 = alloca %struct.ScmObj*, align 8
%loop47187 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50861, i64 2)
store %struct.ScmObj* %loop47187, %struct.ScmObj** %stackaddr$env-ref54119
%stackaddr$env-ref54120 = alloca %struct.ScmObj*, align 8
%k47344 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50861, i64 3)
store %struct.ScmObj* %k47344, %struct.ScmObj** %stackaddr$env-ref54120
%stackaddr$prim54121 = alloca %struct.ScmObj*, align 8
%_95k47345 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53241)
store volatile %struct.ScmObj* %_95k47345, %struct.ScmObj** %stackaddr$prim54121, align 8
%stackaddr$prim54122 = alloca %struct.ScmObj*, align 8
%current_45args53242 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53241)
store volatile %struct.ScmObj* %current_45args53242, %struct.ScmObj** %stackaddr$prim54122, align 8
%stackaddr$prim54123 = alloca %struct.ScmObj*, align 8
%anf_45bind47304 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53242)
store volatile %struct.ScmObj* %anf_45bind47304, %struct.ScmObj** %stackaddr$prim54123, align 8
%truthy$cmp54124 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47304)
%cmp$cmp54124 = icmp eq i64 %truthy$cmp54124, 1
br i1 %cmp$cmp54124, label %truebranch$cmp54124, label %falsebranch$cmp54124
truebranch$cmp54124:
%stackaddr$makeclosure54125 = alloca %struct.ScmObj*, align 8
%fptrToInt54126 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50869 to i64
%ae50869 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54126)
store volatile %struct.ScmObj* %ae50869, %struct.ScmObj** %stackaddr$makeclosure54125, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50869, %struct.ScmObj* %k47344, i64 0)
%ae50870 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54127 = alloca %struct.ScmObj*, align 8
%fptrToInt54128 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50871 to i64
%ae50871 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54128)
store volatile %struct.ScmObj* %ae50871, %struct.ScmObj** %stackaddr$makeclosure54127, align 8
%argslist53249$ae508690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54129 = alloca %struct.ScmObj*, align 8
%argslist53249$ae508691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50871, %struct.ScmObj* %argslist53249$ae508690)
store volatile %struct.ScmObj* %argslist53249$ae508691, %struct.ScmObj** %stackaddr$prim54129, align 8
%stackaddr$prim54130 = alloca %struct.ScmObj*, align 8
%argslist53249$ae508692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50870, %struct.ScmObj* %argslist53249$ae508691)
store volatile %struct.ScmObj* %argslist53249$ae508692, %struct.ScmObj** %stackaddr$prim54130, align 8
%clofunc54131 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50869)
musttail call tailcc void %clofunc54131(%struct.ScmObj* %ae50869, %struct.ScmObj* %argslist53249$ae508692)
ret void
falsebranch$cmp54124:
%ae50902 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54132 = alloca %struct.ScmObj*, align 8
%anf_45bind47306 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %loop47187, %struct.ScmObj* %ae50902)
store volatile %struct.ScmObj* %anf_45bind47306, %struct.ScmObj** %stackaddr$prim54132, align 8
%stackaddr$prim54133 = alloca %struct.ScmObj*, align 8
%anf_45bind47307 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %m47190, %struct.ScmObj* %n47189)
store volatile %struct.ScmObj* %anf_45bind47307, %struct.ScmObj** %stackaddr$prim54133, align 8
%stackaddr$makeclosure54134 = alloca %struct.ScmObj*, align 8
%fptrToInt54135 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50906 to i64
%ae50906 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54135)
store volatile %struct.ScmObj* %ae50906, %struct.ScmObj** %stackaddr$makeclosure54134, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50906, %struct.ScmObj* %m47190, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50906, %struct.ScmObj* %k47344, i64 1)
%argslist53254$anf_45bind473060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54136 = alloca %struct.ScmObj*, align 8
%argslist53254$anf_45bind473061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47307, %struct.ScmObj* %argslist53254$anf_45bind473060)
store volatile %struct.ScmObj* %argslist53254$anf_45bind473061, %struct.ScmObj** %stackaddr$prim54136, align 8
%stackaddr$prim54137 = alloca %struct.ScmObj*, align 8
%argslist53254$anf_45bind473062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %n47189, %struct.ScmObj* %argslist53254$anf_45bind473061)
store volatile %struct.ScmObj* %argslist53254$anf_45bind473062, %struct.ScmObj** %stackaddr$prim54137, align 8
%stackaddr$prim54138 = alloca %struct.ScmObj*, align 8
%argslist53254$anf_45bind473063 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50906, %struct.ScmObj* %argslist53254$anf_45bind473062)
store volatile %struct.ScmObj* %argslist53254$anf_45bind473063, %struct.ScmObj** %stackaddr$prim54138, align 8
%clofunc54139 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47306)
musttail call tailcc void %clofunc54139(%struct.ScmObj* %anf_45bind47306, %struct.ScmObj* %argslist53254$anf_45bind473063)
ret void
}

define tailcc void @proc_clo$ae50869(%struct.ScmObj* %env$ae50869,%struct.ScmObj* %current_45args53244) {
%stackaddr$env-ref54140 = alloca %struct.ScmObj*, align 8
%k47344 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50869, i64 0)
store %struct.ScmObj* %k47344, %struct.ScmObj** %stackaddr$env-ref54140
%stackaddr$prim54141 = alloca %struct.ScmObj*, align 8
%_95k47346 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53244)
store volatile %struct.ScmObj* %_95k47346, %struct.ScmObj** %stackaddr$prim54141, align 8
%stackaddr$prim54142 = alloca %struct.ScmObj*, align 8
%current_45args53245 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53244)
store volatile %struct.ScmObj* %current_45args53245, %struct.ScmObj** %stackaddr$prim54142, align 8
%stackaddr$prim54143 = alloca %struct.ScmObj*, align 8
%anf_45bind47305 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53245)
store volatile %struct.ScmObj* %anf_45bind47305, %struct.ScmObj** %stackaddr$prim54143, align 8
%argslist53247$anf_45bind473050 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54144 = alloca %struct.ScmObj*, align 8
%argslist53247$anf_45bind473051 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47344, %struct.ScmObj* %argslist53247$anf_45bind473050)
store volatile %struct.ScmObj* %argslist53247$anf_45bind473051, %struct.ScmObj** %stackaddr$prim54144, align 8
%clofunc54145 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47305)
musttail call tailcc void %clofunc54145(%struct.ScmObj* %anf_45bind47305, %struct.ScmObj* %argslist53247$anf_45bind473051)
ret void
}

define tailcc void @proc_clo$ae50871(%struct.ScmObj* %env$ae50871,%struct.ScmObj* %lst4719147347) {
%stackaddr$prim54146 = alloca %struct.ScmObj*, align 8
%k47348 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4719147347)
store volatile %struct.ScmObj* %k47348, %struct.ScmObj** %stackaddr$prim54146, align 8
%stackaddr$prim54147 = alloca %struct.ScmObj*, align 8
%lst47191 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4719147347)
store volatile %struct.ScmObj* %lst47191, %struct.ScmObj** %stackaddr$prim54147, align 8
%ae50875 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53248$k473480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54148 = alloca %struct.ScmObj*, align 8
%argslist53248$k473481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47191, %struct.ScmObj* %argslist53248$k473480)
store volatile %struct.ScmObj* %argslist53248$k473481, %struct.ScmObj** %stackaddr$prim54148, align 8
%stackaddr$prim54149 = alloca %struct.ScmObj*, align 8
%argslist53248$k473482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50875, %struct.ScmObj* %argslist53248$k473481)
store volatile %struct.ScmObj* %argslist53248$k473482, %struct.ScmObj** %stackaddr$prim54149, align 8
%clofunc54150 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47348)
musttail call tailcc void %clofunc54150(%struct.ScmObj* %k47348, %struct.ScmObj* %argslist53248$k473482)
ret void
}

define tailcc void @proc_clo$ae50906(%struct.ScmObj* %env$ae50906,%struct.ScmObj* %current_45args53250) {
%stackaddr$env-ref54151 = alloca %struct.ScmObj*, align 8
%m47190 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50906, i64 0)
store %struct.ScmObj* %m47190, %struct.ScmObj** %stackaddr$env-ref54151
%stackaddr$env-ref54152 = alloca %struct.ScmObj*, align 8
%k47344 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50906, i64 1)
store %struct.ScmObj* %k47344, %struct.ScmObj** %stackaddr$env-ref54152
%stackaddr$prim54153 = alloca %struct.ScmObj*, align 8
%_95k47349 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53250)
store volatile %struct.ScmObj* %_95k47349, %struct.ScmObj** %stackaddr$prim54153, align 8
%stackaddr$prim54154 = alloca %struct.ScmObj*, align 8
%current_45args53251 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53250)
store volatile %struct.ScmObj* %current_45args53251, %struct.ScmObj** %stackaddr$prim54154, align 8
%stackaddr$prim54155 = alloca %struct.ScmObj*, align 8
%anf_45bind47308 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53251)
store volatile %struct.ScmObj* %anf_45bind47308, %struct.ScmObj** %stackaddr$prim54155, align 8
%stackaddr$prim54156 = alloca %struct.ScmObj*, align 8
%cpsprim47350 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %m47190, %struct.ScmObj* %anf_45bind47308)
store volatile %struct.ScmObj* %cpsprim47350, %struct.ScmObj** %stackaddr$prim54156, align 8
%ae50912 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53253$k473440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54157 = alloca %struct.ScmObj*, align 8
%argslist53253$k473441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47350, %struct.ScmObj* %argslist53253$k473440)
store volatile %struct.ScmObj* %argslist53253$k473441, %struct.ScmObj** %stackaddr$prim54157, align 8
%stackaddr$prim54158 = alloca %struct.ScmObj*, align 8
%argslist53253$k473442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50912, %struct.ScmObj* %argslist53253$k473441)
store volatile %struct.ScmObj* %argslist53253$k473442, %struct.ScmObj** %stackaddr$prim54158, align 8
%clofunc54159 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47344)
musttail call tailcc void %clofunc54159(%struct.ScmObj* %k47344, %struct.ScmObj* %argslist53253$k473442)
ret void
}

define tailcc void @proc_clo$ae50831(%struct.ScmObj* %env$ae50831,%struct.ScmObj* %current_45args53257) {
%stackaddr$prim54160 = alloca %struct.ScmObj*, align 8
%k47351 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53257)
store volatile %struct.ScmObj* %k47351, %struct.ScmObj** %stackaddr$prim54160, align 8
%stackaddr$prim54161 = alloca %struct.ScmObj*, align 8
%current_45args53258 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53257)
store volatile %struct.ScmObj* %current_45args53258, %struct.ScmObj** %stackaddr$prim54161, align 8
%stackaddr$prim54162 = alloca %struct.ScmObj*, align 8
%x47127 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53258)
store volatile %struct.ScmObj* %x47127, %struct.ScmObj** %stackaddr$prim54162, align 8
%stackaddr$prim54163 = alloca %struct.ScmObj*, align 8
%anf_45bind47300 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47127)
store volatile %struct.ScmObj* %anf_45bind47300, %struct.ScmObj** %stackaddr$prim54163, align 8
%stackaddr$prim54164 = alloca %struct.ScmObj*, align 8
%anf_45bind47301 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47300)
store volatile %struct.ScmObj* %anf_45bind47301, %struct.ScmObj** %stackaddr$prim54164, align 8
%stackaddr$prim54165 = alloca %struct.ScmObj*, align 8
%anf_45bind47302 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47301)
store volatile %struct.ScmObj* %anf_45bind47302, %struct.ScmObj** %stackaddr$prim54165, align 8
%stackaddr$prim54166 = alloca %struct.ScmObj*, align 8
%cpsprim47352 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47302)
store volatile %struct.ScmObj* %cpsprim47352, %struct.ScmObj** %stackaddr$prim54166, align 8
%ae50837 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53260$k473510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54167 = alloca %struct.ScmObj*, align 8
%argslist53260$k473511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47352, %struct.ScmObj* %argslist53260$k473510)
store volatile %struct.ScmObj* %argslist53260$k473511, %struct.ScmObj** %stackaddr$prim54167, align 8
%stackaddr$prim54168 = alloca %struct.ScmObj*, align 8
%argslist53260$k473512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50837, %struct.ScmObj* %argslist53260$k473511)
store volatile %struct.ScmObj* %argslist53260$k473512, %struct.ScmObj** %stackaddr$prim54168, align 8
%clofunc54169 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47351)
musttail call tailcc void %clofunc54169(%struct.ScmObj* %k47351, %struct.ScmObj* %argslist53260$k473512)
ret void
}

define tailcc void @proc_clo$ae50807(%struct.ScmObj* %env$ae50807,%struct.ScmObj* %current_45args53262) {
%stackaddr$prim54170 = alloca %struct.ScmObj*, align 8
%k47353 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53262)
store volatile %struct.ScmObj* %k47353, %struct.ScmObj** %stackaddr$prim54170, align 8
%stackaddr$prim54171 = alloca %struct.ScmObj*, align 8
%current_45args53263 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53262)
store volatile %struct.ScmObj* %current_45args53263, %struct.ScmObj** %stackaddr$prim54171, align 8
%stackaddr$prim54172 = alloca %struct.ScmObj*, align 8
%x47129 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53263)
store volatile %struct.ScmObj* %x47129, %struct.ScmObj** %stackaddr$prim54172, align 8
%stackaddr$prim54173 = alloca %struct.ScmObj*, align 8
%anf_45bind47298 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47129)
store volatile %struct.ScmObj* %anf_45bind47298, %struct.ScmObj** %stackaddr$prim54173, align 8
%stackaddr$prim54174 = alloca %struct.ScmObj*, align 8
%anf_45bind47299 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47298)
store volatile %struct.ScmObj* %anf_45bind47299, %struct.ScmObj** %stackaddr$prim54174, align 8
%stackaddr$prim54175 = alloca %struct.ScmObj*, align 8
%cpsprim47354 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47299)
store volatile %struct.ScmObj* %cpsprim47354, %struct.ScmObj** %stackaddr$prim54175, align 8
%ae50812 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53265$k473530 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54176 = alloca %struct.ScmObj*, align 8
%argslist53265$k473531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47354, %struct.ScmObj* %argslist53265$k473530)
store volatile %struct.ScmObj* %argslist53265$k473531, %struct.ScmObj** %stackaddr$prim54176, align 8
%stackaddr$prim54177 = alloca %struct.ScmObj*, align 8
%argslist53265$k473532 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50812, %struct.ScmObj* %argslist53265$k473531)
store volatile %struct.ScmObj* %argslist53265$k473532, %struct.ScmObj** %stackaddr$prim54177, align 8
%clofunc54178 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47353)
musttail call tailcc void %clofunc54178(%struct.ScmObj* %k47353, %struct.ScmObj* %argslist53265$k473532)
ret void
}

define tailcc void @proc_clo$ae50785(%struct.ScmObj* %env$ae50785,%struct.ScmObj* %current_45args53267) {
%stackaddr$prim54179 = alloca %struct.ScmObj*, align 8
%k47355 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53267)
store volatile %struct.ScmObj* %k47355, %struct.ScmObj** %stackaddr$prim54179, align 8
%stackaddr$prim54180 = alloca %struct.ScmObj*, align 8
%current_45args53268 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53267)
store volatile %struct.ScmObj* %current_45args53268, %struct.ScmObj** %stackaddr$prim54180, align 8
%stackaddr$prim54181 = alloca %struct.ScmObj*, align 8
%x47131 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53268)
store volatile %struct.ScmObj* %x47131, %struct.ScmObj** %stackaddr$prim54181, align 8
%stackaddr$prim54182 = alloca %struct.ScmObj*, align 8
%anf_45bind47297 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47131)
store volatile %struct.ScmObj* %anf_45bind47297, %struct.ScmObj** %stackaddr$prim54182, align 8
%stackaddr$prim54183 = alloca %struct.ScmObj*, align 8
%cpsprim47356 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47297)
store volatile %struct.ScmObj* %cpsprim47356, %struct.ScmObj** %stackaddr$prim54183, align 8
%ae50789 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53270$k473550 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54184 = alloca %struct.ScmObj*, align 8
%argslist53270$k473551 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47356, %struct.ScmObj* %argslist53270$k473550)
store volatile %struct.ScmObj* %argslist53270$k473551, %struct.ScmObj** %stackaddr$prim54184, align 8
%stackaddr$prim54185 = alloca %struct.ScmObj*, align 8
%argslist53270$k473552 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50789, %struct.ScmObj* %argslist53270$k473551)
store volatile %struct.ScmObj* %argslist53270$k473552, %struct.ScmObj** %stackaddr$prim54185, align 8
%clofunc54186 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47355)
musttail call tailcc void %clofunc54186(%struct.ScmObj* %k47355, %struct.ScmObj* %argslist53270$k473552)
ret void
}

define tailcc void @proc_clo$ae50765(%struct.ScmObj* %env$ae50765,%struct.ScmObj* %current_45args53272) {
%stackaddr$prim54187 = alloca %struct.ScmObj*, align 8
%k47357 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53272)
store volatile %struct.ScmObj* %k47357, %struct.ScmObj** %stackaddr$prim54187, align 8
%stackaddr$prim54188 = alloca %struct.ScmObj*, align 8
%current_45args53273 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53272)
store volatile %struct.ScmObj* %current_45args53273, %struct.ScmObj** %stackaddr$prim54188, align 8
%stackaddr$prim54189 = alloca %struct.ScmObj*, align 8
%x47133 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53273)
store volatile %struct.ScmObj* %x47133, %struct.ScmObj** %stackaddr$prim54189, align 8
%stackaddr$prim54190 = alloca %struct.ScmObj*, align 8
%cpsprim47358 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47133)
store volatile %struct.ScmObj* %cpsprim47358, %struct.ScmObj** %stackaddr$prim54190, align 8
%ae50768 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53275$k473570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54191 = alloca %struct.ScmObj*, align 8
%argslist53275$k473571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47358, %struct.ScmObj* %argslist53275$k473570)
store volatile %struct.ScmObj* %argslist53275$k473571, %struct.ScmObj** %stackaddr$prim54191, align 8
%stackaddr$prim54192 = alloca %struct.ScmObj*, align 8
%argslist53275$k473572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50768, %struct.ScmObj* %argslist53275$k473571)
store volatile %struct.ScmObj* %argslist53275$k473572, %struct.ScmObj** %stackaddr$prim54192, align 8
%clofunc54193 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47357)
musttail call tailcc void %clofunc54193(%struct.ScmObj* %k47357, %struct.ScmObj* %argslist53275$k473572)
ret void
}

define tailcc void @proc_clo$ae50667(%struct.ScmObj* %env$ae50667,%struct.ScmObj* %args4713547359) {
%stackaddr$env-ref54194 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50667, i64 0)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref54194
%stackaddr$prim54195 = alloca %struct.ScmObj*, align 8
%k47360 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4713547359)
store volatile %struct.ScmObj* %k47360, %struct.ScmObj** %stackaddr$prim54195, align 8
%stackaddr$prim54196 = alloca %struct.ScmObj*, align 8
%args47135 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4713547359)
store volatile %struct.ScmObj* %args47135, %struct.ScmObj** %stackaddr$prim54196, align 8
%stackaddr$prim54197 = alloca %struct.ScmObj*, align 8
%anf_45bind47291 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args47135)
store volatile %struct.ScmObj* %anf_45bind47291, %struct.ScmObj** %stackaddr$prim54197, align 8
%truthy$cmp54198 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47291)
%cmp$cmp54198 = icmp eq i64 %truthy$cmp54198, 1
br i1 %cmp$cmp54198, label %truebranch$cmp54198, label %falsebranch$cmp54198
truebranch$cmp54198:
%ae50673 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50674 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist53277$k473600 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54199 = alloca %struct.ScmObj*, align 8
%argslist53277$k473601 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50674, %struct.ScmObj* %argslist53277$k473600)
store volatile %struct.ScmObj* %argslist53277$k473601, %struct.ScmObj** %stackaddr$prim54199, align 8
%stackaddr$prim54200 = alloca %struct.ScmObj*, align 8
%argslist53277$k473602 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50673, %struct.ScmObj* %argslist53277$k473601)
store volatile %struct.ScmObj* %argslist53277$k473602, %struct.ScmObj** %stackaddr$prim54200, align 8
%clofunc54201 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47360)
musttail call tailcc void %clofunc54201(%struct.ScmObj* %k47360, %struct.ScmObj* %argslist53277$k473602)
ret void
falsebranch$cmp54198:
%stackaddr$prim54202 = alloca %struct.ScmObj*, align 8
%anf_45bind47292 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47135)
store volatile %struct.ScmObj* %anf_45bind47292, %struct.ScmObj** %stackaddr$prim54202, align 8
%stackaddr$prim54203 = alloca %struct.ScmObj*, align 8
%anf_45bind47293 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47292)
store volatile %struct.ScmObj* %anf_45bind47293, %struct.ScmObj** %stackaddr$prim54203, align 8
%truthy$cmp54204 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47293)
%cmp$cmp54204 = icmp eq i64 %truthy$cmp54204, 1
br i1 %cmp$cmp54204, label %truebranch$cmp54204, label %falsebranch$cmp54204
truebranch$cmp54204:
%stackaddr$prim54205 = alloca %struct.ScmObj*, align 8
%cpsprim47361 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47135)
store volatile %struct.ScmObj* %cpsprim47361, %struct.ScmObj** %stackaddr$prim54205, align 8
%ae50686 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53278$k473600 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54206 = alloca %struct.ScmObj*, align 8
%argslist53278$k473601 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47361, %struct.ScmObj* %argslist53278$k473600)
store volatile %struct.ScmObj* %argslist53278$k473601, %struct.ScmObj** %stackaddr$prim54206, align 8
%stackaddr$prim54207 = alloca %struct.ScmObj*, align 8
%argslist53278$k473602 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50686, %struct.ScmObj* %argslist53278$k473601)
store volatile %struct.ScmObj* %argslist53278$k473602, %struct.ScmObj** %stackaddr$prim54207, align 8
%clofunc54208 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47360)
musttail call tailcc void %clofunc54208(%struct.ScmObj* %k47360, %struct.ScmObj* %argslist53278$k473602)
ret void
falsebranch$cmp54204:
%stackaddr$makeclosure54209 = alloca %struct.ScmObj*, align 8
%fptrToInt54210 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50691 to i64
%ae50691 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54210)
store volatile %struct.ScmObj* %ae50691, %struct.ScmObj** %stackaddr$makeclosure54209, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50691, %struct.ScmObj* %_37foldl147074, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50691, %struct.ScmObj* %k47360, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50691, %struct.ScmObj* %args47135, i64 2)
%ae50692 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54211 = alloca %struct.ScmObj*, align 8
%fptrToInt54212 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50693 to i64
%ae50693 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54212)
store volatile %struct.ScmObj* %ae50693, %struct.ScmObj** %stackaddr$makeclosure54211, align 8
%argslist53288$ae506910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54213 = alloca %struct.ScmObj*, align 8
%argslist53288$ae506911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50693, %struct.ScmObj* %argslist53288$ae506910)
store volatile %struct.ScmObj* %argslist53288$ae506911, %struct.ScmObj** %stackaddr$prim54213, align 8
%stackaddr$prim54214 = alloca %struct.ScmObj*, align 8
%argslist53288$ae506912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50692, %struct.ScmObj* %argslist53288$ae506911)
store volatile %struct.ScmObj* %argslist53288$ae506912, %struct.ScmObj** %stackaddr$prim54214, align 8
%clofunc54215 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50691)
musttail call tailcc void %clofunc54215(%struct.ScmObj* %ae50691, %struct.ScmObj* %argslist53288$ae506912)
ret void
}

define tailcc void @proc_clo$ae50691(%struct.ScmObj* %env$ae50691,%struct.ScmObj* %current_45args53279) {
%stackaddr$env-ref54216 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50691, i64 0)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref54216
%stackaddr$env-ref54217 = alloca %struct.ScmObj*, align 8
%k47360 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50691, i64 1)
store %struct.ScmObj* %k47360, %struct.ScmObj** %stackaddr$env-ref54217
%stackaddr$env-ref54218 = alloca %struct.ScmObj*, align 8
%args47135 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50691, i64 2)
store %struct.ScmObj* %args47135, %struct.ScmObj** %stackaddr$env-ref54218
%stackaddr$prim54219 = alloca %struct.ScmObj*, align 8
%_95k47362 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53279)
store volatile %struct.ScmObj* %_95k47362, %struct.ScmObj** %stackaddr$prim54219, align 8
%stackaddr$prim54220 = alloca %struct.ScmObj*, align 8
%current_45args53280 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53279)
store volatile %struct.ScmObj* %current_45args53280, %struct.ScmObj** %stackaddr$prim54220, align 8
%stackaddr$prim54221 = alloca %struct.ScmObj*, align 8
%anf_45bind47294 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53280)
store volatile %struct.ScmObj* %anf_45bind47294, %struct.ScmObj** %stackaddr$prim54221, align 8
%stackaddr$prim54222 = alloca %struct.ScmObj*, align 8
%anf_45bind47295 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47135)
store volatile %struct.ScmObj* %anf_45bind47295, %struct.ScmObj** %stackaddr$prim54222, align 8
%stackaddr$prim54223 = alloca %struct.ScmObj*, align 8
%anf_45bind47296 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47135)
store volatile %struct.ScmObj* %anf_45bind47296, %struct.ScmObj** %stackaddr$prim54223, align 8
%argslist53282$_37foldl1470740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54224 = alloca %struct.ScmObj*, align 8
%argslist53282$_37foldl1470741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47296, %struct.ScmObj* %argslist53282$_37foldl1470740)
store volatile %struct.ScmObj* %argslist53282$_37foldl1470741, %struct.ScmObj** %stackaddr$prim54224, align 8
%stackaddr$prim54225 = alloca %struct.ScmObj*, align 8
%argslist53282$_37foldl1470742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47295, %struct.ScmObj* %argslist53282$_37foldl1470741)
store volatile %struct.ScmObj* %argslist53282$_37foldl1470742, %struct.ScmObj** %stackaddr$prim54225, align 8
%stackaddr$prim54226 = alloca %struct.ScmObj*, align 8
%argslist53282$_37foldl1470743 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47294, %struct.ScmObj* %argslist53282$_37foldl1470742)
store volatile %struct.ScmObj* %argslist53282$_37foldl1470743, %struct.ScmObj** %stackaddr$prim54226, align 8
%stackaddr$prim54227 = alloca %struct.ScmObj*, align 8
%argslist53282$_37foldl1470744 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47360, %struct.ScmObj* %argslist53282$_37foldl1470743)
store volatile %struct.ScmObj* %argslist53282$_37foldl1470744, %struct.ScmObj** %stackaddr$prim54227, align 8
%clofunc54228 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147074)
musttail call tailcc void %clofunc54228(%struct.ScmObj* %_37foldl147074, %struct.ScmObj* %argslist53282$_37foldl1470744)
ret void
}

define tailcc void @proc_clo$ae50693(%struct.ScmObj* %env$ae50693,%struct.ScmObj* %current_45args53283) {
%stackaddr$prim54229 = alloca %struct.ScmObj*, align 8
%k47363 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53283)
store volatile %struct.ScmObj* %k47363, %struct.ScmObj** %stackaddr$prim54229, align 8
%stackaddr$prim54230 = alloca %struct.ScmObj*, align 8
%current_45args53284 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53283)
store volatile %struct.ScmObj* %current_45args53284, %struct.ScmObj** %stackaddr$prim54230, align 8
%stackaddr$prim54231 = alloca %struct.ScmObj*, align 8
%n47137 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53284)
store volatile %struct.ScmObj* %n47137, %struct.ScmObj** %stackaddr$prim54231, align 8
%stackaddr$prim54232 = alloca %struct.ScmObj*, align 8
%current_45args53285 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53284)
store volatile %struct.ScmObj* %current_45args53285, %struct.ScmObj** %stackaddr$prim54232, align 8
%stackaddr$prim54233 = alloca %struct.ScmObj*, align 8
%v47136 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53285)
store volatile %struct.ScmObj* %v47136, %struct.ScmObj** %stackaddr$prim54233, align 8
%stackaddr$prim54234 = alloca %struct.ScmObj*, align 8
%cpsprim47364 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v47136, %struct.ScmObj* %n47137)
store volatile %struct.ScmObj* %cpsprim47364, %struct.ScmObj** %stackaddr$prim54234, align 8
%ae50697 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53287$k473630 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54235 = alloca %struct.ScmObj*, align 8
%argslist53287$k473631 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47364, %struct.ScmObj* %argslist53287$k473630)
store volatile %struct.ScmObj* %argslist53287$k473631, %struct.ScmObj** %stackaddr$prim54235, align 8
%stackaddr$prim54236 = alloca %struct.ScmObj*, align 8
%argslist53287$k473632 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50697, %struct.ScmObj* %argslist53287$k473631)
store volatile %struct.ScmObj* %argslist53287$k473632, %struct.ScmObj** %stackaddr$prim54236, align 8
%clofunc54237 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47363)
musttail call tailcc void %clofunc54237(%struct.ScmObj* %k47363, %struct.ScmObj* %argslist53287$k473632)
ret void
}

define tailcc void @proc_clo$ae50263(%struct.ScmObj* %env$ae50263,%struct.ScmObj* %current_45args53290) {
%stackaddr$prim54238 = alloca %struct.ScmObj*, align 8
%k47365 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53290)
store volatile %struct.ScmObj* %k47365, %struct.ScmObj** %stackaddr$prim54238, align 8
%stackaddr$prim54239 = alloca %struct.ScmObj*, align 8
%current_45args53291 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53290)
store volatile %struct.ScmObj* %current_45args53291, %struct.ScmObj** %stackaddr$prim54239, align 8
%stackaddr$prim54240 = alloca %struct.ScmObj*, align 8
%v47140 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53291)
store volatile %struct.ScmObj* %v47140, %struct.ScmObj** %stackaddr$prim54240, align 8
%stackaddr$prim54241 = alloca %struct.ScmObj*, align 8
%current_45args53292 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53291)
store volatile %struct.ScmObj* %current_45args53292, %struct.ScmObj** %stackaddr$prim54241, align 8
%stackaddr$prim54242 = alloca %struct.ScmObj*, align 8
%lst47139 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53292)
store volatile %struct.ScmObj* %lst47139, %struct.ScmObj** %stackaddr$prim54242, align 8
%ae50264 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54243 = alloca %struct.ScmObj*, align 8
%lst47141 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50264, %struct.ScmObj* %lst47139)
store volatile %struct.ScmObj* %lst47141, %struct.ScmObj** %stackaddr$prim54243, align 8
%stackaddr$makeclosure54244 = alloca %struct.ScmObj*, align 8
%fptrToInt54245 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50266 to i64
%ae50266 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54245)
store volatile %struct.ScmObj* %ae50266, %struct.ScmObj** %stackaddr$makeclosure54244, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50266, %struct.ScmObj* %lst47141, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50266, %struct.ScmObj* %k47365, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50266, %struct.ScmObj* %v47140, i64 2)
%ae50267 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54246 = alloca %struct.ScmObj*, align 8
%fptrToInt54247 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50268 to i64
%ae50268 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54247)
store volatile %struct.ScmObj* %ae50268, %struct.ScmObj** %stackaddr$makeclosure54246, align 8
%argslist53314$ae502660 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54248 = alloca %struct.ScmObj*, align 8
%argslist53314$ae502661 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50268, %struct.ScmObj* %argslist53314$ae502660)
store volatile %struct.ScmObj* %argslist53314$ae502661, %struct.ScmObj** %stackaddr$prim54248, align 8
%stackaddr$prim54249 = alloca %struct.ScmObj*, align 8
%argslist53314$ae502662 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50267, %struct.ScmObj* %argslist53314$ae502661)
store volatile %struct.ScmObj* %argslist53314$ae502662, %struct.ScmObj** %stackaddr$prim54249, align 8
%clofunc54250 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50266)
musttail call tailcc void %clofunc54250(%struct.ScmObj* %ae50266, %struct.ScmObj* %argslist53314$ae502662)
ret void
}

define tailcc void @proc_clo$ae50266(%struct.ScmObj* %env$ae50266,%struct.ScmObj* %current_45args53294) {
%stackaddr$env-ref54251 = alloca %struct.ScmObj*, align 8
%lst47141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50266, i64 0)
store %struct.ScmObj* %lst47141, %struct.ScmObj** %stackaddr$env-ref54251
%stackaddr$env-ref54252 = alloca %struct.ScmObj*, align 8
%k47365 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50266, i64 1)
store %struct.ScmObj* %k47365, %struct.ScmObj** %stackaddr$env-ref54252
%stackaddr$env-ref54253 = alloca %struct.ScmObj*, align 8
%v47140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50266, i64 2)
store %struct.ScmObj* %v47140, %struct.ScmObj** %stackaddr$env-ref54253
%stackaddr$prim54254 = alloca %struct.ScmObj*, align 8
%_95k47366 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53294)
store volatile %struct.ScmObj* %_95k47366, %struct.ScmObj** %stackaddr$prim54254, align 8
%stackaddr$prim54255 = alloca %struct.ScmObj*, align 8
%current_45args53295 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53294)
store volatile %struct.ScmObj* %current_45args53295, %struct.ScmObj** %stackaddr$prim54255, align 8
%stackaddr$prim54256 = alloca %struct.ScmObj*, align 8
%anf_45bind47283 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53295)
store volatile %struct.ScmObj* %anf_45bind47283, %struct.ScmObj** %stackaddr$prim54256, align 8
%stackaddr$makeclosure54257 = alloca %struct.ScmObj*, align 8
%fptrToInt54258 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50282 to i64
%ae50282 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54258)
store volatile %struct.ScmObj* %ae50282, %struct.ScmObj** %stackaddr$makeclosure54257, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50282, %struct.ScmObj* %lst47141, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50282, %struct.ScmObj* %k47365, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50282, %struct.ScmObj* %v47140, i64 2)
%stackaddr$makeclosure54259 = alloca %struct.ScmObj*, align 8
%fptrToInt54260 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50283 to i64
%ae50283 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54260)
store volatile %struct.ScmObj* %ae50283, %struct.ScmObj** %stackaddr$makeclosure54259, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50283, %struct.ScmObj* %lst47141, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50283, %struct.ScmObj* %k47365, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50283, %struct.ScmObj* %v47140, i64 2)
%argslist53309$anf_45bind472830 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54261 = alloca %struct.ScmObj*, align 8
%argslist53309$anf_45bind472831 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50283, %struct.ScmObj* %argslist53309$anf_45bind472830)
store volatile %struct.ScmObj* %argslist53309$anf_45bind472831, %struct.ScmObj** %stackaddr$prim54261, align 8
%stackaddr$prim54262 = alloca %struct.ScmObj*, align 8
%argslist53309$anf_45bind472832 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50282, %struct.ScmObj* %argslist53309$anf_45bind472831)
store volatile %struct.ScmObj* %argslist53309$anf_45bind472832, %struct.ScmObj** %stackaddr$prim54262, align 8
%clofunc54263 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47283)
musttail call tailcc void %clofunc54263(%struct.ScmObj* %anf_45bind47283, %struct.ScmObj* %argslist53309$anf_45bind472832)
ret void
}

define tailcc void @proc_clo$ae50282(%struct.ScmObj* %env$ae50282,%struct.ScmObj* %current_45args53297) {
%stackaddr$env-ref54264 = alloca %struct.ScmObj*, align 8
%lst47141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50282, i64 0)
store %struct.ScmObj* %lst47141, %struct.ScmObj** %stackaddr$env-ref54264
%stackaddr$env-ref54265 = alloca %struct.ScmObj*, align 8
%k47365 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50282, i64 1)
store %struct.ScmObj* %k47365, %struct.ScmObj** %stackaddr$env-ref54265
%stackaddr$env-ref54266 = alloca %struct.ScmObj*, align 8
%v47140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50282, i64 2)
store %struct.ScmObj* %v47140, %struct.ScmObj** %stackaddr$env-ref54266
%stackaddr$prim54267 = alloca %struct.ScmObj*, align 8
%_95k47367 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53297)
store volatile %struct.ScmObj* %_95k47367, %struct.ScmObj** %stackaddr$prim54267, align 8
%stackaddr$prim54268 = alloca %struct.ScmObj*, align 8
%current_45args53298 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53297)
store volatile %struct.ScmObj* %current_45args53298, %struct.ScmObj** %stackaddr$prim54268, align 8
%stackaddr$prim54269 = alloca %struct.ScmObj*, align 8
%cc47142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53298)
store volatile %struct.ScmObj* %cc47142, %struct.ScmObj** %stackaddr$prim54269, align 8
%ae50391 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54270 = alloca %struct.ScmObj*, align 8
%anf_45bind47284 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47141, %struct.ScmObj* %ae50391)
store volatile %struct.ScmObj* %anf_45bind47284, %struct.ScmObj** %stackaddr$prim54270, align 8
%stackaddr$prim54271 = alloca %struct.ScmObj*, align 8
%anf_45bind47285 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47284)
store volatile %struct.ScmObj* %anf_45bind47285, %struct.ScmObj** %stackaddr$prim54271, align 8
%truthy$cmp54272 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47285)
%cmp$cmp54272 = icmp eq i64 %truthy$cmp54272, 1
br i1 %cmp$cmp54272, label %truebranch$cmp54272, label %falsebranch$cmp54272
truebranch$cmp54272:
%ae50395 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50396 = call %struct.ScmObj* @const_init_false()
%argslist53300$k473650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54273 = alloca %struct.ScmObj*, align 8
%argslist53300$k473651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50396, %struct.ScmObj* %argslist53300$k473650)
store volatile %struct.ScmObj* %argslist53300$k473651, %struct.ScmObj** %stackaddr$prim54273, align 8
%stackaddr$prim54274 = alloca %struct.ScmObj*, align 8
%argslist53300$k473652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50395, %struct.ScmObj* %argslist53300$k473651)
store volatile %struct.ScmObj* %argslist53300$k473652, %struct.ScmObj** %stackaddr$prim54274, align 8
%clofunc54275 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47365)
musttail call tailcc void %clofunc54275(%struct.ScmObj* %k47365, %struct.ScmObj* %argslist53300$k473652)
ret void
falsebranch$cmp54272:
%ae50404 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54276 = alloca %struct.ScmObj*, align 8
%anf_45bind47286 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47141, %struct.ScmObj* %ae50404)
store volatile %struct.ScmObj* %anf_45bind47286, %struct.ScmObj** %stackaddr$prim54276, align 8
%stackaddr$prim54277 = alloca %struct.ScmObj*, align 8
%anf_45bind47287 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47286)
store volatile %struct.ScmObj* %anf_45bind47287, %struct.ScmObj** %stackaddr$prim54277, align 8
%stackaddr$prim54278 = alloca %struct.ScmObj*, align 8
%anf_45bind47288 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind47287, %struct.ScmObj* %v47140)
store volatile %struct.ScmObj* %anf_45bind47288, %struct.ScmObj** %stackaddr$prim54278, align 8
%truthy$cmp54279 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47288)
%cmp$cmp54279 = icmp eq i64 %truthy$cmp54279, 1
br i1 %cmp$cmp54279, label %truebranch$cmp54279, label %falsebranch$cmp54279
truebranch$cmp54279:
%ae50410 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54280 = alloca %struct.ScmObj*, align 8
%cpsprim47368 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47141, %struct.ScmObj* %ae50410)
store volatile %struct.ScmObj* %cpsprim47368, %struct.ScmObj** %stackaddr$prim54280, align 8
%ae50412 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53301$k473650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54281 = alloca %struct.ScmObj*, align 8
%argslist53301$k473651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47368, %struct.ScmObj* %argslist53301$k473650)
store volatile %struct.ScmObj* %argslist53301$k473651, %struct.ScmObj** %stackaddr$prim54281, align 8
%stackaddr$prim54282 = alloca %struct.ScmObj*, align 8
%argslist53301$k473652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50412, %struct.ScmObj* %argslist53301$k473651)
store volatile %struct.ScmObj* %argslist53301$k473652, %struct.ScmObj** %stackaddr$prim54282, align 8
%clofunc54283 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47365)
musttail call tailcc void %clofunc54283(%struct.ScmObj* %k47365, %struct.ScmObj* %argslist53301$k473652)
ret void
falsebranch$cmp54279:
%ae50423 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54284 = alloca %struct.ScmObj*, align 8
%anf_45bind47289 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47141, %struct.ScmObj* %ae50423)
store volatile %struct.ScmObj* %anf_45bind47289, %struct.ScmObj** %stackaddr$prim54284, align 8
%stackaddr$prim54285 = alloca %struct.ScmObj*, align 8
%anf_45bind47290 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47289)
store volatile %struct.ScmObj* %anf_45bind47290, %struct.ScmObj** %stackaddr$prim54285, align 8
%ae50426 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54286 = alloca %struct.ScmObj*, align 8
%_95047144 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47141, %struct.ScmObj* %ae50426, %struct.ScmObj* %anf_45bind47290)
store volatile %struct.ScmObj* %_95047144, %struct.ScmObj** %stackaddr$prim54286, align 8
%argslist53302$cc471420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54287 = alloca %struct.ScmObj*, align 8
%argslist53302$cc471421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47142, %struct.ScmObj* %argslist53302$cc471420)
store volatile %struct.ScmObj* %argslist53302$cc471421, %struct.ScmObj** %stackaddr$prim54287, align 8
%stackaddr$prim54288 = alloca %struct.ScmObj*, align 8
%argslist53302$cc471422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47365, %struct.ScmObj* %argslist53302$cc471421)
store volatile %struct.ScmObj* %argslist53302$cc471422, %struct.ScmObj** %stackaddr$prim54288, align 8
%clofunc54289 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47142)
musttail call tailcc void %clofunc54289(%struct.ScmObj* %cc47142, %struct.ScmObj* %argslist53302$cc471422)
ret void
}

define tailcc void @proc_clo$ae50283(%struct.ScmObj* %env$ae50283,%struct.ScmObj* %current_45args53303) {
%stackaddr$env-ref54290 = alloca %struct.ScmObj*, align 8
%lst47141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50283, i64 0)
store %struct.ScmObj* %lst47141, %struct.ScmObj** %stackaddr$env-ref54290
%stackaddr$env-ref54291 = alloca %struct.ScmObj*, align 8
%k47365 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50283, i64 1)
store %struct.ScmObj* %k47365, %struct.ScmObj** %stackaddr$env-ref54291
%stackaddr$env-ref54292 = alloca %struct.ScmObj*, align 8
%v47140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50283, i64 2)
store %struct.ScmObj* %v47140, %struct.ScmObj** %stackaddr$env-ref54292
%stackaddr$prim54293 = alloca %struct.ScmObj*, align 8
%_95k47367 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53303)
store volatile %struct.ScmObj* %_95k47367, %struct.ScmObj** %stackaddr$prim54293, align 8
%stackaddr$prim54294 = alloca %struct.ScmObj*, align 8
%current_45args53304 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53303)
store volatile %struct.ScmObj* %current_45args53304, %struct.ScmObj** %stackaddr$prim54294, align 8
%stackaddr$prim54295 = alloca %struct.ScmObj*, align 8
%cc47142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53304)
store volatile %struct.ScmObj* %cc47142, %struct.ScmObj** %stackaddr$prim54295, align 8
%ae50285 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54296 = alloca %struct.ScmObj*, align 8
%anf_45bind47284 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47141, %struct.ScmObj* %ae50285)
store volatile %struct.ScmObj* %anf_45bind47284, %struct.ScmObj** %stackaddr$prim54296, align 8
%stackaddr$prim54297 = alloca %struct.ScmObj*, align 8
%anf_45bind47285 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47284)
store volatile %struct.ScmObj* %anf_45bind47285, %struct.ScmObj** %stackaddr$prim54297, align 8
%truthy$cmp54298 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47285)
%cmp$cmp54298 = icmp eq i64 %truthy$cmp54298, 1
br i1 %cmp$cmp54298, label %truebranch$cmp54298, label %falsebranch$cmp54298
truebranch$cmp54298:
%ae50289 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50290 = call %struct.ScmObj* @const_init_false()
%argslist53306$k473650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54299 = alloca %struct.ScmObj*, align 8
%argslist53306$k473651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50290, %struct.ScmObj* %argslist53306$k473650)
store volatile %struct.ScmObj* %argslist53306$k473651, %struct.ScmObj** %stackaddr$prim54299, align 8
%stackaddr$prim54300 = alloca %struct.ScmObj*, align 8
%argslist53306$k473652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50289, %struct.ScmObj* %argslist53306$k473651)
store volatile %struct.ScmObj* %argslist53306$k473652, %struct.ScmObj** %stackaddr$prim54300, align 8
%clofunc54301 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47365)
musttail call tailcc void %clofunc54301(%struct.ScmObj* %k47365, %struct.ScmObj* %argslist53306$k473652)
ret void
falsebranch$cmp54298:
%ae50298 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54302 = alloca %struct.ScmObj*, align 8
%anf_45bind47286 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47141, %struct.ScmObj* %ae50298)
store volatile %struct.ScmObj* %anf_45bind47286, %struct.ScmObj** %stackaddr$prim54302, align 8
%stackaddr$prim54303 = alloca %struct.ScmObj*, align 8
%anf_45bind47287 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47286)
store volatile %struct.ScmObj* %anf_45bind47287, %struct.ScmObj** %stackaddr$prim54303, align 8
%stackaddr$prim54304 = alloca %struct.ScmObj*, align 8
%anf_45bind47288 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind47287, %struct.ScmObj* %v47140)
store volatile %struct.ScmObj* %anf_45bind47288, %struct.ScmObj** %stackaddr$prim54304, align 8
%truthy$cmp54305 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47288)
%cmp$cmp54305 = icmp eq i64 %truthy$cmp54305, 1
br i1 %cmp$cmp54305, label %truebranch$cmp54305, label %falsebranch$cmp54305
truebranch$cmp54305:
%ae50304 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54306 = alloca %struct.ScmObj*, align 8
%cpsprim47368 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47141, %struct.ScmObj* %ae50304)
store volatile %struct.ScmObj* %cpsprim47368, %struct.ScmObj** %stackaddr$prim54306, align 8
%ae50306 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53307$k473650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54307 = alloca %struct.ScmObj*, align 8
%argslist53307$k473651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47368, %struct.ScmObj* %argslist53307$k473650)
store volatile %struct.ScmObj* %argslist53307$k473651, %struct.ScmObj** %stackaddr$prim54307, align 8
%stackaddr$prim54308 = alloca %struct.ScmObj*, align 8
%argslist53307$k473652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50306, %struct.ScmObj* %argslist53307$k473651)
store volatile %struct.ScmObj* %argslist53307$k473652, %struct.ScmObj** %stackaddr$prim54308, align 8
%clofunc54309 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47365)
musttail call tailcc void %clofunc54309(%struct.ScmObj* %k47365, %struct.ScmObj* %argslist53307$k473652)
ret void
falsebranch$cmp54305:
%ae50317 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54310 = alloca %struct.ScmObj*, align 8
%anf_45bind47289 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47141, %struct.ScmObj* %ae50317)
store volatile %struct.ScmObj* %anf_45bind47289, %struct.ScmObj** %stackaddr$prim54310, align 8
%stackaddr$prim54311 = alloca %struct.ScmObj*, align 8
%anf_45bind47290 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47289)
store volatile %struct.ScmObj* %anf_45bind47290, %struct.ScmObj** %stackaddr$prim54311, align 8
%ae50320 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54312 = alloca %struct.ScmObj*, align 8
%_95047144 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47141, %struct.ScmObj* %ae50320, %struct.ScmObj* %anf_45bind47290)
store volatile %struct.ScmObj* %_95047144, %struct.ScmObj** %stackaddr$prim54312, align 8
%argslist53308$cc471420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54313 = alloca %struct.ScmObj*, align 8
%argslist53308$cc471421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47142, %struct.ScmObj* %argslist53308$cc471420)
store volatile %struct.ScmObj* %argslist53308$cc471421, %struct.ScmObj** %stackaddr$prim54313, align 8
%stackaddr$prim54314 = alloca %struct.ScmObj*, align 8
%argslist53308$cc471422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47365, %struct.ScmObj* %argslist53308$cc471421)
store volatile %struct.ScmObj* %argslist53308$cc471422, %struct.ScmObj** %stackaddr$prim54314, align 8
%clofunc54315 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47142)
musttail call tailcc void %clofunc54315(%struct.ScmObj* %cc47142, %struct.ScmObj* %argslist53308$cc471422)
ret void
}

define tailcc void @proc_clo$ae50268(%struct.ScmObj* %env$ae50268,%struct.ScmObj* %current_45args53310) {
%stackaddr$prim54316 = alloca %struct.ScmObj*, align 8
%k47369 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53310)
store volatile %struct.ScmObj* %k47369, %struct.ScmObj** %stackaddr$prim54316, align 8
%stackaddr$prim54317 = alloca %struct.ScmObj*, align 8
%current_45args53311 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53310)
store volatile %struct.ScmObj* %current_45args53311, %struct.ScmObj** %stackaddr$prim54317, align 8
%stackaddr$prim54318 = alloca %struct.ScmObj*, align 8
%u47143 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53311)
store volatile %struct.ScmObj* %u47143, %struct.ScmObj** %stackaddr$prim54318, align 8
%argslist53313$u471430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54319 = alloca %struct.ScmObj*, align 8
%argslist53313$u471431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u47143, %struct.ScmObj* %argslist53313$u471430)
store volatile %struct.ScmObj* %argslist53313$u471431, %struct.ScmObj** %stackaddr$prim54319, align 8
%stackaddr$prim54320 = alloca %struct.ScmObj*, align 8
%argslist53313$u471432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47369, %struct.ScmObj* %argslist53313$u471431)
store volatile %struct.ScmObj* %argslist53313$u471432, %struct.ScmObj** %stackaddr$prim54320, align 8
%clofunc54321 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u47143)
musttail call tailcc void %clofunc54321(%struct.ScmObj* %u47143, %struct.ScmObj* %argslist53313$u471432)
ret void
}

define tailcc void @proc_clo$ae49727(%struct.ScmObj* %env$ae49727,%struct.ScmObj* %current_45args53316) {
%stackaddr$prim54322 = alloca %struct.ScmObj*, align 8
%k47370 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53316)
store volatile %struct.ScmObj* %k47370, %struct.ScmObj** %stackaddr$prim54322, align 8
%stackaddr$prim54323 = alloca %struct.ScmObj*, align 8
%current_45args53317 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53316)
store volatile %struct.ScmObj* %current_45args53317, %struct.ScmObj** %stackaddr$prim54323, align 8
%stackaddr$prim54324 = alloca %struct.ScmObj*, align 8
%lst47147 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53317)
store volatile %struct.ScmObj* %lst47147, %struct.ScmObj** %stackaddr$prim54324, align 8
%stackaddr$prim54325 = alloca %struct.ScmObj*, align 8
%current_45args53318 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53317)
store volatile %struct.ScmObj* %current_45args53318, %struct.ScmObj** %stackaddr$prim54325, align 8
%stackaddr$prim54326 = alloca %struct.ScmObj*, align 8
%n47146 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53318)
store volatile %struct.ScmObj* %n47146, %struct.ScmObj** %stackaddr$prim54326, align 8
%ae49728 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54327 = alloca %struct.ScmObj*, align 8
%n47149 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49728, %struct.ScmObj* %n47146)
store volatile %struct.ScmObj* %n47149, %struct.ScmObj** %stackaddr$prim54327, align 8
%ae49730 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54328 = alloca %struct.ScmObj*, align 8
%lst47148 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49730, %struct.ScmObj* %lst47147)
store volatile %struct.ScmObj* %lst47148, %struct.ScmObj** %stackaddr$prim54328, align 8
%stackaddr$makeclosure54329 = alloca %struct.ScmObj*, align 8
%fptrToInt54330 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49732 to i64
%ae49732 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54330)
store volatile %struct.ScmObj* %ae49732, %struct.ScmObj** %stackaddr$makeclosure54329, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49732, %struct.ScmObj* %n47149, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49732, %struct.ScmObj* %lst47148, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49732, %struct.ScmObj* %k47370, i64 2)
%ae49733 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54331 = alloca %struct.ScmObj*, align 8
%fptrToInt54332 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49734 to i64
%ae49734 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54332)
store volatile %struct.ScmObj* %ae49734, %struct.ScmObj** %stackaddr$makeclosure54331, align 8
%argslist53338$ae497320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54333 = alloca %struct.ScmObj*, align 8
%argslist53338$ae497321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49734, %struct.ScmObj* %argslist53338$ae497320)
store volatile %struct.ScmObj* %argslist53338$ae497321, %struct.ScmObj** %stackaddr$prim54333, align 8
%stackaddr$prim54334 = alloca %struct.ScmObj*, align 8
%argslist53338$ae497322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49733, %struct.ScmObj* %argslist53338$ae497321)
store volatile %struct.ScmObj* %argslist53338$ae497322, %struct.ScmObj** %stackaddr$prim54334, align 8
%clofunc54335 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49732)
musttail call tailcc void %clofunc54335(%struct.ScmObj* %ae49732, %struct.ScmObj* %argslist53338$ae497322)
ret void
}

define tailcc void @proc_clo$ae49732(%struct.ScmObj* %env$ae49732,%struct.ScmObj* %current_45args53320) {
%stackaddr$env-ref54336 = alloca %struct.ScmObj*, align 8
%n47149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49732, i64 0)
store %struct.ScmObj* %n47149, %struct.ScmObj** %stackaddr$env-ref54336
%stackaddr$env-ref54337 = alloca %struct.ScmObj*, align 8
%lst47148 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49732, i64 1)
store %struct.ScmObj* %lst47148, %struct.ScmObj** %stackaddr$env-ref54337
%stackaddr$env-ref54338 = alloca %struct.ScmObj*, align 8
%k47370 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49732, i64 2)
store %struct.ScmObj* %k47370, %struct.ScmObj** %stackaddr$env-ref54338
%stackaddr$prim54339 = alloca %struct.ScmObj*, align 8
%_95k47371 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53320)
store volatile %struct.ScmObj* %_95k47371, %struct.ScmObj** %stackaddr$prim54339, align 8
%stackaddr$prim54340 = alloca %struct.ScmObj*, align 8
%current_45args53321 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53320)
store volatile %struct.ScmObj* %current_45args53321, %struct.ScmObj** %stackaddr$prim54340, align 8
%stackaddr$prim54341 = alloca %struct.ScmObj*, align 8
%anf_45bind47276 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53321)
store volatile %struct.ScmObj* %anf_45bind47276, %struct.ScmObj** %stackaddr$prim54341, align 8
%stackaddr$makeclosure54342 = alloca %struct.ScmObj*, align 8
%fptrToInt54343 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49748 to i64
%ae49748 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54343)
store volatile %struct.ScmObj* %ae49748, %struct.ScmObj** %stackaddr$makeclosure54342, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49748, %struct.ScmObj* %n47149, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49748, %struct.ScmObj* %lst47148, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49748, %struct.ScmObj* %k47370, i64 2)
%stackaddr$makeclosure54344 = alloca %struct.ScmObj*, align 8
%fptrToInt54345 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49749 to i64
%ae49749 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54345)
store volatile %struct.ScmObj* %ae49749, %struct.ScmObj** %stackaddr$makeclosure54344, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49749, %struct.ScmObj* %n47149, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49749, %struct.ScmObj* %lst47148, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49749, %struct.ScmObj* %k47370, i64 2)
%argslist53333$anf_45bind472760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54346 = alloca %struct.ScmObj*, align 8
%argslist53333$anf_45bind472761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49749, %struct.ScmObj* %argslist53333$anf_45bind472760)
store volatile %struct.ScmObj* %argslist53333$anf_45bind472761, %struct.ScmObj** %stackaddr$prim54346, align 8
%stackaddr$prim54347 = alloca %struct.ScmObj*, align 8
%argslist53333$anf_45bind472762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49748, %struct.ScmObj* %argslist53333$anf_45bind472761)
store volatile %struct.ScmObj* %argslist53333$anf_45bind472762, %struct.ScmObj** %stackaddr$prim54347, align 8
%clofunc54348 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47276)
musttail call tailcc void %clofunc54348(%struct.ScmObj* %anf_45bind47276, %struct.ScmObj* %argslist53333$anf_45bind472762)
ret void
}

define tailcc void @proc_clo$ae49748(%struct.ScmObj* %env$ae49748,%struct.ScmObj* %current_45args53323) {
%stackaddr$env-ref54349 = alloca %struct.ScmObj*, align 8
%n47149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49748, i64 0)
store %struct.ScmObj* %n47149, %struct.ScmObj** %stackaddr$env-ref54349
%stackaddr$env-ref54350 = alloca %struct.ScmObj*, align 8
%lst47148 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49748, i64 1)
store %struct.ScmObj* %lst47148, %struct.ScmObj** %stackaddr$env-ref54350
%stackaddr$env-ref54351 = alloca %struct.ScmObj*, align 8
%k47370 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49748, i64 2)
store %struct.ScmObj* %k47370, %struct.ScmObj** %stackaddr$env-ref54351
%stackaddr$prim54352 = alloca %struct.ScmObj*, align 8
%_95k47372 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53323)
store volatile %struct.ScmObj* %_95k47372, %struct.ScmObj** %stackaddr$prim54352, align 8
%stackaddr$prim54353 = alloca %struct.ScmObj*, align 8
%current_45args53324 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53323)
store volatile %struct.ScmObj* %current_45args53324, %struct.ScmObj** %stackaddr$prim54353, align 8
%stackaddr$prim54354 = alloca %struct.ScmObj*, align 8
%cc47150 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53324)
store volatile %struct.ScmObj* %cc47150, %struct.ScmObj** %stackaddr$prim54354, align 8
%ae49891 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54355 = alloca %struct.ScmObj*, align 8
%anf_45bind47277 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47149, %struct.ScmObj* %ae49891)
store volatile %struct.ScmObj* %anf_45bind47277, %struct.ScmObj** %stackaddr$prim54355, align 8
%ae49892 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54356 = alloca %struct.ScmObj*, align 8
%anf_45bind47278 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae49892, %struct.ScmObj* %anf_45bind47277)
store volatile %struct.ScmObj* %anf_45bind47278, %struct.ScmObj** %stackaddr$prim54356, align 8
%truthy$cmp54357 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47278)
%cmp$cmp54357 = icmp eq i64 %truthy$cmp54357, 1
br i1 %cmp$cmp54357, label %truebranch$cmp54357, label %falsebranch$cmp54357
truebranch$cmp54357:
%ae49896 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54358 = alloca %struct.ScmObj*, align 8
%cpsprim47373 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47148, %struct.ScmObj* %ae49896)
store volatile %struct.ScmObj* %cpsprim47373, %struct.ScmObj** %stackaddr$prim54358, align 8
%ae49898 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53326$k473700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54359 = alloca %struct.ScmObj*, align 8
%argslist53326$k473701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47373, %struct.ScmObj* %argslist53326$k473700)
store volatile %struct.ScmObj* %argslist53326$k473701, %struct.ScmObj** %stackaddr$prim54359, align 8
%stackaddr$prim54360 = alloca %struct.ScmObj*, align 8
%argslist53326$k473702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49898, %struct.ScmObj* %argslist53326$k473701)
store volatile %struct.ScmObj* %argslist53326$k473702, %struct.ScmObj** %stackaddr$prim54360, align 8
%clofunc54361 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47370)
musttail call tailcc void %clofunc54361(%struct.ScmObj* %k47370, %struct.ScmObj* %argslist53326$k473702)
ret void
falsebranch$cmp54357:
%ae49909 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54362 = alloca %struct.ScmObj*, align 8
%anf_45bind47279 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47148, %struct.ScmObj* %ae49909)
store volatile %struct.ScmObj* %anf_45bind47279, %struct.ScmObj** %stackaddr$prim54362, align 8
%stackaddr$prim54363 = alloca %struct.ScmObj*, align 8
%anf_45bind47280 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47279)
store volatile %struct.ScmObj* %anf_45bind47280, %struct.ScmObj** %stackaddr$prim54363, align 8
%ae49912 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54364 = alloca %struct.ScmObj*, align 8
%_95047153 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47148, %struct.ScmObj* %ae49912, %struct.ScmObj* %anf_45bind47280)
store volatile %struct.ScmObj* %_95047153, %struct.ScmObj** %stackaddr$prim54364, align 8
%ae49915 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54365 = alloca %struct.ScmObj*, align 8
%anf_45bind47281 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47149, %struct.ScmObj* %ae49915)
store volatile %struct.ScmObj* %anf_45bind47281, %struct.ScmObj** %stackaddr$prim54365, align 8
%ae49917 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54366 = alloca %struct.ScmObj*, align 8
%anf_45bind47282 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind47281, %struct.ScmObj* %ae49917)
store volatile %struct.ScmObj* %anf_45bind47282, %struct.ScmObj** %stackaddr$prim54366, align 8
%ae49919 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54367 = alloca %struct.ScmObj*, align 8
%_95147152 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n47149, %struct.ScmObj* %ae49919, %struct.ScmObj* %anf_45bind47282)
store volatile %struct.ScmObj* %_95147152, %struct.ScmObj** %stackaddr$prim54367, align 8
%argslist53327$cc471500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54368 = alloca %struct.ScmObj*, align 8
%argslist53327$cc471501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47150, %struct.ScmObj* %argslist53327$cc471500)
store volatile %struct.ScmObj* %argslist53327$cc471501, %struct.ScmObj** %stackaddr$prim54368, align 8
%stackaddr$prim54369 = alloca %struct.ScmObj*, align 8
%argslist53327$cc471502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47370, %struct.ScmObj* %argslist53327$cc471501)
store volatile %struct.ScmObj* %argslist53327$cc471502, %struct.ScmObj** %stackaddr$prim54369, align 8
%clofunc54370 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47150)
musttail call tailcc void %clofunc54370(%struct.ScmObj* %cc47150, %struct.ScmObj* %argslist53327$cc471502)
ret void
}

define tailcc void @proc_clo$ae49749(%struct.ScmObj* %env$ae49749,%struct.ScmObj* %current_45args53328) {
%stackaddr$env-ref54371 = alloca %struct.ScmObj*, align 8
%n47149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49749, i64 0)
store %struct.ScmObj* %n47149, %struct.ScmObj** %stackaddr$env-ref54371
%stackaddr$env-ref54372 = alloca %struct.ScmObj*, align 8
%lst47148 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49749, i64 1)
store %struct.ScmObj* %lst47148, %struct.ScmObj** %stackaddr$env-ref54372
%stackaddr$env-ref54373 = alloca %struct.ScmObj*, align 8
%k47370 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49749, i64 2)
store %struct.ScmObj* %k47370, %struct.ScmObj** %stackaddr$env-ref54373
%stackaddr$prim54374 = alloca %struct.ScmObj*, align 8
%_95k47372 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53328)
store volatile %struct.ScmObj* %_95k47372, %struct.ScmObj** %stackaddr$prim54374, align 8
%stackaddr$prim54375 = alloca %struct.ScmObj*, align 8
%current_45args53329 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53328)
store volatile %struct.ScmObj* %current_45args53329, %struct.ScmObj** %stackaddr$prim54375, align 8
%stackaddr$prim54376 = alloca %struct.ScmObj*, align 8
%cc47150 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53329)
store volatile %struct.ScmObj* %cc47150, %struct.ScmObj** %stackaddr$prim54376, align 8
%ae49751 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54377 = alloca %struct.ScmObj*, align 8
%anf_45bind47277 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47149, %struct.ScmObj* %ae49751)
store volatile %struct.ScmObj* %anf_45bind47277, %struct.ScmObj** %stackaddr$prim54377, align 8
%ae49752 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54378 = alloca %struct.ScmObj*, align 8
%anf_45bind47278 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae49752, %struct.ScmObj* %anf_45bind47277)
store volatile %struct.ScmObj* %anf_45bind47278, %struct.ScmObj** %stackaddr$prim54378, align 8
%truthy$cmp54379 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47278)
%cmp$cmp54379 = icmp eq i64 %truthy$cmp54379, 1
br i1 %cmp$cmp54379, label %truebranch$cmp54379, label %falsebranch$cmp54379
truebranch$cmp54379:
%ae49756 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54380 = alloca %struct.ScmObj*, align 8
%cpsprim47373 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47148, %struct.ScmObj* %ae49756)
store volatile %struct.ScmObj* %cpsprim47373, %struct.ScmObj** %stackaddr$prim54380, align 8
%ae49758 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53331$k473700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54381 = alloca %struct.ScmObj*, align 8
%argslist53331$k473701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47373, %struct.ScmObj* %argslist53331$k473700)
store volatile %struct.ScmObj* %argslist53331$k473701, %struct.ScmObj** %stackaddr$prim54381, align 8
%stackaddr$prim54382 = alloca %struct.ScmObj*, align 8
%argslist53331$k473702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49758, %struct.ScmObj* %argslist53331$k473701)
store volatile %struct.ScmObj* %argslist53331$k473702, %struct.ScmObj** %stackaddr$prim54382, align 8
%clofunc54383 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47370)
musttail call tailcc void %clofunc54383(%struct.ScmObj* %k47370, %struct.ScmObj* %argslist53331$k473702)
ret void
falsebranch$cmp54379:
%ae49769 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54384 = alloca %struct.ScmObj*, align 8
%anf_45bind47279 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47148, %struct.ScmObj* %ae49769)
store volatile %struct.ScmObj* %anf_45bind47279, %struct.ScmObj** %stackaddr$prim54384, align 8
%stackaddr$prim54385 = alloca %struct.ScmObj*, align 8
%anf_45bind47280 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47279)
store volatile %struct.ScmObj* %anf_45bind47280, %struct.ScmObj** %stackaddr$prim54385, align 8
%ae49772 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54386 = alloca %struct.ScmObj*, align 8
%_95047153 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47148, %struct.ScmObj* %ae49772, %struct.ScmObj* %anf_45bind47280)
store volatile %struct.ScmObj* %_95047153, %struct.ScmObj** %stackaddr$prim54386, align 8
%ae49775 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54387 = alloca %struct.ScmObj*, align 8
%anf_45bind47281 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47149, %struct.ScmObj* %ae49775)
store volatile %struct.ScmObj* %anf_45bind47281, %struct.ScmObj** %stackaddr$prim54387, align 8
%ae49777 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54388 = alloca %struct.ScmObj*, align 8
%anf_45bind47282 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind47281, %struct.ScmObj* %ae49777)
store volatile %struct.ScmObj* %anf_45bind47282, %struct.ScmObj** %stackaddr$prim54388, align 8
%ae49779 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54389 = alloca %struct.ScmObj*, align 8
%_95147152 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n47149, %struct.ScmObj* %ae49779, %struct.ScmObj* %anf_45bind47282)
store volatile %struct.ScmObj* %_95147152, %struct.ScmObj** %stackaddr$prim54389, align 8
%argslist53332$cc471500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54390 = alloca %struct.ScmObj*, align 8
%argslist53332$cc471501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47150, %struct.ScmObj* %argslist53332$cc471500)
store volatile %struct.ScmObj* %argslist53332$cc471501, %struct.ScmObj** %stackaddr$prim54390, align 8
%stackaddr$prim54391 = alloca %struct.ScmObj*, align 8
%argslist53332$cc471502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47370, %struct.ScmObj* %argslist53332$cc471501)
store volatile %struct.ScmObj* %argslist53332$cc471502, %struct.ScmObj** %stackaddr$prim54391, align 8
%clofunc54392 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47150)
musttail call tailcc void %clofunc54392(%struct.ScmObj* %cc47150, %struct.ScmObj* %argslist53332$cc471502)
ret void
}

define tailcc void @proc_clo$ae49734(%struct.ScmObj* %env$ae49734,%struct.ScmObj* %current_45args53334) {
%stackaddr$prim54393 = alloca %struct.ScmObj*, align 8
%k47374 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53334)
store volatile %struct.ScmObj* %k47374, %struct.ScmObj** %stackaddr$prim54393, align 8
%stackaddr$prim54394 = alloca %struct.ScmObj*, align 8
%current_45args53335 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53334)
store volatile %struct.ScmObj* %current_45args53335, %struct.ScmObj** %stackaddr$prim54394, align 8
%stackaddr$prim54395 = alloca %struct.ScmObj*, align 8
%u47151 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53335)
store volatile %struct.ScmObj* %u47151, %struct.ScmObj** %stackaddr$prim54395, align 8
%argslist53337$u471510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54396 = alloca %struct.ScmObj*, align 8
%argslist53337$u471511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u47151, %struct.ScmObj* %argslist53337$u471510)
store volatile %struct.ScmObj* %argslist53337$u471511, %struct.ScmObj** %stackaddr$prim54396, align 8
%stackaddr$prim54397 = alloca %struct.ScmObj*, align 8
%argslist53337$u471512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47374, %struct.ScmObj* %argslist53337$u471511)
store volatile %struct.ScmObj* %argslist53337$u471512, %struct.ScmObj** %stackaddr$prim54397, align 8
%clofunc54398 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u47151)
musttail call tailcc void %clofunc54398(%struct.ScmObj* %u47151, %struct.ScmObj* %argslist53337$u471512)
ret void
}

define tailcc void @proc_clo$ae49311(%struct.ScmObj* %env$ae49311,%struct.ScmObj* %current_45args53340) {
%stackaddr$prim54399 = alloca %struct.ScmObj*, align 8
%k47375 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53340)
store volatile %struct.ScmObj* %k47375, %struct.ScmObj** %stackaddr$prim54399, align 8
%stackaddr$prim54400 = alloca %struct.ScmObj*, align 8
%current_45args53341 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53340)
store volatile %struct.ScmObj* %current_45args53341, %struct.ScmObj** %stackaddr$prim54400, align 8
%stackaddr$prim54401 = alloca %struct.ScmObj*, align 8
%a47155 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53341)
store volatile %struct.ScmObj* %a47155, %struct.ScmObj** %stackaddr$prim54401, align 8
%ae49312 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54402 = alloca %struct.ScmObj*, align 8
%a47156 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49312, %struct.ScmObj* %a47155)
store volatile %struct.ScmObj* %a47156, %struct.ScmObj** %stackaddr$prim54402, align 8
%stackaddr$makeclosure54403 = alloca %struct.ScmObj*, align 8
%fptrToInt54404 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49314 to i64
%ae49314 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54404)
store volatile %struct.ScmObj* %ae49314, %struct.ScmObj** %stackaddr$makeclosure54403, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49314, %struct.ScmObj* %a47156, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49314, %struct.ScmObj* %k47375, i64 1)
%ae49315 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54405 = alloca %struct.ScmObj*, align 8
%fptrToInt54406 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49316 to i64
%ae49316 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54406)
store volatile %struct.ScmObj* %ae49316, %struct.ScmObj** %stackaddr$makeclosure54405, align 8
%argslist53363$ae493140 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54407 = alloca %struct.ScmObj*, align 8
%argslist53363$ae493141 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49316, %struct.ScmObj* %argslist53363$ae493140)
store volatile %struct.ScmObj* %argslist53363$ae493141, %struct.ScmObj** %stackaddr$prim54407, align 8
%stackaddr$prim54408 = alloca %struct.ScmObj*, align 8
%argslist53363$ae493142 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49315, %struct.ScmObj* %argslist53363$ae493141)
store volatile %struct.ScmObj* %argslist53363$ae493142, %struct.ScmObj** %stackaddr$prim54408, align 8
%clofunc54409 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49314)
musttail call tailcc void %clofunc54409(%struct.ScmObj* %ae49314, %struct.ScmObj* %argslist53363$ae493142)
ret void
}

define tailcc void @proc_clo$ae49314(%struct.ScmObj* %env$ae49314,%struct.ScmObj* %current_45args53343) {
%stackaddr$env-ref54410 = alloca %struct.ScmObj*, align 8
%a47156 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49314, i64 0)
store %struct.ScmObj* %a47156, %struct.ScmObj** %stackaddr$env-ref54410
%stackaddr$env-ref54411 = alloca %struct.ScmObj*, align 8
%k47375 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49314, i64 1)
store %struct.ScmObj* %k47375, %struct.ScmObj** %stackaddr$env-ref54411
%stackaddr$prim54412 = alloca %struct.ScmObj*, align 8
%_95k47376 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53343)
store volatile %struct.ScmObj* %_95k47376, %struct.ScmObj** %stackaddr$prim54412, align 8
%stackaddr$prim54413 = alloca %struct.ScmObj*, align 8
%current_45args53344 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53343)
store volatile %struct.ScmObj* %current_45args53344, %struct.ScmObj** %stackaddr$prim54413, align 8
%stackaddr$prim54414 = alloca %struct.ScmObj*, align 8
%anf_45bind47268 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53344)
store volatile %struct.ScmObj* %anf_45bind47268, %struct.ScmObj** %stackaddr$prim54414, align 8
%stackaddr$makeclosure54415 = alloca %struct.ScmObj*, align 8
%fptrToInt54416 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49333 to i64
%ae49333 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54416)
store volatile %struct.ScmObj* %ae49333, %struct.ScmObj** %stackaddr$makeclosure54415, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49333, %struct.ScmObj* %a47156, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49333, %struct.ScmObj* %k47375, i64 1)
%stackaddr$makeclosure54417 = alloca %struct.ScmObj*, align 8
%fptrToInt54418 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49334 to i64
%ae49334 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54418)
store volatile %struct.ScmObj* %ae49334, %struct.ScmObj** %stackaddr$makeclosure54417, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49334, %struct.ScmObj* %a47156, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49334, %struct.ScmObj* %k47375, i64 1)
%argslist53358$anf_45bind472680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54419 = alloca %struct.ScmObj*, align 8
%argslist53358$anf_45bind472681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49334, %struct.ScmObj* %argslist53358$anf_45bind472680)
store volatile %struct.ScmObj* %argslist53358$anf_45bind472681, %struct.ScmObj** %stackaddr$prim54419, align 8
%stackaddr$prim54420 = alloca %struct.ScmObj*, align 8
%argslist53358$anf_45bind472682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49333, %struct.ScmObj* %argslist53358$anf_45bind472681)
store volatile %struct.ScmObj* %argslist53358$anf_45bind472682, %struct.ScmObj** %stackaddr$prim54420, align 8
%clofunc54421 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47268)
musttail call tailcc void %clofunc54421(%struct.ScmObj* %anf_45bind47268, %struct.ScmObj* %argslist53358$anf_45bind472682)
ret void
}

define tailcc void @proc_clo$ae49333(%struct.ScmObj* %env$ae49333,%struct.ScmObj* %current_45args53346) {
%stackaddr$env-ref54422 = alloca %struct.ScmObj*, align 8
%a47156 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49333, i64 0)
store %struct.ScmObj* %a47156, %struct.ScmObj** %stackaddr$env-ref54422
%stackaddr$env-ref54423 = alloca %struct.ScmObj*, align 8
%k47375 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49333, i64 1)
store %struct.ScmObj* %k47375, %struct.ScmObj** %stackaddr$env-ref54423
%stackaddr$prim54424 = alloca %struct.ScmObj*, align 8
%_95k47377 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53346)
store volatile %struct.ScmObj* %_95k47377, %struct.ScmObj** %stackaddr$prim54424, align 8
%stackaddr$prim54425 = alloca %struct.ScmObj*, align 8
%current_45args53347 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53346)
store volatile %struct.ScmObj* %current_45args53347, %struct.ScmObj** %stackaddr$prim54425, align 8
%stackaddr$prim54426 = alloca %struct.ScmObj*, align 8
%cc47157 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53347)
store volatile %struct.ScmObj* %cc47157, %struct.ScmObj** %stackaddr$prim54426, align 8
%ae49449 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54427 = alloca %struct.ScmObj*, align 8
%anf_45bind47269 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47156, %struct.ScmObj* %ae49449)
store volatile %struct.ScmObj* %anf_45bind47269, %struct.ScmObj** %stackaddr$prim54427, align 8
%stackaddr$prim54428 = alloca %struct.ScmObj*, align 8
%anf_45bind47270 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47269)
store volatile %struct.ScmObj* %anf_45bind47270, %struct.ScmObj** %stackaddr$prim54428, align 8
%truthy$cmp54429 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47270)
%cmp$cmp54429 = icmp eq i64 %truthy$cmp54429, 1
br i1 %cmp$cmp54429, label %truebranch$cmp54429, label %falsebranch$cmp54429
truebranch$cmp54429:
%ae49453 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49454 = call %struct.ScmObj* @const_init_true()
%argslist53349$k473750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54430 = alloca %struct.ScmObj*, align 8
%argslist53349$k473751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49454, %struct.ScmObj* %argslist53349$k473750)
store volatile %struct.ScmObj* %argslist53349$k473751, %struct.ScmObj** %stackaddr$prim54430, align 8
%stackaddr$prim54431 = alloca %struct.ScmObj*, align 8
%argslist53349$k473752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49453, %struct.ScmObj* %argslist53349$k473751)
store volatile %struct.ScmObj* %argslist53349$k473752, %struct.ScmObj** %stackaddr$prim54431, align 8
%clofunc54432 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47375)
musttail call tailcc void %clofunc54432(%struct.ScmObj* %k47375, %struct.ScmObj* %argslist53349$k473752)
ret void
falsebranch$cmp54429:
%ae49462 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54433 = alloca %struct.ScmObj*, align 8
%anf_45bind47271 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47156, %struct.ScmObj* %ae49462)
store volatile %struct.ScmObj* %anf_45bind47271, %struct.ScmObj** %stackaddr$prim54433, align 8
%stackaddr$prim54434 = alloca %struct.ScmObj*, align 8
%anf_45bind47272 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind47271)
store volatile %struct.ScmObj* %anf_45bind47272, %struct.ScmObj** %stackaddr$prim54434, align 8
%truthy$cmp54435 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47272)
%cmp$cmp54435 = icmp eq i64 %truthy$cmp54435, 1
br i1 %cmp$cmp54435, label %truebranch$cmp54435, label %falsebranch$cmp54435
truebranch$cmp54435:
%ae49466 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54436 = alloca %struct.ScmObj*, align 8
%anf_45bind47273 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47156, %struct.ScmObj* %ae49466)
store volatile %struct.ScmObj* %anf_45bind47273, %struct.ScmObj** %stackaddr$prim54436, align 8
%stackaddr$prim54437 = alloca %struct.ScmObj*, align 8
%b47159 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47273)
store volatile %struct.ScmObj* %b47159, %struct.ScmObj** %stackaddr$prim54437, align 8
%ae49469 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54438 = alloca %struct.ScmObj*, align 8
%anf_45bind47274 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47156, %struct.ScmObj* %ae49469)
store volatile %struct.ScmObj* %anf_45bind47274, %struct.ScmObj** %stackaddr$prim54438, align 8
%stackaddr$prim54439 = alloca %struct.ScmObj*, align 8
%anf_45bind47275 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47274)
store volatile %struct.ScmObj* %anf_45bind47275, %struct.ScmObj** %stackaddr$prim54439, align 8
%ae49472 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54440 = alloca %struct.ScmObj*, align 8
%_95047160 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a47156, %struct.ScmObj* %ae49472, %struct.ScmObj* %anf_45bind47275)
store volatile %struct.ScmObj* %_95047160, %struct.ScmObj** %stackaddr$prim54440, align 8
%argslist53350$cc471570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54441 = alloca %struct.ScmObj*, align 8
%argslist53350$cc471571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47157, %struct.ScmObj* %argslist53350$cc471570)
store volatile %struct.ScmObj* %argslist53350$cc471571, %struct.ScmObj** %stackaddr$prim54441, align 8
%stackaddr$prim54442 = alloca %struct.ScmObj*, align 8
%argslist53350$cc471572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47375, %struct.ScmObj* %argslist53350$cc471571)
store volatile %struct.ScmObj* %argslist53350$cc471572, %struct.ScmObj** %stackaddr$prim54442, align 8
%clofunc54443 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47157)
musttail call tailcc void %clofunc54443(%struct.ScmObj* %cc47157, %struct.ScmObj* %argslist53350$cc471572)
ret void
falsebranch$cmp54435:
%ae49505 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49506 = call %struct.ScmObj* @const_init_false()
%argslist53351$k473750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54444 = alloca %struct.ScmObj*, align 8
%argslist53351$k473751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49506, %struct.ScmObj* %argslist53351$k473750)
store volatile %struct.ScmObj* %argslist53351$k473751, %struct.ScmObj** %stackaddr$prim54444, align 8
%stackaddr$prim54445 = alloca %struct.ScmObj*, align 8
%argslist53351$k473752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49505, %struct.ScmObj* %argslist53351$k473751)
store volatile %struct.ScmObj* %argslist53351$k473752, %struct.ScmObj** %stackaddr$prim54445, align 8
%clofunc54446 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47375)
musttail call tailcc void %clofunc54446(%struct.ScmObj* %k47375, %struct.ScmObj* %argslist53351$k473752)
ret void
}

define tailcc void @proc_clo$ae49334(%struct.ScmObj* %env$ae49334,%struct.ScmObj* %current_45args53352) {
%stackaddr$env-ref54447 = alloca %struct.ScmObj*, align 8
%a47156 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49334, i64 0)
store %struct.ScmObj* %a47156, %struct.ScmObj** %stackaddr$env-ref54447
%stackaddr$env-ref54448 = alloca %struct.ScmObj*, align 8
%k47375 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49334, i64 1)
store %struct.ScmObj* %k47375, %struct.ScmObj** %stackaddr$env-ref54448
%stackaddr$prim54449 = alloca %struct.ScmObj*, align 8
%_95k47377 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53352)
store volatile %struct.ScmObj* %_95k47377, %struct.ScmObj** %stackaddr$prim54449, align 8
%stackaddr$prim54450 = alloca %struct.ScmObj*, align 8
%current_45args53353 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53352)
store volatile %struct.ScmObj* %current_45args53353, %struct.ScmObj** %stackaddr$prim54450, align 8
%stackaddr$prim54451 = alloca %struct.ScmObj*, align 8
%cc47157 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53353)
store volatile %struct.ScmObj* %cc47157, %struct.ScmObj** %stackaddr$prim54451, align 8
%ae49336 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54452 = alloca %struct.ScmObj*, align 8
%anf_45bind47269 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47156, %struct.ScmObj* %ae49336)
store volatile %struct.ScmObj* %anf_45bind47269, %struct.ScmObj** %stackaddr$prim54452, align 8
%stackaddr$prim54453 = alloca %struct.ScmObj*, align 8
%anf_45bind47270 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47269)
store volatile %struct.ScmObj* %anf_45bind47270, %struct.ScmObj** %stackaddr$prim54453, align 8
%truthy$cmp54454 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47270)
%cmp$cmp54454 = icmp eq i64 %truthy$cmp54454, 1
br i1 %cmp$cmp54454, label %truebranch$cmp54454, label %falsebranch$cmp54454
truebranch$cmp54454:
%ae49340 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49341 = call %struct.ScmObj* @const_init_true()
%argslist53355$k473750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54455 = alloca %struct.ScmObj*, align 8
%argslist53355$k473751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49341, %struct.ScmObj* %argslist53355$k473750)
store volatile %struct.ScmObj* %argslist53355$k473751, %struct.ScmObj** %stackaddr$prim54455, align 8
%stackaddr$prim54456 = alloca %struct.ScmObj*, align 8
%argslist53355$k473752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49340, %struct.ScmObj* %argslist53355$k473751)
store volatile %struct.ScmObj* %argslist53355$k473752, %struct.ScmObj** %stackaddr$prim54456, align 8
%clofunc54457 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47375)
musttail call tailcc void %clofunc54457(%struct.ScmObj* %k47375, %struct.ScmObj* %argslist53355$k473752)
ret void
falsebranch$cmp54454:
%ae49349 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54458 = alloca %struct.ScmObj*, align 8
%anf_45bind47271 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47156, %struct.ScmObj* %ae49349)
store volatile %struct.ScmObj* %anf_45bind47271, %struct.ScmObj** %stackaddr$prim54458, align 8
%stackaddr$prim54459 = alloca %struct.ScmObj*, align 8
%anf_45bind47272 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind47271)
store volatile %struct.ScmObj* %anf_45bind47272, %struct.ScmObj** %stackaddr$prim54459, align 8
%truthy$cmp54460 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47272)
%cmp$cmp54460 = icmp eq i64 %truthy$cmp54460, 1
br i1 %cmp$cmp54460, label %truebranch$cmp54460, label %falsebranch$cmp54460
truebranch$cmp54460:
%ae49353 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54461 = alloca %struct.ScmObj*, align 8
%anf_45bind47273 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47156, %struct.ScmObj* %ae49353)
store volatile %struct.ScmObj* %anf_45bind47273, %struct.ScmObj** %stackaddr$prim54461, align 8
%stackaddr$prim54462 = alloca %struct.ScmObj*, align 8
%b47159 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47273)
store volatile %struct.ScmObj* %b47159, %struct.ScmObj** %stackaddr$prim54462, align 8
%ae49356 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54463 = alloca %struct.ScmObj*, align 8
%anf_45bind47274 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47156, %struct.ScmObj* %ae49356)
store volatile %struct.ScmObj* %anf_45bind47274, %struct.ScmObj** %stackaddr$prim54463, align 8
%stackaddr$prim54464 = alloca %struct.ScmObj*, align 8
%anf_45bind47275 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47274)
store volatile %struct.ScmObj* %anf_45bind47275, %struct.ScmObj** %stackaddr$prim54464, align 8
%ae49359 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54465 = alloca %struct.ScmObj*, align 8
%_95047160 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a47156, %struct.ScmObj* %ae49359, %struct.ScmObj* %anf_45bind47275)
store volatile %struct.ScmObj* %_95047160, %struct.ScmObj** %stackaddr$prim54465, align 8
%argslist53356$cc471570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54466 = alloca %struct.ScmObj*, align 8
%argslist53356$cc471571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47157, %struct.ScmObj* %argslist53356$cc471570)
store volatile %struct.ScmObj* %argslist53356$cc471571, %struct.ScmObj** %stackaddr$prim54466, align 8
%stackaddr$prim54467 = alloca %struct.ScmObj*, align 8
%argslist53356$cc471572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47375, %struct.ScmObj* %argslist53356$cc471571)
store volatile %struct.ScmObj* %argslist53356$cc471572, %struct.ScmObj** %stackaddr$prim54467, align 8
%clofunc54468 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47157)
musttail call tailcc void %clofunc54468(%struct.ScmObj* %cc47157, %struct.ScmObj* %argslist53356$cc471572)
ret void
falsebranch$cmp54460:
%ae49392 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49393 = call %struct.ScmObj* @const_init_false()
%argslist53357$k473750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54469 = alloca %struct.ScmObj*, align 8
%argslist53357$k473751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49393, %struct.ScmObj* %argslist53357$k473750)
store volatile %struct.ScmObj* %argslist53357$k473751, %struct.ScmObj** %stackaddr$prim54469, align 8
%stackaddr$prim54470 = alloca %struct.ScmObj*, align 8
%argslist53357$k473752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49392, %struct.ScmObj* %argslist53357$k473751)
store volatile %struct.ScmObj* %argslist53357$k473752, %struct.ScmObj** %stackaddr$prim54470, align 8
%clofunc54471 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47375)
musttail call tailcc void %clofunc54471(%struct.ScmObj* %k47375, %struct.ScmObj* %argslist53357$k473752)
ret void
}

define tailcc void @proc_clo$ae49316(%struct.ScmObj* %env$ae49316,%struct.ScmObj* %current_45args53359) {
%stackaddr$prim54472 = alloca %struct.ScmObj*, align 8
%k47378 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53359)
store volatile %struct.ScmObj* %k47378, %struct.ScmObj** %stackaddr$prim54472, align 8
%stackaddr$prim54473 = alloca %struct.ScmObj*, align 8
%current_45args53360 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53359)
store volatile %struct.ScmObj* %current_45args53360, %struct.ScmObj** %stackaddr$prim54473, align 8
%stackaddr$prim54474 = alloca %struct.ScmObj*, align 8
%k47158 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53360)
store volatile %struct.ScmObj* %k47158, %struct.ScmObj** %stackaddr$prim54474, align 8
%ae49318 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53362$k473780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54475 = alloca %struct.ScmObj*, align 8
%argslist53362$k473781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47158, %struct.ScmObj* %argslist53362$k473780)
store volatile %struct.ScmObj* %argslist53362$k473781, %struct.ScmObj** %stackaddr$prim54475, align 8
%stackaddr$prim54476 = alloca %struct.ScmObj*, align 8
%argslist53362$k473782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49318, %struct.ScmObj* %argslist53362$k473781)
store volatile %struct.ScmObj* %argslist53362$k473782, %struct.ScmObj** %stackaddr$prim54476, align 8
%clofunc54477 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47378)
musttail call tailcc void %clofunc54477(%struct.ScmObj* %k47378, %struct.ScmObj* %argslist53362$k473782)
ret void
}

define tailcc void @proc_clo$ae49239(%struct.ScmObj* %env$ae49239,%struct.ScmObj* %current_45args53365) {
%stackaddr$env-ref54478 = alloca %struct.ScmObj*, align 8
%_37append47162 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49239, i64 0)
store %struct.ScmObj* %_37append47162, %struct.ScmObj** %stackaddr$env-ref54478
%stackaddr$prim54479 = alloca %struct.ScmObj*, align 8
%k47379 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53365)
store volatile %struct.ScmObj* %k47379, %struct.ScmObj** %stackaddr$prim54479, align 8
%stackaddr$prim54480 = alloca %struct.ScmObj*, align 8
%current_45args53366 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53365)
store volatile %struct.ScmObj* %current_45args53366, %struct.ScmObj** %stackaddr$prim54480, align 8
%stackaddr$prim54481 = alloca %struct.ScmObj*, align 8
%ls047165 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53366)
store volatile %struct.ScmObj* %ls047165, %struct.ScmObj** %stackaddr$prim54481, align 8
%stackaddr$prim54482 = alloca %struct.ScmObj*, align 8
%current_45args53367 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53366)
store volatile %struct.ScmObj* %current_45args53367, %struct.ScmObj** %stackaddr$prim54482, align 8
%stackaddr$prim54483 = alloca %struct.ScmObj*, align 8
%ls147164 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53367)
store volatile %struct.ScmObj* %ls147164, %struct.ScmObj** %stackaddr$prim54483, align 8
%stackaddr$prim54484 = alloca %struct.ScmObj*, align 8
%anf_45bind47262 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls047165)
store volatile %struct.ScmObj* %anf_45bind47262, %struct.ScmObj** %stackaddr$prim54484, align 8
%truthy$cmp54485 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47262)
%cmp$cmp54485 = icmp eq i64 %truthy$cmp54485, 1
br i1 %cmp$cmp54485, label %truebranch$cmp54485, label %falsebranch$cmp54485
truebranch$cmp54485:
%ae49243 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53369$k473790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54486 = alloca %struct.ScmObj*, align 8
%argslist53369$k473791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls147164, %struct.ScmObj* %argslist53369$k473790)
store volatile %struct.ScmObj* %argslist53369$k473791, %struct.ScmObj** %stackaddr$prim54486, align 8
%stackaddr$prim54487 = alloca %struct.ScmObj*, align 8
%argslist53369$k473792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49243, %struct.ScmObj* %argslist53369$k473791)
store volatile %struct.ScmObj* %argslist53369$k473792, %struct.ScmObj** %stackaddr$prim54487, align 8
%clofunc54488 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47379)
musttail call tailcc void %clofunc54488(%struct.ScmObj* %k47379, %struct.ScmObj* %argslist53369$k473792)
ret void
falsebranch$cmp54485:
%stackaddr$prim54489 = alloca %struct.ScmObj*, align 8
%anf_45bind47263 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls047165)
store volatile %struct.ScmObj* %anf_45bind47263, %struct.ScmObj** %stackaddr$prim54489, align 8
%ae49250 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54490 = alloca %struct.ScmObj*, align 8
%anf_45bind47264 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append47162, %struct.ScmObj* %ae49250)
store volatile %struct.ScmObj* %anf_45bind47264, %struct.ScmObj** %stackaddr$prim54490, align 8
%stackaddr$prim54491 = alloca %struct.ScmObj*, align 8
%anf_45bind47265 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls047165)
store volatile %struct.ScmObj* %anf_45bind47265, %struct.ScmObj** %stackaddr$prim54491, align 8
%stackaddr$makeclosure54492 = alloca %struct.ScmObj*, align 8
%fptrToInt54493 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49253 to i64
%ae49253 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54493)
store volatile %struct.ScmObj* %ae49253, %struct.ScmObj** %stackaddr$makeclosure54492, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49253, %struct.ScmObj* %k47379, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49253, %struct.ScmObj* %anf_45bind47263, i64 1)
%argslist53374$anf_45bind472640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54494 = alloca %struct.ScmObj*, align 8
%argslist53374$anf_45bind472641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls147164, %struct.ScmObj* %argslist53374$anf_45bind472640)
store volatile %struct.ScmObj* %argslist53374$anf_45bind472641, %struct.ScmObj** %stackaddr$prim54494, align 8
%stackaddr$prim54495 = alloca %struct.ScmObj*, align 8
%argslist53374$anf_45bind472642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47265, %struct.ScmObj* %argslist53374$anf_45bind472641)
store volatile %struct.ScmObj* %argslist53374$anf_45bind472642, %struct.ScmObj** %stackaddr$prim54495, align 8
%stackaddr$prim54496 = alloca %struct.ScmObj*, align 8
%argslist53374$anf_45bind472643 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49253, %struct.ScmObj* %argslist53374$anf_45bind472642)
store volatile %struct.ScmObj* %argslist53374$anf_45bind472643, %struct.ScmObj** %stackaddr$prim54496, align 8
%clofunc54497 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47264)
musttail call tailcc void %clofunc54497(%struct.ScmObj* %anf_45bind47264, %struct.ScmObj* %argslist53374$anf_45bind472643)
ret void
}

define tailcc void @proc_clo$ae49253(%struct.ScmObj* %env$ae49253,%struct.ScmObj* %current_45args53370) {
%stackaddr$env-ref54498 = alloca %struct.ScmObj*, align 8
%k47379 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49253, i64 0)
store %struct.ScmObj* %k47379, %struct.ScmObj** %stackaddr$env-ref54498
%stackaddr$env-ref54499 = alloca %struct.ScmObj*, align 8
%anf_45bind47263 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49253, i64 1)
store %struct.ScmObj* %anf_45bind47263, %struct.ScmObj** %stackaddr$env-ref54499
%stackaddr$prim54500 = alloca %struct.ScmObj*, align 8
%_95k47380 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53370)
store volatile %struct.ScmObj* %_95k47380, %struct.ScmObj** %stackaddr$prim54500, align 8
%stackaddr$prim54501 = alloca %struct.ScmObj*, align 8
%current_45args53371 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53370)
store volatile %struct.ScmObj* %current_45args53371, %struct.ScmObj** %stackaddr$prim54501, align 8
%stackaddr$prim54502 = alloca %struct.ScmObj*, align 8
%anf_45bind47266 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53371)
store volatile %struct.ScmObj* %anf_45bind47266, %struct.ScmObj** %stackaddr$prim54502, align 8
%stackaddr$prim54503 = alloca %struct.ScmObj*, align 8
%cpsprim47381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47263, %struct.ScmObj* %anf_45bind47266)
store volatile %struct.ScmObj* %cpsprim47381, %struct.ScmObj** %stackaddr$prim54503, align 8
%ae49259 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53373$k473790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54504 = alloca %struct.ScmObj*, align 8
%argslist53373$k473791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47381, %struct.ScmObj* %argslist53373$k473790)
store volatile %struct.ScmObj* %argslist53373$k473791, %struct.ScmObj** %stackaddr$prim54504, align 8
%stackaddr$prim54505 = alloca %struct.ScmObj*, align 8
%argslist53373$k473792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49259, %struct.ScmObj* %argslist53373$k473791)
store volatile %struct.ScmObj* %argslist53373$k473792, %struct.ScmObj** %stackaddr$prim54505, align 8
%clofunc54506 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47379)
musttail call tailcc void %clofunc54506(%struct.ScmObj* %k47379, %struct.ScmObj* %argslist53373$k473792)
ret void
}

define tailcc void @proc_clo$ae49213(%struct.ScmObj* %env$ae49213,%struct.ScmObj* %current_45args53376) {
%stackaddr$prim54507 = alloca %struct.ScmObj*, align 8
%k47382 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53376)
store volatile %struct.ScmObj* %k47382, %struct.ScmObj** %stackaddr$prim54507, align 8
%stackaddr$prim54508 = alloca %struct.ScmObj*, align 8
%current_45args53377 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53376)
store volatile %struct.ScmObj* %current_45args53377, %struct.ScmObj** %stackaddr$prim54508, align 8
%stackaddr$prim54509 = alloca %struct.ScmObj*, align 8
%a47168 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53377)
store volatile %struct.ScmObj* %a47168, %struct.ScmObj** %stackaddr$prim54509, align 8
%stackaddr$prim54510 = alloca %struct.ScmObj*, align 8
%current_45args53378 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53377)
store volatile %struct.ScmObj* %current_45args53378, %struct.ScmObj** %stackaddr$prim54510, align 8
%stackaddr$prim54511 = alloca %struct.ScmObj*, align 8
%b47167 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53378)
store volatile %struct.ScmObj* %b47167, %struct.ScmObj** %stackaddr$prim54511, align 8
%stackaddr$prim54512 = alloca %struct.ScmObj*, align 8
%anf_45bind47261 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a47168, %struct.ScmObj* %b47167)
store volatile %struct.ScmObj* %anf_45bind47261, %struct.ScmObj** %stackaddr$prim54512, align 8
%stackaddr$prim54513 = alloca %struct.ScmObj*, align 8
%cpsprim47383 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind47261)
store volatile %struct.ScmObj* %cpsprim47383, %struct.ScmObj** %stackaddr$prim54513, align 8
%ae49218 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53380$k473820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54514 = alloca %struct.ScmObj*, align 8
%argslist53380$k473821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47383, %struct.ScmObj* %argslist53380$k473820)
store volatile %struct.ScmObj* %argslist53380$k473821, %struct.ScmObj** %stackaddr$prim54514, align 8
%stackaddr$prim54515 = alloca %struct.ScmObj*, align 8
%argslist53380$k473822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49218, %struct.ScmObj* %argslist53380$k473821)
store volatile %struct.ScmObj* %argslist53380$k473822, %struct.ScmObj** %stackaddr$prim54515, align 8
%clofunc54516 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47382)
musttail call tailcc void %clofunc54516(%struct.ScmObj* %k47382, %struct.ScmObj* %argslist53380$k473822)
ret void
}

define tailcc void @proc_clo$ae49189(%struct.ScmObj* %env$ae49189,%struct.ScmObj* %current_45args53382) {
%stackaddr$prim54517 = alloca %struct.ScmObj*, align 8
%k47384 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53382)
store volatile %struct.ScmObj* %k47384, %struct.ScmObj** %stackaddr$prim54517, align 8
%stackaddr$prim54518 = alloca %struct.ScmObj*, align 8
%current_45args53383 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53382)
store volatile %struct.ScmObj* %current_45args53383, %struct.ScmObj** %stackaddr$prim54518, align 8
%stackaddr$prim54519 = alloca %struct.ScmObj*, align 8
%a47171 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53383)
store volatile %struct.ScmObj* %a47171, %struct.ScmObj** %stackaddr$prim54519, align 8
%stackaddr$prim54520 = alloca %struct.ScmObj*, align 8
%current_45args53384 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53383)
store volatile %struct.ScmObj* %current_45args53384, %struct.ScmObj** %stackaddr$prim54520, align 8
%stackaddr$prim54521 = alloca %struct.ScmObj*, align 8
%b47170 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53384)
store volatile %struct.ScmObj* %b47170, %struct.ScmObj** %stackaddr$prim54521, align 8
%stackaddr$prim54522 = alloca %struct.ScmObj*, align 8
%anf_45bind47260 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a47171, %struct.ScmObj* %b47170)
store volatile %struct.ScmObj* %anf_45bind47260, %struct.ScmObj** %stackaddr$prim54522, align 8
%stackaddr$prim54523 = alloca %struct.ScmObj*, align 8
%cpsprim47385 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind47260)
store volatile %struct.ScmObj* %cpsprim47385, %struct.ScmObj** %stackaddr$prim54523, align 8
%ae49194 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53386$k473840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54524 = alloca %struct.ScmObj*, align 8
%argslist53386$k473841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47385, %struct.ScmObj* %argslist53386$k473840)
store volatile %struct.ScmObj* %argslist53386$k473841, %struct.ScmObj** %stackaddr$prim54524, align 8
%stackaddr$prim54525 = alloca %struct.ScmObj*, align 8
%argslist53386$k473842 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49194, %struct.ScmObj* %argslist53386$k473841)
store volatile %struct.ScmObj* %argslist53386$k473842, %struct.ScmObj** %stackaddr$prim54525, align 8
%clofunc54526 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47384)
musttail call tailcc void %clofunc54526(%struct.ScmObj* %k47384, %struct.ScmObj* %argslist53386$k473842)
ret void
}

define tailcc void @proc_clo$ae48795(%struct.ScmObj* %env$ae48795,%struct.ScmObj* %current_45args53389) {
%stackaddr$env-ref54527 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48795, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54527
%stackaddr$env-ref54528 = alloca %struct.ScmObj*, align 8
%_37map147121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48795, i64 1)
store %struct.ScmObj* %_37map147121, %struct.ScmObj** %stackaddr$env-ref54528
%stackaddr$env-ref54529 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48795, i64 2)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref54529
%stackaddr$prim54530 = alloca %struct.ScmObj*, align 8
%k47386 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53389)
store volatile %struct.ScmObj* %k47386, %struct.ScmObj** %stackaddr$prim54530, align 8
%stackaddr$prim54531 = alloca %struct.ScmObj*, align 8
%current_45args53390 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53389)
store volatile %struct.ScmObj* %current_45args53390, %struct.ScmObj** %stackaddr$prim54531, align 8
%stackaddr$prim54532 = alloca %struct.ScmObj*, align 8
%_37foldl47173 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53390)
store volatile %struct.ScmObj* %_37foldl47173, %struct.ScmObj** %stackaddr$prim54532, align 8
%ae48797 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54533 = alloca %struct.ScmObj*, align 8
%fptrToInt54534 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48798 to i64
%ae48798 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54534)
store volatile %struct.ScmObj* %ae48798, %struct.ScmObj** %stackaddr$makeclosure54533, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48798, %struct.ScmObj* %_37foldl47173, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48798, %struct.ScmObj* %_37foldr147090, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48798, %struct.ScmObj* %_37map147121, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48798, %struct.ScmObj* %_37foldr47095, i64 3)
%argslist53447$k473860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54535 = alloca %struct.ScmObj*, align 8
%argslist53447$k473861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48798, %struct.ScmObj* %argslist53447$k473860)
store volatile %struct.ScmObj* %argslist53447$k473861, %struct.ScmObj** %stackaddr$prim54535, align 8
%stackaddr$prim54536 = alloca %struct.ScmObj*, align 8
%argslist53447$k473862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48797, %struct.ScmObj* %argslist53447$k473861)
store volatile %struct.ScmObj* %argslist53447$k473862, %struct.ScmObj** %stackaddr$prim54536, align 8
%clofunc54537 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47386)
musttail call tailcc void %clofunc54537(%struct.ScmObj* %k47386, %struct.ScmObj* %argslist53447$k473862)
ret void
}

define tailcc void @proc_clo$ae48798(%struct.ScmObj* %env$ae48798,%struct.ScmObj* %args4717447387) {
%stackaddr$env-ref54538 = alloca %struct.ScmObj*, align 8
%_37foldl47173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48798, i64 0)
store %struct.ScmObj* %_37foldl47173, %struct.ScmObj** %stackaddr$env-ref54538
%stackaddr$env-ref54539 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48798, i64 1)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54539
%stackaddr$env-ref54540 = alloca %struct.ScmObj*, align 8
%_37map147121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48798, i64 2)
store %struct.ScmObj* %_37map147121, %struct.ScmObj** %stackaddr$env-ref54540
%stackaddr$env-ref54541 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48798, i64 3)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref54541
%stackaddr$prim54542 = alloca %struct.ScmObj*, align 8
%k47388 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4717447387)
store volatile %struct.ScmObj* %k47388, %struct.ScmObj** %stackaddr$prim54542, align 8
%stackaddr$prim54543 = alloca %struct.ScmObj*, align 8
%args47174 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4717447387)
store volatile %struct.ScmObj* %args47174, %struct.ScmObj** %stackaddr$prim54543, align 8
%stackaddr$prim54544 = alloca %struct.ScmObj*, align 8
%f47177 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47174)
store volatile %struct.ScmObj* %f47177, %struct.ScmObj** %stackaddr$prim54544, align 8
%stackaddr$prim54545 = alloca %struct.ScmObj*, align 8
%anf_45bind47248 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47174)
store volatile %struct.ScmObj* %anf_45bind47248, %struct.ScmObj** %stackaddr$prim54545, align 8
%stackaddr$prim54546 = alloca %struct.ScmObj*, align 8
%acc47176 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47248)
store volatile %struct.ScmObj* %acc47176, %struct.ScmObj** %stackaddr$prim54546, align 8
%stackaddr$prim54547 = alloca %struct.ScmObj*, align 8
%anf_45bind47249 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47174)
store volatile %struct.ScmObj* %anf_45bind47249, %struct.ScmObj** %stackaddr$prim54547, align 8
%stackaddr$prim54548 = alloca %struct.ScmObj*, align 8
%lsts47175 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47249)
store volatile %struct.ScmObj* %lsts47175, %struct.ScmObj** %stackaddr$prim54548, align 8
%stackaddr$makeclosure54549 = alloca %struct.ScmObj*, align 8
%fptrToInt54550 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48806 to i64
%ae48806 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt54550)
store volatile %struct.ScmObj* %ae48806, %struct.ScmObj** %stackaddr$makeclosure54549, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48806, %struct.ScmObj* %lsts47175, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48806, %struct.ScmObj* %_37foldr47095, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48806, %struct.ScmObj* %_37foldl47173, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48806, %struct.ScmObj* %_37foldr147090, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48806, %struct.ScmObj* %_37map147121, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48806, %struct.ScmObj* %k47388, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48806, %struct.ScmObj* %f47177, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae48806, %struct.ScmObj* %acc47176, i64 7)
%ae48807 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54551 = alloca %struct.ScmObj*, align 8
%fptrToInt54552 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48808 to i64
%ae48808 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54552)
store volatile %struct.ScmObj* %ae48808, %struct.ScmObj** %stackaddr$makeclosure54551, align 8
%argslist53446$ae488060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54553 = alloca %struct.ScmObj*, align 8
%argslist53446$ae488061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48808, %struct.ScmObj* %argslist53446$ae488060)
store volatile %struct.ScmObj* %argslist53446$ae488061, %struct.ScmObj** %stackaddr$prim54553, align 8
%stackaddr$prim54554 = alloca %struct.ScmObj*, align 8
%argslist53446$ae488062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48807, %struct.ScmObj* %argslist53446$ae488061)
store volatile %struct.ScmObj* %argslist53446$ae488062, %struct.ScmObj** %stackaddr$prim54554, align 8
%clofunc54555 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48806)
musttail call tailcc void %clofunc54555(%struct.ScmObj* %ae48806, %struct.ScmObj* %argslist53446$ae488062)
ret void
}

define tailcc void @proc_clo$ae48806(%struct.ScmObj* %env$ae48806,%struct.ScmObj* %current_45args53392) {
%stackaddr$env-ref54556 = alloca %struct.ScmObj*, align 8
%lsts47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48806, i64 0)
store %struct.ScmObj* %lsts47175, %struct.ScmObj** %stackaddr$env-ref54556
%stackaddr$env-ref54557 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48806, i64 1)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref54557
%stackaddr$env-ref54558 = alloca %struct.ScmObj*, align 8
%_37foldl47173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48806, i64 2)
store %struct.ScmObj* %_37foldl47173, %struct.ScmObj** %stackaddr$env-ref54558
%stackaddr$env-ref54559 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48806, i64 3)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54559
%stackaddr$env-ref54560 = alloca %struct.ScmObj*, align 8
%_37map147121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48806, i64 4)
store %struct.ScmObj* %_37map147121, %struct.ScmObj** %stackaddr$env-ref54560
%stackaddr$env-ref54561 = alloca %struct.ScmObj*, align 8
%k47388 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48806, i64 5)
store %struct.ScmObj* %k47388, %struct.ScmObj** %stackaddr$env-ref54561
%stackaddr$env-ref54562 = alloca %struct.ScmObj*, align 8
%f47177 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48806, i64 6)
store %struct.ScmObj* %f47177, %struct.ScmObj** %stackaddr$env-ref54562
%stackaddr$env-ref54563 = alloca %struct.ScmObj*, align 8
%acc47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48806, i64 7)
store %struct.ScmObj* %acc47176, %struct.ScmObj** %stackaddr$env-ref54563
%stackaddr$prim54564 = alloca %struct.ScmObj*, align 8
%_95k47389 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53392)
store volatile %struct.ScmObj* %_95k47389, %struct.ScmObj** %stackaddr$prim54564, align 8
%stackaddr$prim54565 = alloca %struct.ScmObj*, align 8
%current_45args53393 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53392)
store volatile %struct.ScmObj* %current_45args53393, %struct.ScmObj** %stackaddr$prim54565, align 8
%stackaddr$prim54566 = alloca %struct.ScmObj*, align 8
%anf_45bind47250 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53393)
store volatile %struct.ScmObj* %anf_45bind47250, %struct.ScmObj** %stackaddr$prim54566, align 8
%stackaddr$makeclosure54567 = alloca %struct.ScmObj*, align 8
%fptrToInt54568 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48838 to i64
%ae48838 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt54568)
store volatile %struct.ScmObj* %ae48838, %struct.ScmObj** %stackaddr$makeclosure54567, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48838, %struct.ScmObj* %lsts47175, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48838, %struct.ScmObj* %_37foldr47095, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48838, %struct.ScmObj* %_37foldl47173, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48838, %struct.ScmObj* %_37map147121, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48838, %struct.ScmObj* %k47388, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48838, %struct.ScmObj* %f47177, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48838, %struct.ScmObj* %acc47176, i64 6)
%ae48840 = call %struct.ScmObj* @const_init_false()
%argslist53439$_37foldr1470900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54569 = alloca %struct.ScmObj*, align 8
%argslist53439$_37foldr1470901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47175, %struct.ScmObj* %argslist53439$_37foldr1470900)
store volatile %struct.ScmObj* %argslist53439$_37foldr1470901, %struct.ScmObj** %stackaddr$prim54569, align 8
%stackaddr$prim54570 = alloca %struct.ScmObj*, align 8
%argslist53439$_37foldr1470902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48840, %struct.ScmObj* %argslist53439$_37foldr1470901)
store volatile %struct.ScmObj* %argslist53439$_37foldr1470902, %struct.ScmObj** %stackaddr$prim54570, align 8
%stackaddr$prim54571 = alloca %struct.ScmObj*, align 8
%argslist53439$_37foldr1470903 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47250, %struct.ScmObj* %argslist53439$_37foldr1470902)
store volatile %struct.ScmObj* %argslist53439$_37foldr1470903, %struct.ScmObj** %stackaddr$prim54571, align 8
%stackaddr$prim54572 = alloca %struct.ScmObj*, align 8
%argslist53439$_37foldr1470904 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48838, %struct.ScmObj* %argslist53439$_37foldr1470903)
store volatile %struct.ScmObj* %argslist53439$_37foldr1470904, %struct.ScmObj** %stackaddr$prim54572, align 8
%clofunc54573 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147090)
musttail call tailcc void %clofunc54573(%struct.ScmObj* %_37foldr147090, %struct.ScmObj* %argslist53439$_37foldr1470904)
ret void
}

define tailcc void @proc_clo$ae48838(%struct.ScmObj* %env$ae48838,%struct.ScmObj* %current_45args53395) {
%stackaddr$env-ref54574 = alloca %struct.ScmObj*, align 8
%lsts47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48838, i64 0)
store %struct.ScmObj* %lsts47175, %struct.ScmObj** %stackaddr$env-ref54574
%stackaddr$env-ref54575 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48838, i64 1)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref54575
%stackaddr$env-ref54576 = alloca %struct.ScmObj*, align 8
%_37foldl47173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48838, i64 2)
store %struct.ScmObj* %_37foldl47173, %struct.ScmObj** %stackaddr$env-ref54576
%stackaddr$env-ref54577 = alloca %struct.ScmObj*, align 8
%_37map147121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48838, i64 3)
store %struct.ScmObj* %_37map147121, %struct.ScmObj** %stackaddr$env-ref54577
%stackaddr$env-ref54578 = alloca %struct.ScmObj*, align 8
%k47388 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48838, i64 4)
store %struct.ScmObj* %k47388, %struct.ScmObj** %stackaddr$env-ref54578
%stackaddr$env-ref54579 = alloca %struct.ScmObj*, align 8
%f47177 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48838, i64 5)
store %struct.ScmObj* %f47177, %struct.ScmObj** %stackaddr$env-ref54579
%stackaddr$env-ref54580 = alloca %struct.ScmObj*, align 8
%acc47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48838, i64 6)
store %struct.ScmObj* %acc47176, %struct.ScmObj** %stackaddr$env-ref54580
%stackaddr$prim54581 = alloca %struct.ScmObj*, align 8
%_95k47390 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53395)
store volatile %struct.ScmObj* %_95k47390, %struct.ScmObj** %stackaddr$prim54581, align 8
%stackaddr$prim54582 = alloca %struct.ScmObj*, align 8
%current_45args53396 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53395)
store volatile %struct.ScmObj* %current_45args53396, %struct.ScmObj** %stackaddr$prim54582, align 8
%stackaddr$prim54583 = alloca %struct.ScmObj*, align 8
%anf_45bind47251 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53396)
store volatile %struct.ScmObj* %anf_45bind47251, %struct.ScmObj** %stackaddr$prim54583, align 8
%truthy$cmp54584 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47251)
%cmp$cmp54584 = icmp eq i64 %truthy$cmp54584, 1
br i1 %cmp$cmp54584, label %truebranch$cmp54584, label %falsebranch$cmp54584
truebranch$cmp54584:
%ae48849 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53398$k473880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54585 = alloca %struct.ScmObj*, align 8
%argslist53398$k473881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47176, %struct.ScmObj* %argslist53398$k473880)
store volatile %struct.ScmObj* %argslist53398$k473881, %struct.ScmObj** %stackaddr$prim54585, align 8
%stackaddr$prim54586 = alloca %struct.ScmObj*, align 8
%argslist53398$k473882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48849, %struct.ScmObj* %argslist53398$k473881)
store volatile %struct.ScmObj* %argslist53398$k473882, %struct.ScmObj** %stackaddr$prim54586, align 8
%clofunc54587 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47388)
musttail call tailcc void %clofunc54587(%struct.ScmObj* %k47388, %struct.ScmObj* %argslist53398$k473882)
ret void
falsebranch$cmp54584:
%stackaddr$makeclosure54588 = alloca %struct.ScmObj*, align 8
%fptrToInt54589 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48854 to i64
%ae48854 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt54589)
store volatile %struct.ScmObj* %ae48854, %struct.ScmObj** %stackaddr$makeclosure54588, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48854, %struct.ScmObj* %lsts47175, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48854, %struct.ScmObj* %_37foldr47095, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48854, %struct.ScmObj* %_37foldl47173, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48854, %struct.ScmObj* %_37map147121, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48854, %struct.ScmObj* %k47388, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48854, %struct.ScmObj* %f47177, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48854, %struct.ScmObj* %acc47176, i64 6)
%ae48855 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54590 = alloca %struct.ScmObj*, align 8
%fptrToInt54591 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48856 to i64
%ae48856 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54591)
store volatile %struct.ScmObj* %ae48856, %struct.ScmObj** %stackaddr$makeclosure54590, align 8
%argslist53438$ae488540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54592 = alloca %struct.ScmObj*, align 8
%argslist53438$ae488541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48856, %struct.ScmObj* %argslist53438$ae488540)
store volatile %struct.ScmObj* %argslist53438$ae488541, %struct.ScmObj** %stackaddr$prim54592, align 8
%stackaddr$prim54593 = alloca %struct.ScmObj*, align 8
%argslist53438$ae488542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48855, %struct.ScmObj* %argslist53438$ae488541)
store volatile %struct.ScmObj* %argslist53438$ae488542, %struct.ScmObj** %stackaddr$prim54593, align 8
%clofunc54594 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48854)
musttail call tailcc void %clofunc54594(%struct.ScmObj* %ae48854, %struct.ScmObj* %argslist53438$ae488542)
ret void
}

define tailcc void @proc_clo$ae48854(%struct.ScmObj* %env$ae48854,%struct.ScmObj* %current_45args53399) {
%stackaddr$env-ref54595 = alloca %struct.ScmObj*, align 8
%lsts47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48854, i64 0)
store %struct.ScmObj* %lsts47175, %struct.ScmObj** %stackaddr$env-ref54595
%stackaddr$env-ref54596 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48854, i64 1)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref54596
%stackaddr$env-ref54597 = alloca %struct.ScmObj*, align 8
%_37foldl47173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48854, i64 2)
store %struct.ScmObj* %_37foldl47173, %struct.ScmObj** %stackaddr$env-ref54597
%stackaddr$env-ref54598 = alloca %struct.ScmObj*, align 8
%_37map147121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48854, i64 3)
store %struct.ScmObj* %_37map147121, %struct.ScmObj** %stackaddr$env-ref54598
%stackaddr$env-ref54599 = alloca %struct.ScmObj*, align 8
%k47388 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48854, i64 4)
store %struct.ScmObj* %k47388, %struct.ScmObj** %stackaddr$env-ref54599
%stackaddr$env-ref54600 = alloca %struct.ScmObj*, align 8
%f47177 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48854, i64 5)
store %struct.ScmObj* %f47177, %struct.ScmObj** %stackaddr$env-ref54600
%stackaddr$env-ref54601 = alloca %struct.ScmObj*, align 8
%acc47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48854, i64 6)
store %struct.ScmObj* %acc47176, %struct.ScmObj** %stackaddr$env-ref54601
%stackaddr$prim54602 = alloca %struct.ScmObj*, align 8
%_95k47391 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53399)
store volatile %struct.ScmObj* %_95k47391, %struct.ScmObj** %stackaddr$prim54602, align 8
%stackaddr$prim54603 = alloca %struct.ScmObj*, align 8
%current_45args53400 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53399)
store volatile %struct.ScmObj* %current_45args53400, %struct.ScmObj** %stackaddr$prim54603, align 8
%stackaddr$prim54604 = alloca %struct.ScmObj*, align 8
%anf_45bind47252 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53400)
store volatile %struct.ScmObj* %anf_45bind47252, %struct.ScmObj** %stackaddr$prim54604, align 8
%stackaddr$makeclosure54605 = alloca %struct.ScmObj*, align 8
%fptrToInt54606 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48875 to i64
%ae48875 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt54606)
store volatile %struct.ScmObj* %ae48875, %struct.ScmObj** %stackaddr$makeclosure54605, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48875, %struct.ScmObj* %lsts47175, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48875, %struct.ScmObj* %_37foldr47095, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48875, %struct.ScmObj* %_37foldl47173, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48875, %struct.ScmObj* %_37map147121, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48875, %struct.ScmObj* %k47388, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48875, %struct.ScmObj* %f47177, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48875, %struct.ScmObj* %acc47176, i64 6)
%argslist53433$_37map1471210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54607 = alloca %struct.ScmObj*, align 8
%argslist53433$_37map1471211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47175, %struct.ScmObj* %argslist53433$_37map1471210)
store volatile %struct.ScmObj* %argslist53433$_37map1471211, %struct.ScmObj** %stackaddr$prim54607, align 8
%stackaddr$prim54608 = alloca %struct.ScmObj*, align 8
%argslist53433$_37map1471212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47252, %struct.ScmObj* %argslist53433$_37map1471211)
store volatile %struct.ScmObj* %argslist53433$_37map1471212, %struct.ScmObj** %stackaddr$prim54608, align 8
%stackaddr$prim54609 = alloca %struct.ScmObj*, align 8
%argslist53433$_37map1471213 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48875, %struct.ScmObj* %argslist53433$_37map1471212)
store volatile %struct.ScmObj* %argslist53433$_37map1471213, %struct.ScmObj** %stackaddr$prim54609, align 8
%clofunc54610 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147121)
musttail call tailcc void %clofunc54610(%struct.ScmObj* %_37map147121, %struct.ScmObj* %argslist53433$_37map1471213)
ret void
}

define tailcc void @proc_clo$ae48875(%struct.ScmObj* %env$ae48875,%struct.ScmObj* %current_45args53402) {
%stackaddr$env-ref54611 = alloca %struct.ScmObj*, align 8
%lsts47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48875, i64 0)
store %struct.ScmObj* %lsts47175, %struct.ScmObj** %stackaddr$env-ref54611
%stackaddr$env-ref54612 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48875, i64 1)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref54612
%stackaddr$env-ref54613 = alloca %struct.ScmObj*, align 8
%_37foldl47173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48875, i64 2)
store %struct.ScmObj* %_37foldl47173, %struct.ScmObj** %stackaddr$env-ref54613
%stackaddr$env-ref54614 = alloca %struct.ScmObj*, align 8
%_37map147121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48875, i64 3)
store %struct.ScmObj* %_37map147121, %struct.ScmObj** %stackaddr$env-ref54614
%stackaddr$env-ref54615 = alloca %struct.ScmObj*, align 8
%k47388 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48875, i64 4)
store %struct.ScmObj* %k47388, %struct.ScmObj** %stackaddr$env-ref54615
%stackaddr$env-ref54616 = alloca %struct.ScmObj*, align 8
%f47177 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48875, i64 5)
store %struct.ScmObj* %f47177, %struct.ScmObj** %stackaddr$env-ref54616
%stackaddr$env-ref54617 = alloca %struct.ScmObj*, align 8
%acc47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48875, i64 6)
store %struct.ScmObj* %acc47176, %struct.ScmObj** %stackaddr$env-ref54617
%stackaddr$prim54618 = alloca %struct.ScmObj*, align 8
%_95k47392 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53402)
store volatile %struct.ScmObj* %_95k47392, %struct.ScmObj** %stackaddr$prim54618, align 8
%stackaddr$prim54619 = alloca %struct.ScmObj*, align 8
%current_45args53403 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53402)
store volatile %struct.ScmObj* %current_45args53403, %struct.ScmObj** %stackaddr$prim54619, align 8
%stackaddr$prim54620 = alloca %struct.ScmObj*, align 8
%lsts_4347182 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53403)
store volatile %struct.ScmObj* %lsts_4347182, %struct.ScmObj** %stackaddr$prim54620, align 8
%stackaddr$makeclosure54621 = alloca %struct.ScmObj*, align 8
%fptrToInt54622 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48878 to i64
%ae48878 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt54622)
store volatile %struct.ScmObj* %ae48878, %struct.ScmObj** %stackaddr$makeclosure54621, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48878, %struct.ScmObj* %lsts47175, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48878, %struct.ScmObj* %_37foldr47095, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48878, %struct.ScmObj* %_37foldl47173, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48878, %struct.ScmObj* %_37map147121, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48878, %struct.ScmObj* %lsts_4347182, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48878, %struct.ScmObj* %k47388, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48878, %struct.ScmObj* %f47177, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae48878, %struct.ScmObj* %acc47176, i64 7)
%ae48879 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54623 = alloca %struct.ScmObj*, align 8
%fptrToInt54624 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48880 to i64
%ae48880 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54624)
store volatile %struct.ScmObj* %ae48880, %struct.ScmObj** %stackaddr$makeclosure54623, align 8
%argslist53432$ae488780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54625 = alloca %struct.ScmObj*, align 8
%argslist53432$ae488781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48880, %struct.ScmObj* %argslist53432$ae488780)
store volatile %struct.ScmObj* %argslist53432$ae488781, %struct.ScmObj** %stackaddr$prim54625, align 8
%stackaddr$prim54626 = alloca %struct.ScmObj*, align 8
%argslist53432$ae488782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48879, %struct.ScmObj* %argslist53432$ae488781)
store volatile %struct.ScmObj* %argslist53432$ae488782, %struct.ScmObj** %stackaddr$prim54626, align 8
%clofunc54627 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48878)
musttail call tailcc void %clofunc54627(%struct.ScmObj* %ae48878, %struct.ScmObj* %argslist53432$ae488782)
ret void
}

define tailcc void @proc_clo$ae48878(%struct.ScmObj* %env$ae48878,%struct.ScmObj* %current_45args53405) {
%stackaddr$env-ref54628 = alloca %struct.ScmObj*, align 8
%lsts47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48878, i64 0)
store %struct.ScmObj* %lsts47175, %struct.ScmObj** %stackaddr$env-ref54628
%stackaddr$env-ref54629 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48878, i64 1)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref54629
%stackaddr$env-ref54630 = alloca %struct.ScmObj*, align 8
%_37foldl47173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48878, i64 2)
store %struct.ScmObj* %_37foldl47173, %struct.ScmObj** %stackaddr$env-ref54630
%stackaddr$env-ref54631 = alloca %struct.ScmObj*, align 8
%_37map147121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48878, i64 3)
store %struct.ScmObj* %_37map147121, %struct.ScmObj** %stackaddr$env-ref54631
%stackaddr$env-ref54632 = alloca %struct.ScmObj*, align 8
%lsts_4347182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48878, i64 4)
store %struct.ScmObj* %lsts_4347182, %struct.ScmObj** %stackaddr$env-ref54632
%stackaddr$env-ref54633 = alloca %struct.ScmObj*, align 8
%k47388 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48878, i64 5)
store %struct.ScmObj* %k47388, %struct.ScmObj** %stackaddr$env-ref54633
%stackaddr$env-ref54634 = alloca %struct.ScmObj*, align 8
%f47177 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48878, i64 6)
store %struct.ScmObj* %f47177, %struct.ScmObj** %stackaddr$env-ref54634
%stackaddr$env-ref54635 = alloca %struct.ScmObj*, align 8
%acc47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48878, i64 7)
store %struct.ScmObj* %acc47176, %struct.ScmObj** %stackaddr$env-ref54635
%stackaddr$prim54636 = alloca %struct.ScmObj*, align 8
%_95k47393 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53405)
store volatile %struct.ScmObj* %_95k47393, %struct.ScmObj** %stackaddr$prim54636, align 8
%stackaddr$prim54637 = alloca %struct.ScmObj*, align 8
%current_45args53406 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53405)
store volatile %struct.ScmObj* %current_45args53406, %struct.ScmObj** %stackaddr$prim54637, align 8
%stackaddr$prim54638 = alloca %struct.ScmObj*, align 8
%anf_45bind47253 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53406)
store volatile %struct.ScmObj* %anf_45bind47253, %struct.ScmObj** %stackaddr$prim54638, align 8
%stackaddr$makeclosure54639 = alloca %struct.ScmObj*, align 8
%fptrToInt54640 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48899 to i64
%ae48899 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt54640)
store volatile %struct.ScmObj* %ae48899, %struct.ScmObj** %stackaddr$makeclosure54639, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48899, %struct.ScmObj* %_37foldl47173, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48899, %struct.ScmObj* %lsts_4347182, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48899, %struct.ScmObj* %k47388, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48899, %struct.ScmObj* %f47177, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48899, %struct.ScmObj* %acc47176, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48899, %struct.ScmObj* %_37foldr47095, i64 5)
%argslist53427$_37map1471210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54641 = alloca %struct.ScmObj*, align 8
%argslist53427$_37map1471211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47175, %struct.ScmObj* %argslist53427$_37map1471210)
store volatile %struct.ScmObj* %argslist53427$_37map1471211, %struct.ScmObj** %stackaddr$prim54641, align 8
%stackaddr$prim54642 = alloca %struct.ScmObj*, align 8
%argslist53427$_37map1471212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47253, %struct.ScmObj* %argslist53427$_37map1471211)
store volatile %struct.ScmObj* %argslist53427$_37map1471212, %struct.ScmObj** %stackaddr$prim54642, align 8
%stackaddr$prim54643 = alloca %struct.ScmObj*, align 8
%argslist53427$_37map1471213 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48899, %struct.ScmObj* %argslist53427$_37map1471212)
store volatile %struct.ScmObj* %argslist53427$_37map1471213, %struct.ScmObj** %stackaddr$prim54643, align 8
%clofunc54644 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147121)
musttail call tailcc void %clofunc54644(%struct.ScmObj* %_37map147121, %struct.ScmObj* %argslist53427$_37map1471213)
ret void
}

define tailcc void @proc_clo$ae48899(%struct.ScmObj* %env$ae48899,%struct.ScmObj* %current_45args53408) {
%stackaddr$env-ref54645 = alloca %struct.ScmObj*, align 8
%_37foldl47173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48899, i64 0)
store %struct.ScmObj* %_37foldl47173, %struct.ScmObj** %stackaddr$env-ref54645
%stackaddr$env-ref54646 = alloca %struct.ScmObj*, align 8
%lsts_4347182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48899, i64 1)
store %struct.ScmObj* %lsts_4347182, %struct.ScmObj** %stackaddr$env-ref54646
%stackaddr$env-ref54647 = alloca %struct.ScmObj*, align 8
%k47388 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48899, i64 2)
store %struct.ScmObj* %k47388, %struct.ScmObj** %stackaddr$env-ref54647
%stackaddr$env-ref54648 = alloca %struct.ScmObj*, align 8
%f47177 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48899, i64 3)
store %struct.ScmObj* %f47177, %struct.ScmObj** %stackaddr$env-ref54648
%stackaddr$env-ref54649 = alloca %struct.ScmObj*, align 8
%acc47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48899, i64 4)
store %struct.ScmObj* %acc47176, %struct.ScmObj** %stackaddr$env-ref54649
%stackaddr$env-ref54650 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48899, i64 5)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref54650
%stackaddr$prim54651 = alloca %struct.ScmObj*, align 8
%_95k47394 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53408)
store volatile %struct.ScmObj* %_95k47394, %struct.ScmObj** %stackaddr$prim54651, align 8
%stackaddr$prim54652 = alloca %struct.ScmObj*, align 8
%current_45args53409 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53408)
store volatile %struct.ScmObj* %current_45args53409, %struct.ScmObj** %stackaddr$prim54652, align 8
%stackaddr$prim54653 = alloca %struct.ScmObj*, align 8
%vs47180 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53409)
store volatile %struct.ScmObj* %vs47180, %struct.ScmObj** %stackaddr$prim54653, align 8
%stackaddr$makeclosure54654 = alloca %struct.ScmObj*, align 8
%fptrToInt54655 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48902 to i64
%ae48902 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt54655)
store volatile %struct.ScmObj* %ae48902, %struct.ScmObj** %stackaddr$makeclosure54654, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48902, %struct.ScmObj* %vs47180, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48902, %struct.ScmObj* %k47388, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48902, %struct.ScmObj* %_37foldl47173, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48902, %struct.ScmObj* %lsts_4347182, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48902, %struct.ScmObj* %f47177, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48902, %struct.ScmObj* %acc47176, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48902, %struct.ScmObj* %_37foldr47095, i64 6)
%ae48903 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54656 = alloca %struct.ScmObj*, align 8
%fptrToInt54657 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48904 to i64
%ae48904 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54657)
store volatile %struct.ScmObj* %ae48904, %struct.ScmObj** %stackaddr$makeclosure54656, align 8
%argslist53426$ae489020 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54658 = alloca %struct.ScmObj*, align 8
%argslist53426$ae489021 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48904, %struct.ScmObj* %argslist53426$ae489020)
store volatile %struct.ScmObj* %argslist53426$ae489021, %struct.ScmObj** %stackaddr$prim54658, align 8
%stackaddr$prim54659 = alloca %struct.ScmObj*, align 8
%argslist53426$ae489022 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48903, %struct.ScmObj* %argslist53426$ae489021)
store volatile %struct.ScmObj* %argslist53426$ae489022, %struct.ScmObj** %stackaddr$prim54659, align 8
%clofunc54660 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48902)
musttail call tailcc void %clofunc54660(%struct.ScmObj* %ae48902, %struct.ScmObj* %argslist53426$ae489022)
ret void
}

define tailcc void @proc_clo$ae48902(%struct.ScmObj* %env$ae48902,%struct.ScmObj* %current_45args53411) {
%stackaddr$env-ref54661 = alloca %struct.ScmObj*, align 8
%vs47180 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48902, i64 0)
store %struct.ScmObj* %vs47180, %struct.ScmObj** %stackaddr$env-ref54661
%stackaddr$env-ref54662 = alloca %struct.ScmObj*, align 8
%k47388 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48902, i64 1)
store %struct.ScmObj* %k47388, %struct.ScmObj** %stackaddr$env-ref54662
%stackaddr$env-ref54663 = alloca %struct.ScmObj*, align 8
%_37foldl47173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48902, i64 2)
store %struct.ScmObj* %_37foldl47173, %struct.ScmObj** %stackaddr$env-ref54663
%stackaddr$env-ref54664 = alloca %struct.ScmObj*, align 8
%lsts_4347182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48902, i64 3)
store %struct.ScmObj* %lsts_4347182, %struct.ScmObj** %stackaddr$env-ref54664
%stackaddr$env-ref54665 = alloca %struct.ScmObj*, align 8
%f47177 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48902, i64 4)
store %struct.ScmObj* %f47177, %struct.ScmObj** %stackaddr$env-ref54665
%stackaddr$env-ref54666 = alloca %struct.ScmObj*, align 8
%acc47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48902, i64 5)
store %struct.ScmObj* %acc47176, %struct.ScmObj** %stackaddr$env-ref54666
%stackaddr$env-ref54667 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48902, i64 6)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref54667
%stackaddr$prim54668 = alloca %struct.ScmObj*, align 8
%_95k47395 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53411)
store volatile %struct.ScmObj* %_95k47395, %struct.ScmObj** %stackaddr$prim54668, align 8
%stackaddr$prim54669 = alloca %struct.ScmObj*, align 8
%current_45args53412 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53411)
store volatile %struct.ScmObj* %current_45args53412, %struct.ScmObj** %stackaddr$prim54669, align 8
%stackaddr$prim54670 = alloca %struct.ScmObj*, align 8
%anf_45bind47254 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53412)
store volatile %struct.ScmObj* %anf_45bind47254, %struct.ScmObj** %stackaddr$prim54670, align 8
%ae48925 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54671 = alloca %struct.ScmObj*, align 8
%anf_45bind47255 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47176, %struct.ScmObj* %ae48925)
store volatile %struct.ScmObj* %anf_45bind47255, %struct.ScmObj** %stackaddr$prim54671, align 8
%stackaddr$makeclosure54672 = alloca %struct.ScmObj*, align 8
%fptrToInt54673 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48927 to i64
%ae48927 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54673)
store volatile %struct.ScmObj* %ae48927, %struct.ScmObj** %stackaddr$makeclosure54672, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48927, %struct.ScmObj* %_37foldl47173, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48927, %struct.ScmObj* %lsts_4347182, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48927, %struct.ScmObj* %k47388, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48927, %struct.ScmObj* %f47177, i64 3)
%argslist53420$_37foldr470950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54674 = alloca %struct.ScmObj*, align 8
%argslist53420$_37foldr470951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs47180, %struct.ScmObj* %argslist53420$_37foldr470950)
store volatile %struct.ScmObj* %argslist53420$_37foldr470951, %struct.ScmObj** %stackaddr$prim54674, align 8
%stackaddr$prim54675 = alloca %struct.ScmObj*, align 8
%argslist53420$_37foldr470952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47255, %struct.ScmObj* %argslist53420$_37foldr470951)
store volatile %struct.ScmObj* %argslist53420$_37foldr470952, %struct.ScmObj** %stackaddr$prim54675, align 8
%stackaddr$prim54676 = alloca %struct.ScmObj*, align 8
%argslist53420$_37foldr470953 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47254, %struct.ScmObj* %argslist53420$_37foldr470952)
store volatile %struct.ScmObj* %argslist53420$_37foldr470953, %struct.ScmObj** %stackaddr$prim54676, align 8
%stackaddr$prim54677 = alloca %struct.ScmObj*, align 8
%argslist53420$_37foldr470954 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48927, %struct.ScmObj* %argslist53420$_37foldr470953)
store volatile %struct.ScmObj* %argslist53420$_37foldr470954, %struct.ScmObj** %stackaddr$prim54677, align 8
%clofunc54678 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr47095)
musttail call tailcc void %clofunc54678(%struct.ScmObj* %_37foldr47095, %struct.ScmObj* %argslist53420$_37foldr470954)
ret void
}

define tailcc void @proc_clo$ae48927(%struct.ScmObj* %env$ae48927,%struct.ScmObj* %current_45args53414) {
%stackaddr$env-ref54679 = alloca %struct.ScmObj*, align 8
%_37foldl47173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48927, i64 0)
store %struct.ScmObj* %_37foldl47173, %struct.ScmObj** %stackaddr$env-ref54679
%stackaddr$env-ref54680 = alloca %struct.ScmObj*, align 8
%lsts_4347182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48927, i64 1)
store %struct.ScmObj* %lsts_4347182, %struct.ScmObj** %stackaddr$env-ref54680
%stackaddr$env-ref54681 = alloca %struct.ScmObj*, align 8
%k47388 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48927, i64 2)
store %struct.ScmObj* %k47388, %struct.ScmObj** %stackaddr$env-ref54681
%stackaddr$env-ref54682 = alloca %struct.ScmObj*, align 8
%f47177 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48927, i64 3)
store %struct.ScmObj* %f47177, %struct.ScmObj** %stackaddr$env-ref54682
%stackaddr$prim54683 = alloca %struct.ScmObj*, align 8
%_95k47396 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53414)
store volatile %struct.ScmObj* %_95k47396, %struct.ScmObj** %stackaddr$prim54683, align 8
%stackaddr$prim54684 = alloca %struct.ScmObj*, align 8
%current_45args53415 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53414)
store volatile %struct.ScmObj* %current_45args53415, %struct.ScmObj** %stackaddr$prim54684, align 8
%stackaddr$prim54685 = alloca %struct.ScmObj*, align 8
%anf_45bind47256 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53415)
store volatile %struct.ScmObj* %anf_45bind47256, %struct.ScmObj** %stackaddr$prim54685, align 8
%stackaddr$makeclosure54686 = alloca %struct.ScmObj*, align 8
%fptrToInt54687 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48931 to i64
%ae48931 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54687)
store volatile %struct.ScmObj* %ae48931, %struct.ScmObj** %stackaddr$makeclosure54686, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48931, %struct.ScmObj* %_37foldl47173, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48931, %struct.ScmObj* %lsts_4347182, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48931, %struct.ScmObj* %k47388, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48931, %struct.ScmObj* %f47177, i64 3)
%stackaddr$prim54688 = alloca %struct.ScmObj*, align 8
%cpsargs47399 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48931, %struct.ScmObj* %anf_45bind47256)
store volatile %struct.ScmObj* %cpsargs47399, %struct.ScmObj** %stackaddr$prim54688, align 8
%clofunc54689 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47177)
musttail call tailcc void %clofunc54689(%struct.ScmObj* %f47177, %struct.ScmObj* %cpsargs47399)
ret void
}

define tailcc void @proc_clo$ae48931(%struct.ScmObj* %env$ae48931,%struct.ScmObj* %current_45args53417) {
%stackaddr$env-ref54690 = alloca %struct.ScmObj*, align 8
%_37foldl47173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48931, i64 0)
store %struct.ScmObj* %_37foldl47173, %struct.ScmObj** %stackaddr$env-ref54690
%stackaddr$env-ref54691 = alloca %struct.ScmObj*, align 8
%lsts_4347182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48931, i64 1)
store %struct.ScmObj* %lsts_4347182, %struct.ScmObj** %stackaddr$env-ref54691
%stackaddr$env-ref54692 = alloca %struct.ScmObj*, align 8
%k47388 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48931, i64 2)
store %struct.ScmObj* %k47388, %struct.ScmObj** %stackaddr$env-ref54692
%stackaddr$env-ref54693 = alloca %struct.ScmObj*, align 8
%f47177 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48931, i64 3)
store %struct.ScmObj* %f47177, %struct.ScmObj** %stackaddr$env-ref54693
%stackaddr$prim54694 = alloca %struct.ScmObj*, align 8
%_95k47397 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53417)
store volatile %struct.ScmObj* %_95k47397, %struct.ScmObj** %stackaddr$prim54694, align 8
%stackaddr$prim54695 = alloca %struct.ScmObj*, align 8
%current_45args53418 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53417)
store volatile %struct.ScmObj* %current_45args53418, %struct.ScmObj** %stackaddr$prim54695, align 8
%stackaddr$prim54696 = alloca %struct.ScmObj*, align 8
%acc_4347184 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53418)
store volatile %struct.ScmObj* %acc_4347184, %struct.ScmObj** %stackaddr$prim54696, align 8
%stackaddr$prim54697 = alloca %struct.ScmObj*, align 8
%anf_45bind47257 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4347184, %struct.ScmObj* %lsts_4347182)
store volatile %struct.ScmObj* %anf_45bind47257, %struct.ScmObj** %stackaddr$prim54697, align 8
%stackaddr$prim54698 = alloca %struct.ScmObj*, align 8
%anf_45bind47258 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47177, %struct.ScmObj* %anf_45bind47257)
store volatile %struct.ScmObj* %anf_45bind47258, %struct.ScmObj** %stackaddr$prim54698, align 8
%stackaddr$prim54699 = alloca %struct.ScmObj*, align 8
%cpsargs47398 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47388, %struct.ScmObj* %anf_45bind47258)
store volatile %struct.ScmObj* %cpsargs47398, %struct.ScmObj** %stackaddr$prim54699, align 8
%clofunc54700 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl47173)
musttail call tailcc void %clofunc54700(%struct.ScmObj* %_37foldl47173, %struct.ScmObj* %cpsargs47398)
ret void
}

define tailcc void @proc_clo$ae48904(%struct.ScmObj* %env$ae48904,%struct.ScmObj* %current_45args53421) {
%stackaddr$prim54701 = alloca %struct.ScmObj*, align 8
%k47400 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53421)
store volatile %struct.ScmObj* %k47400, %struct.ScmObj** %stackaddr$prim54701, align 8
%stackaddr$prim54702 = alloca %struct.ScmObj*, align 8
%current_45args53422 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53421)
store volatile %struct.ScmObj* %current_45args53422, %struct.ScmObj** %stackaddr$prim54702, align 8
%stackaddr$prim54703 = alloca %struct.ScmObj*, align 8
%a47186 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53422)
store volatile %struct.ScmObj* %a47186, %struct.ScmObj** %stackaddr$prim54703, align 8
%stackaddr$prim54704 = alloca %struct.ScmObj*, align 8
%current_45args53423 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53422)
store volatile %struct.ScmObj* %current_45args53423, %struct.ScmObj** %stackaddr$prim54704, align 8
%stackaddr$prim54705 = alloca %struct.ScmObj*, align 8
%b47185 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53423)
store volatile %struct.ScmObj* %b47185, %struct.ScmObj** %stackaddr$prim54705, align 8
%stackaddr$prim54706 = alloca %struct.ScmObj*, align 8
%cpsprim47401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a47186, %struct.ScmObj* %b47185)
store volatile %struct.ScmObj* %cpsprim47401, %struct.ScmObj** %stackaddr$prim54706, align 8
%ae48908 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53425$k474000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54707 = alloca %struct.ScmObj*, align 8
%argslist53425$k474001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47401, %struct.ScmObj* %argslist53425$k474000)
store volatile %struct.ScmObj* %argslist53425$k474001, %struct.ScmObj** %stackaddr$prim54707, align 8
%stackaddr$prim54708 = alloca %struct.ScmObj*, align 8
%argslist53425$k474002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48908, %struct.ScmObj* %argslist53425$k474001)
store volatile %struct.ScmObj* %argslist53425$k474002, %struct.ScmObj** %stackaddr$prim54708, align 8
%clofunc54709 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47400)
musttail call tailcc void %clofunc54709(%struct.ScmObj* %k47400, %struct.ScmObj* %argslist53425$k474002)
ret void
}

define tailcc void @proc_clo$ae48880(%struct.ScmObj* %env$ae48880,%struct.ScmObj* %current_45args53428) {
%stackaddr$prim54710 = alloca %struct.ScmObj*, align 8
%k47402 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53428)
store volatile %struct.ScmObj* %k47402, %struct.ScmObj** %stackaddr$prim54710, align 8
%stackaddr$prim54711 = alloca %struct.ScmObj*, align 8
%current_45args53429 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53428)
store volatile %struct.ScmObj* %current_45args53429, %struct.ScmObj** %stackaddr$prim54711, align 8
%stackaddr$prim54712 = alloca %struct.ScmObj*, align 8
%x47181 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53429)
store volatile %struct.ScmObj* %x47181, %struct.ScmObj** %stackaddr$prim54712, align 8
%stackaddr$prim54713 = alloca %struct.ScmObj*, align 8
%cpsprim47403 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47181)
store volatile %struct.ScmObj* %cpsprim47403, %struct.ScmObj** %stackaddr$prim54713, align 8
%ae48883 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53431$k474020 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54714 = alloca %struct.ScmObj*, align 8
%argslist53431$k474021 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47403, %struct.ScmObj* %argslist53431$k474020)
store volatile %struct.ScmObj* %argslist53431$k474021, %struct.ScmObj** %stackaddr$prim54714, align 8
%stackaddr$prim54715 = alloca %struct.ScmObj*, align 8
%argslist53431$k474022 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48883, %struct.ScmObj* %argslist53431$k474021)
store volatile %struct.ScmObj* %argslist53431$k474022, %struct.ScmObj** %stackaddr$prim54715, align 8
%clofunc54716 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47402)
musttail call tailcc void %clofunc54716(%struct.ScmObj* %k47402, %struct.ScmObj* %argslist53431$k474022)
ret void
}

define tailcc void @proc_clo$ae48856(%struct.ScmObj* %env$ae48856,%struct.ScmObj* %current_45args53434) {
%stackaddr$prim54717 = alloca %struct.ScmObj*, align 8
%k47404 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53434)
store volatile %struct.ScmObj* %k47404, %struct.ScmObj** %stackaddr$prim54717, align 8
%stackaddr$prim54718 = alloca %struct.ScmObj*, align 8
%current_45args53435 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53434)
store volatile %struct.ScmObj* %current_45args53435, %struct.ScmObj** %stackaddr$prim54718, align 8
%stackaddr$prim54719 = alloca %struct.ScmObj*, align 8
%x47183 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53435)
store volatile %struct.ScmObj* %x47183, %struct.ScmObj** %stackaddr$prim54719, align 8
%stackaddr$prim54720 = alloca %struct.ScmObj*, align 8
%cpsprim47405 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47183)
store volatile %struct.ScmObj* %cpsprim47405, %struct.ScmObj** %stackaddr$prim54720, align 8
%ae48859 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53437$k474040 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54721 = alloca %struct.ScmObj*, align 8
%argslist53437$k474041 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47405, %struct.ScmObj* %argslist53437$k474040)
store volatile %struct.ScmObj* %argslist53437$k474041, %struct.ScmObj** %stackaddr$prim54721, align 8
%stackaddr$prim54722 = alloca %struct.ScmObj*, align 8
%argslist53437$k474042 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48859, %struct.ScmObj* %argslist53437$k474041)
store volatile %struct.ScmObj* %argslist53437$k474042, %struct.ScmObj** %stackaddr$prim54722, align 8
%clofunc54723 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47404)
musttail call tailcc void %clofunc54723(%struct.ScmObj* %k47404, %struct.ScmObj* %argslist53437$k474042)
ret void
}

define tailcc void @proc_clo$ae48808(%struct.ScmObj* %env$ae48808,%struct.ScmObj* %current_45args53440) {
%stackaddr$prim54724 = alloca %struct.ScmObj*, align 8
%k47406 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53440)
store volatile %struct.ScmObj* %k47406, %struct.ScmObj** %stackaddr$prim54724, align 8
%stackaddr$prim54725 = alloca %struct.ScmObj*, align 8
%current_45args53441 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53440)
store volatile %struct.ScmObj* %current_45args53441, %struct.ScmObj** %stackaddr$prim54725, align 8
%stackaddr$prim54726 = alloca %struct.ScmObj*, align 8
%lst47179 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53441)
store volatile %struct.ScmObj* %lst47179, %struct.ScmObj** %stackaddr$prim54726, align 8
%stackaddr$prim54727 = alloca %struct.ScmObj*, align 8
%current_45args53442 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53441)
store volatile %struct.ScmObj* %current_45args53442, %struct.ScmObj** %stackaddr$prim54727, align 8
%stackaddr$prim54728 = alloca %struct.ScmObj*, align 8
%b47178 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53442)
store volatile %struct.ScmObj* %b47178, %struct.ScmObj** %stackaddr$prim54728, align 8
%truthy$cmp54729 = call i64 @is_truthy_value(%struct.ScmObj* %b47178)
%cmp$cmp54729 = icmp eq i64 %truthy$cmp54729, 1
br i1 %cmp$cmp54729, label %truebranch$cmp54729, label %falsebranch$cmp54729
truebranch$cmp54729:
%ae48811 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53444$k474060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54730 = alloca %struct.ScmObj*, align 8
%argslist53444$k474061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b47178, %struct.ScmObj* %argslist53444$k474060)
store volatile %struct.ScmObj* %argslist53444$k474061, %struct.ScmObj** %stackaddr$prim54730, align 8
%stackaddr$prim54731 = alloca %struct.ScmObj*, align 8
%argslist53444$k474062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48811, %struct.ScmObj* %argslist53444$k474061)
store volatile %struct.ScmObj* %argslist53444$k474062, %struct.ScmObj** %stackaddr$prim54731, align 8
%clofunc54732 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47406)
musttail call tailcc void %clofunc54732(%struct.ScmObj* %k47406, %struct.ScmObj* %argslist53444$k474062)
ret void
falsebranch$cmp54729:
%stackaddr$prim54733 = alloca %struct.ScmObj*, align 8
%cpsprim47407 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47179)
store volatile %struct.ScmObj* %cpsprim47407, %struct.ScmObj** %stackaddr$prim54733, align 8
%ae48818 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53445$k474060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54734 = alloca %struct.ScmObj*, align 8
%argslist53445$k474061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47407, %struct.ScmObj* %argslist53445$k474060)
store volatile %struct.ScmObj* %argslist53445$k474061, %struct.ScmObj** %stackaddr$prim54734, align 8
%stackaddr$prim54735 = alloca %struct.ScmObj*, align 8
%argslist53445$k474062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48818, %struct.ScmObj* %argslist53445$k474061)
store volatile %struct.ScmObj* %argslist53445$k474062, %struct.ScmObj** %stackaddr$prim54735, align 8
%clofunc54736 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47406)
musttail call tailcc void %clofunc54736(%struct.ScmObj* %k47406, %struct.ScmObj* %argslist53445$k474062)
ret void
}

define tailcc void @proc_clo$ae48649(%struct.ScmObj* %env$ae48649,%struct.ScmObj* %args4711747408) {
%stackaddr$env-ref54737 = alloca %struct.ScmObj*, align 8
%_37drop_45right47109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48649, i64 0)
store %struct.ScmObj* %_37drop_45right47109, %struct.ScmObj** %stackaddr$env-ref54737
%stackaddr$env-ref54738 = alloca %struct.ScmObj*, align 8
%_37last47112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48649, i64 1)
store %struct.ScmObj* %_37last47112, %struct.ScmObj** %stackaddr$env-ref54738
%stackaddr$env-ref54739 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48649, i64 2)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref54739
%stackaddr$prim54740 = alloca %struct.ScmObj*, align 8
%k47409 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4711747408)
store volatile %struct.ScmObj* %k47409, %struct.ScmObj** %stackaddr$prim54740, align 8
%stackaddr$prim54741 = alloca %struct.ScmObj*, align 8
%args47117 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4711747408)
store volatile %struct.ScmObj* %args47117, %struct.ScmObj** %stackaddr$prim54741, align 8
%stackaddr$prim54742 = alloca %struct.ScmObj*, align 8
%f47119 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47117)
store volatile %struct.ScmObj* %f47119, %struct.ScmObj** %stackaddr$prim54742, align 8
%stackaddr$prim54743 = alloca %struct.ScmObj*, align 8
%lsts47118 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47117)
store volatile %struct.ScmObj* %lsts47118, %struct.ScmObj** %stackaddr$prim54743, align 8
%stackaddr$makeclosure54744 = alloca %struct.ScmObj*, align 8
%fptrToInt54745 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48654 to i64
%ae48654 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54745)
store volatile %struct.ScmObj* %ae48654, %struct.ScmObj** %stackaddr$makeclosure54744, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48654, %struct.ScmObj* %k47409, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48654, %struct.ScmObj* %lsts47118, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48654, %struct.ScmObj* %_37foldr47095, i64 2)
%ae48655 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54746 = alloca %struct.ScmObj*, align 8
%fptrToInt54747 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48656 to i64
%ae48656 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54747)
store volatile %struct.ScmObj* %ae48656, %struct.ScmObj** %stackaddr$makeclosure54746, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48656, %struct.ScmObj* %_37drop_45right47109, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48656, %struct.ScmObj* %f47119, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48656, %struct.ScmObj* %_37last47112, i64 2)
%argslist53464$ae486540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54748 = alloca %struct.ScmObj*, align 8
%argslist53464$ae486541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48656, %struct.ScmObj* %argslist53464$ae486540)
store volatile %struct.ScmObj* %argslist53464$ae486541, %struct.ScmObj** %stackaddr$prim54748, align 8
%stackaddr$prim54749 = alloca %struct.ScmObj*, align 8
%argslist53464$ae486542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48655, %struct.ScmObj* %argslist53464$ae486541)
store volatile %struct.ScmObj* %argslist53464$ae486542, %struct.ScmObj** %stackaddr$prim54749, align 8
%clofunc54750 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48654)
musttail call tailcc void %clofunc54750(%struct.ScmObj* %ae48654, %struct.ScmObj* %argslist53464$ae486542)
ret void
}

define tailcc void @proc_clo$ae48654(%struct.ScmObj* %env$ae48654,%struct.ScmObj* %current_45args53449) {
%stackaddr$env-ref54751 = alloca %struct.ScmObj*, align 8
%k47409 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48654, i64 0)
store %struct.ScmObj* %k47409, %struct.ScmObj** %stackaddr$env-ref54751
%stackaddr$env-ref54752 = alloca %struct.ScmObj*, align 8
%lsts47118 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48654, i64 1)
store %struct.ScmObj* %lsts47118, %struct.ScmObj** %stackaddr$env-ref54752
%stackaddr$env-ref54753 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48654, i64 2)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref54753
%stackaddr$prim54754 = alloca %struct.ScmObj*, align 8
%_95k47410 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53449)
store volatile %struct.ScmObj* %_95k47410, %struct.ScmObj** %stackaddr$prim54754, align 8
%stackaddr$prim54755 = alloca %struct.ScmObj*, align 8
%current_45args53450 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53449)
store volatile %struct.ScmObj* %current_45args53450, %struct.ScmObj** %stackaddr$prim54755, align 8
%stackaddr$prim54756 = alloca %struct.ScmObj*, align 8
%anf_45bind47245 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53450)
store volatile %struct.ScmObj* %anf_45bind47245, %struct.ScmObj** %stackaddr$prim54756, align 8
%ae48717 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54757 = alloca %struct.ScmObj*, align 8
%anf_45bind47246 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48717, %struct.ScmObj* %lsts47118)
store volatile %struct.ScmObj* %anf_45bind47246, %struct.ScmObj** %stackaddr$prim54757, align 8
%stackaddr$prim54758 = alloca %struct.ScmObj*, align 8
%anf_45bind47247 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47245, %struct.ScmObj* %anf_45bind47246)
store volatile %struct.ScmObj* %anf_45bind47247, %struct.ScmObj** %stackaddr$prim54758, align 8
%stackaddr$prim54759 = alloca %struct.ScmObj*, align 8
%cpsargs47411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47409, %struct.ScmObj* %anf_45bind47247)
store volatile %struct.ScmObj* %cpsargs47411, %struct.ScmObj** %stackaddr$prim54759, align 8
%clofunc54760 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr47095)
musttail call tailcc void %clofunc54760(%struct.ScmObj* %_37foldr47095, %struct.ScmObj* %cpsargs47411)
ret void
}

define tailcc void @proc_clo$ae48656(%struct.ScmObj* %env$ae48656,%struct.ScmObj* %fargs4712047412) {
%stackaddr$env-ref54761 = alloca %struct.ScmObj*, align 8
%_37drop_45right47109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48656, i64 0)
store %struct.ScmObj* %_37drop_45right47109, %struct.ScmObj** %stackaddr$env-ref54761
%stackaddr$env-ref54762 = alloca %struct.ScmObj*, align 8
%f47119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48656, i64 1)
store %struct.ScmObj* %f47119, %struct.ScmObj** %stackaddr$env-ref54762
%stackaddr$env-ref54763 = alloca %struct.ScmObj*, align 8
%_37last47112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48656, i64 2)
store %struct.ScmObj* %_37last47112, %struct.ScmObj** %stackaddr$env-ref54763
%stackaddr$prim54764 = alloca %struct.ScmObj*, align 8
%k47413 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs4712047412)
store volatile %struct.ScmObj* %k47413, %struct.ScmObj** %stackaddr$prim54764, align 8
%stackaddr$prim54765 = alloca %struct.ScmObj*, align 8
%fargs47120 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs4712047412)
store volatile %struct.ScmObj* %fargs47120, %struct.ScmObj** %stackaddr$prim54765, align 8
%stackaddr$makeclosure54766 = alloca %struct.ScmObj*, align 8
%fptrToInt54767 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48660 to i64
%ae48660 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54767)
store volatile %struct.ScmObj* %ae48660, %struct.ScmObj** %stackaddr$makeclosure54766, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48660, %struct.ScmObj* %k47413, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48660, %struct.ScmObj* %fargs47120, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48660, %struct.ScmObj* %f47119, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48660, %struct.ScmObj* %_37last47112, i64 3)
%ae48662 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist53463$_37drop_45right471090 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54768 = alloca %struct.ScmObj*, align 8
%argslist53463$_37drop_45right471091 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48662, %struct.ScmObj* %argslist53463$_37drop_45right471090)
store volatile %struct.ScmObj* %argslist53463$_37drop_45right471091, %struct.ScmObj** %stackaddr$prim54768, align 8
%stackaddr$prim54769 = alloca %struct.ScmObj*, align 8
%argslist53463$_37drop_45right471092 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs47120, %struct.ScmObj* %argslist53463$_37drop_45right471091)
store volatile %struct.ScmObj* %argslist53463$_37drop_45right471092, %struct.ScmObj** %stackaddr$prim54769, align 8
%stackaddr$prim54770 = alloca %struct.ScmObj*, align 8
%argslist53463$_37drop_45right471093 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48660, %struct.ScmObj* %argslist53463$_37drop_45right471092)
store volatile %struct.ScmObj* %argslist53463$_37drop_45right471093, %struct.ScmObj** %stackaddr$prim54770, align 8
%clofunc54771 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right47109)
musttail call tailcc void %clofunc54771(%struct.ScmObj* %_37drop_45right47109, %struct.ScmObj* %argslist53463$_37drop_45right471093)
ret void
}

define tailcc void @proc_clo$ae48660(%struct.ScmObj* %env$ae48660,%struct.ScmObj* %current_45args53452) {
%stackaddr$env-ref54772 = alloca %struct.ScmObj*, align 8
%k47413 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48660, i64 0)
store %struct.ScmObj* %k47413, %struct.ScmObj** %stackaddr$env-ref54772
%stackaddr$env-ref54773 = alloca %struct.ScmObj*, align 8
%fargs47120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48660, i64 1)
store %struct.ScmObj* %fargs47120, %struct.ScmObj** %stackaddr$env-ref54773
%stackaddr$env-ref54774 = alloca %struct.ScmObj*, align 8
%f47119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48660, i64 2)
store %struct.ScmObj* %f47119, %struct.ScmObj** %stackaddr$env-ref54774
%stackaddr$env-ref54775 = alloca %struct.ScmObj*, align 8
%_37last47112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48660, i64 3)
store %struct.ScmObj* %_37last47112, %struct.ScmObj** %stackaddr$env-ref54775
%stackaddr$prim54776 = alloca %struct.ScmObj*, align 8
%_95k47414 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53452)
store volatile %struct.ScmObj* %_95k47414, %struct.ScmObj** %stackaddr$prim54776, align 8
%stackaddr$prim54777 = alloca %struct.ScmObj*, align 8
%current_45args53453 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53452)
store volatile %struct.ScmObj* %current_45args53453, %struct.ScmObj** %stackaddr$prim54777, align 8
%stackaddr$prim54778 = alloca %struct.ScmObj*, align 8
%anf_45bind47242 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53453)
store volatile %struct.ScmObj* %anf_45bind47242, %struct.ScmObj** %stackaddr$prim54778, align 8
%stackaddr$makeclosure54779 = alloca %struct.ScmObj*, align 8
%fptrToInt54780 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48667 to i64
%ae48667 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54780)
store volatile %struct.ScmObj* %ae48667, %struct.ScmObj** %stackaddr$makeclosure54779, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48667, %struct.ScmObj* %k47413, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48667, %struct.ScmObj* %fargs47120, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48667, %struct.ScmObj* %_37last47112, i64 2)
%stackaddr$prim54781 = alloca %struct.ScmObj*, align 8
%cpsargs47418 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48667, %struct.ScmObj* %anf_45bind47242)
store volatile %struct.ScmObj* %cpsargs47418, %struct.ScmObj** %stackaddr$prim54781, align 8
%clofunc54782 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47119)
musttail call tailcc void %clofunc54782(%struct.ScmObj* %f47119, %struct.ScmObj* %cpsargs47418)
ret void
}

define tailcc void @proc_clo$ae48667(%struct.ScmObj* %env$ae48667,%struct.ScmObj* %current_45args53455) {
%stackaddr$env-ref54783 = alloca %struct.ScmObj*, align 8
%k47413 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48667, i64 0)
store %struct.ScmObj* %k47413, %struct.ScmObj** %stackaddr$env-ref54783
%stackaddr$env-ref54784 = alloca %struct.ScmObj*, align 8
%fargs47120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48667, i64 1)
store %struct.ScmObj* %fargs47120, %struct.ScmObj** %stackaddr$env-ref54784
%stackaddr$env-ref54785 = alloca %struct.ScmObj*, align 8
%_37last47112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48667, i64 2)
store %struct.ScmObj* %_37last47112, %struct.ScmObj** %stackaddr$env-ref54785
%stackaddr$prim54786 = alloca %struct.ScmObj*, align 8
%_95k47415 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53455)
store volatile %struct.ScmObj* %_95k47415, %struct.ScmObj** %stackaddr$prim54786, align 8
%stackaddr$prim54787 = alloca %struct.ScmObj*, align 8
%current_45args53456 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53455)
store volatile %struct.ScmObj* %current_45args53456, %struct.ScmObj** %stackaddr$prim54787, align 8
%stackaddr$prim54788 = alloca %struct.ScmObj*, align 8
%anf_45bind47243 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53456)
store volatile %struct.ScmObj* %anf_45bind47243, %struct.ScmObj** %stackaddr$prim54788, align 8
%stackaddr$makeclosure54789 = alloca %struct.ScmObj*, align 8
%fptrToInt54790 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48672 to i64
%ae48672 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54790)
store volatile %struct.ScmObj* %ae48672, %struct.ScmObj** %stackaddr$makeclosure54789, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48672, %struct.ScmObj* %k47413, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48672, %struct.ScmObj* %anf_45bind47243, i64 1)
%argslist53462$_37last471120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54791 = alloca %struct.ScmObj*, align 8
%argslist53462$_37last471121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs47120, %struct.ScmObj* %argslist53462$_37last471120)
store volatile %struct.ScmObj* %argslist53462$_37last471121, %struct.ScmObj** %stackaddr$prim54791, align 8
%stackaddr$prim54792 = alloca %struct.ScmObj*, align 8
%argslist53462$_37last471122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48672, %struct.ScmObj* %argslist53462$_37last471121)
store volatile %struct.ScmObj* %argslist53462$_37last471122, %struct.ScmObj** %stackaddr$prim54792, align 8
%clofunc54793 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last47112)
musttail call tailcc void %clofunc54793(%struct.ScmObj* %_37last47112, %struct.ScmObj* %argslist53462$_37last471122)
ret void
}

define tailcc void @proc_clo$ae48672(%struct.ScmObj* %env$ae48672,%struct.ScmObj* %current_45args53458) {
%stackaddr$env-ref54794 = alloca %struct.ScmObj*, align 8
%k47413 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48672, i64 0)
store %struct.ScmObj* %k47413, %struct.ScmObj** %stackaddr$env-ref54794
%stackaddr$env-ref54795 = alloca %struct.ScmObj*, align 8
%anf_45bind47243 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48672, i64 1)
store %struct.ScmObj* %anf_45bind47243, %struct.ScmObj** %stackaddr$env-ref54795
%stackaddr$prim54796 = alloca %struct.ScmObj*, align 8
%_95k47416 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53458)
store volatile %struct.ScmObj* %_95k47416, %struct.ScmObj** %stackaddr$prim54796, align 8
%stackaddr$prim54797 = alloca %struct.ScmObj*, align 8
%current_45args53459 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53458)
store volatile %struct.ScmObj* %current_45args53459, %struct.ScmObj** %stackaddr$prim54797, align 8
%stackaddr$prim54798 = alloca %struct.ScmObj*, align 8
%anf_45bind47244 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53459)
store volatile %struct.ScmObj* %anf_45bind47244, %struct.ScmObj** %stackaddr$prim54798, align 8
%stackaddr$prim54799 = alloca %struct.ScmObj*, align 8
%cpsprim47417 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47243, %struct.ScmObj* %anf_45bind47244)
store volatile %struct.ScmObj* %cpsprim47417, %struct.ScmObj** %stackaddr$prim54799, align 8
%ae48677 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53461$k474130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54800 = alloca %struct.ScmObj*, align 8
%argslist53461$k474131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47417, %struct.ScmObj* %argslist53461$k474130)
store volatile %struct.ScmObj* %argslist53461$k474131, %struct.ScmObj** %stackaddr$prim54800, align 8
%stackaddr$prim54801 = alloca %struct.ScmObj*, align 8
%argslist53461$k474132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48677, %struct.ScmObj* %argslist53461$k474131)
store volatile %struct.ScmObj* %argslist53461$k474132, %struct.ScmObj** %stackaddr$prim54801, align 8
%clofunc54802 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47413)
musttail call tailcc void %clofunc54802(%struct.ScmObj* %k47413, %struct.ScmObj* %argslist53461$k474132)
ret void
}

define tailcc void @proc_clo$ae48572(%struct.ScmObj* %env$ae48572,%struct.ScmObj* %current_45args53466) {
%stackaddr$env-ref54803 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48572, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54803
%stackaddr$prim54804 = alloca %struct.ScmObj*, align 8
%k47419 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53466)
store volatile %struct.ScmObj* %k47419, %struct.ScmObj** %stackaddr$prim54804, align 8
%stackaddr$prim54805 = alloca %struct.ScmObj*, align 8
%current_45args53467 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53466)
store volatile %struct.ScmObj* %current_45args53467, %struct.ScmObj** %stackaddr$prim54805, align 8
%stackaddr$prim54806 = alloca %struct.ScmObj*, align 8
%f47123 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53467)
store volatile %struct.ScmObj* %f47123, %struct.ScmObj** %stackaddr$prim54806, align 8
%stackaddr$prim54807 = alloca %struct.ScmObj*, align 8
%current_45args53468 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53467)
store volatile %struct.ScmObj* %current_45args53468, %struct.ScmObj** %stackaddr$prim54807, align 8
%stackaddr$prim54808 = alloca %struct.ScmObj*, align 8
%lst47122 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53468)
store volatile %struct.ScmObj* %lst47122, %struct.ScmObj** %stackaddr$prim54808, align 8
%stackaddr$makeclosure54809 = alloca %struct.ScmObj*, align 8
%fptrToInt54810 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48573 to i64
%ae48573 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54810)
store volatile %struct.ScmObj* %ae48573, %struct.ScmObj** %stackaddr$makeclosure54809, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48573, %struct.ScmObj* %lst47122, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48573, %struct.ScmObj* %_37foldr147090, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48573, %struct.ScmObj* %k47419, i64 2)
%ae48574 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54811 = alloca %struct.ScmObj*, align 8
%fptrToInt54812 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48575 to i64
%ae48575 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54812)
store volatile %struct.ScmObj* %ae48575, %struct.ScmObj** %stackaddr$makeclosure54811, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48575, %struct.ScmObj* %f47123, i64 0)
%argslist53483$ae485730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54813 = alloca %struct.ScmObj*, align 8
%argslist53483$ae485731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48575, %struct.ScmObj* %argslist53483$ae485730)
store volatile %struct.ScmObj* %argslist53483$ae485731, %struct.ScmObj** %stackaddr$prim54813, align 8
%stackaddr$prim54814 = alloca %struct.ScmObj*, align 8
%argslist53483$ae485732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48574, %struct.ScmObj* %argslist53483$ae485731)
store volatile %struct.ScmObj* %argslist53483$ae485732, %struct.ScmObj** %stackaddr$prim54814, align 8
%clofunc54815 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48573)
musttail call tailcc void %clofunc54815(%struct.ScmObj* %ae48573, %struct.ScmObj* %argslist53483$ae485732)
ret void
}

define tailcc void @proc_clo$ae48573(%struct.ScmObj* %env$ae48573,%struct.ScmObj* %current_45args53470) {
%stackaddr$env-ref54816 = alloca %struct.ScmObj*, align 8
%lst47122 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48573, i64 0)
store %struct.ScmObj* %lst47122, %struct.ScmObj** %stackaddr$env-ref54816
%stackaddr$env-ref54817 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48573, i64 1)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54817
%stackaddr$env-ref54818 = alloca %struct.ScmObj*, align 8
%k47419 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48573, i64 2)
store %struct.ScmObj* %k47419, %struct.ScmObj** %stackaddr$env-ref54818
%stackaddr$prim54819 = alloca %struct.ScmObj*, align 8
%_95k47420 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53470)
store volatile %struct.ScmObj* %_95k47420, %struct.ScmObj** %stackaddr$prim54819, align 8
%stackaddr$prim54820 = alloca %struct.ScmObj*, align 8
%current_45args53471 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53470)
store volatile %struct.ScmObj* %current_45args53471, %struct.ScmObj** %stackaddr$prim54820, align 8
%stackaddr$prim54821 = alloca %struct.ScmObj*, align 8
%anf_45bind47241 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53471)
store volatile %struct.ScmObj* %anf_45bind47241, %struct.ScmObj** %stackaddr$prim54821, align 8
%ae48607 = call %struct.ScmObj* @const_init_null()
%argslist53473$_37foldr1470900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54822 = alloca %struct.ScmObj*, align 8
%argslist53473$_37foldr1470901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47122, %struct.ScmObj* %argslist53473$_37foldr1470900)
store volatile %struct.ScmObj* %argslist53473$_37foldr1470901, %struct.ScmObj** %stackaddr$prim54822, align 8
%stackaddr$prim54823 = alloca %struct.ScmObj*, align 8
%argslist53473$_37foldr1470902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48607, %struct.ScmObj* %argslist53473$_37foldr1470901)
store volatile %struct.ScmObj* %argslist53473$_37foldr1470902, %struct.ScmObj** %stackaddr$prim54823, align 8
%stackaddr$prim54824 = alloca %struct.ScmObj*, align 8
%argslist53473$_37foldr1470903 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47241, %struct.ScmObj* %argslist53473$_37foldr1470902)
store volatile %struct.ScmObj* %argslist53473$_37foldr1470903, %struct.ScmObj** %stackaddr$prim54824, align 8
%stackaddr$prim54825 = alloca %struct.ScmObj*, align 8
%argslist53473$_37foldr1470904 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47419, %struct.ScmObj* %argslist53473$_37foldr1470903)
store volatile %struct.ScmObj* %argslist53473$_37foldr1470904, %struct.ScmObj** %stackaddr$prim54825, align 8
%clofunc54826 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147090)
musttail call tailcc void %clofunc54826(%struct.ScmObj* %_37foldr147090, %struct.ScmObj* %argslist53473$_37foldr1470904)
ret void
}

define tailcc void @proc_clo$ae48575(%struct.ScmObj* %env$ae48575,%struct.ScmObj* %current_45args53474) {
%stackaddr$env-ref54827 = alloca %struct.ScmObj*, align 8
%f47123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48575, i64 0)
store %struct.ScmObj* %f47123, %struct.ScmObj** %stackaddr$env-ref54827
%stackaddr$prim54828 = alloca %struct.ScmObj*, align 8
%k47421 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53474)
store volatile %struct.ScmObj* %k47421, %struct.ScmObj** %stackaddr$prim54828, align 8
%stackaddr$prim54829 = alloca %struct.ScmObj*, align 8
%current_45args53475 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53474)
store volatile %struct.ScmObj* %current_45args53475, %struct.ScmObj** %stackaddr$prim54829, align 8
%stackaddr$prim54830 = alloca %struct.ScmObj*, align 8
%v47125 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53475)
store volatile %struct.ScmObj* %v47125, %struct.ScmObj** %stackaddr$prim54830, align 8
%stackaddr$prim54831 = alloca %struct.ScmObj*, align 8
%current_45args53476 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53475)
store volatile %struct.ScmObj* %current_45args53476, %struct.ScmObj** %stackaddr$prim54831, align 8
%stackaddr$prim54832 = alloca %struct.ScmObj*, align 8
%r47124 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53476)
store volatile %struct.ScmObj* %r47124, %struct.ScmObj** %stackaddr$prim54832, align 8
%stackaddr$makeclosure54833 = alloca %struct.ScmObj*, align 8
%fptrToInt54834 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48577 to i64
%ae48577 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54834)
store volatile %struct.ScmObj* %ae48577, %struct.ScmObj** %stackaddr$makeclosure54833, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48577, %struct.ScmObj* %r47124, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48577, %struct.ScmObj* %k47421, i64 1)
%argslist53482$f471230 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54835 = alloca %struct.ScmObj*, align 8
%argslist53482$f471231 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v47125, %struct.ScmObj* %argslist53482$f471230)
store volatile %struct.ScmObj* %argslist53482$f471231, %struct.ScmObj** %stackaddr$prim54835, align 8
%stackaddr$prim54836 = alloca %struct.ScmObj*, align 8
%argslist53482$f471232 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48577, %struct.ScmObj* %argslist53482$f471231)
store volatile %struct.ScmObj* %argslist53482$f471232, %struct.ScmObj** %stackaddr$prim54836, align 8
%clofunc54837 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47123)
musttail call tailcc void %clofunc54837(%struct.ScmObj* %f47123, %struct.ScmObj* %argslist53482$f471232)
ret void
}

define tailcc void @proc_clo$ae48577(%struct.ScmObj* %env$ae48577,%struct.ScmObj* %current_45args53478) {
%stackaddr$env-ref54838 = alloca %struct.ScmObj*, align 8
%r47124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48577, i64 0)
store %struct.ScmObj* %r47124, %struct.ScmObj** %stackaddr$env-ref54838
%stackaddr$env-ref54839 = alloca %struct.ScmObj*, align 8
%k47421 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48577, i64 1)
store %struct.ScmObj* %k47421, %struct.ScmObj** %stackaddr$env-ref54839
%stackaddr$prim54840 = alloca %struct.ScmObj*, align 8
%_95k47422 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53478)
store volatile %struct.ScmObj* %_95k47422, %struct.ScmObj** %stackaddr$prim54840, align 8
%stackaddr$prim54841 = alloca %struct.ScmObj*, align 8
%current_45args53479 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53478)
store volatile %struct.ScmObj* %current_45args53479, %struct.ScmObj** %stackaddr$prim54841, align 8
%stackaddr$prim54842 = alloca %struct.ScmObj*, align 8
%anf_45bind47240 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53479)
store volatile %struct.ScmObj* %anf_45bind47240, %struct.ScmObj** %stackaddr$prim54842, align 8
%stackaddr$prim54843 = alloca %struct.ScmObj*, align 8
%cpsprim47423 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47240, %struct.ScmObj* %r47124)
store volatile %struct.ScmObj* %cpsprim47423, %struct.ScmObj** %stackaddr$prim54843, align 8
%ae48582 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53481$k474210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54844 = alloca %struct.ScmObj*, align 8
%argslist53481$k474211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47423, %struct.ScmObj* %argslist53481$k474210)
store volatile %struct.ScmObj* %argslist53481$k474211, %struct.ScmObj** %stackaddr$prim54844, align 8
%stackaddr$prim54845 = alloca %struct.ScmObj*, align 8
%argslist53481$k474212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48582, %struct.ScmObj* %argslist53481$k474211)
store volatile %struct.ScmObj* %argslist53481$k474212, %struct.ScmObj** %stackaddr$prim54845, align 8
%clofunc54846 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47421)
musttail call tailcc void %clofunc54846(%struct.ScmObj* %k47421, %struct.ScmObj* %argslist53481$k474212)
ret void
}

define tailcc void @proc_clo$ae48186(%struct.ScmObj* %env$ae48186,%struct.ScmObj* %current_45args53486) {
%stackaddr$env-ref54847 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48186, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54847
%stackaddr$env-ref54848 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48186, i64 1)
store %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$env-ref54848
%stackaddr$prim54849 = alloca %struct.ScmObj*, align 8
%k47424 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53486)
store volatile %struct.ScmObj* %k47424, %struct.ScmObj** %stackaddr$prim54849, align 8
%stackaddr$prim54850 = alloca %struct.ScmObj*, align 8
%current_45args53487 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53486)
store volatile %struct.ScmObj* %current_45args53487, %struct.ScmObj** %stackaddr$prim54850, align 8
%stackaddr$prim54851 = alloca %struct.ScmObj*, align 8
%_37foldr47096 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53487)
store volatile %struct.ScmObj* %_37foldr47096, %struct.ScmObj** %stackaddr$prim54851, align 8
%ae48188 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54852 = alloca %struct.ScmObj*, align 8
%fptrToInt54853 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48189 to i64
%ae48189 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54853)
store volatile %struct.ScmObj* %ae48189, %struct.ScmObj** %stackaddr$makeclosure54852, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48189, %struct.ScmObj* %_37foldr147090, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48189, %struct.ScmObj* %_37map147086, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48189, %struct.ScmObj* %_37foldr47096, i64 2)
%argslist53544$k474240 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54854 = alloca %struct.ScmObj*, align 8
%argslist53544$k474241 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48189, %struct.ScmObj* %argslist53544$k474240)
store volatile %struct.ScmObj* %argslist53544$k474241, %struct.ScmObj** %stackaddr$prim54854, align 8
%stackaddr$prim54855 = alloca %struct.ScmObj*, align 8
%argslist53544$k474242 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48188, %struct.ScmObj* %argslist53544$k474241)
store volatile %struct.ScmObj* %argslist53544$k474242, %struct.ScmObj** %stackaddr$prim54855, align 8
%clofunc54856 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47424)
musttail call tailcc void %clofunc54856(%struct.ScmObj* %k47424, %struct.ScmObj* %argslist53544$k474242)
ret void
}

define tailcc void @proc_clo$ae48189(%struct.ScmObj* %env$ae48189,%struct.ScmObj* %args4709747425) {
%stackaddr$env-ref54857 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48189, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54857
%stackaddr$env-ref54858 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48189, i64 1)
store %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$env-ref54858
%stackaddr$env-ref54859 = alloca %struct.ScmObj*, align 8
%_37foldr47096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48189, i64 2)
store %struct.ScmObj* %_37foldr47096, %struct.ScmObj** %stackaddr$env-ref54859
%stackaddr$prim54860 = alloca %struct.ScmObj*, align 8
%k47426 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4709747425)
store volatile %struct.ScmObj* %k47426, %struct.ScmObj** %stackaddr$prim54860, align 8
%stackaddr$prim54861 = alloca %struct.ScmObj*, align 8
%args47097 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4709747425)
store volatile %struct.ScmObj* %args47097, %struct.ScmObj** %stackaddr$prim54861, align 8
%stackaddr$prim54862 = alloca %struct.ScmObj*, align 8
%f47100 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47097)
store volatile %struct.ScmObj* %f47100, %struct.ScmObj** %stackaddr$prim54862, align 8
%stackaddr$prim54863 = alloca %struct.ScmObj*, align 8
%anf_45bind47227 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47097)
store volatile %struct.ScmObj* %anf_45bind47227, %struct.ScmObj** %stackaddr$prim54863, align 8
%stackaddr$prim54864 = alloca %struct.ScmObj*, align 8
%acc47099 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47227)
store volatile %struct.ScmObj* %acc47099, %struct.ScmObj** %stackaddr$prim54864, align 8
%stackaddr$prim54865 = alloca %struct.ScmObj*, align 8
%anf_45bind47228 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47097)
store volatile %struct.ScmObj* %anf_45bind47228, %struct.ScmObj** %stackaddr$prim54865, align 8
%stackaddr$prim54866 = alloca %struct.ScmObj*, align 8
%lsts47098 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47228)
store volatile %struct.ScmObj* %lsts47098, %struct.ScmObj** %stackaddr$prim54866, align 8
%stackaddr$makeclosure54867 = alloca %struct.ScmObj*, align 8
%fptrToInt54868 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48197 to i64
%ae48197 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt54868)
store volatile %struct.ScmObj* %ae48197, %struct.ScmObj** %stackaddr$makeclosure54867, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48197, %struct.ScmObj* %k47426, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48197, %struct.ScmObj* %_37foldr147090, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48197, %struct.ScmObj* %_37map147086, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48197, %struct.ScmObj* %f47100, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48197, %struct.ScmObj* %acc47099, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48197, %struct.ScmObj* %lsts47098, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48197, %struct.ScmObj* %_37foldr47096, i64 6)
%ae48198 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54869 = alloca %struct.ScmObj*, align 8
%fptrToInt54870 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48199 to i64
%ae48199 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54870)
store volatile %struct.ScmObj* %ae48199, %struct.ScmObj** %stackaddr$makeclosure54869, align 8
%argslist53543$ae481970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54871 = alloca %struct.ScmObj*, align 8
%argslist53543$ae481971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48199, %struct.ScmObj* %argslist53543$ae481970)
store volatile %struct.ScmObj* %argslist53543$ae481971, %struct.ScmObj** %stackaddr$prim54871, align 8
%stackaddr$prim54872 = alloca %struct.ScmObj*, align 8
%argslist53543$ae481972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48198, %struct.ScmObj* %argslist53543$ae481971)
store volatile %struct.ScmObj* %argslist53543$ae481972, %struct.ScmObj** %stackaddr$prim54872, align 8
%clofunc54873 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48197)
musttail call tailcc void %clofunc54873(%struct.ScmObj* %ae48197, %struct.ScmObj* %argslist53543$ae481972)
ret void
}

define tailcc void @proc_clo$ae48197(%struct.ScmObj* %env$ae48197,%struct.ScmObj* %current_45args53489) {
%stackaddr$env-ref54874 = alloca %struct.ScmObj*, align 8
%k47426 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48197, i64 0)
store %struct.ScmObj* %k47426, %struct.ScmObj** %stackaddr$env-ref54874
%stackaddr$env-ref54875 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48197, i64 1)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54875
%stackaddr$env-ref54876 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48197, i64 2)
store %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$env-ref54876
%stackaddr$env-ref54877 = alloca %struct.ScmObj*, align 8
%f47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48197, i64 3)
store %struct.ScmObj* %f47100, %struct.ScmObj** %stackaddr$env-ref54877
%stackaddr$env-ref54878 = alloca %struct.ScmObj*, align 8
%acc47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48197, i64 4)
store %struct.ScmObj* %acc47099, %struct.ScmObj** %stackaddr$env-ref54878
%stackaddr$env-ref54879 = alloca %struct.ScmObj*, align 8
%lsts47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48197, i64 5)
store %struct.ScmObj* %lsts47098, %struct.ScmObj** %stackaddr$env-ref54879
%stackaddr$env-ref54880 = alloca %struct.ScmObj*, align 8
%_37foldr47096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48197, i64 6)
store %struct.ScmObj* %_37foldr47096, %struct.ScmObj** %stackaddr$env-ref54880
%stackaddr$prim54881 = alloca %struct.ScmObj*, align 8
%_95k47427 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53489)
store volatile %struct.ScmObj* %_95k47427, %struct.ScmObj** %stackaddr$prim54881, align 8
%stackaddr$prim54882 = alloca %struct.ScmObj*, align 8
%current_45args53490 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53489)
store volatile %struct.ScmObj* %current_45args53490, %struct.ScmObj** %stackaddr$prim54882, align 8
%stackaddr$prim54883 = alloca %struct.ScmObj*, align 8
%anf_45bind47229 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53490)
store volatile %struct.ScmObj* %anf_45bind47229, %struct.ScmObj** %stackaddr$prim54883, align 8
%stackaddr$makeclosure54884 = alloca %struct.ScmObj*, align 8
%fptrToInt54885 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48229 to i64
%ae48229 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt54885)
store volatile %struct.ScmObj* %ae48229, %struct.ScmObj** %stackaddr$makeclosure54884, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48229, %struct.ScmObj* %k47426, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48229, %struct.ScmObj* %_37foldr147090, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48229, %struct.ScmObj* %_37map147086, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48229, %struct.ScmObj* %f47100, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48229, %struct.ScmObj* %acc47099, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48229, %struct.ScmObj* %lsts47098, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48229, %struct.ScmObj* %_37foldr47096, i64 6)
%ae48231 = call %struct.ScmObj* @const_init_false()
%argslist53536$_37foldr1470900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54886 = alloca %struct.ScmObj*, align 8
%argslist53536$_37foldr1470901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47098, %struct.ScmObj* %argslist53536$_37foldr1470900)
store volatile %struct.ScmObj* %argslist53536$_37foldr1470901, %struct.ScmObj** %stackaddr$prim54886, align 8
%stackaddr$prim54887 = alloca %struct.ScmObj*, align 8
%argslist53536$_37foldr1470902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48231, %struct.ScmObj* %argslist53536$_37foldr1470901)
store volatile %struct.ScmObj* %argslist53536$_37foldr1470902, %struct.ScmObj** %stackaddr$prim54887, align 8
%stackaddr$prim54888 = alloca %struct.ScmObj*, align 8
%argslist53536$_37foldr1470903 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47229, %struct.ScmObj* %argslist53536$_37foldr1470902)
store volatile %struct.ScmObj* %argslist53536$_37foldr1470903, %struct.ScmObj** %stackaddr$prim54888, align 8
%stackaddr$prim54889 = alloca %struct.ScmObj*, align 8
%argslist53536$_37foldr1470904 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48229, %struct.ScmObj* %argslist53536$_37foldr1470903)
store volatile %struct.ScmObj* %argslist53536$_37foldr1470904, %struct.ScmObj** %stackaddr$prim54889, align 8
%clofunc54890 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147090)
musttail call tailcc void %clofunc54890(%struct.ScmObj* %_37foldr147090, %struct.ScmObj* %argslist53536$_37foldr1470904)
ret void
}

define tailcc void @proc_clo$ae48229(%struct.ScmObj* %env$ae48229,%struct.ScmObj* %current_45args53492) {
%stackaddr$env-ref54891 = alloca %struct.ScmObj*, align 8
%k47426 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48229, i64 0)
store %struct.ScmObj* %k47426, %struct.ScmObj** %stackaddr$env-ref54891
%stackaddr$env-ref54892 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48229, i64 1)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54892
%stackaddr$env-ref54893 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48229, i64 2)
store %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$env-ref54893
%stackaddr$env-ref54894 = alloca %struct.ScmObj*, align 8
%f47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48229, i64 3)
store %struct.ScmObj* %f47100, %struct.ScmObj** %stackaddr$env-ref54894
%stackaddr$env-ref54895 = alloca %struct.ScmObj*, align 8
%acc47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48229, i64 4)
store %struct.ScmObj* %acc47099, %struct.ScmObj** %stackaddr$env-ref54895
%stackaddr$env-ref54896 = alloca %struct.ScmObj*, align 8
%lsts47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48229, i64 5)
store %struct.ScmObj* %lsts47098, %struct.ScmObj** %stackaddr$env-ref54896
%stackaddr$env-ref54897 = alloca %struct.ScmObj*, align 8
%_37foldr47096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48229, i64 6)
store %struct.ScmObj* %_37foldr47096, %struct.ScmObj** %stackaddr$env-ref54897
%stackaddr$prim54898 = alloca %struct.ScmObj*, align 8
%_95k47428 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53492)
store volatile %struct.ScmObj* %_95k47428, %struct.ScmObj** %stackaddr$prim54898, align 8
%stackaddr$prim54899 = alloca %struct.ScmObj*, align 8
%current_45args53493 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53492)
store volatile %struct.ScmObj* %current_45args53493, %struct.ScmObj** %stackaddr$prim54899, align 8
%stackaddr$prim54900 = alloca %struct.ScmObj*, align 8
%anf_45bind47230 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53493)
store volatile %struct.ScmObj* %anf_45bind47230, %struct.ScmObj** %stackaddr$prim54900, align 8
%truthy$cmp54901 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47230)
%cmp$cmp54901 = icmp eq i64 %truthy$cmp54901, 1
br i1 %cmp$cmp54901, label %truebranch$cmp54901, label %falsebranch$cmp54901
truebranch$cmp54901:
%ae48240 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53495$k474260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54902 = alloca %struct.ScmObj*, align 8
%argslist53495$k474261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47099, %struct.ScmObj* %argslist53495$k474260)
store volatile %struct.ScmObj* %argslist53495$k474261, %struct.ScmObj** %stackaddr$prim54902, align 8
%stackaddr$prim54903 = alloca %struct.ScmObj*, align 8
%argslist53495$k474262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48240, %struct.ScmObj* %argslist53495$k474261)
store volatile %struct.ScmObj* %argslist53495$k474262, %struct.ScmObj** %stackaddr$prim54903, align 8
%clofunc54904 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47426)
musttail call tailcc void %clofunc54904(%struct.ScmObj* %k47426, %struct.ScmObj* %argslist53495$k474262)
ret void
falsebranch$cmp54901:
%stackaddr$makeclosure54905 = alloca %struct.ScmObj*, align 8
%fptrToInt54906 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48245 to i64
%ae48245 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt54906)
store volatile %struct.ScmObj* %ae48245, %struct.ScmObj** %stackaddr$makeclosure54905, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48245, %struct.ScmObj* %k47426, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48245, %struct.ScmObj* %_37foldr147090, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48245, %struct.ScmObj* %_37map147086, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48245, %struct.ScmObj* %f47100, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48245, %struct.ScmObj* %acc47099, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48245, %struct.ScmObj* %lsts47098, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48245, %struct.ScmObj* %_37foldr47096, i64 6)
%ae48246 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54907 = alloca %struct.ScmObj*, align 8
%fptrToInt54908 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48247 to i64
%ae48247 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54908)
store volatile %struct.ScmObj* %ae48247, %struct.ScmObj** %stackaddr$makeclosure54907, align 8
%argslist53535$ae482450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54909 = alloca %struct.ScmObj*, align 8
%argslist53535$ae482451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48247, %struct.ScmObj* %argslist53535$ae482450)
store volatile %struct.ScmObj* %argslist53535$ae482451, %struct.ScmObj** %stackaddr$prim54909, align 8
%stackaddr$prim54910 = alloca %struct.ScmObj*, align 8
%argslist53535$ae482452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48246, %struct.ScmObj* %argslist53535$ae482451)
store volatile %struct.ScmObj* %argslist53535$ae482452, %struct.ScmObj** %stackaddr$prim54910, align 8
%clofunc54911 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48245)
musttail call tailcc void %clofunc54911(%struct.ScmObj* %ae48245, %struct.ScmObj* %argslist53535$ae482452)
ret void
}

define tailcc void @proc_clo$ae48245(%struct.ScmObj* %env$ae48245,%struct.ScmObj* %current_45args53496) {
%stackaddr$env-ref54912 = alloca %struct.ScmObj*, align 8
%k47426 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48245, i64 0)
store %struct.ScmObj* %k47426, %struct.ScmObj** %stackaddr$env-ref54912
%stackaddr$env-ref54913 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48245, i64 1)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54913
%stackaddr$env-ref54914 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48245, i64 2)
store %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$env-ref54914
%stackaddr$env-ref54915 = alloca %struct.ScmObj*, align 8
%f47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48245, i64 3)
store %struct.ScmObj* %f47100, %struct.ScmObj** %stackaddr$env-ref54915
%stackaddr$env-ref54916 = alloca %struct.ScmObj*, align 8
%acc47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48245, i64 4)
store %struct.ScmObj* %acc47099, %struct.ScmObj** %stackaddr$env-ref54916
%stackaddr$env-ref54917 = alloca %struct.ScmObj*, align 8
%lsts47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48245, i64 5)
store %struct.ScmObj* %lsts47098, %struct.ScmObj** %stackaddr$env-ref54917
%stackaddr$env-ref54918 = alloca %struct.ScmObj*, align 8
%_37foldr47096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48245, i64 6)
store %struct.ScmObj* %_37foldr47096, %struct.ScmObj** %stackaddr$env-ref54918
%stackaddr$prim54919 = alloca %struct.ScmObj*, align 8
%_95k47429 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53496)
store volatile %struct.ScmObj* %_95k47429, %struct.ScmObj** %stackaddr$prim54919, align 8
%stackaddr$prim54920 = alloca %struct.ScmObj*, align 8
%current_45args53497 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53496)
store volatile %struct.ScmObj* %current_45args53497, %struct.ScmObj** %stackaddr$prim54920, align 8
%stackaddr$prim54921 = alloca %struct.ScmObj*, align 8
%anf_45bind47231 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53497)
store volatile %struct.ScmObj* %anf_45bind47231, %struct.ScmObj** %stackaddr$prim54921, align 8
%stackaddr$makeclosure54922 = alloca %struct.ScmObj*, align 8
%fptrToInt54923 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48266 to i64
%ae48266 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt54923)
store volatile %struct.ScmObj* %ae48266, %struct.ScmObj** %stackaddr$makeclosure54922, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48266, %struct.ScmObj* %k47426, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48266, %struct.ScmObj* %_37foldr147090, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48266, %struct.ScmObj* %_37map147086, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48266, %struct.ScmObj* %f47100, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48266, %struct.ScmObj* %acc47099, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48266, %struct.ScmObj* %lsts47098, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48266, %struct.ScmObj* %_37foldr47096, i64 6)
%argslist53530$_37map1470860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54924 = alloca %struct.ScmObj*, align 8
%argslist53530$_37map1470861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47098, %struct.ScmObj* %argslist53530$_37map1470860)
store volatile %struct.ScmObj* %argslist53530$_37map1470861, %struct.ScmObj** %stackaddr$prim54924, align 8
%stackaddr$prim54925 = alloca %struct.ScmObj*, align 8
%argslist53530$_37map1470862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47231, %struct.ScmObj* %argslist53530$_37map1470861)
store volatile %struct.ScmObj* %argslist53530$_37map1470862, %struct.ScmObj** %stackaddr$prim54925, align 8
%stackaddr$prim54926 = alloca %struct.ScmObj*, align 8
%argslist53530$_37map1470863 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48266, %struct.ScmObj* %argslist53530$_37map1470862)
store volatile %struct.ScmObj* %argslist53530$_37map1470863, %struct.ScmObj** %stackaddr$prim54926, align 8
%clofunc54927 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147086)
musttail call tailcc void %clofunc54927(%struct.ScmObj* %_37map147086, %struct.ScmObj* %argslist53530$_37map1470863)
ret void
}

define tailcc void @proc_clo$ae48266(%struct.ScmObj* %env$ae48266,%struct.ScmObj* %current_45args53499) {
%stackaddr$env-ref54928 = alloca %struct.ScmObj*, align 8
%k47426 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48266, i64 0)
store %struct.ScmObj* %k47426, %struct.ScmObj** %stackaddr$env-ref54928
%stackaddr$env-ref54929 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48266, i64 1)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54929
%stackaddr$env-ref54930 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48266, i64 2)
store %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$env-ref54930
%stackaddr$env-ref54931 = alloca %struct.ScmObj*, align 8
%f47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48266, i64 3)
store %struct.ScmObj* %f47100, %struct.ScmObj** %stackaddr$env-ref54931
%stackaddr$env-ref54932 = alloca %struct.ScmObj*, align 8
%acc47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48266, i64 4)
store %struct.ScmObj* %acc47099, %struct.ScmObj** %stackaddr$env-ref54932
%stackaddr$env-ref54933 = alloca %struct.ScmObj*, align 8
%lsts47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48266, i64 5)
store %struct.ScmObj* %lsts47098, %struct.ScmObj** %stackaddr$env-ref54933
%stackaddr$env-ref54934 = alloca %struct.ScmObj*, align 8
%_37foldr47096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48266, i64 6)
store %struct.ScmObj* %_37foldr47096, %struct.ScmObj** %stackaddr$env-ref54934
%stackaddr$prim54935 = alloca %struct.ScmObj*, align 8
%_95k47430 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53499)
store volatile %struct.ScmObj* %_95k47430, %struct.ScmObj** %stackaddr$prim54935, align 8
%stackaddr$prim54936 = alloca %struct.ScmObj*, align 8
%current_45args53500 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53499)
store volatile %struct.ScmObj* %current_45args53500, %struct.ScmObj** %stackaddr$prim54936, align 8
%stackaddr$prim54937 = alloca %struct.ScmObj*, align 8
%lsts_4347105 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53500)
store volatile %struct.ScmObj* %lsts_4347105, %struct.ScmObj** %stackaddr$prim54937, align 8
%stackaddr$makeclosure54938 = alloca %struct.ScmObj*, align 8
%fptrToInt54939 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48269 to i64
%ae48269 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt54939)
store volatile %struct.ScmObj* %ae48269, %struct.ScmObj** %stackaddr$makeclosure54938, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48269, %struct.ScmObj* %k47426, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48269, %struct.ScmObj* %_37foldr147090, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48269, %struct.ScmObj* %lsts_4347105, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48269, %struct.ScmObj* %_37map147086, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48269, %struct.ScmObj* %f47100, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48269, %struct.ScmObj* %acc47099, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48269, %struct.ScmObj* %lsts47098, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae48269, %struct.ScmObj* %_37foldr47096, i64 7)
%ae48270 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54940 = alloca %struct.ScmObj*, align 8
%fptrToInt54941 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48271 to i64
%ae48271 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54941)
store volatile %struct.ScmObj* %ae48271, %struct.ScmObj** %stackaddr$makeclosure54940, align 8
%argslist53529$ae482690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54942 = alloca %struct.ScmObj*, align 8
%argslist53529$ae482691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48271, %struct.ScmObj* %argslist53529$ae482690)
store volatile %struct.ScmObj* %argslist53529$ae482691, %struct.ScmObj** %stackaddr$prim54942, align 8
%stackaddr$prim54943 = alloca %struct.ScmObj*, align 8
%argslist53529$ae482692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48270, %struct.ScmObj* %argslist53529$ae482691)
store volatile %struct.ScmObj* %argslist53529$ae482692, %struct.ScmObj** %stackaddr$prim54943, align 8
%clofunc54944 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48269)
musttail call tailcc void %clofunc54944(%struct.ScmObj* %ae48269, %struct.ScmObj* %argslist53529$ae482692)
ret void
}

define tailcc void @proc_clo$ae48269(%struct.ScmObj* %env$ae48269,%struct.ScmObj* %current_45args53502) {
%stackaddr$env-ref54945 = alloca %struct.ScmObj*, align 8
%k47426 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48269, i64 0)
store %struct.ScmObj* %k47426, %struct.ScmObj** %stackaddr$env-ref54945
%stackaddr$env-ref54946 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48269, i64 1)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54946
%stackaddr$env-ref54947 = alloca %struct.ScmObj*, align 8
%lsts_4347105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48269, i64 2)
store %struct.ScmObj* %lsts_4347105, %struct.ScmObj** %stackaddr$env-ref54947
%stackaddr$env-ref54948 = alloca %struct.ScmObj*, align 8
%_37map147086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48269, i64 3)
store %struct.ScmObj* %_37map147086, %struct.ScmObj** %stackaddr$env-ref54948
%stackaddr$env-ref54949 = alloca %struct.ScmObj*, align 8
%f47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48269, i64 4)
store %struct.ScmObj* %f47100, %struct.ScmObj** %stackaddr$env-ref54949
%stackaddr$env-ref54950 = alloca %struct.ScmObj*, align 8
%acc47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48269, i64 5)
store %struct.ScmObj* %acc47099, %struct.ScmObj** %stackaddr$env-ref54950
%stackaddr$env-ref54951 = alloca %struct.ScmObj*, align 8
%lsts47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48269, i64 6)
store %struct.ScmObj* %lsts47098, %struct.ScmObj** %stackaddr$env-ref54951
%stackaddr$env-ref54952 = alloca %struct.ScmObj*, align 8
%_37foldr47096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48269, i64 7)
store %struct.ScmObj* %_37foldr47096, %struct.ScmObj** %stackaddr$env-ref54952
%stackaddr$prim54953 = alloca %struct.ScmObj*, align 8
%_95k47431 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53502)
store volatile %struct.ScmObj* %_95k47431, %struct.ScmObj** %stackaddr$prim54953, align 8
%stackaddr$prim54954 = alloca %struct.ScmObj*, align 8
%current_45args53503 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53502)
store volatile %struct.ScmObj* %current_45args53503, %struct.ScmObj** %stackaddr$prim54954, align 8
%stackaddr$prim54955 = alloca %struct.ScmObj*, align 8
%anf_45bind47232 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53503)
store volatile %struct.ScmObj* %anf_45bind47232, %struct.ScmObj** %stackaddr$prim54955, align 8
%stackaddr$makeclosure54956 = alloca %struct.ScmObj*, align 8
%fptrToInt54957 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48290 to i64
%ae48290 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt54957)
store volatile %struct.ScmObj* %ae48290, %struct.ScmObj** %stackaddr$makeclosure54956, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48290, %struct.ScmObj* %k47426, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48290, %struct.ScmObj* %_37foldr147090, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48290, %struct.ScmObj* %lsts_4347105, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48290, %struct.ScmObj* %f47100, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48290, %struct.ScmObj* %acc47099, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48290, %struct.ScmObj* %_37foldr47096, i64 5)
%argslist53524$_37map1470860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54958 = alloca %struct.ScmObj*, align 8
%argslist53524$_37map1470861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47098, %struct.ScmObj* %argslist53524$_37map1470860)
store volatile %struct.ScmObj* %argslist53524$_37map1470861, %struct.ScmObj** %stackaddr$prim54958, align 8
%stackaddr$prim54959 = alloca %struct.ScmObj*, align 8
%argslist53524$_37map1470862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47232, %struct.ScmObj* %argslist53524$_37map1470861)
store volatile %struct.ScmObj* %argslist53524$_37map1470862, %struct.ScmObj** %stackaddr$prim54959, align 8
%stackaddr$prim54960 = alloca %struct.ScmObj*, align 8
%argslist53524$_37map1470863 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48290, %struct.ScmObj* %argslist53524$_37map1470862)
store volatile %struct.ScmObj* %argslist53524$_37map1470863, %struct.ScmObj** %stackaddr$prim54960, align 8
%clofunc54961 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147086)
musttail call tailcc void %clofunc54961(%struct.ScmObj* %_37map147086, %struct.ScmObj* %argslist53524$_37map1470863)
ret void
}

define tailcc void @proc_clo$ae48290(%struct.ScmObj* %env$ae48290,%struct.ScmObj* %current_45args53505) {
%stackaddr$env-ref54962 = alloca %struct.ScmObj*, align 8
%k47426 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48290, i64 0)
store %struct.ScmObj* %k47426, %struct.ScmObj** %stackaddr$env-ref54962
%stackaddr$env-ref54963 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48290, i64 1)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54963
%stackaddr$env-ref54964 = alloca %struct.ScmObj*, align 8
%lsts_4347105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48290, i64 2)
store %struct.ScmObj* %lsts_4347105, %struct.ScmObj** %stackaddr$env-ref54964
%stackaddr$env-ref54965 = alloca %struct.ScmObj*, align 8
%f47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48290, i64 3)
store %struct.ScmObj* %f47100, %struct.ScmObj** %stackaddr$env-ref54965
%stackaddr$env-ref54966 = alloca %struct.ScmObj*, align 8
%acc47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48290, i64 4)
store %struct.ScmObj* %acc47099, %struct.ScmObj** %stackaddr$env-ref54966
%stackaddr$env-ref54967 = alloca %struct.ScmObj*, align 8
%_37foldr47096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48290, i64 5)
store %struct.ScmObj* %_37foldr47096, %struct.ScmObj** %stackaddr$env-ref54967
%stackaddr$prim54968 = alloca %struct.ScmObj*, align 8
%_95k47432 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53505)
store volatile %struct.ScmObj* %_95k47432, %struct.ScmObj** %stackaddr$prim54968, align 8
%stackaddr$prim54969 = alloca %struct.ScmObj*, align 8
%current_45args53506 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53505)
store volatile %struct.ScmObj* %current_45args53506, %struct.ScmObj** %stackaddr$prim54969, align 8
%stackaddr$prim54970 = alloca %struct.ScmObj*, align 8
%vs47103 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53506)
store volatile %struct.ScmObj* %vs47103, %struct.ScmObj** %stackaddr$prim54970, align 8
%stackaddr$makeclosure54971 = alloca %struct.ScmObj*, align 8
%fptrToInt54972 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48293 to i64
%ae48293 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt54972)
store volatile %struct.ScmObj* %ae48293, %struct.ScmObj** %stackaddr$makeclosure54971, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48293, %struct.ScmObj* %k47426, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48293, %struct.ScmObj* %_37foldr147090, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48293, %struct.ScmObj* %lsts_4347105, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48293, %struct.ScmObj* %vs47103, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48293, %struct.ScmObj* %f47100, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48293, %struct.ScmObj* %acc47099, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48293, %struct.ScmObj* %_37foldr47096, i64 6)
%ae48294 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54973 = alloca %struct.ScmObj*, align 8
%fptrToInt54974 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48295 to i64
%ae48295 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54974)
store volatile %struct.ScmObj* %ae48295, %struct.ScmObj** %stackaddr$makeclosure54973, align 8
%argslist53523$ae482930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54975 = alloca %struct.ScmObj*, align 8
%argslist53523$ae482931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48295, %struct.ScmObj* %argslist53523$ae482930)
store volatile %struct.ScmObj* %argslist53523$ae482931, %struct.ScmObj** %stackaddr$prim54975, align 8
%stackaddr$prim54976 = alloca %struct.ScmObj*, align 8
%argslist53523$ae482932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48294, %struct.ScmObj* %argslist53523$ae482931)
store volatile %struct.ScmObj* %argslist53523$ae482932, %struct.ScmObj** %stackaddr$prim54976, align 8
%clofunc54977 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48293)
musttail call tailcc void %clofunc54977(%struct.ScmObj* %ae48293, %struct.ScmObj* %argslist53523$ae482932)
ret void
}

define tailcc void @proc_clo$ae48293(%struct.ScmObj* %env$ae48293,%struct.ScmObj* %current_45args53508) {
%stackaddr$env-ref54978 = alloca %struct.ScmObj*, align 8
%k47426 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48293, i64 0)
store %struct.ScmObj* %k47426, %struct.ScmObj** %stackaddr$env-ref54978
%stackaddr$env-ref54979 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48293, i64 1)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54979
%stackaddr$env-ref54980 = alloca %struct.ScmObj*, align 8
%lsts_4347105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48293, i64 2)
store %struct.ScmObj* %lsts_4347105, %struct.ScmObj** %stackaddr$env-ref54980
%stackaddr$env-ref54981 = alloca %struct.ScmObj*, align 8
%vs47103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48293, i64 3)
store %struct.ScmObj* %vs47103, %struct.ScmObj** %stackaddr$env-ref54981
%stackaddr$env-ref54982 = alloca %struct.ScmObj*, align 8
%f47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48293, i64 4)
store %struct.ScmObj* %f47100, %struct.ScmObj** %stackaddr$env-ref54982
%stackaddr$env-ref54983 = alloca %struct.ScmObj*, align 8
%acc47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48293, i64 5)
store %struct.ScmObj* %acc47099, %struct.ScmObj** %stackaddr$env-ref54983
%stackaddr$env-ref54984 = alloca %struct.ScmObj*, align 8
%_37foldr47096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48293, i64 6)
store %struct.ScmObj* %_37foldr47096, %struct.ScmObj** %stackaddr$env-ref54984
%stackaddr$prim54985 = alloca %struct.ScmObj*, align 8
%_95k47433 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53508)
store volatile %struct.ScmObj* %_95k47433, %struct.ScmObj** %stackaddr$prim54985, align 8
%stackaddr$prim54986 = alloca %struct.ScmObj*, align 8
%current_45args53509 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53508)
store volatile %struct.ScmObj* %current_45args53509, %struct.ScmObj** %stackaddr$prim54986, align 8
%stackaddr$prim54987 = alloca %struct.ScmObj*, align 8
%anf_45bind47233 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53509)
store volatile %struct.ScmObj* %anf_45bind47233, %struct.ScmObj** %stackaddr$prim54987, align 8
%stackaddr$prim54988 = alloca %struct.ScmObj*, align 8
%anf_45bind47234 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47099, %struct.ScmObj* %lsts_4347105)
store volatile %struct.ScmObj* %anf_45bind47234, %struct.ScmObj** %stackaddr$prim54988, align 8
%stackaddr$prim54989 = alloca %struct.ScmObj*, align 8
%anf_45bind47235 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47100, %struct.ScmObj* %anf_45bind47234)
store volatile %struct.ScmObj* %anf_45bind47235, %struct.ScmObj** %stackaddr$prim54989, align 8
%stackaddr$makeclosure54990 = alloca %struct.ScmObj*, align 8
%fptrToInt54991 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48319 to i64
%ae48319 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54991)
store volatile %struct.ScmObj* %ae48319, %struct.ScmObj** %stackaddr$makeclosure54990, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48319, %struct.ScmObj* %k47426, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48319, %struct.ScmObj* %_37foldr147090, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48319, %struct.ScmObj* %anf_45bind47233, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48319, %struct.ScmObj* %vs47103, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48319, %struct.ScmObj* %f47100, i64 4)
%stackaddr$prim54992 = alloca %struct.ScmObj*, align 8
%cpsargs47437 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48319, %struct.ScmObj* %anf_45bind47235)
store volatile %struct.ScmObj* %cpsargs47437, %struct.ScmObj** %stackaddr$prim54992, align 8
%clofunc54993 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr47096)
musttail call tailcc void %clofunc54993(%struct.ScmObj* %_37foldr47096, %struct.ScmObj* %cpsargs47437)
ret void
}

define tailcc void @proc_clo$ae48319(%struct.ScmObj* %env$ae48319,%struct.ScmObj* %current_45args53511) {
%stackaddr$env-ref54994 = alloca %struct.ScmObj*, align 8
%k47426 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48319, i64 0)
store %struct.ScmObj* %k47426, %struct.ScmObj** %stackaddr$env-ref54994
%stackaddr$env-ref54995 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48319, i64 1)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref54995
%stackaddr$env-ref54996 = alloca %struct.ScmObj*, align 8
%anf_45bind47233 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48319, i64 2)
store %struct.ScmObj* %anf_45bind47233, %struct.ScmObj** %stackaddr$env-ref54996
%stackaddr$env-ref54997 = alloca %struct.ScmObj*, align 8
%vs47103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48319, i64 3)
store %struct.ScmObj* %vs47103, %struct.ScmObj** %stackaddr$env-ref54997
%stackaddr$env-ref54998 = alloca %struct.ScmObj*, align 8
%f47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48319, i64 4)
store %struct.ScmObj* %f47100, %struct.ScmObj** %stackaddr$env-ref54998
%stackaddr$prim54999 = alloca %struct.ScmObj*, align 8
%_95k47434 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53511)
store volatile %struct.ScmObj* %_95k47434, %struct.ScmObj** %stackaddr$prim54999, align 8
%stackaddr$prim55000 = alloca %struct.ScmObj*, align 8
%current_45args53512 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53511)
store volatile %struct.ScmObj* %current_45args53512, %struct.ScmObj** %stackaddr$prim55000, align 8
%stackaddr$prim55001 = alloca %struct.ScmObj*, align 8
%anf_45bind47236 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53512)
store volatile %struct.ScmObj* %anf_45bind47236, %struct.ScmObj** %stackaddr$prim55001, align 8
%ae48324 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55002 = alloca %struct.ScmObj*, align 8
%anf_45bind47237 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47236, %struct.ScmObj* %ae48324)
store volatile %struct.ScmObj* %anf_45bind47237, %struct.ScmObj** %stackaddr$prim55002, align 8
%stackaddr$makeclosure55003 = alloca %struct.ScmObj*, align 8
%fptrToInt55004 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48326 to i64
%ae48326 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55004)
store volatile %struct.ScmObj* %ae48326, %struct.ScmObj** %stackaddr$makeclosure55003, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48326, %struct.ScmObj* %k47426, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48326, %struct.ScmObj* %f47100, i64 1)
%argslist53517$_37foldr1470900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55005 = alloca %struct.ScmObj*, align 8
%argslist53517$_37foldr1470901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs47103, %struct.ScmObj* %argslist53517$_37foldr1470900)
store volatile %struct.ScmObj* %argslist53517$_37foldr1470901, %struct.ScmObj** %stackaddr$prim55005, align 8
%stackaddr$prim55006 = alloca %struct.ScmObj*, align 8
%argslist53517$_37foldr1470902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47237, %struct.ScmObj* %argslist53517$_37foldr1470901)
store volatile %struct.ScmObj* %argslist53517$_37foldr1470902, %struct.ScmObj** %stackaddr$prim55006, align 8
%stackaddr$prim55007 = alloca %struct.ScmObj*, align 8
%argslist53517$_37foldr1470903 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47233, %struct.ScmObj* %argslist53517$_37foldr1470902)
store volatile %struct.ScmObj* %argslist53517$_37foldr1470903, %struct.ScmObj** %stackaddr$prim55007, align 8
%stackaddr$prim55008 = alloca %struct.ScmObj*, align 8
%argslist53517$_37foldr1470904 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48326, %struct.ScmObj* %argslist53517$_37foldr1470903)
store volatile %struct.ScmObj* %argslist53517$_37foldr1470904, %struct.ScmObj** %stackaddr$prim55008, align 8
%clofunc55009 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147090)
musttail call tailcc void %clofunc55009(%struct.ScmObj* %_37foldr147090, %struct.ScmObj* %argslist53517$_37foldr1470904)
ret void
}

define tailcc void @proc_clo$ae48326(%struct.ScmObj* %env$ae48326,%struct.ScmObj* %current_45args53514) {
%stackaddr$env-ref55010 = alloca %struct.ScmObj*, align 8
%k47426 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48326, i64 0)
store %struct.ScmObj* %k47426, %struct.ScmObj** %stackaddr$env-ref55010
%stackaddr$env-ref55011 = alloca %struct.ScmObj*, align 8
%f47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48326, i64 1)
store %struct.ScmObj* %f47100, %struct.ScmObj** %stackaddr$env-ref55011
%stackaddr$prim55012 = alloca %struct.ScmObj*, align 8
%_95k47435 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53514)
store volatile %struct.ScmObj* %_95k47435, %struct.ScmObj** %stackaddr$prim55012, align 8
%stackaddr$prim55013 = alloca %struct.ScmObj*, align 8
%current_45args53515 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53514)
store volatile %struct.ScmObj* %current_45args53515, %struct.ScmObj** %stackaddr$prim55013, align 8
%stackaddr$prim55014 = alloca %struct.ScmObj*, align 8
%anf_45bind47238 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53515)
store volatile %struct.ScmObj* %anf_45bind47238, %struct.ScmObj** %stackaddr$prim55014, align 8
%stackaddr$prim55015 = alloca %struct.ScmObj*, align 8
%cpsargs47436 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47426, %struct.ScmObj* %anf_45bind47238)
store volatile %struct.ScmObj* %cpsargs47436, %struct.ScmObj** %stackaddr$prim55015, align 8
%clofunc55016 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47100)
musttail call tailcc void %clofunc55016(%struct.ScmObj* %f47100, %struct.ScmObj* %cpsargs47436)
ret void
}

define tailcc void @proc_clo$ae48295(%struct.ScmObj* %env$ae48295,%struct.ScmObj* %current_45args53518) {
%stackaddr$prim55017 = alloca %struct.ScmObj*, align 8
%k47438 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53518)
store volatile %struct.ScmObj* %k47438, %struct.ScmObj** %stackaddr$prim55017, align 8
%stackaddr$prim55018 = alloca %struct.ScmObj*, align 8
%current_45args53519 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53518)
store volatile %struct.ScmObj* %current_45args53519, %struct.ScmObj** %stackaddr$prim55018, align 8
%stackaddr$prim55019 = alloca %struct.ScmObj*, align 8
%a47108 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53519)
store volatile %struct.ScmObj* %a47108, %struct.ScmObj** %stackaddr$prim55019, align 8
%stackaddr$prim55020 = alloca %struct.ScmObj*, align 8
%current_45args53520 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53519)
store volatile %struct.ScmObj* %current_45args53520, %struct.ScmObj** %stackaddr$prim55020, align 8
%stackaddr$prim55021 = alloca %struct.ScmObj*, align 8
%b47107 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53520)
store volatile %struct.ScmObj* %b47107, %struct.ScmObj** %stackaddr$prim55021, align 8
%stackaddr$prim55022 = alloca %struct.ScmObj*, align 8
%cpsprim47439 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a47108, %struct.ScmObj* %b47107)
store volatile %struct.ScmObj* %cpsprim47439, %struct.ScmObj** %stackaddr$prim55022, align 8
%ae48299 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53522$k474380 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55023 = alloca %struct.ScmObj*, align 8
%argslist53522$k474381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47439, %struct.ScmObj* %argslist53522$k474380)
store volatile %struct.ScmObj* %argslist53522$k474381, %struct.ScmObj** %stackaddr$prim55023, align 8
%stackaddr$prim55024 = alloca %struct.ScmObj*, align 8
%argslist53522$k474382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48299, %struct.ScmObj* %argslist53522$k474381)
store volatile %struct.ScmObj* %argslist53522$k474382, %struct.ScmObj** %stackaddr$prim55024, align 8
%clofunc55025 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47438)
musttail call tailcc void %clofunc55025(%struct.ScmObj* %k47438, %struct.ScmObj* %argslist53522$k474382)
ret void
}

define tailcc void @proc_clo$ae48271(%struct.ScmObj* %env$ae48271,%struct.ScmObj* %current_45args53525) {
%stackaddr$prim55026 = alloca %struct.ScmObj*, align 8
%k47440 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53525)
store volatile %struct.ScmObj* %k47440, %struct.ScmObj** %stackaddr$prim55026, align 8
%stackaddr$prim55027 = alloca %struct.ScmObj*, align 8
%current_45args53526 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53525)
store volatile %struct.ScmObj* %current_45args53526, %struct.ScmObj** %stackaddr$prim55027, align 8
%stackaddr$prim55028 = alloca %struct.ScmObj*, align 8
%x47104 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53526)
store volatile %struct.ScmObj* %x47104, %struct.ScmObj** %stackaddr$prim55028, align 8
%stackaddr$prim55029 = alloca %struct.ScmObj*, align 8
%cpsprim47441 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47104)
store volatile %struct.ScmObj* %cpsprim47441, %struct.ScmObj** %stackaddr$prim55029, align 8
%ae48274 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53528$k474400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55030 = alloca %struct.ScmObj*, align 8
%argslist53528$k474401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47441, %struct.ScmObj* %argslist53528$k474400)
store volatile %struct.ScmObj* %argslist53528$k474401, %struct.ScmObj** %stackaddr$prim55030, align 8
%stackaddr$prim55031 = alloca %struct.ScmObj*, align 8
%argslist53528$k474402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48274, %struct.ScmObj* %argslist53528$k474401)
store volatile %struct.ScmObj* %argslist53528$k474402, %struct.ScmObj** %stackaddr$prim55031, align 8
%clofunc55032 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47440)
musttail call tailcc void %clofunc55032(%struct.ScmObj* %k47440, %struct.ScmObj* %argslist53528$k474402)
ret void
}

define tailcc void @proc_clo$ae48247(%struct.ScmObj* %env$ae48247,%struct.ScmObj* %current_45args53531) {
%stackaddr$prim55033 = alloca %struct.ScmObj*, align 8
%k47442 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53531)
store volatile %struct.ScmObj* %k47442, %struct.ScmObj** %stackaddr$prim55033, align 8
%stackaddr$prim55034 = alloca %struct.ScmObj*, align 8
%current_45args53532 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53531)
store volatile %struct.ScmObj* %current_45args53532, %struct.ScmObj** %stackaddr$prim55034, align 8
%stackaddr$prim55035 = alloca %struct.ScmObj*, align 8
%x47106 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53532)
store volatile %struct.ScmObj* %x47106, %struct.ScmObj** %stackaddr$prim55035, align 8
%stackaddr$prim55036 = alloca %struct.ScmObj*, align 8
%cpsprim47443 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47106)
store volatile %struct.ScmObj* %cpsprim47443, %struct.ScmObj** %stackaddr$prim55036, align 8
%ae48250 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53534$k474420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55037 = alloca %struct.ScmObj*, align 8
%argslist53534$k474421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47443, %struct.ScmObj* %argslist53534$k474420)
store volatile %struct.ScmObj* %argslist53534$k474421, %struct.ScmObj** %stackaddr$prim55037, align 8
%stackaddr$prim55038 = alloca %struct.ScmObj*, align 8
%argslist53534$k474422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48250, %struct.ScmObj* %argslist53534$k474421)
store volatile %struct.ScmObj* %argslist53534$k474422, %struct.ScmObj** %stackaddr$prim55038, align 8
%clofunc55039 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47442)
musttail call tailcc void %clofunc55039(%struct.ScmObj* %k47442, %struct.ScmObj* %argslist53534$k474422)
ret void
}

define tailcc void @proc_clo$ae48199(%struct.ScmObj* %env$ae48199,%struct.ScmObj* %current_45args53537) {
%stackaddr$prim55040 = alloca %struct.ScmObj*, align 8
%k47444 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53537)
store volatile %struct.ScmObj* %k47444, %struct.ScmObj** %stackaddr$prim55040, align 8
%stackaddr$prim55041 = alloca %struct.ScmObj*, align 8
%current_45args53538 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53537)
store volatile %struct.ScmObj* %current_45args53538, %struct.ScmObj** %stackaddr$prim55041, align 8
%stackaddr$prim55042 = alloca %struct.ScmObj*, align 8
%lst47102 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53538)
store volatile %struct.ScmObj* %lst47102, %struct.ScmObj** %stackaddr$prim55042, align 8
%stackaddr$prim55043 = alloca %struct.ScmObj*, align 8
%current_45args53539 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53538)
store volatile %struct.ScmObj* %current_45args53539, %struct.ScmObj** %stackaddr$prim55043, align 8
%stackaddr$prim55044 = alloca %struct.ScmObj*, align 8
%b47101 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53539)
store volatile %struct.ScmObj* %b47101, %struct.ScmObj** %stackaddr$prim55044, align 8
%truthy$cmp55045 = call i64 @is_truthy_value(%struct.ScmObj* %b47101)
%cmp$cmp55045 = icmp eq i64 %truthy$cmp55045, 1
br i1 %cmp$cmp55045, label %truebranch$cmp55045, label %falsebranch$cmp55045
truebranch$cmp55045:
%ae48202 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53541$k474440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55046 = alloca %struct.ScmObj*, align 8
%argslist53541$k474441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b47101, %struct.ScmObj* %argslist53541$k474440)
store volatile %struct.ScmObj* %argslist53541$k474441, %struct.ScmObj** %stackaddr$prim55046, align 8
%stackaddr$prim55047 = alloca %struct.ScmObj*, align 8
%argslist53541$k474442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48202, %struct.ScmObj* %argslist53541$k474441)
store volatile %struct.ScmObj* %argslist53541$k474442, %struct.ScmObj** %stackaddr$prim55047, align 8
%clofunc55048 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47444)
musttail call tailcc void %clofunc55048(%struct.ScmObj* %k47444, %struct.ScmObj* %argslist53541$k474442)
ret void
falsebranch$cmp55045:
%stackaddr$prim55049 = alloca %struct.ScmObj*, align 8
%cpsprim47445 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47102)
store volatile %struct.ScmObj* %cpsprim47445, %struct.ScmObj** %stackaddr$prim55049, align 8
%ae48209 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53542$k474440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55050 = alloca %struct.ScmObj*, align 8
%argslist53542$k474441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47445, %struct.ScmObj* %argslist53542$k474440)
store volatile %struct.ScmObj* %argslist53542$k474441, %struct.ScmObj** %stackaddr$prim55050, align 8
%stackaddr$prim55051 = alloca %struct.ScmObj*, align 8
%argslist53542$k474442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48209, %struct.ScmObj* %argslist53542$k474441)
store volatile %struct.ScmObj* %argslist53542$k474442, %struct.ScmObj** %stackaddr$prim55051, align 8
%clofunc55052 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47444)
musttail call tailcc void %clofunc55052(%struct.ScmObj* %k47444, %struct.ScmObj* %argslist53542$k474442)
ret void
}

define tailcc void @proc_clo$ae48156(%struct.ScmObj* %env$ae48156,%struct.ScmObj* %current_45args53546) {
%stackaddr$env-ref55053 = alloca %struct.ScmObj*, align 8
%_37take47082 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48156, i64 0)
store %struct.ScmObj* %_37take47082, %struct.ScmObj** %stackaddr$env-ref55053
%stackaddr$env-ref55054 = alloca %struct.ScmObj*, align 8
%_37length47079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48156, i64 1)
store %struct.ScmObj* %_37length47079, %struct.ScmObj** %stackaddr$env-ref55054
%stackaddr$prim55055 = alloca %struct.ScmObj*, align 8
%k47446 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53546)
store volatile %struct.ScmObj* %k47446, %struct.ScmObj** %stackaddr$prim55055, align 8
%stackaddr$prim55056 = alloca %struct.ScmObj*, align 8
%current_45args53547 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53546)
store volatile %struct.ScmObj* %current_45args53547, %struct.ScmObj** %stackaddr$prim55056, align 8
%stackaddr$prim55057 = alloca %struct.ScmObj*, align 8
%lst47111 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53547)
store volatile %struct.ScmObj* %lst47111, %struct.ScmObj** %stackaddr$prim55057, align 8
%stackaddr$prim55058 = alloca %struct.ScmObj*, align 8
%current_45args53548 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53547)
store volatile %struct.ScmObj* %current_45args53548, %struct.ScmObj** %stackaddr$prim55058, align 8
%stackaddr$prim55059 = alloca %struct.ScmObj*, align 8
%n47110 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53548)
store volatile %struct.ScmObj* %n47110, %struct.ScmObj** %stackaddr$prim55059, align 8
%stackaddr$makeclosure55060 = alloca %struct.ScmObj*, align 8
%fptrToInt55061 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48158 to i64
%ae48158 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55061)
store volatile %struct.ScmObj* %ae48158, %struct.ScmObj** %stackaddr$makeclosure55060, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48158, %struct.ScmObj* %k47446, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48158, %struct.ScmObj* %n47110, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48158, %struct.ScmObj* %_37take47082, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48158, %struct.ScmObj* %lst47111, i64 3)
%argslist53554$_37length470790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55062 = alloca %struct.ScmObj*, align 8
%argslist53554$_37length470791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47111, %struct.ScmObj* %argslist53554$_37length470790)
store volatile %struct.ScmObj* %argslist53554$_37length470791, %struct.ScmObj** %stackaddr$prim55062, align 8
%stackaddr$prim55063 = alloca %struct.ScmObj*, align 8
%argslist53554$_37length470792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48158, %struct.ScmObj* %argslist53554$_37length470791)
store volatile %struct.ScmObj* %argslist53554$_37length470792, %struct.ScmObj** %stackaddr$prim55063, align 8
%clofunc55064 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length47079)
musttail call tailcc void %clofunc55064(%struct.ScmObj* %_37length47079, %struct.ScmObj* %argslist53554$_37length470792)
ret void
}

define tailcc void @proc_clo$ae48158(%struct.ScmObj* %env$ae48158,%struct.ScmObj* %current_45args53550) {
%stackaddr$env-ref55065 = alloca %struct.ScmObj*, align 8
%k47446 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48158, i64 0)
store %struct.ScmObj* %k47446, %struct.ScmObj** %stackaddr$env-ref55065
%stackaddr$env-ref55066 = alloca %struct.ScmObj*, align 8
%n47110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48158, i64 1)
store %struct.ScmObj* %n47110, %struct.ScmObj** %stackaddr$env-ref55066
%stackaddr$env-ref55067 = alloca %struct.ScmObj*, align 8
%_37take47082 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48158, i64 2)
store %struct.ScmObj* %_37take47082, %struct.ScmObj** %stackaddr$env-ref55067
%stackaddr$env-ref55068 = alloca %struct.ScmObj*, align 8
%lst47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48158, i64 3)
store %struct.ScmObj* %lst47111, %struct.ScmObj** %stackaddr$env-ref55068
%stackaddr$prim55069 = alloca %struct.ScmObj*, align 8
%_95k47447 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53550)
store volatile %struct.ScmObj* %_95k47447, %struct.ScmObj** %stackaddr$prim55069, align 8
%stackaddr$prim55070 = alloca %struct.ScmObj*, align 8
%current_45args53551 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53550)
store volatile %struct.ScmObj* %current_45args53551, %struct.ScmObj** %stackaddr$prim55070, align 8
%stackaddr$prim55071 = alloca %struct.ScmObj*, align 8
%anf_45bind47225 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53551)
store volatile %struct.ScmObj* %anf_45bind47225, %struct.ScmObj** %stackaddr$prim55071, align 8
%stackaddr$prim55072 = alloca %struct.ScmObj*, align 8
%anf_45bind47226 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind47225, %struct.ScmObj* %n47110)
store volatile %struct.ScmObj* %anf_45bind47226, %struct.ScmObj** %stackaddr$prim55072, align 8
%argslist53553$_37take470820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55073 = alloca %struct.ScmObj*, align 8
%argslist53553$_37take470821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47226, %struct.ScmObj* %argslist53553$_37take470820)
store volatile %struct.ScmObj* %argslist53553$_37take470821, %struct.ScmObj** %stackaddr$prim55073, align 8
%stackaddr$prim55074 = alloca %struct.ScmObj*, align 8
%argslist53553$_37take470822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47111, %struct.ScmObj* %argslist53553$_37take470821)
store volatile %struct.ScmObj* %argslist53553$_37take470822, %struct.ScmObj** %stackaddr$prim55074, align 8
%stackaddr$prim55075 = alloca %struct.ScmObj*, align 8
%argslist53553$_37take470823 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47446, %struct.ScmObj* %argslist53553$_37take470822)
store volatile %struct.ScmObj* %argslist53553$_37take470823, %struct.ScmObj** %stackaddr$prim55075, align 8
%clofunc55076 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take47082)
musttail call tailcc void %clofunc55076(%struct.ScmObj* %_37take47082, %struct.ScmObj* %argslist53553$_37take470823)
ret void
}

define tailcc void @proc_clo$ae48102(%struct.ScmObj* %env$ae48102,%struct.ScmObj* %current_45args53556) {
%stackaddr$env-ref55077 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48102, i64 0)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref55077
%stackaddr$prim55078 = alloca %struct.ScmObj*, align 8
%k47448 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53556)
store volatile %struct.ScmObj* %k47448, %struct.ScmObj** %stackaddr$prim55078, align 8
%stackaddr$prim55079 = alloca %struct.ScmObj*, align 8
%current_45args53557 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53556)
store volatile %struct.ScmObj* %current_45args53557, %struct.ScmObj** %stackaddr$prim55079, align 8
%stackaddr$prim55080 = alloca %struct.ScmObj*, align 8
%lst47113 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53557)
store volatile %struct.ScmObj* %lst47113, %struct.ScmObj** %stackaddr$prim55080, align 8
%stackaddr$makeclosure55081 = alloca %struct.ScmObj*, align 8
%fptrToInt55082 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48103 to i64
%ae48103 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55082)
store volatile %struct.ScmObj* %ae48103, %struct.ScmObj** %stackaddr$makeclosure55081, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48103, %struct.ScmObj* %_37foldl147074, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48103, %struct.ScmObj* %lst47113, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48103, %struct.ScmObj* %k47448, i64 2)
%ae48104 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55083 = alloca %struct.ScmObj*, align 8
%fptrToInt55084 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48105 to i64
%ae48105 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55084)
store volatile %struct.ScmObj* %ae48105, %struct.ScmObj** %stackaddr$makeclosure55083, align 8
%argslist53568$ae481030 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55085 = alloca %struct.ScmObj*, align 8
%argslist53568$ae481031 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48105, %struct.ScmObj* %argslist53568$ae481030)
store volatile %struct.ScmObj* %argslist53568$ae481031, %struct.ScmObj** %stackaddr$prim55085, align 8
%stackaddr$prim55086 = alloca %struct.ScmObj*, align 8
%argslist53568$ae481032 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48104, %struct.ScmObj* %argslist53568$ae481031)
store volatile %struct.ScmObj* %argslist53568$ae481032, %struct.ScmObj** %stackaddr$prim55086, align 8
%clofunc55087 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48103)
musttail call tailcc void %clofunc55087(%struct.ScmObj* %ae48103, %struct.ScmObj* %argslist53568$ae481032)
ret void
}

define tailcc void @proc_clo$ae48103(%struct.ScmObj* %env$ae48103,%struct.ScmObj* %current_45args53559) {
%stackaddr$env-ref55088 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48103, i64 0)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref55088
%stackaddr$env-ref55089 = alloca %struct.ScmObj*, align 8
%lst47113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48103, i64 1)
store %struct.ScmObj* %lst47113, %struct.ScmObj** %stackaddr$env-ref55089
%stackaddr$env-ref55090 = alloca %struct.ScmObj*, align 8
%k47448 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48103, i64 2)
store %struct.ScmObj* %k47448, %struct.ScmObj** %stackaddr$env-ref55090
%stackaddr$prim55091 = alloca %struct.ScmObj*, align 8
%_95k47449 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53559)
store volatile %struct.ScmObj* %_95k47449, %struct.ScmObj** %stackaddr$prim55091, align 8
%stackaddr$prim55092 = alloca %struct.ScmObj*, align 8
%current_45args53560 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53559)
store volatile %struct.ScmObj* %current_45args53560, %struct.ScmObj** %stackaddr$prim55092, align 8
%stackaddr$prim55093 = alloca %struct.ScmObj*, align 8
%anf_45bind47224 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53560)
store volatile %struct.ScmObj* %anf_45bind47224, %struct.ScmObj** %stackaddr$prim55093, align 8
%ae48124 = call %struct.ScmObj* @const_init_null()
%argslist53562$_37foldl1470740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55094 = alloca %struct.ScmObj*, align 8
%argslist53562$_37foldl1470741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47113, %struct.ScmObj* %argslist53562$_37foldl1470740)
store volatile %struct.ScmObj* %argslist53562$_37foldl1470741, %struct.ScmObj** %stackaddr$prim55094, align 8
%stackaddr$prim55095 = alloca %struct.ScmObj*, align 8
%argslist53562$_37foldl1470742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48124, %struct.ScmObj* %argslist53562$_37foldl1470741)
store volatile %struct.ScmObj* %argslist53562$_37foldl1470742, %struct.ScmObj** %stackaddr$prim55095, align 8
%stackaddr$prim55096 = alloca %struct.ScmObj*, align 8
%argslist53562$_37foldl1470743 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47224, %struct.ScmObj* %argslist53562$_37foldl1470742)
store volatile %struct.ScmObj* %argslist53562$_37foldl1470743, %struct.ScmObj** %stackaddr$prim55096, align 8
%stackaddr$prim55097 = alloca %struct.ScmObj*, align 8
%argslist53562$_37foldl1470744 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47448, %struct.ScmObj* %argslist53562$_37foldl1470743)
store volatile %struct.ScmObj* %argslist53562$_37foldl1470744, %struct.ScmObj** %stackaddr$prim55097, align 8
%clofunc55098 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147074)
musttail call tailcc void %clofunc55098(%struct.ScmObj* %_37foldl147074, %struct.ScmObj* %argslist53562$_37foldl1470744)
ret void
}

define tailcc void @proc_clo$ae48105(%struct.ScmObj* %env$ae48105,%struct.ScmObj* %current_45args53563) {
%stackaddr$prim55099 = alloca %struct.ScmObj*, align 8
%k47450 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53563)
store volatile %struct.ScmObj* %k47450, %struct.ScmObj** %stackaddr$prim55099, align 8
%stackaddr$prim55100 = alloca %struct.ScmObj*, align 8
%current_45args53564 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53563)
store volatile %struct.ScmObj* %current_45args53564, %struct.ScmObj** %stackaddr$prim55100, align 8
%stackaddr$prim55101 = alloca %struct.ScmObj*, align 8
%x47115 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53564)
store volatile %struct.ScmObj* %x47115, %struct.ScmObj** %stackaddr$prim55101, align 8
%stackaddr$prim55102 = alloca %struct.ScmObj*, align 8
%current_45args53565 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53564)
store volatile %struct.ScmObj* %current_45args53565, %struct.ScmObj** %stackaddr$prim55102, align 8
%stackaddr$prim55103 = alloca %struct.ScmObj*, align 8
%y47114 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53565)
store volatile %struct.ScmObj* %y47114, %struct.ScmObj** %stackaddr$prim55103, align 8
%ae48107 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53567$k474500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55104 = alloca %struct.ScmObj*, align 8
%argslist53567$k474501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x47115, %struct.ScmObj* %argslist53567$k474500)
store volatile %struct.ScmObj* %argslist53567$k474501, %struct.ScmObj** %stackaddr$prim55104, align 8
%stackaddr$prim55105 = alloca %struct.ScmObj*, align 8
%argslist53567$k474502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48107, %struct.ScmObj* %argslist53567$k474501)
store volatile %struct.ScmObj* %argslist53567$k474502, %struct.ScmObj** %stackaddr$prim55105, align 8
%clofunc55106 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47450)
musttail call tailcc void %clofunc55106(%struct.ScmObj* %k47450, %struct.ScmObj* %argslist53567$k474502)
ret void
}

define tailcc void @proc_clo$ae48023(%struct.ScmObj* %env$ae48023,%struct.ScmObj* %current_45args53571) {
%stackaddr$prim55107 = alloca %struct.ScmObj*, align 8
%k47451 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53571)
store volatile %struct.ScmObj* %k47451, %struct.ScmObj** %stackaddr$prim55107, align 8
%stackaddr$prim55108 = alloca %struct.ScmObj*, align 8
%current_45args53572 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53571)
store volatile %struct.ScmObj* %current_45args53572, %struct.ScmObj** %stackaddr$prim55108, align 8
%stackaddr$prim55109 = alloca %struct.ScmObj*, align 8
%_37foldl147075 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53572)
store volatile %struct.ScmObj* %_37foldl147075, %struct.ScmObj** %stackaddr$prim55109, align 8
%ae48025 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55110 = alloca %struct.ScmObj*, align 8
%fptrToInt55111 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48026 to i64
%ae48026 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55111)
store volatile %struct.ScmObj* %ae48026, %struct.ScmObj** %stackaddr$makeclosure55110, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48026, %struct.ScmObj* %_37foldl147075, i64 0)
%argslist53585$k474510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55112 = alloca %struct.ScmObj*, align 8
%argslist53585$k474511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48026, %struct.ScmObj* %argslist53585$k474510)
store volatile %struct.ScmObj* %argslist53585$k474511, %struct.ScmObj** %stackaddr$prim55112, align 8
%stackaddr$prim55113 = alloca %struct.ScmObj*, align 8
%argslist53585$k474512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48025, %struct.ScmObj* %argslist53585$k474511)
store volatile %struct.ScmObj* %argslist53585$k474512, %struct.ScmObj** %stackaddr$prim55113, align 8
%clofunc55114 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47451)
musttail call tailcc void %clofunc55114(%struct.ScmObj* %k47451, %struct.ScmObj* %argslist53585$k474512)
ret void
}

define tailcc void @proc_clo$ae48026(%struct.ScmObj* %env$ae48026,%struct.ScmObj* %current_45args53574) {
%stackaddr$env-ref55115 = alloca %struct.ScmObj*, align 8
%_37foldl147075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48026, i64 0)
store %struct.ScmObj* %_37foldl147075, %struct.ScmObj** %stackaddr$env-ref55115
%stackaddr$prim55116 = alloca %struct.ScmObj*, align 8
%k47452 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53574)
store volatile %struct.ScmObj* %k47452, %struct.ScmObj** %stackaddr$prim55116, align 8
%stackaddr$prim55117 = alloca %struct.ScmObj*, align 8
%current_45args53575 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53574)
store volatile %struct.ScmObj* %current_45args53575, %struct.ScmObj** %stackaddr$prim55117, align 8
%stackaddr$prim55118 = alloca %struct.ScmObj*, align 8
%f47078 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53575)
store volatile %struct.ScmObj* %f47078, %struct.ScmObj** %stackaddr$prim55118, align 8
%stackaddr$prim55119 = alloca %struct.ScmObj*, align 8
%current_45args53576 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53575)
store volatile %struct.ScmObj* %current_45args53576, %struct.ScmObj** %stackaddr$prim55119, align 8
%stackaddr$prim55120 = alloca %struct.ScmObj*, align 8
%acc47077 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53576)
store volatile %struct.ScmObj* %acc47077, %struct.ScmObj** %stackaddr$prim55120, align 8
%stackaddr$prim55121 = alloca %struct.ScmObj*, align 8
%current_45args53577 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53576)
store volatile %struct.ScmObj* %current_45args53577, %struct.ScmObj** %stackaddr$prim55121, align 8
%stackaddr$prim55122 = alloca %struct.ScmObj*, align 8
%lst47076 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53577)
store volatile %struct.ScmObj* %lst47076, %struct.ScmObj** %stackaddr$prim55122, align 8
%stackaddr$prim55123 = alloca %struct.ScmObj*, align 8
%anf_45bind47219 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47076)
store volatile %struct.ScmObj* %anf_45bind47219, %struct.ScmObj** %stackaddr$prim55123, align 8
%truthy$cmp55124 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47219)
%cmp$cmp55124 = icmp eq i64 %truthy$cmp55124, 1
br i1 %cmp$cmp55124, label %truebranch$cmp55124, label %falsebranch$cmp55124
truebranch$cmp55124:
%ae48030 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53579$k474520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55125 = alloca %struct.ScmObj*, align 8
%argslist53579$k474521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47077, %struct.ScmObj* %argslist53579$k474520)
store volatile %struct.ScmObj* %argslist53579$k474521, %struct.ScmObj** %stackaddr$prim55125, align 8
%stackaddr$prim55126 = alloca %struct.ScmObj*, align 8
%argslist53579$k474522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48030, %struct.ScmObj* %argslist53579$k474521)
store volatile %struct.ScmObj* %argslist53579$k474522, %struct.ScmObj** %stackaddr$prim55126, align 8
%clofunc55127 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47452)
musttail call tailcc void %clofunc55127(%struct.ScmObj* %k47452, %struct.ScmObj* %argslist53579$k474522)
ret void
falsebranch$cmp55124:
%stackaddr$prim55128 = alloca %struct.ScmObj*, align 8
%anf_45bind47220 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47076)
store volatile %struct.ScmObj* %anf_45bind47220, %struct.ScmObj** %stackaddr$prim55128, align 8
%stackaddr$makeclosure55129 = alloca %struct.ScmObj*, align 8
%fptrToInt55130 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48037 to i64
%ae48037 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55130)
store volatile %struct.ScmObj* %ae48037, %struct.ScmObj** %stackaddr$makeclosure55129, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48037, %struct.ScmObj* %f47078, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48037, %struct.ScmObj* %lst47076, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48037, %struct.ScmObj* %_37foldl147075, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48037, %struct.ScmObj* %k47452, i64 3)
%argslist53584$f470780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55131 = alloca %struct.ScmObj*, align 8
%argslist53584$f470781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47077, %struct.ScmObj* %argslist53584$f470780)
store volatile %struct.ScmObj* %argslist53584$f470781, %struct.ScmObj** %stackaddr$prim55131, align 8
%stackaddr$prim55132 = alloca %struct.ScmObj*, align 8
%argslist53584$f470782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47220, %struct.ScmObj* %argslist53584$f470781)
store volatile %struct.ScmObj* %argslist53584$f470782, %struct.ScmObj** %stackaddr$prim55132, align 8
%stackaddr$prim55133 = alloca %struct.ScmObj*, align 8
%argslist53584$f470783 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48037, %struct.ScmObj* %argslist53584$f470782)
store volatile %struct.ScmObj* %argslist53584$f470783, %struct.ScmObj** %stackaddr$prim55133, align 8
%clofunc55134 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47078)
musttail call tailcc void %clofunc55134(%struct.ScmObj* %f47078, %struct.ScmObj* %argslist53584$f470783)
ret void
}

define tailcc void @proc_clo$ae48037(%struct.ScmObj* %env$ae48037,%struct.ScmObj* %current_45args53580) {
%stackaddr$env-ref55135 = alloca %struct.ScmObj*, align 8
%f47078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48037, i64 0)
store %struct.ScmObj* %f47078, %struct.ScmObj** %stackaddr$env-ref55135
%stackaddr$env-ref55136 = alloca %struct.ScmObj*, align 8
%lst47076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48037, i64 1)
store %struct.ScmObj* %lst47076, %struct.ScmObj** %stackaddr$env-ref55136
%stackaddr$env-ref55137 = alloca %struct.ScmObj*, align 8
%_37foldl147075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48037, i64 2)
store %struct.ScmObj* %_37foldl147075, %struct.ScmObj** %stackaddr$env-ref55137
%stackaddr$env-ref55138 = alloca %struct.ScmObj*, align 8
%k47452 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48037, i64 3)
store %struct.ScmObj* %k47452, %struct.ScmObj** %stackaddr$env-ref55138
%stackaddr$prim55139 = alloca %struct.ScmObj*, align 8
%_95k47453 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53580)
store volatile %struct.ScmObj* %_95k47453, %struct.ScmObj** %stackaddr$prim55139, align 8
%stackaddr$prim55140 = alloca %struct.ScmObj*, align 8
%current_45args53581 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53580)
store volatile %struct.ScmObj* %current_45args53581, %struct.ScmObj** %stackaddr$prim55140, align 8
%stackaddr$prim55141 = alloca %struct.ScmObj*, align 8
%anf_45bind47221 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53581)
store volatile %struct.ScmObj* %anf_45bind47221, %struct.ScmObj** %stackaddr$prim55141, align 8
%stackaddr$prim55142 = alloca %struct.ScmObj*, align 8
%anf_45bind47222 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47076)
store volatile %struct.ScmObj* %anf_45bind47222, %struct.ScmObj** %stackaddr$prim55142, align 8
%argslist53583$_37foldl1470750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55143 = alloca %struct.ScmObj*, align 8
%argslist53583$_37foldl1470751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47222, %struct.ScmObj* %argslist53583$_37foldl1470750)
store volatile %struct.ScmObj* %argslist53583$_37foldl1470751, %struct.ScmObj** %stackaddr$prim55143, align 8
%stackaddr$prim55144 = alloca %struct.ScmObj*, align 8
%argslist53583$_37foldl1470752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47221, %struct.ScmObj* %argslist53583$_37foldl1470751)
store volatile %struct.ScmObj* %argslist53583$_37foldl1470752, %struct.ScmObj** %stackaddr$prim55144, align 8
%stackaddr$prim55145 = alloca %struct.ScmObj*, align 8
%argslist53583$_37foldl1470753 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47078, %struct.ScmObj* %argslist53583$_37foldl1470752)
store volatile %struct.ScmObj* %argslist53583$_37foldl1470753, %struct.ScmObj** %stackaddr$prim55145, align 8
%stackaddr$prim55146 = alloca %struct.ScmObj*, align 8
%argslist53583$_37foldl1470754 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47452, %struct.ScmObj* %argslist53583$_37foldl1470753)
store volatile %struct.ScmObj* %argslist53583$_37foldl1470754, %struct.ScmObj** %stackaddr$prim55146, align 8
%clofunc55147 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147075)
musttail call tailcc void %clofunc55147(%struct.ScmObj* %_37foldl147075, %struct.ScmObj* %argslist53583$_37foldl1470754)
ret void
}

define tailcc void @proc_clo$ae47940(%struct.ScmObj* %env$ae47940,%struct.ScmObj* %current_45args53588) {
%stackaddr$prim55148 = alloca %struct.ScmObj*, align 8
%k47454 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53588)
store volatile %struct.ScmObj* %k47454, %struct.ScmObj** %stackaddr$prim55148, align 8
%stackaddr$prim55149 = alloca %struct.ScmObj*, align 8
%current_45args53589 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53588)
store volatile %struct.ScmObj* %current_45args53589, %struct.ScmObj** %stackaddr$prim55149, align 8
%stackaddr$prim55150 = alloca %struct.ScmObj*, align 8
%_37length47080 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53589)
store volatile %struct.ScmObj* %_37length47080, %struct.ScmObj** %stackaddr$prim55150, align 8
%ae47942 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55151 = alloca %struct.ScmObj*, align 8
%fptrToInt55152 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47943 to i64
%ae47943 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55152)
store volatile %struct.ScmObj* %ae47943, %struct.ScmObj** %stackaddr$makeclosure55151, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47943, %struct.ScmObj* %_37length47080, i64 0)
%argslist53600$k474540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55153 = alloca %struct.ScmObj*, align 8
%argslist53600$k474541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47943, %struct.ScmObj* %argslist53600$k474540)
store volatile %struct.ScmObj* %argslist53600$k474541, %struct.ScmObj** %stackaddr$prim55153, align 8
%stackaddr$prim55154 = alloca %struct.ScmObj*, align 8
%argslist53600$k474542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47942, %struct.ScmObj* %argslist53600$k474541)
store volatile %struct.ScmObj* %argslist53600$k474542, %struct.ScmObj** %stackaddr$prim55154, align 8
%clofunc55155 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47454)
musttail call tailcc void %clofunc55155(%struct.ScmObj* %k47454, %struct.ScmObj* %argslist53600$k474542)
ret void
}

define tailcc void @proc_clo$ae47943(%struct.ScmObj* %env$ae47943,%struct.ScmObj* %current_45args53591) {
%stackaddr$env-ref55156 = alloca %struct.ScmObj*, align 8
%_37length47080 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47943, i64 0)
store %struct.ScmObj* %_37length47080, %struct.ScmObj** %stackaddr$env-ref55156
%stackaddr$prim55157 = alloca %struct.ScmObj*, align 8
%k47455 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53591)
store volatile %struct.ScmObj* %k47455, %struct.ScmObj** %stackaddr$prim55157, align 8
%stackaddr$prim55158 = alloca %struct.ScmObj*, align 8
%current_45args53592 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53591)
store volatile %struct.ScmObj* %current_45args53592, %struct.ScmObj** %stackaddr$prim55158, align 8
%stackaddr$prim55159 = alloca %struct.ScmObj*, align 8
%lst47081 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53592)
store volatile %struct.ScmObj* %lst47081, %struct.ScmObj** %stackaddr$prim55159, align 8
%stackaddr$prim55160 = alloca %struct.ScmObj*, align 8
%anf_45bind47215 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47081)
store volatile %struct.ScmObj* %anf_45bind47215, %struct.ScmObj** %stackaddr$prim55160, align 8
%truthy$cmp55161 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47215)
%cmp$cmp55161 = icmp eq i64 %truthy$cmp55161, 1
br i1 %cmp$cmp55161, label %truebranch$cmp55161, label %falsebranch$cmp55161
truebranch$cmp55161:
%ae47947 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47948 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53594$k474550 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55162 = alloca %struct.ScmObj*, align 8
%argslist53594$k474551 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47948, %struct.ScmObj* %argslist53594$k474550)
store volatile %struct.ScmObj* %argslist53594$k474551, %struct.ScmObj** %stackaddr$prim55162, align 8
%stackaddr$prim55163 = alloca %struct.ScmObj*, align 8
%argslist53594$k474552 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47947, %struct.ScmObj* %argslist53594$k474551)
store volatile %struct.ScmObj* %argslist53594$k474552, %struct.ScmObj** %stackaddr$prim55163, align 8
%clofunc55164 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47455)
musttail call tailcc void %clofunc55164(%struct.ScmObj* %k47455, %struct.ScmObj* %argslist53594$k474552)
ret void
falsebranch$cmp55161:
%stackaddr$prim55165 = alloca %struct.ScmObj*, align 8
%anf_45bind47216 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47081)
store volatile %struct.ScmObj* %anf_45bind47216, %struct.ScmObj** %stackaddr$prim55165, align 8
%stackaddr$makeclosure55166 = alloca %struct.ScmObj*, align 8
%fptrToInt55167 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47957 to i64
%ae47957 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55167)
store volatile %struct.ScmObj* %ae47957, %struct.ScmObj** %stackaddr$makeclosure55166, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47957, %struct.ScmObj* %k47455, i64 0)
%argslist53599$_37length470800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55168 = alloca %struct.ScmObj*, align 8
%argslist53599$_37length470801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47216, %struct.ScmObj* %argslist53599$_37length470800)
store volatile %struct.ScmObj* %argslist53599$_37length470801, %struct.ScmObj** %stackaddr$prim55168, align 8
%stackaddr$prim55169 = alloca %struct.ScmObj*, align 8
%argslist53599$_37length470802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47957, %struct.ScmObj* %argslist53599$_37length470801)
store volatile %struct.ScmObj* %argslist53599$_37length470802, %struct.ScmObj** %stackaddr$prim55169, align 8
%clofunc55170 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length47080)
musttail call tailcc void %clofunc55170(%struct.ScmObj* %_37length47080, %struct.ScmObj* %argslist53599$_37length470802)
ret void
}

define tailcc void @proc_clo$ae47957(%struct.ScmObj* %env$ae47957,%struct.ScmObj* %current_45args53595) {
%stackaddr$env-ref55171 = alloca %struct.ScmObj*, align 8
%k47455 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47957, i64 0)
store %struct.ScmObj* %k47455, %struct.ScmObj** %stackaddr$env-ref55171
%stackaddr$prim55172 = alloca %struct.ScmObj*, align 8
%_95k47456 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53595)
store volatile %struct.ScmObj* %_95k47456, %struct.ScmObj** %stackaddr$prim55172, align 8
%stackaddr$prim55173 = alloca %struct.ScmObj*, align 8
%current_45args53596 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53595)
store volatile %struct.ScmObj* %current_45args53596, %struct.ScmObj** %stackaddr$prim55173, align 8
%stackaddr$prim55174 = alloca %struct.ScmObj*, align 8
%anf_45bind47217 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53596)
store volatile %struct.ScmObj* %anf_45bind47217, %struct.ScmObj** %stackaddr$prim55174, align 8
%ae47959 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55175 = alloca %struct.ScmObj*, align 8
%cpsprim47457 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae47959, %struct.ScmObj* %anf_45bind47217)
store volatile %struct.ScmObj* %cpsprim47457, %struct.ScmObj** %stackaddr$prim55175, align 8
%ae47962 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53598$k474550 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55176 = alloca %struct.ScmObj*, align 8
%argslist53598$k474551 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47457, %struct.ScmObj* %argslist53598$k474550)
store volatile %struct.ScmObj* %argslist53598$k474551, %struct.ScmObj** %stackaddr$prim55176, align 8
%stackaddr$prim55177 = alloca %struct.ScmObj*, align 8
%argslist53598$k474552 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47962, %struct.ScmObj* %argslist53598$k474551)
store volatile %struct.ScmObj* %argslist53598$k474552, %struct.ScmObj** %stackaddr$prim55177, align 8
%clofunc55178 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47455)
musttail call tailcc void %clofunc55178(%struct.ScmObj* %k47455, %struct.ScmObj* %argslist53598$k474552)
ret void
}

define tailcc void @proc_clo$ae47790(%struct.ScmObj* %env$ae47790,%struct.ScmObj* %current_45args53603) {
%stackaddr$prim55179 = alloca %struct.ScmObj*, align 8
%k47458 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53603)
store volatile %struct.ScmObj* %k47458, %struct.ScmObj** %stackaddr$prim55179, align 8
%stackaddr$prim55180 = alloca %struct.ScmObj*, align 8
%current_45args53604 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53603)
store volatile %struct.ScmObj* %current_45args53604, %struct.ScmObj** %stackaddr$prim55180, align 8
%stackaddr$prim55181 = alloca %struct.ScmObj*, align 8
%_37take47083 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53604)
store volatile %struct.ScmObj* %_37take47083, %struct.ScmObj** %stackaddr$prim55181, align 8
%ae47792 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55182 = alloca %struct.ScmObj*, align 8
%fptrToInt55183 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47793 to i64
%ae47793 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55183)
store volatile %struct.ScmObj* %ae47793, %struct.ScmObj** %stackaddr$makeclosure55182, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47793, %struct.ScmObj* %_37take47083, i64 0)
%argslist53617$k474580 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55184 = alloca %struct.ScmObj*, align 8
%argslist53617$k474581 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47793, %struct.ScmObj* %argslist53617$k474580)
store volatile %struct.ScmObj* %argslist53617$k474581, %struct.ScmObj** %stackaddr$prim55184, align 8
%stackaddr$prim55185 = alloca %struct.ScmObj*, align 8
%argslist53617$k474582 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47792, %struct.ScmObj* %argslist53617$k474581)
store volatile %struct.ScmObj* %argslist53617$k474582, %struct.ScmObj** %stackaddr$prim55185, align 8
%clofunc55186 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47458)
musttail call tailcc void %clofunc55186(%struct.ScmObj* %k47458, %struct.ScmObj* %argslist53617$k474582)
ret void
}

define tailcc void @proc_clo$ae47793(%struct.ScmObj* %env$ae47793,%struct.ScmObj* %current_45args53606) {
%stackaddr$env-ref55187 = alloca %struct.ScmObj*, align 8
%_37take47083 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47793, i64 0)
store %struct.ScmObj* %_37take47083, %struct.ScmObj** %stackaddr$env-ref55187
%stackaddr$prim55188 = alloca %struct.ScmObj*, align 8
%k47459 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53606)
store volatile %struct.ScmObj* %k47459, %struct.ScmObj** %stackaddr$prim55188, align 8
%stackaddr$prim55189 = alloca %struct.ScmObj*, align 8
%current_45args53607 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53606)
store volatile %struct.ScmObj* %current_45args53607, %struct.ScmObj** %stackaddr$prim55189, align 8
%stackaddr$prim55190 = alloca %struct.ScmObj*, align 8
%lst47085 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53607)
store volatile %struct.ScmObj* %lst47085, %struct.ScmObj** %stackaddr$prim55190, align 8
%stackaddr$prim55191 = alloca %struct.ScmObj*, align 8
%current_45args53608 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53607)
store volatile %struct.ScmObj* %current_45args53608, %struct.ScmObj** %stackaddr$prim55191, align 8
%stackaddr$prim55192 = alloca %struct.ScmObj*, align 8
%n47084 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53608)
store volatile %struct.ScmObj* %n47084, %struct.ScmObj** %stackaddr$prim55192, align 8
%ae47795 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55193 = alloca %struct.ScmObj*, align 8
%anf_45bind47208 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n47084, %struct.ScmObj* %ae47795)
store volatile %struct.ScmObj* %anf_45bind47208, %struct.ScmObj** %stackaddr$prim55193, align 8
%truthy$cmp55194 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47208)
%cmp$cmp55194 = icmp eq i64 %truthy$cmp55194, 1
br i1 %cmp$cmp55194, label %truebranch$cmp55194, label %falsebranch$cmp55194
truebranch$cmp55194:
%ae47798 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47799 = call %struct.ScmObj* @const_init_null()
%argslist53610$k474590 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55195 = alloca %struct.ScmObj*, align 8
%argslist53610$k474591 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47799, %struct.ScmObj* %argslist53610$k474590)
store volatile %struct.ScmObj* %argslist53610$k474591, %struct.ScmObj** %stackaddr$prim55195, align 8
%stackaddr$prim55196 = alloca %struct.ScmObj*, align 8
%argslist53610$k474592 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47798, %struct.ScmObj* %argslist53610$k474591)
store volatile %struct.ScmObj* %argslist53610$k474592, %struct.ScmObj** %stackaddr$prim55196, align 8
%clofunc55197 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47459)
musttail call tailcc void %clofunc55197(%struct.ScmObj* %k47459, %struct.ScmObj* %argslist53610$k474592)
ret void
falsebranch$cmp55194:
%stackaddr$prim55198 = alloca %struct.ScmObj*, align 8
%anf_45bind47209 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47085)
store volatile %struct.ScmObj* %anf_45bind47209, %struct.ScmObj** %stackaddr$prim55198, align 8
%truthy$cmp55199 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47209)
%cmp$cmp55199 = icmp eq i64 %truthy$cmp55199, 1
br i1 %cmp$cmp55199, label %truebranch$cmp55199, label %falsebranch$cmp55199
truebranch$cmp55199:
%ae47809 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47810 = call %struct.ScmObj* @const_init_null()
%argslist53611$k474590 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55200 = alloca %struct.ScmObj*, align 8
%argslist53611$k474591 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47810, %struct.ScmObj* %argslist53611$k474590)
store volatile %struct.ScmObj* %argslist53611$k474591, %struct.ScmObj** %stackaddr$prim55200, align 8
%stackaddr$prim55201 = alloca %struct.ScmObj*, align 8
%argslist53611$k474592 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47809, %struct.ScmObj* %argslist53611$k474591)
store volatile %struct.ScmObj* %argslist53611$k474592, %struct.ScmObj** %stackaddr$prim55201, align 8
%clofunc55202 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47459)
musttail call tailcc void %clofunc55202(%struct.ScmObj* %k47459, %struct.ScmObj* %argslist53611$k474592)
ret void
falsebranch$cmp55199:
%stackaddr$prim55203 = alloca %struct.ScmObj*, align 8
%anf_45bind47210 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47085)
store volatile %struct.ScmObj* %anf_45bind47210, %struct.ScmObj** %stackaddr$prim55203, align 8
%stackaddr$prim55204 = alloca %struct.ScmObj*, align 8
%anf_45bind47211 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47085)
store volatile %struct.ScmObj* %anf_45bind47211, %struct.ScmObj** %stackaddr$prim55204, align 8
%ae47820 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55205 = alloca %struct.ScmObj*, align 8
%anf_45bind47212 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n47084, %struct.ScmObj* %ae47820)
store volatile %struct.ScmObj* %anf_45bind47212, %struct.ScmObj** %stackaddr$prim55205, align 8
%stackaddr$makeclosure55206 = alloca %struct.ScmObj*, align 8
%fptrToInt55207 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47822 to i64
%ae47822 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55207)
store volatile %struct.ScmObj* %ae47822, %struct.ScmObj** %stackaddr$makeclosure55206, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47822, %struct.ScmObj* %k47459, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47822, %struct.ScmObj* %anf_45bind47210, i64 1)
%argslist53616$_37take470830 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55208 = alloca %struct.ScmObj*, align 8
%argslist53616$_37take470831 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47212, %struct.ScmObj* %argslist53616$_37take470830)
store volatile %struct.ScmObj* %argslist53616$_37take470831, %struct.ScmObj** %stackaddr$prim55208, align 8
%stackaddr$prim55209 = alloca %struct.ScmObj*, align 8
%argslist53616$_37take470832 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47211, %struct.ScmObj* %argslist53616$_37take470831)
store volatile %struct.ScmObj* %argslist53616$_37take470832, %struct.ScmObj** %stackaddr$prim55209, align 8
%stackaddr$prim55210 = alloca %struct.ScmObj*, align 8
%argslist53616$_37take470833 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47822, %struct.ScmObj* %argslist53616$_37take470832)
store volatile %struct.ScmObj* %argslist53616$_37take470833, %struct.ScmObj** %stackaddr$prim55210, align 8
%clofunc55211 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take47083)
musttail call tailcc void %clofunc55211(%struct.ScmObj* %_37take47083, %struct.ScmObj* %argslist53616$_37take470833)
ret void
}

define tailcc void @proc_clo$ae47822(%struct.ScmObj* %env$ae47822,%struct.ScmObj* %current_45args53612) {
%stackaddr$env-ref55212 = alloca %struct.ScmObj*, align 8
%k47459 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47822, i64 0)
store %struct.ScmObj* %k47459, %struct.ScmObj** %stackaddr$env-ref55212
%stackaddr$env-ref55213 = alloca %struct.ScmObj*, align 8
%anf_45bind47210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47822, i64 1)
store %struct.ScmObj* %anf_45bind47210, %struct.ScmObj** %stackaddr$env-ref55213
%stackaddr$prim55214 = alloca %struct.ScmObj*, align 8
%_95k47460 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53612)
store volatile %struct.ScmObj* %_95k47460, %struct.ScmObj** %stackaddr$prim55214, align 8
%stackaddr$prim55215 = alloca %struct.ScmObj*, align 8
%current_45args53613 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53612)
store volatile %struct.ScmObj* %current_45args53613, %struct.ScmObj** %stackaddr$prim55215, align 8
%stackaddr$prim55216 = alloca %struct.ScmObj*, align 8
%anf_45bind47213 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53613)
store volatile %struct.ScmObj* %anf_45bind47213, %struct.ScmObj** %stackaddr$prim55216, align 8
%stackaddr$prim55217 = alloca %struct.ScmObj*, align 8
%cpsprim47461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47210, %struct.ScmObj* %anf_45bind47213)
store volatile %struct.ScmObj* %cpsprim47461, %struct.ScmObj** %stackaddr$prim55217, align 8
%ae47828 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53615$k474590 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55218 = alloca %struct.ScmObj*, align 8
%argslist53615$k474591 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47461, %struct.ScmObj* %argslist53615$k474590)
store volatile %struct.ScmObj* %argslist53615$k474591, %struct.ScmObj** %stackaddr$prim55218, align 8
%stackaddr$prim55219 = alloca %struct.ScmObj*, align 8
%argslist53615$k474592 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47828, %struct.ScmObj* %argslist53615$k474591)
store volatile %struct.ScmObj* %argslist53615$k474592, %struct.ScmObj** %stackaddr$prim55219, align 8
%clofunc55220 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47459)
musttail call tailcc void %clofunc55220(%struct.ScmObj* %k47459, %struct.ScmObj* %argslist53615$k474592)
ret void
}

define tailcc void @proc_clo$ae47693(%struct.ScmObj* %env$ae47693,%struct.ScmObj* %current_45args53620) {
%stackaddr$prim55221 = alloca %struct.ScmObj*, align 8
%k47462 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53620)
store volatile %struct.ScmObj* %k47462, %struct.ScmObj** %stackaddr$prim55221, align 8
%stackaddr$prim55222 = alloca %struct.ScmObj*, align 8
%current_45args53621 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53620)
store volatile %struct.ScmObj* %current_45args53621, %struct.ScmObj** %stackaddr$prim55222, align 8
%stackaddr$prim55223 = alloca %struct.ScmObj*, align 8
%_37map47087 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53621)
store volatile %struct.ScmObj* %_37map47087, %struct.ScmObj** %stackaddr$prim55223, align 8
%ae47695 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55224 = alloca %struct.ScmObj*, align 8
%fptrToInt55225 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47696 to i64
%ae47696 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55225)
store volatile %struct.ScmObj* %ae47696, %struct.ScmObj** %stackaddr$makeclosure55224, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47696, %struct.ScmObj* %_37map47087, i64 0)
%argslist53637$k474620 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55226 = alloca %struct.ScmObj*, align 8
%argslist53637$k474621 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47696, %struct.ScmObj* %argslist53637$k474620)
store volatile %struct.ScmObj* %argslist53637$k474621, %struct.ScmObj** %stackaddr$prim55226, align 8
%stackaddr$prim55227 = alloca %struct.ScmObj*, align 8
%argslist53637$k474622 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47695, %struct.ScmObj* %argslist53637$k474621)
store volatile %struct.ScmObj* %argslist53637$k474622, %struct.ScmObj** %stackaddr$prim55227, align 8
%clofunc55228 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47462)
musttail call tailcc void %clofunc55228(%struct.ScmObj* %k47462, %struct.ScmObj* %argslist53637$k474622)
ret void
}

define tailcc void @proc_clo$ae47696(%struct.ScmObj* %env$ae47696,%struct.ScmObj* %current_45args53623) {
%stackaddr$env-ref55229 = alloca %struct.ScmObj*, align 8
%_37map47087 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47696, i64 0)
store %struct.ScmObj* %_37map47087, %struct.ScmObj** %stackaddr$env-ref55229
%stackaddr$prim55230 = alloca %struct.ScmObj*, align 8
%k47463 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53623)
store volatile %struct.ScmObj* %k47463, %struct.ScmObj** %stackaddr$prim55230, align 8
%stackaddr$prim55231 = alloca %struct.ScmObj*, align 8
%current_45args53624 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53623)
store volatile %struct.ScmObj* %current_45args53624, %struct.ScmObj** %stackaddr$prim55231, align 8
%stackaddr$prim55232 = alloca %struct.ScmObj*, align 8
%f47089 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53624)
store volatile %struct.ScmObj* %f47089, %struct.ScmObj** %stackaddr$prim55232, align 8
%stackaddr$prim55233 = alloca %struct.ScmObj*, align 8
%current_45args53625 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53624)
store volatile %struct.ScmObj* %current_45args53625, %struct.ScmObj** %stackaddr$prim55233, align 8
%stackaddr$prim55234 = alloca %struct.ScmObj*, align 8
%lst47088 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53625)
store volatile %struct.ScmObj* %lst47088, %struct.ScmObj** %stackaddr$prim55234, align 8
%stackaddr$prim55235 = alloca %struct.ScmObj*, align 8
%anf_45bind47202 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47088)
store volatile %struct.ScmObj* %anf_45bind47202, %struct.ScmObj** %stackaddr$prim55235, align 8
%truthy$cmp55236 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47202)
%cmp$cmp55236 = icmp eq i64 %truthy$cmp55236, 1
br i1 %cmp$cmp55236, label %truebranch$cmp55236, label %falsebranch$cmp55236
truebranch$cmp55236:
%ae47700 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47701 = call %struct.ScmObj* @const_init_null()
%argslist53627$k474630 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55237 = alloca %struct.ScmObj*, align 8
%argslist53627$k474631 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47701, %struct.ScmObj* %argslist53627$k474630)
store volatile %struct.ScmObj* %argslist53627$k474631, %struct.ScmObj** %stackaddr$prim55237, align 8
%stackaddr$prim55238 = alloca %struct.ScmObj*, align 8
%argslist53627$k474632 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47700, %struct.ScmObj* %argslist53627$k474631)
store volatile %struct.ScmObj* %argslist53627$k474632, %struct.ScmObj** %stackaddr$prim55238, align 8
%clofunc55239 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47463)
musttail call tailcc void %clofunc55239(%struct.ScmObj* %k47463, %struct.ScmObj* %argslist53627$k474632)
ret void
falsebranch$cmp55236:
%stackaddr$prim55240 = alloca %struct.ScmObj*, align 8
%anf_45bind47203 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47088)
store volatile %struct.ScmObj* %anf_45bind47203, %struct.ScmObj** %stackaddr$prim55240, align 8
%stackaddr$makeclosure55241 = alloca %struct.ScmObj*, align 8
%fptrToInt55242 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47710 to i64
%ae47710 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55242)
store volatile %struct.ScmObj* %ae47710, %struct.ScmObj** %stackaddr$makeclosure55241, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47710, %struct.ScmObj* %f47089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47710, %struct.ScmObj* %lst47088, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47710, %struct.ScmObj* %_37map47087, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae47710, %struct.ScmObj* %k47463, i64 3)
%argslist53636$f470890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55243 = alloca %struct.ScmObj*, align 8
%argslist53636$f470891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47203, %struct.ScmObj* %argslist53636$f470890)
store volatile %struct.ScmObj* %argslist53636$f470891, %struct.ScmObj** %stackaddr$prim55243, align 8
%stackaddr$prim55244 = alloca %struct.ScmObj*, align 8
%argslist53636$f470892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47710, %struct.ScmObj* %argslist53636$f470891)
store volatile %struct.ScmObj* %argslist53636$f470892, %struct.ScmObj** %stackaddr$prim55244, align 8
%clofunc55245 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47089)
musttail call tailcc void %clofunc55245(%struct.ScmObj* %f47089, %struct.ScmObj* %argslist53636$f470892)
ret void
}

define tailcc void @proc_clo$ae47710(%struct.ScmObj* %env$ae47710,%struct.ScmObj* %current_45args53628) {
%stackaddr$env-ref55246 = alloca %struct.ScmObj*, align 8
%f47089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47710, i64 0)
store %struct.ScmObj* %f47089, %struct.ScmObj** %stackaddr$env-ref55246
%stackaddr$env-ref55247 = alloca %struct.ScmObj*, align 8
%lst47088 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47710, i64 1)
store %struct.ScmObj* %lst47088, %struct.ScmObj** %stackaddr$env-ref55247
%stackaddr$env-ref55248 = alloca %struct.ScmObj*, align 8
%_37map47087 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47710, i64 2)
store %struct.ScmObj* %_37map47087, %struct.ScmObj** %stackaddr$env-ref55248
%stackaddr$env-ref55249 = alloca %struct.ScmObj*, align 8
%k47463 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47710, i64 3)
store %struct.ScmObj* %k47463, %struct.ScmObj** %stackaddr$env-ref55249
%stackaddr$prim55250 = alloca %struct.ScmObj*, align 8
%_95k47464 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53628)
store volatile %struct.ScmObj* %_95k47464, %struct.ScmObj** %stackaddr$prim55250, align 8
%stackaddr$prim55251 = alloca %struct.ScmObj*, align 8
%current_45args53629 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53628)
store volatile %struct.ScmObj* %current_45args53629, %struct.ScmObj** %stackaddr$prim55251, align 8
%stackaddr$prim55252 = alloca %struct.ScmObj*, align 8
%anf_45bind47204 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53629)
store volatile %struct.ScmObj* %anf_45bind47204, %struct.ScmObj** %stackaddr$prim55252, align 8
%stackaddr$prim55253 = alloca %struct.ScmObj*, align 8
%anf_45bind47205 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47088)
store volatile %struct.ScmObj* %anf_45bind47205, %struct.ScmObj** %stackaddr$prim55253, align 8
%stackaddr$makeclosure55254 = alloca %struct.ScmObj*, align 8
%fptrToInt55255 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47714 to i64
%ae47714 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55255)
store volatile %struct.ScmObj* %ae47714, %struct.ScmObj** %stackaddr$makeclosure55254, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47714, %struct.ScmObj* %anf_45bind47204, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47714, %struct.ScmObj* %k47463, i64 1)
%argslist53635$_37map470870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55256 = alloca %struct.ScmObj*, align 8
%argslist53635$_37map470871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47205, %struct.ScmObj* %argslist53635$_37map470870)
store volatile %struct.ScmObj* %argslist53635$_37map470871, %struct.ScmObj** %stackaddr$prim55256, align 8
%stackaddr$prim55257 = alloca %struct.ScmObj*, align 8
%argslist53635$_37map470872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47089, %struct.ScmObj* %argslist53635$_37map470871)
store volatile %struct.ScmObj* %argslist53635$_37map470872, %struct.ScmObj** %stackaddr$prim55257, align 8
%stackaddr$prim55258 = alloca %struct.ScmObj*, align 8
%argslist53635$_37map470873 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47714, %struct.ScmObj* %argslist53635$_37map470872)
store volatile %struct.ScmObj* %argslist53635$_37map470873, %struct.ScmObj** %stackaddr$prim55258, align 8
%clofunc55259 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map47087)
musttail call tailcc void %clofunc55259(%struct.ScmObj* %_37map47087, %struct.ScmObj* %argslist53635$_37map470873)
ret void
}

define tailcc void @proc_clo$ae47714(%struct.ScmObj* %env$ae47714,%struct.ScmObj* %current_45args53631) {
%stackaddr$env-ref55260 = alloca %struct.ScmObj*, align 8
%anf_45bind47204 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47714, i64 0)
store %struct.ScmObj* %anf_45bind47204, %struct.ScmObj** %stackaddr$env-ref55260
%stackaddr$env-ref55261 = alloca %struct.ScmObj*, align 8
%k47463 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47714, i64 1)
store %struct.ScmObj* %k47463, %struct.ScmObj** %stackaddr$env-ref55261
%stackaddr$prim55262 = alloca %struct.ScmObj*, align 8
%_95k47465 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53631)
store volatile %struct.ScmObj* %_95k47465, %struct.ScmObj** %stackaddr$prim55262, align 8
%stackaddr$prim55263 = alloca %struct.ScmObj*, align 8
%current_45args53632 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53631)
store volatile %struct.ScmObj* %current_45args53632, %struct.ScmObj** %stackaddr$prim55263, align 8
%stackaddr$prim55264 = alloca %struct.ScmObj*, align 8
%anf_45bind47206 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53632)
store volatile %struct.ScmObj* %anf_45bind47206, %struct.ScmObj** %stackaddr$prim55264, align 8
%stackaddr$prim55265 = alloca %struct.ScmObj*, align 8
%cpsprim47466 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47204, %struct.ScmObj* %anf_45bind47206)
store volatile %struct.ScmObj* %cpsprim47466, %struct.ScmObj** %stackaddr$prim55265, align 8
%ae47720 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53634$k474630 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55266 = alloca %struct.ScmObj*, align 8
%argslist53634$k474631 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47466, %struct.ScmObj* %argslist53634$k474630)
store volatile %struct.ScmObj* %argslist53634$k474631, %struct.ScmObj** %stackaddr$prim55266, align 8
%stackaddr$prim55267 = alloca %struct.ScmObj*, align 8
%argslist53634$k474632 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47720, %struct.ScmObj* %argslist53634$k474631)
store volatile %struct.ScmObj* %argslist53634$k474632, %struct.ScmObj** %stackaddr$prim55267, align 8
%clofunc55268 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47463)
musttail call tailcc void %clofunc55268(%struct.ScmObj* %k47463, %struct.ScmObj* %argslist53634$k474632)
ret void
}

define tailcc void @proc_clo$ae47613(%struct.ScmObj* %env$ae47613,%struct.ScmObj* %current_45args53640) {
%stackaddr$prim55269 = alloca %struct.ScmObj*, align 8
%k47467 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53640)
store volatile %struct.ScmObj* %k47467, %struct.ScmObj** %stackaddr$prim55269, align 8
%stackaddr$prim55270 = alloca %struct.ScmObj*, align 8
%current_45args53641 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53640)
store volatile %struct.ScmObj* %current_45args53641, %struct.ScmObj** %stackaddr$prim55270, align 8
%stackaddr$prim55271 = alloca %struct.ScmObj*, align 8
%_37foldr147091 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53641)
store volatile %struct.ScmObj* %_37foldr147091, %struct.ScmObj** %stackaddr$prim55271, align 8
%ae47615 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55272 = alloca %struct.ScmObj*, align 8
%fptrToInt55273 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47616 to i64
%ae47616 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55273)
store volatile %struct.ScmObj* %ae47616, %struct.ScmObj** %stackaddr$makeclosure55272, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47616, %struct.ScmObj* %_37foldr147091, i64 0)
%argslist53654$k474670 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55274 = alloca %struct.ScmObj*, align 8
%argslist53654$k474671 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47616, %struct.ScmObj* %argslist53654$k474670)
store volatile %struct.ScmObj* %argslist53654$k474671, %struct.ScmObj** %stackaddr$prim55274, align 8
%stackaddr$prim55275 = alloca %struct.ScmObj*, align 8
%argslist53654$k474672 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47615, %struct.ScmObj* %argslist53654$k474671)
store volatile %struct.ScmObj* %argslist53654$k474672, %struct.ScmObj** %stackaddr$prim55275, align 8
%clofunc55276 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47467)
musttail call tailcc void %clofunc55276(%struct.ScmObj* %k47467, %struct.ScmObj* %argslist53654$k474672)
ret void
}

define tailcc void @proc_clo$ae47616(%struct.ScmObj* %env$ae47616,%struct.ScmObj* %current_45args53643) {
%stackaddr$env-ref55277 = alloca %struct.ScmObj*, align 8
%_37foldr147091 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47616, i64 0)
store %struct.ScmObj* %_37foldr147091, %struct.ScmObj** %stackaddr$env-ref55277
%stackaddr$prim55278 = alloca %struct.ScmObj*, align 8
%k47468 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53643)
store volatile %struct.ScmObj* %k47468, %struct.ScmObj** %stackaddr$prim55278, align 8
%stackaddr$prim55279 = alloca %struct.ScmObj*, align 8
%current_45args53644 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53643)
store volatile %struct.ScmObj* %current_45args53644, %struct.ScmObj** %stackaddr$prim55279, align 8
%stackaddr$prim55280 = alloca %struct.ScmObj*, align 8
%f47094 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53644)
store volatile %struct.ScmObj* %f47094, %struct.ScmObj** %stackaddr$prim55280, align 8
%stackaddr$prim55281 = alloca %struct.ScmObj*, align 8
%current_45args53645 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53644)
store volatile %struct.ScmObj* %current_45args53645, %struct.ScmObj** %stackaddr$prim55281, align 8
%stackaddr$prim55282 = alloca %struct.ScmObj*, align 8
%acc47093 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53645)
store volatile %struct.ScmObj* %acc47093, %struct.ScmObj** %stackaddr$prim55282, align 8
%stackaddr$prim55283 = alloca %struct.ScmObj*, align 8
%current_45args53646 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53645)
store volatile %struct.ScmObj* %current_45args53646, %struct.ScmObj** %stackaddr$prim55283, align 8
%stackaddr$prim55284 = alloca %struct.ScmObj*, align 8
%lst47092 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53646)
store volatile %struct.ScmObj* %lst47092, %struct.ScmObj** %stackaddr$prim55284, align 8
%stackaddr$prim55285 = alloca %struct.ScmObj*, align 8
%anf_45bind47197 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47092)
store volatile %struct.ScmObj* %anf_45bind47197, %struct.ScmObj** %stackaddr$prim55285, align 8
%truthy$cmp55286 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47197)
%cmp$cmp55286 = icmp eq i64 %truthy$cmp55286, 1
br i1 %cmp$cmp55286, label %truebranch$cmp55286, label %falsebranch$cmp55286
truebranch$cmp55286:
%ae47620 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53648$k474680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55287 = alloca %struct.ScmObj*, align 8
%argslist53648$k474681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47093, %struct.ScmObj* %argslist53648$k474680)
store volatile %struct.ScmObj* %argslist53648$k474681, %struct.ScmObj** %stackaddr$prim55287, align 8
%stackaddr$prim55288 = alloca %struct.ScmObj*, align 8
%argslist53648$k474682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47620, %struct.ScmObj* %argslist53648$k474681)
store volatile %struct.ScmObj* %argslist53648$k474682, %struct.ScmObj** %stackaddr$prim55288, align 8
%clofunc55289 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47468)
musttail call tailcc void %clofunc55289(%struct.ScmObj* %k47468, %struct.ScmObj* %argslist53648$k474682)
ret void
falsebranch$cmp55286:
%stackaddr$prim55290 = alloca %struct.ScmObj*, align 8
%anf_45bind47198 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47092)
store volatile %struct.ScmObj* %anf_45bind47198, %struct.ScmObj** %stackaddr$prim55290, align 8
%stackaddr$prim55291 = alloca %struct.ScmObj*, align 8
%anf_45bind47199 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47092)
store volatile %struct.ScmObj* %anf_45bind47199, %struct.ScmObj** %stackaddr$prim55291, align 8
%stackaddr$makeclosure55292 = alloca %struct.ScmObj*, align 8
%fptrToInt55293 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47628 to i64
%ae47628 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55293)
store volatile %struct.ScmObj* %ae47628, %struct.ScmObj** %stackaddr$makeclosure55292, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47628, %struct.ScmObj* %f47094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47628, %struct.ScmObj* %anf_45bind47198, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47628, %struct.ScmObj* %k47468, i64 2)
%argslist53653$_37foldr1470910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55294 = alloca %struct.ScmObj*, align 8
%argslist53653$_37foldr1470911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47199, %struct.ScmObj* %argslist53653$_37foldr1470910)
store volatile %struct.ScmObj* %argslist53653$_37foldr1470911, %struct.ScmObj** %stackaddr$prim55294, align 8
%stackaddr$prim55295 = alloca %struct.ScmObj*, align 8
%argslist53653$_37foldr1470912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47093, %struct.ScmObj* %argslist53653$_37foldr1470911)
store volatile %struct.ScmObj* %argslist53653$_37foldr1470912, %struct.ScmObj** %stackaddr$prim55295, align 8
%stackaddr$prim55296 = alloca %struct.ScmObj*, align 8
%argslist53653$_37foldr1470913 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47094, %struct.ScmObj* %argslist53653$_37foldr1470912)
store volatile %struct.ScmObj* %argslist53653$_37foldr1470913, %struct.ScmObj** %stackaddr$prim55296, align 8
%stackaddr$prim55297 = alloca %struct.ScmObj*, align 8
%argslist53653$_37foldr1470914 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47628, %struct.ScmObj* %argslist53653$_37foldr1470913)
store volatile %struct.ScmObj* %argslist53653$_37foldr1470914, %struct.ScmObj** %stackaddr$prim55297, align 8
%clofunc55298 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147091)
musttail call tailcc void %clofunc55298(%struct.ScmObj* %_37foldr147091, %struct.ScmObj* %argslist53653$_37foldr1470914)
ret void
}

define tailcc void @proc_clo$ae47628(%struct.ScmObj* %env$ae47628,%struct.ScmObj* %current_45args53649) {
%stackaddr$env-ref55299 = alloca %struct.ScmObj*, align 8
%f47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47628, i64 0)
store %struct.ScmObj* %f47094, %struct.ScmObj** %stackaddr$env-ref55299
%stackaddr$env-ref55300 = alloca %struct.ScmObj*, align 8
%anf_45bind47198 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47628, i64 1)
store %struct.ScmObj* %anf_45bind47198, %struct.ScmObj** %stackaddr$env-ref55300
%stackaddr$env-ref55301 = alloca %struct.ScmObj*, align 8
%k47468 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47628, i64 2)
store %struct.ScmObj* %k47468, %struct.ScmObj** %stackaddr$env-ref55301
%stackaddr$prim55302 = alloca %struct.ScmObj*, align 8
%_95k47469 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53649)
store volatile %struct.ScmObj* %_95k47469, %struct.ScmObj** %stackaddr$prim55302, align 8
%stackaddr$prim55303 = alloca %struct.ScmObj*, align 8
%current_45args53650 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53649)
store volatile %struct.ScmObj* %current_45args53650, %struct.ScmObj** %stackaddr$prim55303, align 8
%stackaddr$prim55304 = alloca %struct.ScmObj*, align 8
%anf_45bind47200 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53650)
store volatile %struct.ScmObj* %anf_45bind47200, %struct.ScmObj** %stackaddr$prim55304, align 8
%argslist53652$f470940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55305 = alloca %struct.ScmObj*, align 8
%argslist53652$f470941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47200, %struct.ScmObj* %argslist53652$f470940)
store volatile %struct.ScmObj* %argslist53652$f470941, %struct.ScmObj** %stackaddr$prim55305, align 8
%stackaddr$prim55306 = alloca %struct.ScmObj*, align 8
%argslist53652$f470942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47198, %struct.ScmObj* %argslist53652$f470941)
store volatile %struct.ScmObj* %argslist53652$f470942, %struct.ScmObj** %stackaddr$prim55306, align 8
%stackaddr$prim55307 = alloca %struct.ScmObj*, align 8
%argslist53652$f470943 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47468, %struct.ScmObj* %argslist53652$f470942)
store volatile %struct.ScmObj* %argslist53652$f470943, %struct.ScmObj** %stackaddr$prim55307, align 8
%clofunc55308 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47094)
musttail call tailcc void %clofunc55308(%struct.ScmObj* %f47094, %struct.ScmObj* %argslist53652$f470943)
ret void
}

define tailcc void @proc_clo$ae47496(%struct.ScmObj* %env$ae47496,%struct.ScmObj* %current_45args53657) {
%stackaddr$prim55309 = alloca %struct.ScmObj*, align 8
%k47470 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53657)
store volatile %struct.ScmObj* %k47470, %struct.ScmObj** %stackaddr$prim55309, align 8
%stackaddr$prim55310 = alloca %struct.ScmObj*, align 8
%current_45args53658 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53657)
store volatile %struct.ScmObj* %current_45args53658, %struct.ScmObj** %stackaddr$prim55310, align 8
%stackaddr$prim55311 = alloca %struct.ScmObj*, align 8
%y47071 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53658)
store volatile %struct.ScmObj* %y47071, %struct.ScmObj** %stackaddr$prim55311, align 8
%ae47498 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55312 = alloca %struct.ScmObj*, align 8
%fptrToInt55313 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47499 to i64
%ae47499 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55313)
store volatile %struct.ScmObj* %ae47499, %struct.ScmObj** %stackaddr$makeclosure55312, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47499, %struct.ScmObj* %y47071, i64 0)
%argslist53676$k474700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55314 = alloca %struct.ScmObj*, align 8
%argslist53676$k474701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47499, %struct.ScmObj* %argslist53676$k474700)
store volatile %struct.ScmObj* %argslist53676$k474701, %struct.ScmObj** %stackaddr$prim55314, align 8
%stackaddr$prim55315 = alloca %struct.ScmObj*, align 8
%argslist53676$k474702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47498, %struct.ScmObj* %argslist53676$k474701)
store volatile %struct.ScmObj* %argslist53676$k474702, %struct.ScmObj** %stackaddr$prim55315, align 8
%clofunc55316 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47470)
musttail call tailcc void %clofunc55316(%struct.ScmObj* %k47470, %struct.ScmObj* %argslist53676$k474702)
ret void
}

define tailcc void @proc_clo$ae47499(%struct.ScmObj* %env$ae47499,%struct.ScmObj* %current_45args53660) {
%stackaddr$env-ref55317 = alloca %struct.ScmObj*, align 8
%y47071 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47499, i64 0)
store %struct.ScmObj* %y47071, %struct.ScmObj** %stackaddr$env-ref55317
%stackaddr$prim55318 = alloca %struct.ScmObj*, align 8
%k47471 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53660)
store volatile %struct.ScmObj* %k47471, %struct.ScmObj** %stackaddr$prim55318, align 8
%stackaddr$prim55319 = alloca %struct.ScmObj*, align 8
%current_45args53661 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53660)
store volatile %struct.ScmObj* %current_45args53661, %struct.ScmObj** %stackaddr$prim55319, align 8
%stackaddr$prim55320 = alloca %struct.ScmObj*, align 8
%f47072 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53661)
store volatile %struct.ScmObj* %f47072, %struct.ScmObj** %stackaddr$prim55320, align 8
%stackaddr$makeclosure55321 = alloca %struct.ScmObj*, align 8
%fptrToInt55322 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47500 to i64
%ae47500 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55322)
store volatile %struct.ScmObj* %ae47500, %struct.ScmObj** %stackaddr$makeclosure55321, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47500, %struct.ScmObj* %f47072, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47500, %struct.ScmObj* %k47471, i64 1)
%ae47501 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55323 = alloca %struct.ScmObj*, align 8
%fptrToInt55324 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47502 to i64
%ae47502 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55324)
store volatile %struct.ScmObj* %ae47502, %struct.ScmObj** %stackaddr$makeclosure55323, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47502, %struct.ScmObj* %f47072, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47502, %struct.ScmObj* %y47071, i64 1)
%argslist53675$ae475000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55325 = alloca %struct.ScmObj*, align 8
%argslist53675$ae475001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47502, %struct.ScmObj* %argslist53675$ae475000)
store volatile %struct.ScmObj* %argslist53675$ae475001, %struct.ScmObj** %stackaddr$prim55325, align 8
%stackaddr$prim55326 = alloca %struct.ScmObj*, align 8
%argslist53675$ae475002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47501, %struct.ScmObj* %argslist53675$ae475001)
store volatile %struct.ScmObj* %argslist53675$ae475002, %struct.ScmObj** %stackaddr$prim55326, align 8
%clofunc55327 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47500)
musttail call tailcc void %clofunc55327(%struct.ScmObj* %ae47500, %struct.ScmObj* %argslist53675$ae475002)
ret void
}

define tailcc void @proc_clo$ae47500(%struct.ScmObj* %env$ae47500,%struct.ScmObj* %current_45args53663) {
%stackaddr$env-ref55328 = alloca %struct.ScmObj*, align 8
%f47072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47500, i64 0)
store %struct.ScmObj* %f47072, %struct.ScmObj** %stackaddr$env-ref55328
%stackaddr$env-ref55329 = alloca %struct.ScmObj*, align 8
%k47471 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47500, i64 1)
store %struct.ScmObj* %k47471, %struct.ScmObj** %stackaddr$env-ref55329
%stackaddr$prim55330 = alloca %struct.ScmObj*, align 8
%_95k47472 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53663)
store volatile %struct.ScmObj* %_95k47472, %struct.ScmObj** %stackaddr$prim55330, align 8
%stackaddr$prim55331 = alloca %struct.ScmObj*, align 8
%current_45args53664 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53663)
store volatile %struct.ScmObj* %current_45args53664, %struct.ScmObj** %stackaddr$prim55331, align 8
%stackaddr$prim55332 = alloca %struct.ScmObj*, align 8
%anf_45bind47195 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53664)
store volatile %struct.ScmObj* %anf_45bind47195, %struct.ScmObj** %stackaddr$prim55332, align 8
%argslist53666$f470720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55333 = alloca %struct.ScmObj*, align 8
%argslist53666$f470721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47195, %struct.ScmObj* %argslist53666$f470720)
store volatile %struct.ScmObj* %argslist53666$f470721, %struct.ScmObj** %stackaddr$prim55333, align 8
%stackaddr$prim55334 = alloca %struct.ScmObj*, align 8
%argslist53666$f470722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47471, %struct.ScmObj* %argslist53666$f470721)
store volatile %struct.ScmObj* %argslist53666$f470722, %struct.ScmObj** %stackaddr$prim55334, align 8
%clofunc55335 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47072)
musttail call tailcc void %clofunc55335(%struct.ScmObj* %f47072, %struct.ScmObj* %argslist53666$f470722)
ret void
}

define tailcc void @proc_clo$ae47502(%struct.ScmObj* %env$ae47502,%struct.ScmObj* %args4707347473) {
%stackaddr$env-ref55336 = alloca %struct.ScmObj*, align 8
%f47072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47502, i64 0)
store %struct.ScmObj* %f47072, %struct.ScmObj** %stackaddr$env-ref55336
%stackaddr$env-ref55337 = alloca %struct.ScmObj*, align 8
%y47071 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47502, i64 1)
store %struct.ScmObj* %y47071, %struct.ScmObj** %stackaddr$env-ref55337
%stackaddr$prim55338 = alloca %struct.ScmObj*, align 8
%k47474 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4707347473)
store volatile %struct.ScmObj* %k47474, %struct.ScmObj** %stackaddr$prim55338, align 8
%stackaddr$prim55339 = alloca %struct.ScmObj*, align 8
%args47073 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4707347473)
store volatile %struct.ScmObj* %args47073, %struct.ScmObj** %stackaddr$prim55339, align 8
%stackaddr$makeclosure55340 = alloca %struct.ScmObj*, align 8
%fptrToInt55341 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47506 to i64
%ae47506 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55341)
store volatile %struct.ScmObj* %ae47506, %struct.ScmObj** %stackaddr$makeclosure55340, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47506, %struct.ScmObj* %k47474, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47506, %struct.ScmObj* %args47073, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47506, %struct.ScmObj* %f47072, i64 2)
%argslist53674$y470710 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55342 = alloca %struct.ScmObj*, align 8
%argslist53674$y470711 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y47071, %struct.ScmObj* %argslist53674$y470710)
store volatile %struct.ScmObj* %argslist53674$y470711, %struct.ScmObj** %stackaddr$prim55342, align 8
%stackaddr$prim55343 = alloca %struct.ScmObj*, align 8
%argslist53674$y470712 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47506, %struct.ScmObj* %argslist53674$y470711)
store volatile %struct.ScmObj* %argslist53674$y470712, %struct.ScmObj** %stackaddr$prim55343, align 8
%clofunc55344 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y47071)
musttail call tailcc void %clofunc55344(%struct.ScmObj* %y47071, %struct.ScmObj* %argslist53674$y470712)
ret void
}

define tailcc void @proc_clo$ae47506(%struct.ScmObj* %env$ae47506,%struct.ScmObj* %current_45args53667) {
%stackaddr$env-ref55345 = alloca %struct.ScmObj*, align 8
%k47474 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47506, i64 0)
store %struct.ScmObj* %k47474, %struct.ScmObj** %stackaddr$env-ref55345
%stackaddr$env-ref55346 = alloca %struct.ScmObj*, align 8
%args47073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47506, i64 1)
store %struct.ScmObj* %args47073, %struct.ScmObj** %stackaddr$env-ref55346
%stackaddr$env-ref55347 = alloca %struct.ScmObj*, align 8
%f47072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47506, i64 2)
store %struct.ScmObj* %f47072, %struct.ScmObj** %stackaddr$env-ref55347
%stackaddr$prim55348 = alloca %struct.ScmObj*, align 8
%_95k47475 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53667)
store volatile %struct.ScmObj* %_95k47475, %struct.ScmObj** %stackaddr$prim55348, align 8
%stackaddr$prim55349 = alloca %struct.ScmObj*, align 8
%current_45args53668 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53667)
store volatile %struct.ScmObj* %current_45args53668, %struct.ScmObj** %stackaddr$prim55349, align 8
%stackaddr$prim55350 = alloca %struct.ScmObj*, align 8
%anf_45bind47193 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53668)
store volatile %struct.ScmObj* %anf_45bind47193, %struct.ScmObj** %stackaddr$prim55350, align 8
%stackaddr$makeclosure55351 = alloca %struct.ScmObj*, align 8
%fptrToInt55352 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47509 to i64
%ae47509 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55352)
store volatile %struct.ScmObj* %ae47509, %struct.ScmObj** %stackaddr$makeclosure55351, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47509, %struct.ScmObj* %k47474, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47509, %struct.ScmObj* %args47073, i64 1)
%argslist53673$anf_45bind471930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55353 = alloca %struct.ScmObj*, align 8
%argslist53673$anf_45bind471931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47072, %struct.ScmObj* %argslist53673$anf_45bind471930)
store volatile %struct.ScmObj* %argslist53673$anf_45bind471931, %struct.ScmObj** %stackaddr$prim55353, align 8
%stackaddr$prim55354 = alloca %struct.ScmObj*, align 8
%argslist53673$anf_45bind471932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47509, %struct.ScmObj* %argslist53673$anf_45bind471931)
store volatile %struct.ScmObj* %argslist53673$anf_45bind471932, %struct.ScmObj** %stackaddr$prim55354, align 8
%clofunc55355 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47193)
musttail call tailcc void %clofunc55355(%struct.ScmObj* %anf_45bind47193, %struct.ScmObj* %argslist53673$anf_45bind471932)
ret void
}

define tailcc void @proc_clo$ae47509(%struct.ScmObj* %env$ae47509,%struct.ScmObj* %current_45args53670) {
%stackaddr$env-ref55356 = alloca %struct.ScmObj*, align 8
%k47474 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47509, i64 0)
store %struct.ScmObj* %k47474, %struct.ScmObj** %stackaddr$env-ref55356
%stackaddr$env-ref55357 = alloca %struct.ScmObj*, align 8
%args47073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47509, i64 1)
store %struct.ScmObj* %args47073, %struct.ScmObj** %stackaddr$env-ref55357
%stackaddr$prim55358 = alloca %struct.ScmObj*, align 8
%_95k47476 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53670)
store volatile %struct.ScmObj* %_95k47476, %struct.ScmObj** %stackaddr$prim55358, align 8
%stackaddr$prim55359 = alloca %struct.ScmObj*, align 8
%current_45args53671 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53670)
store volatile %struct.ScmObj* %current_45args53671, %struct.ScmObj** %stackaddr$prim55359, align 8
%stackaddr$prim55360 = alloca %struct.ScmObj*, align 8
%anf_45bind47194 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53671)
store volatile %struct.ScmObj* %anf_45bind47194, %struct.ScmObj** %stackaddr$prim55360, align 8
%stackaddr$prim55361 = alloca %struct.ScmObj*, align 8
%cpsargs47477 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47474, %struct.ScmObj* %args47073)
store volatile %struct.ScmObj* %cpsargs47477, %struct.ScmObj** %stackaddr$prim55361, align 8
%clofunc55362 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47194)
musttail call tailcc void %clofunc55362(%struct.ScmObj* %anf_45bind47194, %struct.ScmObj* %cpsargs47477)
ret void
}

define tailcc void @proc_clo$ae47481(%struct.ScmObj* %env$ae47481,%struct.ScmObj* %current_45args53678) {
%stackaddr$prim55363 = alloca %struct.ScmObj*, align 8
%k47478 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53678)
store volatile %struct.ScmObj* %k47478, %struct.ScmObj** %stackaddr$prim55363, align 8
%stackaddr$prim55364 = alloca %struct.ScmObj*, align 8
%current_45args53679 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53678)
store volatile %struct.ScmObj* %current_45args53679, %struct.ScmObj** %stackaddr$prim55364, align 8
%stackaddr$prim55365 = alloca %struct.ScmObj*, align 8
%yu47070 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53679)
store volatile %struct.ScmObj* %yu47070, %struct.ScmObj** %stackaddr$prim55365, align 8
%argslist53681$yu470700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55366 = alloca %struct.ScmObj*, align 8
%argslist53681$yu470701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu47070, %struct.ScmObj* %argslist53681$yu470700)
store volatile %struct.ScmObj* %argslist53681$yu470701, %struct.ScmObj** %stackaddr$prim55366, align 8
%stackaddr$prim55367 = alloca %struct.ScmObj*, align 8
%argslist53681$yu470702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47478, %struct.ScmObj* %argslist53681$yu470701)
store volatile %struct.ScmObj* %argslist53681$yu470702, %struct.ScmObj** %stackaddr$prim55367, align 8
%clofunc55368 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu47070)
musttail call tailcc void %clofunc55368(%struct.ScmObj* %yu47070, %struct.ScmObj* %argslist53681$yu470702)
ret void
}