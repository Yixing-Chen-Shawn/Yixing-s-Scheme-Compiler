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
%mainenv54642 = call %struct.ScmObj* @const_init_null()
%mainargs54643 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv54642, %struct.ScmObj* %mainargs54643)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv54640,%struct.ScmObj* %mainargs54641) {
%stackaddr$makeclosure54644 = alloca %struct.ScmObj*, align 8
%fptrToInt54645 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48436 to i64
%ae48436 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54645)
store volatile %struct.ScmObj* %ae48436, %struct.ScmObj** %stackaddr$makeclosure54644, align 8
%ae48437 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54646 = alloca %struct.ScmObj*, align 8
%fptrToInt54647 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48438 to i64
%ae48438 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54647)
store volatile %struct.ScmObj* %ae48438, %struct.ScmObj** %stackaddr$makeclosure54646, align 8
%argslist54639$ae484360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54648 = alloca %struct.ScmObj*, align 8
%argslist54639$ae484361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48438, %struct.ScmObj* %argslist54639$ae484360)
store volatile %struct.ScmObj* %argslist54639$ae484361, %struct.ScmObj** %stackaddr$prim54648, align 8
%stackaddr$prim54649 = alloca %struct.ScmObj*, align 8
%argslist54639$ae484362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48437, %struct.ScmObj* %argslist54639$ae484361)
store volatile %struct.ScmObj* %argslist54639$ae484362, %struct.ScmObj** %stackaddr$prim54649, align 8
%clofunc54650 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48436)
musttail call tailcc void %clofunc54650(%struct.ScmObj* %ae48436, %struct.ScmObj* %argslist54639$ae484362)
ret void
}

define tailcc void @proc_clo$ae48436(%struct.ScmObj* %env$ae48436,%struct.ScmObj* %current_45args54090) {
%stackaddr$prim54651 = alloca %struct.ScmObj*, align 8
%_95k48268 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54090)
store volatile %struct.ScmObj* %_95k48268, %struct.ScmObj** %stackaddr$prim54651, align 8
%stackaddr$prim54652 = alloca %struct.ScmObj*, align 8
%current_45args54091 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54090)
store volatile %struct.ScmObj* %current_45args54091, %struct.ScmObj** %stackaddr$prim54652, align 8
%stackaddr$prim54653 = alloca %struct.ScmObj*, align 8
%anf_45bind48149 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54091)
store volatile %struct.ScmObj* %anf_45bind48149, %struct.ScmObj** %stackaddr$prim54653, align 8
%stackaddr$makeclosure54654 = alloca %struct.ScmObj*, align 8
%fptrToInt54655 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48451 to i64
%ae48451 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54655)
store volatile %struct.ScmObj* %ae48451, %struct.ScmObj** %stackaddr$makeclosure54654, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48451, %struct.ScmObj* %anf_45bind48149, i64 0)
%ae48452 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54656 = alloca %struct.ScmObj*, align 8
%fptrToInt54657 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48453 to i64
%ae48453 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54657)
store volatile %struct.ScmObj* %ae48453, %struct.ScmObj** %stackaddr$makeclosure54656, align 8
%argslist54634$ae484510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54658 = alloca %struct.ScmObj*, align 8
%argslist54634$ae484511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48453, %struct.ScmObj* %argslist54634$ae484510)
store volatile %struct.ScmObj* %argslist54634$ae484511, %struct.ScmObj** %stackaddr$prim54658, align 8
%stackaddr$prim54659 = alloca %struct.ScmObj*, align 8
%argslist54634$ae484512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48452, %struct.ScmObj* %argslist54634$ae484511)
store volatile %struct.ScmObj* %argslist54634$ae484512, %struct.ScmObj** %stackaddr$prim54659, align 8
%clofunc54660 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48451)
musttail call tailcc void %clofunc54660(%struct.ScmObj* %ae48451, %struct.ScmObj* %argslist54634$ae484512)
ret void
}

define tailcc void @proc_clo$ae48451(%struct.ScmObj* %env$ae48451,%struct.ScmObj* %current_45args54093) {
%stackaddr$env-ref54661 = alloca %struct.ScmObj*, align 8
%anf_45bind48149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48451, i64 0)
store %struct.ScmObj* %anf_45bind48149, %struct.ScmObj** %stackaddr$env-ref54661
%stackaddr$prim54662 = alloca %struct.ScmObj*, align 8
%_95k48269 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54093)
store volatile %struct.ScmObj* %_95k48269, %struct.ScmObj** %stackaddr$prim54662, align 8
%stackaddr$prim54663 = alloca %struct.ScmObj*, align 8
%current_45args54094 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54093)
store volatile %struct.ScmObj* %current_45args54094, %struct.ScmObj** %stackaddr$prim54663, align 8
%stackaddr$prim54664 = alloca %struct.ScmObj*, align 8
%anf_45bind48153 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54094)
store volatile %struct.ScmObj* %anf_45bind48153, %struct.ScmObj** %stackaddr$prim54664, align 8
%stackaddr$makeclosure54665 = alloca %struct.ScmObj*, align 8
%fptrToInt54666 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48566 to i64
%ae48566 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54666)
store volatile %struct.ScmObj* %ae48566, %struct.ScmObj** %stackaddr$makeclosure54665, align 8
%argslist54613$anf_45bind481490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54667 = alloca %struct.ScmObj*, align 8
%argslist54613$anf_45bind481491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48153, %struct.ScmObj* %argslist54613$anf_45bind481490)
store volatile %struct.ScmObj* %argslist54613$anf_45bind481491, %struct.ScmObj** %stackaddr$prim54667, align 8
%stackaddr$prim54668 = alloca %struct.ScmObj*, align 8
%argslist54613$anf_45bind481492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48566, %struct.ScmObj* %argslist54613$anf_45bind481491)
store volatile %struct.ScmObj* %argslist54613$anf_45bind481492, %struct.ScmObj** %stackaddr$prim54668, align 8
%clofunc54669 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48149)
musttail call tailcc void %clofunc54669(%struct.ScmObj* %anf_45bind48149, %struct.ScmObj* %argslist54613$anf_45bind481492)
ret void
}

define tailcc void @proc_clo$ae48566(%struct.ScmObj* %env$ae48566,%struct.ScmObj* %current_45args54096) {
%stackaddr$prim54670 = alloca %struct.ScmObj*, align 8
%_95k48270 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54096)
store volatile %struct.ScmObj* %_95k48270, %struct.ScmObj** %stackaddr$prim54670, align 8
%stackaddr$prim54671 = alloca %struct.ScmObj*, align 8
%current_45args54097 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54096)
store volatile %struct.ScmObj* %current_45args54097, %struct.ScmObj** %stackaddr$prim54671, align 8
%stackaddr$prim54672 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54097)
store volatile %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$prim54672, align 8
%stackaddr$makeclosure54673 = alloca %struct.ScmObj*, align 8
%fptrToInt54674 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48568 to i64
%ae48568 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54674)
store volatile %struct.ScmObj* %ae48568, %struct.ScmObj** %stackaddr$makeclosure54673, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48568, %struct.ScmObj* %Ycmb48026, i64 0)
%ae48569 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54675 = alloca %struct.ScmObj*, align 8
%fptrToInt54676 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48570 to i64
%ae48570 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54676)
store volatile %struct.ScmObj* %ae48570, %struct.ScmObj** %stackaddr$makeclosure54675, align 8
%argslist54612$ae485680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54677 = alloca %struct.ScmObj*, align 8
%argslist54612$ae485681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48570, %struct.ScmObj* %argslist54612$ae485680)
store volatile %struct.ScmObj* %argslist54612$ae485681, %struct.ScmObj** %stackaddr$prim54677, align 8
%stackaddr$prim54678 = alloca %struct.ScmObj*, align 8
%argslist54612$ae485682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48569, %struct.ScmObj* %argslist54612$ae485681)
store volatile %struct.ScmObj* %argslist54612$ae485682, %struct.ScmObj** %stackaddr$prim54678, align 8
%clofunc54679 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48568)
musttail call tailcc void %clofunc54679(%struct.ScmObj* %ae48568, %struct.ScmObj* %argslist54612$ae485682)
ret void
}

define tailcc void @proc_clo$ae48568(%struct.ScmObj* %env$ae48568,%struct.ScmObj* %current_45args54099) {
%stackaddr$env-ref54680 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48568, i64 0)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref54680
%stackaddr$prim54681 = alloca %struct.ScmObj*, align 8
%_95k48271 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54099)
store volatile %struct.ScmObj* %_95k48271, %struct.ScmObj** %stackaddr$prim54681, align 8
%stackaddr$prim54682 = alloca %struct.ScmObj*, align 8
%current_45args54100 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54099)
store volatile %struct.ScmObj* %current_45args54100, %struct.ScmObj** %stackaddr$prim54682, align 8
%stackaddr$prim54683 = alloca %struct.ScmObj*, align 8
%anf_45bind48158 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54100)
store volatile %struct.ScmObj* %anf_45bind48158, %struct.ScmObj** %stackaddr$prim54683, align 8
%stackaddr$makeclosure54684 = alloca %struct.ScmObj*, align 8
%fptrToInt54685 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48646 to i64
%ae48646 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54685)
store volatile %struct.ScmObj* %ae48646, %struct.ScmObj** %stackaddr$makeclosure54684, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48646, %struct.ScmObj* %Ycmb48026, i64 0)
%argslist54596$Ycmb480260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54686 = alloca %struct.ScmObj*, align 8
%argslist54596$Ycmb480261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48158, %struct.ScmObj* %argslist54596$Ycmb480260)
store volatile %struct.ScmObj* %argslist54596$Ycmb480261, %struct.ScmObj** %stackaddr$prim54686, align 8
%stackaddr$prim54687 = alloca %struct.ScmObj*, align 8
%argslist54596$Ycmb480262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48646, %struct.ScmObj* %argslist54596$Ycmb480261)
store volatile %struct.ScmObj* %argslist54596$Ycmb480262, %struct.ScmObj** %stackaddr$prim54687, align 8
%clofunc54688 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48026)
musttail call tailcc void %clofunc54688(%struct.ScmObj* %Ycmb48026, %struct.ScmObj* %argslist54596$Ycmb480262)
ret void
}

define tailcc void @proc_clo$ae48646(%struct.ScmObj* %env$ae48646,%struct.ScmObj* %current_45args54102) {
%stackaddr$env-ref54689 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48646, i64 0)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref54689
%stackaddr$prim54690 = alloca %struct.ScmObj*, align 8
%_95k48272 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54102)
store volatile %struct.ScmObj* %_95k48272, %struct.ScmObj** %stackaddr$prim54690, align 8
%stackaddr$prim54691 = alloca %struct.ScmObj*, align 8
%current_45args54103 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54102)
store volatile %struct.ScmObj* %current_45args54103, %struct.ScmObj** %stackaddr$prim54691, align 8
%stackaddr$prim54692 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54103)
store volatile %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$prim54692, align 8
%stackaddr$makeclosure54693 = alloca %struct.ScmObj*, align 8
%fptrToInt54694 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48648 to i64
%ae48648 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54694)
store volatile %struct.ScmObj* %ae48648, %struct.ScmObj** %stackaddr$makeclosure54693, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48648, %struct.ScmObj* %_37foldr148047, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48648, %struct.ScmObj* %Ycmb48026, i64 1)
%ae48649 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54695 = alloca %struct.ScmObj*, align 8
%fptrToInt54696 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48650 to i64
%ae48650 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54696)
store volatile %struct.ScmObj* %ae48650, %struct.ScmObj** %stackaddr$makeclosure54695, align 8
%argslist54595$ae486480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54697 = alloca %struct.ScmObj*, align 8
%argslist54595$ae486481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48650, %struct.ScmObj* %argslist54595$ae486480)
store volatile %struct.ScmObj* %argslist54595$ae486481, %struct.ScmObj** %stackaddr$prim54697, align 8
%stackaddr$prim54698 = alloca %struct.ScmObj*, align 8
%argslist54595$ae486482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48649, %struct.ScmObj* %argslist54595$ae486481)
store volatile %struct.ScmObj* %argslist54595$ae486482, %struct.ScmObj** %stackaddr$prim54698, align 8
%clofunc54699 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48648)
musttail call tailcc void %clofunc54699(%struct.ScmObj* %ae48648, %struct.ScmObj* %argslist54595$ae486482)
ret void
}

define tailcc void @proc_clo$ae48648(%struct.ScmObj* %env$ae48648,%struct.ScmObj* %current_45args54105) {
%stackaddr$env-ref54700 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48648, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref54700
%stackaddr$env-ref54701 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48648, i64 1)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref54701
%stackaddr$prim54702 = alloca %struct.ScmObj*, align 8
%_95k48273 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54105)
store volatile %struct.ScmObj* %_95k48273, %struct.ScmObj** %stackaddr$prim54702, align 8
%stackaddr$prim54703 = alloca %struct.ScmObj*, align 8
%current_45args54106 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54105)
store volatile %struct.ScmObj* %current_45args54106, %struct.ScmObj** %stackaddr$prim54703, align 8
%stackaddr$prim54704 = alloca %struct.ScmObj*, align 8
%anf_45bind48164 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54106)
store volatile %struct.ScmObj* %anf_45bind48164, %struct.ScmObj** %stackaddr$prim54704, align 8
%stackaddr$makeclosure54705 = alloca %struct.ScmObj*, align 8
%fptrToInt54706 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48743 to i64
%ae48743 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54706)
store volatile %struct.ScmObj* %ae48743, %struct.ScmObj** %stackaddr$makeclosure54705, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48743, %struct.ScmObj* %_37foldr148047, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48743, %struct.ScmObj* %Ycmb48026, i64 1)
%argslist54576$Ycmb480260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54707 = alloca %struct.ScmObj*, align 8
%argslist54576$Ycmb480261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48164, %struct.ScmObj* %argslist54576$Ycmb480260)
store volatile %struct.ScmObj* %argslist54576$Ycmb480261, %struct.ScmObj** %stackaddr$prim54707, align 8
%stackaddr$prim54708 = alloca %struct.ScmObj*, align 8
%argslist54576$Ycmb480262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48743, %struct.ScmObj* %argslist54576$Ycmb480261)
store volatile %struct.ScmObj* %argslist54576$Ycmb480262, %struct.ScmObj** %stackaddr$prim54708, align 8
%clofunc54709 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48026)
musttail call tailcc void %clofunc54709(%struct.ScmObj* %Ycmb48026, %struct.ScmObj* %argslist54576$Ycmb480262)
ret void
}

define tailcc void @proc_clo$ae48743(%struct.ScmObj* %env$ae48743,%struct.ScmObj* %current_45args54108) {
%stackaddr$env-ref54710 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48743, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref54710
%stackaddr$env-ref54711 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48743, i64 1)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref54711
%stackaddr$prim54712 = alloca %struct.ScmObj*, align 8
%_95k48274 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54108)
store volatile %struct.ScmObj* %_95k48274, %struct.ScmObj** %stackaddr$prim54712, align 8
%stackaddr$prim54713 = alloca %struct.ScmObj*, align 8
%current_45args54109 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54108)
store volatile %struct.ScmObj* %current_45args54109, %struct.ScmObj** %stackaddr$prim54713, align 8
%stackaddr$prim54714 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54109)
store volatile %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$prim54714, align 8
%stackaddr$makeclosure54715 = alloca %struct.ScmObj*, align 8
%fptrToInt54716 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48745 to i64
%ae48745 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54716)
store volatile %struct.ScmObj* %ae48745, %struct.ScmObj** %stackaddr$makeclosure54715, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48745, %struct.ScmObj* %_37foldr148047, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48745, %struct.ScmObj* %_37map148043, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48745, %struct.ScmObj* %Ycmb48026, i64 2)
%ae48746 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54717 = alloca %struct.ScmObj*, align 8
%fptrToInt54718 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48747 to i64
%ae48747 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54718)
store volatile %struct.ScmObj* %ae48747, %struct.ScmObj** %stackaddr$makeclosure54717, align 8
%argslist54575$ae487450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54719 = alloca %struct.ScmObj*, align 8
%argslist54575$ae487451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48747, %struct.ScmObj* %argslist54575$ae487450)
store volatile %struct.ScmObj* %argslist54575$ae487451, %struct.ScmObj** %stackaddr$prim54719, align 8
%stackaddr$prim54720 = alloca %struct.ScmObj*, align 8
%argslist54575$ae487452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48746, %struct.ScmObj* %argslist54575$ae487451)
store volatile %struct.ScmObj* %argslist54575$ae487452, %struct.ScmObj** %stackaddr$prim54720, align 8
%clofunc54721 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48745)
musttail call tailcc void %clofunc54721(%struct.ScmObj* %ae48745, %struct.ScmObj* %argslist54575$ae487452)
ret void
}

define tailcc void @proc_clo$ae48745(%struct.ScmObj* %env$ae48745,%struct.ScmObj* %current_45args54111) {
%stackaddr$env-ref54722 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48745, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref54722
%stackaddr$env-ref54723 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48745, i64 1)
store %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$env-ref54723
%stackaddr$env-ref54724 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48745, i64 2)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref54724
%stackaddr$prim54725 = alloca %struct.ScmObj*, align 8
%_95k48275 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54111)
store volatile %struct.ScmObj* %_95k48275, %struct.ScmObj** %stackaddr$prim54725, align 8
%stackaddr$prim54726 = alloca %struct.ScmObj*, align 8
%current_45args54112 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54111)
store volatile %struct.ScmObj* %current_45args54112, %struct.ScmObj** %stackaddr$prim54726, align 8
%stackaddr$prim54727 = alloca %struct.ScmObj*, align 8
%anf_45bind48171 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54112)
store volatile %struct.ScmObj* %anf_45bind48171, %struct.ScmObj** %stackaddr$prim54727, align 8
%stackaddr$makeclosure54728 = alloca %struct.ScmObj*, align 8
%fptrToInt54729 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48893 to i64
%ae48893 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54729)
store volatile %struct.ScmObj* %ae48893, %struct.ScmObj** %stackaddr$makeclosure54728, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48893, %struct.ScmObj* %_37foldr148047, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48893, %struct.ScmObj* %_37map148043, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48893, %struct.ScmObj* %Ycmb48026, i64 2)
%argslist54559$Ycmb480260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54730 = alloca %struct.ScmObj*, align 8
%argslist54559$Ycmb480261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48171, %struct.ScmObj* %argslist54559$Ycmb480260)
store volatile %struct.ScmObj* %argslist54559$Ycmb480261, %struct.ScmObj** %stackaddr$prim54730, align 8
%stackaddr$prim54731 = alloca %struct.ScmObj*, align 8
%argslist54559$Ycmb480262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48893, %struct.ScmObj* %argslist54559$Ycmb480261)
store volatile %struct.ScmObj* %argslist54559$Ycmb480262, %struct.ScmObj** %stackaddr$prim54731, align 8
%clofunc54732 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48026)
musttail call tailcc void %clofunc54732(%struct.ScmObj* %Ycmb48026, %struct.ScmObj* %argslist54559$Ycmb480262)
ret void
}

define tailcc void @proc_clo$ae48893(%struct.ScmObj* %env$ae48893,%struct.ScmObj* %current_45args54114) {
%stackaddr$env-ref54733 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48893, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref54733
%stackaddr$env-ref54734 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48893, i64 1)
store %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$env-ref54734
%stackaddr$env-ref54735 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48893, i64 2)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref54735
%stackaddr$prim54736 = alloca %struct.ScmObj*, align 8
%_95k48276 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54114)
store volatile %struct.ScmObj* %_95k48276, %struct.ScmObj** %stackaddr$prim54736, align 8
%stackaddr$prim54737 = alloca %struct.ScmObj*, align 8
%current_45args54115 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54114)
store volatile %struct.ScmObj* %current_45args54115, %struct.ScmObj** %stackaddr$prim54737, align 8
%stackaddr$prim54738 = alloca %struct.ScmObj*, align 8
%_37take48039 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54115)
store volatile %struct.ScmObj* %_37take48039, %struct.ScmObj** %stackaddr$prim54738, align 8
%stackaddr$makeclosure54739 = alloca %struct.ScmObj*, align 8
%fptrToInt54740 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48895 to i64
%ae48895 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54740)
store volatile %struct.ScmObj* %ae48895, %struct.ScmObj** %stackaddr$makeclosure54739, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48895, %struct.ScmObj* %_37take48039, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48895, %struct.ScmObj* %_37foldr148047, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48895, %struct.ScmObj* %_37map148043, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48895, %struct.ScmObj* %Ycmb48026, i64 3)
%ae48896 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54741 = alloca %struct.ScmObj*, align 8
%fptrToInt54742 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48897 to i64
%ae48897 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54742)
store volatile %struct.ScmObj* %ae48897, %struct.ScmObj** %stackaddr$makeclosure54741, align 8
%argslist54558$ae488950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54743 = alloca %struct.ScmObj*, align 8
%argslist54558$ae488951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48897, %struct.ScmObj* %argslist54558$ae488950)
store volatile %struct.ScmObj* %argslist54558$ae488951, %struct.ScmObj** %stackaddr$prim54743, align 8
%stackaddr$prim54744 = alloca %struct.ScmObj*, align 8
%argslist54558$ae488952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48896, %struct.ScmObj* %argslist54558$ae488951)
store volatile %struct.ScmObj* %argslist54558$ae488952, %struct.ScmObj** %stackaddr$prim54744, align 8
%clofunc54745 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48895)
musttail call tailcc void %clofunc54745(%struct.ScmObj* %ae48895, %struct.ScmObj* %argslist54558$ae488952)
ret void
}

define tailcc void @proc_clo$ae48895(%struct.ScmObj* %env$ae48895,%struct.ScmObj* %current_45args54117) {
%stackaddr$env-ref54746 = alloca %struct.ScmObj*, align 8
%_37take48039 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48895, i64 0)
store %struct.ScmObj* %_37take48039, %struct.ScmObj** %stackaddr$env-ref54746
%stackaddr$env-ref54747 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48895, i64 1)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref54747
%stackaddr$env-ref54748 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48895, i64 2)
store %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$env-ref54748
%stackaddr$env-ref54749 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48895, i64 3)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref54749
%stackaddr$prim54750 = alloca %struct.ScmObj*, align 8
%_95k48277 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54117)
store volatile %struct.ScmObj* %_95k48277, %struct.ScmObj** %stackaddr$prim54750, align 8
%stackaddr$prim54751 = alloca %struct.ScmObj*, align 8
%current_45args54118 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54117)
store volatile %struct.ScmObj* %current_45args54118, %struct.ScmObj** %stackaddr$prim54751, align 8
%stackaddr$prim54752 = alloca %struct.ScmObj*, align 8
%anf_45bind48175 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54118)
store volatile %struct.ScmObj* %anf_45bind48175, %struct.ScmObj** %stackaddr$prim54752, align 8
%stackaddr$makeclosure54753 = alloca %struct.ScmObj*, align 8
%fptrToInt54754 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48976 to i64
%ae48976 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54754)
store volatile %struct.ScmObj* %ae48976, %struct.ScmObj** %stackaddr$makeclosure54753, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48976, %struct.ScmObj* %_37take48039, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48976, %struct.ScmObj* %_37foldr148047, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48976, %struct.ScmObj* %_37map148043, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48976, %struct.ScmObj* %Ycmb48026, i64 3)
%argslist54544$Ycmb480260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54755 = alloca %struct.ScmObj*, align 8
%argslist54544$Ycmb480261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48175, %struct.ScmObj* %argslist54544$Ycmb480260)
store volatile %struct.ScmObj* %argslist54544$Ycmb480261, %struct.ScmObj** %stackaddr$prim54755, align 8
%stackaddr$prim54756 = alloca %struct.ScmObj*, align 8
%argslist54544$Ycmb480262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48976, %struct.ScmObj* %argslist54544$Ycmb480261)
store volatile %struct.ScmObj* %argslist54544$Ycmb480262, %struct.ScmObj** %stackaddr$prim54756, align 8
%clofunc54757 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48026)
musttail call tailcc void %clofunc54757(%struct.ScmObj* %Ycmb48026, %struct.ScmObj* %argslist54544$Ycmb480262)
ret void
}

define tailcc void @proc_clo$ae48976(%struct.ScmObj* %env$ae48976,%struct.ScmObj* %current_45args54120) {
%stackaddr$env-ref54758 = alloca %struct.ScmObj*, align 8
%_37take48039 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48976, i64 0)
store %struct.ScmObj* %_37take48039, %struct.ScmObj** %stackaddr$env-ref54758
%stackaddr$env-ref54759 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48976, i64 1)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref54759
%stackaddr$env-ref54760 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48976, i64 2)
store %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$env-ref54760
%stackaddr$env-ref54761 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48976, i64 3)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref54761
%stackaddr$prim54762 = alloca %struct.ScmObj*, align 8
%_95k48278 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54120)
store volatile %struct.ScmObj* %_95k48278, %struct.ScmObj** %stackaddr$prim54762, align 8
%stackaddr$prim54763 = alloca %struct.ScmObj*, align 8
%current_45args54121 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54120)
store volatile %struct.ScmObj* %current_45args54121, %struct.ScmObj** %stackaddr$prim54763, align 8
%stackaddr$prim54764 = alloca %struct.ScmObj*, align 8
%_37length48036 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54121)
store volatile %struct.ScmObj* %_37length48036, %struct.ScmObj** %stackaddr$prim54764, align 8
%stackaddr$makeclosure54765 = alloca %struct.ScmObj*, align 8
%fptrToInt54766 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48978 to i64
%ae48978 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54766)
store volatile %struct.ScmObj* %ae48978, %struct.ScmObj** %stackaddr$makeclosure54765, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48978, %struct.ScmObj* %_37take48039, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48978, %struct.ScmObj* %_37length48036, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48978, %struct.ScmObj* %_37foldr148047, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48978, %struct.ScmObj* %_37map148043, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48978, %struct.ScmObj* %Ycmb48026, i64 4)
%ae48979 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54767 = alloca %struct.ScmObj*, align 8
%fptrToInt54768 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48980 to i64
%ae48980 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54768)
store volatile %struct.ScmObj* %ae48980, %struct.ScmObj** %stackaddr$makeclosure54767, align 8
%argslist54543$ae489780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54769 = alloca %struct.ScmObj*, align 8
%argslist54543$ae489781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48980, %struct.ScmObj* %argslist54543$ae489780)
store volatile %struct.ScmObj* %argslist54543$ae489781, %struct.ScmObj** %stackaddr$prim54769, align 8
%stackaddr$prim54770 = alloca %struct.ScmObj*, align 8
%argslist54543$ae489782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48979, %struct.ScmObj* %argslist54543$ae489781)
store volatile %struct.ScmObj* %argslist54543$ae489782, %struct.ScmObj** %stackaddr$prim54770, align 8
%clofunc54771 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48978)
musttail call tailcc void %clofunc54771(%struct.ScmObj* %ae48978, %struct.ScmObj* %argslist54543$ae489782)
ret void
}

define tailcc void @proc_clo$ae48978(%struct.ScmObj* %env$ae48978,%struct.ScmObj* %current_45args54123) {
%stackaddr$env-ref54772 = alloca %struct.ScmObj*, align 8
%_37take48039 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48978, i64 0)
store %struct.ScmObj* %_37take48039, %struct.ScmObj** %stackaddr$env-ref54772
%stackaddr$env-ref54773 = alloca %struct.ScmObj*, align 8
%_37length48036 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48978, i64 1)
store %struct.ScmObj* %_37length48036, %struct.ScmObj** %stackaddr$env-ref54773
%stackaddr$env-ref54774 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48978, i64 2)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref54774
%stackaddr$env-ref54775 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48978, i64 3)
store %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$env-ref54775
%stackaddr$env-ref54776 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48978, i64 4)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref54776
%stackaddr$prim54777 = alloca %struct.ScmObj*, align 8
%_95k48279 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54123)
store volatile %struct.ScmObj* %_95k48279, %struct.ScmObj** %stackaddr$prim54777, align 8
%stackaddr$prim54778 = alloca %struct.ScmObj*, align 8
%current_45args54124 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54123)
store volatile %struct.ScmObj* %current_45args54124, %struct.ScmObj** %stackaddr$prim54778, align 8
%stackaddr$prim54779 = alloca %struct.ScmObj*, align 8
%anf_45bind48180 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54124)
store volatile %struct.ScmObj* %anf_45bind48180, %struct.ScmObj** %stackaddr$prim54779, align 8
%stackaddr$makeclosure54780 = alloca %struct.ScmObj*, align 8
%fptrToInt54781 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49055 to i64
%ae49055 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54781)
store volatile %struct.ScmObj* %ae49055, %struct.ScmObj** %stackaddr$makeclosure54780, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49055, %struct.ScmObj* %_37take48039, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49055, %struct.ScmObj* %_37length48036, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49055, %struct.ScmObj* %_37foldr148047, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49055, %struct.ScmObj* %_37map148043, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49055, %struct.ScmObj* %Ycmb48026, i64 4)
%argslist54527$Ycmb480260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54782 = alloca %struct.ScmObj*, align 8
%argslist54527$Ycmb480261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48180, %struct.ScmObj* %argslist54527$Ycmb480260)
store volatile %struct.ScmObj* %argslist54527$Ycmb480261, %struct.ScmObj** %stackaddr$prim54782, align 8
%stackaddr$prim54783 = alloca %struct.ScmObj*, align 8
%argslist54527$Ycmb480262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49055, %struct.ScmObj* %argslist54527$Ycmb480261)
store volatile %struct.ScmObj* %argslist54527$Ycmb480262, %struct.ScmObj** %stackaddr$prim54783, align 8
%clofunc54784 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48026)
musttail call tailcc void %clofunc54784(%struct.ScmObj* %Ycmb48026, %struct.ScmObj* %argslist54527$Ycmb480262)
ret void
}

define tailcc void @proc_clo$ae49055(%struct.ScmObj* %env$ae49055,%struct.ScmObj* %current_45args54126) {
%stackaddr$env-ref54785 = alloca %struct.ScmObj*, align 8
%_37take48039 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49055, i64 0)
store %struct.ScmObj* %_37take48039, %struct.ScmObj** %stackaddr$env-ref54785
%stackaddr$env-ref54786 = alloca %struct.ScmObj*, align 8
%_37length48036 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49055, i64 1)
store %struct.ScmObj* %_37length48036, %struct.ScmObj** %stackaddr$env-ref54786
%stackaddr$env-ref54787 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49055, i64 2)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref54787
%stackaddr$env-ref54788 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49055, i64 3)
store %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$env-ref54788
%stackaddr$env-ref54789 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49055, i64 4)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref54789
%stackaddr$prim54790 = alloca %struct.ScmObj*, align 8
%_95k48280 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54126)
store volatile %struct.ScmObj* %_95k48280, %struct.ScmObj** %stackaddr$prim54790, align 8
%stackaddr$prim54791 = alloca %struct.ScmObj*, align 8
%current_45args54127 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54126)
store volatile %struct.ScmObj* %current_45args54127, %struct.ScmObj** %stackaddr$prim54791, align 8
%stackaddr$prim54792 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54127)
store volatile %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$prim54792, align 8
%stackaddr$makeclosure54793 = alloca %struct.ScmObj*, align 8
%fptrToInt54794 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49057 to i64
%ae49057 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt54794)
store volatile %struct.ScmObj* %ae49057, %struct.ScmObj** %stackaddr$makeclosure54793, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49057, %struct.ScmObj* %_37foldr148047, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49057, %struct.ScmObj* %_37foldl148031, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49057, %struct.ScmObj* %_37take48039, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49057, %struct.ScmObj* %_37length48036, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49057, %struct.ScmObj* %_37map148043, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49057, %struct.ScmObj* %Ycmb48026, i64 5)
%ae49058 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54795 = alloca %struct.ScmObj*, align 8
%fptrToInt54796 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49059 to i64
%ae49059 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54796)
store volatile %struct.ScmObj* %ae49059, %struct.ScmObj** %stackaddr$makeclosure54795, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49059, %struct.ScmObj* %_37foldl148031, i64 0)
%argslist54526$ae490570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54797 = alloca %struct.ScmObj*, align 8
%argslist54526$ae490571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49059, %struct.ScmObj* %argslist54526$ae490570)
store volatile %struct.ScmObj* %argslist54526$ae490571, %struct.ScmObj** %stackaddr$prim54797, align 8
%stackaddr$prim54798 = alloca %struct.ScmObj*, align 8
%argslist54526$ae490572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49058, %struct.ScmObj* %argslist54526$ae490571)
store volatile %struct.ScmObj* %argslist54526$ae490572, %struct.ScmObj** %stackaddr$prim54798, align 8
%clofunc54799 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49057)
musttail call tailcc void %clofunc54799(%struct.ScmObj* %ae49057, %struct.ScmObj* %argslist54526$ae490572)
ret void
}

define tailcc void @proc_clo$ae49057(%struct.ScmObj* %env$ae49057,%struct.ScmObj* %current_45args54129) {
%stackaddr$env-ref54800 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49057, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref54800
%stackaddr$env-ref54801 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49057, i64 1)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref54801
%stackaddr$env-ref54802 = alloca %struct.ScmObj*, align 8
%_37take48039 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49057, i64 2)
store %struct.ScmObj* %_37take48039, %struct.ScmObj** %stackaddr$env-ref54802
%stackaddr$env-ref54803 = alloca %struct.ScmObj*, align 8
%_37length48036 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49057, i64 3)
store %struct.ScmObj* %_37length48036, %struct.ScmObj** %stackaddr$env-ref54803
%stackaddr$env-ref54804 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49057, i64 4)
store %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$env-ref54804
%stackaddr$env-ref54805 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49057, i64 5)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref54805
%stackaddr$prim54806 = alloca %struct.ScmObj*, align 8
%_95k48281 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54129)
store volatile %struct.ScmObj* %_95k48281, %struct.ScmObj** %stackaddr$prim54806, align 8
%stackaddr$prim54807 = alloca %struct.ScmObj*, align 8
%current_45args54130 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54129)
store volatile %struct.ScmObj* %current_45args54130, %struct.ScmObj** %stackaddr$prim54807, align 8
%stackaddr$prim54808 = alloca %struct.ScmObj*, align 8
%_37last48069 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54130)
store volatile %struct.ScmObj* %_37last48069, %struct.ScmObj** %stackaddr$prim54808, align 8
%stackaddr$makeclosure54809 = alloca %struct.ScmObj*, align 8
%fptrToInt54810 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49111 to i64
%ae49111 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54810)
store volatile %struct.ScmObj* %ae49111, %struct.ScmObj** %stackaddr$makeclosure54809, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49111, %struct.ScmObj* %_37foldr148047, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49111, %struct.ScmObj* %_37foldl148031, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49111, %struct.ScmObj* %_37last48069, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49111, %struct.ScmObj* %_37map148043, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49111, %struct.ScmObj* %Ycmb48026, i64 4)
%ae49112 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54811 = alloca %struct.ScmObj*, align 8
%fptrToInt54812 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49113 to i64
%ae49113 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54812)
store volatile %struct.ScmObj* %ae49113, %struct.ScmObj** %stackaddr$makeclosure54811, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49113, %struct.ScmObj* %_37take48039, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49113, %struct.ScmObj* %_37length48036, i64 1)
%argslist54512$ae491110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54813 = alloca %struct.ScmObj*, align 8
%argslist54512$ae491111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49113, %struct.ScmObj* %argslist54512$ae491110)
store volatile %struct.ScmObj* %argslist54512$ae491111, %struct.ScmObj** %stackaddr$prim54813, align 8
%stackaddr$prim54814 = alloca %struct.ScmObj*, align 8
%argslist54512$ae491112 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49112, %struct.ScmObj* %argslist54512$ae491111)
store volatile %struct.ScmObj* %argslist54512$ae491112, %struct.ScmObj** %stackaddr$prim54814, align 8
%clofunc54815 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49111)
musttail call tailcc void %clofunc54815(%struct.ScmObj* %ae49111, %struct.ScmObj* %argslist54512$ae491112)
ret void
}

define tailcc void @proc_clo$ae49111(%struct.ScmObj* %env$ae49111,%struct.ScmObj* %current_45args54132) {
%stackaddr$env-ref54816 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49111, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref54816
%stackaddr$env-ref54817 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49111, i64 1)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref54817
%stackaddr$env-ref54818 = alloca %struct.ScmObj*, align 8
%_37last48069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49111, i64 2)
store %struct.ScmObj* %_37last48069, %struct.ScmObj** %stackaddr$env-ref54818
%stackaddr$env-ref54819 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49111, i64 3)
store %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$env-ref54819
%stackaddr$env-ref54820 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49111, i64 4)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref54820
%stackaddr$prim54821 = alloca %struct.ScmObj*, align 8
%_95k48282 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54132)
store volatile %struct.ScmObj* %_95k48282, %struct.ScmObj** %stackaddr$prim54821, align 8
%stackaddr$prim54822 = alloca %struct.ScmObj*, align 8
%current_45args54133 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54132)
store volatile %struct.ScmObj* %current_45args54133, %struct.ScmObj** %stackaddr$prim54822, align 8
%stackaddr$prim54823 = alloca %struct.ScmObj*, align 8
%_37drop_45right48066 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54133)
store volatile %struct.ScmObj* %_37drop_45right48066, %struct.ScmObj** %stackaddr$prim54823, align 8
%stackaddr$makeclosure54824 = alloca %struct.ScmObj*, align 8
%fptrToInt54825 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49141 to i64
%ae49141 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54825)
store volatile %struct.ScmObj* %ae49141, %struct.ScmObj** %stackaddr$makeclosure54824, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49141, %struct.ScmObj* %_37foldr148047, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49141, %struct.ScmObj* %_37foldl148031, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49141, %struct.ScmObj* %_37last48069, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49141, %struct.ScmObj* %_37drop_45right48066, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49141, %struct.ScmObj* %Ycmb48026, i64 4)
%ae49142 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54826 = alloca %struct.ScmObj*, align 8
%fptrToInt54827 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49143 to i64
%ae49143 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54827)
store volatile %struct.ScmObj* %ae49143, %struct.ScmObj** %stackaddr$makeclosure54826, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49143, %struct.ScmObj* %_37foldr148047, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49143, %struct.ScmObj* %_37map148043, i64 1)
%argslist54502$ae491410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54828 = alloca %struct.ScmObj*, align 8
%argslist54502$ae491411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49143, %struct.ScmObj* %argslist54502$ae491410)
store volatile %struct.ScmObj* %argslist54502$ae491411, %struct.ScmObj** %stackaddr$prim54828, align 8
%stackaddr$prim54829 = alloca %struct.ScmObj*, align 8
%argslist54502$ae491412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49142, %struct.ScmObj* %argslist54502$ae491411)
store volatile %struct.ScmObj* %argslist54502$ae491412, %struct.ScmObj** %stackaddr$prim54829, align 8
%clofunc54830 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49141)
musttail call tailcc void %clofunc54830(%struct.ScmObj* %ae49141, %struct.ScmObj* %argslist54502$ae491412)
ret void
}

define tailcc void @proc_clo$ae49141(%struct.ScmObj* %env$ae49141,%struct.ScmObj* %current_45args54135) {
%stackaddr$env-ref54831 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49141, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref54831
%stackaddr$env-ref54832 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49141, i64 1)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref54832
%stackaddr$env-ref54833 = alloca %struct.ScmObj*, align 8
%_37last48069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49141, i64 2)
store %struct.ScmObj* %_37last48069, %struct.ScmObj** %stackaddr$env-ref54833
%stackaddr$env-ref54834 = alloca %struct.ScmObj*, align 8
%_37drop_45right48066 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49141, i64 3)
store %struct.ScmObj* %_37drop_45right48066, %struct.ScmObj** %stackaddr$env-ref54834
%stackaddr$env-ref54835 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49141, i64 4)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref54835
%stackaddr$prim54836 = alloca %struct.ScmObj*, align 8
%_95k48283 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54135)
store volatile %struct.ScmObj* %_95k48283, %struct.ScmObj** %stackaddr$prim54836, align 8
%stackaddr$prim54837 = alloca %struct.ScmObj*, align 8
%current_45args54136 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54135)
store volatile %struct.ScmObj* %current_45args54136, %struct.ScmObj** %stackaddr$prim54837, align 8
%stackaddr$prim54838 = alloca %struct.ScmObj*, align 8
%anf_45bind48196 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54136)
store volatile %struct.ScmObj* %anf_45bind48196, %struct.ScmObj** %stackaddr$prim54838, align 8
%stackaddr$makeclosure54839 = alloca %struct.ScmObj*, align 8
%fptrToInt54840 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49525 to i64
%ae49525 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54840)
store volatile %struct.ScmObj* %ae49525, %struct.ScmObj** %stackaddr$makeclosure54839, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49525, %struct.ScmObj* %_37foldr148047, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49525, %struct.ScmObj* %_37foldl148031, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49525, %struct.ScmObj* %_37last48069, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49525, %struct.ScmObj* %_37drop_45right48066, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49525, %struct.ScmObj* %Ycmb48026, i64 4)
%argslist54442$Ycmb480260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54841 = alloca %struct.ScmObj*, align 8
%argslist54442$Ycmb480261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48196, %struct.ScmObj* %argslist54442$Ycmb480260)
store volatile %struct.ScmObj* %argslist54442$Ycmb480261, %struct.ScmObj** %stackaddr$prim54841, align 8
%stackaddr$prim54842 = alloca %struct.ScmObj*, align 8
%argslist54442$Ycmb480262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49525, %struct.ScmObj* %argslist54442$Ycmb480261)
store volatile %struct.ScmObj* %argslist54442$Ycmb480262, %struct.ScmObj** %stackaddr$prim54842, align 8
%clofunc54843 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48026)
musttail call tailcc void %clofunc54843(%struct.ScmObj* %Ycmb48026, %struct.ScmObj* %argslist54442$Ycmb480262)
ret void
}

define tailcc void @proc_clo$ae49525(%struct.ScmObj* %env$ae49525,%struct.ScmObj* %current_45args54138) {
%stackaddr$env-ref54844 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49525, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref54844
%stackaddr$env-ref54845 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49525, i64 1)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref54845
%stackaddr$env-ref54846 = alloca %struct.ScmObj*, align 8
%_37last48069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49525, i64 2)
store %struct.ScmObj* %_37last48069, %struct.ScmObj** %stackaddr$env-ref54846
%stackaddr$env-ref54847 = alloca %struct.ScmObj*, align 8
%_37drop_45right48066 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49525, i64 3)
store %struct.ScmObj* %_37drop_45right48066, %struct.ScmObj** %stackaddr$env-ref54847
%stackaddr$env-ref54848 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49525, i64 4)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref54848
%stackaddr$prim54849 = alloca %struct.ScmObj*, align 8
%_95k48284 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54138)
store volatile %struct.ScmObj* %_95k48284, %struct.ScmObj** %stackaddr$prim54849, align 8
%stackaddr$prim54850 = alloca %struct.ScmObj*, align 8
%current_45args54139 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54138)
store volatile %struct.ScmObj* %current_45args54139, %struct.ScmObj** %stackaddr$prim54850, align 8
%stackaddr$prim54851 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54139)
store volatile %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$prim54851, align 8
%stackaddr$makeclosure54852 = alloca %struct.ScmObj*, align 8
%fptrToInt54853 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49527 to i64
%ae49527 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt54853)
store volatile %struct.ScmObj* %ae49527, %struct.ScmObj** %stackaddr$makeclosure54852, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49527, %struct.ScmObj* %_37foldr148047, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49527, %struct.ScmObj* %_37foldl148031, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49527, %struct.ScmObj* %_37last48069, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49527, %struct.ScmObj* %_37foldr48052, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49527, %struct.ScmObj* %_37drop_45right48066, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49527, %struct.ScmObj* %Ycmb48026, i64 5)
%ae49528 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54854 = alloca %struct.ScmObj*, align 8
%fptrToInt54855 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49529 to i64
%ae49529 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54855)
store volatile %struct.ScmObj* %ae49529, %struct.ScmObj** %stackaddr$makeclosure54854, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49529, %struct.ScmObj* %_37foldr148047, i64 0)
%argslist54441$ae495270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54856 = alloca %struct.ScmObj*, align 8
%argslist54441$ae495271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49529, %struct.ScmObj* %argslist54441$ae495270)
store volatile %struct.ScmObj* %argslist54441$ae495271, %struct.ScmObj** %stackaddr$prim54856, align 8
%stackaddr$prim54857 = alloca %struct.ScmObj*, align 8
%argslist54441$ae495272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49528, %struct.ScmObj* %argslist54441$ae495271)
store volatile %struct.ScmObj* %argslist54441$ae495272, %struct.ScmObj** %stackaddr$prim54857, align 8
%clofunc54858 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49527)
musttail call tailcc void %clofunc54858(%struct.ScmObj* %ae49527, %struct.ScmObj* %argslist54441$ae495272)
ret void
}

define tailcc void @proc_clo$ae49527(%struct.ScmObj* %env$ae49527,%struct.ScmObj* %current_45args54141) {
%stackaddr$env-ref54859 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49527, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref54859
%stackaddr$env-ref54860 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49527, i64 1)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref54860
%stackaddr$env-ref54861 = alloca %struct.ScmObj*, align 8
%_37last48069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49527, i64 2)
store %struct.ScmObj* %_37last48069, %struct.ScmObj** %stackaddr$env-ref54861
%stackaddr$env-ref54862 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49527, i64 3)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref54862
%stackaddr$env-ref54863 = alloca %struct.ScmObj*, align 8
%_37drop_45right48066 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49527, i64 4)
store %struct.ScmObj* %_37drop_45right48066, %struct.ScmObj** %stackaddr$env-ref54863
%stackaddr$env-ref54864 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49527, i64 5)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref54864
%stackaddr$prim54865 = alloca %struct.ScmObj*, align 8
%_95k48285 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54141)
store volatile %struct.ScmObj* %_95k48285, %struct.ScmObj** %stackaddr$prim54865, align 8
%stackaddr$prim54866 = alloca %struct.ScmObj*, align 8
%current_45args54142 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54141)
store volatile %struct.ScmObj* %current_45args54142, %struct.ScmObj** %stackaddr$prim54866, align 8
%stackaddr$prim54867 = alloca %struct.ScmObj*, align 8
%_37map148078 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54142)
store volatile %struct.ScmObj* %_37map148078, %struct.ScmObj** %stackaddr$prim54867, align 8
%stackaddr$makeclosure54868 = alloca %struct.ScmObj*, align 8
%fptrToInt54869 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49604 to i64
%ae49604 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54869)
store volatile %struct.ScmObj* %ae49604, %struct.ScmObj** %stackaddr$makeclosure54868, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49604, %struct.ScmObj* %_37foldr148047, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49604, %struct.ScmObj* %_37foldl148031, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49604, %struct.ScmObj* %_37foldr48052, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49604, %struct.ScmObj* %_37map148078, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49604, %struct.ScmObj* %Ycmb48026, i64 4)
%ae49605 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54870 = alloca %struct.ScmObj*, align 8
%fptrToInt54871 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49606 to i64
%ae49606 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54871)
store volatile %struct.ScmObj* %ae49606, %struct.ScmObj** %stackaddr$makeclosure54870, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49606, %struct.ScmObj* %_37last48069, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49606, %struct.ScmObj* %_37foldr48052, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49606, %struct.ScmObj* %_37drop_45right48066, i64 2)
%argslist54422$ae496040 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54872 = alloca %struct.ScmObj*, align 8
%argslist54422$ae496041 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49606, %struct.ScmObj* %argslist54422$ae496040)
store volatile %struct.ScmObj* %argslist54422$ae496041, %struct.ScmObj** %stackaddr$prim54872, align 8
%stackaddr$prim54873 = alloca %struct.ScmObj*, align 8
%argslist54422$ae496042 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49605, %struct.ScmObj* %argslist54422$ae496041)
store volatile %struct.ScmObj* %argslist54422$ae496042, %struct.ScmObj** %stackaddr$prim54873, align 8
%clofunc54874 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49604)
musttail call tailcc void %clofunc54874(%struct.ScmObj* %ae49604, %struct.ScmObj* %argslist54422$ae496042)
ret void
}

define tailcc void @proc_clo$ae49604(%struct.ScmObj* %env$ae49604,%struct.ScmObj* %current_45args54144) {
%stackaddr$env-ref54875 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49604, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref54875
%stackaddr$env-ref54876 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49604, i64 1)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref54876
%stackaddr$env-ref54877 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49604, i64 2)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref54877
%stackaddr$env-ref54878 = alloca %struct.ScmObj*, align 8
%_37map148078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49604, i64 3)
store %struct.ScmObj* %_37map148078, %struct.ScmObj** %stackaddr$env-ref54878
%stackaddr$env-ref54879 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49604, i64 4)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref54879
%stackaddr$prim54880 = alloca %struct.ScmObj*, align 8
%_95k48286 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54144)
store volatile %struct.ScmObj* %_95k48286, %struct.ScmObj** %stackaddr$prim54880, align 8
%stackaddr$prim54881 = alloca %struct.ScmObj*, align 8
%current_45args54145 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54144)
store volatile %struct.ScmObj* %current_45args54145, %struct.ScmObj** %stackaddr$prim54881, align 8
%stackaddr$prim54882 = alloca %struct.ScmObj*, align 8
%_37map48073 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54145)
store volatile %struct.ScmObj* %_37map48073, %struct.ScmObj** %stackaddr$prim54882, align 8
%stackaddr$makeclosure54883 = alloca %struct.ScmObj*, align 8
%fptrToInt54884 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49750 to i64
%ae49750 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54884)
store volatile %struct.ScmObj* %ae49750, %struct.ScmObj** %stackaddr$makeclosure54883, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49750, %struct.ScmObj* %_37foldl148031, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49750, %struct.ScmObj* %Ycmb48026, i64 1)
%ae49751 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54885 = alloca %struct.ScmObj*, align 8
%fptrToInt54886 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49752 to i64
%ae49752 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54886)
store volatile %struct.ScmObj* %ae49752, %struct.ScmObj** %stackaddr$makeclosure54885, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49752, %struct.ScmObj* %_37foldr48052, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49752, %struct.ScmObj* %_37foldr148047, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49752, %struct.ScmObj* %_37map148078, i64 2)
%argslist54405$ae497500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54887 = alloca %struct.ScmObj*, align 8
%argslist54405$ae497501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49752, %struct.ScmObj* %argslist54405$ae497500)
store volatile %struct.ScmObj* %argslist54405$ae497501, %struct.ScmObj** %stackaddr$prim54887, align 8
%stackaddr$prim54888 = alloca %struct.ScmObj*, align 8
%argslist54405$ae497502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49751, %struct.ScmObj* %argslist54405$ae497501)
store volatile %struct.ScmObj* %argslist54405$ae497502, %struct.ScmObj** %stackaddr$prim54888, align 8
%clofunc54889 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49750)
musttail call tailcc void %clofunc54889(%struct.ScmObj* %ae49750, %struct.ScmObj* %argslist54405$ae497502)
ret void
}

define tailcc void @proc_clo$ae49750(%struct.ScmObj* %env$ae49750,%struct.ScmObj* %current_45args54147) {
%stackaddr$env-ref54890 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49750, i64 0)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref54890
%stackaddr$env-ref54891 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49750, i64 1)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref54891
%stackaddr$prim54892 = alloca %struct.ScmObj*, align 8
%_95k48287 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54147)
store volatile %struct.ScmObj* %_95k48287, %struct.ScmObj** %stackaddr$prim54892, align 8
%stackaddr$prim54893 = alloca %struct.ScmObj*, align 8
%current_45args54148 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54147)
store volatile %struct.ScmObj* %current_45args54148, %struct.ScmObj** %stackaddr$prim54893, align 8
%stackaddr$prim54894 = alloca %struct.ScmObj*, align 8
%anf_45bind48216 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54148)
store volatile %struct.ScmObj* %anf_45bind48216, %struct.ScmObj** %stackaddr$prim54894, align 8
%stackaddr$makeclosure54895 = alloca %struct.ScmObj*, align 8
%fptrToInt54896 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50142 to i64
%ae50142 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54896)
store volatile %struct.ScmObj* %ae50142, %struct.ScmObj** %stackaddr$makeclosure54895, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50142, %struct.ScmObj* %_37foldl148031, i64 0)
%argslist54345$Ycmb480260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54897 = alloca %struct.ScmObj*, align 8
%argslist54345$Ycmb480261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48216, %struct.ScmObj* %argslist54345$Ycmb480260)
store volatile %struct.ScmObj* %argslist54345$Ycmb480261, %struct.ScmObj** %stackaddr$prim54897, align 8
%stackaddr$prim54898 = alloca %struct.ScmObj*, align 8
%argslist54345$Ycmb480262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50142, %struct.ScmObj* %argslist54345$Ycmb480261)
store volatile %struct.ScmObj* %argslist54345$Ycmb480262, %struct.ScmObj** %stackaddr$prim54898, align 8
%clofunc54899 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48026)
musttail call tailcc void %clofunc54899(%struct.ScmObj* %Ycmb48026, %struct.ScmObj* %argslist54345$Ycmb480262)
ret void
}

define tailcc void @proc_clo$ae50142(%struct.ScmObj* %env$ae50142,%struct.ScmObj* %current_45args54150) {
%stackaddr$env-ref54900 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50142, i64 0)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref54900
%stackaddr$prim54901 = alloca %struct.ScmObj*, align 8
%_95k48288 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54150)
store volatile %struct.ScmObj* %_95k48288, %struct.ScmObj** %stackaddr$prim54901, align 8
%stackaddr$prim54902 = alloca %struct.ScmObj*, align 8
%current_45args54151 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54150)
store volatile %struct.ScmObj* %current_45args54151, %struct.ScmObj** %stackaddr$prim54902, align 8
%stackaddr$prim54903 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54151)
store volatile %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$prim54903, align 8
%stackaddr$makeclosure54904 = alloca %struct.ScmObj*, align 8
%fptrToInt54905 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50144 to i64
%ae50144 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54905)
store volatile %struct.ScmObj* %ae50144, %struct.ScmObj** %stackaddr$makeclosure54904, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50144, %struct.ScmObj* %_37foldl148031, i64 0)
%ae50145 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54906 = alloca %struct.ScmObj*, align 8
%fptrToInt54907 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50146 to i64
%ae50146 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54907)
store volatile %struct.ScmObj* %ae50146, %struct.ScmObj** %stackaddr$makeclosure54906, align 8
%argslist54344$ae501440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54908 = alloca %struct.ScmObj*, align 8
%argslist54344$ae501441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50146, %struct.ScmObj* %argslist54344$ae501440)
store volatile %struct.ScmObj* %argslist54344$ae501441, %struct.ScmObj** %stackaddr$prim54908, align 8
%stackaddr$prim54909 = alloca %struct.ScmObj*, align 8
%argslist54344$ae501442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50145, %struct.ScmObj* %argslist54344$ae501441)
store volatile %struct.ScmObj* %argslist54344$ae501442, %struct.ScmObj** %stackaddr$prim54909, align 8
%clofunc54910 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50144)
musttail call tailcc void %clofunc54910(%struct.ScmObj* %ae50144, %struct.ScmObj* %argslist54344$ae501442)
ret void
}

define tailcc void @proc_clo$ae50144(%struct.ScmObj* %env$ae50144,%struct.ScmObj* %current_45args54153) {
%stackaddr$env-ref54911 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50144, i64 0)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref54911
%stackaddr$prim54912 = alloca %struct.ScmObj*, align 8
%_95k48289 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54153)
store volatile %struct.ScmObj* %_95k48289, %struct.ScmObj** %stackaddr$prim54912, align 8
%stackaddr$prim54913 = alloca %struct.ScmObj*, align 8
%current_45args54154 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54153)
store volatile %struct.ScmObj* %current_45args54154, %struct.ScmObj** %stackaddr$prim54913, align 8
%stackaddr$prim54914 = alloca %struct.ScmObj*, align 8
%_37_6248126 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54154)
store volatile %struct.ScmObj* %_37_6248126, %struct.ScmObj** %stackaddr$prim54914, align 8
%stackaddr$makeclosure54915 = alloca %struct.ScmObj*, align 8
%fptrToInt54916 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50168 to i64
%ae50168 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54916)
store volatile %struct.ScmObj* %ae50168, %struct.ScmObj** %stackaddr$makeclosure54915, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50168, %struct.ScmObj* %_37foldl148031, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50168, %struct.ScmObj* %_37_6248126, i64 1)
%ae50169 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54917 = alloca %struct.ScmObj*, align 8
%fptrToInt54918 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50170 to i64
%ae50170 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54918)
store volatile %struct.ScmObj* %ae50170, %struct.ScmObj** %stackaddr$makeclosure54917, align 8
%argslist54338$ae501680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54919 = alloca %struct.ScmObj*, align 8
%argslist54338$ae501681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50170, %struct.ScmObj* %argslist54338$ae501680)
store volatile %struct.ScmObj* %argslist54338$ae501681, %struct.ScmObj** %stackaddr$prim54919, align 8
%stackaddr$prim54920 = alloca %struct.ScmObj*, align 8
%argslist54338$ae501682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50169, %struct.ScmObj* %argslist54338$ae501681)
store volatile %struct.ScmObj* %argslist54338$ae501682, %struct.ScmObj** %stackaddr$prim54920, align 8
%clofunc54921 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50168)
musttail call tailcc void %clofunc54921(%struct.ScmObj* %ae50168, %struct.ScmObj* %argslist54338$ae501682)
ret void
}

define tailcc void @proc_clo$ae50168(%struct.ScmObj* %env$ae50168,%struct.ScmObj* %current_45args54156) {
%stackaddr$env-ref54922 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50168, i64 0)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref54922
%stackaddr$env-ref54923 = alloca %struct.ScmObj*, align 8
%_37_6248126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50168, i64 1)
store %struct.ScmObj* %_37_6248126, %struct.ScmObj** %stackaddr$env-ref54923
%stackaddr$prim54924 = alloca %struct.ScmObj*, align 8
%_95k48290 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54156)
store volatile %struct.ScmObj* %_95k48290, %struct.ScmObj** %stackaddr$prim54924, align 8
%stackaddr$prim54925 = alloca %struct.ScmObj*, align 8
%current_45args54157 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54156)
store volatile %struct.ScmObj* %current_45args54157, %struct.ScmObj** %stackaddr$prim54925, align 8
%stackaddr$prim54926 = alloca %struct.ScmObj*, align 8
%_37_62_6148123 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54157)
store volatile %struct.ScmObj* %_37_62_6148123, %struct.ScmObj** %stackaddr$prim54926, align 8
%ae50192 = call %struct.ScmObj* @const_init_int(i64 1)
%ae50193 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54927 = alloca %struct.ScmObj*, align 8
%_37append48119 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50192, %struct.ScmObj* %ae50193)
store volatile %struct.ScmObj* %_37append48119, %struct.ScmObj** %stackaddr$prim54927, align 8
%stackaddr$makeclosure54928 = alloca %struct.ScmObj*, align 8
%fptrToInt54929 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50194 to i64
%ae50194 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54929)
store volatile %struct.ScmObj* %ae50194, %struct.ScmObj** %stackaddr$makeclosure54928, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50194, %struct.ScmObj* %_37append48119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50194, %struct.ScmObj* %_37foldl148031, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50194, %struct.ScmObj* %_37_6248126, i64 2)
%ae50195 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54930 = alloca %struct.ScmObj*, align 8
%fptrToInt54931 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50196 to i64
%ae50196 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54931)
store volatile %struct.ScmObj* %ae50196, %struct.ScmObj** %stackaddr$makeclosure54930, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50196, %struct.ScmObj* %_37append48119, i64 0)
%argslist54332$ae501940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54932 = alloca %struct.ScmObj*, align 8
%argslist54332$ae501941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50196, %struct.ScmObj* %argslist54332$ae501940)
store volatile %struct.ScmObj* %argslist54332$ae501941, %struct.ScmObj** %stackaddr$prim54932, align 8
%stackaddr$prim54933 = alloca %struct.ScmObj*, align 8
%argslist54332$ae501942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50195, %struct.ScmObj* %argslist54332$ae501941)
store volatile %struct.ScmObj* %argslist54332$ae501942, %struct.ScmObj** %stackaddr$prim54933, align 8
%clofunc54934 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50194)
musttail call tailcc void %clofunc54934(%struct.ScmObj* %ae50194, %struct.ScmObj* %argslist54332$ae501942)
ret void
}

define tailcc void @proc_clo$ae50194(%struct.ScmObj* %env$ae50194,%struct.ScmObj* %current_45args54159) {
%stackaddr$env-ref54935 = alloca %struct.ScmObj*, align 8
%_37append48119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50194, i64 0)
store %struct.ScmObj* %_37append48119, %struct.ScmObj** %stackaddr$env-ref54935
%stackaddr$env-ref54936 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50194, i64 1)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref54936
%stackaddr$env-ref54937 = alloca %struct.ScmObj*, align 8
%_37_6248126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50194, i64 2)
store %struct.ScmObj* %_37_6248126, %struct.ScmObj** %stackaddr$env-ref54937
%stackaddr$prim54938 = alloca %struct.ScmObj*, align 8
%_95k48291 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54159)
store volatile %struct.ScmObj* %_95k48291, %struct.ScmObj** %stackaddr$prim54938, align 8
%stackaddr$prim54939 = alloca %struct.ScmObj*, align 8
%current_45args54160 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54159)
store volatile %struct.ScmObj* %current_45args54160, %struct.ScmObj** %stackaddr$prim54939, align 8
%stackaddr$prim54940 = alloca %struct.ScmObj*, align 8
%anf_45bind48224 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54160)
store volatile %struct.ScmObj* %anf_45bind48224, %struct.ScmObj** %stackaddr$prim54940, align 8
%ae50262 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54941 = alloca %struct.ScmObj*, align 8
%_95048120 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append48119, %struct.ScmObj* %ae50262, %struct.ScmObj* %anf_45bind48224)
store volatile %struct.ScmObj* %_95048120, %struct.ScmObj** %stackaddr$prim54941, align 8
%ae50265 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54942 = alloca %struct.ScmObj*, align 8
%_37append48118 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append48119, %struct.ScmObj* %ae50265)
store volatile %struct.ScmObj* %_37append48118, %struct.ScmObj** %stackaddr$prim54942, align 8
%stackaddr$makeclosure54943 = alloca %struct.ScmObj*, align 8
%fptrToInt54944 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50266 to i64
%ae50266 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54944)
store volatile %struct.ScmObj* %ae50266, %struct.ScmObj** %stackaddr$makeclosure54943, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50266, %struct.ScmObj* %_37foldl148031, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50266, %struct.ScmObj* %_37_6248126, i64 1)
%ae50267 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54945 = alloca %struct.ScmObj*, align 8
%fptrToInt54946 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50268 to i64
%ae50268 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54946)
store volatile %struct.ScmObj* %ae50268, %struct.ScmObj** %stackaddr$makeclosure54945, align 8
%argslist54321$ae502660 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54947 = alloca %struct.ScmObj*, align 8
%argslist54321$ae502661 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50268, %struct.ScmObj* %argslist54321$ae502660)
store volatile %struct.ScmObj* %argslist54321$ae502661, %struct.ScmObj** %stackaddr$prim54947, align 8
%stackaddr$prim54948 = alloca %struct.ScmObj*, align 8
%argslist54321$ae502662 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50267, %struct.ScmObj* %argslist54321$ae502661)
store volatile %struct.ScmObj* %argslist54321$ae502662, %struct.ScmObj** %stackaddr$prim54948, align 8
%clofunc54949 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50266)
musttail call tailcc void %clofunc54949(%struct.ScmObj* %ae50266, %struct.ScmObj* %argslist54321$ae502662)
ret void
}

define tailcc void @proc_clo$ae50266(%struct.ScmObj* %env$ae50266,%struct.ScmObj* %current_45args54162) {
%stackaddr$env-ref54950 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50266, i64 0)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref54950
%stackaddr$env-ref54951 = alloca %struct.ScmObj*, align 8
%_37_6248126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50266, i64 1)
store %struct.ScmObj* %_37_6248126, %struct.ScmObj** %stackaddr$env-ref54951
%stackaddr$prim54952 = alloca %struct.ScmObj*, align 8
%_95k48292 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54162)
store volatile %struct.ScmObj* %_95k48292, %struct.ScmObj** %stackaddr$prim54952, align 8
%stackaddr$prim54953 = alloca %struct.ScmObj*, align 8
%current_45args54163 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54162)
store volatile %struct.ScmObj* %current_45args54163, %struct.ScmObj** %stackaddr$prim54953, align 8
%stackaddr$prim54954 = alloca %struct.ScmObj*, align 8
%_37list_6348111 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54163)
store volatile %struct.ScmObj* %_37list_6348111, %struct.ScmObj** %stackaddr$prim54954, align 8
%stackaddr$makeclosure54955 = alloca %struct.ScmObj*, align 8
%fptrToInt54956 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50682 to i64
%ae50682 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54956)
store volatile %struct.ScmObj* %ae50682, %struct.ScmObj** %stackaddr$makeclosure54955, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50682, %struct.ScmObj* %_37foldl148031, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50682, %struct.ScmObj* %_37_6248126, i64 1)
%ae50683 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54957 = alloca %struct.ScmObj*, align 8
%fptrToInt54958 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50684 to i64
%ae50684 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54958)
store volatile %struct.ScmObj* %ae50684, %struct.ScmObj** %stackaddr$makeclosure54957, align 8
%argslist54296$ae506820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54959 = alloca %struct.ScmObj*, align 8
%argslist54296$ae506821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50684, %struct.ScmObj* %argslist54296$ae506820)
store volatile %struct.ScmObj* %argslist54296$ae506821, %struct.ScmObj** %stackaddr$prim54959, align 8
%stackaddr$prim54960 = alloca %struct.ScmObj*, align 8
%argslist54296$ae506822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50683, %struct.ScmObj* %argslist54296$ae506821)
store volatile %struct.ScmObj* %argslist54296$ae506822, %struct.ScmObj** %stackaddr$prim54960, align 8
%clofunc54961 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50682)
musttail call tailcc void %clofunc54961(%struct.ScmObj* %ae50682, %struct.ScmObj* %argslist54296$ae506822)
ret void
}

define tailcc void @proc_clo$ae50682(%struct.ScmObj* %env$ae50682,%struct.ScmObj* %current_45args54165) {
%stackaddr$env-ref54962 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50682, i64 0)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref54962
%stackaddr$env-ref54963 = alloca %struct.ScmObj*, align 8
%_37_6248126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50682, i64 1)
store %struct.ScmObj* %_37_6248126, %struct.ScmObj** %stackaddr$env-ref54963
%stackaddr$prim54964 = alloca %struct.ScmObj*, align 8
%_95k48293 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54165)
store volatile %struct.ScmObj* %_95k48293, %struct.ScmObj** %stackaddr$prim54964, align 8
%stackaddr$prim54965 = alloca %struct.ScmObj*, align 8
%current_45args54166 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54165)
store volatile %struct.ScmObj* %current_45args54166, %struct.ScmObj** %stackaddr$prim54965, align 8
%stackaddr$prim54966 = alloca %struct.ScmObj*, align 8
%_37drop48102 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54166)
store volatile %struct.ScmObj* %_37drop48102, %struct.ScmObj** %stackaddr$prim54966, align 8
%stackaddr$makeclosure54967 = alloca %struct.ScmObj*, align 8
%fptrToInt54968 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51218 to i64
%ae51218 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54968)
store volatile %struct.ScmObj* %ae51218, %struct.ScmObj** %stackaddr$makeclosure54967, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51218, %struct.ScmObj* %_37foldl148031, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51218, %struct.ScmObj* %_37_6248126, i64 1)
%ae51219 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54969 = alloca %struct.ScmObj*, align 8
%fptrToInt54970 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51220 to i64
%ae51220 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54970)
store volatile %struct.ScmObj* %ae51220, %struct.ScmObj** %stackaddr$makeclosure54969, align 8
%argslist54272$ae512180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54971 = alloca %struct.ScmObj*, align 8
%argslist54272$ae512181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51220, %struct.ScmObj* %argslist54272$ae512180)
store volatile %struct.ScmObj* %argslist54272$ae512181, %struct.ScmObj** %stackaddr$prim54971, align 8
%stackaddr$prim54972 = alloca %struct.ScmObj*, align 8
%argslist54272$ae512182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51219, %struct.ScmObj* %argslist54272$ae512181)
store volatile %struct.ScmObj* %argslist54272$ae512182, %struct.ScmObj** %stackaddr$prim54972, align 8
%clofunc54973 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51218)
musttail call tailcc void %clofunc54973(%struct.ScmObj* %ae51218, %struct.ScmObj* %argslist54272$ae512182)
ret void
}

define tailcc void @proc_clo$ae51218(%struct.ScmObj* %env$ae51218,%struct.ScmObj* %current_45args54168) {
%stackaddr$env-ref54974 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51218, i64 0)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref54974
%stackaddr$env-ref54975 = alloca %struct.ScmObj*, align 8
%_37_6248126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51218, i64 1)
store %struct.ScmObj* %_37_6248126, %struct.ScmObj** %stackaddr$env-ref54975
%stackaddr$prim54976 = alloca %struct.ScmObj*, align 8
%_95k48294 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54168)
store volatile %struct.ScmObj* %_95k48294, %struct.ScmObj** %stackaddr$prim54976, align 8
%stackaddr$prim54977 = alloca %struct.ScmObj*, align 8
%current_45args54169 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54168)
store volatile %struct.ScmObj* %current_45args54169, %struct.ScmObj** %stackaddr$prim54977, align 8
%stackaddr$prim54978 = alloca %struct.ScmObj*, align 8
%_37memv48095 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54169)
store volatile %struct.ScmObj* %_37memv48095, %struct.ScmObj** %stackaddr$prim54978, align 8
%stackaddr$makeclosure54979 = alloca %struct.ScmObj*, align 8
%fptrToInt54980 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51622 to i64
%ae51622 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54980)
store volatile %struct.ScmObj* %ae51622, %struct.ScmObj** %stackaddr$makeclosure54979, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51622, %struct.ScmObj* %_37_6248126, i64 0)
%ae51623 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54981 = alloca %struct.ScmObj*, align 8
%fptrToInt54982 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51624 to i64
%ae51624 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54982)
store volatile %struct.ScmObj* %ae51624, %struct.ScmObj** %stackaddr$makeclosure54981, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51624, %struct.ScmObj* %_37foldl148031, i64 0)
%argslist54246$ae516220 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54983 = alloca %struct.ScmObj*, align 8
%argslist54246$ae516221 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51624, %struct.ScmObj* %argslist54246$ae516220)
store volatile %struct.ScmObj* %argslist54246$ae516221, %struct.ScmObj** %stackaddr$prim54983, align 8
%stackaddr$prim54984 = alloca %struct.ScmObj*, align 8
%argslist54246$ae516222 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51623, %struct.ScmObj* %argslist54246$ae516221)
store volatile %struct.ScmObj* %argslist54246$ae516222, %struct.ScmObj** %stackaddr$prim54984, align 8
%clofunc54985 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51622)
musttail call tailcc void %clofunc54985(%struct.ScmObj* %ae51622, %struct.ScmObj* %argslist54246$ae516222)
ret void
}

define tailcc void @proc_clo$ae51622(%struct.ScmObj* %env$ae51622,%struct.ScmObj* %current_45args54171) {
%stackaddr$env-ref54986 = alloca %struct.ScmObj*, align 8
%_37_6248126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51622, i64 0)
store %struct.ScmObj* %_37_6248126, %struct.ScmObj** %stackaddr$env-ref54986
%stackaddr$prim54987 = alloca %struct.ScmObj*, align 8
%_95k48295 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54171)
store volatile %struct.ScmObj* %_95k48295, %struct.ScmObj** %stackaddr$prim54987, align 8
%stackaddr$prim54988 = alloca %struct.ScmObj*, align 8
%current_45args54172 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54171)
store volatile %struct.ScmObj* %current_45args54172, %struct.ScmObj** %stackaddr$prim54988, align 8
%stackaddr$prim54989 = alloca %struct.ScmObj*, align 8
%_37_4748091 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54172)
store volatile %struct.ScmObj* %_37_4748091, %struct.ScmObj** %stackaddr$prim54989, align 8
%stackaddr$makeclosure54990 = alloca %struct.ScmObj*, align 8
%fptrToInt54991 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51720 to i64
%ae51720 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54991)
store volatile %struct.ScmObj* %ae51720, %struct.ScmObj** %stackaddr$makeclosure54990, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51720, %struct.ScmObj* %_37_6248126, i64 0)
%ae51721 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54992 = alloca %struct.ScmObj*, align 8
%fptrToInt54993 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51722 to i64
%ae51722 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54993)
store volatile %struct.ScmObj* %ae51722, %struct.ScmObj** %stackaddr$makeclosure54992, align 8
%argslist54233$ae517200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54994 = alloca %struct.ScmObj*, align 8
%argslist54233$ae517201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51722, %struct.ScmObj* %argslist54233$ae517200)
store volatile %struct.ScmObj* %argslist54233$ae517201, %struct.ScmObj** %stackaddr$prim54994, align 8
%stackaddr$prim54995 = alloca %struct.ScmObj*, align 8
%argslist54233$ae517202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51721, %struct.ScmObj* %argslist54233$ae517201)
store volatile %struct.ScmObj* %argslist54233$ae517202, %struct.ScmObj** %stackaddr$prim54995, align 8
%clofunc54996 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51720)
musttail call tailcc void %clofunc54996(%struct.ScmObj* %ae51720, %struct.ScmObj* %argslist54233$ae517202)
ret void
}

define tailcc void @proc_clo$ae51720(%struct.ScmObj* %env$ae51720,%struct.ScmObj* %current_45args54174) {
%stackaddr$env-ref54997 = alloca %struct.ScmObj*, align 8
%_37_6248126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51720, i64 0)
store %struct.ScmObj* %_37_6248126, %struct.ScmObj** %stackaddr$env-ref54997
%stackaddr$prim54998 = alloca %struct.ScmObj*, align 8
%_95k48296 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54174)
store volatile %struct.ScmObj* %_95k48296, %struct.ScmObj** %stackaddr$prim54998, align 8
%stackaddr$prim54999 = alloca %struct.ScmObj*, align 8
%current_45args54175 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54174)
store volatile %struct.ScmObj* %current_45args54175, %struct.ScmObj** %stackaddr$prim54999, align 8
%stackaddr$prim55000 = alloca %struct.ScmObj*, align 8
%_37first48089 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54175)
store volatile %struct.ScmObj* %_37first48089, %struct.ScmObj** %stackaddr$prim55000, align 8
%stackaddr$makeclosure55001 = alloca %struct.ScmObj*, align 8
%fptrToInt55002 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51740 to i64
%ae51740 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55002)
store volatile %struct.ScmObj* %ae51740, %struct.ScmObj** %stackaddr$makeclosure55001, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51740, %struct.ScmObj* %_37_6248126, i64 0)
%ae51741 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55003 = alloca %struct.ScmObj*, align 8
%fptrToInt55004 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51742 to i64
%ae51742 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55004)
store volatile %struct.ScmObj* %ae51742, %struct.ScmObj** %stackaddr$makeclosure55003, align 8
%argslist54228$ae517400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55005 = alloca %struct.ScmObj*, align 8
%argslist54228$ae517401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51742, %struct.ScmObj* %argslist54228$ae517400)
store volatile %struct.ScmObj* %argslist54228$ae517401, %struct.ScmObj** %stackaddr$prim55005, align 8
%stackaddr$prim55006 = alloca %struct.ScmObj*, align 8
%argslist54228$ae517402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51741, %struct.ScmObj* %argslist54228$ae517401)
store volatile %struct.ScmObj* %argslist54228$ae517402, %struct.ScmObj** %stackaddr$prim55006, align 8
%clofunc55007 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51740)
musttail call tailcc void %clofunc55007(%struct.ScmObj* %ae51740, %struct.ScmObj* %argslist54228$ae517402)
ret void
}

define tailcc void @proc_clo$ae51740(%struct.ScmObj* %env$ae51740,%struct.ScmObj* %current_45args54177) {
%stackaddr$env-ref55008 = alloca %struct.ScmObj*, align 8
%_37_6248126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51740, i64 0)
store %struct.ScmObj* %_37_6248126, %struct.ScmObj** %stackaddr$env-ref55008
%stackaddr$prim55009 = alloca %struct.ScmObj*, align 8
%_95k48297 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54177)
store volatile %struct.ScmObj* %_95k48297, %struct.ScmObj** %stackaddr$prim55009, align 8
%stackaddr$prim55010 = alloca %struct.ScmObj*, align 8
%current_45args54178 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54177)
store volatile %struct.ScmObj* %current_45args54178, %struct.ScmObj** %stackaddr$prim55010, align 8
%stackaddr$prim55011 = alloca %struct.ScmObj*, align 8
%_37second48087 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54178)
store volatile %struct.ScmObj* %_37second48087, %struct.ScmObj** %stackaddr$prim55011, align 8
%stackaddr$makeclosure55012 = alloca %struct.ScmObj*, align 8
%fptrToInt55013 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51762 to i64
%ae51762 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55013)
store volatile %struct.ScmObj* %ae51762, %struct.ScmObj** %stackaddr$makeclosure55012, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51762, %struct.ScmObj* %_37_6248126, i64 0)
%ae51763 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55014 = alloca %struct.ScmObj*, align 8
%fptrToInt55015 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51764 to i64
%ae51764 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55015)
store volatile %struct.ScmObj* %ae51764, %struct.ScmObj** %stackaddr$makeclosure55014, align 8
%argslist54223$ae517620 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55016 = alloca %struct.ScmObj*, align 8
%argslist54223$ae517621 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51764, %struct.ScmObj* %argslist54223$ae517620)
store volatile %struct.ScmObj* %argslist54223$ae517621, %struct.ScmObj** %stackaddr$prim55016, align 8
%stackaddr$prim55017 = alloca %struct.ScmObj*, align 8
%argslist54223$ae517622 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51763, %struct.ScmObj* %argslist54223$ae517621)
store volatile %struct.ScmObj* %argslist54223$ae517622, %struct.ScmObj** %stackaddr$prim55017, align 8
%clofunc55018 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51762)
musttail call tailcc void %clofunc55018(%struct.ScmObj* %ae51762, %struct.ScmObj* %argslist54223$ae517622)
ret void
}

define tailcc void @proc_clo$ae51762(%struct.ScmObj* %env$ae51762,%struct.ScmObj* %current_45args54180) {
%stackaddr$env-ref55019 = alloca %struct.ScmObj*, align 8
%_37_6248126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51762, i64 0)
store %struct.ScmObj* %_37_6248126, %struct.ScmObj** %stackaddr$env-ref55019
%stackaddr$prim55020 = alloca %struct.ScmObj*, align 8
%_95k48298 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54180)
store volatile %struct.ScmObj* %_95k48298, %struct.ScmObj** %stackaddr$prim55020, align 8
%stackaddr$prim55021 = alloca %struct.ScmObj*, align 8
%current_45args54181 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54180)
store volatile %struct.ScmObj* %current_45args54181, %struct.ScmObj** %stackaddr$prim55021, align 8
%stackaddr$prim55022 = alloca %struct.ScmObj*, align 8
%_37third48085 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54181)
store volatile %struct.ScmObj* %_37third48085, %struct.ScmObj** %stackaddr$prim55022, align 8
%stackaddr$makeclosure55023 = alloca %struct.ScmObj*, align 8
%fptrToInt55024 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51786 to i64
%ae51786 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55024)
store volatile %struct.ScmObj* %ae51786, %struct.ScmObj** %stackaddr$makeclosure55023, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51786, %struct.ScmObj* %_37_6248126, i64 0)
%ae51787 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55025 = alloca %struct.ScmObj*, align 8
%fptrToInt55026 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51788 to i64
%ae51788 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55026)
store volatile %struct.ScmObj* %ae51788, %struct.ScmObj** %stackaddr$makeclosure55025, align 8
%argslist54218$ae517860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55027 = alloca %struct.ScmObj*, align 8
%argslist54218$ae517861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51788, %struct.ScmObj* %argslist54218$ae517860)
store volatile %struct.ScmObj* %argslist54218$ae517861, %struct.ScmObj** %stackaddr$prim55027, align 8
%stackaddr$prim55028 = alloca %struct.ScmObj*, align 8
%argslist54218$ae517862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51787, %struct.ScmObj* %argslist54218$ae517861)
store volatile %struct.ScmObj* %argslist54218$ae517862, %struct.ScmObj** %stackaddr$prim55028, align 8
%clofunc55029 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51786)
musttail call tailcc void %clofunc55029(%struct.ScmObj* %ae51786, %struct.ScmObj* %argslist54218$ae517862)
ret void
}

define tailcc void @proc_clo$ae51786(%struct.ScmObj* %env$ae51786,%struct.ScmObj* %current_45args54183) {
%stackaddr$env-ref55030 = alloca %struct.ScmObj*, align 8
%_37_6248126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51786, i64 0)
store %struct.ScmObj* %_37_6248126, %struct.ScmObj** %stackaddr$env-ref55030
%stackaddr$prim55031 = alloca %struct.ScmObj*, align 8
%_95k48299 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54183)
store volatile %struct.ScmObj* %_95k48299, %struct.ScmObj** %stackaddr$prim55031, align 8
%stackaddr$prim55032 = alloca %struct.ScmObj*, align 8
%current_45args54184 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54183)
store volatile %struct.ScmObj* %current_45args54184, %struct.ScmObj** %stackaddr$prim55032, align 8
%stackaddr$prim55033 = alloca %struct.ScmObj*, align 8
%_37fourth48083 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54184)
store volatile %struct.ScmObj* %_37fourth48083, %struct.ScmObj** %stackaddr$prim55033, align 8
%stackaddr$prim55034 = alloca %struct.ScmObj*, align 8
%anf_45bind48260 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind48260, %struct.ScmObj** %stackaddr$prim55034, align 8
%ae51812 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55035 = alloca %struct.ScmObj*, align 8
%loop48144 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51812, %struct.ScmObj* %anf_45bind48260)
store volatile %struct.ScmObj* %loop48144, %struct.ScmObj** %stackaddr$prim55035, align 8
%stackaddr$makeclosure55036 = alloca %struct.ScmObj*, align 8
%fptrToInt55037 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51814 to i64
%ae51814 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55037)
store volatile %struct.ScmObj* %ae51814, %struct.ScmObj** %stackaddr$makeclosure55036, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51814, %struct.ScmObj* %loop48144, i64 0)
%ae51815 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55038 = alloca %struct.ScmObj*, align 8
%fptrToInt55039 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51816 to i64
%ae51816 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55039)
store volatile %struct.ScmObj* %ae51816, %struct.ScmObj** %stackaddr$makeclosure55038, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51816, %struct.ScmObj* %loop48144, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51816, %struct.ScmObj* %_37_6248126, i64 1)
%argslist54213$ae518140 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55040 = alloca %struct.ScmObj*, align 8
%argslist54213$ae518141 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51816, %struct.ScmObj* %argslist54213$ae518140)
store volatile %struct.ScmObj* %argslist54213$ae518141, %struct.ScmObj** %stackaddr$prim55040, align 8
%stackaddr$prim55041 = alloca %struct.ScmObj*, align 8
%argslist54213$ae518142 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51815, %struct.ScmObj* %argslist54213$ae518141)
store volatile %struct.ScmObj* %argslist54213$ae518142, %struct.ScmObj** %stackaddr$prim55041, align 8
%clofunc55042 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51814)
musttail call tailcc void %clofunc55042(%struct.ScmObj* %ae51814, %struct.ScmObj* %argslist54213$ae518142)
ret void
}

define tailcc void @proc_clo$ae51814(%struct.ScmObj* %env$ae51814,%struct.ScmObj* %current_45args54186) {
%stackaddr$env-ref55043 = alloca %struct.ScmObj*, align 8
%loop48144 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51814, i64 0)
store %struct.ScmObj* %loop48144, %struct.ScmObj** %stackaddr$env-ref55043
%stackaddr$prim55044 = alloca %struct.ScmObj*, align 8
%_95k48300 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54186)
store volatile %struct.ScmObj* %_95k48300, %struct.ScmObj** %stackaddr$prim55044, align 8
%stackaddr$prim55045 = alloca %struct.ScmObj*, align 8
%current_45args54187 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54186)
store volatile %struct.ScmObj* %current_45args54187, %struct.ScmObj** %stackaddr$prim55045, align 8
%stackaddr$prim55046 = alloca %struct.ScmObj*, align 8
%anf_45bind48266 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54187)
store volatile %struct.ScmObj* %anf_45bind48266, %struct.ScmObj** %stackaddr$prim55046, align 8
%ae51930 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55047 = alloca %struct.ScmObj*, align 8
%t4802548145 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %loop48144, %struct.ScmObj* %ae51930, %struct.ScmObj* %anf_45bind48266)
store volatile %struct.ScmObj* %t4802548145, %struct.ScmObj** %stackaddr$prim55047, align 8
%ae51933 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55048 = alloca %struct.ScmObj*, align 8
%anf_45bind48267 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %loop48144, %struct.ScmObj* %ae51933)
store volatile %struct.ScmObj* %anf_45bind48267, %struct.ScmObj** %stackaddr$prim55048, align 8
%stackaddr$makeclosure55049 = alloca %struct.ScmObj*, align 8
%fptrToInt55050 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51935 to i64
%ae51935 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55050)
store volatile %struct.ScmObj* %ae51935, %struct.ScmObj** %stackaddr$makeclosure55049, align 8
%ae51936 = call %struct.ScmObj* @const_init_int(i64 1)
%ae51937 = call %struct.ScmObj* @const_init_int(i64 2)
%argslist54193$anf_45bind482670 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55051 = alloca %struct.ScmObj*, align 8
%argslist54193$anf_45bind482671 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51937, %struct.ScmObj* %argslist54193$anf_45bind482670)
store volatile %struct.ScmObj* %argslist54193$anf_45bind482671, %struct.ScmObj** %stackaddr$prim55051, align 8
%stackaddr$prim55052 = alloca %struct.ScmObj*, align 8
%argslist54193$anf_45bind482672 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51936, %struct.ScmObj* %argslist54193$anf_45bind482671)
store volatile %struct.ScmObj* %argslist54193$anf_45bind482672, %struct.ScmObj** %stackaddr$prim55052, align 8
%stackaddr$prim55053 = alloca %struct.ScmObj*, align 8
%argslist54193$anf_45bind482673 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51935, %struct.ScmObj* %argslist54193$anf_45bind482672)
store volatile %struct.ScmObj* %argslist54193$anf_45bind482673, %struct.ScmObj** %stackaddr$prim55053, align 8
%clofunc55054 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48267)
musttail call tailcc void %clofunc55054(%struct.ScmObj* %anf_45bind48267, %struct.ScmObj* %argslist54193$anf_45bind482673)
ret void
}

define tailcc void @proc_clo$ae51935(%struct.ScmObj* %env$ae51935,%struct.ScmObj* %current_45args54189) {
%stackaddr$prim55055 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54189)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim55055, align 8
%stackaddr$prim55056 = alloca %struct.ScmObj*, align 8
%current_45args54190 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54189)
store volatile %struct.ScmObj* %current_45args54190, %struct.ScmObj** %stackaddr$prim55056, align 8
%stackaddr$prim55057 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54190)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim55057, align 8
%stackaddr$prim55058 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim55058, align 8
%argslist54192$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55059 = alloca %struct.ScmObj*, align 8
%argslist54192$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist54192$k0)
store volatile %struct.ScmObj* %argslist54192$k1, %struct.ScmObj** %stackaddr$prim55059, align 8
%clofunc55060 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc55060(%struct.ScmObj* %k, %struct.ScmObj* %argslist54192$k1)
ret void
}

define tailcc void @proc_clo$ae51816(%struct.ScmObj* %env$ae51816,%struct.ScmObj* %current_45args54194) {
%stackaddr$env-ref55061 = alloca %struct.ScmObj*, align 8
%loop48144 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51816, i64 0)
store %struct.ScmObj* %loop48144, %struct.ScmObj** %stackaddr$env-ref55061
%stackaddr$env-ref55062 = alloca %struct.ScmObj*, align 8
%_37_6248126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51816, i64 1)
store %struct.ScmObj* %_37_6248126, %struct.ScmObj** %stackaddr$env-ref55062
%stackaddr$prim55063 = alloca %struct.ScmObj*, align 8
%k48301 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54194)
store volatile %struct.ScmObj* %k48301, %struct.ScmObj** %stackaddr$prim55063, align 8
%stackaddr$prim55064 = alloca %struct.ScmObj*, align 8
%current_45args54195 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54194)
store volatile %struct.ScmObj* %current_45args54195, %struct.ScmObj** %stackaddr$prim55064, align 8
%stackaddr$prim55065 = alloca %struct.ScmObj*, align 8
%m48147 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54195)
store volatile %struct.ScmObj* %m48147, %struct.ScmObj** %stackaddr$prim55065, align 8
%stackaddr$prim55066 = alloca %struct.ScmObj*, align 8
%current_45args54196 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54195)
store volatile %struct.ScmObj* %current_45args54196, %struct.ScmObj** %stackaddr$prim55066, align 8
%stackaddr$prim55067 = alloca %struct.ScmObj*, align 8
%n48146 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54196)
store volatile %struct.ScmObj* %n48146, %struct.ScmObj** %stackaddr$prim55067, align 8
%stackaddr$makeclosure55068 = alloca %struct.ScmObj*, align 8
%fptrToInt55069 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51818 to i64
%ae51818 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55069)
store volatile %struct.ScmObj* %ae51818, %struct.ScmObj** %stackaddr$makeclosure55068, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51818, %struct.ScmObj* %m48147, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51818, %struct.ScmObj* %n48146, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51818, %struct.ScmObj* %loop48144, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51818, %struct.ScmObj* %k48301, i64 3)
%ae51820 = call %struct.ScmObj* @const_init_int(i64 1000)
%argslist54212$_37_62481260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55070 = alloca %struct.ScmObj*, align 8
%argslist54212$_37_62481261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51820, %struct.ScmObj* %argslist54212$_37_62481260)
store volatile %struct.ScmObj* %argslist54212$_37_62481261, %struct.ScmObj** %stackaddr$prim55070, align 8
%stackaddr$prim55071 = alloca %struct.ScmObj*, align 8
%argslist54212$_37_62481262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %m48147, %struct.ScmObj* %argslist54212$_37_62481261)
store volatile %struct.ScmObj* %argslist54212$_37_62481262, %struct.ScmObj** %stackaddr$prim55071, align 8
%stackaddr$prim55072 = alloca %struct.ScmObj*, align 8
%argslist54212$_37_62481263 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51818, %struct.ScmObj* %argslist54212$_37_62481262)
store volatile %struct.ScmObj* %argslist54212$_37_62481263, %struct.ScmObj** %stackaddr$prim55072, align 8
%clofunc55073 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37_6248126)
musttail call tailcc void %clofunc55073(%struct.ScmObj* %_37_6248126, %struct.ScmObj* %argslist54212$_37_62481263)
ret void
}

define tailcc void @proc_clo$ae51818(%struct.ScmObj* %env$ae51818,%struct.ScmObj* %current_45args54198) {
%stackaddr$env-ref55074 = alloca %struct.ScmObj*, align 8
%m48147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51818, i64 0)
store %struct.ScmObj* %m48147, %struct.ScmObj** %stackaddr$env-ref55074
%stackaddr$env-ref55075 = alloca %struct.ScmObj*, align 8
%n48146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51818, i64 1)
store %struct.ScmObj* %n48146, %struct.ScmObj** %stackaddr$env-ref55075
%stackaddr$env-ref55076 = alloca %struct.ScmObj*, align 8
%loop48144 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51818, i64 2)
store %struct.ScmObj* %loop48144, %struct.ScmObj** %stackaddr$env-ref55076
%stackaddr$env-ref55077 = alloca %struct.ScmObj*, align 8
%k48301 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51818, i64 3)
store %struct.ScmObj* %k48301, %struct.ScmObj** %stackaddr$env-ref55077
%stackaddr$prim55078 = alloca %struct.ScmObj*, align 8
%_95k48302 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54198)
store volatile %struct.ScmObj* %_95k48302, %struct.ScmObj** %stackaddr$prim55078, align 8
%stackaddr$prim55079 = alloca %struct.ScmObj*, align 8
%current_45args54199 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54198)
store volatile %struct.ScmObj* %current_45args54199, %struct.ScmObj** %stackaddr$prim55079, align 8
%stackaddr$prim55080 = alloca %struct.ScmObj*, align 8
%anf_45bind48261 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54199)
store volatile %struct.ScmObj* %anf_45bind48261, %struct.ScmObj** %stackaddr$prim55080, align 8
%truthy$cmp55081 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48261)
%cmp$cmp55081 = icmp eq i64 %truthy$cmp55081, 1
br i1 %cmp$cmp55081, label %truebranch$cmp55081, label %falsebranch$cmp55081
truebranch$cmp55081:
%stackaddr$makeclosure55082 = alloca %struct.ScmObj*, align 8
%fptrToInt55083 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51826 to i64
%ae51826 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55083)
store volatile %struct.ScmObj* %ae51826, %struct.ScmObj** %stackaddr$makeclosure55082, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51826, %struct.ScmObj* %k48301, i64 0)
%ae51827 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55084 = alloca %struct.ScmObj*, align 8
%fptrToInt55085 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51828 to i64
%ae51828 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55085)
store volatile %struct.ScmObj* %ae51828, %struct.ScmObj** %stackaddr$makeclosure55084, align 8
%argslist54206$ae518260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55086 = alloca %struct.ScmObj*, align 8
%argslist54206$ae518261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51828, %struct.ScmObj* %argslist54206$ae518260)
store volatile %struct.ScmObj* %argslist54206$ae518261, %struct.ScmObj** %stackaddr$prim55086, align 8
%stackaddr$prim55087 = alloca %struct.ScmObj*, align 8
%argslist54206$ae518262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51827, %struct.ScmObj* %argslist54206$ae518261)
store volatile %struct.ScmObj* %argslist54206$ae518262, %struct.ScmObj** %stackaddr$prim55087, align 8
%clofunc55088 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51826)
musttail call tailcc void %clofunc55088(%struct.ScmObj* %ae51826, %struct.ScmObj* %argslist54206$ae518262)
ret void
falsebranch$cmp55081:
%ae51859 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55089 = alloca %struct.ScmObj*, align 8
%anf_45bind48263 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %loop48144, %struct.ScmObj* %ae51859)
store volatile %struct.ScmObj* %anf_45bind48263, %struct.ScmObj** %stackaddr$prim55089, align 8
%stackaddr$prim55090 = alloca %struct.ScmObj*, align 8
%anf_45bind48264 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %m48147, %struct.ScmObj* %n48146)
store volatile %struct.ScmObj* %anf_45bind48264, %struct.ScmObj** %stackaddr$prim55090, align 8
%stackaddr$makeclosure55091 = alloca %struct.ScmObj*, align 8
%fptrToInt55092 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51863 to i64
%ae51863 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55092)
store volatile %struct.ScmObj* %ae51863, %struct.ScmObj** %stackaddr$makeclosure55091, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51863, %struct.ScmObj* %m48147, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51863, %struct.ScmObj* %k48301, i64 1)
%argslist54211$anf_45bind482630 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55093 = alloca %struct.ScmObj*, align 8
%argslist54211$anf_45bind482631 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48264, %struct.ScmObj* %argslist54211$anf_45bind482630)
store volatile %struct.ScmObj* %argslist54211$anf_45bind482631, %struct.ScmObj** %stackaddr$prim55093, align 8
%stackaddr$prim55094 = alloca %struct.ScmObj*, align 8
%argslist54211$anf_45bind482632 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %n48146, %struct.ScmObj* %argslist54211$anf_45bind482631)
store volatile %struct.ScmObj* %argslist54211$anf_45bind482632, %struct.ScmObj** %stackaddr$prim55094, align 8
%stackaddr$prim55095 = alloca %struct.ScmObj*, align 8
%argslist54211$anf_45bind482633 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51863, %struct.ScmObj* %argslist54211$anf_45bind482632)
store volatile %struct.ScmObj* %argslist54211$anf_45bind482633, %struct.ScmObj** %stackaddr$prim55095, align 8
%clofunc55096 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48263)
musttail call tailcc void %clofunc55096(%struct.ScmObj* %anf_45bind48263, %struct.ScmObj* %argslist54211$anf_45bind482633)
ret void
}

define tailcc void @proc_clo$ae51826(%struct.ScmObj* %env$ae51826,%struct.ScmObj* %current_45args54201) {
%stackaddr$env-ref55097 = alloca %struct.ScmObj*, align 8
%k48301 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51826, i64 0)
store %struct.ScmObj* %k48301, %struct.ScmObj** %stackaddr$env-ref55097
%stackaddr$prim55098 = alloca %struct.ScmObj*, align 8
%_95k48303 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54201)
store volatile %struct.ScmObj* %_95k48303, %struct.ScmObj** %stackaddr$prim55098, align 8
%stackaddr$prim55099 = alloca %struct.ScmObj*, align 8
%current_45args54202 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54201)
store volatile %struct.ScmObj* %current_45args54202, %struct.ScmObj** %stackaddr$prim55099, align 8
%stackaddr$prim55100 = alloca %struct.ScmObj*, align 8
%anf_45bind48262 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54202)
store volatile %struct.ScmObj* %anf_45bind48262, %struct.ScmObj** %stackaddr$prim55100, align 8
%argslist54204$anf_45bind482620 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55101 = alloca %struct.ScmObj*, align 8
%argslist54204$anf_45bind482621 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48301, %struct.ScmObj* %argslist54204$anf_45bind482620)
store volatile %struct.ScmObj* %argslist54204$anf_45bind482621, %struct.ScmObj** %stackaddr$prim55101, align 8
%clofunc55102 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48262)
musttail call tailcc void %clofunc55102(%struct.ScmObj* %anf_45bind48262, %struct.ScmObj* %argslist54204$anf_45bind482621)
ret void
}

define tailcc void @proc_clo$ae51828(%struct.ScmObj* %env$ae51828,%struct.ScmObj* %lst4814848304) {
%stackaddr$prim55103 = alloca %struct.ScmObj*, align 8
%k48305 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4814848304)
store volatile %struct.ScmObj* %k48305, %struct.ScmObj** %stackaddr$prim55103, align 8
%stackaddr$prim55104 = alloca %struct.ScmObj*, align 8
%lst48148 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4814848304)
store volatile %struct.ScmObj* %lst48148, %struct.ScmObj** %stackaddr$prim55104, align 8
%ae51832 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54205$k483050 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55105 = alloca %struct.ScmObj*, align 8
%argslist54205$k483051 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48148, %struct.ScmObj* %argslist54205$k483050)
store volatile %struct.ScmObj* %argslist54205$k483051, %struct.ScmObj** %stackaddr$prim55105, align 8
%stackaddr$prim55106 = alloca %struct.ScmObj*, align 8
%argslist54205$k483052 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51832, %struct.ScmObj* %argslist54205$k483051)
store volatile %struct.ScmObj* %argslist54205$k483052, %struct.ScmObj** %stackaddr$prim55106, align 8
%clofunc55107 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48305)
musttail call tailcc void %clofunc55107(%struct.ScmObj* %k48305, %struct.ScmObj* %argslist54205$k483052)
ret void
}

define tailcc void @proc_clo$ae51863(%struct.ScmObj* %env$ae51863,%struct.ScmObj* %current_45args54207) {
%stackaddr$env-ref55108 = alloca %struct.ScmObj*, align 8
%m48147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51863, i64 0)
store %struct.ScmObj* %m48147, %struct.ScmObj** %stackaddr$env-ref55108
%stackaddr$env-ref55109 = alloca %struct.ScmObj*, align 8
%k48301 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51863, i64 1)
store %struct.ScmObj* %k48301, %struct.ScmObj** %stackaddr$env-ref55109
%stackaddr$prim55110 = alloca %struct.ScmObj*, align 8
%_95k48306 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54207)
store volatile %struct.ScmObj* %_95k48306, %struct.ScmObj** %stackaddr$prim55110, align 8
%stackaddr$prim55111 = alloca %struct.ScmObj*, align 8
%current_45args54208 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54207)
store volatile %struct.ScmObj* %current_45args54208, %struct.ScmObj** %stackaddr$prim55111, align 8
%stackaddr$prim55112 = alloca %struct.ScmObj*, align 8
%anf_45bind48265 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54208)
store volatile %struct.ScmObj* %anf_45bind48265, %struct.ScmObj** %stackaddr$prim55112, align 8
%stackaddr$prim55113 = alloca %struct.ScmObj*, align 8
%cpsprim48307 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %m48147, %struct.ScmObj* %anf_45bind48265)
store volatile %struct.ScmObj* %cpsprim48307, %struct.ScmObj** %stackaddr$prim55113, align 8
%ae51869 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54210$k483010 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55114 = alloca %struct.ScmObj*, align 8
%argslist54210$k483011 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48307, %struct.ScmObj* %argslist54210$k483010)
store volatile %struct.ScmObj* %argslist54210$k483011, %struct.ScmObj** %stackaddr$prim55114, align 8
%stackaddr$prim55115 = alloca %struct.ScmObj*, align 8
%argslist54210$k483012 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51869, %struct.ScmObj* %argslist54210$k483011)
store volatile %struct.ScmObj* %argslist54210$k483012, %struct.ScmObj** %stackaddr$prim55115, align 8
%clofunc55116 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48301)
musttail call tailcc void %clofunc55116(%struct.ScmObj* %k48301, %struct.ScmObj* %argslist54210$k483012)
ret void
}

define tailcc void @proc_clo$ae51788(%struct.ScmObj* %env$ae51788,%struct.ScmObj* %current_45args54214) {
%stackaddr$prim55117 = alloca %struct.ScmObj*, align 8
%k48308 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54214)
store volatile %struct.ScmObj* %k48308, %struct.ScmObj** %stackaddr$prim55117, align 8
%stackaddr$prim55118 = alloca %struct.ScmObj*, align 8
%current_45args54215 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54214)
store volatile %struct.ScmObj* %current_45args54215, %struct.ScmObj** %stackaddr$prim55118, align 8
%stackaddr$prim55119 = alloca %struct.ScmObj*, align 8
%x48084 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54215)
store volatile %struct.ScmObj* %x48084, %struct.ScmObj** %stackaddr$prim55119, align 8
%stackaddr$prim55120 = alloca %struct.ScmObj*, align 8
%anf_45bind48257 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48084)
store volatile %struct.ScmObj* %anf_45bind48257, %struct.ScmObj** %stackaddr$prim55120, align 8
%stackaddr$prim55121 = alloca %struct.ScmObj*, align 8
%anf_45bind48258 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48257)
store volatile %struct.ScmObj* %anf_45bind48258, %struct.ScmObj** %stackaddr$prim55121, align 8
%stackaddr$prim55122 = alloca %struct.ScmObj*, align 8
%anf_45bind48259 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48258)
store volatile %struct.ScmObj* %anf_45bind48259, %struct.ScmObj** %stackaddr$prim55122, align 8
%stackaddr$prim55123 = alloca %struct.ScmObj*, align 8
%cpsprim48309 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48259)
store volatile %struct.ScmObj* %cpsprim48309, %struct.ScmObj** %stackaddr$prim55123, align 8
%ae51794 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54217$k483080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55124 = alloca %struct.ScmObj*, align 8
%argslist54217$k483081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48309, %struct.ScmObj* %argslist54217$k483080)
store volatile %struct.ScmObj* %argslist54217$k483081, %struct.ScmObj** %stackaddr$prim55124, align 8
%stackaddr$prim55125 = alloca %struct.ScmObj*, align 8
%argslist54217$k483082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51794, %struct.ScmObj* %argslist54217$k483081)
store volatile %struct.ScmObj* %argslist54217$k483082, %struct.ScmObj** %stackaddr$prim55125, align 8
%clofunc55126 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48308)
musttail call tailcc void %clofunc55126(%struct.ScmObj* %k48308, %struct.ScmObj* %argslist54217$k483082)
ret void
}

define tailcc void @proc_clo$ae51764(%struct.ScmObj* %env$ae51764,%struct.ScmObj* %current_45args54219) {
%stackaddr$prim55127 = alloca %struct.ScmObj*, align 8
%k48310 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54219)
store volatile %struct.ScmObj* %k48310, %struct.ScmObj** %stackaddr$prim55127, align 8
%stackaddr$prim55128 = alloca %struct.ScmObj*, align 8
%current_45args54220 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54219)
store volatile %struct.ScmObj* %current_45args54220, %struct.ScmObj** %stackaddr$prim55128, align 8
%stackaddr$prim55129 = alloca %struct.ScmObj*, align 8
%x48086 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54220)
store volatile %struct.ScmObj* %x48086, %struct.ScmObj** %stackaddr$prim55129, align 8
%stackaddr$prim55130 = alloca %struct.ScmObj*, align 8
%anf_45bind48255 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48086)
store volatile %struct.ScmObj* %anf_45bind48255, %struct.ScmObj** %stackaddr$prim55130, align 8
%stackaddr$prim55131 = alloca %struct.ScmObj*, align 8
%anf_45bind48256 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48255)
store volatile %struct.ScmObj* %anf_45bind48256, %struct.ScmObj** %stackaddr$prim55131, align 8
%stackaddr$prim55132 = alloca %struct.ScmObj*, align 8
%cpsprim48311 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48256)
store volatile %struct.ScmObj* %cpsprim48311, %struct.ScmObj** %stackaddr$prim55132, align 8
%ae51769 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54222$k483100 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55133 = alloca %struct.ScmObj*, align 8
%argslist54222$k483101 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48311, %struct.ScmObj* %argslist54222$k483100)
store volatile %struct.ScmObj* %argslist54222$k483101, %struct.ScmObj** %stackaddr$prim55133, align 8
%stackaddr$prim55134 = alloca %struct.ScmObj*, align 8
%argslist54222$k483102 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51769, %struct.ScmObj* %argslist54222$k483101)
store volatile %struct.ScmObj* %argslist54222$k483102, %struct.ScmObj** %stackaddr$prim55134, align 8
%clofunc55135 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48310)
musttail call tailcc void %clofunc55135(%struct.ScmObj* %k48310, %struct.ScmObj* %argslist54222$k483102)
ret void
}

define tailcc void @proc_clo$ae51742(%struct.ScmObj* %env$ae51742,%struct.ScmObj* %current_45args54224) {
%stackaddr$prim55136 = alloca %struct.ScmObj*, align 8
%k48312 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54224)
store volatile %struct.ScmObj* %k48312, %struct.ScmObj** %stackaddr$prim55136, align 8
%stackaddr$prim55137 = alloca %struct.ScmObj*, align 8
%current_45args54225 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54224)
store volatile %struct.ScmObj* %current_45args54225, %struct.ScmObj** %stackaddr$prim55137, align 8
%stackaddr$prim55138 = alloca %struct.ScmObj*, align 8
%x48088 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54225)
store volatile %struct.ScmObj* %x48088, %struct.ScmObj** %stackaddr$prim55138, align 8
%stackaddr$prim55139 = alloca %struct.ScmObj*, align 8
%anf_45bind48254 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48088)
store volatile %struct.ScmObj* %anf_45bind48254, %struct.ScmObj** %stackaddr$prim55139, align 8
%stackaddr$prim55140 = alloca %struct.ScmObj*, align 8
%cpsprim48313 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48254)
store volatile %struct.ScmObj* %cpsprim48313, %struct.ScmObj** %stackaddr$prim55140, align 8
%ae51746 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54227$k483120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55141 = alloca %struct.ScmObj*, align 8
%argslist54227$k483121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48313, %struct.ScmObj* %argslist54227$k483120)
store volatile %struct.ScmObj* %argslist54227$k483121, %struct.ScmObj** %stackaddr$prim55141, align 8
%stackaddr$prim55142 = alloca %struct.ScmObj*, align 8
%argslist54227$k483122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51746, %struct.ScmObj* %argslist54227$k483121)
store volatile %struct.ScmObj* %argslist54227$k483122, %struct.ScmObj** %stackaddr$prim55142, align 8
%clofunc55143 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48312)
musttail call tailcc void %clofunc55143(%struct.ScmObj* %k48312, %struct.ScmObj* %argslist54227$k483122)
ret void
}

define tailcc void @proc_clo$ae51722(%struct.ScmObj* %env$ae51722,%struct.ScmObj* %current_45args54229) {
%stackaddr$prim55144 = alloca %struct.ScmObj*, align 8
%k48314 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54229)
store volatile %struct.ScmObj* %k48314, %struct.ScmObj** %stackaddr$prim55144, align 8
%stackaddr$prim55145 = alloca %struct.ScmObj*, align 8
%current_45args54230 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54229)
store volatile %struct.ScmObj* %current_45args54230, %struct.ScmObj** %stackaddr$prim55145, align 8
%stackaddr$prim55146 = alloca %struct.ScmObj*, align 8
%x48090 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54230)
store volatile %struct.ScmObj* %x48090, %struct.ScmObj** %stackaddr$prim55146, align 8
%stackaddr$prim55147 = alloca %struct.ScmObj*, align 8
%cpsprim48315 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48090)
store volatile %struct.ScmObj* %cpsprim48315, %struct.ScmObj** %stackaddr$prim55147, align 8
%ae51725 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54232$k483140 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55148 = alloca %struct.ScmObj*, align 8
%argslist54232$k483141 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48315, %struct.ScmObj* %argslist54232$k483140)
store volatile %struct.ScmObj* %argslist54232$k483141, %struct.ScmObj** %stackaddr$prim55148, align 8
%stackaddr$prim55149 = alloca %struct.ScmObj*, align 8
%argslist54232$k483142 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51725, %struct.ScmObj* %argslist54232$k483141)
store volatile %struct.ScmObj* %argslist54232$k483142, %struct.ScmObj** %stackaddr$prim55149, align 8
%clofunc55150 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48314)
musttail call tailcc void %clofunc55150(%struct.ScmObj* %k48314, %struct.ScmObj* %argslist54232$k483142)
ret void
}

define tailcc void @proc_clo$ae51624(%struct.ScmObj* %env$ae51624,%struct.ScmObj* %args4809248316) {
%stackaddr$env-ref55151 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51624, i64 0)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref55151
%stackaddr$prim55152 = alloca %struct.ScmObj*, align 8
%k48317 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4809248316)
store volatile %struct.ScmObj* %k48317, %struct.ScmObj** %stackaddr$prim55152, align 8
%stackaddr$prim55153 = alloca %struct.ScmObj*, align 8
%args48092 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4809248316)
store volatile %struct.ScmObj* %args48092, %struct.ScmObj** %stackaddr$prim55153, align 8
%stackaddr$prim55154 = alloca %struct.ScmObj*, align 8
%anf_45bind48248 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args48092)
store volatile %struct.ScmObj* %anf_45bind48248, %struct.ScmObj** %stackaddr$prim55154, align 8
%truthy$cmp55155 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48248)
%cmp$cmp55155 = icmp eq i64 %truthy$cmp55155, 1
br i1 %cmp$cmp55155, label %truebranch$cmp55155, label %falsebranch$cmp55155
truebranch$cmp55155:
%ae51630 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51631 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist54234$k483170 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55156 = alloca %struct.ScmObj*, align 8
%argslist54234$k483171 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51631, %struct.ScmObj* %argslist54234$k483170)
store volatile %struct.ScmObj* %argslist54234$k483171, %struct.ScmObj** %stackaddr$prim55156, align 8
%stackaddr$prim55157 = alloca %struct.ScmObj*, align 8
%argslist54234$k483172 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51630, %struct.ScmObj* %argslist54234$k483171)
store volatile %struct.ScmObj* %argslist54234$k483172, %struct.ScmObj** %stackaddr$prim55157, align 8
%clofunc55158 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48317)
musttail call tailcc void %clofunc55158(%struct.ScmObj* %k48317, %struct.ScmObj* %argslist54234$k483172)
ret void
falsebranch$cmp55155:
%stackaddr$prim55159 = alloca %struct.ScmObj*, align 8
%anf_45bind48249 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48092)
store volatile %struct.ScmObj* %anf_45bind48249, %struct.ScmObj** %stackaddr$prim55159, align 8
%stackaddr$prim55160 = alloca %struct.ScmObj*, align 8
%anf_45bind48250 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48249)
store volatile %struct.ScmObj* %anf_45bind48250, %struct.ScmObj** %stackaddr$prim55160, align 8
%truthy$cmp55161 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48250)
%cmp$cmp55161 = icmp eq i64 %truthy$cmp55161, 1
br i1 %cmp$cmp55161, label %truebranch$cmp55161, label %falsebranch$cmp55161
truebranch$cmp55161:
%stackaddr$prim55162 = alloca %struct.ScmObj*, align 8
%cpsprim48318 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48092)
store volatile %struct.ScmObj* %cpsprim48318, %struct.ScmObj** %stackaddr$prim55162, align 8
%ae51643 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54235$k483170 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55163 = alloca %struct.ScmObj*, align 8
%argslist54235$k483171 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48318, %struct.ScmObj* %argslist54235$k483170)
store volatile %struct.ScmObj* %argslist54235$k483171, %struct.ScmObj** %stackaddr$prim55163, align 8
%stackaddr$prim55164 = alloca %struct.ScmObj*, align 8
%argslist54235$k483172 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51643, %struct.ScmObj* %argslist54235$k483171)
store volatile %struct.ScmObj* %argslist54235$k483172, %struct.ScmObj** %stackaddr$prim55164, align 8
%clofunc55165 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48317)
musttail call tailcc void %clofunc55165(%struct.ScmObj* %k48317, %struct.ScmObj* %argslist54235$k483172)
ret void
falsebranch$cmp55161:
%stackaddr$makeclosure55166 = alloca %struct.ScmObj*, align 8
%fptrToInt55167 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51648 to i64
%ae51648 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55167)
store volatile %struct.ScmObj* %ae51648, %struct.ScmObj** %stackaddr$makeclosure55166, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51648, %struct.ScmObj* %_37foldl148031, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51648, %struct.ScmObj* %k48317, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51648, %struct.ScmObj* %args48092, i64 2)
%ae51649 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55168 = alloca %struct.ScmObj*, align 8
%fptrToInt55169 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51650 to i64
%ae51650 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55169)
store volatile %struct.ScmObj* %ae51650, %struct.ScmObj** %stackaddr$makeclosure55168, align 8
%argslist54245$ae516480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55170 = alloca %struct.ScmObj*, align 8
%argslist54245$ae516481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51650, %struct.ScmObj* %argslist54245$ae516480)
store volatile %struct.ScmObj* %argslist54245$ae516481, %struct.ScmObj** %stackaddr$prim55170, align 8
%stackaddr$prim55171 = alloca %struct.ScmObj*, align 8
%argslist54245$ae516482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51649, %struct.ScmObj* %argslist54245$ae516481)
store volatile %struct.ScmObj* %argslist54245$ae516482, %struct.ScmObj** %stackaddr$prim55171, align 8
%clofunc55172 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51648)
musttail call tailcc void %clofunc55172(%struct.ScmObj* %ae51648, %struct.ScmObj* %argslist54245$ae516482)
ret void
}

define tailcc void @proc_clo$ae51648(%struct.ScmObj* %env$ae51648,%struct.ScmObj* %current_45args54236) {
%stackaddr$env-ref55173 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51648, i64 0)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref55173
%stackaddr$env-ref55174 = alloca %struct.ScmObj*, align 8
%k48317 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51648, i64 1)
store %struct.ScmObj* %k48317, %struct.ScmObj** %stackaddr$env-ref55174
%stackaddr$env-ref55175 = alloca %struct.ScmObj*, align 8
%args48092 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51648, i64 2)
store %struct.ScmObj* %args48092, %struct.ScmObj** %stackaddr$env-ref55175
%stackaddr$prim55176 = alloca %struct.ScmObj*, align 8
%_95k48319 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54236)
store volatile %struct.ScmObj* %_95k48319, %struct.ScmObj** %stackaddr$prim55176, align 8
%stackaddr$prim55177 = alloca %struct.ScmObj*, align 8
%current_45args54237 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54236)
store volatile %struct.ScmObj* %current_45args54237, %struct.ScmObj** %stackaddr$prim55177, align 8
%stackaddr$prim55178 = alloca %struct.ScmObj*, align 8
%anf_45bind48251 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54237)
store volatile %struct.ScmObj* %anf_45bind48251, %struct.ScmObj** %stackaddr$prim55178, align 8
%stackaddr$prim55179 = alloca %struct.ScmObj*, align 8
%anf_45bind48252 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48092)
store volatile %struct.ScmObj* %anf_45bind48252, %struct.ScmObj** %stackaddr$prim55179, align 8
%stackaddr$prim55180 = alloca %struct.ScmObj*, align 8
%anf_45bind48253 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48092)
store volatile %struct.ScmObj* %anf_45bind48253, %struct.ScmObj** %stackaddr$prim55180, align 8
%argslist54239$_37foldl1480310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55181 = alloca %struct.ScmObj*, align 8
%argslist54239$_37foldl1480311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48253, %struct.ScmObj* %argslist54239$_37foldl1480310)
store volatile %struct.ScmObj* %argslist54239$_37foldl1480311, %struct.ScmObj** %stackaddr$prim55181, align 8
%stackaddr$prim55182 = alloca %struct.ScmObj*, align 8
%argslist54239$_37foldl1480312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48252, %struct.ScmObj* %argslist54239$_37foldl1480311)
store volatile %struct.ScmObj* %argslist54239$_37foldl1480312, %struct.ScmObj** %stackaddr$prim55182, align 8
%stackaddr$prim55183 = alloca %struct.ScmObj*, align 8
%argslist54239$_37foldl1480313 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48251, %struct.ScmObj* %argslist54239$_37foldl1480312)
store volatile %struct.ScmObj* %argslist54239$_37foldl1480313, %struct.ScmObj** %stackaddr$prim55183, align 8
%stackaddr$prim55184 = alloca %struct.ScmObj*, align 8
%argslist54239$_37foldl1480314 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48317, %struct.ScmObj* %argslist54239$_37foldl1480313)
store volatile %struct.ScmObj* %argslist54239$_37foldl1480314, %struct.ScmObj** %stackaddr$prim55184, align 8
%clofunc55185 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148031)
musttail call tailcc void %clofunc55185(%struct.ScmObj* %_37foldl148031, %struct.ScmObj* %argslist54239$_37foldl1480314)
ret void
}

define tailcc void @proc_clo$ae51650(%struct.ScmObj* %env$ae51650,%struct.ScmObj* %current_45args54240) {
%stackaddr$prim55186 = alloca %struct.ScmObj*, align 8
%k48320 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54240)
store volatile %struct.ScmObj* %k48320, %struct.ScmObj** %stackaddr$prim55186, align 8
%stackaddr$prim55187 = alloca %struct.ScmObj*, align 8
%current_45args54241 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54240)
store volatile %struct.ScmObj* %current_45args54241, %struct.ScmObj** %stackaddr$prim55187, align 8
%stackaddr$prim55188 = alloca %struct.ScmObj*, align 8
%n48094 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54241)
store volatile %struct.ScmObj* %n48094, %struct.ScmObj** %stackaddr$prim55188, align 8
%stackaddr$prim55189 = alloca %struct.ScmObj*, align 8
%current_45args54242 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54241)
store volatile %struct.ScmObj* %current_45args54242, %struct.ScmObj** %stackaddr$prim55189, align 8
%stackaddr$prim55190 = alloca %struct.ScmObj*, align 8
%v48093 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54242)
store volatile %struct.ScmObj* %v48093, %struct.ScmObj** %stackaddr$prim55190, align 8
%stackaddr$prim55191 = alloca %struct.ScmObj*, align 8
%cpsprim48321 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v48093, %struct.ScmObj* %n48094)
store volatile %struct.ScmObj* %cpsprim48321, %struct.ScmObj** %stackaddr$prim55191, align 8
%ae51654 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54244$k483200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55192 = alloca %struct.ScmObj*, align 8
%argslist54244$k483201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48321, %struct.ScmObj* %argslist54244$k483200)
store volatile %struct.ScmObj* %argslist54244$k483201, %struct.ScmObj** %stackaddr$prim55192, align 8
%stackaddr$prim55193 = alloca %struct.ScmObj*, align 8
%argslist54244$k483202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51654, %struct.ScmObj* %argslist54244$k483201)
store volatile %struct.ScmObj* %argslist54244$k483202, %struct.ScmObj** %stackaddr$prim55193, align 8
%clofunc55194 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48320)
musttail call tailcc void %clofunc55194(%struct.ScmObj* %k48320, %struct.ScmObj* %argslist54244$k483202)
ret void
}

define tailcc void @proc_clo$ae51220(%struct.ScmObj* %env$ae51220,%struct.ScmObj* %current_45args54247) {
%stackaddr$prim55195 = alloca %struct.ScmObj*, align 8
%k48322 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54247)
store volatile %struct.ScmObj* %k48322, %struct.ScmObj** %stackaddr$prim55195, align 8
%stackaddr$prim55196 = alloca %struct.ScmObj*, align 8
%current_45args54248 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54247)
store volatile %struct.ScmObj* %current_45args54248, %struct.ScmObj** %stackaddr$prim55196, align 8
%stackaddr$prim55197 = alloca %struct.ScmObj*, align 8
%v48097 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54248)
store volatile %struct.ScmObj* %v48097, %struct.ScmObj** %stackaddr$prim55197, align 8
%stackaddr$prim55198 = alloca %struct.ScmObj*, align 8
%current_45args54249 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54248)
store volatile %struct.ScmObj* %current_45args54249, %struct.ScmObj** %stackaddr$prim55198, align 8
%stackaddr$prim55199 = alloca %struct.ScmObj*, align 8
%lst48096 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54249)
store volatile %struct.ScmObj* %lst48096, %struct.ScmObj** %stackaddr$prim55199, align 8
%ae51221 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55200 = alloca %struct.ScmObj*, align 8
%lst48098 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51221, %struct.ScmObj* %lst48096)
store volatile %struct.ScmObj* %lst48098, %struct.ScmObj** %stackaddr$prim55200, align 8
%stackaddr$makeclosure55201 = alloca %struct.ScmObj*, align 8
%fptrToInt55202 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51223 to i64
%ae51223 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55202)
store volatile %struct.ScmObj* %ae51223, %struct.ScmObj** %stackaddr$makeclosure55201, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51223, %struct.ScmObj* %lst48098, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51223, %struct.ScmObj* %k48322, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51223, %struct.ScmObj* %v48097, i64 2)
%ae51224 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55203 = alloca %struct.ScmObj*, align 8
%fptrToInt55204 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51225 to i64
%ae51225 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55204)
store volatile %struct.ScmObj* %ae51225, %struct.ScmObj** %stackaddr$makeclosure55203, align 8
%argslist54271$ae512230 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55205 = alloca %struct.ScmObj*, align 8
%argslist54271$ae512231 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51225, %struct.ScmObj* %argslist54271$ae512230)
store volatile %struct.ScmObj* %argslist54271$ae512231, %struct.ScmObj** %stackaddr$prim55205, align 8
%stackaddr$prim55206 = alloca %struct.ScmObj*, align 8
%argslist54271$ae512232 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51224, %struct.ScmObj* %argslist54271$ae512231)
store volatile %struct.ScmObj* %argslist54271$ae512232, %struct.ScmObj** %stackaddr$prim55206, align 8
%clofunc55207 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51223)
musttail call tailcc void %clofunc55207(%struct.ScmObj* %ae51223, %struct.ScmObj* %argslist54271$ae512232)
ret void
}

define tailcc void @proc_clo$ae51223(%struct.ScmObj* %env$ae51223,%struct.ScmObj* %current_45args54251) {
%stackaddr$env-ref55208 = alloca %struct.ScmObj*, align 8
%lst48098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51223, i64 0)
store %struct.ScmObj* %lst48098, %struct.ScmObj** %stackaddr$env-ref55208
%stackaddr$env-ref55209 = alloca %struct.ScmObj*, align 8
%k48322 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51223, i64 1)
store %struct.ScmObj* %k48322, %struct.ScmObj** %stackaddr$env-ref55209
%stackaddr$env-ref55210 = alloca %struct.ScmObj*, align 8
%v48097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51223, i64 2)
store %struct.ScmObj* %v48097, %struct.ScmObj** %stackaddr$env-ref55210
%stackaddr$prim55211 = alloca %struct.ScmObj*, align 8
%_95k48323 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54251)
store volatile %struct.ScmObj* %_95k48323, %struct.ScmObj** %stackaddr$prim55211, align 8
%stackaddr$prim55212 = alloca %struct.ScmObj*, align 8
%current_45args54252 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54251)
store volatile %struct.ScmObj* %current_45args54252, %struct.ScmObj** %stackaddr$prim55212, align 8
%stackaddr$prim55213 = alloca %struct.ScmObj*, align 8
%anf_45bind48240 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54252)
store volatile %struct.ScmObj* %anf_45bind48240, %struct.ScmObj** %stackaddr$prim55213, align 8
%stackaddr$makeclosure55214 = alloca %struct.ScmObj*, align 8
%fptrToInt55215 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51239 to i64
%ae51239 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55215)
store volatile %struct.ScmObj* %ae51239, %struct.ScmObj** %stackaddr$makeclosure55214, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51239, %struct.ScmObj* %lst48098, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51239, %struct.ScmObj* %k48322, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51239, %struct.ScmObj* %v48097, i64 2)
%stackaddr$makeclosure55216 = alloca %struct.ScmObj*, align 8
%fptrToInt55217 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51240 to i64
%ae51240 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55217)
store volatile %struct.ScmObj* %ae51240, %struct.ScmObj** %stackaddr$makeclosure55216, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51240, %struct.ScmObj* %lst48098, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51240, %struct.ScmObj* %k48322, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51240, %struct.ScmObj* %v48097, i64 2)
%argslist54266$anf_45bind482400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55218 = alloca %struct.ScmObj*, align 8
%argslist54266$anf_45bind482401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51240, %struct.ScmObj* %argslist54266$anf_45bind482400)
store volatile %struct.ScmObj* %argslist54266$anf_45bind482401, %struct.ScmObj** %stackaddr$prim55218, align 8
%stackaddr$prim55219 = alloca %struct.ScmObj*, align 8
%argslist54266$anf_45bind482402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51239, %struct.ScmObj* %argslist54266$anf_45bind482401)
store volatile %struct.ScmObj* %argslist54266$anf_45bind482402, %struct.ScmObj** %stackaddr$prim55219, align 8
%clofunc55220 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48240)
musttail call tailcc void %clofunc55220(%struct.ScmObj* %anf_45bind48240, %struct.ScmObj* %argslist54266$anf_45bind482402)
ret void
}

define tailcc void @proc_clo$ae51239(%struct.ScmObj* %env$ae51239,%struct.ScmObj* %current_45args54254) {
%stackaddr$env-ref55221 = alloca %struct.ScmObj*, align 8
%lst48098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51239, i64 0)
store %struct.ScmObj* %lst48098, %struct.ScmObj** %stackaddr$env-ref55221
%stackaddr$env-ref55222 = alloca %struct.ScmObj*, align 8
%k48322 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51239, i64 1)
store %struct.ScmObj* %k48322, %struct.ScmObj** %stackaddr$env-ref55222
%stackaddr$env-ref55223 = alloca %struct.ScmObj*, align 8
%v48097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51239, i64 2)
store %struct.ScmObj* %v48097, %struct.ScmObj** %stackaddr$env-ref55223
%stackaddr$prim55224 = alloca %struct.ScmObj*, align 8
%_95k48324 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54254)
store volatile %struct.ScmObj* %_95k48324, %struct.ScmObj** %stackaddr$prim55224, align 8
%stackaddr$prim55225 = alloca %struct.ScmObj*, align 8
%current_45args54255 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54254)
store volatile %struct.ScmObj* %current_45args54255, %struct.ScmObj** %stackaddr$prim55225, align 8
%stackaddr$prim55226 = alloca %struct.ScmObj*, align 8
%cc48099 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54255)
store volatile %struct.ScmObj* %cc48099, %struct.ScmObj** %stackaddr$prim55226, align 8
%ae51348 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55227 = alloca %struct.ScmObj*, align 8
%anf_45bind48241 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48098, %struct.ScmObj* %ae51348)
store volatile %struct.ScmObj* %anf_45bind48241, %struct.ScmObj** %stackaddr$prim55227, align 8
%stackaddr$prim55228 = alloca %struct.ScmObj*, align 8
%anf_45bind48242 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48241)
store volatile %struct.ScmObj* %anf_45bind48242, %struct.ScmObj** %stackaddr$prim55228, align 8
%truthy$cmp55229 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48242)
%cmp$cmp55229 = icmp eq i64 %truthy$cmp55229, 1
br i1 %cmp$cmp55229, label %truebranch$cmp55229, label %falsebranch$cmp55229
truebranch$cmp55229:
%ae51352 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51353 = call %struct.ScmObj* @const_init_false()
%argslist54257$k483220 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55230 = alloca %struct.ScmObj*, align 8
%argslist54257$k483221 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51353, %struct.ScmObj* %argslist54257$k483220)
store volatile %struct.ScmObj* %argslist54257$k483221, %struct.ScmObj** %stackaddr$prim55230, align 8
%stackaddr$prim55231 = alloca %struct.ScmObj*, align 8
%argslist54257$k483222 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51352, %struct.ScmObj* %argslist54257$k483221)
store volatile %struct.ScmObj* %argslist54257$k483222, %struct.ScmObj** %stackaddr$prim55231, align 8
%clofunc55232 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48322)
musttail call tailcc void %clofunc55232(%struct.ScmObj* %k48322, %struct.ScmObj* %argslist54257$k483222)
ret void
falsebranch$cmp55229:
%ae51361 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55233 = alloca %struct.ScmObj*, align 8
%anf_45bind48243 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48098, %struct.ScmObj* %ae51361)
store volatile %struct.ScmObj* %anf_45bind48243, %struct.ScmObj** %stackaddr$prim55233, align 8
%stackaddr$prim55234 = alloca %struct.ScmObj*, align 8
%anf_45bind48244 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48243)
store volatile %struct.ScmObj* %anf_45bind48244, %struct.ScmObj** %stackaddr$prim55234, align 8
%stackaddr$prim55235 = alloca %struct.ScmObj*, align 8
%anf_45bind48245 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind48244, %struct.ScmObj* %v48097)
store volatile %struct.ScmObj* %anf_45bind48245, %struct.ScmObj** %stackaddr$prim55235, align 8
%truthy$cmp55236 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48245)
%cmp$cmp55236 = icmp eq i64 %truthy$cmp55236, 1
br i1 %cmp$cmp55236, label %truebranch$cmp55236, label %falsebranch$cmp55236
truebranch$cmp55236:
%ae51367 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55237 = alloca %struct.ScmObj*, align 8
%cpsprim48325 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48098, %struct.ScmObj* %ae51367)
store volatile %struct.ScmObj* %cpsprim48325, %struct.ScmObj** %stackaddr$prim55237, align 8
%ae51369 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54258$k483220 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55238 = alloca %struct.ScmObj*, align 8
%argslist54258$k483221 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48325, %struct.ScmObj* %argslist54258$k483220)
store volatile %struct.ScmObj* %argslist54258$k483221, %struct.ScmObj** %stackaddr$prim55238, align 8
%stackaddr$prim55239 = alloca %struct.ScmObj*, align 8
%argslist54258$k483222 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51369, %struct.ScmObj* %argslist54258$k483221)
store volatile %struct.ScmObj* %argslist54258$k483222, %struct.ScmObj** %stackaddr$prim55239, align 8
%clofunc55240 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48322)
musttail call tailcc void %clofunc55240(%struct.ScmObj* %k48322, %struct.ScmObj* %argslist54258$k483222)
ret void
falsebranch$cmp55236:
%ae51380 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55241 = alloca %struct.ScmObj*, align 8
%anf_45bind48246 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48098, %struct.ScmObj* %ae51380)
store volatile %struct.ScmObj* %anf_45bind48246, %struct.ScmObj** %stackaddr$prim55241, align 8
%stackaddr$prim55242 = alloca %struct.ScmObj*, align 8
%anf_45bind48247 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48246)
store volatile %struct.ScmObj* %anf_45bind48247, %struct.ScmObj** %stackaddr$prim55242, align 8
%ae51383 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55243 = alloca %struct.ScmObj*, align 8
%_95048101 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48098, %struct.ScmObj* %ae51383, %struct.ScmObj* %anf_45bind48247)
store volatile %struct.ScmObj* %_95048101, %struct.ScmObj** %stackaddr$prim55243, align 8
%argslist54259$cc480990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55244 = alloca %struct.ScmObj*, align 8
%argslist54259$cc480991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48099, %struct.ScmObj* %argslist54259$cc480990)
store volatile %struct.ScmObj* %argslist54259$cc480991, %struct.ScmObj** %stackaddr$prim55244, align 8
%stackaddr$prim55245 = alloca %struct.ScmObj*, align 8
%argslist54259$cc480992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48322, %struct.ScmObj* %argslist54259$cc480991)
store volatile %struct.ScmObj* %argslist54259$cc480992, %struct.ScmObj** %stackaddr$prim55245, align 8
%clofunc55246 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48099)
musttail call tailcc void %clofunc55246(%struct.ScmObj* %cc48099, %struct.ScmObj* %argslist54259$cc480992)
ret void
}

define tailcc void @proc_clo$ae51240(%struct.ScmObj* %env$ae51240,%struct.ScmObj* %current_45args54260) {
%stackaddr$env-ref55247 = alloca %struct.ScmObj*, align 8
%lst48098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51240, i64 0)
store %struct.ScmObj* %lst48098, %struct.ScmObj** %stackaddr$env-ref55247
%stackaddr$env-ref55248 = alloca %struct.ScmObj*, align 8
%k48322 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51240, i64 1)
store %struct.ScmObj* %k48322, %struct.ScmObj** %stackaddr$env-ref55248
%stackaddr$env-ref55249 = alloca %struct.ScmObj*, align 8
%v48097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51240, i64 2)
store %struct.ScmObj* %v48097, %struct.ScmObj** %stackaddr$env-ref55249
%stackaddr$prim55250 = alloca %struct.ScmObj*, align 8
%_95k48324 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54260)
store volatile %struct.ScmObj* %_95k48324, %struct.ScmObj** %stackaddr$prim55250, align 8
%stackaddr$prim55251 = alloca %struct.ScmObj*, align 8
%current_45args54261 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54260)
store volatile %struct.ScmObj* %current_45args54261, %struct.ScmObj** %stackaddr$prim55251, align 8
%stackaddr$prim55252 = alloca %struct.ScmObj*, align 8
%cc48099 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54261)
store volatile %struct.ScmObj* %cc48099, %struct.ScmObj** %stackaddr$prim55252, align 8
%ae51242 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55253 = alloca %struct.ScmObj*, align 8
%anf_45bind48241 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48098, %struct.ScmObj* %ae51242)
store volatile %struct.ScmObj* %anf_45bind48241, %struct.ScmObj** %stackaddr$prim55253, align 8
%stackaddr$prim55254 = alloca %struct.ScmObj*, align 8
%anf_45bind48242 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48241)
store volatile %struct.ScmObj* %anf_45bind48242, %struct.ScmObj** %stackaddr$prim55254, align 8
%truthy$cmp55255 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48242)
%cmp$cmp55255 = icmp eq i64 %truthy$cmp55255, 1
br i1 %cmp$cmp55255, label %truebranch$cmp55255, label %falsebranch$cmp55255
truebranch$cmp55255:
%ae51246 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51247 = call %struct.ScmObj* @const_init_false()
%argslist54263$k483220 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55256 = alloca %struct.ScmObj*, align 8
%argslist54263$k483221 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51247, %struct.ScmObj* %argslist54263$k483220)
store volatile %struct.ScmObj* %argslist54263$k483221, %struct.ScmObj** %stackaddr$prim55256, align 8
%stackaddr$prim55257 = alloca %struct.ScmObj*, align 8
%argslist54263$k483222 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51246, %struct.ScmObj* %argslist54263$k483221)
store volatile %struct.ScmObj* %argslist54263$k483222, %struct.ScmObj** %stackaddr$prim55257, align 8
%clofunc55258 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48322)
musttail call tailcc void %clofunc55258(%struct.ScmObj* %k48322, %struct.ScmObj* %argslist54263$k483222)
ret void
falsebranch$cmp55255:
%ae51255 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55259 = alloca %struct.ScmObj*, align 8
%anf_45bind48243 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48098, %struct.ScmObj* %ae51255)
store volatile %struct.ScmObj* %anf_45bind48243, %struct.ScmObj** %stackaddr$prim55259, align 8
%stackaddr$prim55260 = alloca %struct.ScmObj*, align 8
%anf_45bind48244 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48243)
store volatile %struct.ScmObj* %anf_45bind48244, %struct.ScmObj** %stackaddr$prim55260, align 8
%stackaddr$prim55261 = alloca %struct.ScmObj*, align 8
%anf_45bind48245 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind48244, %struct.ScmObj* %v48097)
store volatile %struct.ScmObj* %anf_45bind48245, %struct.ScmObj** %stackaddr$prim55261, align 8
%truthy$cmp55262 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48245)
%cmp$cmp55262 = icmp eq i64 %truthy$cmp55262, 1
br i1 %cmp$cmp55262, label %truebranch$cmp55262, label %falsebranch$cmp55262
truebranch$cmp55262:
%ae51261 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55263 = alloca %struct.ScmObj*, align 8
%cpsprim48325 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48098, %struct.ScmObj* %ae51261)
store volatile %struct.ScmObj* %cpsprim48325, %struct.ScmObj** %stackaddr$prim55263, align 8
%ae51263 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54264$k483220 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55264 = alloca %struct.ScmObj*, align 8
%argslist54264$k483221 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48325, %struct.ScmObj* %argslist54264$k483220)
store volatile %struct.ScmObj* %argslist54264$k483221, %struct.ScmObj** %stackaddr$prim55264, align 8
%stackaddr$prim55265 = alloca %struct.ScmObj*, align 8
%argslist54264$k483222 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51263, %struct.ScmObj* %argslist54264$k483221)
store volatile %struct.ScmObj* %argslist54264$k483222, %struct.ScmObj** %stackaddr$prim55265, align 8
%clofunc55266 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48322)
musttail call tailcc void %clofunc55266(%struct.ScmObj* %k48322, %struct.ScmObj* %argslist54264$k483222)
ret void
falsebranch$cmp55262:
%ae51274 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55267 = alloca %struct.ScmObj*, align 8
%anf_45bind48246 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48098, %struct.ScmObj* %ae51274)
store volatile %struct.ScmObj* %anf_45bind48246, %struct.ScmObj** %stackaddr$prim55267, align 8
%stackaddr$prim55268 = alloca %struct.ScmObj*, align 8
%anf_45bind48247 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48246)
store volatile %struct.ScmObj* %anf_45bind48247, %struct.ScmObj** %stackaddr$prim55268, align 8
%ae51277 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55269 = alloca %struct.ScmObj*, align 8
%_95048101 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48098, %struct.ScmObj* %ae51277, %struct.ScmObj* %anf_45bind48247)
store volatile %struct.ScmObj* %_95048101, %struct.ScmObj** %stackaddr$prim55269, align 8
%argslist54265$cc480990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55270 = alloca %struct.ScmObj*, align 8
%argslist54265$cc480991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48099, %struct.ScmObj* %argslist54265$cc480990)
store volatile %struct.ScmObj* %argslist54265$cc480991, %struct.ScmObj** %stackaddr$prim55270, align 8
%stackaddr$prim55271 = alloca %struct.ScmObj*, align 8
%argslist54265$cc480992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48322, %struct.ScmObj* %argslist54265$cc480991)
store volatile %struct.ScmObj* %argslist54265$cc480992, %struct.ScmObj** %stackaddr$prim55271, align 8
%clofunc55272 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48099)
musttail call tailcc void %clofunc55272(%struct.ScmObj* %cc48099, %struct.ScmObj* %argslist54265$cc480992)
ret void
}

define tailcc void @proc_clo$ae51225(%struct.ScmObj* %env$ae51225,%struct.ScmObj* %current_45args54267) {
%stackaddr$prim55273 = alloca %struct.ScmObj*, align 8
%k48326 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54267)
store volatile %struct.ScmObj* %k48326, %struct.ScmObj** %stackaddr$prim55273, align 8
%stackaddr$prim55274 = alloca %struct.ScmObj*, align 8
%current_45args54268 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54267)
store volatile %struct.ScmObj* %current_45args54268, %struct.ScmObj** %stackaddr$prim55274, align 8
%stackaddr$prim55275 = alloca %struct.ScmObj*, align 8
%u48100 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54268)
store volatile %struct.ScmObj* %u48100, %struct.ScmObj** %stackaddr$prim55275, align 8
%argslist54270$u481000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55276 = alloca %struct.ScmObj*, align 8
%argslist54270$u481001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u48100, %struct.ScmObj* %argslist54270$u481000)
store volatile %struct.ScmObj* %argslist54270$u481001, %struct.ScmObj** %stackaddr$prim55276, align 8
%stackaddr$prim55277 = alloca %struct.ScmObj*, align 8
%argslist54270$u481002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48326, %struct.ScmObj* %argslist54270$u481001)
store volatile %struct.ScmObj* %argslist54270$u481002, %struct.ScmObj** %stackaddr$prim55277, align 8
%clofunc55278 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u48100)
musttail call tailcc void %clofunc55278(%struct.ScmObj* %u48100, %struct.ScmObj* %argslist54270$u481002)
ret void
}

define tailcc void @proc_clo$ae50684(%struct.ScmObj* %env$ae50684,%struct.ScmObj* %current_45args54273) {
%stackaddr$prim55279 = alloca %struct.ScmObj*, align 8
%k48327 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54273)
store volatile %struct.ScmObj* %k48327, %struct.ScmObj** %stackaddr$prim55279, align 8
%stackaddr$prim55280 = alloca %struct.ScmObj*, align 8
%current_45args54274 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54273)
store volatile %struct.ScmObj* %current_45args54274, %struct.ScmObj** %stackaddr$prim55280, align 8
%stackaddr$prim55281 = alloca %struct.ScmObj*, align 8
%lst48104 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54274)
store volatile %struct.ScmObj* %lst48104, %struct.ScmObj** %stackaddr$prim55281, align 8
%stackaddr$prim55282 = alloca %struct.ScmObj*, align 8
%current_45args54275 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54274)
store volatile %struct.ScmObj* %current_45args54275, %struct.ScmObj** %stackaddr$prim55282, align 8
%stackaddr$prim55283 = alloca %struct.ScmObj*, align 8
%n48103 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54275)
store volatile %struct.ScmObj* %n48103, %struct.ScmObj** %stackaddr$prim55283, align 8
%ae50685 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55284 = alloca %struct.ScmObj*, align 8
%n48106 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50685, %struct.ScmObj* %n48103)
store volatile %struct.ScmObj* %n48106, %struct.ScmObj** %stackaddr$prim55284, align 8
%ae50687 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55285 = alloca %struct.ScmObj*, align 8
%lst48105 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50687, %struct.ScmObj* %lst48104)
store volatile %struct.ScmObj* %lst48105, %struct.ScmObj** %stackaddr$prim55285, align 8
%stackaddr$makeclosure55286 = alloca %struct.ScmObj*, align 8
%fptrToInt55287 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50689 to i64
%ae50689 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55287)
store volatile %struct.ScmObj* %ae50689, %struct.ScmObj** %stackaddr$makeclosure55286, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50689, %struct.ScmObj* %lst48105, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50689, %struct.ScmObj* %k48327, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50689, %struct.ScmObj* %n48106, i64 2)
%ae50690 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55288 = alloca %struct.ScmObj*, align 8
%fptrToInt55289 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50691 to i64
%ae50691 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55289)
store volatile %struct.ScmObj* %ae50691, %struct.ScmObj** %stackaddr$makeclosure55288, align 8
%argslist54295$ae506890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55290 = alloca %struct.ScmObj*, align 8
%argslist54295$ae506891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50691, %struct.ScmObj* %argslist54295$ae506890)
store volatile %struct.ScmObj* %argslist54295$ae506891, %struct.ScmObj** %stackaddr$prim55290, align 8
%stackaddr$prim55291 = alloca %struct.ScmObj*, align 8
%argslist54295$ae506892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50690, %struct.ScmObj* %argslist54295$ae506891)
store volatile %struct.ScmObj* %argslist54295$ae506892, %struct.ScmObj** %stackaddr$prim55291, align 8
%clofunc55292 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50689)
musttail call tailcc void %clofunc55292(%struct.ScmObj* %ae50689, %struct.ScmObj* %argslist54295$ae506892)
ret void
}

define tailcc void @proc_clo$ae50689(%struct.ScmObj* %env$ae50689,%struct.ScmObj* %current_45args54277) {
%stackaddr$env-ref55293 = alloca %struct.ScmObj*, align 8
%lst48105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50689, i64 0)
store %struct.ScmObj* %lst48105, %struct.ScmObj** %stackaddr$env-ref55293
%stackaddr$env-ref55294 = alloca %struct.ScmObj*, align 8
%k48327 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50689, i64 1)
store %struct.ScmObj* %k48327, %struct.ScmObj** %stackaddr$env-ref55294
%stackaddr$env-ref55295 = alloca %struct.ScmObj*, align 8
%n48106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50689, i64 2)
store %struct.ScmObj* %n48106, %struct.ScmObj** %stackaddr$env-ref55295
%stackaddr$prim55296 = alloca %struct.ScmObj*, align 8
%_95k48328 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54277)
store volatile %struct.ScmObj* %_95k48328, %struct.ScmObj** %stackaddr$prim55296, align 8
%stackaddr$prim55297 = alloca %struct.ScmObj*, align 8
%current_45args54278 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54277)
store volatile %struct.ScmObj* %current_45args54278, %struct.ScmObj** %stackaddr$prim55297, align 8
%stackaddr$prim55298 = alloca %struct.ScmObj*, align 8
%anf_45bind48233 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54278)
store volatile %struct.ScmObj* %anf_45bind48233, %struct.ScmObj** %stackaddr$prim55298, align 8
%stackaddr$makeclosure55299 = alloca %struct.ScmObj*, align 8
%fptrToInt55300 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50705 to i64
%ae50705 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55300)
store volatile %struct.ScmObj* %ae50705, %struct.ScmObj** %stackaddr$makeclosure55299, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50705, %struct.ScmObj* %lst48105, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50705, %struct.ScmObj* %k48327, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50705, %struct.ScmObj* %n48106, i64 2)
%stackaddr$makeclosure55301 = alloca %struct.ScmObj*, align 8
%fptrToInt55302 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50706 to i64
%ae50706 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55302)
store volatile %struct.ScmObj* %ae50706, %struct.ScmObj** %stackaddr$makeclosure55301, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50706, %struct.ScmObj* %lst48105, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50706, %struct.ScmObj* %k48327, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50706, %struct.ScmObj* %n48106, i64 2)
%argslist54290$anf_45bind482330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55303 = alloca %struct.ScmObj*, align 8
%argslist54290$anf_45bind482331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50706, %struct.ScmObj* %argslist54290$anf_45bind482330)
store volatile %struct.ScmObj* %argslist54290$anf_45bind482331, %struct.ScmObj** %stackaddr$prim55303, align 8
%stackaddr$prim55304 = alloca %struct.ScmObj*, align 8
%argslist54290$anf_45bind482332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50705, %struct.ScmObj* %argslist54290$anf_45bind482331)
store volatile %struct.ScmObj* %argslist54290$anf_45bind482332, %struct.ScmObj** %stackaddr$prim55304, align 8
%clofunc55305 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48233)
musttail call tailcc void %clofunc55305(%struct.ScmObj* %anf_45bind48233, %struct.ScmObj* %argslist54290$anf_45bind482332)
ret void
}

define tailcc void @proc_clo$ae50705(%struct.ScmObj* %env$ae50705,%struct.ScmObj* %current_45args54280) {
%stackaddr$env-ref55306 = alloca %struct.ScmObj*, align 8
%lst48105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50705, i64 0)
store %struct.ScmObj* %lst48105, %struct.ScmObj** %stackaddr$env-ref55306
%stackaddr$env-ref55307 = alloca %struct.ScmObj*, align 8
%k48327 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50705, i64 1)
store %struct.ScmObj* %k48327, %struct.ScmObj** %stackaddr$env-ref55307
%stackaddr$env-ref55308 = alloca %struct.ScmObj*, align 8
%n48106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50705, i64 2)
store %struct.ScmObj* %n48106, %struct.ScmObj** %stackaddr$env-ref55308
%stackaddr$prim55309 = alloca %struct.ScmObj*, align 8
%_95k48329 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54280)
store volatile %struct.ScmObj* %_95k48329, %struct.ScmObj** %stackaddr$prim55309, align 8
%stackaddr$prim55310 = alloca %struct.ScmObj*, align 8
%current_45args54281 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54280)
store volatile %struct.ScmObj* %current_45args54281, %struct.ScmObj** %stackaddr$prim55310, align 8
%stackaddr$prim55311 = alloca %struct.ScmObj*, align 8
%cc48107 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54281)
store volatile %struct.ScmObj* %cc48107, %struct.ScmObj** %stackaddr$prim55311, align 8
%ae50848 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55312 = alloca %struct.ScmObj*, align 8
%anf_45bind48234 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48106, %struct.ScmObj* %ae50848)
store volatile %struct.ScmObj* %anf_45bind48234, %struct.ScmObj** %stackaddr$prim55312, align 8
%ae50849 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55313 = alloca %struct.ScmObj*, align 8
%anf_45bind48235 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae50849, %struct.ScmObj* %anf_45bind48234)
store volatile %struct.ScmObj* %anf_45bind48235, %struct.ScmObj** %stackaddr$prim55313, align 8
%truthy$cmp55314 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48235)
%cmp$cmp55314 = icmp eq i64 %truthy$cmp55314, 1
br i1 %cmp$cmp55314, label %truebranch$cmp55314, label %falsebranch$cmp55314
truebranch$cmp55314:
%ae50853 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55315 = alloca %struct.ScmObj*, align 8
%cpsprim48330 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48105, %struct.ScmObj* %ae50853)
store volatile %struct.ScmObj* %cpsprim48330, %struct.ScmObj** %stackaddr$prim55315, align 8
%ae50855 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54283$k483270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55316 = alloca %struct.ScmObj*, align 8
%argslist54283$k483271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48330, %struct.ScmObj* %argslist54283$k483270)
store volatile %struct.ScmObj* %argslist54283$k483271, %struct.ScmObj** %stackaddr$prim55316, align 8
%stackaddr$prim55317 = alloca %struct.ScmObj*, align 8
%argslist54283$k483272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50855, %struct.ScmObj* %argslist54283$k483271)
store volatile %struct.ScmObj* %argslist54283$k483272, %struct.ScmObj** %stackaddr$prim55317, align 8
%clofunc55318 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48327)
musttail call tailcc void %clofunc55318(%struct.ScmObj* %k48327, %struct.ScmObj* %argslist54283$k483272)
ret void
falsebranch$cmp55314:
%ae50866 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55319 = alloca %struct.ScmObj*, align 8
%anf_45bind48236 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48105, %struct.ScmObj* %ae50866)
store volatile %struct.ScmObj* %anf_45bind48236, %struct.ScmObj** %stackaddr$prim55319, align 8
%stackaddr$prim55320 = alloca %struct.ScmObj*, align 8
%anf_45bind48237 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48236)
store volatile %struct.ScmObj* %anf_45bind48237, %struct.ScmObj** %stackaddr$prim55320, align 8
%ae50869 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55321 = alloca %struct.ScmObj*, align 8
%_95048110 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48105, %struct.ScmObj* %ae50869, %struct.ScmObj* %anf_45bind48237)
store volatile %struct.ScmObj* %_95048110, %struct.ScmObj** %stackaddr$prim55321, align 8
%ae50872 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55322 = alloca %struct.ScmObj*, align 8
%anf_45bind48238 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48106, %struct.ScmObj* %ae50872)
store volatile %struct.ScmObj* %anf_45bind48238, %struct.ScmObj** %stackaddr$prim55322, align 8
%ae50874 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55323 = alloca %struct.ScmObj*, align 8
%anf_45bind48239 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind48238, %struct.ScmObj* %ae50874)
store volatile %struct.ScmObj* %anf_45bind48239, %struct.ScmObj** %stackaddr$prim55323, align 8
%ae50876 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55324 = alloca %struct.ScmObj*, align 8
%_95148109 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n48106, %struct.ScmObj* %ae50876, %struct.ScmObj* %anf_45bind48239)
store volatile %struct.ScmObj* %_95148109, %struct.ScmObj** %stackaddr$prim55324, align 8
%argslist54284$cc481070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55325 = alloca %struct.ScmObj*, align 8
%argslist54284$cc481071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48107, %struct.ScmObj* %argslist54284$cc481070)
store volatile %struct.ScmObj* %argslist54284$cc481071, %struct.ScmObj** %stackaddr$prim55325, align 8
%stackaddr$prim55326 = alloca %struct.ScmObj*, align 8
%argslist54284$cc481072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48327, %struct.ScmObj* %argslist54284$cc481071)
store volatile %struct.ScmObj* %argslist54284$cc481072, %struct.ScmObj** %stackaddr$prim55326, align 8
%clofunc55327 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48107)
musttail call tailcc void %clofunc55327(%struct.ScmObj* %cc48107, %struct.ScmObj* %argslist54284$cc481072)
ret void
}

define tailcc void @proc_clo$ae50706(%struct.ScmObj* %env$ae50706,%struct.ScmObj* %current_45args54285) {
%stackaddr$env-ref55328 = alloca %struct.ScmObj*, align 8
%lst48105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50706, i64 0)
store %struct.ScmObj* %lst48105, %struct.ScmObj** %stackaddr$env-ref55328
%stackaddr$env-ref55329 = alloca %struct.ScmObj*, align 8
%k48327 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50706, i64 1)
store %struct.ScmObj* %k48327, %struct.ScmObj** %stackaddr$env-ref55329
%stackaddr$env-ref55330 = alloca %struct.ScmObj*, align 8
%n48106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50706, i64 2)
store %struct.ScmObj* %n48106, %struct.ScmObj** %stackaddr$env-ref55330
%stackaddr$prim55331 = alloca %struct.ScmObj*, align 8
%_95k48329 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54285)
store volatile %struct.ScmObj* %_95k48329, %struct.ScmObj** %stackaddr$prim55331, align 8
%stackaddr$prim55332 = alloca %struct.ScmObj*, align 8
%current_45args54286 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54285)
store volatile %struct.ScmObj* %current_45args54286, %struct.ScmObj** %stackaddr$prim55332, align 8
%stackaddr$prim55333 = alloca %struct.ScmObj*, align 8
%cc48107 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54286)
store volatile %struct.ScmObj* %cc48107, %struct.ScmObj** %stackaddr$prim55333, align 8
%ae50708 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55334 = alloca %struct.ScmObj*, align 8
%anf_45bind48234 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48106, %struct.ScmObj* %ae50708)
store volatile %struct.ScmObj* %anf_45bind48234, %struct.ScmObj** %stackaddr$prim55334, align 8
%ae50709 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55335 = alloca %struct.ScmObj*, align 8
%anf_45bind48235 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae50709, %struct.ScmObj* %anf_45bind48234)
store volatile %struct.ScmObj* %anf_45bind48235, %struct.ScmObj** %stackaddr$prim55335, align 8
%truthy$cmp55336 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48235)
%cmp$cmp55336 = icmp eq i64 %truthy$cmp55336, 1
br i1 %cmp$cmp55336, label %truebranch$cmp55336, label %falsebranch$cmp55336
truebranch$cmp55336:
%ae50713 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55337 = alloca %struct.ScmObj*, align 8
%cpsprim48330 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48105, %struct.ScmObj* %ae50713)
store volatile %struct.ScmObj* %cpsprim48330, %struct.ScmObj** %stackaddr$prim55337, align 8
%ae50715 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54288$k483270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55338 = alloca %struct.ScmObj*, align 8
%argslist54288$k483271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48330, %struct.ScmObj* %argslist54288$k483270)
store volatile %struct.ScmObj* %argslist54288$k483271, %struct.ScmObj** %stackaddr$prim55338, align 8
%stackaddr$prim55339 = alloca %struct.ScmObj*, align 8
%argslist54288$k483272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50715, %struct.ScmObj* %argslist54288$k483271)
store volatile %struct.ScmObj* %argslist54288$k483272, %struct.ScmObj** %stackaddr$prim55339, align 8
%clofunc55340 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48327)
musttail call tailcc void %clofunc55340(%struct.ScmObj* %k48327, %struct.ScmObj* %argslist54288$k483272)
ret void
falsebranch$cmp55336:
%ae50726 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55341 = alloca %struct.ScmObj*, align 8
%anf_45bind48236 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48105, %struct.ScmObj* %ae50726)
store volatile %struct.ScmObj* %anf_45bind48236, %struct.ScmObj** %stackaddr$prim55341, align 8
%stackaddr$prim55342 = alloca %struct.ScmObj*, align 8
%anf_45bind48237 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48236)
store volatile %struct.ScmObj* %anf_45bind48237, %struct.ScmObj** %stackaddr$prim55342, align 8
%ae50729 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55343 = alloca %struct.ScmObj*, align 8
%_95048110 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48105, %struct.ScmObj* %ae50729, %struct.ScmObj* %anf_45bind48237)
store volatile %struct.ScmObj* %_95048110, %struct.ScmObj** %stackaddr$prim55343, align 8
%ae50732 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55344 = alloca %struct.ScmObj*, align 8
%anf_45bind48238 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48106, %struct.ScmObj* %ae50732)
store volatile %struct.ScmObj* %anf_45bind48238, %struct.ScmObj** %stackaddr$prim55344, align 8
%ae50734 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55345 = alloca %struct.ScmObj*, align 8
%anf_45bind48239 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind48238, %struct.ScmObj* %ae50734)
store volatile %struct.ScmObj* %anf_45bind48239, %struct.ScmObj** %stackaddr$prim55345, align 8
%ae50736 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55346 = alloca %struct.ScmObj*, align 8
%_95148109 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n48106, %struct.ScmObj* %ae50736, %struct.ScmObj* %anf_45bind48239)
store volatile %struct.ScmObj* %_95148109, %struct.ScmObj** %stackaddr$prim55346, align 8
%argslist54289$cc481070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55347 = alloca %struct.ScmObj*, align 8
%argslist54289$cc481071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48107, %struct.ScmObj* %argslist54289$cc481070)
store volatile %struct.ScmObj* %argslist54289$cc481071, %struct.ScmObj** %stackaddr$prim55347, align 8
%stackaddr$prim55348 = alloca %struct.ScmObj*, align 8
%argslist54289$cc481072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48327, %struct.ScmObj* %argslist54289$cc481071)
store volatile %struct.ScmObj* %argslist54289$cc481072, %struct.ScmObj** %stackaddr$prim55348, align 8
%clofunc55349 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48107)
musttail call tailcc void %clofunc55349(%struct.ScmObj* %cc48107, %struct.ScmObj* %argslist54289$cc481072)
ret void
}

define tailcc void @proc_clo$ae50691(%struct.ScmObj* %env$ae50691,%struct.ScmObj* %current_45args54291) {
%stackaddr$prim55350 = alloca %struct.ScmObj*, align 8
%k48331 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54291)
store volatile %struct.ScmObj* %k48331, %struct.ScmObj** %stackaddr$prim55350, align 8
%stackaddr$prim55351 = alloca %struct.ScmObj*, align 8
%current_45args54292 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54291)
store volatile %struct.ScmObj* %current_45args54292, %struct.ScmObj** %stackaddr$prim55351, align 8
%stackaddr$prim55352 = alloca %struct.ScmObj*, align 8
%u48108 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54292)
store volatile %struct.ScmObj* %u48108, %struct.ScmObj** %stackaddr$prim55352, align 8
%argslist54294$u481080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55353 = alloca %struct.ScmObj*, align 8
%argslist54294$u481081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u48108, %struct.ScmObj* %argslist54294$u481080)
store volatile %struct.ScmObj* %argslist54294$u481081, %struct.ScmObj** %stackaddr$prim55353, align 8
%stackaddr$prim55354 = alloca %struct.ScmObj*, align 8
%argslist54294$u481082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48331, %struct.ScmObj* %argslist54294$u481081)
store volatile %struct.ScmObj* %argslist54294$u481082, %struct.ScmObj** %stackaddr$prim55354, align 8
%clofunc55355 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u48108)
musttail call tailcc void %clofunc55355(%struct.ScmObj* %u48108, %struct.ScmObj* %argslist54294$u481082)
ret void
}

define tailcc void @proc_clo$ae50268(%struct.ScmObj* %env$ae50268,%struct.ScmObj* %current_45args54297) {
%stackaddr$prim55356 = alloca %struct.ScmObj*, align 8
%k48332 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54297)
store volatile %struct.ScmObj* %k48332, %struct.ScmObj** %stackaddr$prim55356, align 8
%stackaddr$prim55357 = alloca %struct.ScmObj*, align 8
%current_45args54298 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54297)
store volatile %struct.ScmObj* %current_45args54298, %struct.ScmObj** %stackaddr$prim55357, align 8
%stackaddr$prim55358 = alloca %struct.ScmObj*, align 8
%a48112 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54298)
store volatile %struct.ScmObj* %a48112, %struct.ScmObj** %stackaddr$prim55358, align 8
%ae50269 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55359 = alloca %struct.ScmObj*, align 8
%a48113 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50269, %struct.ScmObj* %a48112)
store volatile %struct.ScmObj* %a48113, %struct.ScmObj** %stackaddr$prim55359, align 8
%stackaddr$makeclosure55360 = alloca %struct.ScmObj*, align 8
%fptrToInt55361 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50271 to i64
%ae50271 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55361)
store volatile %struct.ScmObj* %ae50271, %struct.ScmObj** %stackaddr$makeclosure55360, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50271, %struct.ScmObj* %a48113, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50271, %struct.ScmObj* %k48332, i64 1)
%ae50272 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55362 = alloca %struct.ScmObj*, align 8
%fptrToInt55363 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50273 to i64
%ae50273 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55363)
store volatile %struct.ScmObj* %ae50273, %struct.ScmObj** %stackaddr$makeclosure55362, align 8
%argslist54320$ae502710 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55364 = alloca %struct.ScmObj*, align 8
%argslist54320$ae502711 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50273, %struct.ScmObj* %argslist54320$ae502710)
store volatile %struct.ScmObj* %argslist54320$ae502711, %struct.ScmObj** %stackaddr$prim55364, align 8
%stackaddr$prim55365 = alloca %struct.ScmObj*, align 8
%argslist54320$ae502712 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50272, %struct.ScmObj* %argslist54320$ae502711)
store volatile %struct.ScmObj* %argslist54320$ae502712, %struct.ScmObj** %stackaddr$prim55365, align 8
%clofunc55366 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50271)
musttail call tailcc void %clofunc55366(%struct.ScmObj* %ae50271, %struct.ScmObj* %argslist54320$ae502712)
ret void
}

define tailcc void @proc_clo$ae50271(%struct.ScmObj* %env$ae50271,%struct.ScmObj* %current_45args54300) {
%stackaddr$env-ref55367 = alloca %struct.ScmObj*, align 8
%a48113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50271, i64 0)
store %struct.ScmObj* %a48113, %struct.ScmObj** %stackaddr$env-ref55367
%stackaddr$env-ref55368 = alloca %struct.ScmObj*, align 8
%k48332 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50271, i64 1)
store %struct.ScmObj* %k48332, %struct.ScmObj** %stackaddr$env-ref55368
%stackaddr$prim55369 = alloca %struct.ScmObj*, align 8
%_95k48333 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54300)
store volatile %struct.ScmObj* %_95k48333, %struct.ScmObj** %stackaddr$prim55369, align 8
%stackaddr$prim55370 = alloca %struct.ScmObj*, align 8
%current_45args54301 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54300)
store volatile %struct.ScmObj* %current_45args54301, %struct.ScmObj** %stackaddr$prim55370, align 8
%stackaddr$prim55371 = alloca %struct.ScmObj*, align 8
%anf_45bind48225 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54301)
store volatile %struct.ScmObj* %anf_45bind48225, %struct.ScmObj** %stackaddr$prim55371, align 8
%stackaddr$makeclosure55372 = alloca %struct.ScmObj*, align 8
%fptrToInt55373 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50290 to i64
%ae50290 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55373)
store volatile %struct.ScmObj* %ae50290, %struct.ScmObj** %stackaddr$makeclosure55372, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50290, %struct.ScmObj* %a48113, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50290, %struct.ScmObj* %k48332, i64 1)
%stackaddr$makeclosure55374 = alloca %struct.ScmObj*, align 8
%fptrToInt55375 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50291 to i64
%ae50291 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55375)
store volatile %struct.ScmObj* %ae50291, %struct.ScmObj** %stackaddr$makeclosure55374, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50291, %struct.ScmObj* %a48113, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50291, %struct.ScmObj* %k48332, i64 1)
%argslist54315$anf_45bind482250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55376 = alloca %struct.ScmObj*, align 8
%argslist54315$anf_45bind482251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50291, %struct.ScmObj* %argslist54315$anf_45bind482250)
store volatile %struct.ScmObj* %argslist54315$anf_45bind482251, %struct.ScmObj** %stackaddr$prim55376, align 8
%stackaddr$prim55377 = alloca %struct.ScmObj*, align 8
%argslist54315$anf_45bind482252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50290, %struct.ScmObj* %argslist54315$anf_45bind482251)
store volatile %struct.ScmObj* %argslist54315$anf_45bind482252, %struct.ScmObj** %stackaddr$prim55377, align 8
%clofunc55378 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48225)
musttail call tailcc void %clofunc55378(%struct.ScmObj* %anf_45bind48225, %struct.ScmObj* %argslist54315$anf_45bind482252)
ret void
}

define tailcc void @proc_clo$ae50290(%struct.ScmObj* %env$ae50290,%struct.ScmObj* %current_45args54303) {
%stackaddr$env-ref55379 = alloca %struct.ScmObj*, align 8
%a48113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50290, i64 0)
store %struct.ScmObj* %a48113, %struct.ScmObj** %stackaddr$env-ref55379
%stackaddr$env-ref55380 = alloca %struct.ScmObj*, align 8
%k48332 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50290, i64 1)
store %struct.ScmObj* %k48332, %struct.ScmObj** %stackaddr$env-ref55380
%stackaddr$prim55381 = alloca %struct.ScmObj*, align 8
%_95k48334 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54303)
store volatile %struct.ScmObj* %_95k48334, %struct.ScmObj** %stackaddr$prim55381, align 8
%stackaddr$prim55382 = alloca %struct.ScmObj*, align 8
%current_45args54304 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54303)
store volatile %struct.ScmObj* %current_45args54304, %struct.ScmObj** %stackaddr$prim55382, align 8
%stackaddr$prim55383 = alloca %struct.ScmObj*, align 8
%cc48114 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54304)
store volatile %struct.ScmObj* %cc48114, %struct.ScmObj** %stackaddr$prim55383, align 8
%ae50406 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55384 = alloca %struct.ScmObj*, align 8
%anf_45bind48226 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48113, %struct.ScmObj* %ae50406)
store volatile %struct.ScmObj* %anf_45bind48226, %struct.ScmObj** %stackaddr$prim55384, align 8
%stackaddr$prim55385 = alloca %struct.ScmObj*, align 8
%anf_45bind48227 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48226)
store volatile %struct.ScmObj* %anf_45bind48227, %struct.ScmObj** %stackaddr$prim55385, align 8
%truthy$cmp55386 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48227)
%cmp$cmp55386 = icmp eq i64 %truthy$cmp55386, 1
br i1 %cmp$cmp55386, label %truebranch$cmp55386, label %falsebranch$cmp55386
truebranch$cmp55386:
%ae50410 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50411 = call %struct.ScmObj* @const_init_true()
%argslist54306$k483320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55387 = alloca %struct.ScmObj*, align 8
%argslist54306$k483321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50411, %struct.ScmObj* %argslist54306$k483320)
store volatile %struct.ScmObj* %argslist54306$k483321, %struct.ScmObj** %stackaddr$prim55387, align 8
%stackaddr$prim55388 = alloca %struct.ScmObj*, align 8
%argslist54306$k483322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50410, %struct.ScmObj* %argslist54306$k483321)
store volatile %struct.ScmObj* %argslist54306$k483322, %struct.ScmObj** %stackaddr$prim55388, align 8
%clofunc55389 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48332)
musttail call tailcc void %clofunc55389(%struct.ScmObj* %k48332, %struct.ScmObj* %argslist54306$k483322)
ret void
falsebranch$cmp55386:
%ae50419 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55390 = alloca %struct.ScmObj*, align 8
%anf_45bind48228 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48113, %struct.ScmObj* %ae50419)
store volatile %struct.ScmObj* %anf_45bind48228, %struct.ScmObj** %stackaddr$prim55390, align 8
%stackaddr$prim55391 = alloca %struct.ScmObj*, align 8
%anf_45bind48229 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind48228)
store volatile %struct.ScmObj* %anf_45bind48229, %struct.ScmObj** %stackaddr$prim55391, align 8
%truthy$cmp55392 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48229)
%cmp$cmp55392 = icmp eq i64 %truthy$cmp55392, 1
br i1 %cmp$cmp55392, label %truebranch$cmp55392, label %falsebranch$cmp55392
truebranch$cmp55392:
%ae50423 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55393 = alloca %struct.ScmObj*, align 8
%anf_45bind48230 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48113, %struct.ScmObj* %ae50423)
store volatile %struct.ScmObj* %anf_45bind48230, %struct.ScmObj** %stackaddr$prim55393, align 8
%stackaddr$prim55394 = alloca %struct.ScmObj*, align 8
%b48116 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48230)
store volatile %struct.ScmObj* %b48116, %struct.ScmObj** %stackaddr$prim55394, align 8
%ae50426 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55395 = alloca %struct.ScmObj*, align 8
%anf_45bind48231 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48113, %struct.ScmObj* %ae50426)
store volatile %struct.ScmObj* %anf_45bind48231, %struct.ScmObj** %stackaddr$prim55395, align 8
%stackaddr$prim55396 = alloca %struct.ScmObj*, align 8
%anf_45bind48232 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48231)
store volatile %struct.ScmObj* %anf_45bind48232, %struct.ScmObj** %stackaddr$prim55396, align 8
%ae50429 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55397 = alloca %struct.ScmObj*, align 8
%_95048117 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a48113, %struct.ScmObj* %ae50429, %struct.ScmObj* %anf_45bind48232)
store volatile %struct.ScmObj* %_95048117, %struct.ScmObj** %stackaddr$prim55397, align 8
%argslist54307$cc481140 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55398 = alloca %struct.ScmObj*, align 8
%argslist54307$cc481141 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48114, %struct.ScmObj* %argslist54307$cc481140)
store volatile %struct.ScmObj* %argslist54307$cc481141, %struct.ScmObj** %stackaddr$prim55398, align 8
%stackaddr$prim55399 = alloca %struct.ScmObj*, align 8
%argslist54307$cc481142 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48332, %struct.ScmObj* %argslist54307$cc481141)
store volatile %struct.ScmObj* %argslist54307$cc481142, %struct.ScmObj** %stackaddr$prim55399, align 8
%clofunc55400 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48114)
musttail call tailcc void %clofunc55400(%struct.ScmObj* %cc48114, %struct.ScmObj* %argslist54307$cc481142)
ret void
falsebranch$cmp55392:
%ae50462 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50463 = call %struct.ScmObj* @const_init_false()
%argslist54308$k483320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55401 = alloca %struct.ScmObj*, align 8
%argslist54308$k483321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50463, %struct.ScmObj* %argslist54308$k483320)
store volatile %struct.ScmObj* %argslist54308$k483321, %struct.ScmObj** %stackaddr$prim55401, align 8
%stackaddr$prim55402 = alloca %struct.ScmObj*, align 8
%argslist54308$k483322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50462, %struct.ScmObj* %argslist54308$k483321)
store volatile %struct.ScmObj* %argslist54308$k483322, %struct.ScmObj** %stackaddr$prim55402, align 8
%clofunc55403 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48332)
musttail call tailcc void %clofunc55403(%struct.ScmObj* %k48332, %struct.ScmObj* %argslist54308$k483322)
ret void
}

define tailcc void @proc_clo$ae50291(%struct.ScmObj* %env$ae50291,%struct.ScmObj* %current_45args54309) {
%stackaddr$env-ref55404 = alloca %struct.ScmObj*, align 8
%a48113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50291, i64 0)
store %struct.ScmObj* %a48113, %struct.ScmObj** %stackaddr$env-ref55404
%stackaddr$env-ref55405 = alloca %struct.ScmObj*, align 8
%k48332 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50291, i64 1)
store %struct.ScmObj* %k48332, %struct.ScmObj** %stackaddr$env-ref55405
%stackaddr$prim55406 = alloca %struct.ScmObj*, align 8
%_95k48334 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54309)
store volatile %struct.ScmObj* %_95k48334, %struct.ScmObj** %stackaddr$prim55406, align 8
%stackaddr$prim55407 = alloca %struct.ScmObj*, align 8
%current_45args54310 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54309)
store volatile %struct.ScmObj* %current_45args54310, %struct.ScmObj** %stackaddr$prim55407, align 8
%stackaddr$prim55408 = alloca %struct.ScmObj*, align 8
%cc48114 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54310)
store volatile %struct.ScmObj* %cc48114, %struct.ScmObj** %stackaddr$prim55408, align 8
%ae50293 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55409 = alloca %struct.ScmObj*, align 8
%anf_45bind48226 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48113, %struct.ScmObj* %ae50293)
store volatile %struct.ScmObj* %anf_45bind48226, %struct.ScmObj** %stackaddr$prim55409, align 8
%stackaddr$prim55410 = alloca %struct.ScmObj*, align 8
%anf_45bind48227 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48226)
store volatile %struct.ScmObj* %anf_45bind48227, %struct.ScmObj** %stackaddr$prim55410, align 8
%truthy$cmp55411 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48227)
%cmp$cmp55411 = icmp eq i64 %truthy$cmp55411, 1
br i1 %cmp$cmp55411, label %truebranch$cmp55411, label %falsebranch$cmp55411
truebranch$cmp55411:
%ae50297 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50298 = call %struct.ScmObj* @const_init_true()
%argslist54312$k483320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55412 = alloca %struct.ScmObj*, align 8
%argslist54312$k483321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50298, %struct.ScmObj* %argslist54312$k483320)
store volatile %struct.ScmObj* %argslist54312$k483321, %struct.ScmObj** %stackaddr$prim55412, align 8
%stackaddr$prim55413 = alloca %struct.ScmObj*, align 8
%argslist54312$k483322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50297, %struct.ScmObj* %argslist54312$k483321)
store volatile %struct.ScmObj* %argslist54312$k483322, %struct.ScmObj** %stackaddr$prim55413, align 8
%clofunc55414 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48332)
musttail call tailcc void %clofunc55414(%struct.ScmObj* %k48332, %struct.ScmObj* %argslist54312$k483322)
ret void
falsebranch$cmp55411:
%ae50306 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55415 = alloca %struct.ScmObj*, align 8
%anf_45bind48228 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48113, %struct.ScmObj* %ae50306)
store volatile %struct.ScmObj* %anf_45bind48228, %struct.ScmObj** %stackaddr$prim55415, align 8
%stackaddr$prim55416 = alloca %struct.ScmObj*, align 8
%anf_45bind48229 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind48228)
store volatile %struct.ScmObj* %anf_45bind48229, %struct.ScmObj** %stackaddr$prim55416, align 8
%truthy$cmp55417 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48229)
%cmp$cmp55417 = icmp eq i64 %truthy$cmp55417, 1
br i1 %cmp$cmp55417, label %truebranch$cmp55417, label %falsebranch$cmp55417
truebranch$cmp55417:
%ae50310 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55418 = alloca %struct.ScmObj*, align 8
%anf_45bind48230 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48113, %struct.ScmObj* %ae50310)
store volatile %struct.ScmObj* %anf_45bind48230, %struct.ScmObj** %stackaddr$prim55418, align 8
%stackaddr$prim55419 = alloca %struct.ScmObj*, align 8
%b48116 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48230)
store volatile %struct.ScmObj* %b48116, %struct.ScmObj** %stackaddr$prim55419, align 8
%ae50313 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55420 = alloca %struct.ScmObj*, align 8
%anf_45bind48231 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48113, %struct.ScmObj* %ae50313)
store volatile %struct.ScmObj* %anf_45bind48231, %struct.ScmObj** %stackaddr$prim55420, align 8
%stackaddr$prim55421 = alloca %struct.ScmObj*, align 8
%anf_45bind48232 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48231)
store volatile %struct.ScmObj* %anf_45bind48232, %struct.ScmObj** %stackaddr$prim55421, align 8
%ae50316 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55422 = alloca %struct.ScmObj*, align 8
%_95048117 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a48113, %struct.ScmObj* %ae50316, %struct.ScmObj* %anf_45bind48232)
store volatile %struct.ScmObj* %_95048117, %struct.ScmObj** %stackaddr$prim55422, align 8
%argslist54313$cc481140 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55423 = alloca %struct.ScmObj*, align 8
%argslist54313$cc481141 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48114, %struct.ScmObj* %argslist54313$cc481140)
store volatile %struct.ScmObj* %argslist54313$cc481141, %struct.ScmObj** %stackaddr$prim55423, align 8
%stackaddr$prim55424 = alloca %struct.ScmObj*, align 8
%argslist54313$cc481142 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48332, %struct.ScmObj* %argslist54313$cc481141)
store volatile %struct.ScmObj* %argslist54313$cc481142, %struct.ScmObj** %stackaddr$prim55424, align 8
%clofunc55425 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48114)
musttail call tailcc void %clofunc55425(%struct.ScmObj* %cc48114, %struct.ScmObj* %argslist54313$cc481142)
ret void
falsebranch$cmp55417:
%ae50349 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50350 = call %struct.ScmObj* @const_init_false()
%argslist54314$k483320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55426 = alloca %struct.ScmObj*, align 8
%argslist54314$k483321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50350, %struct.ScmObj* %argslist54314$k483320)
store volatile %struct.ScmObj* %argslist54314$k483321, %struct.ScmObj** %stackaddr$prim55426, align 8
%stackaddr$prim55427 = alloca %struct.ScmObj*, align 8
%argslist54314$k483322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50349, %struct.ScmObj* %argslist54314$k483321)
store volatile %struct.ScmObj* %argslist54314$k483322, %struct.ScmObj** %stackaddr$prim55427, align 8
%clofunc55428 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48332)
musttail call tailcc void %clofunc55428(%struct.ScmObj* %k48332, %struct.ScmObj* %argslist54314$k483322)
ret void
}

define tailcc void @proc_clo$ae50273(%struct.ScmObj* %env$ae50273,%struct.ScmObj* %current_45args54316) {
%stackaddr$prim55429 = alloca %struct.ScmObj*, align 8
%k48335 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54316)
store volatile %struct.ScmObj* %k48335, %struct.ScmObj** %stackaddr$prim55429, align 8
%stackaddr$prim55430 = alloca %struct.ScmObj*, align 8
%current_45args54317 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54316)
store volatile %struct.ScmObj* %current_45args54317, %struct.ScmObj** %stackaddr$prim55430, align 8
%stackaddr$prim55431 = alloca %struct.ScmObj*, align 8
%k48115 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54317)
store volatile %struct.ScmObj* %k48115, %struct.ScmObj** %stackaddr$prim55431, align 8
%ae50275 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54319$k483350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55432 = alloca %struct.ScmObj*, align 8
%argslist54319$k483351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48115, %struct.ScmObj* %argslist54319$k483350)
store volatile %struct.ScmObj* %argslist54319$k483351, %struct.ScmObj** %stackaddr$prim55432, align 8
%stackaddr$prim55433 = alloca %struct.ScmObj*, align 8
%argslist54319$k483352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50275, %struct.ScmObj* %argslist54319$k483351)
store volatile %struct.ScmObj* %argslist54319$k483352, %struct.ScmObj** %stackaddr$prim55433, align 8
%clofunc55434 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48335)
musttail call tailcc void %clofunc55434(%struct.ScmObj* %k48335, %struct.ScmObj* %argslist54319$k483352)
ret void
}

define tailcc void @proc_clo$ae50196(%struct.ScmObj* %env$ae50196,%struct.ScmObj* %current_45args54322) {
%stackaddr$env-ref55435 = alloca %struct.ScmObj*, align 8
%_37append48119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50196, i64 0)
store %struct.ScmObj* %_37append48119, %struct.ScmObj** %stackaddr$env-ref55435
%stackaddr$prim55436 = alloca %struct.ScmObj*, align 8
%k48336 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54322)
store volatile %struct.ScmObj* %k48336, %struct.ScmObj** %stackaddr$prim55436, align 8
%stackaddr$prim55437 = alloca %struct.ScmObj*, align 8
%current_45args54323 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54322)
store volatile %struct.ScmObj* %current_45args54323, %struct.ScmObj** %stackaddr$prim55437, align 8
%stackaddr$prim55438 = alloca %struct.ScmObj*, align 8
%ls048122 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54323)
store volatile %struct.ScmObj* %ls048122, %struct.ScmObj** %stackaddr$prim55438, align 8
%stackaddr$prim55439 = alloca %struct.ScmObj*, align 8
%current_45args54324 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54323)
store volatile %struct.ScmObj* %current_45args54324, %struct.ScmObj** %stackaddr$prim55439, align 8
%stackaddr$prim55440 = alloca %struct.ScmObj*, align 8
%ls148121 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54324)
store volatile %struct.ScmObj* %ls148121, %struct.ScmObj** %stackaddr$prim55440, align 8
%stackaddr$prim55441 = alloca %struct.ScmObj*, align 8
%anf_45bind48219 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls048122)
store volatile %struct.ScmObj* %anf_45bind48219, %struct.ScmObj** %stackaddr$prim55441, align 8
%truthy$cmp55442 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48219)
%cmp$cmp55442 = icmp eq i64 %truthy$cmp55442, 1
br i1 %cmp$cmp55442, label %truebranch$cmp55442, label %falsebranch$cmp55442
truebranch$cmp55442:
%ae50200 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54326$k483360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55443 = alloca %struct.ScmObj*, align 8
%argslist54326$k483361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls148121, %struct.ScmObj* %argslist54326$k483360)
store volatile %struct.ScmObj* %argslist54326$k483361, %struct.ScmObj** %stackaddr$prim55443, align 8
%stackaddr$prim55444 = alloca %struct.ScmObj*, align 8
%argslist54326$k483362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50200, %struct.ScmObj* %argslist54326$k483361)
store volatile %struct.ScmObj* %argslist54326$k483362, %struct.ScmObj** %stackaddr$prim55444, align 8
%clofunc55445 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48336)
musttail call tailcc void %clofunc55445(%struct.ScmObj* %k48336, %struct.ScmObj* %argslist54326$k483362)
ret void
falsebranch$cmp55442:
%stackaddr$prim55446 = alloca %struct.ScmObj*, align 8
%anf_45bind48220 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls048122)
store volatile %struct.ScmObj* %anf_45bind48220, %struct.ScmObj** %stackaddr$prim55446, align 8
%ae50207 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55447 = alloca %struct.ScmObj*, align 8
%anf_45bind48221 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append48119, %struct.ScmObj* %ae50207)
store volatile %struct.ScmObj* %anf_45bind48221, %struct.ScmObj** %stackaddr$prim55447, align 8
%stackaddr$prim55448 = alloca %struct.ScmObj*, align 8
%anf_45bind48222 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls048122)
store volatile %struct.ScmObj* %anf_45bind48222, %struct.ScmObj** %stackaddr$prim55448, align 8
%stackaddr$makeclosure55449 = alloca %struct.ScmObj*, align 8
%fptrToInt55450 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50210 to i64
%ae50210 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55450)
store volatile %struct.ScmObj* %ae50210, %struct.ScmObj** %stackaddr$makeclosure55449, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50210, %struct.ScmObj* %k48336, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50210, %struct.ScmObj* %anf_45bind48220, i64 1)
%argslist54331$anf_45bind482210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55451 = alloca %struct.ScmObj*, align 8
%argslist54331$anf_45bind482211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls148121, %struct.ScmObj* %argslist54331$anf_45bind482210)
store volatile %struct.ScmObj* %argslist54331$anf_45bind482211, %struct.ScmObj** %stackaddr$prim55451, align 8
%stackaddr$prim55452 = alloca %struct.ScmObj*, align 8
%argslist54331$anf_45bind482212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48222, %struct.ScmObj* %argslist54331$anf_45bind482211)
store volatile %struct.ScmObj* %argslist54331$anf_45bind482212, %struct.ScmObj** %stackaddr$prim55452, align 8
%stackaddr$prim55453 = alloca %struct.ScmObj*, align 8
%argslist54331$anf_45bind482213 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50210, %struct.ScmObj* %argslist54331$anf_45bind482212)
store volatile %struct.ScmObj* %argslist54331$anf_45bind482213, %struct.ScmObj** %stackaddr$prim55453, align 8
%clofunc55454 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48221)
musttail call tailcc void %clofunc55454(%struct.ScmObj* %anf_45bind48221, %struct.ScmObj* %argslist54331$anf_45bind482213)
ret void
}

define tailcc void @proc_clo$ae50210(%struct.ScmObj* %env$ae50210,%struct.ScmObj* %current_45args54327) {
%stackaddr$env-ref55455 = alloca %struct.ScmObj*, align 8
%k48336 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50210, i64 0)
store %struct.ScmObj* %k48336, %struct.ScmObj** %stackaddr$env-ref55455
%stackaddr$env-ref55456 = alloca %struct.ScmObj*, align 8
%anf_45bind48220 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50210, i64 1)
store %struct.ScmObj* %anf_45bind48220, %struct.ScmObj** %stackaddr$env-ref55456
%stackaddr$prim55457 = alloca %struct.ScmObj*, align 8
%_95k48337 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54327)
store volatile %struct.ScmObj* %_95k48337, %struct.ScmObj** %stackaddr$prim55457, align 8
%stackaddr$prim55458 = alloca %struct.ScmObj*, align 8
%current_45args54328 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54327)
store volatile %struct.ScmObj* %current_45args54328, %struct.ScmObj** %stackaddr$prim55458, align 8
%stackaddr$prim55459 = alloca %struct.ScmObj*, align 8
%anf_45bind48223 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54328)
store volatile %struct.ScmObj* %anf_45bind48223, %struct.ScmObj** %stackaddr$prim55459, align 8
%stackaddr$prim55460 = alloca %struct.ScmObj*, align 8
%cpsprim48338 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48220, %struct.ScmObj* %anf_45bind48223)
store volatile %struct.ScmObj* %cpsprim48338, %struct.ScmObj** %stackaddr$prim55460, align 8
%ae50216 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54330$k483360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55461 = alloca %struct.ScmObj*, align 8
%argslist54330$k483361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48338, %struct.ScmObj* %argslist54330$k483360)
store volatile %struct.ScmObj* %argslist54330$k483361, %struct.ScmObj** %stackaddr$prim55461, align 8
%stackaddr$prim55462 = alloca %struct.ScmObj*, align 8
%argslist54330$k483362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50216, %struct.ScmObj* %argslist54330$k483361)
store volatile %struct.ScmObj* %argslist54330$k483362, %struct.ScmObj** %stackaddr$prim55462, align 8
%clofunc55463 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48336)
musttail call tailcc void %clofunc55463(%struct.ScmObj* %k48336, %struct.ScmObj* %argslist54330$k483362)
ret void
}

define tailcc void @proc_clo$ae50170(%struct.ScmObj* %env$ae50170,%struct.ScmObj* %current_45args54333) {
%stackaddr$prim55464 = alloca %struct.ScmObj*, align 8
%k48339 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54333)
store volatile %struct.ScmObj* %k48339, %struct.ScmObj** %stackaddr$prim55464, align 8
%stackaddr$prim55465 = alloca %struct.ScmObj*, align 8
%current_45args54334 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54333)
store volatile %struct.ScmObj* %current_45args54334, %struct.ScmObj** %stackaddr$prim55465, align 8
%stackaddr$prim55466 = alloca %struct.ScmObj*, align 8
%a48125 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54334)
store volatile %struct.ScmObj* %a48125, %struct.ScmObj** %stackaddr$prim55466, align 8
%stackaddr$prim55467 = alloca %struct.ScmObj*, align 8
%current_45args54335 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54334)
store volatile %struct.ScmObj* %current_45args54335, %struct.ScmObj** %stackaddr$prim55467, align 8
%stackaddr$prim55468 = alloca %struct.ScmObj*, align 8
%b48124 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54335)
store volatile %struct.ScmObj* %b48124, %struct.ScmObj** %stackaddr$prim55468, align 8
%stackaddr$prim55469 = alloca %struct.ScmObj*, align 8
%anf_45bind48218 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a48125, %struct.ScmObj* %b48124)
store volatile %struct.ScmObj* %anf_45bind48218, %struct.ScmObj** %stackaddr$prim55469, align 8
%stackaddr$prim55470 = alloca %struct.ScmObj*, align 8
%cpsprim48340 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind48218)
store volatile %struct.ScmObj* %cpsprim48340, %struct.ScmObj** %stackaddr$prim55470, align 8
%ae50175 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54337$k483390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55471 = alloca %struct.ScmObj*, align 8
%argslist54337$k483391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48340, %struct.ScmObj* %argslist54337$k483390)
store volatile %struct.ScmObj* %argslist54337$k483391, %struct.ScmObj** %stackaddr$prim55471, align 8
%stackaddr$prim55472 = alloca %struct.ScmObj*, align 8
%argslist54337$k483392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50175, %struct.ScmObj* %argslist54337$k483391)
store volatile %struct.ScmObj* %argslist54337$k483392, %struct.ScmObj** %stackaddr$prim55472, align 8
%clofunc55473 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48339)
musttail call tailcc void %clofunc55473(%struct.ScmObj* %k48339, %struct.ScmObj* %argslist54337$k483392)
ret void
}

define tailcc void @proc_clo$ae50146(%struct.ScmObj* %env$ae50146,%struct.ScmObj* %current_45args54339) {
%stackaddr$prim55474 = alloca %struct.ScmObj*, align 8
%k48341 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54339)
store volatile %struct.ScmObj* %k48341, %struct.ScmObj** %stackaddr$prim55474, align 8
%stackaddr$prim55475 = alloca %struct.ScmObj*, align 8
%current_45args54340 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54339)
store volatile %struct.ScmObj* %current_45args54340, %struct.ScmObj** %stackaddr$prim55475, align 8
%stackaddr$prim55476 = alloca %struct.ScmObj*, align 8
%a48128 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54340)
store volatile %struct.ScmObj* %a48128, %struct.ScmObj** %stackaddr$prim55476, align 8
%stackaddr$prim55477 = alloca %struct.ScmObj*, align 8
%current_45args54341 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54340)
store volatile %struct.ScmObj* %current_45args54341, %struct.ScmObj** %stackaddr$prim55477, align 8
%stackaddr$prim55478 = alloca %struct.ScmObj*, align 8
%b48127 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54341)
store volatile %struct.ScmObj* %b48127, %struct.ScmObj** %stackaddr$prim55478, align 8
%stackaddr$prim55479 = alloca %struct.ScmObj*, align 8
%anf_45bind48217 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a48128, %struct.ScmObj* %b48127)
store volatile %struct.ScmObj* %anf_45bind48217, %struct.ScmObj** %stackaddr$prim55479, align 8
%stackaddr$prim55480 = alloca %struct.ScmObj*, align 8
%cpsprim48342 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind48217)
store volatile %struct.ScmObj* %cpsprim48342, %struct.ScmObj** %stackaddr$prim55480, align 8
%ae50151 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54343$k483410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55481 = alloca %struct.ScmObj*, align 8
%argslist54343$k483411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48342, %struct.ScmObj* %argslist54343$k483410)
store volatile %struct.ScmObj* %argslist54343$k483411, %struct.ScmObj** %stackaddr$prim55481, align 8
%stackaddr$prim55482 = alloca %struct.ScmObj*, align 8
%argslist54343$k483412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50151, %struct.ScmObj* %argslist54343$k483411)
store volatile %struct.ScmObj* %argslist54343$k483412, %struct.ScmObj** %stackaddr$prim55482, align 8
%clofunc55483 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48341)
musttail call tailcc void %clofunc55483(%struct.ScmObj* %k48341, %struct.ScmObj* %argslist54343$k483412)
ret void
}

define tailcc void @proc_clo$ae49752(%struct.ScmObj* %env$ae49752,%struct.ScmObj* %current_45args54346) {
%stackaddr$env-ref55484 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49752, i64 0)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref55484
%stackaddr$env-ref55485 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49752, i64 1)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55485
%stackaddr$env-ref55486 = alloca %struct.ScmObj*, align 8
%_37map148078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49752, i64 2)
store %struct.ScmObj* %_37map148078, %struct.ScmObj** %stackaddr$env-ref55486
%stackaddr$prim55487 = alloca %struct.ScmObj*, align 8
%k48343 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54346)
store volatile %struct.ScmObj* %k48343, %struct.ScmObj** %stackaddr$prim55487, align 8
%stackaddr$prim55488 = alloca %struct.ScmObj*, align 8
%current_45args54347 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54346)
store volatile %struct.ScmObj* %current_45args54347, %struct.ScmObj** %stackaddr$prim55488, align 8
%stackaddr$prim55489 = alloca %struct.ScmObj*, align 8
%_37foldl48130 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54347)
store volatile %struct.ScmObj* %_37foldl48130, %struct.ScmObj** %stackaddr$prim55489, align 8
%ae49754 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55490 = alloca %struct.ScmObj*, align 8
%fptrToInt55491 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49755 to i64
%ae49755 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55491)
store volatile %struct.ScmObj* %ae49755, %struct.ScmObj** %stackaddr$makeclosure55490, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49755, %struct.ScmObj* %_37foldr48052, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49755, %struct.ScmObj* %_37foldl48130, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49755, %struct.ScmObj* %_37foldr148047, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49755, %struct.ScmObj* %_37map148078, i64 3)
%argslist54404$k483430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55492 = alloca %struct.ScmObj*, align 8
%argslist54404$k483431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49755, %struct.ScmObj* %argslist54404$k483430)
store volatile %struct.ScmObj* %argslist54404$k483431, %struct.ScmObj** %stackaddr$prim55492, align 8
%stackaddr$prim55493 = alloca %struct.ScmObj*, align 8
%argslist54404$k483432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49754, %struct.ScmObj* %argslist54404$k483431)
store volatile %struct.ScmObj* %argslist54404$k483432, %struct.ScmObj** %stackaddr$prim55493, align 8
%clofunc55494 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48343)
musttail call tailcc void %clofunc55494(%struct.ScmObj* %k48343, %struct.ScmObj* %argslist54404$k483432)
ret void
}

define tailcc void @proc_clo$ae49755(%struct.ScmObj* %env$ae49755,%struct.ScmObj* %args4813148344) {
%stackaddr$env-ref55495 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49755, i64 0)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref55495
%stackaddr$env-ref55496 = alloca %struct.ScmObj*, align 8
%_37foldl48130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49755, i64 1)
store %struct.ScmObj* %_37foldl48130, %struct.ScmObj** %stackaddr$env-ref55496
%stackaddr$env-ref55497 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49755, i64 2)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55497
%stackaddr$env-ref55498 = alloca %struct.ScmObj*, align 8
%_37map148078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49755, i64 3)
store %struct.ScmObj* %_37map148078, %struct.ScmObj** %stackaddr$env-ref55498
%stackaddr$prim55499 = alloca %struct.ScmObj*, align 8
%k48345 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4813148344)
store volatile %struct.ScmObj* %k48345, %struct.ScmObj** %stackaddr$prim55499, align 8
%stackaddr$prim55500 = alloca %struct.ScmObj*, align 8
%args48131 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4813148344)
store volatile %struct.ScmObj* %args48131, %struct.ScmObj** %stackaddr$prim55500, align 8
%stackaddr$prim55501 = alloca %struct.ScmObj*, align 8
%f48134 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48131)
store volatile %struct.ScmObj* %f48134, %struct.ScmObj** %stackaddr$prim55501, align 8
%stackaddr$prim55502 = alloca %struct.ScmObj*, align 8
%anf_45bind48205 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48131)
store volatile %struct.ScmObj* %anf_45bind48205, %struct.ScmObj** %stackaddr$prim55502, align 8
%stackaddr$prim55503 = alloca %struct.ScmObj*, align 8
%acc48133 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48205)
store volatile %struct.ScmObj* %acc48133, %struct.ScmObj** %stackaddr$prim55503, align 8
%stackaddr$prim55504 = alloca %struct.ScmObj*, align 8
%anf_45bind48206 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48131)
store volatile %struct.ScmObj* %anf_45bind48206, %struct.ScmObj** %stackaddr$prim55504, align 8
%stackaddr$prim55505 = alloca %struct.ScmObj*, align 8
%lsts48132 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48206)
store volatile %struct.ScmObj* %lsts48132, %struct.ScmObj** %stackaddr$prim55505, align 8
%stackaddr$makeclosure55506 = alloca %struct.ScmObj*, align 8
%fptrToInt55507 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49763 to i64
%ae49763 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt55507)
store volatile %struct.ScmObj* %ae49763, %struct.ScmObj** %stackaddr$makeclosure55506, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49763, %struct.ScmObj* %lsts48132, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49763, %struct.ScmObj* %_37foldr48052, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49763, %struct.ScmObj* %k48345, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49763, %struct.ScmObj* %f48134, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49763, %struct.ScmObj* %acc48133, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49763, %struct.ScmObj* %_37foldl48130, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49763, %struct.ScmObj* %_37foldr148047, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae49763, %struct.ScmObj* %_37map148078, i64 7)
%ae49764 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55508 = alloca %struct.ScmObj*, align 8
%fptrToInt55509 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49765 to i64
%ae49765 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55509)
store volatile %struct.ScmObj* %ae49765, %struct.ScmObj** %stackaddr$makeclosure55508, align 8
%argslist54403$ae497630 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55510 = alloca %struct.ScmObj*, align 8
%argslist54403$ae497631 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49765, %struct.ScmObj* %argslist54403$ae497630)
store volatile %struct.ScmObj* %argslist54403$ae497631, %struct.ScmObj** %stackaddr$prim55510, align 8
%stackaddr$prim55511 = alloca %struct.ScmObj*, align 8
%argslist54403$ae497632 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49764, %struct.ScmObj* %argslist54403$ae497631)
store volatile %struct.ScmObj* %argslist54403$ae497632, %struct.ScmObj** %stackaddr$prim55511, align 8
%clofunc55512 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49763)
musttail call tailcc void %clofunc55512(%struct.ScmObj* %ae49763, %struct.ScmObj* %argslist54403$ae497632)
ret void
}

define tailcc void @proc_clo$ae49763(%struct.ScmObj* %env$ae49763,%struct.ScmObj* %current_45args54349) {
%stackaddr$env-ref55513 = alloca %struct.ScmObj*, align 8
%lsts48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49763, i64 0)
store %struct.ScmObj* %lsts48132, %struct.ScmObj** %stackaddr$env-ref55513
%stackaddr$env-ref55514 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49763, i64 1)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref55514
%stackaddr$env-ref55515 = alloca %struct.ScmObj*, align 8
%k48345 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49763, i64 2)
store %struct.ScmObj* %k48345, %struct.ScmObj** %stackaddr$env-ref55515
%stackaddr$env-ref55516 = alloca %struct.ScmObj*, align 8
%f48134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49763, i64 3)
store %struct.ScmObj* %f48134, %struct.ScmObj** %stackaddr$env-ref55516
%stackaddr$env-ref55517 = alloca %struct.ScmObj*, align 8
%acc48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49763, i64 4)
store %struct.ScmObj* %acc48133, %struct.ScmObj** %stackaddr$env-ref55517
%stackaddr$env-ref55518 = alloca %struct.ScmObj*, align 8
%_37foldl48130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49763, i64 5)
store %struct.ScmObj* %_37foldl48130, %struct.ScmObj** %stackaddr$env-ref55518
%stackaddr$env-ref55519 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49763, i64 6)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55519
%stackaddr$env-ref55520 = alloca %struct.ScmObj*, align 8
%_37map148078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49763, i64 7)
store %struct.ScmObj* %_37map148078, %struct.ScmObj** %stackaddr$env-ref55520
%stackaddr$prim55521 = alloca %struct.ScmObj*, align 8
%_95k48346 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54349)
store volatile %struct.ScmObj* %_95k48346, %struct.ScmObj** %stackaddr$prim55521, align 8
%stackaddr$prim55522 = alloca %struct.ScmObj*, align 8
%current_45args54350 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54349)
store volatile %struct.ScmObj* %current_45args54350, %struct.ScmObj** %stackaddr$prim55522, align 8
%stackaddr$prim55523 = alloca %struct.ScmObj*, align 8
%anf_45bind48207 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54350)
store volatile %struct.ScmObj* %anf_45bind48207, %struct.ScmObj** %stackaddr$prim55523, align 8
%stackaddr$makeclosure55524 = alloca %struct.ScmObj*, align 8
%fptrToInt55525 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49795 to i64
%ae49795 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55525)
store volatile %struct.ScmObj* %ae49795, %struct.ScmObj** %stackaddr$makeclosure55524, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49795, %struct.ScmObj* %lsts48132, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49795, %struct.ScmObj* %_37foldr48052, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49795, %struct.ScmObj* %k48345, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49795, %struct.ScmObj* %f48134, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49795, %struct.ScmObj* %acc48133, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49795, %struct.ScmObj* %_37foldl48130, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49795, %struct.ScmObj* %_37map148078, i64 6)
%ae49797 = call %struct.ScmObj* @const_init_false()
%argslist54396$_37foldr1480470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55526 = alloca %struct.ScmObj*, align 8
%argslist54396$_37foldr1480471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48132, %struct.ScmObj* %argslist54396$_37foldr1480470)
store volatile %struct.ScmObj* %argslist54396$_37foldr1480471, %struct.ScmObj** %stackaddr$prim55526, align 8
%stackaddr$prim55527 = alloca %struct.ScmObj*, align 8
%argslist54396$_37foldr1480472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49797, %struct.ScmObj* %argslist54396$_37foldr1480471)
store volatile %struct.ScmObj* %argslist54396$_37foldr1480472, %struct.ScmObj** %stackaddr$prim55527, align 8
%stackaddr$prim55528 = alloca %struct.ScmObj*, align 8
%argslist54396$_37foldr1480473 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48207, %struct.ScmObj* %argslist54396$_37foldr1480472)
store volatile %struct.ScmObj* %argslist54396$_37foldr1480473, %struct.ScmObj** %stackaddr$prim55528, align 8
%stackaddr$prim55529 = alloca %struct.ScmObj*, align 8
%argslist54396$_37foldr1480474 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49795, %struct.ScmObj* %argslist54396$_37foldr1480473)
store volatile %struct.ScmObj* %argslist54396$_37foldr1480474, %struct.ScmObj** %stackaddr$prim55529, align 8
%clofunc55530 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148047)
musttail call tailcc void %clofunc55530(%struct.ScmObj* %_37foldr148047, %struct.ScmObj* %argslist54396$_37foldr1480474)
ret void
}

define tailcc void @proc_clo$ae49795(%struct.ScmObj* %env$ae49795,%struct.ScmObj* %current_45args54352) {
%stackaddr$env-ref55531 = alloca %struct.ScmObj*, align 8
%lsts48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49795, i64 0)
store %struct.ScmObj* %lsts48132, %struct.ScmObj** %stackaddr$env-ref55531
%stackaddr$env-ref55532 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49795, i64 1)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref55532
%stackaddr$env-ref55533 = alloca %struct.ScmObj*, align 8
%k48345 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49795, i64 2)
store %struct.ScmObj* %k48345, %struct.ScmObj** %stackaddr$env-ref55533
%stackaddr$env-ref55534 = alloca %struct.ScmObj*, align 8
%f48134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49795, i64 3)
store %struct.ScmObj* %f48134, %struct.ScmObj** %stackaddr$env-ref55534
%stackaddr$env-ref55535 = alloca %struct.ScmObj*, align 8
%acc48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49795, i64 4)
store %struct.ScmObj* %acc48133, %struct.ScmObj** %stackaddr$env-ref55535
%stackaddr$env-ref55536 = alloca %struct.ScmObj*, align 8
%_37foldl48130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49795, i64 5)
store %struct.ScmObj* %_37foldl48130, %struct.ScmObj** %stackaddr$env-ref55536
%stackaddr$env-ref55537 = alloca %struct.ScmObj*, align 8
%_37map148078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49795, i64 6)
store %struct.ScmObj* %_37map148078, %struct.ScmObj** %stackaddr$env-ref55537
%stackaddr$prim55538 = alloca %struct.ScmObj*, align 8
%_95k48347 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54352)
store volatile %struct.ScmObj* %_95k48347, %struct.ScmObj** %stackaddr$prim55538, align 8
%stackaddr$prim55539 = alloca %struct.ScmObj*, align 8
%current_45args54353 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54352)
store volatile %struct.ScmObj* %current_45args54353, %struct.ScmObj** %stackaddr$prim55539, align 8
%stackaddr$prim55540 = alloca %struct.ScmObj*, align 8
%anf_45bind48208 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54353)
store volatile %struct.ScmObj* %anf_45bind48208, %struct.ScmObj** %stackaddr$prim55540, align 8
%truthy$cmp55541 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48208)
%cmp$cmp55541 = icmp eq i64 %truthy$cmp55541, 1
br i1 %cmp$cmp55541, label %truebranch$cmp55541, label %falsebranch$cmp55541
truebranch$cmp55541:
%ae49806 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54355$k483450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55542 = alloca %struct.ScmObj*, align 8
%argslist54355$k483451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48133, %struct.ScmObj* %argslist54355$k483450)
store volatile %struct.ScmObj* %argslist54355$k483451, %struct.ScmObj** %stackaddr$prim55542, align 8
%stackaddr$prim55543 = alloca %struct.ScmObj*, align 8
%argslist54355$k483452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49806, %struct.ScmObj* %argslist54355$k483451)
store volatile %struct.ScmObj* %argslist54355$k483452, %struct.ScmObj** %stackaddr$prim55543, align 8
%clofunc55544 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48345)
musttail call tailcc void %clofunc55544(%struct.ScmObj* %k48345, %struct.ScmObj* %argslist54355$k483452)
ret void
falsebranch$cmp55541:
%stackaddr$makeclosure55545 = alloca %struct.ScmObj*, align 8
%fptrToInt55546 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49811 to i64
%ae49811 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55546)
store volatile %struct.ScmObj* %ae49811, %struct.ScmObj** %stackaddr$makeclosure55545, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49811, %struct.ScmObj* %lsts48132, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49811, %struct.ScmObj* %_37foldr48052, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49811, %struct.ScmObj* %k48345, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49811, %struct.ScmObj* %f48134, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49811, %struct.ScmObj* %acc48133, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49811, %struct.ScmObj* %_37foldl48130, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49811, %struct.ScmObj* %_37map148078, i64 6)
%ae49812 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55547 = alloca %struct.ScmObj*, align 8
%fptrToInt55548 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49813 to i64
%ae49813 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55548)
store volatile %struct.ScmObj* %ae49813, %struct.ScmObj** %stackaddr$makeclosure55547, align 8
%argslist54395$ae498110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55549 = alloca %struct.ScmObj*, align 8
%argslist54395$ae498111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49813, %struct.ScmObj* %argslist54395$ae498110)
store volatile %struct.ScmObj* %argslist54395$ae498111, %struct.ScmObj** %stackaddr$prim55549, align 8
%stackaddr$prim55550 = alloca %struct.ScmObj*, align 8
%argslist54395$ae498112 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49812, %struct.ScmObj* %argslist54395$ae498111)
store volatile %struct.ScmObj* %argslist54395$ae498112, %struct.ScmObj** %stackaddr$prim55550, align 8
%clofunc55551 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49811)
musttail call tailcc void %clofunc55551(%struct.ScmObj* %ae49811, %struct.ScmObj* %argslist54395$ae498112)
ret void
}

define tailcc void @proc_clo$ae49811(%struct.ScmObj* %env$ae49811,%struct.ScmObj* %current_45args54356) {
%stackaddr$env-ref55552 = alloca %struct.ScmObj*, align 8
%lsts48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49811, i64 0)
store %struct.ScmObj* %lsts48132, %struct.ScmObj** %stackaddr$env-ref55552
%stackaddr$env-ref55553 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49811, i64 1)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref55553
%stackaddr$env-ref55554 = alloca %struct.ScmObj*, align 8
%k48345 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49811, i64 2)
store %struct.ScmObj* %k48345, %struct.ScmObj** %stackaddr$env-ref55554
%stackaddr$env-ref55555 = alloca %struct.ScmObj*, align 8
%f48134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49811, i64 3)
store %struct.ScmObj* %f48134, %struct.ScmObj** %stackaddr$env-ref55555
%stackaddr$env-ref55556 = alloca %struct.ScmObj*, align 8
%acc48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49811, i64 4)
store %struct.ScmObj* %acc48133, %struct.ScmObj** %stackaddr$env-ref55556
%stackaddr$env-ref55557 = alloca %struct.ScmObj*, align 8
%_37foldl48130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49811, i64 5)
store %struct.ScmObj* %_37foldl48130, %struct.ScmObj** %stackaddr$env-ref55557
%stackaddr$env-ref55558 = alloca %struct.ScmObj*, align 8
%_37map148078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49811, i64 6)
store %struct.ScmObj* %_37map148078, %struct.ScmObj** %stackaddr$env-ref55558
%stackaddr$prim55559 = alloca %struct.ScmObj*, align 8
%_95k48348 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54356)
store volatile %struct.ScmObj* %_95k48348, %struct.ScmObj** %stackaddr$prim55559, align 8
%stackaddr$prim55560 = alloca %struct.ScmObj*, align 8
%current_45args54357 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54356)
store volatile %struct.ScmObj* %current_45args54357, %struct.ScmObj** %stackaddr$prim55560, align 8
%stackaddr$prim55561 = alloca %struct.ScmObj*, align 8
%anf_45bind48209 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54357)
store volatile %struct.ScmObj* %anf_45bind48209, %struct.ScmObj** %stackaddr$prim55561, align 8
%stackaddr$makeclosure55562 = alloca %struct.ScmObj*, align 8
%fptrToInt55563 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49832 to i64
%ae49832 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55563)
store volatile %struct.ScmObj* %ae49832, %struct.ScmObj** %stackaddr$makeclosure55562, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49832, %struct.ScmObj* %lsts48132, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49832, %struct.ScmObj* %_37foldr48052, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49832, %struct.ScmObj* %k48345, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49832, %struct.ScmObj* %f48134, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49832, %struct.ScmObj* %acc48133, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49832, %struct.ScmObj* %_37foldl48130, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49832, %struct.ScmObj* %_37map148078, i64 6)
%argslist54390$_37map1480780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55564 = alloca %struct.ScmObj*, align 8
%argslist54390$_37map1480781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48132, %struct.ScmObj* %argslist54390$_37map1480780)
store volatile %struct.ScmObj* %argslist54390$_37map1480781, %struct.ScmObj** %stackaddr$prim55564, align 8
%stackaddr$prim55565 = alloca %struct.ScmObj*, align 8
%argslist54390$_37map1480782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48209, %struct.ScmObj* %argslist54390$_37map1480781)
store volatile %struct.ScmObj* %argslist54390$_37map1480782, %struct.ScmObj** %stackaddr$prim55565, align 8
%stackaddr$prim55566 = alloca %struct.ScmObj*, align 8
%argslist54390$_37map1480783 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49832, %struct.ScmObj* %argslist54390$_37map1480782)
store volatile %struct.ScmObj* %argslist54390$_37map1480783, %struct.ScmObj** %stackaddr$prim55566, align 8
%clofunc55567 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148078)
musttail call tailcc void %clofunc55567(%struct.ScmObj* %_37map148078, %struct.ScmObj* %argslist54390$_37map1480783)
ret void
}

define tailcc void @proc_clo$ae49832(%struct.ScmObj* %env$ae49832,%struct.ScmObj* %current_45args54359) {
%stackaddr$env-ref55568 = alloca %struct.ScmObj*, align 8
%lsts48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49832, i64 0)
store %struct.ScmObj* %lsts48132, %struct.ScmObj** %stackaddr$env-ref55568
%stackaddr$env-ref55569 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49832, i64 1)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref55569
%stackaddr$env-ref55570 = alloca %struct.ScmObj*, align 8
%k48345 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49832, i64 2)
store %struct.ScmObj* %k48345, %struct.ScmObj** %stackaddr$env-ref55570
%stackaddr$env-ref55571 = alloca %struct.ScmObj*, align 8
%f48134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49832, i64 3)
store %struct.ScmObj* %f48134, %struct.ScmObj** %stackaddr$env-ref55571
%stackaddr$env-ref55572 = alloca %struct.ScmObj*, align 8
%acc48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49832, i64 4)
store %struct.ScmObj* %acc48133, %struct.ScmObj** %stackaddr$env-ref55572
%stackaddr$env-ref55573 = alloca %struct.ScmObj*, align 8
%_37foldl48130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49832, i64 5)
store %struct.ScmObj* %_37foldl48130, %struct.ScmObj** %stackaddr$env-ref55573
%stackaddr$env-ref55574 = alloca %struct.ScmObj*, align 8
%_37map148078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49832, i64 6)
store %struct.ScmObj* %_37map148078, %struct.ScmObj** %stackaddr$env-ref55574
%stackaddr$prim55575 = alloca %struct.ScmObj*, align 8
%_95k48349 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54359)
store volatile %struct.ScmObj* %_95k48349, %struct.ScmObj** %stackaddr$prim55575, align 8
%stackaddr$prim55576 = alloca %struct.ScmObj*, align 8
%current_45args54360 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54359)
store volatile %struct.ScmObj* %current_45args54360, %struct.ScmObj** %stackaddr$prim55576, align 8
%stackaddr$prim55577 = alloca %struct.ScmObj*, align 8
%lsts_4348139 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54360)
store volatile %struct.ScmObj* %lsts_4348139, %struct.ScmObj** %stackaddr$prim55577, align 8
%stackaddr$makeclosure55578 = alloca %struct.ScmObj*, align 8
%fptrToInt55579 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49835 to i64
%ae49835 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt55579)
store volatile %struct.ScmObj* %ae49835, %struct.ScmObj** %stackaddr$makeclosure55578, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49835, %struct.ScmObj* %lsts48132, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49835, %struct.ScmObj* %_37foldr48052, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49835, %struct.ScmObj* %k48345, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49835, %struct.ScmObj* %f48134, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49835, %struct.ScmObj* %acc48133, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49835, %struct.ScmObj* %_37foldl48130, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49835, %struct.ScmObj* %_37map148078, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae49835, %struct.ScmObj* %lsts_4348139, i64 7)
%ae49836 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55580 = alloca %struct.ScmObj*, align 8
%fptrToInt55581 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49837 to i64
%ae49837 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55581)
store volatile %struct.ScmObj* %ae49837, %struct.ScmObj** %stackaddr$makeclosure55580, align 8
%argslist54389$ae498350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55582 = alloca %struct.ScmObj*, align 8
%argslist54389$ae498351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49837, %struct.ScmObj* %argslist54389$ae498350)
store volatile %struct.ScmObj* %argslist54389$ae498351, %struct.ScmObj** %stackaddr$prim55582, align 8
%stackaddr$prim55583 = alloca %struct.ScmObj*, align 8
%argslist54389$ae498352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49836, %struct.ScmObj* %argslist54389$ae498351)
store volatile %struct.ScmObj* %argslist54389$ae498352, %struct.ScmObj** %stackaddr$prim55583, align 8
%clofunc55584 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49835)
musttail call tailcc void %clofunc55584(%struct.ScmObj* %ae49835, %struct.ScmObj* %argslist54389$ae498352)
ret void
}

define tailcc void @proc_clo$ae49835(%struct.ScmObj* %env$ae49835,%struct.ScmObj* %current_45args54362) {
%stackaddr$env-ref55585 = alloca %struct.ScmObj*, align 8
%lsts48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49835, i64 0)
store %struct.ScmObj* %lsts48132, %struct.ScmObj** %stackaddr$env-ref55585
%stackaddr$env-ref55586 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49835, i64 1)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref55586
%stackaddr$env-ref55587 = alloca %struct.ScmObj*, align 8
%k48345 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49835, i64 2)
store %struct.ScmObj* %k48345, %struct.ScmObj** %stackaddr$env-ref55587
%stackaddr$env-ref55588 = alloca %struct.ScmObj*, align 8
%f48134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49835, i64 3)
store %struct.ScmObj* %f48134, %struct.ScmObj** %stackaddr$env-ref55588
%stackaddr$env-ref55589 = alloca %struct.ScmObj*, align 8
%acc48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49835, i64 4)
store %struct.ScmObj* %acc48133, %struct.ScmObj** %stackaddr$env-ref55589
%stackaddr$env-ref55590 = alloca %struct.ScmObj*, align 8
%_37foldl48130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49835, i64 5)
store %struct.ScmObj* %_37foldl48130, %struct.ScmObj** %stackaddr$env-ref55590
%stackaddr$env-ref55591 = alloca %struct.ScmObj*, align 8
%_37map148078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49835, i64 6)
store %struct.ScmObj* %_37map148078, %struct.ScmObj** %stackaddr$env-ref55591
%stackaddr$env-ref55592 = alloca %struct.ScmObj*, align 8
%lsts_4348139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49835, i64 7)
store %struct.ScmObj* %lsts_4348139, %struct.ScmObj** %stackaddr$env-ref55592
%stackaddr$prim55593 = alloca %struct.ScmObj*, align 8
%_95k48350 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54362)
store volatile %struct.ScmObj* %_95k48350, %struct.ScmObj** %stackaddr$prim55593, align 8
%stackaddr$prim55594 = alloca %struct.ScmObj*, align 8
%current_45args54363 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54362)
store volatile %struct.ScmObj* %current_45args54363, %struct.ScmObj** %stackaddr$prim55594, align 8
%stackaddr$prim55595 = alloca %struct.ScmObj*, align 8
%anf_45bind48210 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54363)
store volatile %struct.ScmObj* %anf_45bind48210, %struct.ScmObj** %stackaddr$prim55595, align 8
%stackaddr$makeclosure55596 = alloca %struct.ScmObj*, align 8
%fptrToInt55597 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49856 to i64
%ae49856 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt55597)
store volatile %struct.ScmObj* %ae49856, %struct.ScmObj** %stackaddr$makeclosure55596, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49856, %struct.ScmObj* %k48345, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49856, %struct.ScmObj* %f48134, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49856, %struct.ScmObj* %acc48133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49856, %struct.ScmObj* %_37foldr48052, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49856, %struct.ScmObj* %_37foldl48130, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49856, %struct.ScmObj* %lsts_4348139, i64 5)
%argslist54384$_37map1480780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55598 = alloca %struct.ScmObj*, align 8
%argslist54384$_37map1480781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48132, %struct.ScmObj* %argslist54384$_37map1480780)
store volatile %struct.ScmObj* %argslist54384$_37map1480781, %struct.ScmObj** %stackaddr$prim55598, align 8
%stackaddr$prim55599 = alloca %struct.ScmObj*, align 8
%argslist54384$_37map1480782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48210, %struct.ScmObj* %argslist54384$_37map1480781)
store volatile %struct.ScmObj* %argslist54384$_37map1480782, %struct.ScmObj** %stackaddr$prim55599, align 8
%stackaddr$prim55600 = alloca %struct.ScmObj*, align 8
%argslist54384$_37map1480783 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49856, %struct.ScmObj* %argslist54384$_37map1480782)
store volatile %struct.ScmObj* %argslist54384$_37map1480783, %struct.ScmObj** %stackaddr$prim55600, align 8
%clofunc55601 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148078)
musttail call tailcc void %clofunc55601(%struct.ScmObj* %_37map148078, %struct.ScmObj* %argslist54384$_37map1480783)
ret void
}

define tailcc void @proc_clo$ae49856(%struct.ScmObj* %env$ae49856,%struct.ScmObj* %current_45args54365) {
%stackaddr$env-ref55602 = alloca %struct.ScmObj*, align 8
%k48345 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49856, i64 0)
store %struct.ScmObj* %k48345, %struct.ScmObj** %stackaddr$env-ref55602
%stackaddr$env-ref55603 = alloca %struct.ScmObj*, align 8
%f48134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49856, i64 1)
store %struct.ScmObj* %f48134, %struct.ScmObj** %stackaddr$env-ref55603
%stackaddr$env-ref55604 = alloca %struct.ScmObj*, align 8
%acc48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49856, i64 2)
store %struct.ScmObj* %acc48133, %struct.ScmObj** %stackaddr$env-ref55604
%stackaddr$env-ref55605 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49856, i64 3)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref55605
%stackaddr$env-ref55606 = alloca %struct.ScmObj*, align 8
%_37foldl48130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49856, i64 4)
store %struct.ScmObj* %_37foldl48130, %struct.ScmObj** %stackaddr$env-ref55606
%stackaddr$env-ref55607 = alloca %struct.ScmObj*, align 8
%lsts_4348139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49856, i64 5)
store %struct.ScmObj* %lsts_4348139, %struct.ScmObj** %stackaddr$env-ref55607
%stackaddr$prim55608 = alloca %struct.ScmObj*, align 8
%_95k48351 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54365)
store volatile %struct.ScmObj* %_95k48351, %struct.ScmObj** %stackaddr$prim55608, align 8
%stackaddr$prim55609 = alloca %struct.ScmObj*, align 8
%current_45args54366 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54365)
store volatile %struct.ScmObj* %current_45args54366, %struct.ScmObj** %stackaddr$prim55609, align 8
%stackaddr$prim55610 = alloca %struct.ScmObj*, align 8
%vs48137 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54366)
store volatile %struct.ScmObj* %vs48137, %struct.ScmObj** %stackaddr$prim55610, align 8
%stackaddr$makeclosure55611 = alloca %struct.ScmObj*, align 8
%fptrToInt55612 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49859 to i64
%ae49859 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55612)
store volatile %struct.ScmObj* %ae49859, %struct.ScmObj** %stackaddr$makeclosure55611, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49859, %struct.ScmObj* %vs48137, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49859, %struct.ScmObj* %k48345, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49859, %struct.ScmObj* %f48134, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49859, %struct.ScmObj* %acc48133, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49859, %struct.ScmObj* %_37foldr48052, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49859, %struct.ScmObj* %_37foldl48130, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49859, %struct.ScmObj* %lsts_4348139, i64 6)
%ae49860 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55613 = alloca %struct.ScmObj*, align 8
%fptrToInt55614 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49861 to i64
%ae49861 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55614)
store volatile %struct.ScmObj* %ae49861, %struct.ScmObj** %stackaddr$makeclosure55613, align 8
%argslist54383$ae498590 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55615 = alloca %struct.ScmObj*, align 8
%argslist54383$ae498591 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49861, %struct.ScmObj* %argslist54383$ae498590)
store volatile %struct.ScmObj* %argslist54383$ae498591, %struct.ScmObj** %stackaddr$prim55615, align 8
%stackaddr$prim55616 = alloca %struct.ScmObj*, align 8
%argslist54383$ae498592 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49860, %struct.ScmObj* %argslist54383$ae498591)
store volatile %struct.ScmObj* %argslist54383$ae498592, %struct.ScmObj** %stackaddr$prim55616, align 8
%clofunc55617 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49859)
musttail call tailcc void %clofunc55617(%struct.ScmObj* %ae49859, %struct.ScmObj* %argslist54383$ae498592)
ret void
}

define tailcc void @proc_clo$ae49859(%struct.ScmObj* %env$ae49859,%struct.ScmObj* %current_45args54368) {
%stackaddr$env-ref55618 = alloca %struct.ScmObj*, align 8
%vs48137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49859, i64 0)
store %struct.ScmObj* %vs48137, %struct.ScmObj** %stackaddr$env-ref55618
%stackaddr$env-ref55619 = alloca %struct.ScmObj*, align 8
%k48345 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49859, i64 1)
store %struct.ScmObj* %k48345, %struct.ScmObj** %stackaddr$env-ref55619
%stackaddr$env-ref55620 = alloca %struct.ScmObj*, align 8
%f48134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49859, i64 2)
store %struct.ScmObj* %f48134, %struct.ScmObj** %stackaddr$env-ref55620
%stackaddr$env-ref55621 = alloca %struct.ScmObj*, align 8
%acc48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49859, i64 3)
store %struct.ScmObj* %acc48133, %struct.ScmObj** %stackaddr$env-ref55621
%stackaddr$env-ref55622 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49859, i64 4)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref55622
%stackaddr$env-ref55623 = alloca %struct.ScmObj*, align 8
%_37foldl48130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49859, i64 5)
store %struct.ScmObj* %_37foldl48130, %struct.ScmObj** %stackaddr$env-ref55623
%stackaddr$env-ref55624 = alloca %struct.ScmObj*, align 8
%lsts_4348139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49859, i64 6)
store %struct.ScmObj* %lsts_4348139, %struct.ScmObj** %stackaddr$env-ref55624
%stackaddr$prim55625 = alloca %struct.ScmObj*, align 8
%_95k48352 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54368)
store volatile %struct.ScmObj* %_95k48352, %struct.ScmObj** %stackaddr$prim55625, align 8
%stackaddr$prim55626 = alloca %struct.ScmObj*, align 8
%current_45args54369 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54368)
store volatile %struct.ScmObj* %current_45args54369, %struct.ScmObj** %stackaddr$prim55626, align 8
%stackaddr$prim55627 = alloca %struct.ScmObj*, align 8
%anf_45bind48211 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54369)
store volatile %struct.ScmObj* %anf_45bind48211, %struct.ScmObj** %stackaddr$prim55627, align 8
%ae49882 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55628 = alloca %struct.ScmObj*, align 8
%anf_45bind48212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48133, %struct.ScmObj* %ae49882)
store volatile %struct.ScmObj* %anf_45bind48212, %struct.ScmObj** %stackaddr$prim55628, align 8
%stackaddr$makeclosure55629 = alloca %struct.ScmObj*, align 8
%fptrToInt55630 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49884 to i64
%ae49884 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55630)
store volatile %struct.ScmObj* %ae49884, %struct.ScmObj** %stackaddr$makeclosure55629, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49884, %struct.ScmObj* %k48345, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49884, %struct.ScmObj* %f48134, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49884, %struct.ScmObj* %_37foldl48130, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49884, %struct.ScmObj* %lsts_4348139, i64 3)
%argslist54377$_37foldr480520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55631 = alloca %struct.ScmObj*, align 8
%argslist54377$_37foldr480521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs48137, %struct.ScmObj* %argslist54377$_37foldr480520)
store volatile %struct.ScmObj* %argslist54377$_37foldr480521, %struct.ScmObj** %stackaddr$prim55631, align 8
%stackaddr$prim55632 = alloca %struct.ScmObj*, align 8
%argslist54377$_37foldr480522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48212, %struct.ScmObj* %argslist54377$_37foldr480521)
store volatile %struct.ScmObj* %argslist54377$_37foldr480522, %struct.ScmObj** %stackaddr$prim55632, align 8
%stackaddr$prim55633 = alloca %struct.ScmObj*, align 8
%argslist54377$_37foldr480523 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48211, %struct.ScmObj* %argslist54377$_37foldr480522)
store volatile %struct.ScmObj* %argslist54377$_37foldr480523, %struct.ScmObj** %stackaddr$prim55633, align 8
%stackaddr$prim55634 = alloca %struct.ScmObj*, align 8
%argslist54377$_37foldr480524 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49884, %struct.ScmObj* %argslist54377$_37foldr480523)
store volatile %struct.ScmObj* %argslist54377$_37foldr480524, %struct.ScmObj** %stackaddr$prim55634, align 8
%clofunc55635 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr48052)
musttail call tailcc void %clofunc55635(%struct.ScmObj* %_37foldr48052, %struct.ScmObj* %argslist54377$_37foldr480524)
ret void
}

define tailcc void @proc_clo$ae49884(%struct.ScmObj* %env$ae49884,%struct.ScmObj* %current_45args54371) {
%stackaddr$env-ref55636 = alloca %struct.ScmObj*, align 8
%k48345 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49884, i64 0)
store %struct.ScmObj* %k48345, %struct.ScmObj** %stackaddr$env-ref55636
%stackaddr$env-ref55637 = alloca %struct.ScmObj*, align 8
%f48134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49884, i64 1)
store %struct.ScmObj* %f48134, %struct.ScmObj** %stackaddr$env-ref55637
%stackaddr$env-ref55638 = alloca %struct.ScmObj*, align 8
%_37foldl48130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49884, i64 2)
store %struct.ScmObj* %_37foldl48130, %struct.ScmObj** %stackaddr$env-ref55638
%stackaddr$env-ref55639 = alloca %struct.ScmObj*, align 8
%lsts_4348139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49884, i64 3)
store %struct.ScmObj* %lsts_4348139, %struct.ScmObj** %stackaddr$env-ref55639
%stackaddr$prim55640 = alloca %struct.ScmObj*, align 8
%_95k48353 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54371)
store volatile %struct.ScmObj* %_95k48353, %struct.ScmObj** %stackaddr$prim55640, align 8
%stackaddr$prim55641 = alloca %struct.ScmObj*, align 8
%current_45args54372 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54371)
store volatile %struct.ScmObj* %current_45args54372, %struct.ScmObj** %stackaddr$prim55641, align 8
%stackaddr$prim55642 = alloca %struct.ScmObj*, align 8
%anf_45bind48213 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54372)
store volatile %struct.ScmObj* %anf_45bind48213, %struct.ScmObj** %stackaddr$prim55642, align 8
%stackaddr$makeclosure55643 = alloca %struct.ScmObj*, align 8
%fptrToInt55644 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49888 to i64
%ae49888 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55644)
store volatile %struct.ScmObj* %ae49888, %struct.ScmObj** %stackaddr$makeclosure55643, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49888, %struct.ScmObj* %k48345, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49888, %struct.ScmObj* %f48134, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49888, %struct.ScmObj* %_37foldl48130, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49888, %struct.ScmObj* %lsts_4348139, i64 3)
%stackaddr$prim55645 = alloca %struct.ScmObj*, align 8
%cpsargs48356 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49888, %struct.ScmObj* %anf_45bind48213)
store volatile %struct.ScmObj* %cpsargs48356, %struct.ScmObj** %stackaddr$prim55645, align 8
%clofunc55646 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48134)
musttail call tailcc void %clofunc55646(%struct.ScmObj* %f48134, %struct.ScmObj* %cpsargs48356)
ret void
}

define tailcc void @proc_clo$ae49888(%struct.ScmObj* %env$ae49888,%struct.ScmObj* %current_45args54374) {
%stackaddr$env-ref55647 = alloca %struct.ScmObj*, align 8
%k48345 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49888, i64 0)
store %struct.ScmObj* %k48345, %struct.ScmObj** %stackaddr$env-ref55647
%stackaddr$env-ref55648 = alloca %struct.ScmObj*, align 8
%f48134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49888, i64 1)
store %struct.ScmObj* %f48134, %struct.ScmObj** %stackaddr$env-ref55648
%stackaddr$env-ref55649 = alloca %struct.ScmObj*, align 8
%_37foldl48130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49888, i64 2)
store %struct.ScmObj* %_37foldl48130, %struct.ScmObj** %stackaddr$env-ref55649
%stackaddr$env-ref55650 = alloca %struct.ScmObj*, align 8
%lsts_4348139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49888, i64 3)
store %struct.ScmObj* %lsts_4348139, %struct.ScmObj** %stackaddr$env-ref55650
%stackaddr$prim55651 = alloca %struct.ScmObj*, align 8
%_95k48354 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54374)
store volatile %struct.ScmObj* %_95k48354, %struct.ScmObj** %stackaddr$prim55651, align 8
%stackaddr$prim55652 = alloca %struct.ScmObj*, align 8
%current_45args54375 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54374)
store volatile %struct.ScmObj* %current_45args54375, %struct.ScmObj** %stackaddr$prim55652, align 8
%stackaddr$prim55653 = alloca %struct.ScmObj*, align 8
%acc_4348141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54375)
store volatile %struct.ScmObj* %acc_4348141, %struct.ScmObj** %stackaddr$prim55653, align 8
%stackaddr$prim55654 = alloca %struct.ScmObj*, align 8
%anf_45bind48214 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4348141, %struct.ScmObj* %lsts_4348139)
store volatile %struct.ScmObj* %anf_45bind48214, %struct.ScmObj** %stackaddr$prim55654, align 8
%stackaddr$prim55655 = alloca %struct.ScmObj*, align 8
%anf_45bind48215 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48134, %struct.ScmObj* %anf_45bind48214)
store volatile %struct.ScmObj* %anf_45bind48215, %struct.ScmObj** %stackaddr$prim55655, align 8
%stackaddr$prim55656 = alloca %struct.ScmObj*, align 8
%cpsargs48355 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48345, %struct.ScmObj* %anf_45bind48215)
store volatile %struct.ScmObj* %cpsargs48355, %struct.ScmObj** %stackaddr$prim55656, align 8
%clofunc55657 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl48130)
musttail call tailcc void %clofunc55657(%struct.ScmObj* %_37foldl48130, %struct.ScmObj* %cpsargs48355)
ret void
}

define tailcc void @proc_clo$ae49861(%struct.ScmObj* %env$ae49861,%struct.ScmObj* %current_45args54378) {
%stackaddr$prim55658 = alloca %struct.ScmObj*, align 8
%k48357 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54378)
store volatile %struct.ScmObj* %k48357, %struct.ScmObj** %stackaddr$prim55658, align 8
%stackaddr$prim55659 = alloca %struct.ScmObj*, align 8
%current_45args54379 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54378)
store volatile %struct.ScmObj* %current_45args54379, %struct.ScmObj** %stackaddr$prim55659, align 8
%stackaddr$prim55660 = alloca %struct.ScmObj*, align 8
%a48143 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54379)
store volatile %struct.ScmObj* %a48143, %struct.ScmObj** %stackaddr$prim55660, align 8
%stackaddr$prim55661 = alloca %struct.ScmObj*, align 8
%current_45args54380 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54379)
store volatile %struct.ScmObj* %current_45args54380, %struct.ScmObj** %stackaddr$prim55661, align 8
%stackaddr$prim55662 = alloca %struct.ScmObj*, align 8
%b48142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54380)
store volatile %struct.ScmObj* %b48142, %struct.ScmObj** %stackaddr$prim55662, align 8
%stackaddr$prim55663 = alloca %struct.ScmObj*, align 8
%cpsprim48358 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a48143, %struct.ScmObj* %b48142)
store volatile %struct.ScmObj* %cpsprim48358, %struct.ScmObj** %stackaddr$prim55663, align 8
%ae49865 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54382$k483570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55664 = alloca %struct.ScmObj*, align 8
%argslist54382$k483571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48358, %struct.ScmObj* %argslist54382$k483570)
store volatile %struct.ScmObj* %argslist54382$k483571, %struct.ScmObj** %stackaddr$prim55664, align 8
%stackaddr$prim55665 = alloca %struct.ScmObj*, align 8
%argslist54382$k483572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49865, %struct.ScmObj* %argslist54382$k483571)
store volatile %struct.ScmObj* %argslist54382$k483572, %struct.ScmObj** %stackaddr$prim55665, align 8
%clofunc55666 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48357)
musttail call tailcc void %clofunc55666(%struct.ScmObj* %k48357, %struct.ScmObj* %argslist54382$k483572)
ret void
}

define tailcc void @proc_clo$ae49837(%struct.ScmObj* %env$ae49837,%struct.ScmObj* %current_45args54385) {
%stackaddr$prim55667 = alloca %struct.ScmObj*, align 8
%k48359 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54385)
store volatile %struct.ScmObj* %k48359, %struct.ScmObj** %stackaddr$prim55667, align 8
%stackaddr$prim55668 = alloca %struct.ScmObj*, align 8
%current_45args54386 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54385)
store volatile %struct.ScmObj* %current_45args54386, %struct.ScmObj** %stackaddr$prim55668, align 8
%stackaddr$prim55669 = alloca %struct.ScmObj*, align 8
%x48138 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54386)
store volatile %struct.ScmObj* %x48138, %struct.ScmObj** %stackaddr$prim55669, align 8
%stackaddr$prim55670 = alloca %struct.ScmObj*, align 8
%cpsprim48360 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48138)
store volatile %struct.ScmObj* %cpsprim48360, %struct.ScmObj** %stackaddr$prim55670, align 8
%ae49840 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54388$k483590 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55671 = alloca %struct.ScmObj*, align 8
%argslist54388$k483591 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48360, %struct.ScmObj* %argslist54388$k483590)
store volatile %struct.ScmObj* %argslist54388$k483591, %struct.ScmObj** %stackaddr$prim55671, align 8
%stackaddr$prim55672 = alloca %struct.ScmObj*, align 8
%argslist54388$k483592 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49840, %struct.ScmObj* %argslist54388$k483591)
store volatile %struct.ScmObj* %argslist54388$k483592, %struct.ScmObj** %stackaddr$prim55672, align 8
%clofunc55673 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48359)
musttail call tailcc void %clofunc55673(%struct.ScmObj* %k48359, %struct.ScmObj* %argslist54388$k483592)
ret void
}

define tailcc void @proc_clo$ae49813(%struct.ScmObj* %env$ae49813,%struct.ScmObj* %current_45args54391) {
%stackaddr$prim55674 = alloca %struct.ScmObj*, align 8
%k48361 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54391)
store volatile %struct.ScmObj* %k48361, %struct.ScmObj** %stackaddr$prim55674, align 8
%stackaddr$prim55675 = alloca %struct.ScmObj*, align 8
%current_45args54392 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54391)
store volatile %struct.ScmObj* %current_45args54392, %struct.ScmObj** %stackaddr$prim55675, align 8
%stackaddr$prim55676 = alloca %struct.ScmObj*, align 8
%x48140 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54392)
store volatile %struct.ScmObj* %x48140, %struct.ScmObj** %stackaddr$prim55676, align 8
%stackaddr$prim55677 = alloca %struct.ScmObj*, align 8
%cpsprim48362 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48140)
store volatile %struct.ScmObj* %cpsprim48362, %struct.ScmObj** %stackaddr$prim55677, align 8
%ae49816 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54394$k483610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55678 = alloca %struct.ScmObj*, align 8
%argslist54394$k483611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48362, %struct.ScmObj* %argslist54394$k483610)
store volatile %struct.ScmObj* %argslist54394$k483611, %struct.ScmObj** %stackaddr$prim55678, align 8
%stackaddr$prim55679 = alloca %struct.ScmObj*, align 8
%argslist54394$k483612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49816, %struct.ScmObj* %argslist54394$k483611)
store volatile %struct.ScmObj* %argslist54394$k483612, %struct.ScmObj** %stackaddr$prim55679, align 8
%clofunc55680 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48361)
musttail call tailcc void %clofunc55680(%struct.ScmObj* %k48361, %struct.ScmObj* %argslist54394$k483612)
ret void
}

define tailcc void @proc_clo$ae49765(%struct.ScmObj* %env$ae49765,%struct.ScmObj* %current_45args54397) {
%stackaddr$prim55681 = alloca %struct.ScmObj*, align 8
%k48363 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54397)
store volatile %struct.ScmObj* %k48363, %struct.ScmObj** %stackaddr$prim55681, align 8
%stackaddr$prim55682 = alloca %struct.ScmObj*, align 8
%current_45args54398 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54397)
store volatile %struct.ScmObj* %current_45args54398, %struct.ScmObj** %stackaddr$prim55682, align 8
%stackaddr$prim55683 = alloca %struct.ScmObj*, align 8
%lst48136 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54398)
store volatile %struct.ScmObj* %lst48136, %struct.ScmObj** %stackaddr$prim55683, align 8
%stackaddr$prim55684 = alloca %struct.ScmObj*, align 8
%current_45args54399 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54398)
store volatile %struct.ScmObj* %current_45args54399, %struct.ScmObj** %stackaddr$prim55684, align 8
%stackaddr$prim55685 = alloca %struct.ScmObj*, align 8
%b48135 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54399)
store volatile %struct.ScmObj* %b48135, %struct.ScmObj** %stackaddr$prim55685, align 8
%truthy$cmp55686 = call i64 @is_truthy_value(%struct.ScmObj* %b48135)
%cmp$cmp55686 = icmp eq i64 %truthy$cmp55686, 1
br i1 %cmp$cmp55686, label %truebranch$cmp55686, label %falsebranch$cmp55686
truebranch$cmp55686:
%ae49768 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54401$k483630 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55687 = alloca %struct.ScmObj*, align 8
%argslist54401$k483631 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b48135, %struct.ScmObj* %argslist54401$k483630)
store volatile %struct.ScmObj* %argslist54401$k483631, %struct.ScmObj** %stackaddr$prim55687, align 8
%stackaddr$prim55688 = alloca %struct.ScmObj*, align 8
%argslist54401$k483632 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49768, %struct.ScmObj* %argslist54401$k483631)
store volatile %struct.ScmObj* %argslist54401$k483632, %struct.ScmObj** %stackaddr$prim55688, align 8
%clofunc55689 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48363)
musttail call tailcc void %clofunc55689(%struct.ScmObj* %k48363, %struct.ScmObj* %argslist54401$k483632)
ret void
falsebranch$cmp55686:
%stackaddr$prim55690 = alloca %struct.ScmObj*, align 8
%cpsprim48364 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48136)
store volatile %struct.ScmObj* %cpsprim48364, %struct.ScmObj** %stackaddr$prim55690, align 8
%ae49775 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54402$k483630 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55691 = alloca %struct.ScmObj*, align 8
%argslist54402$k483631 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48364, %struct.ScmObj* %argslist54402$k483630)
store volatile %struct.ScmObj* %argslist54402$k483631, %struct.ScmObj** %stackaddr$prim55691, align 8
%stackaddr$prim55692 = alloca %struct.ScmObj*, align 8
%argslist54402$k483632 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49775, %struct.ScmObj* %argslist54402$k483631)
store volatile %struct.ScmObj* %argslist54402$k483632, %struct.ScmObj** %stackaddr$prim55692, align 8
%clofunc55693 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48363)
musttail call tailcc void %clofunc55693(%struct.ScmObj* %k48363, %struct.ScmObj* %argslist54402$k483632)
ret void
}

define tailcc void @proc_clo$ae49606(%struct.ScmObj* %env$ae49606,%struct.ScmObj* %args4807448365) {
%stackaddr$env-ref55694 = alloca %struct.ScmObj*, align 8
%_37last48069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49606, i64 0)
store %struct.ScmObj* %_37last48069, %struct.ScmObj** %stackaddr$env-ref55694
%stackaddr$env-ref55695 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49606, i64 1)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref55695
%stackaddr$env-ref55696 = alloca %struct.ScmObj*, align 8
%_37drop_45right48066 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49606, i64 2)
store %struct.ScmObj* %_37drop_45right48066, %struct.ScmObj** %stackaddr$env-ref55696
%stackaddr$prim55697 = alloca %struct.ScmObj*, align 8
%k48366 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4807448365)
store volatile %struct.ScmObj* %k48366, %struct.ScmObj** %stackaddr$prim55697, align 8
%stackaddr$prim55698 = alloca %struct.ScmObj*, align 8
%args48074 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4807448365)
store volatile %struct.ScmObj* %args48074, %struct.ScmObj** %stackaddr$prim55698, align 8
%stackaddr$prim55699 = alloca %struct.ScmObj*, align 8
%f48076 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48074)
store volatile %struct.ScmObj* %f48076, %struct.ScmObj** %stackaddr$prim55699, align 8
%stackaddr$prim55700 = alloca %struct.ScmObj*, align 8
%lsts48075 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48074)
store volatile %struct.ScmObj* %lsts48075, %struct.ScmObj** %stackaddr$prim55700, align 8
%stackaddr$makeclosure55701 = alloca %struct.ScmObj*, align 8
%fptrToInt55702 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49611 to i64
%ae49611 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55702)
store volatile %struct.ScmObj* %ae49611, %struct.ScmObj** %stackaddr$makeclosure55701, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49611, %struct.ScmObj* %_37foldr48052, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49611, %struct.ScmObj* %k48366, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49611, %struct.ScmObj* %lsts48075, i64 2)
%ae49612 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55703 = alloca %struct.ScmObj*, align 8
%fptrToInt55704 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49613 to i64
%ae49613 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55704)
store volatile %struct.ScmObj* %ae49613, %struct.ScmObj** %stackaddr$makeclosure55703, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49613, %struct.ScmObj* %_37last48069, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49613, %struct.ScmObj* %_37drop_45right48066, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49613, %struct.ScmObj* %f48076, i64 2)
%argslist54421$ae496110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55705 = alloca %struct.ScmObj*, align 8
%argslist54421$ae496111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49613, %struct.ScmObj* %argslist54421$ae496110)
store volatile %struct.ScmObj* %argslist54421$ae496111, %struct.ScmObj** %stackaddr$prim55705, align 8
%stackaddr$prim55706 = alloca %struct.ScmObj*, align 8
%argslist54421$ae496112 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49612, %struct.ScmObj* %argslist54421$ae496111)
store volatile %struct.ScmObj* %argslist54421$ae496112, %struct.ScmObj** %stackaddr$prim55706, align 8
%clofunc55707 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49611)
musttail call tailcc void %clofunc55707(%struct.ScmObj* %ae49611, %struct.ScmObj* %argslist54421$ae496112)
ret void
}

define tailcc void @proc_clo$ae49611(%struct.ScmObj* %env$ae49611,%struct.ScmObj* %current_45args54406) {
%stackaddr$env-ref55708 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49611, i64 0)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref55708
%stackaddr$env-ref55709 = alloca %struct.ScmObj*, align 8
%k48366 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49611, i64 1)
store %struct.ScmObj* %k48366, %struct.ScmObj** %stackaddr$env-ref55709
%stackaddr$env-ref55710 = alloca %struct.ScmObj*, align 8
%lsts48075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49611, i64 2)
store %struct.ScmObj* %lsts48075, %struct.ScmObj** %stackaddr$env-ref55710
%stackaddr$prim55711 = alloca %struct.ScmObj*, align 8
%_95k48367 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54406)
store volatile %struct.ScmObj* %_95k48367, %struct.ScmObj** %stackaddr$prim55711, align 8
%stackaddr$prim55712 = alloca %struct.ScmObj*, align 8
%current_45args54407 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54406)
store volatile %struct.ScmObj* %current_45args54407, %struct.ScmObj** %stackaddr$prim55712, align 8
%stackaddr$prim55713 = alloca %struct.ScmObj*, align 8
%anf_45bind48202 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54407)
store volatile %struct.ScmObj* %anf_45bind48202, %struct.ScmObj** %stackaddr$prim55713, align 8
%ae49674 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55714 = alloca %struct.ScmObj*, align 8
%anf_45bind48203 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49674, %struct.ScmObj* %lsts48075)
store volatile %struct.ScmObj* %anf_45bind48203, %struct.ScmObj** %stackaddr$prim55714, align 8
%stackaddr$prim55715 = alloca %struct.ScmObj*, align 8
%anf_45bind48204 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48202, %struct.ScmObj* %anf_45bind48203)
store volatile %struct.ScmObj* %anf_45bind48204, %struct.ScmObj** %stackaddr$prim55715, align 8
%stackaddr$prim55716 = alloca %struct.ScmObj*, align 8
%cpsargs48368 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48366, %struct.ScmObj* %anf_45bind48204)
store volatile %struct.ScmObj* %cpsargs48368, %struct.ScmObj** %stackaddr$prim55716, align 8
%clofunc55717 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr48052)
musttail call tailcc void %clofunc55717(%struct.ScmObj* %_37foldr48052, %struct.ScmObj* %cpsargs48368)
ret void
}

define tailcc void @proc_clo$ae49613(%struct.ScmObj* %env$ae49613,%struct.ScmObj* %fargs4807748369) {
%stackaddr$env-ref55718 = alloca %struct.ScmObj*, align 8
%_37last48069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49613, i64 0)
store %struct.ScmObj* %_37last48069, %struct.ScmObj** %stackaddr$env-ref55718
%stackaddr$env-ref55719 = alloca %struct.ScmObj*, align 8
%_37drop_45right48066 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49613, i64 1)
store %struct.ScmObj* %_37drop_45right48066, %struct.ScmObj** %stackaddr$env-ref55719
%stackaddr$env-ref55720 = alloca %struct.ScmObj*, align 8
%f48076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49613, i64 2)
store %struct.ScmObj* %f48076, %struct.ScmObj** %stackaddr$env-ref55720
%stackaddr$prim55721 = alloca %struct.ScmObj*, align 8
%k48370 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs4807748369)
store volatile %struct.ScmObj* %k48370, %struct.ScmObj** %stackaddr$prim55721, align 8
%stackaddr$prim55722 = alloca %struct.ScmObj*, align 8
%fargs48077 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs4807748369)
store volatile %struct.ScmObj* %fargs48077, %struct.ScmObj** %stackaddr$prim55722, align 8
%stackaddr$makeclosure55723 = alloca %struct.ScmObj*, align 8
%fptrToInt55724 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49617 to i64
%ae49617 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55724)
store volatile %struct.ScmObj* %ae49617, %struct.ScmObj** %stackaddr$makeclosure55723, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49617, %struct.ScmObj* %_37last48069, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49617, %struct.ScmObj* %k48370, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49617, %struct.ScmObj* %fargs48077, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49617, %struct.ScmObj* %f48076, i64 3)
%ae49619 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist54420$_37drop_45right480660 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55725 = alloca %struct.ScmObj*, align 8
%argslist54420$_37drop_45right480661 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49619, %struct.ScmObj* %argslist54420$_37drop_45right480660)
store volatile %struct.ScmObj* %argslist54420$_37drop_45right480661, %struct.ScmObj** %stackaddr$prim55725, align 8
%stackaddr$prim55726 = alloca %struct.ScmObj*, align 8
%argslist54420$_37drop_45right480662 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs48077, %struct.ScmObj* %argslist54420$_37drop_45right480661)
store volatile %struct.ScmObj* %argslist54420$_37drop_45right480662, %struct.ScmObj** %stackaddr$prim55726, align 8
%stackaddr$prim55727 = alloca %struct.ScmObj*, align 8
%argslist54420$_37drop_45right480663 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49617, %struct.ScmObj* %argslist54420$_37drop_45right480662)
store volatile %struct.ScmObj* %argslist54420$_37drop_45right480663, %struct.ScmObj** %stackaddr$prim55727, align 8
%clofunc55728 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right48066)
musttail call tailcc void %clofunc55728(%struct.ScmObj* %_37drop_45right48066, %struct.ScmObj* %argslist54420$_37drop_45right480663)
ret void
}

define tailcc void @proc_clo$ae49617(%struct.ScmObj* %env$ae49617,%struct.ScmObj* %current_45args54409) {
%stackaddr$env-ref55729 = alloca %struct.ScmObj*, align 8
%_37last48069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49617, i64 0)
store %struct.ScmObj* %_37last48069, %struct.ScmObj** %stackaddr$env-ref55729
%stackaddr$env-ref55730 = alloca %struct.ScmObj*, align 8
%k48370 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49617, i64 1)
store %struct.ScmObj* %k48370, %struct.ScmObj** %stackaddr$env-ref55730
%stackaddr$env-ref55731 = alloca %struct.ScmObj*, align 8
%fargs48077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49617, i64 2)
store %struct.ScmObj* %fargs48077, %struct.ScmObj** %stackaddr$env-ref55731
%stackaddr$env-ref55732 = alloca %struct.ScmObj*, align 8
%f48076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49617, i64 3)
store %struct.ScmObj* %f48076, %struct.ScmObj** %stackaddr$env-ref55732
%stackaddr$prim55733 = alloca %struct.ScmObj*, align 8
%_95k48371 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54409)
store volatile %struct.ScmObj* %_95k48371, %struct.ScmObj** %stackaddr$prim55733, align 8
%stackaddr$prim55734 = alloca %struct.ScmObj*, align 8
%current_45args54410 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54409)
store volatile %struct.ScmObj* %current_45args54410, %struct.ScmObj** %stackaddr$prim55734, align 8
%stackaddr$prim55735 = alloca %struct.ScmObj*, align 8
%anf_45bind48199 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54410)
store volatile %struct.ScmObj* %anf_45bind48199, %struct.ScmObj** %stackaddr$prim55735, align 8
%stackaddr$makeclosure55736 = alloca %struct.ScmObj*, align 8
%fptrToInt55737 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49624 to i64
%ae49624 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55737)
store volatile %struct.ScmObj* %ae49624, %struct.ScmObj** %stackaddr$makeclosure55736, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49624, %struct.ScmObj* %_37last48069, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49624, %struct.ScmObj* %k48370, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49624, %struct.ScmObj* %fargs48077, i64 2)
%stackaddr$prim55738 = alloca %struct.ScmObj*, align 8
%cpsargs48375 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49624, %struct.ScmObj* %anf_45bind48199)
store volatile %struct.ScmObj* %cpsargs48375, %struct.ScmObj** %stackaddr$prim55738, align 8
%clofunc55739 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48076)
musttail call tailcc void %clofunc55739(%struct.ScmObj* %f48076, %struct.ScmObj* %cpsargs48375)
ret void
}

define tailcc void @proc_clo$ae49624(%struct.ScmObj* %env$ae49624,%struct.ScmObj* %current_45args54412) {
%stackaddr$env-ref55740 = alloca %struct.ScmObj*, align 8
%_37last48069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49624, i64 0)
store %struct.ScmObj* %_37last48069, %struct.ScmObj** %stackaddr$env-ref55740
%stackaddr$env-ref55741 = alloca %struct.ScmObj*, align 8
%k48370 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49624, i64 1)
store %struct.ScmObj* %k48370, %struct.ScmObj** %stackaddr$env-ref55741
%stackaddr$env-ref55742 = alloca %struct.ScmObj*, align 8
%fargs48077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49624, i64 2)
store %struct.ScmObj* %fargs48077, %struct.ScmObj** %stackaddr$env-ref55742
%stackaddr$prim55743 = alloca %struct.ScmObj*, align 8
%_95k48372 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54412)
store volatile %struct.ScmObj* %_95k48372, %struct.ScmObj** %stackaddr$prim55743, align 8
%stackaddr$prim55744 = alloca %struct.ScmObj*, align 8
%current_45args54413 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54412)
store volatile %struct.ScmObj* %current_45args54413, %struct.ScmObj** %stackaddr$prim55744, align 8
%stackaddr$prim55745 = alloca %struct.ScmObj*, align 8
%anf_45bind48200 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54413)
store volatile %struct.ScmObj* %anf_45bind48200, %struct.ScmObj** %stackaddr$prim55745, align 8
%stackaddr$makeclosure55746 = alloca %struct.ScmObj*, align 8
%fptrToInt55747 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49629 to i64
%ae49629 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55747)
store volatile %struct.ScmObj* %ae49629, %struct.ScmObj** %stackaddr$makeclosure55746, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49629, %struct.ScmObj* %anf_45bind48200, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49629, %struct.ScmObj* %k48370, i64 1)
%argslist54419$_37last480690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55748 = alloca %struct.ScmObj*, align 8
%argslist54419$_37last480691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs48077, %struct.ScmObj* %argslist54419$_37last480690)
store volatile %struct.ScmObj* %argslist54419$_37last480691, %struct.ScmObj** %stackaddr$prim55748, align 8
%stackaddr$prim55749 = alloca %struct.ScmObj*, align 8
%argslist54419$_37last480692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49629, %struct.ScmObj* %argslist54419$_37last480691)
store volatile %struct.ScmObj* %argslist54419$_37last480692, %struct.ScmObj** %stackaddr$prim55749, align 8
%clofunc55750 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last48069)
musttail call tailcc void %clofunc55750(%struct.ScmObj* %_37last48069, %struct.ScmObj* %argslist54419$_37last480692)
ret void
}

define tailcc void @proc_clo$ae49629(%struct.ScmObj* %env$ae49629,%struct.ScmObj* %current_45args54415) {
%stackaddr$env-ref55751 = alloca %struct.ScmObj*, align 8
%anf_45bind48200 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49629, i64 0)
store %struct.ScmObj* %anf_45bind48200, %struct.ScmObj** %stackaddr$env-ref55751
%stackaddr$env-ref55752 = alloca %struct.ScmObj*, align 8
%k48370 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49629, i64 1)
store %struct.ScmObj* %k48370, %struct.ScmObj** %stackaddr$env-ref55752
%stackaddr$prim55753 = alloca %struct.ScmObj*, align 8
%_95k48373 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54415)
store volatile %struct.ScmObj* %_95k48373, %struct.ScmObj** %stackaddr$prim55753, align 8
%stackaddr$prim55754 = alloca %struct.ScmObj*, align 8
%current_45args54416 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54415)
store volatile %struct.ScmObj* %current_45args54416, %struct.ScmObj** %stackaddr$prim55754, align 8
%stackaddr$prim55755 = alloca %struct.ScmObj*, align 8
%anf_45bind48201 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54416)
store volatile %struct.ScmObj* %anf_45bind48201, %struct.ScmObj** %stackaddr$prim55755, align 8
%stackaddr$prim55756 = alloca %struct.ScmObj*, align 8
%cpsprim48374 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48200, %struct.ScmObj* %anf_45bind48201)
store volatile %struct.ScmObj* %cpsprim48374, %struct.ScmObj** %stackaddr$prim55756, align 8
%ae49634 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54418$k483700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55757 = alloca %struct.ScmObj*, align 8
%argslist54418$k483701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48374, %struct.ScmObj* %argslist54418$k483700)
store volatile %struct.ScmObj* %argslist54418$k483701, %struct.ScmObj** %stackaddr$prim55757, align 8
%stackaddr$prim55758 = alloca %struct.ScmObj*, align 8
%argslist54418$k483702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49634, %struct.ScmObj* %argslist54418$k483701)
store volatile %struct.ScmObj* %argslist54418$k483702, %struct.ScmObj** %stackaddr$prim55758, align 8
%clofunc55759 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48370)
musttail call tailcc void %clofunc55759(%struct.ScmObj* %k48370, %struct.ScmObj* %argslist54418$k483702)
ret void
}

define tailcc void @proc_clo$ae49529(%struct.ScmObj* %env$ae49529,%struct.ScmObj* %current_45args54423) {
%stackaddr$env-ref55760 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49529, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55760
%stackaddr$prim55761 = alloca %struct.ScmObj*, align 8
%k48376 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54423)
store volatile %struct.ScmObj* %k48376, %struct.ScmObj** %stackaddr$prim55761, align 8
%stackaddr$prim55762 = alloca %struct.ScmObj*, align 8
%current_45args54424 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54423)
store volatile %struct.ScmObj* %current_45args54424, %struct.ScmObj** %stackaddr$prim55762, align 8
%stackaddr$prim55763 = alloca %struct.ScmObj*, align 8
%f48080 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54424)
store volatile %struct.ScmObj* %f48080, %struct.ScmObj** %stackaddr$prim55763, align 8
%stackaddr$prim55764 = alloca %struct.ScmObj*, align 8
%current_45args54425 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54424)
store volatile %struct.ScmObj* %current_45args54425, %struct.ScmObj** %stackaddr$prim55764, align 8
%stackaddr$prim55765 = alloca %struct.ScmObj*, align 8
%lst48079 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54425)
store volatile %struct.ScmObj* %lst48079, %struct.ScmObj** %stackaddr$prim55765, align 8
%stackaddr$makeclosure55766 = alloca %struct.ScmObj*, align 8
%fptrToInt55767 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49530 to i64
%ae49530 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55767)
store volatile %struct.ScmObj* %ae49530, %struct.ScmObj** %stackaddr$makeclosure55766, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49530, %struct.ScmObj* %lst48079, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49530, %struct.ScmObj* %_37foldr148047, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49530, %struct.ScmObj* %k48376, i64 2)
%ae49531 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55768 = alloca %struct.ScmObj*, align 8
%fptrToInt55769 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49532 to i64
%ae49532 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55769)
store volatile %struct.ScmObj* %ae49532, %struct.ScmObj** %stackaddr$makeclosure55768, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49532, %struct.ScmObj* %f48080, i64 0)
%argslist54440$ae495300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55770 = alloca %struct.ScmObj*, align 8
%argslist54440$ae495301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49532, %struct.ScmObj* %argslist54440$ae495300)
store volatile %struct.ScmObj* %argslist54440$ae495301, %struct.ScmObj** %stackaddr$prim55770, align 8
%stackaddr$prim55771 = alloca %struct.ScmObj*, align 8
%argslist54440$ae495302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49531, %struct.ScmObj* %argslist54440$ae495301)
store volatile %struct.ScmObj* %argslist54440$ae495302, %struct.ScmObj** %stackaddr$prim55771, align 8
%clofunc55772 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49530)
musttail call tailcc void %clofunc55772(%struct.ScmObj* %ae49530, %struct.ScmObj* %argslist54440$ae495302)
ret void
}

define tailcc void @proc_clo$ae49530(%struct.ScmObj* %env$ae49530,%struct.ScmObj* %current_45args54427) {
%stackaddr$env-ref55773 = alloca %struct.ScmObj*, align 8
%lst48079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49530, i64 0)
store %struct.ScmObj* %lst48079, %struct.ScmObj** %stackaddr$env-ref55773
%stackaddr$env-ref55774 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49530, i64 1)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55774
%stackaddr$env-ref55775 = alloca %struct.ScmObj*, align 8
%k48376 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49530, i64 2)
store %struct.ScmObj* %k48376, %struct.ScmObj** %stackaddr$env-ref55775
%stackaddr$prim55776 = alloca %struct.ScmObj*, align 8
%_95k48377 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54427)
store volatile %struct.ScmObj* %_95k48377, %struct.ScmObj** %stackaddr$prim55776, align 8
%stackaddr$prim55777 = alloca %struct.ScmObj*, align 8
%current_45args54428 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54427)
store volatile %struct.ScmObj* %current_45args54428, %struct.ScmObj** %stackaddr$prim55777, align 8
%stackaddr$prim55778 = alloca %struct.ScmObj*, align 8
%anf_45bind48198 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54428)
store volatile %struct.ScmObj* %anf_45bind48198, %struct.ScmObj** %stackaddr$prim55778, align 8
%ae49564 = call %struct.ScmObj* @const_init_null()
%argslist54430$_37foldr1480470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55779 = alloca %struct.ScmObj*, align 8
%argslist54430$_37foldr1480471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48079, %struct.ScmObj* %argslist54430$_37foldr1480470)
store volatile %struct.ScmObj* %argslist54430$_37foldr1480471, %struct.ScmObj** %stackaddr$prim55779, align 8
%stackaddr$prim55780 = alloca %struct.ScmObj*, align 8
%argslist54430$_37foldr1480472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49564, %struct.ScmObj* %argslist54430$_37foldr1480471)
store volatile %struct.ScmObj* %argslist54430$_37foldr1480472, %struct.ScmObj** %stackaddr$prim55780, align 8
%stackaddr$prim55781 = alloca %struct.ScmObj*, align 8
%argslist54430$_37foldr1480473 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48198, %struct.ScmObj* %argslist54430$_37foldr1480472)
store volatile %struct.ScmObj* %argslist54430$_37foldr1480473, %struct.ScmObj** %stackaddr$prim55781, align 8
%stackaddr$prim55782 = alloca %struct.ScmObj*, align 8
%argslist54430$_37foldr1480474 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48376, %struct.ScmObj* %argslist54430$_37foldr1480473)
store volatile %struct.ScmObj* %argslist54430$_37foldr1480474, %struct.ScmObj** %stackaddr$prim55782, align 8
%clofunc55783 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148047)
musttail call tailcc void %clofunc55783(%struct.ScmObj* %_37foldr148047, %struct.ScmObj* %argslist54430$_37foldr1480474)
ret void
}

define tailcc void @proc_clo$ae49532(%struct.ScmObj* %env$ae49532,%struct.ScmObj* %current_45args54431) {
%stackaddr$env-ref55784 = alloca %struct.ScmObj*, align 8
%f48080 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49532, i64 0)
store %struct.ScmObj* %f48080, %struct.ScmObj** %stackaddr$env-ref55784
%stackaddr$prim55785 = alloca %struct.ScmObj*, align 8
%k48378 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54431)
store volatile %struct.ScmObj* %k48378, %struct.ScmObj** %stackaddr$prim55785, align 8
%stackaddr$prim55786 = alloca %struct.ScmObj*, align 8
%current_45args54432 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54431)
store volatile %struct.ScmObj* %current_45args54432, %struct.ScmObj** %stackaddr$prim55786, align 8
%stackaddr$prim55787 = alloca %struct.ScmObj*, align 8
%v48082 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54432)
store volatile %struct.ScmObj* %v48082, %struct.ScmObj** %stackaddr$prim55787, align 8
%stackaddr$prim55788 = alloca %struct.ScmObj*, align 8
%current_45args54433 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54432)
store volatile %struct.ScmObj* %current_45args54433, %struct.ScmObj** %stackaddr$prim55788, align 8
%stackaddr$prim55789 = alloca %struct.ScmObj*, align 8
%r48081 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54433)
store volatile %struct.ScmObj* %r48081, %struct.ScmObj** %stackaddr$prim55789, align 8
%stackaddr$makeclosure55790 = alloca %struct.ScmObj*, align 8
%fptrToInt55791 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49534 to i64
%ae49534 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55791)
store volatile %struct.ScmObj* %ae49534, %struct.ScmObj** %stackaddr$makeclosure55790, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49534, %struct.ScmObj* %r48081, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49534, %struct.ScmObj* %k48378, i64 1)
%argslist54439$f480800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55792 = alloca %struct.ScmObj*, align 8
%argslist54439$f480801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v48082, %struct.ScmObj* %argslist54439$f480800)
store volatile %struct.ScmObj* %argslist54439$f480801, %struct.ScmObj** %stackaddr$prim55792, align 8
%stackaddr$prim55793 = alloca %struct.ScmObj*, align 8
%argslist54439$f480802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49534, %struct.ScmObj* %argslist54439$f480801)
store volatile %struct.ScmObj* %argslist54439$f480802, %struct.ScmObj** %stackaddr$prim55793, align 8
%clofunc55794 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48080)
musttail call tailcc void %clofunc55794(%struct.ScmObj* %f48080, %struct.ScmObj* %argslist54439$f480802)
ret void
}

define tailcc void @proc_clo$ae49534(%struct.ScmObj* %env$ae49534,%struct.ScmObj* %current_45args54435) {
%stackaddr$env-ref55795 = alloca %struct.ScmObj*, align 8
%r48081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49534, i64 0)
store %struct.ScmObj* %r48081, %struct.ScmObj** %stackaddr$env-ref55795
%stackaddr$env-ref55796 = alloca %struct.ScmObj*, align 8
%k48378 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49534, i64 1)
store %struct.ScmObj* %k48378, %struct.ScmObj** %stackaddr$env-ref55796
%stackaddr$prim55797 = alloca %struct.ScmObj*, align 8
%_95k48379 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54435)
store volatile %struct.ScmObj* %_95k48379, %struct.ScmObj** %stackaddr$prim55797, align 8
%stackaddr$prim55798 = alloca %struct.ScmObj*, align 8
%current_45args54436 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54435)
store volatile %struct.ScmObj* %current_45args54436, %struct.ScmObj** %stackaddr$prim55798, align 8
%stackaddr$prim55799 = alloca %struct.ScmObj*, align 8
%anf_45bind48197 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54436)
store volatile %struct.ScmObj* %anf_45bind48197, %struct.ScmObj** %stackaddr$prim55799, align 8
%stackaddr$prim55800 = alloca %struct.ScmObj*, align 8
%cpsprim48380 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48197, %struct.ScmObj* %r48081)
store volatile %struct.ScmObj* %cpsprim48380, %struct.ScmObj** %stackaddr$prim55800, align 8
%ae49539 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54438$k483780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55801 = alloca %struct.ScmObj*, align 8
%argslist54438$k483781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48380, %struct.ScmObj* %argslist54438$k483780)
store volatile %struct.ScmObj* %argslist54438$k483781, %struct.ScmObj** %stackaddr$prim55801, align 8
%stackaddr$prim55802 = alloca %struct.ScmObj*, align 8
%argslist54438$k483782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49539, %struct.ScmObj* %argslist54438$k483781)
store volatile %struct.ScmObj* %argslist54438$k483782, %struct.ScmObj** %stackaddr$prim55802, align 8
%clofunc55803 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48378)
musttail call tailcc void %clofunc55803(%struct.ScmObj* %k48378, %struct.ScmObj* %argslist54438$k483782)
ret void
}

define tailcc void @proc_clo$ae49143(%struct.ScmObj* %env$ae49143,%struct.ScmObj* %current_45args54443) {
%stackaddr$env-ref55804 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49143, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55804
%stackaddr$env-ref55805 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49143, i64 1)
store %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$env-ref55805
%stackaddr$prim55806 = alloca %struct.ScmObj*, align 8
%k48381 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54443)
store volatile %struct.ScmObj* %k48381, %struct.ScmObj** %stackaddr$prim55806, align 8
%stackaddr$prim55807 = alloca %struct.ScmObj*, align 8
%current_45args54444 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54443)
store volatile %struct.ScmObj* %current_45args54444, %struct.ScmObj** %stackaddr$prim55807, align 8
%stackaddr$prim55808 = alloca %struct.ScmObj*, align 8
%_37foldr48053 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54444)
store volatile %struct.ScmObj* %_37foldr48053, %struct.ScmObj** %stackaddr$prim55808, align 8
%ae49145 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55809 = alloca %struct.ScmObj*, align 8
%fptrToInt55810 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49146 to i64
%ae49146 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55810)
store volatile %struct.ScmObj* %ae49146, %struct.ScmObj** %stackaddr$makeclosure55809, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49146, %struct.ScmObj* %_37foldr48053, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49146, %struct.ScmObj* %_37foldr148047, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49146, %struct.ScmObj* %_37map148043, i64 2)
%argslist54501$k483810 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55811 = alloca %struct.ScmObj*, align 8
%argslist54501$k483811 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49146, %struct.ScmObj* %argslist54501$k483810)
store volatile %struct.ScmObj* %argslist54501$k483811, %struct.ScmObj** %stackaddr$prim55811, align 8
%stackaddr$prim55812 = alloca %struct.ScmObj*, align 8
%argslist54501$k483812 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49145, %struct.ScmObj* %argslist54501$k483811)
store volatile %struct.ScmObj* %argslist54501$k483812, %struct.ScmObj** %stackaddr$prim55812, align 8
%clofunc55813 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48381)
musttail call tailcc void %clofunc55813(%struct.ScmObj* %k48381, %struct.ScmObj* %argslist54501$k483812)
ret void
}

define tailcc void @proc_clo$ae49146(%struct.ScmObj* %env$ae49146,%struct.ScmObj* %args4805448382) {
%stackaddr$env-ref55814 = alloca %struct.ScmObj*, align 8
%_37foldr48053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49146, i64 0)
store %struct.ScmObj* %_37foldr48053, %struct.ScmObj** %stackaddr$env-ref55814
%stackaddr$env-ref55815 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49146, i64 1)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55815
%stackaddr$env-ref55816 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49146, i64 2)
store %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$env-ref55816
%stackaddr$prim55817 = alloca %struct.ScmObj*, align 8
%k48383 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4805448382)
store volatile %struct.ScmObj* %k48383, %struct.ScmObj** %stackaddr$prim55817, align 8
%stackaddr$prim55818 = alloca %struct.ScmObj*, align 8
%args48054 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4805448382)
store volatile %struct.ScmObj* %args48054, %struct.ScmObj** %stackaddr$prim55818, align 8
%stackaddr$prim55819 = alloca %struct.ScmObj*, align 8
%f48057 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48054)
store volatile %struct.ScmObj* %f48057, %struct.ScmObj** %stackaddr$prim55819, align 8
%stackaddr$prim55820 = alloca %struct.ScmObj*, align 8
%anf_45bind48184 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48054)
store volatile %struct.ScmObj* %anf_45bind48184, %struct.ScmObj** %stackaddr$prim55820, align 8
%stackaddr$prim55821 = alloca %struct.ScmObj*, align 8
%acc48056 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48184)
store volatile %struct.ScmObj* %acc48056, %struct.ScmObj** %stackaddr$prim55821, align 8
%stackaddr$prim55822 = alloca %struct.ScmObj*, align 8
%anf_45bind48185 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48054)
store volatile %struct.ScmObj* %anf_45bind48185, %struct.ScmObj** %stackaddr$prim55822, align 8
%stackaddr$prim55823 = alloca %struct.ScmObj*, align 8
%lsts48055 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48185)
store volatile %struct.ScmObj* %lsts48055, %struct.ScmObj** %stackaddr$prim55823, align 8
%stackaddr$makeclosure55824 = alloca %struct.ScmObj*, align 8
%fptrToInt55825 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49154 to i64
%ae49154 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55825)
store volatile %struct.ScmObj* %ae49154, %struct.ScmObj** %stackaddr$makeclosure55824, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49154, %struct.ScmObj* %k48383, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49154, %struct.ScmObj* %_37foldr148047, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49154, %struct.ScmObj* %f48057, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49154, %struct.ScmObj* %acc48056, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49154, %struct.ScmObj* %lsts48055, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49154, %struct.ScmObj* %_37foldr48053, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49154, %struct.ScmObj* %_37map148043, i64 6)
%ae49155 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55826 = alloca %struct.ScmObj*, align 8
%fptrToInt55827 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49156 to i64
%ae49156 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55827)
store volatile %struct.ScmObj* %ae49156, %struct.ScmObj** %stackaddr$makeclosure55826, align 8
%argslist54500$ae491540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55828 = alloca %struct.ScmObj*, align 8
%argslist54500$ae491541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49156, %struct.ScmObj* %argslist54500$ae491540)
store volatile %struct.ScmObj* %argslist54500$ae491541, %struct.ScmObj** %stackaddr$prim55828, align 8
%stackaddr$prim55829 = alloca %struct.ScmObj*, align 8
%argslist54500$ae491542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49155, %struct.ScmObj* %argslist54500$ae491541)
store volatile %struct.ScmObj* %argslist54500$ae491542, %struct.ScmObj** %stackaddr$prim55829, align 8
%clofunc55830 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49154)
musttail call tailcc void %clofunc55830(%struct.ScmObj* %ae49154, %struct.ScmObj* %argslist54500$ae491542)
ret void
}

define tailcc void @proc_clo$ae49154(%struct.ScmObj* %env$ae49154,%struct.ScmObj* %current_45args54446) {
%stackaddr$env-ref55831 = alloca %struct.ScmObj*, align 8
%k48383 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49154, i64 0)
store %struct.ScmObj* %k48383, %struct.ScmObj** %stackaddr$env-ref55831
%stackaddr$env-ref55832 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49154, i64 1)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55832
%stackaddr$env-ref55833 = alloca %struct.ScmObj*, align 8
%f48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49154, i64 2)
store %struct.ScmObj* %f48057, %struct.ScmObj** %stackaddr$env-ref55833
%stackaddr$env-ref55834 = alloca %struct.ScmObj*, align 8
%acc48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49154, i64 3)
store %struct.ScmObj* %acc48056, %struct.ScmObj** %stackaddr$env-ref55834
%stackaddr$env-ref55835 = alloca %struct.ScmObj*, align 8
%lsts48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49154, i64 4)
store %struct.ScmObj* %lsts48055, %struct.ScmObj** %stackaddr$env-ref55835
%stackaddr$env-ref55836 = alloca %struct.ScmObj*, align 8
%_37foldr48053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49154, i64 5)
store %struct.ScmObj* %_37foldr48053, %struct.ScmObj** %stackaddr$env-ref55836
%stackaddr$env-ref55837 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49154, i64 6)
store %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$env-ref55837
%stackaddr$prim55838 = alloca %struct.ScmObj*, align 8
%_95k48384 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54446)
store volatile %struct.ScmObj* %_95k48384, %struct.ScmObj** %stackaddr$prim55838, align 8
%stackaddr$prim55839 = alloca %struct.ScmObj*, align 8
%current_45args54447 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54446)
store volatile %struct.ScmObj* %current_45args54447, %struct.ScmObj** %stackaddr$prim55839, align 8
%stackaddr$prim55840 = alloca %struct.ScmObj*, align 8
%anf_45bind48186 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54447)
store volatile %struct.ScmObj* %anf_45bind48186, %struct.ScmObj** %stackaddr$prim55840, align 8
%stackaddr$makeclosure55841 = alloca %struct.ScmObj*, align 8
%fptrToInt55842 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49186 to i64
%ae49186 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55842)
store volatile %struct.ScmObj* %ae49186, %struct.ScmObj** %stackaddr$makeclosure55841, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49186, %struct.ScmObj* %k48383, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49186, %struct.ScmObj* %_37foldr148047, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49186, %struct.ScmObj* %f48057, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49186, %struct.ScmObj* %acc48056, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49186, %struct.ScmObj* %lsts48055, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49186, %struct.ScmObj* %_37foldr48053, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49186, %struct.ScmObj* %_37map148043, i64 6)
%ae49188 = call %struct.ScmObj* @const_init_false()
%argslist54493$_37foldr1480470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55843 = alloca %struct.ScmObj*, align 8
%argslist54493$_37foldr1480471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48055, %struct.ScmObj* %argslist54493$_37foldr1480470)
store volatile %struct.ScmObj* %argslist54493$_37foldr1480471, %struct.ScmObj** %stackaddr$prim55843, align 8
%stackaddr$prim55844 = alloca %struct.ScmObj*, align 8
%argslist54493$_37foldr1480472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49188, %struct.ScmObj* %argslist54493$_37foldr1480471)
store volatile %struct.ScmObj* %argslist54493$_37foldr1480472, %struct.ScmObj** %stackaddr$prim55844, align 8
%stackaddr$prim55845 = alloca %struct.ScmObj*, align 8
%argslist54493$_37foldr1480473 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48186, %struct.ScmObj* %argslist54493$_37foldr1480472)
store volatile %struct.ScmObj* %argslist54493$_37foldr1480473, %struct.ScmObj** %stackaddr$prim55845, align 8
%stackaddr$prim55846 = alloca %struct.ScmObj*, align 8
%argslist54493$_37foldr1480474 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49186, %struct.ScmObj* %argslist54493$_37foldr1480473)
store volatile %struct.ScmObj* %argslist54493$_37foldr1480474, %struct.ScmObj** %stackaddr$prim55846, align 8
%clofunc55847 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148047)
musttail call tailcc void %clofunc55847(%struct.ScmObj* %_37foldr148047, %struct.ScmObj* %argslist54493$_37foldr1480474)
ret void
}

define tailcc void @proc_clo$ae49186(%struct.ScmObj* %env$ae49186,%struct.ScmObj* %current_45args54449) {
%stackaddr$env-ref55848 = alloca %struct.ScmObj*, align 8
%k48383 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49186, i64 0)
store %struct.ScmObj* %k48383, %struct.ScmObj** %stackaddr$env-ref55848
%stackaddr$env-ref55849 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49186, i64 1)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55849
%stackaddr$env-ref55850 = alloca %struct.ScmObj*, align 8
%f48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49186, i64 2)
store %struct.ScmObj* %f48057, %struct.ScmObj** %stackaddr$env-ref55850
%stackaddr$env-ref55851 = alloca %struct.ScmObj*, align 8
%acc48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49186, i64 3)
store %struct.ScmObj* %acc48056, %struct.ScmObj** %stackaddr$env-ref55851
%stackaddr$env-ref55852 = alloca %struct.ScmObj*, align 8
%lsts48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49186, i64 4)
store %struct.ScmObj* %lsts48055, %struct.ScmObj** %stackaddr$env-ref55852
%stackaddr$env-ref55853 = alloca %struct.ScmObj*, align 8
%_37foldr48053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49186, i64 5)
store %struct.ScmObj* %_37foldr48053, %struct.ScmObj** %stackaddr$env-ref55853
%stackaddr$env-ref55854 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49186, i64 6)
store %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$env-ref55854
%stackaddr$prim55855 = alloca %struct.ScmObj*, align 8
%_95k48385 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54449)
store volatile %struct.ScmObj* %_95k48385, %struct.ScmObj** %stackaddr$prim55855, align 8
%stackaddr$prim55856 = alloca %struct.ScmObj*, align 8
%current_45args54450 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54449)
store volatile %struct.ScmObj* %current_45args54450, %struct.ScmObj** %stackaddr$prim55856, align 8
%stackaddr$prim55857 = alloca %struct.ScmObj*, align 8
%anf_45bind48187 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54450)
store volatile %struct.ScmObj* %anf_45bind48187, %struct.ScmObj** %stackaddr$prim55857, align 8
%truthy$cmp55858 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48187)
%cmp$cmp55858 = icmp eq i64 %truthy$cmp55858, 1
br i1 %cmp$cmp55858, label %truebranch$cmp55858, label %falsebranch$cmp55858
truebranch$cmp55858:
%ae49197 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54452$k483830 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55859 = alloca %struct.ScmObj*, align 8
%argslist54452$k483831 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48056, %struct.ScmObj* %argslist54452$k483830)
store volatile %struct.ScmObj* %argslist54452$k483831, %struct.ScmObj** %stackaddr$prim55859, align 8
%stackaddr$prim55860 = alloca %struct.ScmObj*, align 8
%argslist54452$k483832 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49197, %struct.ScmObj* %argslist54452$k483831)
store volatile %struct.ScmObj* %argslist54452$k483832, %struct.ScmObj** %stackaddr$prim55860, align 8
%clofunc55861 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48383)
musttail call tailcc void %clofunc55861(%struct.ScmObj* %k48383, %struct.ScmObj* %argslist54452$k483832)
ret void
falsebranch$cmp55858:
%stackaddr$makeclosure55862 = alloca %struct.ScmObj*, align 8
%fptrToInt55863 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49202 to i64
%ae49202 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55863)
store volatile %struct.ScmObj* %ae49202, %struct.ScmObj** %stackaddr$makeclosure55862, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49202, %struct.ScmObj* %k48383, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49202, %struct.ScmObj* %_37foldr148047, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49202, %struct.ScmObj* %f48057, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49202, %struct.ScmObj* %acc48056, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49202, %struct.ScmObj* %lsts48055, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49202, %struct.ScmObj* %_37foldr48053, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49202, %struct.ScmObj* %_37map148043, i64 6)
%ae49203 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55864 = alloca %struct.ScmObj*, align 8
%fptrToInt55865 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49204 to i64
%ae49204 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55865)
store volatile %struct.ScmObj* %ae49204, %struct.ScmObj** %stackaddr$makeclosure55864, align 8
%argslist54492$ae492020 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55866 = alloca %struct.ScmObj*, align 8
%argslist54492$ae492021 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49204, %struct.ScmObj* %argslist54492$ae492020)
store volatile %struct.ScmObj* %argslist54492$ae492021, %struct.ScmObj** %stackaddr$prim55866, align 8
%stackaddr$prim55867 = alloca %struct.ScmObj*, align 8
%argslist54492$ae492022 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49203, %struct.ScmObj* %argslist54492$ae492021)
store volatile %struct.ScmObj* %argslist54492$ae492022, %struct.ScmObj** %stackaddr$prim55867, align 8
%clofunc55868 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49202)
musttail call tailcc void %clofunc55868(%struct.ScmObj* %ae49202, %struct.ScmObj* %argslist54492$ae492022)
ret void
}

define tailcc void @proc_clo$ae49202(%struct.ScmObj* %env$ae49202,%struct.ScmObj* %current_45args54453) {
%stackaddr$env-ref55869 = alloca %struct.ScmObj*, align 8
%k48383 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49202, i64 0)
store %struct.ScmObj* %k48383, %struct.ScmObj** %stackaddr$env-ref55869
%stackaddr$env-ref55870 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49202, i64 1)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55870
%stackaddr$env-ref55871 = alloca %struct.ScmObj*, align 8
%f48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49202, i64 2)
store %struct.ScmObj* %f48057, %struct.ScmObj** %stackaddr$env-ref55871
%stackaddr$env-ref55872 = alloca %struct.ScmObj*, align 8
%acc48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49202, i64 3)
store %struct.ScmObj* %acc48056, %struct.ScmObj** %stackaddr$env-ref55872
%stackaddr$env-ref55873 = alloca %struct.ScmObj*, align 8
%lsts48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49202, i64 4)
store %struct.ScmObj* %lsts48055, %struct.ScmObj** %stackaddr$env-ref55873
%stackaddr$env-ref55874 = alloca %struct.ScmObj*, align 8
%_37foldr48053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49202, i64 5)
store %struct.ScmObj* %_37foldr48053, %struct.ScmObj** %stackaddr$env-ref55874
%stackaddr$env-ref55875 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49202, i64 6)
store %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$env-ref55875
%stackaddr$prim55876 = alloca %struct.ScmObj*, align 8
%_95k48386 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54453)
store volatile %struct.ScmObj* %_95k48386, %struct.ScmObj** %stackaddr$prim55876, align 8
%stackaddr$prim55877 = alloca %struct.ScmObj*, align 8
%current_45args54454 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54453)
store volatile %struct.ScmObj* %current_45args54454, %struct.ScmObj** %stackaddr$prim55877, align 8
%stackaddr$prim55878 = alloca %struct.ScmObj*, align 8
%anf_45bind48188 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54454)
store volatile %struct.ScmObj* %anf_45bind48188, %struct.ScmObj** %stackaddr$prim55878, align 8
%stackaddr$makeclosure55879 = alloca %struct.ScmObj*, align 8
%fptrToInt55880 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49223 to i64
%ae49223 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55880)
store volatile %struct.ScmObj* %ae49223, %struct.ScmObj** %stackaddr$makeclosure55879, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49223, %struct.ScmObj* %k48383, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49223, %struct.ScmObj* %_37foldr148047, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49223, %struct.ScmObj* %f48057, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49223, %struct.ScmObj* %acc48056, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49223, %struct.ScmObj* %lsts48055, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49223, %struct.ScmObj* %_37foldr48053, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49223, %struct.ScmObj* %_37map148043, i64 6)
%argslist54487$_37map1480430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55881 = alloca %struct.ScmObj*, align 8
%argslist54487$_37map1480431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48055, %struct.ScmObj* %argslist54487$_37map1480430)
store volatile %struct.ScmObj* %argslist54487$_37map1480431, %struct.ScmObj** %stackaddr$prim55881, align 8
%stackaddr$prim55882 = alloca %struct.ScmObj*, align 8
%argslist54487$_37map1480432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48188, %struct.ScmObj* %argslist54487$_37map1480431)
store volatile %struct.ScmObj* %argslist54487$_37map1480432, %struct.ScmObj** %stackaddr$prim55882, align 8
%stackaddr$prim55883 = alloca %struct.ScmObj*, align 8
%argslist54487$_37map1480433 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49223, %struct.ScmObj* %argslist54487$_37map1480432)
store volatile %struct.ScmObj* %argslist54487$_37map1480433, %struct.ScmObj** %stackaddr$prim55883, align 8
%clofunc55884 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148043)
musttail call tailcc void %clofunc55884(%struct.ScmObj* %_37map148043, %struct.ScmObj* %argslist54487$_37map1480433)
ret void
}

define tailcc void @proc_clo$ae49223(%struct.ScmObj* %env$ae49223,%struct.ScmObj* %current_45args54456) {
%stackaddr$env-ref55885 = alloca %struct.ScmObj*, align 8
%k48383 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49223, i64 0)
store %struct.ScmObj* %k48383, %struct.ScmObj** %stackaddr$env-ref55885
%stackaddr$env-ref55886 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49223, i64 1)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55886
%stackaddr$env-ref55887 = alloca %struct.ScmObj*, align 8
%f48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49223, i64 2)
store %struct.ScmObj* %f48057, %struct.ScmObj** %stackaddr$env-ref55887
%stackaddr$env-ref55888 = alloca %struct.ScmObj*, align 8
%acc48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49223, i64 3)
store %struct.ScmObj* %acc48056, %struct.ScmObj** %stackaddr$env-ref55888
%stackaddr$env-ref55889 = alloca %struct.ScmObj*, align 8
%lsts48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49223, i64 4)
store %struct.ScmObj* %lsts48055, %struct.ScmObj** %stackaddr$env-ref55889
%stackaddr$env-ref55890 = alloca %struct.ScmObj*, align 8
%_37foldr48053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49223, i64 5)
store %struct.ScmObj* %_37foldr48053, %struct.ScmObj** %stackaddr$env-ref55890
%stackaddr$env-ref55891 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49223, i64 6)
store %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$env-ref55891
%stackaddr$prim55892 = alloca %struct.ScmObj*, align 8
%_95k48387 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54456)
store volatile %struct.ScmObj* %_95k48387, %struct.ScmObj** %stackaddr$prim55892, align 8
%stackaddr$prim55893 = alloca %struct.ScmObj*, align 8
%current_45args54457 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54456)
store volatile %struct.ScmObj* %current_45args54457, %struct.ScmObj** %stackaddr$prim55893, align 8
%stackaddr$prim55894 = alloca %struct.ScmObj*, align 8
%lsts_4348062 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54457)
store volatile %struct.ScmObj* %lsts_4348062, %struct.ScmObj** %stackaddr$prim55894, align 8
%stackaddr$makeclosure55895 = alloca %struct.ScmObj*, align 8
%fptrToInt55896 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49226 to i64
%ae49226 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt55896)
store volatile %struct.ScmObj* %ae49226, %struct.ScmObj** %stackaddr$makeclosure55895, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49226, %struct.ScmObj* %k48383, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49226, %struct.ScmObj* %_37foldr148047, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49226, %struct.ScmObj* %f48057, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49226, %struct.ScmObj* %acc48056, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49226, %struct.ScmObj* %lsts48055, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49226, %struct.ScmObj* %_37foldr48053, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49226, %struct.ScmObj* %lsts_4348062, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae49226, %struct.ScmObj* %_37map148043, i64 7)
%ae49227 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55897 = alloca %struct.ScmObj*, align 8
%fptrToInt55898 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49228 to i64
%ae49228 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55898)
store volatile %struct.ScmObj* %ae49228, %struct.ScmObj** %stackaddr$makeclosure55897, align 8
%argslist54486$ae492260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55899 = alloca %struct.ScmObj*, align 8
%argslist54486$ae492261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49228, %struct.ScmObj* %argslist54486$ae492260)
store volatile %struct.ScmObj* %argslist54486$ae492261, %struct.ScmObj** %stackaddr$prim55899, align 8
%stackaddr$prim55900 = alloca %struct.ScmObj*, align 8
%argslist54486$ae492262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49227, %struct.ScmObj* %argslist54486$ae492261)
store volatile %struct.ScmObj* %argslist54486$ae492262, %struct.ScmObj** %stackaddr$prim55900, align 8
%clofunc55901 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49226)
musttail call tailcc void %clofunc55901(%struct.ScmObj* %ae49226, %struct.ScmObj* %argslist54486$ae492262)
ret void
}

define tailcc void @proc_clo$ae49226(%struct.ScmObj* %env$ae49226,%struct.ScmObj* %current_45args54459) {
%stackaddr$env-ref55902 = alloca %struct.ScmObj*, align 8
%k48383 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49226, i64 0)
store %struct.ScmObj* %k48383, %struct.ScmObj** %stackaddr$env-ref55902
%stackaddr$env-ref55903 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49226, i64 1)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55903
%stackaddr$env-ref55904 = alloca %struct.ScmObj*, align 8
%f48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49226, i64 2)
store %struct.ScmObj* %f48057, %struct.ScmObj** %stackaddr$env-ref55904
%stackaddr$env-ref55905 = alloca %struct.ScmObj*, align 8
%acc48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49226, i64 3)
store %struct.ScmObj* %acc48056, %struct.ScmObj** %stackaddr$env-ref55905
%stackaddr$env-ref55906 = alloca %struct.ScmObj*, align 8
%lsts48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49226, i64 4)
store %struct.ScmObj* %lsts48055, %struct.ScmObj** %stackaddr$env-ref55906
%stackaddr$env-ref55907 = alloca %struct.ScmObj*, align 8
%_37foldr48053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49226, i64 5)
store %struct.ScmObj* %_37foldr48053, %struct.ScmObj** %stackaddr$env-ref55907
%stackaddr$env-ref55908 = alloca %struct.ScmObj*, align 8
%lsts_4348062 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49226, i64 6)
store %struct.ScmObj* %lsts_4348062, %struct.ScmObj** %stackaddr$env-ref55908
%stackaddr$env-ref55909 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49226, i64 7)
store %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$env-ref55909
%stackaddr$prim55910 = alloca %struct.ScmObj*, align 8
%_95k48388 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54459)
store volatile %struct.ScmObj* %_95k48388, %struct.ScmObj** %stackaddr$prim55910, align 8
%stackaddr$prim55911 = alloca %struct.ScmObj*, align 8
%current_45args54460 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54459)
store volatile %struct.ScmObj* %current_45args54460, %struct.ScmObj** %stackaddr$prim55911, align 8
%stackaddr$prim55912 = alloca %struct.ScmObj*, align 8
%anf_45bind48189 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54460)
store volatile %struct.ScmObj* %anf_45bind48189, %struct.ScmObj** %stackaddr$prim55912, align 8
%stackaddr$makeclosure55913 = alloca %struct.ScmObj*, align 8
%fptrToInt55914 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49247 to i64
%ae49247 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt55914)
store volatile %struct.ScmObj* %ae49247, %struct.ScmObj** %stackaddr$makeclosure55913, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49247, %struct.ScmObj* %k48383, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49247, %struct.ScmObj* %_37foldr148047, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49247, %struct.ScmObj* %f48057, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49247, %struct.ScmObj* %acc48056, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49247, %struct.ScmObj* %_37foldr48053, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49247, %struct.ScmObj* %lsts_4348062, i64 5)
%argslist54481$_37map1480430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55915 = alloca %struct.ScmObj*, align 8
%argslist54481$_37map1480431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48055, %struct.ScmObj* %argslist54481$_37map1480430)
store volatile %struct.ScmObj* %argslist54481$_37map1480431, %struct.ScmObj** %stackaddr$prim55915, align 8
%stackaddr$prim55916 = alloca %struct.ScmObj*, align 8
%argslist54481$_37map1480432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48189, %struct.ScmObj* %argslist54481$_37map1480431)
store volatile %struct.ScmObj* %argslist54481$_37map1480432, %struct.ScmObj** %stackaddr$prim55916, align 8
%stackaddr$prim55917 = alloca %struct.ScmObj*, align 8
%argslist54481$_37map1480433 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49247, %struct.ScmObj* %argslist54481$_37map1480432)
store volatile %struct.ScmObj* %argslist54481$_37map1480433, %struct.ScmObj** %stackaddr$prim55917, align 8
%clofunc55918 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148043)
musttail call tailcc void %clofunc55918(%struct.ScmObj* %_37map148043, %struct.ScmObj* %argslist54481$_37map1480433)
ret void
}

define tailcc void @proc_clo$ae49247(%struct.ScmObj* %env$ae49247,%struct.ScmObj* %current_45args54462) {
%stackaddr$env-ref55919 = alloca %struct.ScmObj*, align 8
%k48383 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49247, i64 0)
store %struct.ScmObj* %k48383, %struct.ScmObj** %stackaddr$env-ref55919
%stackaddr$env-ref55920 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49247, i64 1)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55920
%stackaddr$env-ref55921 = alloca %struct.ScmObj*, align 8
%f48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49247, i64 2)
store %struct.ScmObj* %f48057, %struct.ScmObj** %stackaddr$env-ref55921
%stackaddr$env-ref55922 = alloca %struct.ScmObj*, align 8
%acc48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49247, i64 3)
store %struct.ScmObj* %acc48056, %struct.ScmObj** %stackaddr$env-ref55922
%stackaddr$env-ref55923 = alloca %struct.ScmObj*, align 8
%_37foldr48053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49247, i64 4)
store %struct.ScmObj* %_37foldr48053, %struct.ScmObj** %stackaddr$env-ref55923
%stackaddr$env-ref55924 = alloca %struct.ScmObj*, align 8
%lsts_4348062 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49247, i64 5)
store %struct.ScmObj* %lsts_4348062, %struct.ScmObj** %stackaddr$env-ref55924
%stackaddr$prim55925 = alloca %struct.ScmObj*, align 8
%_95k48389 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54462)
store volatile %struct.ScmObj* %_95k48389, %struct.ScmObj** %stackaddr$prim55925, align 8
%stackaddr$prim55926 = alloca %struct.ScmObj*, align 8
%current_45args54463 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54462)
store volatile %struct.ScmObj* %current_45args54463, %struct.ScmObj** %stackaddr$prim55926, align 8
%stackaddr$prim55927 = alloca %struct.ScmObj*, align 8
%vs48060 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54463)
store volatile %struct.ScmObj* %vs48060, %struct.ScmObj** %stackaddr$prim55927, align 8
%stackaddr$makeclosure55928 = alloca %struct.ScmObj*, align 8
%fptrToInt55929 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49250 to i64
%ae49250 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55929)
store volatile %struct.ScmObj* %ae49250, %struct.ScmObj** %stackaddr$makeclosure55928, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49250, %struct.ScmObj* %k48383, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49250, %struct.ScmObj* %_37foldr148047, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49250, %struct.ScmObj* %f48057, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49250, %struct.ScmObj* %acc48056, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49250, %struct.ScmObj* %_37foldr48053, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49250, %struct.ScmObj* %lsts_4348062, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49250, %struct.ScmObj* %vs48060, i64 6)
%ae49251 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55930 = alloca %struct.ScmObj*, align 8
%fptrToInt55931 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49252 to i64
%ae49252 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55931)
store volatile %struct.ScmObj* %ae49252, %struct.ScmObj** %stackaddr$makeclosure55930, align 8
%argslist54480$ae492500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55932 = alloca %struct.ScmObj*, align 8
%argslist54480$ae492501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49252, %struct.ScmObj* %argslist54480$ae492500)
store volatile %struct.ScmObj* %argslist54480$ae492501, %struct.ScmObj** %stackaddr$prim55932, align 8
%stackaddr$prim55933 = alloca %struct.ScmObj*, align 8
%argslist54480$ae492502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49251, %struct.ScmObj* %argslist54480$ae492501)
store volatile %struct.ScmObj* %argslist54480$ae492502, %struct.ScmObj** %stackaddr$prim55933, align 8
%clofunc55934 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49250)
musttail call tailcc void %clofunc55934(%struct.ScmObj* %ae49250, %struct.ScmObj* %argslist54480$ae492502)
ret void
}

define tailcc void @proc_clo$ae49250(%struct.ScmObj* %env$ae49250,%struct.ScmObj* %current_45args54465) {
%stackaddr$env-ref55935 = alloca %struct.ScmObj*, align 8
%k48383 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49250, i64 0)
store %struct.ScmObj* %k48383, %struct.ScmObj** %stackaddr$env-ref55935
%stackaddr$env-ref55936 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49250, i64 1)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55936
%stackaddr$env-ref55937 = alloca %struct.ScmObj*, align 8
%f48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49250, i64 2)
store %struct.ScmObj* %f48057, %struct.ScmObj** %stackaddr$env-ref55937
%stackaddr$env-ref55938 = alloca %struct.ScmObj*, align 8
%acc48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49250, i64 3)
store %struct.ScmObj* %acc48056, %struct.ScmObj** %stackaddr$env-ref55938
%stackaddr$env-ref55939 = alloca %struct.ScmObj*, align 8
%_37foldr48053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49250, i64 4)
store %struct.ScmObj* %_37foldr48053, %struct.ScmObj** %stackaddr$env-ref55939
%stackaddr$env-ref55940 = alloca %struct.ScmObj*, align 8
%lsts_4348062 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49250, i64 5)
store %struct.ScmObj* %lsts_4348062, %struct.ScmObj** %stackaddr$env-ref55940
%stackaddr$env-ref55941 = alloca %struct.ScmObj*, align 8
%vs48060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49250, i64 6)
store %struct.ScmObj* %vs48060, %struct.ScmObj** %stackaddr$env-ref55941
%stackaddr$prim55942 = alloca %struct.ScmObj*, align 8
%_95k48390 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54465)
store volatile %struct.ScmObj* %_95k48390, %struct.ScmObj** %stackaddr$prim55942, align 8
%stackaddr$prim55943 = alloca %struct.ScmObj*, align 8
%current_45args54466 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54465)
store volatile %struct.ScmObj* %current_45args54466, %struct.ScmObj** %stackaddr$prim55943, align 8
%stackaddr$prim55944 = alloca %struct.ScmObj*, align 8
%anf_45bind48190 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54466)
store volatile %struct.ScmObj* %anf_45bind48190, %struct.ScmObj** %stackaddr$prim55944, align 8
%stackaddr$prim55945 = alloca %struct.ScmObj*, align 8
%anf_45bind48191 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48056, %struct.ScmObj* %lsts_4348062)
store volatile %struct.ScmObj* %anf_45bind48191, %struct.ScmObj** %stackaddr$prim55945, align 8
%stackaddr$prim55946 = alloca %struct.ScmObj*, align 8
%anf_45bind48192 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48057, %struct.ScmObj* %anf_45bind48191)
store volatile %struct.ScmObj* %anf_45bind48192, %struct.ScmObj** %stackaddr$prim55946, align 8
%stackaddr$makeclosure55947 = alloca %struct.ScmObj*, align 8
%fptrToInt55948 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49276 to i64
%ae49276 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55948)
store volatile %struct.ScmObj* %ae49276, %struct.ScmObj** %stackaddr$makeclosure55947, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49276, %struct.ScmObj* %k48383, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49276, %struct.ScmObj* %_37foldr148047, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49276, %struct.ScmObj* %f48057, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49276, %struct.ScmObj* %anf_45bind48190, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49276, %struct.ScmObj* %vs48060, i64 4)
%stackaddr$prim55949 = alloca %struct.ScmObj*, align 8
%cpsargs48394 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49276, %struct.ScmObj* %anf_45bind48192)
store volatile %struct.ScmObj* %cpsargs48394, %struct.ScmObj** %stackaddr$prim55949, align 8
%clofunc55950 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr48053)
musttail call tailcc void %clofunc55950(%struct.ScmObj* %_37foldr48053, %struct.ScmObj* %cpsargs48394)
ret void
}

define tailcc void @proc_clo$ae49276(%struct.ScmObj* %env$ae49276,%struct.ScmObj* %current_45args54468) {
%stackaddr$env-ref55951 = alloca %struct.ScmObj*, align 8
%k48383 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49276, i64 0)
store %struct.ScmObj* %k48383, %struct.ScmObj** %stackaddr$env-ref55951
%stackaddr$env-ref55952 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49276, i64 1)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55952
%stackaddr$env-ref55953 = alloca %struct.ScmObj*, align 8
%f48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49276, i64 2)
store %struct.ScmObj* %f48057, %struct.ScmObj** %stackaddr$env-ref55953
%stackaddr$env-ref55954 = alloca %struct.ScmObj*, align 8
%anf_45bind48190 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49276, i64 3)
store %struct.ScmObj* %anf_45bind48190, %struct.ScmObj** %stackaddr$env-ref55954
%stackaddr$env-ref55955 = alloca %struct.ScmObj*, align 8
%vs48060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49276, i64 4)
store %struct.ScmObj* %vs48060, %struct.ScmObj** %stackaddr$env-ref55955
%stackaddr$prim55956 = alloca %struct.ScmObj*, align 8
%_95k48391 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54468)
store volatile %struct.ScmObj* %_95k48391, %struct.ScmObj** %stackaddr$prim55956, align 8
%stackaddr$prim55957 = alloca %struct.ScmObj*, align 8
%current_45args54469 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54468)
store volatile %struct.ScmObj* %current_45args54469, %struct.ScmObj** %stackaddr$prim55957, align 8
%stackaddr$prim55958 = alloca %struct.ScmObj*, align 8
%anf_45bind48193 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54469)
store volatile %struct.ScmObj* %anf_45bind48193, %struct.ScmObj** %stackaddr$prim55958, align 8
%ae49281 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55959 = alloca %struct.ScmObj*, align 8
%anf_45bind48194 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48193, %struct.ScmObj* %ae49281)
store volatile %struct.ScmObj* %anf_45bind48194, %struct.ScmObj** %stackaddr$prim55959, align 8
%stackaddr$makeclosure55960 = alloca %struct.ScmObj*, align 8
%fptrToInt55961 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49283 to i64
%ae49283 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55961)
store volatile %struct.ScmObj* %ae49283, %struct.ScmObj** %stackaddr$makeclosure55960, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49283, %struct.ScmObj* %f48057, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49283, %struct.ScmObj* %k48383, i64 1)
%argslist54474$_37foldr1480470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55962 = alloca %struct.ScmObj*, align 8
%argslist54474$_37foldr1480471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs48060, %struct.ScmObj* %argslist54474$_37foldr1480470)
store volatile %struct.ScmObj* %argslist54474$_37foldr1480471, %struct.ScmObj** %stackaddr$prim55962, align 8
%stackaddr$prim55963 = alloca %struct.ScmObj*, align 8
%argslist54474$_37foldr1480472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48194, %struct.ScmObj* %argslist54474$_37foldr1480471)
store volatile %struct.ScmObj* %argslist54474$_37foldr1480472, %struct.ScmObj** %stackaddr$prim55963, align 8
%stackaddr$prim55964 = alloca %struct.ScmObj*, align 8
%argslist54474$_37foldr1480473 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48190, %struct.ScmObj* %argslist54474$_37foldr1480472)
store volatile %struct.ScmObj* %argslist54474$_37foldr1480473, %struct.ScmObj** %stackaddr$prim55964, align 8
%stackaddr$prim55965 = alloca %struct.ScmObj*, align 8
%argslist54474$_37foldr1480474 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49283, %struct.ScmObj* %argslist54474$_37foldr1480473)
store volatile %struct.ScmObj* %argslist54474$_37foldr1480474, %struct.ScmObj** %stackaddr$prim55965, align 8
%clofunc55966 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148047)
musttail call tailcc void %clofunc55966(%struct.ScmObj* %_37foldr148047, %struct.ScmObj* %argslist54474$_37foldr1480474)
ret void
}

define tailcc void @proc_clo$ae49283(%struct.ScmObj* %env$ae49283,%struct.ScmObj* %current_45args54471) {
%stackaddr$env-ref55967 = alloca %struct.ScmObj*, align 8
%f48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49283, i64 0)
store %struct.ScmObj* %f48057, %struct.ScmObj** %stackaddr$env-ref55967
%stackaddr$env-ref55968 = alloca %struct.ScmObj*, align 8
%k48383 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49283, i64 1)
store %struct.ScmObj* %k48383, %struct.ScmObj** %stackaddr$env-ref55968
%stackaddr$prim55969 = alloca %struct.ScmObj*, align 8
%_95k48392 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54471)
store volatile %struct.ScmObj* %_95k48392, %struct.ScmObj** %stackaddr$prim55969, align 8
%stackaddr$prim55970 = alloca %struct.ScmObj*, align 8
%current_45args54472 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54471)
store volatile %struct.ScmObj* %current_45args54472, %struct.ScmObj** %stackaddr$prim55970, align 8
%stackaddr$prim55971 = alloca %struct.ScmObj*, align 8
%anf_45bind48195 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54472)
store volatile %struct.ScmObj* %anf_45bind48195, %struct.ScmObj** %stackaddr$prim55971, align 8
%stackaddr$prim55972 = alloca %struct.ScmObj*, align 8
%cpsargs48393 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48383, %struct.ScmObj* %anf_45bind48195)
store volatile %struct.ScmObj* %cpsargs48393, %struct.ScmObj** %stackaddr$prim55972, align 8
%clofunc55973 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48057)
musttail call tailcc void %clofunc55973(%struct.ScmObj* %f48057, %struct.ScmObj* %cpsargs48393)
ret void
}

define tailcc void @proc_clo$ae49252(%struct.ScmObj* %env$ae49252,%struct.ScmObj* %current_45args54475) {
%stackaddr$prim55974 = alloca %struct.ScmObj*, align 8
%k48395 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54475)
store volatile %struct.ScmObj* %k48395, %struct.ScmObj** %stackaddr$prim55974, align 8
%stackaddr$prim55975 = alloca %struct.ScmObj*, align 8
%current_45args54476 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54475)
store volatile %struct.ScmObj* %current_45args54476, %struct.ScmObj** %stackaddr$prim55975, align 8
%stackaddr$prim55976 = alloca %struct.ScmObj*, align 8
%a48065 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54476)
store volatile %struct.ScmObj* %a48065, %struct.ScmObj** %stackaddr$prim55976, align 8
%stackaddr$prim55977 = alloca %struct.ScmObj*, align 8
%current_45args54477 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54476)
store volatile %struct.ScmObj* %current_45args54477, %struct.ScmObj** %stackaddr$prim55977, align 8
%stackaddr$prim55978 = alloca %struct.ScmObj*, align 8
%b48064 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54477)
store volatile %struct.ScmObj* %b48064, %struct.ScmObj** %stackaddr$prim55978, align 8
%stackaddr$prim55979 = alloca %struct.ScmObj*, align 8
%cpsprim48396 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a48065, %struct.ScmObj* %b48064)
store volatile %struct.ScmObj* %cpsprim48396, %struct.ScmObj** %stackaddr$prim55979, align 8
%ae49256 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54479$k483950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55980 = alloca %struct.ScmObj*, align 8
%argslist54479$k483951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48396, %struct.ScmObj* %argslist54479$k483950)
store volatile %struct.ScmObj* %argslist54479$k483951, %struct.ScmObj** %stackaddr$prim55980, align 8
%stackaddr$prim55981 = alloca %struct.ScmObj*, align 8
%argslist54479$k483952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49256, %struct.ScmObj* %argslist54479$k483951)
store volatile %struct.ScmObj* %argslist54479$k483952, %struct.ScmObj** %stackaddr$prim55981, align 8
%clofunc55982 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48395)
musttail call tailcc void %clofunc55982(%struct.ScmObj* %k48395, %struct.ScmObj* %argslist54479$k483952)
ret void
}

define tailcc void @proc_clo$ae49228(%struct.ScmObj* %env$ae49228,%struct.ScmObj* %current_45args54482) {
%stackaddr$prim55983 = alloca %struct.ScmObj*, align 8
%k48397 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54482)
store volatile %struct.ScmObj* %k48397, %struct.ScmObj** %stackaddr$prim55983, align 8
%stackaddr$prim55984 = alloca %struct.ScmObj*, align 8
%current_45args54483 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54482)
store volatile %struct.ScmObj* %current_45args54483, %struct.ScmObj** %stackaddr$prim55984, align 8
%stackaddr$prim55985 = alloca %struct.ScmObj*, align 8
%x48061 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54483)
store volatile %struct.ScmObj* %x48061, %struct.ScmObj** %stackaddr$prim55985, align 8
%stackaddr$prim55986 = alloca %struct.ScmObj*, align 8
%cpsprim48398 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48061)
store volatile %struct.ScmObj* %cpsprim48398, %struct.ScmObj** %stackaddr$prim55986, align 8
%ae49231 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54485$k483970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55987 = alloca %struct.ScmObj*, align 8
%argslist54485$k483971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48398, %struct.ScmObj* %argslist54485$k483970)
store volatile %struct.ScmObj* %argslist54485$k483971, %struct.ScmObj** %stackaddr$prim55987, align 8
%stackaddr$prim55988 = alloca %struct.ScmObj*, align 8
%argslist54485$k483972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49231, %struct.ScmObj* %argslist54485$k483971)
store volatile %struct.ScmObj* %argslist54485$k483972, %struct.ScmObj** %stackaddr$prim55988, align 8
%clofunc55989 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48397)
musttail call tailcc void %clofunc55989(%struct.ScmObj* %k48397, %struct.ScmObj* %argslist54485$k483972)
ret void
}

define tailcc void @proc_clo$ae49204(%struct.ScmObj* %env$ae49204,%struct.ScmObj* %current_45args54488) {
%stackaddr$prim55990 = alloca %struct.ScmObj*, align 8
%k48399 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54488)
store volatile %struct.ScmObj* %k48399, %struct.ScmObj** %stackaddr$prim55990, align 8
%stackaddr$prim55991 = alloca %struct.ScmObj*, align 8
%current_45args54489 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54488)
store volatile %struct.ScmObj* %current_45args54489, %struct.ScmObj** %stackaddr$prim55991, align 8
%stackaddr$prim55992 = alloca %struct.ScmObj*, align 8
%x48063 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54489)
store volatile %struct.ScmObj* %x48063, %struct.ScmObj** %stackaddr$prim55992, align 8
%stackaddr$prim55993 = alloca %struct.ScmObj*, align 8
%cpsprim48400 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48063)
store volatile %struct.ScmObj* %cpsprim48400, %struct.ScmObj** %stackaddr$prim55993, align 8
%ae49207 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54491$k483990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55994 = alloca %struct.ScmObj*, align 8
%argslist54491$k483991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48400, %struct.ScmObj* %argslist54491$k483990)
store volatile %struct.ScmObj* %argslist54491$k483991, %struct.ScmObj** %stackaddr$prim55994, align 8
%stackaddr$prim55995 = alloca %struct.ScmObj*, align 8
%argslist54491$k483992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49207, %struct.ScmObj* %argslist54491$k483991)
store volatile %struct.ScmObj* %argslist54491$k483992, %struct.ScmObj** %stackaddr$prim55995, align 8
%clofunc55996 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48399)
musttail call tailcc void %clofunc55996(%struct.ScmObj* %k48399, %struct.ScmObj* %argslist54491$k483992)
ret void
}

define tailcc void @proc_clo$ae49156(%struct.ScmObj* %env$ae49156,%struct.ScmObj* %current_45args54494) {
%stackaddr$prim55997 = alloca %struct.ScmObj*, align 8
%k48401 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54494)
store volatile %struct.ScmObj* %k48401, %struct.ScmObj** %stackaddr$prim55997, align 8
%stackaddr$prim55998 = alloca %struct.ScmObj*, align 8
%current_45args54495 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54494)
store volatile %struct.ScmObj* %current_45args54495, %struct.ScmObj** %stackaddr$prim55998, align 8
%stackaddr$prim55999 = alloca %struct.ScmObj*, align 8
%lst48059 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54495)
store volatile %struct.ScmObj* %lst48059, %struct.ScmObj** %stackaddr$prim55999, align 8
%stackaddr$prim56000 = alloca %struct.ScmObj*, align 8
%current_45args54496 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54495)
store volatile %struct.ScmObj* %current_45args54496, %struct.ScmObj** %stackaddr$prim56000, align 8
%stackaddr$prim56001 = alloca %struct.ScmObj*, align 8
%b48058 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54496)
store volatile %struct.ScmObj* %b48058, %struct.ScmObj** %stackaddr$prim56001, align 8
%truthy$cmp56002 = call i64 @is_truthy_value(%struct.ScmObj* %b48058)
%cmp$cmp56002 = icmp eq i64 %truthy$cmp56002, 1
br i1 %cmp$cmp56002, label %truebranch$cmp56002, label %falsebranch$cmp56002
truebranch$cmp56002:
%ae49159 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54498$k484010 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56003 = alloca %struct.ScmObj*, align 8
%argslist54498$k484011 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b48058, %struct.ScmObj* %argslist54498$k484010)
store volatile %struct.ScmObj* %argslist54498$k484011, %struct.ScmObj** %stackaddr$prim56003, align 8
%stackaddr$prim56004 = alloca %struct.ScmObj*, align 8
%argslist54498$k484012 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49159, %struct.ScmObj* %argslist54498$k484011)
store volatile %struct.ScmObj* %argslist54498$k484012, %struct.ScmObj** %stackaddr$prim56004, align 8
%clofunc56005 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48401)
musttail call tailcc void %clofunc56005(%struct.ScmObj* %k48401, %struct.ScmObj* %argslist54498$k484012)
ret void
falsebranch$cmp56002:
%stackaddr$prim56006 = alloca %struct.ScmObj*, align 8
%cpsprim48402 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48059)
store volatile %struct.ScmObj* %cpsprim48402, %struct.ScmObj** %stackaddr$prim56006, align 8
%ae49166 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54499$k484010 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56007 = alloca %struct.ScmObj*, align 8
%argslist54499$k484011 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48402, %struct.ScmObj* %argslist54499$k484010)
store volatile %struct.ScmObj* %argslist54499$k484011, %struct.ScmObj** %stackaddr$prim56007, align 8
%stackaddr$prim56008 = alloca %struct.ScmObj*, align 8
%argslist54499$k484012 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49166, %struct.ScmObj* %argslist54499$k484011)
store volatile %struct.ScmObj* %argslist54499$k484012, %struct.ScmObj** %stackaddr$prim56008, align 8
%clofunc56009 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48401)
musttail call tailcc void %clofunc56009(%struct.ScmObj* %k48401, %struct.ScmObj* %argslist54499$k484012)
ret void
}

define tailcc void @proc_clo$ae49113(%struct.ScmObj* %env$ae49113,%struct.ScmObj* %current_45args54503) {
%stackaddr$env-ref56010 = alloca %struct.ScmObj*, align 8
%_37take48039 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49113, i64 0)
store %struct.ScmObj* %_37take48039, %struct.ScmObj** %stackaddr$env-ref56010
%stackaddr$env-ref56011 = alloca %struct.ScmObj*, align 8
%_37length48036 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49113, i64 1)
store %struct.ScmObj* %_37length48036, %struct.ScmObj** %stackaddr$env-ref56011
%stackaddr$prim56012 = alloca %struct.ScmObj*, align 8
%k48403 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54503)
store volatile %struct.ScmObj* %k48403, %struct.ScmObj** %stackaddr$prim56012, align 8
%stackaddr$prim56013 = alloca %struct.ScmObj*, align 8
%current_45args54504 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54503)
store volatile %struct.ScmObj* %current_45args54504, %struct.ScmObj** %stackaddr$prim56013, align 8
%stackaddr$prim56014 = alloca %struct.ScmObj*, align 8
%lst48068 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54504)
store volatile %struct.ScmObj* %lst48068, %struct.ScmObj** %stackaddr$prim56014, align 8
%stackaddr$prim56015 = alloca %struct.ScmObj*, align 8
%current_45args54505 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54504)
store volatile %struct.ScmObj* %current_45args54505, %struct.ScmObj** %stackaddr$prim56015, align 8
%stackaddr$prim56016 = alloca %struct.ScmObj*, align 8
%n48067 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54505)
store volatile %struct.ScmObj* %n48067, %struct.ScmObj** %stackaddr$prim56016, align 8
%stackaddr$makeclosure56017 = alloca %struct.ScmObj*, align 8
%fptrToInt56018 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49115 to i64
%ae49115 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56018)
store volatile %struct.ScmObj* %ae49115, %struct.ScmObj** %stackaddr$makeclosure56017, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49115, %struct.ScmObj* %k48403, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49115, %struct.ScmObj* %n48067, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49115, %struct.ScmObj* %_37take48039, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49115, %struct.ScmObj* %lst48068, i64 3)
%argslist54511$_37length480360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56019 = alloca %struct.ScmObj*, align 8
%argslist54511$_37length480361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48068, %struct.ScmObj* %argslist54511$_37length480360)
store volatile %struct.ScmObj* %argslist54511$_37length480361, %struct.ScmObj** %stackaddr$prim56019, align 8
%stackaddr$prim56020 = alloca %struct.ScmObj*, align 8
%argslist54511$_37length480362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49115, %struct.ScmObj* %argslist54511$_37length480361)
store volatile %struct.ScmObj* %argslist54511$_37length480362, %struct.ScmObj** %stackaddr$prim56020, align 8
%clofunc56021 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length48036)
musttail call tailcc void %clofunc56021(%struct.ScmObj* %_37length48036, %struct.ScmObj* %argslist54511$_37length480362)
ret void
}

define tailcc void @proc_clo$ae49115(%struct.ScmObj* %env$ae49115,%struct.ScmObj* %current_45args54507) {
%stackaddr$env-ref56022 = alloca %struct.ScmObj*, align 8
%k48403 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49115, i64 0)
store %struct.ScmObj* %k48403, %struct.ScmObj** %stackaddr$env-ref56022
%stackaddr$env-ref56023 = alloca %struct.ScmObj*, align 8
%n48067 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49115, i64 1)
store %struct.ScmObj* %n48067, %struct.ScmObj** %stackaddr$env-ref56023
%stackaddr$env-ref56024 = alloca %struct.ScmObj*, align 8
%_37take48039 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49115, i64 2)
store %struct.ScmObj* %_37take48039, %struct.ScmObj** %stackaddr$env-ref56024
%stackaddr$env-ref56025 = alloca %struct.ScmObj*, align 8
%lst48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49115, i64 3)
store %struct.ScmObj* %lst48068, %struct.ScmObj** %stackaddr$env-ref56025
%stackaddr$prim56026 = alloca %struct.ScmObj*, align 8
%_95k48404 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54507)
store volatile %struct.ScmObj* %_95k48404, %struct.ScmObj** %stackaddr$prim56026, align 8
%stackaddr$prim56027 = alloca %struct.ScmObj*, align 8
%current_45args54508 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54507)
store volatile %struct.ScmObj* %current_45args54508, %struct.ScmObj** %stackaddr$prim56027, align 8
%stackaddr$prim56028 = alloca %struct.ScmObj*, align 8
%anf_45bind48182 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54508)
store volatile %struct.ScmObj* %anf_45bind48182, %struct.ScmObj** %stackaddr$prim56028, align 8
%stackaddr$prim56029 = alloca %struct.ScmObj*, align 8
%anf_45bind48183 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind48182, %struct.ScmObj* %n48067)
store volatile %struct.ScmObj* %anf_45bind48183, %struct.ScmObj** %stackaddr$prim56029, align 8
%argslist54510$_37take480390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56030 = alloca %struct.ScmObj*, align 8
%argslist54510$_37take480391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48183, %struct.ScmObj* %argslist54510$_37take480390)
store volatile %struct.ScmObj* %argslist54510$_37take480391, %struct.ScmObj** %stackaddr$prim56030, align 8
%stackaddr$prim56031 = alloca %struct.ScmObj*, align 8
%argslist54510$_37take480392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48068, %struct.ScmObj* %argslist54510$_37take480391)
store volatile %struct.ScmObj* %argslist54510$_37take480392, %struct.ScmObj** %stackaddr$prim56031, align 8
%stackaddr$prim56032 = alloca %struct.ScmObj*, align 8
%argslist54510$_37take480393 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48403, %struct.ScmObj* %argslist54510$_37take480392)
store volatile %struct.ScmObj* %argslist54510$_37take480393, %struct.ScmObj** %stackaddr$prim56032, align 8
%clofunc56033 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take48039)
musttail call tailcc void %clofunc56033(%struct.ScmObj* %_37take48039, %struct.ScmObj* %argslist54510$_37take480393)
ret void
}

define tailcc void @proc_clo$ae49059(%struct.ScmObj* %env$ae49059,%struct.ScmObj* %current_45args54513) {
%stackaddr$env-ref56034 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49059, i64 0)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref56034
%stackaddr$prim56035 = alloca %struct.ScmObj*, align 8
%k48405 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54513)
store volatile %struct.ScmObj* %k48405, %struct.ScmObj** %stackaddr$prim56035, align 8
%stackaddr$prim56036 = alloca %struct.ScmObj*, align 8
%current_45args54514 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54513)
store volatile %struct.ScmObj* %current_45args54514, %struct.ScmObj** %stackaddr$prim56036, align 8
%stackaddr$prim56037 = alloca %struct.ScmObj*, align 8
%lst48070 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54514)
store volatile %struct.ScmObj* %lst48070, %struct.ScmObj** %stackaddr$prim56037, align 8
%stackaddr$makeclosure56038 = alloca %struct.ScmObj*, align 8
%fptrToInt56039 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49060 to i64
%ae49060 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56039)
store volatile %struct.ScmObj* %ae49060, %struct.ScmObj** %stackaddr$makeclosure56038, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49060, %struct.ScmObj* %lst48070, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49060, %struct.ScmObj* %k48405, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49060, %struct.ScmObj* %_37foldl148031, i64 2)
%ae49061 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56040 = alloca %struct.ScmObj*, align 8
%fptrToInt56041 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49062 to i64
%ae49062 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56041)
store volatile %struct.ScmObj* %ae49062, %struct.ScmObj** %stackaddr$makeclosure56040, align 8
%argslist54525$ae490600 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56042 = alloca %struct.ScmObj*, align 8
%argslist54525$ae490601 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49062, %struct.ScmObj* %argslist54525$ae490600)
store volatile %struct.ScmObj* %argslist54525$ae490601, %struct.ScmObj** %stackaddr$prim56042, align 8
%stackaddr$prim56043 = alloca %struct.ScmObj*, align 8
%argslist54525$ae490602 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49061, %struct.ScmObj* %argslist54525$ae490601)
store volatile %struct.ScmObj* %argslist54525$ae490602, %struct.ScmObj** %stackaddr$prim56043, align 8
%clofunc56044 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49060)
musttail call tailcc void %clofunc56044(%struct.ScmObj* %ae49060, %struct.ScmObj* %argslist54525$ae490602)
ret void
}

define tailcc void @proc_clo$ae49060(%struct.ScmObj* %env$ae49060,%struct.ScmObj* %current_45args54516) {
%stackaddr$env-ref56045 = alloca %struct.ScmObj*, align 8
%lst48070 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49060, i64 0)
store %struct.ScmObj* %lst48070, %struct.ScmObj** %stackaddr$env-ref56045
%stackaddr$env-ref56046 = alloca %struct.ScmObj*, align 8
%k48405 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49060, i64 1)
store %struct.ScmObj* %k48405, %struct.ScmObj** %stackaddr$env-ref56046
%stackaddr$env-ref56047 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49060, i64 2)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref56047
%stackaddr$prim56048 = alloca %struct.ScmObj*, align 8
%_95k48406 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54516)
store volatile %struct.ScmObj* %_95k48406, %struct.ScmObj** %stackaddr$prim56048, align 8
%stackaddr$prim56049 = alloca %struct.ScmObj*, align 8
%current_45args54517 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54516)
store volatile %struct.ScmObj* %current_45args54517, %struct.ScmObj** %stackaddr$prim56049, align 8
%stackaddr$prim56050 = alloca %struct.ScmObj*, align 8
%anf_45bind48181 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54517)
store volatile %struct.ScmObj* %anf_45bind48181, %struct.ScmObj** %stackaddr$prim56050, align 8
%ae49081 = call %struct.ScmObj* @const_init_null()
%argslist54519$_37foldl1480310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56051 = alloca %struct.ScmObj*, align 8
%argslist54519$_37foldl1480311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48070, %struct.ScmObj* %argslist54519$_37foldl1480310)
store volatile %struct.ScmObj* %argslist54519$_37foldl1480311, %struct.ScmObj** %stackaddr$prim56051, align 8
%stackaddr$prim56052 = alloca %struct.ScmObj*, align 8
%argslist54519$_37foldl1480312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49081, %struct.ScmObj* %argslist54519$_37foldl1480311)
store volatile %struct.ScmObj* %argslist54519$_37foldl1480312, %struct.ScmObj** %stackaddr$prim56052, align 8
%stackaddr$prim56053 = alloca %struct.ScmObj*, align 8
%argslist54519$_37foldl1480313 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48181, %struct.ScmObj* %argslist54519$_37foldl1480312)
store volatile %struct.ScmObj* %argslist54519$_37foldl1480313, %struct.ScmObj** %stackaddr$prim56053, align 8
%stackaddr$prim56054 = alloca %struct.ScmObj*, align 8
%argslist54519$_37foldl1480314 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48405, %struct.ScmObj* %argslist54519$_37foldl1480313)
store volatile %struct.ScmObj* %argslist54519$_37foldl1480314, %struct.ScmObj** %stackaddr$prim56054, align 8
%clofunc56055 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148031)
musttail call tailcc void %clofunc56055(%struct.ScmObj* %_37foldl148031, %struct.ScmObj* %argslist54519$_37foldl1480314)
ret void
}

define tailcc void @proc_clo$ae49062(%struct.ScmObj* %env$ae49062,%struct.ScmObj* %current_45args54520) {
%stackaddr$prim56056 = alloca %struct.ScmObj*, align 8
%k48407 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54520)
store volatile %struct.ScmObj* %k48407, %struct.ScmObj** %stackaddr$prim56056, align 8
%stackaddr$prim56057 = alloca %struct.ScmObj*, align 8
%current_45args54521 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54520)
store volatile %struct.ScmObj* %current_45args54521, %struct.ScmObj** %stackaddr$prim56057, align 8
%stackaddr$prim56058 = alloca %struct.ScmObj*, align 8
%x48072 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54521)
store volatile %struct.ScmObj* %x48072, %struct.ScmObj** %stackaddr$prim56058, align 8
%stackaddr$prim56059 = alloca %struct.ScmObj*, align 8
%current_45args54522 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54521)
store volatile %struct.ScmObj* %current_45args54522, %struct.ScmObj** %stackaddr$prim56059, align 8
%stackaddr$prim56060 = alloca %struct.ScmObj*, align 8
%y48071 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54522)
store volatile %struct.ScmObj* %y48071, %struct.ScmObj** %stackaddr$prim56060, align 8
%ae49064 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54524$k484070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56061 = alloca %struct.ScmObj*, align 8
%argslist54524$k484071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x48072, %struct.ScmObj* %argslist54524$k484070)
store volatile %struct.ScmObj* %argslist54524$k484071, %struct.ScmObj** %stackaddr$prim56061, align 8
%stackaddr$prim56062 = alloca %struct.ScmObj*, align 8
%argslist54524$k484072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49064, %struct.ScmObj* %argslist54524$k484071)
store volatile %struct.ScmObj* %argslist54524$k484072, %struct.ScmObj** %stackaddr$prim56062, align 8
%clofunc56063 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48407)
musttail call tailcc void %clofunc56063(%struct.ScmObj* %k48407, %struct.ScmObj* %argslist54524$k484072)
ret void
}

define tailcc void @proc_clo$ae48980(%struct.ScmObj* %env$ae48980,%struct.ScmObj* %current_45args54528) {
%stackaddr$prim56064 = alloca %struct.ScmObj*, align 8
%k48408 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54528)
store volatile %struct.ScmObj* %k48408, %struct.ScmObj** %stackaddr$prim56064, align 8
%stackaddr$prim56065 = alloca %struct.ScmObj*, align 8
%current_45args54529 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54528)
store volatile %struct.ScmObj* %current_45args54529, %struct.ScmObj** %stackaddr$prim56065, align 8
%stackaddr$prim56066 = alloca %struct.ScmObj*, align 8
%_37foldl148032 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54529)
store volatile %struct.ScmObj* %_37foldl148032, %struct.ScmObj** %stackaddr$prim56066, align 8
%ae48982 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56067 = alloca %struct.ScmObj*, align 8
%fptrToInt56068 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48983 to i64
%ae48983 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56068)
store volatile %struct.ScmObj* %ae48983, %struct.ScmObj** %stackaddr$makeclosure56067, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48983, %struct.ScmObj* %_37foldl148032, i64 0)
%argslist54542$k484080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56069 = alloca %struct.ScmObj*, align 8
%argslist54542$k484081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48983, %struct.ScmObj* %argslist54542$k484080)
store volatile %struct.ScmObj* %argslist54542$k484081, %struct.ScmObj** %stackaddr$prim56069, align 8
%stackaddr$prim56070 = alloca %struct.ScmObj*, align 8
%argslist54542$k484082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48982, %struct.ScmObj* %argslist54542$k484081)
store volatile %struct.ScmObj* %argslist54542$k484082, %struct.ScmObj** %stackaddr$prim56070, align 8
%clofunc56071 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48408)
musttail call tailcc void %clofunc56071(%struct.ScmObj* %k48408, %struct.ScmObj* %argslist54542$k484082)
ret void
}

define tailcc void @proc_clo$ae48983(%struct.ScmObj* %env$ae48983,%struct.ScmObj* %current_45args54531) {
%stackaddr$env-ref56072 = alloca %struct.ScmObj*, align 8
%_37foldl148032 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48983, i64 0)
store %struct.ScmObj* %_37foldl148032, %struct.ScmObj** %stackaddr$env-ref56072
%stackaddr$prim56073 = alloca %struct.ScmObj*, align 8
%k48409 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54531)
store volatile %struct.ScmObj* %k48409, %struct.ScmObj** %stackaddr$prim56073, align 8
%stackaddr$prim56074 = alloca %struct.ScmObj*, align 8
%current_45args54532 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54531)
store volatile %struct.ScmObj* %current_45args54532, %struct.ScmObj** %stackaddr$prim56074, align 8
%stackaddr$prim56075 = alloca %struct.ScmObj*, align 8
%f48035 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54532)
store volatile %struct.ScmObj* %f48035, %struct.ScmObj** %stackaddr$prim56075, align 8
%stackaddr$prim56076 = alloca %struct.ScmObj*, align 8
%current_45args54533 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54532)
store volatile %struct.ScmObj* %current_45args54533, %struct.ScmObj** %stackaddr$prim56076, align 8
%stackaddr$prim56077 = alloca %struct.ScmObj*, align 8
%acc48034 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54533)
store volatile %struct.ScmObj* %acc48034, %struct.ScmObj** %stackaddr$prim56077, align 8
%stackaddr$prim56078 = alloca %struct.ScmObj*, align 8
%current_45args54534 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54533)
store volatile %struct.ScmObj* %current_45args54534, %struct.ScmObj** %stackaddr$prim56078, align 8
%stackaddr$prim56079 = alloca %struct.ScmObj*, align 8
%lst48033 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54534)
store volatile %struct.ScmObj* %lst48033, %struct.ScmObj** %stackaddr$prim56079, align 8
%stackaddr$prim56080 = alloca %struct.ScmObj*, align 8
%anf_45bind48176 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48033)
store volatile %struct.ScmObj* %anf_45bind48176, %struct.ScmObj** %stackaddr$prim56080, align 8
%truthy$cmp56081 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48176)
%cmp$cmp56081 = icmp eq i64 %truthy$cmp56081, 1
br i1 %cmp$cmp56081, label %truebranch$cmp56081, label %falsebranch$cmp56081
truebranch$cmp56081:
%ae48987 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54536$k484090 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56082 = alloca %struct.ScmObj*, align 8
%argslist54536$k484091 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48034, %struct.ScmObj* %argslist54536$k484090)
store volatile %struct.ScmObj* %argslist54536$k484091, %struct.ScmObj** %stackaddr$prim56082, align 8
%stackaddr$prim56083 = alloca %struct.ScmObj*, align 8
%argslist54536$k484092 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48987, %struct.ScmObj* %argslist54536$k484091)
store volatile %struct.ScmObj* %argslist54536$k484092, %struct.ScmObj** %stackaddr$prim56083, align 8
%clofunc56084 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48409)
musttail call tailcc void %clofunc56084(%struct.ScmObj* %k48409, %struct.ScmObj* %argslist54536$k484092)
ret void
falsebranch$cmp56081:
%stackaddr$prim56085 = alloca %struct.ScmObj*, align 8
%anf_45bind48177 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48033)
store volatile %struct.ScmObj* %anf_45bind48177, %struct.ScmObj** %stackaddr$prim56085, align 8
%stackaddr$makeclosure56086 = alloca %struct.ScmObj*, align 8
%fptrToInt56087 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48994 to i64
%ae48994 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56087)
store volatile %struct.ScmObj* %ae48994, %struct.ScmObj** %stackaddr$makeclosure56086, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48994, %struct.ScmObj* %k48409, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48994, %struct.ScmObj* %f48035, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48994, %struct.ScmObj* %lst48033, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48994, %struct.ScmObj* %_37foldl148032, i64 3)
%argslist54541$f480350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56088 = alloca %struct.ScmObj*, align 8
%argslist54541$f480351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48034, %struct.ScmObj* %argslist54541$f480350)
store volatile %struct.ScmObj* %argslist54541$f480351, %struct.ScmObj** %stackaddr$prim56088, align 8
%stackaddr$prim56089 = alloca %struct.ScmObj*, align 8
%argslist54541$f480352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48177, %struct.ScmObj* %argslist54541$f480351)
store volatile %struct.ScmObj* %argslist54541$f480352, %struct.ScmObj** %stackaddr$prim56089, align 8
%stackaddr$prim56090 = alloca %struct.ScmObj*, align 8
%argslist54541$f480353 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48994, %struct.ScmObj* %argslist54541$f480352)
store volatile %struct.ScmObj* %argslist54541$f480353, %struct.ScmObj** %stackaddr$prim56090, align 8
%clofunc56091 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48035)
musttail call tailcc void %clofunc56091(%struct.ScmObj* %f48035, %struct.ScmObj* %argslist54541$f480353)
ret void
}

define tailcc void @proc_clo$ae48994(%struct.ScmObj* %env$ae48994,%struct.ScmObj* %current_45args54537) {
%stackaddr$env-ref56092 = alloca %struct.ScmObj*, align 8
%k48409 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48994, i64 0)
store %struct.ScmObj* %k48409, %struct.ScmObj** %stackaddr$env-ref56092
%stackaddr$env-ref56093 = alloca %struct.ScmObj*, align 8
%f48035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48994, i64 1)
store %struct.ScmObj* %f48035, %struct.ScmObj** %stackaddr$env-ref56093
%stackaddr$env-ref56094 = alloca %struct.ScmObj*, align 8
%lst48033 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48994, i64 2)
store %struct.ScmObj* %lst48033, %struct.ScmObj** %stackaddr$env-ref56094
%stackaddr$env-ref56095 = alloca %struct.ScmObj*, align 8
%_37foldl148032 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48994, i64 3)
store %struct.ScmObj* %_37foldl148032, %struct.ScmObj** %stackaddr$env-ref56095
%stackaddr$prim56096 = alloca %struct.ScmObj*, align 8
%_95k48410 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54537)
store volatile %struct.ScmObj* %_95k48410, %struct.ScmObj** %stackaddr$prim56096, align 8
%stackaddr$prim56097 = alloca %struct.ScmObj*, align 8
%current_45args54538 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54537)
store volatile %struct.ScmObj* %current_45args54538, %struct.ScmObj** %stackaddr$prim56097, align 8
%stackaddr$prim56098 = alloca %struct.ScmObj*, align 8
%anf_45bind48178 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54538)
store volatile %struct.ScmObj* %anf_45bind48178, %struct.ScmObj** %stackaddr$prim56098, align 8
%stackaddr$prim56099 = alloca %struct.ScmObj*, align 8
%anf_45bind48179 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48033)
store volatile %struct.ScmObj* %anf_45bind48179, %struct.ScmObj** %stackaddr$prim56099, align 8
%argslist54540$_37foldl1480320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56100 = alloca %struct.ScmObj*, align 8
%argslist54540$_37foldl1480321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48179, %struct.ScmObj* %argslist54540$_37foldl1480320)
store volatile %struct.ScmObj* %argslist54540$_37foldl1480321, %struct.ScmObj** %stackaddr$prim56100, align 8
%stackaddr$prim56101 = alloca %struct.ScmObj*, align 8
%argslist54540$_37foldl1480322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48178, %struct.ScmObj* %argslist54540$_37foldl1480321)
store volatile %struct.ScmObj* %argslist54540$_37foldl1480322, %struct.ScmObj** %stackaddr$prim56101, align 8
%stackaddr$prim56102 = alloca %struct.ScmObj*, align 8
%argslist54540$_37foldl1480323 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48035, %struct.ScmObj* %argslist54540$_37foldl1480322)
store volatile %struct.ScmObj* %argslist54540$_37foldl1480323, %struct.ScmObj** %stackaddr$prim56102, align 8
%stackaddr$prim56103 = alloca %struct.ScmObj*, align 8
%argslist54540$_37foldl1480324 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48409, %struct.ScmObj* %argslist54540$_37foldl1480323)
store volatile %struct.ScmObj* %argslist54540$_37foldl1480324, %struct.ScmObj** %stackaddr$prim56103, align 8
%clofunc56104 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148032)
musttail call tailcc void %clofunc56104(%struct.ScmObj* %_37foldl148032, %struct.ScmObj* %argslist54540$_37foldl1480324)
ret void
}

define tailcc void @proc_clo$ae48897(%struct.ScmObj* %env$ae48897,%struct.ScmObj* %current_45args54545) {
%stackaddr$prim56105 = alloca %struct.ScmObj*, align 8
%k48411 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54545)
store volatile %struct.ScmObj* %k48411, %struct.ScmObj** %stackaddr$prim56105, align 8
%stackaddr$prim56106 = alloca %struct.ScmObj*, align 8
%current_45args54546 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54545)
store volatile %struct.ScmObj* %current_45args54546, %struct.ScmObj** %stackaddr$prim56106, align 8
%stackaddr$prim56107 = alloca %struct.ScmObj*, align 8
%_37length48037 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54546)
store volatile %struct.ScmObj* %_37length48037, %struct.ScmObj** %stackaddr$prim56107, align 8
%ae48899 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56108 = alloca %struct.ScmObj*, align 8
%fptrToInt56109 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48900 to i64
%ae48900 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56109)
store volatile %struct.ScmObj* %ae48900, %struct.ScmObj** %stackaddr$makeclosure56108, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48900, %struct.ScmObj* %_37length48037, i64 0)
%argslist54557$k484110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56110 = alloca %struct.ScmObj*, align 8
%argslist54557$k484111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48900, %struct.ScmObj* %argslist54557$k484110)
store volatile %struct.ScmObj* %argslist54557$k484111, %struct.ScmObj** %stackaddr$prim56110, align 8
%stackaddr$prim56111 = alloca %struct.ScmObj*, align 8
%argslist54557$k484112 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48899, %struct.ScmObj* %argslist54557$k484111)
store volatile %struct.ScmObj* %argslist54557$k484112, %struct.ScmObj** %stackaddr$prim56111, align 8
%clofunc56112 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48411)
musttail call tailcc void %clofunc56112(%struct.ScmObj* %k48411, %struct.ScmObj* %argslist54557$k484112)
ret void
}

define tailcc void @proc_clo$ae48900(%struct.ScmObj* %env$ae48900,%struct.ScmObj* %current_45args54548) {
%stackaddr$env-ref56113 = alloca %struct.ScmObj*, align 8
%_37length48037 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48900, i64 0)
store %struct.ScmObj* %_37length48037, %struct.ScmObj** %stackaddr$env-ref56113
%stackaddr$prim56114 = alloca %struct.ScmObj*, align 8
%k48412 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54548)
store volatile %struct.ScmObj* %k48412, %struct.ScmObj** %stackaddr$prim56114, align 8
%stackaddr$prim56115 = alloca %struct.ScmObj*, align 8
%current_45args54549 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54548)
store volatile %struct.ScmObj* %current_45args54549, %struct.ScmObj** %stackaddr$prim56115, align 8
%stackaddr$prim56116 = alloca %struct.ScmObj*, align 8
%lst48038 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54549)
store volatile %struct.ScmObj* %lst48038, %struct.ScmObj** %stackaddr$prim56116, align 8
%stackaddr$prim56117 = alloca %struct.ScmObj*, align 8
%anf_45bind48172 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48038)
store volatile %struct.ScmObj* %anf_45bind48172, %struct.ScmObj** %stackaddr$prim56117, align 8
%truthy$cmp56118 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48172)
%cmp$cmp56118 = icmp eq i64 %truthy$cmp56118, 1
br i1 %cmp$cmp56118, label %truebranch$cmp56118, label %falsebranch$cmp56118
truebranch$cmp56118:
%ae48904 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48905 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54551$k484120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56119 = alloca %struct.ScmObj*, align 8
%argslist54551$k484121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48905, %struct.ScmObj* %argslist54551$k484120)
store volatile %struct.ScmObj* %argslist54551$k484121, %struct.ScmObj** %stackaddr$prim56119, align 8
%stackaddr$prim56120 = alloca %struct.ScmObj*, align 8
%argslist54551$k484122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48904, %struct.ScmObj* %argslist54551$k484121)
store volatile %struct.ScmObj* %argslist54551$k484122, %struct.ScmObj** %stackaddr$prim56120, align 8
%clofunc56121 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48412)
musttail call tailcc void %clofunc56121(%struct.ScmObj* %k48412, %struct.ScmObj* %argslist54551$k484122)
ret void
falsebranch$cmp56118:
%stackaddr$prim56122 = alloca %struct.ScmObj*, align 8
%anf_45bind48173 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48038)
store volatile %struct.ScmObj* %anf_45bind48173, %struct.ScmObj** %stackaddr$prim56122, align 8
%stackaddr$makeclosure56123 = alloca %struct.ScmObj*, align 8
%fptrToInt56124 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48914 to i64
%ae48914 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56124)
store volatile %struct.ScmObj* %ae48914, %struct.ScmObj** %stackaddr$makeclosure56123, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48914, %struct.ScmObj* %k48412, i64 0)
%argslist54556$_37length480370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56125 = alloca %struct.ScmObj*, align 8
%argslist54556$_37length480371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48173, %struct.ScmObj* %argslist54556$_37length480370)
store volatile %struct.ScmObj* %argslist54556$_37length480371, %struct.ScmObj** %stackaddr$prim56125, align 8
%stackaddr$prim56126 = alloca %struct.ScmObj*, align 8
%argslist54556$_37length480372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48914, %struct.ScmObj* %argslist54556$_37length480371)
store volatile %struct.ScmObj* %argslist54556$_37length480372, %struct.ScmObj** %stackaddr$prim56126, align 8
%clofunc56127 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length48037)
musttail call tailcc void %clofunc56127(%struct.ScmObj* %_37length48037, %struct.ScmObj* %argslist54556$_37length480372)
ret void
}

define tailcc void @proc_clo$ae48914(%struct.ScmObj* %env$ae48914,%struct.ScmObj* %current_45args54552) {
%stackaddr$env-ref56128 = alloca %struct.ScmObj*, align 8
%k48412 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48914, i64 0)
store %struct.ScmObj* %k48412, %struct.ScmObj** %stackaddr$env-ref56128
%stackaddr$prim56129 = alloca %struct.ScmObj*, align 8
%_95k48413 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54552)
store volatile %struct.ScmObj* %_95k48413, %struct.ScmObj** %stackaddr$prim56129, align 8
%stackaddr$prim56130 = alloca %struct.ScmObj*, align 8
%current_45args54553 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54552)
store volatile %struct.ScmObj* %current_45args54553, %struct.ScmObj** %stackaddr$prim56130, align 8
%stackaddr$prim56131 = alloca %struct.ScmObj*, align 8
%anf_45bind48174 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54553)
store volatile %struct.ScmObj* %anf_45bind48174, %struct.ScmObj** %stackaddr$prim56131, align 8
%ae48916 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56132 = alloca %struct.ScmObj*, align 8
%cpsprim48414 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae48916, %struct.ScmObj* %anf_45bind48174)
store volatile %struct.ScmObj* %cpsprim48414, %struct.ScmObj** %stackaddr$prim56132, align 8
%ae48919 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54555$k484120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56133 = alloca %struct.ScmObj*, align 8
%argslist54555$k484121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48414, %struct.ScmObj* %argslist54555$k484120)
store volatile %struct.ScmObj* %argslist54555$k484121, %struct.ScmObj** %stackaddr$prim56133, align 8
%stackaddr$prim56134 = alloca %struct.ScmObj*, align 8
%argslist54555$k484122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48919, %struct.ScmObj* %argslist54555$k484121)
store volatile %struct.ScmObj* %argslist54555$k484122, %struct.ScmObj** %stackaddr$prim56134, align 8
%clofunc56135 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48412)
musttail call tailcc void %clofunc56135(%struct.ScmObj* %k48412, %struct.ScmObj* %argslist54555$k484122)
ret void
}

define tailcc void @proc_clo$ae48747(%struct.ScmObj* %env$ae48747,%struct.ScmObj* %current_45args54560) {
%stackaddr$prim56136 = alloca %struct.ScmObj*, align 8
%k48415 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54560)
store volatile %struct.ScmObj* %k48415, %struct.ScmObj** %stackaddr$prim56136, align 8
%stackaddr$prim56137 = alloca %struct.ScmObj*, align 8
%current_45args54561 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54560)
store volatile %struct.ScmObj* %current_45args54561, %struct.ScmObj** %stackaddr$prim56137, align 8
%stackaddr$prim56138 = alloca %struct.ScmObj*, align 8
%_37take48040 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54561)
store volatile %struct.ScmObj* %_37take48040, %struct.ScmObj** %stackaddr$prim56138, align 8
%ae48749 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56139 = alloca %struct.ScmObj*, align 8
%fptrToInt56140 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48750 to i64
%ae48750 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56140)
store volatile %struct.ScmObj* %ae48750, %struct.ScmObj** %stackaddr$makeclosure56139, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48750, %struct.ScmObj* %_37take48040, i64 0)
%argslist54574$k484150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56141 = alloca %struct.ScmObj*, align 8
%argslist54574$k484151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48750, %struct.ScmObj* %argslist54574$k484150)
store volatile %struct.ScmObj* %argslist54574$k484151, %struct.ScmObj** %stackaddr$prim56141, align 8
%stackaddr$prim56142 = alloca %struct.ScmObj*, align 8
%argslist54574$k484152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48749, %struct.ScmObj* %argslist54574$k484151)
store volatile %struct.ScmObj* %argslist54574$k484152, %struct.ScmObj** %stackaddr$prim56142, align 8
%clofunc56143 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48415)
musttail call tailcc void %clofunc56143(%struct.ScmObj* %k48415, %struct.ScmObj* %argslist54574$k484152)
ret void
}

define tailcc void @proc_clo$ae48750(%struct.ScmObj* %env$ae48750,%struct.ScmObj* %current_45args54563) {
%stackaddr$env-ref56144 = alloca %struct.ScmObj*, align 8
%_37take48040 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48750, i64 0)
store %struct.ScmObj* %_37take48040, %struct.ScmObj** %stackaddr$env-ref56144
%stackaddr$prim56145 = alloca %struct.ScmObj*, align 8
%k48416 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54563)
store volatile %struct.ScmObj* %k48416, %struct.ScmObj** %stackaddr$prim56145, align 8
%stackaddr$prim56146 = alloca %struct.ScmObj*, align 8
%current_45args54564 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54563)
store volatile %struct.ScmObj* %current_45args54564, %struct.ScmObj** %stackaddr$prim56146, align 8
%stackaddr$prim56147 = alloca %struct.ScmObj*, align 8
%lst48042 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54564)
store volatile %struct.ScmObj* %lst48042, %struct.ScmObj** %stackaddr$prim56147, align 8
%stackaddr$prim56148 = alloca %struct.ScmObj*, align 8
%current_45args54565 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54564)
store volatile %struct.ScmObj* %current_45args54565, %struct.ScmObj** %stackaddr$prim56148, align 8
%stackaddr$prim56149 = alloca %struct.ScmObj*, align 8
%n48041 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54565)
store volatile %struct.ScmObj* %n48041, %struct.ScmObj** %stackaddr$prim56149, align 8
%ae48752 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56150 = alloca %struct.ScmObj*, align 8
%anf_45bind48165 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n48041, %struct.ScmObj* %ae48752)
store volatile %struct.ScmObj* %anf_45bind48165, %struct.ScmObj** %stackaddr$prim56150, align 8
%truthy$cmp56151 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48165)
%cmp$cmp56151 = icmp eq i64 %truthy$cmp56151, 1
br i1 %cmp$cmp56151, label %truebranch$cmp56151, label %falsebranch$cmp56151
truebranch$cmp56151:
%ae48755 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48756 = call %struct.ScmObj* @const_init_null()
%argslist54567$k484160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56152 = alloca %struct.ScmObj*, align 8
%argslist54567$k484161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48756, %struct.ScmObj* %argslist54567$k484160)
store volatile %struct.ScmObj* %argslist54567$k484161, %struct.ScmObj** %stackaddr$prim56152, align 8
%stackaddr$prim56153 = alloca %struct.ScmObj*, align 8
%argslist54567$k484162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48755, %struct.ScmObj* %argslist54567$k484161)
store volatile %struct.ScmObj* %argslist54567$k484162, %struct.ScmObj** %stackaddr$prim56153, align 8
%clofunc56154 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48416)
musttail call tailcc void %clofunc56154(%struct.ScmObj* %k48416, %struct.ScmObj* %argslist54567$k484162)
ret void
falsebranch$cmp56151:
%stackaddr$prim56155 = alloca %struct.ScmObj*, align 8
%anf_45bind48166 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48042)
store volatile %struct.ScmObj* %anf_45bind48166, %struct.ScmObj** %stackaddr$prim56155, align 8
%truthy$cmp56156 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48166)
%cmp$cmp56156 = icmp eq i64 %truthy$cmp56156, 1
br i1 %cmp$cmp56156, label %truebranch$cmp56156, label %falsebranch$cmp56156
truebranch$cmp56156:
%ae48766 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48767 = call %struct.ScmObj* @const_init_null()
%argslist54568$k484160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56157 = alloca %struct.ScmObj*, align 8
%argslist54568$k484161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48767, %struct.ScmObj* %argslist54568$k484160)
store volatile %struct.ScmObj* %argslist54568$k484161, %struct.ScmObj** %stackaddr$prim56157, align 8
%stackaddr$prim56158 = alloca %struct.ScmObj*, align 8
%argslist54568$k484162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48766, %struct.ScmObj* %argslist54568$k484161)
store volatile %struct.ScmObj* %argslist54568$k484162, %struct.ScmObj** %stackaddr$prim56158, align 8
%clofunc56159 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48416)
musttail call tailcc void %clofunc56159(%struct.ScmObj* %k48416, %struct.ScmObj* %argslist54568$k484162)
ret void
falsebranch$cmp56156:
%stackaddr$prim56160 = alloca %struct.ScmObj*, align 8
%anf_45bind48167 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48042)
store volatile %struct.ScmObj* %anf_45bind48167, %struct.ScmObj** %stackaddr$prim56160, align 8
%stackaddr$prim56161 = alloca %struct.ScmObj*, align 8
%anf_45bind48168 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48042)
store volatile %struct.ScmObj* %anf_45bind48168, %struct.ScmObj** %stackaddr$prim56161, align 8
%ae48777 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56162 = alloca %struct.ScmObj*, align 8
%anf_45bind48169 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n48041, %struct.ScmObj* %ae48777)
store volatile %struct.ScmObj* %anf_45bind48169, %struct.ScmObj** %stackaddr$prim56162, align 8
%stackaddr$makeclosure56163 = alloca %struct.ScmObj*, align 8
%fptrToInt56164 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48779 to i64
%ae48779 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56164)
store volatile %struct.ScmObj* %ae48779, %struct.ScmObj** %stackaddr$makeclosure56163, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48779, %struct.ScmObj* %anf_45bind48167, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48779, %struct.ScmObj* %k48416, i64 1)
%argslist54573$_37take480400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56165 = alloca %struct.ScmObj*, align 8
%argslist54573$_37take480401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48169, %struct.ScmObj* %argslist54573$_37take480400)
store volatile %struct.ScmObj* %argslist54573$_37take480401, %struct.ScmObj** %stackaddr$prim56165, align 8
%stackaddr$prim56166 = alloca %struct.ScmObj*, align 8
%argslist54573$_37take480402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48168, %struct.ScmObj* %argslist54573$_37take480401)
store volatile %struct.ScmObj* %argslist54573$_37take480402, %struct.ScmObj** %stackaddr$prim56166, align 8
%stackaddr$prim56167 = alloca %struct.ScmObj*, align 8
%argslist54573$_37take480403 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48779, %struct.ScmObj* %argslist54573$_37take480402)
store volatile %struct.ScmObj* %argslist54573$_37take480403, %struct.ScmObj** %stackaddr$prim56167, align 8
%clofunc56168 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take48040)
musttail call tailcc void %clofunc56168(%struct.ScmObj* %_37take48040, %struct.ScmObj* %argslist54573$_37take480403)
ret void
}

define tailcc void @proc_clo$ae48779(%struct.ScmObj* %env$ae48779,%struct.ScmObj* %current_45args54569) {
%stackaddr$env-ref56169 = alloca %struct.ScmObj*, align 8
%anf_45bind48167 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48779, i64 0)
store %struct.ScmObj* %anf_45bind48167, %struct.ScmObj** %stackaddr$env-ref56169
%stackaddr$env-ref56170 = alloca %struct.ScmObj*, align 8
%k48416 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48779, i64 1)
store %struct.ScmObj* %k48416, %struct.ScmObj** %stackaddr$env-ref56170
%stackaddr$prim56171 = alloca %struct.ScmObj*, align 8
%_95k48417 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54569)
store volatile %struct.ScmObj* %_95k48417, %struct.ScmObj** %stackaddr$prim56171, align 8
%stackaddr$prim56172 = alloca %struct.ScmObj*, align 8
%current_45args54570 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54569)
store volatile %struct.ScmObj* %current_45args54570, %struct.ScmObj** %stackaddr$prim56172, align 8
%stackaddr$prim56173 = alloca %struct.ScmObj*, align 8
%anf_45bind48170 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54570)
store volatile %struct.ScmObj* %anf_45bind48170, %struct.ScmObj** %stackaddr$prim56173, align 8
%stackaddr$prim56174 = alloca %struct.ScmObj*, align 8
%cpsprim48418 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48167, %struct.ScmObj* %anf_45bind48170)
store volatile %struct.ScmObj* %cpsprim48418, %struct.ScmObj** %stackaddr$prim56174, align 8
%ae48785 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54572$k484160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56175 = alloca %struct.ScmObj*, align 8
%argslist54572$k484161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48418, %struct.ScmObj* %argslist54572$k484160)
store volatile %struct.ScmObj* %argslist54572$k484161, %struct.ScmObj** %stackaddr$prim56175, align 8
%stackaddr$prim56176 = alloca %struct.ScmObj*, align 8
%argslist54572$k484162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48785, %struct.ScmObj* %argslist54572$k484161)
store volatile %struct.ScmObj* %argslist54572$k484162, %struct.ScmObj** %stackaddr$prim56176, align 8
%clofunc56177 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48416)
musttail call tailcc void %clofunc56177(%struct.ScmObj* %k48416, %struct.ScmObj* %argslist54572$k484162)
ret void
}

define tailcc void @proc_clo$ae48650(%struct.ScmObj* %env$ae48650,%struct.ScmObj* %current_45args54577) {
%stackaddr$prim56178 = alloca %struct.ScmObj*, align 8
%k48419 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54577)
store volatile %struct.ScmObj* %k48419, %struct.ScmObj** %stackaddr$prim56178, align 8
%stackaddr$prim56179 = alloca %struct.ScmObj*, align 8
%current_45args54578 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54577)
store volatile %struct.ScmObj* %current_45args54578, %struct.ScmObj** %stackaddr$prim56179, align 8
%stackaddr$prim56180 = alloca %struct.ScmObj*, align 8
%_37map48044 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54578)
store volatile %struct.ScmObj* %_37map48044, %struct.ScmObj** %stackaddr$prim56180, align 8
%ae48652 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56181 = alloca %struct.ScmObj*, align 8
%fptrToInt56182 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48653 to i64
%ae48653 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56182)
store volatile %struct.ScmObj* %ae48653, %struct.ScmObj** %stackaddr$makeclosure56181, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48653, %struct.ScmObj* %_37map48044, i64 0)
%argslist54594$k484190 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56183 = alloca %struct.ScmObj*, align 8
%argslist54594$k484191 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48653, %struct.ScmObj* %argslist54594$k484190)
store volatile %struct.ScmObj* %argslist54594$k484191, %struct.ScmObj** %stackaddr$prim56183, align 8
%stackaddr$prim56184 = alloca %struct.ScmObj*, align 8
%argslist54594$k484192 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48652, %struct.ScmObj* %argslist54594$k484191)
store volatile %struct.ScmObj* %argslist54594$k484192, %struct.ScmObj** %stackaddr$prim56184, align 8
%clofunc56185 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48419)
musttail call tailcc void %clofunc56185(%struct.ScmObj* %k48419, %struct.ScmObj* %argslist54594$k484192)
ret void
}

define tailcc void @proc_clo$ae48653(%struct.ScmObj* %env$ae48653,%struct.ScmObj* %current_45args54580) {
%stackaddr$env-ref56186 = alloca %struct.ScmObj*, align 8
%_37map48044 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48653, i64 0)
store %struct.ScmObj* %_37map48044, %struct.ScmObj** %stackaddr$env-ref56186
%stackaddr$prim56187 = alloca %struct.ScmObj*, align 8
%k48420 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54580)
store volatile %struct.ScmObj* %k48420, %struct.ScmObj** %stackaddr$prim56187, align 8
%stackaddr$prim56188 = alloca %struct.ScmObj*, align 8
%current_45args54581 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54580)
store volatile %struct.ScmObj* %current_45args54581, %struct.ScmObj** %stackaddr$prim56188, align 8
%stackaddr$prim56189 = alloca %struct.ScmObj*, align 8
%f48046 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54581)
store volatile %struct.ScmObj* %f48046, %struct.ScmObj** %stackaddr$prim56189, align 8
%stackaddr$prim56190 = alloca %struct.ScmObj*, align 8
%current_45args54582 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54581)
store volatile %struct.ScmObj* %current_45args54582, %struct.ScmObj** %stackaddr$prim56190, align 8
%stackaddr$prim56191 = alloca %struct.ScmObj*, align 8
%lst48045 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54582)
store volatile %struct.ScmObj* %lst48045, %struct.ScmObj** %stackaddr$prim56191, align 8
%stackaddr$prim56192 = alloca %struct.ScmObj*, align 8
%anf_45bind48159 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48045)
store volatile %struct.ScmObj* %anf_45bind48159, %struct.ScmObj** %stackaddr$prim56192, align 8
%truthy$cmp56193 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48159)
%cmp$cmp56193 = icmp eq i64 %truthy$cmp56193, 1
br i1 %cmp$cmp56193, label %truebranch$cmp56193, label %falsebranch$cmp56193
truebranch$cmp56193:
%ae48657 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48658 = call %struct.ScmObj* @const_init_null()
%argslist54584$k484200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56194 = alloca %struct.ScmObj*, align 8
%argslist54584$k484201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48658, %struct.ScmObj* %argslist54584$k484200)
store volatile %struct.ScmObj* %argslist54584$k484201, %struct.ScmObj** %stackaddr$prim56194, align 8
%stackaddr$prim56195 = alloca %struct.ScmObj*, align 8
%argslist54584$k484202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48657, %struct.ScmObj* %argslist54584$k484201)
store volatile %struct.ScmObj* %argslist54584$k484202, %struct.ScmObj** %stackaddr$prim56195, align 8
%clofunc56196 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48420)
musttail call tailcc void %clofunc56196(%struct.ScmObj* %k48420, %struct.ScmObj* %argslist54584$k484202)
ret void
falsebranch$cmp56193:
%stackaddr$prim56197 = alloca %struct.ScmObj*, align 8
%anf_45bind48160 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48045)
store volatile %struct.ScmObj* %anf_45bind48160, %struct.ScmObj** %stackaddr$prim56197, align 8
%stackaddr$makeclosure56198 = alloca %struct.ScmObj*, align 8
%fptrToInt56199 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48667 to i64
%ae48667 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56199)
store volatile %struct.ScmObj* %ae48667, %struct.ScmObj** %stackaddr$makeclosure56198, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48667, %struct.ScmObj* %k48420, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48667, %struct.ScmObj* %f48046, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48667, %struct.ScmObj* %lst48045, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48667, %struct.ScmObj* %_37map48044, i64 3)
%argslist54593$f480460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56200 = alloca %struct.ScmObj*, align 8
%argslist54593$f480461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48160, %struct.ScmObj* %argslist54593$f480460)
store volatile %struct.ScmObj* %argslist54593$f480461, %struct.ScmObj** %stackaddr$prim56200, align 8
%stackaddr$prim56201 = alloca %struct.ScmObj*, align 8
%argslist54593$f480462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48667, %struct.ScmObj* %argslist54593$f480461)
store volatile %struct.ScmObj* %argslist54593$f480462, %struct.ScmObj** %stackaddr$prim56201, align 8
%clofunc56202 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48046)
musttail call tailcc void %clofunc56202(%struct.ScmObj* %f48046, %struct.ScmObj* %argslist54593$f480462)
ret void
}

define tailcc void @proc_clo$ae48667(%struct.ScmObj* %env$ae48667,%struct.ScmObj* %current_45args54585) {
%stackaddr$env-ref56203 = alloca %struct.ScmObj*, align 8
%k48420 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48667, i64 0)
store %struct.ScmObj* %k48420, %struct.ScmObj** %stackaddr$env-ref56203
%stackaddr$env-ref56204 = alloca %struct.ScmObj*, align 8
%f48046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48667, i64 1)
store %struct.ScmObj* %f48046, %struct.ScmObj** %stackaddr$env-ref56204
%stackaddr$env-ref56205 = alloca %struct.ScmObj*, align 8
%lst48045 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48667, i64 2)
store %struct.ScmObj* %lst48045, %struct.ScmObj** %stackaddr$env-ref56205
%stackaddr$env-ref56206 = alloca %struct.ScmObj*, align 8
%_37map48044 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48667, i64 3)
store %struct.ScmObj* %_37map48044, %struct.ScmObj** %stackaddr$env-ref56206
%stackaddr$prim56207 = alloca %struct.ScmObj*, align 8
%_95k48421 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54585)
store volatile %struct.ScmObj* %_95k48421, %struct.ScmObj** %stackaddr$prim56207, align 8
%stackaddr$prim56208 = alloca %struct.ScmObj*, align 8
%current_45args54586 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54585)
store volatile %struct.ScmObj* %current_45args54586, %struct.ScmObj** %stackaddr$prim56208, align 8
%stackaddr$prim56209 = alloca %struct.ScmObj*, align 8
%anf_45bind48161 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54586)
store volatile %struct.ScmObj* %anf_45bind48161, %struct.ScmObj** %stackaddr$prim56209, align 8
%stackaddr$prim56210 = alloca %struct.ScmObj*, align 8
%anf_45bind48162 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48045)
store volatile %struct.ScmObj* %anf_45bind48162, %struct.ScmObj** %stackaddr$prim56210, align 8
%stackaddr$makeclosure56211 = alloca %struct.ScmObj*, align 8
%fptrToInt56212 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48671 to i64
%ae48671 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56212)
store volatile %struct.ScmObj* %ae48671, %struct.ScmObj** %stackaddr$makeclosure56211, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48671, %struct.ScmObj* %k48420, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48671, %struct.ScmObj* %anf_45bind48161, i64 1)
%argslist54592$_37map480440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56213 = alloca %struct.ScmObj*, align 8
%argslist54592$_37map480441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48162, %struct.ScmObj* %argslist54592$_37map480440)
store volatile %struct.ScmObj* %argslist54592$_37map480441, %struct.ScmObj** %stackaddr$prim56213, align 8
%stackaddr$prim56214 = alloca %struct.ScmObj*, align 8
%argslist54592$_37map480442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48046, %struct.ScmObj* %argslist54592$_37map480441)
store volatile %struct.ScmObj* %argslist54592$_37map480442, %struct.ScmObj** %stackaddr$prim56214, align 8
%stackaddr$prim56215 = alloca %struct.ScmObj*, align 8
%argslist54592$_37map480443 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48671, %struct.ScmObj* %argslist54592$_37map480442)
store volatile %struct.ScmObj* %argslist54592$_37map480443, %struct.ScmObj** %stackaddr$prim56215, align 8
%clofunc56216 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map48044)
musttail call tailcc void %clofunc56216(%struct.ScmObj* %_37map48044, %struct.ScmObj* %argslist54592$_37map480443)
ret void
}

define tailcc void @proc_clo$ae48671(%struct.ScmObj* %env$ae48671,%struct.ScmObj* %current_45args54588) {
%stackaddr$env-ref56217 = alloca %struct.ScmObj*, align 8
%k48420 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48671, i64 0)
store %struct.ScmObj* %k48420, %struct.ScmObj** %stackaddr$env-ref56217
%stackaddr$env-ref56218 = alloca %struct.ScmObj*, align 8
%anf_45bind48161 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48671, i64 1)
store %struct.ScmObj* %anf_45bind48161, %struct.ScmObj** %stackaddr$env-ref56218
%stackaddr$prim56219 = alloca %struct.ScmObj*, align 8
%_95k48422 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54588)
store volatile %struct.ScmObj* %_95k48422, %struct.ScmObj** %stackaddr$prim56219, align 8
%stackaddr$prim56220 = alloca %struct.ScmObj*, align 8
%current_45args54589 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54588)
store volatile %struct.ScmObj* %current_45args54589, %struct.ScmObj** %stackaddr$prim56220, align 8
%stackaddr$prim56221 = alloca %struct.ScmObj*, align 8
%anf_45bind48163 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54589)
store volatile %struct.ScmObj* %anf_45bind48163, %struct.ScmObj** %stackaddr$prim56221, align 8
%stackaddr$prim56222 = alloca %struct.ScmObj*, align 8
%cpsprim48423 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48161, %struct.ScmObj* %anf_45bind48163)
store volatile %struct.ScmObj* %cpsprim48423, %struct.ScmObj** %stackaddr$prim56222, align 8
%ae48677 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54591$k484200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56223 = alloca %struct.ScmObj*, align 8
%argslist54591$k484201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48423, %struct.ScmObj* %argslist54591$k484200)
store volatile %struct.ScmObj* %argslist54591$k484201, %struct.ScmObj** %stackaddr$prim56223, align 8
%stackaddr$prim56224 = alloca %struct.ScmObj*, align 8
%argslist54591$k484202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48677, %struct.ScmObj* %argslist54591$k484201)
store volatile %struct.ScmObj* %argslist54591$k484202, %struct.ScmObj** %stackaddr$prim56224, align 8
%clofunc56225 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48420)
musttail call tailcc void %clofunc56225(%struct.ScmObj* %k48420, %struct.ScmObj* %argslist54591$k484202)
ret void
}

define tailcc void @proc_clo$ae48570(%struct.ScmObj* %env$ae48570,%struct.ScmObj* %current_45args54597) {
%stackaddr$prim56226 = alloca %struct.ScmObj*, align 8
%k48424 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54597)
store volatile %struct.ScmObj* %k48424, %struct.ScmObj** %stackaddr$prim56226, align 8
%stackaddr$prim56227 = alloca %struct.ScmObj*, align 8
%current_45args54598 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54597)
store volatile %struct.ScmObj* %current_45args54598, %struct.ScmObj** %stackaddr$prim56227, align 8
%stackaddr$prim56228 = alloca %struct.ScmObj*, align 8
%_37foldr148048 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54598)
store volatile %struct.ScmObj* %_37foldr148048, %struct.ScmObj** %stackaddr$prim56228, align 8
%ae48572 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56229 = alloca %struct.ScmObj*, align 8
%fptrToInt56230 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48573 to i64
%ae48573 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56230)
store volatile %struct.ScmObj* %ae48573, %struct.ScmObj** %stackaddr$makeclosure56229, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48573, %struct.ScmObj* %_37foldr148048, i64 0)
%argslist54611$k484240 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56231 = alloca %struct.ScmObj*, align 8
%argslist54611$k484241 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48573, %struct.ScmObj* %argslist54611$k484240)
store volatile %struct.ScmObj* %argslist54611$k484241, %struct.ScmObj** %stackaddr$prim56231, align 8
%stackaddr$prim56232 = alloca %struct.ScmObj*, align 8
%argslist54611$k484242 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48572, %struct.ScmObj* %argslist54611$k484241)
store volatile %struct.ScmObj* %argslist54611$k484242, %struct.ScmObj** %stackaddr$prim56232, align 8
%clofunc56233 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48424)
musttail call tailcc void %clofunc56233(%struct.ScmObj* %k48424, %struct.ScmObj* %argslist54611$k484242)
ret void
}

define tailcc void @proc_clo$ae48573(%struct.ScmObj* %env$ae48573,%struct.ScmObj* %current_45args54600) {
%stackaddr$env-ref56234 = alloca %struct.ScmObj*, align 8
%_37foldr148048 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48573, i64 0)
store %struct.ScmObj* %_37foldr148048, %struct.ScmObj** %stackaddr$env-ref56234
%stackaddr$prim56235 = alloca %struct.ScmObj*, align 8
%k48425 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54600)
store volatile %struct.ScmObj* %k48425, %struct.ScmObj** %stackaddr$prim56235, align 8
%stackaddr$prim56236 = alloca %struct.ScmObj*, align 8
%current_45args54601 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54600)
store volatile %struct.ScmObj* %current_45args54601, %struct.ScmObj** %stackaddr$prim56236, align 8
%stackaddr$prim56237 = alloca %struct.ScmObj*, align 8
%f48051 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54601)
store volatile %struct.ScmObj* %f48051, %struct.ScmObj** %stackaddr$prim56237, align 8
%stackaddr$prim56238 = alloca %struct.ScmObj*, align 8
%current_45args54602 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54601)
store volatile %struct.ScmObj* %current_45args54602, %struct.ScmObj** %stackaddr$prim56238, align 8
%stackaddr$prim56239 = alloca %struct.ScmObj*, align 8
%acc48050 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54602)
store volatile %struct.ScmObj* %acc48050, %struct.ScmObj** %stackaddr$prim56239, align 8
%stackaddr$prim56240 = alloca %struct.ScmObj*, align 8
%current_45args54603 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54602)
store volatile %struct.ScmObj* %current_45args54603, %struct.ScmObj** %stackaddr$prim56240, align 8
%stackaddr$prim56241 = alloca %struct.ScmObj*, align 8
%lst48049 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54603)
store volatile %struct.ScmObj* %lst48049, %struct.ScmObj** %stackaddr$prim56241, align 8
%stackaddr$prim56242 = alloca %struct.ScmObj*, align 8
%anf_45bind48154 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48049)
store volatile %struct.ScmObj* %anf_45bind48154, %struct.ScmObj** %stackaddr$prim56242, align 8
%truthy$cmp56243 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48154)
%cmp$cmp56243 = icmp eq i64 %truthy$cmp56243, 1
br i1 %cmp$cmp56243, label %truebranch$cmp56243, label %falsebranch$cmp56243
truebranch$cmp56243:
%ae48577 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54605$k484250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56244 = alloca %struct.ScmObj*, align 8
%argslist54605$k484251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48050, %struct.ScmObj* %argslist54605$k484250)
store volatile %struct.ScmObj* %argslist54605$k484251, %struct.ScmObj** %stackaddr$prim56244, align 8
%stackaddr$prim56245 = alloca %struct.ScmObj*, align 8
%argslist54605$k484252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48577, %struct.ScmObj* %argslist54605$k484251)
store volatile %struct.ScmObj* %argslist54605$k484252, %struct.ScmObj** %stackaddr$prim56245, align 8
%clofunc56246 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48425)
musttail call tailcc void %clofunc56246(%struct.ScmObj* %k48425, %struct.ScmObj* %argslist54605$k484252)
ret void
falsebranch$cmp56243:
%stackaddr$prim56247 = alloca %struct.ScmObj*, align 8
%anf_45bind48155 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48049)
store volatile %struct.ScmObj* %anf_45bind48155, %struct.ScmObj** %stackaddr$prim56247, align 8
%stackaddr$prim56248 = alloca %struct.ScmObj*, align 8
%anf_45bind48156 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48049)
store volatile %struct.ScmObj* %anf_45bind48156, %struct.ScmObj** %stackaddr$prim56248, align 8
%stackaddr$makeclosure56249 = alloca %struct.ScmObj*, align 8
%fptrToInt56250 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48585 to i64
%ae48585 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56250)
store volatile %struct.ScmObj* %ae48585, %struct.ScmObj** %stackaddr$makeclosure56249, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48585, %struct.ScmObj* %k48425, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48585, %struct.ScmObj* %f48051, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48585, %struct.ScmObj* %anf_45bind48155, i64 2)
%argslist54610$_37foldr1480480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56251 = alloca %struct.ScmObj*, align 8
%argslist54610$_37foldr1480481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48156, %struct.ScmObj* %argslist54610$_37foldr1480480)
store volatile %struct.ScmObj* %argslist54610$_37foldr1480481, %struct.ScmObj** %stackaddr$prim56251, align 8
%stackaddr$prim56252 = alloca %struct.ScmObj*, align 8
%argslist54610$_37foldr1480482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48050, %struct.ScmObj* %argslist54610$_37foldr1480481)
store volatile %struct.ScmObj* %argslist54610$_37foldr1480482, %struct.ScmObj** %stackaddr$prim56252, align 8
%stackaddr$prim56253 = alloca %struct.ScmObj*, align 8
%argslist54610$_37foldr1480483 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48051, %struct.ScmObj* %argslist54610$_37foldr1480482)
store volatile %struct.ScmObj* %argslist54610$_37foldr1480483, %struct.ScmObj** %stackaddr$prim56253, align 8
%stackaddr$prim56254 = alloca %struct.ScmObj*, align 8
%argslist54610$_37foldr1480484 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48585, %struct.ScmObj* %argslist54610$_37foldr1480483)
store volatile %struct.ScmObj* %argslist54610$_37foldr1480484, %struct.ScmObj** %stackaddr$prim56254, align 8
%clofunc56255 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148048)
musttail call tailcc void %clofunc56255(%struct.ScmObj* %_37foldr148048, %struct.ScmObj* %argslist54610$_37foldr1480484)
ret void
}

define tailcc void @proc_clo$ae48585(%struct.ScmObj* %env$ae48585,%struct.ScmObj* %current_45args54606) {
%stackaddr$env-ref56256 = alloca %struct.ScmObj*, align 8
%k48425 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48585, i64 0)
store %struct.ScmObj* %k48425, %struct.ScmObj** %stackaddr$env-ref56256
%stackaddr$env-ref56257 = alloca %struct.ScmObj*, align 8
%f48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48585, i64 1)
store %struct.ScmObj* %f48051, %struct.ScmObj** %stackaddr$env-ref56257
%stackaddr$env-ref56258 = alloca %struct.ScmObj*, align 8
%anf_45bind48155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48585, i64 2)
store %struct.ScmObj* %anf_45bind48155, %struct.ScmObj** %stackaddr$env-ref56258
%stackaddr$prim56259 = alloca %struct.ScmObj*, align 8
%_95k48426 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54606)
store volatile %struct.ScmObj* %_95k48426, %struct.ScmObj** %stackaddr$prim56259, align 8
%stackaddr$prim56260 = alloca %struct.ScmObj*, align 8
%current_45args54607 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54606)
store volatile %struct.ScmObj* %current_45args54607, %struct.ScmObj** %stackaddr$prim56260, align 8
%stackaddr$prim56261 = alloca %struct.ScmObj*, align 8
%anf_45bind48157 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54607)
store volatile %struct.ScmObj* %anf_45bind48157, %struct.ScmObj** %stackaddr$prim56261, align 8
%argslist54609$f480510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56262 = alloca %struct.ScmObj*, align 8
%argslist54609$f480511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48157, %struct.ScmObj* %argslist54609$f480510)
store volatile %struct.ScmObj* %argslist54609$f480511, %struct.ScmObj** %stackaddr$prim56262, align 8
%stackaddr$prim56263 = alloca %struct.ScmObj*, align 8
%argslist54609$f480512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48155, %struct.ScmObj* %argslist54609$f480511)
store volatile %struct.ScmObj* %argslist54609$f480512, %struct.ScmObj** %stackaddr$prim56263, align 8
%stackaddr$prim56264 = alloca %struct.ScmObj*, align 8
%argslist54609$f480513 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48425, %struct.ScmObj* %argslist54609$f480512)
store volatile %struct.ScmObj* %argslist54609$f480513, %struct.ScmObj** %stackaddr$prim56264, align 8
%clofunc56265 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48051)
musttail call tailcc void %clofunc56265(%struct.ScmObj* %f48051, %struct.ScmObj* %argslist54609$f480513)
ret void
}

define tailcc void @proc_clo$ae48453(%struct.ScmObj* %env$ae48453,%struct.ScmObj* %current_45args54614) {
%stackaddr$prim56266 = alloca %struct.ScmObj*, align 8
%k48427 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54614)
store volatile %struct.ScmObj* %k48427, %struct.ScmObj** %stackaddr$prim56266, align 8
%stackaddr$prim56267 = alloca %struct.ScmObj*, align 8
%current_45args54615 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54614)
store volatile %struct.ScmObj* %current_45args54615, %struct.ScmObj** %stackaddr$prim56267, align 8
%stackaddr$prim56268 = alloca %struct.ScmObj*, align 8
%y48028 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54615)
store volatile %struct.ScmObj* %y48028, %struct.ScmObj** %stackaddr$prim56268, align 8
%ae48455 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56269 = alloca %struct.ScmObj*, align 8
%fptrToInt56270 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48456 to i64
%ae48456 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56270)
store volatile %struct.ScmObj* %ae48456, %struct.ScmObj** %stackaddr$makeclosure56269, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48456, %struct.ScmObj* %y48028, i64 0)
%argslist54633$k484270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56271 = alloca %struct.ScmObj*, align 8
%argslist54633$k484271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48456, %struct.ScmObj* %argslist54633$k484270)
store volatile %struct.ScmObj* %argslist54633$k484271, %struct.ScmObj** %stackaddr$prim56271, align 8
%stackaddr$prim56272 = alloca %struct.ScmObj*, align 8
%argslist54633$k484272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48455, %struct.ScmObj* %argslist54633$k484271)
store volatile %struct.ScmObj* %argslist54633$k484272, %struct.ScmObj** %stackaddr$prim56272, align 8
%clofunc56273 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48427)
musttail call tailcc void %clofunc56273(%struct.ScmObj* %k48427, %struct.ScmObj* %argslist54633$k484272)
ret void
}

define tailcc void @proc_clo$ae48456(%struct.ScmObj* %env$ae48456,%struct.ScmObj* %current_45args54617) {
%stackaddr$env-ref56274 = alloca %struct.ScmObj*, align 8
%y48028 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48456, i64 0)
store %struct.ScmObj* %y48028, %struct.ScmObj** %stackaddr$env-ref56274
%stackaddr$prim56275 = alloca %struct.ScmObj*, align 8
%k48428 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54617)
store volatile %struct.ScmObj* %k48428, %struct.ScmObj** %stackaddr$prim56275, align 8
%stackaddr$prim56276 = alloca %struct.ScmObj*, align 8
%current_45args54618 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54617)
store volatile %struct.ScmObj* %current_45args54618, %struct.ScmObj** %stackaddr$prim56276, align 8
%stackaddr$prim56277 = alloca %struct.ScmObj*, align 8
%f48029 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54618)
store volatile %struct.ScmObj* %f48029, %struct.ScmObj** %stackaddr$prim56277, align 8
%stackaddr$makeclosure56278 = alloca %struct.ScmObj*, align 8
%fptrToInt56279 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48457 to i64
%ae48457 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56279)
store volatile %struct.ScmObj* %ae48457, %struct.ScmObj** %stackaddr$makeclosure56278, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48457, %struct.ScmObj* %f48029, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48457, %struct.ScmObj* %k48428, i64 1)
%ae48458 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56280 = alloca %struct.ScmObj*, align 8
%fptrToInt56281 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48459 to i64
%ae48459 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56281)
store volatile %struct.ScmObj* %ae48459, %struct.ScmObj** %stackaddr$makeclosure56280, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48459, %struct.ScmObj* %f48029, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48459, %struct.ScmObj* %y48028, i64 1)
%argslist54632$ae484570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56282 = alloca %struct.ScmObj*, align 8
%argslist54632$ae484571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48459, %struct.ScmObj* %argslist54632$ae484570)
store volatile %struct.ScmObj* %argslist54632$ae484571, %struct.ScmObj** %stackaddr$prim56282, align 8
%stackaddr$prim56283 = alloca %struct.ScmObj*, align 8
%argslist54632$ae484572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48458, %struct.ScmObj* %argslist54632$ae484571)
store volatile %struct.ScmObj* %argslist54632$ae484572, %struct.ScmObj** %stackaddr$prim56283, align 8
%clofunc56284 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48457)
musttail call tailcc void %clofunc56284(%struct.ScmObj* %ae48457, %struct.ScmObj* %argslist54632$ae484572)
ret void
}

define tailcc void @proc_clo$ae48457(%struct.ScmObj* %env$ae48457,%struct.ScmObj* %current_45args54620) {
%stackaddr$env-ref56285 = alloca %struct.ScmObj*, align 8
%f48029 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48457, i64 0)
store %struct.ScmObj* %f48029, %struct.ScmObj** %stackaddr$env-ref56285
%stackaddr$env-ref56286 = alloca %struct.ScmObj*, align 8
%k48428 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48457, i64 1)
store %struct.ScmObj* %k48428, %struct.ScmObj** %stackaddr$env-ref56286
%stackaddr$prim56287 = alloca %struct.ScmObj*, align 8
%_95k48429 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54620)
store volatile %struct.ScmObj* %_95k48429, %struct.ScmObj** %stackaddr$prim56287, align 8
%stackaddr$prim56288 = alloca %struct.ScmObj*, align 8
%current_45args54621 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54620)
store volatile %struct.ScmObj* %current_45args54621, %struct.ScmObj** %stackaddr$prim56288, align 8
%stackaddr$prim56289 = alloca %struct.ScmObj*, align 8
%anf_45bind48152 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54621)
store volatile %struct.ScmObj* %anf_45bind48152, %struct.ScmObj** %stackaddr$prim56289, align 8
%argslist54623$f480290 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56290 = alloca %struct.ScmObj*, align 8
%argslist54623$f480291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48152, %struct.ScmObj* %argslist54623$f480290)
store volatile %struct.ScmObj* %argslist54623$f480291, %struct.ScmObj** %stackaddr$prim56290, align 8
%stackaddr$prim56291 = alloca %struct.ScmObj*, align 8
%argslist54623$f480292 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48428, %struct.ScmObj* %argslist54623$f480291)
store volatile %struct.ScmObj* %argslist54623$f480292, %struct.ScmObj** %stackaddr$prim56291, align 8
%clofunc56292 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48029)
musttail call tailcc void %clofunc56292(%struct.ScmObj* %f48029, %struct.ScmObj* %argslist54623$f480292)
ret void
}

define tailcc void @proc_clo$ae48459(%struct.ScmObj* %env$ae48459,%struct.ScmObj* %args4803048430) {
%stackaddr$env-ref56293 = alloca %struct.ScmObj*, align 8
%f48029 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48459, i64 0)
store %struct.ScmObj* %f48029, %struct.ScmObj** %stackaddr$env-ref56293
%stackaddr$env-ref56294 = alloca %struct.ScmObj*, align 8
%y48028 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48459, i64 1)
store %struct.ScmObj* %y48028, %struct.ScmObj** %stackaddr$env-ref56294
%stackaddr$prim56295 = alloca %struct.ScmObj*, align 8
%k48431 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4803048430)
store volatile %struct.ScmObj* %k48431, %struct.ScmObj** %stackaddr$prim56295, align 8
%stackaddr$prim56296 = alloca %struct.ScmObj*, align 8
%args48030 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4803048430)
store volatile %struct.ScmObj* %args48030, %struct.ScmObj** %stackaddr$prim56296, align 8
%stackaddr$makeclosure56297 = alloca %struct.ScmObj*, align 8
%fptrToInt56298 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48463 to i64
%ae48463 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56298)
store volatile %struct.ScmObj* %ae48463, %struct.ScmObj** %stackaddr$makeclosure56297, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48463, %struct.ScmObj* %k48431, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48463, %struct.ScmObj* %args48030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48463, %struct.ScmObj* %f48029, i64 2)
%argslist54631$y480280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56299 = alloca %struct.ScmObj*, align 8
%argslist54631$y480281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y48028, %struct.ScmObj* %argslist54631$y480280)
store volatile %struct.ScmObj* %argslist54631$y480281, %struct.ScmObj** %stackaddr$prim56299, align 8
%stackaddr$prim56300 = alloca %struct.ScmObj*, align 8
%argslist54631$y480282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48463, %struct.ScmObj* %argslist54631$y480281)
store volatile %struct.ScmObj* %argslist54631$y480282, %struct.ScmObj** %stackaddr$prim56300, align 8
%clofunc56301 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y48028)
musttail call tailcc void %clofunc56301(%struct.ScmObj* %y48028, %struct.ScmObj* %argslist54631$y480282)
ret void
}

define tailcc void @proc_clo$ae48463(%struct.ScmObj* %env$ae48463,%struct.ScmObj* %current_45args54624) {
%stackaddr$env-ref56302 = alloca %struct.ScmObj*, align 8
%k48431 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48463, i64 0)
store %struct.ScmObj* %k48431, %struct.ScmObj** %stackaddr$env-ref56302
%stackaddr$env-ref56303 = alloca %struct.ScmObj*, align 8
%args48030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48463, i64 1)
store %struct.ScmObj* %args48030, %struct.ScmObj** %stackaddr$env-ref56303
%stackaddr$env-ref56304 = alloca %struct.ScmObj*, align 8
%f48029 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48463, i64 2)
store %struct.ScmObj* %f48029, %struct.ScmObj** %stackaddr$env-ref56304
%stackaddr$prim56305 = alloca %struct.ScmObj*, align 8
%_95k48432 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54624)
store volatile %struct.ScmObj* %_95k48432, %struct.ScmObj** %stackaddr$prim56305, align 8
%stackaddr$prim56306 = alloca %struct.ScmObj*, align 8
%current_45args54625 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54624)
store volatile %struct.ScmObj* %current_45args54625, %struct.ScmObj** %stackaddr$prim56306, align 8
%stackaddr$prim56307 = alloca %struct.ScmObj*, align 8
%anf_45bind48150 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54625)
store volatile %struct.ScmObj* %anf_45bind48150, %struct.ScmObj** %stackaddr$prim56307, align 8
%stackaddr$makeclosure56308 = alloca %struct.ScmObj*, align 8
%fptrToInt56309 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48466 to i64
%ae48466 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56309)
store volatile %struct.ScmObj* %ae48466, %struct.ScmObj** %stackaddr$makeclosure56308, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48466, %struct.ScmObj* %k48431, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48466, %struct.ScmObj* %args48030, i64 1)
%argslist54630$anf_45bind481500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56310 = alloca %struct.ScmObj*, align 8
%argslist54630$anf_45bind481501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48029, %struct.ScmObj* %argslist54630$anf_45bind481500)
store volatile %struct.ScmObj* %argslist54630$anf_45bind481501, %struct.ScmObj** %stackaddr$prim56310, align 8
%stackaddr$prim56311 = alloca %struct.ScmObj*, align 8
%argslist54630$anf_45bind481502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48466, %struct.ScmObj* %argslist54630$anf_45bind481501)
store volatile %struct.ScmObj* %argslist54630$anf_45bind481502, %struct.ScmObj** %stackaddr$prim56311, align 8
%clofunc56312 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48150)
musttail call tailcc void %clofunc56312(%struct.ScmObj* %anf_45bind48150, %struct.ScmObj* %argslist54630$anf_45bind481502)
ret void
}

define tailcc void @proc_clo$ae48466(%struct.ScmObj* %env$ae48466,%struct.ScmObj* %current_45args54627) {
%stackaddr$env-ref56313 = alloca %struct.ScmObj*, align 8
%k48431 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48466, i64 0)
store %struct.ScmObj* %k48431, %struct.ScmObj** %stackaddr$env-ref56313
%stackaddr$env-ref56314 = alloca %struct.ScmObj*, align 8
%args48030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48466, i64 1)
store %struct.ScmObj* %args48030, %struct.ScmObj** %stackaddr$env-ref56314
%stackaddr$prim56315 = alloca %struct.ScmObj*, align 8
%_95k48433 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54627)
store volatile %struct.ScmObj* %_95k48433, %struct.ScmObj** %stackaddr$prim56315, align 8
%stackaddr$prim56316 = alloca %struct.ScmObj*, align 8
%current_45args54628 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54627)
store volatile %struct.ScmObj* %current_45args54628, %struct.ScmObj** %stackaddr$prim56316, align 8
%stackaddr$prim56317 = alloca %struct.ScmObj*, align 8
%anf_45bind48151 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54628)
store volatile %struct.ScmObj* %anf_45bind48151, %struct.ScmObj** %stackaddr$prim56317, align 8
%stackaddr$prim56318 = alloca %struct.ScmObj*, align 8
%cpsargs48434 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48431, %struct.ScmObj* %args48030)
store volatile %struct.ScmObj* %cpsargs48434, %struct.ScmObj** %stackaddr$prim56318, align 8
%clofunc56319 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48151)
musttail call tailcc void %clofunc56319(%struct.ScmObj* %anf_45bind48151, %struct.ScmObj* %cpsargs48434)
ret void
}

define tailcc void @proc_clo$ae48438(%struct.ScmObj* %env$ae48438,%struct.ScmObj* %current_45args54635) {
%stackaddr$prim56320 = alloca %struct.ScmObj*, align 8
%k48435 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54635)
store volatile %struct.ScmObj* %k48435, %struct.ScmObj** %stackaddr$prim56320, align 8
%stackaddr$prim56321 = alloca %struct.ScmObj*, align 8
%current_45args54636 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54635)
store volatile %struct.ScmObj* %current_45args54636, %struct.ScmObj** %stackaddr$prim56321, align 8
%stackaddr$prim56322 = alloca %struct.ScmObj*, align 8
%yu48027 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54636)
store volatile %struct.ScmObj* %yu48027, %struct.ScmObj** %stackaddr$prim56322, align 8
%argslist54638$yu480270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56323 = alloca %struct.ScmObj*, align 8
%argslist54638$yu480271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu48027, %struct.ScmObj* %argslist54638$yu480270)
store volatile %struct.ScmObj* %argslist54638$yu480271, %struct.ScmObj** %stackaddr$prim56323, align 8
%stackaddr$prim56324 = alloca %struct.ScmObj*, align 8
%argslist54638$yu480272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48435, %struct.ScmObj* %argslist54638$yu480271)
store volatile %struct.ScmObj* %argslist54638$yu480272, %struct.ScmObj** %stackaddr$prim56324, align 8
%clofunc56325 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu48027)
musttail call tailcc void %clofunc56325(%struct.ScmObj* %yu48027, %struct.ScmObj* %argslist54638$yu480272)
ret void
}