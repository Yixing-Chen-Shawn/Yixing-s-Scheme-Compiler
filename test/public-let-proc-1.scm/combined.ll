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

@global$sym$ae4391047430 = private unnamed_addr constant [8 x i8] c"promise\00", align 8

define ccc i32 @main() {
%mainenv46935 = call %struct.ScmObj* @const_init_null()
%mainargs46936 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv46935, %struct.ScmObj* %mainargs46936)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv46933,%struct.ScmObj* %mainargs46934) {
%stackaddr$makeclosure46937 = alloca %struct.ScmObj*, align 8
%fptrToInt46938 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40522 to i64
%ae40522 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt46938)
store volatile %struct.ScmObj* %ae40522, %struct.ScmObj** %stackaddr$makeclosure46937, align 8
%ae40523 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure46939 = alloca %struct.ScmObj*, align 8
%fptrToInt46940 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40524 to i64
%ae40524 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt46940)
store volatile %struct.ScmObj* %ae40524, %struct.ScmObj** %stackaddr$makeclosure46939, align 8
%args46932$ae40522$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim46941 = alloca %struct.ScmObj*, align 8
%args46932$ae40522$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40524, %struct.ScmObj* %args46932$ae40522$0)
store volatile %struct.ScmObj* %args46932$ae40522$1, %struct.ScmObj** %stackaddr$prim46941, align 8
%stackaddr$prim46942 = alloca %struct.ScmObj*, align 8
%args46932$ae40522$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40523, %struct.ScmObj* %args46932$ae40522$1)
store volatile %struct.ScmObj* %args46932$ae40522$2, %struct.ScmObj** %stackaddr$prim46942, align 8
%clofunc46943 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40522)
musttail call tailcc void %clofunc46943(%struct.ScmObj* %ae40522, %struct.ScmObj* %args46932$ae40522$2)
ret void
}

define tailcc void @proc_clo$ae40522(%struct.ScmObj* %env$ae40522,%struct.ScmObj* %current_45args46373) {
%stackaddr$prim46944 = alloca %struct.ScmObj*, align 8
%_95k40351 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46373)
store volatile %struct.ScmObj* %_95k40351, %struct.ScmObj** %stackaddr$prim46944, align 8
%stackaddr$prim46945 = alloca %struct.ScmObj*, align 8
%current_45args46374 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46373)
store volatile %struct.ScmObj* %current_45args46374, %struct.ScmObj** %stackaddr$prim46945, align 8
%stackaddr$prim46946 = alloca %struct.ScmObj*, align 8
%anf_45bind40228 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46374)
store volatile %struct.ScmObj* %anf_45bind40228, %struct.ScmObj** %stackaddr$prim46946, align 8
%stackaddr$makeclosure46947 = alloca %struct.ScmObj*, align 8
%fptrToInt46948 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40537 to i64
%ae40537 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt46948)
store volatile %struct.ScmObj* %ae40537, %struct.ScmObj** %stackaddr$makeclosure46947, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40537, %struct.ScmObj* %anf_45bind40228, i64 0)
%ae40538 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure46949 = alloca %struct.ScmObj*, align 8
%fptrToInt46950 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40539 to i64
%ae40539 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt46950)
store volatile %struct.ScmObj* %ae40539, %struct.ScmObj** %stackaddr$makeclosure46949, align 8
%args46927$ae40537$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim46951 = alloca %struct.ScmObj*, align 8
%args46927$ae40537$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40539, %struct.ScmObj* %args46927$ae40537$0)
store volatile %struct.ScmObj* %args46927$ae40537$1, %struct.ScmObj** %stackaddr$prim46951, align 8
%stackaddr$prim46952 = alloca %struct.ScmObj*, align 8
%args46927$ae40537$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40538, %struct.ScmObj* %args46927$ae40537$1)
store volatile %struct.ScmObj* %args46927$ae40537$2, %struct.ScmObj** %stackaddr$prim46952, align 8
%clofunc46953 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40537)
musttail call tailcc void %clofunc46953(%struct.ScmObj* %ae40537, %struct.ScmObj* %args46927$ae40537$2)
ret void
}

define tailcc void @proc_clo$ae40537(%struct.ScmObj* %env$ae40537,%struct.ScmObj* %current_45args46376) {
%stackaddr$env-ref46954 = alloca %struct.ScmObj*, align 8
%anf_45bind40228 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40537, i64 0)
store %struct.ScmObj* %anf_45bind40228, %struct.ScmObj** %stackaddr$env-ref46954
%stackaddr$prim46955 = alloca %struct.ScmObj*, align 8
%_95k40352 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46376)
store volatile %struct.ScmObj* %_95k40352, %struct.ScmObj** %stackaddr$prim46955, align 8
%stackaddr$prim46956 = alloca %struct.ScmObj*, align 8
%current_45args46377 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46376)
store volatile %struct.ScmObj* %current_45args46377, %struct.ScmObj** %stackaddr$prim46956, align 8
%stackaddr$prim46957 = alloca %struct.ScmObj*, align 8
%anf_45bind40232 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46377)
store volatile %struct.ScmObj* %anf_45bind40232, %struct.ScmObj** %stackaddr$prim46957, align 8
%stackaddr$makeclosure46958 = alloca %struct.ScmObj*, align 8
%fptrToInt46959 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40652 to i64
%ae40652 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt46959)
store volatile %struct.ScmObj* %ae40652, %struct.ScmObj** %stackaddr$makeclosure46958, align 8
%args46906$anf_45bind40228$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim46960 = alloca %struct.ScmObj*, align 8
%args46906$anf_45bind40228$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40232, %struct.ScmObj* %args46906$anf_45bind40228$0)
store volatile %struct.ScmObj* %args46906$anf_45bind40228$1, %struct.ScmObj** %stackaddr$prim46960, align 8
%stackaddr$prim46961 = alloca %struct.ScmObj*, align 8
%args46906$anf_45bind40228$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40652, %struct.ScmObj* %args46906$anf_45bind40228$1)
store volatile %struct.ScmObj* %args46906$anf_45bind40228$2, %struct.ScmObj** %stackaddr$prim46961, align 8
%clofunc46962 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40228)
musttail call tailcc void %clofunc46962(%struct.ScmObj* %anf_45bind40228, %struct.ScmObj* %args46906$anf_45bind40228$2)
ret void
}

define tailcc void @proc_clo$ae40652(%struct.ScmObj* %env$ae40652,%struct.ScmObj* %current_45args46379) {
%stackaddr$prim46963 = alloca %struct.ScmObj*, align 8
%_95k40353 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46379)
store volatile %struct.ScmObj* %_95k40353, %struct.ScmObj** %stackaddr$prim46963, align 8
%stackaddr$prim46964 = alloca %struct.ScmObj*, align 8
%current_45args46380 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46379)
store volatile %struct.ScmObj* %current_45args46380, %struct.ScmObj** %stackaddr$prim46964, align 8
%stackaddr$prim46965 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46380)
store volatile %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$prim46965, align 8
%stackaddr$makeclosure46966 = alloca %struct.ScmObj*, align 8
%fptrToInt46967 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40654 to i64
%ae40654 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt46967)
store volatile %struct.ScmObj* %ae40654, %struct.ScmObj** %stackaddr$makeclosure46966, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40654, %struct.ScmObj* %Ycmb40103, i64 0)
%ae40655 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure46968 = alloca %struct.ScmObj*, align 8
%fptrToInt46969 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40656 to i64
%ae40656 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt46969)
store volatile %struct.ScmObj* %ae40656, %struct.ScmObj** %stackaddr$makeclosure46968, align 8
%args46905$ae40654$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim46970 = alloca %struct.ScmObj*, align 8
%args46905$ae40654$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40656, %struct.ScmObj* %args46905$ae40654$0)
store volatile %struct.ScmObj* %args46905$ae40654$1, %struct.ScmObj** %stackaddr$prim46970, align 8
%stackaddr$prim46971 = alloca %struct.ScmObj*, align 8
%args46905$ae40654$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40655, %struct.ScmObj* %args46905$ae40654$1)
store volatile %struct.ScmObj* %args46905$ae40654$2, %struct.ScmObj** %stackaddr$prim46971, align 8
%clofunc46972 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40654)
musttail call tailcc void %clofunc46972(%struct.ScmObj* %ae40654, %struct.ScmObj* %args46905$ae40654$2)
ret void
}

define tailcc void @proc_clo$ae40654(%struct.ScmObj* %env$ae40654,%struct.ScmObj* %current_45args46382) {
%stackaddr$env-ref46973 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40654, i64 0)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref46973
%stackaddr$prim46974 = alloca %struct.ScmObj*, align 8
%_95k40354 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46382)
store volatile %struct.ScmObj* %_95k40354, %struct.ScmObj** %stackaddr$prim46974, align 8
%stackaddr$prim46975 = alloca %struct.ScmObj*, align 8
%current_45args46383 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46382)
store volatile %struct.ScmObj* %current_45args46383, %struct.ScmObj** %stackaddr$prim46975, align 8
%stackaddr$prim46976 = alloca %struct.ScmObj*, align 8
%anf_45bind40237 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46383)
store volatile %struct.ScmObj* %anf_45bind40237, %struct.ScmObj** %stackaddr$prim46976, align 8
%stackaddr$makeclosure46977 = alloca %struct.ScmObj*, align 8
%fptrToInt46978 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40732 to i64
%ae40732 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt46978)
store volatile %struct.ScmObj* %ae40732, %struct.ScmObj** %stackaddr$makeclosure46977, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40732, %struct.ScmObj* %Ycmb40103, i64 0)
%args46889$Ycmb40103$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim46979 = alloca %struct.ScmObj*, align 8
%args46889$Ycmb40103$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40237, %struct.ScmObj* %args46889$Ycmb40103$0)
store volatile %struct.ScmObj* %args46889$Ycmb40103$1, %struct.ScmObj** %stackaddr$prim46979, align 8
%stackaddr$prim46980 = alloca %struct.ScmObj*, align 8
%args46889$Ycmb40103$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40732, %struct.ScmObj* %args46889$Ycmb40103$1)
store volatile %struct.ScmObj* %args46889$Ycmb40103$2, %struct.ScmObj** %stackaddr$prim46980, align 8
%clofunc46981 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40103)
musttail call tailcc void %clofunc46981(%struct.ScmObj* %Ycmb40103, %struct.ScmObj* %args46889$Ycmb40103$2)
ret void
}

define tailcc void @proc_clo$ae40732(%struct.ScmObj* %env$ae40732,%struct.ScmObj* %current_45args46385) {
%stackaddr$env-ref46982 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40732, i64 0)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref46982
%stackaddr$prim46983 = alloca %struct.ScmObj*, align 8
%_95k40355 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46385)
store volatile %struct.ScmObj* %_95k40355, %struct.ScmObj** %stackaddr$prim46983, align 8
%stackaddr$prim46984 = alloca %struct.ScmObj*, align 8
%current_45args46386 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46385)
store volatile %struct.ScmObj* %current_45args46386, %struct.ScmObj** %stackaddr$prim46984, align 8
%stackaddr$prim46985 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46386)
store volatile %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$prim46985, align 8
%stackaddr$makeclosure46986 = alloca %struct.ScmObj*, align 8
%fptrToInt46987 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40734 to i64
%ae40734 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt46987)
store volatile %struct.ScmObj* %ae40734, %struct.ScmObj** %stackaddr$makeclosure46986, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40734, %struct.ScmObj* %Ycmb40103, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40734, %struct.ScmObj* %_37foldr140124, i64 1)
%ae40735 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure46988 = alloca %struct.ScmObj*, align 8
%fptrToInt46989 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40736 to i64
%ae40736 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt46989)
store volatile %struct.ScmObj* %ae40736, %struct.ScmObj** %stackaddr$makeclosure46988, align 8
%args46888$ae40734$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim46990 = alloca %struct.ScmObj*, align 8
%args46888$ae40734$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40736, %struct.ScmObj* %args46888$ae40734$0)
store volatile %struct.ScmObj* %args46888$ae40734$1, %struct.ScmObj** %stackaddr$prim46990, align 8
%stackaddr$prim46991 = alloca %struct.ScmObj*, align 8
%args46888$ae40734$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40735, %struct.ScmObj* %args46888$ae40734$1)
store volatile %struct.ScmObj* %args46888$ae40734$2, %struct.ScmObj** %stackaddr$prim46991, align 8
%clofunc46992 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40734)
musttail call tailcc void %clofunc46992(%struct.ScmObj* %ae40734, %struct.ScmObj* %args46888$ae40734$2)
ret void
}

define tailcc void @proc_clo$ae40734(%struct.ScmObj* %env$ae40734,%struct.ScmObj* %current_45args46388) {
%stackaddr$env-ref46993 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40734, i64 0)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref46993
%stackaddr$env-ref46994 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40734, i64 1)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref46994
%stackaddr$prim46995 = alloca %struct.ScmObj*, align 8
%_95k40356 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46388)
store volatile %struct.ScmObj* %_95k40356, %struct.ScmObj** %stackaddr$prim46995, align 8
%stackaddr$prim46996 = alloca %struct.ScmObj*, align 8
%current_45args46389 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46388)
store volatile %struct.ScmObj* %current_45args46389, %struct.ScmObj** %stackaddr$prim46996, align 8
%stackaddr$prim46997 = alloca %struct.ScmObj*, align 8
%anf_45bind40243 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46389)
store volatile %struct.ScmObj* %anf_45bind40243, %struct.ScmObj** %stackaddr$prim46997, align 8
%stackaddr$makeclosure46998 = alloca %struct.ScmObj*, align 8
%fptrToInt46999 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40829 to i64
%ae40829 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt46999)
store volatile %struct.ScmObj* %ae40829, %struct.ScmObj** %stackaddr$makeclosure46998, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40829, %struct.ScmObj* %Ycmb40103, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40829, %struct.ScmObj* %_37foldr140124, i64 1)
%args46869$Ycmb40103$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47000 = alloca %struct.ScmObj*, align 8
%args46869$Ycmb40103$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40243, %struct.ScmObj* %args46869$Ycmb40103$0)
store volatile %struct.ScmObj* %args46869$Ycmb40103$1, %struct.ScmObj** %stackaddr$prim47000, align 8
%stackaddr$prim47001 = alloca %struct.ScmObj*, align 8
%args46869$Ycmb40103$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40829, %struct.ScmObj* %args46869$Ycmb40103$1)
store volatile %struct.ScmObj* %args46869$Ycmb40103$2, %struct.ScmObj** %stackaddr$prim47001, align 8
%clofunc47002 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40103)
musttail call tailcc void %clofunc47002(%struct.ScmObj* %Ycmb40103, %struct.ScmObj* %args46869$Ycmb40103$2)
ret void
}

define tailcc void @proc_clo$ae40829(%struct.ScmObj* %env$ae40829,%struct.ScmObj* %current_45args46391) {
%stackaddr$env-ref47003 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40829, i64 0)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref47003
%stackaddr$env-ref47004 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40829, i64 1)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref47004
%stackaddr$prim47005 = alloca %struct.ScmObj*, align 8
%_95k40357 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46391)
store volatile %struct.ScmObj* %_95k40357, %struct.ScmObj** %stackaddr$prim47005, align 8
%stackaddr$prim47006 = alloca %struct.ScmObj*, align 8
%current_45args46392 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46391)
store volatile %struct.ScmObj* %current_45args46392, %struct.ScmObj** %stackaddr$prim47006, align 8
%stackaddr$prim47007 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46392)
store volatile %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$prim47007, align 8
%stackaddr$makeclosure47008 = alloca %struct.ScmObj*, align 8
%fptrToInt47009 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40831 to i64
%ae40831 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47009)
store volatile %struct.ScmObj* %ae40831, %struct.ScmObj** %stackaddr$makeclosure47008, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40831, %struct.ScmObj* %_37map140120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40831, %struct.ScmObj* %Ycmb40103, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40831, %struct.ScmObj* %_37foldr140124, i64 2)
%ae40832 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47010 = alloca %struct.ScmObj*, align 8
%fptrToInt47011 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40833 to i64
%ae40833 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47011)
store volatile %struct.ScmObj* %ae40833, %struct.ScmObj** %stackaddr$makeclosure47010, align 8
%args46868$ae40831$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47012 = alloca %struct.ScmObj*, align 8
%args46868$ae40831$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40833, %struct.ScmObj* %args46868$ae40831$0)
store volatile %struct.ScmObj* %args46868$ae40831$1, %struct.ScmObj** %stackaddr$prim47012, align 8
%stackaddr$prim47013 = alloca %struct.ScmObj*, align 8
%args46868$ae40831$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40832, %struct.ScmObj* %args46868$ae40831$1)
store volatile %struct.ScmObj* %args46868$ae40831$2, %struct.ScmObj** %stackaddr$prim47013, align 8
%clofunc47014 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40831)
musttail call tailcc void %clofunc47014(%struct.ScmObj* %ae40831, %struct.ScmObj* %args46868$ae40831$2)
ret void
}

define tailcc void @proc_clo$ae40831(%struct.ScmObj* %env$ae40831,%struct.ScmObj* %current_45args46394) {
%stackaddr$env-ref47015 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40831, i64 0)
store %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$env-ref47015
%stackaddr$env-ref47016 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40831, i64 1)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref47016
%stackaddr$env-ref47017 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40831, i64 2)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref47017
%stackaddr$prim47018 = alloca %struct.ScmObj*, align 8
%_95k40358 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46394)
store volatile %struct.ScmObj* %_95k40358, %struct.ScmObj** %stackaddr$prim47018, align 8
%stackaddr$prim47019 = alloca %struct.ScmObj*, align 8
%current_45args46395 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46394)
store volatile %struct.ScmObj* %current_45args46395, %struct.ScmObj** %stackaddr$prim47019, align 8
%stackaddr$prim47020 = alloca %struct.ScmObj*, align 8
%anf_45bind40250 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46395)
store volatile %struct.ScmObj* %anf_45bind40250, %struct.ScmObj** %stackaddr$prim47020, align 8
%stackaddr$makeclosure47021 = alloca %struct.ScmObj*, align 8
%fptrToInt47022 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40979 to i64
%ae40979 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47022)
store volatile %struct.ScmObj* %ae40979, %struct.ScmObj** %stackaddr$makeclosure47021, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40979, %struct.ScmObj* %_37map140120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40979, %struct.ScmObj* %Ycmb40103, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40979, %struct.ScmObj* %_37foldr140124, i64 2)
%args46852$Ycmb40103$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47023 = alloca %struct.ScmObj*, align 8
%args46852$Ycmb40103$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40250, %struct.ScmObj* %args46852$Ycmb40103$0)
store volatile %struct.ScmObj* %args46852$Ycmb40103$1, %struct.ScmObj** %stackaddr$prim47023, align 8
%stackaddr$prim47024 = alloca %struct.ScmObj*, align 8
%args46852$Ycmb40103$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40979, %struct.ScmObj* %args46852$Ycmb40103$1)
store volatile %struct.ScmObj* %args46852$Ycmb40103$2, %struct.ScmObj** %stackaddr$prim47024, align 8
%clofunc47025 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40103)
musttail call tailcc void %clofunc47025(%struct.ScmObj* %Ycmb40103, %struct.ScmObj* %args46852$Ycmb40103$2)
ret void
}

define tailcc void @proc_clo$ae40979(%struct.ScmObj* %env$ae40979,%struct.ScmObj* %current_45args46397) {
%stackaddr$env-ref47026 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40979, i64 0)
store %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$env-ref47026
%stackaddr$env-ref47027 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40979, i64 1)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref47027
%stackaddr$env-ref47028 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40979, i64 2)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref47028
%stackaddr$prim47029 = alloca %struct.ScmObj*, align 8
%_95k40359 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46397)
store volatile %struct.ScmObj* %_95k40359, %struct.ScmObj** %stackaddr$prim47029, align 8
%stackaddr$prim47030 = alloca %struct.ScmObj*, align 8
%current_45args46398 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46397)
store volatile %struct.ScmObj* %current_45args46398, %struct.ScmObj** %stackaddr$prim47030, align 8
%stackaddr$prim47031 = alloca %struct.ScmObj*, align 8
%_37take40116 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46398)
store volatile %struct.ScmObj* %_37take40116, %struct.ScmObj** %stackaddr$prim47031, align 8
%stackaddr$makeclosure47032 = alloca %struct.ScmObj*, align 8
%fptrToInt47033 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40981 to i64
%ae40981 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt47033)
store volatile %struct.ScmObj* %ae40981, %struct.ScmObj** %stackaddr$makeclosure47032, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40981, %struct.ScmObj* %_37map140120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40981, %struct.ScmObj* %Ycmb40103, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40981, %struct.ScmObj* %_37take40116, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae40981, %struct.ScmObj* %_37foldr140124, i64 3)
%ae40982 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47034 = alloca %struct.ScmObj*, align 8
%fptrToInt47035 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40983 to i64
%ae40983 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47035)
store volatile %struct.ScmObj* %ae40983, %struct.ScmObj** %stackaddr$makeclosure47034, align 8
%args46851$ae40981$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47036 = alloca %struct.ScmObj*, align 8
%args46851$ae40981$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40983, %struct.ScmObj* %args46851$ae40981$0)
store volatile %struct.ScmObj* %args46851$ae40981$1, %struct.ScmObj** %stackaddr$prim47036, align 8
%stackaddr$prim47037 = alloca %struct.ScmObj*, align 8
%args46851$ae40981$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40982, %struct.ScmObj* %args46851$ae40981$1)
store volatile %struct.ScmObj* %args46851$ae40981$2, %struct.ScmObj** %stackaddr$prim47037, align 8
%clofunc47038 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40981)
musttail call tailcc void %clofunc47038(%struct.ScmObj* %ae40981, %struct.ScmObj* %args46851$ae40981$2)
ret void
}

define tailcc void @proc_clo$ae40981(%struct.ScmObj* %env$ae40981,%struct.ScmObj* %current_45args46400) {
%stackaddr$env-ref47039 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40981, i64 0)
store %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$env-ref47039
%stackaddr$env-ref47040 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40981, i64 1)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref47040
%stackaddr$env-ref47041 = alloca %struct.ScmObj*, align 8
%_37take40116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40981, i64 2)
store %struct.ScmObj* %_37take40116, %struct.ScmObj** %stackaddr$env-ref47041
%stackaddr$env-ref47042 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40981, i64 3)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref47042
%stackaddr$prim47043 = alloca %struct.ScmObj*, align 8
%_95k40360 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46400)
store volatile %struct.ScmObj* %_95k40360, %struct.ScmObj** %stackaddr$prim47043, align 8
%stackaddr$prim47044 = alloca %struct.ScmObj*, align 8
%current_45args46401 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46400)
store volatile %struct.ScmObj* %current_45args46401, %struct.ScmObj** %stackaddr$prim47044, align 8
%stackaddr$prim47045 = alloca %struct.ScmObj*, align 8
%anf_45bind40254 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46401)
store volatile %struct.ScmObj* %anf_45bind40254, %struct.ScmObj** %stackaddr$prim47045, align 8
%stackaddr$makeclosure47046 = alloca %struct.ScmObj*, align 8
%fptrToInt47047 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41062 to i64
%ae41062 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt47047)
store volatile %struct.ScmObj* %ae41062, %struct.ScmObj** %stackaddr$makeclosure47046, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41062, %struct.ScmObj* %_37map140120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41062, %struct.ScmObj* %Ycmb40103, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41062, %struct.ScmObj* %_37take40116, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41062, %struct.ScmObj* %_37foldr140124, i64 3)
%args46837$Ycmb40103$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47048 = alloca %struct.ScmObj*, align 8
%args46837$Ycmb40103$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40254, %struct.ScmObj* %args46837$Ycmb40103$0)
store volatile %struct.ScmObj* %args46837$Ycmb40103$1, %struct.ScmObj** %stackaddr$prim47048, align 8
%stackaddr$prim47049 = alloca %struct.ScmObj*, align 8
%args46837$Ycmb40103$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41062, %struct.ScmObj* %args46837$Ycmb40103$1)
store volatile %struct.ScmObj* %args46837$Ycmb40103$2, %struct.ScmObj** %stackaddr$prim47049, align 8
%clofunc47050 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40103)
musttail call tailcc void %clofunc47050(%struct.ScmObj* %Ycmb40103, %struct.ScmObj* %args46837$Ycmb40103$2)
ret void
}

define tailcc void @proc_clo$ae41062(%struct.ScmObj* %env$ae41062,%struct.ScmObj* %current_45args46403) {
%stackaddr$env-ref47051 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41062, i64 0)
store %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$env-ref47051
%stackaddr$env-ref47052 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41062, i64 1)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref47052
%stackaddr$env-ref47053 = alloca %struct.ScmObj*, align 8
%_37take40116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41062, i64 2)
store %struct.ScmObj* %_37take40116, %struct.ScmObj** %stackaddr$env-ref47053
%stackaddr$env-ref47054 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41062, i64 3)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref47054
%stackaddr$prim47055 = alloca %struct.ScmObj*, align 8
%_95k40361 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46403)
store volatile %struct.ScmObj* %_95k40361, %struct.ScmObj** %stackaddr$prim47055, align 8
%stackaddr$prim47056 = alloca %struct.ScmObj*, align 8
%current_45args46404 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46403)
store volatile %struct.ScmObj* %current_45args46404, %struct.ScmObj** %stackaddr$prim47056, align 8
%stackaddr$prim47057 = alloca %struct.ScmObj*, align 8
%_37length40113 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46404)
store volatile %struct.ScmObj* %_37length40113, %struct.ScmObj** %stackaddr$prim47057, align 8
%stackaddr$makeclosure47058 = alloca %struct.ScmObj*, align 8
%fptrToInt47059 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41064 to i64
%ae41064 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt47059)
store volatile %struct.ScmObj* %ae41064, %struct.ScmObj** %stackaddr$makeclosure47058, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41064, %struct.ScmObj* %_37map140120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41064, %struct.ScmObj* %Ycmb40103, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41064, %struct.ScmObj* %_37take40116, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41064, %struct.ScmObj* %_37length40113, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41064, %struct.ScmObj* %_37foldr140124, i64 4)
%ae41065 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47060 = alloca %struct.ScmObj*, align 8
%fptrToInt47061 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41066 to i64
%ae41066 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47061)
store volatile %struct.ScmObj* %ae41066, %struct.ScmObj** %stackaddr$makeclosure47060, align 8
%args46836$ae41064$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47062 = alloca %struct.ScmObj*, align 8
%args46836$ae41064$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41066, %struct.ScmObj* %args46836$ae41064$0)
store volatile %struct.ScmObj* %args46836$ae41064$1, %struct.ScmObj** %stackaddr$prim47062, align 8
%stackaddr$prim47063 = alloca %struct.ScmObj*, align 8
%args46836$ae41064$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41065, %struct.ScmObj* %args46836$ae41064$1)
store volatile %struct.ScmObj* %args46836$ae41064$2, %struct.ScmObj** %stackaddr$prim47063, align 8
%clofunc47064 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41064)
musttail call tailcc void %clofunc47064(%struct.ScmObj* %ae41064, %struct.ScmObj* %args46836$ae41064$2)
ret void
}

define tailcc void @proc_clo$ae41064(%struct.ScmObj* %env$ae41064,%struct.ScmObj* %current_45args46406) {
%stackaddr$env-ref47065 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41064, i64 0)
store %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$env-ref47065
%stackaddr$env-ref47066 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41064, i64 1)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref47066
%stackaddr$env-ref47067 = alloca %struct.ScmObj*, align 8
%_37take40116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41064, i64 2)
store %struct.ScmObj* %_37take40116, %struct.ScmObj** %stackaddr$env-ref47067
%stackaddr$env-ref47068 = alloca %struct.ScmObj*, align 8
%_37length40113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41064, i64 3)
store %struct.ScmObj* %_37length40113, %struct.ScmObj** %stackaddr$env-ref47068
%stackaddr$env-ref47069 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41064, i64 4)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref47069
%stackaddr$prim47070 = alloca %struct.ScmObj*, align 8
%_95k40362 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46406)
store volatile %struct.ScmObj* %_95k40362, %struct.ScmObj** %stackaddr$prim47070, align 8
%stackaddr$prim47071 = alloca %struct.ScmObj*, align 8
%current_45args46407 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46406)
store volatile %struct.ScmObj* %current_45args46407, %struct.ScmObj** %stackaddr$prim47071, align 8
%stackaddr$prim47072 = alloca %struct.ScmObj*, align 8
%anf_45bind40259 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46407)
store volatile %struct.ScmObj* %anf_45bind40259, %struct.ScmObj** %stackaddr$prim47072, align 8
%stackaddr$makeclosure47073 = alloca %struct.ScmObj*, align 8
%fptrToInt47074 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41141 to i64
%ae41141 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt47074)
store volatile %struct.ScmObj* %ae41141, %struct.ScmObj** %stackaddr$makeclosure47073, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41141, %struct.ScmObj* %_37map140120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41141, %struct.ScmObj* %Ycmb40103, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41141, %struct.ScmObj* %_37take40116, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41141, %struct.ScmObj* %_37length40113, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41141, %struct.ScmObj* %_37foldr140124, i64 4)
%args46820$Ycmb40103$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47075 = alloca %struct.ScmObj*, align 8
%args46820$Ycmb40103$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40259, %struct.ScmObj* %args46820$Ycmb40103$0)
store volatile %struct.ScmObj* %args46820$Ycmb40103$1, %struct.ScmObj** %stackaddr$prim47075, align 8
%stackaddr$prim47076 = alloca %struct.ScmObj*, align 8
%args46820$Ycmb40103$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41141, %struct.ScmObj* %args46820$Ycmb40103$1)
store volatile %struct.ScmObj* %args46820$Ycmb40103$2, %struct.ScmObj** %stackaddr$prim47076, align 8
%clofunc47077 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40103)
musttail call tailcc void %clofunc47077(%struct.ScmObj* %Ycmb40103, %struct.ScmObj* %args46820$Ycmb40103$2)
ret void
}

define tailcc void @proc_clo$ae41141(%struct.ScmObj* %env$ae41141,%struct.ScmObj* %current_45args46409) {
%stackaddr$env-ref47078 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41141, i64 0)
store %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$env-ref47078
%stackaddr$env-ref47079 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41141, i64 1)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref47079
%stackaddr$env-ref47080 = alloca %struct.ScmObj*, align 8
%_37take40116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41141, i64 2)
store %struct.ScmObj* %_37take40116, %struct.ScmObj** %stackaddr$env-ref47080
%stackaddr$env-ref47081 = alloca %struct.ScmObj*, align 8
%_37length40113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41141, i64 3)
store %struct.ScmObj* %_37length40113, %struct.ScmObj** %stackaddr$env-ref47081
%stackaddr$env-ref47082 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41141, i64 4)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref47082
%stackaddr$prim47083 = alloca %struct.ScmObj*, align 8
%_95k40363 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46409)
store volatile %struct.ScmObj* %_95k40363, %struct.ScmObj** %stackaddr$prim47083, align 8
%stackaddr$prim47084 = alloca %struct.ScmObj*, align 8
%current_45args46410 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46409)
store volatile %struct.ScmObj* %current_45args46410, %struct.ScmObj** %stackaddr$prim47084, align 8
%stackaddr$prim47085 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46410)
store volatile %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$prim47085, align 8
%stackaddr$makeclosure47086 = alloca %struct.ScmObj*, align 8
%fptrToInt47087 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41143 to i64
%ae41143 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt47087)
store volatile %struct.ScmObj* %ae41143, %struct.ScmObj** %stackaddr$makeclosure47086, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41143, %struct.ScmObj* %_37foldr140124, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41143, %struct.ScmObj* %_37foldl140108, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41143, %struct.ScmObj* %_37map140120, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41143, %struct.ScmObj* %Ycmb40103, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41143, %struct.ScmObj* %_37take40116, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41143, %struct.ScmObj* %_37length40113, i64 5)
%ae41144 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47088 = alloca %struct.ScmObj*, align 8
%fptrToInt47089 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41145 to i64
%ae41145 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47089)
store volatile %struct.ScmObj* %ae41145, %struct.ScmObj** %stackaddr$makeclosure47088, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41145, %struct.ScmObj* %_37foldl140108, i64 0)
%args46819$ae41143$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47090 = alloca %struct.ScmObj*, align 8
%args46819$ae41143$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41145, %struct.ScmObj* %args46819$ae41143$0)
store volatile %struct.ScmObj* %args46819$ae41143$1, %struct.ScmObj** %stackaddr$prim47090, align 8
%stackaddr$prim47091 = alloca %struct.ScmObj*, align 8
%args46819$ae41143$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41144, %struct.ScmObj* %args46819$ae41143$1)
store volatile %struct.ScmObj* %args46819$ae41143$2, %struct.ScmObj** %stackaddr$prim47091, align 8
%clofunc47092 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41143)
musttail call tailcc void %clofunc47092(%struct.ScmObj* %ae41143, %struct.ScmObj* %args46819$ae41143$2)
ret void
}

define tailcc void @proc_clo$ae41143(%struct.ScmObj* %env$ae41143,%struct.ScmObj* %current_45args46412) {
%stackaddr$env-ref47093 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41143, i64 0)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref47093
%stackaddr$env-ref47094 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41143, i64 1)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref47094
%stackaddr$env-ref47095 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41143, i64 2)
store %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$env-ref47095
%stackaddr$env-ref47096 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41143, i64 3)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref47096
%stackaddr$env-ref47097 = alloca %struct.ScmObj*, align 8
%_37take40116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41143, i64 4)
store %struct.ScmObj* %_37take40116, %struct.ScmObj** %stackaddr$env-ref47097
%stackaddr$env-ref47098 = alloca %struct.ScmObj*, align 8
%_37length40113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41143, i64 5)
store %struct.ScmObj* %_37length40113, %struct.ScmObj** %stackaddr$env-ref47098
%stackaddr$prim47099 = alloca %struct.ScmObj*, align 8
%_95k40364 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46412)
store volatile %struct.ScmObj* %_95k40364, %struct.ScmObj** %stackaddr$prim47099, align 8
%stackaddr$prim47100 = alloca %struct.ScmObj*, align 8
%current_45args46413 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46412)
store volatile %struct.ScmObj* %current_45args46413, %struct.ScmObj** %stackaddr$prim47100, align 8
%stackaddr$prim47101 = alloca %struct.ScmObj*, align 8
%_37last40146 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46413)
store volatile %struct.ScmObj* %_37last40146, %struct.ScmObj** %stackaddr$prim47101, align 8
%stackaddr$makeclosure47102 = alloca %struct.ScmObj*, align 8
%fptrToInt47103 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41197 to i64
%ae41197 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt47103)
store volatile %struct.ScmObj* %ae41197, %struct.ScmObj** %stackaddr$makeclosure47102, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41197, %struct.ScmObj* %_37foldr140124, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41197, %struct.ScmObj* %_37foldl140108, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41197, %struct.ScmObj* %_37map140120, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41197, %struct.ScmObj* %Ycmb40103, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41197, %struct.ScmObj* %_37last40146, i64 4)
%ae41198 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47104 = alloca %struct.ScmObj*, align 8
%fptrToInt47105 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41199 to i64
%ae41199 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47105)
store volatile %struct.ScmObj* %ae41199, %struct.ScmObj** %stackaddr$makeclosure47104, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41199, %struct.ScmObj* %_37take40116, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41199, %struct.ScmObj* %_37length40113, i64 1)
%args46805$ae41197$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47106 = alloca %struct.ScmObj*, align 8
%args46805$ae41197$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41199, %struct.ScmObj* %args46805$ae41197$0)
store volatile %struct.ScmObj* %args46805$ae41197$1, %struct.ScmObj** %stackaddr$prim47106, align 8
%stackaddr$prim47107 = alloca %struct.ScmObj*, align 8
%args46805$ae41197$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41198, %struct.ScmObj* %args46805$ae41197$1)
store volatile %struct.ScmObj* %args46805$ae41197$2, %struct.ScmObj** %stackaddr$prim47107, align 8
%clofunc47108 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41197)
musttail call tailcc void %clofunc47108(%struct.ScmObj* %ae41197, %struct.ScmObj* %args46805$ae41197$2)
ret void
}

define tailcc void @proc_clo$ae41197(%struct.ScmObj* %env$ae41197,%struct.ScmObj* %current_45args46415) {
%stackaddr$env-ref47109 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41197, i64 0)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref47109
%stackaddr$env-ref47110 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41197, i64 1)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref47110
%stackaddr$env-ref47111 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41197, i64 2)
store %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$env-ref47111
%stackaddr$env-ref47112 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41197, i64 3)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref47112
%stackaddr$env-ref47113 = alloca %struct.ScmObj*, align 8
%_37last40146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41197, i64 4)
store %struct.ScmObj* %_37last40146, %struct.ScmObj** %stackaddr$env-ref47113
%stackaddr$prim47114 = alloca %struct.ScmObj*, align 8
%_95k40365 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46415)
store volatile %struct.ScmObj* %_95k40365, %struct.ScmObj** %stackaddr$prim47114, align 8
%stackaddr$prim47115 = alloca %struct.ScmObj*, align 8
%current_45args46416 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46415)
store volatile %struct.ScmObj* %current_45args46416, %struct.ScmObj** %stackaddr$prim47115, align 8
%stackaddr$prim47116 = alloca %struct.ScmObj*, align 8
%_37drop_45right40143 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46416)
store volatile %struct.ScmObj* %_37drop_45right40143, %struct.ScmObj** %stackaddr$prim47116, align 8
%stackaddr$makeclosure47117 = alloca %struct.ScmObj*, align 8
%fptrToInt47118 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41227 to i64
%ae41227 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt47118)
store volatile %struct.ScmObj* %ae41227, %struct.ScmObj** %stackaddr$makeclosure47117, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41227, %struct.ScmObj* %_37foldr140124, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41227, %struct.ScmObj* %_37foldl140108, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41227, %struct.ScmObj* %Ycmb40103, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41227, %struct.ScmObj* %_37last40146, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41227, %struct.ScmObj* %_37drop_45right40143, i64 4)
%ae41228 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47119 = alloca %struct.ScmObj*, align 8
%fptrToInt47120 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41229 to i64
%ae41229 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47120)
store volatile %struct.ScmObj* %ae41229, %struct.ScmObj** %stackaddr$makeclosure47119, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41229, %struct.ScmObj* %_37map140120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41229, %struct.ScmObj* %_37foldr140124, i64 1)
%args46795$ae41227$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47121 = alloca %struct.ScmObj*, align 8
%args46795$ae41227$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41229, %struct.ScmObj* %args46795$ae41227$0)
store volatile %struct.ScmObj* %args46795$ae41227$1, %struct.ScmObj** %stackaddr$prim47121, align 8
%stackaddr$prim47122 = alloca %struct.ScmObj*, align 8
%args46795$ae41227$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41228, %struct.ScmObj* %args46795$ae41227$1)
store volatile %struct.ScmObj* %args46795$ae41227$2, %struct.ScmObj** %stackaddr$prim47122, align 8
%clofunc47123 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41227)
musttail call tailcc void %clofunc47123(%struct.ScmObj* %ae41227, %struct.ScmObj* %args46795$ae41227$2)
ret void
}

define tailcc void @proc_clo$ae41227(%struct.ScmObj* %env$ae41227,%struct.ScmObj* %current_45args46418) {
%stackaddr$env-ref47124 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41227, i64 0)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref47124
%stackaddr$env-ref47125 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41227, i64 1)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref47125
%stackaddr$env-ref47126 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41227, i64 2)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref47126
%stackaddr$env-ref47127 = alloca %struct.ScmObj*, align 8
%_37last40146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41227, i64 3)
store %struct.ScmObj* %_37last40146, %struct.ScmObj** %stackaddr$env-ref47127
%stackaddr$env-ref47128 = alloca %struct.ScmObj*, align 8
%_37drop_45right40143 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41227, i64 4)
store %struct.ScmObj* %_37drop_45right40143, %struct.ScmObj** %stackaddr$env-ref47128
%stackaddr$prim47129 = alloca %struct.ScmObj*, align 8
%_95k40366 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46418)
store volatile %struct.ScmObj* %_95k40366, %struct.ScmObj** %stackaddr$prim47129, align 8
%stackaddr$prim47130 = alloca %struct.ScmObj*, align 8
%current_45args46419 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46418)
store volatile %struct.ScmObj* %current_45args46419, %struct.ScmObj** %stackaddr$prim47130, align 8
%stackaddr$prim47131 = alloca %struct.ScmObj*, align 8
%anf_45bind40275 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46419)
store volatile %struct.ScmObj* %anf_45bind40275, %struct.ScmObj** %stackaddr$prim47131, align 8
%stackaddr$makeclosure47132 = alloca %struct.ScmObj*, align 8
%fptrToInt47133 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41611 to i64
%ae41611 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt47133)
store volatile %struct.ScmObj* %ae41611, %struct.ScmObj** %stackaddr$makeclosure47132, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41611, %struct.ScmObj* %_37foldr140124, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41611, %struct.ScmObj* %_37foldl140108, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41611, %struct.ScmObj* %Ycmb40103, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41611, %struct.ScmObj* %_37last40146, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41611, %struct.ScmObj* %_37drop_45right40143, i64 4)
%args46735$Ycmb40103$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47134 = alloca %struct.ScmObj*, align 8
%args46735$Ycmb40103$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40275, %struct.ScmObj* %args46735$Ycmb40103$0)
store volatile %struct.ScmObj* %args46735$Ycmb40103$1, %struct.ScmObj** %stackaddr$prim47134, align 8
%stackaddr$prim47135 = alloca %struct.ScmObj*, align 8
%args46735$Ycmb40103$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41611, %struct.ScmObj* %args46735$Ycmb40103$1)
store volatile %struct.ScmObj* %args46735$Ycmb40103$2, %struct.ScmObj** %stackaddr$prim47135, align 8
%clofunc47136 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40103)
musttail call tailcc void %clofunc47136(%struct.ScmObj* %Ycmb40103, %struct.ScmObj* %args46735$Ycmb40103$2)
ret void
}

define tailcc void @proc_clo$ae41611(%struct.ScmObj* %env$ae41611,%struct.ScmObj* %current_45args46421) {
%stackaddr$env-ref47137 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41611, i64 0)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref47137
%stackaddr$env-ref47138 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41611, i64 1)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref47138
%stackaddr$env-ref47139 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41611, i64 2)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref47139
%stackaddr$env-ref47140 = alloca %struct.ScmObj*, align 8
%_37last40146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41611, i64 3)
store %struct.ScmObj* %_37last40146, %struct.ScmObj** %stackaddr$env-ref47140
%stackaddr$env-ref47141 = alloca %struct.ScmObj*, align 8
%_37drop_45right40143 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41611, i64 4)
store %struct.ScmObj* %_37drop_45right40143, %struct.ScmObj** %stackaddr$env-ref47141
%stackaddr$prim47142 = alloca %struct.ScmObj*, align 8
%_95k40367 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46421)
store volatile %struct.ScmObj* %_95k40367, %struct.ScmObj** %stackaddr$prim47142, align 8
%stackaddr$prim47143 = alloca %struct.ScmObj*, align 8
%current_45args46422 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46421)
store volatile %struct.ScmObj* %current_45args46422, %struct.ScmObj** %stackaddr$prim47143, align 8
%stackaddr$prim47144 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46422)
store volatile %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$prim47144, align 8
%stackaddr$makeclosure47145 = alloca %struct.ScmObj*, align 8
%fptrToInt47146 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41613 to i64
%ae41613 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt47146)
store volatile %struct.ScmObj* %ae41613, %struct.ScmObj** %stackaddr$makeclosure47145, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41613, %struct.ScmObj* %_37foldr140124, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41613, %struct.ScmObj* %_37foldl140108, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41613, %struct.ScmObj* %Ycmb40103, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41613, %struct.ScmObj* %_37last40146, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41613, %struct.ScmObj* %_37foldr40129, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41613, %struct.ScmObj* %_37drop_45right40143, i64 5)
%ae41614 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47147 = alloca %struct.ScmObj*, align 8
%fptrToInt47148 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41615 to i64
%ae41615 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47148)
store volatile %struct.ScmObj* %ae41615, %struct.ScmObj** %stackaddr$makeclosure47147, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41615, %struct.ScmObj* %_37foldr140124, i64 0)
%args46734$ae41613$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47149 = alloca %struct.ScmObj*, align 8
%args46734$ae41613$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41615, %struct.ScmObj* %args46734$ae41613$0)
store volatile %struct.ScmObj* %args46734$ae41613$1, %struct.ScmObj** %stackaddr$prim47149, align 8
%stackaddr$prim47150 = alloca %struct.ScmObj*, align 8
%args46734$ae41613$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41614, %struct.ScmObj* %args46734$ae41613$1)
store volatile %struct.ScmObj* %args46734$ae41613$2, %struct.ScmObj** %stackaddr$prim47150, align 8
%clofunc47151 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41613)
musttail call tailcc void %clofunc47151(%struct.ScmObj* %ae41613, %struct.ScmObj* %args46734$ae41613$2)
ret void
}

define tailcc void @proc_clo$ae41613(%struct.ScmObj* %env$ae41613,%struct.ScmObj* %current_45args46424) {
%stackaddr$env-ref47152 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41613, i64 0)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref47152
%stackaddr$env-ref47153 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41613, i64 1)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref47153
%stackaddr$env-ref47154 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41613, i64 2)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref47154
%stackaddr$env-ref47155 = alloca %struct.ScmObj*, align 8
%_37last40146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41613, i64 3)
store %struct.ScmObj* %_37last40146, %struct.ScmObj** %stackaddr$env-ref47155
%stackaddr$env-ref47156 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41613, i64 4)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref47156
%stackaddr$env-ref47157 = alloca %struct.ScmObj*, align 8
%_37drop_45right40143 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41613, i64 5)
store %struct.ScmObj* %_37drop_45right40143, %struct.ScmObj** %stackaddr$env-ref47157
%stackaddr$prim47158 = alloca %struct.ScmObj*, align 8
%_95k40368 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46424)
store volatile %struct.ScmObj* %_95k40368, %struct.ScmObj** %stackaddr$prim47158, align 8
%stackaddr$prim47159 = alloca %struct.ScmObj*, align 8
%current_45args46425 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46424)
store volatile %struct.ScmObj* %current_45args46425, %struct.ScmObj** %stackaddr$prim47159, align 8
%stackaddr$prim47160 = alloca %struct.ScmObj*, align 8
%_37map140155 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46425)
store volatile %struct.ScmObj* %_37map140155, %struct.ScmObj** %stackaddr$prim47160, align 8
%stackaddr$makeclosure47161 = alloca %struct.ScmObj*, align 8
%fptrToInt47162 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41690 to i64
%ae41690 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt47162)
store volatile %struct.ScmObj* %ae41690, %struct.ScmObj** %stackaddr$makeclosure47161, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41690, %struct.ScmObj* %_37foldr140124, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41690, %struct.ScmObj* %_37foldl140108, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41690, %struct.ScmObj* %Ycmb40103, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41690, %struct.ScmObj* %_37foldr40129, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41690, %struct.ScmObj* %_37map140155, i64 4)
%ae41691 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47163 = alloca %struct.ScmObj*, align 8
%fptrToInt47164 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41692 to i64
%ae41692 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47164)
store volatile %struct.ScmObj* %ae41692, %struct.ScmObj** %stackaddr$makeclosure47163, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41692, %struct.ScmObj* %_37last40146, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41692, %struct.ScmObj* %_37foldr40129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41692, %struct.ScmObj* %_37drop_45right40143, i64 2)
%args46715$ae41690$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47165 = alloca %struct.ScmObj*, align 8
%args46715$ae41690$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41692, %struct.ScmObj* %args46715$ae41690$0)
store volatile %struct.ScmObj* %args46715$ae41690$1, %struct.ScmObj** %stackaddr$prim47165, align 8
%stackaddr$prim47166 = alloca %struct.ScmObj*, align 8
%args46715$ae41690$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41691, %struct.ScmObj* %args46715$ae41690$1)
store volatile %struct.ScmObj* %args46715$ae41690$2, %struct.ScmObj** %stackaddr$prim47166, align 8
%clofunc47167 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41690)
musttail call tailcc void %clofunc47167(%struct.ScmObj* %ae41690, %struct.ScmObj* %args46715$ae41690$2)
ret void
}

define tailcc void @proc_clo$ae41690(%struct.ScmObj* %env$ae41690,%struct.ScmObj* %current_45args46427) {
%stackaddr$env-ref47168 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41690, i64 0)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref47168
%stackaddr$env-ref47169 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41690, i64 1)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref47169
%stackaddr$env-ref47170 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41690, i64 2)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref47170
%stackaddr$env-ref47171 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41690, i64 3)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref47171
%stackaddr$env-ref47172 = alloca %struct.ScmObj*, align 8
%_37map140155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41690, i64 4)
store %struct.ScmObj* %_37map140155, %struct.ScmObj** %stackaddr$env-ref47172
%stackaddr$prim47173 = alloca %struct.ScmObj*, align 8
%_95k40369 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46427)
store volatile %struct.ScmObj* %_95k40369, %struct.ScmObj** %stackaddr$prim47173, align 8
%stackaddr$prim47174 = alloca %struct.ScmObj*, align 8
%current_45args46428 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46427)
store volatile %struct.ScmObj* %current_45args46428, %struct.ScmObj** %stackaddr$prim47174, align 8
%stackaddr$prim47175 = alloca %struct.ScmObj*, align 8
%_37map40150 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46428)
store volatile %struct.ScmObj* %_37map40150, %struct.ScmObj** %stackaddr$prim47175, align 8
%stackaddr$makeclosure47176 = alloca %struct.ScmObj*, align 8
%fptrToInt47177 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41836 to i64
%ae41836 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47177)
store volatile %struct.ScmObj* %ae41836, %struct.ScmObj** %stackaddr$makeclosure47176, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41836, %struct.ScmObj* %Ycmb40103, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41836, %struct.ScmObj* %_37foldl140108, i64 1)
%ae41837 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47178 = alloca %struct.ScmObj*, align 8
%fptrToInt47179 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41838 to i64
%ae41838 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47179)
store volatile %struct.ScmObj* %ae41838, %struct.ScmObj** %stackaddr$makeclosure47178, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41838, %struct.ScmObj* %_37foldr40129, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41838, %struct.ScmObj* %_37foldr140124, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41838, %struct.ScmObj* %_37map140155, i64 2)
%args46698$ae41836$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47180 = alloca %struct.ScmObj*, align 8
%args46698$ae41836$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41838, %struct.ScmObj* %args46698$ae41836$0)
store volatile %struct.ScmObj* %args46698$ae41836$1, %struct.ScmObj** %stackaddr$prim47180, align 8
%stackaddr$prim47181 = alloca %struct.ScmObj*, align 8
%args46698$ae41836$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41837, %struct.ScmObj* %args46698$ae41836$1)
store volatile %struct.ScmObj* %args46698$ae41836$2, %struct.ScmObj** %stackaddr$prim47181, align 8
%clofunc47182 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41836)
musttail call tailcc void %clofunc47182(%struct.ScmObj* %ae41836, %struct.ScmObj* %args46698$ae41836$2)
ret void
}

define tailcc void @proc_clo$ae41836(%struct.ScmObj* %env$ae41836,%struct.ScmObj* %current_45args46430) {
%stackaddr$env-ref47183 = alloca %struct.ScmObj*, align 8
%Ycmb40103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41836, i64 0)
store %struct.ScmObj* %Ycmb40103, %struct.ScmObj** %stackaddr$env-ref47183
%stackaddr$env-ref47184 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41836, i64 1)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref47184
%stackaddr$prim47185 = alloca %struct.ScmObj*, align 8
%_95k40370 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46430)
store volatile %struct.ScmObj* %_95k40370, %struct.ScmObj** %stackaddr$prim47185, align 8
%stackaddr$prim47186 = alloca %struct.ScmObj*, align 8
%current_45args46431 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46430)
store volatile %struct.ScmObj* %current_45args46431, %struct.ScmObj** %stackaddr$prim47186, align 8
%stackaddr$prim47187 = alloca %struct.ScmObj*, align 8
%anf_45bind40295 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46431)
store volatile %struct.ScmObj* %anf_45bind40295, %struct.ScmObj** %stackaddr$prim47187, align 8
%stackaddr$makeclosure47188 = alloca %struct.ScmObj*, align 8
%fptrToInt47189 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42228 to i64
%ae42228 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47189)
store volatile %struct.ScmObj* %ae42228, %struct.ScmObj** %stackaddr$makeclosure47188, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42228, %struct.ScmObj* %_37foldl140108, i64 0)
%args46638$Ycmb40103$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47190 = alloca %struct.ScmObj*, align 8
%args46638$Ycmb40103$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40295, %struct.ScmObj* %args46638$Ycmb40103$0)
store volatile %struct.ScmObj* %args46638$Ycmb40103$1, %struct.ScmObj** %stackaddr$prim47190, align 8
%stackaddr$prim47191 = alloca %struct.ScmObj*, align 8
%args46638$Ycmb40103$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42228, %struct.ScmObj* %args46638$Ycmb40103$1)
store volatile %struct.ScmObj* %args46638$Ycmb40103$2, %struct.ScmObj** %stackaddr$prim47191, align 8
%clofunc47192 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40103)
musttail call tailcc void %clofunc47192(%struct.ScmObj* %Ycmb40103, %struct.ScmObj* %args46638$Ycmb40103$2)
ret void
}

define tailcc void @proc_clo$ae42228(%struct.ScmObj* %env$ae42228,%struct.ScmObj* %current_45args46433) {
%stackaddr$env-ref47193 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42228, i64 0)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref47193
%stackaddr$prim47194 = alloca %struct.ScmObj*, align 8
%_95k40371 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46433)
store volatile %struct.ScmObj* %_95k40371, %struct.ScmObj** %stackaddr$prim47194, align 8
%stackaddr$prim47195 = alloca %struct.ScmObj*, align 8
%current_45args46434 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46433)
store volatile %struct.ScmObj* %current_45args46434, %struct.ScmObj** %stackaddr$prim47195, align 8
%stackaddr$prim47196 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46434)
store volatile %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$prim47196, align 8
%stackaddr$makeclosure47197 = alloca %struct.ScmObj*, align 8
%fptrToInt47198 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42230 to i64
%ae42230 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47198)
store volatile %struct.ScmObj* %ae42230, %struct.ScmObj** %stackaddr$makeclosure47197, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42230, %struct.ScmObj* %_37foldl140108, i64 0)
%ae42231 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47199 = alloca %struct.ScmObj*, align 8
%fptrToInt47200 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42232 to i64
%ae42232 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47200)
store volatile %struct.ScmObj* %ae42232, %struct.ScmObj** %stackaddr$makeclosure47199, align 8
%args46637$ae42230$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47201 = alloca %struct.ScmObj*, align 8
%args46637$ae42230$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42232, %struct.ScmObj* %args46637$ae42230$0)
store volatile %struct.ScmObj* %args46637$ae42230$1, %struct.ScmObj** %stackaddr$prim47201, align 8
%stackaddr$prim47202 = alloca %struct.ScmObj*, align 8
%args46637$ae42230$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42231, %struct.ScmObj* %args46637$ae42230$1)
store volatile %struct.ScmObj* %args46637$ae42230$2, %struct.ScmObj** %stackaddr$prim47202, align 8
%clofunc47203 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42230)
musttail call tailcc void %clofunc47203(%struct.ScmObj* %ae42230, %struct.ScmObj* %args46637$ae42230$2)
ret void
}

define tailcc void @proc_clo$ae42230(%struct.ScmObj* %env$ae42230,%struct.ScmObj* %current_45args46436) {
%stackaddr$env-ref47204 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42230, i64 0)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref47204
%stackaddr$prim47205 = alloca %struct.ScmObj*, align 8
%_95k40372 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46436)
store volatile %struct.ScmObj* %_95k40372, %struct.ScmObj** %stackaddr$prim47205, align 8
%stackaddr$prim47206 = alloca %struct.ScmObj*, align 8
%current_45args46437 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46436)
store volatile %struct.ScmObj* %current_45args46437, %struct.ScmObj** %stackaddr$prim47206, align 8
%stackaddr$prim47207 = alloca %struct.ScmObj*, align 8
%_37_6240203 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46437)
store volatile %struct.ScmObj* %_37_6240203, %struct.ScmObj** %stackaddr$prim47207, align 8
%stackaddr$makeclosure47208 = alloca %struct.ScmObj*, align 8
%fptrToInt47209 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42254 to i64
%ae42254 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47209)
store volatile %struct.ScmObj* %ae42254, %struct.ScmObj** %stackaddr$makeclosure47208, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42254, %struct.ScmObj* %_37foldl140108, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42254, %struct.ScmObj* %_37_6240203, i64 1)
%ae42255 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47210 = alloca %struct.ScmObj*, align 8
%fptrToInt47211 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42256 to i64
%ae42256 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47211)
store volatile %struct.ScmObj* %ae42256, %struct.ScmObj** %stackaddr$makeclosure47210, align 8
%args46631$ae42254$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47212 = alloca %struct.ScmObj*, align 8
%args46631$ae42254$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42256, %struct.ScmObj* %args46631$ae42254$0)
store volatile %struct.ScmObj* %args46631$ae42254$1, %struct.ScmObj** %stackaddr$prim47212, align 8
%stackaddr$prim47213 = alloca %struct.ScmObj*, align 8
%args46631$ae42254$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42255, %struct.ScmObj* %args46631$ae42254$1)
store volatile %struct.ScmObj* %args46631$ae42254$2, %struct.ScmObj** %stackaddr$prim47213, align 8
%clofunc47214 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42254)
musttail call tailcc void %clofunc47214(%struct.ScmObj* %ae42254, %struct.ScmObj* %args46631$ae42254$2)
ret void
}

define tailcc void @proc_clo$ae42254(%struct.ScmObj* %env$ae42254,%struct.ScmObj* %current_45args46439) {
%stackaddr$env-ref47215 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42254, i64 0)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref47215
%stackaddr$env-ref47216 = alloca %struct.ScmObj*, align 8
%_37_6240203 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42254, i64 1)
store %struct.ScmObj* %_37_6240203, %struct.ScmObj** %stackaddr$env-ref47216
%stackaddr$prim47217 = alloca %struct.ScmObj*, align 8
%_95k40373 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46439)
store volatile %struct.ScmObj* %_95k40373, %struct.ScmObj** %stackaddr$prim47217, align 8
%stackaddr$prim47218 = alloca %struct.ScmObj*, align 8
%current_45args46440 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46439)
store volatile %struct.ScmObj* %current_45args46440, %struct.ScmObj** %stackaddr$prim47218, align 8
%stackaddr$prim47219 = alloca %struct.ScmObj*, align 8
%_37_62_6140200 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46440)
store volatile %struct.ScmObj* %_37_62_6140200, %struct.ScmObj** %stackaddr$prim47219, align 8
%ae42278 = call %struct.ScmObj* @const_init_int(i64 1)
%ae42279 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47220 = alloca %struct.ScmObj*, align 8
%_37append40196 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42278, %struct.ScmObj* %ae42279)
store volatile %struct.ScmObj* %_37append40196, %struct.ScmObj** %stackaddr$prim47220, align 8
%stackaddr$makeclosure47221 = alloca %struct.ScmObj*, align 8
%fptrToInt47222 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42280 to i64
%ae42280 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47222)
store volatile %struct.ScmObj* %ae42280, %struct.ScmObj** %stackaddr$makeclosure47221, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42280, %struct.ScmObj* %_37append40196, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42280, %struct.ScmObj* %_37foldl140108, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42280, %struct.ScmObj* %_37_6240203, i64 2)
%ae42281 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47223 = alloca %struct.ScmObj*, align 8
%fptrToInt47224 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42282 to i64
%ae42282 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47224)
store volatile %struct.ScmObj* %ae42282, %struct.ScmObj** %stackaddr$makeclosure47223, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42282, %struct.ScmObj* %_37append40196, i64 0)
%args46625$ae42280$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47225 = alloca %struct.ScmObj*, align 8
%args46625$ae42280$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42282, %struct.ScmObj* %args46625$ae42280$0)
store volatile %struct.ScmObj* %args46625$ae42280$1, %struct.ScmObj** %stackaddr$prim47225, align 8
%stackaddr$prim47226 = alloca %struct.ScmObj*, align 8
%args46625$ae42280$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42281, %struct.ScmObj* %args46625$ae42280$1)
store volatile %struct.ScmObj* %args46625$ae42280$2, %struct.ScmObj** %stackaddr$prim47226, align 8
%clofunc47227 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42280)
musttail call tailcc void %clofunc47227(%struct.ScmObj* %ae42280, %struct.ScmObj* %args46625$ae42280$2)
ret void
}

define tailcc void @proc_clo$ae42280(%struct.ScmObj* %env$ae42280,%struct.ScmObj* %current_45args46442) {
%stackaddr$env-ref47228 = alloca %struct.ScmObj*, align 8
%_37append40196 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42280, i64 0)
store %struct.ScmObj* %_37append40196, %struct.ScmObj** %stackaddr$env-ref47228
%stackaddr$env-ref47229 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42280, i64 1)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref47229
%stackaddr$env-ref47230 = alloca %struct.ScmObj*, align 8
%_37_6240203 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42280, i64 2)
store %struct.ScmObj* %_37_6240203, %struct.ScmObj** %stackaddr$env-ref47230
%stackaddr$prim47231 = alloca %struct.ScmObj*, align 8
%_95k40374 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46442)
store volatile %struct.ScmObj* %_95k40374, %struct.ScmObj** %stackaddr$prim47231, align 8
%stackaddr$prim47232 = alloca %struct.ScmObj*, align 8
%current_45args46443 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46442)
store volatile %struct.ScmObj* %current_45args46443, %struct.ScmObj** %stackaddr$prim47232, align 8
%stackaddr$prim47233 = alloca %struct.ScmObj*, align 8
%anf_45bind40303 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46443)
store volatile %struct.ScmObj* %anf_45bind40303, %struct.ScmObj** %stackaddr$prim47233, align 8
%ae42348 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47234 = alloca %struct.ScmObj*, align 8
%_95040197 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append40196, %struct.ScmObj* %ae42348, %struct.ScmObj* %anf_45bind40303)
store volatile %struct.ScmObj* %_95040197, %struct.ScmObj** %stackaddr$prim47234, align 8
%ae42351 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47235 = alloca %struct.ScmObj*, align 8
%_37append40195 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append40196, %struct.ScmObj* %ae42351)
store volatile %struct.ScmObj* %_37append40195, %struct.ScmObj** %stackaddr$prim47235, align 8
%stackaddr$makeclosure47236 = alloca %struct.ScmObj*, align 8
%fptrToInt47237 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42352 to i64
%ae42352 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47237)
store volatile %struct.ScmObj* %ae42352, %struct.ScmObj** %stackaddr$makeclosure47236, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42352, %struct.ScmObj* %_37foldl140108, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42352, %struct.ScmObj* %_37_6240203, i64 1)
%ae42353 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47238 = alloca %struct.ScmObj*, align 8
%fptrToInt47239 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42354 to i64
%ae42354 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47239)
store volatile %struct.ScmObj* %ae42354, %struct.ScmObj** %stackaddr$makeclosure47238, align 8
%args46614$ae42352$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47240 = alloca %struct.ScmObj*, align 8
%args46614$ae42352$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42354, %struct.ScmObj* %args46614$ae42352$0)
store volatile %struct.ScmObj* %args46614$ae42352$1, %struct.ScmObj** %stackaddr$prim47240, align 8
%stackaddr$prim47241 = alloca %struct.ScmObj*, align 8
%args46614$ae42352$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42353, %struct.ScmObj* %args46614$ae42352$1)
store volatile %struct.ScmObj* %args46614$ae42352$2, %struct.ScmObj** %stackaddr$prim47241, align 8
%clofunc47242 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42352)
musttail call tailcc void %clofunc47242(%struct.ScmObj* %ae42352, %struct.ScmObj* %args46614$ae42352$2)
ret void
}

define tailcc void @proc_clo$ae42352(%struct.ScmObj* %env$ae42352,%struct.ScmObj* %current_45args46445) {
%stackaddr$env-ref47243 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42352, i64 0)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref47243
%stackaddr$env-ref47244 = alloca %struct.ScmObj*, align 8
%_37_6240203 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42352, i64 1)
store %struct.ScmObj* %_37_6240203, %struct.ScmObj** %stackaddr$env-ref47244
%stackaddr$prim47245 = alloca %struct.ScmObj*, align 8
%_95k40375 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46445)
store volatile %struct.ScmObj* %_95k40375, %struct.ScmObj** %stackaddr$prim47245, align 8
%stackaddr$prim47246 = alloca %struct.ScmObj*, align 8
%current_45args46446 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46445)
store volatile %struct.ScmObj* %current_45args46446, %struct.ScmObj** %stackaddr$prim47246, align 8
%stackaddr$prim47247 = alloca %struct.ScmObj*, align 8
%_37list_6340188 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46446)
store volatile %struct.ScmObj* %_37list_6340188, %struct.ScmObj** %stackaddr$prim47247, align 8
%stackaddr$makeclosure47248 = alloca %struct.ScmObj*, align 8
%fptrToInt47249 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42768 to i64
%ae42768 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47249)
store volatile %struct.ScmObj* %ae42768, %struct.ScmObj** %stackaddr$makeclosure47248, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42768, %struct.ScmObj* %_37foldl140108, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42768, %struct.ScmObj* %_37_6240203, i64 1)
%ae42769 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47250 = alloca %struct.ScmObj*, align 8
%fptrToInt47251 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42770 to i64
%ae42770 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47251)
store volatile %struct.ScmObj* %ae42770, %struct.ScmObj** %stackaddr$makeclosure47250, align 8
%args46589$ae42768$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47252 = alloca %struct.ScmObj*, align 8
%args46589$ae42768$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42770, %struct.ScmObj* %args46589$ae42768$0)
store volatile %struct.ScmObj* %args46589$ae42768$1, %struct.ScmObj** %stackaddr$prim47252, align 8
%stackaddr$prim47253 = alloca %struct.ScmObj*, align 8
%args46589$ae42768$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42769, %struct.ScmObj* %args46589$ae42768$1)
store volatile %struct.ScmObj* %args46589$ae42768$2, %struct.ScmObj** %stackaddr$prim47253, align 8
%clofunc47254 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42768)
musttail call tailcc void %clofunc47254(%struct.ScmObj* %ae42768, %struct.ScmObj* %args46589$ae42768$2)
ret void
}

define tailcc void @proc_clo$ae42768(%struct.ScmObj* %env$ae42768,%struct.ScmObj* %current_45args46448) {
%stackaddr$env-ref47255 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42768, i64 0)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref47255
%stackaddr$env-ref47256 = alloca %struct.ScmObj*, align 8
%_37_6240203 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42768, i64 1)
store %struct.ScmObj* %_37_6240203, %struct.ScmObj** %stackaddr$env-ref47256
%stackaddr$prim47257 = alloca %struct.ScmObj*, align 8
%_95k40376 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46448)
store volatile %struct.ScmObj* %_95k40376, %struct.ScmObj** %stackaddr$prim47257, align 8
%stackaddr$prim47258 = alloca %struct.ScmObj*, align 8
%current_45args46449 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46448)
store volatile %struct.ScmObj* %current_45args46449, %struct.ScmObj** %stackaddr$prim47258, align 8
%stackaddr$prim47259 = alloca %struct.ScmObj*, align 8
%_37drop40179 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46449)
store volatile %struct.ScmObj* %_37drop40179, %struct.ScmObj** %stackaddr$prim47259, align 8
%stackaddr$makeclosure47260 = alloca %struct.ScmObj*, align 8
%fptrToInt47261 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43304 to i64
%ae43304 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47261)
store volatile %struct.ScmObj* %ae43304, %struct.ScmObj** %stackaddr$makeclosure47260, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43304, %struct.ScmObj* %_37foldl140108, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43304, %struct.ScmObj* %_37_6240203, i64 1)
%ae43305 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47262 = alloca %struct.ScmObj*, align 8
%fptrToInt47263 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43306 to i64
%ae43306 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47263)
store volatile %struct.ScmObj* %ae43306, %struct.ScmObj** %stackaddr$makeclosure47262, align 8
%args46565$ae43304$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47264 = alloca %struct.ScmObj*, align 8
%args46565$ae43304$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43306, %struct.ScmObj* %args46565$ae43304$0)
store volatile %struct.ScmObj* %args46565$ae43304$1, %struct.ScmObj** %stackaddr$prim47264, align 8
%stackaddr$prim47265 = alloca %struct.ScmObj*, align 8
%args46565$ae43304$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43305, %struct.ScmObj* %args46565$ae43304$1)
store volatile %struct.ScmObj* %args46565$ae43304$2, %struct.ScmObj** %stackaddr$prim47265, align 8
%clofunc47266 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43304)
musttail call tailcc void %clofunc47266(%struct.ScmObj* %ae43304, %struct.ScmObj* %args46565$ae43304$2)
ret void
}

define tailcc void @proc_clo$ae43304(%struct.ScmObj* %env$ae43304,%struct.ScmObj* %current_45args46451) {
%stackaddr$env-ref47267 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43304, i64 0)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref47267
%stackaddr$env-ref47268 = alloca %struct.ScmObj*, align 8
%_37_6240203 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43304, i64 1)
store %struct.ScmObj* %_37_6240203, %struct.ScmObj** %stackaddr$env-ref47268
%stackaddr$prim47269 = alloca %struct.ScmObj*, align 8
%_95k40377 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46451)
store volatile %struct.ScmObj* %_95k40377, %struct.ScmObj** %stackaddr$prim47269, align 8
%stackaddr$prim47270 = alloca %struct.ScmObj*, align 8
%current_45args46452 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46451)
store volatile %struct.ScmObj* %current_45args46452, %struct.ScmObj** %stackaddr$prim47270, align 8
%stackaddr$prim47271 = alloca %struct.ScmObj*, align 8
%_37memv40172 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46452)
store volatile %struct.ScmObj* %_37memv40172, %struct.ScmObj** %stackaddr$prim47271, align 8
%stackaddr$makeclosure47272 = alloca %struct.ScmObj*, align 8
%fptrToInt47273 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43708 to i64
%ae43708 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47273)
store volatile %struct.ScmObj* %ae43708, %struct.ScmObj** %stackaddr$makeclosure47272, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43708, %struct.ScmObj* %_37_6240203, i64 0)
%ae43709 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47274 = alloca %struct.ScmObj*, align 8
%fptrToInt47275 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43710 to i64
%ae43710 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47275)
store volatile %struct.ScmObj* %ae43710, %struct.ScmObj** %stackaddr$makeclosure47274, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43710, %struct.ScmObj* %_37foldl140108, i64 0)
%args46539$ae43708$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47276 = alloca %struct.ScmObj*, align 8
%args46539$ae43708$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43710, %struct.ScmObj* %args46539$ae43708$0)
store volatile %struct.ScmObj* %args46539$ae43708$1, %struct.ScmObj** %stackaddr$prim47276, align 8
%stackaddr$prim47277 = alloca %struct.ScmObj*, align 8
%args46539$ae43708$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43709, %struct.ScmObj* %args46539$ae43708$1)
store volatile %struct.ScmObj* %args46539$ae43708$2, %struct.ScmObj** %stackaddr$prim47277, align 8
%clofunc47278 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43708)
musttail call tailcc void %clofunc47278(%struct.ScmObj* %ae43708, %struct.ScmObj* %args46539$ae43708$2)
ret void
}

define tailcc void @proc_clo$ae43708(%struct.ScmObj* %env$ae43708,%struct.ScmObj* %current_45args46454) {
%stackaddr$env-ref47279 = alloca %struct.ScmObj*, align 8
%_37_6240203 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43708, i64 0)
store %struct.ScmObj* %_37_6240203, %struct.ScmObj** %stackaddr$env-ref47279
%stackaddr$prim47280 = alloca %struct.ScmObj*, align 8
%_95k40378 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46454)
store volatile %struct.ScmObj* %_95k40378, %struct.ScmObj** %stackaddr$prim47280, align 8
%stackaddr$prim47281 = alloca %struct.ScmObj*, align 8
%current_45args46455 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46454)
store volatile %struct.ScmObj* %current_45args46455, %struct.ScmObj** %stackaddr$prim47281, align 8
%stackaddr$prim47282 = alloca %struct.ScmObj*, align 8
%_37_4740168 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46455)
store volatile %struct.ScmObj* %_37_4740168, %struct.ScmObj** %stackaddr$prim47282, align 8
%stackaddr$makeclosure47283 = alloca %struct.ScmObj*, align 8
%fptrToInt47284 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43806 to i64
%ae43806 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47284)
store volatile %struct.ScmObj* %ae43806, %struct.ScmObj** %stackaddr$makeclosure47283, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43806, %struct.ScmObj* %_37_6240203, i64 0)
%ae43807 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47285 = alloca %struct.ScmObj*, align 8
%fptrToInt47286 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43808 to i64
%ae43808 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47286)
store volatile %struct.ScmObj* %ae43808, %struct.ScmObj** %stackaddr$makeclosure47285, align 8
%args46526$ae43806$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47287 = alloca %struct.ScmObj*, align 8
%args46526$ae43806$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43808, %struct.ScmObj* %args46526$ae43806$0)
store volatile %struct.ScmObj* %args46526$ae43806$1, %struct.ScmObj** %stackaddr$prim47287, align 8
%stackaddr$prim47288 = alloca %struct.ScmObj*, align 8
%args46526$ae43806$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43807, %struct.ScmObj* %args46526$ae43806$1)
store volatile %struct.ScmObj* %args46526$ae43806$2, %struct.ScmObj** %stackaddr$prim47288, align 8
%clofunc47289 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43806)
musttail call tailcc void %clofunc47289(%struct.ScmObj* %ae43806, %struct.ScmObj* %args46526$ae43806$2)
ret void
}

define tailcc void @proc_clo$ae43806(%struct.ScmObj* %env$ae43806,%struct.ScmObj* %current_45args46457) {
%stackaddr$env-ref47290 = alloca %struct.ScmObj*, align 8
%_37_6240203 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43806, i64 0)
store %struct.ScmObj* %_37_6240203, %struct.ScmObj** %stackaddr$env-ref47290
%stackaddr$prim47291 = alloca %struct.ScmObj*, align 8
%_95k40379 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46457)
store volatile %struct.ScmObj* %_95k40379, %struct.ScmObj** %stackaddr$prim47291, align 8
%stackaddr$prim47292 = alloca %struct.ScmObj*, align 8
%current_45args46458 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46457)
store volatile %struct.ScmObj* %current_45args46458, %struct.ScmObj** %stackaddr$prim47292, align 8
%stackaddr$prim47293 = alloca %struct.ScmObj*, align 8
%_37first40166 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46458)
store volatile %struct.ScmObj* %_37first40166, %struct.ScmObj** %stackaddr$prim47293, align 8
%stackaddr$makeclosure47294 = alloca %struct.ScmObj*, align 8
%fptrToInt47295 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43826 to i64
%ae43826 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47295)
store volatile %struct.ScmObj* %ae43826, %struct.ScmObj** %stackaddr$makeclosure47294, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43826, %struct.ScmObj* %_37_6240203, i64 0)
%ae43827 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47296 = alloca %struct.ScmObj*, align 8
%fptrToInt47297 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43828 to i64
%ae43828 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47297)
store volatile %struct.ScmObj* %ae43828, %struct.ScmObj** %stackaddr$makeclosure47296, align 8
%args46521$ae43826$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47298 = alloca %struct.ScmObj*, align 8
%args46521$ae43826$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43828, %struct.ScmObj* %args46521$ae43826$0)
store volatile %struct.ScmObj* %args46521$ae43826$1, %struct.ScmObj** %stackaddr$prim47298, align 8
%stackaddr$prim47299 = alloca %struct.ScmObj*, align 8
%args46521$ae43826$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43827, %struct.ScmObj* %args46521$ae43826$1)
store volatile %struct.ScmObj* %args46521$ae43826$2, %struct.ScmObj** %stackaddr$prim47299, align 8
%clofunc47300 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43826)
musttail call tailcc void %clofunc47300(%struct.ScmObj* %ae43826, %struct.ScmObj* %args46521$ae43826$2)
ret void
}

define tailcc void @proc_clo$ae43826(%struct.ScmObj* %env$ae43826,%struct.ScmObj* %current_45args46460) {
%stackaddr$env-ref47301 = alloca %struct.ScmObj*, align 8
%_37_6240203 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43826, i64 0)
store %struct.ScmObj* %_37_6240203, %struct.ScmObj** %stackaddr$env-ref47301
%stackaddr$prim47302 = alloca %struct.ScmObj*, align 8
%_95k40380 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46460)
store volatile %struct.ScmObj* %_95k40380, %struct.ScmObj** %stackaddr$prim47302, align 8
%stackaddr$prim47303 = alloca %struct.ScmObj*, align 8
%current_45args46461 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46460)
store volatile %struct.ScmObj* %current_45args46461, %struct.ScmObj** %stackaddr$prim47303, align 8
%stackaddr$prim47304 = alloca %struct.ScmObj*, align 8
%_37second40164 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46461)
store volatile %struct.ScmObj* %_37second40164, %struct.ScmObj** %stackaddr$prim47304, align 8
%stackaddr$makeclosure47305 = alloca %struct.ScmObj*, align 8
%fptrToInt47306 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43848 to i64
%ae43848 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47306)
store volatile %struct.ScmObj* %ae43848, %struct.ScmObj** %stackaddr$makeclosure47305, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43848, %struct.ScmObj* %_37_6240203, i64 0)
%ae43849 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47307 = alloca %struct.ScmObj*, align 8
%fptrToInt47308 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43850 to i64
%ae43850 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47308)
store volatile %struct.ScmObj* %ae43850, %struct.ScmObj** %stackaddr$makeclosure47307, align 8
%args46516$ae43848$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47309 = alloca %struct.ScmObj*, align 8
%args46516$ae43848$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43850, %struct.ScmObj* %args46516$ae43848$0)
store volatile %struct.ScmObj* %args46516$ae43848$1, %struct.ScmObj** %stackaddr$prim47309, align 8
%stackaddr$prim47310 = alloca %struct.ScmObj*, align 8
%args46516$ae43848$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43849, %struct.ScmObj* %args46516$ae43848$1)
store volatile %struct.ScmObj* %args46516$ae43848$2, %struct.ScmObj** %stackaddr$prim47310, align 8
%clofunc47311 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43848)
musttail call tailcc void %clofunc47311(%struct.ScmObj* %ae43848, %struct.ScmObj* %args46516$ae43848$2)
ret void
}

define tailcc void @proc_clo$ae43848(%struct.ScmObj* %env$ae43848,%struct.ScmObj* %current_45args46463) {
%stackaddr$env-ref47312 = alloca %struct.ScmObj*, align 8
%_37_6240203 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43848, i64 0)
store %struct.ScmObj* %_37_6240203, %struct.ScmObj** %stackaddr$env-ref47312
%stackaddr$prim47313 = alloca %struct.ScmObj*, align 8
%_95k40381 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46463)
store volatile %struct.ScmObj* %_95k40381, %struct.ScmObj** %stackaddr$prim47313, align 8
%stackaddr$prim47314 = alloca %struct.ScmObj*, align 8
%current_45args46464 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46463)
store volatile %struct.ScmObj* %current_45args46464, %struct.ScmObj** %stackaddr$prim47314, align 8
%stackaddr$prim47315 = alloca %struct.ScmObj*, align 8
%_37third40162 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46464)
store volatile %struct.ScmObj* %_37third40162, %struct.ScmObj** %stackaddr$prim47315, align 8
%stackaddr$makeclosure47316 = alloca %struct.ScmObj*, align 8
%fptrToInt47317 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43872 to i64
%ae43872 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47317)
store volatile %struct.ScmObj* %ae43872, %struct.ScmObj** %stackaddr$makeclosure47316, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43872, %struct.ScmObj* %_37_6240203, i64 0)
%ae43873 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47318 = alloca %struct.ScmObj*, align 8
%fptrToInt47319 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43874 to i64
%ae43874 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47319)
store volatile %struct.ScmObj* %ae43874, %struct.ScmObj** %stackaddr$makeclosure47318, align 8
%args46511$ae43872$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47320 = alloca %struct.ScmObj*, align 8
%args46511$ae43872$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43874, %struct.ScmObj* %args46511$ae43872$0)
store volatile %struct.ScmObj* %args46511$ae43872$1, %struct.ScmObj** %stackaddr$prim47320, align 8
%stackaddr$prim47321 = alloca %struct.ScmObj*, align 8
%args46511$ae43872$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43873, %struct.ScmObj* %args46511$ae43872$1)
store volatile %struct.ScmObj* %args46511$ae43872$2, %struct.ScmObj** %stackaddr$prim47321, align 8
%clofunc47322 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43872)
musttail call tailcc void %clofunc47322(%struct.ScmObj* %ae43872, %struct.ScmObj* %args46511$ae43872$2)
ret void
}

define tailcc void @proc_clo$ae43872(%struct.ScmObj* %env$ae43872,%struct.ScmObj* %current_45args46466) {
%stackaddr$env-ref47323 = alloca %struct.ScmObj*, align 8
%_37_6240203 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43872, i64 0)
store %struct.ScmObj* %_37_6240203, %struct.ScmObj** %stackaddr$env-ref47323
%stackaddr$prim47324 = alloca %struct.ScmObj*, align 8
%_95k40382 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46466)
store volatile %struct.ScmObj* %_95k40382, %struct.ScmObj** %stackaddr$prim47324, align 8
%stackaddr$prim47325 = alloca %struct.ScmObj*, align 8
%current_45args46467 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46466)
store volatile %struct.ScmObj* %current_45args46467, %struct.ScmObj** %stackaddr$prim47325, align 8
%stackaddr$prim47326 = alloca %struct.ScmObj*, align 8
%_37fourth40160 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46467)
store volatile %struct.ScmObj* %_37fourth40160, %struct.ScmObj** %stackaddr$prim47326, align 8
%stackaddr$makeclosure47327 = alloca %struct.ScmObj*, align 8
%fptrToInt47328 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43898 to i64
%ae43898 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47328)
store volatile %struct.ScmObj* %ae43898, %struct.ScmObj** %stackaddr$makeclosure47327, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43898, %struct.ScmObj* %_37_6240203, i64 0)
%ae43899 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47329 = alloca %struct.ScmObj*, align 8
%fptrToInt47330 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43900 to i64
%ae43900 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47330)
store volatile %struct.ScmObj* %ae43900, %struct.ScmObj** %stackaddr$makeclosure47329, align 8
%args46506$ae43898$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47331 = alloca %struct.ScmObj*, align 8
%args46506$ae43898$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43900, %struct.ScmObj* %args46506$ae43898$0)
store volatile %struct.ScmObj* %args46506$ae43898$1, %struct.ScmObj** %stackaddr$prim47331, align 8
%stackaddr$prim47332 = alloca %struct.ScmObj*, align 8
%args46506$ae43898$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43899, %struct.ScmObj* %args46506$ae43898$1)
store volatile %struct.ScmObj* %args46506$ae43898$2, %struct.ScmObj** %stackaddr$prim47332, align 8
%clofunc47333 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43898)
musttail call tailcc void %clofunc47333(%struct.ScmObj* %ae43898, %struct.ScmObj* %args46506$ae43898$2)
ret void
}

define tailcc void @proc_clo$ae43898(%struct.ScmObj* %env$ae43898,%struct.ScmObj* %current_45args46469) {
%stackaddr$env-ref47334 = alloca %struct.ScmObj*, align 8
%_37_6240203 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43898, i64 0)
store %struct.ScmObj* %_37_6240203, %struct.ScmObj** %stackaddr$env-ref47334
%stackaddr$prim47335 = alloca %struct.ScmObj*, align 8
%_95k40383 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46469)
store volatile %struct.ScmObj* %_95k40383, %struct.ScmObj** %stackaddr$prim47335, align 8
%stackaddr$prim47336 = alloca %struct.ScmObj*, align 8
%current_45args46470 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46469)
store volatile %struct.ScmObj* %current_45args46470, %struct.ScmObj** %stackaddr$prim47336, align 8
%stackaddr$prim47337 = alloca %struct.ScmObj*, align 8
%promise_6340221 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46470)
store volatile %struct.ScmObj* %promise_6340221, %struct.ScmObj** %stackaddr$prim47337, align 8
%stackaddr$prim47338 = alloca %struct.ScmObj*, align 8
%anf_45bind40343 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind40343, %struct.ScmObj** %stackaddr$prim47338, align 8
%ae43985 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim47339 = alloca %struct.ScmObj*, align 8
%loop40223 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae43985, %struct.ScmObj* %anf_45bind40343)
store volatile %struct.ScmObj* %loop40223, %struct.ScmObj** %stackaddr$prim47339, align 8
%stackaddr$makeclosure47340 = alloca %struct.ScmObj*, align 8
%fptrToInt47341 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43987 to i64
%ae43987 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47341)
store volatile %struct.ScmObj* %ae43987, %struct.ScmObj** %stackaddr$makeclosure47340, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43987, %struct.ScmObj* %loop40223, i64 0)
%ae43988 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47342 = alloca %struct.ScmObj*, align 8
%fptrToInt47343 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43989 to i64
%ae43989 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47343)
store volatile %struct.ScmObj* %ae43989, %struct.ScmObj** %stackaddr$makeclosure47342, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43989, %struct.ScmObj* %loop40223, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43989, %struct.ScmObj* %_37_6240203, i64 1)
%args46499$ae43987$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47344 = alloca %struct.ScmObj*, align 8
%args46499$ae43987$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43989, %struct.ScmObj* %args46499$ae43987$0)
store volatile %struct.ScmObj* %args46499$ae43987$1, %struct.ScmObj** %stackaddr$prim47344, align 8
%stackaddr$prim47345 = alloca %struct.ScmObj*, align 8
%args46499$ae43987$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43988, %struct.ScmObj* %args46499$ae43987$1)
store volatile %struct.ScmObj* %args46499$ae43987$2, %struct.ScmObj** %stackaddr$prim47345, align 8
%clofunc47346 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43987)
musttail call tailcc void %clofunc47346(%struct.ScmObj* %ae43987, %struct.ScmObj* %args46499$ae43987$2)
ret void
}

define tailcc void @proc_clo$ae43987(%struct.ScmObj* %env$ae43987,%struct.ScmObj* %current_45args46472) {
%stackaddr$env-ref47347 = alloca %struct.ScmObj*, align 8
%loop40223 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43987, i64 0)
store %struct.ScmObj* %loop40223, %struct.ScmObj** %stackaddr$env-ref47347
%stackaddr$prim47348 = alloca %struct.ScmObj*, align 8
%_95k40384 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46472)
store volatile %struct.ScmObj* %_95k40384, %struct.ScmObj** %stackaddr$prim47348, align 8
%stackaddr$prim47349 = alloca %struct.ScmObj*, align 8
%current_45args46473 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46472)
store volatile %struct.ScmObj* %current_45args46473, %struct.ScmObj** %stackaddr$prim47349, align 8
%stackaddr$prim47350 = alloca %struct.ScmObj*, align 8
%anf_45bind40349 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46473)
store volatile %struct.ScmObj* %anf_45bind40349, %struct.ScmObj** %stackaddr$prim47350, align 8
%ae44103 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47351 = alloca %struct.ScmObj*, align 8
%t4010240224 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %loop40223, %struct.ScmObj* %ae44103, %struct.ScmObj* %anf_45bind40349)
store volatile %struct.ScmObj* %t4010240224, %struct.ScmObj** %stackaddr$prim47351, align 8
%ae44106 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47352 = alloca %struct.ScmObj*, align 8
%anf_45bind40350 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %loop40223, %struct.ScmObj* %ae44106)
store volatile %struct.ScmObj* %anf_45bind40350, %struct.ScmObj** %stackaddr$prim47352, align 8
%stackaddr$makeclosure47353 = alloca %struct.ScmObj*, align 8
%fptrToInt47354 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44108 to i64
%ae44108 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47354)
store volatile %struct.ScmObj* %ae44108, %struct.ScmObj** %stackaddr$makeclosure47353, align 8
%ae44109 = call %struct.ScmObj* @const_init_int(i64 1)
%ae44110 = call %struct.ScmObj* @const_init_int(i64 2)
%args46479$anf_45bind40350$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47355 = alloca %struct.ScmObj*, align 8
%args46479$anf_45bind40350$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44110, %struct.ScmObj* %args46479$anf_45bind40350$0)
store volatile %struct.ScmObj* %args46479$anf_45bind40350$1, %struct.ScmObj** %stackaddr$prim47355, align 8
%stackaddr$prim47356 = alloca %struct.ScmObj*, align 8
%args46479$anf_45bind40350$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44109, %struct.ScmObj* %args46479$anf_45bind40350$1)
store volatile %struct.ScmObj* %args46479$anf_45bind40350$2, %struct.ScmObj** %stackaddr$prim47356, align 8
%stackaddr$prim47357 = alloca %struct.ScmObj*, align 8
%args46479$anf_45bind40350$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44108, %struct.ScmObj* %args46479$anf_45bind40350$2)
store volatile %struct.ScmObj* %args46479$anf_45bind40350$3, %struct.ScmObj** %stackaddr$prim47357, align 8
%clofunc47358 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40350)
musttail call tailcc void %clofunc47358(%struct.ScmObj* %anf_45bind40350, %struct.ScmObj* %args46479$anf_45bind40350$3)
ret void
}

define tailcc void @proc_clo$ae44108(%struct.ScmObj* %env$ae44108,%struct.ScmObj* %current_45args46475) {
%stackaddr$prim47359 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46475)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim47359, align 8
%stackaddr$prim47360 = alloca %struct.ScmObj*, align 8
%current_45args46476 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46475)
store volatile %struct.ScmObj* %current_45args46476, %struct.ScmObj** %stackaddr$prim47360, align 8
%stackaddr$prim47361 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46476)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim47361, align 8
%stackaddr$prim47362 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim47362, align 8
%args46478$k$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47363 = alloca %struct.ScmObj*, align 8
%args46478$k$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %args46478$k$0)
store volatile %struct.ScmObj* %args46478$k$1, %struct.ScmObj** %stackaddr$prim47363, align 8
%clofunc47364 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc47364(%struct.ScmObj* %k, %struct.ScmObj* %args46478$k$1)
ret void
}

define tailcc void @proc_clo$ae43989(%struct.ScmObj* %env$ae43989,%struct.ScmObj* %current_45args46480) {
%stackaddr$env-ref47365 = alloca %struct.ScmObj*, align 8
%loop40223 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43989, i64 0)
store %struct.ScmObj* %loop40223, %struct.ScmObj** %stackaddr$env-ref47365
%stackaddr$env-ref47366 = alloca %struct.ScmObj*, align 8
%_37_6240203 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43989, i64 1)
store %struct.ScmObj* %_37_6240203, %struct.ScmObj** %stackaddr$env-ref47366
%stackaddr$prim47367 = alloca %struct.ScmObj*, align 8
%k40385 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46480)
store volatile %struct.ScmObj* %k40385, %struct.ScmObj** %stackaddr$prim47367, align 8
%stackaddr$prim47368 = alloca %struct.ScmObj*, align 8
%current_45args46481 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46480)
store volatile %struct.ScmObj* %current_45args46481, %struct.ScmObj** %stackaddr$prim47368, align 8
%stackaddr$prim47369 = alloca %struct.ScmObj*, align 8
%m40226 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46481)
store volatile %struct.ScmObj* %m40226, %struct.ScmObj** %stackaddr$prim47369, align 8
%stackaddr$prim47370 = alloca %struct.ScmObj*, align 8
%current_45args46482 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46481)
store volatile %struct.ScmObj* %current_45args46482, %struct.ScmObj** %stackaddr$prim47370, align 8
%stackaddr$prim47371 = alloca %struct.ScmObj*, align 8
%n40225 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46482)
store volatile %struct.ScmObj* %n40225, %struct.ScmObj** %stackaddr$prim47371, align 8
%stackaddr$makeclosure47372 = alloca %struct.ScmObj*, align 8
%fptrToInt47373 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43991 to i64
%ae43991 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt47373)
store volatile %struct.ScmObj* %ae43991, %struct.ScmObj** %stackaddr$makeclosure47372, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43991, %struct.ScmObj* %n40225, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43991, %struct.ScmObj* %k40385, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43991, %struct.ScmObj* %m40226, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae43991, %struct.ScmObj* %loop40223, i64 3)
%ae43993 = call %struct.ScmObj* @const_init_int(i64 1000)
%args46498$_37_6240203$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47374 = alloca %struct.ScmObj*, align 8
%args46498$_37_6240203$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43993, %struct.ScmObj* %args46498$_37_6240203$0)
store volatile %struct.ScmObj* %args46498$_37_6240203$1, %struct.ScmObj** %stackaddr$prim47374, align 8
%stackaddr$prim47375 = alloca %struct.ScmObj*, align 8
%args46498$_37_6240203$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %m40226, %struct.ScmObj* %args46498$_37_6240203$1)
store volatile %struct.ScmObj* %args46498$_37_6240203$2, %struct.ScmObj** %stackaddr$prim47375, align 8
%stackaddr$prim47376 = alloca %struct.ScmObj*, align 8
%args46498$_37_6240203$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43991, %struct.ScmObj* %args46498$_37_6240203$2)
store volatile %struct.ScmObj* %args46498$_37_6240203$3, %struct.ScmObj** %stackaddr$prim47376, align 8
%clofunc47377 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37_6240203)
musttail call tailcc void %clofunc47377(%struct.ScmObj* %_37_6240203, %struct.ScmObj* %args46498$_37_6240203$3)
ret void
}

define tailcc void @proc_clo$ae43991(%struct.ScmObj* %env$ae43991,%struct.ScmObj* %current_45args46484) {
%stackaddr$env-ref47378 = alloca %struct.ScmObj*, align 8
%n40225 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43991, i64 0)
store %struct.ScmObj* %n40225, %struct.ScmObj** %stackaddr$env-ref47378
%stackaddr$env-ref47379 = alloca %struct.ScmObj*, align 8
%k40385 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43991, i64 1)
store %struct.ScmObj* %k40385, %struct.ScmObj** %stackaddr$env-ref47379
%stackaddr$env-ref47380 = alloca %struct.ScmObj*, align 8
%m40226 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43991, i64 2)
store %struct.ScmObj* %m40226, %struct.ScmObj** %stackaddr$env-ref47380
%stackaddr$env-ref47381 = alloca %struct.ScmObj*, align 8
%loop40223 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43991, i64 3)
store %struct.ScmObj* %loop40223, %struct.ScmObj** %stackaddr$env-ref47381
%stackaddr$prim47382 = alloca %struct.ScmObj*, align 8
%_95k40386 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46484)
store volatile %struct.ScmObj* %_95k40386, %struct.ScmObj** %stackaddr$prim47382, align 8
%stackaddr$prim47383 = alloca %struct.ScmObj*, align 8
%current_45args46485 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46484)
store volatile %struct.ScmObj* %current_45args46485, %struct.ScmObj** %stackaddr$prim47383, align 8
%stackaddr$prim47384 = alloca %struct.ScmObj*, align 8
%anf_45bind40344 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46485)
store volatile %struct.ScmObj* %anf_45bind40344, %struct.ScmObj** %stackaddr$prim47384, align 8
%truthy$cmp47385 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40344)
%cmp$cmp47385 = icmp eq i64 %truthy$cmp47385, 1
br i1 %cmp$cmp47385, label %truebranch$cmp47385, label %falsebranch$cmp47385
truebranch$cmp47385:
%stackaddr$makeclosure47386 = alloca %struct.ScmObj*, align 8
%fptrToInt47387 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43999 to i64
%ae43999 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47387)
store volatile %struct.ScmObj* %ae43999, %struct.ScmObj** %stackaddr$makeclosure47386, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43999, %struct.ScmObj* %k40385, i64 0)
%ae44000 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47388 = alloca %struct.ScmObj*, align 8
%fptrToInt47389 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44001 to i64
%ae44001 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47389)
store volatile %struct.ScmObj* %ae44001, %struct.ScmObj** %stackaddr$makeclosure47388, align 8
%args46492$ae43999$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47390 = alloca %struct.ScmObj*, align 8
%args46492$ae43999$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44001, %struct.ScmObj* %args46492$ae43999$0)
store volatile %struct.ScmObj* %args46492$ae43999$1, %struct.ScmObj** %stackaddr$prim47390, align 8
%stackaddr$prim47391 = alloca %struct.ScmObj*, align 8
%args46492$ae43999$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44000, %struct.ScmObj* %args46492$ae43999$1)
store volatile %struct.ScmObj* %args46492$ae43999$2, %struct.ScmObj** %stackaddr$prim47391, align 8
%clofunc47392 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43999)
musttail call tailcc void %clofunc47392(%struct.ScmObj* %ae43999, %struct.ScmObj* %args46492$ae43999$2)
ret void
falsebranch$cmp47385:
%ae44032 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47393 = alloca %struct.ScmObj*, align 8
%anf_45bind40346 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %loop40223, %struct.ScmObj* %ae44032)
store volatile %struct.ScmObj* %anf_45bind40346, %struct.ScmObj** %stackaddr$prim47393, align 8
%stackaddr$prim47394 = alloca %struct.ScmObj*, align 8
%anf_45bind40347 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %m40226, %struct.ScmObj* %n40225)
store volatile %struct.ScmObj* %anf_45bind40347, %struct.ScmObj** %stackaddr$prim47394, align 8
%stackaddr$makeclosure47395 = alloca %struct.ScmObj*, align 8
%fptrToInt47396 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44036 to i64
%ae44036 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47396)
store volatile %struct.ScmObj* %ae44036, %struct.ScmObj** %stackaddr$makeclosure47395, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44036, %struct.ScmObj* %m40226, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44036, %struct.ScmObj* %k40385, i64 1)
%args46497$anf_45bind40346$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47397 = alloca %struct.ScmObj*, align 8
%args46497$anf_45bind40346$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40347, %struct.ScmObj* %args46497$anf_45bind40346$0)
store volatile %struct.ScmObj* %args46497$anf_45bind40346$1, %struct.ScmObj** %stackaddr$prim47397, align 8
%stackaddr$prim47398 = alloca %struct.ScmObj*, align 8
%args46497$anf_45bind40346$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %n40225, %struct.ScmObj* %args46497$anf_45bind40346$1)
store volatile %struct.ScmObj* %args46497$anf_45bind40346$2, %struct.ScmObj** %stackaddr$prim47398, align 8
%stackaddr$prim47399 = alloca %struct.ScmObj*, align 8
%args46497$anf_45bind40346$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44036, %struct.ScmObj* %args46497$anf_45bind40346$2)
store volatile %struct.ScmObj* %args46497$anf_45bind40346$3, %struct.ScmObj** %stackaddr$prim47399, align 8
%clofunc47400 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40346)
musttail call tailcc void %clofunc47400(%struct.ScmObj* %anf_45bind40346, %struct.ScmObj* %args46497$anf_45bind40346$3)
ret void
}

define tailcc void @proc_clo$ae43999(%struct.ScmObj* %env$ae43999,%struct.ScmObj* %current_45args46487) {
%stackaddr$env-ref47401 = alloca %struct.ScmObj*, align 8
%k40385 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43999, i64 0)
store %struct.ScmObj* %k40385, %struct.ScmObj** %stackaddr$env-ref47401
%stackaddr$prim47402 = alloca %struct.ScmObj*, align 8
%_95k40387 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46487)
store volatile %struct.ScmObj* %_95k40387, %struct.ScmObj** %stackaddr$prim47402, align 8
%stackaddr$prim47403 = alloca %struct.ScmObj*, align 8
%current_45args46488 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46487)
store volatile %struct.ScmObj* %current_45args46488, %struct.ScmObj** %stackaddr$prim47403, align 8
%stackaddr$prim47404 = alloca %struct.ScmObj*, align 8
%anf_45bind40345 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46488)
store volatile %struct.ScmObj* %anf_45bind40345, %struct.ScmObj** %stackaddr$prim47404, align 8
%args46490$anf_45bind40345$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47405 = alloca %struct.ScmObj*, align 8
%args46490$anf_45bind40345$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40385, %struct.ScmObj* %args46490$anf_45bind40345$0)
store volatile %struct.ScmObj* %args46490$anf_45bind40345$1, %struct.ScmObj** %stackaddr$prim47405, align 8
%clofunc47406 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40345)
musttail call tailcc void %clofunc47406(%struct.ScmObj* %anf_45bind40345, %struct.ScmObj* %args46490$anf_45bind40345$1)
ret void
}

define tailcc void @proc_clo$ae44001(%struct.ScmObj* %env$ae44001,%struct.ScmObj* %lst4022740388) {
%stackaddr$prim47407 = alloca %struct.ScmObj*, align 8
%k40389 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4022740388)
store volatile %struct.ScmObj* %k40389, %struct.ScmObj** %stackaddr$prim47407, align 8
%stackaddr$prim47408 = alloca %struct.ScmObj*, align 8
%lst40227 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4022740388)
store volatile %struct.ScmObj* %lst40227, %struct.ScmObj** %stackaddr$prim47408, align 8
%ae44005 = call %struct.ScmObj* @const_init_int(i64 0)
%args46491$k40389$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47409 = alloca %struct.ScmObj*, align 8
%args46491$k40389$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40227, %struct.ScmObj* %args46491$k40389$0)
store volatile %struct.ScmObj* %args46491$k40389$1, %struct.ScmObj** %stackaddr$prim47409, align 8
%stackaddr$prim47410 = alloca %struct.ScmObj*, align 8
%args46491$k40389$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44005, %struct.ScmObj* %args46491$k40389$1)
store volatile %struct.ScmObj* %args46491$k40389$2, %struct.ScmObj** %stackaddr$prim47410, align 8
%clofunc47411 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40389)
musttail call tailcc void %clofunc47411(%struct.ScmObj* %k40389, %struct.ScmObj* %args46491$k40389$2)
ret void
}

define tailcc void @proc_clo$ae44036(%struct.ScmObj* %env$ae44036,%struct.ScmObj* %current_45args46493) {
%stackaddr$env-ref47412 = alloca %struct.ScmObj*, align 8
%m40226 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44036, i64 0)
store %struct.ScmObj* %m40226, %struct.ScmObj** %stackaddr$env-ref47412
%stackaddr$env-ref47413 = alloca %struct.ScmObj*, align 8
%k40385 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44036, i64 1)
store %struct.ScmObj* %k40385, %struct.ScmObj** %stackaddr$env-ref47413
%stackaddr$prim47414 = alloca %struct.ScmObj*, align 8
%_95k40390 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46493)
store volatile %struct.ScmObj* %_95k40390, %struct.ScmObj** %stackaddr$prim47414, align 8
%stackaddr$prim47415 = alloca %struct.ScmObj*, align 8
%current_45args46494 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46493)
store volatile %struct.ScmObj* %current_45args46494, %struct.ScmObj** %stackaddr$prim47415, align 8
%stackaddr$prim47416 = alloca %struct.ScmObj*, align 8
%anf_45bind40348 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46494)
store volatile %struct.ScmObj* %anf_45bind40348, %struct.ScmObj** %stackaddr$prim47416, align 8
%stackaddr$prim47417 = alloca %struct.ScmObj*, align 8
%cpsprim40391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %m40226, %struct.ScmObj* %anf_45bind40348)
store volatile %struct.ScmObj* %cpsprim40391, %struct.ScmObj** %stackaddr$prim47417, align 8
%ae44042 = call %struct.ScmObj* @const_init_int(i64 0)
%args46496$k40385$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47418 = alloca %struct.ScmObj*, align 8
%args46496$k40385$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40391, %struct.ScmObj* %args46496$k40385$0)
store volatile %struct.ScmObj* %args46496$k40385$1, %struct.ScmObj** %stackaddr$prim47418, align 8
%stackaddr$prim47419 = alloca %struct.ScmObj*, align 8
%args46496$k40385$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44042, %struct.ScmObj* %args46496$k40385$1)
store volatile %struct.ScmObj* %args46496$k40385$2, %struct.ScmObj** %stackaddr$prim47419, align 8
%clofunc47420 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40385)
musttail call tailcc void %clofunc47420(%struct.ScmObj* %k40385, %struct.ScmObj* %args46496$k40385$2)
ret void
}

define tailcc void @proc_clo$ae43900(%struct.ScmObj* %env$ae43900,%struct.ScmObj* %current_45args46500) {
%stackaddr$prim47421 = alloca %struct.ScmObj*, align 8
%k40392 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46500)
store volatile %struct.ScmObj* %k40392, %struct.ScmObj** %stackaddr$prim47421, align 8
%stackaddr$prim47422 = alloca %struct.ScmObj*, align 8
%current_45args46501 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46500)
store volatile %struct.ScmObj* %current_45args46501, %struct.ScmObj** %stackaddr$prim47422, align 8
%stackaddr$prim47423 = alloca %struct.ScmObj*, align 8
%thunk40222 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46501)
store volatile %struct.ScmObj* %thunk40222, %struct.ScmObj** %stackaddr$prim47423, align 8
%stackaddr$prim47424 = alloca %struct.ScmObj*, align 8
%anf_45bind40339 = call %struct.ScmObj* @prim_vector_63(%struct.ScmObj* %thunk40222)
store volatile %struct.ScmObj* %anf_45bind40339, %struct.ScmObj** %stackaddr$prim47424, align 8
%truthy$cmp47425 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40339)
%cmp$cmp47425 = icmp eq i64 %truthy$cmp47425, 1
br i1 %cmp$cmp47425, label %truebranch$cmp47425, label %falsebranch$cmp47425
truebranch$cmp47425:
%stackaddr$prim47426 = alloca %struct.ScmObj*, align 8
%anf_45bind40340 = call %struct.ScmObj* @prim_vector_45length(%struct.ScmObj* %thunk40222)
store volatile %struct.ScmObj* %anf_45bind40340, %struct.ScmObj** %stackaddr$prim47426, align 8
%ae43905 = call %struct.ScmObj* @const_init_int(i64 3)
%stackaddr$prim47427 = alloca %struct.ScmObj*, align 8
%anf_45bind40341 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind40340, %struct.ScmObj* %ae43905)
store volatile %struct.ScmObj* %anf_45bind40341, %struct.ScmObj** %stackaddr$prim47427, align 8
%truthy$cmp47428 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40341)
%cmp$cmp47428 = icmp eq i64 %truthy$cmp47428, 1
br i1 %cmp$cmp47428, label %truebranch$cmp47428, label %falsebranch$cmp47428
truebranch$cmp47428:
%ae43908 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47429 = alloca %struct.ScmObj*, align 8
%anf_45bind40342 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk40222, %struct.ScmObj* %ae43908)
store volatile %struct.ScmObj* %anf_45bind40342, %struct.ScmObj** %stackaddr$prim47429, align 8
%ae43910 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @global$sym$ae4391047430, i32 0, i32 0))
%stackaddr$prim47431 = alloca %struct.ScmObj*, align 8
%cpsprim40393 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind40342, %struct.ScmObj* %ae43910)
store volatile %struct.ScmObj* %cpsprim40393, %struct.ScmObj** %stackaddr$prim47431, align 8
%ae43912 = call %struct.ScmObj* @const_init_int(i64 0)
%args46503$k40392$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47432 = alloca %struct.ScmObj*, align 8
%args46503$k40392$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40393, %struct.ScmObj* %args46503$k40392$0)
store volatile %struct.ScmObj* %args46503$k40392$1, %struct.ScmObj** %stackaddr$prim47432, align 8
%stackaddr$prim47433 = alloca %struct.ScmObj*, align 8
%args46503$k40392$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43912, %struct.ScmObj* %args46503$k40392$1)
store volatile %struct.ScmObj* %args46503$k40392$2, %struct.ScmObj** %stackaddr$prim47433, align 8
%clofunc47434 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40392)
musttail call tailcc void %clofunc47434(%struct.ScmObj* %k40392, %struct.ScmObj* %args46503$k40392$2)
ret void
falsebranch$cmp47428:
%ae43930 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43931 = call %struct.ScmObj* @const_init_false()
%args46504$k40392$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47435 = alloca %struct.ScmObj*, align 8
%args46504$k40392$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43931, %struct.ScmObj* %args46504$k40392$0)
store volatile %struct.ScmObj* %args46504$k40392$1, %struct.ScmObj** %stackaddr$prim47435, align 8
%stackaddr$prim47436 = alloca %struct.ScmObj*, align 8
%args46504$k40392$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43930, %struct.ScmObj* %args46504$k40392$1)
store volatile %struct.ScmObj* %args46504$k40392$2, %struct.ScmObj** %stackaddr$prim47436, align 8
%clofunc47437 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40392)
musttail call tailcc void %clofunc47437(%struct.ScmObj* %k40392, %struct.ScmObj* %args46504$k40392$2)
ret void
falsebranch$cmp47425:
%ae43952 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43953 = call %struct.ScmObj* @const_init_false()
%args46505$k40392$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47438 = alloca %struct.ScmObj*, align 8
%args46505$k40392$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43953, %struct.ScmObj* %args46505$k40392$0)
store volatile %struct.ScmObj* %args46505$k40392$1, %struct.ScmObj** %stackaddr$prim47438, align 8
%stackaddr$prim47439 = alloca %struct.ScmObj*, align 8
%args46505$k40392$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43952, %struct.ScmObj* %args46505$k40392$1)
store volatile %struct.ScmObj* %args46505$k40392$2, %struct.ScmObj** %stackaddr$prim47439, align 8
%clofunc47440 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40392)
musttail call tailcc void %clofunc47440(%struct.ScmObj* %k40392, %struct.ScmObj* %args46505$k40392$2)
ret void
}

define tailcc void @proc_clo$ae43874(%struct.ScmObj* %env$ae43874,%struct.ScmObj* %current_45args46507) {
%stackaddr$prim47441 = alloca %struct.ScmObj*, align 8
%k40394 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46507)
store volatile %struct.ScmObj* %k40394, %struct.ScmObj** %stackaddr$prim47441, align 8
%stackaddr$prim47442 = alloca %struct.ScmObj*, align 8
%current_45args46508 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46507)
store volatile %struct.ScmObj* %current_45args46508, %struct.ScmObj** %stackaddr$prim47442, align 8
%stackaddr$prim47443 = alloca %struct.ScmObj*, align 8
%x40161 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46508)
store volatile %struct.ScmObj* %x40161, %struct.ScmObj** %stackaddr$prim47443, align 8
%stackaddr$prim47444 = alloca %struct.ScmObj*, align 8
%anf_45bind40336 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40161)
store volatile %struct.ScmObj* %anf_45bind40336, %struct.ScmObj** %stackaddr$prim47444, align 8
%stackaddr$prim47445 = alloca %struct.ScmObj*, align 8
%anf_45bind40337 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40336)
store volatile %struct.ScmObj* %anf_45bind40337, %struct.ScmObj** %stackaddr$prim47445, align 8
%stackaddr$prim47446 = alloca %struct.ScmObj*, align 8
%anf_45bind40338 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40337)
store volatile %struct.ScmObj* %anf_45bind40338, %struct.ScmObj** %stackaddr$prim47446, align 8
%stackaddr$prim47447 = alloca %struct.ScmObj*, align 8
%cpsprim40395 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40338)
store volatile %struct.ScmObj* %cpsprim40395, %struct.ScmObj** %stackaddr$prim47447, align 8
%ae43880 = call %struct.ScmObj* @const_init_int(i64 0)
%args46510$k40394$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47448 = alloca %struct.ScmObj*, align 8
%args46510$k40394$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40395, %struct.ScmObj* %args46510$k40394$0)
store volatile %struct.ScmObj* %args46510$k40394$1, %struct.ScmObj** %stackaddr$prim47448, align 8
%stackaddr$prim47449 = alloca %struct.ScmObj*, align 8
%args46510$k40394$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43880, %struct.ScmObj* %args46510$k40394$1)
store volatile %struct.ScmObj* %args46510$k40394$2, %struct.ScmObj** %stackaddr$prim47449, align 8
%clofunc47450 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40394)
musttail call tailcc void %clofunc47450(%struct.ScmObj* %k40394, %struct.ScmObj* %args46510$k40394$2)
ret void
}

define tailcc void @proc_clo$ae43850(%struct.ScmObj* %env$ae43850,%struct.ScmObj* %current_45args46512) {
%stackaddr$prim47451 = alloca %struct.ScmObj*, align 8
%k40396 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46512)
store volatile %struct.ScmObj* %k40396, %struct.ScmObj** %stackaddr$prim47451, align 8
%stackaddr$prim47452 = alloca %struct.ScmObj*, align 8
%current_45args46513 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46512)
store volatile %struct.ScmObj* %current_45args46513, %struct.ScmObj** %stackaddr$prim47452, align 8
%stackaddr$prim47453 = alloca %struct.ScmObj*, align 8
%x40163 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46513)
store volatile %struct.ScmObj* %x40163, %struct.ScmObj** %stackaddr$prim47453, align 8
%stackaddr$prim47454 = alloca %struct.ScmObj*, align 8
%anf_45bind40334 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40163)
store volatile %struct.ScmObj* %anf_45bind40334, %struct.ScmObj** %stackaddr$prim47454, align 8
%stackaddr$prim47455 = alloca %struct.ScmObj*, align 8
%anf_45bind40335 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40334)
store volatile %struct.ScmObj* %anf_45bind40335, %struct.ScmObj** %stackaddr$prim47455, align 8
%stackaddr$prim47456 = alloca %struct.ScmObj*, align 8
%cpsprim40397 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40335)
store volatile %struct.ScmObj* %cpsprim40397, %struct.ScmObj** %stackaddr$prim47456, align 8
%ae43855 = call %struct.ScmObj* @const_init_int(i64 0)
%args46515$k40396$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47457 = alloca %struct.ScmObj*, align 8
%args46515$k40396$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40397, %struct.ScmObj* %args46515$k40396$0)
store volatile %struct.ScmObj* %args46515$k40396$1, %struct.ScmObj** %stackaddr$prim47457, align 8
%stackaddr$prim47458 = alloca %struct.ScmObj*, align 8
%args46515$k40396$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43855, %struct.ScmObj* %args46515$k40396$1)
store volatile %struct.ScmObj* %args46515$k40396$2, %struct.ScmObj** %stackaddr$prim47458, align 8
%clofunc47459 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40396)
musttail call tailcc void %clofunc47459(%struct.ScmObj* %k40396, %struct.ScmObj* %args46515$k40396$2)
ret void
}

define tailcc void @proc_clo$ae43828(%struct.ScmObj* %env$ae43828,%struct.ScmObj* %current_45args46517) {
%stackaddr$prim47460 = alloca %struct.ScmObj*, align 8
%k40398 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46517)
store volatile %struct.ScmObj* %k40398, %struct.ScmObj** %stackaddr$prim47460, align 8
%stackaddr$prim47461 = alloca %struct.ScmObj*, align 8
%current_45args46518 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46517)
store volatile %struct.ScmObj* %current_45args46518, %struct.ScmObj** %stackaddr$prim47461, align 8
%stackaddr$prim47462 = alloca %struct.ScmObj*, align 8
%x40165 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46518)
store volatile %struct.ScmObj* %x40165, %struct.ScmObj** %stackaddr$prim47462, align 8
%stackaddr$prim47463 = alloca %struct.ScmObj*, align 8
%anf_45bind40333 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40165)
store volatile %struct.ScmObj* %anf_45bind40333, %struct.ScmObj** %stackaddr$prim47463, align 8
%stackaddr$prim47464 = alloca %struct.ScmObj*, align 8
%cpsprim40399 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40333)
store volatile %struct.ScmObj* %cpsprim40399, %struct.ScmObj** %stackaddr$prim47464, align 8
%ae43832 = call %struct.ScmObj* @const_init_int(i64 0)
%args46520$k40398$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47465 = alloca %struct.ScmObj*, align 8
%args46520$k40398$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40399, %struct.ScmObj* %args46520$k40398$0)
store volatile %struct.ScmObj* %args46520$k40398$1, %struct.ScmObj** %stackaddr$prim47465, align 8
%stackaddr$prim47466 = alloca %struct.ScmObj*, align 8
%args46520$k40398$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43832, %struct.ScmObj* %args46520$k40398$1)
store volatile %struct.ScmObj* %args46520$k40398$2, %struct.ScmObj** %stackaddr$prim47466, align 8
%clofunc47467 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40398)
musttail call tailcc void %clofunc47467(%struct.ScmObj* %k40398, %struct.ScmObj* %args46520$k40398$2)
ret void
}

define tailcc void @proc_clo$ae43808(%struct.ScmObj* %env$ae43808,%struct.ScmObj* %current_45args46522) {
%stackaddr$prim47468 = alloca %struct.ScmObj*, align 8
%k40400 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46522)
store volatile %struct.ScmObj* %k40400, %struct.ScmObj** %stackaddr$prim47468, align 8
%stackaddr$prim47469 = alloca %struct.ScmObj*, align 8
%current_45args46523 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46522)
store volatile %struct.ScmObj* %current_45args46523, %struct.ScmObj** %stackaddr$prim47469, align 8
%stackaddr$prim47470 = alloca %struct.ScmObj*, align 8
%x40167 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46523)
store volatile %struct.ScmObj* %x40167, %struct.ScmObj** %stackaddr$prim47470, align 8
%stackaddr$prim47471 = alloca %struct.ScmObj*, align 8
%cpsprim40401 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40167)
store volatile %struct.ScmObj* %cpsprim40401, %struct.ScmObj** %stackaddr$prim47471, align 8
%ae43811 = call %struct.ScmObj* @const_init_int(i64 0)
%args46525$k40400$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47472 = alloca %struct.ScmObj*, align 8
%args46525$k40400$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40401, %struct.ScmObj* %args46525$k40400$0)
store volatile %struct.ScmObj* %args46525$k40400$1, %struct.ScmObj** %stackaddr$prim47472, align 8
%stackaddr$prim47473 = alloca %struct.ScmObj*, align 8
%args46525$k40400$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43811, %struct.ScmObj* %args46525$k40400$1)
store volatile %struct.ScmObj* %args46525$k40400$2, %struct.ScmObj** %stackaddr$prim47473, align 8
%clofunc47474 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40400)
musttail call tailcc void %clofunc47474(%struct.ScmObj* %k40400, %struct.ScmObj* %args46525$k40400$2)
ret void
}

define tailcc void @proc_clo$ae43710(%struct.ScmObj* %env$ae43710,%struct.ScmObj* %args4016940402) {
%stackaddr$env-ref47475 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43710, i64 0)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref47475
%stackaddr$prim47476 = alloca %struct.ScmObj*, align 8
%k40403 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4016940402)
store volatile %struct.ScmObj* %k40403, %struct.ScmObj** %stackaddr$prim47476, align 8
%stackaddr$prim47477 = alloca %struct.ScmObj*, align 8
%args40169 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4016940402)
store volatile %struct.ScmObj* %args40169, %struct.ScmObj** %stackaddr$prim47477, align 8
%stackaddr$prim47478 = alloca %struct.ScmObj*, align 8
%anf_45bind40327 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args40169)
store volatile %struct.ScmObj* %anf_45bind40327, %struct.ScmObj** %stackaddr$prim47478, align 8
%truthy$cmp47479 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40327)
%cmp$cmp47479 = icmp eq i64 %truthy$cmp47479, 1
br i1 %cmp$cmp47479, label %truebranch$cmp47479, label %falsebranch$cmp47479
truebranch$cmp47479:
%ae43716 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43717 = call %struct.ScmObj* @const_init_int(i64 1)
%args46527$k40403$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47480 = alloca %struct.ScmObj*, align 8
%args46527$k40403$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43717, %struct.ScmObj* %args46527$k40403$0)
store volatile %struct.ScmObj* %args46527$k40403$1, %struct.ScmObj** %stackaddr$prim47480, align 8
%stackaddr$prim47481 = alloca %struct.ScmObj*, align 8
%args46527$k40403$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43716, %struct.ScmObj* %args46527$k40403$1)
store volatile %struct.ScmObj* %args46527$k40403$2, %struct.ScmObj** %stackaddr$prim47481, align 8
%clofunc47482 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40403)
musttail call tailcc void %clofunc47482(%struct.ScmObj* %k40403, %struct.ScmObj* %args46527$k40403$2)
ret void
falsebranch$cmp47479:
%stackaddr$prim47483 = alloca %struct.ScmObj*, align 8
%anf_45bind40328 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40169)
store volatile %struct.ScmObj* %anf_45bind40328, %struct.ScmObj** %stackaddr$prim47483, align 8
%stackaddr$prim47484 = alloca %struct.ScmObj*, align 8
%anf_45bind40329 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40328)
store volatile %struct.ScmObj* %anf_45bind40329, %struct.ScmObj** %stackaddr$prim47484, align 8
%truthy$cmp47485 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40329)
%cmp$cmp47485 = icmp eq i64 %truthy$cmp47485, 1
br i1 %cmp$cmp47485, label %truebranch$cmp47485, label %falsebranch$cmp47485
truebranch$cmp47485:
%stackaddr$prim47486 = alloca %struct.ScmObj*, align 8
%cpsprim40404 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40169)
store volatile %struct.ScmObj* %cpsprim40404, %struct.ScmObj** %stackaddr$prim47486, align 8
%ae43729 = call %struct.ScmObj* @const_init_int(i64 0)
%args46528$k40403$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47487 = alloca %struct.ScmObj*, align 8
%args46528$k40403$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40404, %struct.ScmObj* %args46528$k40403$0)
store volatile %struct.ScmObj* %args46528$k40403$1, %struct.ScmObj** %stackaddr$prim47487, align 8
%stackaddr$prim47488 = alloca %struct.ScmObj*, align 8
%args46528$k40403$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43729, %struct.ScmObj* %args46528$k40403$1)
store volatile %struct.ScmObj* %args46528$k40403$2, %struct.ScmObj** %stackaddr$prim47488, align 8
%clofunc47489 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40403)
musttail call tailcc void %clofunc47489(%struct.ScmObj* %k40403, %struct.ScmObj* %args46528$k40403$2)
ret void
falsebranch$cmp47485:
%stackaddr$makeclosure47490 = alloca %struct.ScmObj*, align 8
%fptrToInt47491 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43734 to i64
%ae43734 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47491)
store volatile %struct.ScmObj* %ae43734, %struct.ScmObj** %stackaddr$makeclosure47490, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43734, %struct.ScmObj* %args40169, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43734, %struct.ScmObj* %k40403, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43734, %struct.ScmObj* %_37foldl140108, i64 2)
%ae43735 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47492 = alloca %struct.ScmObj*, align 8
%fptrToInt47493 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43736 to i64
%ae43736 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47493)
store volatile %struct.ScmObj* %ae43736, %struct.ScmObj** %stackaddr$makeclosure47492, align 8
%args46538$ae43734$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47494 = alloca %struct.ScmObj*, align 8
%args46538$ae43734$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43736, %struct.ScmObj* %args46538$ae43734$0)
store volatile %struct.ScmObj* %args46538$ae43734$1, %struct.ScmObj** %stackaddr$prim47494, align 8
%stackaddr$prim47495 = alloca %struct.ScmObj*, align 8
%args46538$ae43734$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43735, %struct.ScmObj* %args46538$ae43734$1)
store volatile %struct.ScmObj* %args46538$ae43734$2, %struct.ScmObj** %stackaddr$prim47495, align 8
%clofunc47496 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43734)
musttail call tailcc void %clofunc47496(%struct.ScmObj* %ae43734, %struct.ScmObj* %args46538$ae43734$2)
ret void
}

define tailcc void @proc_clo$ae43734(%struct.ScmObj* %env$ae43734,%struct.ScmObj* %current_45args46529) {
%stackaddr$env-ref47497 = alloca %struct.ScmObj*, align 8
%args40169 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43734, i64 0)
store %struct.ScmObj* %args40169, %struct.ScmObj** %stackaddr$env-ref47497
%stackaddr$env-ref47498 = alloca %struct.ScmObj*, align 8
%k40403 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43734, i64 1)
store %struct.ScmObj* %k40403, %struct.ScmObj** %stackaddr$env-ref47498
%stackaddr$env-ref47499 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43734, i64 2)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref47499
%stackaddr$prim47500 = alloca %struct.ScmObj*, align 8
%_95k40405 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46529)
store volatile %struct.ScmObj* %_95k40405, %struct.ScmObj** %stackaddr$prim47500, align 8
%stackaddr$prim47501 = alloca %struct.ScmObj*, align 8
%current_45args46530 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46529)
store volatile %struct.ScmObj* %current_45args46530, %struct.ScmObj** %stackaddr$prim47501, align 8
%stackaddr$prim47502 = alloca %struct.ScmObj*, align 8
%anf_45bind40330 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46530)
store volatile %struct.ScmObj* %anf_45bind40330, %struct.ScmObj** %stackaddr$prim47502, align 8
%stackaddr$prim47503 = alloca %struct.ScmObj*, align 8
%anf_45bind40331 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40169)
store volatile %struct.ScmObj* %anf_45bind40331, %struct.ScmObj** %stackaddr$prim47503, align 8
%stackaddr$prim47504 = alloca %struct.ScmObj*, align 8
%anf_45bind40332 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40169)
store volatile %struct.ScmObj* %anf_45bind40332, %struct.ScmObj** %stackaddr$prim47504, align 8
%args46532$_37foldl140108$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47505 = alloca %struct.ScmObj*, align 8
%args46532$_37foldl140108$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40332, %struct.ScmObj* %args46532$_37foldl140108$0)
store volatile %struct.ScmObj* %args46532$_37foldl140108$1, %struct.ScmObj** %stackaddr$prim47505, align 8
%stackaddr$prim47506 = alloca %struct.ScmObj*, align 8
%args46532$_37foldl140108$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40331, %struct.ScmObj* %args46532$_37foldl140108$1)
store volatile %struct.ScmObj* %args46532$_37foldl140108$2, %struct.ScmObj** %stackaddr$prim47506, align 8
%stackaddr$prim47507 = alloca %struct.ScmObj*, align 8
%args46532$_37foldl140108$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40330, %struct.ScmObj* %args46532$_37foldl140108$2)
store volatile %struct.ScmObj* %args46532$_37foldl140108$3, %struct.ScmObj** %stackaddr$prim47507, align 8
%stackaddr$prim47508 = alloca %struct.ScmObj*, align 8
%args46532$_37foldl140108$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40403, %struct.ScmObj* %args46532$_37foldl140108$3)
store volatile %struct.ScmObj* %args46532$_37foldl140108$4, %struct.ScmObj** %stackaddr$prim47508, align 8
%clofunc47509 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140108)
musttail call tailcc void %clofunc47509(%struct.ScmObj* %_37foldl140108, %struct.ScmObj* %args46532$_37foldl140108$4)
ret void
}

define tailcc void @proc_clo$ae43736(%struct.ScmObj* %env$ae43736,%struct.ScmObj* %current_45args46533) {
%stackaddr$prim47510 = alloca %struct.ScmObj*, align 8
%k40406 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46533)
store volatile %struct.ScmObj* %k40406, %struct.ScmObj** %stackaddr$prim47510, align 8
%stackaddr$prim47511 = alloca %struct.ScmObj*, align 8
%current_45args46534 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46533)
store volatile %struct.ScmObj* %current_45args46534, %struct.ScmObj** %stackaddr$prim47511, align 8
%stackaddr$prim47512 = alloca %struct.ScmObj*, align 8
%n40171 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46534)
store volatile %struct.ScmObj* %n40171, %struct.ScmObj** %stackaddr$prim47512, align 8
%stackaddr$prim47513 = alloca %struct.ScmObj*, align 8
%current_45args46535 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46534)
store volatile %struct.ScmObj* %current_45args46535, %struct.ScmObj** %stackaddr$prim47513, align 8
%stackaddr$prim47514 = alloca %struct.ScmObj*, align 8
%v40170 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46535)
store volatile %struct.ScmObj* %v40170, %struct.ScmObj** %stackaddr$prim47514, align 8
%stackaddr$prim47515 = alloca %struct.ScmObj*, align 8
%cpsprim40407 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v40170, %struct.ScmObj* %n40171)
store volatile %struct.ScmObj* %cpsprim40407, %struct.ScmObj** %stackaddr$prim47515, align 8
%ae43740 = call %struct.ScmObj* @const_init_int(i64 0)
%args46537$k40406$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47516 = alloca %struct.ScmObj*, align 8
%args46537$k40406$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40407, %struct.ScmObj* %args46537$k40406$0)
store volatile %struct.ScmObj* %args46537$k40406$1, %struct.ScmObj** %stackaddr$prim47516, align 8
%stackaddr$prim47517 = alloca %struct.ScmObj*, align 8
%args46537$k40406$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43740, %struct.ScmObj* %args46537$k40406$1)
store volatile %struct.ScmObj* %args46537$k40406$2, %struct.ScmObj** %stackaddr$prim47517, align 8
%clofunc47518 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40406)
musttail call tailcc void %clofunc47518(%struct.ScmObj* %k40406, %struct.ScmObj* %args46537$k40406$2)
ret void
}

define tailcc void @proc_clo$ae43306(%struct.ScmObj* %env$ae43306,%struct.ScmObj* %current_45args46540) {
%stackaddr$prim47519 = alloca %struct.ScmObj*, align 8
%k40408 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46540)
store volatile %struct.ScmObj* %k40408, %struct.ScmObj** %stackaddr$prim47519, align 8
%stackaddr$prim47520 = alloca %struct.ScmObj*, align 8
%current_45args46541 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46540)
store volatile %struct.ScmObj* %current_45args46541, %struct.ScmObj** %stackaddr$prim47520, align 8
%stackaddr$prim47521 = alloca %struct.ScmObj*, align 8
%v40174 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46541)
store volatile %struct.ScmObj* %v40174, %struct.ScmObj** %stackaddr$prim47521, align 8
%stackaddr$prim47522 = alloca %struct.ScmObj*, align 8
%current_45args46542 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46541)
store volatile %struct.ScmObj* %current_45args46542, %struct.ScmObj** %stackaddr$prim47522, align 8
%stackaddr$prim47523 = alloca %struct.ScmObj*, align 8
%lst40173 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46542)
store volatile %struct.ScmObj* %lst40173, %struct.ScmObj** %stackaddr$prim47523, align 8
%ae43307 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim47524 = alloca %struct.ScmObj*, align 8
%lst40175 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae43307, %struct.ScmObj* %lst40173)
store volatile %struct.ScmObj* %lst40175, %struct.ScmObj** %stackaddr$prim47524, align 8
%stackaddr$makeclosure47525 = alloca %struct.ScmObj*, align 8
%fptrToInt47526 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43309 to i64
%ae43309 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47526)
store volatile %struct.ScmObj* %ae43309, %struct.ScmObj** %stackaddr$makeclosure47525, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43309, %struct.ScmObj* %k40408, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43309, %struct.ScmObj* %lst40175, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43309, %struct.ScmObj* %v40174, i64 2)
%ae43310 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47527 = alloca %struct.ScmObj*, align 8
%fptrToInt47528 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43311 to i64
%ae43311 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47528)
store volatile %struct.ScmObj* %ae43311, %struct.ScmObj** %stackaddr$makeclosure47527, align 8
%args46564$ae43309$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47529 = alloca %struct.ScmObj*, align 8
%args46564$ae43309$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43311, %struct.ScmObj* %args46564$ae43309$0)
store volatile %struct.ScmObj* %args46564$ae43309$1, %struct.ScmObj** %stackaddr$prim47529, align 8
%stackaddr$prim47530 = alloca %struct.ScmObj*, align 8
%args46564$ae43309$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43310, %struct.ScmObj* %args46564$ae43309$1)
store volatile %struct.ScmObj* %args46564$ae43309$2, %struct.ScmObj** %stackaddr$prim47530, align 8
%clofunc47531 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43309)
musttail call tailcc void %clofunc47531(%struct.ScmObj* %ae43309, %struct.ScmObj* %args46564$ae43309$2)
ret void
}

define tailcc void @proc_clo$ae43309(%struct.ScmObj* %env$ae43309,%struct.ScmObj* %current_45args46544) {
%stackaddr$env-ref47532 = alloca %struct.ScmObj*, align 8
%k40408 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43309, i64 0)
store %struct.ScmObj* %k40408, %struct.ScmObj** %stackaddr$env-ref47532
%stackaddr$env-ref47533 = alloca %struct.ScmObj*, align 8
%lst40175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43309, i64 1)
store %struct.ScmObj* %lst40175, %struct.ScmObj** %stackaddr$env-ref47533
%stackaddr$env-ref47534 = alloca %struct.ScmObj*, align 8
%v40174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43309, i64 2)
store %struct.ScmObj* %v40174, %struct.ScmObj** %stackaddr$env-ref47534
%stackaddr$prim47535 = alloca %struct.ScmObj*, align 8
%_95k40409 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46544)
store volatile %struct.ScmObj* %_95k40409, %struct.ScmObj** %stackaddr$prim47535, align 8
%stackaddr$prim47536 = alloca %struct.ScmObj*, align 8
%current_45args46545 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46544)
store volatile %struct.ScmObj* %current_45args46545, %struct.ScmObj** %stackaddr$prim47536, align 8
%stackaddr$prim47537 = alloca %struct.ScmObj*, align 8
%anf_45bind40319 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46545)
store volatile %struct.ScmObj* %anf_45bind40319, %struct.ScmObj** %stackaddr$prim47537, align 8
%stackaddr$makeclosure47538 = alloca %struct.ScmObj*, align 8
%fptrToInt47539 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43325 to i64
%ae43325 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47539)
store volatile %struct.ScmObj* %ae43325, %struct.ScmObj** %stackaddr$makeclosure47538, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43325, %struct.ScmObj* %k40408, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43325, %struct.ScmObj* %lst40175, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43325, %struct.ScmObj* %v40174, i64 2)
%stackaddr$makeclosure47540 = alloca %struct.ScmObj*, align 8
%fptrToInt47541 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43326 to i64
%ae43326 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47541)
store volatile %struct.ScmObj* %ae43326, %struct.ScmObj** %stackaddr$makeclosure47540, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43326, %struct.ScmObj* %k40408, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43326, %struct.ScmObj* %lst40175, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43326, %struct.ScmObj* %v40174, i64 2)
%args46559$anf_45bind40319$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47542 = alloca %struct.ScmObj*, align 8
%args46559$anf_45bind40319$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43326, %struct.ScmObj* %args46559$anf_45bind40319$0)
store volatile %struct.ScmObj* %args46559$anf_45bind40319$1, %struct.ScmObj** %stackaddr$prim47542, align 8
%stackaddr$prim47543 = alloca %struct.ScmObj*, align 8
%args46559$anf_45bind40319$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43325, %struct.ScmObj* %args46559$anf_45bind40319$1)
store volatile %struct.ScmObj* %args46559$anf_45bind40319$2, %struct.ScmObj** %stackaddr$prim47543, align 8
%clofunc47544 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40319)
musttail call tailcc void %clofunc47544(%struct.ScmObj* %anf_45bind40319, %struct.ScmObj* %args46559$anf_45bind40319$2)
ret void
}

define tailcc void @proc_clo$ae43325(%struct.ScmObj* %env$ae43325,%struct.ScmObj* %current_45args46547) {
%stackaddr$env-ref47545 = alloca %struct.ScmObj*, align 8
%k40408 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43325, i64 0)
store %struct.ScmObj* %k40408, %struct.ScmObj** %stackaddr$env-ref47545
%stackaddr$env-ref47546 = alloca %struct.ScmObj*, align 8
%lst40175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43325, i64 1)
store %struct.ScmObj* %lst40175, %struct.ScmObj** %stackaddr$env-ref47546
%stackaddr$env-ref47547 = alloca %struct.ScmObj*, align 8
%v40174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43325, i64 2)
store %struct.ScmObj* %v40174, %struct.ScmObj** %stackaddr$env-ref47547
%stackaddr$prim47548 = alloca %struct.ScmObj*, align 8
%_95k40410 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46547)
store volatile %struct.ScmObj* %_95k40410, %struct.ScmObj** %stackaddr$prim47548, align 8
%stackaddr$prim47549 = alloca %struct.ScmObj*, align 8
%current_45args46548 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46547)
store volatile %struct.ScmObj* %current_45args46548, %struct.ScmObj** %stackaddr$prim47549, align 8
%stackaddr$prim47550 = alloca %struct.ScmObj*, align 8
%cc40176 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46548)
store volatile %struct.ScmObj* %cc40176, %struct.ScmObj** %stackaddr$prim47550, align 8
%ae43434 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47551 = alloca %struct.ScmObj*, align 8
%anf_45bind40320 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40175, %struct.ScmObj* %ae43434)
store volatile %struct.ScmObj* %anf_45bind40320, %struct.ScmObj** %stackaddr$prim47551, align 8
%stackaddr$prim47552 = alloca %struct.ScmObj*, align 8
%anf_45bind40321 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40320)
store volatile %struct.ScmObj* %anf_45bind40321, %struct.ScmObj** %stackaddr$prim47552, align 8
%truthy$cmp47553 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40321)
%cmp$cmp47553 = icmp eq i64 %truthy$cmp47553, 1
br i1 %cmp$cmp47553, label %truebranch$cmp47553, label %falsebranch$cmp47553
truebranch$cmp47553:
%ae43438 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43439 = call %struct.ScmObj* @const_init_false()
%args46550$k40408$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47554 = alloca %struct.ScmObj*, align 8
%args46550$k40408$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43439, %struct.ScmObj* %args46550$k40408$0)
store volatile %struct.ScmObj* %args46550$k40408$1, %struct.ScmObj** %stackaddr$prim47554, align 8
%stackaddr$prim47555 = alloca %struct.ScmObj*, align 8
%args46550$k40408$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43438, %struct.ScmObj* %args46550$k40408$1)
store volatile %struct.ScmObj* %args46550$k40408$2, %struct.ScmObj** %stackaddr$prim47555, align 8
%clofunc47556 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40408)
musttail call tailcc void %clofunc47556(%struct.ScmObj* %k40408, %struct.ScmObj* %args46550$k40408$2)
ret void
falsebranch$cmp47553:
%ae43447 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47557 = alloca %struct.ScmObj*, align 8
%anf_45bind40322 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40175, %struct.ScmObj* %ae43447)
store volatile %struct.ScmObj* %anf_45bind40322, %struct.ScmObj** %stackaddr$prim47557, align 8
%stackaddr$prim47558 = alloca %struct.ScmObj*, align 8
%anf_45bind40323 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40322)
store volatile %struct.ScmObj* %anf_45bind40323, %struct.ScmObj** %stackaddr$prim47558, align 8
%stackaddr$prim47559 = alloca %struct.ScmObj*, align 8
%anf_45bind40324 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind40323, %struct.ScmObj* %v40174)
store volatile %struct.ScmObj* %anf_45bind40324, %struct.ScmObj** %stackaddr$prim47559, align 8
%truthy$cmp47560 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40324)
%cmp$cmp47560 = icmp eq i64 %truthy$cmp47560, 1
br i1 %cmp$cmp47560, label %truebranch$cmp47560, label %falsebranch$cmp47560
truebranch$cmp47560:
%ae43453 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47561 = alloca %struct.ScmObj*, align 8
%cpsprim40411 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40175, %struct.ScmObj* %ae43453)
store volatile %struct.ScmObj* %cpsprim40411, %struct.ScmObj** %stackaddr$prim47561, align 8
%ae43455 = call %struct.ScmObj* @const_init_int(i64 0)
%args46551$k40408$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47562 = alloca %struct.ScmObj*, align 8
%args46551$k40408$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40411, %struct.ScmObj* %args46551$k40408$0)
store volatile %struct.ScmObj* %args46551$k40408$1, %struct.ScmObj** %stackaddr$prim47562, align 8
%stackaddr$prim47563 = alloca %struct.ScmObj*, align 8
%args46551$k40408$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43455, %struct.ScmObj* %args46551$k40408$1)
store volatile %struct.ScmObj* %args46551$k40408$2, %struct.ScmObj** %stackaddr$prim47563, align 8
%clofunc47564 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40408)
musttail call tailcc void %clofunc47564(%struct.ScmObj* %k40408, %struct.ScmObj* %args46551$k40408$2)
ret void
falsebranch$cmp47560:
%ae43466 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47565 = alloca %struct.ScmObj*, align 8
%anf_45bind40325 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40175, %struct.ScmObj* %ae43466)
store volatile %struct.ScmObj* %anf_45bind40325, %struct.ScmObj** %stackaddr$prim47565, align 8
%stackaddr$prim47566 = alloca %struct.ScmObj*, align 8
%anf_45bind40326 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40325)
store volatile %struct.ScmObj* %anf_45bind40326, %struct.ScmObj** %stackaddr$prim47566, align 8
%ae43469 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47567 = alloca %struct.ScmObj*, align 8
%_95040178 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40175, %struct.ScmObj* %ae43469, %struct.ScmObj* %anf_45bind40326)
store volatile %struct.ScmObj* %_95040178, %struct.ScmObj** %stackaddr$prim47567, align 8
%args46552$cc40176$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47568 = alloca %struct.ScmObj*, align 8
%args46552$cc40176$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40176, %struct.ScmObj* %args46552$cc40176$0)
store volatile %struct.ScmObj* %args46552$cc40176$1, %struct.ScmObj** %stackaddr$prim47568, align 8
%stackaddr$prim47569 = alloca %struct.ScmObj*, align 8
%args46552$cc40176$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40408, %struct.ScmObj* %args46552$cc40176$1)
store volatile %struct.ScmObj* %args46552$cc40176$2, %struct.ScmObj** %stackaddr$prim47569, align 8
%clofunc47570 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40176)
musttail call tailcc void %clofunc47570(%struct.ScmObj* %cc40176, %struct.ScmObj* %args46552$cc40176$2)
ret void
}

define tailcc void @proc_clo$ae43326(%struct.ScmObj* %env$ae43326,%struct.ScmObj* %current_45args46553) {
%stackaddr$env-ref47571 = alloca %struct.ScmObj*, align 8
%k40408 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43326, i64 0)
store %struct.ScmObj* %k40408, %struct.ScmObj** %stackaddr$env-ref47571
%stackaddr$env-ref47572 = alloca %struct.ScmObj*, align 8
%lst40175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43326, i64 1)
store %struct.ScmObj* %lst40175, %struct.ScmObj** %stackaddr$env-ref47572
%stackaddr$env-ref47573 = alloca %struct.ScmObj*, align 8
%v40174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43326, i64 2)
store %struct.ScmObj* %v40174, %struct.ScmObj** %stackaddr$env-ref47573
%stackaddr$prim47574 = alloca %struct.ScmObj*, align 8
%_95k40410 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46553)
store volatile %struct.ScmObj* %_95k40410, %struct.ScmObj** %stackaddr$prim47574, align 8
%stackaddr$prim47575 = alloca %struct.ScmObj*, align 8
%current_45args46554 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46553)
store volatile %struct.ScmObj* %current_45args46554, %struct.ScmObj** %stackaddr$prim47575, align 8
%stackaddr$prim47576 = alloca %struct.ScmObj*, align 8
%cc40176 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46554)
store volatile %struct.ScmObj* %cc40176, %struct.ScmObj** %stackaddr$prim47576, align 8
%ae43328 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47577 = alloca %struct.ScmObj*, align 8
%anf_45bind40320 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40175, %struct.ScmObj* %ae43328)
store volatile %struct.ScmObj* %anf_45bind40320, %struct.ScmObj** %stackaddr$prim47577, align 8
%stackaddr$prim47578 = alloca %struct.ScmObj*, align 8
%anf_45bind40321 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40320)
store volatile %struct.ScmObj* %anf_45bind40321, %struct.ScmObj** %stackaddr$prim47578, align 8
%truthy$cmp47579 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40321)
%cmp$cmp47579 = icmp eq i64 %truthy$cmp47579, 1
br i1 %cmp$cmp47579, label %truebranch$cmp47579, label %falsebranch$cmp47579
truebranch$cmp47579:
%ae43332 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43333 = call %struct.ScmObj* @const_init_false()
%args46556$k40408$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47580 = alloca %struct.ScmObj*, align 8
%args46556$k40408$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43333, %struct.ScmObj* %args46556$k40408$0)
store volatile %struct.ScmObj* %args46556$k40408$1, %struct.ScmObj** %stackaddr$prim47580, align 8
%stackaddr$prim47581 = alloca %struct.ScmObj*, align 8
%args46556$k40408$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43332, %struct.ScmObj* %args46556$k40408$1)
store volatile %struct.ScmObj* %args46556$k40408$2, %struct.ScmObj** %stackaddr$prim47581, align 8
%clofunc47582 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40408)
musttail call tailcc void %clofunc47582(%struct.ScmObj* %k40408, %struct.ScmObj* %args46556$k40408$2)
ret void
falsebranch$cmp47579:
%ae43341 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47583 = alloca %struct.ScmObj*, align 8
%anf_45bind40322 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40175, %struct.ScmObj* %ae43341)
store volatile %struct.ScmObj* %anf_45bind40322, %struct.ScmObj** %stackaddr$prim47583, align 8
%stackaddr$prim47584 = alloca %struct.ScmObj*, align 8
%anf_45bind40323 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40322)
store volatile %struct.ScmObj* %anf_45bind40323, %struct.ScmObj** %stackaddr$prim47584, align 8
%stackaddr$prim47585 = alloca %struct.ScmObj*, align 8
%anf_45bind40324 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind40323, %struct.ScmObj* %v40174)
store volatile %struct.ScmObj* %anf_45bind40324, %struct.ScmObj** %stackaddr$prim47585, align 8
%truthy$cmp47586 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40324)
%cmp$cmp47586 = icmp eq i64 %truthy$cmp47586, 1
br i1 %cmp$cmp47586, label %truebranch$cmp47586, label %falsebranch$cmp47586
truebranch$cmp47586:
%ae43347 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47587 = alloca %struct.ScmObj*, align 8
%cpsprim40411 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40175, %struct.ScmObj* %ae43347)
store volatile %struct.ScmObj* %cpsprim40411, %struct.ScmObj** %stackaddr$prim47587, align 8
%ae43349 = call %struct.ScmObj* @const_init_int(i64 0)
%args46557$k40408$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47588 = alloca %struct.ScmObj*, align 8
%args46557$k40408$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40411, %struct.ScmObj* %args46557$k40408$0)
store volatile %struct.ScmObj* %args46557$k40408$1, %struct.ScmObj** %stackaddr$prim47588, align 8
%stackaddr$prim47589 = alloca %struct.ScmObj*, align 8
%args46557$k40408$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43349, %struct.ScmObj* %args46557$k40408$1)
store volatile %struct.ScmObj* %args46557$k40408$2, %struct.ScmObj** %stackaddr$prim47589, align 8
%clofunc47590 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40408)
musttail call tailcc void %clofunc47590(%struct.ScmObj* %k40408, %struct.ScmObj* %args46557$k40408$2)
ret void
falsebranch$cmp47586:
%ae43360 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47591 = alloca %struct.ScmObj*, align 8
%anf_45bind40325 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40175, %struct.ScmObj* %ae43360)
store volatile %struct.ScmObj* %anf_45bind40325, %struct.ScmObj** %stackaddr$prim47591, align 8
%stackaddr$prim47592 = alloca %struct.ScmObj*, align 8
%anf_45bind40326 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40325)
store volatile %struct.ScmObj* %anf_45bind40326, %struct.ScmObj** %stackaddr$prim47592, align 8
%ae43363 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47593 = alloca %struct.ScmObj*, align 8
%_95040178 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40175, %struct.ScmObj* %ae43363, %struct.ScmObj* %anf_45bind40326)
store volatile %struct.ScmObj* %_95040178, %struct.ScmObj** %stackaddr$prim47593, align 8
%args46558$cc40176$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47594 = alloca %struct.ScmObj*, align 8
%args46558$cc40176$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40176, %struct.ScmObj* %args46558$cc40176$0)
store volatile %struct.ScmObj* %args46558$cc40176$1, %struct.ScmObj** %stackaddr$prim47594, align 8
%stackaddr$prim47595 = alloca %struct.ScmObj*, align 8
%args46558$cc40176$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40408, %struct.ScmObj* %args46558$cc40176$1)
store volatile %struct.ScmObj* %args46558$cc40176$2, %struct.ScmObj** %stackaddr$prim47595, align 8
%clofunc47596 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40176)
musttail call tailcc void %clofunc47596(%struct.ScmObj* %cc40176, %struct.ScmObj* %args46558$cc40176$2)
ret void
}

define tailcc void @proc_clo$ae43311(%struct.ScmObj* %env$ae43311,%struct.ScmObj* %current_45args46560) {
%stackaddr$prim47597 = alloca %struct.ScmObj*, align 8
%k40412 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46560)
store volatile %struct.ScmObj* %k40412, %struct.ScmObj** %stackaddr$prim47597, align 8
%stackaddr$prim47598 = alloca %struct.ScmObj*, align 8
%current_45args46561 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46560)
store volatile %struct.ScmObj* %current_45args46561, %struct.ScmObj** %stackaddr$prim47598, align 8
%stackaddr$prim47599 = alloca %struct.ScmObj*, align 8
%u40177 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46561)
store volatile %struct.ScmObj* %u40177, %struct.ScmObj** %stackaddr$prim47599, align 8
%args46563$u40177$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47600 = alloca %struct.ScmObj*, align 8
%args46563$u40177$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u40177, %struct.ScmObj* %args46563$u40177$0)
store volatile %struct.ScmObj* %args46563$u40177$1, %struct.ScmObj** %stackaddr$prim47600, align 8
%stackaddr$prim47601 = alloca %struct.ScmObj*, align 8
%args46563$u40177$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40412, %struct.ScmObj* %args46563$u40177$1)
store volatile %struct.ScmObj* %args46563$u40177$2, %struct.ScmObj** %stackaddr$prim47601, align 8
%clofunc47602 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u40177)
musttail call tailcc void %clofunc47602(%struct.ScmObj* %u40177, %struct.ScmObj* %args46563$u40177$2)
ret void
}

define tailcc void @proc_clo$ae42770(%struct.ScmObj* %env$ae42770,%struct.ScmObj* %current_45args46566) {
%stackaddr$prim47603 = alloca %struct.ScmObj*, align 8
%k40413 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46566)
store volatile %struct.ScmObj* %k40413, %struct.ScmObj** %stackaddr$prim47603, align 8
%stackaddr$prim47604 = alloca %struct.ScmObj*, align 8
%current_45args46567 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46566)
store volatile %struct.ScmObj* %current_45args46567, %struct.ScmObj** %stackaddr$prim47604, align 8
%stackaddr$prim47605 = alloca %struct.ScmObj*, align 8
%lst40181 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46567)
store volatile %struct.ScmObj* %lst40181, %struct.ScmObj** %stackaddr$prim47605, align 8
%stackaddr$prim47606 = alloca %struct.ScmObj*, align 8
%current_45args46568 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46567)
store volatile %struct.ScmObj* %current_45args46568, %struct.ScmObj** %stackaddr$prim47606, align 8
%stackaddr$prim47607 = alloca %struct.ScmObj*, align 8
%n40180 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46568)
store volatile %struct.ScmObj* %n40180, %struct.ScmObj** %stackaddr$prim47607, align 8
%ae42771 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim47608 = alloca %struct.ScmObj*, align 8
%n40183 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42771, %struct.ScmObj* %n40180)
store volatile %struct.ScmObj* %n40183, %struct.ScmObj** %stackaddr$prim47608, align 8
%ae42773 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim47609 = alloca %struct.ScmObj*, align 8
%lst40182 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42773, %struct.ScmObj* %lst40181)
store volatile %struct.ScmObj* %lst40182, %struct.ScmObj** %stackaddr$prim47609, align 8
%stackaddr$makeclosure47610 = alloca %struct.ScmObj*, align 8
%fptrToInt47611 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42775 to i64
%ae42775 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47611)
store volatile %struct.ScmObj* %ae42775, %struct.ScmObj** %stackaddr$makeclosure47610, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42775, %struct.ScmObj* %n40183, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42775, %struct.ScmObj* %lst40182, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42775, %struct.ScmObj* %k40413, i64 2)
%ae42776 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47612 = alloca %struct.ScmObj*, align 8
%fptrToInt47613 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42777 to i64
%ae42777 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47613)
store volatile %struct.ScmObj* %ae42777, %struct.ScmObj** %stackaddr$makeclosure47612, align 8
%args46588$ae42775$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47614 = alloca %struct.ScmObj*, align 8
%args46588$ae42775$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42777, %struct.ScmObj* %args46588$ae42775$0)
store volatile %struct.ScmObj* %args46588$ae42775$1, %struct.ScmObj** %stackaddr$prim47614, align 8
%stackaddr$prim47615 = alloca %struct.ScmObj*, align 8
%args46588$ae42775$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42776, %struct.ScmObj* %args46588$ae42775$1)
store volatile %struct.ScmObj* %args46588$ae42775$2, %struct.ScmObj** %stackaddr$prim47615, align 8
%clofunc47616 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42775)
musttail call tailcc void %clofunc47616(%struct.ScmObj* %ae42775, %struct.ScmObj* %args46588$ae42775$2)
ret void
}

define tailcc void @proc_clo$ae42775(%struct.ScmObj* %env$ae42775,%struct.ScmObj* %current_45args46570) {
%stackaddr$env-ref47617 = alloca %struct.ScmObj*, align 8
%n40183 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42775, i64 0)
store %struct.ScmObj* %n40183, %struct.ScmObj** %stackaddr$env-ref47617
%stackaddr$env-ref47618 = alloca %struct.ScmObj*, align 8
%lst40182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42775, i64 1)
store %struct.ScmObj* %lst40182, %struct.ScmObj** %stackaddr$env-ref47618
%stackaddr$env-ref47619 = alloca %struct.ScmObj*, align 8
%k40413 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42775, i64 2)
store %struct.ScmObj* %k40413, %struct.ScmObj** %stackaddr$env-ref47619
%stackaddr$prim47620 = alloca %struct.ScmObj*, align 8
%_95k40414 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46570)
store volatile %struct.ScmObj* %_95k40414, %struct.ScmObj** %stackaddr$prim47620, align 8
%stackaddr$prim47621 = alloca %struct.ScmObj*, align 8
%current_45args46571 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46570)
store volatile %struct.ScmObj* %current_45args46571, %struct.ScmObj** %stackaddr$prim47621, align 8
%stackaddr$prim47622 = alloca %struct.ScmObj*, align 8
%anf_45bind40312 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46571)
store volatile %struct.ScmObj* %anf_45bind40312, %struct.ScmObj** %stackaddr$prim47622, align 8
%stackaddr$makeclosure47623 = alloca %struct.ScmObj*, align 8
%fptrToInt47624 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42791 to i64
%ae42791 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47624)
store volatile %struct.ScmObj* %ae42791, %struct.ScmObj** %stackaddr$makeclosure47623, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42791, %struct.ScmObj* %n40183, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42791, %struct.ScmObj* %lst40182, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42791, %struct.ScmObj* %k40413, i64 2)
%stackaddr$makeclosure47625 = alloca %struct.ScmObj*, align 8
%fptrToInt47626 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42792 to i64
%ae42792 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47626)
store volatile %struct.ScmObj* %ae42792, %struct.ScmObj** %stackaddr$makeclosure47625, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42792, %struct.ScmObj* %n40183, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42792, %struct.ScmObj* %lst40182, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42792, %struct.ScmObj* %k40413, i64 2)
%args46583$anf_45bind40312$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47627 = alloca %struct.ScmObj*, align 8
%args46583$anf_45bind40312$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42792, %struct.ScmObj* %args46583$anf_45bind40312$0)
store volatile %struct.ScmObj* %args46583$anf_45bind40312$1, %struct.ScmObj** %stackaddr$prim47627, align 8
%stackaddr$prim47628 = alloca %struct.ScmObj*, align 8
%args46583$anf_45bind40312$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42791, %struct.ScmObj* %args46583$anf_45bind40312$1)
store volatile %struct.ScmObj* %args46583$anf_45bind40312$2, %struct.ScmObj** %stackaddr$prim47628, align 8
%clofunc47629 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40312)
musttail call tailcc void %clofunc47629(%struct.ScmObj* %anf_45bind40312, %struct.ScmObj* %args46583$anf_45bind40312$2)
ret void
}

define tailcc void @proc_clo$ae42791(%struct.ScmObj* %env$ae42791,%struct.ScmObj* %current_45args46573) {
%stackaddr$env-ref47630 = alloca %struct.ScmObj*, align 8
%n40183 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42791, i64 0)
store %struct.ScmObj* %n40183, %struct.ScmObj** %stackaddr$env-ref47630
%stackaddr$env-ref47631 = alloca %struct.ScmObj*, align 8
%lst40182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42791, i64 1)
store %struct.ScmObj* %lst40182, %struct.ScmObj** %stackaddr$env-ref47631
%stackaddr$env-ref47632 = alloca %struct.ScmObj*, align 8
%k40413 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42791, i64 2)
store %struct.ScmObj* %k40413, %struct.ScmObj** %stackaddr$env-ref47632
%stackaddr$prim47633 = alloca %struct.ScmObj*, align 8
%_95k40415 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46573)
store volatile %struct.ScmObj* %_95k40415, %struct.ScmObj** %stackaddr$prim47633, align 8
%stackaddr$prim47634 = alloca %struct.ScmObj*, align 8
%current_45args46574 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46573)
store volatile %struct.ScmObj* %current_45args46574, %struct.ScmObj** %stackaddr$prim47634, align 8
%stackaddr$prim47635 = alloca %struct.ScmObj*, align 8
%cc40184 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46574)
store volatile %struct.ScmObj* %cc40184, %struct.ScmObj** %stackaddr$prim47635, align 8
%ae42934 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47636 = alloca %struct.ScmObj*, align 8
%anf_45bind40313 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40183, %struct.ScmObj* %ae42934)
store volatile %struct.ScmObj* %anf_45bind40313, %struct.ScmObj** %stackaddr$prim47636, align 8
%ae42935 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47637 = alloca %struct.ScmObj*, align 8
%anf_45bind40314 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae42935, %struct.ScmObj* %anf_45bind40313)
store volatile %struct.ScmObj* %anf_45bind40314, %struct.ScmObj** %stackaddr$prim47637, align 8
%truthy$cmp47638 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40314)
%cmp$cmp47638 = icmp eq i64 %truthy$cmp47638, 1
br i1 %cmp$cmp47638, label %truebranch$cmp47638, label %falsebranch$cmp47638
truebranch$cmp47638:
%ae42939 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47639 = alloca %struct.ScmObj*, align 8
%cpsprim40416 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40182, %struct.ScmObj* %ae42939)
store volatile %struct.ScmObj* %cpsprim40416, %struct.ScmObj** %stackaddr$prim47639, align 8
%ae42941 = call %struct.ScmObj* @const_init_int(i64 0)
%args46576$k40413$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47640 = alloca %struct.ScmObj*, align 8
%args46576$k40413$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40416, %struct.ScmObj* %args46576$k40413$0)
store volatile %struct.ScmObj* %args46576$k40413$1, %struct.ScmObj** %stackaddr$prim47640, align 8
%stackaddr$prim47641 = alloca %struct.ScmObj*, align 8
%args46576$k40413$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42941, %struct.ScmObj* %args46576$k40413$1)
store volatile %struct.ScmObj* %args46576$k40413$2, %struct.ScmObj** %stackaddr$prim47641, align 8
%clofunc47642 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40413)
musttail call tailcc void %clofunc47642(%struct.ScmObj* %k40413, %struct.ScmObj* %args46576$k40413$2)
ret void
falsebranch$cmp47638:
%ae42952 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47643 = alloca %struct.ScmObj*, align 8
%anf_45bind40315 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40182, %struct.ScmObj* %ae42952)
store volatile %struct.ScmObj* %anf_45bind40315, %struct.ScmObj** %stackaddr$prim47643, align 8
%stackaddr$prim47644 = alloca %struct.ScmObj*, align 8
%anf_45bind40316 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40315)
store volatile %struct.ScmObj* %anf_45bind40316, %struct.ScmObj** %stackaddr$prim47644, align 8
%ae42955 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47645 = alloca %struct.ScmObj*, align 8
%_95040187 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40182, %struct.ScmObj* %ae42955, %struct.ScmObj* %anf_45bind40316)
store volatile %struct.ScmObj* %_95040187, %struct.ScmObj** %stackaddr$prim47645, align 8
%ae42958 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47646 = alloca %struct.ScmObj*, align 8
%anf_45bind40317 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40183, %struct.ScmObj* %ae42958)
store volatile %struct.ScmObj* %anf_45bind40317, %struct.ScmObj** %stackaddr$prim47646, align 8
%ae42960 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim47647 = alloca %struct.ScmObj*, align 8
%anf_45bind40318 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind40317, %struct.ScmObj* %ae42960)
store volatile %struct.ScmObj* %anf_45bind40318, %struct.ScmObj** %stackaddr$prim47647, align 8
%ae42962 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47648 = alloca %struct.ScmObj*, align 8
%_95140186 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n40183, %struct.ScmObj* %ae42962, %struct.ScmObj* %anf_45bind40318)
store volatile %struct.ScmObj* %_95140186, %struct.ScmObj** %stackaddr$prim47648, align 8
%args46577$cc40184$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47649 = alloca %struct.ScmObj*, align 8
%args46577$cc40184$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40184, %struct.ScmObj* %args46577$cc40184$0)
store volatile %struct.ScmObj* %args46577$cc40184$1, %struct.ScmObj** %stackaddr$prim47649, align 8
%stackaddr$prim47650 = alloca %struct.ScmObj*, align 8
%args46577$cc40184$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40413, %struct.ScmObj* %args46577$cc40184$1)
store volatile %struct.ScmObj* %args46577$cc40184$2, %struct.ScmObj** %stackaddr$prim47650, align 8
%clofunc47651 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40184)
musttail call tailcc void %clofunc47651(%struct.ScmObj* %cc40184, %struct.ScmObj* %args46577$cc40184$2)
ret void
}

define tailcc void @proc_clo$ae42792(%struct.ScmObj* %env$ae42792,%struct.ScmObj* %current_45args46578) {
%stackaddr$env-ref47652 = alloca %struct.ScmObj*, align 8
%n40183 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42792, i64 0)
store %struct.ScmObj* %n40183, %struct.ScmObj** %stackaddr$env-ref47652
%stackaddr$env-ref47653 = alloca %struct.ScmObj*, align 8
%lst40182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42792, i64 1)
store %struct.ScmObj* %lst40182, %struct.ScmObj** %stackaddr$env-ref47653
%stackaddr$env-ref47654 = alloca %struct.ScmObj*, align 8
%k40413 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42792, i64 2)
store %struct.ScmObj* %k40413, %struct.ScmObj** %stackaddr$env-ref47654
%stackaddr$prim47655 = alloca %struct.ScmObj*, align 8
%_95k40415 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46578)
store volatile %struct.ScmObj* %_95k40415, %struct.ScmObj** %stackaddr$prim47655, align 8
%stackaddr$prim47656 = alloca %struct.ScmObj*, align 8
%current_45args46579 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46578)
store volatile %struct.ScmObj* %current_45args46579, %struct.ScmObj** %stackaddr$prim47656, align 8
%stackaddr$prim47657 = alloca %struct.ScmObj*, align 8
%cc40184 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46579)
store volatile %struct.ScmObj* %cc40184, %struct.ScmObj** %stackaddr$prim47657, align 8
%ae42794 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47658 = alloca %struct.ScmObj*, align 8
%anf_45bind40313 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40183, %struct.ScmObj* %ae42794)
store volatile %struct.ScmObj* %anf_45bind40313, %struct.ScmObj** %stackaddr$prim47658, align 8
%ae42795 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47659 = alloca %struct.ScmObj*, align 8
%anf_45bind40314 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae42795, %struct.ScmObj* %anf_45bind40313)
store volatile %struct.ScmObj* %anf_45bind40314, %struct.ScmObj** %stackaddr$prim47659, align 8
%truthy$cmp47660 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40314)
%cmp$cmp47660 = icmp eq i64 %truthy$cmp47660, 1
br i1 %cmp$cmp47660, label %truebranch$cmp47660, label %falsebranch$cmp47660
truebranch$cmp47660:
%ae42799 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47661 = alloca %struct.ScmObj*, align 8
%cpsprim40416 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40182, %struct.ScmObj* %ae42799)
store volatile %struct.ScmObj* %cpsprim40416, %struct.ScmObj** %stackaddr$prim47661, align 8
%ae42801 = call %struct.ScmObj* @const_init_int(i64 0)
%args46581$k40413$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47662 = alloca %struct.ScmObj*, align 8
%args46581$k40413$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40416, %struct.ScmObj* %args46581$k40413$0)
store volatile %struct.ScmObj* %args46581$k40413$1, %struct.ScmObj** %stackaddr$prim47662, align 8
%stackaddr$prim47663 = alloca %struct.ScmObj*, align 8
%args46581$k40413$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42801, %struct.ScmObj* %args46581$k40413$1)
store volatile %struct.ScmObj* %args46581$k40413$2, %struct.ScmObj** %stackaddr$prim47663, align 8
%clofunc47664 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40413)
musttail call tailcc void %clofunc47664(%struct.ScmObj* %k40413, %struct.ScmObj* %args46581$k40413$2)
ret void
falsebranch$cmp47660:
%ae42812 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47665 = alloca %struct.ScmObj*, align 8
%anf_45bind40315 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40182, %struct.ScmObj* %ae42812)
store volatile %struct.ScmObj* %anf_45bind40315, %struct.ScmObj** %stackaddr$prim47665, align 8
%stackaddr$prim47666 = alloca %struct.ScmObj*, align 8
%anf_45bind40316 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40315)
store volatile %struct.ScmObj* %anf_45bind40316, %struct.ScmObj** %stackaddr$prim47666, align 8
%ae42815 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47667 = alloca %struct.ScmObj*, align 8
%_95040187 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40182, %struct.ScmObj* %ae42815, %struct.ScmObj* %anf_45bind40316)
store volatile %struct.ScmObj* %_95040187, %struct.ScmObj** %stackaddr$prim47667, align 8
%ae42818 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47668 = alloca %struct.ScmObj*, align 8
%anf_45bind40317 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40183, %struct.ScmObj* %ae42818)
store volatile %struct.ScmObj* %anf_45bind40317, %struct.ScmObj** %stackaddr$prim47668, align 8
%ae42820 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim47669 = alloca %struct.ScmObj*, align 8
%anf_45bind40318 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind40317, %struct.ScmObj* %ae42820)
store volatile %struct.ScmObj* %anf_45bind40318, %struct.ScmObj** %stackaddr$prim47669, align 8
%ae42822 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47670 = alloca %struct.ScmObj*, align 8
%_95140186 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n40183, %struct.ScmObj* %ae42822, %struct.ScmObj* %anf_45bind40318)
store volatile %struct.ScmObj* %_95140186, %struct.ScmObj** %stackaddr$prim47670, align 8
%args46582$cc40184$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47671 = alloca %struct.ScmObj*, align 8
%args46582$cc40184$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40184, %struct.ScmObj* %args46582$cc40184$0)
store volatile %struct.ScmObj* %args46582$cc40184$1, %struct.ScmObj** %stackaddr$prim47671, align 8
%stackaddr$prim47672 = alloca %struct.ScmObj*, align 8
%args46582$cc40184$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40413, %struct.ScmObj* %args46582$cc40184$1)
store volatile %struct.ScmObj* %args46582$cc40184$2, %struct.ScmObj** %stackaddr$prim47672, align 8
%clofunc47673 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40184)
musttail call tailcc void %clofunc47673(%struct.ScmObj* %cc40184, %struct.ScmObj* %args46582$cc40184$2)
ret void
}

define tailcc void @proc_clo$ae42777(%struct.ScmObj* %env$ae42777,%struct.ScmObj* %current_45args46584) {
%stackaddr$prim47674 = alloca %struct.ScmObj*, align 8
%k40417 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46584)
store volatile %struct.ScmObj* %k40417, %struct.ScmObj** %stackaddr$prim47674, align 8
%stackaddr$prim47675 = alloca %struct.ScmObj*, align 8
%current_45args46585 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46584)
store volatile %struct.ScmObj* %current_45args46585, %struct.ScmObj** %stackaddr$prim47675, align 8
%stackaddr$prim47676 = alloca %struct.ScmObj*, align 8
%u40185 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46585)
store volatile %struct.ScmObj* %u40185, %struct.ScmObj** %stackaddr$prim47676, align 8
%args46587$u40185$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47677 = alloca %struct.ScmObj*, align 8
%args46587$u40185$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u40185, %struct.ScmObj* %args46587$u40185$0)
store volatile %struct.ScmObj* %args46587$u40185$1, %struct.ScmObj** %stackaddr$prim47677, align 8
%stackaddr$prim47678 = alloca %struct.ScmObj*, align 8
%args46587$u40185$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40417, %struct.ScmObj* %args46587$u40185$1)
store volatile %struct.ScmObj* %args46587$u40185$2, %struct.ScmObj** %stackaddr$prim47678, align 8
%clofunc47679 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u40185)
musttail call tailcc void %clofunc47679(%struct.ScmObj* %u40185, %struct.ScmObj* %args46587$u40185$2)
ret void
}

define tailcc void @proc_clo$ae42354(%struct.ScmObj* %env$ae42354,%struct.ScmObj* %current_45args46590) {
%stackaddr$prim47680 = alloca %struct.ScmObj*, align 8
%k40418 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46590)
store volatile %struct.ScmObj* %k40418, %struct.ScmObj** %stackaddr$prim47680, align 8
%stackaddr$prim47681 = alloca %struct.ScmObj*, align 8
%current_45args46591 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46590)
store volatile %struct.ScmObj* %current_45args46591, %struct.ScmObj** %stackaddr$prim47681, align 8
%stackaddr$prim47682 = alloca %struct.ScmObj*, align 8
%a40189 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46591)
store volatile %struct.ScmObj* %a40189, %struct.ScmObj** %stackaddr$prim47682, align 8
%ae42355 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim47683 = alloca %struct.ScmObj*, align 8
%a40190 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42355, %struct.ScmObj* %a40189)
store volatile %struct.ScmObj* %a40190, %struct.ScmObj** %stackaddr$prim47683, align 8
%stackaddr$makeclosure47684 = alloca %struct.ScmObj*, align 8
%fptrToInt47685 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42357 to i64
%ae42357 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47685)
store volatile %struct.ScmObj* %ae42357, %struct.ScmObj** %stackaddr$makeclosure47684, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42357, %struct.ScmObj* %k40418, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42357, %struct.ScmObj* %a40190, i64 1)
%ae42358 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47686 = alloca %struct.ScmObj*, align 8
%fptrToInt47687 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42359 to i64
%ae42359 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47687)
store volatile %struct.ScmObj* %ae42359, %struct.ScmObj** %stackaddr$makeclosure47686, align 8
%args46613$ae42357$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47688 = alloca %struct.ScmObj*, align 8
%args46613$ae42357$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42359, %struct.ScmObj* %args46613$ae42357$0)
store volatile %struct.ScmObj* %args46613$ae42357$1, %struct.ScmObj** %stackaddr$prim47688, align 8
%stackaddr$prim47689 = alloca %struct.ScmObj*, align 8
%args46613$ae42357$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42358, %struct.ScmObj* %args46613$ae42357$1)
store volatile %struct.ScmObj* %args46613$ae42357$2, %struct.ScmObj** %stackaddr$prim47689, align 8
%clofunc47690 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42357)
musttail call tailcc void %clofunc47690(%struct.ScmObj* %ae42357, %struct.ScmObj* %args46613$ae42357$2)
ret void
}

define tailcc void @proc_clo$ae42357(%struct.ScmObj* %env$ae42357,%struct.ScmObj* %current_45args46593) {
%stackaddr$env-ref47691 = alloca %struct.ScmObj*, align 8
%k40418 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42357, i64 0)
store %struct.ScmObj* %k40418, %struct.ScmObj** %stackaddr$env-ref47691
%stackaddr$env-ref47692 = alloca %struct.ScmObj*, align 8
%a40190 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42357, i64 1)
store %struct.ScmObj* %a40190, %struct.ScmObj** %stackaddr$env-ref47692
%stackaddr$prim47693 = alloca %struct.ScmObj*, align 8
%_95k40419 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46593)
store volatile %struct.ScmObj* %_95k40419, %struct.ScmObj** %stackaddr$prim47693, align 8
%stackaddr$prim47694 = alloca %struct.ScmObj*, align 8
%current_45args46594 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46593)
store volatile %struct.ScmObj* %current_45args46594, %struct.ScmObj** %stackaddr$prim47694, align 8
%stackaddr$prim47695 = alloca %struct.ScmObj*, align 8
%anf_45bind40304 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46594)
store volatile %struct.ScmObj* %anf_45bind40304, %struct.ScmObj** %stackaddr$prim47695, align 8
%stackaddr$makeclosure47696 = alloca %struct.ScmObj*, align 8
%fptrToInt47697 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42376 to i64
%ae42376 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47697)
store volatile %struct.ScmObj* %ae42376, %struct.ScmObj** %stackaddr$makeclosure47696, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42376, %struct.ScmObj* %k40418, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42376, %struct.ScmObj* %a40190, i64 1)
%stackaddr$makeclosure47698 = alloca %struct.ScmObj*, align 8
%fptrToInt47699 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42377 to i64
%ae42377 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47699)
store volatile %struct.ScmObj* %ae42377, %struct.ScmObj** %stackaddr$makeclosure47698, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42377, %struct.ScmObj* %k40418, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42377, %struct.ScmObj* %a40190, i64 1)
%args46608$anf_45bind40304$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47700 = alloca %struct.ScmObj*, align 8
%args46608$anf_45bind40304$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42377, %struct.ScmObj* %args46608$anf_45bind40304$0)
store volatile %struct.ScmObj* %args46608$anf_45bind40304$1, %struct.ScmObj** %stackaddr$prim47700, align 8
%stackaddr$prim47701 = alloca %struct.ScmObj*, align 8
%args46608$anf_45bind40304$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42376, %struct.ScmObj* %args46608$anf_45bind40304$1)
store volatile %struct.ScmObj* %args46608$anf_45bind40304$2, %struct.ScmObj** %stackaddr$prim47701, align 8
%clofunc47702 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40304)
musttail call tailcc void %clofunc47702(%struct.ScmObj* %anf_45bind40304, %struct.ScmObj* %args46608$anf_45bind40304$2)
ret void
}

define tailcc void @proc_clo$ae42376(%struct.ScmObj* %env$ae42376,%struct.ScmObj* %current_45args46596) {
%stackaddr$env-ref47703 = alloca %struct.ScmObj*, align 8
%k40418 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42376, i64 0)
store %struct.ScmObj* %k40418, %struct.ScmObj** %stackaddr$env-ref47703
%stackaddr$env-ref47704 = alloca %struct.ScmObj*, align 8
%a40190 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42376, i64 1)
store %struct.ScmObj* %a40190, %struct.ScmObj** %stackaddr$env-ref47704
%stackaddr$prim47705 = alloca %struct.ScmObj*, align 8
%_95k40420 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46596)
store volatile %struct.ScmObj* %_95k40420, %struct.ScmObj** %stackaddr$prim47705, align 8
%stackaddr$prim47706 = alloca %struct.ScmObj*, align 8
%current_45args46597 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46596)
store volatile %struct.ScmObj* %current_45args46597, %struct.ScmObj** %stackaddr$prim47706, align 8
%stackaddr$prim47707 = alloca %struct.ScmObj*, align 8
%cc40191 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46597)
store volatile %struct.ScmObj* %cc40191, %struct.ScmObj** %stackaddr$prim47707, align 8
%ae42492 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47708 = alloca %struct.ScmObj*, align 8
%anf_45bind40305 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40190, %struct.ScmObj* %ae42492)
store volatile %struct.ScmObj* %anf_45bind40305, %struct.ScmObj** %stackaddr$prim47708, align 8
%stackaddr$prim47709 = alloca %struct.ScmObj*, align 8
%anf_45bind40306 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40305)
store volatile %struct.ScmObj* %anf_45bind40306, %struct.ScmObj** %stackaddr$prim47709, align 8
%truthy$cmp47710 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40306)
%cmp$cmp47710 = icmp eq i64 %truthy$cmp47710, 1
br i1 %cmp$cmp47710, label %truebranch$cmp47710, label %falsebranch$cmp47710
truebranch$cmp47710:
%ae42496 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42497 = call %struct.ScmObj* @const_init_true()
%args46599$k40418$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47711 = alloca %struct.ScmObj*, align 8
%args46599$k40418$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42497, %struct.ScmObj* %args46599$k40418$0)
store volatile %struct.ScmObj* %args46599$k40418$1, %struct.ScmObj** %stackaddr$prim47711, align 8
%stackaddr$prim47712 = alloca %struct.ScmObj*, align 8
%args46599$k40418$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42496, %struct.ScmObj* %args46599$k40418$1)
store volatile %struct.ScmObj* %args46599$k40418$2, %struct.ScmObj** %stackaddr$prim47712, align 8
%clofunc47713 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40418)
musttail call tailcc void %clofunc47713(%struct.ScmObj* %k40418, %struct.ScmObj* %args46599$k40418$2)
ret void
falsebranch$cmp47710:
%ae42505 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47714 = alloca %struct.ScmObj*, align 8
%anf_45bind40307 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40190, %struct.ScmObj* %ae42505)
store volatile %struct.ScmObj* %anf_45bind40307, %struct.ScmObj** %stackaddr$prim47714, align 8
%stackaddr$prim47715 = alloca %struct.ScmObj*, align 8
%anf_45bind40308 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind40307)
store volatile %struct.ScmObj* %anf_45bind40308, %struct.ScmObj** %stackaddr$prim47715, align 8
%truthy$cmp47716 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40308)
%cmp$cmp47716 = icmp eq i64 %truthy$cmp47716, 1
br i1 %cmp$cmp47716, label %truebranch$cmp47716, label %falsebranch$cmp47716
truebranch$cmp47716:
%ae42509 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47717 = alloca %struct.ScmObj*, align 8
%anf_45bind40309 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40190, %struct.ScmObj* %ae42509)
store volatile %struct.ScmObj* %anf_45bind40309, %struct.ScmObj** %stackaddr$prim47717, align 8
%stackaddr$prim47718 = alloca %struct.ScmObj*, align 8
%b40193 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40309)
store volatile %struct.ScmObj* %b40193, %struct.ScmObj** %stackaddr$prim47718, align 8
%ae42512 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47719 = alloca %struct.ScmObj*, align 8
%anf_45bind40310 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40190, %struct.ScmObj* %ae42512)
store volatile %struct.ScmObj* %anf_45bind40310, %struct.ScmObj** %stackaddr$prim47719, align 8
%stackaddr$prim47720 = alloca %struct.ScmObj*, align 8
%anf_45bind40311 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40310)
store volatile %struct.ScmObj* %anf_45bind40311, %struct.ScmObj** %stackaddr$prim47720, align 8
%ae42515 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47721 = alloca %struct.ScmObj*, align 8
%_95040194 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a40190, %struct.ScmObj* %ae42515, %struct.ScmObj* %anf_45bind40311)
store volatile %struct.ScmObj* %_95040194, %struct.ScmObj** %stackaddr$prim47721, align 8
%args46600$cc40191$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47722 = alloca %struct.ScmObj*, align 8
%args46600$cc40191$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40191, %struct.ScmObj* %args46600$cc40191$0)
store volatile %struct.ScmObj* %args46600$cc40191$1, %struct.ScmObj** %stackaddr$prim47722, align 8
%stackaddr$prim47723 = alloca %struct.ScmObj*, align 8
%args46600$cc40191$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40418, %struct.ScmObj* %args46600$cc40191$1)
store volatile %struct.ScmObj* %args46600$cc40191$2, %struct.ScmObj** %stackaddr$prim47723, align 8
%clofunc47724 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40191)
musttail call tailcc void %clofunc47724(%struct.ScmObj* %cc40191, %struct.ScmObj* %args46600$cc40191$2)
ret void
falsebranch$cmp47716:
%ae42548 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42549 = call %struct.ScmObj* @const_init_false()
%args46601$k40418$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47725 = alloca %struct.ScmObj*, align 8
%args46601$k40418$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42549, %struct.ScmObj* %args46601$k40418$0)
store volatile %struct.ScmObj* %args46601$k40418$1, %struct.ScmObj** %stackaddr$prim47725, align 8
%stackaddr$prim47726 = alloca %struct.ScmObj*, align 8
%args46601$k40418$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42548, %struct.ScmObj* %args46601$k40418$1)
store volatile %struct.ScmObj* %args46601$k40418$2, %struct.ScmObj** %stackaddr$prim47726, align 8
%clofunc47727 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40418)
musttail call tailcc void %clofunc47727(%struct.ScmObj* %k40418, %struct.ScmObj* %args46601$k40418$2)
ret void
}

define tailcc void @proc_clo$ae42377(%struct.ScmObj* %env$ae42377,%struct.ScmObj* %current_45args46602) {
%stackaddr$env-ref47728 = alloca %struct.ScmObj*, align 8
%k40418 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42377, i64 0)
store %struct.ScmObj* %k40418, %struct.ScmObj** %stackaddr$env-ref47728
%stackaddr$env-ref47729 = alloca %struct.ScmObj*, align 8
%a40190 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42377, i64 1)
store %struct.ScmObj* %a40190, %struct.ScmObj** %stackaddr$env-ref47729
%stackaddr$prim47730 = alloca %struct.ScmObj*, align 8
%_95k40420 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46602)
store volatile %struct.ScmObj* %_95k40420, %struct.ScmObj** %stackaddr$prim47730, align 8
%stackaddr$prim47731 = alloca %struct.ScmObj*, align 8
%current_45args46603 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46602)
store volatile %struct.ScmObj* %current_45args46603, %struct.ScmObj** %stackaddr$prim47731, align 8
%stackaddr$prim47732 = alloca %struct.ScmObj*, align 8
%cc40191 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46603)
store volatile %struct.ScmObj* %cc40191, %struct.ScmObj** %stackaddr$prim47732, align 8
%ae42379 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47733 = alloca %struct.ScmObj*, align 8
%anf_45bind40305 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40190, %struct.ScmObj* %ae42379)
store volatile %struct.ScmObj* %anf_45bind40305, %struct.ScmObj** %stackaddr$prim47733, align 8
%stackaddr$prim47734 = alloca %struct.ScmObj*, align 8
%anf_45bind40306 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40305)
store volatile %struct.ScmObj* %anf_45bind40306, %struct.ScmObj** %stackaddr$prim47734, align 8
%truthy$cmp47735 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40306)
%cmp$cmp47735 = icmp eq i64 %truthy$cmp47735, 1
br i1 %cmp$cmp47735, label %truebranch$cmp47735, label %falsebranch$cmp47735
truebranch$cmp47735:
%ae42383 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42384 = call %struct.ScmObj* @const_init_true()
%args46605$k40418$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47736 = alloca %struct.ScmObj*, align 8
%args46605$k40418$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42384, %struct.ScmObj* %args46605$k40418$0)
store volatile %struct.ScmObj* %args46605$k40418$1, %struct.ScmObj** %stackaddr$prim47736, align 8
%stackaddr$prim47737 = alloca %struct.ScmObj*, align 8
%args46605$k40418$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42383, %struct.ScmObj* %args46605$k40418$1)
store volatile %struct.ScmObj* %args46605$k40418$2, %struct.ScmObj** %stackaddr$prim47737, align 8
%clofunc47738 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40418)
musttail call tailcc void %clofunc47738(%struct.ScmObj* %k40418, %struct.ScmObj* %args46605$k40418$2)
ret void
falsebranch$cmp47735:
%ae42392 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47739 = alloca %struct.ScmObj*, align 8
%anf_45bind40307 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40190, %struct.ScmObj* %ae42392)
store volatile %struct.ScmObj* %anf_45bind40307, %struct.ScmObj** %stackaddr$prim47739, align 8
%stackaddr$prim47740 = alloca %struct.ScmObj*, align 8
%anf_45bind40308 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind40307)
store volatile %struct.ScmObj* %anf_45bind40308, %struct.ScmObj** %stackaddr$prim47740, align 8
%truthy$cmp47741 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40308)
%cmp$cmp47741 = icmp eq i64 %truthy$cmp47741, 1
br i1 %cmp$cmp47741, label %truebranch$cmp47741, label %falsebranch$cmp47741
truebranch$cmp47741:
%ae42396 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47742 = alloca %struct.ScmObj*, align 8
%anf_45bind40309 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40190, %struct.ScmObj* %ae42396)
store volatile %struct.ScmObj* %anf_45bind40309, %struct.ScmObj** %stackaddr$prim47742, align 8
%stackaddr$prim47743 = alloca %struct.ScmObj*, align 8
%b40193 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40309)
store volatile %struct.ScmObj* %b40193, %struct.ScmObj** %stackaddr$prim47743, align 8
%ae42399 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47744 = alloca %struct.ScmObj*, align 8
%anf_45bind40310 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40190, %struct.ScmObj* %ae42399)
store volatile %struct.ScmObj* %anf_45bind40310, %struct.ScmObj** %stackaddr$prim47744, align 8
%stackaddr$prim47745 = alloca %struct.ScmObj*, align 8
%anf_45bind40311 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40310)
store volatile %struct.ScmObj* %anf_45bind40311, %struct.ScmObj** %stackaddr$prim47745, align 8
%ae42402 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47746 = alloca %struct.ScmObj*, align 8
%_95040194 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a40190, %struct.ScmObj* %ae42402, %struct.ScmObj* %anf_45bind40311)
store volatile %struct.ScmObj* %_95040194, %struct.ScmObj** %stackaddr$prim47746, align 8
%args46606$cc40191$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47747 = alloca %struct.ScmObj*, align 8
%args46606$cc40191$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40191, %struct.ScmObj* %args46606$cc40191$0)
store volatile %struct.ScmObj* %args46606$cc40191$1, %struct.ScmObj** %stackaddr$prim47747, align 8
%stackaddr$prim47748 = alloca %struct.ScmObj*, align 8
%args46606$cc40191$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40418, %struct.ScmObj* %args46606$cc40191$1)
store volatile %struct.ScmObj* %args46606$cc40191$2, %struct.ScmObj** %stackaddr$prim47748, align 8
%clofunc47749 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40191)
musttail call tailcc void %clofunc47749(%struct.ScmObj* %cc40191, %struct.ScmObj* %args46606$cc40191$2)
ret void
falsebranch$cmp47741:
%ae42435 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42436 = call %struct.ScmObj* @const_init_false()
%args46607$k40418$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47750 = alloca %struct.ScmObj*, align 8
%args46607$k40418$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42436, %struct.ScmObj* %args46607$k40418$0)
store volatile %struct.ScmObj* %args46607$k40418$1, %struct.ScmObj** %stackaddr$prim47750, align 8
%stackaddr$prim47751 = alloca %struct.ScmObj*, align 8
%args46607$k40418$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42435, %struct.ScmObj* %args46607$k40418$1)
store volatile %struct.ScmObj* %args46607$k40418$2, %struct.ScmObj** %stackaddr$prim47751, align 8
%clofunc47752 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40418)
musttail call tailcc void %clofunc47752(%struct.ScmObj* %k40418, %struct.ScmObj* %args46607$k40418$2)
ret void
}

define tailcc void @proc_clo$ae42359(%struct.ScmObj* %env$ae42359,%struct.ScmObj* %current_45args46609) {
%stackaddr$prim47753 = alloca %struct.ScmObj*, align 8
%k40421 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46609)
store volatile %struct.ScmObj* %k40421, %struct.ScmObj** %stackaddr$prim47753, align 8
%stackaddr$prim47754 = alloca %struct.ScmObj*, align 8
%current_45args46610 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46609)
store volatile %struct.ScmObj* %current_45args46610, %struct.ScmObj** %stackaddr$prim47754, align 8
%stackaddr$prim47755 = alloca %struct.ScmObj*, align 8
%k40192 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46610)
store volatile %struct.ScmObj* %k40192, %struct.ScmObj** %stackaddr$prim47755, align 8
%ae42361 = call %struct.ScmObj* @const_init_int(i64 0)
%args46612$k40421$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47756 = alloca %struct.ScmObj*, align 8
%args46612$k40421$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40192, %struct.ScmObj* %args46612$k40421$0)
store volatile %struct.ScmObj* %args46612$k40421$1, %struct.ScmObj** %stackaddr$prim47756, align 8
%stackaddr$prim47757 = alloca %struct.ScmObj*, align 8
%args46612$k40421$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42361, %struct.ScmObj* %args46612$k40421$1)
store volatile %struct.ScmObj* %args46612$k40421$2, %struct.ScmObj** %stackaddr$prim47757, align 8
%clofunc47758 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40421)
musttail call tailcc void %clofunc47758(%struct.ScmObj* %k40421, %struct.ScmObj* %args46612$k40421$2)
ret void
}

define tailcc void @proc_clo$ae42282(%struct.ScmObj* %env$ae42282,%struct.ScmObj* %current_45args46615) {
%stackaddr$env-ref47759 = alloca %struct.ScmObj*, align 8
%_37append40196 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42282, i64 0)
store %struct.ScmObj* %_37append40196, %struct.ScmObj** %stackaddr$env-ref47759
%stackaddr$prim47760 = alloca %struct.ScmObj*, align 8
%k40422 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46615)
store volatile %struct.ScmObj* %k40422, %struct.ScmObj** %stackaddr$prim47760, align 8
%stackaddr$prim47761 = alloca %struct.ScmObj*, align 8
%current_45args46616 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46615)
store volatile %struct.ScmObj* %current_45args46616, %struct.ScmObj** %stackaddr$prim47761, align 8
%stackaddr$prim47762 = alloca %struct.ScmObj*, align 8
%ls040199 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46616)
store volatile %struct.ScmObj* %ls040199, %struct.ScmObj** %stackaddr$prim47762, align 8
%stackaddr$prim47763 = alloca %struct.ScmObj*, align 8
%current_45args46617 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46616)
store volatile %struct.ScmObj* %current_45args46617, %struct.ScmObj** %stackaddr$prim47763, align 8
%stackaddr$prim47764 = alloca %struct.ScmObj*, align 8
%ls140198 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46617)
store volatile %struct.ScmObj* %ls140198, %struct.ScmObj** %stackaddr$prim47764, align 8
%stackaddr$prim47765 = alloca %struct.ScmObj*, align 8
%anf_45bind40298 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls040199)
store volatile %struct.ScmObj* %anf_45bind40298, %struct.ScmObj** %stackaddr$prim47765, align 8
%truthy$cmp47766 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40298)
%cmp$cmp47766 = icmp eq i64 %truthy$cmp47766, 1
br i1 %cmp$cmp47766, label %truebranch$cmp47766, label %falsebranch$cmp47766
truebranch$cmp47766:
%ae42286 = call %struct.ScmObj* @const_init_int(i64 0)
%args46619$k40422$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47767 = alloca %struct.ScmObj*, align 8
%args46619$k40422$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls140198, %struct.ScmObj* %args46619$k40422$0)
store volatile %struct.ScmObj* %args46619$k40422$1, %struct.ScmObj** %stackaddr$prim47767, align 8
%stackaddr$prim47768 = alloca %struct.ScmObj*, align 8
%args46619$k40422$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42286, %struct.ScmObj* %args46619$k40422$1)
store volatile %struct.ScmObj* %args46619$k40422$2, %struct.ScmObj** %stackaddr$prim47768, align 8
%clofunc47769 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40422)
musttail call tailcc void %clofunc47769(%struct.ScmObj* %k40422, %struct.ScmObj* %args46619$k40422$2)
ret void
falsebranch$cmp47766:
%stackaddr$prim47770 = alloca %struct.ScmObj*, align 8
%anf_45bind40299 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls040199)
store volatile %struct.ScmObj* %anf_45bind40299, %struct.ScmObj** %stackaddr$prim47770, align 8
%ae42293 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47771 = alloca %struct.ScmObj*, align 8
%anf_45bind40300 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append40196, %struct.ScmObj* %ae42293)
store volatile %struct.ScmObj* %anf_45bind40300, %struct.ScmObj** %stackaddr$prim47771, align 8
%stackaddr$prim47772 = alloca %struct.ScmObj*, align 8
%anf_45bind40301 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls040199)
store volatile %struct.ScmObj* %anf_45bind40301, %struct.ScmObj** %stackaddr$prim47772, align 8
%stackaddr$makeclosure47773 = alloca %struct.ScmObj*, align 8
%fptrToInt47774 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42296 to i64
%ae42296 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47774)
store volatile %struct.ScmObj* %ae42296, %struct.ScmObj** %stackaddr$makeclosure47773, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42296, %struct.ScmObj* %k40422, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42296, %struct.ScmObj* %anf_45bind40299, i64 1)
%args46624$anf_45bind40300$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47775 = alloca %struct.ScmObj*, align 8
%args46624$anf_45bind40300$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls140198, %struct.ScmObj* %args46624$anf_45bind40300$0)
store volatile %struct.ScmObj* %args46624$anf_45bind40300$1, %struct.ScmObj** %stackaddr$prim47775, align 8
%stackaddr$prim47776 = alloca %struct.ScmObj*, align 8
%args46624$anf_45bind40300$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40301, %struct.ScmObj* %args46624$anf_45bind40300$1)
store volatile %struct.ScmObj* %args46624$anf_45bind40300$2, %struct.ScmObj** %stackaddr$prim47776, align 8
%stackaddr$prim47777 = alloca %struct.ScmObj*, align 8
%args46624$anf_45bind40300$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42296, %struct.ScmObj* %args46624$anf_45bind40300$2)
store volatile %struct.ScmObj* %args46624$anf_45bind40300$3, %struct.ScmObj** %stackaddr$prim47777, align 8
%clofunc47778 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40300)
musttail call tailcc void %clofunc47778(%struct.ScmObj* %anf_45bind40300, %struct.ScmObj* %args46624$anf_45bind40300$3)
ret void
}

define tailcc void @proc_clo$ae42296(%struct.ScmObj* %env$ae42296,%struct.ScmObj* %current_45args46620) {
%stackaddr$env-ref47779 = alloca %struct.ScmObj*, align 8
%k40422 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42296, i64 0)
store %struct.ScmObj* %k40422, %struct.ScmObj** %stackaddr$env-ref47779
%stackaddr$env-ref47780 = alloca %struct.ScmObj*, align 8
%anf_45bind40299 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42296, i64 1)
store %struct.ScmObj* %anf_45bind40299, %struct.ScmObj** %stackaddr$env-ref47780
%stackaddr$prim47781 = alloca %struct.ScmObj*, align 8
%_95k40423 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46620)
store volatile %struct.ScmObj* %_95k40423, %struct.ScmObj** %stackaddr$prim47781, align 8
%stackaddr$prim47782 = alloca %struct.ScmObj*, align 8
%current_45args46621 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46620)
store volatile %struct.ScmObj* %current_45args46621, %struct.ScmObj** %stackaddr$prim47782, align 8
%stackaddr$prim47783 = alloca %struct.ScmObj*, align 8
%anf_45bind40302 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46621)
store volatile %struct.ScmObj* %anf_45bind40302, %struct.ScmObj** %stackaddr$prim47783, align 8
%stackaddr$prim47784 = alloca %struct.ScmObj*, align 8
%cpsprim40424 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40299, %struct.ScmObj* %anf_45bind40302)
store volatile %struct.ScmObj* %cpsprim40424, %struct.ScmObj** %stackaddr$prim47784, align 8
%ae42302 = call %struct.ScmObj* @const_init_int(i64 0)
%args46623$k40422$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47785 = alloca %struct.ScmObj*, align 8
%args46623$k40422$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40424, %struct.ScmObj* %args46623$k40422$0)
store volatile %struct.ScmObj* %args46623$k40422$1, %struct.ScmObj** %stackaddr$prim47785, align 8
%stackaddr$prim47786 = alloca %struct.ScmObj*, align 8
%args46623$k40422$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42302, %struct.ScmObj* %args46623$k40422$1)
store volatile %struct.ScmObj* %args46623$k40422$2, %struct.ScmObj** %stackaddr$prim47786, align 8
%clofunc47787 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40422)
musttail call tailcc void %clofunc47787(%struct.ScmObj* %k40422, %struct.ScmObj* %args46623$k40422$2)
ret void
}

define tailcc void @proc_clo$ae42256(%struct.ScmObj* %env$ae42256,%struct.ScmObj* %current_45args46626) {
%stackaddr$prim47788 = alloca %struct.ScmObj*, align 8
%k40425 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46626)
store volatile %struct.ScmObj* %k40425, %struct.ScmObj** %stackaddr$prim47788, align 8
%stackaddr$prim47789 = alloca %struct.ScmObj*, align 8
%current_45args46627 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46626)
store volatile %struct.ScmObj* %current_45args46627, %struct.ScmObj** %stackaddr$prim47789, align 8
%stackaddr$prim47790 = alloca %struct.ScmObj*, align 8
%a40202 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46627)
store volatile %struct.ScmObj* %a40202, %struct.ScmObj** %stackaddr$prim47790, align 8
%stackaddr$prim47791 = alloca %struct.ScmObj*, align 8
%current_45args46628 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46627)
store volatile %struct.ScmObj* %current_45args46628, %struct.ScmObj** %stackaddr$prim47791, align 8
%stackaddr$prim47792 = alloca %struct.ScmObj*, align 8
%b40201 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46628)
store volatile %struct.ScmObj* %b40201, %struct.ScmObj** %stackaddr$prim47792, align 8
%stackaddr$prim47793 = alloca %struct.ScmObj*, align 8
%anf_45bind40297 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a40202, %struct.ScmObj* %b40201)
store volatile %struct.ScmObj* %anf_45bind40297, %struct.ScmObj** %stackaddr$prim47793, align 8
%stackaddr$prim47794 = alloca %struct.ScmObj*, align 8
%cpsprim40426 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind40297)
store volatile %struct.ScmObj* %cpsprim40426, %struct.ScmObj** %stackaddr$prim47794, align 8
%ae42261 = call %struct.ScmObj* @const_init_int(i64 0)
%args46630$k40425$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47795 = alloca %struct.ScmObj*, align 8
%args46630$k40425$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40426, %struct.ScmObj* %args46630$k40425$0)
store volatile %struct.ScmObj* %args46630$k40425$1, %struct.ScmObj** %stackaddr$prim47795, align 8
%stackaddr$prim47796 = alloca %struct.ScmObj*, align 8
%args46630$k40425$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42261, %struct.ScmObj* %args46630$k40425$1)
store volatile %struct.ScmObj* %args46630$k40425$2, %struct.ScmObj** %stackaddr$prim47796, align 8
%clofunc47797 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40425)
musttail call tailcc void %clofunc47797(%struct.ScmObj* %k40425, %struct.ScmObj* %args46630$k40425$2)
ret void
}

define tailcc void @proc_clo$ae42232(%struct.ScmObj* %env$ae42232,%struct.ScmObj* %current_45args46632) {
%stackaddr$prim47798 = alloca %struct.ScmObj*, align 8
%k40427 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46632)
store volatile %struct.ScmObj* %k40427, %struct.ScmObj** %stackaddr$prim47798, align 8
%stackaddr$prim47799 = alloca %struct.ScmObj*, align 8
%current_45args46633 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46632)
store volatile %struct.ScmObj* %current_45args46633, %struct.ScmObj** %stackaddr$prim47799, align 8
%stackaddr$prim47800 = alloca %struct.ScmObj*, align 8
%a40205 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46633)
store volatile %struct.ScmObj* %a40205, %struct.ScmObj** %stackaddr$prim47800, align 8
%stackaddr$prim47801 = alloca %struct.ScmObj*, align 8
%current_45args46634 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46633)
store volatile %struct.ScmObj* %current_45args46634, %struct.ScmObj** %stackaddr$prim47801, align 8
%stackaddr$prim47802 = alloca %struct.ScmObj*, align 8
%b40204 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46634)
store volatile %struct.ScmObj* %b40204, %struct.ScmObj** %stackaddr$prim47802, align 8
%stackaddr$prim47803 = alloca %struct.ScmObj*, align 8
%anf_45bind40296 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a40205, %struct.ScmObj* %b40204)
store volatile %struct.ScmObj* %anf_45bind40296, %struct.ScmObj** %stackaddr$prim47803, align 8
%stackaddr$prim47804 = alloca %struct.ScmObj*, align 8
%cpsprim40428 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind40296)
store volatile %struct.ScmObj* %cpsprim40428, %struct.ScmObj** %stackaddr$prim47804, align 8
%ae42237 = call %struct.ScmObj* @const_init_int(i64 0)
%args46636$k40427$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47805 = alloca %struct.ScmObj*, align 8
%args46636$k40427$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40428, %struct.ScmObj* %args46636$k40427$0)
store volatile %struct.ScmObj* %args46636$k40427$1, %struct.ScmObj** %stackaddr$prim47805, align 8
%stackaddr$prim47806 = alloca %struct.ScmObj*, align 8
%args46636$k40427$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42237, %struct.ScmObj* %args46636$k40427$1)
store volatile %struct.ScmObj* %args46636$k40427$2, %struct.ScmObj** %stackaddr$prim47806, align 8
%clofunc47807 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40427)
musttail call tailcc void %clofunc47807(%struct.ScmObj* %k40427, %struct.ScmObj* %args46636$k40427$2)
ret void
}

define tailcc void @proc_clo$ae41838(%struct.ScmObj* %env$ae41838,%struct.ScmObj* %current_45args46639) {
%stackaddr$env-ref47808 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41838, i64 0)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref47808
%stackaddr$env-ref47809 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41838, i64 1)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref47809
%stackaddr$env-ref47810 = alloca %struct.ScmObj*, align 8
%_37map140155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41838, i64 2)
store %struct.ScmObj* %_37map140155, %struct.ScmObj** %stackaddr$env-ref47810
%stackaddr$prim47811 = alloca %struct.ScmObj*, align 8
%k40429 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46639)
store volatile %struct.ScmObj* %k40429, %struct.ScmObj** %stackaddr$prim47811, align 8
%stackaddr$prim47812 = alloca %struct.ScmObj*, align 8
%current_45args46640 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46639)
store volatile %struct.ScmObj* %current_45args46640, %struct.ScmObj** %stackaddr$prim47812, align 8
%stackaddr$prim47813 = alloca %struct.ScmObj*, align 8
%_37foldl40207 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46640)
store volatile %struct.ScmObj* %_37foldl40207, %struct.ScmObj** %stackaddr$prim47813, align 8
%ae41840 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47814 = alloca %struct.ScmObj*, align 8
%fptrToInt47815 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41841 to i64
%ae41841 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt47815)
store volatile %struct.ScmObj* %ae41841, %struct.ScmObj** %stackaddr$makeclosure47814, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41841, %struct.ScmObj* %_37foldr40129, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41841, %struct.ScmObj* %_37foldl40207, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41841, %struct.ScmObj* %_37foldr140124, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41841, %struct.ScmObj* %_37map140155, i64 3)
%args46697$k40429$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47816 = alloca %struct.ScmObj*, align 8
%args46697$k40429$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41841, %struct.ScmObj* %args46697$k40429$0)
store volatile %struct.ScmObj* %args46697$k40429$1, %struct.ScmObj** %stackaddr$prim47816, align 8
%stackaddr$prim47817 = alloca %struct.ScmObj*, align 8
%args46697$k40429$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41840, %struct.ScmObj* %args46697$k40429$1)
store volatile %struct.ScmObj* %args46697$k40429$2, %struct.ScmObj** %stackaddr$prim47817, align 8
%clofunc47818 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40429)
musttail call tailcc void %clofunc47818(%struct.ScmObj* %k40429, %struct.ScmObj* %args46697$k40429$2)
ret void
}

define tailcc void @proc_clo$ae41841(%struct.ScmObj* %env$ae41841,%struct.ScmObj* %args4020840430) {
%stackaddr$env-ref47819 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41841, i64 0)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref47819
%stackaddr$env-ref47820 = alloca %struct.ScmObj*, align 8
%_37foldl40207 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41841, i64 1)
store %struct.ScmObj* %_37foldl40207, %struct.ScmObj** %stackaddr$env-ref47820
%stackaddr$env-ref47821 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41841, i64 2)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref47821
%stackaddr$env-ref47822 = alloca %struct.ScmObj*, align 8
%_37map140155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41841, i64 3)
store %struct.ScmObj* %_37map140155, %struct.ScmObj** %stackaddr$env-ref47822
%stackaddr$prim47823 = alloca %struct.ScmObj*, align 8
%k40431 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4020840430)
store volatile %struct.ScmObj* %k40431, %struct.ScmObj** %stackaddr$prim47823, align 8
%stackaddr$prim47824 = alloca %struct.ScmObj*, align 8
%args40208 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4020840430)
store volatile %struct.ScmObj* %args40208, %struct.ScmObj** %stackaddr$prim47824, align 8
%stackaddr$prim47825 = alloca %struct.ScmObj*, align 8
%f40211 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40208)
store volatile %struct.ScmObj* %f40211, %struct.ScmObj** %stackaddr$prim47825, align 8
%stackaddr$prim47826 = alloca %struct.ScmObj*, align 8
%anf_45bind40284 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40208)
store volatile %struct.ScmObj* %anf_45bind40284, %struct.ScmObj** %stackaddr$prim47826, align 8
%stackaddr$prim47827 = alloca %struct.ScmObj*, align 8
%acc40210 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40284)
store volatile %struct.ScmObj* %acc40210, %struct.ScmObj** %stackaddr$prim47827, align 8
%stackaddr$prim47828 = alloca %struct.ScmObj*, align 8
%anf_45bind40285 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40208)
store volatile %struct.ScmObj* %anf_45bind40285, %struct.ScmObj** %stackaddr$prim47828, align 8
%stackaddr$prim47829 = alloca %struct.ScmObj*, align 8
%lsts40209 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40285)
store volatile %struct.ScmObj* %lsts40209, %struct.ScmObj** %stackaddr$prim47829, align 8
%stackaddr$makeclosure47830 = alloca %struct.ScmObj*, align 8
%fptrToInt47831 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41849 to i64
%ae41849 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt47831)
store volatile %struct.ScmObj* %ae41849, %struct.ScmObj** %stackaddr$makeclosure47830, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41849, %struct.ScmObj* %lsts40209, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41849, %struct.ScmObj* %_37foldr40129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41849, %struct.ScmObj* %_37foldl40207, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41849, %struct.ScmObj* %k40431, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41849, %struct.ScmObj* %f40211, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41849, %struct.ScmObj* %acc40210, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41849, %struct.ScmObj* %_37foldr140124, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae41849, %struct.ScmObj* %_37map140155, i64 7)
%ae41850 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47832 = alloca %struct.ScmObj*, align 8
%fptrToInt47833 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41851 to i64
%ae41851 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47833)
store volatile %struct.ScmObj* %ae41851, %struct.ScmObj** %stackaddr$makeclosure47832, align 8
%args46696$ae41849$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47834 = alloca %struct.ScmObj*, align 8
%args46696$ae41849$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41851, %struct.ScmObj* %args46696$ae41849$0)
store volatile %struct.ScmObj* %args46696$ae41849$1, %struct.ScmObj** %stackaddr$prim47834, align 8
%stackaddr$prim47835 = alloca %struct.ScmObj*, align 8
%args46696$ae41849$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41850, %struct.ScmObj* %args46696$ae41849$1)
store volatile %struct.ScmObj* %args46696$ae41849$2, %struct.ScmObj** %stackaddr$prim47835, align 8
%clofunc47836 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41849)
musttail call tailcc void %clofunc47836(%struct.ScmObj* %ae41849, %struct.ScmObj* %args46696$ae41849$2)
ret void
}

define tailcc void @proc_clo$ae41849(%struct.ScmObj* %env$ae41849,%struct.ScmObj* %current_45args46642) {
%stackaddr$env-ref47837 = alloca %struct.ScmObj*, align 8
%lsts40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41849, i64 0)
store %struct.ScmObj* %lsts40209, %struct.ScmObj** %stackaddr$env-ref47837
%stackaddr$env-ref47838 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41849, i64 1)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref47838
%stackaddr$env-ref47839 = alloca %struct.ScmObj*, align 8
%_37foldl40207 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41849, i64 2)
store %struct.ScmObj* %_37foldl40207, %struct.ScmObj** %stackaddr$env-ref47839
%stackaddr$env-ref47840 = alloca %struct.ScmObj*, align 8
%k40431 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41849, i64 3)
store %struct.ScmObj* %k40431, %struct.ScmObj** %stackaddr$env-ref47840
%stackaddr$env-ref47841 = alloca %struct.ScmObj*, align 8
%f40211 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41849, i64 4)
store %struct.ScmObj* %f40211, %struct.ScmObj** %stackaddr$env-ref47841
%stackaddr$env-ref47842 = alloca %struct.ScmObj*, align 8
%acc40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41849, i64 5)
store %struct.ScmObj* %acc40210, %struct.ScmObj** %stackaddr$env-ref47842
%stackaddr$env-ref47843 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41849, i64 6)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref47843
%stackaddr$env-ref47844 = alloca %struct.ScmObj*, align 8
%_37map140155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41849, i64 7)
store %struct.ScmObj* %_37map140155, %struct.ScmObj** %stackaddr$env-ref47844
%stackaddr$prim47845 = alloca %struct.ScmObj*, align 8
%_95k40432 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46642)
store volatile %struct.ScmObj* %_95k40432, %struct.ScmObj** %stackaddr$prim47845, align 8
%stackaddr$prim47846 = alloca %struct.ScmObj*, align 8
%current_45args46643 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46642)
store volatile %struct.ScmObj* %current_45args46643, %struct.ScmObj** %stackaddr$prim47846, align 8
%stackaddr$prim47847 = alloca %struct.ScmObj*, align 8
%anf_45bind40286 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46643)
store volatile %struct.ScmObj* %anf_45bind40286, %struct.ScmObj** %stackaddr$prim47847, align 8
%stackaddr$makeclosure47848 = alloca %struct.ScmObj*, align 8
%fptrToInt47849 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41881 to i64
%ae41881 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt47849)
store volatile %struct.ScmObj* %ae41881, %struct.ScmObj** %stackaddr$makeclosure47848, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41881, %struct.ScmObj* %lsts40209, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41881, %struct.ScmObj* %_37foldr40129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41881, %struct.ScmObj* %_37foldl40207, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41881, %struct.ScmObj* %k40431, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41881, %struct.ScmObj* %f40211, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41881, %struct.ScmObj* %acc40210, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41881, %struct.ScmObj* %_37map140155, i64 6)
%ae41883 = call %struct.ScmObj* @const_init_false()
%args46689$_37foldr140124$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47850 = alloca %struct.ScmObj*, align 8
%args46689$_37foldr140124$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40209, %struct.ScmObj* %args46689$_37foldr140124$0)
store volatile %struct.ScmObj* %args46689$_37foldr140124$1, %struct.ScmObj** %stackaddr$prim47850, align 8
%stackaddr$prim47851 = alloca %struct.ScmObj*, align 8
%args46689$_37foldr140124$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41883, %struct.ScmObj* %args46689$_37foldr140124$1)
store volatile %struct.ScmObj* %args46689$_37foldr140124$2, %struct.ScmObj** %stackaddr$prim47851, align 8
%stackaddr$prim47852 = alloca %struct.ScmObj*, align 8
%args46689$_37foldr140124$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40286, %struct.ScmObj* %args46689$_37foldr140124$2)
store volatile %struct.ScmObj* %args46689$_37foldr140124$3, %struct.ScmObj** %stackaddr$prim47852, align 8
%stackaddr$prim47853 = alloca %struct.ScmObj*, align 8
%args46689$_37foldr140124$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41881, %struct.ScmObj* %args46689$_37foldr140124$3)
store volatile %struct.ScmObj* %args46689$_37foldr140124$4, %struct.ScmObj** %stackaddr$prim47853, align 8
%clofunc47854 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140124)
musttail call tailcc void %clofunc47854(%struct.ScmObj* %_37foldr140124, %struct.ScmObj* %args46689$_37foldr140124$4)
ret void
}

define tailcc void @proc_clo$ae41881(%struct.ScmObj* %env$ae41881,%struct.ScmObj* %current_45args46645) {
%stackaddr$env-ref47855 = alloca %struct.ScmObj*, align 8
%lsts40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41881, i64 0)
store %struct.ScmObj* %lsts40209, %struct.ScmObj** %stackaddr$env-ref47855
%stackaddr$env-ref47856 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41881, i64 1)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref47856
%stackaddr$env-ref47857 = alloca %struct.ScmObj*, align 8
%_37foldl40207 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41881, i64 2)
store %struct.ScmObj* %_37foldl40207, %struct.ScmObj** %stackaddr$env-ref47857
%stackaddr$env-ref47858 = alloca %struct.ScmObj*, align 8
%k40431 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41881, i64 3)
store %struct.ScmObj* %k40431, %struct.ScmObj** %stackaddr$env-ref47858
%stackaddr$env-ref47859 = alloca %struct.ScmObj*, align 8
%f40211 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41881, i64 4)
store %struct.ScmObj* %f40211, %struct.ScmObj** %stackaddr$env-ref47859
%stackaddr$env-ref47860 = alloca %struct.ScmObj*, align 8
%acc40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41881, i64 5)
store %struct.ScmObj* %acc40210, %struct.ScmObj** %stackaddr$env-ref47860
%stackaddr$env-ref47861 = alloca %struct.ScmObj*, align 8
%_37map140155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41881, i64 6)
store %struct.ScmObj* %_37map140155, %struct.ScmObj** %stackaddr$env-ref47861
%stackaddr$prim47862 = alloca %struct.ScmObj*, align 8
%_95k40433 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46645)
store volatile %struct.ScmObj* %_95k40433, %struct.ScmObj** %stackaddr$prim47862, align 8
%stackaddr$prim47863 = alloca %struct.ScmObj*, align 8
%current_45args46646 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46645)
store volatile %struct.ScmObj* %current_45args46646, %struct.ScmObj** %stackaddr$prim47863, align 8
%stackaddr$prim47864 = alloca %struct.ScmObj*, align 8
%anf_45bind40287 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46646)
store volatile %struct.ScmObj* %anf_45bind40287, %struct.ScmObj** %stackaddr$prim47864, align 8
%truthy$cmp47865 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40287)
%cmp$cmp47865 = icmp eq i64 %truthy$cmp47865, 1
br i1 %cmp$cmp47865, label %truebranch$cmp47865, label %falsebranch$cmp47865
truebranch$cmp47865:
%ae41892 = call %struct.ScmObj* @const_init_int(i64 0)
%args46648$k40431$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47866 = alloca %struct.ScmObj*, align 8
%args46648$k40431$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40210, %struct.ScmObj* %args46648$k40431$0)
store volatile %struct.ScmObj* %args46648$k40431$1, %struct.ScmObj** %stackaddr$prim47866, align 8
%stackaddr$prim47867 = alloca %struct.ScmObj*, align 8
%args46648$k40431$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41892, %struct.ScmObj* %args46648$k40431$1)
store volatile %struct.ScmObj* %args46648$k40431$2, %struct.ScmObj** %stackaddr$prim47867, align 8
%clofunc47868 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40431)
musttail call tailcc void %clofunc47868(%struct.ScmObj* %k40431, %struct.ScmObj* %args46648$k40431$2)
ret void
falsebranch$cmp47865:
%stackaddr$makeclosure47869 = alloca %struct.ScmObj*, align 8
%fptrToInt47870 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41897 to i64
%ae41897 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt47870)
store volatile %struct.ScmObj* %ae41897, %struct.ScmObj** %stackaddr$makeclosure47869, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41897, %struct.ScmObj* %lsts40209, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41897, %struct.ScmObj* %_37foldr40129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41897, %struct.ScmObj* %_37foldl40207, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41897, %struct.ScmObj* %k40431, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41897, %struct.ScmObj* %f40211, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41897, %struct.ScmObj* %acc40210, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41897, %struct.ScmObj* %_37map140155, i64 6)
%ae41898 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47871 = alloca %struct.ScmObj*, align 8
%fptrToInt47872 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41899 to i64
%ae41899 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47872)
store volatile %struct.ScmObj* %ae41899, %struct.ScmObj** %stackaddr$makeclosure47871, align 8
%args46688$ae41897$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47873 = alloca %struct.ScmObj*, align 8
%args46688$ae41897$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41899, %struct.ScmObj* %args46688$ae41897$0)
store volatile %struct.ScmObj* %args46688$ae41897$1, %struct.ScmObj** %stackaddr$prim47873, align 8
%stackaddr$prim47874 = alloca %struct.ScmObj*, align 8
%args46688$ae41897$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41898, %struct.ScmObj* %args46688$ae41897$1)
store volatile %struct.ScmObj* %args46688$ae41897$2, %struct.ScmObj** %stackaddr$prim47874, align 8
%clofunc47875 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41897)
musttail call tailcc void %clofunc47875(%struct.ScmObj* %ae41897, %struct.ScmObj* %args46688$ae41897$2)
ret void
}

define tailcc void @proc_clo$ae41897(%struct.ScmObj* %env$ae41897,%struct.ScmObj* %current_45args46649) {
%stackaddr$env-ref47876 = alloca %struct.ScmObj*, align 8
%lsts40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41897, i64 0)
store %struct.ScmObj* %lsts40209, %struct.ScmObj** %stackaddr$env-ref47876
%stackaddr$env-ref47877 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41897, i64 1)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref47877
%stackaddr$env-ref47878 = alloca %struct.ScmObj*, align 8
%_37foldl40207 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41897, i64 2)
store %struct.ScmObj* %_37foldl40207, %struct.ScmObj** %stackaddr$env-ref47878
%stackaddr$env-ref47879 = alloca %struct.ScmObj*, align 8
%k40431 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41897, i64 3)
store %struct.ScmObj* %k40431, %struct.ScmObj** %stackaddr$env-ref47879
%stackaddr$env-ref47880 = alloca %struct.ScmObj*, align 8
%f40211 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41897, i64 4)
store %struct.ScmObj* %f40211, %struct.ScmObj** %stackaddr$env-ref47880
%stackaddr$env-ref47881 = alloca %struct.ScmObj*, align 8
%acc40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41897, i64 5)
store %struct.ScmObj* %acc40210, %struct.ScmObj** %stackaddr$env-ref47881
%stackaddr$env-ref47882 = alloca %struct.ScmObj*, align 8
%_37map140155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41897, i64 6)
store %struct.ScmObj* %_37map140155, %struct.ScmObj** %stackaddr$env-ref47882
%stackaddr$prim47883 = alloca %struct.ScmObj*, align 8
%_95k40434 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46649)
store volatile %struct.ScmObj* %_95k40434, %struct.ScmObj** %stackaddr$prim47883, align 8
%stackaddr$prim47884 = alloca %struct.ScmObj*, align 8
%current_45args46650 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46649)
store volatile %struct.ScmObj* %current_45args46650, %struct.ScmObj** %stackaddr$prim47884, align 8
%stackaddr$prim47885 = alloca %struct.ScmObj*, align 8
%anf_45bind40288 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46650)
store volatile %struct.ScmObj* %anf_45bind40288, %struct.ScmObj** %stackaddr$prim47885, align 8
%stackaddr$makeclosure47886 = alloca %struct.ScmObj*, align 8
%fptrToInt47887 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41918 to i64
%ae41918 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt47887)
store volatile %struct.ScmObj* %ae41918, %struct.ScmObj** %stackaddr$makeclosure47886, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41918, %struct.ScmObj* %lsts40209, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41918, %struct.ScmObj* %_37foldr40129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41918, %struct.ScmObj* %_37foldl40207, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41918, %struct.ScmObj* %k40431, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41918, %struct.ScmObj* %f40211, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41918, %struct.ScmObj* %acc40210, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41918, %struct.ScmObj* %_37map140155, i64 6)
%args46683$_37map140155$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47888 = alloca %struct.ScmObj*, align 8
%args46683$_37map140155$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40209, %struct.ScmObj* %args46683$_37map140155$0)
store volatile %struct.ScmObj* %args46683$_37map140155$1, %struct.ScmObj** %stackaddr$prim47888, align 8
%stackaddr$prim47889 = alloca %struct.ScmObj*, align 8
%args46683$_37map140155$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40288, %struct.ScmObj* %args46683$_37map140155$1)
store volatile %struct.ScmObj* %args46683$_37map140155$2, %struct.ScmObj** %stackaddr$prim47889, align 8
%stackaddr$prim47890 = alloca %struct.ScmObj*, align 8
%args46683$_37map140155$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41918, %struct.ScmObj* %args46683$_37map140155$2)
store volatile %struct.ScmObj* %args46683$_37map140155$3, %struct.ScmObj** %stackaddr$prim47890, align 8
%clofunc47891 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140155)
musttail call tailcc void %clofunc47891(%struct.ScmObj* %_37map140155, %struct.ScmObj* %args46683$_37map140155$3)
ret void
}

define tailcc void @proc_clo$ae41918(%struct.ScmObj* %env$ae41918,%struct.ScmObj* %current_45args46652) {
%stackaddr$env-ref47892 = alloca %struct.ScmObj*, align 8
%lsts40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41918, i64 0)
store %struct.ScmObj* %lsts40209, %struct.ScmObj** %stackaddr$env-ref47892
%stackaddr$env-ref47893 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41918, i64 1)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref47893
%stackaddr$env-ref47894 = alloca %struct.ScmObj*, align 8
%_37foldl40207 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41918, i64 2)
store %struct.ScmObj* %_37foldl40207, %struct.ScmObj** %stackaddr$env-ref47894
%stackaddr$env-ref47895 = alloca %struct.ScmObj*, align 8
%k40431 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41918, i64 3)
store %struct.ScmObj* %k40431, %struct.ScmObj** %stackaddr$env-ref47895
%stackaddr$env-ref47896 = alloca %struct.ScmObj*, align 8
%f40211 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41918, i64 4)
store %struct.ScmObj* %f40211, %struct.ScmObj** %stackaddr$env-ref47896
%stackaddr$env-ref47897 = alloca %struct.ScmObj*, align 8
%acc40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41918, i64 5)
store %struct.ScmObj* %acc40210, %struct.ScmObj** %stackaddr$env-ref47897
%stackaddr$env-ref47898 = alloca %struct.ScmObj*, align 8
%_37map140155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41918, i64 6)
store %struct.ScmObj* %_37map140155, %struct.ScmObj** %stackaddr$env-ref47898
%stackaddr$prim47899 = alloca %struct.ScmObj*, align 8
%_95k40435 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46652)
store volatile %struct.ScmObj* %_95k40435, %struct.ScmObj** %stackaddr$prim47899, align 8
%stackaddr$prim47900 = alloca %struct.ScmObj*, align 8
%current_45args46653 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46652)
store volatile %struct.ScmObj* %current_45args46653, %struct.ScmObj** %stackaddr$prim47900, align 8
%stackaddr$prim47901 = alloca %struct.ScmObj*, align 8
%lsts_4340216 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46653)
store volatile %struct.ScmObj* %lsts_4340216, %struct.ScmObj** %stackaddr$prim47901, align 8
%stackaddr$makeclosure47902 = alloca %struct.ScmObj*, align 8
%fptrToInt47903 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41921 to i64
%ae41921 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt47903)
store volatile %struct.ScmObj* %ae41921, %struct.ScmObj** %stackaddr$makeclosure47902, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41921, %struct.ScmObj* %lsts40209, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41921, %struct.ScmObj* %_37foldr40129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41921, %struct.ScmObj* %_37foldl40207, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41921, %struct.ScmObj* %k40431, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41921, %struct.ScmObj* %lsts_4340216, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41921, %struct.ScmObj* %f40211, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41921, %struct.ScmObj* %acc40210, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae41921, %struct.ScmObj* %_37map140155, i64 7)
%ae41922 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47904 = alloca %struct.ScmObj*, align 8
%fptrToInt47905 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41923 to i64
%ae41923 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47905)
store volatile %struct.ScmObj* %ae41923, %struct.ScmObj** %stackaddr$makeclosure47904, align 8
%args46682$ae41921$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47906 = alloca %struct.ScmObj*, align 8
%args46682$ae41921$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41923, %struct.ScmObj* %args46682$ae41921$0)
store volatile %struct.ScmObj* %args46682$ae41921$1, %struct.ScmObj** %stackaddr$prim47906, align 8
%stackaddr$prim47907 = alloca %struct.ScmObj*, align 8
%args46682$ae41921$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41922, %struct.ScmObj* %args46682$ae41921$1)
store volatile %struct.ScmObj* %args46682$ae41921$2, %struct.ScmObj** %stackaddr$prim47907, align 8
%clofunc47908 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41921)
musttail call tailcc void %clofunc47908(%struct.ScmObj* %ae41921, %struct.ScmObj* %args46682$ae41921$2)
ret void
}

define tailcc void @proc_clo$ae41921(%struct.ScmObj* %env$ae41921,%struct.ScmObj* %current_45args46655) {
%stackaddr$env-ref47909 = alloca %struct.ScmObj*, align 8
%lsts40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41921, i64 0)
store %struct.ScmObj* %lsts40209, %struct.ScmObj** %stackaddr$env-ref47909
%stackaddr$env-ref47910 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41921, i64 1)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref47910
%stackaddr$env-ref47911 = alloca %struct.ScmObj*, align 8
%_37foldl40207 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41921, i64 2)
store %struct.ScmObj* %_37foldl40207, %struct.ScmObj** %stackaddr$env-ref47911
%stackaddr$env-ref47912 = alloca %struct.ScmObj*, align 8
%k40431 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41921, i64 3)
store %struct.ScmObj* %k40431, %struct.ScmObj** %stackaddr$env-ref47912
%stackaddr$env-ref47913 = alloca %struct.ScmObj*, align 8
%lsts_4340216 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41921, i64 4)
store %struct.ScmObj* %lsts_4340216, %struct.ScmObj** %stackaddr$env-ref47913
%stackaddr$env-ref47914 = alloca %struct.ScmObj*, align 8
%f40211 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41921, i64 5)
store %struct.ScmObj* %f40211, %struct.ScmObj** %stackaddr$env-ref47914
%stackaddr$env-ref47915 = alloca %struct.ScmObj*, align 8
%acc40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41921, i64 6)
store %struct.ScmObj* %acc40210, %struct.ScmObj** %stackaddr$env-ref47915
%stackaddr$env-ref47916 = alloca %struct.ScmObj*, align 8
%_37map140155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41921, i64 7)
store %struct.ScmObj* %_37map140155, %struct.ScmObj** %stackaddr$env-ref47916
%stackaddr$prim47917 = alloca %struct.ScmObj*, align 8
%_95k40436 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46655)
store volatile %struct.ScmObj* %_95k40436, %struct.ScmObj** %stackaddr$prim47917, align 8
%stackaddr$prim47918 = alloca %struct.ScmObj*, align 8
%current_45args46656 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46655)
store volatile %struct.ScmObj* %current_45args46656, %struct.ScmObj** %stackaddr$prim47918, align 8
%stackaddr$prim47919 = alloca %struct.ScmObj*, align 8
%anf_45bind40289 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46656)
store volatile %struct.ScmObj* %anf_45bind40289, %struct.ScmObj** %stackaddr$prim47919, align 8
%stackaddr$makeclosure47920 = alloca %struct.ScmObj*, align 8
%fptrToInt47921 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41942 to i64
%ae41942 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt47921)
store volatile %struct.ScmObj* %ae41942, %struct.ScmObj** %stackaddr$makeclosure47920, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41942, %struct.ScmObj* %_37foldl40207, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41942, %struct.ScmObj* %k40431, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41942, %struct.ScmObj* %lsts_4340216, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41942, %struct.ScmObj* %f40211, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41942, %struct.ScmObj* %acc40210, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41942, %struct.ScmObj* %_37foldr40129, i64 5)
%args46677$_37map140155$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47922 = alloca %struct.ScmObj*, align 8
%args46677$_37map140155$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40209, %struct.ScmObj* %args46677$_37map140155$0)
store volatile %struct.ScmObj* %args46677$_37map140155$1, %struct.ScmObj** %stackaddr$prim47922, align 8
%stackaddr$prim47923 = alloca %struct.ScmObj*, align 8
%args46677$_37map140155$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40289, %struct.ScmObj* %args46677$_37map140155$1)
store volatile %struct.ScmObj* %args46677$_37map140155$2, %struct.ScmObj** %stackaddr$prim47923, align 8
%stackaddr$prim47924 = alloca %struct.ScmObj*, align 8
%args46677$_37map140155$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41942, %struct.ScmObj* %args46677$_37map140155$2)
store volatile %struct.ScmObj* %args46677$_37map140155$3, %struct.ScmObj** %stackaddr$prim47924, align 8
%clofunc47925 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140155)
musttail call tailcc void %clofunc47925(%struct.ScmObj* %_37map140155, %struct.ScmObj* %args46677$_37map140155$3)
ret void
}

define tailcc void @proc_clo$ae41942(%struct.ScmObj* %env$ae41942,%struct.ScmObj* %current_45args46658) {
%stackaddr$env-ref47926 = alloca %struct.ScmObj*, align 8
%_37foldl40207 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41942, i64 0)
store %struct.ScmObj* %_37foldl40207, %struct.ScmObj** %stackaddr$env-ref47926
%stackaddr$env-ref47927 = alloca %struct.ScmObj*, align 8
%k40431 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41942, i64 1)
store %struct.ScmObj* %k40431, %struct.ScmObj** %stackaddr$env-ref47927
%stackaddr$env-ref47928 = alloca %struct.ScmObj*, align 8
%lsts_4340216 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41942, i64 2)
store %struct.ScmObj* %lsts_4340216, %struct.ScmObj** %stackaddr$env-ref47928
%stackaddr$env-ref47929 = alloca %struct.ScmObj*, align 8
%f40211 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41942, i64 3)
store %struct.ScmObj* %f40211, %struct.ScmObj** %stackaddr$env-ref47929
%stackaddr$env-ref47930 = alloca %struct.ScmObj*, align 8
%acc40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41942, i64 4)
store %struct.ScmObj* %acc40210, %struct.ScmObj** %stackaddr$env-ref47930
%stackaddr$env-ref47931 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41942, i64 5)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref47931
%stackaddr$prim47932 = alloca %struct.ScmObj*, align 8
%_95k40437 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46658)
store volatile %struct.ScmObj* %_95k40437, %struct.ScmObj** %stackaddr$prim47932, align 8
%stackaddr$prim47933 = alloca %struct.ScmObj*, align 8
%current_45args46659 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46658)
store volatile %struct.ScmObj* %current_45args46659, %struct.ScmObj** %stackaddr$prim47933, align 8
%stackaddr$prim47934 = alloca %struct.ScmObj*, align 8
%vs40214 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46659)
store volatile %struct.ScmObj* %vs40214, %struct.ScmObj** %stackaddr$prim47934, align 8
%stackaddr$makeclosure47935 = alloca %struct.ScmObj*, align 8
%fptrToInt47936 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41945 to i64
%ae41945 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt47936)
store volatile %struct.ScmObj* %ae41945, %struct.ScmObj** %stackaddr$makeclosure47935, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41945, %struct.ScmObj* %_37foldl40207, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41945, %struct.ScmObj* %k40431, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41945, %struct.ScmObj* %lsts_4340216, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41945, %struct.ScmObj* %vs40214, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41945, %struct.ScmObj* %f40211, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41945, %struct.ScmObj* %acc40210, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41945, %struct.ScmObj* %_37foldr40129, i64 6)
%ae41946 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47937 = alloca %struct.ScmObj*, align 8
%fptrToInt47938 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41947 to i64
%ae41947 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47938)
store volatile %struct.ScmObj* %ae41947, %struct.ScmObj** %stackaddr$makeclosure47937, align 8
%args46676$ae41945$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47939 = alloca %struct.ScmObj*, align 8
%args46676$ae41945$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41947, %struct.ScmObj* %args46676$ae41945$0)
store volatile %struct.ScmObj* %args46676$ae41945$1, %struct.ScmObj** %stackaddr$prim47939, align 8
%stackaddr$prim47940 = alloca %struct.ScmObj*, align 8
%args46676$ae41945$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41946, %struct.ScmObj* %args46676$ae41945$1)
store volatile %struct.ScmObj* %args46676$ae41945$2, %struct.ScmObj** %stackaddr$prim47940, align 8
%clofunc47941 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41945)
musttail call tailcc void %clofunc47941(%struct.ScmObj* %ae41945, %struct.ScmObj* %args46676$ae41945$2)
ret void
}

define tailcc void @proc_clo$ae41945(%struct.ScmObj* %env$ae41945,%struct.ScmObj* %current_45args46661) {
%stackaddr$env-ref47942 = alloca %struct.ScmObj*, align 8
%_37foldl40207 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41945, i64 0)
store %struct.ScmObj* %_37foldl40207, %struct.ScmObj** %stackaddr$env-ref47942
%stackaddr$env-ref47943 = alloca %struct.ScmObj*, align 8
%k40431 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41945, i64 1)
store %struct.ScmObj* %k40431, %struct.ScmObj** %stackaddr$env-ref47943
%stackaddr$env-ref47944 = alloca %struct.ScmObj*, align 8
%lsts_4340216 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41945, i64 2)
store %struct.ScmObj* %lsts_4340216, %struct.ScmObj** %stackaddr$env-ref47944
%stackaddr$env-ref47945 = alloca %struct.ScmObj*, align 8
%vs40214 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41945, i64 3)
store %struct.ScmObj* %vs40214, %struct.ScmObj** %stackaddr$env-ref47945
%stackaddr$env-ref47946 = alloca %struct.ScmObj*, align 8
%f40211 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41945, i64 4)
store %struct.ScmObj* %f40211, %struct.ScmObj** %stackaddr$env-ref47946
%stackaddr$env-ref47947 = alloca %struct.ScmObj*, align 8
%acc40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41945, i64 5)
store %struct.ScmObj* %acc40210, %struct.ScmObj** %stackaddr$env-ref47947
%stackaddr$env-ref47948 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41945, i64 6)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref47948
%stackaddr$prim47949 = alloca %struct.ScmObj*, align 8
%_95k40438 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46661)
store volatile %struct.ScmObj* %_95k40438, %struct.ScmObj** %stackaddr$prim47949, align 8
%stackaddr$prim47950 = alloca %struct.ScmObj*, align 8
%current_45args46662 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46661)
store volatile %struct.ScmObj* %current_45args46662, %struct.ScmObj** %stackaddr$prim47950, align 8
%stackaddr$prim47951 = alloca %struct.ScmObj*, align 8
%anf_45bind40290 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46662)
store volatile %struct.ScmObj* %anf_45bind40290, %struct.ScmObj** %stackaddr$prim47951, align 8
%ae41968 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47952 = alloca %struct.ScmObj*, align 8
%anf_45bind40291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40210, %struct.ScmObj* %ae41968)
store volatile %struct.ScmObj* %anf_45bind40291, %struct.ScmObj** %stackaddr$prim47952, align 8
%stackaddr$makeclosure47953 = alloca %struct.ScmObj*, align 8
%fptrToInt47954 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41970 to i64
%ae41970 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt47954)
store volatile %struct.ScmObj* %ae41970, %struct.ScmObj** %stackaddr$makeclosure47953, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41970, %struct.ScmObj* %_37foldl40207, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41970, %struct.ScmObj* %k40431, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41970, %struct.ScmObj* %lsts_4340216, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41970, %struct.ScmObj* %f40211, i64 3)
%args46670$_37foldr40129$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47955 = alloca %struct.ScmObj*, align 8
%args46670$_37foldr40129$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs40214, %struct.ScmObj* %args46670$_37foldr40129$0)
store volatile %struct.ScmObj* %args46670$_37foldr40129$1, %struct.ScmObj** %stackaddr$prim47955, align 8
%stackaddr$prim47956 = alloca %struct.ScmObj*, align 8
%args46670$_37foldr40129$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40291, %struct.ScmObj* %args46670$_37foldr40129$1)
store volatile %struct.ScmObj* %args46670$_37foldr40129$2, %struct.ScmObj** %stackaddr$prim47956, align 8
%stackaddr$prim47957 = alloca %struct.ScmObj*, align 8
%args46670$_37foldr40129$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40290, %struct.ScmObj* %args46670$_37foldr40129$2)
store volatile %struct.ScmObj* %args46670$_37foldr40129$3, %struct.ScmObj** %stackaddr$prim47957, align 8
%stackaddr$prim47958 = alloca %struct.ScmObj*, align 8
%args46670$_37foldr40129$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41970, %struct.ScmObj* %args46670$_37foldr40129$3)
store volatile %struct.ScmObj* %args46670$_37foldr40129$4, %struct.ScmObj** %stackaddr$prim47958, align 8
%clofunc47959 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr40129)
musttail call tailcc void %clofunc47959(%struct.ScmObj* %_37foldr40129, %struct.ScmObj* %args46670$_37foldr40129$4)
ret void
}

define tailcc void @proc_clo$ae41970(%struct.ScmObj* %env$ae41970,%struct.ScmObj* %current_45args46664) {
%stackaddr$env-ref47960 = alloca %struct.ScmObj*, align 8
%_37foldl40207 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41970, i64 0)
store %struct.ScmObj* %_37foldl40207, %struct.ScmObj** %stackaddr$env-ref47960
%stackaddr$env-ref47961 = alloca %struct.ScmObj*, align 8
%k40431 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41970, i64 1)
store %struct.ScmObj* %k40431, %struct.ScmObj** %stackaddr$env-ref47961
%stackaddr$env-ref47962 = alloca %struct.ScmObj*, align 8
%lsts_4340216 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41970, i64 2)
store %struct.ScmObj* %lsts_4340216, %struct.ScmObj** %stackaddr$env-ref47962
%stackaddr$env-ref47963 = alloca %struct.ScmObj*, align 8
%f40211 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41970, i64 3)
store %struct.ScmObj* %f40211, %struct.ScmObj** %stackaddr$env-ref47963
%stackaddr$prim47964 = alloca %struct.ScmObj*, align 8
%_95k40439 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46664)
store volatile %struct.ScmObj* %_95k40439, %struct.ScmObj** %stackaddr$prim47964, align 8
%stackaddr$prim47965 = alloca %struct.ScmObj*, align 8
%current_45args46665 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46664)
store volatile %struct.ScmObj* %current_45args46665, %struct.ScmObj** %stackaddr$prim47965, align 8
%stackaddr$prim47966 = alloca %struct.ScmObj*, align 8
%anf_45bind40292 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46665)
store volatile %struct.ScmObj* %anf_45bind40292, %struct.ScmObj** %stackaddr$prim47966, align 8
%stackaddr$makeclosure47967 = alloca %struct.ScmObj*, align 8
%fptrToInt47968 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41974 to i64
%ae41974 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt47968)
store volatile %struct.ScmObj* %ae41974, %struct.ScmObj** %stackaddr$makeclosure47967, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41974, %struct.ScmObj* %_37foldl40207, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41974, %struct.ScmObj* %k40431, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41974, %struct.ScmObj* %lsts_4340216, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41974, %struct.ScmObj* %f40211, i64 3)
%stackaddr$prim47969 = alloca %struct.ScmObj*, align 8
%cpsargs40442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41974, %struct.ScmObj* %anf_45bind40292)
store volatile %struct.ScmObj* %cpsargs40442, %struct.ScmObj** %stackaddr$prim47969, align 8
%clofunc47970 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40211)
musttail call tailcc void %clofunc47970(%struct.ScmObj* %f40211, %struct.ScmObj* %cpsargs40442)
ret void
}

define tailcc void @proc_clo$ae41974(%struct.ScmObj* %env$ae41974,%struct.ScmObj* %current_45args46667) {
%stackaddr$env-ref47971 = alloca %struct.ScmObj*, align 8
%_37foldl40207 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41974, i64 0)
store %struct.ScmObj* %_37foldl40207, %struct.ScmObj** %stackaddr$env-ref47971
%stackaddr$env-ref47972 = alloca %struct.ScmObj*, align 8
%k40431 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41974, i64 1)
store %struct.ScmObj* %k40431, %struct.ScmObj** %stackaddr$env-ref47972
%stackaddr$env-ref47973 = alloca %struct.ScmObj*, align 8
%lsts_4340216 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41974, i64 2)
store %struct.ScmObj* %lsts_4340216, %struct.ScmObj** %stackaddr$env-ref47973
%stackaddr$env-ref47974 = alloca %struct.ScmObj*, align 8
%f40211 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41974, i64 3)
store %struct.ScmObj* %f40211, %struct.ScmObj** %stackaddr$env-ref47974
%stackaddr$prim47975 = alloca %struct.ScmObj*, align 8
%_95k40440 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46667)
store volatile %struct.ScmObj* %_95k40440, %struct.ScmObj** %stackaddr$prim47975, align 8
%stackaddr$prim47976 = alloca %struct.ScmObj*, align 8
%current_45args46668 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46667)
store volatile %struct.ScmObj* %current_45args46668, %struct.ScmObj** %stackaddr$prim47976, align 8
%stackaddr$prim47977 = alloca %struct.ScmObj*, align 8
%acc_4340218 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46668)
store volatile %struct.ScmObj* %acc_4340218, %struct.ScmObj** %stackaddr$prim47977, align 8
%stackaddr$prim47978 = alloca %struct.ScmObj*, align 8
%anf_45bind40293 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4340218, %struct.ScmObj* %lsts_4340216)
store volatile %struct.ScmObj* %anf_45bind40293, %struct.ScmObj** %stackaddr$prim47978, align 8
%stackaddr$prim47979 = alloca %struct.ScmObj*, align 8
%anf_45bind40294 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40211, %struct.ScmObj* %anf_45bind40293)
store volatile %struct.ScmObj* %anf_45bind40294, %struct.ScmObj** %stackaddr$prim47979, align 8
%stackaddr$prim47980 = alloca %struct.ScmObj*, align 8
%cpsargs40441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40431, %struct.ScmObj* %anf_45bind40294)
store volatile %struct.ScmObj* %cpsargs40441, %struct.ScmObj** %stackaddr$prim47980, align 8
%clofunc47981 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl40207)
musttail call tailcc void %clofunc47981(%struct.ScmObj* %_37foldl40207, %struct.ScmObj* %cpsargs40441)
ret void
}

define tailcc void @proc_clo$ae41947(%struct.ScmObj* %env$ae41947,%struct.ScmObj* %current_45args46671) {
%stackaddr$prim47982 = alloca %struct.ScmObj*, align 8
%k40443 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46671)
store volatile %struct.ScmObj* %k40443, %struct.ScmObj** %stackaddr$prim47982, align 8
%stackaddr$prim47983 = alloca %struct.ScmObj*, align 8
%current_45args46672 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46671)
store volatile %struct.ScmObj* %current_45args46672, %struct.ScmObj** %stackaddr$prim47983, align 8
%stackaddr$prim47984 = alloca %struct.ScmObj*, align 8
%a40220 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46672)
store volatile %struct.ScmObj* %a40220, %struct.ScmObj** %stackaddr$prim47984, align 8
%stackaddr$prim47985 = alloca %struct.ScmObj*, align 8
%current_45args46673 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46672)
store volatile %struct.ScmObj* %current_45args46673, %struct.ScmObj** %stackaddr$prim47985, align 8
%stackaddr$prim47986 = alloca %struct.ScmObj*, align 8
%b40219 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46673)
store volatile %struct.ScmObj* %b40219, %struct.ScmObj** %stackaddr$prim47986, align 8
%stackaddr$prim47987 = alloca %struct.ScmObj*, align 8
%cpsprim40444 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a40220, %struct.ScmObj* %b40219)
store volatile %struct.ScmObj* %cpsprim40444, %struct.ScmObj** %stackaddr$prim47987, align 8
%ae41951 = call %struct.ScmObj* @const_init_int(i64 0)
%args46675$k40443$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47988 = alloca %struct.ScmObj*, align 8
%args46675$k40443$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40444, %struct.ScmObj* %args46675$k40443$0)
store volatile %struct.ScmObj* %args46675$k40443$1, %struct.ScmObj** %stackaddr$prim47988, align 8
%stackaddr$prim47989 = alloca %struct.ScmObj*, align 8
%args46675$k40443$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41951, %struct.ScmObj* %args46675$k40443$1)
store volatile %struct.ScmObj* %args46675$k40443$2, %struct.ScmObj** %stackaddr$prim47989, align 8
%clofunc47990 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40443)
musttail call tailcc void %clofunc47990(%struct.ScmObj* %k40443, %struct.ScmObj* %args46675$k40443$2)
ret void
}

define tailcc void @proc_clo$ae41923(%struct.ScmObj* %env$ae41923,%struct.ScmObj* %current_45args46678) {
%stackaddr$prim47991 = alloca %struct.ScmObj*, align 8
%k40445 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46678)
store volatile %struct.ScmObj* %k40445, %struct.ScmObj** %stackaddr$prim47991, align 8
%stackaddr$prim47992 = alloca %struct.ScmObj*, align 8
%current_45args46679 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46678)
store volatile %struct.ScmObj* %current_45args46679, %struct.ScmObj** %stackaddr$prim47992, align 8
%stackaddr$prim47993 = alloca %struct.ScmObj*, align 8
%x40215 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46679)
store volatile %struct.ScmObj* %x40215, %struct.ScmObj** %stackaddr$prim47993, align 8
%stackaddr$prim47994 = alloca %struct.ScmObj*, align 8
%cpsprim40446 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40215)
store volatile %struct.ScmObj* %cpsprim40446, %struct.ScmObj** %stackaddr$prim47994, align 8
%ae41926 = call %struct.ScmObj* @const_init_int(i64 0)
%args46681$k40445$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47995 = alloca %struct.ScmObj*, align 8
%args46681$k40445$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40446, %struct.ScmObj* %args46681$k40445$0)
store volatile %struct.ScmObj* %args46681$k40445$1, %struct.ScmObj** %stackaddr$prim47995, align 8
%stackaddr$prim47996 = alloca %struct.ScmObj*, align 8
%args46681$k40445$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41926, %struct.ScmObj* %args46681$k40445$1)
store volatile %struct.ScmObj* %args46681$k40445$2, %struct.ScmObj** %stackaddr$prim47996, align 8
%clofunc47997 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40445)
musttail call tailcc void %clofunc47997(%struct.ScmObj* %k40445, %struct.ScmObj* %args46681$k40445$2)
ret void
}

define tailcc void @proc_clo$ae41899(%struct.ScmObj* %env$ae41899,%struct.ScmObj* %current_45args46684) {
%stackaddr$prim47998 = alloca %struct.ScmObj*, align 8
%k40447 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46684)
store volatile %struct.ScmObj* %k40447, %struct.ScmObj** %stackaddr$prim47998, align 8
%stackaddr$prim47999 = alloca %struct.ScmObj*, align 8
%current_45args46685 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46684)
store volatile %struct.ScmObj* %current_45args46685, %struct.ScmObj** %stackaddr$prim47999, align 8
%stackaddr$prim48000 = alloca %struct.ScmObj*, align 8
%x40217 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46685)
store volatile %struct.ScmObj* %x40217, %struct.ScmObj** %stackaddr$prim48000, align 8
%stackaddr$prim48001 = alloca %struct.ScmObj*, align 8
%cpsprim40448 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40217)
store volatile %struct.ScmObj* %cpsprim40448, %struct.ScmObj** %stackaddr$prim48001, align 8
%ae41902 = call %struct.ScmObj* @const_init_int(i64 0)
%args46687$k40447$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48002 = alloca %struct.ScmObj*, align 8
%args46687$k40447$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40448, %struct.ScmObj* %args46687$k40447$0)
store volatile %struct.ScmObj* %args46687$k40447$1, %struct.ScmObj** %stackaddr$prim48002, align 8
%stackaddr$prim48003 = alloca %struct.ScmObj*, align 8
%args46687$k40447$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41902, %struct.ScmObj* %args46687$k40447$1)
store volatile %struct.ScmObj* %args46687$k40447$2, %struct.ScmObj** %stackaddr$prim48003, align 8
%clofunc48004 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40447)
musttail call tailcc void %clofunc48004(%struct.ScmObj* %k40447, %struct.ScmObj* %args46687$k40447$2)
ret void
}

define tailcc void @proc_clo$ae41851(%struct.ScmObj* %env$ae41851,%struct.ScmObj* %current_45args46690) {
%stackaddr$prim48005 = alloca %struct.ScmObj*, align 8
%k40449 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46690)
store volatile %struct.ScmObj* %k40449, %struct.ScmObj** %stackaddr$prim48005, align 8
%stackaddr$prim48006 = alloca %struct.ScmObj*, align 8
%current_45args46691 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46690)
store volatile %struct.ScmObj* %current_45args46691, %struct.ScmObj** %stackaddr$prim48006, align 8
%stackaddr$prim48007 = alloca %struct.ScmObj*, align 8
%lst40213 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46691)
store volatile %struct.ScmObj* %lst40213, %struct.ScmObj** %stackaddr$prim48007, align 8
%stackaddr$prim48008 = alloca %struct.ScmObj*, align 8
%current_45args46692 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46691)
store volatile %struct.ScmObj* %current_45args46692, %struct.ScmObj** %stackaddr$prim48008, align 8
%stackaddr$prim48009 = alloca %struct.ScmObj*, align 8
%b40212 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46692)
store volatile %struct.ScmObj* %b40212, %struct.ScmObj** %stackaddr$prim48009, align 8
%truthy$cmp48010 = call i64 @is_truthy_value(%struct.ScmObj* %b40212)
%cmp$cmp48010 = icmp eq i64 %truthy$cmp48010, 1
br i1 %cmp$cmp48010, label %truebranch$cmp48010, label %falsebranch$cmp48010
truebranch$cmp48010:
%ae41854 = call %struct.ScmObj* @const_init_int(i64 0)
%args46694$k40449$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48011 = alloca %struct.ScmObj*, align 8
%args46694$k40449$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b40212, %struct.ScmObj* %args46694$k40449$0)
store volatile %struct.ScmObj* %args46694$k40449$1, %struct.ScmObj** %stackaddr$prim48011, align 8
%stackaddr$prim48012 = alloca %struct.ScmObj*, align 8
%args46694$k40449$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41854, %struct.ScmObj* %args46694$k40449$1)
store volatile %struct.ScmObj* %args46694$k40449$2, %struct.ScmObj** %stackaddr$prim48012, align 8
%clofunc48013 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40449)
musttail call tailcc void %clofunc48013(%struct.ScmObj* %k40449, %struct.ScmObj* %args46694$k40449$2)
ret void
falsebranch$cmp48010:
%stackaddr$prim48014 = alloca %struct.ScmObj*, align 8
%cpsprim40450 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40213)
store volatile %struct.ScmObj* %cpsprim40450, %struct.ScmObj** %stackaddr$prim48014, align 8
%ae41861 = call %struct.ScmObj* @const_init_int(i64 0)
%args46695$k40449$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48015 = alloca %struct.ScmObj*, align 8
%args46695$k40449$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40450, %struct.ScmObj* %args46695$k40449$0)
store volatile %struct.ScmObj* %args46695$k40449$1, %struct.ScmObj** %stackaddr$prim48015, align 8
%stackaddr$prim48016 = alloca %struct.ScmObj*, align 8
%args46695$k40449$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41861, %struct.ScmObj* %args46695$k40449$1)
store volatile %struct.ScmObj* %args46695$k40449$2, %struct.ScmObj** %stackaddr$prim48016, align 8
%clofunc48017 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40449)
musttail call tailcc void %clofunc48017(%struct.ScmObj* %k40449, %struct.ScmObj* %args46695$k40449$2)
ret void
}

define tailcc void @proc_clo$ae41692(%struct.ScmObj* %env$ae41692,%struct.ScmObj* %args4015140451) {
%stackaddr$env-ref48018 = alloca %struct.ScmObj*, align 8
%_37last40146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41692, i64 0)
store %struct.ScmObj* %_37last40146, %struct.ScmObj** %stackaddr$env-ref48018
%stackaddr$env-ref48019 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41692, i64 1)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref48019
%stackaddr$env-ref48020 = alloca %struct.ScmObj*, align 8
%_37drop_45right40143 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41692, i64 2)
store %struct.ScmObj* %_37drop_45right40143, %struct.ScmObj** %stackaddr$env-ref48020
%stackaddr$prim48021 = alloca %struct.ScmObj*, align 8
%k40452 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4015140451)
store volatile %struct.ScmObj* %k40452, %struct.ScmObj** %stackaddr$prim48021, align 8
%stackaddr$prim48022 = alloca %struct.ScmObj*, align 8
%args40151 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4015140451)
store volatile %struct.ScmObj* %args40151, %struct.ScmObj** %stackaddr$prim48022, align 8
%stackaddr$prim48023 = alloca %struct.ScmObj*, align 8
%f40153 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40151)
store volatile %struct.ScmObj* %f40153, %struct.ScmObj** %stackaddr$prim48023, align 8
%stackaddr$prim48024 = alloca %struct.ScmObj*, align 8
%lsts40152 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40151)
store volatile %struct.ScmObj* %lsts40152, %struct.ScmObj** %stackaddr$prim48024, align 8
%stackaddr$makeclosure48025 = alloca %struct.ScmObj*, align 8
%fptrToInt48026 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41697 to i64
%ae41697 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48026)
store volatile %struct.ScmObj* %ae41697, %struct.ScmObj** %stackaddr$makeclosure48025, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41697, %struct.ScmObj* %lsts40152, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41697, %struct.ScmObj* %k40452, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41697, %struct.ScmObj* %_37foldr40129, i64 2)
%ae41698 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48027 = alloca %struct.ScmObj*, align 8
%fptrToInt48028 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41699 to i64
%ae41699 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48028)
store volatile %struct.ScmObj* %ae41699, %struct.ScmObj** %stackaddr$makeclosure48027, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41699, %struct.ScmObj* %f40153, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41699, %struct.ScmObj* %_37last40146, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41699, %struct.ScmObj* %_37drop_45right40143, i64 2)
%args46714$ae41697$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48029 = alloca %struct.ScmObj*, align 8
%args46714$ae41697$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41699, %struct.ScmObj* %args46714$ae41697$0)
store volatile %struct.ScmObj* %args46714$ae41697$1, %struct.ScmObj** %stackaddr$prim48029, align 8
%stackaddr$prim48030 = alloca %struct.ScmObj*, align 8
%args46714$ae41697$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41698, %struct.ScmObj* %args46714$ae41697$1)
store volatile %struct.ScmObj* %args46714$ae41697$2, %struct.ScmObj** %stackaddr$prim48030, align 8
%clofunc48031 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41697)
musttail call tailcc void %clofunc48031(%struct.ScmObj* %ae41697, %struct.ScmObj* %args46714$ae41697$2)
ret void
}

define tailcc void @proc_clo$ae41697(%struct.ScmObj* %env$ae41697,%struct.ScmObj* %current_45args46699) {
%stackaddr$env-ref48032 = alloca %struct.ScmObj*, align 8
%lsts40152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41697, i64 0)
store %struct.ScmObj* %lsts40152, %struct.ScmObj** %stackaddr$env-ref48032
%stackaddr$env-ref48033 = alloca %struct.ScmObj*, align 8
%k40452 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41697, i64 1)
store %struct.ScmObj* %k40452, %struct.ScmObj** %stackaddr$env-ref48033
%stackaddr$env-ref48034 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41697, i64 2)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref48034
%stackaddr$prim48035 = alloca %struct.ScmObj*, align 8
%_95k40453 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46699)
store volatile %struct.ScmObj* %_95k40453, %struct.ScmObj** %stackaddr$prim48035, align 8
%stackaddr$prim48036 = alloca %struct.ScmObj*, align 8
%current_45args46700 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46699)
store volatile %struct.ScmObj* %current_45args46700, %struct.ScmObj** %stackaddr$prim48036, align 8
%stackaddr$prim48037 = alloca %struct.ScmObj*, align 8
%anf_45bind40281 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46700)
store volatile %struct.ScmObj* %anf_45bind40281, %struct.ScmObj** %stackaddr$prim48037, align 8
%ae41760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48038 = alloca %struct.ScmObj*, align 8
%anf_45bind40282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41760, %struct.ScmObj* %lsts40152)
store volatile %struct.ScmObj* %anf_45bind40282, %struct.ScmObj** %stackaddr$prim48038, align 8
%stackaddr$prim48039 = alloca %struct.ScmObj*, align 8
%anf_45bind40283 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40281, %struct.ScmObj* %anf_45bind40282)
store volatile %struct.ScmObj* %anf_45bind40283, %struct.ScmObj** %stackaddr$prim48039, align 8
%stackaddr$prim48040 = alloca %struct.ScmObj*, align 8
%cpsargs40454 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40452, %struct.ScmObj* %anf_45bind40283)
store volatile %struct.ScmObj* %cpsargs40454, %struct.ScmObj** %stackaddr$prim48040, align 8
%clofunc48041 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr40129)
musttail call tailcc void %clofunc48041(%struct.ScmObj* %_37foldr40129, %struct.ScmObj* %cpsargs40454)
ret void
}

define tailcc void @proc_clo$ae41699(%struct.ScmObj* %env$ae41699,%struct.ScmObj* %fargs4015440455) {
%stackaddr$env-ref48042 = alloca %struct.ScmObj*, align 8
%f40153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41699, i64 0)
store %struct.ScmObj* %f40153, %struct.ScmObj** %stackaddr$env-ref48042
%stackaddr$env-ref48043 = alloca %struct.ScmObj*, align 8
%_37last40146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41699, i64 1)
store %struct.ScmObj* %_37last40146, %struct.ScmObj** %stackaddr$env-ref48043
%stackaddr$env-ref48044 = alloca %struct.ScmObj*, align 8
%_37drop_45right40143 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41699, i64 2)
store %struct.ScmObj* %_37drop_45right40143, %struct.ScmObj** %stackaddr$env-ref48044
%stackaddr$prim48045 = alloca %struct.ScmObj*, align 8
%k40456 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs4015440455)
store volatile %struct.ScmObj* %k40456, %struct.ScmObj** %stackaddr$prim48045, align 8
%stackaddr$prim48046 = alloca %struct.ScmObj*, align 8
%fargs40154 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs4015440455)
store volatile %struct.ScmObj* %fargs40154, %struct.ScmObj** %stackaddr$prim48046, align 8
%stackaddr$makeclosure48047 = alloca %struct.ScmObj*, align 8
%fptrToInt48048 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41703 to i64
%ae41703 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48048)
store volatile %struct.ScmObj* %ae41703, %struct.ScmObj** %stackaddr$makeclosure48047, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41703, %struct.ScmObj* %f40153, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41703, %struct.ScmObj* %k40456, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41703, %struct.ScmObj* %_37last40146, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41703, %struct.ScmObj* %fargs40154, i64 3)
%ae41705 = call %struct.ScmObj* @const_init_int(i64 1)
%args46713$_37drop_45right40143$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48049 = alloca %struct.ScmObj*, align 8
%args46713$_37drop_45right40143$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41705, %struct.ScmObj* %args46713$_37drop_45right40143$0)
store volatile %struct.ScmObj* %args46713$_37drop_45right40143$1, %struct.ScmObj** %stackaddr$prim48049, align 8
%stackaddr$prim48050 = alloca %struct.ScmObj*, align 8
%args46713$_37drop_45right40143$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs40154, %struct.ScmObj* %args46713$_37drop_45right40143$1)
store volatile %struct.ScmObj* %args46713$_37drop_45right40143$2, %struct.ScmObj** %stackaddr$prim48050, align 8
%stackaddr$prim48051 = alloca %struct.ScmObj*, align 8
%args46713$_37drop_45right40143$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41703, %struct.ScmObj* %args46713$_37drop_45right40143$2)
store volatile %struct.ScmObj* %args46713$_37drop_45right40143$3, %struct.ScmObj** %stackaddr$prim48051, align 8
%clofunc48052 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right40143)
musttail call tailcc void %clofunc48052(%struct.ScmObj* %_37drop_45right40143, %struct.ScmObj* %args46713$_37drop_45right40143$3)
ret void
}

define tailcc void @proc_clo$ae41703(%struct.ScmObj* %env$ae41703,%struct.ScmObj* %current_45args46702) {
%stackaddr$env-ref48053 = alloca %struct.ScmObj*, align 8
%f40153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41703, i64 0)
store %struct.ScmObj* %f40153, %struct.ScmObj** %stackaddr$env-ref48053
%stackaddr$env-ref48054 = alloca %struct.ScmObj*, align 8
%k40456 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41703, i64 1)
store %struct.ScmObj* %k40456, %struct.ScmObj** %stackaddr$env-ref48054
%stackaddr$env-ref48055 = alloca %struct.ScmObj*, align 8
%_37last40146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41703, i64 2)
store %struct.ScmObj* %_37last40146, %struct.ScmObj** %stackaddr$env-ref48055
%stackaddr$env-ref48056 = alloca %struct.ScmObj*, align 8
%fargs40154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41703, i64 3)
store %struct.ScmObj* %fargs40154, %struct.ScmObj** %stackaddr$env-ref48056
%stackaddr$prim48057 = alloca %struct.ScmObj*, align 8
%_95k40457 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46702)
store volatile %struct.ScmObj* %_95k40457, %struct.ScmObj** %stackaddr$prim48057, align 8
%stackaddr$prim48058 = alloca %struct.ScmObj*, align 8
%current_45args46703 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46702)
store volatile %struct.ScmObj* %current_45args46703, %struct.ScmObj** %stackaddr$prim48058, align 8
%stackaddr$prim48059 = alloca %struct.ScmObj*, align 8
%anf_45bind40278 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46703)
store volatile %struct.ScmObj* %anf_45bind40278, %struct.ScmObj** %stackaddr$prim48059, align 8
%stackaddr$makeclosure48060 = alloca %struct.ScmObj*, align 8
%fptrToInt48061 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41710 to i64
%ae41710 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48061)
store volatile %struct.ScmObj* %ae41710, %struct.ScmObj** %stackaddr$makeclosure48060, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41710, %struct.ScmObj* %k40456, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41710, %struct.ScmObj* %_37last40146, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41710, %struct.ScmObj* %fargs40154, i64 2)
%stackaddr$prim48062 = alloca %struct.ScmObj*, align 8
%cpsargs40461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41710, %struct.ScmObj* %anf_45bind40278)
store volatile %struct.ScmObj* %cpsargs40461, %struct.ScmObj** %stackaddr$prim48062, align 8
%clofunc48063 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40153)
musttail call tailcc void %clofunc48063(%struct.ScmObj* %f40153, %struct.ScmObj* %cpsargs40461)
ret void
}

define tailcc void @proc_clo$ae41710(%struct.ScmObj* %env$ae41710,%struct.ScmObj* %current_45args46705) {
%stackaddr$env-ref48064 = alloca %struct.ScmObj*, align 8
%k40456 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41710, i64 0)
store %struct.ScmObj* %k40456, %struct.ScmObj** %stackaddr$env-ref48064
%stackaddr$env-ref48065 = alloca %struct.ScmObj*, align 8
%_37last40146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41710, i64 1)
store %struct.ScmObj* %_37last40146, %struct.ScmObj** %stackaddr$env-ref48065
%stackaddr$env-ref48066 = alloca %struct.ScmObj*, align 8
%fargs40154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41710, i64 2)
store %struct.ScmObj* %fargs40154, %struct.ScmObj** %stackaddr$env-ref48066
%stackaddr$prim48067 = alloca %struct.ScmObj*, align 8
%_95k40458 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46705)
store volatile %struct.ScmObj* %_95k40458, %struct.ScmObj** %stackaddr$prim48067, align 8
%stackaddr$prim48068 = alloca %struct.ScmObj*, align 8
%current_45args46706 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46705)
store volatile %struct.ScmObj* %current_45args46706, %struct.ScmObj** %stackaddr$prim48068, align 8
%stackaddr$prim48069 = alloca %struct.ScmObj*, align 8
%anf_45bind40279 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46706)
store volatile %struct.ScmObj* %anf_45bind40279, %struct.ScmObj** %stackaddr$prim48069, align 8
%stackaddr$makeclosure48070 = alloca %struct.ScmObj*, align 8
%fptrToInt48071 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41715 to i64
%ae41715 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48071)
store volatile %struct.ScmObj* %ae41715, %struct.ScmObj** %stackaddr$makeclosure48070, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41715, %struct.ScmObj* %k40456, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41715, %struct.ScmObj* %anf_45bind40279, i64 1)
%args46712$_37last40146$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48072 = alloca %struct.ScmObj*, align 8
%args46712$_37last40146$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs40154, %struct.ScmObj* %args46712$_37last40146$0)
store volatile %struct.ScmObj* %args46712$_37last40146$1, %struct.ScmObj** %stackaddr$prim48072, align 8
%stackaddr$prim48073 = alloca %struct.ScmObj*, align 8
%args46712$_37last40146$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41715, %struct.ScmObj* %args46712$_37last40146$1)
store volatile %struct.ScmObj* %args46712$_37last40146$2, %struct.ScmObj** %stackaddr$prim48073, align 8
%clofunc48074 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last40146)
musttail call tailcc void %clofunc48074(%struct.ScmObj* %_37last40146, %struct.ScmObj* %args46712$_37last40146$2)
ret void
}

define tailcc void @proc_clo$ae41715(%struct.ScmObj* %env$ae41715,%struct.ScmObj* %current_45args46708) {
%stackaddr$env-ref48075 = alloca %struct.ScmObj*, align 8
%k40456 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41715, i64 0)
store %struct.ScmObj* %k40456, %struct.ScmObj** %stackaddr$env-ref48075
%stackaddr$env-ref48076 = alloca %struct.ScmObj*, align 8
%anf_45bind40279 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41715, i64 1)
store %struct.ScmObj* %anf_45bind40279, %struct.ScmObj** %stackaddr$env-ref48076
%stackaddr$prim48077 = alloca %struct.ScmObj*, align 8
%_95k40459 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46708)
store volatile %struct.ScmObj* %_95k40459, %struct.ScmObj** %stackaddr$prim48077, align 8
%stackaddr$prim48078 = alloca %struct.ScmObj*, align 8
%current_45args46709 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46708)
store volatile %struct.ScmObj* %current_45args46709, %struct.ScmObj** %stackaddr$prim48078, align 8
%stackaddr$prim48079 = alloca %struct.ScmObj*, align 8
%anf_45bind40280 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46709)
store volatile %struct.ScmObj* %anf_45bind40280, %struct.ScmObj** %stackaddr$prim48079, align 8
%stackaddr$prim48080 = alloca %struct.ScmObj*, align 8
%cpsprim40460 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40279, %struct.ScmObj* %anf_45bind40280)
store volatile %struct.ScmObj* %cpsprim40460, %struct.ScmObj** %stackaddr$prim48080, align 8
%ae41720 = call %struct.ScmObj* @const_init_int(i64 0)
%args46711$k40456$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48081 = alloca %struct.ScmObj*, align 8
%args46711$k40456$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40460, %struct.ScmObj* %args46711$k40456$0)
store volatile %struct.ScmObj* %args46711$k40456$1, %struct.ScmObj** %stackaddr$prim48081, align 8
%stackaddr$prim48082 = alloca %struct.ScmObj*, align 8
%args46711$k40456$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41720, %struct.ScmObj* %args46711$k40456$1)
store volatile %struct.ScmObj* %args46711$k40456$2, %struct.ScmObj** %stackaddr$prim48082, align 8
%clofunc48083 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40456)
musttail call tailcc void %clofunc48083(%struct.ScmObj* %k40456, %struct.ScmObj* %args46711$k40456$2)
ret void
}

define tailcc void @proc_clo$ae41615(%struct.ScmObj* %env$ae41615,%struct.ScmObj* %current_45args46716) {
%stackaddr$env-ref48084 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41615, i64 0)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48084
%stackaddr$prim48085 = alloca %struct.ScmObj*, align 8
%k40462 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46716)
store volatile %struct.ScmObj* %k40462, %struct.ScmObj** %stackaddr$prim48085, align 8
%stackaddr$prim48086 = alloca %struct.ScmObj*, align 8
%current_45args46717 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46716)
store volatile %struct.ScmObj* %current_45args46717, %struct.ScmObj** %stackaddr$prim48086, align 8
%stackaddr$prim48087 = alloca %struct.ScmObj*, align 8
%f40157 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46717)
store volatile %struct.ScmObj* %f40157, %struct.ScmObj** %stackaddr$prim48087, align 8
%stackaddr$prim48088 = alloca %struct.ScmObj*, align 8
%current_45args46718 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46717)
store volatile %struct.ScmObj* %current_45args46718, %struct.ScmObj** %stackaddr$prim48088, align 8
%stackaddr$prim48089 = alloca %struct.ScmObj*, align 8
%lst40156 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46718)
store volatile %struct.ScmObj* %lst40156, %struct.ScmObj** %stackaddr$prim48089, align 8
%stackaddr$makeclosure48090 = alloca %struct.ScmObj*, align 8
%fptrToInt48091 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41616 to i64
%ae41616 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48091)
store volatile %struct.ScmObj* %ae41616, %struct.ScmObj** %stackaddr$makeclosure48090, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41616, %struct.ScmObj* %lst40156, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41616, %struct.ScmObj* %_37foldr140124, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41616, %struct.ScmObj* %k40462, i64 2)
%ae41617 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48092 = alloca %struct.ScmObj*, align 8
%fptrToInt48093 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41618 to i64
%ae41618 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48093)
store volatile %struct.ScmObj* %ae41618, %struct.ScmObj** %stackaddr$makeclosure48092, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41618, %struct.ScmObj* %f40157, i64 0)
%args46733$ae41616$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48094 = alloca %struct.ScmObj*, align 8
%args46733$ae41616$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41618, %struct.ScmObj* %args46733$ae41616$0)
store volatile %struct.ScmObj* %args46733$ae41616$1, %struct.ScmObj** %stackaddr$prim48094, align 8
%stackaddr$prim48095 = alloca %struct.ScmObj*, align 8
%args46733$ae41616$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41617, %struct.ScmObj* %args46733$ae41616$1)
store volatile %struct.ScmObj* %args46733$ae41616$2, %struct.ScmObj** %stackaddr$prim48095, align 8
%clofunc48096 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41616)
musttail call tailcc void %clofunc48096(%struct.ScmObj* %ae41616, %struct.ScmObj* %args46733$ae41616$2)
ret void
}

define tailcc void @proc_clo$ae41616(%struct.ScmObj* %env$ae41616,%struct.ScmObj* %current_45args46720) {
%stackaddr$env-ref48097 = alloca %struct.ScmObj*, align 8
%lst40156 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41616, i64 0)
store %struct.ScmObj* %lst40156, %struct.ScmObj** %stackaddr$env-ref48097
%stackaddr$env-ref48098 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41616, i64 1)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48098
%stackaddr$env-ref48099 = alloca %struct.ScmObj*, align 8
%k40462 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41616, i64 2)
store %struct.ScmObj* %k40462, %struct.ScmObj** %stackaddr$env-ref48099
%stackaddr$prim48100 = alloca %struct.ScmObj*, align 8
%_95k40463 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46720)
store volatile %struct.ScmObj* %_95k40463, %struct.ScmObj** %stackaddr$prim48100, align 8
%stackaddr$prim48101 = alloca %struct.ScmObj*, align 8
%current_45args46721 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46720)
store volatile %struct.ScmObj* %current_45args46721, %struct.ScmObj** %stackaddr$prim48101, align 8
%stackaddr$prim48102 = alloca %struct.ScmObj*, align 8
%anf_45bind40277 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46721)
store volatile %struct.ScmObj* %anf_45bind40277, %struct.ScmObj** %stackaddr$prim48102, align 8
%ae41650 = call %struct.ScmObj* @const_init_null()
%args46723$_37foldr140124$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48103 = alloca %struct.ScmObj*, align 8
%args46723$_37foldr140124$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40156, %struct.ScmObj* %args46723$_37foldr140124$0)
store volatile %struct.ScmObj* %args46723$_37foldr140124$1, %struct.ScmObj** %stackaddr$prim48103, align 8
%stackaddr$prim48104 = alloca %struct.ScmObj*, align 8
%args46723$_37foldr140124$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41650, %struct.ScmObj* %args46723$_37foldr140124$1)
store volatile %struct.ScmObj* %args46723$_37foldr140124$2, %struct.ScmObj** %stackaddr$prim48104, align 8
%stackaddr$prim48105 = alloca %struct.ScmObj*, align 8
%args46723$_37foldr140124$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40277, %struct.ScmObj* %args46723$_37foldr140124$2)
store volatile %struct.ScmObj* %args46723$_37foldr140124$3, %struct.ScmObj** %stackaddr$prim48105, align 8
%stackaddr$prim48106 = alloca %struct.ScmObj*, align 8
%args46723$_37foldr140124$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40462, %struct.ScmObj* %args46723$_37foldr140124$3)
store volatile %struct.ScmObj* %args46723$_37foldr140124$4, %struct.ScmObj** %stackaddr$prim48106, align 8
%clofunc48107 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140124)
musttail call tailcc void %clofunc48107(%struct.ScmObj* %_37foldr140124, %struct.ScmObj* %args46723$_37foldr140124$4)
ret void
}

define tailcc void @proc_clo$ae41618(%struct.ScmObj* %env$ae41618,%struct.ScmObj* %current_45args46724) {
%stackaddr$env-ref48108 = alloca %struct.ScmObj*, align 8
%f40157 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41618, i64 0)
store %struct.ScmObj* %f40157, %struct.ScmObj** %stackaddr$env-ref48108
%stackaddr$prim48109 = alloca %struct.ScmObj*, align 8
%k40464 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46724)
store volatile %struct.ScmObj* %k40464, %struct.ScmObj** %stackaddr$prim48109, align 8
%stackaddr$prim48110 = alloca %struct.ScmObj*, align 8
%current_45args46725 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46724)
store volatile %struct.ScmObj* %current_45args46725, %struct.ScmObj** %stackaddr$prim48110, align 8
%stackaddr$prim48111 = alloca %struct.ScmObj*, align 8
%v40159 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46725)
store volatile %struct.ScmObj* %v40159, %struct.ScmObj** %stackaddr$prim48111, align 8
%stackaddr$prim48112 = alloca %struct.ScmObj*, align 8
%current_45args46726 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46725)
store volatile %struct.ScmObj* %current_45args46726, %struct.ScmObj** %stackaddr$prim48112, align 8
%stackaddr$prim48113 = alloca %struct.ScmObj*, align 8
%r40158 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46726)
store volatile %struct.ScmObj* %r40158, %struct.ScmObj** %stackaddr$prim48113, align 8
%stackaddr$makeclosure48114 = alloca %struct.ScmObj*, align 8
%fptrToInt48115 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41620 to i64
%ae41620 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48115)
store volatile %struct.ScmObj* %ae41620, %struct.ScmObj** %stackaddr$makeclosure48114, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41620, %struct.ScmObj* %k40464, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41620, %struct.ScmObj* %r40158, i64 1)
%args46732$f40157$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48116 = alloca %struct.ScmObj*, align 8
%args46732$f40157$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v40159, %struct.ScmObj* %args46732$f40157$0)
store volatile %struct.ScmObj* %args46732$f40157$1, %struct.ScmObj** %stackaddr$prim48116, align 8
%stackaddr$prim48117 = alloca %struct.ScmObj*, align 8
%args46732$f40157$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41620, %struct.ScmObj* %args46732$f40157$1)
store volatile %struct.ScmObj* %args46732$f40157$2, %struct.ScmObj** %stackaddr$prim48117, align 8
%clofunc48118 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40157)
musttail call tailcc void %clofunc48118(%struct.ScmObj* %f40157, %struct.ScmObj* %args46732$f40157$2)
ret void
}

define tailcc void @proc_clo$ae41620(%struct.ScmObj* %env$ae41620,%struct.ScmObj* %current_45args46728) {
%stackaddr$env-ref48119 = alloca %struct.ScmObj*, align 8
%k40464 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41620, i64 0)
store %struct.ScmObj* %k40464, %struct.ScmObj** %stackaddr$env-ref48119
%stackaddr$env-ref48120 = alloca %struct.ScmObj*, align 8
%r40158 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41620, i64 1)
store %struct.ScmObj* %r40158, %struct.ScmObj** %stackaddr$env-ref48120
%stackaddr$prim48121 = alloca %struct.ScmObj*, align 8
%_95k40465 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46728)
store volatile %struct.ScmObj* %_95k40465, %struct.ScmObj** %stackaddr$prim48121, align 8
%stackaddr$prim48122 = alloca %struct.ScmObj*, align 8
%current_45args46729 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46728)
store volatile %struct.ScmObj* %current_45args46729, %struct.ScmObj** %stackaddr$prim48122, align 8
%stackaddr$prim48123 = alloca %struct.ScmObj*, align 8
%anf_45bind40276 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46729)
store volatile %struct.ScmObj* %anf_45bind40276, %struct.ScmObj** %stackaddr$prim48123, align 8
%stackaddr$prim48124 = alloca %struct.ScmObj*, align 8
%cpsprim40466 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40276, %struct.ScmObj* %r40158)
store volatile %struct.ScmObj* %cpsprim40466, %struct.ScmObj** %stackaddr$prim48124, align 8
%ae41625 = call %struct.ScmObj* @const_init_int(i64 0)
%args46731$k40464$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48125 = alloca %struct.ScmObj*, align 8
%args46731$k40464$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40466, %struct.ScmObj* %args46731$k40464$0)
store volatile %struct.ScmObj* %args46731$k40464$1, %struct.ScmObj** %stackaddr$prim48125, align 8
%stackaddr$prim48126 = alloca %struct.ScmObj*, align 8
%args46731$k40464$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41625, %struct.ScmObj* %args46731$k40464$1)
store volatile %struct.ScmObj* %args46731$k40464$2, %struct.ScmObj** %stackaddr$prim48126, align 8
%clofunc48127 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40464)
musttail call tailcc void %clofunc48127(%struct.ScmObj* %k40464, %struct.ScmObj* %args46731$k40464$2)
ret void
}

define tailcc void @proc_clo$ae41229(%struct.ScmObj* %env$ae41229,%struct.ScmObj* %current_45args46736) {
%stackaddr$env-ref48128 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41229, i64 0)
store %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$env-ref48128
%stackaddr$env-ref48129 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41229, i64 1)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48129
%stackaddr$prim48130 = alloca %struct.ScmObj*, align 8
%k40467 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46736)
store volatile %struct.ScmObj* %k40467, %struct.ScmObj** %stackaddr$prim48130, align 8
%stackaddr$prim48131 = alloca %struct.ScmObj*, align 8
%current_45args46737 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46736)
store volatile %struct.ScmObj* %current_45args46737, %struct.ScmObj** %stackaddr$prim48131, align 8
%stackaddr$prim48132 = alloca %struct.ScmObj*, align 8
%_37foldr40130 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46737)
store volatile %struct.ScmObj* %_37foldr40130, %struct.ScmObj** %stackaddr$prim48132, align 8
%ae41231 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48133 = alloca %struct.ScmObj*, align 8
%fptrToInt48134 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41232 to i64
%ae41232 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48134)
store volatile %struct.ScmObj* %ae41232, %struct.ScmObj** %stackaddr$makeclosure48133, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41232, %struct.ScmObj* %_37map140120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41232, %struct.ScmObj* %_37foldr40130, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41232, %struct.ScmObj* %_37foldr140124, i64 2)
%args46794$k40467$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48135 = alloca %struct.ScmObj*, align 8
%args46794$k40467$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41232, %struct.ScmObj* %args46794$k40467$0)
store volatile %struct.ScmObj* %args46794$k40467$1, %struct.ScmObj** %stackaddr$prim48135, align 8
%stackaddr$prim48136 = alloca %struct.ScmObj*, align 8
%args46794$k40467$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41231, %struct.ScmObj* %args46794$k40467$1)
store volatile %struct.ScmObj* %args46794$k40467$2, %struct.ScmObj** %stackaddr$prim48136, align 8
%clofunc48137 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40467)
musttail call tailcc void %clofunc48137(%struct.ScmObj* %k40467, %struct.ScmObj* %args46794$k40467$2)
ret void
}

define tailcc void @proc_clo$ae41232(%struct.ScmObj* %env$ae41232,%struct.ScmObj* %args4013140468) {
%stackaddr$env-ref48138 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41232, i64 0)
store %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$env-ref48138
%stackaddr$env-ref48139 = alloca %struct.ScmObj*, align 8
%_37foldr40130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41232, i64 1)
store %struct.ScmObj* %_37foldr40130, %struct.ScmObj** %stackaddr$env-ref48139
%stackaddr$env-ref48140 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41232, i64 2)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48140
%stackaddr$prim48141 = alloca %struct.ScmObj*, align 8
%k40469 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4013140468)
store volatile %struct.ScmObj* %k40469, %struct.ScmObj** %stackaddr$prim48141, align 8
%stackaddr$prim48142 = alloca %struct.ScmObj*, align 8
%args40131 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4013140468)
store volatile %struct.ScmObj* %args40131, %struct.ScmObj** %stackaddr$prim48142, align 8
%stackaddr$prim48143 = alloca %struct.ScmObj*, align 8
%f40134 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40131)
store volatile %struct.ScmObj* %f40134, %struct.ScmObj** %stackaddr$prim48143, align 8
%stackaddr$prim48144 = alloca %struct.ScmObj*, align 8
%anf_45bind40263 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40131)
store volatile %struct.ScmObj* %anf_45bind40263, %struct.ScmObj** %stackaddr$prim48144, align 8
%stackaddr$prim48145 = alloca %struct.ScmObj*, align 8
%acc40133 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40263)
store volatile %struct.ScmObj* %acc40133, %struct.ScmObj** %stackaddr$prim48145, align 8
%stackaddr$prim48146 = alloca %struct.ScmObj*, align 8
%anf_45bind40264 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40131)
store volatile %struct.ScmObj* %anf_45bind40264, %struct.ScmObj** %stackaddr$prim48146, align 8
%stackaddr$prim48147 = alloca %struct.ScmObj*, align 8
%lsts40132 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40264)
store volatile %struct.ScmObj* %lsts40132, %struct.ScmObj** %stackaddr$prim48147, align 8
%stackaddr$makeclosure48148 = alloca %struct.ScmObj*, align 8
%fptrToInt48149 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41240 to i64
%ae41240 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt48149)
store volatile %struct.ScmObj* %ae41240, %struct.ScmObj** %stackaddr$makeclosure48148, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41240, %struct.ScmObj* %k40469, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41240, %struct.ScmObj* %acc40133, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41240, %struct.ScmObj* %_37map140120, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41240, %struct.ScmObj* %f40134, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41240, %struct.ScmObj* %lsts40132, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41240, %struct.ScmObj* %_37foldr40130, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41240, %struct.ScmObj* %_37foldr140124, i64 6)
%ae41241 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48150 = alloca %struct.ScmObj*, align 8
%fptrToInt48151 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41242 to i64
%ae41242 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48151)
store volatile %struct.ScmObj* %ae41242, %struct.ScmObj** %stackaddr$makeclosure48150, align 8
%args46793$ae41240$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48152 = alloca %struct.ScmObj*, align 8
%args46793$ae41240$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41242, %struct.ScmObj* %args46793$ae41240$0)
store volatile %struct.ScmObj* %args46793$ae41240$1, %struct.ScmObj** %stackaddr$prim48152, align 8
%stackaddr$prim48153 = alloca %struct.ScmObj*, align 8
%args46793$ae41240$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41241, %struct.ScmObj* %args46793$ae41240$1)
store volatile %struct.ScmObj* %args46793$ae41240$2, %struct.ScmObj** %stackaddr$prim48153, align 8
%clofunc48154 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41240)
musttail call tailcc void %clofunc48154(%struct.ScmObj* %ae41240, %struct.ScmObj* %args46793$ae41240$2)
ret void
}

define tailcc void @proc_clo$ae41240(%struct.ScmObj* %env$ae41240,%struct.ScmObj* %current_45args46739) {
%stackaddr$env-ref48155 = alloca %struct.ScmObj*, align 8
%k40469 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41240, i64 0)
store %struct.ScmObj* %k40469, %struct.ScmObj** %stackaddr$env-ref48155
%stackaddr$env-ref48156 = alloca %struct.ScmObj*, align 8
%acc40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41240, i64 1)
store %struct.ScmObj* %acc40133, %struct.ScmObj** %stackaddr$env-ref48156
%stackaddr$env-ref48157 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41240, i64 2)
store %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$env-ref48157
%stackaddr$env-ref48158 = alloca %struct.ScmObj*, align 8
%f40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41240, i64 3)
store %struct.ScmObj* %f40134, %struct.ScmObj** %stackaddr$env-ref48158
%stackaddr$env-ref48159 = alloca %struct.ScmObj*, align 8
%lsts40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41240, i64 4)
store %struct.ScmObj* %lsts40132, %struct.ScmObj** %stackaddr$env-ref48159
%stackaddr$env-ref48160 = alloca %struct.ScmObj*, align 8
%_37foldr40130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41240, i64 5)
store %struct.ScmObj* %_37foldr40130, %struct.ScmObj** %stackaddr$env-ref48160
%stackaddr$env-ref48161 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41240, i64 6)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48161
%stackaddr$prim48162 = alloca %struct.ScmObj*, align 8
%_95k40470 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46739)
store volatile %struct.ScmObj* %_95k40470, %struct.ScmObj** %stackaddr$prim48162, align 8
%stackaddr$prim48163 = alloca %struct.ScmObj*, align 8
%current_45args46740 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46739)
store volatile %struct.ScmObj* %current_45args46740, %struct.ScmObj** %stackaddr$prim48163, align 8
%stackaddr$prim48164 = alloca %struct.ScmObj*, align 8
%anf_45bind40265 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46740)
store volatile %struct.ScmObj* %anf_45bind40265, %struct.ScmObj** %stackaddr$prim48164, align 8
%stackaddr$makeclosure48165 = alloca %struct.ScmObj*, align 8
%fptrToInt48166 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41272 to i64
%ae41272 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt48166)
store volatile %struct.ScmObj* %ae41272, %struct.ScmObj** %stackaddr$makeclosure48165, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41272, %struct.ScmObj* %k40469, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41272, %struct.ScmObj* %acc40133, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41272, %struct.ScmObj* %_37map140120, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41272, %struct.ScmObj* %f40134, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41272, %struct.ScmObj* %lsts40132, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41272, %struct.ScmObj* %_37foldr40130, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41272, %struct.ScmObj* %_37foldr140124, i64 6)
%ae41274 = call %struct.ScmObj* @const_init_false()
%args46786$_37foldr140124$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48167 = alloca %struct.ScmObj*, align 8
%args46786$_37foldr140124$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40132, %struct.ScmObj* %args46786$_37foldr140124$0)
store volatile %struct.ScmObj* %args46786$_37foldr140124$1, %struct.ScmObj** %stackaddr$prim48167, align 8
%stackaddr$prim48168 = alloca %struct.ScmObj*, align 8
%args46786$_37foldr140124$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41274, %struct.ScmObj* %args46786$_37foldr140124$1)
store volatile %struct.ScmObj* %args46786$_37foldr140124$2, %struct.ScmObj** %stackaddr$prim48168, align 8
%stackaddr$prim48169 = alloca %struct.ScmObj*, align 8
%args46786$_37foldr140124$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40265, %struct.ScmObj* %args46786$_37foldr140124$2)
store volatile %struct.ScmObj* %args46786$_37foldr140124$3, %struct.ScmObj** %stackaddr$prim48169, align 8
%stackaddr$prim48170 = alloca %struct.ScmObj*, align 8
%args46786$_37foldr140124$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41272, %struct.ScmObj* %args46786$_37foldr140124$3)
store volatile %struct.ScmObj* %args46786$_37foldr140124$4, %struct.ScmObj** %stackaddr$prim48170, align 8
%clofunc48171 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140124)
musttail call tailcc void %clofunc48171(%struct.ScmObj* %_37foldr140124, %struct.ScmObj* %args46786$_37foldr140124$4)
ret void
}

define tailcc void @proc_clo$ae41272(%struct.ScmObj* %env$ae41272,%struct.ScmObj* %current_45args46742) {
%stackaddr$env-ref48172 = alloca %struct.ScmObj*, align 8
%k40469 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41272, i64 0)
store %struct.ScmObj* %k40469, %struct.ScmObj** %stackaddr$env-ref48172
%stackaddr$env-ref48173 = alloca %struct.ScmObj*, align 8
%acc40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41272, i64 1)
store %struct.ScmObj* %acc40133, %struct.ScmObj** %stackaddr$env-ref48173
%stackaddr$env-ref48174 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41272, i64 2)
store %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$env-ref48174
%stackaddr$env-ref48175 = alloca %struct.ScmObj*, align 8
%f40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41272, i64 3)
store %struct.ScmObj* %f40134, %struct.ScmObj** %stackaddr$env-ref48175
%stackaddr$env-ref48176 = alloca %struct.ScmObj*, align 8
%lsts40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41272, i64 4)
store %struct.ScmObj* %lsts40132, %struct.ScmObj** %stackaddr$env-ref48176
%stackaddr$env-ref48177 = alloca %struct.ScmObj*, align 8
%_37foldr40130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41272, i64 5)
store %struct.ScmObj* %_37foldr40130, %struct.ScmObj** %stackaddr$env-ref48177
%stackaddr$env-ref48178 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41272, i64 6)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48178
%stackaddr$prim48179 = alloca %struct.ScmObj*, align 8
%_95k40471 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46742)
store volatile %struct.ScmObj* %_95k40471, %struct.ScmObj** %stackaddr$prim48179, align 8
%stackaddr$prim48180 = alloca %struct.ScmObj*, align 8
%current_45args46743 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46742)
store volatile %struct.ScmObj* %current_45args46743, %struct.ScmObj** %stackaddr$prim48180, align 8
%stackaddr$prim48181 = alloca %struct.ScmObj*, align 8
%anf_45bind40266 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46743)
store volatile %struct.ScmObj* %anf_45bind40266, %struct.ScmObj** %stackaddr$prim48181, align 8
%truthy$cmp48182 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40266)
%cmp$cmp48182 = icmp eq i64 %truthy$cmp48182, 1
br i1 %cmp$cmp48182, label %truebranch$cmp48182, label %falsebranch$cmp48182
truebranch$cmp48182:
%ae41283 = call %struct.ScmObj* @const_init_int(i64 0)
%args46745$k40469$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48183 = alloca %struct.ScmObj*, align 8
%args46745$k40469$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40133, %struct.ScmObj* %args46745$k40469$0)
store volatile %struct.ScmObj* %args46745$k40469$1, %struct.ScmObj** %stackaddr$prim48183, align 8
%stackaddr$prim48184 = alloca %struct.ScmObj*, align 8
%args46745$k40469$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41283, %struct.ScmObj* %args46745$k40469$1)
store volatile %struct.ScmObj* %args46745$k40469$2, %struct.ScmObj** %stackaddr$prim48184, align 8
%clofunc48185 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40469)
musttail call tailcc void %clofunc48185(%struct.ScmObj* %k40469, %struct.ScmObj* %args46745$k40469$2)
ret void
falsebranch$cmp48182:
%stackaddr$makeclosure48186 = alloca %struct.ScmObj*, align 8
%fptrToInt48187 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41288 to i64
%ae41288 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt48187)
store volatile %struct.ScmObj* %ae41288, %struct.ScmObj** %stackaddr$makeclosure48186, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41288, %struct.ScmObj* %k40469, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41288, %struct.ScmObj* %acc40133, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41288, %struct.ScmObj* %_37map140120, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41288, %struct.ScmObj* %f40134, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41288, %struct.ScmObj* %lsts40132, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41288, %struct.ScmObj* %_37foldr40130, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41288, %struct.ScmObj* %_37foldr140124, i64 6)
%ae41289 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48188 = alloca %struct.ScmObj*, align 8
%fptrToInt48189 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41290 to i64
%ae41290 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48189)
store volatile %struct.ScmObj* %ae41290, %struct.ScmObj** %stackaddr$makeclosure48188, align 8
%args46785$ae41288$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48190 = alloca %struct.ScmObj*, align 8
%args46785$ae41288$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41290, %struct.ScmObj* %args46785$ae41288$0)
store volatile %struct.ScmObj* %args46785$ae41288$1, %struct.ScmObj** %stackaddr$prim48190, align 8
%stackaddr$prim48191 = alloca %struct.ScmObj*, align 8
%args46785$ae41288$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41289, %struct.ScmObj* %args46785$ae41288$1)
store volatile %struct.ScmObj* %args46785$ae41288$2, %struct.ScmObj** %stackaddr$prim48191, align 8
%clofunc48192 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41288)
musttail call tailcc void %clofunc48192(%struct.ScmObj* %ae41288, %struct.ScmObj* %args46785$ae41288$2)
ret void
}

define tailcc void @proc_clo$ae41288(%struct.ScmObj* %env$ae41288,%struct.ScmObj* %current_45args46746) {
%stackaddr$env-ref48193 = alloca %struct.ScmObj*, align 8
%k40469 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41288, i64 0)
store %struct.ScmObj* %k40469, %struct.ScmObj** %stackaddr$env-ref48193
%stackaddr$env-ref48194 = alloca %struct.ScmObj*, align 8
%acc40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41288, i64 1)
store %struct.ScmObj* %acc40133, %struct.ScmObj** %stackaddr$env-ref48194
%stackaddr$env-ref48195 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41288, i64 2)
store %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$env-ref48195
%stackaddr$env-ref48196 = alloca %struct.ScmObj*, align 8
%f40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41288, i64 3)
store %struct.ScmObj* %f40134, %struct.ScmObj** %stackaddr$env-ref48196
%stackaddr$env-ref48197 = alloca %struct.ScmObj*, align 8
%lsts40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41288, i64 4)
store %struct.ScmObj* %lsts40132, %struct.ScmObj** %stackaddr$env-ref48197
%stackaddr$env-ref48198 = alloca %struct.ScmObj*, align 8
%_37foldr40130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41288, i64 5)
store %struct.ScmObj* %_37foldr40130, %struct.ScmObj** %stackaddr$env-ref48198
%stackaddr$env-ref48199 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41288, i64 6)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48199
%stackaddr$prim48200 = alloca %struct.ScmObj*, align 8
%_95k40472 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46746)
store volatile %struct.ScmObj* %_95k40472, %struct.ScmObj** %stackaddr$prim48200, align 8
%stackaddr$prim48201 = alloca %struct.ScmObj*, align 8
%current_45args46747 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46746)
store volatile %struct.ScmObj* %current_45args46747, %struct.ScmObj** %stackaddr$prim48201, align 8
%stackaddr$prim48202 = alloca %struct.ScmObj*, align 8
%anf_45bind40267 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46747)
store volatile %struct.ScmObj* %anf_45bind40267, %struct.ScmObj** %stackaddr$prim48202, align 8
%stackaddr$makeclosure48203 = alloca %struct.ScmObj*, align 8
%fptrToInt48204 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41309 to i64
%ae41309 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt48204)
store volatile %struct.ScmObj* %ae41309, %struct.ScmObj** %stackaddr$makeclosure48203, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41309, %struct.ScmObj* %k40469, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41309, %struct.ScmObj* %acc40133, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41309, %struct.ScmObj* %_37map140120, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41309, %struct.ScmObj* %f40134, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41309, %struct.ScmObj* %lsts40132, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41309, %struct.ScmObj* %_37foldr40130, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41309, %struct.ScmObj* %_37foldr140124, i64 6)
%args46780$_37map140120$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48205 = alloca %struct.ScmObj*, align 8
%args46780$_37map140120$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40132, %struct.ScmObj* %args46780$_37map140120$0)
store volatile %struct.ScmObj* %args46780$_37map140120$1, %struct.ScmObj** %stackaddr$prim48205, align 8
%stackaddr$prim48206 = alloca %struct.ScmObj*, align 8
%args46780$_37map140120$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40267, %struct.ScmObj* %args46780$_37map140120$1)
store volatile %struct.ScmObj* %args46780$_37map140120$2, %struct.ScmObj** %stackaddr$prim48206, align 8
%stackaddr$prim48207 = alloca %struct.ScmObj*, align 8
%args46780$_37map140120$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41309, %struct.ScmObj* %args46780$_37map140120$2)
store volatile %struct.ScmObj* %args46780$_37map140120$3, %struct.ScmObj** %stackaddr$prim48207, align 8
%clofunc48208 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140120)
musttail call tailcc void %clofunc48208(%struct.ScmObj* %_37map140120, %struct.ScmObj* %args46780$_37map140120$3)
ret void
}

define tailcc void @proc_clo$ae41309(%struct.ScmObj* %env$ae41309,%struct.ScmObj* %current_45args46749) {
%stackaddr$env-ref48209 = alloca %struct.ScmObj*, align 8
%k40469 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41309, i64 0)
store %struct.ScmObj* %k40469, %struct.ScmObj** %stackaddr$env-ref48209
%stackaddr$env-ref48210 = alloca %struct.ScmObj*, align 8
%acc40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41309, i64 1)
store %struct.ScmObj* %acc40133, %struct.ScmObj** %stackaddr$env-ref48210
%stackaddr$env-ref48211 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41309, i64 2)
store %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$env-ref48211
%stackaddr$env-ref48212 = alloca %struct.ScmObj*, align 8
%f40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41309, i64 3)
store %struct.ScmObj* %f40134, %struct.ScmObj** %stackaddr$env-ref48212
%stackaddr$env-ref48213 = alloca %struct.ScmObj*, align 8
%lsts40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41309, i64 4)
store %struct.ScmObj* %lsts40132, %struct.ScmObj** %stackaddr$env-ref48213
%stackaddr$env-ref48214 = alloca %struct.ScmObj*, align 8
%_37foldr40130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41309, i64 5)
store %struct.ScmObj* %_37foldr40130, %struct.ScmObj** %stackaddr$env-ref48214
%stackaddr$env-ref48215 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41309, i64 6)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48215
%stackaddr$prim48216 = alloca %struct.ScmObj*, align 8
%_95k40473 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46749)
store volatile %struct.ScmObj* %_95k40473, %struct.ScmObj** %stackaddr$prim48216, align 8
%stackaddr$prim48217 = alloca %struct.ScmObj*, align 8
%current_45args46750 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46749)
store volatile %struct.ScmObj* %current_45args46750, %struct.ScmObj** %stackaddr$prim48217, align 8
%stackaddr$prim48218 = alloca %struct.ScmObj*, align 8
%lsts_4340139 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46750)
store volatile %struct.ScmObj* %lsts_4340139, %struct.ScmObj** %stackaddr$prim48218, align 8
%stackaddr$makeclosure48219 = alloca %struct.ScmObj*, align 8
%fptrToInt48220 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41312 to i64
%ae41312 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt48220)
store volatile %struct.ScmObj* %ae41312, %struct.ScmObj** %stackaddr$makeclosure48219, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41312, %struct.ScmObj* %k40469, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41312, %struct.ScmObj* %acc40133, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41312, %struct.ScmObj* %_37map140120, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41312, %struct.ScmObj* %f40134, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41312, %struct.ScmObj* %lsts40132, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41312, %struct.ScmObj* %_37foldr40130, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41312, %struct.ScmObj* %_37foldr140124, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae41312, %struct.ScmObj* %lsts_4340139, i64 7)
%ae41313 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48221 = alloca %struct.ScmObj*, align 8
%fptrToInt48222 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41314 to i64
%ae41314 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48222)
store volatile %struct.ScmObj* %ae41314, %struct.ScmObj** %stackaddr$makeclosure48221, align 8
%args46779$ae41312$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48223 = alloca %struct.ScmObj*, align 8
%args46779$ae41312$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41314, %struct.ScmObj* %args46779$ae41312$0)
store volatile %struct.ScmObj* %args46779$ae41312$1, %struct.ScmObj** %stackaddr$prim48223, align 8
%stackaddr$prim48224 = alloca %struct.ScmObj*, align 8
%args46779$ae41312$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41313, %struct.ScmObj* %args46779$ae41312$1)
store volatile %struct.ScmObj* %args46779$ae41312$2, %struct.ScmObj** %stackaddr$prim48224, align 8
%clofunc48225 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41312)
musttail call tailcc void %clofunc48225(%struct.ScmObj* %ae41312, %struct.ScmObj* %args46779$ae41312$2)
ret void
}

define tailcc void @proc_clo$ae41312(%struct.ScmObj* %env$ae41312,%struct.ScmObj* %current_45args46752) {
%stackaddr$env-ref48226 = alloca %struct.ScmObj*, align 8
%k40469 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41312, i64 0)
store %struct.ScmObj* %k40469, %struct.ScmObj** %stackaddr$env-ref48226
%stackaddr$env-ref48227 = alloca %struct.ScmObj*, align 8
%acc40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41312, i64 1)
store %struct.ScmObj* %acc40133, %struct.ScmObj** %stackaddr$env-ref48227
%stackaddr$env-ref48228 = alloca %struct.ScmObj*, align 8
%_37map140120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41312, i64 2)
store %struct.ScmObj* %_37map140120, %struct.ScmObj** %stackaddr$env-ref48228
%stackaddr$env-ref48229 = alloca %struct.ScmObj*, align 8
%f40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41312, i64 3)
store %struct.ScmObj* %f40134, %struct.ScmObj** %stackaddr$env-ref48229
%stackaddr$env-ref48230 = alloca %struct.ScmObj*, align 8
%lsts40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41312, i64 4)
store %struct.ScmObj* %lsts40132, %struct.ScmObj** %stackaddr$env-ref48230
%stackaddr$env-ref48231 = alloca %struct.ScmObj*, align 8
%_37foldr40130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41312, i64 5)
store %struct.ScmObj* %_37foldr40130, %struct.ScmObj** %stackaddr$env-ref48231
%stackaddr$env-ref48232 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41312, i64 6)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48232
%stackaddr$env-ref48233 = alloca %struct.ScmObj*, align 8
%lsts_4340139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41312, i64 7)
store %struct.ScmObj* %lsts_4340139, %struct.ScmObj** %stackaddr$env-ref48233
%stackaddr$prim48234 = alloca %struct.ScmObj*, align 8
%_95k40474 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46752)
store volatile %struct.ScmObj* %_95k40474, %struct.ScmObj** %stackaddr$prim48234, align 8
%stackaddr$prim48235 = alloca %struct.ScmObj*, align 8
%current_45args46753 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46752)
store volatile %struct.ScmObj* %current_45args46753, %struct.ScmObj** %stackaddr$prim48235, align 8
%stackaddr$prim48236 = alloca %struct.ScmObj*, align 8
%anf_45bind40268 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46753)
store volatile %struct.ScmObj* %anf_45bind40268, %struct.ScmObj** %stackaddr$prim48236, align 8
%stackaddr$makeclosure48237 = alloca %struct.ScmObj*, align 8
%fptrToInt48238 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41333 to i64
%ae41333 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt48238)
store volatile %struct.ScmObj* %ae41333, %struct.ScmObj** %stackaddr$makeclosure48237, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41333, %struct.ScmObj* %k40469, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41333, %struct.ScmObj* %acc40133, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41333, %struct.ScmObj* %f40134, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41333, %struct.ScmObj* %_37foldr40130, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41333, %struct.ScmObj* %_37foldr140124, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41333, %struct.ScmObj* %lsts_4340139, i64 5)
%args46774$_37map140120$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48239 = alloca %struct.ScmObj*, align 8
%args46774$_37map140120$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40132, %struct.ScmObj* %args46774$_37map140120$0)
store volatile %struct.ScmObj* %args46774$_37map140120$1, %struct.ScmObj** %stackaddr$prim48239, align 8
%stackaddr$prim48240 = alloca %struct.ScmObj*, align 8
%args46774$_37map140120$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40268, %struct.ScmObj* %args46774$_37map140120$1)
store volatile %struct.ScmObj* %args46774$_37map140120$2, %struct.ScmObj** %stackaddr$prim48240, align 8
%stackaddr$prim48241 = alloca %struct.ScmObj*, align 8
%args46774$_37map140120$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41333, %struct.ScmObj* %args46774$_37map140120$2)
store volatile %struct.ScmObj* %args46774$_37map140120$3, %struct.ScmObj** %stackaddr$prim48241, align 8
%clofunc48242 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140120)
musttail call tailcc void %clofunc48242(%struct.ScmObj* %_37map140120, %struct.ScmObj* %args46774$_37map140120$3)
ret void
}

define tailcc void @proc_clo$ae41333(%struct.ScmObj* %env$ae41333,%struct.ScmObj* %current_45args46755) {
%stackaddr$env-ref48243 = alloca %struct.ScmObj*, align 8
%k40469 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41333, i64 0)
store %struct.ScmObj* %k40469, %struct.ScmObj** %stackaddr$env-ref48243
%stackaddr$env-ref48244 = alloca %struct.ScmObj*, align 8
%acc40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41333, i64 1)
store %struct.ScmObj* %acc40133, %struct.ScmObj** %stackaddr$env-ref48244
%stackaddr$env-ref48245 = alloca %struct.ScmObj*, align 8
%f40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41333, i64 2)
store %struct.ScmObj* %f40134, %struct.ScmObj** %stackaddr$env-ref48245
%stackaddr$env-ref48246 = alloca %struct.ScmObj*, align 8
%_37foldr40130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41333, i64 3)
store %struct.ScmObj* %_37foldr40130, %struct.ScmObj** %stackaddr$env-ref48246
%stackaddr$env-ref48247 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41333, i64 4)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48247
%stackaddr$env-ref48248 = alloca %struct.ScmObj*, align 8
%lsts_4340139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41333, i64 5)
store %struct.ScmObj* %lsts_4340139, %struct.ScmObj** %stackaddr$env-ref48248
%stackaddr$prim48249 = alloca %struct.ScmObj*, align 8
%_95k40475 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46755)
store volatile %struct.ScmObj* %_95k40475, %struct.ScmObj** %stackaddr$prim48249, align 8
%stackaddr$prim48250 = alloca %struct.ScmObj*, align 8
%current_45args46756 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46755)
store volatile %struct.ScmObj* %current_45args46756, %struct.ScmObj** %stackaddr$prim48250, align 8
%stackaddr$prim48251 = alloca %struct.ScmObj*, align 8
%vs40137 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46756)
store volatile %struct.ScmObj* %vs40137, %struct.ScmObj** %stackaddr$prim48251, align 8
%stackaddr$makeclosure48252 = alloca %struct.ScmObj*, align 8
%fptrToInt48253 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41336 to i64
%ae41336 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt48253)
store volatile %struct.ScmObj* %ae41336, %struct.ScmObj** %stackaddr$makeclosure48252, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41336, %struct.ScmObj* %k40469, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41336, %struct.ScmObj* %acc40133, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41336, %struct.ScmObj* %vs40137, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41336, %struct.ScmObj* %f40134, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41336, %struct.ScmObj* %_37foldr40130, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41336, %struct.ScmObj* %_37foldr140124, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41336, %struct.ScmObj* %lsts_4340139, i64 6)
%ae41337 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48254 = alloca %struct.ScmObj*, align 8
%fptrToInt48255 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41338 to i64
%ae41338 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48255)
store volatile %struct.ScmObj* %ae41338, %struct.ScmObj** %stackaddr$makeclosure48254, align 8
%args46773$ae41336$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48256 = alloca %struct.ScmObj*, align 8
%args46773$ae41336$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41338, %struct.ScmObj* %args46773$ae41336$0)
store volatile %struct.ScmObj* %args46773$ae41336$1, %struct.ScmObj** %stackaddr$prim48256, align 8
%stackaddr$prim48257 = alloca %struct.ScmObj*, align 8
%args46773$ae41336$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41337, %struct.ScmObj* %args46773$ae41336$1)
store volatile %struct.ScmObj* %args46773$ae41336$2, %struct.ScmObj** %stackaddr$prim48257, align 8
%clofunc48258 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41336)
musttail call tailcc void %clofunc48258(%struct.ScmObj* %ae41336, %struct.ScmObj* %args46773$ae41336$2)
ret void
}

define tailcc void @proc_clo$ae41336(%struct.ScmObj* %env$ae41336,%struct.ScmObj* %current_45args46758) {
%stackaddr$env-ref48259 = alloca %struct.ScmObj*, align 8
%k40469 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41336, i64 0)
store %struct.ScmObj* %k40469, %struct.ScmObj** %stackaddr$env-ref48259
%stackaddr$env-ref48260 = alloca %struct.ScmObj*, align 8
%acc40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41336, i64 1)
store %struct.ScmObj* %acc40133, %struct.ScmObj** %stackaddr$env-ref48260
%stackaddr$env-ref48261 = alloca %struct.ScmObj*, align 8
%vs40137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41336, i64 2)
store %struct.ScmObj* %vs40137, %struct.ScmObj** %stackaddr$env-ref48261
%stackaddr$env-ref48262 = alloca %struct.ScmObj*, align 8
%f40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41336, i64 3)
store %struct.ScmObj* %f40134, %struct.ScmObj** %stackaddr$env-ref48262
%stackaddr$env-ref48263 = alloca %struct.ScmObj*, align 8
%_37foldr40130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41336, i64 4)
store %struct.ScmObj* %_37foldr40130, %struct.ScmObj** %stackaddr$env-ref48263
%stackaddr$env-ref48264 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41336, i64 5)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48264
%stackaddr$env-ref48265 = alloca %struct.ScmObj*, align 8
%lsts_4340139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41336, i64 6)
store %struct.ScmObj* %lsts_4340139, %struct.ScmObj** %stackaddr$env-ref48265
%stackaddr$prim48266 = alloca %struct.ScmObj*, align 8
%_95k40476 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46758)
store volatile %struct.ScmObj* %_95k40476, %struct.ScmObj** %stackaddr$prim48266, align 8
%stackaddr$prim48267 = alloca %struct.ScmObj*, align 8
%current_45args46759 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46758)
store volatile %struct.ScmObj* %current_45args46759, %struct.ScmObj** %stackaddr$prim48267, align 8
%stackaddr$prim48268 = alloca %struct.ScmObj*, align 8
%anf_45bind40269 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46759)
store volatile %struct.ScmObj* %anf_45bind40269, %struct.ScmObj** %stackaddr$prim48268, align 8
%stackaddr$prim48269 = alloca %struct.ScmObj*, align 8
%anf_45bind40270 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40133, %struct.ScmObj* %lsts_4340139)
store volatile %struct.ScmObj* %anf_45bind40270, %struct.ScmObj** %stackaddr$prim48269, align 8
%stackaddr$prim48270 = alloca %struct.ScmObj*, align 8
%anf_45bind40271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40134, %struct.ScmObj* %anf_45bind40270)
store volatile %struct.ScmObj* %anf_45bind40271, %struct.ScmObj** %stackaddr$prim48270, align 8
%stackaddr$makeclosure48271 = alloca %struct.ScmObj*, align 8
%fptrToInt48272 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41362 to i64
%ae41362 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt48272)
store volatile %struct.ScmObj* %ae41362, %struct.ScmObj** %stackaddr$makeclosure48271, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41362, %struct.ScmObj* %vs40137, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41362, %struct.ScmObj* %f40134, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41362, %struct.ScmObj* %k40469, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41362, %struct.ScmObj* %anf_45bind40269, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41362, %struct.ScmObj* %_37foldr140124, i64 4)
%stackaddr$prim48273 = alloca %struct.ScmObj*, align 8
%cpsargs40480 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41362, %struct.ScmObj* %anf_45bind40271)
store volatile %struct.ScmObj* %cpsargs40480, %struct.ScmObj** %stackaddr$prim48273, align 8
%clofunc48274 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr40130)
musttail call tailcc void %clofunc48274(%struct.ScmObj* %_37foldr40130, %struct.ScmObj* %cpsargs40480)
ret void
}

define tailcc void @proc_clo$ae41362(%struct.ScmObj* %env$ae41362,%struct.ScmObj* %current_45args46761) {
%stackaddr$env-ref48275 = alloca %struct.ScmObj*, align 8
%vs40137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41362, i64 0)
store %struct.ScmObj* %vs40137, %struct.ScmObj** %stackaddr$env-ref48275
%stackaddr$env-ref48276 = alloca %struct.ScmObj*, align 8
%f40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41362, i64 1)
store %struct.ScmObj* %f40134, %struct.ScmObj** %stackaddr$env-ref48276
%stackaddr$env-ref48277 = alloca %struct.ScmObj*, align 8
%k40469 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41362, i64 2)
store %struct.ScmObj* %k40469, %struct.ScmObj** %stackaddr$env-ref48277
%stackaddr$env-ref48278 = alloca %struct.ScmObj*, align 8
%anf_45bind40269 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41362, i64 3)
store %struct.ScmObj* %anf_45bind40269, %struct.ScmObj** %stackaddr$env-ref48278
%stackaddr$env-ref48279 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41362, i64 4)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48279
%stackaddr$prim48280 = alloca %struct.ScmObj*, align 8
%_95k40477 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46761)
store volatile %struct.ScmObj* %_95k40477, %struct.ScmObj** %stackaddr$prim48280, align 8
%stackaddr$prim48281 = alloca %struct.ScmObj*, align 8
%current_45args46762 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46761)
store volatile %struct.ScmObj* %current_45args46762, %struct.ScmObj** %stackaddr$prim48281, align 8
%stackaddr$prim48282 = alloca %struct.ScmObj*, align 8
%anf_45bind40272 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46762)
store volatile %struct.ScmObj* %anf_45bind40272, %struct.ScmObj** %stackaddr$prim48282, align 8
%ae41367 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48283 = alloca %struct.ScmObj*, align 8
%anf_45bind40273 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40272, %struct.ScmObj* %ae41367)
store volatile %struct.ScmObj* %anf_45bind40273, %struct.ScmObj** %stackaddr$prim48283, align 8
%stackaddr$makeclosure48284 = alloca %struct.ScmObj*, align 8
%fptrToInt48285 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41369 to i64
%ae41369 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48285)
store volatile %struct.ScmObj* %ae41369, %struct.ScmObj** %stackaddr$makeclosure48284, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41369, %struct.ScmObj* %f40134, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41369, %struct.ScmObj* %k40469, i64 1)
%args46767$_37foldr140124$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48286 = alloca %struct.ScmObj*, align 8
%args46767$_37foldr140124$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs40137, %struct.ScmObj* %args46767$_37foldr140124$0)
store volatile %struct.ScmObj* %args46767$_37foldr140124$1, %struct.ScmObj** %stackaddr$prim48286, align 8
%stackaddr$prim48287 = alloca %struct.ScmObj*, align 8
%args46767$_37foldr140124$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40273, %struct.ScmObj* %args46767$_37foldr140124$1)
store volatile %struct.ScmObj* %args46767$_37foldr140124$2, %struct.ScmObj** %stackaddr$prim48287, align 8
%stackaddr$prim48288 = alloca %struct.ScmObj*, align 8
%args46767$_37foldr140124$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40269, %struct.ScmObj* %args46767$_37foldr140124$2)
store volatile %struct.ScmObj* %args46767$_37foldr140124$3, %struct.ScmObj** %stackaddr$prim48288, align 8
%stackaddr$prim48289 = alloca %struct.ScmObj*, align 8
%args46767$_37foldr140124$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41369, %struct.ScmObj* %args46767$_37foldr140124$3)
store volatile %struct.ScmObj* %args46767$_37foldr140124$4, %struct.ScmObj** %stackaddr$prim48289, align 8
%clofunc48290 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140124)
musttail call tailcc void %clofunc48290(%struct.ScmObj* %_37foldr140124, %struct.ScmObj* %args46767$_37foldr140124$4)
ret void
}

define tailcc void @proc_clo$ae41369(%struct.ScmObj* %env$ae41369,%struct.ScmObj* %current_45args46764) {
%stackaddr$env-ref48291 = alloca %struct.ScmObj*, align 8
%f40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41369, i64 0)
store %struct.ScmObj* %f40134, %struct.ScmObj** %stackaddr$env-ref48291
%stackaddr$env-ref48292 = alloca %struct.ScmObj*, align 8
%k40469 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41369, i64 1)
store %struct.ScmObj* %k40469, %struct.ScmObj** %stackaddr$env-ref48292
%stackaddr$prim48293 = alloca %struct.ScmObj*, align 8
%_95k40478 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46764)
store volatile %struct.ScmObj* %_95k40478, %struct.ScmObj** %stackaddr$prim48293, align 8
%stackaddr$prim48294 = alloca %struct.ScmObj*, align 8
%current_45args46765 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46764)
store volatile %struct.ScmObj* %current_45args46765, %struct.ScmObj** %stackaddr$prim48294, align 8
%stackaddr$prim48295 = alloca %struct.ScmObj*, align 8
%anf_45bind40274 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46765)
store volatile %struct.ScmObj* %anf_45bind40274, %struct.ScmObj** %stackaddr$prim48295, align 8
%stackaddr$prim48296 = alloca %struct.ScmObj*, align 8
%cpsargs40479 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40469, %struct.ScmObj* %anf_45bind40274)
store volatile %struct.ScmObj* %cpsargs40479, %struct.ScmObj** %stackaddr$prim48296, align 8
%clofunc48297 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40134)
musttail call tailcc void %clofunc48297(%struct.ScmObj* %f40134, %struct.ScmObj* %cpsargs40479)
ret void
}

define tailcc void @proc_clo$ae41338(%struct.ScmObj* %env$ae41338,%struct.ScmObj* %current_45args46768) {
%stackaddr$prim48298 = alloca %struct.ScmObj*, align 8
%k40481 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46768)
store volatile %struct.ScmObj* %k40481, %struct.ScmObj** %stackaddr$prim48298, align 8
%stackaddr$prim48299 = alloca %struct.ScmObj*, align 8
%current_45args46769 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46768)
store volatile %struct.ScmObj* %current_45args46769, %struct.ScmObj** %stackaddr$prim48299, align 8
%stackaddr$prim48300 = alloca %struct.ScmObj*, align 8
%a40142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46769)
store volatile %struct.ScmObj* %a40142, %struct.ScmObj** %stackaddr$prim48300, align 8
%stackaddr$prim48301 = alloca %struct.ScmObj*, align 8
%current_45args46770 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46769)
store volatile %struct.ScmObj* %current_45args46770, %struct.ScmObj** %stackaddr$prim48301, align 8
%stackaddr$prim48302 = alloca %struct.ScmObj*, align 8
%b40141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46770)
store volatile %struct.ScmObj* %b40141, %struct.ScmObj** %stackaddr$prim48302, align 8
%stackaddr$prim48303 = alloca %struct.ScmObj*, align 8
%cpsprim40482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a40142, %struct.ScmObj* %b40141)
store volatile %struct.ScmObj* %cpsprim40482, %struct.ScmObj** %stackaddr$prim48303, align 8
%ae41342 = call %struct.ScmObj* @const_init_int(i64 0)
%args46772$k40481$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48304 = alloca %struct.ScmObj*, align 8
%args46772$k40481$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40482, %struct.ScmObj* %args46772$k40481$0)
store volatile %struct.ScmObj* %args46772$k40481$1, %struct.ScmObj** %stackaddr$prim48304, align 8
%stackaddr$prim48305 = alloca %struct.ScmObj*, align 8
%args46772$k40481$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41342, %struct.ScmObj* %args46772$k40481$1)
store volatile %struct.ScmObj* %args46772$k40481$2, %struct.ScmObj** %stackaddr$prim48305, align 8
%clofunc48306 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40481)
musttail call tailcc void %clofunc48306(%struct.ScmObj* %k40481, %struct.ScmObj* %args46772$k40481$2)
ret void
}

define tailcc void @proc_clo$ae41314(%struct.ScmObj* %env$ae41314,%struct.ScmObj* %current_45args46775) {
%stackaddr$prim48307 = alloca %struct.ScmObj*, align 8
%k40483 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46775)
store volatile %struct.ScmObj* %k40483, %struct.ScmObj** %stackaddr$prim48307, align 8
%stackaddr$prim48308 = alloca %struct.ScmObj*, align 8
%current_45args46776 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46775)
store volatile %struct.ScmObj* %current_45args46776, %struct.ScmObj** %stackaddr$prim48308, align 8
%stackaddr$prim48309 = alloca %struct.ScmObj*, align 8
%x40138 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46776)
store volatile %struct.ScmObj* %x40138, %struct.ScmObj** %stackaddr$prim48309, align 8
%stackaddr$prim48310 = alloca %struct.ScmObj*, align 8
%cpsprim40484 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40138)
store volatile %struct.ScmObj* %cpsprim40484, %struct.ScmObj** %stackaddr$prim48310, align 8
%ae41317 = call %struct.ScmObj* @const_init_int(i64 0)
%args46778$k40483$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48311 = alloca %struct.ScmObj*, align 8
%args46778$k40483$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40484, %struct.ScmObj* %args46778$k40483$0)
store volatile %struct.ScmObj* %args46778$k40483$1, %struct.ScmObj** %stackaddr$prim48311, align 8
%stackaddr$prim48312 = alloca %struct.ScmObj*, align 8
%args46778$k40483$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41317, %struct.ScmObj* %args46778$k40483$1)
store volatile %struct.ScmObj* %args46778$k40483$2, %struct.ScmObj** %stackaddr$prim48312, align 8
%clofunc48313 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40483)
musttail call tailcc void %clofunc48313(%struct.ScmObj* %k40483, %struct.ScmObj* %args46778$k40483$2)
ret void
}

define tailcc void @proc_clo$ae41290(%struct.ScmObj* %env$ae41290,%struct.ScmObj* %current_45args46781) {
%stackaddr$prim48314 = alloca %struct.ScmObj*, align 8
%k40485 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46781)
store volatile %struct.ScmObj* %k40485, %struct.ScmObj** %stackaddr$prim48314, align 8
%stackaddr$prim48315 = alloca %struct.ScmObj*, align 8
%current_45args46782 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46781)
store volatile %struct.ScmObj* %current_45args46782, %struct.ScmObj** %stackaddr$prim48315, align 8
%stackaddr$prim48316 = alloca %struct.ScmObj*, align 8
%x40140 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46782)
store volatile %struct.ScmObj* %x40140, %struct.ScmObj** %stackaddr$prim48316, align 8
%stackaddr$prim48317 = alloca %struct.ScmObj*, align 8
%cpsprim40486 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40140)
store volatile %struct.ScmObj* %cpsprim40486, %struct.ScmObj** %stackaddr$prim48317, align 8
%ae41293 = call %struct.ScmObj* @const_init_int(i64 0)
%args46784$k40485$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48318 = alloca %struct.ScmObj*, align 8
%args46784$k40485$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40486, %struct.ScmObj* %args46784$k40485$0)
store volatile %struct.ScmObj* %args46784$k40485$1, %struct.ScmObj** %stackaddr$prim48318, align 8
%stackaddr$prim48319 = alloca %struct.ScmObj*, align 8
%args46784$k40485$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41293, %struct.ScmObj* %args46784$k40485$1)
store volatile %struct.ScmObj* %args46784$k40485$2, %struct.ScmObj** %stackaddr$prim48319, align 8
%clofunc48320 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40485)
musttail call tailcc void %clofunc48320(%struct.ScmObj* %k40485, %struct.ScmObj* %args46784$k40485$2)
ret void
}

define tailcc void @proc_clo$ae41242(%struct.ScmObj* %env$ae41242,%struct.ScmObj* %current_45args46787) {
%stackaddr$prim48321 = alloca %struct.ScmObj*, align 8
%k40487 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46787)
store volatile %struct.ScmObj* %k40487, %struct.ScmObj** %stackaddr$prim48321, align 8
%stackaddr$prim48322 = alloca %struct.ScmObj*, align 8
%current_45args46788 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46787)
store volatile %struct.ScmObj* %current_45args46788, %struct.ScmObj** %stackaddr$prim48322, align 8
%stackaddr$prim48323 = alloca %struct.ScmObj*, align 8
%lst40136 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46788)
store volatile %struct.ScmObj* %lst40136, %struct.ScmObj** %stackaddr$prim48323, align 8
%stackaddr$prim48324 = alloca %struct.ScmObj*, align 8
%current_45args46789 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46788)
store volatile %struct.ScmObj* %current_45args46789, %struct.ScmObj** %stackaddr$prim48324, align 8
%stackaddr$prim48325 = alloca %struct.ScmObj*, align 8
%b40135 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46789)
store volatile %struct.ScmObj* %b40135, %struct.ScmObj** %stackaddr$prim48325, align 8
%truthy$cmp48326 = call i64 @is_truthy_value(%struct.ScmObj* %b40135)
%cmp$cmp48326 = icmp eq i64 %truthy$cmp48326, 1
br i1 %cmp$cmp48326, label %truebranch$cmp48326, label %falsebranch$cmp48326
truebranch$cmp48326:
%ae41245 = call %struct.ScmObj* @const_init_int(i64 0)
%args46791$k40487$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48327 = alloca %struct.ScmObj*, align 8
%args46791$k40487$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b40135, %struct.ScmObj* %args46791$k40487$0)
store volatile %struct.ScmObj* %args46791$k40487$1, %struct.ScmObj** %stackaddr$prim48327, align 8
%stackaddr$prim48328 = alloca %struct.ScmObj*, align 8
%args46791$k40487$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41245, %struct.ScmObj* %args46791$k40487$1)
store volatile %struct.ScmObj* %args46791$k40487$2, %struct.ScmObj** %stackaddr$prim48328, align 8
%clofunc48329 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40487)
musttail call tailcc void %clofunc48329(%struct.ScmObj* %k40487, %struct.ScmObj* %args46791$k40487$2)
ret void
falsebranch$cmp48326:
%stackaddr$prim48330 = alloca %struct.ScmObj*, align 8
%cpsprim40488 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40136)
store volatile %struct.ScmObj* %cpsprim40488, %struct.ScmObj** %stackaddr$prim48330, align 8
%ae41252 = call %struct.ScmObj* @const_init_int(i64 0)
%args46792$k40487$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48331 = alloca %struct.ScmObj*, align 8
%args46792$k40487$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40488, %struct.ScmObj* %args46792$k40487$0)
store volatile %struct.ScmObj* %args46792$k40487$1, %struct.ScmObj** %stackaddr$prim48331, align 8
%stackaddr$prim48332 = alloca %struct.ScmObj*, align 8
%args46792$k40487$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41252, %struct.ScmObj* %args46792$k40487$1)
store volatile %struct.ScmObj* %args46792$k40487$2, %struct.ScmObj** %stackaddr$prim48332, align 8
%clofunc48333 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40487)
musttail call tailcc void %clofunc48333(%struct.ScmObj* %k40487, %struct.ScmObj* %args46792$k40487$2)
ret void
}

define tailcc void @proc_clo$ae41199(%struct.ScmObj* %env$ae41199,%struct.ScmObj* %current_45args46796) {
%stackaddr$env-ref48334 = alloca %struct.ScmObj*, align 8
%_37take40116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41199, i64 0)
store %struct.ScmObj* %_37take40116, %struct.ScmObj** %stackaddr$env-ref48334
%stackaddr$env-ref48335 = alloca %struct.ScmObj*, align 8
%_37length40113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41199, i64 1)
store %struct.ScmObj* %_37length40113, %struct.ScmObj** %stackaddr$env-ref48335
%stackaddr$prim48336 = alloca %struct.ScmObj*, align 8
%k40489 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46796)
store volatile %struct.ScmObj* %k40489, %struct.ScmObj** %stackaddr$prim48336, align 8
%stackaddr$prim48337 = alloca %struct.ScmObj*, align 8
%current_45args46797 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46796)
store volatile %struct.ScmObj* %current_45args46797, %struct.ScmObj** %stackaddr$prim48337, align 8
%stackaddr$prim48338 = alloca %struct.ScmObj*, align 8
%lst40145 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46797)
store volatile %struct.ScmObj* %lst40145, %struct.ScmObj** %stackaddr$prim48338, align 8
%stackaddr$prim48339 = alloca %struct.ScmObj*, align 8
%current_45args46798 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46797)
store volatile %struct.ScmObj* %current_45args46798, %struct.ScmObj** %stackaddr$prim48339, align 8
%stackaddr$prim48340 = alloca %struct.ScmObj*, align 8
%n40144 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46798)
store volatile %struct.ScmObj* %n40144, %struct.ScmObj** %stackaddr$prim48340, align 8
%stackaddr$makeclosure48341 = alloca %struct.ScmObj*, align 8
%fptrToInt48342 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41201 to i64
%ae41201 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48342)
store volatile %struct.ScmObj* %ae41201, %struct.ScmObj** %stackaddr$makeclosure48341, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41201, %struct.ScmObj* %k40489, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41201, %struct.ScmObj* %_37take40116, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41201, %struct.ScmObj* %lst40145, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41201, %struct.ScmObj* %n40144, i64 3)
%args46804$_37length40113$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48343 = alloca %struct.ScmObj*, align 8
%args46804$_37length40113$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40145, %struct.ScmObj* %args46804$_37length40113$0)
store volatile %struct.ScmObj* %args46804$_37length40113$1, %struct.ScmObj** %stackaddr$prim48343, align 8
%stackaddr$prim48344 = alloca %struct.ScmObj*, align 8
%args46804$_37length40113$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41201, %struct.ScmObj* %args46804$_37length40113$1)
store volatile %struct.ScmObj* %args46804$_37length40113$2, %struct.ScmObj** %stackaddr$prim48344, align 8
%clofunc48345 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length40113)
musttail call tailcc void %clofunc48345(%struct.ScmObj* %_37length40113, %struct.ScmObj* %args46804$_37length40113$2)
ret void
}

define tailcc void @proc_clo$ae41201(%struct.ScmObj* %env$ae41201,%struct.ScmObj* %current_45args46800) {
%stackaddr$env-ref48346 = alloca %struct.ScmObj*, align 8
%k40489 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41201, i64 0)
store %struct.ScmObj* %k40489, %struct.ScmObj** %stackaddr$env-ref48346
%stackaddr$env-ref48347 = alloca %struct.ScmObj*, align 8
%_37take40116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41201, i64 1)
store %struct.ScmObj* %_37take40116, %struct.ScmObj** %stackaddr$env-ref48347
%stackaddr$env-ref48348 = alloca %struct.ScmObj*, align 8
%lst40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41201, i64 2)
store %struct.ScmObj* %lst40145, %struct.ScmObj** %stackaddr$env-ref48348
%stackaddr$env-ref48349 = alloca %struct.ScmObj*, align 8
%n40144 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41201, i64 3)
store %struct.ScmObj* %n40144, %struct.ScmObj** %stackaddr$env-ref48349
%stackaddr$prim48350 = alloca %struct.ScmObj*, align 8
%_95k40490 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46800)
store volatile %struct.ScmObj* %_95k40490, %struct.ScmObj** %stackaddr$prim48350, align 8
%stackaddr$prim48351 = alloca %struct.ScmObj*, align 8
%current_45args46801 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46800)
store volatile %struct.ScmObj* %current_45args46801, %struct.ScmObj** %stackaddr$prim48351, align 8
%stackaddr$prim48352 = alloca %struct.ScmObj*, align 8
%anf_45bind40261 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46801)
store volatile %struct.ScmObj* %anf_45bind40261, %struct.ScmObj** %stackaddr$prim48352, align 8
%stackaddr$prim48353 = alloca %struct.ScmObj*, align 8
%anf_45bind40262 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind40261, %struct.ScmObj* %n40144)
store volatile %struct.ScmObj* %anf_45bind40262, %struct.ScmObj** %stackaddr$prim48353, align 8
%args46803$_37take40116$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48354 = alloca %struct.ScmObj*, align 8
%args46803$_37take40116$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40262, %struct.ScmObj* %args46803$_37take40116$0)
store volatile %struct.ScmObj* %args46803$_37take40116$1, %struct.ScmObj** %stackaddr$prim48354, align 8
%stackaddr$prim48355 = alloca %struct.ScmObj*, align 8
%args46803$_37take40116$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40145, %struct.ScmObj* %args46803$_37take40116$1)
store volatile %struct.ScmObj* %args46803$_37take40116$2, %struct.ScmObj** %stackaddr$prim48355, align 8
%stackaddr$prim48356 = alloca %struct.ScmObj*, align 8
%args46803$_37take40116$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40489, %struct.ScmObj* %args46803$_37take40116$2)
store volatile %struct.ScmObj* %args46803$_37take40116$3, %struct.ScmObj** %stackaddr$prim48356, align 8
%clofunc48357 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take40116)
musttail call tailcc void %clofunc48357(%struct.ScmObj* %_37take40116, %struct.ScmObj* %args46803$_37take40116$3)
ret void
}

define tailcc void @proc_clo$ae41145(%struct.ScmObj* %env$ae41145,%struct.ScmObj* %current_45args46806) {
%stackaddr$env-ref48358 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41145, i64 0)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref48358
%stackaddr$prim48359 = alloca %struct.ScmObj*, align 8
%k40491 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46806)
store volatile %struct.ScmObj* %k40491, %struct.ScmObj** %stackaddr$prim48359, align 8
%stackaddr$prim48360 = alloca %struct.ScmObj*, align 8
%current_45args46807 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46806)
store volatile %struct.ScmObj* %current_45args46807, %struct.ScmObj** %stackaddr$prim48360, align 8
%stackaddr$prim48361 = alloca %struct.ScmObj*, align 8
%lst40147 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46807)
store volatile %struct.ScmObj* %lst40147, %struct.ScmObj** %stackaddr$prim48361, align 8
%stackaddr$makeclosure48362 = alloca %struct.ScmObj*, align 8
%fptrToInt48363 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41146 to i64
%ae41146 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48363)
store volatile %struct.ScmObj* %ae41146, %struct.ScmObj** %stackaddr$makeclosure48362, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41146, %struct.ScmObj* %lst40147, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41146, %struct.ScmObj* %_37foldl140108, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41146, %struct.ScmObj* %k40491, i64 2)
%ae41147 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48364 = alloca %struct.ScmObj*, align 8
%fptrToInt48365 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41148 to i64
%ae41148 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48365)
store volatile %struct.ScmObj* %ae41148, %struct.ScmObj** %stackaddr$makeclosure48364, align 8
%args46818$ae41146$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48366 = alloca %struct.ScmObj*, align 8
%args46818$ae41146$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41148, %struct.ScmObj* %args46818$ae41146$0)
store volatile %struct.ScmObj* %args46818$ae41146$1, %struct.ScmObj** %stackaddr$prim48366, align 8
%stackaddr$prim48367 = alloca %struct.ScmObj*, align 8
%args46818$ae41146$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41147, %struct.ScmObj* %args46818$ae41146$1)
store volatile %struct.ScmObj* %args46818$ae41146$2, %struct.ScmObj** %stackaddr$prim48367, align 8
%clofunc48368 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41146)
musttail call tailcc void %clofunc48368(%struct.ScmObj* %ae41146, %struct.ScmObj* %args46818$ae41146$2)
ret void
}

define tailcc void @proc_clo$ae41146(%struct.ScmObj* %env$ae41146,%struct.ScmObj* %current_45args46809) {
%stackaddr$env-ref48369 = alloca %struct.ScmObj*, align 8
%lst40147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41146, i64 0)
store %struct.ScmObj* %lst40147, %struct.ScmObj** %stackaddr$env-ref48369
%stackaddr$env-ref48370 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41146, i64 1)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref48370
%stackaddr$env-ref48371 = alloca %struct.ScmObj*, align 8
%k40491 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41146, i64 2)
store %struct.ScmObj* %k40491, %struct.ScmObj** %stackaddr$env-ref48371
%stackaddr$prim48372 = alloca %struct.ScmObj*, align 8
%_95k40492 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46809)
store volatile %struct.ScmObj* %_95k40492, %struct.ScmObj** %stackaddr$prim48372, align 8
%stackaddr$prim48373 = alloca %struct.ScmObj*, align 8
%current_45args46810 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46809)
store volatile %struct.ScmObj* %current_45args46810, %struct.ScmObj** %stackaddr$prim48373, align 8
%stackaddr$prim48374 = alloca %struct.ScmObj*, align 8
%anf_45bind40260 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46810)
store volatile %struct.ScmObj* %anf_45bind40260, %struct.ScmObj** %stackaddr$prim48374, align 8
%ae41167 = call %struct.ScmObj* @const_init_null()
%args46812$_37foldl140108$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48375 = alloca %struct.ScmObj*, align 8
%args46812$_37foldl140108$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40147, %struct.ScmObj* %args46812$_37foldl140108$0)
store volatile %struct.ScmObj* %args46812$_37foldl140108$1, %struct.ScmObj** %stackaddr$prim48375, align 8
%stackaddr$prim48376 = alloca %struct.ScmObj*, align 8
%args46812$_37foldl140108$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41167, %struct.ScmObj* %args46812$_37foldl140108$1)
store volatile %struct.ScmObj* %args46812$_37foldl140108$2, %struct.ScmObj** %stackaddr$prim48376, align 8
%stackaddr$prim48377 = alloca %struct.ScmObj*, align 8
%args46812$_37foldl140108$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40260, %struct.ScmObj* %args46812$_37foldl140108$2)
store volatile %struct.ScmObj* %args46812$_37foldl140108$3, %struct.ScmObj** %stackaddr$prim48377, align 8
%stackaddr$prim48378 = alloca %struct.ScmObj*, align 8
%args46812$_37foldl140108$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40491, %struct.ScmObj* %args46812$_37foldl140108$3)
store volatile %struct.ScmObj* %args46812$_37foldl140108$4, %struct.ScmObj** %stackaddr$prim48378, align 8
%clofunc48379 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140108)
musttail call tailcc void %clofunc48379(%struct.ScmObj* %_37foldl140108, %struct.ScmObj* %args46812$_37foldl140108$4)
ret void
}

define tailcc void @proc_clo$ae41148(%struct.ScmObj* %env$ae41148,%struct.ScmObj* %current_45args46813) {
%stackaddr$prim48380 = alloca %struct.ScmObj*, align 8
%k40493 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46813)
store volatile %struct.ScmObj* %k40493, %struct.ScmObj** %stackaddr$prim48380, align 8
%stackaddr$prim48381 = alloca %struct.ScmObj*, align 8
%current_45args46814 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46813)
store volatile %struct.ScmObj* %current_45args46814, %struct.ScmObj** %stackaddr$prim48381, align 8
%stackaddr$prim48382 = alloca %struct.ScmObj*, align 8
%x40149 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46814)
store volatile %struct.ScmObj* %x40149, %struct.ScmObj** %stackaddr$prim48382, align 8
%stackaddr$prim48383 = alloca %struct.ScmObj*, align 8
%current_45args46815 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46814)
store volatile %struct.ScmObj* %current_45args46815, %struct.ScmObj** %stackaddr$prim48383, align 8
%stackaddr$prim48384 = alloca %struct.ScmObj*, align 8
%y40148 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46815)
store volatile %struct.ScmObj* %y40148, %struct.ScmObj** %stackaddr$prim48384, align 8
%ae41150 = call %struct.ScmObj* @const_init_int(i64 0)
%args46817$k40493$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48385 = alloca %struct.ScmObj*, align 8
%args46817$k40493$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x40149, %struct.ScmObj* %args46817$k40493$0)
store volatile %struct.ScmObj* %args46817$k40493$1, %struct.ScmObj** %stackaddr$prim48385, align 8
%stackaddr$prim48386 = alloca %struct.ScmObj*, align 8
%args46817$k40493$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41150, %struct.ScmObj* %args46817$k40493$1)
store volatile %struct.ScmObj* %args46817$k40493$2, %struct.ScmObj** %stackaddr$prim48386, align 8
%clofunc48387 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40493)
musttail call tailcc void %clofunc48387(%struct.ScmObj* %k40493, %struct.ScmObj* %args46817$k40493$2)
ret void
}

define tailcc void @proc_clo$ae41066(%struct.ScmObj* %env$ae41066,%struct.ScmObj* %current_45args46821) {
%stackaddr$prim48388 = alloca %struct.ScmObj*, align 8
%k40494 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46821)
store volatile %struct.ScmObj* %k40494, %struct.ScmObj** %stackaddr$prim48388, align 8
%stackaddr$prim48389 = alloca %struct.ScmObj*, align 8
%current_45args46822 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46821)
store volatile %struct.ScmObj* %current_45args46822, %struct.ScmObj** %stackaddr$prim48389, align 8
%stackaddr$prim48390 = alloca %struct.ScmObj*, align 8
%_37foldl140109 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46822)
store volatile %struct.ScmObj* %_37foldl140109, %struct.ScmObj** %stackaddr$prim48390, align 8
%ae41068 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48391 = alloca %struct.ScmObj*, align 8
%fptrToInt48392 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41069 to i64
%ae41069 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48392)
store volatile %struct.ScmObj* %ae41069, %struct.ScmObj** %stackaddr$makeclosure48391, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41069, %struct.ScmObj* %_37foldl140109, i64 0)
%args46835$k40494$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48393 = alloca %struct.ScmObj*, align 8
%args46835$k40494$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41069, %struct.ScmObj* %args46835$k40494$0)
store volatile %struct.ScmObj* %args46835$k40494$1, %struct.ScmObj** %stackaddr$prim48393, align 8
%stackaddr$prim48394 = alloca %struct.ScmObj*, align 8
%args46835$k40494$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41068, %struct.ScmObj* %args46835$k40494$1)
store volatile %struct.ScmObj* %args46835$k40494$2, %struct.ScmObj** %stackaddr$prim48394, align 8
%clofunc48395 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40494)
musttail call tailcc void %clofunc48395(%struct.ScmObj* %k40494, %struct.ScmObj* %args46835$k40494$2)
ret void
}

define tailcc void @proc_clo$ae41069(%struct.ScmObj* %env$ae41069,%struct.ScmObj* %current_45args46824) {
%stackaddr$env-ref48396 = alloca %struct.ScmObj*, align 8
%_37foldl140109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41069, i64 0)
store %struct.ScmObj* %_37foldl140109, %struct.ScmObj** %stackaddr$env-ref48396
%stackaddr$prim48397 = alloca %struct.ScmObj*, align 8
%k40495 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46824)
store volatile %struct.ScmObj* %k40495, %struct.ScmObj** %stackaddr$prim48397, align 8
%stackaddr$prim48398 = alloca %struct.ScmObj*, align 8
%current_45args46825 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46824)
store volatile %struct.ScmObj* %current_45args46825, %struct.ScmObj** %stackaddr$prim48398, align 8
%stackaddr$prim48399 = alloca %struct.ScmObj*, align 8
%f40112 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46825)
store volatile %struct.ScmObj* %f40112, %struct.ScmObj** %stackaddr$prim48399, align 8
%stackaddr$prim48400 = alloca %struct.ScmObj*, align 8
%current_45args46826 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46825)
store volatile %struct.ScmObj* %current_45args46826, %struct.ScmObj** %stackaddr$prim48400, align 8
%stackaddr$prim48401 = alloca %struct.ScmObj*, align 8
%acc40111 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46826)
store volatile %struct.ScmObj* %acc40111, %struct.ScmObj** %stackaddr$prim48401, align 8
%stackaddr$prim48402 = alloca %struct.ScmObj*, align 8
%current_45args46827 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46826)
store volatile %struct.ScmObj* %current_45args46827, %struct.ScmObj** %stackaddr$prim48402, align 8
%stackaddr$prim48403 = alloca %struct.ScmObj*, align 8
%lst40110 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46827)
store volatile %struct.ScmObj* %lst40110, %struct.ScmObj** %stackaddr$prim48403, align 8
%stackaddr$prim48404 = alloca %struct.ScmObj*, align 8
%anf_45bind40255 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40110)
store volatile %struct.ScmObj* %anf_45bind40255, %struct.ScmObj** %stackaddr$prim48404, align 8
%truthy$cmp48405 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40255)
%cmp$cmp48405 = icmp eq i64 %truthy$cmp48405, 1
br i1 %cmp$cmp48405, label %truebranch$cmp48405, label %falsebranch$cmp48405
truebranch$cmp48405:
%ae41073 = call %struct.ScmObj* @const_init_int(i64 0)
%args46829$k40495$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48406 = alloca %struct.ScmObj*, align 8
%args46829$k40495$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40111, %struct.ScmObj* %args46829$k40495$0)
store volatile %struct.ScmObj* %args46829$k40495$1, %struct.ScmObj** %stackaddr$prim48406, align 8
%stackaddr$prim48407 = alloca %struct.ScmObj*, align 8
%args46829$k40495$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41073, %struct.ScmObj* %args46829$k40495$1)
store volatile %struct.ScmObj* %args46829$k40495$2, %struct.ScmObj** %stackaddr$prim48407, align 8
%clofunc48408 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40495)
musttail call tailcc void %clofunc48408(%struct.ScmObj* %k40495, %struct.ScmObj* %args46829$k40495$2)
ret void
falsebranch$cmp48405:
%stackaddr$prim48409 = alloca %struct.ScmObj*, align 8
%anf_45bind40256 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40110)
store volatile %struct.ScmObj* %anf_45bind40256, %struct.ScmObj** %stackaddr$prim48409, align 8
%stackaddr$makeclosure48410 = alloca %struct.ScmObj*, align 8
%fptrToInt48411 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41080 to i64
%ae41080 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48411)
store volatile %struct.ScmObj* %ae41080, %struct.ScmObj** %stackaddr$makeclosure48410, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41080, %struct.ScmObj* %f40112, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41080, %struct.ScmObj* %k40495, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41080, %struct.ScmObj* %lst40110, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41080, %struct.ScmObj* %_37foldl140109, i64 3)
%args46834$f40112$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48412 = alloca %struct.ScmObj*, align 8
%args46834$f40112$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40111, %struct.ScmObj* %args46834$f40112$0)
store volatile %struct.ScmObj* %args46834$f40112$1, %struct.ScmObj** %stackaddr$prim48412, align 8
%stackaddr$prim48413 = alloca %struct.ScmObj*, align 8
%args46834$f40112$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40256, %struct.ScmObj* %args46834$f40112$1)
store volatile %struct.ScmObj* %args46834$f40112$2, %struct.ScmObj** %stackaddr$prim48413, align 8
%stackaddr$prim48414 = alloca %struct.ScmObj*, align 8
%args46834$f40112$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41080, %struct.ScmObj* %args46834$f40112$2)
store volatile %struct.ScmObj* %args46834$f40112$3, %struct.ScmObj** %stackaddr$prim48414, align 8
%clofunc48415 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40112)
musttail call tailcc void %clofunc48415(%struct.ScmObj* %f40112, %struct.ScmObj* %args46834$f40112$3)
ret void
}

define tailcc void @proc_clo$ae41080(%struct.ScmObj* %env$ae41080,%struct.ScmObj* %current_45args46830) {
%stackaddr$env-ref48416 = alloca %struct.ScmObj*, align 8
%f40112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41080, i64 0)
store %struct.ScmObj* %f40112, %struct.ScmObj** %stackaddr$env-ref48416
%stackaddr$env-ref48417 = alloca %struct.ScmObj*, align 8
%k40495 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41080, i64 1)
store %struct.ScmObj* %k40495, %struct.ScmObj** %stackaddr$env-ref48417
%stackaddr$env-ref48418 = alloca %struct.ScmObj*, align 8
%lst40110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41080, i64 2)
store %struct.ScmObj* %lst40110, %struct.ScmObj** %stackaddr$env-ref48418
%stackaddr$env-ref48419 = alloca %struct.ScmObj*, align 8
%_37foldl140109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41080, i64 3)
store %struct.ScmObj* %_37foldl140109, %struct.ScmObj** %stackaddr$env-ref48419
%stackaddr$prim48420 = alloca %struct.ScmObj*, align 8
%_95k40496 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46830)
store volatile %struct.ScmObj* %_95k40496, %struct.ScmObj** %stackaddr$prim48420, align 8
%stackaddr$prim48421 = alloca %struct.ScmObj*, align 8
%current_45args46831 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46830)
store volatile %struct.ScmObj* %current_45args46831, %struct.ScmObj** %stackaddr$prim48421, align 8
%stackaddr$prim48422 = alloca %struct.ScmObj*, align 8
%anf_45bind40257 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46831)
store volatile %struct.ScmObj* %anf_45bind40257, %struct.ScmObj** %stackaddr$prim48422, align 8
%stackaddr$prim48423 = alloca %struct.ScmObj*, align 8
%anf_45bind40258 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40110)
store volatile %struct.ScmObj* %anf_45bind40258, %struct.ScmObj** %stackaddr$prim48423, align 8
%args46833$_37foldl140109$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48424 = alloca %struct.ScmObj*, align 8
%args46833$_37foldl140109$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40258, %struct.ScmObj* %args46833$_37foldl140109$0)
store volatile %struct.ScmObj* %args46833$_37foldl140109$1, %struct.ScmObj** %stackaddr$prim48424, align 8
%stackaddr$prim48425 = alloca %struct.ScmObj*, align 8
%args46833$_37foldl140109$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40257, %struct.ScmObj* %args46833$_37foldl140109$1)
store volatile %struct.ScmObj* %args46833$_37foldl140109$2, %struct.ScmObj** %stackaddr$prim48425, align 8
%stackaddr$prim48426 = alloca %struct.ScmObj*, align 8
%args46833$_37foldl140109$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40112, %struct.ScmObj* %args46833$_37foldl140109$2)
store volatile %struct.ScmObj* %args46833$_37foldl140109$3, %struct.ScmObj** %stackaddr$prim48426, align 8
%stackaddr$prim48427 = alloca %struct.ScmObj*, align 8
%args46833$_37foldl140109$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40495, %struct.ScmObj* %args46833$_37foldl140109$3)
store volatile %struct.ScmObj* %args46833$_37foldl140109$4, %struct.ScmObj** %stackaddr$prim48427, align 8
%clofunc48428 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140109)
musttail call tailcc void %clofunc48428(%struct.ScmObj* %_37foldl140109, %struct.ScmObj* %args46833$_37foldl140109$4)
ret void
}

define tailcc void @proc_clo$ae40983(%struct.ScmObj* %env$ae40983,%struct.ScmObj* %current_45args46838) {
%stackaddr$prim48429 = alloca %struct.ScmObj*, align 8
%k40497 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46838)
store volatile %struct.ScmObj* %k40497, %struct.ScmObj** %stackaddr$prim48429, align 8
%stackaddr$prim48430 = alloca %struct.ScmObj*, align 8
%current_45args46839 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46838)
store volatile %struct.ScmObj* %current_45args46839, %struct.ScmObj** %stackaddr$prim48430, align 8
%stackaddr$prim48431 = alloca %struct.ScmObj*, align 8
%_37length40114 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46839)
store volatile %struct.ScmObj* %_37length40114, %struct.ScmObj** %stackaddr$prim48431, align 8
%ae40985 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48432 = alloca %struct.ScmObj*, align 8
%fptrToInt48433 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40986 to i64
%ae40986 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48433)
store volatile %struct.ScmObj* %ae40986, %struct.ScmObj** %stackaddr$makeclosure48432, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40986, %struct.ScmObj* %_37length40114, i64 0)
%args46850$k40497$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48434 = alloca %struct.ScmObj*, align 8
%args46850$k40497$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40986, %struct.ScmObj* %args46850$k40497$0)
store volatile %struct.ScmObj* %args46850$k40497$1, %struct.ScmObj** %stackaddr$prim48434, align 8
%stackaddr$prim48435 = alloca %struct.ScmObj*, align 8
%args46850$k40497$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40985, %struct.ScmObj* %args46850$k40497$1)
store volatile %struct.ScmObj* %args46850$k40497$2, %struct.ScmObj** %stackaddr$prim48435, align 8
%clofunc48436 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40497)
musttail call tailcc void %clofunc48436(%struct.ScmObj* %k40497, %struct.ScmObj* %args46850$k40497$2)
ret void
}

define tailcc void @proc_clo$ae40986(%struct.ScmObj* %env$ae40986,%struct.ScmObj* %current_45args46841) {
%stackaddr$env-ref48437 = alloca %struct.ScmObj*, align 8
%_37length40114 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40986, i64 0)
store %struct.ScmObj* %_37length40114, %struct.ScmObj** %stackaddr$env-ref48437
%stackaddr$prim48438 = alloca %struct.ScmObj*, align 8
%k40498 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46841)
store volatile %struct.ScmObj* %k40498, %struct.ScmObj** %stackaddr$prim48438, align 8
%stackaddr$prim48439 = alloca %struct.ScmObj*, align 8
%current_45args46842 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46841)
store volatile %struct.ScmObj* %current_45args46842, %struct.ScmObj** %stackaddr$prim48439, align 8
%stackaddr$prim48440 = alloca %struct.ScmObj*, align 8
%lst40115 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46842)
store volatile %struct.ScmObj* %lst40115, %struct.ScmObj** %stackaddr$prim48440, align 8
%stackaddr$prim48441 = alloca %struct.ScmObj*, align 8
%anf_45bind40251 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40115)
store volatile %struct.ScmObj* %anf_45bind40251, %struct.ScmObj** %stackaddr$prim48441, align 8
%truthy$cmp48442 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40251)
%cmp$cmp48442 = icmp eq i64 %truthy$cmp48442, 1
br i1 %cmp$cmp48442, label %truebranch$cmp48442, label %falsebranch$cmp48442
truebranch$cmp48442:
%ae40990 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40991 = call %struct.ScmObj* @const_init_int(i64 0)
%args46844$k40498$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48443 = alloca %struct.ScmObj*, align 8
%args46844$k40498$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40991, %struct.ScmObj* %args46844$k40498$0)
store volatile %struct.ScmObj* %args46844$k40498$1, %struct.ScmObj** %stackaddr$prim48443, align 8
%stackaddr$prim48444 = alloca %struct.ScmObj*, align 8
%args46844$k40498$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40990, %struct.ScmObj* %args46844$k40498$1)
store volatile %struct.ScmObj* %args46844$k40498$2, %struct.ScmObj** %stackaddr$prim48444, align 8
%clofunc48445 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40498)
musttail call tailcc void %clofunc48445(%struct.ScmObj* %k40498, %struct.ScmObj* %args46844$k40498$2)
ret void
falsebranch$cmp48442:
%stackaddr$prim48446 = alloca %struct.ScmObj*, align 8
%anf_45bind40252 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40115)
store volatile %struct.ScmObj* %anf_45bind40252, %struct.ScmObj** %stackaddr$prim48446, align 8
%stackaddr$makeclosure48447 = alloca %struct.ScmObj*, align 8
%fptrToInt48448 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41000 to i64
%ae41000 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48448)
store volatile %struct.ScmObj* %ae41000, %struct.ScmObj** %stackaddr$makeclosure48447, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41000, %struct.ScmObj* %k40498, i64 0)
%args46849$_37length40114$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48449 = alloca %struct.ScmObj*, align 8
%args46849$_37length40114$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40252, %struct.ScmObj* %args46849$_37length40114$0)
store volatile %struct.ScmObj* %args46849$_37length40114$1, %struct.ScmObj** %stackaddr$prim48449, align 8
%stackaddr$prim48450 = alloca %struct.ScmObj*, align 8
%args46849$_37length40114$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41000, %struct.ScmObj* %args46849$_37length40114$1)
store volatile %struct.ScmObj* %args46849$_37length40114$2, %struct.ScmObj** %stackaddr$prim48450, align 8
%clofunc48451 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length40114)
musttail call tailcc void %clofunc48451(%struct.ScmObj* %_37length40114, %struct.ScmObj* %args46849$_37length40114$2)
ret void
}

define tailcc void @proc_clo$ae41000(%struct.ScmObj* %env$ae41000,%struct.ScmObj* %current_45args46845) {
%stackaddr$env-ref48452 = alloca %struct.ScmObj*, align 8
%k40498 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41000, i64 0)
store %struct.ScmObj* %k40498, %struct.ScmObj** %stackaddr$env-ref48452
%stackaddr$prim48453 = alloca %struct.ScmObj*, align 8
%_95k40499 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46845)
store volatile %struct.ScmObj* %_95k40499, %struct.ScmObj** %stackaddr$prim48453, align 8
%stackaddr$prim48454 = alloca %struct.ScmObj*, align 8
%current_45args46846 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46845)
store volatile %struct.ScmObj* %current_45args46846, %struct.ScmObj** %stackaddr$prim48454, align 8
%stackaddr$prim48455 = alloca %struct.ScmObj*, align 8
%anf_45bind40253 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46846)
store volatile %struct.ScmObj* %anf_45bind40253, %struct.ScmObj** %stackaddr$prim48455, align 8
%ae41002 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim48456 = alloca %struct.ScmObj*, align 8
%cpsprim40500 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae41002, %struct.ScmObj* %anf_45bind40253)
store volatile %struct.ScmObj* %cpsprim40500, %struct.ScmObj** %stackaddr$prim48456, align 8
%ae41005 = call %struct.ScmObj* @const_init_int(i64 0)
%args46848$k40498$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48457 = alloca %struct.ScmObj*, align 8
%args46848$k40498$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40500, %struct.ScmObj* %args46848$k40498$0)
store volatile %struct.ScmObj* %args46848$k40498$1, %struct.ScmObj** %stackaddr$prim48457, align 8
%stackaddr$prim48458 = alloca %struct.ScmObj*, align 8
%args46848$k40498$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41005, %struct.ScmObj* %args46848$k40498$1)
store volatile %struct.ScmObj* %args46848$k40498$2, %struct.ScmObj** %stackaddr$prim48458, align 8
%clofunc48459 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40498)
musttail call tailcc void %clofunc48459(%struct.ScmObj* %k40498, %struct.ScmObj* %args46848$k40498$2)
ret void
}

define tailcc void @proc_clo$ae40833(%struct.ScmObj* %env$ae40833,%struct.ScmObj* %current_45args46853) {
%stackaddr$prim48460 = alloca %struct.ScmObj*, align 8
%k40501 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46853)
store volatile %struct.ScmObj* %k40501, %struct.ScmObj** %stackaddr$prim48460, align 8
%stackaddr$prim48461 = alloca %struct.ScmObj*, align 8
%current_45args46854 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46853)
store volatile %struct.ScmObj* %current_45args46854, %struct.ScmObj** %stackaddr$prim48461, align 8
%stackaddr$prim48462 = alloca %struct.ScmObj*, align 8
%_37take40117 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46854)
store volatile %struct.ScmObj* %_37take40117, %struct.ScmObj** %stackaddr$prim48462, align 8
%ae40835 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48463 = alloca %struct.ScmObj*, align 8
%fptrToInt48464 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40836 to i64
%ae40836 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48464)
store volatile %struct.ScmObj* %ae40836, %struct.ScmObj** %stackaddr$makeclosure48463, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40836, %struct.ScmObj* %_37take40117, i64 0)
%args46867$k40501$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48465 = alloca %struct.ScmObj*, align 8
%args46867$k40501$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40836, %struct.ScmObj* %args46867$k40501$0)
store volatile %struct.ScmObj* %args46867$k40501$1, %struct.ScmObj** %stackaddr$prim48465, align 8
%stackaddr$prim48466 = alloca %struct.ScmObj*, align 8
%args46867$k40501$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40835, %struct.ScmObj* %args46867$k40501$1)
store volatile %struct.ScmObj* %args46867$k40501$2, %struct.ScmObj** %stackaddr$prim48466, align 8
%clofunc48467 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40501)
musttail call tailcc void %clofunc48467(%struct.ScmObj* %k40501, %struct.ScmObj* %args46867$k40501$2)
ret void
}

define tailcc void @proc_clo$ae40836(%struct.ScmObj* %env$ae40836,%struct.ScmObj* %current_45args46856) {
%stackaddr$env-ref48468 = alloca %struct.ScmObj*, align 8
%_37take40117 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40836, i64 0)
store %struct.ScmObj* %_37take40117, %struct.ScmObj** %stackaddr$env-ref48468
%stackaddr$prim48469 = alloca %struct.ScmObj*, align 8
%k40502 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46856)
store volatile %struct.ScmObj* %k40502, %struct.ScmObj** %stackaddr$prim48469, align 8
%stackaddr$prim48470 = alloca %struct.ScmObj*, align 8
%current_45args46857 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46856)
store volatile %struct.ScmObj* %current_45args46857, %struct.ScmObj** %stackaddr$prim48470, align 8
%stackaddr$prim48471 = alloca %struct.ScmObj*, align 8
%lst40119 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46857)
store volatile %struct.ScmObj* %lst40119, %struct.ScmObj** %stackaddr$prim48471, align 8
%stackaddr$prim48472 = alloca %struct.ScmObj*, align 8
%current_45args46858 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46857)
store volatile %struct.ScmObj* %current_45args46858, %struct.ScmObj** %stackaddr$prim48472, align 8
%stackaddr$prim48473 = alloca %struct.ScmObj*, align 8
%n40118 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46858)
store volatile %struct.ScmObj* %n40118, %struct.ScmObj** %stackaddr$prim48473, align 8
%ae40838 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48474 = alloca %struct.ScmObj*, align 8
%anf_45bind40244 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n40118, %struct.ScmObj* %ae40838)
store volatile %struct.ScmObj* %anf_45bind40244, %struct.ScmObj** %stackaddr$prim48474, align 8
%truthy$cmp48475 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40244)
%cmp$cmp48475 = icmp eq i64 %truthy$cmp48475, 1
br i1 %cmp$cmp48475, label %truebranch$cmp48475, label %falsebranch$cmp48475
truebranch$cmp48475:
%ae40841 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40842 = call %struct.ScmObj* @const_init_null()
%args46860$k40502$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48476 = alloca %struct.ScmObj*, align 8
%args46860$k40502$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40842, %struct.ScmObj* %args46860$k40502$0)
store volatile %struct.ScmObj* %args46860$k40502$1, %struct.ScmObj** %stackaddr$prim48476, align 8
%stackaddr$prim48477 = alloca %struct.ScmObj*, align 8
%args46860$k40502$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40841, %struct.ScmObj* %args46860$k40502$1)
store volatile %struct.ScmObj* %args46860$k40502$2, %struct.ScmObj** %stackaddr$prim48477, align 8
%clofunc48478 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40502)
musttail call tailcc void %clofunc48478(%struct.ScmObj* %k40502, %struct.ScmObj* %args46860$k40502$2)
ret void
falsebranch$cmp48475:
%stackaddr$prim48479 = alloca %struct.ScmObj*, align 8
%anf_45bind40245 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40119)
store volatile %struct.ScmObj* %anf_45bind40245, %struct.ScmObj** %stackaddr$prim48479, align 8
%truthy$cmp48480 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40245)
%cmp$cmp48480 = icmp eq i64 %truthy$cmp48480, 1
br i1 %cmp$cmp48480, label %truebranch$cmp48480, label %falsebranch$cmp48480
truebranch$cmp48480:
%ae40852 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40853 = call %struct.ScmObj* @const_init_null()
%args46861$k40502$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48481 = alloca %struct.ScmObj*, align 8
%args46861$k40502$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40853, %struct.ScmObj* %args46861$k40502$0)
store volatile %struct.ScmObj* %args46861$k40502$1, %struct.ScmObj** %stackaddr$prim48481, align 8
%stackaddr$prim48482 = alloca %struct.ScmObj*, align 8
%args46861$k40502$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40852, %struct.ScmObj* %args46861$k40502$1)
store volatile %struct.ScmObj* %args46861$k40502$2, %struct.ScmObj** %stackaddr$prim48482, align 8
%clofunc48483 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40502)
musttail call tailcc void %clofunc48483(%struct.ScmObj* %k40502, %struct.ScmObj* %args46861$k40502$2)
ret void
falsebranch$cmp48480:
%stackaddr$prim48484 = alloca %struct.ScmObj*, align 8
%anf_45bind40246 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40119)
store volatile %struct.ScmObj* %anf_45bind40246, %struct.ScmObj** %stackaddr$prim48484, align 8
%stackaddr$prim48485 = alloca %struct.ScmObj*, align 8
%anf_45bind40247 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40119)
store volatile %struct.ScmObj* %anf_45bind40247, %struct.ScmObj** %stackaddr$prim48485, align 8
%ae40863 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim48486 = alloca %struct.ScmObj*, align 8
%anf_45bind40248 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n40118, %struct.ScmObj* %ae40863)
store volatile %struct.ScmObj* %anf_45bind40248, %struct.ScmObj** %stackaddr$prim48486, align 8
%stackaddr$makeclosure48487 = alloca %struct.ScmObj*, align 8
%fptrToInt48488 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40865 to i64
%ae40865 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48488)
store volatile %struct.ScmObj* %ae40865, %struct.ScmObj** %stackaddr$makeclosure48487, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40865, %struct.ScmObj* %k40502, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40865, %struct.ScmObj* %anf_45bind40246, i64 1)
%args46866$_37take40117$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48489 = alloca %struct.ScmObj*, align 8
%args46866$_37take40117$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40248, %struct.ScmObj* %args46866$_37take40117$0)
store volatile %struct.ScmObj* %args46866$_37take40117$1, %struct.ScmObj** %stackaddr$prim48489, align 8
%stackaddr$prim48490 = alloca %struct.ScmObj*, align 8
%args46866$_37take40117$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40247, %struct.ScmObj* %args46866$_37take40117$1)
store volatile %struct.ScmObj* %args46866$_37take40117$2, %struct.ScmObj** %stackaddr$prim48490, align 8
%stackaddr$prim48491 = alloca %struct.ScmObj*, align 8
%args46866$_37take40117$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40865, %struct.ScmObj* %args46866$_37take40117$2)
store volatile %struct.ScmObj* %args46866$_37take40117$3, %struct.ScmObj** %stackaddr$prim48491, align 8
%clofunc48492 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take40117)
musttail call tailcc void %clofunc48492(%struct.ScmObj* %_37take40117, %struct.ScmObj* %args46866$_37take40117$3)
ret void
}

define tailcc void @proc_clo$ae40865(%struct.ScmObj* %env$ae40865,%struct.ScmObj* %current_45args46862) {
%stackaddr$env-ref48493 = alloca %struct.ScmObj*, align 8
%k40502 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40865, i64 0)
store %struct.ScmObj* %k40502, %struct.ScmObj** %stackaddr$env-ref48493
%stackaddr$env-ref48494 = alloca %struct.ScmObj*, align 8
%anf_45bind40246 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40865, i64 1)
store %struct.ScmObj* %anf_45bind40246, %struct.ScmObj** %stackaddr$env-ref48494
%stackaddr$prim48495 = alloca %struct.ScmObj*, align 8
%_95k40503 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46862)
store volatile %struct.ScmObj* %_95k40503, %struct.ScmObj** %stackaddr$prim48495, align 8
%stackaddr$prim48496 = alloca %struct.ScmObj*, align 8
%current_45args46863 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46862)
store volatile %struct.ScmObj* %current_45args46863, %struct.ScmObj** %stackaddr$prim48496, align 8
%stackaddr$prim48497 = alloca %struct.ScmObj*, align 8
%anf_45bind40249 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46863)
store volatile %struct.ScmObj* %anf_45bind40249, %struct.ScmObj** %stackaddr$prim48497, align 8
%stackaddr$prim48498 = alloca %struct.ScmObj*, align 8
%cpsprim40504 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40246, %struct.ScmObj* %anf_45bind40249)
store volatile %struct.ScmObj* %cpsprim40504, %struct.ScmObj** %stackaddr$prim48498, align 8
%ae40871 = call %struct.ScmObj* @const_init_int(i64 0)
%args46865$k40502$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48499 = alloca %struct.ScmObj*, align 8
%args46865$k40502$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40504, %struct.ScmObj* %args46865$k40502$0)
store volatile %struct.ScmObj* %args46865$k40502$1, %struct.ScmObj** %stackaddr$prim48499, align 8
%stackaddr$prim48500 = alloca %struct.ScmObj*, align 8
%args46865$k40502$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40871, %struct.ScmObj* %args46865$k40502$1)
store volatile %struct.ScmObj* %args46865$k40502$2, %struct.ScmObj** %stackaddr$prim48500, align 8
%clofunc48501 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40502)
musttail call tailcc void %clofunc48501(%struct.ScmObj* %k40502, %struct.ScmObj* %args46865$k40502$2)
ret void
}

define tailcc void @proc_clo$ae40736(%struct.ScmObj* %env$ae40736,%struct.ScmObj* %current_45args46870) {
%stackaddr$prim48502 = alloca %struct.ScmObj*, align 8
%k40505 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46870)
store volatile %struct.ScmObj* %k40505, %struct.ScmObj** %stackaddr$prim48502, align 8
%stackaddr$prim48503 = alloca %struct.ScmObj*, align 8
%current_45args46871 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46870)
store volatile %struct.ScmObj* %current_45args46871, %struct.ScmObj** %stackaddr$prim48503, align 8
%stackaddr$prim48504 = alloca %struct.ScmObj*, align 8
%_37map40121 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46871)
store volatile %struct.ScmObj* %_37map40121, %struct.ScmObj** %stackaddr$prim48504, align 8
%ae40738 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48505 = alloca %struct.ScmObj*, align 8
%fptrToInt48506 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40739 to i64
%ae40739 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48506)
store volatile %struct.ScmObj* %ae40739, %struct.ScmObj** %stackaddr$makeclosure48505, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40739, %struct.ScmObj* %_37map40121, i64 0)
%args46887$k40505$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48507 = alloca %struct.ScmObj*, align 8
%args46887$k40505$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40739, %struct.ScmObj* %args46887$k40505$0)
store volatile %struct.ScmObj* %args46887$k40505$1, %struct.ScmObj** %stackaddr$prim48507, align 8
%stackaddr$prim48508 = alloca %struct.ScmObj*, align 8
%args46887$k40505$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40738, %struct.ScmObj* %args46887$k40505$1)
store volatile %struct.ScmObj* %args46887$k40505$2, %struct.ScmObj** %stackaddr$prim48508, align 8
%clofunc48509 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40505)
musttail call tailcc void %clofunc48509(%struct.ScmObj* %k40505, %struct.ScmObj* %args46887$k40505$2)
ret void
}

define tailcc void @proc_clo$ae40739(%struct.ScmObj* %env$ae40739,%struct.ScmObj* %current_45args46873) {
%stackaddr$env-ref48510 = alloca %struct.ScmObj*, align 8
%_37map40121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40739, i64 0)
store %struct.ScmObj* %_37map40121, %struct.ScmObj** %stackaddr$env-ref48510
%stackaddr$prim48511 = alloca %struct.ScmObj*, align 8
%k40506 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46873)
store volatile %struct.ScmObj* %k40506, %struct.ScmObj** %stackaddr$prim48511, align 8
%stackaddr$prim48512 = alloca %struct.ScmObj*, align 8
%current_45args46874 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46873)
store volatile %struct.ScmObj* %current_45args46874, %struct.ScmObj** %stackaddr$prim48512, align 8
%stackaddr$prim48513 = alloca %struct.ScmObj*, align 8
%f40123 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46874)
store volatile %struct.ScmObj* %f40123, %struct.ScmObj** %stackaddr$prim48513, align 8
%stackaddr$prim48514 = alloca %struct.ScmObj*, align 8
%current_45args46875 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46874)
store volatile %struct.ScmObj* %current_45args46875, %struct.ScmObj** %stackaddr$prim48514, align 8
%stackaddr$prim48515 = alloca %struct.ScmObj*, align 8
%lst40122 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46875)
store volatile %struct.ScmObj* %lst40122, %struct.ScmObj** %stackaddr$prim48515, align 8
%stackaddr$prim48516 = alloca %struct.ScmObj*, align 8
%anf_45bind40238 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40122)
store volatile %struct.ScmObj* %anf_45bind40238, %struct.ScmObj** %stackaddr$prim48516, align 8
%truthy$cmp48517 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40238)
%cmp$cmp48517 = icmp eq i64 %truthy$cmp48517, 1
br i1 %cmp$cmp48517, label %truebranch$cmp48517, label %falsebranch$cmp48517
truebranch$cmp48517:
%ae40743 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40744 = call %struct.ScmObj* @const_init_null()
%args46877$k40506$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48518 = alloca %struct.ScmObj*, align 8
%args46877$k40506$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40744, %struct.ScmObj* %args46877$k40506$0)
store volatile %struct.ScmObj* %args46877$k40506$1, %struct.ScmObj** %stackaddr$prim48518, align 8
%stackaddr$prim48519 = alloca %struct.ScmObj*, align 8
%args46877$k40506$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40743, %struct.ScmObj* %args46877$k40506$1)
store volatile %struct.ScmObj* %args46877$k40506$2, %struct.ScmObj** %stackaddr$prim48519, align 8
%clofunc48520 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40506)
musttail call tailcc void %clofunc48520(%struct.ScmObj* %k40506, %struct.ScmObj* %args46877$k40506$2)
ret void
falsebranch$cmp48517:
%stackaddr$prim48521 = alloca %struct.ScmObj*, align 8
%anf_45bind40239 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40122)
store volatile %struct.ScmObj* %anf_45bind40239, %struct.ScmObj** %stackaddr$prim48521, align 8
%stackaddr$makeclosure48522 = alloca %struct.ScmObj*, align 8
%fptrToInt48523 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40753 to i64
%ae40753 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48523)
store volatile %struct.ScmObj* %ae40753, %struct.ScmObj** %stackaddr$makeclosure48522, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40753, %struct.ScmObj* %k40506, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40753, %struct.ScmObj* %lst40122, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40753, %struct.ScmObj* %_37map40121, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae40753, %struct.ScmObj* %f40123, i64 3)
%args46886$f40123$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48524 = alloca %struct.ScmObj*, align 8
%args46886$f40123$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40239, %struct.ScmObj* %args46886$f40123$0)
store volatile %struct.ScmObj* %args46886$f40123$1, %struct.ScmObj** %stackaddr$prim48524, align 8
%stackaddr$prim48525 = alloca %struct.ScmObj*, align 8
%args46886$f40123$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40753, %struct.ScmObj* %args46886$f40123$1)
store volatile %struct.ScmObj* %args46886$f40123$2, %struct.ScmObj** %stackaddr$prim48525, align 8
%clofunc48526 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40123)
musttail call tailcc void %clofunc48526(%struct.ScmObj* %f40123, %struct.ScmObj* %args46886$f40123$2)
ret void
}

define tailcc void @proc_clo$ae40753(%struct.ScmObj* %env$ae40753,%struct.ScmObj* %current_45args46878) {
%stackaddr$env-ref48527 = alloca %struct.ScmObj*, align 8
%k40506 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40753, i64 0)
store %struct.ScmObj* %k40506, %struct.ScmObj** %stackaddr$env-ref48527
%stackaddr$env-ref48528 = alloca %struct.ScmObj*, align 8
%lst40122 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40753, i64 1)
store %struct.ScmObj* %lst40122, %struct.ScmObj** %stackaddr$env-ref48528
%stackaddr$env-ref48529 = alloca %struct.ScmObj*, align 8
%_37map40121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40753, i64 2)
store %struct.ScmObj* %_37map40121, %struct.ScmObj** %stackaddr$env-ref48529
%stackaddr$env-ref48530 = alloca %struct.ScmObj*, align 8
%f40123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40753, i64 3)
store %struct.ScmObj* %f40123, %struct.ScmObj** %stackaddr$env-ref48530
%stackaddr$prim48531 = alloca %struct.ScmObj*, align 8
%_95k40507 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46878)
store volatile %struct.ScmObj* %_95k40507, %struct.ScmObj** %stackaddr$prim48531, align 8
%stackaddr$prim48532 = alloca %struct.ScmObj*, align 8
%current_45args46879 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46878)
store volatile %struct.ScmObj* %current_45args46879, %struct.ScmObj** %stackaddr$prim48532, align 8
%stackaddr$prim48533 = alloca %struct.ScmObj*, align 8
%anf_45bind40240 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46879)
store volatile %struct.ScmObj* %anf_45bind40240, %struct.ScmObj** %stackaddr$prim48533, align 8
%stackaddr$prim48534 = alloca %struct.ScmObj*, align 8
%anf_45bind40241 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40122)
store volatile %struct.ScmObj* %anf_45bind40241, %struct.ScmObj** %stackaddr$prim48534, align 8
%stackaddr$makeclosure48535 = alloca %struct.ScmObj*, align 8
%fptrToInt48536 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40757 to i64
%ae40757 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48536)
store volatile %struct.ScmObj* %ae40757, %struct.ScmObj** %stackaddr$makeclosure48535, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40757, %struct.ScmObj* %anf_45bind40240, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40757, %struct.ScmObj* %k40506, i64 1)
%args46885$_37map40121$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48537 = alloca %struct.ScmObj*, align 8
%args46885$_37map40121$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40241, %struct.ScmObj* %args46885$_37map40121$0)
store volatile %struct.ScmObj* %args46885$_37map40121$1, %struct.ScmObj** %stackaddr$prim48537, align 8
%stackaddr$prim48538 = alloca %struct.ScmObj*, align 8
%args46885$_37map40121$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40123, %struct.ScmObj* %args46885$_37map40121$1)
store volatile %struct.ScmObj* %args46885$_37map40121$2, %struct.ScmObj** %stackaddr$prim48538, align 8
%stackaddr$prim48539 = alloca %struct.ScmObj*, align 8
%args46885$_37map40121$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40757, %struct.ScmObj* %args46885$_37map40121$2)
store volatile %struct.ScmObj* %args46885$_37map40121$3, %struct.ScmObj** %stackaddr$prim48539, align 8
%clofunc48540 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map40121)
musttail call tailcc void %clofunc48540(%struct.ScmObj* %_37map40121, %struct.ScmObj* %args46885$_37map40121$3)
ret void
}

define tailcc void @proc_clo$ae40757(%struct.ScmObj* %env$ae40757,%struct.ScmObj* %current_45args46881) {
%stackaddr$env-ref48541 = alloca %struct.ScmObj*, align 8
%anf_45bind40240 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40757, i64 0)
store %struct.ScmObj* %anf_45bind40240, %struct.ScmObj** %stackaddr$env-ref48541
%stackaddr$env-ref48542 = alloca %struct.ScmObj*, align 8
%k40506 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40757, i64 1)
store %struct.ScmObj* %k40506, %struct.ScmObj** %stackaddr$env-ref48542
%stackaddr$prim48543 = alloca %struct.ScmObj*, align 8
%_95k40508 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46881)
store volatile %struct.ScmObj* %_95k40508, %struct.ScmObj** %stackaddr$prim48543, align 8
%stackaddr$prim48544 = alloca %struct.ScmObj*, align 8
%current_45args46882 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46881)
store volatile %struct.ScmObj* %current_45args46882, %struct.ScmObj** %stackaddr$prim48544, align 8
%stackaddr$prim48545 = alloca %struct.ScmObj*, align 8
%anf_45bind40242 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46882)
store volatile %struct.ScmObj* %anf_45bind40242, %struct.ScmObj** %stackaddr$prim48545, align 8
%stackaddr$prim48546 = alloca %struct.ScmObj*, align 8
%cpsprim40509 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40240, %struct.ScmObj* %anf_45bind40242)
store volatile %struct.ScmObj* %cpsprim40509, %struct.ScmObj** %stackaddr$prim48546, align 8
%ae40763 = call %struct.ScmObj* @const_init_int(i64 0)
%args46884$k40506$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48547 = alloca %struct.ScmObj*, align 8
%args46884$k40506$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40509, %struct.ScmObj* %args46884$k40506$0)
store volatile %struct.ScmObj* %args46884$k40506$1, %struct.ScmObj** %stackaddr$prim48547, align 8
%stackaddr$prim48548 = alloca %struct.ScmObj*, align 8
%args46884$k40506$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40763, %struct.ScmObj* %args46884$k40506$1)
store volatile %struct.ScmObj* %args46884$k40506$2, %struct.ScmObj** %stackaddr$prim48548, align 8
%clofunc48549 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40506)
musttail call tailcc void %clofunc48549(%struct.ScmObj* %k40506, %struct.ScmObj* %args46884$k40506$2)
ret void
}

define tailcc void @proc_clo$ae40656(%struct.ScmObj* %env$ae40656,%struct.ScmObj* %current_45args46890) {
%stackaddr$prim48550 = alloca %struct.ScmObj*, align 8
%k40510 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46890)
store volatile %struct.ScmObj* %k40510, %struct.ScmObj** %stackaddr$prim48550, align 8
%stackaddr$prim48551 = alloca %struct.ScmObj*, align 8
%current_45args46891 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46890)
store volatile %struct.ScmObj* %current_45args46891, %struct.ScmObj** %stackaddr$prim48551, align 8
%stackaddr$prim48552 = alloca %struct.ScmObj*, align 8
%_37foldr140125 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46891)
store volatile %struct.ScmObj* %_37foldr140125, %struct.ScmObj** %stackaddr$prim48552, align 8
%ae40658 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48553 = alloca %struct.ScmObj*, align 8
%fptrToInt48554 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40659 to i64
%ae40659 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48554)
store volatile %struct.ScmObj* %ae40659, %struct.ScmObj** %stackaddr$makeclosure48553, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40659, %struct.ScmObj* %_37foldr140125, i64 0)
%args46904$k40510$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48555 = alloca %struct.ScmObj*, align 8
%args46904$k40510$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40659, %struct.ScmObj* %args46904$k40510$0)
store volatile %struct.ScmObj* %args46904$k40510$1, %struct.ScmObj** %stackaddr$prim48555, align 8
%stackaddr$prim48556 = alloca %struct.ScmObj*, align 8
%args46904$k40510$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40658, %struct.ScmObj* %args46904$k40510$1)
store volatile %struct.ScmObj* %args46904$k40510$2, %struct.ScmObj** %stackaddr$prim48556, align 8
%clofunc48557 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40510)
musttail call tailcc void %clofunc48557(%struct.ScmObj* %k40510, %struct.ScmObj* %args46904$k40510$2)
ret void
}

define tailcc void @proc_clo$ae40659(%struct.ScmObj* %env$ae40659,%struct.ScmObj* %current_45args46893) {
%stackaddr$env-ref48558 = alloca %struct.ScmObj*, align 8
%_37foldr140125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40659, i64 0)
store %struct.ScmObj* %_37foldr140125, %struct.ScmObj** %stackaddr$env-ref48558
%stackaddr$prim48559 = alloca %struct.ScmObj*, align 8
%k40511 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46893)
store volatile %struct.ScmObj* %k40511, %struct.ScmObj** %stackaddr$prim48559, align 8
%stackaddr$prim48560 = alloca %struct.ScmObj*, align 8
%current_45args46894 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46893)
store volatile %struct.ScmObj* %current_45args46894, %struct.ScmObj** %stackaddr$prim48560, align 8
%stackaddr$prim48561 = alloca %struct.ScmObj*, align 8
%f40128 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46894)
store volatile %struct.ScmObj* %f40128, %struct.ScmObj** %stackaddr$prim48561, align 8
%stackaddr$prim48562 = alloca %struct.ScmObj*, align 8
%current_45args46895 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46894)
store volatile %struct.ScmObj* %current_45args46895, %struct.ScmObj** %stackaddr$prim48562, align 8
%stackaddr$prim48563 = alloca %struct.ScmObj*, align 8
%acc40127 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46895)
store volatile %struct.ScmObj* %acc40127, %struct.ScmObj** %stackaddr$prim48563, align 8
%stackaddr$prim48564 = alloca %struct.ScmObj*, align 8
%current_45args46896 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46895)
store volatile %struct.ScmObj* %current_45args46896, %struct.ScmObj** %stackaddr$prim48564, align 8
%stackaddr$prim48565 = alloca %struct.ScmObj*, align 8
%lst40126 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46896)
store volatile %struct.ScmObj* %lst40126, %struct.ScmObj** %stackaddr$prim48565, align 8
%stackaddr$prim48566 = alloca %struct.ScmObj*, align 8
%anf_45bind40233 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40126)
store volatile %struct.ScmObj* %anf_45bind40233, %struct.ScmObj** %stackaddr$prim48566, align 8
%truthy$cmp48567 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40233)
%cmp$cmp48567 = icmp eq i64 %truthy$cmp48567, 1
br i1 %cmp$cmp48567, label %truebranch$cmp48567, label %falsebranch$cmp48567
truebranch$cmp48567:
%ae40663 = call %struct.ScmObj* @const_init_int(i64 0)
%args46898$k40511$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48568 = alloca %struct.ScmObj*, align 8
%args46898$k40511$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40127, %struct.ScmObj* %args46898$k40511$0)
store volatile %struct.ScmObj* %args46898$k40511$1, %struct.ScmObj** %stackaddr$prim48568, align 8
%stackaddr$prim48569 = alloca %struct.ScmObj*, align 8
%args46898$k40511$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40663, %struct.ScmObj* %args46898$k40511$1)
store volatile %struct.ScmObj* %args46898$k40511$2, %struct.ScmObj** %stackaddr$prim48569, align 8
%clofunc48570 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40511)
musttail call tailcc void %clofunc48570(%struct.ScmObj* %k40511, %struct.ScmObj* %args46898$k40511$2)
ret void
falsebranch$cmp48567:
%stackaddr$prim48571 = alloca %struct.ScmObj*, align 8
%anf_45bind40234 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40126)
store volatile %struct.ScmObj* %anf_45bind40234, %struct.ScmObj** %stackaddr$prim48571, align 8
%stackaddr$prim48572 = alloca %struct.ScmObj*, align 8
%anf_45bind40235 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40126)
store volatile %struct.ScmObj* %anf_45bind40235, %struct.ScmObj** %stackaddr$prim48572, align 8
%stackaddr$makeclosure48573 = alloca %struct.ScmObj*, align 8
%fptrToInt48574 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40671 to i64
%ae40671 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48574)
store volatile %struct.ScmObj* %ae40671, %struct.ScmObj** %stackaddr$makeclosure48573, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40671, %struct.ScmObj* %f40128, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40671, %struct.ScmObj* %k40511, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40671, %struct.ScmObj* %anf_45bind40234, i64 2)
%args46903$_37foldr140125$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48575 = alloca %struct.ScmObj*, align 8
%args46903$_37foldr140125$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40235, %struct.ScmObj* %args46903$_37foldr140125$0)
store volatile %struct.ScmObj* %args46903$_37foldr140125$1, %struct.ScmObj** %stackaddr$prim48575, align 8
%stackaddr$prim48576 = alloca %struct.ScmObj*, align 8
%args46903$_37foldr140125$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40127, %struct.ScmObj* %args46903$_37foldr140125$1)
store volatile %struct.ScmObj* %args46903$_37foldr140125$2, %struct.ScmObj** %stackaddr$prim48576, align 8
%stackaddr$prim48577 = alloca %struct.ScmObj*, align 8
%args46903$_37foldr140125$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40128, %struct.ScmObj* %args46903$_37foldr140125$2)
store volatile %struct.ScmObj* %args46903$_37foldr140125$3, %struct.ScmObj** %stackaddr$prim48577, align 8
%stackaddr$prim48578 = alloca %struct.ScmObj*, align 8
%args46903$_37foldr140125$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40671, %struct.ScmObj* %args46903$_37foldr140125$3)
store volatile %struct.ScmObj* %args46903$_37foldr140125$4, %struct.ScmObj** %stackaddr$prim48578, align 8
%clofunc48579 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140125)
musttail call tailcc void %clofunc48579(%struct.ScmObj* %_37foldr140125, %struct.ScmObj* %args46903$_37foldr140125$4)
ret void
}

define tailcc void @proc_clo$ae40671(%struct.ScmObj* %env$ae40671,%struct.ScmObj* %current_45args46899) {
%stackaddr$env-ref48580 = alloca %struct.ScmObj*, align 8
%f40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40671, i64 0)
store %struct.ScmObj* %f40128, %struct.ScmObj** %stackaddr$env-ref48580
%stackaddr$env-ref48581 = alloca %struct.ScmObj*, align 8
%k40511 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40671, i64 1)
store %struct.ScmObj* %k40511, %struct.ScmObj** %stackaddr$env-ref48581
%stackaddr$env-ref48582 = alloca %struct.ScmObj*, align 8
%anf_45bind40234 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40671, i64 2)
store %struct.ScmObj* %anf_45bind40234, %struct.ScmObj** %stackaddr$env-ref48582
%stackaddr$prim48583 = alloca %struct.ScmObj*, align 8
%_95k40512 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46899)
store volatile %struct.ScmObj* %_95k40512, %struct.ScmObj** %stackaddr$prim48583, align 8
%stackaddr$prim48584 = alloca %struct.ScmObj*, align 8
%current_45args46900 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46899)
store volatile %struct.ScmObj* %current_45args46900, %struct.ScmObj** %stackaddr$prim48584, align 8
%stackaddr$prim48585 = alloca %struct.ScmObj*, align 8
%anf_45bind40236 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46900)
store volatile %struct.ScmObj* %anf_45bind40236, %struct.ScmObj** %stackaddr$prim48585, align 8
%args46902$f40128$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48586 = alloca %struct.ScmObj*, align 8
%args46902$f40128$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40236, %struct.ScmObj* %args46902$f40128$0)
store volatile %struct.ScmObj* %args46902$f40128$1, %struct.ScmObj** %stackaddr$prim48586, align 8
%stackaddr$prim48587 = alloca %struct.ScmObj*, align 8
%args46902$f40128$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40234, %struct.ScmObj* %args46902$f40128$1)
store volatile %struct.ScmObj* %args46902$f40128$2, %struct.ScmObj** %stackaddr$prim48587, align 8
%stackaddr$prim48588 = alloca %struct.ScmObj*, align 8
%args46902$f40128$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40511, %struct.ScmObj* %args46902$f40128$2)
store volatile %struct.ScmObj* %args46902$f40128$3, %struct.ScmObj** %stackaddr$prim48588, align 8
%clofunc48589 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40128)
musttail call tailcc void %clofunc48589(%struct.ScmObj* %f40128, %struct.ScmObj* %args46902$f40128$3)
ret void
}

define tailcc void @proc_clo$ae40539(%struct.ScmObj* %env$ae40539,%struct.ScmObj* %current_45args46907) {
%stackaddr$prim48590 = alloca %struct.ScmObj*, align 8
%k40513 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46907)
store volatile %struct.ScmObj* %k40513, %struct.ScmObj** %stackaddr$prim48590, align 8
%stackaddr$prim48591 = alloca %struct.ScmObj*, align 8
%current_45args46908 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46907)
store volatile %struct.ScmObj* %current_45args46908, %struct.ScmObj** %stackaddr$prim48591, align 8
%stackaddr$prim48592 = alloca %struct.ScmObj*, align 8
%y40105 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46908)
store volatile %struct.ScmObj* %y40105, %struct.ScmObj** %stackaddr$prim48592, align 8
%ae40541 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48593 = alloca %struct.ScmObj*, align 8
%fptrToInt48594 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40542 to i64
%ae40542 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48594)
store volatile %struct.ScmObj* %ae40542, %struct.ScmObj** %stackaddr$makeclosure48593, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40542, %struct.ScmObj* %y40105, i64 0)
%args46926$k40513$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48595 = alloca %struct.ScmObj*, align 8
%args46926$k40513$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40542, %struct.ScmObj* %args46926$k40513$0)
store volatile %struct.ScmObj* %args46926$k40513$1, %struct.ScmObj** %stackaddr$prim48595, align 8
%stackaddr$prim48596 = alloca %struct.ScmObj*, align 8
%args46926$k40513$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40541, %struct.ScmObj* %args46926$k40513$1)
store volatile %struct.ScmObj* %args46926$k40513$2, %struct.ScmObj** %stackaddr$prim48596, align 8
%clofunc48597 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40513)
musttail call tailcc void %clofunc48597(%struct.ScmObj* %k40513, %struct.ScmObj* %args46926$k40513$2)
ret void
}

define tailcc void @proc_clo$ae40542(%struct.ScmObj* %env$ae40542,%struct.ScmObj* %current_45args46910) {
%stackaddr$env-ref48598 = alloca %struct.ScmObj*, align 8
%y40105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40542, i64 0)
store %struct.ScmObj* %y40105, %struct.ScmObj** %stackaddr$env-ref48598
%stackaddr$prim48599 = alloca %struct.ScmObj*, align 8
%k40514 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46910)
store volatile %struct.ScmObj* %k40514, %struct.ScmObj** %stackaddr$prim48599, align 8
%stackaddr$prim48600 = alloca %struct.ScmObj*, align 8
%current_45args46911 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46910)
store volatile %struct.ScmObj* %current_45args46911, %struct.ScmObj** %stackaddr$prim48600, align 8
%stackaddr$prim48601 = alloca %struct.ScmObj*, align 8
%f40106 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46911)
store volatile %struct.ScmObj* %f40106, %struct.ScmObj** %stackaddr$prim48601, align 8
%stackaddr$makeclosure48602 = alloca %struct.ScmObj*, align 8
%fptrToInt48603 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40543 to i64
%ae40543 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48603)
store volatile %struct.ScmObj* %ae40543, %struct.ScmObj** %stackaddr$makeclosure48602, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40543, %struct.ScmObj* %k40514, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40543, %struct.ScmObj* %f40106, i64 1)
%ae40544 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48604 = alloca %struct.ScmObj*, align 8
%fptrToInt48605 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40545 to i64
%ae40545 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48605)
store volatile %struct.ScmObj* %ae40545, %struct.ScmObj** %stackaddr$makeclosure48604, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40545, %struct.ScmObj* %y40105, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40545, %struct.ScmObj* %f40106, i64 1)
%args46925$ae40543$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48606 = alloca %struct.ScmObj*, align 8
%args46925$ae40543$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40545, %struct.ScmObj* %args46925$ae40543$0)
store volatile %struct.ScmObj* %args46925$ae40543$1, %struct.ScmObj** %stackaddr$prim48606, align 8
%stackaddr$prim48607 = alloca %struct.ScmObj*, align 8
%args46925$ae40543$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40544, %struct.ScmObj* %args46925$ae40543$1)
store volatile %struct.ScmObj* %args46925$ae40543$2, %struct.ScmObj** %stackaddr$prim48607, align 8
%clofunc48608 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40543)
musttail call tailcc void %clofunc48608(%struct.ScmObj* %ae40543, %struct.ScmObj* %args46925$ae40543$2)
ret void
}

define tailcc void @proc_clo$ae40543(%struct.ScmObj* %env$ae40543,%struct.ScmObj* %current_45args46913) {
%stackaddr$env-ref48609 = alloca %struct.ScmObj*, align 8
%k40514 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40543, i64 0)
store %struct.ScmObj* %k40514, %struct.ScmObj** %stackaddr$env-ref48609
%stackaddr$env-ref48610 = alloca %struct.ScmObj*, align 8
%f40106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40543, i64 1)
store %struct.ScmObj* %f40106, %struct.ScmObj** %stackaddr$env-ref48610
%stackaddr$prim48611 = alloca %struct.ScmObj*, align 8
%_95k40515 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46913)
store volatile %struct.ScmObj* %_95k40515, %struct.ScmObj** %stackaddr$prim48611, align 8
%stackaddr$prim48612 = alloca %struct.ScmObj*, align 8
%current_45args46914 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46913)
store volatile %struct.ScmObj* %current_45args46914, %struct.ScmObj** %stackaddr$prim48612, align 8
%stackaddr$prim48613 = alloca %struct.ScmObj*, align 8
%anf_45bind40231 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46914)
store volatile %struct.ScmObj* %anf_45bind40231, %struct.ScmObj** %stackaddr$prim48613, align 8
%args46916$f40106$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48614 = alloca %struct.ScmObj*, align 8
%args46916$f40106$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40231, %struct.ScmObj* %args46916$f40106$0)
store volatile %struct.ScmObj* %args46916$f40106$1, %struct.ScmObj** %stackaddr$prim48614, align 8
%stackaddr$prim48615 = alloca %struct.ScmObj*, align 8
%args46916$f40106$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40514, %struct.ScmObj* %args46916$f40106$1)
store volatile %struct.ScmObj* %args46916$f40106$2, %struct.ScmObj** %stackaddr$prim48615, align 8
%clofunc48616 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40106)
musttail call tailcc void %clofunc48616(%struct.ScmObj* %f40106, %struct.ScmObj* %args46916$f40106$2)
ret void
}

define tailcc void @proc_clo$ae40545(%struct.ScmObj* %env$ae40545,%struct.ScmObj* %args4010740516) {
%stackaddr$env-ref48617 = alloca %struct.ScmObj*, align 8
%y40105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40545, i64 0)
store %struct.ScmObj* %y40105, %struct.ScmObj** %stackaddr$env-ref48617
%stackaddr$env-ref48618 = alloca %struct.ScmObj*, align 8
%f40106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40545, i64 1)
store %struct.ScmObj* %f40106, %struct.ScmObj** %stackaddr$env-ref48618
%stackaddr$prim48619 = alloca %struct.ScmObj*, align 8
%k40517 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4010740516)
store volatile %struct.ScmObj* %k40517, %struct.ScmObj** %stackaddr$prim48619, align 8
%stackaddr$prim48620 = alloca %struct.ScmObj*, align 8
%args40107 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4010740516)
store volatile %struct.ScmObj* %args40107, %struct.ScmObj** %stackaddr$prim48620, align 8
%stackaddr$makeclosure48621 = alloca %struct.ScmObj*, align 8
%fptrToInt48622 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40549 to i64
%ae40549 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48622)
store volatile %struct.ScmObj* %ae40549, %struct.ScmObj** %stackaddr$makeclosure48621, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40549, %struct.ScmObj* %k40517, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40549, %struct.ScmObj* %args40107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40549, %struct.ScmObj* %f40106, i64 2)
%args46924$y40105$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48623 = alloca %struct.ScmObj*, align 8
%args46924$y40105$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y40105, %struct.ScmObj* %args46924$y40105$0)
store volatile %struct.ScmObj* %args46924$y40105$1, %struct.ScmObj** %stackaddr$prim48623, align 8
%stackaddr$prim48624 = alloca %struct.ScmObj*, align 8
%args46924$y40105$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40549, %struct.ScmObj* %args46924$y40105$1)
store volatile %struct.ScmObj* %args46924$y40105$2, %struct.ScmObj** %stackaddr$prim48624, align 8
%clofunc48625 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y40105)
musttail call tailcc void %clofunc48625(%struct.ScmObj* %y40105, %struct.ScmObj* %args46924$y40105$2)
ret void
}

define tailcc void @proc_clo$ae40549(%struct.ScmObj* %env$ae40549,%struct.ScmObj* %current_45args46917) {
%stackaddr$env-ref48626 = alloca %struct.ScmObj*, align 8
%k40517 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40549, i64 0)
store %struct.ScmObj* %k40517, %struct.ScmObj** %stackaddr$env-ref48626
%stackaddr$env-ref48627 = alloca %struct.ScmObj*, align 8
%args40107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40549, i64 1)
store %struct.ScmObj* %args40107, %struct.ScmObj** %stackaddr$env-ref48627
%stackaddr$env-ref48628 = alloca %struct.ScmObj*, align 8
%f40106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40549, i64 2)
store %struct.ScmObj* %f40106, %struct.ScmObj** %stackaddr$env-ref48628
%stackaddr$prim48629 = alloca %struct.ScmObj*, align 8
%_95k40518 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46917)
store volatile %struct.ScmObj* %_95k40518, %struct.ScmObj** %stackaddr$prim48629, align 8
%stackaddr$prim48630 = alloca %struct.ScmObj*, align 8
%current_45args46918 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46917)
store volatile %struct.ScmObj* %current_45args46918, %struct.ScmObj** %stackaddr$prim48630, align 8
%stackaddr$prim48631 = alloca %struct.ScmObj*, align 8
%anf_45bind40229 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46918)
store volatile %struct.ScmObj* %anf_45bind40229, %struct.ScmObj** %stackaddr$prim48631, align 8
%stackaddr$makeclosure48632 = alloca %struct.ScmObj*, align 8
%fptrToInt48633 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40552 to i64
%ae40552 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48633)
store volatile %struct.ScmObj* %ae40552, %struct.ScmObj** %stackaddr$makeclosure48632, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40552, %struct.ScmObj* %k40517, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40552, %struct.ScmObj* %args40107, i64 1)
%args46923$anf_45bind40229$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48634 = alloca %struct.ScmObj*, align 8
%args46923$anf_45bind40229$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40106, %struct.ScmObj* %args46923$anf_45bind40229$0)
store volatile %struct.ScmObj* %args46923$anf_45bind40229$1, %struct.ScmObj** %stackaddr$prim48634, align 8
%stackaddr$prim48635 = alloca %struct.ScmObj*, align 8
%args46923$anf_45bind40229$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40552, %struct.ScmObj* %args46923$anf_45bind40229$1)
store volatile %struct.ScmObj* %args46923$anf_45bind40229$2, %struct.ScmObj** %stackaddr$prim48635, align 8
%clofunc48636 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40229)
musttail call tailcc void %clofunc48636(%struct.ScmObj* %anf_45bind40229, %struct.ScmObj* %args46923$anf_45bind40229$2)
ret void
}

define tailcc void @proc_clo$ae40552(%struct.ScmObj* %env$ae40552,%struct.ScmObj* %current_45args46920) {
%stackaddr$env-ref48637 = alloca %struct.ScmObj*, align 8
%k40517 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40552, i64 0)
store %struct.ScmObj* %k40517, %struct.ScmObj** %stackaddr$env-ref48637
%stackaddr$env-ref48638 = alloca %struct.ScmObj*, align 8
%args40107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40552, i64 1)
store %struct.ScmObj* %args40107, %struct.ScmObj** %stackaddr$env-ref48638
%stackaddr$prim48639 = alloca %struct.ScmObj*, align 8
%_95k40519 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46920)
store volatile %struct.ScmObj* %_95k40519, %struct.ScmObj** %stackaddr$prim48639, align 8
%stackaddr$prim48640 = alloca %struct.ScmObj*, align 8
%current_45args46921 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46920)
store volatile %struct.ScmObj* %current_45args46921, %struct.ScmObj** %stackaddr$prim48640, align 8
%stackaddr$prim48641 = alloca %struct.ScmObj*, align 8
%anf_45bind40230 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46921)
store volatile %struct.ScmObj* %anf_45bind40230, %struct.ScmObj** %stackaddr$prim48641, align 8
%stackaddr$prim48642 = alloca %struct.ScmObj*, align 8
%cpsargs40520 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40517, %struct.ScmObj* %args40107)
store volatile %struct.ScmObj* %cpsargs40520, %struct.ScmObj** %stackaddr$prim48642, align 8
%clofunc48643 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40230)
musttail call tailcc void %clofunc48643(%struct.ScmObj* %anf_45bind40230, %struct.ScmObj* %cpsargs40520)
ret void
}

define tailcc void @proc_clo$ae40524(%struct.ScmObj* %env$ae40524,%struct.ScmObj* %current_45args46928) {
%stackaddr$prim48644 = alloca %struct.ScmObj*, align 8
%k40521 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46928)
store volatile %struct.ScmObj* %k40521, %struct.ScmObj** %stackaddr$prim48644, align 8
%stackaddr$prim48645 = alloca %struct.ScmObj*, align 8
%current_45args46929 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46928)
store volatile %struct.ScmObj* %current_45args46929, %struct.ScmObj** %stackaddr$prim48645, align 8
%stackaddr$prim48646 = alloca %struct.ScmObj*, align 8
%yu40104 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46929)
store volatile %struct.ScmObj* %yu40104, %struct.ScmObj** %stackaddr$prim48646, align 8
%args46931$yu40104$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48647 = alloca %struct.ScmObj*, align 8
%args46931$yu40104$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu40104, %struct.ScmObj* %args46931$yu40104$0)
store volatile %struct.ScmObj* %args46931$yu40104$1, %struct.ScmObj** %stackaddr$prim48647, align 8
%stackaddr$prim48648 = alloca %struct.ScmObj*, align 8
%args46931$yu40104$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40521, %struct.ScmObj* %args46931$yu40104$1)
store volatile %struct.ScmObj* %args46931$yu40104$2, %struct.ScmObj** %stackaddr$prim48648, align 8
%clofunc48649 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu40104)
musttail call tailcc void %clofunc48649(%struct.ScmObj* %yu40104, %struct.ScmObj* %args46931$yu40104$2)
ret void
}