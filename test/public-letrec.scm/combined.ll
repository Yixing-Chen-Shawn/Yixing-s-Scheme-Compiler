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
%mainenv55618 = call %struct.ScmObj* @const_init_null()
%mainargs55619 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv55618, %struct.ScmObj* %mainargs55619)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv55616,%struct.ScmObj* %mainargs55617) {
%stackaddr$makeclosure55620 = alloca %struct.ScmObj*, align 8
%fptrToInt55621 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47508 to i64
%ae47508 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55621)
store volatile %struct.ScmObj* %ae47508, %struct.ScmObj** %stackaddr$makeclosure55620, align 8
%ae47509 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55622 = alloca %struct.ScmObj*, align 8
%fptrToInt55623 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47510 to i64
%ae47510 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55623)
store volatile %struct.ScmObj* %ae47510, %struct.ScmObj** %stackaddr$makeclosure55622, align 8
%argslist55615$ae475080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55624 = alloca %struct.ScmObj*, align 8
%argslist55615$ae475081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47510, %struct.ScmObj* %argslist55615$ae475080)
store volatile %struct.ScmObj* %argslist55615$ae475081, %struct.ScmObj** %stackaddr$prim55624, align 8
%stackaddr$prim55625 = alloca %struct.ScmObj*, align 8
%argslist55615$ae475082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47509, %struct.ScmObj* %argslist55615$ae475081)
store volatile %struct.ScmObj* %argslist55615$ae475082, %struct.ScmObj** %stackaddr$prim55625, align 8
%clofunc55626 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47508)
musttail call tailcc void %clofunc55626(%struct.ScmObj* %ae47508, %struct.ScmObj* %argslist55615$ae475082)
ret void
}

define tailcc void @proc_clo$ae47508(%struct.ScmObj* %env$ae47508,%struct.ScmObj* %current_45args55089) {
%stackaddr$prim55627 = alloca %struct.ScmObj*, align 8
%_95k47347 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55089)
store volatile %struct.ScmObj* %_95k47347, %struct.ScmObj** %stackaddr$prim55627, align 8
%stackaddr$prim55628 = alloca %struct.ScmObj*, align 8
%current_45args55090 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55089)
store volatile %struct.ScmObj* %current_45args55090, %struct.ScmObj** %stackaddr$prim55628, align 8
%stackaddr$prim55629 = alloca %struct.ScmObj*, align 8
%anf_45bind47210 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55090)
store volatile %struct.ScmObj* %anf_45bind47210, %struct.ScmObj** %stackaddr$prim55629, align 8
%stackaddr$makeclosure55630 = alloca %struct.ScmObj*, align 8
%fptrToInt55631 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47523 to i64
%ae47523 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55631)
store volatile %struct.ScmObj* %ae47523, %struct.ScmObj** %stackaddr$makeclosure55630, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47523, %struct.ScmObj* %anf_45bind47210, i64 0)
%ae47524 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55632 = alloca %struct.ScmObj*, align 8
%fptrToInt55633 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47525 to i64
%ae47525 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55633)
store volatile %struct.ScmObj* %ae47525, %struct.ScmObj** %stackaddr$makeclosure55632, align 8
%argslist55610$ae475230 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55634 = alloca %struct.ScmObj*, align 8
%argslist55610$ae475231 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47525, %struct.ScmObj* %argslist55610$ae475230)
store volatile %struct.ScmObj* %argslist55610$ae475231, %struct.ScmObj** %stackaddr$prim55634, align 8
%stackaddr$prim55635 = alloca %struct.ScmObj*, align 8
%argslist55610$ae475232 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47524, %struct.ScmObj* %argslist55610$ae475231)
store volatile %struct.ScmObj* %argslist55610$ae475232, %struct.ScmObj** %stackaddr$prim55635, align 8
%clofunc55636 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47523)
musttail call tailcc void %clofunc55636(%struct.ScmObj* %ae47523, %struct.ScmObj* %argslist55610$ae475232)
ret void
}

define tailcc void @proc_clo$ae47523(%struct.ScmObj* %env$ae47523,%struct.ScmObj* %current_45args55092) {
%stackaddr$env-ref55637 = alloca %struct.ScmObj*, align 8
%anf_45bind47210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47523, i64 0)
store %struct.ScmObj* %anf_45bind47210, %struct.ScmObj** %stackaddr$env-ref55637
%stackaddr$prim55638 = alloca %struct.ScmObj*, align 8
%_95k47348 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55092)
store volatile %struct.ScmObj* %_95k47348, %struct.ScmObj** %stackaddr$prim55638, align 8
%stackaddr$prim55639 = alloca %struct.ScmObj*, align 8
%current_45args55093 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55092)
store volatile %struct.ScmObj* %current_45args55093, %struct.ScmObj** %stackaddr$prim55639, align 8
%stackaddr$prim55640 = alloca %struct.ScmObj*, align 8
%anf_45bind47214 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55093)
store volatile %struct.ScmObj* %anf_45bind47214, %struct.ScmObj** %stackaddr$prim55640, align 8
%stackaddr$makeclosure55641 = alloca %struct.ScmObj*, align 8
%fptrToInt55642 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47638 to i64
%ae47638 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55642)
store volatile %struct.ScmObj* %ae47638, %struct.ScmObj** %stackaddr$makeclosure55641, align 8
%argslist55589$anf_45bind472100 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55643 = alloca %struct.ScmObj*, align 8
%argslist55589$anf_45bind472101 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47214, %struct.ScmObj* %argslist55589$anf_45bind472100)
store volatile %struct.ScmObj* %argslist55589$anf_45bind472101, %struct.ScmObj** %stackaddr$prim55643, align 8
%stackaddr$prim55644 = alloca %struct.ScmObj*, align 8
%argslist55589$anf_45bind472102 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47638, %struct.ScmObj* %argslist55589$anf_45bind472101)
store volatile %struct.ScmObj* %argslist55589$anf_45bind472102, %struct.ScmObj** %stackaddr$prim55644, align 8
%clofunc55645 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47210)
musttail call tailcc void %clofunc55645(%struct.ScmObj* %anf_45bind47210, %struct.ScmObj* %argslist55589$anf_45bind472102)
ret void
}

define tailcc void @proc_clo$ae47638(%struct.ScmObj* %env$ae47638,%struct.ScmObj* %current_45args55095) {
%stackaddr$prim55646 = alloca %struct.ScmObj*, align 8
%_95k47349 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55095)
store volatile %struct.ScmObj* %_95k47349, %struct.ScmObj** %stackaddr$prim55646, align 8
%stackaddr$prim55647 = alloca %struct.ScmObj*, align 8
%current_45args55096 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55095)
store volatile %struct.ScmObj* %current_45args55096, %struct.ScmObj** %stackaddr$prim55647, align 8
%stackaddr$prim55648 = alloca %struct.ScmObj*, align 8
%Ycmb47075 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55096)
store volatile %struct.ScmObj* %Ycmb47075, %struct.ScmObj** %stackaddr$prim55648, align 8
%stackaddr$makeclosure55649 = alloca %struct.ScmObj*, align 8
%fptrToInt55650 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47640 to i64
%ae47640 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55650)
store volatile %struct.ScmObj* %ae47640, %struct.ScmObj** %stackaddr$makeclosure55649, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47640, %struct.ScmObj* %Ycmb47075, i64 0)
%ae47641 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55651 = alloca %struct.ScmObj*, align 8
%fptrToInt55652 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47642 to i64
%ae47642 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55652)
store volatile %struct.ScmObj* %ae47642, %struct.ScmObj** %stackaddr$makeclosure55651, align 8
%argslist55588$ae476400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55653 = alloca %struct.ScmObj*, align 8
%argslist55588$ae476401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47642, %struct.ScmObj* %argslist55588$ae476400)
store volatile %struct.ScmObj* %argslist55588$ae476401, %struct.ScmObj** %stackaddr$prim55653, align 8
%stackaddr$prim55654 = alloca %struct.ScmObj*, align 8
%argslist55588$ae476402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47641, %struct.ScmObj* %argslist55588$ae476401)
store volatile %struct.ScmObj* %argslist55588$ae476402, %struct.ScmObj** %stackaddr$prim55654, align 8
%clofunc55655 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47640)
musttail call tailcc void %clofunc55655(%struct.ScmObj* %ae47640, %struct.ScmObj* %argslist55588$ae476402)
ret void
}

define tailcc void @proc_clo$ae47640(%struct.ScmObj* %env$ae47640,%struct.ScmObj* %current_45args55098) {
%stackaddr$env-ref55656 = alloca %struct.ScmObj*, align 8
%Ycmb47075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47640, i64 0)
store %struct.ScmObj* %Ycmb47075, %struct.ScmObj** %stackaddr$env-ref55656
%stackaddr$prim55657 = alloca %struct.ScmObj*, align 8
%_95k47350 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55098)
store volatile %struct.ScmObj* %_95k47350, %struct.ScmObj** %stackaddr$prim55657, align 8
%stackaddr$prim55658 = alloca %struct.ScmObj*, align 8
%current_45args55099 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55098)
store volatile %struct.ScmObj* %current_45args55099, %struct.ScmObj** %stackaddr$prim55658, align 8
%stackaddr$prim55659 = alloca %struct.ScmObj*, align 8
%anf_45bind47219 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55099)
store volatile %struct.ScmObj* %anf_45bind47219, %struct.ScmObj** %stackaddr$prim55659, align 8
%stackaddr$makeclosure55660 = alloca %struct.ScmObj*, align 8
%fptrToInt55661 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47718 to i64
%ae47718 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55661)
store volatile %struct.ScmObj* %ae47718, %struct.ScmObj** %stackaddr$makeclosure55660, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47718, %struct.ScmObj* %Ycmb47075, i64 0)
%argslist55572$Ycmb470750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55662 = alloca %struct.ScmObj*, align 8
%argslist55572$Ycmb470751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47219, %struct.ScmObj* %argslist55572$Ycmb470750)
store volatile %struct.ScmObj* %argslist55572$Ycmb470751, %struct.ScmObj** %stackaddr$prim55662, align 8
%stackaddr$prim55663 = alloca %struct.ScmObj*, align 8
%argslist55572$Ycmb470752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47718, %struct.ScmObj* %argslist55572$Ycmb470751)
store volatile %struct.ScmObj* %argslist55572$Ycmb470752, %struct.ScmObj** %stackaddr$prim55663, align 8
%clofunc55664 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47075)
musttail call tailcc void %clofunc55664(%struct.ScmObj* %Ycmb47075, %struct.ScmObj* %argslist55572$Ycmb470752)
ret void
}

define tailcc void @proc_clo$ae47718(%struct.ScmObj* %env$ae47718,%struct.ScmObj* %current_45args55101) {
%stackaddr$env-ref55665 = alloca %struct.ScmObj*, align 8
%Ycmb47075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47718, i64 0)
store %struct.ScmObj* %Ycmb47075, %struct.ScmObj** %stackaddr$env-ref55665
%stackaddr$prim55666 = alloca %struct.ScmObj*, align 8
%_95k47351 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55101)
store volatile %struct.ScmObj* %_95k47351, %struct.ScmObj** %stackaddr$prim55666, align 8
%stackaddr$prim55667 = alloca %struct.ScmObj*, align 8
%current_45args55102 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55101)
store volatile %struct.ScmObj* %current_45args55102, %struct.ScmObj** %stackaddr$prim55667, align 8
%stackaddr$prim55668 = alloca %struct.ScmObj*, align 8
%_37foldr147096 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55102)
store volatile %struct.ScmObj* %_37foldr147096, %struct.ScmObj** %stackaddr$prim55668, align 8
%stackaddr$makeclosure55669 = alloca %struct.ScmObj*, align 8
%fptrToInt55670 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47720 to i64
%ae47720 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55670)
store volatile %struct.ScmObj* %ae47720, %struct.ScmObj** %stackaddr$makeclosure55669, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47720, %struct.ScmObj* %Ycmb47075, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47720, %struct.ScmObj* %_37foldr147096, i64 1)
%ae47721 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55671 = alloca %struct.ScmObj*, align 8
%fptrToInt55672 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47722 to i64
%ae47722 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55672)
store volatile %struct.ScmObj* %ae47722, %struct.ScmObj** %stackaddr$makeclosure55671, align 8
%argslist55571$ae477200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55673 = alloca %struct.ScmObj*, align 8
%argslist55571$ae477201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47722, %struct.ScmObj* %argslist55571$ae477200)
store volatile %struct.ScmObj* %argslist55571$ae477201, %struct.ScmObj** %stackaddr$prim55673, align 8
%stackaddr$prim55674 = alloca %struct.ScmObj*, align 8
%argslist55571$ae477202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47721, %struct.ScmObj* %argslist55571$ae477201)
store volatile %struct.ScmObj* %argslist55571$ae477202, %struct.ScmObj** %stackaddr$prim55674, align 8
%clofunc55675 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47720)
musttail call tailcc void %clofunc55675(%struct.ScmObj* %ae47720, %struct.ScmObj* %argslist55571$ae477202)
ret void
}

define tailcc void @proc_clo$ae47720(%struct.ScmObj* %env$ae47720,%struct.ScmObj* %current_45args55104) {
%stackaddr$env-ref55676 = alloca %struct.ScmObj*, align 8
%Ycmb47075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47720, i64 0)
store %struct.ScmObj* %Ycmb47075, %struct.ScmObj** %stackaddr$env-ref55676
%stackaddr$env-ref55677 = alloca %struct.ScmObj*, align 8
%_37foldr147096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47720, i64 1)
store %struct.ScmObj* %_37foldr147096, %struct.ScmObj** %stackaddr$env-ref55677
%stackaddr$prim55678 = alloca %struct.ScmObj*, align 8
%_95k47352 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55104)
store volatile %struct.ScmObj* %_95k47352, %struct.ScmObj** %stackaddr$prim55678, align 8
%stackaddr$prim55679 = alloca %struct.ScmObj*, align 8
%current_45args55105 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55104)
store volatile %struct.ScmObj* %current_45args55105, %struct.ScmObj** %stackaddr$prim55679, align 8
%stackaddr$prim55680 = alloca %struct.ScmObj*, align 8
%anf_45bind47225 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55105)
store volatile %struct.ScmObj* %anf_45bind47225, %struct.ScmObj** %stackaddr$prim55680, align 8
%stackaddr$makeclosure55681 = alloca %struct.ScmObj*, align 8
%fptrToInt55682 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47815 to i64
%ae47815 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55682)
store volatile %struct.ScmObj* %ae47815, %struct.ScmObj** %stackaddr$makeclosure55681, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47815, %struct.ScmObj* %Ycmb47075, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47815, %struct.ScmObj* %_37foldr147096, i64 1)
%argslist55552$Ycmb470750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55683 = alloca %struct.ScmObj*, align 8
%argslist55552$Ycmb470751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47225, %struct.ScmObj* %argslist55552$Ycmb470750)
store volatile %struct.ScmObj* %argslist55552$Ycmb470751, %struct.ScmObj** %stackaddr$prim55683, align 8
%stackaddr$prim55684 = alloca %struct.ScmObj*, align 8
%argslist55552$Ycmb470752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47815, %struct.ScmObj* %argslist55552$Ycmb470751)
store volatile %struct.ScmObj* %argslist55552$Ycmb470752, %struct.ScmObj** %stackaddr$prim55684, align 8
%clofunc55685 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47075)
musttail call tailcc void %clofunc55685(%struct.ScmObj* %Ycmb47075, %struct.ScmObj* %argslist55552$Ycmb470752)
ret void
}

define tailcc void @proc_clo$ae47815(%struct.ScmObj* %env$ae47815,%struct.ScmObj* %current_45args55107) {
%stackaddr$env-ref55686 = alloca %struct.ScmObj*, align 8
%Ycmb47075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47815, i64 0)
store %struct.ScmObj* %Ycmb47075, %struct.ScmObj** %stackaddr$env-ref55686
%stackaddr$env-ref55687 = alloca %struct.ScmObj*, align 8
%_37foldr147096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47815, i64 1)
store %struct.ScmObj* %_37foldr147096, %struct.ScmObj** %stackaddr$env-ref55687
%stackaddr$prim55688 = alloca %struct.ScmObj*, align 8
%_95k47353 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55107)
store volatile %struct.ScmObj* %_95k47353, %struct.ScmObj** %stackaddr$prim55688, align 8
%stackaddr$prim55689 = alloca %struct.ScmObj*, align 8
%current_45args55108 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55107)
store volatile %struct.ScmObj* %current_45args55108, %struct.ScmObj** %stackaddr$prim55689, align 8
%stackaddr$prim55690 = alloca %struct.ScmObj*, align 8
%_37map147092 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55108)
store volatile %struct.ScmObj* %_37map147092, %struct.ScmObj** %stackaddr$prim55690, align 8
%stackaddr$makeclosure55691 = alloca %struct.ScmObj*, align 8
%fptrToInt55692 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47817 to i64
%ae47817 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55692)
store volatile %struct.ScmObj* %ae47817, %struct.ScmObj** %stackaddr$makeclosure55691, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47817, %struct.ScmObj* %_37map147092, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47817, %struct.ScmObj* %Ycmb47075, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47817, %struct.ScmObj* %_37foldr147096, i64 2)
%ae47818 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55693 = alloca %struct.ScmObj*, align 8
%fptrToInt55694 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47819 to i64
%ae47819 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55694)
store volatile %struct.ScmObj* %ae47819, %struct.ScmObj** %stackaddr$makeclosure55693, align 8
%argslist55551$ae478170 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55695 = alloca %struct.ScmObj*, align 8
%argslist55551$ae478171 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47819, %struct.ScmObj* %argslist55551$ae478170)
store volatile %struct.ScmObj* %argslist55551$ae478171, %struct.ScmObj** %stackaddr$prim55695, align 8
%stackaddr$prim55696 = alloca %struct.ScmObj*, align 8
%argslist55551$ae478172 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47818, %struct.ScmObj* %argslist55551$ae478171)
store volatile %struct.ScmObj* %argslist55551$ae478172, %struct.ScmObj** %stackaddr$prim55696, align 8
%clofunc55697 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47817)
musttail call tailcc void %clofunc55697(%struct.ScmObj* %ae47817, %struct.ScmObj* %argslist55551$ae478172)
ret void
}

define tailcc void @proc_clo$ae47817(%struct.ScmObj* %env$ae47817,%struct.ScmObj* %current_45args55110) {
%stackaddr$env-ref55698 = alloca %struct.ScmObj*, align 8
%_37map147092 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47817, i64 0)
store %struct.ScmObj* %_37map147092, %struct.ScmObj** %stackaddr$env-ref55698
%stackaddr$env-ref55699 = alloca %struct.ScmObj*, align 8
%Ycmb47075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47817, i64 1)
store %struct.ScmObj* %Ycmb47075, %struct.ScmObj** %stackaddr$env-ref55699
%stackaddr$env-ref55700 = alloca %struct.ScmObj*, align 8
%_37foldr147096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47817, i64 2)
store %struct.ScmObj* %_37foldr147096, %struct.ScmObj** %stackaddr$env-ref55700
%stackaddr$prim55701 = alloca %struct.ScmObj*, align 8
%_95k47354 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55110)
store volatile %struct.ScmObj* %_95k47354, %struct.ScmObj** %stackaddr$prim55701, align 8
%stackaddr$prim55702 = alloca %struct.ScmObj*, align 8
%current_45args55111 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55110)
store volatile %struct.ScmObj* %current_45args55111, %struct.ScmObj** %stackaddr$prim55702, align 8
%stackaddr$prim55703 = alloca %struct.ScmObj*, align 8
%anf_45bind47232 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55111)
store volatile %struct.ScmObj* %anf_45bind47232, %struct.ScmObj** %stackaddr$prim55703, align 8
%stackaddr$makeclosure55704 = alloca %struct.ScmObj*, align 8
%fptrToInt55705 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47965 to i64
%ae47965 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55705)
store volatile %struct.ScmObj* %ae47965, %struct.ScmObj** %stackaddr$makeclosure55704, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47965, %struct.ScmObj* %_37map147092, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47965, %struct.ScmObj* %Ycmb47075, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47965, %struct.ScmObj* %_37foldr147096, i64 2)
%argslist55535$Ycmb470750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55706 = alloca %struct.ScmObj*, align 8
%argslist55535$Ycmb470751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47232, %struct.ScmObj* %argslist55535$Ycmb470750)
store volatile %struct.ScmObj* %argslist55535$Ycmb470751, %struct.ScmObj** %stackaddr$prim55706, align 8
%stackaddr$prim55707 = alloca %struct.ScmObj*, align 8
%argslist55535$Ycmb470752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47965, %struct.ScmObj* %argslist55535$Ycmb470751)
store volatile %struct.ScmObj* %argslist55535$Ycmb470752, %struct.ScmObj** %stackaddr$prim55707, align 8
%clofunc55708 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47075)
musttail call tailcc void %clofunc55708(%struct.ScmObj* %Ycmb47075, %struct.ScmObj* %argslist55535$Ycmb470752)
ret void
}

define tailcc void @proc_clo$ae47965(%struct.ScmObj* %env$ae47965,%struct.ScmObj* %current_45args55113) {
%stackaddr$env-ref55709 = alloca %struct.ScmObj*, align 8
%_37map147092 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47965, i64 0)
store %struct.ScmObj* %_37map147092, %struct.ScmObj** %stackaddr$env-ref55709
%stackaddr$env-ref55710 = alloca %struct.ScmObj*, align 8
%Ycmb47075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47965, i64 1)
store %struct.ScmObj* %Ycmb47075, %struct.ScmObj** %stackaddr$env-ref55710
%stackaddr$env-ref55711 = alloca %struct.ScmObj*, align 8
%_37foldr147096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47965, i64 2)
store %struct.ScmObj* %_37foldr147096, %struct.ScmObj** %stackaddr$env-ref55711
%stackaddr$prim55712 = alloca %struct.ScmObj*, align 8
%_95k47355 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55113)
store volatile %struct.ScmObj* %_95k47355, %struct.ScmObj** %stackaddr$prim55712, align 8
%stackaddr$prim55713 = alloca %struct.ScmObj*, align 8
%current_45args55114 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55113)
store volatile %struct.ScmObj* %current_45args55114, %struct.ScmObj** %stackaddr$prim55713, align 8
%stackaddr$prim55714 = alloca %struct.ScmObj*, align 8
%_37take47088 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55114)
store volatile %struct.ScmObj* %_37take47088, %struct.ScmObj** %stackaddr$prim55714, align 8
%stackaddr$makeclosure55715 = alloca %struct.ScmObj*, align 8
%fptrToInt55716 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47967 to i64
%ae47967 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55716)
store volatile %struct.ScmObj* %ae47967, %struct.ScmObj** %stackaddr$makeclosure55715, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47967, %struct.ScmObj* %_37map147092, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47967, %struct.ScmObj* %Ycmb47075, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47967, %struct.ScmObj* %_37take47088, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae47967, %struct.ScmObj* %_37foldr147096, i64 3)
%ae47968 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55717 = alloca %struct.ScmObj*, align 8
%fptrToInt55718 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47969 to i64
%ae47969 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55718)
store volatile %struct.ScmObj* %ae47969, %struct.ScmObj** %stackaddr$makeclosure55717, align 8
%argslist55534$ae479670 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55719 = alloca %struct.ScmObj*, align 8
%argslist55534$ae479671 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47969, %struct.ScmObj* %argslist55534$ae479670)
store volatile %struct.ScmObj* %argslist55534$ae479671, %struct.ScmObj** %stackaddr$prim55719, align 8
%stackaddr$prim55720 = alloca %struct.ScmObj*, align 8
%argslist55534$ae479672 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47968, %struct.ScmObj* %argslist55534$ae479671)
store volatile %struct.ScmObj* %argslist55534$ae479672, %struct.ScmObj** %stackaddr$prim55720, align 8
%clofunc55721 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47967)
musttail call tailcc void %clofunc55721(%struct.ScmObj* %ae47967, %struct.ScmObj* %argslist55534$ae479672)
ret void
}

define tailcc void @proc_clo$ae47967(%struct.ScmObj* %env$ae47967,%struct.ScmObj* %current_45args55116) {
%stackaddr$env-ref55722 = alloca %struct.ScmObj*, align 8
%_37map147092 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47967, i64 0)
store %struct.ScmObj* %_37map147092, %struct.ScmObj** %stackaddr$env-ref55722
%stackaddr$env-ref55723 = alloca %struct.ScmObj*, align 8
%Ycmb47075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47967, i64 1)
store %struct.ScmObj* %Ycmb47075, %struct.ScmObj** %stackaddr$env-ref55723
%stackaddr$env-ref55724 = alloca %struct.ScmObj*, align 8
%_37take47088 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47967, i64 2)
store %struct.ScmObj* %_37take47088, %struct.ScmObj** %stackaddr$env-ref55724
%stackaddr$env-ref55725 = alloca %struct.ScmObj*, align 8
%_37foldr147096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47967, i64 3)
store %struct.ScmObj* %_37foldr147096, %struct.ScmObj** %stackaddr$env-ref55725
%stackaddr$prim55726 = alloca %struct.ScmObj*, align 8
%_95k47356 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55116)
store volatile %struct.ScmObj* %_95k47356, %struct.ScmObj** %stackaddr$prim55726, align 8
%stackaddr$prim55727 = alloca %struct.ScmObj*, align 8
%current_45args55117 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55116)
store volatile %struct.ScmObj* %current_45args55117, %struct.ScmObj** %stackaddr$prim55727, align 8
%stackaddr$prim55728 = alloca %struct.ScmObj*, align 8
%anf_45bind47236 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55117)
store volatile %struct.ScmObj* %anf_45bind47236, %struct.ScmObj** %stackaddr$prim55728, align 8
%stackaddr$makeclosure55729 = alloca %struct.ScmObj*, align 8
%fptrToInt55730 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48048 to i64
%ae48048 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55730)
store volatile %struct.ScmObj* %ae48048, %struct.ScmObj** %stackaddr$makeclosure55729, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48048, %struct.ScmObj* %_37map147092, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48048, %struct.ScmObj* %Ycmb47075, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48048, %struct.ScmObj* %_37take47088, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48048, %struct.ScmObj* %_37foldr147096, i64 3)
%argslist55520$Ycmb470750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55731 = alloca %struct.ScmObj*, align 8
%argslist55520$Ycmb470751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47236, %struct.ScmObj* %argslist55520$Ycmb470750)
store volatile %struct.ScmObj* %argslist55520$Ycmb470751, %struct.ScmObj** %stackaddr$prim55731, align 8
%stackaddr$prim55732 = alloca %struct.ScmObj*, align 8
%argslist55520$Ycmb470752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48048, %struct.ScmObj* %argslist55520$Ycmb470751)
store volatile %struct.ScmObj* %argslist55520$Ycmb470752, %struct.ScmObj** %stackaddr$prim55732, align 8
%clofunc55733 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47075)
musttail call tailcc void %clofunc55733(%struct.ScmObj* %Ycmb47075, %struct.ScmObj* %argslist55520$Ycmb470752)
ret void
}

define tailcc void @proc_clo$ae48048(%struct.ScmObj* %env$ae48048,%struct.ScmObj* %current_45args55119) {
%stackaddr$env-ref55734 = alloca %struct.ScmObj*, align 8
%_37map147092 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48048, i64 0)
store %struct.ScmObj* %_37map147092, %struct.ScmObj** %stackaddr$env-ref55734
%stackaddr$env-ref55735 = alloca %struct.ScmObj*, align 8
%Ycmb47075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48048, i64 1)
store %struct.ScmObj* %Ycmb47075, %struct.ScmObj** %stackaddr$env-ref55735
%stackaddr$env-ref55736 = alloca %struct.ScmObj*, align 8
%_37take47088 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48048, i64 2)
store %struct.ScmObj* %_37take47088, %struct.ScmObj** %stackaddr$env-ref55736
%stackaddr$env-ref55737 = alloca %struct.ScmObj*, align 8
%_37foldr147096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48048, i64 3)
store %struct.ScmObj* %_37foldr147096, %struct.ScmObj** %stackaddr$env-ref55737
%stackaddr$prim55738 = alloca %struct.ScmObj*, align 8
%_95k47357 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55119)
store volatile %struct.ScmObj* %_95k47357, %struct.ScmObj** %stackaddr$prim55738, align 8
%stackaddr$prim55739 = alloca %struct.ScmObj*, align 8
%current_45args55120 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55119)
store volatile %struct.ScmObj* %current_45args55120, %struct.ScmObj** %stackaddr$prim55739, align 8
%stackaddr$prim55740 = alloca %struct.ScmObj*, align 8
%_37length47085 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55120)
store volatile %struct.ScmObj* %_37length47085, %struct.ScmObj** %stackaddr$prim55740, align 8
%stackaddr$makeclosure55741 = alloca %struct.ScmObj*, align 8
%fptrToInt55742 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48050 to i64
%ae48050 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55742)
store volatile %struct.ScmObj* %ae48050, %struct.ScmObj** %stackaddr$makeclosure55741, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48050, %struct.ScmObj* %_37map147092, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48050, %struct.ScmObj* %Ycmb47075, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48050, %struct.ScmObj* %_37take47088, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48050, %struct.ScmObj* %_37length47085, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48050, %struct.ScmObj* %_37foldr147096, i64 4)
%ae48051 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55743 = alloca %struct.ScmObj*, align 8
%fptrToInt55744 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48052 to i64
%ae48052 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55744)
store volatile %struct.ScmObj* %ae48052, %struct.ScmObj** %stackaddr$makeclosure55743, align 8
%argslist55519$ae480500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55745 = alloca %struct.ScmObj*, align 8
%argslist55519$ae480501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48052, %struct.ScmObj* %argslist55519$ae480500)
store volatile %struct.ScmObj* %argslist55519$ae480501, %struct.ScmObj** %stackaddr$prim55745, align 8
%stackaddr$prim55746 = alloca %struct.ScmObj*, align 8
%argslist55519$ae480502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48051, %struct.ScmObj* %argslist55519$ae480501)
store volatile %struct.ScmObj* %argslist55519$ae480502, %struct.ScmObj** %stackaddr$prim55746, align 8
%clofunc55747 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48050)
musttail call tailcc void %clofunc55747(%struct.ScmObj* %ae48050, %struct.ScmObj* %argslist55519$ae480502)
ret void
}

define tailcc void @proc_clo$ae48050(%struct.ScmObj* %env$ae48050,%struct.ScmObj* %current_45args55122) {
%stackaddr$env-ref55748 = alloca %struct.ScmObj*, align 8
%_37map147092 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48050, i64 0)
store %struct.ScmObj* %_37map147092, %struct.ScmObj** %stackaddr$env-ref55748
%stackaddr$env-ref55749 = alloca %struct.ScmObj*, align 8
%Ycmb47075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48050, i64 1)
store %struct.ScmObj* %Ycmb47075, %struct.ScmObj** %stackaddr$env-ref55749
%stackaddr$env-ref55750 = alloca %struct.ScmObj*, align 8
%_37take47088 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48050, i64 2)
store %struct.ScmObj* %_37take47088, %struct.ScmObj** %stackaddr$env-ref55750
%stackaddr$env-ref55751 = alloca %struct.ScmObj*, align 8
%_37length47085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48050, i64 3)
store %struct.ScmObj* %_37length47085, %struct.ScmObj** %stackaddr$env-ref55751
%stackaddr$env-ref55752 = alloca %struct.ScmObj*, align 8
%_37foldr147096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48050, i64 4)
store %struct.ScmObj* %_37foldr147096, %struct.ScmObj** %stackaddr$env-ref55752
%stackaddr$prim55753 = alloca %struct.ScmObj*, align 8
%_95k47358 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55122)
store volatile %struct.ScmObj* %_95k47358, %struct.ScmObj** %stackaddr$prim55753, align 8
%stackaddr$prim55754 = alloca %struct.ScmObj*, align 8
%current_45args55123 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55122)
store volatile %struct.ScmObj* %current_45args55123, %struct.ScmObj** %stackaddr$prim55754, align 8
%stackaddr$prim55755 = alloca %struct.ScmObj*, align 8
%anf_45bind47241 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55123)
store volatile %struct.ScmObj* %anf_45bind47241, %struct.ScmObj** %stackaddr$prim55755, align 8
%stackaddr$makeclosure55756 = alloca %struct.ScmObj*, align 8
%fptrToInt55757 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48127 to i64
%ae48127 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55757)
store volatile %struct.ScmObj* %ae48127, %struct.ScmObj** %stackaddr$makeclosure55756, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48127, %struct.ScmObj* %_37map147092, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48127, %struct.ScmObj* %Ycmb47075, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48127, %struct.ScmObj* %_37take47088, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48127, %struct.ScmObj* %_37length47085, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48127, %struct.ScmObj* %_37foldr147096, i64 4)
%argslist55503$Ycmb470750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55758 = alloca %struct.ScmObj*, align 8
%argslist55503$Ycmb470751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47241, %struct.ScmObj* %argslist55503$Ycmb470750)
store volatile %struct.ScmObj* %argslist55503$Ycmb470751, %struct.ScmObj** %stackaddr$prim55758, align 8
%stackaddr$prim55759 = alloca %struct.ScmObj*, align 8
%argslist55503$Ycmb470752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48127, %struct.ScmObj* %argslist55503$Ycmb470751)
store volatile %struct.ScmObj* %argslist55503$Ycmb470752, %struct.ScmObj** %stackaddr$prim55759, align 8
%clofunc55760 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47075)
musttail call tailcc void %clofunc55760(%struct.ScmObj* %Ycmb47075, %struct.ScmObj* %argslist55503$Ycmb470752)
ret void
}

define tailcc void @proc_clo$ae48127(%struct.ScmObj* %env$ae48127,%struct.ScmObj* %current_45args55125) {
%stackaddr$env-ref55761 = alloca %struct.ScmObj*, align 8
%_37map147092 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48127, i64 0)
store %struct.ScmObj* %_37map147092, %struct.ScmObj** %stackaddr$env-ref55761
%stackaddr$env-ref55762 = alloca %struct.ScmObj*, align 8
%Ycmb47075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48127, i64 1)
store %struct.ScmObj* %Ycmb47075, %struct.ScmObj** %stackaddr$env-ref55762
%stackaddr$env-ref55763 = alloca %struct.ScmObj*, align 8
%_37take47088 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48127, i64 2)
store %struct.ScmObj* %_37take47088, %struct.ScmObj** %stackaddr$env-ref55763
%stackaddr$env-ref55764 = alloca %struct.ScmObj*, align 8
%_37length47085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48127, i64 3)
store %struct.ScmObj* %_37length47085, %struct.ScmObj** %stackaddr$env-ref55764
%stackaddr$env-ref55765 = alloca %struct.ScmObj*, align 8
%_37foldr147096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48127, i64 4)
store %struct.ScmObj* %_37foldr147096, %struct.ScmObj** %stackaddr$env-ref55765
%stackaddr$prim55766 = alloca %struct.ScmObj*, align 8
%_95k47359 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55125)
store volatile %struct.ScmObj* %_95k47359, %struct.ScmObj** %stackaddr$prim55766, align 8
%stackaddr$prim55767 = alloca %struct.ScmObj*, align 8
%current_45args55126 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55125)
store volatile %struct.ScmObj* %current_45args55126, %struct.ScmObj** %stackaddr$prim55767, align 8
%stackaddr$prim55768 = alloca %struct.ScmObj*, align 8
%_37foldl147080 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55126)
store volatile %struct.ScmObj* %_37foldl147080, %struct.ScmObj** %stackaddr$prim55768, align 8
%stackaddr$makeclosure55769 = alloca %struct.ScmObj*, align 8
%fptrToInt55770 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48129 to i64
%ae48129 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt55770)
store volatile %struct.ScmObj* %ae48129, %struct.ScmObj** %stackaddr$makeclosure55769, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48129, %struct.ScmObj* %_37foldr147096, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48129, %struct.ScmObj* %_37foldl147080, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48129, %struct.ScmObj* %_37map147092, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48129, %struct.ScmObj* %Ycmb47075, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48129, %struct.ScmObj* %_37take47088, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48129, %struct.ScmObj* %_37length47085, i64 5)
%ae48130 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55771 = alloca %struct.ScmObj*, align 8
%fptrToInt55772 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48131 to i64
%ae48131 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55772)
store volatile %struct.ScmObj* %ae48131, %struct.ScmObj** %stackaddr$makeclosure55771, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48131, %struct.ScmObj* %_37foldl147080, i64 0)
%argslist55502$ae481290 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55773 = alloca %struct.ScmObj*, align 8
%argslist55502$ae481291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48131, %struct.ScmObj* %argslist55502$ae481290)
store volatile %struct.ScmObj* %argslist55502$ae481291, %struct.ScmObj** %stackaddr$prim55773, align 8
%stackaddr$prim55774 = alloca %struct.ScmObj*, align 8
%argslist55502$ae481292 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48130, %struct.ScmObj* %argslist55502$ae481291)
store volatile %struct.ScmObj* %argslist55502$ae481292, %struct.ScmObj** %stackaddr$prim55774, align 8
%clofunc55775 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48129)
musttail call tailcc void %clofunc55775(%struct.ScmObj* %ae48129, %struct.ScmObj* %argslist55502$ae481292)
ret void
}

define tailcc void @proc_clo$ae48129(%struct.ScmObj* %env$ae48129,%struct.ScmObj* %current_45args55128) {
%stackaddr$env-ref55776 = alloca %struct.ScmObj*, align 8
%_37foldr147096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48129, i64 0)
store %struct.ScmObj* %_37foldr147096, %struct.ScmObj** %stackaddr$env-ref55776
%stackaddr$env-ref55777 = alloca %struct.ScmObj*, align 8
%_37foldl147080 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48129, i64 1)
store %struct.ScmObj* %_37foldl147080, %struct.ScmObj** %stackaddr$env-ref55777
%stackaddr$env-ref55778 = alloca %struct.ScmObj*, align 8
%_37map147092 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48129, i64 2)
store %struct.ScmObj* %_37map147092, %struct.ScmObj** %stackaddr$env-ref55778
%stackaddr$env-ref55779 = alloca %struct.ScmObj*, align 8
%Ycmb47075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48129, i64 3)
store %struct.ScmObj* %Ycmb47075, %struct.ScmObj** %stackaddr$env-ref55779
%stackaddr$env-ref55780 = alloca %struct.ScmObj*, align 8
%_37take47088 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48129, i64 4)
store %struct.ScmObj* %_37take47088, %struct.ScmObj** %stackaddr$env-ref55780
%stackaddr$env-ref55781 = alloca %struct.ScmObj*, align 8
%_37length47085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48129, i64 5)
store %struct.ScmObj* %_37length47085, %struct.ScmObj** %stackaddr$env-ref55781
%stackaddr$prim55782 = alloca %struct.ScmObj*, align 8
%_95k47360 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55128)
store volatile %struct.ScmObj* %_95k47360, %struct.ScmObj** %stackaddr$prim55782, align 8
%stackaddr$prim55783 = alloca %struct.ScmObj*, align 8
%current_45args55129 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55128)
store volatile %struct.ScmObj* %current_45args55129, %struct.ScmObj** %stackaddr$prim55783, align 8
%stackaddr$prim55784 = alloca %struct.ScmObj*, align 8
%_37last47118 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55129)
store volatile %struct.ScmObj* %_37last47118, %struct.ScmObj** %stackaddr$prim55784, align 8
%stackaddr$makeclosure55785 = alloca %struct.ScmObj*, align 8
%fptrToInt55786 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48183 to i64
%ae48183 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55786)
store volatile %struct.ScmObj* %ae48183, %struct.ScmObj** %stackaddr$makeclosure55785, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48183, %struct.ScmObj* %_37foldr147096, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48183, %struct.ScmObj* %_37foldl147080, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48183, %struct.ScmObj* %_37map147092, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48183, %struct.ScmObj* %Ycmb47075, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48183, %struct.ScmObj* %_37last47118, i64 4)
%ae48184 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55787 = alloca %struct.ScmObj*, align 8
%fptrToInt55788 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48185 to i64
%ae48185 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55788)
store volatile %struct.ScmObj* %ae48185, %struct.ScmObj** %stackaddr$makeclosure55787, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48185, %struct.ScmObj* %_37take47088, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48185, %struct.ScmObj* %_37length47085, i64 1)
%argslist55488$ae481830 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55789 = alloca %struct.ScmObj*, align 8
%argslist55488$ae481831 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48185, %struct.ScmObj* %argslist55488$ae481830)
store volatile %struct.ScmObj* %argslist55488$ae481831, %struct.ScmObj** %stackaddr$prim55789, align 8
%stackaddr$prim55790 = alloca %struct.ScmObj*, align 8
%argslist55488$ae481832 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48184, %struct.ScmObj* %argslist55488$ae481831)
store volatile %struct.ScmObj* %argslist55488$ae481832, %struct.ScmObj** %stackaddr$prim55790, align 8
%clofunc55791 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48183)
musttail call tailcc void %clofunc55791(%struct.ScmObj* %ae48183, %struct.ScmObj* %argslist55488$ae481832)
ret void
}

define tailcc void @proc_clo$ae48183(%struct.ScmObj* %env$ae48183,%struct.ScmObj* %current_45args55131) {
%stackaddr$env-ref55792 = alloca %struct.ScmObj*, align 8
%_37foldr147096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48183, i64 0)
store %struct.ScmObj* %_37foldr147096, %struct.ScmObj** %stackaddr$env-ref55792
%stackaddr$env-ref55793 = alloca %struct.ScmObj*, align 8
%_37foldl147080 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48183, i64 1)
store %struct.ScmObj* %_37foldl147080, %struct.ScmObj** %stackaddr$env-ref55793
%stackaddr$env-ref55794 = alloca %struct.ScmObj*, align 8
%_37map147092 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48183, i64 2)
store %struct.ScmObj* %_37map147092, %struct.ScmObj** %stackaddr$env-ref55794
%stackaddr$env-ref55795 = alloca %struct.ScmObj*, align 8
%Ycmb47075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48183, i64 3)
store %struct.ScmObj* %Ycmb47075, %struct.ScmObj** %stackaddr$env-ref55795
%stackaddr$env-ref55796 = alloca %struct.ScmObj*, align 8
%_37last47118 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48183, i64 4)
store %struct.ScmObj* %_37last47118, %struct.ScmObj** %stackaddr$env-ref55796
%stackaddr$prim55797 = alloca %struct.ScmObj*, align 8
%_95k47361 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55131)
store volatile %struct.ScmObj* %_95k47361, %struct.ScmObj** %stackaddr$prim55797, align 8
%stackaddr$prim55798 = alloca %struct.ScmObj*, align 8
%current_45args55132 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55131)
store volatile %struct.ScmObj* %current_45args55132, %struct.ScmObj** %stackaddr$prim55798, align 8
%stackaddr$prim55799 = alloca %struct.ScmObj*, align 8
%_37drop_45right47115 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55132)
store volatile %struct.ScmObj* %_37drop_45right47115, %struct.ScmObj** %stackaddr$prim55799, align 8
%stackaddr$makeclosure55800 = alloca %struct.ScmObj*, align 8
%fptrToInt55801 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48213 to i64
%ae48213 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55801)
store volatile %struct.ScmObj* %ae48213, %struct.ScmObj** %stackaddr$makeclosure55800, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48213, %struct.ScmObj* %_37foldr147096, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48213, %struct.ScmObj* %_37foldl147080, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48213, %struct.ScmObj* %Ycmb47075, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48213, %struct.ScmObj* %_37last47118, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48213, %struct.ScmObj* %_37drop_45right47115, i64 4)
%ae48214 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55802 = alloca %struct.ScmObj*, align 8
%fptrToInt55803 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48215 to i64
%ae48215 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55803)
store volatile %struct.ScmObj* %ae48215, %struct.ScmObj** %stackaddr$makeclosure55802, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48215, %struct.ScmObj* %_37map147092, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48215, %struct.ScmObj* %_37foldr147096, i64 1)
%argslist55478$ae482130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55804 = alloca %struct.ScmObj*, align 8
%argslist55478$ae482131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48215, %struct.ScmObj* %argslist55478$ae482130)
store volatile %struct.ScmObj* %argslist55478$ae482131, %struct.ScmObj** %stackaddr$prim55804, align 8
%stackaddr$prim55805 = alloca %struct.ScmObj*, align 8
%argslist55478$ae482132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48214, %struct.ScmObj* %argslist55478$ae482131)
store volatile %struct.ScmObj* %argslist55478$ae482132, %struct.ScmObj** %stackaddr$prim55805, align 8
%clofunc55806 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48213)
musttail call tailcc void %clofunc55806(%struct.ScmObj* %ae48213, %struct.ScmObj* %argslist55478$ae482132)
ret void
}

define tailcc void @proc_clo$ae48213(%struct.ScmObj* %env$ae48213,%struct.ScmObj* %current_45args55134) {
%stackaddr$env-ref55807 = alloca %struct.ScmObj*, align 8
%_37foldr147096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48213, i64 0)
store %struct.ScmObj* %_37foldr147096, %struct.ScmObj** %stackaddr$env-ref55807
%stackaddr$env-ref55808 = alloca %struct.ScmObj*, align 8
%_37foldl147080 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48213, i64 1)
store %struct.ScmObj* %_37foldl147080, %struct.ScmObj** %stackaddr$env-ref55808
%stackaddr$env-ref55809 = alloca %struct.ScmObj*, align 8
%Ycmb47075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48213, i64 2)
store %struct.ScmObj* %Ycmb47075, %struct.ScmObj** %stackaddr$env-ref55809
%stackaddr$env-ref55810 = alloca %struct.ScmObj*, align 8
%_37last47118 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48213, i64 3)
store %struct.ScmObj* %_37last47118, %struct.ScmObj** %stackaddr$env-ref55810
%stackaddr$env-ref55811 = alloca %struct.ScmObj*, align 8
%_37drop_45right47115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48213, i64 4)
store %struct.ScmObj* %_37drop_45right47115, %struct.ScmObj** %stackaddr$env-ref55811
%stackaddr$prim55812 = alloca %struct.ScmObj*, align 8
%_95k47362 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55134)
store volatile %struct.ScmObj* %_95k47362, %struct.ScmObj** %stackaddr$prim55812, align 8
%stackaddr$prim55813 = alloca %struct.ScmObj*, align 8
%current_45args55135 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55134)
store volatile %struct.ScmObj* %current_45args55135, %struct.ScmObj** %stackaddr$prim55813, align 8
%stackaddr$prim55814 = alloca %struct.ScmObj*, align 8
%anf_45bind47257 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55135)
store volatile %struct.ScmObj* %anf_45bind47257, %struct.ScmObj** %stackaddr$prim55814, align 8
%stackaddr$makeclosure55815 = alloca %struct.ScmObj*, align 8
%fptrToInt55816 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48597 to i64
%ae48597 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55816)
store volatile %struct.ScmObj* %ae48597, %struct.ScmObj** %stackaddr$makeclosure55815, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48597, %struct.ScmObj* %_37foldr147096, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48597, %struct.ScmObj* %_37foldl147080, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48597, %struct.ScmObj* %Ycmb47075, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48597, %struct.ScmObj* %_37last47118, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48597, %struct.ScmObj* %_37drop_45right47115, i64 4)
%argslist55418$Ycmb470750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55817 = alloca %struct.ScmObj*, align 8
%argslist55418$Ycmb470751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47257, %struct.ScmObj* %argslist55418$Ycmb470750)
store volatile %struct.ScmObj* %argslist55418$Ycmb470751, %struct.ScmObj** %stackaddr$prim55817, align 8
%stackaddr$prim55818 = alloca %struct.ScmObj*, align 8
%argslist55418$Ycmb470752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48597, %struct.ScmObj* %argslist55418$Ycmb470751)
store volatile %struct.ScmObj* %argslist55418$Ycmb470752, %struct.ScmObj** %stackaddr$prim55818, align 8
%clofunc55819 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47075)
musttail call tailcc void %clofunc55819(%struct.ScmObj* %Ycmb47075, %struct.ScmObj* %argslist55418$Ycmb470752)
ret void
}

define tailcc void @proc_clo$ae48597(%struct.ScmObj* %env$ae48597,%struct.ScmObj* %current_45args55137) {
%stackaddr$env-ref55820 = alloca %struct.ScmObj*, align 8
%_37foldr147096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48597, i64 0)
store %struct.ScmObj* %_37foldr147096, %struct.ScmObj** %stackaddr$env-ref55820
%stackaddr$env-ref55821 = alloca %struct.ScmObj*, align 8
%_37foldl147080 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48597, i64 1)
store %struct.ScmObj* %_37foldl147080, %struct.ScmObj** %stackaddr$env-ref55821
%stackaddr$env-ref55822 = alloca %struct.ScmObj*, align 8
%Ycmb47075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48597, i64 2)
store %struct.ScmObj* %Ycmb47075, %struct.ScmObj** %stackaddr$env-ref55822
%stackaddr$env-ref55823 = alloca %struct.ScmObj*, align 8
%_37last47118 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48597, i64 3)
store %struct.ScmObj* %_37last47118, %struct.ScmObj** %stackaddr$env-ref55823
%stackaddr$env-ref55824 = alloca %struct.ScmObj*, align 8
%_37drop_45right47115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48597, i64 4)
store %struct.ScmObj* %_37drop_45right47115, %struct.ScmObj** %stackaddr$env-ref55824
%stackaddr$prim55825 = alloca %struct.ScmObj*, align 8
%_95k47363 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55137)
store volatile %struct.ScmObj* %_95k47363, %struct.ScmObj** %stackaddr$prim55825, align 8
%stackaddr$prim55826 = alloca %struct.ScmObj*, align 8
%current_45args55138 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55137)
store volatile %struct.ScmObj* %current_45args55138, %struct.ScmObj** %stackaddr$prim55826, align 8
%stackaddr$prim55827 = alloca %struct.ScmObj*, align 8
%_37foldr47101 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55138)
store volatile %struct.ScmObj* %_37foldr47101, %struct.ScmObj** %stackaddr$prim55827, align 8
%stackaddr$makeclosure55828 = alloca %struct.ScmObj*, align 8
%fptrToInt55829 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48599 to i64
%ae48599 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt55829)
store volatile %struct.ScmObj* %ae48599, %struct.ScmObj** %stackaddr$makeclosure55828, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48599, %struct.ScmObj* %_37foldr147096, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48599, %struct.ScmObj* %_37foldl147080, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48599, %struct.ScmObj* %Ycmb47075, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48599, %struct.ScmObj* %_37last47118, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48599, %struct.ScmObj* %_37foldr47101, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48599, %struct.ScmObj* %_37drop_45right47115, i64 5)
%ae48600 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55830 = alloca %struct.ScmObj*, align 8
%fptrToInt55831 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48601 to i64
%ae48601 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55831)
store volatile %struct.ScmObj* %ae48601, %struct.ScmObj** %stackaddr$makeclosure55830, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48601, %struct.ScmObj* %_37foldr147096, i64 0)
%argslist55417$ae485990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55832 = alloca %struct.ScmObj*, align 8
%argslist55417$ae485991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48601, %struct.ScmObj* %argslist55417$ae485990)
store volatile %struct.ScmObj* %argslist55417$ae485991, %struct.ScmObj** %stackaddr$prim55832, align 8
%stackaddr$prim55833 = alloca %struct.ScmObj*, align 8
%argslist55417$ae485992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48600, %struct.ScmObj* %argslist55417$ae485991)
store volatile %struct.ScmObj* %argslist55417$ae485992, %struct.ScmObj** %stackaddr$prim55833, align 8
%clofunc55834 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48599)
musttail call tailcc void %clofunc55834(%struct.ScmObj* %ae48599, %struct.ScmObj* %argslist55417$ae485992)
ret void
}

define tailcc void @proc_clo$ae48599(%struct.ScmObj* %env$ae48599,%struct.ScmObj* %current_45args55140) {
%stackaddr$env-ref55835 = alloca %struct.ScmObj*, align 8
%_37foldr147096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48599, i64 0)
store %struct.ScmObj* %_37foldr147096, %struct.ScmObj** %stackaddr$env-ref55835
%stackaddr$env-ref55836 = alloca %struct.ScmObj*, align 8
%_37foldl147080 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48599, i64 1)
store %struct.ScmObj* %_37foldl147080, %struct.ScmObj** %stackaddr$env-ref55836
%stackaddr$env-ref55837 = alloca %struct.ScmObj*, align 8
%Ycmb47075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48599, i64 2)
store %struct.ScmObj* %Ycmb47075, %struct.ScmObj** %stackaddr$env-ref55837
%stackaddr$env-ref55838 = alloca %struct.ScmObj*, align 8
%_37last47118 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48599, i64 3)
store %struct.ScmObj* %_37last47118, %struct.ScmObj** %stackaddr$env-ref55838
%stackaddr$env-ref55839 = alloca %struct.ScmObj*, align 8
%_37foldr47101 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48599, i64 4)
store %struct.ScmObj* %_37foldr47101, %struct.ScmObj** %stackaddr$env-ref55839
%stackaddr$env-ref55840 = alloca %struct.ScmObj*, align 8
%_37drop_45right47115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48599, i64 5)
store %struct.ScmObj* %_37drop_45right47115, %struct.ScmObj** %stackaddr$env-ref55840
%stackaddr$prim55841 = alloca %struct.ScmObj*, align 8
%_95k47364 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55140)
store volatile %struct.ScmObj* %_95k47364, %struct.ScmObj** %stackaddr$prim55841, align 8
%stackaddr$prim55842 = alloca %struct.ScmObj*, align 8
%current_45args55141 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55140)
store volatile %struct.ScmObj* %current_45args55141, %struct.ScmObj** %stackaddr$prim55842, align 8
%stackaddr$prim55843 = alloca %struct.ScmObj*, align 8
%_37map147127 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55141)
store volatile %struct.ScmObj* %_37map147127, %struct.ScmObj** %stackaddr$prim55843, align 8
%stackaddr$makeclosure55844 = alloca %struct.ScmObj*, align 8
%fptrToInt55845 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48676 to i64
%ae48676 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55845)
store volatile %struct.ScmObj* %ae48676, %struct.ScmObj** %stackaddr$makeclosure55844, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48676, %struct.ScmObj* %_37foldr147096, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48676, %struct.ScmObj* %_37foldl147080, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48676, %struct.ScmObj* %Ycmb47075, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48676, %struct.ScmObj* %_37foldr47101, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48676, %struct.ScmObj* %_37map147127, i64 4)
%ae48677 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55846 = alloca %struct.ScmObj*, align 8
%fptrToInt55847 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48678 to i64
%ae48678 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55847)
store volatile %struct.ScmObj* %ae48678, %struct.ScmObj** %stackaddr$makeclosure55846, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48678, %struct.ScmObj* %_37last47118, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48678, %struct.ScmObj* %_37foldr47101, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48678, %struct.ScmObj* %_37drop_45right47115, i64 2)
%argslist55398$ae486760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55848 = alloca %struct.ScmObj*, align 8
%argslist55398$ae486761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48678, %struct.ScmObj* %argslist55398$ae486760)
store volatile %struct.ScmObj* %argslist55398$ae486761, %struct.ScmObj** %stackaddr$prim55848, align 8
%stackaddr$prim55849 = alloca %struct.ScmObj*, align 8
%argslist55398$ae486762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48677, %struct.ScmObj* %argslist55398$ae486761)
store volatile %struct.ScmObj* %argslist55398$ae486762, %struct.ScmObj** %stackaddr$prim55849, align 8
%clofunc55850 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48676)
musttail call tailcc void %clofunc55850(%struct.ScmObj* %ae48676, %struct.ScmObj* %argslist55398$ae486762)
ret void
}

define tailcc void @proc_clo$ae48676(%struct.ScmObj* %env$ae48676,%struct.ScmObj* %current_45args55143) {
%stackaddr$env-ref55851 = alloca %struct.ScmObj*, align 8
%_37foldr147096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48676, i64 0)
store %struct.ScmObj* %_37foldr147096, %struct.ScmObj** %stackaddr$env-ref55851
%stackaddr$env-ref55852 = alloca %struct.ScmObj*, align 8
%_37foldl147080 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48676, i64 1)
store %struct.ScmObj* %_37foldl147080, %struct.ScmObj** %stackaddr$env-ref55852
%stackaddr$env-ref55853 = alloca %struct.ScmObj*, align 8
%Ycmb47075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48676, i64 2)
store %struct.ScmObj* %Ycmb47075, %struct.ScmObj** %stackaddr$env-ref55853
%stackaddr$env-ref55854 = alloca %struct.ScmObj*, align 8
%_37foldr47101 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48676, i64 3)
store %struct.ScmObj* %_37foldr47101, %struct.ScmObj** %stackaddr$env-ref55854
%stackaddr$env-ref55855 = alloca %struct.ScmObj*, align 8
%_37map147127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48676, i64 4)
store %struct.ScmObj* %_37map147127, %struct.ScmObj** %stackaddr$env-ref55855
%stackaddr$prim55856 = alloca %struct.ScmObj*, align 8
%_95k47365 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55143)
store volatile %struct.ScmObj* %_95k47365, %struct.ScmObj** %stackaddr$prim55856, align 8
%stackaddr$prim55857 = alloca %struct.ScmObj*, align 8
%current_45args55144 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55143)
store volatile %struct.ScmObj* %current_45args55144, %struct.ScmObj** %stackaddr$prim55857, align 8
%stackaddr$prim55858 = alloca %struct.ScmObj*, align 8
%_37map47122 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55144)
store volatile %struct.ScmObj* %_37map47122, %struct.ScmObj** %stackaddr$prim55858, align 8
%stackaddr$makeclosure55859 = alloca %struct.ScmObj*, align 8
%fptrToInt55860 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48822 to i64
%ae48822 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55860)
store volatile %struct.ScmObj* %ae48822, %struct.ScmObj** %stackaddr$makeclosure55859, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48822, %struct.ScmObj* %Ycmb47075, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48822, %struct.ScmObj* %_37foldl147080, i64 1)
%ae48823 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55861 = alloca %struct.ScmObj*, align 8
%fptrToInt55862 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48824 to i64
%ae48824 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55862)
store volatile %struct.ScmObj* %ae48824, %struct.ScmObj** %stackaddr$makeclosure55861, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48824, %struct.ScmObj* %_37foldr47101, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48824, %struct.ScmObj* %_37foldr147096, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48824, %struct.ScmObj* %_37map147127, i64 2)
%argslist55381$ae488220 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55863 = alloca %struct.ScmObj*, align 8
%argslist55381$ae488221 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48824, %struct.ScmObj* %argslist55381$ae488220)
store volatile %struct.ScmObj* %argslist55381$ae488221, %struct.ScmObj** %stackaddr$prim55863, align 8
%stackaddr$prim55864 = alloca %struct.ScmObj*, align 8
%argslist55381$ae488222 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48823, %struct.ScmObj* %argslist55381$ae488221)
store volatile %struct.ScmObj* %argslist55381$ae488222, %struct.ScmObj** %stackaddr$prim55864, align 8
%clofunc55865 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48822)
musttail call tailcc void %clofunc55865(%struct.ScmObj* %ae48822, %struct.ScmObj* %argslist55381$ae488222)
ret void
}

define tailcc void @proc_clo$ae48822(%struct.ScmObj* %env$ae48822,%struct.ScmObj* %current_45args55146) {
%stackaddr$env-ref55866 = alloca %struct.ScmObj*, align 8
%Ycmb47075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48822, i64 0)
store %struct.ScmObj* %Ycmb47075, %struct.ScmObj** %stackaddr$env-ref55866
%stackaddr$env-ref55867 = alloca %struct.ScmObj*, align 8
%_37foldl147080 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48822, i64 1)
store %struct.ScmObj* %_37foldl147080, %struct.ScmObj** %stackaddr$env-ref55867
%stackaddr$prim55868 = alloca %struct.ScmObj*, align 8
%_95k47366 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55146)
store volatile %struct.ScmObj* %_95k47366, %struct.ScmObj** %stackaddr$prim55868, align 8
%stackaddr$prim55869 = alloca %struct.ScmObj*, align 8
%current_45args55147 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55146)
store volatile %struct.ScmObj* %current_45args55147, %struct.ScmObj** %stackaddr$prim55869, align 8
%stackaddr$prim55870 = alloca %struct.ScmObj*, align 8
%anf_45bind47277 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55147)
store volatile %struct.ScmObj* %anf_45bind47277, %struct.ScmObj** %stackaddr$prim55870, align 8
%stackaddr$makeclosure55871 = alloca %struct.ScmObj*, align 8
%fptrToInt55872 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49214 to i64
%ae49214 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55872)
store volatile %struct.ScmObj* %ae49214, %struct.ScmObj** %stackaddr$makeclosure55871, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49214, %struct.ScmObj* %_37foldl147080, i64 0)
%argslist55321$Ycmb470750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55873 = alloca %struct.ScmObj*, align 8
%argslist55321$Ycmb470751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47277, %struct.ScmObj* %argslist55321$Ycmb470750)
store volatile %struct.ScmObj* %argslist55321$Ycmb470751, %struct.ScmObj** %stackaddr$prim55873, align 8
%stackaddr$prim55874 = alloca %struct.ScmObj*, align 8
%argslist55321$Ycmb470752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49214, %struct.ScmObj* %argslist55321$Ycmb470751)
store volatile %struct.ScmObj* %argslist55321$Ycmb470752, %struct.ScmObj** %stackaddr$prim55874, align 8
%clofunc55875 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47075)
musttail call tailcc void %clofunc55875(%struct.ScmObj* %Ycmb47075, %struct.ScmObj* %argslist55321$Ycmb470752)
ret void
}

define tailcc void @proc_clo$ae49214(%struct.ScmObj* %env$ae49214,%struct.ScmObj* %current_45args55149) {
%stackaddr$env-ref55876 = alloca %struct.ScmObj*, align 8
%_37foldl147080 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49214, i64 0)
store %struct.ScmObj* %_37foldl147080, %struct.ScmObj** %stackaddr$env-ref55876
%stackaddr$prim55877 = alloca %struct.ScmObj*, align 8
%_95k47367 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55149)
store volatile %struct.ScmObj* %_95k47367, %struct.ScmObj** %stackaddr$prim55877, align 8
%stackaddr$prim55878 = alloca %struct.ScmObj*, align 8
%current_45args55150 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55149)
store volatile %struct.ScmObj* %current_45args55150, %struct.ScmObj** %stackaddr$prim55878, align 8
%stackaddr$prim55879 = alloca %struct.ScmObj*, align 8
%_37foldl47178 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55150)
store volatile %struct.ScmObj* %_37foldl47178, %struct.ScmObj** %stackaddr$prim55879, align 8
%stackaddr$makeclosure55880 = alloca %struct.ScmObj*, align 8
%fptrToInt55881 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49216 to i64
%ae49216 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55881)
store volatile %struct.ScmObj* %ae49216, %struct.ScmObj** %stackaddr$makeclosure55880, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49216, %struct.ScmObj* %_37foldl147080, i64 0)
%ae49217 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55882 = alloca %struct.ScmObj*, align 8
%fptrToInt55883 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49218 to i64
%ae49218 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55883)
store volatile %struct.ScmObj* %ae49218, %struct.ScmObj** %stackaddr$makeclosure55882, align 8
%argslist55320$ae492160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55884 = alloca %struct.ScmObj*, align 8
%argslist55320$ae492161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49218, %struct.ScmObj* %argslist55320$ae492160)
store volatile %struct.ScmObj* %argslist55320$ae492161, %struct.ScmObj** %stackaddr$prim55884, align 8
%stackaddr$prim55885 = alloca %struct.ScmObj*, align 8
%argslist55320$ae492162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49217, %struct.ScmObj* %argslist55320$ae492161)
store volatile %struct.ScmObj* %argslist55320$ae492162, %struct.ScmObj** %stackaddr$prim55885, align 8
%clofunc55886 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49216)
musttail call tailcc void %clofunc55886(%struct.ScmObj* %ae49216, %struct.ScmObj* %argslist55320$ae492162)
ret void
}

define tailcc void @proc_clo$ae49216(%struct.ScmObj* %env$ae49216,%struct.ScmObj* %current_45args55152) {
%stackaddr$env-ref55887 = alloca %struct.ScmObj*, align 8
%_37foldl147080 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49216, i64 0)
store %struct.ScmObj* %_37foldl147080, %struct.ScmObj** %stackaddr$env-ref55887
%stackaddr$prim55888 = alloca %struct.ScmObj*, align 8
%_95k47368 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55152)
store volatile %struct.ScmObj* %_95k47368, %struct.ScmObj** %stackaddr$prim55888, align 8
%stackaddr$prim55889 = alloca %struct.ScmObj*, align 8
%current_45args55153 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55152)
store volatile %struct.ScmObj* %current_45args55153, %struct.ScmObj** %stackaddr$prim55889, align 8
%stackaddr$prim55890 = alloca %struct.ScmObj*, align 8
%_37_6247175 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55153)
store volatile %struct.ScmObj* %_37_6247175, %struct.ScmObj** %stackaddr$prim55890, align 8
%stackaddr$makeclosure55891 = alloca %struct.ScmObj*, align 8
%fptrToInt55892 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49240 to i64
%ae49240 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55892)
store volatile %struct.ScmObj* %ae49240, %struct.ScmObj** %stackaddr$makeclosure55891, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49240, %struct.ScmObj* %_37foldl147080, i64 0)
%ae49241 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55893 = alloca %struct.ScmObj*, align 8
%fptrToInt55894 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49242 to i64
%ae49242 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55894)
store volatile %struct.ScmObj* %ae49242, %struct.ScmObj** %stackaddr$makeclosure55893, align 8
%argslist55314$ae492400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55895 = alloca %struct.ScmObj*, align 8
%argslist55314$ae492401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49242, %struct.ScmObj* %argslist55314$ae492400)
store volatile %struct.ScmObj* %argslist55314$ae492401, %struct.ScmObj** %stackaddr$prim55895, align 8
%stackaddr$prim55896 = alloca %struct.ScmObj*, align 8
%argslist55314$ae492402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49241, %struct.ScmObj* %argslist55314$ae492401)
store volatile %struct.ScmObj* %argslist55314$ae492402, %struct.ScmObj** %stackaddr$prim55896, align 8
%clofunc55897 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49240)
musttail call tailcc void %clofunc55897(%struct.ScmObj* %ae49240, %struct.ScmObj* %argslist55314$ae492402)
ret void
}

define tailcc void @proc_clo$ae49240(%struct.ScmObj* %env$ae49240,%struct.ScmObj* %current_45args55155) {
%stackaddr$env-ref55898 = alloca %struct.ScmObj*, align 8
%_37foldl147080 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49240, i64 0)
store %struct.ScmObj* %_37foldl147080, %struct.ScmObj** %stackaddr$env-ref55898
%stackaddr$prim55899 = alloca %struct.ScmObj*, align 8
%_95k47369 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55155)
store volatile %struct.ScmObj* %_95k47369, %struct.ScmObj** %stackaddr$prim55899, align 8
%stackaddr$prim55900 = alloca %struct.ScmObj*, align 8
%current_45args55156 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55155)
store volatile %struct.ScmObj* %current_45args55156, %struct.ScmObj** %stackaddr$prim55900, align 8
%stackaddr$prim55901 = alloca %struct.ScmObj*, align 8
%_37_62_6147172 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55156)
store volatile %struct.ScmObj* %_37_62_6147172, %struct.ScmObj** %stackaddr$prim55901, align 8
%ae49264 = call %struct.ScmObj* @const_init_int(i64 1)
%ae49265 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55902 = alloca %struct.ScmObj*, align 8
%_37append47168 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49264, %struct.ScmObj* %ae49265)
store volatile %struct.ScmObj* %_37append47168, %struct.ScmObj** %stackaddr$prim55902, align 8
%stackaddr$makeclosure55903 = alloca %struct.ScmObj*, align 8
%fptrToInt55904 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49266 to i64
%ae49266 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55904)
store volatile %struct.ScmObj* %ae49266, %struct.ScmObj** %stackaddr$makeclosure55903, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49266, %struct.ScmObj* %_37append47168, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49266, %struct.ScmObj* %_37foldl147080, i64 1)
%ae49267 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55905 = alloca %struct.ScmObj*, align 8
%fptrToInt55906 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49268 to i64
%ae49268 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55906)
store volatile %struct.ScmObj* %ae49268, %struct.ScmObj** %stackaddr$makeclosure55905, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49268, %struct.ScmObj* %_37append47168, i64 0)
%argslist55308$ae492660 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55907 = alloca %struct.ScmObj*, align 8
%argslist55308$ae492661 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49268, %struct.ScmObj* %argslist55308$ae492660)
store volatile %struct.ScmObj* %argslist55308$ae492661, %struct.ScmObj** %stackaddr$prim55907, align 8
%stackaddr$prim55908 = alloca %struct.ScmObj*, align 8
%argslist55308$ae492662 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49267, %struct.ScmObj* %argslist55308$ae492661)
store volatile %struct.ScmObj* %argslist55308$ae492662, %struct.ScmObj** %stackaddr$prim55908, align 8
%clofunc55909 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49266)
musttail call tailcc void %clofunc55909(%struct.ScmObj* %ae49266, %struct.ScmObj* %argslist55308$ae492662)
ret void
}

define tailcc void @proc_clo$ae49266(%struct.ScmObj* %env$ae49266,%struct.ScmObj* %current_45args55158) {
%stackaddr$env-ref55910 = alloca %struct.ScmObj*, align 8
%_37append47168 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49266, i64 0)
store %struct.ScmObj* %_37append47168, %struct.ScmObj** %stackaddr$env-ref55910
%stackaddr$env-ref55911 = alloca %struct.ScmObj*, align 8
%_37foldl147080 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49266, i64 1)
store %struct.ScmObj* %_37foldl147080, %struct.ScmObj** %stackaddr$env-ref55911
%stackaddr$prim55912 = alloca %struct.ScmObj*, align 8
%_95k47370 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55158)
store volatile %struct.ScmObj* %_95k47370, %struct.ScmObj** %stackaddr$prim55912, align 8
%stackaddr$prim55913 = alloca %struct.ScmObj*, align 8
%current_45args55159 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55158)
store volatile %struct.ScmObj* %current_45args55159, %struct.ScmObj** %stackaddr$prim55913, align 8
%stackaddr$prim55914 = alloca %struct.ScmObj*, align 8
%anf_45bind47285 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55159)
store volatile %struct.ScmObj* %anf_45bind47285, %struct.ScmObj** %stackaddr$prim55914, align 8
%ae49334 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55915 = alloca %struct.ScmObj*, align 8
%_95047169 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append47168, %struct.ScmObj* %ae49334, %struct.ScmObj* %anf_45bind47285)
store volatile %struct.ScmObj* %_95047169, %struct.ScmObj** %stackaddr$prim55915, align 8
%ae49337 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55916 = alloca %struct.ScmObj*, align 8
%_37append47167 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append47168, %struct.ScmObj* %ae49337)
store volatile %struct.ScmObj* %_37append47167, %struct.ScmObj** %stackaddr$prim55916, align 8
%stackaddr$makeclosure55917 = alloca %struct.ScmObj*, align 8
%fptrToInt55918 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49338 to i64
%ae49338 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55918)
store volatile %struct.ScmObj* %ae49338, %struct.ScmObj** %stackaddr$makeclosure55917, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49338, %struct.ScmObj* %_37foldl147080, i64 0)
%ae49339 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55919 = alloca %struct.ScmObj*, align 8
%fptrToInt55920 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49340 to i64
%ae49340 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55920)
store volatile %struct.ScmObj* %ae49340, %struct.ScmObj** %stackaddr$makeclosure55919, align 8
%argslist55297$ae493380 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55921 = alloca %struct.ScmObj*, align 8
%argslist55297$ae493381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49340, %struct.ScmObj* %argslist55297$ae493380)
store volatile %struct.ScmObj* %argslist55297$ae493381, %struct.ScmObj** %stackaddr$prim55921, align 8
%stackaddr$prim55922 = alloca %struct.ScmObj*, align 8
%argslist55297$ae493382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49339, %struct.ScmObj* %argslist55297$ae493381)
store volatile %struct.ScmObj* %argslist55297$ae493382, %struct.ScmObj** %stackaddr$prim55922, align 8
%clofunc55923 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49338)
musttail call tailcc void %clofunc55923(%struct.ScmObj* %ae49338, %struct.ScmObj* %argslist55297$ae493382)
ret void
}

define tailcc void @proc_clo$ae49338(%struct.ScmObj* %env$ae49338,%struct.ScmObj* %current_45args55161) {
%stackaddr$env-ref55924 = alloca %struct.ScmObj*, align 8
%_37foldl147080 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49338, i64 0)
store %struct.ScmObj* %_37foldl147080, %struct.ScmObj** %stackaddr$env-ref55924
%stackaddr$prim55925 = alloca %struct.ScmObj*, align 8
%_95k47371 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55161)
store volatile %struct.ScmObj* %_95k47371, %struct.ScmObj** %stackaddr$prim55925, align 8
%stackaddr$prim55926 = alloca %struct.ScmObj*, align 8
%current_45args55162 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55161)
store volatile %struct.ScmObj* %current_45args55162, %struct.ScmObj** %stackaddr$prim55926, align 8
%stackaddr$prim55927 = alloca %struct.ScmObj*, align 8
%_37list_6347160 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55162)
store volatile %struct.ScmObj* %_37list_6347160, %struct.ScmObj** %stackaddr$prim55927, align 8
%stackaddr$makeclosure55928 = alloca %struct.ScmObj*, align 8
%fptrToInt55929 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49754 to i64
%ae49754 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55929)
store volatile %struct.ScmObj* %ae49754, %struct.ScmObj** %stackaddr$makeclosure55928, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49754, %struct.ScmObj* %_37foldl147080, i64 0)
%ae49755 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55930 = alloca %struct.ScmObj*, align 8
%fptrToInt55931 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49756 to i64
%ae49756 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55931)
store volatile %struct.ScmObj* %ae49756, %struct.ScmObj** %stackaddr$makeclosure55930, align 8
%argslist55272$ae497540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55932 = alloca %struct.ScmObj*, align 8
%argslist55272$ae497541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49756, %struct.ScmObj* %argslist55272$ae497540)
store volatile %struct.ScmObj* %argslist55272$ae497541, %struct.ScmObj** %stackaddr$prim55932, align 8
%stackaddr$prim55933 = alloca %struct.ScmObj*, align 8
%argslist55272$ae497542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49755, %struct.ScmObj* %argslist55272$ae497541)
store volatile %struct.ScmObj* %argslist55272$ae497542, %struct.ScmObj** %stackaddr$prim55933, align 8
%clofunc55934 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49754)
musttail call tailcc void %clofunc55934(%struct.ScmObj* %ae49754, %struct.ScmObj* %argslist55272$ae497542)
ret void
}

define tailcc void @proc_clo$ae49754(%struct.ScmObj* %env$ae49754,%struct.ScmObj* %current_45args55164) {
%stackaddr$env-ref55935 = alloca %struct.ScmObj*, align 8
%_37foldl147080 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49754, i64 0)
store %struct.ScmObj* %_37foldl147080, %struct.ScmObj** %stackaddr$env-ref55935
%stackaddr$prim55936 = alloca %struct.ScmObj*, align 8
%_95k47372 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55164)
store volatile %struct.ScmObj* %_95k47372, %struct.ScmObj** %stackaddr$prim55936, align 8
%stackaddr$prim55937 = alloca %struct.ScmObj*, align 8
%current_45args55165 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55164)
store volatile %struct.ScmObj* %current_45args55165, %struct.ScmObj** %stackaddr$prim55937, align 8
%stackaddr$prim55938 = alloca %struct.ScmObj*, align 8
%_37drop47151 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55165)
store volatile %struct.ScmObj* %_37drop47151, %struct.ScmObj** %stackaddr$prim55938, align 8
%stackaddr$makeclosure55939 = alloca %struct.ScmObj*, align 8
%fptrToInt55940 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50290 to i64
%ae50290 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55940)
store volatile %struct.ScmObj* %ae50290, %struct.ScmObj** %stackaddr$makeclosure55939, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50290, %struct.ScmObj* %_37foldl147080, i64 0)
%ae50291 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55941 = alloca %struct.ScmObj*, align 8
%fptrToInt55942 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50292 to i64
%ae50292 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55942)
store volatile %struct.ScmObj* %ae50292, %struct.ScmObj** %stackaddr$makeclosure55941, align 8
%argslist55248$ae502900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55943 = alloca %struct.ScmObj*, align 8
%argslist55248$ae502901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50292, %struct.ScmObj* %argslist55248$ae502900)
store volatile %struct.ScmObj* %argslist55248$ae502901, %struct.ScmObj** %stackaddr$prim55943, align 8
%stackaddr$prim55944 = alloca %struct.ScmObj*, align 8
%argslist55248$ae502902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50291, %struct.ScmObj* %argslist55248$ae502901)
store volatile %struct.ScmObj* %argslist55248$ae502902, %struct.ScmObj** %stackaddr$prim55944, align 8
%clofunc55945 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50290)
musttail call tailcc void %clofunc55945(%struct.ScmObj* %ae50290, %struct.ScmObj* %argslist55248$ae502902)
ret void
}

define tailcc void @proc_clo$ae50290(%struct.ScmObj* %env$ae50290,%struct.ScmObj* %current_45args55167) {
%stackaddr$env-ref55946 = alloca %struct.ScmObj*, align 8
%_37foldl147080 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50290, i64 0)
store %struct.ScmObj* %_37foldl147080, %struct.ScmObj** %stackaddr$env-ref55946
%stackaddr$prim55947 = alloca %struct.ScmObj*, align 8
%_95k47373 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55167)
store volatile %struct.ScmObj* %_95k47373, %struct.ScmObj** %stackaddr$prim55947, align 8
%stackaddr$prim55948 = alloca %struct.ScmObj*, align 8
%current_45args55168 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55167)
store volatile %struct.ScmObj* %current_45args55168, %struct.ScmObj** %stackaddr$prim55948, align 8
%stackaddr$prim55949 = alloca %struct.ScmObj*, align 8
%_37memv47144 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55168)
store volatile %struct.ScmObj* %_37memv47144, %struct.ScmObj** %stackaddr$prim55949, align 8
%stackaddr$makeclosure55950 = alloca %struct.ScmObj*, align 8
%fptrToInt55951 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50694 to i64
%ae50694 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55951)
store volatile %struct.ScmObj* %ae50694, %struct.ScmObj** %stackaddr$makeclosure55950, align 8
%ae50695 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55952 = alloca %struct.ScmObj*, align 8
%fptrToInt55953 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50696 to i64
%ae50696 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55953)
store volatile %struct.ScmObj* %ae50696, %struct.ScmObj** %stackaddr$makeclosure55952, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50696, %struct.ScmObj* %_37foldl147080, i64 0)
%argslist55222$ae506940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55954 = alloca %struct.ScmObj*, align 8
%argslist55222$ae506941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50696, %struct.ScmObj* %argslist55222$ae506940)
store volatile %struct.ScmObj* %argslist55222$ae506941, %struct.ScmObj** %stackaddr$prim55954, align 8
%stackaddr$prim55955 = alloca %struct.ScmObj*, align 8
%argslist55222$ae506942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50695, %struct.ScmObj* %argslist55222$ae506941)
store volatile %struct.ScmObj* %argslist55222$ae506942, %struct.ScmObj** %stackaddr$prim55955, align 8
%clofunc55956 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50694)
musttail call tailcc void %clofunc55956(%struct.ScmObj* %ae50694, %struct.ScmObj* %argslist55222$ae506942)
ret void
}

define tailcc void @proc_clo$ae50694(%struct.ScmObj* %env$ae50694,%struct.ScmObj* %current_45args55170) {
%stackaddr$prim55957 = alloca %struct.ScmObj*, align 8
%_95k47374 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55170)
store volatile %struct.ScmObj* %_95k47374, %struct.ScmObj** %stackaddr$prim55957, align 8
%stackaddr$prim55958 = alloca %struct.ScmObj*, align 8
%current_45args55171 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55170)
store volatile %struct.ScmObj* %current_45args55171, %struct.ScmObj** %stackaddr$prim55958, align 8
%stackaddr$prim55959 = alloca %struct.ScmObj*, align 8
%_37_4747140 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55171)
store volatile %struct.ScmObj* %_37_4747140, %struct.ScmObj** %stackaddr$prim55959, align 8
%stackaddr$makeclosure55960 = alloca %struct.ScmObj*, align 8
%fptrToInt55961 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50792 to i64
%ae50792 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55961)
store volatile %struct.ScmObj* %ae50792, %struct.ScmObj** %stackaddr$makeclosure55960, align 8
%ae50793 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55962 = alloca %struct.ScmObj*, align 8
%fptrToInt55963 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50794 to i64
%ae50794 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55963)
store volatile %struct.ScmObj* %ae50794, %struct.ScmObj** %stackaddr$makeclosure55962, align 8
%argslist55209$ae507920 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55964 = alloca %struct.ScmObj*, align 8
%argslist55209$ae507921 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50794, %struct.ScmObj* %argslist55209$ae507920)
store volatile %struct.ScmObj* %argslist55209$ae507921, %struct.ScmObj** %stackaddr$prim55964, align 8
%stackaddr$prim55965 = alloca %struct.ScmObj*, align 8
%argslist55209$ae507922 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50793, %struct.ScmObj* %argslist55209$ae507921)
store volatile %struct.ScmObj* %argslist55209$ae507922, %struct.ScmObj** %stackaddr$prim55965, align 8
%clofunc55966 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50792)
musttail call tailcc void %clofunc55966(%struct.ScmObj* %ae50792, %struct.ScmObj* %argslist55209$ae507922)
ret void
}

define tailcc void @proc_clo$ae50792(%struct.ScmObj* %env$ae50792,%struct.ScmObj* %current_45args55173) {
%stackaddr$prim55967 = alloca %struct.ScmObj*, align 8
%_95k47375 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55173)
store volatile %struct.ScmObj* %_95k47375, %struct.ScmObj** %stackaddr$prim55967, align 8
%stackaddr$prim55968 = alloca %struct.ScmObj*, align 8
%current_45args55174 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55173)
store volatile %struct.ScmObj* %current_45args55174, %struct.ScmObj** %stackaddr$prim55968, align 8
%stackaddr$prim55969 = alloca %struct.ScmObj*, align 8
%_37first47138 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55174)
store volatile %struct.ScmObj* %_37first47138, %struct.ScmObj** %stackaddr$prim55969, align 8
%stackaddr$makeclosure55970 = alloca %struct.ScmObj*, align 8
%fptrToInt55971 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50812 to i64
%ae50812 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55971)
store volatile %struct.ScmObj* %ae50812, %struct.ScmObj** %stackaddr$makeclosure55970, align 8
%ae50813 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55972 = alloca %struct.ScmObj*, align 8
%fptrToInt55973 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50814 to i64
%ae50814 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55973)
store volatile %struct.ScmObj* %ae50814, %struct.ScmObj** %stackaddr$makeclosure55972, align 8
%argslist55204$ae508120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55974 = alloca %struct.ScmObj*, align 8
%argslist55204$ae508121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50814, %struct.ScmObj* %argslist55204$ae508120)
store volatile %struct.ScmObj* %argslist55204$ae508121, %struct.ScmObj** %stackaddr$prim55974, align 8
%stackaddr$prim55975 = alloca %struct.ScmObj*, align 8
%argslist55204$ae508122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50813, %struct.ScmObj* %argslist55204$ae508121)
store volatile %struct.ScmObj* %argslist55204$ae508122, %struct.ScmObj** %stackaddr$prim55975, align 8
%clofunc55976 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50812)
musttail call tailcc void %clofunc55976(%struct.ScmObj* %ae50812, %struct.ScmObj* %argslist55204$ae508122)
ret void
}

define tailcc void @proc_clo$ae50812(%struct.ScmObj* %env$ae50812,%struct.ScmObj* %current_45args55176) {
%stackaddr$prim55977 = alloca %struct.ScmObj*, align 8
%_95k47376 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55176)
store volatile %struct.ScmObj* %_95k47376, %struct.ScmObj** %stackaddr$prim55977, align 8
%stackaddr$prim55978 = alloca %struct.ScmObj*, align 8
%current_45args55177 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55176)
store volatile %struct.ScmObj* %current_45args55177, %struct.ScmObj** %stackaddr$prim55978, align 8
%stackaddr$prim55979 = alloca %struct.ScmObj*, align 8
%_37second47136 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55177)
store volatile %struct.ScmObj* %_37second47136, %struct.ScmObj** %stackaddr$prim55979, align 8
%stackaddr$makeclosure55980 = alloca %struct.ScmObj*, align 8
%fptrToInt55981 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50834 to i64
%ae50834 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55981)
store volatile %struct.ScmObj* %ae50834, %struct.ScmObj** %stackaddr$makeclosure55980, align 8
%ae50835 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55982 = alloca %struct.ScmObj*, align 8
%fptrToInt55983 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50836 to i64
%ae50836 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55983)
store volatile %struct.ScmObj* %ae50836, %struct.ScmObj** %stackaddr$makeclosure55982, align 8
%argslist55199$ae508340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55984 = alloca %struct.ScmObj*, align 8
%argslist55199$ae508341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50836, %struct.ScmObj* %argslist55199$ae508340)
store volatile %struct.ScmObj* %argslist55199$ae508341, %struct.ScmObj** %stackaddr$prim55984, align 8
%stackaddr$prim55985 = alloca %struct.ScmObj*, align 8
%argslist55199$ae508342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50835, %struct.ScmObj* %argslist55199$ae508341)
store volatile %struct.ScmObj* %argslist55199$ae508342, %struct.ScmObj** %stackaddr$prim55985, align 8
%clofunc55986 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50834)
musttail call tailcc void %clofunc55986(%struct.ScmObj* %ae50834, %struct.ScmObj* %argslist55199$ae508342)
ret void
}

define tailcc void @proc_clo$ae50834(%struct.ScmObj* %env$ae50834,%struct.ScmObj* %current_45args55179) {
%stackaddr$prim55987 = alloca %struct.ScmObj*, align 8
%_95k47377 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55179)
store volatile %struct.ScmObj* %_95k47377, %struct.ScmObj** %stackaddr$prim55987, align 8
%stackaddr$prim55988 = alloca %struct.ScmObj*, align 8
%current_45args55180 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55179)
store volatile %struct.ScmObj* %current_45args55180, %struct.ScmObj** %stackaddr$prim55988, align 8
%stackaddr$prim55989 = alloca %struct.ScmObj*, align 8
%_37third47134 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55180)
store volatile %struct.ScmObj* %_37third47134, %struct.ScmObj** %stackaddr$prim55989, align 8
%stackaddr$makeclosure55990 = alloca %struct.ScmObj*, align 8
%fptrToInt55991 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50858 to i64
%ae50858 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55991)
store volatile %struct.ScmObj* %ae50858, %struct.ScmObj** %stackaddr$makeclosure55990, align 8
%ae50859 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55992 = alloca %struct.ScmObj*, align 8
%fptrToInt55993 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50860 to i64
%ae50860 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55993)
store volatile %struct.ScmObj* %ae50860, %struct.ScmObj** %stackaddr$makeclosure55992, align 8
%argslist55194$ae508580 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55994 = alloca %struct.ScmObj*, align 8
%argslist55194$ae508581 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50860, %struct.ScmObj* %argslist55194$ae508580)
store volatile %struct.ScmObj* %argslist55194$ae508581, %struct.ScmObj** %stackaddr$prim55994, align 8
%stackaddr$prim55995 = alloca %struct.ScmObj*, align 8
%argslist55194$ae508582 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50859, %struct.ScmObj* %argslist55194$ae508581)
store volatile %struct.ScmObj* %argslist55194$ae508582, %struct.ScmObj** %stackaddr$prim55995, align 8
%clofunc55996 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50858)
musttail call tailcc void %clofunc55996(%struct.ScmObj* %ae50858, %struct.ScmObj* %argslist55194$ae508582)
ret void
}

define tailcc void @proc_clo$ae50858(%struct.ScmObj* %env$ae50858,%struct.ScmObj* %current_45args55182) {
%stackaddr$prim55997 = alloca %struct.ScmObj*, align 8
%_95k47378 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55182)
store volatile %struct.ScmObj* %_95k47378, %struct.ScmObj** %stackaddr$prim55997, align 8
%stackaddr$prim55998 = alloca %struct.ScmObj*, align 8
%current_45args55183 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55182)
store volatile %struct.ScmObj* %current_45args55183, %struct.ScmObj** %stackaddr$prim55998, align 8
%stackaddr$prim55999 = alloca %struct.ScmObj*, align 8
%_37fourth47132 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55183)
store volatile %struct.ScmObj* %_37fourth47132, %struct.ScmObj** %stackaddr$prim55999, align 8
%stackaddr$prim56000 = alloca %struct.ScmObj*, align 8
%anf_45bind47321 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind47321, %struct.ScmObj** %stackaddr$prim56000, align 8
%ae50884 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56001 = alloca %struct.ScmObj*, align 8
%x47193 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50884, %struct.ScmObj* %anf_45bind47321)
store volatile %struct.ScmObj* %x47193, %struct.ScmObj** %stackaddr$prim56001, align 8
%ae50887 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50888 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56002 = alloca %struct.ScmObj*, align 8
%t4706847194 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %x47193, %struct.ScmObj* %ae50887, %struct.ScmObj* %ae50888)
store volatile %struct.ScmObj* %t4706847194, %struct.ScmObj** %stackaddr$prim56002, align 8
%stackaddr$prim56003 = alloca %struct.ScmObj*, align 8
%anf_45bind47322 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind47322, %struct.ScmObj** %stackaddr$prim56003, align 8
%ae50889 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56004 = alloca %struct.ScmObj*, align 8
%a47196 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50889, %struct.ScmObj* %anf_45bind47322)
store volatile %struct.ScmObj* %a47196, %struct.ScmObj** %stackaddr$prim56004, align 8
%stackaddr$prim56005 = alloca %struct.ScmObj*, align 8
%anf_45bind47323 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind47323, %struct.ScmObj** %stackaddr$prim56005, align 8
%ae50891 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56006 = alloca %struct.ScmObj*, align 8
%b47195 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50891, %struct.ScmObj* %anf_45bind47323)
store volatile %struct.ScmObj* %b47195, %struct.ScmObj** %stackaddr$prim56006, align 8
%ae50894 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56007 = alloca %struct.ScmObj*, align 8
%anf_45bind47324 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %x47193, %struct.ScmObj* %ae50894)
store volatile %struct.ScmObj* %anf_45bind47324, %struct.ScmObj** %stackaddr$prim56007, align 8
%ae50896 = call %struct.ScmObj* @const_init_int(i64 3)
%stackaddr$prim56008 = alloca %struct.ScmObj*, align 8
%anf_45bind47325 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %anf_45bind47324, %struct.ScmObj* %ae50896)
store volatile %struct.ScmObj* %anf_45bind47325, %struct.ScmObj** %stackaddr$prim56008, align 8
%ae50898 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56009 = alloca %struct.ScmObj*, align 8
%anf_45bind47326 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %x47193, %struct.ScmObj* %ae50898, %struct.ScmObj* %anf_45bind47325)
store volatile %struct.ScmObj* %anf_45bind47326, %struct.ScmObj** %stackaddr$prim56009, align 8
%ae50901 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56010 = alloca %struct.ScmObj*, align 8
%t4707047198 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a47196, %struct.ScmObj* %ae50901, %struct.ScmObj* %anf_45bind47326)
store volatile %struct.ScmObj* %t4707047198, %struct.ScmObj** %stackaddr$prim56010, align 8
%ae50904 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56011 = alloca %struct.ScmObj*, align 8
%anf_45bind47327 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %x47193, %struct.ScmObj* %ae50904)
store volatile %struct.ScmObj* %anf_45bind47327, %struct.ScmObj** %stackaddr$prim56011, align 8
%ae50906 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim56012 = alloca %struct.ScmObj*, align 8
%anf_45bind47328 = call %struct.ScmObj* @prim__42(%struct.ScmObj* %anf_45bind47327, %struct.ScmObj* %ae50906)
store volatile %struct.ScmObj* %anf_45bind47328, %struct.ScmObj** %stackaddr$prim56012, align 8
%ae50908 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56013 = alloca %struct.ScmObj*, align 8
%anf_45bind47329 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %x47193, %struct.ScmObj* %ae50908, %struct.ScmObj* %anf_45bind47328)
store volatile %struct.ScmObj* %anf_45bind47329, %struct.ScmObj** %stackaddr$prim56013, align 8
%ae50911 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56014 = alloca %struct.ScmObj*, align 8
%t4706947197 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %b47195, %struct.ScmObj* %ae50911, %struct.ScmObj* %anf_45bind47329)
store volatile %struct.ScmObj* %t4706947197, %struct.ScmObj** %stackaddr$prim56014, align 8
%stackaddr$prim56015 = alloca %struct.ScmObj*, align 8
%anf_45bind47330 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind47330, %struct.ScmObj** %stackaddr$prim56015, align 8
%ae50913 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56016 = alloca %struct.ScmObj*, align 8
%a47200 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50913, %struct.ScmObj* %anf_45bind47330)
store volatile %struct.ScmObj* %a47200, %struct.ScmObj** %stackaddr$prim56016, align 8
%stackaddr$prim56017 = alloca %struct.ScmObj*, align 8
%anf_45bind47331 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind47331, %struct.ScmObj** %stackaddr$prim56017, align 8
%ae50915 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56018 = alloca %struct.ScmObj*, align 8
%b47199 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50915, %struct.ScmObj* %anf_45bind47331)
store volatile %struct.ScmObj* %b47199, %struct.ScmObj** %stackaddr$prim56018, align 8
%ae50918 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56019 = alloca %struct.ScmObj*, align 8
%anf_45bind47332 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %x47193, %struct.ScmObj* %ae50918)
store volatile %struct.ScmObj* %anf_45bind47332, %struct.ScmObj** %stackaddr$prim56019, align 8
%ae50920 = call %struct.ScmObj* @const_init_int(i64 3)
%stackaddr$prim56020 = alloca %struct.ScmObj*, align 8
%anf_45bind47333 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %anf_45bind47332, %struct.ScmObj* %ae50920)
store volatile %struct.ScmObj* %anf_45bind47333, %struct.ScmObj** %stackaddr$prim56020, align 8
%ae50922 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56021 = alloca %struct.ScmObj*, align 8
%anf_45bind47334 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %x47193, %struct.ScmObj* %ae50922, %struct.ScmObj* %anf_45bind47333)
store volatile %struct.ScmObj* %anf_45bind47334, %struct.ScmObj** %stackaddr$prim56021, align 8
%ae50925 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56022 = alloca %struct.ScmObj*, align 8
%t4707247202 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a47200, %struct.ScmObj* %ae50925, %struct.ScmObj* %anf_45bind47334)
store volatile %struct.ScmObj* %t4707247202, %struct.ScmObj** %stackaddr$prim56022, align 8
%ae50928 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56023 = alloca %struct.ScmObj*, align 8
%anf_45bind47335 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %x47193, %struct.ScmObj* %ae50928)
store volatile %struct.ScmObj* %anf_45bind47335, %struct.ScmObj** %stackaddr$prim56023, align 8
%ae50930 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim56024 = alloca %struct.ScmObj*, align 8
%anf_45bind47336 = call %struct.ScmObj* @prim__42(%struct.ScmObj* %anf_45bind47335, %struct.ScmObj* %ae50930)
store volatile %struct.ScmObj* %anf_45bind47336, %struct.ScmObj** %stackaddr$prim56024, align 8
%ae50932 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56025 = alloca %struct.ScmObj*, align 8
%anf_45bind47337 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %x47193, %struct.ScmObj* %ae50932, %struct.ScmObj* %anf_45bind47336)
store volatile %struct.ScmObj* %anf_45bind47337, %struct.ScmObj** %stackaddr$prim56025, align 8
%ae50935 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56026 = alloca %struct.ScmObj*, align 8
%t4707147201 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %b47199, %struct.ScmObj* %ae50935, %struct.ScmObj* %anf_45bind47337)
store volatile %struct.ScmObj* %t4707147201, %struct.ScmObj** %stackaddr$prim56026, align 8
%ae50938 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56027 = alloca %struct.ScmObj*, align 8
%anf_45bind47338 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %x47193, %struct.ScmObj* %ae50938)
store volatile %struct.ScmObj* %anf_45bind47338, %struct.ScmObj** %stackaddr$prim56027, align 8
%ae50940 = call %struct.ScmObj* @const_init_int(i64 3)
%stackaddr$prim56028 = alloca %struct.ScmObj*, align 8
%anf_45bind47339 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %anf_45bind47338, %struct.ScmObj* %ae50940)
store volatile %struct.ScmObj* %anf_45bind47339, %struct.ScmObj** %stackaddr$prim56028, align 8
%ae50942 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56029 = alloca %struct.ScmObj*, align 8
%a47203 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %x47193, %struct.ScmObj* %ae50942, %struct.ScmObj* %anf_45bind47339)
store volatile %struct.ScmObj* %a47203, %struct.ScmObj** %stackaddr$prim56029, align 8
%ae50945 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56030 = alloca %struct.ScmObj*, align 8
%anf_45bind47340 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %x47193, %struct.ScmObj* %ae50945)
store volatile %struct.ScmObj* %anf_45bind47340, %struct.ScmObj** %stackaddr$prim56030, align 8
%ae50947 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim56031 = alloca %struct.ScmObj*, align 8
%anf_45bind47341 = call %struct.ScmObj* @prim__42(%struct.ScmObj* %anf_45bind47340, %struct.ScmObj* %ae50947)
store volatile %struct.ScmObj* %anf_45bind47341, %struct.ScmObj** %stackaddr$prim56031, align 8
%ae50949 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56032 = alloca %struct.ScmObj*, align 8
%b47204 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %x47193, %struct.ScmObj* %ae50949, %struct.ScmObj* %anf_45bind47341)
store volatile %struct.ScmObj* %b47204, %struct.ScmObj** %stackaddr$prim56032, align 8
%e47205 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim56033 = alloca %struct.ScmObj*, align 8
%anf_45bind47342 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind47342, %struct.ScmObj** %stackaddr$prim56033, align 8
%ae50951 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56034 = alloca %struct.ScmObj*, align 8
%e47207 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50951, %struct.ScmObj* %anf_45bind47342)
store volatile %struct.ScmObj* %e47207, %struct.ScmObj** %stackaddr$prim56034, align 8
%stackaddr$prim56035 = alloca %struct.ScmObj*, align 8
%anf_45bind47343 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind47343, %struct.ScmObj** %stackaddr$prim56035, align 8
%ae50953 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56036 = alloca %struct.ScmObj*, align 8
%f47206 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50953, %struct.ScmObj* %anf_45bind47343)
store volatile %struct.ScmObj* %f47206, %struct.ScmObj** %stackaddr$prim56036, align 8
%ae50956 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50957 = call %struct.ScmObj* @const_init_int(i64 5)
%stackaddr$prim56037 = alloca %struct.ScmObj*, align 8
%t4707447209 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %e47207, %struct.ScmObj* %ae50956, %struct.ScmObj* %ae50957)
store volatile %struct.ScmObj* %t4707447209, %struct.ScmObj** %stackaddr$prim56037, align 8
%ae50959 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56038 = alloca %struct.ScmObj*, align 8
%anf_45bind47344 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %e47207, %struct.ScmObj* %ae50959)
store volatile %struct.ScmObj* %anf_45bind47344, %struct.ScmObj** %stackaddr$prim56038, align 8
%ae50961 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56039 = alloca %struct.ScmObj*, align 8
%t4707347208 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %f47206, %struct.ScmObj* %ae50961, %struct.ScmObj* %anf_45bind47344)
store volatile %struct.ScmObj* %t4707347208, %struct.ScmObj** %stackaddr$prim56039, align 8
%ae50964 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56040 = alloca %struct.ScmObj*, align 8
%anf_45bind47345 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %x47193, %struct.ScmObj* %ae50964)
store volatile %struct.ScmObj* %anf_45bind47345, %struct.ScmObj** %stackaddr$prim56040, align 8
%ae50966 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56041 = alloca %struct.ScmObj*, align 8
%anf_45bind47346 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %f47206, %struct.ScmObj* %ae50966)
store volatile %struct.ScmObj* %anf_45bind47346, %struct.ScmObj** %stackaddr$prim56041, align 8
%stackaddr$prim56042 = alloca %struct.ScmObj*, align 8
%cpsprim47379 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %anf_45bind47345, %struct.ScmObj* %anf_45bind47346)
store volatile %struct.ScmObj* %cpsprim47379, %struct.ScmObj** %stackaddr$prim56042, align 8
%stackaddr$makeclosure56043 = alloca %struct.ScmObj*, align 8
%fptrToInt56044 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50969 to i64
%ae50969 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56044)
store volatile %struct.ScmObj* %ae50969, %struct.ScmObj** %stackaddr$makeclosure56043, align 8
%ae50970 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55189$ae509690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56045 = alloca %struct.ScmObj*, align 8
%argslist55189$ae509691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47379, %struct.ScmObj* %argslist55189$ae509690)
store volatile %struct.ScmObj* %argslist55189$ae509691, %struct.ScmObj** %stackaddr$prim56045, align 8
%stackaddr$prim56046 = alloca %struct.ScmObj*, align 8
%argslist55189$ae509692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50970, %struct.ScmObj* %argslist55189$ae509691)
store volatile %struct.ScmObj* %argslist55189$ae509692, %struct.ScmObj** %stackaddr$prim56046, align 8
%clofunc56047 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50969)
musttail call tailcc void %clofunc56047(%struct.ScmObj* %ae50969, %struct.ScmObj* %argslist55189$ae509692)
ret void
}

define tailcc void @proc_clo$ae50969(%struct.ScmObj* %env$ae50969,%struct.ScmObj* %current_45args55185) {
%stackaddr$prim56048 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55185)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim56048, align 8
%stackaddr$prim56049 = alloca %struct.ScmObj*, align 8
%current_45args55186 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55185)
store volatile %struct.ScmObj* %current_45args55186, %struct.ScmObj** %stackaddr$prim56049, align 8
%stackaddr$prim56050 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55186)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim56050, align 8
%stackaddr$prim56051 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim56051, align 8
%argslist55188$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56052 = alloca %struct.ScmObj*, align 8
%argslist55188$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist55188$k0)
store volatile %struct.ScmObj* %argslist55188$k1, %struct.ScmObj** %stackaddr$prim56052, align 8
%clofunc56053 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc56053(%struct.ScmObj* %k, %struct.ScmObj* %argslist55188$k1)
ret void
}

define tailcc void @proc_clo$ae50860(%struct.ScmObj* %env$ae50860,%struct.ScmObj* %current_45args55190) {
%stackaddr$prim56054 = alloca %struct.ScmObj*, align 8
%k47380 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55190)
store volatile %struct.ScmObj* %k47380, %struct.ScmObj** %stackaddr$prim56054, align 8
%stackaddr$prim56055 = alloca %struct.ScmObj*, align 8
%current_45args55191 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55190)
store volatile %struct.ScmObj* %current_45args55191, %struct.ScmObj** %stackaddr$prim56055, align 8
%stackaddr$prim56056 = alloca %struct.ScmObj*, align 8
%x47133 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55191)
store volatile %struct.ScmObj* %x47133, %struct.ScmObj** %stackaddr$prim56056, align 8
%stackaddr$prim56057 = alloca %struct.ScmObj*, align 8
%anf_45bind47318 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47133)
store volatile %struct.ScmObj* %anf_45bind47318, %struct.ScmObj** %stackaddr$prim56057, align 8
%stackaddr$prim56058 = alloca %struct.ScmObj*, align 8
%anf_45bind47319 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47318)
store volatile %struct.ScmObj* %anf_45bind47319, %struct.ScmObj** %stackaddr$prim56058, align 8
%stackaddr$prim56059 = alloca %struct.ScmObj*, align 8
%anf_45bind47320 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47319)
store volatile %struct.ScmObj* %anf_45bind47320, %struct.ScmObj** %stackaddr$prim56059, align 8
%stackaddr$prim56060 = alloca %struct.ScmObj*, align 8
%cpsprim47381 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47320)
store volatile %struct.ScmObj* %cpsprim47381, %struct.ScmObj** %stackaddr$prim56060, align 8
%ae50866 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55193$k473800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56061 = alloca %struct.ScmObj*, align 8
%argslist55193$k473801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47381, %struct.ScmObj* %argslist55193$k473800)
store volatile %struct.ScmObj* %argslist55193$k473801, %struct.ScmObj** %stackaddr$prim56061, align 8
%stackaddr$prim56062 = alloca %struct.ScmObj*, align 8
%argslist55193$k473802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50866, %struct.ScmObj* %argslist55193$k473801)
store volatile %struct.ScmObj* %argslist55193$k473802, %struct.ScmObj** %stackaddr$prim56062, align 8
%clofunc56063 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47380)
musttail call tailcc void %clofunc56063(%struct.ScmObj* %k47380, %struct.ScmObj* %argslist55193$k473802)
ret void
}

define tailcc void @proc_clo$ae50836(%struct.ScmObj* %env$ae50836,%struct.ScmObj* %current_45args55195) {
%stackaddr$prim56064 = alloca %struct.ScmObj*, align 8
%k47382 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55195)
store volatile %struct.ScmObj* %k47382, %struct.ScmObj** %stackaddr$prim56064, align 8
%stackaddr$prim56065 = alloca %struct.ScmObj*, align 8
%current_45args55196 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55195)
store volatile %struct.ScmObj* %current_45args55196, %struct.ScmObj** %stackaddr$prim56065, align 8
%stackaddr$prim56066 = alloca %struct.ScmObj*, align 8
%x47135 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55196)
store volatile %struct.ScmObj* %x47135, %struct.ScmObj** %stackaddr$prim56066, align 8
%stackaddr$prim56067 = alloca %struct.ScmObj*, align 8
%anf_45bind47316 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47135)
store volatile %struct.ScmObj* %anf_45bind47316, %struct.ScmObj** %stackaddr$prim56067, align 8
%stackaddr$prim56068 = alloca %struct.ScmObj*, align 8
%anf_45bind47317 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47316)
store volatile %struct.ScmObj* %anf_45bind47317, %struct.ScmObj** %stackaddr$prim56068, align 8
%stackaddr$prim56069 = alloca %struct.ScmObj*, align 8
%cpsprim47383 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47317)
store volatile %struct.ScmObj* %cpsprim47383, %struct.ScmObj** %stackaddr$prim56069, align 8
%ae50841 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55198$k473820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56070 = alloca %struct.ScmObj*, align 8
%argslist55198$k473821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47383, %struct.ScmObj* %argslist55198$k473820)
store volatile %struct.ScmObj* %argslist55198$k473821, %struct.ScmObj** %stackaddr$prim56070, align 8
%stackaddr$prim56071 = alloca %struct.ScmObj*, align 8
%argslist55198$k473822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50841, %struct.ScmObj* %argslist55198$k473821)
store volatile %struct.ScmObj* %argslist55198$k473822, %struct.ScmObj** %stackaddr$prim56071, align 8
%clofunc56072 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47382)
musttail call tailcc void %clofunc56072(%struct.ScmObj* %k47382, %struct.ScmObj* %argslist55198$k473822)
ret void
}

define tailcc void @proc_clo$ae50814(%struct.ScmObj* %env$ae50814,%struct.ScmObj* %current_45args55200) {
%stackaddr$prim56073 = alloca %struct.ScmObj*, align 8
%k47384 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55200)
store volatile %struct.ScmObj* %k47384, %struct.ScmObj** %stackaddr$prim56073, align 8
%stackaddr$prim56074 = alloca %struct.ScmObj*, align 8
%current_45args55201 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55200)
store volatile %struct.ScmObj* %current_45args55201, %struct.ScmObj** %stackaddr$prim56074, align 8
%stackaddr$prim56075 = alloca %struct.ScmObj*, align 8
%x47137 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55201)
store volatile %struct.ScmObj* %x47137, %struct.ScmObj** %stackaddr$prim56075, align 8
%stackaddr$prim56076 = alloca %struct.ScmObj*, align 8
%anf_45bind47315 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47137)
store volatile %struct.ScmObj* %anf_45bind47315, %struct.ScmObj** %stackaddr$prim56076, align 8
%stackaddr$prim56077 = alloca %struct.ScmObj*, align 8
%cpsprim47385 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47315)
store volatile %struct.ScmObj* %cpsprim47385, %struct.ScmObj** %stackaddr$prim56077, align 8
%ae50818 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55203$k473840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56078 = alloca %struct.ScmObj*, align 8
%argslist55203$k473841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47385, %struct.ScmObj* %argslist55203$k473840)
store volatile %struct.ScmObj* %argslist55203$k473841, %struct.ScmObj** %stackaddr$prim56078, align 8
%stackaddr$prim56079 = alloca %struct.ScmObj*, align 8
%argslist55203$k473842 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50818, %struct.ScmObj* %argslist55203$k473841)
store volatile %struct.ScmObj* %argslist55203$k473842, %struct.ScmObj** %stackaddr$prim56079, align 8
%clofunc56080 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47384)
musttail call tailcc void %clofunc56080(%struct.ScmObj* %k47384, %struct.ScmObj* %argslist55203$k473842)
ret void
}

define tailcc void @proc_clo$ae50794(%struct.ScmObj* %env$ae50794,%struct.ScmObj* %current_45args55205) {
%stackaddr$prim56081 = alloca %struct.ScmObj*, align 8
%k47386 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55205)
store volatile %struct.ScmObj* %k47386, %struct.ScmObj** %stackaddr$prim56081, align 8
%stackaddr$prim56082 = alloca %struct.ScmObj*, align 8
%current_45args55206 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55205)
store volatile %struct.ScmObj* %current_45args55206, %struct.ScmObj** %stackaddr$prim56082, align 8
%stackaddr$prim56083 = alloca %struct.ScmObj*, align 8
%x47139 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55206)
store volatile %struct.ScmObj* %x47139, %struct.ScmObj** %stackaddr$prim56083, align 8
%stackaddr$prim56084 = alloca %struct.ScmObj*, align 8
%cpsprim47387 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47139)
store volatile %struct.ScmObj* %cpsprim47387, %struct.ScmObj** %stackaddr$prim56084, align 8
%ae50797 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55208$k473860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56085 = alloca %struct.ScmObj*, align 8
%argslist55208$k473861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47387, %struct.ScmObj* %argslist55208$k473860)
store volatile %struct.ScmObj* %argslist55208$k473861, %struct.ScmObj** %stackaddr$prim56085, align 8
%stackaddr$prim56086 = alloca %struct.ScmObj*, align 8
%argslist55208$k473862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50797, %struct.ScmObj* %argslist55208$k473861)
store volatile %struct.ScmObj* %argslist55208$k473862, %struct.ScmObj** %stackaddr$prim56086, align 8
%clofunc56087 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47386)
musttail call tailcc void %clofunc56087(%struct.ScmObj* %k47386, %struct.ScmObj* %argslist55208$k473862)
ret void
}

define tailcc void @proc_clo$ae50696(%struct.ScmObj* %env$ae50696,%struct.ScmObj* %args4714147388) {
%stackaddr$env-ref56088 = alloca %struct.ScmObj*, align 8
%_37foldl147080 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50696, i64 0)
store %struct.ScmObj* %_37foldl147080, %struct.ScmObj** %stackaddr$env-ref56088
%stackaddr$prim56089 = alloca %struct.ScmObj*, align 8
%k47389 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4714147388)
store volatile %struct.ScmObj* %k47389, %struct.ScmObj** %stackaddr$prim56089, align 8
%stackaddr$prim56090 = alloca %struct.ScmObj*, align 8
%args47141 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4714147388)
store volatile %struct.ScmObj* %args47141, %struct.ScmObj** %stackaddr$prim56090, align 8
%stackaddr$prim56091 = alloca %struct.ScmObj*, align 8
%anf_45bind47309 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args47141)
store volatile %struct.ScmObj* %anf_45bind47309, %struct.ScmObj** %stackaddr$prim56091, align 8
%truthy$cmp56092 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47309)
%cmp$cmp56092 = icmp eq i64 %truthy$cmp56092, 1
br i1 %cmp$cmp56092, label %truebranch$cmp56092, label %falsebranch$cmp56092
truebranch$cmp56092:
%ae50702 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50703 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist55210$k473890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56093 = alloca %struct.ScmObj*, align 8
%argslist55210$k473891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50703, %struct.ScmObj* %argslist55210$k473890)
store volatile %struct.ScmObj* %argslist55210$k473891, %struct.ScmObj** %stackaddr$prim56093, align 8
%stackaddr$prim56094 = alloca %struct.ScmObj*, align 8
%argslist55210$k473892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50702, %struct.ScmObj* %argslist55210$k473891)
store volatile %struct.ScmObj* %argslist55210$k473892, %struct.ScmObj** %stackaddr$prim56094, align 8
%clofunc56095 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47389)
musttail call tailcc void %clofunc56095(%struct.ScmObj* %k47389, %struct.ScmObj* %argslist55210$k473892)
ret void
falsebranch$cmp56092:
%stackaddr$prim56096 = alloca %struct.ScmObj*, align 8
%anf_45bind47310 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47141)
store volatile %struct.ScmObj* %anf_45bind47310, %struct.ScmObj** %stackaddr$prim56096, align 8
%stackaddr$prim56097 = alloca %struct.ScmObj*, align 8
%anf_45bind47311 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47310)
store volatile %struct.ScmObj* %anf_45bind47311, %struct.ScmObj** %stackaddr$prim56097, align 8
%truthy$cmp56098 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47311)
%cmp$cmp56098 = icmp eq i64 %truthy$cmp56098, 1
br i1 %cmp$cmp56098, label %truebranch$cmp56098, label %falsebranch$cmp56098
truebranch$cmp56098:
%stackaddr$prim56099 = alloca %struct.ScmObj*, align 8
%cpsprim47390 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47141)
store volatile %struct.ScmObj* %cpsprim47390, %struct.ScmObj** %stackaddr$prim56099, align 8
%ae50715 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55211$k473890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56100 = alloca %struct.ScmObj*, align 8
%argslist55211$k473891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47390, %struct.ScmObj* %argslist55211$k473890)
store volatile %struct.ScmObj* %argslist55211$k473891, %struct.ScmObj** %stackaddr$prim56100, align 8
%stackaddr$prim56101 = alloca %struct.ScmObj*, align 8
%argslist55211$k473892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50715, %struct.ScmObj* %argslist55211$k473891)
store volatile %struct.ScmObj* %argslist55211$k473892, %struct.ScmObj** %stackaddr$prim56101, align 8
%clofunc56102 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47389)
musttail call tailcc void %clofunc56102(%struct.ScmObj* %k47389, %struct.ScmObj* %argslist55211$k473892)
ret void
falsebranch$cmp56098:
%stackaddr$makeclosure56103 = alloca %struct.ScmObj*, align 8
%fptrToInt56104 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50720 to i64
%ae50720 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56104)
store volatile %struct.ScmObj* %ae50720, %struct.ScmObj** %stackaddr$makeclosure56103, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50720, %struct.ScmObj* %args47141, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50720, %struct.ScmObj* %k47389, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50720, %struct.ScmObj* %_37foldl147080, i64 2)
%ae50721 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56105 = alloca %struct.ScmObj*, align 8
%fptrToInt56106 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50722 to i64
%ae50722 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56106)
store volatile %struct.ScmObj* %ae50722, %struct.ScmObj** %stackaddr$makeclosure56105, align 8
%argslist55221$ae507200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56107 = alloca %struct.ScmObj*, align 8
%argslist55221$ae507201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50722, %struct.ScmObj* %argslist55221$ae507200)
store volatile %struct.ScmObj* %argslist55221$ae507201, %struct.ScmObj** %stackaddr$prim56107, align 8
%stackaddr$prim56108 = alloca %struct.ScmObj*, align 8
%argslist55221$ae507202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50721, %struct.ScmObj* %argslist55221$ae507201)
store volatile %struct.ScmObj* %argslist55221$ae507202, %struct.ScmObj** %stackaddr$prim56108, align 8
%clofunc56109 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50720)
musttail call tailcc void %clofunc56109(%struct.ScmObj* %ae50720, %struct.ScmObj* %argslist55221$ae507202)
ret void
}

define tailcc void @proc_clo$ae50720(%struct.ScmObj* %env$ae50720,%struct.ScmObj* %current_45args55212) {
%stackaddr$env-ref56110 = alloca %struct.ScmObj*, align 8
%args47141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50720, i64 0)
store %struct.ScmObj* %args47141, %struct.ScmObj** %stackaddr$env-ref56110
%stackaddr$env-ref56111 = alloca %struct.ScmObj*, align 8
%k47389 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50720, i64 1)
store %struct.ScmObj* %k47389, %struct.ScmObj** %stackaddr$env-ref56111
%stackaddr$env-ref56112 = alloca %struct.ScmObj*, align 8
%_37foldl147080 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50720, i64 2)
store %struct.ScmObj* %_37foldl147080, %struct.ScmObj** %stackaddr$env-ref56112
%stackaddr$prim56113 = alloca %struct.ScmObj*, align 8
%_95k47391 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55212)
store volatile %struct.ScmObj* %_95k47391, %struct.ScmObj** %stackaddr$prim56113, align 8
%stackaddr$prim56114 = alloca %struct.ScmObj*, align 8
%current_45args55213 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55212)
store volatile %struct.ScmObj* %current_45args55213, %struct.ScmObj** %stackaddr$prim56114, align 8
%stackaddr$prim56115 = alloca %struct.ScmObj*, align 8
%anf_45bind47312 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55213)
store volatile %struct.ScmObj* %anf_45bind47312, %struct.ScmObj** %stackaddr$prim56115, align 8
%stackaddr$prim56116 = alloca %struct.ScmObj*, align 8
%anf_45bind47313 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47141)
store volatile %struct.ScmObj* %anf_45bind47313, %struct.ScmObj** %stackaddr$prim56116, align 8
%stackaddr$prim56117 = alloca %struct.ScmObj*, align 8
%anf_45bind47314 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47141)
store volatile %struct.ScmObj* %anf_45bind47314, %struct.ScmObj** %stackaddr$prim56117, align 8
%argslist55215$_37foldl1470800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56118 = alloca %struct.ScmObj*, align 8
%argslist55215$_37foldl1470801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47314, %struct.ScmObj* %argslist55215$_37foldl1470800)
store volatile %struct.ScmObj* %argslist55215$_37foldl1470801, %struct.ScmObj** %stackaddr$prim56118, align 8
%stackaddr$prim56119 = alloca %struct.ScmObj*, align 8
%argslist55215$_37foldl1470802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47313, %struct.ScmObj* %argslist55215$_37foldl1470801)
store volatile %struct.ScmObj* %argslist55215$_37foldl1470802, %struct.ScmObj** %stackaddr$prim56119, align 8
%stackaddr$prim56120 = alloca %struct.ScmObj*, align 8
%argslist55215$_37foldl1470803 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47312, %struct.ScmObj* %argslist55215$_37foldl1470802)
store volatile %struct.ScmObj* %argslist55215$_37foldl1470803, %struct.ScmObj** %stackaddr$prim56120, align 8
%stackaddr$prim56121 = alloca %struct.ScmObj*, align 8
%argslist55215$_37foldl1470804 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47389, %struct.ScmObj* %argslist55215$_37foldl1470803)
store volatile %struct.ScmObj* %argslist55215$_37foldl1470804, %struct.ScmObj** %stackaddr$prim56121, align 8
%clofunc56122 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147080)
musttail call tailcc void %clofunc56122(%struct.ScmObj* %_37foldl147080, %struct.ScmObj* %argslist55215$_37foldl1470804)
ret void
}

define tailcc void @proc_clo$ae50722(%struct.ScmObj* %env$ae50722,%struct.ScmObj* %current_45args55216) {
%stackaddr$prim56123 = alloca %struct.ScmObj*, align 8
%k47392 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55216)
store volatile %struct.ScmObj* %k47392, %struct.ScmObj** %stackaddr$prim56123, align 8
%stackaddr$prim56124 = alloca %struct.ScmObj*, align 8
%current_45args55217 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55216)
store volatile %struct.ScmObj* %current_45args55217, %struct.ScmObj** %stackaddr$prim56124, align 8
%stackaddr$prim56125 = alloca %struct.ScmObj*, align 8
%n47143 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55217)
store volatile %struct.ScmObj* %n47143, %struct.ScmObj** %stackaddr$prim56125, align 8
%stackaddr$prim56126 = alloca %struct.ScmObj*, align 8
%current_45args55218 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55217)
store volatile %struct.ScmObj* %current_45args55218, %struct.ScmObj** %stackaddr$prim56126, align 8
%stackaddr$prim56127 = alloca %struct.ScmObj*, align 8
%v47142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55218)
store volatile %struct.ScmObj* %v47142, %struct.ScmObj** %stackaddr$prim56127, align 8
%stackaddr$prim56128 = alloca %struct.ScmObj*, align 8
%cpsprim47393 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v47142, %struct.ScmObj* %n47143)
store volatile %struct.ScmObj* %cpsprim47393, %struct.ScmObj** %stackaddr$prim56128, align 8
%ae50726 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55220$k473920 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56129 = alloca %struct.ScmObj*, align 8
%argslist55220$k473921 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47393, %struct.ScmObj* %argslist55220$k473920)
store volatile %struct.ScmObj* %argslist55220$k473921, %struct.ScmObj** %stackaddr$prim56129, align 8
%stackaddr$prim56130 = alloca %struct.ScmObj*, align 8
%argslist55220$k473922 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50726, %struct.ScmObj* %argslist55220$k473921)
store volatile %struct.ScmObj* %argslist55220$k473922, %struct.ScmObj** %stackaddr$prim56130, align 8
%clofunc56131 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47392)
musttail call tailcc void %clofunc56131(%struct.ScmObj* %k47392, %struct.ScmObj* %argslist55220$k473922)
ret void
}

define tailcc void @proc_clo$ae50292(%struct.ScmObj* %env$ae50292,%struct.ScmObj* %current_45args55223) {
%stackaddr$prim56132 = alloca %struct.ScmObj*, align 8
%k47394 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55223)
store volatile %struct.ScmObj* %k47394, %struct.ScmObj** %stackaddr$prim56132, align 8
%stackaddr$prim56133 = alloca %struct.ScmObj*, align 8
%current_45args55224 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55223)
store volatile %struct.ScmObj* %current_45args55224, %struct.ScmObj** %stackaddr$prim56133, align 8
%stackaddr$prim56134 = alloca %struct.ScmObj*, align 8
%v47146 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55224)
store volatile %struct.ScmObj* %v47146, %struct.ScmObj** %stackaddr$prim56134, align 8
%stackaddr$prim56135 = alloca %struct.ScmObj*, align 8
%current_45args55225 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55224)
store volatile %struct.ScmObj* %current_45args55225, %struct.ScmObj** %stackaddr$prim56135, align 8
%stackaddr$prim56136 = alloca %struct.ScmObj*, align 8
%lst47145 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55225)
store volatile %struct.ScmObj* %lst47145, %struct.ScmObj** %stackaddr$prim56136, align 8
%ae50293 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56137 = alloca %struct.ScmObj*, align 8
%lst47147 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50293, %struct.ScmObj* %lst47145)
store volatile %struct.ScmObj* %lst47147, %struct.ScmObj** %stackaddr$prim56137, align 8
%stackaddr$makeclosure56138 = alloca %struct.ScmObj*, align 8
%fptrToInt56139 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50295 to i64
%ae50295 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56139)
store volatile %struct.ScmObj* %ae50295, %struct.ScmObj** %stackaddr$makeclosure56138, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50295, %struct.ScmObj* %k47394, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50295, %struct.ScmObj* %lst47147, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50295, %struct.ScmObj* %v47146, i64 2)
%ae50296 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56140 = alloca %struct.ScmObj*, align 8
%fptrToInt56141 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50297 to i64
%ae50297 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56141)
store volatile %struct.ScmObj* %ae50297, %struct.ScmObj** %stackaddr$makeclosure56140, align 8
%argslist55247$ae502950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56142 = alloca %struct.ScmObj*, align 8
%argslist55247$ae502951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50297, %struct.ScmObj* %argslist55247$ae502950)
store volatile %struct.ScmObj* %argslist55247$ae502951, %struct.ScmObj** %stackaddr$prim56142, align 8
%stackaddr$prim56143 = alloca %struct.ScmObj*, align 8
%argslist55247$ae502952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50296, %struct.ScmObj* %argslist55247$ae502951)
store volatile %struct.ScmObj* %argslist55247$ae502952, %struct.ScmObj** %stackaddr$prim56143, align 8
%clofunc56144 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50295)
musttail call tailcc void %clofunc56144(%struct.ScmObj* %ae50295, %struct.ScmObj* %argslist55247$ae502952)
ret void
}

define tailcc void @proc_clo$ae50295(%struct.ScmObj* %env$ae50295,%struct.ScmObj* %current_45args55227) {
%stackaddr$env-ref56145 = alloca %struct.ScmObj*, align 8
%k47394 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50295, i64 0)
store %struct.ScmObj* %k47394, %struct.ScmObj** %stackaddr$env-ref56145
%stackaddr$env-ref56146 = alloca %struct.ScmObj*, align 8
%lst47147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50295, i64 1)
store %struct.ScmObj* %lst47147, %struct.ScmObj** %stackaddr$env-ref56146
%stackaddr$env-ref56147 = alloca %struct.ScmObj*, align 8
%v47146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50295, i64 2)
store %struct.ScmObj* %v47146, %struct.ScmObj** %stackaddr$env-ref56147
%stackaddr$prim56148 = alloca %struct.ScmObj*, align 8
%_95k47395 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55227)
store volatile %struct.ScmObj* %_95k47395, %struct.ScmObj** %stackaddr$prim56148, align 8
%stackaddr$prim56149 = alloca %struct.ScmObj*, align 8
%current_45args55228 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55227)
store volatile %struct.ScmObj* %current_45args55228, %struct.ScmObj** %stackaddr$prim56149, align 8
%stackaddr$prim56150 = alloca %struct.ScmObj*, align 8
%anf_45bind47301 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55228)
store volatile %struct.ScmObj* %anf_45bind47301, %struct.ScmObj** %stackaddr$prim56150, align 8
%stackaddr$makeclosure56151 = alloca %struct.ScmObj*, align 8
%fptrToInt56152 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50311 to i64
%ae50311 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56152)
store volatile %struct.ScmObj* %ae50311, %struct.ScmObj** %stackaddr$makeclosure56151, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50311, %struct.ScmObj* %k47394, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50311, %struct.ScmObj* %lst47147, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50311, %struct.ScmObj* %v47146, i64 2)
%stackaddr$makeclosure56153 = alloca %struct.ScmObj*, align 8
%fptrToInt56154 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50312 to i64
%ae50312 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56154)
store volatile %struct.ScmObj* %ae50312, %struct.ScmObj** %stackaddr$makeclosure56153, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50312, %struct.ScmObj* %k47394, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50312, %struct.ScmObj* %lst47147, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50312, %struct.ScmObj* %v47146, i64 2)
%argslist55242$anf_45bind473010 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56155 = alloca %struct.ScmObj*, align 8
%argslist55242$anf_45bind473011 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50312, %struct.ScmObj* %argslist55242$anf_45bind473010)
store volatile %struct.ScmObj* %argslist55242$anf_45bind473011, %struct.ScmObj** %stackaddr$prim56155, align 8
%stackaddr$prim56156 = alloca %struct.ScmObj*, align 8
%argslist55242$anf_45bind473012 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50311, %struct.ScmObj* %argslist55242$anf_45bind473011)
store volatile %struct.ScmObj* %argslist55242$anf_45bind473012, %struct.ScmObj** %stackaddr$prim56156, align 8
%clofunc56157 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47301)
musttail call tailcc void %clofunc56157(%struct.ScmObj* %anf_45bind47301, %struct.ScmObj* %argslist55242$anf_45bind473012)
ret void
}

define tailcc void @proc_clo$ae50311(%struct.ScmObj* %env$ae50311,%struct.ScmObj* %current_45args55230) {
%stackaddr$env-ref56158 = alloca %struct.ScmObj*, align 8
%k47394 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50311, i64 0)
store %struct.ScmObj* %k47394, %struct.ScmObj** %stackaddr$env-ref56158
%stackaddr$env-ref56159 = alloca %struct.ScmObj*, align 8
%lst47147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50311, i64 1)
store %struct.ScmObj* %lst47147, %struct.ScmObj** %stackaddr$env-ref56159
%stackaddr$env-ref56160 = alloca %struct.ScmObj*, align 8
%v47146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50311, i64 2)
store %struct.ScmObj* %v47146, %struct.ScmObj** %stackaddr$env-ref56160
%stackaddr$prim56161 = alloca %struct.ScmObj*, align 8
%_95k47396 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55230)
store volatile %struct.ScmObj* %_95k47396, %struct.ScmObj** %stackaddr$prim56161, align 8
%stackaddr$prim56162 = alloca %struct.ScmObj*, align 8
%current_45args55231 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55230)
store volatile %struct.ScmObj* %current_45args55231, %struct.ScmObj** %stackaddr$prim56162, align 8
%stackaddr$prim56163 = alloca %struct.ScmObj*, align 8
%cc47148 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55231)
store volatile %struct.ScmObj* %cc47148, %struct.ScmObj** %stackaddr$prim56163, align 8
%ae50420 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56164 = alloca %struct.ScmObj*, align 8
%anf_45bind47302 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47147, %struct.ScmObj* %ae50420)
store volatile %struct.ScmObj* %anf_45bind47302, %struct.ScmObj** %stackaddr$prim56164, align 8
%stackaddr$prim56165 = alloca %struct.ScmObj*, align 8
%anf_45bind47303 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47302)
store volatile %struct.ScmObj* %anf_45bind47303, %struct.ScmObj** %stackaddr$prim56165, align 8
%truthy$cmp56166 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47303)
%cmp$cmp56166 = icmp eq i64 %truthy$cmp56166, 1
br i1 %cmp$cmp56166, label %truebranch$cmp56166, label %falsebranch$cmp56166
truebranch$cmp56166:
%ae50424 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50425 = call %struct.ScmObj* @const_init_false()
%argslist55233$k473940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56167 = alloca %struct.ScmObj*, align 8
%argslist55233$k473941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50425, %struct.ScmObj* %argslist55233$k473940)
store volatile %struct.ScmObj* %argslist55233$k473941, %struct.ScmObj** %stackaddr$prim56167, align 8
%stackaddr$prim56168 = alloca %struct.ScmObj*, align 8
%argslist55233$k473942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50424, %struct.ScmObj* %argslist55233$k473941)
store volatile %struct.ScmObj* %argslist55233$k473942, %struct.ScmObj** %stackaddr$prim56168, align 8
%clofunc56169 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47394)
musttail call tailcc void %clofunc56169(%struct.ScmObj* %k47394, %struct.ScmObj* %argslist55233$k473942)
ret void
falsebranch$cmp56166:
%ae50433 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56170 = alloca %struct.ScmObj*, align 8
%anf_45bind47304 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47147, %struct.ScmObj* %ae50433)
store volatile %struct.ScmObj* %anf_45bind47304, %struct.ScmObj** %stackaddr$prim56170, align 8
%stackaddr$prim56171 = alloca %struct.ScmObj*, align 8
%anf_45bind47305 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47304)
store volatile %struct.ScmObj* %anf_45bind47305, %struct.ScmObj** %stackaddr$prim56171, align 8
%stackaddr$prim56172 = alloca %struct.ScmObj*, align 8
%anf_45bind47306 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind47305, %struct.ScmObj* %v47146)
store volatile %struct.ScmObj* %anf_45bind47306, %struct.ScmObj** %stackaddr$prim56172, align 8
%truthy$cmp56173 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47306)
%cmp$cmp56173 = icmp eq i64 %truthy$cmp56173, 1
br i1 %cmp$cmp56173, label %truebranch$cmp56173, label %falsebranch$cmp56173
truebranch$cmp56173:
%ae50439 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56174 = alloca %struct.ScmObj*, align 8
%cpsprim47397 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47147, %struct.ScmObj* %ae50439)
store volatile %struct.ScmObj* %cpsprim47397, %struct.ScmObj** %stackaddr$prim56174, align 8
%ae50441 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55234$k473940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56175 = alloca %struct.ScmObj*, align 8
%argslist55234$k473941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47397, %struct.ScmObj* %argslist55234$k473940)
store volatile %struct.ScmObj* %argslist55234$k473941, %struct.ScmObj** %stackaddr$prim56175, align 8
%stackaddr$prim56176 = alloca %struct.ScmObj*, align 8
%argslist55234$k473942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50441, %struct.ScmObj* %argslist55234$k473941)
store volatile %struct.ScmObj* %argslist55234$k473942, %struct.ScmObj** %stackaddr$prim56176, align 8
%clofunc56177 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47394)
musttail call tailcc void %clofunc56177(%struct.ScmObj* %k47394, %struct.ScmObj* %argslist55234$k473942)
ret void
falsebranch$cmp56173:
%ae50452 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56178 = alloca %struct.ScmObj*, align 8
%anf_45bind47307 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47147, %struct.ScmObj* %ae50452)
store volatile %struct.ScmObj* %anf_45bind47307, %struct.ScmObj** %stackaddr$prim56178, align 8
%stackaddr$prim56179 = alloca %struct.ScmObj*, align 8
%anf_45bind47308 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47307)
store volatile %struct.ScmObj* %anf_45bind47308, %struct.ScmObj** %stackaddr$prim56179, align 8
%ae50455 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56180 = alloca %struct.ScmObj*, align 8
%_95047150 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47147, %struct.ScmObj* %ae50455, %struct.ScmObj* %anf_45bind47308)
store volatile %struct.ScmObj* %_95047150, %struct.ScmObj** %stackaddr$prim56180, align 8
%argslist55235$cc471480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56181 = alloca %struct.ScmObj*, align 8
%argslist55235$cc471481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47148, %struct.ScmObj* %argslist55235$cc471480)
store volatile %struct.ScmObj* %argslist55235$cc471481, %struct.ScmObj** %stackaddr$prim56181, align 8
%stackaddr$prim56182 = alloca %struct.ScmObj*, align 8
%argslist55235$cc471482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47394, %struct.ScmObj* %argslist55235$cc471481)
store volatile %struct.ScmObj* %argslist55235$cc471482, %struct.ScmObj** %stackaddr$prim56182, align 8
%clofunc56183 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47148)
musttail call tailcc void %clofunc56183(%struct.ScmObj* %cc47148, %struct.ScmObj* %argslist55235$cc471482)
ret void
}

define tailcc void @proc_clo$ae50312(%struct.ScmObj* %env$ae50312,%struct.ScmObj* %current_45args55236) {
%stackaddr$env-ref56184 = alloca %struct.ScmObj*, align 8
%k47394 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50312, i64 0)
store %struct.ScmObj* %k47394, %struct.ScmObj** %stackaddr$env-ref56184
%stackaddr$env-ref56185 = alloca %struct.ScmObj*, align 8
%lst47147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50312, i64 1)
store %struct.ScmObj* %lst47147, %struct.ScmObj** %stackaddr$env-ref56185
%stackaddr$env-ref56186 = alloca %struct.ScmObj*, align 8
%v47146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50312, i64 2)
store %struct.ScmObj* %v47146, %struct.ScmObj** %stackaddr$env-ref56186
%stackaddr$prim56187 = alloca %struct.ScmObj*, align 8
%_95k47396 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55236)
store volatile %struct.ScmObj* %_95k47396, %struct.ScmObj** %stackaddr$prim56187, align 8
%stackaddr$prim56188 = alloca %struct.ScmObj*, align 8
%current_45args55237 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55236)
store volatile %struct.ScmObj* %current_45args55237, %struct.ScmObj** %stackaddr$prim56188, align 8
%stackaddr$prim56189 = alloca %struct.ScmObj*, align 8
%cc47148 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55237)
store volatile %struct.ScmObj* %cc47148, %struct.ScmObj** %stackaddr$prim56189, align 8
%ae50314 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56190 = alloca %struct.ScmObj*, align 8
%anf_45bind47302 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47147, %struct.ScmObj* %ae50314)
store volatile %struct.ScmObj* %anf_45bind47302, %struct.ScmObj** %stackaddr$prim56190, align 8
%stackaddr$prim56191 = alloca %struct.ScmObj*, align 8
%anf_45bind47303 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47302)
store volatile %struct.ScmObj* %anf_45bind47303, %struct.ScmObj** %stackaddr$prim56191, align 8
%truthy$cmp56192 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47303)
%cmp$cmp56192 = icmp eq i64 %truthy$cmp56192, 1
br i1 %cmp$cmp56192, label %truebranch$cmp56192, label %falsebranch$cmp56192
truebranch$cmp56192:
%ae50318 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50319 = call %struct.ScmObj* @const_init_false()
%argslist55239$k473940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56193 = alloca %struct.ScmObj*, align 8
%argslist55239$k473941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50319, %struct.ScmObj* %argslist55239$k473940)
store volatile %struct.ScmObj* %argslist55239$k473941, %struct.ScmObj** %stackaddr$prim56193, align 8
%stackaddr$prim56194 = alloca %struct.ScmObj*, align 8
%argslist55239$k473942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50318, %struct.ScmObj* %argslist55239$k473941)
store volatile %struct.ScmObj* %argslist55239$k473942, %struct.ScmObj** %stackaddr$prim56194, align 8
%clofunc56195 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47394)
musttail call tailcc void %clofunc56195(%struct.ScmObj* %k47394, %struct.ScmObj* %argslist55239$k473942)
ret void
falsebranch$cmp56192:
%ae50327 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56196 = alloca %struct.ScmObj*, align 8
%anf_45bind47304 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47147, %struct.ScmObj* %ae50327)
store volatile %struct.ScmObj* %anf_45bind47304, %struct.ScmObj** %stackaddr$prim56196, align 8
%stackaddr$prim56197 = alloca %struct.ScmObj*, align 8
%anf_45bind47305 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47304)
store volatile %struct.ScmObj* %anf_45bind47305, %struct.ScmObj** %stackaddr$prim56197, align 8
%stackaddr$prim56198 = alloca %struct.ScmObj*, align 8
%anf_45bind47306 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind47305, %struct.ScmObj* %v47146)
store volatile %struct.ScmObj* %anf_45bind47306, %struct.ScmObj** %stackaddr$prim56198, align 8
%truthy$cmp56199 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47306)
%cmp$cmp56199 = icmp eq i64 %truthy$cmp56199, 1
br i1 %cmp$cmp56199, label %truebranch$cmp56199, label %falsebranch$cmp56199
truebranch$cmp56199:
%ae50333 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56200 = alloca %struct.ScmObj*, align 8
%cpsprim47397 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47147, %struct.ScmObj* %ae50333)
store volatile %struct.ScmObj* %cpsprim47397, %struct.ScmObj** %stackaddr$prim56200, align 8
%ae50335 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55240$k473940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56201 = alloca %struct.ScmObj*, align 8
%argslist55240$k473941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47397, %struct.ScmObj* %argslist55240$k473940)
store volatile %struct.ScmObj* %argslist55240$k473941, %struct.ScmObj** %stackaddr$prim56201, align 8
%stackaddr$prim56202 = alloca %struct.ScmObj*, align 8
%argslist55240$k473942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50335, %struct.ScmObj* %argslist55240$k473941)
store volatile %struct.ScmObj* %argslist55240$k473942, %struct.ScmObj** %stackaddr$prim56202, align 8
%clofunc56203 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47394)
musttail call tailcc void %clofunc56203(%struct.ScmObj* %k47394, %struct.ScmObj* %argslist55240$k473942)
ret void
falsebranch$cmp56199:
%ae50346 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56204 = alloca %struct.ScmObj*, align 8
%anf_45bind47307 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47147, %struct.ScmObj* %ae50346)
store volatile %struct.ScmObj* %anf_45bind47307, %struct.ScmObj** %stackaddr$prim56204, align 8
%stackaddr$prim56205 = alloca %struct.ScmObj*, align 8
%anf_45bind47308 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47307)
store volatile %struct.ScmObj* %anf_45bind47308, %struct.ScmObj** %stackaddr$prim56205, align 8
%ae50349 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56206 = alloca %struct.ScmObj*, align 8
%_95047150 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47147, %struct.ScmObj* %ae50349, %struct.ScmObj* %anf_45bind47308)
store volatile %struct.ScmObj* %_95047150, %struct.ScmObj** %stackaddr$prim56206, align 8
%argslist55241$cc471480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56207 = alloca %struct.ScmObj*, align 8
%argslist55241$cc471481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47148, %struct.ScmObj* %argslist55241$cc471480)
store volatile %struct.ScmObj* %argslist55241$cc471481, %struct.ScmObj** %stackaddr$prim56207, align 8
%stackaddr$prim56208 = alloca %struct.ScmObj*, align 8
%argslist55241$cc471482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47394, %struct.ScmObj* %argslist55241$cc471481)
store volatile %struct.ScmObj* %argslist55241$cc471482, %struct.ScmObj** %stackaddr$prim56208, align 8
%clofunc56209 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47148)
musttail call tailcc void %clofunc56209(%struct.ScmObj* %cc47148, %struct.ScmObj* %argslist55241$cc471482)
ret void
}

define tailcc void @proc_clo$ae50297(%struct.ScmObj* %env$ae50297,%struct.ScmObj* %current_45args55243) {
%stackaddr$prim56210 = alloca %struct.ScmObj*, align 8
%k47398 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55243)
store volatile %struct.ScmObj* %k47398, %struct.ScmObj** %stackaddr$prim56210, align 8
%stackaddr$prim56211 = alloca %struct.ScmObj*, align 8
%current_45args55244 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55243)
store volatile %struct.ScmObj* %current_45args55244, %struct.ScmObj** %stackaddr$prim56211, align 8
%stackaddr$prim56212 = alloca %struct.ScmObj*, align 8
%u47149 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55244)
store volatile %struct.ScmObj* %u47149, %struct.ScmObj** %stackaddr$prim56212, align 8
%argslist55246$u471490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56213 = alloca %struct.ScmObj*, align 8
%argslist55246$u471491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u47149, %struct.ScmObj* %argslist55246$u471490)
store volatile %struct.ScmObj* %argslist55246$u471491, %struct.ScmObj** %stackaddr$prim56213, align 8
%stackaddr$prim56214 = alloca %struct.ScmObj*, align 8
%argslist55246$u471492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47398, %struct.ScmObj* %argslist55246$u471491)
store volatile %struct.ScmObj* %argslist55246$u471492, %struct.ScmObj** %stackaddr$prim56214, align 8
%clofunc56215 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u47149)
musttail call tailcc void %clofunc56215(%struct.ScmObj* %u47149, %struct.ScmObj* %argslist55246$u471492)
ret void
}

define tailcc void @proc_clo$ae49756(%struct.ScmObj* %env$ae49756,%struct.ScmObj* %current_45args55249) {
%stackaddr$prim56216 = alloca %struct.ScmObj*, align 8
%k47399 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55249)
store volatile %struct.ScmObj* %k47399, %struct.ScmObj** %stackaddr$prim56216, align 8
%stackaddr$prim56217 = alloca %struct.ScmObj*, align 8
%current_45args55250 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55249)
store volatile %struct.ScmObj* %current_45args55250, %struct.ScmObj** %stackaddr$prim56217, align 8
%stackaddr$prim56218 = alloca %struct.ScmObj*, align 8
%lst47153 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55250)
store volatile %struct.ScmObj* %lst47153, %struct.ScmObj** %stackaddr$prim56218, align 8
%stackaddr$prim56219 = alloca %struct.ScmObj*, align 8
%current_45args55251 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55250)
store volatile %struct.ScmObj* %current_45args55251, %struct.ScmObj** %stackaddr$prim56219, align 8
%stackaddr$prim56220 = alloca %struct.ScmObj*, align 8
%n47152 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55251)
store volatile %struct.ScmObj* %n47152, %struct.ScmObj** %stackaddr$prim56220, align 8
%ae49757 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56221 = alloca %struct.ScmObj*, align 8
%n47155 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49757, %struct.ScmObj* %n47152)
store volatile %struct.ScmObj* %n47155, %struct.ScmObj** %stackaddr$prim56221, align 8
%ae49759 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56222 = alloca %struct.ScmObj*, align 8
%lst47154 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49759, %struct.ScmObj* %lst47153)
store volatile %struct.ScmObj* %lst47154, %struct.ScmObj** %stackaddr$prim56222, align 8
%stackaddr$makeclosure56223 = alloca %struct.ScmObj*, align 8
%fptrToInt56224 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49761 to i64
%ae49761 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56224)
store volatile %struct.ScmObj* %ae49761, %struct.ScmObj** %stackaddr$makeclosure56223, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49761, %struct.ScmObj* %n47155, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49761, %struct.ScmObj* %lst47154, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49761, %struct.ScmObj* %k47399, i64 2)
%ae49762 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56225 = alloca %struct.ScmObj*, align 8
%fptrToInt56226 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49763 to i64
%ae49763 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56226)
store volatile %struct.ScmObj* %ae49763, %struct.ScmObj** %stackaddr$makeclosure56225, align 8
%argslist55271$ae497610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56227 = alloca %struct.ScmObj*, align 8
%argslist55271$ae497611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49763, %struct.ScmObj* %argslist55271$ae497610)
store volatile %struct.ScmObj* %argslist55271$ae497611, %struct.ScmObj** %stackaddr$prim56227, align 8
%stackaddr$prim56228 = alloca %struct.ScmObj*, align 8
%argslist55271$ae497612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49762, %struct.ScmObj* %argslist55271$ae497611)
store volatile %struct.ScmObj* %argslist55271$ae497612, %struct.ScmObj** %stackaddr$prim56228, align 8
%clofunc56229 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49761)
musttail call tailcc void %clofunc56229(%struct.ScmObj* %ae49761, %struct.ScmObj* %argslist55271$ae497612)
ret void
}

define tailcc void @proc_clo$ae49761(%struct.ScmObj* %env$ae49761,%struct.ScmObj* %current_45args55253) {
%stackaddr$env-ref56230 = alloca %struct.ScmObj*, align 8
%n47155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49761, i64 0)
store %struct.ScmObj* %n47155, %struct.ScmObj** %stackaddr$env-ref56230
%stackaddr$env-ref56231 = alloca %struct.ScmObj*, align 8
%lst47154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49761, i64 1)
store %struct.ScmObj* %lst47154, %struct.ScmObj** %stackaddr$env-ref56231
%stackaddr$env-ref56232 = alloca %struct.ScmObj*, align 8
%k47399 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49761, i64 2)
store %struct.ScmObj* %k47399, %struct.ScmObj** %stackaddr$env-ref56232
%stackaddr$prim56233 = alloca %struct.ScmObj*, align 8
%_95k47400 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55253)
store volatile %struct.ScmObj* %_95k47400, %struct.ScmObj** %stackaddr$prim56233, align 8
%stackaddr$prim56234 = alloca %struct.ScmObj*, align 8
%current_45args55254 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55253)
store volatile %struct.ScmObj* %current_45args55254, %struct.ScmObj** %stackaddr$prim56234, align 8
%stackaddr$prim56235 = alloca %struct.ScmObj*, align 8
%anf_45bind47294 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55254)
store volatile %struct.ScmObj* %anf_45bind47294, %struct.ScmObj** %stackaddr$prim56235, align 8
%stackaddr$makeclosure56236 = alloca %struct.ScmObj*, align 8
%fptrToInt56237 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49777 to i64
%ae49777 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56237)
store volatile %struct.ScmObj* %ae49777, %struct.ScmObj** %stackaddr$makeclosure56236, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49777, %struct.ScmObj* %n47155, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49777, %struct.ScmObj* %lst47154, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49777, %struct.ScmObj* %k47399, i64 2)
%stackaddr$makeclosure56238 = alloca %struct.ScmObj*, align 8
%fptrToInt56239 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49778 to i64
%ae49778 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56239)
store volatile %struct.ScmObj* %ae49778, %struct.ScmObj** %stackaddr$makeclosure56238, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49778, %struct.ScmObj* %n47155, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49778, %struct.ScmObj* %lst47154, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49778, %struct.ScmObj* %k47399, i64 2)
%argslist55266$anf_45bind472940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56240 = alloca %struct.ScmObj*, align 8
%argslist55266$anf_45bind472941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49778, %struct.ScmObj* %argslist55266$anf_45bind472940)
store volatile %struct.ScmObj* %argslist55266$anf_45bind472941, %struct.ScmObj** %stackaddr$prim56240, align 8
%stackaddr$prim56241 = alloca %struct.ScmObj*, align 8
%argslist55266$anf_45bind472942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49777, %struct.ScmObj* %argslist55266$anf_45bind472941)
store volatile %struct.ScmObj* %argslist55266$anf_45bind472942, %struct.ScmObj** %stackaddr$prim56241, align 8
%clofunc56242 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47294)
musttail call tailcc void %clofunc56242(%struct.ScmObj* %anf_45bind47294, %struct.ScmObj* %argslist55266$anf_45bind472942)
ret void
}

define tailcc void @proc_clo$ae49777(%struct.ScmObj* %env$ae49777,%struct.ScmObj* %current_45args55256) {
%stackaddr$env-ref56243 = alloca %struct.ScmObj*, align 8
%n47155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49777, i64 0)
store %struct.ScmObj* %n47155, %struct.ScmObj** %stackaddr$env-ref56243
%stackaddr$env-ref56244 = alloca %struct.ScmObj*, align 8
%lst47154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49777, i64 1)
store %struct.ScmObj* %lst47154, %struct.ScmObj** %stackaddr$env-ref56244
%stackaddr$env-ref56245 = alloca %struct.ScmObj*, align 8
%k47399 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49777, i64 2)
store %struct.ScmObj* %k47399, %struct.ScmObj** %stackaddr$env-ref56245
%stackaddr$prim56246 = alloca %struct.ScmObj*, align 8
%_95k47401 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55256)
store volatile %struct.ScmObj* %_95k47401, %struct.ScmObj** %stackaddr$prim56246, align 8
%stackaddr$prim56247 = alloca %struct.ScmObj*, align 8
%current_45args55257 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55256)
store volatile %struct.ScmObj* %current_45args55257, %struct.ScmObj** %stackaddr$prim56247, align 8
%stackaddr$prim56248 = alloca %struct.ScmObj*, align 8
%cc47156 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55257)
store volatile %struct.ScmObj* %cc47156, %struct.ScmObj** %stackaddr$prim56248, align 8
%ae49920 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56249 = alloca %struct.ScmObj*, align 8
%anf_45bind47295 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47155, %struct.ScmObj* %ae49920)
store volatile %struct.ScmObj* %anf_45bind47295, %struct.ScmObj** %stackaddr$prim56249, align 8
%ae49921 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56250 = alloca %struct.ScmObj*, align 8
%anf_45bind47296 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae49921, %struct.ScmObj* %anf_45bind47295)
store volatile %struct.ScmObj* %anf_45bind47296, %struct.ScmObj** %stackaddr$prim56250, align 8
%truthy$cmp56251 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47296)
%cmp$cmp56251 = icmp eq i64 %truthy$cmp56251, 1
br i1 %cmp$cmp56251, label %truebranch$cmp56251, label %falsebranch$cmp56251
truebranch$cmp56251:
%ae49925 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56252 = alloca %struct.ScmObj*, align 8
%cpsprim47402 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47154, %struct.ScmObj* %ae49925)
store volatile %struct.ScmObj* %cpsprim47402, %struct.ScmObj** %stackaddr$prim56252, align 8
%ae49927 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55259$k473990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56253 = alloca %struct.ScmObj*, align 8
%argslist55259$k473991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47402, %struct.ScmObj* %argslist55259$k473990)
store volatile %struct.ScmObj* %argslist55259$k473991, %struct.ScmObj** %stackaddr$prim56253, align 8
%stackaddr$prim56254 = alloca %struct.ScmObj*, align 8
%argslist55259$k473992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49927, %struct.ScmObj* %argslist55259$k473991)
store volatile %struct.ScmObj* %argslist55259$k473992, %struct.ScmObj** %stackaddr$prim56254, align 8
%clofunc56255 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47399)
musttail call tailcc void %clofunc56255(%struct.ScmObj* %k47399, %struct.ScmObj* %argslist55259$k473992)
ret void
falsebranch$cmp56251:
%ae49938 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56256 = alloca %struct.ScmObj*, align 8
%anf_45bind47297 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47154, %struct.ScmObj* %ae49938)
store volatile %struct.ScmObj* %anf_45bind47297, %struct.ScmObj** %stackaddr$prim56256, align 8
%stackaddr$prim56257 = alloca %struct.ScmObj*, align 8
%anf_45bind47298 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47297)
store volatile %struct.ScmObj* %anf_45bind47298, %struct.ScmObj** %stackaddr$prim56257, align 8
%ae49941 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56258 = alloca %struct.ScmObj*, align 8
%_95047159 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47154, %struct.ScmObj* %ae49941, %struct.ScmObj* %anf_45bind47298)
store volatile %struct.ScmObj* %_95047159, %struct.ScmObj** %stackaddr$prim56258, align 8
%ae49944 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56259 = alloca %struct.ScmObj*, align 8
%anf_45bind47299 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47155, %struct.ScmObj* %ae49944)
store volatile %struct.ScmObj* %anf_45bind47299, %struct.ScmObj** %stackaddr$prim56259, align 8
%ae49946 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56260 = alloca %struct.ScmObj*, align 8
%anf_45bind47300 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind47299, %struct.ScmObj* %ae49946)
store volatile %struct.ScmObj* %anf_45bind47300, %struct.ScmObj** %stackaddr$prim56260, align 8
%ae49948 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56261 = alloca %struct.ScmObj*, align 8
%_95147158 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n47155, %struct.ScmObj* %ae49948, %struct.ScmObj* %anf_45bind47300)
store volatile %struct.ScmObj* %_95147158, %struct.ScmObj** %stackaddr$prim56261, align 8
%argslist55260$cc471560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56262 = alloca %struct.ScmObj*, align 8
%argslist55260$cc471561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47156, %struct.ScmObj* %argslist55260$cc471560)
store volatile %struct.ScmObj* %argslist55260$cc471561, %struct.ScmObj** %stackaddr$prim56262, align 8
%stackaddr$prim56263 = alloca %struct.ScmObj*, align 8
%argslist55260$cc471562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47399, %struct.ScmObj* %argslist55260$cc471561)
store volatile %struct.ScmObj* %argslist55260$cc471562, %struct.ScmObj** %stackaddr$prim56263, align 8
%clofunc56264 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47156)
musttail call tailcc void %clofunc56264(%struct.ScmObj* %cc47156, %struct.ScmObj* %argslist55260$cc471562)
ret void
}

define tailcc void @proc_clo$ae49778(%struct.ScmObj* %env$ae49778,%struct.ScmObj* %current_45args55261) {
%stackaddr$env-ref56265 = alloca %struct.ScmObj*, align 8
%n47155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49778, i64 0)
store %struct.ScmObj* %n47155, %struct.ScmObj** %stackaddr$env-ref56265
%stackaddr$env-ref56266 = alloca %struct.ScmObj*, align 8
%lst47154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49778, i64 1)
store %struct.ScmObj* %lst47154, %struct.ScmObj** %stackaddr$env-ref56266
%stackaddr$env-ref56267 = alloca %struct.ScmObj*, align 8
%k47399 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49778, i64 2)
store %struct.ScmObj* %k47399, %struct.ScmObj** %stackaddr$env-ref56267
%stackaddr$prim56268 = alloca %struct.ScmObj*, align 8
%_95k47401 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55261)
store volatile %struct.ScmObj* %_95k47401, %struct.ScmObj** %stackaddr$prim56268, align 8
%stackaddr$prim56269 = alloca %struct.ScmObj*, align 8
%current_45args55262 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55261)
store volatile %struct.ScmObj* %current_45args55262, %struct.ScmObj** %stackaddr$prim56269, align 8
%stackaddr$prim56270 = alloca %struct.ScmObj*, align 8
%cc47156 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55262)
store volatile %struct.ScmObj* %cc47156, %struct.ScmObj** %stackaddr$prim56270, align 8
%ae49780 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56271 = alloca %struct.ScmObj*, align 8
%anf_45bind47295 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47155, %struct.ScmObj* %ae49780)
store volatile %struct.ScmObj* %anf_45bind47295, %struct.ScmObj** %stackaddr$prim56271, align 8
%ae49781 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56272 = alloca %struct.ScmObj*, align 8
%anf_45bind47296 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae49781, %struct.ScmObj* %anf_45bind47295)
store volatile %struct.ScmObj* %anf_45bind47296, %struct.ScmObj** %stackaddr$prim56272, align 8
%truthy$cmp56273 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47296)
%cmp$cmp56273 = icmp eq i64 %truthy$cmp56273, 1
br i1 %cmp$cmp56273, label %truebranch$cmp56273, label %falsebranch$cmp56273
truebranch$cmp56273:
%ae49785 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56274 = alloca %struct.ScmObj*, align 8
%cpsprim47402 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47154, %struct.ScmObj* %ae49785)
store volatile %struct.ScmObj* %cpsprim47402, %struct.ScmObj** %stackaddr$prim56274, align 8
%ae49787 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55264$k473990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56275 = alloca %struct.ScmObj*, align 8
%argslist55264$k473991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47402, %struct.ScmObj* %argslist55264$k473990)
store volatile %struct.ScmObj* %argslist55264$k473991, %struct.ScmObj** %stackaddr$prim56275, align 8
%stackaddr$prim56276 = alloca %struct.ScmObj*, align 8
%argslist55264$k473992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49787, %struct.ScmObj* %argslist55264$k473991)
store volatile %struct.ScmObj* %argslist55264$k473992, %struct.ScmObj** %stackaddr$prim56276, align 8
%clofunc56277 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47399)
musttail call tailcc void %clofunc56277(%struct.ScmObj* %k47399, %struct.ScmObj* %argslist55264$k473992)
ret void
falsebranch$cmp56273:
%ae49798 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56278 = alloca %struct.ScmObj*, align 8
%anf_45bind47297 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47154, %struct.ScmObj* %ae49798)
store volatile %struct.ScmObj* %anf_45bind47297, %struct.ScmObj** %stackaddr$prim56278, align 8
%stackaddr$prim56279 = alloca %struct.ScmObj*, align 8
%anf_45bind47298 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47297)
store volatile %struct.ScmObj* %anf_45bind47298, %struct.ScmObj** %stackaddr$prim56279, align 8
%ae49801 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56280 = alloca %struct.ScmObj*, align 8
%_95047159 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47154, %struct.ScmObj* %ae49801, %struct.ScmObj* %anf_45bind47298)
store volatile %struct.ScmObj* %_95047159, %struct.ScmObj** %stackaddr$prim56280, align 8
%ae49804 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56281 = alloca %struct.ScmObj*, align 8
%anf_45bind47299 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47155, %struct.ScmObj* %ae49804)
store volatile %struct.ScmObj* %anf_45bind47299, %struct.ScmObj** %stackaddr$prim56281, align 8
%ae49806 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56282 = alloca %struct.ScmObj*, align 8
%anf_45bind47300 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind47299, %struct.ScmObj* %ae49806)
store volatile %struct.ScmObj* %anf_45bind47300, %struct.ScmObj** %stackaddr$prim56282, align 8
%ae49808 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56283 = alloca %struct.ScmObj*, align 8
%_95147158 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n47155, %struct.ScmObj* %ae49808, %struct.ScmObj* %anf_45bind47300)
store volatile %struct.ScmObj* %_95147158, %struct.ScmObj** %stackaddr$prim56283, align 8
%argslist55265$cc471560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56284 = alloca %struct.ScmObj*, align 8
%argslist55265$cc471561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47156, %struct.ScmObj* %argslist55265$cc471560)
store volatile %struct.ScmObj* %argslist55265$cc471561, %struct.ScmObj** %stackaddr$prim56284, align 8
%stackaddr$prim56285 = alloca %struct.ScmObj*, align 8
%argslist55265$cc471562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47399, %struct.ScmObj* %argslist55265$cc471561)
store volatile %struct.ScmObj* %argslist55265$cc471562, %struct.ScmObj** %stackaddr$prim56285, align 8
%clofunc56286 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47156)
musttail call tailcc void %clofunc56286(%struct.ScmObj* %cc47156, %struct.ScmObj* %argslist55265$cc471562)
ret void
}

define tailcc void @proc_clo$ae49763(%struct.ScmObj* %env$ae49763,%struct.ScmObj* %current_45args55267) {
%stackaddr$prim56287 = alloca %struct.ScmObj*, align 8
%k47403 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55267)
store volatile %struct.ScmObj* %k47403, %struct.ScmObj** %stackaddr$prim56287, align 8
%stackaddr$prim56288 = alloca %struct.ScmObj*, align 8
%current_45args55268 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55267)
store volatile %struct.ScmObj* %current_45args55268, %struct.ScmObj** %stackaddr$prim56288, align 8
%stackaddr$prim56289 = alloca %struct.ScmObj*, align 8
%u47157 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55268)
store volatile %struct.ScmObj* %u47157, %struct.ScmObj** %stackaddr$prim56289, align 8
%argslist55270$u471570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56290 = alloca %struct.ScmObj*, align 8
%argslist55270$u471571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u47157, %struct.ScmObj* %argslist55270$u471570)
store volatile %struct.ScmObj* %argslist55270$u471571, %struct.ScmObj** %stackaddr$prim56290, align 8
%stackaddr$prim56291 = alloca %struct.ScmObj*, align 8
%argslist55270$u471572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47403, %struct.ScmObj* %argslist55270$u471571)
store volatile %struct.ScmObj* %argslist55270$u471572, %struct.ScmObj** %stackaddr$prim56291, align 8
%clofunc56292 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u47157)
musttail call tailcc void %clofunc56292(%struct.ScmObj* %u47157, %struct.ScmObj* %argslist55270$u471572)
ret void
}

define tailcc void @proc_clo$ae49340(%struct.ScmObj* %env$ae49340,%struct.ScmObj* %current_45args55273) {
%stackaddr$prim56293 = alloca %struct.ScmObj*, align 8
%k47404 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55273)
store volatile %struct.ScmObj* %k47404, %struct.ScmObj** %stackaddr$prim56293, align 8
%stackaddr$prim56294 = alloca %struct.ScmObj*, align 8
%current_45args55274 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55273)
store volatile %struct.ScmObj* %current_45args55274, %struct.ScmObj** %stackaddr$prim56294, align 8
%stackaddr$prim56295 = alloca %struct.ScmObj*, align 8
%a47161 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55274)
store volatile %struct.ScmObj* %a47161, %struct.ScmObj** %stackaddr$prim56295, align 8
%ae49341 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56296 = alloca %struct.ScmObj*, align 8
%a47162 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49341, %struct.ScmObj* %a47161)
store volatile %struct.ScmObj* %a47162, %struct.ScmObj** %stackaddr$prim56296, align 8
%stackaddr$makeclosure56297 = alloca %struct.ScmObj*, align 8
%fptrToInt56298 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49343 to i64
%ae49343 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56298)
store volatile %struct.ScmObj* %ae49343, %struct.ScmObj** %stackaddr$makeclosure56297, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49343, %struct.ScmObj* %k47404, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49343, %struct.ScmObj* %a47162, i64 1)
%ae49344 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56299 = alloca %struct.ScmObj*, align 8
%fptrToInt56300 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49345 to i64
%ae49345 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56300)
store volatile %struct.ScmObj* %ae49345, %struct.ScmObj** %stackaddr$makeclosure56299, align 8
%argslist55296$ae493430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56301 = alloca %struct.ScmObj*, align 8
%argslist55296$ae493431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49345, %struct.ScmObj* %argslist55296$ae493430)
store volatile %struct.ScmObj* %argslist55296$ae493431, %struct.ScmObj** %stackaddr$prim56301, align 8
%stackaddr$prim56302 = alloca %struct.ScmObj*, align 8
%argslist55296$ae493432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49344, %struct.ScmObj* %argslist55296$ae493431)
store volatile %struct.ScmObj* %argslist55296$ae493432, %struct.ScmObj** %stackaddr$prim56302, align 8
%clofunc56303 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49343)
musttail call tailcc void %clofunc56303(%struct.ScmObj* %ae49343, %struct.ScmObj* %argslist55296$ae493432)
ret void
}

define tailcc void @proc_clo$ae49343(%struct.ScmObj* %env$ae49343,%struct.ScmObj* %current_45args55276) {
%stackaddr$env-ref56304 = alloca %struct.ScmObj*, align 8
%k47404 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49343, i64 0)
store %struct.ScmObj* %k47404, %struct.ScmObj** %stackaddr$env-ref56304
%stackaddr$env-ref56305 = alloca %struct.ScmObj*, align 8
%a47162 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49343, i64 1)
store %struct.ScmObj* %a47162, %struct.ScmObj** %stackaddr$env-ref56305
%stackaddr$prim56306 = alloca %struct.ScmObj*, align 8
%_95k47405 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55276)
store volatile %struct.ScmObj* %_95k47405, %struct.ScmObj** %stackaddr$prim56306, align 8
%stackaddr$prim56307 = alloca %struct.ScmObj*, align 8
%current_45args55277 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55276)
store volatile %struct.ScmObj* %current_45args55277, %struct.ScmObj** %stackaddr$prim56307, align 8
%stackaddr$prim56308 = alloca %struct.ScmObj*, align 8
%anf_45bind47286 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55277)
store volatile %struct.ScmObj* %anf_45bind47286, %struct.ScmObj** %stackaddr$prim56308, align 8
%stackaddr$makeclosure56309 = alloca %struct.ScmObj*, align 8
%fptrToInt56310 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49362 to i64
%ae49362 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56310)
store volatile %struct.ScmObj* %ae49362, %struct.ScmObj** %stackaddr$makeclosure56309, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49362, %struct.ScmObj* %k47404, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49362, %struct.ScmObj* %a47162, i64 1)
%stackaddr$makeclosure56311 = alloca %struct.ScmObj*, align 8
%fptrToInt56312 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49363 to i64
%ae49363 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56312)
store volatile %struct.ScmObj* %ae49363, %struct.ScmObj** %stackaddr$makeclosure56311, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49363, %struct.ScmObj* %k47404, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49363, %struct.ScmObj* %a47162, i64 1)
%argslist55291$anf_45bind472860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56313 = alloca %struct.ScmObj*, align 8
%argslist55291$anf_45bind472861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49363, %struct.ScmObj* %argslist55291$anf_45bind472860)
store volatile %struct.ScmObj* %argslist55291$anf_45bind472861, %struct.ScmObj** %stackaddr$prim56313, align 8
%stackaddr$prim56314 = alloca %struct.ScmObj*, align 8
%argslist55291$anf_45bind472862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49362, %struct.ScmObj* %argslist55291$anf_45bind472861)
store volatile %struct.ScmObj* %argslist55291$anf_45bind472862, %struct.ScmObj** %stackaddr$prim56314, align 8
%clofunc56315 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47286)
musttail call tailcc void %clofunc56315(%struct.ScmObj* %anf_45bind47286, %struct.ScmObj* %argslist55291$anf_45bind472862)
ret void
}

define tailcc void @proc_clo$ae49362(%struct.ScmObj* %env$ae49362,%struct.ScmObj* %current_45args55279) {
%stackaddr$env-ref56316 = alloca %struct.ScmObj*, align 8
%k47404 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49362, i64 0)
store %struct.ScmObj* %k47404, %struct.ScmObj** %stackaddr$env-ref56316
%stackaddr$env-ref56317 = alloca %struct.ScmObj*, align 8
%a47162 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49362, i64 1)
store %struct.ScmObj* %a47162, %struct.ScmObj** %stackaddr$env-ref56317
%stackaddr$prim56318 = alloca %struct.ScmObj*, align 8
%_95k47406 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55279)
store volatile %struct.ScmObj* %_95k47406, %struct.ScmObj** %stackaddr$prim56318, align 8
%stackaddr$prim56319 = alloca %struct.ScmObj*, align 8
%current_45args55280 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55279)
store volatile %struct.ScmObj* %current_45args55280, %struct.ScmObj** %stackaddr$prim56319, align 8
%stackaddr$prim56320 = alloca %struct.ScmObj*, align 8
%cc47163 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55280)
store volatile %struct.ScmObj* %cc47163, %struct.ScmObj** %stackaddr$prim56320, align 8
%ae49478 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56321 = alloca %struct.ScmObj*, align 8
%anf_45bind47287 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47162, %struct.ScmObj* %ae49478)
store volatile %struct.ScmObj* %anf_45bind47287, %struct.ScmObj** %stackaddr$prim56321, align 8
%stackaddr$prim56322 = alloca %struct.ScmObj*, align 8
%anf_45bind47288 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47287)
store volatile %struct.ScmObj* %anf_45bind47288, %struct.ScmObj** %stackaddr$prim56322, align 8
%truthy$cmp56323 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47288)
%cmp$cmp56323 = icmp eq i64 %truthy$cmp56323, 1
br i1 %cmp$cmp56323, label %truebranch$cmp56323, label %falsebranch$cmp56323
truebranch$cmp56323:
%ae49482 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49483 = call %struct.ScmObj* @const_init_true()
%argslist55282$k474040 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56324 = alloca %struct.ScmObj*, align 8
%argslist55282$k474041 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49483, %struct.ScmObj* %argslist55282$k474040)
store volatile %struct.ScmObj* %argslist55282$k474041, %struct.ScmObj** %stackaddr$prim56324, align 8
%stackaddr$prim56325 = alloca %struct.ScmObj*, align 8
%argslist55282$k474042 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49482, %struct.ScmObj* %argslist55282$k474041)
store volatile %struct.ScmObj* %argslist55282$k474042, %struct.ScmObj** %stackaddr$prim56325, align 8
%clofunc56326 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47404)
musttail call tailcc void %clofunc56326(%struct.ScmObj* %k47404, %struct.ScmObj* %argslist55282$k474042)
ret void
falsebranch$cmp56323:
%ae49491 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56327 = alloca %struct.ScmObj*, align 8
%anf_45bind47289 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47162, %struct.ScmObj* %ae49491)
store volatile %struct.ScmObj* %anf_45bind47289, %struct.ScmObj** %stackaddr$prim56327, align 8
%stackaddr$prim56328 = alloca %struct.ScmObj*, align 8
%anf_45bind47290 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind47289)
store volatile %struct.ScmObj* %anf_45bind47290, %struct.ScmObj** %stackaddr$prim56328, align 8
%truthy$cmp56329 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47290)
%cmp$cmp56329 = icmp eq i64 %truthy$cmp56329, 1
br i1 %cmp$cmp56329, label %truebranch$cmp56329, label %falsebranch$cmp56329
truebranch$cmp56329:
%ae49495 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56330 = alloca %struct.ScmObj*, align 8
%anf_45bind47291 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47162, %struct.ScmObj* %ae49495)
store volatile %struct.ScmObj* %anf_45bind47291, %struct.ScmObj** %stackaddr$prim56330, align 8
%stackaddr$prim56331 = alloca %struct.ScmObj*, align 8
%b47165 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47291)
store volatile %struct.ScmObj* %b47165, %struct.ScmObj** %stackaddr$prim56331, align 8
%ae49498 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56332 = alloca %struct.ScmObj*, align 8
%anf_45bind47292 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47162, %struct.ScmObj* %ae49498)
store volatile %struct.ScmObj* %anf_45bind47292, %struct.ScmObj** %stackaddr$prim56332, align 8
%stackaddr$prim56333 = alloca %struct.ScmObj*, align 8
%anf_45bind47293 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47292)
store volatile %struct.ScmObj* %anf_45bind47293, %struct.ScmObj** %stackaddr$prim56333, align 8
%ae49501 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56334 = alloca %struct.ScmObj*, align 8
%_95047166 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a47162, %struct.ScmObj* %ae49501, %struct.ScmObj* %anf_45bind47293)
store volatile %struct.ScmObj* %_95047166, %struct.ScmObj** %stackaddr$prim56334, align 8
%argslist55283$cc471630 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56335 = alloca %struct.ScmObj*, align 8
%argslist55283$cc471631 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47163, %struct.ScmObj* %argslist55283$cc471630)
store volatile %struct.ScmObj* %argslist55283$cc471631, %struct.ScmObj** %stackaddr$prim56335, align 8
%stackaddr$prim56336 = alloca %struct.ScmObj*, align 8
%argslist55283$cc471632 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47404, %struct.ScmObj* %argslist55283$cc471631)
store volatile %struct.ScmObj* %argslist55283$cc471632, %struct.ScmObj** %stackaddr$prim56336, align 8
%clofunc56337 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47163)
musttail call tailcc void %clofunc56337(%struct.ScmObj* %cc47163, %struct.ScmObj* %argslist55283$cc471632)
ret void
falsebranch$cmp56329:
%ae49534 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49535 = call %struct.ScmObj* @const_init_false()
%argslist55284$k474040 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56338 = alloca %struct.ScmObj*, align 8
%argslist55284$k474041 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49535, %struct.ScmObj* %argslist55284$k474040)
store volatile %struct.ScmObj* %argslist55284$k474041, %struct.ScmObj** %stackaddr$prim56338, align 8
%stackaddr$prim56339 = alloca %struct.ScmObj*, align 8
%argslist55284$k474042 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49534, %struct.ScmObj* %argslist55284$k474041)
store volatile %struct.ScmObj* %argslist55284$k474042, %struct.ScmObj** %stackaddr$prim56339, align 8
%clofunc56340 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47404)
musttail call tailcc void %clofunc56340(%struct.ScmObj* %k47404, %struct.ScmObj* %argslist55284$k474042)
ret void
}

define tailcc void @proc_clo$ae49363(%struct.ScmObj* %env$ae49363,%struct.ScmObj* %current_45args55285) {
%stackaddr$env-ref56341 = alloca %struct.ScmObj*, align 8
%k47404 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49363, i64 0)
store %struct.ScmObj* %k47404, %struct.ScmObj** %stackaddr$env-ref56341
%stackaddr$env-ref56342 = alloca %struct.ScmObj*, align 8
%a47162 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49363, i64 1)
store %struct.ScmObj* %a47162, %struct.ScmObj** %stackaddr$env-ref56342
%stackaddr$prim56343 = alloca %struct.ScmObj*, align 8
%_95k47406 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55285)
store volatile %struct.ScmObj* %_95k47406, %struct.ScmObj** %stackaddr$prim56343, align 8
%stackaddr$prim56344 = alloca %struct.ScmObj*, align 8
%current_45args55286 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55285)
store volatile %struct.ScmObj* %current_45args55286, %struct.ScmObj** %stackaddr$prim56344, align 8
%stackaddr$prim56345 = alloca %struct.ScmObj*, align 8
%cc47163 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55286)
store volatile %struct.ScmObj* %cc47163, %struct.ScmObj** %stackaddr$prim56345, align 8
%ae49365 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56346 = alloca %struct.ScmObj*, align 8
%anf_45bind47287 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47162, %struct.ScmObj* %ae49365)
store volatile %struct.ScmObj* %anf_45bind47287, %struct.ScmObj** %stackaddr$prim56346, align 8
%stackaddr$prim56347 = alloca %struct.ScmObj*, align 8
%anf_45bind47288 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47287)
store volatile %struct.ScmObj* %anf_45bind47288, %struct.ScmObj** %stackaddr$prim56347, align 8
%truthy$cmp56348 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47288)
%cmp$cmp56348 = icmp eq i64 %truthy$cmp56348, 1
br i1 %cmp$cmp56348, label %truebranch$cmp56348, label %falsebranch$cmp56348
truebranch$cmp56348:
%ae49369 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49370 = call %struct.ScmObj* @const_init_true()
%argslist55288$k474040 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56349 = alloca %struct.ScmObj*, align 8
%argslist55288$k474041 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49370, %struct.ScmObj* %argslist55288$k474040)
store volatile %struct.ScmObj* %argslist55288$k474041, %struct.ScmObj** %stackaddr$prim56349, align 8
%stackaddr$prim56350 = alloca %struct.ScmObj*, align 8
%argslist55288$k474042 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49369, %struct.ScmObj* %argslist55288$k474041)
store volatile %struct.ScmObj* %argslist55288$k474042, %struct.ScmObj** %stackaddr$prim56350, align 8
%clofunc56351 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47404)
musttail call tailcc void %clofunc56351(%struct.ScmObj* %k47404, %struct.ScmObj* %argslist55288$k474042)
ret void
falsebranch$cmp56348:
%ae49378 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56352 = alloca %struct.ScmObj*, align 8
%anf_45bind47289 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47162, %struct.ScmObj* %ae49378)
store volatile %struct.ScmObj* %anf_45bind47289, %struct.ScmObj** %stackaddr$prim56352, align 8
%stackaddr$prim56353 = alloca %struct.ScmObj*, align 8
%anf_45bind47290 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind47289)
store volatile %struct.ScmObj* %anf_45bind47290, %struct.ScmObj** %stackaddr$prim56353, align 8
%truthy$cmp56354 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47290)
%cmp$cmp56354 = icmp eq i64 %truthy$cmp56354, 1
br i1 %cmp$cmp56354, label %truebranch$cmp56354, label %falsebranch$cmp56354
truebranch$cmp56354:
%ae49382 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56355 = alloca %struct.ScmObj*, align 8
%anf_45bind47291 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47162, %struct.ScmObj* %ae49382)
store volatile %struct.ScmObj* %anf_45bind47291, %struct.ScmObj** %stackaddr$prim56355, align 8
%stackaddr$prim56356 = alloca %struct.ScmObj*, align 8
%b47165 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47291)
store volatile %struct.ScmObj* %b47165, %struct.ScmObj** %stackaddr$prim56356, align 8
%ae49385 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56357 = alloca %struct.ScmObj*, align 8
%anf_45bind47292 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47162, %struct.ScmObj* %ae49385)
store volatile %struct.ScmObj* %anf_45bind47292, %struct.ScmObj** %stackaddr$prim56357, align 8
%stackaddr$prim56358 = alloca %struct.ScmObj*, align 8
%anf_45bind47293 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47292)
store volatile %struct.ScmObj* %anf_45bind47293, %struct.ScmObj** %stackaddr$prim56358, align 8
%ae49388 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56359 = alloca %struct.ScmObj*, align 8
%_95047166 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a47162, %struct.ScmObj* %ae49388, %struct.ScmObj* %anf_45bind47293)
store volatile %struct.ScmObj* %_95047166, %struct.ScmObj** %stackaddr$prim56359, align 8
%argslist55289$cc471630 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56360 = alloca %struct.ScmObj*, align 8
%argslist55289$cc471631 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47163, %struct.ScmObj* %argslist55289$cc471630)
store volatile %struct.ScmObj* %argslist55289$cc471631, %struct.ScmObj** %stackaddr$prim56360, align 8
%stackaddr$prim56361 = alloca %struct.ScmObj*, align 8
%argslist55289$cc471632 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47404, %struct.ScmObj* %argslist55289$cc471631)
store volatile %struct.ScmObj* %argslist55289$cc471632, %struct.ScmObj** %stackaddr$prim56361, align 8
%clofunc56362 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47163)
musttail call tailcc void %clofunc56362(%struct.ScmObj* %cc47163, %struct.ScmObj* %argslist55289$cc471632)
ret void
falsebranch$cmp56354:
%ae49421 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49422 = call %struct.ScmObj* @const_init_false()
%argslist55290$k474040 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56363 = alloca %struct.ScmObj*, align 8
%argslist55290$k474041 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49422, %struct.ScmObj* %argslist55290$k474040)
store volatile %struct.ScmObj* %argslist55290$k474041, %struct.ScmObj** %stackaddr$prim56363, align 8
%stackaddr$prim56364 = alloca %struct.ScmObj*, align 8
%argslist55290$k474042 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49421, %struct.ScmObj* %argslist55290$k474041)
store volatile %struct.ScmObj* %argslist55290$k474042, %struct.ScmObj** %stackaddr$prim56364, align 8
%clofunc56365 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47404)
musttail call tailcc void %clofunc56365(%struct.ScmObj* %k47404, %struct.ScmObj* %argslist55290$k474042)
ret void
}

define tailcc void @proc_clo$ae49345(%struct.ScmObj* %env$ae49345,%struct.ScmObj* %current_45args55292) {
%stackaddr$prim56366 = alloca %struct.ScmObj*, align 8
%k47407 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55292)
store volatile %struct.ScmObj* %k47407, %struct.ScmObj** %stackaddr$prim56366, align 8
%stackaddr$prim56367 = alloca %struct.ScmObj*, align 8
%current_45args55293 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55292)
store volatile %struct.ScmObj* %current_45args55293, %struct.ScmObj** %stackaddr$prim56367, align 8
%stackaddr$prim56368 = alloca %struct.ScmObj*, align 8
%k47164 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55293)
store volatile %struct.ScmObj* %k47164, %struct.ScmObj** %stackaddr$prim56368, align 8
%ae49347 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55295$k474070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56369 = alloca %struct.ScmObj*, align 8
%argslist55295$k474071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47164, %struct.ScmObj* %argslist55295$k474070)
store volatile %struct.ScmObj* %argslist55295$k474071, %struct.ScmObj** %stackaddr$prim56369, align 8
%stackaddr$prim56370 = alloca %struct.ScmObj*, align 8
%argslist55295$k474072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49347, %struct.ScmObj* %argslist55295$k474071)
store volatile %struct.ScmObj* %argslist55295$k474072, %struct.ScmObj** %stackaddr$prim56370, align 8
%clofunc56371 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47407)
musttail call tailcc void %clofunc56371(%struct.ScmObj* %k47407, %struct.ScmObj* %argslist55295$k474072)
ret void
}

define tailcc void @proc_clo$ae49268(%struct.ScmObj* %env$ae49268,%struct.ScmObj* %current_45args55298) {
%stackaddr$env-ref56372 = alloca %struct.ScmObj*, align 8
%_37append47168 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49268, i64 0)
store %struct.ScmObj* %_37append47168, %struct.ScmObj** %stackaddr$env-ref56372
%stackaddr$prim56373 = alloca %struct.ScmObj*, align 8
%k47408 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55298)
store volatile %struct.ScmObj* %k47408, %struct.ScmObj** %stackaddr$prim56373, align 8
%stackaddr$prim56374 = alloca %struct.ScmObj*, align 8
%current_45args55299 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55298)
store volatile %struct.ScmObj* %current_45args55299, %struct.ScmObj** %stackaddr$prim56374, align 8
%stackaddr$prim56375 = alloca %struct.ScmObj*, align 8
%ls047171 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55299)
store volatile %struct.ScmObj* %ls047171, %struct.ScmObj** %stackaddr$prim56375, align 8
%stackaddr$prim56376 = alloca %struct.ScmObj*, align 8
%current_45args55300 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55299)
store volatile %struct.ScmObj* %current_45args55300, %struct.ScmObj** %stackaddr$prim56376, align 8
%stackaddr$prim56377 = alloca %struct.ScmObj*, align 8
%ls147170 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55300)
store volatile %struct.ScmObj* %ls147170, %struct.ScmObj** %stackaddr$prim56377, align 8
%stackaddr$prim56378 = alloca %struct.ScmObj*, align 8
%anf_45bind47280 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls047171)
store volatile %struct.ScmObj* %anf_45bind47280, %struct.ScmObj** %stackaddr$prim56378, align 8
%truthy$cmp56379 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47280)
%cmp$cmp56379 = icmp eq i64 %truthy$cmp56379, 1
br i1 %cmp$cmp56379, label %truebranch$cmp56379, label %falsebranch$cmp56379
truebranch$cmp56379:
%ae49272 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55302$k474080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56380 = alloca %struct.ScmObj*, align 8
%argslist55302$k474081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls147170, %struct.ScmObj* %argslist55302$k474080)
store volatile %struct.ScmObj* %argslist55302$k474081, %struct.ScmObj** %stackaddr$prim56380, align 8
%stackaddr$prim56381 = alloca %struct.ScmObj*, align 8
%argslist55302$k474082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49272, %struct.ScmObj* %argslist55302$k474081)
store volatile %struct.ScmObj* %argslist55302$k474082, %struct.ScmObj** %stackaddr$prim56381, align 8
%clofunc56382 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47408)
musttail call tailcc void %clofunc56382(%struct.ScmObj* %k47408, %struct.ScmObj* %argslist55302$k474082)
ret void
falsebranch$cmp56379:
%stackaddr$prim56383 = alloca %struct.ScmObj*, align 8
%anf_45bind47281 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls047171)
store volatile %struct.ScmObj* %anf_45bind47281, %struct.ScmObj** %stackaddr$prim56383, align 8
%ae49279 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56384 = alloca %struct.ScmObj*, align 8
%anf_45bind47282 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append47168, %struct.ScmObj* %ae49279)
store volatile %struct.ScmObj* %anf_45bind47282, %struct.ScmObj** %stackaddr$prim56384, align 8
%stackaddr$prim56385 = alloca %struct.ScmObj*, align 8
%anf_45bind47283 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls047171)
store volatile %struct.ScmObj* %anf_45bind47283, %struct.ScmObj** %stackaddr$prim56385, align 8
%stackaddr$makeclosure56386 = alloca %struct.ScmObj*, align 8
%fptrToInt56387 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49282 to i64
%ae49282 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56387)
store volatile %struct.ScmObj* %ae49282, %struct.ScmObj** %stackaddr$makeclosure56386, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49282, %struct.ScmObj* %anf_45bind47281, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49282, %struct.ScmObj* %k47408, i64 1)
%argslist55307$anf_45bind472820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56388 = alloca %struct.ScmObj*, align 8
%argslist55307$anf_45bind472821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls147170, %struct.ScmObj* %argslist55307$anf_45bind472820)
store volatile %struct.ScmObj* %argslist55307$anf_45bind472821, %struct.ScmObj** %stackaddr$prim56388, align 8
%stackaddr$prim56389 = alloca %struct.ScmObj*, align 8
%argslist55307$anf_45bind472822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47283, %struct.ScmObj* %argslist55307$anf_45bind472821)
store volatile %struct.ScmObj* %argslist55307$anf_45bind472822, %struct.ScmObj** %stackaddr$prim56389, align 8
%stackaddr$prim56390 = alloca %struct.ScmObj*, align 8
%argslist55307$anf_45bind472823 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49282, %struct.ScmObj* %argslist55307$anf_45bind472822)
store volatile %struct.ScmObj* %argslist55307$anf_45bind472823, %struct.ScmObj** %stackaddr$prim56390, align 8
%clofunc56391 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47282)
musttail call tailcc void %clofunc56391(%struct.ScmObj* %anf_45bind47282, %struct.ScmObj* %argslist55307$anf_45bind472823)
ret void
}

define tailcc void @proc_clo$ae49282(%struct.ScmObj* %env$ae49282,%struct.ScmObj* %current_45args55303) {
%stackaddr$env-ref56392 = alloca %struct.ScmObj*, align 8
%anf_45bind47281 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49282, i64 0)
store %struct.ScmObj* %anf_45bind47281, %struct.ScmObj** %stackaddr$env-ref56392
%stackaddr$env-ref56393 = alloca %struct.ScmObj*, align 8
%k47408 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49282, i64 1)
store %struct.ScmObj* %k47408, %struct.ScmObj** %stackaddr$env-ref56393
%stackaddr$prim56394 = alloca %struct.ScmObj*, align 8
%_95k47409 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55303)
store volatile %struct.ScmObj* %_95k47409, %struct.ScmObj** %stackaddr$prim56394, align 8
%stackaddr$prim56395 = alloca %struct.ScmObj*, align 8
%current_45args55304 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55303)
store volatile %struct.ScmObj* %current_45args55304, %struct.ScmObj** %stackaddr$prim56395, align 8
%stackaddr$prim56396 = alloca %struct.ScmObj*, align 8
%anf_45bind47284 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55304)
store volatile %struct.ScmObj* %anf_45bind47284, %struct.ScmObj** %stackaddr$prim56396, align 8
%stackaddr$prim56397 = alloca %struct.ScmObj*, align 8
%cpsprim47410 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47281, %struct.ScmObj* %anf_45bind47284)
store volatile %struct.ScmObj* %cpsprim47410, %struct.ScmObj** %stackaddr$prim56397, align 8
%ae49288 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55306$k474080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56398 = alloca %struct.ScmObj*, align 8
%argslist55306$k474081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47410, %struct.ScmObj* %argslist55306$k474080)
store volatile %struct.ScmObj* %argslist55306$k474081, %struct.ScmObj** %stackaddr$prim56398, align 8
%stackaddr$prim56399 = alloca %struct.ScmObj*, align 8
%argslist55306$k474082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49288, %struct.ScmObj* %argslist55306$k474081)
store volatile %struct.ScmObj* %argslist55306$k474082, %struct.ScmObj** %stackaddr$prim56399, align 8
%clofunc56400 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47408)
musttail call tailcc void %clofunc56400(%struct.ScmObj* %k47408, %struct.ScmObj* %argslist55306$k474082)
ret void
}

define tailcc void @proc_clo$ae49242(%struct.ScmObj* %env$ae49242,%struct.ScmObj* %current_45args55309) {
%stackaddr$prim56401 = alloca %struct.ScmObj*, align 8
%k47411 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55309)
store volatile %struct.ScmObj* %k47411, %struct.ScmObj** %stackaddr$prim56401, align 8
%stackaddr$prim56402 = alloca %struct.ScmObj*, align 8
%current_45args55310 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55309)
store volatile %struct.ScmObj* %current_45args55310, %struct.ScmObj** %stackaddr$prim56402, align 8
%stackaddr$prim56403 = alloca %struct.ScmObj*, align 8
%a47174 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55310)
store volatile %struct.ScmObj* %a47174, %struct.ScmObj** %stackaddr$prim56403, align 8
%stackaddr$prim56404 = alloca %struct.ScmObj*, align 8
%current_45args55311 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55310)
store volatile %struct.ScmObj* %current_45args55311, %struct.ScmObj** %stackaddr$prim56404, align 8
%stackaddr$prim56405 = alloca %struct.ScmObj*, align 8
%b47173 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55311)
store volatile %struct.ScmObj* %b47173, %struct.ScmObj** %stackaddr$prim56405, align 8
%stackaddr$prim56406 = alloca %struct.ScmObj*, align 8
%anf_45bind47279 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a47174, %struct.ScmObj* %b47173)
store volatile %struct.ScmObj* %anf_45bind47279, %struct.ScmObj** %stackaddr$prim56406, align 8
%stackaddr$prim56407 = alloca %struct.ScmObj*, align 8
%cpsprim47412 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind47279)
store volatile %struct.ScmObj* %cpsprim47412, %struct.ScmObj** %stackaddr$prim56407, align 8
%ae49247 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55313$k474110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56408 = alloca %struct.ScmObj*, align 8
%argslist55313$k474111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47412, %struct.ScmObj* %argslist55313$k474110)
store volatile %struct.ScmObj* %argslist55313$k474111, %struct.ScmObj** %stackaddr$prim56408, align 8
%stackaddr$prim56409 = alloca %struct.ScmObj*, align 8
%argslist55313$k474112 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49247, %struct.ScmObj* %argslist55313$k474111)
store volatile %struct.ScmObj* %argslist55313$k474112, %struct.ScmObj** %stackaddr$prim56409, align 8
%clofunc56410 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47411)
musttail call tailcc void %clofunc56410(%struct.ScmObj* %k47411, %struct.ScmObj* %argslist55313$k474112)
ret void
}

define tailcc void @proc_clo$ae49218(%struct.ScmObj* %env$ae49218,%struct.ScmObj* %current_45args55315) {
%stackaddr$prim56411 = alloca %struct.ScmObj*, align 8
%k47413 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55315)
store volatile %struct.ScmObj* %k47413, %struct.ScmObj** %stackaddr$prim56411, align 8
%stackaddr$prim56412 = alloca %struct.ScmObj*, align 8
%current_45args55316 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55315)
store volatile %struct.ScmObj* %current_45args55316, %struct.ScmObj** %stackaddr$prim56412, align 8
%stackaddr$prim56413 = alloca %struct.ScmObj*, align 8
%a47177 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55316)
store volatile %struct.ScmObj* %a47177, %struct.ScmObj** %stackaddr$prim56413, align 8
%stackaddr$prim56414 = alloca %struct.ScmObj*, align 8
%current_45args55317 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55316)
store volatile %struct.ScmObj* %current_45args55317, %struct.ScmObj** %stackaddr$prim56414, align 8
%stackaddr$prim56415 = alloca %struct.ScmObj*, align 8
%b47176 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55317)
store volatile %struct.ScmObj* %b47176, %struct.ScmObj** %stackaddr$prim56415, align 8
%stackaddr$prim56416 = alloca %struct.ScmObj*, align 8
%anf_45bind47278 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a47177, %struct.ScmObj* %b47176)
store volatile %struct.ScmObj* %anf_45bind47278, %struct.ScmObj** %stackaddr$prim56416, align 8
%stackaddr$prim56417 = alloca %struct.ScmObj*, align 8
%cpsprim47414 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind47278)
store volatile %struct.ScmObj* %cpsprim47414, %struct.ScmObj** %stackaddr$prim56417, align 8
%ae49223 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55319$k474130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56418 = alloca %struct.ScmObj*, align 8
%argslist55319$k474131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47414, %struct.ScmObj* %argslist55319$k474130)
store volatile %struct.ScmObj* %argslist55319$k474131, %struct.ScmObj** %stackaddr$prim56418, align 8
%stackaddr$prim56419 = alloca %struct.ScmObj*, align 8
%argslist55319$k474132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49223, %struct.ScmObj* %argslist55319$k474131)
store volatile %struct.ScmObj* %argslist55319$k474132, %struct.ScmObj** %stackaddr$prim56419, align 8
%clofunc56420 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47413)
musttail call tailcc void %clofunc56420(%struct.ScmObj* %k47413, %struct.ScmObj* %argslist55319$k474132)
ret void
}

define tailcc void @proc_clo$ae48824(%struct.ScmObj* %env$ae48824,%struct.ScmObj* %current_45args55322) {
%stackaddr$env-ref56421 = alloca %struct.ScmObj*, align 8
%_37foldr47101 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48824, i64 0)
store %struct.ScmObj* %_37foldr47101, %struct.ScmObj** %stackaddr$env-ref56421
%stackaddr$env-ref56422 = alloca %struct.ScmObj*, align 8
%_37foldr147096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48824, i64 1)
store %struct.ScmObj* %_37foldr147096, %struct.ScmObj** %stackaddr$env-ref56422
%stackaddr$env-ref56423 = alloca %struct.ScmObj*, align 8
%_37map147127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48824, i64 2)
store %struct.ScmObj* %_37map147127, %struct.ScmObj** %stackaddr$env-ref56423
%stackaddr$prim56424 = alloca %struct.ScmObj*, align 8
%k47415 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55322)
store volatile %struct.ScmObj* %k47415, %struct.ScmObj** %stackaddr$prim56424, align 8
%stackaddr$prim56425 = alloca %struct.ScmObj*, align 8
%current_45args55323 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55322)
store volatile %struct.ScmObj* %current_45args55323, %struct.ScmObj** %stackaddr$prim56425, align 8
%stackaddr$prim56426 = alloca %struct.ScmObj*, align 8
%_37foldl47179 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55323)
store volatile %struct.ScmObj* %_37foldl47179, %struct.ScmObj** %stackaddr$prim56426, align 8
%ae48826 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56427 = alloca %struct.ScmObj*, align 8
%fptrToInt56428 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48827 to i64
%ae48827 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56428)
store volatile %struct.ScmObj* %ae48827, %struct.ScmObj** %stackaddr$makeclosure56427, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48827, %struct.ScmObj* %_37foldr47101, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48827, %struct.ScmObj* %_37foldl47179, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48827, %struct.ScmObj* %_37foldr147096, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48827, %struct.ScmObj* %_37map147127, i64 3)
%argslist55380$k474150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56429 = alloca %struct.ScmObj*, align 8
%argslist55380$k474151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48827, %struct.ScmObj* %argslist55380$k474150)
store volatile %struct.ScmObj* %argslist55380$k474151, %struct.ScmObj** %stackaddr$prim56429, align 8
%stackaddr$prim56430 = alloca %struct.ScmObj*, align 8
%argslist55380$k474152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48826, %struct.ScmObj* %argslist55380$k474151)
store volatile %struct.ScmObj* %argslist55380$k474152, %struct.ScmObj** %stackaddr$prim56430, align 8
%clofunc56431 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47415)
musttail call tailcc void %clofunc56431(%struct.ScmObj* %k47415, %struct.ScmObj* %argslist55380$k474152)
ret void
}

define tailcc void @proc_clo$ae48827(%struct.ScmObj* %env$ae48827,%struct.ScmObj* %args4718047416) {
%stackaddr$env-ref56432 = alloca %struct.ScmObj*, align 8
%_37foldr47101 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48827, i64 0)
store %struct.ScmObj* %_37foldr47101, %struct.ScmObj** %stackaddr$env-ref56432
%stackaddr$env-ref56433 = alloca %struct.ScmObj*, align 8
%_37foldl47179 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48827, i64 1)
store %struct.ScmObj* %_37foldl47179, %struct.ScmObj** %stackaddr$env-ref56433
%stackaddr$env-ref56434 = alloca %struct.ScmObj*, align 8
%_37foldr147096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48827, i64 2)
store %struct.ScmObj* %_37foldr147096, %struct.ScmObj** %stackaddr$env-ref56434
%stackaddr$env-ref56435 = alloca %struct.ScmObj*, align 8
%_37map147127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48827, i64 3)
store %struct.ScmObj* %_37map147127, %struct.ScmObj** %stackaddr$env-ref56435
%stackaddr$prim56436 = alloca %struct.ScmObj*, align 8
%k47417 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4718047416)
store volatile %struct.ScmObj* %k47417, %struct.ScmObj** %stackaddr$prim56436, align 8
%stackaddr$prim56437 = alloca %struct.ScmObj*, align 8
%args47180 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4718047416)
store volatile %struct.ScmObj* %args47180, %struct.ScmObj** %stackaddr$prim56437, align 8
%stackaddr$prim56438 = alloca %struct.ScmObj*, align 8
%f47183 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47180)
store volatile %struct.ScmObj* %f47183, %struct.ScmObj** %stackaddr$prim56438, align 8
%stackaddr$prim56439 = alloca %struct.ScmObj*, align 8
%anf_45bind47266 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47180)
store volatile %struct.ScmObj* %anf_45bind47266, %struct.ScmObj** %stackaddr$prim56439, align 8
%stackaddr$prim56440 = alloca %struct.ScmObj*, align 8
%acc47182 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47266)
store volatile %struct.ScmObj* %acc47182, %struct.ScmObj** %stackaddr$prim56440, align 8
%stackaddr$prim56441 = alloca %struct.ScmObj*, align 8
%anf_45bind47267 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47180)
store volatile %struct.ScmObj* %anf_45bind47267, %struct.ScmObj** %stackaddr$prim56441, align 8
%stackaddr$prim56442 = alloca %struct.ScmObj*, align 8
%lsts47181 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47267)
store volatile %struct.ScmObj* %lsts47181, %struct.ScmObj** %stackaddr$prim56442, align 8
%stackaddr$makeclosure56443 = alloca %struct.ScmObj*, align 8
%fptrToInt56444 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48835 to i64
%ae48835 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt56444)
store volatile %struct.ScmObj* %ae48835, %struct.ScmObj** %stackaddr$makeclosure56443, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48835, %struct.ScmObj* %lsts47181, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48835, %struct.ScmObj* %_37foldr47101, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48835, %struct.ScmObj* %f47183, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48835, %struct.ScmObj* %acc47182, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48835, %struct.ScmObj* %_37foldl47179, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48835, %struct.ScmObj* %k47417, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48835, %struct.ScmObj* %_37foldr147096, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae48835, %struct.ScmObj* %_37map147127, i64 7)
%ae48836 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56445 = alloca %struct.ScmObj*, align 8
%fptrToInt56446 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48837 to i64
%ae48837 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56446)
store volatile %struct.ScmObj* %ae48837, %struct.ScmObj** %stackaddr$makeclosure56445, align 8
%argslist55379$ae488350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56447 = alloca %struct.ScmObj*, align 8
%argslist55379$ae488351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48837, %struct.ScmObj* %argslist55379$ae488350)
store volatile %struct.ScmObj* %argslist55379$ae488351, %struct.ScmObj** %stackaddr$prim56447, align 8
%stackaddr$prim56448 = alloca %struct.ScmObj*, align 8
%argslist55379$ae488352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48836, %struct.ScmObj* %argslist55379$ae488351)
store volatile %struct.ScmObj* %argslist55379$ae488352, %struct.ScmObj** %stackaddr$prim56448, align 8
%clofunc56449 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48835)
musttail call tailcc void %clofunc56449(%struct.ScmObj* %ae48835, %struct.ScmObj* %argslist55379$ae488352)
ret void
}

define tailcc void @proc_clo$ae48835(%struct.ScmObj* %env$ae48835,%struct.ScmObj* %current_45args55325) {
%stackaddr$env-ref56450 = alloca %struct.ScmObj*, align 8
%lsts47181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48835, i64 0)
store %struct.ScmObj* %lsts47181, %struct.ScmObj** %stackaddr$env-ref56450
%stackaddr$env-ref56451 = alloca %struct.ScmObj*, align 8
%_37foldr47101 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48835, i64 1)
store %struct.ScmObj* %_37foldr47101, %struct.ScmObj** %stackaddr$env-ref56451
%stackaddr$env-ref56452 = alloca %struct.ScmObj*, align 8
%f47183 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48835, i64 2)
store %struct.ScmObj* %f47183, %struct.ScmObj** %stackaddr$env-ref56452
%stackaddr$env-ref56453 = alloca %struct.ScmObj*, align 8
%acc47182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48835, i64 3)
store %struct.ScmObj* %acc47182, %struct.ScmObj** %stackaddr$env-ref56453
%stackaddr$env-ref56454 = alloca %struct.ScmObj*, align 8
%_37foldl47179 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48835, i64 4)
store %struct.ScmObj* %_37foldl47179, %struct.ScmObj** %stackaddr$env-ref56454
%stackaddr$env-ref56455 = alloca %struct.ScmObj*, align 8
%k47417 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48835, i64 5)
store %struct.ScmObj* %k47417, %struct.ScmObj** %stackaddr$env-ref56455
%stackaddr$env-ref56456 = alloca %struct.ScmObj*, align 8
%_37foldr147096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48835, i64 6)
store %struct.ScmObj* %_37foldr147096, %struct.ScmObj** %stackaddr$env-ref56456
%stackaddr$env-ref56457 = alloca %struct.ScmObj*, align 8
%_37map147127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48835, i64 7)
store %struct.ScmObj* %_37map147127, %struct.ScmObj** %stackaddr$env-ref56457
%stackaddr$prim56458 = alloca %struct.ScmObj*, align 8
%_95k47418 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55325)
store volatile %struct.ScmObj* %_95k47418, %struct.ScmObj** %stackaddr$prim56458, align 8
%stackaddr$prim56459 = alloca %struct.ScmObj*, align 8
%current_45args55326 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55325)
store volatile %struct.ScmObj* %current_45args55326, %struct.ScmObj** %stackaddr$prim56459, align 8
%stackaddr$prim56460 = alloca %struct.ScmObj*, align 8
%anf_45bind47268 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55326)
store volatile %struct.ScmObj* %anf_45bind47268, %struct.ScmObj** %stackaddr$prim56460, align 8
%stackaddr$makeclosure56461 = alloca %struct.ScmObj*, align 8
%fptrToInt56462 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48867 to i64
%ae48867 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56462)
store volatile %struct.ScmObj* %ae48867, %struct.ScmObj** %stackaddr$makeclosure56461, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48867, %struct.ScmObj* %lsts47181, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48867, %struct.ScmObj* %_37foldr47101, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48867, %struct.ScmObj* %f47183, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48867, %struct.ScmObj* %acc47182, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48867, %struct.ScmObj* %_37foldl47179, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48867, %struct.ScmObj* %k47417, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48867, %struct.ScmObj* %_37map147127, i64 6)
%ae48869 = call %struct.ScmObj* @const_init_false()
%argslist55372$_37foldr1470960 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56463 = alloca %struct.ScmObj*, align 8
%argslist55372$_37foldr1470961 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47181, %struct.ScmObj* %argslist55372$_37foldr1470960)
store volatile %struct.ScmObj* %argslist55372$_37foldr1470961, %struct.ScmObj** %stackaddr$prim56463, align 8
%stackaddr$prim56464 = alloca %struct.ScmObj*, align 8
%argslist55372$_37foldr1470962 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48869, %struct.ScmObj* %argslist55372$_37foldr1470961)
store volatile %struct.ScmObj* %argslist55372$_37foldr1470962, %struct.ScmObj** %stackaddr$prim56464, align 8
%stackaddr$prim56465 = alloca %struct.ScmObj*, align 8
%argslist55372$_37foldr1470963 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47268, %struct.ScmObj* %argslist55372$_37foldr1470962)
store volatile %struct.ScmObj* %argslist55372$_37foldr1470963, %struct.ScmObj** %stackaddr$prim56465, align 8
%stackaddr$prim56466 = alloca %struct.ScmObj*, align 8
%argslist55372$_37foldr1470964 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48867, %struct.ScmObj* %argslist55372$_37foldr1470963)
store volatile %struct.ScmObj* %argslist55372$_37foldr1470964, %struct.ScmObj** %stackaddr$prim56466, align 8
%clofunc56467 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147096)
musttail call tailcc void %clofunc56467(%struct.ScmObj* %_37foldr147096, %struct.ScmObj* %argslist55372$_37foldr1470964)
ret void
}

define tailcc void @proc_clo$ae48867(%struct.ScmObj* %env$ae48867,%struct.ScmObj* %current_45args55328) {
%stackaddr$env-ref56468 = alloca %struct.ScmObj*, align 8
%lsts47181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48867, i64 0)
store %struct.ScmObj* %lsts47181, %struct.ScmObj** %stackaddr$env-ref56468
%stackaddr$env-ref56469 = alloca %struct.ScmObj*, align 8
%_37foldr47101 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48867, i64 1)
store %struct.ScmObj* %_37foldr47101, %struct.ScmObj** %stackaddr$env-ref56469
%stackaddr$env-ref56470 = alloca %struct.ScmObj*, align 8
%f47183 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48867, i64 2)
store %struct.ScmObj* %f47183, %struct.ScmObj** %stackaddr$env-ref56470
%stackaddr$env-ref56471 = alloca %struct.ScmObj*, align 8
%acc47182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48867, i64 3)
store %struct.ScmObj* %acc47182, %struct.ScmObj** %stackaddr$env-ref56471
%stackaddr$env-ref56472 = alloca %struct.ScmObj*, align 8
%_37foldl47179 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48867, i64 4)
store %struct.ScmObj* %_37foldl47179, %struct.ScmObj** %stackaddr$env-ref56472
%stackaddr$env-ref56473 = alloca %struct.ScmObj*, align 8
%k47417 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48867, i64 5)
store %struct.ScmObj* %k47417, %struct.ScmObj** %stackaddr$env-ref56473
%stackaddr$env-ref56474 = alloca %struct.ScmObj*, align 8
%_37map147127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48867, i64 6)
store %struct.ScmObj* %_37map147127, %struct.ScmObj** %stackaddr$env-ref56474
%stackaddr$prim56475 = alloca %struct.ScmObj*, align 8
%_95k47419 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55328)
store volatile %struct.ScmObj* %_95k47419, %struct.ScmObj** %stackaddr$prim56475, align 8
%stackaddr$prim56476 = alloca %struct.ScmObj*, align 8
%current_45args55329 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55328)
store volatile %struct.ScmObj* %current_45args55329, %struct.ScmObj** %stackaddr$prim56476, align 8
%stackaddr$prim56477 = alloca %struct.ScmObj*, align 8
%anf_45bind47269 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55329)
store volatile %struct.ScmObj* %anf_45bind47269, %struct.ScmObj** %stackaddr$prim56477, align 8
%truthy$cmp56478 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47269)
%cmp$cmp56478 = icmp eq i64 %truthy$cmp56478, 1
br i1 %cmp$cmp56478, label %truebranch$cmp56478, label %falsebranch$cmp56478
truebranch$cmp56478:
%ae48878 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55331$k474170 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56479 = alloca %struct.ScmObj*, align 8
%argslist55331$k474171 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47182, %struct.ScmObj* %argslist55331$k474170)
store volatile %struct.ScmObj* %argslist55331$k474171, %struct.ScmObj** %stackaddr$prim56479, align 8
%stackaddr$prim56480 = alloca %struct.ScmObj*, align 8
%argslist55331$k474172 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48878, %struct.ScmObj* %argslist55331$k474171)
store volatile %struct.ScmObj* %argslist55331$k474172, %struct.ScmObj** %stackaddr$prim56480, align 8
%clofunc56481 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47417)
musttail call tailcc void %clofunc56481(%struct.ScmObj* %k47417, %struct.ScmObj* %argslist55331$k474172)
ret void
falsebranch$cmp56478:
%stackaddr$makeclosure56482 = alloca %struct.ScmObj*, align 8
%fptrToInt56483 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48883 to i64
%ae48883 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56483)
store volatile %struct.ScmObj* %ae48883, %struct.ScmObj** %stackaddr$makeclosure56482, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48883, %struct.ScmObj* %lsts47181, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48883, %struct.ScmObj* %_37foldr47101, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48883, %struct.ScmObj* %f47183, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48883, %struct.ScmObj* %acc47182, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48883, %struct.ScmObj* %_37foldl47179, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48883, %struct.ScmObj* %k47417, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48883, %struct.ScmObj* %_37map147127, i64 6)
%ae48884 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56484 = alloca %struct.ScmObj*, align 8
%fptrToInt56485 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48885 to i64
%ae48885 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56485)
store volatile %struct.ScmObj* %ae48885, %struct.ScmObj** %stackaddr$makeclosure56484, align 8
%argslist55371$ae488830 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56486 = alloca %struct.ScmObj*, align 8
%argslist55371$ae488831 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48885, %struct.ScmObj* %argslist55371$ae488830)
store volatile %struct.ScmObj* %argslist55371$ae488831, %struct.ScmObj** %stackaddr$prim56486, align 8
%stackaddr$prim56487 = alloca %struct.ScmObj*, align 8
%argslist55371$ae488832 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48884, %struct.ScmObj* %argslist55371$ae488831)
store volatile %struct.ScmObj* %argslist55371$ae488832, %struct.ScmObj** %stackaddr$prim56487, align 8
%clofunc56488 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48883)
musttail call tailcc void %clofunc56488(%struct.ScmObj* %ae48883, %struct.ScmObj* %argslist55371$ae488832)
ret void
}

define tailcc void @proc_clo$ae48883(%struct.ScmObj* %env$ae48883,%struct.ScmObj* %current_45args55332) {
%stackaddr$env-ref56489 = alloca %struct.ScmObj*, align 8
%lsts47181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48883, i64 0)
store %struct.ScmObj* %lsts47181, %struct.ScmObj** %stackaddr$env-ref56489
%stackaddr$env-ref56490 = alloca %struct.ScmObj*, align 8
%_37foldr47101 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48883, i64 1)
store %struct.ScmObj* %_37foldr47101, %struct.ScmObj** %stackaddr$env-ref56490
%stackaddr$env-ref56491 = alloca %struct.ScmObj*, align 8
%f47183 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48883, i64 2)
store %struct.ScmObj* %f47183, %struct.ScmObj** %stackaddr$env-ref56491
%stackaddr$env-ref56492 = alloca %struct.ScmObj*, align 8
%acc47182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48883, i64 3)
store %struct.ScmObj* %acc47182, %struct.ScmObj** %stackaddr$env-ref56492
%stackaddr$env-ref56493 = alloca %struct.ScmObj*, align 8
%_37foldl47179 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48883, i64 4)
store %struct.ScmObj* %_37foldl47179, %struct.ScmObj** %stackaddr$env-ref56493
%stackaddr$env-ref56494 = alloca %struct.ScmObj*, align 8
%k47417 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48883, i64 5)
store %struct.ScmObj* %k47417, %struct.ScmObj** %stackaddr$env-ref56494
%stackaddr$env-ref56495 = alloca %struct.ScmObj*, align 8
%_37map147127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48883, i64 6)
store %struct.ScmObj* %_37map147127, %struct.ScmObj** %stackaddr$env-ref56495
%stackaddr$prim56496 = alloca %struct.ScmObj*, align 8
%_95k47420 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55332)
store volatile %struct.ScmObj* %_95k47420, %struct.ScmObj** %stackaddr$prim56496, align 8
%stackaddr$prim56497 = alloca %struct.ScmObj*, align 8
%current_45args55333 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55332)
store volatile %struct.ScmObj* %current_45args55333, %struct.ScmObj** %stackaddr$prim56497, align 8
%stackaddr$prim56498 = alloca %struct.ScmObj*, align 8
%anf_45bind47270 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55333)
store volatile %struct.ScmObj* %anf_45bind47270, %struct.ScmObj** %stackaddr$prim56498, align 8
%stackaddr$makeclosure56499 = alloca %struct.ScmObj*, align 8
%fptrToInt56500 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48904 to i64
%ae48904 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56500)
store volatile %struct.ScmObj* %ae48904, %struct.ScmObj** %stackaddr$makeclosure56499, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48904, %struct.ScmObj* %lsts47181, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48904, %struct.ScmObj* %_37foldr47101, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48904, %struct.ScmObj* %f47183, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48904, %struct.ScmObj* %acc47182, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48904, %struct.ScmObj* %_37foldl47179, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48904, %struct.ScmObj* %k47417, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48904, %struct.ScmObj* %_37map147127, i64 6)
%argslist55366$_37map1471270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56501 = alloca %struct.ScmObj*, align 8
%argslist55366$_37map1471271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47181, %struct.ScmObj* %argslist55366$_37map1471270)
store volatile %struct.ScmObj* %argslist55366$_37map1471271, %struct.ScmObj** %stackaddr$prim56501, align 8
%stackaddr$prim56502 = alloca %struct.ScmObj*, align 8
%argslist55366$_37map1471272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47270, %struct.ScmObj* %argslist55366$_37map1471271)
store volatile %struct.ScmObj* %argslist55366$_37map1471272, %struct.ScmObj** %stackaddr$prim56502, align 8
%stackaddr$prim56503 = alloca %struct.ScmObj*, align 8
%argslist55366$_37map1471273 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48904, %struct.ScmObj* %argslist55366$_37map1471272)
store volatile %struct.ScmObj* %argslist55366$_37map1471273, %struct.ScmObj** %stackaddr$prim56503, align 8
%clofunc56504 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147127)
musttail call tailcc void %clofunc56504(%struct.ScmObj* %_37map147127, %struct.ScmObj* %argslist55366$_37map1471273)
ret void
}

define tailcc void @proc_clo$ae48904(%struct.ScmObj* %env$ae48904,%struct.ScmObj* %current_45args55335) {
%stackaddr$env-ref56505 = alloca %struct.ScmObj*, align 8
%lsts47181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48904, i64 0)
store %struct.ScmObj* %lsts47181, %struct.ScmObj** %stackaddr$env-ref56505
%stackaddr$env-ref56506 = alloca %struct.ScmObj*, align 8
%_37foldr47101 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48904, i64 1)
store %struct.ScmObj* %_37foldr47101, %struct.ScmObj** %stackaddr$env-ref56506
%stackaddr$env-ref56507 = alloca %struct.ScmObj*, align 8
%f47183 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48904, i64 2)
store %struct.ScmObj* %f47183, %struct.ScmObj** %stackaddr$env-ref56507
%stackaddr$env-ref56508 = alloca %struct.ScmObj*, align 8
%acc47182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48904, i64 3)
store %struct.ScmObj* %acc47182, %struct.ScmObj** %stackaddr$env-ref56508
%stackaddr$env-ref56509 = alloca %struct.ScmObj*, align 8
%_37foldl47179 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48904, i64 4)
store %struct.ScmObj* %_37foldl47179, %struct.ScmObj** %stackaddr$env-ref56509
%stackaddr$env-ref56510 = alloca %struct.ScmObj*, align 8
%k47417 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48904, i64 5)
store %struct.ScmObj* %k47417, %struct.ScmObj** %stackaddr$env-ref56510
%stackaddr$env-ref56511 = alloca %struct.ScmObj*, align 8
%_37map147127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48904, i64 6)
store %struct.ScmObj* %_37map147127, %struct.ScmObj** %stackaddr$env-ref56511
%stackaddr$prim56512 = alloca %struct.ScmObj*, align 8
%_95k47421 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55335)
store volatile %struct.ScmObj* %_95k47421, %struct.ScmObj** %stackaddr$prim56512, align 8
%stackaddr$prim56513 = alloca %struct.ScmObj*, align 8
%current_45args55336 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55335)
store volatile %struct.ScmObj* %current_45args55336, %struct.ScmObj** %stackaddr$prim56513, align 8
%stackaddr$prim56514 = alloca %struct.ScmObj*, align 8
%lsts_4347188 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55336)
store volatile %struct.ScmObj* %lsts_4347188, %struct.ScmObj** %stackaddr$prim56514, align 8
%stackaddr$makeclosure56515 = alloca %struct.ScmObj*, align 8
%fptrToInt56516 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48907 to i64
%ae48907 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt56516)
store volatile %struct.ScmObj* %ae48907, %struct.ScmObj** %stackaddr$makeclosure56515, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48907, %struct.ScmObj* %lsts47181, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48907, %struct.ScmObj* %_37foldr47101, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48907, %struct.ScmObj* %lsts_4347188, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48907, %struct.ScmObj* %f47183, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48907, %struct.ScmObj* %acc47182, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48907, %struct.ScmObj* %_37foldl47179, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48907, %struct.ScmObj* %k47417, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae48907, %struct.ScmObj* %_37map147127, i64 7)
%ae48908 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56517 = alloca %struct.ScmObj*, align 8
%fptrToInt56518 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48909 to i64
%ae48909 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56518)
store volatile %struct.ScmObj* %ae48909, %struct.ScmObj** %stackaddr$makeclosure56517, align 8
%argslist55365$ae489070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56519 = alloca %struct.ScmObj*, align 8
%argslist55365$ae489071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48909, %struct.ScmObj* %argslist55365$ae489070)
store volatile %struct.ScmObj* %argslist55365$ae489071, %struct.ScmObj** %stackaddr$prim56519, align 8
%stackaddr$prim56520 = alloca %struct.ScmObj*, align 8
%argslist55365$ae489072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48908, %struct.ScmObj* %argslist55365$ae489071)
store volatile %struct.ScmObj* %argslist55365$ae489072, %struct.ScmObj** %stackaddr$prim56520, align 8
%clofunc56521 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48907)
musttail call tailcc void %clofunc56521(%struct.ScmObj* %ae48907, %struct.ScmObj* %argslist55365$ae489072)
ret void
}

define tailcc void @proc_clo$ae48907(%struct.ScmObj* %env$ae48907,%struct.ScmObj* %current_45args55338) {
%stackaddr$env-ref56522 = alloca %struct.ScmObj*, align 8
%lsts47181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48907, i64 0)
store %struct.ScmObj* %lsts47181, %struct.ScmObj** %stackaddr$env-ref56522
%stackaddr$env-ref56523 = alloca %struct.ScmObj*, align 8
%_37foldr47101 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48907, i64 1)
store %struct.ScmObj* %_37foldr47101, %struct.ScmObj** %stackaddr$env-ref56523
%stackaddr$env-ref56524 = alloca %struct.ScmObj*, align 8
%lsts_4347188 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48907, i64 2)
store %struct.ScmObj* %lsts_4347188, %struct.ScmObj** %stackaddr$env-ref56524
%stackaddr$env-ref56525 = alloca %struct.ScmObj*, align 8
%f47183 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48907, i64 3)
store %struct.ScmObj* %f47183, %struct.ScmObj** %stackaddr$env-ref56525
%stackaddr$env-ref56526 = alloca %struct.ScmObj*, align 8
%acc47182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48907, i64 4)
store %struct.ScmObj* %acc47182, %struct.ScmObj** %stackaddr$env-ref56526
%stackaddr$env-ref56527 = alloca %struct.ScmObj*, align 8
%_37foldl47179 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48907, i64 5)
store %struct.ScmObj* %_37foldl47179, %struct.ScmObj** %stackaddr$env-ref56527
%stackaddr$env-ref56528 = alloca %struct.ScmObj*, align 8
%k47417 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48907, i64 6)
store %struct.ScmObj* %k47417, %struct.ScmObj** %stackaddr$env-ref56528
%stackaddr$env-ref56529 = alloca %struct.ScmObj*, align 8
%_37map147127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48907, i64 7)
store %struct.ScmObj* %_37map147127, %struct.ScmObj** %stackaddr$env-ref56529
%stackaddr$prim56530 = alloca %struct.ScmObj*, align 8
%_95k47422 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55338)
store volatile %struct.ScmObj* %_95k47422, %struct.ScmObj** %stackaddr$prim56530, align 8
%stackaddr$prim56531 = alloca %struct.ScmObj*, align 8
%current_45args55339 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55338)
store volatile %struct.ScmObj* %current_45args55339, %struct.ScmObj** %stackaddr$prim56531, align 8
%stackaddr$prim56532 = alloca %struct.ScmObj*, align 8
%anf_45bind47271 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55339)
store volatile %struct.ScmObj* %anf_45bind47271, %struct.ScmObj** %stackaddr$prim56532, align 8
%stackaddr$makeclosure56533 = alloca %struct.ScmObj*, align 8
%fptrToInt56534 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48928 to i64
%ae48928 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt56534)
store volatile %struct.ScmObj* %ae48928, %struct.ScmObj** %stackaddr$makeclosure56533, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48928, %struct.ScmObj* %lsts_4347188, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48928, %struct.ScmObj* %f47183, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48928, %struct.ScmObj* %acc47182, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48928, %struct.ScmObj* %_37foldr47101, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48928, %struct.ScmObj* %_37foldl47179, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48928, %struct.ScmObj* %k47417, i64 5)
%argslist55360$_37map1471270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56535 = alloca %struct.ScmObj*, align 8
%argslist55360$_37map1471271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47181, %struct.ScmObj* %argslist55360$_37map1471270)
store volatile %struct.ScmObj* %argslist55360$_37map1471271, %struct.ScmObj** %stackaddr$prim56535, align 8
%stackaddr$prim56536 = alloca %struct.ScmObj*, align 8
%argslist55360$_37map1471272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47271, %struct.ScmObj* %argslist55360$_37map1471271)
store volatile %struct.ScmObj* %argslist55360$_37map1471272, %struct.ScmObj** %stackaddr$prim56536, align 8
%stackaddr$prim56537 = alloca %struct.ScmObj*, align 8
%argslist55360$_37map1471273 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48928, %struct.ScmObj* %argslist55360$_37map1471272)
store volatile %struct.ScmObj* %argslist55360$_37map1471273, %struct.ScmObj** %stackaddr$prim56537, align 8
%clofunc56538 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147127)
musttail call tailcc void %clofunc56538(%struct.ScmObj* %_37map147127, %struct.ScmObj* %argslist55360$_37map1471273)
ret void
}

define tailcc void @proc_clo$ae48928(%struct.ScmObj* %env$ae48928,%struct.ScmObj* %current_45args55341) {
%stackaddr$env-ref56539 = alloca %struct.ScmObj*, align 8
%lsts_4347188 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48928, i64 0)
store %struct.ScmObj* %lsts_4347188, %struct.ScmObj** %stackaddr$env-ref56539
%stackaddr$env-ref56540 = alloca %struct.ScmObj*, align 8
%f47183 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48928, i64 1)
store %struct.ScmObj* %f47183, %struct.ScmObj** %stackaddr$env-ref56540
%stackaddr$env-ref56541 = alloca %struct.ScmObj*, align 8
%acc47182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48928, i64 2)
store %struct.ScmObj* %acc47182, %struct.ScmObj** %stackaddr$env-ref56541
%stackaddr$env-ref56542 = alloca %struct.ScmObj*, align 8
%_37foldr47101 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48928, i64 3)
store %struct.ScmObj* %_37foldr47101, %struct.ScmObj** %stackaddr$env-ref56542
%stackaddr$env-ref56543 = alloca %struct.ScmObj*, align 8
%_37foldl47179 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48928, i64 4)
store %struct.ScmObj* %_37foldl47179, %struct.ScmObj** %stackaddr$env-ref56543
%stackaddr$env-ref56544 = alloca %struct.ScmObj*, align 8
%k47417 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48928, i64 5)
store %struct.ScmObj* %k47417, %struct.ScmObj** %stackaddr$env-ref56544
%stackaddr$prim56545 = alloca %struct.ScmObj*, align 8
%_95k47423 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55341)
store volatile %struct.ScmObj* %_95k47423, %struct.ScmObj** %stackaddr$prim56545, align 8
%stackaddr$prim56546 = alloca %struct.ScmObj*, align 8
%current_45args55342 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55341)
store volatile %struct.ScmObj* %current_45args55342, %struct.ScmObj** %stackaddr$prim56546, align 8
%stackaddr$prim56547 = alloca %struct.ScmObj*, align 8
%vs47186 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55342)
store volatile %struct.ScmObj* %vs47186, %struct.ScmObj** %stackaddr$prim56547, align 8
%stackaddr$makeclosure56548 = alloca %struct.ScmObj*, align 8
%fptrToInt56549 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48931 to i64
%ae48931 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56549)
store volatile %struct.ScmObj* %ae48931, %struct.ScmObj** %stackaddr$makeclosure56548, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48931, %struct.ScmObj* %lsts_4347188, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48931, %struct.ScmObj* %vs47186, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48931, %struct.ScmObj* %f47183, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48931, %struct.ScmObj* %acc47182, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48931, %struct.ScmObj* %_37foldr47101, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48931, %struct.ScmObj* %_37foldl47179, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48931, %struct.ScmObj* %k47417, i64 6)
%ae48932 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56550 = alloca %struct.ScmObj*, align 8
%fptrToInt56551 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48933 to i64
%ae48933 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56551)
store volatile %struct.ScmObj* %ae48933, %struct.ScmObj** %stackaddr$makeclosure56550, align 8
%argslist55359$ae489310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56552 = alloca %struct.ScmObj*, align 8
%argslist55359$ae489311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48933, %struct.ScmObj* %argslist55359$ae489310)
store volatile %struct.ScmObj* %argslist55359$ae489311, %struct.ScmObj** %stackaddr$prim56552, align 8
%stackaddr$prim56553 = alloca %struct.ScmObj*, align 8
%argslist55359$ae489312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48932, %struct.ScmObj* %argslist55359$ae489311)
store volatile %struct.ScmObj* %argslist55359$ae489312, %struct.ScmObj** %stackaddr$prim56553, align 8
%clofunc56554 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48931)
musttail call tailcc void %clofunc56554(%struct.ScmObj* %ae48931, %struct.ScmObj* %argslist55359$ae489312)
ret void
}

define tailcc void @proc_clo$ae48931(%struct.ScmObj* %env$ae48931,%struct.ScmObj* %current_45args55344) {
%stackaddr$env-ref56555 = alloca %struct.ScmObj*, align 8
%lsts_4347188 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48931, i64 0)
store %struct.ScmObj* %lsts_4347188, %struct.ScmObj** %stackaddr$env-ref56555
%stackaddr$env-ref56556 = alloca %struct.ScmObj*, align 8
%vs47186 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48931, i64 1)
store %struct.ScmObj* %vs47186, %struct.ScmObj** %stackaddr$env-ref56556
%stackaddr$env-ref56557 = alloca %struct.ScmObj*, align 8
%f47183 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48931, i64 2)
store %struct.ScmObj* %f47183, %struct.ScmObj** %stackaddr$env-ref56557
%stackaddr$env-ref56558 = alloca %struct.ScmObj*, align 8
%acc47182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48931, i64 3)
store %struct.ScmObj* %acc47182, %struct.ScmObj** %stackaddr$env-ref56558
%stackaddr$env-ref56559 = alloca %struct.ScmObj*, align 8
%_37foldr47101 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48931, i64 4)
store %struct.ScmObj* %_37foldr47101, %struct.ScmObj** %stackaddr$env-ref56559
%stackaddr$env-ref56560 = alloca %struct.ScmObj*, align 8
%_37foldl47179 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48931, i64 5)
store %struct.ScmObj* %_37foldl47179, %struct.ScmObj** %stackaddr$env-ref56560
%stackaddr$env-ref56561 = alloca %struct.ScmObj*, align 8
%k47417 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48931, i64 6)
store %struct.ScmObj* %k47417, %struct.ScmObj** %stackaddr$env-ref56561
%stackaddr$prim56562 = alloca %struct.ScmObj*, align 8
%_95k47424 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55344)
store volatile %struct.ScmObj* %_95k47424, %struct.ScmObj** %stackaddr$prim56562, align 8
%stackaddr$prim56563 = alloca %struct.ScmObj*, align 8
%current_45args55345 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55344)
store volatile %struct.ScmObj* %current_45args55345, %struct.ScmObj** %stackaddr$prim56563, align 8
%stackaddr$prim56564 = alloca %struct.ScmObj*, align 8
%anf_45bind47272 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55345)
store volatile %struct.ScmObj* %anf_45bind47272, %struct.ScmObj** %stackaddr$prim56564, align 8
%ae48954 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56565 = alloca %struct.ScmObj*, align 8
%anf_45bind47273 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47182, %struct.ScmObj* %ae48954)
store volatile %struct.ScmObj* %anf_45bind47273, %struct.ScmObj** %stackaddr$prim56565, align 8
%stackaddr$makeclosure56566 = alloca %struct.ScmObj*, align 8
%fptrToInt56567 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48956 to i64
%ae48956 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56567)
store volatile %struct.ScmObj* %ae48956, %struct.ScmObj** %stackaddr$makeclosure56566, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48956, %struct.ScmObj* %lsts_4347188, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48956, %struct.ScmObj* %f47183, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48956, %struct.ScmObj* %_37foldl47179, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48956, %struct.ScmObj* %k47417, i64 3)
%argslist55353$_37foldr471010 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56568 = alloca %struct.ScmObj*, align 8
%argslist55353$_37foldr471011 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs47186, %struct.ScmObj* %argslist55353$_37foldr471010)
store volatile %struct.ScmObj* %argslist55353$_37foldr471011, %struct.ScmObj** %stackaddr$prim56568, align 8
%stackaddr$prim56569 = alloca %struct.ScmObj*, align 8
%argslist55353$_37foldr471012 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47273, %struct.ScmObj* %argslist55353$_37foldr471011)
store volatile %struct.ScmObj* %argslist55353$_37foldr471012, %struct.ScmObj** %stackaddr$prim56569, align 8
%stackaddr$prim56570 = alloca %struct.ScmObj*, align 8
%argslist55353$_37foldr471013 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47272, %struct.ScmObj* %argslist55353$_37foldr471012)
store volatile %struct.ScmObj* %argslist55353$_37foldr471013, %struct.ScmObj** %stackaddr$prim56570, align 8
%stackaddr$prim56571 = alloca %struct.ScmObj*, align 8
%argslist55353$_37foldr471014 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48956, %struct.ScmObj* %argslist55353$_37foldr471013)
store volatile %struct.ScmObj* %argslist55353$_37foldr471014, %struct.ScmObj** %stackaddr$prim56571, align 8
%clofunc56572 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr47101)
musttail call tailcc void %clofunc56572(%struct.ScmObj* %_37foldr47101, %struct.ScmObj* %argslist55353$_37foldr471014)
ret void
}

define tailcc void @proc_clo$ae48956(%struct.ScmObj* %env$ae48956,%struct.ScmObj* %current_45args55347) {
%stackaddr$env-ref56573 = alloca %struct.ScmObj*, align 8
%lsts_4347188 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48956, i64 0)
store %struct.ScmObj* %lsts_4347188, %struct.ScmObj** %stackaddr$env-ref56573
%stackaddr$env-ref56574 = alloca %struct.ScmObj*, align 8
%f47183 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48956, i64 1)
store %struct.ScmObj* %f47183, %struct.ScmObj** %stackaddr$env-ref56574
%stackaddr$env-ref56575 = alloca %struct.ScmObj*, align 8
%_37foldl47179 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48956, i64 2)
store %struct.ScmObj* %_37foldl47179, %struct.ScmObj** %stackaddr$env-ref56575
%stackaddr$env-ref56576 = alloca %struct.ScmObj*, align 8
%k47417 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48956, i64 3)
store %struct.ScmObj* %k47417, %struct.ScmObj** %stackaddr$env-ref56576
%stackaddr$prim56577 = alloca %struct.ScmObj*, align 8
%_95k47425 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55347)
store volatile %struct.ScmObj* %_95k47425, %struct.ScmObj** %stackaddr$prim56577, align 8
%stackaddr$prim56578 = alloca %struct.ScmObj*, align 8
%current_45args55348 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55347)
store volatile %struct.ScmObj* %current_45args55348, %struct.ScmObj** %stackaddr$prim56578, align 8
%stackaddr$prim56579 = alloca %struct.ScmObj*, align 8
%anf_45bind47274 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55348)
store volatile %struct.ScmObj* %anf_45bind47274, %struct.ScmObj** %stackaddr$prim56579, align 8
%stackaddr$makeclosure56580 = alloca %struct.ScmObj*, align 8
%fptrToInt56581 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48960 to i64
%ae48960 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56581)
store volatile %struct.ScmObj* %ae48960, %struct.ScmObj** %stackaddr$makeclosure56580, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48960, %struct.ScmObj* %lsts_4347188, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48960, %struct.ScmObj* %f47183, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48960, %struct.ScmObj* %_37foldl47179, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48960, %struct.ScmObj* %k47417, i64 3)
%stackaddr$prim56582 = alloca %struct.ScmObj*, align 8
%cpsargs47428 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48960, %struct.ScmObj* %anf_45bind47274)
store volatile %struct.ScmObj* %cpsargs47428, %struct.ScmObj** %stackaddr$prim56582, align 8
%clofunc56583 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47183)
musttail call tailcc void %clofunc56583(%struct.ScmObj* %f47183, %struct.ScmObj* %cpsargs47428)
ret void
}

define tailcc void @proc_clo$ae48960(%struct.ScmObj* %env$ae48960,%struct.ScmObj* %current_45args55350) {
%stackaddr$env-ref56584 = alloca %struct.ScmObj*, align 8
%lsts_4347188 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48960, i64 0)
store %struct.ScmObj* %lsts_4347188, %struct.ScmObj** %stackaddr$env-ref56584
%stackaddr$env-ref56585 = alloca %struct.ScmObj*, align 8
%f47183 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48960, i64 1)
store %struct.ScmObj* %f47183, %struct.ScmObj** %stackaddr$env-ref56585
%stackaddr$env-ref56586 = alloca %struct.ScmObj*, align 8
%_37foldl47179 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48960, i64 2)
store %struct.ScmObj* %_37foldl47179, %struct.ScmObj** %stackaddr$env-ref56586
%stackaddr$env-ref56587 = alloca %struct.ScmObj*, align 8
%k47417 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48960, i64 3)
store %struct.ScmObj* %k47417, %struct.ScmObj** %stackaddr$env-ref56587
%stackaddr$prim56588 = alloca %struct.ScmObj*, align 8
%_95k47426 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55350)
store volatile %struct.ScmObj* %_95k47426, %struct.ScmObj** %stackaddr$prim56588, align 8
%stackaddr$prim56589 = alloca %struct.ScmObj*, align 8
%current_45args55351 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55350)
store volatile %struct.ScmObj* %current_45args55351, %struct.ScmObj** %stackaddr$prim56589, align 8
%stackaddr$prim56590 = alloca %struct.ScmObj*, align 8
%acc_4347190 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55351)
store volatile %struct.ScmObj* %acc_4347190, %struct.ScmObj** %stackaddr$prim56590, align 8
%stackaddr$prim56591 = alloca %struct.ScmObj*, align 8
%anf_45bind47275 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4347190, %struct.ScmObj* %lsts_4347188)
store volatile %struct.ScmObj* %anf_45bind47275, %struct.ScmObj** %stackaddr$prim56591, align 8
%stackaddr$prim56592 = alloca %struct.ScmObj*, align 8
%anf_45bind47276 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47183, %struct.ScmObj* %anf_45bind47275)
store volatile %struct.ScmObj* %anf_45bind47276, %struct.ScmObj** %stackaddr$prim56592, align 8
%stackaddr$prim56593 = alloca %struct.ScmObj*, align 8
%cpsargs47427 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47417, %struct.ScmObj* %anf_45bind47276)
store volatile %struct.ScmObj* %cpsargs47427, %struct.ScmObj** %stackaddr$prim56593, align 8
%clofunc56594 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl47179)
musttail call tailcc void %clofunc56594(%struct.ScmObj* %_37foldl47179, %struct.ScmObj* %cpsargs47427)
ret void
}

define tailcc void @proc_clo$ae48933(%struct.ScmObj* %env$ae48933,%struct.ScmObj* %current_45args55354) {
%stackaddr$prim56595 = alloca %struct.ScmObj*, align 8
%k47429 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55354)
store volatile %struct.ScmObj* %k47429, %struct.ScmObj** %stackaddr$prim56595, align 8
%stackaddr$prim56596 = alloca %struct.ScmObj*, align 8
%current_45args55355 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55354)
store volatile %struct.ScmObj* %current_45args55355, %struct.ScmObj** %stackaddr$prim56596, align 8
%stackaddr$prim56597 = alloca %struct.ScmObj*, align 8
%a47192 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55355)
store volatile %struct.ScmObj* %a47192, %struct.ScmObj** %stackaddr$prim56597, align 8
%stackaddr$prim56598 = alloca %struct.ScmObj*, align 8
%current_45args55356 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55355)
store volatile %struct.ScmObj* %current_45args55356, %struct.ScmObj** %stackaddr$prim56598, align 8
%stackaddr$prim56599 = alloca %struct.ScmObj*, align 8
%b47191 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55356)
store volatile %struct.ScmObj* %b47191, %struct.ScmObj** %stackaddr$prim56599, align 8
%stackaddr$prim56600 = alloca %struct.ScmObj*, align 8
%cpsprim47430 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a47192, %struct.ScmObj* %b47191)
store volatile %struct.ScmObj* %cpsprim47430, %struct.ScmObj** %stackaddr$prim56600, align 8
%ae48937 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55358$k474290 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56601 = alloca %struct.ScmObj*, align 8
%argslist55358$k474291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47430, %struct.ScmObj* %argslist55358$k474290)
store volatile %struct.ScmObj* %argslist55358$k474291, %struct.ScmObj** %stackaddr$prim56601, align 8
%stackaddr$prim56602 = alloca %struct.ScmObj*, align 8
%argslist55358$k474292 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48937, %struct.ScmObj* %argslist55358$k474291)
store volatile %struct.ScmObj* %argslist55358$k474292, %struct.ScmObj** %stackaddr$prim56602, align 8
%clofunc56603 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47429)
musttail call tailcc void %clofunc56603(%struct.ScmObj* %k47429, %struct.ScmObj* %argslist55358$k474292)
ret void
}

define tailcc void @proc_clo$ae48909(%struct.ScmObj* %env$ae48909,%struct.ScmObj* %current_45args55361) {
%stackaddr$prim56604 = alloca %struct.ScmObj*, align 8
%k47431 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55361)
store volatile %struct.ScmObj* %k47431, %struct.ScmObj** %stackaddr$prim56604, align 8
%stackaddr$prim56605 = alloca %struct.ScmObj*, align 8
%current_45args55362 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55361)
store volatile %struct.ScmObj* %current_45args55362, %struct.ScmObj** %stackaddr$prim56605, align 8
%stackaddr$prim56606 = alloca %struct.ScmObj*, align 8
%x47187 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55362)
store volatile %struct.ScmObj* %x47187, %struct.ScmObj** %stackaddr$prim56606, align 8
%stackaddr$prim56607 = alloca %struct.ScmObj*, align 8
%cpsprim47432 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47187)
store volatile %struct.ScmObj* %cpsprim47432, %struct.ScmObj** %stackaddr$prim56607, align 8
%ae48912 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55364$k474310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56608 = alloca %struct.ScmObj*, align 8
%argslist55364$k474311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47432, %struct.ScmObj* %argslist55364$k474310)
store volatile %struct.ScmObj* %argslist55364$k474311, %struct.ScmObj** %stackaddr$prim56608, align 8
%stackaddr$prim56609 = alloca %struct.ScmObj*, align 8
%argslist55364$k474312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48912, %struct.ScmObj* %argslist55364$k474311)
store volatile %struct.ScmObj* %argslist55364$k474312, %struct.ScmObj** %stackaddr$prim56609, align 8
%clofunc56610 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47431)
musttail call tailcc void %clofunc56610(%struct.ScmObj* %k47431, %struct.ScmObj* %argslist55364$k474312)
ret void
}

define tailcc void @proc_clo$ae48885(%struct.ScmObj* %env$ae48885,%struct.ScmObj* %current_45args55367) {
%stackaddr$prim56611 = alloca %struct.ScmObj*, align 8
%k47433 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55367)
store volatile %struct.ScmObj* %k47433, %struct.ScmObj** %stackaddr$prim56611, align 8
%stackaddr$prim56612 = alloca %struct.ScmObj*, align 8
%current_45args55368 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55367)
store volatile %struct.ScmObj* %current_45args55368, %struct.ScmObj** %stackaddr$prim56612, align 8
%stackaddr$prim56613 = alloca %struct.ScmObj*, align 8
%x47189 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55368)
store volatile %struct.ScmObj* %x47189, %struct.ScmObj** %stackaddr$prim56613, align 8
%stackaddr$prim56614 = alloca %struct.ScmObj*, align 8
%cpsprim47434 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47189)
store volatile %struct.ScmObj* %cpsprim47434, %struct.ScmObj** %stackaddr$prim56614, align 8
%ae48888 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55370$k474330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56615 = alloca %struct.ScmObj*, align 8
%argslist55370$k474331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47434, %struct.ScmObj* %argslist55370$k474330)
store volatile %struct.ScmObj* %argslist55370$k474331, %struct.ScmObj** %stackaddr$prim56615, align 8
%stackaddr$prim56616 = alloca %struct.ScmObj*, align 8
%argslist55370$k474332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48888, %struct.ScmObj* %argslist55370$k474331)
store volatile %struct.ScmObj* %argslist55370$k474332, %struct.ScmObj** %stackaddr$prim56616, align 8
%clofunc56617 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47433)
musttail call tailcc void %clofunc56617(%struct.ScmObj* %k47433, %struct.ScmObj* %argslist55370$k474332)
ret void
}

define tailcc void @proc_clo$ae48837(%struct.ScmObj* %env$ae48837,%struct.ScmObj* %current_45args55373) {
%stackaddr$prim56618 = alloca %struct.ScmObj*, align 8
%k47435 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55373)
store volatile %struct.ScmObj* %k47435, %struct.ScmObj** %stackaddr$prim56618, align 8
%stackaddr$prim56619 = alloca %struct.ScmObj*, align 8
%current_45args55374 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55373)
store volatile %struct.ScmObj* %current_45args55374, %struct.ScmObj** %stackaddr$prim56619, align 8
%stackaddr$prim56620 = alloca %struct.ScmObj*, align 8
%lst47185 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55374)
store volatile %struct.ScmObj* %lst47185, %struct.ScmObj** %stackaddr$prim56620, align 8
%stackaddr$prim56621 = alloca %struct.ScmObj*, align 8
%current_45args55375 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55374)
store volatile %struct.ScmObj* %current_45args55375, %struct.ScmObj** %stackaddr$prim56621, align 8
%stackaddr$prim56622 = alloca %struct.ScmObj*, align 8
%b47184 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55375)
store volatile %struct.ScmObj* %b47184, %struct.ScmObj** %stackaddr$prim56622, align 8
%truthy$cmp56623 = call i64 @is_truthy_value(%struct.ScmObj* %b47184)
%cmp$cmp56623 = icmp eq i64 %truthy$cmp56623, 1
br i1 %cmp$cmp56623, label %truebranch$cmp56623, label %falsebranch$cmp56623
truebranch$cmp56623:
%ae48840 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55377$k474350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56624 = alloca %struct.ScmObj*, align 8
%argslist55377$k474351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b47184, %struct.ScmObj* %argslist55377$k474350)
store volatile %struct.ScmObj* %argslist55377$k474351, %struct.ScmObj** %stackaddr$prim56624, align 8
%stackaddr$prim56625 = alloca %struct.ScmObj*, align 8
%argslist55377$k474352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48840, %struct.ScmObj* %argslist55377$k474351)
store volatile %struct.ScmObj* %argslist55377$k474352, %struct.ScmObj** %stackaddr$prim56625, align 8
%clofunc56626 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47435)
musttail call tailcc void %clofunc56626(%struct.ScmObj* %k47435, %struct.ScmObj* %argslist55377$k474352)
ret void
falsebranch$cmp56623:
%stackaddr$prim56627 = alloca %struct.ScmObj*, align 8
%cpsprim47436 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47185)
store volatile %struct.ScmObj* %cpsprim47436, %struct.ScmObj** %stackaddr$prim56627, align 8
%ae48847 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55378$k474350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56628 = alloca %struct.ScmObj*, align 8
%argslist55378$k474351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47436, %struct.ScmObj* %argslist55378$k474350)
store volatile %struct.ScmObj* %argslist55378$k474351, %struct.ScmObj** %stackaddr$prim56628, align 8
%stackaddr$prim56629 = alloca %struct.ScmObj*, align 8
%argslist55378$k474352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48847, %struct.ScmObj* %argslist55378$k474351)
store volatile %struct.ScmObj* %argslist55378$k474352, %struct.ScmObj** %stackaddr$prim56629, align 8
%clofunc56630 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47435)
musttail call tailcc void %clofunc56630(%struct.ScmObj* %k47435, %struct.ScmObj* %argslist55378$k474352)
ret void
}

define tailcc void @proc_clo$ae48678(%struct.ScmObj* %env$ae48678,%struct.ScmObj* %args4712347437) {
%stackaddr$env-ref56631 = alloca %struct.ScmObj*, align 8
%_37last47118 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48678, i64 0)
store %struct.ScmObj* %_37last47118, %struct.ScmObj** %stackaddr$env-ref56631
%stackaddr$env-ref56632 = alloca %struct.ScmObj*, align 8
%_37foldr47101 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48678, i64 1)
store %struct.ScmObj* %_37foldr47101, %struct.ScmObj** %stackaddr$env-ref56632
%stackaddr$env-ref56633 = alloca %struct.ScmObj*, align 8
%_37drop_45right47115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48678, i64 2)
store %struct.ScmObj* %_37drop_45right47115, %struct.ScmObj** %stackaddr$env-ref56633
%stackaddr$prim56634 = alloca %struct.ScmObj*, align 8
%k47438 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4712347437)
store volatile %struct.ScmObj* %k47438, %struct.ScmObj** %stackaddr$prim56634, align 8
%stackaddr$prim56635 = alloca %struct.ScmObj*, align 8
%args47123 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4712347437)
store volatile %struct.ScmObj* %args47123, %struct.ScmObj** %stackaddr$prim56635, align 8
%stackaddr$prim56636 = alloca %struct.ScmObj*, align 8
%f47125 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47123)
store volatile %struct.ScmObj* %f47125, %struct.ScmObj** %stackaddr$prim56636, align 8
%stackaddr$prim56637 = alloca %struct.ScmObj*, align 8
%lsts47124 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47123)
store volatile %struct.ScmObj* %lsts47124, %struct.ScmObj** %stackaddr$prim56637, align 8
%stackaddr$makeclosure56638 = alloca %struct.ScmObj*, align 8
%fptrToInt56639 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48683 to i64
%ae48683 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56639)
store volatile %struct.ScmObj* %ae48683, %struct.ScmObj** %stackaddr$makeclosure56638, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48683, %struct.ScmObj* %lsts47124, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48683, %struct.ScmObj* %k47438, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48683, %struct.ScmObj* %_37foldr47101, i64 2)
%ae48684 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56640 = alloca %struct.ScmObj*, align 8
%fptrToInt56641 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48685 to i64
%ae48685 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56641)
store volatile %struct.ScmObj* %ae48685, %struct.ScmObj** %stackaddr$makeclosure56640, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48685, %struct.ScmObj* %f47125, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48685, %struct.ScmObj* %_37last47118, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48685, %struct.ScmObj* %_37drop_45right47115, i64 2)
%argslist55397$ae486830 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56642 = alloca %struct.ScmObj*, align 8
%argslist55397$ae486831 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48685, %struct.ScmObj* %argslist55397$ae486830)
store volatile %struct.ScmObj* %argslist55397$ae486831, %struct.ScmObj** %stackaddr$prim56642, align 8
%stackaddr$prim56643 = alloca %struct.ScmObj*, align 8
%argslist55397$ae486832 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48684, %struct.ScmObj* %argslist55397$ae486831)
store volatile %struct.ScmObj* %argslist55397$ae486832, %struct.ScmObj** %stackaddr$prim56643, align 8
%clofunc56644 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48683)
musttail call tailcc void %clofunc56644(%struct.ScmObj* %ae48683, %struct.ScmObj* %argslist55397$ae486832)
ret void
}

define tailcc void @proc_clo$ae48683(%struct.ScmObj* %env$ae48683,%struct.ScmObj* %current_45args55382) {
%stackaddr$env-ref56645 = alloca %struct.ScmObj*, align 8
%lsts47124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48683, i64 0)
store %struct.ScmObj* %lsts47124, %struct.ScmObj** %stackaddr$env-ref56645
%stackaddr$env-ref56646 = alloca %struct.ScmObj*, align 8
%k47438 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48683, i64 1)
store %struct.ScmObj* %k47438, %struct.ScmObj** %stackaddr$env-ref56646
%stackaddr$env-ref56647 = alloca %struct.ScmObj*, align 8
%_37foldr47101 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48683, i64 2)
store %struct.ScmObj* %_37foldr47101, %struct.ScmObj** %stackaddr$env-ref56647
%stackaddr$prim56648 = alloca %struct.ScmObj*, align 8
%_95k47439 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55382)
store volatile %struct.ScmObj* %_95k47439, %struct.ScmObj** %stackaddr$prim56648, align 8
%stackaddr$prim56649 = alloca %struct.ScmObj*, align 8
%current_45args55383 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55382)
store volatile %struct.ScmObj* %current_45args55383, %struct.ScmObj** %stackaddr$prim56649, align 8
%stackaddr$prim56650 = alloca %struct.ScmObj*, align 8
%anf_45bind47263 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55383)
store volatile %struct.ScmObj* %anf_45bind47263, %struct.ScmObj** %stackaddr$prim56650, align 8
%ae48746 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56651 = alloca %struct.ScmObj*, align 8
%anf_45bind47264 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48746, %struct.ScmObj* %lsts47124)
store volatile %struct.ScmObj* %anf_45bind47264, %struct.ScmObj** %stackaddr$prim56651, align 8
%stackaddr$prim56652 = alloca %struct.ScmObj*, align 8
%anf_45bind47265 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47263, %struct.ScmObj* %anf_45bind47264)
store volatile %struct.ScmObj* %anf_45bind47265, %struct.ScmObj** %stackaddr$prim56652, align 8
%stackaddr$prim56653 = alloca %struct.ScmObj*, align 8
%cpsargs47440 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47438, %struct.ScmObj* %anf_45bind47265)
store volatile %struct.ScmObj* %cpsargs47440, %struct.ScmObj** %stackaddr$prim56653, align 8
%clofunc56654 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr47101)
musttail call tailcc void %clofunc56654(%struct.ScmObj* %_37foldr47101, %struct.ScmObj* %cpsargs47440)
ret void
}

define tailcc void @proc_clo$ae48685(%struct.ScmObj* %env$ae48685,%struct.ScmObj* %fargs4712647441) {
%stackaddr$env-ref56655 = alloca %struct.ScmObj*, align 8
%f47125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48685, i64 0)
store %struct.ScmObj* %f47125, %struct.ScmObj** %stackaddr$env-ref56655
%stackaddr$env-ref56656 = alloca %struct.ScmObj*, align 8
%_37last47118 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48685, i64 1)
store %struct.ScmObj* %_37last47118, %struct.ScmObj** %stackaddr$env-ref56656
%stackaddr$env-ref56657 = alloca %struct.ScmObj*, align 8
%_37drop_45right47115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48685, i64 2)
store %struct.ScmObj* %_37drop_45right47115, %struct.ScmObj** %stackaddr$env-ref56657
%stackaddr$prim56658 = alloca %struct.ScmObj*, align 8
%k47442 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs4712647441)
store volatile %struct.ScmObj* %k47442, %struct.ScmObj** %stackaddr$prim56658, align 8
%stackaddr$prim56659 = alloca %struct.ScmObj*, align 8
%fargs47126 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs4712647441)
store volatile %struct.ScmObj* %fargs47126, %struct.ScmObj** %stackaddr$prim56659, align 8
%stackaddr$makeclosure56660 = alloca %struct.ScmObj*, align 8
%fptrToInt56661 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48689 to i64
%ae48689 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56661)
store volatile %struct.ScmObj* %ae48689, %struct.ScmObj** %stackaddr$makeclosure56660, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48689, %struct.ScmObj* %fargs47126, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48689, %struct.ScmObj* %f47125, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48689, %struct.ScmObj* %k47442, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48689, %struct.ScmObj* %_37last47118, i64 3)
%ae48691 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist55396$_37drop_45right471150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56662 = alloca %struct.ScmObj*, align 8
%argslist55396$_37drop_45right471151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48691, %struct.ScmObj* %argslist55396$_37drop_45right471150)
store volatile %struct.ScmObj* %argslist55396$_37drop_45right471151, %struct.ScmObj** %stackaddr$prim56662, align 8
%stackaddr$prim56663 = alloca %struct.ScmObj*, align 8
%argslist55396$_37drop_45right471152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs47126, %struct.ScmObj* %argslist55396$_37drop_45right471151)
store volatile %struct.ScmObj* %argslist55396$_37drop_45right471152, %struct.ScmObj** %stackaddr$prim56663, align 8
%stackaddr$prim56664 = alloca %struct.ScmObj*, align 8
%argslist55396$_37drop_45right471153 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48689, %struct.ScmObj* %argslist55396$_37drop_45right471152)
store volatile %struct.ScmObj* %argslist55396$_37drop_45right471153, %struct.ScmObj** %stackaddr$prim56664, align 8
%clofunc56665 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right47115)
musttail call tailcc void %clofunc56665(%struct.ScmObj* %_37drop_45right47115, %struct.ScmObj* %argslist55396$_37drop_45right471153)
ret void
}

define tailcc void @proc_clo$ae48689(%struct.ScmObj* %env$ae48689,%struct.ScmObj* %current_45args55385) {
%stackaddr$env-ref56666 = alloca %struct.ScmObj*, align 8
%fargs47126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48689, i64 0)
store %struct.ScmObj* %fargs47126, %struct.ScmObj** %stackaddr$env-ref56666
%stackaddr$env-ref56667 = alloca %struct.ScmObj*, align 8
%f47125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48689, i64 1)
store %struct.ScmObj* %f47125, %struct.ScmObj** %stackaddr$env-ref56667
%stackaddr$env-ref56668 = alloca %struct.ScmObj*, align 8
%k47442 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48689, i64 2)
store %struct.ScmObj* %k47442, %struct.ScmObj** %stackaddr$env-ref56668
%stackaddr$env-ref56669 = alloca %struct.ScmObj*, align 8
%_37last47118 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48689, i64 3)
store %struct.ScmObj* %_37last47118, %struct.ScmObj** %stackaddr$env-ref56669
%stackaddr$prim56670 = alloca %struct.ScmObj*, align 8
%_95k47443 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55385)
store volatile %struct.ScmObj* %_95k47443, %struct.ScmObj** %stackaddr$prim56670, align 8
%stackaddr$prim56671 = alloca %struct.ScmObj*, align 8
%current_45args55386 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55385)
store volatile %struct.ScmObj* %current_45args55386, %struct.ScmObj** %stackaddr$prim56671, align 8
%stackaddr$prim56672 = alloca %struct.ScmObj*, align 8
%anf_45bind47260 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55386)
store volatile %struct.ScmObj* %anf_45bind47260, %struct.ScmObj** %stackaddr$prim56672, align 8
%stackaddr$makeclosure56673 = alloca %struct.ScmObj*, align 8
%fptrToInt56674 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48696 to i64
%ae48696 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56674)
store volatile %struct.ScmObj* %ae48696, %struct.ScmObj** %stackaddr$makeclosure56673, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48696, %struct.ScmObj* %fargs47126, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48696, %struct.ScmObj* %k47442, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48696, %struct.ScmObj* %_37last47118, i64 2)
%stackaddr$prim56675 = alloca %struct.ScmObj*, align 8
%cpsargs47447 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48696, %struct.ScmObj* %anf_45bind47260)
store volatile %struct.ScmObj* %cpsargs47447, %struct.ScmObj** %stackaddr$prim56675, align 8
%clofunc56676 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47125)
musttail call tailcc void %clofunc56676(%struct.ScmObj* %f47125, %struct.ScmObj* %cpsargs47447)
ret void
}

define tailcc void @proc_clo$ae48696(%struct.ScmObj* %env$ae48696,%struct.ScmObj* %current_45args55388) {
%stackaddr$env-ref56677 = alloca %struct.ScmObj*, align 8
%fargs47126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48696, i64 0)
store %struct.ScmObj* %fargs47126, %struct.ScmObj** %stackaddr$env-ref56677
%stackaddr$env-ref56678 = alloca %struct.ScmObj*, align 8
%k47442 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48696, i64 1)
store %struct.ScmObj* %k47442, %struct.ScmObj** %stackaddr$env-ref56678
%stackaddr$env-ref56679 = alloca %struct.ScmObj*, align 8
%_37last47118 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48696, i64 2)
store %struct.ScmObj* %_37last47118, %struct.ScmObj** %stackaddr$env-ref56679
%stackaddr$prim56680 = alloca %struct.ScmObj*, align 8
%_95k47444 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55388)
store volatile %struct.ScmObj* %_95k47444, %struct.ScmObj** %stackaddr$prim56680, align 8
%stackaddr$prim56681 = alloca %struct.ScmObj*, align 8
%current_45args55389 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55388)
store volatile %struct.ScmObj* %current_45args55389, %struct.ScmObj** %stackaddr$prim56681, align 8
%stackaddr$prim56682 = alloca %struct.ScmObj*, align 8
%anf_45bind47261 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55389)
store volatile %struct.ScmObj* %anf_45bind47261, %struct.ScmObj** %stackaddr$prim56682, align 8
%stackaddr$makeclosure56683 = alloca %struct.ScmObj*, align 8
%fptrToInt56684 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48701 to i64
%ae48701 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56684)
store volatile %struct.ScmObj* %ae48701, %struct.ScmObj** %stackaddr$makeclosure56683, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48701, %struct.ScmObj* %k47442, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48701, %struct.ScmObj* %anf_45bind47261, i64 1)
%argslist55395$_37last471180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56685 = alloca %struct.ScmObj*, align 8
%argslist55395$_37last471181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs47126, %struct.ScmObj* %argslist55395$_37last471180)
store volatile %struct.ScmObj* %argslist55395$_37last471181, %struct.ScmObj** %stackaddr$prim56685, align 8
%stackaddr$prim56686 = alloca %struct.ScmObj*, align 8
%argslist55395$_37last471182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48701, %struct.ScmObj* %argslist55395$_37last471181)
store volatile %struct.ScmObj* %argslist55395$_37last471182, %struct.ScmObj** %stackaddr$prim56686, align 8
%clofunc56687 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last47118)
musttail call tailcc void %clofunc56687(%struct.ScmObj* %_37last47118, %struct.ScmObj* %argslist55395$_37last471182)
ret void
}

define tailcc void @proc_clo$ae48701(%struct.ScmObj* %env$ae48701,%struct.ScmObj* %current_45args55391) {
%stackaddr$env-ref56688 = alloca %struct.ScmObj*, align 8
%k47442 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48701, i64 0)
store %struct.ScmObj* %k47442, %struct.ScmObj** %stackaddr$env-ref56688
%stackaddr$env-ref56689 = alloca %struct.ScmObj*, align 8
%anf_45bind47261 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48701, i64 1)
store %struct.ScmObj* %anf_45bind47261, %struct.ScmObj** %stackaddr$env-ref56689
%stackaddr$prim56690 = alloca %struct.ScmObj*, align 8
%_95k47445 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55391)
store volatile %struct.ScmObj* %_95k47445, %struct.ScmObj** %stackaddr$prim56690, align 8
%stackaddr$prim56691 = alloca %struct.ScmObj*, align 8
%current_45args55392 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55391)
store volatile %struct.ScmObj* %current_45args55392, %struct.ScmObj** %stackaddr$prim56691, align 8
%stackaddr$prim56692 = alloca %struct.ScmObj*, align 8
%anf_45bind47262 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55392)
store volatile %struct.ScmObj* %anf_45bind47262, %struct.ScmObj** %stackaddr$prim56692, align 8
%stackaddr$prim56693 = alloca %struct.ScmObj*, align 8
%cpsprim47446 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47261, %struct.ScmObj* %anf_45bind47262)
store volatile %struct.ScmObj* %cpsprim47446, %struct.ScmObj** %stackaddr$prim56693, align 8
%ae48706 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55394$k474420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56694 = alloca %struct.ScmObj*, align 8
%argslist55394$k474421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47446, %struct.ScmObj* %argslist55394$k474420)
store volatile %struct.ScmObj* %argslist55394$k474421, %struct.ScmObj** %stackaddr$prim56694, align 8
%stackaddr$prim56695 = alloca %struct.ScmObj*, align 8
%argslist55394$k474422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48706, %struct.ScmObj* %argslist55394$k474421)
store volatile %struct.ScmObj* %argslist55394$k474422, %struct.ScmObj** %stackaddr$prim56695, align 8
%clofunc56696 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47442)
musttail call tailcc void %clofunc56696(%struct.ScmObj* %k47442, %struct.ScmObj* %argslist55394$k474422)
ret void
}

define tailcc void @proc_clo$ae48601(%struct.ScmObj* %env$ae48601,%struct.ScmObj* %current_45args55399) {
%stackaddr$env-ref56697 = alloca %struct.ScmObj*, align 8
%_37foldr147096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48601, i64 0)
store %struct.ScmObj* %_37foldr147096, %struct.ScmObj** %stackaddr$env-ref56697
%stackaddr$prim56698 = alloca %struct.ScmObj*, align 8
%k47448 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55399)
store volatile %struct.ScmObj* %k47448, %struct.ScmObj** %stackaddr$prim56698, align 8
%stackaddr$prim56699 = alloca %struct.ScmObj*, align 8
%current_45args55400 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55399)
store volatile %struct.ScmObj* %current_45args55400, %struct.ScmObj** %stackaddr$prim56699, align 8
%stackaddr$prim56700 = alloca %struct.ScmObj*, align 8
%f47129 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55400)
store volatile %struct.ScmObj* %f47129, %struct.ScmObj** %stackaddr$prim56700, align 8
%stackaddr$prim56701 = alloca %struct.ScmObj*, align 8
%current_45args55401 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55400)
store volatile %struct.ScmObj* %current_45args55401, %struct.ScmObj** %stackaddr$prim56701, align 8
%stackaddr$prim56702 = alloca %struct.ScmObj*, align 8
%lst47128 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55401)
store volatile %struct.ScmObj* %lst47128, %struct.ScmObj** %stackaddr$prim56702, align 8
%stackaddr$makeclosure56703 = alloca %struct.ScmObj*, align 8
%fptrToInt56704 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48602 to i64
%ae48602 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56704)
store volatile %struct.ScmObj* %ae48602, %struct.ScmObj** %stackaddr$makeclosure56703, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48602, %struct.ScmObj* %k47448, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48602, %struct.ScmObj* %lst47128, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48602, %struct.ScmObj* %_37foldr147096, i64 2)
%ae48603 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56705 = alloca %struct.ScmObj*, align 8
%fptrToInt56706 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48604 to i64
%ae48604 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56706)
store volatile %struct.ScmObj* %ae48604, %struct.ScmObj** %stackaddr$makeclosure56705, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48604, %struct.ScmObj* %f47129, i64 0)
%argslist55416$ae486020 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56707 = alloca %struct.ScmObj*, align 8
%argslist55416$ae486021 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48604, %struct.ScmObj* %argslist55416$ae486020)
store volatile %struct.ScmObj* %argslist55416$ae486021, %struct.ScmObj** %stackaddr$prim56707, align 8
%stackaddr$prim56708 = alloca %struct.ScmObj*, align 8
%argslist55416$ae486022 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48603, %struct.ScmObj* %argslist55416$ae486021)
store volatile %struct.ScmObj* %argslist55416$ae486022, %struct.ScmObj** %stackaddr$prim56708, align 8
%clofunc56709 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48602)
musttail call tailcc void %clofunc56709(%struct.ScmObj* %ae48602, %struct.ScmObj* %argslist55416$ae486022)
ret void
}

define tailcc void @proc_clo$ae48602(%struct.ScmObj* %env$ae48602,%struct.ScmObj* %current_45args55403) {
%stackaddr$env-ref56710 = alloca %struct.ScmObj*, align 8
%k47448 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48602, i64 0)
store %struct.ScmObj* %k47448, %struct.ScmObj** %stackaddr$env-ref56710
%stackaddr$env-ref56711 = alloca %struct.ScmObj*, align 8
%lst47128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48602, i64 1)
store %struct.ScmObj* %lst47128, %struct.ScmObj** %stackaddr$env-ref56711
%stackaddr$env-ref56712 = alloca %struct.ScmObj*, align 8
%_37foldr147096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48602, i64 2)
store %struct.ScmObj* %_37foldr147096, %struct.ScmObj** %stackaddr$env-ref56712
%stackaddr$prim56713 = alloca %struct.ScmObj*, align 8
%_95k47449 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55403)
store volatile %struct.ScmObj* %_95k47449, %struct.ScmObj** %stackaddr$prim56713, align 8
%stackaddr$prim56714 = alloca %struct.ScmObj*, align 8
%current_45args55404 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55403)
store volatile %struct.ScmObj* %current_45args55404, %struct.ScmObj** %stackaddr$prim56714, align 8
%stackaddr$prim56715 = alloca %struct.ScmObj*, align 8
%anf_45bind47259 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55404)
store volatile %struct.ScmObj* %anf_45bind47259, %struct.ScmObj** %stackaddr$prim56715, align 8
%ae48636 = call %struct.ScmObj* @const_init_null()
%argslist55406$_37foldr1470960 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56716 = alloca %struct.ScmObj*, align 8
%argslist55406$_37foldr1470961 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47128, %struct.ScmObj* %argslist55406$_37foldr1470960)
store volatile %struct.ScmObj* %argslist55406$_37foldr1470961, %struct.ScmObj** %stackaddr$prim56716, align 8
%stackaddr$prim56717 = alloca %struct.ScmObj*, align 8
%argslist55406$_37foldr1470962 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48636, %struct.ScmObj* %argslist55406$_37foldr1470961)
store volatile %struct.ScmObj* %argslist55406$_37foldr1470962, %struct.ScmObj** %stackaddr$prim56717, align 8
%stackaddr$prim56718 = alloca %struct.ScmObj*, align 8
%argslist55406$_37foldr1470963 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47259, %struct.ScmObj* %argslist55406$_37foldr1470962)
store volatile %struct.ScmObj* %argslist55406$_37foldr1470963, %struct.ScmObj** %stackaddr$prim56718, align 8
%stackaddr$prim56719 = alloca %struct.ScmObj*, align 8
%argslist55406$_37foldr1470964 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47448, %struct.ScmObj* %argslist55406$_37foldr1470963)
store volatile %struct.ScmObj* %argslist55406$_37foldr1470964, %struct.ScmObj** %stackaddr$prim56719, align 8
%clofunc56720 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147096)
musttail call tailcc void %clofunc56720(%struct.ScmObj* %_37foldr147096, %struct.ScmObj* %argslist55406$_37foldr1470964)
ret void
}

define tailcc void @proc_clo$ae48604(%struct.ScmObj* %env$ae48604,%struct.ScmObj* %current_45args55407) {
%stackaddr$env-ref56721 = alloca %struct.ScmObj*, align 8
%f47129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48604, i64 0)
store %struct.ScmObj* %f47129, %struct.ScmObj** %stackaddr$env-ref56721
%stackaddr$prim56722 = alloca %struct.ScmObj*, align 8
%k47450 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55407)
store volatile %struct.ScmObj* %k47450, %struct.ScmObj** %stackaddr$prim56722, align 8
%stackaddr$prim56723 = alloca %struct.ScmObj*, align 8
%current_45args55408 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55407)
store volatile %struct.ScmObj* %current_45args55408, %struct.ScmObj** %stackaddr$prim56723, align 8
%stackaddr$prim56724 = alloca %struct.ScmObj*, align 8
%v47131 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55408)
store volatile %struct.ScmObj* %v47131, %struct.ScmObj** %stackaddr$prim56724, align 8
%stackaddr$prim56725 = alloca %struct.ScmObj*, align 8
%current_45args55409 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55408)
store volatile %struct.ScmObj* %current_45args55409, %struct.ScmObj** %stackaddr$prim56725, align 8
%stackaddr$prim56726 = alloca %struct.ScmObj*, align 8
%r47130 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55409)
store volatile %struct.ScmObj* %r47130, %struct.ScmObj** %stackaddr$prim56726, align 8
%stackaddr$makeclosure56727 = alloca %struct.ScmObj*, align 8
%fptrToInt56728 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48606 to i64
%ae48606 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56728)
store volatile %struct.ScmObj* %ae48606, %struct.ScmObj** %stackaddr$makeclosure56727, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48606, %struct.ScmObj* %k47450, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48606, %struct.ScmObj* %r47130, i64 1)
%argslist55415$f471290 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56729 = alloca %struct.ScmObj*, align 8
%argslist55415$f471291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v47131, %struct.ScmObj* %argslist55415$f471290)
store volatile %struct.ScmObj* %argslist55415$f471291, %struct.ScmObj** %stackaddr$prim56729, align 8
%stackaddr$prim56730 = alloca %struct.ScmObj*, align 8
%argslist55415$f471292 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48606, %struct.ScmObj* %argslist55415$f471291)
store volatile %struct.ScmObj* %argslist55415$f471292, %struct.ScmObj** %stackaddr$prim56730, align 8
%clofunc56731 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47129)
musttail call tailcc void %clofunc56731(%struct.ScmObj* %f47129, %struct.ScmObj* %argslist55415$f471292)
ret void
}

define tailcc void @proc_clo$ae48606(%struct.ScmObj* %env$ae48606,%struct.ScmObj* %current_45args55411) {
%stackaddr$env-ref56732 = alloca %struct.ScmObj*, align 8
%k47450 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48606, i64 0)
store %struct.ScmObj* %k47450, %struct.ScmObj** %stackaddr$env-ref56732
%stackaddr$env-ref56733 = alloca %struct.ScmObj*, align 8
%r47130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48606, i64 1)
store %struct.ScmObj* %r47130, %struct.ScmObj** %stackaddr$env-ref56733
%stackaddr$prim56734 = alloca %struct.ScmObj*, align 8
%_95k47451 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55411)
store volatile %struct.ScmObj* %_95k47451, %struct.ScmObj** %stackaddr$prim56734, align 8
%stackaddr$prim56735 = alloca %struct.ScmObj*, align 8
%current_45args55412 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55411)
store volatile %struct.ScmObj* %current_45args55412, %struct.ScmObj** %stackaddr$prim56735, align 8
%stackaddr$prim56736 = alloca %struct.ScmObj*, align 8
%anf_45bind47258 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55412)
store volatile %struct.ScmObj* %anf_45bind47258, %struct.ScmObj** %stackaddr$prim56736, align 8
%stackaddr$prim56737 = alloca %struct.ScmObj*, align 8
%cpsprim47452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47258, %struct.ScmObj* %r47130)
store volatile %struct.ScmObj* %cpsprim47452, %struct.ScmObj** %stackaddr$prim56737, align 8
%ae48611 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55414$k474500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56738 = alloca %struct.ScmObj*, align 8
%argslist55414$k474501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47452, %struct.ScmObj* %argslist55414$k474500)
store volatile %struct.ScmObj* %argslist55414$k474501, %struct.ScmObj** %stackaddr$prim56738, align 8
%stackaddr$prim56739 = alloca %struct.ScmObj*, align 8
%argslist55414$k474502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48611, %struct.ScmObj* %argslist55414$k474501)
store volatile %struct.ScmObj* %argslist55414$k474502, %struct.ScmObj** %stackaddr$prim56739, align 8
%clofunc56740 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47450)
musttail call tailcc void %clofunc56740(%struct.ScmObj* %k47450, %struct.ScmObj* %argslist55414$k474502)
ret void
}

define tailcc void @proc_clo$ae48215(%struct.ScmObj* %env$ae48215,%struct.ScmObj* %current_45args55419) {
%stackaddr$env-ref56741 = alloca %struct.ScmObj*, align 8
%_37map147092 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48215, i64 0)
store %struct.ScmObj* %_37map147092, %struct.ScmObj** %stackaddr$env-ref56741
%stackaddr$env-ref56742 = alloca %struct.ScmObj*, align 8
%_37foldr147096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48215, i64 1)
store %struct.ScmObj* %_37foldr147096, %struct.ScmObj** %stackaddr$env-ref56742
%stackaddr$prim56743 = alloca %struct.ScmObj*, align 8
%k47453 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55419)
store volatile %struct.ScmObj* %k47453, %struct.ScmObj** %stackaddr$prim56743, align 8
%stackaddr$prim56744 = alloca %struct.ScmObj*, align 8
%current_45args55420 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55419)
store volatile %struct.ScmObj* %current_45args55420, %struct.ScmObj** %stackaddr$prim56744, align 8
%stackaddr$prim56745 = alloca %struct.ScmObj*, align 8
%_37foldr47102 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55420)
store volatile %struct.ScmObj* %_37foldr47102, %struct.ScmObj** %stackaddr$prim56745, align 8
%ae48217 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56746 = alloca %struct.ScmObj*, align 8
%fptrToInt56747 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48218 to i64
%ae48218 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56747)
store volatile %struct.ScmObj* %ae48218, %struct.ScmObj** %stackaddr$makeclosure56746, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48218, %struct.ScmObj* %_37map147092, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48218, %struct.ScmObj* %_37foldr47102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48218, %struct.ScmObj* %_37foldr147096, i64 2)
%argslist55477$k474530 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56748 = alloca %struct.ScmObj*, align 8
%argslist55477$k474531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48218, %struct.ScmObj* %argslist55477$k474530)
store volatile %struct.ScmObj* %argslist55477$k474531, %struct.ScmObj** %stackaddr$prim56748, align 8
%stackaddr$prim56749 = alloca %struct.ScmObj*, align 8
%argslist55477$k474532 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48217, %struct.ScmObj* %argslist55477$k474531)
store volatile %struct.ScmObj* %argslist55477$k474532, %struct.ScmObj** %stackaddr$prim56749, align 8
%clofunc56750 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47453)
musttail call tailcc void %clofunc56750(%struct.ScmObj* %k47453, %struct.ScmObj* %argslist55477$k474532)
ret void
}

define tailcc void @proc_clo$ae48218(%struct.ScmObj* %env$ae48218,%struct.ScmObj* %args4710347454) {
%stackaddr$env-ref56751 = alloca %struct.ScmObj*, align 8
%_37map147092 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48218, i64 0)
store %struct.ScmObj* %_37map147092, %struct.ScmObj** %stackaddr$env-ref56751
%stackaddr$env-ref56752 = alloca %struct.ScmObj*, align 8
%_37foldr47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48218, i64 1)
store %struct.ScmObj* %_37foldr47102, %struct.ScmObj** %stackaddr$env-ref56752
%stackaddr$env-ref56753 = alloca %struct.ScmObj*, align 8
%_37foldr147096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48218, i64 2)
store %struct.ScmObj* %_37foldr147096, %struct.ScmObj** %stackaddr$env-ref56753
%stackaddr$prim56754 = alloca %struct.ScmObj*, align 8
%k47455 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4710347454)
store volatile %struct.ScmObj* %k47455, %struct.ScmObj** %stackaddr$prim56754, align 8
%stackaddr$prim56755 = alloca %struct.ScmObj*, align 8
%args47103 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4710347454)
store volatile %struct.ScmObj* %args47103, %struct.ScmObj** %stackaddr$prim56755, align 8
%stackaddr$prim56756 = alloca %struct.ScmObj*, align 8
%f47106 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47103)
store volatile %struct.ScmObj* %f47106, %struct.ScmObj** %stackaddr$prim56756, align 8
%stackaddr$prim56757 = alloca %struct.ScmObj*, align 8
%anf_45bind47245 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47103)
store volatile %struct.ScmObj* %anf_45bind47245, %struct.ScmObj** %stackaddr$prim56757, align 8
%stackaddr$prim56758 = alloca %struct.ScmObj*, align 8
%acc47105 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47245)
store volatile %struct.ScmObj* %acc47105, %struct.ScmObj** %stackaddr$prim56758, align 8
%stackaddr$prim56759 = alloca %struct.ScmObj*, align 8
%anf_45bind47246 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47103)
store volatile %struct.ScmObj* %anf_45bind47246, %struct.ScmObj** %stackaddr$prim56759, align 8
%stackaddr$prim56760 = alloca %struct.ScmObj*, align 8
%lsts47104 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47246)
store volatile %struct.ScmObj* %lsts47104, %struct.ScmObj** %stackaddr$prim56760, align 8
%stackaddr$makeclosure56761 = alloca %struct.ScmObj*, align 8
%fptrToInt56762 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48226 to i64
%ae48226 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56762)
store volatile %struct.ScmObj* %ae48226, %struct.ScmObj** %stackaddr$makeclosure56761, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48226, %struct.ScmObj* %_37map147092, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48226, %struct.ScmObj* %f47106, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48226, %struct.ScmObj* %acc47105, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48226, %struct.ScmObj* %lsts47104, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48226, %struct.ScmObj* %k47455, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48226, %struct.ScmObj* %_37foldr47102, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48226, %struct.ScmObj* %_37foldr147096, i64 6)
%ae48227 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56763 = alloca %struct.ScmObj*, align 8
%fptrToInt56764 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48228 to i64
%ae48228 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56764)
store volatile %struct.ScmObj* %ae48228, %struct.ScmObj** %stackaddr$makeclosure56763, align 8
%argslist55476$ae482260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56765 = alloca %struct.ScmObj*, align 8
%argslist55476$ae482261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48228, %struct.ScmObj* %argslist55476$ae482260)
store volatile %struct.ScmObj* %argslist55476$ae482261, %struct.ScmObj** %stackaddr$prim56765, align 8
%stackaddr$prim56766 = alloca %struct.ScmObj*, align 8
%argslist55476$ae482262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48227, %struct.ScmObj* %argslist55476$ae482261)
store volatile %struct.ScmObj* %argslist55476$ae482262, %struct.ScmObj** %stackaddr$prim56766, align 8
%clofunc56767 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48226)
musttail call tailcc void %clofunc56767(%struct.ScmObj* %ae48226, %struct.ScmObj* %argslist55476$ae482262)
ret void
}

define tailcc void @proc_clo$ae48226(%struct.ScmObj* %env$ae48226,%struct.ScmObj* %current_45args55422) {
%stackaddr$env-ref56768 = alloca %struct.ScmObj*, align 8
%_37map147092 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48226, i64 0)
store %struct.ScmObj* %_37map147092, %struct.ScmObj** %stackaddr$env-ref56768
%stackaddr$env-ref56769 = alloca %struct.ScmObj*, align 8
%f47106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48226, i64 1)
store %struct.ScmObj* %f47106, %struct.ScmObj** %stackaddr$env-ref56769
%stackaddr$env-ref56770 = alloca %struct.ScmObj*, align 8
%acc47105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48226, i64 2)
store %struct.ScmObj* %acc47105, %struct.ScmObj** %stackaddr$env-ref56770
%stackaddr$env-ref56771 = alloca %struct.ScmObj*, align 8
%lsts47104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48226, i64 3)
store %struct.ScmObj* %lsts47104, %struct.ScmObj** %stackaddr$env-ref56771
%stackaddr$env-ref56772 = alloca %struct.ScmObj*, align 8
%k47455 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48226, i64 4)
store %struct.ScmObj* %k47455, %struct.ScmObj** %stackaddr$env-ref56772
%stackaddr$env-ref56773 = alloca %struct.ScmObj*, align 8
%_37foldr47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48226, i64 5)
store %struct.ScmObj* %_37foldr47102, %struct.ScmObj** %stackaddr$env-ref56773
%stackaddr$env-ref56774 = alloca %struct.ScmObj*, align 8
%_37foldr147096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48226, i64 6)
store %struct.ScmObj* %_37foldr147096, %struct.ScmObj** %stackaddr$env-ref56774
%stackaddr$prim56775 = alloca %struct.ScmObj*, align 8
%_95k47456 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55422)
store volatile %struct.ScmObj* %_95k47456, %struct.ScmObj** %stackaddr$prim56775, align 8
%stackaddr$prim56776 = alloca %struct.ScmObj*, align 8
%current_45args55423 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55422)
store volatile %struct.ScmObj* %current_45args55423, %struct.ScmObj** %stackaddr$prim56776, align 8
%stackaddr$prim56777 = alloca %struct.ScmObj*, align 8
%anf_45bind47247 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55423)
store volatile %struct.ScmObj* %anf_45bind47247, %struct.ScmObj** %stackaddr$prim56777, align 8
%stackaddr$makeclosure56778 = alloca %struct.ScmObj*, align 8
%fptrToInt56779 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48258 to i64
%ae48258 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56779)
store volatile %struct.ScmObj* %ae48258, %struct.ScmObj** %stackaddr$makeclosure56778, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48258, %struct.ScmObj* %_37map147092, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48258, %struct.ScmObj* %f47106, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48258, %struct.ScmObj* %acc47105, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48258, %struct.ScmObj* %lsts47104, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48258, %struct.ScmObj* %k47455, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48258, %struct.ScmObj* %_37foldr47102, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48258, %struct.ScmObj* %_37foldr147096, i64 6)
%ae48260 = call %struct.ScmObj* @const_init_false()
%argslist55469$_37foldr1470960 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56780 = alloca %struct.ScmObj*, align 8
%argslist55469$_37foldr1470961 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47104, %struct.ScmObj* %argslist55469$_37foldr1470960)
store volatile %struct.ScmObj* %argslist55469$_37foldr1470961, %struct.ScmObj** %stackaddr$prim56780, align 8
%stackaddr$prim56781 = alloca %struct.ScmObj*, align 8
%argslist55469$_37foldr1470962 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48260, %struct.ScmObj* %argslist55469$_37foldr1470961)
store volatile %struct.ScmObj* %argslist55469$_37foldr1470962, %struct.ScmObj** %stackaddr$prim56781, align 8
%stackaddr$prim56782 = alloca %struct.ScmObj*, align 8
%argslist55469$_37foldr1470963 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47247, %struct.ScmObj* %argslist55469$_37foldr1470962)
store volatile %struct.ScmObj* %argslist55469$_37foldr1470963, %struct.ScmObj** %stackaddr$prim56782, align 8
%stackaddr$prim56783 = alloca %struct.ScmObj*, align 8
%argslist55469$_37foldr1470964 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48258, %struct.ScmObj* %argslist55469$_37foldr1470963)
store volatile %struct.ScmObj* %argslist55469$_37foldr1470964, %struct.ScmObj** %stackaddr$prim56783, align 8
%clofunc56784 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147096)
musttail call tailcc void %clofunc56784(%struct.ScmObj* %_37foldr147096, %struct.ScmObj* %argslist55469$_37foldr1470964)
ret void
}

define tailcc void @proc_clo$ae48258(%struct.ScmObj* %env$ae48258,%struct.ScmObj* %current_45args55425) {
%stackaddr$env-ref56785 = alloca %struct.ScmObj*, align 8
%_37map147092 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48258, i64 0)
store %struct.ScmObj* %_37map147092, %struct.ScmObj** %stackaddr$env-ref56785
%stackaddr$env-ref56786 = alloca %struct.ScmObj*, align 8
%f47106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48258, i64 1)
store %struct.ScmObj* %f47106, %struct.ScmObj** %stackaddr$env-ref56786
%stackaddr$env-ref56787 = alloca %struct.ScmObj*, align 8
%acc47105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48258, i64 2)
store %struct.ScmObj* %acc47105, %struct.ScmObj** %stackaddr$env-ref56787
%stackaddr$env-ref56788 = alloca %struct.ScmObj*, align 8
%lsts47104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48258, i64 3)
store %struct.ScmObj* %lsts47104, %struct.ScmObj** %stackaddr$env-ref56788
%stackaddr$env-ref56789 = alloca %struct.ScmObj*, align 8
%k47455 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48258, i64 4)
store %struct.ScmObj* %k47455, %struct.ScmObj** %stackaddr$env-ref56789
%stackaddr$env-ref56790 = alloca %struct.ScmObj*, align 8
%_37foldr47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48258, i64 5)
store %struct.ScmObj* %_37foldr47102, %struct.ScmObj** %stackaddr$env-ref56790
%stackaddr$env-ref56791 = alloca %struct.ScmObj*, align 8
%_37foldr147096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48258, i64 6)
store %struct.ScmObj* %_37foldr147096, %struct.ScmObj** %stackaddr$env-ref56791
%stackaddr$prim56792 = alloca %struct.ScmObj*, align 8
%_95k47457 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55425)
store volatile %struct.ScmObj* %_95k47457, %struct.ScmObj** %stackaddr$prim56792, align 8
%stackaddr$prim56793 = alloca %struct.ScmObj*, align 8
%current_45args55426 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55425)
store volatile %struct.ScmObj* %current_45args55426, %struct.ScmObj** %stackaddr$prim56793, align 8
%stackaddr$prim56794 = alloca %struct.ScmObj*, align 8
%anf_45bind47248 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55426)
store volatile %struct.ScmObj* %anf_45bind47248, %struct.ScmObj** %stackaddr$prim56794, align 8
%truthy$cmp56795 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47248)
%cmp$cmp56795 = icmp eq i64 %truthy$cmp56795, 1
br i1 %cmp$cmp56795, label %truebranch$cmp56795, label %falsebranch$cmp56795
truebranch$cmp56795:
%ae48269 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55428$k474550 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56796 = alloca %struct.ScmObj*, align 8
%argslist55428$k474551 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47105, %struct.ScmObj* %argslist55428$k474550)
store volatile %struct.ScmObj* %argslist55428$k474551, %struct.ScmObj** %stackaddr$prim56796, align 8
%stackaddr$prim56797 = alloca %struct.ScmObj*, align 8
%argslist55428$k474552 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48269, %struct.ScmObj* %argslist55428$k474551)
store volatile %struct.ScmObj* %argslist55428$k474552, %struct.ScmObj** %stackaddr$prim56797, align 8
%clofunc56798 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47455)
musttail call tailcc void %clofunc56798(%struct.ScmObj* %k47455, %struct.ScmObj* %argslist55428$k474552)
ret void
falsebranch$cmp56795:
%stackaddr$makeclosure56799 = alloca %struct.ScmObj*, align 8
%fptrToInt56800 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48274 to i64
%ae48274 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56800)
store volatile %struct.ScmObj* %ae48274, %struct.ScmObj** %stackaddr$makeclosure56799, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48274, %struct.ScmObj* %_37map147092, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48274, %struct.ScmObj* %f47106, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48274, %struct.ScmObj* %acc47105, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48274, %struct.ScmObj* %lsts47104, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48274, %struct.ScmObj* %k47455, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48274, %struct.ScmObj* %_37foldr47102, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48274, %struct.ScmObj* %_37foldr147096, i64 6)
%ae48275 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56801 = alloca %struct.ScmObj*, align 8
%fptrToInt56802 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48276 to i64
%ae48276 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56802)
store volatile %struct.ScmObj* %ae48276, %struct.ScmObj** %stackaddr$makeclosure56801, align 8
%argslist55468$ae482740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56803 = alloca %struct.ScmObj*, align 8
%argslist55468$ae482741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48276, %struct.ScmObj* %argslist55468$ae482740)
store volatile %struct.ScmObj* %argslist55468$ae482741, %struct.ScmObj** %stackaddr$prim56803, align 8
%stackaddr$prim56804 = alloca %struct.ScmObj*, align 8
%argslist55468$ae482742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48275, %struct.ScmObj* %argslist55468$ae482741)
store volatile %struct.ScmObj* %argslist55468$ae482742, %struct.ScmObj** %stackaddr$prim56804, align 8
%clofunc56805 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48274)
musttail call tailcc void %clofunc56805(%struct.ScmObj* %ae48274, %struct.ScmObj* %argslist55468$ae482742)
ret void
}

define tailcc void @proc_clo$ae48274(%struct.ScmObj* %env$ae48274,%struct.ScmObj* %current_45args55429) {
%stackaddr$env-ref56806 = alloca %struct.ScmObj*, align 8
%_37map147092 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48274, i64 0)
store %struct.ScmObj* %_37map147092, %struct.ScmObj** %stackaddr$env-ref56806
%stackaddr$env-ref56807 = alloca %struct.ScmObj*, align 8
%f47106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48274, i64 1)
store %struct.ScmObj* %f47106, %struct.ScmObj** %stackaddr$env-ref56807
%stackaddr$env-ref56808 = alloca %struct.ScmObj*, align 8
%acc47105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48274, i64 2)
store %struct.ScmObj* %acc47105, %struct.ScmObj** %stackaddr$env-ref56808
%stackaddr$env-ref56809 = alloca %struct.ScmObj*, align 8
%lsts47104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48274, i64 3)
store %struct.ScmObj* %lsts47104, %struct.ScmObj** %stackaddr$env-ref56809
%stackaddr$env-ref56810 = alloca %struct.ScmObj*, align 8
%k47455 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48274, i64 4)
store %struct.ScmObj* %k47455, %struct.ScmObj** %stackaddr$env-ref56810
%stackaddr$env-ref56811 = alloca %struct.ScmObj*, align 8
%_37foldr47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48274, i64 5)
store %struct.ScmObj* %_37foldr47102, %struct.ScmObj** %stackaddr$env-ref56811
%stackaddr$env-ref56812 = alloca %struct.ScmObj*, align 8
%_37foldr147096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48274, i64 6)
store %struct.ScmObj* %_37foldr147096, %struct.ScmObj** %stackaddr$env-ref56812
%stackaddr$prim56813 = alloca %struct.ScmObj*, align 8
%_95k47458 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55429)
store volatile %struct.ScmObj* %_95k47458, %struct.ScmObj** %stackaddr$prim56813, align 8
%stackaddr$prim56814 = alloca %struct.ScmObj*, align 8
%current_45args55430 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55429)
store volatile %struct.ScmObj* %current_45args55430, %struct.ScmObj** %stackaddr$prim56814, align 8
%stackaddr$prim56815 = alloca %struct.ScmObj*, align 8
%anf_45bind47249 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55430)
store volatile %struct.ScmObj* %anf_45bind47249, %struct.ScmObj** %stackaddr$prim56815, align 8
%stackaddr$makeclosure56816 = alloca %struct.ScmObj*, align 8
%fptrToInt56817 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48295 to i64
%ae48295 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56817)
store volatile %struct.ScmObj* %ae48295, %struct.ScmObj** %stackaddr$makeclosure56816, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48295, %struct.ScmObj* %_37map147092, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48295, %struct.ScmObj* %f47106, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48295, %struct.ScmObj* %acc47105, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48295, %struct.ScmObj* %lsts47104, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48295, %struct.ScmObj* %k47455, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48295, %struct.ScmObj* %_37foldr47102, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48295, %struct.ScmObj* %_37foldr147096, i64 6)
%argslist55463$_37map1470920 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56818 = alloca %struct.ScmObj*, align 8
%argslist55463$_37map1470921 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47104, %struct.ScmObj* %argslist55463$_37map1470920)
store volatile %struct.ScmObj* %argslist55463$_37map1470921, %struct.ScmObj** %stackaddr$prim56818, align 8
%stackaddr$prim56819 = alloca %struct.ScmObj*, align 8
%argslist55463$_37map1470922 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47249, %struct.ScmObj* %argslist55463$_37map1470921)
store volatile %struct.ScmObj* %argslist55463$_37map1470922, %struct.ScmObj** %stackaddr$prim56819, align 8
%stackaddr$prim56820 = alloca %struct.ScmObj*, align 8
%argslist55463$_37map1470923 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48295, %struct.ScmObj* %argslist55463$_37map1470922)
store volatile %struct.ScmObj* %argslist55463$_37map1470923, %struct.ScmObj** %stackaddr$prim56820, align 8
%clofunc56821 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147092)
musttail call tailcc void %clofunc56821(%struct.ScmObj* %_37map147092, %struct.ScmObj* %argslist55463$_37map1470923)
ret void
}

define tailcc void @proc_clo$ae48295(%struct.ScmObj* %env$ae48295,%struct.ScmObj* %current_45args55432) {
%stackaddr$env-ref56822 = alloca %struct.ScmObj*, align 8
%_37map147092 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48295, i64 0)
store %struct.ScmObj* %_37map147092, %struct.ScmObj** %stackaddr$env-ref56822
%stackaddr$env-ref56823 = alloca %struct.ScmObj*, align 8
%f47106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48295, i64 1)
store %struct.ScmObj* %f47106, %struct.ScmObj** %stackaddr$env-ref56823
%stackaddr$env-ref56824 = alloca %struct.ScmObj*, align 8
%acc47105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48295, i64 2)
store %struct.ScmObj* %acc47105, %struct.ScmObj** %stackaddr$env-ref56824
%stackaddr$env-ref56825 = alloca %struct.ScmObj*, align 8
%lsts47104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48295, i64 3)
store %struct.ScmObj* %lsts47104, %struct.ScmObj** %stackaddr$env-ref56825
%stackaddr$env-ref56826 = alloca %struct.ScmObj*, align 8
%k47455 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48295, i64 4)
store %struct.ScmObj* %k47455, %struct.ScmObj** %stackaddr$env-ref56826
%stackaddr$env-ref56827 = alloca %struct.ScmObj*, align 8
%_37foldr47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48295, i64 5)
store %struct.ScmObj* %_37foldr47102, %struct.ScmObj** %stackaddr$env-ref56827
%stackaddr$env-ref56828 = alloca %struct.ScmObj*, align 8
%_37foldr147096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48295, i64 6)
store %struct.ScmObj* %_37foldr147096, %struct.ScmObj** %stackaddr$env-ref56828
%stackaddr$prim56829 = alloca %struct.ScmObj*, align 8
%_95k47459 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55432)
store volatile %struct.ScmObj* %_95k47459, %struct.ScmObj** %stackaddr$prim56829, align 8
%stackaddr$prim56830 = alloca %struct.ScmObj*, align 8
%current_45args55433 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55432)
store volatile %struct.ScmObj* %current_45args55433, %struct.ScmObj** %stackaddr$prim56830, align 8
%stackaddr$prim56831 = alloca %struct.ScmObj*, align 8
%lsts_4347111 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55433)
store volatile %struct.ScmObj* %lsts_4347111, %struct.ScmObj** %stackaddr$prim56831, align 8
%stackaddr$makeclosure56832 = alloca %struct.ScmObj*, align 8
%fptrToInt56833 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48298 to i64
%ae48298 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt56833)
store volatile %struct.ScmObj* %ae48298, %struct.ScmObj** %stackaddr$makeclosure56832, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48298, %struct.ScmObj* %_37map147092, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48298, %struct.ScmObj* %f47106, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48298, %struct.ScmObj* %acc47105, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48298, %struct.ScmObj* %lsts47104, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48298, %struct.ScmObj* %k47455, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48298, %struct.ScmObj* %_37foldr47102, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48298, %struct.ScmObj* %_37foldr147096, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae48298, %struct.ScmObj* %lsts_4347111, i64 7)
%ae48299 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56834 = alloca %struct.ScmObj*, align 8
%fptrToInt56835 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48300 to i64
%ae48300 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56835)
store volatile %struct.ScmObj* %ae48300, %struct.ScmObj** %stackaddr$makeclosure56834, align 8
%argslist55462$ae482980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56836 = alloca %struct.ScmObj*, align 8
%argslist55462$ae482981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48300, %struct.ScmObj* %argslist55462$ae482980)
store volatile %struct.ScmObj* %argslist55462$ae482981, %struct.ScmObj** %stackaddr$prim56836, align 8
%stackaddr$prim56837 = alloca %struct.ScmObj*, align 8
%argslist55462$ae482982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48299, %struct.ScmObj* %argslist55462$ae482981)
store volatile %struct.ScmObj* %argslist55462$ae482982, %struct.ScmObj** %stackaddr$prim56837, align 8
%clofunc56838 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48298)
musttail call tailcc void %clofunc56838(%struct.ScmObj* %ae48298, %struct.ScmObj* %argslist55462$ae482982)
ret void
}

define tailcc void @proc_clo$ae48298(%struct.ScmObj* %env$ae48298,%struct.ScmObj* %current_45args55435) {
%stackaddr$env-ref56839 = alloca %struct.ScmObj*, align 8
%_37map147092 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48298, i64 0)
store %struct.ScmObj* %_37map147092, %struct.ScmObj** %stackaddr$env-ref56839
%stackaddr$env-ref56840 = alloca %struct.ScmObj*, align 8
%f47106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48298, i64 1)
store %struct.ScmObj* %f47106, %struct.ScmObj** %stackaddr$env-ref56840
%stackaddr$env-ref56841 = alloca %struct.ScmObj*, align 8
%acc47105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48298, i64 2)
store %struct.ScmObj* %acc47105, %struct.ScmObj** %stackaddr$env-ref56841
%stackaddr$env-ref56842 = alloca %struct.ScmObj*, align 8
%lsts47104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48298, i64 3)
store %struct.ScmObj* %lsts47104, %struct.ScmObj** %stackaddr$env-ref56842
%stackaddr$env-ref56843 = alloca %struct.ScmObj*, align 8
%k47455 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48298, i64 4)
store %struct.ScmObj* %k47455, %struct.ScmObj** %stackaddr$env-ref56843
%stackaddr$env-ref56844 = alloca %struct.ScmObj*, align 8
%_37foldr47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48298, i64 5)
store %struct.ScmObj* %_37foldr47102, %struct.ScmObj** %stackaddr$env-ref56844
%stackaddr$env-ref56845 = alloca %struct.ScmObj*, align 8
%_37foldr147096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48298, i64 6)
store %struct.ScmObj* %_37foldr147096, %struct.ScmObj** %stackaddr$env-ref56845
%stackaddr$env-ref56846 = alloca %struct.ScmObj*, align 8
%lsts_4347111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48298, i64 7)
store %struct.ScmObj* %lsts_4347111, %struct.ScmObj** %stackaddr$env-ref56846
%stackaddr$prim56847 = alloca %struct.ScmObj*, align 8
%_95k47460 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55435)
store volatile %struct.ScmObj* %_95k47460, %struct.ScmObj** %stackaddr$prim56847, align 8
%stackaddr$prim56848 = alloca %struct.ScmObj*, align 8
%current_45args55436 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55435)
store volatile %struct.ScmObj* %current_45args55436, %struct.ScmObj** %stackaddr$prim56848, align 8
%stackaddr$prim56849 = alloca %struct.ScmObj*, align 8
%anf_45bind47250 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55436)
store volatile %struct.ScmObj* %anf_45bind47250, %struct.ScmObj** %stackaddr$prim56849, align 8
%stackaddr$makeclosure56850 = alloca %struct.ScmObj*, align 8
%fptrToInt56851 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48319 to i64
%ae48319 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt56851)
store volatile %struct.ScmObj* %ae48319, %struct.ScmObj** %stackaddr$makeclosure56850, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48319, %struct.ScmObj* %f47106, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48319, %struct.ScmObj* %acc47105, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48319, %struct.ScmObj* %k47455, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48319, %struct.ScmObj* %_37foldr47102, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48319, %struct.ScmObj* %_37foldr147096, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48319, %struct.ScmObj* %lsts_4347111, i64 5)
%argslist55457$_37map1470920 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56852 = alloca %struct.ScmObj*, align 8
%argslist55457$_37map1470921 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47104, %struct.ScmObj* %argslist55457$_37map1470920)
store volatile %struct.ScmObj* %argslist55457$_37map1470921, %struct.ScmObj** %stackaddr$prim56852, align 8
%stackaddr$prim56853 = alloca %struct.ScmObj*, align 8
%argslist55457$_37map1470922 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47250, %struct.ScmObj* %argslist55457$_37map1470921)
store volatile %struct.ScmObj* %argslist55457$_37map1470922, %struct.ScmObj** %stackaddr$prim56853, align 8
%stackaddr$prim56854 = alloca %struct.ScmObj*, align 8
%argslist55457$_37map1470923 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48319, %struct.ScmObj* %argslist55457$_37map1470922)
store volatile %struct.ScmObj* %argslist55457$_37map1470923, %struct.ScmObj** %stackaddr$prim56854, align 8
%clofunc56855 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147092)
musttail call tailcc void %clofunc56855(%struct.ScmObj* %_37map147092, %struct.ScmObj* %argslist55457$_37map1470923)
ret void
}

define tailcc void @proc_clo$ae48319(%struct.ScmObj* %env$ae48319,%struct.ScmObj* %current_45args55438) {
%stackaddr$env-ref56856 = alloca %struct.ScmObj*, align 8
%f47106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48319, i64 0)
store %struct.ScmObj* %f47106, %struct.ScmObj** %stackaddr$env-ref56856
%stackaddr$env-ref56857 = alloca %struct.ScmObj*, align 8
%acc47105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48319, i64 1)
store %struct.ScmObj* %acc47105, %struct.ScmObj** %stackaddr$env-ref56857
%stackaddr$env-ref56858 = alloca %struct.ScmObj*, align 8
%k47455 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48319, i64 2)
store %struct.ScmObj* %k47455, %struct.ScmObj** %stackaddr$env-ref56858
%stackaddr$env-ref56859 = alloca %struct.ScmObj*, align 8
%_37foldr47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48319, i64 3)
store %struct.ScmObj* %_37foldr47102, %struct.ScmObj** %stackaddr$env-ref56859
%stackaddr$env-ref56860 = alloca %struct.ScmObj*, align 8
%_37foldr147096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48319, i64 4)
store %struct.ScmObj* %_37foldr147096, %struct.ScmObj** %stackaddr$env-ref56860
%stackaddr$env-ref56861 = alloca %struct.ScmObj*, align 8
%lsts_4347111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48319, i64 5)
store %struct.ScmObj* %lsts_4347111, %struct.ScmObj** %stackaddr$env-ref56861
%stackaddr$prim56862 = alloca %struct.ScmObj*, align 8
%_95k47461 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55438)
store volatile %struct.ScmObj* %_95k47461, %struct.ScmObj** %stackaddr$prim56862, align 8
%stackaddr$prim56863 = alloca %struct.ScmObj*, align 8
%current_45args55439 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55438)
store volatile %struct.ScmObj* %current_45args55439, %struct.ScmObj** %stackaddr$prim56863, align 8
%stackaddr$prim56864 = alloca %struct.ScmObj*, align 8
%vs47109 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55439)
store volatile %struct.ScmObj* %vs47109, %struct.ScmObj** %stackaddr$prim56864, align 8
%stackaddr$makeclosure56865 = alloca %struct.ScmObj*, align 8
%fptrToInt56866 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48322 to i64
%ae48322 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56866)
store volatile %struct.ScmObj* %ae48322, %struct.ScmObj** %stackaddr$makeclosure56865, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48322, %struct.ScmObj* %vs47109, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48322, %struct.ScmObj* %f47106, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48322, %struct.ScmObj* %acc47105, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48322, %struct.ScmObj* %k47455, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48322, %struct.ScmObj* %_37foldr47102, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48322, %struct.ScmObj* %_37foldr147096, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48322, %struct.ScmObj* %lsts_4347111, i64 6)
%ae48323 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56867 = alloca %struct.ScmObj*, align 8
%fptrToInt56868 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48324 to i64
%ae48324 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56868)
store volatile %struct.ScmObj* %ae48324, %struct.ScmObj** %stackaddr$makeclosure56867, align 8
%argslist55456$ae483220 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56869 = alloca %struct.ScmObj*, align 8
%argslist55456$ae483221 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48324, %struct.ScmObj* %argslist55456$ae483220)
store volatile %struct.ScmObj* %argslist55456$ae483221, %struct.ScmObj** %stackaddr$prim56869, align 8
%stackaddr$prim56870 = alloca %struct.ScmObj*, align 8
%argslist55456$ae483222 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48323, %struct.ScmObj* %argslist55456$ae483221)
store volatile %struct.ScmObj* %argslist55456$ae483222, %struct.ScmObj** %stackaddr$prim56870, align 8
%clofunc56871 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48322)
musttail call tailcc void %clofunc56871(%struct.ScmObj* %ae48322, %struct.ScmObj* %argslist55456$ae483222)
ret void
}

define tailcc void @proc_clo$ae48322(%struct.ScmObj* %env$ae48322,%struct.ScmObj* %current_45args55441) {
%stackaddr$env-ref56872 = alloca %struct.ScmObj*, align 8
%vs47109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48322, i64 0)
store %struct.ScmObj* %vs47109, %struct.ScmObj** %stackaddr$env-ref56872
%stackaddr$env-ref56873 = alloca %struct.ScmObj*, align 8
%f47106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48322, i64 1)
store %struct.ScmObj* %f47106, %struct.ScmObj** %stackaddr$env-ref56873
%stackaddr$env-ref56874 = alloca %struct.ScmObj*, align 8
%acc47105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48322, i64 2)
store %struct.ScmObj* %acc47105, %struct.ScmObj** %stackaddr$env-ref56874
%stackaddr$env-ref56875 = alloca %struct.ScmObj*, align 8
%k47455 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48322, i64 3)
store %struct.ScmObj* %k47455, %struct.ScmObj** %stackaddr$env-ref56875
%stackaddr$env-ref56876 = alloca %struct.ScmObj*, align 8
%_37foldr47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48322, i64 4)
store %struct.ScmObj* %_37foldr47102, %struct.ScmObj** %stackaddr$env-ref56876
%stackaddr$env-ref56877 = alloca %struct.ScmObj*, align 8
%_37foldr147096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48322, i64 5)
store %struct.ScmObj* %_37foldr147096, %struct.ScmObj** %stackaddr$env-ref56877
%stackaddr$env-ref56878 = alloca %struct.ScmObj*, align 8
%lsts_4347111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48322, i64 6)
store %struct.ScmObj* %lsts_4347111, %struct.ScmObj** %stackaddr$env-ref56878
%stackaddr$prim56879 = alloca %struct.ScmObj*, align 8
%_95k47462 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55441)
store volatile %struct.ScmObj* %_95k47462, %struct.ScmObj** %stackaddr$prim56879, align 8
%stackaddr$prim56880 = alloca %struct.ScmObj*, align 8
%current_45args55442 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55441)
store volatile %struct.ScmObj* %current_45args55442, %struct.ScmObj** %stackaddr$prim56880, align 8
%stackaddr$prim56881 = alloca %struct.ScmObj*, align 8
%anf_45bind47251 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55442)
store volatile %struct.ScmObj* %anf_45bind47251, %struct.ScmObj** %stackaddr$prim56881, align 8
%stackaddr$prim56882 = alloca %struct.ScmObj*, align 8
%anf_45bind47252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47105, %struct.ScmObj* %lsts_4347111)
store volatile %struct.ScmObj* %anf_45bind47252, %struct.ScmObj** %stackaddr$prim56882, align 8
%stackaddr$prim56883 = alloca %struct.ScmObj*, align 8
%anf_45bind47253 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47106, %struct.ScmObj* %anf_45bind47252)
store volatile %struct.ScmObj* %anf_45bind47253, %struct.ScmObj** %stackaddr$prim56883, align 8
%stackaddr$makeclosure56884 = alloca %struct.ScmObj*, align 8
%fptrToInt56885 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48348 to i64
%ae48348 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt56885)
store volatile %struct.ScmObj* %ae48348, %struct.ScmObj** %stackaddr$makeclosure56884, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48348, %struct.ScmObj* %vs47109, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48348, %struct.ScmObj* %anf_45bind47251, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48348, %struct.ScmObj* %f47106, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48348, %struct.ScmObj* %k47455, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48348, %struct.ScmObj* %_37foldr147096, i64 4)
%stackaddr$prim56886 = alloca %struct.ScmObj*, align 8
%cpsargs47466 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48348, %struct.ScmObj* %anf_45bind47253)
store volatile %struct.ScmObj* %cpsargs47466, %struct.ScmObj** %stackaddr$prim56886, align 8
%clofunc56887 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr47102)
musttail call tailcc void %clofunc56887(%struct.ScmObj* %_37foldr47102, %struct.ScmObj* %cpsargs47466)
ret void
}

define tailcc void @proc_clo$ae48348(%struct.ScmObj* %env$ae48348,%struct.ScmObj* %current_45args55444) {
%stackaddr$env-ref56888 = alloca %struct.ScmObj*, align 8
%vs47109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48348, i64 0)
store %struct.ScmObj* %vs47109, %struct.ScmObj** %stackaddr$env-ref56888
%stackaddr$env-ref56889 = alloca %struct.ScmObj*, align 8
%anf_45bind47251 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48348, i64 1)
store %struct.ScmObj* %anf_45bind47251, %struct.ScmObj** %stackaddr$env-ref56889
%stackaddr$env-ref56890 = alloca %struct.ScmObj*, align 8
%f47106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48348, i64 2)
store %struct.ScmObj* %f47106, %struct.ScmObj** %stackaddr$env-ref56890
%stackaddr$env-ref56891 = alloca %struct.ScmObj*, align 8
%k47455 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48348, i64 3)
store %struct.ScmObj* %k47455, %struct.ScmObj** %stackaddr$env-ref56891
%stackaddr$env-ref56892 = alloca %struct.ScmObj*, align 8
%_37foldr147096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48348, i64 4)
store %struct.ScmObj* %_37foldr147096, %struct.ScmObj** %stackaddr$env-ref56892
%stackaddr$prim56893 = alloca %struct.ScmObj*, align 8
%_95k47463 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55444)
store volatile %struct.ScmObj* %_95k47463, %struct.ScmObj** %stackaddr$prim56893, align 8
%stackaddr$prim56894 = alloca %struct.ScmObj*, align 8
%current_45args55445 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55444)
store volatile %struct.ScmObj* %current_45args55445, %struct.ScmObj** %stackaddr$prim56894, align 8
%stackaddr$prim56895 = alloca %struct.ScmObj*, align 8
%anf_45bind47254 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55445)
store volatile %struct.ScmObj* %anf_45bind47254, %struct.ScmObj** %stackaddr$prim56895, align 8
%ae48353 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56896 = alloca %struct.ScmObj*, align 8
%anf_45bind47255 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47254, %struct.ScmObj* %ae48353)
store volatile %struct.ScmObj* %anf_45bind47255, %struct.ScmObj** %stackaddr$prim56896, align 8
%stackaddr$makeclosure56897 = alloca %struct.ScmObj*, align 8
%fptrToInt56898 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48355 to i64
%ae48355 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56898)
store volatile %struct.ScmObj* %ae48355, %struct.ScmObj** %stackaddr$makeclosure56897, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48355, %struct.ScmObj* %f47106, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48355, %struct.ScmObj* %k47455, i64 1)
%argslist55450$_37foldr1470960 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56899 = alloca %struct.ScmObj*, align 8
%argslist55450$_37foldr1470961 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs47109, %struct.ScmObj* %argslist55450$_37foldr1470960)
store volatile %struct.ScmObj* %argslist55450$_37foldr1470961, %struct.ScmObj** %stackaddr$prim56899, align 8
%stackaddr$prim56900 = alloca %struct.ScmObj*, align 8
%argslist55450$_37foldr1470962 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47255, %struct.ScmObj* %argslist55450$_37foldr1470961)
store volatile %struct.ScmObj* %argslist55450$_37foldr1470962, %struct.ScmObj** %stackaddr$prim56900, align 8
%stackaddr$prim56901 = alloca %struct.ScmObj*, align 8
%argslist55450$_37foldr1470963 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47251, %struct.ScmObj* %argslist55450$_37foldr1470962)
store volatile %struct.ScmObj* %argslist55450$_37foldr1470963, %struct.ScmObj** %stackaddr$prim56901, align 8
%stackaddr$prim56902 = alloca %struct.ScmObj*, align 8
%argslist55450$_37foldr1470964 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48355, %struct.ScmObj* %argslist55450$_37foldr1470963)
store volatile %struct.ScmObj* %argslist55450$_37foldr1470964, %struct.ScmObj** %stackaddr$prim56902, align 8
%clofunc56903 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147096)
musttail call tailcc void %clofunc56903(%struct.ScmObj* %_37foldr147096, %struct.ScmObj* %argslist55450$_37foldr1470964)
ret void
}

define tailcc void @proc_clo$ae48355(%struct.ScmObj* %env$ae48355,%struct.ScmObj* %current_45args55447) {
%stackaddr$env-ref56904 = alloca %struct.ScmObj*, align 8
%f47106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48355, i64 0)
store %struct.ScmObj* %f47106, %struct.ScmObj** %stackaddr$env-ref56904
%stackaddr$env-ref56905 = alloca %struct.ScmObj*, align 8
%k47455 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48355, i64 1)
store %struct.ScmObj* %k47455, %struct.ScmObj** %stackaddr$env-ref56905
%stackaddr$prim56906 = alloca %struct.ScmObj*, align 8
%_95k47464 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55447)
store volatile %struct.ScmObj* %_95k47464, %struct.ScmObj** %stackaddr$prim56906, align 8
%stackaddr$prim56907 = alloca %struct.ScmObj*, align 8
%current_45args55448 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55447)
store volatile %struct.ScmObj* %current_45args55448, %struct.ScmObj** %stackaddr$prim56907, align 8
%stackaddr$prim56908 = alloca %struct.ScmObj*, align 8
%anf_45bind47256 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55448)
store volatile %struct.ScmObj* %anf_45bind47256, %struct.ScmObj** %stackaddr$prim56908, align 8
%stackaddr$prim56909 = alloca %struct.ScmObj*, align 8
%cpsargs47465 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47455, %struct.ScmObj* %anf_45bind47256)
store volatile %struct.ScmObj* %cpsargs47465, %struct.ScmObj** %stackaddr$prim56909, align 8
%clofunc56910 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47106)
musttail call tailcc void %clofunc56910(%struct.ScmObj* %f47106, %struct.ScmObj* %cpsargs47465)
ret void
}

define tailcc void @proc_clo$ae48324(%struct.ScmObj* %env$ae48324,%struct.ScmObj* %current_45args55451) {
%stackaddr$prim56911 = alloca %struct.ScmObj*, align 8
%k47467 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55451)
store volatile %struct.ScmObj* %k47467, %struct.ScmObj** %stackaddr$prim56911, align 8
%stackaddr$prim56912 = alloca %struct.ScmObj*, align 8
%current_45args55452 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55451)
store volatile %struct.ScmObj* %current_45args55452, %struct.ScmObj** %stackaddr$prim56912, align 8
%stackaddr$prim56913 = alloca %struct.ScmObj*, align 8
%a47114 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55452)
store volatile %struct.ScmObj* %a47114, %struct.ScmObj** %stackaddr$prim56913, align 8
%stackaddr$prim56914 = alloca %struct.ScmObj*, align 8
%current_45args55453 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55452)
store volatile %struct.ScmObj* %current_45args55453, %struct.ScmObj** %stackaddr$prim56914, align 8
%stackaddr$prim56915 = alloca %struct.ScmObj*, align 8
%b47113 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55453)
store volatile %struct.ScmObj* %b47113, %struct.ScmObj** %stackaddr$prim56915, align 8
%stackaddr$prim56916 = alloca %struct.ScmObj*, align 8
%cpsprim47468 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a47114, %struct.ScmObj* %b47113)
store volatile %struct.ScmObj* %cpsprim47468, %struct.ScmObj** %stackaddr$prim56916, align 8
%ae48328 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55455$k474670 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56917 = alloca %struct.ScmObj*, align 8
%argslist55455$k474671 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47468, %struct.ScmObj* %argslist55455$k474670)
store volatile %struct.ScmObj* %argslist55455$k474671, %struct.ScmObj** %stackaddr$prim56917, align 8
%stackaddr$prim56918 = alloca %struct.ScmObj*, align 8
%argslist55455$k474672 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48328, %struct.ScmObj* %argslist55455$k474671)
store volatile %struct.ScmObj* %argslist55455$k474672, %struct.ScmObj** %stackaddr$prim56918, align 8
%clofunc56919 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47467)
musttail call tailcc void %clofunc56919(%struct.ScmObj* %k47467, %struct.ScmObj* %argslist55455$k474672)
ret void
}

define tailcc void @proc_clo$ae48300(%struct.ScmObj* %env$ae48300,%struct.ScmObj* %current_45args55458) {
%stackaddr$prim56920 = alloca %struct.ScmObj*, align 8
%k47469 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55458)
store volatile %struct.ScmObj* %k47469, %struct.ScmObj** %stackaddr$prim56920, align 8
%stackaddr$prim56921 = alloca %struct.ScmObj*, align 8
%current_45args55459 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55458)
store volatile %struct.ScmObj* %current_45args55459, %struct.ScmObj** %stackaddr$prim56921, align 8
%stackaddr$prim56922 = alloca %struct.ScmObj*, align 8
%x47110 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55459)
store volatile %struct.ScmObj* %x47110, %struct.ScmObj** %stackaddr$prim56922, align 8
%stackaddr$prim56923 = alloca %struct.ScmObj*, align 8
%cpsprim47470 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47110)
store volatile %struct.ScmObj* %cpsprim47470, %struct.ScmObj** %stackaddr$prim56923, align 8
%ae48303 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55461$k474690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56924 = alloca %struct.ScmObj*, align 8
%argslist55461$k474691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47470, %struct.ScmObj* %argslist55461$k474690)
store volatile %struct.ScmObj* %argslist55461$k474691, %struct.ScmObj** %stackaddr$prim56924, align 8
%stackaddr$prim56925 = alloca %struct.ScmObj*, align 8
%argslist55461$k474692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48303, %struct.ScmObj* %argslist55461$k474691)
store volatile %struct.ScmObj* %argslist55461$k474692, %struct.ScmObj** %stackaddr$prim56925, align 8
%clofunc56926 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47469)
musttail call tailcc void %clofunc56926(%struct.ScmObj* %k47469, %struct.ScmObj* %argslist55461$k474692)
ret void
}

define tailcc void @proc_clo$ae48276(%struct.ScmObj* %env$ae48276,%struct.ScmObj* %current_45args55464) {
%stackaddr$prim56927 = alloca %struct.ScmObj*, align 8
%k47471 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55464)
store volatile %struct.ScmObj* %k47471, %struct.ScmObj** %stackaddr$prim56927, align 8
%stackaddr$prim56928 = alloca %struct.ScmObj*, align 8
%current_45args55465 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55464)
store volatile %struct.ScmObj* %current_45args55465, %struct.ScmObj** %stackaddr$prim56928, align 8
%stackaddr$prim56929 = alloca %struct.ScmObj*, align 8
%x47112 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55465)
store volatile %struct.ScmObj* %x47112, %struct.ScmObj** %stackaddr$prim56929, align 8
%stackaddr$prim56930 = alloca %struct.ScmObj*, align 8
%cpsprim47472 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47112)
store volatile %struct.ScmObj* %cpsprim47472, %struct.ScmObj** %stackaddr$prim56930, align 8
%ae48279 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55467$k474710 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56931 = alloca %struct.ScmObj*, align 8
%argslist55467$k474711 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47472, %struct.ScmObj* %argslist55467$k474710)
store volatile %struct.ScmObj* %argslist55467$k474711, %struct.ScmObj** %stackaddr$prim56931, align 8
%stackaddr$prim56932 = alloca %struct.ScmObj*, align 8
%argslist55467$k474712 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48279, %struct.ScmObj* %argslist55467$k474711)
store volatile %struct.ScmObj* %argslist55467$k474712, %struct.ScmObj** %stackaddr$prim56932, align 8
%clofunc56933 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47471)
musttail call tailcc void %clofunc56933(%struct.ScmObj* %k47471, %struct.ScmObj* %argslist55467$k474712)
ret void
}

define tailcc void @proc_clo$ae48228(%struct.ScmObj* %env$ae48228,%struct.ScmObj* %current_45args55470) {
%stackaddr$prim56934 = alloca %struct.ScmObj*, align 8
%k47473 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55470)
store volatile %struct.ScmObj* %k47473, %struct.ScmObj** %stackaddr$prim56934, align 8
%stackaddr$prim56935 = alloca %struct.ScmObj*, align 8
%current_45args55471 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55470)
store volatile %struct.ScmObj* %current_45args55471, %struct.ScmObj** %stackaddr$prim56935, align 8
%stackaddr$prim56936 = alloca %struct.ScmObj*, align 8
%lst47108 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55471)
store volatile %struct.ScmObj* %lst47108, %struct.ScmObj** %stackaddr$prim56936, align 8
%stackaddr$prim56937 = alloca %struct.ScmObj*, align 8
%current_45args55472 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55471)
store volatile %struct.ScmObj* %current_45args55472, %struct.ScmObj** %stackaddr$prim56937, align 8
%stackaddr$prim56938 = alloca %struct.ScmObj*, align 8
%b47107 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55472)
store volatile %struct.ScmObj* %b47107, %struct.ScmObj** %stackaddr$prim56938, align 8
%truthy$cmp56939 = call i64 @is_truthy_value(%struct.ScmObj* %b47107)
%cmp$cmp56939 = icmp eq i64 %truthy$cmp56939, 1
br i1 %cmp$cmp56939, label %truebranch$cmp56939, label %falsebranch$cmp56939
truebranch$cmp56939:
%ae48231 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55474$k474730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56940 = alloca %struct.ScmObj*, align 8
%argslist55474$k474731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b47107, %struct.ScmObj* %argslist55474$k474730)
store volatile %struct.ScmObj* %argslist55474$k474731, %struct.ScmObj** %stackaddr$prim56940, align 8
%stackaddr$prim56941 = alloca %struct.ScmObj*, align 8
%argslist55474$k474732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48231, %struct.ScmObj* %argslist55474$k474731)
store volatile %struct.ScmObj* %argslist55474$k474732, %struct.ScmObj** %stackaddr$prim56941, align 8
%clofunc56942 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47473)
musttail call tailcc void %clofunc56942(%struct.ScmObj* %k47473, %struct.ScmObj* %argslist55474$k474732)
ret void
falsebranch$cmp56939:
%stackaddr$prim56943 = alloca %struct.ScmObj*, align 8
%cpsprim47474 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47108)
store volatile %struct.ScmObj* %cpsprim47474, %struct.ScmObj** %stackaddr$prim56943, align 8
%ae48238 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55475$k474730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56944 = alloca %struct.ScmObj*, align 8
%argslist55475$k474731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47474, %struct.ScmObj* %argslist55475$k474730)
store volatile %struct.ScmObj* %argslist55475$k474731, %struct.ScmObj** %stackaddr$prim56944, align 8
%stackaddr$prim56945 = alloca %struct.ScmObj*, align 8
%argslist55475$k474732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48238, %struct.ScmObj* %argslist55475$k474731)
store volatile %struct.ScmObj* %argslist55475$k474732, %struct.ScmObj** %stackaddr$prim56945, align 8
%clofunc56946 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47473)
musttail call tailcc void %clofunc56946(%struct.ScmObj* %k47473, %struct.ScmObj* %argslist55475$k474732)
ret void
}

define tailcc void @proc_clo$ae48185(%struct.ScmObj* %env$ae48185,%struct.ScmObj* %current_45args55479) {
%stackaddr$env-ref56947 = alloca %struct.ScmObj*, align 8
%_37take47088 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48185, i64 0)
store %struct.ScmObj* %_37take47088, %struct.ScmObj** %stackaddr$env-ref56947
%stackaddr$env-ref56948 = alloca %struct.ScmObj*, align 8
%_37length47085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48185, i64 1)
store %struct.ScmObj* %_37length47085, %struct.ScmObj** %stackaddr$env-ref56948
%stackaddr$prim56949 = alloca %struct.ScmObj*, align 8
%k47475 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55479)
store volatile %struct.ScmObj* %k47475, %struct.ScmObj** %stackaddr$prim56949, align 8
%stackaddr$prim56950 = alloca %struct.ScmObj*, align 8
%current_45args55480 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55479)
store volatile %struct.ScmObj* %current_45args55480, %struct.ScmObj** %stackaddr$prim56950, align 8
%stackaddr$prim56951 = alloca %struct.ScmObj*, align 8
%lst47117 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55480)
store volatile %struct.ScmObj* %lst47117, %struct.ScmObj** %stackaddr$prim56951, align 8
%stackaddr$prim56952 = alloca %struct.ScmObj*, align 8
%current_45args55481 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55480)
store volatile %struct.ScmObj* %current_45args55481, %struct.ScmObj** %stackaddr$prim56952, align 8
%stackaddr$prim56953 = alloca %struct.ScmObj*, align 8
%n47116 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55481)
store volatile %struct.ScmObj* %n47116, %struct.ScmObj** %stackaddr$prim56953, align 8
%stackaddr$makeclosure56954 = alloca %struct.ScmObj*, align 8
%fptrToInt56955 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48187 to i64
%ae48187 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56955)
store volatile %struct.ScmObj* %ae48187, %struct.ScmObj** %stackaddr$makeclosure56954, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48187, %struct.ScmObj* %k47475, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48187, %struct.ScmObj* %_37take47088, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48187, %struct.ScmObj* %lst47117, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48187, %struct.ScmObj* %n47116, i64 3)
%argslist55487$_37length470850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56956 = alloca %struct.ScmObj*, align 8
%argslist55487$_37length470851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47117, %struct.ScmObj* %argslist55487$_37length470850)
store volatile %struct.ScmObj* %argslist55487$_37length470851, %struct.ScmObj** %stackaddr$prim56956, align 8
%stackaddr$prim56957 = alloca %struct.ScmObj*, align 8
%argslist55487$_37length470852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48187, %struct.ScmObj* %argslist55487$_37length470851)
store volatile %struct.ScmObj* %argslist55487$_37length470852, %struct.ScmObj** %stackaddr$prim56957, align 8
%clofunc56958 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length47085)
musttail call tailcc void %clofunc56958(%struct.ScmObj* %_37length47085, %struct.ScmObj* %argslist55487$_37length470852)
ret void
}

define tailcc void @proc_clo$ae48187(%struct.ScmObj* %env$ae48187,%struct.ScmObj* %current_45args55483) {
%stackaddr$env-ref56959 = alloca %struct.ScmObj*, align 8
%k47475 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48187, i64 0)
store %struct.ScmObj* %k47475, %struct.ScmObj** %stackaddr$env-ref56959
%stackaddr$env-ref56960 = alloca %struct.ScmObj*, align 8
%_37take47088 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48187, i64 1)
store %struct.ScmObj* %_37take47088, %struct.ScmObj** %stackaddr$env-ref56960
%stackaddr$env-ref56961 = alloca %struct.ScmObj*, align 8
%lst47117 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48187, i64 2)
store %struct.ScmObj* %lst47117, %struct.ScmObj** %stackaddr$env-ref56961
%stackaddr$env-ref56962 = alloca %struct.ScmObj*, align 8
%n47116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48187, i64 3)
store %struct.ScmObj* %n47116, %struct.ScmObj** %stackaddr$env-ref56962
%stackaddr$prim56963 = alloca %struct.ScmObj*, align 8
%_95k47476 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55483)
store volatile %struct.ScmObj* %_95k47476, %struct.ScmObj** %stackaddr$prim56963, align 8
%stackaddr$prim56964 = alloca %struct.ScmObj*, align 8
%current_45args55484 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55483)
store volatile %struct.ScmObj* %current_45args55484, %struct.ScmObj** %stackaddr$prim56964, align 8
%stackaddr$prim56965 = alloca %struct.ScmObj*, align 8
%anf_45bind47243 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55484)
store volatile %struct.ScmObj* %anf_45bind47243, %struct.ScmObj** %stackaddr$prim56965, align 8
%stackaddr$prim56966 = alloca %struct.ScmObj*, align 8
%anf_45bind47244 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind47243, %struct.ScmObj* %n47116)
store volatile %struct.ScmObj* %anf_45bind47244, %struct.ScmObj** %stackaddr$prim56966, align 8
%argslist55486$_37take470880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56967 = alloca %struct.ScmObj*, align 8
%argslist55486$_37take470881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47244, %struct.ScmObj* %argslist55486$_37take470880)
store volatile %struct.ScmObj* %argslist55486$_37take470881, %struct.ScmObj** %stackaddr$prim56967, align 8
%stackaddr$prim56968 = alloca %struct.ScmObj*, align 8
%argslist55486$_37take470882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47117, %struct.ScmObj* %argslist55486$_37take470881)
store volatile %struct.ScmObj* %argslist55486$_37take470882, %struct.ScmObj** %stackaddr$prim56968, align 8
%stackaddr$prim56969 = alloca %struct.ScmObj*, align 8
%argslist55486$_37take470883 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47475, %struct.ScmObj* %argslist55486$_37take470882)
store volatile %struct.ScmObj* %argslist55486$_37take470883, %struct.ScmObj** %stackaddr$prim56969, align 8
%clofunc56970 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take47088)
musttail call tailcc void %clofunc56970(%struct.ScmObj* %_37take47088, %struct.ScmObj* %argslist55486$_37take470883)
ret void
}

define tailcc void @proc_clo$ae48131(%struct.ScmObj* %env$ae48131,%struct.ScmObj* %current_45args55489) {
%stackaddr$env-ref56971 = alloca %struct.ScmObj*, align 8
%_37foldl147080 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48131, i64 0)
store %struct.ScmObj* %_37foldl147080, %struct.ScmObj** %stackaddr$env-ref56971
%stackaddr$prim56972 = alloca %struct.ScmObj*, align 8
%k47477 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55489)
store volatile %struct.ScmObj* %k47477, %struct.ScmObj** %stackaddr$prim56972, align 8
%stackaddr$prim56973 = alloca %struct.ScmObj*, align 8
%current_45args55490 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55489)
store volatile %struct.ScmObj* %current_45args55490, %struct.ScmObj** %stackaddr$prim56973, align 8
%stackaddr$prim56974 = alloca %struct.ScmObj*, align 8
%lst47119 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55490)
store volatile %struct.ScmObj* %lst47119, %struct.ScmObj** %stackaddr$prim56974, align 8
%stackaddr$makeclosure56975 = alloca %struct.ScmObj*, align 8
%fptrToInt56976 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48132 to i64
%ae48132 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56976)
store volatile %struct.ScmObj* %ae48132, %struct.ScmObj** %stackaddr$makeclosure56975, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48132, %struct.ScmObj* %k47477, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48132, %struct.ScmObj* %lst47119, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48132, %struct.ScmObj* %_37foldl147080, i64 2)
%ae48133 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56977 = alloca %struct.ScmObj*, align 8
%fptrToInt56978 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48134 to i64
%ae48134 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56978)
store volatile %struct.ScmObj* %ae48134, %struct.ScmObj** %stackaddr$makeclosure56977, align 8
%argslist55501$ae481320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56979 = alloca %struct.ScmObj*, align 8
%argslist55501$ae481321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48134, %struct.ScmObj* %argslist55501$ae481320)
store volatile %struct.ScmObj* %argslist55501$ae481321, %struct.ScmObj** %stackaddr$prim56979, align 8
%stackaddr$prim56980 = alloca %struct.ScmObj*, align 8
%argslist55501$ae481322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48133, %struct.ScmObj* %argslist55501$ae481321)
store volatile %struct.ScmObj* %argslist55501$ae481322, %struct.ScmObj** %stackaddr$prim56980, align 8
%clofunc56981 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48132)
musttail call tailcc void %clofunc56981(%struct.ScmObj* %ae48132, %struct.ScmObj* %argslist55501$ae481322)
ret void
}

define tailcc void @proc_clo$ae48132(%struct.ScmObj* %env$ae48132,%struct.ScmObj* %current_45args55492) {
%stackaddr$env-ref56982 = alloca %struct.ScmObj*, align 8
%k47477 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48132, i64 0)
store %struct.ScmObj* %k47477, %struct.ScmObj** %stackaddr$env-ref56982
%stackaddr$env-ref56983 = alloca %struct.ScmObj*, align 8
%lst47119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48132, i64 1)
store %struct.ScmObj* %lst47119, %struct.ScmObj** %stackaddr$env-ref56983
%stackaddr$env-ref56984 = alloca %struct.ScmObj*, align 8
%_37foldl147080 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48132, i64 2)
store %struct.ScmObj* %_37foldl147080, %struct.ScmObj** %stackaddr$env-ref56984
%stackaddr$prim56985 = alloca %struct.ScmObj*, align 8
%_95k47478 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55492)
store volatile %struct.ScmObj* %_95k47478, %struct.ScmObj** %stackaddr$prim56985, align 8
%stackaddr$prim56986 = alloca %struct.ScmObj*, align 8
%current_45args55493 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55492)
store volatile %struct.ScmObj* %current_45args55493, %struct.ScmObj** %stackaddr$prim56986, align 8
%stackaddr$prim56987 = alloca %struct.ScmObj*, align 8
%anf_45bind47242 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55493)
store volatile %struct.ScmObj* %anf_45bind47242, %struct.ScmObj** %stackaddr$prim56987, align 8
%ae48153 = call %struct.ScmObj* @const_init_null()
%argslist55495$_37foldl1470800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56988 = alloca %struct.ScmObj*, align 8
%argslist55495$_37foldl1470801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47119, %struct.ScmObj* %argslist55495$_37foldl1470800)
store volatile %struct.ScmObj* %argslist55495$_37foldl1470801, %struct.ScmObj** %stackaddr$prim56988, align 8
%stackaddr$prim56989 = alloca %struct.ScmObj*, align 8
%argslist55495$_37foldl1470802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48153, %struct.ScmObj* %argslist55495$_37foldl1470801)
store volatile %struct.ScmObj* %argslist55495$_37foldl1470802, %struct.ScmObj** %stackaddr$prim56989, align 8
%stackaddr$prim56990 = alloca %struct.ScmObj*, align 8
%argslist55495$_37foldl1470803 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47242, %struct.ScmObj* %argslist55495$_37foldl1470802)
store volatile %struct.ScmObj* %argslist55495$_37foldl1470803, %struct.ScmObj** %stackaddr$prim56990, align 8
%stackaddr$prim56991 = alloca %struct.ScmObj*, align 8
%argslist55495$_37foldl1470804 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47477, %struct.ScmObj* %argslist55495$_37foldl1470803)
store volatile %struct.ScmObj* %argslist55495$_37foldl1470804, %struct.ScmObj** %stackaddr$prim56991, align 8
%clofunc56992 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147080)
musttail call tailcc void %clofunc56992(%struct.ScmObj* %_37foldl147080, %struct.ScmObj* %argslist55495$_37foldl1470804)
ret void
}

define tailcc void @proc_clo$ae48134(%struct.ScmObj* %env$ae48134,%struct.ScmObj* %current_45args55496) {
%stackaddr$prim56993 = alloca %struct.ScmObj*, align 8
%k47479 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55496)
store volatile %struct.ScmObj* %k47479, %struct.ScmObj** %stackaddr$prim56993, align 8
%stackaddr$prim56994 = alloca %struct.ScmObj*, align 8
%current_45args55497 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55496)
store volatile %struct.ScmObj* %current_45args55497, %struct.ScmObj** %stackaddr$prim56994, align 8
%stackaddr$prim56995 = alloca %struct.ScmObj*, align 8
%x47121 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55497)
store volatile %struct.ScmObj* %x47121, %struct.ScmObj** %stackaddr$prim56995, align 8
%stackaddr$prim56996 = alloca %struct.ScmObj*, align 8
%current_45args55498 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55497)
store volatile %struct.ScmObj* %current_45args55498, %struct.ScmObj** %stackaddr$prim56996, align 8
%stackaddr$prim56997 = alloca %struct.ScmObj*, align 8
%y47120 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55498)
store volatile %struct.ScmObj* %y47120, %struct.ScmObj** %stackaddr$prim56997, align 8
%ae48136 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55500$k474790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56998 = alloca %struct.ScmObj*, align 8
%argslist55500$k474791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x47121, %struct.ScmObj* %argslist55500$k474790)
store volatile %struct.ScmObj* %argslist55500$k474791, %struct.ScmObj** %stackaddr$prim56998, align 8
%stackaddr$prim56999 = alloca %struct.ScmObj*, align 8
%argslist55500$k474792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48136, %struct.ScmObj* %argslist55500$k474791)
store volatile %struct.ScmObj* %argslist55500$k474792, %struct.ScmObj** %stackaddr$prim56999, align 8
%clofunc57000 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47479)
musttail call tailcc void %clofunc57000(%struct.ScmObj* %k47479, %struct.ScmObj* %argslist55500$k474792)
ret void
}

define tailcc void @proc_clo$ae48052(%struct.ScmObj* %env$ae48052,%struct.ScmObj* %current_45args55504) {
%stackaddr$prim57001 = alloca %struct.ScmObj*, align 8
%k47480 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55504)
store volatile %struct.ScmObj* %k47480, %struct.ScmObj** %stackaddr$prim57001, align 8
%stackaddr$prim57002 = alloca %struct.ScmObj*, align 8
%current_45args55505 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55504)
store volatile %struct.ScmObj* %current_45args55505, %struct.ScmObj** %stackaddr$prim57002, align 8
%stackaddr$prim57003 = alloca %struct.ScmObj*, align 8
%_37foldl147081 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55505)
store volatile %struct.ScmObj* %_37foldl147081, %struct.ScmObj** %stackaddr$prim57003, align 8
%ae48054 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57004 = alloca %struct.ScmObj*, align 8
%fptrToInt57005 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48055 to i64
%ae48055 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57005)
store volatile %struct.ScmObj* %ae48055, %struct.ScmObj** %stackaddr$makeclosure57004, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48055, %struct.ScmObj* %_37foldl147081, i64 0)
%argslist55518$k474800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57006 = alloca %struct.ScmObj*, align 8
%argslist55518$k474801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48055, %struct.ScmObj* %argslist55518$k474800)
store volatile %struct.ScmObj* %argslist55518$k474801, %struct.ScmObj** %stackaddr$prim57006, align 8
%stackaddr$prim57007 = alloca %struct.ScmObj*, align 8
%argslist55518$k474802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48054, %struct.ScmObj* %argslist55518$k474801)
store volatile %struct.ScmObj* %argslist55518$k474802, %struct.ScmObj** %stackaddr$prim57007, align 8
%clofunc57008 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47480)
musttail call tailcc void %clofunc57008(%struct.ScmObj* %k47480, %struct.ScmObj* %argslist55518$k474802)
ret void
}

define tailcc void @proc_clo$ae48055(%struct.ScmObj* %env$ae48055,%struct.ScmObj* %current_45args55507) {
%stackaddr$env-ref57009 = alloca %struct.ScmObj*, align 8
%_37foldl147081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48055, i64 0)
store %struct.ScmObj* %_37foldl147081, %struct.ScmObj** %stackaddr$env-ref57009
%stackaddr$prim57010 = alloca %struct.ScmObj*, align 8
%k47481 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55507)
store volatile %struct.ScmObj* %k47481, %struct.ScmObj** %stackaddr$prim57010, align 8
%stackaddr$prim57011 = alloca %struct.ScmObj*, align 8
%current_45args55508 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55507)
store volatile %struct.ScmObj* %current_45args55508, %struct.ScmObj** %stackaddr$prim57011, align 8
%stackaddr$prim57012 = alloca %struct.ScmObj*, align 8
%f47084 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55508)
store volatile %struct.ScmObj* %f47084, %struct.ScmObj** %stackaddr$prim57012, align 8
%stackaddr$prim57013 = alloca %struct.ScmObj*, align 8
%current_45args55509 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55508)
store volatile %struct.ScmObj* %current_45args55509, %struct.ScmObj** %stackaddr$prim57013, align 8
%stackaddr$prim57014 = alloca %struct.ScmObj*, align 8
%acc47083 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55509)
store volatile %struct.ScmObj* %acc47083, %struct.ScmObj** %stackaddr$prim57014, align 8
%stackaddr$prim57015 = alloca %struct.ScmObj*, align 8
%current_45args55510 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55509)
store volatile %struct.ScmObj* %current_45args55510, %struct.ScmObj** %stackaddr$prim57015, align 8
%stackaddr$prim57016 = alloca %struct.ScmObj*, align 8
%lst47082 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55510)
store volatile %struct.ScmObj* %lst47082, %struct.ScmObj** %stackaddr$prim57016, align 8
%stackaddr$prim57017 = alloca %struct.ScmObj*, align 8
%anf_45bind47237 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47082)
store volatile %struct.ScmObj* %anf_45bind47237, %struct.ScmObj** %stackaddr$prim57017, align 8
%truthy$cmp57018 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47237)
%cmp$cmp57018 = icmp eq i64 %truthy$cmp57018, 1
br i1 %cmp$cmp57018, label %truebranch$cmp57018, label %falsebranch$cmp57018
truebranch$cmp57018:
%ae48059 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55512$k474810 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57019 = alloca %struct.ScmObj*, align 8
%argslist55512$k474811 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47083, %struct.ScmObj* %argslist55512$k474810)
store volatile %struct.ScmObj* %argslist55512$k474811, %struct.ScmObj** %stackaddr$prim57019, align 8
%stackaddr$prim57020 = alloca %struct.ScmObj*, align 8
%argslist55512$k474812 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48059, %struct.ScmObj* %argslist55512$k474811)
store volatile %struct.ScmObj* %argslist55512$k474812, %struct.ScmObj** %stackaddr$prim57020, align 8
%clofunc57021 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47481)
musttail call tailcc void %clofunc57021(%struct.ScmObj* %k47481, %struct.ScmObj* %argslist55512$k474812)
ret void
falsebranch$cmp57018:
%stackaddr$prim57022 = alloca %struct.ScmObj*, align 8
%anf_45bind47238 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47082)
store volatile %struct.ScmObj* %anf_45bind47238, %struct.ScmObj** %stackaddr$prim57022, align 8
%stackaddr$makeclosure57023 = alloca %struct.ScmObj*, align 8
%fptrToInt57024 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48066 to i64
%ae48066 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57024)
store volatile %struct.ScmObj* %ae48066, %struct.ScmObj** %stackaddr$makeclosure57023, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48066, %struct.ScmObj* %k47481, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48066, %struct.ScmObj* %_37foldl147081, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48066, %struct.ScmObj* %f47084, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48066, %struct.ScmObj* %lst47082, i64 3)
%argslist55517$f470840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57025 = alloca %struct.ScmObj*, align 8
%argslist55517$f470841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47083, %struct.ScmObj* %argslist55517$f470840)
store volatile %struct.ScmObj* %argslist55517$f470841, %struct.ScmObj** %stackaddr$prim57025, align 8
%stackaddr$prim57026 = alloca %struct.ScmObj*, align 8
%argslist55517$f470842 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47238, %struct.ScmObj* %argslist55517$f470841)
store volatile %struct.ScmObj* %argslist55517$f470842, %struct.ScmObj** %stackaddr$prim57026, align 8
%stackaddr$prim57027 = alloca %struct.ScmObj*, align 8
%argslist55517$f470843 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48066, %struct.ScmObj* %argslist55517$f470842)
store volatile %struct.ScmObj* %argslist55517$f470843, %struct.ScmObj** %stackaddr$prim57027, align 8
%clofunc57028 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47084)
musttail call tailcc void %clofunc57028(%struct.ScmObj* %f47084, %struct.ScmObj* %argslist55517$f470843)
ret void
}

define tailcc void @proc_clo$ae48066(%struct.ScmObj* %env$ae48066,%struct.ScmObj* %current_45args55513) {
%stackaddr$env-ref57029 = alloca %struct.ScmObj*, align 8
%k47481 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48066, i64 0)
store %struct.ScmObj* %k47481, %struct.ScmObj** %stackaddr$env-ref57029
%stackaddr$env-ref57030 = alloca %struct.ScmObj*, align 8
%_37foldl147081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48066, i64 1)
store %struct.ScmObj* %_37foldl147081, %struct.ScmObj** %stackaddr$env-ref57030
%stackaddr$env-ref57031 = alloca %struct.ScmObj*, align 8
%f47084 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48066, i64 2)
store %struct.ScmObj* %f47084, %struct.ScmObj** %stackaddr$env-ref57031
%stackaddr$env-ref57032 = alloca %struct.ScmObj*, align 8
%lst47082 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48066, i64 3)
store %struct.ScmObj* %lst47082, %struct.ScmObj** %stackaddr$env-ref57032
%stackaddr$prim57033 = alloca %struct.ScmObj*, align 8
%_95k47482 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55513)
store volatile %struct.ScmObj* %_95k47482, %struct.ScmObj** %stackaddr$prim57033, align 8
%stackaddr$prim57034 = alloca %struct.ScmObj*, align 8
%current_45args55514 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55513)
store volatile %struct.ScmObj* %current_45args55514, %struct.ScmObj** %stackaddr$prim57034, align 8
%stackaddr$prim57035 = alloca %struct.ScmObj*, align 8
%anf_45bind47239 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55514)
store volatile %struct.ScmObj* %anf_45bind47239, %struct.ScmObj** %stackaddr$prim57035, align 8
%stackaddr$prim57036 = alloca %struct.ScmObj*, align 8
%anf_45bind47240 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47082)
store volatile %struct.ScmObj* %anf_45bind47240, %struct.ScmObj** %stackaddr$prim57036, align 8
%argslist55516$_37foldl1470810 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57037 = alloca %struct.ScmObj*, align 8
%argslist55516$_37foldl1470811 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47240, %struct.ScmObj* %argslist55516$_37foldl1470810)
store volatile %struct.ScmObj* %argslist55516$_37foldl1470811, %struct.ScmObj** %stackaddr$prim57037, align 8
%stackaddr$prim57038 = alloca %struct.ScmObj*, align 8
%argslist55516$_37foldl1470812 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47239, %struct.ScmObj* %argslist55516$_37foldl1470811)
store volatile %struct.ScmObj* %argslist55516$_37foldl1470812, %struct.ScmObj** %stackaddr$prim57038, align 8
%stackaddr$prim57039 = alloca %struct.ScmObj*, align 8
%argslist55516$_37foldl1470813 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47084, %struct.ScmObj* %argslist55516$_37foldl1470812)
store volatile %struct.ScmObj* %argslist55516$_37foldl1470813, %struct.ScmObj** %stackaddr$prim57039, align 8
%stackaddr$prim57040 = alloca %struct.ScmObj*, align 8
%argslist55516$_37foldl1470814 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47481, %struct.ScmObj* %argslist55516$_37foldl1470813)
store volatile %struct.ScmObj* %argslist55516$_37foldl1470814, %struct.ScmObj** %stackaddr$prim57040, align 8
%clofunc57041 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147081)
musttail call tailcc void %clofunc57041(%struct.ScmObj* %_37foldl147081, %struct.ScmObj* %argslist55516$_37foldl1470814)
ret void
}

define tailcc void @proc_clo$ae47969(%struct.ScmObj* %env$ae47969,%struct.ScmObj* %current_45args55521) {
%stackaddr$prim57042 = alloca %struct.ScmObj*, align 8
%k47483 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55521)
store volatile %struct.ScmObj* %k47483, %struct.ScmObj** %stackaddr$prim57042, align 8
%stackaddr$prim57043 = alloca %struct.ScmObj*, align 8
%current_45args55522 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55521)
store volatile %struct.ScmObj* %current_45args55522, %struct.ScmObj** %stackaddr$prim57043, align 8
%stackaddr$prim57044 = alloca %struct.ScmObj*, align 8
%_37length47086 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55522)
store volatile %struct.ScmObj* %_37length47086, %struct.ScmObj** %stackaddr$prim57044, align 8
%ae47971 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57045 = alloca %struct.ScmObj*, align 8
%fptrToInt57046 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47972 to i64
%ae47972 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57046)
store volatile %struct.ScmObj* %ae47972, %struct.ScmObj** %stackaddr$makeclosure57045, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47972, %struct.ScmObj* %_37length47086, i64 0)
%argslist55533$k474830 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57047 = alloca %struct.ScmObj*, align 8
%argslist55533$k474831 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47972, %struct.ScmObj* %argslist55533$k474830)
store volatile %struct.ScmObj* %argslist55533$k474831, %struct.ScmObj** %stackaddr$prim57047, align 8
%stackaddr$prim57048 = alloca %struct.ScmObj*, align 8
%argslist55533$k474832 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47971, %struct.ScmObj* %argslist55533$k474831)
store volatile %struct.ScmObj* %argslist55533$k474832, %struct.ScmObj** %stackaddr$prim57048, align 8
%clofunc57049 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47483)
musttail call tailcc void %clofunc57049(%struct.ScmObj* %k47483, %struct.ScmObj* %argslist55533$k474832)
ret void
}

define tailcc void @proc_clo$ae47972(%struct.ScmObj* %env$ae47972,%struct.ScmObj* %current_45args55524) {
%stackaddr$env-ref57050 = alloca %struct.ScmObj*, align 8
%_37length47086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47972, i64 0)
store %struct.ScmObj* %_37length47086, %struct.ScmObj** %stackaddr$env-ref57050
%stackaddr$prim57051 = alloca %struct.ScmObj*, align 8
%k47484 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55524)
store volatile %struct.ScmObj* %k47484, %struct.ScmObj** %stackaddr$prim57051, align 8
%stackaddr$prim57052 = alloca %struct.ScmObj*, align 8
%current_45args55525 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55524)
store volatile %struct.ScmObj* %current_45args55525, %struct.ScmObj** %stackaddr$prim57052, align 8
%stackaddr$prim57053 = alloca %struct.ScmObj*, align 8
%lst47087 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55525)
store volatile %struct.ScmObj* %lst47087, %struct.ScmObj** %stackaddr$prim57053, align 8
%stackaddr$prim57054 = alloca %struct.ScmObj*, align 8
%anf_45bind47233 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47087)
store volatile %struct.ScmObj* %anf_45bind47233, %struct.ScmObj** %stackaddr$prim57054, align 8
%truthy$cmp57055 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47233)
%cmp$cmp57055 = icmp eq i64 %truthy$cmp57055, 1
br i1 %cmp$cmp57055, label %truebranch$cmp57055, label %falsebranch$cmp57055
truebranch$cmp57055:
%ae47976 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47977 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55527$k474840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57056 = alloca %struct.ScmObj*, align 8
%argslist55527$k474841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47977, %struct.ScmObj* %argslist55527$k474840)
store volatile %struct.ScmObj* %argslist55527$k474841, %struct.ScmObj** %stackaddr$prim57056, align 8
%stackaddr$prim57057 = alloca %struct.ScmObj*, align 8
%argslist55527$k474842 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47976, %struct.ScmObj* %argslist55527$k474841)
store volatile %struct.ScmObj* %argslist55527$k474842, %struct.ScmObj** %stackaddr$prim57057, align 8
%clofunc57058 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47484)
musttail call tailcc void %clofunc57058(%struct.ScmObj* %k47484, %struct.ScmObj* %argslist55527$k474842)
ret void
falsebranch$cmp57055:
%stackaddr$prim57059 = alloca %struct.ScmObj*, align 8
%anf_45bind47234 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47087)
store volatile %struct.ScmObj* %anf_45bind47234, %struct.ScmObj** %stackaddr$prim57059, align 8
%stackaddr$makeclosure57060 = alloca %struct.ScmObj*, align 8
%fptrToInt57061 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47986 to i64
%ae47986 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57061)
store volatile %struct.ScmObj* %ae47986, %struct.ScmObj** %stackaddr$makeclosure57060, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47986, %struct.ScmObj* %k47484, i64 0)
%argslist55532$_37length470860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57062 = alloca %struct.ScmObj*, align 8
%argslist55532$_37length470861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47234, %struct.ScmObj* %argslist55532$_37length470860)
store volatile %struct.ScmObj* %argslist55532$_37length470861, %struct.ScmObj** %stackaddr$prim57062, align 8
%stackaddr$prim57063 = alloca %struct.ScmObj*, align 8
%argslist55532$_37length470862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47986, %struct.ScmObj* %argslist55532$_37length470861)
store volatile %struct.ScmObj* %argslist55532$_37length470862, %struct.ScmObj** %stackaddr$prim57063, align 8
%clofunc57064 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length47086)
musttail call tailcc void %clofunc57064(%struct.ScmObj* %_37length47086, %struct.ScmObj* %argslist55532$_37length470862)
ret void
}

define tailcc void @proc_clo$ae47986(%struct.ScmObj* %env$ae47986,%struct.ScmObj* %current_45args55528) {
%stackaddr$env-ref57065 = alloca %struct.ScmObj*, align 8
%k47484 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47986, i64 0)
store %struct.ScmObj* %k47484, %struct.ScmObj** %stackaddr$env-ref57065
%stackaddr$prim57066 = alloca %struct.ScmObj*, align 8
%_95k47485 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55528)
store volatile %struct.ScmObj* %_95k47485, %struct.ScmObj** %stackaddr$prim57066, align 8
%stackaddr$prim57067 = alloca %struct.ScmObj*, align 8
%current_45args55529 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55528)
store volatile %struct.ScmObj* %current_45args55529, %struct.ScmObj** %stackaddr$prim57067, align 8
%stackaddr$prim57068 = alloca %struct.ScmObj*, align 8
%anf_45bind47235 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55529)
store volatile %struct.ScmObj* %anf_45bind47235, %struct.ScmObj** %stackaddr$prim57068, align 8
%ae47988 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57069 = alloca %struct.ScmObj*, align 8
%cpsprim47486 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae47988, %struct.ScmObj* %anf_45bind47235)
store volatile %struct.ScmObj* %cpsprim47486, %struct.ScmObj** %stackaddr$prim57069, align 8
%ae47991 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55531$k474840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57070 = alloca %struct.ScmObj*, align 8
%argslist55531$k474841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47486, %struct.ScmObj* %argslist55531$k474840)
store volatile %struct.ScmObj* %argslist55531$k474841, %struct.ScmObj** %stackaddr$prim57070, align 8
%stackaddr$prim57071 = alloca %struct.ScmObj*, align 8
%argslist55531$k474842 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47991, %struct.ScmObj* %argslist55531$k474841)
store volatile %struct.ScmObj* %argslist55531$k474842, %struct.ScmObj** %stackaddr$prim57071, align 8
%clofunc57072 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47484)
musttail call tailcc void %clofunc57072(%struct.ScmObj* %k47484, %struct.ScmObj* %argslist55531$k474842)
ret void
}

define tailcc void @proc_clo$ae47819(%struct.ScmObj* %env$ae47819,%struct.ScmObj* %current_45args55536) {
%stackaddr$prim57073 = alloca %struct.ScmObj*, align 8
%k47487 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55536)
store volatile %struct.ScmObj* %k47487, %struct.ScmObj** %stackaddr$prim57073, align 8
%stackaddr$prim57074 = alloca %struct.ScmObj*, align 8
%current_45args55537 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55536)
store volatile %struct.ScmObj* %current_45args55537, %struct.ScmObj** %stackaddr$prim57074, align 8
%stackaddr$prim57075 = alloca %struct.ScmObj*, align 8
%_37take47089 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55537)
store volatile %struct.ScmObj* %_37take47089, %struct.ScmObj** %stackaddr$prim57075, align 8
%ae47821 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57076 = alloca %struct.ScmObj*, align 8
%fptrToInt57077 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47822 to i64
%ae47822 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57077)
store volatile %struct.ScmObj* %ae47822, %struct.ScmObj** %stackaddr$makeclosure57076, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47822, %struct.ScmObj* %_37take47089, i64 0)
%argslist55550$k474870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57078 = alloca %struct.ScmObj*, align 8
%argslist55550$k474871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47822, %struct.ScmObj* %argslist55550$k474870)
store volatile %struct.ScmObj* %argslist55550$k474871, %struct.ScmObj** %stackaddr$prim57078, align 8
%stackaddr$prim57079 = alloca %struct.ScmObj*, align 8
%argslist55550$k474872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47821, %struct.ScmObj* %argslist55550$k474871)
store volatile %struct.ScmObj* %argslist55550$k474872, %struct.ScmObj** %stackaddr$prim57079, align 8
%clofunc57080 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47487)
musttail call tailcc void %clofunc57080(%struct.ScmObj* %k47487, %struct.ScmObj* %argslist55550$k474872)
ret void
}

define tailcc void @proc_clo$ae47822(%struct.ScmObj* %env$ae47822,%struct.ScmObj* %current_45args55539) {
%stackaddr$env-ref57081 = alloca %struct.ScmObj*, align 8
%_37take47089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47822, i64 0)
store %struct.ScmObj* %_37take47089, %struct.ScmObj** %stackaddr$env-ref57081
%stackaddr$prim57082 = alloca %struct.ScmObj*, align 8
%k47488 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55539)
store volatile %struct.ScmObj* %k47488, %struct.ScmObj** %stackaddr$prim57082, align 8
%stackaddr$prim57083 = alloca %struct.ScmObj*, align 8
%current_45args55540 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55539)
store volatile %struct.ScmObj* %current_45args55540, %struct.ScmObj** %stackaddr$prim57083, align 8
%stackaddr$prim57084 = alloca %struct.ScmObj*, align 8
%lst47091 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55540)
store volatile %struct.ScmObj* %lst47091, %struct.ScmObj** %stackaddr$prim57084, align 8
%stackaddr$prim57085 = alloca %struct.ScmObj*, align 8
%current_45args55541 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55540)
store volatile %struct.ScmObj* %current_45args55541, %struct.ScmObj** %stackaddr$prim57085, align 8
%stackaddr$prim57086 = alloca %struct.ScmObj*, align 8
%n47090 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55541)
store volatile %struct.ScmObj* %n47090, %struct.ScmObj** %stackaddr$prim57086, align 8
%ae47824 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57087 = alloca %struct.ScmObj*, align 8
%anf_45bind47226 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n47090, %struct.ScmObj* %ae47824)
store volatile %struct.ScmObj* %anf_45bind47226, %struct.ScmObj** %stackaddr$prim57087, align 8
%truthy$cmp57088 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47226)
%cmp$cmp57088 = icmp eq i64 %truthy$cmp57088, 1
br i1 %cmp$cmp57088, label %truebranch$cmp57088, label %falsebranch$cmp57088
truebranch$cmp57088:
%ae47827 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47828 = call %struct.ScmObj* @const_init_null()
%argslist55543$k474880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57089 = alloca %struct.ScmObj*, align 8
%argslist55543$k474881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47828, %struct.ScmObj* %argslist55543$k474880)
store volatile %struct.ScmObj* %argslist55543$k474881, %struct.ScmObj** %stackaddr$prim57089, align 8
%stackaddr$prim57090 = alloca %struct.ScmObj*, align 8
%argslist55543$k474882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47827, %struct.ScmObj* %argslist55543$k474881)
store volatile %struct.ScmObj* %argslist55543$k474882, %struct.ScmObj** %stackaddr$prim57090, align 8
%clofunc57091 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47488)
musttail call tailcc void %clofunc57091(%struct.ScmObj* %k47488, %struct.ScmObj* %argslist55543$k474882)
ret void
falsebranch$cmp57088:
%stackaddr$prim57092 = alloca %struct.ScmObj*, align 8
%anf_45bind47227 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47091)
store volatile %struct.ScmObj* %anf_45bind47227, %struct.ScmObj** %stackaddr$prim57092, align 8
%truthy$cmp57093 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47227)
%cmp$cmp57093 = icmp eq i64 %truthy$cmp57093, 1
br i1 %cmp$cmp57093, label %truebranch$cmp57093, label %falsebranch$cmp57093
truebranch$cmp57093:
%ae47838 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47839 = call %struct.ScmObj* @const_init_null()
%argslist55544$k474880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57094 = alloca %struct.ScmObj*, align 8
%argslist55544$k474881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47839, %struct.ScmObj* %argslist55544$k474880)
store volatile %struct.ScmObj* %argslist55544$k474881, %struct.ScmObj** %stackaddr$prim57094, align 8
%stackaddr$prim57095 = alloca %struct.ScmObj*, align 8
%argslist55544$k474882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47838, %struct.ScmObj* %argslist55544$k474881)
store volatile %struct.ScmObj* %argslist55544$k474882, %struct.ScmObj** %stackaddr$prim57095, align 8
%clofunc57096 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47488)
musttail call tailcc void %clofunc57096(%struct.ScmObj* %k47488, %struct.ScmObj* %argslist55544$k474882)
ret void
falsebranch$cmp57093:
%stackaddr$prim57097 = alloca %struct.ScmObj*, align 8
%anf_45bind47228 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47091)
store volatile %struct.ScmObj* %anf_45bind47228, %struct.ScmObj** %stackaddr$prim57097, align 8
%stackaddr$prim57098 = alloca %struct.ScmObj*, align 8
%anf_45bind47229 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47091)
store volatile %struct.ScmObj* %anf_45bind47229, %struct.ScmObj** %stackaddr$prim57098, align 8
%ae47849 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57099 = alloca %struct.ScmObj*, align 8
%anf_45bind47230 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n47090, %struct.ScmObj* %ae47849)
store volatile %struct.ScmObj* %anf_45bind47230, %struct.ScmObj** %stackaddr$prim57099, align 8
%stackaddr$makeclosure57100 = alloca %struct.ScmObj*, align 8
%fptrToInt57101 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47851 to i64
%ae47851 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57101)
store volatile %struct.ScmObj* %ae47851, %struct.ScmObj** %stackaddr$makeclosure57100, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47851, %struct.ScmObj* %k47488, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47851, %struct.ScmObj* %anf_45bind47228, i64 1)
%argslist55549$_37take470890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57102 = alloca %struct.ScmObj*, align 8
%argslist55549$_37take470891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47230, %struct.ScmObj* %argslist55549$_37take470890)
store volatile %struct.ScmObj* %argslist55549$_37take470891, %struct.ScmObj** %stackaddr$prim57102, align 8
%stackaddr$prim57103 = alloca %struct.ScmObj*, align 8
%argslist55549$_37take470892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47229, %struct.ScmObj* %argslist55549$_37take470891)
store volatile %struct.ScmObj* %argslist55549$_37take470892, %struct.ScmObj** %stackaddr$prim57103, align 8
%stackaddr$prim57104 = alloca %struct.ScmObj*, align 8
%argslist55549$_37take470893 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47851, %struct.ScmObj* %argslist55549$_37take470892)
store volatile %struct.ScmObj* %argslist55549$_37take470893, %struct.ScmObj** %stackaddr$prim57104, align 8
%clofunc57105 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take47089)
musttail call tailcc void %clofunc57105(%struct.ScmObj* %_37take47089, %struct.ScmObj* %argslist55549$_37take470893)
ret void
}

define tailcc void @proc_clo$ae47851(%struct.ScmObj* %env$ae47851,%struct.ScmObj* %current_45args55545) {
%stackaddr$env-ref57106 = alloca %struct.ScmObj*, align 8
%k47488 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47851, i64 0)
store %struct.ScmObj* %k47488, %struct.ScmObj** %stackaddr$env-ref57106
%stackaddr$env-ref57107 = alloca %struct.ScmObj*, align 8
%anf_45bind47228 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47851, i64 1)
store %struct.ScmObj* %anf_45bind47228, %struct.ScmObj** %stackaddr$env-ref57107
%stackaddr$prim57108 = alloca %struct.ScmObj*, align 8
%_95k47489 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55545)
store volatile %struct.ScmObj* %_95k47489, %struct.ScmObj** %stackaddr$prim57108, align 8
%stackaddr$prim57109 = alloca %struct.ScmObj*, align 8
%current_45args55546 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55545)
store volatile %struct.ScmObj* %current_45args55546, %struct.ScmObj** %stackaddr$prim57109, align 8
%stackaddr$prim57110 = alloca %struct.ScmObj*, align 8
%anf_45bind47231 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55546)
store volatile %struct.ScmObj* %anf_45bind47231, %struct.ScmObj** %stackaddr$prim57110, align 8
%stackaddr$prim57111 = alloca %struct.ScmObj*, align 8
%cpsprim47490 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47228, %struct.ScmObj* %anf_45bind47231)
store volatile %struct.ScmObj* %cpsprim47490, %struct.ScmObj** %stackaddr$prim57111, align 8
%ae47857 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55548$k474880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57112 = alloca %struct.ScmObj*, align 8
%argslist55548$k474881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47490, %struct.ScmObj* %argslist55548$k474880)
store volatile %struct.ScmObj* %argslist55548$k474881, %struct.ScmObj** %stackaddr$prim57112, align 8
%stackaddr$prim57113 = alloca %struct.ScmObj*, align 8
%argslist55548$k474882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47857, %struct.ScmObj* %argslist55548$k474881)
store volatile %struct.ScmObj* %argslist55548$k474882, %struct.ScmObj** %stackaddr$prim57113, align 8
%clofunc57114 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47488)
musttail call tailcc void %clofunc57114(%struct.ScmObj* %k47488, %struct.ScmObj* %argslist55548$k474882)
ret void
}

define tailcc void @proc_clo$ae47722(%struct.ScmObj* %env$ae47722,%struct.ScmObj* %current_45args55553) {
%stackaddr$prim57115 = alloca %struct.ScmObj*, align 8
%k47491 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55553)
store volatile %struct.ScmObj* %k47491, %struct.ScmObj** %stackaddr$prim57115, align 8
%stackaddr$prim57116 = alloca %struct.ScmObj*, align 8
%current_45args55554 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55553)
store volatile %struct.ScmObj* %current_45args55554, %struct.ScmObj** %stackaddr$prim57116, align 8
%stackaddr$prim57117 = alloca %struct.ScmObj*, align 8
%_37map47093 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55554)
store volatile %struct.ScmObj* %_37map47093, %struct.ScmObj** %stackaddr$prim57117, align 8
%ae47724 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57118 = alloca %struct.ScmObj*, align 8
%fptrToInt57119 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47725 to i64
%ae47725 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57119)
store volatile %struct.ScmObj* %ae47725, %struct.ScmObj** %stackaddr$makeclosure57118, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47725, %struct.ScmObj* %_37map47093, i64 0)
%argslist55570$k474910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57120 = alloca %struct.ScmObj*, align 8
%argslist55570$k474911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47725, %struct.ScmObj* %argslist55570$k474910)
store volatile %struct.ScmObj* %argslist55570$k474911, %struct.ScmObj** %stackaddr$prim57120, align 8
%stackaddr$prim57121 = alloca %struct.ScmObj*, align 8
%argslist55570$k474912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47724, %struct.ScmObj* %argslist55570$k474911)
store volatile %struct.ScmObj* %argslist55570$k474912, %struct.ScmObj** %stackaddr$prim57121, align 8
%clofunc57122 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47491)
musttail call tailcc void %clofunc57122(%struct.ScmObj* %k47491, %struct.ScmObj* %argslist55570$k474912)
ret void
}

define tailcc void @proc_clo$ae47725(%struct.ScmObj* %env$ae47725,%struct.ScmObj* %current_45args55556) {
%stackaddr$env-ref57123 = alloca %struct.ScmObj*, align 8
%_37map47093 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47725, i64 0)
store %struct.ScmObj* %_37map47093, %struct.ScmObj** %stackaddr$env-ref57123
%stackaddr$prim57124 = alloca %struct.ScmObj*, align 8
%k47492 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55556)
store volatile %struct.ScmObj* %k47492, %struct.ScmObj** %stackaddr$prim57124, align 8
%stackaddr$prim57125 = alloca %struct.ScmObj*, align 8
%current_45args55557 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55556)
store volatile %struct.ScmObj* %current_45args55557, %struct.ScmObj** %stackaddr$prim57125, align 8
%stackaddr$prim57126 = alloca %struct.ScmObj*, align 8
%f47095 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55557)
store volatile %struct.ScmObj* %f47095, %struct.ScmObj** %stackaddr$prim57126, align 8
%stackaddr$prim57127 = alloca %struct.ScmObj*, align 8
%current_45args55558 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55557)
store volatile %struct.ScmObj* %current_45args55558, %struct.ScmObj** %stackaddr$prim57127, align 8
%stackaddr$prim57128 = alloca %struct.ScmObj*, align 8
%lst47094 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55558)
store volatile %struct.ScmObj* %lst47094, %struct.ScmObj** %stackaddr$prim57128, align 8
%stackaddr$prim57129 = alloca %struct.ScmObj*, align 8
%anf_45bind47220 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47094)
store volatile %struct.ScmObj* %anf_45bind47220, %struct.ScmObj** %stackaddr$prim57129, align 8
%truthy$cmp57130 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47220)
%cmp$cmp57130 = icmp eq i64 %truthy$cmp57130, 1
br i1 %cmp$cmp57130, label %truebranch$cmp57130, label %falsebranch$cmp57130
truebranch$cmp57130:
%ae47729 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47730 = call %struct.ScmObj* @const_init_null()
%argslist55560$k474920 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57131 = alloca %struct.ScmObj*, align 8
%argslist55560$k474921 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47730, %struct.ScmObj* %argslist55560$k474920)
store volatile %struct.ScmObj* %argslist55560$k474921, %struct.ScmObj** %stackaddr$prim57131, align 8
%stackaddr$prim57132 = alloca %struct.ScmObj*, align 8
%argslist55560$k474922 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47729, %struct.ScmObj* %argslist55560$k474921)
store volatile %struct.ScmObj* %argslist55560$k474922, %struct.ScmObj** %stackaddr$prim57132, align 8
%clofunc57133 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47492)
musttail call tailcc void %clofunc57133(%struct.ScmObj* %k47492, %struct.ScmObj* %argslist55560$k474922)
ret void
falsebranch$cmp57130:
%stackaddr$prim57134 = alloca %struct.ScmObj*, align 8
%anf_45bind47221 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47094)
store volatile %struct.ScmObj* %anf_45bind47221, %struct.ScmObj** %stackaddr$prim57134, align 8
%stackaddr$makeclosure57135 = alloca %struct.ScmObj*, align 8
%fptrToInt57136 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47739 to i64
%ae47739 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57136)
store volatile %struct.ScmObj* %ae47739, %struct.ScmObj** %stackaddr$makeclosure57135, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47739, %struct.ScmObj* %lst47094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47739, %struct.ScmObj* %_37map47093, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47739, %struct.ScmObj* %k47492, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae47739, %struct.ScmObj* %f47095, i64 3)
%argslist55569$f470950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57137 = alloca %struct.ScmObj*, align 8
%argslist55569$f470951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47221, %struct.ScmObj* %argslist55569$f470950)
store volatile %struct.ScmObj* %argslist55569$f470951, %struct.ScmObj** %stackaddr$prim57137, align 8
%stackaddr$prim57138 = alloca %struct.ScmObj*, align 8
%argslist55569$f470952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47739, %struct.ScmObj* %argslist55569$f470951)
store volatile %struct.ScmObj* %argslist55569$f470952, %struct.ScmObj** %stackaddr$prim57138, align 8
%clofunc57139 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47095)
musttail call tailcc void %clofunc57139(%struct.ScmObj* %f47095, %struct.ScmObj* %argslist55569$f470952)
ret void
}

define tailcc void @proc_clo$ae47739(%struct.ScmObj* %env$ae47739,%struct.ScmObj* %current_45args55561) {
%stackaddr$env-ref57140 = alloca %struct.ScmObj*, align 8
%lst47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47739, i64 0)
store %struct.ScmObj* %lst47094, %struct.ScmObj** %stackaddr$env-ref57140
%stackaddr$env-ref57141 = alloca %struct.ScmObj*, align 8
%_37map47093 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47739, i64 1)
store %struct.ScmObj* %_37map47093, %struct.ScmObj** %stackaddr$env-ref57141
%stackaddr$env-ref57142 = alloca %struct.ScmObj*, align 8
%k47492 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47739, i64 2)
store %struct.ScmObj* %k47492, %struct.ScmObj** %stackaddr$env-ref57142
%stackaddr$env-ref57143 = alloca %struct.ScmObj*, align 8
%f47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47739, i64 3)
store %struct.ScmObj* %f47095, %struct.ScmObj** %stackaddr$env-ref57143
%stackaddr$prim57144 = alloca %struct.ScmObj*, align 8
%_95k47493 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55561)
store volatile %struct.ScmObj* %_95k47493, %struct.ScmObj** %stackaddr$prim57144, align 8
%stackaddr$prim57145 = alloca %struct.ScmObj*, align 8
%current_45args55562 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55561)
store volatile %struct.ScmObj* %current_45args55562, %struct.ScmObj** %stackaddr$prim57145, align 8
%stackaddr$prim57146 = alloca %struct.ScmObj*, align 8
%anf_45bind47222 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55562)
store volatile %struct.ScmObj* %anf_45bind47222, %struct.ScmObj** %stackaddr$prim57146, align 8
%stackaddr$prim57147 = alloca %struct.ScmObj*, align 8
%anf_45bind47223 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47094)
store volatile %struct.ScmObj* %anf_45bind47223, %struct.ScmObj** %stackaddr$prim57147, align 8
%stackaddr$makeclosure57148 = alloca %struct.ScmObj*, align 8
%fptrToInt57149 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47743 to i64
%ae47743 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57149)
store volatile %struct.ScmObj* %ae47743, %struct.ScmObj** %stackaddr$makeclosure57148, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47743, %struct.ScmObj* %anf_45bind47222, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47743, %struct.ScmObj* %k47492, i64 1)
%argslist55568$_37map470930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57150 = alloca %struct.ScmObj*, align 8
%argslist55568$_37map470931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47223, %struct.ScmObj* %argslist55568$_37map470930)
store volatile %struct.ScmObj* %argslist55568$_37map470931, %struct.ScmObj** %stackaddr$prim57150, align 8
%stackaddr$prim57151 = alloca %struct.ScmObj*, align 8
%argslist55568$_37map470932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47095, %struct.ScmObj* %argslist55568$_37map470931)
store volatile %struct.ScmObj* %argslist55568$_37map470932, %struct.ScmObj** %stackaddr$prim57151, align 8
%stackaddr$prim57152 = alloca %struct.ScmObj*, align 8
%argslist55568$_37map470933 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47743, %struct.ScmObj* %argslist55568$_37map470932)
store volatile %struct.ScmObj* %argslist55568$_37map470933, %struct.ScmObj** %stackaddr$prim57152, align 8
%clofunc57153 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map47093)
musttail call tailcc void %clofunc57153(%struct.ScmObj* %_37map47093, %struct.ScmObj* %argslist55568$_37map470933)
ret void
}

define tailcc void @proc_clo$ae47743(%struct.ScmObj* %env$ae47743,%struct.ScmObj* %current_45args55564) {
%stackaddr$env-ref57154 = alloca %struct.ScmObj*, align 8
%anf_45bind47222 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47743, i64 0)
store %struct.ScmObj* %anf_45bind47222, %struct.ScmObj** %stackaddr$env-ref57154
%stackaddr$env-ref57155 = alloca %struct.ScmObj*, align 8
%k47492 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47743, i64 1)
store %struct.ScmObj* %k47492, %struct.ScmObj** %stackaddr$env-ref57155
%stackaddr$prim57156 = alloca %struct.ScmObj*, align 8
%_95k47494 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55564)
store volatile %struct.ScmObj* %_95k47494, %struct.ScmObj** %stackaddr$prim57156, align 8
%stackaddr$prim57157 = alloca %struct.ScmObj*, align 8
%current_45args55565 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55564)
store volatile %struct.ScmObj* %current_45args55565, %struct.ScmObj** %stackaddr$prim57157, align 8
%stackaddr$prim57158 = alloca %struct.ScmObj*, align 8
%anf_45bind47224 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55565)
store volatile %struct.ScmObj* %anf_45bind47224, %struct.ScmObj** %stackaddr$prim57158, align 8
%stackaddr$prim57159 = alloca %struct.ScmObj*, align 8
%cpsprim47495 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47222, %struct.ScmObj* %anf_45bind47224)
store volatile %struct.ScmObj* %cpsprim47495, %struct.ScmObj** %stackaddr$prim57159, align 8
%ae47749 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55567$k474920 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57160 = alloca %struct.ScmObj*, align 8
%argslist55567$k474921 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47495, %struct.ScmObj* %argslist55567$k474920)
store volatile %struct.ScmObj* %argslist55567$k474921, %struct.ScmObj** %stackaddr$prim57160, align 8
%stackaddr$prim57161 = alloca %struct.ScmObj*, align 8
%argslist55567$k474922 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47749, %struct.ScmObj* %argslist55567$k474921)
store volatile %struct.ScmObj* %argslist55567$k474922, %struct.ScmObj** %stackaddr$prim57161, align 8
%clofunc57162 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47492)
musttail call tailcc void %clofunc57162(%struct.ScmObj* %k47492, %struct.ScmObj* %argslist55567$k474922)
ret void
}

define tailcc void @proc_clo$ae47642(%struct.ScmObj* %env$ae47642,%struct.ScmObj* %current_45args55573) {
%stackaddr$prim57163 = alloca %struct.ScmObj*, align 8
%k47496 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55573)
store volatile %struct.ScmObj* %k47496, %struct.ScmObj** %stackaddr$prim57163, align 8
%stackaddr$prim57164 = alloca %struct.ScmObj*, align 8
%current_45args55574 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55573)
store volatile %struct.ScmObj* %current_45args55574, %struct.ScmObj** %stackaddr$prim57164, align 8
%stackaddr$prim57165 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55574)
store volatile %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$prim57165, align 8
%ae47644 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57166 = alloca %struct.ScmObj*, align 8
%fptrToInt57167 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47645 to i64
%ae47645 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57167)
store volatile %struct.ScmObj* %ae47645, %struct.ScmObj** %stackaddr$makeclosure57166, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47645, %struct.ScmObj* %_37foldr147097, i64 0)
%argslist55587$k474960 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57168 = alloca %struct.ScmObj*, align 8
%argslist55587$k474961 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47645, %struct.ScmObj* %argslist55587$k474960)
store volatile %struct.ScmObj* %argslist55587$k474961, %struct.ScmObj** %stackaddr$prim57168, align 8
%stackaddr$prim57169 = alloca %struct.ScmObj*, align 8
%argslist55587$k474962 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47644, %struct.ScmObj* %argslist55587$k474961)
store volatile %struct.ScmObj* %argslist55587$k474962, %struct.ScmObj** %stackaddr$prim57169, align 8
%clofunc57170 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47496)
musttail call tailcc void %clofunc57170(%struct.ScmObj* %k47496, %struct.ScmObj* %argslist55587$k474962)
ret void
}

define tailcc void @proc_clo$ae47645(%struct.ScmObj* %env$ae47645,%struct.ScmObj* %current_45args55576) {
%stackaddr$env-ref57171 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47645, i64 0)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref57171
%stackaddr$prim57172 = alloca %struct.ScmObj*, align 8
%k47497 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55576)
store volatile %struct.ScmObj* %k47497, %struct.ScmObj** %stackaddr$prim57172, align 8
%stackaddr$prim57173 = alloca %struct.ScmObj*, align 8
%current_45args55577 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55576)
store volatile %struct.ScmObj* %current_45args55577, %struct.ScmObj** %stackaddr$prim57173, align 8
%stackaddr$prim57174 = alloca %struct.ScmObj*, align 8
%f47100 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55577)
store volatile %struct.ScmObj* %f47100, %struct.ScmObj** %stackaddr$prim57174, align 8
%stackaddr$prim57175 = alloca %struct.ScmObj*, align 8
%current_45args55578 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55577)
store volatile %struct.ScmObj* %current_45args55578, %struct.ScmObj** %stackaddr$prim57175, align 8
%stackaddr$prim57176 = alloca %struct.ScmObj*, align 8
%acc47099 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55578)
store volatile %struct.ScmObj* %acc47099, %struct.ScmObj** %stackaddr$prim57176, align 8
%stackaddr$prim57177 = alloca %struct.ScmObj*, align 8
%current_45args55579 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55578)
store volatile %struct.ScmObj* %current_45args55579, %struct.ScmObj** %stackaddr$prim57177, align 8
%stackaddr$prim57178 = alloca %struct.ScmObj*, align 8
%lst47098 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55579)
store volatile %struct.ScmObj* %lst47098, %struct.ScmObj** %stackaddr$prim57178, align 8
%stackaddr$prim57179 = alloca %struct.ScmObj*, align 8
%anf_45bind47215 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47098)
store volatile %struct.ScmObj* %anf_45bind47215, %struct.ScmObj** %stackaddr$prim57179, align 8
%truthy$cmp57180 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47215)
%cmp$cmp57180 = icmp eq i64 %truthy$cmp57180, 1
br i1 %cmp$cmp57180, label %truebranch$cmp57180, label %falsebranch$cmp57180
truebranch$cmp57180:
%ae47649 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55581$k474970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57181 = alloca %struct.ScmObj*, align 8
%argslist55581$k474971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47099, %struct.ScmObj* %argslist55581$k474970)
store volatile %struct.ScmObj* %argslist55581$k474971, %struct.ScmObj** %stackaddr$prim57181, align 8
%stackaddr$prim57182 = alloca %struct.ScmObj*, align 8
%argslist55581$k474972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47649, %struct.ScmObj* %argslist55581$k474971)
store volatile %struct.ScmObj* %argslist55581$k474972, %struct.ScmObj** %stackaddr$prim57182, align 8
%clofunc57183 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47497)
musttail call tailcc void %clofunc57183(%struct.ScmObj* %k47497, %struct.ScmObj* %argslist55581$k474972)
ret void
falsebranch$cmp57180:
%stackaddr$prim57184 = alloca %struct.ScmObj*, align 8
%anf_45bind47216 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47098)
store volatile %struct.ScmObj* %anf_45bind47216, %struct.ScmObj** %stackaddr$prim57184, align 8
%stackaddr$prim57185 = alloca %struct.ScmObj*, align 8
%anf_45bind47217 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47098)
store volatile %struct.ScmObj* %anf_45bind47217, %struct.ScmObj** %stackaddr$prim57185, align 8
%stackaddr$makeclosure57186 = alloca %struct.ScmObj*, align 8
%fptrToInt57187 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47657 to i64
%ae47657 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57187)
store volatile %struct.ScmObj* %ae47657, %struct.ScmObj** %stackaddr$makeclosure57186, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47657, %struct.ScmObj* %anf_45bind47216, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47657, %struct.ScmObj* %f47100, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47657, %struct.ScmObj* %k47497, i64 2)
%argslist55586$_37foldr1470970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57188 = alloca %struct.ScmObj*, align 8
%argslist55586$_37foldr1470971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47217, %struct.ScmObj* %argslist55586$_37foldr1470970)
store volatile %struct.ScmObj* %argslist55586$_37foldr1470971, %struct.ScmObj** %stackaddr$prim57188, align 8
%stackaddr$prim57189 = alloca %struct.ScmObj*, align 8
%argslist55586$_37foldr1470972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47099, %struct.ScmObj* %argslist55586$_37foldr1470971)
store volatile %struct.ScmObj* %argslist55586$_37foldr1470972, %struct.ScmObj** %stackaddr$prim57189, align 8
%stackaddr$prim57190 = alloca %struct.ScmObj*, align 8
%argslist55586$_37foldr1470973 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47100, %struct.ScmObj* %argslist55586$_37foldr1470972)
store volatile %struct.ScmObj* %argslist55586$_37foldr1470973, %struct.ScmObj** %stackaddr$prim57190, align 8
%stackaddr$prim57191 = alloca %struct.ScmObj*, align 8
%argslist55586$_37foldr1470974 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47657, %struct.ScmObj* %argslist55586$_37foldr1470973)
store volatile %struct.ScmObj* %argslist55586$_37foldr1470974, %struct.ScmObj** %stackaddr$prim57191, align 8
%clofunc57192 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147097)
musttail call tailcc void %clofunc57192(%struct.ScmObj* %_37foldr147097, %struct.ScmObj* %argslist55586$_37foldr1470974)
ret void
}

define tailcc void @proc_clo$ae47657(%struct.ScmObj* %env$ae47657,%struct.ScmObj* %current_45args55582) {
%stackaddr$env-ref57193 = alloca %struct.ScmObj*, align 8
%anf_45bind47216 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47657, i64 0)
store %struct.ScmObj* %anf_45bind47216, %struct.ScmObj** %stackaddr$env-ref57193
%stackaddr$env-ref57194 = alloca %struct.ScmObj*, align 8
%f47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47657, i64 1)
store %struct.ScmObj* %f47100, %struct.ScmObj** %stackaddr$env-ref57194
%stackaddr$env-ref57195 = alloca %struct.ScmObj*, align 8
%k47497 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47657, i64 2)
store %struct.ScmObj* %k47497, %struct.ScmObj** %stackaddr$env-ref57195
%stackaddr$prim57196 = alloca %struct.ScmObj*, align 8
%_95k47498 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55582)
store volatile %struct.ScmObj* %_95k47498, %struct.ScmObj** %stackaddr$prim57196, align 8
%stackaddr$prim57197 = alloca %struct.ScmObj*, align 8
%current_45args55583 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55582)
store volatile %struct.ScmObj* %current_45args55583, %struct.ScmObj** %stackaddr$prim57197, align 8
%stackaddr$prim57198 = alloca %struct.ScmObj*, align 8
%anf_45bind47218 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55583)
store volatile %struct.ScmObj* %anf_45bind47218, %struct.ScmObj** %stackaddr$prim57198, align 8
%argslist55585$f471000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57199 = alloca %struct.ScmObj*, align 8
%argslist55585$f471001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47218, %struct.ScmObj* %argslist55585$f471000)
store volatile %struct.ScmObj* %argslist55585$f471001, %struct.ScmObj** %stackaddr$prim57199, align 8
%stackaddr$prim57200 = alloca %struct.ScmObj*, align 8
%argslist55585$f471002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47216, %struct.ScmObj* %argslist55585$f471001)
store volatile %struct.ScmObj* %argslist55585$f471002, %struct.ScmObj** %stackaddr$prim57200, align 8
%stackaddr$prim57201 = alloca %struct.ScmObj*, align 8
%argslist55585$f471003 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47497, %struct.ScmObj* %argslist55585$f471002)
store volatile %struct.ScmObj* %argslist55585$f471003, %struct.ScmObj** %stackaddr$prim57201, align 8
%clofunc57202 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47100)
musttail call tailcc void %clofunc57202(%struct.ScmObj* %f47100, %struct.ScmObj* %argslist55585$f471003)
ret void
}

define tailcc void @proc_clo$ae47525(%struct.ScmObj* %env$ae47525,%struct.ScmObj* %current_45args55590) {
%stackaddr$prim57203 = alloca %struct.ScmObj*, align 8
%k47499 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55590)
store volatile %struct.ScmObj* %k47499, %struct.ScmObj** %stackaddr$prim57203, align 8
%stackaddr$prim57204 = alloca %struct.ScmObj*, align 8
%current_45args55591 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55590)
store volatile %struct.ScmObj* %current_45args55591, %struct.ScmObj** %stackaddr$prim57204, align 8
%stackaddr$prim57205 = alloca %struct.ScmObj*, align 8
%y47077 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55591)
store volatile %struct.ScmObj* %y47077, %struct.ScmObj** %stackaddr$prim57205, align 8
%ae47527 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57206 = alloca %struct.ScmObj*, align 8
%fptrToInt57207 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47528 to i64
%ae47528 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57207)
store volatile %struct.ScmObj* %ae47528, %struct.ScmObj** %stackaddr$makeclosure57206, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47528, %struct.ScmObj* %y47077, i64 0)
%argslist55609$k474990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57208 = alloca %struct.ScmObj*, align 8
%argslist55609$k474991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47528, %struct.ScmObj* %argslist55609$k474990)
store volatile %struct.ScmObj* %argslist55609$k474991, %struct.ScmObj** %stackaddr$prim57208, align 8
%stackaddr$prim57209 = alloca %struct.ScmObj*, align 8
%argslist55609$k474992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47527, %struct.ScmObj* %argslist55609$k474991)
store volatile %struct.ScmObj* %argslist55609$k474992, %struct.ScmObj** %stackaddr$prim57209, align 8
%clofunc57210 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47499)
musttail call tailcc void %clofunc57210(%struct.ScmObj* %k47499, %struct.ScmObj* %argslist55609$k474992)
ret void
}

define tailcc void @proc_clo$ae47528(%struct.ScmObj* %env$ae47528,%struct.ScmObj* %current_45args55593) {
%stackaddr$env-ref57211 = alloca %struct.ScmObj*, align 8
%y47077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47528, i64 0)
store %struct.ScmObj* %y47077, %struct.ScmObj** %stackaddr$env-ref57211
%stackaddr$prim57212 = alloca %struct.ScmObj*, align 8
%k47500 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55593)
store volatile %struct.ScmObj* %k47500, %struct.ScmObj** %stackaddr$prim57212, align 8
%stackaddr$prim57213 = alloca %struct.ScmObj*, align 8
%current_45args55594 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55593)
store volatile %struct.ScmObj* %current_45args55594, %struct.ScmObj** %stackaddr$prim57213, align 8
%stackaddr$prim57214 = alloca %struct.ScmObj*, align 8
%f47078 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55594)
store volatile %struct.ScmObj* %f47078, %struct.ScmObj** %stackaddr$prim57214, align 8
%stackaddr$makeclosure57215 = alloca %struct.ScmObj*, align 8
%fptrToInt57216 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47529 to i64
%ae47529 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57216)
store volatile %struct.ScmObj* %ae47529, %struct.ScmObj** %stackaddr$makeclosure57215, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47529, %struct.ScmObj* %f47078, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47529, %struct.ScmObj* %k47500, i64 1)
%ae47530 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57217 = alloca %struct.ScmObj*, align 8
%fptrToInt57218 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47531 to i64
%ae47531 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57218)
store volatile %struct.ScmObj* %ae47531, %struct.ScmObj** %stackaddr$makeclosure57217, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47531, %struct.ScmObj* %f47078, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47531, %struct.ScmObj* %y47077, i64 1)
%argslist55608$ae475290 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57219 = alloca %struct.ScmObj*, align 8
%argslist55608$ae475291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47531, %struct.ScmObj* %argslist55608$ae475290)
store volatile %struct.ScmObj* %argslist55608$ae475291, %struct.ScmObj** %stackaddr$prim57219, align 8
%stackaddr$prim57220 = alloca %struct.ScmObj*, align 8
%argslist55608$ae475292 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47530, %struct.ScmObj* %argslist55608$ae475291)
store volatile %struct.ScmObj* %argslist55608$ae475292, %struct.ScmObj** %stackaddr$prim57220, align 8
%clofunc57221 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47529)
musttail call tailcc void %clofunc57221(%struct.ScmObj* %ae47529, %struct.ScmObj* %argslist55608$ae475292)
ret void
}

define tailcc void @proc_clo$ae47529(%struct.ScmObj* %env$ae47529,%struct.ScmObj* %current_45args55596) {
%stackaddr$env-ref57222 = alloca %struct.ScmObj*, align 8
%f47078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47529, i64 0)
store %struct.ScmObj* %f47078, %struct.ScmObj** %stackaddr$env-ref57222
%stackaddr$env-ref57223 = alloca %struct.ScmObj*, align 8
%k47500 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47529, i64 1)
store %struct.ScmObj* %k47500, %struct.ScmObj** %stackaddr$env-ref57223
%stackaddr$prim57224 = alloca %struct.ScmObj*, align 8
%_95k47501 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55596)
store volatile %struct.ScmObj* %_95k47501, %struct.ScmObj** %stackaddr$prim57224, align 8
%stackaddr$prim57225 = alloca %struct.ScmObj*, align 8
%current_45args55597 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55596)
store volatile %struct.ScmObj* %current_45args55597, %struct.ScmObj** %stackaddr$prim57225, align 8
%stackaddr$prim57226 = alloca %struct.ScmObj*, align 8
%anf_45bind47213 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55597)
store volatile %struct.ScmObj* %anf_45bind47213, %struct.ScmObj** %stackaddr$prim57226, align 8
%argslist55599$f470780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57227 = alloca %struct.ScmObj*, align 8
%argslist55599$f470781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47213, %struct.ScmObj* %argslist55599$f470780)
store volatile %struct.ScmObj* %argslist55599$f470781, %struct.ScmObj** %stackaddr$prim57227, align 8
%stackaddr$prim57228 = alloca %struct.ScmObj*, align 8
%argslist55599$f470782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47500, %struct.ScmObj* %argslist55599$f470781)
store volatile %struct.ScmObj* %argslist55599$f470782, %struct.ScmObj** %stackaddr$prim57228, align 8
%clofunc57229 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47078)
musttail call tailcc void %clofunc57229(%struct.ScmObj* %f47078, %struct.ScmObj* %argslist55599$f470782)
ret void
}

define tailcc void @proc_clo$ae47531(%struct.ScmObj* %env$ae47531,%struct.ScmObj* %args4707947502) {
%stackaddr$env-ref57230 = alloca %struct.ScmObj*, align 8
%f47078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47531, i64 0)
store %struct.ScmObj* %f47078, %struct.ScmObj** %stackaddr$env-ref57230
%stackaddr$env-ref57231 = alloca %struct.ScmObj*, align 8
%y47077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47531, i64 1)
store %struct.ScmObj* %y47077, %struct.ScmObj** %stackaddr$env-ref57231
%stackaddr$prim57232 = alloca %struct.ScmObj*, align 8
%k47503 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4707947502)
store volatile %struct.ScmObj* %k47503, %struct.ScmObj** %stackaddr$prim57232, align 8
%stackaddr$prim57233 = alloca %struct.ScmObj*, align 8
%args47079 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4707947502)
store volatile %struct.ScmObj* %args47079, %struct.ScmObj** %stackaddr$prim57233, align 8
%stackaddr$makeclosure57234 = alloca %struct.ScmObj*, align 8
%fptrToInt57235 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47535 to i64
%ae47535 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57235)
store volatile %struct.ScmObj* %ae47535, %struct.ScmObj** %stackaddr$makeclosure57234, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47535, %struct.ScmObj* %f47078, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47535, %struct.ScmObj* %k47503, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47535, %struct.ScmObj* %args47079, i64 2)
%argslist55607$y470770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57236 = alloca %struct.ScmObj*, align 8
%argslist55607$y470771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y47077, %struct.ScmObj* %argslist55607$y470770)
store volatile %struct.ScmObj* %argslist55607$y470771, %struct.ScmObj** %stackaddr$prim57236, align 8
%stackaddr$prim57237 = alloca %struct.ScmObj*, align 8
%argslist55607$y470772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47535, %struct.ScmObj* %argslist55607$y470771)
store volatile %struct.ScmObj* %argslist55607$y470772, %struct.ScmObj** %stackaddr$prim57237, align 8
%clofunc57238 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y47077)
musttail call tailcc void %clofunc57238(%struct.ScmObj* %y47077, %struct.ScmObj* %argslist55607$y470772)
ret void
}

define tailcc void @proc_clo$ae47535(%struct.ScmObj* %env$ae47535,%struct.ScmObj* %current_45args55600) {
%stackaddr$env-ref57239 = alloca %struct.ScmObj*, align 8
%f47078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47535, i64 0)
store %struct.ScmObj* %f47078, %struct.ScmObj** %stackaddr$env-ref57239
%stackaddr$env-ref57240 = alloca %struct.ScmObj*, align 8
%k47503 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47535, i64 1)
store %struct.ScmObj* %k47503, %struct.ScmObj** %stackaddr$env-ref57240
%stackaddr$env-ref57241 = alloca %struct.ScmObj*, align 8
%args47079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47535, i64 2)
store %struct.ScmObj* %args47079, %struct.ScmObj** %stackaddr$env-ref57241
%stackaddr$prim57242 = alloca %struct.ScmObj*, align 8
%_95k47504 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55600)
store volatile %struct.ScmObj* %_95k47504, %struct.ScmObj** %stackaddr$prim57242, align 8
%stackaddr$prim57243 = alloca %struct.ScmObj*, align 8
%current_45args55601 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55600)
store volatile %struct.ScmObj* %current_45args55601, %struct.ScmObj** %stackaddr$prim57243, align 8
%stackaddr$prim57244 = alloca %struct.ScmObj*, align 8
%anf_45bind47211 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55601)
store volatile %struct.ScmObj* %anf_45bind47211, %struct.ScmObj** %stackaddr$prim57244, align 8
%stackaddr$makeclosure57245 = alloca %struct.ScmObj*, align 8
%fptrToInt57246 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47538 to i64
%ae47538 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57246)
store volatile %struct.ScmObj* %ae47538, %struct.ScmObj** %stackaddr$makeclosure57245, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47538, %struct.ScmObj* %k47503, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47538, %struct.ScmObj* %args47079, i64 1)
%argslist55606$anf_45bind472110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57247 = alloca %struct.ScmObj*, align 8
%argslist55606$anf_45bind472111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47078, %struct.ScmObj* %argslist55606$anf_45bind472110)
store volatile %struct.ScmObj* %argslist55606$anf_45bind472111, %struct.ScmObj** %stackaddr$prim57247, align 8
%stackaddr$prim57248 = alloca %struct.ScmObj*, align 8
%argslist55606$anf_45bind472112 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47538, %struct.ScmObj* %argslist55606$anf_45bind472111)
store volatile %struct.ScmObj* %argslist55606$anf_45bind472112, %struct.ScmObj** %stackaddr$prim57248, align 8
%clofunc57249 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47211)
musttail call tailcc void %clofunc57249(%struct.ScmObj* %anf_45bind47211, %struct.ScmObj* %argslist55606$anf_45bind472112)
ret void
}

define tailcc void @proc_clo$ae47538(%struct.ScmObj* %env$ae47538,%struct.ScmObj* %current_45args55603) {
%stackaddr$env-ref57250 = alloca %struct.ScmObj*, align 8
%k47503 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47538, i64 0)
store %struct.ScmObj* %k47503, %struct.ScmObj** %stackaddr$env-ref57250
%stackaddr$env-ref57251 = alloca %struct.ScmObj*, align 8
%args47079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47538, i64 1)
store %struct.ScmObj* %args47079, %struct.ScmObj** %stackaddr$env-ref57251
%stackaddr$prim57252 = alloca %struct.ScmObj*, align 8
%_95k47505 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55603)
store volatile %struct.ScmObj* %_95k47505, %struct.ScmObj** %stackaddr$prim57252, align 8
%stackaddr$prim57253 = alloca %struct.ScmObj*, align 8
%current_45args55604 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55603)
store volatile %struct.ScmObj* %current_45args55604, %struct.ScmObj** %stackaddr$prim57253, align 8
%stackaddr$prim57254 = alloca %struct.ScmObj*, align 8
%anf_45bind47212 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55604)
store volatile %struct.ScmObj* %anf_45bind47212, %struct.ScmObj** %stackaddr$prim57254, align 8
%stackaddr$prim57255 = alloca %struct.ScmObj*, align 8
%cpsargs47506 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47503, %struct.ScmObj* %args47079)
store volatile %struct.ScmObj* %cpsargs47506, %struct.ScmObj** %stackaddr$prim57255, align 8
%clofunc57256 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47212)
musttail call tailcc void %clofunc57256(%struct.ScmObj* %anf_45bind47212, %struct.ScmObj* %cpsargs47506)
ret void
}

define tailcc void @proc_clo$ae47510(%struct.ScmObj* %env$ae47510,%struct.ScmObj* %current_45args55611) {
%stackaddr$prim57257 = alloca %struct.ScmObj*, align 8
%k47507 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55611)
store volatile %struct.ScmObj* %k47507, %struct.ScmObj** %stackaddr$prim57257, align 8
%stackaddr$prim57258 = alloca %struct.ScmObj*, align 8
%current_45args55612 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55611)
store volatile %struct.ScmObj* %current_45args55612, %struct.ScmObj** %stackaddr$prim57258, align 8
%stackaddr$prim57259 = alloca %struct.ScmObj*, align 8
%yu47076 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55612)
store volatile %struct.ScmObj* %yu47076, %struct.ScmObj** %stackaddr$prim57259, align 8
%argslist55614$yu470760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57260 = alloca %struct.ScmObj*, align 8
%argslist55614$yu470761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu47076, %struct.ScmObj* %argslist55614$yu470760)
store volatile %struct.ScmObj* %argslist55614$yu470761, %struct.ScmObj** %stackaddr$prim57260, align 8
%stackaddr$prim57261 = alloca %struct.ScmObj*, align 8
%argslist55614$yu470762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47507, %struct.ScmObj* %argslist55614$yu470761)
store volatile %struct.ScmObj* %argslist55614$yu470762, %struct.ScmObj** %stackaddr$prim57261, align 8
%clofunc57262 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu47076)
musttail call tailcc void %clofunc57262(%struct.ScmObj* %yu47076, %struct.ScmObj* %argslist55614$yu470762)
ret void
}