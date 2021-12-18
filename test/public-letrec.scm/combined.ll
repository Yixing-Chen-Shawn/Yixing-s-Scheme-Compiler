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

@global$sym$ae4393949323 = private unnamed_addr constant [8 x i8] c"promise\00", align 8

define ccc i32 @main() {
%mainenv48868 = call %struct.ScmObj* @const_init_null()
%mainargs48869 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv48868, %struct.ScmObj* %mainargs48869)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv48866,%struct.ScmObj* %mainargs48867) {
%stackaddr$makeclosure48870 = alloca %struct.ScmObj*, align 8
%fptrToInt48871 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40551 to i64
%ae40551 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48871)
store volatile %struct.ScmObj* %ae40551, %struct.ScmObj** %stackaddr$makeclosure48870, align 8
%ae40552 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48872 = alloca %struct.ScmObj*, align 8
%fptrToInt48873 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40553 to i64
%ae40553 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48873)
store volatile %struct.ScmObj* %ae40553, %struct.ScmObj** %stackaddr$makeclosure48872, align 8
%args48865$ae40551$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48874 = alloca %struct.ScmObj*, align 8
%args48865$ae40551$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40553, %struct.ScmObj* %args48865$ae40551$0)
store volatile %struct.ScmObj* %args48865$ae40551$1, %struct.ScmObj** %stackaddr$prim48874, align 8
%stackaddr$prim48875 = alloca %struct.ScmObj*, align 8
%args48865$ae40551$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40552, %struct.ScmObj* %args48865$ae40551$1)
store volatile %struct.ScmObj* %args48865$ae40551$2, %struct.ScmObj** %stackaddr$prim48875, align 8
%clofunc48876 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40551)
musttail call tailcc void %clofunc48876(%struct.ScmObj* %ae40551, %struct.ScmObj* %args48865$ae40551$2)
ret void
}

define tailcc void @proc_clo$ae40551(%struct.ScmObj* %env$ae40551,%struct.ScmObj* %current_45args48329) {
%stackaddr$prim48877 = alloca %struct.ScmObj*, align 8
%_95k40387 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48329)
store volatile %struct.ScmObj* %_95k40387, %struct.ScmObj** %stackaddr$prim48877, align 8
%stackaddr$prim48878 = alloca %struct.ScmObj*, align 8
%current_45args48330 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48329)
store volatile %struct.ScmObj* %current_45args48330, %struct.ScmObj** %stackaddr$prim48878, align 8
%stackaddr$prim48879 = alloca %struct.ScmObj*, align 8
%anf_45bind40246 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48330)
store volatile %struct.ScmObj* %anf_45bind40246, %struct.ScmObj** %stackaddr$prim48879, align 8
%stackaddr$makeclosure48880 = alloca %struct.ScmObj*, align 8
%fptrToInt48881 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40566 to i64
%ae40566 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48881)
store volatile %struct.ScmObj* %ae40566, %struct.ScmObj** %stackaddr$makeclosure48880, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40566, %struct.ScmObj* %anf_45bind40246, i64 0)
%ae40567 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48882 = alloca %struct.ScmObj*, align 8
%fptrToInt48883 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40568 to i64
%ae40568 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48883)
store volatile %struct.ScmObj* %ae40568, %struct.ScmObj** %stackaddr$makeclosure48882, align 8
%args48860$ae40566$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48884 = alloca %struct.ScmObj*, align 8
%args48860$ae40566$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40568, %struct.ScmObj* %args48860$ae40566$0)
store volatile %struct.ScmObj* %args48860$ae40566$1, %struct.ScmObj** %stackaddr$prim48884, align 8
%stackaddr$prim48885 = alloca %struct.ScmObj*, align 8
%args48860$ae40566$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40567, %struct.ScmObj* %args48860$ae40566$1)
store volatile %struct.ScmObj* %args48860$ae40566$2, %struct.ScmObj** %stackaddr$prim48885, align 8
%clofunc48886 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40566)
musttail call tailcc void %clofunc48886(%struct.ScmObj* %ae40566, %struct.ScmObj* %args48860$ae40566$2)
ret void
}

define tailcc void @proc_clo$ae40566(%struct.ScmObj* %env$ae40566,%struct.ScmObj* %current_45args48332) {
%stackaddr$env-ref48887 = alloca %struct.ScmObj*, align 8
%anf_45bind40246 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40566, i64 0)
store %struct.ScmObj* %anf_45bind40246, %struct.ScmObj** %stackaddr$env-ref48887
%stackaddr$prim48888 = alloca %struct.ScmObj*, align 8
%_95k40388 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48332)
store volatile %struct.ScmObj* %_95k40388, %struct.ScmObj** %stackaddr$prim48888, align 8
%stackaddr$prim48889 = alloca %struct.ScmObj*, align 8
%current_45args48333 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48332)
store volatile %struct.ScmObj* %current_45args48333, %struct.ScmObj** %stackaddr$prim48889, align 8
%stackaddr$prim48890 = alloca %struct.ScmObj*, align 8
%anf_45bind40250 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48333)
store volatile %struct.ScmObj* %anf_45bind40250, %struct.ScmObj** %stackaddr$prim48890, align 8
%stackaddr$makeclosure48891 = alloca %struct.ScmObj*, align 8
%fptrToInt48892 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40681 to i64
%ae40681 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48892)
store volatile %struct.ScmObj* %ae40681, %struct.ScmObj** %stackaddr$makeclosure48891, align 8
%args48839$anf_45bind40246$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48893 = alloca %struct.ScmObj*, align 8
%args48839$anf_45bind40246$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40250, %struct.ScmObj* %args48839$anf_45bind40246$0)
store volatile %struct.ScmObj* %args48839$anf_45bind40246$1, %struct.ScmObj** %stackaddr$prim48893, align 8
%stackaddr$prim48894 = alloca %struct.ScmObj*, align 8
%args48839$anf_45bind40246$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40681, %struct.ScmObj* %args48839$anf_45bind40246$1)
store volatile %struct.ScmObj* %args48839$anf_45bind40246$2, %struct.ScmObj** %stackaddr$prim48894, align 8
%clofunc48895 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40246)
musttail call tailcc void %clofunc48895(%struct.ScmObj* %anf_45bind40246, %struct.ScmObj* %args48839$anf_45bind40246$2)
ret void
}

define tailcc void @proc_clo$ae40681(%struct.ScmObj* %env$ae40681,%struct.ScmObj* %current_45args48335) {
%stackaddr$prim48896 = alloca %struct.ScmObj*, align 8
%_95k40389 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48335)
store volatile %struct.ScmObj* %_95k40389, %struct.ScmObj** %stackaddr$prim48896, align 8
%stackaddr$prim48897 = alloca %struct.ScmObj*, align 8
%current_45args48336 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48335)
store volatile %struct.ScmObj* %current_45args48336, %struct.ScmObj** %stackaddr$prim48897, align 8
%stackaddr$prim48898 = alloca %struct.ScmObj*, align 8
%Ycmb40109 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48336)
store volatile %struct.ScmObj* %Ycmb40109, %struct.ScmObj** %stackaddr$prim48898, align 8
%stackaddr$makeclosure48899 = alloca %struct.ScmObj*, align 8
%fptrToInt48900 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40683 to i64
%ae40683 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48900)
store volatile %struct.ScmObj* %ae40683, %struct.ScmObj** %stackaddr$makeclosure48899, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40683, %struct.ScmObj* %Ycmb40109, i64 0)
%ae40684 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48901 = alloca %struct.ScmObj*, align 8
%fptrToInt48902 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40685 to i64
%ae40685 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48902)
store volatile %struct.ScmObj* %ae40685, %struct.ScmObj** %stackaddr$makeclosure48901, align 8
%args48838$ae40683$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48903 = alloca %struct.ScmObj*, align 8
%args48838$ae40683$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40685, %struct.ScmObj* %args48838$ae40683$0)
store volatile %struct.ScmObj* %args48838$ae40683$1, %struct.ScmObj** %stackaddr$prim48903, align 8
%stackaddr$prim48904 = alloca %struct.ScmObj*, align 8
%args48838$ae40683$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40684, %struct.ScmObj* %args48838$ae40683$1)
store volatile %struct.ScmObj* %args48838$ae40683$2, %struct.ScmObj** %stackaddr$prim48904, align 8
%clofunc48905 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40683)
musttail call tailcc void %clofunc48905(%struct.ScmObj* %ae40683, %struct.ScmObj* %args48838$ae40683$2)
ret void
}

define tailcc void @proc_clo$ae40683(%struct.ScmObj* %env$ae40683,%struct.ScmObj* %current_45args48338) {
%stackaddr$env-ref48906 = alloca %struct.ScmObj*, align 8
%Ycmb40109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40683, i64 0)
store %struct.ScmObj* %Ycmb40109, %struct.ScmObj** %stackaddr$env-ref48906
%stackaddr$prim48907 = alloca %struct.ScmObj*, align 8
%_95k40390 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48338)
store volatile %struct.ScmObj* %_95k40390, %struct.ScmObj** %stackaddr$prim48907, align 8
%stackaddr$prim48908 = alloca %struct.ScmObj*, align 8
%current_45args48339 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48338)
store volatile %struct.ScmObj* %current_45args48339, %struct.ScmObj** %stackaddr$prim48908, align 8
%stackaddr$prim48909 = alloca %struct.ScmObj*, align 8
%anf_45bind40255 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48339)
store volatile %struct.ScmObj* %anf_45bind40255, %struct.ScmObj** %stackaddr$prim48909, align 8
%stackaddr$makeclosure48910 = alloca %struct.ScmObj*, align 8
%fptrToInt48911 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40761 to i64
%ae40761 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48911)
store volatile %struct.ScmObj* %ae40761, %struct.ScmObj** %stackaddr$makeclosure48910, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40761, %struct.ScmObj* %Ycmb40109, i64 0)
%args48822$Ycmb40109$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48912 = alloca %struct.ScmObj*, align 8
%args48822$Ycmb40109$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40255, %struct.ScmObj* %args48822$Ycmb40109$0)
store volatile %struct.ScmObj* %args48822$Ycmb40109$1, %struct.ScmObj** %stackaddr$prim48912, align 8
%stackaddr$prim48913 = alloca %struct.ScmObj*, align 8
%args48822$Ycmb40109$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40761, %struct.ScmObj* %args48822$Ycmb40109$1)
store volatile %struct.ScmObj* %args48822$Ycmb40109$2, %struct.ScmObj** %stackaddr$prim48913, align 8
%clofunc48914 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40109)
musttail call tailcc void %clofunc48914(%struct.ScmObj* %Ycmb40109, %struct.ScmObj* %args48822$Ycmb40109$2)
ret void
}

define tailcc void @proc_clo$ae40761(%struct.ScmObj* %env$ae40761,%struct.ScmObj* %current_45args48341) {
%stackaddr$env-ref48915 = alloca %struct.ScmObj*, align 8
%Ycmb40109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40761, i64 0)
store %struct.ScmObj* %Ycmb40109, %struct.ScmObj** %stackaddr$env-ref48915
%stackaddr$prim48916 = alloca %struct.ScmObj*, align 8
%_95k40391 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48341)
store volatile %struct.ScmObj* %_95k40391, %struct.ScmObj** %stackaddr$prim48916, align 8
%stackaddr$prim48917 = alloca %struct.ScmObj*, align 8
%current_45args48342 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48341)
store volatile %struct.ScmObj* %current_45args48342, %struct.ScmObj** %stackaddr$prim48917, align 8
%stackaddr$prim48918 = alloca %struct.ScmObj*, align 8
%_37foldr140130 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48342)
store volatile %struct.ScmObj* %_37foldr140130, %struct.ScmObj** %stackaddr$prim48918, align 8
%stackaddr$makeclosure48919 = alloca %struct.ScmObj*, align 8
%fptrToInt48920 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40763 to i64
%ae40763 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48920)
store volatile %struct.ScmObj* %ae40763, %struct.ScmObj** %stackaddr$makeclosure48919, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40763, %struct.ScmObj* %_37foldr140130, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40763, %struct.ScmObj* %Ycmb40109, i64 1)
%ae40764 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48921 = alloca %struct.ScmObj*, align 8
%fptrToInt48922 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40765 to i64
%ae40765 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48922)
store volatile %struct.ScmObj* %ae40765, %struct.ScmObj** %stackaddr$makeclosure48921, align 8
%args48821$ae40763$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48923 = alloca %struct.ScmObj*, align 8
%args48821$ae40763$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40765, %struct.ScmObj* %args48821$ae40763$0)
store volatile %struct.ScmObj* %args48821$ae40763$1, %struct.ScmObj** %stackaddr$prim48923, align 8
%stackaddr$prim48924 = alloca %struct.ScmObj*, align 8
%args48821$ae40763$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40764, %struct.ScmObj* %args48821$ae40763$1)
store volatile %struct.ScmObj* %args48821$ae40763$2, %struct.ScmObj** %stackaddr$prim48924, align 8
%clofunc48925 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40763)
musttail call tailcc void %clofunc48925(%struct.ScmObj* %ae40763, %struct.ScmObj* %args48821$ae40763$2)
ret void
}

define tailcc void @proc_clo$ae40763(%struct.ScmObj* %env$ae40763,%struct.ScmObj* %current_45args48344) {
%stackaddr$env-ref48926 = alloca %struct.ScmObj*, align 8
%_37foldr140130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40763, i64 0)
store %struct.ScmObj* %_37foldr140130, %struct.ScmObj** %stackaddr$env-ref48926
%stackaddr$env-ref48927 = alloca %struct.ScmObj*, align 8
%Ycmb40109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40763, i64 1)
store %struct.ScmObj* %Ycmb40109, %struct.ScmObj** %stackaddr$env-ref48927
%stackaddr$prim48928 = alloca %struct.ScmObj*, align 8
%_95k40392 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48344)
store volatile %struct.ScmObj* %_95k40392, %struct.ScmObj** %stackaddr$prim48928, align 8
%stackaddr$prim48929 = alloca %struct.ScmObj*, align 8
%current_45args48345 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48344)
store volatile %struct.ScmObj* %current_45args48345, %struct.ScmObj** %stackaddr$prim48929, align 8
%stackaddr$prim48930 = alloca %struct.ScmObj*, align 8
%anf_45bind40261 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48345)
store volatile %struct.ScmObj* %anf_45bind40261, %struct.ScmObj** %stackaddr$prim48930, align 8
%stackaddr$makeclosure48931 = alloca %struct.ScmObj*, align 8
%fptrToInt48932 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40858 to i64
%ae40858 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48932)
store volatile %struct.ScmObj* %ae40858, %struct.ScmObj** %stackaddr$makeclosure48931, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40858, %struct.ScmObj* %_37foldr140130, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40858, %struct.ScmObj* %Ycmb40109, i64 1)
%args48802$Ycmb40109$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48933 = alloca %struct.ScmObj*, align 8
%args48802$Ycmb40109$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40261, %struct.ScmObj* %args48802$Ycmb40109$0)
store volatile %struct.ScmObj* %args48802$Ycmb40109$1, %struct.ScmObj** %stackaddr$prim48933, align 8
%stackaddr$prim48934 = alloca %struct.ScmObj*, align 8
%args48802$Ycmb40109$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40858, %struct.ScmObj* %args48802$Ycmb40109$1)
store volatile %struct.ScmObj* %args48802$Ycmb40109$2, %struct.ScmObj** %stackaddr$prim48934, align 8
%clofunc48935 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40109)
musttail call tailcc void %clofunc48935(%struct.ScmObj* %Ycmb40109, %struct.ScmObj* %args48802$Ycmb40109$2)
ret void
}

define tailcc void @proc_clo$ae40858(%struct.ScmObj* %env$ae40858,%struct.ScmObj* %current_45args48347) {
%stackaddr$env-ref48936 = alloca %struct.ScmObj*, align 8
%_37foldr140130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40858, i64 0)
store %struct.ScmObj* %_37foldr140130, %struct.ScmObj** %stackaddr$env-ref48936
%stackaddr$env-ref48937 = alloca %struct.ScmObj*, align 8
%Ycmb40109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40858, i64 1)
store %struct.ScmObj* %Ycmb40109, %struct.ScmObj** %stackaddr$env-ref48937
%stackaddr$prim48938 = alloca %struct.ScmObj*, align 8
%_95k40393 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48347)
store volatile %struct.ScmObj* %_95k40393, %struct.ScmObj** %stackaddr$prim48938, align 8
%stackaddr$prim48939 = alloca %struct.ScmObj*, align 8
%current_45args48348 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48347)
store volatile %struct.ScmObj* %current_45args48348, %struct.ScmObj** %stackaddr$prim48939, align 8
%stackaddr$prim48940 = alloca %struct.ScmObj*, align 8
%_37map140126 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48348)
store volatile %struct.ScmObj* %_37map140126, %struct.ScmObj** %stackaddr$prim48940, align 8
%stackaddr$makeclosure48941 = alloca %struct.ScmObj*, align 8
%fptrToInt48942 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40860 to i64
%ae40860 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48942)
store volatile %struct.ScmObj* %ae40860, %struct.ScmObj** %stackaddr$makeclosure48941, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40860, %struct.ScmObj* %_37foldr140130, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40860, %struct.ScmObj* %_37map140126, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40860, %struct.ScmObj* %Ycmb40109, i64 2)
%ae40861 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48943 = alloca %struct.ScmObj*, align 8
%fptrToInt48944 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40862 to i64
%ae40862 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48944)
store volatile %struct.ScmObj* %ae40862, %struct.ScmObj** %stackaddr$makeclosure48943, align 8
%args48801$ae40860$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48945 = alloca %struct.ScmObj*, align 8
%args48801$ae40860$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40862, %struct.ScmObj* %args48801$ae40860$0)
store volatile %struct.ScmObj* %args48801$ae40860$1, %struct.ScmObj** %stackaddr$prim48945, align 8
%stackaddr$prim48946 = alloca %struct.ScmObj*, align 8
%args48801$ae40860$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40861, %struct.ScmObj* %args48801$ae40860$1)
store volatile %struct.ScmObj* %args48801$ae40860$2, %struct.ScmObj** %stackaddr$prim48946, align 8
%clofunc48947 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40860)
musttail call tailcc void %clofunc48947(%struct.ScmObj* %ae40860, %struct.ScmObj* %args48801$ae40860$2)
ret void
}

define tailcc void @proc_clo$ae40860(%struct.ScmObj* %env$ae40860,%struct.ScmObj* %current_45args48350) {
%stackaddr$env-ref48948 = alloca %struct.ScmObj*, align 8
%_37foldr140130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40860, i64 0)
store %struct.ScmObj* %_37foldr140130, %struct.ScmObj** %stackaddr$env-ref48948
%stackaddr$env-ref48949 = alloca %struct.ScmObj*, align 8
%_37map140126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40860, i64 1)
store %struct.ScmObj* %_37map140126, %struct.ScmObj** %stackaddr$env-ref48949
%stackaddr$env-ref48950 = alloca %struct.ScmObj*, align 8
%Ycmb40109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40860, i64 2)
store %struct.ScmObj* %Ycmb40109, %struct.ScmObj** %stackaddr$env-ref48950
%stackaddr$prim48951 = alloca %struct.ScmObj*, align 8
%_95k40394 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48350)
store volatile %struct.ScmObj* %_95k40394, %struct.ScmObj** %stackaddr$prim48951, align 8
%stackaddr$prim48952 = alloca %struct.ScmObj*, align 8
%current_45args48351 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48350)
store volatile %struct.ScmObj* %current_45args48351, %struct.ScmObj** %stackaddr$prim48952, align 8
%stackaddr$prim48953 = alloca %struct.ScmObj*, align 8
%anf_45bind40268 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48351)
store volatile %struct.ScmObj* %anf_45bind40268, %struct.ScmObj** %stackaddr$prim48953, align 8
%stackaddr$makeclosure48954 = alloca %struct.ScmObj*, align 8
%fptrToInt48955 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41008 to i64
%ae41008 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48955)
store volatile %struct.ScmObj* %ae41008, %struct.ScmObj** %stackaddr$makeclosure48954, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41008, %struct.ScmObj* %_37foldr140130, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41008, %struct.ScmObj* %_37map140126, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41008, %struct.ScmObj* %Ycmb40109, i64 2)
%args48785$Ycmb40109$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48956 = alloca %struct.ScmObj*, align 8
%args48785$Ycmb40109$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40268, %struct.ScmObj* %args48785$Ycmb40109$0)
store volatile %struct.ScmObj* %args48785$Ycmb40109$1, %struct.ScmObj** %stackaddr$prim48956, align 8
%stackaddr$prim48957 = alloca %struct.ScmObj*, align 8
%args48785$Ycmb40109$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41008, %struct.ScmObj* %args48785$Ycmb40109$1)
store volatile %struct.ScmObj* %args48785$Ycmb40109$2, %struct.ScmObj** %stackaddr$prim48957, align 8
%clofunc48958 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40109)
musttail call tailcc void %clofunc48958(%struct.ScmObj* %Ycmb40109, %struct.ScmObj* %args48785$Ycmb40109$2)
ret void
}

define tailcc void @proc_clo$ae41008(%struct.ScmObj* %env$ae41008,%struct.ScmObj* %current_45args48353) {
%stackaddr$env-ref48959 = alloca %struct.ScmObj*, align 8
%_37foldr140130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41008, i64 0)
store %struct.ScmObj* %_37foldr140130, %struct.ScmObj** %stackaddr$env-ref48959
%stackaddr$env-ref48960 = alloca %struct.ScmObj*, align 8
%_37map140126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41008, i64 1)
store %struct.ScmObj* %_37map140126, %struct.ScmObj** %stackaddr$env-ref48960
%stackaddr$env-ref48961 = alloca %struct.ScmObj*, align 8
%Ycmb40109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41008, i64 2)
store %struct.ScmObj* %Ycmb40109, %struct.ScmObj** %stackaddr$env-ref48961
%stackaddr$prim48962 = alloca %struct.ScmObj*, align 8
%_95k40395 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48353)
store volatile %struct.ScmObj* %_95k40395, %struct.ScmObj** %stackaddr$prim48962, align 8
%stackaddr$prim48963 = alloca %struct.ScmObj*, align 8
%current_45args48354 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48353)
store volatile %struct.ScmObj* %current_45args48354, %struct.ScmObj** %stackaddr$prim48963, align 8
%stackaddr$prim48964 = alloca %struct.ScmObj*, align 8
%_37take40122 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48354)
store volatile %struct.ScmObj* %_37take40122, %struct.ScmObj** %stackaddr$prim48964, align 8
%stackaddr$makeclosure48965 = alloca %struct.ScmObj*, align 8
%fptrToInt48966 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41010 to i64
%ae41010 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48966)
store volatile %struct.ScmObj* %ae41010, %struct.ScmObj** %stackaddr$makeclosure48965, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41010, %struct.ScmObj* %_37foldr140130, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41010, %struct.ScmObj* %_37map140126, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41010, %struct.ScmObj* %Ycmb40109, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41010, %struct.ScmObj* %_37take40122, i64 3)
%ae41011 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48967 = alloca %struct.ScmObj*, align 8
%fptrToInt48968 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41012 to i64
%ae41012 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48968)
store volatile %struct.ScmObj* %ae41012, %struct.ScmObj** %stackaddr$makeclosure48967, align 8
%args48784$ae41010$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48969 = alloca %struct.ScmObj*, align 8
%args48784$ae41010$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41012, %struct.ScmObj* %args48784$ae41010$0)
store volatile %struct.ScmObj* %args48784$ae41010$1, %struct.ScmObj** %stackaddr$prim48969, align 8
%stackaddr$prim48970 = alloca %struct.ScmObj*, align 8
%args48784$ae41010$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41011, %struct.ScmObj* %args48784$ae41010$1)
store volatile %struct.ScmObj* %args48784$ae41010$2, %struct.ScmObj** %stackaddr$prim48970, align 8
%clofunc48971 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41010)
musttail call tailcc void %clofunc48971(%struct.ScmObj* %ae41010, %struct.ScmObj* %args48784$ae41010$2)
ret void
}

define tailcc void @proc_clo$ae41010(%struct.ScmObj* %env$ae41010,%struct.ScmObj* %current_45args48356) {
%stackaddr$env-ref48972 = alloca %struct.ScmObj*, align 8
%_37foldr140130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41010, i64 0)
store %struct.ScmObj* %_37foldr140130, %struct.ScmObj** %stackaddr$env-ref48972
%stackaddr$env-ref48973 = alloca %struct.ScmObj*, align 8
%_37map140126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41010, i64 1)
store %struct.ScmObj* %_37map140126, %struct.ScmObj** %stackaddr$env-ref48973
%stackaddr$env-ref48974 = alloca %struct.ScmObj*, align 8
%Ycmb40109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41010, i64 2)
store %struct.ScmObj* %Ycmb40109, %struct.ScmObj** %stackaddr$env-ref48974
%stackaddr$env-ref48975 = alloca %struct.ScmObj*, align 8
%_37take40122 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41010, i64 3)
store %struct.ScmObj* %_37take40122, %struct.ScmObj** %stackaddr$env-ref48975
%stackaddr$prim48976 = alloca %struct.ScmObj*, align 8
%_95k40396 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48356)
store volatile %struct.ScmObj* %_95k40396, %struct.ScmObj** %stackaddr$prim48976, align 8
%stackaddr$prim48977 = alloca %struct.ScmObj*, align 8
%current_45args48357 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48356)
store volatile %struct.ScmObj* %current_45args48357, %struct.ScmObj** %stackaddr$prim48977, align 8
%stackaddr$prim48978 = alloca %struct.ScmObj*, align 8
%anf_45bind40272 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48357)
store volatile %struct.ScmObj* %anf_45bind40272, %struct.ScmObj** %stackaddr$prim48978, align 8
%stackaddr$makeclosure48979 = alloca %struct.ScmObj*, align 8
%fptrToInt48980 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41091 to i64
%ae41091 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48980)
store volatile %struct.ScmObj* %ae41091, %struct.ScmObj** %stackaddr$makeclosure48979, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41091, %struct.ScmObj* %_37foldr140130, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41091, %struct.ScmObj* %_37map140126, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41091, %struct.ScmObj* %Ycmb40109, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41091, %struct.ScmObj* %_37take40122, i64 3)
%args48770$Ycmb40109$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48981 = alloca %struct.ScmObj*, align 8
%args48770$Ycmb40109$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40272, %struct.ScmObj* %args48770$Ycmb40109$0)
store volatile %struct.ScmObj* %args48770$Ycmb40109$1, %struct.ScmObj** %stackaddr$prim48981, align 8
%stackaddr$prim48982 = alloca %struct.ScmObj*, align 8
%args48770$Ycmb40109$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41091, %struct.ScmObj* %args48770$Ycmb40109$1)
store volatile %struct.ScmObj* %args48770$Ycmb40109$2, %struct.ScmObj** %stackaddr$prim48982, align 8
%clofunc48983 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40109)
musttail call tailcc void %clofunc48983(%struct.ScmObj* %Ycmb40109, %struct.ScmObj* %args48770$Ycmb40109$2)
ret void
}

define tailcc void @proc_clo$ae41091(%struct.ScmObj* %env$ae41091,%struct.ScmObj* %current_45args48359) {
%stackaddr$env-ref48984 = alloca %struct.ScmObj*, align 8
%_37foldr140130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41091, i64 0)
store %struct.ScmObj* %_37foldr140130, %struct.ScmObj** %stackaddr$env-ref48984
%stackaddr$env-ref48985 = alloca %struct.ScmObj*, align 8
%_37map140126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41091, i64 1)
store %struct.ScmObj* %_37map140126, %struct.ScmObj** %stackaddr$env-ref48985
%stackaddr$env-ref48986 = alloca %struct.ScmObj*, align 8
%Ycmb40109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41091, i64 2)
store %struct.ScmObj* %Ycmb40109, %struct.ScmObj** %stackaddr$env-ref48986
%stackaddr$env-ref48987 = alloca %struct.ScmObj*, align 8
%_37take40122 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41091, i64 3)
store %struct.ScmObj* %_37take40122, %struct.ScmObj** %stackaddr$env-ref48987
%stackaddr$prim48988 = alloca %struct.ScmObj*, align 8
%_95k40397 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48359)
store volatile %struct.ScmObj* %_95k40397, %struct.ScmObj** %stackaddr$prim48988, align 8
%stackaddr$prim48989 = alloca %struct.ScmObj*, align 8
%current_45args48360 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48359)
store volatile %struct.ScmObj* %current_45args48360, %struct.ScmObj** %stackaddr$prim48989, align 8
%stackaddr$prim48990 = alloca %struct.ScmObj*, align 8
%_37length40119 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48360)
store volatile %struct.ScmObj* %_37length40119, %struct.ScmObj** %stackaddr$prim48990, align 8
%stackaddr$makeclosure48991 = alloca %struct.ScmObj*, align 8
%fptrToInt48992 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41093 to i64
%ae41093 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt48992)
store volatile %struct.ScmObj* %ae41093, %struct.ScmObj** %stackaddr$makeclosure48991, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41093, %struct.ScmObj* %_37length40119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41093, %struct.ScmObj* %_37foldr140130, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41093, %struct.ScmObj* %_37map140126, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41093, %struct.ScmObj* %Ycmb40109, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41093, %struct.ScmObj* %_37take40122, i64 4)
%ae41094 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48993 = alloca %struct.ScmObj*, align 8
%fptrToInt48994 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41095 to i64
%ae41095 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48994)
store volatile %struct.ScmObj* %ae41095, %struct.ScmObj** %stackaddr$makeclosure48993, align 8
%args48769$ae41093$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48995 = alloca %struct.ScmObj*, align 8
%args48769$ae41093$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41095, %struct.ScmObj* %args48769$ae41093$0)
store volatile %struct.ScmObj* %args48769$ae41093$1, %struct.ScmObj** %stackaddr$prim48995, align 8
%stackaddr$prim48996 = alloca %struct.ScmObj*, align 8
%args48769$ae41093$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41094, %struct.ScmObj* %args48769$ae41093$1)
store volatile %struct.ScmObj* %args48769$ae41093$2, %struct.ScmObj** %stackaddr$prim48996, align 8
%clofunc48997 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41093)
musttail call tailcc void %clofunc48997(%struct.ScmObj* %ae41093, %struct.ScmObj* %args48769$ae41093$2)
ret void
}

define tailcc void @proc_clo$ae41093(%struct.ScmObj* %env$ae41093,%struct.ScmObj* %current_45args48362) {
%stackaddr$env-ref48998 = alloca %struct.ScmObj*, align 8
%_37length40119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41093, i64 0)
store %struct.ScmObj* %_37length40119, %struct.ScmObj** %stackaddr$env-ref48998
%stackaddr$env-ref48999 = alloca %struct.ScmObj*, align 8
%_37foldr140130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41093, i64 1)
store %struct.ScmObj* %_37foldr140130, %struct.ScmObj** %stackaddr$env-ref48999
%stackaddr$env-ref49000 = alloca %struct.ScmObj*, align 8
%_37map140126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41093, i64 2)
store %struct.ScmObj* %_37map140126, %struct.ScmObj** %stackaddr$env-ref49000
%stackaddr$env-ref49001 = alloca %struct.ScmObj*, align 8
%Ycmb40109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41093, i64 3)
store %struct.ScmObj* %Ycmb40109, %struct.ScmObj** %stackaddr$env-ref49001
%stackaddr$env-ref49002 = alloca %struct.ScmObj*, align 8
%_37take40122 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41093, i64 4)
store %struct.ScmObj* %_37take40122, %struct.ScmObj** %stackaddr$env-ref49002
%stackaddr$prim49003 = alloca %struct.ScmObj*, align 8
%_95k40398 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48362)
store volatile %struct.ScmObj* %_95k40398, %struct.ScmObj** %stackaddr$prim49003, align 8
%stackaddr$prim49004 = alloca %struct.ScmObj*, align 8
%current_45args48363 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48362)
store volatile %struct.ScmObj* %current_45args48363, %struct.ScmObj** %stackaddr$prim49004, align 8
%stackaddr$prim49005 = alloca %struct.ScmObj*, align 8
%anf_45bind40277 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48363)
store volatile %struct.ScmObj* %anf_45bind40277, %struct.ScmObj** %stackaddr$prim49005, align 8
%stackaddr$makeclosure49006 = alloca %struct.ScmObj*, align 8
%fptrToInt49007 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41170 to i64
%ae41170 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt49007)
store volatile %struct.ScmObj* %ae41170, %struct.ScmObj** %stackaddr$makeclosure49006, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41170, %struct.ScmObj* %_37length40119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41170, %struct.ScmObj* %_37foldr140130, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41170, %struct.ScmObj* %_37map140126, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41170, %struct.ScmObj* %Ycmb40109, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41170, %struct.ScmObj* %_37take40122, i64 4)
%args48753$Ycmb40109$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49008 = alloca %struct.ScmObj*, align 8
%args48753$Ycmb40109$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40277, %struct.ScmObj* %args48753$Ycmb40109$0)
store volatile %struct.ScmObj* %args48753$Ycmb40109$1, %struct.ScmObj** %stackaddr$prim49008, align 8
%stackaddr$prim49009 = alloca %struct.ScmObj*, align 8
%args48753$Ycmb40109$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41170, %struct.ScmObj* %args48753$Ycmb40109$1)
store volatile %struct.ScmObj* %args48753$Ycmb40109$2, %struct.ScmObj** %stackaddr$prim49009, align 8
%clofunc49010 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40109)
musttail call tailcc void %clofunc49010(%struct.ScmObj* %Ycmb40109, %struct.ScmObj* %args48753$Ycmb40109$2)
ret void
}

define tailcc void @proc_clo$ae41170(%struct.ScmObj* %env$ae41170,%struct.ScmObj* %current_45args48365) {
%stackaddr$env-ref49011 = alloca %struct.ScmObj*, align 8
%_37length40119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41170, i64 0)
store %struct.ScmObj* %_37length40119, %struct.ScmObj** %stackaddr$env-ref49011
%stackaddr$env-ref49012 = alloca %struct.ScmObj*, align 8
%_37foldr140130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41170, i64 1)
store %struct.ScmObj* %_37foldr140130, %struct.ScmObj** %stackaddr$env-ref49012
%stackaddr$env-ref49013 = alloca %struct.ScmObj*, align 8
%_37map140126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41170, i64 2)
store %struct.ScmObj* %_37map140126, %struct.ScmObj** %stackaddr$env-ref49013
%stackaddr$env-ref49014 = alloca %struct.ScmObj*, align 8
%Ycmb40109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41170, i64 3)
store %struct.ScmObj* %Ycmb40109, %struct.ScmObj** %stackaddr$env-ref49014
%stackaddr$env-ref49015 = alloca %struct.ScmObj*, align 8
%_37take40122 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41170, i64 4)
store %struct.ScmObj* %_37take40122, %struct.ScmObj** %stackaddr$env-ref49015
%stackaddr$prim49016 = alloca %struct.ScmObj*, align 8
%_95k40399 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48365)
store volatile %struct.ScmObj* %_95k40399, %struct.ScmObj** %stackaddr$prim49016, align 8
%stackaddr$prim49017 = alloca %struct.ScmObj*, align 8
%current_45args48366 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48365)
store volatile %struct.ScmObj* %current_45args48366, %struct.ScmObj** %stackaddr$prim49017, align 8
%stackaddr$prim49018 = alloca %struct.ScmObj*, align 8
%_37foldl140114 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48366)
store volatile %struct.ScmObj* %_37foldl140114, %struct.ScmObj** %stackaddr$prim49018, align 8
%stackaddr$makeclosure49019 = alloca %struct.ScmObj*, align 8
%fptrToInt49020 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41172 to i64
%ae41172 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt49020)
store volatile %struct.ScmObj* %ae41172, %struct.ScmObj** %stackaddr$makeclosure49019, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41172, %struct.ScmObj* %_37foldr140130, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41172, %struct.ScmObj* %_37foldl140114, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41172, %struct.ScmObj* %_37length40119, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41172, %struct.ScmObj* %_37map140126, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41172, %struct.ScmObj* %Ycmb40109, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41172, %struct.ScmObj* %_37take40122, i64 5)
%ae41173 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49021 = alloca %struct.ScmObj*, align 8
%fptrToInt49022 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41174 to i64
%ae41174 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49022)
store volatile %struct.ScmObj* %ae41174, %struct.ScmObj** %stackaddr$makeclosure49021, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41174, %struct.ScmObj* %_37foldl140114, i64 0)
%args48752$ae41172$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49023 = alloca %struct.ScmObj*, align 8
%args48752$ae41172$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41174, %struct.ScmObj* %args48752$ae41172$0)
store volatile %struct.ScmObj* %args48752$ae41172$1, %struct.ScmObj** %stackaddr$prim49023, align 8
%stackaddr$prim49024 = alloca %struct.ScmObj*, align 8
%args48752$ae41172$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41173, %struct.ScmObj* %args48752$ae41172$1)
store volatile %struct.ScmObj* %args48752$ae41172$2, %struct.ScmObj** %stackaddr$prim49024, align 8
%clofunc49025 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41172)
musttail call tailcc void %clofunc49025(%struct.ScmObj* %ae41172, %struct.ScmObj* %args48752$ae41172$2)
ret void
}

define tailcc void @proc_clo$ae41172(%struct.ScmObj* %env$ae41172,%struct.ScmObj* %current_45args48368) {
%stackaddr$env-ref49026 = alloca %struct.ScmObj*, align 8
%_37foldr140130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41172, i64 0)
store %struct.ScmObj* %_37foldr140130, %struct.ScmObj** %stackaddr$env-ref49026
%stackaddr$env-ref49027 = alloca %struct.ScmObj*, align 8
%_37foldl140114 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41172, i64 1)
store %struct.ScmObj* %_37foldl140114, %struct.ScmObj** %stackaddr$env-ref49027
%stackaddr$env-ref49028 = alloca %struct.ScmObj*, align 8
%_37length40119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41172, i64 2)
store %struct.ScmObj* %_37length40119, %struct.ScmObj** %stackaddr$env-ref49028
%stackaddr$env-ref49029 = alloca %struct.ScmObj*, align 8
%_37map140126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41172, i64 3)
store %struct.ScmObj* %_37map140126, %struct.ScmObj** %stackaddr$env-ref49029
%stackaddr$env-ref49030 = alloca %struct.ScmObj*, align 8
%Ycmb40109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41172, i64 4)
store %struct.ScmObj* %Ycmb40109, %struct.ScmObj** %stackaddr$env-ref49030
%stackaddr$env-ref49031 = alloca %struct.ScmObj*, align 8
%_37take40122 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41172, i64 5)
store %struct.ScmObj* %_37take40122, %struct.ScmObj** %stackaddr$env-ref49031
%stackaddr$prim49032 = alloca %struct.ScmObj*, align 8
%_95k40400 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48368)
store volatile %struct.ScmObj* %_95k40400, %struct.ScmObj** %stackaddr$prim49032, align 8
%stackaddr$prim49033 = alloca %struct.ScmObj*, align 8
%current_45args48369 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48368)
store volatile %struct.ScmObj* %current_45args48369, %struct.ScmObj** %stackaddr$prim49033, align 8
%stackaddr$prim49034 = alloca %struct.ScmObj*, align 8
%_37last40152 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48369)
store volatile %struct.ScmObj* %_37last40152, %struct.ScmObj** %stackaddr$prim49034, align 8
%stackaddr$makeclosure49035 = alloca %struct.ScmObj*, align 8
%fptrToInt49036 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41226 to i64
%ae41226 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt49036)
store volatile %struct.ScmObj* %ae41226, %struct.ScmObj** %stackaddr$makeclosure49035, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41226, %struct.ScmObj* %_37foldr140130, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41226, %struct.ScmObj* %_37foldl140114, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41226, %struct.ScmObj* %_37last40152, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41226, %struct.ScmObj* %_37map140126, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41226, %struct.ScmObj* %Ycmb40109, i64 4)
%ae41227 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49037 = alloca %struct.ScmObj*, align 8
%fptrToInt49038 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41228 to i64
%ae41228 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49038)
store volatile %struct.ScmObj* %ae41228, %struct.ScmObj** %stackaddr$makeclosure49037, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41228, %struct.ScmObj* %_37length40119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41228, %struct.ScmObj* %_37take40122, i64 1)
%args48738$ae41226$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49039 = alloca %struct.ScmObj*, align 8
%args48738$ae41226$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41228, %struct.ScmObj* %args48738$ae41226$0)
store volatile %struct.ScmObj* %args48738$ae41226$1, %struct.ScmObj** %stackaddr$prim49039, align 8
%stackaddr$prim49040 = alloca %struct.ScmObj*, align 8
%args48738$ae41226$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41227, %struct.ScmObj* %args48738$ae41226$1)
store volatile %struct.ScmObj* %args48738$ae41226$2, %struct.ScmObj** %stackaddr$prim49040, align 8
%clofunc49041 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41226)
musttail call tailcc void %clofunc49041(%struct.ScmObj* %ae41226, %struct.ScmObj* %args48738$ae41226$2)
ret void
}

define tailcc void @proc_clo$ae41226(%struct.ScmObj* %env$ae41226,%struct.ScmObj* %current_45args48371) {
%stackaddr$env-ref49042 = alloca %struct.ScmObj*, align 8
%_37foldr140130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41226, i64 0)
store %struct.ScmObj* %_37foldr140130, %struct.ScmObj** %stackaddr$env-ref49042
%stackaddr$env-ref49043 = alloca %struct.ScmObj*, align 8
%_37foldl140114 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41226, i64 1)
store %struct.ScmObj* %_37foldl140114, %struct.ScmObj** %stackaddr$env-ref49043
%stackaddr$env-ref49044 = alloca %struct.ScmObj*, align 8
%_37last40152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41226, i64 2)
store %struct.ScmObj* %_37last40152, %struct.ScmObj** %stackaddr$env-ref49044
%stackaddr$env-ref49045 = alloca %struct.ScmObj*, align 8
%_37map140126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41226, i64 3)
store %struct.ScmObj* %_37map140126, %struct.ScmObj** %stackaddr$env-ref49045
%stackaddr$env-ref49046 = alloca %struct.ScmObj*, align 8
%Ycmb40109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41226, i64 4)
store %struct.ScmObj* %Ycmb40109, %struct.ScmObj** %stackaddr$env-ref49046
%stackaddr$prim49047 = alloca %struct.ScmObj*, align 8
%_95k40401 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48371)
store volatile %struct.ScmObj* %_95k40401, %struct.ScmObj** %stackaddr$prim49047, align 8
%stackaddr$prim49048 = alloca %struct.ScmObj*, align 8
%current_45args48372 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48371)
store volatile %struct.ScmObj* %current_45args48372, %struct.ScmObj** %stackaddr$prim49048, align 8
%stackaddr$prim49049 = alloca %struct.ScmObj*, align 8
%_37drop_45right40149 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48372)
store volatile %struct.ScmObj* %_37drop_45right40149, %struct.ScmObj** %stackaddr$prim49049, align 8
%stackaddr$makeclosure49050 = alloca %struct.ScmObj*, align 8
%fptrToInt49051 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41256 to i64
%ae41256 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt49051)
store volatile %struct.ScmObj* %ae41256, %struct.ScmObj** %stackaddr$makeclosure49050, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41256, %struct.ScmObj* %_37foldr140130, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41256, %struct.ScmObj* %_37foldl140114, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41256, %struct.ScmObj* %_37last40152, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41256, %struct.ScmObj* %_37drop_45right40149, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41256, %struct.ScmObj* %Ycmb40109, i64 4)
%ae41257 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49052 = alloca %struct.ScmObj*, align 8
%fptrToInt49053 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41258 to i64
%ae41258 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49053)
store volatile %struct.ScmObj* %ae41258, %struct.ScmObj** %stackaddr$makeclosure49052, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41258, %struct.ScmObj* %_37foldr140130, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41258, %struct.ScmObj* %_37map140126, i64 1)
%args48728$ae41256$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49054 = alloca %struct.ScmObj*, align 8
%args48728$ae41256$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41258, %struct.ScmObj* %args48728$ae41256$0)
store volatile %struct.ScmObj* %args48728$ae41256$1, %struct.ScmObj** %stackaddr$prim49054, align 8
%stackaddr$prim49055 = alloca %struct.ScmObj*, align 8
%args48728$ae41256$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41257, %struct.ScmObj* %args48728$ae41256$1)
store volatile %struct.ScmObj* %args48728$ae41256$2, %struct.ScmObj** %stackaddr$prim49055, align 8
%clofunc49056 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41256)
musttail call tailcc void %clofunc49056(%struct.ScmObj* %ae41256, %struct.ScmObj* %args48728$ae41256$2)
ret void
}

define tailcc void @proc_clo$ae41256(%struct.ScmObj* %env$ae41256,%struct.ScmObj* %current_45args48374) {
%stackaddr$env-ref49057 = alloca %struct.ScmObj*, align 8
%_37foldr140130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41256, i64 0)
store %struct.ScmObj* %_37foldr140130, %struct.ScmObj** %stackaddr$env-ref49057
%stackaddr$env-ref49058 = alloca %struct.ScmObj*, align 8
%_37foldl140114 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41256, i64 1)
store %struct.ScmObj* %_37foldl140114, %struct.ScmObj** %stackaddr$env-ref49058
%stackaddr$env-ref49059 = alloca %struct.ScmObj*, align 8
%_37last40152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41256, i64 2)
store %struct.ScmObj* %_37last40152, %struct.ScmObj** %stackaddr$env-ref49059
%stackaddr$env-ref49060 = alloca %struct.ScmObj*, align 8
%_37drop_45right40149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41256, i64 3)
store %struct.ScmObj* %_37drop_45right40149, %struct.ScmObj** %stackaddr$env-ref49060
%stackaddr$env-ref49061 = alloca %struct.ScmObj*, align 8
%Ycmb40109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41256, i64 4)
store %struct.ScmObj* %Ycmb40109, %struct.ScmObj** %stackaddr$env-ref49061
%stackaddr$prim49062 = alloca %struct.ScmObj*, align 8
%_95k40402 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48374)
store volatile %struct.ScmObj* %_95k40402, %struct.ScmObj** %stackaddr$prim49062, align 8
%stackaddr$prim49063 = alloca %struct.ScmObj*, align 8
%current_45args48375 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48374)
store volatile %struct.ScmObj* %current_45args48375, %struct.ScmObj** %stackaddr$prim49063, align 8
%stackaddr$prim49064 = alloca %struct.ScmObj*, align 8
%anf_45bind40293 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48375)
store volatile %struct.ScmObj* %anf_45bind40293, %struct.ScmObj** %stackaddr$prim49064, align 8
%stackaddr$makeclosure49065 = alloca %struct.ScmObj*, align 8
%fptrToInt49066 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41640 to i64
%ae41640 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt49066)
store volatile %struct.ScmObj* %ae41640, %struct.ScmObj** %stackaddr$makeclosure49065, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41640, %struct.ScmObj* %_37foldr140130, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41640, %struct.ScmObj* %_37foldl140114, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41640, %struct.ScmObj* %_37last40152, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41640, %struct.ScmObj* %_37drop_45right40149, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41640, %struct.ScmObj* %Ycmb40109, i64 4)
%args48668$Ycmb40109$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49067 = alloca %struct.ScmObj*, align 8
%args48668$Ycmb40109$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40293, %struct.ScmObj* %args48668$Ycmb40109$0)
store volatile %struct.ScmObj* %args48668$Ycmb40109$1, %struct.ScmObj** %stackaddr$prim49067, align 8
%stackaddr$prim49068 = alloca %struct.ScmObj*, align 8
%args48668$Ycmb40109$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41640, %struct.ScmObj* %args48668$Ycmb40109$1)
store volatile %struct.ScmObj* %args48668$Ycmb40109$2, %struct.ScmObj** %stackaddr$prim49068, align 8
%clofunc49069 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40109)
musttail call tailcc void %clofunc49069(%struct.ScmObj* %Ycmb40109, %struct.ScmObj* %args48668$Ycmb40109$2)
ret void
}

define tailcc void @proc_clo$ae41640(%struct.ScmObj* %env$ae41640,%struct.ScmObj* %current_45args48377) {
%stackaddr$env-ref49070 = alloca %struct.ScmObj*, align 8
%_37foldr140130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41640, i64 0)
store %struct.ScmObj* %_37foldr140130, %struct.ScmObj** %stackaddr$env-ref49070
%stackaddr$env-ref49071 = alloca %struct.ScmObj*, align 8
%_37foldl140114 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41640, i64 1)
store %struct.ScmObj* %_37foldl140114, %struct.ScmObj** %stackaddr$env-ref49071
%stackaddr$env-ref49072 = alloca %struct.ScmObj*, align 8
%_37last40152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41640, i64 2)
store %struct.ScmObj* %_37last40152, %struct.ScmObj** %stackaddr$env-ref49072
%stackaddr$env-ref49073 = alloca %struct.ScmObj*, align 8
%_37drop_45right40149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41640, i64 3)
store %struct.ScmObj* %_37drop_45right40149, %struct.ScmObj** %stackaddr$env-ref49073
%stackaddr$env-ref49074 = alloca %struct.ScmObj*, align 8
%Ycmb40109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41640, i64 4)
store %struct.ScmObj* %Ycmb40109, %struct.ScmObj** %stackaddr$env-ref49074
%stackaddr$prim49075 = alloca %struct.ScmObj*, align 8
%_95k40403 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48377)
store volatile %struct.ScmObj* %_95k40403, %struct.ScmObj** %stackaddr$prim49075, align 8
%stackaddr$prim49076 = alloca %struct.ScmObj*, align 8
%current_45args48378 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48377)
store volatile %struct.ScmObj* %current_45args48378, %struct.ScmObj** %stackaddr$prim49076, align 8
%stackaddr$prim49077 = alloca %struct.ScmObj*, align 8
%_37foldr40135 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48378)
store volatile %struct.ScmObj* %_37foldr40135, %struct.ScmObj** %stackaddr$prim49077, align 8
%stackaddr$makeclosure49078 = alloca %struct.ScmObj*, align 8
%fptrToInt49079 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41642 to i64
%ae41642 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt49079)
store volatile %struct.ScmObj* %ae41642, %struct.ScmObj** %stackaddr$makeclosure49078, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41642, %struct.ScmObj* %_37foldr140130, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41642, %struct.ScmObj* %_37foldl140114, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41642, %struct.ScmObj* %_37last40152, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41642, %struct.ScmObj* %_37foldr40135, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41642, %struct.ScmObj* %_37drop_45right40149, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41642, %struct.ScmObj* %Ycmb40109, i64 5)
%ae41643 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49080 = alloca %struct.ScmObj*, align 8
%fptrToInt49081 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41644 to i64
%ae41644 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49081)
store volatile %struct.ScmObj* %ae41644, %struct.ScmObj** %stackaddr$makeclosure49080, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41644, %struct.ScmObj* %_37foldr140130, i64 0)
%args48667$ae41642$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49082 = alloca %struct.ScmObj*, align 8
%args48667$ae41642$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41644, %struct.ScmObj* %args48667$ae41642$0)
store volatile %struct.ScmObj* %args48667$ae41642$1, %struct.ScmObj** %stackaddr$prim49082, align 8
%stackaddr$prim49083 = alloca %struct.ScmObj*, align 8
%args48667$ae41642$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41643, %struct.ScmObj* %args48667$ae41642$1)
store volatile %struct.ScmObj* %args48667$ae41642$2, %struct.ScmObj** %stackaddr$prim49083, align 8
%clofunc49084 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41642)
musttail call tailcc void %clofunc49084(%struct.ScmObj* %ae41642, %struct.ScmObj* %args48667$ae41642$2)
ret void
}

define tailcc void @proc_clo$ae41642(%struct.ScmObj* %env$ae41642,%struct.ScmObj* %current_45args48380) {
%stackaddr$env-ref49085 = alloca %struct.ScmObj*, align 8
%_37foldr140130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41642, i64 0)
store %struct.ScmObj* %_37foldr140130, %struct.ScmObj** %stackaddr$env-ref49085
%stackaddr$env-ref49086 = alloca %struct.ScmObj*, align 8
%_37foldl140114 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41642, i64 1)
store %struct.ScmObj* %_37foldl140114, %struct.ScmObj** %stackaddr$env-ref49086
%stackaddr$env-ref49087 = alloca %struct.ScmObj*, align 8
%_37last40152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41642, i64 2)
store %struct.ScmObj* %_37last40152, %struct.ScmObj** %stackaddr$env-ref49087
%stackaddr$env-ref49088 = alloca %struct.ScmObj*, align 8
%_37foldr40135 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41642, i64 3)
store %struct.ScmObj* %_37foldr40135, %struct.ScmObj** %stackaddr$env-ref49088
%stackaddr$env-ref49089 = alloca %struct.ScmObj*, align 8
%_37drop_45right40149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41642, i64 4)
store %struct.ScmObj* %_37drop_45right40149, %struct.ScmObj** %stackaddr$env-ref49089
%stackaddr$env-ref49090 = alloca %struct.ScmObj*, align 8
%Ycmb40109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41642, i64 5)
store %struct.ScmObj* %Ycmb40109, %struct.ScmObj** %stackaddr$env-ref49090
%stackaddr$prim49091 = alloca %struct.ScmObj*, align 8
%_95k40404 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48380)
store volatile %struct.ScmObj* %_95k40404, %struct.ScmObj** %stackaddr$prim49091, align 8
%stackaddr$prim49092 = alloca %struct.ScmObj*, align 8
%current_45args48381 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48380)
store volatile %struct.ScmObj* %current_45args48381, %struct.ScmObj** %stackaddr$prim49092, align 8
%stackaddr$prim49093 = alloca %struct.ScmObj*, align 8
%_37map140161 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48381)
store volatile %struct.ScmObj* %_37map140161, %struct.ScmObj** %stackaddr$prim49093, align 8
%stackaddr$makeclosure49094 = alloca %struct.ScmObj*, align 8
%fptrToInt49095 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41719 to i64
%ae41719 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt49095)
store volatile %struct.ScmObj* %ae41719, %struct.ScmObj** %stackaddr$makeclosure49094, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41719, %struct.ScmObj* %_37foldr140130, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41719, %struct.ScmObj* %_37foldl140114, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41719, %struct.ScmObj* %_37foldr40135, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41719, %struct.ScmObj* %_37map140161, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41719, %struct.ScmObj* %Ycmb40109, i64 4)
%ae41720 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49096 = alloca %struct.ScmObj*, align 8
%fptrToInt49097 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41721 to i64
%ae41721 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49097)
store volatile %struct.ScmObj* %ae41721, %struct.ScmObj** %stackaddr$makeclosure49096, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41721, %struct.ScmObj* %_37last40152, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41721, %struct.ScmObj* %_37foldr40135, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41721, %struct.ScmObj* %_37drop_45right40149, i64 2)
%args48648$ae41719$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49098 = alloca %struct.ScmObj*, align 8
%args48648$ae41719$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41721, %struct.ScmObj* %args48648$ae41719$0)
store volatile %struct.ScmObj* %args48648$ae41719$1, %struct.ScmObj** %stackaddr$prim49098, align 8
%stackaddr$prim49099 = alloca %struct.ScmObj*, align 8
%args48648$ae41719$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41720, %struct.ScmObj* %args48648$ae41719$1)
store volatile %struct.ScmObj* %args48648$ae41719$2, %struct.ScmObj** %stackaddr$prim49099, align 8
%clofunc49100 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41719)
musttail call tailcc void %clofunc49100(%struct.ScmObj* %ae41719, %struct.ScmObj* %args48648$ae41719$2)
ret void
}

define tailcc void @proc_clo$ae41719(%struct.ScmObj* %env$ae41719,%struct.ScmObj* %current_45args48383) {
%stackaddr$env-ref49101 = alloca %struct.ScmObj*, align 8
%_37foldr140130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41719, i64 0)
store %struct.ScmObj* %_37foldr140130, %struct.ScmObj** %stackaddr$env-ref49101
%stackaddr$env-ref49102 = alloca %struct.ScmObj*, align 8
%_37foldl140114 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41719, i64 1)
store %struct.ScmObj* %_37foldl140114, %struct.ScmObj** %stackaddr$env-ref49102
%stackaddr$env-ref49103 = alloca %struct.ScmObj*, align 8
%_37foldr40135 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41719, i64 2)
store %struct.ScmObj* %_37foldr40135, %struct.ScmObj** %stackaddr$env-ref49103
%stackaddr$env-ref49104 = alloca %struct.ScmObj*, align 8
%_37map140161 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41719, i64 3)
store %struct.ScmObj* %_37map140161, %struct.ScmObj** %stackaddr$env-ref49104
%stackaddr$env-ref49105 = alloca %struct.ScmObj*, align 8
%Ycmb40109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41719, i64 4)
store %struct.ScmObj* %Ycmb40109, %struct.ScmObj** %stackaddr$env-ref49105
%stackaddr$prim49106 = alloca %struct.ScmObj*, align 8
%_95k40405 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48383)
store volatile %struct.ScmObj* %_95k40405, %struct.ScmObj** %stackaddr$prim49106, align 8
%stackaddr$prim49107 = alloca %struct.ScmObj*, align 8
%current_45args48384 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48383)
store volatile %struct.ScmObj* %current_45args48384, %struct.ScmObj** %stackaddr$prim49107, align 8
%stackaddr$prim49108 = alloca %struct.ScmObj*, align 8
%_37map40156 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48384)
store volatile %struct.ScmObj* %_37map40156, %struct.ScmObj** %stackaddr$prim49108, align 8
%stackaddr$makeclosure49109 = alloca %struct.ScmObj*, align 8
%fptrToInt49110 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41865 to i64
%ae41865 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49110)
store volatile %struct.ScmObj* %ae41865, %struct.ScmObj** %stackaddr$makeclosure49109, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41865, %struct.ScmObj* %_37foldl140114, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41865, %struct.ScmObj* %Ycmb40109, i64 1)
%ae41866 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49111 = alloca %struct.ScmObj*, align 8
%fptrToInt49112 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41867 to i64
%ae41867 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49112)
store volatile %struct.ScmObj* %ae41867, %struct.ScmObj** %stackaddr$makeclosure49111, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41867, %struct.ScmObj* %_37foldr40135, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41867, %struct.ScmObj* %_37foldr140130, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41867, %struct.ScmObj* %_37map140161, i64 2)
%args48631$ae41865$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49113 = alloca %struct.ScmObj*, align 8
%args48631$ae41865$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41867, %struct.ScmObj* %args48631$ae41865$0)
store volatile %struct.ScmObj* %args48631$ae41865$1, %struct.ScmObj** %stackaddr$prim49113, align 8
%stackaddr$prim49114 = alloca %struct.ScmObj*, align 8
%args48631$ae41865$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41866, %struct.ScmObj* %args48631$ae41865$1)
store volatile %struct.ScmObj* %args48631$ae41865$2, %struct.ScmObj** %stackaddr$prim49114, align 8
%clofunc49115 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41865)
musttail call tailcc void %clofunc49115(%struct.ScmObj* %ae41865, %struct.ScmObj* %args48631$ae41865$2)
ret void
}

define tailcc void @proc_clo$ae41865(%struct.ScmObj* %env$ae41865,%struct.ScmObj* %current_45args48386) {
%stackaddr$env-ref49116 = alloca %struct.ScmObj*, align 8
%_37foldl140114 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41865, i64 0)
store %struct.ScmObj* %_37foldl140114, %struct.ScmObj** %stackaddr$env-ref49116
%stackaddr$env-ref49117 = alloca %struct.ScmObj*, align 8
%Ycmb40109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41865, i64 1)
store %struct.ScmObj* %Ycmb40109, %struct.ScmObj** %stackaddr$env-ref49117
%stackaddr$prim49118 = alloca %struct.ScmObj*, align 8
%_95k40406 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48386)
store volatile %struct.ScmObj* %_95k40406, %struct.ScmObj** %stackaddr$prim49118, align 8
%stackaddr$prim49119 = alloca %struct.ScmObj*, align 8
%current_45args48387 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48386)
store volatile %struct.ScmObj* %current_45args48387, %struct.ScmObj** %stackaddr$prim49119, align 8
%stackaddr$prim49120 = alloca %struct.ScmObj*, align 8
%anf_45bind40313 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48387)
store volatile %struct.ScmObj* %anf_45bind40313, %struct.ScmObj** %stackaddr$prim49120, align 8
%stackaddr$makeclosure49121 = alloca %struct.ScmObj*, align 8
%fptrToInt49122 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42257 to i64
%ae42257 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49122)
store volatile %struct.ScmObj* %ae42257, %struct.ScmObj** %stackaddr$makeclosure49121, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42257, %struct.ScmObj* %_37foldl140114, i64 0)
%args48571$Ycmb40109$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49123 = alloca %struct.ScmObj*, align 8
%args48571$Ycmb40109$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40313, %struct.ScmObj* %args48571$Ycmb40109$0)
store volatile %struct.ScmObj* %args48571$Ycmb40109$1, %struct.ScmObj** %stackaddr$prim49123, align 8
%stackaddr$prim49124 = alloca %struct.ScmObj*, align 8
%args48571$Ycmb40109$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42257, %struct.ScmObj* %args48571$Ycmb40109$1)
store volatile %struct.ScmObj* %args48571$Ycmb40109$2, %struct.ScmObj** %stackaddr$prim49124, align 8
%clofunc49125 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40109)
musttail call tailcc void %clofunc49125(%struct.ScmObj* %Ycmb40109, %struct.ScmObj* %args48571$Ycmb40109$2)
ret void
}

define tailcc void @proc_clo$ae42257(%struct.ScmObj* %env$ae42257,%struct.ScmObj* %current_45args48389) {
%stackaddr$env-ref49126 = alloca %struct.ScmObj*, align 8
%_37foldl140114 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42257, i64 0)
store %struct.ScmObj* %_37foldl140114, %struct.ScmObj** %stackaddr$env-ref49126
%stackaddr$prim49127 = alloca %struct.ScmObj*, align 8
%_95k40407 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48389)
store volatile %struct.ScmObj* %_95k40407, %struct.ScmObj** %stackaddr$prim49127, align 8
%stackaddr$prim49128 = alloca %struct.ScmObj*, align 8
%current_45args48390 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48389)
store volatile %struct.ScmObj* %current_45args48390, %struct.ScmObj** %stackaddr$prim49128, align 8
%stackaddr$prim49129 = alloca %struct.ScmObj*, align 8
%_37foldl40212 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48390)
store volatile %struct.ScmObj* %_37foldl40212, %struct.ScmObj** %stackaddr$prim49129, align 8
%stackaddr$makeclosure49130 = alloca %struct.ScmObj*, align 8
%fptrToInt49131 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42259 to i64
%ae42259 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49131)
store volatile %struct.ScmObj* %ae42259, %struct.ScmObj** %stackaddr$makeclosure49130, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42259, %struct.ScmObj* %_37foldl140114, i64 0)
%ae42260 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49132 = alloca %struct.ScmObj*, align 8
%fptrToInt49133 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42261 to i64
%ae42261 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49133)
store volatile %struct.ScmObj* %ae42261, %struct.ScmObj** %stackaddr$makeclosure49132, align 8
%args48570$ae42259$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49134 = alloca %struct.ScmObj*, align 8
%args48570$ae42259$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42261, %struct.ScmObj* %args48570$ae42259$0)
store volatile %struct.ScmObj* %args48570$ae42259$1, %struct.ScmObj** %stackaddr$prim49134, align 8
%stackaddr$prim49135 = alloca %struct.ScmObj*, align 8
%args48570$ae42259$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42260, %struct.ScmObj* %args48570$ae42259$1)
store volatile %struct.ScmObj* %args48570$ae42259$2, %struct.ScmObj** %stackaddr$prim49135, align 8
%clofunc49136 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42259)
musttail call tailcc void %clofunc49136(%struct.ScmObj* %ae42259, %struct.ScmObj* %args48570$ae42259$2)
ret void
}

define tailcc void @proc_clo$ae42259(%struct.ScmObj* %env$ae42259,%struct.ScmObj* %current_45args48392) {
%stackaddr$env-ref49137 = alloca %struct.ScmObj*, align 8
%_37foldl140114 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42259, i64 0)
store %struct.ScmObj* %_37foldl140114, %struct.ScmObj** %stackaddr$env-ref49137
%stackaddr$prim49138 = alloca %struct.ScmObj*, align 8
%_95k40408 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48392)
store volatile %struct.ScmObj* %_95k40408, %struct.ScmObj** %stackaddr$prim49138, align 8
%stackaddr$prim49139 = alloca %struct.ScmObj*, align 8
%current_45args48393 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48392)
store volatile %struct.ScmObj* %current_45args48393, %struct.ScmObj** %stackaddr$prim49139, align 8
%stackaddr$prim49140 = alloca %struct.ScmObj*, align 8
%_37_6240209 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48393)
store volatile %struct.ScmObj* %_37_6240209, %struct.ScmObj** %stackaddr$prim49140, align 8
%stackaddr$makeclosure49141 = alloca %struct.ScmObj*, align 8
%fptrToInt49142 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42283 to i64
%ae42283 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49142)
store volatile %struct.ScmObj* %ae42283, %struct.ScmObj** %stackaddr$makeclosure49141, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42283, %struct.ScmObj* %_37foldl140114, i64 0)
%ae42284 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49143 = alloca %struct.ScmObj*, align 8
%fptrToInt49144 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42285 to i64
%ae42285 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49144)
store volatile %struct.ScmObj* %ae42285, %struct.ScmObj** %stackaddr$makeclosure49143, align 8
%args48564$ae42283$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49145 = alloca %struct.ScmObj*, align 8
%args48564$ae42283$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42285, %struct.ScmObj* %args48564$ae42283$0)
store volatile %struct.ScmObj* %args48564$ae42283$1, %struct.ScmObj** %stackaddr$prim49145, align 8
%stackaddr$prim49146 = alloca %struct.ScmObj*, align 8
%args48564$ae42283$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42284, %struct.ScmObj* %args48564$ae42283$1)
store volatile %struct.ScmObj* %args48564$ae42283$2, %struct.ScmObj** %stackaddr$prim49146, align 8
%clofunc49147 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42283)
musttail call tailcc void %clofunc49147(%struct.ScmObj* %ae42283, %struct.ScmObj* %args48564$ae42283$2)
ret void
}

define tailcc void @proc_clo$ae42283(%struct.ScmObj* %env$ae42283,%struct.ScmObj* %current_45args48395) {
%stackaddr$env-ref49148 = alloca %struct.ScmObj*, align 8
%_37foldl140114 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42283, i64 0)
store %struct.ScmObj* %_37foldl140114, %struct.ScmObj** %stackaddr$env-ref49148
%stackaddr$prim49149 = alloca %struct.ScmObj*, align 8
%_95k40409 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48395)
store volatile %struct.ScmObj* %_95k40409, %struct.ScmObj** %stackaddr$prim49149, align 8
%stackaddr$prim49150 = alloca %struct.ScmObj*, align 8
%current_45args48396 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48395)
store volatile %struct.ScmObj* %current_45args48396, %struct.ScmObj** %stackaddr$prim49150, align 8
%stackaddr$prim49151 = alloca %struct.ScmObj*, align 8
%_37_62_6140206 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48396)
store volatile %struct.ScmObj* %_37_62_6140206, %struct.ScmObj** %stackaddr$prim49151, align 8
%ae42307 = call %struct.ScmObj* @const_init_int(i64 1)
%ae42308 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49152 = alloca %struct.ScmObj*, align 8
%_37append40202 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42307, %struct.ScmObj* %ae42308)
store volatile %struct.ScmObj* %_37append40202, %struct.ScmObj** %stackaddr$prim49152, align 8
%stackaddr$makeclosure49153 = alloca %struct.ScmObj*, align 8
%fptrToInt49154 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42309 to i64
%ae42309 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49154)
store volatile %struct.ScmObj* %ae42309, %struct.ScmObj** %stackaddr$makeclosure49153, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42309, %struct.ScmObj* %_37foldl140114, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42309, %struct.ScmObj* %_37append40202, i64 1)
%ae42310 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49155 = alloca %struct.ScmObj*, align 8
%fptrToInt49156 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42311 to i64
%ae42311 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49156)
store volatile %struct.ScmObj* %ae42311, %struct.ScmObj** %stackaddr$makeclosure49155, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42311, %struct.ScmObj* %_37append40202, i64 0)
%args48558$ae42309$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49157 = alloca %struct.ScmObj*, align 8
%args48558$ae42309$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42311, %struct.ScmObj* %args48558$ae42309$0)
store volatile %struct.ScmObj* %args48558$ae42309$1, %struct.ScmObj** %stackaddr$prim49157, align 8
%stackaddr$prim49158 = alloca %struct.ScmObj*, align 8
%args48558$ae42309$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42310, %struct.ScmObj* %args48558$ae42309$1)
store volatile %struct.ScmObj* %args48558$ae42309$2, %struct.ScmObj** %stackaddr$prim49158, align 8
%clofunc49159 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42309)
musttail call tailcc void %clofunc49159(%struct.ScmObj* %ae42309, %struct.ScmObj* %args48558$ae42309$2)
ret void
}

define tailcc void @proc_clo$ae42309(%struct.ScmObj* %env$ae42309,%struct.ScmObj* %current_45args48398) {
%stackaddr$env-ref49160 = alloca %struct.ScmObj*, align 8
%_37foldl140114 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42309, i64 0)
store %struct.ScmObj* %_37foldl140114, %struct.ScmObj** %stackaddr$env-ref49160
%stackaddr$env-ref49161 = alloca %struct.ScmObj*, align 8
%_37append40202 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42309, i64 1)
store %struct.ScmObj* %_37append40202, %struct.ScmObj** %stackaddr$env-ref49161
%stackaddr$prim49162 = alloca %struct.ScmObj*, align 8
%_95k40410 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48398)
store volatile %struct.ScmObj* %_95k40410, %struct.ScmObj** %stackaddr$prim49162, align 8
%stackaddr$prim49163 = alloca %struct.ScmObj*, align 8
%current_45args48399 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48398)
store volatile %struct.ScmObj* %current_45args48399, %struct.ScmObj** %stackaddr$prim49163, align 8
%stackaddr$prim49164 = alloca %struct.ScmObj*, align 8
%anf_45bind40321 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48399)
store volatile %struct.ScmObj* %anf_45bind40321, %struct.ScmObj** %stackaddr$prim49164, align 8
%ae42377 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49165 = alloca %struct.ScmObj*, align 8
%_95040203 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append40202, %struct.ScmObj* %ae42377, %struct.ScmObj* %anf_45bind40321)
store volatile %struct.ScmObj* %_95040203, %struct.ScmObj** %stackaddr$prim49165, align 8
%ae42380 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49166 = alloca %struct.ScmObj*, align 8
%_37append40201 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append40202, %struct.ScmObj* %ae42380)
store volatile %struct.ScmObj* %_37append40201, %struct.ScmObj** %stackaddr$prim49166, align 8
%stackaddr$makeclosure49167 = alloca %struct.ScmObj*, align 8
%fptrToInt49168 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42381 to i64
%ae42381 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49168)
store volatile %struct.ScmObj* %ae42381, %struct.ScmObj** %stackaddr$makeclosure49167, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42381, %struct.ScmObj* %_37foldl140114, i64 0)
%ae42382 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49169 = alloca %struct.ScmObj*, align 8
%fptrToInt49170 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42383 to i64
%ae42383 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49170)
store volatile %struct.ScmObj* %ae42383, %struct.ScmObj** %stackaddr$makeclosure49169, align 8
%args48547$ae42381$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49171 = alloca %struct.ScmObj*, align 8
%args48547$ae42381$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42383, %struct.ScmObj* %args48547$ae42381$0)
store volatile %struct.ScmObj* %args48547$ae42381$1, %struct.ScmObj** %stackaddr$prim49171, align 8
%stackaddr$prim49172 = alloca %struct.ScmObj*, align 8
%args48547$ae42381$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42382, %struct.ScmObj* %args48547$ae42381$1)
store volatile %struct.ScmObj* %args48547$ae42381$2, %struct.ScmObj** %stackaddr$prim49172, align 8
%clofunc49173 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42381)
musttail call tailcc void %clofunc49173(%struct.ScmObj* %ae42381, %struct.ScmObj* %args48547$ae42381$2)
ret void
}

define tailcc void @proc_clo$ae42381(%struct.ScmObj* %env$ae42381,%struct.ScmObj* %current_45args48401) {
%stackaddr$env-ref49174 = alloca %struct.ScmObj*, align 8
%_37foldl140114 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42381, i64 0)
store %struct.ScmObj* %_37foldl140114, %struct.ScmObj** %stackaddr$env-ref49174
%stackaddr$prim49175 = alloca %struct.ScmObj*, align 8
%_95k40411 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48401)
store volatile %struct.ScmObj* %_95k40411, %struct.ScmObj** %stackaddr$prim49175, align 8
%stackaddr$prim49176 = alloca %struct.ScmObj*, align 8
%current_45args48402 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48401)
store volatile %struct.ScmObj* %current_45args48402, %struct.ScmObj** %stackaddr$prim49176, align 8
%stackaddr$prim49177 = alloca %struct.ScmObj*, align 8
%_37list_6340194 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48402)
store volatile %struct.ScmObj* %_37list_6340194, %struct.ScmObj** %stackaddr$prim49177, align 8
%stackaddr$makeclosure49178 = alloca %struct.ScmObj*, align 8
%fptrToInt49179 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42797 to i64
%ae42797 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49179)
store volatile %struct.ScmObj* %ae42797, %struct.ScmObj** %stackaddr$makeclosure49178, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42797, %struct.ScmObj* %_37foldl140114, i64 0)
%ae42798 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49180 = alloca %struct.ScmObj*, align 8
%fptrToInt49181 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42799 to i64
%ae42799 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49181)
store volatile %struct.ScmObj* %ae42799, %struct.ScmObj** %stackaddr$makeclosure49180, align 8
%args48522$ae42797$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49182 = alloca %struct.ScmObj*, align 8
%args48522$ae42797$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42799, %struct.ScmObj* %args48522$ae42797$0)
store volatile %struct.ScmObj* %args48522$ae42797$1, %struct.ScmObj** %stackaddr$prim49182, align 8
%stackaddr$prim49183 = alloca %struct.ScmObj*, align 8
%args48522$ae42797$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42798, %struct.ScmObj* %args48522$ae42797$1)
store volatile %struct.ScmObj* %args48522$ae42797$2, %struct.ScmObj** %stackaddr$prim49183, align 8
%clofunc49184 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42797)
musttail call tailcc void %clofunc49184(%struct.ScmObj* %ae42797, %struct.ScmObj* %args48522$ae42797$2)
ret void
}

define tailcc void @proc_clo$ae42797(%struct.ScmObj* %env$ae42797,%struct.ScmObj* %current_45args48404) {
%stackaddr$env-ref49185 = alloca %struct.ScmObj*, align 8
%_37foldl140114 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42797, i64 0)
store %struct.ScmObj* %_37foldl140114, %struct.ScmObj** %stackaddr$env-ref49185
%stackaddr$prim49186 = alloca %struct.ScmObj*, align 8
%_95k40412 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48404)
store volatile %struct.ScmObj* %_95k40412, %struct.ScmObj** %stackaddr$prim49186, align 8
%stackaddr$prim49187 = alloca %struct.ScmObj*, align 8
%current_45args48405 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48404)
store volatile %struct.ScmObj* %current_45args48405, %struct.ScmObj** %stackaddr$prim49187, align 8
%stackaddr$prim49188 = alloca %struct.ScmObj*, align 8
%_37drop40185 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48405)
store volatile %struct.ScmObj* %_37drop40185, %struct.ScmObj** %stackaddr$prim49188, align 8
%stackaddr$makeclosure49189 = alloca %struct.ScmObj*, align 8
%fptrToInt49190 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43333 to i64
%ae43333 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49190)
store volatile %struct.ScmObj* %ae43333, %struct.ScmObj** %stackaddr$makeclosure49189, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43333, %struct.ScmObj* %_37foldl140114, i64 0)
%ae43334 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49191 = alloca %struct.ScmObj*, align 8
%fptrToInt49192 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43335 to i64
%ae43335 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49192)
store volatile %struct.ScmObj* %ae43335, %struct.ScmObj** %stackaddr$makeclosure49191, align 8
%args48498$ae43333$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49193 = alloca %struct.ScmObj*, align 8
%args48498$ae43333$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43335, %struct.ScmObj* %args48498$ae43333$0)
store volatile %struct.ScmObj* %args48498$ae43333$1, %struct.ScmObj** %stackaddr$prim49193, align 8
%stackaddr$prim49194 = alloca %struct.ScmObj*, align 8
%args48498$ae43333$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43334, %struct.ScmObj* %args48498$ae43333$1)
store volatile %struct.ScmObj* %args48498$ae43333$2, %struct.ScmObj** %stackaddr$prim49194, align 8
%clofunc49195 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43333)
musttail call tailcc void %clofunc49195(%struct.ScmObj* %ae43333, %struct.ScmObj* %args48498$ae43333$2)
ret void
}

define tailcc void @proc_clo$ae43333(%struct.ScmObj* %env$ae43333,%struct.ScmObj* %current_45args48407) {
%stackaddr$env-ref49196 = alloca %struct.ScmObj*, align 8
%_37foldl140114 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43333, i64 0)
store %struct.ScmObj* %_37foldl140114, %struct.ScmObj** %stackaddr$env-ref49196
%stackaddr$prim49197 = alloca %struct.ScmObj*, align 8
%_95k40413 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48407)
store volatile %struct.ScmObj* %_95k40413, %struct.ScmObj** %stackaddr$prim49197, align 8
%stackaddr$prim49198 = alloca %struct.ScmObj*, align 8
%current_45args48408 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48407)
store volatile %struct.ScmObj* %current_45args48408, %struct.ScmObj** %stackaddr$prim49198, align 8
%stackaddr$prim49199 = alloca %struct.ScmObj*, align 8
%_37memv40178 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48408)
store volatile %struct.ScmObj* %_37memv40178, %struct.ScmObj** %stackaddr$prim49199, align 8
%stackaddr$makeclosure49200 = alloca %struct.ScmObj*, align 8
%fptrToInt49201 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43737 to i64
%ae43737 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49201)
store volatile %struct.ScmObj* %ae43737, %struct.ScmObj** %stackaddr$makeclosure49200, align 8
%ae43738 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49202 = alloca %struct.ScmObj*, align 8
%fptrToInt49203 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43739 to i64
%ae43739 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49203)
store volatile %struct.ScmObj* %ae43739, %struct.ScmObj** %stackaddr$makeclosure49202, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43739, %struct.ScmObj* %_37foldl140114, i64 0)
%args48472$ae43737$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49204 = alloca %struct.ScmObj*, align 8
%args48472$ae43737$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43739, %struct.ScmObj* %args48472$ae43737$0)
store volatile %struct.ScmObj* %args48472$ae43737$1, %struct.ScmObj** %stackaddr$prim49204, align 8
%stackaddr$prim49205 = alloca %struct.ScmObj*, align 8
%args48472$ae43737$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43738, %struct.ScmObj* %args48472$ae43737$1)
store volatile %struct.ScmObj* %args48472$ae43737$2, %struct.ScmObj** %stackaddr$prim49205, align 8
%clofunc49206 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43737)
musttail call tailcc void %clofunc49206(%struct.ScmObj* %ae43737, %struct.ScmObj* %args48472$ae43737$2)
ret void
}

define tailcc void @proc_clo$ae43737(%struct.ScmObj* %env$ae43737,%struct.ScmObj* %current_45args48410) {
%stackaddr$prim49207 = alloca %struct.ScmObj*, align 8
%_95k40414 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48410)
store volatile %struct.ScmObj* %_95k40414, %struct.ScmObj** %stackaddr$prim49207, align 8
%stackaddr$prim49208 = alloca %struct.ScmObj*, align 8
%current_45args48411 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48410)
store volatile %struct.ScmObj* %current_45args48411, %struct.ScmObj** %stackaddr$prim49208, align 8
%stackaddr$prim49209 = alloca %struct.ScmObj*, align 8
%_37_4740174 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48411)
store volatile %struct.ScmObj* %_37_4740174, %struct.ScmObj** %stackaddr$prim49209, align 8
%stackaddr$makeclosure49210 = alloca %struct.ScmObj*, align 8
%fptrToInt49211 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43835 to i64
%ae43835 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49211)
store volatile %struct.ScmObj* %ae43835, %struct.ScmObj** %stackaddr$makeclosure49210, align 8
%ae43836 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49212 = alloca %struct.ScmObj*, align 8
%fptrToInt49213 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43837 to i64
%ae43837 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49213)
store volatile %struct.ScmObj* %ae43837, %struct.ScmObj** %stackaddr$makeclosure49212, align 8
%args48459$ae43835$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49214 = alloca %struct.ScmObj*, align 8
%args48459$ae43835$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43837, %struct.ScmObj* %args48459$ae43835$0)
store volatile %struct.ScmObj* %args48459$ae43835$1, %struct.ScmObj** %stackaddr$prim49214, align 8
%stackaddr$prim49215 = alloca %struct.ScmObj*, align 8
%args48459$ae43835$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43836, %struct.ScmObj* %args48459$ae43835$1)
store volatile %struct.ScmObj* %args48459$ae43835$2, %struct.ScmObj** %stackaddr$prim49215, align 8
%clofunc49216 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43835)
musttail call tailcc void %clofunc49216(%struct.ScmObj* %ae43835, %struct.ScmObj* %args48459$ae43835$2)
ret void
}

define tailcc void @proc_clo$ae43835(%struct.ScmObj* %env$ae43835,%struct.ScmObj* %current_45args48413) {
%stackaddr$prim49217 = alloca %struct.ScmObj*, align 8
%_95k40415 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48413)
store volatile %struct.ScmObj* %_95k40415, %struct.ScmObj** %stackaddr$prim49217, align 8
%stackaddr$prim49218 = alloca %struct.ScmObj*, align 8
%current_45args48414 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48413)
store volatile %struct.ScmObj* %current_45args48414, %struct.ScmObj** %stackaddr$prim49218, align 8
%stackaddr$prim49219 = alloca %struct.ScmObj*, align 8
%_37first40172 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48414)
store volatile %struct.ScmObj* %_37first40172, %struct.ScmObj** %stackaddr$prim49219, align 8
%stackaddr$makeclosure49220 = alloca %struct.ScmObj*, align 8
%fptrToInt49221 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43855 to i64
%ae43855 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49221)
store volatile %struct.ScmObj* %ae43855, %struct.ScmObj** %stackaddr$makeclosure49220, align 8
%ae43856 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49222 = alloca %struct.ScmObj*, align 8
%fptrToInt49223 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43857 to i64
%ae43857 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49223)
store volatile %struct.ScmObj* %ae43857, %struct.ScmObj** %stackaddr$makeclosure49222, align 8
%args48454$ae43855$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49224 = alloca %struct.ScmObj*, align 8
%args48454$ae43855$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43857, %struct.ScmObj* %args48454$ae43855$0)
store volatile %struct.ScmObj* %args48454$ae43855$1, %struct.ScmObj** %stackaddr$prim49224, align 8
%stackaddr$prim49225 = alloca %struct.ScmObj*, align 8
%args48454$ae43855$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43856, %struct.ScmObj* %args48454$ae43855$1)
store volatile %struct.ScmObj* %args48454$ae43855$2, %struct.ScmObj** %stackaddr$prim49225, align 8
%clofunc49226 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43855)
musttail call tailcc void %clofunc49226(%struct.ScmObj* %ae43855, %struct.ScmObj* %args48454$ae43855$2)
ret void
}

define tailcc void @proc_clo$ae43855(%struct.ScmObj* %env$ae43855,%struct.ScmObj* %current_45args48416) {
%stackaddr$prim49227 = alloca %struct.ScmObj*, align 8
%_95k40416 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48416)
store volatile %struct.ScmObj* %_95k40416, %struct.ScmObj** %stackaddr$prim49227, align 8
%stackaddr$prim49228 = alloca %struct.ScmObj*, align 8
%current_45args48417 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48416)
store volatile %struct.ScmObj* %current_45args48417, %struct.ScmObj** %stackaddr$prim49228, align 8
%stackaddr$prim49229 = alloca %struct.ScmObj*, align 8
%_37second40170 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48417)
store volatile %struct.ScmObj* %_37second40170, %struct.ScmObj** %stackaddr$prim49229, align 8
%stackaddr$makeclosure49230 = alloca %struct.ScmObj*, align 8
%fptrToInt49231 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43877 to i64
%ae43877 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49231)
store volatile %struct.ScmObj* %ae43877, %struct.ScmObj** %stackaddr$makeclosure49230, align 8
%ae43878 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49232 = alloca %struct.ScmObj*, align 8
%fptrToInt49233 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43879 to i64
%ae43879 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49233)
store volatile %struct.ScmObj* %ae43879, %struct.ScmObj** %stackaddr$makeclosure49232, align 8
%args48449$ae43877$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49234 = alloca %struct.ScmObj*, align 8
%args48449$ae43877$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43879, %struct.ScmObj* %args48449$ae43877$0)
store volatile %struct.ScmObj* %args48449$ae43877$1, %struct.ScmObj** %stackaddr$prim49234, align 8
%stackaddr$prim49235 = alloca %struct.ScmObj*, align 8
%args48449$ae43877$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43878, %struct.ScmObj* %args48449$ae43877$1)
store volatile %struct.ScmObj* %args48449$ae43877$2, %struct.ScmObj** %stackaddr$prim49235, align 8
%clofunc49236 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43877)
musttail call tailcc void %clofunc49236(%struct.ScmObj* %ae43877, %struct.ScmObj* %args48449$ae43877$2)
ret void
}

define tailcc void @proc_clo$ae43877(%struct.ScmObj* %env$ae43877,%struct.ScmObj* %current_45args48419) {
%stackaddr$prim49237 = alloca %struct.ScmObj*, align 8
%_95k40417 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48419)
store volatile %struct.ScmObj* %_95k40417, %struct.ScmObj** %stackaddr$prim49237, align 8
%stackaddr$prim49238 = alloca %struct.ScmObj*, align 8
%current_45args48420 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48419)
store volatile %struct.ScmObj* %current_45args48420, %struct.ScmObj** %stackaddr$prim49238, align 8
%stackaddr$prim49239 = alloca %struct.ScmObj*, align 8
%_37third40168 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48420)
store volatile %struct.ScmObj* %_37third40168, %struct.ScmObj** %stackaddr$prim49239, align 8
%stackaddr$makeclosure49240 = alloca %struct.ScmObj*, align 8
%fptrToInt49241 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43901 to i64
%ae43901 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49241)
store volatile %struct.ScmObj* %ae43901, %struct.ScmObj** %stackaddr$makeclosure49240, align 8
%ae43902 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49242 = alloca %struct.ScmObj*, align 8
%fptrToInt49243 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43903 to i64
%ae43903 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49243)
store volatile %struct.ScmObj* %ae43903, %struct.ScmObj** %stackaddr$makeclosure49242, align 8
%args48444$ae43901$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49244 = alloca %struct.ScmObj*, align 8
%args48444$ae43901$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43903, %struct.ScmObj* %args48444$ae43901$0)
store volatile %struct.ScmObj* %args48444$ae43901$1, %struct.ScmObj** %stackaddr$prim49244, align 8
%stackaddr$prim49245 = alloca %struct.ScmObj*, align 8
%args48444$ae43901$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43902, %struct.ScmObj* %args48444$ae43901$1)
store volatile %struct.ScmObj* %args48444$ae43901$2, %struct.ScmObj** %stackaddr$prim49245, align 8
%clofunc49246 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43901)
musttail call tailcc void %clofunc49246(%struct.ScmObj* %ae43901, %struct.ScmObj* %args48444$ae43901$2)
ret void
}

define tailcc void @proc_clo$ae43901(%struct.ScmObj* %env$ae43901,%struct.ScmObj* %current_45args48422) {
%stackaddr$prim49247 = alloca %struct.ScmObj*, align 8
%_95k40418 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48422)
store volatile %struct.ScmObj* %_95k40418, %struct.ScmObj** %stackaddr$prim49247, align 8
%stackaddr$prim49248 = alloca %struct.ScmObj*, align 8
%current_45args48423 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48422)
store volatile %struct.ScmObj* %current_45args48423, %struct.ScmObj** %stackaddr$prim49248, align 8
%stackaddr$prim49249 = alloca %struct.ScmObj*, align 8
%_37fourth40166 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48423)
store volatile %struct.ScmObj* %_37fourth40166, %struct.ScmObj** %stackaddr$prim49249, align 8
%stackaddr$makeclosure49250 = alloca %struct.ScmObj*, align 8
%fptrToInt49251 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43927 to i64
%ae43927 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49251)
store volatile %struct.ScmObj* %ae43927, %struct.ScmObj** %stackaddr$makeclosure49250, align 8
%ae43928 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49252 = alloca %struct.ScmObj*, align 8
%fptrToInt49253 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43929 to i64
%ae43929 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49253)
store volatile %struct.ScmObj* %ae43929, %struct.ScmObj** %stackaddr$makeclosure49252, align 8
%args48439$ae43927$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49254 = alloca %struct.ScmObj*, align 8
%args48439$ae43927$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43929, %struct.ScmObj* %args48439$ae43927$0)
store volatile %struct.ScmObj* %args48439$ae43927$1, %struct.ScmObj** %stackaddr$prim49254, align 8
%stackaddr$prim49255 = alloca %struct.ScmObj*, align 8
%args48439$ae43927$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43928, %struct.ScmObj* %args48439$ae43927$1)
store volatile %struct.ScmObj* %args48439$ae43927$2, %struct.ScmObj** %stackaddr$prim49255, align 8
%clofunc49256 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43927)
musttail call tailcc void %clofunc49256(%struct.ScmObj* %ae43927, %struct.ScmObj* %args48439$ae43927$2)
ret void
}

define tailcc void @proc_clo$ae43927(%struct.ScmObj* %env$ae43927,%struct.ScmObj* %current_45args48425) {
%stackaddr$prim49257 = alloca %struct.ScmObj*, align 8
%_95k40419 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48425)
store volatile %struct.ScmObj* %_95k40419, %struct.ScmObj** %stackaddr$prim49257, align 8
%stackaddr$prim49258 = alloca %struct.ScmObj*, align 8
%current_45args48426 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48425)
store volatile %struct.ScmObj* %current_45args48426, %struct.ScmObj** %stackaddr$prim49258, align 8
%stackaddr$prim49259 = alloca %struct.ScmObj*, align 8
%promise_6340227 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48426)
store volatile %struct.ScmObj* %promise_6340227, %struct.ScmObj** %stackaddr$prim49259, align 8
%stackaddr$prim49260 = alloca %struct.ScmObj*, align 8
%anf_45bind40361 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind40361, %struct.ScmObj** %stackaddr$prim49260, align 8
%ae44014 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim49261 = alloca %struct.ScmObj*, align 8
%x40229 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae44014, %struct.ScmObj* %anf_45bind40361)
store volatile %struct.ScmObj* %x40229, %struct.ScmObj** %stackaddr$prim49261, align 8
%ae44017 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44018 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim49262 = alloca %struct.ScmObj*, align 8
%t4010240230 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %x40229, %struct.ScmObj* %ae44017, %struct.ScmObj* %ae44018)
store volatile %struct.ScmObj* %t4010240230, %struct.ScmObj** %stackaddr$prim49262, align 8
%stackaddr$prim49263 = alloca %struct.ScmObj*, align 8
%anf_45bind40362 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind40362, %struct.ScmObj** %stackaddr$prim49263, align 8
%ae44019 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim49264 = alloca %struct.ScmObj*, align 8
%a40232 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae44019, %struct.ScmObj* %anf_45bind40362)
store volatile %struct.ScmObj* %a40232, %struct.ScmObj** %stackaddr$prim49264, align 8
%stackaddr$prim49265 = alloca %struct.ScmObj*, align 8
%anf_45bind40363 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind40363, %struct.ScmObj** %stackaddr$prim49265, align 8
%ae44021 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim49266 = alloca %struct.ScmObj*, align 8
%b40231 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae44021, %struct.ScmObj* %anf_45bind40363)
store volatile %struct.ScmObj* %b40231, %struct.ScmObj** %stackaddr$prim49266, align 8
%ae44024 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49267 = alloca %struct.ScmObj*, align 8
%anf_45bind40364 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %x40229, %struct.ScmObj* %ae44024)
store volatile %struct.ScmObj* %anf_45bind40364, %struct.ScmObj** %stackaddr$prim49267, align 8
%ae44026 = call %struct.ScmObj* @const_init_int(i64 3)
%stackaddr$prim49268 = alloca %struct.ScmObj*, align 8
%anf_45bind40365 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %anf_45bind40364, %struct.ScmObj* %ae44026)
store volatile %struct.ScmObj* %anf_45bind40365, %struct.ScmObj** %stackaddr$prim49268, align 8
%ae44028 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49269 = alloca %struct.ScmObj*, align 8
%anf_45bind40366 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %x40229, %struct.ScmObj* %ae44028, %struct.ScmObj* %anf_45bind40365)
store volatile %struct.ScmObj* %anf_45bind40366, %struct.ScmObj** %stackaddr$prim49269, align 8
%ae44031 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49270 = alloca %struct.ScmObj*, align 8
%t4010440234 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a40232, %struct.ScmObj* %ae44031, %struct.ScmObj* %anf_45bind40366)
store volatile %struct.ScmObj* %t4010440234, %struct.ScmObj** %stackaddr$prim49270, align 8
%ae44034 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49271 = alloca %struct.ScmObj*, align 8
%anf_45bind40367 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %x40229, %struct.ScmObj* %ae44034)
store volatile %struct.ScmObj* %anf_45bind40367, %struct.ScmObj** %stackaddr$prim49271, align 8
%ae44036 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim49272 = alloca %struct.ScmObj*, align 8
%anf_45bind40368 = call %struct.ScmObj* @prim__42(%struct.ScmObj* %anf_45bind40367, %struct.ScmObj* %ae44036)
store volatile %struct.ScmObj* %anf_45bind40368, %struct.ScmObj** %stackaddr$prim49272, align 8
%ae44038 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49273 = alloca %struct.ScmObj*, align 8
%anf_45bind40369 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %x40229, %struct.ScmObj* %ae44038, %struct.ScmObj* %anf_45bind40368)
store volatile %struct.ScmObj* %anf_45bind40369, %struct.ScmObj** %stackaddr$prim49273, align 8
%ae44041 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49274 = alloca %struct.ScmObj*, align 8
%t4010340233 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %b40231, %struct.ScmObj* %ae44041, %struct.ScmObj* %anf_45bind40369)
store volatile %struct.ScmObj* %t4010340233, %struct.ScmObj** %stackaddr$prim49274, align 8
%stackaddr$prim49275 = alloca %struct.ScmObj*, align 8
%anf_45bind40370 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind40370, %struct.ScmObj** %stackaddr$prim49275, align 8
%ae44043 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim49276 = alloca %struct.ScmObj*, align 8
%a40236 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae44043, %struct.ScmObj* %anf_45bind40370)
store volatile %struct.ScmObj* %a40236, %struct.ScmObj** %stackaddr$prim49276, align 8
%stackaddr$prim49277 = alloca %struct.ScmObj*, align 8
%anf_45bind40371 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind40371, %struct.ScmObj** %stackaddr$prim49277, align 8
%ae44045 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim49278 = alloca %struct.ScmObj*, align 8
%b40235 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae44045, %struct.ScmObj* %anf_45bind40371)
store volatile %struct.ScmObj* %b40235, %struct.ScmObj** %stackaddr$prim49278, align 8
%ae44048 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49279 = alloca %struct.ScmObj*, align 8
%anf_45bind40372 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %x40229, %struct.ScmObj* %ae44048)
store volatile %struct.ScmObj* %anf_45bind40372, %struct.ScmObj** %stackaddr$prim49279, align 8
%ae44050 = call %struct.ScmObj* @const_init_int(i64 3)
%stackaddr$prim49280 = alloca %struct.ScmObj*, align 8
%anf_45bind40373 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %anf_45bind40372, %struct.ScmObj* %ae44050)
store volatile %struct.ScmObj* %anf_45bind40373, %struct.ScmObj** %stackaddr$prim49280, align 8
%ae44052 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49281 = alloca %struct.ScmObj*, align 8
%anf_45bind40374 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %x40229, %struct.ScmObj* %ae44052, %struct.ScmObj* %anf_45bind40373)
store volatile %struct.ScmObj* %anf_45bind40374, %struct.ScmObj** %stackaddr$prim49281, align 8
%ae44055 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49282 = alloca %struct.ScmObj*, align 8
%t4010640238 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a40236, %struct.ScmObj* %ae44055, %struct.ScmObj* %anf_45bind40374)
store volatile %struct.ScmObj* %t4010640238, %struct.ScmObj** %stackaddr$prim49282, align 8
%ae44058 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49283 = alloca %struct.ScmObj*, align 8
%anf_45bind40375 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %x40229, %struct.ScmObj* %ae44058)
store volatile %struct.ScmObj* %anf_45bind40375, %struct.ScmObj** %stackaddr$prim49283, align 8
%ae44060 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim49284 = alloca %struct.ScmObj*, align 8
%anf_45bind40376 = call %struct.ScmObj* @prim__42(%struct.ScmObj* %anf_45bind40375, %struct.ScmObj* %ae44060)
store volatile %struct.ScmObj* %anf_45bind40376, %struct.ScmObj** %stackaddr$prim49284, align 8
%ae44062 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49285 = alloca %struct.ScmObj*, align 8
%anf_45bind40377 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %x40229, %struct.ScmObj* %ae44062, %struct.ScmObj* %anf_45bind40376)
store volatile %struct.ScmObj* %anf_45bind40377, %struct.ScmObj** %stackaddr$prim49285, align 8
%ae44065 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49286 = alloca %struct.ScmObj*, align 8
%t4010540237 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %b40235, %struct.ScmObj* %ae44065, %struct.ScmObj* %anf_45bind40377)
store volatile %struct.ScmObj* %t4010540237, %struct.ScmObj** %stackaddr$prim49286, align 8
%ae44068 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49287 = alloca %struct.ScmObj*, align 8
%anf_45bind40378 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %x40229, %struct.ScmObj* %ae44068)
store volatile %struct.ScmObj* %anf_45bind40378, %struct.ScmObj** %stackaddr$prim49287, align 8
%ae44070 = call %struct.ScmObj* @const_init_int(i64 3)
%stackaddr$prim49288 = alloca %struct.ScmObj*, align 8
%anf_45bind40379 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %anf_45bind40378, %struct.ScmObj* %ae44070)
store volatile %struct.ScmObj* %anf_45bind40379, %struct.ScmObj** %stackaddr$prim49288, align 8
%ae44072 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49289 = alloca %struct.ScmObj*, align 8
%a40239 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %x40229, %struct.ScmObj* %ae44072, %struct.ScmObj* %anf_45bind40379)
store volatile %struct.ScmObj* %a40239, %struct.ScmObj** %stackaddr$prim49289, align 8
%ae44075 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49290 = alloca %struct.ScmObj*, align 8
%anf_45bind40380 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %x40229, %struct.ScmObj* %ae44075)
store volatile %struct.ScmObj* %anf_45bind40380, %struct.ScmObj** %stackaddr$prim49290, align 8
%ae44077 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim49291 = alloca %struct.ScmObj*, align 8
%anf_45bind40381 = call %struct.ScmObj* @prim__42(%struct.ScmObj* %anf_45bind40380, %struct.ScmObj* %ae44077)
store volatile %struct.ScmObj* %anf_45bind40381, %struct.ScmObj** %stackaddr$prim49291, align 8
%ae44079 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49292 = alloca %struct.ScmObj*, align 8
%b40240 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %x40229, %struct.ScmObj* %ae44079, %struct.ScmObj* %anf_45bind40381)
store volatile %struct.ScmObj* %b40240, %struct.ScmObj** %stackaddr$prim49292, align 8
%e40241 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim49293 = alloca %struct.ScmObj*, align 8
%anf_45bind40382 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind40382, %struct.ScmObj** %stackaddr$prim49293, align 8
%ae44081 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim49294 = alloca %struct.ScmObj*, align 8
%e40243 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae44081, %struct.ScmObj* %anf_45bind40382)
store volatile %struct.ScmObj* %e40243, %struct.ScmObj** %stackaddr$prim49294, align 8
%stackaddr$prim49295 = alloca %struct.ScmObj*, align 8
%anf_45bind40383 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind40383, %struct.ScmObj** %stackaddr$prim49295, align 8
%ae44083 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim49296 = alloca %struct.ScmObj*, align 8
%f40242 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae44083, %struct.ScmObj* %anf_45bind40383)
store volatile %struct.ScmObj* %f40242, %struct.ScmObj** %stackaddr$prim49296, align 8
%ae44086 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44087 = call %struct.ScmObj* @const_init_int(i64 5)
%stackaddr$prim49297 = alloca %struct.ScmObj*, align 8
%t4010840245 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %e40243, %struct.ScmObj* %ae44086, %struct.ScmObj* %ae44087)
store volatile %struct.ScmObj* %t4010840245, %struct.ScmObj** %stackaddr$prim49297, align 8
%ae44089 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49298 = alloca %struct.ScmObj*, align 8
%anf_45bind40384 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %e40243, %struct.ScmObj* %ae44089)
store volatile %struct.ScmObj* %anf_45bind40384, %struct.ScmObj** %stackaddr$prim49298, align 8
%ae44091 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49299 = alloca %struct.ScmObj*, align 8
%t4010740244 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %f40242, %struct.ScmObj* %ae44091, %struct.ScmObj* %anf_45bind40384)
store volatile %struct.ScmObj* %t4010740244, %struct.ScmObj** %stackaddr$prim49299, align 8
%ae44094 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49300 = alloca %struct.ScmObj*, align 8
%anf_45bind40385 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %x40229, %struct.ScmObj* %ae44094)
store volatile %struct.ScmObj* %anf_45bind40385, %struct.ScmObj** %stackaddr$prim49300, align 8
%ae44096 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49301 = alloca %struct.ScmObj*, align 8
%anf_45bind40386 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %f40242, %struct.ScmObj* %ae44096)
store volatile %struct.ScmObj* %anf_45bind40386, %struct.ScmObj** %stackaddr$prim49301, align 8
%stackaddr$prim49302 = alloca %struct.ScmObj*, align 8
%cpsprim40420 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %anf_45bind40385, %struct.ScmObj* %anf_45bind40386)
store volatile %struct.ScmObj* %cpsprim40420, %struct.ScmObj** %stackaddr$prim49302, align 8
%stackaddr$makeclosure49303 = alloca %struct.ScmObj*, align 8
%fptrToInt49304 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44099 to i64
%ae44099 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49304)
store volatile %struct.ScmObj* %ae44099, %struct.ScmObj** %stackaddr$makeclosure49303, align 8
%ae44100 = call %struct.ScmObj* @const_init_int(i64 0)
%args48432$ae44099$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49305 = alloca %struct.ScmObj*, align 8
%args48432$ae44099$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40420, %struct.ScmObj* %args48432$ae44099$0)
store volatile %struct.ScmObj* %args48432$ae44099$1, %struct.ScmObj** %stackaddr$prim49305, align 8
%stackaddr$prim49306 = alloca %struct.ScmObj*, align 8
%args48432$ae44099$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44100, %struct.ScmObj* %args48432$ae44099$1)
store volatile %struct.ScmObj* %args48432$ae44099$2, %struct.ScmObj** %stackaddr$prim49306, align 8
%clofunc49307 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44099)
musttail call tailcc void %clofunc49307(%struct.ScmObj* %ae44099, %struct.ScmObj* %args48432$ae44099$2)
ret void
}

define tailcc void @proc_clo$ae44099(%struct.ScmObj* %env$ae44099,%struct.ScmObj* %current_45args48428) {
%stackaddr$prim49308 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48428)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim49308, align 8
%stackaddr$prim49309 = alloca %struct.ScmObj*, align 8
%current_45args48429 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48428)
store volatile %struct.ScmObj* %current_45args48429, %struct.ScmObj** %stackaddr$prim49309, align 8
%stackaddr$prim49310 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48429)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim49310, align 8
%stackaddr$prim49311 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim49311, align 8
%args48431$k$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49312 = alloca %struct.ScmObj*, align 8
%args48431$k$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %args48431$k$0)
store volatile %struct.ScmObj* %args48431$k$1, %struct.ScmObj** %stackaddr$prim49312, align 8
%clofunc49313 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc49313(%struct.ScmObj* %k, %struct.ScmObj* %args48431$k$1)
ret void
}

define tailcc void @proc_clo$ae43929(%struct.ScmObj* %env$ae43929,%struct.ScmObj* %current_45args48433) {
%stackaddr$prim49314 = alloca %struct.ScmObj*, align 8
%k40421 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48433)
store volatile %struct.ScmObj* %k40421, %struct.ScmObj** %stackaddr$prim49314, align 8
%stackaddr$prim49315 = alloca %struct.ScmObj*, align 8
%current_45args48434 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48433)
store volatile %struct.ScmObj* %current_45args48434, %struct.ScmObj** %stackaddr$prim49315, align 8
%stackaddr$prim49316 = alloca %struct.ScmObj*, align 8
%thunk40228 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48434)
store volatile %struct.ScmObj* %thunk40228, %struct.ScmObj** %stackaddr$prim49316, align 8
%stackaddr$prim49317 = alloca %struct.ScmObj*, align 8
%anf_45bind40357 = call %struct.ScmObj* @prim_vector_63(%struct.ScmObj* %thunk40228)
store volatile %struct.ScmObj* %anf_45bind40357, %struct.ScmObj** %stackaddr$prim49317, align 8
%truthy$cmp49318 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40357)
%cmp$cmp49318 = icmp eq i64 %truthy$cmp49318, 1
br i1 %cmp$cmp49318, label %truebranch$cmp49318, label %falsebranch$cmp49318
truebranch$cmp49318:
%stackaddr$prim49319 = alloca %struct.ScmObj*, align 8
%anf_45bind40358 = call %struct.ScmObj* @prim_vector_45length(%struct.ScmObj* %thunk40228)
store volatile %struct.ScmObj* %anf_45bind40358, %struct.ScmObj** %stackaddr$prim49319, align 8
%ae43934 = call %struct.ScmObj* @const_init_int(i64 3)
%stackaddr$prim49320 = alloca %struct.ScmObj*, align 8
%anf_45bind40359 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind40358, %struct.ScmObj* %ae43934)
store volatile %struct.ScmObj* %anf_45bind40359, %struct.ScmObj** %stackaddr$prim49320, align 8
%truthy$cmp49321 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40359)
%cmp$cmp49321 = icmp eq i64 %truthy$cmp49321, 1
br i1 %cmp$cmp49321, label %truebranch$cmp49321, label %falsebranch$cmp49321
truebranch$cmp49321:
%ae43937 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49322 = alloca %struct.ScmObj*, align 8
%anf_45bind40360 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk40228, %struct.ScmObj* %ae43937)
store volatile %struct.ScmObj* %anf_45bind40360, %struct.ScmObj** %stackaddr$prim49322, align 8
%ae43939 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @global$sym$ae4393949323, i32 0, i32 0))
%stackaddr$prim49324 = alloca %struct.ScmObj*, align 8
%cpsprim40422 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind40360, %struct.ScmObj* %ae43939)
store volatile %struct.ScmObj* %cpsprim40422, %struct.ScmObj** %stackaddr$prim49324, align 8
%ae43941 = call %struct.ScmObj* @const_init_int(i64 0)
%args48436$k40421$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49325 = alloca %struct.ScmObj*, align 8
%args48436$k40421$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40422, %struct.ScmObj* %args48436$k40421$0)
store volatile %struct.ScmObj* %args48436$k40421$1, %struct.ScmObj** %stackaddr$prim49325, align 8
%stackaddr$prim49326 = alloca %struct.ScmObj*, align 8
%args48436$k40421$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43941, %struct.ScmObj* %args48436$k40421$1)
store volatile %struct.ScmObj* %args48436$k40421$2, %struct.ScmObj** %stackaddr$prim49326, align 8
%clofunc49327 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40421)
musttail call tailcc void %clofunc49327(%struct.ScmObj* %k40421, %struct.ScmObj* %args48436$k40421$2)
ret void
falsebranch$cmp49321:
%ae43959 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43960 = call %struct.ScmObj* @const_init_false()
%args48437$k40421$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49328 = alloca %struct.ScmObj*, align 8
%args48437$k40421$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43960, %struct.ScmObj* %args48437$k40421$0)
store volatile %struct.ScmObj* %args48437$k40421$1, %struct.ScmObj** %stackaddr$prim49328, align 8
%stackaddr$prim49329 = alloca %struct.ScmObj*, align 8
%args48437$k40421$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43959, %struct.ScmObj* %args48437$k40421$1)
store volatile %struct.ScmObj* %args48437$k40421$2, %struct.ScmObj** %stackaddr$prim49329, align 8
%clofunc49330 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40421)
musttail call tailcc void %clofunc49330(%struct.ScmObj* %k40421, %struct.ScmObj* %args48437$k40421$2)
ret void
falsebranch$cmp49318:
%ae43981 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43982 = call %struct.ScmObj* @const_init_false()
%args48438$k40421$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49331 = alloca %struct.ScmObj*, align 8
%args48438$k40421$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43982, %struct.ScmObj* %args48438$k40421$0)
store volatile %struct.ScmObj* %args48438$k40421$1, %struct.ScmObj** %stackaddr$prim49331, align 8
%stackaddr$prim49332 = alloca %struct.ScmObj*, align 8
%args48438$k40421$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43981, %struct.ScmObj* %args48438$k40421$1)
store volatile %struct.ScmObj* %args48438$k40421$2, %struct.ScmObj** %stackaddr$prim49332, align 8
%clofunc49333 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40421)
musttail call tailcc void %clofunc49333(%struct.ScmObj* %k40421, %struct.ScmObj* %args48438$k40421$2)
ret void
}

define tailcc void @proc_clo$ae43903(%struct.ScmObj* %env$ae43903,%struct.ScmObj* %current_45args48440) {
%stackaddr$prim49334 = alloca %struct.ScmObj*, align 8
%k40423 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48440)
store volatile %struct.ScmObj* %k40423, %struct.ScmObj** %stackaddr$prim49334, align 8
%stackaddr$prim49335 = alloca %struct.ScmObj*, align 8
%current_45args48441 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48440)
store volatile %struct.ScmObj* %current_45args48441, %struct.ScmObj** %stackaddr$prim49335, align 8
%stackaddr$prim49336 = alloca %struct.ScmObj*, align 8
%x40167 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48441)
store volatile %struct.ScmObj* %x40167, %struct.ScmObj** %stackaddr$prim49336, align 8
%stackaddr$prim49337 = alloca %struct.ScmObj*, align 8
%anf_45bind40354 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40167)
store volatile %struct.ScmObj* %anf_45bind40354, %struct.ScmObj** %stackaddr$prim49337, align 8
%stackaddr$prim49338 = alloca %struct.ScmObj*, align 8
%anf_45bind40355 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40354)
store volatile %struct.ScmObj* %anf_45bind40355, %struct.ScmObj** %stackaddr$prim49338, align 8
%stackaddr$prim49339 = alloca %struct.ScmObj*, align 8
%anf_45bind40356 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40355)
store volatile %struct.ScmObj* %anf_45bind40356, %struct.ScmObj** %stackaddr$prim49339, align 8
%stackaddr$prim49340 = alloca %struct.ScmObj*, align 8
%cpsprim40424 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40356)
store volatile %struct.ScmObj* %cpsprim40424, %struct.ScmObj** %stackaddr$prim49340, align 8
%ae43909 = call %struct.ScmObj* @const_init_int(i64 0)
%args48443$k40423$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49341 = alloca %struct.ScmObj*, align 8
%args48443$k40423$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40424, %struct.ScmObj* %args48443$k40423$0)
store volatile %struct.ScmObj* %args48443$k40423$1, %struct.ScmObj** %stackaddr$prim49341, align 8
%stackaddr$prim49342 = alloca %struct.ScmObj*, align 8
%args48443$k40423$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43909, %struct.ScmObj* %args48443$k40423$1)
store volatile %struct.ScmObj* %args48443$k40423$2, %struct.ScmObj** %stackaddr$prim49342, align 8
%clofunc49343 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40423)
musttail call tailcc void %clofunc49343(%struct.ScmObj* %k40423, %struct.ScmObj* %args48443$k40423$2)
ret void
}

define tailcc void @proc_clo$ae43879(%struct.ScmObj* %env$ae43879,%struct.ScmObj* %current_45args48445) {
%stackaddr$prim49344 = alloca %struct.ScmObj*, align 8
%k40425 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48445)
store volatile %struct.ScmObj* %k40425, %struct.ScmObj** %stackaddr$prim49344, align 8
%stackaddr$prim49345 = alloca %struct.ScmObj*, align 8
%current_45args48446 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48445)
store volatile %struct.ScmObj* %current_45args48446, %struct.ScmObj** %stackaddr$prim49345, align 8
%stackaddr$prim49346 = alloca %struct.ScmObj*, align 8
%x40169 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48446)
store volatile %struct.ScmObj* %x40169, %struct.ScmObj** %stackaddr$prim49346, align 8
%stackaddr$prim49347 = alloca %struct.ScmObj*, align 8
%anf_45bind40352 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40169)
store volatile %struct.ScmObj* %anf_45bind40352, %struct.ScmObj** %stackaddr$prim49347, align 8
%stackaddr$prim49348 = alloca %struct.ScmObj*, align 8
%anf_45bind40353 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40352)
store volatile %struct.ScmObj* %anf_45bind40353, %struct.ScmObj** %stackaddr$prim49348, align 8
%stackaddr$prim49349 = alloca %struct.ScmObj*, align 8
%cpsprim40426 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40353)
store volatile %struct.ScmObj* %cpsprim40426, %struct.ScmObj** %stackaddr$prim49349, align 8
%ae43884 = call %struct.ScmObj* @const_init_int(i64 0)
%args48448$k40425$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49350 = alloca %struct.ScmObj*, align 8
%args48448$k40425$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40426, %struct.ScmObj* %args48448$k40425$0)
store volatile %struct.ScmObj* %args48448$k40425$1, %struct.ScmObj** %stackaddr$prim49350, align 8
%stackaddr$prim49351 = alloca %struct.ScmObj*, align 8
%args48448$k40425$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43884, %struct.ScmObj* %args48448$k40425$1)
store volatile %struct.ScmObj* %args48448$k40425$2, %struct.ScmObj** %stackaddr$prim49351, align 8
%clofunc49352 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40425)
musttail call tailcc void %clofunc49352(%struct.ScmObj* %k40425, %struct.ScmObj* %args48448$k40425$2)
ret void
}

define tailcc void @proc_clo$ae43857(%struct.ScmObj* %env$ae43857,%struct.ScmObj* %current_45args48450) {
%stackaddr$prim49353 = alloca %struct.ScmObj*, align 8
%k40427 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48450)
store volatile %struct.ScmObj* %k40427, %struct.ScmObj** %stackaddr$prim49353, align 8
%stackaddr$prim49354 = alloca %struct.ScmObj*, align 8
%current_45args48451 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48450)
store volatile %struct.ScmObj* %current_45args48451, %struct.ScmObj** %stackaddr$prim49354, align 8
%stackaddr$prim49355 = alloca %struct.ScmObj*, align 8
%x40171 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48451)
store volatile %struct.ScmObj* %x40171, %struct.ScmObj** %stackaddr$prim49355, align 8
%stackaddr$prim49356 = alloca %struct.ScmObj*, align 8
%anf_45bind40351 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40171)
store volatile %struct.ScmObj* %anf_45bind40351, %struct.ScmObj** %stackaddr$prim49356, align 8
%stackaddr$prim49357 = alloca %struct.ScmObj*, align 8
%cpsprim40428 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40351)
store volatile %struct.ScmObj* %cpsprim40428, %struct.ScmObj** %stackaddr$prim49357, align 8
%ae43861 = call %struct.ScmObj* @const_init_int(i64 0)
%args48453$k40427$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49358 = alloca %struct.ScmObj*, align 8
%args48453$k40427$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40428, %struct.ScmObj* %args48453$k40427$0)
store volatile %struct.ScmObj* %args48453$k40427$1, %struct.ScmObj** %stackaddr$prim49358, align 8
%stackaddr$prim49359 = alloca %struct.ScmObj*, align 8
%args48453$k40427$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43861, %struct.ScmObj* %args48453$k40427$1)
store volatile %struct.ScmObj* %args48453$k40427$2, %struct.ScmObj** %stackaddr$prim49359, align 8
%clofunc49360 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40427)
musttail call tailcc void %clofunc49360(%struct.ScmObj* %k40427, %struct.ScmObj* %args48453$k40427$2)
ret void
}

define tailcc void @proc_clo$ae43837(%struct.ScmObj* %env$ae43837,%struct.ScmObj* %current_45args48455) {
%stackaddr$prim49361 = alloca %struct.ScmObj*, align 8
%k40429 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48455)
store volatile %struct.ScmObj* %k40429, %struct.ScmObj** %stackaddr$prim49361, align 8
%stackaddr$prim49362 = alloca %struct.ScmObj*, align 8
%current_45args48456 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48455)
store volatile %struct.ScmObj* %current_45args48456, %struct.ScmObj** %stackaddr$prim49362, align 8
%stackaddr$prim49363 = alloca %struct.ScmObj*, align 8
%x40173 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48456)
store volatile %struct.ScmObj* %x40173, %struct.ScmObj** %stackaddr$prim49363, align 8
%stackaddr$prim49364 = alloca %struct.ScmObj*, align 8
%cpsprim40430 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40173)
store volatile %struct.ScmObj* %cpsprim40430, %struct.ScmObj** %stackaddr$prim49364, align 8
%ae43840 = call %struct.ScmObj* @const_init_int(i64 0)
%args48458$k40429$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49365 = alloca %struct.ScmObj*, align 8
%args48458$k40429$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40430, %struct.ScmObj* %args48458$k40429$0)
store volatile %struct.ScmObj* %args48458$k40429$1, %struct.ScmObj** %stackaddr$prim49365, align 8
%stackaddr$prim49366 = alloca %struct.ScmObj*, align 8
%args48458$k40429$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43840, %struct.ScmObj* %args48458$k40429$1)
store volatile %struct.ScmObj* %args48458$k40429$2, %struct.ScmObj** %stackaddr$prim49366, align 8
%clofunc49367 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40429)
musttail call tailcc void %clofunc49367(%struct.ScmObj* %k40429, %struct.ScmObj* %args48458$k40429$2)
ret void
}

define tailcc void @proc_clo$ae43739(%struct.ScmObj* %env$ae43739,%struct.ScmObj* %args4017540431) {
%stackaddr$env-ref49368 = alloca %struct.ScmObj*, align 8
%_37foldl140114 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43739, i64 0)
store %struct.ScmObj* %_37foldl140114, %struct.ScmObj** %stackaddr$env-ref49368
%stackaddr$prim49369 = alloca %struct.ScmObj*, align 8
%k40432 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4017540431)
store volatile %struct.ScmObj* %k40432, %struct.ScmObj** %stackaddr$prim49369, align 8
%stackaddr$prim49370 = alloca %struct.ScmObj*, align 8
%args40175 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4017540431)
store volatile %struct.ScmObj* %args40175, %struct.ScmObj** %stackaddr$prim49370, align 8
%stackaddr$prim49371 = alloca %struct.ScmObj*, align 8
%anf_45bind40345 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args40175)
store volatile %struct.ScmObj* %anf_45bind40345, %struct.ScmObj** %stackaddr$prim49371, align 8
%truthy$cmp49372 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40345)
%cmp$cmp49372 = icmp eq i64 %truthy$cmp49372, 1
br i1 %cmp$cmp49372, label %truebranch$cmp49372, label %falsebranch$cmp49372
truebranch$cmp49372:
%ae43745 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43746 = call %struct.ScmObj* @const_init_int(i64 1)
%args48460$k40432$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49373 = alloca %struct.ScmObj*, align 8
%args48460$k40432$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43746, %struct.ScmObj* %args48460$k40432$0)
store volatile %struct.ScmObj* %args48460$k40432$1, %struct.ScmObj** %stackaddr$prim49373, align 8
%stackaddr$prim49374 = alloca %struct.ScmObj*, align 8
%args48460$k40432$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43745, %struct.ScmObj* %args48460$k40432$1)
store volatile %struct.ScmObj* %args48460$k40432$2, %struct.ScmObj** %stackaddr$prim49374, align 8
%clofunc49375 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40432)
musttail call tailcc void %clofunc49375(%struct.ScmObj* %k40432, %struct.ScmObj* %args48460$k40432$2)
ret void
falsebranch$cmp49372:
%stackaddr$prim49376 = alloca %struct.ScmObj*, align 8
%anf_45bind40346 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40175)
store volatile %struct.ScmObj* %anf_45bind40346, %struct.ScmObj** %stackaddr$prim49376, align 8
%stackaddr$prim49377 = alloca %struct.ScmObj*, align 8
%anf_45bind40347 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40346)
store volatile %struct.ScmObj* %anf_45bind40347, %struct.ScmObj** %stackaddr$prim49377, align 8
%truthy$cmp49378 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40347)
%cmp$cmp49378 = icmp eq i64 %truthy$cmp49378, 1
br i1 %cmp$cmp49378, label %truebranch$cmp49378, label %falsebranch$cmp49378
truebranch$cmp49378:
%stackaddr$prim49379 = alloca %struct.ScmObj*, align 8
%cpsprim40433 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40175)
store volatile %struct.ScmObj* %cpsprim40433, %struct.ScmObj** %stackaddr$prim49379, align 8
%ae43758 = call %struct.ScmObj* @const_init_int(i64 0)
%args48461$k40432$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49380 = alloca %struct.ScmObj*, align 8
%args48461$k40432$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40433, %struct.ScmObj* %args48461$k40432$0)
store volatile %struct.ScmObj* %args48461$k40432$1, %struct.ScmObj** %stackaddr$prim49380, align 8
%stackaddr$prim49381 = alloca %struct.ScmObj*, align 8
%args48461$k40432$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43758, %struct.ScmObj* %args48461$k40432$1)
store volatile %struct.ScmObj* %args48461$k40432$2, %struct.ScmObj** %stackaddr$prim49381, align 8
%clofunc49382 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40432)
musttail call tailcc void %clofunc49382(%struct.ScmObj* %k40432, %struct.ScmObj* %args48461$k40432$2)
ret void
falsebranch$cmp49378:
%stackaddr$makeclosure49383 = alloca %struct.ScmObj*, align 8
%fptrToInt49384 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43763 to i64
%ae43763 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49384)
store volatile %struct.ScmObj* %ae43763, %struct.ScmObj** %stackaddr$makeclosure49383, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43763, %struct.ScmObj* %_37foldl140114, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43763, %struct.ScmObj* %k40432, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43763, %struct.ScmObj* %args40175, i64 2)
%ae43764 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49385 = alloca %struct.ScmObj*, align 8
%fptrToInt49386 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43765 to i64
%ae43765 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49386)
store volatile %struct.ScmObj* %ae43765, %struct.ScmObj** %stackaddr$makeclosure49385, align 8
%args48471$ae43763$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49387 = alloca %struct.ScmObj*, align 8
%args48471$ae43763$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43765, %struct.ScmObj* %args48471$ae43763$0)
store volatile %struct.ScmObj* %args48471$ae43763$1, %struct.ScmObj** %stackaddr$prim49387, align 8
%stackaddr$prim49388 = alloca %struct.ScmObj*, align 8
%args48471$ae43763$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43764, %struct.ScmObj* %args48471$ae43763$1)
store volatile %struct.ScmObj* %args48471$ae43763$2, %struct.ScmObj** %stackaddr$prim49388, align 8
%clofunc49389 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43763)
musttail call tailcc void %clofunc49389(%struct.ScmObj* %ae43763, %struct.ScmObj* %args48471$ae43763$2)
ret void
}

define tailcc void @proc_clo$ae43763(%struct.ScmObj* %env$ae43763,%struct.ScmObj* %current_45args48462) {
%stackaddr$env-ref49390 = alloca %struct.ScmObj*, align 8
%_37foldl140114 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43763, i64 0)
store %struct.ScmObj* %_37foldl140114, %struct.ScmObj** %stackaddr$env-ref49390
%stackaddr$env-ref49391 = alloca %struct.ScmObj*, align 8
%k40432 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43763, i64 1)
store %struct.ScmObj* %k40432, %struct.ScmObj** %stackaddr$env-ref49391
%stackaddr$env-ref49392 = alloca %struct.ScmObj*, align 8
%args40175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43763, i64 2)
store %struct.ScmObj* %args40175, %struct.ScmObj** %stackaddr$env-ref49392
%stackaddr$prim49393 = alloca %struct.ScmObj*, align 8
%_95k40434 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48462)
store volatile %struct.ScmObj* %_95k40434, %struct.ScmObj** %stackaddr$prim49393, align 8
%stackaddr$prim49394 = alloca %struct.ScmObj*, align 8
%current_45args48463 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48462)
store volatile %struct.ScmObj* %current_45args48463, %struct.ScmObj** %stackaddr$prim49394, align 8
%stackaddr$prim49395 = alloca %struct.ScmObj*, align 8
%anf_45bind40348 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48463)
store volatile %struct.ScmObj* %anf_45bind40348, %struct.ScmObj** %stackaddr$prim49395, align 8
%stackaddr$prim49396 = alloca %struct.ScmObj*, align 8
%anf_45bind40349 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40175)
store volatile %struct.ScmObj* %anf_45bind40349, %struct.ScmObj** %stackaddr$prim49396, align 8
%stackaddr$prim49397 = alloca %struct.ScmObj*, align 8
%anf_45bind40350 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40175)
store volatile %struct.ScmObj* %anf_45bind40350, %struct.ScmObj** %stackaddr$prim49397, align 8
%args48465$_37foldl140114$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49398 = alloca %struct.ScmObj*, align 8
%args48465$_37foldl140114$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40350, %struct.ScmObj* %args48465$_37foldl140114$0)
store volatile %struct.ScmObj* %args48465$_37foldl140114$1, %struct.ScmObj** %stackaddr$prim49398, align 8
%stackaddr$prim49399 = alloca %struct.ScmObj*, align 8
%args48465$_37foldl140114$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40349, %struct.ScmObj* %args48465$_37foldl140114$1)
store volatile %struct.ScmObj* %args48465$_37foldl140114$2, %struct.ScmObj** %stackaddr$prim49399, align 8
%stackaddr$prim49400 = alloca %struct.ScmObj*, align 8
%args48465$_37foldl140114$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40348, %struct.ScmObj* %args48465$_37foldl140114$2)
store volatile %struct.ScmObj* %args48465$_37foldl140114$3, %struct.ScmObj** %stackaddr$prim49400, align 8
%stackaddr$prim49401 = alloca %struct.ScmObj*, align 8
%args48465$_37foldl140114$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40432, %struct.ScmObj* %args48465$_37foldl140114$3)
store volatile %struct.ScmObj* %args48465$_37foldl140114$4, %struct.ScmObj** %stackaddr$prim49401, align 8
%clofunc49402 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140114)
musttail call tailcc void %clofunc49402(%struct.ScmObj* %_37foldl140114, %struct.ScmObj* %args48465$_37foldl140114$4)
ret void
}

define tailcc void @proc_clo$ae43765(%struct.ScmObj* %env$ae43765,%struct.ScmObj* %current_45args48466) {
%stackaddr$prim49403 = alloca %struct.ScmObj*, align 8
%k40435 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48466)
store volatile %struct.ScmObj* %k40435, %struct.ScmObj** %stackaddr$prim49403, align 8
%stackaddr$prim49404 = alloca %struct.ScmObj*, align 8
%current_45args48467 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48466)
store volatile %struct.ScmObj* %current_45args48467, %struct.ScmObj** %stackaddr$prim49404, align 8
%stackaddr$prim49405 = alloca %struct.ScmObj*, align 8
%n40177 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48467)
store volatile %struct.ScmObj* %n40177, %struct.ScmObj** %stackaddr$prim49405, align 8
%stackaddr$prim49406 = alloca %struct.ScmObj*, align 8
%current_45args48468 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48467)
store volatile %struct.ScmObj* %current_45args48468, %struct.ScmObj** %stackaddr$prim49406, align 8
%stackaddr$prim49407 = alloca %struct.ScmObj*, align 8
%v40176 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48468)
store volatile %struct.ScmObj* %v40176, %struct.ScmObj** %stackaddr$prim49407, align 8
%stackaddr$prim49408 = alloca %struct.ScmObj*, align 8
%cpsprim40436 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v40176, %struct.ScmObj* %n40177)
store volatile %struct.ScmObj* %cpsprim40436, %struct.ScmObj** %stackaddr$prim49408, align 8
%ae43769 = call %struct.ScmObj* @const_init_int(i64 0)
%args48470$k40435$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49409 = alloca %struct.ScmObj*, align 8
%args48470$k40435$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40436, %struct.ScmObj* %args48470$k40435$0)
store volatile %struct.ScmObj* %args48470$k40435$1, %struct.ScmObj** %stackaddr$prim49409, align 8
%stackaddr$prim49410 = alloca %struct.ScmObj*, align 8
%args48470$k40435$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43769, %struct.ScmObj* %args48470$k40435$1)
store volatile %struct.ScmObj* %args48470$k40435$2, %struct.ScmObj** %stackaddr$prim49410, align 8
%clofunc49411 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40435)
musttail call tailcc void %clofunc49411(%struct.ScmObj* %k40435, %struct.ScmObj* %args48470$k40435$2)
ret void
}

define tailcc void @proc_clo$ae43335(%struct.ScmObj* %env$ae43335,%struct.ScmObj* %current_45args48473) {
%stackaddr$prim49412 = alloca %struct.ScmObj*, align 8
%k40437 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48473)
store volatile %struct.ScmObj* %k40437, %struct.ScmObj** %stackaddr$prim49412, align 8
%stackaddr$prim49413 = alloca %struct.ScmObj*, align 8
%current_45args48474 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48473)
store volatile %struct.ScmObj* %current_45args48474, %struct.ScmObj** %stackaddr$prim49413, align 8
%stackaddr$prim49414 = alloca %struct.ScmObj*, align 8
%v40180 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48474)
store volatile %struct.ScmObj* %v40180, %struct.ScmObj** %stackaddr$prim49414, align 8
%stackaddr$prim49415 = alloca %struct.ScmObj*, align 8
%current_45args48475 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48474)
store volatile %struct.ScmObj* %current_45args48475, %struct.ScmObj** %stackaddr$prim49415, align 8
%stackaddr$prim49416 = alloca %struct.ScmObj*, align 8
%lst40179 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48475)
store volatile %struct.ScmObj* %lst40179, %struct.ScmObj** %stackaddr$prim49416, align 8
%ae43336 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim49417 = alloca %struct.ScmObj*, align 8
%lst40181 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae43336, %struct.ScmObj* %lst40179)
store volatile %struct.ScmObj* %lst40181, %struct.ScmObj** %stackaddr$prim49417, align 8
%stackaddr$makeclosure49418 = alloca %struct.ScmObj*, align 8
%fptrToInt49419 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43338 to i64
%ae43338 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49419)
store volatile %struct.ScmObj* %ae43338, %struct.ScmObj** %stackaddr$makeclosure49418, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43338, %struct.ScmObj* %k40437, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43338, %struct.ScmObj* %lst40181, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43338, %struct.ScmObj* %v40180, i64 2)
%ae43339 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49420 = alloca %struct.ScmObj*, align 8
%fptrToInt49421 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43340 to i64
%ae43340 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49421)
store volatile %struct.ScmObj* %ae43340, %struct.ScmObj** %stackaddr$makeclosure49420, align 8
%args48497$ae43338$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49422 = alloca %struct.ScmObj*, align 8
%args48497$ae43338$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43340, %struct.ScmObj* %args48497$ae43338$0)
store volatile %struct.ScmObj* %args48497$ae43338$1, %struct.ScmObj** %stackaddr$prim49422, align 8
%stackaddr$prim49423 = alloca %struct.ScmObj*, align 8
%args48497$ae43338$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43339, %struct.ScmObj* %args48497$ae43338$1)
store volatile %struct.ScmObj* %args48497$ae43338$2, %struct.ScmObj** %stackaddr$prim49423, align 8
%clofunc49424 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43338)
musttail call tailcc void %clofunc49424(%struct.ScmObj* %ae43338, %struct.ScmObj* %args48497$ae43338$2)
ret void
}

define tailcc void @proc_clo$ae43338(%struct.ScmObj* %env$ae43338,%struct.ScmObj* %current_45args48477) {
%stackaddr$env-ref49425 = alloca %struct.ScmObj*, align 8
%k40437 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43338, i64 0)
store %struct.ScmObj* %k40437, %struct.ScmObj** %stackaddr$env-ref49425
%stackaddr$env-ref49426 = alloca %struct.ScmObj*, align 8
%lst40181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43338, i64 1)
store %struct.ScmObj* %lst40181, %struct.ScmObj** %stackaddr$env-ref49426
%stackaddr$env-ref49427 = alloca %struct.ScmObj*, align 8
%v40180 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43338, i64 2)
store %struct.ScmObj* %v40180, %struct.ScmObj** %stackaddr$env-ref49427
%stackaddr$prim49428 = alloca %struct.ScmObj*, align 8
%_95k40438 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48477)
store volatile %struct.ScmObj* %_95k40438, %struct.ScmObj** %stackaddr$prim49428, align 8
%stackaddr$prim49429 = alloca %struct.ScmObj*, align 8
%current_45args48478 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48477)
store volatile %struct.ScmObj* %current_45args48478, %struct.ScmObj** %stackaddr$prim49429, align 8
%stackaddr$prim49430 = alloca %struct.ScmObj*, align 8
%anf_45bind40337 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48478)
store volatile %struct.ScmObj* %anf_45bind40337, %struct.ScmObj** %stackaddr$prim49430, align 8
%stackaddr$makeclosure49431 = alloca %struct.ScmObj*, align 8
%fptrToInt49432 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43354 to i64
%ae43354 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49432)
store volatile %struct.ScmObj* %ae43354, %struct.ScmObj** %stackaddr$makeclosure49431, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43354, %struct.ScmObj* %k40437, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43354, %struct.ScmObj* %lst40181, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43354, %struct.ScmObj* %v40180, i64 2)
%stackaddr$makeclosure49433 = alloca %struct.ScmObj*, align 8
%fptrToInt49434 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43355 to i64
%ae43355 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49434)
store volatile %struct.ScmObj* %ae43355, %struct.ScmObj** %stackaddr$makeclosure49433, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43355, %struct.ScmObj* %k40437, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43355, %struct.ScmObj* %lst40181, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43355, %struct.ScmObj* %v40180, i64 2)
%args48492$anf_45bind40337$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49435 = alloca %struct.ScmObj*, align 8
%args48492$anf_45bind40337$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43355, %struct.ScmObj* %args48492$anf_45bind40337$0)
store volatile %struct.ScmObj* %args48492$anf_45bind40337$1, %struct.ScmObj** %stackaddr$prim49435, align 8
%stackaddr$prim49436 = alloca %struct.ScmObj*, align 8
%args48492$anf_45bind40337$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43354, %struct.ScmObj* %args48492$anf_45bind40337$1)
store volatile %struct.ScmObj* %args48492$anf_45bind40337$2, %struct.ScmObj** %stackaddr$prim49436, align 8
%clofunc49437 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40337)
musttail call tailcc void %clofunc49437(%struct.ScmObj* %anf_45bind40337, %struct.ScmObj* %args48492$anf_45bind40337$2)
ret void
}

define tailcc void @proc_clo$ae43354(%struct.ScmObj* %env$ae43354,%struct.ScmObj* %current_45args48480) {
%stackaddr$env-ref49438 = alloca %struct.ScmObj*, align 8
%k40437 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43354, i64 0)
store %struct.ScmObj* %k40437, %struct.ScmObj** %stackaddr$env-ref49438
%stackaddr$env-ref49439 = alloca %struct.ScmObj*, align 8
%lst40181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43354, i64 1)
store %struct.ScmObj* %lst40181, %struct.ScmObj** %stackaddr$env-ref49439
%stackaddr$env-ref49440 = alloca %struct.ScmObj*, align 8
%v40180 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43354, i64 2)
store %struct.ScmObj* %v40180, %struct.ScmObj** %stackaddr$env-ref49440
%stackaddr$prim49441 = alloca %struct.ScmObj*, align 8
%_95k40439 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48480)
store volatile %struct.ScmObj* %_95k40439, %struct.ScmObj** %stackaddr$prim49441, align 8
%stackaddr$prim49442 = alloca %struct.ScmObj*, align 8
%current_45args48481 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48480)
store volatile %struct.ScmObj* %current_45args48481, %struct.ScmObj** %stackaddr$prim49442, align 8
%stackaddr$prim49443 = alloca %struct.ScmObj*, align 8
%cc40182 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48481)
store volatile %struct.ScmObj* %cc40182, %struct.ScmObj** %stackaddr$prim49443, align 8
%ae43463 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49444 = alloca %struct.ScmObj*, align 8
%anf_45bind40338 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40181, %struct.ScmObj* %ae43463)
store volatile %struct.ScmObj* %anf_45bind40338, %struct.ScmObj** %stackaddr$prim49444, align 8
%stackaddr$prim49445 = alloca %struct.ScmObj*, align 8
%anf_45bind40339 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40338)
store volatile %struct.ScmObj* %anf_45bind40339, %struct.ScmObj** %stackaddr$prim49445, align 8
%truthy$cmp49446 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40339)
%cmp$cmp49446 = icmp eq i64 %truthy$cmp49446, 1
br i1 %cmp$cmp49446, label %truebranch$cmp49446, label %falsebranch$cmp49446
truebranch$cmp49446:
%ae43467 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43468 = call %struct.ScmObj* @const_init_false()
%args48483$k40437$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49447 = alloca %struct.ScmObj*, align 8
%args48483$k40437$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43468, %struct.ScmObj* %args48483$k40437$0)
store volatile %struct.ScmObj* %args48483$k40437$1, %struct.ScmObj** %stackaddr$prim49447, align 8
%stackaddr$prim49448 = alloca %struct.ScmObj*, align 8
%args48483$k40437$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43467, %struct.ScmObj* %args48483$k40437$1)
store volatile %struct.ScmObj* %args48483$k40437$2, %struct.ScmObj** %stackaddr$prim49448, align 8
%clofunc49449 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40437)
musttail call tailcc void %clofunc49449(%struct.ScmObj* %k40437, %struct.ScmObj* %args48483$k40437$2)
ret void
falsebranch$cmp49446:
%ae43476 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49450 = alloca %struct.ScmObj*, align 8
%anf_45bind40340 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40181, %struct.ScmObj* %ae43476)
store volatile %struct.ScmObj* %anf_45bind40340, %struct.ScmObj** %stackaddr$prim49450, align 8
%stackaddr$prim49451 = alloca %struct.ScmObj*, align 8
%anf_45bind40341 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40340)
store volatile %struct.ScmObj* %anf_45bind40341, %struct.ScmObj** %stackaddr$prim49451, align 8
%stackaddr$prim49452 = alloca %struct.ScmObj*, align 8
%anf_45bind40342 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind40341, %struct.ScmObj* %v40180)
store volatile %struct.ScmObj* %anf_45bind40342, %struct.ScmObj** %stackaddr$prim49452, align 8
%truthy$cmp49453 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40342)
%cmp$cmp49453 = icmp eq i64 %truthy$cmp49453, 1
br i1 %cmp$cmp49453, label %truebranch$cmp49453, label %falsebranch$cmp49453
truebranch$cmp49453:
%ae43482 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49454 = alloca %struct.ScmObj*, align 8
%cpsprim40440 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40181, %struct.ScmObj* %ae43482)
store volatile %struct.ScmObj* %cpsprim40440, %struct.ScmObj** %stackaddr$prim49454, align 8
%ae43484 = call %struct.ScmObj* @const_init_int(i64 0)
%args48484$k40437$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49455 = alloca %struct.ScmObj*, align 8
%args48484$k40437$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40440, %struct.ScmObj* %args48484$k40437$0)
store volatile %struct.ScmObj* %args48484$k40437$1, %struct.ScmObj** %stackaddr$prim49455, align 8
%stackaddr$prim49456 = alloca %struct.ScmObj*, align 8
%args48484$k40437$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43484, %struct.ScmObj* %args48484$k40437$1)
store volatile %struct.ScmObj* %args48484$k40437$2, %struct.ScmObj** %stackaddr$prim49456, align 8
%clofunc49457 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40437)
musttail call tailcc void %clofunc49457(%struct.ScmObj* %k40437, %struct.ScmObj* %args48484$k40437$2)
ret void
falsebranch$cmp49453:
%ae43495 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49458 = alloca %struct.ScmObj*, align 8
%anf_45bind40343 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40181, %struct.ScmObj* %ae43495)
store volatile %struct.ScmObj* %anf_45bind40343, %struct.ScmObj** %stackaddr$prim49458, align 8
%stackaddr$prim49459 = alloca %struct.ScmObj*, align 8
%anf_45bind40344 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40343)
store volatile %struct.ScmObj* %anf_45bind40344, %struct.ScmObj** %stackaddr$prim49459, align 8
%ae43498 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49460 = alloca %struct.ScmObj*, align 8
%_95040184 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40181, %struct.ScmObj* %ae43498, %struct.ScmObj* %anf_45bind40344)
store volatile %struct.ScmObj* %_95040184, %struct.ScmObj** %stackaddr$prim49460, align 8
%args48485$cc40182$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49461 = alloca %struct.ScmObj*, align 8
%args48485$cc40182$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40182, %struct.ScmObj* %args48485$cc40182$0)
store volatile %struct.ScmObj* %args48485$cc40182$1, %struct.ScmObj** %stackaddr$prim49461, align 8
%stackaddr$prim49462 = alloca %struct.ScmObj*, align 8
%args48485$cc40182$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40437, %struct.ScmObj* %args48485$cc40182$1)
store volatile %struct.ScmObj* %args48485$cc40182$2, %struct.ScmObj** %stackaddr$prim49462, align 8
%clofunc49463 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40182)
musttail call tailcc void %clofunc49463(%struct.ScmObj* %cc40182, %struct.ScmObj* %args48485$cc40182$2)
ret void
}

define tailcc void @proc_clo$ae43355(%struct.ScmObj* %env$ae43355,%struct.ScmObj* %current_45args48486) {
%stackaddr$env-ref49464 = alloca %struct.ScmObj*, align 8
%k40437 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43355, i64 0)
store %struct.ScmObj* %k40437, %struct.ScmObj** %stackaddr$env-ref49464
%stackaddr$env-ref49465 = alloca %struct.ScmObj*, align 8
%lst40181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43355, i64 1)
store %struct.ScmObj* %lst40181, %struct.ScmObj** %stackaddr$env-ref49465
%stackaddr$env-ref49466 = alloca %struct.ScmObj*, align 8
%v40180 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43355, i64 2)
store %struct.ScmObj* %v40180, %struct.ScmObj** %stackaddr$env-ref49466
%stackaddr$prim49467 = alloca %struct.ScmObj*, align 8
%_95k40439 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48486)
store volatile %struct.ScmObj* %_95k40439, %struct.ScmObj** %stackaddr$prim49467, align 8
%stackaddr$prim49468 = alloca %struct.ScmObj*, align 8
%current_45args48487 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48486)
store volatile %struct.ScmObj* %current_45args48487, %struct.ScmObj** %stackaddr$prim49468, align 8
%stackaddr$prim49469 = alloca %struct.ScmObj*, align 8
%cc40182 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48487)
store volatile %struct.ScmObj* %cc40182, %struct.ScmObj** %stackaddr$prim49469, align 8
%ae43357 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49470 = alloca %struct.ScmObj*, align 8
%anf_45bind40338 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40181, %struct.ScmObj* %ae43357)
store volatile %struct.ScmObj* %anf_45bind40338, %struct.ScmObj** %stackaddr$prim49470, align 8
%stackaddr$prim49471 = alloca %struct.ScmObj*, align 8
%anf_45bind40339 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40338)
store volatile %struct.ScmObj* %anf_45bind40339, %struct.ScmObj** %stackaddr$prim49471, align 8
%truthy$cmp49472 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40339)
%cmp$cmp49472 = icmp eq i64 %truthy$cmp49472, 1
br i1 %cmp$cmp49472, label %truebranch$cmp49472, label %falsebranch$cmp49472
truebranch$cmp49472:
%ae43361 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43362 = call %struct.ScmObj* @const_init_false()
%args48489$k40437$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49473 = alloca %struct.ScmObj*, align 8
%args48489$k40437$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43362, %struct.ScmObj* %args48489$k40437$0)
store volatile %struct.ScmObj* %args48489$k40437$1, %struct.ScmObj** %stackaddr$prim49473, align 8
%stackaddr$prim49474 = alloca %struct.ScmObj*, align 8
%args48489$k40437$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43361, %struct.ScmObj* %args48489$k40437$1)
store volatile %struct.ScmObj* %args48489$k40437$2, %struct.ScmObj** %stackaddr$prim49474, align 8
%clofunc49475 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40437)
musttail call tailcc void %clofunc49475(%struct.ScmObj* %k40437, %struct.ScmObj* %args48489$k40437$2)
ret void
falsebranch$cmp49472:
%ae43370 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49476 = alloca %struct.ScmObj*, align 8
%anf_45bind40340 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40181, %struct.ScmObj* %ae43370)
store volatile %struct.ScmObj* %anf_45bind40340, %struct.ScmObj** %stackaddr$prim49476, align 8
%stackaddr$prim49477 = alloca %struct.ScmObj*, align 8
%anf_45bind40341 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40340)
store volatile %struct.ScmObj* %anf_45bind40341, %struct.ScmObj** %stackaddr$prim49477, align 8
%stackaddr$prim49478 = alloca %struct.ScmObj*, align 8
%anf_45bind40342 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind40341, %struct.ScmObj* %v40180)
store volatile %struct.ScmObj* %anf_45bind40342, %struct.ScmObj** %stackaddr$prim49478, align 8
%truthy$cmp49479 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40342)
%cmp$cmp49479 = icmp eq i64 %truthy$cmp49479, 1
br i1 %cmp$cmp49479, label %truebranch$cmp49479, label %falsebranch$cmp49479
truebranch$cmp49479:
%ae43376 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49480 = alloca %struct.ScmObj*, align 8
%cpsprim40440 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40181, %struct.ScmObj* %ae43376)
store volatile %struct.ScmObj* %cpsprim40440, %struct.ScmObj** %stackaddr$prim49480, align 8
%ae43378 = call %struct.ScmObj* @const_init_int(i64 0)
%args48490$k40437$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49481 = alloca %struct.ScmObj*, align 8
%args48490$k40437$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40440, %struct.ScmObj* %args48490$k40437$0)
store volatile %struct.ScmObj* %args48490$k40437$1, %struct.ScmObj** %stackaddr$prim49481, align 8
%stackaddr$prim49482 = alloca %struct.ScmObj*, align 8
%args48490$k40437$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43378, %struct.ScmObj* %args48490$k40437$1)
store volatile %struct.ScmObj* %args48490$k40437$2, %struct.ScmObj** %stackaddr$prim49482, align 8
%clofunc49483 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40437)
musttail call tailcc void %clofunc49483(%struct.ScmObj* %k40437, %struct.ScmObj* %args48490$k40437$2)
ret void
falsebranch$cmp49479:
%ae43389 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49484 = alloca %struct.ScmObj*, align 8
%anf_45bind40343 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40181, %struct.ScmObj* %ae43389)
store volatile %struct.ScmObj* %anf_45bind40343, %struct.ScmObj** %stackaddr$prim49484, align 8
%stackaddr$prim49485 = alloca %struct.ScmObj*, align 8
%anf_45bind40344 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40343)
store volatile %struct.ScmObj* %anf_45bind40344, %struct.ScmObj** %stackaddr$prim49485, align 8
%ae43392 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49486 = alloca %struct.ScmObj*, align 8
%_95040184 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40181, %struct.ScmObj* %ae43392, %struct.ScmObj* %anf_45bind40344)
store volatile %struct.ScmObj* %_95040184, %struct.ScmObj** %stackaddr$prim49486, align 8
%args48491$cc40182$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49487 = alloca %struct.ScmObj*, align 8
%args48491$cc40182$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40182, %struct.ScmObj* %args48491$cc40182$0)
store volatile %struct.ScmObj* %args48491$cc40182$1, %struct.ScmObj** %stackaddr$prim49487, align 8
%stackaddr$prim49488 = alloca %struct.ScmObj*, align 8
%args48491$cc40182$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40437, %struct.ScmObj* %args48491$cc40182$1)
store volatile %struct.ScmObj* %args48491$cc40182$2, %struct.ScmObj** %stackaddr$prim49488, align 8
%clofunc49489 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40182)
musttail call tailcc void %clofunc49489(%struct.ScmObj* %cc40182, %struct.ScmObj* %args48491$cc40182$2)
ret void
}

define tailcc void @proc_clo$ae43340(%struct.ScmObj* %env$ae43340,%struct.ScmObj* %current_45args48493) {
%stackaddr$prim49490 = alloca %struct.ScmObj*, align 8
%k40441 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48493)
store volatile %struct.ScmObj* %k40441, %struct.ScmObj** %stackaddr$prim49490, align 8
%stackaddr$prim49491 = alloca %struct.ScmObj*, align 8
%current_45args48494 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48493)
store volatile %struct.ScmObj* %current_45args48494, %struct.ScmObj** %stackaddr$prim49491, align 8
%stackaddr$prim49492 = alloca %struct.ScmObj*, align 8
%u40183 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48494)
store volatile %struct.ScmObj* %u40183, %struct.ScmObj** %stackaddr$prim49492, align 8
%args48496$u40183$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49493 = alloca %struct.ScmObj*, align 8
%args48496$u40183$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u40183, %struct.ScmObj* %args48496$u40183$0)
store volatile %struct.ScmObj* %args48496$u40183$1, %struct.ScmObj** %stackaddr$prim49493, align 8
%stackaddr$prim49494 = alloca %struct.ScmObj*, align 8
%args48496$u40183$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40441, %struct.ScmObj* %args48496$u40183$1)
store volatile %struct.ScmObj* %args48496$u40183$2, %struct.ScmObj** %stackaddr$prim49494, align 8
%clofunc49495 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u40183)
musttail call tailcc void %clofunc49495(%struct.ScmObj* %u40183, %struct.ScmObj* %args48496$u40183$2)
ret void
}

define tailcc void @proc_clo$ae42799(%struct.ScmObj* %env$ae42799,%struct.ScmObj* %current_45args48499) {
%stackaddr$prim49496 = alloca %struct.ScmObj*, align 8
%k40442 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48499)
store volatile %struct.ScmObj* %k40442, %struct.ScmObj** %stackaddr$prim49496, align 8
%stackaddr$prim49497 = alloca %struct.ScmObj*, align 8
%current_45args48500 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48499)
store volatile %struct.ScmObj* %current_45args48500, %struct.ScmObj** %stackaddr$prim49497, align 8
%stackaddr$prim49498 = alloca %struct.ScmObj*, align 8
%lst40187 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48500)
store volatile %struct.ScmObj* %lst40187, %struct.ScmObj** %stackaddr$prim49498, align 8
%stackaddr$prim49499 = alloca %struct.ScmObj*, align 8
%current_45args48501 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48500)
store volatile %struct.ScmObj* %current_45args48501, %struct.ScmObj** %stackaddr$prim49499, align 8
%stackaddr$prim49500 = alloca %struct.ScmObj*, align 8
%n40186 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48501)
store volatile %struct.ScmObj* %n40186, %struct.ScmObj** %stackaddr$prim49500, align 8
%ae42800 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim49501 = alloca %struct.ScmObj*, align 8
%n40189 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42800, %struct.ScmObj* %n40186)
store volatile %struct.ScmObj* %n40189, %struct.ScmObj** %stackaddr$prim49501, align 8
%ae42802 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim49502 = alloca %struct.ScmObj*, align 8
%lst40188 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42802, %struct.ScmObj* %lst40187)
store volatile %struct.ScmObj* %lst40188, %struct.ScmObj** %stackaddr$prim49502, align 8
%stackaddr$makeclosure49503 = alloca %struct.ScmObj*, align 8
%fptrToInt49504 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42804 to i64
%ae42804 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49504)
store volatile %struct.ScmObj* %ae42804, %struct.ScmObj** %stackaddr$makeclosure49503, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42804, %struct.ScmObj* %n40189, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42804, %struct.ScmObj* %lst40188, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42804, %struct.ScmObj* %k40442, i64 2)
%ae42805 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49505 = alloca %struct.ScmObj*, align 8
%fptrToInt49506 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42806 to i64
%ae42806 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49506)
store volatile %struct.ScmObj* %ae42806, %struct.ScmObj** %stackaddr$makeclosure49505, align 8
%args48521$ae42804$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49507 = alloca %struct.ScmObj*, align 8
%args48521$ae42804$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42806, %struct.ScmObj* %args48521$ae42804$0)
store volatile %struct.ScmObj* %args48521$ae42804$1, %struct.ScmObj** %stackaddr$prim49507, align 8
%stackaddr$prim49508 = alloca %struct.ScmObj*, align 8
%args48521$ae42804$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42805, %struct.ScmObj* %args48521$ae42804$1)
store volatile %struct.ScmObj* %args48521$ae42804$2, %struct.ScmObj** %stackaddr$prim49508, align 8
%clofunc49509 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42804)
musttail call tailcc void %clofunc49509(%struct.ScmObj* %ae42804, %struct.ScmObj* %args48521$ae42804$2)
ret void
}

define tailcc void @proc_clo$ae42804(%struct.ScmObj* %env$ae42804,%struct.ScmObj* %current_45args48503) {
%stackaddr$env-ref49510 = alloca %struct.ScmObj*, align 8
%n40189 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42804, i64 0)
store %struct.ScmObj* %n40189, %struct.ScmObj** %stackaddr$env-ref49510
%stackaddr$env-ref49511 = alloca %struct.ScmObj*, align 8
%lst40188 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42804, i64 1)
store %struct.ScmObj* %lst40188, %struct.ScmObj** %stackaddr$env-ref49511
%stackaddr$env-ref49512 = alloca %struct.ScmObj*, align 8
%k40442 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42804, i64 2)
store %struct.ScmObj* %k40442, %struct.ScmObj** %stackaddr$env-ref49512
%stackaddr$prim49513 = alloca %struct.ScmObj*, align 8
%_95k40443 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48503)
store volatile %struct.ScmObj* %_95k40443, %struct.ScmObj** %stackaddr$prim49513, align 8
%stackaddr$prim49514 = alloca %struct.ScmObj*, align 8
%current_45args48504 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48503)
store volatile %struct.ScmObj* %current_45args48504, %struct.ScmObj** %stackaddr$prim49514, align 8
%stackaddr$prim49515 = alloca %struct.ScmObj*, align 8
%anf_45bind40330 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48504)
store volatile %struct.ScmObj* %anf_45bind40330, %struct.ScmObj** %stackaddr$prim49515, align 8
%stackaddr$makeclosure49516 = alloca %struct.ScmObj*, align 8
%fptrToInt49517 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42820 to i64
%ae42820 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49517)
store volatile %struct.ScmObj* %ae42820, %struct.ScmObj** %stackaddr$makeclosure49516, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42820, %struct.ScmObj* %n40189, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42820, %struct.ScmObj* %lst40188, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42820, %struct.ScmObj* %k40442, i64 2)
%stackaddr$makeclosure49518 = alloca %struct.ScmObj*, align 8
%fptrToInt49519 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42821 to i64
%ae42821 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49519)
store volatile %struct.ScmObj* %ae42821, %struct.ScmObj** %stackaddr$makeclosure49518, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42821, %struct.ScmObj* %n40189, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42821, %struct.ScmObj* %lst40188, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42821, %struct.ScmObj* %k40442, i64 2)
%args48516$anf_45bind40330$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49520 = alloca %struct.ScmObj*, align 8
%args48516$anf_45bind40330$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42821, %struct.ScmObj* %args48516$anf_45bind40330$0)
store volatile %struct.ScmObj* %args48516$anf_45bind40330$1, %struct.ScmObj** %stackaddr$prim49520, align 8
%stackaddr$prim49521 = alloca %struct.ScmObj*, align 8
%args48516$anf_45bind40330$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42820, %struct.ScmObj* %args48516$anf_45bind40330$1)
store volatile %struct.ScmObj* %args48516$anf_45bind40330$2, %struct.ScmObj** %stackaddr$prim49521, align 8
%clofunc49522 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40330)
musttail call tailcc void %clofunc49522(%struct.ScmObj* %anf_45bind40330, %struct.ScmObj* %args48516$anf_45bind40330$2)
ret void
}

define tailcc void @proc_clo$ae42820(%struct.ScmObj* %env$ae42820,%struct.ScmObj* %current_45args48506) {
%stackaddr$env-ref49523 = alloca %struct.ScmObj*, align 8
%n40189 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42820, i64 0)
store %struct.ScmObj* %n40189, %struct.ScmObj** %stackaddr$env-ref49523
%stackaddr$env-ref49524 = alloca %struct.ScmObj*, align 8
%lst40188 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42820, i64 1)
store %struct.ScmObj* %lst40188, %struct.ScmObj** %stackaddr$env-ref49524
%stackaddr$env-ref49525 = alloca %struct.ScmObj*, align 8
%k40442 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42820, i64 2)
store %struct.ScmObj* %k40442, %struct.ScmObj** %stackaddr$env-ref49525
%stackaddr$prim49526 = alloca %struct.ScmObj*, align 8
%_95k40444 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48506)
store volatile %struct.ScmObj* %_95k40444, %struct.ScmObj** %stackaddr$prim49526, align 8
%stackaddr$prim49527 = alloca %struct.ScmObj*, align 8
%current_45args48507 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48506)
store volatile %struct.ScmObj* %current_45args48507, %struct.ScmObj** %stackaddr$prim49527, align 8
%stackaddr$prim49528 = alloca %struct.ScmObj*, align 8
%cc40190 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48507)
store volatile %struct.ScmObj* %cc40190, %struct.ScmObj** %stackaddr$prim49528, align 8
%ae42963 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49529 = alloca %struct.ScmObj*, align 8
%anf_45bind40331 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40189, %struct.ScmObj* %ae42963)
store volatile %struct.ScmObj* %anf_45bind40331, %struct.ScmObj** %stackaddr$prim49529, align 8
%ae42964 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49530 = alloca %struct.ScmObj*, align 8
%anf_45bind40332 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae42964, %struct.ScmObj* %anf_45bind40331)
store volatile %struct.ScmObj* %anf_45bind40332, %struct.ScmObj** %stackaddr$prim49530, align 8
%truthy$cmp49531 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40332)
%cmp$cmp49531 = icmp eq i64 %truthy$cmp49531, 1
br i1 %cmp$cmp49531, label %truebranch$cmp49531, label %falsebranch$cmp49531
truebranch$cmp49531:
%ae42968 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49532 = alloca %struct.ScmObj*, align 8
%cpsprim40445 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40188, %struct.ScmObj* %ae42968)
store volatile %struct.ScmObj* %cpsprim40445, %struct.ScmObj** %stackaddr$prim49532, align 8
%ae42970 = call %struct.ScmObj* @const_init_int(i64 0)
%args48509$k40442$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49533 = alloca %struct.ScmObj*, align 8
%args48509$k40442$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40445, %struct.ScmObj* %args48509$k40442$0)
store volatile %struct.ScmObj* %args48509$k40442$1, %struct.ScmObj** %stackaddr$prim49533, align 8
%stackaddr$prim49534 = alloca %struct.ScmObj*, align 8
%args48509$k40442$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42970, %struct.ScmObj* %args48509$k40442$1)
store volatile %struct.ScmObj* %args48509$k40442$2, %struct.ScmObj** %stackaddr$prim49534, align 8
%clofunc49535 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40442)
musttail call tailcc void %clofunc49535(%struct.ScmObj* %k40442, %struct.ScmObj* %args48509$k40442$2)
ret void
falsebranch$cmp49531:
%ae42981 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49536 = alloca %struct.ScmObj*, align 8
%anf_45bind40333 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40188, %struct.ScmObj* %ae42981)
store volatile %struct.ScmObj* %anf_45bind40333, %struct.ScmObj** %stackaddr$prim49536, align 8
%stackaddr$prim49537 = alloca %struct.ScmObj*, align 8
%anf_45bind40334 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40333)
store volatile %struct.ScmObj* %anf_45bind40334, %struct.ScmObj** %stackaddr$prim49537, align 8
%ae42984 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49538 = alloca %struct.ScmObj*, align 8
%_95040193 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40188, %struct.ScmObj* %ae42984, %struct.ScmObj* %anf_45bind40334)
store volatile %struct.ScmObj* %_95040193, %struct.ScmObj** %stackaddr$prim49538, align 8
%ae42987 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49539 = alloca %struct.ScmObj*, align 8
%anf_45bind40335 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40189, %struct.ScmObj* %ae42987)
store volatile %struct.ScmObj* %anf_45bind40335, %struct.ScmObj** %stackaddr$prim49539, align 8
%ae42989 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim49540 = alloca %struct.ScmObj*, align 8
%anf_45bind40336 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind40335, %struct.ScmObj* %ae42989)
store volatile %struct.ScmObj* %anf_45bind40336, %struct.ScmObj** %stackaddr$prim49540, align 8
%ae42991 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49541 = alloca %struct.ScmObj*, align 8
%_95140192 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n40189, %struct.ScmObj* %ae42991, %struct.ScmObj* %anf_45bind40336)
store volatile %struct.ScmObj* %_95140192, %struct.ScmObj** %stackaddr$prim49541, align 8
%args48510$cc40190$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49542 = alloca %struct.ScmObj*, align 8
%args48510$cc40190$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40190, %struct.ScmObj* %args48510$cc40190$0)
store volatile %struct.ScmObj* %args48510$cc40190$1, %struct.ScmObj** %stackaddr$prim49542, align 8
%stackaddr$prim49543 = alloca %struct.ScmObj*, align 8
%args48510$cc40190$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40442, %struct.ScmObj* %args48510$cc40190$1)
store volatile %struct.ScmObj* %args48510$cc40190$2, %struct.ScmObj** %stackaddr$prim49543, align 8
%clofunc49544 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40190)
musttail call tailcc void %clofunc49544(%struct.ScmObj* %cc40190, %struct.ScmObj* %args48510$cc40190$2)
ret void
}

define tailcc void @proc_clo$ae42821(%struct.ScmObj* %env$ae42821,%struct.ScmObj* %current_45args48511) {
%stackaddr$env-ref49545 = alloca %struct.ScmObj*, align 8
%n40189 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42821, i64 0)
store %struct.ScmObj* %n40189, %struct.ScmObj** %stackaddr$env-ref49545
%stackaddr$env-ref49546 = alloca %struct.ScmObj*, align 8
%lst40188 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42821, i64 1)
store %struct.ScmObj* %lst40188, %struct.ScmObj** %stackaddr$env-ref49546
%stackaddr$env-ref49547 = alloca %struct.ScmObj*, align 8
%k40442 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42821, i64 2)
store %struct.ScmObj* %k40442, %struct.ScmObj** %stackaddr$env-ref49547
%stackaddr$prim49548 = alloca %struct.ScmObj*, align 8
%_95k40444 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48511)
store volatile %struct.ScmObj* %_95k40444, %struct.ScmObj** %stackaddr$prim49548, align 8
%stackaddr$prim49549 = alloca %struct.ScmObj*, align 8
%current_45args48512 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48511)
store volatile %struct.ScmObj* %current_45args48512, %struct.ScmObj** %stackaddr$prim49549, align 8
%stackaddr$prim49550 = alloca %struct.ScmObj*, align 8
%cc40190 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48512)
store volatile %struct.ScmObj* %cc40190, %struct.ScmObj** %stackaddr$prim49550, align 8
%ae42823 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49551 = alloca %struct.ScmObj*, align 8
%anf_45bind40331 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40189, %struct.ScmObj* %ae42823)
store volatile %struct.ScmObj* %anf_45bind40331, %struct.ScmObj** %stackaddr$prim49551, align 8
%ae42824 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49552 = alloca %struct.ScmObj*, align 8
%anf_45bind40332 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae42824, %struct.ScmObj* %anf_45bind40331)
store volatile %struct.ScmObj* %anf_45bind40332, %struct.ScmObj** %stackaddr$prim49552, align 8
%truthy$cmp49553 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40332)
%cmp$cmp49553 = icmp eq i64 %truthy$cmp49553, 1
br i1 %cmp$cmp49553, label %truebranch$cmp49553, label %falsebranch$cmp49553
truebranch$cmp49553:
%ae42828 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49554 = alloca %struct.ScmObj*, align 8
%cpsprim40445 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40188, %struct.ScmObj* %ae42828)
store volatile %struct.ScmObj* %cpsprim40445, %struct.ScmObj** %stackaddr$prim49554, align 8
%ae42830 = call %struct.ScmObj* @const_init_int(i64 0)
%args48514$k40442$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49555 = alloca %struct.ScmObj*, align 8
%args48514$k40442$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40445, %struct.ScmObj* %args48514$k40442$0)
store volatile %struct.ScmObj* %args48514$k40442$1, %struct.ScmObj** %stackaddr$prim49555, align 8
%stackaddr$prim49556 = alloca %struct.ScmObj*, align 8
%args48514$k40442$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42830, %struct.ScmObj* %args48514$k40442$1)
store volatile %struct.ScmObj* %args48514$k40442$2, %struct.ScmObj** %stackaddr$prim49556, align 8
%clofunc49557 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40442)
musttail call tailcc void %clofunc49557(%struct.ScmObj* %k40442, %struct.ScmObj* %args48514$k40442$2)
ret void
falsebranch$cmp49553:
%ae42841 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49558 = alloca %struct.ScmObj*, align 8
%anf_45bind40333 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40188, %struct.ScmObj* %ae42841)
store volatile %struct.ScmObj* %anf_45bind40333, %struct.ScmObj** %stackaddr$prim49558, align 8
%stackaddr$prim49559 = alloca %struct.ScmObj*, align 8
%anf_45bind40334 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40333)
store volatile %struct.ScmObj* %anf_45bind40334, %struct.ScmObj** %stackaddr$prim49559, align 8
%ae42844 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49560 = alloca %struct.ScmObj*, align 8
%_95040193 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40188, %struct.ScmObj* %ae42844, %struct.ScmObj* %anf_45bind40334)
store volatile %struct.ScmObj* %_95040193, %struct.ScmObj** %stackaddr$prim49560, align 8
%ae42847 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49561 = alloca %struct.ScmObj*, align 8
%anf_45bind40335 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40189, %struct.ScmObj* %ae42847)
store volatile %struct.ScmObj* %anf_45bind40335, %struct.ScmObj** %stackaddr$prim49561, align 8
%ae42849 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim49562 = alloca %struct.ScmObj*, align 8
%anf_45bind40336 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind40335, %struct.ScmObj* %ae42849)
store volatile %struct.ScmObj* %anf_45bind40336, %struct.ScmObj** %stackaddr$prim49562, align 8
%ae42851 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49563 = alloca %struct.ScmObj*, align 8
%_95140192 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n40189, %struct.ScmObj* %ae42851, %struct.ScmObj* %anf_45bind40336)
store volatile %struct.ScmObj* %_95140192, %struct.ScmObj** %stackaddr$prim49563, align 8
%args48515$cc40190$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49564 = alloca %struct.ScmObj*, align 8
%args48515$cc40190$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40190, %struct.ScmObj* %args48515$cc40190$0)
store volatile %struct.ScmObj* %args48515$cc40190$1, %struct.ScmObj** %stackaddr$prim49564, align 8
%stackaddr$prim49565 = alloca %struct.ScmObj*, align 8
%args48515$cc40190$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40442, %struct.ScmObj* %args48515$cc40190$1)
store volatile %struct.ScmObj* %args48515$cc40190$2, %struct.ScmObj** %stackaddr$prim49565, align 8
%clofunc49566 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40190)
musttail call tailcc void %clofunc49566(%struct.ScmObj* %cc40190, %struct.ScmObj* %args48515$cc40190$2)
ret void
}

define tailcc void @proc_clo$ae42806(%struct.ScmObj* %env$ae42806,%struct.ScmObj* %current_45args48517) {
%stackaddr$prim49567 = alloca %struct.ScmObj*, align 8
%k40446 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48517)
store volatile %struct.ScmObj* %k40446, %struct.ScmObj** %stackaddr$prim49567, align 8
%stackaddr$prim49568 = alloca %struct.ScmObj*, align 8
%current_45args48518 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48517)
store volatile %struct.ScmObj* %current_45args48518, %struct.ScmObj** %stackaddr$prim49568, align 8
%stackaddr$prim49569 = alloca %struct.ScmObj*, align 8
%u40191 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48518)
store volatile %struct.ScmObj* %u40191, %struct.ScmObj** %stackaddr$prim49569, align 8
%args48520$u40191$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49570 = alloca %struct.ScmObj*, align 8
%args48520$u40191$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u40191, %struct.ScmObj* %args48520$u40191$0)
store volatile %struct.ScmObj* %args48520$u40191$1, %struct.ScmObj** %stackaddr$prim49570, align 8
%stackaddr$prim49571 = alloca %struct.ScmObj*, align 8
%args48520$u40191$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40446, %struct.ScmObj* %args48520$u40191$1)
store volatile %struct.ScmObj* %args48520$u40191$2, %struct.ScmObj** %stackaddr$prim49571, align 8
%clofunc49572 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u40191)
musttail call tailcc void %clofunc49572(%struct.ScmObj* %u40191, %struct.ScmObj* %args48520$u40191$2)
ret void
}

define tailcc void @proc_clo$ae42383(%struct.ScmObj* %env$ae42383,%struct.ScmObj* %current_45args48523) {
%stackaddr$prim49573 = alloca %struct.ScmObj*, align 8
%k40447 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48523)
store volatile %struct.ScmObj* %k40447, %struct.ScmObj** %stackaddr$prim49573, align 8
%stackaddr$prim49574 = alloca %struct.ScmObj*, align 8
%current_45args48524 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48523)
store volatile %struct.ScmObj* %current_45args48524, %struct.ScmObj** %stackaddr$prim49574, align 8
%stackaddr$prim49575 = alloca %struct.ScmObj*, align 8
%a40195 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48524)
store volatile %struct.ScmObj* %a40195, %struct.ScmObj** %stackaddr$prim49575, align 8
%ae42384 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim49576 = alloca %struct.ScmObj*, align 8
%a40196 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42384, %struct.ScmObj* %a40195)
store volatile %struct.ScmObj* %a40196, %struct.ScmObj** %stackaddr$prim49576, align 8
%stackaddr$makeclosure49577 = alloca %struct.ScmObj*, align 8
%fptrToInt49578 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42386 to i64
%ae42386 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49578)
store volatile %struct.ScmObj* %ae42386, %struct.ScmObj** %stackaddr$makeclosure49577, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42386, %struct.ScmObj* %a40196, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42386, %struct.ScmObj* %k40447, i64 1)
%ae42387 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49579 = alloca %struct.ScmObj*, align 8
%fptrToInt49580 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42388 to i64
%ae42388 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49580)
store volatile %struct.ScmObj* %ae42388, %struct.ScmObj** %stackaddr$makeclosure49579, align 8
%args48546$ae42386$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49581 = alloca %struct.ScmObj*, align 8
%args48546$ae42386$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42388, %struct.ScmObj* %args48546$ae42386$0)
store volatile %struct.ScmObj* %args48546$ae42386$1, %struct.ScmObj** %stackaddr$prim49581, align 8
%stackaddr$prim49582 = alloca %struct.ScmObj*, align 8
%args48546$ae42386$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42387, %struct.ScmObj* %args48546$ae42386$1)
store volatile %struct.ScmObj* %args48546$ae42386$2, %struct.ScmObj** %stackaddr$prim49582, align 8
%clofunc49583 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42386)
musttail call tailcc void %clofunc49583(%struct.ScmObj* %ae42386, %struct.ScmObj* %args48546$ae42386$2)
ret void
}

define tailcc void @proc_clo$ae42386(%struct.ScmObj* %env$ae42386,%struct.ScmObj* %current_45args48526) {
%stackaddr$env-ref49584 = alloca %struct.ScmObj*, align 8
%a40196 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42386, i64 0)
store %struct.ScmObj* %a40196, %struct.ScmObj** %stackaddr$env-ref49584
%stackaddr$env-ref49585 = alloca %struct.ScmObj*, align 8
%k40447 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42386, i64 1)
store %struct.ScmObj* %k40447, %struct.ScmObj** %stackaddr$env-ref49585
%stackaddr$prim49586 = alloca %struct.ScmObj*, align 8
%_95k40448 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48526)
store volatile %struct.ScmObj* %_95k40448, %struct.ScmObj** %stackaddr$prim49586, align 8
%stackaddr$prim49587 = alloca %struct.ScmObj*, align 8
%current_45args48527 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48526)
store volatile %struct.ScmObj* %current_45args48527, %struct.ScmObj** %stackaddr$prim49587, align 8
%stackaddr$prim49588 = alloca %struct.ScmObj*, align 8
%anf_45bind40322 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48527)
store volatile %struct.ScmObj* %anf_45bind40322, %struct.ScmObj** %stackaddr$prim49588, align 8
%stackaddr$makeclosure49589 = alloca %struct.ScmObj*, align 8
%fptrToInt49590 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42405 to i64
%ae42405 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49590)
store volatile %struct.ScmObj* %ae42405, %struct.ScmObj** %stackaddr$makeclosure49589, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42405, %struct.ScmObj* %a40196, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42405, %struct.ScmObj* %k40447, i64 1)
%stackaddr$makeclosure49591 = alloca %struct.ScmObj*, align 8
%fptrToInt49592 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42406 to i64
%ae42406 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49592)
store volatile %struct.ScmObj* %ae42406, %struct.ScmObj** %stackaddr$makeclosure49591, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42406, %struct.ScmObj* %a40196, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42406, %struct.ScmObj* %k40447, i64 1)
%args48541$anf_45bind40322$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49593 = alloca %struct.ScmObj*, align 8
%args48541$anf_45bind40322$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42406, %struct.ScmObj* %args48541$anf_45bind40322$0)
store volatile %struct.ScmObj* %args48541$anf_45bind40322$1, %struct.ScmObj** %stackaddr$prim49593, align 8
%stackaddr$prim49594 = alloca %struct.ScmObj*, align 8
%args48541$anf_45bind40322$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42405, %struct.ScmObj* %args48541$anf_45bind40322$1)
store volatile %struct.ScmObj* %args48541$anf_45bind40322$2, %struct.ScmObj** %stackaddr$prim49594, align 8
%clofunc49595 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40322)
musttail call tailcc void %clofunc49595(%struct.ScmObj* %anf_45bind40322, %struct.ScmObj* %args48541$anf_45bind40322$2)
ret void
}

define tailcc void @proc_clo$ae42405(%struct.ScmObj* %env$ae42405,%struct.ScmObj* %current_45args48529) {
%stackaddr$env-ref49596 = alloca %struct.ScmObj*, align 8
%a40196 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42405, i64 0)
store %struct.ScmObj* %a40196, %struct.ScmObj** %stackaddr$env-ref49596
%stackaddr$env-ref49597 = alloca %struct.ScmObj*, align 8
%k40447 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42405, i64 1)
store %struct.ScmObj* %k40447, %struct.ScmObj** %stackaddr$env-ref49597
%stackaddr$prim49598 = alloca %struct.ScmObj*, align 8
%_95k40449 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48529)
store volatile %struct.ScmObj* %_95k40449, %struct.ScmObj** %stackaddr$prim49598, align 8
%stackaddr$prim49599 = alloca %struct.ScmObj*, align 8
%current_45args48530 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48529)
store volatile %struct.ScmObj* %current_45args48530, %struct.ScmObj** %stackaddr$prim49599, align 8
%stackaddr$prim49600 = alloca %struct.ScmObj*, align 8
%cc40197 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48530)
store volatile %struct.ScmObj* %cc40197, %struct.ScmObj** %stackaddr$prim49600, align 8
%ae42521 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49601 = alloca %struct.ScmObj*, align 8
%anf_45bind40323 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40196, %struct.ScmObj* %ae42521)
store volatile %struct.ScmObj* %anf_45bind40323, %struct.ScmObj** %stackaddr$prim49601, align 8
%stackaddr$prim49602 = alloca %struct.ScmObj*, align 8
%anf_45bind40324 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40323)
store volatile %struct.ScmObj* %anf_45bind40324, %struct.ScmObj** %stackaddr$prim49602, align 8
%truthy$cmp49603 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40324)
%cmp$cmp49603 = icmp eq i64 %truthy$cmp49603, 1
br i1 %cmp$cmp49603, label %truebranch$cmp49603, label %falsebranch$cmp49603
truebranch$cmp49603:
%ae42525 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42526 = call %struct.ScmObj* @const_init_true()
%args48532$k40447$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49604 = alloca %struct.ScmObj*, align 8
%args48532$k40447$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42526, %struct.ScmObj* %args48532$k40447$0)
store volatile %struct.ScmObj* %args48532$k40447$1, %struct.ScmObj** %stackaddr$prim49604, align 8
%stackaddr$prim49605 = alloca %struct.ScmObj*, align 8
%args48532$k40447$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42525, %struct.ScmObj* %args48532$k40447$1)
store volatile %struct.ScmObj* %args48532$k40447$2, %struct.ScmObj** %stackaddr$prim49605, align 8
%clofunc49606 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40447)
musttail call tailcc void %clofunc49606(%struct.ScmObj* %k40447, %struct.ScmObj* %args48532$k40447$2)
ret void
falsebranch$cmp49603:
%ae42534 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49607 = alloca %struct.ScmObj*, align 8
%anf_45bind40325 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40196, %struct.ScmObj* %ae42534)
store volatile %struct.ScmObj* %anf_45bind40325, %struct.ScmObj** %stackaddr$prim49607, align 8
%stackaddr$prim49608 = alloca %struct.ScmObj*, align 8
%anf_45bind40326 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind40325)
store volatile %struct.ScmObj* %anf_45bind40326, %struct.ScmObj** %stackaddr$prim49608, align 8
%truthy$cmp49609 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40326)
%cmp$cmp49609 = icmp eq i64 %truthy$cmp49609, 1
br i1 %cmp$cmp49609, label %truebranch$cmp49609, label %falsebranch$cmp49609
truebranch$cmp49609:
%ae42538 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49610 = alloca %struct.ScmObj*, align 8
%anf_45bind40327 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40196, %struct.ScmObj* %ae42538)
store volatile %struct.ScmObj* %anf_45bind40327, %struct.ScmObj** %stackaddr$prim49610, align 8
%stackaddr$prim49611 = alloca %struct.ScmObj*, align 8
%b40199 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40327)
store volatile %struct.ScmObj* %b40199, %struct.ScmObj** %stackaddr$prim49611, align 8
%ae42541 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49612 = alloca %struct.ScmObj*, align 8
%anf_45bind40328 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40196, %struct.ScmObj* %ae42541)
store volatile %struct.ScmObj* %anf_45bind40328, %struct.ScmObj** %stackaddr$prim49612, align 8
%stackaddr$prim49613 = alloca %struct.ScmObj*, align 8
%anf_45bind40329 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40328)
store volatile %struct.ScmObj* %anf_45bind40329, %struct.ScmObj** %stackaddr$prim49613, align 8
%ae42544 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49614 = alloca %struct.ScmObj*, align 8
%_95040200 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a40196, %struct.ScmObj* %ae42544, %struct.ScmObj* %anf_45bind40329)
store volatile %struct.ScmObj* %_95040200, %struct.ScmObj** %stackaddr$prim49614, align 8
%args48533$cc40197$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49615 = alloca %struct.ScmObj*, align 8
%args48533$cc40197$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40197, %struct.ScmObj* %args48533$cc40197$0)
store volatile %struct.ScmObj* %args48533$cc40197$1, %struct.ScmObj** %stackaddr$prim49615, align 8
%stackaddr$prim49616 = alloca %struct.ScmObj*, align 8
%args48533$cc40197$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40447, %struct.ScmObj* %args48533$cc40197$1)
store volatile %struct.ScmObj* %args48533$cc40197$2, %struct.ScmObj** %stackaddr$prim49616, align 8
%clofunc49617 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40197)
musttail call tailcc void %clofunc49617(%struct.ScmObj* %cc40197, %struct.ScmObj* %args48533$cc40197$2)
ret void
falsebranch$cmp49609:
%ae42577 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42578 = call %struct.ScmObj* @const_init_false()
%args48534$k40447$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49618 = alloca %struct.ScmObj*, align 8
%args48534$k40447$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42578, %struct.ScmObj* %args48534$k40447$0)
store volatile %struct.ScmObj* %args48534$k40447$1, %struct.ScmObj** %stackaddr$prim49618, align 8
%stackaddr$prim49619 = alloca %struct.ScmObj*, align 8
%args48534$k40447$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42577, %struct.ScmObj* %args48534$k40447$1)
store volatile %struct.ScmObj* %args48534$k40447$2, %struct.ScmObj** %stackaddr$prim49619, align 8
%clofunc49620 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40447)
musttail call tailcc void %clofunc49620(%struct.ScmObj* %k40447, %struct.ScmObj* %args48534$k40447$2)
ret void
}

define tailcc void @proc_clo$ae42406(%struct.ScmObj* %env$ae42406,%struct.ScmObj* %current_45args48535) {
%stackaddr$env-ref49621 = alloca %struct.ScmObj*, align 8
%a40196 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42406, i64 0)
store %struct.ScmObj* %a40196, %struct.ScmObj** %stackaddr$env-ref49621
%stackaddr$env-ref49622 = alloca %struct.ScmObj*, align 8
%k40447 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42406, i64 1)
store %struct.ScmObj* %k40447, %struct.ScmObj** %stackaddr$env-ref49622
%stackaddr$prim49623 = alloca %struct.ScmObj*, align 8
%_95k40449 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48535)
store volatile %struct.ScmObj* %_95k40449, %struct.ScmObj** %stackaddr$prim49623, align 8
%stackaddr$prim49624 = alloca %struct.ScmObj*, align 8
%current_45args48536 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48535)
store volatile %struct.ScmObj* %current_45args48536, %struct.ScmObj** %stackaddr$prim49624, align 8
%stackaddr$prim49625 = alloca %struct.ScmObj*, align 8
%cc40197 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48536)
store volatile %struct.ScmObj* %cc40197, %struct.ScmObj** %stackaddr$prim49625, align 8
%ae42408 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49626 = alloca %struct.ScmObj*, align 8
%anf_45bind40323 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40196, %struct.ScmObj* %ae42408)
store volatile %struct.ScmObj* %anf_45bind40323, %struct.ScmObj** %stackaddr$prim49626, align 8
%stackaddr$prim49627 = alloca %struct.ScmObj*, align 8
%anf_45bind40324 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40323)
store volatile %struct.ScmObj* %anf_45bind40324, %struct.ScmObj** %stackaddr$prim49627, align 8
%truthy$cmp49628 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40324)
%cmp$cmp49628 = icmp eq i64 %truthy$cmp49628, 1
br i1 %cmp$cmp49628, label %truebranch$cmp49628, label %falsebranch$cmp49628
truebranch$cmp49628:
%ae42412 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42413 = call %struct.ScmObj* @const_init_true()
%args48538$k40447$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49629 = alloca %struct.ScmObj*, align 8
%args48538$k40447$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42413, %struct.ScmObj* %args48538$k40447$0)
store volatile %struct.ScmObj* %args48538$k40447$1, %struct.ScmObj** %stackaddr$prim49629, align 8
%stackaddr$prim49630 = alloca %struct.ScmObj*, align 8
%args48538$k40447$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42412, %struct.ScmObj* %args48538$k40447$1)
store volatile %struct.ScmObj* %args48538$k40447$2, %struct.ScmObj** %stackaddr$prim49630, align 8
%clofunc49631 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40447)
musttail call tailcc void %clofunc49631(%struct.ScmObj* %k40447, %struct.ScmObj* %args48538$k40447$2)
ret void
falsebranch$cmp49628:
%ae42421 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49632 = alloca %struct.ScmObj*, align 8
%anf_45bind40325 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40196, %struct.ScmObj* %ae42421)
store volatile %struct.ScmObj* %anf_45bind40325, %struct.ScmObj** %stackaddr$prim49632, align 8
%stackaddr$prim49633 = alloca %struct.ScmObj*, align 8
%anf_45bind40326 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind40325)
store volatile %struct.ScmObj* %anf_45bind40326, %struct.ScmObj** %stackaddr$prim49633, align 8
%truthy$cmp49634 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40326)
%cmp$cmp49634 = icmp eq i64 %truthy$cmp49634, 1
br i1 %cmp$cmp49634, label %truebranch$cmp49634, label %falsebranch$cmp49634
truebranch$cmp49634:
%ae42425 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49635 = alloca %struct.ScmObj*, align 8
%anf_45bind40327 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40196, %struct.ScmObj* %ae42425)
store volatile %struct.ScmObj* %anf_45bind40327, %struct.ScmObj** %stackaddr$prim49635, align 8
%stackaddr$prim49636 = alloca %struct.ScmObj*, align 8
%b40199 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40327)
store volatile %struct.ScmObj* %b40199, %struct.ScmObj** %stackaddr$prim49636, align 8
%ae42428 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49637 = alloca %struct.ScmObj*, align 8
%anf_45bind40328 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40196, %struct.ScmObj* %ae42428)
store volatile %struct.ScmObj* %anf_45bind40328, %struct.ScmObj** %stackaddr$prim49637, align 8
%stackaddr$prim49638 = alloca %struct.ScmObj*, align 8
%anf_45bind40329 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40328)
store volatile %struct.ScmObj* %anf_45bind40329, %struct.ScmObj** %stackaddr$prim49638, align 8
%ae42431 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49639 = alloca %struct.ScmObj*, align 8
%_95040200 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a40196, %struct.ScmObj* %ae42431, %struct.ScmObj* %anf_45bind40329)
store volatile %struct.ScmObj* %_95040200, %struct.ScmObj** %stackaddr$prim49639, align 8
%args48539$cc40197$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49640 = alloca %struct.ScmObj*, align 8
%args48539$cc40197$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40197, %struct.ScmObj* %args48539$cc40197$0)
store volatile %struct.ScmObj* %args48539$cc40197$1, %struct.ScmObj** %stackaddr$prim49640, align 8
%stackaddr$prim49641 = alloca %struct.ScmObj*, align 8
%args48539$cc40197$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40447, %struct.ScmObj* %args48539$cc40197$1)
store volatile %struct.ScmObj* %args48539$cc40197$2, %struct.ScmObj** %stackaddr$prim49641, align 8
%clofunc49642 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40197)
musttail call tailcc void %clofunc49642(%struct.ScmObj* %cc40197, %struct.ScmObj* %args48539$cc40197$2)
ret void
falsebranch$cmp49634:
%ae42464 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42465 = call %struct.ScmObj* @const_init_false()
%args48540$k40447$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49643 = alloca %struct.ScmObj*, align 8
%args48540$k40447$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42465, %struct.ScmObj* %args48540$k40447$0)
store volatile %struct.ScmObj* %args48540$k40447$1, %struct.ScmObj** %stackaddr$prim49643, align 8
%stackaddr$prim49644 = alloca %struct.ScmObj*, align 8
%args48540$k40447$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42464, %struct.ScmObj* %args48540$k40447$1)
store volatile %struct.ScmObj* %args48540$k40447$2, %struct.ScmObj** %stackaddr$prim49644, align 8
%clofunc49645 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40447)
musttail call tailcc void %clofunc49645(%struct.ScmObj* %k40447, %struct.ScmObj* %args48540$k40447$2)
ret void
}

define tailcc void @proc_clo$ae42388(%struct.ScmObj* %env$ae42388,%struct.ScmObj* %current_45args48542) {
%stackaddr$prim49646 = alloca %struct.ScmObj*, align 8
%k40450 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48542)
store volatile %struct.ScmObj* %k40450, %struct.ScmObj** %stackaddr$prim49646, align 8
%stackaddr$prim49647 = alloca %struct.ScmObj*, align 8
%current_45args48543 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48542)
store volatile %struct.ScmObj* %current_45args48543, %struct.ScmObj** %stackaddr$prim49647, align 8
%stackaddr$prim49648 = alloca %struct.ScmObj*, align 8
%k40198 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48543)
store volatile %struct.ScmObj* %k40198, %struct.ScmObj** %stackaddr$prim49648, align 8
%ae42390 = call %struct.ScmObj* @const_init_int(i64 0)
%args48545$k40450$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49649 = alloca %struct.ScmObj*, align 8
%args48545$k40450$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40198, %struct.ScmObj* %args48545$k40450$0)
store volatile %struct.ScmObj* %args48545$k40450$1, %struct.ScmObj** %stackaddr$prim49649, align 8
%stackaddr$prim49650 = alloca %struct.ScmObj*, align 8
%args48545$k40450$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42390, %struct.ScmObj* %args48545$k40450$1)
store volatile %struct.ScmObj* %args48545$k40450$2, %struct.ScmObj** %stackaddr$prim49650, align 8
%clofunc49651 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40450)
musttail call tailcc void %clofunc49651(%struct.ScmObj* %k40450, %struct.ScmObj* %args48545$k40450$2)
ret void
}

define tailcc void @proc_clo$ae42311(%struct.ScmObj* %env$ae42311,%struct.ScmObj* %current_45args48548) {
%stackaddr$env-ref49652 = alloca %struct.ScmObj*, align 8
%_37append40202 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42311, i64 0)
store %struct.ScmObj* %_37append40202, %struct.ScmObj** %stackaddr$env-ref49652
%stackaddr$prim49653 = alloca %struct.ScmObj*, align 8
%k40451 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48548)
store volatile %struct.ScmObj* %k40451, %struct.ScmObj** %stackaddr$prim49653, align 8
%stackaddr$prim49654 = alloca %struct.ScmObj*, align 8
%current_45args48549 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48548)
store volatile %struct.ScmObj* %current_45args48549, %struct.ScmObj** %stackaddr$prim49654, align 8
%stackaddr$prim49655 = alloca %struct.ScmObj*, align 8
%ls040205 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48549)
store volatile %struct.ScmObj* %ls040205, %struct.ScmObj** %stackaddr$prim49655, align 8
%stackaddr$prim49656 = alloca %struct.ScmObj*, align 8
%current_45args48550 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48549)
store volatile %struct.ScmObj* %current_45args48550, %struct.ScmObj** %stackaddr$prim49656, align 8
%stackaddr$prim49657 = alloca %struct.ScmObj*, align 8
%ls140204 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48550)
store volatile %struct.ScmObj* %ls140204, %struct.ScmObj** %stackaddr$prim49657, align 8
%stackaddr$prim49658 = alloca %struct.ScmObj*, align 8
%anf_45bind40316 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls040205)
store volatile %struct.ScmObj* %anf_45bind40316, %struct.ScmObj** %stackaddr$prim49658, align 8
%truthy$cmp49659 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40316)
%cmp$cmp49659 = icmp eq i64 %truthy$cmp49659, 1
br i1 %cmp$cmp49659, label %truebranch$cmp49659, label %falsebranch$cmp49659
truebranch$cmp49659:
%ae42315 = call %struct.ScmObj* @const_init_int(i64 0)
%args48552$k40451$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49660 = alloca %struct.ScmObj*, align 8
%args48552$k40451$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls140204, %struct.ScmObj* %args48552$k40451$0)
store volatile %struct.ScmObj* %args48552$k40451$1, %struct.ScmObj** %stackaddr$prim49660, align 8
%stackaddr$prim49661 = alloca %struct.ScmObj*, align 8
%args48552$k40451$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42315, %struct.ScmObj* %args48552$k40451$1)
store volatile %struct.ScmObj* %args48552$k40451$2, %struct.ScmObj** %stackaddr$prim49661, align 8
%clofunc49662 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40451)
musttail call tailcc void %clofunc49662(%struct.ScmObj* %k40451, %struct.ScmObj* %args48552$k40451$2)
ret void
falsebranch$cmp49659:
%stackaddr$prim49663 = alloca %struct.ScmObj*, align 8
%anf_45bind40317 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls040205)
store volatile %struct.ScmObj* %anf_45bind40317, %struct.ScmObj** %stackaddr$prim49663, align 8
%ae42322 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49664 = alloca %struct.ScmObj*, align 8
%anf_45bind40318 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append40202, %struct.ScmObj* %ae42322)
store volatile %struct.ScmObj* %anf_45bind40318, %struct.ScmObj** %stackaddr$prim49664, align 8
%stackaddr$prim49665 = alloca %struct.ScmObj*, align 8
%anf_45bind40319 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls040205)
store volatile %struct.ScmObj* %anf_45bind40319, %struct.ScmObj** %stackaddr$prim49665, align 8
%stackaddr$makeclosure49666 = alloca %struct.ScmObj*, align 8
%fptrToInt49667 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42325 to i64
%ae42325 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49667)
store volatile %struct.ScmObj* %ae42325, %struct.ScmObj** %stackaddr$makeclosure49666, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42325, %struct.ScmObj* %k40451, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42325, %struct.ScmObj* %anf_45bind40317, i64 1)
%args48557$anf_45bind40318$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49668 = alloca %struct.ScmObj*, align 8
%args48557$anf_45bind40318$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls140204, %struct.ScmObj* %args48557$anf_45bind40318$0)
store volatile %struct.ScmObj* %args48557$anf_45bind40318$1, %struct.ScmObj** %stackaddr$prim49668, align 8
%stackaddr$prim49669 = alloca %struct.ScmObj*, align 8
%args48557$anf_45bind40318$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40319, %struct.ScmObj* %args48557$anf_45bind40318$1)
store volatile %struct.ScmObj* %args48557$anf_45bind40318$2, %struct.ScmObj** %stackaddr$prim49669, align 8
%stackaddr$prim49670 = alloca %struct.ScmObj*, align 8
%args48557$anf_45bind40318$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42325, %struct.ScmObj* %args48557$anf_45bind40318$2)
store volatile %struct.ScmObj* %args48557$anf_45bind40318$3, %struct.ScmObj** %stackaddr$prim49670, align 8
%clofunc49671 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40318)
musttail call tailcc void %clofunc49671(%struct.ScmObj* %anf_45bind40318, %struct.ScmObj* %args48557$anf_45bind40318$3)
ret void
}

define tailcc void @proc_clo$ae42325(%struct.ScmObj* %env$ae42325,%struct.ScmObj* %current_45args48553) {
%stackaddr$env-ref49672 = alloca %struct.ScmObj*, align 8
%k40451 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42325, i64 0)
store %struct.ScmObj* %k40451, %struct.ScmObj** %stackaddr$env-ref49672
%stackaddr$env-ref49673 = alloca %struct.ScmObj*, align 8
%anf_45bind40317 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42325, i64 1)
store %struct.ScmObj* %anf_45bind40317, %struct.ScmObj** %stackaddr$env-ref49673
%stackaddr$prim49674 = alloca %struct.ScmObj*, align 8
%_95k40452 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48553)
store volatile %struct.ScmObj* %_95k40452, %struct.ScmObj** %stackaddr$prim49674, align 8
%stackaddr$prim49675 = alloca %struct.ScmObj*, align 8
%current_45args48554 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48553)
store volatile %struct.ScmObj* %current_45args48554, %struct.ScmObj** %stackaddr$prim49675, align 8
%stackaddr$prim49676 = alloca %struct.ScmObj*, align 8
%anf_45bind40320 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48554)
store volatile %struct.ScmObj* %anf_45bind40320, %struct.ScmObj** %stackaddr$prim49676, align 8
%stackaddr$prim49677 = alloca %struct.ScmObj*, align 8
%cpsprim40453 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40317, %struct.ScmObj* %anf_45bind40320)
store volatile %struct.ScmObj* %cpsprim40453, %struct.ScmObj** %stackaddr$prim49677, align 8
%ae42331 = call %struct.ScmObj* @const_init_int(i64 0)
%args48556$k40451$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49678 = alloca %struct.ScmObj*, align 8
%args48556$k40451$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40453, %struct.ScmObj* %args48556$k40451$0)
store volatile %struct.ScmObj* %args48556$k40451$1, %struct.ScmObj** %stackaddr$prim49678, align 8
%stackaddr$prim49679 = alloca %struct.ScmObj*, align 8
%args48556$k40451$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42331, %struct.ScmObj* %args48556$k40451$1)
store volatile %struct.ScmObj* %args48556$k40451$2, %struct.ScmObj** %stackaddr$prim49679, align 8
%clofunc49680 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40451)
musttail call tailcc void %clofunc49680(%struct.ScmObj* %k40451, %struct.ScmObj* %args48556$k40451$2)
ret void
}

define tailcc void @proc_clo$ae42285(%struct.ScmObj* %env$ae42285,%struct.ScmObj* %current_45args48559) {
%stackaddr$prim49681 = alloca %struct.ScmObj*, align 8
%k40454 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48559)
store volatile %struct.ScmObj* %k40454, %struct.ScmObj** %stackaddr$prim49681, align 8
%stackaddr$prim49682 = alloca %struct.ScmObj*, align 8
%current_45args48560 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48559)
store volatile %struct.ScmObj* %current_45args48560, %struct.ScmObj** %stackaddr$prim49682, align 8
%stackaddr$prim49683 = alloca %struct.ScmObj*, align 8
%a40208 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48560)
store volatile %struct.ScmObj* %a40208, %struct.ScmObj** %stackaddr$prim49683, align 8
%stackaddr$prim49684 = alloca %struct.ScmObj*, align 8
%current_45args48561 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48560)
store volatile %struct.ScmObj* %current_45args48561, %struct.ScmObj** %stackaddr$prim49684, align 8
%stackaddr$prim49685 = alloca %struct.ScmObj*, align 8
%b40207 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48561)
store volatile %struct.ScmObj* %b40207, %struct.ScmObj** %stackaddr$prim49685, align 8
%stackaddr$prim49686 = alloca %struct.ScmObj*, align 8
%anf_45bind40315 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a40208, %struct.ScmObj* %b40207)
store volatile %struct.ScmObj* %anf_45bind40315, %struct.ScmObj** %stackaddr$prim49686, align 8
%stackaddr$prim49687 = alloca %struct.ScmObj*, align 8
%cpsprim40455 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind40315)
store volatile %struct.ScmObj* %cpsprim40455, %struct.ScmObj** %stackaddr$prim49687, align 8
%ae42290 = call %struct.ScmObj* @const_init_int(i64 0)
%args48563$k40454$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49688 = alloca %struct.ScmObj*, align 8
%args48563$k40454$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40455, %struct.ScmObj* %args48563$k40454$0)
store volatile %struct.ScmObj* %args48563$k40454$1, %struct.ScmObj** %stackaddr$prim49688, align 8
%stackaddr$prim49689 = alloca %struct.ScmObj*, align 8
%args48563$k40454$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42290, %struct.ScmObj* %args48563$k40454$1)
store volatile %struct.ScmObj* %args48563$k40454$2, %struct.ScmObj** %stackaddr$prim49689, align 8
%clofunc49690 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40454)
musttail call tailcc void %clofunc49690(%struct.ScmObj* %k40454, %struct.ScmObj* %args48563$k40454$2)
ret void
}

define tailcc void @proc_clo$ae42261(%struct.ScmObj* %env$ae42261,%struct.ScmObj* %current_45args48565) {
%stackaddr$prim49691 = alloca %struct.ScmObj*, align 8
%k40456 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48565)
store volatile %struct.ScmObj* %k40456, %struct.ScmObj** %stackaddr$prim49691, align 8
%stackaddr$prim49692 = alloca %struct.ScmObj*, align 8
%current_45args48566 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48565)
store volatile %struct.ScmObj* %current_45args48566, %struct.ScmObj** %stackaddr$prim49692, align 8
%stackaddr$prim49693 = alloca %struct.ScmObj*, align 8
%a40211 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48566)
store volatile %struct.ScmObj* %a40211, %struct.ScmObj** %stackaddr$prim49693, align 8
%stackaddr$prim49694 = alloca %struct.ScmObj*, align 8
%current_45args48567 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48566)
store volatile %struct.ScmObj* %current_45args48567, %struct.ScmObj** %stackaddr$prim49694, align 8
%stackaddr$prim49695 = alloca %struct.ScmObj*, align 8
%b40210 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48567)
store volatile %struct.ScmObj* %b40210, %struct.ScmObj** %stackaddr$prim49695, align 8
%stackaddr$prim49696 = alloca %struct.ScmObj*, align 8
%anf_45bind40314 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a40211, %struct.ScmObj* %b40210)
store volatile %struct.ScmObj* %anf_45bind40314, %struct.ScmObj** %stackaddr$prim49696, align 8
%stackaddr$prim49697 = alloca %struct.ScmObj*, align 8
%cpsprim40457 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind40314)
store volatile %struct.ScmObj* %cpsprim40457, %struct.ScmObj** %stackaddr$prim49697, align 8
%ae42266 = call %struct.ScmObj* @const_init_int(i64 0)
%args48569$k40456$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49698 = alloca %struct.ScmObj*, align 8
%args48569$k40456$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40457, %struct.ScmObj* %args48569$k40456$0)
store volatile %struct.ScmObj* %args48569$k40456$1, %struct.ScmObj** %stackaddr$prim49698, align 8
%stackaddr$prim49699 = alloca %struct.ScmObj*, align 8
%args48569$k40456$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42266, %struct.ScmObj* %args48569$k40456$1)
store volatile %struct.ScmObj* %args48569$k40456$2, %struct.ScmObj** %stackaddr$prim49699, align 8
%clofunc49700 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40456)
musttail call tailcc void %clofunc49700(%struct.ScmObj* %k40456, %struct.ScmObj* %args48569$k40456$2)
ret void
}

define tailcc void @proc_clo$ae41867(%struct.ScmObj* %env$ae41867,%struct.ScmObj* %current_45args48572) {
%stackaddr$env-ref49701 = alloca %struct.ScmObj*, align 8
%_37foldr40135 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41867, i64 0)
store %struct.ScmObj* %_37foldr40135, %struct.ScmObj** %stackaddr$env-ref49701
%stackaddr$env-ref49702 = alloca %struct.ScmObj*, align 8
%_37foldr140130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41867, i64 1)
store %struct.ScmObj* %_37foldr140130, %struct.ScmObj** %stackaddr$env-ref49702
%stackaddr$env-ref49703 = alloca %struct.ScmObj*, align 8
%_37map140161 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41867, i64 2)
store %struct.ScmObj* %_37map140161, %struct.ScmObj** %stackaddr$env-ref49703
%stackaddr$prim49704 = alloca %struct.ScmObj*, align 8
%k40458 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48572)
store volatile %struct.ScmObj* %k40458, %struct.ScmObj** %stackaddr$prim49704, align 8
%stackaddr$prim49705 = alloca %struct.ScmObj*, align 8
%current_45args48573 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48572)
store volatile %struct.ScmObj* %current_45args48573, %struct.ScmObj** %stackaddr$prim49705, align 8
%stackaddr$prim49706 = alloca %struct.ScmObj*, align 8
%_37foldl40213 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48573)
store volatile %struct.ScmObj* %_37foldl40213, %struct.ScmObj** %stackaddr$prim49706, align 8
%ae41869 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49707 = alloca %struct.ScmObj*, align 8
%fptrToInt49708 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41870 to i64
%ae41870 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt49708)
store volatile %struct.ScmObj* %ae41870, %struct.ScmObj** %stackaddr$makeclosure49707, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41870, %struct.ScmObj* %_37foldr40135, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41870, %struct.ScmObj* %_37foldl40213, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41870, %struct.ScmObj* %_37foldr140130, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41870, %struct.ScmObj* %_37map140161, i64 3)
%args48630$k40458$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49709 = alloca %struct.ScmObj*, align 8
%args48630$k40458$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41870, %struct.ScmObj* %args48630$k40458$0)
store volatile %struct.ScmObj* %args48630$k40458$1, %struct.ScmObj** %stackaddr$prim49709, align 8
%stackaddr$prim49710 = alloca %struct.ScmObj*, align 8
%args48630$k40458$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41869, %struct.ScmObj* %args48630$k40458$1)
store volatile %struct.ScmObj* %args48630$k40458$2, %struct.ScmObj** %stackaddr$prim49710, align 8
%clofunc49711 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40458)
musttail call tailcc void %clofunc49711(%struct.ScmObj* %k40458, %struct.ScmObj* %args48630$k40458$2)
ret void
}

define tailcc void @proc_clo$ae41870(%struct.ScmObj* %env$ae41870,%struct.ScmObj* %args4021440459) {
%stackaddr$env-ref49712 = alloca %struct.ScmObj*, align 8
%_37foldr40135 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41870, i64 0)
store %struct.ScmObj* %_37foldr40135, %struct.ScmObj** %stackaddr$env-ref49712
%stackaddr$env-ref49713 = alloca %struct.ScmObj*, align 8
%_37foldl40213 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41870, i64 1)
store %struct.ScmObj* %_37foldl40213, %struct.ScmObj** %stackaddr$env-ref49713
%stackaddr$env-ref49714 = alloca %struct.ScmObj*, align 8
%_37foldr140130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41870, i64 2)
store %struct.ScmObj* %_37foldr140130, %struct.ScmObj** %stackaddr$env-ref49714
%stackaddr$env-ref49715 = alloca %struct.ScmObj*, align 8
%_37map140161 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41870, i64 3)
store %struct.ScmObj* %_37map140161, %struct.ScmObj** %stackaddr$env-ref49715
%stackaddr$prim49716 = alloca %struct.ScmObj*, align 8
%k40460 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4021440459)
store volatile %struct.ScmObj* %k40460, %struct.ScmObj** %stackaddr$prim49716, align 8
%stackaddr$prim49717 = alloca %struct.ScmObj*, align 8
%args40214 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4021440459)
store volatile %struct.ScmObj* %args40214, %struct.ScmObj** %stackaddr$prim49717, align 8
%stackaddr$prim49718 = alloca %struct.ScmObj*, align 8
%f40217 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40214)
store volatile %struct.ScmObj* %f40217, %struct.ScmObj** %stackaddr$prim49718, align 8
%stackaddr$prim49719 = alloca %struct.ScmObj*, align 8
%anf_45bind40302 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40214)
store volatile %struct.ScmObj* %anf_45bind40302, %struct.ScmObj** %stackaddr$prim49719, align 8
%stackaddr$prim49720 = alloca %struct.ScmObj*, align 8
%acc40216 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40302)
store volatile %struct.ScmObj* %acc40216, %struct.ScmObj** %stackaddr$prim49720, align 8
%stackaddr$prim49721 = alloca %struct.ScmObj*, align 8
%anf_45bind40303 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40214)
store volatile %struct.ScmObj* %anf_45bind40303, %struct.ScmObj** %stackaddr$prim49721, align 8
%stackaddr$prim49722 = alloca %struct.ScmObj*, align 8
%lsts40215 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40303)
store volatile %struct.ScmObj* %lsts40215, %struct.ScmObj** %stackaddr$prim49722, align 8
%stackaddr$makeclosure49723 = alloca %struct.ScmObj*, align 8
%fptrToInt49724 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41878 to i64
%ae41878 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt49724)
store volatile %struct.ScmObj* %ae41878, %struct.ScmObj** %stackaddr$makeclosure49723, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41878, %struct.ScmObj* %lsts40215, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41878, %struct.ScmObj* %_37foldr40135, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41878, %struct.ScmObj* %f40217, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41878, %struct.ScmObj* %acc40216, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41878, %struct.ScmObj* %_37foldl40213, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41878, %struct.ScmObj* %_37foldr140130, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41878, %struct.ScmObj* %_37map140161, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae41878, %struct.ScmObj* %k40460, i64 7)
%ae41879 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49725 = alloca %struct.ScmObj*, align 8
%fptrToInt49726 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41880 to i64
%ae41880 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49726)
store volatile %struct.ScmObj* %ae41880, %struct.ScmObj** %stackaddr$makeclosure49725, align 8
%args48629$ae41878$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49727 = alloca %struct.ScmObj*, align 8
%args48629$ae41878$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41880, %struct.ScmObj* %args48629$ae41878$0)
store volatile %struct.ScmObj* %args48629$ae41878$1, %struct.ScmObj** %stackaddr$prim49727, align 8
%stackaddr$prim49728 = alloca %struct.ScmObj*, align 8
%args48629$ae41878$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41879, %struct.ScmObj* %args48629$ae41878$1)
store volatile %struct.ScmObj* %args48629$ae41878$2, %struct.ScmObj** %stackaddr$prim49728, align 8
%clofunc49729 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41878)
musttail call tailcc void %clofunc49729(%struct.ScmObj* %ae41878, %struct.ScmObj* %args48629$ae41878$2)
ret void
}

define tailcc void @proc_clo$ae41878(%struct.ScmObj* %env$ae41878,%struct.ScmObj* %current_45args48575) {
%stackaddr$env-ref49730 = alloca %struct.ScmObj*, align 8
%lsts40215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41878, i64 0)
store %struct.ScmObj* %lsts40215, %struct.ScmObj** %stackaddr$env-ref49730
%stackaddr$env-ref49731 = alloca %struct.ScmObj*, align 8
%_37foldr40135 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41878, i64 1)
store %struct.ScmObj* %_37foldr40135, %struct.ScmObj** %stackaddr$env-ref49731
%stackaddr$env-ref49732 = alloca %struct.ScmObj*, align 8
%f40217 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41878, i64 2)
store %struct.ScmObj* %f40217, %struct.ScmObj** %stackaddr$env-ref49732
%stackaddr$env-ref49733 = alloca %struct.ScmObj*, align 8
%acc40216 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41878, i64 3)
store %struct.ScmObj* %acc40216, %struct.ScmObj** %stackaddr$env-ref49733
%stackaddr$env-ref49734 = alloca %struct.ScmObj*, align 8
%_37foldl40213 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41878, i64 4)
store %struct.ScmObj* %_37foldl40213, %struct.ScmObj** %stackaddr$env-ref49734
%stackaddr$env-ref49735 = alloca %struct.ScmObj*, align 8
%_37foldr140130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41878, i64 5)
store %struct.ScmObj* %_37foldr140130, %struct.ScmObj** %stackaddr$env-ref49735
%stackaddr$env-ref49736 = alloca %struct.ScmObj*, align 8
%_37map140161 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41878, i64 6)
store %struct.ScmObj* %_37map140161, %struct.ScmObj** %stackaddr$env-ref49736
%stackaddr$env-ref49737 = alloca %struct.ScmObj*, align 8
%k40460 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41878, i64 7)
store %struct.ScmObj* %k40460, %struct.ScmObj** %stackaddr$env-ref49737
%stackaddr$prim49738 = alloca %struct.ScmObj*, align 8
%_95k40461 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48575)
store volatile %struct.ScmObj* %_95k40461, %struct.ScmObj** %stackaddr$prim49738, align 8
%stackaddr$prim49739 = alloca %struct.ScmObj*, align 8
%current_45args48576 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48575)
store volatile %struct.ScmObj* %current_45args48576, %struct.ScmObj** %stackaddr$prim49739, align 8
%stackaddr$prim49740 = alloca %struct.ScmObj*, align 8
%anf_45bind40304 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48576)
store volatile %struct.ScmObj* %anf_45bind40304, %struct.ScmObj** %stackaddr$prim49740, align 8
%stackaddr$makeclosure49741 = alloca %struct.ScmObj*, align 8
%fptrToInt49742 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41910 to i64
%ae41910 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt49742)
store volatile %struct.ScmObj* %ae41910, %struct.ScmObj** %stackaddr$makeclosure49741, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41910, %struct.ScmObj* %lsts40215, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41910, %struct.ScmObj* %_37foldr40135, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41910, %struct.ScmObj* %f40217, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41910, %struct.ScmObj* %acc40216, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41910, %struct.ScmObj* %_37foldl40213, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41910, %struct.ScmObj* %_37map140161, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41910, %struct.ScmObj* %k40460, i64 6)
%ae41912 = call %struct.ScmObj* @const_init_false()
%args48622$_37foldr140130$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49743 = alloca %struct.ScmObj*, align 8
%args48622$_37foldr140130$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40215, %struct.ScmObj* %args48622$_37foldr140130$0)
store volatile %struct.ScmObj* %args48622$_37foldr140130$1, %struct.ScmObj** %stackaddr$prim49743, align 8
%stackaddr$prim49744 = alloca %struct.ScmObj*, align 8
%args48622$_37foldr140130$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41912, %struct.ScmObj* %args48622$_37foldr140130$1)
store volatile %struct.ScmObj* %args48622$_37foldr140130$2, %struct.ScmObj** %stackaddr$prim49744, align 8
%stackaddr$prim49745 = alloca %struct.ScmObj*, align 8
%args48622$_37foldr140130$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40304, %struct.ScmObj* %args48622$_37foldr140130$2)
store volatile %struct.ScmObj* %args48622$_37foldr140130$3, %struct.ScmObj** %stackaddr$prim49745, align 8
%stackaddr$prim49746 = alloca %struct.ScmObj*, align 8
%args48622$_37foldr140130$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41910, %struct.ScmObj* %args48622$_37foldr140130$3)
store volatile %struct.ScmObj* %args48622$_37foldr140130$4, %struct.ScmObj** %stackaddr$prim49746, align 8
%clofunc49747 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140130)
musttail call tailcc void %clofunc49747(%struct.ScmObj* %_37foldr140130, %struct.ScmObj* %args48622$_37foldr140130$4)
ret void
}

define tailcc void @proc_clo$ae41910(%struct.ScmObj* %env$ae41910,%struct.ScmObj* %current_45args48578) {
%stackaddr$env-ref49748 = alloca %struct.ScmObj*, align 8
%lsts40215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41910, i64 0)
store %struct.ScmObj* %lsts40215, %struct.ScmObj** %stackaddr$env-ref49748
%stackaddr$env-ref49749 = alloca %struct.ScmObj*, align 8
%_37foldr40135 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41910, i64 1)
store %struct.ScmObj* %_37foldr40135, %struct.ScmObj** %stackaddr$env-ref49749
%stackaddr$env-ref49750 = alloca %struct.ScmObj*, align 8
%f40217 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41910, i64 2)
store %struct.ScmObj* %f40217, %struct.ScmObj** %stackaddr$env-ref49750
%stackaddr$env-ref49751 = alloca %struct.ScmObj*, align 8
%acc40216 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41910, i64 3)
store %struct.ScmObj* %acc40216, %struct.ScmObj** %stackaddr$env-ref49751
%stackaddr$env-ref49752 = alloca %struct.ScmObj*, align 8
%_37foldl40213 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41910, i64 4)
store %struct.ScmObj* %_37foldl40213, %struct.ScmObj** %stackaddr$env-ref49752
%stackaddr$env-ref49753 = alloca %struct.ScmObj*, align 8
%_37map140161 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41910, i64 5)
store %struct.ScmObj* %_37map140161, %struct.ScmObj** %stackaddr$env-ref49753
%stackaddr$env-ref49754 = alloca %struct.ScmObj*, align 8
%k40460 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41910, i64 6)
store %struct.ScmObj* %k40460, %struct.ScmObj** %stackaddr$env-ref49754
%stackaddr$prim49755 = alloca %struct.ScmObj*, align 8
%_95k40462 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48578)
store volatile %struct.ScmObj* %_95k40462, %struct.ScmObj** %stackaddr$prim49755, align 8
%stackaddr$prim49756 = alloca %struct.ScmObj*, align 8
%current_45args48579 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48578)
store volatile %struct.ScmObj* %current_45args48579, %struct.ScmObj** %stackaddr$prim49756, align 8
%stackaddr$prim49757 = alloca %struct.ScmObj*, align 8
%anf_45bind40305 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48579)
store volatile %struct.ScmObj* %anf_45bind40305, %struct.ScmObj** %stackaddr$prim49757, align 8
%truthy$cmp49758 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40305)
%cmp$cmp49758 = icmp eq i64 %truthy$cmp49758, 1
br i1 %cmp$cmp49758, label %truebranch$cmp49758, label %falsebranch$cmp49758
truebranch$cmp49758:
%ae41921 = call %struct.ScmObj* @const_init_int(i64 0)
%args48581$k40460$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49759 = alloca %struct.ScmObj*, align 8
%args48581$k40460$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40216, %struct.ScmObj* %args48581$k40460$0)
store volatile %struct.ScmObj* %args48581$k40460$1, %struct.ScmObj** %stackaddr$prim49759, align 8
%stackaddr$prim49760 = alloca %struct.ScmObj*, align 8
%args48581$k40460$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41921, %struct.ScmObj* %args48581$k40460$1)
store volatile %struct.ScmObj* %args48581$k40460$2, %struct.ScmObj** %stackaddr$prim49760, align 8
%clofunc49761 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40460)
musttail call tailcc void %clofunc49761(%struct.ScmObj* %k40460, %struct.ScmObj* %args48581$k40460$2)
ret void
falsebranch$cmp49758:
%stackaddr$makeclosure49762 = alloca %struct.ScmObj*, align 8
%fptrToInt49763 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41926 to i64
%ae41926 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt49763)
store volatile %struct.ScmObj* %ae41926, %struct.ScmObj** %stackaddr$makeclosure49762, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41926, %struct.ScmObj* %lsts40215, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41926, %struct.ScmObj* %_37foldr40135, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41926, %struct.ScmObj* %f40217, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41926, %struct.ScmObj* %acc40216, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41926, %struct.ScmObj* %_37foldl40213, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41926, %struct.ScmObj* %_37map140161, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41926, %struct.ScmObj* %k40460, i64 6)
%ae41927 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49764 = alloca %struct.ScmObj*, align 8
%fptrToInt49765 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41928 to i64
%ae41928 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49765)
store volatile %struct.ScmObj* %ae41928, %struct.ScmObj** %stackaddr$makeclosure49764, align 8
%args48621$ae41926$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49766 = alloca %struct.ScmObj*, align 8
%args48621$ae41926$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41928, %struct.ScmObj* %args48621$ae41926$0)
store volatile %struct.ScmObj* %args48621$ae41926$1, %struct.ScmObj** %stackaddr$prim49766, align 8
%stackaddr$prim49767 = alloca %struct.ScmObj*, align 8
%args48621$ae41926$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41927, %struct.ScmObj* %args48621$ae41926$1)
store volatile %struct.ScmObj* %args48621$ae41926$2, %struct.ScmObj** %stackaddr$prim49767, align 8
%clofunc49768 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41926)
musttail call tailcc void %clofunc49768(%struct.ScmObj* %ae41926, %struct.ScmObj* %args48621$ae41926$2)
ret void
}

define tailcc void @proc_clo$ae41926(%struct.ScmObj* %env$ae41926,%struct.ScmObj* %current_45args48582) {
%stackaddr$env-ref49769 = alloca %struct.ScmObj*, align 8
%lsts40215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41926, i64 0)
store %struct.ScmObj* %lsts40215, %struct.ScmObj** %stackaddr$env-ref49769
%stackaddr$env-ref49770 = alloca %struct.ScmObj*, align 8
%_37foldr40135 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41926, i64 1)
store %struct.ScmObj* %_37foldr40135, %struct.ScmObj** %stackaddr$env-ref49770
%stackaddr$env-ref49771 = alloca %struct.ScmObj*, align 8
%f40217 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41926, i64 2)
store %struct.ScmObj* %f40217, %struct.ScmObj** %stackaddr$env-ref49771
%stackaddr$env-ref49772 = alloca %struct.ScmObj*, align 8
%acc40216 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41926, i64 3)
store %struct.ScmObj* %acc40216, %struct.ScmObj** %stackaddr$env-ref49772
%stackaddr$env-ref49773 = alloca %struct.ScmObj*, align 8
%_37foldl40213 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41926, i64 4)
store %struct.ScmObj* %_37foldl40213, %struct.ScmObj** %stackaddr$env-ref49773
%stackaddr$env-ref49774 = alloca %struct.ScmObj*, align 8
%_37map140161 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41926, i64 5)
store %struct.ScmObj* %_37map140161, %struct.ScmObj** %stackaddr$env-ref49774
%stackaddr$env-ref49775 = alloca %struct.ScmObj*, align 8
%k40460 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41926, i64 6)
store %struct.ScmObj* %k40460, %struct.ScmObj** %stackaddr$env-ref49775
%stackaddr$prim49776 = alloca %struct.ScmObj*, align 8
%_95k40463 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48582)
store volatile %struct.ScmObj* %_95k40463, %struct.ScmObj** %stackaddr$prim49776, align 8
%stackaddr$prim49777 = alloca %struct.ScmObj*, align 8
%current_45args48583 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48582)
store volatile %struct.ScmObj* %current_45args48583, %struct.ScmObj** %stackaddr$prim49777, align 8
%stackaddr$prim49778 = alloca %struct.ScmObj*, align 8
%anf_45bind40306 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48583)
store volatile %struct.ScmObj* %anf_45bind40306, %struct.ScmObj** %stackaddr$prim49778, align 8
%stackaddr$makeclosure49779 = alloca %struct.ScmObj*, align 8
%fptrToInt49780 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41947 to i64
%ae41947 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt49780)
store volatile %struct.ScmObj* %ae41947, %struct.ScmObj** %stackaddr$makeclosure49779, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41947, %struct.ScmObj* %lsts40215, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41947, %struct.ScmObj* %_37foldr40135, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41947, %struct.ScmObj* %f40217, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41947, %struct.ScmObj* %acc40216, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41947, %struct.ScmObj* %_37foldl40213, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41947, %struct.ScmObj* %_37map140161, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41947, %struct.ScmObj* %k40460, i64 6)
%args48616$_37map140161$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49781 = alloca %struct.ScmObj*, align 8
%args48616$_37map140161$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40215, %struct.ScmObj* %args48616$_37map140161$0)
store volatile %struct.ScmObj* %args48616$_37map140161$1, %struct.ScmObj** %stackaddr$prim49781, align 8
%stackaddr$prim49782 = alloca %struct.ScmObj*, align 8
%args48616$_37map140161$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40306, %struct.ScmObj* %args48616$_37map140161$1)
store volatile %struct.ScmObj* %args48616$_37map140161$2, %struct.ScmObj** %stackaddr$prim49782, align 8
%stackaddr$prim49783 = alloca %struct.ScmObj*, align 8
%args48616$_37map140161$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41947, %struct.ScmObj* %args48616$_37map140161$2)
store volatile %struct.ScmObj* %args48616$_37map140161$3, %struct.ScmObj** %stackaddr$prim49783, align 8
%clofunc49784 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140161)
musttail call tailcc void %clofunc49784(%struct.ScmObj* %_37map140161, %struct.ScmObj* %args48616$_37map140161$3)
ret void
}

define tailcc void @proc_clo$ae41947(%struct.ScmObj* %env$ae41947,%struct.ScmObj* %current_45args48585) {
%stackaddr$env-ref49785 = alloca %struct.ScmObj*, align 8
%lsts40215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41947, i64 0)
store %struct.ScmObj* %lsts40215, %struct.ScmObj** %stackaddr$env-ref49785
%stackaddr$env-ref49786 = alloca %struct.ScmObj*, align 8
%_37foldr40135 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41947, i64 1)
store %struct.ScmObj* %_37foldr40135, %struct.ScmObj** %stackaddr$env-ref49786
%stackaddr$env-ref49787 = alloca %struct.ScmObj*, align 8
%f40217 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41947, i64 2)
store %struct.ScmObj* %f40217, %struct.ScmObj** %stackaddr$env-ref49787
%stackaddr$env-ref49788 = alloca %struct.ScmObj*, align 8
%acc40216 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41947, i64 3)
store %struct.ScmObj* %acc40216, %struct.ScmObj** %stackaddr$env-ref49788
%stackaddr$env-ref49789 = alloca %struct.ScmObj*, align 8
%_37foldl40213 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41947, i64 4)
store %struct.ScmObj* %_37foldl40213, %struct.ScmObj** %stackaddr$env-ref49789
%stackaddr$env-ref49790 = alloca %struct.ScmObj*, align 8
%_37map140161 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41947, i64 5)
store %struct.ScmObj* %_37map140161, %struct.ScmObj** %stackaddr$env-ref49790
%stackaddr$env-ref49791 = alloca %struct.ScmObj*, align 8
%k40460 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41947, i64 6)
store %struct.ScmObj* %k40460, %struct.ScmObj** %stackaddr$env-ref49791
%stackaddr$prim49792 = alloca %struct.ScmObj*, align 8
%_95k40464 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48585)
store volatile %struct.ScmObj* %_95k40464, %struct.ScmObj** %stackaddr$prim49792, align 8
%stackaddr$prim49793 = alloca %struct.ScmObj*, align 8
%current_45args48586 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48585)
store volatile %struct.ScmObj* %current_45args48586, %struct.ScmObj** %stackaddr$prim49793, align 8
%stackaddr$prim49794 = alloca %struct.ScmObj*, align 8
%lsts_4340222 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48586)
store volatile %struct.ScmObj* %lsts_4340222, %struct.ScmObj** %stackaddr$prim49794, align 8
%stackaddr$makeclosure49795 = alloca %struct.ScmObj*, align 8
%fptrToInt49796 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41950 to i64
%ae41950 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt49796)
store volatile %struct.ScmObj* %ae41950, %struct.ScmObj** %stackaddr$makeclosure49795, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41950, %struct.ScmObj* %lsts40215, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41950, %struct.ScmObj* %_37foldr40135, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41950, %struct.ScmObj* %f40217, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41950, %struct.ScmObj* %acc40216, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41950, %struct.ScmObj* %_37foldl40213, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41950, %struct.ScmObj* %_37map140161, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41950, %struct.ScmObj* %lsts_4340222, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae41950, %struct.ScmObj* %k40460, i64 7)
%ae41951 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49797 = alloca %struct.ScmObj*, align 8
%fptrToInt49798 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41952 to i64
%ae41952 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49798)
store volatile %struct.ScmObj* %ae41952, %struct.ScmObj** %stackaddr$makeclosure49797, align 8
%args48615$ae41950$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49799 = alloca %struct.ScmObj*, align 8
%args48615$ae41950$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41952, %struct.ScmObj* %args48615$ae41950$0)
store volatile %struct.ScmObj* %args48615$ae41950$1, %struct.ScmObj** %stackaddr$prim49799, align 8
%stackaddr$prim49800 = alloca %struct.ScmObj*, align 8
%args48615$ae41950$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41951, %struct.ScmObj* %args48615$ae41950$1)
store volatile %struct.ScmObj* %args48615$ae41950$2, %struct.ScmObj** %stackaddr$prim49800, align 8
%clofunc49801 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41950)
musttail call tailcc void %clofunc49801(%struct.ScmObj* %ae41950, %struct.ScmObj* %args48615$ae41950$2)
ret void
}

define tailcc void @proc_clo$ae41950(%struct.ScmObj* %env$ae41950,%struct.ScmObj* %current_45args48588) {
%stackaddr$env-ref49802 = alloca %struct.ScmObj*, align 8
%lsts40215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41950, i64 0)
store %struct.ScmObj* %lsts40215, %struct.ScmObj** %stackaddr$env-ref49802
%stackaddr$env-ref49803 = alloca %struct.ScmObj*, align 8
%_37foldr40135 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41950, i64 1)
store %struct.ScmObj* %_37foldr40135, %struct.ScmObj** %stackaddr$env-ref49803
%stackaddr$env-ref49804 = alloca %struct.ScmObj*, align 8
%f40217 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41950, i64 2)
store %struct.ScmObj* %f40217, %struct.ScmObj** %stackaddr$env-ref49804
%stackaddr$env-ref49805 = alloca %struct.ScmObj*, align 8
%acc40216 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41950, i64 3)
store %struct.ScmObj* %acc40216, %struct.ScmObj** %stackaddr$env-ref49805
%stackaddr$env-ref49806 = alloca %struct.ScmObj*, align 8
%_37foldl40213 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41950, i64 4)
store %struct.ScmObj* %_37foldl40213, %struct.ScmObj** %stackaddr$env-ref49806
%stackaddr$env-ref49807 = alloca %struct.ScmObj*, align 8
%_37map140161 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41950, i64 5)
store %struct.ScmObj* %_37map140161, %struct.ScmObj** %stackaddr$env-ref49807
%stackaddr$env-ref49808 = alloca %struct.ScmObj*, align 8
%lsts_4340222 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41950, i64 6)
store %struct.ScmObj* %lsts_4340222, %struct.ScmObj** %stackaddr$env-ref49808
%stackaddr$env-ref49809 = alloca %struct.ScmObj*, align 8
%k40460 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41950, i64 7)
store %struct.ScmObj* %k40460, %struct.ScmObj** %stackaddr$env-ref49809
%stackaddr$prim49810 = alloca %struct.ScmObj*, align 8
%_95k40465 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48588)
store volatile %struct.ScmObj* %_95k40465, %struct.ScmObj** %stackaddr$prim49810, align 8
%stackaddr$prim49811 = alloca %struct.ScmObj*, align 8
%current_45args48589 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48588)
store volatile %struct.ScmObj* %current_45args48589, %struct.ScmObj** %stackaddr$prim49811, align 8
%stackaddr$prim49812 = alloca %struct.ScmObj*, align 8
%anf_45bind40307 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48589)
store volatile %struct.ScmObj* %anf_45bind40307, %struct.ScmObj** %stackaddr$prim49812, align 8
%stackaddr$makeclosure49813 = alloca %struct.ScmObj*, align 8
%fptrToInt49814 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41971 to i64
%ae41971 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt49814)
store volatile %struct.ScmObj* %ae41971, %struct.ScmObj** %stackaddr$makeclosure49813, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41971, %struct.ScmObj* %f40217, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41971, %struct.ScmObj* %acc40216, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41971, %struct.ScmObj* %_37foldr40135, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41971, %struct.ScmObj* %_37foldl40213, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41971, %struct.ScmObj* %lsts_4340222, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41971, %struct.ScmObj* %k40460, i64 5)
%args48610$_37map140161$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49815 = alloca %struct.ScmObj*, align 8
%args48610$_37map140161$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40215, %struct.ScmObj* %args48610$_37map140161$0)
store volatile %struct.ScmObj* %args48610$_37map140161$1, %struct.ScmObj** %stackaddr$prim49815, align 8
%stackaddr$prim49816 = alloca %struct.ScmObj*, align 8
%args48610$_37map140161$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40307, %struct.ScmObj* %args48610$_37map140161$1)
store volatile %struct.ScmObj* %args48610$_37map140161$2, %struct.ScmObj** %stackaddr$prim49816, align 8
%stackaddr$prim49817 = alloca %struct.ScmObj*, align 8
%args48610$_37map140161$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41971, %struct.ScmObj* %args48610$_37map140161$2)
store volatile %struct.ScmObj* %args48610$_37map140161$3, %struct.ScmObj** %stackaddr$prim49817, align 8
%clofunc49818 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140161)
musttail call tailcc void %clofunc49818(%struct.ScmObj* %_37map140161, %struct.ScmObj* %args48610$_37map140161$3)
ret void
}

define tailcc void @proc_clo$ae41971(%struct.ScmObj* %env$ae41971,%struct.ScmObj* %current_45args48591) {
%stackaddr$env-ref49819 = alloca %struct.ScmObj*, align 8
%f40217 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41971, i64 0)
store %struct.ScmObj* %f40217, %struct.ScmObj** %stackaddr$env-ref49819
%stackaddr$env-ref49820 = alloca %struct.ScmObj*, align 8
%acc40216 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41971, i64 1)
store %struct.ScmObj* %acc40216, %struct.ScmObj** %stackaddr$env-ref49820
%stackaddr$env-ref49821 = alloca %struct.ScmObj*, align 8
%_37foldr40135 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41971, i64 2)
store %struct.ScmObj* %_37foldr40135, %struct.ScmObj** %stackaddr$env-ref49821
%stackaddr$env-ref49822 = alloca %struct.ScmObj*, align 8
%_37foldl40213 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41971, i64 3)
store %struct.ScmObj* %_37foldl40213, %struct.ScmObj** %stackaddr$env-ref49822
%stackaddr$env-ref49823 = alloca %struct.ScmObj*, align 8
%lsts_4340222 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41971, i64 4)
store %struct.ScmObj* %lsts_4340222, %struct.ScmObj** %stackaddr$env-ref49823
%stackaddr$env-ref49824 = alloca %struct.ScmObj*, align 8
%k40460 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41971, i64 5)
store %struct.ScmObj* %k40460, %struct.ScmObj** %stackaddr$env-ref49824
%stackaddr$prim49825 = alloca %struct.ScmObj*, align 8
%_95k40466 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48591)
store volatile %struct.ScmObj* %_95k40466, %struct.ScmObj** %stackaddr$prim49825, align 8
%stackaddr$prim49826 = alloca %struct.ScmObj*, align 8
%current_45args48592 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48591)
store volatile %struct.ScmObj* %current_45args48592, %struct.ScmObj** %stackaddr$prim49826, align 8
%stackaddr$prim49827 = alloca %struct.ScmObj*, align 8
%vs40220 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48592)
store volatile %struct.ScmObj* %vs40220, %struct.ScmObj** %stackaddr$prim49827, align 8
%stackaddr$makeclosure49828 = alloca %struct.ScmObj*, align 8
%fptrToInt49829 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41974 to i64
%ae41974 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt49829)
store volatile %struct.ScmObj* %ae41974, %struct.ScmObj** %stackaddr$makeclosure49828, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41974, %struct.ScmObj* %vs40220, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41974, %struct.ScmObj* %k40460, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41974, %struct.ScmObj* %f40217, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41974, %struct.ScmObj* %acc40216, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41974, %struct.ScmObj* %_37foldr40135, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41974, %struct.ScmObj* %_37foldl40213, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41974, %struct.ScmObj* %lsts_4340222, i64 6)
%ae41975 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49830 = alloca %struct.ScmObj*, align 8
%fptrToInt49831 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41976 to i64
%ae41976 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49831)
store volatile %struct.ScmObj* %ae41976, %struct.ScmObj** %stackaddr$makeclosure49830, align 8
%args48609$ae41974$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49832 = alloca %struct.ScmObj*, align 8
%args48609$ae41974$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41976, %struct.ScmObj* %args48609$ae41974$0)
store volatile %struct.ScmObj* %args48609$ae41974$1, %struct.ScmObj** %stackaddr$prim49832, align 8
%stackaddr$prim49833 = alloca %struct.ScmObj*, align 8
%args48609$ae41974$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41975, %struct.ScmObj* %args48609$ae41974$1)
store volatile %struct.ScmObj* %args48609$ae41974$2, %struct.ScmObj** %stackaddr$prim49833, align 8
%clofunc49834 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41974)
musttail call tailcc void %clofunc49834(%struct.ScmObj* %ae41974, %struct.ScmObj* %args48609$ae41974$2)
ret void
}

define tailcc void @proc_clo$ae41974(%struct.ScmObj* %env$ae41974,%struct.ScmObj* %current_45args48594) {
%stackaddr$env-ref49835 = alloca %struct.ScmObj*, align 8
%vs40220 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41974, i64 0)
store %struct.ScmObj* %vs40220, %struct.ScmObj** %stackaddr$env-ref49835
%stackaddr$env-ref49836 = alloca %struct.ScmObj*, align 8
%k40460 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41974, i64 1)
store %struct.ScmObj* %k40460, %struct.ScmObj** %stackaddr$env-ref49836
%stackaddr$env-ref49837 = alloca %struct.ScmObj*, align 8
%f40217 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41974, i64 2)
store %struct.ScmObj* %f40217, %struct.ScmObj** %stackaddr$env-ref49837
%stackaddr$env-ref49838 = alloca %struct.ScmObj*, align 8
%acc40216 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41974, i64 3)
store %struct.ScmObj* %acc40216, %struct.ScmObj** %stackaddr$env-ref49838
%stackaddr$env-ref49839 = alloca %struct.ScmObj*, align 8
%_37foldr40135 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41974, i64 4)
store %struct.ScmObj* %_37foldr40135, %struct.ScmObj** %stackaddr$env-ref49839
%stackaddr$env-ref49840 = alloca %struct.ScmObj*, align 8
%_37foldl40213 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41974, i64 5)
store %struct.ScmObj* %_37foldl40213, %struct.ScmObj** %stackaddr$env-ref49840
%stackaddr$env-ref49841 = alloca %struct.ScmObj*, align 8
%lsts_4340222 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41974, i64 6)
store %struct.ScmObj* %lsts_4340222, %struct.ScmObj** %stackaddr$env-ref49841
%stackaddr$prim49842 = alloca %struct.ScmObj*, align 8
%_95k40467 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48594)
store volatile %struct.ScmObj* %_95k40467, %struct.ScmObj** %stackaddr$prim49842, align 8
%stackaddr$prim49843 = alloca %struct.ScmObj*, align 8
%current_45args48595 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48594)
store volatile %struct.ScmObj* %current_45args48595, %struct.ScmObj** %stackaddr$prim49843, align 8
%stackaddr$prim49844 = alloca %struct.ScmObj*, align 8
%anf_45bind40308 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48595)
store volatile %struct.ScmObj* %anf_45bind40308, %struct.ScmObj** %stackaddr$prim49844, align 8
%ae41997 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49845 = alloca %struct.ScmObj*, align 8
%anf_45bind40309 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40216, %struct.ScmObj* %ae41997)
store volatile %struct.ScmObj* %anf_45bind40309, %struct.ScmObj** %stackaddr$prim49845, align 8
%stackaddr$makeclosure49846 = alloca %struct.ScmObj*, align 8
%fptrToInt49847 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41999 to i64
%ae41999 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt49847)
store volatile %struct.ScmObj* %ae41999, %struct.ScmObj** %stackaddr$makeclosure49846, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41999, %struct.ScmObj* %f40217, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41999, %struct.ScmObj* %_37foldl40213, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41999, %struct.ScmObj* %lsts_4340222, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41999, %struct.ScmObj* %k40460, i64 3)
%args48603$_37foldr40135$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49848 = alloca %struct.ScmObj*, align 8
%args48603$_37foldr40135$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs40220, %struct.ScmObj* %args48603$_37foldr40135$0)
store volatile %struct.ScmObj* %args48603$_37foldr40135$1, %struct.ScmObj** %stackaddr$prim49848, align 8
%stackaddr$prim49849 = alloca %struct.ScmObj*, align 8
%args48603$_37foldr40135$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40309, %struct.ScmObj* %args48603$_37foldr40135$1)
store volatile %struct.ScmObj* %args48603$_37foldr40135$2, %struct.ScmObj** %stackaddr$prim49849, align 8
%stackaddr$prim49850 = alloca %struct.ScmObj*, align 8
%args48603$_37foldr40135$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40308, %struct.ScmObj* %args48603$_37foldr40135$2)
store volatile %struct.ScmObj* %args48603$_37foldr40135$3, %struct.ScmObj** %stackaddr$prim49850, align 8
%stackaddr$prim49851 = alloca %struct.ScmObj*, align 8
%args48603$_37foldr40135$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41999, %struct.ScmObj* %args48603$_37foldr40135$3)
store volatile %struct.ScmObj* %args48603$_37foldr40135$4, %struct.ScmObj** %stackaddr$prim49851, align 8
%clofunc49852 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr40135)
musttail call tailcc void %clofunc49852(%struct.ScmObj* %_37foldr40135, %struct.ScmObj* %args48603$_37foldr40135$4)
ret void
}

define tailcc void @proc_clo$ae41999(%struct.ScmObj* %env$ae41999,%struct.ScmObj* %current_45args48597) {
%stackaddr$env-ref49853 = alloca %struct.ScmObj*, align 8
%f40217 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41999, i64 0)
store %struct.ScmObj* %f40217, %struct.ScmObj** %stackaddr$env-ref49853
%stackaddr$env-ref49854 = alloca %struct.ScmObj*, align 8
%_37foldl40213 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41999, i64 1)
store %struct.ScmObj* %_37foldl40213, %struct.ScmObj** %stackaddr$env-ref49854
%stackaddr$env-ref49855 = alloca %struct.ScmObj*, align 8
%lsts_4340222 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41999, i64 2)
store %struct.ScmObj* %lsts_4340222, %struct.ScmObj** %stackaddr$env-ref49855
%stackaddr$env-ref49856 = alloca %struct.ScmObj*, align 8
%k40460 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41999, i64 3)
store %struct.ScmObj* %k40460, %struct.ScmObj** %stackaddr$env-ref49856
%stackaddr$prim49857 = alloca %struct.ScmObj*, align 8
%_95k40468 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48597)
store volatile %struct.ScmObj* %_95k40468, %struct.ScmObj** %stackaddr$prim49857, align 8
%stackaddr$prim49858 = alloca %struct.ScmObj*, align 8
%current_45args48598 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48597)
store volatile %struct.ScmObj* %current_45args48598, %struct.ScmObj** %stackaddr$prim49858, align 8
%stackaddr$prim49859 = alloca %struct.ScmObj*, align 8
%anf_45bind40310 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48598)
store volatile %struct.ScmObj* %anf_45bind40310, %struct.ScmObj** %stackaddr$prim49859, align 8
%stackaddr$makeclosure49860 = alloca %struct.ScmObj*, align 8
%fptrToInt49861 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42003 to i64
%ae42003 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt49861)
store volatile %struct.ScmObj* %ae42003, %struct.ScmObj** %stackaddr$makeclosure49860, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42003, %struct.ScmObj* %f40217, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42003, %struct.ScmObj* %_37foldl40213, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42003, %struct.ScmObj* %lsts_4340222, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae42003, %struct.ScmObj* %k40460, i64 3)
%stackaddr$prim49862 = alloca %struct.ScmObj*, align 8
%cpsargs40471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42003, %struct.ScmObj* %anf_45bind40310)
store volatile %struct.ScmObj* %cpsargs40471, %struct.ScmObj** %stackaddr$prim49862, align 8
%clofunc49863 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40217)
musttail call tailcc void %clofunc49863(%struct.ScmObj* %f40217, %struct.ScmObj* %cpsargs40471)
ret void
}

define tailcc void @proc_clo$ae42003(%struct.ScmObj* %env$ae42003,%struct.ScmObj* %current_45args48600) {
%stackaddr$env-ref49864 = alloca %struct.ScmObj*, align 8
%f40217 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42003, i64 0)
store %struct.ScmObj* %f40217, %struct.ScmObj** %stackaddr$env-ref49864
%stackaddr$env-ref49865 = alloca %struct.ScmObj*, align 8
%_37foldl40213 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42003, i64 1)
store %struct.ScmObj* %_37foldl40213, %struct.ScmObj** %stackaddr$env-ref49865
%stackaddr$env-ref49866 = alloca %struct.ScmObj*, align 8
%lsts_4340222 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42003, i64 2)
store %struct.ScmObj* %lsts_4340222, %struct.ScmObj** %stackaddr$env-ref49866
%stackaddr$env-ref49867 = alloca %struct.ScmObj*, align 8
%k40460 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42003, i64 3)
store %struct.ScmObj* %k40460, %struct.ScmObj** %stackaddr$env-ref49867
%stackaddr$prim49868 = alloca %struct.ScmObj*, align 8
%_95k40469 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48600)
store volatile %struct.ScmObj* %_95k40469, %struct.ScmObj** %stackaddr$prim49868, align 8
%stackaddr$prim49869 = alloca %struct.ScmObj*, align 8
%current_45args48601 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48600)
store volatile %struct.ScmObj* %current_45args48601, %struct.ScmObj** %stackaddr$prim49869, align 8
%stackaddr$prim49870 = alloca %struct.ScmObj*, align 8
%acc_4340224 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48601)
store volatile %struct.ScmObj* %acc_4340224, %struct.ScmObj** %stackaddr$prim49870, align 8
%stackaddr$prim49871 = alloca %struct.ScmObj*, align 8
%anf_45bind40311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4340224, %struct.ScmObj* %lsts_4340222)
store volatile %struct.ScmObj* %anf_45bind40311, %struct.ScmObj** %stackaddr$prim49871, align 8
%stackaddr$prim49872 = alloca %struct.ScmObj*, align 8
%anf_45bind40312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40217, %struct.ScmObj* %anf_45bind40311)
store volatile %struct.ScmObj* %anf_45bind40312, %struct.ScmObj** %stackaddr$prim49872, align 8
%stackaddr$prim49873 = alloca %struct.ScmObj*, align 8
%cpsargs40470 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40460, %struct.ScmObj* %anf_45bind40312)
store volatile %struct.ScmObj* %cpsargs40470, %struct.ScmObj** %stackaddr$prim49873, align 8
%clofunc49874 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl40213)
musttail call tailcc void %clofunc49874(%struct.ScmObj* %_37foldl40213, %struct.ScmObj* %cpsargs40470)
ret void
}

define tailcc void @proc_clo$ae41976(%struct.ScmObj* %env$ae41976,%struct.ScmObj* %current_45args48604) {
%stackaddr$prim49875 = alloca %struct.ScmObj*, align 8
%k40472 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48604)
store volatile %struct.ScmObj* %k40472, %struct.ScmObj** %stackaddr$prim49875, align 8
%stackaddr$prim49876 = alloca %struct.ScmObj*, align 8
%current_45args48605 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48604)
store volatile %struct.ScmObj* %current_45args48605, %struct.ScmObj** %stackaddr$prim49876, align 8
%stackaddr$prim49877 = alloca %struct.ScmObj*, align 8
%a40226 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48605)
store volatile %struct.ScmObj* %a40226, %struct.ScmObj** %stackaddr$prim49877, align 8
%stackaddr$prim49878 = alloca %struct.ScmObj*, align 8
%current_45args48606 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48605)
store volatile %struct.ScmObj* %current_45args48606, %struct.ScmObj** %stackaddr$prim49878, align 8
%stackaddr$prim49879 = alloca %struct.ScmObj*, align 8
%b40225 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48606)
store volatile %struct.ScmObj* %b40225, %struct.ScmObj** %stackaddr$prim49879, align 8
%stackaddr$prim49880 = alloca %struct.ScmObj*, align 8
%cpsprim40473 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a40226, %struct.ScmObj* %b40225)
store volatile %struct.ScmObj* %cpsprim40473, %struct.ScmObj** %stackaddr$prim49880, align 8
%ae41980 = call %struct.ScmObj* @const_init_int(i64 0)
%args48608$k40472$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49881 = alloca %struct.ScmObj*, align 8
%args48608$k40472$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40473, %struct.ScmObj* %args48608$k40472$0)
store volatile %struct.ScmObj* %args48608$k40472$1, %struct.ScmObj** %stackaddr$prim49881, align 8
%stackaddr$prim49882 = alloca %struct.ScmObj*, align 8
%args48608$k40472$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41980, %struct.ScmObj* %args48608$k40472$1)
store volatile %struct.ScmObj* %args48608$k40472$2, %struct.ScmObj** %stackaddr$prim49882, align 8
%clofunc49883 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40472)
musttail call tailcc void %clofunc49883(%struct.ScmObj* %k40472, %struct.ScmObj* %args48608$k40472$2)
ret void
}

define tailcc void @proc_clo$ae41952(%struct.ScmObj* %env$ae41952,%struct.ScmObj* %current_45args48611) {
%stackaddr$prim49884 = alloca %struct.ScmObj*, align 8
%k40474 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48611)
store volatile %struct.ScmObj* %k40474, %struct.ScmObj** %stackaddr$prim49884, align 8
%stackaddr$prim49885 = alloca %struct.ScmObj*, align 8
%current_45args48612 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48611)
store volatile %struct.ScmObj* %current_45args48612, %struct.ScmObj** %stackaddr$prim49885, align 8
%stackaddr$prim49886 = alloca %struct.ScmObj*, align 8
%x40221 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48612)
store volatile %struct.ScmObj* %x40221, %struct.ScmObj** %stackaddr$prim49886, align 8
%stackaddr$prim49887 = alloca %struct.ScmObj*, align 8
%cpsprim40475 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40221)
store volatile %struct.ScmObj* %cpsprim40475, %struct.ScmObj** %stackaddr$prim49887, align 8
%ae41955 = call %struct.ScmObj* @const_init_int(i64 0)
%args48614$k40474$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49888 = alloca %struct.ScmObj*, align 8
%args48614$k40474$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40475, %struct.ScmObj* %args48614$k40474$0)
store volatile %struct.ScmObj* %args48614$k40474$1, %struct.ScmObj** %stackaddr$prim49888, align 8
%stackaddr$prim49889 = alloca %struct.ScmObj*, align 8
%args48614$k40474$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41955, %struct.ScmObj* %args48614$k40474$1)
store volatile %struct.ScmObj* %args48614$k40474$2, %struct.ScmObj** %stackaddr$prim49889, align 8
%clofunc49890 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40474)
musttail call tailcc void %clofunc49890(%struct.ScmObj* %k40474, %struct.ScmObj* %args48614$k40474$2)
ret void
}

define tailcc void @proc_clo$ae41928(%struct.ScmObj* %env$ae41928,%struct.ScmObj* %current_45args48617) {
%stackaddr$prim49891 = alloca %struct.ScmObj*, align 8
%k40476 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48617)
store volatile %struct.ScmObj* %k40476, %struct.ScmObj** %stackaddr$prim49891, align 8
%stackaddr$prim49892 = alloca %struct.ScmObj*, align 8
%current_45args48618 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48617)
store volatile %struct.ScmObj* %current_45args48618, %struct.ScmObj** %stackaddr$prim49892, align 8
%stackaddr$prim49893 = alloca %struct.ScmObj*, align 8
%x40223 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48618)
store volatile %struct.ScmObj* %x40223, %struct.ScmObj** %stackaddr$prim49893, align 8
%stackaddr$prim49894 = alloca %struct.ScmObj*, align 8
%cpsprim40477 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40223)
store volatile %struct.ScmObj* %cpsprim40477, %struct.ScmObj** %stackaddr$prim49894, align 8
%ae41931 = call %struct.ScmObj* @const_init_int(i64 0)
%args48620$k40476$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49895 = alloca %struct.ScmObj*, align 8
%args48620$k40476$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40477, %struct.ScmObj* %args48620$k40476$0)
store volatile %struct.ScmObj* %args48620$k40476$1, %struct.ScmObj** %stackaddr$prim49895, align 8
%stackaddr$prim49896 = alloca %struct.ScmObj*, align 8
%args48620$k40476$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41931, %struct.ScmObj* %args48620$k40476$1)
store volatile %struct.ScmObj* %args48620$k40476$2, %struct.ScmObj** %stackaddr$prim49896, align 8
%clofunc49897 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40476)
musttail call tailcc void %clofunc49897(%struct.ScmObj* %k40476, %struct.ScmObj* %args48620$k40476$2)
ret void
}

define tailcc void @proc_clo$ae41880(%struct.ScmObj* %env$ae41880,%struct.ScmObj* %current_45args48623) {
%stackaddr$prim49898 = alloca %struct.ScmObj*, align 8
%k40478 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48623)
store volatile %struct.ScmObj* %k40478, %struct.ScmObj** %stackaddr$prim49898, align 8
%stackaddr$prim49899 = alloca %struct.ScmObj*, align 8
%current_45args48624 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48623)
store volatile %struct.ScmObj* %current_45args48624, %struct.ScmObj** %stackaddr$prim49899, align 8
%stackaddr$prim49900 = alloca %struct.ScmObj*, align 8
%lst40219 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48624)
store volatile %struct.ScmObj* %lst40219, %struct.ScmObj** %stackaddr$prim49900, align 8
%stackaddr$prim49901 = alloca %struct.ScmObj*, align 8
%current_45args48625 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48624)
store volatile %struct.ScmObj* %current_45args48625, %struct.ScmObj** %stackaddr$prim49901, align 8
%stackaddr$prim49902 = alloca %struct.ScmObj*, align 8
%b40218 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48625)
store volatile %struct.ScmObj* %b40218, %struct.ScmObj** %stackaddr$prim49902, align 8
%truthy$cmp49903 = call i64 @is_truthy_value(%struct.ScmObj* %b40218)
%cmp$cmp49903 = icmp eq i64 %truthy$cmp49903, 1
br i1 %cmp$cmp49903, label %truebranch$cmp49903, label %falsebranch$cmp49903
truebranch$cmp49903:
%ae41883 = call %struct.ScmObj* @const_init_int(i64 0)
%args48627$k40478$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49904 = alloca %struct.ScmObj*, align 8
%args48627$k40478$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b40218, %struct.ScmObj* %args48627$k40478$0)
store volatile %struct.ScmObj* %args48627$k40478$1, %struct.ScmObj** %stackaddr$prim49904, align 8
%stackaddr$prim49905 = alloca %struct.ScmObj*, align 8
%args48627$k40478$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41883, %struct.ScmObj* %args48627$k40478$1)
store volatile %struct.ScmObj* %args48627$k40478$2, %struct.ScmObj** %stackaddr$prim49905, align 8
%clofunc49906 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40478)
musttail call tailcc void %clofunc49906(%struct.ScmObj* %k40478, %struct.ScmObj* %args48627$k40478$2)
ret void
falsebranch$cmp49903:
%stackaddr$prim49907 = alloca %struct.ScmObj*, align 8
%cpsprim40479 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40219)
store volatile %struct.ScmObj* %cpsprim40479, %struct.ScmObj** %stackaddr$prim49907, align 8
%ae41890 = call %struct.ScmObj* @const_init_int(i64 0)
%args48628$k40478$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49908 = alloca %struct.ScmObj*, align 8
%args48628$k40478$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40479, %struct.ScmObj* %args48628$k40478$0)
store volatile %struct.ScmObj* %args48628$k40478$1, %struct.ScmObj** %stackaddr$prim49908, align 8
%stackaddr$prim49909 = alloca %struct.ScmObj*, align 8
%args48628$k40478$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41890, %struct.ScmObj* %args48628$k40478$1)
store volatile %struct.ScmObj* %args48628$k40478$2, %struct.ScmObj** %stackaddr$prim49909, align 8
%clofunc49910 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40478)
musttail call tailcc void %clofunc49910(%struct.ScmObj* %k40478, %struct.ScmObj* %args48628$k40478$2)
ret void
}

define tailcc void @proc_clo$ae41721(%struct.ScmObj* %env$ae41721,%struct.ScmObj* %args4015740480) {
%stackaddr$env-ref49911 = alloca %struct.ScmObj*, align 8
%_37last40152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41721, i64 0)
store %struct.ScmObj* %_37last40152, %struct.ScmObj** %stackaddr$env-ref49911
%stackaddr$env-ref49912 = alloca %struct.ScmObj*, align 8
%_37foldr40135 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41721, i64 1)
store %struct.ScmObj* %_37foldr40135, %struct.ScmObj** %stackaddr$env-ref49912
%stackaddr$env-ref49913 = alloca %struct.ScmObj*, align 8
%_37drop_45right40149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41721, i64 2)
store %struct.ScmObj* %_37drop_45right40149, %struct.ScmObj** %stackaddr$env-ref49913
%stackaddr$prim49914 = alloca %struct.ScmObj*, align 8
%k40481 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4015740480)
store volatile %struct.ScmObj* %k40481, %struct.ScmObj** %stackaddr$prim49914, align 8
%stackaddr$prim49915 = alloca %struct.ScmObj*, align 8
%args40157 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4015740480)
store volatile %struct.ScmObj* %args40157, %struct.ScmObj** %stackaddr$prim49915, align 8
%stackaddr$prim49916 = alloca %struct.ScmObj*, align 8
%f40159 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40157)
store volatile %struct.ScmObj* %f40159, %struct.ScmObj** %stackaddr$prim49916, align 8
%stackaddr$prim49917 = alloca %struct.ScmObj*, align 8
%lsts40158 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40157)
store volatile %struct.ScmObj* %lsts40158, %struct.ScmObj** %stackaddr$prim49917, align 8
%stackaddr$makeclosure49918 = alloca %struct.ScmObj*, align 8
%fptrToInt49919 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41726 to i64
%ae41726 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49919)
store volatile %struct.ScmObj* %ae41726, %struct.ScmObj** %stackaddr$makeclosure49918, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41726, %struct.ScmObj* %_37foldr40135, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41726, %struct.ScmObj* %k40481, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41726, %struct.ScmObj* %lsts40158, i64 2)
%ae41727 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49920 = alloca %struct.ScmObj*, align 8
%fptrToInt49921 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41728 to i64
%ae41728 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49921)
store volatile %struct.ScmObj* %ae41728, %struct.ScmObj** %stackaddr$makeclosure49920, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41728, %struct.ScmObj* %_37last40152, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41728, %struct.ScmObj* %_37drop_45right40149, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41728, %struct.ScmObj* %f40159, i64 2)
%args48647$ae41726$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49922 = alloca %struct.ScmObj*, align 8
%args48647$ae41726$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41728, %struct.ScmObj* %args48647$ae41726$0)
store volatile %struct.ScmObj* %args48647$ae41726$1, %struct.ScmObj** %stackaddr$prim49922, align 8
%stackaddr$prim49923 = alloca %struct.ScmObj*, align 8
%args48647$ae41726$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41727, %struct.ScmObj* %args48647$ae41726$1)
store volatile %struct.ScmObj* %args48647$ae41726$2, %struct.ScmObj** %stackaddr$prim49923, align 8
%clofunc49924 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41726)
musttail call tailcc void %clofunc49924(%struct.ScmObj* %ae41726, %struct.ScmObj* %args48647$ae41726$2)
ret void
}

define tailcc void @proc_clo$ae41726(%struct.ScmObj* %env$ae41726,%struct.ScmObj* %current_45args48632) {
%stackaddr$env-ref49925 = alloca %struct.ScmObj*, align 8
%_37foldr40135 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41726, i64 0)
store %struct.ScmObj* %_37foldr40135, %struct.ScmObj** %stackaddr$env-ref49925
%stackaddr$env-ref49926 = alloca %struct.ScmObj*, align 8
%k40481 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41726, i64 1)
store %struct.ScmObj* %k40481, %struct.ScmObj** %stackaddr$env-ref49926
%stackaddr$env-ref49927 = alloca %struct.ScmObj*, align 8
%lsts40158 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41726, i64 2)
store %struct.ScmObj* %lsts40158, %struct.ScmObj** %stackaddr$env-ref49927
%stackaddr$prim49928 = alloca %struct.ScmObj*, align 8
%_95k40482 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48632)
store volatile %struct.ScmObj* %_95k40482, %struct.ScmObj** %stackaddr$prim49928, align 8
%stackaddr$prim49929 = alloca %struct.ScmObj*, align 8
%current_45args48633 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48632)
store volatile %struct.ScmObj* %current_45args48633, %struct.ScmObj** %stackaddr$prim49929, align 8
%stackaddr$prim49930 = alloca %struct.ScmObj*, align 8
%anf_45bind40299 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48633)
store volatile %struct.ScmObj* %anf_45bind40299, %struct.ScmObj** %stackaddr$prim49930, align 8
%ae41789 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49931 = alloca %struct.ScmObj*, align 8
%anf_45bind40300 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41789, %struct.ScmObj* %lsts40158)
store volatile %struct.ScmObj* %anf_45bind40300, %struct.ScmObj** %stackaddr$prim49931, align 8
%stackaddr$prim49932 = alloca %struct.ScmObj*, align 8
%anf_45bind40301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40299, %struct.ScmObj* %anf_45bind40300)
store volatile %struct.ScmObj* %anf_45bind40301, %struct.ScmObj** %stackaddr$prim49932, align 8
%stackaddr$prim49933 = alloca %struct.ScmObj*, align 8
%cpsargs40483 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40481, %struct.ScmObj* %anf_45bind40301)
store volatile %struct.ScmObj* %cpsargs40483, %struct.ScmObj** %stackaddr$prim49933, align 8
%clofunc49934 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr40135)
musttail call tailcc void %clofunc49934(%struct.ScmObj* %_37foldr40135, %struct.ScmObj* %cpsargs40483)
ret void
}

define tailcc void @proc_clo$ae41728(%struct.ScmObj* %env$ae41728,%struct.ScmObj* %fargs4016040484) {
%stackaddr$env-ref49935 = alloca %struct.ScmObj*, align 8
%_37last40152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41728, i64 0)
store %struct.ScmObj* %_37last40152, %struct.ScmObj** %stackaddr$env-ref49935
%stackaddr$env-ref49936 = alloca %struct.ScmObj*, align 8
%_37drop_45right40149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41728, i64 1)
store %struct.ScmObj* %_37drop_45right40149, %struct.ScmObj** %stackaddr$env-ref49936
%stackaddr$env-ref49937 = alloca %struct.ScmObj*, align 8
%f40159 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41728, i64 2)
store %struct.ScmObj* %f40159, %struct.ScmObj** %stackaddr$env-ref49937
%stackaddr$prim49938 = alloca %struct.ScmObj*, align 8
%k40485 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs4016040484)
store volatile %struct.ScmObj* %k40485, %struct.ScmObj** %stackaddr$prim49938, align 8
%stackaddr$prim49939 = alloca %struct.ScmObj*, align 8
%fargs40160 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs4016040484)
store volatile %struct.ScmObj* %fargs40160, %struct.ScmObj** %stackaddr$prim49939, align 8
%stackaddr$makeclosure49940 = alloca %struct.ScmObj*, align 8
%fptrToInt49941 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41732 to i64
%ae41732 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt49941)
store volatile %struct.ScmObj* %ae41732, %struct.ScmObj** %stackaddr$makeclosure49940, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41732, %struct.ScmObj* %_37last40152, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41732, %struct.ScmObj* %k40485, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41732, %struct.ScmObj* %fargs40160, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41732, %struct.ScmObj* %f40159, i64 3)
%ae41734 = call %struct.ScmObj* @const_init_int(i64 1)
%args48646$_37drop_45right40149$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49942 = alloca %struct.ScmObj*, align 8
%args48646$_37drop_45right40149$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41734, %struct.ScmObj* %args48646$_37drop_45right40149$0)
store volatile %struct.ScmObj* %args48646$_37drop_45right40149$1, %struct.ScmObj** %stackaddr$prim49942, align 8
%stackaddr$prim49943 = alloca %struct.ScmObj*, align 8
%args48646$_37drop_45right40149$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs40160, %struct.ScmObj* %args48646$_37drop_45right40149$1)
store volatile %struct.ScmObj* %args48646$_37drop_45right40149$2, %struct.ScmObj** %stackaddr$prim49943, align 8
%stackaddr$prim49944 = alloca %struct.ScmObj*, align 8
%args48646$_37drop_45right40149$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41732, %struct.ScmObj* %args48646$_37drop_45right40149$2)
store volatile %struct.ScmObj* %args48646$_37drop_45right40149$3, %struct.ScmObj** %stackaddr$prim49944, align 8
%clofunc49945 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right40149)
musttail call tailcc void %clofunc49945(%struct.ScmObj* %_37drop_45right40149, %struct.ScmObj* %args48646$_37drop_45right40149$3)
ret void
}

define tailcc void @proc_clo$ae41732(%struct.ScmObj* %env$ae41732,%struct.ScmObj* %current_45args48635) {
%stackaddr$env-ref49946 = alloca %struct.ScmObj*, align 8
%_37last40152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41732, i64 0)
store %struct.ScmObj* %_37last40152, %struct.ScmObj** %stackaddr$env-ref49946
%stackaddr$env-ref49947 = alloca %struct.ScmObj*, align 8
%k40485 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41732, i64 1)
store %struct.ScmObj* %k40485, %struct.ScmObj** %stackaddr$env-ref49947
%stackaddr$env-ref49948 = alloca %struct.ScmObj*, align 8
%fargs40160 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41732, i64 2)
store %struct.ScmObj* %fargs40160, %struct.ScmObj** %stackaddr$env-ref49948
%stackaddr$env-ref49949 = alloca %struct.ScmObj*, align 8
%f40159 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41732, i64 3)
store %struct.ScmObj* %f40159, %struct.ScmObj** %stackaddr$env-ref49949
%stackaddr$prim49950 = alloca %struct.ScmObj*, align 8
%_95k40486 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48635)
store volatile %struct.ScmObj* %_95k40486, %struct.ScmObj** %stackaddr$prim49950, align 8
%stackaddr$prim49951 = alloca %struct.ScmObj*, align 8
%current_45args48636 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48635)
store volatile %struct.ScmObj* %current_45args48636, %struct.ScmObj** %stackaddr$prim49951, align 8
%stackaddr$prim49952 = alloca %struct.ScmObj*, align 8
%anf_45bind40296 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48636)
store volatile %struct.ScmObj* %anf_45bind40296, %struct.ScmObj** %stackaddr$prim49952, align 8
%stackaddr$makeclosure49953 = alloca %struct.ScmObj*, align 8
%fptrToInt49954 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41739 to i64
%ae41739 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49954)
store volatile %struct.ScmObj* %ae41739, %struct.ScmObj** %stackaddr$makeclosure49953, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41739, %struct.ScmObj* %_37last40152, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41739, %struct.ScmObj* %k40485, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41739, %struct.ScmObj* %fargs40160, i64 2)
%stackaddr$prim49955 = alloca %struct.ScmObj*, align 8
%cpsargs40490 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41739, %struct.ScmObj* %anf_45bind40296)
store volatile %struct.ScmObj* %cpsargs40490, %struct.ScmObj** %stackaddr$prim49955, align 8
%clofunc49956 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40159)
musttail call tailcc void %clofunc49956(%struct.ScmObj* %f40159, %struct.ScmObj* %cpsargs40490)
ret void
}

define tailcc void @proc_clo$ae41739(%struct.ScmObj* %env$ae41739,%struct.ScmObj* %current_45args48638) {
%stackaddr$env-ref49957 = alloca %struct.ScmObj*, align 8
%_37last40152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41739, i64 0)
store %struct.ScmObj* %_37last40152, %struct.ScmObj** %stackaddr$env-ref49957
%stackaddr$env-ref49958 = alloca %struct.ScmObj*, align 8
%k40485 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41739, i64 1)
store %struct.ScmObj* %k40485, %struct.ScmObj** %stackaddr$env-ref49958
%stackaddr$env-ref49959 = alloca %struct.ScmObj*, align 8
%fargs40160 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41739, i64 2)
store %struct.ScmObj* %fargs40160, %struct.ScmObj** %stackaddr$env-ref49959
%stackaddr$prim49960 = alloca %struct.ScmObj*, align 8
%_95k40487 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48638)
store volatile %struct.ScmObj* %_95k40487, %struct.ScmObj** %stackaddr$prim49960, align 8
%stackaddr$prim49961 = alloca %struct.ScmObj*, align 8
%current_45args48639 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48638)
store volatile %struct.ScmObj* %current_45args48639, %struct.ScmObj** %stackaddr$prim49961, align 8
%stackaddr$prim49962 = alloca %struct.ScmObj*, align 8
%anf_45bind40297 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48639)
store volatile %struct.ScmObj* %anf_45bind40297, %struct.ScmObj** %stackaddr$prim49962, align 8
%stackaddr$makeclosure49963 = alloca %struct.ScmObj*, align 8
%fptrToInt49964 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41744 to i64
%ae41744 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49964)
store volatile %struct.ScmObj* %ae41744, %struct.ScmObj** %stackaddr$makeclosure49963, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41744, %struct.ScmObj* %anf_45bind40297, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41744, %struct.ScmObj* %k40485, i64 1)
%args48645$_37last40152$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49965 = alloca %struct.ScmObj*, align 8
%args48645$_37last40152$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs40160, %struct.ScmObj* %args48645$_37last40152$0)
store volatile %struct.ScmObj* %args48645$_37last40152$1, %struct.ScmObj** %stackaddr$prim49965, align 8
%stackaddr$prim49966 = alloca %struct.ScmObj*, align 8
%args48645$_37last40152$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41744, %struct.ScmObj* %args48645$_37last40152$1)
store volatile %struct.ScmObj* %args48645$_37last40152$2, %struct.ScmObj** %stackaddr$prim49966, align 8
%clofunc49967 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last40152)
musttail call tailcc void %clofunc49967(%struct.ScmObj* %_37last40152, %struct.ScmObj* %args48645$_37last40152$2)
ret void
}

define tailcc void @proc_clo$ae41744(%struct.ScmObj* %env$ae41744,%struct.ScmObj* %current_45args48641) {
%stackaddr$env-ref49968 = alloca %struct.ScmObj*, align 8
%anf_45bind40297 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41744, i64 0)
store %struct.ScmObj* %anf_45bind40297, %struct.ScmObj** %stackaddr$env-ref49968
%stackaddr$env-ref49969 = alloca %struct.ScmObj*, align 8
%k40485 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41744, i64 1)
store %struct.ScmObj* %k40485, %struct.ScmObj** %stackaddr$env-ref49969
%stackaddr$prim49970 = alloca %struct.ScmObj*, align 8
%_95k40488 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48641)
store volatile %struct.ScmObj* %_95k40488, %struct.ScmObj** %stackaddr$prim49970, align 8
%stackaddr$prim49971 = alloca %struct.ScmObj*, align 8
%current_45args48642 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48641)
store volatile %struct.ScmObj* %current_45args48642, %struct.ScmObj** %stackaddr$prim49971, align 8
%stackaddr$prim49972 = alloca %struct.ScmObj*, align 8
%anf_45bind40298 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48642)
store volatile %struct.ScmObj* %anf_45bind40298, %struct.ScmObj** %stackaddr$prim49972, align 8
%stackaddr$prim49973 = alloca %struct.ScmObj*, align 8
%cpsprim40489 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40297, %struct.ScmObj* %anf_45bind40298)
store volatile %struct.ScmObj* %cpsprim40489, %struct.ScmObj** %stackaddr$prim49973, align 8
%ae41749 = call %struct.ScmObj* @const_init_int(i64 0)
%args48644$k40485$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49974 = alloca %struct.ScmObj*, align 8
%args48644$k40485$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40489, %struct.ScmObj* %args48644$k40485$0)
store volatile %struct.ScmObj* %args48644$k40485$1, %struct.ScmObj** %stackaddr$prim49974, align 8
%stackaddr$prim49975 = alloca %struct.ScmObj*, align 8
%args48644$k40485$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41749, %struct.ScmObj* %args48644$k40485$1)
store volatile %struct.ScmObj* %args48644$k40485$2, %struct.ScmObj** %stackaddr$prim49975, align 8
%clofunc49976 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40485)
musttail call tailcc void %clofunc49976(%struct.ScmObj* %k40485, %struct.ScmObj* %args48644$k40485$2)
ret void
}

define tailcc void @proc_clo$ae41644(%struct.ScmObj* %env$ae41644,%struct.ScmObj* %current_45args48649) {
%stackaddr$env-ref49977 = alloca %struct.ScmObj*, align 8
%_37foldr140130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41644, i64 0)
store %struct.ScmObj* %_37foldr140130, %struct.ScmObj** %stackaddr$env-ref49977
%stackaddr$prim49978 = alloca %struct.ScmObj*, align 8
%k40491 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48649)
store volatile %struct.ScmObj* %k40491, %struct.ScmObj** %stackaddr$prim49978, align 8
%stackaddr$prim49979 = alloca %struct.ScmObj*, align 8
%current_45args48650 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48649)
store volatile %struct.ScmObj* %current_45args48650, %struct.ScmObj** %stackaddr$prim49979, align 8
%stackaddr$prim49980 = alloca %struct.ScmObj*, align 8
%f40163 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48650)
store volatile %struct.ScmObj* %f40163, %struct.ScmObj** %stackaddr$prim49980, align 8
%stackaddr$prim49981 = alloca %struct.ScmObj*, align 8
%current_45args48651 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48650)
store volatile %struct.ScmObj* %current_45args48651, %struct.ScmObj** %stackaddr$prim49981, align 8
%stackaddr$prim49982 = alloca %struct.ScmObj*, align 8
%lst40162 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48651)
store volatile %struct.ScmObj* %lst40162, %struct.ScmObj** %stackaddr$prim49982, align 8
%stackaddr$makeclosure49983 = alloca %struct.ScmObj*, align 8
%fptrToInt49984 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41645 to i64
%ae41645 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49984)
store volatile %struct.ScmObj* %ae41645, %struct.ScmObj** %stackaddr$makeclosure49983, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41645, %struct.ScmObj* %lst40162, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41645, %struct.ScmObj* %_37foldr140130, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41645, %struct.ScmObj* %k40491, i64 2)
%ae41646 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49985 = alloca %struct.ScmObj*, align 8
%fptrToInt49986 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41647 to i64
%ae41647 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49986)
store volatile %struct.ScmObj* %ae41647, %struct.ScmObj** %stackaddr$makeclosure49985, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41647, %struct.ScmObj* %f40163, i64 0)
%args48666$ae41645$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49987 = alloca %struct.ScmObj*, align 8
%args48666$ae41645$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41647, %struct.ScmObj* %args48666$ae41645$0)
store volatile %struct.ScmObj* %args48666$ae41645$1, %struct.ScmObj** %stackaddr$prim49987, align 8
%stackaddr$prim49988 = alloca %struct.ScmObj*, align 8
%args48666$ae41645$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41646, %struct.ScmObj* %args48666$ae41645$1)
store volatile %struct.ScmObj* %args48666$ae41645$2, %struct.ScmObj** %stackaddr$prim49988, align 8
%clofunc49989 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41645)
musttail call tailcc void %clofunc49989(%struct.ScmObj* %ae41645, %struct.ScmObj* %args48666$ae41645$2)
ret void
}

define tailcc void @proc_clo$ae41645(%struct.ScmObj* %env$ae41645,%struct.ScmObj* %current_45args48653) {
%stackaddr$env-ref49990 = alloca %struct.ScmObj*, align 8
%lst40162 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41645, i64 0)
store %struct.ScmObj* %lst40162, %struct.ScmObj** %stackaddr$env-ref49990
%stackaddr$env-ref49991 = alloca %struct.ScmObj*, align 8
%_37foldr140130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41645, i64 1)
store %struct.ScmObj* %_37foldr140130, %struct.ScmObj** %stackaddr$env-ref49991
%stackaddr$env-ref49992 = alloca %struct.ScmObj*, align 8
%k40491 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41645, i64 2)
store %struct.ScmObj* %k40491, %struct.ScmObj** %stackaddr$env-ref49992
%stackaddr$prim49993 = alloca %struct.ScmObj*, align 8
%_95k40492 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48653)
store volatile %struct.ScmObj* %_95k40492, %struct.ScmObj** %stackaddr$prim49993, align 8
%stackaddr$prim49994 = alloca %struct.ScmObj*, align 8
%current_45args48654 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48653)
store volatile %struct.ScmObj* %current_45args48654, %struct.ScmObj** %stackaddr$prim49994, align 8
%stackaddr$prim49995 = alloca %struct.ScmObj*, align 8
%anf_45bind40295 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48654)
store volatile %struct.ScmObj* %anf_45bind40295, %struct.ScmObj** %stackaddr$prim49995, align 8
%ae41679 = call %struct.ScmObj* @const_init_null()
%args48656$_37foldr140130$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49996 = alloca %struct.ScmObj*, align 8
%args48656$_37foldr140130$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40162, %struct.ScmObj* %args48656$_37foldr140130$0)
store volatile %struct.ScmObj* %args48656$_37foldr140130$1, %struct.ScmObj** %stackaddr$prim49996, align 8
%stackaddr$prim49997 = alloca %struct.ScmObj*, align 8
%args48656$_37foldr140130$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41679, %struct.ScmObj* %args48656$_37foldr140130$1)
store volatile %struct.ScmObj* %args48656$_37foldr140130$2, %struct.ScmObj** %stackaddr$prim49997, align 8
%stackaddr$prim49998 = alloca %struct.ScmObj*, align 8
%args48656$_37foldr140130$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40295, %struct.ScmObj* %args48656$_37foldr140130$2)
store volatile %struct.ScmObj* %args48656$_37foldr140130$3, %struct.ScmObj** %stackaddr$prim49998, align 8
%stackaddr$prim49999 = alloca %struct.ScmObj*, align 8
%args48656$_37foldr140130$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40491, %struct.ScmObj* %args48656$_37foldr140130$3)
store volatile %struct.ScmObj* %args48656$_37foldr140130$4, %struct.ScmObj** %stackaddr$prim49999, align 8
%clofunc50000 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140130)
musttail call tailcc void %clofunc50000(%struct.ScmObj* %_37foldr140130, %struct.ScmObj* %args48656$_37foldr140130$4)
ret void
}

define tailcc void @proc_clo$ae41647(%struct.ScmObj* %env$ae41647,%struct.ScmObj* %current_45args48657) {
%stackaddr$env-ref50001 = alloca %struct.ScmObj*, align 8
%f40163 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41647, i64 0)
store %struct.ScmObj* %f40163, %struct.ScmObj** %stackaddr$env-ref50001
%stackaddr$prim50002 = alloca %struct.ScmObj*, align 8
%k40493 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48657)
store volatile %struct.ScmObj* %k40493, %struct.ScmObj** %stackaddr$prim50002, align 8
%stackaddr$prim50003 = alloca %struct.ScmObj*, align 8
%current_45args48658 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48657)
store volatile %struct.ScmObj* %current_45args48658, %struct.ScmObj** %stackaddr$prim50003, align 8
%stackaddr$prim50004 = alloca %struct.ScmObj*, align 8
%v40165 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48658)
store volatile %struct.ScmObj* %v40165, %struct.ScmObj** %stackaddr$prim50004, align 8
%stackaddr$prim50005 = alloca %struct.ScmObj*, align 8
%current_45args48659 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48658)
store volatile %struct.ScmObj* %current_45args48659, %struct.ScmObj** %stackaddr$prim50005, align 8
%stackaddr$prim50006 = alloca %struct.ScmObj*, align 8
%r40164 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48659)
store volatile %struct.ScmObj* %r40164, %struct.ScmObj** %stackaddr$prim50006, align 8
%stackaddr$makeclosure50007 = alloca %struct.ScmObj*, align 8
%fptrToInt50008 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41649 to i64
%ae41649 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt50008)
store volatile %struct.ScmObj* %ae41649, %struct.ScmObj** %stackaddr$makeclosure50007, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41649, %struct.ScmObj* %r40164, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41649, %struct.ScmObj* %k40493, i64 1)
%args48665$f40163$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50009 = alloca %struct.ScmObj*, align 8
%args48665$f40163$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v40165, %struct.ScmObj* %args48665$f40163$0)
store volatile %struct.ScmObj* %args48665$f40163$1, %struct.ScmObj** %stackaddr$prim50009, align 8
%stackaddr$prim50010 = alloca %struct.ScmObj*, align 8
%args48665$f40163$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41649, %struct.ScmObj* %args48665$f40163$1)
store volatile %struct.ScmObj* %args48665$f40163$2, %struct.ScmObj** %stackaddr$prim50010, align 8
%clofunc50011 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40163)
musttail call tailcc void %clofunc50011(%struct.ScmObj* %f40163, %struct.ScmObj* %args48665$f40163$2)
ret void
}

define tailcc void @proc_clo$ae41649(%struct.ScmObj* %env$ae41649,%struct.ScmObj* %current_45args48661) {
%stackaddr$env-ref50012 = alloca %struct.ScmObj*, align 8
%r40164 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41649, i64 0)
store %struct.ScmObj* %r40164, %struct.ScmObj** %stackaddr$env-ref50012
%stackaddr$env-ref50013 = alloca %struct.ScmObj*, align 8
%k40493 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41649, i64 1)
store %struct.ScmObj* %k40493, %struct.ScmObj** %stackaddr$env-ref50013
%stackaddr$prim50014 = alloca %struct.ScmObj*, align 8
%_95k40494 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48661)
store volatile %struct.ScmObj* %_95k40494, %struct.ScmObj** %stackaddr$prim50014, align 8
%stackaddr$prim50015 = alloca %struct.ScmObj*, align 8
%current_45args48662 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48661)
store volatile %struct.ScmObj* %current_45args48662, %struct.ScmObj** %stackaddr$prim50015, align 8
%stackaddr$prim50016 = alloca %struct.ScmObj*, align 8
%anf_45bind40294 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48662)
store volatile %struct.ScmObj* %anf_45bind40294, %struct.ScmObj** %stackaddr$prim50016, align 8
%stackaddr$prim50017 = alloca %struct.ScmObj*, align 8
%cpsprim40495 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40294, %struct.ScmObj* %r40164)
store volatile %struct.ScmObj* %cpsprim40495, %struct.ScmObj** %stackaddr$prim50017, align 8
%ae41654 = call %struct.ScmObj* @const_init_int(i64 0)
%args48664$k40493$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50018 = alloca %struct.ScmObj*, align 8
%args48664$k40493$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40495, %struct.ScmObj* %args48664$k40493$0)
store volatile %struct.ScmObj* %args48664$k40493$1, %struct.ScmObj** %stackaddr$prim50018, align 8
%stackaddr$prim50019 = alloca %struct.ScmObj*, align 8
%args48664$k40493$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41654, %struct.ScmObj* %args48664$k40493$1)
store volatile %struct.ScmObj* %args48664$k40493$2, %struct.ScmObj** %stackaddr$prim50019, align 8
%clofunc50020 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40493)
musttail call tailcc void %clofunc50020(%struct.ScmObj* %k40493, %struct.ScmObj* %args48664$k40493$2)
ret void
}

define tailcc void @proc_clo$ae41258(%struct.ScmObj* %env$ae41258,%struct.ScmObj* %current_45args48669) {
%stackaddr$env-ref50021 = alloca %struct.ScmObj*, align 8
%_37foldr140130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41258, i64 0)
store %struct.ScmObj* %_37foldr140130, %struct.ScmObj** %stackaddr$env-ref50021
%stackaddr$env-ref50022 = alloca %struct.ScmObj*, align 8
%_37map140126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41258, i64 1)
store %struct.ScmObj* %_37map140126, %struct.ScmObj** %stackaddr$env-ref50022
%stackaddr$prim50023 = alloca %struct.ScmObj*, align 8
%k40496 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48669)
store volatile %struct.ScmObj* %k40496, %struct.ScmObj** %stackaddr$prim50023, align 8
%stackaddr$prim50024 = alloca %struct.ScmObj*, align 8
%current_45args48670 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48669)
store volatile %struct.ScmObj* %current_45args48670, %struct.ScmObj** %stackaddr$prim50024, align 8
%stackaddr$prim50025 = alloca %struct.ScmObj*, align 8
%_37foldr40136 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48670)
store volatile %struct.ScmObj* %_37foldr40136, %struct.ScmObj** %stackaddr$prim50025, align 8
%ae41260 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50026 = alloca %struct.ScmObj*, align 8
%fptrToInt50027 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41261 to i64
%ae41261 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50027)
store volatile %struct.ScmObj* %ae41261, %struct.ScmObj** %stackaddr$makeclosure50026, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41261, %struct.ScmObj* %_37foldr40136, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41261, %struct.ScmObj* %_37foldr140130, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41261, %struct.ScmObj* %_37map140126, i64 2)
%args48727$k40496$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50028 = alloca %struct.ScmObj*, align 8
%args48727$k40496$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41261, %struct.ScmObj* %args48727$k40496$0)
store volatile %struct.ScmObj* %args48727$k40496$1, %struct.ScmObj** %stackaddr$prim50028, align 8
%stackaddr$prim50029 = alloca %struct.ScmObj*, align 8
%args48727$k40496$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41260, %struct.ScmObj* %args48727$k40496$1)
store volatile %struct.ScmObj* %args48727$k40496$2, %struct.ScmObj** %stackaddr$prim50029, align 8
%clofunc50030 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40496)
musttail call tailcc void %clofunc50030(%struct.ScmObj* %k40496, %struct.ScmObj* %args48727$k40496$2)
ret void
}

define tailcc void @proc_clo$ae41261(%struct.ScmObj* %env$ae41261,%struct.ScmObj* %args4013740497) {
%stackaddr$env-ref50031 = alloca %struct.ScmObj*, align 8
%_37foldr40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41261, i64 0)
store %struct.ScmObj* %_37foldr40136, %struct.ScmObj** %stackaddr$env-ref50031
%stackaddr$env-ref50032 = alloca %struct.ScmObj*, align 8
%_37foldr140130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41261, i64 1)
store %struct.ScmObj* %_37foldr140130, %struct.ScmObj** %stackaddr$env-ref50032
%stackaddr$env-ref50033 = alloca %struct.ScmObj*, align 8
%_37map140126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41261, i64 2)
store %struct.ScmObj* %_37map140126, %struct.ScmObj** %stackaddr$env-ref50033
%stackaddr$prim50034 = alloca %struct.ScmObj*, align 8
%k40498 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4013740497)
store volatile %struct.ScmObj* %k40498, %struct.ScmObj** %stackaddr$prim50034, align 8
%stackaddr$prim50035 = alloca %struct.ScmObj*, align 8
%args40137 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4013740497)
store volatile %struct.ScmObj* %args40137, %struct.ScmObj** %stackaddr$prim50035, align 8
%stackaddr$prim50036 = alloca %struct.ScmObj*, align 8
%f40140 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40137)
store volatile %struct.ScmObj* %f40140, %struct.ScmObj** %stackaddr$prim50036, align 8
%stackaddr$prim50037 = alloca %struct.ScmObj*, align 8
%anf_45bind40281 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40137)
store volatile %struct.ScmObj* %anf_45bind40281, %struct.ScmObj** %stackaddr$prim50037, align 8
%stackaddr$prim50038 = alloca %struct.ScmObj*, align 8
%acc40139 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40281)
store volatile %struct.ScmObj* %acc40139, %struct.ScmObj** %stackaddr$prim50038, align 8
%stackaddr$prim50039 = alloca %struct.ScmObj*, align 8
%anf_45bind40282 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40137)
store volatile %struct.ScmObj* %anf_45bind40282, %struct.ScmObj** %stackaddr$prim50039, align 8
%stackaddr$prim50040 = alloca %struct.ScmObj*, align 8
%lsts40138 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40282)
store volatile %struct.ScmObj* %lsts40138, %struct.ScmObj** %stackaddr$prim50040, align 8
%stackaddr$makeclosure50041 = alloca %struct.ScmObj*, align 8
%fptrToInt50042 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41269 to i64
%ae41269 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt50042)
store volatile %struct.ScmObj* %ae41269, %struct.ScmObj** %stackaddr$makeclosure50041, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41269, %struct.ScmObj* %k40498, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41269, %struct.ScmObj* %_37foldr140130, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41269, %struct.ScmObj* %_37foldr40136, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41269, %struct.ScmObj* %_37map140126, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41269, %struct.ScmObj* %f40140, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41269, %struct.ScmObj* %acc40139, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41269, %struct.ScmObj* %lsts40138, i64 6)
%ae41270 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50043 = alloca %struct.ScmObj*, align 8
%fptrToInt50044 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41271 to i64
%ae41271 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50044)
store volatile %struct.ScmObj* %ae41271, %struct.ScmObj** %stackaddr$makeclosure50043, align 8
%args48726$ae41269$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50045 = alloca %struct.ScmObj*, align 8
%args48726$ae41269$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41271, %struct.ScmObj* %args48726$ae41269$0)
store volatile %struct.ScmObj* %args48726$ae41269$1, %struct.ScmObj** %stackaddr$prim50045, align 8
%stackaddr$prim50046 = alloca %struct.ScmObj*, align 8
%args48726$ae41269$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41270, %struct.ScmObj* %args48726$ae41269$1)
store volatile %struct.ScmObj* %args48726$ae41269$2, %struct.ScmObj** %stackaddr$prim50046, align 8
%clofunc50047 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41269)
musttail call tailcc void %clofunc50047(%struct.ScmObj* %ae41269, %struct.ScmObj* %args48726$ae41269$2)
ret void
}

define tailcc void @proc_clo$ae41269(%struct.ScmObj* %env$ae41269,%struct.ScmObj* %current_45args48672) {
%stackaddr$env-ref50048 = alloca %struct.ScmObj*, align 8
%k40498 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41269, i64 0)
store %struct.ScmObj* %k40498, %struct.ScmObj** %stackaddr$env-ref50048
%stackaddr$env-ref50049 = alloca %struct.ScmObj*, align 8
%_37foldr140130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41269, i64 1)
store %struct.ScmObj* %_37foldr140130, %struct.ScmObj** %stackaddr$env-ref50049
%stackaddr$env-ref50050 = alloca %struct.ScmObj*, align 8
%_37foldr40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41269, i64 2)
store %struct.ScmObj* %_37foldr40136, %struct.ScmObj** %stackaddr$env-ref50050
%stackaddr$env-ref50051 = alloca %struct.ScmObj*, align 8
%_37map140126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41269, i64 3)
store %struct.ScmObj* %_37map140126, %struct.ScmObj** %stackaddr$env-ref50051
%stackaddr$env-ref50052 = alloca %struct.ScmObj*, align 8
%f40140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41269, i64 4)
store %struct.ScmObj* %f40140, %struct.ScmObj** %stackaddr$env-ref50052
%stackaddr$env-ref50053 = alloca %struct.ScmObj*, align 8
%acc40139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41269, i64 5)
store %struct.ScmObj* %acc40139, %struct.ScmObj** %stackaddr$env-ref50053
%stackaddr$env-ref50054 = alloca %struct.ScmObj*, align 8
%lsts40138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41269, i64 6)
store %struct.ScmObj* %lsts40138, %struct.ScmObj** %stackaddr$env-ref50054
%stackaddr$prim50055 = alloca %struct.ScmObj*, align 8
%_95k40499 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48672)
store volatile %struct.ScmObj* %_95k40499, %struct.ScmObj** %stackaddr$prim50055, align 8
%stackaddr$prim50056 = alloca %struct.ScmObj*, align 8
%current_45args48673 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48672)
store volatile %struct.ScmObj* %current_45args48673, %struct.ScmObj** %stackaddr$prim50056, align 8
%stackaddr$prim50057 = alloca %struct.ScmObj*, align 8
%anf_45bind40283 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48673)
store volatile %struct.ScmObj* %anf_45bind40283, %struct.ScmObj** %stackaddr$prim50057, align 8
%stackaddr$makeclosure50058 = alloca %struct.ScmObj*, align 8
%fptrToInt50059 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41301 to i64
%ae41301 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt50059)
store volatile %struct.ScmObj* %ae41301, %struct.ScmObj** %stackaddr$makeclosure50058, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41301, %struct.ScmObj* %k40498, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41301, %struct.ScmObj* %_37foldr140130, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41301, %struct.ScmObj* %_37foldr40136, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41301, %struct.ScmObj* %_37map140126, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41301, %struct.ScmObj* %f40140, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41301, %struct.ScmObj* %acc40139, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41301, %struct.ScmObj* %lsts40138, i64 6)
%ae41303 = call %struct.ScmObj* @const_init_false()
%args48719$_37foldr140130$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50060 = alloca %struct.ScmObj*, align 8
%args48719$_37foldr140130$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40138, %struct.ScmObj* %args48719$_37foldr140130$0)
store volatile %struct.ScmObj* %args48719$_37foldr140130$1, %struct.ScmObj** %stackaddr$prim50060, align 8
%stackaddr$prim50061 = alloca %struct.ScmObj*, align 8
%args48719$_37foldr140130$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41303, %struct.ScmObj* %args48719$_37foldr140130$1)
store volatile %struct.ScmObj* %args48719$_37foldr140130$2, %struct.ScmObj** %stackaddr$prim50061, align 8
%stackaddr$prim50062 = alloca %struct.ScmObj*, align 8
%args48719$_37foldr140130$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40283, %struct.ScmObj* %args48719$_37foldr140130$2)
store volatile %struct.ScmObj* %args48719$_37foldr140130$3, %struct.ScmObj** %stackaddr$prim50062, align 8
%stackaddr$prim50063 = alloca %struct.ScmObj*, align 8
%args48719$_37foldr140130$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41301, %struct.ScmObj* %args48719$_37foldr140130$3)
store volatile %struct.ScmObj* %args48719$_37foldr140130$4, %struct.ScmObj** %stackaddr$prim50063, align 8
%clofunc50064 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140130)
musttail call tailcc void %clofunc50064(%struct.ScmObj* %_37foldr140130, %struct.ScmObj* %args48719$_37foldr140130$4)
ret void
}

define tailcc void @proc_clo$ae41301(%struct.ScmObj* %env$ae41301,%struct.ScmObj* %current_45args48675) {
%stackaddr$env-ref50065 = alloca %struct.ScmObj*, align 8
%k40498 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41301, i64 0)
store %struct.ScmObj* %k40498, %struct.ScmObj** %stackaddr$env-ref50065
%stackaddr$env-ref50066 = alloca %struct.ScmObj*, align 8
%_37foldr140130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41301, i64 1)
store %struct.ScmObj* %_37foldr140130, %struct.ScmObj** %stackaddr$env-ref50066
%stackaddr$env-ref50067 = alloca %struct.ScmObj*, align 8
%_37foldr40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41301, i64 2)
store %struct.ScmObj* %_37foldr40136, %struct.ScmObj** %stackaddr$env-ref50067
%stackaddr$env-ref50068 = alloca %struct.ScmObj*, align 8
%_37map140126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41301, i64 3)
store %struct.ScmObj* %_37map140126, %struct.ScmObj** %stackaddr$env-ref50068
%stackaddr$env-ref50069 = alloca %struct.ScmObj*, align 8
%f40140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41301, i64 4)
store %struct.ScmObj* %f40140, %struct.ScmObj** %stackaddr$env-ref50069
%stackaddr$env-ref50070 = alloca %struct.ScmObj*, align 8
%acc40139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41301, i64 5)
store %struct.ScmObj* %acc40139, %struct.ScmObj** %stackaddr$env-ref50070
%stackaddr$env-ref50071 = alloca %struct.ScmObj*, align 8
%lsts40138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41301, i64 6)
store %struct.ScmObj* %lsts40138, %struct.ScmObj** %stackaddr$env-ref50071
%stackaddr$prim50072 = alloca %struct.ScmObj*, align 8
%_95k40500 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48675)
store volatile %struct.ScmObj* %_95k40500, %struct.ScmObj** %stackaddr$prim50072, align 8
%stackaddr$prim50073 = alloca %struct.ScmObj*, align 8
%current_45args48676 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48675)
store volatile %struct.ScmObj* %current_45args48676, %struct.ScmObj** %stackaddr$prim50073, align 8
%stackaddr$prim50074 = alloca %struct.ScmObj*, align 8
%anf_45bind40284 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48676)
store volatile %struct.ScmObj* %anf_45bind40284, %struct.ScmObj** %stackaddr$prim50074, align 8
%truthy$cmp50075 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40284)
%cmp$cmp50075 = icmp eq i64 %truthy$cmp50075, 1
br i1 %cmp$cmp50075, label %truebranch$cmp50075, label %falsebranch$cmp50075
truebranch$cmp50075:
%ae41312 = call %struct.ScmObj* @const_init_int(i64 0)
%args48678$k40498$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50076 = alloca %struct.ScmObj*, align 8
%args48678$k40498$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40139, %struct.ScmObj* %args48678$k40498$0)
store volatile %struct.ScmObj* %args48678$k40498$1, %struct.ScmObj** %stackaddr$prim50076, align 8
%stackaddr$prim50077 = alloca %struct.ScmObj*, align 8
%args48678$k40498$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41312, %struct.ScmObj* %args48678$k40498$1)
store volatile %struct.ScmObj* %args48678$k40498$2, %struct.ScmObj** %stackaddr$prim50077, align 8
%clofunc50078 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40498)
musttail call tailcc void %clofunc50078(%struct.ScmObj* %k40498, %struct.ScmObj* %args48678$k40498$2)
ret void
falsebranch$cmp50075:
%stackaddr$makeclosure50079 = alloca %struct.ScmObj*, align 8
%fptrToInt50080 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41317 to i64
%ae41317 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt50080)
store volatile %struct.ScmObj* %ae41317, %struct.ScmObj** %stackaddr$makeclosure50079, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41317, %struct.ScmObj* %k40498, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41317, %struct.ScmObj* %_37foldr140130, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41317, %struct.ScmObj* %_37foldr40136, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41317, %struct.ScmObj* %_37map140126, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41317, %struct.ScmObj* %f40140, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41317, %struct.ScmObj* %acc40139, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41317, %struct.ScmObj* %lsts40138, i64 6)
%ae41318 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50081 = alloca %struct.ScmObj*, align 8
%fptrToInt50082 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41319 to i64
%ae41319 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50082)
store volatile %struct.ScmObj* %ae41319, %struct.ScmObj** %stackaddr$makeclosure50081, align 8
%args48718$ae41317$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50083 = alloca %struct.ScmObj*, align 8
%args48718$ae41317$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41319, %struct.ScmObj* %args48718$ae41317$0)
store volatile %struct.ScmObj* %args48718$ae41317$1, %struct.ScmObj** %stackaddr$prim50083, align 8
%stackaddr$prim50084 = alloca %struct.ScmObj*, align 8
%args48718$ae41317$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41318, %struct.ScmObj* %args48718$ae41317$1)
store volatile %struct.ScmObj* %args48718$ae41317$2, %struct.ScmObj** %stackaddr$prim50084, align 8
%clofunc50085 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41317)
musttail call tailcc void %clofunc50085(%struct.ScmObj* %ae41317, %struct.ScmObj* %args48718$ae41317$2)
ret void
}

define tailcc void @proc_clo$ae41317(%struct.ScmObj* %env$ae41317,%struct.ScmObj* %current_45args48679) {
%stackaddr$env-ref50086 = alloca %struct.ScmObj*, align 8
%k40498 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41317, i64 0)
store %struct.ScmObj* %k40498, %struct.ScmObj** %stackaddr$env-ref50086
%stackaddr$env-ref50087 = alloca %struct.ScmObj*, align 8
%_37foldr140130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41317, i64 1)
store %struct.ScmObj* %_37foldr140130, %struct.ScmObj** %stackaddr$env-ref50087
%stackaddr$env-ref50088 = alloca %struct.ScmObj*, align 8
%_37foldr40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41317, i64 2)
store %struct.ScmObj* %_37foldr40136, %struct.ScmObj** %stackaddr$env-ref50088
%stackaddr$env-ref50089 = alloca %struct.ScmObj*, align 8
%_37map140126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41317, i64 3)
store %struct.ScmObj* %_37map140126, %struct.ScmObj** %stackaddr$env-ref50089
%stackaddr$env-ref50090 = alloca %struct.ScmObj*, align 8
%f40140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41317, i64 4)
store %struct.ScmObj* %f40140, %struct.ScmObj** %stackaddr$env-ref50090
%stackaddr$env-ref50091 = alloca %struct.ScmObj*, align 8
%acc40139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41317, i64 5)
store %struct.ScmObj* %acc40139, %struct.ScmObj** %stackaddr$env-ref50091
%stackaddr$env-ref50092 = alloca %struct.ScmObj*, align 8
%lsts40138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41317, i64 6)
store %struct.ScmObj* %lsts40138, %struct.ScmObj** %stackaddr$env-ref50092
%stackaddr$prim50093 = alloca %struct.ScmObj*, align 8
%_95k40501 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48679)
store volatile %struct.ScmObj* %_95k40501, %struct.ScmObj** %stackaddr$prim50093, align 8
%stackaddr$prim50094 = alloca %struct.ScmObj*, align 8
%current_45args48680 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48679)
store volatile %struct.ScmObj* %current_45args48680, %struct.ScmObj** %stackaddr$prim50094, align 8
%stackaddr$prim50095 = alloca %struct.ScmObj*, align 8
%anf_45bind40285 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48680)
store volatile %struct.ScmObj* %anf_45bind40285, %struct.ScmObj** %stackaddr$prim50095, align 8
%stackaddr$makeclosure50096 = alloca %struct.ScmObj*, align 8
%fptrToInt50097 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41338 to i64
%ae41338 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt50097)
store volatile %struct.ScmObj* %ae41338, %struct.ScmObj** %stackaddr$makeclosure50096, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41338, %struct.ScmObj* %k40498, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41338, %struct.ScmObj* %_37foldr140130, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41338, %struct.ScmObj* %_37foldr40136, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41338, %struct.ScmObj* %_37map140126, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41338, %struct.ScmObj* %f40140, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41338, %struct.ScmObj* %acc40139, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41338, %struct.ScmObj* %lsts40138, i64 6)
%args48713$_37map140126$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50098 = alloca %struct.ScmObj*, align 8
%args48713$_37map140126$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40138, %struct.ScmObj* %args48713$_37map140126$0)
store volatile %struct.ScmObj* %args48713$_37map140126$1, %struct.ScmObj** %stackaddr$prim50098, align 8
%stackaddr$prim50099 = alloca %struct.ScmObj*, align 8
%args48713$_37map140126$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40285, %struct.ScmObj* %args48713$_37map140126$1)
store volatile %struct.ScmObj* %args48713$_37map140126$2, %struct.ScmObj** %stackaddr$prim50099, align 8
%stackaddr$prim50100 = alloca %struct.ScmObj*, align 8
%args48713$_37map140126$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41338, %struct.ScmObj* %args48713$_37map140126$2)
store volatile %struct.ScmObj* %args48713$_37map140126$3, %struct.ScmObj** %stackaddr$prim50100, align 8
%clofunc50101 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140126)
musttail call tailcc void %clofunc50101(%struct.ScmObj* %_37map140126, %struct.ScmObj* %args48713$_37map140126$3)
ret void
}

define tailcc void @proc_clo$ae41338(%struct.ScmObj* %env$ae41338,%struct.ScmObj* %current_45args48682) {
%stackaddr$env-ref50102 = alloca %struct.ScmObj*, align 8
%k40498 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41338, i64 0)
store %struct.ScmObj* %k40498, %struct.ScmObj** %stackaddr$env-ref50102
%stackaddr$env-ref50103 = alloca %struct.ScmObj*, align 8
%_37foldr140130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41338, i64 1)
store %struct.ScmObj* %_37foldr140130, %struct.ScmObj** %stackaddr$env-ref50103
%stackaddr$env-ref50104 = alloca %struct.ScmObj*, align 8
%_37foldr40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41338, i64 2)
store %struct.ScmObj* %_37foldr40136, %struct.ScmObj** %stackaddr$env-ref50104
%stackaddr$env-ref50105 = alloca %struct.ScmObj*, align 8
%_37map140126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41338, i64 3)
store %struct.ScmObj* %_37map140126, %struct.ScmObj** %stackaddr$env-ref50105
%stackaddr$env-ref50106 = alloca %struct.ScmObj*, align 8
%f40140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41338, i64 4)
store %struct.ScmObj* %f40140, %struct.ScmObj** %stackaddr$env-ref50106
%stackaddr$env-ref50107 = alloca %struct.ScmObj*, align 8
%acc40139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41338, i64 5)
store %struct.ScmObj* %acc40139, %struct.ScmObj** %stackaddr$env-ref50107
%stackaddr$env-ref50108 = alloca %struct.ScmObj*, align 8
%lsts40138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41338, i64 6)
store %struct.ScmObj* %lsts40138, %struct.ScmObj** %stackaddr$env-ref50108
%stackaddr$prim50109 = alloca %struct.ScmObj*, align 8
%_95k40502 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48682)
store volatile %struct.ScmObj* %_95k40502, %struct.ScmObj** %stackaddr$prim50109, align 8
%stackaddr$prim50110 = alloca %struct.ScmObj*, align 8
%current_45args48683 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48682)
store volatile %struct.ScmObj* %current_45args48683, %struct.ScmObj** %stackaddr$prim50110, align 8
%stackaddr$prim50111 = alloca %struct.ScmObj*, align 8
%lsts_4340145 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48683)
store volatile %struct.ScmObj* %lsts_4340145, %struct.ScmObj** %stackaddr$prim50111, align 8
%stackaddr$makeclosure50112 = alloca %struct.ScmObj*, align 8
%fptrToInt50113 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41341 to i64
%ae41341 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt50113)
store volatile %struct.ScmObj* %ae41341, %struct.ScmObj** %stackaddr$makeclosure50112, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41341, %struct.ScmObj* %k40498, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41341, %struct.ScmObj* %_37foldr140130, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41341, %struct.ScmObj* %_37foldr40136, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41341, %struct.ScmObj* %lsts_4340145, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41341, %struct.ScmObj* %_37map140126, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41341, %struct.ScmObj* %f40140, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41341, %struct.ScmObj* %acc40139, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae41341, %struct.ScmObj* %lsts40138, i64 7)
%ae41342 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50114 = alloca %struct.ScmObj*, align 8
%fptrToInt50115 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41343 to i64
%ae41343 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50115)
store volatile %struct.ScmObj* %ae41343, %struct.ScmObj** %stackaddr$makeclosure50114, align 8
%args48712$ae41341$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50116 = alloca %struct.ScmObj*, align 8
%args48712$ae41341$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41343, %struct.ScmObj* %args48712$ae41341$0)
store volatile %struct.ScmObj* %args48712$ae41341$1, %struct.ScmObj** %stackaddr$prim50116, align 8
%stackaddr$prim50117 = alloca %struct.ScmObj*, align 8
%args48712$ae41341$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41342, %struct.ScmObj* %args48712$ae41341$1)
store volatile %struct.ScmObj* %args48712$ae41341$2, %struct.ScmObj** %stackaddr$prim50117, align 8
%clofunc50118 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41341)
musttail call tailcc void %clofunc50118(%struct.ScmObj* %ae41341, %struct.ScmObj* %args48712$ae41341$2)
ret void
}

define tailcc void @proc_clo$ae41341(%struct.ScmObj* %env$ae41341,%struct.ScmObj* %current_45args48685) {
%stackaddr$env-ref50119 = alloca %struct.ScmObj*, align 8
%k40498 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41341, i64 0)
store %struct.ScmObj* %k40498, %struct.ScmObj** %stackaddr$env-ref50119
%stackaddr$env-ref50120 = alloca %struct.ScmObj*, align 8
%_37foldr140130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41341, i64 1)
store %struct.ScmObj* %_37foldr140130, %struct.ScmObj** %stackaddr$env-ref50120
%stackaddr$env-ref50121 = alloca %struct.ScmObj*, align 8
%_37foldr40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41341, i64 2)
store %struct.ScmObj* %_37foldr40136, %struct.ScmObj** %stackaddr$env-ref50121
%stackaddr$env-ref50122 = alloca %struct.ScmObj*, align 8
%lsts_4340145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41341, i64 3)
store %struct.ScmObj* %lsts_4340145, %struct.ScmObj** %stackaddr$env-ref50122
%stackaddr$env-ref50123 = alloca %struct.ScmObj*, align 8
%_37map140126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41341, i64 4)
store %struct.ScmObj* %_37map140126, %struct.ScmObj** %stackaddr$env-ref50123
%stackaddr$env-ref50124 = alloca %struct.ScmObj*, align 8
%f40140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41341, i64 5)
store %struct.ScmObj* %f40140, %struct.ScmObj** %stackaddr$env-ref50124
%stackaddr$env-ref50125 = alloca %struct.ScmObj*, align 8
%acc40139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41341, i64 6)
store %struct.ScmObj* %acc40139, %struct.ScmObj** %stackaddr$env-ref50125
%stackaddr$env-ref50126 = alloca %struct.ScmObj*, align 8
%lsts40138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41341, i64 7)
store %struct.ScmObj* %lsts40138, %struct.ScmObj** %stackaddr$env-ref50126
%stackaddr$prim50127 = alloca %struct.ScmObj*, align 8
%_95k40503 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48685)
store volatile %struct.ScmObj* %_95k40503, %struct.ScmObj** %stackaddr$prim50127, align 8
%stackaddr$prim50128 = alloca %struct.ScmObj*, align 8
%current_45args48686 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48685)
store volatile %struct.ScmObj* %current_45args48686, %struct.ScmObj** %stackaddr$prim50128, align 8
%stackaddr$prim50129 = alloca %struct.ScmObj*, align 8
%anf_45bind40286 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48686)
store volatile %struct.ScmObj* %anf_45bind40286, %struct.ScmObj** %stackaddr$prim50129, align 8
%stackaddr$makeclosure50130 = alloca %struct.ScmObj*, align 8
%fptrToInt50131 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41362 to i64
%ae41362 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt50131)
store volatile %struct.ScmObj* %ae41362, %struct.ScmObj** %stackaddr$makeclosure50130, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41362, %struct.ScmObj* %k40498, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41362, %struct.ScmObj* %_37foldr140130, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41362, %struct.ScmObj* %_37foldr40136, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41362, %struct.ScmObj* %lsts_4340145, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41362, %struct.ScmObj* %f40140, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41362, %struct.ScmObj* %acc40139, i64 5)
%args48707$_37map140126$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50132 = alloca %struct.ScmObj*, align 8
%args48707$_37map140126$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40138, %struct.ScmObj* %args48707$_37map140126$0)
store volatile %struct.ScmObj* %args48707$_37map140126$1, %struct.ScmObj** %stackaddr$prim50132, align 8
%stackaddr$prim50133 = alloca %struct.ScmObj*, align 8
%args48707$_37map140126$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40286, %struct.ScmObj* %args48707$_37map140126$1)
store volatile %struct.ScmObj* %args48707$_37map140126$2, %struct.ScmObj** %stackaddr$prim50133, align 8
%stackaddr$prim50134 = alloca %struct.ScmObj*, align 8
%args48707$_37map140126$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41362, %struct.ScmObj* %args48707$_37map140126$2)
store volatile %struct.ScmObj* %args48707$_37map140126$3, %struct.ScmObj** %stackaddr$prim50134, align 8
%clofunc50135 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140126)
musttail call tailcc void %clofunc50135(%struct.ScmObj* %_37map140126, %struct.ScmObj* %args48707$_37map140126$3)
ret void
}

define tailcc void @proc_clo$ae41362(%struct.ScmObj* %env$ae41362,%struct.ScmObj* %current_45args48688) {
%stackaddr$env-ref50136 = alloca %struct.ScmObj*, align 8
%k40498 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41362, i64 0)
store %struct.ScmObj* %k40498, %struct.ScmObj** %stackaddr$env-ref50136
%stackaddr$env-ref50137 = alloca %struct.ScmObj*, align 8
%_37foldr140130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41362, i64 1)
store %struct.ScmObj* %_37foldr140130, %struct.ScmObj** %stackaddr$env-ref50137
%stackaddr$env-ref50138 = alloca %struct.ScmObj*, align 8
%_37foldr40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41362, i64 2)
store %struct.ScmObj* %_37foldr40136, %struct.ScmObj** %stackaddr$env-ref50138
%stackaddr$env-ref50139 = alloca %struct.ScmObj*, align 8
%lsts_4340145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41362, i64 3)
store %struct.ScmObj* %lsts_4340145, %struct.ScmObj** %stackaddr$env-ref50139
%stackaddr$env-ref50140 = alloca %struct.ScmObj*, align 8
%f40140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41362, i64 4)
store %struct.ScmObj* %f40140, %struct.ScmObj** %stackaddr$env-ref50140
%stackaddr$env-ref50141 = alloca %struct.ScmObj*, align 8
%acc40139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41362, i64 5)
store %struct.ScmObj* %acc40139, %struct.ScmObj** %stackaddr$env-ref50141
%stackaddr$prim50142 = alloca %struct.ScmObj*, align 8
%_95k40504 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48688)
store volatile %struct.ScmObj* %_95k40504, %struct.ScmObj** %stackaddr$prim50142, align 8
%stackaddr$prim50143 = alloca %struct.ScmObj*, align 8
%current_45args48689 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48688)
store volatile %struct.ScmObj* %current_45args48689, %struct.ScmObj** %stackaddr$prim50143, align 8
%stackaddr$prim50144 = alloca %struct.ScmObj*, align 8
%vs40143 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48689)
store volatile %struct.ScmObj* %vs40143, %struct.ScmObj** %stackaddr$prim50144, align 8
%stackaddr$makeclosure50145 = alloca %struct.ScmObj*, align 8
%fptrToInt50146 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41365 to i64
%ae41365 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt50146)
store volatile %struct.ScmObj* %ae41365, %struct.ScmObj** %stackaddr$makeclosure50145, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41365, %struct.ScmObj* %k40498, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41365, %struct.ScmObj* %_37foldr140130, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41365, %struct.ScmObj* %_37foldr40136, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41365, %struct.ScmObj* %lsts_4340145, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41365, %struct.ScmObj* %vs40143, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41365, %struct.ScmObj* %f40140, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41365, %struct.ScmObj* %acc40139, i64 6)
%ae41366 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50147 = alloca %struct.ScmObj*, align 8
%fptrToInt50148 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41367 to i64
%ae41367 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50148)
store volatile %struct.ScmObj* %ae41367, %struct.ScmObj** %stackaddr$makeclosure50147, align 8
%args48706$ae41365$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50149 = alloca %struct.ScmObj*, align 8
%args48706$ae41365$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41367, %struct.ScmObj* %args48706$ae41365$0)
store volatile %struct.ScmObj* %args48706$ae41365$1, %struct.ScmObj** %stackaddr$prim50149, align 8
%stackaddr$prim50150 = alloca %struct.ScmObj*, align 8
%args48706$ae41365$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41366, %struct.ScmObj* %args48706$ae41365$1)
store volatile %struct.ScmObj* %args48706$ae41365$2, %struct.ScmObj** %stackaddr$prim50150, align 8
%clofunc50151 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41365)
musttail call tailcc void %clofunc50151(%struct.ScmObj* %ae41365, %struct.ScmObj* %args48706$ae41365$2)
ret void
}

define tailcc void @proc_clo$ae41365(%struct.ScmObj* %env$ae41365,%struct.ScmObj* %current_45args48691) {
%stackaddr$env-ref50152 = alloca %struct.ScmObj*, align 8
%k40498 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41365, i64 0)
store %struct.ScmObj* %k40498, %struct.ScmObj** %stackaddr$env-ref50152
%stackaddr$env-ref50153 = alloca %struct.ScmObj*, align 8
%_37foldr140130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41365, i64 1)
store %struct.ScmObj* %_37foldr140130, %struct.ScmObj** %stackaddr$env-ref50153
%stackaddr$env-ref50154 = alloca %struct.ScmObj*, align 8
%_37foldr40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41365, i64 2)
store %struct.ScmObj* %_37foldr40136, %struct.ScmObj** %stackaddr$env-ref50154
%stackaddr$env-ref50155 = alloca %struct.ScmObj*, align 8
%lsts_4340145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41365, i64 3)
store %struct.ScmObj* %lsts_4340145, %struct.ScmObj** %stackaddr$env-ref50155
%stackaddr$env-ref50156 = alloca %struct.ScmObj*, align 8
%vs40143 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41365, i64 4)
store %struct.ScmObj* %vs40143, %struct.ScmObj** %stackaddr$env-ref50156
%stackaddr$env-ref50157 = alloca %struct.ScmObj*, align 8
%f40140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41365, i64 5)
store %struct.ScmObj* %f40140, %struct.ScmObj** %stackaddr$env-ref50157
%stackaddr$env-ref50158 = alloca %struct.ScmObj*, align 8
%acc40139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41365, i64 6)
store %struct.ScmObj* %acc40139, %struct.ScmObj** %stackaddr$env-ref50158
%stackaddr$prim50159 = alloca %struct.ScmObj*, align 8
%_95k40505 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48691)
store volatile %struct.ScmObj* %_95k40505, %struct.ScmObj** %stackaddr$prim50159, align 8
%stackaddr$prim50160 = alloca %struct.ScmObj*, align 8
%current_45args48692 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48691)
store volatile %struct.ScmObj* %current_45args48692, %struct.ScmObj** %stackaddr$prim50160, align 8
%stackaddr$prim50161 = alloca %struct.ScmObj*, align 8
%anf_45bind40287 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48692)
store volatile %struct.ScmObj* %anf_45bind40287, %struct.ScmObj** %stackaddr$prim50161, align 8
%stackaddr$prim50162 = alloca %struct.ScmObj*, align 8
%anf_45bind40288 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40139, %struct.ScmObj* %lsts_4340145)
store volatile %struct.ScmObj* %anf_45bind40288, %struct.ScmObj** %stackaddr$prim50162, align 8
%stackaddr$prim50163 = alloca %struct.ScmObj*, align 8
%anf_45bind40289 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40140, %struct.ScmObj* %anf_45bind40288)
store volatile %struct.ScmObj* %anf_45bind40289, %struct.ScmObj** %stackaddr$prim50163, align 8
%stackaddr$makeclosure50164 = alloca %struct.ScmObj*, align 8
%fptrToInt50165 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41391 to i64
%ae41391 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt50165)
store volatile %struct.ScmObj* %ae41391, %struct.ScmObj** %stackaddr$makeclosure50164, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41391, %struct.ScmObj* %k40498, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41391, %struct.ScmObj* %_37foldr140130, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41391, %struct.ScmObj* %anf_45bind40287, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41391, %struct.ScmObj* %vs40143, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41391, %struct.ScmObj* %f40140, i64 4)
%stackaddr$prim50166 = alloca %struct.ScmObj*, align 8
%cpsargs40509 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41391, %struct.ScmObj* %anf_45bind40289)
store volatile %struct.ScmObj* %cpsargs40509, %struct.ScmObj** %stackaddr$prim50166, align 8
%clofunc50167 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr40136)
musttail call tailcc void %clofunc50167(%struct.ScmObj* %_37foldr40136, %struct.ScmObj* %cpsargs40509)
ret void
}

define tailcc void @proc_clo$ae41391(%struct.ScmObj* %env$ae41391,%struct.ScmObj* %current_45args48694) {
%stackaddr$env-ref50168 = alloca %struct.ScmObj*, align 8
%k40498 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41391, i64 0)
store %struct.ScmObj* %k40498, %struct.ScmObj** %stackaddr$env-ref50168
%stackaddr$env-ref50169 = alloca %struct.ScmObj*, align 8
%_37foldr140130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41391, i64 1)
store %struct.ScmObj* %_37foldr140130, %struct.ScmObj** %stackaddr$env-ref50169
%stackaddr$env-ref50170 = alloca %struct.ScmObj*, align 8
%anf_45bind40287 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41391, i64 2)
store %struct.ScmObj* %anf_45bind40287, %struct.ScmObj** %stackaddr$env-ref50170
%stackaddr$env-ref50171 = alloca %struct.ScmObj*, align 8
%vs40143 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41391, i64 3)
store %struct.ScmObj* %vs40143, %struct.ScmObj** %stackaddr$env-ref50171
%stackaddr$env-ref50172 = alloca %struct.ScmObj*, align 8
%f40140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41391, i64 4)
store %struct.ScmObj* %f40140, %struct.ScmObj** %stackaddr$env-ref50172
%stackaddr$prim50173 = alloca %struct.ScmObj*, align 8
%_95k40506 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48694)
store volatile %struct.ScmObj* %_95k40506, %struct.ScmObj** %stackaddr$prim50173, align 8
%stackaddr$prim50174 = alloca %struct.ScmObj*, align 8
%current_45args48695 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48694)
store volatile %struct.ScmObj* %current_45args48695, %struct.ScmObj** %stackaddr$prim50174, align 8
%stackaddr$prim50175 = alloca %struct.ScmObj*, align 8
%anf_45bind40290 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48695)
store volatile %struct.ScmObj* %anf_45bind40290, %struct.ScmObj** %stackaddr$prim50175, align 8
%ae41396 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50176 = alloca %struct.ScmObj*, align 8
%anf_45bind40291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40290, %struct.ScmObj* %ae41396)
store volatile %struct.ScmObj* %anf_45bind40291, %struct.ScmObj** %stackaddr$prim50176, align 8
%stackaddr$makeclosure50177 = alloca %struct.ScmObj*, align 8
%fptrToInt50178 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41398 to i64
%ae41398 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt50178)
store volatile %struct.ScmObj* %ae41398, %struct.ScmObj** %stackaddr$makeclosure50177, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41398, %struct.ScmObj* %k40498, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41398, %struct.ScmObj* %f40140, i64 1)
%args48700$_37foldr140130$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50179 = alloca %struct.ScmObj*, align 8
%args48700$_37foldr140130$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs40143, %struct.ScmObj* %args48700$_37foldr140130$0)
store volatile %struct.ScmObj* %args48700$_37foldr140130$1, %struct.ScmObj** %stackaddr$prim50179, align 8
%stackaddr$prim50180 = alloca %struct.ScmObj*, align 8
%args48700$_37foldr140130$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40291, %struct.ScmObj* %args48700$_37foldr140130$1)
store volatile %struct.ScmObj* %args48700$_37foldr140130$2, %struct.ScmObj** %stackaddr$prim50180, align 8
%stackaddr$prim50181 = alloca %struct.ScmObj*, align 8
%args48700$_37foldr140130$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40287, %struct.ScmObj* %args48700$_37foldr140130$2)
store volatile %struct.ScmObj* %args48700$_37foldr140130$3, %struct.ScmObj** %stackaddr$prim50181, align 8
%stackaddr$prim50182 = alloca %struct.ScmObj*, align 8
%args48700$_37foldr140130$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41398, %struct.ScmObj* %args48700$_37foldr140130$3)
store volatile %struct.ScmObj* %args48700$_37foldr140130$4, %struct.ScmObj** %stackaddr$prim50182, align 8
%clofunc50183 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140130)
musttail call tailcc void %clofunc50183(%struct.ScmObj* %_37foldr140130, %struct.ScmObj* %args48700$_37foldr140130$4)
ret void
}

define tailcc void @proc_clo$ae41398(%struct.ScmObj* %env$ae41398,%struct.ScmObj* %current_45args48697) {
%stackaddr$env-ref50184 = alloca %struct.ScmObj*, align 8
%k40498 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41398, i64 0)
store %struct.ScmObj* %k40498, %struct.ScmObj** %stackaddr$env-ref50184
%stackaddr$env-ref50185 = alloca %struct.ScmObj*, align 8
%f40140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41398, i64 1)
store %struct.ScmObj* %f40140, %struct.ScmObj** %stackaddr$env-ref50185
%stackaddr$prim50186 = alloca %struct.ScmObj*, align 8
%_95k40507 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48697)
store volatile %struct.ScmObj* %_95k40507, %struct.ScmObj** %stackaddr$prim50186, align 8
%stackaddr$prim50187 = alloca %struct.ScmObj*, align 8
%current_45args48698 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48697)
store volatile %struct.ScmObj* %current_45args48698, %struct.ScmObj** %stackaddr$prim50187, align 8
%stackaddr$prim50188 = alloca %struct.ScmObj*, align 8
%anf_45bind40292 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48698)
store volatile %struct.ScmObj* %anf_45bind40292, %struct.ScmObj** %stackaddr$prim50188, align 8
%stackaddr$prim50189 = alloca %struct.ScmObj*, align 8
%cpsargs40508 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40498, %struct.ScmObj* %anf_45bind40292)
store volatile %struct.ScmObj* %cpsargs40508, %struct.ScmObj** %stackaddr$prim50189, align 8
%clofunc50190 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40140)
musttail call tailcc void %clofunc50190(%struct.ScmObj* %f40140, %struct.ScmObj* %cpsargs40508)
ret void
}

define tailcc void @proc_clo$ae41367(%struct.ScmObj* %env$ae41367,%struct.ScmObj* %current_45args48701) {
%stackaddr$prim50191 = alloca %struct.ScmObj*, align 8
%k40510 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48701)
store volatile %struct.ScmObj* %k40510, %struct.ScmObj** %stackaddr$prim50191, align 8
%stackaddr$prim50192 = alloca %struct.ScmObj*, align 8
%current_45args48702 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48701)
store volatile %struct.ScmObj* %current_45args48702, %struct.ScmObj** %stackaddr$prim50192, align 8
%stackaddr$prim50193 = alloca %struct.ScmObj*, align 8
%a40148 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48702)
store volatile %struct.ScmObj* %a40148, %struct.ScmObj** %stackaddr$prim50193, align 8
%stackaddr$prim50194 = alloca %struct.ScmObj*, align 8
%current_45args48703 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48702)
store volatile %struct.ScmObj* %current_45args48703, %struct.ScmObj** %stackaddr$prim50194, align 8
%stackaddr$prim50195 = alloca %struct.ScmObj*, align 8
%b40147 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48703)
store volatile %struct.ScmObj* %b40147, %struct.ScmObj** %stackaddr$prim50195, align 8
%stackaddr$prim50196 = alloca %struct.ScmObj*, align 8
%cpsprim40511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a40148, %struct.ScmObj* %b40147)
store volatile %struct.ScmObj* %cpsprim40511, %struct.ScmObj** %stackaddr$prim50196, align 8
%ae41371 = call %struct.ScmObj* @const_init_int(i64 0)
%args48705$k40510$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50197 = alloca %struct.ScmObj*, align 8
%args48705$k40510$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40511, %struct.ScmObj* %args48705$k40510$0)
store volatile %struct.ScmObj* %args48705$k40510$1, %struct.ScmObj** %stackaddr$prim50197, align 8
%stackaddr$prim50198 = alloca %struct.ScmObj*, align 8
%args48705$k40510$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41371, %struct.ScmObj* %args48705$k40510$1)
store volatile %struct.ScmObj* %args48705$k40510$2, %struct.ScmObj** %stackaddr$prim50198, align 8
%clofunc50199 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40510)
musttail call tailcc void %clofunc50199(%struct.ScmObj* %k40510, %struct.ScmObj* %args48705$k40510$2)
ret void
}

define tailcc void @proc_clo$ae41343(%struct.ScmObj* %env$ae41343,%struct.ScmObj* %current_45args48708) {
%stackaddr$prim50200 = alloca %struct.ScmObj*, align 8
%k40512 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48708)
store volatile %struct.ScmObj* %k40512, %struct.ScmObj** %stackaddr$prim50200, align 8
%stackaddr$prim50201 = alloca %struct.ScmObj*, align 8
%current_45args48709 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48708)
store volatile %struct.ScmObj* %current_45args48709, %struct.ScmObj** %stackaddr$prim50201, align 8
%stackaddr$prim50202 = alloca %struct.ScmObj*, align 8
%x40144 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48709)
store volatile %struct.ScmObj* %x40144, %struct.ScmObj** %stackaddr$prim50202, align 8
%stackaddr$prim50203 = alloca %struct.ScmObj*, align 8
%cpsprim40513 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40144)
store volatile %struct.ScmObj* %cpsprim40513, %struct.ScmObj** %stackaddr$prim50203, align 8
%ae41346 = call %struct.ScmObj* @const_init_int(i64 0)
%args48711$k40512$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50204 = alloca %struct.ScmObj*, align 8
%args48711$k40512$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40513, %struct.ScmObj* %args48711$k40512$0)
store volatile %struct.ScmObj* %args48711$k40512$1, %struct.ScmObj** %stackaddr$prim50204, align 8
%stackaddr$prim50205 = alloca %struct.ScmObj*, align 8
%args48711$k40512$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41346, %struct.ScmObj* %args48711$k40512$1)
store volatile %struct.ScmObj* %args48711$k40512$2, %struct.ScmObj** %stackaddr$prim50205, align 8
%clofunc50206 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40512)
musttail call tailcc void %clofunc50206(%struct.ScmObj* %k40512, %struct.ScmObj* %args48711$k40512$2)
ret void
}

define tailcc void @proc_clo$ae41319(%struct.ScmObj* %env$ae41319,%struct.ScmObj* %current_45args48714) {
%stackaddr$prim50207 = alloca %struct.ScmObj*, align 8
%k40514 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48714)
store volatile %struct.ScmObj* %k40514, %struct.ScmObj** %stackaddr$prim50207, align 8
%stackaddr$prim50208 = alloca %struct.ScmObj*, align 8
%current_45args48715 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48714)
store volatile %struct.ScmObj* %current_45args48715, %struct.ScmObj** %stackaddr$prim50208, align 8
%stackaddr$prim50209 = alloca %struct.ScmObj*, align 8
%x40146 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48715)
store volatile %struct.ScmObj* %x40146, %struct.ScmObj** %stackaddr$prim50209, align 8
%stackaddr$prim50210 = alloca %struct.ScmObj*, align 8
%cpsprim40515 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40146)
store volatile %struct.ScmObj* %cpsprim40515, %struct.ScmObj** %stackaddr$prim50210, align 8
%ae41322 = call %struct.ScmObj* @const_init_int(i64 0)
%args48717$k40514$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50211 = alloca %struct.ScmObj*, align 8
%args48717$k40514$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40515, %struct.ScmObj* %args48717$k40514$0)
store volatile %struct.ScmObj* %args48717$k40514$1, %struct.ScmObj** %stackaddr$prim50211, align 8
%stackaddr$prim50212 = alloca %struct.ScmObj*, align 8
%args48717$k40514$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41322, %struct.ScmObj* %args48717$k40514$1)
store volatile %struct.ScmObj* %args48717$k40514$2, %struct.ScmObj** %stackaddr$prim50212, align 8
%clofunc50213 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40514)
musttail call tailcc void %clofunc50213(%struct.ScmObj* %k40514, %struct.ScmObj* %args48717$k40514$2)
ret void
}

define tailcc void @proc_clo$ae41271(%struct.ScmObj* %env$ae41271,%struct.ScmObj* %current_45args48720) {
%stackaddr$prim50214 = alloca %struct.ScmObj*, align 8
%k40516 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48720)
store volatile %struct.ScmObj* %k40516, %struct.ScmObj** %stackaddr$prim50214, align 8
%stackaddr$prim50215 = alloca %struct.ScmObj*, align 8
%current_45args48721 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48720)
store volatile %struct.ScmObj* %current_45args48721, %struct.ScmObj** %stackaddr$prim50215, align 8
%stackaddr$prim50216 = alloca %struct.ScmObj*, align 8
%lst40142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48721)
store volatile %struct.ScmObj* %lst40142, %struct.ScmObj** %stackaddr$prim50216, align 8
%stackaddr$prim50217 = alloca %struct.ScmObj*, align 8
%current_45args48722 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48721)
store volatile %struct.ScmObj* %current_45args48722, %struct.ScmObj** %stackaddr$prim50217, align 8
%stackaddr$prim50218 = alloca %struct.ScmObj*, align 8
%b40141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48722)
store volatile %struct.ScmObj* %b40141, %struct.ScmObj** %stackaddr$prim50218, align 8
%truthy$cmp50219 = call i64 @is_truthy_value(%struct.ScmObj* %b40141)
%cmp$cmp50219 = icmp eq i64 %truthy$cmp50219, 1
br i1 %cmp$cmp50219, label %truebranch$cmp50219, label %falsebranch$cmp50219
truebranch$cmp50219:
%ae41274 = call %struct.ScmObj* @const_init_int(i64 0)
%args48724$k40516$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50220 = alloca %struct.ScmObj*, align 8
%args48724$k40516$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b40141, %struct.ScmObj* %args48724$k40516$0)
store volatile %struct.ScmObj* %args48724$k40516$1, %struct.ScmObj** %stackaddr$prim50220, align 8
%stackaddr$prim50221 = alloca %struct.ScmObj*, align 8
%args48724$k40516$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41274, %struct.ScmObj* %args48724$k40516$1)
store volatile %struct.ScmObj* %args48724$k40516$2, %struct.ScmObj** %stackaddr$prim50221, align 8
%clofunc50222 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40516)
musttail call tailcc void %clofunc50222(%struct.ScmObj* %k40516, %struct.ScmObj* %args48724$k40516$2)
ret void
falsebranch$cmp50219:
%stackaddr$prim50223 = alloca %struct.ScmObj*, align 8
%cpsprim40517 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40142)
store volatile %struct.ScmObj* %cpsprim40517, %struct.ScmObj** %stackaddr$prim50223, align 8
%ae41281 = call %struct.ScmObj* @const_init_int(i64 0)
%args48725$k40516$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50224 = alloca %struct.ScmObj*, align 8
%args48725$k40516$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40517, %struct.ScmObj* %args48725$k40516$0)
store volatile %struct.ScmObj* %args48725$k40516$1, %struct.ScmObj** %stackaddr$prim50224, align 8
%stackaddr$prim50225 = alloca %struct.ScmObj*, align 8
%args48725$k40516$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41281, %struct.ScmObj* %args48725$k40516$1)
store volatile %struct.ScmObj* %args48725$k40516$2, %struct.ScmObj** %stackaddr$prim50225, align 8
%clofunc50226 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40516)
musttail call tailcc void %clofunc50226(%struct.ScmObj* %k40516, %struct.ScmObj* %args48725$k40516$2)
ret void
}

define tailcc void @proc_clo$ae41228(%struct.ScmObj* %env$ae41228,%struct.ScmObj* %current_45args48729) {
%stackaddr$env-ref50227 = alloca %struct.ScmObj*, align 8
%_37length40119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41228, i64 0)
store %struct.ScmObj* %_37length40119, %struct.ScmObj** %stackaddr$env-ref50227
%stackaddr$env-ref50228 = alloca %struct.ScmObj*, align 8
%_37take40122 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41228, i64 1)
store %struct.ScmObj* %_37take40122, %struct.ScmObj** %stackaddr$env-ref50228
%stackaddr$prim50229 = alloca %struct.ScmObj*, align 8
%k40518 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48729)
store volatile %struct.ScmObj* %k40518, %struct.ScmObj** %stackaddr$prim50229, align 8
%stackaddr$prim50230 = alloca %struct.ScmObj*, align 8
%current_45args48730 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48729)
store volatile %struct.ScmObj* %current_45args48730, %struct.ScmObj** %stackaddr$prim50230, align 8
%stackaddr$prim50231 = alloca %struct.ScmObj*, align 8
%lst40151 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48730)
store volatile %struct.ScmObj* %lst40151, %struct.ScmObj** %stackaddr$prim50231, align 8
%stackaddr$prim50232 = alloca %struct.ScmObj*, align 8
%current_45args48731 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48730)
store volatile %struct.ScmObj* %current_45args48731, %struct.ScmObj** %stackaddr$prim50232, align 8
%stackaddr$prim50233 = alloca %struct.ScmObj*, align 8
%n40150 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48731)
store volatile %struct.ScmObj* %n40150, %struct.ScmObj** %stackaddr$prim50233, align 8
%stackaddr$makeclosure50234 = alloca %struct.ScmObj*, align 8
%fptrToInt50235 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41230 to i64
%ae41230 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt50235)
store volatile %struct.ScmObj* %ae41230, %struct.ScmObj** %stackaddr$makeclosure50234, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41230, %struct.ScmObj* %k40518, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41230, %struct.ScmObj* %n40150, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41230, %struct.ScmObj* %lst40151, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41230, %struct.ScmObj* %_37take40122, i64 3)
%args48737$_37length40119$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50236 = alloca %struct.ScmObj*, align 8
%args48737$_37length40119$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40151, %struct.ScmObj* %args48737$_37length40119$0)
store volatile %struct.ScmObj* %args48737$_37length40119$1, %struct.ScmObj** %stackaddr$prim50236, align 8
%stackaddr$prim50237 = alloca %struct.ScmObj*, align 8
%args48737$_37length40119$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41230, %struct.ScmObj* %args48737$_37length40119$1)
store volatile %struct.ScmObj* %args48737$_37length40119$2, %struct.ScmObj** %stackaddr$prim50237, align 8
%clofunc50238 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length40119)
musttail call tailcc void %clofunc50238(%struct.ScmObj* %_37length40119, %struct.ScmObj* %args48737$_37length40119$2)
ret void
}

define tailcc void @proc_clo$ae41230(%struct.ScmObj* %env$ae41230,%struct.ScmObj* %current_45args48733) {
%stackaddr$env-ref50239 = alloca %struct.ScmObj*, align 8
%k40518 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41230, i64 0)
store %struct.ScmObj* %k40518, %struct.ScmObj** %stackaddr$env-ref50239
%stackaddr$env-ref50240 = alloca %struct.ScmObj*, align 8
%n40150 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41230, i64 1)
store %struct.ScmObj* %n40150, %struct.ScmObj** %stackaddr$env-ref50240
%stackaddr$env-ref50241 = alloca %struct.ScmObj*, align 8
%lst40151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41230, i64 2)
store %struct.ScmObj* %lst40151, %struct.ScmObj** %stackaddr$env-ref50241
%stackaddr$env-ref50242 = alloca %struct.ScmObj*, align 8
%_37take40122 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41230, i64 3)
store %struct.ScmObj* %_37take40122, %struct.ScmObj** %stackaddr$env-ref50242
%stackaddr$prim50243 = alloca %struct.ScmObj*, align 8
%_95k40519 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48733)
store volatile %struct.ScmObj* %_95k40519, %struct.ScmObj** %stackaddr$prim50243, align 8
%stackaddr$prim50244 = alloca %struct.ScmObj*, align 8
%current_45args48734 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48733)
store volatile %struct.ScmObj* %current_45args48734, %struct.ScmObj** %stackaddr$prim50244, align 8
%stackaddr$prim50245 = alloca %struct.ScmObj*, align 8
%anf_45bind40279 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48734)
store volatile %struct.ScmObj* %anf_45bind40279, %struct.ScmObj** %stackaddr$prim50245, align 8
%stackaddr$prim50246 = alloca %struct.ScmObj*, align 8
%anf_45bind40280 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind40279, %struct.ScmObj* %n40150)
store volatile %struct.ScmObj* %anf_45bind40280, %struct.ScmObj** %stackaddr$prim50246, align 8
%args48736$_37take40122$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50247 = alloca %struct.ScmObj*, align 8
%args48736$_37take40122$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40280, %struct.ScmObj* %args48736$_37take40122$0)
store volatile %struct.ScmObj* %args48736$_37take40122$1, %struct.ScmObj** %stackaddr$prim50247, align 8
%stackaddr$prim50248 = alloca %struct.ScmObj*, align 8
%args48736$_37take40122$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40151, %struct.ScmObj* %args48736$_37take40122$1)
store volatile %struct.ScmObj* %args48736$_37take40122$2, %struct.ScmObj** %stackaddr$prim50248, align 8
%stackaddr$prim50249 = alloca %struct.ScmObj*, align 8
%args48736$_37take40122$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40518, %struct.ScmObj* %args48736$_37take40122$2)
store volatile %struct.ScmObj* %args48736$_37take40122$3, %struct.ScmObj** %stackaddr$prim50249, align 8
%clofunc50250 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take40122)
musttail call tailcc void %clofunc50250(%struct.ScmObj* %_37take40122, %struct.ScmObj* %args48736$_37take40122$3)
ret void
}

define tailcc void @proc_clo$ae41174(%struct.ScmObj* %env$ae41174,%struct.ScmObj* %current_45args48739) {
%stackaddr$env-ref50251 = alloca %struct.ScmObj*, align 8
%_37foldl140114 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41174, i64 0)
store %struct.ScmObj* %_37foldl140114, %struct.ScmObj** %stackaddr$env-ref50251
%stackaddr$prim50252 = alloca %struct.ScmObj*, align 8
%k40520 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48739)
store volatile %struct.ScmObj* %k40520, %struct.ScmObj** %stackaddr$prim50252, align 8
%stackaddr$prim50253 = alloca %struct.ScmObj*, align 8
%current_45args48740 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48739)
store volatile %struct.ScmObj* %current_45args48740, %struct.ScmObj** %stackaddr$prim50253, align 8
%stackaddr$prim50254 = alloca %struct.ScmObj*, align 8
%lst40153 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48740)
store volatile %struct.ScmObj* %lst40153, %struct.ScmObj** %stackaddr$prim50254, align 8
%stackaddr$makeclosure50255 = alloca %struct.ScmObj*, align 8
%fptrToInt50256 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41175 to i64
%ae41175 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50256)
store volatile %struct.ScmObj* %ae41175, %struct.ScmObj** %stackaddr$makeclosure50255, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41175, %struct.ScmObj* %lst40153, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41175, %struct.ScmObj* %k40520, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41175, %struct.ScmObj* %_37foldl140114, i64 2)
%ae41176 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50257 = alloca %struct.ScmObj*, align 8
%fptrToInt50258 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41177 to i64
%ae41177 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50258)
store volatile %struct.ScmObj* %ae41177, %struct.ScmObj** %stackaddr$makeclosure50257, align 8
%args48751$ae41175$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50259 = alloca %struct.ScmObj*, align 8
%args48751$ae41175$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41177, %struct.ScmObj* %args48751$ae41175$0)
store volatile %struct.ScmObj* %args48751$ae41175$1, %struct.ScmObj** %stackaddr$prim50259, align 8
%stackaddr$prim50260 = alloca %struct.ScmObj*, align 8
%args48751$ae41175$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41176, %struct.ScmObj* %args48751$ae41175$1)
store volatile %struct.ScmObj* %args48751$ae41175$2, %struct.ScmObj** %stackaddr$prim50260, align 8
%clofunc50261 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41175)
musttail call tailcc void %clofunc50261(%struct.ScmObj* %ae41175, %struct.ScmObj* %args48751$ae41175$2)
ret void
}

define tailcc void @proc_clo$ae41175(%struct.ScmObj* %env$ae41175,%struct.ScmObj* %current_45args48742) {
%stackaddr$env-ref50262 = alloca %struct.ScmObj*, align 8
%lst40153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41175, i64 0)
store %struct.ScmObj* %lst40153, %struct.ScmObj** %stackaddr$env-ref50262
%stackaddr$env-ref50263 = alloca %struct.ScmObj*, align 8
%k40520 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41175, i64 1)
store %struct.ScmObj* %k40520, %struct.ScmObj** %stackaddr$env-ref50263
%stackaddr$env-ref50264 = alloca %struct.ScmObj*, align 8
%_37foldl140114 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41175, i64 2)
store %struct.ScmObj* %_37foldl140114, %struct.ScmObj** %stackaddr$env-ref50264
%stackaddr$prim50265 = alloca %struct.ScmObj*, align 8
%_95k40521 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48742)
store volatile %struct.ScmObj* %_95k40521, %struct.ScmObj** %stackaddr$prim50265, align 8
%stackaddr$prim50266 = alloca %struct.ScmObj*, align 8
%current_45args48743 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48742)
store volatile %struct.ScmObj* %current_45args48743, %struct.ScmObj** %stackaddr$prim50266, align 8
%stackaddr$prim50267 = alloca %struct.ScmObj*, align 8
%anf_45bind40278 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48743)
store volatile %struct.ScmObj* %anf_45bind40278, %struct.ScmObj** %stackaddr$prim50267, align 8
%ae41196 = call %struct.ScmObj* @const_init_null()
%args48745$_37foldl140114$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50268 = alloca %struct.ScmObj*, align 8
%args48745$_37foldl140114$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40153, %struct.ScmObj* %args48745$_37foldl140114$0)
store volatile %struct.ScmObj* %args48745$_37foldl140114$1, %struct.ScmObj** %stackaddr$prim50268, align 8
%stackaddr$prim50269 = alloca %struct.ScmObj*, align 8
%args48745$_37foldl140114$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41196, %struct.ScmObj* %args48745$_37foldl140114$1)
store volatile %struct.ScmObj* %args48745$_37foldl140114$2, %struct.ScmObj** %stackaddr$prim50269, align 8
%stackaddr$prim50270 = alloca %struct.ScmObj*, align 8
%args48745$_37foldl140114$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40278, %struct.ScmObj* %args48745$_37foldl140114$2)
store volatile %struct.ScmObj* %args48745$_37foldl140114$3, %struct.ScmObj** %stackaddr$prim50270, align 8
%stackaddr$prim50271 = alloca %struct.ScmObj*, align 8
%args48745$_37foldl140114$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40520, %struct.ScmObj* %args48745$_37foldl140114$3)
store volatile %struct.ScmObj* %args48745$_37foldl140114$4, %struct.ScmObj** %stackaddr$prim50271, align 8
%clofunc50272 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140114)
musttail call tailcc void %clofunc50272(%struct.ScmObj* %_37foldl140114, %struct.ScmObj* %args48745$_37foldl140114$4)
ret void
}

define tailcc void @proc_clo$ae41177(%struct.ScmObj* %env$ae41177,%struct.ScmObj* %current_45args48746) {
%stackaddr$prim50273 = alloca %struct.ScmObj*, align 8
%k40522 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48746)
store volatile %struct.ScmObj* %k40522, %struct.ScmObj** %stackaddr$prim50273, align 8
%stackaddr$prim50274 = alloca %struct.ScmObj*, align 8
%current_45args48747 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48746)
store volatile %struct.ScmObj* %current_45args48747, %struct.ScmObj** %stackaddr$prim50274, align 8
%stackaddr$prim50275 = alloca %struct.ScmObj*, align 8
%x40155 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48747)
store volatile %struct.ScmObj* %x40155, %struct.ScmObj** %stackaddr$prim50275, align 8
%stackaddr$prim50276 = alloca %struct.ScmObj*, align 8
%current_45args48748 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48747)
store volatile %struct.ScmObj* %current_45args48748, %struct.ScmObj** %stackaddr$prim50276, align 8
%stackaddr$prim50277 = alloca %struct.ScmObj*, align 8
%y40154 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48748)
store volatile %struct.ScmObj* %y40154, %struct.ScmObj** %stackaddr$prim50277, align 8
%ae41179 = call %struct.ScmObj* @const_init_int(i64 0)
%args48750$k40522$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50278 = alloca %struct.ScmObj*, align 8
%args48750$k40522$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x40155, %struct.ScmObj* %args48750$k40522$0)
store volatile %struct.ScmObj* %args48750$k40522$1, %struct.ScmObj** %stackaddr$prim50278, align 8
%stackaddr$prim50279 = alloca %struct.ScmObj*, align 8
%args48750$k40522$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41179, %struct.ScmObj* %args48750$k40522$1)
store volatile %struct.ScmObj* %args48750$k40522$2, %struct.ScmObj** %stackaddr$prim50279, align 8
%clofunc50280 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40522)
musttail call tailcc void %clofunc50280(%struct.ScmObj* %k40522, %struct.ScmObj* %args48750$k40522$2)
ret void
}

define tailcc void @proc_clo$ae41095(%struct.ScmObj* %env$ae41095,%struct.ScmObj* %current_45args48754) {
%stackaddr$prim50281 = alloca %struct.ScmObj*, align 8
%k40523 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48754)
store volatile %struct.ScmObj* %k40523, %struct.ScmObj** %stackaddr$prim50281, align 8
%stackaddr$prim50282 = alloca %struct.ScmObj*, align 8
%current_45args48755 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48754)
store volatile %struct.ScmObj* %current_45args48755, %struct.ScmObj** %stackaddr$prim50282, align 8
%stackaddr$prim50283 = alloca %struct.ScmObj*, align 8
%_37foldl140115 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48755)
store volatile %struct.ScmObj* %_37foldl140115, %struct.ScmObj** %stackaddr$prim50283, align 8
%ae41097 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50284 = alloca %struct.ScmObj*, align 8
%fptrToInt50285 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41098 to i64
%ae41098 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50285)
store volatile %struct.ScmObj* %ae41098, %struct.ScmObj** %stackaddr$makeclosure50284, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41098, %struct.ScmObj* %_37foldl140115, i64 0)
%args48768$k40523$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50286 = alloca %struct.ScmObj*, align 8
%args48768$k40523$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41098, %struct.ScmObj* %args48768$k40523$0)
store volatile %struct.ScmObj* %args48768$k40523$1, %struct.ScmObj** %stackaddr$prim50286, align 8
%stackaddr$prim50287 = alloca %struct.ScmObj*, align 8
%args48768$k40523$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41097, %struct.ScmObj* %args48768$k40523$1)
store volatile %struct.ScmObj* %args48768$k40523$2, %struct.ScmObj** %stackaddr$prim50287, align 8
%clofunc50288 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40523)
musttail call tailcc void %clofunc50288(%struct.ScmObj* %k40523, %struct.ScmObj* %args48768$k40523$2)
ret void
}

define tailcc void @proc_clo$ae41098(%struct.ScmObj* %env$ae41098,%struct.ScmObj* %current_45args48757) {
%stackaddr$env-ref50289 = alloca %struct.ScmObj*, align 8
%_37foldl140115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41098, i64 0)
store %struct.ScmObj* %_37foldl140115, %struct.ScmObj** %stackaddr$env-ref50289
%stackaddr$prim50290 = alloca %struct.ScmObj*, align 8
%k40524 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48757)
store volatile %struct.ScmObj* %k40524, %struct.ScmObj** %stackaddr$prim50290, align 8
%stackaddr$prim50291 = alloca %struct.ScmObj*, align 8
%current_45args48758 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48757)
store volatile %struct.ScmObj* %current_45args48758, %struct.ScmObj** %stackaddr$prim50291, align 8
%stackaddr$prim50292 = alloca %struct.ScmObj*, align 8
%f40118 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48758)
store volatile %struct.ScmObj* %f40118, %struct.ScmObj** %stackaddr$prim50292, align 8
%stackaddr$prim50293 = alloca %struct.ScmObj*, align 8
%current_45args48759 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48758)
store volatile %struct.ScmObj* %current_45args48759, %struct.ScmObj** %stackaddr$prim50293, align 8
%stackaddr$prim50294 = alloca %struct.ScmObj*, align 8
%acc40117 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48759)
store volatile %struct.ScmObj* %acc40117, %struct.ScmObj** %stackaddr$prim50294, align 8
%stackaddr$prim50295 = alloca %struct.ScmObj*, align 8
%current_45args48760 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48759)
store volatile %struct.ScmObj* %current_45args48760, %struct.ScmObj** %stackaddr$prim50295, align 8
%stackaddr$prim50296 = alloca %struct.ScmObj*, align 8
%lst40116 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48760)
store volatile %struct.ScmObj* %lst40116, %struct.ScmObj** %stackaddr$prim50296, align 8
%stackaddr$prim50297 = alloca %struct.ScmObj*, align 8
%anf_45bind40273 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40116)
store volatile %struct.ScmObj* %anf_45bind40273, %struct.ScmObj** %stackaddr$prim50297, align 8
%truthy$cmp50298 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40273)
%cmp$cmp50298 = icmp eq i64 %truthy$cmp50298, 1
br i1 %cmp$cmp50298, label %truebranch$cmp50298, label %falsebranch$cmp50298
truebranch$cmp50298:
%ae41102 = call %struct.ScmObj* @const_init_int(i64 0)
%args48762$k40524$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50299 = alloca %struct.ScmObj*, align 8
%args48762$k40524$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40117, %struct.ScmObj* %args48762$k40524$0)
store volatile %struct.ScmObj* %args48762$k40524$1, %struct.ScmObj** %stackaddr$prim50299, align 8
%stackaddr$prim50300 = alloca %struct.ScmObj*, align 8
%args48762$k40524$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41102, %struct.ScmObj* %args48762$k40524$1)
store volatile %struct.ScmObj* %args48762$k40524$2, %struct.ScmObj** %stackaddr$prim50300, align 8
%clofunc50301 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40524)
musttail call tailcc void %clofunc50301(%struct.ScmObj* %k40524, %struct.ScmObj* %args48762$k40524$2)
ret void
falsebranch$cmp50298:
%stackaddr$prim50302 = alloca %struct.ScmObj*, align 8
%anf_45bind40274 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40116)
store volatile %struct.ScmObj* %anf_45bind40274, %struct.ScmObj** %stackaddr$prim50302, align 8
%stackaddr$makeclosure50303 = alloca %struct.ScmObj*, align 8
%fptrToInt50304 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41109 to i64
%ae41109 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt50304)
store volatile %struct.ScmObj* %ae41109, %struct.ScmObj** %stackaddr$makeclosure50303, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41109, %struct.ScmObj* %f40118, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41109, %struct.ScmObj* %lst40116, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41109, %struct.ScmObj* %_37foldl140115, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41109, %struct.ScmObj* %k40524, i64 3)
%args48767$f40118$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50305 = alloca %struct.ScmObj*, align 8
%args48767$f40118$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40117, %struct.ScmObj* %args48767$f40118$0)
store volatile %struct.ScmObj* %args48767$f40118$1, %struct.ScmObj** %stackaddr$prim50305, align 8
%stackaddr$prim50306 = alloca %struct.ScmObj*, align 8
%args48767$f40118$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40274, %struct.ScmObj* %args48767$f40118$1)
store volatile %struct.ScmObj* %args48767$f40118$2, %struct.ScmObj** %stackaddr$prim50306, align 8
%stackaddr$prim50307 = alloca %struct.ScmObj*, align 8
%args48767$f40118$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41109, %struct.ScmObj* %args48767$f40118$2)
store volatile %struct.ScmObj* %args48767$f40118$3, %struct.ScmObj** %stackaddr$prim50307, align 8
%clofunc50308 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40118)
musttail call tailcc void %clofunc50308(%struct.ScmObj* %f40118, %struct.ScmObj* %args48767$f40118$3)
ret void
}

define tailcc void @proc_clo$ae41109(%struct.ScmObj* %env$ae41109,%struct.ScmObj* %current_45args48763) {
%stackaddr$env-ref50309 = alloca %struct.ScmObj*, align 8
%f40118 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41109, i64 0)
store %struct.ScmObj* %f40118, %struct.ScmObj** %stackaddr$env-ref50309
%stackaddr$env-ref50310 = alloca %struct.ScmObj*, align 8
%lst40116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41109, i64 1)
store %struct.ScmObj* %lst40116, %struct.ScmObj** %stackaddr$env-ref50310
%stackaddr$env-ref50311 = alloca %struct.ScmObj*, align 8
%_37foldl140115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41109, i64 2)
store %struct.ScmObj* %_37foldl140115, %struct.ScmObj** %stackaddr$env-ref50311
%stackaddr$env-ref50312 = alloca %struct.ScmObj*, align 8
%k40524 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41109, i64 3)
store %struct.ScmObj* %k40524, %struct.ScmObj** %stackaddr$env-ref50312
%stackaddr$prim50313 = alloca %struct.ScmObj*, align 8
%_95k40525 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48763)
store volatile %struct.ScmObj* %_95k40525, %struct.ScmObj** %stackaddr$prim50313, align 8
%stackaddr$prim50314 = alloca %struct.ScmObj*, align 8
%current_45args48764 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48763)
store volatile %struct.ScmObj* %current_45args48764, %struct.ScmObj** %stackaddr$prim50314, align 8
%stackaddr$prim50315 = alloca %struct.ScmObj*, align 8
%anf_45bind40275 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48764)
store volatile %struct.ScmObj* %anf_45bind40275, %struct.ScmObj** %stackaddr$prim50315, align 8
%stackaddr$prim50316 = alloca %struct.ScmObj*, align 8
%anf_45bind40276 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40116)
store volatile %struct.ScmObj* %anf_45bind40276, %struct.ScmObj** %stackaddr$prim50316, align 8
%args48766$_37foldl140115$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50317 = alloca %struct.ScmObj*, align 8
%args48766$_37foldl140115$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40276, %struct.ScmObj* %args48766$_37foldl140115$0)
store volatile %struct.ScmObj* %args48766$_37foldl140115$1, %struct.ScmObj** %stackaddr$prim50317, align 8
%stackaddr$prim50318 = alloca %struct.ScmObj*, align 8
%args48766$_37foldl140115$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40275, %struct.ScmObj* %args48766$_37foldl140115$1)
store volatile %struct.ScmObj* %args48766$_37foldl140115$2, %struct.ScmObj** %stackaddr$prim50318, align 8
%stackaddr$prim50319 = alloca %struct.ScmObj*, align 8
%args48766$_37foldl140115$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40118, %struct.ScmObj* %args48766$_37foldl140115$2)
store volatile %struct.ScmObj* %args48766$_37foldl140115$3, %struct.ScmObj** %stackaddr$prim50319, align 8
%stackaddr$prim50320 = alloca %struct.ScmObj*, align 8
%args48766$_37foldl140115$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40524, %struct.ScmObj* %args48766$_37foldl140115$3)
store volatile %struct.ScmObj* %args48766$_37foldl140115$4, %struct.ScmObj** %stackaddr$prim50320, align 8
%clofunc50321 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140115)
musttail call tailcc void %clofunc50321(%struct.ScmObj* %_37foldl140115, %struct.ScmObj* %args48766$_37foldl140115$4)
ret void
}

define tailcc void @proc_clo$ae41012(%struct.ScmObj* %env$ae41012,%struct.ScmObj* %current_45args48771) {
%stackaddr$prim50322 = alloca %struct.ScmObj*, align 8
%k40526 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48771)
store volatile %struct.ScmObj* %k40526, %struct.ScmObj** %stackaddr$prim50322, align 8
%stackaddr$prim50323 = alloca %struct.ScmObj*, align 8
%current_45args48772 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48771)
store volatile %struct.ScmObj* %current_45args48772, %struct.ScmObj** %stackaddr$prim50323, align 8
%stackaddr$prim50324 = alloca %struct.ScmObj*, align 8
%_37length40120 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48772)
store volatile %struct.ScmObj* %_37length40120, %struct.ScmObj** %stackaddr$prim50324, align 8
%ae41014 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50325 = alloca %struct.ScmObj*, align 8
%fptrToInt50326 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41015 to i64
%ae41015 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50326)
store volatile %struct.ScmObj* %ae41015, %struct.ScmObj** %stackaddr$makeclosure50325, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41015, %struct.ScmObj* %_37length40120, i64 0)
%args48783$k40526$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50327 = alloca %struct.ScmObj*, align 8
%args48783$k40526$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41015, %struct.ScmObj* %args48783$k40526$0)
store volatile %struct.ScmObj* %args48783$k40526$1, %struct.ScmObj** %stackaddr$prim50327, align 8
%stackaddr$prim50328 = alloca %struct.ScmObj*, align 8
%args48783$k40526$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41014, %struct.ScmObj* %args48783$k40526$1)
store volatile %struct.ScmObj* %args48783$k40526$2, %struct.ScmObj** %stackaddr$prim50328, align 8
%clofunc50329 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40526)
musttail call tailcc void %clofunc50329(%struct.ScmObj* %k40526, %struct.ScmObj* %args48783$k40526$2)
ret void
}

define tailcc void @proc_clo$ae41015(%struct.ScmObj* %env$ae41015,%struct.ScmObj* %current_45args48774) {
%stackaddr$env-ref50330 = alloca %struct.ScmObj*, align 8
%_37length40120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41015, i64 0)
store %struct.ScmObj* %_37length40120, %struct.ScmObj** %stackaddr$env-ref50330
%stackaddr$prim50331 = alloca %struct.ScmObj*, align 8
%k40527 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48774)
store volatile %struct.ScmObj* %k40527, %struct.ScmObj** %stackaddr$prim50331, align 8
%stackaddr$prim50332 = alloca %struct.ScmObj*, align 8
%current_45args48775 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48774)
store volatile %struct.ScmObj* %current_45args48775, %struct.ScmObj** %stackaddr$prim50332, align 8
%stackaddr$prim50333 = alloca %struct.ScmObj*, align 8
%lst40121 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48775)
store volatile %struct.ScmObj* %lst40121, %struct.ScmObj** %stackaddr$prim50333, align 8
%stackaddr$prim50334 = alloca %struct.ScmObj*, align 8
%anf_45bind40269 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40121)
store volatile %struct.ScmObj* %anf_45bind40269, %struct.ScmObj** %stackaddr$prim50334, align 8
%truthy$cmp50335 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40269)
%cmp$cmp50335 = icmp eq i64 %truthy$cmp50335, 1
br i1 %cmp$cmp50335, label %truebranch$cmp50335, label %falsebranch$cmp50335
truebranch$cmp50335:
%ae41019 = call %struct.ScmObj* @const_init_int(i64 0)
%ae41020 = call %struct.ScmObj* @const_init_int(i64 0)
%args48777$k40527$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50336 = alloca %struct.ScmObj*, align 8
%args48777$k40527$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41020, %struct.ScmObj* %args48777$k40527$0)
store volatile %struct.ScmObj* %args48777$k40527$1, %struct.ScmObj** %stackaddr$prim50336, align 8
%stackaddr$prim50337 = alloca %struct.ScmObj*, align 8
%args48777$k40527$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41019, %struct.ScmObj* %args48777$k40527$1)
store volatile %struct.ScmObj* %args48777$k40527$2, %struct.ScmObj** %stackaddr$prim50337, align 8
%clofunc50338 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40527)
musttail call tailcc void %clofunc50338(%struct.ScmObj* %k40527, %struct.ScmObj* %args48777$k40527$2)
ret void
falsebranch$cmp50335:
%stackaddr$prim50339 = alloca %struct.ScmObj*, align 8
%anf_45bind40270 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40121)
store volatile %struct.ScmObj* %anf_45bind40270, %struct.ScmObj** %stackaddr$prim50339, align 8
%stackaddr$makeclosure50340 = alloca %struct.ScmObj*, align 8
%fptrToInt50341 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41029 to i64
%ae41029 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50341)
store volatile %struct.ScmObj* %ae41029, %struct.ScmObj** %stackaddr$makeclosure50340, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41029, %struct.ScmObj* %k40527, i64 0)
%args48782$_37length40120$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50342 = alloca %struct.ScmObj*, align 8
%args48782$_37length40120$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40270, %struct.ScmObj* %args48782$_37length40120$0)
store volatile %struct.ScmObj* %args48782$_37length40120$1, %struct.ScmObj** %stackaddr$prim50342, align 8
%stackaddr$prim50343 = alloca %struct.ScmObj*, align 8
%args48782$_37length40120$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41029, %struct.ScmObj* %args48782$_37length40120$1)
store volatile %struct.ScmObj* %args48782$_37length40120$2, %struct.ScmObj** %stackaddr$prim50343, align 8
%clofunc50344 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length40120)
musttail call tailcc void %clofunc50344(%struct.ScmObj* %_37length40120, %struct.ScmObj* %args48782$_37length40120$2)
ret void
}

define tailcc void @proc_clo$ae41029(%struct.ScmObj* %env$ae41029,%struct.ScmObj* %current_45args48778) {
%stackaddr$env-ref50345 = alloca %struct.ScmObj*, align 8
%k40527 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41029, i64 0)
store %struct.ScmObj* %k40527, %struct.ScmObj** %stackaddr$env-ref50345
%stackaddr$prim50346 = alloca %struct.ScmObj*, align 8
%_95k40528 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48778)
store volatile %struct.ScmObj* %_95k40528, %struct.ScmObj** %stackaddr$prim50346, align 8
%stackaddr$prim50347 = alloca %struct.ScmObj*, align 8
%current_45args48779 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48778)
store volatile %struct.ScmObj* %current_45args48779, %struct.ScmObj** %stackaddr$prim50347, align 8
%stackaddr$prim50348 = alloca %struct.ScmObj*, align 8
%anf_45bind40271 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48779)
store volatile %struct.ScmObj* %anf_45bind40271, %struct.ScmObj** %stackaddr$prim50348, align 8
%ae41031 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim50349 = alloca %struct.ScmObj*, align 8
%cpsprim40529 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae41031, %struct.ScmObj* %anf_45bind40271)
store volatile %struct.ScmObj* %cpsprim40529, %struct.ScmObj** %stackaddr$prim50349, align 8
%ae41034 = call %struct.ScmObj* @const_init_int(i64 0)
%args48781$k40527$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50350 = alloca %struct.ScmObj*, align 8
%args48781$k40527$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40529, %struct.ScmObj* %args48781$k40527$0)
store volatile %struct.ScmObj* %args48781$k40527$1, %struct.ScmObj** %stackaddr$prim50350, align 8
%stackaddr$prim50351 = alloca %struct.ScmObj*, align 8
%args48781$k40527$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41034, %struct.ScmObj* %args48781$k40527$1)
store volatile %struct.ScmObj* %args48781$k40527$2, %struct.ScmObj** %stackaddr$prim50351, align 8
%clofunc50352 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40527)
musttail call tailcc void %clofunc50352(%struct.ScmObj* %k40527, %struct.ScmObj* %args48781$k40527$2)
ret void
}

define tailcc void @proc_clo$ae40862(%struct.ScmObj* %env$ae40862,%struct.ScmObj* %current_45args48786) {
%stackaddr$prim50353 = alloca %struct.ScmObj*, align 8
%k40530 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48786)
store volatile %struct.ScmObj* %k40530, %struct.ScmObj** %stackaddr$prim50353, align 8
%stackaddr$prim50354 = alloca %struct.ScmObj*, align 8
%current_45args48787 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48786)
store volatile %struct.ScmObj* %current_45args48787, %struct.ScmObj** %stackaddr$prim50354, align 8
%stackaddr$prim50355 = alloca %struct.ScmObj*, align 8
%_37take40123 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48787)
store volatile %struct.ScmObj* %_37take40123, %struct.ScmObj** %stackaddr$prim50355, align 8
%ae40864 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50356 = alloca %struct.ScmObj*, align 8
%fptrToInt50357 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40865 to i64
%ae40865 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50357)
store volatile %struct.ScmObj* %ae40865, %struct.ScmObj** %stackaddr$makeclosure50356, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40865, %struct.ScmObj* %_37take40123, i64 0)
%args48800$k40530$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50358 = alloca %struct.ScmObj*, align 8
%args48800$k40530$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40865, %struct.ScmObj* %args48800$k40530$0)
store volatile %struct.ScmObj* %args48800$k40530$1, %struct.ScmObj** %stackaddr$prim50358, align 8
%stackaddr$prim50359 = alloca %struct.ScmObj*, align 8
%args48800$k40530$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40864, %struct.ScmObj* %args48800$k40530$1)
store volatile %struct.ScmObj* %args48800$k40530$2, %struct.ScmObj** %stackaddr$prim50359, align 8
%clofunc50360 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40530)
musttail call tailcc void %clofunc50360(%struct.ScmObj* %k40530, %struct.ScmObj* %args48800$k40530$2)
ret void
}

define tailcc void @proc_clo$ae40865(%struct.ScmObj* %env$ae40865,%struct.ScmObj* %current_45args48789) {
%stackaddr$env-ref50361 = alloca %struct.ScmObj*, align 8
%_37take40123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40865, i64 0)
store %struct.ScmObj* %_37take40123, %struct.ScmObj** %stackaddr$env-ref50361
%stackaddr$prim50362 = alloca %struct.ScmObj*, align 8
%k40531 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48789)
store volatile %struct.ScmObj* %k40531, %struct.ScmObj** %stackaddr$prim50362, align 8
%stackaddr$prim50363 = alloca %struct.ScmObj*, align 8
%current_45args48790 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48789)
store volatile %struct.ScmObj* %current_45args48790, %struct.ScmObj** %stackaddr$prim50363, align 8
%stackaddr$prim50364 = alloca %struct.ScmObj*, align 8
%lst40125 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48790)
store volatile %struct.ScmObj* %lst40125, %struct.ScmObj** %stackaddr$prim50364, align 8
%stackaddr$prim50365 = alloca %struct.ScmObj*, align 8
%current_45args48791 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48790)
store volatile %struct.ScmObj* %current_45args48791, %struct.ScmObj** %stackaddr$prim50365, align 8
%stackaddr$prim50366 = alloca %struct.ScmObj*, align 8
%n40124 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48791)
store volatile %struct.ScmObj* %n40124, %struct.ScmObj** %stackaddr$prim50366, align 8
%ae40867 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50367 = alloca %struct.ScmObj*, align 8
%anf_45bind40262 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n40124, %struct.ScmObj* %ae40867)
store volatile %struct.ScmObj* %anf_45bind40262, %struct.ScmObj** %stackaddr$prim50367, align 8
%truthy$cmp50368 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40262)
%cmp$cmp50368 = icmp eq i64 %truthy$cmp50368, 1
br i1 %cmp$cmp50368, label %truebranch$cmp50368, label %falsebranch$cmp50368
truebranch$cmp50368:
%ae40870 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40871 = call %struct.ScmObj* @const_init_null()
%args48793$k40531$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50369 = alloca %struct.ScmObj*, align 8
%args48793$k40531$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40871, %struct.ScmObj* %args48793$k40531$0)
store volatile %struct.ScmObj* %args48793$k40531$1, %struct.ScmObj** %stackaddr$prim50369, align 8
%stackaddr$prim50370 = alloca %struct.ScmObj*, align 8
%args48793$k40531$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40870, %struct.ScmObj* %args48793$k40531$1)
store volatile %struct.ScmObj* %args48793$k40531$2, %struct.ScmObj** %stackaddr$prim50370, align 8
%clofunc50371 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40531)
musttail call tailcc void %clofunc50371(%struct.ScmObj* %k40531, %struct.ScmObj* %args48793$k40531$2)
ret void
falsebranch$cmp50368:
%stackaddr$prim50372 = alloca %struct.ScmObj*, align 8
%anf_45bind40263 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40125)
store volatile %struct.ScmObj* %anf_45bind40263, %struct.ScmObj** %stackaddr$prim50372, align 8
%truthy$cmp50373 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40263)
%cmp$cmp50373 = icmp eq i64 %truthy$cmp50373, 1
br i1 %cmp$cmp50373, label %truebranch$cmp50373, label %falsebranch$cmp50373
truebranch$cmp50373:
%ae40881 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40882 = call %struct.ScmObj* @const_init_null()
%args48794$k40531$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50374 = alloca %struct.ScmObj*, align 8
%args48794$k40531$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40882, %struct.ScmObj* %args48794$k40531$0)
store volatile %struct.ScmObj* %args48794$k40531$1, %struct.ScmObj** %stackaddr$prim50374, align 8
%stackaddr$prim50375 = alloca %struct.ScmObj*, align 8
%args48794$k40531$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40881, %struct.ScmObj* %args48794$k40531$1)
store volatile %struct.ScmObj* %args48794$k40531$2, %struct.ScmObj** %stackaddr$prim50375, align 8
%clofunc50376 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40531)
musttail call tailcc void %clofunc50376(%struct.ScmObj* %k40531, %struct.ScmObj* %args48794$k40531$2)
ret void
falsebranch$cmp50373:
%stackaddr$prim50377 = alloca %struct.ScmObj*, align 8
%anf_45bind40264 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40125)
store volatile %struct.ScmObj* %anf_45bind40264, %struct.ScmObj** %stackaddr$prim50377, align 8
%stackaddr$prim50378 = alloca %struct.ScmObj*, align 8
%anf_45bind40265 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40125)
store volatile %struct.ScmObj* %anf_45bind40265, %struct.ScmObj** %stackaddr$prim50378, align 8
%ae40892 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim50379 = alloca %struct.ScmObj*, align 8
%anf_45bind40266 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n40124, %struct.ScmObj* %ae40892)
store volatile %struct.ScmObj* %anf_45bind40266, %struct.ScmObj** %stackaddr$prim50379, align 8
%stackaddr$makeclosure50380 = alloca %struct.ScmObj*, align 8
%fptrToInt50381 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40894 to i64
%ae40894 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt50381)
store volatile %struct.ScmObj* %ae40894, %struct.ScmObj** %stackaddr$makeclosure50380, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40894, %struct.ScmObj* %anf_45bind40264, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40894, %struct.ScmObj* %k40531, i64 1)
%args48799$_37take40123$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50382 = alloca %struct.ScmObj*, align 8
%args48799$_37take40123$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40266, %struct.ScmObj* %args48799$_37take40123$0)
store volatile %struct.ScmObj* %args48799$_37take40123$1, %struct.ScmObj** %stackaddr$prim50382, align 8
%stackaddr$prim50383 = alloca %struct.ScmObj*, align 8
%args48799$_37take40123$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40265, %struct.ScmObj* %args48799$_37take40123$1)
store volatile %struct.ScmObj* %args48799$_37take40123$2, %struct.ScmObj** %stackaddr$prim50383, align 8
%stackaddr$prim50384 = alloca %struct.ScmObj*, align 8
%args48799$_37take40123$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40894, %struct.ScmObj* %args48799$_37take40123$2)
store volatile %struct.ScmObj* %args48799$_37take40123$3, %struct.ScmObj** %stackaddr$prim50384, align 8
%clofunc50385 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take40123)
musttail call tailcc void %clofunc50385(%struct.ScmObj* %_37take40123, %struct.ScmObj* %args48799$_37take40123$3)
ret void
}

define tailcc void @proc_clo$ae40894(%struct.ScmObj* %env$ae40894,%struct.ScmObj* %current_45args48795) {
%stackaddr$env-ref50386 = alloca %struct.ScmObj*, align 8
%anf_45bind40264 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40894, i64 0)
store %struct.ScmObj* %anf_45bind40264, %struct.ScmObj** %stackaddr$env-ref50386
%stackaddr$env-ref50387 = alloca %struct.ScmObj*, align 8
%k40531 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40894, i64 1)
store %struct.ScmObj* %k40531, %struct.ScmObj** %stackaddr$env-ref50387
%stackaddr$prim50388 = alloca %struct.ScmObj*, align 8
%_95k40532 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48795)
store volatile %struct.ScmObj* %_95k40532, %struct.ScmObj** %stackaddr$prim50388, align 8
%stackaddr$prim50389 = alloca %struct.ScmObj*, align 8
%current_45args48796 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48795)
store volatile %struct.ScmObj* %current_45args48796, %struct.ScmObj** %stackaddr$prim50389, align 8
%stackaddr$prim50390 = alloca %struct.ScmObj*, align 8
%anf_45bind40267 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48796)
store volatile %struct.ScmObj* %anf_45bind40267, %struct.ScmObj** %stackaddr$prim50390, align 8
%stackaddr$prim50391 = alloca %struct.ScmObj*, align 8
%cpsprim40533 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40264, %struct.ScmObj* %anf_45bind40267)
store volatile %struct.ScmObj* %cpsprim40533, %struct.ScmObj** %stackaddr$prim50391, align 8
%ae40900 = call %struct.ScmObj* @const_init_int(i64 0)
%args48798$k40531$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50392 = alloca %struct.ScmObj*, align 8
%args48798$k40531$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40533, %struct.ScmObj* %args48798$k40531$0)
store volatile %struct.ScmObj* %args48798$k40531$1, %struct.ScmObj** %stackaddr$prim50392, align 8
%stackaddr$prim50393 = alloca %struct.ScmObj*, align 8
%args48798$k40531$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40900, %struct.ScmObj* %args48798$k40531$1)
store volatile %struct.ScmObj* %args48798$k40531$2, %struct.ScmObj** %stackaddr$prim50393, align 8
%clofunc50394 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40531)
musttail call tailcc void %clofunc50394(%struct.ScmObj* %k40531, %struct.ScmObj* %args48798$k40531$2)
ret void
}

define tailcc void @proc_clo$ae40765(%struct.ScmObj* %env$ae40765,%struct.ScmObj* %current_45args48803) {
%stackaddr$prim50395 = alloca %struct.ScmObj*, align 8
%k40534 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48803)
store volatile %struct.ScmObj* %k40534, %struct.ScmObj** %stackaddr$prim50395, align 8
%stackaddr$prim50396 = alloca %struct.ScmObj*, align 8
%current_45args48804 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48803)
store volatile %struct.ScmObj* %current_45args48804, %struct.ScmObj** %stackaddr$prim50396, align 8
%stackaddr$prim50397 = alloca %struct.ScmObj*, align 8
%_37map40127 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48804)
store volatile %struct.ScmObj* %_37map40127, %struct.ScmObj** %stackaddr$prim50397, align 8
%ae40767 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50398 = alloca %struct.ScmObj*, align 8
%fptrToInt50399 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40768 to i64
%ae40768 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50399)
store volatile %struct.ScmObj* %ae40768, %struct.ScmObj** %stackaddr$makeclosure50398, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40768, %struct.ScmObj* %_37map40127, i64 0)
%args48820$k40534$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50400 = alloca %struct.ScmObj*, align 8
%args48820$k40534$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40768, %struct.ScmObj* %args48820$k40534$0)
store volatile %struct.ScmObj* %args48820$k40534$1, %struct.ScmObj** %stackaddr$prim50400, align 8
%stackaddr$prim50401 = alloca %struct.ScmObj*, align 8
%args48820$k40534$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40767, %struct.ScmObj* %args48820$k40534$1)
store volatile %struct.ScmObj* %args48820$k40534$2, %struct.ScmObj** %stackaddr$prim50401, align 8
%clofunc50402 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40534)
musttail call tailcc void %clofunc50402(%struct.ScmObj* %k40534, %struct.ScmObj* %args48820$k40534$2)
ret void
}

define tailcc void @proc_clo$ae40768(%struct.ScmObj* %env$ae40768,%struct.ScmObj* %current_45args48806) {
%stackaddr$env-ref50403 = alloca %struct.ScmObj*, align 8
%_37map40127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40768, i64 0)
store %struct.ScmObj* %_37map40127, %struct.ScmObj** %stackaddr$env-ref50403
%stackaddr$prim50404 = alloca %struct.ScmObj*, align 8
%k40535 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48806)
store volatile %struct.ScmObj* %k40535, %struct.ScmObj** %stackaddr$prim50404, align 8
%stackaddr$prim50405 = alloca %struct.ScmObj*, align 8
%current_45args48807 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48806)
store volatile %struct.ScmObj* %current_45args48807, %struct.ScmObj** %stackaddr$prim50405, align 8
%stackaddr$prim50406 = alloca %struct.ScmObj*, align 8
%f40129 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48807)
store volatile %struct.ScmObj* %f40129, %struct.ScmObj** %stackaddr$prim50406, align 8
%stackaddr$prim50407 = alloca %struct.ScmObj*, align 8
%current_45args48808 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48807)
store volatile %struct.ScmObj* %current_45args48808, %struct.ScmObj** %stackaddr$prim50407, align 8
%stackaddr$prim50408 = alloca %struct.ScmObj*, align 8
%lst40128 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48808)
store volatile %struct.ScmObj* %lst40128, %struct.ScmObj** %stackaddr$prim50408, align 8
%stackaddr$prim50409 = alloca %struct.ScmObj*, align 8
%anf_45bind40256 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40128)
store volatile %struct.ScmObj* %anf_45bind40256, %struct.ScmObj** %stackaddr$prim50409, align 8
%truthy$cmp50410 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40256)
%cmp$cmp50410 = icmp eq i64 %truthy$cmp50410, 1
br i1 %cmp$cmp50410, label %truebranch$cmp50410, label %falsebranch$cmp50410
truebranch$cmp50410:
%ae40772 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40773 = call %struct.ScmObj* @const_init_null()
%args48810$k40535$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50411 = alloca %struct.ScmObj*, align 8
%args48810$k40535$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40773, %struct.ScmObj* %args48810$k40535$0)
store volatile %struct.ScmObj* %args48810$k40535$1, %struct.ScmObj** %stackaddr$prim50411, align 8
%stackaddr$prim50412 = alloca %struct.ScmObj*, align 8
%args48810$k40535$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40772, %struct.ScmObj* %args48810$k40535$1)
store volatile %struct.ScmObj* %args48810$k40535$2, %struct.ScmObj** %stackaddr$prim50412, align 8
%clofunc50413 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40535)
musttail call tailcc void %clofunc50413(%struct.ScmObj* %k40535, %struct.ScmObj* %args48810$k40535$2)
ret void
falsebranch$cmp50410:
%stackaddr$prim50414 = alloca %struct.ScmObj*, align 8
%anf_45bind40257 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40128)
store volatile %struct.ScmObj* %anf_45bind40257, %struct.ScmObj** %stackaddr$prim50414, align 8
%stackaddr$makeclosure50415 = alloca %struct.ScmObj*, align 8
%fptrToInt50416 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40782 to i64
%ae40782 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt50416)
store volatile %struct.ScmObj* %ae40782, %struct.ScmObj** %stackaddr$makeclosure50415, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40782, %struct.ScmObj* %k40535, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40782, %struct.ScmObj* %f40129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40782, %struct.ScmObj* %lst40128, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae40782, %struct.ScmObj* %_37map40127, i64 3)
%args48819$f40129$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50417 = alloca %struct.ScmObj*, align 8
%args48819$f40129$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40257, %struct.ScmObj* %args48819$f40129$0)
store volatile %struct.ScmObj* %args48819$f40129$1, %struct.ScmObj** %stackaddr$prim50417, align 8
%stackaddr$prim50418 = alloca %struct.ScmObj*, align 8
%args48819$f40129$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40782, %struct.ScmObj* %args48819$f40129$1)
store volatile %struct.ScmObj* %args48819$f40129$2, %struct.ScmObj** %stackaddr$prim50418, align 8
%clofunc50419 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40129)
musttail call tailcc void %clofunc50419(%struct.ScmObj* %f40129, %struct.ScmObj* %args48819$f40129$2)
ret void
}

define tailcc void @proc_clo$ae40782(%struct.ScmObj* %env$ae40782,%struct.ScmObj* %current_45args48811) {
%stackaddr$env-ref50420 = alloca %struct.ScmObj*, align 8
%k40535 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40782, i64 0)
store %struct.ScmObj* %k40535, %struct.ScmObj** %stackaddr$env-ref50420
%stackaddr$env-ref50421 = alloca %struct.ScmObj*, align 8
%f40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40782, i64 1)
store %struct.ScmObj* %f40129, %struct.ScmObj** %stackaddr$env-ref50421
%stackaddr$env-ref50422 = alloca %struct.ScmObj*, align 8
%lst40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40782, i64 2)
store %struct.ScmObj* %lst40128, %struct.ScmObj** %stackaddr$env-ref50422
%stackaddr$env-ref50423 = alloca %struct.ScmObj*, align 8
%_37map40127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40782, i64 3)
store %struct.ScmObj* %_37map40127, %struct.ScmObj** %stackaddr$env-ref50423
%stackaddr$prim50424 = alloca %struct.ScmObj*, align 8
%_95k40536 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48811)
store volatile %struct.ScmObj* %_95k40536, %struct.ScmObj** %stackaddr$prim50424, align 8
%stackaddr$prim50425 = alloca %struct.ScmObj*, align 8
%current_45args48812 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48811)
store volatile %struct.ScmObj* %current_45args48812, %struct.ScmObj** %stackaddr$prim50425, align 8
%stackaddr$prim50426 = alloca %struct.ScmObj*, align 8
%anf_45bind40258 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48812)
store volatile %struct.ScmObj* %anf_45bind40258, %struct.ScmObj** %stackaddr$prim50426, align 8
%stackaddr$prim50427 = alloca %struct.ScmObj*, align 8
%anf_45bind40259 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40128)
store volatile %struct.ScmObj* %anf_45bind40259, %struct.ScmObj** %stackaddr$prim50427, align 8
%stackaddr$makeclosure50428 = alloca %struct.ScmObj*, align 8
%fptrToInt50429 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40786 to i64
%ae40786 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt50429)
store volatile %struct.ScmObj* %ae40786, %struct.ScmObj** %stackaddr$makeclosure50428, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40786, %struct.ScmObj* %k40535, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40786, %struct.ScmObj* %anf_45bind40258, i64 1)
%args48818$_37map40127$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50430 = alloca %struct.ScmObj*, align 8
%args48818$_37map40127$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40259, %struct.ScmObj* %args48818$_37map40127$0)
store volatile %struct.ScmObj* %args48818$_37map40127$1, %struct.ScmObj** %stackaddr$prim50430, align 8
%stackaddr$prim50431 = alloca %struct.ScmObj*, align 8
%args48818$_37map40127$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40129, %struct.ScmObj* %args48818$_37map40127$1)
store volatile %struct.ScmObj* %args48818$_37map40127$2, %struct.ScmObj** %stackaddr$prim50431, align 8
%stackaddr$prim50432 = alloca %struct.ScmObj*, align 8
%args48818$_37map40127$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40786, %struct.ScmObj* %args48818$_37map40127$2)
store volatile %struct.ScmObj* %args48818$_37map40127$3, %struct.ScmObj** %stackaddr$prim50432, align 8
%clofunc50433 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map40127)
musttail call tailcc void %clofunc50433(%struct.ScmObj* %_37map40127, %struct.ScmObj* %args48818$_37map40127$3)
ret void
}

define tailcc void @proc_clo$ae40786(%struct.ScmObj* %env$ae40786,%struct.ScmObj* %current_45args48814) {
%stackaddr$env-ref50434 = alloca %struct.ScmObj*, align 8
%k40535 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40786, i64 0)
store %struct.ScmObj* %k40535, %struct.ScmObj** %stackaddr$env-ref50434
%stackaddr$env-ref50435 = alloca %struct.ScmObj*, align 8
%anf_45bind40258 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40786, i64 1)
store %struct.ScmObj* %anf_45bind40258, %struct.ScmObj** %stackaddr$env-ref50435
%stackaddr$prim50436 = alloca %struct.ScmObj*, align 8
%_95k40537 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48814)
store volatile %struct.ScmObj* %_95k40537, %struct.ScmObj** %stackaddr$prim50436, align 8
%stackaddr$prim50437 = alloca %struct.ScmObj*, align 8
%current_45args48815 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48814)
store volatile %struct.ScmObj* %current_45args48815, %struct.ScmObj** %stackaddr$prim50437, align 8
%stackaddr$prim50438 = alloca %struct.ScmObj*, align 8
%anf_45bind40260 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48815)
store volatile %struct.ScmObj* %anf_45bind40260, %struct.ScmObj** %stackaddr$prim50438, align 8
%stackaddr$prim50439 = alloca %struct.ScmObj*, align 8
%cpsprim40538 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40258, %struct.ScmObj* %anf_45bind40260)
store volatile %struct.ScmObj* %cpsprim40538, %struct.ScmObj** %stackaddr$prim50439, align 8
%ae40792 = call %struct.ScmObj* @const_init_int(i64 0)
%args48817$k40535$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50440 = alloca %struct.ScmObj*, align 8
%args48817$k40535$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40538, %struct.ScmObj* %args48817$k40535$0)
store volatile %struct.ScmObj* %args48817$k40535$1, %struct.ScmObj** %stackaddr$prim50440, align 8
%stackaddr$prim50441 = alloca %struct.ScmObj*, align 8
%args48817$k40535$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40792, %struct.ScmObj* %args48817$k40535$1)
store volatile %struct.ScmObj* %args48817$k40535$2, %struct.ScmObj** %stackaddr$prim50441, align 8
%clofunc50442 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40535)
musttail call tailcc void %clofunc50442(%struct.ScmObj* %k40535, %struct.ScmObj* %args48817$k40535$2)
ret void
}

define tailcc void @proc_clo$ae40685(%struct.ScmObj* %env$ae40685,%struct.ScmObj* %current_45args48823) {
%stackaddr$prim50443 = alloca %struct.ScmObj*, align 8
%k40539 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48823)
store volatile %struct.ScmObj* %k40539, %struct.ScmObj** %stackaddr$prim50443, align 8
%stackaddr$prim50444 = alloca %struct.ScmObj*, align 8
%current_45args48824 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48823)
store volatile %struct.ScmObj* %current_45args48824, %struct.ScmObj** %stackaddr$prim50444, align 8
%stackaddr$prim50445 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48824)
store volatile %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$prim50445, align 8
%ae40687 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50446 = alloca %struct.ScmObj*, align 8
%fptrToInt50447 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40688 to i64
%ae40688 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50447)
store volatile %struct.ScmObj* %ae40688, %struct.ScmObj** %stackaddr$makeclosure50446, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40688, %struct.ScmObj* %_37foldr140131, i64 0)
%args48837$k40539$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50448 = alloca %struct.ScmObj*, align 8
%args48837$k40539$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40688, %struct.ScmObj* %args48837$k40539$0)
store volatile %struct.ScmObj* %args48837$k40539$1, %struct.ScmObj** %stackaddr$prim50448, align 8
%stackaddr$prim50449 = alloca %struct.ScmObj*, align 8
%args48837$k40539$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40687, %struct.ScmObj* %args48837$k40539$1)
store volatile %struct.ScmObj* %args48837$k40539$2, %struct.ScmObj** %stackaddr$prim50449, align 8
%clofunc50450 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40539)
musttail call tailcc void %clofunc50450(%struct.ScmObj* %k40539, %struct.ScmObj* %args48837$k40539$2)
ret void
}

define tailcc void @proc_clo$ae40688(%struct.ScmObj* %env$ae40688,%struct.ScmObj* %current_45args48826) {
%stackaddr$env-ref50451 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40688, i64 0)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref50451
%stackaddr$prim50452 = alloca %struct.ScmObj*, align 8
%k40540 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48826)
store volatile %struct.ScmObj* %k40540, %struct.ScmObj** %stackaddr$prim50452, align 8
%stackaddr$prim50453 = alloca %struct.ScmObj*, align 8
%current_45args48827 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48826)
store volatile %struct.ScmObj* %current_45args48827, %struct.ScmObj** %stackaddr$prim50453, align 8
%stackaddr$prim50454 = alloca %struct.ScmObj*, align 8
%f40134 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48827)
store volatile %struct.ScmObj* %f40134, %struct.ScmObj** %stackaddr$prim50454, align 8
%stackaddr$prim50455 = alloca %struct.ScmObj*, align 8
%current_45args48828 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48827)
store volatile %struct.ScmObj* %current_45args48828, %struct.ScmObj** %stackaddr$prim50455, align 8
%stackaddr$prim50456 = alloca %struct.ScmObj*, align 8
%acc40133 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48828)
store volatile %struct.ScmObj* %acc40133, %struct.ScmObj** %stackaddr$prim50456, align 8
%stackaddr$prim50457 = alloca %struct.ScmObj*, align 8
%current_45args48829 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48828)
store volatile %struct.ScmObj* %current_45args48829, %struct.ScmObj** %stackaddr$prim50457, align 8
%stackaddr$prim50458 = alloca %struct.ScmObj*, align 8
%lst40132 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48829)
store volatile %struct.ScmObj* %lst40132, %struct.ScmObj** %stackaddr$prim50458, align 8
%stackaddr$prim50459 = alloca %struct.ScmObj*, align 8
%anf_45bind40251 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40132)
store volatile %struct.ScmObj* %anf_45bind40251, %struct.ScmObj** %stackaddr$prim50459, align 8
%truthy$cmp50460 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40251)
%cmp$cmp50460 = icmp eq i64 %truthy$cmp50460, 1
br i1 %cmp$cmp50460, label %truebranch$cmp50460, label %falsebranch$cmp50460
truebranch$cmp50460:
%ae40692 = call %struct.ScmObj* @const_init_int(i64 0)
%args48831$k40540$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50461 = alloca %struct.ScmObj*, align 8
%args48831$k40540$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40133, %struct.ScmObj* %args48831$k40540$0)
store volatile %struct.ScmObj* %args48831$k40540$1, %struct.ScmObj** %stackaddr$prim50461, align 8
%stackaddr$prim50462 = alloca %struct.ScmObj*, align 8
%args48831$k40540$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40692, %struct.ScmObj* %args48831$k40540$1)
store volatile %struct.ScmObj* %args48831$k40540$2, %struct.ScmObj** %stackaddr$prim50462, align 8
%clofunc50463 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40540)
musttail call tailcc void %clofunc50463(%struct.ScmObj* %k40540, %struct.ScmObj* %args48831$k40540$2)
ret void
falsebranch$cmp50460:
%stackaddr$prim50464 = alloca %struct.ScmObj*, align 8
%anf_45bind40252 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40132)
store volatile %struct.ScmObj* %anf_45bind40252, %struct.ScmObj** %stackaddr$prim50464, align 8
%stackaddr$prim50465 = alloca %struct.ScmObj*, align 8
%anf_45bind40253 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40132)
store volatile %struct.ScmObj* %anf_45bind40253, %struct.ScmObj** %stackaddr$prim50465, align 8
%stackaddr$makeclosure50466 = alloca %struct.ScmObj*, align 8
%fptrToInt50467 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40700 to i64
%ae40700 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50467)
store volatile %struct.ScmObj* %ae40700, %struct.ScmObj** %stackaddr$makeclosure50466, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40700, %struct.ScmObj* %k40540, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40700, %struct.ScmObj* %anf_45bind40252, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40700, %struct.ScmObj* %f40134, i64 2)
%args48836$_37foldr140131$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50468 = alloca %struct.ScmObj*, align 8
%args48836$_37foldr140131$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40253, %struct.ScmObj* %args48836$_37foldr140131$0)
store volatile %struct.ScmObj* %args48836$_37foldr140131$1, %struct.ScmObj** %stackaddr$prim50468, align 8
%stackaddr$prim50469 = alloca %struct.ScmObj*, align 8
%args48836$_37foldr140131$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40133, %struct.ScmObj* %args48836$_37foldr140131$1)
store volatile %struct.ScmObj* %args48836$_37foldr140131$2, %struct.ScmObj** %stackaddr$prim50469, align 8
%stackaddr$prim50470 = alloca %struct.ScmObj*, align 8
%args48836$_37foldr140131$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40134, %struct.ScmObj* %args48836$_37foldr140131$2)
store volatile %struct.ScmObj* %args48836$_37foldr140131$3, %struct.ScmObj** %stackaddr$prim50470, align 8
%stackaddr$prim50471 = alloca %struct.ScmObj*, align 8
%args48836$_37foldr140131$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40700, %struct.ScmObj* %args48836$_37foldr140131$3)
store volatile %struct.ScmObj* %args48836$_37foldr140131$4, %struct.ScmObj** %stackaddr$prim50471, align 8
%clofunc50472 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140131)
musttail call tailcc void %clofunc50472(%struct.ScmObj* %_37foldr140131, %struct.ScmObj* %args48836$_37foldr140131$4)
ret void
}

define tailcc void @proc_clo$ae40700(%struct.ScmObj* %env$ae40700,%struct.ScmObj* %current_45args48832) {
%stackaddr$env-ref50473 = alloca %struct.ScmObj*, align 8
%k40540 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40700, i64 0)
store %struct.ScmObj* %k40540, %struct.ScmObj** %stackaddr$env-ref50473
%stackaddr$env-ref50474 = alloca %struct.ScmObj*, align 8
%anf_45bind40252 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40700, i64 1)
store %struct.ScmObj* %anf_45bind40252, %struct.ScmObj** %stackaddr$env-ref50474
%stackaddr$env-ref50475 = alloca %struct.ScmObj*, align 8
%f40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40700, i64 2)
store %struct.ScmObj* %f40134, %struct.ScmObj** %stackaddr$env-ref50475
%stackaddr$prim50476 = alloca %struct.ScmObj*, align 8
%_95k40541 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48832)
store volatile %struct.ScmObj* %_95k40541, %struct.ScmObj** %stackaddr$prim50476, align 8
%stackaddr$prim50477 = alloca %struct.ScmObj*, align 8
%current_45args48833 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48832)
store volatile %struct.ScmObj* %current_45args48833, %struct.ScmObj** %stackaddr$prim50477, align 8
%stackaddr$prim50478 = alloca %struct.ScmObj*, align 8
%anf_45bind40254 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48833)
store volatile %struct.ScmObj* %anf_45bind40254, %struct.ScmObj** %stackaddr$prim50478, align 8
%args48835$f40134$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50479 = alloca %struct.ScmObj*, align 8
%args48835$f40134$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40254, %struct.ScmObj* %args48835$f40134$0)
store volatile %struct.ScmObj* %args48835$f40134$1, %struct.ScmObj** %stackaddr$prim50479, align 8
%stackaddr$prim50480 = alloca %struct.ScmObj*, align 8
%args48835$f40134$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40252, %struct.ScmObj* %args48835$f40134$1)
store volatile %struct.ScmObj* %args48835$f40134$2, %struct.ScmObj** %stackaddr$prim50480, align 8
%stackaddr$prim50481 = alloca %struct.ScmObj*, align 8
%args48835$f40134$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40540, %struct.ScmObj* %args48835$f40134$2)
store volatile %struct.ScmObj* %args48835$f40134$3, %struct.ScmObj** %stackaddr$prim50481, align 8
%clofunc50482 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40134)
musttail call tailcc void %clofunc50482(%struct.ScmObj* %f40134, %struct.ScmObj* %args48835$f40134$3)
ret void
}

define tailcc void @proc_clo$ae40568(%struct.ScmObj* %env$ae40568,%struct.ScmObj* %current_45args48840) {
%stackaddr$prim50483 = alloca %struct.ScmObj*, align 8
%k40542 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48840)
store volatile %struct.ScmObj* %k40542, %struct.ScmObj** %stackaddr$prim50483, align 8
%stackaddr$prim50484 = alloca %struct.ScmObj*, align 8
%current_45args48841 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48840)
store volatile %struct.ScmObj* %current_45args48841, %struct.ScmObj** %stackaddr$prim50484, align 8
%stackaddr$prim50485 = alloca %struct.ScmObj*, align 8
%y40111 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48841)
store volatile %struct.ScmObj* %y40111, %struct.ScmObj** %stackaddr$prim50485, align 8
%ae40570 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50486 = alloca %struct.ScmObj*, align 8
%fptrToInt50487 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40571 to i64
%ae40571 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50487)
store volatile %struct.ScmObj* %ae40571, %struct.ScmObj** %stackaddr$makeclosure50486, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40571, %struct.ScmObj* %y40111, i64 0)
%args48859$k40542$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50488 = alloca %struct.ScmObj*, align 8
%args48859$k40542$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40571, %struct.ScmObj* %args48859$k40542$0)
store volatile %struct.ScmObj* %args48859$k40542$1, %struct.ScmObj** %stackaddr$prim50488, align 8
%stackaddr$prim50489 = alloca %struct.ScmObj*, align 8
%args48859$k40542$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40570, %struct.ScmObj* %args48859$k40542$1)
store volatile %struct.ScmObj* %args48859$k40542$2, %struct.ScmObj** %stackaddr$prim50489, align 8
%clofunc50490 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40542)
musttail call tailcc void %clofunc50490(%struct.ScmObj* %k40542, %struct.ScmObj* %args48859$k40542$2)
ret void
}

define tailcc void @proc_clo$ae40571(%struct.ScmObj* %env$ae40571,%struct.ScmObj* %current_45args48843) {
%stackaddr$env-ref50491 = alloca %struct.ScmObj*, align 8
%y40111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40571, i64 0)
store %struct.ScmObj* %y40111, %struct.ScmObj** %stackaddr$env-ref50491
%stackaddr$prim50492 = alloca %struct.ScmObj*, align 8
%k40543 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48843)
store volatile %struct.ScmObj* %k40543, %struct.ScmObj** %stackaddr$prim50492, align 8
%stackaddr$prim50493 = alloca %struct.ScmObj*, align 8
%current_45args48844 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48843)
store volatile %struct.ScmObj* %current_45args48844, %struct.ScmObj** %stackaddr$prim50493, align 8
%stackaddr$prim50494 = alloca %struct.ScmObj*, align 8
%f40112 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48844)
store volatile %struct.ScmObj* %f40112, %struct.ScmObj** %stackaddr$prim50494, align 8
%stackaddr$makeclosure50495 = alloca %struct.ScmObj*, align 8
%fptrToInt50496 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40572 to i64
%ae40572 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt50496)
store volatile %struct.ScmObj* %ae40572, %struct.ScmObj** %stackaddr$makeclosure50495, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40572, %struct.ScmObj* %f40112, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40572, %struct.ScmObj* %k40543, i64 1)
%ae40573 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50497 = alloca %struct.ScmObj*, align 8
%fptrToInt50498 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40574 to i64
%ae40574 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt50498)
store volatile %struct.ScmObj* %ae40574, %struct.ScmObj** %stackaddr$makeclosure50497, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40574, %struct.ScmObj* %f40112, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40574, %struct.ScmObj* %y40111, i64 1)
%args48858$ae40572$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50499 = alloca %struct.ScmObj*, align 8
%args48858$ae40572$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40574, %struct.ScmObj* %args48858$ae40572$0)
store volatile %struct.ScmObj* %args48858$ae40572$1, %struct.ScmObj** %stackaddr$prim50499, align 8
%stackaddr$prim50500 = alloca %struct.ScmObj*, align 8
%args48858$ae40572$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40573, %struct.ScmObj* %args48858$ae40572$1)
store volatile %struct.ScmObj* %args48858$ae40572$2, %struct.ScmObj** %stackaddr$prim50500, align 8
%clofunc50501 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40572)
musttail call tailcc void %clofunc50501(%struct.ScmObj* %ae40572, %struct.ScmObj* %args48858$ae40572$2)
ret void
}

define tailcc void @proc_clo$ae40572(%struct.ScmObj* %env$ae40572,%struct.ScmObj* %current_45args48846) {
%stackaddr$env-ref50502 = alloca %struct.ScmObj*, align 8
%f40112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40572, i64 0)
store %struct.ScmObj* %f40112, %struct.ScmObj** %stackaddr$env-ref50502
%stackaddr$env-ref50503 = alloca %struct.ScmObj*, align 8
%k40543 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40572, i64 1)
store %struct.ScmObj* %k40543, %struct.ScmObj** %stackaddr$env-ref50503
%stackaddr$prim50504 = alloca %struct.ScmObj*, align 8
%_95k40544 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48846)
store volatile %struct.ScmObj* %_95k40544, %struct.ScmObj** %stackaddr$prim50504, align 8
%stackaddr$prim50505 = alloca %struct.ScmObj*, align 8
%current_45args48847 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48846)
store volatile %struct.ScmObj* %current_45args48847, %struct.ScmObj** %stackaddr$prim50505, align 8
%stackaddr$prim50506 = alloca %struct.ScmObj*, align 8
%anf_45bind40249 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48847)
store volatile %struct.ScmObj* %anf_45bind40249, %struct.ScmObj** %stackaddr$prim50506, align 8
%args48849$f40112$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50507 = alloca %struct.ScmObj*, align 8
%args48849$f40112$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40249, %struct.ScmObj* %args48849$f40112$0)
store volatile %struct.ScmObj* %args48849$f40112$1, %struct.ScmObj** %stackaddr$prim50507, align 8
%stackaddr$prim50508 = alloca %struct.ScmObj*, align 8
%args48849$f40112$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40543, %struct.ScmObj* %args48849$f40112$1)
store volatile %struct.ScmObj* %args48849$f40112$2, %struct.ScmObj** %stackaddr$prim50508, align 8
%clofunc50509 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40112)
musttail call tailcc void %clofunc50509(%struct.ScmObj* %f40112, %struct.ScmObj* %args48849$f40112$2)
ret void
}

define tailcc void @proc_clo$ae40574(%struct.ScmObj* %env$ae40574,%struct.ScmObj* %args4011340545) {
%stackaddr$env-ref50510 = alloca %struct.ScmObj*, align 8
%f40112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40574, i64 0)
store %struct.ScmObj* %f40112, %struct.ScmObj** %stackaddr$env-ref50510
%stackaddr$env-ref50511 = alloca %struct.ScmObj*, align 8
%y40111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40574, i64 1)
store %struct.ScmObj* %y40111, %struct.ScmObj** %stackaddr$env-ref50511
%stackaddr$prim50512 = alloca %struct.ScmObj*, align 8
%k40546 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4011340545)
store volatile %struct.ScmObj* %k40546, %struct.ScmObj** %stackaddr$prim50512, align 8
%stackaddr$prim50513 = alloca %struct.ScmObj*, align 8
%args40113 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4011340545)
store volatile %struct.ScmObj* %args40113, %struct.ScmObj** %stackaddr$prim50513, align 8
%stackaddr$makeclosure50514 = alloca %struct.ScmObj*, align 8
%fptrToInt50515 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40578 to i64
%ae40578 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50515)
store volatile %struct.ScmObj* %ae40578, %struct.ScmObj** %stackaddr$makeclosure50514, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40578, %struct.ScmObj* %k40546, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40578, %struct.ScmObj* %args40113, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40578, %struct.ScmObj* %f40112, i64 2)
%args48857$y40111$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50516 = alloca %struct.ScmObj*, align 8
%args48857$y40111$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y40111, %struct.ScmObj* %args48857$y40111$0)
store volatile %struct.ScmObj* %args48857$y40111$1, %struct.ScmObj** %stackaddr$prim50516, align 8
%stackaddr$prim50517 = alloca %struct.ScmObj*, align 8
%args48857$y40111$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40578, %struct.ScmObj* %args48857$y40111$1)
store volatile %struct.ScmObj* %args48857$y40111$2, %struct.ScmObj** %stackaddr$prim50517, align 8
%clofunc50518 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y40111)
musttail call tailcc void %clofunc50518(%struct.ScmObj* %y40111, %struct.ScmObj* %args48857$y40111$2)
ret void
}

define tailcc void @proc_clo$ae40578(%struct.ScmObj* %env$ae40578,%struct.ScmObj* %current_45args48850) {
%stackaddr$env-ref50519 = alloca %struct.ScmObj*, align 8
%k40546 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40578, i64 0)
store %struct.ScmObj* %k40546, %struct.ScmObj** %stackaddr$env-ref50519
%stackaddr$env-ref50520 = alloca %struct.ScmObj*, align 8
%args40113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40578, i64 1)
store %struct.ScmObj* %args40113, %struct.ScmObj** %stackaddr$env-ref50520
%stackaddr$env-ref50521 = alloca %struct.ScmObj*, align 8
%f40112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40578, i64 2)
store %struct.ScmObj* %f40112, %struct.ScmObj** %stackaddr$env-ref50521
%stackaddr$prim50522 = alloca %struct.ScmObj*, align 8
%_95k40547 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48850)
store volatile %struct.ScmObj* %_95k40547, %struct.ScmObj** %stackaddr$prim50522, align 8
%stackaddr$prim50523 = alloca %struct.ScmObj*, align 8
%current_45args48851 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48850)
store volatile %struct.ScmObj* %current_45args48851, %struct.ScmObj** %stackaddr$prim50523, align 8
%stackaddr$prim50524 = alloca %struct.ScmObj*, align 8
%anf_45bind40247 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48851)
store volatile %struct.ScmObj* %anf_45bind40247, %struct.ScmObj** %stackaddr$prim50524, align 8
%stackaddr$makeclosure50525 = alloca %struct.ScmObj*, align 8
%fptrToInt50526 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40581 to i64
%ae40581 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt50526)
store volatile %struct.ScmObj* %ae40581, %struct.ScmObj** %stackaddr$makeclosure50525, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40581, %struct.ScmObj* %k40546, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40581, %struct.ScmObj* %args40113, i64 1)
%args48856$anf_45bind40247$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50527 = alloca %struct.ScmObj*, align 8
%args48856$anf_45bind40247$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40112, %struct.ScmObj* %args48856$anf_45bind40247$0)
store volatile %struct.ScmObj* %args48856$anf_45bind40247$1, %struct.ScmObj** %stackaddr$prim50527, align 8
%stackaddr$prim50528 = alloca %struct.ScmObj*, align 8
%args48856$anf_45bind40247$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40581, %struct.ScmObj* %args48856$anf_45bind40247$1)
store volatile %struct.ScmObj* %args48856$anf_45bind40247$2, %struct.ScmObj** %stackaddr$prim50528, align 8
%clofunc50529 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40247)
musttail call tailcc void %clofunc50529(%struct.ScmObj* %anf_45bind40247, %struct.ScmObj* %args48856$anf_45bind40247$2)
ret void
}

define tailcc void @proc_clo$ae40581(%struct.ScmObj* %env$ae40581,%struct.ScmObj* %current_45args48853) {
%stackaddr$env-ref50530 = alloca %struct.ScmObj*, align 8
%k40546 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40581, i64 0)
store %struct.ScmObj* %k40546, %struct.ScmObj** %stackaddr$env-ref50530
%stackaddr$env-ref50531 = alloca %struct.ScmObj*, align 8
%args40113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40581, i64 1)
store %struct.ScmObj* %args40113, %struct.ScmObj** %stackaddr$env-ref50531
%stackaddr$prim50532 = alloca %struct.ScmObj*, align 8
%_95k40548 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48853)
store volatile %struct.ScmObj* %_95k40548, %struct.ScmObj** %stackaddr$prim50532, align 8
%stackaddr$prim50533 = alloca %struct.ScmObj*, align 8
%current_45args48854 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48853)
store volatile %struct.ScmObj* %current_45args48854, %struct.ScmObj** %stackaddr$prim50533, align 8
%stackaddr$prim50534 = alloca %struct.ScmObj*, align 8
%anf_45bind40248 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48854)
store volatile %struct.ScmObj* %anf_45bind40248, %struct.ScmObj** %stackaddr$prim50534, align 8
%stackaddr$prim50535 = alloca %struct.ScmObj*, align 8
%cpsargs40549 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40546, %struct.ScmObj* %args40113)
store volatile %struct.ScmObj* %cpsargs40549, %struct.ScmObj** %stackaddr$prim50535, align 8
%clofunc50536 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40248)
musttail call tailcc void %clofunc50536(%struct.ScmObj* %anf_45bind40248, %struct.ScmObj* %cpsargs40549)
ret void
}

define tailcc void @proc_clo$ae40553(%struct.ScmObj* %env$ae40553,%struct.ScmObj* %current_45args48861) {
%stackaddr$prim50537 = alloca %struct.ScmObj*, align 8
%k40550 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48861)
store volatile %struct.ScmObj* %k40550, %struct.ScmObj** %stackaddr$prim50537, align 8
%stackaddr$prim50538 = alloca %struct.ScmObj*, align 8
%current_45args48862 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48861)
store volatile %struct.ScmObj* %current_45args48862, %struct.ScmObj** %stackaddr$prim50538, align 8
%stackaddr$prim50539 = alloca %struct.ScmObj*, align 8
%yu40110 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48862)
store volatile %struct.ScmObj* %yu40110, %struct.ScmObj** %stackaddr$prim50539, align 8
%args48864$yu40110$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50540 = alloca %struct.ScmObj*, align 8
%args48864$yu40110$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu40110, %struct.ScmObj* %args48864$yu40110$0)
store volatile %struct.ScmObj* %args48864$yu40110$1, %struct.ScmObj** %stackaddr$prim50540, align 8
%stackaddr$prim50541 = alloca %struct.ScmObj*, align 8
%args48864$yu40110$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40550, %struct.ScmObj* %args48864$yu40110$1)
store volatile %struct.ScmObj* %args48864$yu40110$2, %struct.ScmObj** %stackaddr$prim50541, align 8
%clofunc50542 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu40110)
musttail call tailcc void %clofunc50542(%struct.ScmObj* %yu40110, %struct.ScmObj* %args48864$yu40110$2)
ret void
}