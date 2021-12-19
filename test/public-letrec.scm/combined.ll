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
%mainenv56575 = call %struct.ScmObj* @const_init_null()
%mainargs56576 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv56575, %struct.ScmObj* %mainargs56576)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv56573,%struct.ScmObj* %mainargs56574) {
%stackaddr$makeclosure56577 = alloca %struct.ScmObj*, align 8
%fptrToInt56578 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48465 to i64
%ae48465 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56578)
store volatile %struct.ScmObj* %ae48465, %struct.ScmObj** %stackaddr$makeclosure56577, align 8
%ae48466 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56579 = alloca %struct.ScmObj*, align 8
%fptrToInt56580 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48467 to i64
%ae48467 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56580)
store volatile %struct.ScmObj* %ae48467, %struct.ScmObj** %stackaddr$makeclosure56579, align 8
%argslist56572$ae484650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56581 = alloca %struct.ScmObj*, align 8
%argslist56572$ae484651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48467, %struct.ScmObj* %argslist56572$ae484650)
store volatile %struct.ScmObj* %argslist56572$ae484651, %struct.ScmObj** %stackaddr$prim56581, align 8
%stackaddr$prim56582 = alloca %struct.ScmObj*, align 8
%argslist56572$ae484652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48466, %struct.ScmObj* %argslist56572$ae484651)
store volatile %struct.ScmObj* %argslist56572$ae484652, %struct.ScmObj** %stackaddr$prim56582, align 8
%clofunc56583 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48465)
musttail call tailcc void %clofunc56583(%struct.ScmObj* %ae48465, %struct.ScmObj* %argslist56572$ae484652)
ret void
}

define tailcc void @proc_clo$ae48465(%struct.ScmObj* %env$ae48465,%struct.ScmObj* %current_45args56046) {
%stackaddr$prim56584 = alloca %struct.ScmObj*, align 8
%_95k48304 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56046)
store volatile %struct.ScmObj* %_95k48304, %struct.ScmObj** %stackaddr$prim56584, align 8
%stackaddr$prim56585 = alloca %struct.ScmObj*, align 8
%current_45args56047 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56046)
store volatile %struct.ScmObj* %current_45args56047, %struct.ScmObj** %stackaddr$prim56585, align 8
%stackaddr$prim56586 = alloca %struct.ScmObj*, align 8
%anf_45bind48167 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56047)
store volatile %struct.ScmObj* %anf_45bind48167, %struct.ScmObj** %stackaddr$prim56586, align 8
%stackaddr$makeclosure56587 = alloca %struct.ScmObj*, align 8
%fptrToInt56588 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48480 to i64
%ae48480 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56588)
store volatile %struct.ScmObj* %ae48480, %struct.ScmObj** %stackaddr$makeclosure56587, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48480, %struct.ScmObj* %anf_45bind48167, i64 0)
%ae48481 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56589 = alloca %struct.ScmObj*, align 8
%fptrToInt56590 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48482 to i64
%ae48482 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56590)
store volatile %struct.ScmObj* %ae48482, %struct.ScmObj** %stackaddr$makeclosure56589, align 8
%argslist56567$ae484800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56591 = alloca %struct.ScmObj*, align 8
%argslist56567$ae484801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48482, %struct.ScmObj* %argslist56567$ae484800)
store volatile %struct.ScmObj* %argslist56567$ae484801, %struct.ScmObj** %stackaddr$prim56591, align 8
%stackaddr$prim56592 = alloca %struct.ScmObj*, align 8
%argslist56567$ae484802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48481, %struct.ScmObj* %argslist56567$ae484801)
store volatile %struct.ScmObj* %argslist56567$ae484802, %struct.ScmObj** %stackaddr$prim56592, align 8
%clofunc56593 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48480)
musttail call tailcc void %clofunc56593(%struct.ScmObj* %ae48480, %struct.ScmObj* %argslist56567$ae484802)
ret void
}

define tailcc void @proc_clo$ae48480(%struct.ScmObj* %env$ae48480,%struct.ScmObj* %current_45args56049) {
%stackaddr$env-ref56594 = alloca %struct.ScmObj*, align 8
%anf_45bind48167 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48480, i64 0)
store %struct.ScmObj* %anf_45bind48167, %struct.ScmObj** %stackaddr$env-ref56594
%stackaddr$prim56595 = alloca %struct.ScmObj*, align 8
%_95k48305 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56049)
store volatile %struct.ScmObj* %_95k48305, %struct.ScmObj** %stackaddr$prim56595, align 8
%stackaddr$prim56596 = alloca %struct.ScmObj*, align 8
%current_45args56050 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56049)
store volatile %struct.ScmObj* %current_45args56050, %struct.ScmObj** %stackaddr$prim56596, align 8
%stackaddr$prim56597 = alloca %struct.ScmObj*, align 8
%anf_45bind48171 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56050)
store volatile %struct.ScmObj* %anf_45bind48171, %struct.ScmObj** %stackaddr$prim56597, align 8
%stackaddr$makeclosure56598 = alloca %struct.ScmObj*, align 8
%fptrToInt56599 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48595 to i64
%ae48595 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56599)
store volatile %struct.ScmObj* %ae48595, %struct.ScmObj** %stackaddr$makeclosure56598, align 8
%argslist56546$anf_45bind481670 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56600 = alloca %struct.ScmObj*, align 8
%argslist56546$anf_45bind481671 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48171, %struct.ScmObj* %argslist56546$anf_45bind481670)
store volatile %struct.ScmObj* %argslist56546$anf_45bind481671, %struct.ScmObj** %stackaddr$prim56600, align 8
%stackaddr$prim56601 = alloca %struct.ScmObj*, align 8
%argslist56546$anf_45bind481672 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48595, %struct.ScmObj* %argslist56546$anf_45bind481671)
store volatile %struct.ScmObj* %argslist56546$anf_45bind481672, %struct.ScmObj** %stackaddr$prim56601, align 8
%clofunc56602 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48167)
musttail call tailcc void %clofunc56602(%struct.ScmObj* %anf_45bind48167, %struct.ScmObj* %argslist56546$anf_45bind481672)
ret void
}

define tailcc void @proc_clo$ae48595(%struct.ScmObj* %env$ae48595,%struct.ScmObj* %current_45args56052) {
%stackaddr$prim56603 = alloca %struct.ScmObj*, align 8
%_95k48306 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56052)
store volatile %struct.ScmObj* %_95k48306, %struct.ScmObj** %stackaddr$prim56603, align 8
%stackaddr$prim56604 = alloca %struct.ScmObj*, align 8
%current_45args56053 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56052)
store volatile %struct.ScmObj* %current_45args56053, %struct.ScmObj** %stackaddr$prim56604, align 8
%stackaddr$prim56605 = alloca %struct.ScmObj*, align 8
%Ycmb48032 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56053)
store volatile %struct.ScmObj* %Ycmb48032, %struct.ScmObj** %stackaddr$prim56605, align 8
%stackaddr$makeclosure56606 = alloca %struct.ScmObj*, align 8
%fptrToInt56607 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48597 to i64
%ae48597 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56607)
store volatile %struct.ScmObj* %ae48597, %struct.ScmObj** %stackaddr$makeclosure56606, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48597, %struct.ScmObj* %Ycmb48032, i64 0)
%ae48598 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56608 = alloca %struct.ScmObj*, align 8
%fptrToInt56609 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48599 to i64
%ae48599 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56609)
store volatile %struct.ScmObj* %ae48599, %struct.ScmObj** %stackaddr$makeclosure56608, align 8
%argslist56545$ae485970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56610 = alloca %struct.ScmObj*, align 8
%argslist56545$ae485971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48599, %struct.ScmObj* %argslist56545$ae485970)
store volatile %struct.ScmObj* %argslist56545$ae485971, %struct.ScmObj** %stackaddr$prim56610, align 8
%stackaddr$prim56611 = alloca %struct.ScmObj*, align 8
%argslist56545$ae485972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48598, %struct.ScmObj* %argslist56545$ae485971)
store volatile %struct.ScmObj* %argslist56545$ae485972, %struct.ScmObj** %stackaddr$prim56611, align 8
%clofunc56612 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48597)
musttail call tailcc void %clofunc56612(%struct.ScmObj* %ae48597, %struct.ScmObj* %argslist56545$ae485972)
ret void
}

define tailcc void @proc_clo$ae48597(%struct.ScmObj* %env$ae48597,%struct.ScmObj* %current_45args56055) {
%stackaddr$env-ref56613 = alloca %struct.ScmObj*, align 8
%Ycmb48032 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48597, i64 0)
store %struct.ScmObj* %Ycmb48032, %struct.ScmObj** %stackaddr$env-ref56613
%stackaddr$prim56614 = alloca %struct.ScmObj*, align 8
%_95k48307 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56055)
store volatile %struct.ScmObj* %_95k48307, %struct.ScmObj** %stackaddr$prim56614, align 8
%stackaddr$prim56615 = alloca %struct.ScmObj*, align 8
%current_45args56056 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56055)
store volatile %struct.ScmObj* %current_45args56056, %struct.ScmObj** %stackaddr$prim56615, align 8
%stackaddr$prim56616 = alloca %struct.ScmObj*, align 8
%anf_45bind48176 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56056)
store volatile %struct.ScmObj* %anf_45bind48176, %struct.ScmObj** %stackaddr$prim56616, align 8
%stackaddr$makeclosure56617 = alloca %struct.ScmObj*, align 8
%fptrToInt56618 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48675 to i64
%ae48675 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56618)
store volatile %struct.ScmObj* %ae48675, %struct.ScmObj** %stackaddr$makeclosure56617, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48675, %struct.ScmObj* %Ycmb48032, i64 0)
%argslist56529$Ycmb480320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56619 = alloca %struct.ScmObj*, align 8
%argslist56529$Ycmb480321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48176, %struct.ScmObj* %argslist56529$Ycmb480320)
store volatile %struct.ScmObj* %argslist56529$Ycmb480321, %struct.ScmObj** %stackaddr$prim56619, align 8
%stackaddr$prim56620 = alloca %struct.ScmObj*, align 8
%argslist56529$Ycmb480322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48675, %struct.ScmObj* %argslist56529$Ycmb480321)
store volatile %struct.ScmObj* %argslist56529$Ycmb480322, %struct.ScmObj** %stackaddr$prim56620, align 8
%clofunc56621 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48032)
musttail call tailcc void %clofunc56621(%struct.ScmObj* %Ycmb48032, %struct.ScmObj* %argslist56529$Ycmb480322)
ret void
}

define tailcc void @proc_clo$ae48675(%struct.ScmObj* %env$ae48675,%struct.ScmObj* %current_45args56058) {
%stackaddr$env-ref56622 = alloca %struct.ScmObj*, align 8
%Ycmb48032 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48675, i64 0)
store %struct.ScmObj* %Ycmb48032, %struct.ScmObj** %stackaddr$env-ref56622
%stackaddr$prim56623 = alloca %struct.ScmObj*, align 8
%_95k48308 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56058)
store volatile %struct.ScmObj* %_95k48308, %struct.ScmObj** %stackaddr$prim56623, align 8
%stackaddr$prim56624 = alloca %struct.ScmObj*, align 8
%current_45args56059 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56058)
store volatile %struct.ScmObj* %current_45args56059, %struct.ScmObj** %stackaddr$prim56624, align 8
%stackaddr$prim56625 = alloca %struct.ScmObj*, align 8
%_37foldr148053 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56059)
store volatile %struct.ScmObj* %_37foldr148053, %struct.ScmObj** %stackaddr$prim56625, align 8
%stackaddr$makeclosure56626 = alloca %struct.ScmObj*, align 8
%fptrToInt56627 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48677 to i64
%ae48677 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56627)
store volatile %struct.ScmObj* %ae48677, %struct.ScmObj** %stackaddr$makeclosure56626, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48677, %struct.ScmObj* %_37foldr148053, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48677, %struct.ScmObj* %Ycmb48032, i64 1)
%ae48678 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56628 = alloca %struct.ScmObj*, align 8
%fptrToInt56629 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48679 to i64
%ae48679 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56629)
store volatile %struct.ScmObj* %ae48679, %struct.ScmObj** %stackaddr$makeclosure56628, align 8
%argslist56528$ae486770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56630 = alloca %struct.ScmObj*, align 8
%argslist56528$ae486771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48679, %struct.ScmObj* %argslist56528$ae486770)
store volatile %struct.ScmObj* %argslist56528$ae486771, %struct.ScmObj** %stackaddr$prim56630, align 8
%stackaddr$prim56631 = alloca %struct.ScmObj*, align 8
%argslist56528$ae486772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48678, %struct.ScmObj* %argslist56528$ae486771)
store volatile %struct.ScmObj* %argslist56528$ae486772, %struct.ScmObj** %stackaddr$prim56631, align 8
%clofunc56632 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48677)
musttail call tailcc void %clofunc56632(%struct.ScmObj* %ae48677, %struct.ScmObj* %argslist56528$ae486772)
ret void
}

define tailcc void @proc_clo$ae48677(%struct.ScmObj* %env$ae48677,%struct.ScmObj* %current_45args56061) {
%stackaddr$env-ref56633 = alloca %struct.ScmObj*, align 8
%_37foldr148053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48677, i64 0)
store %struct.ScmObj* %_37foldr148053, %struct.ScmObj** %stackaddr$env-ref56633
%stackaddr$env-ref56634 = alloca %struct.ScmObj*, align 8
%Ycmb48032 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48677, i64 1)
store %struct.ScmObj* %Ycmb48032, %struct.ScmObj** %stackaddr$env-ref56634
%stackaddr$prim56635 = alloca %struct.ScmObj*, align 8
%_95k48309 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56061)
store volatile %struct.ScmObj* %_95k48309, %struct.ScmObj** %stackaddr$prim56635, align 8
%stackaddr$prim56636 = alloca %struct.ScmObj*, align 8
%current_45args56062 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56061)
store volatile %struct.ScmObj* %current_45args56062, %struct.ScmObj** %stackaddr$prim56636, align 8
%stackaddr$prim56637 = alloca %struct.ScmObj*, align 8
%anf_45bind48182 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56062)
store volatile %struct.ScmObj* %anf_45bind48182, %struct.ScmObj** %stackaddr$prim56637, align 8
%stackaddr$makeclosure56638 = alloca %struct.ScmObj*, align 8
%fptrToInt56639 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48772 to i64
%ae48772 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56639)
store volatile %struct.ScmObj* %ae48772, %struct.ScmObj** %stackaddr$makeclosure56638, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48772, %struct.ScmObj* %_37foldr148053, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48772, %struct.ScmObj* %Ycmb48032, i64 1)
%argslist56509$Ycmb480320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56640 = alloca %struct.ScmObj*, align 8
%argslist56509$Ycmb480321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48182, %struct.ScmObj* %argslist56509$Ycmb480320)
store volatile %struct.ScmObj* %argslist56509$Ycmb480321, %struct.ScmObj** %stackaddr$prim56640, align 8
%stackaddr$prim56641 = alloca %struct.ScmObj*, align 8
%argslist56509$Ycmb480322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48772, %struct.ScmObj* %argslist56509$Ycmb480321)
store volatile %struct.ScmObj* %argslist56509$Ycmb480322, %struct.ScmObj** %stackaddr$prim56641, align 8
%clofunc56642 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48032)
musttail call tailcc void %clofunc56642(%struct.ScmObj* %Ycmb48032, %struct.ScmObj* %argslist56509$Ycmb480322)
ret void
}

define tailcc void @proc_clo$ae48772(%struct.ScmObj* %env$ae48772,%struct.ScmObj* %current_45args56064) {
%stackaddr$env-ref56643 = alloca %struct.ScmObj*, align 8
%_37foldr148053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48772, i64 0)
store %struct.ScmObj* %_37foldr148053, %struct.ScmObj** %stackaddr$env-ref56643
%stackaddr$env-ref56644 = alloca %struct.ScmObj*, align 8
%Ycmb48032 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48772, i64 1)
store %struct.ScmObj* %Ycmb48032, %struct.ScmObj** %stackaddr$env-ref56644
%stackaddr$prim56645 = alloca %struct.ScmObj*, align 8
%_95k48310 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56064)
store volatile %struct.ScmObj* %_95k48310, %struct.ScmObj** %stackaddr$prim56645, align 8
%stackaddr$prim56646 = alloca %struct.ScmObj*, align 8
%current_45args56065 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56064)
store volatile %struct.ScmObj* %current_45args56065, %struct.ScmObj** %stackaddr$prim56646, align 8
%stackaddr$prim56647 = alloca %struct.ScmObj*, align 8
%_37map148049 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56065)
store volatile %struct.ScmObj* %_37map148049, %struct.ScmObj** %stackaddr$prim56647, align 8
%stackaddr$makeclosure56648 = alloca %struct.ScmObj*, align 8
%fptrToInt56649 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48774 to i64
%ae48774 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56649)
store volatile %struct.ScmObj* %ae48774, %struct.ScmObj** %stackaddr$makeclosure56648, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48774, %struct.ScmObj* %_37foldr148053, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48774, %struct.ScmObj* %_37map148049, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48774, %struct.ScmObj* %Ycmb48032, i64 2)
%ae48775 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56650 = alloca %struct.ScmObj*, align 8
%fptrToInt56651 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48776 to i64
%ae48776 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56651)
store volatile %struct.ScmObj* %ae48776, %struct.ScmObj** %stackaddr$makeclosure56650, align 8
%argslist56508$ae487740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56652 = alloca %struct.ScmObj*, align 8
%argslist56508$ae487741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48776, %struct.ScmObj* %argslist56508$ae487740)
store volatile %struct.ScmObj* %argslist56508$ae487741, %struct.ScmObj** %stackaddr$prim56652, align 8
%stackaddr$prim56653 = alloca %struct.ScmObj*, align 8
%argslist56508$ae487742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48775, %struct.ScmObj* %argslist56508$ae487741)
store volatile %struct.ScmObj* %argslist56508$ae487742, %struct.ScmObj** %stackaddr$prim56653, align 8
%clofunc56654 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48774)
musttail call tailcc void %clofunc56654(%struct.ScmObj* %ae48774, %struct.ScmObj* %argslist56508$ae487742)
ret void
}

define tailcc void @proc_clo$ae48774(%struct.ScmObj* %env$ae48774,%struct.ScmObj* %current_45args56067) {
%stackaddr$env-ref56655 = alloca %struct.ScmObj*, align 8
%_37foldr148053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48774, i64 0)
store %struct.ScmObj* %_37foldr148053, %struct.ScmObj** %stackaddr$env-ref56655
%stackaddr$env-ref56656 = alloca %struct.ScmObj*, align 8
%_37map148049 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48774, i64 1)
store %struct.ScmObj* %_37map148049, %struct.ScmObj** %stackaddr$env-ref56656
%stackaddr$env-ref56657 = alloca %struct.ScmObj*, align 8
%Ycmb48032 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48774, i64 2)
store %struct.ScmObj* %Ycmb48032, %struct.ScmObj** %stackaddr$env-ref56657
%stackaddr$prim56658 = alloca %struct.ScmObj*, align 8
%_95k48311 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56067)
store volatile %struct.ScmObj* %_95k48311, %struct.ScmObj** %stackaddr$prim56658, align 8
%stackaddr$prim56659 = alloca %struct.ScmObj*, align 8
%current_45args56068 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56067)
store volatile %struct.ScmObj* %current_45args56068, %struct.ScmObj** %stackaddr$prim56659, align 8
%stackaddr$prim56660 = alloca %struct.ScmObj*, align 8
%anf_45bind48189 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56068)
store volatile %struct.ScmObj* %anf_45bind48189, %struct.ScmObj** %stackaddr$prim56660, align 8
%stackaddr$makeclosure56661 = alloca %struct.ScmObj*, align 8
%fptrToInt56662 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48922 to i64
%ae48922 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56662)
store volatile %struct.ScmObj* %ae48922, %struct.ScmObj** %stackaddr$makeclosure56661, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48922, %struct.ScmObj* %_37foldr148053, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48922, %struct.ScmObj* %_37map148049, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48922, %struct.ScmObj* %Ycmb48032, i64 2)
%argslist56492$Ycmb480320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56663 = alloca %struct.ScmObj*, align 8
%argslist56492$Ycmb480321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48189, %struct.ScmObj* %argslist56492$Ycmb480320)
store volatile %struct.ScmObj* %argslist56492$Ycmb480321, %struct.ScmObj** %stackaddr$prim56663, align 8
%stackaddr$prim56664 = alloca %struct.ScmObj*, align 8
%argslist56492$Ycmb480322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48922, %struct.ScmObj* %argslist56492$Ycmb480321)
store volatile %struct.ScmObj* %argslist56492$Ycmb480322, %struct.ScmObj** %stackaddr$prim56664, align 8
%clofunc56665 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48032)
musttail call tailcc void %clofunc56665(%struct.ScmObj* %Ycmb48032, %struct.ScmObj* %argslist56492$Ycmb480322)
ret void
}

define tailcc void @proc_clo$ae48922(%struct.ScmObj* %env$ae48922,%struct.ScmObj* %current_45args56070) {
%stackaddr$env-ref56666 = alloca %struct.ScmObj*, align 8
%_37foldr148053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48922, i64 0)
store %struct.ScmObj* %_37foldr148053, %struct.ScmObj** %stackaddr$env-ref56666
%stackaddr$env-ref56667 = alloca %struct.ScmObj*, align 8
%_37map148049 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48922, i64 1)
store %struct.ScmObj* %_37map148049, %struct.ScmObj** %stackaddr$env-ref56667
%stackaddr$env-ref56668 = alloca %struct.ScmObj*, align 8
%Ycmb48032 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48922, i64 2)
store %struct.ScmObj* %Ycmb48032, %struct.ScmObj** %stackaddr$env-ref56668
%stackaddr$prim56669 = alloca %struct.ScmObj*, align 8
%_95k48312 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56070)
store volatile %struct.ScmObj* %_95k48312, %struct.ScmObj** %stackaddr$prim56669, align 8
%stackaddr$prim56670 = alloca %struct.ScmObj*, align 8
%current_45args56071 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56070)
store volatile %struct.ScmObj* %current_45args56071, %struct.ScmObj** %stackaddr$prim56670, align 8
%stackaddr$prim56671 = alloca %struct.ScmObj*, align 8
%_37take48045 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56071)
store volatile %struct.ScmObj* %_37take48045, %struct.ScmObj** %stackaddr$prim56671, align 8
%stackaddr$makeclosure56672 = alloca %struct.ScmObj*, align 8
%fptrToInt56673 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48924 to i64
%ae48924 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56673)
store volatile %struct.ScmObj* %ae48924, %struct.ScmObj** %stackaddr$makeclosure56672, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48924, %struct.ScmObj* %_37foldr148053, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48924, %struct.ScmObj* %_37map148049, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48924, %struct.ScmObj* %Ycmb48032, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48924, %struct.ScmObj* %_37take48045, i64 3)
%ae48925 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56674 = alloca %struct.ScmObj*, align 8
%fptrToInt56675 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48926 to i64
%ae48926 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56675)
store volatile %struct.ScmObj* %ae48926, %struct.ScmObj** %stackaddr$makeclosure56674, align 8
%argslist56491$ae489240 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56676 = alloca %struct.ScmObj*, align 8
%argslist56491$ae489241 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48926, %struct.ScmObj* %argslist56491$ae489240)
store volatile %struct.ScmObj* %argslist56491$ae489241, %struct.ScmObj** %stackaddr$prim56676, align 8
%stackaddr$prim56677 = alloca %struct.ScmObj*, align 8
%argslist56491$ae489242 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48925, %struct.ScmObj* %argslist56491$ae489241)
store volatile %struct.ScmObj* %argslist56491$ae489242, %struct.ScmObj** %stackaddr$prim56677, align 8
%clofunc56678 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48924)
musttail call tailcc void %clofunc56678(%struct.ScmObj* %ae48924, %struct.ScmObj* %argslist56491$ae489242)
ret void
}

define tailcc void @proc_clo$ae48924(%struct.ScmObj* %env$ae48924,%struct.ScmObj* %current_45args56073) {
%stackaddr$env-ref56679 = alloca %struct.ScmObj*, align 8
%_37foldr148053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48924, i64 0)
store %struct.ScmObj* %_37foldr148053, %struct.ScmObj** %stackaddr$env-ref56679
%stackaddr$env-ref56680 = alloca %struct.ScmObj*, align 8
%_37map148049 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48924, i64 1)
store %struct.ScmObj* %_37map148049, %struct.ScmObj** %stackaddr$env-ref56680
%stackaddr$env-ref56681 = alloca %struct.ScmObj*, align 8
%Ycmb48032 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48924, i64 2)
store %struct.ScmObj* %Ycmb48032, %struct.ScmObj** %stackaddr$env-ref56681
%stackaddr$env-ref56682 = alloca %struct.ScmObj*, align 8
%_37take48045 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48924, i64 3)
store %struct.ScmObj* %_37take48045, %struct.ScmObj** %stackaddr$env-ref56682
%stackaddr$prim56683 = alloca %struct.ScmObj*, align 8
%_95k48313 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56073)
store volatile %struct.ScmObj* %_95k48313, %struct.ScmObj** %stackaddr$prim56683, align 8
%stackaddr$prim56684 = alloca %struct.ScmObj*, align 8
%current_45args56074 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56073)
store volatile %struct.ScmObj* %current_45args56074, %struct.ScmObj** %stackaddr$prim56684, align 8
%stackaddr$prim56685 = alloca %struct.ScmObj*, align 8
%anf_45bind48193 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56074)
store volatile %struct.ScmObj* %anf_45bind48193, %struct.ScmObj** %stackaddr$prim56685, align 8
%stackaddr$makeclosure56686 = alloca %struct.ScmObj*, align 8
%fptrToInt56687 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49005 to i64
%ae49005 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56687)
store volatile %struct.ScmObj* %ae49005, %struct.ScmObj** %stackaddr$makeclosure56686, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49005, %struct.ScmObj* %_37foldr148053, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49005, %struct.ScmObj* %_37map148049, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49005, %struct.ScmObj* %Ycmb48032, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49005, %struct.ScmObj* %_37take48045, i64 3)
%argslist56477$Ycmb480320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56688 = alloca %struct.ScmObj*, align 8
%argslist56477$Ycmb480321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48193, %struct.ScmObj* %argslist56477$Ycmb480320)
store volatile %struct.ScmObj* %argslist56477$Ycmb480321, %struct.ScmObj** %stackaddr$prim56688, align 8
%stackaddr$prim56689 = alloca %struct.ScmObj*, align 8
%argslist56477$Ycmb480322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49005, %struct.ScmObj* %argslist56477$Ycmb480321)
store volatile %struct.ScmObj* %argslist56477$Ycmb480322, %struct.ScmObj** %stackaddr$prim56689, align 8
%clofunc56690 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48032)
musttail call tailcc void %clofunc56690(%struct.ScmObj* %Ycmb48032, %struct.ScmObj* %argslist56477$Ycmb480322)
ret void
}

define tailcc void @proc_clo$ae49005(%struct.ScmObj* %env$ae49005,%struct.ScmObj* %current_45args56076) {
%stackaddr$env-ref56691 = alloca %struct.ScmObj*, align 8
%_37foldr148053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49005, i64 0)
store %struct.ScmObj* %_37foldr148053, %struct.ScmObj** %stackaddr$env-ref56691
%stackaddr$env-ref56692 = alloca %struct.ScmObj*, align 8
%_37map148049 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49005, i64 1)
store %struct.ScmObj* %_37map148049, %struct.ScmObj** %stackaddr$env-ref56692
%stackaddr$env-ref56693 = alloca %struct.ScmObj*, align 8
%Ycmb48032 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49005, i64 2)
store %struct.ScmObj* %Ycmb48032, %struct.ScmObj** %stackaddr$env-ref56693
%stackaddr$env-ref56694 = alloca %struct.ScmObj*, align 8
%_37take48045 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49005, i64 3)
store %struct.ScmObj* %_37take48045, %struct.ScmObj** %stackaddr$env-ref56694
%stackaddr$prim56695 = alloca %struct.ScmObj*, align 8
%_95k48314 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56076)
store volatile %struct.ScmObj* %_95k48314, %struct.ScmObj** %stackaddr$prim56695, align 8
%stackaddr$prim56696 = alloca %struct.ScmObj*, align 8
%current_45args56077 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56076)
store volatile %struct.ScmObj* %current_45args56077, %struct.ScmObj** %stackaddr$prim56696, align 8
%stackaddr$prim56697 = alloca %struct.ScmObj*, align 8
%_37length48042 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56077)
store volatile %struct.ScmObj* %_37length48042, %struct.ScmObj** %stackaddr$prim56697, align 8
%stackaddr$makeclosure56698 = alloca %struct.ScmObj*, align 8
%fptrToInt56699 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49007 to i64
%ae49007 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt56699)
store volatile %struct.ScmObj* %ae49007, %struct.ScmObj** %stackaddr$makeclosure56698, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49007, %struct.ScmObj* %_37foldr148053, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49007, %struct.ScmObj* %_37map148049, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49007, %struct.ScmObj* %Ycmb48032, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49007, %struct.ScmObj* %_37take48045, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49007, %struct.ScmObj* %_37length48042, i64 4)
%ae49008 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56700 = alloca %struct.ScmObj*, align 8
%fptrToInt56701 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49009 to i64
%ae49009 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56701)
store volatile %struct.ScmObj* %ae49009, %struct.ScmObj** %stackaddr$makeclosure56700, align 8
%argslist56476$ae490070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56702 = alloca %struct.ScmObj*, align 8
%argslist56476$ae490071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49009, %struct.ScmObj* %argslist56476$ae490070)
store volatile %struct.ScmObj* %argslist56476$ae490071, %struct.ScmObj** %stackaddr$prim56702, align 8
%stackaddr$prim56703 = alloca %struct.ScmObj*, align 8
%argslist56476$ae490072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49008, %struct.ScmObj* %argslist56476$ae490071)
store volatile %struct.ScmObj* %argslist56476$ae490072, %struct.ScmObj** %stackaddr$prim56703, align 8
%clofunc56704 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49007)
musttail call tailcc void %clofunc56704(%struct.ScmObj* %ae49007, %struct.ScmObj* %argslist56476$ae490072)
ret void
}

define tailcc void @proc_clo$ae49007(%struct.ScmObj* %env$ae49007,%struct.ScmObj* %current_45args56079) {
%stackaddr$env-ref56705 = alloca %struct.ScmObj*, align 8
%_37foldr148053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49007, i64 0)
store %struct.ScmObj* %_37foldr148053, %struct.ScmObj** %stackaddr$env-ref56705
%stackaddr$env-ref56706 = alloca %struct.ScmObj*, align 8
%_37map148049 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49007, i64 1)
store %struct.ScmObj* %_37map148049, %struct.ScmObj** %stackaddr$env-ref56706
%stackaddr$env-ref56707 = alloca %struct.ScmObj*, align 8
%Ycmb48032 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49007, i64 2)
store %struct.ScmObj* %Ycmb48032, %struct.ScmObj** %stackaddr$env-ref56707
%stackaddr$env-ref56708 = alloca %struct.ScmObj*, align 8
%_37take48045 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49007, i64 3)
store %struct.ScmObj* %_37take48045, %struct.ScmObj** %stackaddr$env-ref56708
%stackaddr$env-ref56709 = alloca %struct.ScmObj*, align 8
%_37length48042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49007, i64 4)
store %struct.ScmObj* %_37length48042, %struct.ScmObj** %stackaddr$env-ref56709
%stackaddr$prim56710 = alloca %struct.ScmObj*, align 8
%_95k48315 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56079)
store volatile %struct.ScmObj* %_95k48315, %struct.ScmObj** %stackaddr$prim56710, align 8
%stackaddr$prim56711 = alloca %struct.ScmObj*, align 8
%current_45args56080 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56079)
store volatile %struct.ScmObj* %current_45args56080, %struct.ScmObj** %stackaddr$prim56711, align 8
%stackaddr$prim56712 = alloca %struct.ScmObj*, align 8
%anf_45bind48198 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56080)
store volatile %struct.ScmObj* %anf_45bind48198, %struct.ScmObj** %stackaddr$prim56712, align 8
%stackaddr$makeclosure56713 = alloca %struct.ScmObj*, align 8
%fptrToInt56714 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49084 to i64
%ae49084 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt56714)
store volatile %struct.ScmObj* %ae49084, %struct.ScmObj** %stackaddr$makeclosure56713, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49084, %struct.ScmObj* %_37foldr148053, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49084, %struct.ScmObj* %_37map148049, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49084, %struct.ScmObj* %Ycmb48032, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49084, %struct.ScmObj* %_37take48045, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49084, %struct.ScmObj* %_37length48042, i64 4)
%argslist56460$Ycmb480320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56715 = alloca %struct.ScmObj*, align 8
%argslist56460$Ycmb480321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48198, %struct.ScmObj* %argslist56460$Ycmb480320)
store volatile %struct.ScmObj* %argslist56460$Ycmb480321, %struct.ScmObj** %stackaddr$prim56715, align 8
%stackaddr$prim56716 = alloca %struct.ScmObj*, align 8
%argslist56460$Ycmb480322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49084, %struct.ScmObj* %argslist56460$Ycmb480321)
store volatile %struct.ScmObj* %argslist56460$Ycmb480322, %struct.ScmObj** %stackaddr$prim56716, align 8
%clofunc56717 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48032)
musttail call tailcc void %clofunc56717(%struct.ScmObj* %Ycmb48032, %struct.ScmObj* %argslist56460$Ycmb480322)
ret void
}

define tailcc void @proc_clo$ae49084(%struct.ScmObj* %env$ae49084,%struct.ScmObj* %current_45args56082) {
%stackaddr$env-ref56718 = alloca %struct.ScmObj*, align 8
%_37foldr148053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49084, i64 0)
store %struct.ScmObj* %_37foldr148053, %struct.ScmObj** %stackaddr$env-ref56718
%stackaddr$env-ref56719 = alloca %struct.ScmObj*, align 8
%_37map148049 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49084, i64 1)
store %struct.ScmObj* %_37map148049, %struct.ScmObj** %stackaddr$env-ref56719
%stackaddr$env-ref56720 = alloca %struct.ScmObj*, align 8
%Ycmb48032 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49084, i64 2)
store %struct.ScmObj* %Ycmb48032, %struct.ScmObj** %stackaddr$env-ref56720
%stackaddr$env-ref56721 = alloca %struct.ScmObj*, align 8
%_37take48045 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49084, i64 3)
store %struct.ScmObj* %_37take48045, %struct.ScmObj** %stackaddr$env-ref56721
%stackaddr$env-ref56722 = alloca %struct.ScmObj*, align 8
%_37length48042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49084, i64 4)
store %struct.ScmObj* %_37length48042, %struct.ScmObj** %stackaddr$env-ref56722
%stackaddr$prim56723 = alloca %struct.ScmObj*, align 8
%_95k48316 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56082)
store volatile %struct.ScmObj* %_95k48316, %struct.ScmObj** %stackaddr$prim56723, align 8
%stackaddr$prim56724 = alloca %struct.ScmObj*, align 8
%current_45args56083 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56082)
store volatile %struct.ScmObj* %current_45args56083, %struct.ScmObj** %stackaddr$prim56724, align 8
%stackaddr$prim56725 = alloca %struct.ScmObj*, align 8
%_37foldl148037 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56083)
store volatile %struct.ScmObj* %_37foldl148037, %struct.ScmObj** %stackaddr$prim56725, align 8
%stackaddr$makeclosure56726 = alloca %struct.ScmObj*, align 8
%fptrToInt56727 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49086 to i64
%ae49086 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt56727)
store volatile %struct.ScmObj* %ae49086, %struct.ScmObj** %stackaddr$makeclosure56726, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49086, %struct.ScmObj* %_37foldr148053, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49086, %struct.ScmObj* %_37foldl148037, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49086, %struct.ScmObj* %_37map148049, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49086, %struct.ScmObj* %Ycmb48032, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49086, %struct.ScmObj* %_37take48045, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49086, %struct.ScmObj* %_37length48042, i64 5)
%ae49087 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56728 = alloca %struct.ScmObj*, align 8
%fptrToInt56729 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49088 to i64
%ae49088 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56729)
store volatile %struct.ScmObj* %ae49088, %struct.ScmObj** %stackaddr$makeclosure56728, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49088, %struct.ScmObj* %_37foldl148037, i64 0)
%argslist56459$ae490860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56730 = alloca %struct.ScmObj*, align 8
%argslist56459$ae490861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49088, %struct.ScmObj* %argslist56459$ae490860)
store volatile %struct.ScmObj* %argslist56459$ae490861, %struct.ScmObj** %stackaddr$prim56730, align 8
%stackaddr$prim56731 = alloca %struct.ScmObj*, align 8
%argslist56459$ae490862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49087, %struct.ScmObj* %argslist56459$ae490861)
store volatile %struct.ScmObj* %argslist56459$ae490862, %struct.ScmObj** %stackaddr$prim56731, align 8
%clofunc56732 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49086)
musttail call tailcc void %clofunc56732(%struct.ScmObj* %ae49086, %struct.ScmObj* %argslist56459$ae490862)
ret void
}

define tailcc void @proc_clo$ae49086(%struct.ScmObj* %env$ae49086,%struct.ScmObj* %current_45args56085) {
%stackaddr$env-ref56733 = alloca %struct.ScmObj*, align 8
%_37foldr148053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49086, i64 0)
store %struct.ScmObj* %_37foldr148053, %struct.ScmObj** %stackaddr$env-ref56733
%stackaddr$env-ref56734 = alloca %struct.ScmObj*, align 8
%_37foldl148037 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49086, i64 1)
store %struct.ScmObj* %_37foldl148037, %struct.ScmObj** %stackaddr$env-ref56734
%stackaddr$env-ref56735 = alloca %struct.ScmObj*, align 8
%_37map148049 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49086, i64 2)
store %struct.ScmObj* %_37map148049, %struct.ScmObj** %stackaddr$env-ref56735
%stackaddr$env-ref56736 = alloca %struct.ScmObj*, align 8
%Ycmb48032 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49086, i64 3)
store %struct.ScmObj* %Ycmb48032, %struct.ScmObj** %stackaddr$env-ref56736
%stackaddr$env-ref56737 = alloca %struct.ScmObj*, align 8
%_37take48045 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49086, i64 4)
store %struct.ScmObj* %_37take48045, %struct.ScmObj** %stackaddr$env-ref56737
%stackaddr$env-ref56738 = alloca %struct.ScmObj*, align 8
%_37length48042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49086, i64 5)
store %struct.ScmObj* %_37length48042, %struct.ScmObj** %stackaddr$env-ref56738
%stackaddr$prim56739 = alloca %struct.ScmObj*, align 8
%_95k48317 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56085)
store volatile %struct.ScmObj* %_95k48317, %struct.ScmObj** %stackaddr$prim56739, align 8
%stackaddr$prim56740 = alloca %struct.ScmObj*, align 8
%current_45args56086 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56085)
store volatile %struct.ScmObj* %current_45args56086, %struct.ScmObj** %stackaddr$prim56740, align 8
%stackaddr$prim56741 = alloca %struct.ScmObj*, align 8
%_37last48075 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56086)
store volatile %struct.ScmObj* %_37last48075, %struct.ScmObj** %stackaddr$prim56741, align 8
%stackaddr$makeclosure56742 = alloca %struct.ScmObj*, align 8
%fptrToInt56743 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49140 to i64
%ae49140 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt56743)
store volatile %struct.ScmObj* %ae49140, %struct.ScmObj** %stackaddr$makeclosure56742, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49140, %struct.ScmObj* %_37foldr148053, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49140, %struct.ScmObj* %_37foldl148037, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49140, %struct.ScmObj* %_37map148049, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49140, %struct.ScmObj* %Ycmb48032, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49140, %struct.ScmObj* %_37last48075, i64 4)
%ae49141 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56744 = alloca %struct.ScmObj*, align 8
%fptrToInt56745 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49142 to i64
%ae49142 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56745)
store volatile %struct.ScmObj* %ae49142, %struct.ScmObj** %stackaddr$makeclosure56744, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49142, %struct.ScmObj* %_37take48045, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49142, %struct.ScmObj* %_37length48042, i64 1)
%argslist56445$ae491400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56746 = alloca %struct.ScmObj*, align 8
%argslist56445$ae491401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49142, %struct.ScmObj* %argslist56445$ae491400)
store volatile %struct.ScmObj* %argslist56445$ae491401, %struct.ScmObj** %stackaddr$prim56746, align 8
%stackaddr$prim56747 = alloca %struct.ScmObj*, align 8
%argslist56445$ae491402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49141, %struct.ScmObj* %argslist56445$ae491401)
store volatile %struct.ScmObj* %argslist56445$ae491402, %struct.ScmObj** %stackaddr$prim56747, align 8
%clofunc56748 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49140)
musttail call tailcc void %clofunc56748(%struct.ScmObj* %ae49140, %struct.ScmObj* %argslist56445$ae491402)
ret void
}

define tailcc void @proc_clo$ae49140(%struct.ScmObj* %env$ae49140,%struct.ScmObj* %current_45args56088) {
%stackaddr$env-ref56749 = alloca %struct.ScmObj*, align 8
%_37foldr148053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49140, i64 0)
store %struct.ScmObj* %_37foldr148053, %struct.ScmObj** %stackaddr$env-ref56749
%stackaddr$env-ref56750 = alloca %struct.ScmObj*, align 8
%_37foldl148037 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49140, i64 1)
store %struct.ScmObj* %_37foldl148037, %struct.ScmObj** %stackaddr$env-ref56750
%stackaddr$env-ref56751 = alloca %struct.ScmObj*, align 8
%_37map148049 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49140, i64 2)
store %struct.ScmObj* %_37map148049, %struct.ScmObj** %stackaddr$env-ref56751
%stackaddr$env-ref56752 = alloca %struct.ScmObj*, align 8
%Ycmb48032 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49140, i64 3)
store %struct.ScmObj* %Ycmb48032, %struct.ScmObj** %stackaddr$env-ref56752
%stackaddr$env-ref56753 = alloca %struct.ScmObj*, align 8
%_37last48075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49140, i64 4)
store %struct.ScmObj* %_37last48075, %struct.ScmObj** %stackaddr$env-ref56753
%stackaddr$prim56754 = alloca %struct.ScmObj*, align 8
%_95k48318 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56088)
store volatile %struct.ScmObj* %_95k48318, %struct.ScmObj** %stackaddr$prim56754, align 8
%stackaddr$prim56755 = alloca %struct.ScmObj*, align 8
%current_45args56089 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56088)
store volatile %struct.ScmObj* %current_45args56089, %struct.ScmObj** %stackaddr$prim56755, align 8
%stackaddr$prim56756 = alloca %struct.ScmObj*, align 8
%_37drop_45right48072 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56089)
store volatile %struct.ScmObj* %_37drop_45right48072, %struct.ScmObj** %stackaddr$prim56756, align 8
%stackaddr$makeclosure56757 = alloca %struct.ScmObj*, align 8
%fptrToInt56758 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49170 to i64
%ae49170 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt56758)
store volatile %struct.ScmObj* %ae49170, %struct.ScmObj** %stackaddr$makeclosure56757, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49170, %struct.ScmObj* %_37foldr148053, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49170, %struct.ScmObj* %_37foldl148037, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49170, %struct.ScmObj* %_37drop_45right48072, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49170, %struct.ScmObj* %Ycmb48032, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49170, %struct.ScmObj* %_37last48075, i64 4)
%ae49171 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56759 = alloca %struct.ScmObj*, align 8
%fptrToInt56760 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49172 to i64
%ae49172 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56760)
store volatile %struct.ScmObj* %ae49172, %struct.ScmObj** %stackaddr$makeclosure56759, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49172, %struct.ScmObj* %_37foldr148053, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49172, %struct.ScmObj* %_37map148049, i64 1)
%argslist56435$ae491700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56761 = alloca %struct.ScmObj*, align 8
%argslist56435$ae491701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49172, %struct.ScmObj* %argslist56435$ae491700)
store volatile %struct.ScmObj* %argslist56435$ae491701, %struct.ScmObj** %stackaddr$prim56761, align 8
%stackaddr$prim56762 = alloca %struct.ScmObj*, align 8
%argslist56435$ae491702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49171, %struct.ScmObj* %argslist56435$ae491701)
store volatile %struct.ScmObj* %argslist56435$ae491702, %struct.ScmObj** %stackaddr$prim56762, align 8
%clofunc56763 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49170)
musttail call tailcc void %clofunc56763(%struct.ScmObj* %ae49170, %struct.ScmObj* %argslist56435$ae491702)
ret void
}

define tailcc void @proc_clo$ae49170(%struct.ScmObj* %env$ae49170,%struct.ScmObj* %current_45args56091) {
%stackaddr$env-ref56764 = alloca %struct.ScmObj*, align 8
%_37foldr148053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49170, i64 0)
store %struct.ScmObj* %_37foldr148053, %struct.ScmObj** %stackaddr$env-ref56764
%stackaddr$env-ref56765 = alloca %struct.ScmObj*, align 8
%_37foldl148037 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49170, i64 1)
store %struct.ScmObj* %_37foldl148037, %struct.ScmObj** %stackaddr$env-ref56765
%stackaddr$env-ref56766 = alloca %struct.ScmObj*, align 8
%_37drop_45right48072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49170, i64 2)
store %struct.ScmObj* %_37drop_45right48072, %struct.ScmObj** %stackaddr$env-ref56766
%stackaddr$env-ref56767 = alloca %struct.ScmObj*, align 8
%Ycmb48032 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49170, i64 3)
store %struct.ScmObj* %Ycmb48032, %struct.ScmObj** %stackaddr$env-ref56767
%stackaddr$env-ref56768 = alloca %struct.ScmObj*, align 8
%_37last48075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49170, i64 4)
store %struct.ScmObj* %_37last48075, %struct.ScmObj** %stackaddr$env-ref56768
%stackaddr$prim56769 = alloca %struct.ScmObj*, align 8
%_95k48319 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56091)
store volatile %struct.ScmObj* %_95k48319, %struct.ScmObj** %stackaddr$prim56769, align 8
%stackaddr$prim56770 = alloca %struct.ScmObj*, align 8
%current_45args56092 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56091)
store volatile %struct.ScmObj* %current_45args56092, %struct.ScmObj** %stackaddr$prim56770, align 8
%stackaddr$prim56771 = alloca %struct.ScmObj*, align 8
%anf_45bind48214 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56092)
store volatile %struct.ScmObj* %anf_45bind48214, %struct.ScmObj** %stackaddr$prim56771, align 8
%stackaddr$makeclosure56772 = alloca %struct.ScmObj*, align 8
%fptrToInt56773 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49554 to i64
%ae49554 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt56773)
store volatile %struct.ScmObj* %ae49554, %struct.ScmObj** %stackaddr$makeclosure56772, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49554, %struct.ScmObj* %_37foldr148053, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49554, %struct.ScmObj* %_37foldl148037, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49554, %struct.ScmObj* %_37drop_45right48072, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49554, %struct.ScmObj* %Ycmb48032, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49554, %struct.ScmObj* %_37last48075, i64 4)
%argslist56375$Ycmb480320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56774 = alloca %struct.ScmObj*, align 8
%argslist56375$Ycmb480321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48214, %struct.ScmObj* %argslist56375$Ycmb480320)
store volatile %struct.ScmObj* %argslist56375$Ycmb480321, %struct.ScmObj** %stackaddr$prim56774, align 8
%stackaddr$prim56775 = alloca %struct.ScmObj*, align 8
%argslist56375$Ycmb480322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49554, %struct.ScmObj* %argslist56375$Ycmb480321)
store volatile %struct.ScmObj* %argslist56375$Ycmb480322, %struct.ScmObj** %stackaddr$prim56775, align 8
%clofunc56776 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48032)
musttail call tailcc void %clofunc56776(%struct.ScmObj* %Ycmb48032, %struct.ScmObj* %argslist56375$Ycmb480322)
ret void
}

define tailcc void @proc_clo$ae49554(%struct.ScmObj* %env$ae49554,%struct.ScmObj* %current_45args56094) {
%stackaddr$env-ref56777 = alloca %struct.ScmObj*, align 8
%_37foldr148053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49554, i64 0)
store %struct.ScmObj* %_37foldr148053, %struct.ScmObj** %stackaddr$env-ref56777
%stackaddr$env-ref56778 = alloca %struct.ScmObj*, align 8
%_37foldl148037 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49554, i64 1)
store %struct.ScmObj* %_37foldl148037, %struct.ScmObj** %stackaddr$env-ref56778
%stackaddr$env-ref56779 = alloca %struct.ScmObj*, align 8
%_37drop_45right48072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49554, i64 2)
store %struct.ScmObj* %_37drop_45right48072, %struct.ScmObj** %stackaddr$env-ref56779
%stackaddr$env-ref56780 = alloca %struct.ScmObj*, align 8
%Ycmb48032 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49554, i64 3)
store %struct.ScmObj* %Ycmb48032, %struct.ScmObj** %stackaddr$env-ref56780
%stackaddr$env-ref56781 = alloca %struct.ScmObj*, align 8
%_37last48075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49554, i64 4)
store %struct.ScmObj* %_37last48075, %struct.ScmObj** %stackaddr$env-ref56781
%stackaddr$prim56782 = alloca %struct.ScmObj*, align 8
%_95k48320 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56094)
store volatile %struct.ScmObj* %_95k48320, %struct.ScmObj** %stackaddr$prim56782, align 8
%stackaddr$prim56783 = alloca %struct.ScmObj*, align 8
%current_45args56095 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56094)
store volatile %struct.ScmObj* %current_45args56095, %struct.ScmObj** %stackaddr$prim56783, align 8
%stackaddr$prim56784 = alloca %struct.ScmObj*, align 8
%_37foldr48058 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56095)
store volatile %struct.ScmObj* %_37foldr48058, %struct.ScmObj** %stackaddr$prim56784, align 8
%stackaddr$makeclosure56785 = alloca %struct.ScmObj*, align 8
%fptrToInt56786 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49556 to i64
%ae49556 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt56786)
store volatile %struct.ScmObj* %ae49556, %struct.ScmObj** %stackaddr$makeclosure56785, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49556, %struct.ScmObj* %_37foldr148053, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49556, %struct.ScmObj* %_37foldl148037, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49556, %struct.ScmObj* %_37drop_45right48072, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49556, %struct.ScmObj* %Ycmb48032, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49556, %struct.ScmObj* %_37last48075, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49556, %struct.ScmObj* %_37foldr48058, i64 5)
%ae49557 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56787 = alloca %struct.ScmObj*, align 8
%fptrToInt56788 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49558 to i64
%ae49558 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56788)
store volatile %struct.ScmObj* %ae49558, %struct.ScmObj** %stackaddr$makeclosure56787, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49558, %struct.ScmObj* %_37foldr148053, i64 0)
%argslist56374$ae495560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56789 = alloca %struct.ScmObj*, align 8
%argslist56374$ae495561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49558, %struct.ScmObj* %argslist56374$ae495560)
store volatile %struct.ScmObj* %argslist56374$ae495561, %struct.ScmObj** %stackaddr$prim56789, align 8
%stackaddr$prim56790 = alloca %struct.ScmObj*, align 8
%argslist56374$ae495562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49557, %struct.ScmObj* %argslist56374$ae495561)
store volatile %struct.ScmObj* %argslist56374$ae495562, %struct.ScmObj** %stackaddr$prim56790, align 8
%clofunc56791 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49556)
musttail call tailcc void %clofunc56791(%struct.ScmObj* %ae49556, %struct.ScmObj* %argslist56374$ae495562)
ret void
}

define tailcc void @proc_clo$ae49556(%struct.ScmObj* %env$ae49556,%struct.ScmObj* %current_45args56097) {
%stackaddr$env-ref56792 = alloca %struct.ScmObj*, align 8
%_37foldr148053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49556, i64 0)
store %struct.ScmObj* %_37foldr148053, %struct.ScmObj** %stackaddr$env-ref56792
%stackaddr$env-ref56793 = alloca %struct.ScmObj*, align 8
%_37foldl148037 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49556, i64 1)
store %struct.ScmObj* %_37foldl148037, %struct.ScmObj** %stackaddr$env-ref56793
%stackaddr$env-ref56794 = alloca %struct.ScmObj*, align 8
%_37drop_45right48072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49556, i64 2)
store %struct.ScmObj* %_37drop_45right48072, %struct.ScmObj** %stackaddr$env-ref56794
%stackaddr$env-ref56795 = alloca %struct.ScmObj*, align 8
%Ycmb48032 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49556, i64 3)
store %struct.ScmObj* %Ycmb48032, %struct.ScmObj** %stackaddr$env-ref56795
%stackaddr$env-ref56796 = alloca %struct.ScmObj*, align 8
%_37last48075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49556, i64 4)
store %struct.ScmObj* %_37last48075, %struct.ScmObj** %stackaddr$env-ref56796
%stackaddr$env-ref56797 = alloca %struct.ScmObj*, align 8
%_37foldr48058 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49556, i64 5)
store %struct.ScmObj* %_37foldr48058, %struct.ScmObj** %stackaddr$env-ref56797
%stackaddr$prim56798 = alloca %struct.ScmObj*, align 8
%_95k48321 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56097)
store volatile %struct.ScmObj* %_95k48321, %struct.ScmObj** %stackaddr$prim56798, align 8
%stackaddr$prim56799 = alloca %struct.ScmObj*, align 8
%current_45args56098 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56097)
store volatile %struct.ScmObj* %current_45args56098, %struct.ScmObj** %stackaddr$prim56799, align 8
%stackaddr$prim56800 = alloca %struct.ScmObj*, align 8
%_37map148084 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56098)
store volatile %struct.ScmObj* %_37map148084, %struct.ScmObj** %stackaddr$prim56800, align 8
%stackaddr$makeclosure56801 = alloca %struct.ScmObj*, align 8
%fptrToInt56802 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49633 to i64
%ae49633 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt56802)
store volatile %struct.ScmObj* %ae49633, %struct.ScmObj** %stackaddr$makeclosure56801, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49633, %struct.ScmObj* %_37foldr148053, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49633, %struct.ScmObj* %_37foldl148037, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49633, %struct.ScmObj* %_37map148084, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49633, %struct.ScmObj* %Ycmb48032, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49633, %struct.ScmObj* %_37foldr48058, i64 4)
%ae49634 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56803 = alloca %struct.ScmObj*, align 8
%fptrToInt56804 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49635 to i64
%ae49635 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56804)
store volatile %struct.ScmObj* %ae49635, %struct.ScmObj** %stackaddr$makeclosure56803, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49635, %struct.ScmObj* %_37drop_45right48072, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49635, %struct.ScmObj* %_37last48075, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49635, %struct.ScmObj* %_37foldr48058, i64 2)
%argslist56355$ae496330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56805 = alloca %struct.ScmObj*, align 8
%argslist56355$ae496331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49635, %struct.ScmObj* %argslist56355$ae496330)
store volatile %struct.ScmObj* %argslist56355$ae496331, %struct.ScmObj** %stackaddr$prim56805, align 8
%stackaddr$prim56806 = alloca %struct.ScmObj*, align 8
%argslist56355$ae496332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49634, %struct.ScmObj* %argslist56355$ae496331)
store volatile %struct.ScmObj* %argslist56355$ae496332, %struct.ScmObj** %stackaddr$prim56806, align 8
%clofunc56807 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49633)
musttail call tailcc void %clofunc56807(%struct.ScmObj* %ae49633, %struct.ScmObj* %argslist56355$ae496332)
ret void
}

define tailcc void @proc_clo$ae49633(%struct.ScmObj* %env$ae49633,%struct.ScmObj* %current_45args56100) {
%stackaddr$env-ref56808 = alloca %struct.ScmObj*, align 8
%_37foldr148053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49633, i64 0)
store %struct.ScmObj* %_37foldr148053, %struct.ScmObj** %stackaddr$env-ref56808
%stackaddr$env-ref56809 = alloca %struct.ScmObj*, align 8
%_37foldl148037 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49633, i64 1)
store %struct.ScmObj* %_37foldl148037, %struct.ScmObj** %stackaddr$env-ref56809
%stackaddr$env-ref56810 = alloca %struct.ScmObj*, align 8
%_37map148084 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49633, i64 2)
store %struct.ScmObj* %_37map148084, %struct.ScmObj** %stackaddr$env-ref56810
%stackaddr$env-ref56811 = alloca %struct.ScmObj*, align 8
%Ycmb48032 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49633, i64 3)
store %struct.ScmObj* %Ycmb48032, %struct.ScmObj** %stackaddr$env-ref56811
%stackaddr$env-ref56812 = alloca %struct.ScmObj*, align 8
%_37foldr48058 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49633, i64 4)
store %struct.ScmObj* %_37foldr48058, %struct.ScmObj** %stackaddr$env-ref56812
%stackaddr$prim56813 = alloca %struct.ScmObj*, align 8
%_95k48322 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56100)
store volatile %struct.ScmObj* %_95k48322, %struct.ScmObj** %stackaddr$prim56813, align 8
%stackaddr$prim56814 = alloca %struct.ScmObj*, align 8
%current_45args56101 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56100)
store volatile %struct.ScmObj* %current_45args56101, %struct.ScmObj** %stackaddr$prim56814, align 8
%stackaddr$prim56815 = alloca %struct.ScmObj*, align 8
%_37map48079 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56101)
store volatile %struct.ScmObj* %_37map48079, %struct.ScmObj** %stackaddr$prim56815, align 8
%stackaddr$makeclosure56816 = alloca %struct.ScmObj*, align 8
%fptrToInt56817 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49779 to i64
%ae49779 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56817)
store volatile %struct.ScmObj* %ae49779, %struct.ScmObj** %stackaddr$makeclosure56816, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49779, %struct.ScmObj* %_37foldl148037, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49779, %struct.ScmObj* %Ycmb48032, i64 1)
%ae49780 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56818 = alloca %struct.ScmObj*, align 8
%fptrToInt56819 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49781 to i64
%ae49781 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56819)
store volatile %struct.ScmObj* %ae49781, %struct.ScmObj** %stackaddr$makeclosure56818, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49781, %struct.ScmObj* %_37foldr148053, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49781, %struct.ScmObj* %_37map148084, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49781, %struct.ScmObj* %_37foldr48058, i64 2)
%argslist56338$ae497790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56820 = alloca %struct.ScmObj*, align 8
%argslist56338$ae497791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49781, %struct.ScmObj* %argslist56338$ae497790)
store volatile %struct.ScmObj* %argslist56338$ae497791, %struct.ScmObj** %stackaddr$prim56820, align 8
%stackaddr$prim56821 = alloca %struct.ScmObj*, align 8
%argslist56338$ae497792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49780, %struct.ScmObj* %argslist56338$ae497791)
store volatile %struct.ScmObj* %argslist56338$ae497792, %struct.ScmObj** %stackaddr$prim56821, align 8
%clofunc56822 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49779)
musttail call tailcc void %clofunc56822(%struct.ScmObj* %ae49779, %struct.ScmObj* %argslist56338$ae497792)
ret void
}

define tailcc void @proc_clo$ae49779(%struct.ScmObj* %env$ae49779,%struct.ScmObj* %current_45args56103) {
%stackaddr$env-ref56823 = alloca %struct.ScmObj*, align 8
%_37foldl148037 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49779, i64 0)
store %struct.ScmObj* %_37foldl148037, %struct.ScmObj** %stackaddr$env-ref56823
%stackaddr$env-ref56824 = alloca %struct.ScmObj*, align 8
%Ycmb48032 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49779, i64 1)
store %struct.ScmObj* %Ycmb48032, %struct.ScmObj** %stackaddr$env-ref56824
%stackaddr$prim56825 = alloca %struct.ScmObj*, align 8
%_95k48323 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56103)
store volatile %struct.ScmObj* %_95k48323, %struct.ScmObj** %stackaddr$prim56825, align 8
%stackaddr$prim56826 = alloca %struct.ScmObj*, align 8
%current_45args56104 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56103)
store volatile %struct.ScmObj* %current_45args56104, %struct.ScmObj** %stackaddr$prim56826, align 8
%stackaddr$prim56827 = alloca %struct.ScmObj*, align 8
%anf_45bind48234 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56104)
store volatile %struct.ScmObj* %anf_45bind48234, %struct.ScmObj** %stackaddr$prim56827, align 8
%stackaddr$makeclosure56828 = alloca %struct.ScmObj*, align 8
%fptrToInt56829 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50171 to i64
%ae50171 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56829)
store volatile %struct.ScmObj* %ae50171, %struct.ScmObj** %stackaddr$makeclosure56828, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50171, %struct.ScmObj* %_37foldl148037, i64 0)
%argslist56278$Ycmb480320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56830 = alloca %struct.ScmObj*, align 8
%argslist56278$Ycmb480321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48234, %struct.ScmObj* %argslist56278$Ycmb480320)
store volatile %struct.ScmObj* %argslist56278$Ycmb480321, %struct.ScmObj** %stackaddr$prim56830, align 8
%stackaddr$prim56831 = alloca %struct.ScmObj*, align 8
%argslist56278$Ycmb480322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50171, %struct.ScmObj* %argslist56278$Ycmb480321)
store volatile %struct.ScmObj* %argslist56278$Ycmb480322, %struct.ScmObj** %stackaddr$prim56831, align 8
%clofunc56832 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48032)
musttail call tailcc void %clofunc56832(%struct.ScmObj* %Ycmb48032, %struct.ScmObj* %argslist56278$Ycmb480322)
ret void
}

define tailcc void @proc_clo$ae50171(%struct.ScmObj* %env$ae50171,%struct.ScmObj* %current_45args56106) {
%stackaddr$env-ref56833 = alloca %struct.ScmObj*, align 8
%_37foldl148037 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50171, i64 0)
store %struct.ScmObj* %_37foldl148037, %struct.ScmObj** %stackaddr$env-ref56833
%stackaddr$prim56834 = alloca %struct.ScmObj*, align 8
%_95k48324 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56106)
store volatile %struct.ScmObj* %_95k48324, %struct.ScmObj** %stackaddr$prim56834, align 8
%stackaddr$prim56835 = alloca %struct.ScmObj*, align 8
%current_45args56107 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56106)
store volatile %struct.ScmObj* %current_45args56107, %struct.ScmObj** %stackaddr$prim56835, align 8
%stackaddr$prim56836 = alloca %struct.ScmObj*, align 8
%_37foldl48135 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56107)
store volatile %struct.ScmObj* %_37foldl48135, %struct.ScmObj** %stackaddr$prim56836, align 8
%stackaddr$makeclosure56837 = alloca %struct.ScmObj*, align 8
%fptrToInt56838 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50173 to i64
%ae50173 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56838)
store volatile %struct.ScmObj* %ae50173, %struct.ScmObj** %stackaddr$makeclosure56837, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50173, %struct.ScmObj* %_37foldl148037, i64 0)
%ae50174 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56839 = alloca %struct.ScmObj*, align 8
%fptrToInt56840 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50175 to i64
%ae50175 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56840)
store volatile %struct.ScmObj* %ae50175, %struct.ScmObj** %stackaddr$makeclosure56839, align 8
%argslist56277$ae501730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56841 = alloca %struct.ScmObj*, align 8
%argslist56277$ae501731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50175, %struct.ScmObj* %argslist56277$ae501730)
store volatile %struct.ScmObj* %argslist56277$ae501731, %struct.ScmObj** %stackaddr$prim56841, align 8
%stackaddr$prim56842 = alloca %struct.ScmObj*, align 8
%argslist56277$ae501732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50174, %struct.ScmObj* %argslist56277$ae501731)
store volatile %struct.ScmObj* %argslist56277$ae501732, %struct.ScmObj** %stackaddr$prim56842, align 8
%clofunc56843 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50173)
musttail call tailcc void %clofunc56843(%struct.ScmObj* %ae50173, %struct.ScmObj* %argslist56277$ae501732)
ret void
}

define tailcc void @proc_clo$ae50173(%struct.ScmObj* %env$ae50173,%struct.ScmObj* %current_45args56109) {
%stackaddr$env-ref56844 = alloca %struct.ScmObj*, align 8
%_37foldl148037 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50173, i64 0)
store %struct.ScmObj* %_37foldl148037, %struct.ScmObj** %stackaddr$env-ref56844
%stackaddr$prim56845 = alloca %struct.ScmObj*, align 8
%_95k48325 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56109)
store volatile %struct.ScmObj* %_95k48325, %struct.ScmObj** %stackaddr$prim56845, align 8
%stackaddr$prim56846 = alloca %struct.ScmObj*, align 8
%current_45args56110 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56109)
store volatile %struct.ScmObj* %current_45args56110, %struct.ScmObj** %stackaddr$prim56846, align 8
%stackaddr$prim56847 = alloca %struct.ScmObj*, align 8
%_37_6248132 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56110)
store volatile %struct.ScmObj* %_37_6248132, %struct.ScmObj** %stackaddr$prim56847, align 8
%stackaddr$makeclosure56848 = alloca %struct.ScmObj*, align 8
%fptrToInt56849 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50197 to i64
%ae50197 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56849)
store volatile %struct.ScmObj* %ae50197, %struct.ScmObj** %stackaddr$makeclosure56848, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50197, %struct.ScmObj* %_37foldl148037, i64 0)
%ae50198 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56850 = alloca %struct.ScmObj*, align 8
%fptrToInt56851 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50199 to i64
%ae50199 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56851)
store volatile %struct.ScmObj* %ae50199, %struct.ScmObj** %stackaddr$makeclosure56850, align 8
%argslist56271$ae501970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56852 = alloca %struct.ScmObj*, align 8
%argslist56271$ae501971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50199, %struct.ScmObj* %argslist56271$ae501970)
store volatile %struct.ScmObj* %argslist56271$ae501971, %struct.ScmObj** %stackaddr$prim56852, align 8
%stackaddr$prim56853 = alloca %struct.ScmObj*, align 8
%argslist56271$ae501972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50198, %struct.ScmObj* %argslist56271$ae501971)
store volatile %struct.ScmObj* %argslist56271$ae501972, %struct.ScmObj** %stackaddr$prim56853, align 8
%clofunc56854 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50197)
musttail call tailcc void %clofunc56854(%struct.ScmObj* %ae50197, %struct.ScmObj* %argslist56271$ae501972)
ret void
}

define tailcc void @proc_clo$ae50197(%struct.ScmObj* %env$ae50197,%struct.ScmObj* %current_45args56112) {
%stackaddr$env-ref56855 = alloca %struct.ScmObj*, align 8
%_37foldl148037 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50197, i64 0)
store %struct.ScmObj* %_37foldl148037, %struct.ScmObj** %stackaddr$env-ref56855
%stackaddr$prim56856 = alloca %struct.ScmObj*, align 8
%_95k48326 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56112)
store volatile %struct.ScmObj* %_95k48326, %struct.ScmObj** %stackaddr$prim56856, align 8
%stackaddr$prim56857 = alloca %struct.ScmObj*, align 8
%current_45args56113 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56112)
store volatile %struct.ScmObj* %current_45args56113, %struct.ScmObj** %stackaddr$prim56857, align 8
%stackaddr$prim56858 = alloca %struct.ScmObj*, align 8
%_37_62_6148129 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56113)
store volatile %struct.ScmObj* %_37_62_6148129, %struct.ScmObj** %stackaddr$prim56858, align 8
%ae50221 = call %struct.ScmObj* @const_init_int(i64 1)
%ae50222 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56859 = alloca %struct.ScmObj*, align 8
%_37append48125 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50221, %struct.ScmObj* %ae50222)
store volatile %struct.ScmObj* %_37append48125, %struct.ScmObj** %stackaddr$prim56859, align 8
%stackaddr$makeclosure56860 = alloca %struct.ScmObj*, align 8
%fptrToInt56861 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50223 to i64
%ae50223 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56861)
store volatile %struct.ScmObj* %ae50223, %struct.ScmObj** %stackaddr$makeclosure56860, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50223, %struct.ScmObj* %_37foldl148037, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50223, %struct.ScmObj* %_37append48125, i64 1)
%ae50224 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56862 = alloca %struct.ScmObj*, align 8
%fptrToInt56863 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50225 to i64
%ae50225 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56863)
store volatile %struct.ScmObj* %ae50225, %struct.ScmObj** %stackaddr$makeclosure56862, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50225, %struct.ScmObj* %_37append48125, i64 0)
%argslist56265$ae502230 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56864 = alloca %struct.ScmObj*, align 8
%argslist56265$ae502231 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50225, %struct.ScmObj* %argslist56265$ae502230)
store volatile %struct.ScmObj* %argslist56265$ae502231, %struct.ScmObj** %stackaddr$prim56864, align 8
%stackaddr$prim56865 = alloca %struct.ScmObj*, align 8
%argslist56265$ae502232 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50224, %struct.ScmObj* %argslist56265$ae502231)
store volatile %struct.ScmObj* %argslist56265$ae502232, %struct.ScmObj** %stackaddr$prim56865, align 8
%clofunc56866 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50223)
musttail call tailcc void %clofunc56866(%struct.ScmObj* %ae50223, %struct.ScmObj* %argslist56265$ae502232)
ret void
}

define tailcc void @proc_clo$ae50223(%struct.ScmObj* %env$ae50223,%struct.ScmObj* %current_45args56115) {
%stackaddr$env-ref56867 = alloca %struct.ScmObj*, align 8
%_37foldl148037 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50223, i64 0)
store %struct.ScmObj* %_37foldl148037, %struct.ScmObj** %stackaddr$env-ref56867
%stackaddr$env-ref56868 = alloca %struct.ScmObj*, align 8
%_37append48125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50223, i64 1)
store %struct.ScmObj* %_37append48125, %struct.ScmObj** %stackaddr$env-ref56868
%stackaddr$prim56869 = alloca %struct.ScmObj*, align 8
%_95k48327 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56115)
store volatile %struct.ScmObj* %_95k48327, %struct.ScmObj** %stackaddr$prim56869, align 8
%stackaddr$prim56870 = alloca %struct.ScmObj*, align 8
%current_45args56116 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56115)
store volatile %struct.ScmObj* %current_45args56116, %struct.ScmObj** %stackaddr$prim56870, align 8
%stackaddr$prim56871 = alloca %struct.ScmObj*, align 8
%anf_45bind48242 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56116)
store volatile %struct.ScmObj* %anf_45bind48242, %struct.ScmObj** %stackaddr$prim56871, align 8
%ae50291 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56872 = alloca %struct.ScmObj*, align 8
%_95048126 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append48125, %struct.ScmObj* %ae50291, %struct.ScmObj* %anf_45bind48242)
store volatile %struct.ScmObj* %_95048126, %struct.ScmObj** %stackaddr$prim56872, align 8
%ae50294 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56873 = alloca %struct.ScmObj*, align 8
%_37append48124 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append48125, %struct.ScmObj* %ae50294)
store volatile %struct.ScmObj* %_37append48124, %struct.ScmObj** %stackaddr$prim56873, align 8
%stackaddr$makeclosure56874 = alloca %struct.ScmObj*, align 8
%fptrToInt56875 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50295 to i64
%ae50295 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56875)
store volatile %struct.ScmObj* %ae50295, %struct.ScmObj** %stackaddr$makeclosure56874, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50295, %struct.ScmObj* %_37foldl148037, i64 0)
%ae50296 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56876 = alloca %struct.ScmObj*, align 8
%fptrToInt56877 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50297 to i64
%ae50297 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56877)
store volatile %struct.ScmObj* %ae50297, %struct.ScmObj** %stackaddr$makeclosure56876, align 8
%argslist56254$ae502950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56878 = alloca %struct.ScmObj*, align 8
%argslist56254$ae502951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50297, %struct.ScmObj* %argslist56254$ae502950)
store volatile %struct.ScmObj* %argslist56254$ae502951, %struct.ScmObj** %stackaddr$prim56878, align 8
%stackaddr$prim56879 = alloca %struct.ScmObj*, align 8
%argslist56254$ae502952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50296, %struct.ScmObj* %argslist56254$ae502951)
store volatile %struct.ScmObj* %argslist56254$ae502952, %struct.ScmObj** %stackaddr$prim56879, align 8
%clofunc56880 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50295)
musttail call tailcc void %clofunc56880(%struct.ScmObj* %ae50295, %struct.ScmObj* %argslist56254$ae502952)
ret void
}

define tailcc void @proc_clo$ae50295(%struct.ScmObj* %env$ae50295,%struct.ScmObj* %current_45args56118) {
%stackaddr$env-ref56881 = alloca %struct.ScmObj*, align 8
%_37foldl148037 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50295, i64 0)
store %struct.ScmObj* %_37foldl148037, %struct.ScmObj** %stackaddr$env-ref56881
%stackaddr$prim56882 = alloca %struct.ScmObj*, align 8
%_95k48328 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56118)
store volatile %struct.ScmObj* %_95k48328, %struct.ScmObj** %stackaddr$prim56882, align 8
%stackaddr$prim56883 = alloca %struct.ScmObj*, align 8
%current_45args56119 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56118)
store volatile %struct.ScmObj* %current_45args56119, %struct.ScmObj** %stackaddr$prim56883, align 8
%stackaddr$prim56884 = alloca %struct.ScmObj*, align 8
%_37list_6348117 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56119)
store volatile %struct.ScmObj* %_37list_6348117, %struct.ScmObj** %stackaddr$prim56884, align 8
%stackaddr$makeclosure56885 = alloca %struct.ScmObj*, align 8
%fptrToInt56886 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50711 to i64
%ae50711 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56886)
store volatile %struct.ScmObj* %ae50711, %struct.ScmObj** %stackaddr$makeclosure56885, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50711, %struct.ScmObj* %_37foldl148037, i64 0)
%ae50712 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56887 = alloca %struct.ScmObj*, align 8
%fptrToInt56888 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50713 to i64
%ae50713 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56888)
store volatile %struct.ScmObj* %ae50713, %struct.ScmObj** %stackaddr$makeclosure56887, align 8
%argslist56229$ae507110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56889 = alloca %struct.ScmObj*, align 8
%argslist56229$ae507111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50713, %struct.ScmObj* %argslist56229$ae507110)
store volatile %struct.ScmObj* %argslist56229$ae507111, %struct.ScmObj** %stackaddr$prim56889, align 8
%stackaddr$prim56890 = alloca %struct.ScmObj*, align 8
%argslist56229$ae507112 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50712, %struct.ScmObj* %argslist56229$ae507111)
store volatile %struct.ScmObj* %argslist56229$ae507112, %struct.ScmObj** %stackaddr$prim56890, align 8
%clofunc56891 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50711)
musttail call tailcc void %clofunc56891(%struct.ScmObj* %ae50711, %struct.ScmObj* %argslist56229$ae507112)
ret void
}

define tailcc void @proc_clo$ae50711(%struct.ScmObj* %env$ae50711,%struct.ScmObj* %current_45args56121) {
%stackaddr$env-ref56892 = alloca %struct.ScmObj*, align 8
%_37foldl148037 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50711, i64 0)
store %struct.ScmObj* %_37foldl148037, %struct.ScmObj** %stackaddr$env-ref56892
%stackaddr$prim56893 = alloca %struct.ScmObj*, align 8
%_95k48329 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56121)
store volatile %struct.ScmObj* %_95k48329, %struct.ScmObj** %stackaddr$prim56893, align 8
%stackaddr$prim56894 = alloca %struct.ScmObj*, align 8
%current_45args56122 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56121)
store volatile %struct.ScmObj* %current_45args56122, %struct.ScmObj** %stackaddr$prim56894, align 8
%stackaddr$prim56895 = alloca %struct.ScmObj*, align 8
%_37drop48108 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56122)
store volatile %struct.ScmObj* %_37drop48108, %struct.ScmObj** %stackaddr$prim56895, align 8
%stackaddr$makeclosure56896 = alloca %struct.ScmObj*, align 8
%fptrToInt56897 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51247 to i64
%ae51247 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56897)
store volatile %struct.ScmObj* %ae51247, %struct.ScmObj** %stackaddr$makeclosure56896, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51247, %struct.ScmObj* %_37foldl148037, i64 0)
%ae51248 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56898 = alloca %struct.ScmObj*, align 8
%fptrToInt56899 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51249 to i64
%ae51249 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56899)
store volatile %struct.ScmObj* %ae51249, %struct.ScmObj** %stackaddr$makeclosure56898, align 8
%argslist56205$ae512470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56900 = alloca %struct.ScmObj*, align 8
%argslist56205$ae512471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51249, %struct.ScmObj* %argslist56205$ae512470)
store volatile %struct.ScmObj* %argslist56205$ae512471, %struct.ScmObj** %stackaddr$prim56900, align 8
%stackaddr$prim56901 = alloca %struct.ScmObj*, align 8
%argslist56205$ae512472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51248, %struct.ScmObj* %argslist56205$ae512471)
store volatile %struct.ScmObj* %argslist56205$ae512472, %struct.ScmObj** %stackaddr$prim56901, align 8
%clofunc56902 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51247)
musttail call tailcc void %clofunc56902(%struct.ScmObj* %ae51247, %struct.ScmObj* %argslist56205$ae512472)
ret void
}

define tailcc void @proc_clo$ae51247(%struct.ScmObj* %env$ae51247,%struct.ScmObj* %current_45args56124) {
%stackaddr$env-ref56903 = alloca %struct.ScmObj*, align 8
%_37foldl148037 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51247, i64 0)
store %struct.ScmObj* %_37foldl148037, %struct.ScmObj** %stackaddr$env-ref56903
%stackaddr$prim56904 = alloca %struct.ScmObj*, align 8
%_95k48330 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56124)
store volatile %struct.ScmObj* %_95k48330, %struct.ScmObj** %stackaddr$prim56904, align 8
%stackaddr$prim56905 = alloca %struct.ScmObj*, align 8
%current_45args56125 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56124)
store volatile %struct.ScmObj* %current_45args56125, %struct.ScmObj** %stackaddr$prim56905, align 8
%stackaddr$prim56906 = alloca %struct.ScmObj*, align 8
%_37memv48101 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56125)
store volatile %struct.ScmObj* %_37memv48101, %struct.ScmObj** %stackaddr$prim56906, align 8
%stackaddr$makeclosure56907 = alloca %struct.ScmObj*, align 8
%fptrToInt56908 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51651 to i64
%ae51651 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56908)
store volatile %struct.ScmObj* %ae51651, %struct.ScmObj** %stackaddr$makeclosure56907, align 8
%ae51652 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56909 = alloca %struct.ScmObj*, align 8
%fptrToInt56910 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51653 to i64
%ae51653 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56910)
store volatile %struct.ScmObj* %ae51653, %struct.ScmObj** %stackaddr$makeclosure56909, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51653, %struct.ScmObj* %_37foldl148037, i64 0)
%argslist56179$ae516510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56911 = alloca %struct.ScmObj*, align 8
%argslist56179$ae516511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51653, %struct.ScmObj* %argslist56179$ae516510)
store volatile %struct.ScmObj* %argslist56179$ae516511, %struct.ScmObj** %stackaddr$prim56911, align 8
%stackaddr$prim56912 = alloca %struct.ScmObj*, align 8
%argslist56179$ae516512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51652, %struct.ScmObj* %argslist56179$ae516511)
store volatile %struct.ScmObj* %argslist56179$ae516512, %struct.ScmObj** %stackaddr$prim56912, align 8
%clofunc56913 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51651)
musttail call tailcc void %clofunc56913(%struct.ScmObj* %ae51651, %struct.ScmObj* %argslist56179$ae516512)
ret void
}

define tailcc void @proc_clo$ae51651(%struct.ScmObj* %env$ae51651,%struct.ScmObj* %current_45args56127) {
%stackaddr$prim56914 = alloca %struct.ScmObj*, align 8
%_95k48331 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56127)
store volatile %struct.ScmObj* %_95k48331, %struct.ScmObj** %stackaddr$prim56914, align 8
%stackaddr$prim56915 = alloca %struct.ScmObj*, align 8
%current_45args56128 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56127)
store volatile %struct.ScmObj* %current_45args56128, %struct.ScmObj** %stackaddr$prim56915, align 8
%stackaddr$prim56916 = alloca %struct.ScmObj*, align 8
%_37_4748097 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56128)
store volatile %struct.ScmObj* %_37_4748097, %struct.ScmObj** %stackaddr$prim56916, align 8
%stackaddr$makeclosure56917 = alloca %struct.ScmObj*, align 8
%fptrToInt56918 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51749 to i64
%ae51749 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56918)
store volatile %struct.ScmObj* %ae51749, %struct.ScmObj** %stackaddr$makeclosure56917, align 8
%ae51750 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56919 = alloca %struct.ScmObj*, align 8
%fptrToInt56920 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51751 to i64
%ae51751 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56920)
store volatile %struct.ScmObj* %ae51751, %struct.ScmObj** %stackaddr$makeclosure56919, align 8
%argslist56166$ae517490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56921 = alloca %struct.ScmObj*, align 8
%argslist56166$ae517491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51751, %struct.ScmObj* %argslist56166$ae517490)
store volatile %struct.ScmObj* %argslist56166$ae517491, %struct.ScmObj** %stackaddr$prim56921, align 8
%stackaddr$prim56922 = alloca %struct.ScmObj*, align 8
%argslist56166$ae517492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51750, %struct.ScmObj* %argslist56166$ae517491)
store volatile %struct.ScmObj* %argslist56166$ae517492, %struct.ScmObj** %stackaddr$prim56922, align 8
%clofunc56923 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51749)
musttail call tailcc void %clofunc56923(%struct.ScmObj* %ae51749, %struct.ScmObj* %argslist56166$ae517492)
ret void
}

define tailcc void @proc_clo$ae51749(%struct.ScmObj* %env$ae51749,%struct.ScmObj* %current_45args56130) {
%stackaddr$prim56924 = alloca %struct.ScmObj*, align 8
%_95k48332 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56130)
store volatile %struct.ScmObj* %_95k48332, %struct.ScmObj** %stackaddr$prim56924, align 8
%stackaddr$prim56925 = alloca %struct.ScmObj*, align 8
%current_45args56131 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56130)
store volatile %struct.ScmObj* %current_45args56131, %struct.ScmObj** %stackaddr$prim56925, align 8
%stackaddr$prim56926 = alloca %struct.ScmObj*, align 8
%_37first48095 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56131)
store volatile %struct.ScmObj* %_37first48095, %struct.ScmObj** %stackaddr$prim56926, align 8
%stackaddr$makeclosure56927 = alloca %struct.ScmObj*, align 8
%fptrToInt56928 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51769 to i64
%ae51769 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56928)
store volatile %struct.ScmObj* %ae51769, %struct.ScmObj** %stackaddr$makeclosure56927, align 8
%ae51770 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56929 = alloca %struct.ScmObj*, align 8
%fptrToInt56930 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51771 to i64
%ae51771 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56930)
store volatile %struct.ScmObj* %ae51771, %struct.ScmObj** %stackaddr$makeclosure56929, align 8
%argslist56161$ae517690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56931 = alloca %struct.ScmObj*, align 8
%argslist56161$ae517691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51771, %struct.ScmObj* %argslist56161$ae517690)
store volatile %struct.ScmObj* %argslist56161$ae517691, %struct.ScmObj** %stackaddr$prim56931, align 8
%stackaddr$prim56932 = alloca %struct.ScmObj*, align 8
%argslist56161$ae517692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51770, %struct.ScmObj* %argslist56161$ae517691)
store volatile %struct.ScmObj* %argslist56161$ae517692, %struct.ScmObj** %stackaddr$prim56932, align 8
%clofunc56933 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51769)
musttail call tailcc void %clofunc56933(%struct.ScmObj* %ae51769, %struct.ScmObj* %argslist56161$ae517692)
ret void
}

define tailcc void @proc_clo$ae51769(%struct.ScmObj* %env$ae51769,%struct.ScmObj* %current_45args56133) {
%stackaddr$prim56934 = alloca %struct.ScmObj*, align 8
%_95k48333 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56133)
store volatile %struct.ScmObj* %_95k48333, %struct.ScmObj** %stackaddr$prim56934, align 8
%stackaddr$prim56935 = alloca %struct.ScmObj*, align 8
%current_45args56134 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56133)
store volatile %struct.ScmObj* %current_45args56134, %struct.ScmObj** %stackaddr$prim56935, align 8
%stackaddr$prim56936 = alloca %struct.ScmObj*, align 8
%_37second48093 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56134)
store volatile %struct.ScmObj* %_37second48093, %struct.ScmObj** %stackaddr$prim56936, align 8
%stackaddr$makeclosure56937 = alloca %struct.ScmObj*, align 8
%fptrToInt56938 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51791 to i64
%ae51791 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56938)
store volatile %struct.ScmObj* %ae51791, %struct.ScmObj** %stackaddr$makeclosure56937, align 8
%ae51792 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56939 = alloca %struct.ScmObj*, align 8
%fptrToInt56940 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51793 to i64
%ae51793 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56940)
store volatile %struct.ScmObj* %ae51793, %struct.ScmObj** %stackaddr$makeclosure56939, align 8
%argslist56156$ae517910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56941 = alloca %struct.ScmObj*, align 8
%argslist56156$ae517911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51793, %struct.ScmObj* %argslist56156$ae517910)
store volatile %struct.ScmObj* %argslist56156$ae517911, %struct.ScmObj** %stackaddr$prim56941, align 8
%stackaddr$prim56942 = alloca %struct.ScmObj*, align 8
%argslist56156$ae517912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51792, %struct.ScmObj* %argslist56156$ae517911)
store volatile %struct.ScmObj* %argslist56156$ae517912, %struct.ScmObj** %stackaddr$prim56942, align 8
%clofunc56943 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51791)
musttail call tailcc void %clofunc56943(%struct.ScmObj* %ae51791, %struct.ScmObj* %argslist56156$ae517912)
ret void
}

define tailcc void @proc_clo$ae51791(%struct.ScmObj* %env$ae51791,%struct.ScmObj* %current_45args56136) {
%stackaddr$prim56944 = alloca %struct.ScmObj*, align 8
%_95k48334 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56136)
store volatile %struct.ScmObj* %_95k48334, %struct.ScmObj** %stackaddr$prim56944, align 8
%stackaddr$prim56945 = alloca %struct.ScmObj*, align 8
%current_45args56137 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56136)
store volatile %struct.ScmObj* %current_45args56137, %struct.ScmObj** %stackaddr$prim56945, align 8
%stackaddr$prim56946 = alloca %struct.ScmObj*, align 8
%_37third48091 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56137)
store volatile %struct.ScmObj* %_37third48091, %struct.ScmObj** %stackaddr$prim56946, align 8
%stackaddr$makeclosure56947 = alloca %struct.ScmObj*, align 8
%fptrToInt56948 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51815 to i64
%ae51815 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56948)
store volatile %struct.ScmObj* %ae51815, %struct.ScmObj** %stackaddr$makeclosure56947, align 8
%ae51816 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56949 = alloca %struct.ScmObj*, align 8
%fptrToInt56950 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51817 to i64
%ae51817 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56950)
store volatile %struct.ScmObj* %ae51817, %struct.ScmObj** %stackaddr$makeclosure56949, align 8
%argslist56151$ae518150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56951 = alloca %struct.ScmObj*, align 8
%argslist56151$ae518151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51817, %struct.ScmObj* %argslist56151$ae518150)
store volatile %struct.ScmObj* %argslist56151$ae518151, %struct.ScmObj** %stackaddr$prim56951, align 8
%stackaddr$prim56952 = alloca %struct.ScmObj*, align 8
%argslist56151$ae518152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51816, %struct.ScmObj* %argslist56151$ae518151)
store volatile %struct.ScmObj* %argslist56151$ae518152, %struct.ScmObj** %stackaddr$prim56952, align 8
%clofunc56953 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51815)
musttail call tailcc void %clofunc56953(%struct.ScmObj* %ae51815, %struct.ScmObj* %argslist56151$ae518152)
ret void
}

define tailcc void @proc_clo$ae51815(%struct.ScmObj* %env$ae51815,%struct.ScmObj* %current_45args56139) {
%stackaddr$prim56954 = alloca %struct.ScmObj*, align 8
%_95k48335 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56139)
store volatile %struct.ScmObj* %_95k48335, %struct.ScmObj** %stackaddr$prim56954, align 8
%stackaddr$prim56955 = alloca %struct.ScmObj*, align 8
%current_45args56140 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56139)
store volatile %struct.ScmObj* %current_45args56140, %struct.ScmObj** %stackaddr$prim56955, align 8
%stackaddr$prim56956 = alloca %struct.ScmObj*, align 8
%_37fourth48089 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56140)
store volatile %struct.ScmObj* %_37fourth48089, %struct.ScmObj** %stackaddr$prim56956, align 8
%stackaddr$prim56957 = alloca %struct.ScmObj*, align 8
%anf_45bind48278 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind48278, %struct.ScmObj** %stackaddr$prim56957, align 8
%ae51841 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56958 = alloca %struct.ScmObj*, align 8
%x48150 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51841, %struct.ScmObj* %anf_45bind48278)
store volatile %struct.ScmObj* %x48150, %struct.ScmObj** %stackaddr$prim56958, align 8
%ae51844 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51845 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56959 = alloca %struct.ScmObj*, align 8
%t4802548151 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %x48150, %struct.ScmObj* %ae51844, %struct.ScmObj* %ae51845)
store volatile %struct.ScmObj* %t4802548151, %struct.ScmObj** %stackaddr$prim56959, align 8
%stackaddr$prim56960 = alloca %struct.ScmObj*, align 8
%anf_45bind48279 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind48279, %struct.ScmObj** %stackaddr$prim56960, align 8
%ae51846 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56961 = alloca %struct.ScmObj*, align 8
%a48153 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51846, %struct.ScmObj* %anf_45bind48279)
store volatile %struct.ScmObj* %a48153, %struct.ScmObj** %stackaddr$prim56961, align 8
%stackaddr$prim56962 = alloca %struct.ScmObj*, align 8
%anf_45bind48280 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind48280, %struct.ScmObj** %stackaddr$prim56962, align 8
%ae51848 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56963 = alloca %struct.ScmObj*, align 8
%b48152 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51848, %struct.ScmObj* %anf_45bind48280)
store volatile %struct.ScmObj* %b48152, %struct.ScmObj** %stackaddr$prim56963, align 8
%ae51851 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56964 = alloca %struct.ScmObj*, align 8
%anf_45bind48281 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %x48150, %struct.ScmObj* %ae51851)
store volatile %struct.ScmObj* %anf_45bind48281, %struct.ScmObj** %stackaddr$prim56964, align 8
%ae51853 = call %struct.ScmObj* @const_init_int(i64 3)
%stackaddr$prim56965 = alloca %struct.ScmObj*, align 8
%anf_45bind48282 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %anf_45bind48281, %struct.ScmObj* %ae51853)
store volatile %struct.ScmObj* %anf_45bind48282, %struct.ScmObj** %stackaddr$prim56965, align 8
%ae51855 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56966 = alloca %struct.ScmObj*, align 8
%anf_45bind48283 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %x48150, %struct.ScmObj* %ae51855, %struct.ScmObj* %anf_45bind48282)
store volatile %struct.ScmObj* %anf_45bind48283, %struct.ScmObj** %stackaddr$prim56966, align 8
%ae51858 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56967 = alloca %struct.ScmObj*, align 8
%t4802748155 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a48153, %struct.ScmObj* %ae51858, %struct.ScmObj* %anf_45bind48283)
store volatile %struct.ScmObj* %t4802748155, %struct.ScmObj** %stackaddr$prim56967, align 8
%ae51861 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56968 = alloca %struct.ScmObj*, align 8
%anf_45bind48284 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %x48150, %struct.ScmObj* %ae51861)
store volatile %struct.ScmObj* %anf_45bind48284, %struct.ScmObj** %stackaddr$prim56968, align 8
%ae51863 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim56969 = alloca %struct.ScmObj*, align 8
%anf_45bind48285 = call %struct.ScmObj* @prim__42(%struct.ScmObj* %anf_45bind48284, %struct.ScmObj* %ae51863)
store volatile %struct.ScmObj* %anf_45bind48285, %struct.ScmObj** %stackaddr$prim56969, align 8
%ae51865 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56970 = alloca %struct.ScmObj*, align 8
%anf_45bind48286 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %x48150, %struct.ScmObj* %ae51865, %struct.ScmObj* %anf_45bind48285)
store volatile %struct.ScmObj* %anf_45bind48286, %struct.ScmObj** %stackaddr$prim56970, align 8
%ae51868 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56971 = alloca %struct.ScmObj*, align 8
%t4802648154 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %b48152, %struct.ScmObj* %ae51868, %struct.ScmObj* %anf_45bind48286)
store volatile %struct.ScmObj* %t4802648154, %struct.ScmObj** %stackaddr$prim56971, align 8
%stackaddr$prim56972 = alloca %struct.ScmObj*, align 8
%anf_45bind48287 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind48287, %struct.ScmObj** %stackaddr$prim56972, align 8
%ae51870 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56973 = alloca %struct.ScmObj*, align 8
%a48157 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51870, %struct.ScmObj* %anf_45bind48287)
store volatile %struct.ScmObj* %a48157, %struct.ScmObj** %stackaddr$prim56973, align 8
%stackaddr$prim56974 = alloca %struct.ScmObj*, align 8
%anf_45bind48288 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind48288, %struct.ScmObj** %stackaddr$prim56974, align 8
%ae51872 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56975 = alloca %struct.ScmObj*, align 8
%b48156 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51872, %struct.ScmObj* %anf_45bind48288)
store volatile %struct.ScmObj* %b48156, %struct.ScmObj** %stackaddr$prim56975, align 8
%ae51875 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56976 = alloca %struct.ScmObj*, align 8
%anf_45bind48289 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %x48150, %struct.ScmObj* %ae51875)
store volatile %struct.ScmObj* %anf_45bind48289, %struct.ScmObj** %stackaddr$prim56976, align 8
%ae51877 = call %struct.ScmObj* @const_init_int(i64 3)
%stackaddr$prim56977 = alloca %struct.ScmObj*, align 8
%anf_45bind48290 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %anf_45bind48289, %struct.ScmObj* %ae51877)
store volatile %struct.ScmObj* %anf_45bind48290, %struct.ScmObj** %stackaddr$prim56977, align 8
%ae51879 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56978 = alloca %struct.ScmObj*, align 8
%anf_45bind48291 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %x48150, %struct.ScmObj* %ae51879, %struct.ScmObj* %anf_45bind48290)
store volatile %struct.ScmObj* %anf_45bind48291, %struct.ScmObj** %stackaddr$prim56978, align 8
%ae51882 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56979 = alloca %struct.ScmObj*, align 8
%t4802948159 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a48157, %struct.ScmObj* %ae51882, %struct.ScmObj* %anf_45bind48291)
store volatile %struct.ScmObj* %t4802948159, %struct.ScmObj** %stackaddr$prim56979, align 8
%ae51885 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56980 = alloca %struct.ScmObj*, align 8
%anf_45bind48292 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %x48150, %struct.ScmObj* %ae51885)
store volatile %struct.ScmObj* %anf_45bind48292, %struct.ScmObj** %stackaddr$prim56980, align 8
%ae51887 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim56981 = alloca %struct.ScmObj*, align 8
%anf_45bind48293 = call %struct.ScmObj* @prim__42(%struct.ScmObj* %anf_45bind48292, %struct.ScmObj* %ae51887)
store volatile %struct.ScmObj* %anf_45bind48293, %struct.ScmObj** %stackaddr$prim56981, align 8
%ae51889 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56982 = alloca %struct.ScmObj*, align 8
%anf_45bind48294 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %x48150, %struct.ScmObj* %ae51889, %struct.ScmObj* %anf_45bind48293)
store volatile %struct.ScmObj* %anf_45bind48294, %struct.ScmObj** %stackaddr$prim56982, align 8
%ae51892 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56983 = alloca %struct.ScmObj*, align 8
%t4802848158 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %b48156, %struct.ScmObj* %ae51892, %struct.ScmObj* %anf_45bind48294)
store volatile %struct.ScmObj* %t4802848158, %struct.ScmObj** %stackaddr$prim56983, align 8
%ae51895 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56984 = alloca %struct.ScmObj*, align 8
%anf_45bind48295 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %x48150, %struct.ScmObj* %ae51895)
store volatile %struct.ScmObj* %anf_45bind48295, %struct.ScmObj** %stackaddr$prim56984, align 8
%ae51897 = call %struct.ScmObj* @const_init_int(i64 3)
%stackaddr$prim56985 = alloca %struct.ScmObj*, align 8
%anf_45bind48296 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %anf_45bind48295, %struct.ScmObj* %ae51897)
store volatile %struct.ScmObj* %anf_45bind48296, %struct.ScmObj** %stackaddr$prim56985, align 8
%ae51899 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56986 = alloca %struct.ScmObj*, align 8
%a48160 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %x48150, %struct.ScmObj* %ae51899, %struct.ScmObj* %anf_45bind48296)
store volatile %struct.ScmObj* %a48160, %struct.ScmObj** %stackaddr$prim56986, align 8
%ae51902 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56987 = alloca %struct.ScmObj*, align 8
%anf_45bind48297 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %x48150, %struct.ScmObj* %ae51902)
store volatile %struct.ScmObj* %anf_45bind48297, %struct.ScmObj** %stackaddr$prim56987, align 8
%ae51904 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim56988 = alloca %struct.ScmObj*, align 8
%anf_45bind48298 = call %struct.ScmObj* @prim__42(%struct.ScmObj* %anf_45bind48297, %struct.ScmObj* %ae51904)
store volatile %struct.ScmObj* %anf_45bind48298, %struct.ScmObj** %stackaddr$prim56988, align 8
%ae51906 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56989 = alloca %struct.ScmObj*, align 8
%b48161 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %x48150, %struct.ScmObj* %ae51906, %struct.ScmObj* %anf_45bind48298)
store volatile %struct.ScmObj* %b48161, %struct.ScmObj** %stackaddr$prim56989, align 8
%e48162 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim56990 = alloca %struct.ScmObj*, align 8
%anf_45bind48299 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind48299, %struct.ScmObj** %stackaddr$prim56990, align 8
%ae51908 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56991 = alloca %struct.ScmObj*, align 8
%e48164 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51908, %struct.ScmObj* %anf_45bind48299)
store volatile %struct.ScmObj* %e48164, %struct.ScmObj** %stackaddr$prim56991, align 8
%stackaddr$prim56992 = alloca %struct.ScmObj*, align 8
%anf_45bind48300 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind48300, %struct.ScmObj** %stackaddr$prim56992, align 8
%ae51910 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56993 = alloca %struct.ScmObj*, align 8
%f48163 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51910, %struct.ScmObj* %anf_45bind48300)
store volatile %struct.ScmObj* %f48163, %struct.ScmObj** %stackaddr$prim56993, align 8
%ae51913 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51914 = call %struct.ScmObj* @const_init_int(i64 5)
%stackaddr$prim56994 = alloca %struct.ScmObj*, align 8
%t4803148166 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %e48164, %struct.ScmObj* %ae51913, %struct.ScmObj* %ae51914)
store volatile %struct.ScmObj* %t4803148166, %struct.ScmObj** %stackaddr$prim56994, align 8
%ae51916 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56995 = alloca %struct.ScmObj*, align 8
%anf_45bind48301 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %e48164, %struct.ScmObj* %ae51916)
store volatile %struct.ScmObj* %anf_45bind48301, %struct.ScmObj** %stackaddr$prim56995, align 8
%ae51918 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56996 = alloca %struct.ScmObj*, align 8
%t4803048165 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %f48163, %struct.ScmObj* %ae51918, %struct.ScmObj* %anf_45bind48301)
store volatile %struct.ScmObj* %t4803048165, %struct.ScmObj** %stackaddr$prim56996, align 8
%ae51921 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56997 = alloca %struct.ScmObj*, align 8
%anf_45bind48302 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %x48150, %struct.ScmObj* %ae51921)
store volatile %struct.ScmObj* %anf_45bind48302, %struct.ScmObj** %stackaddr$prim56997, align 8
%ae51923 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56998 = alloca %struct.ScmObj*, align 8
%anf_45bind48303 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %f48163, %struct.ScmObj* %ae51923)
store volatile %struct.ScmObj* %anf_45bind48303, %struct.ScmObj** %stackaddr$prim56998, align 8
%stackaddr$prim56999 = alloca %struct.ScmObj*, align 8
%cpsprim48336 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %anf_45bind48302, %struct.ScmObj* %anf_45bind48303)
store volatile %struct.ScmObj* %cpsprim48336, %struct.ScmObj** %stackaddr$prim56999, align 8
%stackaddr$makeclosure57000 = alloca %struct.ScmObj*, align 8
%fptrToInt57001 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51926 to i64
%ae51926 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57001)
store volatile %struct.ScmObj* %ae51926, %struct.ScmObj** %stackaddr$makeclosure57000, align 8
%ae51927 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56146$ae519260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57002 = alloca %struct.ScmObj*, align 8
%argslist56146$ae519261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48336, %struct.ScmObj* %argslist56146$ae519260)
store volatile %struct.ScmObj* %argslist56146$ae519261, %struct.ScmObj** %stackaddr$prim57002, align 8
%stackaddr$prim57003 = alloca %struct.ScmObj*, align 8
%argslist56146$ae519262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51927, %struct.ScmObj* %argslist56146$ae519261)
store volatile %struct.ScmObj* %argslist56146$ae519262, %struct.ScmObj** %stackaddr$prim57003, align 8
%clofunc57004 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51926)
musttail call tailcc void %clofunc57004(%struct.ScmObj* %ae51926, %struct.ScmObj* %argslist56146$ae519262)
ret void
}

define tailcc void @proc_clo$ae51926(%struct.ScmObj* %env$ae51926,%struct.ScmObj* %current_45args56142) {
%stackaddr$prim57005 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56142)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim57005, align 8
%stackaddr$prim57006 = alloca %struct.ScmObj*, align 8
%current_45args56143 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56142)
store volatile %struct.ScmObj* %current_45args56143, %struct.ScmObj** %stackaddr$prim57006, align 8
%stackaddr$prim57007 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56143)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim57007, align 8
%stackaddr$prim57008 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim57008, align 8
%argslist56145$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57009 = alloca %struct.ScmObj*, align 8
%argslist56145$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist56145$k0)
store volatile %struct.ScmObj* %argslist56145$k1, %struct.ScmObj** %stackaddr$prim57009, align 8
%clofunc57010 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc57010(%struct.ScmObj* %k, %struct.ScmObj* %argslist56145$k1)
ret void
}

define tailcc void @proc_clo$ae51817(%struct.ScmObj* %env$ae51817,%struct.ScmObj* %current_45args56147) {
%stackaddr$prim57011 = alloca %struct.ScmObj*, align 8
%k48337 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56147)
store volatile %struct.ScmObj* %k48337, %struct.ScmObj** %stackaddr$prim57011, align 8
%stackaddr$prim57012 = alloca %struct.ScmObj*, align 8
%current_45args56148 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56147)
store volatile %struct.ScmObj* %current_45args56148, %struct.ScmObj** %stackaddr$prim57012, align 8
%stackaddr$prim57013 = alloca %struct.ScmObj*, align 8
%x48090 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56148)
store volatile %struct.ScmObj* %x48090, %struct.ScmObj** %stackaddr$prim57013, align 8
%stackaddr$prim57014 = alloca %struct.ScmObj*, align 8
%anf_45bind48275 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48090)
store volatile %struct.ScmObj* %anf_45bind48275, %struct.ScmObj** %stackaddr$prim57014, align 8
%stackaddr$prim57015 = alloca %struct.ScmObj*, align 8
%anf_45bind48276 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48275)
store volatile %struct.ScmObj* %anf_45bind48276, %struct.ScmObj** %stackaddr$prim57015, align 8
%stackaddr$prim57016 = alloca %struct.ScmObj*, align 8
%anf_45bind48277 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48276)
store volatile %struct.ScmObj* %anf_45bind48277, %struct.ScmObj** %stackaddr$prim57016, align 8
%stackaddr$prim57017 = alloca %struct.ScmObj*, align 8
%cpsprim48338 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48277)
store volatile %struct.ScmObj* %cpsprim48338, %struct.ScmObj** %stackaddr$prim57017, align 8
%ae51823 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56150$k483370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57018 = alloca %struct.ScmObj*, align 8
%argslist56150$k483371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48338, %struct.ScmObj* %argslist56150$k483370)
store volatile %struct.ScmObj* %argslist56150$k483371, %struct.ScmObj** %stackaddr$prim57018, align 8
%stackaddr$prim57019 = alloca %struct.ScmObj*, align 8
%argslist56150$k483372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51823, %struct.ScmObj* %argslist56150$k483371)
store volatile %struct.ScmObj* %argslist56150$k483372, %struct.ScmObj** %stackaddr$prim57019, align 8
%clofunc57020 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48337)
musttail call tailcc void %clofunc57020(%struct.ScmObj* %k48337, %struct.ScmObj* %argslist56150$k483372)
ret void
}

define tailcc void @proc_clo$ae51793(%struct.ScmObj* %env$ae51793,%struct.ScmObj* %current_45args56152) {
%stackaddr$prim57021 = alloca %struct.ScmObj*, align 8
%k48339 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56152)
store volatile %struct.ScmObj* %k48339, %struct.ScmObj** %stackaddr$prim57021, align 8
%stackaddr$prim57022 = alloca %struct.ScmObj*, align 8
%current_45args56153 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56152)
store volatile %struct.ScmObj* %current_45args56153, %struct.ScmObj** %stackaddr$prim57022, align 8
%stackaddr$prim57023 = alloca %struct.ScmObj*, align 8
%x48092 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56153)
store volatile %struct.ScmObj* %x48092, %struct.ScmObj** %stackaddr$prim57023, align 8
%stackaddr$prim57024 = alloca %struct.ScmObj*, align 8
%anf_45bind48273 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48092)
store volatile %struct.ScmObj* %anf_45bind48273, %struct.ScmObj** %stackaddr$prim57024, align 8
%stackaddr$prim57025 = alloca %struct.ScmObj*, align 8
%anf_45bind48274 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48273)
store volatile %struct.ScmObj* %anf_45bind48274, %struct.ScmObj** %stackaddr$prim57025, align 8
%stackaddr$prim57026 = alloca %struct.ScmObj*, align 8
%cpsprim48340 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48274)
store volatile %struct.ScmObj* %cpsprim48340, %struct.ScmObj** %stackaddr$prim57026, align 8
%ae51798 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56155$k483390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57027 = alloca %struct.ScmObj*, align 8
%argslist56155$k483391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48340, %struct.ScmObj* %argslist56155$k483390)
store volatile %struct.ScmObj* %argslist56155$k483391, %struct.ScmObj** %stackaddr$prim57027, align 8
%stackaddr$prim57028 = alloca %struct.ScmObj*, align 8
%argslist56155$k483392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51798, %struct.ScmObj* %argslist56155$k483391)
store volatile %struct.ScmObj* %argslist56155$k483392, %struct.ScmObj** %stackaddr$prim57028, align 8
%clofunc57029 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48339)
musttail call tailcc void %clofunc57029(%struct.ScmObj* %k48339, %struct.ScmObj* %argslist56155$k483392)
ret void
}

define tailcc void @proc_clo$ae51771(%struct.ScmObj* %env$ae51771,%struct.ScmObj* %current_45args56157) {
%stackaddr$prim57030 = alloca %struct.ScmObj*, align 8
%k48341 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56157)
store volatile %struct.ScmObj* %k48341, %struct.ScmObj** %stackaddr$prim57030, align 8
%stackaddr$prim57031 = alloca %struct.ScmObj*, align 8
%current_45args56158 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56157)
store volatile %struct.ScmObj* %current_45args56158, %struct.ScmObj** %stackaddr$prim57031, align 8
%stackaddr$prim57032 = alloca %struct.ScmObj*, align 8
%x48094 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56158)
store volatile %struct.ScmObj* %x48094, %struct.ScmObj** %stackaddr$prim57032, align 8
%stackaddr$prim57033 = alloca %struct.ScmObj*, align 8
%anf_45bind48272 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48094)
store volatile %struct.ScmObj* %anf_45bind48272, %struct.ScmObj** %stackaddr$prim57033, align 8
%stackaddr$prim57034 = alloca %struct.ScmObj*, align 8
%cpsprim48342 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48272)
store volatile %struct.ScmObj* %cpsprim48342, %struct.ScmObj** %stackaddr$prim57034, align 8
%ae51775 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56160$k483410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57035 = alloca %struct.ScmObj*, align 8
%argslist56160$k483411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48342, %struct.ScmObj* %argslist56160$k483410)
store volatile %struct.ScmObj* %argslist56160$k483411, %struct.ScmObj** %stackaddr$prim57035, align 8
%stackaddr$prim57036 = alloca %struct.ScmObj*, align 8
%argslist56160$k483412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51775, %struct.ScmObj* %argslist56160$k483411)
store volatile %struct.ScmObj* %argslist56160$k483412, %struct.ScmObj** %stackaddr$prim57036, align 8
%clofunc57037 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48341)
musttail call tailcc void %clofunc57037(%struct.ScmObj* %k48341, %struct.ScmObj* %argslist56160$k483412)
ret void
}

define tailcc void @proc_clo$ae51751(%struct.ScmObj* %env$ae51751,%struct.ScmObj* %current_45args56162) {
%stackaddr$prim57038 = alloca %struct.ScmObj*, align 8
%k48343 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56162)
store volatile %struct.ScmObj* %k48343, %struct.ScmObj** %stackaddr$prim57038, align 8
%stackaddr$prim57039 = alloca %struct.ScmObj*, align 8
%current_45args56163 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56162)
store volatile %struct.ScmObj* %current_45args56163, %struct.ScmObj** %stackaddr$prim57039, align 8
%stackaddr$prim57040 = alloca %struct.ScmObj*, align 8
%x48096 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56163)
store volatile %struct.ScmObj* %x48096, %struct.ScmObj** %stackaddr$prim57040, align 8
%stackaddr$prim57041 = alloca %struct.ScmObj*, align 8
%cpsprim48344 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48096)
store volatile %struct.ScmObj* %cpsprim48344, %struct.ScmObj** %stackaddr$prim57041, align 8
%ae51754 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56165$k483430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57042 = alloca %struct.ScmObj*, align 8
%argslist56165$k483431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48344, %struct.ScmObj* %argslist56165$k483430)
store volatile %struct.ScmObj* %argslist56165$k483431, %struct.ScmObj** %stackaddr$prim57042, align 8
%stackaddr$prim57043 = alloca %struct.ScmObj*, align 8
%argslist56165$k483432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51754, %struct.ScmObj* %argslist56165$k483431)
store volatile %struct.ScmObj* %argslist56165$k483432, %struct.ScmObj** %stackaddr$prim57043, align 8
%clofunc57044 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48343)
musttail call tailcc void %clofunc57044(%struct.ScmObj* %k48343, %struct.ScmObj* %argslist56165$k483432)
ret void
}

define tailcc void @proc_clo$ae51653(%struct.ScmObj* %env$ae51653,%struct.ScmObj* %args4809848345) {
%stackaddr$env-ref57045 = alloca %struct.ScmObj*, align 8
%_37foldl148037 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51653, i64 0)
store %struct.ScmObj* %_37foldl148037, %struct.ScmObj** %stackaddr$env-ref57045
%stackaddr$prim57046 = alloca %struct.ScmObj*, align 8
%k48346 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4809848345)
store volatile %struct.ScmObj* %k48346, %struct.ScmObj** %stackaddr$prim57046, align 8
%stackaddr$prim57047 = alloca %struct.ScmObj*, align 8
%args48098 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4809848345)
store volatile %struct.ScmObj* %args48098, %struct.ScmObj** %stackaddr$prim57047, align 8
%stackaddr$prim57048 = alloca %struct.ScmObj*, align 8
%anf_45bind48266 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args48098)
store volatile %struct.ScmObj* %anf_45bind48266, %struct.ScmObj** %stackaddr$prim57048, align 8
%truthy$cmp57049 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48266)
%cmp$cmp57049 = icmp eq i64 %truthy$cmp57049, 1
br i1 %cmp$cmp57049, label %truebranch$cmp57049, label %falsebranch$cmp57049
truebranch$cmp57049:
%ae51659 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51660 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist56167$k483460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57050 = alloca %struct.ScmObj*, align 8
%argslist56167$k483461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51660, %struct.ScmObj* %argslist56167$k483460)
store volatile %struct.ScmObj* %argslist56167$k483461, %struct.ScmObj** %stackaddr$prim57050, align 8
%stackaddr$prim57051 = alloca %struct.ScmObj*, align 8
%argslist56167$k483462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51659, %struct.ScmObj* %argslist56167$k483461)
store volatile %struct.ScmObj* %argslist56167$k483462, %struct.ScmObj** %stackaddr$prim57051, align 8
%clofunc57052 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48346)
musttail call tailcc void %clofunc57052(%struct.ScmObj* %k48346, %struct.ScmObj* %argslist56167$k483462)
ret void
falsebranch$cmp57049:
%stackaddr$prim57053 = alloca %struct.ScmObj*, align 8
%anf_45bind48267 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48098)
store volatile %struct.ScmObj* %anf_45bind48267, %struct.ScmObj** %stackaddr$prim57053, align 8
%stackaddr$prim57054 = alloca %struct.ScmObj*, align 8
%anf_45bind48268 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48267)
store volatile %struct.ScmObj* %anf_45bind48268, %struct.ScmObj** %stackaddr$prim57054, align 8
%truthy$cmp57055 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48268)
%cmp$cmp57055 = icmp eq i64 %truthy$cmp57055, 1
br i1 %cmp$cmp57055, label %truebranch$cmp57055, label %falsebranch$cmp57055
truebranch$cmp57055:
%stackaddr$prim57056 = alloca %struct.ScmObj*, align 8
%cpsprim48347 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48098)
store volatile %struct.ScmObj* %cpsprim48347, %struct.ScmObj** %stackaddr$prim57056, align 8
%ae51672 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56168$k483460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57057 = alloca %struct.ScmObj*, align 8
%argslist56168$k483461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48347, %struct.ScmObj* %argslist56168$k483460)
store volatile %struct.ScmObj* %argslist56168$k483461, %struct.ScmObj** %stackaddr$prim57057, align 8
%stackaddr$prim57058 = alloca %struct.ScmObj*, align 8
%argslist56168$k483462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51672, %struct.ScmObj* %argslist56168$k483461)
store volatile %struct.ScmObj* %argslist56168$k483462, %struct.ScmObj** %stackaddr$prim57058, align 8
%clofunc57059 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48346)
musttail call tailcc void %clofunc57059(%struct.ScmObj* %k48346, %struct.ScmObj* %argslist56168$k483462)
ret void
falsebranch$cmp57055:
%stackaddr$makeclosure57060 = alloca %struct.ScmObj*, align 8
%fptrToInt57061 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51677 to i64
%ae51677 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57061)
store volatile %struct.ScmObj* %ae51677, %struct.ScmObj** %stackaddr$makeclosure57060, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51677, %struct.ScmObj* %_37foldl148037, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51677, %struct.ScmObj* %args48098, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51677, %struct.ScmObj* %k48346, i64 2)
%ae51678 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57062 = alloca %struct.ScmObj*, align 8
%fptrToInt57063 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51679 to i64
%ae51679 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57063)
store volatile %struct.ScmObj* %ae51679, %struct.ScmObj** %stackaddr$makeclosure57062, align 8
%argslist56178$ae516770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57064 = alloca %struct.ScmObj*, align 8
%argslist56178$ae516771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51679, %struct.ScmObj* %argslist56178$ae516770)
store volatile %struct.ScmObj* %argslist56178$ae516771, %struct.ScmObj** %stackaddr$prim57064, align 8
%stackaddr$prim57065 = alloca %struct.ScmObj*, align 8
%argslist56178$ae516772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51678, %struct.ScmObj* %argslist56178$ae516771)
store volatile %struct.ScmObj* %argslist56178$ae516772, %struct.ScmObj** %stackaddr$prim57065, align 8
%clofunc57066 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51677)
musttail call tailcc void %clofunc57066(%struct.ScmObj* %ae51677, %struct.ScmObj* %argslist56178$ae516772)
ret void
}

define tailcc void @proc_clo$ae51677(%struct.ScmObj* %env$ae51677,%struct.ScmObj* %current_45args56169) {
%stackaddr$env-ref57067 = alloca %struct.ScmObj*, align 8
%_37foldl148037 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51677, i64 0)
store %struct.ScmObj* %_37foldl148037, %struct.ScmObj** %stackaddr$env-ref57067
%stackaddr$env-ref57068 = alloca %struct.ScmObj*, align 8
%args48098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51677, i64 1)
store %struct.ScmObj* %args48098, %struct.ScmObj** %stackaddr$env-ref57068
%stackaddr$env-ref57069 = alloca %struct.ScmObj*, align 8
%k48346 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51677, i64 2)
store %struct.ScmObj* %k48346, %struct.ScmObj** %stackaddr$env-ref57069
%stackaddr$prim57070 = alloca %struct.ScmObj*, align 8
%_95k48348 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56169)
store volatile %struct.ScmObj* %_95k48348, %struct.ScmObj** %stackaddr$prim57070, align 8
%stackaddr$prim57071 = alloca %struct.ScmObj*, align 8
%current_45args56170 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56169)
store volatile %struct.ScmObj* %current_45args56170, %struct.ScmObj** %stackaddr$prim57071, align 8
%stackaddr$prim57072 = alloca %struct.ScmObj*, align 8
%anf_45bind48269 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56170)
store volatile %struct.ScmObj* %anf_45bind48269, %struct.ScmObj** %stackaddr$prim57072, align 8
%stackaddr$prim57073 = alloca %struct.ScmObj*, align 8
%anf_45bind48270 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48098)
store volatile %struct.ScmObj* %anf_45bind48270, %struct.ScmObj** %stackaddr$prim57073, align 8
%stackaddr$prim57074 = alloca %struct.ScmObj*, align 8
%anf_45bind48271 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48098)
store volatile %struct.ScmObj* %anf_45bind48271, %struct.ScmObj** %stackaddr$prim57074, align 8
%argslist56172$_37foldl1480370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57075 = alloca %struct.ScmObj*, align 8
%argslist56172$_37foldl1480371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48271, %struct.ScmObj* %argslist56172$_37foldl1480370)
store volatile %struct.ScmObj* %argslist56172$_37foldl1480371, %struct.ScmObj** %stackaddr$prim57075, align 8
%stackaddr$prim57076 = alloca %struct.ScmObj*, align 8
%argslist56172$_37foldl1480372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48270, %struct.ScmObj* %argslist56172$_37foldl1480371)
store volatile %struct.ScmObj* %argslist56172$_37foldl1480372, %struct.ScmObj** %stackaddr$prim57076, align 8
%stackaddr$prim57077 = alloca %struct.ScmObj*, align 8
%argslist56172$_37foldl1480373 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48269, %struct.ScmObj* %argslist56172$_37foldl1480372)
store volatile %struct.ScmObj* %argslist56172$_37foldl1480373, %struct.ScmObj** %stackaddr$prim57077, align 8
%stackaddr$prim57078 = alloca %struct.ScmObj*, align 8
%argslist56172$_37foldl1480374 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48346, %struct.ScmObj* %argslist56172$_37foldl1480373)
store volatile %struct.ScmObj* %argslist56172$_37foldl1480374, %struct.ScmObj** %stackaddr$prim57078, align 8
%clofunc57079 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148037)
musttail call tailcc void %clofunc57079(%struct.ScmObj* %_37foldl148037, %struct.ScmObj* %argslist56172$_37foldl1480374)
ret void
}

define tailcc void @proc_clo$ae51679(%struct.ScmObj* %env$ae51679,%struct.ScmObj* %current_45args56173) {
%stackaddr$prim57080 = alloca %struct.ScmObj*, align 8
%k48349 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56173)
store volatile %struct.ScmObj* %k48349, %struct.ScmObj** %stackaddr$prim57080, align 8
%stackaddr$prim57081 = alloca %struct.ScmObj*, align 8
%current_45args56174 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56173)
store volatile %struct.ScmObj* %current_45args56174, %struct.ScmObj** %stackaddr$prim57081, align 8
%stackaddr$prim57082 = alloca %struct.ScmObj*, align 8
%n48100 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56174)
store volatile %struct.ScmObj* %n48100, %struct.ScmObj** %stackaddr$prim57082, align 8
%stackaddr$prim57083 = alloca %struct.ScmObj*, align 8
%current_45args56175 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56174)
store volatile %struct.ScmObj* %current_45args56175, %struct.ScmObj** %stackaddr$prim57083, align 8
%stackaddr$prim57084 = alloca %struct.ScmObj*, align 8
%v48099 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56175)
store volatile %struct.ScmObj* %v48099, %struct.ScmObj** %stackaddr$prim57084, align 8
%stackaddr$prim57085 = alloca %struct.ScmObj*, align 8
%cpsprim48350 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v48099, %struct.ScmObj* %n48100)
store volatile %struct.ScmObj* %cpsprim48350, %struct.ScmObj** %stackaddr$prim57085, align 8
%ae51683 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56177$k483490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57086 = alloca %struct.ScmObj*, align 8
%argslist56177$k483491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48350, %struct.ScmObj* %argslist56177$k483490)
store volatile %struct.ScmObj* %argslist56177$k483491, %struct.ScmObj** %stackaddr$prim57086, align 8
%stackaddr$prim57087 = alloca %struct.ScmObj*, align 8
%argslist56177$k483492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51683, %struct.ScmObj* %argslist56177$k483491)
store volatile %struct.ScmObj* %argslist56177$k483492, %struct.ScmObj** %stackaddr$prim57087, align 8
%clofunc57088 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48349)
musttail call tailcc void %clofunc57088(%struct.ScmObj* %k48349, %struct.ScmObj* %argslist56177$k483492)
ret void
}

define tailcc void @proc_clo$ae51249(%struct.ScmObj* %env$ae51249,%struct.ScmObj* %current_45args56180) {
%stackaddr$prim57089 = alloca %struct.ScmObj*, align 8
%k48351 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56180)
store volatile %struct.ScmObj* %k48351, %struct.ScmObj** %stackaddr$prim57089, align 8
%stackaddr$prim57090 = alloca %struct.ScmObj*, align 8
%current_45args56181 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56180)
store volatile %struct.ScmObj* %current_45args56181, %struct.ScmObj** %stackaddr$prim57090, align 8
%stackaddr$prim57091 = alloca %struct.ScmObj*, align 8
%v48103 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56181)
store volatile %struct.ScmObj* %v48103, %struct.ScmObj** %stackaddr$prim57091, align 8
%stackaddr$prim57092 = alloca %struct.ScmObj*, align 8
%current_45args56182 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56181)
store volatile %struct.ScmObj* %current_45args56182, %struct.ScmObj** %stackaddr$prim57092, align 8
%stackaddr$prim57093 = alloca %struct.ScmObj*, align 8
%lst48102 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56182)
store volatile %struct.ScmObj* %lst48102, %struct.ScmObj** %stackaddr$prim57093, align 8
%ae51250 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57094 = alloca %struct.ScmObj*, align 8
%lst48104 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51250, %struct.ScmObj* %lst48102)
store volatile %struct.ScmObj* %lst48104, %struct.ScmObj** %stackaddr$prim57094, align 8
%stackaddr$makeclosure57095 = alloca %struct.ScmObj*, align 8
%fptrToInt57096 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51252 to i64
%ae51252 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57096)
store volatile %struct.ScmObj* %ae51252, %struct.ScmObj** %stackaddr$makeclosure57095, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51252, %struct.ScmObj* %lst48104, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51252, %struct.ScmObj* %v48103, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51252, %struct.ScmObj* %k48351, i64 2)
%ae51253 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57097 = alloca %struct.ScmObj*, align 8
%fptrToInt57098 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51254 to i64
%ae51254 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57098)
store volatile %struct.ScmObj* %ae51254, %struct.ScmObj** %stackaddr$makeclosure57097, align 8
%argslist56204$ae512520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57099 = alloca %struct.ScmObj*, align 8
%argslist56204$ae512521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51254, %struct.ScmObj* %argslist56204$ae512520)
store volatile %struct.ScmObj* %argslist56204$ae512521, %struct.ScmObj** %stackaddr$prim57099, align 8
%stackaddr$prim57100 = alloca %struct.ScmObj*, align 8
%argslist56204$ae512522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51253, %struct.ScmObj* %argslist56204$ae512521)
store volatile %struct.ScmObj* %argslist56204$ae512522, %struct.ScmObj** %stackaddr$prim57100, align 8
%clofunc57101 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51252)
musttail call tailcc void %clofunc57101(%struct.ScmObj* %ae51252, %struct.ScmObj* %argslist56204$ae512522)
ret void
}

define tailcc void @proc_clo$ae51252(%struct.ScmObj* %env$ae51252,%struct.ScmObj* %current_45args56184) {
%stackaddr$env-ref57102 = alloca %struct.ScmObj*, align 8
%lst48104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51252, i64 0)
store %struct.ScmObj* %lst48104, %struct.ScmObj** %stackaddr$env-ref57102
%stackaddr$env-ref57103 = alloca %struct.ScmObj*, align 8
%v48103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51252, i64 1)
store %struct.ScmObj* %v48103, %struct.ScmObj** %stackaddr$env-ref57103
%stackaddr$env-ref57104 = alloca %struct.ScmObj*, align 8
%k48351 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51252, i64 2)
store %struct.ScmObj* %k48351, %struct.ScmObj** %stackaddr$env-ref57104
%stackaddr$prim57105 = alloca %struct.ScmObj*, align 8
%_95k48352 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56184)
store volatile %struct.ScmObj* %_95k48352, %struct.ScmObj** %stackaddr$prim57105, align 8
%stackaddr$prim57106 = alloca %struct.ScmObj*, align 8
%current_45args56185 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56184)
store volatile %struct.ScmObj* %current_45args56185, %struct.ScmObj** %stackaddr$prim57106, align 8
%stackaddr$prim57107 = alloca %struct.ScmObj*, align 8
%anf_45bind48258 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56185)
store volatile %struct.ScmObj* %anf_45bind48258, %struct.ScmObj** %stackaddr$prim57107, align 8
%stackaddr$makeclosure57108 = alloca %struct.ScmObj*, align 8
%fptrToInt57109 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51268 to i64
%ae51268 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57109)
store volatile %struct.ScmObj* %ae51268, %struct.ScmObj** %stackaddr$makeclosure57108, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51268, %struct.ScmObj* %lst48104, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51268, %struct.ScmObj* %v48103, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51268, %struct.ScmObj* %k48351, i64 2)
%stackaddr$makeclosure57110 = alloca %struct.ScmObj*, align 8
%fptrToInt57111 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51269 to i64
%ae51269 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57111)
store volatile %struct.ScmObj* %ae51269, %struct.ScmObj** %stackaddr$makeclosure57110, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51269, %struct.ScmObj* %lst48104, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51269, %struct.ScmObj* %v48103, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51269, %struct.ScmObj* %k48351, i64 2)
%argslist56199$anf_45bind482580 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57112 = alloca %struct.ScmObj*, align 8
%argslist56199$anf_45bind482581 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51269, %struct.ScmObj* %argslist56199$anf_45bind482580)
store volatile %struct.ScmObj* %argslist56199$anf_45bind482581, %struct.ScmObj** %stackaddr$prim57112, align 8
%stackaddr$prim57113 = alloca %struct.ScmObj*, align 8
%argslist56199$anf_45bind482582 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51268, %struct.ScmObj* %argslist56199$anf_45bind482581)
store volatile %struct.ScmObj* %argslist56199$anf_45bind482582, %struct.ScmObj** %stackaddr$prim57113, align 8
%clofunc57114 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48258)
musttail call tailcc void %clofunc57114(%struct.ScmObj* %anf_45bind48258, %struct.ScmObj* %argslist56199$anf_45bind482582)
ret void
}

define tailcc void @proc_clo$ae51268(%struct.ScmObj* %env$ae51268,%struct.ScmObj* %current_45args56187) {
%stackaddr$env-ref57115 = alloca %struct.ScmObj*, align 8
%lst48104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51268, i64 0)
store %struct.ScmObj* %lst48104, %struct.ScmObj** %stackaddr$env-ref57115
%stackaddr$env-ref57116 = alloca %struct.ScmObj*, align 8
%v48103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51268, i64 1)
store %struct.ScmObj* %v48103, %struct.ScmObj** %stackaddr$env-ref57116
%stackaddr$env-ref57117 = alloca %struct.ScmObj*, align 8
%k48351 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51268, i64 2)
store %struct.ScmObj* %k48351, %struct.ScmObj** %stackaddr$env-ref57117
%stackaddr$prim57118 = alloca %struct.ScmObj*, align 8
%_95k48353 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56187)
store volatile %struct.ScmObj* %_95k48353, %struct.ScmObj** %stackaddr$prim57118, align 8
%stackaddr$prim57119 = alloca %struct.ScmObj*, align 8
%current_45args56188 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56187)
store volatile %struct.ScmObj* %current_45args56188, %struct.ScmObj** %stackaddr$prim57119, align 8
%stackaddr$prim57120 = alloca %struct.ScmObj*, align 8
%cc48105 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56188)
store volatile %struct.ScmObj* %cc48105, %struct.ScmObj** %stackaddr$prim57120, align 8
%ae51377 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57121 = alloca %struct.ScmObj*, align 8
%anf_45bind48259 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48104, %struct.ScmObj* %ae51377)
store volatile %struct.ScmObj* %anf_45bind48259, %struct.ScmObj** %stackaddr$prim57121, align 8
%stackaddr$prim57122 = alloca %struct.ScmObj*, align 8
%anf_45bind48260 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48259)
store volatile %struct.ScmObj* %anf_45bind48260, %struct.ScmObj** %stackaddr$prim57122, align 8
%truthy$cmp57123 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48260)
%cmp$cmp57123 = icmp eq i64 %truthy$cmp57123, 1
br i1 %cmp$cmp57123, label %truebranch$cmp57123, label %falsebranch$cmp57123
truebranch$cmp57123:
%ae51381 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51382 = call %struct.ScmObj* @const_init_false()
%argslist56190$k483510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57124 = alloca %struct.ScmObj*, align 8
%argslist56190$k483511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51382, %struct.ScmObj* %argslist56190$k483510)
store volatile %struct.ScmObj* %argslist56190$k483511, %struct.ScmObj** %stackaddr$prim57124, align 8
%stackaddr$prim57125 = alloca %struct.ScmObj*, align 8
%argslist56190$k483512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51381, %struct.ScmObj* %argslist56190$k483511)
store volatile %struct.ScmObj* %argslist56190$k483512, %struct.ScmObj** %stackaddr$prim57125, align 8
%clofunc57126 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48351)
musttail call tailcc void %clofunc57126(%struct.ScmObj* %k48351, %struct.ScmObj* %argslist56190$k483512)
ret void
falsebranch$cmp57123:
%ae51390 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57127 = alloca %struct.ScmObj*, align 8
%anf_45bind48261 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48104, %struct.ScmObj* %ae51390)
store volatile %struct.ScmObj* %anf_45bind48261, %struct.ScmObj** %stackaddr$prim57127, align 8
%stackaddr$prim57128 = alloca %struct.ScmObj*, align 8
%anf_45bind48262 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48261)
store volatile %struct.ScmObj* %anf_45bind48262, %struct.ScmObj** %stackaddr$prim57128, align 8
%stackaddr$prim57129 = alloca %struct.ScmObj*, align 8
%anf_45bind48263 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind48262, %struct.ScmObj* %v48103)
store volatile %struct.ScmObj* %anf_45bind48263, %struct.ScmObj** %stackaddr$prim57129, align 8
%truthy$cmp57130 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48263)
%cmp$cmp57130 = icmp eq i64 %truthy$cmp57130, 1
br i1 %cmp$cmp57130, label %truebranch$cmp57130, label %falsebranch$cmp57130
truebranch$cmp57130:
%ae51396 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57131 = alloca %struct.ScmObj*, align 8
%cpsprim48354 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48104, %struct.ScmObj* %ae51396)
store volatile %struct.ScmObj* %cpsprim48354, %struct.ScmObj** %stackaddr$prim57131, align 8
%ae51398 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56191$k483510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57132 = alloca %struct.ScmObj*, align 8
%argslist56191$k483511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48354, %struct.ScmObj* %argslist56191$k483510)
store volatile %struct.ScmObj* %argslist56191$k483511, %struct.ScmObj** %stackaddr$prim57132, align 8
%stackaddr$prim57133 = alloca %struct.ScmObj*, align 8
%argslist56191$k483512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51398, %struct.ScmObj* %argslist56191$k483511)
store volatile %struct.ScmObj* %argslist56191$k483512, %struct.ScmObj** %stackaddr$prim57133, align 8
%clofunc57134 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48351)
musttail call tailcc void %clofunc57134(%struct.ScmObj* %k48351, %struct.ScmObj* %argslist56191$k483512)
ret void
falsebranch$cmp57130:
%ae51409 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57135 = alloca %struct.ScmObj*, align 8
%anf_45bind48264 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48104, %struct.ScmObj* %ae51409)
store volatile %struct.ScmObj* %anf_45bind48264, %struct.ScmObj** %stackaddr$prim57135, align 8
%stackaddr$prim57136 = alloca %struct.ScmObj*, align 8
%anf_45bind48265 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48264)
store volatile %struct.ScmObj* %anf_45bind48265, %struct.ScmObj** %stackaddr$prim57136, align 8
%ae51412 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57137 = alloca %struct.ScmObj*, align 8
%_95048107 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48104, %struct.ScmObj* %ae51412, %struct.ScmObj* %anf_45bind48265)
store volatile %struct.ScmObj* %_95048107, %struct.ScmObj** %stackaddr$prim57137, align 8
%argslist56192$cc481050 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57138 = alloca %struct.ScmObj*, align 8
%argslist56192$cc481051 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48105, %struct.ScmObj* %argslist56192$cc481050)
store volatile %struct.ScmObj* %argslist56192$cc481051, %struct.ScmObj** %stackaddr$prim57138, align 8
%stackaddr$prim57139 = alloca %struct.ScmObj*, align 8
%argslist56192$cc481052 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48351, %struct.ScmObj* %argslist56192$cc481051)
store volatile %struct.ScmObj* %argslist56192$cc481052, %struct.ScmObj** %stackaddr$prim57139, align 8
%clofunc57140 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48105)
musttail call tailcc void %clofunc57140(%struct.ScmObj* %cc48105, %struct.ScmObj* %argslist56192$cc481052)
ret void
}

define tailcc void @proc_clo$ae51269(%struct.ScmObj* %env$ae51269,%struct.ScmObj* %current_45args56193) {
%stackaddr$env-ref57141 = alloca %struct.ScmObj*, align 8
%lst48104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51269, i64 0)
store %struct.ScmObj* %lst48104, %struct.ScmObj** %stackaddr$env-ref57141
%stackaddr$env-ref57142 = alloca %struct.ScmObj*, align 8
%v48103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51269, i64 1)
store %struct.ScmObj* %v48103, %struct.ScmObj** %stackaddr$env-ref57142
%stackaddr$env-ref57143 = alloca %struct.ScmObj*, align 8
%k48351 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51269, i64 2)
store %struct.ScmObj* %k48351, %struct.ScmObj** %stackaddr$env-ref57143
%stackaddr$prim57144 = alloca %struct.ScmObj*, align 8
%_95k48353 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56193)
store volatile %struct.ScmObj* %_95k48353, %struct.ScmObj** %stackaddr$prim57144, align 8
%stackaddr$prim57145 = alloca %struct.ScmObj*, align 8
%current_45args56194 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56193)
store volatile %struct.ScmObj* %current_45args56194, %struct.ScmObj** %stackaddr$prim57145, align 8
%stackaddr$prim57146 = alloca %struct.ScmObj*, align 8
%cc48105 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56194)
store volatile %struct.ScmObj* %cc48105, %struct.ScmObj** %stackaddr$prim57146, align 8
%ae51271 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57147 = alloca %struct.ScmObj*, align 8
%anf_45bind48259 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48104, %struct.ScmObj* %ae51271)
store volatile %struct.ScmObj* %anf_45bind48259, %struct.ScmObj** %stackaddr$prim57147, align 8
%stackaddr$prim57148 = alloca %struct.ScmObj*, align 8
%anf_45bind48260 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48259)
store volatile %struct.ScmObj* %anf_45bind48260, %struct.ScmObj** %stackaddr$prim57148, align 8
%truthy$cmp57149 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48260)
%cmp$cmp57149 = icmp eq i64 %truthy$cmp57149, 1
br i1 %cmp$cmp57149, label %truebranch$cmp57149, label %falsebranch$cmp57149
truebranch$cmp57149:
%ae51275 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51276 = call %struct.ScmObj* @const_init_false()
%argslist56196$k483510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57150 = alloca %struct.ScmObj*, align 8
%argslist56196$k483511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51276, %struct.ScmObj* %argslist56196$k483510)
store volatile %struct.ScmObj* %argslist56196$k483511, %struct.ScmObj** %stackaddr$prim57150, align 8
%stackaddr$prim57151 = alloca %struct.ScmObj*, align 8
%argslist56196$k483512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51275, %struct.ScmObj* %argslist56196$k483511)
store volatile %struct.ScmObj* %argslist56196$k483512, %struct.ScmObj** %stackaddr$prim57151, align 8
%clofunc57152 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48351)
musttail call tailcc void %clofunc57152(%struct.ScmObj* %k48351, %struct.ScmObj* %argslist56196$k483512)
ret void
falsebranch$cmp57149:
%ae51284 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57153 = alloca %struct.ScmObj*, align 8
%anf_45bind48261 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48104, %struct.ScmObj* %ae51284)
store volatile %struct.ScmObj* %anf_45bind48261, %struct.ScmObj** %stackaddr$prim57153, align 8
%stackaddr$prim57154 = alloca %struct.ScmObj*, align 8
%anf_45bind48262 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48261)
store volatile %struct.ScmObj* %anf_45bind48262, %struct.ScmObj** %stackaddr$prim57154, align 8
%stackaddr$prim57155 = alloca %struct.ScmObj*, align 8
%anf_45bind48263 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind48262, %struct.ScmObj* %v48103)
store volatile %struct.ScmObj* %anf_45bind48263, %struct.ScmObj** %stackaddr$prim57155, align 8
%truthy$cmp57156 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48263)
%cmp$cmp57156 = icmp eq i64 %truthy$cmp57156, 1
br i1 %cmp$cmp57156, label %truebranch$cmp57156, label %falsebranch$cmp57156
truebranch$cmp57156:
%ae51290 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57157 = alloca %struct.ScmObj*, align 8
%cpsprim48354 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48104, %struct.ScmObj* %ae51290)
store volatile %struct.ScmObj* %cpsprim48354, %struct.ScmObj** %stackaddr$prim57157, align 8
%ae51292 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56197$k483510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57158 = alloca %struct.ScmObj*, align 8
%argslist56197$k483511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48354, %struct.ScmObj* %argslist56197$k483510)
store volatile %struct.ScmObj* %argslist56197$k483511, %struct.ScmObj** %stackaddr$prim57158, align 8
%stackaddr$prim57159 = alloca %struct.ScmObj*, align 8
%argslist56197$k483512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51292, %struct.ScmObj* %argslist56197$k483511)
store volatile %struct.ScmObj* %argslist56197$k483512, %struct.ScmObj** %stackaddr$prim57159, align 8
%clofunc57160 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48351)
musttail call tailcc void %clofunc57160(%struct.ScmObj* %k48351, %struct.ScmObj* %argslist56197$k483512)
ret void
falsebranch$cmp57156:
%ae51303 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57161 = alloca %struct.ScmObj*, align 8
%anf_45bind48264 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48104, %struct.ScmObj* %ae51303)
store volatile %struct.ScmObj* %anf_45bind48264, %struct.ScmObj** %stackaddr$prim57161, align 8
%stackaddr$prim57162 = alloca %struct.ScmObj*, align 8
%anf_45bind48265 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48264)
store volatile %struct.ScmObj* %anf_45bind48265, %struct.ScmObj** %stackaddr$prim57162, align 8
%ae51306 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57163 = alloca %struct.ScmObj*, align 8
%_95048107 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48104, %struct.ScmObj* %ae51306, %struct.ScmObj* %anf_45bind48265)
store volatile %struct.ScmObj* %_95048107, %struct.ScmObj** %stackaddr$prim57163, align 8
%argslist56198$cc481050 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57164 = alloca %struct.ScmObj*, align 8
%argslist56198$cc481051 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48105, %struct.ScmObj* %argslist56198$cc481050)
store volatile %struct.ScmObj* %argslist56198$cc481051, %struct.ScmObj** %stackaddr$prim57164, align 8
%stackaddr$prim57165 = alloca %struct.ScmObj*, align 8
%argslist56198$cc481052 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48351, %struct.ScmObj* %argslist56198$cc481051)
store volatile %struct.ScmObj* %argslist56198$cc481052, %struct.ScmObj** %stackaddr$prim57165, align 8
%clofunc57166 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48105)
musttail call tailcc void %clofunc57166(%struct.ScmObj* %cc48105, %struct.ScmObj* %argslist56198$cc481052)
ret void
}

define tailcc void @proc_clo$ae51254(%struct.ScmObj* %env$ae51254,%struct.ScmObj* %current_45args56200) {
%stackaddr$prim57167 = alloca %struct.ScmObj*, align 8
%k48355 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56200)
store volatile %struct.ScmObj* %k48355, %struct.ScmObj** %stackaddr$prim57167, align 8
%stackaddr$prim57168 = alloca %struct.ScmObj*, align 8
%current_45args56201 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56200)
store volatile %struct.ScmObj* %current_45args56201, %struct.ScmObj** %stackaddr$prim57168, align 8
%stackaddr$prim57169 = alloca %struct.ScmObj*, align 8
%u48106 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56201)
store volatile %struct.ScmObj* %u48106, %struct.ScmObj** %stackaddr$prim57169, align 8
%argslist56203$u481060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57170 = alloca %struct.ScmObj*, align 8
%argslist56203$u481061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u48106, %struct.ScmObj* %argslist56203$u481060)
store volatile %struct.ScmObj* %argslist56203$u481061, %struct.ScmObj** %stackaddr$prim57170, align 8
%stackaddr$prim57171 = alloca %struct.ScmObj*, align 8
%argslist56203$u481062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48355, %struct.ScmObj* %argslist56203$u481061)
store volatile %struct.ScmObj* %argslist56203$u481062, %struct.ScmObj** %stackaddr$prim57171, align 8
%clofunc57172 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u48106)
musttail call tailcc void %clofunc57172(%struct.ScmObj* %u48106, %struct.ScmObj* %argslist56203$u481062)
ret void
}

define tailcc void @proc_clo$ae50713(%struct.ScmObj* %env$ae50713,%struct.ScmObj* %current_45args56206) {
%stackaddr$prim57173 = alloca %struct.ScmObj*, align 8
%k48356 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56206)
store volatile %struct.ScmObj* %k48356, %struct.ScmObj** %stackaddr$prim57173, align 8
%stackaddr$prim57174 = alloca %struct.ScmObj*, align 8
%current_45args56207 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56206)
store volatile %struct.ScmObj* %current_45args56207, %struct.ScmObj** %stackaddr$prim57174, align 8
%stackaddr$prim57175 = alloca %struct.ScmObj*, align 8
%lst48110 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56207)
store volatile %struct.ScmObj* %lst48110, %struct.ScmObj** %stackaddr$prim57175, align 8
%stackaddr$prim57176 = alloca %struct.ScmObj*, align 8
%current_45args56208 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56207)
store volatile %struct.ScmObj* %current_45args56208, %struct.ScmObj** %stackaddr$prim57176, align 8
%stackaddr$prim57177 = alloca %struct.ScmObj*, align 8
%n48109 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56208)
store volatile %struct.ScmObj* %n48109, %struct.ScmObj** %stackaddr$prim57177, align 8
%ae50714 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57178 = alloca %struct.ScmObj*, align 8
%n48112 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50714, %struct.ScmObj* %n48109)
store volatile %struct.ScmObj* %n48112, %struct.ScmObj** %stackaddr$prim57178, align 8
%ae50716 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57179 = alloca %struct.ScmObj*, align 8
%lst48111 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50716, %struct.ScmObj* %lst48110)
store volatile %struct.ScmObj* %lst48111, %struct.ScmObj** %stackaddr$prim57179, align 8
%stackaddr$makeclosure57180 = alloca %struct.ScmObj*, align 8
%fptrToInt57181 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50718 to i64
%ae50718 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57181)
store volatile %struct.ScmObj* %ae50718, %struct.ScmObj** %stackaddr$makeclosure57180, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50718, %struct.ScmObj* %k48356, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50718, %struct.ScmObj* %n48112, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50718, %struct.ScmObj* %lst48111, i64 2)
%ae50719 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57182 = alloca %struct.ScmObj*, align 8
%fptrToInt57183 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50720 to i64
%ae50720 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57183)
store volatile %struct.ScmObj* %ae50720, %struct.ScmObj** %stackaddr$makeclosure57182, align 8
%argslist56228$ae507180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57184 = alloca %struct.ScmObj*, align 8
%argslist56228$ae507181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50720, %struct.ScmObj* %argslist56228$ae507180)
store volatile %struct.ScmObj* %argslist56228$ae507181, %struct.ScmObj** %stackaddr$prim57184, align 8
%stackaddr$prim57185 = alloca %struct.ScmObj*, align 8
%argslist56228$ae507182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50719, %struct.ScmObj* %argslist56228$ae507181)
store volatile %struct.ScmObj* %argslist56228$ae507182, %struct.ScmObj** %stackaddr$prim57185, align 8
%clofunc57186 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50718)
musttail call tailcc void %clofunc57186(%struct.ScmObj* %ae50718, %struct.ScmObj* %argslist56228$ae507182)
ret void
}

define tailcc void @proc_clo$ae50718(%struct.ScmObj* %env$ae50718,%struct.ScmObj* %current_45args56210) {
%stackaddr$env-ref57187 = alloca %struct.ScmObj*, align 8
%k48356 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50718, i64 0)
store %struct.ScmObj* %k48356, %struct.ScmObj** %stackaddr$env-ref57187
%stackaddr$env-ref57188 = alloca %struct.ScmObj*, align 8
%n48112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50718, i64 1)
store %struct.ScmObj* %n48112, %struct.ScmObj** %stackaddr$env-ref57188
%stackaddr$env-ref57189 = alloca %struct.ScmObj*, align 8
%lst48111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50718, i64 2)
store %struct.ScmObj* %lst48111, %struct.ScmObj** %stackaddr$env-ref57189
%stackaddr$prim57190 = alloca %struct.ScmObj*, align 8
%_95k48357 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56210)
store volatile %struct.ScmObj* %_95k48357, %struct.ScmObj** %stackaddr$prim57190, align 8
%stackaddr$prim57191 = alloca %struct.ScmObj*, align 8
%current_45args56211 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56210)
store volatile %struct.ScmObj* %current_45args56211, %struct.ScmObj** %stackaddr$prim57191, align 8
%stackaddr$prim57192 = alloca %struct.ScmObj*, align 8
%anf_45bind48251 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56211)
store volatile %struct.ScmObj* %anf_45bind48251, %struct.ScmObj** %stackaddr$prim57192, align 8
%stackaddr$makeclosure57193 = alloca %struct.ScmObj*, align 8
%fptrToInt57194 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50734 to i64
%ae50734 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57194)
store volatile %struct.ScmObj* %ae50734, %struct.ScmObj** %stackaddr$makeclosure57193, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50734, %struct.ScmObj* %k48356, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50734, %struct.ScmObj* %n48112, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50734, %struct.ScmObj* %lst48111, i64 2)
%stackaddr$makeclosure57195 = alloca %struct.ScmObj*, align 8
%fptrToInt57196 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50735 to i64
%ae50735 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57196)
store volatile %struct.ScmObj* %ae50735, %struct.ScmObj** %stackaddr$makeclosure57195, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50735, %struct.ScmObj* %k48356, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50735, %struct.ScmObj* %n48112, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50735, %struct.ScmObj* %lst48111, i64 2)
%argslist56223$anf_45bind482510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57197 = alloca %struct.ScmObj*, align 8
%argslist56223$anf_45bind482511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50735, %struct.ScmObj* %argslist56223$anf_45bind482510)
store volatile %struct.ScmObj* %argslist56223$anf_45bind482511, %struct.ScmObj** %stackaddr$prim57197, align 8
%stackaddr$prim57198 = alloca %struct.ScmObj*, align 8
%argslist56223$anf_45bind482512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50734, %struct.ScmObj* %argslist56223$anf_45bind482511)
store volatile %struct.ScmObj* %argslist56223$anf_45bind482512, %struct.ScmObj** %stackaddr$prim57198, align 8
%clofunc57199 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48251)
musttail call tailcc void %clofunc57199(%struct.ScmObj* %anf_45bind48251, %struct.ScmObj* %argslist56223$anf_45bind482512)
ret void
}

define tailcc void @proc_clo$ae50734(%struct.ScmObj* %env$ae50734,%struct.ScmObj* %current_45args56213) {
%stackaddr$env-ref57200 = alloca %struct.ScmObj*, align 8
%k48356 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50734, i64 0)
store %struct.ScmObj* %k48356, %struct.ScmObj** %stackaddr$env-ref57200
%stackaddr$env-ref57201 = alloca %struct.ScmObj*, align 8
%n48112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50734, i64 1)
store %struct.ScmObj* %n48112, %struct.ScmObj** %stackaddr$env-ref57201
%stackaddr$env-ref57202 = alloca %struct.ScmObj*, align 8
%lst48111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50734, i64 2)
store %struct.ScmObj* %lst48111, %struct.ScmObj** %stackaddr$env-ref57202
%stackaddr$prim57203 = alloca %struct.ScmObj*, align 8
%_95k48358 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56213)
store volatile %struct.ScmObj* %_95k48358, %struct.ScmObj** %stackaddr$prim57203, align 8
%stackaddr$prim57204 = alloca %struct.ScmObj*, align 8
%current_45args56214 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56213)
store volatile %struct.ScmObj* %current_45args56214, %struct.ScmObj** %stackaddr$prim57204, align 8
%stackaddr$prim57205 = alloca %struct.ScmObj*, align 8
%cc48113 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56214)
store volatile %struct.ScmObj* %cc48113, %struct.ScmObj** %stackaddr$prim57205, align 8
%ae50877 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57206 = alloca %struct.ScmObj*, align 8
%anf_45bind48252 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48112, %struct.ScmObj* %ae50877)
store volatile %struct.ScmObj* %anf_45bind48252, %struct.ScmObj** %stackaddr$prim57206, align 8
%ae50878 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57207 = alloca %struct.ScmObj*, align 8
%anf_45bind48253 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae50878, %struct.ScmObj* %anf_45bind48252)
store volatile %struct.ScmObj* %anf_45bind48253, %struct.ScmObj** %stackaddr$prim57207, align 8
%truthy$cmp57208 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48253)
%cmp$cmp57208 = icmp eq i64 %truthy$cmp57208, 1
br i1 %cmp$cmp57208, label %truebranch$cmp57208, label %falsebranch$cmp57208
truebranch$cmp57208:
%ae50882 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57209 = alloca %struct.ScmObj*, align 8
%cpsprim48359 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48111, %struct.ScmObj* %ae50882)
store volatile %struct.ScmObj* %cpsprim48359, %struct.ScmObj** %stackaddr$prim57209, align 8
%ae50884 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56216$k483560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57210 = alloca %struct.ScmObj*, align 8
%argslist56216$k483561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48359, %struct.ScmObj* %argslist56216$k483560)
store volatile %struct.ScmObj* %argslist56216$k483561, %struct.ScmObj** %stackaddr$prim57210, align 8
%stackaddr$prim57211 = alloca %struct.ScmObj*, align 8
%argslist56216$k483562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50884, %struct.ScmObj* %argslist56216$k483561)
store volatile %struct.ScmObj* %argslist56216$k483562, %struct.ScmObj** %stackaddr$prim57211, align 8
%clofunc57212 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48356)
musttail call tailcc void %clofunc57212(%struct.ScmObj* %k48356, %struct.ScmObj* %argslist56216$k483562)
ret void
falsebranch$cmp57208:
%ae50895 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57213 = alloca %struct.ScmObj*, align 8
%anf_45bind48254 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48111, %struct.ScmObj* %ae50895)
store volatile %struct.ScmObj* %anf_45bind48254, %struct.ScmObj** %stackaddr$prim57213, align 8
%stackaddr$prim57214 = alloca %struct.ScmObj*, align 8
%anf_45bind48255 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48254)
store volatile %struct.ScmObj* %anf_45bind48255, %struct.ScmObj** %stackaddr$prim57214, align 8
%ae50898 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57215 = alloca %struct.ScmObj*, align 8
%_95048116 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48111, %struct.ScmObj* %ae50898, %struct.ScmObj* %anf_45bind48255)
store volatile %struct.ScmObj* %_95048116, %struct.ScmObj** %stackaddr$prim57215, align 8
%ae50901 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57216 = alloca %struct.ScmObj*, align 8
%anf_45bind48256 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48112, %struct.ScmObj* %ae50901)
store volatile %struct.ScmObj* %anf_45bind48256, %struct.ScmObj** %stackaddr$prim57216, align 8
%ae50903 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57217 = alloca %struct.ScmObj*, align 8
%anf_45bind48257 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind48256, %struct.ScmObj* %ae50903)
store volatile %struct.ScmObj* %anf_45bind48257, %struct.ScmObj** %stackaddr$prim57217, align 8
%ae50905 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57218 = alloca %struct.ScmObj*, align 8
%_95148115 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n48112, %struct.ScmObj* %ae50905, %struct.ScmObj* %anf_45bind48257)
store volatile %struct.ScmObj* %_95148115, %struct.ScmObj** %stackaddr$prim57218, align 8
%argslist56217$cc481130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57219 = alloca %struct.ScmObj*, align 8
%argslist56217$cc481131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48113, %struct.ScmObj* %argslist56217$cc481130)
store volatile %struct.ScmObj* %argslist56217$cc481131, %struct.ScmObj** %stackaddr$prim57219, align 8
%stackaddr$prim57220 = alloca %struct.ScmObj*, align 8
%argslist56217$cc481132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48356, %struct.ScmObj* %argslist56217$cc481131)
store volatile %struct.ScmObj* %argslist56217$cc481132, %struct.ScmObj** %stackaddr$prim57220, align 8
%clofunc57221 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48113)
musttail call tailcc void %clofunc57221(%struct.ScmObj* %cc48113, %struct.ScmObj* %argslist56217$cc481132)
ret void
}

define tailcc void @proc_clo$ae50735(%struct.ScmObj* %env$ae50735,%struct.ScmObj* %current_45args56218) {
%stackaddr$env-ref57222 = alloca %struct.ScmObj*, align 8
%k48356 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50735, i64 0)
store %struct.ScmObj* %k48356, %struct.ScmObj** %stackaddr$env-ref57222
%stackaddr$env-ref57223 = alloca %struct.ScmObj*, align 8
%n48112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50735, i64 1)
store %struct.ScmObj* %n48112, %struct.ScmObj** %stackaddr$env-ref57223
%stackaddr$env-ref57224 = alloca %struct.ScmObj*, align 8
%lst48111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50735, i64 2)
store %struct.ScmObj* %lst48111, %struct.ScmObj** %stackaddr$env-ref57224
%stackaddr$prim57225 = alloca %struct.ScmObj*, align 8
%_95k48358 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56218)
store volatile %struct.ScmObj* %_95k48358, %struct.ScmObj** %stackaddr$prim57225, align 8
%stackaddr$prim57226 = alloca %struct.ScmObj*, align 8
%current_45args56219 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56218)
store volatile %struct.ScmObj* %current_45args56219, %struct.ScmObj** %stackaddr$prim57226, align 8
%stackaddr$prim57227 = alloca %struct.ScmObj*, align 8
%cc48113 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56219)
store volatile %struct.ScmObj* %cc48113, %struct.ScmObj** %stackaddr$prim57227, align 8
%ae50737 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57228 = alloca %struct.ScmObj*, align 8
%anf_45bind48252 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48112, %struct.ScmObj* %ae50737)
store volatile %struct.ScmObj* %anf_45bind48252, %struct.ScmObj** %stackaddr$prim57228, align 8
%ae50738 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57229 = alloca %struct.ScmObj*, align 8
%anf_45bind48253 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae50738, %struct.ScmObj* %anf_45bind48252)
store volatile %struct.ScmObj* %anf_45bind48253, %struct.ScmObj** %stackaddr$prim57229, align 8
%truthy$cmp57230 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48253)
%cmp$cmp57230 = icmp eq i64 %truthy$cmp57230, 1
br i1 %cmp$cmp57230, label %truebranch$cmp57230, label %falsebranch$cmp57230
truebranch$cmp57230:
%ae50742 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57231 = alloca %struct.ScmObj*, align 8
%cpsprim48359 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48111, %struct.ScmObj* %ae50742)
store volatile %struct.ScmObj* %cpsprim48359, %struct.ScmObj** %stackaddr$prim57231, align 8
%ae50744 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56221$k483560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57232 = alloca %struct.ScmObj*, align 8
%argslist56221$k483561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48359, %struct.ScmObj* %argslist56221$k483560)
store volatile %struct.ScmObj* %argslist56221$k483561, %struct.ScmObj** %stackaddr$prim57232, align 8
%stackaddr$prim57233 = alloca %struct.ScmObj*, align 8
%argslist56221$k483562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50744, %struct.ScmObj* %argslist56221$k483561)
store volatile %struct.ScmObj* %argslist56221$k483562, %struct.ScmObj** %stackaddr$prim57233, align 8
%clofunc57234 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48356)
musttail call tailcc void %clofunc57234(%struct.ScmObj* %k48356, %struct.ScmObj* %argslist56221$k483562)
ret void
falsebranch$cmp57230:
%ae50755 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57235 = alloca %struct.ScmObj*, align 8
%anf_45bind48254 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48111, %struct.ScmObj* %ae50755)
store volatile %struct.ScmObj* %anf_45bind48254, %struct.ScmObj** %stackaddr$prim57235, align 8
%stackaddr$prim57236 = alloca %struct.ScmObj*, align 8
%anf_45bind48255 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48254)
store volatile %struct.ScmObj* %anf_45bind48255, %struct.ScmObj** %stackaddr$prim57236, align 8
%ae50758 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57237 = alloca %struct.ScmObj*, align 8
%_95048116 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48111, %struct.ScmObj* %ae50758, %struct.ScmObj* %anf_45bind48255)
store volatile %struct.ScmObj* %_95048116, %struct.ScmObj** %stackaddr$prim57237, align 8
%ae50761 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57238 = alloca %struct.ScmObj*, align 8
%anf_45bind48256 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48112, %struct.ScmObj* %ae50761)
store volatile %struct.ScmObj* %anf_45bind48256, %struct.ScmObj** %stackaddr$prim57238, align 8
%ae50763 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57239 = alloca %struct.ScmObj*, align 8
%anf_45bind48257 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind48256, %struct.ScmObj* %ae50763)
store volatile %struct.ScmObj* %anf_45bind48257, %struct.ScmObj** %stackaddr$prim57239, align 8
%ae50765 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57240 = alloca %struct.ScmObj*, align 8
%_95148115 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n48112, %struct.ScmObj* %ae50765, %struct.ScmObj* %anf_45bind48257)
store volatile %struct.ScmObj* %_95148115, %struct.ScmObj** %stackaddr$prim57240, align 8
%argslist56222$cc481130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57241 = alloca %struct.ScmObj*, align 8
%argslist56222$cc481131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48113, %struct.ScmObj* %argslist56222$cc481130)
store volatile %struct.ScmObj* %argslist56222$cc481131, %struct.ScmObj** %stackaddr$prim57241, align 8
%stackaddr$prim57242 = alloca %struct.ScmObj*, align 8
%argslist56222$cc481132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48356, %struct.ScmObj* %argslist56222$cc481131)
store volatile %struct.ScmObj* %argslist56222$cc481132, %struct.ScmObj** %stackaddr$prim57242, align 8
%clofunc57243 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48113)
musttail call tailcc void %clofunc57243(%struct.ScmObj* %cc48113, %struct.ScmObj* %argslist56222$cc481132)
ret void
}

define tailcc void @proc_clo$ae50720(%struct.ScmObj* %env$ae50720,%struct.ScmObj* %current_45args56224) {
%stackaddr$prim57244 = alloca %struct.ScmObj*, align 8
%k48360 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56224)
store volatile %struct.ScmObj* %k48360, %struct.ScmObj** %stackaddr$prim57244, align 8
%stackaddr$prim57245 = alloca %struct.ScmObj*, align 8
%current_45args56225 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56224)
store volatile %struct.ScmObj* %current_45args56225, %struct.ScmObj** %stackaddr$prim57245, align 8
%stackaddr$prim57246 = alloca %struct.ScmObj*, align 8
%u48114 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56225)
store volatile %struct.ScmObj* %u48114, %struct.ScmObj** %stackaddr$prim57246, align 8
%argslist56227$u481140 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57247 = alloca %struct.ScmObj*, align 8
%argslist56227$u481141 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u48114, %struct.ScmObj* %argslist56227$u481140)
store volatile %struct.ScmObj* %argslist56227$u481141, %struct.ScmObj** %stackaddr$prim57247, align 8
%stackaddr$prim57248 = alloca %struct.ScmObj*, align 8
%argslist56227$u481142 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48360, %struct.ScmObj* %argslist56227$u481141)
store volatile %struct.ScmObj* %argslist56227$u481142, %struct.ScmObj** %stackaddr$prim57248, align 8
%clofunc57249 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u48114)
musttail call tailcc void %clofunc57249(%struct.ScmObj* %u48114, %struct.ScmObj* %argslist56227$u481142)
ret void
}

define tailcc void @proc_clo$ae50297(%struct.ScmObj* %env$ae50297,%struct.ScmObj* %current_45args56230) {
%stackaddr$prim57250 = alloca %struct.ScmObj*, align 8
%k48361 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56230)
store volatile %struct.ScmObj* %k48361, %struct.ScmObj** %stackaddr$prim57250, align 8
%stackaddr$prim57251 = alloca %struct.ScmObj*, align 8
%current_45args56231 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56230)
store volatile %struct.ScmObj* %current_45args56231, %struct.ScmObj** %stackaddr$prim57251, align 8
%stackaddr$prim57252 = alloca %struct.ScmObj*, align 8
%a48118 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56231)
store volatile %struct.ScmObj* %a48118, %struct.ScmObj** %stackaddr$prim57252, align 8
%ae50298 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57253 = alloca %struct.ScmObj*, align 8
%a48119 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50298, %struct.ScmObj* %a48118)
store volatile %struct.ScmObj* %a48119, %struct.ScmObj** %stackaddr$prim57253, align 8
%stackaddr$makeclosure57254 = alloca %struct.ScmObj*, align 8
%fptrToInt57255 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50300 to i64
%ae50300 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57255)
store volatile %struct.ScmObj* %ae50300, %struct.ScmObj** %stackaddr$makeclosure57254, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50300, %struct.ScmObj* %k48361, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50300, %struct.ScmObj* %a48119, i64 1)
%ae50301 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57256 = alloca %struct.ScmObj*, align 8
%fptrToInt57257 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50302 to i64
%ae50302 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57257)
store volatile %struct.ScmObj* %ae50302, %struct.ScmObj** %stackaddr$makeclosure57256, align 8
%argslist56253$ae503000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57258 = alloca %struct.ScmObj*, align 8
%argslist56253$ae503001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50302, %struct.ScmObj* %argslist56253$ae503000)
store volatile %struct.ScmObj* %argslist56253$ae503001, %struct.ScmObj** %stackaddr$prim57258, align 8
%stackaddr$prim57259 = alloca %struct.ScmObj*, align 8
%argslist56253$ae503002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50301, %struct.ScmObj* %argslist56253$ae503001)
store volatile %struct.ScmObj* %argslist56253$ae503002, %struct.ScmObj** %stackaddr$prim57259, align 8
%clofunc57260 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50300)
musttail call tailcc void %clofunc57260(%struct.ScmObj* %ae50300, %struct.ScmObj* %argslist56253$ae503002)
ret void
}

define tailcc void @proc_clo$ae50300(%struct.ScmObj* %env$ae50300,%struct.ScmObj* %current_45args56233) {
%stackaddr$env-ref57261 = alloca %struct.ScmObj*, align 8
%k48361 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50300, i64 0)
store %struct.ScmObj* %k48361, %struct.ScmObj** %stackaddr$env-ref57261
%stackaddr$env-ref57262 = alloca %struct.ScmObj*, align 8
%a48119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50300, i64 1)
store %struct.ScmObj* %a48119, %struct.ScmObj** %stackaddr$env-ref57262
%stackaddr$prim57263 = alloca %struct.ScmObj*, align 8
%_95k48362 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56233)
store volatile %struct.ScmObj* %_95k48362, %struct.ScmObj** %stackaddr$prim57263, align 8
%stackaddr$prim57264 = alloca %struct.ScmObj*, align 8
%current_45args56234 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56233)
store volatile %struct.ScmObj* %current_45args56234, %struct.ScmObj** %stackaddr$prim57264, align 8
%stackaddr$prim57265 = alloca %struct.ScmObj*, align 8
%anf_45bind48243 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56234)
store volatile %struct.ScmObj* %anf_45bind48243, %struct.ScmObj** %stackaddr$prim57265, align 8
%stackaddr$makeclosure57266 = alloca %struct.ScmObj*, align 8
%fptrToInt57267 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50319 to i64
%ae50319 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57267)
store volatile %struct.ScmObj* %ae50319, %struct.ScmObj** %stackaddr$makeclosure57266, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50319, %struct.ScmObj* %k48361, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50319, %struct.ScmObj* %a48119, i64 1)
%stackaddr$makeclosure57268 = alloca %struct.ScmObj*, align 8
%fptrToInt57269 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50320 to i64
%ae50320 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57269)
store volatile %struct.ScmObj* %ae50320, %struct.ScmObj** %stackaddr$makeclosure57268, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50320, %struct.ScmObj* %k48361, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50320, %struct.ScmObj* %a48119, i64 1)
%argslist56248$anf_45bind482430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57270 = alloca %struct.ScmObj*, align 8
%argslist56248$anf_45bind482431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50320, %struct.ScmObj* %argslist56248$anf_45bind482430)
store volatile %struct.ScmObj* %argslist56248$anf_45bind482431, %struct.ScmObj** %stackaddr$prim57270, align 8
%stackaddr$prim57271 = alloca %struct.ScmObj*, align 8
%argslist56248$anf_45bind482432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50319, %struct.ScmObj* %argslist56248$anf_45bind482431)
store volatile %struct.ScmObj* %argslist56248$anf_45bind482432, %struct.ScmObj** %stackaddr$prim57271, align 8
%clofunc57272 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48243)
musttail call tailcc void %clofunc57272(%struct.ScmObj* %anf_45bind48243, %struct.ScmObj* %argslist56248$anf_45bind482432)
ret void
}

define tailcc void @proc_clo$ae50319(%struct.ScmObj* %env$ae50319,%struct.ScmObj* %current_45args56236) {
%stackaddr$env-ref57273 = alloca %struct.ScmObj*, align 8
%k48361 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50319, i64 0)
store %struct.ScmObj* %k48361, %struct.ScmObj** %stackaddr$env-ref57273
%stackaddr$env-ref57274 = alloca %struct.ScmObj*, align 8
%a48119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50319, i64 1)
store %struct.ScmObj* %a48119, %struct.ScmObj** %stackaddr$env-ref57274
%stackaddr$prim57275 = alloca %struct.ScmObj*, align 8
%_95k48363 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56236)
store volatile %struct.ScmObj* %_95k48363, %struct.ScmObj** %stackaddr$prim57275, align 8
%stackaddr$prim57276 = alloca %struct.ScmObj*, align 8
%current_45args56237 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56236)
store volatile %struct.ScmObj* %current_45args56237, %struct.ScmObj** %stackaddr$prim57276, align 8
%stackaddr$prim57277 = alloca %struct.ScmObj*, align 8
%cc48120 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56237)
store volatile %struct.ScmObj* %cc48120, %struct.ScmObj** %stackaddr$prim57277, align 8
%ae50435 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57278 = alloca %struct.ScmObj*, align 8
%anf_45bind48244 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48119, %struct.ScmObj* %ae50435)
store volatile %struct.ScmObj* %anf_45bind48244, %struct.ScmObj** %stackaddr$prim57278, align 8
%stackaddr$prim57279 = alloca %struct.ScmObj*, align 8
%anf_45bind48245 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48244)
store volatile %struct.ScmObj* %anf_45bind48245, %struct.ScmObj** %stackaddr$prim57279, align 8
%truthy$cmp57280 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48245)
%cmp$cmp57280 = icmp eq i64 %truthy$cmp57280, 1
br i1 %cmp$cmp57280, label %truebranch$cmp57280, label %falsebranch$cmp57280
truebranch$cmp57280:
%ae50439 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50440 = call %struct.ScmObj* @const_init_true()
%argslist56239$k483610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57281 = alloca %struct.ScmObj*, align 8
%argslist56239$k483611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50440, %struct.ScmObj* %argslist56239$k483610)
store volatile %struct.ScmObj* %argslist56239$k483611, %struct.ScmObj** %stackaddr$prim57281, align 8
%stackaddr$prim57282 = alloca %struct.ScmObj*, align 8
%argslist56239$k483612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50439, %struct.ScmObj* %argslist56239$k483611)
store volatile %struct.ScmObj* %argslist56239$k483612, %struct.ScmObj** %stackaddr$prim57282, align 8
%clofunc57283 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48361)
musttail call tailcc void %clofunc57283(%struct.ScmObj* %k48361, %struct.ScmObj* %argslist56239$k483612)
ret void
falsebranch$cmp57280:
%ae50448 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57284 = alloca %struct.ScmObj*, align 8
%anf_45bind48246 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48119, %struct.ScmObj* %ae50448)
store volatile %struct.ScmObj* %anf_45bind48246, %struct.ScmObj** %stackaddr$prim57284, align 8
%stackaddr$prim57285 = alloca %struct.ScmObj*, align 8
%anf_45bind48247 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind48246)
store volatile %struct.ScmObj* %anf_45bind48247, %struct.ScmObj** %stackaddr$prim57285, align 8
%truthy$cmp57286 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48247)
%cmp$cmp57286 = icmp eq i64 %truthy$cmp57286, 1
br i1 %cmp$cmp57286, label %truebranch$cmp57286, label %falsebranch$cmp57286
truebranch$cmp57286:
%ae50452 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57287 = alloca %struct.ScmObj*, align 8
%anf_45bind48248 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48119, %struct.ScmObj* %ae50452)
store volatile %struct.ScmObj* %anf_45bind48248, %struct.ScmObj** %stackaddr$prim57287, align 8
%stackaddr$prim57288 = alloca %struct.ScmObj*, align 8
%b48122 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48248)
store volatile %struct.ScmObj* %b48122, %struct.ScmObj** %stackaddr$prim57288, align 8
%ae50455 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57289 = alloca %struct.ScmObj*, align 8
%anf_45bind48249 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48119, %struct.ScmObj* %ae50455)
store volatile %struct.ScmObj* %anf_45bind48249, %struct.ScmObj** %stackaddr$prim57289, align 8
%stackaddr$prim57290 = alloca %struct.ScmObj*, align 8
%anf_45bind48250 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48249)
store volatile %struct.ScmObj* %anf_45bind48250, %struct.ScmObj** %stackaddr$prim57290, align 8
%ae50458 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57291 = alloca %struct.ScmObj*, align 8
%_95048123 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a48119, %struct.ScmObj* %ae50458, %struct.ScmObj* %anf_45bind48250)
store volatile %struct.ScmObj* %_95048123, %struct.ScmObj** %stackaddr$prim57291, align 8
%argslist56240$cc481200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57292 = alloca %struct.ScmObj*, align 8
%argslist56240$cc481201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48120, %struct.ScmObj* %argslist56240$cc481200)
store volatile %struct.ScmObj* %argslist56240$cc481201, %struct.ScmObj** %stackaddr$prim57292, align 8
%stackaddr$prim57293 = alloca %struct.ScmObj*, align 8
%argslist56240$cc481202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48361, %struct.ScmObj* %argslist56240$cc481201)
store volatile %struct.ScmObj* %argslist56240$cc481202, %struct.ScmObj** %stackaddr$prim57293, align 8
%clofunc57294 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48120)
musttail call tailcc void %clofunc57294(%struct.ScmObj* %cc48120, %struct.ScmObj* %argslist56240$cc481202)
ret void
falsebranch$cmp57286:
%ae50491 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50492 = call %struct.ScmObj* @const_init_false()
%argslist56241$k483610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57295 = alloca %struct.ScmObj*, align 8
%argslist56241$k483611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50492, %struct.ScmObj* %argslist56241$k483610)
store volatile %struct.ScmObj* %argslist56241$k483611, %struct.ScmObj** %stackaddr$prim57295, align 8
%stackaddr$prim57296 = alloca %struct.ScmObj*, align 8
%argslist56241$k483612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50491, %struct.ScmObj* %argslist56241$k483611)
store volatile %struct.ScmObj* %argslist56241$k483612, %struct.ScmObj** %stackaddr$prim57296, align 8
%clofunc57297 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48361)
musttail call tailcc void %clofunc57297(%struct.ScmObj* %k48361, %struct.ScmObj* %argslist56241$k483612)
ret void
}

define tailcc void @proc_clo$ae50320(%struct.ScmObj* %env$ae50320,%struct.ScmObj* %current_45args56242) {
%stackaddr$env-ref57298 = alloca %struct.ScmObj*, align 8
%k48361 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50320, i64 0)
store %struct.ScmObj* %k48361, %struct.ScmObj** %stackaddr$env-ref57298
%stackaddr$env-ref57299 = alloca %struct.ScmObj*, align 8
%a48119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50320, i64 1)
store %struct.ScmObj* %a48119, %struct.ScmObj** %stackaddr$env-ref57299
%stackaddr$prim57300 = alloca %struct.ScmObj*, align 8
%_95k48363 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56242)
store volatile %struct.ScmObj* %_95k48363, %struct.ScmObj** %stackaddr$prim57300, align 8
%stackaddr$prim57301 = alloca %struct.ScmObj*, align 8
%current_45args56243 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56242)
store volatile %struct.ScmObj* %current_45args56243, %struct.ScmObj** %stackaddr$prim57301, align 8
%stackaddr$prim57302 = alloca %struct.ScmObj*, align 8
%cc48120 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56243)
store volatile %struct.ScmObj* %cc48120, %struct.ScmObj** %stackaddr$prim57302, align 8
%ae50322 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57303 = alloca %struct.ScmObj*, align 8
%anf_45bind48244 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48119, %struct.ScmObj* %ae50322)
store volatile %struct.ScmObj* %anf_45bind48244, %struct.ScmObj** %stackaddr$prim57303, align 8
%stackaddr$prim57304 = alloca %struct.ScmObj*, align 8
%anf_45bind48245 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48244)
store volatile %struct.ScmObj* %anf_45bind48245, %struct.ScmObj** %stackaddr$prim57304, align 8
%truthy$cmp57305 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48245)
%cmp$cmp57305 = icmp eq i64 %truthy$cmp57305, 1
br i1 %cmp$cmp57305, label %truebranch$cmp57305, label %falsebranch$cmp57305
truebranch$cmp57305:
%ae50326 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50327 = call %struct.ScmObj* @const_init_true()
%argslist56245$k483610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57306 = alloca %struct.ScmObj*, align 8
%argslist56245$k483611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50327, %struct.ScmObj* %argslist56245$k483610)
store volatile %struct.ScmObj* %argslist56245$k483611, %struct.ScmObj** %stackaddr$prim57306, align 8
%stackaddr$prim57307 = alloca %struct.ScmObj*, align 8
%argslist56245$k483612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50326, %struct.ScmObj* %argslist56245$k483611)
store volatile %struct.ScmObj* %argslist56245$k483612, %struct.ScmObj** %stackaddr$prim57307, align 8
%clofunc57308 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48361)
musttail call tailcc void %clofunc57308(%struct.ScmObj* %k48361, %struct.ScmObj* %argslist56245$k483612)
ret void
falsebranch$cmp57305:
%ae50335 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57309 = alloca %struct.ScmObj*, align 8
%anf_45bind48246 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48119, %struct.ScmObj* %ae50335)
store volatile %struct.ScmObj* %anf_45bind48246, %struct.ScmObj** %stackaddr$prim57309, align 8
%stackaddr$prim57310 = alloca %struct.ScmObj*, align 8
%anf_45bind48247 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind48246)
store volatile %struct.ScmObj* %anf_45bind48247, %struct.ScmObj** %stackaddr$prim57310, align 8
%truthy$cmp57311 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48247)
%cmp$cmp57311 = icmp eq i64 %truthy$cmp57311, 1
br i1 %cmp$cmp57311, label %truebranch$cmp57311, label %falsebranch$cmp57311
truebranch$cmp57311:
%ae50339 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57312 = alloca %struct.ScmObj*, align 8
%anf_45bind48248 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48119, %struct.ScmObj* %ae50339)
store volatile %struct.ScmObj* %anf_45bind48248, %struct.ScmObj** %stackaddr$prim57312, align 8
%stackaddr$prim57313 = alloca %struct.ScmObj*, align 8
%b48122 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48248)
store volatile %struct.ScmObj* %b48122, %struct.ScmObj** %stackaddr$prim57313, align 8
%ae50342 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57314 = alloca %struct.ScmObj*, align 8
%anf_45bind48249 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48119, %struct.ScmObj* %ae50342)
store volatile %struct.ScmObj* %anf_45bind48249, %struct.ScmObj** %stackaddr$prim57314, align 8
%stackaddr$prim57315 = alloca %struct.ScmObj*, align 8
%anf_45bind48250 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48249)
store volatile %struct.ScmObj* %anf_45bind48250, %struct.ScmObj** %stackaddr$prim57315, align 8
%ae50345 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57316 = alloca %struct.ScmObj*, align 8
%_95048123 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a48119, %struct.ScmObj* %ae50345, %struct.ScmObj* %anf_45bind48250)
store volatile %struct.ScmObj* %_95048123, %struct.ScmObj** %stackaddr$prim57316, align 8
%argslist56246$cc481200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57317 = alloca %struct.ScmObj*, align 8
%argslist56246$cc481201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48120, %struct.ScmObj* %argslist56246$cc481200)
store volatile %struct.ScmObj* %argslist56246$cc481201, %struct.ScmObj** %stackaddr$prim57317, align 8
%stackaddr$prim57318 = alloca %struct.ScmObj*, align 8
%argslist56246$cc481202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48361, %struct.ScmObj* %argslist56246$cc481201)
store volatile %struct.ScmObj* %argslist56246$cc481202, %struct.ScmObj** %stackaddr$prim57318, align 8
%clofunc57319 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48120)
musttail call tailcc void %clofunc57319(%struct.ScmObj* %cc48120, %struct.ScmObj* %argslist56246$cc481202)
ret void
falsebranch$cmp57311:
%ae50378 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50379 = call %struct.ScmObj* @const_init_false()
%argslist56247$k483610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57320 = alloca %struct.ScmObj*, align 8
%argslist56247$k483611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50379, %struct.ScmObj* %argslist56247$k483610)
store volatile %struct.ScmObj* %argslist56247$k483611, %struct.ScmObj** %stackaddr$prim57320, align 8
%stackaddr$prim57321 = alloca %struct.ScmObj*, align 8
%argslist56247$k483612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50378, %struct.ScmObj* %argslist56247$k483611)
store volatile %struct.ScmObj* %argslist56247$k483612, %struct.ScmObj** %stackaddr$prim57321, align 8
%clofunc57322 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48361)
musttail call tailcc void %clofunc57322(%struct.ScmObj* %k48361, %struct.ScmObj* %argslist56247$k483612)
ret void
}

define tailcc void @proc_clo$ae50302(%struct.ScmObj* %env$ae50302,%struct.ScmObj* %current_45args56249) {
%stackaddr$prim57323 = alloca %struct.ScmObj*, align 8
%k48364 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56249)
store volatile %struct.ScmObj* %k48364, %struct.ScmObj** %stackaddr$prim57323, align 8
%stackaddr$prim57324 = alloca %struct.ScmObj*, align 8
%current_45args56250 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56249)
store volatile %struct.ScmObj* %current_45args56250, %struct.ScmObj** %stackaddr$prim57324, align 8
%stackaddr$prim57325 = alloca %struct.ScmObj*, align 8
%k48121 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56250)
store volatile %struct.ScmObj* %k48121, %struct.ScmObj** %stackaddr$prim57325, align 8
%ae50304 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56252$k483640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57326 = alloca %struct.ScmObj*, align 8
%argslist56252$k483641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48121, %struct.ScmObj* %argslist56252$k483640)
store volatile %struct.ScmObj* %argslist56252$k483641, %struct.ScmObj** %stackaddr$prim57326, align 8
%stackaddr$prim57327 = alloca %struct.ScmObj*, align 8
%argslist56252$k483642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50304, %struct.ScmObj* %argslist56252$k483641)
store volatile %struct.ScmObj* %argslist56252$k483642, %struct.ScmObj** %stackaddr$prim57327, align 8
%clofunc57328 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48364)
musttail call tailcc void %clofunc57328(%struct.ScmObj* %k48364, %struct.ScmObj* %argslist56252$k483642)
ret void
}

define tailcc void @proc_clo$ae50225(%struct.ScmObj* %env$ae50225,%struct.ScmObj* %current_45args56255) {
%stackaddr$env-ref57329 = alloca %struct.ScmObj*, align 8
%_37append48125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50225, i64 0)
store %struct.ScmObj* %_37append48125, %struct.ScmObj** %stackaddr$env-ref57329
%stackaddr$prim57330 = alloca %struct.ScmObj*, align 8
%k48365 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56255)
store volatile %struct.ScmObj* %k48365, %struct.ScmObj** %stackaddr$prim57330, align 8
%stackaddr$prim57331 = alloca %struct.ScmObj*, align 8
%current_45args56256 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56255)
store volatile %struct.ScmObj* %current_45args56256, %struct.ScmObj** %stackaddr$prim57331, align 8
%stackaddr$prim57332 = alloca %struct.ScmObj*, align 8
%ls048128 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56256)
store volatile %struct.ScmObj* %ls048128, %struct.ScmObj** %stackaddr$prim57332, align 8
%stackaddr$prim57333 = alloca %struct.ScmObj*, align 8
%current_45args56257 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56256)
store volatile %struct.ScmObj* %current_45args56257, %struct.ScmObj** %stackaddr$prim57333, align 8
%stackaddr$prim57334 = alloca %struct.ScmObj*, align 8
%ls148127 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56257)
store volatile %struct.ScmObj* %ls148127, %struct.ScmObj** %stackaddr$prim57334, align 8
%stackaddr$prim57335 = alloca %struct.ScmObj*, align 8
%anf_45bind48237 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls048128)
store volatile %struct.ScmObj* %anf_45bind48237, %struct.ScmObj** %stackaddr$prim57335, align 8
%truthy$cmp57336 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48237)
%cmp$cmp57336 = icmp eq i64 %truthy$cmp57336, 1
br i1 %cmp$cmp57336, label %truebranch$cmp57336, label %falsebranch$cmp57336
truebranch$cmp57336:
%ae50229 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56259$k483650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57337 = alloca %struct.ScmObj*, align 8
%argslist56259$k483651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls148127, %struct.ScmObj* %argslist56259$k483650)
store volatile %struct.ScmObj* %argslist56259$k483651, %struct.ScmObj** %stackaddr$prim57337, align 8
%stackaddr$prim57338 = alloca %struct.ScmObj*, align 8
%argslist56259$k483652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50229, %struct.ScmObj* %argslist56259$k483651)
store volatile %struct.ScmObj* %argslist56259$k483652, %struct.ScmObj** %stackaddr$prim57338, align 8
%clofunc57339 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48365)
musttail call tailcc void %clofunc57339(%struct.ScmObj* %k48365, %struct.ScmObj* %argslist56259$k483652)
ret void
falsebranch$cmp57336:
%stackaddr$prim57340 = alloca %struct.ScmObj*, align 8
%anf_45bind48238 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls048128)
store volatile %struct.ScmObj* %anf_45bind48238, %struct.ScmObj** %stackaddr$prim57340, align 8
%ae50236 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57341 = alloca %struct.ScmObj*, align 8
%anf_45bind48239 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append48125, %struct.ScmObj* %ae50236)
store volatile %struct.ScmObj* %anf_45bind48239, %struct.ScmObj** %stackaddr$prim57341, align 8
%stackaddr$prim57342 = alloca %struct.ScmObj*, align 8
%anf_45bind48240 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls048128)
store volatile %struct.ScmObj* %anf_45bind48240, %struct.ScmObj** %stackaddr$prim57342, align 8
%stackaddr$makeclosure57343 = alloca %struct.ScmObj*, align 8
%fptrToInt57344 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50239 to i64
%ae50239 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57344)
store volatile %struct.ScmObj* %ae50239, %struct.ScmObj** %stackaddr$makeclosure57343, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50239, %struct.ScmObj* %anf_45bind48238, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50239, %struct.ScmObj* %k48365, i64 1)
%argslist56264$anf_45bind482390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57345 = alloca %struct.ScmObj*, align 8
%argslist56264$anf_45bind482391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls148127, %struct.ScmObj* %argslist56264$anf_45bind482390)
store volatile %struct.ScmObj* %argslist56264$anf_45bind482391, %struct.ScmObj** %stackaddr$prim57345, align 8
%stackaddr$prim57346 = alloca %struct.ScmObj*, align 8
%argslist56264$anf_45bind482392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48240, %struct.ScmObj* %argslist56264$anf_45bind482391)
store volatile %struct.ScmObj* %argslist56264$anf_45bind482392, %struct.ScmObj** %stackaddr$prim57346, align 8
%stackaddr$prim57347 = alloca %struct.ScmObj*, align 8
%argslist56264$anf_45bind482393 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50239, %struct.ScmObj* %argslist56264$anf_45bind482392)
store volatile %struct.ScmObj* %argslist56264$anf_45bind482393, %struct.ScmObj** %stackaddr$prim57347, align 8
%clofunc57348 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48239)
musttail call tailcc void %clofunc57348(%struct.ScmObj* %anf_45bind48239, %struct.ScmObj* %argslist56264$anf_45bind482393)
ret void
}

define tailcc void @proc_clo$ae50239(%struct.ScmObj* %env$ae50239,%struct.ScmObj* %current_45args56260) {
%stackaddr$env-ref57349 = alloca %struct.ScmObj*, align 8
%anf_45bind48238 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50239, i64 0)
store %struct.ScmObj* %anf_45bind48238, %struct.ScmObj** %stackaddr$env-ref57349
%stackaddr$env-ref57350 = alloca %struct.ScmObj*, align 8
%k48365 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50239, i64 1)
store %struct.ScmObj* %k48365, %struct.ScmObj** %stackaddr$env-ref57350
%stackaddr$prim57351 = alloca %struct.ScmObj*, align 8
%_95k48366 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56260)
store volatile %struct.ScmObj* %_95k48366, %struct.ScmObj** %stackaddr$prim57351, align 8
%stackaddr$prim57352 = alloca %struct.ScmObj*, align 8
%current_45args56261 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56260)
store volatile %struct.ScmObj* %current_45args56261, %struct.ScmObj** %stackaddr$prim57352, align 8
%stackaddr$prim57353 = alloca %struct.ScmObj*, align 8
%anf_45bind48241 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56261)
store volatile %struct.ScmObj* %anf_45bind48241, %struct.ScmObj** %stackaddr$prim57353, align 8
%stackaddr$prim57354 = alloca %struct.ScmObj*, align 8
%cpsprim48367 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48238, %struct.ScmObj* %anf_45bind48241)
store volatile %struct.ScmObj* %cpsprim48367, %struct.ScmObj** %stackaddr$prim57354, align 8
%ae50245 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56263$k483650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57355 = alloca %struct.ScmObj*, align 8
%argslist56263$k483651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48367, %struct.ScmObj* %argslist56263$k483650)
store volatile %struct.ScmObj* %argslist56263$k483651, %struct.ScmObj** %stackaddr$prim57355, align 8
%stackaddr$prim57356 = alloca %struct.ScmObj*, align 8
%argslist56263$k483652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50245, %struct.ScmObj* %argslist56263$k483651)
store volatile %struct.ScmObj* %argslist56263$k483652, %struct.ScmObj** %stackaddr$prim57356, align 8
%clofunc57357 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48365)
musttail call tailcc void %clofunc57357(%struct.ScmObj* %k48365, %struct.ScmObj* %argslist56263$k483652)
ret void
}

define tailcc void @proc_clo$ae50199(%struct.ScmObj* %env$ae50199,%struct.ScmObj* %current_45args56266) {
%stackaddr$prim57358 = alloca %struct.ScmObj*, align 8
%k48368 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56266)
store volatile %struct.ScmObj* %k48368, %struct.ScmObj** %stackaddr$prim57358, align 8
%stackaddr$prim57359 = alloca %struct.ScmObj*, align 8
%current_45args56267 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56266)
store volatile %struct.ScmObj* %current_45args56267, %struct.ScmObj** %stackaddr$prim57359, align 8
%stackaddr$prim57360 = alloca %struct.ScmObj*, align 8
%a48131 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56267)
store volatile %struct.ScmObj* %a48131, %struct.ScmObj** %stackaddr$prim57360, align 8
%stackaddr$prim57361 = alloca %struct.ScmObj*, align 8
%current_45args56268 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56267)
store volatile %struct.ScmObj* %current_45args56268, %struct.ScmObj** %stackaddr$prim57361, align 8
%stackaddr$prim57362 = alloca %struct.ScmObj*, align 8
%b48130 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56268)
store volatile %struct.ScmObj* %b48130, %struct.ScmObj** %stackaddr$prim57362, align 8
%stackaddr$prim57363 = alloca %struct.ScmObj*, align 8
%anf_45bind48236 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a48131, %struct.ScmObj* %b48130)
store volatile %struct.ScmObj* %anf_45bind48236, %struct.ScmObj** %stackaddr$prim57363, align 8
%stackaddr$prim57364 = alloca %struct.ScmObj*, align 8
%cpsprim48369 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind48236)
store volatile %struct.ScmObj* %cpsprim48369, %struct.ScmObj** %stackaddr$prim57364, align 8
%ae50204 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56270$k483680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57365 = alloca %struct.ScmObj*, align 8
%argslist56270$k483681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48369, %struct.ScmObj* %argslist56270$k483680)
store volatile %struct.ScmObj* %argslist56270$k483681, %struct.ScmObj** %stackaddr$prim57365, align 8
%stackaddr$prim57366 = alloca %struct.ScmObj*, align 8
%argslist56270$k483682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50204, %struct.ScmObj* %argslist56270$k483681)
store volatile %struct.ScmObj* %argslist56270$k483682, %struct.ScmObj** %stackaddr$prim57366, align 8
%clofunc57367 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48368)
musttail call tailcc void %clofunc57367(%struct.ScmObj* %k48368, %struct.ScmObj* %argslist56270$k483682)
ret void
}

define tailcc void @proc_clo$ae50175(%struct.ScmObj* %env$ae50175,%struct.ScmObj* %current_45args56272) {
%stackaddr$prim57368 = alloca %struct.ScmObj*, align 8
%k48370 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56272)
store volatile %struct.ScmObj* %k48370, %struct.ScmObj** %stackaddr$prim57368, align 8
%stackaddr$prim57369 = alloca %struct.ScmObj*, align 8
%current_45args56273 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56272)
store volatile %struct.ScmObj* %current_45args56273, %struct.ScmObj** %stackaddr$prim57369, align 8
%stackaddr$prim57370 = alloca %struct.ScmObj*, align 8
%a48134 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56273)
store volatile %struct.ScmObj* %a48134, %struct.ScmObj** %stackaddr$prim57370, align 8
%stackaddr$prim57371 = alloca %struct.ScmObj*, align 8
%current_45args56274 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56273)
store volatile %struct.ScmObj* %current_45args56274, %struct.ScmObj** %stackaddr$prim57371, align 8
%stackaddr$prim57372 = alloca %struct.ScmObj*, align 8
%b48133 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56274)
store volatile %struct.ScmObj* %b48133, %struct.ScmObj** %stackaddr$prim57372, align 8
%stackaddr$prim57373 = alloca %struct.ScmObj*, align 8
%anf_45bind48235 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a48134, %struct.ScmObj* %b48133)
store volatile %struct.ScmObj* %anf_45bind48235, %struct.ScmObj** %stackaddr$prim57373, align 8
%stackaddr$prim57374 = alloca %struct.ScmObj*, align 8
%cpsprim48371 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind48235)
store volatile %struct.ScmObj* %cpsprim48371, %struct.ScmObj** %stackaddr$prim57374, align 8
%ae50180 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56276$k483700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57375 = alloca %struct.ScmObj*, align 8
%argslist56276$k483701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48371, %struct.ScmObj* %argslist56276$k483700)
store volatile %struct.ScmObj* %argslist56276$k483701, %struct.ScmObj** %stackaddr$prim57375, align 8
%stackaddr$prim57376 = alloca %struct.ScmObj*, align 8
%argslist56276$k483702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50180, %struct.ScmObj* %argslist56276$k483701)
store volatile %struct.ScmObj* %argslist56276$k483702, %struct.ScmObj** %stackaddr$prim57376, align 8
%clofunc57377 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48370)
musttail call tailcc void %clofunc57377(%struct.ScmObj* %k48370, %struct.ScmObj* %argslist56276$k483702)
ret void
}

define tailcc void @proc_clo$ae49781(%struct.ScmObj* %env$ae49781,%struct.ScmObj* %current_45args56279) {
%stackaddr$env-ref57378 = alloca %struct.ScmObj*, align 8
%_37foldr148053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49781, i64 0)
store %struct.ScmObj* %_37foldr148053, %struct.ScmObj** %stackaddr$env-ref57378
%stackaddr$env-ref57379 = alloca %struct.ScmObj*, align 8
%_37map148084 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49781, i64 1)
store %struct.ScmObj* %_37map148084, %struct.ScmObj** %stackaddr$env-ref57379
%stackaddr$env-ref57380 = alloca %struct.ScmObj*, align 8
%_37foldr48058 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49781, i64 2)
store %struct.ScmObj* %_37foldr48058, %struct.ScmObj** %stackaddr$env-ref57380
%stackaddr$prim57381 = alloca %struct.ScmObj*, align 8
%k48372 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56279)
store volatile %struct.ScmObj* %k48372, %struct.ScmObj** %stackaddr$prim57381, align 8
%stackaddr$prim57382 = alloca %struct.ScmObj*, align 8
%current_45args56280 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56279)
store volatile %struct.ScmObj* %current_45args56280, %struct.ScmObj** %stackaddr$prim57382, align 8
%stackaddr$prim57383 = alloca %struct.ScmObj*, align 8
%_37foldl48136 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56280)
store volatile %struct.ScmObj* %_37foldl48136, %struct.ScmObj** %stackaddr$prim57383, align 8
%ae49783 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57384 = alloca %struct.ScmObj*, align 8
%fptrToInt57385 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49784 to i64
%ae49784 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57385)
store volatile %struct.ScmObj* %ae49784, %struct.ScmObj** %stackaddr$makeclosure57384, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49784, %struct.ScmObj* %_37foldl48136, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49784, %struct.ScmObj* %_37foldr148053, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49784, %struct.ScmObj* %_37map148084, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49784, %struct.ScmObj* %_37foldr48058, i64 3)
%argslist56337$k483720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57386 = alloca %struct.ScmObj*, align 8
%argslist56337$k483721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49784, %struct.ScmObj* %argslist56337$k483720)
store volatile %struct.ScmObj* %argslist56337$k483721, %struct.ScmObj** %stackaddr$prim57386, align 8
%stackaddr$prim57387 = alloca %struct.ScmObj*, align 8
%argslist56337$k483722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49783, %struct.ScmObj* %argslist56337$k483721)
store volatile %struct.ScmObj* %argslist56337$k483722, %struct.ScmObj** %stackaddr$prim57387, align 8
%clofunc57388 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48372)
musttail call tailcc void %clofunc57388(%struct.ScmObj* %k48372, %struct.ScmObj* %argslist56337$k483722)
ret void
}

define tailcc void @proc_clo$ae49784(%struct.ScmObj* %env$ae49784,%struct.ScmObj* %args4813748373) {
%stackaddr$env-ref57389 = alloca %struct.ScmObj*, align 8
%_37foldl48136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49784, i64 0)
store %struct.ScmObj* %_37foldl48136, %struct.ScmObj** %stackaddr$env-ref57389
%stackaddr$env-ref57390 = alloca %struct.ScmObj*, align 8
%_37foldr148053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49784, i64 1)
store %struct.ScmObj* %_37foldr148053, %struct.ScmObj** %stackaddr$env-ref57390
%stackaddr$env-ref57391 = alloca %struct.ScmObj*, align 8
%_37map148084 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49784, i64 2)
store %struct.ScmObj* %_37map148084, %struct.ScmObj** %stackaddr$env-ref57391
%stackaddr$env-ref57392 = alloca %struct.ScmObj*, align 8
%_37foldr48058 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49784, i64 3)
store %struct.ScmObj* %_37foldr48058, %struct.ScmObj** %stackaddr$env-ref57392
%stackaddr$prim57393 = alloca %struct.ScmObj*, align 8
%k48374 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4813748373)
store volatile %struct.ScmObj* %k48374, %struct.ScmObj** %stackaddr$prim57393, align 8
%stackaddr$prim57394 = alloca %struct.ScmObj*, align 8
%args48137 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4813748373)
store volatile %struct.ScmObj* %args48137, %struct.ScmObj** %stackaddr$prim57394, align 8
%stackaddr$prim57395 = alloca %struct.ScmObj*, align 8
%f48140 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48137)
store volatile %struct.ScmObj* %f48140, %struct.ScmObj** %stackaddr$prim57395, align 8
%stackaddr$prim57396 = alloca %struct.ScmObj*, align 8
%anf_45bind48223 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48137)
store volatile %struct.ScmObj* %anf_45bind48223, %struct.ScmObj** %stackaddr$prim57396, align 8
%stackaddr$prim57397 = alloca %struct.ScmObj*, align 8
%acc48139 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48223)
store volatile %struct.ScmObj* %acc48139, %struct.ScmObj** %stackaddr$prim57397, align 8
%stackaddr$prim57398 = alloca %struct.ScmObj*, align 8
%anf_45bind48224 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48137)
store volatile %struct.ScmObj* %anf_45bind48224, %struct.ScmObj** %stackaddr$prim57398, align 8
%stackaddr$prim57399 = alloca %struct.ScmObj*, align 8
%lsts48138 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48224)
store volatile %struct.ScmObj* %lsts48138, %struct.ScmObj** %stackaddr$prim57399, align 8
%stackaddr$makeclosure57400 = alloca %struct.ScmObj*, align 8
%fptrToInt57401 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49792 to i64
%ae49792 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt57401)
store volatile %struct.ScmObj* %ae49792, %struct.ScmObj** %stackaddr$makeclosure57400, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49792, %struct.ScmObj* %lsts48138, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49792, %struct.ScmObj* %_37foldr48058, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49792, %struct.ScmObj* %_37foldl48136, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49792, %struct.ScmObj* %k48374, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49792, %struct.ScmObj* %_37foldr148053, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49792, %struct.ScmObj* %_37map148084, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49792, %struct.ScmObj* %f48140, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae49792, %struct.ScmObj* %acc48139, i64 7)
%ae49793 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57402 = alloca %struct.ScmObj*, align 8
%fptrToInt57403 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49794 to i64
%ae49794 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57403)
store volatile %struct.ScmObj* %ae49794, %struct.ScmObj** %stackaddr$makeclosure57402, align 8
%argslist56336$ae497920 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57404 = alloca %struct.ScmObj*, align 8
%argslist56336$ae497921 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49794, %struct.ScmObj* %argslist56336$ae497920)
store volatile %struct.ScmObj* %argslist56336$ae497921, %struct.ScmObj** %stackaddr$prim57404, align 8
%stackaddr$prim57405 = alloca %struct.ScmObj*, align 8
%argslist56336$ae497922 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49793, %struct.ScmObj* %argslist56336$ae497921)
store volatile %struct.ScmObj* %argslist56336$ae497922, %struct.ScmObj** %stackaddr$prim57405, align 8
%clofunc57406 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49792)
musttail call tailcc void %clofunc57406(%struct.ScmObj* %ae49792, %struct.ScmObj* %argslist56336$ae497922)
ret void
}

define tailcc void @proc_clo$ae49792(%struct.ScmObj* %env$ae49792,%struct.ScmObj* %current_45args56282) {
%stackaddr$env-ref57407 = alloca %struct.ScmObj*, align 8
%lsts48138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49792, i64 0)
store %struct.ScmObj* %lsts48138, %struct.ScmObj** %stackaddr$env-ref57407
%stackaddr$env-ref57408 = alloca %struct.ScmObj*, align 8
%_37foldr48058 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49792, i64 1)
store %struct.ScmObj* %_37foldr48058, %struct.ScmObj** %stackaddr$env-ref57408
%stackaddr$env-ref57409 = alloca %struct.ScmObj*, align 8
%_37foldl48136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49792, i64 2)
store %struct.ScmObj* %_37foldl48136, %struct.ScmObj** %stackaddr$env-ref57409
%stackaddr$env-ref57410 = alloca %struct.ScmObj*, align 8
%k48374 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49792, i64 3)
store %struct.ScmObj* %k48374, %struct.ScmObj** %stackaddr$env-ref57410
%stackaddr$env-ref57411 = alloca %struct.ScmObj*, align 8
%_37foldr148053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49792, i64 4)
store %struct.ScmObj* %_37foldr148053, %struct.ScmObj** %stackaddr$env-ref57411
%stackaddr$env-ref57412 = alloca %struct.ScmObj*, align 8
%_37map148084 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49792, i64 5)
store %struct.ScmObj* %_37map148084, %struct.ScmObj** %stackaddr$env-ref57412
%stackaddr$env-ref57413 = alloca %struct.ScmObj*, align 8
%f48140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49792, i64 6)
store %struct.ScmObj* %f48140, %struct.ScmObj** %stackaddr$env-ref57413
%stackaddr$env-ref57414 = alloca %struct.ScmObj*, align 8
%acc48139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49792, i64 7)
store %struct.ScmObj* %acc48139, %struct.ScmObj** %stackaddr$env-ref57414
%stackaddr$prim57415 = alloca %struct.ScmObj*, align 8
%_95k48375 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56282)
store volatile %struct.ScmObj* %_95k48375, %struct.ScmObj** %stackaddr$prim57415, align 8
%stackaddr$prim57416 = alloca %struct.ScmObj*, align 8
%current_45args56283 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56282)
store volatile %struct.ScmObj* %current_45args56283, %struct.ScmObj** %stackaddr$prim57416, align 8
%stackaddr$prim57417 = alloca %struct.ScmObj*, align 8
%anf_45bind48225 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56283)
store volatile %struct.ScmObj* %anf_45bind48225, %struct.ScmObj** %stackaddr$prim57417, align 8
%stackaddr$makeclosure57418 = alloca %struct.ScmObj*, align 8
%fptrToInt57419 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49824 to i64
%ae49824 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt57419)
store volatile %struct.ScmObj* %ae49824, %struct.ScmObj** %stackaddr$makeclosure57418, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49824, %struct.ScmObj* %lsts48138, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49824, %struct.ScmObj* %_37foldr48058, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49824, %struct.ScmObj* %_37foldl48136, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49824, %struct.ScmObj* %k48374, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49824, %struct.ScmObj* %_37map148084, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49824, %struct.ScmObj* %f48140, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49824, %struct.ScmObj* %acc48139, i64 6)
%ae49826 = call %struct.ScmObj* @const_init_false()
%argslist56329$_37foldr1480530 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57420 = alloca %struct.ScmObj*, align 8
%argslist56329$_37foldr1480531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48138, %struct.ScmObj* %argslist56329$_37foldr1480530)
store volatile %struct.ScmObj* %argslist56329$_37foldr1480531, %struct.ScmObj** %stackaddr$prim57420, align 8
%stackaddr$prim57421 = alloca %struct.ScmObj*, align 8
%argslist56329$_37foldr1480532 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49826, %struct.ScmObj* %argslist56329$_37foldr1480531)
store volatile %struct.ScmObj* %argslist56329$_37foldr1480532, %struct.ScmObj** %stackaddr$prim57421, align 8
%stackaddr$prim57422 = alloca %struct.ScmObj*, align 8
%argslist56329$_37foldr1480533 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48225, %struct.ScmObj* %argslist56329$_37foldr1480532)
store volatile %struct.ScmObj* %argslist56329$_37foldr1480533, %struct.ScmObj** %stackaddr$prim57422, align 8
%stackaddr$prim57423 = alloca %struct.ScmObj*, align 8
%argslist56329$_37foldr1480534 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49824, %struct.ScmObj* %argslist56329$_37foldr1480533)
store volatile %struct.ScmObj* %argslist56329$_37foldr1480534, %struct.ScmObj** %stackaddr$prim57423, align 8
%clofunc57424 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148053)
musttail call tailcc void %clofunc57424(%struct.ScmObj* %_37foldr148053, %struct.ScmObj* %argslist56329$_37foldr1480534)
ret void
}

define tailcc void @proc_clo$ae49824(%struct.ScmObj* %env$ae49824,%struct.ScmObj* %current_45args56285) {
%stackaddr$env-ref57425 = alloca %struct.ScmObj*, align 8
%lsts48138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49824, i64 0)
store %struct.ScmObj* %lsts48138, %struct.ScmObj** %stackaddr$env-ref57425
%stackaddr$env-ref57426 = alloca %struct.ScmObj*, align 8
%_37foldr48058 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49824, i64 1)
store %struct.ScmObj* %_37foldr48058, %struct.ScmObj** %stackaddr$env-ref57426
%stackaddr$env-ref57427 = alloca %struct.ScmObj*, align 8
%_37foldl48136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49824, i64 2)
store %struct.ScmObj* %_37foldl48136, %struct.ScmObj** %stackaddr$env-ref57427
%stackaddr$env-ref57428 = alloca %struct.ScmObj*, align 8
%k48374 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49824, i64 3)
store %struct.ScmObj* %k48374, %struct.ScmObj** %stackaddr$env-ref57428
%stackaddr$env-ref57429 = alloca %struct.ScmObj*, align 8
%_37map148084 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49824, i64 4)
store %struct.ScmObj* %_37map148084, %struct.ScmObj** %stackaddr$env-ref57429
%stackaddr$env-ref57430 = alloca %struct.ScmObj*, align 8
%f48140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49824, i64 5)
store %struct.ScmObj* %f48140, %struct.ScmObj** %stackaddr$env-ref57430
%stackaddr$env-ref57431 = alloca %struct.ScmObj*, align 8
%acc48139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49824, i64 6)
store %struct.ScmObj* %acc48139, %struct.ScmObj** %stackaddr$env-ref57431
%stackaddr$prim57432 = alloca %struct.ScmObj*, align 8
%_95k48376 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56285)
store volatile %struct.ScmObj* %_95k48376, %struct.ScmObj** %stackaddr$prim57432, align 8
%stackaddr$prim57433 = alloca %struct.ScmObj*, align 8
%current_45args56286 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56285)
store volatile %struct.ScmObj* %current_45args56286, %struct.ScmObj** %stackaddr$prim57433, align 8
%stackaddr$prim57434 = alloca %struct.ScmObj*, align 8
%anf_45bind48226 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56286)
store volatile %struct.ScmObj* %anf_45bind48226, %struct.ScmObj** %stackaddr$prim57434, align 8
%truthy$cmp57435 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48226)
%cmp$cmp57435 = icmp eq i64 %truthy$cmp57435, 1
br i1 %cmp$cmp57435, label %truebranch$cmp57435, label %falsebranch$cmp57435
truebranch$cmp57435:
%ae49835 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56288$k483740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57436 = alloca %struct.ScmObj*, align 8
%argslist56288$k483741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48139, %struct.ScmObj* %argslist56288$k483740)
store volatile %struct.ScmObj* %argslist56288$k483741, %struct.ScmObj** %stackaddr$prim57436, align 8
%stackaddr$prim57437 = alloca %struct.ScmObj*, align 8
%argslist56288$k483742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49835, %struct.ScmObj* %argslist56288$k483741)
store volatile %struct.ScmObj* %argslist56288$k483742, %struct.ScmObj** %stackaddr$prim57437, align 8
%clofunc57438 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48374)
musttail call tailcc void %clofunc57438(%struct.ScmObj* %k48374, %struct.ScmObj* %argslist56288$k483742)
ret void
falsebranch$cmp57435:
%stackaddr$makeclosure57439 = alloca %struct.ScmObj*, align 8
%fptrToInt57440 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49840 to i64
%ae49840 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt57440)
store volatile %struct.ScmObj* %ae49840, %struct.ScmObj** %stackaddr$makeclosure57439, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49840, %struct.ScmObj* %lsts48138, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49840, %struct.ScmObj* %_37foldr48058, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49840, %struct.ScmObj* %_37foldl48136, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49840, %struct.ScmObj* %k48374, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49840, %struct.ScmObj* %_37map148084, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49840, %struct.ScmObj* %f48140, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49840, %struct.ScmObj* %acc48139, i64 6)
%ae49841 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57441 = alloca %struct.ScmObj*, align 8
%fptrToInt57442 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49842 to i64
%ae49842 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57442)
store volatile %struct.ScmObj* %ae49842, %struct.ScmObj** %stackaddr$makeclosure57441, align 8
%argslist56328$ae498400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57443 = alloca %struct.ScmObj*, align 8
%argslist56328$ae498401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49842, %struct.ScmObj* %argslist56328$ae498400)
store volatile %struct.ScmObj* %argslist56328$ae498401, %struct.ScmObj** %stackaddr$prim57443, align 8
%stackaddr$prim57444 = alloca %struct.ScmObj*, align 8
%argslist56328$ae498402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49841, %struct.ScmObj* %argslist56328$ae498401)
store volatile %struct.ScmObj* %argslist56328$ae498402, %struct.ScmObj** %stackaddr$prim57444, align 8
%clofunc57445 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49840)
musttail call tailcc void %clofunc57445(%struct.ScmObj* %ae49840, %struct.ScmObj* %argslist56328$ae498402)
ret void
}

define tailcc void @proc_clo$ae49840(%struct.ScmObj* %env$ae49840,%struct.ScmObj* %current_45args56289) {
%stackaddr$env-ref57446 = alloca %struct.ScmObj*, align 8
%lsts48138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49840, i64 0)
store %struct.ScmObj* %lsts48138, %struct.ScmObj** %stackaddr$env-ref57446
%stackaddr$env-ref57447 = alloca %struct.ScmObj*, align 8
%_37foldr48058 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49840, i64 1)
store %struct.ScmObj* %_37foldr48058, %struct.ScmObj** %stackaddr$env-ref57447
%stackaddr$env-ref57448 = alloca %struct.ScmObj*, align 8
%_37foldl48136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49840, i64 2)
store %struct.ScmObj* %_37foldl48136, %struct.ScmObj** %stackaddr$env-ref57448
%stackaddr$env-ref57449 = alloca %struct.ScmObj*, align 8
%k48374 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49840, i64 3)
store %struct.ScmObj* %k48374, %struct.ScmObj** %stackaddr$env-ref57449
%stackaddr$env-ref57450 = alloca %struct.ScmObj*, align 8
%_37map148084 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49840, i64 4)
store %struct.ScmObj* %_37map148084, %struct.ScmObj** %stackaddr$env-ref57450
%stackaddr$env-ref57451 = alloca %struct.ScmObj*, align 8
%f48140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49840, i64 5)
store %struct.ScmObj* %f48140, %struct.ScmObj** %stackaddr$env-ref57451
%stackaddr$env-ref57452 = alloca %struct.ScmObj*, align 8
%acc48139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49840, i64 6)
store %struct.ScmObj* %acc48139, %struct.ScmObj** %stackaddr$env-ref57452
%stackaddr$prim57453 = alloca %struct.ScmObj*, align 8
%_95k48377 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56289)
store volatile %struct.ScmObj* %_95k48377, %struct.ScmObj** %stackaddr$prim57453, align 8
%stackaddr$prim57454 = alloca %struct.ScmObj*, align 8
%current_45args56290 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56289)
store volatile %struct.ScmObj* %current_45args56290, %struct.ScmObj** %stackaddr$prim57454, align 8
%stackaddr$prim57455 = alloca %struct.ScmObj*, align 8
%anf_45bind48227 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56290)
store volatile %struct.ScmObj* %anf_45bind48227, %struct.ScmObj** %stackaddr$prim57455, align 8
%stackaddr$makeclosure57456 = alloca %struct.ScmObj*, align 8
%fptrToInt57457 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49861 to i64
%ae49861 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt57457)
store volatile %struct.ScmObj* %ae49861, %struct.ScmObj** %stackaddr$makeclosure57456, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49861, %struct.ScmObj* %lsts48138, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49861, %struct.ScmObj* %_37foldr48058, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49861, %struct.ScmObj* %_37foldl48136, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49861, %struct.ScmObj* %k48374, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49861, %struct.ScmObj* %_37map148084, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49861, %struct.ScmObj* %f48140, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49861, %struct.ScmObj* %acc48139, i64 6)
%argslist56323$_37map1480840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57458 = alloca %struct.ScmObj*, align 8
%argslist56323$_37map1480841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48138, %struct.ScmObj* %argslist56323$_37map1480840)
store volatile %struct.ScmObj* %argslist56323$_37map1480841, %struct.ScmObj** %stackaddr$prim57458, align 8
%stackaddr$prim57459 = alloca %struct.ScmObj*, align 8
%argslist56323$_37map1480842 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48227, %struct.ScmObj* %argslist56323$_37map1480841)
store volatile %struct.ScmObj* %argslist56323$_37map1480842, %struct.ScmObj** %stackaddr$prim57459, align 8
%stackaddr$prim57460 = alloca %struct.ScmObj*, align 8
%argslist56323$_37map1480843 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49861, %struct.ScmObj* %argslist56323$_37map1480842)
store volatile %struct.ScmObj* %argslist56323$_37map1480843, %struct.ScmObj** %stackaddr$prim57460, align 8
%clofunc57461 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148084)
musttail call tailcc void %clofunc57461(%struct.ScmObj* %_37map148084, %struct.ScmObj* %argslist56323$_37map1480843)
ret void
}

define tailcc void @proc_clo$ae49861(%struct.ScmObj* %env$ae49861,%struct.ScmObj* %current_45args56292) {
%stackaddr$env-ref57462 = alloca %struct.ScmObj*, align 8
%lsts48138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49861, i64 0)
store %struct.ScmObj* %lsts48138, %struct.ScmObj** %stackaddr$env-ref57462
%stackaddr$env-ref57463 = alloca %struct.ScmObj*, align 8
%_37foldr48058 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49861, i64 1)
store %struct.ScmObj* %_37foldr48058, %struct.ScmObj** %stackaddr$env-ref57463
%stackaddr$env-ref57464 = alloca %struct.ScmObj*, align 8
%_37foldl48136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49861, i64 2)
store %struct.ScmObj* %_37foldl48136, %struct.ScmObj** %stackaddr$env-ref57464
%stackaddr$env-ref57465 = alloca %struct.ScmObj*, align 8
%k48374 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49861, i64 3)
store %struct.ScmObj* %k48374, %struct.ScmObj** %stackaddr$env-ref57465
%stackaddr$env-ref57466 = alloca %struct.ScmObj*, align 8
%_37map148084 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49861, i64 4)
store %struct.ScmObj* %_37map148084, %struct.ScmObj** %stackaddr$env-ref57466
%stackaddr$env-ref57467 = alloca %struct.ScmObj*, align 8
%f48140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49861, i64 5)
store %struct.ScmObj* %f48140, %struct.ScmObj** %stackaddr$env-ref57467
%stackaddr$env-ref57468 = alloca %struct.ScmObj*, align 8
%acc48139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49861, i64 6)
store %struct.ScmObj* %acc48139, %struct.ScmObj** %stackaddr$env-ref57468
%stackaddr$prim57469 = alloca %struct.ScmObj*, align 8
%_95k48378 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56292)
store volatile %struct.ScmObj* %_95k48378, %struct.ScmObj** %stackaddr$prim57469, align 8
%stackaddr$prim57470 = alloca %struct.ScmObj*, align 8
%current_45args56293 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56292)
store volatile %struct.ScmObj* %current_45args56293, %struct.ScmObj** %stackaddr$prim57470, align 8
%stackaddr$prim57471 = alloca %struct.ScmObj*, align 8
%lsts_4348145 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56293)
store volatile %struct.ScmObj* %lsts_4348145, %struct.ScmObj** %stackaddr$prim57471, align 8
%stackaddr$makeclosure57472 = alloca %struct.ScmObj*, align 8
%fptrToInt57473 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49864 to i64
%ae49864 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt57473)
store volatile %struct.ScmObj* %ae49864, %struct.ScmObj** %stackaddr$makeclosure57472, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49864, %struct.ScmObj* %lsts48138, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49864, %struct.ScmObj* %_37foldr48058, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49864, %struct.ScmObj* %_37foldl48136, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49864, %struct.ScmObj* %k48374, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49864, %struct.ScmObj* %_37map148084, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49864, %struct.ScmObj* %lsts_4348145, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49864, %struct.ScmObj* %f48140, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae49864, %struct.ScmObj* %acc48139, i64 7)
%ae49865 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57474 = alloca %struct.ScmObj*, align 8
%fptrToInt57475 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49866 to i64
%ae49866 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57475)
store volatile %struct.ScmObj* %ae49866, %struct.ScmObj** %stackaddr$makeclosure57474, align 8
%argslist56322$ae498640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57476 = alloca %struct.ScmObj*, align 8
%argslist56322$ae498641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49866, %struct.ScmObj* %argslist56322$ae498640)
store volatile %struct.ScmObj* %argslist56322$ae498641, %struct.ScmObj** %stackaddr$prim57476, align 8
%stackaddr$prim57477 = alloca %struct.ScmObj*, align 8
%argslist56322$ae498642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49865, %struct.ScmObj* %argslist56322$ae498641)
store volatile %struct.ScmObj* %argslist56322$ae498642, %struct.ScmObj** %stackaddr$prim57477, align 8
%clofunc57478 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49864)
musttail call tailcc void %clofunc57478(%struct.ScmObj* %ae49864, %struct.ScmObj* %argslist56322$ae498642)
ret void
}

define tailcc void @proc_clo$ae49864(%struct.ScmObj* %env$ae49864,%struct.ScmObj* %current_45args56295) {
%stackaddr$env-ref57479 = alloca %struct.ScmObj*, align 8
%lsts48138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49864, i64 0)
store %struct.ScmObj* %lsts48138, %struct.ScmObj** %stackaddr$env-ref57479
%stackaddr$env-ref57480 = alloca %struct.ScmObj*, align 8
%_37foldr48058 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49864, i64 1)
store %struct.ScmObj* %_37foldr48058, %struct.ScmObj** %stackaddr$env-ref57480
%stackaddr$env-ref57481 = alloca %struct.ScmObj*, align 8
%_37foldl48136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49864, i64 2)
store %struct.ScmObj* %_37foldl48136, %struct.ScmObj** %stackaddr$env-ref57481
%stackaddr$env-ref57482 = alloca %struct.ScmObj*, align 8
%k48374 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49864, i64 3)
store %struct.ScmObj* %k48374, %struct.ScmObj** %stackaddr$env-ref57482
%stackaddr$env-ref57483 = alloca %struct.ScmObj*, align 8
%_37map148084 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49864, i64 4)
store %struct.ScmObj* %_37map148084, %struct.ScmObj** %stackaddr$env-ref57483
%stackaddr$env-ref57484 = alloca %struct.ScmObj*, align 8
%lsts_4348145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49864, i64 5)
store %struct.ScmObj* %lsts_4348145, %struct.ScmObj** %stackaddr$env-ref57484
%stackaddr$env-ref57485 = alloca %struct.ScmObj*, align 8
%f48140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49864, i64 6)
store %struct.ScmObj* %f48140, %struct.ScmObj** %stackaddr$env-ref57485
%stackaddr$env-ref57486 = alloca %struct.ScmObj*, align 8
%acc48139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49864, i64 7)
store %struct.ScmObj* %acc48139, %struct.ScmObj** %stackaddr$env-ref57486
%stackaddr$prim57487 = alloca %struct.ScmObj*, align 8
%_95k48379 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56295)
store volatile %struct.ScmObj* %_95k48379, %struct.ScmObj** %stackaddr$prim57487, align 8
%stackaddr$prim57488 = alloca %struct.ScmObj*, align 8
%current_45args56296 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56295)
store volatile %struct.ScmObj* %current_45args56296, %struct.ScmObj** %stackaddr$prim57488, align 8
%stackaddr$prim57489 = alloca %struct.ScmObj*, align 8
%anf_45bind48228 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56296)
store volatile %struct.ScmObj* %anf_45bind48228, %struct.ScmObj** %stackaddr$prim57489, align 8
%stackaddr$makeclosure57490 = alloca %struct.ScmObj*, align 8
%fptrToInt57491 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49885 to i64
%ae49885 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt57491)
store volatile %struct.ScmObj* %ae49885, %struct.ScmObj** %stackaddr$makeclosure57490, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49885, %struct.ScmObj* %_37foldl48136, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49885, %struct.ScmObj* %k48374, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49885, %struct.ScmObj* %lsts_4348145, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49885, %struct.ScmObj* %f48140, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49885, %struct.ScmObj* %acc48139, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49885, %struct.ScmObj* %_37foldr48058, i64 5)
%argslist56317$_37map1480840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57492 = alloca %struct.ScmObj*, align 8
%argslist56317$_37map1480841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48138, %struct.ScmObj* %argslist56317$_37map1480840)
store volatile %struct.ScmObj* %argslist56317$_37map1480841, %struct.ScmObj** %stackaddr$prim57492, align 8
%stackaddr$prim57493 = alloca %struct.ScmObj*, align 8
%argslist56317$_37map1480842 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48228, %struct.ScmObj* %argslist56317$_37map1480841)
store volatile %struct.ScmObj* %argslist56317$_37map1480842, %struct.ScmObj** %stackaddr$prim57493, align 8
%stackaddr$prim57494 = alloca %struct.ScmObj*, align 8
%argslist56317$_37map1480843 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49885, %struct.ScmObj* %argslist56317$_37map1480842)
store volatile %struct.ScmObj* %argslist56317$_37map1480843, %struct.ScmObj** %stackaddr$prim57494, align 8
%clofunc57495 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148084)
musttail call tailcc void %clofunc57495(%struct.ScmObj* %_37map148084, %struct.ScmObj* %argslist56317$_37map1480843)
ret void
}

define tailcc void @proc_clo$ae49885(%struct.ScmObj* %env$ae49885,%struct.ScmObj* %current_45args56298) {
%stackaddr$env-ref57496 = alloca %struct.ScmObj*, align 8
%_37foldl48136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49885, i64 0)
store %struct.ScmObj* %_37foldl48136, %struct.ScmObj** %stackaddr$env-ref57496
%stackaddr$env-ref57497 = alloca %struct.ScmObj*, align 8
%k48374 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49885, i64 1)
store %struct.ScmObj* %k48374, %struct.ScmObj** %stackaddr$env-ref57497
%stackaddr$env-ref57498 = alloca %struct.ScmObj*, align 8
%lsts_4348145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49885, i64 2)
store %struct.ScmObj* %lsts_4348145, %struct.ScmObj** %stackaddr$env-ref57498
%stackaddr$env-ref57499 = alloca %struct.ScmObj*, align 8
%f48140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49885, i64 3)
store %struct.ScmObj* %f48140, %struct.ScmObj** %stackaddr$env-ref57499
%stackaddr$env-ref57500 = alloca %struct.ScmObj*, align 8
%acc48139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49885, i64 4)
store %struct.ScmObj* %acc48139, %struct.ScmObj** %stackaddr$env-ref57500
%stackaddr$env-ref57501 = alloca %struct.ScmObj*, align 8
%_37foldr48058 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49885, i64 5)
store %struct.ScmObj* %_37foldr48058, %struct.ScmObj** %stackaddr$env-ref57501
%stackaddr$prim57502 = alloca %struct.ScmObj*, align 8
%_95k48380 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56298)
store volatile %struct.ScmObj* %_95k48380, %struct.ScmObj** %stackaddr$prim57502, align 8
%stackaddr$prim57503 = alloca %struct.ScmObj*, align 8
%current_45args56299 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56298)
store volatile %struct.ScmObj* %current_45args56299, %struct.ScmObj** %stackaddr$prim57503, align 8
%stackaddr$prim57504 = alloca %struct.ScmObj*, align 8
%vs48143 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56299)
store volatile %struct.ScmObj* %vs48143, %struct.ScmObj** %stackaddr$prim57504, align 8
%stackaddr$makeclosure57505 = alloca %struct.ScmObj*, align 8
%fptrToInt57506 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49888 to i64
%ae49888 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt57506)
store volatile %struct.ScmObj* %ae49888, %struct.ScmObj** %stackaddr$makeclosure57505, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49888, %struct.ScmObj* %_37foldl48136, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49888, %struct.ScmObj* %k48374, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49888, %struct.ScmObj* %lsts_4348145, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49888, %struct.ScmObj* %vs48143, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49888, %struct.ScmObj* %f48140, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49888, %struct.ScmObj* %acc48139, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49888, %struct.ScmObj* %_37foldr48058, i64 6)
%ae49889 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57507 = alloca %struct.ScmObj*, align 8
%fptrToInt57508 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49890 to i64
%ae49890 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57508)
store volatile %struct.ScmObj* %ae49890, %struct.ScmObj** %stackaddr$makeclosure57507, align 8
%argslist56316$ae498880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57509 = alloca %struct.ScmObj*, align 8
%argslist56316$ae498881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49890, %struct.ScmObj* %argslist56316$ae498880)
store volatile %struct.ScmObj* %argslist56316$ae498881, %struct.ScmObj** %stackaddr$prim57509, align 8
%stackaddr$prim57510 = alloca %struct.ScmObj*, align 8
%argslist56316$ae498882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49889, %struct.ScmObj* %argslist56316$ae498881)
store volatile %struct.ScmObj* %argslist56316$ae498882, %struct.ScmObj** %stackaddr$prim57510, align 8
%clofunc57511 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49888)
musttail call tailcc void %clofunc57511(%struct.ScmObj* %ae49888, %struct.ScmObj* %argslist56316$ae498882)
ret void
}

define tailcc void @proc_clo$ae49888(%struct.ScmObj* %env$ae49888,%struct.ScmObj* %current_45args56301) {
%stackaddr$env-ref57512 = alloca %struct.ScmObj*, align 8
%_37foldl48136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49888, i64 0)
store %struct.ScmObj* %_37foldl48136, %struct.ScmObj** %stackaddr$env-ref57512
%stackaddr$env-ref57513 = alloca %struct.ScmObj*, align 8
%k48374 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49888, i64 1)
store %struct.ScmObj* %k48374, %struct.ScmObj** %stackaddr$env-ref57513
%stackaddr$env-ref57514 = alloca %struct.ScmObj*, align 8
%lsts_4348145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49888, i64 2)
store %struct.ScmObj* %lsts_4348145, %struct.ScmObj** %stackaddr$env-ref57514
%stackaddr$env-ref57515 = alloca %struct.ScmObj*, align 8
%vs48143 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49888, i64 3)
store %struct.ScmObj* %vs48143, %struct.ScmObj** %stackaddr$env-ref57515
%stackaddr$env-ref57516 = alloca %struct.ScmObj*, align 8
%f48140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49888, i64 4)
store %struct.ScmObj* %f48140, %struct.ScmObj** %stackaddr$env-ref57516
%stackaddr$env-ref57517 = alloca %struct.ScmObj*, align 8
%acc48139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49888, i64 5)
store %struct.ScmObj* %acc48139, %struct.ScmObj** %stackaddr$env-ref57517
%stackaddr$env-ref57518 = alloca %struct.ScmObj*, align 8
%_37foldr48058 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49888, i64 6)
store %struct.ScmObj* %_37foldr48058, %struct.ScmObj** %stackaddr$env-ref57518
%stackaddr$prim57519 = alloca %struct.ScmObj*, align 8
%_95k48381 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56301)
store volatile %struct.ScmObj* %_95k48381, %struct.ScmObj** %stackaddr$prim57519, align 8
%stackaddr$prim57520 = alloca %struct.ScmObj*, align 8
%current_45args56302 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56301)
store volatile %struct.ScmObj* %current_45args56302, %struct.ScmObj** %stackaddr$prim57520, align 8
%stackaddr$prim57521 = alloca %struct.ScmObj*, align 8
%anf_45bind48229 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56302)
store volatile %struct.ScmObj* %anf_45bind48229, %struct.ScmObj** %stackaddr$prim57521, align 8
%ae49911 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57522 = alloca %struct.ScmObj*, align 8
%anf_45bind48230 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48139, %struct.ScmObj* %ae49911)
store volatile %struct.ScmObj* %anf_45bind48230, %struct.ScmObj** %stackaddr$prim57522, align 8
%stackaddr$makeclosure57523 = alloca %struct.ScmObj*, align 8
%fptrToInt57524 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49913 to i64
%ae49913 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57524)
store volatile %struct.ScmObj* %ae49913, %struct.ScmObj** %stackaddr$makeclosure57523, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49913, %struct.ScmObj* %_37foldl48136, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49913, %struct.ScmObj* %k48374, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49913, %struct.ScmObj* %lsts_4348145, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49913, %struct.ScmObj* %f48140, i64 3)
%argslist56310$_37foldr480580 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57525 = alloca %struct.ScmObj*, align 8
%argslist56310$_37foldr480581 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs48143, %struct.ScmObj* %argslist56310$_37foldr480580)
store volatile %struct.ScmObj* %argslist56310$_37foldr480581, %struct.ScmObj** %stackaddr$prim57525, align 8
%stackaddr$prim57526 = alloca %struct.ScmObj*, align 8
%argslist56310$_37foldr480582 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48230, %struct.ScmObj* %argslist56310$_37foldr480581)
store volatile %struct.ScmObj* %argslist56310$_37foldr480582, %struct.ScmObj** %stackaddr$prim57526, align 8
%stackaddr$prim57527 = alloca %struct.ScmObj*, align 8
%argslist56310$_37foldr480583 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48229, %struct.ScmObj* %argslist56310$_37foldr480582)
store volatile %struct.ScmObj* %argslist56310$_37foldr480583, %struct.ScmObj** %stackaddr$prim57527, align 8
%stackaddr$prim57528 = alloca %struct.ScmObj*, align 8
%argslist56310$_37foldr480584 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49913, %struct.ScmObj* %argslist56310$_37foldr480583)
store volatile %struct.ScmObj* %argslist56310$_37foldr480584, %struct.ScmObj** %stackaddr$prim57528, align 8
%clofunc57529 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr48058)
musttail call tailcc void %clofunc57529(%struct.ScmObj* %_37foldr48058, %struct.ScmObj* %argslist56310$_37foldr480584)
ret void
}

define tailcc void @proc_clo$ae49913(%struct.ScmObj* %env$ae49913,%struct.ScmObj* %current_45args56304) {
%stackaddr$env-ref57530 = alloca %struct.ScmObj*, align 8
%_37foldl48136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49913, i64 0)
store %struct.ScmObj* %_37foldl48136, %struct.ScmObj** %stackaddr$env-ref57530
%stackaddr$env-ref57531 = alloca %struct.ScmObj*, align 8
%k48374 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49913, i64 1)
store %struct.ScmObj* %k48374, %struct.ScmObj** %stackaddr$env-ref57531
%stackaddr$env-ref57532 = alloca %struct.ScmObj*, align 8
%lsts_4348145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49913, i64 2)
store %struct.ScmObj* %lsts_4348145, %struct.ScmObj** %stackaddr$env-ref57532
%stackaddr$env-ref57533 = alloca %struct.ScmObj*, align 8
%f48140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49913, i64 3)
store %struct.ScmObj* %f48140, %struct.ScmObj** %stackaddr$env-ref57533
%stackaddr$prim57534 = alloca %struct.ScmObj*, align 8
%_95k48382 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56304)
store volatile %struct.ScmObj* %_95k48382, %struct.ScmObj** %stackaddr$prim57534, align 8
%stackaddr$prim57535 = alloca %struct.ScmObj*, align 8
%current_45args56305 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56304)
store volatile %struct.ScmObj* %current_45args56305, %struct.ScmObj** %stackaddr$prim57535, align 8
%stackaddr$prim57536 = alloca %struct.ScmObj*, align 8
%anf_45bind48231 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56305)
store volatile %struct.ScmObj* %anf_45bind48231, %struct.ScmObj** %stackaddr$prim57536, align 8
%stackaddr$makeclosure57537 = alloca %struct.ScmObj*, align 8
%fptrToInt57538 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49917 to i64
%ae49917 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57538)
store volatile %struct.ScmObj* %ae49917, %struct.ScmObj** %stackaddr$makeclosure57537, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49917, %struct.ScmObj* %_37foldl48136, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49917, %struct.ScmObj* %k48374, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49917, %struct.ScmObj* %lsts_4348145, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49917, %struct.ScmObj* %f48140, i64 3)
%stackaddr$prim57539 = alloca %struct.ScmObj*, align 8
%cpsargs48385 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49917, %struct.ScmObj* %anf_45bind48231)
store volatile %struct.ScmObj* %cpsargs48385, %struct.ScmObj** %stackaddr$prim57539, align 8
%clofunc57540 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48140)
musttail call tailcc void %clofunc57540(%struct.ScmObj* %f48140, %struct.ScmObj* %cpsargs48385)
ret void
}

define tailcc void @proc_clo$ae49917(%struct.ScmObj* %env$ae49917,%struct.ScmObj* %current_45args56307) {
%stackaddr$env-ref57541 = alloca %struct.ScmObj*, align 8
%_37foldl48136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49917, i64 0)
store %struct.ScmObj* %_37foldl48136, %struct.ScmObj** %stackaddr$env-ref57541
%stackaddr$env-ref57542 = alloca %struct.ScmObj*, align 8
%k48374 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49917, i64 1)
store %struct.ScmObj* %k48374, %struct.ScmObj** %stackaddr$env-ref57542
%stackaddr$env-ref57543 = alloca %struct.ScmObj*, align 8
%lsts_4348145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49917, i64 2)
store %struct.ScmObj* %lsts_4348145, %struct.ScmObj** %stackaddr$env-ref57543
%stackaddr$env-ref57544 = alloca %struct.ScmObj*, align 8
%f48140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49917, i64 3)
store %struct.ScmObj* %f48140, %struct.ScmObj** %stackaddr$env-ref57544
%stackaddr$prim57545 = alloca %struct.ScmObj*, align 8
%_95k48383 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56307)
store volatile %struct.ScmObj* %_95k48383, %struct.ScmObj** %stackaddr$prim57545, align 8
%stackaddr$prim57546 = alloca %struct.ScmObj*, align 8
%current_45args56308 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56307)
store volatile %struct.ScmObj* %current_45args56308, %struct.ScmObj** %stackaddr$prim57546, align 8
%stackaddr$prim57547 = alloca %struct.ScmObj*, align 8
%acc_4348147 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56308)
store volatile %struct.ScmObj* %acc_4348147, %struct.ScmObj** %stackaddr$prim57547, align 8
%stackaddr$prim57548 = alloca %struct.ScmObj*, align 8
%anf_45bind48232 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4348147, %struct.ScmObj* %lsts_4348145)
store volatile %struct.ScmObj* %anf_45bind48232, %struct.ScmObj** %stackaddr$prim57548, align 8
%stackaddr$prim57549 = alloca %struct.ScmObj*, align 8
%anf_45bind48233 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48140, %struct.ScmObj* %anf_45bind48232)
store volatile %struct.ScmObj* %anf_45bind48233, %struct.ScmObj** %stackaddr$prim57549, align 8
%stackaddr$prim57550 = alloca %struct.ScmObj*, align 8
%cpsargs48384 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48374, %struct.ScmObj* %anf_45bind48233)
store volatile %struct.ScmObj* %cpsargs48384, %struct.ScmObj** %stackaddr$prim57550, align 8
%clofunc57551 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl48136)
musttail call tailcc void %clofunc57551(%struct.ScmObj* %_37foldl48136, %struct.ScmObj* %cpsargs48384)
ret void
}

define tailcc void @proc_clo$ae49890(%struct.ScmObj* %env$ae49890,%struct.ScmObj* %current_45args56311) {
%stackaddr$prim57552 = alloca %struct.ScmObj*, align 8
%k48386 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56311)
store volatile %struct.ScmObj* %k48386, %struct.ScmObj** %stackaddr$prim57552, align 8
%stackaddr$prim57553 = alloca %struct.ScmObj*, align 8
%current_45args56312 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56311)
store volatile %struct.ScmObj* %current_45args56312, %struct.ScmObj** %stackaddr$prim57553, align 8
%stackaddr$prim57554 = alloca %struct.ScmObj*, align 8
%a48149 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56312)
store volatile %struct.ScmObj* %a48149, %struct.ScmObj** %stackaddr$prim57554, align 8
%stackaddr$prim57555 = alloca %struct.ScmObj*, align 8
%current_45args56313 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56312)
store volatile %struct.ScmObj* %current_45args56313, %struct.ScmObj** %stackaddr$prim57555, align 8
%stackaddr$prim57556 = alloca %struct.ScmObj*, align 8
%b48148 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56313)
store volatile %struct.ScmObj* %b48148, %struct.ScmObj** %stackaddr$prim57556, align 8
%stackaddr$prim57557 = alloca %struct.ScmObj*, align 8
%cpsprim48387 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a48149, %struct.ScmObj* %b48148)
store volatile %struct.ScmObj* %cpsprim48387, %struct.ScmObj** %stackaddr$prim57557, align 8
%ae49894 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56315$k483860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57558 = alloca %struct.ScmObj*, align 8
%argslist56315$k483861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48387, %struct.ScmObj* %argslist56315$k483860)
store volatile %struct.ScmObj* %argslist56315$k483861, %struct.ScmObj** %stackaddr$prim57558, align 8
%stackaddr$prim57559 = alloca %struct.ScmObj*, align 8
%argslist56315$k483862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49894, %struct.ScmObj* %argslist56315$k483861)
store volatile %struct.ScmObj* %argslist56315$k483862, %struct.ScmObj** %stackaddr$prim57559, align 8
%clofunc57560 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48386)
musttail call tailcc void %clofunc57560(%struct.ScmObj* %k48386, %struct.ScmObj* %argslist56315$k483862)
ret void
}

define tailcc void @proc_clo$ae49866(%struct.ScmObj* %env$ae49866,%struct.ScmObj* %current_45args56318) {
%stackaddr$prim57561 = alloca %struct.ScmObj*, align 8
%k48388 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56318)
store volatile %struct.ScmObj* %k48388, %struct.ScmObj** %stackaddr$prim57561, align 8
%stackaddr$prim57562 = alloca %struct.ScmObj*, align 8
%current_45args56319 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56318)
store volatile %struct.ScmObj* %current_45args56319, %struct.ScmObj** %stackaddr$prim57562, align 8
%stackaddr$prim57563 = alloca %struct.ScmObj*, align 8
%x48144 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56319)
store volatile %struct.ScmObj* %x48144, %struct.ScmObj** %stackaddr$prim57563, align 8
%stackaddr$prim57564 = alloca %struct.ScmObj*, align 8
%cpsprim48389 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48144)
store volatile %struct.ScmObj* %cpsprim48389, %struct.ScmObj** %stackaddr$prim57564, align 8
%ae49869 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56321$k483880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57565 = alloca %struct.ScmObj*, align 8
%argslist56321$k483881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48389, %struct.ScmObj* %argslist56321$k483880)
store volatile %struct.ScmObj* %argslist56321$k483881, %struct.ScmObj** %stackaddr$prim57565, align 8
%stackaddr$prim57566 = alloca %struct.ScmObj*, align 8
%argslist56321$k483882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49869, %struct.ScmObj* %argslist56321$k483881)
store volatile %struct.ScmObj* %argslist56321$k483882, %struct.ScmObj** %stackaddr$prim57566, align 8
%clofunc57567 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48388)
musttail call tailcc void %clofunc57567(%struct.ScmObj* %k48388, %struct.ScmObj* %argslist56321$k483882)
ret void
}

define tailcc void @proc_clo$ae49842(%struct.ScmObj* %env$ae49842,%struct.ScmObj* %current_45args56324) {
%stackaddr$prim57568 = alloca %struct.ScmObj*, align 8
%k48390 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56324)
store volatile %struct.ScmObj* %k48390, %struct.ScmObj** %stackaddr$prim57568, align 8
%stackaddr$prim57569 = alloca %struct.ScmObj*, align 8
%current_45args56325 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56324)
store volatile %struct.ScmObj* %current_45args56325, %struct.ScmObj** %stackaddr$prim57569, align 8
%stackaddr$prim57570 = alloca %struct.ScmObj*, align 8
%x48146 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56325)
store volatile %struct.ScmObj* %x48146, %struct.ScmObj** %stackaddr$prim57570, align 8
%stackaddr$prim57571 = alloca %struct.ScmObj*, align 8
%cpsprim48391 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48146)
store volatile %struct.ScmObj* %cpsprim48391, %struct.ScmObj** %stackaddr$prim57571, align 8
%ae49845 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56327$k483900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57572 = alloca %struct.ScmObj*, align 8
%argslist56327$k483901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48391, %struct.ScmObj* %argslist56327$k483900)
store volatile %struct.ScmObj* %argslist56327$k483901, %struct.ScmObj** %stackaddr$prim57572, align 8
%stackaddr$prim57573 = alloca %struct.ScmObj*, align 8
%argslist56327$k483902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49845, %struct.ScmObj* %argslist56327$k483901)
store volatile %struct.ScmObj* %argslist56327$k483902, %struct.ScmObj** %stackaddr$prim57573, align 8
%clofunc57574 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48390)
musttail call tailcc void %clofunc57574(%struct.ScmObj* %k48390, %struct.ScmObj* %argslist56327$k483902)
ret void
}

define tailcc void @proc_clo$ae49794(%struct.ScmObj* %env$ae49794,%struct.ScmObj* %current_45args56330) {
%stackaddr$prim57575 = alloca %struct.ScmObj*, align 8
%k48392 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56330)
store volatile %struct.ScmObj* %k48392, %struct.ScmObj** %stackaddr$prim57575, align 8
%stackaddr$prim57576 = alloca %struct.ScmObj*, align 8
%current_45args56331 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56330)
store volatile %struct.ScmObj* %current_45args56331, %struct.ScmObj** %stackaddr$prim57576, align 8
%stackaddr$prim57577 = alloca %struct.ScmObj*, align 8
%lst48142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56331)
store volatile %struct.ScmObj* %lst48142, %struct.ScmObj** %stackaddr$prim57577, align 8
%stackaddr$prim57578 = alloca %struct.ScmObj*, align 8
%current_45args56332 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56331)
store volatile %struct.ScmObj* %current_45args56332, %struct.ScmObj** %stackaddr$prim57578, align 8
%stackaddr$prim57579 = alloca %struct.ScmObj*, align 8
%b48141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56332)
store volatile %struct.ScmObj* %b48141, %struct.ScmObj** %stackaddr$prim57579, align 8
%truthy$cmp57580 = call i64 @is_truthy_value(%struct.ScmObj* %b48141)
%cmp$cmp57580 = icmp eq i64 %truthy$cmp57580, 1
br i1 %cmp$cmp57580, label %truebranch$cmp57580, label %falsebranch$cmp57580
truebranch$cmp57580:
%ae49797 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56334$k483920 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57581 = alloca %struct.ScmObj*, align 8
%argslist56334$k483921 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b48141, %struct.ScmObj* %argslist56334$k483920)
store volatile %struct.ScmObj* %argslist56334$k483921, %struct.ScmObj** %stackaddr$prim57581, align 8
%stackaddr$prim57582 = alloca %struct.ScmObj*, align 8
%argslist56334$k483922 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49797, %struct.ScmObj* %argslist56334$k483921)
store volatile %struct.ScmObj* %argslist56334$k483922, %struct.ScmObj** %stackaddr$prim57582, align 8
%clofunc57583 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48392)
musttail call tailcc void %clofunc57583(%struct.ScmObj* %k48392, %struct.ScmObj* %argslist56334$k483922)
ret void
falsebranch$cmp57580:
%stackaddr$prim57584 = alloca %struct.ScmObj*, align 8
%cpsprim48393 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48142)
store volatile %struct.ScmObj* %cpsprim48393, %struct.ScmObj** %stackaddr$prim57584, align 8
%ae49804 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56335$k483920 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57585 = alloca %struct.ScmObj*, align 8
%argslist56335$k483921 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48393, %struct.ScmObj* %argslist56335$k483920)
store volatile %struct.ScmObj* %argslist56335$k483921, %struct.ScmObj** %stackaddr$prim57585, align 8
%stackaddr$prim57586 = alloca %struct.ScmObj*, align 8
%argslist56335$k483922 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49804, %struct.ScmObj* %argslist56335$k483921)
store volatile %struct.ScmObj* %argslist56335$k483922, %struct.ScmObj** %stackaddr$prim57586, align 8
%clofunc57587 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48392)
musttail call tailcc void %clofunc57587(%struct.ScmObj* %k48392, %struct.ScmObj* %argslist56335$k483922)
ret void
}

define tailcc void @proc_clo$ae49635(%struct.ScmObj* %env$ae49635,%struct.ScmObj* %args4808048394) {
%stackaddr$env-ref57588 = alloca %struct.ScmObj*, align 8
%_37drop_45right48072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49635, i64 0)
store %struct.ScmObj* %_37drop_45right48072, %struct.ScmObj** %stackaddr$env-ref57588
%stackaddr$env-ref57589 = alloca %struct.ScmObj*, align 8
%_37last48075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49635, i64 1)
store %struct.ScmObj* %_37last48075, %struct.ScmObj** %stackaddr$env-ref57589
%stackaddr$env-ref57590 = alloca %struct.ScmObj*, align 8
%_37foldr48058 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49635, i64 2)
store %struct.ScmObj* %_37foldr48058, %struct.ScmObj** %stackaddr$env-ref57590
%stackaddr$prim57591 = alloca %struct.ScmObj*, align 8
%k48395 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4808048394)
store volatile %struct.ScmObj* %k48395, %struct.ScmObj** %stackaddr$prim57591, align 8
%stackaddr$prim57592 = alloca %struct.ScmObj*, align 8
%args48080 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4808048394)
store volatile %struct.ScmObj* %args48080, %struct.ScmObj** %stackaddr$prim57592, align 8
%stackaddr$prim57593 = alloca %struct.ScmObj*, align 8
%f48082 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48080)
store volatile %struct.ScmObj* %f48082, %struct.ScmObj** %stackaddr$prim57593, align 8
%stackaddr$prim57594 = alloca %struct.ScmObj*, align 8
%lsts48081 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48080)
store volatile %struct.ScmObj* %lsts48081, %struct.ScmObj** %stackaddr$prim57594, align 8
%stackaddr$makeclosure57595 = alloca %struct.ScmObj*, align 8
%fptrToInt57596 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49640 to i64
%ae49640 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57596)
store volatile %struct.ScmObj* %ae49640, %struct.ScmObj** %stackaddr$makeclosure57595, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49640, %struct.ScmObj* %lsts48081, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49640, %struct.ScmObj* %k48395, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49640, %struct.ScmObj* %_37foldr48058, i64 2)
%ae49641 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57597 = alloca %struct.ScmObj*, align 8
%fptrToInt57598 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49642 to i64
%ae49642 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57598)
store volatile %struct.ScmObj* %ae49642, %struct.ScmObj** %stackaddr$makeclosure57597, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49642, %struct.ScmObj* %_37drop_45right48072, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49642, %struct.ScmObj* %f48082, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49642, %struct.ScmObj* %_37last48075, i64 2)
%argslist56354$ae496400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57599 = alloca %struct.ScmObj*, align 8
%argslist56354$ae496401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49642, %struct.ScmObj* %argslist56354$ae496400)
store volatile %struct.ScmObj* %argslist56354$ae496401, %struct.ScmObj** %stackaddr$prim57599, align 8
%stackaddr$prim57600 = alloca %struct.ScmObj*, align 8
%argslist56354$ae496402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49641, %struct.ScmObj* %argslist56354$ae496401)
store volatile %struct.ScmObj* %argslist56354$ae496402, %struct.ScmObj** %stackaddr$prim57600, align 8
%clofunc57601 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49640)
musttail call tailcc void %clofunc57601(%struct.ScmObj* %ae49640, %struct.ScmObj* %argslist56354$ae496402)
ret void
}

define tailcc void @proc_clo$ae49640(%struct.ScmObj* %env$ae49640,%struct.ScmObj* %current_45args56339) {
%stackaddr$env-ref57602 = alloca %struct.ScmObj*, align 8
%lsts48081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49640, i64 0)
store %struct.ScmObj* %lsts48081, %struct.ScmObj** %stackaddr$env-ref57602
%stackaddr$env-ref57603 = alloca %struct.ScmObj*, align 8
%k48395 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49640, i64 1)
store %struct.ScmObj* %k48395, %struct.ScmObj** %stackaddr$env-ref57603
%stackaddr$env-ref57604 = alloca %struct.ScmObj*, align 8
%_37foldr48058 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49640, i64 2)
store %struct.ScmObj* %_37foldr48058, %struct.ScmObj** %stackaddr$env-ref57604
%stackaddr$prim57605 = alloca %struct.ScmObj*, align 8
%_95k48396 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56339)
store volatile %struct.ScmObj* %_95k48396, %struct.ScmObj** %stackaddr$prim57605, align 8
%stackaddr$prim57606 = alloca %struct.ScmObj*, align 8
%current_45args56340 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56339)
store volatile %struct.ScmObj* %current_45args56340, %struct.ScmObj** %stackaddr$prim57606, align 8
%stackaddr$prim57607 = alloca %struct.ScmObj*, align 8
%anf_45bind48220 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56340)
store volatile %struct.ScmObj* %anf_45bind48220, %struct.ScmObj** %stackaddr$prim57607, align 8
%ae49703 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57608 = alloca %struct.ScmObj*, align 8
%anf_45bind48221 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49703, %struct.ScmObj* %lsts48081)
store volatile %struct.ScmObj* %anf_45bind48221, %struct.ScmObj** %stackaddr$prim57608, align 8
%stackaddr$prim57609 = alloca %struct.ScmObj*, align 8
%anf_45bind48222 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48220, %struct.ScmObj* %anf_45bind48221)
store volatile %struct.ScmObj* %anf_45bind48222, %struct.ScmObj** %stackaddr$prim57609, align 8
%stackaddr$prim57610 = alloca %struct.ScmObj*, align 8
%cpsargs48397 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48395, %struct.ScmObj* %anf_45bind48222)
store volatile %struct.ScmObj* %cpsargs48397, %struct.ScmObj** %stackaddr$prim57610, align 8
%clofunc57611 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr48058)
musttail call tailcc void %clofunc57611(%struct.ScmObj* %_37foldr48058, %struct.ScmObj* %cpsargs48397)
ret void
}

define tailcc void @proc_clo$ae49642(%struct.ScmObj* %env$ae49642,%struct.ScmObj* %fargs4808348398) {
%stackaddr$env-ref57612 = alloca %struct.ScmObj*, align 8
%_37drop_45right48072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49642, i64 0)
store %struct.ScmObj* %_37drop_45right48072, %struct.ScmObj** %stackaddr$env-ref57612
%stackaddr$env-ref57613 = alloca %struct.ScmObj*, align 8
%f48082 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49642, i64 1)
store %struct.ScmObj* %f48082, %struct.ScmObj** %stackaddr$env-ref57613
%stackaddr$env-ref57614 = alloca %struct.ScmObj*, align 8
%_37last48075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49642, i64 2)
store %struct.ScmObj* %_37last48075, %struct.ScmObj** %stackaddr$env-ref57614
%stackaddr$prim57615 = alloca %struct.ScmObj*, align 8
%k48399 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs4808348398)
store volatile %struct.ScmObj* %k48399, %struct.ScmObj** %stackaddr$prim57615, align 8
%stackaddr$prim57616 = alloca %struct.ScmObj*, align 8
%fargs48083 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs4808348398)
store volatile %struct.ScmObj* %fargs48083, %struct.ScmObj** %stackaddr$prim57616, align 8
%stackaddr$makeclosure57617 = alloca %struct.ScmObj*, align 8
%fptrToInt57618 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49646 to i64
%ae49646 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57618)
store volatile %struct.ScmObj* %ae49646, %struct.ScmObj** %stackaddr$makeclosure57617, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49646, %struct.ScmObj* %fargs48083, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49646, %struct.ScmObj* %f48082, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49646, %struct.ScmObj* %k48399, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49646, %struct.ScmObj* %_37last48075, i64 3)
%ae49648 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist56353$_37drop_45right480720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57619 = alloca %struct.ScmObj*, align 8
%argslist56353$_37drop_45right480721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49648, %struct.ScmObj* %argslist56353$_37drop_45right480720)
store volatile %struct.ScmObj* %argslist56353$_37drop_45right480721, %struct.ScmObj** %stackaddr$prim57619, align 8
%stackaddr$prim57620 = alloca %struct.ScmObj*, align 8
%argslist56353$_37drop_45right480722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs48083, %struct.ScmObj* %argslist56353$_37drop_45right480721)
store volatile %struct.ScmObj* %argslist56353$_37drop_45right480722, %struct.ScmObj** %stackaddr$prim57620, align 8
%stackaddr$prim57621 = alloca %struct.ScmObj*, align 8
%argslist56353$_37drop_45right480723 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49646, %struct.ScmObj* %argslist56353$_37drop_45right480722)
store volatile %struct.ScmObj* %argslist56353$_37drop_45right480723, %struct.ScmObj** %stackaddr$prim57621, align 8
%clofunc57622 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right48072)
musttail call tailcc void %clofunc57622(%struct.ScmObj* %_37drop_45right48072, %struct.ScmObj* %argslist56353$_37drop_45right480723)
ret void
}

define tailcc void @proc_clo$ae49646(%struct.ScmObj* %env$ae49646,%struct.ScmObj* %current_45args56342) {
%stackaddr$env-ref57623 = alloca %struct.ScmObj*, align 8
%fargs48083 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49646, i64 0)
store %struct.ScmObj* %fargs48083, %struct.ScmObj** %stackaddr$env-ref57623
%stackaddr$env-ref57624 = alloca %struct.ScmObj*, align 8
%f48082 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49646, i64 1)
store %struct.ScmObj* %f48082, %struct.ScmObj** %stackaddr$env-ref57624
%stackaddr$env-ref57625 = alloca %struct.ScmObj*, align 8
%k48399 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49646, i64 2)
store %struct.ScmObj* %k48399, %struct.ScmObj** %stackaddr$env-ref57625
%stackaddr$env-ref57626 = alloca %struct.ScmObj*, align 8
%_37last48075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49646, i64 3)
store %struct.ScmObj* %_37last48075, %struct.ScmObj** %stackaddr$env-ref57626
%stackaddr$prim57627 = alloca %struct.ScmObj*, align 8
%_95k48400 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56342)
store volatile %struct.ScmObj* %_95k48400, %struct.ScmObj** %stackaddr$prim57627, align 8
%stackaddr$prim57628 = alloca %struct.ScmObj*, align 8
%current_45args56343 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56342)
store volatile %struct.ScmObj* %current_45args56343, %struct.ScmObj** %stackaddr$prim57628, align 8
%stackaddr$prim57629 = alloca %struct.ScmObj*, align 8
%anf_45bind48217 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56343)
store volatile %struct.ScmObj* %anf_45bind48217, %struct.ScmObj** %stackaddr$prim57629, align 8
%stackaddr$makeclosure57630 = alloca %struct.ScmObj*, align 8
%fptrToInt57631 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49653 to i64
%ae49653 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57631)
store volatile %struct.ScmObj* %ae49653, %struct.ScmObj** %stackaddr$makeclosure57630, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49653, %struct.ScmObj* %fargs48083, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49653, %struct.ScmObj* %k48399, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49653, %struct.ScmObj* %_37last48075, i64 2)
%stackaddr$prim57632 = alloca %struct.ScmObj*, align 8
%cpsargs48404 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49653, %struct.ScmObj* %anf_45bind48217)
store volatile %struct.ScmObj* %cpsargs48404, %struct.ScmObj** %stackaddr$prim57632, align 8
%clofunc57633 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48082)
musttail call tailcc void %clofunc57633(%struct.ScmObj* %f48082, %struct.ScmObj* %cpsargs48404)
ret void
}

define tailcc void @proc_clo$ae49653(%struct.ScmObj* %env$ae49653,%struct.ScmObj* %current_45args56345) {
%stackaddr$env-ref57634 = alloca %struct.ScmObj*, align 8
%fargs48083 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49653, i64 0)
store %struct.ScmObj* %fargs48083, %struct.ScmObj** %stackaddr$env-ref57634
%stackaddr$env-ref57635 = alloca %struct.ScmObj*, align 8
%k48399 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49653, i64 1)
store %struct.ScmObj* %k48399, %struct.ScmObj** %stackaddr$env-ref57635
%stackaddr$env-ref57636 = alloca %struct.ScmObj*, align 8
%_37last48075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49653, i64 2)
store %struct.ScmObj* %_37last48075, %struct.ScmObj** %stackaddr$env-ref57636
%stackaddr$prim57637 = alloca %struct.ScmObj*, align 8
%_95k48401 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56345)
store volatile %struct.ScmObj* %_95k48401, %struct.ScmObj** %stackaddr$prim57637, align 8
%stackaddr$prim57638 = alloca %struct.ScmObj*, align 8
%current_45args56346 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56345)
store volatile %struct.ScmObj* %current_45args56346, %struct.ScmObj** %stackaddr$prim57638, align 8
%stackaddr$prim57639 = alloca %struct.ScmObj*, align 8
%anf_45bind48218 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56346)
store volatile %struct.ScmObj* %anf_45bind48218, %struct.ScmObj** %stackaddr$prim57639, align 8
%stackaddr$makeclosure57640 = alloca %struct.ScmObj*, align 8
%fptrToInt57641 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49658 to i64
%ae49658 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57641)
store volatile %struct.ScmObj* %ae49658, %struct.ScmObj** %stackaddr$makeclosure57640, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49658, %struct.ScmObj* %k48399, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49658, %struct.ScmObj* %anf_45bind48218, i64 1)
%argslist56352$_37last480750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57642 = alloca %struct.ScmObj*, align 8
%argslist56352$_37last480751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs48083, %struct.ScmObj* %argslist56352$_37last480750)
store volatile %struct.ScmObj* %argslist56352$_37last480751, %struct.ScmObj** %stackaddr$prim57642, align 8
%stackaddr$prim57643 = alloca %struct.ScmObj*, align 8
%argslist56352$_37last480752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49658, %struct.ScmObj* %argslist56352$_37last480751)
store volatile %struct.ScmObj* %argslist56352$_37last480752, %struct.ScmObj** %stackaddr$prim57643, align 8
%clofunc57644 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last48075)
musttail call tailcc void %clofunc57644(%struct.ScmObj* %_37last48075, %struct.ScmObj* %argslist56352$_37last480752)
ret void
}

define tailcc void @proc_clo$ae49658(%struct.ScmObj* %env$ae49658,%struct.ScmObj* %current_45args56348) {
%stackaddr$env-ref57645 = alloca %struct.ScmObj*, align 8
%k48399 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49658, i64 0)
store %struct.ScmObj* %k48399, %struct.ScmObj** %stackaddr$env-ref57645
%stackaddr$env-ref57646 = alloca %struct.ScmObj*, align 8
%anf_45bind48218 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49658, i64 1)
store %struct.ScmObj* %anf_45bind48218, %struct.ScmObj** %stackaddr$env-ref57646
%stackaddr$prim57647 = alloca %struct.ScmObj*, align 8
%_95k48402 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56348)
store volatile %struct.ScmObj* %_95k48402, %struct.ScmObj** %stackaddr$prim57647, align 8
%stackaddr$prim57648 = alloca %struct.ScmObj*, align 8
%current_45args56349 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56348)
store volatile %struct.ScmObj* %current_45args56349, %struct.ScmObj** %stackaddr$prim57648, align 8
%stackaddr$prim57649 = alloca %struct.ScmObj*, align 8
%anf_45bind48219 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56349)
store volatile %struct.ScmObj* %anf_45bind48219, %struct.ScmObj** %stackaddr$prim57649, align 8
%stackaddr$prim57650 = alloca %struct.ScmObj*, align 8
%cpsprim48403 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48218, %struct.ScmObj* %anf_45bind48219)
store volatile %struct.ScmObj* %cpsprim48403, %struct.ScmObj** %stackaddr$prim57650, align 8
%ae49663 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56351$k483990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57651 = alloca %struct.ScmObj*, align 8
%argslist56351$k483991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48403, %struct.ScmObj* %argslist56351$k483990)
store volatile %struct.ScmObj* %argslist56351$k483991, %struct.ScmObj** %stackaddr$prim57651, align 8
%stackaddr$prim57652 = alloca %struct.ScmObj*, align 8
%argslist56351$k483992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49663, %struct.ScmObj* %argslist56351$k483991)
store volatile %struct.ScmObj* %argslist56351$k483992, %struct.ScmObj** %stackaddr$prim57652, align 8
%clofunc57653 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48399)
musttail call tailcc void %clofunc57653(%struct.ScmObj* %k48399, %struct.ScmObj* %argslist56351$k483992)
ret void
}

define tailcc void @proc_clo$ae49558(%struct.ScmObj* %env$ae49558,%struct.ScmObj* %current_45args56356) {
%stackaddr$env-ref57654 = alloca %struct.ScmObj*, align 8
%_37foldr148053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49558, i64 0)
store %struct.ScmObj* %_37foldr148053, %struct.ScmObj** %stackaddr$env-ref57654
%stackaddr$prim57655 = alloca %struct.ScmObj*, align 8
%k48405 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56356)
store volatile %struct.ScmObj* %k48405, %struct.ScmObj** %stackaddr$prim57655, align 8
%stackaddr$prim57656 = alloca %struct.ScmObj*, align 8
%current_45args56357 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56356)
store volatile %struct.ScmObj* %current_45args56357, %struct.ScmObj** %stackaddr$prim57656, align 8
%stackaddr$prim57657 = alloca %struct.ScmObj*, align 8
%f48086 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56357)
store volatile %struct.ScmObj* %f48086, %struct.ScmObj** %stackaddr$prim57657, align 8
%stackaddr$prim57658 = alloca %struct.ScmObj*, align 8
%current_45args56358 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56357)
store volatile %struct.ScmObj* %current_45args56358, %struct.ScmObj** %stackaddr$prim57658, align 8
%stackaddr$prim57659 = alloca %struct.ScmObj*, align 8
%lst48085 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56358)
store volatile %struct.ScmObj* %lst48085, %struct.ScmObj** %stackaddr$prim57659, align 8
%stackaddr$makeclosure57660 = alloca %struct.ScmObj*, align 8
%fptrToInt57661 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49559 to i64
%ae49559 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57661)
store volatile %struct.ScmObj* %ae49559, %struct.ScmObj** %stackaddr$makeclosure57660, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49559, %struct.ScmObj* %k48405, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49559, %struct.ScmObj* %lst48085, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49559, %struct.ScmObj* %_37foldr148053, i64 2)
%ae49560 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57662 = alloca %struct.ScmObj*, align 8
%fptrToInt57663 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49561 to i64
%ae49561 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57663)
store volatile %struct.ScmObj* %ae49561, %struct.ScmObj** %stackaddr$makeclosure57662, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49561, %struct.ScmObj* %f48086, i64 0)
%argslist56373$ae495590 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57664 = alloca %struct.ScmObj*, align 8
%argslist56373$ae495591 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49561, %struct.ScmObj* %argslist56373$ae495590)
store volatile %struct.ScmObj* %argslist56373$ae495591, %struct.ScmObj** %stackaddr$prim57664, align 8
%stackaddr$prim57665 = alloca %struct.ScmObj*, align 8
%argslist56373$ae495592 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49560, %struct.ScmObj* %argslist56373$ae495591)
store volatile %struct.ScmObj* %argslist56373$ae495592, %struct.ScmObj** %stackaddr$prim57665, align 8
%clofunc57666 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49559)
musttail call tailcc void %clofunc57666(%struct.ScmObj* %ae49559, %struct.ScmObj* %argslist56373$ae495592)
ret void
}

define tailcc void @proc_clo$ae49559(%struct.ScmObj* %env$ae49559,%struct.ScmObj* %current_45args56360) {
%stackaddr$env-ref57667 = alloca %struct.ScmObj*, align 8
%k48405 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49559, i64 0)
store %struct.ScmObj* %k48405, %struct.ScmObj** %stackaddr$env-ref57667
%stackaddr$env-ref57668 = alloca %struct.ScmObj*, align 8
%lst48085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49559, i64 1)
store %struct.ScmObj* %lst48085, %struct.ScmObj** %stackaddr$env-ref57668
%stackaddr$env-ref57669 = alloca %struct.ScmObj*, align 8
%_37foldr148053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49559, i64 2)
store %struct.ScmObj* %_37foldr148053, %struct.ScmObj** %stackaddr$env-ref57669
%stackaddr$prim57670 = alloca %struct.ScmObj*, align 8
%_95k48406 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56360)
store volatile %struct.ScmObj* %_95k48406, %struct.ScmObj** %stackaddr$prim57670, align 8
%stackaddr$prim57671 = alloca %struct.ScmObj*, align 8
%current_45args56361 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56360)
store volatile %struct.ScmObj* %current_45args56361, %struct.ScmObj** %stackaddr$prim57671, align 8
%stackaddr$prim57672 = alloca %struct.ScmObj*, align 8
%anf_45bind48216 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56361)
store volatile %struct.ScmObj* %anf_45bind48216, %struct.ScmObj** %stackaddr$prim57672, align 8
%ae49593 = call %struct.ScmObj* @const_init_null()
%argslist56363$_37foldr1480530 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57673 = alloca %struct.ScmObj*, align 8
%argslist56363$_37foldr1480531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48085, %struct.ScmObj* %argslist56363$_37foldr1480530)
store volatile %struct.ScmObj* %argslist56363$_37foldr1480531, %struct.ScmObj** %stackaddr$prim57673, align 8
%stackaddr$prim57674 = alloca %struct.ScmObj*, align 8
%argslist56363$_37foldr1480532 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49593, %struct.ScmObj* %argslist56363$_37foldr1480531)
store volatile %struct.ScmObj* %argslist56363$_37foldr1480532, %struct.ScmObj** %stackaddr$prim57674, align 8
%stackaddr$prim57675 = alloca %struct.ScmObj*, align 8
%argslist56363$_37foldr1480533 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48216, %struct.ScmObj* %argslist56363$_37foldr1480532)
store volatile %struct.ScmObj* %argslist56363$_37foldr1480533, %struct.ScmObj** %stackaddr$prim57675, align 8
%stackaddr$prim57676 = alloca %struct.ScmObj*, align 8
%argslist56363$_37foldr1480534 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48405, %struct.ScmObj* %argslist56363$_37foldr1480533)
store volatile %struct.ScmObj* %argslist56363$_37foldr1480534, %struct.ScmObj** %stackaddr$prim57676, align 8
%clofunc57677 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148053)
musttail call tailcc void %clofunc57677(%struct.ScmObj* %_37foldr148053, %struct.ScmObj* %argslist56363$_37foldr1480534)
ret void
}

define tailcc void @proc_clo$ae49561(%struct.ScmObj* %env$ae49561,%struct.ScmObj* %current_45args56364) {
%stackaddr$env-ref57678 = alloca %struct.ScmObj*, align 8
%f48086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49561, i64 0)
store %struct.ScmObj* %f48086, %struct.ScmObj** %stackaddr$env-ref57678
%stackaddr$prim57679 = alloca %struct.ScmObj*, align 8
%k48407 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56364)
store volatile %struct.ScmObj* %k48407, %struct.ScmObj** %stackaddr$prim57679, align 8
%stackaddr$prim57680 = alloca %struct.ScmObj*, align 8
%current_45args56365 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56364)
store volatile %struct.ScmObj* %current_45args56365, %struct.ScmObj** %stackaddr$prim57680, align 8
%stackaddr$prim57681 = alloca %struct.ScmObj*, align 8
%v48088 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56365)
store volatile %struct.ScmObj* %v48088, %struct.ScmObj** %stackaddr$prim57681, align 8
%stackaddr$prim57682 = alloca %struct.ScmObj*, align 8
%current_45args56366 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56365)
store volatile %struct.ScmObj* %current_45args56366, %struct.ScmObj** %stackaddr$prim57682, align 8
%stackaddr$prim57683 = alloca %struct.ScmObj*, align 8
%r48087 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56366)
store volatile %struct.ScmObj* %r48087, %struct.ScmObj** %stackaddr$prim57683, align 8
%stackaddr$makeclosure57684 = alloca %struct.ScmObj*, align 8
%fptrToInt57685 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49563 to i64
%ae49563 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57685)
store volatile %struct.ScmObj* %ae49563, %struct.ScmObj** %stackaddr$makeclosure57684, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49563, %struct.ScmObj* %k48407, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49563, %struct.ScmObj* %r48087, i64 1)
%argslist56372$f480860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57686 = alloca %struct.ScmObj*, align 8
%argslist56372$f480861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v48088, %struct.ScmObj* %argslist56372$f480860)
store volatile %struct.ScmObj* %argslist56372$f480861, %struct.ScmObj** %stackaddr$prim57686, align 8
%stackaddr$prim57687 = alloca %struct.ScmObj*, align 8
%argslist56372$f480862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49563, %struct.ScmObj* %argslist56372$f480861)
store volatile %struct.ScmObj* %argslist56372$f480862, %struct.ScmObj** %stackaddr$prim57687, align 8
%clofunc57688 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48086)
musttail call tailcc void %clofunc57688(%struct.ScmObj* %f48086, %struct.ScmObj* %argslist56372$f480862)
ret void
}

define tailcc void @proc_clo$ae49563(%struct.ScmObj* %env$ae49563,%struct.ScmObj* %current_45args56368) {
%stackaddr$env-ref57689 = alloca %struct.ScmObj*, align 8
%k48407 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49563, i64 0)
store %struct.ScmObj* %k48407, %struct.ScmObj** %stackaddr$env-ref57689
%stackaddr$env-ref57690 = alloca %struct.ScmObj*, align 8
%r48087 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49563, i64 1)
store %struct.ScmObj* %r48087, %struct.ScmObj** %stackaddr$env-ref57690
%stackaddr$prim57691 = alloca %struct.ScmObj*, align 8
%_95k48408 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56368)
store volatile %struct.ScmObj* %_95k48408, %struct.ScmObj** %stackaddr$prim57691, align 8
%stackaddr$prim57692 = alloca %struct.ScmObj*, align 8
%current_45args56369 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56368)
store volatile %struct.ScmObj* %current_45args56369, %struct.ScmObj** %stackaddr$prim57692, align 8
%stackaddr$prim57693 = alloca %struct.ScmObj*, align 8
%anf_45bind48215 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56369)
store volatile %struct.ScmObj* %anf_45bind48215, %struct.ScmObj** %stackaddr$prim57693, align 8
%stackaddr$prim57694 = alloca %struct.ScmObj*, align 8
%cpsprim48409 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48215, %struct.ScmObj* %r48087)
store volatile %struct.ScmObj* %cpsprim48409, %struct.ScmObj** %stackaddr$prim57694, align 8
%ae49568 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56371$k484070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57695 = alloca %struct.ScmObj*, align 8
%argslist56371$k484071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48409, %struct.ScmObj* %argslist56371$k484070)
store volatile %struct.ScmObj* %argslist56371$k484071, %struct.ScmObj** %stackaddr$prim57695, align 8
%stackaddr$prim57696 = alloca %struct.ScmObj*, align 8
%argslist56371$k484072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49568, %struct.ScmObj* %argslist56371$k484071)
store volatile %struct.ScmObj* %argslist56371$k484072, %struct.ScmObj** %stackaddr$prim57696, align 8
%clofunc57697 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48407)
musttail call tailcc void %clofunc57697(%struct.ScmObj* %k48407, %struct.ScmObj* %argslist56371$k484072)
ret void
}

define tailcc void @proc_clo$ae49172(%struct.ScmObj* %env$ae49172,%struct.ScmObj* %current_45args56376) {
%stackaddr$env-ref57698 = alloca %struct.ScmObj*, align 8
%_37foldr148053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49172, i64 0)
store %struct.ScmObj* %_37foldr148053, %struct.ScmObj** %stackaddr$env-ref57698
%stackaddr$env-ref57699 = alloca %struct.ScmObj*, align 8
%_37map148049 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49172, i64 1)
store %struct.ScmObj* %_37map148049, %struct.ScmObj** %stackaddr$env-ref57699
%stackaddr$prim57700 = alloca %struct.ScmObj*, align 8
%k48410 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56376)
store volatile %struct.ScmObj* %k48410, %struct.ScmObj** %stackaddr$prim57700, align 8
%stackaddr$prim57701 = alloca %struct.ScmObj*, align 8
%current_45args56377 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56376)
store volatile %struct.ScmObj* %current_45args56377, %struct.ScmObj** %stackaddr$prim57701, align 8
%stackaddr$prim57702 = alloca %struct.ScmObj*, align 8
%_37foldr48059 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56377)
store volatile %struct.ScmObj* %_37foldr48059, %struct.ScmObj** %stackaddr$prim57702, align 8
%ae49174 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57703 = alloca %struct.ScmObj*, align 8
%fptrToInt57704 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49175 to i64
%ae49175 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57704)
store volatile %struct.ScmObj* %ae49175, %struct.ScmObj** %stackaddr$makeclosure57703, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49175, %struct.ScmObj* %_37foldr148053, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49175, %struct.ScmObj* %_37map148049, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49175, %struct.ScmObj* %_37foldr48059, i64 2)
%argslist56434$k484100 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57705 = alloca %struct.ScmObj*, align 8
%argslist56434$k484101 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49175, %struct.ScmObj* %argslist56434$k484100)
store volatile %struct.ScmObj* %argslist56434$k484101, %struct.ScmObj** %stackaddr$prim57705, align 8
%stackaddr$prim57706 = alloca %struct.ScmObj*, align 8
%argslist56434$k484102 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49174, %struct.ScmObj* %argslist56434$k484101)
store volatile %struct.ScmObj* %argslist56434$k484102, %struct.ScmObj** %stackaddr$prim57706, align 8
%clofunc57707 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48410)
musttail call tailcc void %clofunc57707(%struct.ScmObj* %k48410, %struct.ScmObj* %argslist56434$k484102)
ret void
}

define tailcc void @proc_clo$ae49175(%struct.ScmObj* %env$ae49175,%struct.ScmObj* %args4806048411) {
%stackaddr$env-ref57708 = alloca %struct.ScmObj*, align 8
%_37foldr148053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49175, i64 0)
store %struct.ScmObj* %_37foldr148053, %struct.ScmObj** %stackaddr$env-ref57708
%stackaddr$env-ref57709 = alloca %struct.ScmObj*, align 8
%_37map148049 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49175, i64 1)
store %struct.ScmObj* %_37map148049, %struct.ScmObj** %stackaddr$env-ref57709
%stackaddr$env-ref57710 = alloca %struct.ScmObj*, align 8
%_37foldr48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49175, i64 2)
store %struct.ScmObj* %_37foldr48059, %struct.ScmObj** %stackaddr$env-ref57710
%stackaddr$prim57711 = alloca %struct.ScmObj*, align 8
%k48412 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4806048411)
store volatile %struct.ScmObj* %k48412, %struct.ScmObj** %stackaddr$prim57711, align 8
%stackaddr$prim57712 = alloca %struct.ScmObj*, align 8
%args48060 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4806048411)
store volatile %struct.ScmObj* %args48060, %struct.ScmObj** %stackaddr$prim57712, align 8
%stackaddr$prim57713 = alloca %struct.ScmObj*, align 8
%f48063 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48060)
store volatile %struct.ScmObj* %f48063, %struct.ScmObj** %stackaddr$prim57713, align 8
%stackaddr$prim57714 = alloca %struct.ScmObj*, align 8
%anf_45bind48202 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48060)
store volatile %struct.ScmObj* %anf_45bind48202, %struct.ScmObj** %stackaddr$prim57714, align 8
%stackaddr$prim57715 = alloca %struct.ScmObj*, align 8
%acc48062 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48202)
store volatile %struct.ScmObj* %acc48062, %struct.ScmObj** %stackaddr$prim57715, align 8
%stackaddr$prim57716 = alloca %struct.ScmObj*, align 8
%anf_45bind48203 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48060)
store volatile %struct.ScmObj* %anf_45bind48203, %struct.ScmObj** %stackaddr$prim57716, align 8
%stackaddr$prim57717 = alloca %struct.ScmObj*, align 8
%lsts48061 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48203)
store volatile %struct.ScmObj* %lsts48061, %struct.ScmObj** %stackaddr$prim57717, align 8
%stackaddr$makeclosure57718 = alloca %struct.ScmObj*, align 8
%fptrToInt57719 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49183 to i64
%ae49183 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt57719)
store volatile %struct.ScmObj* %ae49183, %struct.ScmObj** %stackaddr$makeclosure57718, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49183, %struct.ScmObj* %_37foldr148053, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49183, %struct.ScmObj* %_37map148049, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49183, %struct.ScmObj* %f48063, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49183, %struct.ScmObj* %acc48062, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49183, %struct.ScmObj* %lsts48061, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49183, %struct.ScmObj* %k48412, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49183, %struct.ScmObj* %_37foldr48059, i64 6)
%ae49184 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57720 = alloca %struct.ScmObj*, align 8
%fptrToInt57721 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49185 to i64
%ae49185 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57721)
store volatile %struct.ScmObj* %ae49185, %struct.ScmObj** %stackaddr$makeclosure57720, align 8
%argslist56433$ae491830 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57722 = alloca %struct.ScmObj*, align 8
%argslist56433$ae491831 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49185, %struct.ScmObj* %argslist56433$ae491830)
store volatile %struct.ScmObj* %argslist56433$ae491831, %struct.ScmObj** %stackaddr$prim57722, align 8
%stackaddr$prim57723 = alloca %struct.ScmObj*, align 8
%argslist56433$ae491832 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49184, %struct.ScmObj* %argslist56433$ae491831)
store volatile %struct.ScmObj* %argslist56433$ae491832, %struct.ScmObj** %stackaddr$prim57723, align 8
%clofunc57724 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49183)
musttail call tailcc void %clofunc57724(%struct.ScmObj* %ae49183, %struct.ScmObj* %argslist56433$ae491832)
ret void
}

define tailcc void @proc_clo$ae49183(%struct.ScmObj* %env$ae49183,%struct.ScmObj* %current_45args56379) {
%stackaddr$env-ref57725 = alloca %struct.ScmObj*, align 8
%_37foldr148053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49183, i64 0)
store %struct.ScmObj* %_37foldr148053, %struct.ScmObj** %stackaddr$env-ref57725
%stackaddr$env-ref57726 = alloca %struct.ScmObj*, align 8
%_37map148049 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49183, i64 1)
store %struct.ScmObj* %_37map148049, %struct.ScmObj** %stackaddr$env-ref57726
%stackaddr$env-ref57727 = alloca %struct.ScmObj*, align 8
%f48063 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49183, i64 2)
store %struct.ScmObj* %f48063, %struct.ScmObj** %stackaddr$env-ref57727
%stackaddr$env-ref57728 = alloca %struct.ScmObj*, align 8
%acc48062 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49183, i64 3)
store %struct.ScmObj* %acc48062, %struct.ScmObj** %stackaddr$env-ref57728
%stackaddr$env-ref57729 = alloca %struct.ScmObj*, align 8
%lsts48061 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49183, i64 4)
store %struct.ScmObj* %lsts48061, %struct.ScmObj** %stackaddr$env-ref57729
%stackaddr$env-ref57730 = alloca %struct.ScmObj*, align 8
%k48412 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49183, i64 5)
store %struct.ScmObj* %k48412, %struct.ScmObj** %stackaddr$env-ref57730
%stackaddr$env-ref57731 = alloca %struct.ScmObj*, align 8
%_37foldr48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49183, i64 6)
store %struct.ScmObj* %_37foldr48059, %struct.ScmObj** %stackaddr$env-ref57731
%stackaddr$prim57732 = alloca %struct.ScmObj*, align 8
%_95k48413 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56379)
store volatile %struct.ScmObj* %_95k48413, %struct.ScmObj** %stackaddr$prim57732, align 8
%stackaddr$prim57733 = alloca %struct.ScmObj*, align 8
%current_45args56380 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56379)
store volatile %struct.ScmObj* %current_45args56380, %struct.ScmObj** %stackaddr$prim57733, align 8
%stackaddr$prim57734 = alloca %struct.ScmObj*, align 8
%anf_45bind48204 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56380)
store volatile %struct.ScmObj* %anf_45bind48204, %struct.ScmObj** %stackaddr$prim57734, align 8
%stackaddr$makeclosure57735 = alloca %struct.ScmObj*, align 8
%fptrToInt57736 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49215 to i64
%ae49215 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt57736)
store volatile %struct.ScmObj* %ae49215, %struct.ScmObj** %stackaddr$makeclosure57735, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49215, %struct.ScmObj* %_37foldr148053, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49215, %struct.ScmObj* %_37map148049, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49215, %struct.ScmObj* %f48063, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49215, %struct.ScmObj* %acc48062, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49215, %struct.ScmObj* %lsts48061, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49215, %struct.ScmObj* %k48412, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49215, %struct.ScmObj* %_37foldr48059, i64 6)
%ae49217 = call %struct.ScmObj* @const_init_false()
%argslist56426$_37foldr1480530 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57737 = alloca %struct.ScmObj*, align 8
%argslist56426$_37foldr1480531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48061, %struct.ScmObj* %argslist56426$_37foldr1480530)
store volatile %struct.ScmObj* %argslist56426$_37foldr1480531, %struct.ScmObj** %stackaddr$prim57737, align 8
%stackaddr$prim57738 = alloca %struct.ScmObj*, align 8
%argslist56426$_37foldr1480532 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49217, %struct.ScmObj* %argslist56426$_37foldr1480531)
store volatile %struct.ScmObj* %argslist56426$_37foldr1480532, %struct.ScmObj** %stackaddr$prim57738, align 8
%stackaddr$prim57739 = alloca %struct.ScmObj*, align 8
%argslist56426$_37foldr1480533 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48204, %struct.ScmObj* %argslist56426$_37foldr1480532)
store volatile %struct.ScmObj* %argslist56426$_37foldr1480533, %struct.ScmObj** %stackaddr$prim57739, align 8
%stackaddr$prim57740 = alloca %struct.ScmObj*, align 8
%argslist56426$_37foldr1480534 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49215, %struct.ScmObj* %argslist56426$_37foldr1480533)
store volatile %struct.ScmObj* %argslist56426$_37foldr1480534, %struct.ScmObj** %stackaddr$prim57740, align 8
%clofunc57741 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148053)
musttail call tailcc void %clofunc57741(%struct.ScmObj* %_37foldr148053, %struct.ScmObj* %argslist56426$_37foldr1480534)
ret void
}

define tailcc void @proc_clo$ae49215(%struct.ScmObj* %env$ae49215,%struct.ScmObj* %current_45args56382) {
%stackaddr$env-ref57742 = alloca %struct.ScmObj*, align 8
%_37foldr148053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49215, i64 0)
store %struct.ScmObj* %_37foldr148053, %struct.ScmObj** %stackaddr$env-ref57742
%stackaddr$env-ref57743 = alloca %struct.ScmObj*, align 8
%_37map148049 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49215, i64 1)
store %struct.ScmObj* %_37map148049, %struct.ScmObj** %stackaddr$env-ref57743
%stackaddr$env-ref57744 = alloca %struct.ScmObj*, align 8
%f48063 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49215, i64 2)
store %struct.ScmObj* %f48063, %struct.ScmObj** %stackaddr$env-ref57744
%stackaddr$env-ref57745 = alloca %struct.ScmObj*, align 8
%acc48062 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49215, i64 3)
store %struct.ScmObj* %acc48062, %struct.ScmObj** %stackaddr$env-ref57745
%stackaddr$env-ref57746 = alloca %struct.ScmObj*, align 8
%lsts48061 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49215, i64 4)
store %struct.ScmObj* %lsts48061, %struct.ScmObj** %stackaddr$env-ref57746
%stackaddr$env-ref57747 = alloca %struct.ScmObj*, align 8
%k48412 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49215, i64 5)
store %struct.ScmObj* %k48412, %struct.ScmObj** %stackaddr$env-ref57747
%stackaddr$env-ref57748 = alloca %struct.ScmObj*, align 8
%_37foldr48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49215, i64 6)
store %struct.ScmObj* %_37foldr48059, %struct.ScmObj** %stackaddr$env-ref57748
%stackaddr$prim57749 = alloca %struct.ScmObj*, align 8
%_95k48414 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56382)
store volatile %struct.ScmObj* %_95k48414, %struct.ScmObj** %stackaddr$prim57749, align 8
%stackaddr$prim57750 = alloca %struct.ScmObj*, align 8
%current_45args56383 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56382)
store volatile %struct.ScmObj* %current_45args56383, %struct.ScmObj** %stackaddr$prim57750, align 8
%stackaddr$prim57751 = alloca %struct.ScmObj*, align 8
%anf_45bind48205 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56383)
store volatile %struct.ScmObj* %anf_45bind48205, %struct.ScmObj** %stackaddr$prim57751, align 8
%truthy$cmp57752 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48205)
%cmp$cmp57752 = icmp eq i64 %truthy$cmp57752, 1
br i1 %cmp$cmp57752, label %truebranch$cmp57752, label %falsebranch$cmp57752
truebranch$cmp57752:
%ae49226 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56385$k484120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57753 = alloca %struct.ScmObj*, align 8
%argslist56385$k484121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48062, %struct.ScmObj* %argslist56385$k484120)
store volatile %struct.ScmObj* %argslist56385$k484121, %struct.ScmObj** %stackaddr$prim57753, align 8
%stackaddr$prim57754 = alloca %struct.ScmObj*, align 8
%argslist56385$k484122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49226, %struct.ScmObj* %argslist56385$k484121)
store volatile %struct.ScmObj* %argslist56385$k484122, %struct.ScmObj** %stackaddr$prim57754, align 8
%clofunc57755 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48412)
musttail call tailcc void %clofunc57755(%struct.ScmObj* %k48412, %struct.ScmObj* %argslist56385$k484122)
ret void
falsebranch$cmp57752:
%stackaddr$makeclosure57756 = alloca %struct.ScmObj*, align 8
%fptrToInt57757 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49231 to i64
%ae49231 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt57757)
store volatile %struct.ScmObj* %ae49231, %struct.ScmObj** %stackaddr$makeclosure57756, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49231, %struct.ScmObj* %_37foldr148053, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49231, %struct.ScmObj* %_37map148049, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49231, %struct.ScmObj* %f48063, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49231, %struct.ScmObj* %acc48062, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49231, %struct.ScmObj* %lsts48061, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49231, %struct.ScmObj* %k48412, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49231, %struct.ScmObj* %_37foldr48059, i64 6)
%ae49232 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57758 = alloca %struct.ScmObj*, align 8
%fptrToInt57759 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49233 to i64
%ae49233 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57759)
store volatile %struct.ScmObj* %ae49233, %struct.ScmObj** %stackaddr$makeclosure57758, align 8
%argslist56425$ae492310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57760 = alloca %struct.ScmObj*, align 8
%argslist56425$ae492311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49233, %struct.ScmObj* %argslist56425$ae492310)
store volatile %struct.ScmObj* %argslist56425$ae492311, %struct.ScmObj** %stackaddr$prim57760, align 8
%stackaddr$prim57761 = alloca %struct.ScmObj*, align 8
%argslist56425$ae492312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49232, %struct.ScmObj* %argslist56425$ae492311)
store volatile %struct.ScmObj* %argslist56425$ae492312, %struct.ScmObj** %stackaddr$prim57761, align 8
%clofunc57762 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49231)
musttail call tailcc void %clofunc57762(%struct.ScmObj* %ae49231, %struct.ScmObj* %argslist56425$ae492312)
ret void
}

define tailcc void @proc_clo$ae49231(%struct.ScmObj* %env$ae49231,%struct.ScmObj* %current_45args56386) {
%stackaddr$env-ref57763 = alloca %struct.ScmObj*, align 8
%_37foldr148053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49231, i64 0)
store %struct.ScmObj* %_37foldr148053, %struct.ScmObj** %stackaddr$env-ref57763
%stackaddr$env-ref57764 = alloca %struct.ScmObj*, align 8
%_37map148049 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49231, i64 1)
store %struct.ScmObj* %_37map148049, %struct.ScmObj** %stackaddr$env-ref57764
%stackaddr$env-ref57765 = alloca %struct.ScmObj*, align 8
%f48063 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49231, i64 2)
store %struct.ScmObj* %f48063, %struct.ScmObj** %stackaddr$env-ref57765
%stackaddr$env-ref57766 = alloca %struct.ScmObj*, align 8
%acc48062 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49231, i64 3)
store %struct.ScmObj* %acc48062, %struct.ScmObj** %stackaddr$env-ref57766
%stackaddr$env-ref57767 = alloca %struct.ScmObj*, align 8
%lsts48061 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49231, i64 4)
store %struct.ScmObj* %lsts48061, %struct.ScmObj** %stackaddr$env-ref57767
%stackaddr$env-ref57768 = alloca %struct.ScmObj*, align 8
%k48412 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49231, i64 5)
store %struct.ScmObj* %k48412, %struct.ScmObj** %stackaddr$env-ref57768
%stackaddr$env-ref57769 = alloca %struct.ScmObj*, align 8
%_37foldr48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49231, i64 6)
store %struct.ScmObj* %_37foldr48059, %struct.ScmObj** %stackaddr$env-ref57769
%stackaddr$prim57770 = alloca %struct.ScmObj*, align 8
%_95k48415 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56386)
store volatile %struct.ScmObj* %_95k48415, %struct.ScmObj** %stackaddr$prim57770, align 8
%stackaddr$prim57771 = alloca %struct.ScmObj*, align 8
%current_45args56387 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56386)
store volatile %struct.ScmObj* %current_45args56387, %struct.ScmObj** %stackaddr$prim57771, align 8
%stackaddr$prim57772 = alloca %struct.ScmObj*, align 8
%anf_45bind48206 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56387)
store volatile %struct.ScmObj* %anf_45bind48206, %struct.ScmObj** %stackaddr$prim57772, align 8
%stackaddr$makeclosure57773 = alloca %struct.ScmObj*, align 8
%fptrToInt57774 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49252 to i64
%ae49252 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt57774)
store volatile %struct.ScmObj* %ae49252, %struct.ScmObj** %stackaddr$makeclosure57773, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49252, %struct.ScmObj* %_37foldr148053, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49252, %struct.ScmObj* %_37map148049, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49252, %struct.ScmObj* %f48063, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49252, %struct.ScmObj* %acc48062, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49252, %struct.ScmObj* %lsts48061, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49252, %struct.ScmObj* %k48412, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49252, %struct.ScmObj* %_37foldr48059, i64 6)
%argslist56420$_37map1480490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57775 = alloca %struct.ScmObj*, align 8
%argslist56420$_37map1480491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48061, %struct.ScmObj* %argslist56420$_37map1480490)
store volatile %struct.ScmObj* %argslist56420$_37map1480491, %struct.ScmObj** %stackaddr$prim57775, align 8
%stackaddr$prim57776 = alloca %struct.ScmObj*, align 8
%argslist56420$_37map1480492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48206, %struct.ScmObj* %argslist56420$_37map1480491)
store volatile %struct.ScmObj* %argslist56420$_37map1480492, %struct.ScmObj** %stackaddr$prim57776, align 8
%stackaddr$prim57777 = alloca %struct.ScmObj*, align 8
%argslist56420$_37map1480493 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49252, %struct.ScmObj* %argslist56420$_37map1480492)
store volatile %struct.ScmObj* %argslist56420$_37map1480493, %struct.ScmObj** %stackaddr$prim57777, align 8
%clofunc57778 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148049)
musttail call tailcc void %clofunc57778(%struct.ScmObj* %_37map148049, %struct.ScmObj* %argslist56420$_37map1480493)
ret void
}

define tailcc void @proc_clo$ae49252(%struct.ScmObj* %env$ae49252,%struct.ScmObj* %current_45args56389) {
%stackaddr$env-ref57779 = alloca %struct.ScmObj*, align 8
%_37foldr148053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49252, i64 0)
store %struct.ScmObj* %_37foldr148053, %struct.ScmObj** %stackaddr$env-ref57779
%stackaddr$env-ref57780 = alloca %struct.ScmObj*, align 8
%_37map148049 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49252, i64 1)
store %struct.ScmObj* %_37map148049, %struct.ScmObj** %stackaddr$env-ref57780
%stackaddr$env-ref57781 = alloca %struct.ScmObj*, align 8
%f48063 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49252, i64 2)
store %struct.ScmObj* %f48063, %struct.ScmObj** %stackaddr$env-ref57781
%stackaddr$env-ref57782 = alloca %struct.ScmObj*, align 8
%acc48062 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49252, i64 3)
store %struct.ScmObj* %acc48062, %struct.ScmObj** %stackaddr$env-ref57782
%stackaddr$env-ref57783 = alloca %struct.ScmObj*, align 8
%lsts48061 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49252, i64 4)
store %struct.ScmObj* %lsts48061, %struct.ScmObj** %stackaddr$env-ref57783
%stackaddr$env-ref57784 = alloca %struct.ScmObj*, align 8
%k48412 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49252, i64 5)
store %struct.ScmObj* %k48412, %struct.ScmObj** %stackaddr$env-ref57784
%stackaddr$env-ref57785 = alloca %struct.ScmObj*, align 8
%_37foldr48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49252, i64 6)
store %struct.ScmObj* %_37foldr48059, %struct.ScmObj** %stackaddr$env-ref57785
%stackaddr$prim57786 = alloca %struct.ScmObj*, align 8
%_95k48416 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56389)
store volatile %struct.ScmObj* %_95k48416, %struct.ScmObj** %stackaddr$prim57786, align 8
%stackaddr$prim57787 = alloca %struct.ScmObj*, align 8
%current_45args56390 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56389)
store volatile %struct.ScmObj* %current_45args56390, %struct.ScmObj** %stackaddr$prim57787, align 8
%stackaddr$prim57788 = alloca %struct.ScmObj*, align 8
%lsts_4348068 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56390)
store volatile %struct.ScmObj* %lsts_4348068, %struct.ScmObj** %stackaddr$prim57788, align 8
%stackaddr$makeclosure57789 = alloca %struct.ScmObj*, align 8
%fptrToInt57790 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49255 to i64
%ae49255 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt57790)
store volatile %struct.ScmObj* %ae49255, %struct.ScmObj** %stackaddr$makeclosure57789, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49255, %struct.ScmObj* %_37foldr148053, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49255, %struct.ScmObj* %lsts_4348068, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49255, %struct.ScmObj* %_37map148049, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49255, %struct.ScmObj* %f48063, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49255, %struct.ScmObj* %acc48062, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49255, %struct.ScmObj* %lsts48061, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49255, %struct.ScmObj* %k48412, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae49255, %struct.ScmObj* %_37foldr48059, i64 7)
%ae49256 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57791 = alloca %struct.ScmObj*, align 8
%fptrToInt57792 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49257 to i64
%ae49257 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57792)
store volatile %struct.ScmObj* %ae49257, %struct.ScmObj** %stackaddr$makeclosure57791, align 8
%argslist56419$ae492550 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57793 = alloca %struct.ScmObj*, align 8
%argslist56419$ae492551 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49257, %struct.ScmObj* %argslist56419$ae492550)
store volatile %struct.ScmObj* %argslist56419$ae492551, %struct.ScmObj** %stackaddr$prim57793, align 8
%stackaddr$prim57794 = alloca %struct.ScmObj*, align 8
%argslist56419$ae492552 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49256, %struct.ScmObj* %argslist56419$ae492551)
store volatile %struct.ScmObj* %argslist56419$ae492552, %struct.ScmObj** %stackaddr$prim57794, align 8
%clofunc57795 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49255)
musttail call tailcc void %clofunc57795(%struct.ScmObj* %ae49255, %struct.ScmObj* %argslist56419$ae492552)
ret void
}

define tailcc void @proc_clo$ae49255(%struct.ScmObj* %env$ae49255,%struct.ScmObj* %current_45args56392) {
%stackaddr$env-ref57796 = alloca %struct.ScmObj*, align 8
%_37foldr148053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49255, i64 0)
store %struct.ScmObj* %_37foldr148053, %struct.ScmObj** %stackaddr$env-ref57796
%stackaddr$env-ref57797 = alloca %struct.ScmObj*, align 8
%lsts_4348068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49255, i64 1)
store %struct.ScmObj* %lsts_4348068, %struct.ScmObj** %stackaddr$env-ref57797
%stackaddr$env-ref57798 = alloca %struct.ScmObj*, align 8
%_37map148049 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49255, i64 2)
store %struct.ScmObj* %_37map148049, %struct.ScmObj** %stackaddr$env-ref57798
%stackaddr$env-ref57799 = alloca %struct.ScmObj*, align 8
%f48063 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49255, i64 3)
store %struct.ScmObj* %f48063, %struct.ScmObj** %stackaddr$env-ref57799
%stackaddr$env-ref57800 = alloca %struct.ScmObj*, align 8
%acc48062 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49255, i64 4)
store %struct.ScmObj* %acc48062, %struct.ScmObj** %stackaddr$env-ref57800
%stackaddr$env-ref57801 = alloca %struct.ScmObj*, align 8
%lsts48061 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49255, i64 5)
store %struct.ScmObj* %lsts48061, %struct.ScmObj** %stackaddr$env-ref57801
%stackaddr$env-ref57802 = alloca %struct.ScmObj*, align 8
%k48412 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49255, i64 6)
store %struct.ScmObj* %k48412, %struct.ScmObj** %stackaddr$env-ref57802
%stackaddr$env-ref57803 = alloca %struct.ScmObj*, align 8
%_37foldr48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49255, i64 7)
store %struct.ScmObj* %_37foldr48059, %struct.ScmObj** %stackaddr$env-ref57803
%stackaddr$prim57804 = alloca %struct.ScmObj*, align 8
%_95k48417 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56392)
store volatile %struct.ScmObj* %_95k48417, %struct.ScmObj** %stackaddr$prim57804, align 8
%stackaddr$prim57805 = alloca %struct.ScmObj*, align 8
%current_45args56393 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56392)
store volatile %struct.ScmObj* %current_45args56393, %struct.ScmObj** %stackaddr$prim57805, align 8
%stackaddr$prim57806 = alloca %struct.ScmObj*, align 8
%anf_45bind48207 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56393)
store volatile %struct.ScmObj* %anf_45bind48207, %struct.ScmObj** %stackaddr$prim57806, align 8
%stackaddr$makeclosure57807 = alloca %struct.ScmObj*, align 8
%fptrToInt57808 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49276 to i64
%ae49276 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt57808)
store volatile %struct.ScmObj* %ae49276, %struct.ScmObj** %stackaddr$makeclosure57807, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49276, %struct.ScmObj* %_37foldr148053, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49276, %struct.ScmObj* %lsts_4348068, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49276, %struct.ScmObj* %f48063, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49276, %struct.ScmObj* %acc48062, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49276, %struct.ScmObj* %k48412, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49276, %struct.ScmObj* %_37foldr48059, i64 5)
%argslist56414$_37map1480490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57809 = alloca %struct.ScmObj*, align 8
%argslist56414$_37map1480491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48061, %struct.ScmObj* %argslist56414$_37map1480490)
store volatile %struct.ScmObj* %argslist56414$_37map1480491, %struct.ScmObj** %stackaddr$prim57809, align 8
%stackaddr$prim57810 = alloca %struct.ScmObj*, align 8
%argslist56414$_37map1480492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48207, %struct.ScmObj* %argslist56414$_37map1480491)
store volatile %struct.ScmObj* %argslist56414$_37map1480492, %struct.ScmObj** %stackaddr$prim57810, align 8
%stackaddr$prim57811 = alloca %struct.ScmObj*, align 8
%argslist56414$_37map1480493 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49276, %struct.ScmObj* %argslist56414$_37map1480492)
store volatile %struct.ScmObj* %argslist56414$_37map1480493, %struct.ScmObj** %stackaddr$prim57811, align 8
%clofunc57812 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148049)
musttail call tailcc void %clofunc57812(%struct.ScmObj* %_37map148049, %struct.ScmObj* %argslist56414$_37map1480493)
ret void
}

define tailcc void @proc_clo$ae49276(%struct.ScmObj* %env$ae49276,%struct.ScmObj* %current_45args56395) {
%stackaddr$env-ref57813 = alloca %struct.ScmObj*, align 8
%_37foldr148053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49276, i64 0)
store %struct.ScmObj* %_37foldr148053, %struct.ScmObj** %stackaddr$env-ref57813
%stackaddr$env-ref57814 = alloca %struct.ScmObj*, align 8
%lsts_4348068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49276, i64 1)
store %struct.ScmObj* %lsts_4348068, %struct.ScmObj** %stackaddr$env-ref57814
%stackaddr$env-ref57815 = alloca %struct.ScmObj*, align 8
%f48063 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49276, i64 2)
store %struct.ScmObj* %f48063, %struct.ScmObj** %stackaddr$env-ref57815
%stackaddr$env-ref57816 = alloca %struct.ScmObj*, align 8
%acc48062 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49276, i64 3)
store %struct.ScmObj* %acc48062, %struct.ScmObj** %stackaddr$env-ref57816
%stackaddr$env-ref57817 = alloca %struct.ScmObj*, align 8
%k48412 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49276, i64 4)
store %struct.ScmObj* %k48412, %struct.ScmObj** %stackaddr$env-ref57817
%stackaddr$env-ref57818 = alloca %struct.ScmObj*, align 8
%_37foldr48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49276, i64 5)
store %struct.ScmObj* %_37foldr48059, %struct.ScmObj** %stackaddr$env-ref57818
%stackaddr$prim57819 = alloca %struct.ScmObj*, align 8
%_95k48418 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56395)
store volatile %struct.ScmObj* %_95k48418, %struct.ScmObj** %stackaddr$prim57819, align 8
%stackaddr$prim57820 = alloca %struct.ScmObj*, align 8
%current_45args56396 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56395)
store volatile %struct.ScmObj* %current_45args56396, %struct.ScmObj** %stackaddr$prim57820, align 8
%stackaddr$prim57821 = alloca %struct.ScmObj*, align 8
%vs48066 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56396)
store volatile %struct.ScmObj* %vs48066, %struct.ScmObj** %stackaddr$prim57821, align 8
%stackaddr$makeclosure57822 = alloca %struct.ScmObj*, align 8
%fptrToInt57823 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49279 to i64
%ae49279 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt57823)
store volatile %struct.ScmObj* %ae49279, %struct.ScmObj** %stackaddr$makeclosure57822, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49279, %struct.ScmObj* %_37foldr148053, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49279, %struct.ScmObj* %lsts_4348068, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49279, %struct.ScmObj* %vs48066, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49279, %struct.ScmObj* %f48063, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49279, %struct.ScmObj* %acc48062, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49279, %struct.ScmObj* %k48412, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49279, %struct.ScmObj* %_37foldr48059, i64 6)
%ae49280 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57824 = alloca %struct.ScmObj*, align 8
%fptrToInt57825 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49281 to i64
%ae49281 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57825)
store volatile %struct.ScmObj* %ae49281, %struct.ScmObj** %stackaddr$makeclosure57824, align 8
%argslist56413$ae492790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57826 = alloca %struct.ScmObj*, align 8
%argslist56413$ae492791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49281, %struct.ScmObj* %argslist56413$ae492790)
store volatile %struct.ScmObj* %argslist56413$ae492791, %struct.ScmObj** %stackaddr$prim57826, align 8
%stackaddr$prim57827 = alloca %struct.ScmObj*, align 8
%argslist56413$ae492792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49280, %struct.ScmObj* %argslist56413$ae492791)
store volatile %struct.ScmObj* %argslist56413$ae492792, %struct.ScmObj** %stackaddr$prim57827, align 8
%clofunc57828 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49279)
musttail call tailcc void %clofunc57828(%struct.ScmObj* %ae49279, %struct.ScmObj* %argslist56413$ae492792)
ret void
}

define tailcc void @proc_clo$ae49279(%struct.ScmObj* %env$ae49279,%struct.ScmObj* %current_45args56398) {
%stackaddr$env-ref57829 = alloca %struct.ScmObj*, align 8
%_37foldr148053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49279, i64 0)
store %struct.ScmObj* %_37foldr148053, %struct.ScmObj** %stackaddr$env-ref57829
%stackaddr$env-ref57830 = alloca %struct.ScmObj*, align 8
%lsts_4348068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49279, i64 1)
store %struct.ScmObj* %lsts_4348068, %struct.ScmObj** %stackaddr$env-ref57830
%stackaddr$env-ref57831 = alloca %struct.ScmObj*, align 8
%vs48066 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49279, i64 2)
store %struct.ScmObj* %vs48066, %struct.ScmObj** %stackaddr$env-ref57831
%stackaddr$env-ref57832 = alloca %struct.ScmObj*, align 8
%f48063 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49279, i64 3)
store %struct.ScmObj* %f48063, %struct.ScmObj** %stackaddr$env-ref57832
%stackaddr$env-ref57833 = alloca %struct.ScmObj*, align 8
%acc48062 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49279, i64 4)
store %struct.ScmObj* %acc48062, %struct.ScmObj** %stackaddr$env-ref57833
%stackaddr$env-ref57834 = alloca %struct.ScmObj*, align 8
%k48412 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49279, i64 5)
store %struct.ScmObj* %k48412, %struct.ScmObj** %stackaddr$env-ref57834
%stackaddr$env-ref57835 = alloca %struct.ScmObj*, align 8
%_37foldr48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49279, i64 6)
store %struct.ScmObj* %_37foldr48059, %struct.ScmObj** %stackaddr$env-ref57835
%stackaddr$prim57836 = alloca %struct.ScmObj*, align 8
%_95k48419 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56398)
store volatile %struct.ScmObj* %_95k48419, %struct.ScmObj** %stackaddr$prim57836, align 8
%stackaddr$prim57837 = alloca %struct.ScmObj*, align 8
%current_45args56399 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56398)
store volatile %struct.ScmObj* %current_45args56399, %struct.ScmObj** %stackaddr$prim57837, align 8
%stackaddr$prim57838 = alloca %struct.ScmObj*, align 8
%anf_45bind48208 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56399)
store volatile %struct.ScmObj* %anf_45bind48208, %struct.ScmObj** %stackaddr$prim57838, align 8
%stackaddr$prim57839 = alloca %struct.ScmObj*, align 8
%anf_45bind48209 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48062, %struct.ScmObj* %lsts_4348068)
store volatile %struct.ScmObj* %anf_45bind48209, %struct.ScmObj** %stackaddr$prim57839, align 8
%stackaddr$prim57840 = alloca %struct.ScmObj*, align 8
%anf_45bind48210 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48063, %struct.ScmObj* %anf_45bind48209)
store volatile %struct.ScmObj* %anf_45bind48210, %struct.ScmObj** %stackaddr$prim57840, align 8
%stackaddr$makeclosure57841 = alloca %struct.ScmObj*, align 8
%fptrToInt57842 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49305 to i64
%ae49305 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt57842)
store volatile %struct.ScmObj* %ae49305, %struct.ScmObj** %stackaddr$makeclosure57841, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49305, %struct.ScmObj* %_37foldr148053, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49305, %struct.ScmObj* %vs48066, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49305, %struct.ScmObj* %anf_45bind48208, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49305, %struct.ScmObj* %f48063, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49305, %struct.ScmObj* %k48412, i64 4)
%stackaddr$prim57843 = alloca %struct.ScmObj*, align 8
%cpsargs48423 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49305, %struct.ScmObj* %anf_45bind48210)
store volatile %struct.ScmObj* %cpsargs48423, %struct.ScmObj** %stackaddr$prim57843, align 8
%clofunc57844 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr48059)
musttail call tailcc void %clofunc57844(%struct.ScmObj* %_37foldr48059, %struct.ScmObj* %cpsargs48423)
ret void
}

define tailcc void @proc_clo$ae49305(%struct.ScmObj* %env$ae49305,%struct.ScmObj* %current_45args56401) {
%stackaddr$env-ref57845 = alloca %struct.ScmObj*, align 8
%_37foldr148053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49305, i64 0)
store %struct.ScmObj* %_37foldr148053, %struct.ScmObj** %stackaddr$env-ref57845
%stackaddr$env-ref57846 = alloca %struct.ScmObj*, align 8
%vs48066 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49305, i64 1)
store %struct.ScmObj* %vs48066, %struct.ScmObj** %stackaddr$env-ref57846
%stackaddr$env-ref57847 = alloca %struct.ScmObj*, align 8
%anf_45bind48208 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49305, i64 2)
store %struct.ScmObj* %anf_45bind48208, %struct.ScmObj** %stackaddr$env-ref57847
%stackaddr$env-ref57848 = alloca %struct.ScmObj*, align 8
%f48063 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49305, i64 3)
store %struct.ScmObj* %f48063, %struct.ScmObj** %stackaddr$env-ref57848
%stackaddr$env-ref57849 = alloca %struct.ScmObj*, align 8
%k48412 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49305, i64 4)
store %struct.ScmObj* %k48412, %struct.ScmObj** %stackaddr$env-ref57849
%stackaddr$prim57850 = alloca %struct.ScmObj*, align 8
%_95k48420 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56401)
store volatile %struct.ScmObj* %_95k48420, %struct.ScmObj** %stackaddr$prim57850, align 8
%stackaddr$prim57851 = alloca %struct.ScmObj*, align 8
%current_45args56402 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56401)
store volatile %struct.ScmObj* %current_45args56402, %struct.ScmObj** %stackaddr$prim57851, align 8
%stackaddr$prim57852 = alloca %struct.ScmObj*, align 8
%anf_45bind48211 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56402)
store volatile %struct.ScmObj* %anf_45bind48211, %struct.ScmObj** %stackaddr$prim57852, align 8
%ae49310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57853 = alloca %struct.ScmObj*, align 8
%anf_45bind48212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48211, %struct.ScmObj* %ae49310)
store volatile %struct.ScmObj* %anf_45bind48212, %struct.ScmObj** %stackaddr$prim57853, align 8
%stackaddr$makeclosure57854 = alloca %struct.ScmObj*, align 8
%fptrToInt57855 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49312 to i64
%ae49312 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57855)
store volatile %struct.ScmObj* %ae49312, %struct.ScmObj** %stackaddr$makeclosure57854, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49312, %struct.ScmObj* %f48063, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49312, %struct.ScmObj* %k48412, i64 1)
%argslist56407$_37foldr1480530 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57856 = alloca %struct.ScmObj*, align 8
%argslist56407$_37foldr1480531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs48066, %struct.ScmObj* %argslist56407$_37foldr1480530)
store volatile %struct.ScmObj* %argslist56407$_37foldr1480531, %struct.ScmObj** %stackaddr$prim57856, align 8
%stackaddr$prim57857 = alloca %struct.ScmObj*, align 8
%argslist56407$_37foldr1480532 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48212, %struct.ScmObj* %argslist56407$_37foldr1480531)
store volatile %struct.ScmObj* %argslist56407$_37foldr1480532, %struct.ScmObj** %stackaddr$prim57857, align 8
%stackaddr$prim57858 = alloca %struct.ScmObj*, align 8
%argslist56407$_37foldr1480533 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48208, %struct.ScmObj* %argslist56407$_37foldr1480532)
store volatile %struct.ScmObj* %argslist56407$_37foldr1480533, %struct.ScmObj** %stackaddr$prim57858, align 8
%stackaddr$prim57859 = alloca %struct.ScmObj*, align 8
%argslist56407$_37foldr1480534 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49312, %struct.ScmObj* %argslist56407$_37foldr1480533)
store volatile %struct.ScmObj* %argslist56407$_37foldr1480534, %struct.ScmObj** %stackaddr$prim57859, align 8
%clofunc57860 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148053)
musttail call tailcc void %clofunc57860(%struct.ScmObj* %_37foldr148053, %struct.ScmObj* %argslist56407$_37foldr1480534)
ret void
}

define tailcc void @proc_clo$ae49312(%struct.ScmObj* %env$ae49312,%struct.ScmObj* %current_45args56404) {
%stackaddr$env-ref57861 = alloca %struct.ScmObj*, align 8
%f48063 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49312, i64 0)
store %struct.ScmObj* %f48063, %struct.ScmObj** %stackaddr$env-ref57861
%stackaddr$env-ref57862 = alloca %struct.ScmObj*, align 8
%k48412 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49312, i64 1)
store %struct.ScmObj* %k48412, %struct.ScmObj** %stackaddr$env-ref57862
%stackaddr$prim57863 = alloca %struct.ScmObj*, align 8
%_95k48421 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56404)
store volatile %struct.ScmObj* %_95k48421, %struct.ScmObj** %stackaddr$prim57863, align 8
%stackaddr$prim57864 = alloca %struct.ScmObj*, align 8
%current_45args56405 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56404)
store volatile %struct.ScmObj* %current_45args56405, %struct.ScmObj** %stackaddr$prim57864, align 8
%stackaddr$prim57865 = alloca %struct.ScmObj*, align 8
%anf_45bind48213 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56405)
store volatile %struct.ScmObj* %anf_45bind48213, %struct.ScmObj** %stackaddr$prim57865, align 8
%stackaddr$prim57866 = alloca %struct.ScmObj*, align 8
%cpsargs48422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48412, %struct.ScmObj* %anf_45bind48213)
store volatile %struct.ScmObj* %cpsargs48422, %struct.ScmObj** %stackaddr$prim57866, align 8
%clofunc57867 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48063)
musttail call tailcc void %clofunc57867(%struct.ScmObj* %f48063, %struct.ScmObj* %cpsargs48422)
ret void
}

define tailcc void @proc_clo$ae49281(%struct.ScmObj* %env$ae49281,%struct.ScmObj* %current_45args56408) {
%stackaddr$prim57868 = alloca %struct.ScmObj*, align 8
%k48424 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56408)
store volatile %struct.ScmObj* %k48424, %struct.ScmObj** %stackaddr$prim57868, align 8
%stackaddr$prim57869 = alloca %struct.ScmObj*, align 8
%current_45args56409 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56408)
store volatile %struct.ScmObj* %current_45args56409, %struct.ScmObj** %stackaddr$prim57869, align 8
%stackaddr$prim57870 = alloca %struct.ScmObj*, align 8
%a48071 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56409)
store volatile %struct.ScmObj* %a48071, %struct.ScmObj** %stackaddr$prim57870, align 8
%stackaddr$prim57871 = alloca %struct.ScmObj*, align 8
%current_45args56410 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56409)
store volatile %struct.ScmObj* %current_45args56410, %struct.ScmObj** %stackaddr$prim57871, align 8
%stackaddr$prim57872 = alloca %struct.ScmObj*, align 8
%b48070 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56410)
store volatile %struct.ScmObj* %b48070, %struct.ScmObj** %stackaddr$prim57872, align 8
%stackaddr$prim57873 = alloca %struct.ScmObj*, align 8
%cpsprim48425 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a48071, %struct.ScmObj* %b48070)
store volatile %struct.ScmObj* %cpsprim48425, %struct.ScmObj** %stackaddr$prim57873, align 8
%ae49285 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56412$k484240 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57874 = alloca %struct.ScmObj*, align 8
%argslist56412$k484241 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48425, %struct.ScmObj* %argslist56412$k484240)
store volatile %struct.ScmObj* %argslist56412$k484241, %struct.ScmObj** %stackaddr$prim57874, align 8
%stackaddr$prim57875 = alloca %struct.ScmObj*, align 8
%argslist56412$k484242 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49285, %struct.ScmObj* %argslist56412$k484241)
store volatile %struct.ScmObj* %argslist56412$k484242, %struct.ScmObj** %stackaddr$prim57875, align 8
%clofunc57876 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48424)
musttail call tailcc void %clofunc57876(%struct.ScmObj* %k48424, %struct.ScmObj* %argslist56412$k484242)
ret void
}

define tailcc void @proc_clo$ae49257(%struct.ScmObj* %env$ae49257,%struct.ScmObj* %current_45args56415) {
%stackaddr$prim57877 = alloca %struct.ScmObj*, align 8
%k48426 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56415)
store volatile %struct.ScmObj* %k48426, %struct.ScmObj** %stackaddr$prim57877, align 8
%stackaddr$prim57878 = alloca %struct.ScmObj*, align 8
%current_45args56416 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56415)
store volatile %struct.ScmObj* %current_45args56416, %struct.ScmObj** %stackaddr$prim57878, align 8
%stackaddr$prim57879 = alloca %struct.ScmObj*, align 8
%x48067 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56416)
store volatile %struct.ScmObj* %x48067, %struct.ScmObj** %stackaddr$prim57879, align 8
%stackaddr$prim57880 = alloca %struct.ScmObj*, align 8
%cpsprim48427 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48067)
store volatile %struct.ScmObj* %cpsprim48427, %struct.ScmObj** %stackaddr$prim57880, align 8
%ae49260 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56418$k484260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57881 = alloca %struct.ScmObj*, align 8
%argslist56418$k484261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48427, %struct.ScmObj* %argslist56418$k484260)
store volatile %struct.ScmObj* %argslist56418$k484261, %struct.ScmObj** %stackaddr$prim57881, align 8
%stackaddr$prim57882 = alloca %struct.ScmObj*, align 8
%argslist56418$k484262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49260, %struct.ScmObj* %argslist56418$k484261)
store volatile %struct.ScmObj* %argslist56418$k484262, %struct.ScmObj** %stackaddr$prim57882, align 8
%clofunc57883 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48426)
musttail call tailcc void %clofunc57883(%struct.ScmObj* %k48426, %struct.ScmObj* %argslist56418$k484262)
ret void
}

define tailcc void @proc_clo$ae49233(%struct.ScmObj* %env$ae49233,%struct.ScmObj* %current_45args56421) {
%stackaddr$prim57884 = alloca %struct.ScmObj*, align 8
%k48428 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56421)
store volatile %struct.ScmObj* %k48428, %struct.ScmObj** %stackaddr$prim57884, align 8
%stackaddr$prim57885 = alloca %struct.ScmObj*, align 8
%current_45args56422 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56421)
store volatile %struct.ScmObj* %current_45args56422, %struct.ScmObj** %stackaddr$prim57885, align 8
%stackaddr$prim57886 = alloca %struct.ScmObj*, align 8
%x48069 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56422)
store volatile %struct.ScmObj* %x48069, %struct.ScmObj** %stackaddr$prim57886, align 8
%stackaddr$prim57887 = alloca %struct.ScmObj*, align 8
%cpsprim48429 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48069)
store volatile %struct.ScmObj* %cpsprim48429, %struct.ScmObj** %stackaddr$prim57887, align 8
%ae49236 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56424$k484280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57888 = alloca %struct.ScmObj*, align 8
%argslist56424$k484281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48429, %struct.ScmObj* %argslist56424$k484280)
store volatile %struct.ScmObj* %argslist56424$k484281, %struct.ScmObj** %stackaddr$prim57888, align 8
%stackaddr$prim57889 = alloca %struct.ScmObj*, align 8
%argslist56424$k484282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49236, %struct.ScmObj* %argslist56424$k484281)
store volatile %struct.ScmObj* %argslist56424$k484282, %struct.ScmObj** %stackaddr$prim57889, align 8
%clofunc57890 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48428)
musttail call tailcc void %clofunc57890(%struct.ScmObj* %k48428, %struct.ScmObj* %argslist56424$k484282)
ret void
}

define tailcc void @proc_clo$ae49185(%struct.ScmObj* %env$ae49185,%struct.ScmObj* %current_45args56427) {
%stackaddr$prim57891 = alloca %struct.ScmObj*, align 8
%k48430 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56427)
store volatile %struct.ScmObj* %k48430, %struct.ScmObj** %stackaddr$prim57891, align 8
%stackaddr$prim57892 = alloca %struct.ScmObj*, align 8
%current_45args56428 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56427)
store volatile %struct.ScmObj* %current_45args56428, %struct.ScmObj** %stackaddr$prim57892, align 8
%stackaddr$prim57893 = alloca %struct.ScmObj*, align 8
%lst48065 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56428)
store volatile %struct.ScmObj* %lst48065, %struct.ScmObj** %stackaddr$prim57893, align 8
%stackaddr$prim57894 = alloca %struct.ScmObj*, align 8
%current_45args56429 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56428)
store volatile %struct.ScmObj* %current_45args56429, %struct.ScmObj** %stackaddr$prim57894, align 8
%stackaddr$prim57895 = alloca %struct.ScmObj*, align 8
%b48064 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56429)
store volatile %struct.ScmObj* %b48064, %struct.ScmObj** %stackaddr$prim57895, align 8
%truthy$cmp57896 = call i64 @is_truthy_value(%struct.ScmObj* %b48064)
%cmp$cmp57896 = icmp eq i64 %truthy$cmp57896, 1
br i1 %cmp$cmp57896, label %truebranch$cmp57896, label %falsebranch$cmp57896
truebranch$cmp57896:
%ae49188 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56431$k484300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57897 = alloca %struct.ScmObj*, align 8
%argslist56431$k484301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b48064, %struct.ScmObj* %argslist56431$k484300)
store volatile %struct.ScmObj* %argslist56431$k484301, %struct.ScmObj** %stackaddr$prim57897, align 8
%stackaddr$prim57898 = alloca %struct.ScmObj*, align 8
%argslist56431$k484302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49188, %struct.ScmObj* %argslist56431$k484301)
store volatile %struct.ScmObj* %argslist56431$k484302, %struct.ScmObj** %stackaddr$prim57898, align 8
%clofunc57899 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48430)
musttail call tailcc void %clofunc57899(%struct.ScmObj* %k48430, %struct.ScmObj* %argslist56431$k484302)
ret void
falsebranch$cmp57896:
%stackaddr$prim57900 = alloca %struct.ScmObj*, align 8
%cpsprim48431 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48065)
store volatile %struct.ScmObj* %cpsprim48431, %struct.ScmObj** %stackaddr$prim57900, align 8
%ae49195 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56432$k484300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57901 = alloca %struct.ScmObj*, align 8
%argslist56432$k484301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48431, %struct.ScmObj* %argslist56432$k484300)
store volatile %struct.ScmObj* %argslist56432$k484301, %struct.ScmObj** %stackaddr$prim57901, align 8
%stackaddr$prim57902 = alloca %struct.ScmObj*, align 8
%argslist56432$k484302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49195, %struct.ScmObj* %argslist56432$k484301)
store volatile %struct.ScmObj* %argslist56432$k484302, %struct.ScmObj** %stackaddr$prim57902, align 8
%clofunc57903 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48430)
musttail call tailcc void %clofunc57903(%struct.ScmObj* %k48430, %struct.ScmObj* %argslist56432$k484302)
ret void
}

define tailcc void @proc_clo$ae49142(%struct.ScmObj* %env$ae49142,%struct.ScmObj* %current_45args56436) {
%stackaddr$env-ref57904 = alloca %struct.ScmObj*, align 8
%_37take48045 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49142, i64 0)
store %struct.ScmObj* %_37take48045, %struct.ScmObj** %stackaddr$env-ref57904
%stackaddr$env-ref57905 = alloca %struct.ScmObj*, align 8
%_37length48042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49142, i64 1)
store %struct.ScmObj* %_37length48042, %struct.ScmObj** %stackaddr$env-ref57905
%stackaddr$prim57906 = alloca %struct.ScmObj*, align 8
%k48432 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56436)
store volatile %struct.ScmObj* %k48432, %struct.ScmObj** %stackaddr$prim57906, align 8
%stackaddr$prim57907 = alloca %struct.ScmObj*, align 8
%current_45args56437 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56436)
store volatile %struct.ScmObj* %current_45args56437, %struct.ScmObj** %stackaddr$prim57907, align 8
%stackaddr$prim57908 = alloca %struct.ScmObj*, align 8
%lst48074 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56437)
store volatile %struct.ScmObj* %lst48074, %struct.ScmObj** %stackaddr$prim57908, align 8
%stackaddr$prim57909 = alloca %struct.ScmObj*, align 8
%current_45args56438 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56437)
store volatile %struct.ScmObj* %current_45args56438, %struct.ScmObj** %stackaddr$prim57909, align 8
%stackaddr$prim57910 = alloca %struct.ScmObj*, align 8
%n48073 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56438)
store volatile %struct.ScmObj* %n48073, %struct.ScmObj** %stackaddr$prim57910, align 8
%stackaddr$makeclosure57911 = alloca %struct.ScmObj*, align 8
%fptrToInt57912 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49144 to i64
%ae49144 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57912)
store volatile %struct.ScmObj* %ae49144, %struct.ScmObj** %stackaddr$makeclosure57911, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49144, %struct.ScmObj* %n48073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49144, %struct.ScmObj* %k48432, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49144, %struct.ScmObj* %_37take48045, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49144, %struct.ScmObj* %lst48074, i64 3)
%argslist56444$_37length480420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57913 = alloca %struct.ScmObj*, align 8
%argslist56444$_37length480421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48074, %struct.ScmObj* %argslist56444$_37length480420)
store volatile %struct.ScmObj* %argslist56444$_37length480421, %struct.ScmObj** %stackaddr$prim57913, align 8
%stackaddr$prim57914 = alloca %struct.ScmObj*, align 8
%argslist56444$_37length480422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49144, %struct.ScmObj* %argslist56444$_37length480421)
store volatile %struct.ScmObj* %argslist56444$_37length480422, %struct.ScmObj** %stackaddr$prim57914, align 8
%clofunc57915 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length48042)
musttail call tailcc void %clofunc57915(%struct.ScmObj* %_37length48042, %struct.ScmObj* %argslist56444$_37length480422)
ret void
}

define tailcc void @proc_clo$ae49144(%struct.ScmObj* %env$ae49144,%struct.ScmObj* %current_45args56440) {
%stackaddr$env-ref57916 = alloca %struct.ScmObj*, align 8
%n48073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49144, i64 0)
store %struct.ScmObj* %n48073, %struct.ScmObj** %stackaddr$env-ref57916
%stackaddr$env-ref57917 = alloca %struct.ScmObj*, align 8
%k48432 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49144, i64 1)
store %struct.ScmObj* %k48432, %struct.ScmObj** %stackaddr$env-ref57917
%stackaddr$env-ref57918 = alloca %struct.ScmObj*, align 8
%_37take48045 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49144, i64 2)
store %struct.ScmObj* %_37take48045, %struct.ScmObj** %stackaddr$env-ref57918
%stackaddr$env-ref57919 = alloca %struct.ScmObj*, align 8
%lst48074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49144, i64 3)
store %struct.ScmObj* %lst48074, %struct.ScmObj** %stackaddr$env-ref57919
%stackaddr$prim57920 = alloca %struct.ScmObj*, align 8
%_95k48433 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56440)
store volatile %struct.ScmObj* %_95k48433, %struct.ScmObj** %stackaddr$prim57920, align 8
%stackaddr$prim57921 = alloca %struct.ScmObj*, align 8
%current_45args56441 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56440)
store volatile %struct.ScmObj* %current_45args56441, %struct.ScmObj** %stackaddr$prim57921, align 8
%stackaddr$prim57922 = alloca %struct.ScmObj*, align 8
%anf_45bind48200 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56441)
store volatile %struct.ScmObj* %anf_45bind48200, %struct.ScmObj** %stackaddr$prim57922, align 8
%stackaddr$prim57923 = alloca %struct.ScmObj*, align 8
%anf_45bind48201 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind48200, %struct.ScmObj* %n48073)
store volatile %struct.ScmObj* %anf_45bind48201, %struct.ScmObj** %stackaddr$prim57923, align 8
%argslist56443$_37take480450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57924 = alloca %struct.ScmObj*, align 8
%argslist56443$_37take480451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48201, %struct.ScmObj* %argslist56443$_37take480450)
store volatile %struct.ScmObj* %argslist56443$_37take480451, %struct.ScmObj** %stackaddr$prim57924, align 8
%stackaddr$prim57925 = alloca %struct.ScmObj*, align 8
%argslist56443$_37take480452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48074, %struct.ScmObj* %argslist56443$_37take480451)
store volatile %struct.ScmObj* %argslist56443$_37take480452, %struct.ScmObj** %stackaddr$prim57925, align 8
%stackaddr$prim57926 = alloca %struct.ScmObj*, align 8
%argslist56443$_37take480453 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48432, %struct.ScmObj* %argslist56443$_37take480452)
store volatile %struct.ScmObj* %argslist56443$_37take480453, %struct.ScmObj** %stackaddr$prim57926, align 8
%clofunc57927 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take48045)
musttail call tailcc void %clofunc57927(%struct.ScmObj* %_37take48045, %struct.ScmObj* %argslist56443$_37take480453)
ret void
}

define tailcc void @proc_clo$ae49088(%struct.ScmObj* %env$ae49088,%struct.ScmObj* %current_45args56446) {
%stackaddr$env-ref57928 = alloca %struct.ScmObj*, align 8
%_37foldl148037 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49088, i64 0)
store %struct.ScmObj* %_37foldl148037, %struct.ScmObj** %stackaddr$env-ref57928
%stackaddr$prim57929 = alloca %struct.ScmObj*, align 8
%k48434 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56446)
store volatile %struct.ScmObj* %k48434, %struct.ScmObj** %stackaddr$prim57929, align 8
%stackaddr$prim57930 = alloca %struct.ScmObj*, align 8
%current_45args56447 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56446)
store volatile %struct.ScmObj* %current_45args56447, %struct.ScmObj** %stackaddr$prim57930, align 8
%stackaddr$prim57931 = alloca %struct.ScmObj*, align 8
%lst48076 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56447)
store volatile %struct.ScmObj* %lst48076, %struct.ScmObj** %stackaddr$prim57931, align 8
%stackaddr$makeclosure57932 = alloca %struct.ScmObj*, align 8
%fptrToInt57933 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49089 to i64
%ae49089 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57933)
store volatile %struct.ScmObj* %ae49089, %struct.ScmObj** %stackaddr$makeclosure57932, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49089, %struct.ScmObj* %_37foldl148037, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49089, %struct.ScmObj* %k48434, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49089, %struct.ScmObj* %lst48076, i64 2)
%ae49090 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57934 = alloca %struct.ScmObj*, align 8
%fptrToInt57935 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49091 to i64
%ae49091 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57935)
store volatile %struct.ScmObj* %ae49091, %struct.ScmObj** %stackaddr$makeclosure57934, align 8
%argslist56458$ae490890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57936 = alloca %struct.ScmObj*, align 8
%argslist56458$ae490891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49091, %struct.ScmObj* %argslist56458$ae490890)
store volatile %struct.ScmObj* %argslist56458$ae490891, %struct.ScmObj** %stackaddr$prim57936, align 8
%stackaddr$prim57937 = alloca %struct.ScmObj*, align 8
%argslist56458$ae490892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49090, %struct.ScmObj* %argslist56458$ae490891)
store volatile %struct.ScmObj* %argslist56458$ae490892, %struct.ScmObj** %stackaddr$prim57937, align 8
%clofunc57938 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49089)
musttail call tailcc void %clofunc57938(%struct.ScmObj* %ae49089, %struct.ScmObj* %argslist56458$ae490892)
ret void
}

define tailcc void @proc_clo$ae49089(%struct.ScmObj* %env$ae49089,%struct.ScmObj* %current_45args56449) {
%stackaddr$env-ref57939 = alloca %struct.ScmObj*, align 8
%_37foldl148037 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49089, i64 0)
store %struct.ScmObj* %_37foldl148037, %struct.ScmObj** %stackaddr$env-ref57939
%stackaddr$env-ref57940 = alloca %struct.ScmObj*, align 8
%k48434 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49089, i64 1)
store %struct.ScmObj* %k48434, %struct.ScmObj** %stackaddr$env-ref57940
%stackaddr$env-ref57941 = alloca %struct.ScmObj*, align 8
%lst48076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49089, i64 2)
store %struct.ScmObj* %lst48076, %struct.ScmObj** %stackaddr$env-ref57941
%stackaddr$prim57942 = alloca %struct.ScmObj*, align 8
%_95k48435 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56449)
store volatile %struct.ScmObj* %_95k48435, %struct.ScmObj** %stackaddr$prim57942, align 8
%stackaddr$prim57943 = alloca %struct.ScmObj*, align 8
%current_45args56450 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56449)
store volatile %struct.ScmObj* %current_45args56450, %struct.ScmObj** %stackaddr$prim57943, align 8
%stackaddr$prim57944 = alloca %struct.ScmObj*, align 8
%anf_45bind48199 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56450)
store volatile %struct.ScmObj* %anf_45bind48199, %struct.ScmObj** %stackaddr$prim57944, align 8
%ae49110 = call %struct.ScmObj* @const_init_null()
%argslist56452$_37foldl1480370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57945 = alloca %struct.ScmObj*, align 8
%argslist56452$_37foldl1480371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48076, %struct.ScmObj* %argslist56452$_37foldl1480370)
store volatile %struct.ScmObj* %argslist56452$_37foldl1480371, %struct.ScmObj** %stackaddr$prim57945, align 8
%stackaddr$prim57946 = alloca %struct.ScmObj*, align 8
%argslist56452$_37foldl1480372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49110, %struct.ScmObj* %argslist56452$_37foldl1480371)
store volatile %struct.ScmObj* %argslist56452$_37foldl1480372, %struct.ScmObj** %stackaddr$prim57946, align 8
%stackaddr$prim57947 = alloca %struct.ScmObj*, align 8
%argslist56452$_37foldl1480373 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48199, %struct.ScmObj* %argslist56452$_37foldl1480372)
store volatile %struct.ScmObj* %argslist56452$_37foldl1480373, %struct.ScmObj** %stackaddr$prim57947, align 8
%stackaddr$prim57948 = alloca %struct.ScmObj*, align 8
%argslist56452$_37foldl1480374 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48434, %struct.ScmObj* %argslist56452$_37foldl1480373)
store volatile %struct.ScmObj* %argslist56452$_37foldl1480374, %struct.ScmObj** %stackaddr$prim57948, align 8
%clofunc57949 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148037)
musttail call tailcc void %clofunc57949(%struct.ScmObj* %_37foldl148037, %struct.ScmObj* %argslist56452$_37foldl1480374)
ret void
}

define tailcc void @proc_clo$ae49091(%struct.ScmObj* %env$ae49091,%struct.ScmObj* %current_45args56453) {
%stackaddr$prim57950 = alloca %struct.ScmObj*, align 8
%k48436 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56453)
store volatile %struct.ScmObj* %k48436, %struct.ScmObj** %stackaddr$prim57950, align 8
%stackaddr$prim57951 = alloca %struct.ScmObj*, align 8
%current_45args56454 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56453)
store volatile %struct.ScmObj* %current_45args56454, %struct.ScmObj** %stackaddr$prim57951, align 8
%stackaddr$prim57952 = alloca %struct.ScmObj*, align 8
%x48078 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56454)
store volatile %struct.ScmObj* %x48078, %struct.ScmObj** %stackaddr$prim57952, align 8
%stackaddr$prim57953 = alloca %struct.ScmObj*, align 8
%current_45args56455 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56454)
store volatile %struct.ScmObj* %current_45args56455, %struct.ScmObj** %stackaddr$prim57953, align 8
%stackaddr$prim57954 = alloca %struct.ScmObj*, align 8
%y48077 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56455)
store volatile %struct.ScmObj* %y48077, %struct.ScmObj** %stackaddr$prim57954, align 8
%ae49093 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56457$k484360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57955 = alloca %struct.ScmObj*, align 8
%argslist56457$k484361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x48078, %struct.ScmObj* %argslist56457$k484360)
store volatile %struct.ScmObj* %argslist56457$k484361, %struct.ScmObj** %stackaddr$prim57955, align 8
%stackaddr$prim57956 = alloca %struct.ScmObj*, align 8
%argslist56457$k484362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49093, %struct.ScmObj* %argslist56457$k484361)
store volatile %struct.ScmObj* %argslist56457$k484362, %struct.ScmObj** %stackaddr$prim57956, align 8
%clofunc57957 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48436)
musttail call tailcc void %clofunc57957(%struct.ScmObj* %k48436, %struct.ScmObj* %argslist56457$k484362)
ret void
}

define tailcc void @proc_clo$ae49009(%struct.ScmObj* %env$ae49009,%struct.ScmObj* %current_45args56461) {
%stackaddr$prim57958 = alloca %struct.ScmObj*, align 8
%k48437 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56461)
store volatile %struct.ScmObj* %k48437, %struct.ScmObj** %stackaddr$prim57958, align 8
%stackaddr$prim57959 = alloca %struct.ScmObj*, align 8
%current_45args56462 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56461)
store volatile %struct.ScmObj* %current_45args56462, %struct.ScmObj** %stackaddr$prim57959, align 8
%stackaddr$prim57960 = alloca %struct.ScmObj*, align 8
%_37foldl148038 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56462)
store volatile %struct.ScmObj* %_37foldl148038, %struct.ScmObj** %stackaddr$prim57960, align 8
%ae49011 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57961 = alloca %struct.ScmObj*, align 8
%fptrToInt57962 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49012 to i64
%ae49012 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57962)
store volatile %struct.ScmObj* %ae49012, %struct.ScmObj** %stackaddr$makeclosure57961, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49012, %struct.ScmObj* %_37foldl148038, i64 0)
%argslist56475$k484370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57963 = alloca %struct.ScmObj*, align 8
%argslist56475$k484371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49012, %struct.ScmObj* %argslist56475$k484370)
store volatile %struct.ScmObj* %argslist56475$k484371, %struct.ScmObj** %stackaddr$prim57963, align 8
%stackaddr$prim57964 = alloca %struct.ScmObj*, align 8
%argslist56475$k484372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49011, %struct.ScmObj* %argslist56475$k484371)
store volatile %struct.ScmObj* %argslist56475$k484372, %struct.ScmObj** %stackaddr$prim57964, align 8
%clofunc57965 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48437)
musttail call tailcc void %clofunc57965(%struct.ScmObj* %k48437, %struct.ScmObj* %argslist56475$k484372)
ret void
}

define tailcc void @proc_clo$ae49012(%struct.ScmObj* %env$ae49012,%struct.ScmObj* %current_45args56464) {
%stackaddr$env-ref57966 = alloca %struct.ScmObj*, align 8
%_37foldl148038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49012, i64 0)
store %struct.ScmObj* %_37foldl148038, %struct.ScmObj** %stackaddr$env-ref57966
%stackaddr$prim57967 = alloca %struct.ScmObj*, align 8
%k48438 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56464)
store volatile %struct.ScmObj* %k48438, %struct.ScmObj** %stackaddr$prim57967, align 8
%stackaddr$prim57968 = alloca %struct.ScmObj*, align 8
%current_45args56465 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56464)
store volatile %struct.ScmObj* %current_45args56465, %struct.ScmObj** %stackaddr$prim57968, align 8
%stackaddr$prim57969 = alloca %struct.ScmObj*, align 8
%f48041 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56465)
store volatile %struct.ScmObj* %f48041, %struct.ScmObj** %stackaddr$prim57969, align 8
%stackaddr$prim57970 = alloca %struct.ScmObj*, align 8
%current_45args56466 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56465)
store volatile %struct.ScmObj* %current_45args56466, %struct.ScmObj** %stackaddr$prim57970, align 8
%stackaddr$prim57971 = alloca %struct.ScmObj*, align 8
%acc48040 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56466)
store volatile %struct.ScmObj* %acc48040, %struct.ScmObj** %stackaddr$prim57971, align 8
%stackaddr$prim57972 = alloca %struct.ScmObj*, align 8
%current_45args56467 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56466)
store volatile %struct.ScmObj* %current_45args56467, %struct.ScmObj** %stackaddr$prim57972, align 8
%stackaddr$prim57973 = alloca %struct.ScmObj*, align 8
%lst48039 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56467)
store volatile %struct.ScmObj* %lst48039, %struct.ScmObj** %stackaddr$prim57973, align 8
%stackaddr$prim57974 = alloca %struct.ScmObj*, align 8
%anf_45bind48194 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48039)
store volatile %struct.ScmObj* %anf_45bind48194, %struct.ScmObj** %stackaddr$prim57974, align 8
%truthy$cmp57975 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48194)
%cmp$cmp57975 = icmp eq i64 %truthy$cmp57975, 1
br i1 %cmp$cmp57975, label %truebranch$cmp57975, label %falsebranch$cmp57975
truebranch$cmp57975:
%ae49016 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56469$k484380 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57976 = alloca %struct.ScmObj*, align 8
%argslist56469$k484381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48040, %struct.ScmObj* %argslist56469$k484380)
store volatile %struct.ScmObj* %argslist56469$k484381, %struct.ScmObj** %stackaddr$prim57976, align 8
%stackaddr$prim57977 = alloca %struct.ScmObj*, align 8
%argslist56469$k484382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49016, %struct.ScmObj* %argslist56469$k484381)
store volatile %struct.ScmObj* %argslist56469$k484382, %struct.ScmObj** %stackaddr$prim57977, align 8
%clofunc57978 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48438)
musttail call tailcc void %clofunc57978(%struct.ScmObj* %k48438, %struct.ScmObj* %argslist56469$k484382)
ret void
falsebranch$cmp57975:
%stackaddr$prim57979 = alloca %struct.ScmObj*, align 8
%anf_45bind48195 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48039)
store volatile %struct.ScmObj* %anf_45bind48195, %struct.ScmObj** %stackaddr$prim57979, align 8
%stackaddr$makeclosure57980 = alloca %struct.ScmObj*, align 8
%fptrToInt57981 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49023 to i64
%ae49023 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57981)
store volatile %struct.ScmObj* %ae49023, %struct.ScmObj** %stackaddr$makeclosure57980, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49023, %struct.ScmObj* %_37foldl148038, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49023, %struct.ScmObj* %k48438, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49023, %struct.ScmObj* %f48041, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49023, %struct.ScmObj* %lst48039, i64 3)
%argslist56474$f480410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57982 = alloca %struct.ScmObj*, align 8
%argslist56474$f480411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48040, %struct.ScmObj* %argslist56474$f480410)
store volatile %struct.ScmObj* %argslist56474$f480411, %struct.ScmObj** %stackaddr$prim57982, align 8
%stackaddr$prim57983 = alloca %struct.ScmObj*, align 8
%argslist56474$f480412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48195, %struct.ScmObj* %argslist56474$f480411)
store volatile %struct.ScmObj* %argslist56474$f480412, %struct.ScmObj** %stackaddr$prim57983, align 8
%stackaddr$prim57984 = alloca %struct.ScmObj*, align 8
%argslist56474$f480413 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49023, %struct.ScmObj* %argslist56474$f480412)
store volatile %struct.ScmObj* %argslist56474$f480413, %struct.ScmObj** %stackaddr$prim57984, align 8
%clofunc57985 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48041)
musttail call tailcc void %clofunc57985(%struct.ScmObj* %f48041, %struct.ScmObj* %argslist56474$f480413)
ret void
}

define tailcc void @proc_clo$ae49023(%struct.ScmObj* %env$ae49023,%struct.ScmObj* %current_45args56470) {
%stackaddr$env-ref57986 = alloca %struct.ScmObj*, align 8
%_37foldl148038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49023, i64 0)
store %struct.ScmObj* %_37foldl148038, %struct.ScmObj** %stackaddr$env-ref57986
%stackaddr$env-ref57987 = alloca %struct.ScmObj*, align 8
%k48438 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49023, i64 1)
store %struct.ScmObj* %k48438, %struct.ScmObj** %stackaddr$env-ref57987
%stackaddr$env-ref57988 = alloca %struct.ScmObj*, align 8
%f48041 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49023, i64 2)
store %struct.ScmObj* %f48041, %struct.ScmObj** %stackaddr$env-ref57988
%stackaddr$env-ref57989 = alloca %struct.ScmObj*, align 8
%lst48039 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49023, i64 3)
store %struct.ScmObj* %lst48039, %struct.ScmObj** %stackaddr$env-ref57989
%stackaddr$prim57990 = alloca %struct.ScmObj*, align 8
%_95k48439 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56470)
store volatile %struct.ScmObj* %_95k48439, %struct.ScmObj** %stackaddr$prim57990, align 8
%stackaddr$prim57991 = alloca %struct.ScmObj*, align 8
%current_45args56471 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56470)
store volatile %struct.ScmObj* %current_45args56471, %struct.ScmObj** %stackaddr$prim57991, align 8
%stackaddr$prim57992 = alloca %struct.ScmObj*, align 8
%anf_45bind48196 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56471)
store volatile %struct.ScmObj* %anf_45bind48196, %struct.ScmObj** %stackaddr$prim57992, align 8
%stackaddr$prim57993 = alloca %struct.ScmObj*, align 8
%anf_45bind48197 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48039)
store volatile %struct.ScmObj* %anf_45bind48197, %struct.ScmObj** %stackaddr$prim57993, align 8
%argslist56473$_37foldl1480380 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57994 = alloca %struct.ScmObj*, align 8
%argslist56473$_37foldl1480381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48197, %struct.ScmObj* %argslist56473$_37foldl1480380)
store volatile %struct.ScmObj* %argslist56473$_37foldl1480381, %struct.ScmObj** %stackaddr$prim57994, align 8
%stackaddr$prim57995 = alloca %struct.ScmObj*, align 8
%argslist56473$_37foldl1480382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48196, %struct.ScmObj* %argslist56473$_37foldl1480381)
store volatile %struct.ScmObj* %argslist56473$_37foldl1480382, %struct.ScmObj** %stackaddr$prim57995, align 8
%stackaddr$prim57996 = alloca %struct.ScmObj*, align 8
%argslist56473$_37foldl1480383 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48041, %struct.ScmObj* %argslist56473$_37foldl1480382)
store volatile %struct.ScmObj* %argslist56473$_37foldl1480383, %struct.ScmObj** %stackaddr$prim57996, align 8
%stackaddr$prim57997 = alloca %struct.ScmObj*, align 8
%argslist56473$_37foldl1480384 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48438, %struct.ScmObj* %argslist56473$_37foldl1480383)
store volatile %struct.ScmObj* %argslist56473$_37foldl1480384, %struct.ScmObj** %stackaddr$prim57997, align 8
%clofunc57998 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148038)
musttail call tailcc void %clofunc57998(%struct.ScmObj* %_37foldl148038, %struct.ScmObj* %argslist56473$_37foldl1480384)
ret void
}

define tailcc void @proc_clo$ae48926(%struct.ScmObj* %env$ae48926,%struct.ScmObj* %current_45args56478) {
%stackaddr$prim57999 = alloca %struct.ScmObj*, align 8
%k48440 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56478)
store volatile %struct.ScmObj* %k48440, %struct.ScmObj** %stackaddr$prim57999, align 8
%stackaddr$prim58000 = alloca %struct.ScmObj*, align 8
%current_45args56479 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56478)
store volatile %struct.ScmObj* %current_45args56479, %struct.ScmObj** %stackaddr$prim58000, align 8
%stackaddr$prim58001 = alloca %struct.ScmObj*, align 8
%_37length48043 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56479)
store volatile %struct.ScmObj* %_37length48043, %struct.ScmObj** %stackaddr$prim58001, align 8
%ae48928 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58002 = alloca %struct.ScmObj*, align 8
%fptrToInt58003 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48929 to i64
%ae48929 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58003)
store volatile %struct.ScmObj* %ae48929, %struct.ScmObj** %stackaddr$makeclosure58002, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48929, %struct.ScmObj* %_37length48043, i64 0)
%argslist56490$k484400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58004 = alloca %struct.ScmObj*, align 8
%argslist56490$k484401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48929, %struct.ScmObj* %argslist56490$k484400)
store volatile %struct.ScmObj* %argslist56490$k484401, %struct.ScmObj** %stackaddr$prim58004, align 8
%stackaddr$prim58005 = alloca %struct.ScmObj*, align 8
%argslist56490$k484402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48928, %struct.ScmObj* %argslist56490$k484401)
store volatile %struct.ScmObj* %argslist56490$k484402, %struct.ScmObj** %stackaddr$prim58005, align 8
%clofunc58006 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48440)
musttail call tailcc void %clofunc58006(%struct.ScmObj* %k48440, %struct.ScmObj* %argslist56490$k484402)
ret void
}

define tailcc void @proc_clo$ae48929(%struct.ScmObj* %env$ae48929,%struct.ScmObj* %current_45args56481) {
%stackaddr$env-ref58007 = alloca %struct.ScmObj*, align 8
%_37length48043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48929, i64 0)
store %struct.ScmObj* %_37length48043, %struct.ScmObj** %stackaddr$env-ref58007
%stackaddr$prim58008 = alloca %struct.ScmObj*, align 8
%k48441 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56481)
store volatile %struct.ScmObj* %k48441, %struct.ScmObj** %stackaddr$prim58008, align 8
%stackaddr$prim58009 = alloca %struct.ScmObj*, align 8
%current_45args56482 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56481)
store volatile %struct.ScmObj* %current_45args56482, %struct.ScmObj** %stackaddr$prim58009, align 8
%stackaddr$prim58010 = alloca %struct.ScmObj*, align 8
%lst48044 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56482)
store volatile %struct.ScmObj* %lst48044, %struct.ScmObj** %stackaddr$prim58010, align 8
%stackaddr$prim58011 = alloca %struct.ScmObj*, align 8
%anf_45bind48190 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48044)
store volatile %struct.ScmObj* %anf_45bind48190, %struct.ScmObj** %stackaddr$prim58011, align 8
%truthy$cmp58012 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48190)
%cmp$cmp58012 = icmp eq i64 %truthy$cmp58012, 1
br i1 %cmp$cmp58012, label %truebranch$cmp58012, label %falsebranch$cmp58012
truebranch$cmp58012:
%ae48933 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48934 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56484$k484410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58013 = alloca %struct.ScmObj*, align 8
%argslist56484$k484411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48934, %struct.ScmObj* %argslist56484$k484410)
store volatile %struct.ScmObj* %argslist56484$k484411, %struct.ScmObj** %stackaddr$prim58013, align 8
%stackaddr$prim58014 = alloca %struct.ScmObj*, align 8
%argslist56484$k484412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48933, %struct.ScmObj* %argslist56484$k484411)
store volatile %struct.ScmObj* %argslist56484$k484412, %struct.ScmObj** %stackaddr$prim58014, align 8
%clofunc58015 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48441)
musttail call tailcc void %clofunc58015(%struct.ScmObj* %k48441, %struct.ScmObj* %argslist56484$k484412)
ret void
falsebranch$cmp58012:
%stackaddr$prim58016 = alloca %struct.ScmObj*, align 8
%anf_45bind48191 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48044)
store volatile %struct.ScmObj* %anf_45bind48191, %struct.ScmObj** %stackaddr$prim58016, align 8
%stackaddr$makeclosure58017 = alloca %struct.ScmObj*, align 8
%fptrToInt58018 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48943 to i64
%ae48943 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58018)
store volatile %struct.ScmObj* %ae48943, %struct.ScmObj** %stackaddr$makeclosure58017, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48943, %struct.ScmObj* %k48441, i64 0)
%argslist56489$_37length480430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58019 = alloca %struct.ScmObj*, align 8
%argslist56489$_37length480431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48191, %struct.ScmObj* %argslist56489$_37length480430)
store volatile %struct.ScmObj* %argslist56489$_37length480431, %struct.ScmObj** %stackaddr$prim58019, align 8
%stackaddr$prim58020 = alloca %struct.ScmObj*, align 8
%argslist56489$_37length480432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48943, %struct.ScmObj* %argslist56489$_37length480431)
store volatile %struct.ScmObj* %argslist56489$_37length480432, %struct.ScmObj** %stackaddr$prim58020, align 8
%clofunc58021 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length48043)
musttail call tailcc void %clofunc58021(%struct.ScmObj* %_37length48043, %struct.ScmObj* %argslist56489$_37length480432)
ret void
}

define tailcc void @proc_clo$ae48943(%struct.ScmObj* %env$ae48943,%struct.ScmObj* %current_45args56485) {
%stackaddr$env-ref58022 = alloca %struct.ScmObj*, align 8
%k48441 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48943, i64 0)
store %struct.ScmObj* %k48441, %struct.ScmObj** %stackaddr$env-ref58022
%stackaddr$prim58023 = alloca %struct.ScmObj*, align 8
%_95k48442 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56485)
store volatile %struct.ScmObj* %_95k48442, %struct.ScmObj** %stackaddr$prim58023, align 8
%stackaddr$prim58024 = alloca %struct.ScmObj*, align 8
%current_45args56486 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56485)
store volatile %struct.ScmObj* %current_45args56486, %struct.ScmObj** %stackaddr$prim58024, align 8
%stackaddr$prim58025 = alloca %struct.ScmObj*, align 8
%anf_45bind48192 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56486)
store volatile %struct.ScmObj* %anf_45bind48192, %struct.ScmObj** %stackaddr$prim58025, align 8
%ae48945 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim58026 = alloca %struct.ScmObj*, align 8
%cpsprim48443 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae48945, %struct.ScmObj* %anf_45bind48192)
store volatile %struct.ScmObj* %cpsprim48443, %struct.ScmObj** %stackaddr$prim58026, align 8
%ae48948 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56488$k484410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58027 = alloca %struct.ScmObj*, align 8
%argslist56488$k484411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48443, %struct.ScmObj* %argslist56488$k484410)
store volatile %struct.ScmObj* %argslist56488$k484411, %struct.ScmObj** %stackaddr$prim58027, align 8
%stackaddr$prim58028 = alloca %struct.ScmObj*, align 8
%argslist56488$k484412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48948, %struct.ScmObj* %argslist56488$k484411)
store volatile %struct.ScmObj* %argslist56488$k484412, %struct.ScmObj** %stackaddr$prim58028, align 8
%clofunc58029 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48441)
musttail call tailcc void %clofunc58029(%struct.ScmObj* %k48441, %struct.ScmObj* %argslist56488$k484412)
ret void
}

define tailcc void @proc_clo$ae48776(%struct.ScmObj* %env$ae48776,%struct.ScmObj* %current_45args56493) {
%stackaddr$prim58030 = alloca %struct.ScmObj*, align 8
%k48444 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56493)
store volatile %struct.ScmObj* %k48444, %struct.ScmObj** %stackaddr$prim58030, align 8
%stackaddr$prim58031 = alloca %struct.ScmObj*, align 8
%current_45args56494 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56493)
store volatile %struct.ScmObj* %current_45args56494, %struct.ScmObj** %stackaddr$prim58031, align 8
%stackaddr$prim58032 = alloca %struct.ScmObj*, align 8
%_37take48046 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56494)
store volatile %struct.ScmObj* %_37take48046, %struct.ScmObj** %stackaddr$prim58032, align 8
%ae48778 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58033 = alloca %struct.ScmObj*, align 8
%fptrToInt58034 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48779 to i64
%ae48779 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58034)
store volatile %struct.ScmObj* %ae48779, %struct.ScmObj** %stackaddr$makeclosure58033, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48779, %struct.ScmObj* %_37take48046, i64 0)
%argslist56507$k484440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58035 = alloca %struct.ScmObj*, align 8
%argslist56507$k484441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48779, %struct.ScmObj* %argslist56507$k484440)
store volatile %struct.ScmObj* %argslist56507$k484441, %struct.ScmObj** %stackaddr$prim58035, align 8
%stackaddr$prim58036 = alloca %struct.ScmObj*, align 8
%argslist56507$k484442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48778, %struct.ScmObj* %argslist56507$k484441)
store volatile %struct.ScmObj* %argslist56507$k484442, %struct.ScmObj** %stackaddr$prim58036, align 8
%clofunc58037 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48444)
musttail call tailcc void %clofunc58037(%struct.ScmObj* %k48444, %struct.ScmObj* %argslist56507$k484442)
ret void
}

define tailcc void @proc_clo$ae48779(%struct.ScmObj* %env$ae48779,%struct.ScmObj* %current_45args56496) {
%stackaddr$env-ref58038 = alloca %struct.ScmObj*, align 8
%_37take48046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48779, i64 0)
store %struct.ScmObj* %_37take48046, %struct.ScmObj** %stackaddr$env-ref58038
%stackaddr$prim58039 = alloca %struct.ScmObj*, align 8
%k48445 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56496)
store volatile %struct.ScmObj* %k48445, %struct.ScmObj** %stackaddr$prim58039, align 8
%stackaddr$prim58040 = alloca %struct.ScmObj*, align 8
%current_45args56497 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56496)
store volatile %struct.ScmObj* %current_45args56497, %struct.ScmObj** %stackaddr$prim58040, align 8
%stackaddr$prim58041 = alloca %struct.ScmObj*, align 8
%lst48048 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56497)
store volatile %struct.ScmObj* %lst48048, %struct.ScmObj** %stackaddr$prim58041, align 8
%stackaddr$prim58042 = alloca %struct.ScmObj*, align 8
%current_45args56498 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56497)
store volatile %struct.ScmObj* %current_45args56498, %struct.ScmObj** %stackaddr$prim58042, align 8
%stackaddr$prim58043 = alloca %struct.ScmObj*, align 8
%n48047 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56498)
store volatile %struct.ScmObj* %n48047, %struct.ScmObj** %stackaddr$prim58043, align 8
%ae48781 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58044 = alloca %struct.ScmObj*, align 8
%anf_45bind48183 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n48047, %struct.ScmObj* %ae48781)
store volatile %struct.ScmObj* %anf_45bind48183, %struct.ScmObj** %stackaddr$prim58044, align 8
%truthy$cmp58045 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48183)
%cmp$cmp58045 = icmp eq i64 %truthy$cmp58045, 1
br i1 %cmp$cmp58045, label %truebranch$cmp58045, label %falsebranch$cmp58045
truebranch$cmp58045:
%ae48784 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48785 = call %struct.ScmObj* @const_init_null()
%argslist56500$k484450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58046 = alloca %struct.ScmObj*, align 8
%argslist56500$k484451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48785, %struct.ScmObj* %argslist56500$k484450)
store volatile %struct.ScmObj* %argslist56500$k484451, %struct.ScmObj** %stackaddr$prim58046, align 8
%stackaddr$prim58047 = alloca %struct.ScmObj*, align 8
%argslist56500$k484452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48784, %struct.ScmObj* %argslist56500$k484451)
store volatile %struct.ScmObj* %argslist56500$k484452, %struct.ScmObj** %stackaddr$prim58047, align 8
%clofunc58048 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48445)
musttail call tailcc void %clofunc58048(%struct.ScmObj* %k48445, %struct.ScmObj* %argslist56500$k484452)
ret void
falsebranch$cmp58045:
%stackaddr$prim58049 = alloca %struct.ScmObj*, align 8
%anf_45bind48184 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48048)
store volatile %struct.ScmObj* %anf_45bind48184, %struct.ScmObj** %stackaddr$prim58049, align 8
%truthy$cmp58050 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48184)
%cmp$cmp58050 = icmp eq i64 %truthy$cmp58050, 1
br i1 %cmp$cmp58050, label %truebranch$cmp58050, label %falsebranch$cmp58050
truebranch$cmp58050:
%ae48795 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48796 = call %struct.ScmObj* @const_init_null()
%argslist56501$k484450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58051 = alloca %struct.ScmObj*, align 8
%argslist56501$k484451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48796, %struct.ScmObj* %argslist56501$k484450)
store volatile %struct.ScmObj* %argslist56501$k484451, %struct.ScmObj** %stackaddr$prim58051, align 8
%stackaddr$prim58052 = alloca %struct.ScmObj*, align 8
%argslist56501$k484452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48795, %struct.ScmObj* %argslist56501$k484451)
store volatile %struct.ScmObj* %argslist56501$k484452, %struct.ScmObj** %stackaddr$prim58052, align 8
%clofunc58053 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48445)
musttail call tailcc void %clofunc58053(%struct.ScmObj* %k48445, %struct.ScmObj* %argslist56501$k484452)
ret void
falsebranch$cmp58050:
%stackaddr$prim58054 = alloca %struct.ScmObj*, align 8
%anf_45bind48185 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48048)
store volatile %struct.ScmObj* %anf_45bind48185, %struct.ScmObj** %stackaddr$prim58054, align 8
%stackaddr$prim58055 = alloca %struct.ScmObj*, align 8
%anf_45bind48186 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48048)
store volatile %struct.ScmObj* %anf_45bind48186, %struct.ScmObj** %stackaddr$prim58055, align 8
%ae48806 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim58056 = alloca %struct.ScmObj*, align 8
%anf_45bind48187 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n48047, %struct.ScmObj* %ae48806)
store volatile %struct.ScmObj* %anf_45bind48187, %struct.ScmObj** %stackaddr$prim58056, align 8
%stackaddr$makeclosure58057 = alloca %struct.ScmObj*, align 8
%fptrToInt58058 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48808 to i64
%ae48808 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58058)
store volatile %struct.ScmObj* %ae48808, %struct.ScmObj** %stackaddr$makeclosure58057, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48808, %struct.ScmObj* %anf_45bind48185, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48808, %struct.ScmObj* %k48445, i64 1)
%argslist56506$_37take480460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58059 = alloca %struct.ScmObj*, align 8
%argslist56506$_37take480461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48187, %struct.ScmObj* %argslist56506$_37take480460)
store volatile %struct.ScmObj* %argslist56506$_37take480461, %struct.ScmObj** %stackaddr$prim58059, align 8
%stackaddr$prim58060 = alloca %struct.ScmObj*, align 8
%argslist56506$_37take480462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48186, %struct.ScmObj* %argslist56506$_37take480461)
store volatile %struct.ScmObj* %argslist56506$_37take480462, %struct.ScmObj** %stackaddr$prim58060, align 8
%stackaddr$prim58061 = alloca %struct.ScmObj*, align 8
%argslist56506$_37take480463 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48808, %struct.ScmObj* %argslist56506$_37take480462)
store volatile %struct.ScmObj* %argslist56506$_37take480463, %struct.ScmObj** %stackaddr$prim58061, align 8
%clofunc58062 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take48046)
musttail call tailcc void %clofunc58062(%struct.ScmObj* %_37take48046, %struct.ScmObj* %argslist56506$_37take480463)
ret void
}

define tailcc void @proc_clo$ae48808(%struct.ScmObj* %env$ae48808,%struct.ScmObj* %current_45args56502) {
%stackaddr$env-ref58063 = alloca %struct.ScmObj*, align 8
%anf_45bind48185 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48808, i64 0)
store %struct.ScmObj* %anf_45bind48185, %struct.ScmObj** %stackaddr$env-ref58063
%stackaddr$env-ref58064 = alloca %struct.ScmObj*, align 8
%k48445 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48808, i64 1)
store %struct.ScmObj* %k48445, %struct.ScmObj** %stackaddr$env-ref58064
%stackaddr$prim58065 = alloca %struct.ScmObj*, align 8
%_95k48446 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56502)
store volatile %struct.ScmObj* %_95k48446, %struct.ScmObj** %stackaddr$prim58065, align 8
%stackaddr$prim58066 = alloca %struct.ScmObj*, align 8
%current_45args56503 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56502)
store volatile %struct.ScmObj* %current_45args56503, %struct.ScmObj** %stackaddr$prim58066, align 8
%stackaddr$prim58067 = alloca %struct.ScmObj*, align 8
%anf_45bind48188 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56503)
store volatile %struct.ScmObj* %anf_45bind48188, %struct.ScmObj** %stackaddr$prim58067, align 8
%stackaddr$prim58068 = alloca %struct.ScmObj*, align 8
%cpsprim48447 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48185, %struct.ScmObj* %anf_45bind48188)
store volatile %struct.ScmObj* %cpsprim48447, %struct.ScmObj** %stackaddr$prim58068, align 8
%ae48814 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56505$k484450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58069 = alloca %struct.ScmObj*, align 8
%argslist56505$k484451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48447, %struct.ScmObj* %argslist56505$k484450)
store volatile %struct.ScmObj* %argslist56505$k484451, %struct.ScmObj** %stackaddr$prim58069, align 8
%stackaddr$prim58070 = alloca %struct.ScmObj*, align 8
%argslist56505$k484452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48814, %struct.ScmObj* %argslist56505$k484451)
store volatile %struct.ScmObj* %argslist56505$k484452, %struct.ScmObj** %stackaddr$prim58070, align 8
%clofunc58071 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48445)
musttail call tailcc void %clofunc58071(%struct.ScmObj* %k48445, %struct.ScmObj* %argslist56505$k484452)
ret void
}

define tailcc void @proc_clo$ae48679(%struct.ScmObj* %env$ae48679,%struct.ScmObj* %current_45args56510) {
%stackaddr$prim58072 = alloca %struct.ScmObj*, align 8
%k48448 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56510)
store volatile %struct.ScmObj* %k48448, %struct.ScmObj** %stackaddr$prim58072, align 8
%stackaddr$prim58073 = alloca %struct.ScmObj*, align 8
%current_45args56511 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56510)
store volatile %struct.ScmObj* %current_45args56511, %struct.ScmObj** %stackaddr$prim58073, align 8
%stackaddr$prim58074 = alloca %struct.ScmObj*, align 8
%_37map48050 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56511)
store volatile %struct.ScmObj* %_37map48050, %struct.ScmObj** %stackaddr$prim58074, align 8
%ae48681 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58075 = alloca %struct.ScmObj*, align 8
%fptrToInt58076 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48682 to i64
%ae48682 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58076)
store volatile %struct.ScmObj* %ae48682, %struct.ScmObj** %stackaddr$makeclosure58075, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48682, %struct.ScmObj* %_37map48050, i64 0)
%argslist56527$k484480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58077 = alloca %struct.ScmObj*, align 8
%argslist56527$k484481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48682, %struct.ScmObj* %argslist56527$k484480)
store volatile %struct.ScmObj* %argslist56527$k484481, %struct.ScmObj** %stackaddr$prim58077, align 8
%stackaddr$prim58078 = alloca %struct.ScmObj*, align 8
%argslist56527$k484482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48681, %struct.ScmObj* %argslist56527$k484481)
store volatile %struct.ScmObj* %argslist56527$k484482, %struct.ScmObj** %stackaddr$prim58078, align 8
%clofunc58079 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48448)
musttail call tailcc void %clofunc58079(%struct.ScmObj* %k48448, %struct.ScmObj* %argslist56527$k484482)
ret void
}

define tailcc void @proc_clo$ae48682(%struct.ScmObj* %env$ae48682,%struct.ScmObj* %current_45args56513) {
%stackaddr$env-ref58080 = alloca %struct.ScmObj*, align 8
%_37map48050 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48682, i64 0)
store %struct.ScmObj* %_37map48050, %struct.ScmObj** %stackaddr$env-ref58080
%stackaddr$prim58081 = alloca %struct.ScmObj*, align 8
%k48449 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56513)
store volatile %struct.ScmObj* %k48449, %struct.ScmObj** %stackaddr$prim58081, align 8
%stackaddr$prim58082 = alloca %struct.ScmObj*, align 8
%current_45args56514 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56513)
store volatile %struct.ScmObj* %current_45args56514, %struct.ScmObj** %stackaddr$prim58082, align 8
%stackaddr$prim58083 = alloca %struct.ScmObj*, align 8
%f48052 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56514)
store volatile %struct.ScmObj* %f48052, %struct.ScmObj** %stackaddr$prim58083, align 8
%stackaddr$prim58084 = alloca %struct.ScmObj*, align 8
%current_45args56515 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56514)
store volatile %struct.ScmObj* %current_45args56515, %struct.ScmObj** %stackaddr$prim58084, align 8
%stackaddr$prim58085 = alloca %struct.ScmObj*, align 8
%lst48051 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56515)
store volatile %struct.ScmObj* %lst48051, %struct.ScmObj** %stackaddr$prim58085, align 8
%stackaddr$prim58086 = alloca %struct.ScmObj*, align 8
%anf_45bind48177 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48051)
store volatile %struct.ScmObj* %anf_45bind48177, %struct.ScmObj** %stackaddr$prim58086, align 8
%truthy$cmp58087 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48177)
%cmp$cmp58087 = icmp eq i64 %truthy$cmp58087, 1
br i1 %cmp$cmp58087, label %truebranch$cmp58087, label %falsebranch$cmp58087
truebranch$cmp58087:
%ae48686 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48687 = call %struct.ScmObj* @const_init_null()
%argslist56517$k484490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58088 = alloca %struct.ScmObj*, align 8
%argslist56517$k484491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48687, %struct.ScmObj* %argslist56517$k484490)
store volatile %struct.ScmObj* %argslist56517$k484491, %struct.ScmObj** %stackaddr$prim58088, align 8
%stackaddr$prim58089 = alloca %struct.ScmObj*, align 8
%argslist56517$k484492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48686, %struct.ScmObj* %argslist56517$k484491)
store volatile %struct.ScmObj* %argslist56517$k484492, %struct.ScmObj** %stackaddr$prim58089, align 8
%clofunc58090 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48449)
musttail call tailcc void %clofunc58090(%struct.ScmObj* %k48449, %struct.ScmObj* %argslist56517$k484492)
ret void
falsebranch$cmp58087:
%stackaddr$prim58091 = alloca %struct.ScmObj*, align 8
%anf_45bind48178 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48051)
store volatile %struct.ScmObj* %anf_45bind48178, %struct.ScmObj** %stackaddr$prim58091, align 8
%stackaddr$makeclosure58092 = alloca %struct.ScmObj*, align 8
%fptrToInt58093 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48696 to i64
%ae48696 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt58093)
store volatile %struct.ScmObj* %ae48696, %struct.ScmObj** %stackaddr$makeclosure58092, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48696, %struct.ScmObj* %f48052, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48696, %struct.ScmObj* %lst48051, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48696, %struct.ScmObj* %_37map48050, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48696, %struct.ScmObj* %k48449, i64 3)
%argslist56526$f480520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58094 = alloca %struct.ScmObj*, align 8
%argslist56526$f480521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48178, %struct.ScmObj* %argslist56526$f480520)
store volatile %struct.ScmObj* %argslist56526$f480521, %struct.ScmObj** %stackaddr$prim58094, align 8
%stackaddr$prim58095 = alloca %struct.ScmObj*, align 8
%argslist56526$f480522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48696, %struct.ScmObj* %argslist56526$f480521)
store volatile %struct.ScmObj* %argslist56526$f480522, %struct.ScmObj** %stackaddr$prim58095, align 8
%clofunc58096 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48052)
musttail call tailcc void %clofunc58096(%struct.ScmObj* %f48052, %struct.ScmObj* %argslist56526$f480522)
ret void
}

define tailcc void @proc_clo$ae48696(%struct.ScmObj* %env$ae48696,%struct.ScmObj* %current_45args56518) {
%stackaddr$env-ref58097 = alloca %struct.ScmObj*, align 8
%f48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48696, i64 0)
store %struct.ScmObj* %f48052, %struct.ScmObj** %stackaddr$env-ref58097
%stackaddr$env-ref58098 = alloca %struct.ScmObj*, align 8
%lst48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48696, i64 1)
store %struct.ScmObj* %lst48051, %struct.ScmObj** %stackaddr$env-ref58098
%stackaddr$env-ref58099 = alloca %struct.ScmObj*, align 8
%_37map48050 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48696, i64 2)
store %struct.ScmObj* %_37map48050, %struct.ScmObj** %stackaddr$env-ref58099
%stackaddr$env-ref58100 = alloca %struct.ScmObj*, align 8
%k48449 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48696, i64 3)
store %struct.ScmObj* %k48449, %struct.ScmObj** %stackaddr$env-ref58100
%stackaddr$prim58101 = alloca %struct.ScmObj*, align 8
%_95k48450 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56518)
store volatile %struct.ScmObj* %_95k48450, %struct.ScmObj** %stackaddr$prim58101, align 8
%stackaddr$prim58102 = alloca %struct.ScmObj*, align 8
%current_45args56519 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56518)
store volatile %struct.ScmObj* %current_45args56519, %struct.ScmObj** %stackaddr$prim58102, align 8
%stackaddr$prim58103 = alloca %struct.ScmObj*, align 8
%anf_45bind48179 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56519)
store volatile %struct.ScmObj* %anf_45bind48179, %struct.ScmObj** %stackaddr$prim58103, align 8
%stackaddr$prim58104 = alloca %struct.ScmObj*, align 8
%anf_45bind48180 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48051)
store volatile %struct.ScmObj* %anf_45bind48180, %struct.ScmObj** %stackaddr$prim58104, align 8
%stackaddr$makeclosure58105 = alloca %struct.ScmObj*, align 8
%fptrToInt58106 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48700 to i64
%ae48700 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58106)
store volatile %struct.ScmObj* %ae48700, %struct.ScmObj** %stackaddr$makeclosure58105, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48700, %struct.ScmObj* %anf_45bind48179, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48700, %struct.ScmObj* %k48449, i64 1)
%argslist56525$_37map480500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58107 = alloca %struct.ScmObj*, align 8
%argslist56525$_37map480501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48180, %struct.ScmObj* %argslist56525$_37map480500)
store volatile %struct.ScmObj* %argslist56525$_37map480501, %struct.ScmObj** %stackaddr$prim58107, align 8
%stackaddr$prim58108 = alloca %struct.ScmObj*, align 8
%argslist56525$_37map480502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48052, %struct.ScmObj* %argslist56525$_37map480501)
store volatile %struct.ScmObj* %argslist56525$_37map480502, %struct.ScmObj** %stackaddr$prim58108, align 8
%stackaddr$prim58109 = alloca %struct.ScmObj*, align 8
%argslist56525$_37map480503 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48700, %struct.ScmObj* %argslist56525$_37map480502)
store volatile %struct.ScmObj* %argslist56525$_37map480503, %struct.ScmObj** %stackaddr$prim58109, align 8
%clofunc58110 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map48050)
musttail call tailcc void %clofunc58110(%struct.ScmObj* %_37map48050, %struct.ScmObj* %argslist56525$_37map480503)
ret void
}

define tailcc void @proc_clo$ae48700(%struct.ScmObj* %env$ae48700,%struct.ScmObj* %current_45args56521) {
%stackaddr$env-ref58111 = alloca %struct.ScmObj*, align 8
%anf_45bind48179 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48700, i64 0)
store %struct.ScmObj* %anf_45bind48179, %struct.ScmObj** %stackaddr$env-ref58111
%stackaddr$env-ref58112 = alloca %struct.ScmObj*, align 8
%k48449 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48700, i64 1)
store %struct.ScmObj* %k48449, %struct.ScmObj** %stackaddr$env-ref58112
%stackaddr$prim58113 = alloca %struct.ScmObj*, align 8
%_95k48451 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56521)
store volatile %struct.ScmObj* %_95k48451, %struct.ScmObj** %stackaddr$prim58113, align 8
%stackaddr$prim58114 = alloca %struct.ScmObj*, align 8
%current_45args56522 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56521)
store volatile %struct.ScmObj* %current_45args56522, %struct.ScmObj** %stackaddr$prim58114, align 8
%stackaddr$prim58115 = alloca %struct.ScmObj*, align 8
%anf_45bind48181 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56522)
store volatile %struct.ScmObj* %anf_45bind48181, %struct.ScmObj** %stackaddr$prim58115, align 8
%stackaddr$prim58116 = alloca %struct.ScmObj*, align 8
%cpsprim48452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48179, %struct.ScmObj* %anf_45bind48181)
store volatile %struct.ScmObj* %cpsprim48452, %struct.ScmObj** %stackaddr$prim58116, align 8
%ae48706 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56524$k484490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58117 = alloca %struct.ScmObj*, align 8
%argslist56524$k484491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48452, %struct.ScmObj* %argslist56524$k484490)
store volatile %struct.ScmObj* %argslist56524$k484491, %struct.ScmObj** %stackaddr$prim58117, align 8
%stackaddr$prim58118 = alloca %struct.ScmObj*, align 8
%argslist56524$k484492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48706, %struct.ScmObj* %argslist56524$k484491)
store volatile %struct.ScmObj* %argslist56524$k484492, %struct.ScmObj** %stackaddr$prim58118, align 8
%clofunc58119 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48449)
musttail call tailcc void %clofunc58119(%struct.ScmObj* %k48449, %struct.ScmObj* %argslist56524$k484492)
ret void
}

define tailcc void @proc_clo$ae48599(%struct.ScmObj* %env$ae48599,%struct.ScmObj* %current_45args56530) {
%stackaddr$prim58120 = alloca %struct.ScmObj*, align 8
%k48453 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56530)
store volatile %struct.ScmObj* %k48453, %struct.ScmObj** %stackaddr$prim58120, align 8
%stackaddr$prim58121 = alloca %struct.ScmObj*, align 8
%current_45args56531 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56530)
store volatile %struct.ScmObj* %current_45args56531, %struct.ScmObj** %stackaddr$prim58121, align 8
%stackaddr$prim58122 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56531)
store volatile %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$prim58122, align 8
%ae48601 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58123 = alloca %struct.ScmObj*, align 8
%fptrToInt58124 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48602 to i64
%ae48602 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58124)
store volatile %struct.ScmObj* %ae48602, %struct.ScmObj** %stackaddr$makeclosure58123, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48602, %struct.ScmObj* %_37foldr148054, i64 0)
%argslist56544$k484530 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58125 = alloca %struct.ScmObj*, align 8
%argslist56544$k484531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48602, %struct.ScmObj* %argslist56544$k484530)
store volatile %struct.ScmObj* %argslist56544$k484531, %struct.ScmObj** %stackaddr$prim58125, align 8
%stackaddr$prim58126 = alloca %struct.ScmObj*, align 8
%argslist56544$k484532 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48601, %struct.ScmObj* %argslist56544$k484531)
store volatile %struct.ScmObj* %argslist56544$k484532, %struct.ScmObj** %stackaddr$prim58126, align 8
%clofunc58127 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48453)
musttail call tailcc void %clofunc58127(%struct.ScmObj* %k48453, %struct.ScmObj* %argslist56544$k484532)
ret void
}

define tailcc void @proc_clo$ae48602(%struct.ScmObj* %env$ae48602,%struct.ScmObj* %current_45args56533) {
%stackaddr$env-ref58128 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48602, i64 0)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref58128
%stackaddr$prim58129 = alloca %struct.ScmObj*, align 8
%k48454 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56533)
store volatile %struct.ScmObj* %k48454, %struct.ScmObj** %stackaddr$prim58129, align 8
%stackaddr$prim58130 = alloca %struct.ScmObj*, align 8
%current_45args56534 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56533)
store volatile %struct.ScmObj* %current_45args56534, %struct.ScmObj** %stackaddr$prim58130, align 8
%stackaddr$prim58131 = alloca %struct.ScmObj*, align 8
%f48057 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56534)
store volatile %struct.ScmObj* %f48057, %struct.ScmObj** %stackaddr$prim58131, align 8
%stackaddr$prim58132 = alloca %struct.ScmObj*, align 8
%current_45args56535 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56534)
store volatile %struct.ScmObj* %current_45args56535, %struct.ScmObj** %stackaddr$prim58132, align 8
%stackaddr$prim58133 = alloca %struct.ScmObj*, align 8
%acc48056 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56535)
store volatile %struct.ScmObj* %acc48056, %struct.ScmObj** %stackaddr$prim58133, align 8
%stackaddr$prim58134 = alloca %struct.ScmObj*, align 8
%current_45args56536 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56535)
store volatile %struct.ScmObj* %current_45args56536, %struct.ScmObj** %stackaddr$prim58134, align 8
%stackaddr$prim58135 = alloca %struct.ScmObj*, align 8
%lst48055 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56536)
store volatile %struct.ScmObj* %lst48055, %struct.ScmObj** %stackaddr$prim58135, align 8
%stackaddr$prim58136 = alloca %struct.ScmObj*, align 8
%anf_45bind48172 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48055)
store volatile %struct.ScmObj* %anf_45bind48172, %struct.ScmObj** %stackaddr$prim58136, align 8
%truthy$cmp58137 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48172)
%cmp$cmp58137 = icmp eq i64 %truthy$cmp58137, 1
br i1 %cmp$cmp58137, label %truebranch$cmp58137, label %falsebranch$cmp58137
truebranch$cmp58137:
%ae48606 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56538$k484540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58138 = alloca %struct.ScmObj*, align 8
%argslist56538$k484541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48056, %struct.ScmObj* %argslist56538$k484540)
store volatile %struct.ScmObj* %argslist56538$k484541, %struct.ScmObj** %stackaddr$prim58138, align 8
%stackaddr$prim58139 = alloca %struct.ScmObj*, align 8
%argslist56538$k484542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48606, %struct.ScmObj* %argslist56538$k484541)
store volatile %struct.ScmObj* %argslist56538$k484542, %struct.ScmObj** %stackaddr$prim58139, align 8
%clofunc58140 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48454)
musttail call tailcc void %clofunc58140(%struct.ScmObj* %k48454, %struct.ScmObj* %argslist56538$k484542)
ret void
falsebranch$cmp58137:
%stackaddr$prim58141 = alloca %struct.ScmObj*, align 8
%anf_45bind48173 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48055)
store volatile %struct.ScmObj* %anf_45bind48173, %struct.ScmObj** %stackaddr$prim58141, align 8
%stackaddr$prim58142 = alloca %struct.ScmObj*, align 8
%anf_45bind48174 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48055)
store volatile %struct.ScmObj* %anf_45bind48174, %struct.ScmObj** %stackaddr$prim58142, align 8
%stackaddr$makeclosure58143 = alloca %struct.ScmObj*, align 8
%fptrToInt58144 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48614 to i64
%ae48614 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58144)
store volatile %struct.ScmObj* %ae48614, %struct.ScmObj** %stackaddr$makeclosure58143, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48614, %struct.ScmObj* %f48057, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48614, %struct.ScmObj* %k48454, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48614, %struct.ScmObj* %anf_45bind48173, i64 2)
%argslist56543$_37foldr1480540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58145 = alloca %struct.ScmObj*, align 8
%argslist56543$_37foldr1480541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48174, %struct.ScmObj* %argslist56543$_37foldr1480540)
store volatile %struct.ScmObj* %argslist56543$_37foldr1480541, %struct.ScmObj** %stackaddr$prim58145, align 8
%stackaddr$prim58146 = alloca %struct.ScmObj*, align 8
%argslist56543$_37foldr1480542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48056, %struct.ScmObj* %argslist56543$_37foldr1480541)
store volatile %struct.ScmObj* %argslist56543$_37foldr1480542, %struct.ScmObj** %stackaddr$prim58146, align 8
%stackaddr$prim58147 = alloca %struct.ScmObj*, align 8
%argslist56543$_37foldr1480543 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48057, %struct.ScmObj* %argslist56543$_37foldr1480542)
store volatile %struct.ScmObj* %argslist56543$_37foldr1480543, %struct.ScmObj** %stackaddr$prim58147, align 8
%stackaddr$prim58148 = alloca %struct.ScmObj*, align 8
%argslist56543$_37foldr1480544 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48614, %struct.ScmObj* %argslist56543$_37foldr1480543)
store volatile %struct.ScmObj* %argslist56543$_37foldr1480544, %struct.ScmObj** %stackaddr$prim58148, align 8
%clofunc58149 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148054)
musttail call tailcc void %clofunc58149(%struct.ScmObj* %_37foldr148054, %struct.ScmObj* %argslist56543$_37foldr1480544)
ret void
}

define tailcc void @proc_clo$ae48614(%struct.ScmObj* %env$ae48614,%struct.ScmObj* %current_45args56539) {
%stackaddr$env-ref58150 = alloca %struct.ScmObj*, align 8
%f48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48614, i64 0)
store %struct.ScmObj* %f48057, %struct.ScmObj** %stackaddr$env-ref58150
%stackaddr$env-ref58151 = alloca %struct.ScmObj*, align 8
%k48454 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48614, i64 1)
store %struct.ScmObj* %k48454, %struct.ScmObj** %stackaddr$env-ref58151
%stackaddr$env-ref58152 = alloca %struct.ScmObj*, align 8
%anf_45bind48173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48614, i64 2)
store %struct.ScmObj* %anf_45bind48173, %struct.ScmObj** %stackaddr$env-ref58152
%stackaddr$prim58153 = alloca %struct.ScmObj*, align 8
%_95k48455 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56539)
store volatile %struct.ScmObj* %_95k48455, %struct.ScmObj** %stackaddr$prim58153, align 8
%stackaddr$prim58154 = alloca %struct.ScmObj*, align 8
%current_45args56540 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56539)
store volatile %struct.ScmObj* %current_45args56540, %struct.ScmObj** %stackaddr$prim58154, align 8
%stackaddr$prim58155 = alloca %struct.ScmObj*, align 8
%anf_45bind48175 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56540)
store volatile %struct.ScmObj* %anf_45bind48175, %struct.ScmObj** %stackaddr$prim58155, align 8
%argslist56542$f480570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58156 = alloca %struct.ScmObj*, align 8
%argslist56542$f480571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48175, %struct.ScmObj* %argslist56542$f480570)
store volatile %struct.ScmObj* %argslist56542$f480571, %struct.ScmObj** %stackaddr$prim58156, align 8
%stackaddr$prim58157 = alloca %struct.ScmObj*, align 8
%argslist56542$f480572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48173, %struct.ScmObj* %argslist56542$f480571)
store volatile %struct.ScmObj* %argslist56542$f480572, %struct.ScmObj** %stackaddr$prim58157, align 8
%stackaddr$prim58158 = alloca %struct.ScmObj*, align 8
%argslist56542$f480573 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48454, %struct.ScmObj* %argslist56542$f480572)
store volatile %struct.ScmObj* %argslist56542$f480573, %struct.ScmObj** %stackaddr$prim58158, align 8
%clofunc58159 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48057)
musttail call tailcc void %clofunc58159(%struct.ScmObj* %f48057, %struct.ScmObj* %argslist56542$f480573)
ret void
}

define tailcc void @proc_clo$ae48482(%struct.ScmObj* %env$ae48482,%struct.ScmObj* %current_45args56547) {
%stackaddr$prim58160 = alloca %struct.ScmObj*, align 8
%k48456 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56547)
store volatile %struct.ScmObj* %k48456, %struct.ScmObj** %stackaddr$prim58160, align 8
%stackaddr$prim58161 = alloca %struct.ScmObj*, align 8
%current_45args56548 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56547)
store volatile %struct.ScmObj* %current_45args56548, %struct.ScmObj** %stackaddr$prim58161, align 8
%stackaddr$prim58162 = alloca %struct.ScmObj*, align 8
%y48034 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56548)
store volatile %struct.ScmObj* %y48034, %struct.ScmObj** %stackaddr$prim58162, align 8
%ae48484 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58163 = alloca %struct.ScmObj*, align 8
%fptrToInt58164 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48485 to i64
%ae48485 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58164)
store volatile %struct.ScmObj* %ae48485, %struct.ScmObj** %stackaddr$makeclosure58163, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48485, %struct.ScmObj* %y48034, i64 0)
%argslist56566$k484560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58165 = alloca %struct.ScmObj*, align 8
%argslist56566$k484561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48485, %struct.ScmObj* %argslist56566$k484560)
store volatile %struct.ScmObj* %argslist56566$k484561, %struct.ScmObj** %stackaddr$prim58165, align 8
%stackaddr$prim58166 = alloca %struct.ScmObj*, align 8
%argslist56566$k484562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48484, %struct.ScmObj* %argslist56566$k484561)
store volatile %struct.ScmObj* %argslist56566$k484562, %struct.ScmObj** %stackaddr$prim58166, align 8
%clofunc58167 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48456)
musttail call tailcc void %clofunc58167(%struct.ScmObj* %k48456, %struct.ScmObj* %argslist56566$k484562)
ret void
}

define tailcc void @proc_clo$ae48485(%struct.ScmObj* %env$ae48485,%struct.ScmObj* %current_45args56550) {
%stackaddr$env-ref58168 = alloca %struct.ScmObj*, align 8
%y48034 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48485, i64 0)
store %struct.ScmObj* %y48034, %struct.ScmObj** %stackaddr$env-ref58168
%stackaddr$prim58169 = alloca %struct.ScmObj*, align 8
%k48457 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56550)
store volatile %struct.ScmObj* %k48457, %struct.ScmObj** %stackaddr$prim58169, align 8
%stackaddr$prim58170 = alloca %struct.ScmObj*, align 8
%current_45args56551 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56550)
store volatile %struct.ScmObj* %current_45args56551, %struct.ScmObj** %stackaddr$prim58170, align 8
%stackaddr$prim58171 = alloca %struct.ScmObj*, align 8
%f48035 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56551)
store volatile %struct.ScmObj* %f48035, %struct.ScmObj** %stackaddr$prim58171, align 8
%stackaddr$makeclosure58172 = alloca %struct.ScmObj*, align 8
%fptrToInt58173 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48486 to i64
%ae48486 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58173)
store volatile %struct.ScmObj* %ae48486, %struct.ScmObj** %stackaddr$makeclosure58172, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48486, %struct.ScmObj* %k48457, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48486, %struct.ScmObj* %f48035, i64 1)
%ae48487 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58174 = alloca %struct.ScmObj*, align 8
%fptrToInt58175 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48488 to i64
%ae48488 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58175)
store volatile %struct.ScmObj* %ae48488, %struct.ScmObj** %stackaddr$makeclosure58174, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48488, %struct.ScmObj* %f48035, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48488, %struct.ScmObj* %y48034, i64 1)
%argslist56565$ae484860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58176 = alloca %struct.ScmObj*, align 8
%argslist56565$ae484861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48488, %struct.ScmObj* %argslist56565$ae484860)
store volatile %struct.ScmObj* %argslist56565$ae484861, %struct.ScmObj** %stackaddr$prim58176, align 8
%stackaddr$prim58177 = alloca %struct.ScmObj*, align 8
%argslist56565$ae484862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48487, %struct.ScmObj* %argslist56565$ae484861)
store volatile %struct.ScmObj* %argslist56565$ae484862, %struct.ScmObj** %stackaddr$prim58177, align 8
%clofunc58178 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48486)
musttail call tailcc void %clofunc58178(%struct.ScmObj* %ae48486, %struct.ScmObj* %argslist56565$ae484862)
ret void
}

define tailcc void @proc_clo$ae48486(%struct.ScmObj* %env$ae48486,%struct.ScmObj* %current_45args56553) {
%stackaddr$env-ref58179 = alloca %struct.ScmObj*, align 8
%k48457 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48486, i64 0)
store %struct.ScmObj* %k48457, %struct.ScmObj** %stackaddr$env-ref58179
%stackaddr$env-ref58180 = alloca %struct.ScmObj*, align 8
%f48035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48486, i64 1)
store %struct.ScmObj* %f48035, %struct.ScmObj** %stackaddr$env-ref58180
%stackaddr$prim58181 = alloca %struct.ScmObj*, align 8
%_95k48458 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56553)
store volatile %struct.ScmObj* %_95k48458, %struct.ScmObj** %stackaddr$prim58181, align 8
%stackaddr$prim58182 = alloca %struct.ScmObj*, align 8
%current_45args56554 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56553)
store volatile %struct.ScmObj* %current_45args56554, %struct.ScmObj** %stackaddr$prim58182, align 8
%stackaddr$prim58183 = alloca %struct.ScmObj*, align 8
%anf_45bind48170 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56554)
store volatile %struct.ScmObj* %anf_45bind48170, %struct.ScmObj** %stackaddr$prim58183, align 8
%argslist56556$f480350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58184 = alloca %struct.ScmObj*, align 8
%argslist56556$f480351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48170, %struct.ScmObj* %argslist56556$f480350)
store volatile %struct.ScmObj* %argslist56556$f480351, %struct.ScmObj** %stackaddr$prim58184, align 8
%stackaddr$prim58185 = alloca %struct.ScmObj*, align 8
%argslist56556$f480352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48457, %struct.ScmObj* %argslist56556$f480351)
store volatile %struct.ScmObj* %argslist56556$f480352, %struct.ScmObj** %stackaddr$prim58185, align 8
%clofunc58186 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48035)
musttail call tailcc void %clofunc58186(%struct.ScmObj* %f48035, %struct.ScmObj* %argslist56556$f480352)
ret void
}

define tailcc void @proc_clo$ae48488(%struct.ScmObj* %env$ae48488,%struct.ScmObj* %args4803648459) {
%stackaddr$env-ref58187 = alloca %struct.ScmObj*, align 8
%f48035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48488, i64 0)
store %struct.ScmObj* %f48035, %struct.ScmObj** %stackaddr$env-ref58187
%stackaddr$env-ref58188 = alloca %struct.ScmObj*, align 8
%y48034 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48488, i64 1)
store %struct.ScmObj* %y48034, %struct.ScmObj** %stackaddr$env-ref58188
%stackaddr$prim58189 = alloca %struct.ScmObj*, align 8
%k48460 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4803648459)
store volatile %struct.ScmObj* %k48460, %struct.ScmObj** %stackaddr$prim58189, align 8
%stackaddr$prim58190 = alloca %struct.ScmObj*, align 8
%args48036 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4803648459)
store volatile %struct.ScmObj* %args48036, %struct.ScmObj** %stackaddr$prim58190, align 8
%stackaddr$makeclosure58191 = alloca %struct.ScmObj*, align 8
%fptrToInt58192 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48492 to i64
%ae48492 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58192)
store volatile %struct.ScmObj* %ae48492, %struct.ScmObj** %stackaddr$makeclosure58191, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48492, %struct.ScmObj* %args48036, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48492, %struct.ScmObj* %f48035, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48492, %struct.ScmObj* %k48460, i64 2)
%argslist56564$y480340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58193 = alloca %struct.ScmObj*, align 8
%argslist56564$y480341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y48034, %struct.ScmObj* %argslist56564$y480340)
store volatile %struct.ScmObj* %argslist56564$y480341, %struct.ScmObj** %stackaddr$prim58193, align 8
%stackaddr$prim58194 = alloca %struct.ScmObj*, align 8
%argslist56564$y480342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48492, %struct.ScmObj* %argslist56564$y480341)
store volatile %struct.ScmObj* %argslist56564$y480342, %struct.ScmObj** %stackaddr$prim58194, align 8
%clofunc58195 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y48034)
musttail call tailcc void %clofunc58195(%struct.ScmObj* %y48034, %struct.ScmObj* %argslist56564$y480342)
ret void
}

define tailcc void @proc_clo$ae48492(%struct.ScmObj* %env$ae48492,%struct.ScmObj* %current_45args56557) {
%stackaddr$env-ref58196 = alloca %struct.ScmObj*, align 8
%args48036 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48492, i64 0)
store %struct.ScmObj* %args48036, %struct.ScmObj** %stackaddr$env-ref58196
%stackaddr$env-ref58197 = alloca %struct.ScmObj*, align 8
%f48035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48492, i64 1)
store %struct.ScmObj* %f48035, %struct.ScmObj** %stackaddr$env-ref58197
%stackaddr$env-ref58198 = alloca %struct.ScmObj*, align 8
%k48460 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48492, i64 2)
store %struct.ScmObj* %k48460, %struct.ScmObj** %stackaddr$env-ref58198
%stackaddr$prim58199 = alloca %struct.ScmObj*, align 8
%_95k48461 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56557)
store volatile %struct.ScmObj* %_95k48461, %struct.ScmObj** %stackaddr$prim58199, align 8
%stackaddr$prim58200 = alloca %struct.ScmObj*, align 8
%current_45args56558 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56557)
store volatile %struct.ScmObj* %current_45args56558, %struct.ScmObj** %stackaddr$prim58200, align 8
%stackaddr$prim58201 = alloca %struct.ScmObj*, align 8
%anf_45bind48168 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56558)
store volatile %struct.ScmObj* %anf_45bind48168, %struct.ScmObj** %stackaddr$prim58201, align 8
%stackaddr$makeclosure58202 = alloca %struct.ScmObj*, align 8
%fptrToInt58203 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48495 to i64
%ae48495 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58203)
store volatile %struct.ScmObj* %ae48495, %struct.ScmObj** %stackaddr$makeclosure58202, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48495, %struct.ScmObj* %args48036, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48495, %struct.ScmObj* %k48460, i64 1)
%argslist56563$anf_45bind481680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58204 = alloca %struct.ScmObj*, align 8
%argslist56563$anf_45bind481681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48035, %struct.ScmObj* %argslist56563$anf_45bind481680)
store volatile %struct.ScmObj* %argslist56563$anf_45bind481681, %struct.ScmObj** %stackaddr$prim58204, align 8
%stackaddr$prim58205 = alloca %struct.ScmObj*, align 8
%argslist56563$anf_45bind481682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48495, %struct.ScmObj* %argslist56563$anf_45bind481681)
store volatile %struct.ScmObj* %argslist56563$anf_45bind481682, %struct.ScmObj** %stackaddr$prim58205, align 8
%clofunc58206 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48168)
musttail call tailcc void %clofunc58206(%struct.ScmObj* %anf_45bind48168, %struct.ScmObj* %argslist56563$anf_45bind481682)
ret void
}

define tailcc void @proc_clo$ae48495(%struct.ScmObj* %env$ae48495,%struct.ScmObj* %current_45args56560) {
%stackaddr$env-ref58207 = alloca %struct.ScmObj*, align 8
%args48036 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48495, i64 0)
store %struct.ScmObj* %args48036, %struct.ScmObj** %stackaddr$env-ref58207
%stackaddr$env-ref58208 = alloca %struct.ScmObj*, align 8
%k48460 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48495, i64 1)
store %struct.ScmObj* %k48460, %struct.ScmObj** %stackaddr$env-ref58208
%stackaddr$prim58209 = alloca %struct.ScmObj*, align 8
%_95k48462 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56560)
store volatile %struct.ScmObj* %_95k48462, %struct.ScmObj** %stackaddr$prim58209, align 8
%stackaddr$prim58210 = alloca %struct.ScmObj*, align 8
%current_45args56561 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56560)
store volatile %struct.ScmObj* %current_45args56561, %struct.ScmObj** %stackaddr$prim58210, align 8
%stackaddr$prim58211 = alloca %struct.ScmObj*, align 8
%anf_45bind48169 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56561)
store volatile %struct.ScmObj* %anf_45bind48169, %struct.ScmObj** %stackaddr$prim58211, align 8
%stackaddr$prim58212 = alloca %struct.ScmObj*, align 8
%cpsargs48463 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48460, %struct.ScmObj* %args48036)
store volatile %struct.ScmObj* %cpsargs48463, %struct.ScmObj** %stackaddr$prim58212, align 8
%clofunc58213 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48169)
musttail call tailcc void %clofunc58213(%struct.ScmObj* %anf_45bind48169, %struct.ScmObj* %cpsargs48463)
ret void
}

define tailcc void @proc_clo$ae48467(%struct.ScmObj* %env$ae48467,%struct.ScmObj* %current_45args56568) {
%stackaddr$prim58214 = alloca %struct.ScmObj*, align 8
%k48464 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56568)
store volatile %struct.ScmObj* %k48464, %struct.ScmObj** %stackaddr$prim58214, align 8
%stackaddr$prim58215 = alloca %struct.ScmObj*, align 8
%current_45args56569 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56568)
store volatile %struct.ScmObj* %current_45args56569, %struct.ScmObj** %stackaddr$prim58215, align 8
%stackaddr$prim58216 = alloca %struct.ScmObj*, align 8
%yu48033 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56569)
store volatile %struct.ScmObj* %yu48033, %struct.ScmObj** %stackaddr$prim58216, align 8
%argslist56571$yu480330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58217 = alloca %struct.ScmObj*, align 8
%argslist56571$yu480331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu48033, %struct.ScmObj* %argslist56571$yu480330)
store volatile %struct.ScmObj* %argslist56571$yu480331, %struct.ScmObj** %stackaddr$prim58217, align 8
%stackaddr$prim58218 = alloca %struct.ScmObj*, align 8
%argslist56571$yu480332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48464, %struct.ScmObj* %argslist56571$yu480331)
store volatile %struct.ScmObj* %argslist56571$yu480332, %struct.ScmObj** %stackaddr$prim58218, align 8
%clofunc58219 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu48033)
musttail call tailcc void %clofunc58219(%struct.ScmObj* %yu48033, %struct.ScmObj* %argslist56571$yu480332)
ret void
}