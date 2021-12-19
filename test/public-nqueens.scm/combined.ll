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
%mainenv58000 = call %struct.ScmObj* @const_init_null()
%mainargs58001 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv58000, %struct.ScmObj* %mainargs58001)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv57998,%struct.ScmObj* %mainargs57999) {
%stackaddr$makeclosure58002 = alloca %struct.ScmObj*, align 8
%fptrToInt58003 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48530 to i64
%ae48530 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58003)
store volatile %struct.ScmObj* %ae48530, %struct.ScmObj** %stackaddr$makeclosure58002, align 8
%ae48531 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58004 = alloca %struct.ScmObj*, align 8
%fptrToInt58005 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48532 to i64
%ae48532 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58005)
store volatile %struct.ScmObj* %ae48532, %struct.ScmObj** %stackaddr$makeclosure58004, align 8
%argslist57997$ae485300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58006 = alloca %struct.ScmObj*, align 8
%argslist57997$ae485301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48532, %struct.ScmObj* %argslist57997$ae485300)
store volatile %struct.ScmObj* %argslist57997$ae485301, %struct.ScmObj** %stackaddr$prim58006, align 8
%stackaddr$prim58007 = alloca %struct.ScmObj*, align 8
%argslist57997$ae485302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48531, %struct.ScmObj* %argslist57997$ae485301)
store volatile %struct.ScmObj* %argslist57997$ae485302, %struct.ScmObj** %stackaddr$prim58007, align 8
%clofunc58008 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48530)
musttail call tailcc void %clofunc58008(%struct.ScmObj* %ae48530, %struct.ScmObj* %argslist57997$ae485302)
ret void
}

define tailcc void @proc_clo$ae48530(%struct.ScmObj* %env$ae48530,%struct.ScmObj* %current_45args57355) {
%stackaddr$prim58009 = alloca %struct.ScmObj*, align 8
%_95k48338 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57355)
store volatile %struct.ScmObj* %_95k48338, %struct.ScmObj** %stackaddr$prim58009, align 8
%stackaddr$prim58010 = alloca %struct.ScmObj*, align 8
%current_45args57356 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57355)
store volatile %struct.ScmObj* %current_45args57356, %struct.ScmObj** %stackaddr$prim58010, align 8
%stackaddr$prim58011 = alloca %struct.ScmObj*, align 8
%anf_45bind48172 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57356)
store volatile %struct.ScmObj* %anf_45bind48172, %struct.ScmObj** %stackaddr$prim58011, align 8
%stackaddr$makeclosure58012 = alloca %struct.ScmObj*, align 8
%fptrToInt58013 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48545 to i64
%ae48545 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58013)
store volatile %struct.ScmObj* %ae48545, %struct.ScmObj** %stackaddr$makeclosure58012, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48545, %struct.ScmObj* %anf_45bind48172, i64 0)
%ae48546 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58014 = alloca %struct.ScmObj*, align 8
%fptrToInt58015 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48547 to i64
%ae48547 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58015)
store volatile %struct.ScmObj* %ae48547, %struct.ScmObj** %stackaddr$makeclosure58014, align 8
%argslist57992$ae485450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58016 = alloca %struct.ScmObj*, align 8
%argslist57992$ae485451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48547, %struct.ScmObj* %argslist57992$ae485450)
store volatile %struct.ScmObj* %argslist57992$ae485451, %struct.ScmObj** %stackaddr$prim58016, align 8
%stackaddr$prim58017 = alloca %struct.ScmObj*, align 8
%argslist57992$ae485452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48546, %struct.ScmObj* %argslist57992$ae485451)
store volatile %struct.ScmObj* %argslist57992$ae485452, %struct.ScmObj** %stackaddr$prim58017, align 8
%clofunc58018 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48545)
musttail call tailcc void %clofunc58018(%struct.ScmObj* %ae48545, %struct.ScmObj* %argslist57992$ae485452)
ret void
}

define tailcc void @proc_clo$ae48545(%struct.ScmObj* %env$ae48545,%struct.ScmObj* %current_45args57358) {
%stackaddr$env-ref58019 = alloca %struct.ScmObj*, align 8
%anf_45bind48172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48545, i64 0)
store %struct.ScmObj* %anf_45bind48172, %struct.ScmObj** %stackaddr$env-ref58019
%stackaddr$prim58020 = alloca %struct.ScmObj*, align 8
%_95k48339 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57358)
store volatile %struct.ScmObj* %_95k48339, %struct.ScmObj** %stackaddr$prim58020, align 8
%stackaddr$prim58021 = alloca %struct.ScmObj*, align 8
%current_45args57359 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57358)
store volatile %struct.ScmObj* %current_45args57359, %struct.ScmObj** %stackaddr$prim58021, align 8
%stackaddr$prim58022 = alloca %struct.ScmObj*, align 8
%anf_45bind48176 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57359)
store volatile %struct.ScmObj* %anf_45bind48176, %struct.ScmObj** %stackaddr$prim58022, align 8
%stackaddr$makeclosure58023 = alloca %struct.ScmObj*, align 8
%fptrToInt58024 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48660 to i64
%ae48660 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58024)
store volatile %struct.ScmObj* %ae48660, %struct.ScmObj** %stackaddr$makeclosure58023, align 8
%argslist57971$anf_45bind481720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58025 = alloca %struct.ScmObj*, align 8
%argslist57971$anf_45bind481721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48176, %struct.ScmObj* %argslist57971$anf_45bind481720)
store volatile %struct.ScmObj* %argslist57971$anf_45bind481721, %struct.ScmObj** %stackaddr$prim58025, align 8
%stackaddr$prim58026 = alloca %struct.ScmObj*, align 8
%argslist57971$anf_45bind481722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48660, %struct.ScmObj* %argslist57971$anf_45bind481721)
store volatile %struct.ScmObj* %argslist57971$anf_45bind481722, %struct.ScmObj** %stackaddr$prim58026, align 8
%clofunc58027 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48172)
musttail call tailcc void %clofunc58027(%struct.ScmObj* %anf_45bind48172, %struct.ScmObj* %argslist57971$anf_45bind481722)
ret void
}

define tailcc void @proc_clo$ae48660(%struct.ScmObj* %env$ae48660,%struct.ScmObj* %current_45args57361) {
%stackaddr$prim58028 = alloca %struct.ScmObj*, align 8
%_95k48340 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57361)
store volatile %struct.ScmObj* %_95k48340, %struct.ScmObj** %stackaddr$prim58028, align 8
%stackaddr$prim58029 = alloca %struct.ScmObj*, align 8
%current_45args57362 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57361)
store volatile %struct.ScmObj* %current_45args57362, %struct.ScmObj** %stackaddr$prim58029, align 8
%stackaddr$prim58030 = alloca %struct.ScmObj*, align 8
%Ycmb48030 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57362)
store volatile %struct.ScmObj* %Ycmb48030, %struct.ScmObj** %stackaddr$prim58030, align 8
%stackaddr$makeclosure58031 = alloca %struct.ScmObj*, align 8
%fptrToInt58032 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48662 to i64
%ae48662 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58032)
store volatile %struct.ScmObj* %ae48662, %struct.ScmObj** %stackaddr$makeclosure58031, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48662, %struct.ScmObj* %Ycmb48030, i64 0)
%ae48663 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58033 = alloca %struct.ScmObj*, align 8
%fptrToInt58034 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48664 to i64
%ae48664 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58034)
store volatile %struct.ScmObj* %ae48664, %struct.ScmObj** %stackaddr$makeclosure58033, align 8
%argslist57970$ae486620 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58035 = alloca %struct.ScmObj*, align 8
%argslist57970$ae486621 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48664, %struct.ScmObj* %argslist57970$ae486620)
store volatile %struct.ScmObj* %argslist57970$ae486621, %struct.ScmObj** %stackaddr$prim58035, align 8
%stackaddr$prim58036 = alloca %struct.ScmObj*, align 8
%argslist57970$ae486622 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48663, %struct.ScmObj* %argslist57970$ae486621)
store volatile %struct.ScmObj* %argslist57970$ae486622, %struct.ScmObj** %stackaddr$prim58036, align 8
%clofunc58037 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48662)
musttail call tailcc void %clofunc58037(%struct.ScmObj* %ae48662, %struct.ScmObj* %argslist57970$ae486622)
ret void
}

define tailcc void @proc_clo$ae48662(%struct.ScmObj* %env$ae48662,%struct.ScmObj* %current_45args57364) {
%stackaddr$env-ref58038 = alloca %struct.ScmObj*, align 8
%Ycmb48030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48662, i64 0)
store %struct.ScmObj* %Ycmb48030, %struct.ScmObj** %stackaddr$env-ref58038
%stackaddr$prim58039 = alloca %struct.ScmObj*, align 8
%_95k48341 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57364)
store volatile %struct.ScmObj* %_95k48341, %struct.ScmObj** %stackaddr$prim58039, align 8
%stackaddr$prim58040 = alloca %struct.ScmObj*, align 8
%current_45args57365 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57364)
store volatile %struct.ScmObj* %current_45args57365, %struct.ScmObj** %stackaddr$prim58040, align 8
%stackaddr$prim58041 = alloca %struct.ScmObj*, align 8
%anf_45bind48181 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57365)
store volatile %struct.ScmObj* %anf_45bind48181, %struct.ScmObj** %stackaddr$prim58041, align 8
%stackaddr$makeclosure58042 = alloca %struct.ScmObj*, align 8
%fptrToInt58043 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48740 to i64
%ae48740 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58043)
store volatile %struct.ScmObj* %ae48740, %struct.ScmObj** %stackaddr$makeclosure58042, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48740, %struct.ScmObj* %Ycmb48030, i64 0)
%argslist57954$Ycmb480300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58044 = alloca %struct.ScmObj*, align 8
%argslist57954$Ycmb480301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48181, %struct.ScmObj* %argslist57954$Ycmb480300)
store volatile %struct.ScmObj* %argslist57954$Ycmb480301, %struct.ScmObj** %stackaddr$prim58044, align 8
%stackaddr$prim58045 = alloca %struct.ScmObj*, align 8
%argslist57954$Ycmb480302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48740, %struct.ScmObj* %argslist57954$Ycmb480301)
store volatile %struct.ScmObj* %argslist57954$Ycmb480302, %struct.ScmObj** %stackaddr$prim58045, align 8
%clofunc58046 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48030)
musttail call tailcc void %clofunc58046(%struct.ScmObj* %Ycmb48030, %struct.ScmObj* %argslist57954$Ycmb480302)
ret void
}

define tailcc void @proc_clo$ae48740(%struct.ScmObj* %env$ae48740,%struct.ScmObj* %current_45args57367) {
%stackaddr$env-ref58047 = alloca %struct.ScmObj*, align 8
%Ycmb48030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48740, i64 0)
store %struct.ScmObj* %Ycmb48030, %struct.ScmObj** %stackaddr$env-ref58047
%stackaddr$prim58048 = alloca %struct.ScmObj*, align 8
%_95k48342 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57367)
store volatile %struct.ScmObj* %_95k48342, %struct.ScmObj** %stackaddr$prim58048, align 8
%stackaddr$prim58049 = alloca %struct.ScmObj*, align 8
%current_45args57368 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57367)
store volatile %struct.ScmObj* %current_45args57368, %struct.ScmObj** %stackaddr$prim58049, align 8
%stackaddr$prim58050 = alloca %struct.ScmObj*, align 8
%_37foldr148051 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57368)
store volatile %struct.ScmObj* %_37foldr148051, %struct.ScmObj** %stackaddr$prim58050, align 8
%stackaddr$makeclosure58051 = alloca %struct.ScmObj*, align 8
%fptrToInt58052 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48742 to i64
%ae48742 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58052)
store volatile %struct.ScmObj* %ae48742, %struct.ScmObj** %stackaddr$makeclosure58051, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48742, %struct.ScmObj* %_37foldr148051, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48742, %struct.ScmObj* %Ycmb48030, i64 1)
%ae48743 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58053 = alloca %struct.ScmObj*, align 8
%fptrToInt58054 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48744 to i64
%ae48744 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58054)
store volatile %struct.ScmObj* %ae48744, %struct.ScmObj** %stackaddr$makeclosure58053, align 8
%argslist57953$ae487420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58055 = alloca %struct.ScmObj*, align 8
%argslist57953$ae487421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48744, %struct.ScmObj* %argslist57953$ae487420)
store volatile %struct.ScmObj* %argslist57953$ae487421, %struct.ScmObj** %stackaddr$prim58055, align 8
%stackaddr$prim58056 = alloca %struct.ScmObj*, align 8
%argslist57953$ae487422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48743, %struct.ScmObj* %argslist57953$ae487421)
store volatile %struct.ScmObj* %argslist57953$ae487422, %struct.ScmObj** %stackaddr$prim58056, align 8
%clofunc58057 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48742)
musttail call tailcc void %clofunc58057(%struct.ScmObj* %ae48742, %struct.ScmObj* %argslist57953$ae487422)
ret void
}

define tailcc void @proc_clo$ae48742(%struct.ScmObj* %env$ae48742,%struct.ScmObj* %current_45args57370) {
%stackaddr$env-ref58058 = alloca %struct.ScmObj*, align 8
%_37foldr148051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48742, i64 0)
store %struct.ScmObj* %_37foldr148051, %struct.ScmObj** %stackaddr$env-ref58058
%stackaddr$env-ref58059 = alloca %struct.ScmObj*, align 8
%Ycmb48030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48742, i64 1)
store %struct.ScmObj* %Ycmb48030, %struct.ScmObj** %stackaddr$env-ref58059
%stackaddr$prim58060 = alloca %struct.ScmObj*, align 8
%_95k48343 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57370)
store volatile %struct.ScmObj* %_95k48343, %struct.ScmObj** %stackaddr$prim58060, align 8
%stackaddr$prim58061 = alloca %struct.ScmObj*, align 8
%current_45args57371 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57370)
store volatile %struct.ScmObj* %current_45args57371, %struct.ScmObj** %stackaddr$prim58061, align 8
%stackaddr$prim58062 = alloca %struct.ScmObj*, align 8
%anf_45bind48187 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57371)
store volatile %struct.ScmObj* %anf_45bind48187, %struct.ScmObj** %stackaddr$prim58062, align 8
%stackaddr$makeclosure58063 = alloca %struct.ScmObj*, align 8
%fptrToInt58064 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48837 to i64
%ae48837 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58064)
store volatile %struct.ScmObj* %ae48837, %struct.ScmObj** %stackaddr$makeclosure58063, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48837, %struct.ScmObj* %_37foldr148051, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48837, %struct.ScmObj* %Ycmb48030, i64 1)
%argslist57934$Ycmb480300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58065 = alloca %struct.ScmObj*, align 8
%argslist57934$Ycmb480301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48187, %struct.ScmObj* %argslist57934$Ycmb480300)
store volatile %struct.ScmObj* %argslist57934$Ycmb480301, %struct.ScmObj** %stackaddr$prim58065, align 8
%stackaddr$prim58066 = alloca %struct.ScmObj*, align 8
%argslist57934$Ycmb480302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48837, %struct.ScmObj* %argslist57934$Ycmb480301)
store volatile %struct.ScmObj* %argslist57934$Ycmb480302, %struct.ScmObj** %stackaddr$prim58066, align 8
%clofunc58067 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48030)
musttail call tailcc void %clofunc58067(%struct.ScmObj* %Ycmb48030, %struct.ScmObj* %argslist57934$Ycmb480302)
ret void
}

define tailcc void @proc_clo$ae48837(%struct.ScmObj* %env$ae48837,%struct.ScmObj* %current_45args57373) {
%stackaddr$env-ref58068 = alloca %struct.ScmObj*, align 8
%_37foldr148051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48837, i64 0)
store %struct.ScmObj* %_37foldr148051, %struct.ScmObj** %stackaddr$env-ref58068
%stackaddr$env-ref58069 = alloca %struct.ScmObj*, align 8
%Ycmb48030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48837, i64 1)
store %struct.ScmObj* %Ycmb48030, %struct.ScmObj** %stackaddr$env-ref58069
%stackaddr$prim58070 = alloca %struct.ScmObj*, align 8
%_95k48344 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57373)
store volatile %struct.ScmObj* %_95k48344, %struct.ScmObj** %stackaddr$prim58070, align 8
%stackaddr$prim58071 = alloca %struct.ScmObj*, align 8
%current_45args57374 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57373)
store volatile %struct.ScmObj* %current_45args57374, %struct.ScmObj** %stackaddr$prim58071, align 8
%stackaddr$prim58072 = alloca %struct.ScmObj*, align 8
%_37map148047 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57374)
store volatile %struct.ScmObj* %_37map148047, %struct.ScmObj** %stackaddr$prim58072, align 8
%stackaddr$makeclosure58073 = alloca %struct.ScmObj*, align 8
%fptrToInt58074 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48839 to i64
%ae48839 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58074)
store volatile %struct.ScmObj* %ae48839, %struct.ScmObj** %stackaddr$makeclosure58073, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48839, %struct.ScmObj* %_37foldr148051, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48839, %struct.ScmObj* %_37map148047, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48839, %struct.ScmObj* %Ycmb48030, i64 2)
%ae48840 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58075 = alloca %struct.ScmObj*, align 8
%fptrToInt58076 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48841 to i64
%ae48841 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58076)
store volatile %struct.ScmObj* %ae48841, %struct.ScmObj** %stackaddr$makeclosure58075, align 8
%argslist57933$ae488390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58077 = alloca %struct.ScmObj*, align 8
%argslist57933$ae488391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48841, %struct.ScmObj* %argslist57933$ae488390)
store volatile %struct.ScmObj* %argslist57933$ae488391, %struct.ScmObj** %stackaddr$prim58077, align 8
%stackaddr$prim58078 = alloca %struct.ScmObj*, align 8
%argslist57933$ae488392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48840, %struct.ScmObj* %argslist57933$ae488391)
store volatile %struct.ScmObj* %argslist57933$ae488392, %struct.ScmObj** %stackaddr$prim58078, align 8
%clofunc58079 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48839)
musttail call tailcc void %clofunc58079(%struct.ScmObj* %ae48839, %struct.ScmObj* %argslist57933$ae488392)
ret void
}

define tailcc void @proc_clo$ae48839(%struct.ScmObj* %env$ae48839,%struct.ScmObj* %current_45args57376) {
%stackaddr$env-ref58080 = alloca %struct.ScmObj*, align 8
%_37foldr148051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48839, i64 0)
store %struct.ScmObj* %_37foldr148051, %struct.ScmObj** %stackaddr$env-ref58080
%stackaddr$env-ref58081 = alloca %struct.ScmObj*, align 8
%_37map148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48839, i64 1)
store %struct.ScmObj* %_37map148047, %struct.ScmObj** %stackaddr$env-ref58081
%stackaddr$env-ref58082 = alloca %struct.ScmObj*, align 8
%Ycmb48030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48839, i64 2)
store %struct.ScmObj* %Ycmb48030, %struct.ScmObj** %stackaddr$env-ref58082
%stackaddr$prim58083 = alloca %struct.ScmObj*, align 8
%_95k48345 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57376)
store volatile %struct.ScmObj* %_95k48345, %struct.ScmObj** %stackaddr$prim58083, align 8
%stackaddr$prim58084 = alloca %struct.ScmObj*, align 8
%current_45args57377 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57376)
store volatile %struct.ScmObj* %current_45args57377, %struct.ScmObj** %stackaddr$prim58084, align 8
%stackaddr$prim58085 = alloca %struct.ScmObj*, align 8
%anf_45bind48194 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57377)
store volatile %struct.ScmObj* %anf_45bind48194, %struct.ScmObj** %stackaddr$prim58085, align 8
%stackaddr$makeclosure58086 = alloca %struct.ScmObj*, align 8
%fptrToInt58087 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48987 to i64
%ae48987 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58087)
store volatile %struct.ScmObj* %ae48987, %struct.ScmObj** %stackaddr$makeclosure58086, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48987, %struct.ScmObj* %_37foldr148051, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48987, %struct.ScmObj* %_37map148047, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48987, %struct.ScmObj* %Ycmb48030, i64 2)
%argslist57917$Ycmb480300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58088 = alloca %struct.ScmObj*, align 8
%argslist57917$Ycmb480301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48194, %struct.ScmObj* %argslist57917$Ycmb480300)
store volatile %struct.ScmObj* %argslist57917$Ycmb480301, %struct.ScmObj** %stackaddr$prim58088, align 8
%stackaddr$prim58089 = alloca %struct.ScmObj*, align 8
%argslist57917$Ycmb480302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48987, %struct.ScmObj* %argslist57917$Ycmb480301)
store volatile %struct.ScmObj* %argslist57917$Ycmb480302, %struct.ScmObj** %stackaddr$prim58089, align 8
%clofunc58090 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48030)
musttail call tailcc void %clofunc58090(%struct.ScmObj* %Ycmb48030, %struct.ScmObj* %argslist57917$Ycmb480302)
ret void
}

define tailcc void @proc_clo$ae48987(%struct.ScmObj* %env$ae48987,%struct.ScmObj* %current_45args57379) {
%stackaddr$env-ref58091 = alloca %struct.ScmObj*, align 8
%_37foldr148051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48987, i64 0)
store %struct.ScmObj* %_37foldr148051, %struct.ScmObj** %stackaddr$env-ref58091
%stackaddr$env-ref58092 = alloca %struct.ScmObj*, align 8
%_37map148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48987, i64 1)
store %struct.ScmObj* %_37map148047, %struct.ScmObj** %stackaddr$env-ref58092
%stackaddr$env-ref58093 = alloca %struct.ScmObj*, align 8
%Ycmb48030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48987, i64 2)
store %struct.ScmObj* %Ycmb48030, %struct.ScmObj** %stackaddr$env-ref58093
%stackaddr$prim58094 = alloca %struct.ScmObj*, align 8
%_95k48346 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57379)
store volatile %struct.ScmObj* %_95k48346, %struct.ScmObj** %stackaddr$prim58094, align 8
%stackaddr$prim58095 = alloca %struct.ScmObj*, align 8
%current_45args57380 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57379)
store volatile %struct.ScmObj* %current_45args57380, %struct.ScmObj** %stackaddr$prim58095, align 8
%stackaddr$prim58096 = alloca %struct.ScmObj*, align 8
%_37take48043 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57380)
store volatile %struct.ScmObj* %_37take48043, %struct.ScmObj** %stackaddr$prim58096, align 8
%stackaddr$makeclosure58097 = alloca %struct.ScmObj*, align 8
%fptrToInt58098 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48989 to i64
%ae48989 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt58098)
store volatile %struct.ScmObj* %ae48989, %struct.ScmObj** %stackaddr$makeclosure58097, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48989, %struct.ScmObj* %_37foldr148051, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48989, %struct.ScmObj* %_37map148047, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48989, %struct.ScmObj* %Ycmb48030, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48989, %struct.ScmObj* %_37take48043, i64 3)
%ae48990 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58099 = alloca %struct.ScmObj*, align 8
%fptrToInt58100 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48991 to i64
%ae48991 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58100)
store volatile %struct.ScmObj* %ae48991, %struct.ScmObj** %stackaddr$makeclosure58099, align 8
%argslist57916$ae489890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58101 = alloca %struct.ScmObj*, align 8
%argslist57916$ae489891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48991, %struct.ScmObj* %argslist57916$ae489890)
store volatile %struct.ScmObj* %argslist57916$ae489891, %struct.ScmObj** %stackaddr$prim58101, align 8
%stackaddr$prim58102 = alloca %struct.ScmObj*, align 8
%argslist57916$ae489892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48990, %struct.ScmObj* %argslist57916$ae489891)
store volatile %struct.ScmObj* %argslist57916$ae489892, %struct.ScmObj** %stackaddr$prim58102, align 8
%clofunc58103 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48989)
musttail call tailcc void %clofunc58103(%struct.ScmObj* %ae48989, %struct.ScmObj* %argslist57916$ae489892)
ret void
}

define tailcc void @proc_clo$ae48989(%struct.ScmObj* %env$ae48989,%struct.ScmObj* %current_45args57382) {
%stackaddr$env-ref58104 = alloca %struct.ScmObj*, align 8
%_37foldr148051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48989, i64 0)
store %struct.ScmObj* %_37foldr148051, %struct.ScmObj** %stackaddr$env-ref58104
%stackaddr$env-ref58105 = alloca %struct.ScmObj*, align 8
%_37map148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48989, i64 1)
store %struct.ScmObj* %_37map148047, %struct.ScmObj** %stackaddr$env-ref58105
%stackaddr$env-ref58106 = alloca %struct.ScmObj*, align 8
%Ycmb48030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48989, i64 2)
store %struct.ScmObj* %Ycmb48030, %struct.ScmObj** %stackaddr$env-ref58106
%stackaddr$env-ref58107 = alloca %struct.ScmObj*, align 8
%_37take48043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48989, i64 3)
store %struct.ScmObj* %_37take48043, %struct.ScmObj** %stackaddr$env-ref58107
%stackaddr$prim58108 = alloca %struct.ScmObj*, align 8
%_95k48347 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57382)
store volatile %struct.ScmObj* %_95k48347, %struct.ScmObj** %stackaddr$prim58108, align 8
%stackaddr$prim58109 = alloca %struct.ScmObj*, align 8
%current_45args57383 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57382)
store volatile %struct.ScmObj* %current_45args57383, %struct.ScmObj** %stackaddr$prim58109, align 8
%stackaddr$prim58110 = alloca %struct.ScmObj*, align 8
%anf_45bind48198 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57383)
store volatile %struct.ScmObj* %anf_45bind48198, %struct.ScmObj** %stackaddr$prim58110, align 8
%stackaddr$makeclosure58111 = alloca %struct.ScmObj*, align 8
%fptrToInt58112 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49070 to i64
%ae49070 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt58112)
store volatile %struct.ScmObj* %ae49070, %struct.ScmObj** %stackaddr$makeclosure58111, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49070, %struct.ScmObj* %_37foldr148051, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49070, %struct.ScmObj* %_37map148047, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49070, %struct.ScmObj* %Ycmb48030, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49070, %struct.ScmObj* %_37take48043, i64 3)
%argslist57902$Ycmb480300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58113 = alloca %struct.ScmObj*, align 8
%argslist57902$Ycmb480301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48198, %struct.ScmObj* %argslist57902$Ycmb480300)
store volatile %struct.ScmObj* %argslist57902$Ycmb480301, %struct.ScmObj** %stackaddr$prim58113, align 8
%stackaddr$prim58114 = alloca %struct.ScmObj*, align 8
%argslist57902$Ycmb480302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49070, %struct.ScmObj* %argslist57902$Ycmb480301)
store volatile %struct.ScmObj* %argslist57902$Ycmb480302, %struct.ScmObj** %stackaddr$prim58114, align 8
%clofunc58115 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48030)
musttail call tailcc void %clofunc58115(%struct.ScmObj* %Ycmb48030, %struct.ScmObj* %argslist57902$Ycmb480302)
ret void
}

define tailcc void @proc_clo$ae49070(%struct.ScmObj* %env$ae49070,%struct.ScmObj* %current_45args57385) {
%stackaddr$env-ref58116 = alloca %struct.ScmObj*, align 8
%_37foldr148051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49070, i64 0)
store %struct.ScmObj* %_37foldr148051, %struct.ScmObj** %stackaddr$env-ref58116
%stackaddr$env-ref58117 = alloca %struct.ScmObj*, align 8
%_37map148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49070, i64 1)
store %struct.ScmObj* %_37map148047, %struct.ScmObj** %stackaddr$env-ref58117
%stackaddr$env-ref58118 = alloca %struct.ScmObj*, align 8
%Ycmb48030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49070, i64 2)
store %struct.ScmObj* %Ycmb48030, %struct.ScmObj** %stackaddr$env-ref58118
%stackaddr$env-ref58119 = alloca %struct.ScmObj*, align 8
%_37take48043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49070, i64 3)
store %struct.ScmObj* %_37take48043, %struct.ScmObj** %stackaddr$env-ref58119
%stackaddr$prim58120 = alloca %struct.ScmObj*, align 8
%_95k48348 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57385)
store volatile %struct.ScmObj* %_95k48348, %struct.ScmObj** %stackaddr$prim58120, align 8
%stackaddr$prim58121 = alloca %struct.ScmObj*, align 8
%current_45args57386 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57385)
store volatile %struct.ScmObj* %current_45args57386, %struct.ScmObj** %stackaddr$prim58121, align 8
%stackaddr$prim58122 = alloca %struct.ScmObj*, align 8
%_37length48040 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57386)
store volatile %struct.ScmObj* %_37length48040, %struct.ScmObj** %stackaddr$prim58122, align 8
%stackaddr$makeclosure58123 = alloca %struct.ScmObj*, align 8
%fptrToInt58124 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49072 to i64
%ae49072 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt58124)
store volatile %struct.ScmObj* %ae49072, %struct.ScmObj** %stackaddr$makeclosure58123, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49072, %struct.ScmObj* %_37length48040, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49072, %struct.ScmObj* %_37foldr148051, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49072, %struct.ScmObj* %_37map148047, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49072, %struct.ScmObj* %Ycmb48030, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49072, %struct.ScmObj* %_37take48043, i64 4)
%ae49073 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58125 = alloca %struct.ScmObj*, align 8
%fptrToInt58126 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49074 to i64
%ae49074 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58126)
store volatile %struct.ScmObj* %ae49074, %struct.ScmObj** %stackaddr$makeclosure58125, align 8
%argslist57901$ae490720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58127 = alloca %struct.ScmObj*, align 8
%argslist57901$ae490721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49074, %struct.ScmObj* %argslist57901$ae490720)
store volatile %struct.ScmObj* %argslist57901$ae490721, %struct.ScmObj** %stackaddr$prim58127, align 8
%stackaddr$prim58128 = alloca %struct.ScmObj*, align 8
%argslist57901$ae490722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49073, %struct.ScmObj* %argslist57901$ae490721)
store volatile %struct.ScmObj* %argslist57901$ae490722, %struct.ScmObj** %stackaddr$prim58128, align 8
%clofunc58129 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49072)
musttail call tailcc void %clofunc58129(%struct.ScmObj* %ae49072, %struct.ScmObj* %argslist57901$ae490722)
ret void
}

define tailcc void @proc_clo$ae49072(%struct.ScmObj* %env$ae49072,%struct.ScmObj* %current_45args57388) {
%stackaddr$env-ref58130 = alloca %struct.ScmObj*, align 8
%_37length48040 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49072, i64 0)
store %struct.ScmObj* %_37length48040, %struct.ScmObj** %stackaddr$env-ref58130
%stackaddr$env-ref58131 = alloca %struct.ScmObj*, align 8
%_37foldr148051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49072, i64 1)
store %struct.ScmObj* %_37foldr148051, %struct.ScmObj** %stackaddr$env-ref58131
%stackaddr$env-ref58132 = alloca %struct.ScmObj*, align 8
%_37map148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49072, i64 2)
store %struct.ScmObj* %_37map148047, %struct.ScmObj** %stackaddr$env-ref58132
%stackaddr$env-ref58133 = alloca %struct.ScmObj*, align 8
%Ycmb48030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49072, i64 3)
store %struct.ScmObj* %Ycmb48030, %struct.ScmObj** %stackaddr$env-ref58133
%stackaddr$env-ref58134 = alloca %struct.ScmObj*, align 8
%_37take48043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49072, i64 4)
store %struct.ScmObj* %_37take48043, %struct.ScmObj** %stackaddr$env-ref58134
%stackaddr$prim58135 = alloca %struct.ScmObj*, align 8
%_95k48349 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57388)
store volatile %struct.ScmObj* %_95k48349, %struct.ScmObj** %stackaddr$prim58135, align 8
%stackaddr$prim58136 = alloca %struct.ScmObj*, align 8
%current_45args57389 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57388)
store volatile %struct.ScmObj* %current_45args57389, %struct.ScmObj** %stackaddr$prim58136, align 8
%stackaddr$prim58137 = alloca %struct.ScmObj*, align 8
%anf_45bind48203 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57389)
store volatile %struct.ScmObj* %anf_45bind48203, %struct.ScmObj** %stackaddr$prim58137, align 8
%stackaddr$makeclosure58138 = alloca %struct.ScmObj*, align 8
%fptrToInt58139 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49149 to i64
%ae49149 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt58139)
store volatile %struct.ScmObj* %ae49149, %struct.ScmObj** %stackaddr$makeclosure58138, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49149, %struct.ScmObj* %_37length48040, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49149, %struct.ScmObj* %_37foldr148051, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49149, %struct.ScmObj* %_37map148047, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49149, %struct.ScmObj* %Ycmb48030, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49149, %struct.ScmObj* %_37take48043, i64 4)
%argslist57885$Ycmb480300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58140 = alloca %struct.ScmObj*, align 8
%argslist57885$Ycmb480301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48203, %struct.ScmObj* %argslist57885$Ycmb480300)
store volatile %struct.ScmObj* %argslist57885$Ycmb480301, %struct.ScmObj** %stackaddr$prim58140, align 8
%stackaddr$prim58141 = alloca %struct.ScmObj*, align 8
%argslist57885$Ycmb480302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49149, %struct.ScmObj* %argslist57885$Ycmb480301)
store volatile %struct.ScmObj* %argslist57885$Ycmb480302, %struct.ScmObj** %stackaddr$prim58141, align 8
%clofunc58142 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48030)
musttail call tailcc void %clofunc58142(%struct.ScmObj* %Ycmb48030, %struct.ScmObj* %argslist57885$Ycmb480302)
ret void
}

define tailcc void @proc_clo$ae49149(%struct.ScmObj* %env$ae49149,%struct.ScmObj* %current_45args57391) {
%stackaddr$env-ref58143 = alloca %struct.ScmObj*, align 8
%_37length48040 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49149, i64 0)
store %struct.ScmObj* %_37length48040, %struct.ScmObj** %stackaddr$env-ref58143
%stackaddr$env-ref58144 = alloca %struct.ScmObj*, align 8
%_37foldr148051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49149, i64 1)
store %struct.ScmObj* %_37foldr148051, %struct.ScmObj** %stackaddr$env-ref58144
%stackaddr$env-ref58145 = alloca %struct.ScmObj*, align 8
%_37map148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49149, i64 2)
store %struct.ScmObj* %_37map148047, %struct.ScmObj** %stackaddr$env-ref58145
%stackaddr$env-ref58146 = alloca %struct.ScmObj*, align 8
%Ycmb48030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49149, i64 3)
store %struct.ScmObj* %Ycmb48030, %struct.ScmObj** %stackaddr$env-ref58146
%stackaddr$env-ref58147 = alloca %struct.ScmObj*, align 8
%_37take48043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49149, i64 4)
store %struct.ScmObj* %_37take48043, %struct.ScmObj** %stackaddr$env-ref58147
%stackaddr$prim58148 = alloca %struct.ScmObj*, align 8
%_95k48350 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57391)
store volatile %struct.ScmObj* %_95k48350, %struct.ScmObj** %stackaddr$prim58148, align 8
%stackaddr$prim58149 = alloca %struct.ScmObj*, align 8
%current_45args57392 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57391)
store volatile %struct.ScmObj* %current_45args57392, %struct.ScmObj** %stackaddr$prim58149, align 8
%stackaddr$prim58150 = alloca %struct.ScmObj*, align 8
%_37foldl148035 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57392)
store volatile %struct.ScmObj* %_37foldl148035, %struct.ScmObj** %stackaddr$prim58150, align 8
%stackaddr$makeclosure58151 = alloca %struct.ScmObj*, align 8
%fptrToInt58152 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49151 to i64
%ae49151 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt58152)
store volatile %struct.ScmObj* %ae49151, %struct.ScmObj** %stackaddr$makeclosure58151, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49151, %struct.ScmObj* %_37foldr148051, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49151, %struct.ScmObj* %_37foldl148035, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49151, %struct.ScmObj* %_37length48040, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49151, %struct.ScmObj* %_37map148047, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49151, %struct.ScmObj* %Ycmb48030, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49151, %struct.ScmObj* %_37take48043, i64 5)
%ae49152 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58153 = alloca %struct.ScmObj*, align 8
%fptrToInt58154 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49153 to i64
%ae49153 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58154)
store volatile %struct.ScmObj* %ae49153, %struct.ScmObj** %stackaddr$makeclosure58153, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49153, %struct.ScmObj* %_37foldl148035, i64 0)
%argslist57884$ae491510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58155 = alloca %struct.ScmObj*, align 8
%argslist57884$ae491511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49153, %struct.ScmObj* %argslist57884$ae491510)
store volatile %struct.ScmObj* %argslist57884$ae491511, %struct.ScmObj** %stackaddr$prim58155, align 8
%stackaddr$prim58156 = alloca %struct.ScmObj*, align 8
%argslist57884$ae491512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49152, %struct.ScmObj* %argslist57884$ae491511)
store volatile %struct.ScmObj* %argslist57884$ae491512, %struct.ScmObj** %stackaddr$prim58156, align 8
%clofunc58157 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49151)
musttail call tailcc void %clofunc58157(%struct.ScmObj* %ae49151, %struct.ScmObj* %argslist57884$ae491512)
ret void
}

define tailcc void @proc_clo$ae49151(%struct.ScmObj* %env$ae49151,%struct.ScmObj* %current_45args57394) {
%stackaddr$env-ref58158 = alloca %struct.ScmObj*, align 8
%_37foldr148051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49151, i64 0)
store %struct.ScmObj* %_37foldr148051, %struct.ScmObj** %stackaddr$env-ref58158
%stackaddr$env-ref58159 = alloca %struct.ScmObj*, align 8
%_37foldl148035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49151, i64 1)
store %struct.ScmObj* %_37foldl148035, %struct.ScmObj** %stackaddr$env-ref58159
%stackaddr$env-ref58160 = alloca %struct.ScmObj*, align 8
%_37length48040 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49151, i64 2)
store %struct.ScmObj* %_37length48040, %struct.ScmObj** %stackaddr$env-ref58160
%stackaddr$env-ref58161 = alloca %struct.ScmObj*, align 8
%_37map148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49151, i64 3)
store %struct.ScmObj* %_37map148047, %struct.ScmObj** %stackaddr$env-ref58161
%stackaddr$env-ref58162 = alloca %struct.ScmObj*, align 8
%Ycmb48030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49151, i64 4)
store %struct.ScmObj* %Ycmb48030, %struct.ScmObj** %stackaddr$env-ref58162
%stackaddr$env-ref58163 = alloca %struct.ScmObj*, align 8
%_37take48043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49151, i64 5)
store %struct.ScmObj* %_37take48043, %struct.ScmObj** %stackaddr$env-ref58163
%stackaddr$prim58164 = alloca %struct.ScmObj*, align 8
%_95k48351 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57394)
store volatile %struct.ScmObj* %_95k48351, %struct.ScmObj** %stackaddr$prim58164, align 8
%stackaddr$prim58165 = alloca %struct.ScmObj*, align 8
%current_45args57395 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57394)
store volatile %struct.ScmObj* %current_45args57395, %struct.ScmObj** %stackaddr$prim58165, align 8
%stackaddr$prim58166 = alloca %struct.ScmObj*, align 8
%_37last48073 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57395)
store volatile %struct.ScmObj* %_37last48073, %struct.ScmObj** %stackaddr$prim58166, align 8
%stackaddr$makeclosure58167 = alloca %struct.ScmObj*, align 8
%fptrToInt58168 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49205 to i64
%ae49205 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt58168)
store volatile %struct.ScmObj* %ae49205, %struct.ScmObj** %stackaddr$makeclosure58167, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49205, %struct.ScmObj* %_37foldr148051, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49205, %struct.ScmObj* %_37foldl148035, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49205, %struct.ScmObj* %_37last48073, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49205, %struct.ScmObj* %_37map148047, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49205, %struct.ScmObj* %Ycmb48030, i64 4)
%ae49206 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58169 = alloca %struct.ScmObj*, align 8
%fptrToInt58170 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49207 to i64
%ae49207 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58170)
store volatile %struct.ScmObj* %ae49207, %struct.ScmObj** %stackaddr$makeclosure58169, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49207, %struct.ScmObj* %_37length48040, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49207, %struct.ScmObj* %_37take48043, i64 1)
%argslist57870$ae492050 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58171 = alloca %struct.ScmObj*, align 8
%argslist57870$ae492051 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49207, %struct.ScmObj* %argslist57870$ae492050)
store volatile %struct.ScmObj* %argslist57870$ae492051, %struct.ScmObj** %stackaddr$prim58171, align 8
%stackaddr$prim58172 = alloca %struct.ScmObj*, align 8
%argslist57870$ae492052 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49206, %struct.ScmObj* %argslist57870$ae492051)
store volatile %struct.ScmObj* %argslist57870$ae492052, %struct.ScmObj** %stackaddr$prim58172, align 8
%clofunc58173 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49205)
musttail call tailcc void %clofunc58173(%struct.ScmObj* %ae49205, %struct.ScmObj* %argslist57870$ae492052)
ret void
}

define tailcc void @proc_clo$ae49205(%struct.ScmObj* %env$ae49205,%struct.ScmObj* %current_45args57397) {
%stackaddr$env-ref58174 = alloca %struct.ScmObj*, align 8
%_37foldr148051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49205, i64 0)
store %struct.ScmObj* %_37foldr148051, %struct.ScmObj** %stackaddr$env-ref58174
%stackaddr$env-ref58175 = alloca %struct.ScmObj*, align 8
%_37foldl148035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49205, i64 1)
store %struct.ScmObj* %_37foldl148035, %struct.ScmObj** %stackaddr$env-ref58175
%stackaddr$env-ref58176 = alloca %struct.ScmObj*, align 8
%_37last48073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49205, i64 2)
store %struct.ScmObj* %_37last48073, %struct.ScmObj** %stackaddr$env-ref58176
%stackaddr$env-ref58177 = alloca %struct.ScmObj*, align 8
%_37map148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49205, i64 3)
store %struct.ScmObj* %_37map148047, %struct.ScmObj** %stackaddr$env-ref58177
%stackaddr$env-ref58178 = alloca %struct.ScmObj*, align 8
%Ycmb48030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49205, i64 4)
store %struct.ScmObj* %Ycmb48030, %struct.ScmObj** %stackaddr$env-ref58178
%stackaddr$prim58179 = alloca %struct.ScmObj*, align 8
%_95k48352 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57397)
store volatile %struct.ScmObj* %_95k48352, %struct.ScmObj** %stackaddr$prim58179, align 8
%stackaddr$prim58180 = alloca %struct.ScmObj*, align 8
%current_45args57398 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57397)
store volatile %struct.ScmObj* %current_45args57398, %struct.ScmObj** %stackaddr$prim58180, align 8
%stackaddr$prim58181 = alloca %struct.ScmObj*, align 8
%_37drop_45right48070 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57398)
store volatile %struct.ScmObj* %_37drop_45right48070, %struct.ScmObj** %stackaddr$prim58181, align 8
%stackaddr$makeclosure58182 = alloca %struct.ScmObj*, align 8
%fptrToInt58183 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49235 to i64
%ae49235 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt58183)
store volatile %struct.ScmObj* %ae49235, %struct.ScmObj** %stackaddr$makeclosure58182, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49235, %struct.ScmObj* %_37foldr148051, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49235, %struct.ScmObj* %_37foldl148035, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49235, %struct.ScmObj* %_37last48073, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49235, %struct.ScmObj* %_37drop_45right48070, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49235, %struct.ScmObj* %Ycmb48030, i64 4)
%ae49236 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58184 = alloca %struct.ScmObj*, align 8
%fptrToInt58185 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49237 to i64
%ae49237 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58185)
store volatile %struct.ScmObj* %ae49237, %struct.ScmObj** %stackaddr$makeclosure58184, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49237, %struct.ScmObj* %_37foldr148051, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49237, %struct.ScmObj* %_37map148047, i64 1)
%argslist57860$ae492350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58186 = alloca %struct.ScmObj*, align 8
%argslist57860$ae492351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49237, %struct.ScmObj* %argslist57860$ae492350)
store volatile %struct.ScmObj* %argslist57860$ae492351, %struct.ScmObj** %stackaddr$prim58186, align 8
%stackaddr$prim58187 = alloca %struct.ScmObj*, align 8
%argslist57860$ae492352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49236, %struct.ScmObj* %argslist57860$ae492351)
store volatile %struct.ScmObj* %argslist57860$ae492352, %struct.ScmObj** %stackaddr$prim58187, align 8
%clofunc58188 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49235)
musttail call tailcc void %clofunc58188(%struct.ScmObj* %ae49235, %struct.ScmObj* %argslist57860$ae492352)
ret void
}

define tailcc void @proc_clo$ae49235(%struct.ScmObj* %env$ae49235,%struct.ScmObj* %current_45args57400) {
%stackaddr$env-ref58189 = alloca %struct.ScmObj*, align 8
%_37foldr148051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49235, i64 0)
store %struct.ScmObj* %_37foldr148051, %struct.ScmObj** %stackaddr$env-ref58189
%stackaddr$env-ref58190 = alloca %struct.ScmObj*, align 8
%_37foldl148035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49235, i64 1)
store %struct.ScmObj* %_37foldl148035, %struct.ScmObj** %stackaddr$env-ref58190
%stackaddr$env-ref58191 = alloca %struct.ScmObj*, align 8
%_37last48073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49235, i64 2)
store %struct.ScmObj* %_37last48073, %struct.ScmObj** %stackaddr$env-ref58191
%stackaddr$env-ref58192 = alloca %struct.ScmObj*, align 8
%_37drop_45right48070 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49235, i64 3)
store %struct.ScmObj* %_37drop_45right48070, %struct.ScmObj** %stackaddr$env-ref58192
%stackaddr$env-ref58193 = alloca %struct.ScmObj*, align 8
%Ycmb48030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49235, i64 4)
store %struct.ScmObj* %Ycmb48030, %struct.ScmObj** %stackaddr$env-ref58193
%stackaddr$prim58194 = alloca %struct.ScmObj*, align 8
%_95k48353 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57400)
store volatile %struct.ScmObj* %_95k48353, %struct.ScmObj** %stackaddr$prim58194, align 8
%stackaddr$prim58195 = alloca %struct.ScmObj*, align 8
%current_45args57401 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57400)
store volatile %struct.ScmObj* %current_45args57401, %struct.ScmObj** %stackaddr$prim58195, align 8
%stackaddr$prim58196 = alloca %struct.ScmObj*, align 8
%anf_45bind48219 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57401)
store volatile %struct.ScmObj* %anf_45bind48219, %struct.ScmObj** %stackaddr$prim58196, align 8
%stackaddr$makeclosure58197 = alloca %struct.ScmObj*, align 8
%fptrToInt58198 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49619 to i64
%ae49619 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt58198)
store volatile %struct.ScmObj* %ae49619, %struct.ScmObj** %stackaddr$makeclosure58197, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49619, %struct.ScmObj* %_37foldr148051, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49619, %struct.ScmObj* %_37foldl148035, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49619, %struct.ScmObj* %_37last48073, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49619, %struct.ScmObj* %_37drop_45right48070, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49619, %struct.ScmObj* %Ycmb48030, i64 4)
%argslist57800$Ycmb480300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58199 = alloca %struct.ScmObj*, align 8
%argslist57800$Ycmb480301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48219, %struct.ScmObj* %argslist57800$Ycmb480300)
store volatile %struct.ScmObj* %argslist57800$Ycmb480301, %struct.ScmObj** %stackaddr$prim58199, align 8
%stackaddr$prim58200 = alloca %struct.ScmObj*, align 8
%argslist57800$Ycmb480302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49619, %struct.ScmObj* %argslist57800$Ycmb480301)
store volatile %struct.ScmObj* %argslist57800$Ycmb480302, %struct.ScmObj** %stackaddr$prim58200, align 8
%clofunc58201 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48030)
musttail call tailcc void %clofunc58201(%struct.ScmObj* %Ycmb48030, %struct.ScmObj* %argslist57800$Ycmb480302)
ret void
}

define tailcc void @proc_clo$ae49619(%struct.ScmObj* %env$ae49619,%struct.ScmObj* %current_45args57403) {
%stackaddr$env-ref58202 = alloca %struct.ScmObj*, align 8
%_37foldr148051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49619, i64 0)
store %struct.ScmObj* %_37foldr148051, %struct.ScmObj** %stackaddr$env-ref58202
%stackaddr$env-ref58203 = alloca %struct.ScmObj*, align 8
%_37foldl148035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49619, i64 1)
store %struct.ScmObj* %_37foldl148035, %struct.ScmObj** %stackaddr$env-ref58203
%stackaddr$env-ref58204 = alloca %struct.ScmObj*, align 8
%_37last48073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49619, i64 2)
store %struct.ScmObj* %_37last48073, %struct.ScmObj** %stackaddr$env-ref58204
%stackaddr$env-ref58205 = alloca %struct.ScmObj*, align 8
%_37drop_45right48070 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49619, i64 3)
store %struct.ScmObj* %_37drop_45right48070, %struct.ScmObj** %stackaddr$env-ref58205
%stackaddr$env-ref58206 = alloca %struct.ScmObj*, align 8
%Ycmb48030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49619, i64 4)
store %struct.ScmObj* %Ycmb48030, %struct.ScmObj** %stackaddr$env-ref58206
%stackaddr$prim58207 = alloca %struct.ScmObj*, align 8
%_95k48354 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57403)
store volatile %struct.ScmObj* %_95k48354, %struct.ScmObj** %stackaddr$prim58207, align 8
%stackaddr$prim58208 = alloca %struct.ScmObj*, align 8
%current_45args57404 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57403)
store volatile %struct.ScmObj* %current_45args57404, %struct.ScmObj** %stackaddr$prim58208, align 8
%stackaddr$prim58209 = alloca %struct.ScmObj*, align 8
%_37foldr48056 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57404)
store volatile %struct.ScmObj* %_37foldr48056, %struct.ScmObj** %stackaddr$prim58209, align 8
%stackaddr$makeclosure58210 = alloca %struct.ScmObj*, align 8
%fptrToInt58211 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49621 to i64
%ae49621 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt58211)
store volatile %struct.ScmObj* %ae49621, %struct.ScmObj** %stackaddr$makeclosure58210, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49621, %struct.ScmObj* %_37foldr148051, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49621, %struct.ScmObj* %_37foldl148035, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49621, %struct.ScmObj* %_37last48073, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49621, %struct.ScmObj* %_37foldr48056, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49621, %struct.ScmObj* %_37drop_45right48070, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49621, %struct.ScmObj* %Ycmb48030, i64 5)
%ae49622 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58212 = alloca %struct.ScmObj*, align 8
%fptrToInt58213 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49623 to i64
%ae49623 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58213)
store volatile %struct.ScmObj* %ae49623, %struct.ScmObj** %stackaddr$makeclosure58212, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49623, %struct.ScmObj* %_37foldr148051, i64 0)
%argslist57799$ae496210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58214 = alloca %struct.ScmObj*, align 8
%argslist57799$ae496211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49623, %struct.ScmObj* %argslist57799$ae496210)
store volatile %struct.ScmObj* %argslist57799$ae496211, %struct.ScmObj** %stackaddr$prim58214, align 8
%stackaddr$prim58215 = alloca %struct.ScmObj*, align 8
%argslist57799$ae496212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49622, %struct.ScmObj* %argslist57799$ae496211)
store volatile %struct.ScmObj* %argslist57799$ae496212, %struct.ScmObj** %stackaddr$prim58215, align 8
%clofunc58216 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49621)
musttail call tailcc void %clofunc58216(%struct.ScmObj* %ae49621, %struct.ScmObj* %argslist57799$ae496212)
ret void
}

define tailcc void @proc_clo$ae49621(%struct.ScmObj* %env$ae49621,%struct.ScmObj* %current_45args57406) {
%stackaddr$env-ref58217 = alloca %struct.ScmObj*, align 8
%_37foldr148051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49621, i64 0)
store %struct.ScmObj* %_37foldr148051, %struct.ScmObj** %stackaddr$env-ref58217
%stackaddr$env-ref58218 = alloca %struct.ScmObj*, align 8
%_37foldl148035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49621, i64 1)
store %struct.ScmObj* %_37foldl148035, %struct.ScmObj** %stackaddr$env-ref58218
%stackaddr$env-ref58219 = alloca %struct.ScmObj*, align 8
%_37last48073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49621, i64 2)
store %struct.ScmObj* %_37last48073, %struct.ScmObj** %stackaddr$env-ref58219
%stackaddr$env-ref58220 = alloca %struct.ScmObj*, align 8
%_37foldr48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49621, i64 3)
store %struct.ScmObj* %_37foldr48056, %struct.ScmObj** %stackaddr$env-ref58220
%stackaddr$env-ref58221 = alloca %struct.ScmObj*, align 8
%_37drop_45right48070 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49621, i64 4)
store %struct.ScmObj* %_37drop_45right48070, %struct.ScmObj** %stackaddr$env-ref58221
%stackaddr$env-ref58222 = alloca %struct.ScmObj*, align 8
%Ycmb48030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49621, i64 5)
store %struct.ScmObj* %Ycmb48030, %struct.ScmObj** %stackaddr$env-ref58222
%stackaddr$prim58223 = alloca %struct.ScmObj*, align 8
%_95k48355 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57406)
store volatile %struct.ScmObj* %_95k48355, %struct.ScmObj** %stackaddr$prim58223, align 8
%stackaddr$prim58224 = alloca %struct.ScmObj*, align 8
%current_45args57407 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57406)
store volatile %struct.ScmObj* %current_45args57407, %struct.ScmObj** %stackaddr$prim58224, align 8
%stackaddr$prim58225 = alloca %struct.ScmObj*, align 8
%_37map148082 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57407)
store volatile %struct.ScmObj* %_37map148082, %struct.ScmObj** %stackaddr$prim58225, align 8
%stackaddr$makeclosure58226 = alloca %struct.ScmObj*, align 8
%fptrToInt58227 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49698 to i64
%ae49698 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt58227)
store volatile %struct.ScmObj* %ae49698, %struct.ScmObj** %stackaddr$makeclosure58226, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49698, %struct.ScmObj* %_37foldr148051, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49698, %struct.ScmObj* %_37foldl148035, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49698, %struct.ScmObj* %_37foldr48056, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49698, %struct.ScmObj* %_37map148082, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49698, %struct.ScmObj* %Ycmb48030, i64 4)
%ae49699 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58228 = alloca %struct.ScmObj*, align 8
%fptrToInt58229 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49700 to i64
%ae49700 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58229)
store volatile %struct.ScmObj* %ae49700, %struct.ScmObj** %stackaddr$makeclosure58228, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49700, %struct.ScmObj* %_37last48073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49700, %struct.ScmObj* %_37foldr48056, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49700, %struct.ScmObj* %_37drop_45right48070, i64 2)
%argslist57780$ae496980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58230 = alloca %struct.ScmObj*, align 8
%argslist57780$ae496981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49700, %struct.ScmObj* %argslist57780$ae496980)
store volatile %struct.ScmObj* %argslist57780$ae496981, %struct.ScmObj** %stackaddr$prim58230, align 8
%stackaddr$prim58231 = alloca %struct.ScmObj*, align 8
%argslist57780$ae496982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49699, %struct.ScmObj* %argslist57780$ae496981)
store volatile %struct.ScmObj* %argslist57780$ae496982, %struct.ScmObj** %stackaddr$prim58231, align 8
%clofunc58232 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49698)
musttail call tailcc void %clofunc58232(%struct.ScmObj* %ae49698, %struct.ScmObj* %argslist57780$ae496982)
ret void
}

define tailcc void @proc_clo$ae49698(%struct.ScmObj* %env$ae49698,%struct.ScmObj* %current_45args57409) {
%stackaddr$env-ref58233 = alloca %struct.ScmObj*, align 8
%_37foldr148051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49698, i64 0)
store %struct.ScmObj* %_37foldr148051, %struct.ScmObj** %stackaddr$env-ref58233
%stackaddr$env-ref58234 = alloca %struct.ScmObj*, align 8
%_37foldl148035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49698, i64 1)
store %struct.ScmObj* %_37foldl148035, %struct.ScmObj** %stackaddr$env-ref58234
%stackaddr$env-ref58235 = alloca %struct.ScmObj*, align 8
%_37foldr48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49698, i64 2)
store %struct.ScmObj* %_37foldr48056, %struct.ScmObj** %stackaddr$env-ref58235
%stackaddr$env-ref58236 = alloca %struct.ScmObj*, align 8
%_37map148082 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49698, i64 3)
store %struct.ScmObj* %_37map148082, %struct.ScmObj** %stackaddr$env-ref58236
%stackaddr$env-ref58237 = alloca %struct.ScmObj*, align 8
%Ycmb48030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49698, i64 4)
store %struct.ScmObj* %Ycmb48030, %struct.ScmObj** %stackaddr$env-ref58237
%stackaddr$prim58238 = alloca %struct.ScmObj*, align 8
%_95k48356 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57409)
store volatile %struct.ScmObj* %_95k48356, %struct.ScmObj** %stackaddr$prim58238, align 8
%stackaddr$prim58239 = alloca %struct.ScmObj*, align 8
%current_45args57410 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57409)
store volatile %struct.ScmObj* %current_45args57410, %struct.ScmObj** %stackaddr$prim58239, align 8
%stackaddr$prim58240 = alloca %struct.ScmObj*, align 8
%_37map48077 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57410)
store volatile %struct.ScmObj* %_37map48077, %struct.ScmObj** %stackaddr$prim58240, align 8
%stackaddr$makeclosure58241 = alloca %struct.ScmObj*, align 8
%fptrToInt58242 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49844 to i64
%ae49844 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58242)
store volatile %struct.ScmObj* %ae49844, %struct.ScmObj** %stackaddr$makeclosure58241, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49844, %struct.ScmObj* %_37foldl148035, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49844, %struct.ScmObj* %Ycmb48030, i64 1)
%ae49845 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58243 = alloca %struct.ScmObj*, align 8
%fptrToInt58244 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49846 to i64
%ae49846 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58244)
store volatile %struct.ScmObj* %ae49846, %struct.ScmObj** %stackaddr$makeclosure58243, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49846, %struct.ScmObj* %_37foldr48056, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49846, %struct.ScmObj* %_37foldr148051, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49846, %struct.ScmObj* %_37map148082, i64 2)
%argslist57763$ae498440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58245 = alloca %struct.ScmObj*, align 8
%argslist57763$ae498441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49846, %struct.ScmObj* %argslist57763$ae498440)
store volatile %struct.ScmObj* %argslist57763$ae498441, %struct.ScmObj** %stackaddr$prim58245, align 8
%stackaddr$prim58246 = alloca %struct.ScmObj*, align 8
%argslist57763$ae498442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49845, %struct.ScmObj* %argslist57763$ae498441)
store volatile %struct.ScmObj* %argslist57763$ae498442, %struct.ScmObj** %stackaddr$prim58246, align 8
%clofunc58247 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49844)
musttail call tailcc void %clofunc58247(%struct.ScmObj* %ae49844, %struct.ScmObj* %argslist57763$ae498442)
ret void
}

define tailcc void @proc_clo$ae49844(%struct.ScmObj* %env$ae49844,%struct.ScmObj* %current_45args57412) {
%stackaddr$env-ref58248 = alloca %struct.ScmObj*, align 8
%_37foldl148035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49844, i64 0)
store %struct.ScmObj* %_37foldl148035, %struct.ScmObj** %stackaddr$env-ref58248
%stackaddr$env-ref58249 = alloca %struct.ScmObj*, align 8
%Ycmb48030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49844, i64 1)
store %struct.ScmObj* %Ycmb48030, %struct.ScmObj** %stackaddr$env-ref58249
%stackaddr$prim58250 = alloca %struct.ScmObj*, align 8
%_95k48357 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57412)
store volatile %struct.ScmObj* %_95k48357, %struct.ScmObj** %stackaddr$prim58250, align 8
%stackaddr$prim58251 = alloca %struct.ScmObj*, align 8
%current_45args57413 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57412)
store volatile %struct.ScmObj* %current_45args57413, %struct.ScmObj** %stackaddr$prim58251, align 8
%stackaddr$prim58252 = alloca %struct.ScmObj*, align 8
%anf_45bind48239 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57413)
store volatile %struct.ScmObj* %anf_45bind48239, %struct.ScmObj** %stackaddr$prim58252, align 8
%stackaddr$makeclosure58253 = alloca %struct.ScmObj*, align 8
%fptrToInt58254 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50236 to i64
%ae50236 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58254)
store volatile %struct.ScmObj* %ae50236, %struct.ScmObj** %stackaddr$makeclosure58253, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50236, %struct.ScmObj* %_37foldl148035, i64 0)
%argslist57703$Ycmb480300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58255 = alloca %struct.ScmObj*, align 8
%argslist57703$Ycmb480301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48239, %struct.ScmObj* %argslist57703$Ycmb480300)
store volatile %struct.ScmObj* %argslist57703$Ycmb480301, %struct.ScmObj** %stackaddr$prim58255, align 8
%stackaddr$prim58256 = alloca %struct.ScmObj*, align 8
%argslist57703$Ycmb480302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50236, %struct.ScmObj* %argslist57703$Ycmb480301)
store volatile %struct.ScmObj* %argslist57703$Ycmb480302, %struct.ScmObj** %stackaddr$prim58256, align 8
%clofunc58257 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48030)
musttail call tailcc void %clofunc58257(%struct.ScmObj* %Ycmb48030, %struct.ScmObj* %argslist57703$Ycmb480302)
ret void
}

define tailcc void @proc_clo$ae50236(%struct.ScmObj* %env$ae50236,%struct.ScmObj* %current_45args57415) {
%stackaddr$env-ref58258 = alloca %struct.ScmObj*, align 8
%_37foldl148035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50236, i64 0)
store %struct.ScmObj* %_37foldl148035, %struct.ScmObj** %stackaddr$env-ref58258
%stackaddr$prim58259 = alloca %struct.ScmObj*, align 8
%_95k48358 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57415)
store volatile %struct.ScmObj* %_95k48358, %struct.ScmObj** %stackaddr$prim58259, align 8
%stackaddr$prim58260 = alloca %struct.ScmObj*, align 8
%current_45args57416 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57415)
store volatile %struct.ScmObj* %current_45args57416, %struct.ScmObj** %stackaddr$prim58260, align 8
%stackaddr$prim58261 = alloca %struct.ScmObj*, align 8
%_37foldl48133 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57416)
store volatile %struct.ScmObj* %_37foldl48133, %struct.ScmObj** %stackaddr$prim58261, align 8
%stackaddr$makeclosure58262 = alloca %struct.ScmObj*, align 8
%fptrToInt58263 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50238 to i64
%ae50238 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58263)
store volatile %struct.ScmObj* %ae50238, %struct.ScmObj** %stackaddr$makeclosure58262, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50238, %struct.ScmObj* %_37foldl148035, i64 0)
%ae50239 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58264 = alloca %struct.ScmObj*, align 8
%fptrToInt58265 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50240 to i64
%ae50240 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58265)
store volatile %struct.ScmObj* %ae50240, %struct.ScmObj** %stackaddr$makeclosure58264, align 8
%argslist57702$ae502380 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58266 = alloca %struct.ScmObj*, align 8
%argslist57702$ae502381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50240, %struct.ScmObj* %argslist57702$ae502380)
store volatile %struct.ScmObj* %argslist57702$ae502381, %struct.ScmObj** %stackaddr$prim58266, align 8
%stackaddr$prim58267 = alloca %struct.ScmObj*, align 8
%argslist57702$ae502382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50239, %struct.ScmObj* %argslist57702$ae502381)
store volatile %struct.ScmObj* %argslist57702$ae502382, %struct.ScmObj** %stackaddr$prim58267, align 8
%clofunc58268 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50238)
musttail call tailcc void %clofunc58268(%struct.ScmObj* %ae50238, %struct.ScmObj* %argslist57702$ae502382)
ret void
}

define tailcc void @proc_clo$ae50238(%struct.ScmObj* %env$ae50238,%struct.ScmObj* %current_45args57418) {
%stackaddr$env-ref58269 = alloca %struct.ScmObj*, align 8
%_37foldl148035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50238, i64 0)
store %struct.ScmObj* %_37foldl148035, %struct.ScmObj** %stackaddr$env-ref58269
%stackaddr$prim58270 = alloca %struct.ScmObj*, align 8
%_95k48359 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57418)
store volatile %struct.ScmObj* %_95k48359, %struct.ScmObj** %stackaddr$prim58270, align 8
%stackaddr$prim58271 = alloca %struct.ScmObj*, align 8
%current_45args57419 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57418)
store volatile %struct.ScmObj* %current_45args57419, %struct.ScmObj** %stackaddr$prim58271, align 8
%stackaddr$prim58272 = alloca %struct.ScmObj*, align 8
%_37_6248130 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57419)
store volatile %struct.ScmObj* %_37_6248130, %struct.ScmObj** %stackaddr$prim58272, align 8
%stackaddr$makeclosure58273 = alloca %struct.ScmObj*, align 8
%fptrToInt58274 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50262 to i64
%ae50262 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58274)
store volatile %struct.ScmObj* %ae50262, %struct.ScmObj** %stackaddr$makeclosure58273, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50262, %struct.ScmObj* %_37foldl148035, i64 0)
%ae50263 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58275 = alloca %struct.ScmObj*, align 8
%fptrToInt58276 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50264 to i64
%ae50264 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58276)
store volatile %struct.ScmObj* %ae50264, %struct.ScmObj** %stackaddr$makeclosure58275, align 8
%argslist57696$ae502620 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58277 = alloca %struct.ScmObj*, align 8
%argslist57696$ae502621 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50264, %struct.ScmObj* %argslist57696$ae502620)
store volatile %struct.ScmObj* %argslist57696$ae502621, %struct.ScmObj** %stackaddr$prim58277, align 8
%stackaddr$prim58278 = alloca %struct.ScmObj*, align 8
%argslist57696$ae502622 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50263, %struct.ScmObj* %argslist57696$ae502621)
store volatile %struct.ScmObj* %argslist57696$ae502622, %struct.ScmObj** %stackaddr$prim58278, align 8
%clofunc58279 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50262)
musttail call tailcc void %clofunc58279(%struct.ScmObj* %ae50262, %struct.ScmObj* %argslist57696$ae502622)
ret void
}

define tailcc void @proc_clo$ae50262(%struct.ScmObj* %env$ae50262,%struct.ScmObj* %current_45args57421) {
%stackaddr$env-ref58280 = alloca %struct.ScmObj*, align 8
%_37foldl148035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50262, i64 0)
store %struct.ScmObj* %_37foldl148035, %struct.ScmObj** %stackaddr$env-ref58280
%stackaddr$prim58281 = alloca %struct.ScmObj*, align 8
%_95k48360 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57421)
store volatile %struct.ScmObj* %_95k48360, %struct.ScmObj** %stackaddr$prim58281, align 8
%stackaddr$prim58282 = alloca %struct.ScmObj*, align 8
%current_45args57422 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57421)
store volatile %struct.ScmObj* %current_45args57422, %struct.ScmObj** %stackaddr$prim58282, align 8
%stackaddr$prim58283 = alloca %struct.ScmObj*, align 8
%_37_62_6148127 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57422)
store volatile %struct.ScmObj* %_37_62_6148127, %struct.ScmObj** %stackaddr$prim58283, align 8
%ae50286 = call %struct.ScmObj* @const_init_int(i64 1)
%ae50287 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58284 = alloca %struct.ScmObj*, align 8
%_37append48123 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50286, %struct.ScmObj* %ae50287)
store volatile %struct.ScmObj* %_37append48123, %struct.ScmObj** %stackaddr$prim58284, align 8
%stackaddr$makeclosure58285 = alloca %struct.ScmObj*, align 8
%fptrToInt58286 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50288 to i64
%ae50288 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58286)
store volatile %struct.ScmObj* %ae50288, %struct.ScmObj** %stackaddr$makeclosure58285, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50288, %struct.ScmObj* %_37foldl148035, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50288, %struct.ScmObj* %_37append48123, i64 1)
%ae50289 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58287 = alloca %struct.ScmObj*, align 8
%fptrToInt58288 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50290 to i64
%ae50290 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58288)
store volatile %struct.ScmObj* %ae50290, %struct.ScmObj** %stackaddr$makeclosure58287, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50290, %struct.ScmObj* %_37append48123, i64 0)
%argslist57690$ae502880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58289 = alloca %struct.ScmObj*, align 8
%argslist57690$ae502881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50290, %struct.ScmObj* %argslist57690$ae502880)
store volatile %struct.ScmObj* %argslist57690$ae502881, %struct.ScmObj** %stackaddr$prim58289, align 8
%stackaddr$prim58290 = alloca %struct.ScmObj*, align 8
%argslist57690$ae502882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50289, %struct.ScmObj* %argslist57690$ae502881)
store volatile %struct.ScmObj* %argslist57690$ae502882, %struct.ScmObj** %stackaddr$prim58290, align 8
%clofunc58291 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50288)
musttail call tailcc void %clofunc58291(%struct.ScmObj* %ae50288, %struct.ScmObj* %argslist57690$ae502882)
ret void
}

define tailcc void @proc_clo$ae50288(%struct.ScmObj* %env$ae50288,%struct.ScmObj* %current_45args57424) {
%stackaddr$env-ref58292 = alloca %struct.ScmObj*, align 8
%_37foldl148035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50288, i64 0)
store %struct.ScmObj* %_37foldl148035, %struct.ScmObj** %stackaddr$env-ref58292
%stackaddr$env-ref58293 = alloca %struct.ScmObj*, align 8
%_37append48123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50288, i64 1)
store %struct.ScmObj* %_37append48123, %struct.ScmObj** %stackaddr$env-ref58293
%stackaddr$prim58294 = alloca %struct.ScmObj*, align 8
%_95k48361 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57424)
store volatile %struct.ScmObj* %_95k48361, %struct.ScmObj** %stackaddr$prim58294, align 8
%stackaddr$prim58295 = alloca %struct.ScmObj*, align 8
%current_45args57425 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57424)
store volatile %struct.ScmObj* %current_45args57425, %struct.ScmObj** %stackaddr$prim58295, align 8
%stackaddr$prim58296 = alloca %struct.ScmObj*, align 8
%anf_45bind48247 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57425)
store volatile %struct.ScmObj* %anf_45bind48247, %struct.ScmObj** %stackaddr$prim58296, align 8
%ae50356 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58297 = alloca %struct.ScmObj*, align 8
%_95048124 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append48123, %struct.ScmObj* %ae50356, %struct.ScmObj* %anf_45bind48247)
store volatile %struct.ScmObj* %_95048124, %struct.ScmObj** %stackaddr$prim58297, align 8
%ae50359 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58298 = alloca %struct.ScmObj*, align 8
%_37append48122 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append48123, %struct.ScmObj* %ae50359)
store volatile %struct.ScmObj* %_37append48122, %struct.ScmObj** %stackaddr$prim58298, align 8
%stackaddr$makeclosure58299 = alloca %struct.ScmObj*, align 8
%fptrToInt58300 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50360 to i64
%ae50360 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58300)
store volatile %struct.ScmObj* %ae50360, %struct.ScmObj** %stackaddr$makeclosure58299, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50360, %struct.ScmObj* %_37foldl148035, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50360, %struct.ScmObj* %_37append48122, i64 1)
%ae50361 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58301 = alloca %struct.ScmObj*, align 8
%fptrToInt58302 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50362 to i64
%ae50362 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58302)
store volatile %struct.ScmObj* %ae50362, %struct.ScmObj** %stackaddr$makeclosure58301, align 8
%argslist57679$ae503600 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58303 = alloca %struct.ScmObj*, align 8
%argslist57679$ae503601 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50362, %struct.ScmObj* %argslist57679$ae503600)
store volatile %struct.ScmObj* %argslist57679$ae503601, %struct.ScmObj** %stackaddr$prim58303, align 8
%stackaddr$prim58304 = alloca %struct.ScmObj*, align 8
%argslist57679$ae503602 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50361, %struct.ScmObj* %argslist57679$ae503601)
store volatile %struct.ScmObj* %argslist57679$ae503602, %struct.ScmObj** %stackaddr$prim58304, align 8
%clofunc58305 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50360)
musttail call tailcc void %clofunc58305(%struct.ScmObj* %ae50360, %struct.ScmObj* %argslist57679$ae503602)
ret void
}

define tailcc void @proc_clo$ae50360(%struct.ScmObj* %env$ae50360,%struct.ScmObj* %current_45args57427) {
%stackaddr$env-ref58306 = alloca %struct.ScmObj*, align 8
%_37foldl148035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50360, i64 0)
store %struct.ScmObj* %_37foldl148035, %struct.ScmObj** %stackaddr$env-ref58306
%stackaddr$env-ref58307 = alloca %struct.ScmObj*, align 8
%_37append48122 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50360, i64 1)
store %struct.ScmObj* %_37append48122, %struct.ScmObj** %stackaddr$env-ref58307
%stackaddr$prim58308 = alloca %struct.ScmObj*, align 8
%_95k48362 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57427)
store volatile %struct.ScmObj* %_95k48362, %struct.ScmObj** %stackaddr$prim58308, align 8
%stackaddr$prim58309 = alloca %struct.ScmObj*, align 8
%current_45args57428 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57427)
store volatile %struct.ScmObj* %current_45args57428, %struct.ScmObj** %stackaddr$prim58309, align 8
%stackaddr$prim58310 = alloca %struct.ScmObj*, align 8
%_37list_6348115 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57428)
store volatile %struct.ScmObj* %_37list_6348115, %struct.ScmObj** %stackaddr$prim58310, align 8
%stackaddr$makeclosure58311 = alloca %struct.ScmObj*, align 8
%fptrToInt58312 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50776 to i64
%ae50776 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58312)
store volatile %struct.ScmObj* %ae50776, %struct.ScmObj** %stackaddr$makeclosure58311, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50776, %struct.ScmObj* %_37foldl148035, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50776, %struct.ScmObj* %_37append48122, i64 1)
%ae50777 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58313 = alloca %struct.ScmObj*, align 8
%fptrToInt58314 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50778 to i64
%ae50778 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58314)
store volatile %struct.ScmObj* %ae50778, %struct.ScmObj** %stackaddr$makeclosure58313, align 8
%argslist57654$ae507760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58315 = alloca %struct.ScmObj*, align 8
%argslist57654$ae507761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50778, %struct.ScmObj* %argslist57654$ae507760)
store volatile %struct.ScmObj* %argslist57654$ae507761, %struct.ScmObj** %stackaddr$prim58315, align 8
%stackaddr$prim58316 = alloca %struct.ScmObj*, align 8
%argslist57654$ae507762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50777, %struct.ScmObj* %argslist57654$ae507761)
store volatile %struct.ScmObj* %argslist57654$ae507762, %struct.ScmObj** %stackaddr$prim58316, align 8
%clofunc58317 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50776)
musttail call tailcc void %clofunc58317(%struct.ScmObj* %ae50776, %struct.ScmObj* %argslist57654$ae507762)
ret void
}

define tailcc void @proc_clo$ae50776(%struct.ScmObj* %env$ae50776,%struct.ScmObj* %current_45args57430) {
%stackaddr$env-ref58318 = alloca %struct.ScmObj*, align 8
%_37foldl148035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50776, i64 0)
store %struct.ScmObj* %_37foldl148035, %struct.ScmObj** %stackaddr$env-ref58318
%stackaddr$env-ref58319 = alloca %struct.ScmObj*, align 8
%_37append48122 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50776, i64 1)
store %struct.ScmObj* %_37append48122, %struct.ScmObj** %stackaddr$env-ref58319
%stackaddr$prim58320 = alloca %struct.ScmObj*, align 8
%_95k48363 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57430)
store volatile %struct.ScmObj* %_95k48363, %struct.ScmObj** %stackaddr$prim58320, align 8
%stackaddr$prim58321 = alloca %struct.ScmObj*, align 8
%current_45args57431 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57430)
store volatile %struct.ScmObj* %current_45args57431, %struct.ScmObj** %stackaddr$prim58321, align 8
%stackaddr$prim58322 = alloca %struct.ScmObj*, align 8
%_37drop48106 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57431)
store volatile %struct.ScmObj* %_37drop48106, %struct.ScmObj** %stackaddr$prim58322, align 8
%stackaddr$makeclosure58323 = alloca %struct.ScmObj*, align 8
%fptrToInt58324 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51312 to i64
%ae51312 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58324)
store volatile %struct.ScmObj* %ae51312, %struct.ScmObj** %stackaddr$makeclosure58323, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51312, %struct.ScmObj* %_37foldl148035, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51312, %struct.ScmObj* %_37append48122, i64 1)
%ae51313 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58325 = alloca %struct.ScmObj*, align 8
%fptrToInt58326 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51314 to i64
%ae51314 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58326)
store volatile %struct.ScmObj* %ae51314, %struct.ScmObj** %stackaddr$makeclosure58325, align 8
%argslist57630$ae513120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58327 = alloca %struct.ScmObj*, align 8
%argslist57630$ae513121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51314, %struct.ScmObj* %argslist57630$ae513120)
store volatile %struct.ScmObj* %argslist57630$ae513121, %struct.ScmObj** %stackaddr$prim58327, align 8
%stackaddr$prim58328 = alloca %struct.ScmObj*, align 8
%argslist57630$ae513122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51313, %struct.ScmObj* %argslist57630$ae513121)
store volatile %struct.ScmObj* %argslist57630$ae513122, %struct.ScmObj** %stackaddr$prim58328, align 8
%clofunc58329 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51312)
musttail call tailcc void %clofunc58329(%struct.ScmObj* %ae51312, %struct.ScmObj* %argslist57630$ae513122)
ret void
}

define tailcc void @proc_clo$ae51312(%struct.ScmObj* %env$ae51312,%struct.ScmObj* %current_45args57433) {
%stackaddr$env-ref58330 = alloca %struct.ScmObj*, align 8
%_37foldl148035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51312, i64 0)
store %struct.ScmObj* %_37foldl148035, %struct.ScmObj** %stackaddr$env-ref58330
%stackaddr$env-ref58331 = alloca %struct.ScmObj*, align 8
%_37append48122 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51312, i64 1)
store %struct.ScmObj* %_37append48122, %struct.ScmObj** %stackaddr$env-ref58331
%stackaddr$prim58332 = alloca %struct.ScmObj*, align 8
%_95k48364 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57433)
store volatile %struct.ScmObj* %_95k48364, %struct.ScmObj** %stackaddr$prim58332, align 8
%stackaddr$prim58333 = alloca %struct.ScmObj*, align 8
%current_45args57434 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57433)
store volatile %struct.ScmObj* %current_45args57434, %struct.ScmObj** %stackaddr$prim58333, align 8
%stackaddr$prim58334 = alloca %struct.ScmObj*, align 8
%_37memv48099 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57434)
store volatile %struct.ScmObj* %_37memv48099, %struct.ScmObj** %stackaddr$prim58334, align 8
%stackaddr$makeclosure58335 = alloca %struct.ScmObj*, align 8
%fptrToInt58336 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51716 to i64
%ae51716 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58336)
store volatile %struct.ScmObj* %ae51716, %struct.ScmObj** %stackaddr$makeclosure58335, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51716, %struct.ScmObj* %_37append48122, i64 0)
%ae51717 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58337 = alloca %struct.ScmObj*, align 8
%fptrToInt58338 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51718 to i64
%ae51718 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58338)
store volatile %struct.ScmObj* %ae51718, %struct.ScmObj** %stackaddr$makeclosure58337, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51718, %struct.ScmObj* %_37foldl148035, i64 0)
%argslist57604$ae517160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58339 = alloca %struct.ScmObj*, align 8
%argslist57604$ae517161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51718, %struct.ScmObj* %argslist57604$ae517160)
store volatile %struct.ScmObj* %argslist57604$ae517161, %struct.ScmObj** %stackaddr$prim58339, align 8
%stackaddr$prim58340 = alloca %struct.ScmObj*, align 8
%argslist57604$ae517162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51717, %struct.ScmObj* %argslist57604$ae517161)
store volatile %struct.ScmObj* %argslist57604$ae517162, %struct.ScmObj** %stackaddr$prim58340, align 8
%clofunc58341 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51716)
musttail call tailcc void %clofunc58341(%struct.ScmObj* %ae51716, %struct.ScmObj* %argslist57604$ae517162)
ret void
}

define tailcc void @proc_clo$ae51716(%struct.ScmObj* %env$ae51716,%struct.ScmObj* %current_45args57436) {
%stackaddr$env-ref58342 = alloca %struct.ScmObj*, align 8
%_37append48122 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51716, i64 0)
store %struct.ScmObj* %_37append48122, %struct.ScmObj** %stackaddr$env-ref58342
%stackaddr$prim58343 = alloca %struct.ScmObj*, align 8
%_95k48365 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57436)
store volatile %struct.ScmObj* %_95k48365, %struct.ScmObj** %stackaddr$prim58343, align 8
%stackaddr$prim58344 = alloca %struct.ScmObj*, align 8
%current_45args57437 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57436)
store volatile %struct.ScmObj* %current_45args57437, %struct.ScmObj** %stackaddr$prim58344, align 8
%stackaddr$prim58345 = alloca %struct.ScmObj*, align 8
%_37_4748095 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57437)
store volatile %struct.ScmObj* %_37_4748095, %struct.ScmObj** %stackaddr$prim58345, align 8
%stackaddr$makeclosure58346 = alloca %struct.ScmObj*, align 8
%fptrToInt58347 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51814 to i64
%ae51814 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58347)
store volatile %struct.ScmObj* %ae51814, %struct.ScmObj** %stackaddr$makeclosure58346, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51814, %struct.ScmObj* %_37append48122, i64 0)
%ae51815 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58348 = alloca %struct.ScmObj*, align 8
%fptrToInt58349 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51816 to i64
%ae51816 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58349)
store volatile %struct.ScmObj* %ae51816, %struct.ScmObj** %stackaddr$makeclosure58348, align 8
%argslist57591$ae518140 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58350 = alloca %struct.ScmObj*, align 8
%argslist57591$ae518141 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51816, %struct.ScmObj* %argslist57591$ae518140)
store volatile %struct.ScmObj* %argslist57591$ae518141, %struct.ScmObj** %stackaddr$prim58350, align 8
%stackaddr$prim58351 = alloca %struct.ScmObj*, align 8
%argslist57591$ae518142 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51815, %struct.ScmObj* %argslist57591$ae518141)
store volatile %struct.ScmObj* %argslist57591$ae518142, %struct.ScmObj** %stackaddr$prim58351, align 8
%clofunc58352 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51814)
musttail call tailcc void %clofunc58352(%struct.ScmObj* %ae51814, %struct.ScmObj* %argslist57591$ae518142)
ret void
}

define tailcc void @proc_clo$ae51814(%struct.ScmObj* %env$ae51814,%struct.ScmObj* %current_45args57439) {
%stackaddr$env-ref58353 = alloca %struct.ScmObj*, align 8
%_37append48122 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51814, i64 0)
store %struct.ScmObj* %_37append48122, %struct.ScmObj** %stackaddr$env-ref58353
%stackaddr$prim58354 = alloca %struct.ScmObj*, align 8
%_95k48366 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57439)
store volatile %struct.ScmObj* %_95k48366, %struct.ScmObj** %stackaddr$prim58354, align 8
%stackaddr$prim58355 = alloca %struct.ScmObj*, align 8
%current_45args57440 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57439)
store volatile %struct.ScmObj* %current_45args57440, %struct.ScmObj** %stackaddr$prim58355, align 8
%stackaddr$prim58356 = alloca %struct.ScmObj*, align 8
%_37first48093 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57440)
store volatile %struct.ScmObj* %_37first48093, %struct.ScmObj** %stackaddr$prim58356, align 8
%stackaddr$makeclosure58357 = alloca %struct.ScmObj*, align 8
%fptrToInt58358 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51834 to i64
%ae51834 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58358)
store volatile %struct.ScmObj* %ae51834, %struct.ScmObj** %stackaddr$makeclosure58357, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51834, %struct.ScmObj* %_37append48122, i64 0)
%ae51835 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58359 = alloca %struct.ScmObj*, align 8
%fptrToInt58360 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51836 to i64
%ae51836 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58360)
store volatile %struct.ScmObj* %ae51836, %struct.ScmObj** %stackaddr$makeclosure58359, align 8
%argslist57586$ae518340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58361 = alloca %struct.ScmObj*, align 8
%argslist57586$ae518341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51836, %struct.ScmObj* %argslist57586$ae518340)
store volatile %struct.ScmObj* %argslist57586$ae518341, %struct.ScmObj** %stackaddr$prim58361, align 8
%stackaddr$prim58362 = alloca %struct.ScmObj*, align 8
%argslist57586$ae518342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51835, %struct.ScmObj* %argslist57586$ae518341)
store volatile %struct.ScmObj* %argslist57586$ae518342, %struct.ScmObj** %stackaddr$prim58362, align 8
%clofunc58363 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51834)
musttail call tailcc void %clofunc58363(%struct.ScmObj* %ae51834, %struct.ScmObj* %argslist57586$ae518342)
ret void
}

define tailcc void @proc_clo$ae51834(%struct.ScmObj* %env$ae51834,%struct.ScmObj* %current_45args57442) {
%stackaddr$env-ref58364 = alloca %struct.ScmObj*, align 8
%_37append48122 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51834, i64 0)
store %struct.ScmObj* %_37append48122, %struct.ScmObj** %stackaddr$env-ref58364
%stackaddr$prim58365 = alloca %struct.ScmObj*, align 8
%_95k48367 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57442)
store volatile %struct.ScmObj* %_95k48367, %struct.ScmObj** %stackaddr$prim58365, align 8
%stackaddr$prim58366 = alloca %struct.ScmObj*, align 8
%current_45args57443 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57442)
store volatile %struct.ScmObj* %current_45args57443, %struct.ScmObj** %stackaddr$prim58366, align 8
%stackaddr$prim58367 = alloca %struct.ScmObj*, align 8
%_37second48091 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57443)
store volatile %struct.ScmObj* %_37second48091, %struct.ScmObj** %stackaddr$prim58367, align 8
%stackaddr$makeclosure58368 = alloca %struct.ScmObj*, align 8
%fptrToInt58369 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51856 to i64
%ae51856 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58369)
store volatile %struct.ScmObj* %ae51856, %struct.ScmObj** %stackaddr$makeclosure58368, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51856, %struct.ScmObj* %_37append48122, i64 0)
%ae51857 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58370 = alloca %struct.ScmObj*, align 8
%fptrToInt58371 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51858 to i64
%ae51858 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58371)
store volatile %struct.ScmObj* %ae51858, %struct.ScmObj** %stackaddr$makeclosure58370, align 8
%argslist57581$ae518560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58372 = alloca %struct.ScmObj*, align 8
%argslist57581$ae518561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51858, %struct.ScmObj* %argslist57581$ae518560)
store volatile %struct.ScmObj* %argslist57581$ae518561, %struct.ScmObj** %stackaddr$prim58372, align 8
%stackaddr$prim58373 = alloca %struct.ScmObj*, align 8
%argslist57581$ae518562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51857, %struct.ScmObj* %argslist57581$ae518561)
store volatile %struct.ScmObj* %argslist57581$ae518562, %struct.ScmObj** %stackaddr$prim58373, align 8
%clofunc58374 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51856)
musttail call tailcc void %clofunc58374(%struct.ScmObj* %ae51856, %struct.ScmObj* %argslist57581$ae518562)
ret void
}

define tailcc void @proc_clo$ae51856(%struct.ScmObj* %env$ae51856,%struct.ScmObj* %current_45args57445) {
%stackaddr$env-ref58375 = alloca %struct.ScmObj*, align 8
%_37append48122 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51856, i64 0)
store %struct.ScmObj* %_37append48122, %struct.ScmObj** %stackaddr$env-ref58375
%stackaddr$prim58376 = alloca %struct.ScmObj*, align 8
%_95k48368 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57445)
store volatile %struct.ScmObj* %_95k48368, %struct.ScmObj** %stackaddr$prim58376, align 8
%stackaddr$prim58377 = alloca %struct.ScmObj*, align 8
%current_45args57446 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57445)
store volatile %struct.ScmObj* %current_45args57446, %struct.ScmObj** %stackaddr$prim58377, align 8
%stackaddr$prim58378 = alloca %struct.ScmObj*, align 8
%_37third48089 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57446)
store volatile %struct.ScmObj* %_37third48089, %struct.ScmObj** %stackaddr$prim58378, align 8
%stackaddr$makeclosure58379 = alloca %struct.ScmObj*, align 8
%fptrToInt58380 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51880 to i64
%ae51880 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58380)
store volatile %struct.ScmObj* %ae51880, %struct.ScmObj** %stackaddr$makeclosure58379, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51880, %struct.ScmObj* %_37append48122, i64 0)
%ae51881 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58381 = alloca %struct.ScmObj*, align 8
%fptrToInt58382 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51882 to i64
%ae51882 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58382)
store volatile %struct.ScmObj* %ae51882, %struct.ScmObj** %stackaddr$makeclosure58381, align 8
%argslist57576$ae518800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58383 = alloca %struct.ScmObj*, align 8
%argslist57576$ae518801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51882, %struct.ScmObj* %argslist57576$ae518800)
store volatile %struct.ScmObj* %argslist57576$ae518801, %struct.ScmObj** %stackaddr$prim58383, align 8
%stackaddr$prim58384 = alloca %struct.ScmObj*, align 8
%argslist57576$ae518802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51881, %struct.ScmObj* %argslist57576$ae518801)
store volatile %struct.ScmObj* %argslist57576$ae518802, %struct.ScmObj** %stackaddr$prim58384, align 8
%clofunc58385 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51880)
musttail call tailcc void %clofunc58385(%struct.ScmObj* %ae51880, %struct.ScmObj* %argslist57576$ae518802)
ret void
}

define tailcc void @proc_clo$ae51880(%struct.ScmObj* %env$ae51880,%struct.ScmObj* %current_45args57448) {
%stackaddr$env-ref58386 = alloca %struct.ScmObj*, align 8
%_37append48122 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51880, i64 0)
store %struct.ScmObj* %_37append48122, %struct.ScmObj** %stackaddr$env-ref58386
%stackaddr$prim58387 = alloca %struct.ScmObj*, align 8
%_95k48369 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57448)
store volatile %struct.ScmObj* %_95k48369, %struct.ScmObj** %stackaddr$prim58387, align 8
%stackaddr$prim58388 = alloca %struct.ScmObj*, align 8
%current_45args57449 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57448)
store volatile %struct.ScmObj* %current_45args57449, %struct.ScmObj** %stackaddr$prim58388, align 8
%stackaddr$prim58389 = alloca %struct.ScmObj*, align 8
%_37fourth48087 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57449)
store volatile %struct.ScmObj* %_37fourth48087, %struct.ScmObj** %stackaddr$prim58389, align 8
%stackaddr$prim58390 = alloca %struct.ScmObj*, align 8
%anf_45bind48283 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind48283, %struct.ScmObj** %stackaddr$prim58390, align 8
%ae51906 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim58391 = alloca %struct.ScmObj*, align 8
%nqueens48148 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51906, %struct.ScmObj* %anf_45bind48283)
store volatile %struct.ScmObj* %nqueens48148, %struct.ScmObj** %stackaddr$prim58391, align 8
%stackaddr$makeclosure58392 = alloca %struct.ScmObj*, align 8
%fptrToInt58393 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51908 to i64
%ae51908 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58393)
store volatile %struct.ScmObj* %ae51908, %struct.ScmObj** %stackaddr$makeclosure58392, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51908, %struct.ScmObj* %nqueens48148, i64 0)
%ae51909 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58394 = alloca %struct.ScmObj*, align 8
%fptrToInt58395 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51910 to i64
%ae51910 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58395)
store volatile %struct.ScmObj* %ae51910, %struct.ScmObj** %stackaddr$makeclosure58394, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51910, %struct.ScmObj* %_37append48122, i64 0)
%argslist57571$ae519080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58396 = alloca %struct.ScmObj*, align 8
%argslist57571$ae519081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51910, %struct.ScmObj* %argslist57571$ae519080)
store volatile %struct.ScmObj* %argslist57571$ae519081, %struct.ScmObj** %stackaddr$prim58396, align 8
%stackaddr$prim58397 = alloca %struct.ScmObj*, align 8
%argslist57571$ae519082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51909, %struct.ScmObj* %argslist57571$ae519081)
store volatile %struct.ScmObj* %argslist57571$ae519082, %struct.ScmObj** %stackaddr$prim58397, align 8
%clofunc58398 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51908)
musttail call tailcc void %clofunc58398(%struct.ScmObj* %ae51908, %struct.ScmObj* %argslist57571$ae519082)
ret void
}

define tailcc void @proc_clo$ae51908(%struct.ScmObj* %env$ae51908,%struct.ScmObj* %current_45args57451) {
%stackaddr$env-ref58399 = alloca %struct.ScmObj*, align 8
%nqueens48148 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51908, i64 0)
store %struct.ScmObj* %nqueens48148, %struct.ScmObj** %stackaddr$env-ref58399
%stackaddr$prim58400 = alloca %struct.ScmObj*, align 8
%_95k48370 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57451)
store volatile %struct.ScmObj* %_95k48370, %struct.ScmObj** %stackaddr$prim58400, align 8
%stackaddr$prim58401 = alloca %struct.ScmObj*, align 8
%current_45args57452 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57451)
store volatile %struct.ScmObj* %current_45args57452, %struct.ScmObj** %stackaddr$prim58401, align 8
%stackaddr$prim58402 = alloca %struct.ScmObj*, align 8
%anf_45bind48336 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57452)
store volatile %struct.ScmObj* %anf_45bind48336, %struct.ScmObj** %stackaddr$prim58402, align 8
%ae54117 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58403 = alloca %struct.ScmObj*, align 8
%t4802548149 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %nqueens48148, %struct.ScmObj* %ae54117, %struct.ScmObj* %anf_45bind48336)
store volatile %struct.ScmObj* %t4802548149, %struct.ScmObj** %stackaddr$prim58403, align 8
%ae54120 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58404 = alloca %struct.ScmObj*, align 8
%anf_45bind48337 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %nqueens48148, %struct.ScmObj* %ae54120)
store volatile %struct.ScmObj* %anf_45bind48337, %struct.ScmObj** %stackaddr$prim58404, align 8
%stackaddr$makeclosure58405 = alloca %struct.ScmObj*, align 8
%fptrToInt58406 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae54122 to i64
%ae54122 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58406)
store volatile %struct.ScmObj* %ae54122, %struct.ScmObj** %stackaddr$makeclosure58405, align 8
%ae54123 = call %struct.ScmObj* @const_init_int(i64 7)
%argslist57458$anf_45bind483370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58407 = alloca %struct.ScmObj*, align 8
%argslist57458$anf_45bind483371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae54123, %struct.ScmObj* %argslist57458$anf_45bind483370)
store volatile %struct.ScmObj* %argslist57458$anf_45bind483371, %struct.ScmObj** %stackaddr$prim58407, align 8
%stackaddr$prim58408 = alloca %struct.ScmObj*, align 8
%argslist57458$anf_45bind483372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae54122, %struct.ScmObj* %argslist57458$anf_45bind483371)
store volatile %struct.ScmObj* %argslist57458$anf_45bind483372, %struct.ScmObj** %stackaddr$prim58408, align 8
%clofunc58409 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48337)
musttail call tailcc void %clofunc58409(%struct.ScmObj* %anf_45bind48337, %struct.ScmObj* %argslist57458$anf_45bind483372)
ret void
}

define tailcc void @proc_clo$ae54122(%struct.ScmObj* %env$ae54122,%struct.ScmObj* %current_45args57454) {
%stackaddr$prim58410 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57454)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim58410, align 8
%stackaddr$prim58411 = alloca %struct.ScmObj*, align 8
%current_45args57455 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57454)
store volatile %struct.ScmObj* %current_45args57455, %struct.ScmObj** %stackaddr$prim58411, align 8
%stackaddr$prim58412 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57455)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim58412, align 8
%stackaddr$prim58413 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim58413, align 8
%argslist57457$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58414 = alloca %struct.ScmObj*, align 8
%argslist57457$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist57457$k0)
store volatile %struct.ScmObj* %argslist57457$k1, %struct.ScmObj** %stackaddr$prim58414, align 8
%clofunc58415 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc58415(%struct.ScmObj* %k, %struct.ScmObj* %argslist57457$k1)
ret void
}

define tailcc void @proc_clo$ae51910(%struct.ScmObj* %env$ae51910,%struct.ScmObj* %current_45args57459) {
%stackaddr$env-ref58416 = alloca %struct.ScmObj*, align 8
%_37append48122 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51910, i64 0)
store %struct.ScmObj* %_37append48122, %struct.ScmObj** %stackaddr$env-ref58416
%stackaddr$prim58417 = alloca %struct.ScmObj*, align 8
%k48371 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57459)
store volatile %struct.ScmObj* %k48371, %struct.ScmObj** %stackaddr$prim58417, align 8
%stackaddr$prim58418 = alloca %struct.ScmObj*, align 8
%current_45args57460 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57459)
store volatile %struct.ScmObj* %current_45args57460, %struct.ScmObj** %stackaddr$prim58418, align 8
%stackaddr$prim58419 = alloca %struct.ScmObj*, align 8
%n48150 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57460)
store volatile %struct.ScmObj* %n48150, %struct.ScmObj** %stackaddr$prim58419, align 8
%stackaddr$prim58420 = alloca %struct.ScmObj*, align 8
%anf_45bind48284 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind48284, %struct.ScmObj** %stackaddr$prim58420, align 8
%ae51911 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim58421 = alloca %struct.ScmObj*, align 8
%one_45to48153 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51911, %struct.ScmObj* %anf_45bind48284)
store volatile %struct.ScmObj* %one_45to48153, %struct.ScmObj** %stackaddr$prim58421, align 8
%stackaddr$prim58422 = alloca %struct.ScmObj*, align 8
%anf_45bind48285 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind48285, %struct.ScmObj** %stackaddr$prim58422, align 8
%ae51913 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim58423 = alloca %struct.ScmObj*, align 8
%my_45try48152 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51913, %struct.ScmObj* %anf_45bind48285)
store volatile %struct.ScmObj* %my_45try48152, %struct.ScmObj** %stackaddr$prim58423, align 8
%stackaddr$prim58424 = alloca %struct.ScmObj*, align 8
%anf_45bind48286 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind48286, %struct.ScmObj** %stackaddr$prim58424, align 8
%ae51915 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim58425 = alloca %struct.ScmObj*, align 8
%ok_6348151 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51915, %struct.ScmObj* %anf_45bind48286)
store volatile %struct.ScmObj* %ok_6348151, %struct.ScmObj** %stackaddr$prim58425, align 8
%stackaddr$makeclosure58426 = alloca %struct.ScmObj*, align 8
%fptrToInt58427 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51917 to i64
%ae51917 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt58427)
store volatile %struct.ScmObj* %ae51917, %struct.ScmObj** %stackaddr$makeclosure58426, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51917, %struct.ScmObj* %one_45to48153, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51917, %struct.ScmObj* %my_45try48152, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51917, %struct.ScmObj* %ok_6348151, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51917, %struct.ScmObj* %n48150, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51917, %struct.ScmObj* %k48371, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51917, %struct.ScmObj* %_37append48122, i64 5)
%ae51918 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58428 = alloca %struct.ScmObj*, align 8
%fptrToInt58429 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51919 to i64
%ae51919 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58429)
store volatile %struct.ScmObj* %ae51919, %struct.ScmObj** %stackaddr$makeclosure58428, align 8
%argslist57570$ae519170 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58430 = alloca %struct.ScmObj*, align 8
%argslist57570$ae519171 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51919, %struct.ScmObj* %argslist57570$ae519170)
store volatile %struct.ScmObj* %argslist57570$ae519171, %struct.ScmObj** %stackaddr$prim58430, align 8
%stackaddr$prim58431 = alloca %struct.ScmObj*, align 8
%argslist57570$ae519172 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51918, %struct.ScmObj* %argslist57570$ae519171)
store volatile %struct.ScmObj* %argslist57570$ae519172, %struct.ScmObj** %stackaddr$prim58431, align 8
%clofunc58432 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51917)
musttail call tailcc void %clofunc58432(%struct.ScmObj* %ae51917, %struct.ScmObj* %argslist57570$ae519172)
ret void
}

define tailcc void @proc_clo$ae51917(%struct.ScmObj* %env$ae51917,%struct.ScmObj* %current_45args57462) {
%stackaddr$env-ref58433 = alloca %struct.ScmObj*, align 8
%one_45to48153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51917, i64 0)
store %struct.ScmObj* %one_45to48153, %struct.ScmObj** %stackaddr$env-ref58433
%stackaddr$env-ref58434 = alloca %struct.ScmObj*, align 8
%my_45try48152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51917, i64 1)
store %struct.ScmObj* %my_45try48152, %struct.ScmObj** %stackaddr$env-ref58434
%stackaddr$env-ref58435 = alloca %struct.ScmObj*, align 8
%ok_6348151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51917, i64 2)
store %struct.ScmObj* %ok_6348151, %struct.ScmObj** %stackaddr$env-ref58435
%stackaddr$env-ref58436 = alloca %struct.ScmObj*, align 8
%n48150 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51917, i64 3)
store %struct.ScmObj* %n48150, %struct.ScmObj** %stackaddr$env-ref58436
%stackaddr$env-ref58437 = alloca %struct.ScmObj*, align 8
%k48371 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51917, i64 4)
store %struct.ScmObj* %k48371, %struct.ScmObj** %stackaddr$env-ref58437
%stackaddr$env-ref58438 = alloca %struct.ScmObj*, align 8
%_37append48122 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51917, i64 5)
store %struct.ScmObj* %_37append48122, %struct.ScmObj** %stackaddr$env-ref58438
%stackaddr$prim58439 = alloca %struct.ScmObj*, align 8
%_95k48372 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57462)
store volatile %struct.ScmObj* %_95k48372, %struct.ScmObj** %stackaddr$prim58439, align 8
%stackaddr$prim58440 = alloca %struct.ScmObj*, align 8
%current_45args57463 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57462)
store volatile %struct.ScmObj* %current_45args57463, %struct.ScmObj** %stackaddr$prim58440, align 8
%stackaddr$prim58441 = alloca %struct.ScmObj*, align 8
%anf_45bind48296 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57463)
store volatile %struct.ScmObj* %anf_45bind48296, %struct.ScmObj** %stackaddr$prim58441, align 8
%ae52184 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58442 = alloca %struct.ScmObj*, align 8
%t4802848163 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %one_45to48153, %struct.ScmObj* %ae52184, %struct.ScmObj* %anf_45bind48296)
store volatile %struct.ScmObj* %t4802848163, %struct.ScmObj** %stackaddr$prim58442, align 8
%stackaddr$makeclosure58443 = alloca %struct.ScmObj*, align 8
%fptrToInt58444 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52186 to i64
%ae52186 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt58444)
store volatile %struct.ScmObj* %ae52186, %struct.ScmObj** %stackaddr$makeclosure58443, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52186, %struct.ScmObj* %one_45to48153, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52186, %struct.ScmObj* %my_45try48152, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52186, %struct.ScmObj* %ok_6348151, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52186, %struct.ScmObj* %n48150, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52186, %struct.ScmObj* %k48371, i64 4)
%ae52187 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58445 = alloca %struct.ScmObj*, align 8
%fptrToInt58446 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52188 to i64
%ae52188 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58446)
store volatile %struct.ScmObj* %ae52188, %struct.ScmObj** %stackaddr$makeclosure58445, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52188, %struct.ScmObj* %my_45try48152, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52188, %struct.ScmObj* %ok_6348151, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52188, %struct.ScmObj* %_37append48122, i64 2)
%argslist57546$ae521860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58447 = alloca %struct.ScmObj*, align 8
%argslist57546$ae521861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52188, %struct.ScmObj* %argslist57546$ae521860)
store volatile %struct.ScmObj* %argslist57546$ae521861, %struct.ScmObj** %stackaddr$prim58447, align 8
%stackaddr$prim58448 = alloca %struct.ScmObj*, align 8
%argslist57546$ae521862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52187, %struct.ScmObj* %argslist57546$ae521861)
store volatile %struct.ScmObj* %argslist57546$ae521862, %struct.ScmObj** %stackaddr$prim58448, align 8
%clofunc58449 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52186)
musttail call tailcc void %clofunc58449(%struct.ScmObj* %ae52186, %struct.ScmObj* %argslist57546$ae521862)
ret void
}

define tailcc void @proc_clo$ae52186(%struct.ScmObj* %env$ae52186,%struct.ScmObj* %current_45args57465) {
%stackaddr$env-ref58450 = alloca %struct.ScmObj*, align 8
%one_45to48153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52186, i64 0)
store %struct.ScmObj* %one_45to48153, %struct.ScmObj** %stackaddr$env-ref58450
%stackaddr$env-ref58451 = alloca %struct.ScmObj*, align 8
%my_45try48152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52186, i64 1)
store %struct.ScmObj* %my_45try48152, %struct.ScmObj** %stackaddr$env-ref58451
%stackaddr$env-ref58452 = alloca %struct.ScmObj*, align 8
%ok_6348151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52186, i64 2)
store %struct.ScmObj* %ok_6348151, %struct.ScmObj** %stackaddr$env-ref58452
%stackaddr$env-ref58453 = alloca %struct.ScmObj*, align 8
%n48150 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52186, i64 3)
store %struct.ScmObj* %n48150, %struct.ScmObj** %stackaddr$env-ref58453
%stackaddr$env-ref58454 = alloca %struct.ScmObj*, align 8
%k48371 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52186, i64 4)
store %struct.ScmObj* %k48371, %struct.ScmObj** %stackaddr$env-ref58454
%stackaddr$prim58455 = alloca %struct.ScmObj*, align 8
%_95k48373 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57465)
store volatile %struct.ScmObj* %_95k48373, %struct.ScmObj** %stackaddr$prim58455, align 8
%stackaddr$prim58456 = alloca %struct.ScmObj*, align 8
%current_45args57466 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57465)
store volatile %struct.ScmObj* %current_45args57466, %struct.ScmObj** %stackaddr$prim58456, align 8
%stackaddr$prim58457 = alloca %struct.ScmObj*, align 8
%anf_45bind48315 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57466)
store volatile %struct.ScmObj* %anf_45bind48315, %struct.ScmObj** %stackaddr$prim58457, align 8
%ae52576 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58458 = alloca %struct.ScmObj*, align 8
%t4802748158 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %my_45try48152, %struct.ScmObj* %ae52576, %struct.ScmObj* %anf_45bind48315)
store volatile %struct.ScmObj* %t4802748158, %struct.ScmObj** %stackaddr$prim58458, align 8
%stackaddr$makeclosure58459 = alloca %struct.ScmObj*, align 8
%fptrToInt58460 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52578 to i64
%ae52578 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt58460)
store volatile %struct.ScmObj* %ae52578, %struct.ScmObj** %stackaddr$makeclosure58459, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52578, %struct.ScmObj* %one_45to48153, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52578, %struct.ScmObj* %my_45try48152, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52578, %struct.ScmObj* %ok_6348151, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52578, %struct.ScmObj* %n48150, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52578, %struct.ScmObj* %k48371, i64 4)
%ae52579 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58461 = alloca %struct.ScmObj*, align 8
%fptrToInt58462 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52580 to i64
%ae52580 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58462)
store volatile %struct.ScmObj* %ae52580, %struct.ScmObj** %stackaddr$makeclosure58461, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52580, %struct.ScmObj* %ok_6348151, i64 0)
%argslist57503$ae525780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58463 = alloca %struct.ScmObj*, align 8
%argslist57503$ae525781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52580, %struct.ScmObj* %argslist57503$ae525780)
store volatile %struct.ScmObj* %argslist57503$ae525781, %struct.ScmObj** %stackaddr$prim58463, align 8
%stackaddr$prim58464 = alloca %struct.ScmObj*, align 8
%argslist57503$ae525782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52579, %struct.ScmObj* %argslist57503$ae525781)
store volatile %struct.ScmObj* %argslist57503$ae525782, %struct.ScmObj** %stackaddr$prim58464, align 8
%clofunc58465 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52578)
musttail call tailcc void %clofunc58465(%struct.ScmObj* %ae52578, %struct.ScmObj* %argslist57503$ae525782)
ret void
}

define tailcc void @proc_clo$ae52578(%struct.ScmObj* %env$ae52578,%struct.ScmObj* %current_45args57468) {
%stackaddr$env-ref58466 = alloca %struct.ScmObj*, align 8
%one_45to48153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52578, i64 0)
store %struct.ScmObj* %one_45to48153, %struct.ScmObj** %stackaddr$env-ref58466
%stackaddr$env-ref58467 = alloca %struct.ScmObj*, align 8
%my_45try48152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52578, i64 1)
store %struct.ScmObj* %my_45try48152, %struct.ScmObj** %stackaddr$env-ref58467
%stackaddr$env-ref58468 = alloca %struct.ScmObj*, align 8
%ok_6348151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52578, i64 2)
store %struct.ScmObj* %ok_6348151, %struct.ScmObj** %stackaddr$env-ref58468
%stackaddr$env-ref58469 = alloca %struct.ScmObj*, align 8
%n48150 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52578, i64 3)
store %struct.ScmObj* %n48150, %struct.ScmObj** %stackaddr$env-ref58469
%stackaddr$env-ref58470 = alloca %struct.ScmObj*, align 8
%k48371 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52578, i64 4)
store %struct.ScmObj* %k48371, %struct.ScmObj** %stackaddr$env-ref58470
%stackaddr$prim58471 = alloca %struct.ScmObj*, align 8
%_95k48374 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57468)
store volatile %struct.ScmObj* %_95k48374, %struct.ScmObj** %stackaddr$prim58471, align 8
%stackaddr$prim58472 = alloca %struct.ScmObj*, align 8
%current_45args57469 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57468)
store volatile %struct.ScmObj* %current_45args57469, %struct.ScmObj** %stackaddr$prim58472, align 8
%stackaddr$prim58473 = alloca %struct.ScmObj*, align 8
%anf_45bind48328 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57469)
store volatile %struct.ScmObj* %anf_45bind48328, %struct.ScmObj** %stackaddr$prim58473, align 8
%ae52694 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58474 = alloca %struct.ScmObj*, align 8
%t4802648154 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %ok_6348151, %struct.ScmObj* %ae52694, %struct.ScmObj* %anf_45bind48328)
store volatile %struct.ScmObj* %t4802648154, %struct.ScmObj** %stackaddr$prim58474, align 8
%ae52697 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58475 = alloca %struct.ScmObj*, align 8
%anf_45bind48329 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %my_45try48152, %struct.ScmObj* %ae52697)
store volatile %struct.ScmObj* %anf_45bind48329, %struct.ScmObj** %stackaddr$prim58475, align 8
%ae52699 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58476 = alloca %struct.ScmObj*, align 8
%anf_45bind48330 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %one_45to48153, %struct.ScmObj* %ae52699)
store volatile %struct.ScmObj* %anf_45bind48330, %struct.ScmObj** %stackaddr$prim58476, align 8
%stackaddr$makeclosure58477 = alloca %struct.ScmObj*, align 8
%fptrToInt58478 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52701 to i64
%ae52701 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58478)
store volatile %struct.ScmObj* %ae52701, %struct.ScmObj** %stackaddr$makeclosure58477, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52701, %struct.ScmObj* %anf_45bind48329, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52701, %struct.ScmObj* %k48371, i64 1)
%argslist57493$anf_45bind483300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58479 = alloca %struct.ScmObj*, align 8
%argslist57493$anf_45bind483301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %n48150, %struct.ScmObj* %argslist57493$anf_45bind483300)
store volatile %struct.ScmObj* %argslist57493$anf_45bind483301, %struct.ScmObj** %stackaddr$prim58479, align 8
%stackaddr$prim58480 = alloca %struct.ScmObj*, align 8
%argslist57493$anf_45bind483302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52701, %struct.ScmObj* %argslist57493$anf_45bind483301)
store volatile %struct.ScmObj* %argslist57493$anf_45bind483302, %struct.ScmObj** %stackaddr$prim58480, align 8
%clofunc58481 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48330)
musttail call tailcc void %clofunc58481(%struct.ScmObj* %anf_45bind48330, %struct.ScmObj* %argslist57493$anf_45bind483302)
ret void
}

define tailcc void @proc_clo$ae52701(%struct.ScmObj* %env$ae52701,%struct.ScmObj* %current_45args57471) {
%stackaddr$env-ref58482 = alloca %struct.ScmObj*, align 8
%anf_45bind48329 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52701, i64 0)
store %struct.ScmObj* %anf_45bind48329, %struct.ScmObj** %stackaddr$env-ref58482
%stackaddr$env-ref58483 = alloca %struct.ScmObj*, align 8
%k48371 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52701, i64 1)
store %struct.ScmObj* %k48371, %struct.ScmObj** %stackaddr$env-ref58483
%stackaddr$prim58484 = alloca %struct.ScmObj*, align 8
%_95k48375 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57471)
store volatile %struct.ScmObj* %_95k48375, %struct.ScmObj** %stackaddr$prim58484, align 8
%stackaddr$prim58485 = alloca %struct.ScmObj*, align 8
%current_45args57472 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57471)
store volatile %struct.ScmObj* %current_45args57472, %struct.ScmObj** %stackaddr$prim58485, align 8
%stackaddr$prim58486 = alloca %struct.ScmObj*, align 8
%anf_45bind48331 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57472)
store volatile %struct.ScmObj* %anf_45bind48331, %struct.ScmObj** %stackaddr$prim58486, align 8
%stackaddr$makeclosure58487 = alloca %struct.ScmObj*, align 8
%fptrToInt58488 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52703 to i64
%ae52703 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58488)
store volatile %struct.ScmObj* %ae52703, %struct.ScmObj** %stackaddr$makeclosure58487, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52703, %struct.ScmObj* %anf_45bind48329, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52703, %struct.ScmObj* %k48371, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52703, %struct.ScmObj* %anf_45bind48331, i64 2)
%ae52704 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58489 = alloca %struct.ScmObj*, align 8
%fptrToInt58490 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52705 to i64
%ae52705 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58490)
store volatile %struct.ScmObj* %ae52705, %struct.ScmObj** %stackaddr$makeclosure58489, align 8
%argslist57492$ae527030 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58491 = alloca %struct.ScmObj*, align 8
%argslist57492$ae527031 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52705, %struct.ScmObj* %argslist57492$ae527030)
store volatile %struct.ScmObj* %argslist57492$ae527031, %struct.ScmObj** %stackaddr$prim58491, align 8
%stackaddr$prim58492 = alloca %struct.ScmObj*, align 8
%argslist57492$ae527032 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52704, %struct.ScmObj* %argslist57492$ae527031)
store volatile %struct.ScmObj* %argslist57492$ae527032, %struct.ScmObj** %stackaddr$prim58492, align 8
%clofunc58493 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52703)
musttail call tailcc void %clofunc58493(%struct.ScmObj* %ae52703, %struct.ScmObj* %argslist57492$ae527032)
ret void
}

define tailcc void @proc_clo$ae52703(%struct.ScmObj* %env$ae52703,%struct.ScmObj* %current_45args57474) {
%stackaddr$env-ref58494 = alloca %struct.ScmObj*, align 8
%anf_45bind48329 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52703, i64 0)
store %struct.ScmObj* %anf_45bind48329, %struct.ScmObj** %stackaddr$env-ref58494
%stackaddr$env-ref58495 = alloca %struct.ScmObj*, align 8
%k48371 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52703, i64 1)
store %struct.ScmObj* %k48371, %struct.ScmObj** %stackaddr$env-ref58495
%stackaddr$env-ref58496 = alloca %struct.ScmObj*, align 8
%anf_45bind48331 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52703, i64 2)
store %struct.ScmObj* %anf_45bind48331, %struct.ScmObj** %stackaddr$env-ref58496
%stackaddr$prim58497 = alloca %struct.ScmObj*, align 8
%_95k48376 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57474)
store volatile %struct.ScmObj* %_95k48376, %struct.ScmObj** %stackaddr$prim58497, align 8
%stackaddr$prim58498 = alloca %struct.ScmObj*, align 8
%current_45args57475 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57474)
store volatile %struct.ScmObj* %current_45args57475, %struct.ScmObj** %stackaddr$prim58498, align 8
%stackaddr$prim58499 = alloca %struct.ScmObj*, align 8
%anf_45bind48332 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57475)
store volatile %struct.ScmObj* %anf_45bind48332, %struct.ScmObj** %stackaddr$prim58499, align 8
%stackaddr$makeclosure58500 = alloca %struct.ScmObj*, align 8
%fptrToInt58501 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52726 to i64
%ae52726 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58501)
store volatile %struct.ScmObj* %ae52726, %struct.ScmObj** %stackaddr$makeclosure58500, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52726, %struct.ScmObj* %anf_45bind48329, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52726, %struct.ScmObj* %k48371, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52726, %struct.ScmObj* %anf_45bind48331, i64 2)
%argslist57490$anf_45bind483320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58502 = alloca %struct.ScmObj*, align 8
%argslist57490$anf_45bind483321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52726, %struct.ScmObj* %argslist57490$anf_45bind483320)
store volatile %struct.ScmObj* %argslist57490$anf_45bind483321, %struct.ScmObj** %stackaddr$prim58502, align 8
%clofunc58503 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48332)
musttail call tailcc void %clofunc58503(%struct.ScmObj* %anf_45bind48332, %struct.ScmObj* %argslist57490$anf_45bind483321)
ret void
}

define tailcc void @proc_clo$ae52726(%struct.ScmObj* %env$ae52726,%struct.ScmObj* %current_45args57477) {
%stackaddr$env-ref58504 = alloca %struct.ScmObj*, align 8
%anf_45bind48329 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52726, i64 0)
store %struct.ScmObj* %anf_45bind48329, %struct.ScmObj** %stackaddr$env-ref58504
%stackaddr$env-ref58505 = alloca %struct.ScmObj*, align 8
%k48371 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52726, i64 1)
store %struct.ScmObj* %k48371, %struct.ScmObj** %stackaddr$env-ref58505
%stackaddr$env-ref58506 = alloca %struct.ScmObj*, align 8
%anf_45bind48331 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52726, i64 2)
store %struct.ScmObj* %anf_45bind48331, %struct.ScmObj** %stackaddr$env-ref58506
%stackaddr$prim58507 = alloca %struct.ScmObj*, align 8
%_95k48377 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57477)
store volatile %struct.ScmObj* %_95k48377, %struct.ScmObj** %stackaddr$prim58507, align 8
%stackaddr$prim58508 = alloca %struct.ScmObj*, align 8
%current_45args57478 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57477)
store volatile %struct.ScmObj* %current_45args57478, %struct.ScmObj** %stackaddr$prim58508, align 8
%stackaddr$prim58509 = alloca %struct.ScmObj*, align 8
%anf_45bind48333 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57478)
store volatile %struct.ScmObj* %anf_45bind48333, %struct.ScmObj** %stackaddr$prim58509, align 8
%stackaddr$makeclosure58510 = alloca %struct.ScmObj*, align 8
%fptrToInt58511 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52727 to i64
%ae52727 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt58511)
store volatile %struct.ScmObj* %ae52727, %struct.ScmObj** %stackaddr$makeclosure58510, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52727, %struct.ScmObj* %anf_45bind48329, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52727, %struct.ScmObj* %k48371, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52727, %struct.ScmObj* %anf_45bind48333, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52727, %struct.ScmObj* %anf_45bind48331, i64 3)
%ae52728 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58512 = alloca %struct.ScmObj*, align 8
%fptrToInt58513 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52729 to i64
%ae52729 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58513)
store volatile %struct.ScmObj* %ae52729, %struct.ScmObj** %stackaddr$makeclosure58512, align 8
%argslist57489$ae527270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58514 = alloca %struct.ScmObj*, align 8
%argslist57489$ae527271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52729, %struct.ScmObj* %argslist57489$ae527270)
store volatile %struct.ScmObj* %argslist57489$ae527271, %struct.ScmObj** %stackaddr$prim58514, align 8
%stackaddr$prim58515 = alloca %struct.ScmObj*, align 8
%argslist57489$ae527272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52728, %struct.ScmObj* %argslist57489$ae527271)
store volatile %struct.ScmObj* %argslist57489$ae527272, %struct.ScmObj** %stackaddr$prim58515, align 8
%clofunc58516 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52727)
musttail call tailcc void %clofunc58516(%struct.ScmObj* %ae52727, %struct.ScmObj* %argslist57489$ae527272)
ret void
}

define tailcc void @proc_clo$ae52727(%struct.ScmObj* %env$ae52727,%struct.ScmObj* %current_45args57480) {
%stackaddr$env-ref58517 = alloca %struct.ScmObj*, align 8
%anf_45bind48329 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52727, i64 0)
store %struct.ScmObj* %anf_45bind48329, %struct.ScmObj** %stackaddr$env-ref58517
%stackaddr$env-ref58518 = alloca %struct.ScmObj*, align 8
%k48371 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52727, i64 1)
store %struct.ScmObj* %k48371, %struct.ScmObj** %stackaddr$env-ref58518
%stackaddr$env-ref58519 = alloca %struct.ScmObj*, align 8
%anf_45bind48333 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52727, i64 2)
store %struct.ScmObj* %anf_45bind48333, %struct.ScmObj** %stackaddr$env-ref58519
%stackaddr$env-ref58520 = alloca %struct.ScmObj*, align 8
%anf_45bind48331 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52727, i64 3)
store %struct.ScmObj* %anf_45bind48331, %struct.ScmObj** %stackaddr$env-ref58520
%stackaddr$prim58521 = alloca %struct.ScmObj*, align 8
%_95k48378 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57480)
store volatile %struct.ScmObj* %_95k48378, %struct.ScmObj** %stackaddr$prim58521, align 8
%stackaddr$prim58522 = alloca %struct.ScmObj*, align 8
%current_45args57481 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57480)
store volatile %struct.ScmObj* %current_45args57481, %struct.ScmObj** %stackaddr$prim58522, align 8
%stackaddr$prim58523 = alloca %struct.ScmObj*, align 8
%anf_45bind48334 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57481)
store volatile %struct.ScmObj* %anf_45bind48334, %struct.ScmObj** %stackaddr$prim58523, align 8
%stackaddr$makeclosure58524 = alloca %struct.ScmObj*, align 8
%fptrToInt58525 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52750 to i64
%ae52750 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt58525)
store volatile %struct.ScmObj* %ae52750, %struct.ScmObj** %stackaddr$makeclosure58524, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52750, %struct.ScmObj* %anf_45bind48329, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52750, %struct.ScmObj* %k48371, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52750, %struct.ScmObj* %anf_45bind48333, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52750, %struct.ScmObj* %anf_45bind48331, i64 3)
%argslist57487$anf_45bind483340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58526 = alloca %struct.ScmObj*, align 8
%argslist57487$anf_45bind483341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52750, %struct.ScmObj* %argslist57487$anf_45bind483340)
store volatile %struct.ScmObj* %argslist57487$anf_45bind483341, %struct.ScmObj** %stackaddr$prim58526, align 8
%clofunc58527 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48334)
musttail call tailcc void %clofunc58527(%struct.ScmObj* %anf_45bind48334, %struct.ScmObj* %argslist57487$anf_45bind483341)
ret void
}

define tailcc void @proc_clo$ae52750(%struct.ScmObj* %env$ae52750,%struct.ScmObj* %current_45args57483) {
%stackaddr$env-ref58528 = alloca %struct.ScmObj*, align 8
%anf_45bind48329 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52750, i64 0)
store %struct.ScmObj* %anf_45bind48329, %struct.ScmObj** %stackaddr$env-ref58528
%stackaddr$env-ref58529 = alloca %struct.ScmObj*, align 8
%k48371 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52750, i64 1)
store %struct.ScmObj* %k48371, %struct.ScmObj** %stackaddr$env-ref58529
%stackaddr$env-ref58530 = alloca %struct.ScmObj*, align 8
%anf_45bind48333 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52750, i64 2)
store %struct.ScmObj* %anf_45bind48333, %struct.ScmObj** %stackaddr$env-ref58530
%stackaddr$env-ref58531 = alloca %struct.ScmObj*, align 8
%anf_45bind48331 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52750, i64 3)
store %struct.ScmObj* %anf_45bind48331, %struct.ScmObj** %stackaddr$env-ref58531
%stackaddr$prim58532 = alloca %struct.ScmObj*, align 8
%_95k48379 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57483)
store volatile %struct.ScmObj* %_95k48379, %struct.ScmObj** %stackaddr$prim58532, align 8
%stackaddr$prim58533 = alloca %struct.ScmObj*, align 8
%current_45args57484 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57483)
store volatile %struct.ScmObj* %current_45args57484, %struct.ScmObj** %stackaddr$prim58533, align 8
%stackaddr$prim58534 = alloca %struct.ScmObj*, align 8
%anf_45bind48335 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57484)
store volatile %struct.ScmObj* %anf_45bind48335, %struct.ScmObj** %stackaddr$prim58534, align 8
%argslist57486$anf_45bind483290 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58535 = alloca %struct.ScmObj*, align 8
%argslist57486$anf_45bind483291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48335, %struct.ScmObj* %argslist57486$anf_45bind483290)
store volatile %struct.ScmObj* %argslist57486$anf_45bind483291, %struct.ScmObj** %stackaddr$prim58535, align 8
%stackaddr$prim58536 = alloca %struct.ScmObj*, align 8
%argslist57486$anf_45bind483292 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48333, %struct.ScmObj* %argslist57486$anf_45bind483291)
store volatile %struct.ScmObj* %argslist57486$anf_45bind483292, %struct.ScmObj** %stackaddr$prim58536, align 8
%stackaddr$prim58537 = alloca %struct.ScmObj*, align 8
%argslist57486$anf_45bind483293 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48331, %struct.ScmObj* %argslist57486$anf_45bind483292)
store volatile %struct.ScmObj* %argslist57486$anf_45bind483293, %struct.ScmObj** %stackaddr$prim58537, align 8
%stackaddr$prim58538 = alloca %struct.ScmObj*, align 8
%argslist57486$anf_45bind483294 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48371, %struct.ScmObj* %argslist57486$anf_45bind483293)
store volatile %struct.ScmObj* %argslist57486$anf_45bind483294, %struct.ScmObj** %stackaddr$prim58538, align 8
%clofunc58539 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48329)
musttail call tailcc void %clofunc58539(%struct.ScmObj* %anf_45bind48329, %struct.ScmObj* %argslist57486$anf_45bind483294)
ret void
}

define tailcc void @proc_clo$ae52729(%struct.ScmObj* %env$ae52729,%struct.ScmObj* %lst4817148380) {
%stackaddr$prim58540 = alloca %struct.ScmObj*, align 8
%k48381 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4817148380)
store volatile %struct.ScmObj* %k48381, %struct.ScmObj** %stackaddr$prim58540, align 8
%stackaddr$prim58541 = alloca %struct.ScmObj*, align 8
%lst48171 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4817148380)
store volatile %struct.ScmObj* %lst48171, %struct.ScmObj** %stackaddr$prim58541, align 8
%ae52733 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57488$k483810 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58542 = alloca %struct.ScmObj*, align 8
%argslist57488$k483811 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48171, %struct.ScmObj* %argslist57488$k483810)
store volatile %struct.ScmObj* %argslist57488$k483811, %struct.ScmObj** %stackaddr$prim58542, align 8
%stackaddr$prim58543 = alloca %struct.ScmObj*, align 8
%argslist57488$k483812 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52733, %struct.ScmObj* %argslist57488$k483811)
store volatile %struct.ScmObj* %argslist57488$k483812, %struct.ScmObj** %stackaddr$prim58543, align 8
%clofunc58544 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48381)
musttail call tailcc void %clofunc58544(%struct.ScmObj* %k48381, %struct.ScmObj* %argslist57488$k483812)
ret void
}

define tailcc void @proc_clo$ae52705(%struct.ScmObj* %env$ae52705,%struct.ScmObj* %lst4817048382) {
%stackaddr$prim58545 = alloca %struct.ScmObj*, align 8
%k48383 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4817048382)
store volatile %struct.ScmObj* %k48383, %struct.ScmObj** %stackaddr$prim58545, align 8
%stackaddr$prim58546 = alloca %struct.ScmObj*, align 8
%lst48170 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4817048382)
store volatile %struct.ScmObj* %lst48170, %struct.ScmObj** %stackaddr$prim58546, align 8
%ae52709 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57491$k483830 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58547 = alloca %struct.ScmObj*, align 8
%argslist57491$k483831 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48170, %struct.ScmObj* %argslist57491$k483830)
store volatile %struct.ScmObj* %argslist57491$k483831, %struct.ScmObj** %stackaddr$prim58547, align 8
%stackaddr$prim58548 = alloca %struct.ScmObj*, align 8
%argslist57491$k483832 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52709, %struct.ScmObj* %argslist57491$k483831)
store volatile %struct.ScmObj* %argslist57491$k483832, %struct.ScmObj** %stackaddr$prim58548, align 8
%clofunc58549 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48383)
musttail call tailcc void %clofunc58549(%struct.ScmObj* %k48383, %struct.ScmObj* %argslist57491$k483832)
ret void
}

define tailcc void @proc_clo$ae52580(%struct.ScmObj* %env$ae52580,%struct.ScmObj* %current_45args57494) {
%stackaddr$env-ref58550 = alloca %struct.ScmObj*, align 8
%ok_6348151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52580, i64 0)
store %struct.ScmObj* %ok_6348151, %struct.ScmObj** %stackaddr$env-ref58550
%stackaddr$prim58551 = alloca %struct.ScmObj*, align 8
%k48384 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57494)
store volatile %struct.ScmObj* %k48384, %struct.ScmObj** %stackaddr$prim58551, align 8
%stackaddr$prim58552 = alloca %struct.ScmObj*, align 8
%current_45args57495 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57494)
store volatile %struct.ScmObj* %current_45args57495, %struct.ScmObj** %stackaddr$prim58552, align 8
%stackaddr$prim58553 = alloca %struct.ScmObj*, align 8
%row48157 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57495)
store volatile %struct.ScmObj* %row48157, %struct.ScmObj** %stackaddr$prim58553, align 8
%stackaddr$prim58554 = alloca %struct.ScmObj*, align 8
%current_45args57496 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57495)
store volatile %struct.ScmObj* %current_45args57496, %struct.ScmObj** %stackaddr$prim58554, align 8
%stackaddr$prim58555 = alloca %struct.ScmObj*, align 8
%dist48156 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57496)
store volatile %struct.ScmObj* %dist48156, %struct.ScmObj** %stackaddr$prim58555, align 8
%stackaddr$prim58556 = alloca %struct.ScmObj*, align 8
%current_45args57497 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57496)
store volatile %struct.ScmObj* %current_45args57497, %struct.ScmObj** %stackaddr$prim58556, align 8
%stackaddr$prim58557 = alloca %struct.ScmObj*, align 8
%placed48155 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57497)
store volatile %struct.ScmObj* %placed48155, %struct.ScmObj** %stackaddr$prim58557, align 8
%stackaddr$prim58558 = alloca %struct.ScmObj*, align 8
%anf_45bind48316 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %placed48155)
store volatile %struct.ScmObj* %anf_45bind48316, %struct.ScmObj** %stackaddr$prim58558, align 8
%truthy$cmp58559 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48316)
%cmp$cmp58559 = icmp eq i64 %truthy$cmp58559, 1
br i1 %cmp$cmp58559, label %truebranch$cmp58559, label %falsebranch$cmp58559
truebranch$cmp58559:
%ae52584 = call %struct.ScmObj* @const_init_int(i64 0)
%ae52585 = call %struct.ScmObj* @const_init_true()
%argslist57499$k483840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58560 = alloca %struct.ScmObj*, align 8
%argslist57499$k483841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52585, %struct.ScmObj* %argslist57499$k483840)
store volatile %struct.ScmObj* %argslist57499$k483841, %struct.ScmObj** %stackaddr$prim58560, align 8
%stackaddr$prim58561 = alloca %struct.ScmObj*, align 8
%argslist57499$k483842 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52584, %struct.ScmObj* %argslist57499$k483841)
store volatile %struct.ScmObj* %argslist57499$k483842, %struct.ScmObj** %stackaddr$prim58561, align 8
%clofunc58562 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48384)
musttail call tailcc void %clofunc58562(%struct.ScmObj* %k48384, %struct.ScmObj* %argslist57499$k483842)
ret void
falsebranch$cmp58559:
%stackaddr$prim58563 = alloca %struct.ScmObj*, align 8
%anf_45bind48317 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %placed48155)
store volatile %struct.ScmObj* %anf_45bind48317, %struct.ScmObj** %stackaddr$prim58563, align 8
%stackaddr$prim58564 = alloca %struct.ScmObj*, align 8
%anf_45bind48318 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %row48157, %struct.ScmObj* %dist48156)
store volatile %struct.ScmObj* %anf_45bind48318, %struct.ScmObj** %stackaddr$prim58564, align 8
%stackaddr$prim58565 = alloca %struct.ScmObj*, align 8
%anf_45bind48319 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %anf_45bind48317, %struct.ScmObj* %anf_45bind48318)
store volatile %struct.ScmObj* %anf_45bind48319, %struct.ScmObj** %stackaddr$prim58565, align 8
%stackaddr$prim58566 = alloca %struct.ScmObj*, align 8
%anf_45bind48320 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind48319)
store volatile %struct.ScmObj* %anf_45bind48320, %struct.ScmObj** %stackaddr$prim58566, align 8
%truthy$cmp58567 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48320)
%cmp$cmp58567 = icmp eq i64 %truthy$cmp58567, 1
br i1 %cmp$cmp58567, label %truebranch$cmp58567, label %falsebranch$cmp58567
truebranch$cmp58567:
%stackaddr$prim58568 = alloca %struct.ScmObj*, align 8
%anf_45bind48321 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %placed48155)
store volatile %struct.ScmObj* %anf_45bind48321, %struct.ScmObj** %stackaddr$prim58568, align 8
%stackaddr$prim58569 = alloca %struct.ScmObj*, align 8
%anf_45bind48322 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %row48157, %struct.ScmObj* %dist48156)
store volatile %struct.ScmObj* %anf_45bind48322, %struct.ScmObj** %stackaddr$prim58569, align 8
%stackaddr$prim58570 = alloca %struct.ScmObj*, align 8
%anf_45bind48323 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %anf_45bind48321, %struct.ScmObj* %anf_45bind48322)
store volatile %struct.ScmObj* %anf_45bind48323, %struct.ScmObj** %stackaddr$prim58570, align 8
%stackaddr$prim58571 = alloca %struct.ScmObj*, align 8
%anf_45bind48324 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind48323)
store volatile %struct.ScmObj* %anf_45bind48324, %struct.ScmObj** %stackaddr$prim58571, align 8
%truthy$cmp58572 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48324)
%cmp$cmp58572 = icmp eq i64 %truthy$cmp58572, 1
br i1 %cmp$cmp58572, label %truebranch$cmp58572, label %falsebranch$cmp58572
truebranch$cmp58572:
%ae52607 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58573 = alloca %struct.ScmObj*, align 8
%anf_45bind48325 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %ok_6348151, %struct.ScmObj* %ae52607)
store volatile %struct.ScmObj* %anf_45bind48325, %struct.ScmObj** %stackaddr$prim58573, align 8
%ae52609 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim58574 = alloca %struct.ScmObj*, align 8
%anf_45bind48326 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %dist48156, %struct.ScmObj* %ae52609)
store volatile %struct.ScmObj* %anf_45bind48326, %struct.ScmObj** %stackaddr$prim58574, align 8
%stackaddr$prim58575 = alloca %struct.ScmObj*, align 8
%anf_45bind48327 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %placed48155)
store volatile %struct.ScmObj* %anf_45bind48327, %struct.ScmObj** %stackaddr$prim58575, align 8
%argslist57500$anf_45bind483250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58576 = alloca %struct.ScmObj*, align 8
%argslist57500$anf_45bind483251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48327, %struct.ScmObj* %argslist57500$anf_45bind483250)
store volatile %struct.ScmObj* %argslist57500$anf_45bind483251, %struct.ScmObj** %stackaddr$prim58576, align 8
%stackaddr$prim58577 = alloca %struct.ScmObj*, align 8
%argslist57500$anf_45bind483252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48326, %struct.ScmObj* %argslist57500$anf_45bind483251)
store volatile %struct.ScmObj* %argslist57500$anf_45bind483252, %struct.ScmObj** %stackaddr$prim58577, align 8
%stackaddr$prim58578 = alloca %struct.ScmObj*, align 8
%argslist57500$anf_45bind483253 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %row48157, %struct.ScmObj* %argslist57500$anf_45bind483252)
store volatile %struct.ScmObj* %argslist57500$anf_45bind483253, %struct.ScmObj** %stackaddr$prim58578, align 8
%stackaddr$prim58579 = alloca %struct.ScmObj*, align 8
%argslist57500$anf_45bind483254 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48384, %struct.ScmObj* %argslist57500$anf_45bind483253)
store volatile %struct.ScmObj* %argslist57500$anf_45bind483254, %struct.ScmObj** %stackaddr$prim58579, align 8
%clofunc58580 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48325)
musttail call tailcc void %clofunc58580(%struct.ScmObj* %anf_45bind48325, %struct.ScmObj* %argslist57500$anf_45bind483254)
ret void
falsebranch$cmp58572:
%ae52635 = call %struct.ScmObj* @const_init_int(i64 0)
%ae52636 = call %struct.ScmObj* @const_init_false()
%argslist57501$k483840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58581 = alloca %struct.ScmObj*, align 8
%argslist57501$k483841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52636, %struct.ScmObj* %argslist57501$k483840)
store volatile %struct.ScmObj* %argslist57501$k483841, %struct.ScmObj** %stackaddr$prim58581, align 8
%stackaddr$prim58582 = alloca %struct.ScmObj*, align 8
%argslist57501$k483842 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52635, %struct.ScmObj* %argslist57501$k483841)
store volatile %struct.ScmObj* %argslist57501$k483842, %struct.ScmObj** %stackaddr$prim58582, align 8
%clofunc58583 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48384)
musttail call tailcc void %clofunc58583(%struct.ScmObj* %k48384, %struct.ScmObj* %argslist57501$k483842)
ret void
falsebranch$cmp58567:
%ae52644 = call %struct.ScmObj* @const_init_int(i64 0)
%ae52645 = call %struct.ScmObj* @const_init_false()
%argslist57502$k483840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58584 = alloca %struct.ScmObj*, align 8
%argslist57502$k483841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52645, %struct.ScmObj* %argslist57502$k483840)
store volatile %struct.ScmObj* %argslist57502$k483841, %struct.ScmObj** %stackaddr$prim58584, align 8
%stackaddr$prim58585 = alloca %struct.ScmObj*, align 8
%argslist57502$k483842 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52644, %struct.ScmObj* %argslist57502$k483841)
store volatile %struct.ScmObj* %argslist57502$k483842, %struct.ScmObj** %stackaddr$prim58585, align 8
%clofunc58586 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48384)
musttail call tailcc void %clofunc58586(%struct.ScmObj* %k48384, %struct.ScmObj* %argslist57502$k483842)
ret void
}

define tailcc void @proc_clo$ae52188(%struct.ScmObj* %env$ae52188,%struct.ScmObj* %current_45args57504) {
%stackaddr$env-ref58587 = alloca %struct.ScmObj*, align 8
%my_45try48152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52188, i64 0)
store %struct.ScmObj* %my_45try48152, %struct.ScmObj** %stackaddr$env-ref58587
%stackaddr$env-ref58588 = alloca %struct.ScmObj*, align 8
%ok_6348151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52188, i64 1)
store %struct.ScmObj* %ok_6348151, %struct.ScmObj** %stackaddr$env-ref58588
%stackaddr$env-ref58589 = alloca %struct.ScmObj*, align 8
%_37append48122 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52188, i64 2)
store %struct.ScmObj* %_37append48122, %struct.ScmObj** %stackaddr$env-ref58589
%stackaddr$prim58590 = alloca %struct.ScmObj*, align 8
%k48385 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57504)
store volatile %struct.ScmObj* %k48385, %struct.ScmObj** %stackaddr$prim58590, align 8
%stackaddr$prim58591 = alloca %struct.ScmObj*, align 8
%current_45args57505 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57504)
store volatile %struct.ScmObj* %current_45args57505, %struct.ScmObj** %stackaddr$prim58591, align 8
%stackaddr$prim58592 = alloca %struct.ScmObj*, align 8
%x48161 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57505)
store volatile %struct.ScmObj* %x48161, %struct.ScmObj** %stackaddr$prim58592, align 8
%stackaddr$prim58593 = alloca %struct.ScmObj*, align 8
%current_45args57506 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57505)
store volatile %struct.ScmObj* %current_45args57506, %struct.ScmObj** %stackaddr$prim58593, align 8
%stackaddr$prim58594 = alloca %struct.ScmObj*, align 8
%y48160 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57506)
store volatile %struct.ScmObj* %y48160, %struct.ScmObj** %stackaddr$prim58594, align 8
%stackaddr$prim58595 = alloca %struct.ScmObj*, align 8
%current_45args57507 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57506)
store volatile %struct.ScmObj* %current_45args57507, %struct.ScmObj** %stackaddr$prim58595, align 8
%stackaddr$prim58596 = alloca %struct.ScmObj*, align 8
%z48159 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57507)
store volatile %struct.ScmObj* %z48159, %struct.ScmObj** %stackaddr$prim58596, align 8
%stackaddr$prim58597 = alloca %struct.ScmObj*, align 8
%anf_45bind48297 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %x48161)
store volatile %struct.ScmObj* %anf_45bind48297, %struct.ScmObj** %stackaddr$prim58597, align 8
%truthy$cmp58598 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48297)
%cmp$cmp58598 = icmp eq i64 %truthy$cmp58598, 1
br i1 %cmp$cmp58598, label %truebranch$cmp58598, label %falsebranch$cmp58598
truebranch$cmp58598:
%stackaddr$prim58599 = alloca %struct.ScmObj*, align 8
%anf_45bind48298 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %y48160)
store volatile %struct.ScmObj* %anf_45bind48298, %struct.ScmObj** %stackaddr$prim58599, align 8
%truthy$cmp58600 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48298)
%cmp$cmp58600 = icmp eq i64 %truthy$cmp58600, 1
br i1 %cmp$cmp58600, label %truebranch$cmp58600, label %falsebranch$cmp58600
truebranch$cmp58600:
%ae52194 = call %struct.ScmObj* @const_init_int(i64 0)
%ae52195 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist57509$k483850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58601 = alloca %struct.ScmObj*, align 8
%argslist57509$k483851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52195, %struct.ScmObj* %argslist57509$k483850)
store volatile %struct.ScmObj* %argslist57509$k483851, %struct.ScmObj** %stackaddr$prim58601, align 8
%stackaddr$prim58602 = alloca %struct.ScmObj*, align 8
%argslist57509$k483852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52194, %struct.ScmObj* %argslist57509$k483851)
store volatile %struct.ScmObj* %argslist57509$k483852, %struct.ScmObj** %stackaddr$prim58602, align 8
%clofunc58603 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48385)
musttail call tailcc void %clofunc58603(%struct.ScmObj* %k48385, %struct.ScmObj* %argslist57509$k483852)
ret void
falsebranch$cmp58600:
%ae52203 = call %struct.ScmObj* @const_init_int(i64 0)
%ae52204 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57510$k483850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58604 = alloca %struct.ScmObj*, align 8
%argslist57510$k483851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52204, %struct.ScmObj* %argslist57510$k483850)
store volatile %struct.ScmObj* %argslist57510$k483851, %struct.ScmObj** %stackaddr$prim58604, align 8
%stackaddr$prim58605 = alloca %struct.ScmObj*, align 8
%argslist57510$k483852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52203, %struct.ScmObj* %argslist57510$k483851)
store volatile %struct.ScmObj* %argslist57510$k483852, %struct.ScmObj** %stackaddr$prim58605, align 8
%clofunc58606 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48385)
musttail call tailcc void %clofunc58606(%struct.ScmObj* %k48385, %struct.ScmObj* %argslist57510$k483852)
ret void
falsebranch$cmp58598:
%ae52212 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58607 = alloca %struct.ScmObj*, align 8
%anf_45bind48299 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %ok_6348151, %struct.ScmObj* %ae52212)
store volatile %struct.ScmObj* %anf_45bind48299, %struct.ScmObj** %stackaddr$prim58607, align 8
%stackaddr$prim58608 = alloca %struct.ScmObj*, align 8
%anf_45bind48300 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48161)
store volatile %struct.ScmObj* %anf_45bind48300, %struct.ScmObj** %stackaddr$prim58608, align 8
%stackaddr$makeclosure58609 = alloca %struct.ScmObj*, align 8
%fptrToInt58610 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52215 to i64
%ae52215 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt58610)
store volatile %struct.ScmObj* %ae52215, %struct.ScmObj** %stackaddr$makeclosure58609, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52215, %struct.ScmObj* %k48385, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52215, %struct.ScmObj* %x48161, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52215, %struct.ScmObj* %my_45try48152, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52215, %struct.ScmObj* %y48160, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52215, %struct.ScmObj* %z48159, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae52215, %struct.ScmObj* %_37append48122, i64 5)
%ae52217 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist57545$anf_45bind482990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58611 = alloca %struct.ScmObj*, align 8
%argslist57545$anf_45bind482991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %z48159, %struct.ScmObj* %argslist57545$anf_45bind482990)
store volatile %struct.ScmObj* %argslist57545$anf_45bind482991, %struct.ScmObj** %stackaddr$prim58611, align 8
%stackaddr$prim58612 = alloca %struct.ScmObj*, align 8
%argslist57545$anf_45bind482992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52217, %struct.ScmObj* %argslist57545$anf_45bind482991)
store volatile %struct.ScmObj* %argslist57545$anf_45bind482992, %struct.ScmObj** %stackaddr$prim58612, align 8
%stackaddr$prim58613 = alloca %struct.ScmObj*, align 8
%argslist57545$anf_45bind482993 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48300, %struct.ScmObj* %argslist57545$anf_45bind482992)
store volatile %struct.ScmObj* %argslist57545$anf_45bind482993, %struct.ScmObj** %stackaddr$prim58613, align 8
%stackaddr$prim58614 = alloca %struct.ScmObj*, align 8
%argslist57545$anf_45bind482994 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52215, %struct.ScmObj* %argslist57545$anf_45bind482993)
store volatile %struct.ScmObj* %argslist57545$anf_45bind482994, %struct.ScmObj** %stackaddr$prim58614, align 8
%clofunc58615 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48299)
musttail call tailcc void %clofunc58615(%struct.ScmObj* %anf_45bind48299, %struct.ScmObj* %argslist57545$anf_45bind482994)
ret void
}

define tailcc void @proc_clo$ae52215(%struct.ScmObj* %env$ae52215,%struct.ScmObj* %current_45args57511) {
%stackaddr$env-ref58616 = alloca %struct.ScmObj*, align 8
%k48385 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52215, i64 0)
store %struct.ScmObj* %k48385, %struct.ScmObj** %stackaddr$env-ref58616
%stackaddr$env-ref58617 = alloca %struct.ScmObj*, align 8
%x48161 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52215, i64 1)
store %struct.ScmObj* %x48161, %struct.ScmObj** %stackaddr$env-ref58617
%stackaddr$env-ref58618 = alloca %struct.ScmObj*, align 8
%my_45try48152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52215, i64 2)
store %struct.ScmObj* %my_45try48152, %struct.ScmObj** %stackaddr$env-ref58618
%stackaddr$env-ref58619 = alloca %struct.ScmObj*, align 8
%y48160 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52215, i64 3)
store %struct.ScmObj* %y48160, %struct.ScmObj** %stackaddr$env-ref58619
%stackaddr$env-ref58620 = alloca %struct.ScmObj*, align 8
%z48159 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52215, i64 4)
store %struct.ScmObj* %z48159, %struct.ScmObj** %stackaddr$env-ref58620
%stackaddr$env-ref58621 = alloca %struct.ScmObj*, align 8
%_37append48122 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52215, i64 5)
store %struct.ScmObj* %_37append48122, %struct.ScmObj** %stackaddr$env-ref58621
%stackaddr$prim58622 = alloca %struct.ScmObj*, align 8
%_95k48386 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57511)
store volatile %struct.ScmObj* %_95k48386, %struct.ScmObj** %stackaddr$prim58622, align 8
%stackaddr$prim58623 = alloca %struct.ScmObj*, align 8
%current_45args57512 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57511)
store volatile %struct.ScmObj* %current_45args57512, %struct.ScmObj** %stackaddr$prim58623, align 8
%stackaddr$prim58624 = alloca %struct.ScmObj*, align 8
%anf_45bind48301 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57512)
store volatile %struct.ScmObj* %anf_45bind48301, %struct.ScmObj** %stackaddr$prim58624, align 8
%truthy$cmp58625 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48301)
%cmp$cmp58625 = icmp eq i64 %truthy$cmp58625, 1
br i1 %cmp$cmp58625, label %truebranch$cmp58625, label %falsebranch$cmp58625
truebranch$cmp58625:
%ae52226 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58626 = alloca %struct.ScmObj*, align 8
%anf_45bind48302 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %my_45try48152, %struct.ScmObj* %ae52226)
store volatile %struct.ScmObj* %anf_45bind48302, %struct.ScmObj** %stackaddr$prim58626, align 8
%stackaddr$prim58627 = alloca %struct.ScmObj*, align 8
%anf_45bind48303 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48161)
store volatile %struct.ScmObj* %anf_45bind48303, %struct.ScmObj** %stackaddr$prim58627, align 8
%stackaddr$makeclosure58628 = alloca %struct.ScmObj*, align 8
%fptrToInt58629 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52229 to i64
%ae52229 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt58629)
store volatile %struct.ScmObj* %ae52229, %struct.ScmObj** %stackaddr$makeclosure58628, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52229, %struct.ScmObj* %k48385, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52229, %struct.ScmObj* %x48161, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52229, %struct.ScmObj* %my_45try48152, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52229, %struct.ScmObj* %y48160, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52229, %struct.ScmObj* %z48159, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae52229, %struct.ScmObj* %anf_45bind48302, i64 5)
%argslist57535$_37append481220 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58630 = alloca %struct.ScmObj*, align 8
%argslist57535$_37append481221 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y48160, %struct.ScmObj* %argslist57535$_37append481220)
store volatile %struct.ScmObj* %argslist57535$_37append481221, %struct.ScmObj** %stackaddr$prim58630, align 8
%stackaddr$prim58631 = alloca %struct.ScmObj*, align 8
%argslist57535$_37append481222 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48303, %struct.ScmObj* %argslist57535$_37append481221)
store volatile %struct.ScmObj* %argslist57535$_37append481222, %struct.ScmObj** %stackaddr$prim58631, align 8
%stackaddr$prim58632 = alloca %struct.ScmObj*, align 8
%argslist57535$_37append481223 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52229, %struct.ScmObj* %argslist57535$_37append481222)
store volatile %struct.ScmObj* %argslist57535$_37append481223, %struct.ScmObj** %stackaddr$prim58632, align 8
%clofunc58633 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37append48122)
musttail call tailcc void %clofunc58633(%struct.ScmObj* %_37append48122, %struct.ScmObj* %argslist57535$_37append481223)
ret void
falsebranch$cmp58625:
%stackaddr$makeclosure58634 = alloca %struct.ScmObj*, align 8
%fptrToInt58635 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52364 to i64
%ae52364 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt58635)
store volatile %struct.ScmObj* %ae52364, %struct.ScmObj** %stackaddr$makeclosure58634, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52364, %struct.ScmObj* %k48385, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52364, %struct.ScmObj* %x48161, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52364, %struct.ScmObj* %my_45try48152, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52364, %struct.ScmObj* %y48160, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52364, %struct.ScmObj* %z48159, i64 4)
%ae52365 = call %struct.ScmObj* @const_init_int(i64 0)
%ae52366 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57544$ae523640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58636 = alloca %struct.ScmObj*, align 8
%argslist57544$ae523641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52366, %struct.ScmObj* %argslist57544$ae523640)
store volatile %struct.ScmObj* %argslist57544$ae523641, %struct.ScmObj** %stackaddr$prim58636, align 8
%stackaddr$prim58637 = alloca %struct.ScmObj*, align 8
%argslist57544$ae523642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52365, %struct.ScmObj* %argslist57544$ae523641)
store volatile %struct.ScmObj* %argslist57544$ae523642, %struct.ScmObj** %stackaddr$prim58637, align 8
%clofunc58638 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52364)
musttail call tailcc void %clofunc58638(%struct.ScmObj* %ae52364, %struct.ScmObj* %argslist57544$ae523642)
ret void
}

define tailcc void @proc_clo$ae52229(%struct.ScmObj* %env$ae52229,%struct.ScmObj* %current_45args57514) {
%stackaddr$env-ref58639 = alloca %struct.ScmObj*, align 8
%k48385 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52229, i64 0)
store %struct.ScmObj* %k48385, %struct.ScmObj** %stackaddr$env-ref58639
%stackaddr$env-ref58640 = alloca %struct.ScmObj*, align 8
%x48161 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52229, i64 1)
store %struct.ScmObj* %x48161, %struct.ScmObj** %stackaddr$env-ref58640
%stackaddr$env-ref58641 = alloca %struct.ScmObj*, align 8
%my_45try48152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52229, i64 2)
store %struct.ScmObj* %my_45try48152, %struct.ScmObj** %stackaddr$env-ref58641
%stackaddr$env-ref58642 = alloca %struct.ScmObj*, align 8
%y48160 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52229, i64 3)
store %struct.ScmObj* %y48160, %struct.ScmObj** %stackaddr$env-ref58642
%stackaddr$env-ref58643 = alloca %struct.ScmObj*, align 8
%z48159 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52229, i64 4)
store %struct.ScmObj* %z48159, %struct.ScmObj** %stackaddr$env-ref58643
%stackaddr$env-ref58644 = alloca %struct.ScmObj*, align 8
%anf_45bind48302 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52229, i64 5)
store %struct.ScmObj* %anf_45bind48302, %struct.ScmObj** %stackaddr$env-ref58644
%stackaddr$prim58645 = alloca %struct.ScmObj*, align 8
%_95k48390 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57514)
store volatile %struct.ScmObj* %_95k48390, %struct.ScmObj** %stackaddr$prim58645, align 8
%stackaddr$prim58646 = alloca %struct.ScmObj*, align 8
%current_45args57515 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57514)
store volatile %struct.ScmObj* %current_45args57515, %struct.ScmObj** %stackaddr$prim58646, align 8
%stackaddr$prim58647 = alloca %struct.ScmObj*, align 8
%anf_45bind48304 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57515)
store volatile %struct.ScmObj* %anf_45bind48304, %struct.ScmObj** %stackaddr$prim58647, align 8
%stackaddr$makeclosure58648 = alloca %struct.ScmObj*, align 8
%fptrToInt58649 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52232 to i64
%ae52232 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt58649)
store volatile %struct.ScmObj* %ae52232, %struct.ScmObj** %stackaddr$makeclosure58648, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52232, %struct.ScmObj* %k48385, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52232, %struct.ScmObj* %x48161, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52232, %struct.ScmObj* %anf_45bind48304, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52232, %struct.ScmObj* %y48160, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52232, %struct.ScmObj* %my_45try48152, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae52232, %struct.ScmObj* %z48159, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae52232, %struct.ScmObj* %anf_45bind48302, i64 6)
%ae52233 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58650 = alloca %struct.ScmObj*, align 8
%fptrToInt58651 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52234 to i64
%ae52234 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58651)
store volatile %struct.ScmObj* %ae52234, %struct.ScmObj** %stackaddr$makeclosure58650, align 8
%argslist57534$ae522320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58652 = alloca %struct.ScmObj*, align 8
%argslist57534$ae522321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52234, %struct.ScmObj* %argslist57534$ae522320)
store volatile %struct.ScmObj* %argslist57534$ae522321, %struct.ScmObj** %stackaddr$prim58652, align 8
%stackaddr$prim58653 = alloca %struct.ScmObj*, align 8
%argslist57534$ae522322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52233, %struct.ScmObj* %argslist57534$ae522321)
store volatile %struct.ScmObj* %argslist57534$ae522322, %struct.ScmObj** %stackaddr$prim58653, align 8
%clofunc58654 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52232)
musttail call tailcc void %clofunc58654(%struct.ScmObj* %ae52232, %struct.ScmObj* %argslist57534$ae522322)
ret void
}

define tailcc void @proc_clo$ae52232(%struct.ScmObj* %env$ae52232,%struct.ScmObj* %current_45args57517) {
%stackaddr$env-ref58655 = alloca %struct.ScmObj*, align 8
%k48385 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52232, i64 0)
store %struct.ScmObj* %k48385, %struct.ScmObj** %stackaddr$env-ref58655
%stackaddr$env-ref58656 = alloca %struct.ScmObj*, align 8
%x48161 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52232, i64 1)
store %struct.ScmObj* %x48161, %struct.ScmObj** %stackaddr$env-ref58656
%stackaddr$env-ref58657 = alloca %struct.ScmObj*, align 8
%anf_45bind48304 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52232, i64 2)
store %struct.ScmObj* %anf_45bind48304, %struct.ScmObj** %stackaddr$env-ref58657
%stackaddr$env-ref58658 = alloca %struct.ScmObj*, align 8
%y48160 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52232, i64 3)
store %struct.ScmObj* %y48160, %struct.ScmObj** %stackaddr$env-ref58658
%stackaddr$env-ref58659 = alloca %struct.ScmObj*, align 8
%my_45try48152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52232, i64 4)
store %struct.ScmObj* %my_45try48152, %struct.ScmObj** %stackaddr$env-ref58659
%stackaddr$env-ref58660 = alloca %struct.ScmObj*, align 8
%z48159 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52232, i64 5)
store %struct.ScmObj* %z48159, %struct.ScmObj** %stackaddr$env-ref58660
%stackaddr$env-ref58661 = alloca %struct.ScmObj*, align 8
%anf_45bind48302 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52232, i64 6)
store %struct.ScmObj* %anf_45bind48302, %struct.ScmObj** %stackaddr$env-ref58661
%stackaddr$prim58662 = alloca %struct.ScmObj*, align 8
%_95k48391 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57517)
store volatile %struct.ScmObj* %_95k48391, %struct.ScmObj** %stackaddr$prim58662, align 8
%stackaddr$prim58663 = alloca %struct.ScmObj*, align 8
%current_45args57518 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57517)
store volatile %struct.ScmObj* %current_45args57518, %struct.ScmObj** %stackaddr$prim58663, align 8
%stackaddr$prim58664 = alloca %struct.ScmObj*, align 8
%anf_45bind48305 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57518)
store volatile %struct.ScmObj* %anf_45bind48305, %struct.ScmObj** %stackaddr$prim58664, align 8
%stackaddr$makeclosure58665 = alloca %struct.ScmObj*, align 8
%fptrToInt58666 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52255 to i64
%ae52255 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt58666)
store volatile %struct.ScmObj* %ae52255, %struct.ScmObj** %stackaddr$makeclosure58665, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52255, %struct.ScmObj* %k48385, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52255, %struct.ScmObj* %x48161, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52255, %struct.ScmObj* %anf_45bind48304, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52255, %struct.ScmObj* %y48160, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52255, %struct.ScmObj* %my_45try48152, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae52255, %struct.ScmObj* %z48159, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae52255, %struct.ScmObj* %anf_45bind48302, i64 6)
%argslist57532$anf_45bind483050 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58667 = alloca %struct.ScmObj*, align 8
%argslist57532$anf_45bind483051 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52255, %struct.ScmObj* %argslist57532$anf_45bind483050)
store volatile %struct.ScmObj* %argslist57532$anf_45bind483051, %struct.ScmObj** %stackaddr$prim58667, align 8
%clofunc58668 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48305)
musttail call tailcc void %clofunc58668(%struct.ScmObj* %anf_45bind48305, %struct.ScmObj* %argslist57532$anf_45bind483051)
ret void
}

define tailcc void @proc_clo$ae52255(%struct.ScmObj* %env$ae52255,%struct.ScmObj* %current_45args57520) {
%stackaddr$env-ref58669 = alloca %struct.ScmObj*, align 8
%k48385 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52255, i64 0)
store %struct.ScmObj* %k48385, %struct.ScmObj** %stackaddr$env-ref58669
%stackaddr$env-ref58670 = alloca %struct.ScmObj*, align 8
%x48161 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52255, i64 1)
store %struct.ScmObj* %x48161, %struct.ScmObj** %stackaddr$env-ref58670
%stackaddr$env-ref58671 = alloca %struct.ScmObj*, align 8
%anf_45bind48304 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52255, i64 2)
store %struct.ScmObj* %anf_45bind48304, %struct.ScmObj** %stackaddr$env-ref58671
%stackaddr$env-ref58672 = alloca %struct.ScmObj*, align 8
%y48160 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52255, i64 3)
store %struct.ScmObj* %y48160, %struct.ScmObj** %stackaddr$env-ref58672
%stackaddr$env-ref58673 = alloca %struct.ScmObj*, align 8
%my_45try48152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52255, i64 4)
store %struct.ScmObj* %my_45try48152, %struct.ScmObj** %stackaddr$env-ref58673
%stackaddr$env-ref58674 = alloca %struct.ScmObj*, align 8
%z48159 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52255, i64 5)
store %struct.ScmObj* %z48159, %struct.ScmObj** %stackaddr$env-ref58674
%stackaddr$env-ref58675 = alloca %struct.ScmObj*, align 8
%anf_45bind48302 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52255, i64 6)
store %struct.ScmObj* %anf_45bind48302, %struct.ScmObj** %stackaddr$env-ref58675
%stackaddr$prim58676 = alloca %struct.ScmObj*, align 8
%_95k48392 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57520)
store volatile %struct.ScmObj* %_95k48392, %struct.ScmObj** %stackaddr$prim58676, align 8
%stackaddr$prim58677 = alloca %struct.ScmObj*, align 8
%current_45args57521 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57520)
store volatile %struct.ScmObj* %current_45args57521, %struct.ScmObj** %stackaddr$prim58677, align 8
%stackaddr$prim58678 = alloca %struct.ScmObj*, align 8
%anf_45bind48306 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57521)
store volatile %struct.ScmObj* %anf_45bind48306, %struct.ScmObj** %stackaddr$prim58678, align 8
%stackaddr$prim58679 = alloca %struct.ScmObj*, align 8
%anf_45bind48307 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48161)
store volatile %struct.ScmObj* %anf_45bind48307, %struct.ScmObj** %stackaddr$prim58679, align 8
%stackaddr$prim58680 = alloca %struct.ScmObj*, align 8
%anf_45bind48308 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48307, %struct.ScmObj* %z48159)
store volatile %struct.ScmObj* %anf_45bind48308, %struct.ScmObj** %stackaddr$prim58680, align 8
%stackaddr$makeclosure58681 = alloca %struct.ScmObj*, align 8
%fptrToInt58682 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52260 to i64
%ae52260 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt58682)
store volatile %struct.ScmObj* %ae52260, %struct.ScmObj** %stackaddr$makeclosure58681, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52260, %struct.ScmObj* %k48385, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52260, %struct.ScmObj* %x48161, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52260, %struct.ScmObj* %my_45try48152, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52260, %struct.ScmObj* %y48160, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52260, %struct.ScmObj* %z48159, i64 4)
%argslist57531$anf_45bind483020 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58683 = alloca %struct.ScmObj*, align 8
%argslist57531$anf_45bind483021 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48308, %struct.ScmObj* %argslist57531$anf_45bind483020)
store volatile %struct.ScmObj* %argslist57531$anf_45bind483021, %struct.ScmObj** %stackaddr$prim58683, align 8
%stackaddr$prim58684 = alloca %struct.ScmObj*, align 8
%argslist57531$anf_45bind483022 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48306, %struct.ScmObj* %argslist57531$anf_45bind483021)
store volatile %struct.ScmObj* %argslist57531$anf_45bind483022, %struct.ScmObj** %stackaddr$prim58684, align 8
%stackaddr$prim58685 = alloca %struct.ScmObj*, align 8
%argslist57531$anf_45bind483023 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48304, %struct.ScmObj* %argslist57531$anf_45bind483022)
store volatile %struct.ScmObj* %argslist57531$anf_45bind483023, %struct.ScmObj** %stackaddr$prim58685, align 8
%stackaddr$prim58686 = alloca %struct.ScmObj*, align 8
%argslist57531$anf_45bind483024 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52260, %struct.ScmObj* %argslist57531$anf_45bind483023)
store volatile %struct.ScmObj* %argslist57531$anf_45bind483024, %struct.ScmObj** %stackaddr$prim58686, align 8
%clofunc58687 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48302)
musttail call tailcc void %clofunc58687(%struct.ScmObj* %anf_45bind48302, %struct.ScmObj* %argslist57531$anf_45bind483024)
ret void
}

define tailcc void @proc_clo$ae52260(%struct.ScmObj* %env$ae52260,%struct.ScmObj* %current_45args57523) {
%stackaddr$env-ref58688 = alloca %struct.ScmObj*, align 8
%k48385 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52260, i64 0)
store %struct.ScmObj* %k48385, %struct.ScmObj** %stackaddr$env-ref58688
%stackaddr$env-ref58689 = alloca %struct.ScmObj*, align 8
%x48161 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52260, i64 1)
store %struct.ScmObj* %x48161, %struct.ScmObj** %stackaddr$env-ref58689
%stackaddr$env-ref58690 = alloca %struct.ScmObj*, align 8
%my_45try48152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52260, i64 2)
store %struct.ScmObj* %my_45try48152, %struct.ScmObj** %stackaddr$env-ref58690
%stackaddr$env-ref58691 = alloca %struct.ScmObj*, align 8
%y48160 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52260, i64 3)
store %struct.ScmObj* %y48160, %struct.ScmObj** %stackaddr$env-ref58691
%stackaddr$env-ref58692 = alloca %struct.ScmObj*, align 8
%z48159 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52260, i64 4)
store %struct.ScmObj* %z48159, %struct.ScmObj** %stackaddr$env-ref58692
%stackaddr$prim58693 = alloca %struct.ScmObj*, align 8
%_95k48387 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57523)
store volatile %struct.ScmObj* %_95k48387, %struct.ScmObj** %stackaddr$prim58693, align 8
%stackaddr$prim58694 = alloca %struct.ScmObj*, align 8
%current_45args57524 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57523)
store volatile %struct.ScmObj* %current_45args57524, %struct.ScmObj** %stackaddr$prim58694, align 8
%stackaddr$prim58695 = alloca %struct.ScmObj*, align 8
%anf_45bind48309 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57524)
store volatile %struct.ScmObj* %anf_45bind48309, %struct.ScmObj** %stackaddr$prim58695, align 8
%ae52265 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58696 = alloca %struct.ScmObj*, align 8
%anf_45bind48310 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %my_45try48152, %struct.ScmObj* %ae52265)
store volatile %struct.ScmObj* %anf_45bind48310, %struct.ScmObj** %stackaddr$prim58696, align 8
%stackaddr$prim58697 = alloca %struct.ScmObj*, align 8
%anf_45bind48311 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48161)
store volatile %struct.ScmObj* %anf_45bind48311, %struct.ScmObj** %stackaddr$prim58697, align 8
%stackaddr$prim58698 = alloca %struct.ScmObj*, align 8
%anf_45bind48312 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48161)
store volatile %struct.ScmObj* %anf_45bind48312, %struct.ScmObj** %stackaddr$prim58698, align 8
%stackaddr$prim58699 = alloca %struct.ScmObj*, align 8
%anf_45bind48313 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48312, %struct.ScmObj* %y48160)
store volatile %struct.ScmObj* %anf_45bind48313, %struct.ScmObj** %stackaddr$prim58699, align 8
%stackaddr$makeclosure58700 = alloca %struct.ScmObj*, align 8
%fptrToInt58701 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52271 to i64
%ae52271 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58701)
store volatile %struct.ScmObj* %ae52271, %struct.ScmObj** %stackaddr$makeclosure58700, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52271, %struct.ScmObj* %anf_45bind48309, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52271, %struct.ScmObj* %k48385, i64 1)
%argslist57530$anf_45bind483100 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58702 = alloca %struct.ScmObj*, align 8
%argslist57530$anf_45bind483101 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %z48159, %struct.ScmObj* %argslist57530$anf_45bind483100)
store volatile %struct.ScmObj* %argslist57530$anf_45bind483101, %struct.ScmObj** %stackaddr$prim58702, align 8
%stackaddr$prim58703 = alloca %struct.ScmObj*, align 8
%argslist57530$anf_45bind483102 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48313, %struct.ScmObj* %argslist57530$anf_45bind483101)
store volatile %struct.ScmObj* %argslist57530$anf_45bind483102, %struct.ScmObj** %stackaddr$prim58703, align 8
%stackaddr$prim58704 = alloca %struct.ScmObj*, align 8
%argslist57530$anf_45bind483103 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48311, %struct.ScmObj* %argslist57530$anf_45bind483102)
store volatile %struct.ScmObj* %argslist57530$anf_45bind483103, %struct.ScmObj** %stackaddr$prim58704, align 8
%stackaddr$prim58705 = alloca %struct.ScmObj*, align 8
%argslist57530$anf_45bind483104 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52271, %struct.ScmObj* %argslist57530$anf_45bind483103)
store volatile %struct.ScmObj* %argslist57530$anf_45bind483104, %struct.ScmObj** %stackaddr$prim58705, align 8
%clofunc58706 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48310)
musttail call tailcc void %clofunc58706(%struct.ScmObj* %anf_45bind48310, %struct.ScmObj* %argslist57530$anf_45bind483104)
ret void
}

define tailcc void @proc_clo$ae52271(%struct.ScmObj* %env$ae52271,%struct.ScmObj* %current_45args57526) {
%stackaddr$env-ref58707 = alloca %struct.ScmObj*, align 8
%anf_45bind48309 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52271, i64 0)
store %struct.ScmObj* %anf_45bind48309, %struct.ScmObj** %stackaddr$env-ref58707
%stackaddr$env-ref58708 = alloca %struct.ScmObj*, align 8
%k48385 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52271, i64 1)
store %struct.ScmObj* %k48385, %struct.ScmObj** %stackaddr$env-ref58708
%stackaddr$prim58709 = alloca %struct.ScmObj*, align 8
%_95k48388 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57526)
store volatile %struct.ScmObj* %_95k48388, %struct.ScmObj** %stackaddr$prim58709, align 8
%stackaddr$prim58710 = alloca %struct.ScmObj*, align 8
%current_45args57527 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57526)
store volatile %struct.ScmObj* %current_45args57527, %struct.ScmObj** %stackaddr$prim58710, align 8
%stackaddr$prim58711 = alloca %struct.ScmObj*, align 8
%anf_45bind48314 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57527)
store volatile %struct.ScmObj* %anf_45bind48314, %struct.ScmObj** %stackaddr$prim58711, align 8
%stackaddr$prim58712 = alloca %struct.ScmObj*, align 8
%cpsprim48389 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %anf_45bind48309, %struct.ScmObj* %anf_45bind48314)
store volatile %struct.ScmObj* %cpsprim48389, %struct.ScmObj** %stackaddr$prim58712, align 8
%ae52278 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57529$k483850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58713 = alloca %struct.ScmObj*, align 8
%argslist57529$k483851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48389, %struct.ScmObj* %argslist57529$k483850)
store volatile %struct.ScmObj* %argslist57529$k483851, %struct.ScmObj** %stackaddr$prim58713, align 8
%stackaddr$prim58714 = alloca %struct.ScmObj*, align 8
%argslist57529$k483852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52278, %struct.ScmObj* %argslist57529$k483851)
store volatile %struct.ScmObj* %argslist57529$k483852, %struct.ScmObj** %stackaddr$prim58714, align 8
%clofunc58715 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48385)
musttail call tailcc void %clofunc58715(%struct.ScmObj* %k48385, %struct.ScmObj* %argslist57529$k483852)
ret void
}

define tailcc void @proc_clo$ae52234(%struct.ScmObj* %env$ae52234,%struct.ScmObj* %lst4816248393) {
%stackaddr$prim58716 = alloca %struct.ScmObj*, align 8
%k48394 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4816248393)
store volatile %struct.ScmObj* %k48394, %struct.ScmObj** %stackaddr$prim58716, align 8
%stackaddr$prim58717 = alloca %struct.ScmObj*, align 8
%lst48162 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4816248393)
store volatile %struct.ScmObj* %lst48162, %struct.ScmObj** %stackaddr$prim58717, align 8
%ae52238 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57533$k483940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58718 = alloca %struct.ScmObj*, align 8
%argslist57533$k483941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48162, %struct.ScmObj* %argslist57533$k483940)
store volatile %struct.ScmObj* %argslist57533$k483941, %struct.ScmObj** %stackaddr$prim58718, align 8
%stackaddr$prim58719 = alloca %struct.ScmObj*, align 8
%argslist57533$k483942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52238, %struct.ScmObj* %argslist57533$k483941)
store volatile %struct.ScmObj* %argslist57533$k483942, %struct.ScmObj** %stackaddr$prim58719, align 8
%clofunc58720 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48394)
musttail call tailcc void %clofunc58720(%struct.ScmObj* %k48394, %struct.ScmObj* %argslist57533$k483942)
ret void
}

define tailcc void @proc_clo$ae52364(%struct.ScmObj* %env$ae52364,%struct.ScmObj* %current_45args57536) {
%stackaddr$env-ref58721 = alloca %struct.ScmObj*, align 8
%k48385 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52364, i64 0)
store %struct.ScmObj* %k48385, %struct.ScmObj** %stackaddr$env-ref58721
%stackaddr$env-ref58722 = alloca %struct.ScmObj*, align 8
%x48161 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52364, i64 1)
store %struct.ScmObj* %x48161, %struct.ScmObj** %stackaddr$env-ref58722
%stackaddr$env-ref58723 = alloca %struct.ScmObj*, align 8
%my_45try48152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52364, i64 2)
store %struct.ScmObj* %my_45try48152, %struct.ScmObj** %stackaddr$env-ref58723
%stackaddr$env-ref58724 = alloca %struct.ScmObj*, align 8
%y48160 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52364, i64 3)
store %struct.ScmObj* %y48160, %struct.ScmObj** %stackaddr$env-ref58724
%stackaddr$env-ref58725 = alloca %struct.ScmObj*, align 8
%z48159 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52364, i64 4)
store %struct.ScmObj* %z48159, %struct.ScmObj** %stackaddr$env-ref58725
%stackaddr$prim58726 = alloca %struct.ScmObj*, align 8
%_95k48387 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57536)
store volatile %struct.ScmObj* %_95k48387, %struct.ScmObj** %stackaddr$prim58726, align 8
%stackaddr$prim58727 = alloca %struct.ScmObj*, align 8
%current_45args57537 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57536)
store volatile %struct.ScmObj* %current_45args57537, %struct.ScmObj** %stackaddr$prim58727, align 8
%stackaddr$prim58728 = alloca %struct.ScmObj*, align 8
%anf_45bind48309 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57537)
store volatile %struct.ScmObj* %anf_45bind48309, %struct.ScmObj** %stackaddr$prim58728, align 8
%ae52374 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58729 = alloca %struct.ScmObj*, align 8
%anf_45bind48310 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %my_45try48152, %struct.ScmObj* %ae52374)
store volatile %struct.ScmObj* %anf_45bind48310, %struct.ScmObj** %stackaddr$prim58729, align 8
%stackaddr$prim58730 = alloca %struct.ScmObj*, align 8
%anf_45bind48311 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48161)
store volatile %struct.ScmObj* %anf_45bind48311, %struct.ScmObj** %stackaddr$prim58730, align 8
%stackaddr$prim58731 = alloca %struct.ScmObj*, align 8
%anf_45bind48312 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48161)
store volatile %struct.ScmObj* %anf_45bind48312, %struct.ScmObj** %stackaddr$prim58731, align 8
%stackaddr$prim58732 = alloca %struct.ScmObj*, align 8
%anf_45bind48313 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48312, %struct.ScmObj* %y48160)
store volatile %struct.ScmObj* %anf_45bind48313, %struct.ScmObj** %stackaddr$prim58732, align 8
%stackaddr$makeclosure58733 = alloca %struct.ScmObj*, align 8
%fptrToInt58734 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52380 to i64
%ae52380 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58734)
store volatile %struct.ScmObj* %ae52380, %struct.ScmObj** %stackaddr$makeclosure58733, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52380, %struct.ScmObj* %anf_45bind48309, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52380, %struct.ScmObj* %k48385, i64 1)
%argslist57543$anf_45bind483100 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58735 = alloca %struct.ScmObj*, align 8
%argslist57543$anf_45bind483101 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %z48159, %struct.ScmObj* %argslist57543$anf_45bind483100)
store volatile %struct.ScmObj* %argslist57543$anf_45bind483101, %struct.ScmObj** %stackaddr$prim58735, align 8
%stackaddr$prim58736 = alloca %struct.ScmObj*, align 8
%argslist57543$anf_45bind483102 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48313, %struct.ScmObj* %argslist57543$anf_45bind483101)
store volatile %struct.ScmObj* %argslist57543$anf_45bind483102, %struct.ScmObj** %stackaddr$prim58736, align 8
%stackaddr$prim58737 = alloca %struct.ScmObj*, align 8
%argslist57543$anf_45bind483103 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48311, %struct.ScmObj* %argslist57543$anf_45bind483102)
store volatile %struct.ScmObj* %argslist57543$anf_45bind483103, %struct.ScmObj** %stackaddr$prim58737, align 8
%stackaddr$prim58738 = alloca %struct.ScmObj*, align 8
%argslist57543$anf_45bind483104 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52380, %struct.ScmObj* %argslist57543$anf_45bind483103)
store volatile %struct.ScmObj* %argslist57543$anf_45bind483104, %struct.ScmObj** %stackaddr$prim58738, align 8
%clofunc58739 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48310)
musttail call tailcc void %clofunc58739(%struct.ScmObj* %anf_45bind48310, %struct.ScmObj* %argslist57543$anf_45bind483104)
ret void
}

define tailcc void @proc_clo$ae52380(%struct.ScmObj* %env$ae52380,%struct.ScmObj* %current_45args57539) {
%stackaddr$env-ref58740 = alloca %struct.ScmObj*, align 8
%anf_45bind48309 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52380, i64 0)
store %struct.ScmObj* %anf_45bind48309, %struct.ScmObj** %stackaddr$env-ref58740
%stackaddr$env-ref58741 = alloca %struct.ScmObj*, align 8
%k48385 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52380, i64 1)
store %struct.ScmObj* %k48385, %struct.ScmObj** %stackaddr$env-ref58741
%stackaddr$prim58742 = alloca %struct.ScmObj*, align 8
%_95k48388 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57539)
store volatile %struct.ScmObj* %_95k48388, %struct.ScmObj** %stackaddr$prim58742, align 8
%stackaddr$prim58743 = alloca %struct.ScmObj*, align 8
%current_45args57540 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57539)
store volatile %struct.ScmObj* %current_45args57540, %struct.ScmObj** %stackaddr$prim58743, align 8
%stackaddr$prim58744 = alloca %struct.ScmObj*, align 8
%anf_45bind48314 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57540)
store volatile %struct.ScmObj* %anf_45bind48314, %struct.ScmObj** %stackaddr$prim58744, align 8
%stackaddr$prim58745 = alloca %struct.ScmObj*, align 8
%cpsprim48389 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %anf_45bind48309, %struct.ScmObj* %anf_45bind48314)
store volatile %struct.ScmObj* %cpsprim48389, %struct.ScmObj** %stackaddr$prim58745, align 8
%ae52387 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57542$k483850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58746 = alloca %struct.ScmObj*, align 8
%argslist57542$k483851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48389, %struct.ScmObj* %argslist57542$k483850)
store volatile %struct.ScmObj* %argslist57542$k483851, %struct.ScmObj** %stackaddr$prim58746, align 8
%stackaddr$prim58747 = alloca %struct.ScmObj*, align 8
%argslist57542$k483852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52387, %struct.ScmObj* %argslist57542$k483851)
store volatile %struct.ScmObj* %argslist57542$k483852, %struct.ScmObj** %stackaddr$prim58747, align 8
%clofunc58748 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48385)
musttail call tailcc void %clofunc58748(%struct.ScmObj* %k48385, %struct.ScmObj* %argslist57542$k483852)
ret void
}

define tailcc void @proc_clo$ae51919(%struct.ScmObj* %env$ae51919,%struct.ScmObj* %current_45args57547) {
%stackaddr$prim58749 = alloca %struct.ScmObj*, align 8
%k48395 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57547)
store volatile %struct.ScmObj* %k48395, %struct.ScmObj** %stackaddr$prim58749, align 8
%stackaddr$prim58750 = alloca %struct.ScmObj*, align 8
%current_45args57548 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57547)
store volatile %struct.ScmObj* %current_45args57548, %struct.ScmObj** %stackaddr$prim58750, align 8
%stackaddr$prim58751 = alloca %struct.ScmObj*, align 8
%n48164 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57548)
store volatile %struct.ScmObj* %n48164, %struct.ScmObj** %stackaddr$prim58751, align 8
%stackaddr$prim58752 = alloca %struct.ScmObj*, align 8
%anf_45bind48287 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind48287, %struct.ScmObj** %stackaddr$prim58752, align 8
%ae51920 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim58753 = alloca %struct.ScmObj*, align 8
%loop48165 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51920, %struct.ScmObj* %anf_45bind48287)
store volatile %struct.ScmObj* %loop48165, %struct.ScmObj** %stackaddr$prim58753, align 8
%stackaddr$makeclosure58754 = alloca %struct.ScmObj*, align 8
%fptrToInt58755 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51922 to i64
%ae51922 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58755)
store volatile %struct.ScmObj* %ae51922, %struct.ScmObj** %stackaddr$makeclosure58754, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51922, %struct.ScmObj* %loop48165, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51922, %struct.ScmObj* %n48164, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51922, %struct.ScmObj* %k48395, i64 2)
%ae51923 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58756 = alloca %struct.ScmObj*, align 8
%fptrToInt58757 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51924 to i64
%ae51924 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58757)
store volatile %struct.ScmObj* %ae51924, %struct.ScmObj** %stackaddr$makeclosure58756, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51924, %struct.ScmObj* %loop48165, i64 0)
%argslist57569$ae519220 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58758 = alloca %struct.ScmObj*, align 8
%argslist57569$ae519221 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51924, %struct.ScmObj* %argslist57569$ae519220)
store volatile %struct.ScmObj* %argslist57569$ae519221, %struct.ScmObj** %stackaddr$prim58758, align 8
%stackaddr$prim58759 = alloca %struct.ScmObj*, align 8
%argslist57569$ae519222 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51923, %struct.ScmObj* %argslist57569$ae519221)
store volatile %struct.ScmObj* %argslist57569$ae519222, %struct.ScmObj** %stackaddr$prim58759, align 8
%clofunc58760 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51922)
musttail call tailcc void %clofunc58760(%struct.ScmObj* %ae51922, %struct.ScmObj* %argslist57569$ae519222)
ret void
}

define tailcc void @proc_clo$ae51922(%struct.ScmObj* %env$ae51922,%struct.ScmObj* %current_45args57550) {
%stackaddr$env-ref58761 = alloca %struct.ScmObj*, align 8
%loop48165 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51922, i64 0)
store %struct.ScmObj* %loop48165, %struct.ScmObj** %stackaddr$env-ref58761
%stackaddr$env-ref58762 = alloca %struct.ScmObj*, align 8
%n48164 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51922, i64 1)
store %struct.ScmObj* %n48164, %struct.ScmObj** %stackaddr$env-ref58762
%stackaddr$env-ref58763 = alloca %struct.ScmObj*, align 8
%k48395 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51922, i64 2)
store %struct.ScmObj* %k48395, %struct.ScmObj** %stackaddr$env-ref58763
%stackaddr$prim58764 = alloca %struct.ScmObj*, align 8
%_95k48396 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57550)
store volatile %struct.ScmObj* %_95k48396, %struct.ScmObj** %stackaddr$prim58764, align 8
%stackaddr$prim58765 = alloca %struct.ScmObj*, align 8
%current_45args57551 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57550)
store volatile %struct.ScmObj* %current_45args57551, %struct.ScmObj** %stackaddr$prim58765, align 8
%stackaddr$prim58766 = alloca %struct.ScmObj*, align 8
%anf_45bind48292 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57551)
store volatile %struct.ScmObj* %anf_45bind48292, %struct.ScmObj** %stackaddr$prim58766, align 8
%ae52001 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58767 = alloca %struct.ScmObj*, align 8
%t4802948166 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %loop48165, %struct.ScmObj* %ae52001, %struct.ScmObj* %anf_45bind48292)
store volatile %struct.ScmObj* %t4802948166, %struct.ScmObj** %stackaddr$prim58767, align 8
%ae52004 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58768 = alloca %struct.ScmObj*, align 8
%anf_45bind48293 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %loop48165, %struct.ScmObj* %ae52004)
store volatile %struct.ScmObj* %anf_45bind48293, %struct.ScmObj** %stackaddr$prim58768, align 8
%stackaddr$makeclosure58769 = alloca %struct.ScmObj*, align 8
%fptrToInt58770 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52005 to i64
%ae52005 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58770)
store volatile %struct.ScmObj* %ae52005, %struct.ScmObj** %stackaddr$makeclosure58769, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52005, %struct.ScmObj* %anf_45bind48293, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52005, %struct.ScmObj* %n48164, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52005, %struct.ScmObj* %k48395, i64 2)
%ae52006 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58771 = alloca %struct.ScmObj*, align 8
%fptrToInt58772 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52007 to i64
%ae52007 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58772)
store volatile %struct.ScmObj* %ae52007, %struct.ScmObj** %stackaddr$makeclosure58771, align 8
%argslist57562$ae520050 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58773 = alloca %struct.ScmObj*, align 8
%argslist57562$ae520051 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52007, %struct.ScmObj* %argslist57562$ae520050)
store volatile %struct.ScmObj* %argslist57562$ae520051, %struct.ScmObj** %stackaddr$prim58773, align 8
%stackaddr$prim58774 = alloca %struct.ScmObj*, align 8
%argslist57562$ae520052 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52006, %struct.ScmObj* %argslist57562$ae520051)
store volatile %struct.ScmObj* %argslist57562$ae520052, %struct.ScmObj** %stackaddr$prim58774, align 8
%clofunc58775 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52005)
musttail call tailcc void %clofunc58775(%struct.ScmObj* %ae52005, %struct.ScmObj* %argslist57562$ae520052)
ret void
}

define tailcc void @proc_clo$ae52005(%struct.ScmObj* %env$ae52005,%struct.ScmObj* %current_45args57553) {
%stackaddr$env-ref58776 = alloca %struct.ScmObj*, align 8
%anf_45bind48293 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52005, i64 0)
store %struct.ScmObj* %anf_45bind48293, %struct.ScmObj** %stackaddr$env-ref58776
%stackaddr$env-ref58777 = alloca %struct.ScmObj*, align 8
%n48164 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52005, i64 1)
store %struct.ScmObj* %n48164, %struct.ScmObj** %stackaddr$env-ref58777
%stackaddr$env-ref58778 = alloca %struct.ScmObj*, align 8
%k48395 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52005, i64 2)
store %struct.ScmObj* %k48395, %struct.ScmObj** %stackaddr$env-ref58778
%stackaddr$prim58779 = alloca %struct.ScmObj*, align 8
%_95k48397 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57553)
store volatile %struct.ScmObj* %_95k48397, %struct.ScmObj** %stackaddr$prim58779, align 8
%stackaddr$prim58780 = alloca %struct.ScmObj*, align 8
%current_45args57554 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57553)
store volatile %struct.ScmObj* %current_45args57554, %struct.ScmObj** %stackaddr$prim58780, align 8
%stackaddr$prim58781 = alloca %struct.ScmObj*, align 8
%anf_45bind48294 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57554)
store volatile %struct.ScmObj* %anf_45bind48294, %struct.ScmObj** %stackaddr$prim58781, align 8
%stackaddr$makeclosure58782 = alloca %struct.ScmObj*, align 8
%fptrToInt58783 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52028 to i64
%ae52028 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58783)
store volatile %struct.ScmObj* %ae52028, %struct.ScmObj** %stackaddr$makeclosure58782, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52028, %struct.ScmObj* %anf_45bind48293, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52028, %struct.ScmObj* %n48164, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52028, %struct.ScmObj* %k48395, i64 2)
%argslist57560$anf_45bind482940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58784 = alloca %struct.ScmObj*, align 8
%argslist57560$anf_45bind482941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52028, %struct.ScmObj* %argslist57560$anf_45bind482940)
store volatile %struct.ScmObj* %argslist57560$anf_45bind482941, %struct.ScmObj** %stackaddr$prim58784, align 8
%clofunc58785 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48294)
musttail call tailcc void %clofunc58785(%struct.ScmObj* %anf_45bind48294, %struct.ScmObj* %argslist57560$anf_45bind482941)
ret void
}

define tailcc void @proc_clo$ae52028(%struct.ScmObj* %env$ae52028,%struct.ScmObj* %current_45args57556) {
%stackaddr$env-ref58786 = alloca %struct.ScmObj*, align 8
%anf_45bind48293 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52028, i64 0)
store %struct.ScmObj* %anf_45bind48293, %struct.ScmObj** %stackaddr$env-ref58786
%stackaddr$env-ref58787 = alloca %struct.ScmObj*, align 8
%n48164 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52028, i64 1)
store %struct.ScmObj* %n48164, %struct.ScmObj** %stackaddr$env-ref58787
%stackaddr$env-ref58788 = alloca %struct.ScmObj*, align 8
%k48395 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52028, i64 2)
store %struct.ScmObj* %k48395, %struct.ScmObj** %stackaddr$env-ref58788
%stackaddr$prim58789 = alloca %struct.ScmObj*, align 8
%_95k48398 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57556)
store volatile %struct.ScmObj* %_95k48398, %struct.ScmObj** %stackaddr$prim58789, align 8
%stackaddr$prim58790 = alloca %struct.ScmObj*, align 8
%current_45args57557 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57556)
store volatile %struct.ScmObj* %current_45args57557, %struct.ScmObj** %stackaddr$prim58790, align 8
%stackaddr$prim58791 = alloca %struct.ScmObj*, align 8
%anf_45bind48295 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57557)
store volatile %struct.ScmObj* %anf_45bind48295, %struct.ScmObj** %stackaddr$prim58791, align 8
%argslist57559$anf_45bind482930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58792 = alloca %struct.ScmObj*, align 8
%argslist57559$anf_45bind482931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48295, %struct.ScmObj* %argslist57559$anf_45bind482930)
store volatile %struct.ScmObj* %argslist57559$anf_45bind482931, %struct.ScmObj** %stackaddr$prim58792, align 8
%stackaddr$prim58793 = alloca %struct.ScmObj*, align 8
%argslist57559$anf_45bind482932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %n48164, %struct.ScmObj* %argslist57559$anf_45bind482931)
store volatile %struct.ScmObj* %argslist57559$anf_45bind482932, %struct.ScmObj** %stackaddr$prim58793, align 8
%stackaddr$prim58794 = alloca %struct.ScmObj*, align 8
%argslist57559$anf_45bind482933 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48395, %struct.ScmObj* %argslist57559$anf_45bind482932)
store volatile %struct.ScmObj* %argslist57559$anf_45bind482933, %struct.ScmObj** %stackaddr$prim58794, align 8
%clofunc58795 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48293)
musttail call tailcc void %clofunc58795(%struct.ScmObj* %anf_45bind48293, %struct.ScmObj* %argslist57559$anf_45bind482933)
ret void
}

define tailcc void @proc_clo$ae52007(%struct.ScmObj* %env$ae52007,%struct.ScmObj* %lst4816948399) {
%stackaddr$prim58796 = alloca %struct.ScmObj*, align 8
%k48400 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4816948399)
store volatile %struct.ScmObj* %k48400, %struct.ScmObj** %stackaddr$prim58796, align 8
%stackaddr$prim58797 = alloca %struct.ScmObj*, align 8
%lst48169 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4816948399)
store volatile %struct.ScmObj* %lst48169, %struct.ScmObj** %stackaddr$prim58797, align 8
%ae52011 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57561$k484000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58798 = alloca %struct.ScmObj*, align 8
%argslist57561$k484001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48169, %struct.ScmObj* %argslist57561$k484000)
store volatile %struct.ScmObj* %argslist57561$k484001, %struct.ScmObj** %stackaddr$prim58798, align 8
%stackaddr$prim58799 = alloca %struct.ScmObj*, align 8
%argslist57561$k484002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52011, %struct.ScmObj* %argslist57561$k484001)
store volatile %struct.ScmObj* %argslist57561$k484002, %struct.ScmObj** %stackaddr$prim58799, align 8
%clofunc58800 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48400)
musttail call tailcc void %clofunc58800(%struct.ScmObj* %k48400, %struct.ScmObj* %argslist57561$k484002)
ret void
}

define tailcc void @proc_clo$ae51924(%struct.ScmObj* %env$ae51924,%struct.ScmObj* %current_45args57563) {
%stackaddr$env-ref58801 = alloca %struct.ScmObj*, align 8
%loop48165 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51924, i64 0)
store %struct.ScmObj* %loop48165, %struct.ScmObj** %stackaddr$env-ref58801
%stackaddr$prim58802 = alloca %struct.ScmObj*, align 8
%k48401 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57563)
store volatile %struct.ScmObj* %k48401, %struct.ScmObj** %stackaddr$prim58802, align 8
%stackaddr$prim58803 = alloca %struct.ScmObj*, align 8
%current_45args57564 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57563)
store volatile %struct.ScmObj* %current_45args57564, %struct.ScmObj** %stackaddr$prim58803, align 8
%stackaddr$prim58804 = alloca %struct.ScmObj*, align 8
%i48168 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57564)
store volatile %struct.ScmObj* %i48168, %struct.ScmObj** %stackaddr$prim58804, align 8
%stackaddr$prim58805 = alloca %struct.ScmObj*, align 8
%current_45args57565 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57564)
store volatile %struct.ScmObj* %current_45args57565, %struct.ScmObj** %stackaddr$prim58805, align 8
%stackaddr$prim58806 = alloca %struct.ScmObj*, align 8
%l48167 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57565)
store volatile %struct.ScmObj* %l48167, %struct.ScmObj** %stackaddr$prim58806, align 8
%ae51926 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58807 = alloca %struct.ScmObj*, align 8
%anf_45bind48288 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %i48168, %struct.ScmObj* %ae51926)
store volatile %struct.ScmObj* %anf_45bind48288, %struct.ScmObj** %stackaddr$prim58807, align 8
%truthy$cmp58808 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48288)
%cmp$cmp58808 = icmp eq i64 %truthy$cmp58808, 1
br i1 %cmp$cmp58808, label %truebranch$cmp58808, label %falsebranch$cmp58808
truebranch$cmp58808:
%ae51929 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57567$k484010 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58809 = alloca %struct.ScmObj*, align 8
%argslist57567$k484011 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %l48167, %struct.ScmObj* %argslist57567$k484010)
store volatile %struct.ScmObj* %argslist57567$k484011, %struct.ScmObj** %stackaddr$prim58809, align 8
%stackaddr$prim58810 = alloca %struct.ScmObj*, align 8
%argslist57567$k484012 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51929, %struct.ScmObj* %argslist57567$k484011)
store volatile %struct.ScmObj* %argslist57567$k484012, %struct.ScmObj** %stackaddr$prim58810, align 8
%clofunc58811 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48401)
musttail call tailcc void %clofunc58811(%struct.ScmObj* %k48401, %struct.ScmObj* %argslist57567$k484012)
ret void
falsebranch$cmp58808:
%ae51935 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58812 = alloca %struct.ScmObj*, align 8
%anf_45bind48289 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %loop48165, %struct.ScmObj* %ae51935)
store volatile %struct.ScmObj* %anf_45bind48289, %struct.ScmObj** %stackaddr$prim58812, align 8
%ae51937 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim58813 = alloca %struct.ScmObj*, align 8
%anf_45bind48290 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %i48168, %struct.ScmObj* %ae51937)
store volatile %struct.ScmObj* %anf_45bind48290, %struct.ScmObj** %stackaddr$prim58813, align 8
%stackaddr$prim58814 = alloca %struct.ScmObj*, align 8
%anf_45bind48291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %i48168, %struct.ScmObj* %l48167)
store volatile %struct.ScmObj* %anf_45bind48291, %struct.ScmObj** %stackaddr$prim58814, align 8
%argslist57568$anf_45bind482890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58815 = alloca %struct.ScmObj*, align 8
%argslist57568$anf_45bind482891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48291, %struct.ScmObj* %argslist57568$anf_45bind482890)
store volatile %struct.ScmObj* %argslist57568$anf_45bind482891, %struct.ScmObj** %stackaddr$prim58815, align 8
%stackaddr$prim58816 = alloca %struct.ScmObj*, align 8
%argslist57568$anf_45bind482892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48290, %struct.ScmObj* %argslist57568$anf_45bind482891)
store volatile %struct.ScmObj* %argslist57568$anf_45bind482892, %struct.ScmObj** %stackaddr$prim58816, align 8
%stackaddr$prim58817 = alloca %struct.ScmObj*, align 8
%argslist57568$anf_45bind482893 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48401, %struct.ScmObj* %argslist57568$anf_45bind482892)
store volatile %struct.ScmObj* %argslist57568$anf_45bind482893, %struct.ScmObj** %stackaddr$prim58817, align 8
%clofunc58818 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48289)
musttail call tailcc void %clofunc58818(%struct.ScmObj* %anf_45bind48289, %struct.ScmObj* %argslist57568$anf_45bind482893)
ret void
}

define tailcc void @proc_clo$ae51882(%struct.ScmObj* %env$ae51882,%struct.ScmObj* %current_45args57572) {
%stackaddr$prim58819 = alloca %struct.ScmObj*, align 8
%k48402 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57572)
store volatile %struct.ScmObj* %k48402, %struct.ScmObj** %stackaddr$prim58819, align 8
%stackaddr$prim58820 = alloca %struct.ScmObj*, align 8
%current_45args57573 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57572)
store volatile %struct.ScmObj* %current_45args57573, %struct.ScmObj** %stackaddr$prim58820, align 8
%stackaddr$prim58821 = alloca %struct.ScmObj*, align 8
%x48088 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57573)
store volatile %struct.ScmObj* %x48088, %struct.ScmObj** %stackaddr$prim58821, align 8
%stackaddr$prim58822 = alloca %struct.ScmObj*, align 8
%anf_45bind48280 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48088)
store volatile %struct.ScmObj* %anf_45bind48280, %struct.ScmObj** %stackaddr$prim58822, align 8
%stackaddr$prim58823 = alloca %struct.ScmObj*, align 8
%anf_45bind48281 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48280)
store volatile %struct.ScmObj* %anf_45bind48281, %struct.ScmObj** %stackaddr$prim58823, align 8
%stackaddr$prim58824 = alloca %struct.ScmObj*, align 8
%anf_45bind48282 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48281)
store volatile %struct.ScmObj* %anf_45bind48282, %struct.ScmObj** %stackaddr$prim58824, align 8
%stackaddr$prim58825 = alloca %struct.ScmObj*, align 8
%cpsprim48403 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48282)
store volatile %struct.ScmObj* %cpsprim48403, %struct.ScmObj** %stackaddr$prim58825, align 8
%ae51888 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57575$k484020 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58826 = alloca %struct.ScmObj*, align 8
%argslist57575$k484021 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48403, %struct.ScmObj* %argslist57575$k484020)
store volatile %struct.ScmObj* %argslist57575$k484021, %struct.ScmObj** %stackaddr$prim58826, align 8
%stackaddr$prim58827 = alloca %struct.ScmObj*, align 8
%argslist57575$k484022 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51888, %struct.ScmObj* %argslist57575$k484021)
store volatile %struct.ScmObj* %argslist57575$k484022, %struct.ScmObj** %stackaddr$prim58827, align 8
%clofunc58828 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48402)
musttail call tailcc void %clofunc58828(%struct.ScmObj* %k48402, %struct.ScmObj* %argslist57575$k484022)
ret void
}

define tailcc void @proc_clo$ae51858(%struct.ScmObj* %env$ae51858,%struct.ScmObj* %current_45args57577) {
%stackaddr$prim58829 = alloca %struct.ScmObj*, align 8
%k48404 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57577)
store volatile %struct.ScmObj* %k48404, %struct.ScmObj** %stackaddr$prim58829, align 8
%stackaddr$prim58830 = alloca %struct.ScmObj*, align 8
%current_45args57578 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57577)
store volatile %struct.ScmObj* %current_45args57578, %struct.ScmObj** %stackaddr$prim58830, align 8
%stackaddr$prim58831 = alloca %struct.ScmObj*, align 8
%x48090 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57578)
store volatile %struct.ScmObj* %x48090, %struct.ScmObj** %stackaddr$prim58831, align 8
%stackaddr$prim58832 = alloca %struct.ScmObj*, align 8
%anf_45bind48278 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48090)
store volatile %struct.ScmObj* %anf_45bind48278, %struct.ScmObj** %stackaddr$prim58832, align 8
%stackaddr$prim58833 = alloca %struct.ScmObj*, align 8
%anf_45bind48279 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48278)
store volatile %struct.ScmObj* %anf_45bind48279, %struct.ScmObj** %stackaddr$prim58833, align 8
%stackaddr$prim58834 = alloca %struct.ScmObj*, align 8
%cpsprim48405 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48279)
store volatile %struct.ScmObj* %cpsprim48405, %struct.ScmObj** %stackaddr$prim58834, align 8
%ae51863 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57580$k484040 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58835 = alloca %struct.ScmObj*, align 8
%argslist57580$k484041 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48405, %struct.ScmObj* %argslist57580$k484040)
store volatile %struct.ScmObj* %argslist57580$k484041, %struct.ScmObj** %stackaddr$prim58835, align 8
%stackaddr$prim58836 = alloca %struct.ScmObj*, align 8
%argslist57580$k484042 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51863, %struct.ScmObj* %argslist57580$k484041)
store volatile %struct.ScmObj* %argslist57580$k484042, %struct.ScmObj** %stackaddr$prim58836, align 8
%clofunc58837 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48404)
musttail call tailcc void %clofunc58837(%struct.ScmObj* %k48404, %struct.ScmObj* %argslist57580$k484042)
ret void
}

define tailcc void @proc_clo$ae51836(%struct.ScmObj* %env$ae51836,%struct.ScmObj* %current_45args57582) {
%stackaddr$prim58838 = alloca %struct.ScmObj*, align 8
%k48406 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57582)
store volatile %struct.ScmObj* %k48406, %struct.ScmObj** %stackaddr$prim58838, align 8
%stackaddr$prim58839 = alloca %struct.ScmObj*, align 8
%current_45args57583 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57582)
store volatile %struct.ScmObj* %current_45args57583, %struct.ScmObj** %stackaddr$prim58839, align 8
%stackaddr$prim58840 = alloca %struct.ScmObj*, align 8
%x48092 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57583)
store volatile %struct.ScmObj* %x48092, %struct.ScmObj** %stackaddr$prim58840, align 8
%stackaddr$prim58841 = alloca %struct.ScmObj*, align 8
%anf_45bind48277 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48092)
store volatile %struct.ScmObj* %anf_45bind48277, %struct.ScmObj** %stackaddr$prim58841, align 8
%stackaddr$prim58842 = alloca %struct.ScmObj*, align 8
%cpsprim48407 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48277)
store volatile %struct.ScmObj* %cpsprim48407, %struct.ScmObj** %stackaddr$prim58842, align 8
%ae51840 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57585$k484060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58843 = alloca %struct.ScmObj*, align 8
%argslist57585$k484061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48407, %struct.ScmObj* %argslist57585$k484060)
store volatile %struct.ScmObj* %argslist57585$k484061, %struct.ScmObj** %stackaddr$prim58843, align 8
%stackaddr$prim58844 = alloca %struct.ScmObj*, align 8
%argslist57585$k484062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51840, %struct.ScmObj* %argslist57585$k484061)
store volatile %struct.ScmObj* %argslist57585$k484062, %struct.ScmObj** %stackaddr$prim58844, align 8
%clofunc58845 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48406)
musttail call tailcc void %clofunc58845(%struct.ScmObj* %k48406, %struct.ScmObj* %argslist57585$k484062)
ret void
}

define tailcc void @proc_clo$ae51816(%struct.ScmObj* %env$ae51816,%struct.ScmObj* %current_45args57587) {
%stackaddr$prim58846 = alloca %struct.ScmObj*, align 8
%k48408 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57587)
store volatile %struct.ScmObj* %k48408, %struct.ScmObj** %stackaddr$prim58846, align 8
%stackaddr$prim58847 = alloca %struct.ScmObj*, align 8
%current_45args57588 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57587)
store volatile %struct.ScmObj* %current_45args57588, %struct.ScmObj** %stackaddr$prim58847, align 8
%stackaddr$prim58848 = alloca %struct.ScmObj*, align 8
%x48094 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57588)
store volatile %struct.ScmObj* %x48094, %struct.ScmObj** %stackaddr$prim58848, align 8
%stackaddr$prim58849 = alloca %struct.ScmObj*, align 8
%cpsprim48409 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48094)
store volatile %struct.ScmObj* %cpsprim48409, %struct.ScmObj** %stackaddr$prim58849, align 8
%ae51819 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57590$k484080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58850 = alloca %struct.ScmObj*, align 8
%argslist57590$k484081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48409, %struct.ScmObj* %argslist57590$k484080)
store volatile %struct.ScmObj* %argslist57590$k484081, %struct.ScmObj** %stackaddr$prim58850, align 8
%stackaddr$prim58851 = alloca %struct.ScmObj*, align 8
%argslist57590$k484082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51819, %struct.ScmObj* %argslist57590$k484081)
store volatile %struct.ScmObj* %argslist57590$k484082, %struct.ScmObj** %stackaddr$prim58851, align 8
%clofunc58852 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48408)
musttail call tailcc void %clofunc58852(%struct.ScmObj* %k48408, %struct.ScmObj* %argslist57590$k484082)
ret void
}

define tailcc void @proc_clo$ae51718(%struct.ScmObj* %env$ae51718,%struct.ScmObj* %args4809648410) {
%stackaddr$env-ref58853 = alloca %struct.ScmObj*, align 8
%_37foldl148035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51718, i64 0)
store %struct.ScmObj* %_37foldl148035, %struct.ScmObj** %stackaddr$env-ref58853
%stackaddr$prim58854 = alloca %struct.ScmObj*, align 8
%k48411 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4809648410)
store volatile %struct.ScmObj* %k48411, %struct.ScmObj** %stackaddr$prim58854, align 8
%stackaddr$prim58855 = alloca %struct.ScmObj*, align 8
%args48096 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4809648410)
store volatile %struct.ScmObj* %args48096, %struct.ScmObj** %stackaddr$prim58855, align 8
%stackaddr$prim58856 = alloca %struct.ScmObj*, align 8
%anf_45bind48271 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args48096)
store volatile %struct.ScmObj* %anf_45bind48271, %struct.ScmObj** %stackaddr$prim58856, align 8
%truthy$cmp58857 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48271)
%cmp$cmp58857 = icmp eq i64 %truthy$cmp58857, 1
br i1 %cmp$cmp58857, label %truebranch$cmp58857, label %falsebranch$cmp58857
truebranch$cmp58857:
%ae51724 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51725 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist57592$k484110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58858 = alloca %struct.ScmObj*, align 8
%argslist57592$k484111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51725, %struct.ScmObj* %argslist57592$k484110)
store volatile %struct.ScmObj* %argslist57592$k484111, %struct.ScmObj** %stackaddr$prim58858, align 8
%stackaddr$prim58859 = alloca %struct.ScmObj*, align 8
%argslist57592$k484112 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51724, %struct.ScmObj* %argslist57592$k484111)
store volatile %struct.ScmObj* %argslist57592$k484112, %struct.ScmObj** %stackaddr$prim58859, align 8
%clofunc58860 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48411)
musttail call tailcc void %clofunc58860(%struct.ScmObj* %k48411, %struct.ScmObj* %argslist57592$k484112)
ret void
falsebranch$cmp58857:
%stackaddr$prim58861 = alloca %struct.ScmObj*, align 8
%anf_45bind48272 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48096)
store volatile %struct.ScmObj* %anf_45bind48272, %struct.ScmObj** %stackaddr$prim58861, align 8
%stackaddr$prim58862 = alloca %struct.ScmObj*, align 8
%anf_45bind48273 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48272)
store volatile %struct.ScmObj* %anf_45bind48273, %struct.ScmObj** %stackaddr$prim58862, align 8
%truthy$cmp58863 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48273)
%cmp$cmp58863 = icmp eq i64 %truthy$cmp58863, 1
br i1 %cmp$cmp58863, label %truebranch$cmp58863, label %falsebranch$cmp58863
truebranch$cmp58863:
%stackaddr$prim58864 = alloca %struct.ScmObj*, align 8
%cpsprim48412 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48096)
store volatile %struct.ScmObj* %cpsprim48412, %struct.ScmObj** %stackaddr$prim58864, align 8
%ae51737 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57593$k484110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58865 = alloca %struct.ScmObj*, align 8
%argslist57593$k484111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48412, %struct.ScmObj* %argslist57593$k484110)
store volatile %struct.ScmObj* %argslist57593$k484111, %struct.ScmObj** %stackaddr$prim58865, align 8
%stackaddr$prim58866 = alloca %struct.ScmObj*, align 8
%argslist57593$k484112 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51737, %struct.ScmObj* %argslist57593$k484111)
store volatile %struct.ScmObj* %argslist57593$k484112, %struct.ScmObj** %stackaddr$prim58866, align 8
%clofunc58867 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48411)
musttail call tailcc void %clofunc58867(%struct.ScmObj* %k48411, %struct.ScmObj* %argslist57593$k484112)
ret void
falsebranch$cmp58863:
%stackaddr$makeclosure58868 = alloca %struct.ScmObj*, align 8
%fptrToInt58869 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51742 to i64
%ae51742 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58869)
store volatile %struct.ScmObj* %ae51742, %struct.ScmObj** %stackaddr$makeclosure58868, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51742, %struct.ScmObj* %_37foldl148035, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51742, %struct.ScmObj* %args48096, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51742, %struct.ScmObj* %k48411, i64 2)
%ae51743 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58870 = alloca %struct.ScmObj*, align 8
%fptrToInt58871 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51744 to i64
%ae51744 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58871)
store volatile %struct.ScmObj* %ae51744, %struct.ScmObj** %stackaddr$makeclosure58870, align 8
%argslist57603$ae517420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58872 = alloca %struct.ScmObj*, align 8
%argslist57603$ae517421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51744, %struct.ScmObj* %argslist57603$ae517420)
store volatile %struct.ScmObj* %argslist57603$ae517421, %struct.ScmObj** %stackaddr$prim58872, align 8
%stackaddr$prim58873 = alloca %struct.ScmObj*, align 8
%argslist57603$ae517422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51743, %struct.ScmObj* %argslist57603$ae517421)
store volatile %struct.ScmObj* %argslist57603$ae517422, %struct.ScmObj** %stackaddr$prim58873, align 8
%clofunc58874 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51742)
musttail call tailcc void %clofunc58874(%struct.ScmObj* %ae51742, %struct.ScmObj* %argslist57603$ae517422)
ret void
}

define tailcc void @proc_clo$ae51742(%struct.ScmObj* %env$ae51742,%struct.ScmObj* %current_45args57594) {
%stackaddr$env-ref58875 = alloca %struct.ScmObj*, align 8
%_37foldl148035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51742, i64 0)
store %struct.ScmObj* %_37foldl148035, %struct.ScmObj** %stackaddr$env-ref58875
%stackaddr$env-ref58876 = alloca %struct.ScmObj*, align 8
%args48096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51742, i64 1)
store %struct.ScmObj* %args48096, %struct.ScmObj** %stackaddr$env-ref58876
%stackaddr$env-ref58877 = alloca %struct.ScmObj*, align 8
%k48411 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51742, i64 2)
store %struct.ScmObj* %k48411, %struct.ScmObj** %stackaddr$env-ref58877
%stackaddr$prim58878 = alloca %struct.ScmObj*, align 8
%_95k48413 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57594)
store volatile %struct.ScmObj* %_95k48413, %struct.ScmObj** %stackaddr$prim58878, align 8
%stackaddr$prim58879 = alloca %struct.ScmObj*, align 8
%current_45args57595 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57594)
store volatile %struct.ScmObj* %current_45args57595, %struct.ScmObj** %stackaddr$prim58879, align 8
%stackaddr$prim58880 = alloca %struct.ScmObj*, align 8
%anf_45bind48274 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57595)
store volatile %struct.ScmObj* %anf_45bind48274, %struct.ScmObj** %stackaddr$prim58880, align 8
%stackaddr$prim58881 = alloca %struct.ScmObj*, align 8
%anf_45bind48275 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48096)
store volatile %struct.ScmObj* %anf_45bind48275, %struct.ScmObj** %stackaddr$prim58881, align 8
%stackaddr$prim58882 = alloca %struct.ScmObj*, align 8
%anf_45bind48276 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48096)
store volatile %struct.ScmObj* %anf_45bind48276, %struct.ScmObj** %stackaddr$prim58882, align 8
%argslist57597$_37foldl1480350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58883 = alloca %struct.ScmObj*, align 8
%argslist57597$_37foldl1480351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48276, %struct.ScmObj* %argslist57597$_37foldl1480350)
store volatile %struct.ScmObj* %argslist57597$_37foldl1480351, %struct.ScmObj** %stackaddr$prim58883, align 8
%stackaddr$prim58884 = alloca %struct.ScmObj*, align 8
%argslist57597$_37foldl1480352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48275, %struct.ScmObj* %argslist57597$_37foldl1480351)
store volatile %struct.ScmObj* %argslist57597$_37foldl1480352, %struct.ScmObj** %stackaddr$prim58884, align 8
%stackaddr$prim58885 = alloca %struct.ScmObj*, align 8
%argslist57597$_37foldl1480353 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48274, %struct.ScmObj* %argslist57597$_37foldl1480352)
store volatile %struct.ScmObj* %argslist57597$_37foldl1480353, %struct.ScmObj** %stackaddr$prim58885, align 8
%stackaddr$prim58886 = alloca %struct.ScmObj*, align 8
%argslist57597$_37foldl1480354 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48411, %struct.ScmObj* %argslist57597$_37foldl1480353)
store volatile %struct.ScmObj* %argslist57597$_37foldl1480354, %struct.ScmObj** %stackaddr$prim58886, align 8
%clofunc58887 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148035)
musttail call tailcc void %clofunc58887(%struct.ScmObj* %_37foldl148035, %struct.ScmObj* %argslist57597$_37foldl1480354)
ret void
}

define tailcc void @proc_clo$ae51744(%struct.ScmObj* %env$ae51744,%struct.ScmObj* %current_45args57598) {
%stackaddr$prim58888 = alloca %struct.ScmObj*, align 8
%k48414 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57598)
store volatile %struct.ScmObj* %k48414, %struct.ScmObj** %stackaddr$prim58888, align 8
%stackaddr$prim58889 = alloca %struct.ScmObj*, align 8
%current_45args57599 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57598)
store volatile %struct.ScmObj* %current_45args57599, %struct.ScmObj** %stackaddr$prim58889, align 8
%stackaddr$prim58890 = alloca %struct.ScmObj*, align 8
%n48098 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57599)
store volatile %struct.ScmObj* %n48098, %struct.ScmObj** %stackaddr$prim58890, align 8
%stackaddr$prim58891 = alloca %struct.ScmObj*, align 8
%current_45args57600 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57599)
store volatile %struct.ScmObj* %current_45args57600, %struct.ScmObj** %stackaddr$prim58891, align 8
%stackaddr$prim58892 = alloca %struct.ScmObj*, align 8
%v48097 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57600)
store volatile %struct.ScmObj* %v48097, %struct.ScmObj** %stackaddr$prim58892, align 8
%stackaddr$prim58893 = alloca %struct.ScmObj*, align 8
%cpsprim48415 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v48097, %struct.ScmObj* %n48098)
store volatile %struct.ScmObj* %cpsprim48415, %struct.ScmObj** %stackaddr$prim58893, align 8
%ae51748 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57602$k484140 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58894 = alloca %struct.ScmObj*, align 8
%argslist57602$k484141 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48415, %struct.ScmObj* %argslist57602$k484140)
store volatile %struct.ScmObj* %argslist57602$k484141, %struct.ScmObj** %stackaddr$prim58894, align 8
%stackaddr$prim58895 = alloca %struct.ScmObj*, align 8
%argslist57602$k484142 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51748, %struct.ScmObj* %argslist57602$k484141)
store volatile %struct.ScmObj* %argslist57602$k484142, %struct.ScmObj** %stackaddr$prim58895, align 8
%clofunc58896 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48414)
musttail call tailcc void %clofunc58896(%struct.ScmObj* %k48414, %struct.ScmObj* %argslist57602$k484142)
ret void
}

define tailcc void @proc_clo$ae51314(%struct.ScmObj* %env$ae51314,%struct.ScmObj* %current_45args57605) {
%stackaddr$prim58897 = alloca %struct.ScmObj*, align 8
%k48416 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57605)
store volatile %struct.ScmObj* %k48416, %struct.ScmObj** %stackaddr$prim58897, align 8
%stackaddr$prim58898 = alloca %struct.ScmObj*, align 8
%current_45args57606 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57605)
store volatile %struct.ScmObj* %current_45args57606, %struct.ScmObj** %stackaddr$prim58898, align 8
%stackaddr$prim58899 = alloca %struct.ScmObj*, align 8
%v48101 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57606)
store volatile %struct.ScmObj* %v48101, %struct.ScmObj** %stackaddr$prim58899, align 8
%stackaddr$prim58900 = alloca %struct.ScmObj*, align 8
%current_45args57607 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57606)
store volatile %struct.ScmObj* %current_45args57607, %struct.ScmObj** %stackaddr$prim58900, align 8
%stackaddr$prim58901 = alloca %struct.ScmObj*, align 8
%lst48100 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57607)
store volatile %struct.ScmObj* %lst48100, %struct.ScmObj** %stackaddr$prim58901, align 8
%ae51315 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim58902 = alloca %struct.ScmObj*, align 8
%lst48102 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51315, %struct.ScmObj* %lst48100)
store volatile %struct.ScmObj* %lst48102, %struct.ScmObj** %stackaddr$prim58902, align 8
%stackaddr$makeclosure58903 = alloca %struct.ScmObj*, align 8
%fptrToInt58904 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51317 to i64
%ae51317 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58904)
store volatile %struct.ScmObj* %ae51317, %struct.ScmObj** %stackaddr$makeclosure58903, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51317, %struct.ScmObj* %lst48102, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51317, %struct.ScmObj* %v48101, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51317, %struct.ScmObj* %k48416, i64 2)
%ae51318 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58905 = alloca %struct.ScmObj*, align 8
%fptrToInt58906 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51319 to i64
%ae51319 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58906)
store volatile %struct.ScmObj* %ae51319, %struct.ScmObj** %stackaddr$makeclosure58905, align 8
%argslist57629$ae513170 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58907 = alloca %struct.ScmObj*, align 8
%argslist57629$ae513171 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51319, %struct.ScmObj* %argslist57629$ae513170)
store volatile %struct.ScmObj* %argslist57629$ae513171, %struct.ScmObj** %stackaddr$prim58907, align 8
%stackaddr$prim58908 = alloca %struct.ScmObj*, align 8
%argslist57629$ae513172 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51318, %struct.ScmObj* %argslist57629$ae513171)
store volatile %struct.ScmObj* %argslist57629$ae513172, %struct.ScmObj** %stackaddr$prim58908, align 8
%clofunc58909 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51317)
musttail call tailcc void %clofunc58909(%struct.ScmObj* %ae51317, %struct.ScmObj* %argslist57629$ae513172)
ret void
}

define tailcc void @proc_clo$ae51317(%struct.ScmObj* %env$ae51317,%struct.ScmObj* %current_45args57609) {
%stackaddr$env-ref58910 = alloca %struct.ScmObj*, align 8
%lst48102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51317, i64 0)
store %struct.ScmObj* %lst48102, %struct.ScmObj** %stackaddr$env-ref58910
%stackaddr$env-ref58911 = alloca %struct.ScmObj*, align 8
%v48101 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51317, i64 1)
store %struct.ScmObj* %v48101, %struct.ScmObj** %stackaddr$env-ref58911
%stackaddr$env-ref58912 = alloca %struct.ScmObj*, align 8
%k48416 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51317, i64 2)
store %struct.ScmObj* %k48416, %struct.ScmObj** %stackaddr$env-ref58912
%stackaddr$prim58913 = alloca %struct.ScmObj*, align 8
%_95k48417 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57609)
store volatile %struct.ScmObj* %_95k48417, %struct.ScmObj** %stackaddr$prim58913, align 8
%stackaddr$prim58914 = alloca %struct.ScmObj*, align 8
%current_45args57610 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57609)
store volatile %struct.ScmObj* %current_45args57610, %struct.ScmObj** %stackaddr$prim58914, align 8
%stackaddr$prim58915 = alloca %struct.ScmObj*, align 8
%anf_45bind48263 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57610)
store volatile %struct.ScmObj* %anf_45bind48263, %struct.ScmObj** %stackaddr$prim58915, align 8
%stackaddr$makeclosure58916 = alloca %struct.ScmObj*, align 8
%fptrToInt58917 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51333 to i64
%ae51333 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58917)
store volatile %struct.ScmObj* %ae51333, %struct.ScmObj** %stackaddr$makeclosure58916, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51333, %struct.ScmObj* %lst48102, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51333, %struct.ScmObj* %v48101, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51333, %struct.ScmObj* %k48416, i64 2)
%stackaddr$makeclosure58918 = alloca %struct.ScmObj*, align 8
%fptrToInt58919 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51334 to i64
%ae51334 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58919)
store volatile %struct.ScmObj* %ae51334, %struct.ScmObj** %stackaddr$makeclosure58918, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51334, %struct.ScmObj* %lst48102, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51334, %struct.ScmObj* %v48101, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51334, %struct.ScmObj* %k48416, i64 2)
%argslist57624$anf_45bind482630 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58920 = alloca %struct.ScmObj*, align 8
%argslist57624$anf_45bind482631 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51334, %struct.ScmObj* %argslist57624$anf_45bind482630)
store volatile %struct.ScmObj* %argslist57624$anf_45bind482631, %struct.ScmObj** %stackaddr$prim58920, align 8
%stackaddr$prim58921 = alloca %struct.ScmObj*, align 8
%argslist57624$anf_45bind482632 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51333, %struct.ScmObj* %argslist57624$anf_45bind482631)
store volatile %struct.ScmObj* %argslist57624$anf_45bind482632, %struct.ScmObj** %stackaddr$prim58921, align 8
%clofunc58922 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48263)
musttail call tailcc void %clofunc58922(%struct.ScmObj* %anf_45bind48263, %struct.ScmObj* %argslist57624$anf_45bind482632)
ret void
}

define tailcc void @proc_clo$ae51333(%struct.ScmObj* %env$ae51333,%struct.ScmObj* %current_45args57612) {
%stackaddr$env-ref58923 = alloca %struct.ScmObj*, align 8
%lst48102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51333, i64 0)
store %struct.ScmObj* %lst48102, %struct.ScmObj** %stackaddr$env-ref58923
%stackaddr$env-ref58924 = alloca %struct.ScmObj*, align 8
%v48101 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51333, i64 1)
store %struct.ScmObj* %v48101, %struct.ScmObj** %stackaddr$env-ref58924
%stackaddr$env-ref58925 = alloca %struct.ScmObj*, align 8
%k48416 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51333, i64 2)
store %struct.ScmObj* %k48416, %struct.ScmObj** %stackaddr$env-ref58925
%stackaddr$prim58926 = alloca %struct.ScmObj*, align 8
%_95k48418 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57612)
store volatile %struct.ScmObj* %_95k48418, %struct.ScmObj** %stackaddr$prim58926, align 8
%stackaddr$prim58927 = alloca %struct.ScmObj*, align 8
%current_45args57613 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57612)
store volatile %struct.ScmObj* %current_45args57613, %struct.ScmObj** %stackaddr$prim58927, align 8
%stackaddr$prim58928 = alloca %struct.ScmObj*, align 8
%cc48103 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57613)
store volatile %struct.ScmObj* %cc48103, %struct.ScmObj** %stackaddr$prim58928, align 8
%ae51442 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58929 = alloca %struct.ScmObj*, align 8
%anf_45bind48264 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48102, %struct.ScmObj* %ae51442)
store volatile %struct.ScmObj* %anf_45bind48264, %struct.ScmObj** %stackaddr$prim58929, align 8
%stackaddr$prim58930 = alloca %struct.ScmObj*, align 8
%anf_45bind48265 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48264)
store volatile %struct.ScmObj* %anf_45bind48265, %struct.ScmObj** %stackaddr$prim58930, align 8
%truthy$cmp58931 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48265)
%cmp$cmp58931 = icmp eq i64 %truthy$cmp58931, 1
br i1 %cmp$cmp58931, label %truebranch$cmp58931, label %falsebranch$cmp58931
truebranch$cmp58931:
%ae51446 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51447 = call %struct.ScmObj* @const_init_false()
%argslist57615$k484160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58932 = alloca %struct.ScmObj*, align 8
%argslist57615$k484161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51447, %struct.ScmObj* %argslist57615$k484160)
store volatile %struct.ScmObj* %argslist57615$k484161, %struct.ScmObj** %stackaddr$prim58932, align 8
%stackaddr$prim58933 = alloca %struct.ScmObj*, align 8
%argslist57615$k484162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51446, %struct.ScmObj* %argslist57615$k484161)
store volatile %struct.ScmObj* %argslist57615$k484162, %struct.ScmObj** %stackaddr$prim58933, align 8
%clofunc58934 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48416)
musttail call tailcc void %clofunc58934(%struct.ScmObj* %k48416, %struct.ScmObj* %argslist57615$k484162)
ret void
falsebranch$cmp58931:
%ae51455 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58935 = alloca %struct.ScmObj*, align 8
%anf_45bind48266 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48102, %struct.ScmObj* %ae51455)
store volatile %struct.ScmObj* %anf_45bind48266, %struct.ScmObj** %stackaddr$prim58935, align 8
%stackaddr$prim58936 = alloca %struct.ScmObj*, align 8
%anf_45bind48267 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48266)
store volatile %struct.ScmObj* %anf_45bind48267, %struct.ScmObj** %stackaddr$prim58936, align 8
%stackaddr$prim58937 = alloca %struct.ScmObj*, align 8
%anf_45bind48268 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind48267, %struct.ScmObj* %v48101)
store volatile %struct.ScmObj* %anf_45bind48268, %struct.ScmObj** %stackaddr$prim58937, align 8
%truthy$cmp58938 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48268)
%cmp$cmp58938 = icmp eq i64 %truthy$cmp58938, 1
br i1 %cmp$cmp58938, label %truebranch$cmp58938, label %falsebranch$cmp58938
truebranch$cmp58938:
%ae51461 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58939 = alloca %struct.ScmObj*, align 8
%cpsprim48419 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48102, %struct.ScmObj* %ae51461)
store volatile %struct.ScmObj* %cpsprim48419, %struct.ScmObj** %stackaddr$prim58939, align 8
%ae51463 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57616$k484160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58940 = alloca %struct.ScmObj*, align 8
%argslist57616$k484161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48419, %struct.ScmObj* %argslist57616$k484160)
store volatile %struct.ScmObj* %argslist57616$k484161, %struct.ScmObj** %stackaddr$prim58940, align 8
%stackaddr$prim58941 = alloca %struct.ScmObj*, align 8
%argslist57616$k484162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51463, %struct.ScmObj* %argslist57616$k484161)
store volatile %struct.ScmObj* %argslist57616$k484162, %struct.ScmObj** %stackaddr$prim58941, align 8
%clofunc58942 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48416)
musttail call tailcc void %clofunc58942(%struct.ScmObj* %k48416, %struct.ScmObj* %argslist57616$k484162)
ret void
falsebranch$cmp58938:
%ae51474 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58943 = alloca %struct.ScmObj*, align 8
%anf_45bind48269 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48102, %struct.ScmObj* %ae51474)
store volatile %struct.ScmObj* %anf_45bind48269, %struct.ScmObj** %stackaddr$prim58943, align 8
%stackaddr$prim58944 = alloca %struct.ScmObj*, align 8
%anf_45bind48270 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48269)
store volatile %struct.ScmObj* %anf_45bind48270, %struct.ScmObj** %stackaddr$prim58944, align 8
%ae51477 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58945 = alloca %struct.ScmObj*, align 8
%_95048105 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48102, %struct.ScmObj* %ae51477, %struct.ScmObj* %anf_45bind48270)
store volatile %struct.ScmObj* %_95048105, %struct.ScmObj** %stackaddr$prim58945, align 8
%argslist57617$cc481030 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58946 = alloca %struct.ScmObj*, align 8
%argslist57617$cc481031 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48103, %struct.ScmObj* %argslist57617$cc481030)
store volatile %struct.ScmObj* %argslist57617$cc481031, %struct.ScmObj** %stackaddr$prim58946, align 8
%stackaddr$prim58947 = alloca %struct.ScmObj*, align 8
%argslist57617$cc481032 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48416, %struct.ScmObj* %argslist57617$cc481031)
store volatile %struct.ScmObj* %argslist57617$cc481032, %struct.ScmObj** %stackaddr$prim58947, align 8
%clofunc58948 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48103)
musttail call tailcc void %clofunc58948(%struct.ScmObj* %cc48103, %struct.ScmObj* %argslist57617$cc481032)
ret void
}

define tailcc void @proc_clo$ae51334(%struct.ScmObj* %env$ae51334,%struct.ScmObj* %current_45args57618) {
%stackaddr$env-ref58949 = alloca %struct.ScmObj*, align 8
%lst48102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51334, i64 0)
store %struct.ScmObj* %lst48102, %struct.ScmObj** %stackaddr$env-ref58949
%stackaddr$env-ref58950 = alloca %struct.ScmObj*, align 8
%v48101 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51334, i64 1)
store %struct.ScmObj* %v48101, %struct.ScmObj** %stackaddr$env-ref58950
%stackaddr$env-ref58951 = alloca %struct.ScmObj*, align 8
%k48416 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51334, i64 2)
store %struct.ScmObj* %k48416, %struct.ScmObj** %stackaddr$env-ref58951
%stackaddr$prim58952 = alloca %struct.ScmObj*, align 8
%_95k48418 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57618)
store volatile %struct.ScmObj* %_95k48418, %struct.ScmObj** %stackaddr$prim58952, align 8
%stackaddr$prim58953 = alloca %struct.ScmObj*, align 8
%current_45args57619 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57618)
store volatile %struct.ScmObj* %current_45args57619, %struct.ScmObj** %stackaddr$prim58953, align 8
%stackaddr$prim58954 = alloca %struct.ScmObj*, align 8
%cc48103 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57619)
store volatile %struct.ScmObj* %cc48103, %struct.ScmObj** %stackaddr$prim58954, align 8
%ae51336 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58955 = alloca %struct.ScmObj*, align 8
%anf_45bind48264 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48102, %struct.ScmObj* %ae51336)
store volatile %struct.ScmObj* %anf_45bind48264, %struct.ScmObj** %stackaddr$prim58955, align 8
%stackaddr$prim58956 = alloca %struct.ScmObj*, align 8
%anf_45bind48265 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48264)
store volatile %struct.ScmObj* %anf_45bind48265, %struct.ScmObj** %stackaddr$prim58956, align 8
%truthy$cmp58957 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48265)
%cmp$cmp58957 = icmp eq i64 %truthy$cmp58957, 1
br i1 %cmp$cmp58957, label %truebranch$cmp58957, label %falsebranch$cmp58957
truebranch$cmp58957:
%ae51340 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51341 = call %struct.ScmObj* @const_init_false()
%argslist57621$k484160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58958 = alloca %struct.ScmObj*, align 8
%argslist57621$k484161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51341, %struct.ScmObj* %argslist57621$k484160)
store volatile %struct.ScmObj* %argslist57621$k484161, %struct.ScmObj** %stackaddr$prim58958, align 8
%stackaddr$prim58959 = alloca %struct.ScmObj*, align 8
%argslist57621$k484162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51340, %struct.ScmObj* %argslist57621$k484161)
store volatile %struct.ScmObj* %argslist57621$k484162, %struct.ScmObj** %stackaddr$prim58959, align 8
%clofunc58960 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48416)
musttail call tailcc void %clofunc58960(%struct.ScmObj* %k48416, %struct.ScmObj* %argslist57621$k484162)
ret void
falsebranch$cmp58957:
%ae51349 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58961 = alloca %struct.ScmObj*, align 8
%anf_45bind48266 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48102, %struct.ScmObj* %ae51349)
store volatile %struct.ScmObj* %anf_45bind48266, %struct.ScmObj** %stackaddr$prim58961, align 8
%stackaddr$prim58962 = alloca %struct.ScmObj*, align 8
%anf_45bind48267 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48266)
store volatile %struct.ScmObj* %anf_45bind48267, %struct.ScmObj** %stackaddr$prim58962, align 8
%stackaddr$prim58963 = alloca %struct.ScmObj*, align 8
%anf_45bind48268 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind48267, %struct.ScmObj* %v48101)
store volatile %struct.ScmObj* %anf_45bind48268, %struct.ScmObj** %stackaddr$prim58963, align 8
%truthy$cmp58964 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48268)
%cmp$cmp58964 = icmp eq i64 %truthy$cmp58964, 1
br i1 %cmp$cmp58964, label %truebranch$cmp58964, label %falsebranch$cmp58964
truebranch$cmp58964:
%ae51355 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58965 = alloca %struct.ScmObj*, align 8
%cpsprim48419 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48102, %struct.ScmObj* %ae51355)
store volatile %struct.ScmObj* %cpsprim48419, %struct.ScmObj** %stackaddr$prim58965, align 8
%ae51357 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57622$k484160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58966 = alloca %struct.ScmObj*, align 8
%argslist57622$k484161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48419, %struct.ScmObj* %argslist57622$k484160)
store volatile %struct.ScmObj* %argslist57622$k484161, %struct.ScmObj** %stackaddr$prim58966, align 8
%stackaddr$prim58967 = alloca %struct.ScmObj*, align 8
%argslist57622$k484162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51357, %struct.ScmObj* %argslist57622$k484161)
store volatile %struct.ScmObj* %argslist57622$k484162, %struct.ScmObj** %stackaddr$prim58967, align 8
%clofunc58968 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48416)
musttail call tailcc void %clofunc58968(%struct.ScmObj* %k48416, %struct.ScmObj* %argslist57622$k484162)
ret void
falsebranch$cmp58964:
%ae51368 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58969 = alloca %struct.ScmObj*, align 8
%anf_45bind48269 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48102, %struct.ScmObj* %ae51368)
store volatile %struct.ScmObj* %anf_45bind48269, %struct.ScmObj** %stackaddr$prim58969, align 8
%stackaddr$prim58970 = alloca %struct.ScmObj*, align 8
%anf_45bind48270 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48269)
store volatile %struct.ScmObj* %anf_45bind48270, %struct.ScmObj** %stackaddr$prim58970, align 8
%ae51371 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58971 = alloca %struct.ScmObj*, align 8
%_95048105 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48102, %struct.ScmObj* %ae51371, %struct.ScmObj* %anf_45bind48270)
store volatile %struct.ScmObj* %_95048105, %struct.ScmObj** %stackaddr$prim58971, align 8
%argslist57623$cc481030 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58972 = alloca %struct.ScmObj*, align 8
%argslist57623$cc481031 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48103, %struct.ScmObj* %argslist57623$cc481030)
store volatile %struct.ScmObj* %argslist57623$cc481031, %struct.ScmObj** %stackaddr$prim58972, align 8
%stackaddr$prim58973 = alloca %struct.ScmObj*, align 8
%argslist57623$cc481032 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48416, %struct.ScmObj* %argslist57623$cc481031)
store volatile %struct.ScmObj* %argslist57623$cc481032, %struct.ScmObj** %stackaddr$prim58973, align 8
%clofunc58974 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48103)
musttail call tailcc void %clofunc58974(%struct.ScmObj* %cc48103, %struct.ScmObj* %argslist57623$cc481032)
ret void
}

define tailcc void @proc_clo$ae51319(%struct.ScmObj* %env$ae51319,%struct.ScmObj* %current_45args57625) {
%stackaddr$prim58975 = alloca %struct.ScmObj*, align 8
%k48420 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57625)
store volatile %struct.ScmObj* %k48420, %struct.ScmObj** %stackaddr$prim58975, align 8
%stackaddr$prim58976 = alloca %struct.ScmObj*, align 8
%current_45args57626 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57625)
store volatile %struct.ScmObj* %current_45args57626, %struct.ScmObj** %stackaddr$prim58976, align 8
%stackaddr$prim58977 = alloca %struct.ScmObj*, align 8
%u48104 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57626)
store volatile %struct.ScmObj* %u48104, %struct.ScmObj** %stackaddr$prim58977, align 8
%argslist57628$u481040 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58978 = alloca %struct.ScmObj*, align 8
%argslist57628$u481041 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u48104, %struct.ScmObj* %argslist57628$u481040)
store volatile %struct.ScmObj* %argslist57628$u481041, %struct.ScmObj** %stackaddr$prim58978, align 8
%stackaddr$prim58979 = alloca %struct.ScmObj*, align 8
%argslist57628$u481042 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48420, %struct.ScmObj* %argslist57628$u481041)
store volatile %struct.ScmObj* %argslist57628$u481042, %struct.ScmObj** %stackaddr$prim58979, align 8
%clofunc58980 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u48104)
musttail call tailcc void %clofunc58980(%struct.ScmObj* %u48104, %struct.ScmObj* %argslist57628$u481042)
ret void
}

define tailcc void @proc_clo$ae50778(%struct.ScmObj* %env$ae50778,%struct.ScmObj* %current_45args57631) {
%stackaddr$prim58981 = alloca %struct.ScmObj*, align 8
%k48421 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57631)
store volatile %struct.ScmObj* %k48421, %struct.ScmObj** %stackaddr$prim58981, align 8
%stackaddr$prim58982 = alloca %struct.ScmObj*, align 8
%current_45args57632 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57631)
store volatile %struct.ScmObj* %current_45args57632, %struct.ScmObj** %stackaddr$prim58982, align 8
%stackaddr$prim58983 = alloca %struct.ScmObj*, align 8
%lst48108 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57632)
store volatile %struct.ScmObj* %lst48108, %struct.ScmObj** %stackaddr$prim58983, align 8
%stackaddr$prim58984 = alloca %struct.ScmObj*, align 8
%current_45args57633 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57632)
store volatile %struct.ScmObj* %current_45args57633, %struct.ScmObj** %stackaddr$prim58984, align 8
%stackaddr$prim58985 = alloca %struct.ScmObj*, align 8
%n48107 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57633)
store volatile %struct.ScmObj* %n48107, %struct.ScmObj** %stackaddr$prim58985, align 8
%ae50779 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim58986 = alloca %struct.ScmObj*, align 8
%n48110 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50779, %struct.ScmObj* %n48107)
store volatile %struct.ScmObj* %n48110, %struct.ScmObj** %stackaddr$prim58986, align 8
%ae50781 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim58987 = alloca %struct.ScmObj*, align 8
%lst48109 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50781, %struct.ScmObj* %lst48108)
store volatile %struct.ScmObj* %lst48109, %struct.ScmObj** %stackaddr$prim58987, align 8
%stackaddr$makeclosure58988 = alloca %struct.ScmObj*, align 8
%fptrToInt58989 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50783 to i64
%ae50783 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58989)
store volatile %struct.ScmObj* %ae50783, %struct.ScmObj** %stackaddr$makeclosure58988, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50783, %struct.ScmObj* %k48421, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50783, %struct.ScmObj* %n48110, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50783, %struct.ScmObj* %lst48109, i64 2)
%ae50784 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58990 = alloca %struct.ScmObj*, align 8
%fptrToInt58991 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50785 to i64
%ae50785 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58991)
store volatile %struct.ScmObj* %ae50785, %struct.ScmObj** %stackaddr$makeclosure58990, align 8
%argslist57653$ae507830 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58992 = alloca %struct.ScmObj*, align 8
%argslist57653$ae507831 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50785, %struct.ScmObj* %argslist57653$ae507830)
store volatile %struct.ScmObj* %argslist57653$ae507831, %struct.ScmObj** %stackaddr$prim58992, align 8
%stackaddr$prim58993 = alloca %struct.ScmObj*, align 8
%argslist57653$ae507832 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50784, %struct.ScmObj* %argslist57653$ae507831)
store volatile %struct.ScmObj* %argslist57653$ae507832, %struct.ScmObj** %stackaddr$prim58993, align 8
%clofunc58994 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50783)
musttail call tailcc void %clofunc58994(%struct.ScmObj* %ae50783, %struct.ScmObj* %argslist57653$ae507832)
ret void
}

define tailcc void @proc_clo$ae50783(%struct.ScmObj* %env$ae50783,%struct.ScmObj* %current_45args57635) {
%stackaddr$env-ref58995 = alloca %struct.ScmObj*, align 8
%k48421 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50783, i64 0)
store %struct.ScmObj* %k48421, %struct.ScmObj** %stackaddr$env-ref58995
%stackaddr$env-ref58996 = alloca %struct.ScmObj*, align 8
%n48110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50783, i64 1)
store %struct.ScmObj* %n48110, %struct.ScmObj** %stackaddr$env-ref58996
%stackaddr$env-ref58997 = alloca %struct.ScmObj*, align 8
%lst48109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50783, i64 2)
store %struct.ScmObj* %lst48109, %struct.ScmObj** %stackaddr$env-ref58997
%stackaddr$prim58998 = alloca %struct.ScmObj*, align 8
%_95k48422 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57635)
store volatile %struct.ScmObj* %_95k48422, %struct.ScmObj** %stackaddr$prim58998, align 8
%stackaddr$prim58999 = alloca %struct.ScmObj*, align 8
%current_45args57636 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57635)
store volatile %struct.ScmObj* %current_45args57636, %struct.ScmObj** %stackaddr$prim58999, align 8
%stackaddr$prim59000 = alloca %struct.ScmObj*, align 8
%anf_45bind48256 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57636)
store volatile %struct.ScmObj* %anf_45bind48256, %struct.ScmObj** %stackaddr$prim59000, align 8
%stackaddr$makeclosure59001 = alloca %struct.ScmObj*, align 8
%fptrToInt59002 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50799 to i64
%ae50799 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt59002)
store volatile %struct.ScmObj* %ae50799, %struct.ScmObj** %stackaddr$makeclosure59001, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50799, %struct.ScmObj* %k48421, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50799, %struct.ScmObj* %n48110, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50799, %struct.ScmObj* %lst48109, i64 2)
%stackaddr$makeclosure59003 = alloca %struct.ScmObj*, align 8
%fptrToInt59004 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50800 to i64
%ae50800 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt59004)
store volatile %struct.ScmObj* %ae50800, %struct.ScmObj** %stackaddr$makeclosure59003, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50800, %struct.ScmObj* %k48421, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50800, %struct.ScmObj* %n48110, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50800, %struct.ScmObj* %lst48109, i64 2)
%argslist57648$anf_45bind482560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59005 = alloca %struct.ScmObj*, align 8
%argslist57648$anf_45bind482561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50800, %struct.ScmObj* %argslist57648$anf_45bind482560)
store volatile %struct.ScmObj* %argslist57648$anf_45bind482561, %struct.ScmObj** %stackaddr$prim59005, align 8
%stackaddr$prim59006 = alloca %struct.ScmObj*, align 8
%argslist57648$anf_45bind482562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50799, %struct.ScmObj* %argslist57648$anf_45bind482561)
store volatile %struct.ScmObj* %argslist57648$anf_45bind482562, %struct.ScmObj** %stackaddr$prim59006, align 8
%clofunc59007 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48256)
musttail call tailcc void %clofunc59007(%struct.ScmObj* %anf_45bind48256, %struct.ScmObj* %argslist57648$anf_45bind482562)
ret void
}

define tailcc void @proc_clo$ae50799(%struct.ScmObj* %env$ae50799,%struct.ScmObj* %current_45args57638) {
%stackaddr$env-ref59008 = alloca %struct.ScmObj*, align 8
%k48421 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50799, i64 0)
store %struct.ScmObj* %k48421, %struct.ScmObj** %stackaddr$env-ref59008
%stackaddr$env-ref59009 = alloca %struct.ScmObj*, align 8
%n48110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50799, i64 1)
store %struct.ScmObj* %n48110, %struct.ScmObj** %stackaddr$env-ref59009
%stackaddr$env-ref59010 = alloca %struct.ScmObj*, align 8
%lst48109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50799, i64 2)
store %struct.ScmObj* %lst48109, %struct.ScmObj** %stackaddr$env-ref59010
%stackaddr$prim59011 = alloca %struct.ScmObj*, align 8
%_95k48423 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57638)
store volatile %struct.ScmObj* %_95k48423, %struct.ScmObj** %stackaddr$prim59011, align 8
%stackaddr$prim59012 = alloca %struct.ScmObj*, align 8
%current_45args57639 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57638)
store volatile %struct.ScmObj* %current_45args57639, %struct.ScmObj** %stackaddr$prim59012, align 8
%stackaddr$prim59013 = alloca %struct.ScmObj*, align 8
%cc48111 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57639)
store volatile %struct.ScmObj* %cc48111, %struct.ScmObj** %stackaddr$prim59013, align 8
%ae50942 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59014 = alloca %struct.ScmObj*, align 8
%anf_45bind48257 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48110, %struct.ScmObj* %ae50942)
store volatile %struct.ScmObj* %anf_45bind48257, %struct.ScmObj** %stackaddr$prim59014, align 8
%ae50943 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59015 = alloca %struct.ScmObj*, align 8
%anf_45bind48258 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae50943, %struct.ScmObj* %anf_45bind48257)
store volatile %struct.ScmObj* %anf_45bind48258, %struct.ScmObj** %stackaddr$prim59015, align 8
%truthy$cmp59016 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48258)
%cmp$cmp59016 = icmp eq i64 %truthy$cmp59016, 1
br i1 %cmp$cmp59016, label %truebranch$cmp59016, label %falsebranch$cmp59016
truebranch$cmp59016:
%ae50947 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59017 = alloca %struct.ScmObj*, align 8
%cpsprim48424 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48109, %struct.ScmObj* %ae50947)
store volatile %struct.ScmObj* %cpsprim48424, %struct.ScmObj** %stackaddr$prim59017, align 8
%ae50949 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57641$k484210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59018 = alloca %struct.ScmObj*, align 8
%argslist57641$k484211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48424, %struct.ScmObj* %argslist57641$k484210)
store volatile %struct.ScmObj* %argslist57641$k484211, %struct.ScmObj** %stackaddr$prim59018, align 8
%stackaddr$prim59019 = alloca %struct.ScmObj*, align 8
%argslist57641$k484212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50949, %struct.ScmObj* %argslist57641$k484211)
store volatile %struct.ScmObj* %argslist57641$k484212, %struct.ScmObj** %stackaddr$prim59019, align 8
%clofunc59020 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48421)
musttail call tailcc void %clofunc59020(%struct.ScmObj* %k48421, %struct.ScmObj* %argslist57641$k484212)
ret void
falsebranch$cmp59016:
%ae50960 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59021 = alloca %struct.ScmObj*, align 8
%anf_45bind48259 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48109, %struct.ScmObj* %ae50960)
store volatile %struct.ScmObj* %anf_45bind48259, %struct.ScmObj** %stackaddr$prim59021, align 8
%stackaddr$prim59022 = alloca %struct.ScmObj*, align 8
%anf_45bind48260 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48259)
store volatile %struct.ScmObj* %anf_45bind48260, %struct.ScmObj** %stackaddr$prim59022, align 8
%ae50963 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59023 = alloca %struct.ScmObj*, align 8
%_95048114 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48109, %struct.ScmObj* %ae50963, %struct.ScmObj* %anf_45bind48260)
store volatile %struct.ScmObj* %_95048114, %struct.ScmObj** %stackaddr$prim59023, align 8
%ae50966 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59024 = alloca %struct.ScmObj*, align 8
%anf_45bind48261 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48110, %struct.ScmObj* %ae50966)
store volatile %struct.ScmObj* %anf_45bind48261, %struct.ScmObj** %stackaddr$prim59024, align 8
%ae50968 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim59025 = alloca %struct.ScmObj*, align 8
%anf_45bind48262 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind48261, %struct.ScmObj* %ae50968)
store volatile %struct.ScmObj* %anf_45bind48262, %struct.ScmObj** %stackaddr$prim59025, align 8
%ae50970 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59026 = alloca %struct.ScmObj*, align 8
%_95148113 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n48110, %struct.ScmObj* %ae50970, %struct.ScmObj* %anf_45bind48262)
store volatile %struct.ScmObj* %_95148113, %struct.ScmObj** %stackaddr$prim59026, align 8
%argslist57642$cc481110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59027 = alloca %struct.ScmObj*, align 8
%argslist57642$cc481111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48111, %struct.ScmObj* %argslist57642$cc481110)
store volatile %struct.ScmObj* %argslist57642$cc481111, %struct.ScmObj** %stackaddr$prim59027, align 8
%stackaddr$prim59028 = alloca %struct.ScmObj*, align 8
%argslist57642$cc481112 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48421, %struct.ScmObj* %argslist57642$cc481111)
store volatile %struct.ScmObj* %argslist57642$cc481112, %struct.ScmObj** %stackaddr$prim59028, align 8
%clofunc59029 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48111)
musttail call tailcc void %clofunc59029(%struct.ScmObj* %cc48111, %struct.ScmObj* %argslist57642$cc481112)
ret void
}

define tailcc void @proc_clo$ae50800(%struct.ScmObj* %env$ae50800,%struct.ScmObj* %current_45args57643) {
%stackaddr$env-ref59030 = alloca %struct.ScmObj*, align 8
%k48421 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50800, i64 0)
store %struct.ScmObj* %k48421, %struct.ScmObj** %stackaddr$env-ref59030
%stackaddr$env-ref59031 = alloca %struct.ScmObj*, align 8
%n48110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50800, i64 1)
store %struct.ScmObj* %n48110, %struct.ScmObj** %stackaddr$env-ref59031
%stackaddr$env-ref59032 = alloca %struct.ScmObj*, align 8
%lst48109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50800, i64 2)
store %struct.ScmObj* %lst48109, %struct.ScmObj** %stackaddr$env-ref59032
%stackaddr$prim59033 = alloca %struct.ScmObj*, align 8
%_95k48423 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57643)
store volatile %struct.ScmObj* %_95k48423, %struct.ScmObj** %stackaddr$prim59033, align 8
%stackaddr$prim59034 = alloca %struct.ScmObj*, align 8
%current_45args57644 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57643)
store volatile %struct.ScmObj* %current_45args57644, %struct.ScmObj** %stackaddr$prim59034, align 8
%stackaddr$prim59035 = alloca %struct.ScmObj*, align 8
%cc48111 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57644)
store volatile %struct.ScmObj* %cc48111, %struct.ScmObj** %stackaddr$prim59035, align 8
%ae50802 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59036 = alloca %struct.ScmObj*, align 8
%anf_45bind48257 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48110, %struct.ScmObj* %ae50802)
store volatile %struct.ScmObj* %anf_45bind48257, %struct.ScmObj** %stackaddr$prim59036, align 8
%ae50803 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59037 = alloca %struct.ScmObj*, align 8
%anf_45bind48258 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae50803, %struct.ScmObj* %anf_45bind48257)
store volatile %struct.ScmObj* %anf_45bind48258, %struct.ScmObj** %stackaddr$prim59037, align 8
%truthy$cmp59038 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48258)
%cmp$cmp59038 = icmp eq i64 %truthy$cmp59038, 1
br i1 %cmp$cmp59038, label %truebranch$cmp59038, label %falsebranch$cmp59038
truebranch$cmp59038:
%ae50807 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59039 = alloca %struct.ScmObj*, align 8
%cpsprim48424 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48109, %struct.ScmObj* %ae50807)
store volatile %struct.ScmObj* %cpsprim48424, %struct.ScmObj** %stackaddr$prim59039, align 8
%ae50809 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57646$k484210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59040 = alloca %struct.ScmObj*, align 8
%argslist57646$k484211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48424, %struct.ScmObj* %argslist57646$k484210)
store volatile %struct.ScmObj* %argslist57646$k484211, %struct.ScmObj** %stackaddr$prim59040, align 8
%stackaddr$prim59041 = alloca %struct.ScmObj*, align 8
%argslist57646$k484212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50809, %struct.ScmObj* %argslist57646$k484211)
store volatile %struct.ScmObj* %argslist57646$k484212, %struct.ScmObj** %stackaddr$prim59041, align 8
%clofunc59042 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48421)
musttail call tailcc void %clofunc59042(%struct.ScmObj* %k48421, %struct.ScmObj* %argslist57646$k484212)
ret void
falsebranch$cmp59038:
%ae50820 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59043 = alloca %struct.ScmObj*, align 8
%anf_45bind48259 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48109, %struct.ScmObj* %ae50820)
store volatile %struct.ScmObj* %anf_45bind48259, %struct.ScmObj** %stackaddr$prim59043, align 8
%stackaddr$prim59044 = alloca %struct.ScmObj*, align 8
%anf_45bind48260 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48259)
store volatile %struct.ScmObj* %anf_45bind48260, %struct.ScmObj** %stackaddr$prim59044, align 8
%ae50823 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59045 = alloca %struct.ScmObj*, align 8
%_95048114 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48109, %struct.ScmObj* %ae50823, %struct.ScmObj* %anf_45bind48260)
store volatile %struct.ScmObj* %_95048114, %struct.ScmObj** %stackaddr$prim59045, align 8
%ae50826 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59046 = alloca %struct.ScmObj*, align 8
%anf_45bind48261 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48110, %struct.ScmObj* %ae50826)
store volatile %struct.ScmObj* %anf_45bind48261, %struct.ScmObj** %stackaddr$prim59046, align 8
%ae50828 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim59047 = alloca %struct.ScmObj*, align 8
%anf_45bind48262 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind48261, %struct.ScmObj* %ae50828)
store volatile %struct.ScmObj* %anf_45bind48262, %struct.ScmObj** %stackaddr$prim59047, align 8
%ae50830 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59048 = alloca %struct.ScmObj*, align 8
%_95148113 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n48110, %struct.ScmObj* %ae50830, %struct.ScmObj* %anf_45bind48262)
store volatile %struct.ScmObj* %_95148113, %struct.ScmObj** %stackaddr$prim59048, align 8
%argslist57647$cc481110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59049 = alloca %struct.ScmObj*, align 8
%argslist57647$cc481111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48111, %struct.ScmObj* %argslist57647$cc481110)
store volatile %struct.ScmObj* %argslist57647$cc481111, %struct.ScmObj** %stackaddr$prim59049, align 8
%stackaddr$prim59050 = alloca %struct.ScmObj*, align 8
%argslist57647$cc481112 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48421, %struct.ScmObj* %argslist57647$cc481111)
store volatile %struct.ScmObj* %argslist57647$cc481112, %struct.ScmObj** %stackaddr$prim59050, align 8
%clofunc59051 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48111)
musttail call tailcc void %clofunc59051(%struct.ScmObj* %cc48111, %struct.ScmObj* %argslist57647$cc481112)
ret void
}

define tailcc void @proc_clo$ae50785(%struct.ScmObj* %env$ae50785,%struct.ScmObj* %current_45args57649) {
%stackaddr$prim59052 = alloca %struct.ScmObj*, align 8
%k48425 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57649)
store volatile %struct.ScmObj* %k48425, %struct.ScmObj** %stackaddr$prim59052, align 8
%stackaddr$prim59053 = alloca %struct.ScmObj*, align 8
%current_45args57650 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57649)
store volatile %struct.ScmObj* %current_45args57650, %struct.ScmObj** %stackaddr$prim59053, align 8
%stackaddr$prim59054 = alloca %struct.ScmObj*, align 8
%u48112 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57650)
store volatile %struct.ScmObj* %u48112, %struct.ScmObj** %stackaddr$prim59054, align 8
%argslist57652$u481120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59055 = alloca %struct.ScmObj*, align 8
%argslist57652$u481121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u48112, %struct.ScmObj* %argslist57652$u481120)
store volatile %struct.ScmObj* %argslist57652$u481121, %struct.ScmObj** %stackaddr$prim59055, align 8
%stackaddr$prim59056 = alloca %struct.ScmObj*, align 8
%argslist57652$u481122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48425, %struct.ScmObj* %argslist57652$u481121)
store volatile %struct.ScmObj* %argslist57652$u481122, %struct.ScmObj** %stackaddr$prim59056, align 8
%clofunc59057 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u48112)
musttail call tailcc void %clofunc59057(%struct.ScmObj* %u48112, %struct.ScmObj* %argslist57652$u481122)
ret void
}

define tailcc void @proc_clo$ae50362(%struct.ScmObj* %env$ae50362,%struct.ScmObj* %current_45args57655) {
%stackaddr$prim59058 = alloca %struct.ScmObj*, align 8
%k48426 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57655)
store volatile %struct.ScmObj* %k48426, %struct.ScmObj** %stackaddr$prim59058, align 8
%stackaddr$prim59059 = alloca %struct.ScmObj*, align 8
%current_45args57656 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57655)
store volatile %struct.ScmObj* %current_45args57656, %struct.ScmObj** %stackaddr$prim59059, align 8
%stackaddr$prim59060 = alloca %struct.ScmObj*, align 8
%a48116 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57656)
store volatile %struct.ScmObj* %a48116, %struct.ScmObj** %stackaddr$prim59060, align 8
%ae50363 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim59061 = alloca %struct.ScmObj*, align 8
%a48117 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50363, %struct.ScmObj* %a48116)
store volatile %struct.ScmObj* %a48117, %struct.ScmObj** %stackaddr$prim59061, align 8
%stackaddr$makeclosure59062 = alloca %struct.ScmObj*, align 8
%fptrToInt59063 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50365 to i64
%ae50365 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt59063)
store volatile %struct.ScmObj* %ae50365, %struct.ScmObj** %stackaddr$makeclosure59062, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50365, %struct.ScmObj* %a48117, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50365, %struct.ScmObj* %k48426, i64 1)
%ae50366 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59064 = alloca %struct.ScmObj*, align 8
%fptrToInt59065 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50367 to i64
%ae50367 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt59065)
store volatile %struct.ScmObj* %ae50367, %struct.ScmObj** %stackaddr$makeclosure59064, align 8
%argslist57678$ae503650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59066 = alloca %struct.ScmObj*, align 8
%argslist57678$ae503651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50367, %struct.ScmObj* %argslist57678$ae503650)
store volatile %struct.ScmObj* %argslist57678$ae503651, %struct.ScmObj** %stackaddr$prim59066, align 8
%stackaddr$prim59067 = alloca %struct.ScmObj*, align 8
%argslist57678$ae503652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50366, %struct.ScmObj* %argslist57678$ae503651)
store volatile %struct.ScmObj* %argslist57678$ae503652, %struct.ScmObj** %stackaddr$prim59067, align 8
%clofunc59068 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50365)
musttail call tailcc void %clofunc59068(%struct.ScmObj* %ae50365, %struct.ScmObj* %argslist57678$ae503652)
ret void
}

define tailcc void @proc_clo$ae50365(%struct.ScmObj* %env$ae50365,%struct.ScmObj* %current_45args57658) {
%stackaddr$env-ref59069 = alloca %struct.ScmObj*, align 8
%a48117 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50365, i64 0)
store %struct.ScmObj* %a48117, %struct.ScmObj** %stackaddr$env-ref59069
%stackaddr$env-ref59070 = alloca %struct.ScmObj*, align 8
%k48426 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50365, i64 1)
store %struct.ScmObj* %k48426, %struct.ScmObj** %stackaddr$env-ref59070
%stackaddr$prim59071 = alloca %struct.ScmObj*, align 8
%_95k48427 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57658)
store volatile %struct.ScmObj* %_95k48427, %struct.ScmObj** %stackaddr$prim59071, align 8
%stackaddr$prim59072 = alloca %struct.ScmObj*, align 8
%current_45args57659 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57658)
store volatile %struct.ScmObj* %current_45args57659, %struct.ScmObj** %stackaddr$prim59072, align 8
%stackaddr$prim59073 = alloca %struct.ScmObj*, align 8
%anf_45bind48248 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57659)
store volatile %struct.ScmObj* %anf_45bind48248, %struct.ScmObj** %stackaddr$prim59073, align 8
%stackaddr$makeclosure59074 = alloca %struct.ScmObj*, align 8
%fptrToInt59075 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50384 to i64
%ae50384 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt59075)
store volatile %struct.ScmObj* %ae50384, %struct.ScmObj** %stackaddr$makeclosure59074, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50384, %struct.ScmObj* %a48117, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50384, %struct.ScmObj* %k48426, i64 1)
%stackaddr$makeclosure59076 = alloca %struct.ScmObj*, align 8
%fptrToInt59077 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50385 to i64
%ae50385 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt59077)
store volatile %struct.ScmObj* %ae50385, %struct.ScmObj** %stackaddr$makeclosure59076, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50385, %struct.ScmObj* %a48117, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50385, %struct.ScmObj* %k48426, i64 1)
%argslist57673$anf_45bind482480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59078 = alloca %struct.ScmObj*, align 8
%argslist57673$anf_45bind482481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50385, %struct.ScmObj* %argslist57673$anf_45bind482480)
store volatile %struct.ScmObj* %argslist57673$anf_45bind482481, %struct.ScmObj** %stackaddr$prim59078, align 8
%stackaddr$prim59079 = alloca %struct.ScmObj*, align 8
%argslist57673$anf_45bind482482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50384, %struct.ScmObj* %argslist57673$anf_45bind482481)
store volatile %struct.ScmObj* %argslist57673$anf_45bind482482, %struct.ScmObj** %stackaddr$prim59079, align 8
%clofunc59080 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48248)
musttail call tailcc void %clofunc59080(%struct.ScmObj* %anf_45bind48248, %struct.ScmObj* %argslist57673$anf_45bind482482)
ret void
}

define tailcc void @proc_clo$ae50384(%struct.ScmObj* %env$ae50384,%struct.ScmObj* %current_45args57661) {
%stackaddr$env-ref59081 = alloca %struct.ScmObj*, align 8
%a48117 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50384, i64 0)
store %struct.ScmObj* %a48117, %struct.ScmObj** %stackaddr$env-ref59081
%stackaddr$env-ref59082 = alloca %struct.ScmObj*, align 8
%k48426 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50384, i64 1)
store %struct.ScmObj* %k48426, %struct.ScmObj** %stackaddr$env-ref59082
%stackaddr$prim59083 = alloca %struct.ScmObj*, align 8
%_95k48428 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57661)
store volatile %struct.ScmObj* %_95k48428, %struct.ScmObj** %stackaddr$prim59083, align 8
%stackaddr$prim59084 = alloca %struct.ScmObj*, align 8
%current_45args57662 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57661)
store volatile %struct.ScmObj* %current_45args57662, %struct.ScmObj** %stackaddr$prim59084, align 8
%stackaddr$prim59085 = alloca %struct.ScmObj*, align 8
%cc48118 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57662)
store volatile %struct.ScmObj* %cc48118, %struct.ScmObj** %stackaddr$prim59085, align 8
%ae50500 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59086 = alloca %struct.ScmObj*, align 8
%anf_45bind48249 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48117, %struct.ScmObj* %ae50500)
store volatile %struct.ScmObj* %anf_45bind48249, %struct.ScmObj** %stackaddr$prim59086, align 8
%stackaddr$prim59087 = alloca %struct.ScmObj*, align 8
%anf_45bind48250 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48249)
store volatile %struct.ScmObj* %anf_45bind48250, %struct.ScmObj** %stackaddr$prim59087, align 8
%truthy$cmp59088 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48250)
%cmp$cmp59088 = icmp eq i64 %truthy$cmp59088, 1
br i1 %cmp$cmp59088, label %truebranch$cmp59088, label %falsebranch$cmp59088
truebranch$cmp59088:
%ae50504 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50505 = call %struct.ScmObj* @const_init_true()
%argslist57664$k484260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59089 = alloca %struct.ScmObj*, align 8
%argslist57664$k484261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50505, %struct.ScmObj* %argslist57664$k484260)
store volatile %struct.ScmObj* %argslist57664$k484261, %struct.ScmObj** %stackaddr$prim59089, align 8
%stackaddr$prim59090 = alloca %struct.ScmObj*, align 8
%argslist57664$k484262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50504, %struct.ScmObj* %argslist57664$k484261)
store volatile %struct.ScmObj* %argslist57664$k484262, %struct.ScmObj** %stackaddr$prim59090, align 8
%clofunc59091 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48426)
musttail call tailcc void %clofunc59091(%struct.ScmObj* %k48426, %struct.ScmObj* %argslist57664$k484262)
ret void
falsebranch$cmp59088:
%ae50513 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59092 = alloca %struct.ScmObj*, align 8
%anf_45bind48251 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48117, %struct.ScmObj* %ae50513)
store volatile %struct.ScmObj* %anf_45bind48251, %struct.ScmObj** %stackaddr$prim59092, align 8
%stackaddr$prim59093 = alloca %struct.ScmObj*, align 8
%anf_45bind48252 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind48251)
store volatile %struct.ScmObj* %anf_45bind48252, %struct.ScmObj** %stackaddr$prim59093, align 8
%truthy$cmp59094 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48252)
%cmp$cmp59094 = icmp eq i64 %truthy$cmp59094, 1
br i1 %cmp$cmp59094, label %truebranch$cmp59094, label %falsebranch$cmp59094
truebranch$cmp59094:
%ae50517 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59095 = alloca %struct.ScmObj*, align 8
%anf_45bind48253 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48117, %struct.ScmObj* %ae50517)
store volatile %struct.ScmObj* %anf_45bind48253, %struct.ScmObj** %stackaddr$prim59095, align 8
%stackaddr$prim59096 = alloca %struct.ScmObj*, align 8
%b48120 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48253)
store volatile %struct.ScmObj* %b48120, %struct.ScmObj** %stackaddr$prim59096, align 8
%ae50520 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59097 = alloca %struct.ScmObj*, align 8
%anf_45bind48254 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48117, %struct.ScmObj* %ae50520)
store volatile %struct.ScmObj* %anf_45bind48254, %struct.ScmObj** %stackaddr$prim59097, align 8
%stackaddr$prim59098 = alloca %struct.ScmObj*, align 8
%anf_45bind48255 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48254)
store volatile %struct.ScmObj* %anf_45bind48255, %struct.ScmObj** %stackaddr$prim59098, align 8
%ae50523 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59099 = alloca %struct.ScmObj*, align 8
%_95048121 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a48117, %struct.ScmObj* %ae50523, %struct.ScmObj* %anf_45bind48255)
store volatile %struct.ScmObj* %_95048121, %struct.ScmObj** %stackaddr$prim59099, align 8
%argslist57665$cc481180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59100 = alloca %struct.ScmObj*, align 8
%argslist57665$cc481181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48118, %struct.ScmObj* %argslist57665$cc481180)
store volatile %struct.ScmObj* %argslist57665$cc481181, %struct.ScmObj** %stackaddr$prim59100, align 8
%stackaddr$prim59101 = alloca %struct.ScmObj*, align 8
%argslist57665$cc481182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48426, %struct.ScmObj* %argslist57665$cc481181)
store volatile %struct.ScmObj* %argslist57665$cc481182, %struct.ScmObj** %stackaddr$prim59101, align 8
%clofunc59102 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48118)
musttail call tailcc void %clofunc59102(%struct.ScmObj* %cc48118, %struct.ScmObj* %argslist57665$cc481182)
ret void
falsebranch$cmp59094:
%ae50556 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50557 = call %struct.ScmObj* @const_init_false()
%argslist57666$k484260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59103 = alloca %struct.ScmObj*, align 8
%argslist57666$k484261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50557, %struct.ScmObj* %argslist57666$k484260)
store volatile %struct.ScmObj* %argslist57666$k484261, %struct.ScmObj** %stackaddr$prim59103, align 8
%stackaddr$prim59104 = alloca %struct.ScmObj*, align 8
%argslist57666$k484262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50556, %struct.ScmObj* %argslist57666$k484261)
store volatile %struct.ScmObj* %argslist57666$k484262, %struct.ScmObj** %stackaddr$prim59104, align 8
%clofunc59105 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48426)
musttail call tailcc void %clofunc59105(%struct.ScmObj* %k48426, %struct.ScmObj* %argslist57666$k484262)
ret void
}

define tailcc void @proc_clo$ae50385(%struct.ScmObj* %env$ae50385,%struct.ScmObj* %current_45args57667) {
%stackaddr$env-ref59106 = alloca %struct.ScmObj*, align 8
%a48117 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50385, i64 0)
store %struct.ScmObj* %a48117, %struct.ScmObj** %stackaddr$env-ref59106
%stackaddr$env-ref59107 = alloca %struct.ScmObj*, align 8
%k48426 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50385, i64 1)
store %struct.ScmObj* %k48426, %struct.ScmObj** %stackaddr$env-ref59107
%stackaddr$prim59108 = alloca %struct.ScmObj*, align 8
%_95k48428 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57667)
store volatile %struct.ScmObj* %_95k48428, %struct.ScmObj** %stackaddr$prim59108, align 8
%stackaddr$prim59109 = alloca %struct.ScmObj*, align 8
%current_45args57668 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57667)
store volatile %struct.ScmObj* %current_45args57668, %struct.ScmObj** %stackaddr$prim59109, align 8
%stackaddr$prim59110 = alloca %struct.ScmObj*, align 8
%cc48118 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57668)
store volatile %struct.ScmObj* %cc48118, %struct.ScmObj** %stackaddr$prim59110, align 8
%ae50387 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59111 = alloca %struct.ScmObj*, align 8
%anf_45bind48249 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48117, %struct.ScmObj* %ae50387)
store volatile %struct.ScmObj* %anf_45bind48249, %struct.ScmObj** %stackaddr$prim59111, align 8
%stackaddr$prim59112 = alloca %struct.ScmObj*, align 8
%anf_45bind48250 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48249)
store volatile %struct.ScmObj* %anf_45bind48250, %struct.ScmObj** %stackaddr$prim59112, align 8
%truthy$cmp59113 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48250)
%cmp$cmp59113 = icmp eq i64 %truthy$cmp59113, 1
br i1 %cmp$cmp59113, label %truebranch$cmp59113, label %falsebranch$cmp59113
truebranch$cmp59113:
%ae50391 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50392 = call %struct.ScmObj* @const_init_true()
%argslist57670$k484260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59114 = alloca %struct.ScmObj*, align 8
%argslist57670$k484261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50392, %struct.ScmObj* %argslist57670$k484260)
store volatile %struct.ScmObj* %argslist57670$k484261, %struct.ScmObj** %stackaddr$prim59114, align 8
%stackaddr$prim59115 = alloca %struct.ScmObj*, align 8
%argslist57670$k484262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50391, %struct.ScmObj* %argslist57670$k484261)
store volatile %struct.ScmObj* %argslist57670$k484262, %struct.ScmObj** %stackaddr$prim59115, align 8
%clofunc59116 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48426)
musttail call tailcc void %clofunc59116(%struct.ScmObj* %k48426, %struct.ScmObj* %argslist57670$k484262)
ret void
falsebranch$cmp59113:
%ae50400 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59117 = alloca %struct.ScmObj*, align 8
%anf_45bind48251 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48117, %struct.ScmObj* %ae50400)
store volatile %struct.ScmObj* %anf_45bind48251, %struct.ScmObj** %stackaddr$prim59117, align 8
%stackaddr$prim59118 = alloca %struct.ScmObj*, align 8
%anf_45bind48252 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind48251)
store volatile %struct.ScmObj* %anf_45bind48252, %struct.ScmObj** %stackaddr$prim59118, align 8
%truthy$cmp59119 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48252)
%cmp$cmp59119 = icmp eq i64 %truthy$cmp59119, 1
br i1 %cmp$cmp59119, label %truebranch$cmp59119, label %falsebranch$cmp59119
truebranch$cmp59119:
%ae50404 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59120 = alloca %struct.ScmObj*, align 8
%anf_45bind48253 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48117, %struct.ScmObj* %ae50404)
store volatile %struct.ScmObj* %anf_45bind48253, %struct.ScmObj** %stackaddr$prim59120, align 8
%stackaddr$prim59121 = alloca %struct.ScmObj*, align 8
%b48120 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48253)
store volatile %struct.ScmObj* %b48120, %struct.ScmObj** %stackaddr$prim59121, align 8
%ae50407 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59122 = alloca %struct.ScmObj*, align 8
%anf_45bind48254 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48117, %struct.ScmObj* %ae50407)
store volatile %struct.ScmObj* %anf_45bind48254, %struct.ScmObj** %stackaddr$prim59122, align 8
%stackaddr$prim59123 = alloca %struct.ScmObj*, align 8
%anf_45bind48255 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48254)
store volatile %struct.ScmObj* %anf_45bind48255, %struct.ScmObj** %stackaddr$prim59123, align 8
%ae50410 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59124 = alloca %struct.ScmObj*, align 8
%_95048121 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a48117, %struct.ScmObj* %ae50410, %struct.ScmObj* %anf_45bind48255)
store volatile %struct.ScmObj* %_95048121, %struct.ScmObj** %stackaddr$prim59124, align 8
%argslist57671$cc481180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59125 = alloca %struct.ScmObj*, align 8
%argslist57671$cc481181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48118, %struct.ScmObj* %argslist57671$cc481180)
store volatile %struct.ScmObj* %argslist57671$cc481181, %struct.ScmObj** %stackaddr$prim59125, align 8
%stackaddr$prim59126 = alloca %struct.ScmObj*, align 8
%argslist57671$cc481182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48426, %struct.ScmObj* %argslist57671$cc481181)
store volatile %struct.ScmObj* %argslist57671$cc481182, %struct.ScmObj** %stackaddr$prim59126, align 8
%clofunc59127 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48118)
musttail call tailcc void %clofunc59127(%struct.ScmObj* %cc48118, %struct.ScmObj* %argslist57671$cc481182)
ret void
falsebranch$cmp59119:
%ae50443 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50444 = call %struct.ScmObj* @const_init_false()
%argslist57672$k484260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59128 = alloca %struct.ScmObj*, align 8
%argslist57672$k484261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50444, %struct.ScmObj* %argslist57672$k484260)
store volatile %struct.ScmObj* %argslist57672$k484261, %struct.ScmObj** %stackaddr$prim59128, align 8
%stackaddr$prim59129 = alloca %struct.ScmObj*, align 8
%argslist57672$k484262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50443, %struct.ScmObj* %argslist57672$k484261)
store volatile %struct.ScmObj* %argslist57672$k484262, %struct.ScmObj** %stackaddr$prim59129, align 8
%clofunc59130 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48426)
musttail call tailcc void %clofunc59130(%struct.ScmObj* %k48426, %struct.ScmObj* %argslist57672$k484262)
ret void
}

define tailcc void @proc_clo$ae50367(%struct.ScmObj* %env$ae50367,%struct.ScmObj* %current_45args57674) {
%stackaddr$prim59131 = alloca %struct.ScmObj*, align 8
%k48429 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57674)
store volatile %struct.ScmObj* %k48429, %struct.ScmObj** %stackaddr$prim59131, align 8
%stackaddr$prim59132 = alloca %struct.ScmObj*, align 8
%current_45args57675 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57674)
store volatile %struct.ScmObj* %current_45args57675, %struct.ScmObj** %stackaddr$prim59132, align 8
%stackaddr$prim59133 = alloca %struct.ScmObj*, align 8
%k48119 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57675)
store volatile %struct.ScmObj* %k48119, %struct.ScmObj** %stackaddr$prim59133, align 8
%ae50369 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57677$k484290 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59134 = alloca %struct.ScmObj*, align 8
%argslist57677$k484291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48119, %struct.ScmObj* %argslist57677$k484290)
store volatile %struct.ScmObj* %argslist57677$k484291, %struct.ScmObj** %stackaddr$prim59134, align 8
%stackaddr$prim59135 = alloca %struct.ScmObj*, align 8
%argslist57677$k484292 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50369, %struct.ScmObj* %argslist57677$k484291)
store volatile %struct.ScmObj* %argslist57677$k484292, %struct.ScmObj** %stackaddr$prim59135, align 8
%clofunc59136 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48429)
musttail call tailcc void %clofunc59136(%struct.ScmObj* %k48429, %struct.ScmObj* %argslist57677$k484292)
ret void
}

define tailcc void @proc_clo$ae50290(%struct.ScmObj* %env$ae50290,%struct.ScmObj* %current_45args57680) {
%stackaddr$env-ref59137 = alloca %struct.ScmObj*, align 8
%_37append48123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50290, i64 0)
store %struct.ScmObj* %_37append48123, %struct.ScmObj** %stackaddr$env-ref59137
%stackaddr$prim59138 = alloca %struct.ScmObj*, align 8
%k48430 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57680)
store volatile %struct.ScmObj* %k48430, %struct.ScmObj** %stackaddr$prim59138, align 8
%stackaddr$prim59139 = alloca %struct.ScmObj*, align 8
%current_45args57681 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57680)
store volatile %struct.ScmObj* %current_45args57681, %struct.ScmObj** %stackaddr$prim59139, align 8
%stackaddr$prim59140 = alloca %struct.ScmObj*, align 8
%ls048126 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57681)
store volatile %struct.ScmObj* %ls048126, %struct.ScmObj** %stackaddr$prim59140, align 8
%stackaddr$prim59141 = alloca %struct.ScmObj*, align 8
%current_45args57682 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57681)
store volatile %struct.ScmObj* %current_45args57682, %struct.ScmObj** %stackaddr$prim59141, align 8
%stackaddr$prim59142 = alloca %struct.ScmObj*, align 8
%ls148125 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57682)
store volatile %struct.ScmObj* %ls148125, %struct.ScmObj** %stackaddr$prim59142, align 8
%stackaddr$prim59143 = alloca %struct.ScmObj*, align 8
%anf_45bind48242 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls048126)
store volatile %struct.ScmObj* %anf_45bind48242, %struct.ScmObj** %stackaddr$prim59143, align 8
%truthy$cmp59144 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48242)
%cmp$cmp59144 = icmp eq i64 %truthy$cmp59144, 1
br i1 %cmp$cmp59144, label %truebranch$cmp59144, label %falsebranch$cmp59144
truebranch$cmp59144:
%ae50294 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57684$k484300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59145 = alloca %struct.ScmObj*, align 8
%argslist57684$k484301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls148125, %struct.ScmObj* %argslist57684$k484300)
store volatile %struct.ScmObj* %argslist57684$k484301, %struct.ScmObj** %stackaddr$prim59145, align 8
%stackaddr$prim59146 = alloca %struct.ScmObj*, align 8
%argslist57684$k484302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50294, %struct.ScmObj* %argslist57684$k484301)
store volatile %struct.ScmObj* %argslist57684$k484302, %struct.ScmObj** %stackaddr$prim59146, align 8
%clofunc59147 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48430)
musttail call tailcc void %clofunc59147(%struct.ScmObj* %k48430, %struct.ScmObj* %argslist57684$k484302)
ret void
falsebranch$cmp59144:
%stackaddr$prim59148 = alloca %struct.ScmObj*, align 8
%anf_45bind48243 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls048126)
store volatile %struct.ScmObj* %anf_45bind48243, %struct.ScmObj** %stackaddr$prim59148, align 8
%ae50301 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59149 = alloca %struct.ScmObj*, align 8
%anf_45bind48244 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append48123, %struct.ScmObj* %ae50301)
store volatile %struct.ScmObj* %anf_45bind48244, %struct.ScmObj** %stackaddr$prim59149, align 8
%stackaddr$prim59150 = alloca %struct.ScmObj*, align 8
%anf_45bind48245 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls048126)
store volatile %struct.ScmObj* %anf_45bind48245, %struct.ScmObj** %stackaddr$prim59150, align 8
%stackaddr$makeclosure59151 = alloca %struct.ScmObj*, align 8
%fptrToInt59152 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50304 to i64
%ae50304 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt59152)
store volatile %struct.ScmObj* %ae50304, %struct.ScmObj** %stackaddr$makeclosure59151, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50304, %struct.ScmObj* %anf_45bind48243, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50304, %struct.ScmObj* %k48430, i64 1)
%argslist57689$anf_45bind482440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59153 = alloca %struct.ScmObj*, align 8
%argslist57689$anf_45bind482441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls148125, %struct.ScmObj* %argslist57689$anf_45bind482440)
store volatile %struct.ScmObj* %argslist57689$anf_45bind482441, %struct.ScmObj** %stackaddr$prim59153, align 8
%stackaddr$prim59154 = alloca %struct.ScmObj*, align 8
%argslist57689$anf_45bind482442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48245, %struct.ScmObj* %argslist57689$anf_45bind482441)
store volatile %struct.ScmObj* %argslist57689$anf_45bind482442, %struct.ScmObj** %stackaddr$prim59154, align 8
%stackaddr$prim59155 = alloca %struct.ScmObj*, align 8
%argslist57689$anf_45bind482443 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50304, %struct.ScmObj* %argslist57689$anf_45bind482442)
store volatile %struct.ScmObj* %argslist57689$anf_45bind482443, %struct.ScmObj** %stackaddr$prim59155, align 8
%clofunc59156 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48244)
musttail call tailcc void %clofunc59156(%struct.ScmObj* %anf_45bind48244, %struct.ScmObj* %argslist57689$anf_45bind482443)
ret void
}

define tailcc void @proc_clo$ae50304(%struct.ScmObj* %env$ae50304,%struct.ScmObj* %current_45args57685) {
%stackaddr$env-ref59157 = alloca %struct.ScmObj*, align 8
%anf_45bind48243 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50304, i64 0)
store %struct.ScmObj* %anf_45bind48243, %struct.ScmObj** %stackaddr$env-ref59157
%stackaddr$env-ref59158 = alloca %struct.ScmObj*, align 8
%k48430 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50304, i64 1)
store %struct.ScmObj* %k48430, %struct.ScmObj** %stackaddr$env-ref59158
%stackaddr$prim59159 = alloca %struct.ScmObj*, align 8
%_95k48431 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57685)
store volatile %struct.ScmObj* %_95k48431, %struct.ScmObj** %stackaddr$prim59159, align 8
%stackaddr$prim59160 = alloca %struct.ScmObj*, align 8
%current_45args57686 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57685)
store volatile %struct.ScmObj* %current_45args57686, %struct.ScmObj** %stackaddr$prim59160, align 8
%stackaddr$prim59161 = alloca %struct.ScmObj*, align 8
%anf_45bind48246 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57686)
store volatile %struct.ScmObj* %anf_45bind48246, %struct.ScmObj** %stackaddr$prim59161, align 8
%stackaddr$prim59162 = alloca %struct.ScmObj*, align 8
%cpsprim48432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48243, %struct.ScmObj* %anf_45bind48246)
store volatile %struct.ScmObj* %cpsprim48432, %struct.ScmObj** %stackaddr$prim59162, align 8
%ae50310 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57688$k484300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59163 = alloca %struct.ScmObj*, align 8
%argslist57688$k484301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48432, %struct.ScmObj* %argslist57688$k484300)
store volatile %struct.ScmObj* %argslist57688$k484301, %struct.ScmObj** %stackaddr$prim59163, align 8
%stackaddr$prim59164 = alloca %struct.ScmObj*, align 8
%argslist57688$k484302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50310, %struct.ScmObj* %argslist57688$k484301)
store volatile %struct.ScmObj* %argslist57688$k484302, %struct.ScmObj** %stackaddr$prim59164, align 8
%clofunc59165 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48430)
musttail call tailcc void %clofunc59165(%struct.ScmObj* %k48430, %struct.ScmObj* %argslist57688$k484302)
ret void
}

define tailcc void @proc_clo$ae50264(%struct.ScmObj* %env$ae50264,%struct.ScmObj* %current_45args57691) {
%stackaddr$prim59166 = alloca %struct.ScmObj*, align 8
%k48433 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57691)
store volatile %struct.ScmObj* %k48433, %struct.ScmObj** %stackaddr$prim59166, align 8
%stackaddr$prim59167 = alloca %struct.ScmObj*, align 8
%current_45args57692 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57691)
store volatile %struct.ScmObj* %current_45args57692, %struct.ScmObj** %stackaddr$prim59167, align 8
%stackaddr$prim59168 = alloca %struct.ScmObj*, align 8
%a48129 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57692)
store volatile %struct.ScmObj* %a48129, %struct.ScmObj** %stackaddr$prim59168, align 8
%stackaddr$prim59169 = alloca %struct.ScmObj*, align 8
%current_45args57693 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57692)
store volatile %struct.ScmObj* %current_45args57693, %struct.ScmObj** %stackaddr$prim59169, align 8
%stackaddr$prim59170 = alloca %struct.ScmObj*, align 8
%b48128 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57693)
store volatile %struct.ScmObj* %b48128, %struct.ScmObj** %stackaddr$prim59170, align 8
%stackaddr$prim59171 = alloca %struct.ScmObj*, align 8
%anf_45bind48241 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a48129, %struct.ScmObj* %b48128)
store volatile %struct.ScmObj* %anf_45bind48241, %struct.ScmObj** %stackaddr$prim59171, align 8
%stackaddr$prim59172 = alloca %struct.ScmObj*, align 8
%cpsprim48434 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind48241)
store volatile %struct.ScmObj* %cpsprim48434, %struct.ScmObj** %stackaddr$prim59172, align 8
%ae50269 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57695$k484330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59173 = alloca %struct.ScmObj*, align 8
%argslist57695$k484331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48434, %struct.ScmObj* %argslist57695$k484330)
store volatile %struct.ScmObj* %argslist57695$k484331, %struct.ScmObj** %stackaddr$prim59173, align 8
%stackaddr$prim59174 = alloca %struct.ScmObj*, align 8
%argslist57695$k484332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50269, %struct.ScmObj* %argslist57695$k484331)
store volatile %struct.ScmObj* %argslist57695$k484332, %struct.ScmObj** %stackaddr$prim59174, align 8
%clofunc59175 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48433)
musttail call tailcc void %clofunc59175(%struct.ScmObj* %k48433, %struct.ScmObj* %argslist57695$k484332)
ret void
}

define tailcc void @proc_clo$ae50240(%struct.ScmObj* %env$ae50240,%struct.ScmObj* %current_45args57697) {
%stackaddr$prim59176 = alloca %struct.ScmObj*, align 8
%k48435 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57697)
store volatile %struct.ScmObj* %k48435, %struct.ScmObj** %stackaddr$prim59176, align 8
%stackaddr$prim59177 = alloca %struct.ScmObj*, align 8
%current_45args57698 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57697)
store volatile %struct.ScmObj* %current_45args57698, %struct.ScmObj** %stackaddr$prim59177, align 8
%stackaddr$prim59178 = alloca %struct.ScmObj*, align 8
%a48132 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57698)
store volatile %struct.ScmObj* %a48132, %struct.ScmObj** %stackaddr$prim59178, align 8
%stackaddr$prim59179 = alloca %struct.ScmObj*, align 8
%current_45args57699 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57698)
store volatile %struct.ScmObj* %current_45args57699, %struct.ScmObj** %stackaddr$prim59179, align 8
%stackaddr$prim59180 = alloca %struct.ScmObj*, align 8
%b48131 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57699)
store volatile %struct.ScmObj* %b48131, %struct.ScmObj** %stackaddr$prim59180, align 8
%stackaddr$prim59181 = alloca %struct.ScmObj*, align 8
%anf_45bind48240 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a48132, %struct.ScmObj* %b48131)
store volatile %struct.ScmObj* %anf_45bind48240, %struct.ScmObj** %stackaddr$prim59181, align 8
%stackaddr$prim59182 = alloca %struct.ScmObj*, align 8
%cpsprim48436 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind48240)
store volatile %struct.ScmObj* %cpsprim48436, %struct.ScmObj** %stackaddr$prim59182, align 8
%ae50245 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57701$k484350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59183 = alloca %struct.ScmObj*, align 8
%argslist57701$k484351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48436, %struct.ScmObj* %argslist57701$k484350)
store volatile %struct.ScmObj* %argslist57701$k484351, %struct.ScmObj** %stackaddr$prim59183, align 8
%stackaddr$prim59184 = alloca %struct.ScmObj*, align 8
%argslist57701$k484352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50245, %struct.ScmObj* %argslist57701$k484351)
store volatile %struct.ScmObj* %argslist57701$k484352, %struct.ScmObj** %stackaddr$prim59184, align 8
%clofunc59185 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48435)
musttail call tailcc void %clofunc59185(%struct.ScmObj* %k48435, %struct.ScmObj* %argslist57701$k484352)
ret void
}

define tailcc void @proc_clo$ae49846(%struct.ScmObj* %env$ae49846,%struct.ScmObj* %current_45args57704) {
%stackaddr$env-ref59186 = alloca %struct.ScmObj*, align 8
%_37foldr48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49846, i64 0)
store %struct.ScmObj* %_37foldr48056, %struct.ScmObj** %stackaddr$env-ref59186
%stackaddr$env-ref59187 = alloca %struct.ScmObj*, align 8
%_37foldr148051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49846, i64 1)
store %struct.ScmObj* %_37foldr148051, %struct.ScmObj** %stackaddr$env-ref59187
%stackaddr$env-ref59188 = alloca %struct.ScmObj*, align 8
%_37map148082 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49846, i64 2)
store %struct.ScmObj* %_37map148082, %struct.ScmObj** %stackaddr$env-ref59188
%stackaddr$prim59189 = alloca %struct.ScmObj*, align 8
%k48437 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57704)
store volatile %struct.ScmObj* %k48437, %struct.ScmObj** %stackaddr$prim59189, align 8
%stackaddr$prim59190 = alloca %struct.ScmObj*, align 8
%current_45args57705 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57704)
store volatile %struct.ScmObj* %current_45args57705, %struct.ScmObj** %stackaddr$prim59190, align 8
%stackaddr$prim59191 = alloca %struct.ScmObj*, align 8
%_37foldl48134 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57705)
store volatile %struct.ScmObj* %_37foldl48134, %struct.ScmObj** %stackaddr$prim59191, align 8
%ae49848 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59192 = alloca %struct.ScmObj*, align 8
%fptrToInt59193 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49849 to i64
%ae49849 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt59193)
store volatile %struct.ScmObj* %ae49849, %struct.ScmObj** %stackaddr$makeclosure59192, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49849, %struct.ScmObj* %_37foldr48056, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49849, %struct.ScmObj* %_37foldl48134, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49849, %struct.ScmObj* %_37foldr148051, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49849, %struct.ScmObj* %_37map148082, i64 3)
%argslist57762$k484370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59194 = alloca %struct.ScmObj*, align 8
%argslist57762$k484371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49849, %struct.ScmObj* %argslist57762$k484370)
store volatile %struct.ScmObj* %argslist57762$k484371, %struct.ScmObj** %stackaddr$prim59194, align 8
%stackaddr$prim59195 = alloca %struct.ScmObj*, align 8
%argslist57762$k484372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49848, %struct.ScmObj* %argslist57762$k484371)
store volatile %struct.ScmObj* %argslist57762$k484372, %struct.ScmObj** %stackaddr$prim59195, align 8
%clofunc59196 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48437)
musttail call tailcc void %clofunc59196(%struct.ScmObj* %k48437, %struct.ScmObj* %argslist57762$k484372)
ret void
}

define tailcc void @proc_clo$ae49849(%struct.ScmObj* %env$ae49849,%struct.ScmObj* %args4813548438) {
%stackaddr$env-ref59197 = alloca %struct.ScmObj*, align 8
%_37foldr48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49849, i64 0)
store %struct.ScmObj* %_37foldr48056, %struct.ScmObj** %stackaddr$env-ref59197
%stackaddr$env-ref59198 = alloca %struct.ScmObj*, align 8
%_37foldl48134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49849, i64 1)
store %struct.ScmObj* %_37foldl48134, %struct.ScmObj** %stackaddr$env-ref59198
%stackaddr$env-ref59199 = alloca %struct.ScmObj*, align 8
%_37foldr148051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49849, i64 2)
store %struct.ScmObj* %_37foldr148051, %struct.ScmObj** %stackaddr$env-ref59199
%stackaddr$env-ref59200 = alloca %struct.ScmObj*, align 8
%_37map148082 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49849, i64 3)
store %struct.ScmObj* %_37map148082, %struct.ScmObj** %stackaddr$env-ref59200
%stackaddr$prim59201 = alloca %struct.ScmObj*, align 8
%k48439 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4813548438)
store volatile %struct.ScmObj* %k48439, %struct.ScmObj** %stackaddr$prim59201, align 8
%stackaddr$prim59202 = alloca %struct.ScmObj*, align 8
%args48135 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4813548438)
store volatile %struct.ScmObj* %args48135, %struct.ScmObj** %stackaddr$prim59202, align 8
%stackaddr$prim59203 = alloca %struct.ScmObj*, align 8
%f48138 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48135)
store volatile %struct.ScmObj* %f48138, %struct.ScmObj** %stackaddr$prim59203, align 8
%stackaddr$prim59204 = alloca %struct.ScmObj*, align 8
%anf_45bind48228 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48135)
store volatile %struct.ScmObj* %anf_45bind48228, %struct.ScmObj** %stackaddr$prim59204, align 8
%stackaddr$prim59205 = alloca %struct.ScmObj*, align 8
%acc48137 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48228)
store volatile %struct.ScmObj* %acc48137, %struct.ScmObj** %stackaddr$prim59205, align 8
%stackaddr$prim59206 = alloca %struct.ScmObj*, align 8
%anf_45bind48229 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48135)
store volatile %struct.ScmObj* %anf_45bind48229, %struct.ScmObj** %stackaddr$prim59206, align 8
%stackaddr$prim59207 = alloca %struct.ScmObj*, align 8
%lsts48136 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48229)
store volatile %struct.ScmObj* %lsts48136, %struct.ScmObj** %stackaddr$prim59207, align 8
%stackaddr$makeclosure59208 = alloca %struct.ScmObj*, align 8
%fptrToInt59209 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49857 to i64
%ae49857 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt59209)
store volatile %struct.ScmObj* %ae49857, %struct.ScmObj** %stackaddr$makeclosure59208, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49857, %struct.ScmObj* %lsts48136, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49857, %struct.ScmObj* %_37foldr48056, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49857, %struct.ScmObj* %acc48137, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49857, %struct.ScmObj* %k48439, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49857, %struct.ScmObj* %_37foldl48134, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49857, %struct.ScmObj* %_37foldr148051, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49857, %struct.ScmObj* %_37map148082, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae49857, %struct.ScmObj* %f48138, i64 7)
%ae49858 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59210 = alloca %struct.ScmObj*, align 8
%fptrToInt59211 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49859 to i64
%ae49859 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt59211)
store volatile %struct.ScmObj* %ae49859, %struct.ScmObj** %stackaddr$makeclosure59210, align 8
%argslist57761$ae498570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59212 = alloca %struct.ScmObj*, align 8
%argslist57761$ae498571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49859, %struct.ScmObj* %argslist57761$ae498570)
store volatile %struct.ScmObj* %argslist57761$ae498571, %struct.ScmObj** %stackaddr$prim59212, align 8
%stackaddr$prim59213 = alloca %struct.ScmObj*, align 8
%argslist57761$ae498572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49858, %struct.ScmObj* %argslist57761$ae498571)
store volatile %struct.ScmObj* %argslist57761$ae498572, %struct.ScmObj** %stackaddr$prim59213, align 8
%clofunc59214 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49857)
musttail call tailcc void %clofunc59214(%struct.ScmObj* %ae49857, %struct.ScmObj* %argslist57761$ae498572)
ret void
}

define tailcc void @proc_clo$ae49857(%struct.ScmObj* %env$ae49857,%struct.ScmObj* %current_45args57707) {
%stackaddr$env-ref59215 = alloca %struct.ScmObj*, align 8
%lsts48136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49857, i64 0)
store %struct.ScmObj* %lsts48136, %struct.ScmObj** %stackaddr$env-ref59215
%stackaddr$env-ref59216 = alloca %struct.ScmObj*, align 8
%_37foldr48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49857, i64 1)
store %struct.ScmObj* %_37foldr48056, %struct.ScmObj** %stackaddr$env-ref59216
%stackaddr$env-ref59217 = alloca %struct.ScmObj*, align 8
%acc48137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49857, i64 2)
store %struct.ScmObj* %acc48137, %struct.ScmObj** %stackaddr$env-ref59217
%stackaddr$env-ref59218 = alloca %struct.ScmObj*, align 8
%k48439 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49857, i64 3)
store %struct.ScmObj* %k48439, %struct.ScmObj** %stackaddr$env-ref59218
%stackaddr$env-ref59219 = alloca %struct.ScmObj*, align 8
%_37foldl48134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49857, i64 4)
store %struct.ScmObj* %_37foldl48134, %struct.ScmObj** %stackaddr$env-ref59219
%stackaddr$env-ref59220 = alloca %struct.ScmObj*, align 8
%_37foldr148051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49857, i64 5)
store %struct.ScmObj* %_37foldr148051, %struct.ScmObj** %stackaddr$env-ref59220
%stackaddr$env-ref59221 = alloca %struct.ScmObj*, align 8
%_37map148082 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49857, i64 6)
store %struct.ScmObj* %_37map148082, %struct.ScmObj** %stackaddr$env-ref59221
%stackaddr$env-ref59222 = alloca %struct.ScmObj*, align 8
%f48138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49857, i64 7)
store %struct.ScmObj* %f48138, %struct.ScmObj** %stackaddr$env-ref59222
%stackaddr$prim59223 = alloca %struct.ScmObj*, align 8
%_95k48440 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57707)
store volatile %struct.ScmObj* %_95k48440, %struct.ScmObj** %stackaddr$prim59223, align 8
%stackaddr$prim59224 = alloca %struct.ScmObj*, align 8
%current_45args57708 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57707)
store volatile %struct.ScmObj* %current_45args57708, %struct.ScmObj** %stackaddr$prim59224, align 8
%stackaddr$prim59225 = alloca %struct.ScmObj*, align 8
%anf_45bind48230 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57708)
store volatile %struct.ScmObj* %anf_45bind48230, %struct.ScmObj** %stackaddr$prim59225, align 8
%stackaddr$makeclosure59226 = alloca %struct.ScmObj*, align 8
%fptrToInt59227 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49889 to i64
%ae49889 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt59227)
store volatile %struct.ScmObj* %ae49889, %struct.ScmObj** %stackaddr$makeclosure59226, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49889, %struct.ScmObj* %lsts48136, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49889, %struct.ScmObj* %_37foldr48056, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49889, %struct.ScmObj* %acc48137, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49889, %struct.ScmObj* %k48439, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49889, %struct.ScmObj* %_37foldl48134, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49889, %struct.ScmObj* %_37map148082, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49889, %struct.ScmObj* %f48138, i64 6)
%ae49891 = call %struct.ScmObj* @const_init_false()
%argslist57754$_37foldr1480510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59228 = alloca %struct.ScmObj*, align 8
%argslist57754$_37foldr1480511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48136, %struct.ScmObj* %argslist57754$_37foldr1480510)
store volatile %struct.ScmObj* %argslist57754$_37foldr1480511, %struct.ScmObj** %stackaddr$prim59228, align 8
%stackaddr$prim59229 = alloca %struct.ScmObj*, align 8
%argslist57754$_37foldr1480512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49891, %struct.ScmObj* %argslist57754$_37foldr1480511)
store volatile %struct.ScmObj* %argslist57754$_37foldr1480512, %struct.ScmObj** %stackaddr$prim59229, align 8
%stackaddr$prim59230 = alloca %struct.ScmObj*, align 8
%argslist57754$_37foldr1480513 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48230, %struct.ScmObj* %argslist57754$_37foldr1480512)
store volatile %struct.ScmObj* %argslist57754$_37foldr1480513, %struct.ScmObj** %stackaddr$prim59230, align 8
%stackaddr$prim59231 = alloca %struct.ScmObj*, align 8
%argslist57754$_37foldr1480514 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49889, %struct.ScmObj* %argslist57754$_37foldr1480513)
store volatile %struct.ScmObj* %argslist57754$_37foldr1480514, %struct.ScmObj** %stackaddr$prim59231, align 8
%clofunc59232 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148051)
musttail call tailcc void %clofunc59232(%struct.ScmObj* %_37foldr148051, %struct.ScmObj* %argslist57754$_37foldr1480514)
ret void
}

define tailcc void @proc_clo$ae49889(%struct.ScmObj* %env$ae49889,%struct.ScmObj* %current_45args57710) {
%stackaddr$env-ref59233 = alloca %struct.ScmObj*, align 8
%lsts48136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49889, i64 0)
store %struct.ScmObj* %lsts48136, %struct.ScmObj** %stackaddr$env-ref59233
%stackaddr$env-ref59234 = alloca %struct.ScmObj*, align 8
%_37foldr48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49889, i64 1)
store %struct.ScmObj* %_37foldr48056, %struct.ScmObj** %stackaddr$env-ref59234
%stackaddr$env-ref59235 = alloca %struct.ScmObj*, align 8
%acc48137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49889, i64 2)
store %struct.ScmObj* %acc48137, %struct.ScmObj** %stackaddr$env-ref59235
%stackaddr$env-ref59236 = alloca %struct.ScmObj*, align 8
%k48439 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49889, i64 3)
store %struct.ScmObj* %k48439, %struct.ScmObj** %stackaddr$env-ref59236
%stackaddr$env-ref59237 = alloca %struct.ScmObj*, align 8
%_37foldl48134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49889, i64 4)
store %struct.ScmObj* %_37foldl48134, %struct.ScmObj** %stackaddr$env-ref59237
%stackaddr$env-ref59238 = alloca %struct.ScmObj*, align 8
%_37map148082 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49889, i64 5)
store %struct.ScmObj* %_37map148082, %struct.ScmObj** %stackaddr$env-ref59238
%stackaddr$env-ref59239 = alloca %struct.ScmObj*, align 8
%f48138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49889, i64 6)
store %struct.ScmObj* %f48138, %struct.ScmObj** %stackaddr$env-ref59239
%stackaddr$prim59240 = alloca %struct.ScmObj*, align 8
%_95k48441 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57710)
store volatile %struct.ScmObj* %_95k48441, %struct.ScmObj** %stackaddr$prim59240, align 8
%stackaddr$prim59241 = alloca %struct.ScmObj*, align 8
%current_45args57711 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57710)
store volatile %struct.ScmObj* %current_45args57711, %struct.ScmObj** %stackaddr$prim59241, align 8
%stackaddr$prim59242 = alloca %struct.ScmObj*, align 8
%anf_45bind48231 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57711)
store volatile %struct.ScmObj* %anf_45bind48231, %struct.ScmObj** %stackaddr$prim59242, align 8
%truthy$cmp59243 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48231)
%cmp$cmp59243 = icmp eq i64 %truthy$cmp59243, 1
br i1 %cmp$cmp59243, label %truebranch$cmp59243, label %falsebranch$cmp59243
truebranch$cmp59243:
%ae49900 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57713$k484390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59244 = alloca %struct.ScmObj*, align 8
%argslist57713$k484391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48137, %struct.ScmObj* %argslist57713$k484390)
store volatile %struct.ScmObj* %argslist57713$k484391, %struct.ScmObj** %stackaddr$prim59244, align 8
%stackaddr$prim59245 = alloca %struct.ScmObj*, align 8
%argslist57713$k484392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49900, %struct.ScmObj* %argslist57713$k484391)
store volatile %struct.ScmObj* %argslist57713$k484392, %struct.ScmObj** %stackaddr$prim59245, align 8
%clofunc59246 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48439)
musttail call tailcc void %clofunc59246(%struct.ScmObj* %k48439, %struct.ScmObj* %argslist57713$k484392)
ret void
falsebranch$cmp59243:
%stackaddr$makeclosure59247 = alloca %struct.ScmObj*, align 8
%fptrToInt59248 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49905 to i64
%ae49905 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt59248)
store volatile %struct.ScmObj* %ae49905, %struct.ScmObj** %stackaddr$makeclosure59247, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49905, %struct.ScmObj* %lsts48136, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49905, %struct.ScmObj* %_37foldr48056, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49905, %struct.ScmObj* %acc48137, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49905, %struct.ScmObj* %k48439, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49905, %struct.ScmObj* %_37foldl48134, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49905, %struct.ScmObj* %_37map148082, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49905, %struct.ScmObj* %f48138, i64 6)
%ae49906 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59249 = alloca %struct.ScmObj*, align 8
%fptrToInt59250 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49907 to i64
%ae49907 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt59250)
store volatile %struct.ScmObj* %ae49907, %struct.ScmObj** %stackaddr$makeclosure59249, align 8
%argslist57753$ae499050 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59251 = alloca %struct.ScmObj*, align 8
%argslist57753$ae499051 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49907, %struct.ScmObj* %argslist57753$ae499050)
store volatile %struct.ScmObj* %argslist57753$ae499051, %struct.ScmObj** %stackaddr$prim59251, align 8
%stackaddr$prim59252 = alloca %struct.ScmObj*, align 8
%argslist57753$ae499052 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49906, %struct.ScmObj* %argslist57753$ae499051)
store volatile %struct.ScmObj* %argslist57753$ae499052, %struct.ScmObj** %stackaddr$prim59252, align 8
%clofunc59253 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49905)
musttail call tailcc void %clofunc59253(%struct.ScmObj* %ae49905, %struct.ScmObj* %argslist57753$ae499052)
ret void
}

define tailcc void @proc_clo$ae49905(%struct.ScmObj* %env$ae49905,%struct.ScmObj* %current_45args57714) {
%stackaddr$env-ref59254 = alloca %struct.ScmObj*, align 8
%lsts48136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49905, i64 0)
store %struct.ScmObj* %lsts48136, %struct.ScmObj** %stackaddr$env-ref59254
%stackaddr$env-ref59255 = alloca %struct.ScmObj*, align 8
%_37foldr48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49905, i64 1)
store %struct.ScmObj* %_37foldr48056, %struct.ScmObj** %stackaddr$env-ref59255
%stackaddr$env-ref59256 = alloca %struct.ScmObj*, align 8
%acc48137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49905, i64 2)
store %struct.ScmObj* %acc48137, %struct.ScmObj** %stackaddr$env-ref59256
%stackaddr$env-ref59257 = alloca %struct.ScmObj*, align 8
%k48439 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49905, i64 3)
store %struct.ScmObj* %k48439, %struct.ScmObj** %stackaddr$env-ref59257
%stackaddr$env-ref59258 = alloca %struct.ScmObj*, align 8
%_37foldl48134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49905, i64 4)
store %struct.ScmObj* %_37foldl48134, %struct.ScmObj** %stackaddr$env-ref59258
%stackaddr$env-ref59259 = alloca %struct.ScmObj*, align 8
%_37map148082 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49905, i64 5)
store %struct.ScmObj* %_37map148082, %struct.ScmObj** %stackaddr$env-ref59259
%stackaddr$env-ref59260 = alloca %struct.ScmObj*, align 8
%f48138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49905, i64 6)
store %struct.ScmObj* %f48138, %struct.ScmObj** %stackaddr$env-ref59260
%stackaddr$prim59261 = alloca %struct.ScmObj*, align 8
%_95k48442 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57714)
store volatile %struct.ScmObj* %_95k48442, %struct.ScmObj** %stackaddr$prim59261, align 8
%stackaddr$prim59262 = alloca %struct.ScmObj*, align 8
%current_45args57715 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57714)
store volatile %struct.ScmObj* %current_45args57715, %struct.ScmObj** %stackaddr$prim59262, align 8
%stackaddr$prim59263 = alloca %struct.ScmObj*, align 8
%anf_45bind48232 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57715)
store volatile %struct.ScmObj* %anf_45bind48232, %struct.ScmObj** %stackaddr$prim59263, align 8
%stackaddr$makeclosure59264 = alloca %struct.ScmObj*, align 8
%fptrToInt59265 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49926 to i64
%ae49926 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt59265)
store volatile %struct.ScmObj* %ae49926, %struct.ScmObj** %stackaddr$makeclosure59264, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49926, %struct.ScmObj* %lsts48136, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49926, %struct.ScmObj* %_37foldr48056, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49926, %struct.ScmObj* %acc48137, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49926, %struct.ScmObj* %k48439, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49926, %struct.ScmObj* %_37foldl48134, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49926, %struct.ScmObj* %_37map148082, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49926, %struct.ScmObj* %f48138, i64 6)
%argslist57748$_37map1480820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59266 = alloca %struct.ScmObj*, align 8
%argslist57748$_37map1480821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48136, %struct.ScmObj* %argslist57748$_37map1480820)
store volatile %struct.ScmObj* %argslist57748$_37map1480821, %struct.ScmObj** %stackaddr$prim59266, align 8
%stackaddr$prim59267 = alloca %struct.ScmObj*, align 8
%argslist57748$_37map1480822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48232, %struct.ScmObj* %argslist57748$_37map1480821)
store volatile %struct.ScmObj* %argslist57748$_37map1480822, %struct.ScmObj** %stackaddr$prim59267, align 8
%stackaddr$prim59268 = alloca %struct.ScmObj*, align 8
%argslist57748$_37map1480823 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49926, %struct.ScmObj* %argslist57748$_37map1480822)
store volatile %struct.ScmObj* %argslist57748$_37map1480823, %struct.ScmObj** %stackaddr$prim59268, align 8
%clofunc59269 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148082)
musttail call tailcc void %clofunc59269(%struct.ScmObj* %_37map148082, %struct.ScmObj* %argslist57748$_37map1480823)
ret void
}

define tailcc void @proc_clo$ae49926(%struct.ScmObj* %env$ae49926,%struct.ScmObj* %current_45args57717) {
%stackaddr$env-ref59270 = alloca %struct.ScmObj*, align 8
%lsts48136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49926, i64 0)
store %struct.ScmObj* %lsts48136, %struct.ScmObj** %stackaddr$env-ref59270
%stackaddr$env-ref59271 = alloca %struct.ScmObj*, align 8
%_37foldr48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49926, i64 1)
store %struct.ScmObj* %_37foldr48056, %struct.ScmObj** %stackaddr$env-ref59271
%stackaddr$env-ref59272 = alloca %struct.ScmObj*, align 8
%acc48137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49926, i64 2)
store %struct.ScmObj* %acc48137, %struct.ScmObj** %stackaddr$env-ref59272
%stackaddr$env-ref59273 = alloca %struct.ScmObj*, align 8
%k48439 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49926, i64 3)
store %struct.ScmObj* %k48439, %struct.ScmObj** %stackaddr$env-ref59273
%stackaddr$env-ref59274 = alloca %struct.ScmObj*, align 8
%_37foldl48134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49926, i64 4)
store %struct.ScmObj* %_37foldl48134, %struct.ScmObj** %stackaddr$env-ref59274
%stackaddr$env-ref59275 = alloca %struct.ScmObj*, align 8
%_37map148082 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49926, i64 5)
store %struct.ScmObj* %_37map148082, %struct.ScmObj** %stackaddr$env-ref59275
%stackaddr$env-ref59276 = alloca %struct.ScmObj*, align 8
%f48138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49926, i64 6)
store %struct.ScmObj* %f48138, %struct.ScmObj** %stackaddr$env-ref59276
%stackaddr$prim59277 = alloca %struct.ScmObj*, align 8
%_95k48443 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57717)
store volatile %struct.ScmObj* %_95k48443, %struct.ScmObj** %stackaddr$prim59277, align 8
%stackaddr$prim59278 = alloca %struct.ScmObj*, align 8
%current_45args57718 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57717)
store volatile %struct.ScmObj* %current_45args57718, %struct.ScmObj** %stackaddr$prim59278, align 8
%stackaddr$prim59279 = alloca %struct.ScmObj*, align 8
%lsts_4348143 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57718)
store volatile %struct.ScmObj* %lsts_4348143, %struct.ScmObj** %stackaddr$prim59279, align 8
%stackaddr$makeclosure59280 = alloca %struct.ScmObj*, align 8
%fptrToInt59281 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49929 to i64
%ae49929 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt59281)
store volatile %struct.ScmObj* %ae49929, %struct.ScmObj** %stackaddr$makeclosure59280, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49929, %struct.ScmObj* %lsts48136, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49929, %struct.ScmObj* %_37foldr48056, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49929, %struct.ScmObj* %acc48137, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49929, %struct.ScmObj* %k48439, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49929, %struct.ScmObj* %_37foldl48134, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49929, %struct.ScmObj* %_37map148082, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49929, %struct.ScmObj* %lsts_4348143, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae49929, %struct.ScmObj* %f48138, i64 7)
%ae49930 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59282 = alloca %struct.ScmObj*, align 8
%fptrToInt59283 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49931 to i64
%ae49931 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt59283)
store volatile %struct.ScmObj* %ae49931, %struct.ScmObj** %stackaddr$makeclosure59282, align 8
%argslist57747$ae499290 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59284 = alloca %struct.ScmObj*, align 8
%argslist57747$ae499291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49931, %struct.ScmObj* %argslist57747$ae499290)
store volatile %struct.ScmObj* %argslist57747$ae499291, %struct.ScmObj** %stackaddr$prim59284, align 8
%stackaddr$prim59285 = alloca %struct.ScmObj*, align 8
%argslist57747$ae499292 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49930, %struct.ScmObj* %argslist57747$ae499291)
store volatile %struct.ScmObj* %argslist57747$ae499292, %struct.ScmObj** %stackaddr$prim59285, align 8
%clofunc59286 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49929)
musttail call tailcc void %clofunc59286(%struct.ScmObj* %ae49929, %struct.ScmObj* %argslist57747$ae499292)
ret void
}

define tailcc void @proc_clo$ae49929(%struct.ScmObj* %env$ae49929,%struct.ScmObj* %current_45args57720) {
%stackaddr$env-ref59287 = alloca %struct.ScmObj*, align 8
%lsts48136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49929, i64 0)
store %struct.ScmObj* %lsts48136, %struct.ScmObj** %stackaddr$env-ref59287
%stackaddr$env-ref59288 = alloca %struct.ScmObj*, align 8
%_37foldr48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49929, i64 1)
store %struct.ScmObj* %_37foldr48056, %struct.ScmObj** %stackaddr$env-ref59288
%stackaddr$env-ref59289 = alloca %struct.ScmObj*, align 8
%acc48137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49929, i64 2)
store %struct.ScmObj* %acc48137, %struct.ScmObj** %stackaddr$env-ref59289
%stackaddr$env-ref59290 = alloca %struct.ScmObj*, align 8
%k48439 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49929, i64 3)
store %struct.ScmObj* %k48439, %struct.ScmObj** %stackaddr$env-ref59290
%stackaddr$env-ref59291 = alloca %struct.ScmObj*, align 8
%_37foldl48134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49929, i64 4)
store %struct.ScmObj* %_37foldl48134, %struct.ScmObj** %stackaddr$env-ref59291
%stackaddr$env-ref59292 = alloca %struct.ScmObj*, align 8
%_37map148082 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49929, i64 5)
store %struct.ScmObj* %_37map148082, %struct.ScmObj** %stackaddr$env-ref59292
%stackaddr$env-ref59293 = alloca %struct.ScmObj*, align 8
%lsts_4348143 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49929, i64 6)
store %struct.ScmObj* %lsts_4348143, %struct.ScmObj** %stackaddr$env-ref59293
%stackaddr$env-ref59294 = alloca %struct.ScmObj*, align 8
%f48138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49929, i64 7)
store %struct.ScmObj* %f48138, %struct.ScmObj** %stackaddr$env-ref59294
%stackaddr$prim59295 = alloca %struct.ScmObj*, align 8
%_95k48444 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57720)
store volatile %struct.ScmObj* %_95k48444, %struct.ScmObj** %stackaddr$prim59295, align 8
%stackaddr$prim59296 = alloca %struct.ScmObj*, align 8
%current_45args57721 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57720)
store volatile %struct.ScmObj* %current_45args57721, %struct.ScmObj** %stackaddr$prim59296, align 8
%stackaddr$prim59297 = alloca %struct.ScmObj*, align 8
%anf_45bind48233 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57721)
store volatile %struct.ScmObj* %anf_45bind48233, %struct.ScmObj** %stackaddr$prim59297, align 8
%stackaddr$makeclosure59298 = alloca %struct.ScmObj*, align 8
%fptrToInt59299 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49950 to i64
%ae49950 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt59299)
store volatile %struct.ScmObj* %ae49950, %struct.ScmObj** %stackaddr$makeclosure59298, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49950, %struct.ScmObj* %acc48137, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49950, %struct.ScmObj* %_37foldr48056, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49950, %struct.ScmObj* %k48439, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49950, %struct.ScmObj* %_37foldl48134, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49950, %struct.ScmObj* %lsts_4348143, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49950, %struct.ScmObj* %f48138, i64 5)
%argslist57742$_37map1480820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59300 = alloca %struct.ScmObj*, align 8
%argslist57742$_37map1480821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48136, %struct.ScmObj* %argslist57742$_37map1480820)
store volatile %struct.ScmObj* %argslist57742$_37map1480821, %struct.ScmObj** %stackaddr$prim59300, align 8
%stackaddr$prim59301 = alloca %struct.ScmObj*, align 8
%argslist57742$_37map1480822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48233, %struct.ScmObj* %argslist57742$_37map1480821)
store volatile %struct.ScmObj* %argslist57742$_37map1480822, %struct.ScmObj** %stackaddr$prim59301, align 8
%stackaddr$prim59302 = alloca %struct.ScmObj*, align 8
%argslist57742$_37map1480823 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49950, %struct.ScmObj* %argslist57742$_37map1480822)
store volatile %struct.ScmObj* %argslist57742$_37map1480823, %struct.ScmObj** %stackaddr$prim59302, align 8
%clofunc59303 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148082)
musttail call tailcc void %clofunc59303(%struct.ScmObj* %_37map148082, %struct.ScmObj* %argslist57742$_37map1480823)
ret void
}

define tailcc void @proc_clo$ae49950(%struct.ScmObj* %env$ae49950,%struct.ScmObj* %current_45args57723) {
%stackaddr$env-ref59304 = alloca %struct.ScmObj*, align 8
%acc48137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49950, i64 0)
store %struct.ScmObj* %acc48137, %struct.ScmObj** %stackaddr$env-ref59304
%stackaddr$env-ref59305 = alloca %struct.ScmObj*, align 8
%_37foldr48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49950, i64 1)
store %struct.ScmObj* %_37foldr48056, %struct.ScmObj** %stackaddr$env-ref59305
%stackaddr$env-ref59306 = alloca %struct.ScmObj*, align 8
%k48439 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49950, i64 2)
store %struct.ScmObj* %k48439, %struct.ScmObj** %stackaddr$env-ref59306
%stackaddr$env-ref59307 = alloca %struct.ScmObj*, align 8
%_37foldl48134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49950, i64 3)
store %struct.ScmObj* %_37foldl48134, %struct.ScmObj** %stackaddr$env-ref59307
%stackaddr$env-ref59308 = alloca %struct.ScmObj*, align 8
%lsts_4348143 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49950, i64 4)
store %struct.ScmObj* %lsts_4348143, %struct.ScmObj** %stackaddr$env-ref59308
%stackaddr$env-ref59309 = alloca %struct.ScmObj*, align 8
%f48138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49950, i64 5)
store %struct.ScmObj* %f48138, %struct.ScmObj** %stackaddr$env-ref59309
%stackaddr$prim59310 = alloca %struct.ScmObj*, align 8
%_95k48445 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57723)
store volatile %struct.ScmObj* %_95k48445, %struct.ScmObj** %stackaddr$prim59310, align 8
%stackaddr$prim59311 = alloca %struct.ScmObj*, align 8
%current_45args57724 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57723)
store volatile %struct.ScmObj* %current_45args57724, %struct.ScmObj** %stackaddr$prim59311, align 8
%stackaddr$prim59312 = alloca %struct.ScmObj*, align 8
%vs48141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57724)
store volatile %struct.ScmObj* %vs48141, %struct.ScmObj** %stackaddr$prim59312, align 8
%stackaddr$makeclosure59313 = alloca %struct.ScmObj*, align 8
%fptrToInt59314 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49953 to i64
%ae49953 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt59314)
store volatile %struct.ScmObj* %ae49953, %struct.ScmObj** %stackaddr$makeclosure59313, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49953, %struct.ScmObj* %acc48137, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49953, %struct.ScmObj* %_37foldr48056, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49953, %struct.ScmObj* %k48439, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49953, %struct.ScmObj* %_37foldl48134, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49953, %struct.ScmObj* %lsts_4348143, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49953, %struct.ScmObj* %vs48141, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49953, %struct.ScmObj* %f48138, i64 6)
%ae49954 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59315 = alloca %struct.ScmObj*, align 8
%fptrToInt59316 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49955 to i64
%ae49955 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt59316)
store volatile %struct.ScmObj* %ae49955, %struct.ScmObj** %stackaddr$makeclosure59315, align 8
%argslist57741$ae499530 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59317 = alloca %struct.ScmObj*, align 8
%argslist57741$ae499531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49955, %struct.ScmObj* %argslist57741$ae499530)
store volatile %struct.ScmObj* %argslist57741$ae499531, %struct.ScmObj** %stackaddr$prim59317, align 8
%stackaddr$prim59318 = alloca %struct.ScmObj*, align 8
%argslist57741$ae499532 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49954, %struct.ScmObj* %argslist57741$ae499531)
store volatile %struct.ScmObj* %argslist57741$ae499532, %struct.ScmObj** %stackaddr$prim59318, align 8
%clofunc59319 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49953)
musttail call tailcc void %clofunc59319(%struct.ScmObj* %ae49953, %struct.ScmObj* %argslist57741$ae499532)
ret void
}

define tailcc void @proc_clo$ae49953(%struct.ScmObj* %env$ae49953,%struct.ScmObj* %current_45args57726) {
%stackaddr$env-ref59320 = alloca %struct.ScmObj*, align 8
%acc48137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49953, i64 0)
store %struct.ScmObj* %acc48137, %struct.ScmObj** %stackaddr$env-ref59320
%stackaddr$env-ref59321 = alloca %struct.ScmObj*, align 8
%_37foldr48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49953, i64 1)
store %struct.ScmObj* %_37foldr48056, %struct.ScmObj** %stackaddr$env-ref59321
%stackaddr$env-ref59322 = alloca %struct.ScmObj*, align 8
%k48439 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49953, i64 2)
store %struct.ScmObj* %k48439, %struct.ScmObj** %stackaddr$env-ref59322
%stackaddr$env-ref59323 = alloca %struct.ScmObj*, align 8
%_37foldl48134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49953, i64 3)
store %struct.ScmObj* %_37foldl48134, %struct.ScmObj** %stackaddr$env-ref59323
%stackaddr$env-ref59324 = alloca %struct.ScmObj*, align 8
%lsts_4348143 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49953, i64 4)
store %struct.ScmObj* %lsts_4348143, %struct.ScmObj** %stackaddr$env-ref59324
%stackaddr$env-ref59325 = alloca %struct.ScmObj*, align 8
%vs48141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49953, i64 5)
store %struct.ScmObj* %vs48141, %struct.ScmObj** %stackaddr$env-ref59325
%stackaddr$env-ref59326 = alloca %struct.ScmObj*, align 8
%f48138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49953, i64 6)
store %struct.ScmObj* %f48138, %struct.ScmObj** %stackaddr$env-ref59326
%stackaddr$prim59327 = alloca %struct.ScmObj*, align 8
%_95k48446 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57726)
store volatile %struct.ScmObj* %_95k48446, %struct.ScmObj** %stackaddr$prim59327, align 8
%stackaddr$prim59328 = alloca %struct.ScmObj*, align 8
%current_45args57727 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57726)
store volatile %struct.ScmObj* %current_45args57727, %struct.ScmObj** %stackaddr$prim59328, align 8
%stackaddr$prim59329 = alloca %struct.ScmObj*, align 8
%anf_45bind48234 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57727)
store volatile %struct.ScmObj* %anf_45bind48234, %struct.ScmObj** %stackaddr$prim59329, align 8
%ae49976 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59330 = alloca %struct.ScmObj*, align 8
%anf_45bind48235 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48137, %struct.ScmObj* %ae49976)
store volatile %struct.ScmObj* %anf_45bind48235, %struct.ScmObj** %stackaddr$prim59330, align 8
%stackaddr$makeclosure59331 = alloca %struct.ScmObj*, align 8
%fptrToInt59332 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49978 to i64
%ae49978 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt59332)
store volatile %struct.ScmObj* %ae49978, %struct.ScmObj** %stackaddr$makeclosure59331, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49978, %struct.ScmObj* %k48439, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49978, %struct.ScmObj* %_37foldl48134, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49978, %struct.ScmObj* %lsts_4348143, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49978, %struct.ScmObj* %f48138, i64 3)
%argslist57735$_37foldr480560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59333 = alloca %struct.ScmObj*, align 8
%argslist57735$_37foldr480561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs48141, %struct.ScmObj* %argslist57735$_37foldr480560)
store volatile %struct.ScmObj* %argslist57735$_37foldr480561, %struct.ScmObj** %stackaddr$prim59333, align 8
%stackaddr$prim59334 = alloca %struct.ScmObj*, align 8
%argslist57735$_37foldr480562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48235, %struct.ScmObj* %argslist57735$_37foldr480561)
store volatile %struct.ScmObj* %argslist57735$_37foldr480562, %struct.ScmObj** %stackaddr$prim59334, align 8
%stackaddr$prim59335 = alloca %struct.ScmObj*, align 8
%argslist57735$_37foldr480563 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48234, %struct.ScmObj* %argslist57735$_37foldr480562)
store volatile %struct.ScmObj* %argslist57735$_37foldr480563, %struct.ScmObj** %stackaddr$prim59335, align 8
%stackaddr$prim59336 = alloca %struct.ScmObj*, align 8
%argslist57735$_37foldr480564 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49978, %struct.ScmObj* %argslist57735$_37foldr480563)
store volatile %struct.ScmObj* %argslist57735$_37foldr480564, %struct.ScmObj** %stackaddr$prim59336, align 8
%clofunc59337 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr48056)
musttail call tailcc void %clofunc59337(%struct.ScmObj* %_37foldr48056, %struct.ScmObj* %argslist57735$_37foldr480564)
ret void
}

define tailcc void @proc_clo$ae49978(%struct.ScmObj* %env$ae49978,%struct.ScmObj* %current_45args57729) {
%stackaddr$env-ref59338 = alloca %struct.ScmObj*, align 8
%k48439 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49978, i64 0)
store %struct.ScmObj* %k48439, %struct.ScmObj** %stackaddr$env-ref59338
%stackaddr$env-ref59339 = alloca %struct.ScmObj*, align 8
%_37foldl48134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49978, i64 1)
store %struct.ScmObj* %_37foldl48134, %struct.ScmObj** %stackaddr$env-ref59339
%stackaddr$env-ref59340 = alloca %struct.ScmObj*, align 8
%lsts_4348143 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49978, i64 2)
store %struct.ScmObj* %lsts_4348143, %struct.ScmObj** %stackaddr$env-ref59340
%stackaddr$env-ref59341 = alloca %struct.ScmObj*, align 8
%f48138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49978, i64 3)
store %struct.ScmObj* %f48138, %struct.ScmObj** %stackaddr$env-ref59341
%stackaddr$prim59342 = alloca %struct.ScmObj*, align 8
%_95k48447 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57729)
store volatile %struct.ScmObj* %_95k48447, %struct.ScmObj** %stackaddr$prim59342, align 8
%stackaddr$prim59343 = alloca %struct.ScmObj*, align 8
%current_45args57730 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57729)
store volatile %struct.ScmObj* %current_45args57730, %struct.ScmObj** %stackaddr$prim59343, align 8
%stackaddr$prim59344 = alloca %struct.ScmObj*, align 8
%anf_45bind48236 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57730)
store volatile %struct.ScmObj* %anf_45bind48236, %struct.ScmObj** %stackaddr$prim59344, align 8
%stackaddr$makeclosure59345 = alloca %struct.ScmObj*, align 8
%fptrToInt59346 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49982 to i64
%ae49982 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt59346)
store volatile %struct.ScmObj* %ae49982, %struct.ScmObj** %stackaddr$makeclosure59345, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49982, %struct.ScmObj* %k48439, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49982, %struct.ScmObj* %_37foldl48134, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49982, %struct.ScmObj* %lsts_4348143, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49982, %struct.ScmObj* %f48138, i64 3)
%stackaddr$prim59347 = alloca %struct.ScmObj*, align 8
%cpsargs48450 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49982, %struct.ScmObj* %anf_45bind48236)
store volatile %struct.ScmObj* %cpsargs48450, %struct.ScmObj** %stackaddr$prim59347, align 8
%clofunc59348 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48138)
musttail call tailcc void %clofunc59348(%struct.ScmObj* %f48138, %struct.ScmObj* %cpsargs48450)
ret void
}

define tailcc void @proc_clo$ae49982(%struct.ScmObj* %env$ae49982,%struct.ScmObj* %current_45args57732) {
%stackaddr$env-ref59349 = alloca %struct.ScmObj*, align 8
%k48439 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49982, i64 0)
store %struct.ScmObj* %k48439, %struct.ScmObj** %stackaddr$env-ref59349
%stackaddr$env-ref59350 = alloca %struct.ScmObj*, align 8
%_37foldl48134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49982, i64 1)
store %struct.ScmObj* %_37foldl48134, %struct.ScmObj** %stackaddr$env-ref59350
%stackaddr$env-ref59351 = alloca %struct.ScmObj*, align 8
%lsts_4348143 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49982, i64 2)
store %struct.ScmObj* %lsts_4348143, %struct.ScmObj** %stackaddr$env-ref59351
%stackaddr$env-ref59352 = alloca %struct.ScmObj*, align 8
%f48138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49982, i64 3)
store %struct.ScmObj* %f48138, %struct.ScmObj** %stackaddr$env-ref59352
%stackaddr$prim59353 = alloca %struct.ScmObj*, align 8
%_95k48448 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57732)
store volatile %struct.ScmObj* %_95k48448, %struct.ScmObj** %stackaddr$prim59353, align 8
%stackaddr$prim59354 = alloca %struct.ScmObj*, align 8
%current_45args57733 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57732)
store volatile %struct.ScmObj* %current_45args57733, %struct.ScmObj** %stackaddr$prim59354, align 8
%stackaddr$prim59355 = alloca %struct.ScmObj*, align 8
%acc_4348145 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57733)
store volatile %struct.ScmObj* %acc_4348145, %struct.ScmObj** %stackaddr$prim59355, align 8
%stackaddr$prim59356 = alloca %struct.ScmObj*, align 8
%anf_45bind48237 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4348145, %struct.ScmObj* %lsts_4348143)
store volatile %struct.ScmObj* %anf_45bind48237, %struct.ScmObj** %stackaddr$prim59356, align 8
%stackaddr$prim59357 = alloca %struct.ScmObj*, align 8
%anf_45bind48238 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48138, %struct.ScmObj* %anf_45bind48237)
store volatile %struct.ScmObj* %anf_45bind48238, %struct.ScmObj** %stackaddr$prim59357, align 8
%stackaddr$prim59358 = alloca %struct.ScmObj*, align 8
%cpsargs48449 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48439, %struct.ScmObj* %anf_45bind48238)
store volatile %struct.ScmObj* %cpsargs48449, %struct.ScmObj** %stackaddr$prim59358, align 8
%clofunc59359 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl48134)
musttail call tailcc void %clofunc59359(%struct.ScmObj* %_37foldl48134, %struct.ScmObj* %cpsargs48449)
ret void
}

define tailcc void @proc_clo$ae49955(%struct.ScmObj* %env$ae49955,%struct.ScmObj* %current_45args57736) {
%stackaddr$prim59360 = alloca %struct.ScmObj*, align 8
%k48451 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57736)
store volatile %struct.ScmObj* %k48451, %struct.ScmObj** %stackaddr$prim59360, align 8
%stackaddr$prim59361 = alloca %struct.ScmObj*, align 8
%current_45args57737 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57736)
store volatile %struct.ScmObj* %current_45args57737, %struct.ScmObj** %stackaddr$prim59361, align 8
%stackaddr$prim59362 = alloca %struct.ScmObj*, align 8
%a48147 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57737)
store volatile %struct.ScmObj* %a48147, %struct.ScmObj** %stackaddr$prim59362, align 8
%stackaddr$prim59363 = alloca %struct.ScmObj*, align 8
%current_45args57738 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57737)
store volatile %struct.ScmObj* %current_45args57738, %struct.ScmObj** %stackaddr$prim59363, align 8
%stackaddr$prim59364 = alloca %struct.ScmObj*, align 8
%b48146 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57738)
store volatile %struct.ScmObj* %b48146, %struct.ScmObj** %stackaddr$prim59364, align 8
%stackaddr$prim59365 = alloca %struct.ScmObj*, align 8
%cpsprim48452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a48147, %struct.ScmObj* %b48146)
store volatile %struct.ScmObj* %cpsprim48452, %struct.ScmObj** %stackaddr$prim59365, align 8
%ae49959 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57740$k484510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59366 = alloca %struct.ScmObj*, align 8
%argslist57740$k484511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48452, %struct.ScmObj* %argslist57740$k484510)
store volatile %struct.ScmObj* %argslist57740$k484511, %struct.ScmObj** %stackaddr$prim59366, align 8
%stackaddr$prim59367 = alloca %struct.ScmObj*, align 8
%argslist57740$k484512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49959, %struct.ScmObj* %argslist57740$k484511)
store volatile %struct.ScmObj* %argslist57740$k484512, %struct.ScmObj** %stackaddr$prim59367, align 8
%clofunc59368 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48451)
musttail call tailcc void %clofunc59368(%struct.ScmObj* %k48451, %struct.ScmObj* %argslist57740$k484512)
ret void
}

define tailcc void @proc_clo$ae49931(%struct.ScmObj* %env$ae49931,%struct.ScmObj* %current_45args57743) {
%stackaddr$prim59369 = alloca %struct.ScmObj*, align 8
%k48453 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57743)
store volatile %struct.ScmObj* %k48453, %struct.ScmObj** %stackaddr$prim59369, align 8
%stackaddr$prim59370 = alloca %struct.ScmObj*, align 8
%current_45args57744 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57743)
store volatile %struct.ScmObj* %current_45args57744, %struct.ScmObj** %stackaddr$prim59370, align 8
%stackaddr$prim59371 = alloca %struct.ScmObj*, align 8
%x48142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57744)
store volatile %struct.ScmObj* %x48142, %struct.ScmObj** %stackaddr$prim59371, align 8
%stackaddr$prim59372 = alloca %struct.ScmObj*, align 8
%cpsprim48454 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48142)
store volatile %struct.ScmObj* %cpsprim48454, %struct.ScmObj** %stackaddr$prim59372, align 8
%ae49934 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57746$k484530 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59373 = alloca %struct.ScmObj*, align 8
%argslist57746$k484531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48454, %struct.ScmObj* %argslist57746$k484530)
store volatile %struct.ScmObj* %argslist57746$k484531, %struct.ScmObj** %stackaddr$prim59373, align 8
%stackaddr$prim59374 = alloca %struct.ScmObj*, align 8
%argslist57746$k484532 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49934, %struct.ScmObj* %argslist57746$k484531)
store volatile %struct.ScmObj* %argslist57746$k484532, %struct.ScmObj** %stackaddr$prim59374, align 8
%clofunc59375 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48453)
musttail call tailcc void %clofunc59375(%struct.ScmObj* %k48453, %struct.ScmObj* %argslist57746$k484532)
ret void
}

define tailcc void @proc_clo$ae49907(%struct.ScmObj* %env$ae49907,%struct.ScmObj* %current_45args57749) {
%stackaddr$prim59376 = alloca %struct.ScmObj*, align 8
%k48455 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57749)
store volatile %struct.ScmObj* %k48455, %struct.ScmObj** %stackaddr$prim59376, align 8
%stackaddr$prim59377 = alloca %struct.ScmObj*, align 8
%current_45args57750 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57749)
store volatile %struct.ScmObj* %current_45args57750, %struct.ScmObj** %stackaddr$prim59377, align 8
%stackaddr$prim59378 = alloca %struct.ScmObj*, align 8
%x48144 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57750)
store volatile %struct.ScmObj* %x48144, %struct.ScmObj** %stackaddr$prim59378, align 8
%stackaddr$prim59379 = alloca %struct.ScmObj*, align 8
%cpsprim48456 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48144)
store volatile %struct.ScmObj* %cpsprim48456, %struct.ScmObj** %stackaddr$prim59379, align 8
%ae49910 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57752$k484550 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59380 = alloca %struct.ScmObj*, align 8
%argslist57752$k484551 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48456, %struct.ScmObj* %argslist57752$k484550)
store volatile %struct.ScmObj* %argslist57752$k484551, %struct.ScmObj** %stackaddr$prim59380, align 8
%stackaddr$prim59381 = alloca %struct.ScmObj*, align 8
%argslist57752$k484552 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49910, %struct.ScmObj* %argslist57752$k484551)
store volatile %struct.ScmObj* %argslist57752$k484552, %struct.ScmObj** %stackaddr$prim59381, align 8
%clofunc59382 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48455)
musttail call tailcc void %clofunc59382(%struct.ScmObj* %k48455, %struct.ScmObj* %argslist57752$k484552)
ret void
}

define tailcc void @proc_clo$ae49859(%struct.ScmObj* %env$ae49859,%struct.ScmObj* %current_45args57755) {
%stackaddr$prim59383 = alloca %struct.ScmObj*, align 8
%k48457 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57755)
store volatile %struct.ScmObj* %k48457, %struct.ScmObj** %stackaddr$prim59383, align 8
%stackaddr$prim59384 = alloca %struct.ScmObj*, align 8
%current_45args57756 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57755)
store volatile %struct.ScmObj* %current_45args57756, %struct.ScmObj** %stackaddr$prim59384, align 8
%stackaddr$prim59385 = alloca %struct.ScmObj*, align 8
%lst48140 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57756)
store volatile %struct.ScmObj* %lst48140, %struct.ScmObj** %stackaddr$prim59385, align 8
%stackaddr$prim59386 = alloca %struct.ScmObj*, align 8
%current_45args57757 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57756)
store volatile %struct.ScmObj* %current_45args57757, %struct.ScmObj** %stackaddr$prim59386, align 8
%stackaddr$prim59387 = alloca %struct.ScmObj*, align 8
%b48139 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57757)
store volatile %struct.ScmObj* %b48139, %struct.ScmObj** %stackaddr$prim59387, align 8
%truthy$cmp59388 = call i64 @is_truthy_value(%struct.ScmObj* %b48139)
%cmp$cmp59388 = icmp eq i64 %truthy$cmp59388, 1
br i1 %cmp$cmp59388, label %truebranch$cmp59388, label %falsebranch$cmp59388
truebranch$cmp59388:
%ae49862 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57759$k484570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59389 = alloca %struct.ScmObj*, align 8
%argslist57759$k484571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b48139, %struct.ScmObj* %argslist57759$k484570)
store volatile %struct.ScmObj* %argslist57759$k484571, %struct.ScmObj** %stackaddr$prim59389, align 8
%stackaddr$prim59390 = alloca %struct.ScmObj*, align 8
%argslist57759$k484572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49862, %struct.ScmObj* %argslist57759$k484571)
store volatile %struct.ScmObj* %argslist57759$k484572, %struct.ScmObj** %stackaddr$prim59390, align 8
%clofunc59391 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48457)
musttail call tailcc void %clofunc59391(%struct.ScmObj* %k48457, %struct.ScmObj* %argslist57759$k484572)
ret void
falsebranch$cmp59388:
%stackaddr$prim59392 = alloca %struct.ScmObj*, align 8
%cpsprim48458 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48140)
store volatile %struct.ScmObj* %cpsprim48458, %struct.ScmObj** %stackaddr$prim59392, align 8
%ae49869 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57760$k484570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59393 = alloca %struct.ScmObj*, align 8
%argslist57760$k484571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48458, %struct.ScmObj* %argslist57760$k484570)
store volatile %struct.ScmObj* %argslist57760$k484571, %struct.ScmObj** %stackaddr$prim59393, align 8
%stackaddr$prim59394 = alloca %struct.ScmObj*, align 8
%argslist57760$k484572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49869, %struct.ScmObj* %argslist57760$k484571)
store volatile %struct.ScmObj* %argslist57760$k484572, %struct.ScmObj** %stackaddr$prim59394, align 8
%clofunc59395 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48457)
musttail call tailcc void %clofunc59395(%struct.ScmObj* %k48457, %struct.ScmObj* %argslist57760$k484572)
ret void
}

define tailcc void @proc_clo$ae49700(%struct.ScmObj* %env$ae49700,%struct.ScmObj* %args4807848459) {
%stackaddr$env-ref59396 = alloca %struct.ScmObj*, align 8
%_37last48073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49700, i64 0)
store %struct.ScmObj* %_37last48073, %struct.ScmObj** %stackaddr$env-ref59396
%stackaddr$env-ref59397 = alloca %struct.ScmObj*, align 8
%_37foldr48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49700, i64 1)
store %struct.ScmObj* %_37foldr48056, %struct.ScmObj** %stackaddr$env-ref59397
%stackaddr$env-ref59398 = alloca %struct.ScmObj*, align 8
%_37drop_45right48070 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49700, i64 2)
store %struct.ScmObj* %_37drop_45right48070, %struct.ScmObj** %stackaddr$env-ref59398
%stackaddr$prim59399 = alloca %struct.ScmObj*, align 8
%k48460 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4807848459)
store volatile %struct.ScmObj* %k48460, %struct.ScmObj** %stackaddr$prim59399, align 8
%stackaddr$prim59400 = alloca %struct.ScmObj*, align 8
%args48078 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4807848459)
store volatile %struct.ScmObj* %args48078, %struct.ScmObj** %stackaddr$prim59400, align 8
%stackaddr$prim59401 = alloca %struct.ScmObj*, align 8
%f48080 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48078)
store volatile %struct.ScmObj* %f48080, %struct.ScmObj** %stackaddr$prim59401, align 8
%stackaddr$prim59402 = alloca %struct.ScmObj*, align 8
%lsts48079 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48078)
store volatile %struct.ScmObj* %lsts48079, %struct.ScmObj** %stackaddr$prim59402, align 8
%stackaddr$makeclosure59403 = alloca %struct.ScmObj*, align 8
%fptrToInt59404 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49705 to i64
%ae49705 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt59404)
store volatile %struct.ScmObj* %ae49705, %struct.ScmObj** %stackaddr$makeclosure59403, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49705, %struct.ScmObj* %_37foldr48056, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49705, %struct.ScmObj* %lsts48079, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49705, %struct.ScmObj* %k48460, i64 2)
%ae49706 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59405 = alloca %struct.ScmObj*, align 8
%fptrToInt59406 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49707 to i64
%ae49707 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt59406)
store volatile %struct.ScmObj* %ae49707, %struct.ScmObj** %stackaddr$makeclosure59405, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49707, %struct.ScmObj* %_37last48073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49707, %struct.ScmObj* %_37drop_45right48070, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49707, %struct.ScmObj* %f48080, i64 2)
%argslist57779$ae497050 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59407 = alloca %struct.ScmObj*, align 8
%argslist57779$ae497051 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49707, %struct.ScmObj* %argslist57779$ae497050)
store volatile %struct.ScmObj* %argslist57779$ae497051, %struct.ScmObj** %stackaddr$prim59407, align 8
%stackaddr$prim59408 = alloca %struct.ScmObj*, align 8
%argslist57779$ae497052 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49706, %struct.ScmObj* %argslist57779$ae497051)
store volatile %struct.ScmObj* %argslist57779$ae497052, %struct.ScmObj** %stackaddr$prim59408, align 8
%clofunc59409 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49705)
musttail call tailcc void %clofunc59409(%struct.ScmObj* %ae49705, %struct.ScmObj* %argslist57779$ae497052)
ret void
}

define tailcc void @proc_clo$ae49705(%struct.ScmObj* %env$ae49705,%struct.ScmObj* %current_45args57764) {
%stackaddr$env-ref59410 = alloca %struct.ScmObj*, align 8
%_37foldr48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49705, i64 0)
store %struct.ScmObj* %_37foldr48056, %struct.ScmObj** %stackaddr$env-ref59410
%stackaddr$env-ref59411 = alloca %struct.ScmObj*, align 8
%lsts48079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49705, i64 1)
store %struct.ScmObj* %lsts48079, %struct.ScmObj** %stackaddr$env-ref59411
%stackaddr$env-ref59412 = alloca %struct.ScmObj*, align 8
%k48460 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49705, i64 2)
store %struct.ScmObj* %k48460, %struct.ScmObj** %stackaddr$env-ref59412
%stackaddr$prim59413 = alloca %struct.ScmObj*, align 8
%_95k48461 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57764)
store volatile %struct.ScmObj* %_95k48461, %struct.ScmObj** %stackaddr$prim59413, align 8
%stackaddr$prim59414 = alloca %struct.ScmObj*, align 8
%current_45args57765 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57764)
store volatile %struct.ScmObj* %current_45args57765, %struct.ScmObj** %stackaddr$prim59414, align 8
%stackaddr$prim59415 = alloca %struct.ScmObj*, align 8
%anf_45bind48225 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57765)
store volatile %struct.ScmObj* %anf_45bind48225, %struct.ScmObj** %stackaddr$prim59415, align 8
%ae49768 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59416 = alloca %struct.ScmObj*, align 8
%anf_45bind48226 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49768, %struct.ScmObj* %lsts48079)
store volatile %struct.ScmObj* %anf_45bind48226, %struct.ScmObj** %stackaddr$prim59416, align 8
%stackaddr$prim59417 = alloca %struct.ScmObj*, align 8
%anf_45bind48227 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48225, %struct.ScmObj* %anf_45bind48226)
store volatile %struct.ScmObj* %anf_45bind48227, %struct.ScmObj** %stackaddr$prim59417, align 8
%stackaddr$prim59418 = alloca %struct.ScmObj*, align 8
%cpsargs48462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48460, %struct.ScmObj* %anf_45bind48227)
store volatile %struct.ScmObj* %cpsargs48462, %struct.ScmObj** %stackaddr$prim59418, align 8
%clofunc59419 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr48056)
musttail call tailcc void %clofunc59419(%struct.ScmObj* %_37foldr48056, %struct.ScmObj* %cpsargs48462)
ret void
}

define tailcc void @proc_clo$ae49707(%struct.ScmObj* %env$ae49707,%struct.ScmObj* %fargs4808148463) {
%stackaddr$env-ref59420 = alloca %struct.ScmObj*, align 8
%_37last48073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49707, i64 0)
store %struct.ScmObj* %_37last48073, %struct.ScmObj** %stackaddr$env-ref59420
%stackaddr$env-ref59421 = alloca %struct.ScmObj*, align 8
%_37drop_45right48070 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49707, i64 1)
store %struct.ScmObj* %_37drop_45right48070, %struct.ScmObj** %stackaddr$env-ref59421
%stackaddr$env-ref59422 = alloca %struct.ScmObj*, align 8
%f48080 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49707, i64 2)
store %struct.ScmObj* %f48080, %struct.ScmObj** %stackaddr$env-ref59422
%stackaddr$prim59423 = alloca %struct.ScmObj*, align 8
%k48464 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs4808148463)
store volatile %struct.ScmObj* %k48464, %struct.ScmObj** %stackaddr$prim59423, align 8
%stackaddr$prim59424 = alloca %struct.ScmObj*, align 8
%fargs48081 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs4808148463)
store volatile %struct.ScmObj* %fargs48081, %struct.ScmObj** %stackaddr$prim59424, align 8
%stackaddr$makeclosure59425 = alloca %struct.ScmObj*, align 8
%fptrToInt59426 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49711 to i64
%ae49711 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt59426)
store volatile %struct.ScmObj* %ae49711, %struct.ScmObj** %stackaddr$makeclosure59425, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49711, %struct.ScmObj* %f48080, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49711, %struct.ScmObj* %k48464, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49711, %struct.ScmObj* %_37last48073, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49711, %struct.ScmObj* %fargs48081, i64 3)
%ae49713 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist57778$_37drop_45right480700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59427 = alloca %struct.ScmObj*, align 8
%argslist57778$_37drop_45right480701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49713, %struct.ScmObj* %argslist57778$_37drop_45right480700)
store volatile %struct.ScmObj* %argslist57778$_37drop_45right480701, %struct.ScmObj** %stackaddr$prim59427, align 8
%stackaddr$prim59428 = alloca %struct.ScmObj*, align 8
%argslist57778$_37drop_45right480702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs48081, %struct.ScmObj* %argslist57778$_37drop_45right480701)
store volatile %struct.ScmObj* %argslist57778$_37drop_45right480702, %struct.ScmObj** %stackaddr$prim59428, align 8
%stackaddr$prim59429 = alloca %struct.ScmObj*, align 8
%argslist57778$_37drop_45right480703 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49711, %struct.ScmObj* %argslist57778$_37drop_45right480702)
store volatile %struct.ScmObj* %argslist57778$_37drop_45right480703, %struct.ScmObj** %stackaddr$prim59429, align 8
%clofunc59430 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right48070)
musttail call tailcc void %clofunc59430(%struct.ScmObj* %_37drop_45right48070, %struct.ScmObj* %argslist57778$_37drop_45right480703)
ret void
}

define tailcc void @proc_clo$ae49711(%struct.ScmObj* %env$ae49711,%struct.ScmObj* %current_45args57767) {
%stackaddr$env-ref59431 = alloca %struct.ScmObj*, align 8
%f48080 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49711, i64 0)
store %struct.ScmObj* %f48080, %struct.ScmObj** %stackaddr$env-ref59431
%stackaddr$env-ref59432 = alloca %struct.ScmObj*, align 8
%k48464 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49711, i64 1)
store %struct.ScmObj* %k48464, %struct.ScmObj** %stackaddr$env-ref59432
%stackaddr$env-ref59433 = alloca %struct.ScmObj*, align 8
%_37last48073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49711, i64 2)
store %struct.ScmObj* %_37last48073, %struct.ScmObj** %stackaddr$env-ref59433
%stackaddr$env-ref59434 = alloca %struct.ScmObj*, align 8
%fargs48081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49711, i64 3)
store %struct.ScmObj* %fargs48081, %struct.ScmObj** %stackaddr$env-ref59434
%stackaddr$prim59435 = alloca %struct.ScmObj*, align 8
%_95k48465 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57767)
store volatile %struct.ScmObj* %_95k48465, %struct.ScmObj** %stackaddr$prim59435, align 8
%stackaddr$prim59436 = alloca %struct.ScmObj*, align 8
%current_45args57768 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57767)
store volatile %struct.ScmObj* %current_45args57768, %struct.ScmObj** %stackaddr$prim59436, align 8
%stackaddr$prim59437 = alloca %struct.ScmObj*, align 8
%anf_45bind48222 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57768)
store volatile %struct.ScmObj* %anf_45bind48222, %struct.ScmObj** %stackaddr$prim59437, align 8
%stackaddr$makeclosure59438 = alloca %struct.ScmObj*, align 8
%fptrToInt59439 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49718 to i64
%ae49718 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt59439)
store volatile %struct.ScmObj* %ae49718, %struct.ScmObj** %stackaddr$makeclosure59438, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49718, %struct.ScmObj* %_37last48073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49718, %struct.ScmObj* %fargs48081, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49718, %struct.ScmObj* %k48464, i64 2)
%stackaddr$prim59440 = alloca %struct.ScmObj*, align 8
%cpsargs48469 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49718, %struct.ScmObj* %anf_45bind48222)
store volatile %struct.ScmObj* %cpsargs48469, %struct.ScmObj** %stackaddr$prim59440, align 8
%clofunc59441 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48080)
musttail call tailcc void %clofunc59441(%struct.ScmObj* %f48080, %struct.ScmObj* %cpsargs48469)
ret void
}

define tailcc void @proc_clo$ae49718(%struct.ScmObj* %env$ae49718,%struct.ScmObj* %current_45args57770) {
%stackaddr$env-ref59442 = alloca %struct.ScmObj*, align 8
%_37last48073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49718, i64 0)
store %struct.ScmObj* %_37last48073, %struct.ScmObj** %stackaddr$env-ref59442
%stackaddr$env-ref59443 = alloca %struct.ScmObj*, align 8
%fargs48081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49718, i64 1)
store %struct.ScmObj* %fargs48081, %struct.ScmObj** %stackaddr$env-ref59443
%stackaddr$env-ref59444 = alloca %struct.ScmObj*, align 8
%k48464 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49718, i64 2)
store %struct.ScmObj* %k48464, %struct.ScmObj** %stackaddr$env-ref59444
%stackaddr$prim59445 = alloca %struct.ScmObj*, align 8
%_95k48466 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57770)
store volatile %struct.ScmObj* %_95k48466, %struct.ScmObj** %stackaddr$prim59445, align 8
%stackaddr$prim59446 = alloca %struct.ScmObj*, align 8
%current_45args57771 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57770)
store volatile %struct.ScmObj* %current_45args57771, %struct.ScmObj** %stackaddr$prim59446, align 8
%stackaddr$prim59447 = alloca %struct.ScmObj*, align 8
%anf_45bind48223 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57771)
store volatile %struct.ScmObj* %anf_45bind48223, %struct.ScmObj** %stackaddr$prim59447, align 8
%stackaddr$makeclosure59448 = alloca %struct.ScmObj*, align 8
%fptrToInt59449 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49723 to i64
%ae49723 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt59449)
store volatile %struct.ScmObj* %ae49723, %struct.ScmObj** %stackaddr$makeclosure59448, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49723, %struct.ScmObj* %k48464, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49723, %struct.ScmObj* %anf_45bind48223, i64 1)
%argslist57777$_37last480730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59450 = alloca %struct.ScmObj*, align 8
%argslist57777$_37last480731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs48081, %struct.ScmObj* %argslist57777$_37last480730)
store volatile %struct.ScmObj* %argslist57777$_37last480731, %struct.ScmObj** %stackaddr$prim59450, align 8
%stackaddr$prim59451 = alloca %struct.ScmObj*, align 8
%argslist57777$_37last480732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49723, %struct.ScmObj* %argslist57777$_37last480731)
store volatile %struct.ScmObj* %argslist57777$_37last480732, %struct.ScmObj** %stackaddr$prim59451, align 8
%clofunc59452 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last48073)
musttail call tailcc void %clofunc59452(%struct.ScmObj* %_37last48073, %struct.ScmObj* %argslist57777$_37last480732)
ret void
}

define tailcc void @proc_clo$ae49723(%struct.ScmObj* %env$ae49723,%struct.ScmObj* %current_45args57773) {
%stackaddr$env-ref59453 = alloca %struct.ScmObj*, align 8
%k48464 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49723, i64 0)
store %struct.ScmObj* %k48464, %struct.ScmObj** %stackaddr$env-ref59453
%stackaddr$env-ref59454 = alloca %struct.ScmObj*, align 8
%anf_45bind48223 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49723, i64 1)
store %struct.ScmObj* %anf_45bind48223, %struct.ScmObj** %stackaddr$env-ref59454
%stackaddr$prim59455 = alloca %struct.ScmObj*, align 8
%_95k48467 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57773)
store volatile %struct.ScmObj* %_95k48467, %struct.ScmObj** %stackaddr$prim59455, align 8
%stackaddr$prim59456 = alloca %struct.ScmObj*, align 8
%current_45args57774 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57773)
store volatile %struct.ScmObj* %current_45args57774, %struct.ScmObj** %stackaddr$prim59456, align 8
%stackaddr$prim59457 = alloca %struct.ScmObj*, align 8
%anf_45bind48224 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57774)
store volatile %struct.ScmObj* %anf_45bind48224, %struct.ScmObj** %stackaddr$prim59457, align 8
%stackaddr$prim59458 = alloca %struct.ScmObj*, align 8
%cpsprim48468 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48223, %struct.ScmObj* %anf_45bind48224)
store volatile %struct.ScmObj* %cpsprim48468, %struct.ScmObj** %stackaddr$prim59458, align 8
%ae49728 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57776$k484640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59459 = alloca %struct.ScmObj*, align 8
%argslist57776$k484641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48468, %struct.ScmObj* %argslist57776$k484640)
store volatile %struct.ScmObj* %argslist57776$k484641, %struct.ScmObj** %stackaddr$prim59459, align 8
%stackaddr$prim59460 = alloca %struct.ScmObj*, align 8
%argslist57776$k484642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49728, %struct.ScmObj* %argslist57776$k484641)
store volatile %struct.ScmObj* %argslist57776$k484642, %struct.ScmObj** %stackaddr$prim59460, align 8
%clofunc59461 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48464)
musttail call tailcc void %clofunc59461(%struct.ScmObj* %k48464, %struct.ScmObj* %argslist57776$k484642)
ret void
}

define tailcc void @proc_clo$ae49623(%struct.ScmObj* %env$ae49623,%struct.ScmObj* %current_45args57781) {
%stackaddr$env-ref59462 = alloca %struct.ScmObj*, align 8
%_37foldr148051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49623, i64 0)
store %struct.ScmObj* %_37foldr148051, %struct.ScmObj** %stackaddr$env-ref59462
%stackaddr$prim59463 = alloca %struct.ScmObj*, align 8
%k48470 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57781)
store volatile %struct.ScmObj* %k48470, %struct.ScmObj** %stackaddr$prim59463, align 8
%stackaddr$prim59464 = alloca %struct.ScmObj*, align 8
%current_45args57782 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57781)
store volatile %struct.ScmObj* %current_45args57782, %struct.ScmObj** %stackaddr$prim59464, align 8
%stackaddr$prim59465 = alloca %struct.ScmObj*, align 8
%f48084 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57782)
store volatile %struct.ScmObj* %f48084, %struct.ScmObj** %stackaddr$prim59465, align 8
%stackaddr$prim59466 = alloca %struct.ScmObj*, align 8
%current_45args57783 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57782)
store volatile %struct.ScmObj* %current_45args57783, %struct.ScmObj** %stackaddr$prim59466, align 8
%stackaddr$prim59467 = alloca %struct.ScmObj*, align 8
%lst48083 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57783)
store volatile %struct.ScmObj* %lst48083, %struct.ScmObj** %stackaddr$prim59467, align 8
%stackaddr$makeclosure59468 = alloca %struct.ScmObj*, align 8
%fptrToInt59469 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49624 to i64
%ae49624 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt59469)
store volatile %struct.ScmObj* %ae49624, %struct.ScmObj** %stackaddr$makeclosure59468, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49624, %struct.ScmObj* %lst48083, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49624, %struct.ScmObj* %_37foldr148051, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49624, %struct.ScmObj* %k48470, i64 2)
%ae49625 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59470 = alloca %struct.ScmObj*, align 8
%fptrToInt59471 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49626 to i64
%ae49626 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt59471)
store volatile %struct.ScmObj* %ae49626, %struct.ScmObj** %stackaddr$makeclosure59470, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49626, %struct.ScmObj* %f48084, i64 0)
%argslist57798$ae496240 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59472 = alloca %struct.ScmObj*, align 8
%argslist57798$ae496241 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49626, %struct.ScmObj* %argslist57798$ae496240)
store volatile %struct.ScmObj* %argslist57798$ae496241, %struct.ScmObj** %stackaddr$prim59472, align 8
%stackaddr$prim59473 = alloca %struct.ScmObj*, align 8
%argslist57798$ae496242 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49625, %struct.ScmObj* %argslist57798$ae496241)
store volatile %struct.ScmObj* %argslist57798$ae496242, %struct.ScmObj** %stackaddr$prim59473, align 8
%clofunc59474 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49624)
musttail call tailcc void %clofunc59474(%struct.ScmObj* %ae49624, %struct.ScmObj* %argslist57798$ae496242)
ret void
}

define tailcc void @proc_clo$ae49624(%struct.ScmObj* %env$ae49624,%struct.ScmObj* %current_45args57785) {
%stackaddr$env-ref59475 = alloca %struct.ScmObj*, align 8
%lst48083 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49624, i64 0)
store %struct.ScmObj* %lst48083, %struct.ScmObj** %stackaddr$env-ref59475
%stackaddr$env-ref59476 = alloca %struct.ScmObj*, align 8
%_37foldr148051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49624, i64 1)
store %struct.ScmObj* %_37foldr148051, %struct.ScmObj** %stackaddr$env-ref59476
%stackaddr$env-ref59477 = alloca %struct.ScmObj*, align 8
%k48470 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49624, i64 2)
store %struct.ScmObj* %k48470, %struct.ScmObj** %stackaddr$env-ref59477
%stackaddr$prim59478 = alloca %struct.ScmObj*, align 8
%_95k48471 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57785)
store volatile %struct.ScmObj* %_95k48471, %struct.ScmObj** %stackaddr$prim59478, align 8
%stackaddr$prim59479 = alloca %struct.ScmObj*, align 8
%current_45args57786 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57785)
store volatile %struct.ScmObj* %current_45args57786, %struct.ScmObj** %stackaddr$prim59479, align 8
%stackaddr$prim59480 = alloca %struct.ScmObj*, align 8
%anf_45bind48221 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57786)
store volatile %struct.ScmObj* %anf_45bind48221, %struct.ScmObj** %stackaddr$prim59480, align 8
%ae49658 = call %struct.ScmObj* @const_init_null()
%argslist57788$_37foldr1480510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59481 = alloca %struct.ScmObj*, align 8
%argslist57788$_37foldr1480511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48083, %struct.ScmObj* %argslist57788$_37foldr1480510)
store volatile %struct.ScmObj* %argslist57788$_37foldr1480511, %struct.ScmObj** %stackaddr$prim59481, align 8
%stackaddr$prim59482 = alloca %struct.ScmObj*, align 8
%argslist57788$_37foldr1480512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49658, %struct.ScmObj* %argslist57788$_37foldr1480511)
store volatile %struct.ScmObj* %argslist57788$_37foldr1480512, %struct.ScmObj** %stackaddr$prim59482, align 8
%stackaddr$prim59483 = alloca %struct.ScmObj*, align 8
%argslist57788$_37foldr1480513 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48221, %struct.ScmObj* %argslist57788$_37foldr1480512)
store volatile %struct.ScmObj* %argslist57788$_37foldr1480513, %struct.ScmObj** %stackaddr$prim59483, align 8
%stackaddr$prim59484 = alloca %struct.ScmObj*, align 8
%argslist57788$_37foldr1480514 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48470, %struct.ScmObj* %argslist57788$_37foldr1480513)
store volatile %struct.ScmObj* %argslist57788$_37foldr1480514, %struct.ScmObj** %stackaddr$prim59484, align 8
%clofunc59485 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148051)
musttail call tailcc void %clofunc59485(%struct.ScmObj* %_37foldr148051, %struct.ScmObj* %argslist57788$_37foldr1480514)
ret void
}

define tailcc void @proc_clo$ae49626(%struct.ScmObj* %env$ae49626,%struct.ScmObj* %current_45args57789) {
%stackaddr$env-ref59486 = alloca %struct.ScmObj*, align 8
%f48084 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49626, i64 0)
store %struct.ScmObj* %f48084, %struct.ScmObj** %stackaddr$env-ref59486
%stackaddr$prim59487 = alloca %struct.ScmObj*, align 8
%k48472 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57789)
store volatile %struct.ScmObj* %k48472, %struct.ScmObj** %stackaddr$prim59487, align 8
%stackaddr$prim59488 = alloca %struct.ScmObj*, align 8
%current_45args57790 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57789)
store volatile %struct.ScmObj* %current_45args57790, %struct.ScmObj** %stackaddr$prim59488, align 8
%stackaddr$prim59489 = alloca %struct.ScmObj*, align 8
%v48086 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57790)
store volatile %struct.ScmObj* %v48086, %struct.ScmObj** %stackaddr$prim59489, align 8
%stackaddr$prim59490 = alloca %struct.ScmObj*, align 8
%current_45args57791 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57790)
store volatile %struct.ScmObj* %current_45args57791, %struct.ScmObj** %stackaddr$prim59490, align 8
%stackaddr$prim59491 = alloca %struct.ScmObj*, align 8
%r48085 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57791)
store volatile %struct.ScmObj* %r48085, %struct.ScmObj** %stackaddr$prim59491, align 8
%stackaddr$makeclosure59492 = alloca %struct.ScmObj*, align 8
%fptrToInt59493 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49628 to i64
%ae49628 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt59493)
store volatile %struct.ScmObj* %ae49628, %struct.ScmObj** %stackaddr$makeclosure59492, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49628, %struct.ScmObj* %k48472, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49628, %struct.ScmObj* %r48085, i64 1)
%argslist57797$f480840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59494 = alloca %struct.ScmObj*, align 8
%argslist57797$f480841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v48086, %struct.ScmObj* %argslist57797$f480840)
store volatile %struct.ScmObj* %argslist57797$f480841, %struct.ScmObj** %stackaddr$prim59494, align 8
%stackaddr$prim59495 = alloca %struct.ScmObj*, align 8
%argslist57797$f480842 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49628, %struct.ScmObj* %argslist57797$f480841)
store volatile %struct.ScmObj* %argslist57797$f480842, %struct.ScmObj** %stackaddr$prim59495, align 8
%clofunc59496 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48084)
musttail call tailcc void %clofunc59496(%struct.ScmObj* %f48084, %struct.ScmObj* %argslist57797$f480842)
ret void
}

define tailcc void @proc_clo$ae49628(%struct.ScmObj* %env$ae49628,%struct.ScmObj* %current_45args57793) {
%stackaddr$env-ref59497 = alloca %struct.ScmObj*, align 8
%k48472 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49628, i64 0)
store %struct.ScmObj* %k48472, %struct.ScmObj** %stackaddr$env-ref59497
%stackaddr$env-ref59498 = alloca %struct.ScmObj*, align 8
%r48085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49628, i64 1)
store %struct.ScmObj* %r48085, %struct.ScmObj** %stackaddr$env-ref59498
%stackaddr$prim59499 = alloca %struct.ScmObj*, align 8
%_95k48473 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57793)
store volatile %struct.ScmObj* %_95k48473, %struct.ScmObj** %stackaddr$prim59499, align 8
%stackaddr$prim59500 = alloca %struct.ScmObj*, align 8
%current_45args57794 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57793)
store volatile %struct.ScmObj* %current_45args57794, %struct.ScmObj** %stackaddr$prim59500, align 8
%stackaddr$prim59501 = alloca %struct.ScmObj*, align 8
%anf_45bind48220 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57794)
store volatile %struct.ScmObj* %anf_45bind48220, %struct.ScmObj** %stackaddr$prim59501, align 8
%stackaddr$prim59502 = alloca %struct.ScmObj*, align 8
%cpsprim48474 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48220, %struct.ScmObj* %r48085)
store volatile %struct.ScmObj* %cpsprim48474, %struct.ScmObj** %stackaddr$prim59502, align 8
%ae49633 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57796$k484720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59503 = alloca %struct.ScmObj*, align 8
%argslist57796$k484721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48474, %struct.ScmObj* %argslist57796$k484720)
store volatile %struct.ScmObj* %argslist57796$k484721, %struct.ScmObj** %stackaddr$prim59503, align 8
%stackaddr$prim59504 = alloca %struct.ScmObj*, align 8
%argslist57796$k484722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49633, %struct.ScmObj* %argslist57796$k484721)
store volatile %struct.ScmObj* %argslist57796$k484722, %struct.ScmObj** %stackaddr$prim59504, align 8
%clofunc59505 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48472)
musttail call tailcc void %clofunc59505(%struct.ScmObj* %k48472, %struct.ScmObj* %argslist57796$k484722)
ret void
}

define tailcc void @proc_clo$ae49237(%struct.ScmObj* %env$ae49237,%struct.ScmObj* %current_45args57801) {
%stackaddr$env-ref59506 = alloca %struct.ScmObj*, align 8
%_37foldr148051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49237, i64 0)
store %struct.ScmObj* %_37foldr148051, %struct.ScmObj** %stackaddr$env-ref59506
%stackaddr$env-ref59507 = alloca %struct.ScmObj*, align 8
%_37map148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49237, i64 1)
store %struct.ScmObj* %_37map148047, %struct.ScmObj** %stackaddr$env-ref59507
%stackaddr$prim59508 = alloca %struct.ScmObj*, align 8
%k48475 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57801)
store volatile %struct.ScmObj* %k48475, %struct.ScmObj** %stackaddr$prim59508, align 8
%stackaddr$prim59509 = alloca %struct.ScmObj*, align 8
%current_45args57802 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57801)
store volatile %struct.ScmObj* %current_45args57802, %struct.ScmObj** %stackaddr$prim59509, align 8
%stackaddr$prim59510 = alloca %struct.ScmObj*, align 8
%_37foldr48057 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57802)
store volatile %struct.ScmObj* %_37foldr48057, %struct.ScmObj** %stackaddr$prim59510, align 8
%ae49239 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59511 = alloca %struct.ScmObj*, align 8
%fptrToInt59512 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49240 to i64
%ae49240 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt59512)
store volatile %struct.ScmObj* %ae49240, %struct.ScmObj** %stackaddr$makeclosure59511, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49240, %struct.ScmObj* %_37foldr48057, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49240, %struct.ScmObj* %_37foldr148051, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49240, %struct.ScmObj* %_37map148047, i64 2)
%argslist57859$k484750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59513 = alloca %struct.ScmObj*, align 8
%argslist57859$k484751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49240, %struct.ScmObj* %argslist57859$k484750)
store volatile %struct.ScmObj* %argslist57859$k484751, %struct.ScmObj** %stackaddr$prim59513, align 8
%stackaddr$prim59514 = alloca %struct.ScmObj*, align 8
%argslist57859$k484752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49239, %struct.ScmObj* %argslist57859$k484751)
store volatile %struct.ScmObj* %argslist57859$k484752, %struct.ScmObj** %stackaddr$prim59514, align 8
%clofunc59515 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48475)
musttail call tailcc void %clofunc59515(%struct.ScmObj* %k48475, %struct.ScmObj* %argslist57859$k484752)
ret void
}

define tailcc void @proc_clo$ae49240(%struct.ScmObj* %env$ae49240,%struct.ScmObj* %args4805848476) {
%stackaddr$env-ref59516 = alloca %struct.ScmObj*, align 8
%_37foldr48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49240, i64 0)
store %struct.ScmObj* %_37foldr48057, %struct.ScmObj** %stackaddr$env-ref59516
%stackaddr$env-ref59517 = alloca %struct.ScmObj*, align 8
%_37foldr148051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49240, i64 1)
store %struct.ScmObj* %_37foldr148051, %struct.ScmObj** %stackaddr$env-ref59517
%stackaddr$env-ref59518 = alloca %struct.ScmObj*, align 8
%_37map148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49240, i64 2)
store %struct.ScmObj* %_37map148047, %struct.ScmObj** %stackaddr$env-ref59518
%stackaddr$prim59519 = alloca %struct.ScmObj*, align 8
%k48477 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4805848476)
store volatile %struct.ScmObj* %k48477, %struct.ScmObj** %stackaddr$prim59519, align 8
%stackaddr$prim59520 = alloca %struct.ScmObj*, align 8
%args48058 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4805848476)
store volatile %struct.ScmObj* %args48058, %struct.ScmObj** %stackaddr$prim59520, align 8
%stackaddr$prim59521 = alloca %struct.ScmObj*, align 8
%f48061 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48058)
store volatile %struct.ScmObj* %f48061, %struct.ScmObj** %stackaddr$prim59521, align 8
%stackaddr$prim59522 = alloca %struct.ScmObj*, align 8
%anf_45bind48207 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48058)
store volatile %struct.ScmObj* %anf_45bind48207, %struct.ScmObj** %stackaddr$prim59522, align 8
%stackaddr$prim59523 = alloca %struct.ScmObj*, align 8
%acc48060 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48207)
store volatile %struct.ScmObj* %acc48060, %struct.ScmObj** %stackaddr$prim59523, align 8
%stackaddr$prim59524 = alloca %struct.ScmObj*, align 8
%anf_45bind48208 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48058)
store volatile %struct.ScmObj* %anf_45bind48208, %struct.ScmObj** %stackaddr$prim59524, align 8
%stackaddr$prim59525 = alloca %struct.ScmObj*, align 8
%lsts48059 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48208)
store volatile %struct.ScmObj* %lsts48059, %struct.ScmObj** %stackaddr$prim59525, align 8
%stackaddr$makeclosure59526 = alloca %struct.ScmObj*, align 8
%fptrToInt59527 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49248 to i64
%ae49248 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt59527)
store volatile %struct.ScmObj* %ae49248, %struct.ScmObj** %stackaddr$makeclosure59526, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49248, %struct.ScmObj* %f48061, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49248, %struct.ScmObj* %k48477, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49248, %struct.ScmObj* %_37foldr48057, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49248, %struct.ScmObj* %_37foldr148051, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49248, %struct.ScmObj* %_37map148047, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49248, %struct.ScmObj* %acc48060, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49248, %struct.ScmObj* %lsts48059, i64 6)
%ae49249 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59528 = alloca %struct.ScmObj*, align 8
%fptrToInt59529 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49250 to i64
%ae49250 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt59529)
store volatile %struct.ScmObj* %ae49250, %struct.ScmObj** %stackaddr$makeclosure59528, align 8
%argslist57858$ae492480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59530 = alloca %struct.ScmObj*, align 8
%argslist57858$ae492481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49250, %struct.ScmObj* %argslist57858$ae492480)
store volatile %struct.ScmObj* %argslist57858$ae492481, %struct.ScmObj** %stackaddr$prim59530, align 8
%stackaddr$prim59531 = alloca %struct.ScmObj*, align 8
%argslist57858$ae492482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49249, %struct.ScmObj* %argslist57858$ae492481)
store volatile %struct.ScmObj* %argslist57858$ae492482, %struct.ScmObj** %stackaddr$prim59531, align 8
%clofunc59532 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49248)
musttail call tailcc void %clofunc59532(%struct.ScmObj* %ae49248, %struct.ScmObj* %argslist57858$ae492482)
ret void
}

define tailcc void @proc_clo$ae49248(%struct.ScmObj* %env$ae49248,%struct.ScmObj* %current_45args57804) {
%stackaddr$env-ref59533 = alloca %struct.ScmObj*, align 8
%f48061 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49248, i64 0)
store %struct.ScmObj* %f48061, %struct.ScmObj** %stackaddr$env-ref59533
%stackaddr$env-ref59534 = alloca %struct.ScmObj*, align 8
%k48477 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49248, i64 1)
store %struct.ScmObj* %k48477, %struct.ScmObj** %stackaddr$env-ref59534
%stackaddr$env-ref59535 = alloca %struct.ScmObj*, align 8
%_37foldr48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49248, i64 2)
store %struct.ScmObj* %_37foldr48057, %struct.ScmObj** %stackaddr$env-ref59535
%stackaddr$env-ref59536 = alloca %struct.ScmObj*, align 8
%_37foldr148051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49248, i64 3)
store %struct.ScmObj* %_37foldr148051, %struct.ScmObj** %stackaddr$env-ref59536
%stackaddr$env-ref59537 = alloca %struct.ScmObj*, align 8
%_37map148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49248, i64 4)
store %struct.ScmObj* %_37map148047, %struct.ScmObj** %stackaddr$env-ref59537
%stackaddr$env-ref59538 = alloca %struct.ScmObj*, align 8
%acc48060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49248, i64 5)
store %struct.ScmObj* %acc48060, %struct.ScmObj** %stackaddr$env-ref59538
%stackaddr$env-ref59539 = alloca %struct.ScmObj*, align 8
%lsts48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49248, i64 6)
store %struct.ScmObj* %lsts48059, %struct.ScmObj** %stackaddr$env-ref59539
%stackaddr$prim59540 = alloca %struct.ScmObj*, align 8
%_95k48478 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57804)
store volatile %struct.ScmObj* %_95k48478, %struct.ScmObj** %stackaddr$prim59540, align 8
%stackaddr$prim59541 = alloca %struct.ScmObj*, align 8
%current_45args57805 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57804)
store volatile %struct.ScmObj* %current_45args57805, %struct.ScmObj** %stackaddr$prim59541, align 8
%stackaddr$prim59542 = alloca %struct.ScmObj*, align 8
%anf_45bind48209 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57805)
store volatile %struct.ScmObj* %anf_45bind48209, %struct.ScmObj** %stackaddr$prim59542, align 8
%stackaddr$makeclosure59543 = alloca %struct.ScmObj*, align 8
%fptrToInt59544 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49280 to i64
%ae49280 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt59544)
store volatile %struct.ScmObj* %ae49280, %struct.ScmObj** %stackaddr$makeclosure59543, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49280, %struct.ScmObj* %f48061, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49280, %struct.ScmObj* %k48477, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49280, %struct.ScmObj* %_37foldr48057, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49280, %struct.ScmObj* %_37foldr148051, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49280, %struct.ScmObj* %_37map148047, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49280, %struct.ScmObj* %acc48060, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49280, %struct.ScmObj* %lsts48059, i64 6)
%ae49282 = call %struct.ScmObj* @const_init_false()
%argslist57851$_37foldr1480510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59545 = alloca %struct.ScmObj*, align 8
%argslist57851$_37foldr1480511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48059, %struct.ScmObj* %argslist57851$_37foldr1480510)
store volatile %struct.ScmObj* %argslist57851$_37foldr1480511, %struct.ScmObj** %stackaddr$prim59545, align 8
%stackaddr$prim59546 = alloca %struct.ScmObj*, align 8
%argslist57851$_37foldr1480512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49282, %struct.ScmObj* %argslist57851$_37foldr1480511)
store volatile %struct.ScmObj* %argslist57851$_37foldr1480512, %struct.ScmObj** %stackaddr$prim59546, align 8
%stackaddr$prim59547 = alloca %struct.ScmObj*, align 8
%argslist57851$_37foldr1480513 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48209, %struct.ScmObj* %argslist57851$_37foldr1480512)
store volatile %struct.ScmObj* %argslist57851$_37foldr1480513, %struct.ScmObj** %stackaddr$prim59547, align 8
%stackaddr$prim59548 = alloca %struct.ScmObj*, align 8
%argslist57851$_37foldr1480514 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49280, %struct.ScmObj* %argslist57851$_37foldr1480513)
store volatile %struct.ScmObj* %argslist57851$_37foldr1480514, %struct.ScmObj** %stackaddr$prim59548, align 8
%clofunc59549 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148051)
musttail call tailcc void %clofunc59549(%struct.ScmObj* %_37foldr148051, %struct.ScmObj* %argslist57851$_37foldr1480514)
ret void
}

define tailcc void @proc_clo$ae49280(%struct.ScmObj* %env$ae49280,%struct.ScmObj* %current_45args57807) {
%stackaddr$env-ref59550 = alloca %struct.ScmObj*, align 8
%f48061 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49280, i64 0)
store %struct.ScmObj* %f48061, %struct.ScmObj** %stackaddr$env-ref59550
%stackaddr$env-ref59551 = alloca %struct.ScmObj*, align 8
%k48477 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49280, i64 1)
store %struct.ScmObj* %k48477, %struct.ScmObj** %stackaddr$env-ref59551
%stackaddr$env-ref59552 = alloca %struct.ScmObj*, align 8
%_37foldr48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49280, i64 2)
store %struct.ScmObj* %_37foldr48057, %struct.ScmObj** %stackaddr$env-ref59552
%stackaddr$env-ref59553 = alloca %struct.ScmObj*, align 8
%_37foldr148051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49280, i64 3)
store %struct.ScmObj* %_37foldr148051, %struct.ScmObj** %stackaddr$env-ref59553
%stackaddr$env-ref59554 = alloca %struct.ScmObj*, align 8
%_37map148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49280, i64 4)
store %struct.ScmObj* %_37map148047, %struct.ScmObj** %stackaddr$env-ref59554
%stackaddr$env-ref59555 = alloca %struct.ScmObj*, align 8
%acc48060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49280, i64 5)
store %struct.ScmObj* %acc48060, %struct.ScmObj** %stackaddr$env-ref59555
%stackaddr$env-ref59556 = alloca %struct.ScmObj*, align 8
%lsts48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49280, i64 6)
store %struct.ScmObj* %lsts48059, %struct.ScmObj** %stackaddr$env-ref59556
%stackaddr$prim59557 = alloca %struct.ScmObj*, align 8
%_95k48479 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57807)
store volatile %struct.ScmObj* %_95k48479, %struct.ScmObj** %stackaddr$prim59557, align 8
%stackaddr$prim59558 = alloca %struct.ScmObj*, align 8
%current_45args57808 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57807)
store volatile %struct.ScmObj* %current_45args57808, %struct.ScmObj** %stackaddr$prim59558, align 8
%stackaddr$prim59559 = alloca %struct.ScmObj*, align 8
%anf_45bind48210 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57808)
store volatile %struct.ScmObj* %anf_45bind48210, %struct.ScmObj** %stackaddr$prim59559, align 8
%truthy$cmp59560 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48210)
%cmp$cmp59560 = icmp eq i64 %truthy$cmp59560, 1
br i1 %cmp$cmp59560, label %truebranch$cmp59560, label %falsebranch$cmp59560
truebranch$cmp59560:
%ae49291 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57810$k484770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59561 = alloca %struct.ScmObj*, align 8
%argslist57810$k484771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48060, %struct.ScmObj* %argslist57810$k484770)
store volatile %struct.ScmObj* %argslist57810$k484771, %struct.ScmObj** %stackaddr$prim59561, align 8
%stackaddr$prim59562 = alloca %struct.ScmObj*, align 8
%argslist57810$k484772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49291, %struct.ScmObj* %argslist57810$k484771)
store volatile %struct.ScmObj* %argslist57810$k484772, %struct.ScmObj** %stackaddr$prim59562, align 8
%clofunc59563 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48477)
musttail call tailcc void %clofunc59563(%struct.ScmObj* %k48477, %struct.ScmObj* %argslist57810$k484772)
ret void
falsebranch$cmp59560:
%stackaddr$makeclosure59564 = alloca %struct.ScmObj*, align 8
%fptrToInt59565 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49296 to i64
%ae49296 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt59565)
store volatile %struct.ScmObj* %ae49296, %struct.ScmObj** %stackaddr$makeclosure59564, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49296, %struct.ScmObj* %f48061, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49296, %struct.ScmObj* %k48477, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49296, %struct.ScmObj* %_37foldr48057, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49296, %struct.ScmObj* %_37foldr148051, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49296, %struct.ScmObj* %_37map148047, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49296, %struct.ScmObj* %acc48060, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49296, %struct.ScmObj* %lsts48059, i64 6)
%ae49297 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59566 = alloca %struct.ScmObj*, align 8
%fptrToInt59567 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49298 to i64
%ae49298 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt59567)
store volatile %struct.ScmObj* %ae49298, %struct.ScmObj** %stackaddr$makeclosure59566, align 8
%argslist57850$ae492960 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59568 = alloca %struct.ScmObj*, align 8
%argslist57850$ae492961 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49298, %struct.ScmObj* %argslist57850$ae492960)
store volatile %struct.ScmObj* %argslist57850$ae492961, %struct.ScmObj** %stackaddr$prim59568, align 8
%stackaddr$prim59569 = alloca %struct.ScmObj*, align 8
%argslist57850$ae492962 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49297, %struct.ScmObj* %argslist57850$ae492961)
store volatile %struct.ScmObj* %argslist57850$ae492962, %struct.ScmObj** %stackaddr$prim59569, align 8
%clofunc59570 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49296)
musttail call tailcc void %clofunc59570(%struct.ScmObj* %ae49296, %struct.ScmObj* %argslist57850$ae492962)
ret void
}

define tailcc void @proc_clo$ae49296(%struct.ScmObj* %env$ae49296,%struct.ScmObj* %current_45args57811) {
%stackaddr$env-ref59571 = alloca %struct.ScmObj*, align 8
%f48061 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49296, i64 0)
store %struct.ScmObj* %f48061, %struct.ScmObj** %stackaddr$env-ref59571
%stackaddr$env-ref59572 = alloca %struct.ScmObj*, align 8
%k48477 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49296, i64 1)
store %struct.ScmObj* %k48477, %struct.ScmObj** %stackaddr$env-ref59572
%stackaddr$env-ref59573 = alloca %struct.ScmObj*, align 8
%_37foldr48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49296, i64 2)
store %struct.ScmObj* %_37foldr48057, %struct.ScmObj** %stackaddr$env-ref59573
%stackaddr$env-ref59574 = alloca %struct.ScmObj*, align 8
%_37foldr148051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49296, i64 3)
store %struct.ScmObj* %_37foldr148051, %struct.ScmObj** %stackaddr$env-ref59574
%stackaddr$env-ref59575 = alloca %struct.ScmObj*, align 8
%_37map148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49296, i64 4)
store %struct.ScmObj* %_37map148047, %struct.ScmObj** %stackaddr$env-ref59575
%stackaddr$env-ref59576 = alloca %struct.ScmObj*, align 8
%acc48060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49296, i64 5)
store %struct.ScmObj* %acc48060, %struct.ScmObj** %stackaddr$env-ref59576
%stackaddr$env-ref59577 = alloca %struct.ScmObj*, align 8
%lsts48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49296, i64 6)
store %struct.ScmObj* %lsts48059, %struct.ScmObj** %stackaddr$env-ref59577
%stackaddr$prim59578 = alloca %struct.ScmObj*, align 8
%_95k48480 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57811)
store volatile %struct.ScmObj* %_95k48480, %struct.ScmObj** %stackaddr$prim59578, align 8
%stackaddr$prim59579 = alloca %struct.ScmObj*, align 8
%current_45args57812 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57811)
store volatile %struct.ScmObj* %current_45args57812, %struct.ScmObj** %stackaddr$prim59579, align 8
%stackaddr$prim59580 = alloca %struct.ScmObj*, align 8
%anf_45bind48211 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57812)
store volatile %struct.ScmObj* %anf_45bind48211, %struct.ScmObj** %stackaddr$prim59580, align 8
%stackaddr$makeclosure59581 = alloca %struct.ScmObj*, align 8
%fptrToInt59582 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49317 to i64
%ae49317 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt59582)
store volatile %struct.ScmObj* %ae49317, %struct.ScmObj** %stackaddr$makeclosure59581, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49317, %struct.ScmObj* %f48061, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49317, %struct.ScmObj* %k48477, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49317, %struct.ScmObj* %_37foldr48057, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49317, %struct.ScmObj* %_37foldr148051, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49317, %struct.ScmObj* %_37map148047, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49317, %struct.ScmObj* %acc48060, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49317, %struct.ScmObj* %lsts48059, i64 6)
%argslist57845$_37map1480470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59583 = alloca %struct.ScmObj*, align 8
%argslist57845$_37map1480471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48059, %struct.ScmObj* %argslist57845$_37map1480470)
store volatile %struct.ScmObj* %argslist57845$_37map1480471, %struct.ScmObj** %stackaddr$prim59583, align 8
%stackaddr$prim59584 = alloca %struct.ScmObj*, align 8
%argslist57845$_37map1480472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48211, %struct.ScmObj* %argslist57845$_37map1480471)
store volatile %struct.ScmObj* %argslist57845$_37map1480472, %struct.ScmObj** %stackaddr$prim59584, align 8
%stackaddr$prim59585 = alloca %struct.ScmObj*, align 8
%argslist57845$_37map1480473 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49317, %struct.ScmObj* %argslist57845$_37map1480472)
store volatile %struct.ScmObj* %argslist57845$_37map1480473, %struct.ScmObj** %stackaddr$prim59585, align 8
%clofunc59586 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148047)
musttail call tailcc void %clofunc59586(%struct.ScmObj* %_37map148047, %struct.ScmObj* %argslist57845$_37map1480473)
ret void
}

define tailcc void @proc_clo$ae49317(%struct.ScmObj* %env$ae49317,%struct.ScmObj* %current_45args57814) {
%stackaddr$env-ref59587 = alloca %struct.ScmObj*, align 8
%f48061 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49317, i64 0)
store %struct.ScmObj* %f48061, %struct.ScmObj** %stackaddr$env-ref59587
%stackaddr$env-ref59588 = alloca %struct.ScmObj*, align 8
%k48477 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49317, i64 1)
store %struct.ScmObj* %k48477, %struct.ScmObj** %stackaddr$env-ref59588
%stackaddr$env-ref59589 = alloca %struct.ScmObj*, align 8
%_37foldr48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49317, i64 2)
store %struct.ScmObj* %_37foldr48057, %struct.ScmObj** %stackaddr$env-ref59589
%stackaddr$env-ref59590 = alloca %struct.ScmObj*, align 8
%_37foldr148051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49317, i64 3)
store %struct.ScmObj* %_37foldr148051, %struct.ScmObj** %stackaddr$env-ref59590
%stackaddr$env-ref59591 = alloca %struct.ScmObj*, align 8
%_37map148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49317, i64 4)
store %struct.ScmObj* %_37map148047, %struct.ScmObj** %stackaddr$env-ref59591
%stackaddr$env-ref59592 = alloca %struct.ScmObj*, align 8
%acc48060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49317, i64 5)
store %struct.ScmObj* %acc48060, %struct.ScmObj** %stackaddr$env-ref59592
%stackaddr$env-ref59593 = alloca %struct.ScmObj*, align 8
%lsts48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49317, i64 6)
store %struct.ScmObj* %lsts48059, %struct.ScmObj** %stackaddr$env-ref59593
%stackaddr$prim59594 = alloca %struct.ScmObj*, align 8
%_95k48481 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57814)
store volatile %struct.ScmObj* %_95k48481, %struct.ScmObj** %stackaddr$prim59594, align 8
%stackaddr$prim59595 = alloca %struct.ScmObj*, align 8
%current_45args57815 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57814)
store volatile %struct.ScmObj* %current_45args57815, %struct.ScmObj** %stackaddr$prim59595, align 8
%stackaddr$prim59596 = alloca %struct.ScmObj*, align 8
%lsts_4348066 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57815)
store volatile %struct.ScmObj* %lsts_4348066, %struct.ScmObj** %stackaddr$prim59596, align 8
%stackaddr$makeclosure59597 = alloca %struct.ScmObj*, align 8
%fptrToInt59598 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49320 to i64
%ae49320 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt59598)
store volatile %struct.ScmObj* %ae49320, %struct.ScmObj** %stackaddr$makeclosure59597, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49320, %struct.ScmObj* %f48061, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49320, %struct.ScmObj* %k48477, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49320, %struct.ScmObj* %_37foldr48057, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49320, %struct.ScmObj* %_37foldr148051, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49320, %struct.ScmObj* %lsts_4348066, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49320, %struct.ScmObj* %_37map148047, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49320, %struct.ScmObj* %acc48060, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae49320, %struct.ScmObj* %lsts48059, i64 7)
%ae49321 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59599 = alloca %struct.ScmObj*, align 8
%fptrToInt59600 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49322 to i64
%ae49322 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt59600)
store volatile %struct.ScmObj* %ae49322, %struct.ScmObj** %stackaddr$makeclosure59599, align 8
%argslist57844$ae493200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59601 = alloca %struct.ScmObj*, align 8
%argslist57844$ae493201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49322, %struct.ScmObj* %argslist57844$ae493200)
store volatile %struct.ScmObj* %argslist57844$ae493201, %struct.ScmObj** %stackaddr$prim59601, align 8
%stackaddr$prim59602 = alloca %struct.ScmObj*, align 8
%argslist57844$ae493202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49321, %struct.ScmObj* %argslist57844$ae493201)
store volatile %struct.ScmObj* %argslist57844$ae493202, %struct.ScmObj** %stackaddr$prim59602, align 8
%clofunc59603 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49320)
musttail call tailcc void %clofunc59603(%struct.ScmObj* %ae49320, %struct.ScmObj* %argslist57844$ae493202)
ret void
}

define tailcc void @proc_clo$ae49320(%struct.ScmObj* %env$ae49320,%struct.ScmObj* %current_45args57817) {
%stackaddr$env-ref59604 = alloca %struct.ScmObj*, align 8
%f48061 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49320, i64 0)
store %struct.ScmObj* %f48061, %struct.ScmObj** %stackaddr$env-ref59604
%stackaddr$env-ref59605 = alloca %struct.ScmObj*, align 8
%k48477 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49320, i64 1)
store %struct.ScmObj* %k48477, %struct.ScmObj** %stackaddr$env-ref59605
%stackaddr$env-ref59606 = alloca %struct.ScmObj*, align 8
%_37foldr48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49320, i64 2)
store %struct.ScmObj* %_37foldr48057, %struct.ScmObj** %stackaddr$env-ref59606
%stackaddr$env-ref59607 = alloca %struct.ScmObj*, align 8
%_37foldr148051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49320, i64 3)
store %struct.ScmObj* %_37foldr148051, %struct.ScmObj** %stackaddr$env-ref59607
%stackaddr$env-ref59608 = alloca %struct.ScmObj*, align 8
%lsts_4348066 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49320, i64 4)
store %struct.ScmObj* %lsts_4348066, %struct.ScmObj** %stackaddr$env-ref59608
%stackaddr$env-ref59609 = alloca %struct.ScmObj*, align 8
%_37map148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49320, i64 5)
store %struct.ScmObj* %_37map148047, %struct.ScmObj** %stackaddr$env-ref59609
%stackaddr$env-ref59610 = alloca %struct.ScmObj*, align 8
%acc48060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49320, i64 6)
store %struct.ScmObj* %acc48060, %struct.ScmObj** %stackaddr$env-ref59610
%stackaddr$env-ref59611 = alloca %struct.ScmObj*, align 8
%lsts48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49320, i64 7)
store %struct.ScmObj* %lsts48059, %struct.ScmObj** %stackaddr$env-ref59611
%stackaddr$prim59612 = alloca %struct.ScmObj*, align 8
%_95k48482 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57817)
store volatile %struct.ScmObj* %_95k48482, %struct.ScmObj** %stackaddr$prim59612, align 8
%stackaddr$prim59613 = alloca %struct.ScmObj*, align 8
%current_45args57818 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57817)
store volatile %struct.ScmObj* %current_45args57818, %struct.ScmObj** %stackaddr$prim59613, align 8
%stackaddr$prim59614 = alloca %struct.ScmObj*, align 8
%anf_45bind48212 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57818)
store volatile %struct.ScmObj* %anf_45bind48212, %struct.ScmObj** %stackaddr$prim59614, align 8
%stackaddr$makeclosure59615 = alloca %struct.ScmObj*, align 8
%fptrToInt59616 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49341 to i64
%ae49341 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt59616)
store volatile %struct.ScmObj* %ae49341, %struct.ScmObj** %stackaddr$makeclosure59615, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49341, %struct.ScmObj* %f48061, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49341, %struct.ScmObj* %k48477, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49341, %struct.ScmObj* %_37foldr48057, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49341, %struct.ScmObj* %_37foldr148051, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49341, %struct.ScmObj* %lsts_4348066, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49341, %struct.ScmObj* %acc48060, i64 5)
%argslist57839$_37map1480470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59617 = alloca %struct.ScmObj*, align 8
%argslist57839$_37map1480471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48059, %struct.ScmObj* %argslist57839$_37map1480470)
store volatile %struct.ScmObj* %argslist57839$_37map1480471, %struct.ScmObj** %stackaddr$prim59617, align 8
%stackaddr$prim59618 = alloca %struct.ScmObj*, align 8
%argslist57839$_37map1480472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48212, %struct.ScmObj* %argslist57839$_37map1480471)
store volatile %struct.ScmObj* %argslist57839$_37map1480472, %struct.ScmObj** %stackaddr$prim59618, align 8
%stackaddr$prim59619 = alloca %struct.ScmObj*, align 8
%argslist57839$_37map1480473 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49341, %struct.ScmObj* %argslist57839$_37map1480472)
store volatile %struct.ScmObj* %argslist57839$_37map1480473, %struct.ScmObj** %stackaddr$prim59619, align 8
%clofunc59620 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148047)
musttail call tailcc void %clofunc59620(%struct.ScmObj* %_37map148047, %struct.ScmObj* %argslist57839$_37map1480473)
ret void
}

define tailcc void @proc_clo$ae49341(%struct.ScmObj* %env$ae49341,%struct.ScmObj* %current_45args57820) {
%stackaddr$env-ref59621 = alloca %struct.ScmObj*, align 8
%f48061 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49341, i64 0)
store %struct.ScmObj* %f48061, %struct.ScmObj** %stackaddr$env-ref59621
%stackaddr$env-ref59622 = alloca %struct.ScmObj*, align 8
%k48477 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49341, i64 1)
store %struct.ScmObj* %k48477, %struct.ScmObj** %stackaddr$env-ref59622
%stackaddr$env-ref59623 = alloca %struct.ScmObj*, align 8
%_37foldr48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49341, i64 2)
store %struct.ScmObj* %_37foldr48057, %struct.ScmObj** %stackaddr$env-ref59623
%stackaddr$env-ref59624 = alloca %struct.ScmObj*, align 8
%_37foldr148051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49341, i64 3)
store %struct.ScmObj* %_37foldr148051, %struct.ScmObj** %stackaddr$env-ref59624
%stackaddr$env-ref59625 = alloca %struct.ScmObj*, align 8
%lsts_4348066 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49341, i64 4)
store %struct.ScmObj* %lsts_4348066, %struct.ScmObj** %stackaddr$env-ref59625
%stackaddr$env-ref59626 = alloca %struct.ScmObj*, align 8
%acc48060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49341, i64 5)
store %struct.ScmObj* %acc48060, %struct.ScmObj** %stackaddr$env-ref59626
%stackaddr$prim59627 = alloca %struct.ScmObj*, align 8
%_95k48483 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57820)
store volatile %struct.ScmObj* %_95k48483, %struct.ScmObj** %stackaddr$prim59627, align 8
%stackaddr$prim59628 = alloca %struct.ScmObj*, align 8
%current_45args57821 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57820)
store volatile %struct.ScmObj* %current_45args57821, %struct.ScmObj** %stackaddr$prim59628, align 8
%stackaddr$prim59629 = alloca %struct.ScmObj*, align 8
%vs48064 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57821)
store volatile %struct.ScmObj* %vs48064, %struct.ScmObj** %stackaddr$prim59629, align 8
%stackaddr$makeclosure59630 = alloca %struct.ScmObj*, align 8
%fptrToInt59631 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49344 to i64
%ae49344 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt59631)
store volatile %struct.ScmObj* %ae49344, %struct.ScmObj** %stackaddr$makeclosure59630, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49344, %struct.ScmObj* %f48061, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49344, %struct.ScmObj* %k48477, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49344, %struct.ScmObj* %_37foldr48057, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49344, %struct.ScmObj* %_37foldr148051, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49344, %struct.ScmObj* %lsts_4348066, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49344, %struct.ScmObj* %vs48064, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49344, %struct.ScmObj* %acc48060, i64 6)
%ae49345 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59632 = alloca %struct.ScmObj*, align 8
%fptrToInt59633 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49346 to i64
%ae49346 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt59633)
store volatile %struct.ScmObj* %ae49346, %struct.ScmObj** %stackaddr$makeclosure59632, align 8
%argslist57838$ae493440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59634 = alloca %struct.ScmObj*, align 8
%argslist57838$ae493441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49346, %struct.ScmObj* %argslist57838$ae493440)
store volatile %struct.ScmObj* %argslist57838$ae493441, %struct.ScmObj** %stackaddr$prim59634, align 8
%stackaddr$prim59635 = alloca %struct.ScmObj*, align 8
%argslist57838$ae493442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49345, %struct.ScmObj* %argslist57838$ae493441)
store volatile %struct.ScmObj* %argslist57838$ae493442, %struct.ScmObj** %stackaddr$prim59635, align 8
%clofunc59636 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49344)
musttail call tailcc void %clofunc59636(%struct.ScmObj* %ae49344, %struct.ScmObj* %argslist57838$ae493442)
ret void
}

define tailcc void @proc_clo$ae49344(%struct.ScmObj* %env$ae49344,%struct.ScmObj* %current_45args57823) {
%stackaddr$env-ref59637 = alloca %struct.ScmObj*, align 8
%f48061 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49344, i64 0)
store %struct.ScmObj* %f48061, %struct.ScmObj** %stackaddr$env-ref59637
%stackaddr$env-ref59638 = alloca %struct.ScmObj*, align 8
%k48477 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49344, i64 1)
store %struct.ScmObj* %k48477, %struct.ScmObj** %stackaddr$env-ref59638
%stackaddr$env-ref59639 = alloca %struct.ScmObj*, align 8
%_37foldr48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49344, i64 2)
store %struct.ScmObj* %_37foldr48057, %struct.ScmObj** %stackaddr$env-ref59639
%stackaddr$env-ref59640 = alloca %struct.ScmObj*, align 8
%_37foldr148051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49344, i64 3)
store %struct.ScmObj* %_37foldr148051, %struct.ScmObj** %stackaddr$env-ref59640
%stackaddr$env-ref59641 = alloca %struct.ScmObj*, align 8
%lsts_4348066 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49344, i64 4)
store %struct.ScmObj* %lsts_4348066, %struct.ScmObj** %stackaddr$env-ref59641
%stackaddr$env-ref59642 = alloca %struct.ScmObj*, align 8
%vs48064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49344, i64 5)
store %struct.ScmObj* %vs48064, %struct.ScmObj** %stackaddr$env-ref59642
%stackaddr$env-ref59643 = alloca %struct.ScmObj*, align 8
%acc48060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49344, i64 6)
store %struct.ScmObj* %acc48060, %struct.ScmObj** %stackaddr$env-ref59643
%stackaddr$prim59644 = alloca %struct.ScmObj*, align 8
%_95k48484 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57823)
store volatile %struct.ScmObj* %_95k48484, %struct.ScmObj** %stackaddr$prim59644, align 8
%stackaddr$prim59645 = alloca %struct.ScmObj*, align 8
%current_45args57824 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57823)
store volatile %struct.ScmObj* %current_45args57824, %struct.ScmObj** %stackaddr$prim59645, align 8
%stackaddr$prim59646 = alloca %struct.ScmObj*, align 8
%anf_45bind48213 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57824)
store volatile %struct.ScmObj* %anf_45bind48213, %struct.ScmObj** %stackaddr$prim59646, align 8
%stackaddr$prim59647 = alloca %struct.ScmObj*, align 8
%anf_45bind48214 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48060, %struct.ScmObj* %lsts_4348066)
store volatile %struct.ScmObj* %anf_45bind48214, %struct.ScmObj** %stackaddr$prim59647, align 8
%stackaddr$prim59648 = alloca %struct.ScmObj*, align 8
%anf_45bind48215 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48061, %struct.ScmObj* %anf_45bind48214)
store volatile %struct.ScmObj* %anf_45bind48215, %struct.ScmObj** %stackaddr$prim59648, align 8
%stackaddr$makeclosure59649 = alloca %struct.ScmObj*, align 8
%fptrToInt59650 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49370 to i64
%ae49370 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt59650)
store volatile %struct.ScmObj* %ae49370, %struct.ScmObj** %stackaddr$makeclosure59649, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49370, %struct.ScmObj* %f48061, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49370, %struct.ScmObj* %k48477, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49370, %struct.ScmObj* %anf_45bind48213, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49370, %struct.ScmObj* %_37foldr148051, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49370, %struct.ScmObj* %vs48064, i64 4)
%stackaddr$prim59651 = alloca %struct.ScmObj*, align 8
%cpsargs48488 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49370, %struct.ScmObj* %anf_45bind48215)
store volatile %struct.ScmObj* %cpsargs48488, %struct.ScmObj** %stackaddr$prim59651, align 8
%clofunc59652 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr48057)
musttail call tailcc void %clofunc59652(%struct.ScmObj* %_37foldr48057, %struct.ScmObj* %cpsargs48488)
ret void
}

define tailcc void @proc_clo$ae49370(%struct.ScmObj* %env$ae49370,%struct.ScmObj* %current_45args57826) {
%stackaddr$env-ref59653 = alloca %struct.ScmObj*, align 8
%f48061 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49370, i64 0)
store %struct.ScmObj* %f48061, %struct.ScmObj** %stackaddr$env-ref59653
%stackaddr$env-ref59654 = alloca %struct.ScmObj*, align 8
%k48477 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49370, i64 1)
store %struct.ScmObj* %k48477, %struct.ScmObj** %stackaddr$env-ref59654
%stackaddr$env-ref59655 = alloca %struct.ScmObj*, align 8
%anf_45bind48213 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49370, i64 2)
store %struct.ScmObj* %anf_45bind48213, %struct.ScmObj** %stackaddr$env-ref59655
%stackaddr$env-ref59656 = alloca %struct.ScmObj*, align 8
%_37foldr148051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49370, i64 3)
store %struct.ScmObj* %_37foldr148051, %struct.ScmObj** %stackaddr$env-ref59656
%stackaddr$env-ref59657 = alloca %struct.ScmObj*, align 8
%vs48064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49370, i64 4)
store %struct.ScmObj* %vs48064, %struct.ScmObj** %stackaddr$env-ref59657
%stackaddr$prim59658 = alloca %struct.ScmObj*, align 8
%_95k48485 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57826)
store volatile %struct.ScmObj* %_95k48485, %struct.ScmObj** %stackaddr$prim59658, align 8
%stackaddr$prim59659 = alloca %struct.ScmObj*, align 8
%current_45args57827 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57826)
store volatile %struct.ScmObj* %current_45args57827, %struct.ScmObj** %stackaddr$prim59659, align 8
%stackaddr$prim59660 = alloca %struct.ScmObj*, align 8
%anf_45bind48216 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57827)
store volatile %struct.ScmObj* %anf_45bind48216, %struct.ScmObj** %stackaddr$prim59660, align 8
%ae49375 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59661 = alloca %struct.ScmObj*, align 8
%anf_45bind48217 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48216, %struct.ScmObj* %ae49375)
store volatile %struct.ScmObj* %anf_45bind48217, %struct.ScmObj** %stackaddr$prim59661, align 8
%stackaddr$makeclosure59662 = alloca %struct.ScmObj*, align 8
%fptrToInt59663 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49377 to i64
%ae49377 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt59663)
store volatile %struct.ScmObj* %ae49377, %struct.ScmObj** %stackaddr$makeclosure59662, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49377, %struct.ScmObj* %f48061, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49377, %struct.ScmObj* %k48477, i64 1)
%argslist57832$_37foldr1480510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59664 = alloca %struct.ScmObj*, align 8
%argslist57832$_37foldr1480511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs48064, %struct.ScmObj* %argslist57832$_37foldr1480510)
store volatile %struct.ScmObj* %argslist57832$_37foldr1480511, %struct.ScmObj** %stackaddr$prim59664, align 8
%stackaddr$prim59665 = alloca %struct.ScmObj*, align 8
%argslist57832$_37foldr1480512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48217, %struct.ScmObj* %argslist57832$_37foldr1480511)
store volatile %struct.ScmObj* %argslist57832$_37foldr1480512, %struct.ScmObj** %stackaddr$prim59665, align 8
%stackaddr$prim59666 = alloca %struct.ScmObj*, align 8
%argslist57832$_37foldr1480513 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48213, %struct.ScmObj* %argslist57832$_37foldr1480512)
store volatile %struct.ScmObj* %argslist57832$_37foldr1480513, %struct.ScmObj** %stackaddr$prim59666, align 8
%stackaddr$prim59667 = alloca %struct.ScmObj*, align 8
%argslist57832$_37foldr1480514 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49377, %struct.ScmObj* %argslist57832$_37foldr1480513)
store volatile %struct.ScmObj* %argslist57832$_37foldr1480514, %struct.ScmObj** %stackaddr$prim59667, align 8
%clofunc59668 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148051)
musttail call tailcc void %clofunc59668(%struct.ScmObj* %_37foldr148051, %struct.ScmObj* %argslist57832$_37foldr1480514)
ret void
}

define tailcc void @proc_clo$ae49377(%struct.ScmObj* %env$ae49377,%struct.ScmObj* %current_45args57829) {
%stackaddr$env-ref59669 = alloca %struct.ScmObj*, align 8
%f48061 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49377, i64 0)
store %struct.ScmObj* %f48061, %struct.ScmObj** %stackaddr$env-ref59669
%stackaddr$env-ref59670 = alloca %struct.ScmObj*, align 8
%k48477 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49377, i64 1)
store %struct.ScmObj* %k48477, %struct.ScmObj** %stackaddr$env-ref59670
%stackaddr$prim59671 = alloca %struct.ScmObj*, align 8
%_95k48486 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57829)
store volatile %struct.ScmObj* %_95k48486, %struct.ScmObj** %stackaddr$prim59671, align 8
%stackaddr$prim59672 = alloca %struct.ScmObj*, align 8
%current_45args57830 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57829)
store volatile %struct.ScmObj* %current_45args57830, %struct.ScmObj** %stackaddr$prim59672, align 8
%stackaddr$prim59673 = alloca %struct.ScmObj*, align 8
%anf_45bind48218 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57830)
store volatile %struct.ScmObj* %anf_45bind48218, %struct.ScmObj** %stackaddr$prim59673, align 8
%stackaddr$prim59674 = alloca %struct.ScmObj*, align 8
%cpsargs48487 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48477, %struct.ScmObj* %anf_45bind48218)
store volatile %struct.ScmObj* %cpsargs48487, %struct.ScmObj** %stackaddr$prim59674, align 8
%clofunc59675 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48061)
musttail call tailcc void %clofunc59675(%struct.ScmObj* %f48061, %struct.ScmObj* %cpsargs48487)
ret void
}

define tailcc void @proc_clo$ae49346(%struct.ScmObj* %env$ae49346,%struct.ScmObj* %current_45args57833) {
%stackaddr$prim59676 = alloca %struct.ScmObj*, align 8
%k48489 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57833)
store volatile %struct.ScmObj* %k48489, %struct.ScmObj** %stackaddr$prim59676, align 8
%stackaddr$prim59677 = alloca %struct.ScmObj*, align 8
%current_45args57834 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57833)
store volatile %struct.ScmObj* %current_45args57834, %struct.ScmObj** %stackaddr$prim59677, align 8
%stackaddr$prim59678 = alloca %struct.ScmObj*, align 8
%a48069 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57834)
store volatile %struct.ScmObj* %a48069, %struct.ScmObj** %stackaddr$prim59678, align 8
%stackaddr$prim59679 = alloca %struct.ScmObj*, align 8
%current_45args57835 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57834)
store volatile %struct.ScmObj* %current_45args57835, %struct.ScmObj** %stackaddr$prim59679, align 8
%stackaddr$prim59680 = alloca %struct.ScmObj*, align 8
%b48068 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57835)
store volatile %struct.ScmObj* %b48068, %struct.ScmObj** %stackaddr$prim59680, align 8
%stackaddr$prim59681 = alloca %struct.ScmObj*, align 8
%cpsprim48490 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a48069, %struct.ScmObj* %b48068)
store volatile %struct.ScmObj* %cpsprim48490, %struct.ScmObj** %stackaddr$prim59681, align 8
%ae49350 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57837$k484890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59682 = alloca %struct.ScmObj*, align 8
%argslist57837$k484891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48490, %struct.ScmObj* %argslist57837$k484890)
store volatile %struct.ScmObj* %argslist57837$k484891, %struct.ScmObj** %stackaddr$prim59682, align 8
%stackaddr$prim59683 = alloca %struct.ScmObj*, align 8
%argslist57837$k484892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49350, %struct.ScmObj* %argslist57837$k484891)
store volatile %struct.ScmObj* %argslist57837$k484892, %struct.ScmObj** %stackaddr$prim59683, align 8
%clofunc59684 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48489)
musttail call tailcc void %clofunc59684(%struct.ScmObj* %k48489, %struct.ScmObj* %argslist57837$k484892)
ret void
}

define tailcc void @proc_clo$ae49322(%struct.ScmObj* %env$ae49322,%struct.ScmObj* %current_45args57840) {
%stackaddr$prim59685 = alloca %struct.ScmObj*, align 8
%k48491 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57840)
store volatile %struct.ScmObj* %k48491, %struct.ScmObj** %stackaddr$prim59685, align 8
%stackaddr$prim59686 = alloca %struct.ScmObj*, align 8
%current_45args57841 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57840)
store volatile %struct.ScmObj* %current_45args57841, %struct.ScmObj** %stackaddr$prim59686, align 8
%stackaddr$prim59687 = alloca %struct.ScmObj*, align 8
%x48065 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57841)
store volatile %struct.ScmObj* %x48065, %struct.ScmObj** %stackaddr$prim59687, align 8
%stackaddr$prim59688 = alloca %struct.ScmObj*, align 8
%cpsprim48492 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48065)
store volatile %struct.ScmObj* %cpsprim48492, %struct.ScmObj** %stackaddr$prim59688, align 8
%ae49325 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57843$k484910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59689 = alloca %struct.ScmObj*, align 8
%argslist57843$k484911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48492, %struct.ScmObj* %argslist57843$k484910)
store volatile %struct.ScmObj* %argslist57843$k484911, %struct.ScmObj** %stackaddr$prim59689, align 8
%stackaddr$prim59690 = alloca %struct.ScmObj*, align 8
%argslist57843$k484912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49325, %struct.ScmObj* %argslist57843$k484911)
store volatile %struct.ScmObj* %argslist57843$k484912, %struct.ScmObj** %stackaddr$prim59690, align 8
%clofunc59691 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48491)
musttail call tailcc void %clofunc59691(%struct.ScmObj* %k48491, %struct.ScmObj* %argslist57843$k484912)
ret void
}

define tailcc void @proc_clo$ae49298(%struct.ScmObj* %env$ae49298,%struct.ScmObj* %current_45args57846) {
%stackaddr$prim59692 = alloca %struct.ScmObj*, align 8
%k48493 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57846)
store volatile %struct.ScmObj* %k48493, %struct.ScmObj** %stackaddr$prim59692, align 8
%stackaddr$prim59693 = alloca %struct.ScmObj*, align 8
%current_45args57847 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57846)
store volatile %struct.ScmObj* %current_45args57847, %struct.ScmObj** %stackaddr$prim59693, align 8
%stackaddr$prim59694 = alloca %struct.ScmObj*, align 8
%x48067 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57847)
store volatile %struct.ScmObj* %x48067, %struct.ScmObj** %stackaddr$prim59694, align 8
%stackaddr$prim59695 = alloca %struct.ScmObj*, align 8
%cpsprim48494 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48067)
store volatile %struct.ScmObj* %cpsprim48494, %struct.ScmObj** %stackaddr$prim59695, align 8
%ae49301 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57849$k484930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59696 = alloca %struct.ScmObj*, align 8
%argslist57849$k484931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48494, %struct.ScmObj* %argslist57849$k484930)
store volatile %struct.ScmObj* %argslist57849$k484931, %struct.ScmObj** %stackaddr$prim59696, align 8
%stackaddr$prim59697 = alloca %struct.ScmObj*, align 8
%argslist57849$k484932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49301, %struct.ScmObj* %argslist57849$k484931)
store volatile %struct.ScmObj* %argslist57849$k484932, %struct.ScmObj** %stackaddr$prim59697, align 8
%clofunc59698 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48493)
musttail call tailcc void %clofunc59698(%struct.ScmObj* %k48493, %struct.ScmObj* %argslist57849$k484932)
ret void
}

define tailcc void @proc_clo$ae49250(%struct.ScmObj* %env$ae49250,%struct.ScmObj* %current_45args57852) {
%stackaddr$prim59699 = alloca %struct.ScmObj*, align 8
%k48495 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57852)
store volatile %struct.ScmObj* %k48495, %struct.ScmObj** %stackaddr$prim59699, align 8
%stackaddr$prim59700 = alloca %struct.ScmObj*, align 8
%current_45args57853 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57852)
store volatile %struct.ScmObj* %current_45args57853, %struct.ScmObj** %stackaddr$prim59700, align 8
%stackaddr$prim59701 = alloca %struct.ScmObj*, align 8
%lst48063 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57853)
store volatile %struct.ScmObj* %lst48063, %struct.ScmObj** %stackaddr$prim59701, align 8
%stackaddr$prim59702 = alloca %struct.ScmObj*, align 8
%current_45args57854 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57853)
store volatile %struct.ScmObj* %current_45args57854, %struct.ScmObj** %stackaddr$prim59702, align 8
%stackaddr$prim59703 = alloca %struct.ScmObj*, align 8
%b48062 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57854)
store volatile %struct.ScmObj* %b48062, %struct.ScmObj** %stackaddr$prim59703, align 8
%truthy$cmp59704 = call i64 @is_truthy_value(%struct.ScmObj* %b48062)
%cmp$cmp59704 = icmp eq i64 %truthy$cmp59704, 1
br i1 %cmp$cmp59704, label %truebranch$cmp59704, label %falsebranch$cmp59704
truebranch$cmp59704:
%ae49253 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57856$k484950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59705 = alloca %struct.ScmObj*, align 8
%argslist57856$k484951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b48062, %struct.ScmObj* %argslist57856$k484950)
store volatile %struct.ScmObj* %argslist57856$k484951, %struct.ScmObj** %stackaddr$prim59705, align 8
%stackaddr$prim59706 = alloca %struct.ScmObj*, align 8
%argslist57856$k484952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49253, %struct.ScmObj* %argslist57856$k484951)
store volatile %struct.ScmObj* %argslist57856$k484952, %struct.ScmObj** %stackaddr$prim59706, align 8
%clofunc59707 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48495)
musttail call tailcc void %clofunc59707(%struct.ScmObj* %k48495, %struct.ScmObj* %argslist57856$k484952)
ret void
falsebranch$cmp59704:
%stackaddr$prim59708 = alloca %struct.ScmObj*, align 8
%cpsprim48496 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48063)
store volatile %struct.ScmObj* %cpsprim48496, %struct.ScmObj** %stackaddr$prim59708, align 8
%ae49260 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57857$k484950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59709 = alloca %struct.ScmObj*, align 8
%argslist57857$k484951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48496, %struct.ScmObj* %argslist57857$k484950)
store volatile %struct.ScmObj* %argslist57857$k484951, %struct.ScmObj** %stackaddr$prim59709, align 8
%stackaddr$prim59710 = alloca %struct.ScmObj*, align 8
%argslist57857$k484952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49260, %struct.ScmObj* %argslist57857$k484951)
store volatile %struct.ScmObj* %argslist57857$k484952, %struct.ScmObj** %stackaddr$prim59710, align 8
%clofunc59711 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48495)
musttail call tailcc void %clofunc59711(%struct.ScmObj* %k48495, %struct.ScmObj* %argslist57857$k484952)
ret void
}

define tailcc void @proc_clo$ae49207(%struct.ScmObj* %env$ae49207,%struct.ScmObj* %current_45args57861) {
%stackaddr$env-ref59712 = alloca %struct.ScmObj*, align 8
%_37length48040 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49207, i64 0)
store %struct.ScmObj* %_37length48040, %struct.ScmObj** %stackaddr$env-ref59712
%stackaddr$env-ref59713 = alloca %struct.ScmObj*, align 8
%_37take48043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49207, i64 1)
store %struct.ScmObj* %_37take48043, %struct.ScmObj** %stackaddr$env-ref59713
%stackaddr$prim59714 = alloca %struct.ScmObj*, align 8
%k48497 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57861)
store volatile %struct.ScmObj* %k48497, %struct.ScmObj** %stackaddr$prim59714, align 8
%stackaddr$prim59715 = alloca %struct.ScmObj*, align 8
%current_45args57862 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57861)
store volatile %struct.ScmObj* %current_45args57862, %struct.ScmObj** %stackaddr$prim59715, align 8
%stackaddr$prim59716 = alloca %struct.ScmObj*, align 8
%lst48072 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57862)
store volatile %struct.ScmObj* %lst48072, %struct.ScmObj** %stackaddr$prim59716, align 8
%stackaddr$prim59717 = alloca %struct.ScmObj*, align 8
%current_45args57863 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57862)
store volatile %struct.ScmObj* %current_45args57863, %struct.ScmObj** %stackaddr$prim59717, align 8
%stackaddr$prim59718 = alloca %struct.ScmObj*, align 8
%n48071 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57863)
store volatile %struct.ScmObj* %n48071, %struct.ScmObj** %stackaddr$prim59718, align 8
%stackaddr$makeclosure59719 = alloca %struct.ScmObj*, align 8
%fptrToInt59720 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49209 to i64
%ae49209 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt59720)
store volatile %struct.ScmObj* %ae49209, %struct.ScmObj** %stackaddr$makeclosure59719, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49209, %struct.ScmObj* %lst48072, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49209, %struct.ScmObj* %n48071, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49209, %struct.ScmObj* %k48497, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49209, %struct.ScmObj* %_37take48043, i64 3)
%argslist57869$_37length480400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59721 = alloca %struct.ScmObj*, align 8
%argslist57869$_37length480401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48072, %struct.ScmObj* %argslist57869$_37length480400)
store volatile %struct.ScmObj* %argslist57869$_37length480401, %struct.ScmObj** %stackaddr$prim59721, align 8
%stackaddr$prim59722 = alloca %struct.ScmObj*, align 8
%argslist57869$_37length480402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49209, %struct.ScmObj* %argslist57869$_37length480401)
store volatile %struct.ScmObj* %argslist57869$_37length480402, %struct.ScmObj** %stackaddr$prim59722, align 8
%clofunc59723 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length48040)
musttail call tailcc void %clofunc59723(%struct.ScmObj* %_37length48040, %struct.ScmObj* %argslist57869$_37length480402)
ret void
}

define tailcc void @proc_clo$ae49209(%struct.ScmObj* %env$ae49209,%struct.ScmObj* %current_45args57865) {
%stackaddr$env-ref59724 = alloca %struct.ScmObj*, align 8
%lst48072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49209, i64 0)
store %struct.ScmObj* %lst48072, %struct.ScmObj** %stackaddr$env-ref59724
%stackaddr$env-ref59725 = alloca %struct.ScmObj*, align 8
%n48071 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49209, i64 1)
store %struct.ScmObj* %n48071, %struct.ScmObj** %stackaddr$env-ref59725
%stackaddr$env-ref59726 = alloca %struct.ScmObj*, align 8
%k48497 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49209, i64 2)
store %struct.ScmObj* %k48497, %struct.ScmObj** %stackaddr$env-ref59726
%stackaddr$env-ref59727 = alloca %struct.ScmObj*, align 8
%_37take48043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49209, i64 3)
store %struct.ScmObj* %_37take48043, %struct.ScmObj** %stackaddr$env-ref59727
%stackaddr$prim59728 = alloca %struct.ScmObj*, align 8
%_95k48498 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57865)
store volatile %struct.ScmObj* %_95k48498, %struct.ScmObj** %stackaddr$prim59728, align 8
%stackaddr$prim59729 = alloca %struct.ScmObj*, align 8
%current_45args57866 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57865)
store volatile %struct.ScmObj* %current_45args57866, %struct.ScmObj** %stackaddr$prim59729, align 8
%stackaddr$prim59730 = alloca %struct.ScmObj*, align 8
%anf_45bind48205 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57866)
store volatile %struct.ScmObj* %anf_45bind48205, %struct.ScmObj** %stackaddr$prim59730, align 8
%stackaddr$prim59731 = alloca %struct.ScmObj*, align 8
%anf_45bind48206 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind48205, %struct.ScmObj* %n48071)
store volatile %struct.ScmObj* %anf_45bind48206, %struct.ScmObj** %stackaddr$prim59731, align 8
%argslist57868$_37take480430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59732 = alloca %struct.ScmObj*, align 8
%argslist57868$_37take480431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48206, %struct.ScmObj* %argslist57868$_37take480430)
store volatile %struct.ScmObj* %argslist57868$_37take480431, %struct.ScmObj** %stackaddr$prim59732, align 8
%stackaddr$prim59733 = alloca %struct.ScmObj*, align 8
%argslist57868$_37take480432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48072, %struct.ScmObj* %argslist57868$_37take480431)
store volatile %struct.ScmObj* %argslist57868$_37take480432, %struct.ScmObj** %stackaddr$prim59733, align 8
%stackaddr$prim59734 = alloca %struct.ScmObj*, align 8
%argslist57868$_37take480433 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48497, %struct.ScmObj* %argslist57868$_37take480432)
store volatile %struct.ScmObj* %argslist57868$_37take480433, %struct.ScmObj** %stackaddr$prim59734, align 8
%clofunc59735 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take48043)
musttail call tailcc void %clofunc59735(%struct.ScmObj* %_37take48043, %struct.ScmObj* %argslist57868$_37take480433)
ret void
}

define tailcc void @proc_clo$ae49153(%struct.ScmObj* %env$ae49153,%struct.ScmObj* %current_45args57871) {
%stackaddr$env-ref59736 = alloca %struct.ScmObj*, align 8
%_37foldl148035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49153, i64 0)
store %struct.ScmObj* %_37foldl148035, %struct.ScmObj** %stackaddr$env-ref59736
%stackaddr$prim59737 = alloca %struct.ScmObj*, align 8
%k48499 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57871)
store volatile %struct.ScmObj* %k48499, %struct.ScmObj** %stackaddr$prim59737, align 8
%stackaddr$prim59738 = alloca %struct.ScmObj*, align 8
%current_45args57872 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57871)
store volatile %struct.ScmObj* %current_45args57872, %struct.ScmObj** %stackaddr$prim59738, align 8
%stackaddr$prim59739 = alloca %struct.ScmObj*, align 8
%lst48074 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57872)
store volatile %struct.ScmObj* %lst48074, %struct.ScmObj** %stackaddr$prim59739, align 8
%stackaddr$makeclosure59740 = alloca %struct.ScmObj*, align 8
%fptrToInt59741 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49154 to i64
%ae49154 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt59741)
store volatile %struct.ScmObj* %ae49154, %struct.ScmObj** %stackaddr$makeclosure59740, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49154, %struct.ScmObj* %_37foldl148035, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49154, %struct.ScmObj* %k48499, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49154, %struct.ScmObj* %lst48074, i64 2)
%ae49155 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59742 = alloca %struct.ScmObj*, align 8
%fptrToInt59743 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49156 to i64
%ae49156 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt59743)
store volatile %struct.ScmObj* %ae49156, %struct.ScmObj** %stackaddr$makeclosure59742, align 8
%argslist57883$ae491540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59744 = alloca %struct.ScmObj*, align 8
%argslist57883$ae491541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49156, %struct.ScmObj* %argslist57883$ae491540)
store volatile %struct.ScmObj* %argslist57883$ae491541, %struct.ScmObj** %stackaddr$prim59744, align 8
%stackaddr$prim59745 = alloca %struct.ScmObj*, align 8
%argslist57883$ae491542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49155, %struct.ScmObj* %argslist57883$ae491541)
store volatile %struct.ScmObj* %argslist57883$ae491542, %struct.ScmObj** %stackaddr$prim59745, align 8
%clofunc59746 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49154)
musttail call tailcc void %clofunc59746(%struct.ScmObj* %ae49154, %struct.ScmObj* %argslist57883$ae491542)
ret void
}

define tailcc void @proc_clo$ae49154(%struct.ScmObj* %env$ae49154,%struct.ScmObj* %current_45args57874) {
%stackaddr$env-ref59747 = alloca %struct.ScmObj*, align 8
%_37foldl148035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49154, i64 0)
store %struct.ScmObj* %_37foldl148035, %struct.ScmObj** %stackaddr$env-ref59747
%stackaddr$env-ref59748 = alloca %struct.ScmObj*, align 8
%k48499 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49154, i64 1)
store %struct.ScmObj* %k48499, %struct.ScmObj** %stackaddr$env-ref59748
%stackaddr$env-ref59749 = alloca %struct.ScmObj*, align 8
%lst48074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49154, i64 2)
store %struct.ScmObj* %lst48074, %struct.ScmObj** %stackaddr$env-ref59749
%stackaddr$prim59750 = alloca %struct.ScmObj*, align 8
%_95k48500 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57874)
store volatile %struct.ScmObj* %_95k48500, %struct.ScmObj** %stackaddr$prim59750, align 8
%stackaddr$prim59751 = alloca %struct.ScmObj*, align 8
%current_45args57875 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57874)
store volatile %struct.ScmObj* %current_45args57875, %struct.ScmObj** %stackaddr$prim59751, align 8
%stackaddr$prim59752 = alloca %struct.ScmObj*, align 8
%anf_45bind48204 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57875)
store volatile %struct.ScmObj* %anf_45bind48204, %struct.ScmObj** %stackaddr$prim59752, align 8
%ae49175 = call %struct.ScmObj* @const_init_null()
%argslist57877$_37foldl1480350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59753 = alloca %struct.ScmObj*, align 8
%argslist57877$_37foldl1480351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48074, %struct.ScmObj* %argslist57877$_37foldl1480350)
store volatile %struct.ScmObj* %argslist57877$_37foldl1480351, %struct.ScmObj** %stackaddr$prim59753, align 8
%stackaddr$prim59754 = alloca %struct.ScmObj*, align 8
%argslist57877$_37foldl1480352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49175, %struct.ScmObj* %argslist57877$_37foldl1480351)
store volatile %struct.ScmObj* %argslist57877$_37foldl1480352, %struct.ScmObj** %stackaddr$prim59754, align 8
%stackaddr$prim59755 = alloca %struct.ScmObj*, align 8
%argslist57877$_37foldl1480353 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48204, %struct.ScmObj* %argslist57877$_37foldl1480352)
store volatile %struct.ScmObj* %argslist57877$_37foldl1480353, %struct.ScmObj** %stackaddr$prim59755, align 8
%stackaddr$prim59756 = alloca %struct.ScmObj*, align 8
%argslist57877$_37foldl1480354 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48499, %struct.ScmObj* %argslist57877$_37foldl1480353)
store volatile %struct.ScmObj* %argslist57877$_37foldl1480354, %struct.ScmObj** %stackaddr$prim59756, align 8
%clofunc59757 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148035)
musttail call tailcc void %clofunc59757(%struct.ScmObj* %_37foldl148035, %struct.ScmObj* %argslist57877$_37foldl1480354)
ret void
}

define tailcc void @proc_clo$ae49156(%struct.ScmObj* %env$ae49156,%struct.ScmObj* %current_45args57878) {
%stackaddr$prim59758 = alloca %struct.ScmObj*, align 8
%k48501 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57878)
store volatile %struct.ScmObj* %k48501, %struct.ScmObj** %stackaddr$prim59758, align 8
%stackaddr$prim59759 = alloca %struct.ScmObj*, align 8
%current_45args57879 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57878)
store volatile %struct.ScmObj* %current_45args57879, %struct.ScmObj** %stackaddr$prim59759, align 8
%stackaddr$prim59760 = alloca %struct.ScmObj*, align 8
%x48076 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57879)
store volatile %struct.ScmObj* %x48076, %struct.ScmObj** %stackaddr$prim59760, align 8
%stackaddr$prim59761 = alloca %struct.ScmObj*, align 8
%current_45args57880 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57879)
store volatile %struct.ScmObj* %current_45args57880, %struct.ScmObj** %stackaddr$prim59761, align 8
%stackaddr$prim59762 = alloca %struct.ScmObj*, align 8
%y48075 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57880)
store volatile %struct.ScmObj* %y48075, %struct.ScmObj** %stackaddr$prim59762, align 8
%ae49158 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57882$k485010 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59763 = alloca %struct.ScmObj*, align 8
%argslist57882$k485011 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x48076, %struct.ScmObj* %argslist57882$k485010)
store volatile %struct.ScmObj* %argslist57882$k485011, %struct.ScmObj** %stackaddr$prim59763, align 8
%stackaddr$prim59764 = alloca %struct.ScmObj*, align 8
%argslist57882$k485012 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49158, %struct.ScmObj* %argslist57882$k485011)
store volatile %struct.ScmObj* %argslist57882$k485012, %struct.ScmObj** %stackaddr$prim59764, align 8
%clofunc59765 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48501)
musttail call tailcc void %clofunc59765(%struct.ScmObj* %k48501, %struct.ScmObj* %argslist57882$k485012)
ret void
}

define tailcc void @proc_clo$ae49074(%struct.ScmObj* %env$ae49074,%struct.ScmObj* %current_45args57886) {
%stackaddr$prim59766 = alloca %struct.ScmObj*, align 8
%k48502 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57886)
store volatile %struct.ScmObj* %k48502, %struct.ScmObj** %stackaddr$prim59766, align 8
%stackaddr$prim59767 = alloca %struct.ScmObj*, align 8
%current_45args57887 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57886)
store volatile %struct.ScmObj* %current_45args57887, %struct.ScmObj** %stackaddr$prim59767, align 8
%stackaddr$prim59768 = alloca %struct.ScmObj*, align 8
%_37foldl148036 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57887)
store volatile %struct.ScmObj* %_37foldl148036, %struct.ScmObj** %stackaddr$prim59768, align 8
%ae49076 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59769 = alloca %struct.ScmObj*, align 8
%fptrToInt59770 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49077 to i64
%ae49077 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt59770)
store volatile %struct.ScmObj* %ae49077, %struct.ScmObj** %stackaddr$makeclosure59769, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49077, %struct.ScmObj* %_37foldl148036, i64 0)
%argslist57900$k485020 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59771 = alloca %struct.ScmObj*, align 8
%argslist57900$k485021 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49077, %struct.ScmObj* %argslist57900$k485020)
store volatile %struct.ScmObj* %argslist57900$k485021, %struct.ScmObj** %stackaddr$prim59771, align 8
%stackaddr$prim59772 = alloca %struct.ScmObj*, align 8
%argslist57900$k485022 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49076, %struct.ScmObj* %argslist57900$k485021)
store volatile %struct.ScmObj* %argslist57900$k485022, %struct.ScmObj** %stackaddr$prim59772, align 8
%clofunc59773 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48502)
musttail call tailcc void %clofunc59773(%struct.ScmObj* %k48502, %struct.ScmObj* %argslist57900$k485022)
ret void
}

define tailcc void @proc_clo$ae49077(%struct.ScmObj* %env$ae49077,%struct.ScmObj* %current_45args57889) {
%stackaddr$env-ref59774 = alloca %struct.ScmObj*, align 8
%_37foldl148036 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49077, i64 0)
store %struct.ScmObj* %_37foldl148036, %struct.ScmObj** %stackaddr$env-ref59774
%stackaddr$prim59775 = alloca %struct.ScmObj*, align 8
%k48503 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57889)
store volatile %struct.ScmObj* %k48503, %struct.ScmObj** %stackaddr$prim59775, align 8
%stackaddr$prim59776 = alloca %struct.ScmObj*, align 8
%current_45args57890 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57889)
store volatile %struct.ScmObj* %current_45args57890, %struct.ScmObj** %stackaddr$prim59776, align 8
%stackaddr$prim59777 = alloca %struct.ScmObj*, align 8
%f48039 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57890)
store volatile %struct.ScmObj* %f48039, %struct.ScmObj** %stackaddr$prim59777, align 8
%stackaddr$prim59778 = alloca %struct.ScmObj*, align 8
%current_45args57891 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57890)
store volatile %struct.ScmObj* %current_45args57891, %struct.ScmObj** %stackaddr$prim59778, align 8
%stackaddr$prim59779 = alloca %struct.ScmObj*, align 8
%acc48038 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57891)
store volatile %struct.ScmObj* %acc48038, %struct.ScmObj** %stackaddr$prim59779, align 8
%stackaddr$prim59780 = alloca %struct.ScmObj*, align 8
%current_45args57892 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57891)
store volatile %struct.ScmObj* %current_45args57892, %struct.ScmObj** %stackaddr$prim59780, align 8
%stackaddr$prim59781 = alloca %struct.ScmObj*, align 8
%lst48037 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57892)
store volatile %struct.ScmObj* %lst48037, %struct.ScmObj** %stackaddr$prim59781, align 8
%stackaddr$prim59782 = alloca %struct.ScmObj*, align 8
%anf_45bind48199 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48037)
store volatile %struct.ScmObj* %anf_45bind48199, %struct.ScmObj** %stackaddr$prim59782, align 8
%truthy$cmp59783 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48199)
%cmp$cmp59783 = icmp eq i64 %truthy$cmp59783, 1
br i1 %cmp$cmp59783, label %truebranch$cmp59783, label %falsebranch$cmp59783
truebranch$cmp59783:
%ae49081 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57894$k485030 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59784 = alloca %struct.ScmObj*, align 8
%argslist57894$k485031 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48038, %struct.ScmObj* %argslist57894$k485030)
store volatile %struct.ScmObj* %argslist57894$k485031, %struct.ScmObj** %stackaddr$prim59784, align 8
%stackaddr$prim59785 = alloca %struct.ScmObj*, align 8
%argslist57894$k485032 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49081, %struct.ScmObj* %argslist57894$k485031)
store volatile %struct.ScmObj* %argslist57894$k485032, %struct.ScmObj** %stackaddr$prim59785, align 8
%clofunc59786 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48503)
musttail call tailcc void %clofunc59786(%struct.ScmObj* %k48503, %struct.ScmObj* %argslist57894$k485032)
ret void
falsebranch$cmp59783:
%stackaddr$prim59787 = alloca %struct.ScmObj*, align 8
%anf_45bind48200 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48037)
store volatile %struct.ScmObj* %anf_45bind48200, %struct.ScmObj** %stackaddr$prim59787, align 8
%stackaddr$makeclosure59788 = alloca %struct.ScmObj*, align 8
%fptrToInt59789 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49088 to i64
%ae49088 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt59789)
store volatile %struct.ScmObj* %ae49088, %struct.ScmObj** %stackaddr$makeclosure59788, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49088, %struct.ScmObj* %f48039, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49088, %struct.ScmObj* %k48503, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49088, %struct.ScmObj* %lst48037, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49088, %struct.ScmObj* %_37foldl148036, i64 3)
%argslist57899$f480390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59790 = alloca %struct.ScmObj*, align 8
%argslist57899$f480391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48038, %struct.ScmObj* %argslist57899$f480390)
store volatile %struct.ScmObj* %argslist57899$f480391, %struct.ScmObj** %stackaddr$prim59790, align 8
%stackaddr$prim59791 = alloca %struct.ScmObj*, align 8
%argslist57899$f480392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48200, %struct.ScmObj* %argslist57899$f480391)
store volatile %struct.ScmObj* %argslist57899$f480392, %struct.ScmObj** %stackaddr$prim59791, align 8
%stackaddr$prim59792 = alloca %struct.ScmObj*, align 8
%argslist57899$f480393 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49088, %struct.ScmObj* %argslist57899$f480392)
store volatile %struct.ScmObj* %argslist57899$f480393, %struct.ScmObj** %stackaddr$prim59792, align 8
%clofunc59793 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48039)
musttail call tailcc void %clofunc59793(%struct.ScmObj* %f48039, %struct.ScmObj* %argslist57899$f480393)
ret void
}

define tailcc void @proc_clo$ae49088(%struct.ScmObj* %env$ae49088,%struct.ScmObj* %current_45args57895) {
%stackaddr$env-ref59794 = alloca %struct.ScmObj*, align 8
%f48039 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49088, i64 0)
store %struct.ScmObj* %f48039, %struct.ScmObj** %stackaddr$env-ref59794
%stackaddr$env-ref59795 = alloca %struct.ScmObj*, align 8
%k48503 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49088, i64 1)
store %struct.ScmObj* %k48503, %struct.ScmObj** %stackaddr$env-ref59795
%stackaddr$env-ref59796 = alloca %struct.ScmObj*, align 8
%lst48037 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49088, i64 2)
store %struct.ScmObj* %lst48037, %struct.ScmObj** %stackaddr$env-ref59796
%stackaddr$env-ref59797 = alloca %struct.ScmObj*, align 8
%_37foldl148036 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49088, i64 3)
store %struct.ScmObj* %_37foldl148036, %struct.ScmObj** %stackaddr$env-ref59797
%stackaddr$prim59798 = alloca %struct.ScmObj*, align 8
%_95k48504 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57895)
store volatile %struct.ScmObj* %_95k48504, %struct.ScmObj** %stackaddr$prim59798, align 8
%stackaddr$prim59799 = alloca %struct.ScmObj*, align 8
%current_45args57896 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57895)
store volatile %struct.ScmObj* %current_45args57896, %struct.ScmObj** %stackaddr$prim59799, align 8
%stackaddr$prim59800 = alloca %struct.ScmObj*, align 8
%anf_45bind48201 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57896)
store volatile %struct.ScmObj* %anf_45bind48201, %struct.ScmObj** %stackaddr$prim59800, align 8
%stackaddr$prim59801 = alloca %struct.ScmObj*, align 8
%anf_45bind48202 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48037)
store volatile %struct.ScmObj* %anf_45bind48202, %struct.ScmObj** %stackaddr$prim59801, align 8
%argslist57898$_37foldl1480360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59802 = alloca %struct.ScmObj*, align 8
%argslist57898$_37foldl1480361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48202, %struct.ScmObj* %argslist57898$_37foldl1480360)
store volatile %struct.ScmObj* %argslist57898$_37foldl1480361, %struct.ScmObj** %stackaddr$prim59802, align 8
%stackaddr$prim59803 = alloca %struct.ScmObj*, align 8
%argslist57898$_37foldl1480362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48201, %struct.ScmObj* %argslist57898$_37foldl1480361)
store volatile %struct.ScmObj* %argslist57898$_37foldl1480362, %struct.ScmObj** %stackaddr$prim59803, align 8
%stackaddr$prim59804 = alloca %struct.ScmObj*, align 8
%argslist57898$_37foldl1480363 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48039, %struct.ScmObj* %argslist57898$_37foldl1480362)
store volatile %struct.ScmObj* %argslist57898$_37foldl1480363, %struct.ScmObj** %stackaddr$prim59804, align 8
%stackaddr$prim59805 = alloca %struct.ScmObj*, align 8
%argslist57898$_37foldl1480364 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48503, %struct.ScmObj* %argslist57898$_37foldl1480363)
store volatile %struct.ScmObj* %argslist57898$_37foldl1480364, %struct.ScmObj** %stackaddr$prim59805, align 8
%clofunc59806 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148036)
musttail call tailcc void %clofunc59806(%struct.ScmObj* %_37foldl148036, %struct.ScmObj* %argslist57898$_37foldl1480364)
ret void
}

define tailcc void @proc_clo$ae48991(%struct.ScmObj* %env$ae48991,%struct.ScmObj* %current_45args57903) {
%stackaddr$prim59807 = alloca %struct.ScmObj*, align 8
%k48505 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57903)
store volatile %struct.ScmObj* %k48505, %struct.ScmObj** %stackaddr$prim59807, align 8
%stackaddr$prim59808 = alloca %struct.ScmObj*, align 8
%current_45args57904 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57903)
store volatile %struct.ScmObj* %current_45args57904, %struct.ScmObj** %stackaddr$prim59808, align 8
%stackaddr$prim59809 = alloca %struct.ScmObj*, align 8
%_37length48041 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57904)
store volatile %struct.ScmObj* %_37length48041, %struct.ScmObj** %stackaddr$prim59809, align 8
%ae48993 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59810 = alloca %struct.ScmObj*, align 8
%fptrToInt59811 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48994 to i64
%ae48994 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt59811)
store volatile %struct.ScmObj* %ae48994, %struct.ScmObj** %stackaddr$makeclosure59810, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48994, %struct.ScmObj* %_37length48041, i64 0)
%argslist57915$k485050 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59812 = alloca %struct.ScmObj*, align 8
%argslist57915$k485051 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48994, %struct.ScmObj* %argslist57915$k485050)
store volatile %struct.ScmObj* %argslist57915$k485051, %struct.ScmObj** %stackaddr$prim59812, align 8
%stackaddr$prim59813 = alloca %struct.ScmObj*, align 8
%argslist57915$k485052 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48993, %struct.ScmObj* %argslist57915$k485051)
store volatile %struct.ScmObj* %argslist57915$k485052, %struct.ScmObj** %stackaddr$prim59813, align 8
%clofunc59814 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48505)
musttail call tailcc void %clofunc59814(%struct.ScmObj* %k48505, %struct.ScmObj* %argslist57915$k485052)
ret void
}

define tailcc void @proc_clo$ae48994(%struct.ScmObj* %env$ae48994,%struct.ScmObj* %current_45args57906) {
%stackaddr$env-ref59815 = alloca %struct.ScmObj*, align 8
%_37length48041 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48994, i64 0)
store %struct.ScmObj* %_37length48041, %struct.ScmObj** %stackaddr$env-ref59815
%stackaddr$prim59816 = alloca %struct.ScmObj*, align 8
%k48506 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57906)
store volatile %struct.ScmObj* %k48506, %struct.ScmObj** %stackaddr$prim59816, align 8
%stackaddr$prim59817 = alloca %struct.ScmObj*, align 8
%current_45args57907 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57906)
store volatile %struct.ScmObj* %current_45args57907, %struct.ScmObj** %stackaddr$prim59817, align 8
%stackaddr$prim59818 = alloca %struct.ScmObj*, align 8
%lst48042 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57907)
store volatile %struct.ScmObj* %lst48042, %struct.ScmObj** %stackaddr$prim59818, align 8
%stackaddr$prim59819 = alloca %struct.ScmObj*, align 8
%anf_45bind48195 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48042)
store volatile %struct.ScmObj* %anf_45bind48195, %struct.ScmObj** %stackaddr$prim59819, align 8
%truthy$cmp59820 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48195)
%cmp$cmp59820 = icmp eq i64 %truthy$cmp59820, 1
br i1 %cmp$cmp59820, label %truebranch$cmp59820, label %falsebranch$cmp59820
truebranch$cmp59820:
%ae48998 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48999 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57909$k485060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59821 = alloca %struct.ScmObj*, align 8
%argslist57909$k485061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48999, %struct.ScmObj* %argslist57909$k485060)
store volatile %struct.ScmObj* %argslist57909$k485061, %struct.ScmObj** %stackaddr$prim59821, align 8
%stackaddr$prim59822 = alloca %struct.ScmObj*, align 8
%argslist57909$k485062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48998, %struct.ScmObj* %argslist57909$k485061)
store volatile %struct.ScmObj* %argslist57909$k485062, %struct.ScmObj** %stackaddr$prim59822, align 8
%clofunc59823 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48506)
musttail call tailcc void %clofunc59823(%struct.ScmObj* %k48506, %struct.ScmObj* %argslist57909$k485062)
ret void
falsebranch$cmp59820:
%stackaddr$prim59824 = alloca %struct.ScmObj*, align 8
%anf_45bind48196 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48042)
store volatile %struct.ScmObj* %anf_45bind48196, %struct.ScmObj** %stackaddr$prim59824, align 8
%stackaddr$makeclosure59825 = alloca %struct.ScmObj*, align 8
%fptrToInt59826 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49008 to i64
%ae49008 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt59826)
store volatile %struct.ScmObj* %ae49008, %struct.ScmObj** %stackaddr$makeclosure59825, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49008, %struct.ScmObj* %k48506, i64 0)
%argslist57914$_37length480410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59827 = alloca %struct.ScmObj*, align 8
%argslist57914$_37length480411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48196, %struct.ScmObj* %argslist57914$_37length480410)
store volatile %struct.ScmObj* %argslist57914$_37length480411, %struct.ScmObj** %stackaddr$prim59827, align 8
%stackaddr$prim59828 = alloca %struct.ScmObj*, align 8
%argslist57914$_37length480412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49008, %struct.ScmObj* %argslist57914$_37length480411)
store volatile %struct.ScmObj* %argslist57914$_37length480412, %struct.ScmObj** %stackaddr$prim59828, align 8
%clofunc59829 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length48041)
musttail call tailcc void %clofunc59829(%struct.ScmObj* %_37length48041, %struct.ScmObj* %argslist57914$_37length480412)
ret void
}

define tailcc void @proc_clo$ae49008(%struct.ScmObj* %env$ae49008,%struct.ScmObj* %current_45args57910) {
%stackaddr$env-ref59830 = alloca %struct.ScmObj*, align 8
%k48506 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49008, i64 0)
store %struct.ScmObj* %k48506, %struct.ScmObj** %stackaddr$env-ref59830
%stackaddr$prim59831 = alloca %struct.ScmObj*, align 8
%_95k48507 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57910)
store volatile %struct.ScmObj* %_95k48507, %struct.ScmObj** %stackaddr$prim59831, align 8
%stackaddr$prim59832 = alloca %struct.ScmObj*, align 8
%current_45args57911 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57910)
store volatile %struct.ScmObj* %current_45args57911, %struct.ScmObj** %stackaddr$prim59832, align 8
%stackaddr$prim59833 = alloca %struct.ScmObj*, align 8
%anf_45bind48197 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57911)
store volatile %struct.ScmObj* %anf_45bind48197, %struct.ScmObj** %stackaddr$prim59833, align 8
%ae49010 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim59834 = alloca %struct.ScmObj*, align 8
%cpsprim48508 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae49010, %struct.ScmObj* %anf_45bind48197)
store volatile %struct.ScmObj* %cpsprim48508, %struct.ScmObj** %stackaddr$prim59834, align 8
%ae49013 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57913$k485060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59835 = alloca %struct.ScmObj*, align 8
%argslist57913$k485061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48508, %struct.ScmObj* %argslist57913$k485060)
store volatile %struct.ScmObj* %argslist57913$k485061, %struct.ScmObj** %stackaddr$prim59835, align 8
%stackaddr$prim59836 = alloca %struct.ScmObj*, align 8
%argslist57913$k485062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49013, %struct.ScmObj* %argslist57913$k485061)
store volatile %struct.ScmObj* %argslist57913$k485062, %struct.ScmObj** %stackaddr$prim59836, align 8
%clofunc59837 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48506)
musttail call tailcc void %clofunc59837(%struct.ScmObj* %k48506, %struct.ScmObj* %argslist57913$k485062)
ret void
}

define tailcc void @proc_clo$ae48841(%struct.ScmObj* %env$ae48841,%struct.ScmObj* %current_45args57918) {
%stackaddr$prim59838 = alloca %struct.ScmObj*, align 8
%k48509 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57918)
store volatile %struct.ScmObj* %k48509, %struct.ScmObj** %stackaddr$prim59838, align 8
%stackaddr$prim59839 = alloca %struct.ScmObj*, align 8
%current_45args57919 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57918)
store volatile %struct.ScmObj* %current_45args57919, %struct.ScmObj** %stackaddr$prim59839, align 8
%stackaddr$prim59840 = alloca %struct.ScmObj*, align 8
%_37take48044 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57919)
store volatile %struct.ScmObj* %_37take48044, %struct.ScmObj** %stackaddr$prim59840, align 8
%ae48843 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59841 = alloca %struct.ScmObj*, align 8
%fptrToInt59842 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48844 to i64
%ae48844 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt59842)
store volatile %struct.ScmObj* %ae48844, %struct.ScmObj** %stackaddr$makeclosure59841, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48844, %struct.ScmObj* %_37take48044, i64 0)
%argslist57932$k485090 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59843 = alloca %struct.ScmObj*, align 8
%argslist57932$k485091 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48844, %struct.ScmObj* %argslist57932$k485090)
store volatile %struct.ScmObj* %argslist57932$k485091, %struct.ScmObj** %stackaddr$prim59843, align 8
%stackaddr$prim59844 = alloca %struct.ScmObj*, align 8
%argslist57932$k485092 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48843, %struct.ScmObj* %argslist57932$k485091)
store volatile %struct.ScmObj* %argslist57932$k485092, %struct.ScmObj** %stackaddr$prim59844, align 8
%clofunc59845 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48509)
musttail call tailcc void %clofunc59845(%struct.ScmObj* %k48509, %struct.ScmObj* %argslist57932$k485092)
ret void
}

define tailcc void @proc_clo$ae48844(%struct.ScmObj* %env$ae48844,%struct.ScmObj* %current_45args57921) {
%stackaddr$env-ref59846 = alloca %struct.ScmObj*, align 8
%_37take48044 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48844, i64 0)
store %struct.ScmObj* %_37take48044, %struct.ScmObj** %stackaddr$env-ref59846
%stackaddr$prim59847 = alloca %struct.ScmObj*, align 8
%k48510 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57921)
store volatile %struct.ScmObj* %k48510, %struct.ScmObj** %stackaddr$prim59847, align 8
%stackaddr$prim59848 = alloca %struct.ScmObj*, align 8
%current_45args57922 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57921)
store volatile %struct.ScmObj* %current_45args57922, %struct.ScmObj** %stackaddr$prim59848, align 8
%stackaddr$prim59849 = alloca %struct.ScmObj*, align 8
%lst48046 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57922)
store volatile %struct.ScmObj* %lst48046, %struct.ScmObj** %stackaddr$prim59849, align 8
%stackaddr$prim59850 = alloca %struct.ScmObj*, align 8
%current_45args57923 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57922)
store volatile %struct.ScmObj* %current_45args57923, %struct.ScmObj** %stackaddr$prim59850, align 8
%stackaddr$prim59851 = alloca %struct.ScmObj*, align 8
%n48045 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57923)
store volatile %struct.ScmObj* %n48045, %struct.ScmObj** %stackaddr$prim59851, align 8
%ae48846 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59852 = alloca %struct.ScmObj*, align 8
%anf_45bind48188 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n48045, %struct.ScmObj* %ae48846)
store volatile %struct.ScmObj* %anf_45bind48188, %struct.ScmObj** %stackaddr$prim59852, align 8
%truthy$cmp59853 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48188)
%cmp$cmp59853 = icmp eq i64 %truthy$cmp59853, 1
br i1 %cmp$cmp59853, label %truebranch$cmp59853, label %falsebranch$cmp59853
truebranch$cmp59853:
%ae48849 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48850 = call %struct.ScmObj* @const_init_null()
%argslist57925$k485100 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59854 = alloca %struct.ScmObj*, align 8
%argslist57925$k485101 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48850, %struct.ScmObj* %argslist57925$k485100)
store volatile %struct.ScmObj* %argslist57925$k485101, %struct.ScmObj** %stackaddr$prim59854, align 8
%stackaddr$prim59855 = alloca %struct.ScmObj*, align 8
%argslist57925$k485102 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48849, %struct.ScmObj* %argslist57925$k485101)
store volatile %struct.ScmObj* %argslist57925$k485102, %struct.ScmObj** %stackaddr$prim59855, align 8
%clofunc59856 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48510)
musttail call tailcc void %clofunc59856(%struct.ScmObj* %k48510, %struct.ScmObj* %argslist57925$k485102)
ret void
falsebranch$cmp59853:
%stackaddr$prim59857 = alloca %struct.ScmObj*, align 8
%anf_45bind48189 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48046)
store volatile %struct.ScmObj* %anf_45bind48189, %struct.ScmObj** %stackaddr$prim59857, align 8
%truthy$cmp59858 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48189)
%cmp$cmp59858 = icmp eq i64 %truthy$cmp59858, 1
br i1 %cmp$cmp59858, label %truebranch$cmp59858, label %falsebranch$cmp59858
truebranch$cmp59858:
%ae48860 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48861 = call %struct.ScmObj* @const_init_null()
%argslist57926$k485100 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59859 = alloca %struct.ScmObj*, align 8
%argslist57926$k485101 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48861, %struct.ScmObj* %argslist57926$k485100)
store volatile %struct.ScmObj* %argslist57926$k485101, %struct.ScmObj** %stackaddr$prim59859, align 8
%stackaddr$prim59860 = alloca %struct.ScmObj*, align 8
%argslist57926$k485102 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48860, %struct.ScmObj* %argslist57926$k485101)
store volatile %struct.ScmObj* %argslist57926$k485102, %struct.ScmObj** %stackaddr$prim59860, align 8
%clofunc59861 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48510)
musttail call tailcc void %clofunc59861(%struct.ScmObj* %k48510, %struct.ScmObj* %argslist57926$k485102)
ret void
falsebranch$cmp59858:
%stackaddr$prim59862 = alloca %struct.ScmObj*, align 8
%anf_45bind48190 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48046)
store volatile %struct.ScmObj* %anf_45bind48190, %struct.ScmObj** %stackaddr$prim59862, align 8
%stackaddr$prim59863 = alloca %struct.ScmObj*, align 8
%anf_45bind48191 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48046)
store volatile %struct.ScmObj* %anf_45bind48191, %struct.ScmObj** %stackaddr$prim59863, align 8
%ae48871 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim59864 = alloca %struct.ScmObj*, align 8
%anf_45bind48192 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n48045, %struct.ScmObj* %ae48871)
store volatile %struct.ScmObj* %anf_45bind48192, %struct.ScmObj** %stackaddr$prim59864, align 8
%stackaddr$makeclosure59865 = alloca %struct.ScmObj*, align 8
%fptrToInt59866 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48873 to i64
%ae48873 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt59866)
store volatile %struct.ScmObj* %ae48873, %struct.ScmObj** %stackaddr$makeclosure59865, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48873, %struct.ScmObj* %k48510, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48873, %struct.ScmObj* %anf_45bind48190, i64 1)
%argslist57931$_37take480440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59867 = alloca %struct.ScmObj*, align 8
%argslist57931$_37take480441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48192, %struct.ScmObj* %argslist57931$_37take480440)
store volatile %struct.ScmObj* %argslist57931$_37take480441, %struct.ScmObj** %stackaddr$prim59867, align 8
%stackaddr$prim59868 = alloca %struct.ScmObj*, align 8
%argslist57931$_37take480442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48191, %struct.ScmObj* %argslist57931$_37take480441)
store volatile %struct.ScmObj* %argslist57931$_37take480442, %struct.ScmObj** %stackaddr$prim59868, align 8
%stackaddr$prim59869 = alloca %struct.ScmObj*, align 8
%argslist57931$_37take480443 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48873, %struct.ScmObj* %argslist57931$_37take480442)
store volatile %struct.ScmObj* %argslist57931$_37take480443, %struct.ScmObj** %stackaddr$prim59869, align 8
%clofunc59870 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take48044)
musttail call tailcc void %clofunc59870(%struct.ScmObj* %_37take48044, %struct.ScmObj* %argslist57931$_37take480443)
ret void
}

define tailcc void @proc_clo$ae48873(%struct.ScmObj* %env$ae48873,%struct.ScmObj* %current_45args57927) {
%stackaddr$env-ref59871 = alloca %struct.ScmObj*, align 8
%k48510 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48873, i64 0)
store %struct.ScmObj* %k48510, %struct.ScmObj** %stackaddr$env-ref59871
%stackaddr$env-ref59872 = alloca %struct.ScmObj*, align 8
%anf_45bind48190 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48873, i64 1)
store %struct.ScmObj* %anf_45bind48190, %struct.ScmObj** %stackaddr$env-ref59872
%stackaddr$prim59873 = alloca %struct.ScmObj*, align 8
%_95k48511 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57927)
store volatile %struct.ScmObj* %_95k48511, %struct.ScmObj** %stackaddr$prim59873, align 8
%stackaddr$prim59874 = alloca %struct.ScmObj*, align 8
%current_45args57928 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57927)
store volatile %struct.ScmObj* %current_45args57928, %struct.ScmObj** %stackaddr$prim59874, align 8
%stackaddr$prim59875 = alloca %struct.ScmObj*, align 8
%anf_45bind48193 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57928)
store volatile %struct.ScmObj* %anf_45bind48193, %struct.ScmObj** %stackaddr$prim59875, align 8
%stackaddr$prim59876 = alloca %struct.ScmObj*, align 8
%cpsprim48512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48190, %struct.ScmObj* %anf_45bind48193)
store volatile %struct.ScmObj* %cpsprim48512, %struct.ScmObj** %stackaddr$prim59876, align 8
%ae48879 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57930$k485100 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59877 = alloca %struct.ScmObj*, align 8
%argslist57930$k485101 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48512, %struct.ScmObj* %argslist57930$k485100)
store volatile %struct.ScmObj* %argslist57930$k485101, %struct.ScmObj** %stackaddr$prim59877, align 8
%stackaddr$prim59878 = alloca %struct.ScmObj*, align 8
%argslist57930$k485102 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48879, %struct.ScmObj* %argslist57930$k485101)
store volatile %struct.ScmObj* %argslist57930$k485102, %struct.ScmObj** %stackaddr$prim59878, align 8
%clofunc59879 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48510)
musttail call tailcc void %clofunc59879(%struct.ScmObj* %k48510, %struct.ScmObj* %argslist57930$k485102)
ret void
}

define tailcc void @proc_clo$ae48744(%struct.ScmObj* %env$ae48744,%struct.ScmObj* %current_45args57935) {
%stackaddr$prim59880 = alloca %struct.ScmObj*, align 8
%k48513 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57935)
store volatile %struct.ScmObj* %k48513, %struct.ScmObj** %stackaddr$prim59880, align 8
%stackaddr$prim59881 = alloca %struct.ScmObj*, align 8
%current_45args57936 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57935)
store volatile %struct.ScmObj* %current_45args57936, %struct.ScmObj** %stackaddr$prim59881, align 8
%stackaddr$prim59882 = alloca %struct.ScmObj*, align 8
%_37map48048 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57936)
store volatile %struct.ScmObj* %_37map48048, %struct.ScmObj** %stackaddr$prim59882, align 8
%ae48746 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59883 = alloca %struct.ScmObj*, align 8
%fptrToInt59884 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48747 to i64
%ae48747 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt59884)
store volatile %struct.ScmObj* %ae48747, %struct.ScmObj** %stackaddr$makeclosure59883, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48747, %struct.ScmObj* %_37map48048, i64 0)
%argslist57952$k485130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59885 = alloca %struct.ScmObj*, align 8
%argslist57952$k485131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48747, %struct.ScmObj* %argslist57952$k485130)
store volatile %struct.ScmObj* %argslist57952$k485131, %struct.ScmObj** %stackaddr$prim59885, align 8
%stackaddr$prim59886 = alloca %struct.ScmObj*, align 8
%argslist57952$k485132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48746, %struct.ScmObj* %argslist57952$k485131)
store volatile %struct.ScmObj* %argslist57952$k485132, %struct.ScmObj** %stackaddr$prim59886, align 8
%clofunc59887 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48513)
musttail call tailcc void %clofunc59887(%struct.ScmObj* %k48513, %struct.ScmObj* %argslist57952$k485132)
ret void
}

define tailcc void @proc_clo$ae48747(%struct.ScmObj* %env$ae48747,%struct.ScmObj* %current_45args57938) {
%stackaddr$env-ref59888 = alloca %struct.ScmObj*, align 8
%_37map48048 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48747, i64 0)
store %struct.ScmObj* %_37map48048, %struct.ScmObj** %stackaddr$env-ref59888
%stackaddr$prim59889 = alloca %struct.ScmObj*, align 8
%k48514 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57938)
store volatile %struct.ScmObj* %k48514, %struct.ScmObj** %stackaddr$prim59889, align 8
%stackaddr$prim59890 = alloca %struct.ScmObj*, align 8
%current_45args57939 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57938)
store volatile %struct.ScmObj* %current_45args57939, %struct.ScmObj** %stackaddr$prim59890, align 8
%stackaddr$prim59891 = alloca %struct.ScmObj*, align 8
%f48050 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57939)
store volatile %struct.ScmObj* %f48050, %struct.ScmObj** %stackaddr$prim59891, align 8
%stackaddr$prim59892 = alloca %struct.ScmObj*, align 8
%current_45args57940 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57939)
store volatile %struct.ScmObj* %current_45args57940, %struct.ScmObj** %stackaddr$prim59892, align 8
%stackaddr$prim59893 = alloca %struct.ScmObj*, align 8
%lst48049 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57940)
store volatile %struct.ScmObj* %lst48049, %struct.ScmObj** %stackaddr$prim59893, align 8
%stackaddr$prim59894 = alloca %struct.ScmObj*, align 8
%anf_45bind48182 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48049)
store volatile %struct.ScmObj* %anf_45bind48182, %struct.ScmObj** %stackaddr$prim59894, align 8
%truthy$cmp59895 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48182)
%cmp$cmp59895 = icmp eq i64 %truthy$cmp59895, 1
br i1 %cmp$cmp59895, label %truebranch$cmp59895, label %falsebranch$cmp59895
truebranch$cmp59895:
%ae48751 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48752 = call %struct.ScmObj* @const_init_null()
%argslist57942$k485140 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59896 = alloca %struct.ScmObj*, align 8
%argslist57942$k485141 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48752, %struct.ScmObj* %argslist57942$k485140)
store volatile %struct.ScmObj* %argslist57942$k485141, %struct.ScmObj** %stackaddr$prim59896, align 8
%stackaddr$prim59897 = alloca %struct.ScmObj*, align 8
%argslist57942$k485142 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48751, %struct.ScmObj* %argslist57942$k485141)
store volatile %struct.ScmObj* %argslist57942$k485142, %struct.ScmObj** %stackaddr$prim59897, align 8
%clofunc59898 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48514)
musttail call tailcc void %clofunc59898(%struct.ScmObj* %k48514, %struct.ScmObj* %argslist57942$k485142)
ret void
falsebranch$cmp59895:
%stackaddr$prim59899 = alloca %struct.ScmObj*, align 8
%anf_45bind48183 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48049)
store volatile %struct.ScmObj* %anf_45bind48183, %struct.ScmObj** %stackaddr$prim59899, align 8
%stackaddr$makeclosure59900 = alloca %struct.ScmObj*, align 8
%fptrToInt59901 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48761 to i64
%ae48761 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt59901)
store volatile %struct.ScmObj* %ae48761, %struct.ScmObj** %stackaddr$makeclosure59900, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48761, %struct.ScmObj* %f48050, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48761, %struct.ScmObj* %k48514, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48761, %struct.ScmObj* %lst48049, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48761, %struct.ScmObj* %_37map48048, i64 3)
%argslist57951$f480500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59902 = alloca %struct.ScmObj*, align 8
%argslist57951$f480501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48183, %struct.ScmObj* %argslist57951$f480500)
store volatile %struct.ScmObj* %argslist57951$f480501, %struct.ScmObj** %stackaddr$prim59902, align 8
%stackaddr$prim59903 = alloca %struct.ScmObj*, align 8
%argslist57951$f480502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48761, %struct.ScmObj* %argslist57951$f480501)
store volatile %struct.ScmObj* %argslist57951$f480502, %struct.ScmObj** %stackaddr$prim59903, align 8
%clofunc59904 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48050)
musttail call tailcc void %clofunc59904(%struct.ScmObj* %f48050, %struct.ScmObj* %argslist57951$f480502)
ret void
}

define tailcc void @proc_clo$ae48761(%struct.ScmObj* %env$ae48761,%struct.ScmObj* %current_45args57943) {
%stackaddr$env-ref59905 = alloca %struct.ScmObj*, align 8
%f48050 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48761, i64 0)
store %struct.ScmObj* %f48050, %struct.ScmObj** %stackaddr$env-ref59905
%stackaddr$env-ref59906 = alloca %struct.ScmObj*, align 8
%k48514 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48761, i64 1)
store %struct.ScmObj* %k48514, %struct.ScmObj** %stackaddr$env-ref59906
%stackaddr$env-ref59907 = alloca %struct.ScmObj*, align 8
%lst48049 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48761, i64 2)
store %struct.ScmObj* %lst48049, %struct.ScmObj** %stackaddr$env-ref59907
%stackaddr$env-ref59908 = alloca %struct.ScmObj*, align 8
%_37map48048 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48761, i64 3)
store %struct.ScmObj* %_37map48048, %struct.ScmObj** %stackaddr$env-ref59908
%stackaddr$prim59909 = alloca %struct.ScmObj*, align 8
%_95k48515 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57943)
store volatile %struct.ScmObj* %_95k48515, %struct.ScmObj** %stackaddr$prim59909, align 8
%stackaddr$prim59910 = alloca %struct.ScmObj*, align 8
%current_45args57944 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57943)
store volatile %struct.ScmObj* %current_45args57944, %struct.ScmObj** %stackaddr$prim59910, align 8
%stackaddr$prim59911 = alloca %struct.ScmObj*, align 8
%anf_45bind48184 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57944)
store volatile %struct.ScmObj* %anf_45bind48184, %struct.ScmObj** %stackaddr$prim59911, align 8
%stackaddr$prim59912 = alloca %struct.ScmObj*, align 8
%anf_45bind48185 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48049)
store volatile %struct.ScmObj* %anf_45bind48185, %struct.ScmObj** %stackaddr$prim59912, align 8
%stackaddr$makeclosure59913 = alloca %struct.ScmObj*, align 8
%fptrToInt59914 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48765 to i64
%ae48765 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt59914)
store volatile %struct.ScmObj* %ae48765, %struct.ScmObj** %stackaddr$makeclosure59913, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48765, %struct.ScmObj* %anf_45bind48184, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48765, %struct.ScmObj* %k48514, i64 1)
%argslist57950$_37map480480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59915 = alloca %struct.ScmObj*, align 8
%argslist57950$_37map480481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48185, %struct.ScmObj* %argslist57950$_37map480480)
store volatile %struct.ScmObj* %argslist57950$_37map480481, %struct.ScmObj** %stackaddr$prim59915, align 8
%stackaddr$prim59916 = alloca %struct.ScmObj*, align 8
%argslist57950$_37map480482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48050, %struct.ScmObj* %argslist57950$_37map480481)
store volatile %struct.ScmObj* %argslist57950$_37map480482, %struct.ScmObj** %stackaddr$prim59916, align 8
%stackaddr$prim59917 = alloca %struct.ScmObj*, align 8
%argslist57950$_37map480483 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48765, %struct.ScmObj* %argslist57950$_37map480482)
store volatile %struct.ScmObj* %argslist57950$_37map480483, %struct.ScmObj** %stackaddr$prim59917, align 8
%clofunc59918 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map48048)
musttail call tailcc void %clofunc59918(%struct.ScmObj* %_37map48048, %struct.ScmObj* %argslist57950$_37map480483)
ret void
}

define tailcc void @proc_clo$ae48765(%struct.ScmObj* %env$ae48765,%struct.ScmObj* %current_45args57946) {
%stackaddr$env-ref59919 = alloca %struct.ScmObj*, align 8
%anf_45bind48184 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48765, i64 0)
store %struct.ScmObj* %anf_45bind48184, %struct.ScmObj** %stackaddr$env-ref59919
%stackaddr$env-ref59920 = alloca %struct.ScmObj*, align 8
%k48514 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48765, i64 1)
store %struct.ScmObj* %k48514, %struct.ScmObj** %stackaddr$env-ref59920
%stackaddr$prim59921 = alloca %struct.ScmObj*, align 8
%_95k48516 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57946)
store volatile %struct.ScmObj* %_95k48516, %struct.ScmObj** %stackaddr$prim59921, align 8
%stackaddr$prim59922 = alloca %struct.ScmObj*, align 8
%current_45args57947 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57946)
store volatile %struct.ScmObj* %current_45args57947, %struct.ScmObj** %stackaddr$prim59922, align 8
%stackaddr$prim59923 = alloca %struct.ScmObj*, align 8
%anf_45bind48186 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57947)
store volatile %struct.ScmObj* %anf_45bind48186, %struct.ScmObj** %stackaddr$prim59923, align 8
%stackaddr$prim59924 = alloca %struct.ScmObj*, align 8
%cpsprim48517 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48184, %struct.ScmObj* %anf_45bind48186)
store volatile %struct.ScmObj* %cpsprim48517, %struct.ScmObj** %stackaddr$prim59924, align 8
%ae48771 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57949$k485140 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59925 = alloca %struct.ScmObj*, align 8
%argslist57949$k485141 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48517, %struct.ScmObj* %argslist57949$k485140)
store volatile %struct.ScmObj* %argslist57949$k485141, %struct.ScmObj** %stackaddr$prim59925, align 8
%stackaddr$prim59926 = alloca %struct.ScmObj*, align 8
%argslist57949$k485142 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48771, %struct.ScmObj* %argslist57949$k485141)
store volatile %struct.ScmObj* %argslist57949$k485142, %struct.ScmObj** %stackaddr$prim59926, align 8
%clofunc59927 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48514)
musttail call tailcc void %clofunc59927(%struct.ScmObj* %k48514, %struct.ScmObj* %argslist57949$k485142)
ret void
}

define tailcc void @proc_clo$ae48664(%struct.ScmObj* %env$ae48664,%struct.ScmObj* %current_45args57955) {
%stackaddr$prim59928 = alloca %struct.ScmObj*, align 8
%k48518 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57955)
store volatile %struct.ScmObj* %k48518, %struct.ScmObj** %stackaddr$prim59928, align 8
%stackaddr$prim59929 = alloca %struct.ScmObj*, align 8
%current_45args57956 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57955)
store volatile %struct.ScmObj* %current_45args57956, %struct.ScmObj** %stackaddr$prim59929, align 8
%stackaddr$prim59930 = alloca %struct.ScmObj*, align 8
%_37foldr148052 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57956)
store volatile %struct.ScmObj* %_37foldr148052, %struct.ScmObj** %stackaddr$prim59930, align 8
%ae48666 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59931 = alloca %struct.ScmObj*, align 8
%fptrToInt59932 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48667 to i64
%ae48667 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt59932)
store volatile %struct.ScmObj* %ae48667, %struct.ScmObj** %stackaddr$makeclosure59931, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48667, %struct.ScmObj* %_37foldr148052, i64 0)
%argslist57969$k485180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59933 = alloca %struct.ScmObj*, align 8
%argslist57969$k485181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48667, %struct.ScmObj* %argslist57969$k485180)
store volatile %struct.ScmObj* %argslist57969$k485181, %struct.ScmObj** %stackaddr$prim59933, align 8
%stackaddr$prim59934 = alloca %struct.ScmObj*, align 8
%argslist57969$k485182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48666, %struct.ScmObj* %argslist57969$k485181)
store volatile %struct.ScmObj* %argslist57969$k485182, %struct.ScmObj** %stackaddr$prim59934, align 8
%clofunc59935 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48518)
musttail call tailcc void %clofunc59935(%struct.ScmObj* %k48518, %struct.ScmObj* %argslist57969$k485182)
ret void
}

define tailcc void @proc_clo$ae48667(%struct.ScmObj* %env$ae48667,%struct.ScmObj* %current_45args57958) {
%stackaddr$env-ref59936 = alloca %struct.ScmObj*, align 8
%_37foldr148052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48667, i64 0)
store %struct.ScmObj* %_37foldr148052, %struct.ScmObj** %stackaddr$env-ref59936
%stackaddr$prim59937 = alloca %struct.ScmObj*, align 8
%k48519 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57958)
store volatile %struct.ScmObj* %k48519, %struct.ScmObj** %stackaddr$prim59937, align 8
%stackaddr$prim59938 = alloca %struct.ScmObj*, align 8
%current_45args57959 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57958)
store volatile %struct.ScmObj* %current_45args57959, %struct.ScmObj** %stackaddr$prim59938, align 8
%stackaddr$prim59939 = alloca %struct.ScmObj*, align 8
%f48055 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57959)
store volatile %struct.ScmObj* %f48055, %struct.ScmObj** %stackaddr$prim59939, align 8
%stackaddr$prim59940 = alloca %struct.ScmObj*, align 8
%current_45args57960 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57959)
store volatile %struct.ScmObj* %current_45args57960, %struct.ScmObj** %stackaddr$prim59940, align 8
%stackaddr$prim59941 = alloca %struct.ScmObj*, align 8
%acc48054 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57960)
store volatile %struct.ScmObj* %acc48054, %struct.ScmObj** %stackaddr$prim59941, align 8
%stackaddr$prim59942 = alloca %struct.ScmObj*, align 8
%current_45args57961 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57960)
store volatile %struct.ScmObj* %current_45args57961, %struct.ScmObj** %stackaddr$prim59942, align 8
%stackaddr$prim59943 = alloca %struct.ScmObj*, align 8
%lst48053 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57961)
store volatile %struct.ScmObj* %lst48053, %struct.ScmObj** %stackaddr$prim59943, align 8
%stackaddr$prim59944 = alloca %struct.ScmObj*, align 8
%anf_45bind48177 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48053)
store volatile %struct.ScmObj* %anf_45bind48177, %struct.ScmObj** %stackaddr$prim59944, align 8
%truthy$cmp59945 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48177)
%cmp$cmp59945 = icmp eq i64 %truthy$cmp59945, 1
br i1 %cmp$cmp59945, label %truebranch$cmp59945, label %falsebranch$cmp59945
truebranch$cmp59945:
%ae48671 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57963$k485190 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59946 = alloca %struct.ScmObj*, align 8
%argslist57963$k485191 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48054, %struct.ScmObj* %argslist57963$k485190)
store volatile %struct.ScmObj* %argslist57963$k485191, %struct.ScmObj** %stackaddr$prim59946, align 8
%stackaddr$prim59947 = alloca %struct.ScmObj*, align 8
%argslist57963$k485192 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48671, %struct.ScmObj* %argslist57963$k485191)
store volatile %struct.ScmObj* %argslist57963$k485192, %struct.ScmObj** %stackaddr$prim59947, align 8
%clofunc59948 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48519)
musttail call tailcc void %clofunc59948(%struct.ScmObj* %k48519, %struct.ScmObj* %argslist57963$k485192)
ret void
falsebranch$cmp59945:
%stackaddr$prim59949 = alloca %struct.ScmObj*, align 8
%anf_45bind48178 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48053)
store volatile %struct.ScmObj* %anf_45bind48178, %struct.ScmObj** %stackaddr$prim59949, align 8
%stackaddr$prim59950 = alloca %struct.ScmObj*, align 8
%anf_45bind48179 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48053)
store volatile %struct.ScmObj* %anf_45bind48179, %struct.ScmObj** %stackaddr$prim59950, align 8
%stackaddr$makeclosure59951 = alloca %struct.ScmObj*, align 8
%fptrToInt59952 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48679 to i64
%ae48679 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt59952)
store volatile %struct.ScmObj* %ae48679, %struct.ScmObj** %stackaddr$makeclosure59951, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48679, %struct.ScmObj* %f48055, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48679, %struct.ScmObj* %k48519, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48679, %struct.ScmObj* %anf_45bind48178, i64 2)
%argslist57968$_37foldr1480520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59953 = alloca %struct.ScmObj*, align 8
%argslist57968$_37foldr1480521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48179, %struct.ScmObj* %argslist57968$_37foldr1480520)
store volatile %struct.ScmObj* %argslist57968$_37foldr1480521, %struct.ScmObj** %stackaddr$prim59953, align 8
%stackaddr$prim59954 = alloca %struct.ScmObj*, align 8
%argslist57968$_37foldr1480522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48054, %struct.ScmObj* %argslist57968$_37foldr1480521)
store volatile %struct.ScmObj* %argslist57968$_37foldr1480522, %struct.ScmObj** %stackaddr$prim59954, align 8
%stackaddr$prim59955 = alloca %struct.ScmObj*, align 8
%argslist57968$_37foldr1480523 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48055, %struct.ScmObj* %argslist57968$_37foldr1480522)
store volatile %struct.ScmObj* %argslist57968$_37foldr1480523, %struct.ScmObj** %stackaddr$prim59955, align 8
%stackaddr$prim59956 = alloca %struct.ScmObj*, align 8
%argslist57968$_37foldr1480524 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48679, %struct.ScmObj* %argslist57968$_37foldr1480523)
store volatile %struct.ScmObj* %argslist57968$_37foldr1480524, %struct.ScmObj** %stackaddr$prim59956, align 8
%clofunc59957 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148052)
musttail call tailcc void %clofunc59957(%struct.ScmObj* %_37foldr148052, %struct.ScmObj* %argslist57968$_37foldr1480524)
ret void
}

define tailcc void @proc_clo$ae48679(%struct.ScmObj* %env$ae48679,%struct.ScmObj* %current_45args57964) {
%stackaddr$env-ref59958 = alloca %struct.ScmObj*, align 8
%f48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48679, i64 0)
store %struct.ScmObj* %f48055, %struct.ScmObj** %stackaddr$env-ref59958
%stackaddr$env-ref59959 = alloca %struct.ScmObj*, align 8
%k48519 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48679, i64 1)
store %struct.ScmObj* %k48519, %struct.ScmObj** %stackaddr$env-ref59959
%stackaddr$env-ref59960 = alloca %struct.ScmObj*, align 8
%anf_45bind48178 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48679, i64 2)
store %struct.ScmObj* %anf_45bind48178, %struct.ScmObj** %stackaddr$env-ref59960
%stackaddr$prim59961 = alloca %struct.ScmObj*, align 8
%_95k48520 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57964)
store volatile %struct.ScmObj* %_95k48520, %struct.ScmObj** %stackaddr$prim59961, align 8
%stackaddr$prim59962 = alloca %struct.ScmObj*, align 8
%current_45args57965 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57964)
store volatile %struct.ScmObj* %current_45args57965, %struct.ScmObj** %stackaddr$prim59962, align 8
%stackaddr$prim59963 = alloca %struct.ScmObj*, align 8
%anf_45bind48180 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57965)
store volatile %struct.ScmObj* %anf_45bind48180, %struct.ScmObj** %stackaddr$prim59963, align 8
%argslist57967$f480550 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59964 = alloca %struct.ScmObj*, align 8
%argslist57967$f480551 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48180, %struct.ScmObj* %argslist57967$f480550)
store volatile %struct.ScmObj* %argslist57967$f480551, %struct.ScmObj** %stackaddr$prim59964, align 8
%stackaddr$prim59965 = alloca %struct.ScmObj*, align 8
%argslist57967$f480552 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48178, %struct.ScmObj* %argslist57967$f480551)
store volatile %struct.ScmObj* %argslist57967$f480552, %struct.ScmObj** %stackaddr$prim59965, align 8
%stackaddr$prim59966 = alloca %struct.ScmObj*, align 8
%argslist57967$f480553 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48519, %struct.ScmObj* %argslist57967$f480552)
store volatile %struct.ScmObj* %argslist57967$f480553, %struct.ScmObj** %stackaddr$prim59966, align 8
%clofunc59967 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48055)
musttail call tailcc void %clofunc59967(%struct.ScmObj* %f48055, %struct.ScmObj* %argslist57967$f480553)
ret void
}

define tailcc void @proc_clo$ae48547(%struct.ScmObj* %env$ae48547,%struct.ScmObj* %current_45args57972) {
%stackaddr$prim59968 = alloca %struct.ScmObj*, align 8
%k48521 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57972)
store volatile %struct.ScmObj* %k48521, %struct.ScmObj** %stackaddr$prim59968, align 8
%stackaddr$prim59969 = alloca %struct.ScmObj*, align 8
%current_45args57973 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57972)
store volatile %struct.ScmObj* %current_45args57973, %struct.ScmObj** %stackaddr$prim59969, align 8
%stackaddr$prim59970 = alloca %struct.ScmObj*, align 8
%y48032 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57973)
store volatile %struct.ScmObj* %y48032, %struct.ScmObj** %stackaddr$prim59970, align 8
%ae48549 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59971 = alloca %struct.ScmObj*, align 8
%fptrToInt59972 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48550 to i64
%ae48550 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt59972)
store volatile %struct.ScmObj* %ae48550, %struct.ScmObj** %stackaddr$makeclosure59971, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48550, %struct.ScmObj* %y48032, i64 0)
%argslist57991$k485210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59973 = alloca %struct.ScmObj*, align 8
%argslist57991$k485211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48550, %struct.ScmObj* %argslist57991$k485210)
store volatile %struct.ScmObj* %argslist57991$k485211, %struct.ScmObj** %stackaddr$prim59973, align 8
%stackaddr$prim59974 = alloca %struct.ScmObj*, align 8
%argslist57991$k485212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48549, %struct.ScmObj* %argslist57991$k485211)
store volatile %struct.ScmObj* %argslist57991$k485212, %struct.ScmObj** %stackaddr$prim59974, align 8
%clofunc59975 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48521)
musttail call tailcc void %clofunc59975(%struct.ScmObj* %k48521, %struct.ScmObj* %argslist57991$k485212)
ret void
}

define tailcc void @proc_clo$ae48550(%struct.ScmObj* %env$ae48550,%struct.ScmObj* %current_45args57975) {
%stackaddr$env-ref59976 = alloca %struct.ScmObj*, align 8
%y48032 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48550, i64 0)
store %struct.ScmObj* %y48032, %struct.ScmObj** %stackaddr$env-ref59976
%stackaddr$prim59977 = alloca %struct.ScmObj*, align 8
%k48522 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57975)
store volatile %struct.ScmObj* %k48522, %struct.ScmObj** %stackaddr$prim59977, align 8
%stackaddr$prim59978 = alloca %struct.ScmObj*, align 8
%current_45args57976 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57975)
store volatile %struct.ScmObj* %current_45args57976, %struct.ScmObj** %stackaddr$prim59978, align 8
%stackaddr$prim59979 = alloca %struct.ScmObj*, align 8
%f48033 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57976)
store volatile %struct.ScmObj* %f48033, %struct.ScmObj** %stackaddr$prim59979, align 8
%stackaddr$makeclosure59980 = alloca %struct.ScmObj*, align 8
%fptrToInt59981 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48551 to i64
%ae48551 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt59981)
store volatile %struct.ScmObj* %ae48551, %struct.ScmObj** %stackaddr$makeclosure59980, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48551, %struct.ScmObj* %f48033, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48551, %struct.ScmObj* %k48522, i64 1)
%ae48552 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59982 = alloca %struct.ScmObj*, align 8
%fptrToInt59983 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48553 to i64
%ae48553 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt59983)
store volatile %struct.ScmObj* %ae48553, %struct.ScmObj** %stackaddr$makeclosure59982, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48553, %struct.ScmObj* %f48033, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48553, %struct.ScmObj* %y48032, i64 1)
%argslist57990$ae485510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59984 = alloca %struct.ScmObj*, align 8
%argslist57990$ae485511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48553, %struct.ScmObj* %argslist57990$ae485510)
store volatile %struct.ScmObj* %argslist57990$ae485511, %struct.ScmObj** %stackaddr$prim59984, align 8
%stackaddr$prim59985 = alloca %struct.ScmObj*, align 8
%argslist57990$ae485512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48552, %struct.ScmObj* %argslist57990$ae485511)
store volatile %struct.ScmObj* %argslist57990$ae485512, %struct.ScmObj** %stackaddr$prim59985, align 8
%clofunc59986 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48551)
musttail call tailcc void %clofunc59986(%struct.ScmObj* %ae48551, %struct.ScmObj* %argslist57990$ae485512)
ret void
}

define tailcc void @proc_clo$ae48551(%struct.ScmObj* %env$ae48551,%struct.ScmObj* %current_45args57978) {
%stackaddr$env-ref59987 = alloca %struct.ScmObj*, align 8
%f48033 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48551, i64 0)
store %struct.ScmObj* %f48033, %struct.ScmObj** %stackaddr$env-ref59987
%stackaddr$env-ref59988 = alloca %struct.ScmObj*, align 8
%k48522 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48551, i64 1)
store %struct.ScmObj* %k48522, %struct.ScmObj** %stackaddr$env-ref59988
%stackaddr$prim59989 = alloca %struct.ScmObj*, align 8
%_95k48523 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57978)
store volatile %struct.ScmObj* %_95k48523, %struct.ScmObj** %stackaddr$prim59989, align 8
%stackaddr$prim59990 = alloca %struct.ScmObj*, align 8
%current_45args57979 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57978)
store volatile %struct.ScmObj* %current_45args57979, %struct.ScmObj** %stackaddr$prim59990, align 8
%stackaddr$prim59991 = alloca %struct.ScmObj*, align 8
%anf_45bind48175 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57979)
store volatile %struct.ScmObj* %anf_45bind48175, %struct.ScmObj** %stackaddr$prim59991, align 8
%argslist57981$f480330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59992 = alloca %struct.ScmObj*, align 8
%argslist57981$f480331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48175, %struct.ScmObj* %argslist57981$f480330)
store volatile %struct.ScmObj* %argslist57981$f480331, %struct.ScmObj** %stackaddr$prim59992, align 8
%stackaddr$prim59993 = alloca %struct.ScmObj*, align 8
%argslist57981$f480332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48522, %struct.ScmObj* %argslist57981$f480331)
store volatile %struct.ScmObj* %argslist57981$f480332, %struct.ScmObj** %stackaddr$prim59993, align 8
%clofunc59994 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48033)
musttail call tailcc void %clofunc59994(%struct.ScmObj* %f48033, %struct.ScmObj* %argslist57981$f480332)
ret void
}

define tailcc void @proc_clo$ae48553(%struct.ScmObj* %env$ae48553,%struct.ScmObj* %args4803448524) {
%stackaddr$env-ref59995 = alloca %struct.ScmObj*, align 8
%f48033 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48553, i64 0)
store %struct.ScmObj* %f48033, %struct.ScmObj** %stackaddr$env-ref59995
%stackaddr$env-ref59996 = alloca %struct.ScmObj*, align 8
%y48032 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48553, i64 1)
store %struct.ScmObj* %y48032, %struct.ScmObj** %stackaddr$env-ref59996
%stackaddr$prim59997 = alloca %struct.ScmObj*, align 8
%k48525 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4803448524)
store volatile %struct.ScmObj* %k48525, %struct.ScmObj** %stackaddr$prim59997, align 8
%stackaddr$prim59998 = alloca %struct.ScmObj*, align 8
%args48034 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4803448524)
store volatile %struct.ScmObj* %args48034, %struct.ScmObj** %stackaddr$prim59998, align 8
%stackaddr$makeclosure59999 = alloca %struct.ScmObj*, align 8
%fptrToInt60000 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48557 to i64
%ae48557 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt60000)
store volatile %struct.ScmObj* %ae48557, %struct.ScmObj** %stackaddr$makeclosure59999, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48557, %struct.ScmObj* %args48034, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48557, %struct.ScmObj* %f48033, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48557, %struct.ScmObj* %k48525, i64 2)
%argslist57989$y480320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60001 = alloca %struct.ScmObj*, align 8
%argslist57989$y480321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y48032, %struct.ScmObj* %argslist57989$y480320)
store volatile %struct.ScmObj* %argslist57989$y480321, %struct.ScmObj** %stackaddr$prim60001, align 8
%stackaddr$prim60002 = alloca %struct.ScmObj*, align 8
%argslist57989$y480322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48557, %struct.ScmObj* %argslist57989$y480321)
store volatile %struct.ScmObj* %argslist57989$y480322, %struct.ScmObj** %stackaddr$prim60002, align 8
%clofunc60003 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y48032)
musttail call tailcc void %clofunc60003(%struct.ScmObj* %y48032, %struct.ScmObj* %argslist57989$y480322)
ret void
}

define tailcc void @proc_clo$ae48557(%struct.ScmObj* %env$ae48557,%struct.ScmObj* %current_45args57982) {
%stackaddr$env-ref60004 = alloca %struct.ScmObj*, align 8
%args48034 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48557, i64 0)
store %struct.ScmObj* %args48034, %struct.ScmObj** %stackaddr$env-ref60004
%stackaddr$env-ref60005 = alloca %struct.ScmObj*, align 8
%f48033 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48557, i64 1)
store %struct.ScmObj* %f48033, %struct.ScmObj** %stackaddr$env-ref60005
%stackaddr$env-ref60006 = alloca %struct.ScmObj*, align 8
%k48525 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48557, i64 2)
store %struct.ScmObj* %k48525, %struct.ScmObj** %stackaddr$env-ref60006
%stackaddr$prim60007 = alloca %struct.ScmObj*, align 8
%_95k48526 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57982)
store volatile %struct.ScmObj* %_95k48526, %struct.ScmObj** %stackaddr$prim60007, align 8
%stackaddr$prim60008 = alloca %struct.ScmObj*, align 8
%current_45args57983 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57982)
store volatile %struct.ScmObj* %current_45args57983, %struct.ScmObj** %stackaddr$prim60008, align 8
%stackaddr$prim60009 = alloca %struct.ScmObj*, align 8
%anf_45bind48173 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57983)
store volatile %struct.ScmObj* %anf_45bind48173, %struct.ScmObj** %stackaddr$prim60009, align 8
%stackaddr$makeclosure60010 = alloca %struct.ScmObj*, align 8
%fptrToInt60011 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48560 to i64
%ae48560 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt60011)
store volatile %struct.ScmObj* %ae48560, %struct.ScmObj** %stackaddr$makeclosure60010, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48560, %struct.ScmObj* %args48034, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48560, %struct.ScmObj* %k48525, i64 1)
%argslist57988$anf_45bind481730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60012 = alloca %struct.ScmObj*, align 8
%argslist57988$anf_45bind481731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48033, %struct.ScmObj* %argslist57988$anf_45bind481730)
store volatile %struct.ScmObj* %argslist57988$anf_45bind481731, %struct.ScmObj** %stackaddr$prim60012, align 8
%stackaddr$prim60013 = alloca %struct.ScmObj*, align 8
%argslist57988$anf_45bind481732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48560, %struct.ScmObj* %argslist57988$anf_45bind481731)
store volatile %struct.ScmObj* %argslist57988$anf_45bind481732, %struct.ScmObj** %stackaddr$prim60013, align 8
%clofunc60014 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48173)
musttail call tailcc void %clofunc60014(%struct.ScmObj* %anf_45bind48173, %struct.ScmObj* %argslist57988$anf_45bind481732)
ret void
}

define tailcc void @proc_clo$ae48560(%struct.ScmObj* %env$ae48560,%struct.ScmObj* %current_45args57985) {
%stackaddr$env-ref60015 = alloca %struct.ScmObj*, align 8
%args48034 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48560, i64 0)
store %struct.ScmObj* %args48034, %struct.ScmObj** %stackaddr$env-ref60015
%stackaddr$env-ref60016 = alloca %struct.ScmObj*, align 8
%k48525 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48560, i64 1)
store %struct.ScmObj* %k48525, %struct.ScmObj** %stackaddr$env-ref60016
%stackaddr$prim60017 = alloca %struct.ScmObj*, align 8
%_95k48527 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57985)
store volatile %struct.ScmObj* %_95k48527, %struct.ScmObj** %stackaddr$prim60017, align 8
%stackaddr$prim60018 = alloca %struct.ScmObj*, align 8
%current_45args57986 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57985)
store volatile %struct.ScmObj* %current_45args57986, %struct.ScmObj** %stackaddr$prim60018, align 8
%stackaddr$prim60019 = alloca %struct.ScmObj*, align 8
%anf_45bind48174 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57986)
store volatile %struct.ScmObj* %anf_45bind48174, %struct.ScmObj** %stackaddr$prim60019, align 8
%stackaddr$prim60020 = alloca %struct.ScmObj*, align 8
%cpsargs48528 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48525, %struct.ScmObj* %args48034)
store volatile %struct.ScmObj* %cpsargs48528, %struct.ScmObj** %stackaddr$prim60020, align 8
%clofunc60021 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48174)
musttail call tailcc void %clofunc60021(%struct.ScmObj* %anf_45bind48174, %struct.ScmObj* %cpsargs48528)
ret void
}

define tailcc void @proc_clo$ae48532(%struct.ScmObj* %env$ae48532,%struct.ScmObj* %current_45args57993) {
%stackaddr$prim60022 = alloca %struct.ScmObj*, align 8
%k48529 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57993)
store volatile %struct.ScmObj* %k48529, %struct.ScmObj** %stackaddr$prim60022, align 8
%stackaddr$prim60023 = alloca %struct.ScmObj*, align 8
%current_45args57994 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57993)
store volatile %struct.ScmObj* %current_45args57994, %struct.ScmObj** %stackaddr$prim60023, align 8
%stackaddr$prim60024 = alloca %struct.ScmObj*, align 8
%yu48031 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57994)
store volatile %struct.ScmObj* %yu48031, %struct.ScmObj** %stackaddr$prim60024, align 8
%argslist57996$yu480310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60025 = alloca %struct.ScmObj*, align 8
%argslist57996$yu480311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu48031, %struct.ScmObj* %argslist57996$yu480310)
store volatile %struct.ScmObj* %argslist57996$yu480311, %struct.ScmObj** %stackaddr$prim60025, align 8
%stackaddr$prim60026 = alloca %struct.ScmObj*, align 8
%argslist57996$yu480312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48529, %struct.ScmObj* %argslist57996$yu480311)
store volatile %struct.ScmObj* %argslist57996$yu480312, %struct.ScmObj** %stackaddr$prim60026, align 8
%clofunc60027 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu48031)
musttail call tailcc void %clofunc60027(%struct.ScmObj* %yu48031, %struct.ScmObj* %argslist57996$yu480312)
ret void
}